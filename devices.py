"""
Megapad-64 Peripheral / Device Layer
=====================================
Memory-mapped I/O devices for the system emulator.

Physical memory map (from docs/architecture.md):
  FFFF_FF00_0000_0000 – FFFF_FF7F_FFFF_FFFF  : 32 GiB MMIO region

We carve out the first slice for our emulated peripherals:

  MMIO_BASE + 0x0000 .. 0x00FF  : UART  (serial console)
  MMIO_BASE + 0x0100 .. 0x01FF  : Timer
  MMIO_BASE + 0x0200 .. 0x02FF  : Storage controller (block device)
  MMIO_BASE + 0x0300 .. 0x03FF  : System info / board-level

All registers are 8-bit accessed; the CPU's mem_read8/mem_write8 hits
these through the MMIO dispatch layer in system.py.
"""

from __future__ import annotations
import socket
import time
import threading
from typing import Optional, TYPE_CHECKING
from collections import deque

if TYPE_CHECKING:
    from nic_backends import NICBackend

# ---------------------------------------------------------------------------
#  Base addresses (offsets within the MMIO aperture)
# ---------------------------------------------------------------------------

MMIO_BASE  = 0xFFFF_FF00_0000_0000

UART_BASE    = 0x0000
TIMER_BASE   = 0x0100
STORAGE_BASE = 0x0200
SYSINFO_BASE = 0x0300
NIC_BASE     = 0x0400
MBOX_BASE    = 0x0500
SPINLOCK_BASE = 0x0600
CRC_BASE     = 0x07C0
AES_BASE     = 0x0700
SHA3_BASE    = 0x0780
TRNG_BASE    = 0x0800
X25519_BASE  = 0x0840
NTT_BASE     = 0x08C0
KEM_BASE     = 0x0900
FB_BASE      = 0x0A00


# ---------------------------------------------------------------------------
#  Device base class
# ---------------------------------------------------------------------------

class Device:
    """Abstract MMIO peripheral."""

    def __init__(self, name: str, base: int, size: int):
        self.name = name
        self.base = base  # offset within MMIO aperture
        self.size = size   # number of bytes in register window

    def read8(self, offset: int) -> int:
        """Read one byte at the given offset within this device."""
        return 0

    def write8(self, offset: int, value: int):
        """Write one byte at the given offset within this device."""
        pass

    def tick(self, cycles: int):
        """Advance the device clock by N CPU cycles. Override for timers etc."""
        pass


# ---------------------------------------------------------------------------
#  UART — Serial Console
# ---------------------------------------------------------------------------
# Register map (offsets from UART_BASE):
#   0x00  TX_DATA   (W)  — write a byte, queued for host output
#   0x01  RX_DATA   (R)  — read next byte from input buffer
#   0x02  STATUS    (R)  — bit 0: TX_READY (always 1 in emulator)
#                          bit 1: RX_AVAIL (1 if input buffer non-empty)
#                          bit 5: TX_EMPTY (always 1)
#   0x03  CONTROL   (RW) — bit 0: RX interrupt enable
#                          bit 1: TX interrupt enable
#   0x04  BAUD_LO   (RW) — baud rate low byte (cosmetic in emulator)
#   0x05  BAUD_HI   (RW) — baud rate high byte

class UART(Device):
    """Emulated serial console — connects to the CLI's terminal."""

    def __init__(self):
        super().__init__("UART", UART_BASE, 0x10)
        self.tx_buffer: deque[int] = deque()   # bytes waiting to be displayed
        self.rx_buffer: deque[int] = deque()   # bytes from keyboard → CPU
        self.control: int = 0
        self.baud_lo: int = 0
        self.baud_hi: int = 0

        # Callbacks
        self.on_tx: Optional[callable] = None  # called with byte when CPU writes TX

    def read8(self, offset: int) -> int:
        if offset == 0x00:     # TX_DATA — reading it is undefined, return 0
            return 0
        elif offset == 0x01:   # RX_DATA
            if self.rx_buffer:
                return self.rx_buffer.popleft()
            return 0
        elif offset == 0x02:   # STATUS
            tx_ready = 1       # always ready in emulator
            rx_avail = 1 if self.rx_buffer else 0
            tx_empty = 1
            return tx_ready | (rx_avail << 1) | (tx_empty << 5)
        elif offset == 0x03:
            return self.control
        elif offset == 0x04:
            return self.baud_lo
        elif offset == 0x05:
            return self.baud_hi
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x00:     # TX_DATA
            self.tx_buffer.append(value)
            if self.on_tx:
                self.on_tx(value)
        elif offset == 0x03:
            self.control = value
        elif offset == 0x04:
            self.baud_lo = value
        elif offset == 0x05:
            self.baud_hi = value

    def inject_input(self, data: bytes | str):
        """Push bytes into the RX buffer (from host keyboard)."""
        if isinstance(data, str):
            data = data.encode("ascii", errors="replace")
        for b in data:
            self.rx_buffer.append(b & 0xFF)

    @property
    def has_rx_data(self) -> bool:
        return len(self.rx_buffer) > 0

    def drain_tx(self) -> str:
        """Return all pending TX bytes as a string and clear the buffer."""
        out = bytes(self.tx_buffer).decode("ascii", errors="replace")
        self.tx_buffer.clear()
        return out


# ---------------------------------------------------------------------------
#  Timer
# ---------------------------------------------------------------------------
# Register map (offsets from TIMER_BASE):
#   0x00  COUNT_LO  (R)   — low byte of free-running counter
#   0x01  COUNT_HI  (R)   — bits 15:8
#   0x02  COUNT_B2  (R)   — bits 23:16
#   0x03  COUNT_B3  (R)   — bits 31:24
#   0x04  COMPARE_LO (RW) — compare match low byte
#   0x05  COMPARE_HI (RW) — compare match high byte
#   0x06  COMPARE_B2 (RW)
#   0x07  COMPARE_B3 (RW)
#   0x08  CONTROL   (RW)  — bit 0: enable
#                           bit 1: compare-match interrupt enable
#                           bit 2: auto-reload on match
#   0x09  STATUS    (RW)  — bit 0: compare-match flag (write 1 to clear)

class Timer(Device):
    """Simple 32-bit free-running timer with compare-match interrupt."""

    def __init__(self):
        super().__init__("Timer", TIMER_BASE, 0x10)
        self.counter: int = 0
        self.compare: int = 0xFFFFFFFF
        self.control: int = 0
        self.status: int = 0
        self.irq_pending: bool = False

    def read8(self, offset: int) -> int:
        if 0x00 <= offset <= 0x03:
            return (self.counter >> (8 * offset)) & 0xFF
        if 0x04 <= offset <= 0x07:
            return (self.compare >> (8 * (offset - 0x04))) & 0xFF
        if offset == 0x08:
            return self.control
        if offset == 0x09:
            return self.status
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if 0x04 <= offset <= 0x07:
            shift = 8 * (offset - 0x04)
            mask = 0xFF << shift
            self.compare = (self.compare & ~mask) | (value << shift)
            self.compare &= 0xFFFFFFFF
        elif offset == 0x08:
            self.control = value
        elif offset == 0x09:
            # Write-1-to-clear
            self.status &= ~value
            if not (self.status & 1):
                self.irq_pending = False

    def tick(self, cycles: int):
        if not (self.control & 1) or cycles == 0:  # not enabled
            return

        if cycles == 1:
            # Fast path for single-tick (common in per-step execution)
            self.counter = (self.counter + 1) & 0xFFFFFFFF
            if self.counter == self.compare:
                self.status |= 1
                if self.control & 2:
                    self.irq_pending = True
                if self.control & 4:
                    self.counter = 0
            return

        # O(1) batch tick — avoid per-cycle Python loop
        old = self.counter
        cmp = self.compare

        if self.control & 4:
            # Auto-reload mode: counter resets to 0 on match
            if cmp == 0:
                # Degenerate: compare=0 means match every tick
                self.status |= 1
                if self.control & 2:
                    self.irq_pending = True
                self.counter = 0
                return
            # How many ticks until we hit compare?
            gap = (cmp - old) & 0xFFFFFFFF
            if cycles >= gap:
                # We hit (and possibly wrap past) the compare value
                self.status |= 1
                if self.control & 2:
                    self.irq_pending = True
                # After match, counter reloads to 0 and keeps counting
                remaining = cycles - gap
                self.counter = remaining % cmp if cmp > 0 else 0
            else:
                self.counter = (old + cycles) & 0xFFFFFFFF
        else:
            # No auto-reload: just check if compare is crossed
            new = old + cycles  # unbounded to detect crossing
            if old < cmp <= new:
                self.status |= 1
                if self.control & 2:
                    self.irq_pending = True
            # Also handle 32-bit wrap-around case
            elif new > 0xFFFFFFFF and cmp <= (new & 0xFFFFFFFF):
                self.status |= 1
                if self.control & 2:
                    self.irq_pending = True
            self.counter = new & 0xFFFFFFFF


# ---------------------------------------------------------------------------
#  Storage Controller — Block Device
# ---------------------------------------------------------------------------
# Emulates a simple sector-based storage device backed by a host file.
#
# Register map (offsets from STORAGE_BASE):
#   0x00  CMD       (W)  — command: 0x01=READ, 0x02=WRITE, 0x03=STATUS
#   0x01  STATUS    (R)  — bit 0: busy, bit 1: error, bit 7: present
#   0x02  SECTOR_0  (RW) — sector number byte 0 (LSB)
#   0x03  SECTOR_1  (RW) — sector number byte 1
#   0x04  SECTOR_2  (RW) — sector number byte 2
#   0x05  SECTOR_3  (RW) — sector number byte 3
#   0x06  DMA_ADDR_0 (RW) — destination/source memory address byte 0
#   0x07  DMA_ADDR_1 (RW)
#   0x08  DMA_ADDR_2 (RW)
#   0x09  DMA_ADDR_3 (RW)
#   0x0A  DMA_ADDR_4 (RW)
#   0x0B  DMA_ADDR_5 (RW)
#   0x0C  DMA_ADDR_6 (RW)
#   0x0D  DMA_ADDR_7 (RW)
#   0x0E  SEC_COUNT  (RW) — number of sectors to transfer (1-255)
#   0x0F  DATA       (RW) — byte-at-a-time data port (alternative to DMA)

SECTOR_SIZE = 512

class Storage(Device):
    """Block device backed by a host file."""

    def __init__(self, image_path: Optional[str] = None):
        super().__init__("Storage", STORAGE_BASE, 0x10)
        self.image_path = image_path
        self._image_data: bytearray = bytearray()
        self.sector_num: int = 0
        self.dma_addr: int = 0
        self.sec_count: int = 1
        self.status: int = 0  # bit 7 = present
        self.busy: bool = False
        self.error: bool = False
        self.data_port_buf: bytearray = bytearray()
        self.data_port_pos: int = 0

        # DMA callback — system.py will set this to perform memory writes
        self.on_dma_read: Optional[callable] = None   # (sector, count) → bytes
        self.on_dma_write: Optional[callable] = None   # (sector, count, data)

        # Reference to system memory (set by system.py)
        self._mem_read: Optional[callable] = None
        self._mem_write: Optional[callable] = None

        if image_path:
            self.load_image(image_path)

    def load_image(self, path: str):
        """Load a disk image from file."""
        try:
            with open(path, "rb") as f:
                self._image_data = bytearray(f.read())
            self.image_path = path
            self.status = 0x80  # present
        except FileNotFoundError:
            # Create empty image
            self._image_data = bytearray(SECTOR_SIZE * 2048)  # 1 MiB default
            self.image_path = path
            self.status = 0x80

    def save_image(self):
        """Flush image data back to host file."""
        if self.image_path and self._image_data:
            with open(self.image_path, "wb") as f:
                f.write(self._image_data)

    @property
    def total_sectors(self) -> int:
        return len(self._image_data) // SECTOR_SIZE if self._image_data else 0

    def read_sectors(self, sector: int, count: int) -> bytearray:
        """Read `count` sectors starting at `sector`."""
        start = sector * SECTOR_SIZE
        end = start + count * SECTOR_SIZE
        if end > len(self._image_data):
            # Pad with zeros beyond image
            data = bytearray(self._image_data[start:])
            data.extend(bytearray(end - len(self._image_data)))
            return data
        return bytearray(self._image_data[start:end])

    def write_sectors(self, sector: int, count: int, data: bytes | bytearray):
        """Write `count` sectors starting at `sector`."""
        start = sector * SECTOR_SIZE
        end = start + count * SECTOR_SIZE
        # Extend image if necessary
        if end > len(self._image_data):
            self._image_data.extend(bytearray(end - len(self._image_data)))
        self._image_data[start:end] = data[:count * SECTOR_SIZE]

    def read8(self, offset: int) -> int:
        if offset == 0x01:    # STATUS
            s = 0
            if self._image_data:
                s |= 0x80    # present
            if self.busy:
                s |= 0x01
            if self.error:
                s |= 0x02
            return s
        if 0x02 <= offset <= 0x05:
            return (self.sector_num >> (8 * (offset - 0x02))) & 0xFF
        if 0x06 <= offset <= 0x0D:
            return (self.dma_addr >> (8 * (offset - 0x06))) & 0xFF
        if offset == 0x0E:
            return self.sec_count & 0xFF
        if offset == 0x0F:  # DATA port
            if self.data_port_pos < len(self.data_port_buf):
                b = self.data_port_buf[self.data_port_pos]
                self.data_port_pos += 1
                return b
            return 0
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x00:    # CMD
            self._execute_cmd(value)
        elif 0x02 <= offset <= 0x05:
            shift = 8 * (offset - 0x02)
            mask = 0xFF << shift
            self.sector_num = (self.sector_num & ~mask) | (value << shift)
            self.sector_num &= 0xFFFFFFFF
        elif 0x06 <= offset <= 0x0D:
            shift = 8 * (offset - 0x06)
            mask = 0xFF << shift
            self.dma_addr = (self.dma_addr & ~mask) | (value << shift)
        elif offset == 0x0E:
            self.sec_count = value if value else 1
        elif offset == 0x0F:  # DATA port write
            self.data_port_buf.append(value)

    def _execute_cmd(self, cmd: int):
        self.error = False
        if cmd == 0x01:       # READ → DMA
            data = self.read_sectors(self.sector_num, self.sec_count)
            if self._mem_write:
                for i, b in enumerate(data):
                    self._mem_write(self.dma_addr + i, b)
            # Also fill data port buffer
            self.data_port_buf = data
            self.data_port_pos = 0
        elif cmd == 0x02:     # WRITE ← DMA
            if self._mem_read:
                nbytes = self.sec_count * SECTOR_SIZE
                data = bytearray()
                for i in range(nbytes):
                    data.append(self._mem_read(self.dma_addr + i))
                self.write_sectors(self.sector_num, self.sec_count, data)
            else:
                # Write from data port buffer
                self.write_sectors(self.sector_num, self.sec_count, self.data_port_buf)
                self.data_port_buf = bytearray()
                self.data_port_pos = 0
        elif cmd == 0x03:     # STATUS (no-op, just read STATUS reg)
            pass
        elif cmd == 0xFF:     # FLUSH (save to host file)
            self.save_image()


# ---------------------------------------------------------------------------
#  System Info (read-only board identification)
# ---------------------------------------------------------------------------
# Register map (offsets from SYSINFO_BASE):
#   0x00  BOARD_ID_0  — 'M'
#   0x01  BOARD_ID_1  — 'P'
#   0x02  BOARD_ID_2  — '6'
#   0x03  BOARD_ID_3  — '4'
#   0x04  VERSION_MAJ
#   0x05  VERSION_MIN
#  Register map (64-bit aligned, matches RTL mp64_soc.v SysInfo):
#   0x00  BOARD_ID_VER — "MP64" + version 2.1  [64'h4D50_3634_0002_0001]
#   0x08  BANK0_SIZE   — Bank 0 (system RAM) size in bytes
#   0x10  NUM_CORES    — total core count
#   0x18  CLUSTER_EN   — per-cluster enable mask (R/W)
#   0x20  HBW_BASE     — High-Bandwidth math RAM base address
#   0x28  HBW_SIZE     — HBW region size in bytes
#   0x30  INT_MEM_TOTAL — total internal memory in bytes
#   0x38  EXT_MEM_BASE — external memory base address
#   0x40  EXT_MEM_SIZE — external memory size in bytes

# Board ID + version packed as 64-bit LE value:
#   bytes 0-1: version (minor=1, then 0x00)
#   bytes 2-3: version (major=2, then 0x00)
#   bytes 4-7: "MP64" reversed → '4','6','P','M' = 0x34,0x36,0x50,0x4D
_BOARD_ID_VER = 0x4D50_3634_0002_0001

class SystemInfo(Device):
    """Board identification and capability reporting (matches RTL SysInfo)."""

    def __init__(self, bank0_size: int = 1 << 20,
                 num_cores: int = 1,
                 hbw_base: int = 0xFFD0_0000,
                 hbw_size: int = 3 * (1 << 20),
                 int_mem_total: int = 4 * (1 << 20),
                 # Legacy compat — ignored if bank0_size is set explicitly
                 mem_size_kib: int | None = None,
                 has_storage: bool = False,
                 has_nic: bool = False,
                 ext_mem_base: int = 0,
                 ext_mem_size: int = 0):
        super().__init__("SysInfo", SYSINFO_BASE, 0x48)
        if mem_size_kib is not None and bank0_size == (1 << 20):
            # Legacy caller: convert KiB → bytes
            bank0_size = mem_size_kib * 1024
        self.bank0_size = bank0_size
        self.num_cores = num_cores
        self.cluster_en = 0
        self.hbw_base = hbw_base
        self.hbw_size = hbw_size
        self.int_mem_total = int_mem_total
        self.ext_mem_base = ext_mem_base
        self.ext_mem_size = ext_mem_size
        # Legacy flags (kept for backward compat, not in RTL)
        self.has_storage = has_storage
        self.has_nic = has_nic

        # Pre-build register table (offset → 64-bit value)
        self._regs = {
            0x00: _BOARD_ID_VER,
            0x08: self.bank0_size,
            0x10: self.num_cores,
            # 0x18 is dynamic (cluster_en)
            0x20: self.hbw_base,
            0x28: self.hbw_size,
            0x30: self.int_mem_total,
            0x38: self.ext_mem_base,
            0x40: self.ext_mem_size,
        }

    def read8(self, offset: int) -> int:
        if offset < 0 or offset >= 0x48:
            return 0
        reg_base = offset & ~0x07            # align down to 8
        byte_idx = offset & 0x07
        if reg_base == 0x18:
            val = self.cluster_en            # dynamic
        else:
            val = self._regs.get(reg_base, 0)
        return (val >> (byte_idx * 8)) & 0xFF

    def write8(self, offset: int, val: int):
        # Only cluster_en (0x18) is writable
        if 0x18 <= offset < 0x20:
            byte_idx = offset - 0x18
            mask = 0xFF << (byte_idx * 8)
            self.cluster_en = (self.cluster_en & ~mask) | ((val & 0xFF) << (byte_idx * 8))


# ---------------------------------------------------------------------------
#  Network Interface Controller (NIC)
# ---------------------------------------------------------------------------
# A simple frame-oriented NIC with DMA support and optional host
# passthrough via UDP.  Frames up to 1500 bytes.
#
# Register map (offsets from NIC_BASE):
#   0x00  CMD        (W)  — 0x01=SEND, 0x02=RECV, 0x03=STATUS, 0x04=RESET
#   0x01  STATUS     (R)  — bit 0: TX busy
#                           bit 1: RX frame available
#                           bit 2: link up
#                           bit 3: error
#                           bit 7: present
#   0x02..0x09  DMA_ADDR (RW)  — 64-bit DMA address in RAM
#   0x0A..0x0B  FRAME_LEN (RW) — 16-bit frame length (for TX, set before SEND;
#                                 for RX, read after RECV)
#   0x0C  IRQ_CTRL   (RW) — bit 0: RX IRQ enable, bit 1: TX IRQ enable
#   0x0D  IRQ_STATUS (R)  — bit 0: RX IRQ pending, bit 1: TX IRQ pending
#   0x0E..0x13  MAC_ADDR (R) — 6-byte MAC address (bytes 0x0E-0x13)
#   0x14  TX_COUNT_LO (R) — frames sent (low byte)
#   0x15  TX_COUNT_HI (R) — frames sent (high byte)
#   0x16  RX_COUNT_LO (R) — frames received (low byte)
#   0x17  RX_COUNT_HI (R) — frames received (high byte)
#   0x18..0x1F  reserved
#   0x20..0x7F  DATA port (RW) — byte-at-a-time alternative to DMA

NIC_MTU = 1500

class NetworkDevice(Device):
    """Emulated NIC with pluggable backend.

    Supports three operating modes:
      1. Loopback (default) — for unit tests; inject_frame() / on_tx_frame
      2. UDP passthrough    — tunnel raw frames over UDP (--nic PORT)
      3. Real backend        — TAP device for real L2 networking (--nic-tap)

    When a NICBackend is attached, TX frames are forwarded to the
    backend's send() and inbound frames from the backend are queued
    into rx_queue automatically.
    """

    def __init__(self, mac: bytes = b'\x02\x4D\x50\x36\x34\x00',
                 passthrough_port: Optional[int] = None,
                 passthrough_host: str = '127.0.0.1',
                 passthrough_peer_port: Optional[int] = None,
                 backend: 'Optional[NICBackend]' = None):
        super().__init__("NIC", NIC_BASE, 0x80)
        self.mac = bytearray(mac[:6].ljust(6, b'\x00'))
        self.dma_addr: int = 0
        self.frame_len: int = 0
        self.irq_ctrl: int = 0
        self.irq_status: int = 0
        self.error: bool = False
        self.link_up: bool = True
        self.tx_count: int = 0
        self.rx_count: int = 0

        # Frame buffers
        self.tx_queue: deque[bytes] = deque(maxlen=64)
        self.rx_queue: deque[bytes] = deque(maxlen=64)

        # Data port (byte-at-a-time alternative to DMA)
        self._data_buf: bytearray = bytearray()
        self._data_pos: int = 0

        # DMA callbacks — system.py sets these
        self._mem_read: Optional[callable] = None
        self._mem_write: Optional[callable] = None

        # TX callback — for test inspection
        self.on_tx_frame: Optional[callable] = None

        # --- Pluggable backend ---
        self._backend: 'Optional[NICBackend]' = backend
        if self._backend is not None:
            self._backend.on_rx_frame = self._backend_rx
            self._backend.start()
            self.link_up = self._backend.link_up

        # UDP passthrough (legacy mode — used when no backend is set)
        self._passthrough_port = passthrough_port
        self._passthrough_host = passthrough_host
        self._passthrough_peer_port = passthrough_peer_port or (
            (passthrough_port + 1) if passthrough_port else None
        )
        self._sock: Optional[socket.socket] = None
        self._rx_thread: Optional[threading.Thread] = None
        self._running = False

        if passthrough_port is not None and self._backend is None:
            self._start_passthrough()

    def _backend_rx(self, frame: bytes):
        """Callback from NICBackend when a frame arrives from the wire."""
        if len(frame) > NIC_MTU:
            frame = frame[:NIC_MTU]
        if len(self.rx_queue) < (self.rx_queue.maxlen or 64):
            self.rx_queue.append(bytes(frame))
            self.rx_count = (self.rx_count + 1) & 0xFFFF
            if self.irq_ctrl & 1:  # RX IRQ enable
                self.irq_status |= 1

    @property
    def backend(self) -> 'Optional[NICBackend]':
        return self._backend

    @property
    def backend_name(self) -> str:
        if self._backend:
            return self._backend.name
        if self._passthrough_port:
            return f"udp-legacy:{self._passthrough_port}"
        return "loopback"

    def _start_passthrough(self):
        """Bind a UDP socket and start a background RX listener thread."""
        try:
            self._sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            self._sock.bind((self._passthrough_host, self._passthrough_port))
            self._sock.settimeout(0.1)
            self._running = True
            self._rx_thread = threading.Thread(
                target=self._rx_listener, daemon=True, name="NIC-RX"
            )
            self._rx_thread.start()
        except OSError:
            self.error = True
            self.link_up = False

    def _rx_listener(self):
        """Background thread: receive UDP datagrams → RX queue."""
        while self._running and self._sock:
            try:
                data, addr = self._sock.recvfrom(NIC_MTU + 64)
                if data and len(self.rx_queue) < self.rx_queue.maxlen:
                    self.rx_queue.append(bytes(data[:NIC_MTU]))
                    self.rx_count = (self.rx_count + 1) & 0xFFFF
                    if self.irq_ctrl & 1:  # RX IRQ enable
                        self.irq_status |= 1
            except socket.timeout:
                continue
            except OSError:
                break

    def stop(self):
        """Shut down the passthrough listener and backend."""
        self._running = False
        if self._sock:
            try:
                self._sock.close()
            except OSError:
                pass
            self._sock = None
        if self._backend:
            self._backend.stop()

    def inject_frame(self, data: bytes):
        """Push a frame into the RX queue (for testing or local injection)."""
        if len(data) > NIC_MTU:
            data = data[:NIC_MTU]
        self.rx_queue.append(bytes(data))
        self.rx_count = (self.rx_count + 1) & 0xFFFF
        if self.irq_ctrl & 1:
            self.irq_status |= 1

    def read8(self, offset: int) -> int:
        if offset == 0x00:      # CMD (write-only)
            return 0
        elif offset == 0x01:    # STATUS
            s = 0x80            # present
            if self.rx_queue:
                s |= 0x02       # RX available
            if self.link_up:
                s |= 0x04       # link up
            if self.error:
                s |= 0x08
            return s
        elif 0x02 <= offset <= 0x09:  # DMA_ADDR
            return (self.dma_addr >> (8 * (offset - 0x02))) & 0xFF
        elif offset == 0x0A:    # FRAME_LEN low
            return self.frame_len & 0xFF
        elif offset == 0x0B:    # FRAME_LEN high
            return (self.frame_len >> 8) & 0xFF
        elif offset == 0x0C:    # IRQ_CTRL
            return self.irq_ctrl
        elif offset == 0x0D:    # IRQ_STATUS
            return self.irq_status
        elif 0x0E <= offset <= 0x13:  # MAC address
            idx = offset - 0x0E
            if idx < 6:
                return self.mac[idx]
            return 0
        elif offset == 0x14:    # TX_COUNT low
            return self.tx_count & 0xFF
        elif offset == 0x15:    # TX_COUNT high
            return (self.tx_count >> 8) & 0xFF
        elif offset == 0x16:    # RX_COUNT low
            return self.rx_count & 0xFF
        elif offset == 0x17:    # RX_COUNT high
            return (self.rx_count >> 8) & 0xFF
        elif 0x20 <= offset <= 0x7F:  # DATA port
            if self._data_pos < len(self._data_buf):
                b = self._data_buf[self._data_pos]
                self._data_pos += 1
                return b
            return 0
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x00:      # CMD
            self._execute_cmd(value)
        elif 0x02 <= offset <= 0x09:  # DMA_ADDR
            shift = 8 * (offset - 0x02)
            mask = 0xFF << shift
            self.dma_addr = (self.dma_addr & ~mask) | (value << shift)
        elif offset == 0x0A:    # FRAME_LEN low
            self.frame_len = (self.frame_len & 0xFF00) | value
        elif offset == 0x0B:    # FRAME_LEN high
            self.frame_len = (self.frame_len & 0x00FF) | (value << 8)
        elif offset == 0x0C:    # IRQ_CTRL
            self.irq_ctrl = value
        elif offset == 0x0D:    # IRQ_STATUS (write-1-to-clear)
            self.irq_status &= ~value
        elif 0x20 <= offset <= 0x7F:  # DATA port write
            self._data_buf.append(value)

    def _execute_cmd(self, cmd: int):
        self.error = False
        if cmd == 0x01:         # SEND — transmit frame
            frame = self._read_tx_frame()
            if frame:
                self.tx_queue.append(frame)
                self.tx_count = (self.tx_count + 1) & 0xFFFF
                if self.on_tx_frame:
                    self.on_tx_frame(frame)
                # Backend TX (real networking)
                if self._backend:
                    if not self._backend.send(frame):
                        self.error = True
                # UDP passthrough (legacy)
                elif self._sock and self._passthrough_peer_port:
                    try:
                        self._sock.sendto(
                            frame,
                            (self._passthrough_host, self._passthrough_peer_port)
                        )
                    except OSError:
                        self.error = True
                if self.irq_ctrl & 2:  # TX IRQ enable
                    self.irq_status |= 2
        elif cmd == 0x02:       # RECV — receive next frame
            if self.rx_queue:
                frame = self.rx_queue.popleft()
                self.frame_len = len(frame)
                self._write_rx_frame(frame)
            else:
                self.frame_len = 0
        elif cmd == 0x03:       # STATUS (no-op, just read STATUS reg)
            pass
        elif cmd == 0x04:       # RESET
            self.tx_queue.clear()
            self.rx_queue.clear()
            self._data_buf = bytearray()
            self._data_pos = 0
            self.frame_len = 0
            self.irq_status = 0
            self.error = False
            self.tx_count = 0
            self.rx_count = 0

    def _read_tx_frame(self) -> Optional[bytes]:
        """Read frame_len bytes from DMA address (or data port fallback)."""
        nbytes = self.frame_len
        if nbytes <= 0 or nbytes > NIC_MTU:
            self.error = True
            return None
        if self._mem_read:
            data = bytearray()
            for i in range(nbytes):
                data.append(self._mem_read(self.dma_addr + i))
            return bytes(data)
        elif self._data_buf:
            frame = bytes(self._data_buf[:nbytes])
            self._data_buf = bytearray()
            self._data_pos = 0
            return frame
        self.error = True
        return None

    def _write_rx_frame(self, frame: bytes):
        """Write received frame to DMA address (and data port buffer)."""
        if self._mem_write:
            for i, b in enumerate(frame):
                self._mem_write(self.dma_addr + i, b)
        # Also make available via data port
        self._data_buf = bytearray(frame)
        self._data_pos = 0

    def drain_tx(self) -> list[bytes]:
        """Return and clear all pending TX frames."""
        frames = list(self.tx_queue)
        self.tx_queue.clear()
        return frames


# ---------------------------------------------------------------------------
#  Inter-Core Mailbox (IPI)
# ---------------------------------------------------------------------------
# Emulates the hardware mailbox from the FPGA multicore SoC.
# Each core has a 64-bit data slot. Writing MBOX_SEND triggers an IPI
# to the target core. The target reads STATUS to see who sent it,
# reads DATA, then writes ACK to clear the pending bit.
#
# Register map (offsets from MBOX_BASE):
#   0x00..0x07  DATA_LO/HI — 64-bit mailbox data (per-requester slot)
#   0x08        SEND       — write target_core_id to send IPI
#   0x09        STATUS     — read: bit[n] = pending IPI from core n
#   0x0A        ACK        — write source_core_id to clear pending bit
#
# Requester identity comes from DeviceBus.requester_id (set by system.py).

class MailboxDevice(Device):
    """Inter-core mailbox with IPI delivery."""

    def __init__(self, num_cores: int = 4):
        super().__init__("Mailbox", MBOX_BASE, 0x10)
        self.num_cores = num_cores
        # data[sender] = 64-bit value written by sender
        self.data: list[int] = [0] * num_cores
        # pending[target] = bitmask of which cores have sent an unacked IPI
        self.pending: list[int] = [0] * num_cores
        # Callback: called with (target_core_id,) when IPI is sent
        self.on_ipi: Optional[callable] = None
        # Callback: called with (acking_core_id,) after ACK clears a pending bit
        self.on_ack: Optional[callable] = None
        # Requester context — set by DeviceBus before each access
        self._requester_id: int = 0

    def read8(self, offset: int) -> int:
        rid = self._requester_id
        if 0x00 <= offset <= 0x07:
            # Read mailbox data for the requesting core's slot
            # Returns the data from the first pending sender
            shift = 8 * offset
            return (self.data[rid] >> shift) & 0xFF
        elif offset == 0x09:  # STATUS
            return self.pending[rid] & 0xFF
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        rid = self._requester_id
        if 0x00 <= offset <= 0x07:
            # Write to this core's outgoing data slot
            shift = 8 * offset
            mask = 0xFF << shift
            self.data[rid] = (self.data[rid] & ~mask) | (value << shift)
            self.data[rid] &= (1 << 64) - 1
        elif offset == 0x08:  # SEND — value is target core ID
            target = value & 0xFF
            if target < self.num_cores and target != rid:
                # Copy sender's outgoing data to target's inbox
                self.data[target] = self.data[rid]
                self.pending[target] |= (1 << rid)
                if self.on_ipi:
                    self.on_ipi(target)
        elif offset == 0x0A:  # ACK — value is source core ID to clear
            source = value & 0xFF
            if source < self.num_cores:
                self.pending[rid] &= ~(1 << source)
                if self.on_ack:
                    self.on_ack(rid)


# ---------------------------------------------------------------------------
#  Hardware Spinlocks
# ---------------------------------------------------------------------------
# 8 test-and-set spinlocks with owner tracking.
# Read SLOCK_ACQUIRE: returns 0 if acquired, 1 if busy (atomic test-and-set).
# Write SLOCK_RELEASE: releases the lock (only if caller is owner).
#
# Lock N is at SPINLOCK_BASE + N*4.
# Offset within each 4-byte slot:
#   +0  ACQUIRE (R) — read returns 0=got it, 1=busy
#   +1  RELEASE (W) — write anything to release

NUM_SPINLOCKS = 8

class SpinlockDevice(Device):
    """Hardware spinlocks with test-and-set semantics."""

    def __init__(self, num_locks: int = NUM_SPINLOCKS):
        super().__init__("Spinlock", SPINLOCK_BASE, num_locks * 4)
        self.num_locks = num_locks
        # locked[i] = True if lock i is held
        self.locked: list[bool] = [False] * num_locks
        # owner[i] = core_id that holds lock i (-1 if free)
        self.owner: list[int] = [-1] * num_locks
        # Requester context — set by DeviceBus before each access
        self._requester_id: int = 0

    def read8(self, offset: int) -> int:
        lock_idx = offset // 4
        sub = offset % 4
        if lock_idx >= self.num_locks:
            return 0xFF
        rid = self._requester_id
        if sub == 0:  # ACQUIRE (test-and-set)
            if not self.locked[lock_idx]:
                # Free — acquire it
                self.locked[lock_idx] = True
                self.owner[lock_idx] = rid
                return 0  # success
            elif self.owner[lock_idx] == rid:
                # Re-entrant — already own it
                return 0
            else:
                return 1  # busy
        return 0

    def write8(self, offset: int, value: int):
        lock_idx = offset // 4
        sub = offset % 4
        if lock_idx >= self.num_locks:
            return
        rid = self._requester_id
        if sub == 1:  # RELEASE
            if self.locked[lock_idx] and self.owner[lock_idx] == rid:
                self.locked[lock_idx] = False
                self.owner[lock_idx] = -1


# ---------------------------------------------------------------------------
#  AES-256-GCM Accelerator
# ---------------------------------------------------------------------------
# Register map (offsets from AES_BASE = 0x0700):
#   0x00..0x1F  AES_KEY    (W)   — 256-bit key (32 bytes LE)
#   0x20..0x2B  AES_IV     (W)   — 96-bit IV/nonce (12 bytes LE)
#   0x30..0x33  AES_AAD_LEN(W)   — AAD length in bytes (32-bit LE)
#   0x34..0x37  AES_DATA_LEN(W)  — Plaintext/ciphertext length (32-bit LE)
#   0x38        AES_CMD    (W)   — 0=encrypt, 1=decrypt
#   0x39        AES_STATUS (R)   — 0=idle, 1=busy, 2=done, 3=auth-fail
#   0x40..0x4F  AES_DIN    (W)   — 128-bit data input (16 bytes LE)
#   0x50..0x5F  AES_DOUT   (R)   — 128-bit data output (16 bytes LE)
#   0x60..0x6F  AES_TAG    (R/W) — 128-bit GCM authentication tag
#
# Data flow:
#   1. Write key (32 bytes to 0x00..0x1F)
#   2. Write IV  (12 bytes to 0x20..0x2B)
#   3. Write AAD_LEN and DATA_LEN
#   4. Write CMD (0=encrypt / 1=decrypt)
#   5. For each 16-byte block: write DIN, read DOUT
#   6. Read TAG (encrypt) or check STATUS for auth-fail (decrypt)

# --- AES-256 S-Box ---
_AES_SBOX = bytes([
    0x63,0x7C,0x77,0x7B,0xF2,0x6B,0x6F,0xC5,0x30,0x01,0x67,0x2B,0xFE,0xD7,0xAB,0x76,
    0xCA,0x82,0xC9,0x7D,0xFA,0x59,0x47,0xF0,0xAD,0xD4,0xA2,0xAF,0x9C,0xA4,0x72,0xC0,
    0xB7,0xFD,0x93,0x26,0x36,0x3F,0xF7,0xCC,0x34,0xA5,0xE5,0xF1,0x71,0xD8,0x31,0x15,
    0x04,0xC7,0x23,0xC3,0x18,0x96,0x05,0x9A,0x07,0x12,0x80,0xE2,0xEB,0x27,0xB2,0x75,
    0x09,0x83,0x2C,0x1A,0x1B,0x6E,0x5A,0xA0,0x52,0x3B,0xD6,0xB3,0x29,0xE3,0x2F,0x84,
    0x53,0xD1,0x00,0xED,0x20,0xFC,0xB1,0x5B,0x6A,0xCB,0xBE,0x39,0x4A,0x4C,0x58,0xCF,
    0xD0,0xEF,0xAA,0xFB,0x43,0x4D,0x33,0x85,0x45,0xF9,0x02,0x7F,0x50,0x3C,0x9F,0xA8,
    0x51,0xA3,0x40,0x8F,0x92,0x9D,0x38,0xF5,0xBC,0xB6,0xDA,0x21,0x10,0xFF,0xF3,0xD2,
    0xCD,0x0C,0x13,0xEC,0x5F,0x97,0x44,0x17,0xC4,0xA7,0x7E,0x3D,0x64,0x5D,0x19,0x73,
    0x60,0x81,0x4F,0xDC,0x22,0x2A,0x90,0x88,0x46,0xEE,0xB8,0x14,0xDE,0x5E,0x0B,0xDB,
    0xE0,0x32,0x3A,0x0A,0x49,0x06,0x24,0x5C,0xC2,0xD3,0xAC,0x62,0x91,0x95,0xE4,0x79,
    0xE7,0xC8,0x37,0x6D,0x8D,0xD5,0x4E,0xA9,0x6C,0x56,0xF4,0xEA,0x65,0x7A,0xAE,0x08,
    0xBA,0x78,0x25,0x2E,0x1C,0xA6,0xB4,0xC6,0xE8,0xDD,0x74,0x1F,0x4B,0xBD,0x8B,0x8A,
    0x70,0x3E,0xB5,0x66,0x48,0x03,0xF6,0x0E,0x61,0x35,0x57,0xB9,0x86,0xC1,0x1D,0x9E,
    0xE1,0xF8,0x98,0x11,0x69,0xD9,0x8E,0x94,0x9B,0x1E,0x87,0xE9,0xCE,0x55,0x28,0xDF,
    0x8C,0xA1,0x89,0x0D,0xBF,0xE6,0x42,0x68,0x41,0x99,0x2D,0x0F,0xB0,0x54,0xBB,0x16,
])

_AES_RCON = [0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1B,0x36]


def _aes256_key_expand(key_bytes: bytes) -> list[bytes]:
    """Expand 32-byte key into 15 round keys (each 16 bytes)."""
    assert len(key_bytes) == 32
    nk, nr = 8, 14
    w = []
    for i in range(nk):
        w.append(key_bytes[4*i:4*i+4])
    for i in range(nk, 4 * (nr + 1)):
        t = bytearray(w[i-1])
        if i % nk == 0:
            t = bytearray([_AES_SBOX[t[1]], _AES_SBOX[t[2]],
                           _AES_SBOX[t[3]], _AES_SBOX[t[0]]])
            t[0] ^= _AES_RCON[i // nk - 1]
        elif i % nk == 4:
            t = bytearray([_AES_SBOX[b] for b in t])
        w.append(bytes(a ^ b for a, b in zip(w[i - nk], t)))
    rkeys = []
    for r in range(nr + 1):
        rkeys.append(b''.join(w[4*r:4*r+4]))
    return rkeys


def _aes_encrypt_block(block: bytes, rkeys: list[bytes]) -> bytes:
    """Encrypt a single 16-byte block with AES-256."""
    assert len(block) == 16
    s = bytearray(a ^ b for a, b in zip(block, rkeys[0]))
    for r in range(1, 14):
        # SubBytes
        s = bytearray(_AES_SBOX[b] for b in s)
        # ShiftRows
        s = bytearray([
            s[0], s[5], s[10], s[15],
            s[4], s[9], s[14], s[3],
            s[8], s[13], s[2], s[7],
            s[12], s[1], s[6], s[11],
        ])
        # MixColumns
        t = bytearray(16)
        for c in range(4):
            a0, a1, a2, a3 = s[4*c], s[4*c+1], s[4*c+2], s[4*c+3]
            t[4*c]   = _gm2(a0) ^ _gm3(a1) ^ a2 ^ a3
            t[4*c+1] = a0 ^ _gm2(a1) ^ _gm3(a2) ^ a3
            t[4*c+2] = a0 ^ a1 ^ _gm2(a2) ^ _gm3(a3)
            t[4*c+3] = _gm3(a0) ^ a1 ^ a2 ^ _gm2(a3)
        s = t
        # AddRoundKey
        s = bytearray(a ^ b for a, b in zip(s, rkeys[r]))
    # Final round (no MixColumns)
    s = bytearray(_AES_SBOX[b] for b in s)
    s = bytearray([
        s[0], s[5], s[10], s[15],
        s[4], s[9], s[14], s[3],
        s[8], s[13], s[2], s[7],
        s[12], s[1], s[6], s[11],
    ])
    return bytes(a ^ b for a, b in zip(s, rkeys[14]))


def _gm2(v):
    """Galois field multiply by 2 in GF(2^8)."""
    return ((v << 1) ^ (0x1B if v & 0x80 else 0)) & 0xFF

def _gm3(v):
    """Galois field multiply by 3 in GF(2^8)."""
    return _gm2(v) ^ v


def _ghash_mult(x: int, h: int) -> int:
    """GF(2^128) multiplication for GHASH (big-endian bit order)."""
    # x, h are 128-bit integers (MSB-first as in GCM spec)
    R = 0xE1000000000000000000000000000000  # reduction polynomial
    z = 0
    v = h
    for i in range(128):
        if (x >> (127 - i)) & 1:
            z ^= v
        if v & 1:
            v = (v >> 1) ^ R
        else:
            v >>= 1
    return z


def _bytes_to_int128(b: bytes) -> int:
    """Convert 16 bytes (big-endian) to 128-bit integer."""
    return int.from_bytes(b, 'big')

def _int128_to_bytes(n: int) -> bytes:
    """Convert 128-bit integer to 16 bytes (big-endian)."""
    return n.to_bytes(16, 'big')


def _inc32(counter: bytearray):
    """Increment the rightmost 32 bits of a 16-byte counter."""
    for i in range(15, 11, -1):
        counter[i] = (counter[i] + 1) & 0xFF
        if counter[i] != 0:
            break


class AESDevice(Device):
    """AES-256-GCM hardware accelerator."""

    def __init__(self):
        super().__init__("AES", AES_BASE, 0x70)
        self._reset()

    def _reset(self):
        self.key = bytearray(32)
        self.iv = bytearray(12)
        self.aad_len = 0
        self.data_len = 0
        self.cmd = 0          # 0=encrypt, 1=decrypt
        self.status = 0       # 0=idle, 2=done, 3=auth-fail
        self.din = bytearray(16)
        self.dout = bytearray(16)
        self.tag = bytearray(16)
        # Internal GCM state
        self._rkeys = None
        self._h = 0           # GHASH subkey H = AES_K(0^128)
        self._counter = bytearray(16)
        self._j0 = bytearray(16)  # initial counter
        self._ghash_state = 0
        self._aad_processed = 0
        self._data_processed = 0
        self._expected_tag = bytearray(16)

    def read8(self, offset: int) -> int:
        if 0x39 <= offset < 0x3A:
            return self.status
        elif 0x50 <= offset < 0x60:
            return self.dout[offset - 0x50]
        elif 0x60 <= offset < 0x70:
            return self.tag[offset - 0x60]
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if 0x00 <= offset < 0x20:
            self.key[offset] = value
        elif 0x20 <= offset < 0x2C:
            self.iv[offset - 0x20] = value
        elif 0x30 <= offset < 0x34:
            idx = offset - 0x30
            self.aad_len = (self.aad_len & ~(0xFF << (8*idx))) | (value << (8*idx))
            self.aad_len &= 0xFFFFFFFF
        elif 0x34 <= offset < 0x38:
            idx = offset - 0x34
            self.data_len = (self.data_len & ~(0xFF << (8*idx))) | (value << (8*idx))
            self.data_len &= 0xFFFFFFFF
        elif offset == 0x38:
            # CMD — triggers key expansion and GCM init
            self.cmd = value & 1
            self._start_gcm()
        elif 0x40 <= offset < 0x50:
            idx = offset - 0x40
            self.din[idx] = value
            if idx == 15:
                # Full 16-byte block written — process it
                self._process_block()
        elif 0x60 <= offset < 0x70:
            # Write expected tag (for decrypt verification)
            self.tag[offset - 0x60] = value

    def _start_gcm(self):
        """Initialize GCM state: expand key, compute H, set up counter."""
        self._rkeys = _aes256_key_expand(bytes(self.key))
        # H = AES_K(0^128)
        h_bytes = _aes_encrypt_block(b'\x00' * 16, self._rkeys)
        self._h = _bytes_to_int128(h_bytes)
        # J0 = IV || 0x00000001
        self._j0 = bytearray(self.iv) + bytearray([0, 0, 0, 1])
        self._counter = bytearray(self._j0)
        self._ghash_state = 0
        self._aad_processed = 0
        self._data_processed = 0
        self.status = 2  # done (ready for blocks)
        # If there's AAD, it should be fed via DIN blocks before data.
        # For simplicity, we handle AAD + data as sequential DIN writes.
        # The first aad_len bytes of DIN writes are AAD, remaining are data.

    def _process_block(self):
        """Process one 16-byte DIN block (AAD or data)."""
        if self._rkeys is None:
            return
        block = bytes(self.din)

        if self._aad_processed < self.aad_len:
            # This block is AAD — just feed to GHASH
            self._ghash_update(block)
            self._aad_processed += 16
            self.dout = bytearray(16)  # no output for AAD
            if self._aad_processed >= self.aad_len:
                # Pad AAD to 128-bit boundary is implicit (we always write full blocks)
                pass
        else:
            # This block is data — encrypt/decrypt via CTR
            _inc32(self._counter)
            keystream = _aes_encrypt_block(bytes(self._counter), self._rkeys)
            out = bytearray(a ^ b for a, b in zip(block, keystream))
            self.dout = out
            self._data_processed += 16

            # GHASH: encrypt feeds ciphertext, decrypt feeds ciphertext (=input)
            if self.cmd == 0:
                self._ghash_update(bytes(out))   # encrypt: hash ciphertext
            else:
                self._ghash_update(block)        # decrypt: hash ciphertext (=input)

            # Check if all data processed → compute tag
            if self._data_processed >= self.data_len:
                self._finalize_tag()

    def _ghash_update(self, block: bytes):
        """Feed a 16-byte block into the GHASH accumulator."""
        x = _bytes_to_int128(block)
        self._ghash_state = _ghash_mult(self._ghash_state ^ x, self._h)

    def _finalize_tag(self):
        """Compute the GCM authentication tag."""
        # Final GHASH block: lengths (AAD bits || data bits), each 64-bit BE
        len_block = (self.aad_len * 8).to_bytes(8, 'big') + \
                    (self.data_len * 8).to_bytes(8, 'big')
        self._ghash_update(len_block)
        # Tag = GHASH_final XOR AES_K(J0)
        s = _int128_to_bytes(self._ghash_state)
        j0_enc = _aes_encrypt_block(bytes(self._j0), self._rkeys)
        computed_tag = bytes(a ^ b for a, b in zip(s, j0_enc))
        if self.cmd == 0:
            # Encrypt: store computed tag
            self.tag = bytearray(computed_tag)
            self.status = 2  # done
        else:
            # Decrypt: compare with expected tag
            if computed_tag == bytes(self.tag):
                self.status = 2  # done, auth OK
            else:
                self.status = 3  # auth fail


# ---------------------------------------------------------------------------
#  SHA-3 (Keccak) Accelerator — SHA3-256
# ---------------------------------------------------------------------------
# Register map (offsets from SHA3_BASE = 0x0780):
#   0x00        SHA3_CMD     (W)   — 0=init (SHA3-256), 1=finalize
#   0x01        SHA3_STATUS  (R)   — 0=idle, 2=done
#   0x08        SHA3_DIN     (W)   — byte input (auto-absorbs at rate)
#   0x10..0x2F  SHA3_DOUT    (R)   — 32-byte hash output (after finalize)

# Keccak round constants
_KECCAK_RC = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808A,
    0x8000000080008000, 0x000000000000808B, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008A,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000A,
    0x000000008000808B, 0x800000000000008B, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800A, 0x800000008000000A, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008,
]

# Keccak rotation offsets (rho), indexed as [x + 5*y]
_KECCAK_ROT = [
     0,  1, 62, 28, 27,    # y=0
    36, 44,  6, 55, 20,    # y=1
     3, 10, 43, 25, 39,    # y=2
    41, 45, 15, 21,  8,    # y=3
    18,  2, 61, 56, 14,    # y=4
]

def _keccak_f1600(state: list[int]) -> list[int]:
    """Keccak-f[1600] permutation on flat array of 25 64-bit lanes (x+5y)."""
    lanes = list(state)
    M64 = 0xFFFFFFFFFFFFFFFF
    def _rot64(x: int, n: int) -> int:
        return ((x << n) | (x >> (64 - n))) & M64 if n else x
    for rc in _KECCAK_RC:
        # θ — column parity
        C = [lanes[x] ^ lanes[x+5] ^ lanes[x+10] ^ lanes[x+15] ^ lanes[x+20]
             for x in range(5)]
        D = [C[(x-1) % 5] ^ _rot64(C[(x+1) % 5], 1) for x in range(5)]
        lanes = [(lanes[i] ^ D[i % 5]) & M64 for i in range(25)]
        # ρ + π
        B = [0] * 25
        for x in range(5):
            for y in range(5):
                src = x + 5 * y
                dst = y + 5 * ((2*x + 3*y) % 5)
                B[dst] = _rot64(lanes[src], _KECCAK_ROT[src])
        # χ
        lanes = [
            (B[x + 5*y] ^ ((~B[((x+1)%5) + 5*y]) & B[((x+2)%5) + 5*y])) & M64
            for y in range(5) for x in range(5)
        ]
        # ι
        lanes[0] = (lanes[0] ^ rc) & M64
    return lanes


class SHA3Device(Device):
    """SHA-3/SHAKE hardware accelerator (matches FPGA mp64_sha3.v).

    Modes: 0=SHA3-256 (rate=136, out=32), 1=SHA3-512 (rate=72, out=64),
           2=SHAKE128 (rate=168, XOF),    3=SHAKE256 (rate=136, XOF).

    Register map (offsets from SHA3_BASE = 0x0780):
      0x00  CMD      (W)  0=NOP, 1=INIT, 2=ABSORB (internal), 3=FINAL,
                          4=SQUEEZE, 5=SQUEEZE_NEXT (auto-permute for streaming)
      0x01  STATUS   (R)  bit 0=busy, bit 1=done
      0x02  CTRL     (W)  mode select (bits [1:0])
      0x08  DIN      (W)  byte input (auto-absorbs at rate boundary)
      0x10..0x4F  DOUT (R)  hash/XOF output (up to 64 bytes)
    """

    _RATES = {0: 136, 1: 72, 2: 168, 3: 136}
    _OUTSZ = {0: 32, 1: 64, 2: 0, 3: 0}  # 0 = extendable (XOF)
    _DSEP  = {0: 0x06, 1: 0x06, 2: 0x1F, 3: 0x1F}  # domain separator

    def __init__(self):
        super().__init__("SHA3", SHA3_BASE, 0x50)
        self.mode = 0
        self._reset()

    def _reset(self):
        self.state = [0] * 25
        self.buf = bytearray()
        self.status = 0
        self.digest = bytearray(64)
        self._squeezed = 0  # bytes already squeezed (for XOF)
        self._stream_pos = 0  # byte position within current squeeze block
        self._squeeze_buf = bytearray()  # full rate output for streaming

    @property
    def rate(self) -> int:
        return self._RATES[self.mode]

    def read8(self, offset: int) -> int:
        if offset == 0x01:
            return self.status
        if offset == 0x02:
            return self.mode
        if 0x10 <= offset < 0x50:
            return self.digest[offset - 0x10] if (offset - 0x10) < len(self.digest) else 0
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x00:  # CMD
            if value == 1:    # INIT
                self._reset()
            elif value == 3:  # FINAL
                self._finalize()
            elif value == 4:  # SQUEEZE (XOF: permute and extract more)
                self._squeeze()
            elif value == 5:  # SQUEEZE_NEXT (streaming: advance 32-byte window)
                self._squeeze_next_stream()
        elif offset == 0x02:  # CTRL — mode select
            self.mode = value & 0x03
        elif offset == 0x08:  # DIN
            self.buf.append(value)
            if len(self.buf) == self.rate:
                self._absorb_block()

    def _absorb_block(self):
        block = self.buf
        for i in range(self.rate // 8):
            lane = int.from_bytes(block[i*8:(i+1)*8], 'little')
            self.state[i] ^= lane
        self.state = _keccak_f1600(self.state)
        self.buf = bytearray()

    def _finalize(self):
        pad = bytearray(self.rate)
        pad[:len(self.buf)] = self.buf
        pad[len(self.buf)] = self._DSEP[self.mode]
        pad[-1] |= 0x80
        for i in range(self.rate // 8):
            lane = int.from_bytes(pad[i*8:(i+1)*8], 'little')
            self.state[i] ^= lane
        self.state = _keccak_f1600(self.state)
        # Squeeze first block of output
        out = bytearray()
        for i in range(self.rate // 8):
            out.extend(self.state[i].to_bytes(8, 'little'))
        outsz = self._OUTSZ[self.mode]
        if outsz > 0:
            self.digest = out[:outsz]
        else:
            self.digest = out[:self.rate]
        # Initialise streaming buffer for SQUEEZE-NEXT
        self._squeeze_buf = bytearray(out[:self.rate])
        self._stream_pos = 0
        self._squeezed = len(self.digest)
        self.status = 2  # done

    def _squeeze(self):
        """SHAKE XOF: apply Keccak-f again and extract another rate block."""
        self.state = _keccak_f1600(self.state)
        out = bytearray()
        for i in range(self.rate // 8):
            out.extend(self.state[i].to_bytes(8, 'little'))
        self.digest = out[:self.rate]
        self._squeezed += self.rate
        self.status = 2

    def _squeeze_next_stream(self):
        """Streaming SQUEEZE: advance DOUT window by 32 bytes.

        Walks through the current rate block 32 bytes at a time.
        When the window would exceed the buffered output, apply
        Keccak-f to produce the next rate block and append it.
        """
        self._stream_pos += 32
        # Extend squeeze buffer if the 64-byte DOUT window would overrun
        while self._stream_pos + 64 > len(self._squeeze_buf):
            self.state = _keccak_f1600(self.state)
            out = bytearray()
            for i in range(self.rate // 8):
                out.extend(self.state[i].to_bytes(8, 'little'))
            self._squeeze_buf.extend(out[:self.rate])
        # Update visible DOUT to show current 64-byte window
        self.digest = bytearray(
            self._squeeze_buf[self._stream_pos:self._stream_pos + 64]
        )
        self._squeezed = self._stream_pos + 64
        self.status = 2


# ---------------------------------------------------------------------------
#  ML-KEM-512 (Kyber) Key Encapsulation Mechanism Accelerator
# ---------------------------------------------------------------------------
# Implements FIPS 203 ML-KEM-512 entirely in the emulator.
# Register map (offsets from KEM_BASE):
#   0x00  STATUS  (R)   0=idle, 2=done
#   0x01  CMD     (W)   1=KEYGEN, 2=ENCAPS, 3=DECAPS
#   0x08  BUF_SEL (W)   Buffer select: 0=SEED(64B) 1=PK(800B)
#                        2=SK(1632B) 3=CT(768B) 4=SS(32B)
#                        Writing resets byte index to 0.
#   0x10  DIN     (W)   Write byte to selected buffer, auto-inc
#   0x18  DOUT    (R)   Read byte from selected buffer, auto-inc
#   0x20  BUF_SIZE(R,2B LE) Size of selected buffer

import hashlib as _hashlib

# --- ML-KEM-512 parameters ---------------------------------------------------
_KEM_N   = 256
_KEM_K   = 2
_KEM_Q   = 3329
_KEM_ETA1 = 3
_KEM_ETA2 = 2
_KEM_DU  = 10
_KEM_DV  = 4

def _bit_rev7(x: int) -> int:
    r = 0
    for _ in range(7):
        r = (r << 1) | (x & 1)
        x >>= 1
    return r

# Pre-compute NTT twiddle factors: ζ = 17 (primitive 256th root of unity mod q)
_KEM_ZETAS = [pow(17, _bit_rev7(i), _KEM_Q) for i in range(128)]

def _kem_ntt(f: list[int]) -> list[int]:
    """FIPS 203 Algorithm 9 — Number Theoretic Transform."""
    fh = list(f)
    i = 1
    length = 128
    while length >= 2:
        for start in range(0, 256, 2 * length):
            z = _KEM_ZETAS[i]; i += 1
            for j in range(start, start + length):
                t = (z * fh[j + length]) % _KEM_Q
                fh[j + length] = (fh[j] - t) % _KEM_Q
                fh[j] = (fh[j] + t) % _KEM_Q
        length //= 2
    return fh

def _kem_intt(fh: list[int]) -> list[int]:
    """FIPS 203 Algorithm 10 — Inverse NTT."""
    f = list(fh)
    i = 127
    length = 2
    while length <= 128:
        for start in range(0, 256, 2 * length):
            z = _KEM_ZETAS[i]; i -= 1
            for j in range(start, start + length):
                t = f[j]
                f[j] = (t + f[j + length]) % _KEM_Q
                f[j + length] = (z * (f[j + length] - t)) % _KEM_Q
        length *= 2
    return [(x * 3303) % _KEM_Q for x in f]  # 3303 = 128⁻¹ mod q

def _kem_basemul(fh: list[int], gh: list[int]) -> list[int]:
    """FIPS 203 Algorithm 11 — Multiply NTT-domain polynomials.

    Processes 64 groups of 4 coefficients.  Each group uses zetas[64+i]
    for the first pair and -zetas[64+i] for the second pair, matching
    the reference pqcrystals-kyber implementation.
    """
    h = [0] * 256
    for i in range(64):
        z = _KEM_ZETAS[64 + i]
        nz = (-z) % _KEM_Q
        # First pair (4i, 4i+1)
        a0, a1 = fh[4*i], fh[4*i+1]
        b0, b1 = gh[4*i], gh[4*i+1]
        h[4*i]   = (a0*b0 + a1*b1*z) % _KEM_Q
        h[4*i+1] = (a0*b1 + a1*b0) % _KEM_Q
        # Second pair (4i+2, 4i+3) — negated zeta
        a0, a1 = fh[4*i+2], fh[4*i+3]
        b0, b1 = gh[4*i+2], gh[4*i+3]
        h[4*i+2] = (a0*b0 + a1*b1*nz) % _KEM_Q
        h[4*i+3] = (a0*b1 + a1*b0) % _KEM_Q
    return h

def _kem_add(a: list[int], b: list[int]) -> list[int]:
    return [(a[i] + b[i]) % _KEM_Q for i in range(256)]

def _kem_sub(a: list[int], b: list[int]) -> list[int]:
    return [(a[i] - b[i]) % _KEM_Q for i in range(256)]

# --- CBD / Encode / Decode / Compress ----------------------------------------

def _kem_cbd(eta: int, B: bytes) -> list[int]:
    """FIPS 203 Algorithm 7 — Centered Binomial Distribution."""
    bits = []
    for byte in B:
        for j in range(8):
            bits.append((byte >> j) & 1)
    f = [0] * 256
    for i in range(256):
        x = sum(bits[2*i*eta + j] for j in range(eta))
        y = sum(bits[2*i*eta + eta + j] for j in range(eta))
        f[i] = (x - y) % _KEM_Q
    return f

def _kem_byte_encode(F: list[int], d: int) -> bytes:
    """FIPS 203 Algorithm 4 — ByteEncode_d."""
    bits: list[int] = []
    for c in F:
        for j in range(d):
            bits.append((c >> j) & 1)
    B = bytearray(32 * d)
    for i, b in enumerate(bits):
        B[i >> 3] |= b << (i & 7)
    return bytes(B)

def _kem_byte_decode(B: bytes, d: int) -> list[int]:
    """FIPS 203 Algorithm 5 — ByteDecode_d."""
    bits: list[int] = []
    for byte in B:
        for j in range(8):
            bits.append((byte >> j) & 1)
    F = [0] * 256
    m = (1 << d) - 1 if d < 12 else _KEM_Q
    for i in range(256):
        v = 0
        for j in range(d):
            v |= bits[i*d + j] << j
        F[i] = v % (m + 1) if d < 12 else v % _KEM_Q
    return F

def _kem_compress(x: int, d: int) -> int:
    return ((x * (1 << d) + _KEM_Q // 2) // _KEM_Q) % (1 << d)

def _kem_decompress(y: int, d: int) -> int:
    return (y * _KEM_Q + (1 << (d - 1))) >> d

def _kem_compress_poly(f: list[int], d: int) -> list[int]:
    return [_kem_compress(c, d) for c in f]

def _kem_decompress_poly(f: list[int], d: int) -> list[int]:
    return [_kem_decompress(c, d) for c in f]

# --- Hash helpers (SHA3/SHAKE via hashlib) ------------------------------------

def _kem_G(x: bytes) -> bytes:
    """G = SHA3-512."""
    return _hashlib.sha3_512(x).digest()

def _kem_H(x: bytes) -> bytes:
    """H = SHA3-256."""
    return _hashlib.sha3_256(x).digest()

def _kem_J(x: bytes) -> bytes:
    """J = SHAKE-256, 32-byte output."""
    return _hashlib.shake_256(x).digest(32)

def _kem_XOF(rho: bytes, i: int, j: int) -> bytes:
    """XOF = SHAKE-128(ρ ‖ i ‖ j) — enough for SampleNTT."""
    return _hashlib.shake_128(rho + bytes([i, j])).digest(840)

def _kem_PRF(eta: int, s: bytes, N: int) -> bytes:
    """PRF = SHAKE-256(s ‖ N) with 64·η output bytes."""
    return _hashlib.shake_256(s + bytes([N])).digest(64 * eta)

def _kem_sample_ntt(xof: bytes) -> list[int]:
    """FIPS 203 Algorithm 6 — SampleNTT (rejection sampling)."""
    a: list[int] = []
    i = 0
    while len(a) < 256:
        d1 = xof[i] + 256 * (xof[i+1] % 16)
        d2 = (xof[i+1] >> 4) + 16 * xof[i+2]
        if d1 < _KEM_Q:
            a.append(d1)
        if d2 < _KEM_Q and len(a) < 256:
            a.append(d2)
        i += 3
    return a

# --- K-PKE (inner CPA-secure scheme) -----------------------------------------

def _kem_pke_keygen(d: bytes) -> tuple[bytes, bytes]:
    """FIPS 203 Algorithm 13 — K-PKE.KeyGen."""
    g = _kem_G(d + bytes([_KEM_K]))
    rho, sigma = g[:32], g[32:]
    A_hat = [[_kem_sample_ntt(_kem_XOF(rho, j, i))
              for j in range(_KEM_K)] for i in range(_KEM_K)]
    N = 0
    s = []
    for _ in range(_KEM_K):
        s.append(_kem_cbd(_KEM_ETA1, _kem_PRF(_KEM_ETA1, sigma, N))); N += 1
    e = []
    for _ in range(_KEM_K):
        e.append(_kem_cbd(_KEM_ETA1, _kem_PRF(_KEM_ETA1, sigma, N))); N += 1
    s_hat = [_kem_ntt(si) for si in s]
    e_hat = [_kem_ntt(ei) for ei in e]
    t_hat = []
    for i in range(_KEM_K):
        ti = [0] * 256
        for j in range(_KEM_K):
            ti = _kem_add(ti, _kem_basemul(A_hat[i][j], s_hat[j]))
        t_hat.append(_kem_add(ti, e_hat[i]))
    ek = bytearray()
    for i in range(_KEM_K):
        ek.extend(_kem_byte_encode(t_hat[i], 12))
    ek.extend(rho)
    dk = bytearray()
    for i in range(_KEM_K):
        dk.extend(_kem_byte_encode(s_hat[i], 12))
    return bytes(ek), bytes(dk)

def _kem_pke_encrypt(ek: bytes, m: bytes, r: bytes) -> bytes:
    """FIPS 203 Algorithm 14 — K-PKE.Encrypt."""
    t_hat = [_kem_byte_decode(ek[384*i:384*(i+1)], 12) for i in range(_KEM_K)]
    rho = ek[384*_KEM_K:]
    A_hat = [[_kem_sample_ntt(_kem_XOF(rho, j, i))
              for j in range(_KEM_K)] for i in range(_KEM_K)]
    N = 0
    rv = []
    for _ in range(_KEM_K):
        rv.append(_kem_cbd(_KEM_ETA1, _kem_PRF(_KEM_ETA1, r, N))); N += 1
    e1 = []
    for _ in range(_KEM_K):
        e1.append(_kem_cbd(_KEM_ETA2, _kem_PRF(_KEM_ETA2, r, N))); N += 1
    e2 = _kem_cbd(_KEM_ETA2, _kem_PRF(_KEM_ETA2, r, N))
    r_hat = [_kem_ntt(ri) for ri in rv]
    u = []
    for i in range(_KEM_K):
        ui = [0] * 256
        for j in range(_KEM_K):
            ui = _kem_add(ui, _kem_basemul(A_hat[j][i], r_hat[j]))
        u.append(_kem_add(_kem_intt(ui), e1[i]))
    mu = _kem_decompress_poly(_kem_byte_decode(m, 1), 1)
    v = [0] * 256
    for i in range(_KEM_K):
        v = _kem_add(v, _kem_basemul(t_hat[i], r_hat[i]))
    v = _kem_add(_kem_add(_kem_intt(v), e2), mu)
    c1 = bytearray()
    for i in range(_KEM_K):
        c1.extend(_kem_byte_encode(_kem_compress_poly(u[i], _KEM_DU), _KEM_DU))
    c2 = _kem_byte_encode(_kem_compress_poly(v, _KEM_DV), _KEM_DV)
    return bytes(c1) + bytes(c2)

def _kem_pke_decrypt(dk: bytes, c: bytes) -> bytes:
    """FIPS 203 Algorithm 15 — K-PKE.Decrypt."""
    du_chunk = 32 * _KEM_DU   # 320
    c1, c2 = c[:du_chunk * _KEM_K], c[du_chunk * _KEM_K:]
    u = [_kem_decompress_poly(_kem_byte_decode(c1[du_chunk*i:du_chunk*(i+1)],
         _KEM_DU), _KEM_DU) for i in range(_KEM_K)]
    v = _kem_decompress_poly(_kem_byte_decode(c2, _KEM_DV), _KEM_DV)
    s_hat = [_kem_byte_decode(dk[384*i:384*(i+1)], 12) for i in range(_KEM_K)]
    u_hat = [_kem_ntt(ui) for ui in u]
    inner = [0] * 256
    for i in range(_KEM_K):
        inner = _kem_add(inner, _kem_basemul(s_hat[i], u_hat[i]))
    w = _kem_sub(v, _kem_intt(inner))
    return bytes(_kem_byte_encode(_kem_compress_poly(w, 1), 1))

# --- ML-KEM (CCA-secure KEM) -------------------------------------------------

def _kem_keygen(d: bytes, z: bytes) -> tuple[bytes, bytes]:
    """FIPS 203 Algorithm 16 — ML-KEM.KeyGen."""
    ek, dk_pke = _kem_pke_keygen(d)
    dk = dk_pke + ek + _kem_H(ek) + z
    return ek, dk

def _kem_encaps(ek: bytes, coin: bytes) -> tuple[bytes, bytes]:
    """FIPS 203 Algorithm 17 — ML-KEM.Encaps.
    Returns (shared_secret, ciphertext)."""
    g = _kem_G(coin + _kem_H(ek))
    K, r = g[:32], g[32:]
    c = _kem_pke_encrypt(ek, coin, r)
    return K, c

def _kem_decaps(c: bytes, dk: bytes) -> bytes:
    """FIPS 203 Algorithm 18 — ML-KEM.Decaps."""
    dk_pke = dk[:384 * _KEM_K]
    ek = dk[384 * _KEM_K : 384 * _KEM_K + 384 * _KEM_K + 32]
    h  = dk[384 * _KEM_K + 384 * _KEM_K + 32 :
            384 * _KEM_K + 384 * _KEM_K + 64]
    z  = dk[384 * _KEM_K + 384 * _KEM_K + 64 :]
    m_prime = _kem_pke_decrypt(dk_pke, c)
    g = _kem_G(m_prime + h)
    K_prime, r_prime = g[:32], g[32:]
    K_bar = _kem_J(z + c)
    c_prime = _kem_pke_encrypt(ek, m_prime, r_prime)
    return K_prime if c == c_prime else K_bar


class KemDevice(Device):
    """ML-KEM-512 (Kyber) hardware KEM accelerator.

    Buffers: 0=SEED/COIN(64B) 1=PK(800B) 2=SK(1632B) 3=CT(768B) 4=SS(32B).
    """

    _BUF_SIZES = [64, 800, 1632, 768, 32]

    def __init__(self):
        super().__init__("KEM", KEM_BASE, 0x28)   # 40-byte register window
        self._reset_kem()

    def _reset_kem(self):
        self.status = 0
        self._buf_sel = 0
        self._buf_idx = 0
        self._bufs = [bytearray(s) for s in self._BUF_SIZES]

    def read8(self, offset: int) -> int:
        if offset == 0x00:
            return self.status
        if offset == 0x18:                        # DOUT
            buf = self._bufs[self._buf_sel]
            if self._buf_idx < len(buf):
                v = buf[self._buf_idx]
                self._buf_idx += 1
                return v
            return 0
        if offset == 0x20:
            return self._BUF_SIZES[self._buf_sel] & 0xFF
        if offset == 0x21:
            return (self._BUF_SIZES[self._buf_sel] >> 8) & 0xFF
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x01:                        # CMD
            if value == 1:
                self._do_keygen()
            elif value == 2:
                self._do_encaps()
            elif value == 3:
                self._do_decaps()
        elif offset == 0x08:                      # BUF_SEL
            self._buf_sel = min(value, 4)
            self._buf_idx = 0
        elif offset == 0x10:                      # DIN
            buf = self._bufs[self._buf_sel]
            if self._buf_idx < len(buf):
                buf[self._buf_idx] = value
                self._buf_idx += 1

    def _do_keygen(self):
        d = bytes(self._bufs[0][:32])
        z = bytes(self._bufs[0][32:64])
        ek, dk = _kem_keygen(d, z)
        self._bufs[1] = bytearray(ek)
        self._bufs[2] = bytearray(dk)
        self.status = 2

    def _do_encaps(self):
        ek   = bytes(self._bufs[1])
        coin = bytes(self._bufs[0][:32])
        K, c = _kem_encaps(ek, coin)
        self._bufs[3] = bytearray(c)
        self._bufs[4] = bytearray(K)
        self.status = 2

    def _do_decaps(self):
        c  = bytes(self._bufs[3])
        dk = bytes(self._bufs[2])
        K  = _kem_decaps(c, dk)
        self._bufs[4] = bytearray(K)
        self.status = 2


# ---------------------------------------------------------------------------
#  CRC Accelerator — Hardware CRC32 / CRC32C / CRC64
# ---------------------------------------------------------------------------
# Register map (offsets from CRC_BASE):
#   0x00  CRC_POLY    (W)   — polynomial select: 0=CRC32, 1=CRC32C, 2=CRC64
#   0x08  CRC_INIT    (W)   — initial CRC value (64-bit LE)
#   0x10  CRC_DIN     (W)   — data input (8 bytes LE, processes on write to 0x17)
#   0x18  CRC_RESULT  (R)   — current CRC value (64-bit LE)
#   0x20  CRC_CTRL    (W)   — 0=reset to init, 1=finalize (XOR-out)

_CRC32_POLY     = 0x04C11DB7
_CRC32C_POLY    = 0x1EDC6F41
_CRC64_ECMA_POLY = 0x42F0E1EBA9EA3693

def _crc_table(poly: int, bits: int) -> list[int]:
    """Build a 256-entry CRC lookup table."""
    mask = (1 << bits) - 1
    top = 1 << (bits - 1)
    tbl = []
    for i in range(256):
        r = i << (bits - 8)
        for _ in range(8):
            if r & top:
                r = ((r << 1) ^ poly) & mask
            else:
                r = (r << 1) & mask
        tbl.append(r)
    return tbl

_TBL_CRC32  = _crc_table(_CRC32_POLY, 32)
_TBL_CRC32C = _crc_table(_CRC32C_POLY, 32)
_TBL_CRC64  = _crc_table(_CRC64_ECMA_POLY, 64)

class CRCDevice(Device):
    """Hardware CRC accelerator supporting CRC32, CRC32C, and CRC64-ECMA."""

    def __init__(self):
        super().__init__("CRC", CRC_BASE, 0x28)
        self.poly_sel = 0       # 0=CRC32, 1=CRC32C, 2=CRC64
        self.init_val = 0xFFFFFFFF
        self.crc = 0xFFFFFFFF
        self.din_buf = bytearray(8)
        self.din_idx = 0

    def _bits(self) -> int:
        return 64 if self.poly_sel == 2 else 32

    def _mask(self) -> int:
        return (1 << self._bits()) - 1

    def _table(self) -> list[int]:
        if self.poly_sel == 0:
            return _TBL_CRC32
        elif self.poly_sel == 1:
            return _TBL_CRC32C
        else:
            return _TBL_CRC64

    def _feed_byte(self, b: int):
        bits = self._bits()
        tbl = self._table()
        mask = self._mask()
        idx = ((self.crc >> (bits - 8)) ^ b) & 0xFF
        self.crc = ((self.crc << 8) ^ tbl[idx]) & mask

    def _feed_data(self):
        """Process the 8-byte DIN buffer."""
        for i in range(8):
            self._feed_byte(self.din_buf[i])

    def read8(self, offset: int) -> int:
        # CRC_RESULT at offset 0x18..0x1F (8 bytes LE)
        if 0x18 <= offset < 0x20:
            byte_idx = offset - 0x18
            return (self.crc >> (8 * byte_idx)) & 0xFF
        return 0

    def write8(self, offset: int, value: int):
        if 0x00 <= offset < 0x08:
            # CRC_POLY — only care about byte 0
            if offset == 0x00:
                self.poly_sel = value & 0x03
        elif 0x08 <= offset < 0x10:
            # CRC_INIT — 8-byte LE write buffer
            byte_idx = offset - 0x08
            if byte_idx == 0:
                self.init_val = value
            else:
                self.init_val = (self.init_val & ~(0xFF << (8 * byte_idx))) | (value << (8 * byte_idx))
            if byte_idx == 7:
                self.init_val &= self._mask()
                self.crc = self.init_val
        elif 0x10 <= offset < 0x18:
            # CRC_DIN — buffer 8 bytes, process on last byte write
            byte_idx = offset - 0x10
            self.din_buf[byte_idx] = value & 0xFF
            if byte_idx == 7:
                self._feed_data()
        elif 0x20 <= offset < 0x28:
            # CRC_CTRL
            if offset == 0x20:
                if value == 0:
                    self.crc = self.init_val
                elif value == 1:
                    # Finalize: XOR with all-ones mask
                    self.crc ^= self._mask()


# ---------------------------------------------------------------------------
#  True Random Number Generator (TRNG)
# ---------------------------------------------------------------------------
# Hardware TRNG backed by os.urandom() in the emulator.
# On real FPGA, ring-oscillator jitter + SHA-3 conditioner.
#
# Register map (offsets from TRNG_BASE = 0x0800):
#   0x00        RAND8    (R)  — read 1 random byte
#   0x08..0x0F  RAND64   (R)  — read 8 random bytes (64-bit LE)
#   0x10        STATUS   (R)  — always 1 (entropy available)
#   0x18..0x1F  SEED     (W)  — write 64-bit seed to mix into pool

import os as _os

class TRNGDevice(Device):
    """True Random Number Generator (CSPRNG-backed in emulator)."""

    def __init__(self):
        super().__init__("TRNG", TRNG_BASE, 0x20)
        # Internal entropy pool — seeded from os.urandom
        self._pool = bytearray(_os.urandom(64))
        self._pool_pos = 0

    def _next_byte(self) -> int:
        """Draw one byte from pool, refill if exhausted."""
        if self._pool_pos >= len(self._pool):
            self._pool = bytearray(_os.urandom(64))
            self._pool_pos = 0
        b = self._pool[self._pool_pos]
        self._pool_pos += 1
        return b

    def read8(self, offset: int) -> int:
        if offset == 0x00:           # RAND8
            return self._next_byte()
        if 0x08 <= offset < 0x10:    # RAND64 — each byte read is independent
            return self._next_byte()
        if offset == 0x10:           # STATUS — always ready
            return 1
        return 0

    def write8(self, offset: int, value: int):
        if 0x18 <= offset < 0x20:    # SEED — mix into pool
            idx = offset - 0x18
            if idx < len(self._pool):
                self._pool[idx] ^= (value & 0xFF)


# ---------------------------------------------------------------------------
#  Field ALU — General GF(2²⁵⁵−19) Coprocessor + Raw 256×256 Multiplier
# ---------------------------------------------------------------------------
# Expanded from X25519 ECDH accelerator.  Mode 0 is backward-compatible
# with the original X25519 scalar multiplication interface.
#
# Register map (offsets from X25519_BASE = 0x0840):
#   Write:
#     0x00..0x1F  OPERAND_A  — 256-bit operand A / scalar (little-endian)
#     0x20..0x3F  OPERAND_B  — 256-bit operand B / point  (little-endian)
#     0x40        CMD        — bits [4:1] = mode, bit [0] = go
#                              mode 0 = X25519 (legacy scalar multiply)
#                              mode 1 = FADD  (a+b) mod p
#                              mode 2 = FSUB  (a−b) mod p
#                              mode 3 = FMUL  (a·b) mod p
#                              mode 4 = FSQR  (a²)  mod p
#                              mode 5 = FINV  a^(p−2) mod p
#                              mode 6 = FPOW  a^b mod p
#                              mode 7 = MUL_RAW  256×256→512-bit product
#   Read:
#     0x00        STATUS     — bit 0 = busy, bit 1 = done
#     0x08..0x27  RESULT_LO  — 256-bit result (or low half for MUL_RAW)
#     0x28..0x47  RESULT_HI  — 256-bit high half (MUL_RAW only, else 0)

# Field ALU mode constants
FIELD_MODE_X25519  = 0
FIELD_MODE_FADD    = 1
FIELD_MODE_FSUB    = 2
FIELD_MODE_FMUL    = 3
FIELD_MODE_FSQR    = 4
FIELD_MODE_FINV    = 5
FIELD_MODE_FPOW    = 6
FIELD_MODE_MUL_RAW = 7

class FieldALUDevice(Device):
    """GF(2²⁵⁵−19) field ALU + raw 256×256 multiplier.

    Backward-compatible: mode 0 = X25519 scalar multiply (RFC 7748).
    """

    _P = (1 << 255) - 19
    _A24 = 121665            # (486662 - 2) / 4, per RFC 7748 §5

    def __init__(self):
        super().__init__("FieldALU", X25519_BASE, 0x48)  # 72-byte window
        self._operand_a = bytearray(32)   # also: scalar for mode 0
        self._operand_b = bytearray(32)   # also: point  for mode 0
        self._result_lo = bytearray(32)
        self._result_hi = bytearray(32)   # only populated by MUL_RAW
        self._busy = False
        self._done = False

    # --- helpers ---

    def _get_a(self) -> int:
        return int.from_bytes(bytes(self._operand_a), 'little')

    def _get_b(self) -> int:
        return int.from_bytes(bytes(self._operand_b), 'little')

    def _set_result(self, lo: int, hi: int = 0):
        self._result_lo = bytearray(lo.to_bytes(32, 'little'))
        self._result_hi = bytearray(hi.to_bytes(32, 'little'))
        self._busy = False
        self._done = True

    # --- RFC 7748 scalar multiplication (mode 0) ---

    @staticmethod
    def _clamp(k_bytes: bytearray) -> int:
        k = list(k_bytes)
        k[0] &= 248
        k[31] &= 127
        k[31] |= 64
        return int.from_bytes(bytes(k), 'little')

    @staticmethod
    def _decode_u(u_bytes: bytearray) -> int:
        return int.from_bytes(bytes(u_bytes), 'little') & ((1 << 255) - 1)

    @classmethod
    def _x25519(cls, k_int: int, u_int: int) -> int:
        p = cls._P
        a24 = cls._A24
        x_1 = u_int
        x_2, z_2 = 1, 0
        x_3, z_3 = u_int, 1
        swap = 0
        for t in range(254, -1, -1):
            k_t = (k_int >> t) & 1
            swap ^= k_t
            if swap:
                x_2, x_3 = x_3, x_2
                z_2, z_3 = z_3, z_2
            swap = k_t
            A  = (x_2 + z_2) % p
            AA = (A * A) % p
            B  = (x_2 - z_2) % p
            BB = (B * B) % p
            E  = (AA - BB) % p
            C  = (x_3 + z_3) % p
            D  = (x_3 - z_3) % p
            DA = (D * A) % p
            CB = (C * B) % p
            x_3 = pow(DA + CB, 2, p)
            z_3 = (x_1 * pow(DA - CB, 2, p)) % p
            x_2 = (AA * BB) % p
            z_2 = (E * (AA + a24 * E)) % p
        if swap:
            x_2, x_3 = x_3, x_2
            z_2, z_3 = z_3, z_2
        return (x_2 * pow(z_2, p - 2, p)) % p

    # --- dispatch ---

    def _execute(self, mode: int):
        p = self._P
        a = self._get_a()
        b = self._get_b()

        if mode == FIELD_MODE_X25519:
            k = self._clamp(self._operand_a)
            u = self._decode_u(self._operand_b)
            r = self._x25519(k, u)
            self._set_result(r)
        elif mode == FIELD_MODE_FADD:
            self._set_result((a + b) % p)
        elif mode == FIELD_MODE_FSUB:
            self._set_result((a - b) % p)
        elif mode == FIELD_MODE_FMUL:
            self._set_result((a * b) % p)
        elif mode == FIELD_MODE_FSQR:
            self._set_result((a * a) % p)
        elif mode == FIELD_MODE_FINV:
            self._set_result(pow(a, p - 2, p))
        elif mode == FIELD_MODE_FPOW:
            self._set_result(pow(a, b, p))
        elif mode == FIELD_MODE_MUL_RAW:
            wide = a * b
            lo = wide & ((1 << 256) - 1)
            hi = wide >> 256
            self._set_result(lo, hi)
        else:
            # Unknown mode — just mark done with zero result
            self._set_result(0)

    # --- MMIO interface ---

    def read8(self, offset: int) -> int:
        if offset == 0x00:                          # STATUS
            return (int(self._done) << 1) | int(self._busy)
        if 0x08 <= offset < 0x28:                   # RESULT_LO (32 bytes)
            return self._result_lo[offset - 0x08]
        if 0x28 <= offset < 0x48:                   # RESULT_HI (32 bytes)
            return self._result_hi[offset - 0x28]
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if 0x00 <= offset < 0x20:                   # OPERAND_A
            self._operand_a[offset] = value
        elif 0x20 <= offset < 0x40:                  # OPERAND_B
            self._operand_b[offset - 0x20] = value
        elif offset == 0x40:                         # CMD
            if (value & 1) and not self._busy:
                mode = (value >> 1) & 0xF
                self._busy = True
                self._done = False
                self._execute(mode)


# ---------------------------------------------------------------------------
#  NTT Engine — 256-point Number Theoretic Transform accelerator
# ---------------------------------------------------------------------------
# Register map (offsets from NTT_BASE = 0x8C0):
#   0x00       STATUS (R)  — 0=idle, 1=busy, 2=done
#   0x08-0x0F  Q      (RW) — modulus, 64-bit LE (default 3329)
#   0x10-0x11  IDX    (RW) — coefficient index, 16-bit LE (0-255)
#   0x18-0x1B  LOAD_A (W)  — write A[IDX], LE 32-bit, IDX++ on byte-3
#   0x1C-0x1F  LOAD_B (W)  — write B[IDX], LE 32-bit, IDX++ on byte-3
#   0x20-0x23  RESULT (R)  — read result[IDX], LE 32-bit, IDX++ on byte-3
#   0x28       CMD    (W)  — bits[2:1]=op, bit[0]=go
#
# CMD operations:
#   0x01  NTT_FWD   — forward NTT(A)        → result
#   0x03  NTT_INV   — inverse NTT(A)        → result
#   0x05  NTT_PMUL  — pointwise A·B mod q   → result
#   0x07  NTT_PADD  — pointwise (A+B) mod q → result

NTT_OP_FWD  = 0
NTT_OP_INV  = 1
NTT_OP_PMUL = 2
NTT_OP_PADD = 3


class NTTDevice(Device):
    """256-point NTT accelerator for ML-KEM / ML-DSA lattice crypto."""

    NTT_N = 256

    def __init__(self):
        super().__init__("NTT", NTT_BASE, 0x30)
        self._q = 3329            # default: ML-KEM modulus
        self._idx = 0
        self._poly_a = [0] * self.NTT_N
        self._poly_b = [0] * self.NTT_N
        self._result = [0] * self.NTT_N
        self._busy = False
        self._done = False
        self._load_a_buf = bytearray(4)
        self._load_b_buf = bytearray(4)
        self._omega = None
        self._omega_inv = None
        self._n_inv = None
        self._update_roots()

    # --- root-of-unity computation ---

    def _update_roots(self):
        """Find primitive N-th root of unity for current q."""
        q = self._q
        n = self.NTT_N
        if q < 2 or (q - 1) % n != 0:
            self._omega = None
            return
        for g in range(2, min(q, 10000)):
            if pow(g, (q - 1) // 2, q) != 1:
                omega = pow(g, (q - 1) // n, q)
                if pow(omega, n // 2, q) != 1:
                    self._omega = omega
                    self._omega_inv = pow(omega, q - 2, q)
                    self._n_inv = pow(n, q - 2, q)
                    return
        self._omega = None

    # --- NTT algorithms ---

    def _bit_reverse(self, a):
        n = len(a)
        j = 0
        for i in range(1, n):
            bit = n >> 1
            while j & bit:
                j ^= bit
                bit >>= 1
            j ^= bit
            if i < j:
                a[i], a[j] = a[j], a[i]

    def _ntt_forward(self, coeffs):
        q = self._q
        n = self.NTT_N
        a = [x % q for x in coeffs]
        self._bit_reverse(a)
        length = 2
        while length <= n:
            wn = pow(self._omega, n // length, q)
            for i in range(0, n, length):
                w = 1
                half = length // 2
                for k in range(half):
                    u = a[i + k]
                    v = (a[i + k + half] * w) % q
                    a[i + k] = (u + v) % q
                    a[i + k + half] = (u - v) % q
                    w = (w * wn) % q
            length <<= 1
        return a

    def _ntt_inverse(self, coeffs):
        q = self._q
        n = self.NTT_N
        a = [x % q for x in coeffs]
        self._bit_reverse(a)
        length = 2
        while length <= n:
            wn = pow(self._omega_inv, n // length, q)
            for i in range(0, n, length):
                w = 1
                half = length // 2
                for k in range(half):
                    u = a[i + k]
                    v = (a[i + k + half] * w) % q
                    a[i + k] = (u + v) % q
                    a[i + k + half] = (u - v) % q
                    w = (w * wn) % q
            length <<= 1
        return [(x * self._n_inv) % q for x in a]

    # --- dispatch ---

    def _execute(self, op):
        if self._omega is None:
            self._done = True
            self._busy = False
            return
        q = self._q
        if op == NTT_OP_FWD:
            self._result = self._ntt_forward(self._poly_a)
        elif op == NTT_OP_INV:
            self._result = self._ntt_inverse(self._poly_a)
        elif op == NTT_OP_PMUL:
            self._result = [(a * b) % q
                            for a, b in zip(self._poly_a, self._poly_b)]
        elif op == NTT_OP_PADD:
            self._result = [(a + b) % q
                            for a, b in zip(self._poly_a, self._poly_b)]
        self._busy = False
        self._done = True

    # --- MMIO interface ---

    def read8(self, offset: int) -> int:
        if offset == 0x00:                          # STATUS
            return (int(self._done) << 1) | int(self._busy)
        if 0x08 <= offset < 0x10:                   # Q (8 bytes)
            return (self._q >> ((offset - 0x08) * 8)) & 0xFF
        if 0x10 <= offset < 0x12:                   # IDX (2 bytes)
            return (self._idx >> ((offset - 0x10) * 8)) & 0xFF
        if 0x20 <= offset < 0x24:                   # RESULT[IDX]
            byte_idx = offset - 0x20
            idx = self._idx % self.NTT_N
            val = self._result[idx]
            r = (val >> (byte_idx * 8)) & 0xFF
            if byte_idx == 3:
                self._idx = (self._idx + 1) % self.NTT_N
            return r
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if 0x08 <= offset < 0x10:                   # Q (8 bytes)
            shift = (offset - 0x08) * 8
            self._q = (self._q & ~(0xFF << shift)) | (value << shift)
            if offset == 0x0F:
                self._update_roots()
        elif 0x10 <= offset < 0x12:                  # IDX (2 bytes)
            shift = (offset - 0x10) * 8
            self._idx = (self._idx & ~(0xFF << shift)) | (value << shift)
        elif 0x18 <= offset < 0x1C:                  # LOAD_A
            byte_idx = offset - 0x18
            self._load_a_buf[byte_idx] = value
            if byte_idx == 3:
                idx = self._idx % self.NTT_N
                self._poly_a[idx] = int.from_bytes(
                    self._load_a_buf, 'little') % self._q
                self._idx = (self._idx + 1) % self.NTT_N
        elif 0x1C <= offset < 0x20:                  # LOAD_B
            byte_idx = offset - 0x1C
            self._load_b_buf[byte_idx] = value
            if byte_idx == 3:
                idx = self._idx % self.NTT_N
                self._poly_b[idx] = int.from_bytes(
                    self._load_b_buf, 'little') % self._q
                self._idx = (self._idx + 1) % self.NTT_N
        elif offset == 0x28:                         # CMD
            if (value & 1) and not self._busy:
                op = (value >> 1) & 0x3
                self._busy = True
                self._done = False
                self._execute(op)


# ---------------------------------------------------------------------------
#  Framebuffer — Memory-Mapped Display Controller
# ---------------------------------------------------------------------------
# A dumb framebuffer device that reads pixel data from HBW RAM.
# The device owns no pixel memory — it simply records configuration
# (base address, dimensions, pixel format, palette) and exposes a
# vsync counter.  The emulator display layer (if enabled) reads the
# HBW RAM region pointed to by FB_BASE each frame.
#
# Register map (offsets from FB_BASE = 0x0A00):
#   0x00..0x07  FB_BASE     (RW)  64-bit pixel data start address
#   0x08..0x0B  FB_WIDTH    (RW)  32-bit active width in pixels
#   0x10..0x13  FB_HEIGHT   (RW)  32-bit active height in pixels
#   0x18..0x1B  FB_STRIDE   (RW)  32-bit bytes per scanline
#   0x20        FB_MODE     (RW)  pixel format (0=8bpp indexed,
#                                  1=RGB565, 2=FP16 gray, 3=RGBA8888)
#   0x28        FB_ENABLE   (RW)  bit 0: scanout on/off
#                                  bit 1: vsync IRQ enable
#   0x30..0x33  FB_VSYNC    (RW)  32-bit frame counter; write 1 to ack
#   0x38        FB_PAL_IDX  (W)   palette index (0–255)
#   0x40..0x43  FB_PAL_DATA (W)   24-bit RGB (0x00RRGGBB), LE
#   0x48        FB_STATUS   (R)   bit 0: enabled, bit 1: in-vblank

# Pixel mode constants
FB_MODE_INDEXED  = 0   # 8-bit indexed (palette), 64 pixels/tile
FB_MODE_RGB565   = 1   # 16-bit RGB565, 32 pixels/tile
FB_MODE_FP16     = 2   # 16-bit FP16 grayscale, 32 pixels/tile
FB_MODE_RGBA8888 = 3   # 32-bit RGBA, 16 pixels/tile

# Bytes per pixel for each mode
FB_BPP = {0: 1, 1: 2, 2: 2, 3: 4}


class FramebufferDevice(Device):
    """Memory-mapped framebuffer controller.

    Does not own pixel memory — reads from HBW RAM at the address
    specified by fb_base.  The emulator display loop reads that RAM
    region each vsync frame.
    """

    def __init__(self):
        super().__init__("Framebuffer", FB_BASE, 0x50)  # 80-byte window
        self.fb_base: int = 0           # 64-bit address in HBW
        self.width: int = 320           # default 320×240
        self.height: int = 240
        self.stride: int = 320          # bytes per row (mode 0: 1 bpp)
        self.mode: int = 0              # FB_MODE_INDEXED
        self.enable: int = 0            # bit 0: scanout, bit 1: vsync IRQ
        self.vsync_count: int = 0       # frame counter
        self.pal_idx: int = 0           # current palette write index
        self.palette: list[int] = [0] * 256  # 24-bit 0x00RRGGBB entries
        self.vblank: bool = False       # toggled by tick()

        # Byte assembly buffers for multi-byte LE writes
        self._base_buf = [0] * 8
        self._width_buf = [0] * 4
        self._height_buf = [0] * 4
        self._stride_buf = [0] * 4
        self._vsync_buf = [0] * 4
        self._pal_data_buf = [0] * 4

        # Cycles-per-frame tracking for vsync tick
        self._frame_cycles = 0
        # ~33333 cycles per frame at 1 MHz → ~30 FPS
        # ~16667 for 60 FPS.  Configurable, default 30 FPS.
        self.cycles_per_frame = 33333

        # Default palette: simple grayscale ramp
        for i in range(256):
            self.palette[i] = (i << 16) | (i << 8) | i

    def read8(self, offset: int) -> int:
        # FB_BASE — 8 bytes LE
        if 0x00 <= offset < 0x08:
            return (self.fb_base >> (8 * offset)) & 0xFF
        # FB_WIDTH — 4 bytes LE
        if 0x08 <= offset < 0x0C:
            return (self.width >> (8 * (offset - 0x08))) & 0xFF
        # FB_HEIGHT — 4 bytes LE
        if 0x10 <= offset < 0x14:
            return (self.height >> (8 * (offset - 0x10))) & 0xFF
        # FB_STRIDE — 4 bytes LE
        if 0x18 <= offset < 0x1C:
            return (self.stride >> (8 * (offset - 0x18))) & 0xFF
        # FB_MODE — 1 byte
        if offset == 0x20:
            return self.mode & 0xFF
        # FB_ENABLE — 1 byte
        if offset == 0x28:
            return self.enable & 0xFF
        # FB_VSYNC — 4 bytes LE
        if 0x30 <= offset < 0x34:
            return (self.vsync_count >> (8 * (offset - 0x30))) & 0xFF
        # FB_STATUS — 1 byte
        if offset == 0x48:
            status = 0
            if self.enable & 1:
                status |= 1       # bit 0: enabled
            if self.vblank:
                status |= 2       # bit 1: in vblank
            return status
        return 0

    def write8(self, offset: int, value: int):
        value = value & 0xFF
        # FB_BASE — 8-byte LE accumulator
        if 0x00 <= offset < 0x08:
            self._base_buf[offset] = value
            if offset == 0x07:  # commit on last byte
                self.fb_base = int.from_bytes(self._base_buf, 'little')
            return
        # FB_WIDTH — 4-byte LE
        if 0x08 <= offset < 0x0C:
            idx = offset - 0x08
            self._width_buf[idx] = value
            if idx == 3:
                self.width = int.from_bytes(self._width_buf, 'little')
            return
        # FB_HEIGHT — 4-byte LE
        if 0x10 <= offset < 0x14:
            idx = offset - 0x10
            self._height_buf[idx] = value
            if idx == 3:
                self.height = int.from_bytes(self._height_buf, 'little')
            return
        # FB_STRIDE — 4-byte LE
        if 0x18 <= offset < 0x1C:
            idx = offset - 0x18
            self._stride_buf[idx] = value
            if idx == 3:
                self.stride = int.from_bytes(self._stride_buf, 'little')
            return
        # FB_MODE — 1 byte
        if offset == 0x20:
            self.mode = value & 0x03  # only modes 0–3
            return
        # FB_ENABLE — 1 byte
        if offset == 0x28:
            self.enable = value & 0x03  # bits 0–1
            return
        # FB_VSYNC — write 1 to ack / clear IRQ
        if 0x30 <= offset < 0x34:
            idx = offset - 0x30
            self._vsync_buf[idx] = value
            if idx == 3:
                ack = int.from_bytes(self._vsync_buf, 'little')
                if ack & 1:
                    # Acknowledge: clear the vblank flag
                    self.vblank = False
            return
        # FB_PAL_IDX — 1 byte
        if offset == 0x38:
            self.pal_idx = value
            return
        # FB_PAL_DATA — 4-byte LE (0x00RRGGBB)
        if 0x40 <= offset < 0x44:
            idx = offset - 0x40
            self._pal_data_buf[idx] = value
            if idx == 3:
                rgb = int.from_bytes(self._pal_data_buf, 'little') & 0x00FFFFFF
                self.palette[self.pal_idx] = rgb
                self.pal_idx = (self.pal_idx + 1) & 0xFF  # auto-increment
            return

    def tick(self, cycles: int):
        """Advance frame counter based on accumulated cycles."""
        if not (self.enable & 1):
            return
        self._frame_cycles += cycles
        if self._frame_cycles >= self.cycles_per_frame:
            self._frame_cycles -= self.cycles_per_frame
            self.vsync_count = (self.vsync_count + 1) & 0xFFFFFFFF
            self.vblank = True

    def bpp(self) -> int:
        """Bytes per pixel for the current mode."""
        return FB_BPP.get(self.mode, 1)

    @property
    def irq_pending(self) -> bool:
        """True if vsync IRQ is enabled and vblank has occurred."""
        return bool((self.enable & 2) and self.vblank)

    @property
    def frame_bytes(self) -> int:
        """Total bytes in the framebuffer (stride × height)."""
        return self.stride * self.height


# ---------------------------------------------------------------------------
#  Device Bus — routes MMIO accesses to the correct device
# ---------------------------------------------------------------------------

class DeviceBus:
    """Routes memory-mapped I/O accesses to registered devices."""

    def __init__(self):
        self.devices: list[Device] = []
        self.requester_id: int = 0  # set by system.py before per-core dispatch

    def register(self, device: Device):
        self.devices.append(device)

    def find_device(self, mmio_offset: int) -> tuple[Optional[Device], int]:
        """Given an offset within the MMIO aperture, find the device and
        the offset within that device. Returns (device, local_offset)."""
        for dev in self.devices:
            if dev.base <= mmio_offset < dev.base + dev.size:
                return dev, mmio_offset - dev.base
        return None, 0

    def _set_requester(self, dev: Device):
        """Propagate requester_id to devices that need it."""
        if hasattr(dev, '_requester_id'):
            dev._requester_id = self.requester_id

    def read8(self, mmio_offset: int) -> int:
        dev, local = self.find_device(mmio_offset)
        if dev:
            self._set_requester(dev)
            return dev.read8(local)
        return 0xFF  # open bus

    def write8(self, mmio_offset: int, value: int):
        dev, local = self.find_device(mmio_offset)
        if dev:
            self._set_requester(dev)
            dev.write8(local, value)

    def tick(self, cycles: int):
        for dev in self.devices:
            dev.tick(cycles)
