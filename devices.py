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
SHA256_BASE  = 0x0940
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
        self._tx_listeners: list = []          # additional TX listeners (display, etc.)

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
            for fn in self._tx_listeners:
                fn(value)
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
#   0x48  NUM_FULL     — number of full (major) cores

# Board ID + version packed as 64-bit LE value:
#   bytes 0-1: version (minor=1, then 0x00)
#   bytes 2-3: version (major=2, then 0x00)
#   bytes 4-7: "MP64" reversed → '4','6','P','M' = 0x34,0x36,0x50,0x4D
_BOARD_ID_VER = 0x4D50_3634_0002_0001

class SystemInfo(Device):
    """Board identification and capability reporting (matches RTL SysInfo)."""

    def __init__(self, bank0_size: int = 1 << 20,
                 num_cores: int = 1,
                 num_full_cores: int | None = None,
                 hbw_base: int = 0xFFD0_0000,
                 hbw_size: int = 3 * (1 << 20),
                 int_mem_total: int = 4 * (1 << 20),
                 # Legacy compat — ignored if bank0_size is set explicitly
                 mem_size_kib: int | None = None,
                 has_storage: bool = False,
                 has_nic: bool = False,
                 ext_mem_base: int = 0,
                 ext_mem_size: int = 0):
        super().__init__("SysInfo", SYSINFO_BASE, 0x50)
        if mem_size_kib is not None and bank0_size == (1 << 20):
            # Legacy caller: convert KiB → bytes
            bank0_size = mem_size_kib * 1024
        self.bank0_size = bank0_size
        self.num_cores = num_cores
        self.num_full_cores = num_full_cores if num_full_cores is not None else num_cores
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
            0x48: self.num_full_cores,
        }

    def read8(self, offset: int) -> int:
        if offset < 0 or offset >= 0x50:
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
#  AES-GCM Accelerator — REMOVED (implemented in C++: accel/mp64_crypto.h)
# ---------------------------------------------------------------------------
# See CryptoAES in mp64_crypto.h. MMIO range: 0x0700 – 0x0770.


# ---------------------------------------------------------------------------
#  SHA-3 Accelerator — REMOVED (implemented in C++: accel/mp64_crypto.h)
# ---------------------------------------------------------------------------
# See CryptoSHA3 in mp64_crypto.h. MMIO range: 0x0780 – 0x07C0.


# ---------------------------------------------------------------------------
#  SHA-256 Accelerator — REMOVED (implemented in C++: accel/mp64_crypto.h)
# ---------------------------------------------------------------------------
# See CryptoSHA256 in mp64_crypto.h. MMIO range: 0x0940 – 0x0980.



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
#  TRNG — REMOVED (implemented in C++: accel/mp64_nic.h)
# ---------------------------------------------------------------------------
# See TRNGDevice in mp64_nic.h. MMIO range: 0x0800 – 0x0820.


# ---------------------------------------------------------------------------
#  Field ALU — REMOVED (implemented in C++: accel/mp64_crypto.h)
# ---------------------------------------------------------------------------
# See CryptoFieldALU in mp64_crypto.h. MMIO range: 0x0840 – 0x0888.

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
