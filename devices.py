"""
Megapad-64 Peripheral / Device Layer
=====================================
Memory-mapped I/O devices for the system emulator.

Physical memory map (from SPECS.html §2):
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
from typing import Optional
from collections import deque

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
CRC_BASE     = 0x0700


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
        if not (self.control & 1):  # not enabled
            return
        for _ in range(cycles):
            self.counter = (self.counter + 1) & 0xFFFFFFFF
            if self.counter == self.compare:
                self.status |= 1  # match flag
                if self.control & 2:  # interrupt enable
                    self.irq_pending = True
                if self.control & 4:  # auto-reload
                    self.counter = 0


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
#   0x06  MEM_SIZE_LO — total memory in KiB, low byte
#   0x07  MEM_SIZE_HI — high byte
#   0x08  STORAGE_PRESENT — 0 or 1
#   0x09  UART_PRESENT    — 0 or 1
#   0x0A  NIC_PRESENT     — 0 or 1

class SystemInfo(Device):
    """Read-only board identification and capability reporting."""

    def __init__(self, mem_size_kib: int = 1024, has_storage: bool = False,
                 has_nic: bool = False):
        super().__init__("SysInfo", SYSINFO_BASE, 0x10)
        self.mem_size_kib = mem_size_kib
        self.has_storage = has_storage
        self.has_nic = has_nic

    def read8(self, offset: int) -> int:
        board_id = b"MP64"
        if 0x00 <= offset <= 0x03:
            return board_id[offset]
        if offset == 0x04:
            return 1   # major version
        if offset == 0x05:
            return 0   # minor version
        if offset == 0x06:
            return self.mem_size_kib & 0xFF
        if offset == 0x07:
            return (self.mem_size_kib >> 8) & 0xFF
        if offset == 0x08:
            return 1 if self.has_storage else 0
        if offset == 0x09:
            return 1  # UART always present
        if offset == 0x0A:
            return 1 if self.has_nic else 0
        return 0


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
    """Emulated NIC with optional UDP passthrough to a real host port."""

    def __init__(self, mac: bytes = b'\x02\x4D\x50\x36\x34\x00',
                 passthrough_port: Optional[int] = None,
                 passthrough_host: str = '127.0.0.1',
                 passthrough_peer_port: Optional[int] = None):
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

        # UDP passthrough
        self._passthrough_port = passthrough_port
        self._passthrough_host = passthrough_host
        self._passthrough_peer_port = passthrough_peer_port or (
            (passthrough_port + 1) if passthrough_port else None
        )
        self._sock: Optional[socket.socket] = None
        self._rx_thread: Optional[threading.Thread] = None
        self._running = False

        if passthrough_port is not None:
            self._start_passthrough()

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
        """Shut down the passthrough listener."""
        self._running = False
        if self._sock:
            try:
                self._sock.close()
            except OSError:
                pass
            self._sock = None

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
                # UDP passthrough
                if self._sock and self._passthrough_peer_port:
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
