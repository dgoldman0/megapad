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
import time
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

class SystemInfo(Device):
    """Read-only board identification and capability reporting."""

    def __init__(self, mem_size_kib: int = 1024, has_storage: bool = False):
        super().__init__("SysInfo", SYSINFO_BASE, 0x10)
        self.mem_size_kib = mem_size_kib
        self.has_storage = has_storage

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
        return 0


# ---------------------------------------------------------------------------
#  Device Bus — routes MMIO accesses to the correct device
# ---------------------------------------------------------------------------

class DeviceBus:
    """Routes memory-mapped I/O accesses to registered devices."""

    def __init__(self):
        self.devices: list[Device] = []

    def register(self, device: Device):
        self.devices.append(device)

    def find_device(self, mmio_offset: int) -> tuple[Optional[Device], int]:
        """Given an offset within the MMIO aperture, find the device and
        the offset within that device. Returns (device, local_offset)."""
        for dev in self.devices:
            if dev.base <= mmio_offset < dev.base + dev.size:
                return dev, mmio_offset - dev.base
        return None, 0

    def read8(self, mmio_offset: int) -> int:
        dev, local = self.find_device(mmio_offset)
        if dev:
            return dev.read8(local)
        return 0xFF  # open bus

    def write8(self, mmio_offset: int, value: int):
        dev, local = self.find_device(mmio_offset)
        if dev:
            dev.write8(local, value)

    def tick(self, cycles: int):
        for dev in self.devices:
            dev.tick(cycles)
