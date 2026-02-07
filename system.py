"""
Megapad-64 System Emulator
===========================
Wires together:
  - The Megapad64 CPU core (megapad64.py)
  - The device bus (devices.py) for MMIO peripherals
  - A unified memory map that dispatches to RAM vs. MMIO vs. CSR

Provides boot(), step(), run(), and configuration entry points.
"""

from __future__ import annotations
from typing import Optional

from megapad64 import Megapad64, HaltError, TrapError, u64
from devices import (
    MMIO_BASE, DeviceBus, UART, Timer, Storage, SystemInfo, NetworkDevice,
    SECTOR_SIZE, UART_BASE, TIMER_BASE, STORAGE_BASE, SYSINFO_BASE, NIC_BASE,
    NIC_MTU,
)

# ---------------------------------------------------------------------------
#  Memory map constants
# ---------------------------------------------------------------------------

# MMIO aperture: 0xFFFF_FF00_0000_0000 .. 0xFFFF_FF7F_FFFF_FFFF
MMIO_START = 0xFFFF_FF00_0000_0000
MMIO_END   = 0xFFFF_FF80_0000_0000  # exclusive

# RAM occupies the low end of the 64-bit address space.
# In the emulator, RAM size is configurable (default 1 MiB).

# Boot vector: on reset, PC (R3) is loaded with this address.
# BIOS is expected to be loaded here.
BOOT_VECTOR = 0x0000_0000_0000_0000


class MegapadSystem:
    """
    Complete Megapad-64 system: CPU + RAM + peripherals.

    Intercepts CPU memory accesses to route MMIO-range addresses
    to the device bus, while normal addresses go to the CPU's
    internal memory array.
    """

    def __init__(self, ram_size: int = 1 << 20,
                 storage_image: Optional[str] = None,
                 nic_port: Optional[int] = None,
                 nic_peer_port: Optional[int] = None):
        self.ram_size = ram_size
        self.cpu = Megapad64(mem_size=ram_size)

        # --- Device bus ---
        self.bus = DeviceBus()

        self.uart = UART()
        self.timer = Timer()
        self.storage = Storage(storage_image)
        self.nic = NetworkDevice(
            passthrough_port=nic_port,
            passthrough_peer_port=nic_peer_port,
        )
        self.sysinfo = SystemInfo(
            mem_size_kib=ram_size // 1024,
            has_storage=storage_image is not None,
            has_nic=True,
        )

        self.bus.register(self.uart)
        self.bus.register(self.timer)
        self.bus.register(self.storage)
        self.bus.register(self.sysinfo)
        self.bus.register(self.nic)

        # Wire storage DMA to CPU memory
        self.storage._mem_read = self._raw_mem_read
        self.storage._mem_write = self._raw_mem_write

        # Wire NIC DMA to CPU memory
        self.nic._mem_read = self._raw_mem_read
        self.nic._mem_write = self._raw_mem_write

        # Patch CPU memory access functions to intercept MMIO
        self._patch_cpu_mem()

        # Default UART TX handler: buffer (CLI will override)
        self._tx_log: list[int] = []
        self.uart.on_tx = lambda b: self._tx_log.append(b)

        # Boot state
        self._booted = False

    # -----------------------------------------------------------------
    #  Memory access patching
    # -----------------------------------------------------------------

    def _patch_cpu_mem(self):
        """
        Replace the CPU's mem_read8 / mem_write8 so that accesses in
        the MMIO range get routed to the device bus.
        """
        original_read8 = self.cpu.mem_read8
        original_write8 = self.cpu.mem_write8

        def patched_read8(addr: int) -> int:
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                offset = addr - MMIO_START
                return self.bus.read8(offset)
            return original_read8(addr)

        def patched_write8(addr: int, val: int):
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                offset = addr - MMIO_START
                self.bus.write8(offset, val)
                return
            original_write8(addr, val)

        self.cpu.mem_read8 = patched_read8
        self.cpu.mem_write8 = patched_write8

        # Also patch wider reads/writes to go through the byte-level
        # functions so MMIO works for 16/32/64-bit accesses too.
        def patched_read16(addr: int) -> int:
            b0 = patched_read8(addr)
            b1 = patched_read8(u64(addr + 1))
            return b0 | (b1 << 8)

        def patched_write16(addr: int, val: int):
            patched_write8(addr, val & 0xFF)
            patched_write8(u64(addr + 1), (val >> 8) & 0xFF)

        def patched_read32(addr: int) -> int:
            v = 0
            for i in range(4):
                v |= patched_read8(u64(addr + i)) << (8 * i)
            return v

        def patched_write32(addr: int, val: int):
            for i in range(4):
                patched_write8(u64(addr + i), (val >> (8 * i)) & 0xFF)

        def patched_read64(addr: int) -> int:
            v = 0
            for i in range(8):
                v |= patched_read8(u64(addr + i)) << (8 * i)
            return v

        def patched_write64(addr: int, val: int):
            for i in range(8):
                patched_write8(u64(addr + i), (val >> (8 * i)) & 0xFF)

        self.cpu.mem_read16 = patched_read16
        self.cpu.mem_write16 = patched_write16
        self.cpu.mem_read32 = patched_read32
        self.cpu.mem_write32 = patched_write32
        self.cpu.mem_read64 = patched_read64
        self.cpu.mem_write64 = patched_write64

    # -----------------------------------------------------------------
    #  Raw memory helpers (bypass MMIO, for DMA)
    # -----------------------------------------------------------------

    def _raw_mem_read(self, addr: int) -> int:
        addr = u64(addr) % self.cpu.mem_size
        return self.cpu.mem[addr]

    def _raw_mem_write(self, addr: int, val: int):
        addr = u64(addr) % self.cpu.mem_size
        self.cpu.mem[addr] = val & 0xFF

    # -----------------------------------------------------------------
    #  Loading
    # -----------------------------------------------------------------

    def load_binary(self, addr: int, data: bytes | bytearray):
        """Load raw bytes into RAM at the given address."""
        self.cpu.load_bytes(addr, data)

    def load_binary_file(self, path: str, addr: int = 0):
        """Load a binary file into RAM."""
        with open(path, "rb") as f:
            data = f.read()
        self.load_binary(addr, data)

    # -----------------------------------------------------------------
    #  Boot
    # -----------------------------------------------------------------

    def boot(self, entry: int = BOOT_VECTOR):
        """Cold boot the system. Sets PC to entry, SP to top of RAM."""
        self.cpu._reset_state()
        self.cpu.pc = entry
        self.cpu.regs[self.cpu.spsel] = self.ram_size  # SP at top of RAM
        self.cpu.regs[2] = self.ram_size  # R2 (X) also usable as stack
        self._booted = True
        self.cpu.halted = False
        self.cpu.idle = False

    # -----------------------------------------------------------------
    #  Execution
    # -----------------------------------------------------------------

    def step(self) -> int:
        """Execute one instruction. Returns cycles consumed."""
        # Wake CPU from idle when UART has received data
        if self.cpu.idle and self.uart.has_rx_data:
            self.cpu.idle = False
        cycles = self.cpu.step()
        self.bus.tick(cycles)
        return cycles

    def run(self, max_steps: int = 1_000_000) -> int:
        """Run until HALT, IDLE, or max_steps."""
        total = 0
        for _ in range(max_steps):
            if self.cpu.halted:
                break
            if self.cpu.idle and not self.uart.has_rx_data:
                # Idle with no pending input — tick bus (timers) and wait
                self.bus.tick(1)
                total += 1
                continue
            try:
                total += self.step()
            except TrapError as e:
                if self.cpu.ivt_base != 0:
                    self.cpu._trap(e.ivec_id)
                else:
                    raise
        return total

    def run_until_halt(self, max_steps: int = 10_000_000) -> int:
        """Run until HALT. Raises HaltError when done."""
        total = 0
        for _ in range(max_steps):
            if self.cpu.halted:
                break
            try:
                total += self.step()
            except HaltError:
                break
            except TrapError as e:
                if self.cpu.ivt_base != 0:
                    self.cpu._trap(e.ivec_id)
                else:
                    raise
        return total

    # -----------------------------------------------------------------
    #  Convenience
    # -----------------------------------------------------------------

    def get_tx_output(self) -> str:
        """Get any UART output that has been produced."""
        return self.uart.drain_tx()

    @property
    def halted(self) -> bool:
        return self.cpu.halted

    @property
    def idle(self) -> bool:
        return self.cpu.idle

    def dump_state(self) -> str:
        """Full CPU + device state dump."""
        lines = ["=== CPU Registers ==="]
        lines.append(self.cpu.dump_regs())
        lines.append(f"\n  Cycles: {self.cpu.cycle_count}")
        lines.append(f"  Halted: {self.cpu.halted}  Idle: {self.cpu.idle}")

        lines.append("\n=== Devices ===")
        lines.append(f"  UART: TX buf={len(self.uart.tx_buffer)} "
                      f"RX buf={len(self.uart.rx_buffer)} "
                      f"ctrl={self.uart.control:#04x}")
        lines.append(f"  Timer: count={self.timer.counter} "
                      f"compare={self.timer.compare} "
                      f"ctrl={self.timer.control:#04x} "
                      f"irq={'Y' if self.timer.irq_pending else 'N'}")
        lines.append(f"  Storage: {'present' if self.storage.status & 0x80 else 'none'} "
                      f"sectors={self.storage.total_sectors} "
                      f"image={self.storage.image_path or 'N/A'}")
        lines.append(f"  NIC: {'link up' if self.nic.link_up else 'link down'} "
                      f"mac={self.nic.mac.hex(':')} "
                      f"tx={self.nic.tx_count} rx={self.nic.rx_count} "
                      f"rxq={len(self.nic.rx_queue)} "
                      f"passthrough={'port ' + str(self.nic._passthrough_port) if self.nic._passthrough_port else 'none'}")
        return "\n".join(lines)
