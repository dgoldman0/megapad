"""
Megapad-64 System Emulator
===========================
Wires together:
  - N Megapad64 CPU cores (megapad64.py) sharing a single memory
  - The device bus (devices.py) for MMIO peripherals
  - A unified memory map that dispatches to RAM vs. MMIO

Supports 1–4 cores with round-robin stepping, per-core IRQ delivery,
inter-core mailbox (IPI), and hardware spinlocks.
"""

from __future__ import annotations
from typing import Optional

from megapad64 import Megapad64, HaltError, TrapError, u64, IVEC_TIMER, IVEC_IPI
from devices import (
    MMIO_BASE, DeviceBus, UART, Timer, Storage, SystemInfo, NetworkDevice,
    MailboxDevice, SpinlockDevice, CRCDevice, AESDevice,
    SECTOR_SIZE, UART_BASE, TIMER_BASE, STORAGE_BASE, SYSINFO_BASE, NIC_BASE,
    MBOX_BASE, SPINLOCK_BASE, CRC_BASE, AES_BASE, NIC_MTU,
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


# Per-core stack layout (matching FPGA mp64_defs.vh)
# Within 1 MiB shared RAM:
#   Core 0: stack at top of 0xF0000–0xFFFFF  → SP = 0x100000
#   Core 1: stack at top of 0xE0000–0xEFFFF  → SP = 0xF0000
#   Core 2: stack at top of 0xD0000–0xDFFFF  → SP = 0xE0000
#   Core 3: stack at top of 0xC0000–0xCFFFF  → SP = 0xD0000
CORE_STACK_TOPS = [0x100000, 0xF0000, 0xE0000, 0xD0000]


class MegapadSystem:
    """
    Complete Megapad-64 system: N CPU cores + shared RAM + peripherals.

    All cores share a single memory array. MMIO-range addresses are
    routed to the device bus. Multicore features include round-robin
    stepping, per-core IRQ delivery, mailbox IPI, and spinlocks.
    """

    def __init__(self, ram_size: int = 1 << 20,
                 storage_image: Optional[str] = None,
                 nic_port: Optional[int] = None,
                 nic_peer_port: Optional[int] = None,
                 num_cores: int = 1):
        self.ram_size = ram_size
        self.num_cores = num_cores

        # Shared memory — all cores reference the same bytearray
        self._shared_mem = bytearray(ram_size)

        # Create CPU cores
        self.cores: list[Megapad64] = []
        for i in range(num_cores):
            cpu = Megapad64(mem_size=ram_size, core_id=i, num_cores=num_cores)
            cpu.mem = self._shared_mem  # share memory
            self.cores.append(cpu)

        # Convenience alias: self.cpu always refers to core 0
        self.cpu = self.cores[0]

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
        self.mailbox = MailboxDevice(num_cores=num_cores)
        self.spinlock = SpinlockDevice()
        self.crc = CRCDevice()
        self.aes = AESDevice()

        self.bus.register(self.uart)
        self.bus.register(self.timer)
        self.bus.register(self.storage)
        self.bus.register(self.sysinfo)
        self.bus.register(self.nic)
        self.bus.register(self.mailbox)
        self.bus.register(self.spinlock)
        self.bus.register(self.crc)
        self.bus.register(self.aes)

        # Wire storage DMA to shared memory
        self.storage._mem_read = self._raw_mem_read
        self.storage._mem_write = self._raw_mem_write

        # Wire NIC DMA to shared memory
        self.nic._mem_read = self._raw_mem_read
        self.nic._mem_write = self._raw_mem_write

        # Wire mailbox IPI delivery
        self.mailbox.on_ipi = self._deliver_ipi
        self.mailbox.on_ack = self._handle_ipi_ack

        # Patch CPU memory access functions to intercept MMIO (per core)
        for cpu in self.cores:
            self._patch_cpu_mem(cpu)

        # Wire CSR IPI stubs into real mailbox operations (per core)
        for cpu in self.cores:
            self._wire_ipi_csrs(cpu)

        # Default UART TX handler: buffer (CLI will override)
        self._tx_log: list[int] = []
        self.uart.on_tx = lambda b: self._tx_log.append(b)

        # Boot state
        self._booted = False

    # -----------------------------------------------------------------
    #  IPI wiring
    # -----------------------------------------------------------------

    def _deliver_ipi(self, target_core: int):
        """Called by MailboxDevice when an IPI is sent to a core."""
        if 0 <= target_core < self.num_cores:
            self.cores[target_core].irq_ipi = True

    def _handle_ipi_ack(self, acking_core: int):
        """Called by MailboxDevice after an MMIO ACK clears a pending bit."""
        if 0 <= acking_core < self.num_cores:
            if self.mailbox.pending[acking_core] == 0:
                self.cores[acking_core].irq_ipi = False

    def _wire_ipi_csrs(self, cpu: Megapad64):
        """Wire the CPU's CSR IPI stubs to the real mailbox device."""
        core_id = cpu.core_id
        mailbox = self.mailbox

        @property
        def ipi_pending_mask(self):
            return mailbox.pending[core_id]

        # Monkey-patch the property onto this specific instance
        # We use a simpler approach: override the methods directly
        def ipi_send(target: int):
            target = target & 0xFF
            if target < self.num_cores and target != core_id:
                mailbox.pending[target] |= (1 << core_id)
                self._deliver_ipi(target)

        def ipi_ack(from_core: int):
            from_core = from_core & 0xFF
            if from_core < self.num_cores:
                mailbox.pending[core_id] &= ~(1 << from_core)
                # Clear irq_ipi if no more pending
                if mailbox.pending[core_id] == 0:
                    cpu.irq_ipi = False

        def get_ipi_pending():
            return mailbox.pending[core_id]

        cpu._ipi_send = ipi_send
        cpu._ipi_ack = ipi_ack
        # Override the property-style access: patch csr_read's MBOX handler
        original_csr_read = cpu.csr_read
        def patched_csr_read(addr):
            if addr == 0x22:  # CSR_MBOX
                return get_ipi_pending()
            return original_csr_read(addr)
        cpu.csr_read = patched_csr_read

    # -----------------------------------------------------------------
    #  Memory access patching
    # -----------------------------------------------------------------

    def _patch_cpu_mem(self, cpu: Megapad64):
        """
        Replace the CPU's mem_read8 / mem_write8 so that accesses in
        the MMIO range get routed to the device bus, with the correct
        requester_id set on the bus.
        """
        original_read8 = cpu.mem_read8
        original_write8 = cpu.mem_write8
        core_id = cpu.core_id
        bus = self.bus

        def patched_read8(addr: int) -> int:
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                bus.requester_id = core_id
                offset = addr - MMIO_START
                return bus.read8(offset)
            return original_read8(addr)

        def patched_write8(addr: int, val: int):
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                bus.requester_id = core_id
                offset = addr - MMIO_START
                bus.write8(offset, val)
                return
            original_write8(addr, val)

        cpu.mem_read8 = patched_read8
        cpu.mem_write8 = patched_write8

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

        cpu.mem_read16 = patched_read16
        cpu.mem_write16 = patched_write16
        cpu.mem_read32 = patched_read32
        cpu.mem_write32 = patched_write32
        cpu.mem_read64 = patched_read64
        cpu.mem_write64 = patched_write64

    # -----------------------------------------------------------------
    #  Raw memory helpers (bypass MMIO, for DMA)
    # -----------------------------------------------------------------

    def _raw_mem_read(self, addr: int) -> int:
        addr = u64(addr) % self.ram_size
        return self._shared_mem[addr]

    def _raw_mem_write(self, addr: int, val: int):
        addr = u64(addr) % self.ram_size
        self._shared_mem[addr] = val & 0xFF

    # -----------------------------------------------------------------
    #  Loading
    # -----------------------------------------------------------------

    def load_binary(self, addr: int, data: bytes | bytearray):
        """Load raw bytes into shared RAM at the given address."""
        for i, b in enumerate(data):
            self._shared_mem[(addr + i) % self.ram_size] = b

    def load_binary_file(self, path: str, addr: int = 0):
        """Load a binary file into RAM."""
        with open(path, "rb") as f:
            data = f.read()
        self.load_binary(addr, data)

    # -----------------------------------------------------------------
    #  Boot
    # -----------------------------------------------------------------

    def boot(self, entry: int = BOOT_VECTOR):
        """Cold boot the system.

        All cores start at the same entry point (matching FPGA behaviour).
        Core 0 gets SP at top of RAM; secondary cores get per-core stacks.
        In the BIOS, core 0 will continue booting while cores 1–3 read
        their COREID CSR and HALT, waiting for an IPI to wake them.
        """
        for i, cpu in enumerate(self.cores):
            cpu._reset_state()
            cpu.pc = entry

            # Per-core stack tops (match FPGA layout)
            if i < len(CORE_STACK_TOPS) and self.ram_size >= CORE_STACK_TOPS[0]:
                sp = CORE_STACK_TOPS[i]
            else:
                # Small RAM or >4 cores: divide equally
                sp = self.ram_size - i * (self.ram_size // self.num_cores)
            cpu.regs[cpu.spsel] = sp
            cpu.regs[2] = sp  # R2 (X) also usable as stack

            cpu.halted = False
            cpu.idle = False

        self._booted = True

    # -----------------------------------------------------------------
    #  Execution
    # -----------------------------------------------------------------

    def step(self) -> int:
        """Execute one instruction on each active core (round-robin).

        Returns total cycles consumed across all cores.
        """
        total_cycles = 0

        for cpu in self.cores:
            # Wake CPU from idle on IPI
            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                cpu.idle = False

            # Wake core 0 from idle on UART RX or timer IRQ
            if cpu.idle and cpu.core_id == 0:
                if self.uart.has_rx_data:
                    cpu.idle = False
                elif self.timer.irq_pending and cpu.flag_i:
                    cpu.idle = False

            if cpu.halted or cpu.idle:
                continue

            try:
                cycles = cpu.step()
                total_cycles += cycles
            except TrapError as e:
                if cpu.ivt_base != 0:
                    cpu._trap(e.ivec_id)
                else:
                    raise

        # Tick devices once per round
        self.bus.tick(1)

        # Deliver timer IRQ to all cores with interrupts enabled
        if self.timer.irq_pending:
            for cpu in self.cores:
                if cpu.flag_i and not cpu.halted:
                    cpu._trap(IVEC_TIMER)
                    break  # only deliver to first eligible core (like FPGA)

        # Deliver IPI interrupts to cores with pending IPI
        for cpu in self.cores:
            if cpu.irq_ipi and cpu.flag_i and not cpu.halted and not cpu.idle:
                cpu._trap(IVEC_IPI)
                # Don't clear irq_ipi here — handler reads STATUS and ACKs

        return max(total_cycles, 1)

    def run(self, max_steps: int = 1_000_000) -> int:
        """Run until all cores HALT, or max_steps."""
        total = 0
        for _ in range(max_steps):
            if self.all_halted:
                break
            if self.all_idle_or_halted and not self.uart.has_rx_data:
                # All cores idle/halted with no pending input — tick bus
                self.bus.tick(1)
                total += 1
                # Check if timer IRQ should wake someone
                if self.timer.irq_pending:
                    for cpu in self.cores:
                        if cpu.idle and cpu.flag_i:
                            cpu.idle = False
                            break
                # Check if IPI should wake someone
                for cpu in self.cores:
                    if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                        cpu.idle = False
                continue
            total += self.step()
        return total

    def run_until_halt(self, max_steps: int = 10_000_000) -> int:
        """Run until all cores HALT."""
        total = 0
        for _ in range(max_steps):
            if self.all_halted:
                break
            try:
                total += self.step()
            except HaltError:
                break
        return total

    # -----------------------------------------------------------------
    #  State queries
    # -----------------------------------------------------------------

    @property
    def all_halted(self) -> bool:
        """True if every core is halted."""
        return all(cpu.halted for cpu in self.cores)

    @property
    def all_idle_or_halted(self) -> bool:
        """True if every core is either idle or halted."""
        return all(cpu.halted or cpu.idle for cpu in self.cores)

    @property
    def halted(self) -> bool:
        """For backward compat: True if core 0 is halted (single-core)
        or all cores are halted (multicore)."""
        if self.num_cores == 1:
            return self.cpu.halted
        return self.all_halted

    @property
    def idle(self) -> bool:
        """For backward compat: True if core 0 is idle."""
        return self.cpu.idle

    # -----------------------------------------------------------------
    #  Convenience
    # -----------------------------------------------------------------

    def get_tx_output(self) -> str:
        """Get any UART output that has been produced."""
        return self.uart.drain_tx()

    def dump_state(self) -> str:
        """Full CPU + device state dump."""
        lines = []
        for i, cpu in enumerate(self.cores):
            lines.append(f"=== Core {i} Registers ===")
            lines.append(cpu.dump_regs())
            lines.append(f"  Cycles: {cpu.cycle_count}")
            lines.append(f"  Halted: {cpu.halted}  Idle: {cpu.idle}  "
                         f"IPI: {cpu.irq_ipi}")
            lines.append("")

        lines.append("=== Devices ===")
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
        if self.num_cores > 1:
            lines.append(f"  Mailbox: cores={self.num_cores} "
                         f"pending={[self.mailbox.pending[i] for i in range(self.num_cores)]}")
            locked = [i for i in range(self.spinlock.num_locks) if self.spinlock.locked[i]]
            lines.append(f"  Spinlocks: locked={locked} "
                         f"owners={[self.spinlock.owner[i] for i in locked]}")
        return "\n".join(lines)
