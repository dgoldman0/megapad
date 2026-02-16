"""
Megapad-64 System Emulator
===========================
Wires together:
  - N Megapad64 CPU cores (megapad64.py) sharing a single memory
  - The device bus (devices.py) for MMIO peripherals
  - A unified memory map that dispatches to RAM vs. MMIO

Supports 1–4 full cores plus up to 3 micro-core clusters (12
micro-cores), round-robin stepping, per-core IRQ delivery, inter-core
mailbox (IPI), hardware spinlocks, and cluster enable/disable gating.
"""

from __future__ import annotations
from typing import Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from nic_backends import NICBackend

try:
    from accel_wrapper import Megapad64, HaltError, TrapError, u64, IVEC_TIMER, IVEC_IPI, IVEC_PRIV_FAULT
except ImportError:
    from megapad64 import Megapad64, HaltError, TrapError, u64, IVEC_TIMER, IVEC_IPI, IVEC_PRIV_FAULT
from megapad64 import (
    Megapad64Micro, CSR_BIST_CMD, CSR_BIST_STATUS, CSR_BIST_FAIL_ADDR,
    CSR_BIST_FAIL_DATA, MICRO_PER_CLUSTER, NUM_CLUSTERS, MICRO_ID_BASE,
    NUM_ALL_CORES, CLUSTER_SPAD_BYTES, CLUSTER_SPAD_ADDR,
    CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
)
from devices import (
    MMIO_BASE, DeviceBus, UART, Timer, Storage, SystemInfo, NetworkDevice,
    MailboxDevice, SpinlockDevice, CRCDevice, AESDevice, SHA3Device, TRNGDevice,
    FieldALUDevice, NTTDevice, KemDevice, FramebufferDevice,
    SECTOR_SIZE, UART_BASE, TIMER_BASE, STORAGE_BASE, SYSINFO_BASE, NIC_BASE,
    MBOX_BASE, SPINLOCK_BASE, CRC_BASE, AES_BASE, SHA3_BASE, TRNG_BASE,
    X25519_BASE, NTT_BASE, KEM_BASE, FB_BASE, NIC_MTU,
)

# ---------------------------------------------------------------------------
#  Memory map constants
# ---------------------------------------------------------------------------

# MMIO aperture: 0xFFFF_FF00_0000_0000 .. 0xFFFF_FF7F_FFFF_FFFF
MMIO_START = 0xFFFF_FF00_0000_0000
MMIO_END   = 0xFFFF_FF80_0000_0000  # exclusive

# RAM occupies the low end of the 64-bit address space.
# In the emulator, RAM size is configurable (default 1 MiB).

# High-Bandwidth (HBW) math RAM — 3 banks of 1 MiB each.
# Physically internal BRAM on FPGA, mapped at a high 32-bit address.
HBW_BASE   = 0xFFD0_0000
HBW_SIZE   = 3 * (1 << 20)          # 3 MiB (banks 1–3)
HBW_END    = HBW_BASE + HBW_SIZE    # exclusive

# External memory (HyperRAM / SDRAM).
# Fills the gap between Bank 0 top and HBW base.
EXT_MEM_BASE = 0x0010_0000          # right after 1 MiB Bank 0

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


# ---------------------------------------------------------------------------
#  MicroCluster — matches RTL mp64_cluster.v
# ---------------------------------------------------------------------------

class MicroCluster:
    """Emulates an mp64_cluster: N micro-cores with shared resources.

    Shared resources:
      - 1 KiB scratchpad RAM (cluster-local, not on main bus)
      - Shared MUL/DIV unit (modelled as immediate, no contention)
      - Hardware barrier register
      - BIST controller for scratchpad/multiplier

    Cluster enable/disable is controlled by the SysInfo CLUSTER_EN
    register.  When disabled, all micro-cores are held in reset.
    """

    def __init__(self, cluster_id: int, id_base: int,
                 n: int = MICRO_PER_CLUSTER,
                 shared_mem: bytearray = None,
                 mem_size: int = 1 << 20,
                 num_all_cores: int = NUM_ALL_CORES):
        self.cluster_id = cluster_id
        self.id_base = id_base
        self.n = n
        self.enabled = False   # matches RTL default: clusters off at reset

        # Scratchpad — 1 KiB, cluster-local
        self.scratchpad = bytearray(CLUSTER_SPAD_BYTES)

        # Barrier register
        self.barrier_arrive = 0   # N-bit mask
        self.barrier_done = False
        self._barrier_all = (1 << n) - 1  # all-arrived mask

        # BIST state
        self.bist_status = 0      # 0=idle, 1=running, 2=pass, 3=fail
        self.bist_fail_addr = 0
        self.bist_fail_data = 0

        # Cluster-level MPU (shared across all micro-cores in cluster)
        self.cl_priv_level = 0   # 0 = supervisor, 1 = user
        self.cl_mpu_base = 0     # inclusive lower bound
        self.cl_mpu_limit = 0    # exclusive upper bound

        # Create micro-cores
        self.cores: list[Megapad64Micro] = []
        for i in range(n):
            core_id = id_base + i
            mc = Megapad64Micro(mem_size=mem_size, core_id=core_id,
                                num_cores=num_all_cores)
            mc._cluster = self
            if shared_mem is not None:
                mc.mem = shared_mem
            self.cores.append(mc)

    # -- Barrier --

    def barrier_arrive_core(self, global_core_id: int):
        """A micro-core signals barrier arrival."""
        local = global_core_id - self.id_base
        if 0 <= local < self.n:
            self.barrier_arrive |= (1 << local)
            if self.barrier_arrive == self._barrier_all:
                self.barrier_done = True
                self.barrier_arrive = 0  # auto-clear

    def barrier_reset(self):
        self.barrier_arrive = 0
        self.barrier_done = False

    # -- BIST --

    def bist_csr_read(self, addr: int) -> int:
        if addr == CSR_BIST_STATUS:
            return self.bist_status
        if addr == CSR_BIST_FAIL_ADDR:
            return self.bist_fail_addr
        if addr == CSR_BIST_FAIL_DATA:
            return self.bist_fail_data
        if addr == CSR_CL_PRIV:
            return self.cl_priv_level
        if addr == CSR_CL_MPU_BASE:
            return self.cl_mpu_base
        if addr == CSR_CL_MPU_LIMIT:
            return self.cl_mpu_limit
        return 0

    def bist_csr_write(self, addr: int, val: int):
        if addr == CSR_BIST_CMD:
            if val == 0:
                return
            self.bist_status = 1  # running
            self.bist_fail_addr = 0
            self.bist_fail_data = 0
            # March C- on scratchpad
            ok = self._bist_spad()
            self.bist_status = 2 if ok else 3
            return
        # Cluster MPU CSRs — only writable from supervisor mode
        if self.cl_priv_level == 0:
            if addr == CSR_CL_PRIV:
                self.cl_priv_level = val & 1
            elif addr == CSR_CL_MPU_BASE:
                self.cl_mpu_base = val & ((1 << 64) - 1)
            elif addr == CSR_CL_MPU_LIMIT:
                self.cl_mpu_limit = val & ((1 << 64) - 1)

    def _bist_spad(self) -> bool:
        """March C- test on scratchpad memory."""
        sz = len(self.scratchpad)
        saved = bytes(self.scratchpad)  # save contents
        ok = True
        # Phase 0: write all 0x00
        for i in range(sz):
            self.scratchpad[i] = 0x00
        # Phase 1: read 0x00, write 0xFF ascending
        for i in range(sz):
            if self.scratchpad[i] != 0x00:
                self.bist_fail_addr = i
                self.bist_fail_data = (0x00 << 8) | self.scratchpad[i]
                ok = False
                break
            self.scratchpad[i] = 0xFF
        if ok:
            # Phase 2: read 0xFF, write 0x00 descending
            for i in range(sz - 1, -1, -1):
                if self.scratchpad[i] != 0xFF:
                    self.bist_fail_addr = i
                    self.bist_fail_data = (0xFF << 8) | self.scratchpad[i]
                    ok = False
                    break
                self.scratchpad[i] = 0x00
        # Restore
        self.scratchpad[:] = saved
        return ok

    # -- Scratchpad access helpers --

    def spad_read8(self, offset: int) -> int:
        offset = offset % CLUSTER_SPAD_BYTES
        return self.scratchpad[offset]

    def spad_write8(self, offset: int, val: int):
        offset = offset % CLUSTER_SPAD_BYTES
        self.scratchpad[offset] = val & 0xFF

    # -- Enable / disable --

    def set_enabled(self, en: bool):
        """Enable or disable the cluster (matching RTL cluster_en gating)."""
        if en and not self.enabled:
            # Coming out of reset — reset all micro-cores
            for mc in self.cores:
                mc._reset_state()
                mc.halted = True  # held in reset → halted
            # Reset cluster MPU to supervisor / open
            self.cl_priv_level = 0
            self.cl_mpu_base = 0
            self.cl_mpu_limit = 0
        self.enabled = en
        if not en:
            # Entering reset — halt all micro-cores
            for mc in self.cores:
                mc.halted = True
                mc.idle = False


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
                 nic_backend: 'Optional[NICBackend]' = None,
                 num_cores: int = 1,
                 num_clusters: int = 0,
                 hbw_size: int = HBW_SIZE,
                 ext_mem_size: int = 0):
        self.ram_size = ram_size          # Bank 0 (system RAM)
        self.num_full_cores = num_cores   # full (major) cores
        self.num_clusters = num_clusters
        self.hbw_size = hbw_size          # Banks 1–3 (HBW math RAM)
        self.ext_mem_size = ext_mem_size  # External memory (HyperRAM/SDRAM)

        # Total core count matches RTL NUM_ALL_CORES
        self.num_micro_cores = num_clusters * MICRO_PER_CLUSTER
        self.num_cores = num_cores + self.num_micro_cores

        # Shared memory — all cores reference the same bytearray
        self._shared_mem = bytearray(ram_size)

        # HBW math RAM (contiguous; banks 1–3)
        self._hbw_mem = bytearray(hbw_size) if hbw_size > 0 else bytearray()

        # External memory (HyperRAM / SDRAM)
        self._ext_mem = bytearray(ext_mem_size) if ext_mem_size > 0 else bytearray()
        self.ext_mem_base = EXT_MEM_BASE if ext_mem_size > 0 else 0
        self.ext_mem_end = (EXT_MEM_BASE + ext_mem_size) if ext_mem_size > 0 else 0

        # Create full CPU cores
        self.cores: list[Megapad64] = []
        for i in range(num_cores):
            cpu = Megapad64(mem_size=ram_size, core_id=i,
                            num_cores=self.num_cores)
            cpu.mem = self._shared_mem
            if hbw_size > 0 and hasattr(cpu, 'attach_hbw'):
                cpu.attach_hbw(self._hbw_mem, HBW_BASE, hbw_size)
            self.cores.append(cpu)

        # Create micro-core clusters
        self.clusters: list[MicroCluster] = []
        for c in range(num_clusters):
            id_base = MICRO_ID_BASE + c * MICRO_PER_CLUSTER
            cluster = MicroCluster(
                cluster_id=c, id_base=id_base,
                n=MICRO_PER_CLUSTER,
                shared_mem=self._shared_mem,
                mem_size=ram_size,
                num_all_cores=self.num_cores,
            )
            self.clusters.append(cluster)
            # Add all micro-cores to the flat core list
            for mc in cluster.cores:
                if hbw_size > 0 and hasattr(mc, 'attach_hbw'):
                    mc.attach_hbw(self._hbw_mem, HBW_BASE, hbw_size)
                self.cores.append(mc)

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
            backend=nic_backend,
        )
        self.sysinfo = SystemInfo(
            bank0_size=ram_size,
            num_cores=self.num_cores,
            hbw_base=HBW_BASE,
            hbw_size=hbw_size,
            int_mem_total=ram_size + hbw_size,
            has_storage=storage_image is not None,
            has_nic=True,
            ext_mem_base=self.ext_mem_base,
            ext_mem_size=ext_mem_size,
        )
        self.mailbox = MailboxDevice(num_cores=self.num_cores)
        self.spinlock = SpinlockDevice()
        self.crc = CRCDevice()
        self.aes = AESDevice()
        self.sha3 = SHA3Device()
        self.trng = TRNGDevice()
        self.x25519 = FieldALUDevice()
        self.ntt = NTTDevice()
        self.kem = KemDevice()
        self.fb = FramebufferDevice()

        self.bus.register(self.uart)
        self.bus.register(self.timer)
        self.bus.register(self.storage)
        self.bus.register(self.sysinfo)
        self.bus.register(self.nic)
        self.bus.register(self.mailbox)
        self.bus.register(self.spinlock)
        self.bus.register(self.crc)
        self.bus.register(self.aes)
        self.bus.register(self.sha3)
        self.bus.register(self.trng)
        self.bus.register(self.x25519)
        self.bus.register(self.ntt)
        self.bus.register(self.kem)
        self.bus.register(self.fb)

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

        # Wire cluster_en callback — writes to SysInfo 0x18 actually
        # enable/disable clusters
        if self.clusters:
            original_sysinfo_write = self.sysinfo.write8
            clusters = self.clusters
            def cluster_en_write(offset: int, val: int):
                original_sysinfo_write(offset, val)
                if 0x18 <= offset < 0x20:
                    # Update cluster enable state
                    en_mask = self.sysinfo.cluster_en
                    for i, cl in enumerate(clusters):
                        cl.set_enabled(bool(en_mask & (1 << i)))
            self.sysinfo.write8 = cluster_en_write

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
        the MMIO range get routed to the device bus, accesses in the
        HBW range get routed to the HBW memory banks, and accesses to
        the cluster scratchpad sentinel get routed to the cluster's
        local scratchpad (micro-cores only).
        """
        original_read8 = cpu.mem_read8
        original_write8 = cpu.mem_write8
        core_id = cpu.core_id
        bus = self.bus
        hbw_mem = self._hbw_mem
        hbw_size = self.hbw_size
        ext_mem = self._ext_mem
        ext_mem_size = self.ext_mem_size
        ext_mem_base = self.ext_mem_base
        ext_mem_end = self.ext_mem_end

        # Scratchpad interception for micro-cores
        cluster = getattr(cpu, '_cluster', None)

        # For micro-cores in a cluster, MPU is enforced at cluster level.
        # For full cores, MPU is per-core.
        if cluster:
            _priv = lambda: cluster.cl_priv_level
            _mpu_base = lambda: cluster.cl_mpu_base
            _mpu_limit = lambda: cluster.cl_mpu_limit
        else:
            _priv = lambda: cpu.priv_level
            _mpu_base = lambda: cpu.mpu_base
            _mpu_limit = lambda: cpu.mpu_limit

        def patched_read8(addr: int) -> int:
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                bus.requester_id = core_id
                offset = addr - MMIO_START
                return bus.read8(offset)
            if cluster and (addr >> 32) == 0xFFFF_FE00:
                return cluster.spad_read8(addr & 0xFFFF_FFFF)
            # MPU / privilege check for non-MMIO accesses
            if _priv():
                # User mode: block HBW entirely
                if hbw_size > 0 and HBW_BASE <= addr < HBW_END:
                    cpu.trap_addr = addr
                    if cluster:
                        cluster.cl_priv_level = 0  # drop to S-mode
                    raise TrapError(IVEC_PRIV_FAULT,
                                    f"User read from HBW @ {addr:#018x}")
                # Check MPU window for RAM
                mpu_b, mpu_l = _mpu_base(), _mpu_limit()
                if mpu_l > mpu_b:
                    if addr < mpu_b or addr >= mpu_l:
                        cpu.trap_addr = addr
                        if cluster:
                            cluster.cl_priv_level = 0  # drop to S-mode
                        raise TrapError(IVEC_PRIV_FAULT,
                                        f"MPU violation @ {addr:#018x}")
            if hbw_size > 0 and HBW_BASE <= addr < HBW_END:
                return hbw_mem[addr - HBW_BASE]
            if ext_mem_size > 0 and ext_mem_base <= addr < ext_mem_end:
                return ext_mem[addr - ext_mem_base]
            return original_read8(addr)

        def patched_write8(addr: int, val: int):
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                bus.requester_id = core_id
                offset = addr - MMIO_START
                bus.write8(offset, val)
                return
            if cluster and (addr >> 32) == 0xFFFF_FE00:
                cluster.spad_write8(addr & 0xFFFF_FFFF, val)
                return
            # MPU / privilege check for non-MMIO accesses
            if _priv():
                # User mode: block HBW entirely
                if hbw_size > 0 and HBW_BASE <= addr < HBW_END:
                    cpu.trap_addr = addr
                    if cluster:
                        cluster.cl_priv_level = 0  # drop to S-mode
                    raise TrapError(IVEC_PRIV_FAULT,
                                    f"User write to HBW @ {addr:#018x}")
                # Check MPU window for RAM
                mpu_b, mpu_l = _mpu_base(), _mpu_limit()
                if mpu_l > mpu_b:
                    if addr < mpu_b or addr >= mpu_l:
                        cpu.trap_addr = addr
                        if cluster:
                            cluster.cl_priv_level = 0  # drop to S-mode
                        raise TrapError(IVEC_PRIV_FAULT,
                                        f"MPU violation @ {addr:#018x}")
            if hbw_size > 0 and HBW_BASE <= addr < HBW_END:
                hbw_mem[addr - HBW_BASE] = val & 0xFF
                return
            if ext_mem_size > 0 and ext_mem_base <= addr < ext_mem_end:
                ext_mem[addr - ext_mem_base] = val & 0xFF
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
        addr = u64(addr)
        if self.hbw_size > 0 and HBW_BASE <= addr < HBW_END:
            return self._hbw_mem[addr - HBW_BASE]
        if self.ext_mem_size > 0 and self.ext_mem_base <= addr < self.ext_mem_end:
            return self._ext_mem[addr - self.ext_mem_base]
        return self._shared_mem[addr % self.ram_size]

    def _raw_mem_write(self, addr: int, val: int):
        addr = u64(addr)
        if self.hbw_size > 0 and HBW_BASE <= addr < HBW_END:
            self._hbw_mem[addr - HBW_BASE] = val & 0xFF
        elif self.ext_mem_size > 0 and self.ext_mem_base <= addr < self.ext_mem_end:
            self._ext_mem[addr - self.ext_mem_base] = val & 0xFF
        else:
            self._shared_mem[addr % self.ram_size] = val & 0xFF

    # -----------------------------------------------------------------
    #  Loading
    # -----------------------------------------------------------------

    def load_binary(self, addr: int, data: bytes | bytearray):
        """Load raw bytes into shared RAM, HBW, or external memory."""
        for i, b in enumerate(data):
            target = (addr + i)
            if self.hbw_size > 0 and HBW_BASE <= target < HBW_END:
                self._hbw_mem[target - HBW_BASE] = b
            elif self.ext_mem_size > 0 and self.ext_mem_base <= target < self.ext_mem_end:
                self._ext_mem[target - self.ext_mem_base] = b
            else:
                self._shared_mem[target % self.ram_size] = b

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

        Full cores start at the entry point (matching FPGA behaviour).
        Core 0 gets SP at top of RAM; secondary cores get per-core stacks.
        Micro-cores start halted — they are only activated when their
        cluster is enabled via the SysInfo CLUSTER_EN register.
        """
        for i, cpu in enumerate(self.cores):
            cpu._reset_state()

            # Micro-cores in clusters start halted (cluster_en defaults to 0)
            if isinstance(cpu, Megapad64Micro):
                cpu.halted = True
                cpu.idle = False
                continue

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

    def run_batch(self, n: int = 100_000) -> int:
        """Execute up to *n* instructions using C++ batch mode when available.

        For single-core systems with the C++ accelerator this runs the
        entire inner loop in C++ (``cpu.run_steps``), calling back to
        Python only for the rare MMIO access.

        For multicore systems with the accelerator, each active core
        runs a chunk of steps in C++ per round, with device ticking
        and IRQ delivery between rounds.

        Falls back to per-step execution when the accelerator is not
        loaded.

        Returns the number of steps actually executed.
        """
        # --- wake checks (same as step()) ---
        for cpu in self.cores:
            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                cpu.idle = False
            if cpu.idle and cpu.core_id == 0:
                if self.uart.has_rx_data:
                    cpu.idle = False
                elif self.timer.irq_pending and cpu.flag_i:
                    cpu.idle = False

        if self.all_halted or self.all_idle_or_halted:
            return 0

        # ---------- C++ fast path (single active core) ----------
        cpu = self.cores[0]
        if (hasattr(cpu, '_accel_backend')
                and not cpu.halted and not cpu.idle
                and all(c.idle or c.halted for c in self.cores[1:])):
            try:
                steps, reason = cpu.run_steps(n)
            except TrapError as e:
                if cpu.ivt_base != 0:
                    cpu._trap(e.ivec_id)
                steps = 1
            except HaltError:
                return 1

            # Timer / device catch-up
            if steps > 0:
                self.bus.tick(steps)

            # Timer IRQ delivery
            if self.timer.irq_pending:
                for c in self.cores:
                    if c.flag_i and not c.halted:
                        c._trap(IVEC_TIMER)

            return max(steps, 1)

        # ---------- C++ multicore path ----------
        # Run each active core for a chunk via C++ run_steps, with
        # device ticking and IRQ delivery between rounds.
        has_accel = hasattr(self.cores[0], '_accel_backend')
        if has_accel and self.num_cores > 1:
            CHUNK = 1000  # steps per core per round
            total = 0
            remaining = n
            while remaining > 0 and not self.all_halted:
                if self.all_idle_or_halted:
                    break
                chunk = min(CHUNK, remaining)
                round_steps = 0
                for cpu in self.cores:
                    if cpu.halted or cpu.idle:
                        continue
                    try:
                        steps, reason = cpu.run_steps(chunk)
                        round_steps += steps
                    except TrapError as e:
                        if cpu.ivt_base != 0:
                            cpu._trap(e.ivec_id)
                        round_steps += 1
                    except HaltError:
                        round_steps += 1

                # Device ticking + IRQ delivery
                if round_steps > 0:
                    self.bus.tick(round_steps)
                    total += round_steps
                    remaining -= round_steps

                # Timer IRQ delivery
                if self.timer.irq_pending:
                    for c in self.cores:
                        if c.flag_i and not c.halted:
                            c._trap(IVEC_TIMER)

                # IPI delivery
                for cpu in self.cores:
                    if cpu.irq_ipi and cpu.flag_i and not cpu.halted and not cpu.idle:
                        cpu._trap(IVEC_IPI)

                if round_steps == 0:
                    break
            return max(total, 1)

        # ---------- Pure-Python fallback ----------
        total = 0
        for _ in range(n):
            if self.all_halted:
                break
            if self.all_idle_or_halted:
                break
            try:
                total += self.step()
            except HaltError:
                break
        return max(total, 1)

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
                      f"backend={self.nic.backend_name}")
        if self.num_cores > 1:
            lines.append(f"  Mailbox: cores={self.num_cores} "
                         f"pending={[self.mailbox.pending[i] for i in range(self.num_cores)]}")
            locked = [i for i in range(self.spinlock.num_locks) if self.spinlock.locked[i]]
            lines.append(f"  Spinlocks: locked={locked} "
                         f"owners={[self.spinlock.owner[i] for i in locked]}")
        return "\n".join(lines)
