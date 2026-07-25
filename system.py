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

from contextlib import nullcontext
from dataclasses import dataclass, replace
import threading
import weakref
from typing import Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from nic_backends import NICBackend

from _mp64_accel import (
    BusFault,
    BusOperation,
    DmaBeat,
    DmaEndpointView,
    ExternalEventKind,
)
from accel_wrapper import (
    HaltError,
    IVEC_BUS_FAULT,
    IVEC_IPI,
    IVEC_PRIV_FAULT,
    IVEC_TIMER,
    Megapad64,
    Megapad64Micro,
    NativeSystemState,
    TrapError,
    u64,
)
from megapad64 import (
    Megapad64Micro as _PyMegapad64Micro,
    CSR_BIST_CMD, CSR_BIST_STATUS, CSR_BIST_FAIL_ADDR,
    CSR_BIST_FAIL_DATA, MICRO_PER_CLUSTER, NUM_CLUSTERS, MICRO_ID_BASE,
    NUM_ALL_CORES, CLUSTER_SPAD_BYTES, CLUSTER_SPAD_ADDR,
    CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT, CSR_CL_IVTBASE,
)
from devices import (
    MMIO_BASE, DeviceBus, BusError, UART, Timer, Storage, SystemInfo, NetworkDevice,
    AudioOutput,
    MailboxDevice, SpinlockDevice, NTTDevice, KemDevice,
    FramebufferDevice, CppFramebufferProxy, CppTimerProxy, CppUartGeomProxy,
    CppRTCProxy,
    MailboxDevice, SpinlockDevice,
    NTTDevice, KemDevice, FramebufferDevice,
    SECTOR_SIZE, UART_BASE, UART_GEOM_BASE, TIMER_BASE, STORAGE_BASE,
    SYSINFO_BASE, NIC_BASE, MBOX_BASE, SPINLOCK_BASE,
    NTT_BASE, KEM_BASE, FB_BASE, NIC_MTU, NIC_MAX_FRAME,
    PortBridgeCSR,
    WotsChainAccel,
)

# Capture the full-core batch extension seams before user/test monkeypatches.
# The Phase-1 native loop bypasses these two methods, so any replacement must
# retain the compatibility scheduler that dynamically dispatches through them.
_CANONICAL_RUN_STEPS_STATS = Megapad64.run_steps_stats
_CANONICAL_RUN_STEPS_STATS_IN_SCOPE = (
    Megapad64._run_steps_stats_in_memory_scope
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

# Dedicated VRAM aperture — sits just below HBW in the 32-bit map.
# On FPGA this maps to a separate SRAM port or second HyperRAM chip.
# Default 4 MiB: enough for double-buffered 1280×720 RGBA8888.
VRAM_BASE         = 0xFF00_0000
VRAM_DEFAULT_SIZE = 4 * (1 << 20)    # 4 MiB

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


def _cpu_logical_memory_use(cpu):
    """Return one full-core operation scope, or a no-op for micro-cores."""
    state = getattr(cpu, "_cs", None)
    if state is None:
        return nullcontext()
    return state._logical_memory_use()


def _cpu_memory_use(cpu):
    """Return a non-executing full-core scope, or a no-op for micro-cores."""
    state = getattr(cpu, "_cs", None)
    if state is None:
        return nullcontext()
    return state._memory_use()


@dataclass(frozen=True)
class SystemRunStats:
    """Cycle and instruction progress from one system batch."""

    instructions_executed: int
    system_cycles_advanced: int
    per_core_instructions: tuple[int, ...]
    per_core_cycles: tuple[int, ...]
    per_core_dispatches: tuple[int, ...] = ()
    per_core_stop_reasons: tuple[tuple[int, ...], ...] = ()
    native_scheduler: bool = False
    native_rounds: int = 0
    native_continuations: int = 0
    system_stop_reason: str = "instruction_limit"
    stop_cycle: int = 0
    event_source_mask: int = 0
    per_core_interrupts: tuple[int, ...] = ()
    interrupts_delivered: int = 0
    external_events_applied: int = 0
    pending_interrupt_core: int = -1
    pending_interrupt_vector: int = -1


# ---------------------------------------------------------------------------
#  MicroCluster — matches RTL mp64_cluster.v
# ---------------------------------------------------------------------------

class MicroCluster:
    """Emulates an mp64_cluster: N micro-cores with shared resources.

    Shared resources:
      - 1 KiB scratchpad RAM (cluster-local, not on main bus)
      - Shared MUL/DIV unit (cooperatively arbitrated in native batches)
      - Shared CRC accumulator, mode, and transaction lock
      - Shared tile and SHA compatibility state
      - Hardware barrier register
      - BIST controller for scratchpad/multiplier

    Cluster enable/disable is controlled by the SysInfo CLUSTER_EN
    register.  When disabled, all micro-cores are held in reset.
    """

    @staticmethod
    def _new_shared_engine_state() -> dict:
        return {
            "sb": 0,
            "sr": 0,
            "sc": 0,
            "sw": 1,
            "tmode": 0,
            "tctrl": 0,
            "tsrc0": 0,
            "tsrc1": 0,
            "tdst": 0,
            "acc": [0, 0, 0, 0],
            "tstride_r": 0,
            "tstride_c": 0,
            "ttile_h": 8,
            "ttile_w": 8,
            "sha_mode": 0,
            "sha_msglen_lo": 0,
            "sha_msglen_hi": 0,
        }

    def __init__(self, cluster_id: int, id_base: int,
                 n: int = MICRO_PER_CLUSTER,
                 shared_mem: bytearray = None,
                 mem_size: int = 1 << 20,
                 num_all_cores: int = NUM_ALL_CORES,
                 native_system=None,
                 native_micro_offset: int = 0):
        self.cluster_id = cluster_id
        self.id_base = id_base
        self.n = n
        self._native_system = native_system
        self._native_cluster_index = (
            cluster_id if native_system is not None else None
        )
        self.enabled = False   # matches RTL default: clusters off at reset

        # Scratchpad — 1 KiB, cluster-local
        self.scratchpad = bytearray(CLUSTER_SPAD_BYTES)
        self._crc_acc = 0xFFFF_FFFF
        self._crc_mode = 0
        self._crc_locked = False
        self._crc_owner: Optional[int] = None
        self._sha_locked = False
        self._sha_owner: Optional[int] = None
        self._tile_state = self._new_shared_engine_state()

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
        self.cl_ivt_base = 0     # shared IVT base address

        # Cluster-shared CRC state. MODE, INIT, or SEED acquires the
        # transaction lock; FIN publishes the final accumulator and releases
        # it. The owner is a local core index, matching the RTL arbiter.
        self.reset_shared_resources()

        # Create micro-cores
        self.cores: list[Megapad64Micro | _PyMegapad64Micro] = []
        for i in range(n):
            core_id = id_base + i
            if native_system is None:
                mc = _PyMegapad64Micro(
                    mem_size=mem_size,
                    core_id=core_id,
                    num_cores=num_all_cores,
                )
            else:
                mc = Megapad64Micro._from_system_state(
                    native_system,
                    native_micro_offset + i,
                    num_cores=num_all_cores,
                )
            mc._cluster = self
            if shared_mem is not None and native_system is None:
                mc.mem = shared_mem
            mc.halted = True
            mc.idle = False
            self.cores.append(mc)

    # -- Shared CRC engine --

    def _native_crc_snapshot(self) -> Optional[dict]:
        if self._native_cluster_index is None:
            return None
        return self._native_system._cluster_crc_snapshot(
            self._native_cluster_index
        )

    def _native_crc_update(self, **changes) -> bool:
        if self._native_cluster_index is None:
            return False
        self._native_system._cluster_crc_update(
            self._native_cluster_index,
            changes,
        )
        return True

    @property
    def crc_acc(self) -> int:
        state = self._native_crc_snapshot()
        return self._crc_acc if state is None else int(state["acc"])

    @crc_acc.setter
    def crc_acc(self, value: int):
        value = int(value) & ((1 << 64) - 1)
        if not self._native_crc_update(acc=value):
            self._crc_acc = value

    @property
    def crc_mode(self) -> int:
        state = self._native_crc_snapshot()
        return self._crc_mode if state is None else int(state["mode"])

    @crc_mode.setter
    def crc_mode(self, value: int):
        value = int(value)
        value = value if value in (0, 1, 2) else 0
        if not self._native_crc_update(mode=value):
            self._crc_mode = value

    @property
    def crc_locked(self) -> bool:
        state = self._native_crc_snapshot()
        return self._crc_locked if state is None else bool(state["locked"])

    @crc_locked.setter
    def crc_locked(self, value: bool):
        value = bool(value)
        if value:
            if not self.crc_locked:
                raise ValueError(
                    "claim the CRC transaction with crc_try_acquire()"
                )
            return
        if not self._native_crc_update(locked=False, owner=None):
            self._crc_locked = False
            self._crc_owner = None

    @property
    def crc_owner(self) -> Optional[int]:
        state = self._native_crc_snapshot()
        if state is None:
            return self._crc_owner
        owner = state["owner"]
        return None if owner is None else int(owner)

    @crc_owner.setter
    def crc_owner(self, value: Optional[int]):
        owner = None if value is None else int(value)
        if owner is not None and not 0 <= owner < self.n:
            raise ValueError("cluster CRC owner is out of range")
        changes = {
            "locked": owner is not None,
            "owner": owner,
        }
        if not self._native_crc_update(**changes):
            self._crc_locked = owner is not None
            self._crc_owner = owner

    def reset_crc(self):
        """Reset shared CRC state and release any stranded transaction."""
        if self._native_cluster_index is not None:
            self._native_system._cluster_crc_update(
                self._native_cluster_index,
                {
                    "acc": 0xFFFF_FFFF,
                    "mode": 0,
                    "locked": False,
                    "owner": None,
                },
            )
            return
        self._crc_acc = 0xFFFF_FFFF
        self._crc_mode = 0
        self._crc_locked = False
        self._crc_owner = None

    def reset_shared_resources(self):
        """Reset cluster engines, transaction locks, and grant cursors."""
        if self._native_cluster_index is not None:
            self._native_system.reset_cluster_state(
                self._native_cluster_index
            )
            return
        self.reset_crc()
        self._sha_locked = False
        self._sha_owner = None
        self._tile_state = self._new_shared_engine_state()

    def crc_try_acquire(self, global_core_id: int) -> bool:
        """Acquire the CRC transaction for a core, or report contention."""
        local = global_core_id - self.id_base
        if not 0 <= local < self.n:
            return False
        if self._native_cluster_index is not None:
            return bool(
                self._native_system._cluster_crc_try_acquire(
                    self._native_cluster_index,
                    local,
                )
            )
        if not self._crc_locked:
            self._crc_locked = True
            self._crc_owner = local
        return self._crc_owner == local

    def crc_is_owner(self, global_core_id: int) -> bool:
        local = global_core_id - self.id_base
        if self._native_cluster_index is not None:
            return bool(
                self._native_system._cluster_crc_is_owner(
                    self._native_cluster_index,
                    local,
                )
            )
        return self._crc_locked and self._crc_owner == local

    def crc_release(self, global_core_id: int):
        """Release the CRC lock when the calling core owns it."""
        if self._native_cluster_index is not None:
            local = global_core_id - self.id_base
            self._native_system._cluster_crc_release(
                self._native_cluster_index,
                local,
            )
            return
        if self.crc_is_owner(global_core_id):
            self._crc_locked = False
            self._crc_owner = None

    # -- Shared SHA transaction ownership --

    def _native_sha_snapshot(self) -> Optional[dict]:
        if self._native_cluster_index is None:
            return None
        return self._native_system._cluster_sha_snapshot(
            self._native_cluster_index
        )

    @property
    def sha_locked(self) -> bool:
        state = self._native_sha_snapshot()
        return self._sha_locked if state is None else bool(state["locked"])

    @property
    def sha_owner(self) -> Optional[int]:
        state = self._native_sha_snapshot()
        if state is None:
            return self._sha_owner
        owner = state["owner"]
        return None if owner is None else int(owner)

    def sha_try_acquire(self, global_core_id: int) -> bool:
        """Acquire SHA INIT-to-FINAL ownership for one local core."""
        local = global_core_id - self.id_base
        if not 0 <= local < self.n:
            return False
        if self._native_cluster_index is not None:
            return bool(
                self._native_system._cluster_sha_try_acquire(
                    self._native_cluster_index,
                    local,
                )
            )
        if not self._sha_locked:
            self._sha_locked = True
            self._sha_owner = local
        return self._sha_owner == local

    def sha_is_owner(self, global_core_id: int) -> bool:
        local = global_core_id - self.id_base
        if self._native_cluster_index is not None:
            return bool(
                self._native_system._cluster_sha_is_owner(
                    self._native_cluster_index,
                    local,
                )
            )
        return self._sha_locked and self._sha_owner == local

    def sha_release(self, global_core_id: int):
        """Release SHA transaction ownership after the owner's FINAL."""
        local = global_core_id - self.id_base
        if self._native_cluster_index is not None:
            self._native_system._cluster_sha_release(
                self._native_cluster_index,
                local,
            )
            return
        if self.sha_is_owner(global_core_id):
            self._sha_locked = False
            self._sha_owner = None

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
        if addr == CSR_CL_IVTBASE:
            return self.cl_ivt_base
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
        # Cluster CSRs — writable unconditionally (user mode stripped)
        if addr == CSR_CL_PRIV:
            self.cl_priv_level = val & 1
        elif addr == CSR_CL_MPU_BASE:
            self.cl_mpu_base = val & ((1 << 64) - 1)
        elif addr == CSR_CL_MPU_LIMIT:
            self.cl_mpu_limit = val & ((1 << 64) - 1)
        elif addr == CSR_CL_IVTBASE:
            self.cl_ivt_base = val & ((1 << 64) - 1)

    def _bist_spad(self) -> bool:
        """March C- test on scratchpad memory."""
        sz = CLUSTER_SPAD_BYTES
        saved = self.spad_snapshot()
        ok = True
        # Phase 0: write all 0x00
        for i in range(sz):
            self.spad_write8(i, 0x00)
        # Phase 1: read 0x00, write 0xFF ascending
        for i in range(sz):
            observed = self.spad_read8(i)
            if observed != 0x00:
                self.bist_fail_addr = i
                self.bist_fail_data = observed
                ok = False
                break
            self.spad_write8(i, 0xFF)
        if ok:
            # Phase 2: read 0xFF, write 0x00 descending
            for i in range(sz - 1, -1, -1):
                observed = self.spad_read8(i)
                if observed != 0xFF:
                    self.bist_fail_addr = i
                    self.bist_fail_data = (0xFF << 8) | observed
                    ok = False
                    break
                self.spad_write8(i, 0x00)
        # Restore
        self.spad_restore(saved)
        return ok

    # -- Scratchpad access helpers --

    def spad_read8(self, offset: int) -> int:
        if self._native_cluster_index is not None:
            return int(
                self._native_system._cluster_spad_read8(
                    self._native_cluster_index,
                    int(offset) % CLUSTER_SPAD_BYTES,
                )
            )
        offset = offset % CLUSTER_SPAD_BYTES
        return self.scratchpad[offset]

    def spad_write8(self, offset: int, val: int):
        if self._native_cluster_index is not None:
            self._native_system._cluster_spad_write8(
                self._native_cluster_index,
                int(offset) % CLUSTER_SPAD_BYTES,
                int(val) & 0xFF,
            )
            return
        offset = offset % CLUSTER_SPAD_BYTES
        self.scratchpad[offset] = val & 0xFF

    def spad_snapshot(self) -> bytes:
        if self._native_cluster_index is not None:
            return bytes(
                self._native_system._cluster_spad_snapshot(
                    self._native_cluster_index
                )
            )
        return bytes(self.scratchpad)

    def spad_restore(self, image: bytes):
        if len(image) != CLUSTER_SPAD_BYTES:
            raise ValueError(
                "cluster scratchpad image must be exactly 1024 bytes"
            )
        if self._native_cluster_index is not None:
            self._native_system._cluster_spad_restore(
                self._native_cluster_index,
                bytes(image),
            )
            return
        self.scratchpad[:] = image

    # -- Shared tile and SHA compatibility bank --

    def _shared_engine_snapshot(self) -> dict:
        if self._native_cluster_index is not None:
            state = dict(
                self._native_system._cluster_tile_snapshot(
                    self._native_cluster_index
                )
            )
            state["acc"] = list(state["acc"])
            return state
        state = dict(self._tile_state)
        state["acc"] = list(self._tile_state["acc"])
        return state

    def load_shared_engine_state(self, cpu) -> None:
        state = self._shared_engine_snapshot()
        for name in (
            "sb", "sr", "sc", "sw", "tmode", "tctrl",
            "tsrc0", "tsrc1", "tdst", "tstride_r", "tstride_c",
            "ttile_h", "ttile_w", "sha_mode", "sha_msglen_lo",
            "sha_msglen_hi",
        ):
            setattr(cpu, name, int(state[name]))
        cpu.acc = list(state["acc"])

    def store_shared_engine_state(self, cpu) -> None:
        state = {
            name: int(getattr(cpu, name))
            for name in (
                "sb", "sr", "sc", "sw", "tmode", "tctrl",
                "tsrc0", "tsrc1", "tdst", "tstride_r", "tstride_c",
                "ttile_h", "ttile_w", "sha_mode", "sha_msglen_lo",
                "sha_msglen_hi",
            )
        }
        state["acc"] = [int(value) for value in cpu.acc]
        if self._native_cluster_index is not None:
            self._native_system._cluster_tile_update(
                self._native_cluster_index,
                state,
            )
            return
        self._tile_state = state

    # -- Enable / disable --

    def set_enabled(self, en: bool):
        """Enable or disable the cluster (matching RTL cluster_en gating)."""
        if en and not self.enabled:
            self.reset_shared_resources()
            # Coming out of reset starts every micro-core at its reset vector.
            for mc in self.cores:
                mc._reset_state()
                mc.halted = False
                mc.idle = False
            # Reset cluster MPU to supervisor / open
            self.cl_priv_level = 0
            self.cl_mpu_base = 0
            self.cl_mpu_limit = 0
        self.enabled = en
        if not en:
            self.reset_shared_resources()
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

    _COMPLETE_SNAPSHOT_UNSUPPORTED_REASON = (
        "complete machine snapshots are unavailable for the native Phase 2 "
        "timeline; MP64SNAP v1 omits the shared clock, main bus, suspended "
        "execution, external-event journal, interrupts, and authoritative "
        "native device state"
    )

    def __init__(self, ram_size: int = 1 << 20,
                 storage_image: Optional[str] = None,
                 nic_port: Optional[int] = None,
                 nic_peer_port: Optional[int] = None,
                 nic_backend: 'Optional[NICBackend]' = None,
                 num_cores: int = 1,
                 num_clusters: int = 0,
                 hbw_size: int = HBW_SIZE,
                 ext_mem_size: int = 0,
                 vram_size: int = VRAM_DEFAULT_SIZE,
                 realtime_clock: bool = False,
                 rtc_epoch_ms: Optional[int] = None,
                 terminal_cols: int = 80,
                 terminal_rows: int = 24):
        self.ram_size = ram_size          # Bank 0 (system RAM)
        self.num_full_cores = num_cores   # full (major) cores
        self.num_clusters = num_clusters
        self.hbw_size = hbw_size          # Banks 1–3 (HBW math RAM)
        self.hbw_end = (HBW_BASE + hbw_size) if hbw_size > 0 else 0
        self.ext_mem_size = ext_mem_size  # External memory (HyperRAM/SDRAM)
        self.vram_size = vram_size        # Dedicated VRAM

        # Total core count matches RTL NUM_ALL_CORES
        self.num_micro_cores = num_clusters * MICRO_PER_CLUSTER
        self.num_cores = num_cores + self.num_micro_cores
        self._scheduler_lock = threading.RLock()

        # Shared memory — all cores reference the same bytearray
        self._shared_mem = bytearray(ram_size)

        # HBW math RAM (contiguous; banks 1–3)
        self._hbw_mem = bytearray(hbw_size) if hbw_size > 0 else bytearray()

        # External memory (HyperRAM / SDRAM)
        self._ext_mem = bytearray(ext_mem_size) if ext_mem_size > 0 else bytearray()
        self.ext_mem_base = EXT_MEM_BASE if ext_mem_size > 0 else 0
        self.ext_mem_end = (EXT_MEM_BASE + ext_mem_size) if ext_mem_size > 0 else 0

        # Dedicated VRAM (separate from HBW and system RAM)
        self._vram_mem = bytearray(vram_size) if vram_size > 0 else bytearray()
        self.vram_base = VRAM_BASE if vram_size > 0 else 0
        self.vram_end = (VRAM_BASE + vram_size) if vram_size > 0 else 0

        # Native Phase-1 owner for all full-core CPUState lifetimes and their
        # single shared mapping set.  Attach each exporter exactly once before
        # borrowing the first core, which seals the mapping against divergent
        # per-core replacement. Heterogeneous micro-cores and Python-bus
        # devices retain their explicit compatibility paths.
        self._native_system = NativeSystemState(
            num_cores,
            self.num_cores,
            num_cores + num_clusters + 2,
        )
        self._native_system.attach_mem(self._shared_mem, ram_size)
        if hbw_size > 0:
            self._native_system.attach_hbw_mem(
                self._hbw_mem, HBW_BASE, hbw_size
            )
        if ext_mem_size > 0:
            self._native_system.attach_ext_mem(
                self._ext_mem, EXT_MEM_BASE, ext_mem_size
            )
        if vram_size > 0:
            self._native_system.attach_vram(
                self._vram_mem, VRAM_BASE, vram_size
            )

        # Create Python API wrappers around the natively owned full cores.
        self.cores: list[Megapad64] = []
        for i in range(num_cores):
            cpu = Megapad64._from_system_state(
                self._native_system,
                i,
                num_cores=self.num_cores,
            )
            self.cores.append(cpu)

        # Create micro-core clusters
        self.clusters: list[MicroCluster] = []
        for c in range(num_clusters):
            id_base = num_cores + c * MICRO_PER_CLUSTER
            cluster = MicroCluster(
                cluster_id=c, id_base=id_base,
                n=MICRO_PER_CLUSTER,
                shared_mem=self._shared_mem,
                mem_size=ram_size,
                num_all_cores=self.num_cores,
                native_system=self._native_system,
                native_micro_offset=c * MICRO_PER_CLUSTER,
            )
            self.clusters.append(cluster)
            # Add all micro-cores to the flat core list
            for mc in cluster.cores:
                self.cores.append(mc)

        # Convenience alias: self.cpu always refers to core 0
        self.cpu = self.cores[0]

        # Python micro-cores observe the same SystemState-owned IPI lines as
        # native full cores.  Their property delegates through these hooks.
        for cpu in self.cores[num_cores:]:
            cpu._irq_ipi_getter = self._native_system.ipi_line
            cpu._irq_ipi_setter = self._native_system.set_ipi_line

        # --- Device bus ---
        self.bus = DeviceBus()

        self.uart = UART()
        # Timer is now handled natively by C++ accelerator — use proxy
        self.timer = CppTimerProxy(self.cores[0]._cs)
        self.storage = Storage(storage_image)
        self.storage._cycle_mutation_guard = (
            self._native_system._require_storage_mutation_allowed
        )
        self.storage._stall_release_guard = (
            self._native_system._require_storage_stall_release_allowed
        )
        self.audio = AudioOutput()
        self.nic = NetworkDevice(
            passthrough_port=nic_port,
            passthrough_peer_port=nic_peer_port,
            backend=nic_backend,
            autostart=False,
        )
        self.sysinfo = SystemInfo(
            bank0_size=ram_size,
            num_cores=self.num_cores,
            num_full_cores=self.num_full_cores,
            hbw_base=HBW_BASE,
            hbw_size=hbw_size,
            int_mem_total=ram_size + hbw_size,
            has_storage=storage_image is not None,
            has_nic=True,
            ext_mem_base=self.ext_mem_base,
            ext_mem_size=ext_mem_size,
            vram_base=self.vram_base,
            vram_size=vram_size,
        )
        self.mailbox = MailboxDevice(num_cores=self.num_cores)
        self.mailbox.attach_ipi_router(
            pending_mask=self._native_system.ipi_pending_mask,
            pending_snapshot=self._native_system.ipi_pending_snapshot,
            send=self._native_system.ipi_send,
            acknowledge=self._native_system.ipi_ack,
        )
        self.spinlock = SpinlockDevice()
        self.ntt = NTTDevice()
        self.kem = KemDevice()
        # FB is now handled natively by C++ accelerator — use proxy
        self.fb = CppFramebufferProxy(self.cores[0]._cs)
        # RTC MMIO is native on every full core through one shared device.
        # The proxy preserves the Python-facing API and micro-core fallback.
        self.rtc = CppRTCProxy(
            self.cores[0]._cs,
            realtime=realtime_clock,
            initial_epoch_ms=rtc_epoch_ms,
        )

        # UART geometry is part of deterministic machine state. Interactive
        # frontends may explicitly publish a different size after startup.
        self.uart_geom = CppUartGeomProxy(
            self.cores[0]._cs,
            initial_cols=terminal_cols,
            initial_rows=terminal_rows,
        )

        # AES, SHA3, NIC, TRNG, UART, FB, Timer, and RTC are handled by native
        # SystemState singletons. NIC MMIO
        # (0x0400) does ~15K–35K accesses per TLS handshake; keeping it
        # in C++ is critical for HTTPS perf.  The Python NetworkDevice
        # remains only as a facade for backend lifecycle, inject_frame(),
        # and status display.

        self.bus.register(self.uart)
        # UART geometry: C++ handles all MMIO; proxy for Python access.
        self.bus.register(self.uart_geom)
        # Timer: C++ handles all MMIO; proxy tick is called via bus.
        self.bus.register(self.timer)
        self.bus.register(self.storage)
        self.bus.register(self.audio)
        self.bus.register(self.sysinfo)
        # Retain the Python NIC for micro-core MMIO and backend/status facade
        # compatibility. Full-core native execution and Python continuations
        # probe their SystemState singleton before reaching this bus entry.
        self.bus.register(self.nic)
        self.bus.register(self.mailbox)
        self.bus.register(self.spinlock)
        self.bus.register(self.ntt)
        self.bus.register(self.kem)
        # FB: NOT registered — C++ handles all MMIO; proxy tick is
        # called explicitly via bus since it's still a Device subclass.
        self.bus.register(self.fb)
        self.bus.register(self.rtc)

        # Port I/O bridge CSR — remap table for OUT/INP → MMIO routing
        self.port_bridge = PortBridgeCSR()
        self.bus.register(self.port_bridge)
        for cpu in self.cores:
            self.port_bridge.attach_cpu(cpu)

        # Python WOTS+ remains the micro-core bus device. Full-core native
        # execution and continuations use SystemState's shared crypto block.
        self.wots = WotsChainAccel()
        self.wots.attach_mem(self._shared_mem)
        self.bus.register(self.wots)
        self.bus.set_tick_driver(self.advance_system_cycles)

        # Wire storage DMA to shared memory
        self.storage._mem_read = self._raw_mem_read
        self.storage._mem_write = self._raw_mem_write
        self.storage._mem_span_valid = self._raw_mem_span_valid
        self.audio._mem_read = self._raw_mem_read
        self.audio._mem_span_valid = self._raw_mem_span_valid

        # ── Shared native NIC, TRNG, and UART ─────────────────
        # DMA uses SystemState's central mappings. TX calls back to Python
        # once per frame, while every full core reaches the same native MMIO
        # state retained by SystemState.
        self._nic_backend = nic_backend
        _py_nic = self.nic
        _be = nic_backend
        cpu0_cs = self.cores[0]._cs
        cpu0_cs.nic_init(bytes(self.nic.mac))
        cpu0_cs.nic_sync_mem_ptrs()

        # TX callback: mirror to Python facade + send via backend.
        def _tx_cb(frame: bytes, py_nic=_py_nic, be=_be) -> bool:
            py_nic.tx_queue.append(frame)
            py_nic.tx_count = (py_nic.tx_count + 1) & 0xFFFF
            if py_nic.on_tx_frame:
                py_nic.on_tx_frame(frame)
            if be is not None:
                return be.send(frame)
            return True

        cpu0_cs.nic_set_tx_callback(_tx_cb)
        cpu0_cs.init_trng()
        cpu0_cs.uart_init()

        # Route host ingress through the SystemState timestamped journal. Keep
        # only weak edges back to the system because the backend and native TX
        # callback can both outlive an individual frontend reference.
        self.uart.attach_native(cpu0_cs)
        system_ref = weakref.ref(self)
        cpu0_ref = weakref.ref(cpu0_cs)
        uart_ref = weakref.ref(self.uart)
        py_nic_ref = weakref.ref(self.nic)

        def _scheduled_uart_inject(
            data: bytes | str,
            _system_ref=system_ref,
            _uart_ref=uart_ref,
        ) -> None:
            system = _system_ref()
            if system is not None:
                system.schedule_uart_input(data)
                return
            uart = _uart_ref()
            if uart is not None:
                UART.inject_input(uart, data)

        self.uart.inject_input = _scheduled_uart_inject

        def _dual_inject(
            data: bytes,
            _system_ref=system_ref,
            _cs_ref=cpu0_ref,
            _nic_ref=py_nic_ref,
        ) -> bool:
            payload = bytes(data)
            system = _system_ref()
            if system is not None:
                cs = _cs_ref()
                if cs is None:
                    return False
                if not payload or len(payload) > NIC_MAX_FRAME:
                    # Preserve inject_frame()'s compatibility contract while
                    # letting the authoritative native NIC latch the error.
                    return bool(cs.nic_inject_frame(payload))
                rx_count = cs.nic_get_rx_count()
                _, staged = system._schedule_external_event(
                    ExternalEventKind.NIC_RX,
                    at_cycle=None,
                    payload=payload,
                )
                if staged:
                    return True
                return cs.nic_get_rx_count() != rx_count
            py_nic = _nic_ref()
            if py_nic is None:
                return False
            return NetworkDevice.inject_frame(py_nic, payload)

        self.nic.inject_frame = _dual_inject

        geometry_ref = weakref.ref(self.uart_geom)

        def _scheduled_geometry_update(
            cols: int,
            rows: int,
            _system_ref=system_ref,
            _geometry_ref=geometry_ref,
        ) -> None:
            system = _system_ref()
            if system is not None:
                system.schedule_terminal_resize(cols, rows)
                return
            geometry = _geometry_ref()
            if geometry is not None:
                CppUartGeomProxy.host_set_size(
                    geometry,
                    cols,
                    rows,
                )

        # Existing display/session callers keep their façade while every
        # spontaneous host geometry update enters the same timestamped log.
        self.uart_geom.host_set_size = _scheduled_geometry_update

        if nic_backend is not None:
            def _native_rx(frame: bytes, _inject=_dual_inject) -> None:
                _inject(frame)

            self.nic.set_host_rx_handler(_native_rx)
        else:
            # Legacy UDP passthrough must use the same journal-aware route.
            self.nic.set_host_rx_handler(_dual_inject)

        # No transport can deliver a frame before every RX source resolves to
        # the journal-aware façade above.
        self.nic.start()
        cpu0_cs.nic_set_link_up(self.nic.link_up)

        # Patch CPU memory access functions to intercept MMIO (per core)
        for cpu in self.cores:
            self._patch_cpu_mem(cpu)

        # Wire CSR IPI stubs into real mailbox operations (per core)
        for cpu in self.cores:
            self._wire_ipi_csrs(cpu)

        # Default UART TX handler: buffer (CLI will override)
        self._tx_log: list[int] = []
        self.uart.on_tx = lambda b: self._tx_log.append(b)

        # Give UART a reference to CPU memory for TX ring buffer drain
        self.uart._cpu_mem = self.cpu.mem

        # Wire cluster_en callback — writes to SysInfo 0x18 actually
        # enable/disable clusters
        if self.clusters:
            original_sysinfo_write = self.sysinfo.write8
            clusters = self.clusters

            def cluster_en_write(offset: int, val: int):
                with self._scheduler_lock:
                    original_sysinfo_write(offset, val)
                    if 0x18 <= offset < 0x20:
                        # Apply one coherent byte-updated mask. The lock is
                        # reentrant for an MMIO write from the active guest
                        # instruction and serializes host writes with batches.
                        en_mask = self.sysinfo.cluster_en
                        for i, cl in enumerate(clusters):
                            cl.set_enabled(bool(en_mask & (1 << i)))

            self.sysinfo.write8 = cluster_en_write

        # Boot state
        self._booted = False

    def _require_complete_snapshot_support(self) -> None:
        """Fail before a partial snapshot can observe or mutate this machine."""
        with self._scheduler_lock:
            self._reject_native_batch_reentry()
            raise RuntimeError(
                self._COMPLETE_SNAPSHOT_UNSUPPORTED_REASON
            )

    # -----------------------------------------------------------------
    #  Timestamped host ingress
    # -----------------------------------------------------------------

    def _schedule_external_event(
        self,
        kind,
        *,
        at_cycle: Optional[int],
        payload: bytes = b"",
        argument0: int = 0,
        argument1: int = 0,
    ) -> tuple[int, bool]:
        """Journal one host event, returning ``(sequence, staged)``."""
        if at_cycle is None:
            staged = self._native_system._try_stage_external_event(
                kind,
                payload,
                argument0,
                argument1,
            )
            if staged is not None:
                return int(staged), True

        with self._scheduler_lock:
            if at_cycle is None:
                # Execution may have opened staging after the optimistic
                # attempt but before this thread acquired the Python lock.
                staged = self._native_system._try_stage_external_event(
                    kind,
                    payload,
                    argument0,
                    argument1,
                )
                if staged is not None:
                    return int(staged), True
            current_cycle = int(self._native_system.system_cycles)
            event_cycle = current_cycle if at_cycle is None else at_cycle
            sequence = self._native_system._schedule_external_event(
                kind,
                event_cycle,
                payload,
                argument0,
                argument1,
            )
            if event_cycle == current_cycle:
                self._native_system._apply_due_external_events()
            return int(sequence), False

    def _begin_external_event_staging_locked(self) -> None:
        """Open the live-ingress gate for one positive execution call."""
        self._native_system._begin_external_event_staging()

    def _close_external_event_staging_locked(self) -> int:
        """Publish live host ingress at the completed execution boundary."""
        return int(
            self._native_system._close_external_event_staging()
        )

    def schedule_uart_input(
        self,
        data: bytes | bytearray | memoryview | str,
        *,
        at_cycle: Optional[int] = None,
    ) -> int:
        """Schedule host-to-guest UART bytes at an exact system cycle."""
        payload = data.encode("utf-8") if isinstance(data, str) else bytes(data)
        sequence, _ = self._schedule_external_event(
            ExternalEventKind.UART_RX,
            at_cycle=at_cycle,
            payload=payload,
        )
        return sequence

    def schedule_nic_frame(
        self,
        data: bytes | bytearray | memoryview,
        *,
        at_cycle: Optional[int] = None,
    ) -> int:
        """Schedule one raw Ethernet frame at an exact system cycle."""
        sequence, _ = self._schedule_external_event(
            ExternalEventKind.NIC_RX,
            at_cycle=at_cycle,
            payload=bytes(data),
        )
        return sequence

    def schedule_terminal_resize(
        self,
        cols: int,
        rows: int,
        *,
        at_cycle: Optional[int] = None,
    ) -> int:
        """Schedule host terminal geometry at an exact system cycle."""
        sequence, _ = self._schedule_external_event(
            ExternalEventKind.UART_GEOMETRY,
            at_cycle=at_cycle,
            argument0=cols,
            argument1=rows,
        )
        return sequence

    def schedule_terminal_resize_response(
        self,
        generation: int,
        *,
        accepted: bool,
        cols: int = 0,
        rows: int = 0,
        at_cycle: Optional[int] = None,
    ) -> int:
        """Journal the host outcome of one firmware resize request."""
        if not 0 <= generation < (1 << 64):
            raise ValueError("terminal resize generation must fit uint64")
        if accepted and (
            not 0 <= cols < (1 << 16)
            or not 0 <= rows < (1 << 16)
        ):
            raise ValueError("terminal resize dimensions must fit uint16")
        packed_dimensions = (
            (int(rows) << 16) | int(cols)
            if accepted
            else 0
        )
        kind = (
            ExternalEventKind.UART_GEOMETRY_ACCEPT
            if accepted
            else ExternalEventKind.UART_GEOMETRY_DENY
        )
        sequence, _ = self._schedule_external_event(
            kind,
            at_cycle=at_cycle,
            argument0=generation,
            argument1=packed_dimensions,
        )
        return sequence

    # -----------------------------------------------------------------
    #  IPI wiring
    # -----------------------------------------------------------------

    def _wire_ipi_csrs(self, cpu: Megapad64):
        """Wire Python-oracle CSR IPI methods to the shared native router."""
        core_id = cpu.core_id
        mailbox = self.mailbox

        def ipi_send(target: int):
            mailbox.send_ipi(
                core_id,
                target,
                publish_payload=False,
            )

        def ipi_ack(from_core: int):
            mailbox.acknowledge_ipi(core_id, from_core)

        def get_ipi_pending():
            return self._native_system.ipi_pending_mask(core_id)

        cpu._ipi_send = ipi_send
        cpu._ipi_ack = ipi_ack
        cpu._ipi_pending_getter = get_ipi_pending
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
        hbw_end = self.hbw_end
        ext_mem = self._ext_mem
        ext_mem_size = self.ext_mem_size
        ext_mem_base = self.ext_mem_base
        ext_mem_end = self.ext_mem_end
        vram_mem = self._vram_mem
        vram_size = self.vram_size
        vram_base = self.vram_base
        vram_end = self.vram_end

        # Scratchpad interception for micro-cores
        cluster = getattr(cpu, '_cluster', None)
        native_state = getattr(cpu, '_cs', None)

        # MPU / privilege enforcement removed (user mode stripped).

        def patched_read8(addr: int) -> int:
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                offset = addr - MMIO_START
                try:
                    # Native full-core instructions and their Python
                    # continuations must observe the same SystemState-owned
                    # SoC singleton. Micro-cores retain the Python bus path
                    # until heterogeneous scheduling moves in Phase 2.
                    if native_state is not None:
                        native_value = (
                            native_state._native_singleton_read8(offset)
                        )
                        if native_value >= 0:
                            return native_value
                    return bus.read8(offset, requester_id=core_id)
                except BusError:
                    cpu.trap_addr = addr
                    raise TrapError(IVEC_BUS_FAULT,
                                    f"Bus timeout @ {addr:#018x}")
            if cluster and (addr >> 32) == 0xFFFF_FE00:
                return cluster.spad_read8(addr & 0xFFFF_FFFF)
            if vram_size > 0 and vram_base <= addr < vram_end:
                return vram_mem[addr - vram_base]
            if hbw_size > 0 and HBW_BASE <= addr < hbw_end:
                return hbw_mem[addr - HBW_BASE]
            if ext_mem_size > 0 and ext_mem_base <= addr < ext_mem_end:
                return ext_mem[addr - ext_mem_base]
            return original_read8(addr)

        def patched_write8(addr: int, val: int):
            addr = u64(addr)
            if MMIO_START <= addr < MMIO_END:
                offset = addr - MMIO_START
                try:
                    if native_state is not None:
                        if native_state._native_singleton_write8(
                            offset,
                            val & 0xFF,
                        ):
                            return
                    bus.write8(
                        offset,
                        val,
                        requester_id=core_id,
                    )
                except BusError:
                    cpu.trap_addr = addr
                    raise TrapError(IVEC_BUS_FAULT,
                                    f"Bus timeout @ {addr:#018x}")
                return
            if cluster and (addr >> 32) == 0xFFFF_FE00:
                cluster.spad_write8(addr & 0xFFFF_FFFF, val)
                return
            if vram_size > 0 and vram_base <= addr < vram_end:
                vram_mem[addr - vram_base] = val & 0xFF
                return
            if hbw_size > 0 and HBW_BASE <= addr < hbw_end:
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

    def _any_nic_rx(self) -> bool:
        """Check if C++ NIC has RX data available."""
        return self.cores[0]._cs.nic_has_rx()

    def _drain_native_uart_output(self) -> bytes:
        """Deliver pending native UART output to Python observers in one batch."""
        return self.uart._drain_native_output()

    def _raw_mem_read(self, addr: int) -> int:
        addr = u64(addr)
        if self.vram_size > 0 and self.vram_base <= addr < self.vram_end:
            return self._vram_mem[addr - self.vram_base]
        if self.hbw_size > 0 and HBW_BASE <= addr < self.hbw_end:
            return self._hbw_mem[addr - HBW_BASE]
        if self.ext_mem_size > 0 and self.ext_mem_base <= addr < self.ext_mem_end:
            return self._ext_mem[addr - self.ext_mem_base]
        return self._shared_mem[addr % self.ram_size]

    def _raw_mem_span_valid(self, addr: int, count: int) -> bool:
        """Require one complete DMA span inside one physical memory window."""
        addr = u64(addr)
        if count <= 0:
            return False
        end = addr + count
        if end > (1 << 64):
            return False
        regions = [
            (0, self.ram_size),
        ]
        if self.hbw_size > 0:
            regions.append((HBW_BASE, self.hbw_end))
        if self.ext_mem_size > 0:
            regions.append((self.ext_mem_base, self.ext_mem_end))
        if self.vram_size > 0:
            regions.append((self.vram_base, self.vram_end))
        return sum(base <= addr and end <= limit
                   for base, limit in regions) == 1

    def _raw_mem_write(self, addr: int, val: int):
        addr = u64(addr)
        if self.vram_size > 0 and self.vram_base <= addr < self.vram_end:
            self._vram_mem[addr - self.vram_base] = val & 0xFF
        elif self.hbw_size > 0 and HBW_BASE <= addr < self.hbw_end:
            self._hbw_mem[addr - HBW_BASE] = val & 0xFF
        elif self.ext_mem_size > 0 and self.ext_mem_base <= addr < self.ext_mem_end:
            self._ext_mem[addr - self.ext_mem_base] = val & 0xFF
        else:
            self._shared_mem[addr % self.ram_size] = val & 0xFF

    @property
    def _scheduler_cursor(self) -> int:
        """Compatibility view of the native scheduler's next core."""
        return int(self._native_system.scheduler_cursor)

    @_scheduler_cursor.setter
    def _scheduler_cursor(self, value: int) -> None:
        self._native_system.scheduler_cursor = int(value)

    # -----------------------------------------------------------------
    #  Loading
    # -----------------------------------------------------------------

    def load_binary(self, addr: int, data: bytes | bytearray):
        """Load raw bytes with slice copies across mapped memory regions.

        Ordinary addresses retain the emulator's historical Bank 0 wrapping
        semantics. HBW, external memory, and VRAM use their physical windows.
        """
        payload = memoryview(data).cast('B')
        pos = 0
        target = u64(addr)
        total = len(payload)

        while pos < total:
            if (self.vram_size > 0
                    and self.vram_base <= target < self.vram_end):
                offset = target - self.vram_base
                count = min(total - pos, self.vram_end - target)
                self._vram_mem[offset:offset + count] = payload[pos:pos + count]
            elif (self.hbw_size > 0
                    and HBW_BASE <= target < self.hbw_end):
                offset = target - HBW_BASE
                count = min(total - pos, self.hbw_size - offset)
                self._hbw_mem[offset:offset + count] = payload[pos:pos + count]
            elif (self.ext_mem_size > 0
                    and self.ext_mem_base <= target < self.ext_mem_end):
                offset = target - self.ext_mem_base
                count = min(total - pos, self.ext_mem_end - target)
                self._ext_mem[offset:offset + count] = payload[pos:pos + count]
            else:
                offset = target % self.ram_size
                count = min(total - pos, self.ram_size - offset)
                self._shared_mem[offset:offset + count] = payload[pos:pos + count]

            pos += count
            target = u64(target + count)

    def load_binary_file(self, path: str, addr: int = 0):
        """Load a binary file into RAM."""
        with open(path, "rb") as f:
            data = f.read()
        self.load_binary(addr, data)

    # -----------------------------------------------------------------
    #  Boot
    # -----------------------------------------------------------------

    def boot(
        self,
        entry: int = BOOT_VECTOR,
        *,
        discard_uart_output: bool = False,
    ):
        """Warm-boot processor and cluster execution state.

        Full cores start at the entry point (matching FPGA behaviour).
        Core 0 gets SP at top of RAM; secondary cores get per-core stacks.
        Micro-cores are reset and then either released or held according to
        the persistent SysInfo CLUSTER_EN mask. Authoritative system time,
        shared ingress devices, and their event journal survive. A session
        frontend may discard output that was not yet presented; this does not
        alter guest-visible UART input or its provenance.
        """
        with self._scheduler_lock:
            self._reject_native_batch_reentry()
            if discard_uart_output:
                self.cpu._cs.uart_drain_tx()
                self.uart._tx_ring_base = 0
                self.uart.tx_buffer.clear()
            self._boot_locked(entry)

    def _boot_locked(self, entry: int) -> None:
        # Cancel every native target and cached DMA beat atomically before a
        # device can discard the controller state that would consume it.
        self._native_system._reset_cycle_execution_and_main_bus()
        self.storage.reset()
        self.audio.reset()
        # System time and shared input devices intentionally survive this CPU
        # reboot, so retain their external-event provenance and future events.
        # A later full-SoC reset must clear devices and the journal together.
        self._scheduler_cursor = 0
        cluster_enable_mask = self.sysinfo.cluster_en
        for cluster in self.clusters:
            cluster.enabled = False
            cluster.reset_shared_resources()
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

        # CLUSTER_EN is a persistent board-control register across this warm
        # CPU boot. Reapply it only after every reduced core has reached the
        # reset vector so register state and execution gating cannot diverge.
        for index, cluster in enumerate(self.clusters):
            if cluster_enable_mask & (1 << index):
                cluster.set_enabled(True)

        # The shared NIC survives a warm CPU boot. Re-register any held DMA
        # beat on the freshly reset fabric so unbounded execution cannot
        # orphan it and the next cycle-bounded call resumes the same byte.
        self._native_system._adopt_native_nic_cycle_dma()
        self._booted = True

    # -----------------------------------------------------------------
    #  Execution
    # -----------------------------------------------------------------

    def _next_external_event_cycle(self) -> Optional[int]:
        """Return the earliest unapplied host-input cycle, if one exists."""
        cycle = self._native_system.external_event_next_cycle
        return None if cycle is None else int(cycle)

    def _require_cycle_unbounded_execution(self) -> None:
        """Reject unsafe post-hoc execution while a timed event is active."""
        if self._next_external_event_cycle() is not None:
            raise RuntimeError(
                "pending external events require cycle-bounded native execution"
            )
        _cycles, deadline, _sources = self._native_system.event_horizon()
        if deadline is not None:
            raise RuntimeError(
                "active event horizons require cycle-bounded native execution"
            )
        if self._native_system.main_bus_timeout_cycle is not None:
            raise RuntimeError(
                "active main-bus grants require cycle-bounded native execution"
            )
        if self._native_system.cycle_execution_pending:
            raise RuntimeError(
                "suspended cycle execution requires cycle-bounded native "
                "execution"
            )

    def _reject_native_batch_reentry(self) -> None:
        """Keep every system execution API outside an active native batch."""
        if self._native_system.native_batch_active:
            raise RuntimeError("native system batch is already active")

    def advance_system_cycles(self, cycles: int) -> None:
        """Advance authoritative system time and every cycle-driven device."""
        with self._scheduler_lock:
            self._advance_system_cycles_locked(cycles)

    def _advance_system_cycles_locked(self, cycles: int) -> None:
        """Advance time while the scheduler transaction lock is held."""
        if cycles < 0:
            raise ValueError("system cycles cannot advance by a negative value")
        if cycles and not self._native_system.native_batch_active:
            current = int(self._native_system.system_cycles)
            external_cycle = self._next_external_event_cycle()
            if (
                external_cycle is not None
                and external_cycle <= current + cycles
            ):
                raise RuntimeError(
                    "system time cannot cross a pending external event"
                )
        if (
            self._native_system.cycle_execution_pending
            and not self._native_system.native_batch_active
        ):
            raise RuntimeError(
                "system time cannot advance while cycle execution is "
                "suspended"
            )
        current, deadline, _sources = self._native_system.event_horizon()
        if deadline is not None and cycles > deadline - current:
            raise ValueError(
                "system clock advance cannot cross the event horizon"
            )
        if cycles > (1 << 64) - 1 - current:
            raise OverflowError("system cycle counter overflow")
        bus_timeout = self._native_system.main_bus_timeout_cycle
        if bus_timeout is not None and cycles > bus_timeout - current:
            raise ValueError(
                "system clock cannot cross the active main bus timeout"
            )

        self._native_system.advance_system_cycles(cycles)
        for device in self.bus.devices:
            if device not in (self.timer, self.fb, self.rtc):
                device.tick(cycles)

    def _deliver_pending_interrupts(self) -> None:
        """Deliver timer and IPI lines at a completed execution boundary."""
        if self.timer.irq_pending:
            for cpu in self.cores:
                if cpu.flag_i and not cpu.halted:
                    cpu._trap(IVEC_TIMER)

        for cpu in self.cores:
            if cpu.irq_ipi and cpu.flag_i and not cpu.halted and not cpu.idle:
                cpu._trap(IVEC_IPI)

    def _native_full_core_batch_eligible(self) -> bool:
        """Whether strict-cycle execution can use only canonical full cores."""
        if self.num_clusters != 0:
            return False
        for cpu in self.cores[:self.num_full_cores]:
            cpu_vars = vars(cpu)
            if (
                "run_steps_stats" in cpu_vars
                or "_run_steps_stats_in_memory_scope" in cpu_vars
            ):
                return False
            cpu_type = type(cpu)
            if (
                getattr(cpu_type, "run_steps_stats", None)
                is not _CANONICAL_RUN_STEPS_STATS
                or getattr(
                    cpu_type,
                    "_run_steps_stats_in_memory_scope",
                    None,
                )
                is not _CANONICAL_RUN_STEPS_STATS_IN_SCOPE
            ):
                return False
        return True

    def _native_system_batch_eligible(self) -> bool:
        """Whether every advertised core can use the native system scheduler."""
        for cpu in self.cores:
            cpu_vars = vars(cpu)
            if (
                "run_steps_stats" in cpu_vars
                or "_run_steps_stats_in_memory_scope" in cpu_vars
            ):
                return False
            cpu_type = type(cpu)
            if (
                getattr(cpu_type, "run_steps_stats", None)
                is not _CANONICAL_RUN_STEPS_STATS
                or getattr(
                    cpu_type,
                    "_run_steps_stats_in_memory_scope",
                    None,
                )
                is not _CANONICAL_RUN_STEPS_STATS_IN_SCOPE
            ):
                return False
        return True

    def _prepare_native_full_core_batch(self) -> None:
        """Apply compatibility wake checks for every native execution core."""
        for cpu in self.cores:
            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                cpu.idle = False
            if cpu.idle and cpu.core_id == 0:
                if self.uart.has_rx_data:
                    cpu.idle = False
                elif self.timer.irq_pending and cpu.flag_i:
                    cpu.idle = False
                elif self._any_nic_rx():
                    cpu.idle = False

    @staticmethod
    def _prepare_native_cycle_batch() -> None:
        """Leave cycle-mode eligibility and wake transitions to native code."""

    @staticmethod
    def _settle_native_batch_trap_error(
        cpu,
        error: TrapError,
        *,
        prefix_steps: int,
        prefix_cycles: int,
        annotate: bool,
    ) -> tuple[int, int, bool]:
        """Reproduce _run_core_batch's tested TrapError accounting."""
        if annotate:
            error.steps_executed = prefix_steps + 1
            error.native_prefix_steps = prefix_steps
            error.native_prefix_cycles = prefix_cycles
        if cpu.ivt_base != 0:
            with _cpu_memory_use(cpu):
                cpu._trap(error.ivec_id)
        return (
            int(getattr(error, "steps_executed", 1)),
            int(getattr(error, "native_prefix_cycles", 0)),
            True,
        )

    def _settle_native_core_continuation(
        self,
        core_index: int,
        stop_reason: int,
        trap_id: int,
        prefix_steps: int,
        prefix_cycles: int,
    ) -> tuple[int, int, bool]:
        """Settle a raw native boundary and return whole-invocation progress."""
        cpu = self.cores[core_index]
        try:
            if stop_reason in (3, 4):
                continuation_cycles = cpu._step_python_fallback()
            elif stop_reason == 5:
                continuation_cycles = cpu._finish_trap(trap_id)
            elif stop_reason == 6:
                continuation_cycles = cpu._finish_reset()
            else:
                raise RuntimeError(
                    f"invalid native continuation reason {stop_reason}"
                )
        except TrapError as error:
            return self._settle_native_batch_trap_error(
                cpu,
                error,
                prefix_steps=prefix_steps,
                prefix_cycles=prefix_cycles,
                annotate=stop_reason in (3, 4, 5),
            )
        return (
            prefix_steps + 1,
            prefix_cycles + continuation_cycles,
            False,
        )

    def _settle_native_core_dispatch_error(
        self,
        core_index: int,
        error: BaseException,
    ):
        """Handle callback exceptions interpreted by the legacy wrapper."""
        cpu = self.cores[core_index]
        if isinstance(error, TrapError):
            return self._settle_native_batch_trap_error(
                cpu,
                error,
                prefix_steps=0,
                prefix_cycles=0,
                annotate=False,
            )
        if isinstance(error, RuntimeError):
            message = str(error)
            if message == "HALT":
                return 0, 0, True
            if message.startswith("TRAP:"):
                try:
                    cycles = cpu._handle_trap(message)
                except TrapError as trap_error:
                    return self._settle_native_batch_trap_error(
                        cpu,
                        trap_error,
                        prefix_steps=0,
                        prefix_cycles=0,
                        annotate=False,
                    )
                return 1, cycles, False
        return None

    def _settle_native_system_round(
        self,
        cycles: int,
        advance_clock: bool,
        drain_uart: bool,
        deliver_interrupts: bool,
    ) -> None:
        """Settle one completed native scheduler round in legacy order."""
        if advance_clock:
            self.bus.tick(cycles)
        if drain_uart:
            self._drain_native_uart_output()
        if deliver_interrupts:
            self._deliver_pending_interrupts()

    def _run_native_full_core_batch(self, n: int) -> SystemRunStats:
        """Adapt the all-core SystemState scheduler to the public result type.

        The method name remains as a compatibility seam for tests and callers
        introduced during Phase 1; callback and result topology now includes
        full and reduced cores in global core-ID order.
        """
        callback_sets = [
            (
                cpu._mmio_read8,
                cpu._mmio_write8,
                cpu._do_output,
                getattr(cpu, '_csr_read_override', None),
            )
            for cpu in self.cores
        ]
        result = self._native_system.run_full_core_batch(
            n,
            callback_sets,
            self._prepare_native_full_core_batch,
            self._settle_native_core_continuation,
            self._settle_native_core_dispatch_error,
            self._settle_native_system_round,
            1000,
        )
        return SystemRunStats(
            int(result.instructions_executed),
            int(result.system_cycles_advanced),
            tuple(int(value) for value in result.per_core_instructions),
            tuple(int(value) for value in result.per_core_cycles),
            tuple(int(value) for value in result.per_core_dispatches),
            tuple(
                tuple(int(value) for value in reasons)
                for reasons in result.per_core_stop_reasons
            ),
            True,
            int(result.rounds),
            int(result.continuations),
        )

    def _run_native_full_core_cycle_batch(
        self,
        max_system_cycles: int,
        max_instructions: int,
    ) -> SystemRunStats:
        """Run full cores to an exact virtual-cycle or event boundary."""
        callback_sets = [
            (
                cpu._mmio_read8,
                cpu._mmio_write8,
                cpu._do_output,
                getattr(cpu, "_csr_read_override", None),
            )
            for cpu in self.cores[:self.num_full_cores]
        ]
        dma_callback_sets = self._cycle_dma_callback_sets()
        previous_strict_submission = (
            self.storage._strict_cycle_submission
        )
        self.storage._strict_cycle_submission = True
        try:
            result = self._native_system.run_full_core_cycle_batch(
                max_system_cycles,
                callback_sets,
                dma_callback_sets,
                self._prepare_native_cycle_batch,
                self._settle_native_core_continuation,
                self._settle_native_system_round,
                max_instructions,
            )
        finally:
            self.storage._strict_cycle_submission = (
                previous_strict_submission
            )
        stop_reason = str(result.system_stop_reason)
        if "." in stop_reason:
            stop_reason = stop_reason.rsplit(".", 1)[-1]
        stop_reason = stop_reason.lower()
        return SystemRunStats(
            instructions_executed=int(result.instructions_executed),
            system_cycles_advanced=int(result.system_cycles_advanced),
            per_core_instructions=tuple(
                int(value) for value in result.per_core_instructions
            ),
            per_core_cycles=tuple(
                int(value) for value in result.per_core_cycles
            ),
            per_core_dispatches=tuple(
                int(value) for value in result.per_core_dispatches
            ),
            per_core_stop_reasons=tuple(
                tuple(int(value) for value in reasons)
                for reasons in result.per_core_stop_reasons
            ),
            native_scheduler=True,
            native_rounds=int(result.rounds),
            native_continuations=int(result.continuations),
            system_stop_reason=stop_reason,
            stop_cycle=int(result.stop_cycle),
            event_source_mask=int(result.event_source_mask),
            per_core_interrupts=tuple(
                int(value) for value in result.per_core_interrupts
            ),
            interrupts_delivered=int(result.interrupts_delivered),
            external_events_applied=int(result.external_events_applied),
            pending_interrupt_core=int(result.pending_interrupt_core),
            pending_interrupt_vector=int(result.pending_interrupt_vector),
        )

    def _inspect_storage_dma(self, _current_cycle: int) -> DmaEndpointView:
        active, pending = self.storage.cycle_dma_view()
        beat = None
        if pending is not None:
            beat = DmaBeat(
                pending.token,
                (
                    BusOperation.WRITE
                    if pending.write
                    else BusOperation.READ
                ),
                pending.address,
                pending.write_data,
            )
        return DmaEndpointView(active, beat)

    def _complete_storage_dma(self, token: int, result) -> None:
        read_value = (
            None
            if result.read_value is None
            else int(result.read_value) & 0xFF
        )
        accepted = self.storage.cycle_dma_complete(
            int(token),
            read_value=read_value,
            faulted=result.fault != BusFault.NONE,
            target_effects_committed=bool(
                result.target_effects_committed
            ),
        )
        if not accepted:
            raise RuntimeError(
                "storage rejected its strict-cycle DMA completion"
            )

    def _cycle_dma_callback_sets(self):
        """Bind the native NIC and Python storage DMA endpoints."""
        return [
            (None, None),
            (
                self._inspect_storage_dma,
                self._complete_storage_dma,
            ),
        ]

    def _run_core_batch(self, cpu, max_steps: int) -> tuple[int, int]:
        """Run one core under one logical operation and recover exact progress."""
        total_steps = 0
        total_cycles = 0
        while total_steps < max_steps:
            with _cpu_logical_memory_use(cpu):
                try:
                    stats = cpu.run_steps_stats(max_steps - total_steps)
                except TrapError as error:
                    if cpu.ivt_base != 0:
                        with _cpu_memory_use(cpu):
                            cpu._trap(error.ivec_id)
                    total_steps += getattr(error, "steps_executed", 1)
                    total_cycles += getattr(
                        error,
                        "native_prefix_cycles",
                        0,
                    )
                    break
                total_steps += stats.steps_executed
                total_cycles += stats.total_cycles
                if (
                    stats.stop_reason != 0
                    or stats.steps_executed == 0
                ):
                    break

        return total_steps, total_cycles

    def step(self) -> int:
        """Execute one instruction on each active core (round-robin).

        Returns total cycles consumed across all cores.
        """
        with self._scheduler_lock:
            self._reject_native_batch_reentry()
            self._require_cycle_unbounded_execution()
            self._begin_external_event_staging_locked()
            try:
                result = self._step_locked()
            finally:
                self._close_external_event_staging_locked()
            return result

    def _step_locked(self) -> int:
        """Execute one deterministic round under the scheduler lock."""
        self._require_cycle_unbounded_execution()
        total_cycles = 0
        elapsed_cycles = 0
        pending_error = None

        for cpu in self.cores:
            # Wake CPU from idle on IPI
            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                cpu.idle = False

            # Wake core 0 from idle on UART RX or timer IRQ or NIC RX
            if cpu.idle and cpu.core_id == 0:
                if self.uart.has_rx_data:
                    cpu.idle = False
                elif self.timer.irq_pending and cpu.flag_i:
                    cpu.idle = False
                elif self._any_nic_rx():
                    cpu.idle = False

            if cpu.halted or cpu.idle:
                continue

            with _cpu_logical_memory_use(cpu):
                try:
                    cycles = cpu.step()
                    total_cycles += cycles
                    elapsed_cycles = max(elapsed_cycles, cycles)
                except TrapError as e:
                    if cpu.ivt_base != 0:
                        with _cpu_memory_use(cpu):
                            cpu._trap(e.ivec_id)
                    else:
                        pending_error = e
                        break
                except Exception as error:
                    pending_error = error
                    break

        # Cores in one deterministic round share one elapsed system frontier.
        if elapsed_cycles > 0:
            self.bus.tick(elapsed_cycles)
        elif pending_error is None:
            self.bus.tick(1)
        self._drain_native_uart_output()

        if pending_error is not None:
            raise pending_error

        self._deliver_pending_interrupts()

        return max(total_cycles, 1)

    def run(self, max_steps: int = 1_000_000) -> int:
        """Run until all cores HALT, or max_steps."""
        self._reject_native_batch_reentry()
        self._require_cycle_unbounded_execution()
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
        """Compatibility adapter returning aggregate executed instructions."""
        return self.run_batch_stats(n).instructions_executed

    def run_batch_stats(self, n: int = 100_000) -> SystemRunStats:
        """Execute a deterministic one-worker batch with exact cycle totals."""
        with self._scheduler_lock:
            self._reject_native_batch_reentry()
            if n <= 0:
                return self._run_batch_stats_locked(n)
            self._require_cycle_unbounded_execution()
            self._begin_external_event_staging_locked()
            try:
                result = self._run_batch_stats_locked(n)
            finally:
                applied = self._close_external_event_staging_locked()
            if applied:
                result = replace(
                    result,
                    external_events_applied=(
                        result.external_events_applied + applied
                    ),
                )
            return result

    def run_cycle_batch(
        self,
        max_system_cycles: int,
        *,
        max_instructions: int = 100_000,
    ) -> SystemRunStats:
        """Run to an exact virtual-cycle, event, or instruction boundary.

        ``max_system_cycles`` is a relative authoritative-system-clock
        budget. ``max_instructions`` remains an aggregate retirement cap.
        A tied event horizon wins over the caller cycle limit and remains
        armed for the caller to process or reschedule. Timestamped UART, NIC,
        and geometry input is applied at its exact cycle. Eligible TIMER/IPI
        lines are accepted before the core's next fetch, with IPI priority and
        trap-frame traffic arbitrated on the equal round-robin main bus. A
        pending interrupt without an installed IVT returns
        ``unhandled_interrupt`` without changing that core.
        """
        if max_system_cycles < 0:
            raise ValueError("max_system_cycles cannot be negative")
        if max_system_cycles > (1 << 64) - 1:
            raise OverflowError("max_system_cycles exceeds uint64")
        if max_instructions < 0:
            raise ValueError("max_instructions cannot be negative")
        if max_instructions > (1 << 63) - 1:
            raise OverflowError("max_instructions exceeds int64")

        with self._scheduler_lock:
            if self.rtc.realtime:
                raise RuntimeError(
                    "cycle-bounded execution does not support a realtime RTC"
                )
            self._reject_native_batch_reentry()
            current_cycle = int(self._native_system.system_cycles)
            if max_system_cycles > (1 << 64) - 1 - current_cycle:
                raise OverflowError("cycle batch deadline overflow")
            if (
                self._native_system.main_bus_timeout_cycle is not None
                and not self._native_system.cycle_execution_pending
            ):
                raise RuntimeError(
                    "cycle-bounded execution cannot adopt an external "
                    "active main-bus grant"
                )
            if not self._native_full_core_batch_eligible():
                raise RuntimeError(
                    "cycle-bounded execution currently requires canonical "
                    "native full cores without micro-core clusters"
                )
            if max_system_cycles == 0 or max_instructions == 0:
                return self._run_native_full_core_cycle_batch(
                    max_system_cycles,
                    max_instructions,
                )
            self._begin_external_event_staging_locked()
            try:
                result = self._run_native_full_core_cycle_batch(
                    max_system_cycles,
                    max_instructions,
                )
            finally:
                applied = self._close_external_event_staging_locked()
            if applied:
                result = replace(
                    result,
                    external_events_applied=(
                        result.external_events_applied + applied
                    ),
                )
            return result

    def _run_batch_stats_locked(self, n: int) -> SystemRunStats:
        """Execute one system batch under the scheduler transaction lock."""
        if n <= 0:
            zeros = (0,) * self.num_cores
            return SystemRunStats(0, 0, zeros, zeros)

        self._reject_native_batch_reentry()
        self._require_cycle_unbounded_execution()
        if self._native_system_batch_eligible():
            return self._run_native_full_core_batch(n)

        # Compatibility path for heterogeneous topologies and deliberate
        # per-instance run_steps_stats overrides.
        clock_start = int(self._native_system.system_cycles)

        # --- wake checks (same as step()) ---
        for cpu in self.cores:
            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                cpu.idle = False
            if cpu.idle and cpu.core_id == 0:
                if self.uart.has_rx_data:
                    cpu.idle = False
                elif self.timer.irq_pending and cpu.flag_i:
                    cpu.idle = False
                elif self._any_nic_rx():
                    cpu.idle = False

        if self.all_halted or self.all_idle_or_halted:
            zeros = (0,) * self.num_cores
            return SystemRunStats(0, 0, zeros, zeros)

        per_core_instructions = [0] * self.num_cores
        per_core_cycles = [0] * self.num_cores
        active_indices = [
            index
            for index, cpu in enumerate(self.cores)
            if not cpu.halted and not cpu.idle
        ]

        # ---------- Native fast path (one active core) ----------
        if len(active_indices) == 1:
            core_index = active_indices[0]
            cpu = self.cores[core_index]
            steps, cycles = self._run_core_batch(cpu, n)
            per_core_instructions[core_index] = steps
            per_core_cycles[core_index] = cycles
            if steps > 0:
                self._scheduler_cursor = (
                    core_index + 1
                ) % self.num_cores
            self.bus.tick(cycles)
            self._drain_native_uart_output()
            self._deliver_pending_interrupts()
            return SystemRunStats(
                steps,
                int(self._native_system.system_cycles) - clock_start,
                tuple(per_core_instructions),
                tuple(per_core_cycles),
            )

        # ---------- Deterministic full-core rounds ----------
        if self.num_cores > 1:
            max_dispatch_steps = 1000
            total = 0
            remaining = n
            while remaining > 0 and not self.all_halted:
                if self.all_idle_or_halted:
                    break
                round_steps = 0
                round_cycles = 0
                round_error = None
                round_start = self._scheduler_cursor
                ordered_indices = (
                    (round_start + offset) % self.num_cores
                    for offset in range(self.num_cores)
                )
                for core_index in ordered_indices:
                    cpu = self.cores[core_index]
                    if cpu.halted or cpu.idle:
                        continue
                    dispatch_steps = min(
                        max_dispatch_steps,
                        remaining - round_steps,
                    )
                    if dispatch_steps <= 0:
                        break
                    try:
                        steps, cycles = self._run_core_batch(
                            cpu,
                            dispatch_steps,
                        )
                    except Exception as error:
                        round_error = error
                        break
                    per_core_instructions[core_index] += steps
                    per_core_cycles[core_index] += cycles
                    round_steps += steps
                    round_cycles = max(round_cycles, cycles)
                    if steps > 0:
                        self._scheduler_cursor = (
                            core_index + 1
                        ) % self.num_cores

                total += round_steps
                remaining -= round_steps
                self.bus.tick(round_cycles)
                if round_error is not None:
                    self._drain_native_uart_output()
                    raise round_error

                self._deliver_pending_interrupts()
                if round_steps == 0:
                    break
            self._drain_native_uart_output()
            return SystemRunStats(
                total,
                int(self._native_system.system_cycles) - clock_start,
                tuple(per_core_instructions),
                tuple(per_core_cycles),
            )

        # ---------- Single core fallback (shouldn't reach here) ----------
        zeros = (0,) * self.num_cores
        return SystemRunStats(0, 0, zeros, zeros)

    def run_until_halt(self, max_steps: int = 10_000_000) -> int:
        """Run until all cores HALT."""
        self._reject_native_batch_reentry()
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
                      f"RX buf={self.uart.rx_pending} "
                      f"ctrl={self.uart.read8(0x03):#04x}")
        lines.append(f"  Timer: count={self.timer.counter} "
                      f"compare={self.timer.compare} "
                      f"ctrl={self.timer.control:#04x} "
                      f"irq={'Y' if self.timer.irq_pending else 'N'}")
        lines.append(f"  Storage: {'present' if self.storage.status & 0x80 else 'none'} "
                      f"sectors={self.storage.total_sectors} "
                      f"image={self.storage.image_path or 'N/A'}")
        lines.append(f"  Audio: generation={self.audio.generation} "
                     f"frames={self.audio.last_frames} "
                     f"rate={self.audio.last_rate or self.audio.rate} "
                     f"channels={self.audio.last_channels or self.audio.channels} "
                     f"sink={'yes' if self.audio.capabilities & 0x02 else 'no'} "
                     f"error={self.audio.error}")
        lines.append(f"  NIC: {'link up' if self.nic.link_up else 'link down'} "
                      f"mac={self.nic.mac.hex(':')} "
                      f"tx={self.nic.tx_count} rx={self.cores[0]._cs.nic_get_rx_count()} "
                      f"rxq={self.cores[0]._cs.nic_rx_queue_size()} "
                      f"backend={self.nic.backend_name}")
        if self.num_cores > 1:
            lines.append(f"  Mailbox: cores={self.num_cores} "
                         f"pending={[self.mailbox.pending[i] for i in range(self.num_cores)]}")
            locked = [i for i in range(self.spinlock.num_locks) if self.spinlock.locked[i]]
            lines.append(f"  Spinlocks: locked={locked} "
                         f"owners={[self.spinlock.owner[i] for i in locked]}")
        return "\n".join(lines)
