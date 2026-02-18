"""
accel_wrapper.py — Drop-in accelerated Megapad-64 CPU
=====================================================

Tries to import the C++ ``_mp64_accel`` extension.  When available, wrap a
``CPUState`` in a thin Python class that presents the **exact same public
API** as :class:`megapad64.Megapad64`.  When the extension is not available
(e.g. wheels not built, PyPy, CI), silently fall back to the pure-Python
implementation.

Usage in system.py / tests::

    from accel_wrapper import Megapad64   # accelerated if available
    cpu = Megapad64()
    cpu.step()                            # → C++ fast path

The wrapper also re-exports everything from megapad64 so you can do::

    from accel_wrapper import HaltError, TrapError, u64, ...
"""

from __future__ import annotations

import importlib
import sys
from typing import Optional

# Always import the pure-Python module — we need its constants and
# exception classes regardless of whether the accelerator is available.
from megapad64 import (
    Megapad64 as _PyMegapad64,
    Megapad64Micro,
    Megapad64Error, TrapError, HaltError,
    u64, s64, sign_extend, zero_extend,
    MASK64, SIGN64,
    # Condition codes
    CC_AL, CC_EQ, CC_NE, CC_CS, CC_CC, CC_MI, CC_PL,
    CC_VS, CC_VC, CC_GT, CC_LE, CC_BQ, CC_BNQ, CC_SAT, CC_EF, CC_NV,
    # CSR addresses
    CSR_FLAGS, CSR_PSEL, CSR_XSEL, CSR_SPSEL, CSR_IVT_BASE,
    CSR_D, CSR_DF, CSR_Q, CSR_T, CSR_IE, CSR_PRIV,
    CSR_MPU_BASE, CSR_MPU_LIMIT,
    CSR_SB, CSR_SR, CSR_SC, CSR_SW,
    CSR_TMODE, CSR_TCTRL, CSR_TSRC0, CSR_TSRC1, CSR_TDST,
    CSR_ACC0, CSR_ACC1, CSR_ACC2, CSR_ACC3,
    CSR_COREID, CSR_NCORES, CSR_MBOX, CSR_IPIACK,
    CSR_IVEC_ID, CSR_TRAP_ADDR,
    CSR_TSTRIDE_R, CSR_TSTRIDE_C, CSR_TTILE_H, CSR_TTILE_W,
    CSR_MEGAPAD_SZ, CSR_CPUID,
    CSR_BIST_CMD, CSR_BIST_STATUS, CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA,
    CSR_TILE_SELFTEST, CSR_TILE_ST_DETAIL,
    CSR_PERF_CYCLES, CSR_PERF_STALLS, CSR_PERF_TILEOPS, CSR_PERF_EXTMEM,
    CSR_PERF_CTRL,
    CSR_BARRIER_ARRIVE, CSR_BARRIER_STATUS,
    CSR_ICACHE_CTRL, CSR_ICACHE_HITS, CSR_ICACHE_MISSES,
    # Cluster MPU CSRs
    CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
    # IVEC IDs
    IVEC_RESET, IVEC_NMI, IVEC_ILLEGAL_OP, IVEC_ALIGN_FAULT,
    IVEC_DIV_ZERO, IVEC_BUS_FAULT, IVEC_SW_TRAP, IVEC_TIMER, IVEC_IPI,
    IVEC_PRIV_FAULT,
    # EW codes
    EW_U8, EW_U16, EW_U32, EW_U64, EW_FP16, EW_BF16,
    # Micro-cluster constants
    NUM_FULL_CORES, NUM_CLUSTERS, MICRO_PER_CLUSTER, NUM_ALL_CORES,
    MICRO_ID_BASE, CLUSTER_SPAD_BYTES, CLUSTER_SPAD_ADDR, CPUID_MICRO,
    # FP helpers (needed for Python fallback in MEX FP)
    _fp16_to_float, _float_to_fp16, _bf16_to_float, _float_to_bf16,
    _fp_decode, _fp_encode, _fp_is_nan,
)

# ---------------------------------------------------------------------------
#  Try importing the C++ accelerator
# ---------------------------------------------------------------------------

_accel = None
try:
    import _mp64_accel
    _accel = _mp64_accel
except ImportError:
    pass

ACCEL_AVAILABLE: bool = _accel is not None
"""True when the C++ accelerator is loaded."""


# ---------------------------------------------------------------------------
#  Accelerated wrapper
# ---------------------------------------------------------------------------

if ACCEL_AVAILABLE:
    class Megapad64:
        """Accelerated Megapad-64 CPU (C++ inner loop, Python-compatible API)."""

        _accel_backend = True  # introspection flag

        def __init__(self, mem_size: int = 1 << 20, core_id: int = 0,
                     num_cores: int = 1):
            self.mem_size = mem_size
            self._mem = bytearray(mem_size)

            # C++ state
            self._cs = _accel.CPUState()
            self._cs.mem_size = mem_size
            self._cs.attach_mem(self._mem, mem_size)
            self._cs.core_id = core_id
            self._cs.num_cores = num_cores
            self._cs.psel = 3
            self._cs.xsel = 2
            self._cs.spsel = 15
            self._cs.ext_modifier = -1

            # Fields that only exist on the Python side
            self.irq_ipi: bool = False
            self.on_output: Optional[callable] = None
            self.on_halt: Optional[callable] = None

            # Keep a pure-Python fallback for MEX FP operations
            self._py_fallback: Optional[_PyMegapad64] = None

        # ── Memory property — auto-syncs C++ pointer on set ──

        @property
        def mem(self):
            return self._mem

        @mem.setter
        def mem(self, value):
            self._mem = value
            sz = len(value)
            self._cs.mem_size = sz
            self._cs.attach_mem(self._mem, sz)

        def attach_hbw(self, buf: bytearray, base: int, size: int):
            """Attach HBW math RAM buffer to C++ state."""
            self._hbw_buf = buf  # prevent GC
            self._cs.attach_hbw_mem(buf, base, size)

        def attach_ext_mem(self, buf: bytearray, base: int, size: int):
            """Attach external memory (HyperRAM/SDRAM) buffer to C++ state."""
            self._ext_mem_buf = buf  # prevent GC
            self._cs.attach_ext_mem(buf, base, size)

        # ── Register access ──────────────────────────────────

        @property
        def regs(self):
            """Return a list-like proxy to the 16 GPRs."""
            return _RegProxy(self._cs)

        @regs.setter
        def regs(self, value):
            for i, v in enumerate(value[:16]):
                self._cs.set_reg(i, u64(v))

        @property
        def pc(self) -> int:
            return self._cs.get_reg(self._cs.psel)

        @pc.setter
        def pc(self, value: int):
            self._cs.set_reg(self._cs.psel, u64(value))

        @property
        def rx(self) -> int:
            return self._cs.get_reg(self._cs.xsel)

        @rx.setter
        def rx(self, value: int):
            self._cs.set_reg(self._cs.xsel, u64(value))

        @property
        def sp(self) -> int:
            return self._cs.get_reg(self._cs.spsel)

        @sp.setter
        def sp(self, value: int):
            self._cs.set_reg(self._cs.spsel, u64(value))

        # ── Scalar state — delegate to CPUState ──────────────

        # Create forwarding properties for all simple scalar attributes
        for _attr in (
            'psel', 'xsel', 'spsel',
            'flag_z', 'flag_c', 'flag_n', 'flag_v',
            'flag_p', 'flag_g', 'flag_i', 'flag_s',
            'd_reg', 'q_out', 't_reg',
            'sb', 'sr', 'sc', 'sw',
            'tmode', 'tctrl', 'tsrc0', 'tsrc1', 'tdst',
            'ivt_base', 'ivec_id', 'trap_addr',
            'ef_flags', 'halted', 'idle', 'cycle_count',
            'tstride_r', 'ttile_h', 'ttile_w',
            'perf_enable', 'perf_cycles', 'perf_stalls',
            'perf_tileops', 'perf_extmem',
            'bist_status', 'bist_fail_addr', 'bist_fail_data',
            'tile_selftest', 'tile_st_detail',
            'icache_enabled', 'icache_hits', 'icache_misses',
            'priv_level',
            'mpu_base', 'mpu_limit',
            'ext_modifier',
            'core_id', 'num_cores',
        ):
            exec(f"""
@property
def {_attr}(self):
    return self._cs.{_attr}
@{_attr}.setter
def {_attr}(self, v):
    self._cs.{_attr} = v
""".strip(), {'property': property}, locals())
            locals()[_attr] = locals()[_attr]  # capture the property

        # Handle _ext_modifier as alias for ext_modifier (Python API uses leading _)
        @property
        def _ext_modifier(self):
            return self._cs.ext_modifier

        @_ext_modifier.setter
        def _ext_modifier(self, v):
            self._cs.ext_modifier = v

        # tstride_c only exists in Python (reserved in C++)
        tstride_c: int = 0

        # ── Accumulators ─────────────────────────────────────

        @property
        def acc(self):
            return _AccProxy(self._cs)

        @acc.setter
        def acc(self, value):
            for i, v in enumerate(value[:4]):
                self._cs.set_acc(i, u64(v))

        # ── I/O ports ────────────────────────────────────────

        @property
        def port_out(self):
            return _PortOutProxy(self._cs)

        @port_out.setter
        def port_out(self, value):
            # Port out is read-only from Python perspective; ignore
            pass

        @property
        def port_in(self):
            return _PortInProxy(self._cs)

        @port_in.setter
        def port_in(self, value):
            for i, v in enumerate(value[:8]):
                self._cs.set_port_in(i, v)

        # ── Flags ────────────────────────────────────────────

        def flags_pack(self) -> int:
            return self._cs.flags_pack()

        def flags_unpack(self, val: int):
            self._cs.flags_unpack(val & 0xFF)

        # ── Memory access ────────────────────────────────────
        # These are the methods that system.py monkey-patches for MMIO.
        # They start as direct C++ buffer access (fast path) and get
        # replaced by patched_read8/write8 closures in system.py.

        def mem_read8(self, addr: int) -> int:
            return self.mem[u64(addr) % self.mem_size]

        def mem_write8(self, addr: int, val: int):
            self.mem[u64(addr) % self.mem_size] = val & 0xFF

        def mem_read16(self, addr: int) -> int:
            a = u64(addr) % self.mem_size
            return self.mem[a] | (self.mem[(a+1) % self.mem_size] << 8)

        def mem_write16(self, addr: int, val: int):
            a = u64(addr) % self.mem_size
            self.mem[a] = val & 0xFF
            self.mem[(a+1) % self.mem_size] = (val >> 8) & 0xFF

        def mem_read32(self, addr: int) -> int:
            a = u64(addr) % self.mem_size
            v = 0
            for i in range(4):
                v |= self.mem[(a+i) % self.mem_size] << (8*i)
            return v

        def mem_write32(self, addr: int, val: int):
            a = u64(addr) % self.mem_size
            for i in range(4):
                self.mem[(a+i) % self.mem_size] = (val >> (8*i)) & 0xFF

        def mem_read64(self, addr: int) -> int:
            a = u64(addr) % self.mem_size
            v = 0
            for i in range(8):
                v |= self.mem[(a+i) % self.mem_size] << (8*i)
            return v

        def mem_write64(self, addr: int, val: int):
            a = u64(addr) % self.mem_size
            for i in range(8):
                self.mem[(a+i) % self.mem_size] = (val >> (8*i)) & 0xFF

        # -- Stack helpers --

        def push64(self, val: int):
            self.sp = u64(self.sp - 8)
            self.mem_write64(self.sp, val)

        def pop64(self) -> int:
            val = self.mem_read64(self.sp)
            self.sp = u64(self.sp + 8)
            return val

        # -- Fetch --

        def fetch8(self) -> int:
            val = self.mem_read8(self.pc)
            self.pc = u64(self.pc + 1)
            return val

        # -- Condition evaluation --

        def eval_cond(self, cc: int) -> bool:
            """Evaluate condition code (same logic as Python version)."""
            if cc == CC_AL:  return True
            if cc == CC_EQ:  return self.flag_z == 1
            if cc == CC_NE:  return self.flag_z == 0
            if cc == CC_CS:  return self.flag_c == 1
            if cc == CC_CC:  return self.flag_c == 0
            if cc == CC_MI:  return self.flag_n == 1
            if cc == CC_PL:  return self.flag_n == 0
            if cc == CC_VS:  return self.flag_v == 1
            if cc == CC_VC:  return self.flag_v == 0
            if cc == CC_GT:  return self.flag_g == 1
            if cc == CC_LE:  return self.flag_g == 0
            if cc == CC_BQ:  return self.q_out == 1
            if cc == CC_BNQ: return self.q_out == 0
            if cc == CC_SAT: return self.flag_s == 1
            if cc == CC_EF:  return self.ef_flags != 0
            if cc == CC_NV:  return False
            return False

        # -- CSR access --

        def csr_read(self, addr: int) -> int:
            """Read a CSR. Can be overridden by system.py for IPI wiring."""
            return _csr_read_py(self, addr)

        def csr_write(self, addr: int, val: int):
            _csr_write_py(self, addr, val)

        # -- Step (delegates to C++ or Python fallback) --

        def step(self):
            """Execute one instruction. Returns number of cycles consumed.

            Tries the C++ fast path first.  Falls back to the pure-Python
            step() for MEX FP operations and other edge cases.
            """
            if self.halted:
                raise HaltError("CPU is halted")
            if self.idle:
                self._cs.cycle_count += 1
                return 1

            old_cycles = self._cs.cycle_count

            try:
                cycles = _accel.step_one(
                    self._cs,
                    mmio_read8=self._mmio_read8,
                    mmio_write8=self._mmio_write8,
                    on_output=self._do_output,
                    csr_read_override=getattr(self, '_csr_read_override', None),
                    mmio_start=0xFFFF_FF00_0000_0000,
                    mmio_end=0xFFFF_FF80_0000_0000,
                )
                return cycles
            except RuntimeError as e:
                msg = str(e)
                if msg == "HALT":
                    if self.on_halt:
                        self.on_halt()
                    raise HaltError("CPU halted")
                elif msg == "MEX_FALLBACK":
                    return self._step_python_fallback()
                elif msg.startswith("TRAP:"):
                    self._handle_trap(msg)
                else:
                    raise

        def _mmio_read8(self, addr):
            """MMIO read callback — uses the monkey-patched mem_read8."""
            return self.mem_read8(addr)

        def _mmio_write8(self, addr, val):
            """MMIO write callback — uses the monkey-patched mem_write8."""
            self.mem_write8(addr, val)

        def _do_output(self, port, val):
            if self.on_output:
                self.on_output(port, val)

        def _handle_trap(self, msg: str):
            """Handle trap strings from C++."""
            if "SW_TRAP" in msg:
                ivec = IVEC_SW_TRAP
            elif "DIV_ZERO" in msg:
                ivec = IVEC_DIV_ZERO
            elif "PRIV_FAULT" in msg:
                ivec = IVEC_PRIV_FAULT
            elif "ILLEGAL_OP" in msg:
                ivec = IVEC_ILLEGAL_OP
            elif "RESET" in msg:
                self._reset_state()
                return  # RESET is not a trap
            else:
                ivec = IVEC_ILLEGAL_OP
            raise TrapError(ivec, msg)

        def _step_python_fallback(self):
            """Fall back to pure-Python step() for complex instructions.
            Returns cycle count."""
            # Sync C++ state → Python fallback CPU
            fb = self._get_fallback()
            _sync_cs_to_py(self._cs, fb)
            fb.mem = self.mem               # share memory
            fb.mem_read8 = self.mem_read8    # use patched MMIO
            fb.mem_write8 = self.mem_write8
            fb.mem_read16 = self.mem_read16
            fb.mem_write16 = self.mem_write16
            fb.mem_read32 = self.mem_read32
            fb.mem_write32 = self.mem_write32
            fb.mem_read64 = self.mem_read64
            fb.mem_write64 = self.mem_write64
            fb.on_output = self.on_output

            cycles = fb.step()

            # Sync back Python → C++
            _sync_py_to_cs(fb, self._cs)
            return cycles

        def _get_fallback(self) -> _PyMegapad64:
            if self._py_fallback is None:
                self._py_fallback = _PyMegapad64(
                    mem_size=self.mem_size,
                    core_id=self.core_id,
                    num_cores=self.num_cores,
                )
            return self._py_fallback

        # -- Batch execution (main acceleration entry point) --

        def run_steps(self, max_steps: int = 1_000_000):
            """Execute up to max_steps instructions in C++ without returning.

            This is the primary acceleration path — the entire loop stays
            in C++ and only calls back to Python for MMIO accesses.
            Returns (steps_executed, stop_reason).
            """
            if self.halted or self.idle:
                return 0, 1 if self.halted else 2

            try:
                result = _accel.run_steps(
                    self._cs,
                    mmio_read8=self._mmio_read8,
                    mmio_write8=self._mmio_write8,
                    on_output=self._do_output,
                    csr_read_override=getattr(self, '_csr_read_override', None),
                    mmio_start=0xFFFF_FF00_0000_0000,
                    mmio_end=0xFFFF_FF80_0000_0000,
                    max_steps=max_steps,
                )
                return result.steps_executed, result.stop_reason
            except RuntimeError as e:
                msg = str(e)
                if msg == "HALT":
                    return 0, 1
                elif msg == "MEX_FALLBACK":
                    # Single-step fallback then resume
                    self._step_python_fallback()
                    return 1, 0
                elif msg.startswith("TRAP:"):
                    self._handle_trap(msg)
                    return 1, 0  # trap handled, continue
                else:
                    raise

        # -- Instruction size (for SKIP) --

        def _next_instruction_size(self) -> int:
            """Peek at the next instruction to determine its byte length."""
            # Delegate to pure-Python version
            fb = self._get_fallback()
            _sync_cs_to_py(self._cs, fb)
            fb.mem = self.mem
            return fb._next_instruction_size()

        # -- Trap delivery --

        def _trap(self, ivec_id: int):
            """Deliver a synchronous trap."""
            if self.ivt_base == 0:
                raise TrapError(ivec_id)
            self.push64(self.flags_pack() | (self.priv_level << 8))
            self.push64(self.pc)
            self.flag_i = 0
            self.priv_level = 0  # escalate to supervisor
            self.ivec_id = ivec_id
            handler = self.mem_read64(self.ivt_base + ivec_id * 8)
            self.pc = handler

        # -- Reset --

        def _reset_state(self):
            """Reinitialize all CPU state (called by system.py boot)."""
            for i in range(16):
                self._cs.set_reg(i, 0)
            self._cs.psel = 3
            self._cs.xsel = 2
            self._cs.spsel = 15
            self._cs.flag_z = self._cs.flag_c = self._cs.flag_n = self._cs.flag_v = 0
            self._cs.flag_p = self._cs.flag_g = self._cs.flag_i = self._cs.flag_s = 0
            self._cs.d_reg = 0
            self._cs.q_out = 0
            self._cs.t_reg = 0
            self._cs.sb = self._cs.sr = self._cs.sc = 0
            self._cs.sw = 1
            self._cs.tmode = self._cs.tctrl = 0
            self._cs.tsrc0 = self._cs.tsrc1 = self._cs.tdst = 0
            for i in range(4):
                self._cs.set_acc(i, 0)
            self._cs.tstride_r = 0
            self.tstride_c = 0
            self._cs.ttile_h = 8
            self._cs.ttile_w = 8
            self._cs.ivt_base = 0
            self._cs.ivec_id = 0
            self.irq_ipi = False
            self._cs.halted = False
            self._cs.idle = False
            self._cs.priv_level = 0
            self._cs.mpu_base = 0
            self._cs.mpu_limit = 0
            self._cs.ext_modifier = -1

        # -- Run (high-level loop) --

        def run(self, max_steps: int = 1_000_000) -> int:
            """Run until HALT, IDLE, or max_steps. Returns total cycles."""
            total = 0
            for _ in range(max_steps):
                if self.halted or self.idle:
                    break
                try:
                    self.step()
                    total += 1
                except TrapError as e:
                    if self.ivt_base != 0:
                        self._trap(e.ivec_id)
                    else:
                        raise
            return total

        # -- Load bytes --

        def load_bytes(self, addr: int, data: bytes | bytearray):
            """Write raw bytes into memory at the given address."""
            for i, b in enumerate(data):
                self.mem[(addr + i) % self.mem_size] = b

        # -- Debug / introspection --

        def dump_regs(self) -> str:
            lines = []
            for i in range(16):
                tag = ""
                if i == self.psel: tag += " <PC"
                if i == self.xsel: tag += " <X"
                if i == self.spsel: tag += " <SP"
                lines.append(f"  R{i:<2d} = {self.regs[i]:#018x}{tag}")
            lines.append(f"  FLAGS = Z={self.flag_z} C={self.flag_c} N={self.flag_n} "
                          f"V={self.flag_v} P={self.flag_p} G={self.flag_g} "
                          f"I={self.flag_i} S={self.flag_s}")
            lines.append(f"  D = {self.d_reg:#04x}  Q = {self.q_out}  "
                          f"PSEL={self.psel} XSEL={self.xsel} SPSEL={self.spsel}")
            return "\n".join(lines)


    # ── Helper proxies for list-like access ──────────────────

    class _RegProxy:
        """Provides list-like access to CPUState registers."""
        __slots__ = ('_cs',)
        def __init__(self, cs): self._cs = cs
        def __getitem__(self, i):
            if isinstance(i, slice):
                return [self._cs.get_reg(j) for j in range(*i.indices(16))]
            return self._cs.get_reg(i & 0xF)
        def __setitem__(self, i, v):
            if isinstance(i, slice):
                for j, val in zip(range(*i.indices(16)), v):
                    self._cs.set_reg(j, u64(val))
            else:
                self._cs.set_reg(i & 0xF, u64(v))
        def __len__(self): return 16
        def __iter__(self): return (self._cs.get_reg(i) for i in range(16))
        def __repr__(self): return repr(list(self))

    class _AccProxy:
        __slots__ = ('_cs',)
        def __init__(self, cs): self._cs = cs
        def __getitem__(self, i): return self._cs.get_acc(i & 3)
        def __setitem__(self, i, v): self._cs.set_acc(i & 3, u64(v))
        def __len__(self): return 4
        def __iter__(self): return (self._cs.get_acc(i) for i in range(4))
        def __repr__(self): return repr(list(self))

    class _PortOutProxy:
        __slots__ = ('_cs',)
        def __init__(self, cs): self._cs = cs
        def __getitem__(self, i): return self._cs.get_port_out(i & 7)
        def __setitem__(self, i, v): pass  # read-only from Python
        def __len__(self): return 8
        def __iter__(self): return (self._cs.get_port_out(i) for i in range(8))
        def __repr__(self): return repr(list(self))

    class _PortInProxy:
        __slots__ = ('_cs',)
        def __init__(self, cs): self._cs = cs
        def __getitem__(self, i): return 0  # input ports read via C++
        def __setitem__(self, i, v): self._cs.set_port_in(i & 7, v)
        def __len__(self): return 8
        def __repr__(self): return repr([0]*8)

    # ── State sync helpers ───────────────────────────────────

    def _sync_cs_to_py(cs, py_cpu: _PyMegapad64):
        """Copy C++ CPUState → Python Megapad64 for fallback execution."""
        for i in range(16):
            py_cpu.regs[i] = cs.get_reg(i)
        py_cpu.psel = cs.psel
        py_cpu.xsel = cs.xsel
        py_cpu.spsel = cs.spsel
        py_cpu.flag_z = cs.flag_z
        py_cpu.flag_c = cs.flag_c
        py_cpu.flag_n = cs.flag_n
        py_cpu.flag_v = cs.flag_v
        py_cpu.flag_p = cs.flag_p
        py_cpu.flag_g = cs.flag_g
        py_cpu.flag_i = cs.flag_i
        py_cpu.flag_s = cs.flag_s
        py_cpu.d_reg = cs.d_reg
        py_cpu.q_out = cs.q_out
        py_cpu.t_reg = cs.t_reg
        py_cpu.sb = cs.sb
        py_cpu.sr = cs.sr
        py_cpu.sc = cs.sc
        py_cpu.sw = cs.sw
        py_cpu.tmode = cs.tmode
        py_cpu.tctrl = cs.tctrl
        py_cpu.tsrc0 = cs.tsrc0
        py_cpu.tsrc1 = cs.tsrc1
        py_cpu.tdst = cs.tdst
        for i in range(4):
            py_cpu.acc[i] = cs.get_acc(i)
        py_cpu.ivt_base = cs.ivt_base
        py_cpu.ivec_id = cs.ivec_id
        py_cpu.trap_addr = cs.trap_addr
        py_cpu.ef_flags = cs.ef_flags
        py_cpu.halted = cs.halted
        py_cpu.idle = cs.idle
        py_cpu.cycle_count = cs.cycle_count
        py_cpu.tstride_r = cs.tstride_r
        py_cpu.ttile_h = cs.ttile_h
        py_cpu.ttile_w = cs.ttile_w
        py_cpu.perf_enable = cs.perf_enable
        py_cpu.perf_cycles = cs.perf_cycles
        py_cpu.perf_stalls = cs.perf_stalls
        py_cpu.perf_tileops = cs.perf_tileops
        py_cpu.perf_extmem = cs.perf_extmem
        py_cpu._ext_modifier = cs.ext_modifier
        py_cpu.bist_status = cs.bist_status
        py_cpu.bist_fail_addr = cs.bist_fail_addr
        py_cpu.bist_fail_data = cs.bist_fail_data
        py_cpu.tile_selftest = cs.tile_selftest
        py_cpu.tile_st_detail = cs.tile_st_detail
        py_cpu.icache_enabled = cs.icache_enabled
        py_cpu.icache_hits = cs.icache_hits
        py_cpu.icache_misses = cs.icache_misses
        py_cpu.priv_level = cs.priv_level
        py_cpu.mpu_base = cs.mpu_base
        py_cpu.mpu_limit = cs.mpu_limit

    def _sync_py_to_cs(py_cpu: _PyMegapad64, cs):
        """Copy Python Megapad64 state → C++ CPUState after fallback."""
        for i in range(16):
            cs.set_reg(i, u64(py_cpu.regs[i]))
        cs.psel = py_cpu.psel
        cs.xsel = py_cpu.xsel
        cs.spsel = py_cpu.spsel
        cs.flag_z = py_cpu.flag_z
        cs.flag_c = py_cpu.flag_c
        cs.flag_n = py_cpu.flag_n
        cs.flag_v = py_cpu.flag_v
        cs.flag_p = py_cpu.flag_p
        cs.flag_g = py_cpu.flag_g
        cs.flag_i = py_cpu.flag_i
        cs.flag_s = py_cpu.flag_s
        cs.d_reg = py_cpu.d_reg
        cs.q_out = py_cpu.q_out
        cs.t_reg = py_cpu.t_reg
        cs.sb = py_cpu.sb
        cs.sr = py_cpu.sr
        cs.sc = py_cpu.sc
        cs.sw = py_cpu.sw
        cs.tmode = py_cpu.tmode
        cs.tctrl = py_cpu.tctrl
        cs.tsrc0 = py_cpu.tsrc0
        cs.tsrc1 = py_cpu.tsrc1
        cs.tdst = py_cpu.tdst
        for i in range(4):
            cs.set_acc(i, u64(py_cpu.acc[i]))
        cs.ivt_base = py_cpu.ivt_base
        cs.ivec_id = py_cpu.ivec_id
        cs.trap_addr = py_cpu.trap_addr
        cs.ef_flags = py_cpu.ef_flags
        cs.halted = py_cpu.halted
        cs.idle = py_cpu.idle
        cs.cycle_count = py_cpu.cycle_count
        cs.tstride_r = py_cpu.tstride_r
        cs.ttile_h = py_cpu.ttile_h
        cs.ttile_w = py_cpu.ttile_w
        cs.perf_enable = py_cpu.perf_enable
        cs.perf_cycles = py_cpu.perf_cycles
        cs.perf_stalls = py_cpu.perf_stalls
        cs.perf_tileops = py_cpu.perf_tileops
        cs.perf_extmem = py_cpu.perf_extmem
        cs.ext_modifier = py_cpu._ext_modifier
        cs.bist_status = py_cpu.bist_status
        cs.bist_fail_addr = py_cpu.bist_fail_addr
        cs.bist_fail_data = py_cpu.bist_fail_data
        cs.tile_selftest = py_cpu.tile_selftest
        cs.tile_st_detail = py_cpu.tile_st_detail
        cs.icache_enabled = py_cpu.icache_enabled
        cs.icache_hits = py_cpu.icache_hits
        cs.icache_misses = py_cpu.icache_misses
        cs.priv_level = py_cpu.priv_level
        cs.mpu_base = py_cpu.mpu_base
        cs.mpu_limit = py_cpu.mpu_limit

    # ── CSR access (Python-side, matching megapad64.py) ──────

    def _csr_read_py(cpu, addr: int) -> int:
        """Python-side CSR read — matches megapad64.py csr_read."""
        cs = cpu._cs
        m = {
            CSR_FLAGS: lambda: cs.flags_pack(),
            CSR_PSEL: lambda: cs.psel,
            CSR_XSEL: lambda: cs.xsel,
            CSR_SPSEL: lambda: cs.spsel,
            CSR_IVT_BASE: lambda: cs.ivt_base,
            CSR_D: lambda: cs.d_reg,
            CSR_DF: lambda: cs.flag_c,
            CSR_Q: lambda: cs.q_out,
            CSR_T: lambda: cs.t_reg,
            CSR_IE: lambda: cs.flag_i,
            CSR_PRIV: lambda: cs.priv_level,
            CSR_MPU_BASE: lambda: cs.mpu_base,
            CSR_MPU_LIMIT: lambda: cs.mpu_limit,
            CSR_SB: lambda: cs.sb,
            CSR_SR: lambda: cs.sr,
            CSR_SC: lambda: cs.sc,
            CSR_SW: lambda: cs.sw,
            CSR_TMODE: lambda: cs.tmode,
            CSR_TCTRL: lambda: cs.tctrl,
            CSR_TSRC0: lambda: cs.tsrc0,
            CSR_TSRC1: lambda: cs.tsrc1,
            CSR_TDST: lambda: cs.tdst,
            CSR_ACC0: lambda: cs.get_acc(0),
            CSR_ACC1: lambda: cs.get_acc(1),
            CSR_ACC2: lambda: cs.get_acc(2),
            CSR_ACC3: lambda: cs.get_acc(3),
            CSR_COREID: lambda: cs.core_id,
            CSR_NCORES: lambda: cs.num_cores,
            CSR_IVEC_ID: lambda: cs.ivec_id,
            CSR_TRAP_ADDR: lambda: cs.trap_addr,
            CSR_MEGAPAD_SZ: lambda: 64,
            CSR_CPUID: lambda: 0x4D503634,
            CSR_TSTRIDE_R: lambda: cs.tstride_r,
            CSR_TTILE_H: lambda: cs.ttile_h,
            CSR_TTILE_W: lambda: cs.ttile_w,
            CSR_BIST_STATUS: lambda: cs.bist_status,
            CSR_BIST_FAIL_ADDR: lambda: cs.bist_fail_addr,
            CSR_BIST_FAIL_DATA: lambda: cs.bist_fail_data,
            CSR_TILE_SELFTEST: lambda: cs.tile_selftest,
            CSR_TILE_ST_DETAIL: lambda: cs.tile_st_detail,
            CSR_PERF_CYCLES: lambda: cs.perf_cycles,
            CSR_PERF_STALLS: lambda: cs.perf_stalls,
            CSR_PERF_TILEOPS: lambda: cs.perf_tileops,
            CSR_PERF_EXTMEM: lambda: cs.perf_extmem,
            CSR_PERF_CTRL: lambda: cs.perf_enable,
            CSR_ICACHE_CTRL: lambda: cs.icache_enabled,
            CSR_ICACHE_HITS: lambda: cs.icache_hits,
            CSR_ICACHE_MISSES: lambda: cs.icache_misses,
        }
        fn = m.get(addr)
        return fn() if fn else 0

    def _csr_write_py(cpu, addr: int, val: int):
        """Python-side CSR write — matches megapad64.py csr_write."""
        cs = cpu._cs
        if addr == CSR_FLAGS:     cs.flags_unpack(val & 0xFF)
        elif addr == CSR_PSEL:    cs.psel = val & 0xF
        elif addr == CSR_XSEL:    cs.xsel = val & 0xF
        elif addr == CSR_SPSEL:   cs.spsel = val & 0xF
        elif addr == CSR_IVT_BASE: cs.ivt_base = val
        elif addr == CSR_D:       cs.d_reg = val & 0xFF
        elif addr == CSR_DF:      cs.flag_c = val & 1
        elif addr == CSR_Q:       cs.q_out = val & 1
        elif addr == CSR_T:       cs.t_reg = val & 0xFF
        elif addr == CSR_IE:      cs.flag_i = val & 1
        elif addr == CSR_PRIV:    cs.priv_level = val & 1
        elif addr == CSR_MPU_BASE: cs.mpu_base = val
        elif addr == CSR_MPU_LIMIT: cs.mpu_limit = val
        elif addr == CSR_SB:      cs.sb = val
        elif addr == CSR_SR:      cs.sr = val
        elif addr == CSR_SC:      cs.sc = val
        elif addr == CSR_SW:      cs.sw = val
        elif addr == CSR_TMODE:   cs.tmode = val
        elif addr == CSR_TCTRL:   cs.tctrl = val
        elif addr == CSR_TSRC0:   cs.tsrc0 = val
        elif addr == CSR_TSRC1:   cs.tsrc1 = val
        elif addr == CSR_TDST:    cs.tdst = val
        elif addr == CSR_ACC0:    cs.set_acc(0, val)
        elif addr == CSR_ACC1:    cs.set_acc(1, val)
        elif addr == CSR_ACC2:    cs.set_acc(2, val)
        elif addr == CSR_ACC3:    cs.set_acc(3, val)
        elif addr == CSR_TSTRIDE_R: cs.tstride_r = val
        elif addr == CSR_TTILE_H: cs.ttile_h = val
        elif addr == CSR_TTILE_W: cs.ttile_w = val
        elif addr == CSR_BIST_CMD:
            if val in (1, 2): cs.bist_status = 2
        elif addr == CSR_TILE_SELFTEST:
            if val == 1:
                cs.tile_selftest = 2
                cs.tile_st_detail = 0
        elif addr == CSR_PERF_CTRL:
            if val & 1: cs.perf_enable = 1
            if val & 2:
                cs.perf_cycles = 0; cs.perf_stalls = 0
                cs.perf_tileops = 0; cs.perf_extmem = 0
                cs.perf_enable = 1
        elif addr == CSR_ICACHE_CTRL:
            cs.icache_enabled = val & 1
            if val & 2:
                cs.icache_hits = 0; cs.icache_misses = 0
                cs.icache_enabled = 1

else:
    # No accelerator available — use pure Python
    Megapad64 = _PyMegapad64
