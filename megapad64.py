"""
Megapad-64 Bytecode Emulator
=============================
A cycle-step emulator for the Megapad-64 TSP architecture, implementing the
instruction encoding from ENCODING.html.  Spiritual successor to the RCA 1802.

Every instruction is decoded from raw bytes in memory — no text parsing at
runtime.  The fetch/decode/execute loop mirrors what real hardware would do:
read byte 0, switch on the F nibble, consume additional bytes as documented.
"""

from __future__ import annotations
import struct
from typing import Optional

# ---------------------------------------------------------------------------
#  Constants
# ---------------------------------------------------------------------------

MASK64 = (1 << 64) - 1
SIGN64 = 1 << 63

# Condition codes (shared by BR / LBR / SKIP)
CC_AL   = 0x0  # always
CC_EQ   = 0x1  # Z=1
CC_NE   = 0x2  # Z=0
CC_CS   = 0x3  # C=1
CC_CC   = 0x4  # C=0
CC_MI   = 0x5  # N=1
CC_PL   = 0x6  # N=0
CC_VS   = 0x7  # V=1
CC_VC   = 0x8  # V=0
CC_GT   = 0x9  # G=1
CC_LE   = 0xA  # G=0
CC_BQ   = 0xB  # Q=1
CC_BNQ  = 0xC  # Q=0
CC_SAT  = 0xD  # S=1
CC_EF   = 0xE  # any EF
CC_NV   = 0xF  # never

# CSR addresses
CSR_FLAGS       = 0x00
CSR_PSEL        = 0x01
CSR_XSEL        = 0x02
CSR_SPSEL       = 0x03
CSR_IVT_BASE    = 0x04  # Interrupt vector table base (aligned with FPGA)
CSR_D           = 0x05  # Legacy 1802 D register
CSR_DF          = 0x06  # Legacy 1802 DF (carry alias)
CSR_Q           = 0x07  # Legacy 1802 Q output
CSR_T           = 0x08  # Legacy 1802 T register
CSR_IE          = 0x09  # Interrupt enable (alias of flag_i)
CSR_SB          = 0x10
CSR_SR          = 0x11
CSR_SC          = 0x12
CSR_SW          = 0x13
CSR_TMODE       = 0x14
CSR_TCTRL       = 0x15
CSR_TSRC0       = 0x16
CSR_TSRC1       = 0x17
CSR_TDST        = 0x18
CSR_ACC0        = 0x19
CSR_ACC1        = 0x1A
CSR_ACC2        = 0x1B
CSR_ACC3        = 0x1C
CSR_COREID      = 0x20  # Read-only: core ID (0..N-1)
CSR_NCORES      = 0x21  # Read-only: number of cores
CSR_MBOX        = 0x22  # Read: pending IPI mask, Write: send IPI
CSR_IPIACK      = 0x23  # Write: acknowledge IPI from core N
CSR_IVEC_ID     = 0x24  # Current interrupt vector ID
CSR_TRAP_ADDR   = 0x25  # Faulting address
CSR_MEGAPAD_SZ  = 0x30
CSR_CPUID       = 0x31

# Performance counter CSRs
CSR_PERF_CYCLES  = 0x68

# BIST CSRs (§6.1 — Memory Built-In Self-Test)
CSR_BIST_CMD       = 0x60   # W: 0=idle, 1=start-full, 2=start-quick
CSR_BIST_STATUS    = 0x61   # R: 0=idle, 1=running, 2=pass, 3=fail
CSR_BIST_FAIL_ADDR = 0x62   # R: first failing address
CSR_BIST_FAIL_DATA = 0x63   # R: expected vs actual data

# Tile Datapath Self-Test CSRs (§6.2)
CSR_TILE_SELFTEST  = 0x64   # W: 1=start; R: 0=idle, 1=running, 2=pass, 3=fail
CSR_TILE_ST_DETAIL = 0x65   # R: bitmask of failed sub-tests
CSR_PERF_STALLS  = 0x69
CSR_PERF_TILEOPS = 0x6A
CSR_PERF_EXTMEM  = 0x6B
CSR_PERF_CTRL    = 0x6C

# IVEC IDs
IVEC_RESET          = 0x00
IVEC_NMI            = 0x01
IVEC_ILLEGAL_OP     = 0x02
IVEC_ALIGN_FAULT    = 0x03
IVEC_DIV_ZERO       = 0x04
IVEC_BUS_FAULT      = 0x05
IVEC_SW_TRAP        = 0x06
IVEC_TIMER          = 0x07
IVEC_IPI            = 0x08  # Inter-processor interrupt

# ---------------------------------------------------------------------------
#  Helpers
# ---------------------------------------------------------------------------

def u64(v: int) -> int:
    """Mask to unsigned 64 bits."""
    return v & MASK64

def s64(v: int) -> int:
    """Interpret a 64-bit value as signed."""
    v = u64(v)
    return v - (1 << 64) if v >= SIGN64 else v

def sign_extend(val: int, bits: int) -> int:
    """Sign-extend a *bits*-wide value to 64 bits."""
    mask = (1 << bits) - 1
    val &= mask
    if val & (1 << (bits - 1)):
        val -= (1 << bits)
    return u64(val)

def zero_extend(val: int, bits: int) -> int:
    return val & ((1 << bits) - 1)

# ---------------------------------------------------------------------------
#  CPU
# ---------------------------------------------------------------------------

class Megapad64Error(Exception):
    """Base for emulator-generated traps."""
    pass

class TrapError(Megapad64Error):
    def __init__(self, ivec_id: int, message: str = ""):
        self.ivec_id = ivec_id
        super().__init__(message or f"Trap IVEC={ivec_id:#04x}")

class HaltError(Megapad64Error):
    pass


class Megapad64:
    """Megapad-64 TSP emulator — bytecode level."""

    def __init__(self, mem_size: int = 1 << 20, core_id: int = 0,
                 num_cores: int = 1):
        self.mem_size = mem_size
        self.mem = bytearray(mem_size)

        # Core identity (multicore)
        self.core_id: int = core_id
        self.num_cores: int = num_cores
        self.irq_ipi: bool = False  # pending IPI from mailbox

        # 16 × 64-bit GPRs
        self.regs: list[int] = [0] * 16

        # Designators (1802 heritage)
        self.psel: int = 3   # which GPR is PC  — default R3 (1802: P)
        self.xsel: int = 2   # which GPR is data pointer R(X) (1802: X)
        self.spsel: int = 15 # which GPR is stack pointer

        # FLAGS: individual bits
        self.flag_z: int = 0  # zero
        self.flag_c: int = 0  # carry / DF
        self.flag_n: int = 0  # negative
        self.flag_v: int = 0  # overflow
        self.flag_p: int = 0  # parity (low 8 bits)
        self.flag_g: int = 0  # unsigned greater (CMP)
        self.flag_i: int = 0  # interrupt enable
        self.flag_s: int = 0  # saturation sticky

        # 1802-heritage special registers
        self.d_reg: int = 0   # 8-bit D accumulator
        self.q_out: int = 0   # Q flip-flop
        self.t_reg: int = 0   # T register (saved XSEL|PSEL)

        # Tile / MEX CSRs
        self.sb: int  = 0
        self.sr: int  = 0
        self.sc: int  = 0
        self.sw: int  = 1
        self.tmode: int = 0
        self.tctrl: int = 0
        self.tsrc0: int = 0
        self.tsrc1: int = 0
        self.tdst:  int = 0
        self.acc = [0, 0, 0, 0]  # ACC0-ACC3

        # System CSRs
        self.ivt_base: int  = 0
        self.ivec_id: int   = 0
        self.trap_addr: int = 0

        # External flag inputs (EF1-EF4) — directly settable
        self.ef_flags: int = 0  # 4 bits

        # I/O ports (4-bit, 7 ports each direction — 1802-style)
        self.port_out: list[int] = [0] * 8  # port 0 unused, 1-7
        self.port_in:  list[int] = [0] * 8

        # State
        self.halted: bool = False
        self.idle: bool   = False
        self.cycle_count: int = 0

        # Performance counters
        self.perf_enable: int  = 1   # counting enabled by default
        self.perf_cycles: int  = 0   # total clock cycles
        self.perf_stalls: int  = 0   # stall cycles (bus/memory wait)
        self.perf_tileops: int = 0   # tile engine operations completed
        self.perf_extmem: int  = 0   # external memory beats

        # Memory BIST state
        self.bist_status: int    = 0   # 0=idle, 2=pass, 3=fail
        self.bist_fail_addr: int = 0   # first failing address
        self.bist_fail_data: int = 0   # expected vs actual (packed)

        # Tile datapath self-test state
        self.tile_selftest: int    = 0   # 0=idle, 2=pass, 3=fail
        self.tile_st_detail: int   = 0   # bitmask of failed sub-tests

        # EXT prefix state
        self._ext_modifier: int = -1  # -1 = no active prefix

        # Callbacks
        self.on_output: Optional[callable] = None  # called with (port, value)
        self.on_halt: Optional[callable] = None

    # -- Property shortcuts --

    @property
    def pc(self) -> int:
        return self.regs[self.psel]

    @pc.setter
    def pc(self, value: int):
        self.regs[self.psel] = u64(value)

    @property
    def rx(self) -> int:
        """R(X) — data pointer register value."""
        return self.regs[self.xsel]

    @rx.setter
    def rx(self, value: int):
        self.regs[self.xsel] = u64(value)

    @property
    def sp(self) -> int:
        return self.regs[self.spsel]

    @sp.setter
    def sp(self, value: int):
        self.regs[self.spsel] = u64(value)

    # -- Flags pack/unpack --

    def flags_pack(self) -> int:
        """Pack flags into an 8-bit byte: [S I G P V N C Z] (bit 7..0)."""
        return (
            (self.flag_z)       |
            (self.flag_c  << 1) |
            (self.flag_n  << 2) |
            (self.flag_v  << 3) |
            (self.flag_p  << 4) |
            (self.flag_g  << 5) |
            (self.flag_i  << 6) |
            (self.flag_s  << 7)
        )

    def flags_unpack(self, val: int):
        self.flag_z = (val >> 0) & 1
        self.flag_c = (val >> 1) & 1
        self.flag_n = (val >> 2) & 1
        self.flag_v = (val >> 3) & 1
        self.flag_p = (val >> 4) & 1
        self.flag_g = (val >> 5) & 1
        self.flag_i = (val >> 6) & 1
        self.flag_s = (val >> 7) & 1

    # -- Memory access --

    def _check_addr(self, addr: int, size: int = 1):
        addr = u64(addr)
        if addr + size > self.mem_size:
            self.trap_addr = addr
            raise TrapError(IVEC_BUS_FAULT, f"Bus fault @ {addr:#018x}")

    def mem_read8(self, addr: int) -> int:
        addr = u64(addr) % self.mem_size
        return self.mem[addr]

    def mem_write8(self, addr: int, val: int):
        addr = u64(addr) % self.mem_size
        self.mem[addr] = val & 0xFF

    def mem_read16(self, addr: int) -> int:
        a = u64(addr) % self.mem_size
        return self.mem[a] | (self.mem[(a+1) % self.mem_size] << 8)

    def mem_write16(self, addr: int, val: int):
        a = u64(addr) % self.mem_size
        self.mem[a]                        = val & 0xFF
        self.mem[(a+1) % self.mem_size]    = (val >> 8) & 0xFF

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
        """Fetch one byte at PC and advance PC."""
        val = self.mem_read8(self.pc)
        self.pc = u64(self.pc + 1)
        return val

    # -- Condition evaluation --

    def eval_cond(self, cc: int) -> bool:
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

    # -- Flag update helpers --

    def _parity8(self, val: int) -> int:
        """Even parity of low 8 bits: 1 if even number of 1-bits."""
        b = val & 0xFF
        b ^= b >> 4
        b ^= b >> 2
        b ^= b >> 1
        return (b & 1) ^ 1  # 1 = even parity

    def _update_flags_arith(self, a: int, b: int, result: int, is_sub: bool = False):
        """Update Z, C, N, V, P for add/sub."""
        r = u64(result)
        self.flag_z = 1 if r == 0 else 0
        self.flag_n = (r >> 63) & 1
        self.flag_p = self._parity8(r)

        if is_sub:
            # C=1 means no borrow (a >= b for unsigned)
            self.flag_c = 1 if u64(a) >= u64(b) else 0
        else:
            # C=1 means carry out
            self.flag_c = 1 if (a + b) > MASK64 else 0

        # Signed overflow
        sa = s64(a)
        sb = s64(b)
        sr = s64(result)
        if is_sub:
            self.flag_v = 1 if ((sa >= 0 and sb < 0 and sr < 0) or
                                (sa < 0 and sb >= 0 and sr >= 0)) else 0
        else:
            self.flag_v = 1 if ((sa >= 0 and sb >= 0 and sr < 0) or
                                (sa < 0 and sb < 0 and sr >= 0)) else 0

    def _update_flags_logic(self, result: int):
        """Update Z, N, P; clear C, V for logic ops."""
        r = u64(result)
        self.flag_z = 1 if r == 0 else 0
        self.flag_n = (r >> 63) & 1
        self.flag_p = self._parity8(r)
        self.flag_c = 0
        self.flag_v = 0

    def _update_flags_cmp(self, a: int, b: int, result: int):
        """Update Z, C, N, V, G for CMP."""
        self._update_flags_arith(a, b, result, is_sub=True)
        self.flag_g = 1 if u64(a) > u64(b) else 0

    # -- CSR read/write --

    def csr_read(self, addr: int) -> int:
        m = {
            CSR_FLAGS:      self.flags_pack,
            CSR_PSEL:       lambda: self.psel,
            CSR_XSEL:       lambda: self.xsel,
            CSR_SPSEL:      lambda: self.spsel,
            CSR_IVT_BASE:   lambda: self.ivt_base,
            CSR_D:          lambda: self.d_reg,
            CSR_DF:         lambda: self.flag_c,
            CSR_Q:          lambda: self.q_out,
            CSR_T:          lambda: self.t_reg,
            CSR_IE:         lambda: self.flag_i,
            CSR_SB:         lambda: self.sb,
            CSR_SR:         lambda: self.sr,
            CSR_SC:         lambda: self.sc,
            CSR_SW:         lambda: self.sw,
            CSR_TMODE:      lambda: self.tmode,
            CSR_TCTRL:      lambda: self.tctrl,
            CSR_TSRC0:      lambda: self.tsrc0,
            CSR_TSRC1:      lambda: self.tsrc1,
            CSR_TDST:       lambda: self.tdst,
            CSR_ACC0:       lambda: self.acc[0],
            CSR_ACC1:       lambda: self.acc[1],
            CSR_ACC2:       lambda: self.acc[2],
            CSR_ACC3:       lambda: self.acc[3],
            CSR_COREID:     lambda: self.core_id,
            CSR_NCORES:     lambda: self.num_cores,
            CSR_MBOX:       lambda: self._ipi_pending_mask,
            CSR_IPIACK:     lambda: 0,  # write-only
            CSR_IVEC_ID:    lambda: self.ivec_id,
            CSR_TRAP_ADDR:  lambda: self.trap_addr,
            CSR_MEGAPAD_SZ: lambda: 0,      # 8 MiB config
            CSR_CPUID:      lambda: 0x4D50_3634_0001_0000,  # "MP64" v1.0
            CSR_PERF_CYCLES:  lambda: u64(self.perf_cycles),
            CSR_PERF_STALLS:  lambda: u64(self.perf_stalls),
            CSR_PERF_TILEOPS: lambda: u64(self.perf_tileops),
            CSR_PERF_EXTMEM:  lambda: u64(self.perf_extmem),
            CSR_PERF_CTRL:    lambda: self.perf_enable & 1,
            CSR_BIST_CMD:       lambda: 0,  # write-only
            CSR_BIST_STATUS:    lambda: self.bist_status,
            CSR_BIST_FAIL_ADDR: lambda: self.bist_fail_addr,
            CSR_BIST_FAIL_DATA: lambda: self.bist_fail_data,
            CSR_TILE_SELFTEST:  lambda: self.tile_selftest,
            CSR_TILE_ST_DETAIL: lambda: self.tile_st_detail,
        }
        fn = m.get(addr)
        if fn is None:
            return 0
        return fn() if callable(fn) else fn

    def csr_write(self, addr: int, val: int):
        val = u64(val)
        dispatch = {
            CSR_FLAGS:    lambda v: self.flags_unpack(v & 0xFF),
            CSR_PSEL:     lambda v: setattr(self, 'psel',  v & 0xF),
            CSR_XSEL:     lambda v: setattr(self, 'xsel',  v & 0xF),
            CSR_SPSEL:    lambda v: setattr(self, 'spsel', v & 0xF),
            CSR_IVT_BASE: lambda v: setattr(self, 'ivt_base', v),
            CSR_D:        lambda v: setattr(self, 'd_reg', v & 0xFF),
            CSR_DF:       lambda v: setattr(self, 'flag_c', v & 1),
            CSR_Q:        lambda v: setattr(self, 'q_out',  v & 1),
            CSR_T:        lambda v: setattr(self, 't_reg',  v & 0xFF),
            CSR_IE:       lambda v: setattr(self, 'flag_i', v & 1),
            CSR_SB:       lambda v: setattr(self, 'sb', v & 0xF),
            CSR_SR:       lambda v: setattr(self, 'sr', v & 0xFFFFF),
            CSR_SC:       lambda v: setattr(self, 'sc', v & 0xFFFFF),
            CSR_SW:       lambda v: setattr(self, 'sw', v & 0xFFFFF),
            CSR_TMODE:    lambda v: setattr(self, 'tmode', v & 0xFF),
            CSR_TCTRL:    lambda v: setattr(self, 'tctrl', v & 0xFF),
            CSR_TSRC0:    lambda v: setattr(self, 'tsrc0', v),
            CSR_TSRC1:    lambda v: setattr(self, 'tsrc1', v),
            CSR_TDST:     lambda v: setattr(self, 'tdst', v),
            CSR_ACC0:     lambda v: self.acc.__setitem__(0, v),
            CSR_ACC1:     lambda v: self.acc.__setitem__(1, v),
            CSR_ACC2:     lambda v: self.acc.__setitem__(2, v),
            CSR_ACC3:     lambda v: self.acc.__setitem__(3, v),
            # COREID, NCORES are read-only — writes ignored
            CSR_MBOX:     lambda v: self._ipi_send(v),
            CSR_IPIACK:   lambda v: self._ipi_ack(v),
            CSR_IVEC_ID:  lambda v: setattr(self, 'ivec_id', v & 0xFF),
            CSR_PERF_CTRL: lambda v: self._perf_ctrl_write(v),
            CSR_BIST_CMD:  lambda v: self._bist_cmd_write(v),
            CSR_TILE_SELFTEST: lambda v: self._tile_selftest_write(v),
        }
        fn = dispatch.get(addr)
        if fn:
            fn(val)

    # -- IPI helpers (stubs — system.py wires the real mailbox) --

    def _perf_ctrl_write(self, val: int):
        """Handle writes to CSR_PERF_CTRL."""
        self.perf_enable = val & 1
        if val & 2:  # bit 1 = reset all counters
            self.perf_cycles  = 0
            self.perf_stalls  = 0
            self.perf_tileops = 0
            self.perf_extmem  = 0

    def _bist_cmd_write(self, val: int):
        """Handle writes to CSR_BIST_CMD — run memory BIST immediately.
        0=idle (no-op), 1=full test, 2=quick (March C- only)."""
        if val == 0:
            return
        self.bist_status = 1     # running
        self.bist_fail_addr = 0
        self.bist_fail_data = 0
        # Scratchpad region 0xFFF80..0xFFFFF is excluded from BIST
        end = min(self.mem_size, 0xFFF80)
        ok = self._bist_march_c_minus(end)
        if ok and val == 1:
            ok = self._bist_checkerboard(end)
        if ok and val == 1:
            ok = self._bist_addr_as_data(end)
        self.bist_status = 2 if ok else 3

    def _bist_march_c_minus(self, end: int) -> bool:
        """March C- pattern: detect stuck-at faults."""
        # Phase 1: Write 0x00 everywhere
        for a in range(end):
            self.mem[a] = 0x00
        # Phase 2: Read 0x00, write 0xFF (ascending)
        for a in range(end):
            if self.mem[a] != 0x00:
                self._bist_fail(a, 0x00, self.mem[a])
                return False
            self.mem[a] = 0xFF
        # Phase 3: Read 0xFF, write 0x00 (descending)
        for a in range(end - 1, -1, -1):
            if self.mem[a] != 0xFF:
                self._bist_fail(a, 0xFF, self.mem[a])
                return False
            self.mem[a] = 0x00
        # Phase 4: Verify all 0x00
        for a in range(end):
            if self.mem[a] != 0x00:
                self._bist_fail(a, 0x00, self.mem[a])
                return False
        return True

    def _bist_checkerboard(self, end: int) -> bool:
        """Checkerboard pattern: detect coupling faults."""
        # Pass 1: 0xAA at even addresses, 0x55 at odd
        for a in range(end):
            self.mem[a] = 0xAA if (a & 1) == 0 else 0x55
        for a in range(end):
            expected = 0xAA if (a & 1) == 0 else 0x55
            if self.mem[a] != expected:
                self._bist_fail(a, expected, self.mem[a])
                return False
        # Pass 2: invert — 0x55 at even, 0xAA at odd
        for a in range(end):
            self.mem[a] = 0x55 if (a & 1) == 0 else 0xAA
        for a in range(end):
            expected = 0x55 if (a & 1) == 0 else 0xAA
            if self.mem[a] != expected:
                self._bist_fail(a, expected, self.mem[a])
                return False
        return True

    def _bist_addr_as_data(self, end: int) -> bool:
        """Address-as-data pattern: detect address decoder faults."""
        for a in range(end):
            self.mem[a] = a & 0xFF
        for a in range(end):
            expected = a & 0xFF
            if self.mem[a] != expected:
                self._bist_fail(a, expected, self.mem[a])
                return False
        return True

    def _bist_fail(self, addr: int, expected: int, actual: int):
        """Record a BIST failure."""
        self.bist_fail_addr = addr
        # Pack expected in upper 32 bits, actual in lower 32 bits
        self.bist_fail_data = ((expected & 0xFFFFFFFF) << 32) | (actual & 0xFFFFFFFF)

    # -- Tile Datapath Self-Test (§6.2) --

    def _tile_selftest_write(self, val: int):
        """Handle writes to CSR_TILE_SELFTEST — run tile self-test."""
        if val != 1:
            return
        self.tile_selftest = 1   # running
        self.tile_st_detail = 0
        # Save tile engine state
        save = (self.tsrc0, self.tsrc1, self.tdst, self.tmode,
                self.tctrl, list(self.acc))
        # Use scratchpad: 0xFFF80..0xFFFFF (128 bytes = 2 tiles)
        # tile_a @ 0xFFF80, tile_b @ 0xFFFC0
        TILE_A = 0xFFF80
        TILE_B = 0xFFFC0
        save_scratch = bytes(self.mem[TILE_A:TILE_A + 128])
        fail_mask = 0
        fail_mask |= self._tile_st_add(TILE_A, TILE_B)    # bit 0
        fail_mask |= self._tile_st_mul(TILE_A, TILE_B)    # bit 1
        fail_mask |= self._tile_st_dot(TILE_A, TILE_B)    # bit 2
        fail_mask |= self._tile_st_sum(TILE_A)             # bit 3
        # Restore scratchpad and tile state
        self.mem[TILE_A:TILE_A + 128] = bytearray(save_scratch)
        self.tsrc0, self.tsrc1, self.tdst, self.tmode, \
            self.tctrl, self.acc = save
        self.tile_st_detail = fail_mask
        self.tile_selftest = 3 if fail_mask else 2

    def _tile_st_write(self, addr: int, data: bytes):
        """Write 64 bytes to memory for tile self-test."""
        a = addr % self.mem_size
        self.mem[a:a+64] = data[:64]

    def _tile_st_read(self, addr: int) -> bytearray:
        """Read 64 bytes from memory for tile self-test."""
        a = addr % self.mem_size
        return bytearray(self.mem[a:a+64])

    def _tile_st_add(self, ta: int, tb: int) -> int:
        """Sub-test 0: TALU ADD in 8-bit unsigned mode.
        Pattern: a[i]=i, b[i]=100. Expect dst[i]=(i+100)&0xFF."""
        pat_a = bytearray(i & 0xFF for i in range(64))
        pat_b = bytearray(100 for _ in range(64))
        self._tile_st_write(ta, pat_a)
        self._tile_st_write(tb, pat_b)
        self.tsrc0, self.tsrc1, self.tdst = ta, tb, ta
        self.tmode = 0x00  # 8-bit unsigned, wrapping
        self.tctrl = 0
        # Execute TALU ADD: op=0, funct=0, ss=0
        self._exec_mex_direct(ss=0, op=0, funct=0)
        result = self._tile_st_read(ta)
        for i in range(64):
            if result[i] != ((i + 100) & 0xFF):
                return 1  # bit 0
        return 0

    def _tile_st_mul(self, ta: int, tb: int) -> int:
        """Sub-test 1: TMUL MUL in 8-bit unsigned mode.
        Pattern: a[i]=i, b[i]=3. Expect dst[i]=(i*3)&0xFF."""
        pat_a = bytearray(i & 0xFF for i in range(64))
        pat_b = bytearray(3 for _ in range(64))
        self._tile_st_write(ta, pat_a)
        self._tile_st_write(tb, pat_b)
        self.tsrc0, self.tsrc1, self.tdst = ta, tb, ta
        self.tmode = 0x00
        self.tctrl = 0
        # Execute TMUL MUL: op=1, funct=0, ss=0
        self._exec_mex_direct(ss=0, op=1, funct=0)
        result = self._tile_st_read(ta)
        for i in range(64):
            if result[i] != ((i * 3) & 0xFF):
                return 2  # bit 1
        return 0

    def _tile_st_dot(self, ta: int, tb: int) -> int:
        """Sub-test 2: TMUL DOT in 8-bit unsigned mode.
        Pattern: a[i]=1, b[i]=1. Expect ACC0=64."""
        pat = bytearray(1 for _ in range(64))
        self._tile_st_write(ta, pat)
        self._tile_st_write(tb, pat)
        self.tsrc0, self.tsrc1, self.tdst = ta, tb, ta
        self.tmode = 0x00
        self.tctrl = 0
        self.acc = [0, 0, 0, 0]
        # Execute TMUL DOT: op=1, funct=1, ss=0
        self._exec_mex_direct(ss=0, op=1, funct=1)
        if self.acc[0] != 64:
            return 4  # bit 2
        return 0

    def _tile_st_sum(self, ta: int) -> int:
        """Sub-test 3: TRED SUM in 8-bit unsigned mode.
        Pattern: a[i]=2. Expect ACC0=128."""
        pat = bytearray(2 for _ in range(64))
        self._tile_st_write(ta, pat)
        self.tsrc0 = ta
        self.tmode = 0x00
        self.tctrl = 0
        self.acc = [0, 0, 0, 0]
        # Execute TRED SUM: op=2, funct=0, ss=0
        self._exec_mex_direct(ss=0, op=2, funct=0)
        if self.acc[0] != 128:
            return 8  # bit 3
        return 0

    def _exec_mex_direct(self, ss: int, op: int, funct: int,
                         broadcast_reg: int = -1):
        """Execute a tile op directly without fetching from PC.
        Used by tile datapath self-test."""
        ew_bits = self.tmode & 0x3
        elem_bytes = 1 << ew_bits
        num_lanes = 64 // elem_bytes
        signed = (self.tmode >> 4) & 1

        def read_tile(addr):
            a = u64(addr) % self.mem_size
            return bytearray(self.mem[a:a+64]) if a+64 <= self.mem_size else bytearray(64)
        def write_tile(addr, data):
            a = u64(addr) % self.mem_size
            if a + 64 <= self.mem_size:
                self.mem[a:a+64] = data[:64]
        def tile_get_elem(tile, lane, eb):
            off = lane * eb
            v = 0
            for i in range(eb):
                v |= tile[off + i] << (8 * i)
            return v
        def tile_set_elem(tile, lane, eb, val):
            off = lane * eb
            for i in range(eb):
                tile[off + i] = (val >> (8 * i)) & 0xFF
        def to_signed(v, eb):
            bits = eb * 8
            if v & (1 << (bits - 1)):
                return v - (1 << bits)
            return v

        src_a = read_tile(self.tsrc0)
        if ss == 0:
            src_b = read_tile(self.tsrc1)
        elif ss == 1:
            bval = self.regs[broadcast_reg] if broadcast_reg >= 0 else 0
            src_b = bytearray(64)
            for lane in range(num_lanes):
                tile_set_elem(src_b, lane, elem_bytes, bval & ((1 << (elem_bytes*8)) - 1))
        elif ss == 3:
            src_a = read_tile(self.tdst)
            src_b = read_tile(self.tsrc0)
        else:
            src_b = bytearray(64)
        dst = bytearray(64)

        if op == 0:  # TALU
            mask = (1 << (elem_bytes * 8)) - 1
            for lane in range(num_lanes):
                ea = tile_get_elem(src_a, lane, elem_bytes)
                eb_val = tile_get_elem(src_b, lane, elem_bytes)
                if funct == 0:  # ADD
                    r = (ea + eb_val) & mask
                elif funct == 1:  # SUB
                    r = (ea - eb_val) & mask
                else:
                    r = 0
                tile_set_elem(dst, lane, elem_bytes, r)
            write_tile(self.tdst, dst)

        elif op == 1:  # TMUL
            if funct == 0:  # MUL
                mask = (1 << (elem_bytes * 8)) - 1
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if signed:
                        r = (to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)) & mask
                    else:
                        r = (ea * eb_val) & mask
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
            elif funct == 1:  # DOT
                total = 0
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if signed:
                        total += to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)
                    else:
                        total += ea * eb_val
                self.acc[0] = total & MASK64
                self.acc[1] = (total >> 64) & MASK64

        elif op == 2:  # TRED
            values = [tile_get_elem(src_a, lane, elem_bytes) for lane in range(num_lanes)]
            if signed:
                values_s = [to_signed(v, elem_bytes) for v in values]
            else:
                values_s = values
            result = 0
            if funct == 0:  # SUM
                result = sum(values_s)
            elif funct == 1:  # MIN
                result = min(values_s)
            elif funct == 2:  # MAX
                result = max(values_s)
            self.acc[0] = result & MASK64
            self.acc[1] = (result >> 64) & MASK64

    @property
    def _ipi_pending_mask(self) -> int:
        """Override in system to return actual pending mask from mailbox."""
        return 0

    def _ipi_send(self, target_core: int):
        """Override in system to route IPI through mailbox device."""
        pass

    def _ipi_ack(self, from_core: int):
        """Override in system to clear IPI pending bit."""
        pass

    # -- Trap entry --

    def _trap(self, ivec: int):
        """Enter a trap/interrupt handler."""
        self.push64(self.flags_pack())
        self.push64(self.pc)
        self.ivec_id = ivec
        handler = self.mem_read64(u64(self.ivt_base + 8 * ivec))
        self.pc = handler
        self.flag_i = 0  # mask interrupts

    # =====================================================================
    #  STEP — the core decode/execute loop
    # =====================================================================

    def step(self) -> int:
        """Execute one instruction. Returns number of cycles consumed."""
        if self.halted:
            raise HaltError("CPU is halted")
        if self.idle:
            # In real hardware, would wait for interrupt/DMA
            self.cycle_count += 1
            return 1

        byte0 = self.fetch8()
        f = (byte0 >> 4) & 0xF  # family
        n = byte0 & 0xF         # operand nibble

        cycles = 1  # default

        # Check for EXT prefix
        if f == 0xF:
            self._ext_modifier = n
            # Re-fetch the actual instruction
            byte0 = self.fetch8()
            f = (byte0 >> 4) & 0xF
            n = byte0 & 0xF
            cycles += 1

        # Dispatch on family
        if   f == 0x0: cycles += self._exec_sys(n)
        elif f == 0x1: cycles += self._exec_inc(n)
        elif f == 0x2: cycles += self._exec_dec(n)
        elif f == 0x3: cycles += self._exec_br(n)
        elif f == 0x4: cycles += self._exec_lbr(n)
        elif f == 0x5: cycles += self._exec_mem(n)
        elif f == 0x6: cycles += self._exec_imm(n)
        elif f == 0x7: cycles += self._exec_alu(n)
        elif f == 0x8: cycles += self._exec_memalu(n)
        elif f == 0x9: cycles += self._exec_io(n)
        elif f == 0xA: cycles += self._exec_sep(n)
        elif f == 0xB: cycles += self._exec_sex(n)
        elif f == 0xC: cycles += self._exec_muldiv(n)
        elif f == 0xD: cycles += self._exec_csr(n)
        elif f == 0xE: cycles += self._exec_mex(n)
        elif f == 0xF:
            # Should not reach here (handled above), but double EXT = trap
            raise TrapError(IVEC_ILLEGAL_OP, "Double EXT prefix")

        # Clear EXT modifier after use
        self._ext_modifier = -1
        self.cycle_count += cycles

        # Update performance counters
        if self.perf_enable:
            self.perf_cycles += cycles
            # Stall cycles: any cycles beyond the base 1 for memory ops
            if f in (0x5, 0x8) and cycles > 1:    # MEM, MEMALU
                self.perf_stalls += cycles - 1
            # Tile ops: MEX family
            if f == 0xE:
                self.perf_tileops += 1

        return cycles

    # =====================================================================
    #  Family executors
    # =====================================================================

    # -- 0x0: SYS --
    def _exec_sys(self, n: int) -> int:
        if n == 0x0:  # IDL
            self.idle = True
            return 0
        elif n == 0x1:  # NOP
            return 0
        elif n == 0x2:  # HALT
            self.halted = True
            if self.on_halt:
                self.on_halt()
            return 0
        elif n == 0x3:  # RESET
            self._reset_state()
            return 0
        elif n == 0x4:  # RTI
            self.pc = self.pop64()
            self.flags_unpack(self.pop64() & 0xFF)
            return 1
        elif n == 0x5:  # RET (1802: pop XSEL|PSEL, IE←1)
            t = self.pop64() & 0xFF
            self.xsel = (t >> 4) & 0xF
            self.psel = t & 0xF
            self.flag_i = 1
            return 1
        elif n == 0x6:  # DIS (pop XSEL|PSEL, IE←0)
            t = self.pop64() & 0xFF
            self.xsel = (t >> 4) & 0xF
            self.psel = t & 0xF
            self.flag_i = 0
            return 1
        elif n == 0x7:  # MARK
            t = ((self.xsel & 0xF) << 4) | (self.psel & 0xF)
            self.t_reg = t
            self.push64(t)
            self.xsel = self.psel  # PSEL → XSEL
            return 1
        elif n == 0x8:  # SAV — store T → M(R(X))
            self.mem_write8(self.rx, self.t_reg)
            return 0
        elif n == 0x9:  # SEQ — Q ← 1
            self.q_out = 1
            return 0
        elif n == 0xA:  # REQ — Q ← 0
            self.q_out = 0
            return 0
        elif n == 0xB:  # EI
            self.flag_i = 1
            return 0
        elif n == 0xC:  # DI
            self.flag_i = 0
            return 0
        elif n == 0xD:  # CALL.L Rn (2 bytes)
            byte1 = self.fetch8()
            rn = byte1 & 0xF
            ret_addr = self.pc  # address after CALL.L
            self.push64(ret_addr)
            self.pc = self.regs[rn]
            return 1
        elif n == 0xE:  # RET.L
            self.pc = self.pop64()
            return 1
        elif n == 0xF:  # TRAP
            self._trap(IVEC_SW_TRAP)
            return 2
        return 0

    # -- 0x1: INC Rn --
    def _exec_inc(self, n: int) -> int:
        self.regs[n] = u64(self.regs[n] + 1)
        return 0

    # -- 0x2: DEC Rn --
    def _exec_dec(self, n: int) -> int:
        self.regs[n] = u64(self.regs[n] - 1)
        return 0

    # -- 0x3: BR (short branch) / SKIP --
    def _exec_br(self, cc: int) -> int:
        # SKIP mode: EXT modifier 6 means skip next instruction
        if self._ext_modifier == 6:
            if self.eval_cond(cc):
                skip_bytes = self._next_instruction_size()
                self.pc = u64(self.pc + skip_bytes)
                return 1
            return 0
        offset_byte = self.fetch8()
        offset = sign_extend(offset_byte, 8)   # signed offset
        offset = s64(offset)
        if self.eval_cond(cc):
            self.pc = u64(self.pc + offset)
            return 1  # taken branch: 1 bubble
        return 0

    # -- 0x4: LBR (long branch) --
    def _exec_lbr(self, cc: int) -> int:
        hi = self.fetch8()
        lo = self.fetch8()
        offset = sign_extend((hi << 8) | lo, 16)
        offset = s64(offset)
        if self.eval_cond(cc):
            self.pc = u64(self.pc + offset)
            return 1
        return 0

    # -- 0x5: MEM (scalar load/store) --
    def _exec_mem(self, sub: int) -> int:
        byte1 = self.fetch8()
        rd = (byte1 >> 4) & 0xF
        rs = byte1 & 0xF

        if sub == 0x0:    # LDN Rd, Rn (64-bit load via R(N))
            addr = self.regs[rs]
            self.regs[rd] = self.mem_read64(addr)
        elif sub == 0x1:  # LDA Rd, Rn (load + advance)
            addr = self.regs[rs]
            self.regs[rd] = self.mem_read64(addr)
            self.regs[rs] = u64(self.regs[rs] + 8)
        elif sub == 0x2:  # LDX Rd (load via R(X))
            self.regs[rd] = self.mem_read64(self.rx)
        elif sub == 0x3:  # LDXA Rd (load via R(X), advance)
            self.regs[rd] = self.mem_read64(self.rx)
            self.rx = u64(self.rx + 8)
        elif sub == 0x4:  # STR Rn, Rs (store 64-bit)
            addr = self.regs[rd]  # rd is actually Rn (dest addr register)
            self.mem_write64(addr, self.regs[rs])
        elif sub == 0x5:  # STXD Rs (store via R(X), decrement)
            self.mem_write64(self.rx, self.regs[rd])
            self.rx = u64(self.rx - 8)
        elif sub == 0x6:  # LD.B Rd, Rn
            addr = self.regs[rs]
            self.regs[rd] = self.mem_read8(addr)
        elif sub == 0x7:  # ST.B Rn, Rs
            addr = self.regs[rd]
            self.mem_write8(addr, self.regs[rs] & 0xFF)
        elif sub == 0x8:  # LD.H Rd, Rn
            addr = self.regs[rs]
            self.regs[rd] = self.mem_read16(addr)
        elif sub == 0x9:  # ST.H Rn, Rs
            addr = self.regs[rd]
            self.mem_write16(addr, self.regs[rs] & 0xFFFF)
        elif sub == 0xA:  # LD.W Rd, Rn
            addr = self.regs[rs]
            self.regs[rd] = self.mem_read32(addr)
        elif sub == 0xB:  # ST.W Rn, Rs
            addr = self.regs[rd]
            self.mem_write32(addr, self.regs[rs] & 0xFFFFFFFF)
        elif sub == 0xC:  # LD.SB Rd, Rn (sign-extend byte)
            addr = self.regs[rs]
            self.regs[rd] = sign_extend(self.mem_read8(addr), 8)
        elif sub == 0xD:  # LD.SH Rd, Rn (sign-extend half)
            addr = self.regs[rs]
            self.regs[rd] = sign_extend(self.mem_read16(addr), 16)
        elif sub == 0xE:  # LD.SW Rd, Rn (sign-extend word)
            addr = self.regs[rs]
            self.regs[rd] = sign_extend(self.mem_read32(addr), 32)
        elif sub == 0xF:  # LD.D Rd, [Rn+off8] (offset load)
            off_byte = self.fetch8()
            off = s64(sign_extend(off_byte, 8)) * 8  # scaled by 8
            addr = u64(self.regs[rs] + off)
            self.regs[rd] = self.mem_read64(addr)
            return 1
        return 0

    # -- 0x6: IMM --
    def _exec_imm(self, sub: int) -> int:
        if sub <= 0xB or sub in (0xC, 0xD, 0xE, 0xF):
            # These have a register byte then possibly immediate
            byte1 = self.fetch8()
            rn = (byte1 >> 4) & 0xF

        if sub == 0x0:    # LDI Rn, imm8 (or imm64 with EXT)
            if self._ext_modifier == 0:  # EXT.IMM64
                imm = 0
                for i in range(8):
                    imm |= self.fetch8() << (8 * i)
                self.regs[rn] = u64(imm)
            else:
                imm = self.fetch8()
                self.regs[rn] = imm  # zero-extend
            return 0
        elif sub == 0x1:  # LHI Rn, imm16
            lo = self.fetch8()
            hi = self.fetch8()
            imm16 = lo | (hi << 8)
            self.regs[rn] = u64((self.regs[rn] & 0x0000FFFFFFFFFFFF) | (imm16 << 48))
            return 0
        elif sub == 0x2:  # ADDI Rn, imm8
            imm = sign_extend(self.fetch8(), 8)
            a = self.regs[rn]
            result = u64(a + s64(imm))
            self._update_flags_arith(a, u64(imm), result)
            self.regs[rn] = result
            return 0
        elif sub == 0x3:  # ANDI Rn, imm8
            imm = self.fetch8()
            self.regs[rn] = self.regs[rn] & imm
            self._update_flags_logic(self.regs[rn])
            return 0
        elif sub == 0x4:  # ORI Rn, imm8
            imm = self.fetch8()
            self.regs[rn] = self.regs[rn] | imm
            self._update_flags_logic(self.regs[rn])
            return 0
        elif sub == 0x5:  # XORI Rn, imm8
            imm = self.fetch8()
            self.regs[rn] = self.regs[rn] ^ imm
            self._update_flags_logic(self.regs[rn])
            return 0
        elif sub == 0x6:  # CMPI Rn, imm8
            imm = sign_extend(self.fetch8(), 8)
            a = self.regs[rn]
            result = u64(a - s64(imm))
            self._update_flags_cmp(a, u64(imm), result)
            return 0
        elif sub == 0x7:  # SUBI Rn, imm8
            imm = sign_extend(self.fetch8(), 8)
            a = self.regs[rn]
            result = u64(a - s64(imm))
            self._update_flags_arith(a, u64(imm), result, is_sub=True)
            self.regs[rn] = result
            return 0
        elif sub == 0x8:  # LSLI Rn, imm4
            # byte1 = Rn|imm4 (already fetched)
            imm4 = byte1 & 0xF
            self.regs[rn] = u64(self.regs[rn] << imm4)
            return 0
        elif sub == 0x9:  # LSRI Rn, imm4
            imm4 = byte1 & 0xF
            self.regs[rn] = self.regs[rn] >> imm4
            return 0
        elif sub == 0xA:  # ASRI Rn, imm4
            imm4 = byte1 & 0xF
            val = s64(self.regs[rn])
            self.regs[rn] = u64(val >> imm4)
            return 0
        elif sub == 0xB:  # ROLI Rn, imm4
            imm4 = byte1 & 0xF
            v = self.regs[rn]
            self.regs[rn] = u64((v << imm4) | (v >> (64 - imm4))) if imm4 else v
            return 0
        elif sub == 0xC:  # GLO Rn — D ← R(N)[7:0]
            self.d_reg = self.regs[rn] & 0xFF
            return 0
        elif sub == 0xD:  # GHI Rn — D ← R(N)[15:8]
            self.d_reg = (self.regs[rn] >> 8) & 0xFF
            return 0
        elif sub == 0xE:  # PLO Rn — R(N)[7:0] ← D
            self.regs[rn] = (self.regs[rn] & ~0xFF) | (self.d_reg & 0xFF)
            return 0
        elif sub == 0xF:  # PHI Rn — R(N)[15:8] ← D
            self.regs[rn] = (self.regs[rn] & ~0xFF00) | ((self.d_reg & 0xFF) << 8)
            return 0
        return 0

    # -- 0x7: ALU --
    def _exec_alu(self, sub: int) -> int:
        byte1 = self.fetch8()
        rd = (byte1 >> 4) & 0xF
        rs = byte1 & 0xF
        a = self.regs[rd]
        b = self.regs[rs]

        if sub == 0x0:    # ADD
            r = u64(a + b)
            self._update_flags_arith(a, b, r)
            self.regs[rd] = r
        elif sub == 0x1:  # ADC
            r = u64(a + b + self.flag_c)
            self._update_flags_arith(a, b + self.flag_c, r)
            self.regs[rd] = r
        elif sub == 0x2:  # SUB
            r = u64(a - b)
            self._update_flags_arith(a, b, r, is_sub=True)
            self.regs[rd] = r
        elif sub == 0x3:  # SBB
            borrow = 1 - self.flag_c
            r = u64(a - b - borrow)
            self._update_flags_arith(a, u64(b + borrow), r, is_sub=True)
            self.regs[rd] = r
        elif sub == 0x4:  # AND
            r = a & b
            self._update_flags_logic(r)
            self.regs[rd] = r
        elif sub == 0x5:  # OR
            r = a | b
            self._update_flags_logic(r)
            self.regs[rd] = r
        elif sub == 0x6:  # XOR
            r = a ^ b
            self._update_flags_logic(r)
            self.regs[rd] = r
        elif sub == 0x7:  # CMP
            r = u64(a - b)
            self._update_flags_cmp(a, b, r)
        elif sub == 0x8:  # MOV
            self.regs[rd] = b
        elif sub == 0x9:  # NOT
            self.regs[rd] = u64(~b)
            self._update_flags_logic(self.regs[rd])
        elif sub == 0xA:  # NEG
            r = u64(0 - b)
            self._update_flags_arith(0, b, r, is_sub=True)
            self.regs[rd] = r
        elif sub == 0xB:  # SHL
            shift = b & 63
            out_bit = (a >> (64 - shift)) & 1 if shift else 0
            r = u64(a << shift)
            self.flag_z = 1 if r == 0 else 0
            self.flag_c = out_bit
            self.flag_n = (r >> 63) & 1
            self.flag_p = self._parity8(r)
            self.regs[rd] = r
        elif sub == 0xC:  # SHR (logical)
            shift = b & 63
            out_bit = (a >> (shift - 1)) & 1 if shift else 0
            r = a >> shift
            self.flag_z = 1 if r == 0 else 0
            self.flag_c = out_bit
            self.flag_n = (r >> 63) & 1
            self.flag_p = self._parity8(r)
            self.regs[rd] = r
        elif sub == 0xD:  # SAR (arithmetic)
            shift = b & 63
            out_bit = (a >> (shift - 1)) & 1 if shift else 0
            r = u64(s64(a) >> shift)
            self.flag_z = 1 if r == 0 else 0
            self.flag_c = out_bit
            self.flag_n = (r >> 63) & 1
            self.flag_p = self._parity8(r)
            self.regs[rd] = r
        elif sub == 0xE:  # ROL
            shift = b & 63
            r = u64((a << shift) | (a >> (64 - shift))) if shift else a
            self.flag_z = 1 if r == 0 else 0
            self.flag_n = (r >> 63) & 1
            self.flag_p = self._parity8(r)
            self.regs[rd] = r
        elif sub == 0xF:  # ROR
            shift = b & 63
            r = u64((a >> shift) | (a << (64 - shift))) if shift else a
            self.flag_z = 1 if r == 0 else 0
            self.flag_n = (r >> 63) & 1
            self.flag_p = self._parity8(r)
            self.regs[rd] = r
        return 0

    # -- 0x8: MEMALU (1802 compat) --
    def _exec_memalu(self, sub: int) -> int:
        if sub == 0x0:    # LDX — D ← M(R(X))
            self.d_reg = self.mem_read8(self.rx)
        elif sub == 0x1:  # OR.X — D ← M(R(X)) | D
            self.d_reg = (self.mem_read8(self.rx) | self.d_reg) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x2:  # AND.X
            self.d_reg = (self.mem_read8(self.rx) & self.d_reg) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x3:  # XOR.X
            self.d_reg = (self.mem_read8(self.rx) ^ self.d_reg) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x4:  # ADD.X — D ← M(R(X)) + D, carry → C
            m = self.mem_read8(self.rx)
            result = m + self.d_reg
            self.flag_c = 1 if result > 0xFF else 0
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x5:  # SD.X — D ← M(R(X)) - D
            m = self.mem_read8(self.rx)
            result = m - self.d_reg
            self.flag_c = 1 if result >= 0 else 0  # 1 = no borrow
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x6:  # SHR.D — shift D right
            self.flag_c = self.d_reg & 1
            self.d_reg = (self.d_reg >> 1) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x7:  # SM.X — D ← D - M(R(X))
            m = self.mem_read8(self.rx)
            result = self.d_reg - m
            self.flag_c = 1 if result >= 0 else 0
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x8:  # ADC.X — D ← M(R(X)) + D + C
            m = self.mem_read8(self.rx)
            result = m + self.d_reg + self.flag_c
            self.flag_c = 1 if result > 0xFF else 0
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0x9:  # SDB.X — D ← M(R(X)) - D - !C
            m = self.mem_read8(self.rx)
            borrow = 1 - self.flag_c
            result = m - self.d_reg - borrow
            self.flag_c = 1 if result >= 0 else 0
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xA:  # SHRC.D — shift D right through carry
            old_c = self.flag_c
            self.flag_c = self.d_reg & 1
            self.d_reg = ((old_c << 7) | (self.d_reg >> 1)) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xB:  # SMB.X — D ← D - M(R(X)) - !C
            m = self.mem_read8(self.rx)
            borrow = 1 - self.flag_c
            result = self.d_reg - m - borrow
            self.flag_c = 1 if result >= 0 else 0
            self.d_reg = result & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xC:  # SHL.D — shift D left
            self.flag_c = (self.d_reg >> 7) & 1
            self.d_reg = (self.d_reg << 1) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xD:  # SHLC.D — shift D left through carry
            old_c = self.flag_c
            self.flag_c = (self.d_reg >> 7) & 1
            self.d_reg = ((self.d_reg << 1) | old_c) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xE:  # IRX — R(X) += 1
            self.rx = u64(self.rx + 1)
        elif sub == 0xF:  # LDXA — D ← M(R(X)); R(X) += 1
            self.d_reg = self.mem_read8(self.rx)
            self.rx = u64(self.rx + 1)
        return 0

    # -- 0x9: I/O --
    def _exec_io(self, n: int) -> int:
        if 1 <= n <= 7:      # OUT N
            val = self.mem_read8(self.rx)
            self.port_out[n] = val & 0xFF
            self.rx = u64(self.rx + 1)
            if self.on_output:
                self.on_output(n, val)
        elif 9 <= n <= 15:   # INP (N-8)
            port = n - 8
            val = self.port_in[port] & 0xFF
            self.mem_write8(self.rx, val)
            self.d_reg = val
        return 0

    # -- 0xA: SEP Rn --
    def _exec_sep(self, n: int) -> int:
        self.psel = n
        return 0

    # -- 0xB: SEX Rn --
    def _exec_sex(self, n: int) -> int:
        self.xsel = n
        return 0

    # -- 0xC: MUL/DIV --
    def _exec_muldiv(self, sub: int) -> int:
        byte1 = self.fetch8()
        rd = (byte1 >> 4) & 0xF
        rs = byte1 & 0xF
        a = self.regs[rd]
        b = self.regs[rs]

        if sub == 0x0:    # MUL (signed low)
            r = (s64(a) * s64(b))
            self.regs[rd] = u64(r)
        elif sub == 0x1:  # MULH (signed high)
            r = (s64(a) * s64(b))
            self.regs[rd] = u64(r >> 64)
        elif sub == 0x2:  # UMUL (unsigned low)
            r = a * b
            self.regs[rd] = u64(r)
        elif sub == 0x3:  # UMULH (unsigned high)
            r = a * b
            self.regs[rd] = u64(r >> 64)
        elif sub == 0x4:  # DIV (signed)
            if b == 0 or (s64(a) == -(1 << 63) and s64(b) == -1):
                raise TrapError(IVEC_DIV_ZERO, "Divide by zero or overflow")
            q = int(s64(a) / s64(b))
            # Python's // rounds toward negative infinity; we want truncation
            if (s64(a) < 0) != (s64(b) < 0) and q * s64(b) != s64(a):
                q += 1  # adjust toward zero
            rem = s64(a) - q * s64(b)
            self.regs[rd] = u64(q)
            self.regs[0] = u64(rem)
        elif sub == 0x5:  # UDIV
            if b == 0:
                raise TrapError(IVEC_DIV_ZERO, "Divide by zero")
            self.regs[0] = a % b
            self.regs[rd] = a // b
        elif sub == 0x6:  # MOD (signed)
            if b == 0:
                raise TrapError(IVEC_DIV_ZERO, "Divide by zero")
            q = int(s64(a) / s64(b))
            if (s64(a) < 0) != (s64(b) < 0) and q * s64(b) != s64(a):
                q += 1
            self.regs[rd] = u64(s64(a) - q * s64(b))
        elif sub == 0x7:  # UMOD
            if b == 0:
                raise TrapError(IVEC_DIV_ZERO, "Divide by zero")
            self.regs[rd] = a % b

        # Flag updates for MUL/DIV
        r = self.regs[rd]
        self.flag_z = 1 if r == 0 else 0
        self.flag_n = (r >> 63) & 1
        return 3  # micro-coded, extra cycles

    # -- 0xD: CSR --
    def _exec_csr(self, n: int) -> int:
        w_bit = (n >> 3) & 1
        reg_lo = n & 0x7  # low 3 bits of register selector
        byte1 = self.fetch8()  # CSR address
        # Register index: for simplicity, use reg_lo directly
        # (the full register index could combine with byte1 bits, but
        #  we keep it simple: reg_lo = the GPR index 0-7, or if the
        #  assembler packs it into the low nibble differently, adjust)
        rn = reg_lo

        if w_bit == 0:  # CSRR: Rd ← CSR[addr]
            self.regs[rn] = u64(self.csr_read(byte1))
        else:           # CSRW: CSR[addr] ← Rs
            self.csr_write(byte1, self.regs[rn])
        return 0

    # -- 0xE: MEX --
    def _exec_mex(self, n: int) -> int:
        ss = (n >> 2) & 0x3   # operand selector
        op = n & 0x3           # major op: 00=TALU, 01=TMUL, 10=TRED, 11=TSYS

        funct_byte = self.fetch8()
        funct = funct_byte & 0x07  # low 3 bits = sub-function

        # Read broadcast register if SS=01
        broadcast_reg = -1
        if ss == 1:
            broadcast_reg = self.fetch8() & 0xF

        # Element width from TMODE
        ew_bits = self.tmode & 0x3
        elem_bytes = 1 << ew_bits  # 1, 2, 4, or 8
        num_lanes = 64 // elem_bytes
        signed = (self.tmode >> 4) & 1

        # Load source tiles as byte arrays
        def read_tile(addr):
            a = u64(addr) % self.mem_size
            return bytearray(self.mem[a:a+64]) if a+64 <= self.mem_size else bytearray(64)

        def write_tile(addr, data):
            a = u64(addr) % self.mem_size
            if a + 64 <= self.mem_size:
                self.mem[a:a+64] = data[:64]

        def tile_get_elem(tile, lane, eb):
            off = lane * eb
            v = 0
            for i in range(eb):
                v |= tile[off + i] << (8 * i)
            return v

        def tile_set_elem(tile, lane, eb, val):
            off = lane * eb
            for i in range(eb):
                tile[off + i] = (val >> (8 * i)) & 0xFF

        def to_signed(v, eb):
            bits = eb * 8
            if v & (1 << (bits - 1)):
                return v - (1 << bits)
            return v

        # Resolve source tiles based on SS
        src_a = read_tile(self.tsrc0)
        if ss == 0x0:
            src_b = read_tile(self.tsrc1)
        elif ss == 0x1:
            # Broadcast Rn across all lanes
            bval = self.regs[broadcast_reg] if broadcast_reg >= 0 else 0
            src_b = bytearray(64)
            for lane in range(num_lanes):
                tile_set_elem(src_b, lane, elem_bytes, bval & ((1 << (elem_bytes*8)) - 1))
        elif ss == 0x2:
            # imm8 splat — funct_byte IS the immediate
            src_b = src_a
            src_a = bytearray(64)
            for i in range(64):
                src_a[i] = funct_byte
            # For imm8 splat, we don't really use funct as sub-function
            funct = 0  # default to ADD for TALU
        elif ss == 0x3:
            # In-place: dst as src_a
            src_a = read_tile(self.tdst)
            src_b = read_tile(self.tsrc0)

        dst = bytearray(64)

        # Extended Tile ALU (EXT modifier 8 = 0xF8 prefix)
        if self._ext_modifier == 8 and op == 0x0:
            rounding = (self.tmode >> 6) & 1
            for lane in range(num_lanes):
                ea = tile_get_elem(src_a, lane, elem_bytes)
                eb_val = tile_get_elem(src_b, lane, elem_bytes)
                bits = elem_bytes * 8
                mask = (1 << bits) - 1
                shift_amt = eb_val & (bits - 1)  # clamp shift to element width
                if funct == 0:    # VSHR — per-lane right shift
                    if signed:
                        sv = to_signed(ea, elem_bytes)
                        if rounding and shift_amt > 0:
                            # Round-to-nearest: add 0.5 ULP before truncation
                            sv += (1 << (shift_amt - 1))
                        r = (sv >> shift_amt) & mask
                    else:
                        if rounding and shift_amt > 0:
                            ea += (1 << (shift_amt - 1))
                        r = (ea >> shift_amt) & mask
                elif funct == 1:  # VSHL — per-lane left shift
                    r = (ea << shift_amt) & mask
                elif funct == 2:  # VSEL — per-lane select (dst = b ? src0 : src1)
                    # Not yet implemented
                    r = ea
                elif funct == 3:  # VCLZ — count leading zeros
                    if ea == 0:
                        r = bits
                    else:
                        r = bits - ea.bit_length()
                else:
                    r = 0
                tile_set_elem(dst, lane, elem_bytes, r)
            write_tile(self.tdst, dst)
            return 1  # 2 cycles for extended tile ALU

        if op == 0x0:  # TALU
            saturate = (self.tmode >> 5) & 1
            for lane in range(num_lanes):
                ea = tile_get_elem(src_a, lane, elem_bytes)
                eb_val = tile_get_elem(src_b, lane, elem_bytes)
                mask = (1 << (elem_bytes * 8)) - 1
                if funct == 0:    # ADD
                    if saturate:
                        if signed:
                            r = to_signed(ea, elem_bytes) + to_signed(eb_val, elem_bytes)
                            hi = (1 << (elem_bytes * 8 - 1)) - 1
                            lo = -(1 << (elem_bytes * 8 - 1))
                            r = max(lo, min(hi, r)) & mask
                        else:
                            r = ea + eb_val
                            r = min(r, mask)
                    else:
                        r = (ea + eb_val) & mask
                elif funct == 1:  # SUB
                    if saturate:
                        if signed:
                            r = to_signed(ea, elem_bytes) - to_signed(eb_val, elem_bytes)
                            hi = (1 << (elem_bytes * 8 - 1)) - 1
                            lo = -(1 << (elem_bytes * 8 - 1))
                            r = max(lo, min(hi, r)) & mask
                        else:
                            r = ea - eb_val
                            r = max(0, r)
                    else:
                        r = (ea - eb_val) & mask
                elif funct == 2:  # AND
                    r = ea & eb_val
                elif funct == 3:  # OR
                    r = ea | eb_val
                elif funct == 4:  # XOR
                    r = ea ^ eb_val
                elif funct == 5:  # MIN
                    if signed:
                        r = min(to_signed(ea, elem_bytes), to_signed(eb_val, elem_bytes)) & mask
                    else:
                        r = min(ea, eb_val)
                elif funct == 6:  # MAX
                    if signed:
                        r = max(to_signed(ea, elem_bytes), to_signed(eb_val, elem_bytes)) & mask
                    else:
                        r = max(ea, eb_val)
                elif funct == 7:  # ABS
                    if signed:
                        sv = to_signed(ea, elem_bytes)
                        r = abs(sv) & mask
                    else:
                        r = ea
                else:
                    r = 0
                tile_set_elem(dst, lane, elem_bytes, r)
            write_tile(self.tdst, dst)
            return 0

        elif op == 0x1:  # TMUL
            if funct == 0:  # MUL
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    mask = (1 << (elem_bytes * 8)) - 1
                    if signed:
                        r = (to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)) & mask
                    else:
                        r = (ea * eb_val) & mask
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
                return 1  # 2 cycle total

            elif funct == 1:  # DOT
                total = 0
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if signed:
                        total += to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)
                    else:
                        total += ea * eb_val
                # Check ACC_ZERO
                if self.tctrl & 0x2:
                    self.acc = [0, 0, 0, 0]
                    self.tctrl &= ~0x2
                # Accumulate or overwrite
                if self.tctrl & 0x1:  # ACC_ACC
                    old = self.acc[0] | (self.acc[1] << 64) | (self.acc[2] << 128) | (self.acc[3] << 192)
                    total = old + total
                # Store into 256-bit ACC
                mask64 = MASK64
                self.acc[0] = total & mask64
                self.acc[1] = (total >> 64) & mask64
                self.acc[2] = (total >> 128) & mask64
                self.acc[3] = (total >> 192) & mask64
                self.flag_z = 1 if total == 0 else 0
                return 3  # 4 cycles total

            elif funct == 2:  # WMUL — widening multiply
                out_eb = elem_bytes * 2
                out_mask = (1 << (out_eb * 8)) - 1
                # Output has num_lanes elements at double width = 128 bytes
                # Written across TDST and TDST+64
                dst0 = bytearray(64)
                dst1 = bytearray(64)
                elems_per_tile = 64 // out_eb
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if signed:
                        r = (to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)) & out_mask
                    else:
                        r = (ea * eb_val) & out_mask
                    if lane < elems_per_tile:
                        tile_set_elem(dst0, lane, out_eb, r)
                    else:
                        tile_set_elem(dst1, lane - elems_per_tile, out_eb, r)
                write_tile(self.tdst, dst0)
                write_tile(u64(self.tdst + 64), dst1)
                return 2

            elif funct == 3:  # MAC — multiply-accumulate in-place
                existing = read_tile(self.tdst)
                mask = (1 << (elem_bytes * 8)) - 1
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    ec = tile_get_elem(existing, lane, elem_bytes)
                    if signed:
                        r = (to_signed(ec, elem_bytes) + to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)) & mask
                    else:
                        r = (ec + ea * eb_val) & mask
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
                return 2

            elif funct == 4:  # FMA — fused multiply-add (dst = a*b + c where c=dst)
                existing = read_tile(self.tdst)
                mask = (1 << (elem_bytes * 8)) - 1
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    ec = tile_get_elem(existing, lane, elem_bytes)
                    if signed:
                        r = (to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes) + to_signed(ec, elem_bytes)) & mask
                    else:
                        r = (ea * eb_val + ec) & mask
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
                return 2

            elif funct == 5:  # DOTACC — 4-way chunked dot product
                chunk_size = num_lanes // 4
                if self.tctrl & 0x2:  # ACC_ZERO
                    self.acc = [0, 0, 0, 0]
                    self.tctrl &= ~0x2
                mask64 = MASK64
                for k in range(4):
                    dot = 0
                    for lane in range(chunk_size):
                        idx = k * chunk_size + lane
                        ea = tile_get_elem(src_a, idx, elem_bytes)
                        eb_val = tile_get_elem(src_b, idx, elem_bytes)
                        if signed:
                            dot += to_signed(ea, elem_bytes) * to_signed(eb_val, elem_bytes)
                        else:
                            dot += ea * eb_val
                    if self.tctrl & 0x1:  # ACC_ACC
                        self.acc[k] = (self.acc[k] + dot) & mask64
                    else:
                        self.acc[k] = dot & mask64
                self.flag_z = 1 if all(a == 0 for a in self.acc) else 0
                return 3

            return 1

        elif op == 0x2:  # TRED
            tile = src_a
            values = [tile_get_elem(tile, lane, elem_bytes) for lane in range(num_lanes)]
            if signed:
                values_s = [to_signed(v, elem_bytes) for v in values]
            else:
                values_s = values

            result = 0
            if funct == 0:    # SUM
                result = sum(values_s)
            elif funct == 1:  # MIN
                result = min(values_s)
            elif funct == 2:  # MAX
                result = max(values_s)
            elif funct == 3:  # POPCNT
                for v in values:
                    result += bin(v).count('1')
            elif funct == 4:  # L1
                if signed:
                    result = sum(abs(v) for v in values_s)
                else:
                    result = sum(values)
            elif funct == 5:  # SUMSQ
                result = sum(v * v for v in values_s)
            elif funct == 6:  # MINIDX
                best_val = values_s[0]
                best_idx = 0
                for i in range(1, num_lanes):
                    if values_s[i] < best_val:
                        best_val = values_s[i]
                        best_idx = i
                # ACC0 = index, ACC1 = value
                mask64 = MASK64
                if self.tctrl & 0x2:  # ACC_ZERO
                    self.acc = [0, 0, 0, 0]
                    self.tctrl &= ~0x2
                if self.tctrl & 0x1:  # ACC_ACC — compare with running min
                    old_val = self.acc[1]
                    if signed:
                        old_signed = old_val if old_val < (1 << 63) else old_val - (1 << 64)
                        if best_val < old_signed:
                            self.acc[0] = best_idx & mask64
                            self.acc[1] = best_val & mask64
                    else:
                        if (best_val & mask64) < old_val:
                            self.acc[0] = best_idx & mask64
                            self.acc[1] = best_val & mask64
                else:
                    self.acc[0] = best_idx & mask64
                    self.acc[1] = best_val & mask64
                self.flag_z = 1 if self.acc[0] == 0 else 0
                return 0
            elif funct == 7:  # MAXIDX
                best_val = values_s[0]
                best_idx = 0
                for i in range(1, num_lanes):
                    if values_s[i] > best_val:
                        best_val = values_s[i]
                        best_idx = i
                # ACC0 = index, ACC1 = value
                mask64 = MASK64
                if self.tctrl & 0x2:  # ACC_ZERO
                    self.acc = [0, 0, 0, 0]
                    self.tctrl &= ~0x2
                if self.tctrl & 0x1:  # ACC_ACC — compare with running max
                    old_val = self.acc[1]
                    if signed:
                        old_signed = old_val if old_val < (1 << 63) else old_val - (1 << 64)
                        if best_val > old_signed:
                            self.acc[0] = best_idx & mask64
                            self.acc[1] = best_val & mask64
                    else:
                        if (best_val & mask64) > old_val:
                            self.acc[0] = best_idx & mask64
                            self.acc[1] = best_val & mask64
                else:
                    self.acc[0] = best_idx & mask64
                    self.acc[1] = best_val & mask64
                self.flag_z = 1 if self.acc[0] == 0 else 0
                return 0

            # Store to ACC
            if self.tctrl & 0x2:  # ACC_ZERO
                self.acc = [0, 0, 0, 0]
                self.tctrl &= ~0x2
            if self.tctrl & 0x1:  # ACC_ACC
                old = self.acc[0] | (self.acc[1] << 64) | (self.acc[2] << 128) | (self.acc[3] << 192)
                result = old + result

            mask64 = MASK64
            self.acc[0] = result & mask64
            self.acc[1] = (result >> 64) & mask64
            self.acc[2] = (result >> 128) & mask64
            self.acc[3] = (result >> 192) & mask64
            self.flag_z = 1 if result == 0 else 0
            return 0

        elif op == 0x3:  # TSYS
            if funct == 0:  # TRANS (8x8 transpose)
                tile = read_tile(self.tdst)
                out = bytearray(64)
                for r in range(8):
                    for c in range(8):
                        out[c * 8 + r] = tile[r * 8 + c]
                write_tile(self.tdst, out)
            elif funct == 4:  # ZERO
                write_tile(self.tdst, bytearray(64))
            elif funct == 3:  # LOADC — load tile from cursor
                addr = self._tile_cursor_addr()
                tile = read_tile(addr)
                write_tile(self.tdst, tile)
            elif funct == 2:  # MOVBANK
                # Copy tile from TSRC0 bank to TDST bank
                src = read_tile(self.tsrc0)
                write_tile(self.tdst, src)
                return 2
            return 0

        return 0

    # -- Tile cursor helper --
    def _tile_cursor_addr(self) -> int:
        bank_base = self.sb * (4 * 1024 * 1024)  # 4 MiB aperture
        return bank_base + (self.sr * self.sw + self.sc) * 64

    # -- Reset helper --
    def _reset_state(self):
        self.regs = [0] * 16
        self.psel = 3
        self.xsel = 2
        self.spsel = 15
        self.flag_z = self.flag_c = self.flag_n = self.flag_v = 0
        self.flag_p = self.flag_g = self.flag_i = self.flag_s = 0
        self.d_reg = 0
        self.q_out = 0
        self.t_reg = 0
        self.sb = self.sr = self.sc = 0
        self.sw = 1
        self.tmode = self.tctrl = 0
        self.tsrc0 = self.tsrc1 = self.tdst = 0
        self.acc = [0, 0, 0, 0]
        self.ivt_base = 0
        self.ivec_id = 0
        self.irq_ipi = False
        self.halted = False
        self.idle = False
        self._ext_modifier = -1
        # Note: core_id and num_cores are NOT reset — they are hardware-fixed

    # -- Instruction size helper (for SKIP) --

    def _next_instruction_size(self) -> int:
        """Peek at the instruction at PC and return its byte size."""
        b0 = self.mem_read8(self.pc)
        f = (b0 >> 4) & 0xF
        n = b0 & 0xF
        # EXT prefix: 1 byte + size of following instruction
        if f == 0xF:
            b1 = self.mem_read8(u64(self.pc + 1))
            f2 = (b1 >> 4) & 0xF
            n2 = b1 & 0xF
            return 1 + self._family_size(f2, n2, u64(self.pc + 1))
        return self._family_size(f, n, self.pc)

    def _family_size(self, f: int, n: int, addr: int) -> int:
        """Return byte size of an instruction given its family and nibble."""
        if f == 0x0:  # SYS
            if n == 0xD:  return 2  # CALL.L
            return 1
        if f == 0x1:  return 1  # INC
        if f == 0x2:  return 1  # DEC
        if f == 0x3:  return 2  # BR
        if f == 0x4:  return 3  # LBR
        if f == 0x5:  # MEM
            if n == 0xF: return 3  # LD.D with offset
            return 2
        if f == 0x6:  # IMM
            if n == 0x0: return 3  # LDI
            if n == 0x1: return 4  # LHI
            if n in (0x2,0x3,0x4,0x5,0x6,0x7): return 3  # ADDI..SUBI
            if n in (0x8,0x9,0xA,0xB): return 2  # shifts
            if n in (0xC,0xD,0xE,0xF): return 2  # GLO/GHI/PLO/PHI
            return 2
        if f == 0x7:  return 2  # ALU
        if f == 0x8:  return 1  # MEMALU
        if f == 0x9:  return 1  # I/O
        if f == 0xA:  return 1  # SEP
        if f == 0xB:  return 1  # SEX
        if f == 0xC:  return 2  # MUL/DIV
        if f == 0xD:  return 2  # CSR
        if f == 0xE:  # MEX — 2 bytes + optional broadcast reg
            ss = (n >> 2) & 0x3
            return 3 if ss == 1 else 2
        return 1  # fallback

    # -- Run loop --

    def run(self, max_steps: int = 1_000_000) -> int:
        """Run until HALT, IDLE, or max_steps. Returns total cycles."""
        total = 0
        for _ in range(max_steps):
            if self.halted or self.idle:
                break
            try:
                total += self.step()
            except TrapError as e:
                # If IVT is set up, enter trap handler; otherwise propagate
                if self.ivt_base != 0:
                    self._trap(e.ivec_id)
                else:
                    raise
        return total

    # -- Load bytes at address --

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
