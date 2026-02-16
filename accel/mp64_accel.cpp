/*
 * mp64_accel.cpp — C++ accelerated core for Megapad-64 emulator
 *
 * Replaces the Python step() loop with a tight C++ implementation.
 * MMIO accesses and tile-engine FP operations call back to Python.
 *
 * Build: see setup_accel.py (pybind11 extension module)
 */

#include <cstdint>
#include <cstring>
#include <stdexcept>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>

namespace py = pybind11;

// ---------------------------------------------------------------------------
//  Constants — must match megapad64.py exactly
// ---------------------------------------------------------------------------

static constexpr uint64_t MASK64 = 0xFFFFFFFFFFFFFFFFULL;
static constexpr uint64_t SIGN64 = 1ULL << 63;

// Condition codes
enum CC {
    CC_AL=0, CC_EQ, CC_NE, CC_CS, CC_CC, CC_MI, CC_PL, CC_VS,
    CC_VC, CC_GT, CC_LE, CC_BQ, CC_BNQ, CC_SAT, CC_EF, CC_NV
};

// CSR addresses
enum CSR {
    CSR_FLAGS=0x00, CSR_PSEL=0x01, CSR_XSEL=0x02, CSR_SPSEL=0x03,
    CSR_IVT_BASE=0x04, CSR_D=0x05, CSR_DF=0x06, CSR_Q=0x07,
    CSR_T=0x08, CSR_IE=0x09, CSR_PRIV=0x0A,
    CSR_MPU_BASE=0x0B, CSR_MPU_LIMIT=0x0C,
    CSR_SB=0x10, CSR_SR=0x11, CSR_SC=0x12, CSR_SW=0x13,
    CSR_TMODE=0x14, CSR_TCTRL=0x15,
    CSR_TSRC0=0x16, CSR_TSRC1=0x17, CSR_TDST=0x18,
    CSR_ACC0=0x19, CSR_ACC1=0x1A, CSR_ACC2=0x1B, CSR_ACC3=0x1C,
    CSR_COREID=0x20, CSR_NCORES=0x21, CSR_MBOX=0x22, CSR_IPIACK=0x23,
    CSR_IVEC_ID=0x24, CSR_TRAP_ADDR=0x25,
    CSR_MEGAPAD_SZ=0x30, CSR_CPUID=0x31,
    CSR_TSTRIDE_R=0x40, CSR_TSTRIDE_C=0x41,
    CSR_TTILE_H=0x42, CSR_TTILE_W=0x43,
    CSR_BIST_CMD=0x60, CSR_BIST_STATUS=0x61,
    CSR_BIST_FAIL_ADDR=0x62, CSR_BIST_FAIL_DATA=0x63,
    CSR_TILE_SELFTEST=0x64, CSR_TILE_ST_DETAIL=0x65,
    CSR_PERF_CYCLES=0x68, CSR_PERF_STALLS=0x69,
    CSR_PERF_TILEOPS=0x6A, CSR_PERF_EXTMEM=0x6B, CSR_PERF_CTRL=0x6C,
    CSR_ICACHE_CTRL=0x70, CSR_ICACHE_HITS=0x71, CSR_ICACHE_MISSES=0x72,
};

// IVEC IDs
enum IVEC {
    IVEC_RESET=0, IVEC_NMI, IVEC_ILLEGAL_OP, IVEC_ALIGN_FAULT,
    IVEC_DIV_ZERO, IVEC_BUS_FAULT, IVEC_SW_TRAP, IVEC_TIMER, IVEC_IPI,
    IVEC_PRIV_FAULT = 15
};

// Tile EW codes
enum EW { EW_U8=0, EW_U16, EW_U32, EW_U64, EW_FP16, EW_BF16 };

// ---------------------------------------------------------------------------
//  CPU State — flat struct
// ---------------------------------------------------------------------------

struct CPUState {
    uint64_t regs[16];      // GP registers
    uint8_t  psel;          // PC register index
    uint8_t  xsel;          // X register index
    uint8_t  spsel;         // SP register index

    // Flags (1 bit each, stored as bytes for speed)
    uint8_t flag_z, flag_c, flag_n, flag_v;
    uint8_t flag_p, flag_g, flag_i, flag_s;

    // 1802 legacy
    uint8_t  d_reg;
    uint8_t  q_out;
    uint8_t  t_reg;

    // Tile CSRs
    uint64_t sb, sr, sc, sw;
    uint64_t tmode, tctrl;
    uint64_t tsrc0, tsrc1, tdst;
    uint64_t acc[4];

    // System CSRs
    uint64_t ivt_base;
    uint64_t ivec_id;
    uint64_t trap_addr;

    // External flags
    uint8_t  ef_flags;

    // I/O ports
    uint8_t  port_out[8];
    uint8_t  port_in[8];

    // State
    bool halted;
    bool idle;
    uint64_t cycle_count;

    // Strided tile addressing
    uint64_t tstride_r, tstride_c;
    uint64_t ttile_h, ttile_w;

    // Performance counters
    uint8_t perf_enable;
    uint64_t perf_cycles, perf_stalls, perf_tileops, perf_extmem;

    // BIST
    uint64_t bist_status, bist_fail_addr, bist_fail_data;
    uint64_t tile_selftest, tile_st_detail;

    // I-cache
    uint8_t  icache_enabled;
    uint64_t icache_hits, icache_misses;

    // Privilege level (0=supervisor, 1=user)
    uint8_t  priv_level;

    // MPU — user-mode memory window
    uint64_t mpu_base;   // inclusive lower bound
    uint64_t mpu_limit;  // exclusive upper bound

    // EXT prefix
    int ext_modifier;   // -1 = none

    // Core identity
    uint8_t  core_id;
    uint8_t  num_cores;

    // Memory
    uint8_t* mem;
    uint64_t mem_size;

    // HBW math RAM (banks 1-3, contiguous)
    uint8_t* hbw_mem;
    uint64_t hbw_base;
    uint64_t hbw_size;
};

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

static inline uint64_t u64(uint64_t v) { return v; }  // native 64-bit
static inline int64_t  s64(uint64_t v) { return static_cast<int64_t>(v); }

static inline uint64_t sign_extend(uint64_t val, int bits) {
    uint64_t mask = (1ULL << bits) - 1;
    val &= mask;
    if (val & (1ULL << (bits - 1)))
        val |= ~mask;  // sign extend
    return val;
}

static inline uint8_t parity8(uint64_t val) {
    uint8_t b = val & 0xFF;
    b ^= b >> 4;
    b ^= b >> 2;
    b ^= b >> 1;
    return (b & 1) ^ 1;
}

// ---------------------------------------------------------------------------
//  Trap signaling
// ---------------------------------------------------------------------------

// We use a special return code to signal traps, halts, and MMIO needs
// to the Python layer.
enum StepResult {
    SR_OK = 0,
    SR_HALT = 1,
    SR_TRAP = 2,
    SR_IDLE = 3,
    SR_MMIO_READ = 4,
    SR_MMIO_WRITE = 5,
    SR_OUTPUT = 6,          // OUT port instruction
    SR_MEX_FALLBACK = 7,   // complex MEX op, fall back to Python
};

// ---------------------------------------------------------------------------
//  Memory access — inlined, direct pointer arithmetic
// ---------------------------------------------------------------------------

static inline uint8_t mem_read8(CPUState& s, uint64_t addr) {
    return s.mem[addr % s.mem_size];
}

static inline void mem_write8(CPUState& s, uint64_t addr, uint8_t val) {
    s.mem[addr % s.mem_size] = val;
}

static inline uint16_t mem_read16(CPUState& s, uint64_t addr) {
    uint64_t a = addr % s.mem_size;
    uint16_t v;
    if (__builtin_expect(a + 2 <= s.mem_size, 1))
        std::memcpy(&v, s.mem + a, 2);
    else {
        v = s.mem[a] | (uint16_t(s.mem[(a+1) % s.mem_size]) << 8);
    }
    return v;
}

static inline void mem_write16(CPUState& s, uint64_t addr, uint16_t val) {
    uint64_t a = addr % s.mem_size;
    if (__builtin_expect(a + 2 <= s.mem_size, 1))
        std::memcpy(s.mem + a, &val, 2);
    else {
        s.mem[a] = val & 0xFF;
        s.mem[(a+1) % s.mem_size] = (val >> 8) & 0xFF;
    }
}

static inline uint32_t mem_read32(CPUState& s, uint64_t addr) {
    uint64_t a = addr % s.mem_size;
    uint32_t v;
    if (__builtin_expect(a + 4 <= s.mem_size, 1))
        std::memcpy(&v, s.mem + a, 4);
    else {
        v = 0;
        for (int i = 0; i < 4; i++)
            v |= uint32_t(s.mem[(a+i) % s.mem_size]) << (8*i);
    }
    return v;
}

static inline void mem_write32(CPUState& s, uint64_t addr, uint32_t val) {
    uint64_t a = addr % s.mem_size;
    if (__builtin_expect(a + 4 <= s.mem_size, 1))
        std::memcpy(s.mem + a, &val, 4);
    else {
        for (int i = 0; i < 4; i++)
            s.mem[(a+i) % s.mem_size] = (val >> (8*i)) & 0xFF;
    }
}

static inline uint64_t mem_read64(CPUState& s, uint64_t addr) {
    uint64_t a = addr % s.mem_size;
    uint64_t v;
    if (__builtin_expect(a + 8 <= s.mem_size, 1))
        std::memcpy(&v, s.mem + a, 8);
    else {
        v = 0;
        for (int i = 0; i < 8; i++)
            v |= uint64_t(s.mem[(a+i) % s.mem_size]) << (8*i);
    }
    return v;
}

static inline void mem_write64(CPUState& s, uint64_t addr, uint64_t val) {
    uint64_t a = addr % s.mem_size;
    if (__builtin_expect(a + 8 <= s.mem_size, 1))
        std::memcpy(s.mem + a, &val, 8);
    else {
        for (int i = 0; i < 8; i++)
            s.mem[(a+i) % s.mem_size] = (val >> (8*i)) & 0xFF;
    }
}

// PC via psel
static inline uint64_t& pc(CPUState& s) { return s.regs[s.psel]; }
static inline uint64_t& rx(CPUState& s) { return s.regs[s.xsel]; }
static inline uint64_t& sp(CPUState& s) { return s.regs[s.spsel]; }

static inline uint8_t fetch8(CPUState& s) {
    uint8_t v = mem_read8(s, pc(s));
    pc(s)++;
    return v;
}

static inline void push64(CPUState& s, uint64_t val) {
    sp(s) -= 8;
    mem_write64(s, sp(s), val);
}

static inline uint64_t pop64(CPUState& s) {
    uint64_t val = mem_read64(s, sp(s));
    sp(s) += 8;
    return val;
}

// ---------------------------------------------------------------------------
//  Flags
// ---------------------------------------------------------------------------

static inline uint8_t flags_pack(const CPUState& s) {
    return s.flag_z | (s.flag_c<<1) | (s.flag_n<<2) | (s.flag_v<<3) |
           (s.flag_p<<4) | (s.flag_g<<5) | (s.flag_i<<6) | (s.flag_s<<7);
}

static inline void flags_unpack(CPUState& s, uint8_t val) {
    s.flag_z = (val>>0)&1; s.flag_c = (val>>1)&1;
    s.flag_n = (val>>2)&1; s.flag_v = (val>>3)&1;
    s.flag_p = (val>>4)&1; s.flag_g = (val>>5)&1;
    s.flag_i = (val>>6)&1; s.flag_s = (val>>7)&1;
}

static inline bool eval_cond(const CPUState& s, int cc) {
    switch (cc) {
        case CC_AL: return true;
        case CC_EQ: return s.flag_z == 1;
        case CC_NE: return s.flag_z == 0;
        case CC_CS: return s.flag_c == 1;
        case CC_CC: return s.flag_c == 0;
        case CC_MI: return s.flag_n == 1;
        case CC_PL: return s.flag_n == 0;
        case CC_VS: return s.flag_v == 1;
        case CC_VC: return s.flag_v == 0;
        case CC_GT: return s.flag_g == 1;
        case CC_LE: return s.flag_g == 0;
        case CC_BQ: return s.q_out == 1;
        case CC_BNQ:return s.q_out == 0;
        case CC_SAT:return s.flag_s == 1;
        case CC_EF: return s.ef_flags != 0;
        case CC_NV: return false;
        default:    return false;
    }
}

static inline void update_flags_arith(CPUState& s, uint64_t a, uint64_t b,
                                       uint64_t result, bool is_sub) {
    s.flag_z = (result == 0) ? 1 : 0;
    s.flag_n = (result >> 63) & 1;
    s.flag_p = parity8(result);
    if (is_sub) {
        s.flag_c = (a >= b) ? 1 : 0;
    } else {
        // Detect carry out: unsigned overflow
        s.flag_c = (result < a || result < b) ? 1 : 0;
        // More precise: check if a+b > MASK64
        // Python does: 1 if (a+b) > MASK64
        // In C++, if a+b wraps, result < a
        // But with b potentially modified by carry, let's use __int128
        __uint128_t wide = (__uint128_t)a + (__uint128_t)b;
        s.flag_c = (wide > MASK64) ? 1 : 0;
    }
    int64_t sa = s64(a), sb = s64(b), sr = s64(result);
    if (is_sub) {
        s.flag_v = ((sa >= 0 && sb < 0 && sr < 0) ||
                    (sa < 0 && sb >= 0 && sr >= 0)) ? 1 : 0;
    } else {
        s.flag_v = ((sa >= 0 && sb >= 0 && sr < 0) ||
                    (sa < 0 && sb < 0 && sr >= 0)) ? 1 : 0;
    }
}

static inline void update_flags_logic(CPUState& s, uint64_t result) {
    s.flag_z = (result == 0) ? 1 : 0;
    s.flag_n = (result >> 63) & 1;
    s.flag_p = parity8(result);
    s.flag_c = 0;
    s.flag_v = 0;
}

static inline void update_flags_cmp(CPUState& s, uint64_t a, uint64_t b,
                                     uint64_t result) {
    update_flags_arith(s, a, b, result, true);
    s.flag_g = (a > b) ? 1 : 0;
}

// ---------------------------------------------------------------------------
//  CSR read/write
// ---------------------------------------------------------------------------

static uint64_t csr_read(CPUState& s, int addr) {
    switch (addr) {
        case CSR_FLAGS:     return flags_pack(s);
        case CSR_PSEL:      return s.psel;
        case CSR_XSEL:      return s.xsel;
        case CSR_SPSEL:     return s.spsel;
        case CSR_IVT_BASE:  return s.ivt_base;
        case CSR_D:         return s.d_reg;
        case CSR_DF:        return s.flag_c;
        case CSR_Q:         return s.q_out;
        case CSR_T:         return s.t_reg;
        case CSR_IE:        return s.flag_i;
        case CSR_PRIV:      return s.priv_level;
        case CSR_MPU_BASE:  return s.mpu_base;
        case CSR_MPU_LIMIT: return s.mpu_limit;
        case CSR_SB:        return s.sb;
        case CSR_SR:        return s.sr;
        case CSR_SC:        return s.sc;
        case CSR_SW:        return s.sw;
        case CSR_TMODE:     return s.tmode;
        case CSR_TCTRL:     return s.tctrl;
        case CSR_TSRC0:     return s.tsrc0;
        case CSR_TSRC1:     return s.tsrc1;
        case CSR_TDST:      return s.tdst;
        case CSR_ACC0:      return s.acc[0];
        case CSR_ACC1:      return s.acc[1];
        case CSR_ACC2:      return s.acc[2];
        case CSR_ACC3:      return s.acc[3];
        case CSR_COREID:    return s.core_id;
        case CSR_NCORES:    return s.num_cores;
        case CSR_IVEC_ID:   return s.ivec_id;
        case CSR_TRAP_ADDR: return s.trap_addr;
        case CSR_MEGAPAD_SZ:return 64;
        case CSR_CPUID:     return 0x4D503634;  // "MP64"
        case CSR_TSTRIDE_R: return s.tstride_r;
        case CSR_TTILE_H:   return s.ttile_h;
        case CSR_TTILE_W:   return s.ttile_w;
        case CSR_BIST_STATUS:    return s.bist_status;
        case CSR_BIST_FAIL_ADDR: return s.bist_fail_addr;
        case CSR_BIST_FAIL_DATA: return s.bist_fail_data;
        case CSR_TILE_SELFTEST:  return s.tile_selftest;
        case CSR_TILE_ST_DETAIL: return s.tile_st_detail;
        case CSR_PERF_CYCLES: return s.perf_cycles;
        case CSR_PERF_STALLS: return s.perf_stalls;
        case CSR_PERF_TILEOPS:return s.perf_tileops;
        case CSR_PERF_EXTMEM: return s.perf_extmem;
        case CSR_PERF_CTRL:   return s.perf_enable;
        case CSR_ICACHE_CTRL: return s.icache_enabled;
        case CSR_ICACHE_HITS: return s.icache_hits;
        case CSR_ICACHE_MISSES:return s.icache_misses;
        default: return 0;
    }
}

static void csr_write(CPUState& s, int addr, uint64_t val) {
    switch (addr) {
        case CSR_FLAGS:     flags_unpack(s, val & 0xFF); break;
        case CSR_PSEL:      s.psel = val & 0xF; break;
        case CSR_XSEL:      s.xsel = val & 0xF; break;
        case CSR_SPSEL:     s.spsel = val & 0xF; break;
        case CSR_IVT_BASE:  s.ivt_base = val; break;
        case CSR_D:         s.d_reg = val & 0xFF; break;
        case CSR_DF:        s.flag_c = val & 1; break;
        case CSR_Q:         s.q_out = val & 1; break;
        case CSR_T:         s.t_reg = val & 0xFF; break;
        case CSR_IE:        s.flag_i = val & 1; break;
        case CSR_PRIV:      s.priv_level = val & 1; break;
        case CSR_MPU_BASE:  s.mpu_base = val; break;
        case CSR_MPU_LIMIT: s.mpu_limit = val; break;
        case CSR_SB:        s.sb = val; break;
        case CSR_SR:        s.sr = val; break;
        case CSR_SC:        s.sc = val; break;
        case CSR_SW:        s.sw = val; break;
        case CSR_TMODE:     s.tmode = val; break;
        case CSR_TCTRL:     s.tctrl = val; break;
        case CSR_TSRC0:     s.tsrc0 = val; break;
        case CSR_TSRC1:     s.tsrc1 = val; break;
        case CSR_TDST:      s.tdst = val; break;
        case CSR_ACC0:      s.acc[0] = val; break;
        case CSR_ACC1:      s.acc[1] = val; break;
        case CSR_ACC2:      s.acc[2] = val; break;
        case CSR_ACC3:      s.acc[3] = val; break;
        case CSR_TSTRIDE_R: s.tstride_r = val; break;
        case CSR_TTILE_H:   s.ttile_h = val; break;
        case CSR_TTILE_W:   s.ttile_w = val; break;
        case CSR_BIST_CMD:
            if (val == 1 || val == 2) s.bist_status = 2;  // instant pass
            break;
        case CSR_TILE_SELFTEST:
            if (val == 1) { s.tile_selftest = 2; s.tile_st_detail = 0; }
            break;
        case CSR_PERF_CTRL:
            if (val & 1) s.perf_enable = 1;
            if (val & 2) {
                s.perf_cycles = 0; s.perf_stalls = 0;
                s.perf_tileops = 0; s.perf_extmem = 0;
                s.perf_enable = 1;
            }
            break;
        case CSR_ICACHE_CTRL:
            s.icache_enabled = val & 1;
            if (val & 2) { s.icache_hits = 0; s.icache_misses = 0; s.icache_enabled = 1; }
            break;
        default: break;
    }
}

// ---------------------------------------------------------------------------
//  Trap delivery
// ---------------------------------------------------------------------------

static void do_trap(CPUState& s, int ivec_id) {
    if (s.ivt_base == 0) return;  // no IVT, caller will handle
    push64(s, flags_pack(s) | ((uint64_t)s.priv_level << 8));
    push64(s, pc(s));
    s.flag_i = 0;
    s.priv_level = 0;  // escalate to supervisor
    s.ivec_id = ivec_id;
    uint64_t handler = mem_read64(s, s.ivt_base + ivec_id * 8);
    pc(s) = handler;
}

// ---------------------------------------------------------------------------
//  _next_instruction_size — for SKIP mode
// ---------------------------------------------------------------------------

static int next_instruction_size(CPUState& s) {
    uint8_t peek = mem_read8(s, pc(s));
    int f = (peek >> 4) & 0xF;
    // Estimate: most instructions are 1 or 2 bytes
    switch (f) {
        case 0x0: { // SYS
            int n = peek & 0xF;
            return (n == 0xD) ? 2 : 1;  // CALL.L is 2 bytes
        }
        case 0x1: case 0x2: return 1;  // INC, DEC
        case 0x3: return 2;  // BR + offset
        case 0x4: return 3;  // LBR + 16-bit offset
        case 0x5: { // MEM
            int sub = peek & 0xF;
            return (sub == 0xF) ? 3 : 2;  // LD.D has extra offset byte
        }
        case 0x6: { // IMM
            int sub = peek & 0xF;
            if (sub == 0x0) return 3;  // LDI Rn, imm8
            if (sub <= 0x7) return 3;  // reg + imm8
            return 2;
        }
        case 0x7: return 2;  // ALU
        case 0x8: return 1;  // MEMALU
        case 0x9: return 1;  // I/O
        case 0xA: case 0xB: return 1;  // SEP, SEX
        case 0xC: return 2;  // MULDIV
        case 0xD: return 2;  // CSR
        case 0xE: return 2;  // MEX
        case 0xF: return 1;  // EXT prefix (shouldn't reach here)
        default: return 1;
    }
}

// ---------------------------------------------------------------------------
//  Tile helpers for integer MEX
// ---------------------------------------------------------------------------

static inline uint64_t tile_get_elem(const uint8_t* tile, int lane, int eb) {
    int off = lane * eb;
    uint64_t v = 0;
    for (int i = 0; i < eb; i++)
        v |= (uint64_t)tile[off + i] << (8 * i);
    return v;
}

static inline void tile_set_elem(uint8_t* tile, int lane, int eb, uint64_t val) {
    int off = lane * eb;
    for (int i = 0; i < eb; i++)
        tile[off + i] = (val >> (8 * i)) & 0xFF;
}

static inline int64_t to_signed_eb(uint64_t v, int eb) {
    int bits = eb * 8;
    if (v & (1ULL << (bits - 1)))
        return (int64_t)(v - (1ULL << bits));
    return (int64_t)v;
}

// ---------------------------------------------------------------------------
//  Integer MEX core — handles TALU, TMUL, TRED, TSYS for integer types
//  Returns -1 if we need Python fallback (FP types, LOAD2D/STORE2D, etc.)
// ---------------------------------------------------------------------------

static int exec_mex_integer(CPUState& s, int n) {
    int ss = (n >> 2) & 0x3;
    int op = n & 0x3;

    uint8_t funct_byte = fetch8(s);
    int funct = funct_byte & 0x07;

    int broadcast_reg = -1;
    if (ss == 1)
        broadcast_reg = fetch8(s) & 0xF;

    int ew_bits = s.tmode & 0x7;
    bool is_fp = ew_bits >= EW_FP16;
    if (is_fp) return -1;  // fallback to Python for FP

    int elem_bytes = 1 << ew_bits;
    int num_lanes = 64 / elem_bytes;
    bool is_signed = (s.tmode >> 4) & 1;

    // Read source tiles
    uint8_t src_a[64], src_b[64], dst[64];
    uint64_t a_addr = s.tsrc0 % s.mem_size;
    if (a_addr + 64 <= s.mem_size)
        std::memcpy(src_a, s.mem + a_addr, 64);
    else
        std::memset(src_a, 0, 64);

    if (ss == 0x0) {  // tile-tile
        uint64_t b_addr = s.tsrc1 % s.mem_size;
        if (b_addr + 64 <= s.mem_size)
            std::memcpy(src_b, s.mem + b_addr, 64);
        else
            std::memset(src_b, 0, 64);
    } else if (ss == 0x1) {  // broadcast
        uint64_t bval = (broadcast_reg >= 0) ? s.regs[broadcast_reg] : 0;
        uint64_t mask = (elem_bytes < 8) ? ((1ULL << (elem_bytes*8)) - 1) : MASK64;
        bval &= mask;
        for (int lane = 0; lane < num_lanes; lane++)
            tile_set_elem(src_b, lane, elem_bytes, bval);
    } else if (ss == 0x2) {  // imm8 splat
        std::memcpy(src_b, src_a, 64);
        std::memset(src_a, funct_byte, 64);
        funct = 0;
    } else {  // ss == 3, in-place
        uint64_t d_addr = s.tdst % s.mem_size;
        if (d_addr + 64 <= s.mem_size)
            std::memcpy(src_a, s.mem + d_addr, 64);
        else
            std::memset(src_a, 0, 64);
        uint64_t b_addr = s.tsrc0 % s.mem_size;
        if (b_addr + 64 <= s.mem_size)
            std::memcpy(src_b, s.mem + b_addr, 64);
        else
            std::memset(src_b, 0, 64);
    }

    std::memset(dst, 0, 64);

    // Extended Tile ALU (EXT modifier 8)
    if (s.ext_modifier == 8 && op == 0x0) {
        bool rounding = (s.tmode >> 6) & 1;
        for (int lane = 0; lane < num_lanes; lane++) {
            uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
            uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
            int bits = elem_bytes * 8;
            uint64_t mask = (elem_bytes < 8) ? ((1ULL << bits) - 1) : MASK64;
            int shift_amt = eb_val & (bits - 1);
            uint64_t r = 0;
            if (funct == 0) {  // VSHR
                if (is_signed) {
                    int64_t sv = to_signed_eb(ea, elem_bytes);
                    if (rounding && shift_amt > 0) sv += (1LL << (shift_amt - 1));
                    r = (sv >> shift_amt) & mask;
                } else {
                    uint64_t v = ea;
                    if (rounding && shift_amt > 0) v += (1ULL << (shift_amt - 1));
                    r = (v >> shift_amt) & mask;
                }
            } else if (funct == 1) {  // VSHL
                r = (ea << shift_amt) & mask;
            } else if (funct == 2) {  // VSEL
                r = ea;
            } else if (funct == 3) {  // VCLZ
                if (ea == 0) r = bits;
                else {
                    r = bits;
                    uint64_t tmp = ea;
                    while (tmp) { tmp >>= 1; r--; }
                }
            }
            tile_set_elem(dst, lane, elem_bytes, r);
        }
        uint64_t d_addr = s.tdst % s.mem_size;
        if (d_addr + 64 <= s.mem_size)
            std::memcpy(s.mem + d_addr, dst, 64);
        return 1;
    }

    if (op == 0x0) {  // TALU (integer)
        bool saturate = (s.tmode >> 5) & 1;
        for (int lane = 0; lane < num_lanes; lane++) {
            uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
            uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
            int bits = elem_bytes * 8;
            uint64_t mask = (elem_bytes < 8) ? ((1ULL << bits) - 1) : MASK64;
            uint64_t r = 0;

            switch (funct) {
                case 0: {  // ADD
                    if (saturate) {
                        if (is_signed) {
                            int64_t sum = to_signed_eb(ea, elem_bytes) +
                                          to_signed_eb(eb_val, elem_bytes);
                            int64_t hi = (1LL << (bits-1)) - 1;
                            int64_t lo = -(1LL << (bits-1));
                            if (sum > hi) sum = hi;
                            if (sum < lo) sum = lo;
                            r = sum & mask;
                        } else {
                            uint64_t sum = ea + eb_val;
                            r = (sum > mask) ? mask : sum;
                        }
                    } else {
                        r = (ea + eb_val) & mask;
                    }
                    break;
                }
                case 1: {  // SUB
                    if (saturate) {
                        if (is_signed) {
                            int64_t diff = to_signed_eb(ea, elem_bytes) -
                                           to_signed_eb(eb_val, elem_bytes);
                            int64_t hi = (1LL << (bits-1)) - 1;
                            int64_t lo = -(1LL << (bits-1));
                            if (diff > hi) diff = hi;
                            if (diff < lo) diff = lo;
                            r = diff & mask;
                        } else {
                            int64_t diff = (int64_t)ea - (int64_t)eb_val;
                            r = (diff < 0) ? 0 : diff;
                        }
                    } else {
                        r = (ea - eb_val) & mask;
                    }
                    break;
                }
                case 2: r = ea & eb_val; break;   // AND
                case 3: r = ea | eb_val; break;   // OR
                case 4: r = ea ^ eb_val; break;   // XOR
                case 5: {  // MIN
                    if (is_signed)
                        r = (to_signed_eb(ea, elem_bytes) < to_signed_eb(eb_val, elem_bytes))
                            ? ea : eb_val;
                    else
                        r = (ea < eb_val) ? ea : eb_val;
                    break;
                }
                case 6: {  // MAX
                    if (is_signed)
                        r = (to_signed_eb(ea, elem_bytes) > to_signed_eb(eb_val, elem_bytes))
                            ? ea : eb_val;
                    else
                        r = (ea > eb_val) ? ea : eb_val;
                    break;
                }
                case 7: {  // ABS
                    if (is_signed) {
                        int64_t sv = to_signed_eb(ea, elem_bytes);
                        r = (sv < 0 ? -sv : sv) & mask;
                    } else {
                        r = ea;
                    }
                    break;
                }
            }
            tile_set_elem(dst, lane, elem_bytes, r);
        }
        uint64_t d_addr = s.tdst % s.mem_size;
        if (d_addr + 64 <= s.mem_size)
            std::memcpy(s.mem + d_addr, dst, 64);
        return 0;
    }

    if (op == 0x1) {  // TMUL
        if (funct == 0) {  // MUL (element-wise)
            for (int lane = 0; lane < num_lanes; lane++) {
                uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
                uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
                uint64_t mask = (elem_bytes < 8) ? ((1ULL << (elem_bytes*8)) - 1) : MASK64;
                uint64_t r;
                if (is_signed)
                    r = (to_signed_eb(ea, elem_bytes) * to_signed_eb(eb_val, elem_bytes)) & mask;
                else
                    r = (ea * eb_val) & mask;
                tile_set_elem(dst, lane, elem_bytes, r);
            }
            uint64_t d_addr = s.tdst % s.mem_size;
            if (d_addr + 64 <= s.mem_size)
                std::memcpy(s.mem + d_addr, dst, 64);
            return 0;
        }
        if (funct == 1 || funct == 4) {  // DOT, DOTACC
            // Handle ACC_ZERO (TCTRL bit 1): clear accumulator, one-shot
            if (s.tctrl & 0x2) {
                s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.tctrl &= ~0x2;
            }
            bool dot_acc = (funct == 4) || ((s.tctrl & 0x1) != 0);
            int64_t acc_val = dot_acc ? (int64_t)s.acc[0] : 0;
            for (int lane = 0; lane < num_lanes; lane++) {
                uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
                uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
                if (is_signed)
                    acc_val += to_signed_eb(ea, elem_bytes) * to_signed_eb(eb_val, elem_bytes);
                else
                    acc_val += (int64_t)(ea * eb_val);
            }
            s.acc[0] = (uint64_t)acc_val;
            s.flag_z = (s.acc[0] == 0) ? 1 : 0;
            return 0;
        }
        if (funct == 2 || funct == 3 || funct == 5 || funct == 6) {
            // FMA, MAC, WMUL, MAXIDX, MINIDX — fall back to Python
            return -1;
        }
        return -1;  // unknown funct
    }

    if (op == 0x2) {  // TRED (reductions)
        uint64_t result = 0;

        // Handle ACC_ZERO (TCTRL bit 1): clear accumulator, one-shot
        if (s.tctrl & 0x2) {
            s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
            s.tctrl &= ~0x2;  // clear the one-shot bit
        }
        bool acc_acc = (s.tctrl & 0x1) != 0;  // ACC_ACC is bit 0

        switch (funct) {
            case 0: {  // SUM
                int64_t sum = 0;
                for (int lane = 0; lane < num_lanes; lane++) {
                    uint64_t v = tile_get_elem(src_a, lane, elem_bytes);
                    if (is_signed)
                        sum += to_signed_eb(v, elem_bytes);
                    else
                        sum += (int64_t)v;
                }
                result = (uint64_t)sum;
                break;
            }
            case 1: {  // MIN
                int64_t mn;
                if (is_signed) {
                    mn = to_signed_eb(tile_get_elem(src_a, 0, elem_bytes), elem_bytes);
                    for (int lane = 1; lane < num_lanes; lane++) {
                        int64_t v = to_signed_eb(tile_get_elem(src_a, lane, elem_bytes), elem_bytes);
                        if (v < mn) mn = v;
                    }
                } else {
                    mn = tile_get_elem(src_a, 0, elem_bytes);
                    for (int lane = 1; lane < num_lanes; lane++) {
                        uint64_t v = tile_get_elem(src_a, lane, elem_bytes);
                        if (v < (uint64_t)mn) mn = v;
                    }
                }
                result = (uint64_t)mn;
                break;
            }
            case 2: {  // MAX
                int64_t mx;
                if (is_signed) {
                    mx = to_signed_eb(tile_get_elem(src_a, 0, elem_bytes), elem_bytes);
                    for (int lane = 1; lane < num_lanes; lane++) {
                        int64_t v = to_signed_eb(tile_get_elem(src_a, lane, elem_bytes), elem_bytes);
                        if (v > mx) mx = v;
                    }
                } else {
                    mx = tile_get_elem(src_a, 0, elem_bytes);
                    for (int lane = 1; lane < num_lanes; lane++) {
                        uint64_t v = tile_get_elem(src_a, lane, elem_bytes);
                        if (v > (uint64_t)mx) mx = v;
                    }
                }
                result = (uint64_t)mx;
                break;
            }
            case 3: {  // POPCNT
                uint64_t cnt = 0;
                for (int i = 0; i < 64; i++)
                    cnt += __builtin_popcount(src_a[i]);
                result = cnt;
                break;
            }
            case 4: {  // L1 (sum of absolute values)
                int64_t sum = 0;
                for (int lane = 0; lane < num_lanes; lane++) {
                    uint64_t v = tile_get_elem(src_a, lane, elem_bytes);
                    if (is_signed) {
                        int64_t sv = to_signed_eb(v, elem_bytes);
                        sum += (sv < 0) ? -sv : sv;
                    } else {
                        sum += v;
                    }
                }
                result = (uint64_t)sum;
                break;
            }
            case 5: {  // SUMSQ (sum of squares)
                int64_t sum = 0;
                for (int lane = 0; lane < num_lanes; lane++) {
                    uint64_t v = tile_get_elem(src_a, lane, elem_bytes);
                    if (is_signed) {
                        int64_t sv = to_signed_eb(v, elem_bytes);
                        sum += sv * sv;
                    } else {
                        sum += (int64_t)(v * v);
                    }
                }
                result = (uint64_t)sum;
                break;
            }
            case 6:   // EMIN (element-min index)
            case 7: { // EMAX (element-max index)
                // Fall back for these
                return -1;
            }
        }
        if (acc_acc) {
            // Accumulate with existing ACC
            result += s.acc[0];
        }
        s.acc[0] = result;
        s.flag_z = (result == 0) ? 1 : 0;
        return 0;
    }

    if (op == 0x3) {  // TSYS
        if (s.ext_modifier == 8) {
            // Extended TSYS: LOAD2D / STORE2D — fall back to Python
            return -1;
        }
        switch (funct) {
            case 0: {  // TRANS (8×8 byte transpose)
                for (int r = 0; r < 8; r++)
                    for (int c = 0; c < 8; c++)
                        dst[c * 8 + r] = src_a[r * 8 + c];
                uint64_t d_addr = s.tdst % s.mem_size;
                if (d_addr + 64 <= s.mem_size)
                    std::memcpy(s.mem + d_addr, dst, 64);
                return 1;
            }
            case 1: {  // ZERO
                uint64_t d_addr = s.tdst % s.mem_size;
                if (d_addr + 64 <= s.mem_size)
                    std::memset(s.mem + d_addr, 0, 64);
                return 0;
            }
            case 2: {  // LOADC (cursor load from SB+SR*SW+SC)
                uint64_t base = s.sb + s.sr * s.sw + s.sc;
                base %= s.mem_size;
                if (base + 64 <= s.mem_size) {
                    uint64_t d_addr = s.tdst % s.mem_size;
                    if (d_addr + 64 <= s.mem_size)
                        std::memcpy(s.mem + d_addr, s.mem + base, 64);
                }
                return 0;
            }
            case 3: {  // MOVBANK (move tile from dst to src0)
                uint64_t s_addr = s.tsrc0 % s.mem_size;
                uint64_t d_addr = s.tdst % s.mem_size;
                if (s_addr + 64 <= s.mem_size && d_addr + 64 <= s.mem_size)
                    std::memcpy(s.mem + s_addr, s.mem + d_addr, 64);
                return 0;
            }
            case 4: {  // FILL  (fill dst tile with byte from src_b lane 0)
                uint8_t fill_byte = src_b[0];
                uint64_t d_addr = s.tdst % s.mem_size;
                if (d_addr + 64 <= s.mem_size)
                    std::memset(s.mem + d_addr, fill_byte, 64);
                return 0;
            }
            default:
                return -1;  // unimplemented TSYS
        }
    }

    return -1;  // shouldn't reach
}

// ---------------------------------------------------------------------------
//  Single step — returns cycle count, or throws on trap/halt
//  mmio_read8 / mmio_write8 are Python callbacks for MMIO
//  py_on_output is callback for OUT port instruction
//  py_csr_read/write override CSR access (for IPI patching in system.py)
// ---------------------------------------------------------------------------

struct StepCallbacks {
    std::function<uint8_t(uint64_t)> mmio_read8;
    std::function<void(uint64_t, uint8_t)> mmio_write8;
    std::function<void(int, int)> on_output;   // (port, value)
    // CSR overrides for system-level patching (IPI etc.)
    std::function<uint64_t(int)> csr_read_override;  // returns value, or -1 for default
    uint64_t mmio_start;
    uint64_t mmio_end;
    bool has_mmio;
};

// MPU check — user-mode memory window enforcement
static inline void mpu_check(CPUState& s, uint64_t addr) {
    if (s.priv_level && s.mpu_limit > s.mpu_base) {
        if (addr < s.mpu_base || addr >= s.mpu_limit) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
    }
}

// Memory access with MMIO and HBW intercept
static inline uint8_t sys_read8(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        return cb.mmio_read8(addr);  // MMIO always allowed
    }
    if (s.priv_level) {
        // User mode: block HBW entirely, check MPU for RAM
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
        return s.hbw_mem[addr - s.hbw_base];
    }
    return mem_read8(s, addr);
}

static inline void sys_write8(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint8_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        cb.mmio_write8(addr, val);  // MMIO always allowed
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
        s.hbw_mem[addr - s.hbw_base] = val;
        return;
    }
    mem_write8(s, addr, val);
}

// Wider MMIO/HBW-aware reads/writes
static inline uint64_t sys_read64(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint64_t v = 0;
        for (int i = 0; i < 8; i++)
            v |= (uint64_t)cb.mmio_read8(addr + i) << (8*i);
        return v;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 8 <= s.hbw_base + s.hbw_size) {
        uint64_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 8);
        return v;
    }
    return mem_read64(s, addr);
}

static inline void sys_write64(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint64_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        for (int i = 0; i < 8; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 8 <= s.hbw_base + s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 8);
        return;
    }
    mem_write64(s, addr, val);
}

static inline uint16_t sys_read16(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        return cb.mmio_read8(addr) | ((uint16_t)cb.mmio_read8(addr+1) << 8);
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 2 <= s.hbw_base + s.hbw_size) {
        uint16_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 2);
        return v;
    }
    return mem_read16(s, addr);
}

static inline void sys_write16(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint16_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        cb.mmio_write8(addr, val & 0xFF);
        cb.mmio_write8(addr+1, (val >> 8) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 2 <= s.hbw_base + s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 2);
        return;
    }
    mem_write16(s, addr, val);
}

static inline uint32_t sys_read32(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t v = 0;
        for (int i = 0; i < 4; i++)
            v |= (uint32_t)cb.mmio_read8(addr + i) << (8*i);
        return v;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 4 <= s.hbw_base + s.hbw_size) {
        uint32_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 4);
        return v;
    }
    return mem_read32(s, addr);
}

static inline void sys_write32(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint32_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        for (int i = 0; i < 4; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && addr + 4 <= s.hbw_base + s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 4);
        return;
    }
    mem_write32(s, addr, val);
}

// Push/pop through MMIO-aware writes
static inline void sys_push64(CPUState& s, const StepCallbacks& cb, uint64_t val) {
    sp(s) -= 8;
    sys_write64(s, cb, sp(s), val);
}

static inline uint64_t sys_pop64(CPUState& s, const StepCallbacks& cb) {
    uint64_t val = sys_read64(s, cb, sp(s));
    sp(s) += 8;
    return val;
}

// ---------------------------------------------------------------------------
//  The main step function
// ---------------------------------------------------------------------------

static int step_one(CPUState& s, const StepCallbacks& cb) {
    if (s.halted)
        throw std::runtime_error("HALT");
    if (s.idle) {
        s.cycle_count++;
        return 1;
    }

    uint8_t byte0 = fetch8(s);
    int f = (byte0 >> 4) & 0xF;
    int n = byte0 & 0xF;
    int cycles = 1;

    // EXT prefix
    if (f == 0xF) {
        s.ext_modifier = n;
        byte0 = fetch8(s);
        f = (byte0 >> 4) & 0xF;
        n = byte0 & 0xF;
        cycles++;
        if (f == 0xF)
            throw std::runtime_error("TRAP:ILLEGAL_OP:Double EXT prefix");
    }

    switch (f) {
    case 0x0: {  // SYS
        switch (n) {
            case 0x0: s.idle = true; break;
            case 0x1: break;  // NOP
            case 0x2: s.halted = true; break;
            case 0x3: /* RESET — leave to Python */ throw std::runtime_error("TRAP:RESET"); break;
            case 0x4: {  // RTI
                pc(s) = sys_pop64(s, cb);
                uint64_t saved = sys_pop64(s, cb);
                flags_unpack(s, saved & 0xFF);
                s.priv_level = (saved >> 8) & 1;
                cycles++;
                break;
            }
            case 0x5: {  // RET
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint64_t t = sys_pop64(s, cb) & 0xFF;
                s.xsel = (t >> 4) & 0xF;
                s.psel = t & 0xF;
                s.flag_i = 1;
                cycles++;
                break;
            }
            case 0x6: {  // DIS
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint64_t t = sys_pop64(s, cb) & 0xFF;
                s.xsel = (t >> 4) & 0xF;
                s.psel = t & 0xF;
                s.flag_i = 0;
                cycles++;
                break;
            }
            case 0x7: {  // MARK
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint8_t t = ((s.xsel & 0xF) << 4) | (s.psel & 0xF);
                s.t_reg = t;
                sys_push64(s, cb, t);
                s.xsel = s.psel;
                cycles++;
                break;
            }
            case 0x8:  // SAV
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                sys_write8(s, cb, rx(s), s.t_reg);
                break;
            case 0x9:  // SEQ
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.q_out = 1; break;
            case 0xA:  // REQ
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.q_out = 0; break;
            case 0xB: s.flag_i = 1; break;  // EI
            case 0xC: s.flag_i = 0; break;  // DI
            case 0xD: {  // CALL.L
                uint8_t b1 = fetch8(s);
                int rn = b1 & 0xF;
                uint64_t ret_addr = pc(s);
                sys_push64(s, cb, ret_addr);
                pc(s) = s.regs[rn];
                cycles++;
                break;
            }
            case 0xE: {  // RET.L
                pc(s) = sys_pop64(s, cb);
                cycles++;
                break;
            }
            case 0xF:  // TRAP
                throw std::runtime_error("TRAP:SW_TRAP");
                break;
        }
        break;
    }

    case 0x1:  // INC Rn
        s.regs[n]++;
        break;

    case 0x2:  // DEC Rn
        s.regs[n]--;
        break;

    case 0x3: {  // BR (short branch) / SKIP
        if (s.ext_modifier == 6) {  // SKIP mode
            if (eval_cond(s, n)) {
                int skip = next_instruction_size(s);
                pc(s) += skip;
                cycles++;
            }
        } else {
            uint8_t off_byte = fetch8(s);
            int64_t offset = s64(sign_extend(off_byte, 8));
            if (eval_cond(s, n)) {
                pc(s) += offset;
                cycles++;
            }
        }
        break;
    }

    case 0x4: {  // LBR (long branch)
        uint8_t hi = fetch8(s);
        uint8_t lo = fetch8(s);
        int64_t offset = s64(sign_extend(((uint16_t)hi << 8) | lo, 16));
        if (eval_cond(s, n)) {
            pc(s) += offset;
            cycles++;
        }
        break;
    }

    case 0x5: {  // MEM
        uint8_t b1 = fetch8(s);
        int rd = (b1 >> 4) & 0xF;
        int rs = b1 & 0xF;
        switch (n) {
            case 0x0:   // LDN
                s.regs[rd] = sys_read64(s, cb, s.regs[rs]);
                break;
            case 0x1:   // LDA
                s.regs[rd] = sys_read64(s, cb, s.regs[rs]);
                s.regs[rs] += 8;
                break;
            case 0x2:   // LDX
                s.regs[rd] = sys_read64(s, cb, rx(s));
                break;
            case 0x3:   // LDXA
                s.regs[rd] = sys_read64(s, cb, rx(s));
                rx(s) += 8;
                break;
            case 0x4:   // STR
                sys_write64(s, cb, s.regs[rd], s.regs[rs]);
                break;
            case 0x5:   // STXD
                sys_write64(s, cb, rx(s), s.regs[rd]);
                rx(s) -= 8;
                break;
            case 0x6:   // LD.B
                s.regs[rd] = sys_read8(s, cb, s.regs[rs]);
                break;
            case 0x7:   // ST.B
                sys_write8(s, cb, s.regs[rd], s.regs[rs] & 0xFF);
                break;
            case 0x8:   // LD.H
                s.regs[rd] = sys_read16(s, cb, s.regs[rs]);
                break;
            case 0x9:   // ST.H
                sys_write16(s, cb, s.regs[rd], s.regs[rs] & 0xFFFF);
                break;
            case 0xA:   // LD.W
                s.regs[rd] = sys_read32(s, cb, s.regs[rs]);
                break;
            case 0xB:   // ST.W
                sys_write32(s, cb, s.regs[rd], s.regs[rs] & 0xFFFFFFFF);
                break;
            case 0xC:   // LD.SB
                s.regs[rd] = sign_extend(sys_read8(s, cb, s.regs[rs]), 8);
                break;
            case 0xD:   // LD.SH
                s.regs[rd] = sign_extend(sys_read16(s, cb, s.regs[rs]), 16);
                break;
            case 0xE:   // LD.SW
                s.regs[rd] = sign_extend(sys_read32(s, cb, s.regs[rs]), 32);
                break;
            case 0xF: { // LD.D [Rn+off8]
                uint8_t off_byte = fetch8(s);
                int64_t off = s64(sign_extend(off_byte, 8)) * 8;
                s.regs[rd] = sys_read64(s, cb, s.regs[rs] + off);
                cycles++;
                break;
            }
        }
        break;
    }

    case 0x6: {  // IMM
        uint8_t b1 = fetch8(s);
        int rn = (b1 >> 4) & 0xF;
        switch (n) {
            case 0x0: {  // LDI
                if (s.ext_modifier == 0) {  // EXT.IMM64
                    uint64_t imm = 0;
                    for (int i = 0; i < 8; i++)
                        imm |= (uint64_t)fetch8(s) << (8*i);
                    s.regs[rn] = imm;
                } else {
                    s.regs[rn] = fetch8(s);
                }
                break;
            }
            case 0x1: {  // LHI
                uint8_t lo = fetch8(s);
                uint8_t hi = fetch8(s);
                uint16_t imm16 = lo | ((uint16_t)hi << 8);
                s.regs[rn] = (s.regs[rn] & 0x0000FFFFFFFFFFFFULL) | ((uint64_t)imm16 << 48);
                break;
            }
            case 0x2: {  // ADDI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a + s64(imm);
                update_flags_arith(s, a, imm, result, false);
                s.regs[rn] = result;
                break;
            }
            case 0x3: {  // ANDI
                uint8_t imm = fetch8(s);
                s.regs[rn] &= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x4: {  // ORI
                uint8_t imm = fetch8(s);
                s.regs[rn] |= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x5: {  // XORI
                uint8_t imm = fetch8(s);
                s.regs[rn] ^= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x6: {  // CMPI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a - s64(imm);
                update_flags_cmp(s, a, imm, result);
                break;
            }
            case 0x7: {  // SUBI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a - s64(imm);
                update_flags_arith(s, a, imm, result, true);
                s.regs[rn] = result;
                break;
            }
            case 0x8: {  // LSLI
                int imm4 = b1 & 0xF;
                s.regs[rn] <<= imm4;
                break;
            }
            case 0x9: {  // LSRI
                int imm4 = b1 & 0xF;
                s.regs[rn] >>= imm4;
                break;
            }
            case 0xA: {  // ASRI
                int imm4 = b1 & 0xF;
                s.regs[rn] = (uint64_t)(s64(s.regs[rn]) >> imm4);
                break;
            }
            case 0xB: {  // ROLI
                int imm4 = b1 & 0xF;
                if (imm4) {
                    uint64_t v = s.regs[rn];
                    s.regs[rn] = (v << imm4) | (v >> (64 - imm4));
                }
                break;
            }
            case 0xC:  // GLO
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.d_reg = s.regs[rn] & 0xFF;
                break;
            case 0xD:  // GHI
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.d_reg = (s.regs[rn] >> 8) & 0xFF;
                break;
            case 0xE:  // PLO
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.regs[rn] = (s.regs[rn] & ~0xFFULL) | (s.d_reg & 0xFF);
                break;
            case 0xF:  // PHI
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.regs[rn] = (s.regs[rn] & ~0xFF00ULL) | (((uint64_t)(s.d_reg & 0xFF)) << 8);
                break;
        }
        break;
    }

    case 0x7: {  // ALU
        uint8_t b1 = fetch8(s);
        int rd = (b1 >> 4) & 0xF;
        int rs = b1 & 0xF;
        uint64_t a = s.regs[rd];
        uint64_t b = s.regs[rs];
        switch (n) {
            case 0x0: {  // ADD
                uint64_t r = a + b;
                update_flags_arith(s, a, b, r, false);
                s.regs[rd] = r;
                break;
            }
            case 0x1: {  // ADC
                uint64_t r = a + b + s.flag_c;
                update_flags_arith(s, a, b + s.flag_c, r, false);
                s.regs[rd] = r;
                break;
            }
            case 0x2: {  // SUB
                uint64_t r = a - b;
                update_flags_arith(s, a, b, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0x3: {  // SBB
                uint64_t borrow = 1 - s.flag_c;
                uint64_t r = a - b - borrow;
                update_flags_arith(s, a, b + borrow, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0x4: {  // AND
                uint64_t r = a & b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x5: {  // OR
                uint64_t r = a | b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x6: {  // XOR
                uint64_t r = a ^ b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x7: {  // CMP
                uint64_t r = a - b;
                update_flags_cmp(s, a, b, r);
                break;
            }
            case 0x8:  // MOV
                s.regs[rd] = b;
                break;
            case 0x9: {  // NOT
                s.regs[rd] = ~b;
                update_flags_logic(s, s.regs[rd]);
                break;
            }
            case 0xA: {  // NEG
                uint64_t r = -b;  // wraps naturally for uint64_t
                update_flags_arith(s, 0, b, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0xB: {  // SHL
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (64 - shift)) & 1) : 0;
                uint64_t r = a << shift;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xC: {  // SHR
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (shift - 1)) & 1) : 0;
                uint64_t r = a >> shift;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xD: {  // SAR
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (shift - 1)) & 1) : 0;
                uint64_t r = (uint64_t)(s64(a) >> shift);
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xE: {  // ROL
                int shift = b & 63;
                uint64_t r = shift ? ((a << shift) | (a >> (64 - shift))) : a;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xF: {  // ROR
                int shift = b & 63;
                uint64_t r = shift ? ((a >> shift) | (a << (64 - shift))) : a;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
        }
        break;
    }

    case 0x8: {  // MEMALU
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        uint8_t m;
        switch (n) {
            case 0x0: s.d_reg = sys_read8(s, cb, rx(s)); break;
            case 0x1:
                s.d_reg = (sys_read8(s, cb, rx(s)) | s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x2:
                s.d_reg = (sys_read8(s, cb, rx(s)) & s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x3:
                s.d_reg = (sys_read8(s, cb, rx(s)) ^ s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x4: {  // ADD.X
                m = sys_read8(s, cb, rx(s));
                int result = m + s.d_reg;
                s.flag_c = result > 0xFF;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x5: {  // SD.X
                m = sys_read8(s, cb, rx(s));
                int result = m - s.d_reg;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x6: {  // SHR.D
                s.flag_c = s.d_reg & 1;
                s.d_reg = (s.d_reg >> 1) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x7: {  // SM.X
                m = sys_read8(s, cb, rx(s));
                int result = s.d_reg - m;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x8: {  // ADC.X
                m = sys_read8(s, cb, rx(s));
                int result = m + s.d_reg + s.flag_c;
                s.flag_c = result > 0xFF;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x9: {  // SDB.X
                m = sys_read8(s, cb, rx(s));
                int borrow = 1 - s.flag_c;
                int result = m - s.d_reg - borrow;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xA: {  // SHRC.D
                uint8_t old_c = s.flag_c;
                s.flag_c = s.d_reg & 1;
                s.d_reg = ((old_c << 7) | (s.d_reg >> 1)) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xB: {  // SMB.X
                m = sys_read8(s, cb, rx(s));
                int borrow = 1 - s.flag_c;
                int result = s.d_reg - m - borrow;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xC: {  // SHL.D
                s.flag_c = (s.d_reg >> 7) & 1;
                s.d_reg = (s.d_reg << 1) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xD: {  // SHLC.D
                uint8_t old_c = s.flag_c;
                s.flag_c = (s.d_reg >> 7) & 1;
                s.d_reg = ((s.d_reg << 1) | old_c) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xE:  // IRX
                rx(s)++;
                break;
            case 0xF:  // LDXA
                s.d_reg = sys_read8(s, cb, rx(s));
                rx(s)++;
                break;
        }
        break;
    }

    case 0x9: {  // I/O
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        if (n >= 1 && n <= 7) {  // OUT
            uint8_t val = sys_read8(s, cb, rx(s));
            s.port_out[n] = val;
            rx(s)++;
            if (cb.on_output)
                cb.on_output(n, val);
        } else if (n >= 9 && n <= 15) {  // INP
            int port = n - 8;
            uint8_t val = s.port_in[port];
            sys_write8(s, cb, rx(s), val);
            s.d_reg = val;
        }
        break;
    }

    case 0xA:  // SEP Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.psel = n;
        break;

    case 0xB:  // SEX Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.xsel = n;
        break;

    case 0xC: {  // MULDIV
        uint8_t b1 = fetch8(s);
        int rd = (b1 >> 4) & 0xF;
        int rs = b1 & 0xF;
        uint64_t a = s.regs[rd];
        uint64_t b = s.regs[rs];
        switch (n) {
            case 0x0: {  // MUL signed low
                __int128 r = (__int128)s64(a) * (__int128)s64(b);
                s.regs[rd] = (uint64_t)r;
                break;
            }
            case 0x1: {  // MULH signed high
                __int128 r = (__int128)s64(a) * (__int128)s64(b);
                s.regs[rd] = (uint64_t)(r >> 64);
                break;
            }
            case 0x2: {  // UMUL unsigned low
                __uint128_t r = (__uint128_t)a * (__uint128_t)b;
                s.regs[rd] = (uint64_t)r;
                break;
            }
            case 0x3: {  // UMULH unsigned high
                __uint128_t r = (__uint128_t)a * (__uint128_t)b;
                s.regs[rd] = (uint64_t)(r >> 64);
                break;
            }
            case 0x4: {  // DIV signed
                if (b == 0 || (s64(a) == INT64_MIN && s64(b) == -1))
                    throw std::runtime_error("TRAP:DIV_ZERO");
                int64_t q = s64(a) / s64(b);  // C++ truncates toward zero
                int64_t rem = s64(a) - q * s64(b);
                s.regs[rd] = (uint64_t)q;
                s.regs[0] = (uint64_t)rem;
                break;
            }
            case 0x5: {  // UDIV
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                s.regs[0] = a % b;
                s.regs[rd] = a / b;
                break;
            }
            case 0x6: {  // MOD signed
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                int64_t q = s64(a) / s64(b);
                s.regs[rd] = (uint64_t)(s64(a) - q * s64(b));
                break;
            }
            case 0x7: {  // UMOD
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                s.regs[rd] = a % b;
                break;
            }
        }
        {
            uint64_t r = s.regs[rd];
            s.flag_z = (r == 0) ? 1 : 0;
            s.flag_n = (r >> 63) & 1;
        }
        cycles += 3;
        break;
    }

    case 0xD: {  // CSR
        int w_bit = (n >> 3) & 1;
        int rn = n & 0x7;
        uint8_t csr_addr = fetch8(s);
        if (w_bit == 0) {
            // CSRR — check for system-level override first
            if (cb.csr_read_override) {
                uint64_t v = cb.csr_read_override(csr_addr);
                if (v != (uint64_t)-1) {
                    s.regs[rn] = v;
                    break;
                }
            }
            s.regs[rn] = csr_read(s, csr_addr);
        } else {
            // Privilege check for protected CSR writes
            if (s.priv_level && (csr_addr == CSR_PRIV || csr_addr == CSR_IVT_BASE ||
                                 csr_addr == CSR_IE || csr_addr == CSR_BIST_CMD ||
                                 csr_addr == CSR_ICACHE_CTRL ||
                                 csr_addr == CSR_MPU_BASE || csr_addr == CSR_MPU_LIMIT)) {
                throw std::runtime_error("TRAP:PRIV_FAULT");
            }
            csr_write(s, csr_addr, s.regs[rn]);
        }
        break;
    }

    case 0xE: {  // MEX
        // Rewind fetch: we consumed funct_byte inside step_one,
        // but exec_mex_integer does its own fetch8. So rewind PC by 1
        // (for the funct_byte) — actually, exec_mex_integer calls fetch8
        // itself, so we DON'T rewind. But we need to undo our family
        // fetch. Actually we already consumed byte0 which had f=0xE.
        // exec_mex_integer will fetch the funct_byte itself. So this
        // is okay — we just call it.
        int rc = exec_mex_integer(s, n);
        if (rc < 0) {
            // Need Python fallback — throw special exception
            throw std::runtime_error("MEX_FALLBACK");
        }
        cycles += rc;
        if (s.perf_enable)
            s.perf_tileops++;
        break;
    }

    default:
        break;
    }

    // Clear EXT modifier
    s.ext_modifier = -1;
    s.cycle_count += cycles;

    // Perf counters
    if (s.perf_enable) {
        s.perf_cycles += cycles;
        if ((f == 0x5 || f == 0x8) && cycles > 1)
            s.perf_stalls += cycles - 1;
    }

    return cycles;
}

// ---------------------------------------------------------------------------
//  run_steps — run N steps in C++, calling back to Python for MMIO
// ---------------------------------------------------------------------------

struct RunResult {
    int64_t total_cycles;
    int steps_executed;
    int stop_reason;  // 0=max_steps, 1=halt, 2=idle
};

static RunResult run_steps(CPUState& s, const StepCallbacks& cb, int max_steps) {
    RunResult result = {0, 0, 0};

    for (int i = 0; i < max_steps; i++) {
        if (s.halted) { result.stop_reason = 1; break; }
        if (s.idle) { result.stop_reason = 2; break; }

        try {
            int cycles = step_one(s, cb);
            result.total_cycles += cycles;
            result.steps_executed++;
        } catch (const std::runtime_error& e) {
            std::string what = e.what();
            if (what == "HALT") {
                result.stop_reason = 1;
                break;
            } else if (what.substr(0, 5) == "TRAP:") {
                // Decode trap type and deliver
                if (what == "TRAP:SW_TRAP") {
                    if (s.ivt_base) do_trap(s, IVEC_SW_TRAP);
                    else throw;
                } else if (what == "TRAP:DIV_ZERO") {
                    if (s.ivt_base) do_trap(s, IVEC_DIV_ZERO);
                    else throw;
                } else if (what == "TRAP:ILLEGAL_OP:Double EXT prefix") {
                    if (s.ivt_base) do_trap(s, IVEC_ILLEGAL_OP);
                    else throw;
                } else if (what == "TRAP:RESET") {
                    throw;  // Let Python handle RESET
                } else {
                    throw;  // Unknown trap → Python
                }
                result.steps_executed++;
            } else if (what == "MEX_FALLBACK") {
                // Return to Python to handle this MEX instruction
                // PC was already advanced past the opcode bytes consumed so far;
                // We need to tell Python to re-execute this instruction.
                // Rewind PC to the start of the MEX instruction:
                // byte0 (1) + funct_byte (1) + maybe broadcast_reg (1) = 2 or 3 bytes
                // This is tricky. Instead, use a different approach:
                // throw to Python which handles it
                throw;
            } else {
                throw;
            }
        }
    }
    return result;
}


// ---------------------------------------------------------------------------
//  pybind11 module
// ---------------------------------------------------------------------------

PYBIND11_MODULE(_mp64_accel, m) {
    m.doc() = "C++ accelerated core for Megapad-64 emulator";

    // Expose CPUState
    py::class_<CPUState>(m, "CPUState")
        .def(py::init<>())
        .def_readwrite("psel", &CPUState::psel)
        .def_readwrite("xsel", &CPUState::xsel)
        .def_readwrite("spsel", &CPUState::spsel)
        .def_readwrite("flag_z", &CPUState::flag_z)
        .def_readwrite("flag_c", &CPUState::flag_c)
        .def_readwrite("flag_n", &CPUState::flag_n)
        .def_readwrite("flag_v", &CPUState::flag_v)
        .def_readwrite("flag_p", &CPUState::flag_p)
        .def_readwrite("flag_g", &CPUState::flag_g)
        .def_readwrite("flag_i", &CPUState::flag_i)
        .def_readwrite("flag_s", &CPUState::flag_s)
        .def_readwrite("d_reg", &CPUState::d_reg)
        .def_readwrite("q_out", &CPUState::q_out)
        .def_readwrite("t_reg", &CPUState::t_reg)
        .def_readwrite("sb", &CPUState::sb)
        .def_readwrite("sr", &CPUState::sr)
        .def_readwrite("sc", &CPUState::sc)
        .def_readwrite("sw", &CPUState::sw)
        .def_readwrite("tmode", &CPUState::tmode)
        .def_readwrite("tctrl", &CPUState::tctrl)
        .def_readwrite("tsrc0", &CPUState::tsrc0)
        .def_readwrite("tsrc1", &CPUState::tsrc1)
        .def_readwrite("tdst", &CPUState::tdst)
        .def_readwrite("ivt_base", &CPUState::ivt_base)
        .def_readwrite("ivec_id", &CPUState::ivec_id)
        .def_readwrite("trap_addr", &CPUState::trap_addr)
        .def_readwrite("ef_flags", &CPUState::ef_flags)
        .def_readwrite("halted", &CPUState::halted)
        .def_readwrite("idle", &CPUState::idle)
        .def_readwrite("cycle_count", &CPUState::cycle_count)
        .def_readwrite("tstride_r", &CPUState::tstride_r)
        .def_readwrite("ttile_h", &CPUState::ttile_h)
        .def_readwrite("ttile_w", &CPUState::ttile_w)
        .def_readwrite("perf_enable", &CPUState::perf_enable)
        .def_readwrite("perf_cycles", &CPUState::perf_cycles)
        .def_readwrite("perf_stalls", &CPUState::perf_stalls)
        .def_readwrite("perf_tileops", &CPUState::perf_tileops)
        .def_readwrite("perf_extmem", &CPUState::perf_extmem)
        .def_readwrite("bist_status", &CPUState::bist_status)
        .def_readwrite("bist_fail_addr", &CPUState::bist_fail_addr)
        .def_readwrite("bist_fail_data", &CPUState::bist_fail_data)
        .def_readwrite("tile_selftest", &CPUState::tile_selftest)
        .def_readwrite("tile_st_detail", &CPUState::tile_st_detail)
        .def_readwrite("icache_enabled", &CPUState::icache_enabled)
        .def_readwrite("icache_hits", &CPUState::icache_hits)
        .def_readwrite("icache_misses", &CPUState::icache_misses)
        .def_readwrite("priv_level", &CPUState::priv_level)
        .def_readwrite("mpu_base", &CPUState::mpu_base)
        .def_readwrite("mpu_limit", &CPUState::mpu_limit)
        .def_readwrite("ext_modifier", &CPUState::ext_modifier)
        .def_readwrite("core_id", &CPUState::core_id)
        .def_readwrite("num_cores", &CPUState::num_cores)
        .def_readwrite("mem_size", &CPUState::mem_size)
        // Register access
        .def("get_reg", [](const CPUState& s, int i) { return s.regs[i & 0xF]; })
        .def("set_reg", [](CPUState& s, int i, uint64_t v) { s.regs[i & 0xF] = v; })
        // Accumulator access
        .def("get_acc", [](const CPUState& s, int i) { return s.acc[i & 3]; })
        .def("set_acc", [](CPUState& s, int i, uint64_t v) { s.acc[i & 3] = v; })
        // Port access
        .def("get_port_out", [](const CPUState& s, int i) { return s.port_out[i & 7]; })
        .def("set_port_in", [](CPUState& s, int i, uint8_t v) { s.port_in[i & 7] = v; })
        // Memory attachment
        .def("attach_mem", [](CPUState& s, py::buffer buf, uint64_t size) {
            py::buffer_info info = buf.request(true);  // writable
            s.mem = static_cast<uint8_t*>(info.ptr);
            s.mem_size = size;
        })
        // HBW memory attachment
        .def("attach_hbw_mem", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            py::buffer_info info = buf.request(true);  // writable
            s.hbw_mem = static_cast<uint8_t*>(info.ptr);
            s.hbw_base = base;
            s.hbw_size = size;
        })
        .def_readwrite("hbw_base", &CPUState::hbw_base)
        .def_readwrite("hbw_size", &CPUState::hbw_size)
        // Flags
        .def("flags_pack", [](const CPUState& s) { return flags_pack(s); })
        .def("flags_unpack", [](CPUState& s, uint8_t v) { flags_unpack(s, v); })
        ;

    // Expose RunResult
    py::class_<RunResult>(m, "RunResult")
        .def_readonly("total_cycles", &RunResult::total_cycles)
        .def_readonly("steps_executed", &RunResult::steps_executed)
        .def_readonly("stop_reason", &RunResult::stop_reason)
        ;

    // Single step function
    m.def("step_one", [](CPUState& s,
                          py::function mmio_read8,
                          py::function mmio_write8,
                          py::function on_output,
                          py::object csr_read_override,
                          uint64_t mmio_start,
                          uint64_t mmio_end) -> int {
        StepCallbacks cb;
        cb.mmio_start = mmio_start;
        cb.mmio_end = mmio_end;
        cb.has_mmio = true;
        cb.mmio_read8 = [&](uint64_t addr) -> uint8_t {
            return mmio_read8(addr).cast<uint8_t>();
        };
        cb.mmio_write8 = [&](uint64_t addr, uint8_t val) {
            mmio_write8(addr, val);
        };
        cb.on_output = [&](int port, int val) {
            on_output(port, val);
        };
        if (!csr_read_override.is_none()) {
            auto fn = csr_read_override.cast<py::function>();
            cb.csr_read_override = [fn](int addr) -> uint64_t {
                py::object result = fn(addr);
                if (result.is_none()) return (uint64_t)-1;
                return result.cast<uint64_t>();
            };
        }
        return step_one(s, cb);
    },
    py::arg("state"),
    py::arg("mmio_read8"),
    py::arg("mmio_write8"),
    py::arg("on_output"),
    py::arg("csr_read_override") = py::none(),
    py::arg("mmio_start") = 0xFFFFFF0000000000ULL,
    py::arg("mmio_end")   = 0xFFFFFF8000000000ULL
    );

    // Batch run function (main acceleration entry point)
    m.def("run_steps", [](CPUState& s,
                           py::function mmio_read8,
                           py::function mmio_write8,
                           py::function on_output,
                           py::object csr_read_override,
                           uint64_t mmio_start,
                           uint64_t mmio_end,
                           int max_steps) -> RunResult {
        StepCallbacks cb;
        cb.mmio_start = mmio_start;
        cb.mmio_end = mmio_end;
        cb.has_mmio = true;
        cb.mmio_read8 = [&](uint64_t addr) -> uint8_t {
            return mmio_read8(addr).cast<uint8_t>();
        };
        cb.mmio_write8 = [&](uint64_t addr, uint8_t val) {
            mmio_write8(addr, val);
        };
        cb.on_output = [&](int port, int val) {
            on_output(port, val);
        };
        if (!csr_read_override.is_none()) {
            auto fn = csr_read_override.cast<py::function>();
            cb.csr_read_override = [fn](int addr) -> uint64_t {
                py::object result = fn(addr);
                if (result.is_none()) return (uint64_t)-1;
                return result.cast<uint64_t>();
            };
        }
        return run_steps(s, cb, max_steps);
    },
    py::arg("state"),
    py::arg("mmio_read8"),
    py::arg("mmio_write8"),
    py::arg("on_output"),
    py::arg("csr_read_override") = py::none(),
    py::arg("mmio_start") = 0xFFFFFF0000000000ULL,
    py::arg("mmio_end")   = 0xFFFFFF8000000000ULL,
    py::arg("max_steps") = 1000000
    );
}
