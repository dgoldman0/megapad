/*
 * mp64_accel.cpp — C++ accelerated core for Megapad-64 emulator
 *
 * Replaces the Python step() loop with a tight C++ implementation.
 * MMIO accesses call back to Python; tile-engine FP operations are now
 * handled natively in C++ (FP16/BF16 TALU, TMUL, TRED).
 *
 * Build: see setup_accel.py (pybind11 extension module)
 */

#include <cstdint>
#include <cmath>
#include <cstring>
#include <stdexcept>
#include <unistd.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>
#include <pybind11/numpy.h>

#include "mp64_crypto.h"
#include "mp64_fb.h"
#include "mp64_nic.h"
#include "mp64_timer.h"
#include "mp64_uart_geom.h"

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
    // EXT.CRYPTO CSRs (Appendix B)
    CSR_CRC_ACC=0x80, CSR_CRC_MODE=0x81,
    CSR_SHA_MODE=0x82, CSR_SHA_MSGLEN=0x83, CSR_SHA_MSGLEN_HI=0x84,
    CSR_GF_PRIME_SEL=0x85,
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
    uint64_t regs[32];      // GP registers (R0-R15 base, R16-R31 via REX)
    uint8_t  psel;          // PC register index
    uint8_t  xsel;          // X register index
    uint8_t  spsel;         // SP register index

    // Flags (1 bit each, stored as bytes for speed)
    uint8_t flag_z, flag_c, flag_n, flag_v;
    uint8_t flag_p, flag_g, flag_i, flag_s;

    // 1802 legacy
    uint8_t  d_reg;
    uint8_t  q_out;
    uint16_t t_reg;          // T packs {XSEL[4:0]<<8 | PSEL[4:0]} — needs 10+ bits

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

    // Port I/O bridge remap table — port_map[1..7] = 12-bit MMIO offset
    // 0xFFFF = disabled (legacy port_out/port_in only)
    uint32_t port_map[8];

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

    // EXT.DICT hardware dictionary cache (64 sets × 4 ways)
    static constexpr int DICT_SETS = 64;
    static constexpr int DICT_WAYS = 4;
    static constexpr int DICT_MAX_NAME = 31;
    struct DictEntry {
        bool     valid;
        uint32_t hash;
        uint8_t  name_len;
        uint8_t  name[31];  // max 31 bytes (5-bit length)
        uint64_t xt;
    };
    DictEntry dict_table[64][4];  // zero-initialized by default

    void dict_clear_all() {
        std::memset(dict_table, 0, sizeof(dict_table));
    }

    // EXT.CRYPTO CRC per-core state (Appendix B, §B.3)
    uint64_t crc_acc;   // 64-bit CRC accumulator
    uint8_t  crc_mode;  // 0=CRC32, 1=CRC32C, 2=CRC64

    // EXT.CRYPTO SHA-2 per-core state (Appendix B, §B.4)
    uint8_t  sha_mode;       // 0=SHA-256, 1=SHA-384, 2=SHA-512
    uint64_t sha_msglen_lo;  // total message length in bits (low 64)
    uint64_t sha_msglen_hi;  // total message length in bits (high 64)

    // EXT.CRYPTO Field ALU per-core state (Appendix B, §B.5)
    uint8_t  gf_prime_sel;   // 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom
    BigNum   gf_custom_p;    // 256-bit custom prime
    BigNum   gf_mont_pinv;   // -p^{-1} mod 2^{256}
    BigNum   gf_prev_lo;     // previous result (low 256)
    BigNum   gf_prev_hi;     // previous result (high 256, for MACR)

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

    // External memory (HyperRAM / SDRAM)
    uint8_t* ext_mem;
    uint64_t ext_mem_base;
    uint64_t ext_mem_size;

    // Dedicated VRAM (framebuffer pixel memory)
    uint8_t* vram_mem;
    uint64_t vram_base;
    uint64_t vram_size;

    // C++ native crypto devices (bypass Python MMIO callbacks)
    CryptoDevices crypto;

    // C++ native NIC device (bypass Python MMIO for networking)
    NICDevice nic;

    // C++ native TRNG device (bypass Python MMIO for random bytes)
    TRNGDevice trng;

    // C++ native framebuffer device (bypass Python MMIO for FB registers)
    FramebufferDevice fb;

    // C++ native timer device (bypass Python MMIO for timer polling)
    TimerDevice timer;

    // C++ native UART geometry device (terminal dimensions)
    UartGeomDevice uart_geom;

    // Accelerator hooks — intercept CALL.L to known BIOS word addresses
    static constexpr int MAX_ACCEL_HOOKS = 8;
    struct AccelHookEntry {
        uint64_t addr;
        int      id;    // 1=RECT_FILL, 2=BLIT_GLYPH
    };
    AccelHookEntry accel_hooks[MAX_ACCEL_HOOKS];
    int accel_hook_count = 0;

    void register_accel_hook(uint64_t addr, int hook_id) {
        if (accel_hook_count < MAX_ACCEL_HOOKS) {
            accel_hooks[accel_hook_count++] = {addr, hook_id};
        }
    }
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

// REX prefix helpers — extract register extension bits from ext_modifier.
// ext_modifier values 1-5 are REX prefixes; 0 is EXT.IMM64, 6 is SKIP, -1 is none.
static inline int rex_s(int m) { return (m >= 1 && m <= 5) ? (m & 1) : 0; }
static inline int rex_d(int m) { return (m >= 1 && m <= 5) ? ((m >> 1) & 1) : 0; }
static inline int rex_n(int m) { return (m >= 1 && m <= 5) ? ((m >> 2) & 1) : 0; }

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
//  Memory access — region-aware (VRAM, XMEM, HBW, Bank 0)
// ---------------------------------------------------------------------------
//
// resolve_mem() maps a unified 64-bit address to a host pointer + region size,
// matching the RTL address decode in mp64_memory.v.  All scalar accessors
// route through it so that string instructions (BFILL, BCOPY) and ordinary
// load/stores work correctly across every memory aperture.
//

struct MemRegion {
    uint8_t* buf;
    uint64_t off;
    uint64_t size;
};

static inline MemRegion resolve_mem(CPUState& s, uint64_t addr) {
    if (s.vram_mem && addr >= s.vram_base && addr < s.vram_base + s.vram_size)
        return {s.vram_mem, addr - s.vram_base, s.vram_size};
    if (s.ext_mem && addr >= s.ext_mem_base && addr < s.ext_mem_base + s.ext_mem_size)
        return {s.ext_mem, addr - s.ext_mem_base, s.ext_mem_size};
    if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size)
        return {s.hbw_mem, addr - s.hbw_base, s.hbw_size};
    return {s.mem, addr % s.mem_size, s.mem_size};
}

static inline uint8_t mem_read8(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    return r.buf[r.off];
}

static inline void mem_write8(CPUState& s, uint64_t addr, uint8_t val) {
    auto r = resolve_mem(s, addr);
    r.buf[r.off] = val;
}

static inline uint16_t mem_read16(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint16_t v;
    if (__builtin_expect(r.off + 2 <= r.size, 1))
        std::memcpy(&v, r.buf + r.off, 2);
    else {
        v = r.buf[r.off] | (uint16_t(r.buf[(r.off+1) % r.size]) << 8);
    }
    return v;
}

static inline void mem_write16(CPUState& s, uint64_t addr, uint16_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(r.off + 2 <= r.size, 1))
        std::memcpy(r.buf + r.off, &val, 2);
    else {
        r.buf[r.off] = val & 0xFF;
        r.buf[(r.off+1) % r.size] = (val >> 8) & 0xFF;
    }
}

static inline uint32_t mem_read32(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint32_t v;
    if (__builtin_expect(r.off + 4 <= r.size, 1))
        std::memcpy(&v, r.buf + r.off, 4);
    else {
        v = 0;
        for (int i = 0; i < 4; i++)
            v |= uint32_t(r.buf[(r.off+i) % r.size]) << (8*i);
    }
    return v;
}

static inline void mem_write32(CPUState& s, uint64_t addr, uint32_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(r.off + 4 <= r.size, 1))
        std::memcpy(r.buf + r.off, &val, 4);
    else {
        for (int i = 0; i < 4; i++)
            r.buf[(r.off+i) % r.size] = (val >> (8*i)) & 0xFF;
    }
}

static inline uint64_t mem_read64(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint64_t v;
    if (__builtin_expect(r.off + 8 <= r.size, 1))
        std::memcpy(&v, r.buf + r.off, 8);
    else {
        v = 0;
        for (int i = 0; i < 8; i++)
            v |= uint64_t(r.buf[(r.off+i) % r.size]) << (8*i);
    }
    return v;
}

static inline void mem_write64(CPUState& s, uint64_t addr, uint64_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(r.off + 8 <= r.size, 1))
        std::memcpy(r.buf + r.off, &val, 8);
    else {
        for (int i = 0; i < 8; i++)
            r.buf[(r.off+i) % r.size] = (val >> (8*i)) & 0xFF;
    }
}

// PC via psel
static inline uint64_t& pc(CPUState& s) { return s.regs[s.psel]; }
static inline uint64_t& rx(CPUState& s) { return s.regs[s.xsel]; }
static inline uint64_t& sp(CPUState& s) { return s.regs[s.spsel]; }

// ---------------------------------------------------------------------------
//  Accelerator hook lookup + native implementations
// ---------------------------------------------------------------------------

static inline int find_accel_hook(CPUState& s, uint64_t target) {
    for (int i = 0; i < s.accel_hook_count; i++) {
        if (s.accel_hooks[i].addr == target) return s.accel_hooks[i].id;
    }
    return 0;
}

// Pop one cell from data stack (r14) — direct memory read
static inline uint64_t pop_data(CPUState& s) {
    uint64_t val;
    std::memcpy(&val, s.mem + (s.regs[14] % s.mem_size), 8);
    s.regs[14] += 8;
    return val;
}

// Resolve guest address to host write pointer (VRAM, ext_mem, HBW, or main RAM)
static inline uint8_t* resolve_write_ptr(CPUState& s, uint64_t addr) {
    if (s.vram_mem && addr >= s.vram_base && addr < s.vram_base + s.vram_size)
        return s.vram_mem + (addr - s.vram_base);
    if (s.ext_mem && addr >= s.ext_mem_base && addr < s.ext_mem_base + s.ext_mem_size)
        return s.ext_mem + (addr - s.ext_mem_base);
    if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size)
        return s.hbw_mem + (addr - s.hbw_base);
    if (addr < s.mem_size)
        return s.mem + addr;
    return nullptr;
}

// Fast read for non-MMIO memory (font data is in main RAM or ext_mem)
static inline uint8_t read8_fast(CPUState& s, uint64_t addr) {
    if (addr < s.mem_size) return s.mem[addr];
    if (s.ext_mem && addr >= s.ext_mem_base && addr < s.ext_mem_base + s.ext_mem_size)
        return s.ext_mem[addr - s.ext_mem_base];
    return 0;
}

// RECT-FILL ( addr stride w h color16 -- )
static int accel_rect_fill(CPUState& s) {
    uint16_t color16 = (uint16_t)pop_data(s);
    int64_t  h       = (int64_t)pop_data(s);
    int64_t  w       = (int64_t)pop_data(s);
    int64_t  stride  = (int64_t)pop_data(s);
    uint64_t addr    = pop_data(s);

    if (w <= 0 || h <= 0) return 1;

    for (int64_t row = 0; row < h; row++) {
        uint8_t* dst = resolve_write_ptr(s, addr);
        if (dst) {
            uint16_t* px = reinterpret_cast<uint16_t*>(dst);
            for (int64_t col = 0; col < w; col++)
                px[col] = color16;
        }
        addr += stride;
    }
    return (int)(5 * w * h + 10);  // simulated cycle cost
}

// BLIT-GLYPH ( glyph-addr pixel-addr stride fg16 -- )
static int accel_blit_glyph(CPUState& s) {
    uint16_t fg16       = (uint16_t)pop_data(s);
    int64_t  stride     = (int64_t)pop_data(s);
    uint64_t pixel_addr = pop_data(s);
    uint64_t glyph_addr = pop_data(s);

    if (glyph_addr == 0) return 1;

    // Read 8 font bytes from guest memory
    uint8_t font_rows[8];
    for (int i = 0; i < 8; i++)
        font_rows[i] = read8_fast(s, glyph_addr + i);

    // Blit 8x8 glyph — only foreground (set) bits written
    for (int row = 0; row < 8; row++) {
        uint8_t bits = font_rows[row];
        if (bits) {  // skip empty rows entirely
            uint8_t* dst = resolve_write_ptr(s, pixel_addr);
            if (dst) {
                uint16_t* px = reinterpret_cast<uint16_t*>(dst);
                for (int col = 0; col < 8; col++) {
                    if (bits & 0x80) px[col] = fg16;
                    bits <<= 1;
                }
            }
        }
        pixel_addr += stride;
    }
    return 120;  // simulated cycle cost
}

// VRAM-COPY ( src dst stride w h -- )
// Copy a w×h byte rectangle within VRAM.  Overlap-safe (memmove per row).
static int accel_vram_copy(CPUState& s) {
    int64_t  h      = (int64_t)pop_data(s);
    int64_t  w      = (int64_t)pop_data(s);
    int64_t  stride = (int64_t)pop_data(s);
    uint64_t dst    = pop_data(s);
    uint64_t src    = pop_data(s);

    if (w <= 0 || h <= 0) return 1;
    if (src == dst) return 1;

    // Determine copy direction for overlap safety
    bool backward = dst > src;
    uint64_t src_row = backward ? src + (uint64_t)(h - 1) * (uint64_t)stride : src;
    uint64_t dst_row = backward ? dst + (uint64_t)(h - 1) * (uint64_t)stride : dst;

    for (int64_t row = 0; row < h; row++) {
        uint8_t* sp = resolve_write_ptr(s, src_row);
        uint8_t* dp = resolve_write_ptr(s, dst_row);
        if (sp && dp) {
            std::memmove(dp, sp, (size_t)w);
        }
        if (backward) {
            src_row -= stride;
            dst_row -= stride;
        } else {
            src_row += stride;
            dst_row += stride;
        }
    }
    return (int)(3 * w * h + 10);  // simulated cycle cost
}

// BLIT-STRING ( c-addr len pixel-addr stride fg16 font-base -- )
// Render a string of 8×8 glyphs.  Foreground-only (transparent bg).
static int accel_blit_string(CPUState& s) {
    uint64_t font_base  = pop_data(s);
    uint16_t fg16       = (uint16_t)pop_data(s);
    int64_t  stride     = (int64_t)pop_data(s);
    uint64_t pixel_addr = pop_data(s);
    int64_t  len        = (int64_t)pop_data(s);
    uint64_t c_addr     = pop_data(s);

    if (len <= 0) return 1;

    for (int64_t i = 0; i < len; i++) {
        uint8_t ch = read8_fast(s, c_addr + i);
        if (ch < 0x20) ch = 0x20;
        uint64_t glyph_addr = font_base + (uint64_t)(ch - 0x20) * 8;

        // Read 8 font bytes
        uint8_t font_rows[8];
        for (int r = 0; r < 8; r++)
            font_rows[r] = read8_fast(s, glyph_addr + r);

        // Blit 8×8 glyph
        uint64_t pa = pixel_addr;
        for (int row = 0; row < 8; row++) {
            uint8_t bits = font_rows[row];
            if (bits) {
                uint8_t* dst = resolve_write_ptr(s, pa);
                if (dst) {
                    uint16_t* px = reinterpret_cast<uint16_t*>(dst);
                    for (int col = 0; col < 8; col++) {
                        if (bits & 0x80) px[col] = fg16;
                        bits <<= 1;
                    }
                }
            }
            pa += stride;
        }
        pixel_addr += 16;  // advance 8 pixels × 2 bytes
    }
    return (int)(120 * len + 10);  // simulated cycle cost
}

static int execute_accel_hook(CPUState& s, int hook_id) {
    switch (hook_id) {
        case 1: return accel_rect_fill(s);
        case 2: return accel_blit_glyph(s);
        case 3: return accel_vram_copy(s);
        case 4: return accel_blit_string(s);
        default: return 0;
    }
}

static inline uint8_t fetch8(CPUState& s) {
    uint64_t a = pc(s);
    uint8_t v;
    if (__builtin_expect(s.ext_mem != nullptr && a >= s.ext_mem_base
                         && a < s.ext_mem_base + s.ext_mem_size, 0)) {
        v = s.ext_mem[a - s.ext_mem_base];
    } else {
        v = s.mem[a % s.mem_size];
    }
    pc(s) = a + 1;
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
        case CSR_CRC_ACC:     return s.crc_acc;
        case CSR_CRC_MODE:    return s.crc_mode;
        case CSR_SHA_MODE:    return s.sha_mode;
        case CSR_SHA_MSGLEN:  return s.sha_msglen_lo;
        case CSR_SHA_MSGLEN_HI: return s.sha_msglen_hi;
        case CSR_GF_PRIME_SEL:return s.gf_prime_sel;
        default: return 0;
    }
}

static void csr_write(CPUState& s, int addr, uint64_t val) {
    switch (addr) {
        case CSR_FLAGS:     flags_unpack(s, val & 0xFF); break;
        case CSR_PSEL:      s.psel = val & 0x1F; break;
        case CSR_XSEL:      s.xsel = val & 0x1F; break;
        case CSR_SPSEL:     s.spsel = val & 0x1F; break;
        case CSR_IVT_BASE:  s.ivt_base = val; break;
        case CSR_D:         s.d_reg = val & 0xFF; break;
        case CSR_DF:        s.flag_c = val & 1; break;
        case CSR_Q:         s.q_out = val & 1; break;
        case CSR_T:         s.t_reg = val & 0xFFFF; break;
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
        case CSR_CRC_ACC:  s.crc_acc = val; break;
        case CSR_CRC_MODE: s.crc_mode = val & 0x03; break;
        case CSR_SHA_MODE: s.sha_mode = val & 0x03; break;
        case CSR_SHA_MSGLEN: s.sha_msglen_lo = val; break;
        case CSR_SHA_MSGLEN_HI: s.sha_msglen_hi = val; break;
        case CSR_GF_PRIME_SEL:  s.gf_prime_sel = val & 0x03; break;
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
    s.idle = false;     // interrupt wakes CPU from idle
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
        case 0xC: {
            int sub = peek & 0xF;
            return (sub == 0xE) ? 3 : 2;  // MULDIV; RORI is 3 bytes
        }
        case 0xD: return 2;  // CSR
        case 0xE: return 2;  // MEX
        case 0xF: return 1;  // EXT prefix (shouldn't reach here)
        default: return 1;
    }
}

// ---------------------------------------------------------------------------
//  FP16 / BF16 conversion helpers (matches megapad64.py _fp16_to_float etc.)
// ---------------------------------------------------------------------------

static inline float fp16_to_float(uint16_t h) {
    uint32_t sign = (h >> 15) & 1;
    uint32_t exp  = (h >> 10) & 0x1F;
    uint32_t frac = h & 0x3FF;
    if (exp == 0) {
        if (frac == 0) {
            // ±0
            uint32_t bits = sign << 31;
            float f; std::memcpy(&f, &bits, 4); return f;
        }
        // Subnormal → normalise
        float val = ldexpf((float)frac / 1024.0f, -14);
        return sign ? -val : val;
    }
    if (exp == 0x1F) {
        if (frac == 0) {
            uint32_t bits = (sign << 31) | 0x7F800000u;
            float f; std::memcpy(&f, &bits, 4); return f;  // ±inf
        }
        uint32_t bits = (sign << 31) | 0x7FC00000u;  // qNaN
        float f; std::memcpy(&f, &bits, 4); return f;
    }
    float val = ldexpf(1.0f + (float)frac / 1024.0f, (int)exp - 15);
    return sign ? -val : val;
}

static inline uint16_t float_to_fp16(float f) {
    uint32_t bits;
    std::memcpy(&bits, &f, 4);
    uint32_t sign   = (bits >> 31) & 1;
    uint32_t exp32  = (bits >> 23) & 0xFF;
    uint32_t frac32 = bits & 0x7FFFFF;

    // NaN
    if (exp32 == 0xFF && frac32 != 0)
        return 0x7E00;  // qNaN
    // Inf
    if (exp32 == 0xFF)
        return (uint16_t)((sign << 15) | 0x7C00);
    // Zero
    if (exp32 == 0 && frac32 == 0)
        return (uint16_t)(sign << 15);

    int new_exp = (int)exp32 - 127 + 15;
    if (new_exp >= 0x1F)
        return (uint16_t)((sign << 15) | 0x7C00);  // overflow → ±inf
    if (new_exp <= 0) {
        if (new_exp < -10)
            return (uint16_t)(sign << 15);  // underflow → ±0
        // Subnormal
        frac32 |= 0x800000;
        int shift = 1 - new_exp;
        uint32_t round_bit = (frac32 >> (12 + shift)) & 1;
        uint32_t sticky    = (frac32 & ((1u << (12 + shift)) - 1)) ? 1 : 0;
        uint32_t result    = frac32 >> (13 + shift);
        if (round_bit && (sticky || (result & 1)))
            result++;
        return (uint16_t)((sign << 15) | (result & 0x3FF));
    }
    // Normal: round mantissa from 23 bits to 10 bits
    uint32_t round_bit = (frac32 >> 12) & 1;
    uint32_t sticky    = (frac32 & 0xFFF) ? 1 : 0;
    uint32_t frac16    = frac32 >> 13;
    if (round_bit && (sticky || (frac16 & 1))) {
        frac16++;
        if (frac16 >= 0x400) {
            frac16 = 0;
            new_exp++;
            if (new_exp >= 0x1F)
                return (uint16_t)((sign << 15) | 0x7C00);
        }
    }
    return (uint16_t)((sign << 15) | (new_exp << 10) | (frac16 & 0x3FF));
}

static inline float bf16_to_float(uint16_t b) {
    uint32_t bits32 = (uint32_t)b << 16;
    float f; std::memcpy(&f, &bits32, 4); return f;
}

static inline uint16_t float_to_bf16(float f) {
    uint32_t bits;
    std::memcpy(&bits, &f, 4);
    uint32_t round_bit = (bits >> 15) & 1;
    uint32_t sticky    = (bits & 0x7FFF) ? 1 : 0;
    uint32_t result    = bits >> 16;
    if (round_bit && (sticky || (result & 1)))
        result++;
    return (uint16_t)(result & 0xFFFF);
}

static inline float fp_decode(uint16_t raw, int ew) {
    return (ew == EW_FP16) ? fp16_to_float(raw) : bf16_to_float(raw);
}

static inline uint16_t fp_encode(float val, int ew) {
    return (ew == EW_FP16) ? float_to_fp16(val) : float_to_bf16(val);
}

static inline bool fp_is_nan(uint16_t raw, int ew) {
    if (ew == EW_FP16)
        return ((raw >> 10) & 0x1F) == 0x1F && (raw & 0x3FF) != 0;
    else  // BF16
        return ((raw >> 7) & 0xFF) == 0xFF && (raw & 0x7F) != 0;
}

static inline uint32_t fp32_to_bits(float f) {
    uint32_t b; std::memcpy(&b, &f, 4); return b;
}

static inline float bits_to_fp32(uint32_t b) {
    float f; std::memcpy(&f, &b, 4); return f;
}

// ---------------------------------------------------------------------------
//  Tile helpers for MEX
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
//  Unified tile memory access (64-byte reads/writes with address decoding)
// ---------------------------------------------------------------------------

static inline void tile_read_64bytes(CPUState& s, uint64_t addr, uint8_t* out) {
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 64 <= s.vram_size) {
        std::memcpy(out, s.vram_mem + (addr - s.vram_base), 64);
        return;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 64 <= s.ext_mem_size) {
        std::memcpy(out, s.ext_mem + (addr - s.ext_mem_base), 64);
        return;
    }
    if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 64 <= s.hbw_size) {
        std::memcpy(out, s.hbw_mem + (addr - s.hbw_base), 64);
        return;
    }
    uint64_t a = addr % s.mem_size;
    if (a + 64 <= s.mem_size)
        std::memcpy(out, s.mem + a, 64);
    else
        std::memset(out, 0, 64);
}

static inline void tile_write_64bytes(CPUState& s, uint64_t addr, const uint8_t* data) {
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 64 <= s.vram_size) {
        std::memcpy(s.vram_mem + (addr - s.vram_base), data, 64);
        return;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 64 <= s.ext_mem_size) {
        std::memcpy(s.ext_mem + (addr - s.ext_mem_base), data, 64);
        return;
    }
    if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 64 <= s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), data, 64);
        return;
    }
    uint64_t a = addr % s.mem_size;
    if (a + 64 <= s.mem_size)
        std::memcpy(s.mem + a, data, 64);
}

static inline void tile_zero_64bytes(CPUState& s, uint64_t addr) {
    static const uint8_t zeros[64] = {0};
    tile_write_64bytes(s, addr, zeros);
}

static inline void tile_fill_64bytes(CPUState& s, uint64_t addr, uint8_t fill) {
    uint8_t buf[64];
    std::memset(buf, fill, 64);
    tile_write_64bytes(s, addr, buf);
}

// ---------------------------------------------------------------------------
//  MEX core — handles TALU, TMUL, TRED, TSYS for all element types
//  Returns -1 only for unimplemented ops (LOAD2D/STORE2D etc.)
// ---------------------------------------------------------------------------

static int exec_mex(CPUState& s, int n) {
    int ss = (n >> 2) & 0x3;
    int op = n & 0x3;

    uint8_t funct_byte = fetch8(s);
    int funct = funct_byte & 0x07;

    int broadcast_reg = -1;
    if (ss == 1)
        broadcast_reg = fetch8(s) & 0xF;

    int ew_bits = s.tmode & 0x7;
    bool is_fp = ew_bits >= EW_FP16;

    int elem_bytes = is_fp ? 2 : (1 << ew_bits);
    int num_lanes = 64 / elem_bytes;
    bool is_signed = (s.tmode >> 4) & 1;

    // Read source tiles
    uint8_t src_a[64], src_b[64], dst[64];
    tile_read_64bytes(s, s.tsrc0, src_a);

    if (ss == 0x0) {  // tile-tile
        tile_read_64bytes(s, s.tsrc1, src_b);
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
        tile_read_64bytes(s, s.tdst, src_a);
        tile_read_64bytes(s, s.tsrc0, src_b);
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
        tile_write_64bytes(s, s.tdst, dst);
        return 1;
    }

    if (op == 0x0) {  // TALU
        if (is_fp) {
            // ---- Floating-point TALU ----
            uint16_t qnan = (ew_bits == EW_FP16) ? 0x7E00 : 0x7FC0;
            for (int lane = 0; lane < num_lanes; lane++) {
                uint16_t ea  = (uint16_t)tile_get_elem(src_a, lane, 2);
                uint16_t eb_val = (uint16_t)tile_get_elem(src_b, lane, 2);
                uint16_t r = 0;
                switch (funct) {
                    case 2: r = ea & eb_val; break;  // AND — bitwise
                    case 3: r = ea | eb_val; break;  // OR
                    case 4: r = ea ^ eb_val; break;  // XOR
                    case 7: r = ea & 0x7FFF; break;  // ABS — clear sign bit
                    case 5: {  // MIN — NaN-propagating
                        if (fp_is_nan(ea, ew_bits) || fp_is_nan(eb_val, ew_bits))
                            r = qnan;
                        else {
                            float fa = fp_decode(ea, ew_bits);
                            float fb = fp_decode(eb_val, ew_bits);
                            r = fp_encode(fa < fb ? fa : fb, ew_bits);
                        }
                        break;
                    }
                    case 6: {  // MAX — NaN-propagating
                        if (fp_is_nan(ea, ew_bits) || fp_is_nan(eb_val, ew_bits))
                            r = qnan;
                        else {
                            float fa = fp_decode(ea, ew_bits);
                            float fb = fp_decode(eb_val, ew_bits);
                            r = fp_encode(fa > fb ? fa : fb, ew_bits);
                        }
                        break;
                    }
                    default: {  // ADD (0) / SUB (1)
                        float fa = fp_decode(ea, ew_bits);
                        float fb = fp_decode(eb_val, ew_bits);
                        r = fp_encode(funct == 0 ? fa + fb : fa - fb, ew_bits);
                        break;
                    }
                }
                tile_set_elem(dst, lane, 2, r);
            }
            tile_write_64bytes(s, s.tdst, dst);
            return 0;
        }

        // ---- Integer TALU ----
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
        tile_write_64bytes(s, s.tdst, dst);
        return 0;
    }

    if (op == 0x1) {  // TMUL
        if (is_fp) {
            // ---- Floating-point TMUL ----
            if (funct == 0) {  // MUL
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fa * fb, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 1;
            }
            if (funct == 1) {  // DOT — FP16/BF16 → FP32 accumulate
                if (s.tctrl & 0x2) {
                    s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
                    s.tctrl &= ~0x2ULL;
                }
                float total = 0.0f;
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    total += fa * fb;
                }
                if (s.tctrl & 0x1)  // ACC_ACC
                    total += bits_to_fp32((uint32_t)s.acc[0]);
                s.acc[0] = fp32_to_bits(total);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 3;
            }
            if (funct == 2) {  // WMUL — fp16/bf16 → fp32 widening multiply
                uint8_t dst0[64], dst1[64];
                std::memset(dst0, 0, 64);
                std::memset(dst1, 0, 64);
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    uint32_t fp32bits = fp32_to_bits(fa * fb);
                    if (lane < 16)
                        tile_set_elem(dst0, lane, 4, fp32bits);
                    else
                        tile_set_elem(dst1, lane - 16, 4, fp32bits);
                }
                tile_write_64bytes(s, s.tdst, dst0);
                tile_write_64bytes(s, s.tdst + 64, dst1);
                return 2;
            }
            if (funct == 3) {  // MAC — fp mul-accumulate: dst += a*b
                uint8_t existing[64];
                tile_read_64bytes(s, s.tdst, existing);
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    float fc = fp_decode((uint16_t)tile_get_elem(existing, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fc + fa * fb, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 2;
            }
            if (funct == 4) {  // FMA — dst = a*b + dst
                uint8_t existing[64];
                tile_read_64bytes(s, s.tdst, existing);
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    float fc = fp_decode((uint16_t)tile_get_elem(existing, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fa * fb + fc, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 2;
            }
            if (funct == 5) {  // DOTACC — 4-way chunked dot, FP32 accumulate
                int chunk_size = num_lanes / 4;
                if (s.tctrl & 0x2) {
                    s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
                    s.tctrl &= ~0x2ULL;
                }
                for (int k = 0; k < 4; k++) {
                    float dot = 0.0f;
                    for (int lane = 0; lane < chunk_size; lane++) {
                        int idx = k * chunk_size + lane;
                        float fa = fp_decode((uint16_t)tile_get_elem(src_a, idx, 2), ew_bits);
                        float fb = fp_decode((uint16_t)tile_get_elem(src_b, idx, 2), ew_bits);
                        dot += fa * fb;
                    }
                    if (s.tctrl & 0x1)  // ACC_ACC
                        dot += bits_to_fp32((uint32_t)s.acc[k]);
                    s.acc[k] = fp32_to_bits(dot);
                }
                s.flag_z = (s.acc[0] == 0 && s.acc[1] == 0 &&
                            s.acc[2] == 0 && s.acc[3] == 0) ? 1 : 0;
                return 3;
            }
            return 1;  // unknown FP TMUL funct
        }

        // ---- Integer TMUL ----
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
            tile_write_64bytes(s, s.tdst, dst);
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
        // Handle ACC_ZERO (TCTRL bit 1): clear accumulator, one-shot
        if (s.tctrl & 0x2) {
            s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
            s.tctrl &= ~0x2;  // clear the one-shot bit
        }
        bool acc_acc = (s.tctrl & 0x1) != 0;  // ACC_ACC is bit 0

        if (is_fp) {
            // ---- Floating-point TRED ----
            // Decode all lanes
            float fp_vals[32] = {0};
            for (int lane = 0; lane < num_lanes; lane++)
                fp_vals[lane] = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);

            if (funct == 0) {  // SUM — FP32 accumulate
                float total = 0.0f;
                for (int lane = 0; lane < num_lanes; lane++)
                    total += fp_vals[lane];
                if (acc_acc)
                    total += bits_to_fp32((uint32_t)s.acc[0]);
                s.acc[0] = fp32_to_bits(total);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 0;
            }
            if (funct == 1) {  // MIN
                float best = fp_vals[0];
                for (int lane = 1; lane < num_lanes; lane++) {
                    if (!std::isnan(fp_vals[lane]) && (std::isnan(best) || fp_vals[lane] < best))
                        best = fp_vals[lane];
                }
                s.acc[0] = fp32_to_bits(best);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 2) {  // MAX
                float best = fp_vals[0];
                for (int lane = 1; lane < num_lanes; lane++) {
                    if (!std::isnan(fp_vals[lane]) && (std::isnan(best) || fp_vals[lane] > best))
                        best = fp_vals[lane];
                }
                s.acc[0] = fp32_to_bits(best);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 5) {  // SUMSQ — FP32 accumulate
                float total = 0.0f;
                for (int lane = 0; lane < num_lanes; lane++)
                    total += fp_vals[lane] * fp_vals[lane];
                if (acc_acc)
                    total += bits_to_fp32((uint32_t)s.acc[0]);
                s.acc[0] = fp32_to_bits(total);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 0;
            }
            if (funct == 6) {  // MINIDX
                int best_idx = 0;
                float best_val = fp_vals[0];
                for (int i = 1; i < num_lanes; i++) {
                    if (!std::isnan(fp_vals[i]) && (std::isnan(best_val) || fp_vals[i] < best_val)) {
                        best_val = fp_vals[i];
                        best_idx = i;
                    }
                }
                s.acc[0] = (uint64_t)best_idx;
                s.acc[1] = fp32_to_bits(best_val);
                s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 7) {  // MAXIDX
                int best_idx = 0;
                float best_val = fp_vals[0];
                for (int i = 1; i < num_lanes; i++) {
                    if (!std::isnan(fp_vals[i]) && (std::isnan(best_val) || fp_vals[i] > best_val)) {
                        best_val = fp_vals[i];
                        best_idx = i;
                    }
                }
                s.acc[0] = (uint64_t)best_idx;
                s.acc[1] = fp32_to_bits(best_val);
                s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            // POPCNT, L1 on FP bits — fall through to integer path
        }

        // ---- Integer TRED ----
        uint64_t result = 0;

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
                tile_write_64bytes(s, s.tdst, dst);
                return 1;
            }
            case 1: {  // ZERO
                tile_zero_64bytes(s, s.tdst);
                return 0;
            }
            case 2: {  // LOADC (cursor load from SB+SR*SW+SC)
                uint64_t base = s.sb + s.sr * s.sw + s.sc;
                uint8_t tmp[64];
                tile_read_64bytes(s, base, tmp);
                tile_write_64bytes(s, s.tdst, tmp);
                return 0;
            }
            case 3: {  // MOVBANK (move tile from dst to src0)
                uint8_t tmp[64];
                tile_read_64bytes(s, s.tdst, tmp);
                tile_write_64bytes(s, s.tsrc0, tmp);
                return 0;
            }
            case 4: {  // FILL  (fill dst tile with byte from src_b lane 0)
                uint8_t fill_byte = src_b[0];
                tile_fill_64bytes(s, s.tdst, fill_byte);
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
        // Try C++ devices first (no Python callback needed)
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off))
            return s.nic.read8(mmio_off);
        if (s.trng.handles(mmio_off))
            return s.trng.read8(mmio_off);
        if (s.crypto.handles(mmio_off))
            return s.crypto.read8(mmio_off);
        if (s.fb.handles(mmio_off))
            return s.fb.read8(mmio_off);
        if (s.timer.handles(mmio_off))
            return s.timer.read8(mmio_off);
        if (s.uart_geom.handles(mmio_off))
            return s.uart_geom.read8(mmio_off);
        return cb.mmio_read8(addr);  // fallback to Python for other devices
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
    if (s.ext_mem && addr >= s.ext_mem_base && addr < s.ext_mem_base + s.ext_mem_size) {
        return s.ext_mem[addr - s.ext_mem_base];
    }
    if (s.vram_mem && addr >= s.vram_base && addr < s.vram_base + s.vram_size) {
        return s.vram_mem[addr - s.vram_base];
    }
    return mem_read8(s, addr);
}

static inline void sys_write8(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint8_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        // Try C++ devices first
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            s.nic.write8(mmio_off, val);
            return;
        }
        if (s.trng.handles(mmio_off)) {
            s.trng.write8(mmio_off, val);
            return;
        }
        if (s.crypto.handles(mmio_off)) {
            s.crypto.write8(mmio_off, val);
            return;
        }
        if (s.fb.handles(mmio_off)) {
            s.fb.write8(mmio_off, val);
            return;
        }
        if (s.timer.handles(mmio_off)) {
            s.timer.write8(mmio_off, val);
            return;
        }
        if (s.uart_geom.handles(mmio_off)) {
            s.uart_geom.write8(mmio_off, val);
            return;
        }
        cb.mmio_write8(addr, val);  // fallback to Python for other devices
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
    if (s.ext_mem && addr >= s.ext_mem_base && addr < s.ext_mem_base + s.ext_mem_size) {
        s.ext_mem[addr - s.ext_mem_base] = val;
        return;
    }
    if (s.vram_mem && addr >= s.vram_base && addr < s.vram_base + s.vram_size) {
        s.vram_mem[addr - s.vram_base] = val;
        return;
    }
    mem_write8(s, addr, val);
}

// Wider MMIO/HBW-aware reads/writes
static inline uint64_t sys_read64(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.nic.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.trng.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.trng.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.crypto.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.crypto.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.fb.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.fb.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.timer.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.timer.read8(mmio_off + i) << (8*i);
            return v;
        }
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
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 8 <= s.hbw_size) {
        uint64_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 8);
        return v;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 8 <= s.ext_mem_size) {
        uint64_t v;
        std::memcpy(&v, s.ext_mem + (addr - s.ext_mem_base), 8);
        return v;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 8 <= s.vram_size) {
        uint64_t v;
        std::memcpy(&v, s.vram_mem + (addr - s.vram_base), 8);
        return v;
    }
    return mem_read64(s, addr);
}

static inline void sys_write64(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint64_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.nic.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.trng.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.trng.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.crypto.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.crypto.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.fb.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.fb.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.timer.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.timer.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        for (int i = 0; i < 8; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 8 <= s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 8);
        return;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 8 <= s.ext_mem_size) {
        std::memcpy(s.ext_mem + (addr - s.ext_mem_base), &val, 8);
        return;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 8 <= s.vram_size) {
        std::memcpy(s.vram_mem + (addr - s.vram_base), &val, 8);
        return;
    }
    mem_write64(s, addr, val);
}

static inline uint16_t sys_read16(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off))
            return s.crypto.read8(mmio_off) | ((uint16_t)s.crypto.read8(mmio_off+1) << 8);
        if (s.fb.handles(mmio_off))
            return s.fb.read8(mmio_off) | ((uint16_t)s.fb.read8(mmio_off+1) << 8);
        if (s.timer.handles(mmio_off))
            return s.timer.read8(mmio_off) | ((uint16_t)s.timer.read8(mmio_off+1) << 8);
        return cb.mmio_read8(addr) | ((uint16_t)cb.mmio_read8(addr+1) << 8);
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 2 <= s.hbw_size) {
        uint16_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 2);
        return v;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 2 <= s.ext_mem_size) {
        uint16_t v;
        std::memcpy(&v, s.ext_mem + (addr - s.ext_mem_base), 2);
        return v;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 2 <= s.vram_size) {
        uint16_t v;
        std::memcpy(&v, s.vram_mem + (addr - s.vram_base), 2);
        return v;
    }
    return mem_read16(s, addr);
}

static inline void sys_write16(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint16_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            s.crypto.write8(mmio_off, val & 0xFF);
            s.crypto.write8(mmio_off+1, (val >> 8) & 0xFF);
            return;
        }
        if (s.fb.handles(mmio_off)) {
            s.fb.write8(mmio_off, val & 0xFF);
            s.fb.write8(mmio_off+1, (val >> 8) & 0xFF);
            return;
        }
        if (s.timer.handles(mmio_off)) {
            s.timer.write8(mmio_off, val & 0xFF);
            s.timer.write8(mmio_off+1, (val >> 8) & 0xFF);
            return;
        }
        cb.mmio_write8(addr, val & 0xFF);
        cb.mmio_write8(addr+1, (val >> 8) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 2 <= s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 2);
        return;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 2 <= s.ext_mem_size) {
        std::memcpy(s.ext_mem + (addr - s.ext_mem_base), &val, 2);
        return;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 2 <= s.vram_size) {
        std::memcpy(s.vram_mem + (addr - s.vram_base), &val, 2);
        return;
    }
    mem_write16(s, addr, val);
}

static inline uint32_t sys_read32(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.crypto.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.fb.handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.fb.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.timer.handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.timer.read8(mmio_off + i) << (8*i);
            return v;
        }
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
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 4 <= s.hbw_size) {
        uint32_t v;
        std::memcpy(&v, s.hbw_mem + (addr - s.hbw_base), 4);
        return v;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 4 <= s.ext_mem_size) {
        uint32_t v;
        std::memcpy(&v, s.ext_mem + (addr - s.ext_mem_base), 4);
        return v;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 4 <= s.vram_size) {
        uint32_t v;
        std::memcpy(&v, s.vram_mem + (addr - s.vram_base), 4);
        return v;
    }
    return mem_read32(s, addr);
}

static inline void sys_write32(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint32_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.crypto.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.fb.handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.fb.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.timer.handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.timer.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        for (int i = 0; i < 4; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        return;
    }
    if (s.priv_level) {
        if (s.hbw_mem && addr >= s.hbw_base && addr < s.hbw_base + s.hbw_size) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.hbw_mem && addr >= s.hbw_base && (addr - s.hbw_base) + 4 <= s.hbw_size) {
        std::memcpy(s.hbw_mem + (addr - s.hbw_base), &val, 4);
        return;
    }
    if (s.ext_mem && addr >= s.ext_mem_base && (addr - s.ext_mem_base) + 4 <= s.ext_mem_size) {
        std::memcpy(s.ext_mem + (addr - s.ext_mem_base), &val, 4);
        return;
    }
    if (s.vram_mem && addr >= s.vram_base && (addr - s.vram_base) + 4 <= s.vram_size) {
        std::memcpy(s.vram_mem + (addr - s.vram_base), &val, 4);
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
//  EXT.STRING (F9) — native C++ implementation
// ---------------------------------------------------------------------------
//  Sub-ops: 00=CMOVE, 01=CMOVE>, 02=BFILL, 03=BCOMP, 04=BSRCH
//  Encoding: F9 <sub-op> <reg-byte[Rd:4][Rs:4]>
//  REX extends Rd/Rs to R16-R31 via ext_modifier set before entry.

static int exec_string(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op   = fetch8(s);
    uint8_t reg_byte = fetch8(s);
    int rd = (rex_d(s.ext_modifier) << 4) | ((reg_byte >> 4) & 0xF);
    int rs = (rex_s(s.ext_modifier) << 4) | (reg_byte & 0xF);

    switch (sub_op) {

    case 0x00: // CMOVE — forward byte copy, len in R0
    case 0x01: // CMOVE> — backward byte copy, len in R0
    {
        uint64_t src = s.regs[rs];
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[0];
        if (ln > 0) {
            if (sub_op == 0x00) {
                for (uint64_t i = 0; i < ln; i++)
                    sys_write8(s, cb, dst + i, sys_read8(s, cb, src + i));
            } else {
                for (uint64_t i = ln; i-- > 0; )
                    sys_write8(s, cb, dst + i, sys_read8(s, cb, src + i));
            }
        }
        s.regs[rs] = src + ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return (int)ln + 2;
    }

    case 0x02: // BFILL — fill block with D[7:0], len in Rs
    {
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[rs];
        uint8_t  fb  = s.d_reg;
        // Fast path: memset when entirely within one RAM region (not MMIO)
        bool in_mmio = cb.has_mmio && dst >= cb.mmio_start && dst < cb.mmio_end;
        if (!in_mmio) {
            auto r = resolve_mem(s, dst);
            if (r.buf && r.off + ln <= r.size) {
                std::memset(r.buf + r.off, fb, (size_t)ln);
                s.regs[rd] = dst + ln;
                s.regs[rs] = 0;
                return (int)ln + 2;
            }
        }
        for (uint64_t i = 0; i < ln; i++)
            sys_write8(s, cb, dst + i, fb);
        s.regs[rd] = dst + ln;
        s.regs[rs] = 0;
        return (int)ln + 2;
    }

    case 0x03: // BCOMP — byte compare, len in R0
    {
        uint64_t src = s.regs[rs];
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[0];
        int cycles = 2;
        uint64_t remaining = ln;
        for (uint64_t i = 0; i < ln; i++) {
            uint8_t sb = sys_read8(s, cb, src + i);
            uint8_t db = sys_read8(s, cb, dst + i);
            cycles++;
            remaining--;
            if (sb != db) {
                s.flag_z = 0;
                s.flag_g = (db > sb) ? 1 : 0;
                s.regs[rs] = src + i;
                s.regs[rd] = dst + i;
                s.regs[0]  = remaining + 1;
                return cycles;
            }
        }
        // All equal
        s.flag_z = 1;
        s.flag_g = 0;
        s.regs[rs] = src + ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return cycles;
    }

    case 0x04: // BSRCH — search for D[7:0] in block at Rd, len in R0
    {
        uint64_t dst    = s.regs[rd];
        uint64_t ln     = s.regs[0];
        uint8_t  needle = s.d_reg;
        int cycles = 2;
        for (uint64_t i = 0; i < ln; i++) {
            uint8_t b = sys_read8(s, cb, dst + i);
            cycles++;
            if (b == needle) {
                s.flag_z = 1;  // found
                s.regs[rs] = i;          // offset
                s.regs[rd] = dst + i;
                s.regs[0]  = ln - i;
                return cycles;
            }
        }
        // Not found
        s.flag_z = 0;
        s.regs[rs] = ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return cycles;
    }

    default:
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.STRING reserved sub-op");
    }
}

// ---------------------------------------------------------------------------
//  EXT.DICT (FA) — native C++ implementation
// ---------------------------------------------------------------------------
//  Sub-ops: 00=DFIND, 01=DINS, 02=DDEL, 03=DCLR
//  Encoding: FA <sub-op> <reg-byte[Rd:4][Rs:4]>
//  REX extends Rd/Rs to R16-R31 via ext_modifier set before entry.
//
//  Hash table: 64 sets × 4 ways, FNV-1a 32-bit hash.
//  Name is a counted string in memory: 1 byte len (5-bit) + len name bytes.

static inline uint32_t fnv1a_32(const uint8_t* data, size_t len) {
    uint32_t h = 0x811C9DC5u;
    for (size_t i = 0; i < len; i++)
        h = (h ^ data[i]) * 0x01000193u;
    return h;
}

// Read a counted-string from guest memory.  Returns name length and cycles.
static inline int dict_read_name(CPUState& s, const StepCallbacks& cb,
                                  uint64_t addr, uint8_t* out_name,
                                  uint8_t& out_len, uint32_t& out_hash) {
    uint8_t raw_len = sys_read8(s, cb, addr) & 0x1F;  // 5-bit, max 31
    out_len = raw_len;
    for (int i = 0; i < raw_len; i++)
        out_name[i] = sys_read8(s, cb, addr + 1 + i);
    out_hash = fnv1a_32(out_name, raw_len);
    return 2 + raw_len;  // 1 len byte + N name bytes + 1 hash cycle
}

static int exec_dict(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op   = fetch8(s);
    uint8_t reg_byte = fetch8(s);
    int rd = (rex_d(s.ext_modifier) << 4) | ((reg_byte >> 4) & 0xF);
    int rs = (rex_s(s.ext_modifier) << 4) | (reg_byte & 0xF);

    switch (sub_op) {

    case 0x00: { // DFIND — lookup counted-string at R[rs]; R[rd] ← XT if found
        uint64_t addr = s.regs[rs];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = s.dict_table[set_idx][w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                s.regs[rd] = e.xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        s.regs[rd] = 0;
        s.flag_z = 0;
        s.flag_v = 0;
        return cycles;
    }

    case 0x01: { // DINS — insert name at R[rs] with XT from R[rd]
        uint64_t addr = s.regs[rs];
        uint64_t xt   = s.regs[rd];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        auto* ways = s.dict_table[set_idx];
        // Update existing match
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                e.xt = xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        // Insert into first empty way
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (!e.valid) {
                e.valid = true;
                e.hash = h;
                e.name_len = nlen;
                std::memcpy(e.name, name, nlen);
                e.xt = xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        // Set full — overflow
        s.flag_z = 0;
        s.flag_v = 1;
        return cycles;
    }

    case 0x02: { // DDEL — delete entry matching name at R[rs]
        uint64_t addr = s.regs[rs];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        auto* ways = s.dict_table[set_idx];
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                e.valid = false;
                e.hash = 0;
                e.name_len = 0;
                e.xt = 0;
                s.flag_z = 1;
                return cycles;
            }
        }
        s.flag_z = 0;
        return cycles;
    }

    case 0x03: { // DCLR — clear entire hash table
        s.dict_clear_all();
        return 66;  // ~64 cycles for bulk clear
    }

    default:
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.DICT reserved sub-op");
    }
}

// ---------------------------------------------------------------------------
//  EXT.CRYPTO (prefix FB) — per-core crypto ISA instructions
// ---------------------------------------------------------------------------

// CRC polynomials (normal / MSB-first form)
static constexpr uint32_t CRC32_POLY   = 0x04C11DB7u;
static constexpr uint32_t CRC32C_POLY  = 0x1EDC6F41u;
static constexpr uint64_t CRC64_POLY   = 0x42F0E1EBA9EA3693ull;

static inline uint32_t crc_byte_32(uint32_t acc, uint8_t b, uint32_t poly) {
    acc ^= (uint32_t)b << 24;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x80000000u)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

static inline uint64_t crc_byte_64(uint64_t acc, uint8_t b, uint64_t poly) {
    acc ^= (uint64_t)b << 56;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x8000000000000000ull)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

// ---------------------------------------------------------------------------
//  SHA-256 / SHA-512 helpers
// ---------------------------------------------------------------------------

static inline uint32_t rotr32(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }
static inline uint64_t rotr64(uint64_t x, int n) { return (x >> n) | (x << (64 - n)); }

static constexpr uint32_t ISA_SHA256_K[64] = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2,
};

static constexpr uint64_t ISA_SHA512_K[80] = {
    0x428a2f98d728ae22ull,0x7137449123ef65cdull,0xb5c0fbcfec4d3b2full,0xe9b5dba58189dbbcull,
    0x3956c25bf348b538ull,0x59f111f1b605d019ull,0x923f82a4af194f9bull,0xab1c5ed5da6d8118ull,
    0xd807aa98a3030242ull,0x12835b0145706fbeull,0x243185be4ee4b28cull,0x550c7dc3d5ffb4e2ull,
    0x72be5d74f27b896full,0x80deb1fe3b1696b1ull,0x9bdc06a725c71235ull,0xc19bf174cf692694ull,
    0xe49b69c19ef14ad2ull,0xefbe4786384f25e3ull,0x0fc19dc68b8cd5b5ull,0x240ca1cc77ac9c65ull,
    0x2de92c6f592b0275ull,0x4a7484aa6ea6e483ull,0x5cb0a9dcbd41fbd4ull,0x76f988da831153b5ull,
    0x983e5152ee66dfabull,0xa831c66d2db43210ull,0xb00327c898fb213full,0xbf597fc7beef0ee4ull,
    0xc6e00bf33da88fc2ull,0xd5a79147930aa725ull,0x06ca6351e003826full,0x142929670a0e6e70ull,
    0x27b70a8546d22ffcull,0x2e1b21385c26c926ull,0x4d2c6dfc5ac42aedull,0x53380d139d95b3dfull,
    0x650a73548baf63deull,0x766a0abb3c77b2a8ull,0x81c2c92e47edaee6ull,0x92722c851482353bull,
    0xa2bfe8a14cf10364ull,0xa81a664bbc423001ull,0xc24b8b70d0f89791ull,0xc76c51a30654be30ull,
    0xd192e819d6ef5218ull,0xd69906245565a910ull,0xf40e35855771202aull,0x106aa07032bbd1b8ull,
    0x19a4c116b8d2d0c8ull,0x1e376c085141ab53ull,0x2748774cdf8eeb99ull,0x34b0bcb5e19b48a8ull,
    0x391c0cb3c5c95a63ull,0x4ed8aa4ae3418acbull,0x5b9cca4f7763e373ull,0x682e6ff3d6b2b8a3ull,
    0x748f82ee5defb2fcull,0x78a5636f43172f60ull,0x84c87814a1f0ab72ull,0x8cc702081a6439ecull,
    0x90befffa23631e28ull,0xa4506cebde82bde9ull,0xbef9a3f7b2c67915ull,0xc67178f2e372532bull,
    0xca273eceea26619cull,0xd186b8c721c0c207ull,0xeada7dd6cde0eb1eull,0xf57d4f7fee6ed178ull,
    0x06f067aa72176fbaull,0x0a637dc5a2c898a6ull,0x113f9804bef90daeull,0x1b710b35131c471bull,
    0x28db77f523047d84ull,0x32caab7b40c72493ull,0x3c9ebe0a15c9bebcull,0x431d67c49c100d4cull,
    0x4cc5d4becb3e42b6ull,0x597f299cfc657e2aull,0x5fcb6fab3ad6faecull,0x6c44198c4a475817ull,
};

static constexpr uint32_t ISA_SHA256_IV[8] = {
    0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,
    0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19,
};
static constexpr uint64_t ISA_SHA384_IV[8] = {
    0xcbbb9d5dc1059ed8ull,0x629a292a367cd507ull,0x9159015a3070dd17ull,0x152fecd8f70e5939ull,
    0x67332667ffc00b31ull,0x8eb44a8768581511ull,0xdb0c2e0d64f98fa7ull,0x47b5481dbefa4fa4ull,
};
static constexpr uint64_t ISA_SHA512_IV[8] = {
    0x6a09e667f3bcc908ull,0xbb67ae8584caa73bull,0x3c6ef372fe94f82bull,0xa54ff53a5f1d36f1ull,
    0x510e527fade682d1ull,0x9b05688c2b3e6c1full,0x1f83d9abfb41bd6bull,0x5be0cd19137e2179ull,
};

// Unpack 8 hash words from ACC0-ACC3 (+ regs[16-19] for SHA-512)
static void sha_unpack(CPUState& s, uint64_t H[8]) {
    if (s.sha_mode == 0) {
        H[0] = (s.acc[0] >> 32) & 0xFFFFFFFFu; H[1] = s.acc[0] & 0xFFFFFFFFu;
        H[2] = (s.acc[1] >> 32) & 0xFFFFFFFFu; H[3] = s.acc[1] & 0xFFFFFFFFu;
        H[4] = (s.acc[2] >> 32) & 0xFFFFFFFFu; H[5] = s.acc[2] & 0xFFFFFFFFu;
        H[6] = (s.acc[3] >> 32) & 0xFFFFFFFFu; H[7] = s.acc[3] & 0xFFFFFFFFu;
    } else {
        H[0] = s.acc[0]; H[1] = s.acc[1]; H[2] = s.acc[2]; H[3] = s.acc[3];
        H[4] = s.regs[16]; H[5] = s.regs[17]; H[6] = s.regs[18]; H[7] = s.regs[19];
    }
}

// Pack 8 hash words back into ACC0-ACC3 (+ regs[16-19] for SHA-512)
static void sha_pack(CPUState& s, const uint64_t H[8]) {
    if (s.sha_mode == 0) {
        s.acc[0] = ((H[0] & 0xFFFFFFFFu) << 32) | (H[1] & 0xFFFFFFFFu);
        s.acc[1] = ((H[2] & 0xFFFFFFFFu) << 32) | (H[3] & 0xFFFFFFFFu);
        s.acc[2] = ((H[4] & 0xFFFFFFFFu) << 32) | (H[5] & 0xFFFFFFFFu);
        s.acc[3] = ((H[6] & 0xFFFFFFFFu) << 32) | (H[7] & 0xFFFFFFFFu);
    } else {
        s.acc[0] = H[0]; s.acc[1] = H[1]; s.acc[2] = H[2]; s.acc[3] = H[3];
        s.regs[16] = H[4]; s.regs[17] = H[5]; s.regs[18] = H[6]; s.regs[19] = H[7];
    }
}

static int sha_block_size(CPUState& s) { return s.sha_mode >= 1 ? 128 : 64; }

// Read one block from memory at TSRC0
static void sha_read_block(CPUState& s, uint8_t* block) {
    int bsz = sha_block_size(s);
    for (int i = 0; i < bsz; i++)
        block[i] = mem_read8(s, s.tsrc0 + i);
}

// SHA-256 compression (one 64-byte block)
static void sha256_compress(uint64_t H[8], const uint8_t block[64]) {
    uint32_t W[64];
    for (int i = 0; i < 16; i++)
        W[i] = ((uint32_t)block[i*4] << 24) | ((uint32_t)block[i*4+1] << 16) |
               ((uint32_t)block[i*4+2] << 8) | block[i*4+3];
    for (int t = 16; t < 64; t++) {
        uint32_t s0 = rotr32(W[t-15],7) ^ rotr32(W[t-15],18) ^ (W[t-15] >> 3);
        uint32_t s1 = rotr32(W[t-2],17) ^ rotr32(W[t-2],19)  ^ (W[t-2] >> 10);
        W[t] = W[t-16] + s0 + W[t-7] + s1;
    }
    uint32_t a=(uint32_t)H[0], b=(uint32_t)H[1], c=(uint32_t)H[2], d=(uint32_t)H[3];
    uint32_t e=(uint32_t)H[4], f=(uint32_t)H[5], g=(uint32_t)H[6], h=(uint32_t)H[7];
    for (int t = 0; t < 64; t++) {
        uint32_t S1 = rotr32(e,6) ^ rotr32(e,11) ^ rotr32(e,25);
        uint32_t ch = (e & f) ^ ((~e) & g);
        uint32_t temp1 = h + S1 + ch + ISA_SHA256_K[t] + W[t];
        uint32_t S0 = rotr32(a,2) ^ rotr32(a,13) ^ rotr32(a,22);
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + maj;
        h=g; g=f; f=e; e=d+temp1; d=c; c=b; b=a; a=temp1+temp2;
    }
    H[0] = ((uint32_t)H[0]+a) & 0xFFFFFFFFu;
    H[1] = ((uint32_t)H[1]+b) & 0xFFFFFFFFu;
    H[2] = ((uint32_t)H[2]+c) & 0xFFFFFFFFu;
    H[3] = ((uint32_t)H[3]+d) & 0xFFFFFFFFu;
    H[4] = ((uint32_t)H[4]+e) & 0xFFFFFFFFu;
    H[5] = ((uint32_t)H[5]+f) & 0xFFFFFFFFu;
    H[6] = ((uint32_t)H[6]+g) & 0xFFFFFFFFu;
    H[7] = ((uint32_t)H[7]+h) & 0xFFFFFFFFu;
}

// SHA-512 compression (one 128-byte block)
static void sha512_compress(uint64_t H[8], const uint8_t block[128]) {
    uint64_t W[80];
    for (int i = 0; i < 16; i++) {
        uint64_t v = 0;
        for (int j = 0; j < 8; j++)
            v = (v << 8) | block[i*8+j];
        W[i] = v;
    }
    for (int t = 16; t < 80; t++) {
        uint64_t s0 = rotr64(W[t-15],1) ^ rotr64(W[t-15],8) ^ (W[t-15] >> 7);
        uint64_t s1 = rotr64(W[t-2],19) ^ rotr64(W[t-2],61) ^ (W[t-2] >> 6);
        W[t] = W[t-16] + s0 + W[t-7] + s1;
    }
    uint64_t a=H[0],b=H[1],c=H[2],d=H[3],e=H[4],f=H[5],g=H[6],h=H[7];
    for (int t = 0; t < 80; t++) {
        uint64_t S1 = rotr64(e,14) ^ rotr64(e,18) ^ rotr64(e,41);
        uint64_t ch = (e & f) ^ ((~e) & g);
        uint64_t temp1 = h + S1 + ch + ISA_SHA512_K[t] + W[t];
        uint64_t S0 = rotr64(a,28) ^ rotr64(a,34) ^ rotr64(a,39);
        uint64_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint64_t temp2 = S0 + maj;
        h=g; g=f; f=e; e=d+temp1; d=c; c=b; b=a; a=temp1+temp2;
    }
    H[0]+=a; H[1]+=b; H[2]+=c; H[3]+=d;
    H[4]+=e; H[5]+=f; H[6]+=g; H[7]+=h;
}

// Run compression on block at M[TSRC0], return cycle count
static int sha_compress(CPUState& s) {
    uint64_t H[8];
    sha_unpack(s, H);
    uint8_t block[128];
    sha_read_block(s, block);
    if (s.sha_mode == 0) {
        sha256_compress(H, block);
        sha_pack(s, H);
        s.flag_z = 1;
        return 64;
    } else {
        sha512_compress(H, block);
        sha_pack(s, H);
        s.flag_z = 1;
        return 80;
    }
}

// Write FIPS 180-4 padding at M[TSRC0+R0], return true if two-block pad
static bool sha_write_pad(CPUState& s) {
    int bsz = sha_block_size(s);
    int lsz = s.sha_mode >= 1 ? 16 : 8;
    int pos = (int)(s.regs[0] & 0xFFFFFFFFull) % bsz;
    uint64_t base = s.tsrc0;

    mem_write8(s, base + pos, 0x80);
    pos++;

    bool two_blocks = pos > (bsz - lsz);
    if (two_blocks) {
        while (pos < bsz) { mem_write8(s, base + pos, 0x00); pos++; }
        s.flag_c = 1;
        return true;
    }
    // zero-fill
    while (pos < bsz - lsz) { mem_write8(s, base + pos, 0x00); pos++; }
    // big-endian length
    uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
    if (s.sha_mode >= 1) {
        for (int i = 0; i < 8; i++)
            mem_write8(s, base + bsz - 16 + i, (uint8_t)(hi >> (56 - i*8)));
    }
    for (int i = 0; i < 8; i++)
        mem_write8(s, base + bsz - 8 + i, (uint8_t)(lo >> (56 - i*8)));
    s.flag_c = 0;
    return false;
}

// ---------------------------------------------------------------------------
//  Field ALU ISA helpers (§B.5)
// ---------------------------------------------------------------------------

static const BigNum GF_BUILTIN_PRIMES[3] = {
    make_curve25519_p(),
    make_secp256k1_p(),
    make_p256_p(),
};

static BigNum gf_get_prime(const CPUState& s) {
    if (s.gf_prime_sel < 3) return GF_BUILTIN_PRIMES[s.gf_prime_sel];
    if (s.gf_prime_sel == 3 && !s.gf_custom_p.is_zero()) return s.gf_custom_p;
    return GF_BUILTIN_PRIMES[0];
}

static bool gf_is_mont(const CPUState& s) {
    return s.gf_prime_sel == 3 && !s.gf_mont_pinv.is_zero();
}

static BigNum gf_acc_to_bignum(const CPUState& s) {
    BigNum r;
    r.w[0] = s.acc[0]; r.w[1] = s.acc[1];
    r.w[2] = s.acc[2]; r.w[3] = s.acc[3];
    return r;
}

static void gf_bignum_to_acc(CPUState& s, const BigNum& v) {
    s.acc[0] = v.w[0]; s.acc[1] = v.w[1];
    s.acc[2] = v.w[2]; s.acc[3] = v.w[3];
}

static BigNum gf_read_tile_b(CPUState& s) {
    uint8_t buf[32];
    uint64_t base = s.tsrc0;
    for (int i = 0; i < 32; i++) buf[i] = mem_read8(s, base + i);
    return BigNum::from_le_bytes(buf);
}

static void gf_write_tile_dst(CPUState& s, const BigNum& v) {
    uint8_t buf[32];
    v.to_le_bytes(buf);
    uint64_t base = s.tdst;
    for (int i = 0; i < 32; i++) mem_write8(s, base + i, buf[i]);
}

static BigNum gf_mulmod_sel(const CPUState& s, const BigNum& a, const BigNum& b, const BigNum& p) {
    if (gf_is_mont(s)) return bn_mont_mulmod(a, b, p, s.gf_mont_pinv);
    return bn_mulmod(a, b, p);
}

static BigNum gf_sqrmod_sel(const CPUState& s, const BigNum& a, const BigNum& p) {
    if (gf_is_mont(s)) return bn_mont_sqrmod(a, p, s.gf_mont_pinv);
    return bn_sqrmod(a, p);
}

static int exec_field(CPUState& s) {
    // Called with op = sub_op & 0xF already extracted by caller
    // We re-read the op from the caller's context — but actually,
    // let's take it as a parameter.
    return 0; // placeholder, see exec_crypto dispatch below
}

static int exec_crypto(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op = fetch8(s);
    int unit = (sub_op >> 4) & 0xF;
    int op   = sub_op & 0xF;

    if (unit == 0x0) {
        // --- CRC unit ---
        bool is64 = (s.crc_mode >= 2);
        uint32_t poly32 = (s.crc_mode == 1) ? CRC32C_POLY : CRC32_POLY;

        switch (op) {
        case 0x0: { // CRC.INIT
            s.crc_acc = is64 ? 0xFFFFFFFFFFFFFFFFull : 0xFFFFFFFFu;
            return 1;
        }
        case 0x1: { // CRC.B Rd, Rs — feed one byte
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint8_t b = (uint8_t)(s.regs[rs] & 0xFF);
            if (is64)
                s.crc_acc = crc_byte_64(s.crc_acc, b, CRC64_POLY);
            else
                s.crc_acc = crc_byte_32((uint32_t)s.crc_acc, b, poly32);
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        case 0x2: { // CRC.Q Rd, Rs — feed 8 bytes (LE order)
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint64_t val = s.regs[rs];
            if (is64) {
                uint64_t acc = s.crc_acc;
                for (int i = 0; i < 8; i++)
                    acc = crc_byte_64(acc, (uint8_t)(val >> (i * 8)), CRC64_POLY);
                s.crc_acc = acc;
            } else {
                uint32_t acc = (uint32_t)s.crc_acc;
                for (int i = 0; i < 8; i++)
                    acc = crc_byte_32(acc, (uint8_t)(val >> (i * 8)), poly32);
                s.crc_acc = acc;
            }
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        case 0x3: { // CRC.FIN Rd, Rs — finalize
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            uint64_t mask = is64 ? 0xFFFFFFFFFFFFFFFFull : 0xFFFFFFFFu;
            s.regs[rd] = s.crc_acc ^ mask;
            return 1;
        }
        case 0x4: { // CRC.MODE imm8
            uint8_t imm = fetch8(s);
            s.crc_mode = imm & 0x03;
            return 1;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO CRC reserved sub-op");
        }
    } else if (unit == 0x1) {
        // --- SHA-2 unit ---
        switch (op) {
        case 0x0: { // SHA.INIT imm8
            uint8_t imm = fetch8(s);
            s.sha_mode = imm & 0x03;
            s.sha_msglen_lo = 0;
            s.sha_msglen_hi = 0;
            uint64_t H[8];
            if (s.sha_mode == 0) {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA256_IV[i];
            } else if (s.sha_mode == 1) {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA384_IV[i];
            } else {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA512_IV[i];
            }
            sha_pack(s, H);
            return 2;
        }
        case 0x1: { // SHA.ROUND — compress block at TSRC0
            return sha_compress(s);
        }
        case 0x2: { // SHA.PAD
            sha_write_pad(s);
            return 3;
        }
        case 0x3: { // SHA.DIN Rd, Rs — feed one byte
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint8_t byte_val = (uint8_t)(s.regs[rs] & 0xFF);
            uint64_t base = s.tsrc0;
            uint64_t r0 = s.regs[0];
            mem_write8(s, base + r0, byte_val);
            r0++;
            // track message length in bits
            uint64_t old = s.sha_msglen_lo;
            s.sha_msglen_lo += 8;
            if (s.sha_msglen_lo < old) s.sha_msglen_hi++;
            // auto-round when block is full
            int bsz = sha_block_size(s);
            int cycles = 1;
            if ((int)r0 >= bsz) {
                cycles += sha_compress(s);
                r0 = 0;
            }
            s.regs[0] = r0;
            s.regs[rd] = r0;
            return cycles;
        }
        case 0x4: { // SHA.DOUT Rd, Rs — read hash word
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            int idx = (int)(s.regs[rs]) & 0x7;
            uint64_t H[8];
            sha_unpack(s, H);
            s.regs[rd] = H[idx];
            return 1;
        }
        case 0x5: { // SHA.FINAL — pad + compress
            bool two_blocks = sha_write_pad(s);
            int cycles = 3;
            if (two_blocks) {
                cycles += sha_compress(s);
                // write second pad block (zeros + length)
                int bsz = sha_block_size(s);
                int lsz = s.sha_mode >= 1 ? 16 : 8;
                uint64_t base = s.tsrc0;
                for (int i = 0; i < bsz - lsz; i++)
                    mem_write8(s, base + i, 0x00);
                uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
                if (s.sha_mode >= 1) {
                    for (int i = 0; i < 8; i++)
                        mem_write8(s, base + bsz - 16 + i, (uint8_t)(hi >> (56 - i*8)));
                }
                for (int i = 0; i < 8; i++)
                    mem_write8(s, base + bsz - 8 + i, (uint8_t)(lo >> (56 - i*8)));
            }
            cycles += sha_compress(s);
            return cycles;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO SHA-2 reserved sub-op");
        }
    } else if (unit == 0x2) {
        // --- Field ALU unit (§B.5) ---
        BigNum p = gf_get_prime(s);
        switch (op) {
        case 0x0: { // GF.ADD
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum r = bn_addmod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x1: { // GF.SUB
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum r = bn_submod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x2: { // GF.MUL
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum r = gf_mulmod_sel(s, a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x3: { // GF.SQR
            BigNum a = gf_acc_to_bignum(s);
            BigNum r = gf_sqrmod_sel(s, a, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x4: { // GF.INV
            BigNum a = gf_acc_to_bignum(s);
            BigNum r = bn_invmod(a, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 767;
        }
        case 0x5: { // GF.POW
            BigNum a = gf_acc_to_bignum(s);
            BigNum e = gf_read_tile_b(s);
            BigNum r = bn_powmod(a, e, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 767;
        }
        case 0x6: { // GF.MULR — raw 256×256→512
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum lo, hi;
            BigNum::mul_wide(a, b, lo, hi);
            gf_bignum_to_acc(s, lo);
            gf_write_tile_dst(s, hi);
            s.gf_prev_lo = lo;
            s.gf_prev_hi = hi;
            return 1;
        }
        case 0x7: { // GF.MAC — (ACC * B + prev) mod p
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum ab = gf_mulmod_sel(s, a, b, p);
            BigNum r = bn_addmod(ab, s.gf_prev_lo, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x8: { // GF.MACR — raw: prev_512 + ACC * B
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum mul_lo, mul_hi;
            BigNum::mul_wide(a, b, mul_lo, mul_hi);
            BigNum sum_lo = s.gf_prev_lo.add(mul_lo);
            BigNum sum_hi = s.gf_prev_hi.add(mul_hi);
            if (sum_lo < s.gf_prev_lo) {
                BigNum one; one.w[0] = 1;
                sum_hi = sum_hi.add(one);
            }
            gf_bignum_to_acc(s, sum_lo);
            gf_write_tile_dst(s, sum_hi);
            s.gf_prev_lo = sum_lo;
            s.gf_prev_hi = sum_hi;
            return 1;
        }
        case 0x9: { // GF.CMOV Rd
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            bool cond = s.regs[rd] != 0;
            BigNum b = gf_read_tile_b(s);
            if (cond) {
                gf_bignum_to_acc(s, b);
                s.gf_prev_lo = b;
            }
            return 1;
        }
        case 0xA: { // GF.CEQ — constant-time equality
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum eq;
            eq.w[0] = (a == b) ? 1 : 0;
            gf_bignum_to_acc(s, eq);
            s.gf_prev_lo = eq;
            s.flag_z = (a == b) ? 1 : 0;
            return 1;
        }
        case 0xB: { // GF.PRIME imm8
            uint8_t imm = fetch8(s);
            s.gf_prime_sel = imm & 0x03;
            return 1;
        }
        case 0xC: { // GF.LDPRIME
            s.gf_custom_p = gf_acc_to_bignum(s);
            s.gf_mont_pinv = gf_read_tile_b(s);
            return 1;
        }
        case 0xD: { // GF.X25519
            BigNum scalar = gf_acc_to_bignum(s);
            BigNum u_coord = gf_read_tile_b(s);
            uint8_t scalar_bytes[32], u_bytes[32];
            scalar.to_le_bytes(scalar_bytes);
            u_coord.to_le_bytes(u_bytes);
            BigNum r = x25519_scalar_mul(scalar_bytes, u_bytes, GF_BUILTIN_PRIMES[0]);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 4335;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO Field ALU reserved sub-op");
        }
    } else {
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO reserved unit");
    }
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

    uint64_t pc_start = pc(s);  // save so we can rewind for MEX_FALLBACK
    uint8_t byte0 = fetch8(s);
    int f = (byte0 >> 4) & 0xF;
    int n = byte0 & 0xF;
    int cycles = 1;

    // EXT prefix
    if (f == 0xF) {
        // EXT.STRING (F9) — execute natively
        if (n == 0x9) {
            cycles += exec_string(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // EXT.DICT (FA) — execute natively
        if (n == 0xA) {
            cycles += exec_dict(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // EXT.CRYPTO (FB) — execute natively
        if (n == 0xB) {
            cycles += exec_crypto(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        s.ext_modifier = n;
        byte0 = fetch8(s);
        f = (byte0 >> 4) & 0xF;
        n = byte0 & 0xF;
        cycles++;
        // REX + EXT.STRING — execute natively with REX bits active
        if (f == 0xF && n == 0x9) {
            cycles += exec_string(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // REX + EXT.DICT — execute natively with REX bits active
        if (f == 0xF && n == 0xA) {
            cycles += exec_dict(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // REX + EXT.CRYPTO — execute natively with REX bits active
        if (f == 0xF && n == 0xB) {
            cycles += exec_crypto(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
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
                uint64_t t = sys_pop64(s, cb) & 0xFFFF;
                s.xsel = (t >> 8) & 0x1F;
                s.psel = t & 0x1F;
                s.flag_i = 1;
                cycles++;
                break;
            }
            case 0x6: {  // DIS
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint64_t t = sys_pop64(s, cb) & 0xFFFF;
                s.xsel = (t >> 8) & 0x1F;
                s.psel = t & 0x1F;
                s.flag_i = 0;
                cycles++;
                break;
            }
            case 0x7: {  // MARK
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint16_t t = ((s.xsel & 0x1F) << 8) | (s.psel & 0x1F);
                s.t_reg = t;
                sys_push64(s, cb, t);
                s.xsel = s.psel;
                cycles++;
                break;
            }
            case 0x8:  // SAV — store T as 16-bit to M(R(X))
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                sys_write16(s, cb, rx(s), s.t_reg);
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
                int rn = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
                uint64_t target = s.regs[rn];
                // Check accelerator hooks BEFORE pushing return address
                int hook = find_accel_hook(s, target);
                if (hook) {
                    // Don't push return addr — we skip the word entirely.
                    // PC stays where it is (after the CALL.L instruction).
                    cycles += execute_accel_hook(s, hook);
                    break;
                }
                uint64_t ret_addr = pc(s);
                sys_push64(s, cb, ret_addr);
                pc(s) = target;
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

    case 0x1: {  // INC Rn
        int rn_inc = n | (rex_n(s.ext_modifier) << 4);
        s.regs[rn_inc]++;
        break;
    }

    case 0x2: {  // DEC Rn
        int rn_dec = n | (rex_n(s.ext_modifier) << 4);
        s.regs[rn_dec]--;
        break;
    }

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
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
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
        int rn = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
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
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
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
            case 0x9: {  // STXI — M(R(X)) ← D[7:0]; R(X)++
                sys_write8(s, cb, rx(s), s.d_reg & 0xFF);
                rx(s)++;
                break;
            }
            case 0xA: {  // SHRC.D
                uint8_t old_c = s.flag_c;
                s.flag_c = s.d_reg & 1;
                s.d_reg = ((old_c << 7) | (s.d_reg >> 1)) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xB: {  // STXD.D — M(R(X)) ← D[7:0]; R(X)--
                sys_write8(s, cb, rx(s), s.d_reg & 0xFF);
                rx(s)--;
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
            // Port bridge → route byte to MMIO device
            uint32_t mmio_off = s.port_map[n];
            if (mmio_off < 0x1000 && cb.has_mmio) {
                uint64_t mmio_addr = cb.mmio_start + mmio_off;
                sys_write8(s, cb, mmio_addr, val);
            }
            if (cb.on_output)
                cb.on_output(n, val);
        } else if (n >= 9 && n <= 15) {  // INP
            int port = n - 8;
            uint8_t val;
            uint32_t mmio_off = s.port_map[port];
            if (mmio_off < 0x1000 && cb.has_mmio) {
                uint64_t mmio_addr = cb.mmio_start + mmio_off;
                val = sys_read8(s, cb, mmio_addr);
            } else {
                val = s.port_in[port];
            }
            sys_write8(s, cb, rx(s), val);
            s.d_reg = val;
        }
        break;
    }

    case 0xA:  // SEP Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.psel = n | (rex_n(s.ext_modifier) << 4);
        break;

    case 0xB:  // SEX Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.xsel = n | (rex_n(s.ext_modifier) << 4);
        break;

    case 0xC: {  // MULDIV
        uint8_t b1 = fetch8(s);
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
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
            // ---- Bitfield ALU (sub-ops 0x8–0xF) ----
            case 0x8: {  // POPCNT
                s.regs[rd] = __builtin_popcountll(b);
                break;
            }
            case 0x9: {  // CLZ
                s.regs[rd] = b ? __builtin_clzll(b) : 64;
                break;
            }
            case 0xA: {  // CTZ
                s.regs[rd] = b ? __builtin_ctzll(b) : 64;
                break;
            }
            case 0xB: {  // BITREV
                uint64_t v = b;
                v = ((v >> 1)  & 0x5555555555555555ULL) | ((v & 0x5555555555555555ULL) << 1);
                v = ((v >> 2)  & 0x3333333333333333ULL) | ((v & 0x3333333333333333ULL) << 2);
                v = ((v >> 4)  & 0x0F0F0F0F0F0F0F0FULL) | ((v & 0x0F0F0F0F0F0F0F0FULL) << 4);
                v = __builtin_bswap64(v);
                s.regs[rd] = v;
                break;
            }
            case 0xC: {  // BEXT (pext)
                // Rd ← pext(Rs, Rd): extract bits from a at positions set in b
                uint64_t src = a, mask = b, result = 0;
                for (int i = 0; mask; i++) {
                    uint64_t lsb = mask & (-mask);
                    if (src & lsb) result |= (1ULL << i);
                    mask &= mask - 1;
                }
                s.regs[rd] = result;
                break;
            }
            case 0xD: {  // BDEP (pdep)
                // Rd ← pdep(Rs, Rd): deposit bits from b into positions set in a
                uint64_t src = b, mask = a, result = 0;
                for (int i = 0; mask; i++) {
                    uint64_t lsb = mask & (-mask);
                    if (src & (1ULL << i)) result |= lsb;
                    mask &= mask - 1;
                }
                s.regs[rd] = result;
                break;
            }
            case 0xE: {  // RORI (3-byte: CE [Rd:4][0000] [imm8])
                uint8_t imm = fetch8(s);
                int shift = imm & 63;
                uint64_t r = shift ? ((a >> shift) | (a << (64 - shift))) : a;
                s.regs[rd] = r;
                break;
            }
            case 0xF: {  // BSWAP
                s.regs[rd] = __builtin_bswap64(b);
                break;
            }
        }
        {
            uint64_t r = s.regs[rd];
            s.flag_z = (r == 0) ? 1 : 0;
            s.flag_n = (r >> 63) & 1;
        }
        if (n <= 0x7) cycles += 3;  // mul/div extra cycles only
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
        int rc = exec_mex(s, n);
        if (rc < 0) {
            // FP tile op — rewind PC to the start of the instruction
            // (including any EXT prefix) so the Python fallback can
            // re-fetch and execute the entire instruction correctly.
            pc(s) = pc_start;
            s.ext_modifier = -1;
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
    // GIL is released by the caller (pybind11 binding).  All Python
    // callbacks in StepCallbacks reacquire it as needed.  This lets
    // background threads (display, NIC RX) run freely while the CPU
    // inner loop executes pure C++.
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
            } else if (what == "EXT_ISA_FALLBACK") {
                // Unhandled EXT ISA op — PC already rewound.
                // Return to Python to execute this instruction.
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
        .def(py::init([]() {
            auto s = std::make_unique<CPUState>();
            // Default port_map entries to disabled (0xFFFF sentinel)
            for (int i = 0; i < 8; i++) s->port_map[i] = 0xFFFF;
            // Zero-init dictionary cache
            s->dict_clear_all();
            // CRC state defaults
            s->crc_acc = 0xFFFFFFFF;
            s->crc_mode = 0;
            // Field ALU state defaults
            s->gf_prime_sel = 0;
            s->gf_custom_p = BigNum();
            s->gf_mont_pinv = BigNum();
            s->gf_prev_lo = BigNum();
            s->gf_prev_hi = BigNum();
            return s;
        }))
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
        .def_readwrite("crc_acc", &CPUState::crc_acc)
        .def_readwrite("crc_mode", &CPUState::crc_mode)
        .def_readwrite("sha_mode", &CPUState::sha_mode)
        .def_readwrite("sha_msglen_lo", &CPUState::sha_msglen_lo)
        .def_readwrite("sha_msglen_hi", &CPUState::sha_msglen_hi)
        .def_readwrite("gf_prime_sel", &CPUState::gf_prime_sel)
        .def_readwrite("core_id", &CPUState::core_id)
        .def_readwrite("num_cores", &CPUState::num_cores)
        .def_readwrite("mem_size", &CPUState::mem_size)
        // Register access
        .def("get_reg", [](const CPUState& s, int i) { return s.regs[i & 0x1F]; })
        .def("set_reg", [](CPUState& s, int i, uint64_t v) { s.regs[i & 0x1F] = v; })
        // Accumulator access
        .def("get_acc", [](const CPUState& s, int i) { return s.acc[i & 3]; })
        .def("set_acc", [](CPUState& s, int i, uint64_t v) { s.acc[i & 3] = v; })
        // Port access
        .def("get_port_out", [](const CPUState& s, int i) { return s.port_out[i & 7]; })
        .def("set_port_in", [](CPUState& s, int i, uint8_t v) { s.port_in[i & 7] = v; })
        // Port bridge remap table
        .def("get_port_map", [](const CPUState& s, int i) -> uint32_t { return s.port_map[i & 7]; })
        .def("set_port_map", [](CPUState& s, int i, uint32_t v) { s.port_map[i & 7] = v; })
        // Memory attachment
        .def("attach_mem", [](CPUState& s, py::buffer buf, uint64_t size) {
            py::buffer_info info = buf.request(true);  // writable
            s.mem = static_cast<uint8_t*>(info.ptr);
            s.mem_size = size;
            // WOTS chain accelerator needs direct memory access (DMA read)
            s.crypto.wots.mem = s.mem;
            s.crypto.wots.mem_size = (uint32_t)size;
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
        // External memory attachment
        .def("attach_ext_mem", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            py::buffer_info info = buf.request(true);  // writable
            s.ext_mem = static_cast<uint8_t*>(info.ptr);
            s.ext_mem_base = base;
            s.ext_mem_size = size;
        })
        .def_readwrite("ext_mem_base", &CPUState::ext_mem_base)
        .def_readwrite("ext_mem_size", &CPUState::ext_mem_size)
        // VRAM memory attachment
        .def("attach_vram", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            py::buffer_info info = buf.request(true);  // writable
            s.vram_mem = static_cast<uint8_t*>(info.ptr);
            s.vram_base = base;
            s.vram_size = size;
        })
        .def_readwrite("vram_base", &CPUState::vram_base)
        .def_readwrite("vram_size", &CPUState::vram_size)
        // Flags
        .def("flags_pack", [](const CPUState& s) { return flags_pack(s); })
        .def("flags_unpack", [](CPUState& s, uint8_t v) { flags_unpack(s, v); })
        // Crypto devices — initialize C++ native crypto accelerators
        .def("init_crypto", [](CPUState& s) {
            s.crypto.init();
            // Ensure WOTS chain has current memory pointer
            s.crypto.wots.mem = s.mem;
            s.crypto.wots.mem_size = (uint32_t)s.mem_size;
        })
        .def("disable_crypto", [](CPUState& s) {
            s.crypto.enabled = false;
        })
        .def("crypto_enabled", [](const CPUState& s) {
            return s.crypto.enabled;
        })
        // Sync crypto state from Python devices (for save/restore)
        .def("crypto_aes_reset", [](CPUState& s) { s.crypto.aes.reset(); })
        .def("crypto_sha3_reset", [](CPUState& s) { s.crypto.sha3.reset(); s.crypto.sha3.mode = 0; })
        .def("crypto_wots_reset", [](CPUState& s) {
            s.crypto.wots.reset();
            s.crypto.wots.sha3 = &s.crypto.sha3;
            s.crypto.wots.mem = s.mem;
            s.crypto.wots.mem_size = (uint32_t)s.mem_size;
        })
        .def("crypto_wots_status", [](const CPUState& s) -> uint8_t {
            return s.crypto.wots.status;
        })
        // Direct crypto MMIO access (for testing / Python-side access)
        .def("crypto_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.crypto.read8(mmio_off);
        })
        .def("crypto_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.crypto.write8(mmio_off, val);
        })
        // ── NIC device ────────────────────────────────────────
        .def("nic_init", [](CPUState& s, py::bytes mac_bytes) {
            std::string mac_str = mac_bytes;
            uint8_t mac[6] = {};
            size_t n = std::min(mac_str.size(), (size_t)6);
            std::memcpy(mac, mac_str.data(), n);
            s.nic.init(mac);
            // Wire memory pointers from CPUState
            s.nic.attach_mem_ptrs(
                s.mem, s.mem_size,
                s.hbw_mem, s.hbw_base, s.hbw_size,
                s.ext_mem, s.ext_mem_base, s.ext_mem_size
            );
        })
        .def("nic_sync_mem_ptrs", [](CPUState& s) {
            // Re-sync memory pointers after attach_ext_mem / attach_hbw_mem
            s.nic.attach_mem_ptrs(
                s.mem, s.mem_size,
                s.hbw_mem, s.hbw_base, s.hbw_size,
                s.ext_mem, s.ext_mem_base, s.ext_mem_size
            );
        })
        .def("nic_set_tx_callback", [](CPUState& s, py::function cb) {
            // tx_callback: called from C++ when NIC sends a frame
            // cb receives (bytes,) and returns bool
            s.nic.tx_callback = [cb](const uint8_t* data, size_t len) -> bool {
                py::gil_scoped_acquire gil;
                try {
                    py::bytes frame(reinterpret_cast<const char*>(data), len);
                    py::object result = cb(frame);
                    if (result.is_none()) return true;
                    return result.cast<bool>();
                } catch (...) {
                    return false;
                }
            };
        })
        .def("nic_inject_frame", [](CPUState& s, py::bytes frame) {
            std::string data = frame;
            s.nic.inject_frame(
                reinterpret_cast<const uint8_t*>(data.data()), data.size()
            );
        })
        .def("nic_has_rx", [](CPUState& s) -> bool {
            return s.nic.has_rx();
        })
        .def("nic_rx_queue_size", [](CPUState& s) -> size_t {
            return s.nic.rx_queue_size();
        })
        .def("nic_tx_queue_size", [](const CPUState& s) -> size_t {
            return s.nic.tx_queue_size();
        })
        .def("nic_drain_one_tx", [](CPUState& s) -> py::bytes {
            auto frame = s.nic.drain_one_tx();
            return py::bytes(reinterpret_cast<const char*>(frame.data()), frame.size());
        })
        .def("nic_set_link_up", [](CPUState& s, bool up) {
            s.nic.link_up = up;
        })
        .def("nic_enabled", [](const CPUState& s) -> bool {
            return s.nic.enabled;
        })
        .def("nic_disable", [](CPUState& s) {
            s.nic.enabled = false;
        })
        .def("nic_reset", [](CPUState& s) {
            s.nic.reset_state();
        })
        .def("nic_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.nic.read8(mmio_off);
        })
        .def("nic_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.nic.write8(mmio_off, val);
        })
        .def("nic_get_tx_count", [](const CPUState& s) -> uint16_t {
            return s.nic.tx_count;
        })
        .def("nic_get_rx_count", [](const CPUState& s) -> uint16_t {
            return s.nic.rx_count;
        })
        // ── TRNG device ───────────────────────────────────────
        .def("init_trng", [](CPUState& s) {
            s.trng.init();
        })
        .def("trng_enabled", [](const CPUState& s) -> bool {
            return s.trng.enabled;
        })
        .def("disable_trng", [](CPUState& s) {
            s.trng.enabled = false;
        })
        // ── Framebuffer device ────────────────────────────────
        .def("fb_init", [](CPUState& s) {
            s.fb.init();
        })
        .def("fb_enabled", [](const CPUState& s) -> bool {
            return s.fb.enabled;
        })
        .def("fb_disable", [](CPUState& s) {
            s.fb.enabled = false;
        })
        .def("fb_tick", [](CPUState& s, uint32_t cycles) {
            s.fb.tick(cycles);
        })
        .def("fb_read8", [](const CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.fb.read8(mmio_off);
        })
        .def("fb_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.fb.write8(mmio_off, val);
        })
        .def("fb_irq_pending", [](const CPUState& s) -> bool {
            return s.fb.irq_pending();
        })
        // FB properties for display thread access
        .def_property("fb_base_addr",
            [](const CPUState& s) -> uint64_t { return s.fb.fb_base; },
            [](CPUState& s, uint64_t v) { s.fb.fb_base = v; })
        .def_property("fb_width",
            [](const CPUState& s) -> uint32_t { return s.fb.width; },
            [](CPUState& s, uint32_t v) { s.fb.width = v; })
        .def_property("fb_height",
            [](const CPUState& s) -> uint32_t { return s.fb.height; },
            [](CPUState& s, uint32_t v) { s.fb.height = v; })
        .def_property("fb_stride",
            [](const CPUState& s) -> uint32_t { return s.fb.stride; },
            [](CPUState& s, uint32_t v) { s.fb.stride = v; })
        .def_property("fb_mode",
            [](const CPUState& s) -> uint8_t { return s.fb.mode; },
            [](CPUState& s, uint8_t v) { s.fb.mode = v; })
        .def_property("fb_enable",
            [](const CPUState& s) -> uint8_t { return s.fb.enable; },
            [](CPUState& s, uint8_t v) { s.fb.enable = v; })
        .def_property("fb_vsync_count",
            [](const CPUState& s) -> uint32_t { return s.fb.vsync_count; },
            [](CPUState& s, uint32_t v) { s.fb.vsync_count = v; })
        .def_property("fb_vblank",
            [](const CPUState& s) -> bool { return s.fb.vblank; },
            [](CPUState& s, bool v) { s.fb.vblank = v; })
        .def_property("fb_cycles_per_frame",
            [](const CPUState& s) -> uint32_t { return s.fb.cycles_per_frame; },
            [](CPUState& s, uint32_t v) { s.fb.cycles_per_frame = v; })
        .def("fb_get_palette", [](const CPUState& s) -> std::vector<uint32_t> {
            return std::vector<uint32_t>(s.fb.palette.begin(), s.fb.palette.end());
        })
        .def("fb_set_palette_entry", [](CPUState& s, int idx, uint32_t rgb) {
            if (idx >= 0 && idx < 256)
                s.fb.palette[idx] = rgb & 0x00FFFFFF;
        })
        // ── Framebuffer render (C++ pixel conversion) ─────────
        //
        // Converts VRAM pixel data into an RGB888 numpy array suitable
        // for pygame.surfarray.blit_array().  Returns shape (w, h, 3)
        // with dtype uint8.  Runs without GIL for maximum throughput.
        //
        // Modes: 0 = 8-bit indexed (palette lookup)
        //        1 = RGB565
        //        3 = RGBA8888 (alpha discarded)
        //
        // Returns None if the framebuffer base address doesn't map to
        // any attached memory region.
        .def("render_fb_rgb", [](CPUState& s) -> py::object {
            uint32_t w = s.fb.width;
            uint32_t h = s.fb.height;
            uint32_t stride = s.fb.stride;
            uint8_t  mode = s.fb.mode;
            uint64_t base = s.fb.fb_base;

            if (w == 0 || h == 0 || w > 4096 || h > 4096)
                return py::none();

            // Resolve base address to a memory pointer
            const uint8_t* src = nullptr;
            uint64_t mem_size = 0;
            uint64_t mem_off = 0;

            if (s.vram_mem && base >= s.vram_base
                && base < s.vram_base + s.vram_size) {
                src = s.vram_mem;
                mem_off = base - s.vram_base;
                mem_size = s.vram_size;
            } else if (s.ext_mem && base >= s.ext_mem_base
                       && base < s.ext_mem_base + s.ext_mem_size) {
                src = s.ext_mem;
                mem_off = base - s.ext_mem_base;
                mem_size = s.ext_mem_size;
            } else if (s.hbw_mem && base >= s.hbw_base
                       && base < s.hbw_base + s.hbw_size) {
                src = s.hbw_mem;
                mem_off = base - s.hbw_base;
                mem_size = s.hbw_size;
            }
            if (!src)
                return py::none();

            // Allocate output: shape (w, h, 3) for pygame surfarray
            auto result = py::array_t<uint8_t>({(int)w, (int)h, 3});
            auto buf = result.mutable_unchecked<3>();

            // Release GIL for the pixel conversion loop
            {
                py::gil_scoped_release release;

                if (mode == 0) {
                    // 8-bit indexed — palette lookup
                    const auto& pal = s.fb.palette;
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + w > mem_size) break;
                        const uint8_t* row = src + row_off;
                        for (uint32_t x = 0; x < w; x++) {
                            uint32_t rgb = pal[row[x]];
                            buf(x, y, 0) = (rgb >> 16) & 0xFF;
                            buf(x, y, 1) = (rgb >>  8) & 0xFF;
                            buf(x, y, 2) =  rgb        & 0xFF;
                        }
                    }
                } else if (mode == 1) {
                    // RGB565
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + (uint64_t)w * 2 > mem_size) break;
                        const uint16_t* row =
                            reinterpret_cast<const uint16_t*>(src + row_off);
                        for (uint32_t x = 0; x < w; x++) {
                            uint16_t px = row[x];
                            buf(x, y, 0) = ((px >> 11) & 0x1F) << 3;
                            buf(x, y, 1) = ((px >>  5) & 0x3F) << 2;
                            buf(x, y, 2) = ( px        & 0x1F) << 3;
                        }
                    }
                } else if (mode == 3) {
                    // RGBA8888 — drop alpha
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + (uint64_t)w * 4 > mem_size) break;
                        const uint8_t* row = src + row_off;
                        for (uint32_t x = 0; x < w; x++) {
                            uint32_t off4 = x * 4;
                            buf(x, y, 0) = row[off4 + 0];
                            buf(x, y, 1) = row[off4 + 1];
                            buf(x, y, 2) = row[off4 + 2];
                        }
                    }
                }
                // Unknown mode: array stays zero-filled (gray/black)
            }

            return result;
        })
        // ── Timer device ──────────────────────────────────────
        .def("timer_init", [](CPUState& s) {
            s.timer.init();
        })
        .def("timer_enabled", [](const CPUState& s) -> bool {
            return s.timer.enabled;
        })
        .def("timer_disable", [](CPUState& s) {
            s.timer.enabled = false;
        })
        .def("timer_tick", [](CPUState& s, uint32_t cycles) {
            s.timer.tick(cycles);
        })
        .def("timer_read8", [](const CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.timer.read8(mmio_off);
        })
        .def("timer_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.timer.write8(mmio_off, val);
        })
        .def_property("timer_irq_pending",
            [](const CPUState& s) -> bool { return s.timer.irq_pending; },
            [](CPUState& s, bool v) { s.timer.irq_pending = v; })
        .def_property("timer_counter",
            [](const CPUState& s) -> uint32_t { return s.timer.counter; },
            [](CPUState& s, uint32_t v) { s.timer.counter = v; })
        .def_property("timer_compare",
            [](const CPUState& s) -> uint32_t { return s.timer.compare; },
            [](CPUState& s, uint32_t v) { s.timer.compare = v; })
        .def_property("timer_control",
            [](const CPUState& s) -> uint8_t { return s.timer.control; },
            [](CPUState& s, uint8_t v) { s.timer.control = v; })
        .def_property("timer_status",
            [](const CPUState& s) -> uint8_t { return s.timer.status; },
            [](CPUState& s, uint8_t v) { s.timer.status = v; })
        // ── UART Geometry device ──────────────────────────────
        .def("uart_geom_init", [](CPUState& s, uint16_t cols, uint16_t rows) {
            s.uart_geom.init(cols, rows);
        }, py::arg("cols") = 80, py::arg("rows") = 30)
        .def("uart_geom_enabled", [](const CPUState& s) -> bool {
            return s.uart_geom.enabled;
        })
        .def("uart_geom_disable", [](CPUState& s) {
            s.uart_geom.enabled = false;
        })
        .def("uart_geom_read8", [](const CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.uart_geom.read8(mmio_off);
        })
        .def("uart_geom_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.uart_geom.write8(mmio_off, val);
        })
        .def_property("uart_geom_cols",
            [](const CPUState& s) -> uint16_t { return s.uart_geom.cols; },
            [](CPUState& s, uint16_t v) { s.uart_geom.cols = v; })
        .def_property("uart_geom_rows",
            [](const CPUState& s) -> uint16_t { return s.uart_geom.rows; },
            [](CPUState& s, uint16_t v) { s.uart_geom.rows = v; })
        .def_property("uart_geom_status",
            [](const CPUState& s) -> uint8_t { return s.uart_geom.status; },
            [](CPUState& s, uint8_t v) { s.uart_geom.status = v; })
        .def_property("uart_geom_ctrl",
            [](const CPUState& s) -> uint8_t { return s.uart_geom.ctrl; },
            [](CPUState& s, uint8_t v) { s.uart_geom.ctrl = v; })
        .def_property("uart_geom_req_cols",
            [](const CPUState& s) -> uint16_t { return s.uart_geom.req_cols; },
            [](CPUState& s, uint16_t v) { s.uart_geom.req_cols = v; })
        .def_property("uart_geom_req_rows",
            [](const CPUState& s) -> uint16_t { return s.uart_geom.req_rows; },
            [](CPUState& s, uint16_t v) { s.uart_geom.req_rows = v; })
        .def("uart_geom_host_set_size", [](CPUState& s, uint16_t c, uint16_t r) {
            s.uart_geom.host_set_size(c, r);
        })
        .def("uart_geom_has_resize_request", [](const CPUState& s) -> bool {
            return s.uart_geom.has_resize_request();
        })
        .def("uart_geom_host_accept_resize", [](CPUState& s, uint16_t c, uint16_t r) {
            s.uart_geom.host_accept_resize(c, r);
        })
        .def("uart_geom_host_deny_resize", [](CPUState& s) {
            s.uart_geom.host_deny_resize();
        })
        // ── Accelerator hooks ─────────────────────────────────
        .def("register_accel_hook", &CPUState::register_accel_hook)
        .def_readonly("accel_hook_count", &CPUState::accel_hook_count)
        // ── Dictionary cache ──────────────────────────────────
        .def("dict_clear", &CPUState::dict_clear_all)
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
    //
    // The GIL is released for the entire batch so that background
    // Python threads (display, NIC RX) can run concurrently with
    // instruction execution.  Each Python callback reacquires the
    // GIL only for the duration of the callback.
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
            py::gil_scoped_acquire acq;
            return mmio_read8(addr).cast<uint8_t>();
        };
        cb.mmio_write8 = [&](uint64_t addr, uint8_t val) {
            py::gil_scoped_acquire acq;
            mmio_write8(addr, val);
        };
        cb.on_output = [&](int port, int val) {
            py::gil_scoped_acquire acq;
            on_output(port, val);
        };
        if (!csr_read_override.is_none()) {
            auto fn = csr_read_override.cast<py::function>();
            cb.csr_read_override = [fn](int addr) -> uint64_t {
                py::gil_scoped_acquire acq;
                py::object result = fn(addr);
                if (result.is_none()) return (uint64_t)-1;
                return result.cast<uint64_t>();
            };
        }
        py::gil_scoped_release release;
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
