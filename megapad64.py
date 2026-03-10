"""
Megapad-64 Bytecode Emulator
=============================
A cycle-step emulator for the Megapad-64 TSP architecture, implementing the
instruction encoding from docs/isa-reference.md.  Spiritual successor to the RCA 1802.

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
CSR_PRIV        = 0x0A  # Privilege level (0=supervisor, 1=user)
CSR_MPU_BASE    = 0x0B  # MPU lower bound (inclusive)
CSR_MPU_LIMIT   = 0x0C  # MPU upper bound (exclusive)
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
# Strided / 2D tile addressing CSRs (§2.5)
CSR_TSTRIDE_R   = 0x40   # Row stride in bytes
CSR_TSTRIDE_C   = 0x41   # Column stride in bytes (reserved)
CSR_TTILE_H     = 0x42   # Tile height (rows to load, 1–8)
CSR_TTILE_W     = 0x43   # Tile width (cols per row in bytes, 1–64)

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

# Cluster MPU CSRs (§6.3 — shared per micro-core cluster)
CSR_CL_PRIV      = 0x6D   # Cluster privilege level (0=S, 1=U)
CSR_CL_MPU_BASE  = 0x6E   # Cluster MPU lower bound (inclusive)
CSR_CL_MPU_LIMIT = 0x6F   # Cluster MPU upper bound (exclusive)

# DMA CSRs (§5.3)
CSR_DMA_RING_BASE  = 0x50   # DMA ring buffer base address
CSR_DMA_RING_SIZE  = 0x51   # DMA ring buffer size
CSR_DMA_HEAD       = 0x52   # DMA head pointer
CSR_DMA_TAIL       = 0x53   # DMA tail pointer
CSR_DMA_STATUS     = 0x54   # DMA status
CSR_DMA_CTRL       = 0x55   # DMA control (bit1=reset)

# QoS CSRs (§5.4)
CSR_QOS_WEIGHT     = 0x58   # Bus arbiter priority weight
CSR_QOS_BWLIMIT    = 0x59   # Bandwidth limit

# EXT.CRYPTO CSRs (Appendix B)
CSR_CRC_ACC       = 0x80   # RW: 64-bit running CRC accumulator
CSR_CRC_MODE      = 0x81   # RW: polynomial select (0=CRC32, 1=CRC32C, 2=CRC64)
CSR_SHA_MODE      = 0x82   # RW: 0=SHA-256, 1=SHA-384, 2=SHA-512
CSR_SHA_MSGLEN    = 0x83   # RW: total message length in bits (low 64)
CSR_SHA_MSGLEN_HI = 0x84   # RW: total message length in bits (high 64)
CSR_GF_PRIME_SEL  = 0x85   # RW: 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom

# I-Cache CSRs (§12.2)
CSR_ICACHE_CTRL   = 0x70   # W: bit0=enable, bit1=invalidate-all; R: bit0=enabled
CSR_ICACHE_HITS   = 0x71   # R: hit counter
CSR_ICACHE_MISSES = 0x72   # R: miss counter

# Cluster-level IVT / Barrier CSRs (§6.3)
CSR_CL_IVTBASE     = 0x73   # Cluster shared IVT base address
CSR_BARRIER_ARRIVE = 0x74   # R/W: per-core barrier arrive bitmask (cluster)
CSR_BARRIER_STATUS = 0x75   # R: barrier done flag + arrive mask  (cluster)

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
IVEC_PRIV_FAULT     = 0x0F  # Privilege violation (vector 15)

# Privilege model — STRIPPED.  The hardware user mode conflicted with
# 1802-heritage SEP/SEX dispatch (family 0xA/0xB) which every Forth
# word needs.  MPU provides address-level protection independently.
# The priv_level CSR is retained as inert state for compatibility.

# Micro-cluster constants (matches mp64_defs.vh)
NUM_FULL_CORES     = 4
NUM_CLUSTERS       = 3
MICRO_PER_CLUSTER  = 4
NUM_ALL_CORES      = NUM_FULL_CORES + NUM_CLUSTERS * MICRO_PER_CLUSTER  # 16
MICRO_ID_BASE      = NUM_FULL_CORES                                      # 4
CLUSTER_SPAD_BYTES = 1024  # 1 KiB per cluster
CLUSTER_SPAD_ADDR  = 0xFFFF_FE00_0000_0000  # scratchpad addr[63:32] sentinel

# Micro-core CPUID: "MP64" v1 "MC" (micro-core variant)
CPUID_MICRO = 0x4D50_3634_0001_4D43

# TMODE EW codes (bits [2:0])
EW_U8    = 0  # 64 lanes × 8-bit
EW_U16   = 1  # 32 lanes × 16-bit
EW_U32   = 2  # 16 lanes × 32-bit
EW_U64   = 3  #  8 lanes × 64-bit
EW_FP16  = 4  # 32 lanes × IEEE 754 half-precision
EW_BF16  = 5  # 32 lanes × bfloat16

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

# ---------------------------------------------------------------------------
#  FP16 / bfloat16 conversion helpers
# ---------------------------------------------------------------------------

def _fp16_to_float(h: int) -> float:
    """Convert IEEE 754 half-precision (16-bit raw) to Python float."""
    sign = (h >> 15) & 1
    exp = (h >> 10) & 0x1F
    frac = h & 0x3FF
    if exp == 0:
        if frac == 0:
            return -0.0 if sign else 0.0
        # Subnormal
        val = (2.0 ** -14) * (frac / 1024.0)
        return -val if sign else val
    if exp == 0x1F:
        if frac == 0:
            return float('-inf') if sign else float('inf')
        return float('nan')
    val = (2.0 ** (exp - 15)) * (1.0 + frac / 1024.0)
    return -val if sign else val


def _float_to_fp16(f: float) -> int:
    """Convert Python float to IEEE 754 half-precision (16-bit raw).
    Uses round-to-nearest-even."""
    import math
    if math.isnan(f):
        return 0x7E00  # quiet NaN
    if math.isinf(f):
        return 0xFC00 if f < 0 else 0x7C00
    if f == 0.0:
        return 0x8000 if math.copysign(1.0, f) < 0 else 0x0000
    # Get FP32 bits for the rounding logic
    bits = struct.unpack('<I', struct.pack('<f', f))[0]
    sign = (bits >> 31) & 1
    exp32 = (bits >> 23) & 0xFF
    frac32 = bits & 0x7FFFFF
    # Rebias exponent: FP32 bias=127, FP16 bias=15
    new_exp = exp32 - 127 + 15
    if new_exp >= 0x1F:
        return (sign << 15) | 0x7C00  # overflow → ±inf
    if new_exp <= 0:
        if new_exp < -10:
            return sign << 15  # underflow → ±0
        # Subnormal: shift mantissa
        frac32 |= 0x800000  # implicit 1
        shift = 1 - new_exp  # how far into subnormal
        # Round-to-nearest-even
        round_bit = (frac32 >> (12 + shift)) & 1
        sticky = (frac32 & ((1 << (12 + shift)) - 1)) != 0
        result = frac32 >> (13 + shift)
        if round_bit and (sticky or (result & 1)):
            result += 1
        return (sign << 15) | (result & 0x3FF)
    # Normal: round mantissa from 23 bits to 10 bits
    round_bit = (frac32 >> 12) & 1
    sticky = (frac32 & 0xFFF) != 0
    frac16 = frac32 >> 13
    if round_bit and (sticky or (frac16 & 1)):
        frac16 += 1
        if frac16 >= 0x400:
            frac16 = 0
            new_exp += 1
            if new_exp >= 0x1F:
                return (sign << 15) | 0x7C00
    return (sign << 15) | (new_exp << 10) | (frac16 & 0x3FF)


def _bf16_to_float(b: int) -> float:
    """Convert bfloat16 (16-bit raw) to Python float."""
    bits32 = (b & 0xFFFF) << 16
    return struct.unpack('<f', struct.pack('<I', bits32))[0]


def _float_to_bf16(f: float) -> int:
    """Convert Python float to bfloat16 (16-bit raw).
    Uses round-to-nearest-even."""
    bits = struct.unpack('<I', struct.pack('<f', f))[0]
    # Round: look at lower 16 bits
    round_bit = (bits >> 15) & 1
    sticky = (bits & 0x7FFF) != 0
    result = bits >> 16
    if round_bit and (sticky or (result & 1)):
        result += 1
        # Overflow of mantissa into exponent is handled naturally
    return result & 0xFFFF


def _fp32_to_bits(f: float) -> int:
    """Python float → 32-bit IEEE 754 bit pattern."""
    return struct.unpack('<I', struct.pack('<f', f))[0]


def _bits_to_fp32(b: int) -> float:
    """32-bit IEEE 754 bit pattern → Python float."""
    return struct.unpack('<f', struct.pack('<I', b & 0xFFFFFFFF))[0]


def _fp_decode(raw: int, ew: int) -> float:
    """Decode a raw 16-bit tile lane value to Python float based on EW code."""
    if ew == EW_FP16:
        return _fp16_to_float(raw & 0xFFFF)
    else:  # EW_BF16
        return _bf16_to_float(raw & 0xFFFF)


def _fp_encode(val: float, ew: int) -> int:
    """Encode a Python float back to 16-bit raw based on EW code."""
    if ew == EW_FP16:
        return _float_to_fp16(val)
    else:  # EW_BF16
        return _float_to_bf16(val)


def _fp_is_nan(raw: int, ew: int) -> bool:
    """Check if a raw 16-bit value is NaN for the given FP format."""
    if ew == EW_FP16:
        return ((raw >> 10) & 0x1F) == 0x1F and (raw & 0x3FF) != 0
    else:  # EW_BF16
        return ((raw >> 7) & 0xFF) == 0xFF and (raw & 0x7F) != 0

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
#  SHA-2 module-level helpers & constants (FIPS 180-4)
# ---------------------------------------------------------------------------

_M32 = 0xFFFF_FFFF
_M64 = 0xFFFF_FFFF_FFFF_FFFF

def _rotr32(x: int, n: int) -> int:
    """32-bit right rotate."""
    return ((x >> n) | (x << (32 - n))) & _M32

def _rotr64(x: int, n: int) -> int:
    """64-bit right rotate."""
    return ((x >> n) | (x << (64 - n))) & _M64

# SHA-256 round constants (§4.2.2)
_K256 = (
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
)

# SHA-512 round constants (§4.2.3)
_K512 = (
    0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
    0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
    0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
    0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
    0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
    0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
    0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
    0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
    0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
    0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
    0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
    0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
    0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
    0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
    0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
    0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
    0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
    0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
    0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817,
)

# SHA-256 initial hash values (§5.3.3)
_SHA256_IV = (
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
)

# SHA-384 initial hash values (§5.3.4)
_SHA384_IV = (
    0xcbbb9d5dc1059ed8, 0x629a292a367cd507,
    0x9159015a3070dd17, 0x152fecd8f70e5939,
    0x67332667ffc00b31, 0x8eb44a8768581511,
    0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4,
)

# SHA-512 initial hash values (§5.3.5)
_SHA512_IV = (
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b,
    0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f,
    0x1f83d9abfb41bd6b, 0x5be0cd19137e2179,
)

def _sha256_compress(H: list[int], block: bytes) -> list[int]:
    """SHA-256 compression function over one 64-byte block.

    H: list of 8 × 32-bit state words [a..h].
    block: 64 bytes (big-endian message block).
    Returns updated H list.
    """
    # Parse block into 16 big-endian 32-bit words
    W = list(struct.unpack('>16I', block))
    # Message schedule expansion (§6.2.2 step 1)
    for t in range(16, 64):
        s0 = _rotr32(W[t-15], 7) ^ _rotr32(W[t-15], 18) ^ (W[t-15] >> 3)
        s1 = _rotr32(W[t-2], 17) ^ _rotr32(W[t-2], 19)  ^ (W[t-2] >> 10)
        W.append((W[t-16] + s0 + W[t-7] + s1) & _M32)
    # Initialize working variables
    a, b, c, d, e, f, g, h = H
    # 64 rounds (§6.2.2 step 3)
    for t in range(64):
        S1 = _rotr32(e, 6) ^ _rotr32(e, 11) ^ _rotr32(e, 25)
        ch = (e & f) ^ ((~e) & g) & _M32
        temp1 = (h + S1 + ch + _K256[t] + W[t]) & _M32
        S0 = _rotr32(a, 2) ^ _rotr32(a, 13) ^ _rotr32(a, 22)
        maj = (a & b) ^ (a & c) ^ (b & c)
        temp2 = (S0 + maj) & _M32
        h, g, f, e, d, c, b, a = g, f, e, (d + temp1) & _M32, c, b, a, (temp1 + temp2) & _M32
    # Add compressed chunk to hash (§6.2.2 step 4)
    return [(H[i] + v) & _M32 for i, v in enumerate((a, b, c, d, e, f, g, h))]


def _sha512_compress(H: list[int], block: bytes) -> list[int]:
    """SHA-512 compression function over one 128-byte block.

    H: list of 8 × 64-bit state words [a..h].
    block: 128 bytes (big-endian message block).
    Returns updated H list.
    """
    W = list(struct.unpack('>16Q', block))
    for t in range(16, 80):
        s0 = _rotr64(W[t-15], 1) ^ _rotr64(W[t-15], 8)  ^ (W[t-15] >> 7)
        s1 = _rotr64(W[t-2], 19) ^ _rotr64(W[t-2], 61)  ^ (W[t-2] >> 6)
        W.append((W[t-16] + s0 + W[t-7] + s1) & _M64)
    a, b, c, d, e, f, g, h = H
    for t in range(80):
        S1 = _rotr64(e, 14) ^ _rotr64(e, 18) ^ _rotr64(e, 41)
        ch = (e & f) ^ ((~e) & g) & _M64
        temp1 = (h + S1 + ch + _K512[t] + W[t]) & _M64
        S0 = _rotr64(a, 28) ^ _rotr64(a, 34) ^ _rotr64(a, 39)
        maj = (a & b) ^ (a & c) ^ (b & c)
        temp2 = (S0 + maj) & _M64
        h, g, f, e, d, c, b, a = g, f, e, (d + temp1) & _M64, c, b, a, (temp1 + temp2) & _M64
    return [(H[i] + v) & _M64 for i, v in enumerate((a, b, c, d, e, f, g, h))]

# ---------------------------------------------------------------------------
#  Field ALU module-level helpers & constants (multi-prime GF arithmetic)
# ---------------------------------------------------------------------------

_M256 = (1 << 256) - 1
_M512 = (1 << 512) - 1

# Built-in primes (matching RTL and C++ implementation)
_GF_PRIMES = [
    (1 << 255) - 19,                                           # Curve25519
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,  # secp256k1
    0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF,  # NIST P-256
]

def _gf_get_prime(sel: int, custom_p: int) -> int:
    """Return the active prime for the given selection."""
    if sel < 3:
        return _GF_PRIMES[sel]
    if sel == 3 and custom_p != 0:
        return custom_p
    return _GF_PRIMES[0]  # fallback

def _gf_addmod(a: int, b: int, p: int) -> int:
    return (a + b) % p

def _gf_submod(a: int, b: int, p: int) -> int:
    return (a - b) % p

def _gf_mulmod(a: int, b: int, p: int) -> int:
    return (a * b) % p

def _gf_sqrmod(a: int, p: int) -> int:
    return (a * a) % p

def _gf_invmod(a: int, p: int) -> int:
    """Fermat inversion: a^(p-2) mod p."""
    return pow(a, p - 2, p)

def _gf_powmod(a: int, e: int, p: int) -> int:
    return pow(a, e, p)

def _gf_mont_mulmod(a: int, b: int, p: int, p_inv: int) -> int:
    """Montgomery multiplication: (a * b * R^{-1}) mod p, where R = 2^256."""
    t = a * b
    m = ((t & _M256) * p_inv) & _M256
    u = (t + m * p) >> 256
    if u >= p:
        u -= p
    return u

def _gf_mont_sqrmod(a: int, p: int, p_inv: int) -> int:
    return _gf_mont_mulmod(a, a, p, p_inv)

def _x25519_scalar_mul(scalar_bytes: bytes, u_bytes: bytes) -> int:
    """RFC 7748 X25519 scalar multiplication.  Returns 256-bit result."""
    p = _GF_PRIMES[0]  # 2^255 - 19

    # Decode scalar with clamping (RFC 7748 §5)
    k = int.from_bytes(scalar_bytes, 'little')
    k &= ~7                          # clear low 3 bits
    k &= ~(1 << 255)                 # clear bit 255
    k |= (1 << 254)                  # set bit 254

    # Decode u-coordinate
    u = int.from_bytes(u_bytes, 'little')
    u &= (1 << 255) - 1              # clear top bit

    # Montgomery ladder
    x_1 = u
    x_2, z_2 = 1, 0
    x_3, z_3 = u, 1
    swap = 0

    for t in range(254, -1, -1):
        k_t = (k >> t) & 1
        swap ^= k_t
        # constant-time conditional swap
        if swap:
            x_2, x_3 = x_3, x_2
            z_2, z_3 = z_3, z_2
        swap = k_t

        A  = (x_2 + z_2) % p
        AA = (A * A) % p
        B  = (x_2 - z_2) % p
        BB = (B * B) % p
        E  = (AA - BB) % p
        C  = (x_3 + z_3) % p
        D  = (x_3 - z_3) % p
        DA = (D * A) % p
        CB = (C * B) % p
        x_3 = pow(DA + CB, 2, p)
        z_3 = (x_1 * pow(DA - CB, 2, p)) % p
        x_2 = (AA * BB) % p
        a24 = 121666
        z_2 = (E * (AA + a24 * E)) % p

    if swap:
        x_2, x_3 = x_3, x_2
        z_2, z_3 = z_3, z_2

    return (x_2 * pow(z_2, p - 2, p)) % p

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

        # 32 × 64-bit GPRs (R0-R15 base, R16-R31 via REX prefix)
        self.regs: list[int] = [0] * 32

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
        self.t_reg: int = 0   # T register (saved XSEL|PSEL, 16-bit for 5-bit selectors)

        # Privilege level: 0 = supervisor, 1 = user
        self.priv_level: int = 0

        # MPU window (user-mode data access bounds)
        self.mpu_base: int = 0
        self.mpu_limit: int = 0

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

        # Port I/O bridge — maps port N to a 12-bit MMIO offset.
        # OUT N writes the byte to MMIO_START + port_map[N].
        # INP N reads the byte from MMIO_START + port_map[N].
        # 0 = disabled (legacy port_out/port_in behavior only).
        from devices import DEFAULT_PORT_MAP
        self.port_map: dict[int, int] = dict(DEFAULT_PORT_MAP)

        # State
        self.halted: bool = False
        self.idle: bool   = False
        self.cycle_count: int = 0

        # Strided / 2D tile addressing
        self.tstride_r: int = 0    # row stride in bytes (0 = disabled)
        self.tstride_c: int = 0    # column stride (reserved)
        self.ttile_h: int   = 8    # tile height (rows, 1-8)
        self.ttile_w: int   = 8    # tile width (bytes per row, 1-64)

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

        # I-cache emulation state (behavioural – no actual cache modelled)
        self.icache_enabled: int = 1   # 1 = cache enabled
        self.icache_hits: int    = 0   # hit counter (always 0 in emulator)
        self.icache_misses: int  = 0   # miss counter (always 0 in emulator)

        # DMA ring buffer state
        self.dma_ring_base: int = 0
        self.dma_ring_size: int = 0
        self.dma_head: int      = 0
        self.dma_tail: int      = 0
        self.dma_status: int    = 0
        self.dma_ctrl: int      = 0

        # QoS state
        self.qos_weight: int    = 0
        self.qos_bwlimit: int   = 0

        # EXT prefix state
        self._ext_modifier: int = -1  # -1 = no active prefix

        # EXT.CRYPTO CRC per-core state (Appendix B, §B.3)
        self.crc_acc: int  = 0xFFFF_FFFF  # CRC accumulator
        self.crc_mode: int = 0            # 0=CRC32, 1=CRC32C, 2=CRC64

        # EXT.CRYPTO SHA-2 per-core state (Appendix B, §B.4)
        self.sha_mode: int      = 0   # 0=SHA-256, 1=SHA-384, 2=SHA-512
        self.sha_msglen_lo: int = 0   # message length in bits (low 64)
        self.sha_msglen_hi: int = 0   # message length in bits (high 64)

        # EXT.CRYPTO Field ALU per-core state (Appendix B, §B.5)
        self.gf_prime_sel: int = 0     # 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom
        self.gf_custom_p:  int = 0     # 256-bit custom prime
        self.gf_mont_pinv: int = 0     # -p^{-1} mod 2^{256} for Montgomery REDC
        self.gf_prev_lo:   int = 0     # 256-bit previous result (low)  for FCMOV/FMAC/MACR
        self.gf_prev_hi:   int = 0     # 256-bit previous result (high) for MACR

        # EXT.DICT hardware dictionary hash table (behavioural model)
        # 64 sets × 4 ways.  Each entry:
        #   (valid, hash32, name_len, name_bytes, xt)
        self._dict_table: list[list[tuple[bool, int, int, bytes, int]]] = [
            [(False, 0, 0, b"", 0) for _ in range(4)]
            for _ in range(64)
        ]

        # Extended memory regions (attached by system.py)
        self._vram_mem: Optional[bytearray] = None
        self._vram_base: int = 0
        self._vram_size: int = 0
        self._hbw_mem: Optional[bytearray] = None
        self._hbw_base: int = 0
        self._hbw_size: int = 0
        self._ext_mem: Optional[bytearray] = None
        self._ext_mem_base: int = 0
        self._ext_mem_size: int = 0

        # Callbacks
        self.on_output: Optional[callable] = None  # called with (port, value)
        self.on_halt: Optional[callable] = None

    # REX prefix helpers — extract register extension bits from _ext_modifier
    @property
    def _rex_s(self) -> int:
        """REX source high bit (ext_mod bit 0)."""
        m = self._ext_modifier
        return (m & 1) if m >= 1 and m <= 5 else 0

    @property
    def _rex_d(self) -> int:
        """REX dest high bit (ext_mod bit 1)."""
        m = self._ext_modifier
        return ((m >> 1) & 1) if m >= 1 and m <= 5 else 0

    @property
    def _rex_n(self) -> int:
        """REX nibble high bit (ext_mod bit 2)."""
        m = self._ext_modifier
        return ((m >> 2) & 1) if m >= 1 and m <= 5 else 0

    # -- Extended memory attach (called by system.py) --

    def attach_hbw(self, buf: bytearray, base: int, size: int):
        """Attach HBW math RAM buffer."""
        self._hbw_mem = buf
        self._hbw_base = base
        self._hbw_size = size

    def attach_ext_mem(self, buf: bytearray, base: int, size: int):
        """Attach external memory (HyperRAM/SDRAM) buffer."""
        self._ext_mem = buf
        self._ext_mem_base = base
        self._ext_mem_size = size

    def attach_vram(self, buf: bytearray, base: int, size: int):
        """Attach dedicated VRAM buffer."""
        self._vram_mem = buf
        self._vram_base = base
        self._vram_size = size

    # -- Tile memory helpers (unified address resolution) --

    def _read_tile(self, addr: int) -> bytearray:
        """Read a 64-byte tile from unified address space."""
        a = u64(addr)
        if self._vram_mem and self._vram_base <= a < self._vram_base + self._vram_size:
            off = a - self._vram_base
            if off + 64 <= self._vram_size:
                return bytearray(self._vram_mem[off:off+64])
            return bytearray(64)
        if self._ext_mem and self._ext_mem_base <= a < self._ext_mem_base + self._ext_mem_size:
            off = a - self._ext_mem_base
            if off + 64 <= self._ext_mem_size:
                return bytearray(self._ext_mem[off:off+64])
            return bytearray(64)
        if self._hbw_mem and self._hbw_base <= a < self._hbw_base + self._hbw_size:
            off = a - self._hbw_base
            if off + 64 <= self._hbw_size:
                return bytearray(self._hbw_mem[off:off+64])
            return bytearray(64)
        a = a % self.mem_size
        if a + 64 <= self.mem_size:
            return bytearray(self.mem[a:a+64])
        return bytearray(64)

    def _write_tile(self, addr: int, data: bytearray):
        """Write a 64-byte tile to unified address space."""
        a = u64(addr)
        if self._vram_mem and self._vram_base <= a < self._vram_base + self._vram_size:
            off = a - self._vram_base
            if off + 64 <= self._vram_size:
                self._vram_mem[off:off+64] = data[:64]
            return
        if self._ext_mem and self._ext_mem_base <= a < self._ext_mem_base + self._ext_mem_size:
            off = a - self._ext_mem_base
            if off + 64 <= self._ext_mem_size:
                self._ext_mem[off:off+64] = data[:64]
            return
        if self._hbw_mem and self._hbw_base <= a < self._hbw_base + self._hbw_size:
            off = a - self._hbw_base
            if off + 64 <= self._hbw_size:
                self._hbw_mem[off:off+64] = data[:64]
            return
        a = a % self.mem_size
        if a + 64 <= self.mem_size:
            self.mem[a:a+64] = data[:64]

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

    def _resolve_addr(self, addr: int) -> tuple:
        """Resolve a unified address to (buffer, offset, buf_size).

        Checks VRAM, XMEM, and HBW apertures before falling back to
        Bank 0 (system RAM).  Mirrors the RTL address decode in
        mp64_memory.v so that string instructions and scalar
        load/stores route correctly to all memory regions.
        """
        a = u64(addr)
        if self._vram_mem and self._vram_base <= a < self._vram_base + self._vram_size:
            return self._vram_mem, a - self._vram_base, self._vram_size
        if self._ext_mem and self._ext_mem_base <= a < self._ext_mem_base + self._ext_mem_size:
            return self._ext_mem, a - self._ext_mem_base, self._ext_mem_size
        if self._hbw_mem and self._hbw_base <= a < self._hbw_base + self._hbw_size:
            return self._hbw_mem, a - self._hbw_base, self._hbw_size
        return self.mem, a % self.mem_size, self.mem_size

    def _check_addr(self, addr: int, size: int = 1):
        addr = u64(addr)
        if addr + size > self.mem_size:
            self.trap_addr = addr
            raise TrapError(IVEC_BUS_FAULT, f"Bus fault @ {addr:#018x}")

    def mem_read8(self, addr: int) -> int:
        buf, off, _ = self._resolve_addr(addr)
        return buf[off]

    def mem_write8(self, addr: int, val: int):
        buf, off, _ = self._resolve_addr(addr)
        buf[off] = val & 0xFF

    def mem_read16(self, addr: int) -> int:
        buf, off, sz = self._resolve_addr(addr)
        return buf[off] | (buf[(off+1) % sz] << 8)

    def mem_write16(self, addr: int, val: int):
        buf, off, sz = self._resolve_addr(addr)
        buf[off]            = val & 0xFF
        buf[(off+1) % sz]   = (val >> 8) & 0xFF

    def mem_read32(self, addr: int) -> int:
        buf, off, sz = self._resolve_addr(addr)
        v = 0
        for i in range(4):
            v |= buf[(off+i) % sz] << (8*i)
        return v

    def mem_write32(self, addr: int, val: int):
        buf, off, sz = self._resolve_addr(addr)
        for i in range(4):
            buf[(off+i) % sz] = (val >> (8*i)) & 0xFF

    def mem_read64(self, addr: int) -> int:
        buf, off, sz = self._resolve_addr(addr)
        v = 0
        for i in range(8):
            v |= buf[(off+i) % sz] << (8*i)
        return v

    def mem_write64(self, addr: int, val: int):
        buf, off, sz = self._resolve_addr(addr)
        for i in range(8):
            buf[(off+i) % sz] = (val >> (8*i)) & 0xFF

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
            CSR_PRIV:       lambda: self.priv_level,
            CSR_MPU_BASE:   lambda: self.mpu_base,
            CSR_MPU_LIMIT:  lambda: self.mpu_limit,
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
            CSR_MEGAPAD_SZ: lambda: self.mem_size,
            CSR_CPUID:      lambda: 0x4D50_3634_0001_4350,  # "MP64" v1 "CP"
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
            CSR_ICACHE_CTRL:    lambda: self.icache_enabled & 1,
            CSR_ICACHE_HITS:    lambda: u64(self.icache_hits),
            CSR_ICACHE_MISSES:  lambda: u64(self.icache_misses),
            CSR_TSTRIDE_R:      lambda: self.tstride_r,
            CSR_TSTRIDE_C:      lambda: self.tstride_c,
            CSR_TTILE_H:        lambda: self.ttile_h,
            CSR_TTILE_W:        lambda: self.ttile_w,
            CSR_DMA_RING_BASE:  lambda: self.dma_ring_base,
            CSR_DMA_RING_SIZE:  lambda: self.dma_ring_size,
            CSR_DMA_HEAD:       lambda: self.dma_head,
            CSR_DMA_TAIL:       lambda: self.dma_tail,
            CSR_DMA_STATUS:     lambda: self.dma_status,
            CSR_DMA_CTRL:       lambda: self.dma_ctrl,
            CSR_QOS_WEIGHT:     lambda: self.qos_weight,
            CSR_QOS_BWLIMIT:    lambda: self.qos_bwlimit,
            # EXT.CRYPTO CSRs (Appendix B)
            CSR_CRC_ACC:        lambda: self.crc_acc,
            CSR_CRC_MODE:       lambda: self.crc_mode,
            CSR_SHA_MODE:       lambda: self.sha_mode,
            CSR_SHA_MSGLEN:     lambda: self.sha_msglen_lo,
            CSR_SHA_MSGLEN_HI:  lambda: self.sha_msglen_hi,
            CSR_GF_PRIME_SEL:   lambda: self.gf_prime_sel,
        }
        fn = m.get(addr)
        if fn is None:
            return 0
        return fn() if callable(fn) else fn

    def csr_write(self, addr: int, val: int):
        val = u64(val)
        dispatch = {
            CSR_FLAGS:    lambda v: self.flags_unpack(v & 0xFF),
            CSR_PSEL:     lambda v: setattr(self, 'psel',  v & 0x1F),
            CSR_XSEL:     lambda v: setattr(self, 'xsel',  v & 0x1F),
            CSR_SPSEL:    lambda v: setattr(self, 'spsel', v & 0x1F),
            CSR_IVT_BASE: lambda v: setattr(self, 'ivt_base', v),
            CSR_D:        lambda v: setattr(self, 'd_reg', v & 0xFF),
            CSR_DF:       lambda v: setattr(self, 'flag_c', v & 1),
            CSR_Q:        lambda v: setattr(self, 'q_out',  v & 1),
            CSR_T:        lambda v: setattr(self, 't_reg',  v & 0xFFFF),
            CSR_IE:       lambda v: setattr(self, 'flag_i', v & 1),
            CSR_PRIV:     lambda v: setattr(self, 'priv_level', v & 1),
            CSR_MPU_BASE: lambda v: setattr(self, 'mpu_base', v),
            CSR_MPU_LIMIT:lambda v: setattr(self, 'mpu_limit', v),
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
            CSR_ICACHE_CTRL:   lambda v: self._icache_ctrl_write(v),
            CSR_TSTRIDE_R: lambda v: setattr(self, 'tstride_r', v & 0xFFFFF),
            CSR_TSTRIDE_C: lambda v: setattr(self, 'tstride_c', v & 0xFFFFF),
            CSR_TTILE_H:   lambda v: setattr(self, 'ttile_h', max(1, min(8, v & 0xFF))),
            CSR_TTILE_W:   lambda v: setattr(self, 'ttile_w', max(1, min(64, v & 0xFF))),
            CSR_DMA_RING_BASE: lambda v: setattr(self, 'dma_ring_base', v),
            CSR_DMA_RING_SIZE: lambda v: setattr(self, 'dma_ring_size', v),
            CSR_DMA_HEAD:      lambda v: setattr(self, 'dma_head', v),
            CSR_DMA_TAIL:      lambda v: setattr(self, 'dma_tail', v),
            CSR_DMA_STATUS:    lambda v: setattr(self, 'dma_status', v),
            CSR_DMA_CTRL:      lambda v: self._dma_ctrl_write(v),
            CSR_QOS_WEIGHT:    lambda v: setattr(self, 'qos_weight', v),
            CSR_QOS_BWLIMIT:   lambda v: setattr(self, 'qos_bwlimit', v),
            # EXT.CRYPTO CSRs (Appendix B)
            CSR_CRC_ACC:       lambda v: setattr(self, 'crc_acc', v & 0xFFFF_FFFF_FFFF_FFFF),
            CSR_CRC_MODE:      lambda v: setattr(self, 'crc_mode', v & 0x03),
            CSR_SHA_MODE:      lambda v: setattr(self, 'sha_mode', v & 0x03),
            CSR_SHA_MSGLEN:    lambda v: setattr(self, 'sha_msglen_lo', v & MASK64),
            CSR_SHA_MSGLEN_HI: lambda v: setattr(self, 'sha_msglen_hi', v & MASK64),
            CSR_GF_PRIME_SEL:  lambda v: setattr(self, 'gf_prime_sel', v & 0x03),
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

    def _icache_ctrl_write(self, val: int):
        """Handle writes to CSR_ICACHE_CTRL.

        Bit 0: enable/disable cache.
        Bit 1: if set, invalidate all lines (auto-clears).
        """
        self.icache_enabled = val & 1
        if val & 2:                 # invalidate-all bit
            self.icache_hits   = 0
            self.icache_misses = 0

    def _dma_ctrl_write(self, val: int):
        """Handle writes to CSR_DMA_CTRL — matches RTL mp64_cpu.v.

        Bit 1: reset pointers and status.
        """
        self.dma_ctrl = u64(val)
        if val & 2:
            self.dma_head = 0
            self.dma_tail = 0
            self.dma_status = 0

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
        ew_bits = self.tmode & 0x7
        is_fp = ew_bits >= EW_FP16
        if is_fp:
            elem_bytes = 2  # both fp16 and bf16 are 16-bit
        else:
            elem_bytes = 1 << ew_bits
        num_lanes = 64 // elem_bytes
        signed = (self.tmode >> 4) & 1

        read_tile = self._read_tile
        write_tile = self._write_tile
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
            if is_fp:
                # ---- Floating-point TALU (self-test) ----
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if funct == 0:  # ADD
                        fa = _fp_decode(ea, ew_bits)
                        fb = _fp_decode(eb_val, ew_bits)
                        r = _fp_encode(fa + fb, ew_bits)
                    elif funct == 1:  # SUB
                        fa = _fp_decode(ea, ew_bits)
                        fb = _fp_decode(eb_val, ew_bits)
                        r = _fp_encode(fa - fb, ew_bits)
                    elif funct == 2:  # AND — bitwise
                        r = ea & eb_val
                    elif funct == 3:  # OR — bitwise
                        r = ea | eb_val
                    elif funct == 4:  # XOR — bitwise
                        r = ea ^ eb_val
                    elif funct == 7:  # ABS — clear sign bit
                        r = ea & 0x7FFF
                    elif funct == 5:  # MIN
                        if _fp_is_nan(ea, ew_bits) or _fp_is_nan(eb_val, ew_bits):
                            r = 0x7E00 if ew_bits == EW_FP16 else 0x7FC0
                        else:
                            fa = _fp_decode(ea, ew_bits)
                            fb = _fp_decode(eb_val, ew_bits)
                            r = _fp_encode(min(fa, fb), ew_bits)
                    elif funct == 6:  # MAX
                        if _fp_is_nan(ea, ew_bits) or _fp_is_nan(eb_val, ew_bits):
                            r = 0x7E00 if ew_bits == EW_FP16 else 0x7FC0
                        else:
                            fa = _fp_decode(ea, ew_bits)
                            fb = _fp_decode(eb_val, ew_bits)
                            r = _fp_encode(max(fa, fb), ew_bits)
                    else:
                        r = 0
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
            else:
                # ---- Integer TALU (self-test) ----
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
            if is_fp:
                # ---- Floating-point TMUL (self-test) ----
                if funct == 0:  # MUL
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        tile_set_elem(dst, lane, elem_bytes, _fp_encode(fa * fb, ew_bits))
                    write_tile(self.tdst, dst)
                elif funct == 1:  # DOT — FP → FP32 accumulate
                    total = 0.0
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        total += float(fa) * float(fb)
                    self.acc[0] = _fp32_to_bits(total)
                    self.acc[1] = 0
            else:
                # ---- Integer TMUL (self-test) ----
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
            if is_fp:
                # ---- Floating-point TRED (self-test) ----
                fp_vals = [_fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                           for lane in range(num_lanes)]
                if funct == 0:  # SUM
                    total = sum(float(v) for v in fp_vals)
                    self.acc[0] = _fp32_to_bits(total)
                    self.acc[1] = 0
                elif funct == 1:  # MIN
                    self.acc[0] = _fp32_to_bits(min(fp_vals))
                    self.acc[1] = 0
                elif funct == 2:  # MAX
                    self.acc[0] = _fp32_to_bits(max(fp_vals))
                    self.acc[1] = 0
            else:
                # ---- Integer TRED (self-test) ----
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

    # -- EXT.STRING (prefix F9) --

    def _exec_string(self) -> int:
        """Execute EXT.STRING (F9) instruction.

        Encoding: F9 <sub-op> <reg-byte>
        Sub-ops: 00=CMOVE, 01=CMOVE>, 02=BFILL, 03=BCOMP, 04=BSRCH
        reg-byte: [Rd:4][Rs:4]  (REX extends to R16-R31)
        """
        sub_op = self.fetch8()
        reg_byte = self.fetch8()
        rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
        rs = (self._rex_s << 4) | (reg_byte & 0xF)

        if sub_op == 0x00:
            return self._string_cmove(rd, rs, forward=True)
        elif sub_op == 0x01:
            return self._string_cmove(rd, rs, forward=False)
        elif sub_op == 0x02:
            return self._string_bfill(rd, rn=rs)
        elif sub_op == 0x03:
            return self._string_bcomp(rd, rs)
        elif sub_op == 0x04:
            return self._string_bsrch(rd, rs)
        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.STRING sub-op {sub_op:#x} reserved")

    def _string_cmove(self, rd: int, rs: int, forward: bool) -> int:
        """CMOVE / CMOVE> — block byte copy."""
        src = self.regs[rs]
        dst = self.regs[rd]
        ln = self.regs[0] & MASK64
        if ln > 0:
            if forward:
                for i in range(ln):
                    b = self.mem_read8(u64(src + i))
                    self.mem_write8(u64(dst + i), b)
            else:
                for i in range(ln - 1, -1, -1):
                    b = self.mem_read8(u64(src + i))
                    self.mem_write8(u64(dst + i), b)
        self.regs[rs] = u64(src + ln)
        self.regs[rd] = u64(dst + ln)
        self.regs[0] = 0
        return ln + 2  # N reads + N writes + setup + done

    def _string_bfill(self, rd: int, rn: int) -> int:
        """BFILL — fill block with D[7:0]."""
        dst = self.regs[rd]
        ln = self.regs[rn] & MASK64
        fb = self.d_reg & 0xFF
        for i in range(ln):
            self.mem_write8(u64(dst + i), fb)
        self.regs[rd] = u64(dst + ln)
        self.regs[rn] = 0
        return ln + 2

    def _string_bcomp(self, rd: int, rs: int) -> int:
        """BCOMP — block byte compare."""
        src = self.regs[rs]
        dst = self.regs[rd]
        ln = self.regs[0] & MASK64
        cycles = 2
        remaining = ln
        for i in range(ln):
            sb = self.mem_read8(u64(src + i))
            db = self.mem_read8(u64(dst + i))
            cycles += 1
            remaining -= 1
            if sb != db:
                self.flag_z = 0
                self.flag_g = 1 if db > sb else 0
                self.regs[rs] = u64(src + i)
                self.regs[rd] = u64(dst + i)
                self.regs[0] = u64(remaining + 1)
                return cycles
        # All bytes matched
        self.flag_z = 1
        self.flag_g = 0
        self.regs[rs] = u64(src + ln)
        self.regs[rd] = u64(dst + ln)
        self.regs[0] = 0
        return cycles

    def _string_bsrch(self, rd: int, rs: int) -> int:
        """BSRCH — search for D[7:0] in block at Rd."""
        dst = self.regs[rd]
        ln = self.regs[0] & MASK64
        needle = self.d_reg & 0xFF
        cycles = 2
        for i in range(ln):
            b = self.mem_read8(u64(dst + i))
            cycles += 1
            if b == needle:
                self.flag_z = 1
                self.regs[rs] = u64(i)           # offset
                self.regs[rd] = u64(dst + i)
                self.regs[0] = u64(ln - i)
                return cycles
        # Not found
        self.flag_z = 0
        self.regs[rs] = u64(ln)
        self.regs[rd] = u64(dst + ln)
        self.regs[0] = 0
        return cycles

    # ================================================================
    # EXT.DICT (FA) — Hardware dictionary search engine
    # ================================================================

    @staticmethod
    def _fnv1a_32(data: bytes) -> int:
        """FNV-1a 32-bit hash, matching RTL implementation."""
        h = 0x811C9DC5
        for b in data:
            h = ((h ^ b) * 0x01000193) & 0xFFFF_FFFF
        return h

    def _exec_dict(self) -> int:
        """Execute EXT.DICT (FA) instruction.

        Encoding: FA <sub-op> <reg-byte>
        Sub-ops: 00=DFIND, 01=DINS, 02=DDEL, 03=DCLR
        reg-byte: [Rd:4][Rs:4]  (REX extends to R16-R31)
        """
        sub_op = self.fetch8()
        reg_byte = self.fetch8()
        rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
        rs = (self._rex_s << 4) | (reg_byte & 0xF)

        if sub_op == 0x00:
            return self._dict_find(rd, rs)
        elif sub_op == 0x01:
            return self._dict_insert(rd, rs)
        elif sub_op == 0x02:
            return self._dict_delete(rd, rs)
        elif sub_op == 0x03:
            return self._dict_clear()
        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.DICT sub-op {sub_op:#x} reserved")

    def _dict_read_name(self, addr: int) -> tuple[bytes, int]:
        """Read a counted-string from memory, return (name_bytes, cycles)."""
        nlen = self.mem_read8(addr) & 0x1F  # 5-bit, max 31
        name = bytes(self.mem_read8(u64(addr + 1 + i)) for i in range(nlen))
        return name, 2 + nlen  # 1 for len byte + nlen name bytes + 1 hash

    def _dict_find(self, rd: int, rs: int) -> int:
        """DFIND: lookup counted-string at R[rs]; R[rd] ← XT if found."""
        addr = self.regs[rs]
        name, cycles = self._dict_read_name(addr)
        h = self._fnv1a_32(name)
        set_idx = h & 0x3F
        for valid, way_h, way_nlen, way_name, way_xt in self._dict_table[set_idx]:
            if valid and way_h == h and way_nlen == len(name) and way_name == name:
                self.regs[rd] = way_xt & MASK64
                self.flag_z = 1
                self.flag_v = 0
                return cycles
        self.regs[rd] = 0
        self.flag_z = 0
        self.flag_v = 0
        return cycles

    def _dict_insert(self, rd: int, rs: int) -> int:
        """DINS: insert name at R[rs] with XT from R[rd]."""
        addr = self.regs[rs]
        xt = self.regs[rd]
        name, cycles = self._dict_read_name(addr)
        h = self._fnv1a_32(name)
        set_idx = h & 0x3F
        ways = self._dict_table[set_idx]
        # Update existing match
        for i, (valid, way_h, way_nlen, way_name, _) in enumerate(ways):
            if valid and way_h == h and way_nlen == len(name) and way_name == name:
                ways[i] = (True, h, len(name), name, xt & MASK64)
                self.flag_z = 1
                self.flag_v = 0
                return cycles
        # Insert into first empty way
        for i, (valid, *_rest) in enumerate(ways):
            if not valid:
                ways[i] = (True, h, len(name), name, xt & MASK64)
                self.flag_z = 1
                self.flag_v = 0
                return cycles
        # Set full — overflow
        self.flag_z = 0
        self.flag_v = 1
        return cycles

    def _dict_delete(self, rd: int, rs: int) -> int:
        """DDEL: delete entry matching name at R[rs]."""
        addr = self.regs[rs]
        name, cycles = self._dict_read_name(addr)
        h = self._fnv1a_32(name)
        set_idx = h & 0x3F
        ways = self._dict_table[set_idx]
        for i, (valid, way_h, way_nlen, way_name, _) in enumerate(ways):
            if valid and way_h == h and way_nlen == len(name) and way_name == name:
                ways[i] = (False, 0, 0, b"", 0)
                self.flag_z = 1
                return cycles
        self.flag_z = 0
        return cycles

    def _dict_clear(self) -> int:
        """DCLR: clear entire hash table."""
        for s in range(64):
            for w in range(4):
                self._dict_table[s][w] = (False, 0, 0, b"", 0)
        return 66  # ~64 cycles for bulk clear

    # -- EXT.CRYPTO (prefix FB) --

    # CRC polynomials (normal / MSB-first form, matching RTL mp64_crc.v)
    _CRC_POLYS = {
        0: (0x04C11DB7, 32),          # CRC32 IEEE 802.3
        1: (0x1EDC6F41, 32),          # CRC32C (Castagnoli)
        2: (0x42F0E1EBA9EA3693, 64),  # CRC64 ECMA-182
    }

    @staticmethod
    def _crc_update_byte(acc: int, byte: int, poly: int, width: int) -> int:
        """Process one byte through the CRC, MSB-first (matching RTL)."""
        if width == 64:
            acc ^= byte << 56
            for _ in range(8):
                if acc & (1 << 63):
                    acc = ((acc << 1) & 0xFFFF_FFFF_FFFF_FFFF) ^ poly
                else:
                    acc = (acc << 1) & 0xFFFF_FFFF_FFFF_FFFF
        else:  # width == 32
            acc ^= byte << 24
            for _ in range(8):
                if acc & (1 << 31):
                    acc = ((acc << 1) & 0xFFFF_FFFF) ^ poly
                else:
                    acc = (acc << 1) & 0xFFFF_FFFF
        return acc

    def _exec_crypto(self) -> int:
        """Execute EXT.CRYPTO (FB) instruction.

        Encoding: FB <sub-op> [DR | imm8]
        sub-op[7:4] = unit: 0=CRC, 1=SHA-2, 2=Field ALU
        sub-op[3:0] = operation within unit
        """
        sub_op = self.fetch8()
        unit = (sub_op >> 4) & 0xF
        op = sub_op & 0xF

        if unit == 0x0:
            return self._exec_crc(op)
        elif unit == 0x1:
            return self._exec_sha(op)
        elif unit == 0x2:
            return self._exec_field(op)
        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.CRYPTO unit {unit:#x} reserved")

    def _exec_crc(self, op: int) -> int:
        """CRC sub-ops (FB 00–0F).
        CRC_ACC (CSR 0x80) and CRC_MODE (CSR 0x81) are per-core state."""
        poly_info = self._CRC_POLYS.get(self.crc_mode, self._CRC_POLYS[0])
        poly, width = poly_info

        if op == 0x0:  # CRC.INIT
            if self.crc_mode >= 2:
                self.crc_acc = 0xFFFF_FFFF_FFFF_FFFF
            else:
                self.crc_acc = 0xFFFF_FFFF
            return 1

        elif op == 0x1:  # CRC.B Rd, Rs — feed one byte
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            rs = (self._rex_s << 4) | (reg_byte & 0xF)
            b = self.regs[rs] & 0xFF
            self.crc_acc = self._crc_update_byte(self.crc_acc, b, poly, width)
            self.regs[rd] = self.crc_acc & MASK64
            return 1

        elif op == 0x2:  # CRC.Q Rd, Rs — feed 8 bytes (LE order)
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            rs = (self._rex_s << 4) | (reg_byte & 0xF)
            val = self.regs[rs]
            acc = self.crc_acc
            for i in range(8):
                b = (val >> (i * 8)) & 0xFF
                acc = self._crc_update_byte(acc, b, poly, width)
            self.crc_acc = acc
            self.regs[rd] = acc & MASK64
            return 1

        elif op == 0x3:  # CRC.FIN Rd, Rs — finalize
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            mask = 0xFFFF_FFFF_FFFF_FFFF if self.crc_mode >= 2 else 0xFFFF_FFFF
            self.regs[rd] = (self.crc_acc ^ mask) & MASK64
            return 1

        elif op == 0x4:  # CRC.MODE imm8
            imm8 = self.fetch8()
            self.crc_mode = imm8 & 0x03
            return 1

        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.CRYPTO CRC sub-op {op:#x} reserved")

    # ------------------------------------------------------------------
    #  EXT.CRYPTO SHA-2 ISA  (sub-ops FB 10–1F, §B.4)
    # ------------------------------------------------------------------

    def _sha_unpack_state(self) -> list[int]:
        """Unpack 8 hash words H[0..7] from ACC0-ACC3 (+ R16-R19 for 512)."""
        if self.sha_mode == 0:  # SHA-256: 2 × 32-bit per ACC
            return [
                (self.acc[0] >> 32) & _M32, self.acc[0] & _M32,
                (self.acc[1] >> 32) & _M32, self.acc[1] & _M32,
                (self.acc[2] >> 32) & _M32, self.acc[2] & _M32,
                (self.acc[3] >> 32) & _M32, self.acc[3] & _M32,
            ]
        else:  # SHA-384 / SHA-512: 64-bit per slot
            return [
                self.acc[0], self.acc[1], self.acc[2], self.acc[3],
                self.regs[16], self.regs[17], self.regs[18], self.regs[19],
            ]

    def _sha_pack_state(self, H: list[int]):
        """Pack 8 hash words back into ACC0-ACC3 (+ R16-R19 for 512)."""
        if self.sha_mode == 0:  # SHA-256
            self.acc[0] = ((H[0] & _M32) << 32) | (H[1] & _M32)
            self.acc[1] = ((H[2] & _M32) << 32) | (H[3] & _M32)
            self.acc[2] = ((H[4] & _M32) << 32) | (H[5] & _M32)
            self.acc[3] = ((H[6] & _M32) << 32) | (H[7] & _M32)
        else:
            self.acc[0], self.acc[1] = H[0], H[1]
            self.acc[2], self.acc[3] = H[2], H[3]
            self.regs[16], self.regs[17] = H[4], H[5]
            self.regs[18], self.regs[19] = H[6], H[7]

    def _sha_block_size(self) -> int:
        return 128 if self.sha_mode >= 1 else 64

    def _sha_read_block(self) -> bytes:
        """Read one message block from tile memory at TSRC0."""
        bsz = self._sha_block_size()
        base = u64(self.tsrc0)
        buf, off, sz = self._resolve_addr(base)
        return bytes(buf[off:off + bsz])

    def _sha_compress(self) -> int:
        """Run SHA-2 compression on the block currently at M[TSRC0].
        Returns cycle count."""
        H = self._sha_unpack_state()
        block = self._sha_read_block()
        if self.sha_mode == 0:
            H = _sha256_compress(H, block)
            cycles = 64
        else:
            H = _sha512_compress(H, block)
            cycles = 80
        self._sha_pack_state(H)
        self.flag_z = True
        return cycles

    def _sha_write_pad(self) -> bool:
        """Apply FIPS 180-4 padding at M[TSRC0].  R0 = byte pos in block.
        Returns True if a two-block pad is needed (C flag)."""
        bsz = self._sha_block_size()
        lsz = 16 if self.sha_mode >= 1 else 8  # length field size
        pos = int(self.regs[0] & MASK64) % bsz
        base = u64(self.tsrc0)

        # 0x80 byte
        self.mem_write8(base + pos, 0x80)
        pos += 1

        two_blocks = pos > (bsz - lsz)

        if two_blocks:
            # fill rest of block with zeros
            while pos < bsz:
                self.mem_write8(base + pos, 0x00)
                pos += 1
            self.flag_c = True
            return True
        else:
            # zero-fill up to length field
            while pos < bsz - lsz:
                self.mem_write8(base + pos, 0x00)
                pos += 1
            # write big-endian length
            lo, hi = self.sha_msglen_lo, self.sha_msglen_hi
            if self.sha_mode >= 1:  # 128-bit length
                for i in range(8):
                    self.mem_write8(base + bsz - 16 + i,
                                   (hi >> (56 - i * 8)) & 0xFF)
            for i in range(8):
                self.mem_write8(base + bsz - 8 + i,
                               (lo >> (56 - i * 8)) & 0xFF)
            self.flag_c = False
            return False

    def _exec_sha(self, op: int) -> int:
        """SHA-2 sub-ops (FB 10–1F)."""

        if op == 0x0:  # SHA.INIT imm8
            imm8 = self.fetch8()
            mode = imm8 & 0x03
            self.sha_mode = mode
            self.sha_msglen_lo = 0
            self.sha_msglen_hi = 0
            if mode == 0:
                self._sha_pack_state(list(_SHA256_IV))
            elif mode == 1:
                self._sha_pack_state(list(_SHA384_IV))
            else:
                self._sha_pack_state(list(_SHA512_IV))
            return 2

        elif op == 0x1:  # SHA.ROUND
            return self._sha_compress()

        elif op == 0x2:  # SHA.PAD
            self._sha_write_pad()
            return 3

        elif op == 0x3:  # SHA.DIN Rd, Rs
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            rs = (self._rex_s << 4) | (reg_byte & 0xF)
            byte_val = self.regs[rs] & 0xFF
            base = u64(self.tsrc0)
            r0 = int(self.regs[0] & MASK64)
            self.mem_write8(base + r0, byte_val)
            r0 += 1
            # track total message length
            self.sha_msglen_lo = (self.sha_msglen_lo + 8) & MASK64
            if self.sha_msglen_lo < 8:  # overflow
                self.sha_msglen_hi = (self.sha_msglen_hi + 1) & MASK64
            # auto-round when block is full
            bsz = self._sha_block_size()
            cycles = 1
            if r0 >= bsz:
                cycles += self._sha_compress()
                r0 = 0
            self.regs[0] = u64(r0)
            self.regs[rd] = u64(r0)
            return cycles

        elif op == 0x4:  # SHA.DOUT Rd, Rs
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            rs = (self._rex_s << 4) | (reg_byte & 0xF)
            idx = int(self.regs[rs]) & 0x7
            H = self._sha_unpack_state()
            self.regs[rd] = H[idx] & MASK64
            return 1

        elif op == 0x5:  # SHA.FINAL
            two_blocks = self._sha_write_pad()
            cycles = 3
            if two_blocks:
                cycles += self._sha_compress()
                # write second pad block (all zeros + length)
                bsz = self._sha_block_size()
                lsz = 16 if self.sha_mode >= 1 else 8
                base = u64(self.tsrc0)
                for i in range(bsz - lsz):
                    self.mem_write8(base + i, 0x00)
                lo, hi = self.sha_msglen_lo, self.sha_msglen_hi
                if self.sha_mode >= 1:
                    for i in range(8):
                        self.mem_write8(base + bsz - 16 + i,
                                       (hi >> (56 - i * 8)) & 0xFF)
                for i in range(8):
                    self.mem_write8(base + bsz - 8 + i,
                                   (lo >> (56 - i * 8)) & 0xFF)
            cycles += self._sha_compress()
            return cycles

        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.CRYPTO SHA-2 sub-op {op:#x} reserved")

    # ------------------------------------------------------------------
    #  EXT.CRYPTO Field ALU ISA  (sub-ops FB 20–2F, §B.5)
    # ------------------------------------------------------------------

    def _gf_acc_to_int(self) -> int:
        """Pack ACC0-ACC3 into a 256-bit little-endian integer."""
        return (self.acc[0]
                | (self.acc[1] << 64)
                | (self.acc[2] << 128)
                | (self.acc[3] << 192))

    def _gf_int_to_acc(self, v: int):
        """Unpack a 256-bit integer back into ACC0-ACC3."""
        v &= _M256
        self.acc[0] = v & MASK64
        self.acc[1] = (v >> 64) & MASK64
        self.acc[2] = (v >> 128) & MASK64
        self.acc[3] = (v >> 192) & MASK64

    def _gf_read_tile_b(self) -> int:
        """Read 32 bytes from M[TSRC0] as a 256-bit little-endian integer."""
        base = u64(self.tsrc0)
        val = 0
        for i in range(32):
            val |= self.mem_read8(base + i) << (i * 8)
        return val

    def _gf_write_tile_dst(self, v: int):
        """Write 32 bytes to M[TDST] as little-endian."""
        base = u64(self.tdst)
        v &= _M256
        for i in range(32):
            self.mem_write8(base + i, (v >> (i * 8)) & 0xFF)

    def _gf_prime(self) -> int:
        return _gf_get_prime(self.gf_prime_sel, self.gf_custom_p)

    def _gf_is_mont(self) -> bool:
        return self.gf_prime_sel == 3 and self.gf_mont_pinv != 0

    def _gf_mulmod_sel(self, a: int, b: int, p: int) -> int:
        """Multiply with Montgomery REDC when custom prime + p_inv set."""
        if self._gf_is_mont():
            return _gf_mont_mulmod(a, b, p, self.gf_mont_pinv)
        return _gf_mulmod(a, b, p)

    def _gf_sqrmod_sel(self, a: int, p: int) -> int:
        if self._gf_is_mont():
            return _gf_mont_sqrmod(a, p, self.gf_mont_pinv)
        return _gf_sqrmod(a, p)

    def _exec_field(self, op: int) -> int:
        """Field ALU sub-ops (FB 20–2F).

        All 256-bit operands:
          A = ACC0-ACC3
          B = M[TSRC0] (32 bytes, little-endian)
        Result → ACC0-ACC3 (and optionally M[TDST] for high half).
        """
        p = self._gf_prime()

        if op == 0x0:  # GF.ADD
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            r = _gf_addmod(a, b, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 1

        elif op == 0x1:  # GF.SUB
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            r = _gf_submod(a, b, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 1

        elif op == 0x2:  # GF.MUL
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            r = self._gf_mulmod_sel(a, b, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 4 if self._gf_is_mont() else 1

        elif op == 0x3:  # GF.SQR
            a = self._gf_acc_to_int()
            r = self._gf_sqrmod_sel(a, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 4 if self._gf_is_mont() else 1

        elif op == 0x4:  # GF.INV  (~767 cycles)
            a = self._gf_acc_to_int()
            r = _gf_invmod(a, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 767

        elif op == 0x5:  # GF.POW  (~767 cycles)
            a = self._gf_acc_to_int()
            e = self._gf_read_tile_b()
            r = _gf_powmod(a, e, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 767

        elif op == 0x6:  # GF.MULR — raw 256×256→512
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            product = a * b
            lo = product & _M256
            hi = (product >> 256) & _M256
            self._gf_int_to_acc(lo)
            self._gf_write_tile_dst(hi)
            self.gf_prev_lo = lo
            self.gf_prev_hi = hi
            return 1

        elif op == 0x7:  # GF.MAC — (ACC * B + prev) mod p
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            ab = self._gf_mulmod_sel(a, b, p)
            r = _gf_addmod(ab, self.gf_prev_lo, p)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 4 if self._gf_is_mont() else 1

        elif op == 0x8:  # GF.MACR — raw: prev_512 + ACC * B
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            mul_lo, mul_hi = a * b & _M256, (a * b >> 256) & _M256
            prev = self.gf_prev_lo | (self.gf_prev_hi << 256)
            total = prev + a * b
            lo = total & _M256
            hi = (total >> 256) & _M256
            self._gf_int_to_acc(lo)
            self._gf_write_tile_dst(hi)
            self.gf_prev_lo = lo
            self.gf_prev_hi = hi
            return 1

        elif op == 0x9:  # GF.CMOV Rd — conditional move
            reg_byte = self.fetch8()
            rd = (self._rex_d << 4) | ((reg_byte >> 4) & 0xF)
            cond = self.regs[rd] != 0
            b = self._gf_read_tile_b()
            if cond:
                self._gf_int_to_acc(b)
                self.gf_prev_lo = b
            # else ACC unchanged (constant-time in hardware)
            return 1

        elif op == 0xA:  # GF.CEQ — constant-time equality
            a = self._gf_acc_to_int()
            b = self._gf_read_tile_b()
            eq = 1 if a == b else 0
            self._gf_int_to_acc(eq)
            self.gf_prev_lo = eq
            self.flag_z = (a == b)
            return 1

        elif op == 0xB:  # GF.PRIME imm8
            imm8 = self.fetch8()
            self.gf_prime_sel = imm8 & 0x03
            return 1

        elif op == 0xC:  # GF.LDPRIME — load custom prime
            self.gf_custom_p = self._gf_acc_to_int()
            self.gf_mont_pinv = self._gf_read_tile_b()
            return 1

        elif op == 0xD:  # GF.X25519 (~4335 cycles)
            scalar = self._gf_acc_to_int()
            u_coord = self._gf_read_tile_b()
            scalar_bytes = scalar.to_bytes(32, 'little')
            u_bytes = u_coord.to_bytes(32, 'little')
            r = _x25519_scalar_mul(scalar_bytes, u_bytes)
            self._gf_int_to_acc(r)
            self.gf_prev_lo = r
            return 4335

        else:
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"EXT.CRYPTO Field ALU sub-op {op:#x} reserved")

    # -- Trap entry --

    def _trap(self, ivec: int):
        """Enter a trap/interrupt handler.

        Pushes flags and PC onto the stack, then jumps to the IVT
        entry.  Bit 8 of the saved-flags qword is reserved (was
        priv_level, now always 0).
        """
        saved = self.flags_pack()          # bit 8 stays 0 (no user mode)
        self.push64(saved)
        self.push64(self.pc)
        self.ivec_id = ivec
        handler = self.mem_read64(u64(self.ivt_base + 8 * ivec))
        self.pc = handler
        self.flag_i = 0       # mask interrupts
        self.idle = False     # interrupt wakes CPU from idle

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
            if n == 0x9:
                # EXT.STRING — self-contained 3-byte instruction
                cycles += self._exec_string()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles
            if n == 0xA:
                # EXT.DICT — self-contained 3-byte instruction
                cycles += self._exec_dict()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles
            if n == 0xB:
                # EXT.CRYPTO — self-contained 2-or-3-byte instruction
                cycles += self._exec_crypto()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles
            self._ext_modifier = n
            # Re-fetch the actual instruction
            byte0 = self.fetch8()
            f = (byte0 >> 4) & 0xF
            n = byte0 & 0xF
            cycles += 1
            # REX + F9: second byte is also 0xF family
            if f == 0xF and n == 0x9:
                cycles += self._exec_string()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles
            # REX + FA: second byte is also 0xF family
            if f == 0xF and n == 0xA:
                cycles += self._exec_dict()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles
            # REX + FB: EXT.CRYPTO with REX prefix
            if f == 0xF and n == 0xB:
                cycles += self._exec_crypto()
                self._ext_modifier = -1
                self.cycle_count += cycles
                if self.perf_enable:
                    self.perf_cycles += cycles
                return cycles

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
            saved = self.pop64()
            self.flags_unpack(saved & 0xFF)
            # bit 8 was priv_level — ignored (user mode stripped)
            return 1
        elif n == 0x5:  # RET (1802: pop XSEL|PSEL, IE←1)
            t = self.pop64() & 0xFFFF
            self.xsel = (t >> 8) & 0x1F
            self.psel = t & 0x1F
            self.flag_i = 1
            return 1
        elif n == 0x6:  # DIS (pop XSEL|PSEL, IE←0)
            t = self.pop64() & 0xFFFF
            self.xsel = (t >> 8) & 0x1F
            self.psel = t & 0x1F
            self.flag_i = 0
            return 1
        elif n == 0x7:  # MARK
            t = ((self.xsel & 0x1F) << 8) | (self.psel & 0x1F)
            self.t_reg = t
            self.push64(t)
            self.xsel = self.psel  # PSEL → XSEL
            return 1
        elif n == 0x8:  # SAV — store T → M(R(X)) as 16-bit
            self.mem_write16(self.rx, self.t_reg)
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
            rn = (byte1 & 0xF) | (self._rex_s << 4)
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
        rd = ((byte1 >> 4) & 0xF) | (self._rex_d << 4)
        rs = (byte1 & 0xF) | (self._rex_s << 4)

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
            rn = ((byte1 >> 4) & 0xF) | (self._rex_d << 4)

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
        rd = ((byte1 >> 4) & 0xF) | (self._rex_d << 4)
        rs = (byte1 & 0xF) | (self._rex_s << 4)
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
        elif sub == 0x9:  # STXI — M(R(X)) ← D, R(X) ← R(X)+1
            self.mem_write8(self.rx, self.d_reg & 0xFF)
            self.rx = u64(self.rx + 1)
        elif sub == 0xA:  # SHRC.D — shift D right through carry
            old_c = self.flag_c
            self.flag_c = self.d_reg & 1
            self.d_reg = ((old_c << 7) | (self.d_reg >> 1)) & 0xFF
            self.flag_z = 1 if self.d_reg == 0 else 0
        elif sub == 0xB:  # STXD.D — M(R(X)) ← D, R(X) ← R(X)-1
            self.mem_write8(self.rx, self.d_reg & 0xFF)
            self.rx = u64(self.rx - 1)
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
    MMIO_START = 0xFFFF_FF00_0000_0000

    def _exec_io(self, n: int) -> int:
        if 1 <= n <= 7:      # OUT N
            val = self.mem_read8(self.rx)
            self.port_out[n] = val & 0xFF
            self.rx = u64(self.rx + 1)
            # Port bridge → route byte to MMIO device
            mmio_off = self.port_map.get(n, 0xFFFF)
            if mmio_off < 0x1000:
                self.mem_write8(u64(self.MMIO_START + mmio_off), val & 0xFF)
            if self.on_output:
                self.on_output(n, val)
        elif 9 <= n <= 15:   # INP (N-8)
            port = n - 8
            mmio_off = self.port_map.get(port, 0xFFFF)
            if mmio_off < 0x1000:
                val = self.mem_read8(u64(self.MMIO_START + mmio_off)) & 0xFF
            else:
                val = self.port_in[port] & 0xFF
            self.mem_write8(self.rx, val)
            self.d_reg = val
        return 0

    # -- 0xA: SEP Rn --
    def _exec_sep(self, n: int) -> int:
        self.psel = n | (self._rex_n << 4)
        return 0

    # -- 0xB: SEX Rn --
    def _exec_sex(self, n: int) -> int:
        self.xsel = n | (self._rex_n << 4)
        return 0

    # -- 0xC: MUL/DIV --
    def _exec_muldiv(self, sub: int) -> int:
        byte1 = self.fetch8()
        rd = ((byte1 >> 4) & 0xF) | (self._rex_d << 4)
        rs = (byte1 & 0xF) | (self._rex_s << 4)
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
        # ---- Bitfield ALU (sub-ops 0x8–0xF) ----
        elif sub == 0x8:  # POPCNT
            self.regs[rd] = bin(b).count('1')
        elif sub == 0x9:  # CLZ
            self.regs[rd] = (64 - b.bit_length()) if b else 64
        elif sub == 0xA:  # CTZ
            self.regs[rd] = ((b & -b).bit_length() - 1) if b else 64
        elif sub == 0xB:  # BITREV
            self.regs[rd] = int(f'{b:064b}'[::-1], 2)
        elif sub == 0xC:  # BEXT (pext)
            src, mask, result, i = a, b, 0, 0
            while mask:
                lsb = mask & (-mask)
                if src & lsb:
                    result |= (1 << i)
                mask &= mask - 1
                i += 1
            self.regs[rd] = u64(result)
        elif sub == 0xD:  # BDEP (pdep)
            src, mask, result, i = b, a, 0, 0
            while mask:
                lsb = mask & (-mask)
                if src & (1 << i):
                    result |= lsb
                mask &= mask - 1
                i += 1
            self.regs[rd] = u64(result)
        elif sub == 0xE:  # RORI (3-byte: CE [Rd:4][0000] [imm8])
            imm = self.fetch8()
            shift = imm & 63
            v = a
            r = ((v >> shift) | (v << (64 - shift))) & MASK64 if shift else v
            self.regs[rd] = r
        elif sub == 0xF:  # BSWAP
            self.regs[rd] = int.from_bytes(b.to_bytes(8, 'little'), 'big')

        # Flag updates
        r = self.regs[rd]
        self.flag_z = 1 if r == 0 else 0
        self.flag_n = (r >> 63) & 1
        return 3 if sub <= 0x7 else 0  # mul/div extra cycles only

    # -- 0xD: CSR --
    def _exec_csr(self, n: int) -> int:
        w_bit = (n >> 3) & 1
        reg_lo = n & 0x7  # low 3 bits of register selector
        byte1 = self.fetch8()  # CSR address
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

        # Element width from TMODE (3-bit EW: 0-3 = int, 4 = fp16, 5 = bf16)
        ew_bits = self.tmode & 0x7
        is_fp = ew_bits >= EW_FP16
        if is_fp:
            elem_bytes = 2  # both fp16 and bf16 are 16-bit
        else:
            elem_bytes = 1 << ew_bits  # 1, 2, 4, or 8
        num_lanes = 64 // elem_bytes
        signed = (self.tmode >> 4) & 1

        # Load source tiles as byte arrays
        read_tile = self._read_tile
        write_tile = self._write_tile

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
            if is_fp:
                # ---- Floating-point TALU ----
                for lane in range(num_lanes):
                    ea = tile_get_elem(src_a, lane, elem_bytes)
                    eb_val = tile_get_elem(src_b, lane, elem_bytes)
                    if funct == 2:    # AND — bitwise, even for FP
                        r = ea & eb_val
                    elif funct == 3:  # OR — bitwise
                        r = ea | eb_val
                    elif funct == 4:  # XOR — bitwise
                        r = ea ^ eb_val
                    elif funct == 7:  # ABS — clear sign bit
                        r = ea & 0x7FFF
                    elif funct == 5:  # MIN — NaN-propagating
                        if _fp_is_nan(ea, ew_bits) or _fp_is_nan(eb_val, ew_bits):
                            r = 0x7E00 if ew_bits == EW_FP16 else 0x7FC0  # qNaN
                        else:
                            fa = _fp_decode(ea, ew_bits)
                            fb = _fp_decode(eb_val, ew_bits)
                            r = _fp_encode(min(fa, fb), ew_bits)
                    elif funct == 6:  # MAX — NaN-propagating
                        if _fp_is_nan(ea, ew_bits) or _fp_is_nan(eb_val, ew_bits):
                            r = 0x7E00 if ew_bits == EW_FP16 else 0x7FC0
                        else:
                            fa = _fp_decode(ea, ew_bits)
                            fb = _fp_decode(eb_val, ew_bits)
                            r = _fp_encode(max(fa, fb), ew_bits)
                    else:
                        # ADD (0) / SUB (1)
                        fa = _fp_decode(ea, ew_bits)
                        fb = _fp_decode(eb_val, ew_bits)
                        if funct == 0:
                            r = _fp_encode(fa + fb, ew_bits)
                        else:  # funct == 1
                            r = _fp_encode(fa - fb, ew_bits)
                    tile_set_elem(dst, lane, elem_bytes, r)
                write_tile(self.tdst, dst)
                return 0

            # ---- Integer TALU ----
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
            if is_fp:
                # ---- Floating-point TMUL ----
                if funct == 0:  # MUL
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        tile_set_elem(dst, lane, elem_bytes, _fp_encode(fa * fb, ew_bits))
                    write_tile(self.tdst, dst)
                    return 1

                elif funct == 1:  # DOT — FP16/BF16 → FP32 accumulate
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]
                        self.tctrl &= ~0x2
                    total = 0.0
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        total += float(fa) * float(fb)
                    if self.tctrl & 0x1:  # ACC_ACC
                        old_f = _bits_to_fp32(self.acc[0])
                        total = old_f + total
                    self.acc[0] = _fp32_to_bits(total)
                    self.acc[1] = 0
                    self.acc[2] = 0
                    self.acc[3] = 0
                    self.flag_z = 1 if total == 0.0 else 0
                    return 3

                elif funct == 2:  # WMUL — fp16/bf16 → fp32 widening multiply
                    # Output: 16 fp32 values in 2 tiles (TDST and TDST+64)
                    dst0 = bytearray(64)
                    dst1 = bytearray(64)
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        fp32_bits = _fp32_to_bits(float(fa) * float(fb))
                        if lane < 16:
                            tile_set_elem(dst0, lane, 4, fp32_bits)
                        else:
                            tile_set_elem(dst1, lane - 16, 4, fp32_bits)
                    write_tile(self.tdst, dst0)
                    write_tile(u64(self.tdst + 64), dst1)
                    return 2

                elif funct == 3:  # MAC — fp mul-accumulate: dst += a*b
                    existing = read_tile(self.tdst)
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        fc = _fp_decode(tile_get_elem(existing, lane, elem_bytes), ew_bits)
                        tile_set_elem(dst, lane, elem_bytes,
                                      _fp_encode(fc + fa * fb, ew_bits))
                    write_tile(self.tdst, dst)
                    return 2

                elif funct == 4:  # FMA — dst = a*b + dst
                    existing = read_tile(self.tdst)
                    for lane in range(num_lanes):
                        fa = _fp_decode(tile_get_elem(src_a, lane, elem_bytes), ew_bits)
                        fb = _fp_decode(tile_get_elem(src_b, lane, elem_bytes), ew_bits)
                        fc = _fp_decode(tile_get_elem(existing, lane, elem_bytes), ew_bits)
                        tile_set_elem(dst, lane, elem_bytes,
                                      _fp_encode(fa * fb + fc, ew_bits))
                    write_tile(self.tdst, dst)
                    return 2

                elif funct == 5:  # DOTACC — 4-way chunked dot, FP32 accumulate
                    chunk_size = num_lanes // 4
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]
                        self.tctrl &= ~0x2
                    for k in range(4):
                        dot = 0.0
                        for lane in range(chunk_size):
                            idx = k * chunk_size + lane
                            fa = _fp_decode(tile_get_elem(src_a, idx, elem_bytes), ew_bits)
                            fb = _fp_decode(tile_get_elem(src_b, idx, elem_bytes), ew_bits)
                            dot += float(fa) * float(fb)
                        if self.tctrl & 0x1:  # ACC_ACC
                            old_f = _bits_to_fp32(self.acc[k])
                            dot = old_f + dot
                        self.acc[k] = _fp32_to_bits(dot)
                    self.flag_z = 1 if all(a == 0 for a in self.acc) else 0
                    return 3

                return 1  # unknown fp TMUL funct

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

            if is_fp:
                # ---- Floating-point TRED ----
                fp_vals = [_fp_decode(tile_get_elem(tile, lane, elem_bytes), ew_bits)
                           for lane in range(num_lanes)]

                if funct == 0:    # SUM — FP32 accumulate
                    total = sum(float(v) for v in fp_vals)
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]
                        self.tctrl &= ~0x2
                    if self.tctrl & 0x1:  # ACC_ACC
                        old_f = _bits_to_fp32(self.acc[0])
                        total = old_f + total
                    self.acc[0] = _fp32_to_bits(total)
                    self.acc[1] = 0; self.acc[2] = 0; self.acc[3] = 0
                    self.flag_z = 1 if total == 0.0 else 0
                    return 0
                elif funct == 1:  # MIN
                    import math
                    # Filter out NaN, or propagate if all NaN
                    non_nan = [v for v in fp_vals if not math.isnan(v)]
                    if non_nan:
                        result_f = min(non_nan)
                    else:
                        result_f = float('nan')
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]; self.tctrl &= ~0x2
                    self.acc[0] = _fp32_to_bits(result_f)
                    self.acc[1] = 0; self.acc[2] = 0; self.acc[3] = 0
                    return 0
                elif funct == 2:  # MAX
                    import math
                    non_nan = [v for v in fp_vals if not math.isnan(v)]
                    if non_nan:
                        result_f = max(non_nan)
                    else:
                        result_f = float('nan')
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]; self.tctrl &= ~0x2
                    self.acc[0] = _fp32_to_bits(result_f)
                    self.acc[1] = 0; self.acc[2] = 0; self.acc[3] = 0
                    return 0
                elif funct == 5:  # SUMSQ — FP32 accumulate
                    total = sum(float(v) * float(v) for v in fp_vals)
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]; self.tctrl &= ~0x2
                    if self.tctrl & 0x1:
                        old_f = _bits_to_fp32(self.acc[0])
                        total = old_f + total
                    self.acc[0] = _fp32_to_bits(total)
                    self.acc[1] = 0; self.acc[2] = 0; self.acc[3] = 0
                    self.flag_z = 1 if total == 0.0 else 0
                    return 0
                elif funct == 6:  # MINIDX
                    import math
                    best_idx = 0
                    best_val = fp_vals[0]
                    for i in range(1, num_lanes):
                        if not math.isnan(fp_vals[i]) and (math.isnan(best_val) or fp_vals[i] < best_val):
                            best_val = fp_vals[i]
                            best_idx = i
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]; self.tctrl &= ~0x2
                    self.acc[0] = best_idx & MASK64
                    self.acc[1] = _fp32_to_bits(best_val)
                    self.acc[2] = 0; self.acc[3] = 0
                    return 0
                elif funct == 7:  # MAXIDX
                    import math
                    best_idx = 0
                    best_val = fp_vals[0]
                    for i in range(1, num_lanes):
                        if not math.isnan(fp_vals[i]) and (math.isnan(best_val) or fp_vals[i] > best_val):
                            best_val = fp_vals[i]
                            best_idx = i
                    if self.tctrl & 0x2:
                        self.acc = [0, 0, 0, 0]; self.tctrl &= ~0x2
                    self.acc[0] = best_idx & MASK64
                    self.acc[1] = _fp32_to_bits(best_val)
                    self.acc[2] = 0; self.acc[3] = 0
                    return 0
                # Unsupported FP reductions (POPCNT, L1) fall through to int path
                # which is arguably correct (bitwise POPCNT on FP bits)

            # ---- Integer TRED ----
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
            # Extended TSYS via EXT.8 prefix
            if self._ext_modifier == 8:
                if funct == 0:  # LOAD2D — strided gather into TDST
                    base = self._tile_cursor_addr()
                    dst = bytearray(64)
                    stride = self.tstride_r if self.tstride_r != 0 else self.ttile_w
                    h = self.ttile_h
                    w = self.ttile_w
                    off = 0
                    for row in range(h):
                        src_addr = base + row * stride
                        for col in range(w):
                            if off < 64:
                                dst[off] = self.mem_read8(u64(src_addr + col))
                                off += 1
                    # Zero-fill remainder
                    while off < 64:
                        dst[off] = 0
                        off += 1
                    write_tile(self.tdst, dst)
                    return h  # ~1 cycle per row
                elif funct == 1:  # STORE2D — strided scatter from TSRC0
                    base = self._tile_cursor_addr()
                    src = read_tile(self.tsrc0)
                    stride = self.tstride_r if self.tstride_r != 0 else self.ttile_w
                    h = self.ttile_h
                    w = self.ttile_w
                    off = 0
                    for row in range(h):
                        dst_addr = base + row * stride
                        for col in range(w):
                            if off < 64:
                                self.mem_write8(u64(dst_addr + col), src[off])
                                off += 1
                    return h
                return 0  # unknown ext TSYS funct

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
            elif funct == 1:  # SHUFFLE — permute lanes by index tile
                # dst[i] = src0[index[i]] where index tile is TSRC1
                src = read_tile(self.tsrc0)
                idx = read_tile(self.tsrc1)
                out = bytearray(64)
                for lane in range(num_lanes):
                    index_val = tile_get_elem(idx, lane, elem_bytes) % num_lanes
                    out_val = tile_get_elem(src, index_val, elem_bytes)
                    tile_set_elem(out, lane, elem_bytes, out_val)
                write_tile(self.tdst, out)
                return 2  # 3 cycles total
            elif funct == 5:  # PACK — narrow elements / FP format convert
                src = read_tile(self.tsrc0)
                out = bytearray(64)

                if is_fp:
                    # FP PACK: Convert to narrower FP format
                    # fp16 PACK → bf16 (same width, different format)
                    # bf16 PACK → fp16 (same width, different format)
                    # Both are 16-bit, so this is format conversion, not narrowing
                    target_ew = EW_BF16 if ew_bits == EW_FP16 else EW_FP16
                    for lane in range(num_lanes):
                        val = tile_get_elem(src, lane, elem_bytes)
                        f = _fp_decode(val, ew_bits)
                        tile_set_elem(out, lane, elem_bytes, _fp_encode(f, target_ew))
                    write_tile(self.tdst, out)
                    return 1

                # Integer PACK: narrows to half width
                if elem_bytes < 2:
                    # Can't pack below 8-bit; copy as-is
                    write_tile(self.tdst, src)
                    return 1
                out_eb = elem_bytes // 2
                out_bits = out_eb * 8
                out_mask = (1 << out_bits) - 1
                saturate = (self.tmode >> 5) & 1
                for lane in range(num_lanes):
                    val = tile_get_elem(src, lane, elem_bytes)
                    if saturate:
                        if signed:
                            sv = to_signed(val, elem_bytes)
                            hi = (1 << (out_bits - 1)) - 1
                            lo = -(1 << (out_bits - 1))
                            sv = max(lo, min(hi, sv))
                            val = sv & out_mask
                        else:
                            val = min(val, out_mask)
                    else:
                        val = val & out_mask
                    tile_set_elem(out, lane, out_eb, val)
                write_tile(self.tdst, out)
                return 1  # 2 cycles total
            elif funct == 6:  # UNPACK — widen elements / FP format convert
                src = read_tile(self.tsrc0)
                out = bytearray(64)

                if is_fp:
                    # FP UNPACK: Widen fp16/bf16 → fp32
                    # Input: 32 × 16-bit FP values in src tile
                    # Output: 16 × 32-bit FP32 values (only first 16 lanes fit)
                    out_lanes = 16  # 64 bytes / 4 bytes per fp32
                    for lane in range(out_lanes):
                        val = tile_get_elem(src, lane, elem_bytes)
                        f = _fp_decode(val, ew_bits)
                        tile_set_elem(out, lane, 4, _fp32_to_bits(f))
                    write_tile(self.tdst, out)
                    return 1

                # Integer UNPACK: widens to double width
                out_eb = elem_bytes * 2
                if out_eb > 8:
                    # Can't widen beyond 64-bit; copy as-is
                    write_tile(self.tdst, src)
                    return 1
                out_mask = (1 << (out_eb * 8)) - 1
                in_bits = elem_bytes * 8
                out_lanes = 64 // out_eb
                for lane in range(out_lanes):
                    val = tile_get_elem(src, lane, elem_bytes)
                    if signed:
                        # Sign-extend
                        sv = to_signed(val, elem_bytes)
                        val = sv & out_mask
                    # else: zero-extend (already unsigned)
                    tile_set_elem(out, lane, out_eb, val)
                write_tile(self.tdst, out)
                return 1  # 2 cycles total
            elif funct == 7:  # RROT — row/column rotate or mirror
                # Control from funct_byte (which we already consumed as funct)
                # But RROT uses the full funct_byte for control bits:
                #   bits[1:0] = direction (0=row-left, 1=row-right, 2=col-up, 3=col-down)
                #   bits[4:2] = amount (0-7)
                #   bit[5]    = mirror flag
                # Since we already extracted funct = funct_byte & 0x07, and funct == 7,
                # the direction was encoded in the low 2 bits along with funct.
                # Re-read: Actually funct_byte IS the control byte for RROT.
                # funct = funct_byte & 7 = 7 identifies this as RROT.
                # The control information comes from a SECOND fetch byte.
                ctrl = self.fetch8()
                direction = ctrl & 0x3
                amount = (ctrl >> 2) & 0x7
                mirror = (ctrl >> 5) & 0x1
                tile = read_tile(self.tsrc0)
                out = bytearray(64)
                # Treat tile as 8×8 matrix of bytes (for 8-bit mode)
                # For wider modes: adjust rows/cols
                rows = 8 // elem_bytes if elem_bytes <= 8 else 1
                cols = 8
                if elem_bytes == 1:
                    rows, cols = 8, 8
                elif elem_bytes == 2:
                    rows, cols = 4, 8  # 4 rows of 8 16-bit = wrong, actually 4 rows × 8 cols of 16-bit would be 64 bytes
                    # Actually: 32 lanes → treat as 4 rows × 8 cols (each element is 2 bytes)
                    rows, cols = 4, 8
                elif elem_bytes == 4:
                    rows, cols = 4, 4
                elif elem_bytes == 8:
                    rows, cols = 2, 4

                if mirror:
                    # Mirror: bit[0] of direction selects H vs V
                    if direction & 1:  # vertical mirror
                        for r in range(rows):
                            for c in range(cols):
                                src_lane = (rows - 1 - r) * cols + c
                                dst_lane = r * cols + c
                                if src_lane < num_lanes and dst_lane < num_lanes:
                                    tile_set_elem(out, dst_lane, elem_bytes,
                                                  tile_get_elem(tile, src_lane, elem_bytes))
                    else:  # horizontal mirror
                        for r in range(rows):
                            for c in range(cols):
                                src_lane = r * cols + (cols - 1 - c)
                                dst_lane = r * cols + c
                                if src_lane < num_lanes and dst_lane < num_lanes:
                                    tile_set_elem(out, dst_lane, elem_bytes,
                                                  tile_get_elem(tile, src_lane, elem_bytes))
                else:
                    # Rotate
                    for r in range(rows):
                        for c in range(cols):
                            if direction == 0:    # row-rotate-left
                                src_c = (c + amount) % cols
                                src_lane = r * cols + src_c
                            elif direction == 1:  # row-rotate-right
                                src_c = (c - amount) % cols
                                src_lane = r * cols + src_c
                            elif direction == 2:  # col-rotate-up
                                src_r = (r + amount) % rows
                                src_lane = src_r * cols + c
                            else:                 # col-rotate-down
                                src_r = (r - amount) % rows
                                src_lane = src_r * cols + c
                            dst_lane = r * cols + c
                            if src_lane < num_lanes and dst_lane < num_lanes:
                                tile_set_elem(out, dst_lane, elem_bytes,
                                              tile_get_elem(tile, src_lane, elem_bytes))
                write_tile(self.tdst, out)
                return 1  # 2 cycles total
            return 0

        return 0

    # -- Tile cursor helper --
    def _tile_cursor_addr(self) -> int:
        bank_base = self.sb * (4 * 1024 * 1024)  # 4 MiB aperture
        return bank_base + (self.sr * self.sw + self.sc) * 64

    # -- Reset helper --
    def _reset_state(self):
        self.regs = [0] * 32
        self.psel = 3
        self.xsel = 2
        self.spsel = 15
        self.flag_z = self.flag_c = self.flag_n = self.flag_v = 0
        self.flag_p = self.flag_g = self.flag_i = self.flag_s = 0
        self.d_reg = 0
        self.q_out = 0
        self.t_reg = 0
        self.priv_level = 0
        self.mpu_base = 0
        self.mpu_limit = 0
        self.sb = self.sr = self.sc = 0
        self.sw = 1
        self.tmode = self.tctrl = 0
        self.tsrc0 = self.tsrc1 = self.tdst = 0
        self.acc = [0, 0, 0, 0]
        self.tstride_r = 0
        self.tstride_c = 0
        self.ttile_h = 8
        self.ttile_w = 8
        self.ivt_base = 0
        self.ivec_id = 0
        self.irq_ipi = False
        self.halted = False
        self.idle = False
        self._ext_modifier = -1
        # DMA / QoS
        self.dma_ring_base = 0
        self.dma_ring_size = 0
        self.dma_head = 0
        self.dma_tail = 0
        self.dma_status = 0
        self.dma_ctrl = 0
        self.qos_weight = 0
        self.qos_bwlimit = 0
        # Note: core_id and num_cores are NOT reset — they are hardware-fixed

    # -- Instruction size helper (for SKIP) --

    def _next_instruction_size(self) -> int:
        """Peek at the instruction at PC and return its byte size."""
        b0 = self.mem_read8(self.pc)
        f = (b0 >> 4) & 0xF
        n = b0 & 0xF
        # EXT prefix: 1 byte + size of following instruction
        if f == 0xF:
            if n == 0x9:  # EXT.STRING: self-contained 3-byte
                return 3
            if n == 0xA:  # EXT.DICT: self-contained 3-byte
                return 3
            b1 = self.mem_read8(u64(self.pc + 1))
            f2 = (b1 >> 4) & 0xF
            n2 = b1 & 0xF
            if f2 == 0xF and n2 == 0x9:  # REX + F9
                return 4  # REX byte + F9 + sub-op + reg-byte
            if f2 == 0xF and n2 == 0xA:  # REX + FA
                return 4  # REX byte + FA + sub-op + reg-byte
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
        if f == 0xC:  # MUL/DIV / Bitfield ALU
            return 3 if n == 0xE else 2  # RORI is 3 bytes
        if f == 0xD:  return 2  # CSR
        if f == 0xE:  # MEX — 2 bytes + optional broadcast reg or RROT ctrl
            ss = (n >> 2) & 0x3
            op = n & 0x3
            if ss == 1:
                return 3  # broadcast: opcode + funct + reg
            if op == 3:   # TSYS
                funct_b = self.mem_read8(u64(addr + 1))
                if (funct_b & 0x07) == 7:  # RROT: opcode + funct + ctrl
                    return 3
            return 2
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


# ---------------------------------------------------------------------------
#  Micro-Core CPU — reduced-feature variant (matches RTL mp64_cpu_micro.v)
# ---------------------------------------------------------------------------
# Differences from the full Megapad64 core:
#   - No I-cache (no CSR_ICACHE_*)
#   - No tile/MEX engine — family 0xE traps as ILLEGAL_OP
#   - No tile datapath self-test (CSR_TILE_SELFTEST/DETAIL → 0)
#   - MUL/DIV delegated to cluster's shared unit (or trap if standalone)
#   - Only PERF_CYCLES counter (stalls/tileops/extmem → 0)
#   - CPUID returns micro-core variant ID
#   - Barrier CSRs (0x74/0x75) forwarded to cluster
#   - BIST CSRs (0x60-0x63) forwarded to cluster
#   - Cluster IVT base CSR (0x73) forwarded to cluster
#   - No 1802-heritage state: D accum, Q flip-flop, T register removed
#   - No 1802-heritage families: MEMALU (0x8), I/O (0x9)
#   - SEP (0xA) and SEX (0xB) ARE available (zero area cost)
#   - No 1802-heritage sub-ops: GLO/GHI/PLO/PHI (0x6C-0x6F),
#     RET/DIS/MARK/SAV/SEQ/REQ (0x05-0x0A) — trap as ILLEGAL_OP
#   - No port_in/port_out arrays
# ---------------------------------------------------------------------------

class Megapad64Micro(Megapad64):
    """Micro-core CPU — lightweight variant for cluster execution.

    Strips CDP1802-heritage features to minimise silicon area:
    no D accumulator, no Q flip-flop, no T register, no SCRT calling
    convention (MARK/SAV/RET/DIS), no port I/O.

    SHARED resources via cluster:
    - MUL/DIV unit (round-robin arbitrated)
    - Tile/MEX engine (round-robin arbitrated)
    - CRC engine (hardware-lock arbitrated; INIT acquires, FIN releases)

    SEP and SEX ARE available on micro-cores (zero area cost — they
    only update the psel/xsel pointer, which micro-cores already have).
    """

    def __init__(self, mem_size: int = 1 << 20, core_id: int = 0,
                 num_cores: int = NUM_ALL_CORES):
        super().__init__(mem_size=mem_size, core_id=core_id, num_cores=num_cores)
        # Cluster reference (set by MicroCluster after construction)
        self._cluster = None
        # Override fields — not available on micro-cores
        self.tile_selftest = 0
        self.tile_st_detail = 0
        self.icache_enabled = 0
        self.icache_hits = 0
        self.icache_misses = 0
        # Strip 1802-heritage state
        self.d_reg = 0
        self.q_out = 0
        self.t_reg = 0
        self.port_out = None  # not present
        self.port_in = None   # not present

    # -- Families 0x8, 0x9 — not available on micro-cores --
    # Note: SEP (0xA) and SEX (0xB) ARE available (zero cost, just
    # update psel/xsel pointers) — inherited from parent class.

    def _exec_memalu(self, sub: int) -> int:
        """MEMALU (1802 D-register ops) not present on micro-cores."""
        raise TrapError(IVEC_ILLEGAL_OP,
                        "MEMALU (family 0x8) not available on micro-core")

    def _exec_io(self, n: int) -> int:
        """Port I/O (1802-style) not present on micro-cores."""
        raise TrapError(IVEC_ILLEGAL_OP,
                        "Port I/O (family 0x9) not available on micro-core")

    def _exec_string(self) -> int:
        """EXT.STRING not available on micro-cores."""
        self.fetch8()  # consume sub-op
        self.fetch8()  # consume reg-byte
        raise TrapError(IVEC_ILLEGAL_OP,
                        "EXT.STRING (F9) not available on micro-core")

    def _exec_dict(self) -> int:
        """EXT.DICT not available on micro-cores."""
        self.fetch8()  # consume sub-op
        self.fetch8()  # consume reg-byte
        raise TrapError(IVEC_ILLEGAL_OP,
                        "EXT.DICT (FA) not available on micro-core")

    # -- MEX (tile engine) — shared via cluster tile engine --

    def _exec_mex(self, n: int) -> int:
        """Tile/MEX uses the cluster's shared tile engine.

        If not inside a cluster, traps as illegal opcode (matching
        RTL behaviour without a tile engine).
        """
        if self._cluster is None:
            self.fetch8()  # consume the funct byte
            raise TrapError(IVEC_ILLEGAL_OP,
                            "MEX (tile engine) not available on standalone micro-core")
        # Delegate to parent class — cluster shared tile engine is modelled
        # as immediate (no cycle-accurate contention in emulator).
        cycles = super()._exec_mex(n)
        return cycles + 3  # shared unit arbitration overhead

    # -- SYS family: trap 1802-heritage sub-ops --

    def _exec_sys(self, n: int) -> int:
        """SYS family — traps RET/DIS/MARK/SAV/SEQ/REQ (1802 SCRT)."""
        if n in (0x5, 0x6, 0x7, 0x8, 0x9, 0xA):
            names = {0x5: 'RET', 0x6: 'DIS', 0x7: 'MARK',
                     0x8: 'SAV', 0x9: 'SEQ', 0xA: 'REQ'}
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"{names[n]} (1802 SCRT) not available on micro-core")
        return super()._exec_sys(n)

    # -- IMM family: trap GLO/GHI/PLO/PHI --

    def _exec_imm(self, sub: int) -> int:
        """IMM family — traps GLO/GHI/PLO/PHI (D-register byte ops)."""
        if sub in (0xC, 0xD, 0xE, 0xF):
            self.fetch8()  # consume the Rx byte
            names = {0xC: 'GLO', 0xD: 'GHI', 0xE: 'PLO', 0xF: 'PHI'}
            raise TrapError(IVEC_ILLEGAL_OP,
                            f"{names[sub]} (D-register) not available on micro-core")
        return super()._exec_imm(sub)

    # -- MUL/DIV / Bitfield ALU — delegated to cluster shared unit or local --

    def _exec_muldiv(self, sub: int) -> int:
        """MUL/DIV uses the cluster's shared multiplier.
        Bitfield Tier 1 (POPCNT, CLZ, CTZ, BITREV) is local to the
        micro-core.  Tier 2 (BEXT, BDEP, RORI, BSWAP) is gated out
        on micro-cores.

        If not inside a cluster, MUL/DIV traps as illegal opcode
        (matching RTL behaviour when IN_CLUSTER=0).
        """
        if sub >= 0xC and sub <= 0xF:  # Tier 2 bitfield — gated out
            self.fetch8()  # consume operand byte
            if sub == 0xE:
                self.fetch8()  # RORI has extra imm byte
            raise TrapError(IVEC_ILLEGAL_OP,
                            "Bitfield Tier 2 not available on micro-core")
        if sub >= 0x8:  # Tier 1 bitfield — local, no cluster needed
            return super()._exec_muldiv(sub)
        if self._cluster is None:
            self.fetch8()  # consume operand byte
            raise TrapError(IVEC_ILLEGAL_OP,
                            "MUL/DIV not available on standalone micro-core")
        # Delegate to parent class — cluster shared MUL/DIV is modelled
        # as immediate (no cycle-accurate contention).
        cycles = super()._exec_muldiv(sub)
        return cycles + 3  # shared unit overhead

    # -- CSR overrides --

    def csr_read(self, addr: int) -> int:
        """Restricted CSR set matching RTL mp64_cpu_micro.v."""
        if addr in (CSR_FLAGS, CSR_PSEL, CSR_XSEL, CSR_SPSEL,
                    CSR_IVT_BASE, CSR_IE, CSR_PRIV,
                    CSR_COREID, CSR_NCORES, CSR_MBOX, CSR_IPIACK,
                    CSR_IVEC_ID, CSR_TRAP_ADDR, CSR_MEGAPAD_SZ,
                    CSR_PERF_CYCLES, CSR_PERF_CTRL):
            return super().csr_read(addr)
        # Tile engine CSRs — forwarded to shared tile engine via parent
        if addr in (CSR_SB, CSR_SR, CSR_SC, CSR_SW,
                    CSR_TMODE, CSR_TCTRL, CSR_TSRC0, CSR_TSRC1, CSR_TDST,
                    CSR_ACC0, CSR_ACC1, CSR_ACC2, CSR_ACC3,
                    CSR_TSTRIDE_R, CSR_TSTRIDE_C, CSR_TTILE_H, CSR_TTILE_W):
            return super().csr_read(addr)
        if addr == CSR_CPUID:
            return CPUID_MICRO
        # D/DF/Q/T CSRs — not present, always return 0
        if addr in (CSR_D, CSR_DF, CSR_Q, CSR_T):
            return 0
        # Barrier CSRs — forwarded to cluster
        if addr == CSR_BARRIER_ARRIVE:
            return self._cluster.barrier_arrive if self._cluster else 0
        if addr == CSR_BARRIER_STATUS:
            if self._cluster:
                done = 1 if self._cluster.barrier_done else 0
                return (done << 8) | (self._cluster.barrier_arrive & 0xFF)
            return 0
        # BIST CSRs — forwarded to cluster
        if addr in (CSR_BIST_CMD, CSR_BIST_STATUS,
                    CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA):
            if self._cluster:
                return self._cluster.bist_csr_read(addr)
            return 0
        # Cluster MPU CSRs — forwarded to cluster
        if addr in (CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                    CSR_CL_IVTBASE):
            if self._cluster:
                return self._cluster.bist_csr_read(addr)
            return 0
        # Removed CSRs: tile, I-cache, perf stalls/tileops/extmem → 0
        return 0

    def csr_write(self, addr: int, val: int):
        """Restricted CSR set matching RTL mp64_cpu_micro.v."""
        val = u64(val)
        if addr in (CSR_FLAGS, CSR_PSEL, CSR_XSEL, CSR_SPSEL,
                    CSR_IVT_BASE, CSR_IE, CSR_PRIV,
                    CSR_IVEC_ID, CSR_PERF_CTRL,
                    CSR_MBOX, CSR_IPIACK):
            return super().csr_write(addr, val)
        # Tile engine CSRs — forwarded to shared tile engine via parent
        if addr in (CSR_SB, CSR_SR, CSR_SC, CSR_SW,
                    CSR_TMODE, CSR_TCTRL, CSR_TSRC0, CSR_TSRC1, CSR_TDST,
                    CSR_ACC0, CSR_ACC1, CSR_ACC2, CSR_ACC3,
                    CSR_TSTRIDE_R, CSR_TSTRIDE_C, CSR_TTILE_H, CSR_TTILE_W):
            return super().csr_write(addr, val)
        # D/DF/Q/T CSR writes — silently ignored
        if addr in (CSR_D, CSR_DF, CSR_Q, CSR_T):
            return
        # Barrier arrive — set this core's bit
        if addr == CSR_BARRIER_ARRIVE:
            if self._cluster:
                self._cluster.barrier_arrive_core(self.core_id)
            return
        # BIST CSRs — forwarded to cluster
        if addr in (CSR_BIST_CMD, CSR_BIST_STATUS,
                    CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA):
            if self._cluster:
                self._cluster.bist_csr_write(addr, val)
            return
        # Cluster MPU CSRs — forwarded to cluster (S-mode only)
        if addr in (CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                    CSR_CL_IVTBASE):
            if self._cluster:
                self._cluster.bist_csr_write(addr, val)
            return
        # All other CSR writes silently ignored

    def _bist_cmd_write(self, val: int):
        if self._cluster:
            self._cluster.bist_csr_write(CSR_BIST_CMD, val)

    def _tile_selftest_write(self, val: int):
        pass  # no tile self-test on micro-cores

    def _icache_ctrl_write(self, val: int):
        pass  # no I-cache on micro-cores
