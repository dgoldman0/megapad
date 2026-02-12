# Extended TPU Specification

**Branch:** `features/extended-tpu`  
**Status:** Design  
**Author:** auto-generated from design discussion  
**Depends on:** Base tile engine (mp64_tile.v), ISA v2.1, quad-core SoC

---

## 1. Overview

The Extended TPU adds five capability families to the Megapad-64's
existing tile engine and SoC infrastructure:

| Family | Purpose | Area Estimate |
|--------|---------|---------------|
| **Enhanced Tile Engine** | TMUL/MAC, views, richer reductions, strided addressing | Medium |
| **Numeric Acceleration** | FP16/bfloat16 tile ops, optional scalar FP32 | Medium |
| **Security / Integrity** | AES-256-GCM, SHA-3/SHAKE, CRC32/CRC64 | Large |
| **Data Movement / QoS** | HW tile DMA, descriptor rings, prefetch, per-core QoS | Medium |
| **Reliability / BIST** | Memory self-test, tile datapath check, perf counters | Small |

All new features are **backward-compatible** — existing code runs
unchanged. New instructions use the existing MEX (0xE_) encoding space
or new CSRs in the 0x40–0x7F range. Crypto accelerators are MMIO-mapped
peripherals.

---

## 2. Enhanced Tile Engine

### 2.1 TMUL / MAC Family

The existing tile engine has two TMUL functions: lane-wise MUL (funct 0)
and DOT product (funct 1). We extend with widening MUL, FMA, and
lane-wise accumulation.

| Funct | Mnemonic | Operation | Result |
|-------|----------|-----------|--------|
| 0 | MUL (existing) | `dst[i] = a[i] × b[i]` | `[TDST]` |
| 1 | DOT (existing) | $\sum_i a_i \times b_i$ | ACC |
| 2 | **WMUL** | `dst[2i:2i+1] = a[i] × b[i]` (widening) | `[TDST]` (double-width) |
| 3 | **MAC** | `dst[i] += a[i] × b[i]` (in-place accumulate) | `[TDST]` |
| 4 | **FMA** | `dst[i] = a[i] × b[i] + c[i]` | `[TDST]` (c = TDST) |
| 5 | **DOTACC** | `ACC[k] += dot(a_chunk_k, b_chunk_k)` for k=0..3 | ACC0–ACC3 |

**WMUL** doubles the element width in the output: 8→16, 16→32, 32→64.
Input tile has N elements; output tile has N elements at double width
(so output is 128 bytes — written across `[TDST]` and `[TDST+64]`).

**DOTACC** splits the tile into 4 equal chunks and produces 4 independent
dot products, one per accumulator register. This is useful for 4-wide
vector dot products in GEMM inner loops.

### 2.2 Saturating Arithmetic & Rounding Shifts

New TALU functions for DSP-style operations. Controlled by new TMODE
bits:

```
TMODE extended layout:
Bits [1:0]  Element width (EW)   — unchanged
Bit  [4]    Signed               — unchanged
Bit  [5]    Saturation mode      — NEW: 1 = saturating arithmetic
Bit  [6]    Rounding mode        — NEW: 1 = round-to-nearest on shifts
```

| Funct | Mnemonic | Operation | Saturation Behavior |
|-------|----------|-----------|-------------------|
| 0 | SADD | `dst[i] = sat(a[i] + b[i])` | Clamp to min/max of element type |
| 1 | SSUB | `dst[i] = sat(a[i] − b[i])` | Clamp to min/max of element type |

When TMODE bit 5 is set, existing ADD/SUB opcodes become saturating.
No new funct codes needed — it's a mode flag.

**Rounding shifts**: When TMODE bit 6 is set, SHR operations add 0.5
before truncating (i.e., add the bit that's about to be shifted out).
This applies to the new VSHR/VSHL lane-wise shifts.

New TALU funct codes for lane-wise shifts:

| Funct | Mnemonic | Operation | Notes |
|-------|----------|-----------|-------|
| — | **VSHR** | `dst[i] = a[i] >> b[i]` | Per-lane right shift (uses TALU funct space via EXT encoding) |
| — | **VSHL** | `dst[i] = a[i] << b[i]` | Per-lane left shift |

These use the previously-unused FAM_EXT (0xF) instruction family to
avoid exhausting the 3-bit TALU funct space:

```
Byte 0: 0xF0 + (SS<<2)     Extended tile ALU
Byte 1: ext_funct           VSHR=0, VSHL=1, VSEL=2, VCLZ=3, ...
Byte 2: [optional reg#]     For broadcast mode
```

### 2.3 Tile Views — Shuffles, Permutes, Format Conversion

New TSYS functions for manipulating tile layout without arithmetic:

| Funct | Mnemonic | Operation | Cycles |
|-------|----------|-----------|--------|
| 0 | TRANS (existing) | 8×8 byte transpose | 1 |
| 1 | **SHUFFLE** | Permute lanes by index tile | 3 |
| 2 | MOVBANK (existing) | Tile copy | 3 |
| 3 | LOADC (existing) | Load from cursor | 1 |
| 4 | ZERO (existing) | Zero tile | 1 |
| 5 | **PACK** | Pack from wider to narrower elements | 2 |
| 6 | **UNPACK** | Unpack from narrower to wider elements | 2 |
| 7 | **RROT** | Row/column rotate or mirror | 2 |

**SHUFFLE**: `[TSRC1]` is an index tile — each byte is a lane index
(0–63). Output: `dst[i] = src0[index[i]]`. This is the universal
permutation: any reordering, duplication, or broadcast can be expressed
as a shuffle. For 16/32/64-bit modes, indices are per-element.

**PACK/UNPACK**: Format conversions between element widths.
- PACK 32→16: takes 16 × 32-bit elements, narrows to 16 × 16-bit (with
  saturation if TMODE bit 5 set), stores in lower half of output tile.
- UNPACK 16→32: takes 16 × 16-bit elements from lower half of input,
  zero-extends (or sign-extends if TMODE bit 4 set) to 32-bit.

**RROT**: Controlled by `mex_imm8` (byte 1):
- Bits [1:0] = direction: 0=row-rotate-left, 1=row-rotate-right,
  2=col-rotate-up, 3=col-rotate-down
- Bits [4:2] = amount (0–7 positions)
- Bit  [5]   = mirror flag: if set, mirror instead of rotate
  (bit [0] selects horizontal vs vertical mirror)

View operations treat the 64-byte tile as an 8×8 matrix of bytes
(in 8-bit mode), 4×8 of 16-bit, 4×4 of 32-bit, or 2×4 of 64-bit.

### 2.4 Enhanced Reductions

Extend the existing 5 reduction functions (SUM, MIN, MAX, POPCNT, L1):

| Funct | Mnemonic | Operation | Notes |
|-------|----------|-----------|-------|
| 0 | SUM (existing) | $\sum_i a_i$ | |
| 1 | MIN (existing) | $\min_i a_i$ | |
| 2 | MAX (existing) | $\max_i a_i$ | |
| 3 | POPCNT (existing) | $\sum_i \text{popcount}(a_i)$ | |
| 4 | L1 (existing) | $\sum_i |a_i|$ | |
| 5 | **SUMSQ** | $\sum_i a_i^2$ | Sum of squares (L2² norm) |
| 6 | **MINIDX** | $\text{argmin}_i a_i$ | Returns index in ACC0, value in ACC1 |
| 7 | **MAXIDX** | $\text{argmax}_i a_i$ | Returns index in ACC0, value in ACC1 |

**SUMSQ** is essential for computing L2 norms and variances. It squares
each lane value (producing a widened intermediate) and sums into the
256-bit accumulator, preventing overflow.

**MINIDX / MAXIDX** return both the index and value. In multi-tile
accumulation mode (TCTRL bit 0), the running min/max is updated along
with its absolute index (accounting for tile offset via an internal
lane counter).

### 2.5 Strided / 2D Tile Addressing

The existing cursor (SB/SR/SC/SW) computes a linear address. We add
**stride registers** that let tile load/store operations skip rows:

| CSR | Address | Description |
|-----|---------|-------------|
| `TSTRIDE_R` | `0x40` | Row stride in bytes (distance between rows) |
| `TSTRIDE_C` | `0x41` | Column stride in bytes (distance between columns) |
| `TTILE_H` | `0x42` | Tile height (rows to load, 1–8) |
| `TTILE_W` | `0x43` | Tile width (columns per row in bytes, 1–64) |

When `TSTRIDE_R ≠ 0`, tile loads become **gather** operations:

$$\text{for } r = 0..\text{TTILE\_H}-1: \quad \text{row}[r] = \text{mem}[\text{addr} + r \times \text{TSTRIDE\_R}]$$

This supports loading non-contiguous tiles from 2D images (e.g., an
8×8 patch from a 640-wide framebuffer with TSTRIDE_R = 640).

A new TSYS instruction **LOAD2D** (extended via FAM_EXT) performs
the strided gather; **STORE2D** does the strided scatter.

---

## 3. Numeric Acceleration

### 3.1 Tile-Side FP16 / bfloat16

New TMODE element types for IEEE 754 half-precision and bfloat16:

```
TMODE extended EW encoding (bits [2:0]):
  000 = u8/i8    (64 lanes)  — existing
  001 = u16/i16  (32 lanes)  — existing
  010 = u32/i32  (16 lanes)  — existing
  011 = u64/i64  ( 8 lanes)  — existing
  100 = fp16     (32 lanes)  — NEW
  101 = bf16     (32 lanes)  — NEW
  110 = reserved
  111 = reserved
```

This requires extending TMODE from 2-bit to 3-bit EW, consuming bit 2
(currently reserved).

**Supported FP operations** (use existing TALU/TMUL functs):

| Operation | fp16 | bf16 | Notes |
|-----------|------|------|-------|
| ADD/SUB | ✅ | ✅ | IEEE round-to-nearest-even |
| MUL | ✅ | ✅ | |
| FMA | ✅ | ✅ | Via new TMUL funct 4 |
| MIN/MAX | ✅ | ✅ | NaN-propagating |
| ABS | ✅ | ✅ | Clear sign bit |
| DOT | ✅ | ✅ | FP16→FP32 accumulate to prevent precision loss |
| SUM | ✅ | ✅ | FP16→FP32 accumulate |
| CVTF | ✅ | ✅ | Convert fp16↔bf16↔fp32 (via PACK/UNPACK) |

**FP16→FP32 accumulation**: When computing DOT or SUM with fp16/bf16
inputs, the accumulator registers ACC0–ACC3 hold **FP32** values.
Products are computed in FP32 precision and accumulated with FP32
addition. This matches the behavior of modern AI accelerators.

### 3.2 Optional Scalar FP32 Unit

A minimal FP32 unit for the CPU's scalar ALU, gated behind a synthesis
parameter `HAS_FPU`:

| Instruction | Encoding | Operation |
|-------------|----------|-----------|
| `FADD Rd, Rs` | FAM_EXT + funct | FP32 add |
| `FMUL Rd, Rs` | FAM_EXT + funct | FP32 multiply |
| `FCVT.I Rd, Rs` | FAM_EXT + funct | FP32 → int64 |
| `FCVT.F Rd, Rs` | FAM_EXT + funct | int64 → FP32 |
| `FCMP Rd, Rs` | FAM_EXT + funct | FP32 compare, set flags |

FP32 values are stored in the **low 32 bits** of any GPR. The upper
32 bits are ignored/zeroed. FP64 is explicitly **not** supported to
keep the datapath small.

**Synthesis parameter**: `parameter HAS_FPU = 0;` — when 0, FP
instructions trap as illegal opcode.

---

## 4. Security & Integrity Accelerators

These are **MMIO-mapped coprocessors**, not tile engine extensions. They
share the MMIO bus alongside the existing UART, timer, etc. The CPU feeds
them data via MMIO register writes and reads results back.

### 4.1 AES-256-GCM

A hardware AES block supporting AES-256 encryption/decryption in GCM
(Galois/Counter Mode) for authenticated encryption.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `AES_KEY[0..7]` | 0x700 | W | 256-bit key (8 × 32-bit writes) |
| `AES_IV[0..2]` | 0x720 | W | 96-bit IV/nonce |
| `AES_AAD_LEN` | 0x730 | W | Additional authenticated data length |
| `AES_DATA_LEN` | 0x734 | W | Plaintext/ciphertext length |
| `AES_CMD` | 0x738 | W | Start: 0=encrypt, 1=decrypt |
| `AES_STATUS` | 0x739 | R | Busy/done/auth-fail flags |
| `AES_DIN` | 0x740 | W | 128-bit data input (4 × 32-bit writes) |
| `AES_DOUT` | 0x750 | R | 128-bit data output |
| `AES_TAG[0..3]` | 0x760 | R/W | 128-bit GCM authentication tag |

**Data flow**: Software writes key, IV, then streams 16-byte blocks via
AES_DIN. The hardware computes AES rounds and GHASH in parallel. When
done, read AES_TAG for authentication. For decryption, write the
expected tag first, then stream ciphertext — AES_STATUS reports
auth-fail if the tag doesn't match.

**Performance target**: 1 block (16 bytes) per 11 cycles (pipelined
AES-256 round function) + 1 cycle GHASH.

**Interrupt**: `IRQX_AES` (vector 12) fires when a block is done, for
interrupt-driven streaming.

### 4.2 SHA-3 / SHAKE Accelerator

Hardware SHA-3 (Keccak) supporting SHA3-256, SHA3-512, SHAKE128,
SHAKE256.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `SHA_CMD` | 0x780 | W | Mode (0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256) |
| `SHA_STATUS` | 0x781 | R | Busy/done/squeeze-ready |
| `SHA_DIN` | 0x790 | W | 64-bit data input (absorb) |
| `SHA_DOUT` | 0x7A0 | R | 64-bit hash output (squeeze) |
| `SHA_RATE` | 0x7A8 | R | Rate in bytes for current mode |
| `SHA_CTRL` | 0x7A9 | W | Init/absorb/squeeze/pad commands |

**Data flow**: Software initializes mode, then feeds message data 8
bytes at a time. Hardware runs Keccak-f[1600] (24 rounds) when the
rate buffer is full. For SHAKE, software can squeeze arbitrary-length
output.

**Performance target**: Keccak-f[1600] in 24 cycles (1 round/cycle),
absorb 136 bytes (SHA3-256 rate) in ~41 cycles (24 rounds + 17
writes).

### 4.3 CRC32 / CRC64

Lightweight CRC accelerator for data integrity. Supports:
- CRC32 (ISO 3309 / ITU-T V.42, polynomial 0x04C11DB7)
- CRC32C (Castagnoli, polynomial 0x1EDC6F41)
- CRC64-ECMA (polynomial 0x42F0E1EBA9EA3693)

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `CRC_POLY` | 0x7C0 | W | Polynomial select (0=CRC32, 1=CRC32C, 2=CRC64) |
| `CRC_INIT` | 0x7C4 | W | Initial CRC value |
| `CRC_DIN` | 0x7C8 | W | 64-bit data input (processes 8 bytes/cycle) |
| `CRC_RESULT` | 0x7D0 | R | Current CRC value |
| `CRC_CTRL` | 0x7D8 | W | 0=reset, 1=finalize (XOR-out) |

**Performance**: 8 bytes per cycle using an 8-byte-wide lookup table
or Sarwate algorithm. For a 512-byte disk sector: 64 cycles.

---

## 5. Data Movement & QoS

### 5.1 Hardware Tile DMA Queues

A per-core DMA engine that can copy tiles between memory regions
without CPU involvement. The CPU pushes descriptors; the hardware
processes them asynchronously.

**DMA descriptor** (32 bytes, stored in scratchpad/BRAM):

```
Offset  Field          Size    Description
0x00    src_addr       8B      Source address (64-bit)
0x08    dst_addr       8B      Destination address (64-bit)
0x10    length         4B      Transfer length in bytes
0x14    flags          4B      Bit 0: interrupt-on-complete
                               Bit 1: src is external memory
                               Bit 2: dst is external memory
                               Bit 3: 2D mode (use stride)
0x18    src_stride     2B      Source row stride (2D mode)
0x1A    dst_stride     2B      Destination row stride (2D mode)
0x1C    rows           2B      Number of rows (2D mode)
0x1E    reserved       2B
```

**DMA CSRs** (per-core, CSR addresses 0x50–0x57):

| CSR | Address | Description |
|-----|---------|-------------|
| `DMA_RING_BASE` | `0x50` | Base address of descriptor ring in BRAM |
| `DMA_RING_SIZE` | `0x51` | Ring size (power of 2, entries) |
| `DMA_HEAD` | `0x52` | Head pointer (software writes to enqueue) |
| `DMA_TAIL` | `0x53` | Tail pointer (hardware advances on completion) |
| `DMA_STATUS` | `0x54` | Idle/busy/error flags |
| `DMA_CTRL` | `0x55` | Enable/disable, interrupt mask |

**Operation**: Software writes descriptors to the ring, advances
DMA_HEAD. The DMA engine reads descriptors from DMA_TAIL, executes
transfers (64 bytes at a time via the tile bus), and advances DMA_TAIL.
An interrupt fires on completion if the descriptor's flag is set.

### 5.2 Prefetch / Write-Combine Buffers

Small (2-entry) prefetch and write-combine buffers per core to hide
external memory latency:

- **Prefetch buffer**: When the CPU issues a PREFETCH hint instruction
  (new FAM_EXT funct), the bus interface begins fetching the target
  cache line (64 bytes) in the background. A subsequent load to the
  same address hits the prefetch buffer instead of stalling.

- **Write-combine buffer**: Consecutive byte/halfword writes to the
  same 64-byte region are coalesced into a single 64-byte burst write
  to external memory. Flushed on address boundary crossing or explicit
  FENCE instruction.

### 5.3 Per-Core QoS Arbitration

The bus arbiter (mp64_bus.v) currently uses simple round-robin. We add
a **weighted round-robin** mode with per-core priority registers:

| CSR | Address | Description |
|-----|---------|-------------|
| `QOS_WEIGHT` | `0x58` | This core's bus priority weight (1–15, default 4) |
| `QOS_BWLIMIT` | `0x59` | Max bus transactions per quantum (0 = unlimited) |

Higher weight → more bus slots per round. This prevents a tile-DMA-heavy
core from starving latency-sensitive cores.

A global **QOS_QUANTUM** register (MMIO 0x7E0) sets the time window
for bandwidth accounting (default: 256 cycles).

---

## 6. Reliability & BIST

### 6.1 Memory BIST

Built-in self-test for the 1 MiB dual-port BRAM. Runs at boot (before
core 0 jumps to BIOS) or on-demand via CSR command.

**Test patterns**:
1. **March C−**: Write 0x00, read 0x00 / write 0xFF, read 0xFF /
   write 0x00, read 0x00. Detects stuck-at faults.
2. **Checkerboard**: Alternating 0xAA/0x55 patterns. Detects coupling
   faults between adjacent cells.
3. **Address-as-data**: Write address value, read back. Detects address
   decoder faults.

| CSR | Address | Description |
|-----|---------|-------------|
| `BIST_CMD` | `0x60` | 0=idle, 1=start-full, 2=start-quick (March C− only) |
| `BIST_STATUS` | `0x61` | 0=idle, 1=running, 2=pass, 3=fail |
| `BIST_FAIL_ADDR` | `0x62` | First failing address (if status=fail) |
| `BIST_FAIL_DATA` | `0x63` | Expected vs actual data at failing address |

**Timing**: Full BIST on 1 MiB at 100 MHz ≈ 30 ms (3 passes × 1M
reads/writes ÷ 100M cycles/sec). Quick BIST (March C− only) ≈ 10 ms.

### 6.2 Tile Datapath Self-Test

Lightweight functional checks for the tile engine ALU and reduction
paths:

1. Load a known test pattern into TSRC0, TSRC1
2. Execute TADD, TMUL, TDOT, TSUM with known inputs
3. Compare ACC / TDST against golden values
4. Report pass/fail via CSR

| CSR | Address | Description |
|-----|---------|-------------|
| `TILE_SELFTEST` | `0x64` | Write 1 to start; read: 0=idle, 1=running, 2=pass, 3=fail |
| `TILE_ST_DETAIL` | `0x65` | Which sub-test failed (bitmask) |

The self-test runs ~50 tile operations, taking ≈200 cycles. It uses
a dedicated 128-byte scratchpad region (0xFFF80–0xFFFFF) that the
BIST avoids.

### 6.3 Performance Counters

Four 64-bit hardware counters per core, accessible via CSR:

| CSR | Address | Counter | Description |
|-----|---------|---------|-------------|
| `PERF_CYCLES` | `0x68` | Core cycles | Total clock cycles since reset |
| `PERF_STALLS` | `0x69` | Stall cycles | Cycles spent waiting for bus/memory |
| `PERF_TILE_OPS` | `0x6A` | Tile operations | Total MEX instructions completed |
| `PERF_EXTMEM` | `0x6B` | Ext mem beats | 64-bit external memory transfers |
| `PERF_CTRL` | `0x6C` | Control | Bit 0: enable counting, Bit 1: reset all |

**Usage**: Software reads counters to compute utilization metrics:

```forth
\ Read cycles and stalls
PERF_CYCLES CSRR R0    ( total-cycles )
PERF_STALLS CSRR R1    ( stall-cycles )
\ Utilization = (cycles - stalls) / cycles × 100
```

Performance counters are per-core and independent. Reading a counter
is non-destructive. The PERF_CTRL reset atomically zeros all four
counters.

---

## 7. MMIO Address Map (Extended)

Additions to the existing MMIO map:

| Offset | Size | Peripheral |
|--------|------|------------|
| 0x000 | 16B | UART (existing) |
| 0x100 | 16B | Timer (existing) |
| 0x200 | 16B | Disk (existing) |
| 0x300 | 16B | SysInfo (existing) |
| 0x400 | 128B | NIC (existing) |
| 0x500 | 256B | Mailbox (existing) |
| 0x600 | 256B | Spinlocks (existing) |
| **0x700** | **64B** | **AES-256-GCM** |
| **0x780** | **64B** | **SHA-3/SHAKE** |
| **0x7C0** | **32B** | **CRC32/CRC64** |
| **0x7E0** | **16B** | **QoS global config** |

### CSR Address Map (Extended)

New per-core CSRs:

| Range | Family |
|-------|--------|
| 0x00–0x09 | CPU core (existing) |
| 0x10–0x1C | Tile engine (existing) |
| 0x20–0x25 | Multicore (existing) |
| 0x30–0x31 | System info (existing) |
| **0x40–0x43** | **Strided/2D tile addressing** |
| **0x50–0x59** | **DMA + QoS** |
| **0x60–0x6C** | **BIST + perf counters** |

---

## 8. Encoding Summary

### Extended TMODE

```
Bit  7  6  5  4  3  2  1  0
     R  RM SAT S  x  EW EW EW
                      ─────── 
R   = reserved
RM  = rounding mode (0=truncate, 1=round-to-nearest)
SAT = saturating mode (0=wrapping, 1=saturating)
S   = signed (existing)
EW  = element width, 3-bit (extended from 2-bit):
      000=8, 001=16, 010=32, 011=64, 100=fp16, 101=bf16
```

### New MEX Functions (via existing funct codes)

TMUL funct 2–5 (WMUL, MAC, FMA, DOTACC) and TRED funct 5–7 (SUMSQ,
MINIDX, MAXIDX) fit within the existing 3-bit funct field.

### Extended Tile Ops (FAM_EXT = 0xF)

```
0xF0: EXTALU SS=0 (tile×tile)    — VSHR, VSHL, VSEL, VCLZ, ...
0xF4: EXTALU SS=1 (broadcast)
0xF8: EXTALU SS=2 (imm8)
0xFC: EXTALU SS=3 (in-place)
0xF1: EXTSYS (LOAD2D, STORE2D, PREFETCH, FENCE)
```

---

## 9. Implementation Priority

Recommended build order based on dependencies and complexity:

| Phase | Feature | Effort | Depends On |
|-------|---------|--------|------------|
| **A** | Performance counters (§6.3) | 1 day | None |
| **A** | Enhanced reductions (§2.4) | 2 days | None |
| **A** | TMUL/MAC family (§2.1) | 2 days | None |
| **B** | Saturating arith + rounding (§2.2) | 2 days | TMODE extension |
| **B** | Tile views / SHUFFLE (§2.3) | 3 days | None |
| **B** | CRC32/CRC64 (§4.3) | 2 days | None |
| **B** | Memory BIST (§6.1) | 2 days | None |
| **C** | Strided/2D addressing (§2.5) | 3 days | None |
| **C** | Tile datapath self-test (§6.2) | 1 day | Phase A tile ops |
| **C** | HW tile DMA (§5.1) | 5 days | None |
| **D** | FP16/bf16 tile ops (§3.1) | 5 days | TMODE 3-bit EW |
| **D** | SHA-3/SHAKE (§4.2) | 5 days | None |
| **D** | AES-256-GCM (§4.1) | 7 days | None |
| **E** | Prefetch/write-combine (§5.2) | 3 days | None |
| **E** | QoS arbitration (§5.3) | 2 days | None |
| **E** | Optional scalar FP32 (§3.2) | 3 days | FP16 datapath |

**Total estimated effort**: ~48 engineering-days across 5 phases.

---

## 10. Testing Strategy

Each feature gets:
1. **Unit testbench** (`tb_<feature>.v`): isolated Verilog test of the
   new module or datapath
2. **Integration test**: added to `tb_multicore_smoke.v` or a new
   `tb_extended_tpu.v` that runs in full SoC context
3. **Emulator parity**: matching implementation in `megapad64.py` so
   software can be developed in parallel with RTL
4. **BIOS Forth words**: thin wrappers (TSHUFFLE, TPACK, TAES-ENC, etc.)
   added to the BIOS dictionary as each feature lands

---

## 11. Open Questions

1. **FP16 accumulator width**: Should DOT/SUM with fp16 inputs use
   FP32 or FP64 accumulators? FP32 matches industry practice (TPU,
   Tensor Cores) but FP64 eliminates all precision concerns for our
   64-lane tiles. **Recommendation**: FP32 — keeps ACC register
   interpretation simple (2 × FP32 per ACC slot).

2. **AES key scheduling**: Pre-expand the key schedule in software
   (saves ~200 LUTs) or in hardware (saves 240 bytes of key schedule
   memory)? **Recommendation**: Hardware — the key schedule is
   deterministic and the area cost is modest.

3. **DMA descriptor ring size**: Fixed 16-entry rings or configurable?
   **Recommendation**: Configurable (DMA_RING_SIZE CSR) with a max of
   64 entries.

4. **Tile shuffle index width**: 6 bits (supports up to 64 lanes) or 8
   bits (allows out-of-range → zero)? **Recommendation**: 8 bits —
   out-of-range indices produce zero, which is useful for padding.

5. **BIST timing**: Should BIST block core 0 boot, or run in parallel
   on a dedicated FSM while core 0 starts? **Recommendation**: Block
   boot — BIST is fast (10–30 ms) and a BRAM fault is catastrophic.
