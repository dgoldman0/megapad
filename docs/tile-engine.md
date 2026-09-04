# Tile Engine Programming Guide

The Megapad-64's **tile engine** is a 64-byte SIMD accelerator built into
the CPU.  It processes data in **tiles** — 64-byte aligned memory regions —
and can perform element-wise arithmetic, dot products, reductions, and
utility operations across 8 to 64 lanes simultaneously.

The production chip has exactly seven physical tile engines.  Full cores
0–3 each have a private engine; each of the three four-microcore clusters
shares one engine behind a deterministic round-robin arbiter (+3 cycles after
a microcore wins cluster admission).  All 16 cores issue the same MEX ISA.

A full core's tile configuration, legacy accumulator, and full-width tile
accumulator are private.  Microcores keep private shadows of their cursor,
mode, control, source, destination, and stride CSRs, while the legacy
accumulator and TACC belong to the cluster's shared physical engine.  A
granted request samples the issuing microcore's shadows, so a sibling changing
its own configuration cannot retarget an in-flight operation.

This guide covers:
- What a tile is and how the engine thinks about data
- The tile CSR registers that control everything
- All four instruction categories (ALU, MUL, Reduction, System)
- Extended operations (VSHR/VSHL/VCLZ, LOAD2D/STORE2D)
- Source selection modes (tile×tile, broadcast, imm8 splat, in-place)
- The 256-bit accumulator
- The explicit 2,048-bit full-width tile accumulator (TACC)
- FP16 / BF16 half-precision support
- BIOS Forth words for tile operations
- How KDOS uses the tile engine for buffers, kernels, and pipelines
- Worked examples

---

## What Is a Tile?

A **tile** is simply 64 contiguous bytes in memory, aligned to a 64-byte
boundary.  Ordinary tile operands do not live in a separate tile register
file: the engine reads and writes them through address pointers stored in
CSRs.  Legacy ACC and TACC are separate architectural result state.

Depending on the element width, a single tile contains:

| Element Width | Lanes | Type | Tile Capacity |
|---------------|-------|------|---------------|
| 8-bit | 64 | u8 / i8 | 64 values |
| 16-bit | 32 | u16 / i16 | 32 values |
| 32-bit | 16 | u32 / i32 | 16 values |
| 64-bit | 8 | u64 / i64 | 8 values |

Architecturally, every tile operation processes **all lanes** in a single
instruction; an implementation may schedule those lanes over fixed arithmetic
beats.  For a 1024-byte buffer at 8-bit width, that's 16 tiles × 1 instruction
= 16 instructions to process the entire buffer.

---

## Tile CSR Registers

The tile engine is controlled entirely through **Control/Status Registers**
(CSRs), accessed with the `CSRW`/`CSRR` instructions (or the Forth words
`TMODE!`, `TSRC0!`, etc.).

### Address Registers

| CSR | Address | Forth Word | Description |
|-----|---------|------------|-------------|
| `TSRC0` | `0x16` | `TSRC0!` | **Source tile 0** — pointer to the first input tile |
| `TSRC1` | `0x17` | `TSRC1!` | **Source tile 1** — pointer to the second input tile |
| `TDST` | `0x18` | `TDST!` | **Destination tile** — where results are written |

Software should normally make all three addresses **64-byte aligned**. The
current backends do not yet agree on misalignment: internal RTL aliases down
to a tile boundary, the untimed executable emulator uses the exact address,
and strict-cycle transport rejects an unaligned beat. KDOS `ARENA-BUFFER`
guarantees only eight-byte alignment. Until that discrepancy is resolved,
portable code must align explicitly; the hosted semantic simulator uses the
exact address and requires the complete 64-byte span to fit one mapped region.

### Mode Register (TMODE, CSR `0x14`)

`TMODE` controls element width, signedness, saturation, rounding, and
FP mode selection.

```
Bit layout:    7  6  5  4  3  2  1  0
               ── ┬─ ┬─ ┬─ ── ┬──┬──┬─
               R  │  │  │  R  │EW│EW│EW
                  │  │  │     └──┴──┘
                  │  │  │  Element width (3 bits)
                  │  │  └─ Signed flag
                  │  └─── Saturation mode
                  └────── Rounding mode
```

| Bits | Field | Values |
|------|-------|--------|
| `[2:0]` | Element Width (EW) | `0`=8-bit, `1`=16-bit, `2`=32-bit, `3`=64-bit, `4`=fp16, `5`=bf16 |
| `[4]` | Signed | `0`=unsigned, `1`=signed (affects MIN, MAX, ABS, MUL, DOT, SUM, L1) |
| `[5]` | Saturation | `0`=wrapping, `1`=saturating (clamp on overflow for ADD/SUB/PACK) |
| `[6]` | Rounding | `0`=truncate, `1`=round-to-nearest (applies to VSHR) |

**Common TMODE values:**

| Value | Meaning |
|-------|---------|
| `0x00` | Unsigned 8-bit (64 lanes) — the most common mode |
| `0x10` | Signed 8-bit (64 lanes) |
| `0x01` | Unsigned 16-bit (32 lanes) |
| `0x11` | Signed 16-bit (32 lanes) |
| `0x02` | Unsigned 32-bit (16 lanes) |
| `0x03` | Unsigned 64-bit (8 lanes) |
| `0x04` | FP16 / IEEE 754 half (32 lanes) |
| `0x05` | BF16 / bfloat16 (32 lanes) |
| `0x20` | Unsigned 8-bit, saturating |
| `0x30` | Signed 8-bit, saturating |
| `0x40` | Unsigned 8-bit, rounding shifts |

### Control Register (TCTRL, CSR `0x15`)

`TCTRL` controls accumulator behavior for DOT and reduction operations.

| Bit | Name | Description |
|-----|------|-------------|
| `0` | `ACC_ACC` | **Accumulate mode** — add result to existing accumulator value instead of overwriting |
| `1` | `ACC_ZERO` | **Zero-first** — clear ACC to zero before this operation, then auto-clear this bit |

**The typical pattern for multi-tile accumulation:**

```forth
2 TCTRL!      \ Set ACC_ZERO → clears ACC, does first op, auto-clears bit 1
              \ ... process first tile ...
1 TCTRL!      \ Set ACC_ACC → subsequent ops ADD into ACC
              \ ... process remaining tiles ...
ACC@          \ Read the accumulated result
```

### Accumulator (ACC0–ACC3, CSRs `0x19`–`0x1C`)

The accumulator is a **256-bit register** split across four 64-bit CSRs:

```
ACC3 (0x1C)    ACC2 (0x1B)    ACC1 (0x1A)    ACC0 (0x19)
[255:192]      [191:128]      [127:64]       [63:0]
```

- Used by all reduction operations (TSUM, TMIN, TMAX, TPOPCNT, TL1) and
  TDOT
- Low 64 bits (`ACC0`) are sufficient for most use cases
- Full 256-bit width prevents overflow during large accumulations
- The Z (zero) flag is set when the accumulated result equals zero

This legacy accumulator is distinct from TACC.  Existing `TDOT`, `TDOTACC`,
`TRED`, `TMAC`, `TFMA`, and `TCTRL` behavior is unchanged by the TACC
extension.

### Cursor Registers

The cursor provides a 2D addressing scheme for tiled data:

| CSR | Address | Description |
|-----|---------|-------------|
| `SB` | `0x10` | Bank selector (0–15, each bank is a 4 MiB aperture) |
| `SR` | `0x11` | Row index |
| `SC` | `0x12` | Column index |
| `SW` | `0x13` | Stride (row width in tiles) |

**Cursor address calculation:**

$$\text{addr} = \text{SB} \times 4\,\text{MiB} + (\text{SR} \times \text{SW} + \text{SC}) \times 64$$

The `LOADC` (TSYS funct 3) instruction loads a tile from the cursor
address into `TSRC0`.

### Stride / 2D Addressing Registers

These CSRs enable non-contiguous tile loads from 2D images (e.g., loading
an 8×8 patch from a 640-byte-wide framebuffer):

| CSR | Address | Forth Word | Description |
|-----|---------|------------|-------------|
| `TSTRIDE_R` | `0x40` | `TSTRIDE-R!` | Row stride in bytes (distance between rows) |
| `TSTRIDE_C` | `0x41` | — | Column stride in bytes |
| `TTILE_H` | `0x42` | `TTILE-H!` | Tile height (rows to load, 1–8) |
| `TTILE_W` | `0x43` | `TTILE-W!` | Tile width (columns per row in bytes, 1–64) |

When `TSTRIDE_R ≠ 0`, the LOAD2D/STORE2D operations perform strided
gather/scatter:

$$\text{for } r = 0..\text{TTILE\_H}-1: \quad \text{tile\_row}[r] = \text{mem}[\text{TSRC0} + r \times \text{TSTRIDE\_R}]$$

This is used with the extended TSYS instructions `LOAD2D` and `STORE2D`
(see Extended Operations below).

---

## MEX Instruction Encoding

All tile engine operations use the **MEX** instruction family (opcode
prefix `0xE`).  Each MEX instruction is 2 or 3 bytes:

```
Byte 0:  1110 SSOO    (0xE0 | SS<<2 | OP)
Byte 1:  funct        (sub-function code, bits [2:0])
Byte 2:  reg#         (only when SS=1, broadcast mode)
```

### Source Selection Modes (SS)

The SS field determines where the two source operands come from:

| SS | Mode | Source A | Source B | Description |
|----|------|----------|----------|-------------|
| `0` | **Tile × Tile** | `[TSRC0]` | `[TSRC1]` | Two independent memory tiles |
| `1` | **Broadcast** | `[TSRC0]` | Register Rn, splatted | A register value replicated to all lanes |
| `2` | **Imm8 Splat** | funct byte, splatted | `[TSRC0]` | The funct byte IS the immediate value; forced to ADD |
| `3` | **In-Place** | `[TDST]` | `[TSRC0]` | Destination tile doubles as source A |

**Broadcast mode** is useful for scaling — multiply every element by a
constant in a register.  **Imm8 splat** adds a small constant (0–255)
to every element in one instruction.  **In-place** mode lets you modify
a tile without needing a separate source buffer.

### Major Operations (OP)

| OP | Category | Result Goes To | Description |
|----|----------|---------------|-------------|
| `0` | **TALU** | `[TDST]` | Element-wise arithmetic and logic |
| `1` | **TMUL** | `[TDST]` or ACC | Multiplication and dot product |
| `2` | **TRED** | ACC | Reductions (sum, min, max, etc.) |
| `3` | **TSYS** | varies | Utility ops (transpose, zero, move, load) |

---

## TALU — Element-Wise Operations

TALU operations compute a per-lane function and write the result tile to
`[TDST]`.

| Funct | Mnemonic | Operation | Notes |
|-------|----------|-----------|-------|
| `0` | **ADD** | `dst[i] = a[i] + b[i]` | Wrapping; saturating if TMODE bit 5 set |
| `1` | **SUB** | `dst[i] = a[i] − b[i]` | Wrapping; saturating if TMODE bit 5 set |
| `2` | **AND** | `dst[i] = a[i] & b[i]` | Bitwise AND |
| `3` | **OR** | `dst[i] = a[i] \| b[i]` | Bitwise OR |
| `4` | **XOR** | `dst[i] = a[i] ^ b[i]` | Bitwise XOR |
| `5` | **MIN** | `dst[i] = min(a[i], b[i])` | Signed-aware when TMODE bit 4 set |
| `6` | **MAX** | `dst[i] = max(a[i], b[i])` | Signed-aware when TMODE bit 4 set |
| `7` | **ABS** | `dst[i] = |a[i]|` | Only uses src_a; unsigned → identity |

---

## TMUL — Multiplication & Dot Product

| Funct | Mnemonic | Operation | Result | Extra Cycles |
|-------|----------|-----------|--------|-------------|
| `0` | **MUL** | `dst[i] = a[i] × b[i]` | `[TDST]` | +1 |
| `1` | **DOT** | $\sum_i a_i \times b_i$ | 256-bit ACC | +3 |
| `2` | **WMUL** | `dst[2i:2i+1] = a[i] × b[i]` | `[TDST]` (double-width) | +1 |
| `3` | **MAC** | `dst[i] += a[i] × b[i]` | `[TDST]` (in-place) | +1 |
| `4` | **FMA** | `dst[i] = a[i] × b[i] + c[i]` | `[TDST]` (c = TDST) | +1 |
| `5` | **DOTACC** | $\text{ACC}[k] += \text{dot}(\text{chunk}_k)$ | ACC0–ACC3 | +3 |
| `6` | **TAMAC** | `TACC[i] += widen(a[i] × b[i])` | 2,048-bit TACC | mode/source dependent |
| `7` | reserved | Illegal operation | — | — |

**DOT** is the workhorse for correlation and linear algebra.  It
multiplies corresponding lanes and sums the products into the accumulator,
respecting the `TCTRL` accumulate/zero bits.

**WMUL** doubles the element width in the output: 8→16, 16→32, 32→64.
Useful for preserving precision in intermediate results.

**MAC** multiplies corresponding elements and adds the products into the
existing destination tile, enabling multi-pass accumulation in the tile
domain.

**FMA** is fused multiply-add: `dst[i] = a[i] × b[i] + dst[i]`.  The
destination tile (TDST) serves as the addend, which is the standard
GEMM accumulation pattern.

**DOTACC** splits the tile into 4 equal chunks and produces 4 independent
dot products, one per accumulator register (ACC0–ACC3).  Useful for 4-wide
vector dot products in GEMM inner loops.

**TAMAC** accumulates widened products lane-by-lane into persistent TACC
state.  Tile×tile (`E1 06`), register broadcast (`E5 06 Rn`), and in-place
(`ED 06`) forms are legal.  Immediate splat is not: its function byte is the
immediate, so that source selector traps before reading memory or changing
TACC.  `TAMAC` never writes `TDST`.

### Multi-Tile Dot Product Pattern

```forth
0 TMODE!                      \ 8-bit unsigned
2 TCTRL!                      \ Zero ACC before first tile

\ First tile pair
addr-a TSRC0!  addr-b TSRC1!
TDOT                          \ ACC = dot(tile0_a, tile0_b)

1 TCTRL!                      \ Accumulate mode for subsequent tiles

\ Second tile pair
addr-a 64 + TSRC0!  addr-b 64 + TSRC1!
TDOT                          \ ACC += dot(tile1_a, tile1_b)

\ ... repeat for remaining tiles ...

ACC@                          \ Read the total dot product
```

---

## TRED — Reduction Operations

Reductions collapse all lanes of source tile A into a single scalar
stored in the 256-bit accumulator.  All respect `TCTRL` bits.

| Funct | Mnemonic | Operation | Notes |
|-------|----------|-----------|-------|
| `0` | **SUM** | $\sum_i a_i$ | Sum of all lanes; signed-aware |
| `1` | **MIN** | $\min_i a_i$ | Minimum lane value; signed-aware |
| `2` | **MAX** | $\max_i a_i$ | Maximum lane value; signed-aware |
| `3` | **POPCNT** | $\sum_i \text{popcount}(a_i)$ | Total count of set bits across all lanes |
| `4` | **L1** | $\sum_i |a_i|$ | L1 norm; signed-aware absolute values |
| `5` | **SUMSQ** | $\sum_i a_i^2$ | Sum of squares (L2² norm); widened to prevent overflow |
| `6` | **MINIDX** | $\text{argmin}_i a_i$ | ACC0 = index of min, ACC1 = min value |
| `7` | **MAXIDX** | $\text{argmax}_i a_i$ | ACC0 = index of max, ACC1 = max value |

### Multi-Tile Sum Example

```forth
0 TMODE!                 \ 8-bit unsigned
2 TCTRL!                 \ Zero ACC

\ First tile
buffer-addr TSRC0!
TSUM                     \ ACC = sum of first 64 bytes

1 TCTRL!                 \ Accumulate for remaining tiles

\ Next tiles...
buffer-addr 64 + TSRC0!
TSUM                     \ ACC += sum of next 64 bytes

ACC@                     \ Total sum of entire buffer
```

---

## TSYS — System / Utility Operations

| Funct | Mnemonic | Operation | Extra Cycles |
|-------|----------|-----------|-------------|
| `0` | **TRANS** | Transpose `[TDST]` as 8×8 byte matrix in-place | 0 |
| `1` | **SHUFFLE** | Permute lanes by index tile: `dst[i] = src0[idx[i]]` | +2 |
| `2` | **MOVBANK** | Copy `[TSRC0]` → `[TDST]` (64-byte tile copy) | +2 |
| `3` | **LOADC** | Load tile from cursor address → `[TSRC0]` | 0 |
| `4` | **ZERO** | Zero 64 bytes at `[TDST]` | 0 |
| `5` | **PACK** | Narrow elements (32→16, 16→8, etc.); saturating if TMODE bit 5 | +1 |
| `6` | **UNPACK** | Widen elements (8→16, 16→32, etc.); sign-extend if TMODE bit 4 | +1 |
| `7` | **RROT** | Row/column rotate or mirror (controlled by imm8 byte) | +1 |

**TRANS** treats the 64 bytes as an 8×8 matrix and swaps rows and
columns.  Useful for data layout transformations.

**SHUFFLE** uses `[TSRC1]` as an index tile — each element is a lane
index.  Output: `dst[i] = src0[index[i]]`.  Out-of-range indices produce
zero.  This is the universal permutation: any reordering, duplication, or
broadcast can be expressed as a shuffle.

**PACK/UNPACK** convert between element widths.  PACK narrows (e.g.,
16-bit → 8-bit), with optional saturation (TMODE bit 5).  UNPACK widens
(e.g., 8-bit → 16-bit), with sign extension if TMODE bit 4 is set.

**RROT** rotates or mirrors the tile treated as a 2D matrix. The geometry
depends on element width: 8-bit = 8×8, 16-bit = 4×8, 32-bit = 4×4,
64-bit = 2×4. Controlled by the `imm8` byte (byte 1 of the MEX insn):

| Bits | Field | Values |
|------|-------|--------|
| `[1:0]` | Direction | 0=row-left, 1=row-right, 2=col-up, 3=col-down |
| `[4:2]` | Amount | 0–7 positions |
| `[5]` | Mirror | 1=mirror instead of rotate (bit 0 selects H vs V) |

**ZERO** is the fastest way to clear memory — 64 bytes in a single
instruction.

---

## Extended Tile Operations

These operations are encoded with the EXT prefix family (`0xF_`) instead
of the standard MEX family (`0xE_`).

### Extended TALU (VSHR, VSHL, VSEL, VCLZ)

Per-lane shift and selection operations, accessed via `EXT.8` prefix
followed by a TALU-class instruction:

| Funct | Mnemonic | Operation | Notes |
|-------|----------|-----------|-------|
| 0 | **VSHR** | `dst[i] = a[i] >> b[i]` | Right shift; rounds if TMODE bit 6 set |
| 1 | **VSHL** | `dst[i] = a[i] << b[i]` | Left shift |
| 2 | **VSEL** | `dst[i] = mask[i] ? a[i] : b[i]` | Conditional select |
| 3 | **VCLZ** | `dst[i] = clz(a[i])` | Count leading zeros per lane |

**Rounding shifts**: When TMODE bit 6 is set, VSHR adds the bit that's
about to be shifted out before truncating (round-to-nearest).  This is
the standard DSP rounding behavior.

### Extended TSYS (LOAD2D, STORE2D)

Strided 2D tile load/store operations for accessing non-contiguous
memory regions:

| Funct | Mnemonic | Operation |
|-------|----------|-----------|
| 0 | **LOAD2D** | Gather rows from `TSRC0 + row × TSTRIDE_R` into tile |
| 1 | **STORE2D** | Scatter tile rows to `TDST + row × TSTRIDE_R` |

These use the stride CSRs (TSTRIDE_R, TTILE_H, TTILE_W) to load/store
non-contiguous tile data.  For example, to load an 8×8 patch from a
640-pixel-wide framebuffer:

```forth
640 TSTRIDE-R!            \ Row stride = 640 bytes
8 TTILE-H!               \ 8 rows
8 TTILE-W!               \ 8 columns per row
frame-base TSRC0!        \ Start address
TLOAD2D                  \ Gather 8×8 patch into tile
```

---

## FP16 / BF16 Half-Precision Support

The tile engine supports IEEE 754 half-precision (FP16) and Google
bfloat16 (BF16) floating-point operations across 32 lanes.

### Enabling FP Mode

Set TMODE element width to 4 (FP16) or 5 (BF16):

```forth
4 TMODE!    \ FP16 mode — or use the convenience word:
FP16-MODE   \ Sets TMODE = 4

5 TMODE!    \ BF16 mode — or:
BF16-MODE   \ Sets TMODE = 5
```

### Supported FP Operations

All standard TALU, TMUL, and TRED operations work with FP16/BF16:

| Operation | Behavior |
|-----------|----------|
| ADD/SUB | IEEE round-to-nearest-even |
| MUL | FP16×FP16 → FP16 |
| FMA | FP16×FP16 + FP16 → FP16 |
| MIN/MAX (TALU) | NaN-**propagating** — if either input is NaN, result is qNaN |
| MIN/MAX (TRED) | NaN-**skipping** — NaN lanes are ignored; first non-NaN wins |
| ABS | Clear sign bit |
| DOT | FP16→FP32 widening multiply, FP32 accumulation |
| SUM | FP16→FP32 widening, FP32 accumulation |
| SUMSQ | FP16→FP32 square, FP32 accumulation |

> **Note:** The TMODE signed flag (bit 4) is irrelevant in FP mode.
> Floating-point comparisons are inherently signed via the sign bit;
> `mode_signed` is not checked on the FP MIN/MAX path.

### FP32 Accumulation

DOT, SUM, and SUMSQ publish one raw binary32 result in ACC0; the Python and
hosted paths clear ACC1--ACC3. `TDOTACC` instead publishes four binary32 chunk
results across ACC0--ACC3.

The reduction order is not yet one backend-independent FP32 algorithm. Python
and the hosted simulator use host-language `sum` for each SUM/SUMSQ tile and
pack once to binary32; the native accelerator currently routes those functions
back to Python, though its direct C++ body uses sequential binary32. RTL uses a
balanced binary32 tree. TDOT uses a binary64 loop in Python/native before its
binary32 pack, while RTL has its own tree. Cancellation and signed-zero results
can differ, so “FP32 accumulation” names the output/intent rather than a bitwise
cross-backend guarantee.
For ACC_ACC, Python/hosted execution widens the existing binary32 ACC0, adds it
to the tile subtotal in binary64, and repacks; that pack is the inter-tile
rounding point.

There is also a known executable conversion discrepancy: the exact FP16
product `0x0017 * 0x5190` lies at the largest-subnormal/minimum-normal tie.
Python/C++ and the hosted compatibility model currently encode it as zero,
where IEEE round-to-nearest-even would produce `0x0400`. Reserved EW 6/7 are
not formats: hosted execution rejects them, while existing Python/C++ and RTL
paths alias them differently. These discrepancies remain open.

---

## Full-Width Tile Accumulator (TACC)

> **Implementation status:** the Python oracle, native accelerator, and
> strict-cycle system model implement this contract in emulator Phase 1.
> Portable RTL implementation is Phase 2 work; the existing RTL must not yet
> be described as implementing TACC.

Each physical tile engine owns one 2,048-bit TACC bank plus owner, valid,
dirty, format, busy, force-pending, and cancellation metadata.  Software
controls its complete lifetime:

1. claim with `TACC.TRY`;
2. initialize with `TACC.CLEAR` or restore with `TACC.LOAD`;
3. execute one or more `TAMAC` instructions;
4. save with `TACC.STORE` when required; and
5. zeroize and relinquish it with `TACC.RELEASE`.

There is no implicit claim, blocking wait, spill, eviction, migration, or
release.  Ownership reserves only persistent TACC state, not the engine:
nonowners may continue stateless MEX work and legacy-ACC operations.  Current
cluster admission, image-stage acquisition, and tile-memory service are equal
round-robin.  Future software-controlled QoS weights may change service order,
but never TACC arithmetic, ownership, image, fault, or retirement semantics.

### Lifecycle instructions

Lifecycle operations use the extended-TSYS namespace.  The canonical source
selector is zero and the upper five function bits are zero; noncanonical
aliases trap.

| Encoding | Assembly | BIOS word | Operation |
|---|---|---|---|
| `F8 E3 02` | `t.acc.try` | `TACC-TRY` | Claim a free bank or retain self-ownership; never wait |
| `F8 E3 03` | `t.acc.clear` | `TACC-CLEAR` | Require ownership, latch `TMODE` format, and clear active lanes |
| `F8 E3 04` | `t.acc.load` | `TACC-LOAD` | Load the canonical image at `TSRC0` |
| `F8 E3 05` | `t.acc.store` | `TACC-STORE` | Store the canonical image at `TDST` |
| `F8 E3 06` | `t.acc.release` | `TACC-RELEASE` | Zeroize, invalidate, and release |
| `F8 E3 07` | — | — | Reserved; illegal operation |

`TACC.TRY` by the existing owner is idempotent.  Losing a claim retires
normally with no mutation; software reads `TACC_STATUS.MINE` to decide whether
to proceed.  Every other protected operation requires ownership.  `TAMAC` and
`STORE` additionally require valid state.

### Status and recovery control

`TACC_STATUS` is a read-only CSR at `0x1D`.  `MINE` is caller-relative; every
other field describes the physical engine.

| Bits | Field | Meaning |
|---|---|---|
| `[0]` | `CLAIMED` | A caller owns this bank |
| `[1]` | `MINE` | The reading core is that owner |
| `[2]` | `VALID` | `CLEAR` or `LOAD` established value and format |
| `[3]` | `DIRTY` | State changed since the last successful `LOAD` or `STORE` |
| `[4]` | `BUSY` | A TACC operation is in flight |
| `[7:5]` | `FORMAT_EW` | Latched element-width code; zero when invalid |
| `[8]` | `FORMAT_SIGNED` | Latched integer signedness |
| `[9]` | `FORCE_PENDING` | Privileged recovery is queued behind active work |
| `[20:16]` | `OWNER` | Absolute core ID; 31 means no owner |

`TACC_CTL` at `0x1E` reads as zero.  A supervisor write of bit 0 pulses
`FORCE_RELEASE`; a user write with bit 0 set raises `IVEC_PRIV_FAULT`.
Recovery zeroizes the bank and clears ownership.  An accepted force pulse has
priority over same-cycle admission.  If work is active, it first reaches its
normal retirement-or-trap boundary, then the queued force wins before any new
TACC admission.  Normal software uses `TACC.RELEASE`; force-release exists for
a terminated or otherwise dead owner.

Production full-core owner IDs are 0–3; microcluster core-ID bases are 4, 8,
and 12.  `OWNER` always reports the absolute issuing core ID rather than a
cluster-local index.

### Formats and arithmetic

`CLEAR` and `LOAD` latch the current `TMODE.EW` and integer signed bit.
`TAMAC` must match that format.  Saturation and shift-rounding bits are not
part of the TACC format and do not affect accumulation.

| `TMODE.EW` | Input lanes | Accumulator lane | Active image |
|---:|---:|---|---:|
| 0 — U8/S8 | 64 | 32-bit integer | 256 bytes |
| 1 — U16/S16 | 32 | 64-bit integer | 256 bytes |
| 2 — U32/S32 | 16 | 64-bit integer | 128 bytes |
| 4 — FP16 | 32 | binary32 | 128 bytes |
| 5 — BF16 | 32 | binary32 | 128 bytes |

EW 3, 6, and 7 are illegal for `CLEAR`, `LOAD`, and `TAMAC`.  Integer
products are exact, extended according to signedness, and accumulated modulo
the lane width without saturation.  Broadcast uses only the low active-width
bits of its GPR.  FP16/BF16 products enter binary32 before one
round-to-nearest-even addition per lane per `TAMAC`; subnormals and IEEE
signed zero are preserved.  NaN input, `0 × infinity`, or invalid infinity
addition produces canonical quiet NaN `0x7FC00000`, which remains canonical
on later accumulations.  Inactive high bank bits are always zero after
initialization or accumulation.

### Canonical image, memory, and faults

`TACC.LOAD` and `TACC.STORE` transfer exactly 256 bytes aligned to 64 bytes as
four consecutive 64-byte beats.  Lanes and bytes within each lane are
little-endian.  U8/S8 and U16/S16 use the full image.  U32/S32, FP16, and BF16
use bytes 0–127; `STORE` writes zeros to bytes 128–255 and `LOAD` ignores them
and commits zeros.  Transfers do not advance or rewrite source, destination,
or cursor CSRs.  A saved context therefore consists of the 256-byte image plus
its format.

Internal memory, attached RAM, and external RAM use the same image.  MMIO is
not a legal image target.  The entire span is preflighted under the caller's
ordinary routed-memory, privilege, and active-MPU policy before a store issues
its first beat.  User-mode HBW images are forbidden.

- Misalignment raises `IVEC_ALIGN_FAULT` with the base in `TRAP_ADDR`.
- Nonownership, invalid state, format mismatch, unsupported mode, and
  noncanonical encoding raise `IVEC_ILLEGAL_OP` before source access or
  mutation and leave `TRAP_ADDR` unchanged.
- A forbidden span faults at its first forbidden byte and issues no store
  beat.
- A source or transport bus error reports the faulting beat or external PHY
  word address.
- `LOAD` stages privately and publishes only after its final acknowledgement;
  every load fault leaves the old bank unchanged.
- A failed external `STORE` may leave only its acknowledged memory prefix
  visible.  TACC remains valid and retains its preinstruction `DIRTY` value.
- A faulting instruction does not retire or increment `PERF_TILE_OPS`.

Every TACC trap saves the architectural PC after the complete decoded
instruction, matching the existing MEX return-PC convention.  An unsuccessful
`TACC.TRY`, by contrast, retires normally and does increment `PERF_TILE_OPS`.

External images serialize each 64-byte beat into eight 64-bit PHY words.
Each successful word increments `PERF_EXTMEM`.  A response on cycle 255 wins;
no response, or a response later than 255 cycles, becomes an acknowledged bus
fault at the exact current word rather than hanging the engine.

### Context switching, reset, and cancellation

Interrupts and ordinary traps preserve ownership and TACC.  A task may retain
state when it deliberately resumes on the same core, but ownership identifies
a core rather than an OS task.  Before migrating an owning task, software must
save any dirty state, retain the format alongside its image, and release the
bank.  Restore by claiming the destination engine, setting the saved `TMODE`
format, and loading the image.

Whole-SoC reset wipes all seven banks.  Resetting one full-core execution
domain wipes only its paired private engine; cluster disable or cluster-engine
reset wipes only that shared engine.  Resetting one microcore cancels that
caller's pending or active operation but does not wipe its cluster's shared
TACC.  Epoch-tagged cancellation rejects late acknowledgements, while already
acknowledged external store words remain visible.

### Phase 1 measured timing

These are uncontended full-core measurements.  “Instruction step” is the
functional Python/native execution API; “strict system” includes image-stage
capture and the registered tile-memory request/ACK path.

| Image path | Instruction step | Strict system | Successful `PERF_EXTMEM` |
|---|---:|---:|---:|
| Internal/attached memory | 6 cycles, 0 added stalls | 9 cycles, 3 stalls | 0 |
| External, default one-cycle PHY response | 34 cycles, 28 stalls | 37 cycles, 31 stalls | 32 |
| External, two-cycle PHY response | 66 cycles, 60 stalls | 69 cycles, 63 stalls | 32 |

The default external result is the six-cycle base path with four tile beats
replaced by 32 serialized PHY-word responses; the strict path adds three
registered-fabric cycles.  Contention and longer PHY responses add elapsed
stall cycles.  A microcore MEX instruction also pays the existing fixed
three-cycle cluster-dispatch cost after it wins admission.

Other uncontended full-core base totals are:

| Operation | Base cycles |
|---|---:|
| `CSRR TACC_STATUS` or `CSRW TACC_CTL` | 1 |
| `TACC.TRY`, `TACC.CLEAR`, or `TACC.RELEASE` | 2 |
| Integer tile×tile/in-place `TAMAC`, U8/U16/U32 | 7 / 5 / 4 |
| Integer broadcast `TAMAC`, U8/U16/U32 | 6 / 4 / 3 |
| FP16/BF16 tile×tile or in-place `TAMAC` | 7 |
| FP16/BF16 broadcast `TAMAC` | 6 |

---

## Complete MEX Opcode Map

For assembler authors and low-level debugging, here is every MEX byte:

| Byte 0 | SS | OP | Category | Size |
|--------|----|----|----------|------|
| `0xE0` | 0 | 0 | TALU tile×tile | 2B |
| `0xE1` | 0 | 1 | TMUL tile×tile | 2B |
| `0xE2` | 0 | 2 | TRED tile×tile | 2B |
| `0xE3` | 0 | 3 | TSYS tile×tile | 2B |
| `0xE4` | 1 | 0 | TALU broadcast | 3B |
| `0xE5` | 1 | 1 | TMUL broadcast | 3B |
| `0xE6` | 1 | 2 | TRED broadcast | 3B |
| `0xE7` | 1 | 3 | TSYS broadcast | 3B |
| `0xE8` | 2 | 0 | TALU imm8 splat | 2B |
| `0xE9` | 2 | 1 | TMUL imm8 splat | 2B |
| `0xEA` | 2 | 2 | TRED imm8 splat | 2B |
| `0xEB` | 2 | 3 | TSYS imm8 splat | 2B |
| `0xEC` | 3 | 0 | TALU in-place | 2B |
| `0xED` | 3 | 1 | TMUL in-place | 2B |
| `0xEE` | 3 | 2 | TRED in-place | 2B |
| `0xEF` | 3 | 3 | TSYS in-place | 2B |

---

## BIOS Forth Words

The BIOS provides thin Forth wrappers for every tile CSR and operation.
These emit the corresponding MEX instruction or CSR access inline.

### CSR Access

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSRC0!` | `( addr -- )` | Set source tile 0 address |
| `TSRC1!` | `( addr -- )` | Set source tile 1 address |
| `TDST!` | `( addr -- )` | Set destination tile address |
| `TMODE!` | `( mode -- )` | Set element width and signedness |
| `TCTRL!` | `( ctrl -- )` | Set accumulator control bits |
| `TMODE@` | `( -- mode )` | Read current TMODE |
| `TCTRL@` | `( -- ctrl )` | Read current TCTRL |
| `ACC@` | `( -- n )` | Read low 64 bits of accumulator |
| `ACC1@` | `( -- n )` | Read accumulator bits [127:64] |
| `ACC2@` | `( -- n )` | Read accumulator bits [191:128] |
| `ACC3@` | `( -- n )` | Read accumulator bits [255:192] |
| `TACC-STATUS@` | `( -- u )` | Read raw `TACC_STATUS` |
| `TACC-CLAIM?` | `( -- flag )` | Try once and return canonical true only when `MINE=1` |

### ALU Operations

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TADD` | `( -- )` | Element-wise add: `[TSRC0] + [TSRC1]` → `[TDST]` |
| `TSUB` | `( -- )` | Element-wise sub: `[TSRC0] − [TSRC1]` → `[TDST]` |
| `TAND` | `( -- )` | Bitwise AND → `[TDST]` |
| `TOR` | `( -- )` | Bitwise OR → `[TDST]` |
| `TXOR` | `( -- )` | Bitwise XOR → `[TDST]` |
| `TEMIN` | `( -- )` | Element-wise min → `[TDST]` |
| `TEMAX` | `( -- )` | Element-wise max → `[TDST]` |
| `TABS` | `( -- )` | Element-wise absolute value → `[TDST]` |

### Multiplication

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TMUL` | `( -- )` | Element-wise multiply → `[TDST]` |
| `TDOT` | `( -- )` | Dot product → ACC (respects TCTRL) |
| `TWMUL` | `( -- )` | Widening multiply → `[TDST]` (double-width output) |
| `TMAC` | `( -- )` | Multiply-accumulate in-place → `[TDST]` |
| `TFMA` | `( -- )` | Fused multiply-add: `a×b + dst` → `[TDST]` |
| `TDOTACC` | `( -- )` | 4-way chunked dot product → ACC0–ACC3 |
| `TAMAC` | `( -- )` | Accumulate widened `[TSRC0] × [TSRC1]` products into TACC |

### TACC Lifecycle

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TACC-TRY` | `( -- )` | Attempt one nonblocking claim |
| `TACC-CLEAR` | `( -- )` | Initialize owned TACC in the current `TMODE` format |
| `TACC-LOAD` | `( -- )` | Load the 256-byte image at `TSRC0` |
| `TACC-STORE` | `( -- )` | Store the 256-byte image at `TDST` |
| `TACC-RELEASE` | `( -- )` | Zeroize and release owned TACC |

The convenience word does not hide a spin.  Software chooses whether and how
long to retry:

```forth
: TACC-ACQUIRE
  BEGIN TACC-CLAIM? 0= WHILE PAUSE REPEAT ;
```

### Reductions

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSUM` | `( -- )` | Sum all lanes → ACC |
| `TMIN` | `( -- )` | Min across all lanes → ACC |
| `TMAX` | `( -- )` | Max across all lanes → ACC |
| `TPOPCNT` | `( -- )` | Population count → ACC |
| `TL1` | `( -- )` | L1 norm → ACC |
| `TSUMSQ` | `( -- )` | Sum of squares → ACC |
| `TMINIDX` | `( -- )` | Min with index: ACC0=index, ACC1=value |
| `TMAXIDX` | `( -- )` | Max with index: ACC0=index, ACC1=value |

### System & Utility

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TTRANS` | `( -- )` | 8×8 byte transpose of `[TDST]` in-place |
| `TZERO` | `( -- )` | Zero 64 bytes at `[TDST]` |
| `TI` | `( -- )` | Print all tile CSR values (debugging) |
| `TVIEW` | `( addr -- )` | Display 64 bytes as a 4×16 hex grid |
| `TFILL` | `( byte addr -- )` | Fill 64 bytes at addr with a byte value |
| `TALIGN` | `( -- )` | Align HERE to next 64-byte boundary |
| `TLOAD2D` | `( -- )` | Strided 2D gather using stride CSRs |
| `TSTORE2D` | `( -- )` | Strided 2D scatter using stride CSRs |

### Stride / 2D CSRs

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSTRIDE-R!` | `( n -- )` | Set row stride (CSR 0x40) |
| `TSTRIDE-R@` | `( -- n )` | Read row stride |
| `TTILE-H!` | `( n -- )` | Set tile height (CSR 0x42) |
| `TTILE-W!` | `( n -- )` | Set tile width (CSR 0x43) |

### FP16 / BF16 Mode

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FP16-MODE` | `( -- )` | Set TMODE = 4 (IEEE FP16, 32 lanes) |
| `BF16-MODE` | `( -- )` | Set TMODE = 5 (bfloat16, 32 lanes) |

### Diagnostics

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BIST-FULL` | `( -- )` | Run full memory self-test |
| `BIST-QUICK` | `( -- )` | Run quick memory self-test (March C− only) |
| `BIST-STATUS` | `( -- n )` | Read BIST result (0=idle, 2=pass, 3=fail) |
| `TILE-TEST` | `( -- )` | Run tile datapath self-test |
| `TILE-TEST@` | `( -- n )` | Read tile self-test result |
| `TILE-DETAIL@` | `( -- n )` | Read tile self-test failure detail bitmask |

### CRC Engine and Capability Discovery (9 BIOS words)

These are ISA-backed CRC words, not tile operations. Full cores have private
CRC state; a micro-core cluster shares an owner-arbitrated CRC engine. BIOS
adds full `(COREID,TASK-ID)` ownership checks under exact interrupt-state
preservation. The exact mode tuples and transaction semantics are defined in
[isa-reference.md](isa-reference.md).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRYPTO-CAPS@` | `( -- caps )` | Read raw System Info crypto capabilities |
| `CRC-MODE!` | `( mode -- status )` | Begin checked mode 0/1/2/4/5/6 without changing the accumulator |
| `CRC-RESET` | `( -- status )` | Require the exact owner and reset to the mode's all-ones initial value |
| `CRC-INIT!` | `( seed -- status )` | Require the exact owner and load a mode-width seed |
| `CRC-FEED` | `( cell -- status )` | Require the exact owner and feed 8 bytes, least-significant byte first |
| `CRC-FEED-BYTE` | `( byte -- status )` | Require the exact owner and feed exactly the low byte |
| `CRC@` | `( -- raw status )` | Return the running accumulator followed by checked status |
| `CRC-RAW-FINAL@` | `( -- raw status )` | Atomically return raw state and release; requires reflected/raw capability |
| `CRC-FINAL@` | `( -- finalized )` | Atomically XOR-finalize and release; misuse returns zero |

---

## KDOS Tile Integration

KDOS builds on the BIOS tile words to provide higher-level operations on
**buffers** — described data regions with type, element width, and length.

### Buffer Tile Operations

These words iterate over all tiles in a buffer, setting up the tile CSRs
for each chunk automatically:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `B.SUM` | `( buf -- n )` | Sum all elements using tile TSUM + accumulation |
| `B.MIN` | `( buf -- n )` | Find minimum element using tile TMIN per tile |
| `B.MAX` | `( buf -- n )` | Find maximum element using tile TMAX per tile |
| `B.ADD` | `( src1 src2 dst -- )` | Element-wise add using tile TADD per tile pair |
| `B.SUB` | `( src1 src2 dst -- )` | Element-wise subtract using tile TSUB per tile pair |

This table names the intended operation family, not stronger validation than
the current source performs. Every word above forces unsigned-byte TMODE.
Rounded-up final tiles are processed in full, so reductions include trailing
physical bytes and ADD/SUB can overwrite a partial destination tail. ADD/SUB
take their count only from the leftmost stack argument named `src1`, loaded
into hardware TSRC0. Current multi-tile B.MIN/B.MAX also have a
stack-order defect: after the first tile they use the running byte extreme as
TSRC0 instead of the advanced data address. `B.SCALE` is a separate scalar
modulo-256 byte loop, not a MEX operation.

**How `B.SUM` works internally:**

1. Set `TMODE` to 8-bit unsigned (`0`)
2. Set `TCTRL` to `2` (zero accumulator)
3. For each 64-byte tile in the buffer:
   - Point `TSRC0` at the current tile offset
   - Execute `TSUM`
   - Switch `TCTRL` to `1` (accumulate) after the first tile
4. Read `ACC@` and push it to the data stack

**How `B.ADD` works internally:**

1. Compute the ceiling tile count from `src1`'s byte size
2. For each tile index:
   - Point `TSRC0` at src1's tile, `TSRC1` at src2's tile, `TDST` at dst's tile
   - Execute `TADD`
3. The destination buffer now holds the element-wise sum

### Tile-Accelerated Kernels

KDOS provides named **kernels** — registered operations with declared
inputs, outputs, and tile-acceleration flags.  The following kernels use
the tile engine:

| Kernel | What It Does | Tile Operations Used |
|--------|-------------|---------------------|
| `kadd` | Element-wise add two buffers | `B.ADD` → `TADD` per tile |
| `ksum` | Sum all elements in a buffer | `B.SUM` → `TSUM` + accumulation |
| `kstats` | Compute sum, min, and max | `B.SUM`, `B.MIN`, `B.MAX` → `TSUM`, `TMIN`, `TMAX` |
| `knorm` | Normalize to 0–255 range | `B.MIN`, `B.MAX` → tile min/max, then CPU rescale |
| `kcorrelate` | Dot product of two buffers | `TDOT` per tile pair with accumulation |

### Pipeline Example

Pipelines chain multiple kernels.  Here's how the built-in
`pipe-add-stats` pipeline works:

```
Step 1: p2-init     → Fill demo-a with 10, demo-b with 20 (CPU)
Step 2: p2-add      → kadd(demo-a, demo-b, demo-c)
                       ↳ B.ADD → TADD per tile (TILE ENGINE)
Step 3: p2-stats    → kstats(demo-c)
                       ↳ B.SUM → TSUM per tile  (TILE ENGINE)
                       ↳ B.MIN → TMIN per tile  (TILE ENGINE)
                       ↳ B.MAX → TMAX per tile  (TILE ENGINE)
```

The tile engine is used **transparently** — the pipeline author just
names the kernels, and the kernel implementations decide whether to use
tile operations or CPU loops.

---

## Worked Example: Computing a Dot Product

Let's compute the dot product of two 256-byte buffers from scratch using
the BIOS tile words.

```forth
\ Allocate two 256-byte tile-aligned buffers
TALIGN HERE 256 ALLOT CONSTANT vec-a
TALIGN HERE 256 ALLOT CONSTANT vec-b

\ Fill vec-a with 3 and vec-b with 7
3 vec-a TFILL  3 vec-a 64 + TFILL
3 vec-a 128 + TFILL  3 vec-a 192 + TFILL
7 vec-b TFILL  7 vec-b 64 + TFILL
7 vec-b 128 + TFILL  7 vec-b 192 + TFILL

\ Configure: 8-bit unsigned, clear accumulator
0 TMODE!
2 TCTRL!

\ Process all 4 tile pairs (256 bytes ÷ 64 = 4 tiles)
vec-a       TSRC0!  vec-b       TSRC1!  TDOT   \ ACC = 64 × 3 × 7 = 1344
1 TCTRL!                                        \ Switch to accumulate
vec-a 64 +  TSRC0!  vec-b 64 +  TSRC1!  TDOT   \ ACC += 1344
vec-a 128 + TSRC0!  vec-b 128 + TSRC1!  TDOT   \ ACC += 1344
vec-a 192 + TSRC0!  vec-b 192 + TSRC1!  TDOT   \ ACC += 1344

ACC@ .   \ Prints 5376  (= 256 × 3 × 7)
```

Or, using KDOS buffers (much simpler):

```forth
0 1 256 BUFFER my-a
0 1 256 BUFFER my-b
3 my-a B.FILL   7 my-b B.FILL
my-a my-b kcorrelate .   \ Prints 5376
```

---

## Worked Examples: Avoiding Intermediate Tile Stores

The integer path can accumulate several tile pairs directly into TACC and
publish one canonical image at the end:

```forth
TACC-ACQUIRE
0 TMODE!  TACC-CLEAR             \ 64 U8 lanes → 64 widened 32-bit lanes
int-a       TSRC0!  int-b       TSRC1!  TAMAC
int-a 64 +  TSRC0!  int-b 64 +  TSRC1!  TAMAC
int-image TDST!  TACC-STORE
TACC-RELEASE
```

The same lifecycle applies to floating work; only the format and sources
change:

```forth
TACC-ACQUIRE
4 TMODE!  TACC-CLEAR             \ 32 FP16 products accumulate as binary32
fp-a       TSRC0!  fp-b       TSRC1!  TAMAC
fp-a 64 +  TSRC0!  fp-b 64 +  TSRC1!  TAMAC
fp-image TDST!  TACC-STORE
TACC-RELEASE
```

Both examples avoid the destination-tile load/add/store traffic required by
legacy `TMAC`/`TFMA` accumulation.  The stored result remains the canonical
256-byte TACC image rather than a silently narrowed tile.

---

## Performance Tips

1. **Always use `TMODE` 0 (8-bit) when possible** — 64 lanes is the
   maximum parallelism the engine offers.

2. **Minimize TCTRL writes** — set `ACC_ZERO` once at the start, then
   `ACC_ACC` once.  Don't re-set them every tile.

3. **Keep data tile-aligned** — use `TALIGN` before allocating buffers.
   TACC images require 64-byte alignment and trap if misaligned.

4. **Use KDOS buffer words** — `B.SUM`, `B.ADD`, etc. handle the
   tile-iteration loop for you, correctly.

5. **TZERO is the fastest memory clear** — 64 bytes per instruction, much
   faster than a CPU byte loop.

6. **MOVBANK for bulk copies** — 64 bytes per instruction, useful for
   data staging.

7. **Claim explicitly and keep retry policy visible** — use one
   `TACC-CLAIM?` when work is optional, or a software `PAUSE`/backoff loop when
   waiting is appropriate.  Ownership never supplies a hidden hardware spin.

8. **Keep TACC across the inner loop** — perform all `TAMAC` operations before
   one final store.  Do not store and reload the image between tile pairs.
