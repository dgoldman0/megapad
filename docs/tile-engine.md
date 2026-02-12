# Tile Engine Programming Guide

The Megapad-64's **tile engine** is a 64-byte SIMD accelerator built into
the CPU.  It processes data in **tiles** — 64-byte aligned memory regions —
and can perform element-wise arithmetic, dot products, reductions, and
utility operations across 8 to 64 lanes simultaneously.

This guide covers:
- What a tile is and how the engine thinks about data
- The tile CSR registers that control everything
- All four instruction categories (ALU, MUL, Reduction, System)
- Extended operations (VSHR/VSHL/VCLZ, LOAD2D/STORE2D)
- Source selection modes (tile×tile, broadcast, imm8 splat, in-place)
- The 256-bit accumulator
- FP16 / BF16 half-precision support
- BIOS Forth words for tile operations
- How KDOS uses the tile engine for buffers, kernels, and pipelines
- Worked examples

---

## What Is a Tile?

A **tile** is simply 64 contiguous bytes in memory, aligned to a 64-byte
boundary.  The tile engine doesn't have its own register file — it reads
from and writes to main memory through address pointers stored in CSRs.

Depending on the element width, a single tile contains:

| Element Width | Lanes | Type | Tile Capacity |
|---------------|-------|------|---------------|
| 8-bit | 64 | u8 / i8 | 64 values |
| 16-bit | 32 | u16 / i16 | 32 values |
| 32-bit | 16 | u32 / i32 | 16 values |
| 64-bit | 8 | u64 / i64 | 8 values |

Every tile operation processes **all lanes in parallel** in a single
instruction.  For a 1024-byte buffer at 8-bit width, that's 16 tiles ×
1 instruction = 16 instructions to process the entire buffer.

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

All three must point to **64-byte-aligned** addresses.

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
| MIN/MAX | NaN-propagating comparison |
| ABS | Clear sign bit |
| DOT | FP16→FP32 widening multiply, FP32 accumulation |
| SUM | FP16→FP32 widening, FP32 accumulation |
| SUMSQ | FP16→FP32 square, FP32 accumulation |

### FP32 Accumulation

When computing DOT, SUM, or SUMSQ with FP16/BF16 inputs, products are
computed in **FP32 precision** and accumulated with FP32 addition.  The
accumulator registers ACC0–ACC3 hold FP32 values.  This matches modern
AI accelerator behavior (TPU, Tensor Cores) and prevents catastrophic
precision loss during large summations.

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

### CRC Engine

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRC-POLY!` | `( n -- )` | Select polynomial (0=CRC32, 1=CRC32C, 2=CRC64) |
| `CRC-INIT!` | `( n -- )` | Set initial CRC value |
| `CRC-FEED` | `( n -- )` | Feed 8 bytes of data |
| `CRC@` | `( -- n )` | Read current CRC value |
| `CRC-RESET` | `( -- )` | Reset CRC engine |
| `CRC-FINAL` | `( -- )` | Finalize CRC (XOR-out) |

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

**How `B.SUM` works internally:**

1. Set `TMODE` to 8-bit unsigned (`0`)
2. Set `TCTRL` to `2` (zero accumulator)
3. For each 64-byte tile in the buffer:
   - Point `TSRC0` at the current tile offset
   - Execute `TSUM`
   - Switch `TCTRL` to `1` (accumulate) after the first tile
4. Read `ACC@` and push it to the data stack

**How `B.ADD` works internally:**

1. Compute the number of tiles: `buffer-size / 64`
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
256 0 1 BUFFER: my-a
256 0 1 BUFFER: my-b
3 my-a B.FILL   7 my-b B.FILL
my-a my-b kcorrelate .   \ Prints 5376
```

---

## Performance Tips

1. **Always use `TMODE` 0 (8-bit) when possible** — 64 lanes is the
   maximum parallelism the engine offers.

2. **Minimize TCTRL writes** — set `ACC_ZERO` once at the start, then
   `ACC_ACC` once.  Don't re-set them every tile.

3. **Keep data tile-aligned** — use `TALIGN` before allocating buffers.
   Misaligned access still works but may cause unexpected boundary effects.

4. **Use KDOS buffer words** — `B.SUM`, `B.ADD`, etc. handle the
   tile-iteration loop for you, correctly.

5. **TZERO is the fastest memory clear** — 64 bytes per instruction, much
   faster than a CPU byte loop.

6. **MOVBANK for bulk copies** — 64 bytes per instruction, useful for
   data staging.
