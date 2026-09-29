# MegaPad floating-point specification

**Status:** Normative. Adopted 2026-09-29 as Phase 1 of
`docs/megapad-full-float-plan.md`. The backends converge on this definition in
Phases 2–8; §12 records what is implemented. Until a phase lands, the
per-backend behaviour recorded in `docs/tile-engine.md` and
`docs/simulator-contract.md` remains the description of current code.

This document is the single definition of every floating-point result on
MegaPad. The Python emulator, native accelerator, hosted simulator, and RTL
implement it. None of them defines it. Where another document disagrees about
floating-point semantics, this one wins.

## 1. Scope

- Tile-engine floating-point formats FP16, BF16, FP32, and FP64 (§2–§7).
- The scalar floating-point engine `FC` (§8) and its control register
  `FPCSR` (§9).
- Cycle costs for the architectural timing model (§10).
- BIOS Forth words (§11).

Integer tile behaviour is out of scope except where this document changes it:
the 4-bit `TMODE.EW` field, `TACC_STATUS` packing, and running MIN/MAX under
`ACC_ACC`.

## 2. Formats

| Name | `TMODE.EW` | Bits | Exponent bits | Stored fraction bits | Precision p | Bias | Tile lanes | Accumulation format `A` | Canonical NaN |
|---|---:|---:|---:|---:|---:|---:|---:|---|---|
| FP16 (IEEE binary16) | 4 | 16 | 5 | 10 | 11 | 15 | 32 | binary32 | `0x7E00` |
| BF16 (bfloat16) | 5 | 16 | 8 | 7 | 8 | 127 | 32 | binary32 | `0x7FC0` |
| FP32 (IEEE binary32) | 6 | 32 | 8 | 23 | 24 | 127 | 16 | binary64 | `0x7FC00000` |
| FP64 (IEEE binary64) | 7 | 64 | 11 | 52 | 53 | 1023 | 8 | binary64 | `0x7FF8000000000000` |

All values are little-endian in memory, and lane `i` of a tile occupies bytes
`[i × w, (i + 1) × w)` for lane width `w`. A NaN is quiet when the most
significant stored fraction bit is 1 and signalling when that bit is 0 and the
fraction is nonzero.

## 3. General rules

**3.1 One rounding.** Every arithmetic operation is defined as its exact real
result rounded once to the destination format. Fused operations (FMA, MAC,
TAMAC, and the tree nodes of §4) round exactly once. No operation rounds
through an intermediate format unless this document names that intermediate.

**3.2 Rounding direction.** Tile operations round to nearest, ties to even
(RNE). The one exception is float-to-integer `TCVT`, which follows
`TMODE[6]` (§6.3). Scalar operations use the rounding direction in their
instruction or in `FPCSR.RM` (§8, §9).

**3.3 Subnormals.** Subnormal inputs and results are supported everywhere.
There is no flush-to-zero and no denormals-are-zero mode.

**3.4 NaN results.** Every arithmetic operation that produces a NaN produces
the canonical quiet NaN of its destination format (§2). This holds whether
the NaN came from a NaN input or from an invalid operation. Payloads and sign
are not propagated. The invalid operations are:

- `0 × ∞`, including inside FMA and TAMAC, even when the addend is a quiet NaN;
- `∞ − ∞`, meaning an addition of infinities with opposite signs;
- `0 / 0` and `∞ / ∞`;
- the square root of a value less than zero;
- conversion of NaN or of an out-of-range value to an integer (§6.3, §8.4).

**3.5 Raw-bit operations.** The following do not interpret lanes as numbers,
and they keep payloads, sign, and signalling bits unchanged:

- ABS (which clears the sign bit only) and AND, OR, XOR;
- VSEL, SHUFFLE, TRANS, RROT;
- MOVBANK, LOADC, ZERO;
- LOAD2D and STORE2D;
- register broadcast;
- the TACC image load and store.

The integer ALU performs scalar sign manipulation (negate, absolute value,
copy sign) as bit operations.

**3.6 Signed zero.**

- An exact zero sum of operands with opposite signs is +0 under every
  rounding direction except round-down, where it is −0.
- `x + x` keeps the sign of `x` when `x` is zero.
- `√(−0) = −0`.
- Comparisons treat −0 and +0 as equal.
- The minimum and maximum operations of §3.8 order −0 below +0.

**3.7 Overflow.** A finite result that overflows becomes ±∞ under RNE and RMM.
Under the directed modes it becomes ±∞ or the largest finite value, following
IEEE 754.

**3.8 Minimum and maximum.** There are two families.

- *Propagating* (IEEE 754-2019 `minimum` and `maximum`). The result is the
  canonical NaN if either operand is NaN, and −0 is less than +0. These
  operations use it: TALU MIN and MAX, and scalar FMIN and FMAX.
- *NaN-skipping.* NaN lanes are ignored, −0 is less than +0, and ties choose
  the lowest lane index. If every candidate is NaN, the result is the
  canonical NaN. These operations use it: TRED MIN, MAX, MINIDX, and MAXIDX,
  including their running comparison with the accumulator under `ACC_ACC`
  (§4.4).

**3.9 Exception flags.** Tile operations raise no floating-point flags and
never trap on floating-point exceptions. Scalar operations set the sticky
flags in `FPCSR` (§9) under IEEE 754 default exception handling. Tininess is
detected after rounding, and underflow is flagged only when the result is both
tiny and inexact.

## 4. Accumulation and the canonical reduction tree

**4.1 Accumulation format.** Each float format has an accumulation format
`A`, given in §2. Converting a lane value to `A` is always exact.
Legacy-accumulator results are one `A` value in ACC0. A binary32 value is
zero-extended into ACC0 `[63:0]`, and reading an old binary32 ACC0 uses bits
`[31:0]`.

**4.2 Leaves.** For a tile of `n` lanes (`n` is 32, 16, or 8), leaf `i` is:

| Operation | Leaf `i` |
|---|---|
| TRED SUM | `widen_A(x[i])` |
| TRED L1 | `widen_A(|x[i]|)` |
| TRED SUMSQ | `RN_A(x[i] × x[i])` |
| TMUL DOT, DOTACC | `RN_A(a[i] × b[i])` |

The product is exact in `A` for FP16 and FP32 inputs. For BF16 it can round
only at the binary32 overflow and underflow limits. For FP64 it rounds.

**4.3 Tree.** Leaves are summed pairwise, in lane order, one level at a time,
with one RNE rounding to `A` at each node:

```
tree(v[0..m-1]):              # m is a power of two
    while m > 1:
        for j in 0 .. m/2 - 1:
            v[j] = RN_A(v[2j] + v[2j+1])
        m = m / 2
    return v[0]
```

DOT, SUM, SUMSQ, and L1 use `tree(leaf[0..n-1])`. DOTACC chunk `k`, for `k`
from 0 to 3, uses `tree(leaf[k·n/4 .. (k+1)·n/4 − 1])`. The tree shape never
depends on how an implementation schedules the adds.

**4.4 Publication to the legacy accumulator.** Let `r` be the tree result, or
for DOTACC the four chunk results `r0`–`r3`.

- **`TCTRL.ACC_ZERO` set.** The result is published directly with no add:
  ACC0 = `r` (ACCk = `rk` for DOTACC). The other ACC words become zero, and
  `ACC_ZERO` auto-clears. A −0 result is kept. `ACC_ZERO` takes priority over
  `ACC_ACC`.
- **Otherwise, `ACC_ACC` set.** For DOT, SUM, SUMSQ, and L1, ACC0 =
  `RN_A(ACC0 + r)`. For DOTACC, ACCk = `RN_A(ACCk + rk)`. For TRED MIN and
  MAX, ACC0 is the NaN-skipping minimum or maximum of the old ACC0 and `r`.
- **Otherwise.** ACC0 = `r` (ACCk = `rk` for DOTACC).

For every operation except DOTACC, ACC1–ACC3 become zero in all three cases.
Z is set when the published ACC0 value is ±0; for DOTACC, when all four
published values are ±0.

**4.5 MINIDX and MAXIDX.** Within a tile, the result is the lane index `i` and
value `v` of the NaN-skipping extreme (§3.8). An all-NaN tile gives `i = 0`
and `v` = the canonical NaN.

- With `ACC_ACC` (and not `ACC_ZERO`), the tile result replaces ACC0 = `i` and
  ACC1 = `v` only when `v` is strictly better than the old ACC1 value (compared
  in `A`), or the old ACC1 value is NaN and `v` is not.
- Otherwise ACC0 = `i` and ACC1 = `v` widened exactly to `A` (the canonical
  NaN of `A` for an all-NaN tile); `ACC_ZERO`, when set, auto-clears.
- ACC2 and ACC3 become zero, and Z is set when ACC0 is zero.

**4.6 Integer MIN and MAX.** Under `ACC_ACC`, integer TRED MIN and MAX now keep
a running minimum or maximum against ACC0. ACC0 is read at 64 bits, with
signedness from `TMODE[4]`. The result is published as before. Previously they
added the tile result to the 256-bit accumulator, which gave neither a minimum
nor a maximum.

## 5. Tile operations in float formats

**5.1 Operands.**

- **Tile × tile, SS=0.** A = `[TSRC0]` and B = `[TSRC1]`.
- **Broadcast, SS=1.** A = `[TSRC0]`. B is the low `w` bits of Rn, replicated
  to every lane as raw bits.
- **Immediate, SS=2.** The unsigned immediate (0–255) is converted exactly to
  the lane format and replicated. It is used as operand A and the tile
  `[TSRC0]` as operand B, and the function is forced to 0 as for integer
  formats. Every value from 0 to 255 is exact in all four formats.
- **In-place, SS=3.** A = `[TDST]` and B = `[TSRC0]`.

**5.2 Operation table.** Rounding is RNE (§3.2). In the table, `A` is the
accumulation format.

| Class | Funct | Operation | Float definition |
|---|---:|---|---|
| TALU | 0 | ADD | `dst[i] = RN(a[i] + b[i])` |
| TALU | 1 | SUB | `dst[i] = RN(a[i] − b[i])` |
| TALU | 2–4 | AND, OR, XOR | raw bits |
| TALU | 5, 6 | MIN, MAX | propagating (§3.8) |
| TALU | 7 | ABS | raw: clear the sign bit of `a[i]` |
| TMUL | 0 | MUL | `dst[i] = RN(a[i] × b[i])` |
| TMUL | 1 | DOT | tree over products → ACC0 (§4) |
| TMUL | 2 | WMUL | FP16, BF16: `RN_32(a[i] × b[i])` as binary32. FP32: exact `a[i] × b[i]` as binary64. Lanes `0..n/2−1` go to `[TDST]` and the rest to `[TDST+64]`. FP64: illegal |
| TMUL | 3 | MAC | `dst[i] = RN(a[i] × b[i] + dst[i])`, fused |
| TMUL | 4 | FMA | `dst[i] = RN(a[i] × b[i] + dst[i])`, fused. The same operation as MAC in float formats |
| TMUL | 5 | DOTACC | four chunk trees → ACC0–ACC3 (§4) |
| TMUL | 6 | TAMAC | §7 |
| TRED | 0 | SUM | tree → ACC0 |
| TRED | 1, 2 | MIN, MAX | NaN-skipping → ACC0 |
| TRED | 3 | POPCNT | raw set-bit count at lane width; integer accumulator rules |
| TRED | 4 | L1 | tree of `|x[i]|` → ACC0 |
| TRED | 5 | SUMSQ | tree of squares → ACC0 |
| TRED | 6, 7 | MINIDX, MAXIDX | §4.5 |
| TSYS | 0–4, 7 | TRANS, SHUFFLE, MOVBANK, LOADC, ZERO, RROT | raw; SHUFFLE and RROT use the format's lane width |
| TSYS | 5, 6 | PACK, UNPACK | illegal in float formats; use `TCVT` |
| EXT TALU | 0, 1, 3 | VSHR, VSHL, VCLZ | illegal in float formats |
| EXT TALU | 2 | VSEL | raw (§6.4) |
| EXT TALU | 4–7 | TDIV, TSQRT, TCVT, TCMP | §6 |

RROT geometry follows lane width: a 16-bit lane gives 4×8, a 32-bit lane
gives 4×4, and a 64-bit lane gives 2×4. SHUFFLE reads its index tile at the
format's lane width as unsigned integers, modulo the lane count.

**5.3 Illegal cases.** The following raise `IVEC_ILLEGAL_OP` before any memory
access or state change:

- a reserved `TMODE.EW` (8–15);
- an operation marked illegal for its format;
- a noncanonical encoding of a new operation (§6).

The reserved-format rule covers every MEX tile operation, including the raw
ones. The TACC lifecycle has its own format rule (§7): `CLEAR`, `LOAD`, and
`TAMAC` trap on a format that is not a legal TACC format, and `TRY`, `STORE`,
and `RELEASE` do not read `TMODE`.

## 6. New extended tile operations

These use the `EXT.8` prefix before a TALU-class MEX byte:
`F8 E0|SS<<2 funct [Rn]`. With SS=2 the function byte is the immediate, so
the function is forced to 0 as it is today. Functions 4–7 therefore cannot be
reached with SS=2.

**6.1 TDIV (function 4).** In float formats only, `dst[i] = RN(a[i] / b[i])`.
Legal sources are SS=0, SS=1, and SS=3, and function byte bits `[7:3]` must be
zero.

- A nonzero finite value divided by zero gives ±∞.
- `0/0` and `∞/∞` give the canonical NaN.

**6.2 TSQRT (function 5).** In float formats only, `dst[i] = RN(√a[i])`.
Operand B is not read. Only SS=0 is legal, and bits `[7:3]` must be zero.

For FP16 and BF16, an implementation may compute TDIV and TSQRT in binary32 and
round once to the lane format. That is exactly correct because 24 ≥ 2p + 2.

**6.3 TCVT (function 6).** Converts a contiguous region from source format
`S = TMODE.EW` to target format `T`.

- **Encoding.** The function byte is `[7:4] = T`, `[3] = 0`, `[2:0] = 6`.
  Only SS=0 is legal.
- **Illegal cases.** It traps if S or T is reserved, `S = T`, or both are
  integer formats.
- **Region shape.** Let `bs` and `bt` be the byte widths of S and T.
  - Widening (`bt > bs`, `k = bt/bs`): reads the tile at `TSRC0` and writes
    `k` tiles at `TDST + 64j`, for `j` from 0 to `k−1`.
  - Narrowing (`bt < bs`, `k = bs/bt`): reads `k` tiles at `TSRC0 + 64j` and
    writes one tile at `TDST`.
  - Equal width: one tile in, one tile out.

  Source lane `i` maps to destination lane `i` across the region. All source
  tiles are read before any destination tile is written, so overlapping
  regions behave as if copied first. Every tile address in both regions
  follows the ordinary tile alignment rule.
- **Values.**
  - int → float: `RN(value)`, with source signedness from `TMODE[4]`.
  - float → float: `RN(value)`. Widening is exact. A NaN becomes the target's
    canonical NaN.
  - float → int: NaN becomes 0. Otherwise the value is rounded toward zero
    when `TMODE[6] = 0`, or RNE when `TMODE[6] = 1`. It is then saturated to
    the target's range, with target signedness from `TMODE[4]`. `TMODE[5]` is
    not used.

**6.4 VSEL (function 2).** VSEL now has one definition in every format:
`dst[i] = msb(M[i]) ? A[i] : B[i]`.

- M is the old `[TDST]`, read before the write.
- A is `[TSRC0]`.
- B is `[TSRC1]` for SS=0, or broadcast Rn for SS=1.
- SS=3 is illegal. SS=2 cannot reach VSEL.

Lanes are raw bits at the format's lane width. This replaces the current,
inconsistent placeholders: Python and native return A, and RTL returns
`msb(B) ? A : 0`.

**6.5 TCMP (function 7).**

- **Encoding.** The function byte is `[7:6] = 0`, `[5:3] = predicate`,
  `[2:0] = 7`. Legal sources are SS=0, SS=1, and SS=3.
- **Result.** `dst[i]` is all ones when `pred(a[i], b[i])` is true and zero
  otherwise, at the format's lane width.

| Predicate | Name | Float meaning | Integer meaning (signedness from `TMODE[4]`) |
|---:|---|---|---|
| 0 | EQ | ordered and equal | equal |
| 1 | NE | unordered or not equal | not equal |
| 2 | LT | ordered and less | less |
| 3 | LE | ordered and less or equal | less or equal |
| 4 | GT | ordered and greater | greater |
| 5 | GE | ordered and greater or equal | greater or equal |
| 6 | UNORD | either is NaN | always false |
| 7 | ORD | neither is NaN | always true |

The result masks feed VSEL directly, and they also work with AND, OR, and XOR.

## 7. TACC float formats

`TACC.CLEAR` and `TACC.LOAD` latch `TMODE.EW` as today. The float formats
are:

| EW | Input lanes | TACC lane | `TAMAC` per lane | Active image |
|---:|---:|---|---|---:|
| 4 — FP16 | 32 | binary32 | `RN_32(acc + a×b)`, exact product | 128 bytes |
| 5 — BF16 | 32 | binary32 | `RN_32(acc + a×b)`, exact product | 128 bytes |
| 6 — FP32 | 16 | binary64 | `RN_64(acc + a×b)`, exact product | 128 bytes |
| 7 — FP64 | 8 | binary64 | `RN_64(acc + a×b)`, fused | 64 bytes |

- NaN results are the canonical NaN of the TACC lane format and stay
  canonical under later accumulation.
- Broadcast uses the low `w` bits of Rn.
- Inactive image bytes store as zero, and load as ignored input that commits
  as zero.
- Only EW 0 and 1 use bytes 128–255. EW 3 stays illegal.

## 8. Scalar floating-point engine (`FC`)

**8.1 Encoding.** `FC` is a self-contained engine prefix, like `F9`, `FA`, and
`FB`.

```
FC op DR          3 bytes   two-operand form
FC op DR T        4 bytes   three-operand form (FMA, FMS only)
```

- `DR` is `[Rd:4][Rs:4]`.
- `T` is `[7:5] = 0`, `[4:0] = Rt`.
- A REX prefix extends Rd and Rs as it does for EXT.STRING.
- `op[7:6]` is the format: `00` = S (binary32), `01` = D (binary64). `10`
  and `11` are reserved.
- `op[5:0]` is the operation (§8.3).

An undefined operation, a reserved format, or nonzero `T[7:5]` raises
`IVEC_ILLEGAL_OP`. The unassigned prefixes F7, FD, FE, and FF raise
`IVEC_ILLEGAL_OP` instead of latching as a silent modifier.

**8.2 Values in registers.** A D value uses all 64 bits. An S value uses bits
`[31:0]`: S results write zero to `[63:32]`, and S inputs ignore `[63:32]`.
Integer results (FEQ, FLT, FLE, FCLASS, FCVT to integer) write all 64 bits.
Operations other than FCMP leave FLAGS unchanged.

**8.3 Operations.** `RM` means the static mode in the opcode or, when that is
7, `FPCSR.RM`. The static modes are 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM, and 7
dynamic. The values 5 and 6 are reserved and trap. Operations without a mode
field use `FPCSR.RM`.

| `op[5:0]` | Mnemonic | Operation | Flags |
|---|---|---|---|
| `0x00` | `FADD.f Rd, Rs` | `Rd ← RN(Rd + Rs)` | NV OF UF NX |
| `0x01` | `FSUB.f Rd, Rs` | `Rd ← RN(Rd − Rs)` | NV OF UF NX |
| `0x02` | `FMUL.f Rd, Rs` | `Rd ← RN(Rd × Rs)` | NV OF UF NX |
| `0x03` | `FDIV.f Rd, Rs` | `Rd ← RN(Rd ÷ Rs)` | NV DZ OF UF NX |
| `0x04` | `FSQRT.f Rd, Rs` | `Rd ← RN(√Rs)` | NV NX |
| `0x05` | `FMIN.f Rd, Rs` | propagating minimum (§3.8) | NV on sNaN |
| `0x06` | `FMAX.f Rd, Rs` | propagating maximum | NV on sNaN |
| `0x07` | `FMA.f Rd, Rs, Rt` | `Rd ← RN(Rs × Rt + Rd)` | NV OF UF NX |
| `0x08` | `FMS.f Rd, Rs, Rt` | `Rd ← RN(Rd − Rs × Rt)` | NV OF UF NX |
| `0x10` | `FCMP.f Rd, Rs` | set FLAGS (§8.5); no register write | NV on sNaN |
| `0x11` | `FEQ.f Rd, Rs` | `Rd ← (Rd = Rs) ? −1 : 0` | NV on sNaN |
| `0x12` | `FLT.f Rd, Rs` | `Rd ← (Rd < Rs) ? −1 : 0` | NV on any NaN |
| `0x13` | `FLE.f Rd, Rs` | `Rd ← (Rd ≤ Rs) ? −1 : 0` | NV on any NaN |
| `0x14` | `FCLASS.f Rd, Rs` | `Rd ←` class mask of Rs (§8.6) | none |
| `0x20`–`0x27` | `FRND.f.rm Rd, Rs` | `Rd ←` Rs rounded to an integral value in the rounding mode given by `op[2:0]` | NV on sNaN |
| `0x28`–`0x2F` | `FCVT.L.f.rm Rd, Rs` | `Rd ←` signed int64 (§8.4), mode `op[2:0]` | NV NX |
| `0x30`–`0x37` | `FCVT.LU.f.rm Rd, Rs` | `Rd ←` unsigned int64 (§8.4), mode `op[2:0]` | NV NX |
| `0x38` | `FCVT.f.L Rd, Rs` | `Rd ← RN(signed int64 Rs)` | NX |
| `0x39` | `FCVT.f.LU Rd, Rs` | `Rd ← RN(unsigned int64 Rs)` | NX |
| `0x3A` | `FCVT.f.F Rd, Rs` | `Rd ←` Rs converted from the other of S/D to `f`; S←D rounds, D←S is exact | NV OF UF NX |
| `0x3B` | `FCVT.H.f Rd, Rs` | `Rd ←` binary16 bits of `RN(Rs)`, zero-extended | NV OF UF NX |
| `0x3C` | `FCVT.f.H Rd, Rs` | `Rd ←` `Rs[15:0]` binary16 converted exactly to `f` | NV on sNaN |
| `0x3D` | `FCVT.B.f Rd, Rs` | `Rd ←` bfloat16 bits of `RN(Rs)`, zero-extended | NV OF UF NX |
| `0x3E` | `FCVT.f.B Rd, Rs` | `Rd ←` `Rs[15:0]` bfloat16 converted exactly to `f` | NV on sNaN |

The remaining codes (`0x09`–`0x0F`, `0x15`–`0x1F`, `0x3F`) are reserved.

Every NaN result is the canonical NaN of the destination format. A signalling
NaN input raises NV. FRND raises no NX, matching C's `nearbyint`, and it keeps
the sign of a zero result.

**8.4 Float to integer.** The value is rounded to an integer in `RM`, then:

- NaN gives 0 and raises NV;
- a value outside the target range saturates to the nearest bound and raises
  NV;
- otherwise NX is raised when the rounding was inexact.

The C cast is `FCVT.L.f.RTZ`.

**8.5 FCMP.** FCMP is a quiet comparison.

| Flag | Set when |
|---|---|
| Z | equal |
| G | greater |
| N | less |
| V | unordered |

C and P are cleared, and S and I are unchanged. The existing EQ, NE, GT, MI,
PL, VS, and VC branches then read as their names say. LE tests only `G = 0`,
so it is also taken for unordered operands. Code that must separate NaN tests
VS first, or uses FLE.

**8.6 FCLASS.** Exactly one bit is set:

| Bit | Class |
|---:|---|
| 0 | −∞ |
| 1 | negative normal |
| 2 | negative subnormal |
| 3 | −0 |
| 4 | +0 |
| 5 | positive subnormal |
| 6 | positive normal |
| 7 | +∞ |
| 8 | signalling NaN |
| 9 | quiet NaN |

**8.7 Microcores.** Microcores implement the complete `FC` engine through one
FP unit shared by each cluster. They reach it through the same request and
completion port as MUL/DIV, and pay the same +3-cycle admission cost. The
flags an operation raises are ORed into the issuing microcore's own `FPCSR`.

## 9. `FPCSR` (CSR `0x0D`)

| Bits | Field | Meaning |
|---|---|---|
| `[2:0]` | `RM` | Dynamic rounding mode: 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM; 5–7 reserved |
| `[3]` | — | Reserved, reads zero |
| `[4]` | `NX` | Inexact (sticky) |
| `[5]` | `UF` | Underflow (sticky) |
| `[6]` | `OF` | Overflow (sticky) |
| `[7]` | `DZ` | Divide by zero (sticky) |
| `[8]` | `NV` | Invalid operation (sticky) |
| `[63:9]` | — | Reserved; writes ignored, reads zero |

- **Scope.** `FPCSR` is private to each core. Each microcore has its own.
- **Reset.** It resets to zero, and flags clear only when software writes
  them.
- **Access.** Both privilege levels may read and write it with CSRR and
  CSRW.
- **Reserved `RM`.** A dynamic-mode operation with a reserved `RM` value
  raises `IVEC_ILLEGAL_OP`.
- **Context switch.** A task switch that moves floating-point work between
  tasks must save and restore `FPCSR`.

## 10. Timing model

These costs are the extra cycles added to the base instruction cycle. Memory
beats, transport stalls, and cluster admission are added on top, exactly as
for existing operations.

- **Physical basis.** The physical FP32/FP64 path is `P` multi-format FMA
  units per engine, and the production chip has `P = 2`. Each unit completes
  one binary64 operation or two binary32 operations per beat.
- **Tree schedule.** The tree schedule reserves one beat for the `ACC_ACC`
  add whether or not `ACC_ACC` is set, so cost never depends on `TCTRL`.
- **FP16 and BF16.** These keep their 32 parallel lanes and the existing
  costs.

| Operation | FP16 / BF16 | FP32 | FP64 |
|---|---:|---:|---:|
| TALU ADD, SUB | 0 | 4 | 4 |
| TALU MIN, MAX, ABS, AND, OR, XOR | 0 | 0 | 0 |
| TMUL MUL | 1 | 4 | 4 |
| TMUL MAC, FMA | 2 | 4 | 4 |
| TMUL WMUL | 2 | 5 | illegal |
| TMUL DOT | 3 | 13 | 9 |
| TMUL DOTACC | 3 | 12 | 8 |
| TRED SUM, L1 | 0 | 9 | 5 |
| TRED SUMSQ | 0 | 13 | 9 |
| TRED MIN, MAX, MINIDX, MAXIDX, POPCNT | 0 | 0 | 0 |
| `TAMAC` arithmetic (plus 1 broadcast or 2 tile source cycles) | 4 | 8 | 4 |
| `TCVT`, width ratio `k` (1 for equal widths) | `4 + (k − 1)` | `4 + (k − 1)` | `4 + (k − 1)` |
| `TCMP`, `VSEL` | 1 | 1 | 1 |
| `TDIV`, `TSQRT` | fixed in Phase 8 | fixed in Phase 8 | fixed in Phase 8 |

`TCVT`, `TCMP`, and `VSEL` cost the same in integer formats.

The resulting full-core `TAMAC` totals are:

| Source | FP16/BF16 | FP32 | FP64 |
|---|---:|---:|---:|
| tile×tile or in-place | 7 | 11 | 7 |
| broadcast | 6 | 10 | 6 |

Scalar `FC` extra cycles are:

| Operations | Extra cycles |
|---|---:|
| FADD, FSUB, FMUL, FMA, FMS, FRND, all FCVT | 3 |
| FMIN, FMAX, FCMP, FEQ, FLT, FLE, FCLASS | 1 |
| FDIV, FSQRT | a data-independent constant per format, fixed in Phase 7 |

Microcores add the +3-cycle cluster admission cost.

Divide and square-root latencies must not depend on operand values. The phase
that chooses their algorithm records the constants here.

## 11. BIOS Forth words

Scalar words operate on cells. An FP32 value lives in the low 32 bits of the
cell. Flags are the canonical Forth −1 or 0.

**Tile mode and operations**

| Word | Stack effect | Meaning |
|---|---|---|
| `FP32-MODE` | `( -- )` | `6 TMODE!` |
| `FP64-MODE` | `( -- )` | `7 TMODE!` |
| `TCVT` | `( ew -- )` | Convert from the current `TMODE` format to `ew` (§6.3) |
| `TCMP` | `( pred -- )` | Compare to mask with predicate 0–7 (§6.5) |
| `TVSEL` | `( -- )` | Select by mask (§6.4) |
| `TDIV` | `( -- )` | Lane divide |
| `TSQRT` | `( -- )` | Lane square root |

**Scalar arithmetic.** The words below are listed with an `F32` prefix, and
there are `F64` words with the same shapes.

| Word | Stack effect | Operation |
|---|---|---|
| `F32+` `F32-` `F32*` `F32/` | `( r1 r2 -- r3 )` | FADD, FSUB, FMUL, FDIV |
| `F32SQRT` | `( r -- r )` | FSQRT |
| `F32FMA` | `( a b c -- a×b+c )` | FMA |
| `F32MIN` `F32MAX` | `( r1 r2 -- r3 )` | propagating min/max |
| `F32=` `F32<` `F32<=` | `( r1 r2 -- flag )` | FEQ, FLT, FLE |
| `F32CLASS` | `( r -- mask )` | FCLASS |
| `F32ROUND` `F32TRUNC` `F32FLOOR` `F32CEIL` | `( r -- r )` | FRND with RNE, RTZ, RDN, RUP |
| `S>F32` `U>F32` | `( n -- r )` | FCVT from signed or unsigned int64 |
| `F32>S` `F32>U` | `( r -- n )` | FCVT to int64 toward zero |

**Conversions and control**

| Word | Stack effect | Operation |
|---|---|---|
| `F32>F64` `F64>F32` | `( r -- r )` | FCVT between S and D |
| `F16>F32` `F32>F16` `BF16>F32` `F32>BF16` | `( r -- r )` | half-format conversions (and the `F64` forms) |
| `FPCSR@` `FPCSR!` | `( -- u )` `( u -- )` | Read or write `FPCSR` |

The hosted simulator binds the same words to the reference oracle, because it
models no instruction encodings.

## 12. Implementation status

| Area | Phase | Status |
|---|---|---|
| This specification | 1 | Adopted |
| Exact reference oracle (`shared/ieee_fp.py`) | 2 | Implemented |
| FP16/BF16 unification, including §3, §4, §5.1, fused MAC/FMA, and integer running MIN/MAX: Python emulator, native accelerator, hosted simulator | 2 | Implemented |
| FP16/BF16 unification: RTL | 2 | Implemented |
| 4-bit `TMODE.EW`, `TMODE`/`TCTRL` write widths, `TACC_STATUS` repack, format descriptors, `FP32-MODE`, `FP64-MODE`; EW 6 and 7 trap until Phases 4–5 | 3 | Implemented |
| FP32/FP64 element-wise operations, operand forms, lane shapes, and the §10 costs; illegal VSHR/VSHL/VCLZ in every float format; the multi-format FMA unit: all four backends | 4 | Implemented |
| FP32/FP64 reductions and TACC formats | 5 | Specified |
| `TCVT`, `TCMP`, `VSEL`; removal of float PACK and UNPACK | 6 | Specified |
| Scalar `FC` engine and `FPCSR` | 7 | Specified |
| `TDIV`, `TSQRT` | 8 | Specified |
