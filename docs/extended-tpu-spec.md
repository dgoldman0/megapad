# Extended TPU Specification

**Branch:** historical origin `features/extended-tpu-impl`; current contract is
integrated in the main design

**Status:** Original extended TPU and functional full-TACC RTL implemented;
routed FPGA acceptance pending

**Author:** auto-generated from design discussion

**Depends on:** Base tile engine (`mp64_tile.v`), ISA v2.1, 4-full-core plus
3-microcluster SoC

> The original extended-TPU features retain their implementation status below.
> The later full-width TACC extension is implemented in the Python/native
> emulator, strict-cycle model, and portable RTL.  Its emulator-generated
> arithmetic vectors are an executable RTL oracle.  Routed FPGA resource and
> timing acceptance remains pending.

---

## 1. Overview

The Extended TPU and full-TACC update add six capability families to the
Megapad-64's
existing tile engine and SoC infrastructure:

| Family | Purpose | Area Estimate | Status |
|--------|---------|---------------|--------|
| **Enhanced Tile Engine** | TMUL/MAC, views, richer reductions, strided addressing | Medium | ✅ Implemented |
| **Numeric Acceleration** | FP16/bfloat16 tile ops, optional scalar FP32 | Medium | ✅ FP16/BF16 done; ☐ scalar FP32 |
| **Full-width TACC** | Persistent widened lane accumulation with explicit ownership | Medium | ✅ Emulator and functional RTL; ☐ routed FPGA acceptance |
| **Security / Integrity** | AES-256-GCM, SHA-3/SHAKE/raw Keccak, checked WOTS chain, 32/64-bit CRC tuples | Large | ✅ Checkpoint-3 WOTS path qualified across execution models, integrated RTL, and BIOS; FPGA routing remains a separate acceptance gate |
| **Data Movement / QoS** | HW tile DMA, descriptor rings, prefetch, per-core QoS | Medium | Weighted arbiter implemented; architectural QoS programming is not integrated; DMA remains design-only |
| **Reliability / BIST** | Memory self-test, tile datapath check, perf counters | Small | ✅ Implemented |

All new features are **backward-compatible** — existing code runs
unchanged. New instructions use the existing MEX (0xE_) encoding space or an
EXT prefix, and most new CSRs are in the 0x40–0x7F range.  TACC uses the
adjacent tile CSRs `0x1D` and `0x1E`.  Crypto accelerators are MMIO-mapped
peripherals.

---

## 2. Enhanced Tile Engine

### 2.1 TMUL / MAC Family

The existing tile engine has two TMUL functions: lane-wise MUL (funct 0)
and DOT product (funct 1).  The original extension adds widening MUL,
destination-tile MAC/FMA, and DOTACC.  The later full-TACC update assigns
function 6 to persistent widened accumulation.

| Funct | Mnemonic | Operation | Result |
|-------|----------|-----------|--------|
| 0 | MUL (existing) | `dst[i] = a[i] × b[i]` | `[TDST]` |
| 1 | DOT (existing) | $\sum_i a_i \times b_i$ | ACC |
| 2 | **WMUL** | `dst[2i:2i+1] = a[i] × b[i]` (widening) | `[TDST]` (double-width) |
| 3 | **MAC** | `dst[i] += a[i] × b[i]` (in-place accumulate) | `[TDST]` |
| 4 | **FMA** | `dst[i] = a[i] × b[i] + c[i]` | `[TDST]` (c = TDST) |
| 5 | **DOTACC** | `ACC[k] += dot(a_chunk_k, b_chunk_k)` for k=0..3 | ACC0–ACC3 |
| 6 | **TAMAC** | `TACC[i] += widen(a[i] × b[i])` | TACC |
| 7 | reserved | Illegal operation | — |

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

### 2.6 Full-Width Persistent TACC

The full-TACC update adds one 2,048-bit accumulator to each physical tile
engine without widening the ordinary tile lane or memory datapath.  There are
seven independent domains: one private engine for each of full cores 0–3 and
one shared engine for each four-microcore cluster.  Microcores retain private
configuration shadows, while legacy ACC, TACC, and lifecycle metadata belong
to their shared engine.

TACC is intentionally explicit software-visible state.  Software performs
`TRY → CLEAR/LOAD → TAMAC... → STORE → RELEASE`; hardware does not infer a
lifetime, block inside a claim, evict an owner, or spill state.  A losing
`TACC.TRY` retires normally, and software reads caller-relative
`TACC_STATUS.MINE` to choose its own retry or backoff policy.  Ownership does
not reserve the engine from nonowner stateless or legacy-ACC MEX work.

`TAMAC` supports tile×tile (`E1 06`), register broadcast (`E5 06 Rn`), and
in-place (`ED 06`) forms.  Its integer formats widen U8/S8 products into 32-bit
lanes and U16/S16 or U32/S32 products into 64-bit lanes.  FP16 and BF16
products accumulate in binary32 with one round-to-nearest-even feedback
addition per lane.  U64 and EW 6–7 remain unsupported.

Lifecycle operations use `F8 E3 02` through `F8 E3 06` for `TRY`, `CLEAR`,
`LOAD`, `STORE`, and `RELEASE`.  `TACC_STATUS` at CSR `0x1D` exposes claimed,
mine, valid, dirty, busy, latched format, force-pending, and absolute owner.
Supervisor-only `TACC_CTL.FORCE_RELEASE` at `0x1E` is the explicit dead-owner
recovery path.

The canonical image is always 256 bytes aligned to 64 bytes and moves as four
serialized 64-byte beats.  U8/S8 and U16/S16 occupy all 256 bytes; U32/S32,
FP16, and BF16 occupy the low 128 bytes and normalize the high half to zero.
External images further serialize into 32 64-bit PHY words.  A response at
cycle 255 wins; no response or a later response faults at the exact word
address.  LOAD publishes atomically, while a faulting external STORE may leave
only its acknowledged prefix visible.  The microcluster scratchpad aperture
is not a TACC image or operand route and faults before traffic even when
ordinary scalar scratchpad access is enabled.

Uncontended emulator Phase-1 image timing is:

| Path | Instruction step | Strict registered system |
|---|---:|---:|
| Internal/attached image | 6 cycles | 9 cycles |
| External, one-cycle PHY words | 34 cycles | 37 cycles |
| External, two-cycle PHY words | 66 cycles | 69 cycles |

The strict external default records 31 stalls and 32 successful external
words.  Contention and longer responses add elapsed stall cycles; microcore
MEX also pays the existing fixed three-cycle post-grant cluster cost.

Interrupts and traps preserve ownership.  Migration requires saving dirty
state and its format as needed, then releasing.  The architectural reset
contract wipes only the defined engine domain; individual microcore reset
cancels that caller without wiping shared cluster TACC.  RTL verifies the
independent scopes through named seams that remain tied inactive until a
production reset controller is specified.  The complete normative contract
is in `docs/isa-reference.md` and the programming guide in
`docs/tile-engine.md`.

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

### 4.1 AES-256/128-GCM

The normative executable/native software ABI provides AES-256 and AES-128
encryption/decryption in GCM (Galois/Counter Mode). The key size is selected
via `AES_KEY_MODE`. The register image below describes that ABI; the current
integrated RTL mismatch is recorded after the table.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `AES_KEY[0..31]` | 0x700..0x71F | W | 32 key bytes; all 32 must be written in either mode |
| `AES_IV[0..11]` | 0x720..0x72B | W | 12-byte IV/nonce |
| `AES_AAD_LEN` | 0x730..0x733 | W32 LE | Additional-authenticated-data length in bytes |
| `AES_DATA_LEN` | 0x734..0x737 | W32 LE | Plaintext/ciphertext length in bytes |
| `AES_CMD` | 0x738 | W8 | Start: low bit 0 = encrypt, 1 = decrypt |
| `AES_STATUS` | 0x739 | R8 | 0 = idle, 1 = active, 2 = done, 3 = authentication or transaction failure |
| `AES_KEY_MODE` | 0x73A | W8 | Low bit 0 = AES-256 (default), 1 = AES-128 |
| `AES_DIN[0..15]` | 0x740..0x74F | W | Ordered 16-byte input window |
| `AES_DOUT[0..15]` | 0x750..0x75F | R | 16-byte output window |
| `AES_TAG[0..15]` | 0x760..0x76F | R/W | 16-byte GCM authentication tag |

Naturally aligned 1-, 2-, 4-, and 8-byte native accesses are admitted when the
whole access remains in `0x700..0x76F` and are decomposed little-endian into
byte callbacks. The BIOS transfers key, IV, input, output, and tag byte-by-byte
and uses 32-bit length stores. Software writes the complete configuration, writes
the expected tag before a decrypt command, then feeds AAD blocks followed by
data blocks through `AES_DIN`. `AES_STATUS` reaches 2 only after final GCM tag
generation or successful comparison; malformed transactions and tag mismatch
reach 3.

> **Integrated RTL discrepancy.** The current SoC selects
> `0x700..0x77F`, supplies no access-size signal to `mp64_aes`, and the leaf
> recognizes mostly 32-bit register starts plus isolated command, status, and
> key-mode byte offsets rather than the executable callback at every byte. It
> also publishes a busy/done/auth-fail bitfield and lacks the executable
> AAD/length-finalization, tag-comparison, and fail-closed transaction behavior.
> It therefore does not currently implement the table above for unchanged BIOS
> software. This is an unresolved implementation mismatch, not an alternative
> normative ABI.

**Historical, unqualified RTL performance target**: 1 block (16 bytes) per
11 cycles (pipelined AES-256 round function, 9 cycles for AES-128) plus 1 cycle
GHASH. Neither the executable native value model nor the hosted simulator
claims this latency, and the current RTL has not qualified the target against
the ABI above.

**AES-128 mode**: Write 1 to `AES_KEY_MODE` before loading the key.
The executable/native mode uses a 10-round key schedule instead of 14. Only the
first 16 key bytes affect AES-128, but the current executable native transaction
check still requires software to write all 32 key bytes; the BIOS does so. Used
by TLS 1.3 cipher suite 0x1301 (TLS_AES_128_GCM_SHA256).

**Unqualified RTL interrupt target**: `IRQX_AES` (vector 12) on block
completion for interrupt-driven streaming. Interrupt behavior is not part of
the current executable/native or hosted AES acceptance claim.

### 4.2 SHA-3 / SHAKE / raw Keccak accelerator

The shared front end occupies exactly `0x780..0x7DF` and supports
SHA3-256, SHA3-512, SHAKE128, SHAKE256, and caller-owned raw
Keccak-f[1600]. Its sponge and raw paths and the checkpoint-3 WOTS sequencer
share one physical 24-round service; no feature gate duplicates that core.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `SHA_CMD` | 0x780 | W8 | 1=INIT, 3=FINAL, 4=NEXT64, 6=KECCAK-F1600, 7=CLEAR; all other values reject |
| `SHA_STATUS` | 0x781 | R8 | Packed phase in bits 1:0 and owner class in bits 3:2 |
| `SHA_CTRL` | 0x782 | R/W8 | Mode: 0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256 |
| `SHA_ERROR` | 0x783 | R8 | Stable protocol/internal error code |
| `SHA_DIN` | 0x788 | W8 | One streaming absorb byte; full-rate writes apply backpressure |
| `SHA_DOUT[0..63]` | 0x790..0x7CF | R8/R64 | Stable 64-byte output window; qword reads must be aligned |
| `SHA_STATE_INDEX` | 0x7D0 | R/W8 | Raw lane selector, 0 through 24 |
| `SHA_STATE_DATA` | 0x7D8..0x7DF | R/W8/R/W64 | Selected raw 64-bit lane, little endian |

Reserved addresses, wrong directions, unsupported widths, misalignment, and
cross-register or cross-window accesses fault atomically. Software selects a
mode, issues INIT, feeds bytes through DIN, then uses FINAL for the first
fixed/XOF window. SHAKE uses NEXT for subsequent sequential 64-byte windows
and ends with CLEAR. Raw callers load all 25 lanes through STATE_INDEX and
STATE_DATA, issue command 6, read the resulting lanes, and clear. The exact
state machine, access-width rules, status encodings, and zeroization behavior
are defined in the [crypto interface contract](crypto-interface-contract.md).

**Performance target**: Keccak-f[1600] in 24 cycles (1 round/cycle).
Absorption accepts one byte per MMIO write and automatically invokes the
shared round service whenever the selected rate fills.

### 4.3 WOTS chain sequencer

The selected production WOTS interface is the exact byte-only range
`0x8A0..0x8BF`.
It reads one immutable context from Bank 0:
`PK.seed[16] || ADRS[32] || node[16]`.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| `WOTS_CONTEXT_ADDR[0..7]` | 0x8A0..0x8A7 | R/W8 | Little-endian 64-bit physical address |
| `WOTS_STEPS` | 0x8A8 | R/W8 | Complete step count 0..15 |
| `WOTS_START` | 0x8A9 | R/W8 | Complete starting index 0..15 |
| `WOTS_CMD_STATUS` | 0x8AA | W8/R8 | Commands NOP=0, GO=1, CLEAR=2; status IDLE=0, BUSY=1, DONE=2, ERROR=3 |
| `WOTS_ERROR` | 0x8AB | R8 | Stable terminal error code |
| `WOTS_CYCLES[0..3]` | 0x8AC..0x8AF | R8 | Saturating little-endian service-cycle count |
| `WOTS_DOUT[0..15]` | 0x8B0..0x8BF | R8 | Stable terminal node |

Any wider, misaligned, crossing, reserved, or wrong-direction access faults
as one architectural access before mutation. Programming changes only in
IDLE, and terminal status/output remain stable through NOP or rejected writes
until CLEAR. GO validates steps, widened nonzero `START + STEPS <= 15`
geometry, the complete nonwrapping Bank 0 context span, and nonzero-work
Keccak ownership in that order.

WOTS is a real 64-bit read-only main-bus requester appended after disk. It is
fixed at weight 1 with no bandwidth cap, allows one accepted outstanding byte
read, and consumes an explicit OK, target-fault, memory-timeout, or protocol
response. Every successful request reads exactly 64 ascending bytes. Zero
steps returns the input node without claiming Keccak; each nonzero step uses
one raw permutation after inserting `START + step` into ADRS bytes 28..31 in
big-endian form and applying the SHAKE256 delimiter/padding block. CLEAR
withdraws a request before acceptance or drains its one accepted response,
then scrubs private DMA/context/Keccak state before IDLE.

The checked BIOS word is
`WOTS-CHAIN ( context-64 start steps dst-16 -- status )`. It preflights
capability and complete spans, computes bounded waits from read-only
`NUM_BUS_PORTS`, stages the result, clears hardware, and only then writes the
caller destination. Failure writes no destination byte; clear timeout retains
crypto guard 8 fail-closed. Exact errors, deadline formulas, state bytes, and
cleanup ordering are in the
[crypto interface contract](crypto-interface-contract.md#wots-chain-contract).

### 4.4 CRC

CRC is an EXT.CRYPTO ISA facility, not an extended-TPU MMIO block. Full cores
have private state and each micro-core cluster shares an owner-arbitrated
engine. It provides byte and 8-byte feeds, arbitrary width-masked seeds, and
atomic final publication. The exact non-reflected parameter tuples, encodings,
and canonical vectors are specified in the [ISA reference](isa-reference.md#extcrypto--core-crypto-isa-fb).
The 8-byte datapath processes a 512-byte disk sector in 64 feed operations.

### 4.5 SHA-256 (SHA-2) Accelerator

SHA-256 for TLS 1.3 cipher suite 0x1301
(TLS_AES_128_GCM_SHA256) and HMAC/HKDF key derivation is an EXT.CRYPTO ISA
facility, not an MMIO accelerator. The former `0x940` register window is
free. Full cores have private engine state; a micro-cluster shares an
owner-arbitrated engine whose transaction ends only at `sha.release`.

The BIOS exposes checked per-core `SHA256-INIT`, `SHA256-UPDATE`,
`SHA256-FINAL`, and `SHA256-CLEAR` transactions. It stores intermediate
state and a dedicated 64-byte block in a private 256-byte context, validates
complete caller spans and saved length/offset consistency before access,
stages digest output until after release, restores outer
ACC/TSRC0/interrupt state, and wipes every terminal path. See the
[ISA reference](isa-reference.md#extcrypto--core-crypto-isa-fb) and
[BIOS Forth reference](bios-forth.md#sha-256-streaming-4-words).

**Performance target**: 64-byte block in ~64 cycles (1 round/cycle,
64 rounds per block).

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

The bus arbiter uses **weighted round-robin** with packed QoS registers. The
single 64-bit weight CSR exposes ports 0..7, and the single 64-bit bandwidth
CSR exposes ports 0..3; other ports retain their elaborated/reset policy:

| CSR | Address | Description |
|-----|---------|-------------|
| `QOS_WEIGHT` | `0x58` | Eight packed 8-bit requester weights for ports 0..7 (1–255; encoded zero canonicalizes to 1) |
| `QOS_BWLIMIT` | `0x59` | Four packed 16-bit maximum-beat values for ports 0..3 per 65,536-cycle epoch (0 = unlimited) |

Higher weight → more consecutive bus beats before rotation. The WOTS port
is immutable weight 1/unlimited while capability bit 3 is advertised, so
software cannot invalidate its checked deadline by throttling that requester.

Bandwidth accounting resets on the fixed 16-bit epoch wrap (65,536 clocks).
These registers are an arbiter-module sideband today: integrated
`mp64_soc.v` ties `qos_csr_wen` low, and CPU CSR storage at `0x58..0x59` is
not connected to it. Consequently architectural software cannot currently
change the fabric policy; focused arbiter/native test seams can exercise the
packed configuration directly.

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
| 0x200 | 24B | Disk (existing, including read-only capacity) |
| 0x300 | 112B | SysInfo (14 × 64-bit regs, including `CRYPTO_CAPS` and `NUM_BUS_PORTS`) |
| 0x400 | 128B | NIC (existing) |
| 0x500 | 16B | Mailbox (existing) |
| 0x600 | 64B | Spinlocks (existing) |
| **0x700** | **112B** | **AES-256/128-GCM** |
| **0x780** | **96B** | **SHA-3/SHAKE** |
| **0x7E0** | **16B** | **Reserved; no integrated QoS MMIO device** |
| **0x800** | **32B** | **TRNG** |
| **0x840** | **128B** | *(free; Field ALU is EXT.CRYPTO)* |
| **0x8A0** | **32B** | **Checked byte-only WOTS chain sequencer** |
| **0x8C0** | **64B** | **NTT Engine** |
| **0x900** | **64B** | **KEM (ML-KEM-512)** |
| **0x940** | **32B** | *(free; SHA-2 is EXT.CRYPTO)* |
| **0x980** | **32B** | *(free; CRC is EXT.CRYPTO)* |
| **0xA00** | **64B** | **Framebuffer** |
| **0xB00** | **32B** | **RTC / System Clock** |

The qualified checkpoint-3 System Info value is `CRYPTO_CAPS = 0xF`. Its
`NUM_BUS_PORTS` value is the exact full-core plus microcluster port count plus
three appended DMA requesters: NIC, disk, and WOTS. WOTS is appended after
disk, preserving both earlier physical indices. Bit 3 was held clear until the
production controller, real DMA, shared service, and checked BIOS path
qualified together.

### CSR Address Map (Extended)

New per-core CSRs:

| Range | Family |
|-------|--------|
| 0x00–0x09 | CPU core (existing) |
| 0x10–0x1C | Tile engine legacy configuration and ACC |
| **0x1D–0x1E** | **TACC status and supervisor recovery control** |
| 0x20–0x25 | Multicore (existing) |
| 0x30–0x31 | System info (existing) |
| **0x40–0x43** | **Strided/2D tile addressing** |
| **0x50–0x59** | **DMA + QoS** |
| **0x60–0x6C** | **BIST + perf counters** |
| **0x70–0x72** | **Instruction cache (ctrl, hits, misses)** |

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

TMUL funct 2–5 (WMUL, MAC, FMA, DOTACC), TMUL function 6 (TAMAC), and
TRED funct 5–7 (SUMSQ, MINIDX, MAXIDX) fit within the existing 3-bit
function field.  TMUL function 7 remains reserved.

### Extended Tile Ops (FAM_EXT = 0xF)

```
0xF0: EXTALU SS=0 (tile×tile)    — VSHR, VSHL, VSEL, VCLZ, ...
0xF4: EXTALU SS=1 (broadcast)
0xF8: EXTALU SS=2 (imm8)
0xFC: EXTALU SS=3 (in-place)
0xF1: EXTSYS (LOAD2D, STORE2D, PREFETCH, FENCE)
```

Canonical TACC lifecycle instructions are three bytes:

```
F8 E3 02  TACC.TRY
F8 E3 03  TACC.CLEAR
F8 E3 04  TACC.LOAD
F8 E3 05  TACC.STORE
F8 E3 06  TACC.RELEASE
```

---

## 9. Historical Implementation Priority

The table below is the original extended-TPU planning record, not the current
implementation status or an active delivery plan.  The status table in
Section 1 and the full-TACC handoff are authoritative.

Recommended build order based on dependencies and complexity:

| Phase | Feature | Effort | Depends On |
|-------|---------|--------|------------|
| **A** | Performance counters (§6.3) | 1 day | None |
| **A** | Enhanced reductions (§2.4) | 2 days | None |
| **A** | TMUL/MAC family (§2.1) | 2 days | None |
| **B** | Saturating arith + rounding (§2.2) | 2 days | TMODE extension |
| **B** | Tile views / SHUFFLE (§2.3) | 3 days | None |
| **B** | CRC32/CRC64 (§4.4) | 2 days | None |
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
3. **Emulator parity**: matching implementation in `emulator/megapad64.py` so
   software can be developed in parallel with RTL
4. **BIOS Forth words**: thin wrappers (TSHUFFLE, TPACK, TAES-ENC, etc.)
   added to the BIOS dictionary as each feature lands

---

## 11. Open Questions

1. **FP16 accumulator width — resolved**: Legacy DOT/SUM and full TACC use
   binary32 accumulation for FP16/BF16 inputs.  TACC applies one
   round-to-nearest-even binary32 feedback addition per active lane.

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
