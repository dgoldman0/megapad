# Megapad-64 Instruction Set Reference

The Megapad-64 is a **64-bit, little-endian** processor with 16
general-purpose registers, a rich flag set, and a built-in SIMD tile
engine.  Its heritage traces to the RCA CDP1802, but it extends that
architecture enormously with 64-bit registers, hardware multiply/divide,
a full condition-code set, a 256-bit accumulator, and 64-byte tile
operations.  The tile engine includes FP16/BF16 support, saturating
arithmetic, rounding shifts, strided 2D addressing, and a full suite of
SIMD operations across 8 to 64 lanes.

This document is the **authoritative ISA reference** — every instruction,
every register, every encoding.

---

## CPU Overview

| Property | Value |
|----------|-------|
| Registers | 16 × 64-bit GPRs (R0–R15) |
| Word size | 64-bit |
| Endianness | Little-endian |
| PC | `R[PSEL]` — default PSEL=3, so PC = R3 |
| Data pointer | `R[XSEL]` — default XSEL=2, so R(X) = R2 |
| Stack pointer | `R[SPSEL]` — default SPSEL=15, so SP = R15 |
| D accumulator | 8-bit legacy accumulator (1802 heritage) |
| Q flip-flop | 1-bit output latch |
| T register | 8-bit saved `XSEL‖PSEL` (for MARK/RET) |
| Flags | 8-bit packed `[S I G P V N C Z]` |
| Tile engine | 64-byte tiles, 256-bit accumulator, SIMD lanes, FP16/BF16 |

The indirection through `PSEL`, `XSEL`, and `SPSEL` means any GPR can
serve as the program counter, data pointer, or stack pointer.  In practice,
the BIOS sets PSEL=3, XSEL=2, SPSEL=15 and never changes them.

---

## Flags Register

The flags register is 8 bits, packed from bit 7 (MSB) to bit 0 (LSB):

```
Bit:   7    6    5    4    3    2    1    0
     ┌────┬────┬────┬────┬────┬────┬────┬────┐
     │ S  │ I  │ G  │ P  │ V  │ N  │ C  │ Z  │
     └────┴────┴────┴────┴────┴────┴────┴────┘
```

| Bit | Flag | Meaning | Set By |
|-----|------|---------|--------|
| 0 | **Z** (Zero) | Result is zero | Most ALU/IMM ops |
| 1 | **C** (Carry) | Carry out / borrow / DF | ADD, SUB, shift, 1802 ops |
| 2 | **N** (Negative) | Bit 63 of result is set | Most ALU/IMM ops |
| 3 | **V** (oVerflow) | Signed overflow occurred | ADD, SUB |
| 4 | **P** (Parity) | Even parity of low 8 bits | Most ALU/IMM ops |
| 5 | **G** (Greater) | Unsigned greater (set by CMP) | CMP, CMPI |
| 6 | **I** (Interrupt) | Interrupts enabled globally | EI, DI, RTI |
| 7 | **S** (Saturation) | Sticky saturation indicator | Tile ops |

---

## Instruction Encoding

Instructions are **variable-length** (1 to 11 bytes).  The first byte
encodes the **family** in the upper nibble and the **sub-opcode or register
selector** in the lower nibble:

```
Byte 0:  [F:4][N:4]
         F = instruction family (0x0–0xF)
         N = sub-opcode, register, or condition code
```

Many two-register instructions pack both register indices into byte 1:

```
Byte 1:  [Rd:4][Rs:4]
         Rd = destination register (upper nibble)
         Rs = source register (lower nibble)
```

### Instruction Size Summary

| Family | Name | Typical Size | Notes |
|--------|------|-------------|-------|
| `0x0` | SYS | 1 byte | CALL.L = 2 bytes |
| `0x1` | INC | 1 byte | Register in low nibble |
| `0x2` | DEC | 1 byte | Register in low nibble |
| `0x3` | BR | 2 bytes | + 1 signed offset byte |
| `0x4` | LBR | 3 bytes | + 2 signed offset bytes |
| `0x5` | MEM | 2 bytes | LD.D = 3 bytes (has offset) |
| `0x6` | IMM | 2–4 bytes | LDI w/ EXT.IMM64 = 10 bytes |
| `0x7` | ALU | 2 bytes | |
| `0x8` | MEMALU | 1 byte | 1802-compatible D ops |
| `0x9` | I/O | 1 byte | 1802-style port I/O |
| `0xA` | SEP | 1 byte | Set PC register |
| `0xB` | SEX | 1 byte | Set data pointer register |
| `0xC` | MULDIV | 2 bytes | +3 extra cycles |
| `0xD` | CSR | 2 bytes | CSR read/write |
| `0xE` | MEX | 2–3 bytes | Tile engine ops |
| `0xF` | EXT | 1 byte | Prefix modifier for next insn |

> **Micro-core restrictions:** Families 0x8, 0x9, 0xA, 0xB, 0xE, and
> SYS sub-ops 0x5–0xA (RET/DIS/MARK/SAV/SEQ/REQ) plus IMM sub-ops
> 0xC–0xF (GLO/GHI/PLO/PHI) are **not available** on micro-cores.
> They trap as `ILLEGAL_OP`.  See [architecture.md](architecture.md)
> for the full stripped-feature list.

---

## Family 0x0 — SYS (System)

Single-byte system operations (except CALL.L which is 2 bytes).

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `00` | **IDL** | 1 | Enter idle state — halt until interrupt or DMA. |
| `01` | **NOP** | 1 | No operation. |
| `02` | **HALT** | 1 | Stop the CPU permanently. |
| `03` | **RESET** | 1 | Full CPU state reset (all registers zeroed, PSEL=3, etc.). |
| `04` | **RTI** | 2 | Return from interrupt: pop PC, pop FLAGS+PRIV (restores IE and privilege level). |
| `05` | **RET** | 2 | 1802-style return: pop T byte, restore XSEL and PSEL, enable interrupts.  **Supervisor-only.** |
| `06` | **DIS** | 2 | Same as RET but disables interrupts (IE ← 0).  **Supervisor-only.** |
| `07` | **MARK** | 2 | Save XSEL‖PSEL into T; push T; set XSEL ← PSEL.  **Supervisor-only.** |
| `08` | **SAV** | 1 | Store T register to memory at R(X): `M(R(X)) ← T`.  **Supervisor-only.** |
| `09` | **SEQ** | 1 | Set Q flip-flop to 1.  **Supervisor-only.** |
| `0A` | **REQ** | 1 | Reset Q flip-flop to 0.  **Supervisor-only.** |
| `0B` | **EI** | 1 | Enable interrupts (I flag ← 1). |
| `0C` | **DI** | 1 | Disable interrupts (I flag ← 0). |
| `0D nn` | **CALL.L Rn** | 2 | Long call: push PC; PC ← R[n].  2 bytes: opcode + register byte. |
| `0E` | **RET.L** | 2 | Long return: PC ← pop from stack. |
| `0F` | **TRAP** | 3 | Software trap — saves FLAGS+PRIV, escalates to supervisor, enters `IVEC_SW_TRAP` handler. |

---

## Family 0x1 — INC (Increment Register)

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `1n` | **INC Rn** | 1 | `R[n] ← R[n] + 1` — increment any register by 1. |

The register index is encoded in the low nibble.  `INC R5` = `0x15`.

---

## Family 0x2 — DEC (Decrement Register)

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `2n` | **DEC Rn** | 1 | `R[n] ← R[n] − 1` — decrement any register by 1. |

---

## Family 0x3 — BR (Short Branch)

Two-byte instructions: opcode + 8-bit **signed** relative offset.
Branch range is −128 to +127 bytes from the instruction following the
offset byte.

| Encoding | Mnemonic | Condition |
|----------|----------|-----------|
| `30 off` | **BR off** | Always (unconditional) |
| `31 off` | **BR.EQ off** | Z = 1 (equal / zero) |
| `32 off` | **BR.NE off** | Z = 0 (not equal) |
| `33 off` | **BR.CS off** | C = 1 (carry set) |
| `34 off` | **BR.CC off** | C = 0 (carry clear) |
| `35 off` | **BR.MI off** | N = 1 (negative) |
| `36 off` | **BR.PL off** | N = 0 (positive or zero) |
| `37 off` | **BR.VS off** | V = 1 (overflow) |
| `38 off` | **BR.VC off** | V = 0 (no overflow) |
| `39 off` | **BR.GT off** | G = 1 (unsigned greater) |
| `3A off` | **BR.LE off** | G = 0 (not unsigned greater) |
| `3B off` | **BR.BQ off** | Q = 1 |
| `3C off` | **BR.BNQ off** | Q = 0 |
| `3D off` | **BR.SAT off** | S = 1 (saturation) |
| `3E off` | **BR.EF off** | Any external flag ≠ 0 |
| `3F off` | **BR.NV off** | Never (useful as 2-byte NOP) |

---

## Family 0x4 — LBR (Long Branch)

Three-byte instructions: opcode + 16-bit **signed** relative offset
(little-endian).  Range: −32,768 to +32,767 bytes.

Same condition codes as BR, just with a larger range:

| Encoding | Mnemonic | Condition |
|----------|----------|-----------|
| `40 lo hi` | **LBR off16** | Always |
| `41 lo hi` | **LBR.EQ off16** | Z = 1 |
| `42 lo hi` | **LBR.NE off16** | Z = 0 |
| ... | ... | (same pattern as BR) |

---

## Family 0x5 — MEM (Scalar Load/Store)

Two-byte instructions (except LD.D which is 3 bytes).  Byte 1 encodes
`[Rd:4][Rs:4]`.

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `50 DR` | **LDN Rd, Rs** | 1 | Load 64-bit: `Rd ← mem64(R[s])` |
| `51 DR` | **LDA Rd, Rs** | 1 | Load with auto-increment: `Rd ← mem64(R[s]); R[s] += 8` |
| `52 Dx` | **LDXR Rd** | 1 | Load from data pointer: `Rd ← mem64(R(X))` |
| `53 Dx` | **LDXAR Rd** | 1 | Load + auto-inc: `Rd ← mem64(R(X)); R(X) += 8` |
| `54 NR` | **STR Rn, Rs** | 1 | Store 64-bit: `mem64(R[n]) ← R[s]` |
| `55 Sx` | **STXD Rs** | 1 | Store + auto-dec: `mem64(R(X)) ← R[s]; R(X) -= 8` |
| `56 DR` | **LD.B Rd, Rs** | 1 | Load byte (zero-extended): `Rd ← zext(mem8(R[s]))` |
| `57 NR` | **ST.B Rn, Rs** | 1 | Store byte: `mem8(R[n]) ← R[s][7:0]` |
| `58 DR` | **LD.H Rd, Rs** | 1 | Load 16-bit halfword (zero-extended) |
| `59 NR` | **ST.H Rn, Rs** | 1 | Store 16-bit halfword |
| `5A DR` | **LD.W Rd, Rs** | 1 | Load 32-bit word (zero-extended) |
| `5B NR` | **ST.W Rn, Rs** | 1 | Store 32-bit word |
| `5C DR` | **LD.SB Rd, Rs** | 1 | Load byte (sign-extended to 64 bits) |
| `5D DR` | **LD.SH Rd, Rs** | 1 | Load 16-bit (sign-extended) |
| `5E DR` | **LD.SW Rd, Rs** | 1 | Load 32-bit (sign-extended) |
| `5F DR off` | **LD.D Rd, [Rs+off8]** | 2 | Displacement load: `Rd ← mem64(R[s] + sext(off8) × 8)`.  3 bytes. |

---

## Family 0x6 — IMM (Immediate Operations)

Variable-length instructions with embedded constants.  Byte 1 is
`[Rn:4][x:4]` where the lower nibble varies by sub-opcode.

| Opcode | Mnemonic | Size | Cycles | Description |
|--------|----------|------|--------|-------------|
| `60 Rx imm8` | **LDI Rn, imm8** | 3 | 1 | `Rn ← zext(imm8)` |
| `F0 60 Rx 8B` | **LDI Rn, imm64** | 10 | 2 | `Rn ← imm64` (with EXT.IMM64 prefix; 8 LE bytes) |
| `61 Rx hi lo` | **LHI Rn, imm16** | 4 | 1 | Load upper: `Rn[63:48] ← imm16`, lower 48 preserved |
| `62 Rx simm8` | **ADDI Rn, simm8** | 3 | 1 | `Rn ← Rn + sext(simm8)`.  Updates ZCNVP. |
| `63 Rx imm8` | **ANDI Rn, imm8** | 3 | 1 | `Rn ← Rn & imm8`.  Updates ZNP, clears CV. |
| `64 Rx imm8` | **ORI Rn, imm8** | 3 | 1 | `Rn ← Rn \| imm8` |
| `65 Rx imm8` | **XORI Rn, imm8** | 3 | 1 | `Rn ← Rn ^ imm8` |
| `66 Rx simm8` | **CMPI Rn, simm8** | 3 | 1 | Compare: flags ← `Rn − sext(simm8)`.  Updates ZCNVPG.  Result discarded. |
| `67 Rx simm8` | **SUBI Rn, simm8** | 3 | 1 | `Rn ← Rn − sext(simm8)`.  Updates ZCNVP. |
| `68 Ri` | **LSLI Rn, imm4** | 2 | 1 | `Rn ← Rn << imm4` (shift amount in low nibble of byte 1) |
| `69 Ri` | **LSRI Rn, imm4** | 2 | 1 | `Rn ← Rn >>> imm4` (logical right shift) |
| `6A Ri` | **ASRI Rn, imm4** | 2 | 1 | `Rn ← Rn >> imm4` (arithmetic right shift) |
| `6B Ri` | **ROLI Rn, imm4** | 2 | 1 | `Rn ← rotate_left(Rn, imm4)` |
| `6C Rx` | **GLO Rn** | 2 | 1 | `D ← R[n][7:0]` — get low byte into D accumulator.  **Supervisor-only.** |
| `6D Rx` | **GHI Rn** | 2 | 1 | `D ← R[n][15:8]` — get high byte into D.  **Supervisor-only.** |
| `6E Rx` | **PLO Rn** | 2 | 1 | `R[n][7:0] ← D` — put D into low byte.  **Supervisor-only.** |
| `6F Rx` | **PHI Rn** | 2 | 1 | `R[n][15:8] ← D` — put D into high byte.  **Supervisor-only.** |

---

## Family 0x7 — ALU (Register-Register)

Two-byte instructions: opcode + `[Rd:4][Rs:4]`.  Result always goes
into Rd.

| Opcode | Mnemonic | Cycles | Flags | Description |
|--------|----------|--------|-------|-------------|
| `70 DR` | **ADD Rd, Rs** | 1 | ZCNVP | `Rd ← Rd + Rs` |
| `71 DR` | **ADC Rd, Rs** | 1 | ZCNVP | `Rd ← Rd + Rs + C` (add with carry) |
| `72 DR` | **SUB Rd, Rs** | 1 | ZCNVP | `Rd ← Rd − Rs` |
| `73 DR` | **SBB Rd, Rs** | 1 | ZCNVP | `Rd ← Rd − Rs − !C` (subtract with borrow) |
| `74 DR` | **AND Rd, Rs** | 1 | ZNP | `Rd ← Rd & Rs` (clears C, V) |
| `75 DR` | **OR Rd, Rs** | 1 | ZNP | `Rd ← Rd \| Rs` |
| `76 DR` | **XOR Rd, Rs** | 1 | ZNP | `Rd ← Rd ^ Rs` |
| `77 DR` | **CMP Rd, Rs** | 1 | ZCNVPG | Compare: flags ← Rd − Rs.  Result discarded.  Also sets G flag. |
| `78 DR` | **MOV Rd, Rs** | 1 | — | `Rd ← Rs` (no flag changes) |
| `79 DR` | **NOT Rd, Rs** | 1 | ZNP | `Rd ← ~Rs` (bitwise complement) |
| `7A DR` | **NEG Rd, Rs** | 1 | ZCNVP | `Rd ← 0 − Rs` (two's complement negate) |
| `7B DR` | **SHL Rd, Rs** | 1 | ZCNP | `Rd ← Rd << (Rs & 63)`.  C = last bit shifted out. |
| `7C DR` | **SHR Rd, Rs** | 1 | ZCNP | `Rd ← Rd >>> (Rs & 63)` (logical shift right) |
| `7D DR` | **SAR Rd, Rs** | 1 | ZCNP | `Rd ← Rd >> (Rs & 63)` (arithmetic shift right, sign-extends) |
| `7E DR` | **ROL Rd, Rs** | 1 | ZNP | `Rd ← rotate_left(Rd, Rs & 63)` |
| `7F DR` | **ROR Rd, Rs** | 1 | ZNP | `Rd ← rotate_right(Rd, Rs & 63)` |

---

## Family 0x8 — MEMALU (1802-Compatible D-Register Operations)

Single-byte instructions that operate on the **8-bit D accumulator** and
memory at `M(R(X))`.  These provide backward compatibility with the
CDP1802 instruction set.

> **Privilege:** All MEMALU instructions are **supervisor-only**.  Executing
> any opcode in this family from user mode triggers `IVEC_PRIV_FAULT`.

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `80` | **LDX** | 1 | `D ← M(R(X))` |
| `81` | **OR.X** | 1 | `D ← M(R(X)) \| D` |
| `82` | **AND.X** | 1 | `D ← M(R(X)) & D` |
| `83` | **XOR.X** | 1 | `D ← M(R(X)) ^ D` |
| `84` | **ADD.X** | 1 | `D ← M(R(X)) + D; C ← carry` |
| `85` | **SD.X** | 1 | `D ← M(R(X)) − D; C ← 1 if no borrow` |
| `86` | **SHR.D** | 1 | `C ← D[0]; D ← D >> 1` |
| `87` | **SM.X** | 1 | `D ← D − M(R(X)); C ← 1 if no borrow` |
| `88` | **ADC.X** | 1 | `D ← M(R(X)) + D + C; C ← carry` |
| `89` | **SDB.X** | 1 | `D ← M(R(X)) − D − !C; C ← 1 if no borrow` |
| `8A` | **SHRC.D** | 1 | Shift right through carry: `old_C → D[7]; D[0] → C` |
| `8B` | **SMB.X** | 1 | `D ← D − M(R(X)) − !C; C ← 1 if no borrow` |
| `8C` | **SHL.D** | 1 | `C ← D[7]; D ← D << 1` |
| `8D` | **SHLC.D** | 1 | Shift left through carry: `old_C → D[0]; D[7] → C` |
| `8E` | **IRX** | 1 | `R(X) ← R(X) + 1` (increment data pointer) |
| `8F` | **LDXA** | 1 | `D ← M(R(X)); R(X) ← R(X) + 1` (load and advance) |

---

## Family 0x9 — I/O (Port Input/Output)

Single-byte 1802-style port I/O.  7 output ports and 7 input ports,
each 4 bits wide.

> **Privilege:** All I/O instructions are **supervisor-only**.  Executing
> any opcode in this family from user mode triggers `IVEC_PRIV_FAULT`.

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `91`–`97` | **OUT 1–7** | 1 | `port_out[n] ← M(R(X)); R(X) += 1` |
| `99`–`9F` | **INP 1–7** | 1 | `D ← port_in[n−8]; M(R(X)) ← D` |

---

## Family 0xA — SEP (Set Program Counter)

> **Privilege:** SEP is **supervisor-only**.  Triggers `IVEC_PRIV_FAULT` in user mode.

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `An` | **SEP Rn** | 1 | `PSEL ← n` — PC is now R[n].  Used for 1802-style subroutine dispatch. |

---

## Family 0xB — SEX (Set Data Pointer)

> **Privilege:** SEX is **supervisor-only**.  Triggers `IVEC_PRIV_FAULT` in user mode.

| Opcode | Mnemonic | Cycles | Description |
|--------|----------|--------|-------------|
| `Bn` | **SEX Rn** | 1 | `XSEL ← n` — R(X) is now R[n]. |

---

## Family 0xC — MULDIV (Multiply / Divide)

Two-byte instructions: opcode + `[Rd:4][Rs:4]`.  All take **4 cycles**.
Division stores the remainder in **R0**.  Division by zero triggers
`IVEC_DIV_ZERO`.

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| `C0 DR` | **MUL Rd, Rs** | Signed multiply: `Rd ← low64(signed(Rd) × signed(Rs))` |
| `C1 DR` | **MULH Rd, Rs** | Signed multiply high: `Rd ← high64(signed(Rd) × signed(Rs))` |
| `C2 DR` | **UMUL Rd, Rs** | Unsigned multiply: `Rd ← low64(Rd × Rs)` |
| `C3 DR` | **UMULH Rd, Rs** | Unsigned multiply high: `Rd ← high64(Rd × Rs)` |
| `C4 DR` | **DIV Rd, Rs** | Signed divide: `Rd ← Rd / Rs; R0 ← remainder` |
| `C5 DR` | **UDIV Rd, Rs** | Unsigned divide: `Rd ← Rd / Rs; R0 ← Rd % Rs` |
| `C6 DR` | **MOD Rd, Rs** | Signed modulo: `Rd ← Rd mod Rs` |
| `C7 DR` | **UMOD Rd, Rs** | Unsigned modulo: `Rd ← Rd mod Rs` |

---

## Family 0xD — CSR (Control/Status Register Access)

Two-byte instructions: opcode + CSR address byte.  The opcode nibble
encodes `[W:1][reg:3]`: W=0 for read, W=1 for write; reg is the GPR
index (0–7).

| Pattern | Mnemonic | Description |
|---------|----------|-------------|
| `D0`–`D7 addr` | **CSRR Rn, CSR[addr]** | Read: `Rn ← CSR[addr]` |
| `D8`–`DF addr` | **CSRW CSR[addr], Rn** | Write: `CSR[addr] ← Rn` |

See the **CSR Register Map** section below for all CSR addresses.

---

## Family 0xE — MEX (Matrix/Element Extension)

The tile engine instruction family.  See the dedicated document
`docs/tile-engine.md` for a full programming guide.

Encoding: `En funct` (2 bytes), or `En funct reg` (3 bytes for broadcast
mode).

The low nibble `n` decodes as `[SS:2][OP:2]`:

- **SS** (bits 3:2) — source selector:
  - 0 = tile × tile (TSRC0 × TSRC1 → TDST)
  - 1 = tile × broadcast register (+1 byte selects Rn)
  - 2 = immediate splat (funct byte IS the immediate, forced to ADD)
  - 3 = in-place (TDST × TSRC0 → TDST)

- **OP** (bits 1:0) — major operation:
  - 0 = TALU (lane-parallel arithmetic)
  - 1 = TMUL (lane-parallel multiply / dot product)
  - 2 = TRED (reduction → accumulator)
  - 3 = TSYS (tile system operations)

### TALU Sub-Functions (OP=0)

| Funct | Name | Semantics |
|-------|------|-----------|
| 0 | **ADD** | `dst[i] = srcA[i] + srcB[i]` (saturating if TMODE bit 5 set) |
| 1 | **SUB** | `dst[i] = srcA[i] − srcB[i]` (saturating if TMODE bit 5 set) |
| 2 | **AND** | `dst[i] = srcA[i] & srcB[i]` |
| 3 | **OR** | `dst[i] = srcA[i] \| srcB[i]` |
| 4 | **XOR** | `dst[i] = srcA[i] ^ srcB[i]` |
| 5 | **MIN** | `dst[i] = min(srcA[i], srcB[i])` |
| 6 | **MAX** | `dst[i] = max(srcA[i], srcB[i])` |
| 7 | **ABS** | `dst[i] = abs(srcA[i])` (signed mode) |

### TMUL Sub-Functions (OP=1)

| Funct | Name | Cycles | Semantics |
|-------|------|--------|-----------|
| 0 | **MUL** | 2 | `dst[i] = srcA[i] × srcB[i]` (truncated) |
| 1 | **DOT** | 4 | `ACC += Σ(srcA[i] × srcB[i])` (dot product) |
| 2 | **WMUL** | 2 | `dst[2i:2i+1] = srcA[i] × srcB[i]` (widening multiply) |
| 3 | **MAC** | 2 | `dst[i] += srcA[i] × srcB[i]` (multiply-accumulate in-place) |
| 4 | **FMA** | 2 | `dst[i] = srcA[i] × srcB[i] + dst[i]` (fused multiply-add) |
| 5 | **DOTACC** | 4 | `ACC[k] += dot(chunk_k)` for k=0..3 (4-way dot product) |

### TRED Sub-Functions (OP=2, result → ACC)

| Funct | Name | Semantics |
|-------|------|-----------|
| 0 | **SUM** | `ACC = Σ src0[i]` |
| 1 | **MIN** | `ACC = min(src0[i])` |
| 2 | **MAX** | `ACC = max(src0[i])` |
| 3 | **POPCNT** | `ACC = Σ popcount(src0[i])` |
| 4 | **L1** | `ACC = Σ |src0[i]|` (L1 norm) |
| 5 | **SUMSQ** | `ACC = Σ src0[i]²` (sum of squares, L2² norm) |
| 6 | **MINIDX** | `ACC0 = argmin index, ACC1 = min value` |
| 7 | **MAXIDX** | `ACC0 = argmax index, ACC1 = max value` |

### TSYS Sub-Functions (OP=3)

| Funct | Name | Cycles | Semantics |
|-------|------|--------|-----------|
| 0 | **TRANS** | 1 | In-place 8×8 byte transpose at TDST |
| 1 | **SHUFFLE** | 3 | Permute: `dst[i] = src0[index_tile[i]]` |
| 2 | **MOVBANK** | 3 | Copy tile: `mem[TDST] ← mem[TSRC0]` |
| 3 | **LOADC** | 1 | Load tile from cursor address |
| 4 | **ZERO** | 1 | Zero 64 bytes at TDST |
| 5 | **PACK** | 2 | Narrow elements: 32→16, 16→8, etc. (saturating if TMODE bit 5) |
| 6 | **UNPACK** | 2 | Widen elements: 8→16, 16→32, etc. (sign-extend if TMODE bit 4) |
| 7 | **RROT** | 2 | Row/column rotate or mirror (controlled by imm8) |

### Extended Tile Operations (via EXT prefix, 0xF_)

When a MEX-class operation is encoded with the EXT prefix family `0xF`
instead of `0xE`, it accesses extended operations:

**Extended TALU** (EXT.8 prefix + TALU):

| Funct | Name | Semantics |
|-------|------|-----------|
| 0 | **VSHR** | `dst[i] = srcA[i] >> srcB[i]` (per-lane right shift; rounds if TMODE bit 6) |
| 1 | **VSHL** | `dst[i] = srcA[i] << srcB[i]` (per-lane left shift) |
| 2 | **VSEL** | `dst[i] = mask[i] ? srcA[i] : srcB[i]` (conditional select) |
| 3 | **VCLZ** | `dst[i] = clz(srcA[i])` (count leading zeros per lane) |

**Extended TSYS** (EXT.8 prefix + TSYS):

| Funct | Name | Semantics |
|-------|------|-----------|
| 0 | **LOAD2D** | Strided gather using TSTRIDE_R/TTILE_H/TTILE_W CSRs |
| 1 | **STORE2D** | Strided scatter using TSTRIDE_R/TTILE_H/TTILE_W CSRs |

---

## Family 0xF — EXT (Prefix Modifier)

Single-byte prefix that modifies the **next** instruction.  The modifier
value is stored and consumed by the following instruction.

| Opcode | Name | Effect |
|--------|------|--------|
| `F0` | **EXT.IMM64** | Next LDI loads a full 64-bit immediate (8 LE bytes) instead of 8-bit. |
| `F6` | **EXT.SKIP** | Next BR becomes a SKIP: if condition is true, skip the following instruction entirely (advance PC past it). |
| `Fn` | **EXT.n** | General modifier *n* stored; consumed by next instruction. |

**Double EXT is illegal** — triggers `IVEC_ILLEGAL_OP`.

### SKIP Pseudo-Instruction

The assembler provides `SKIP.cc` as a convenient mnemonic for
`EXT.SKIP + BR.cc`.  Instead of branching to a target, SKIP conditionally
skips the next instruction:

```asm
CMPI R4, 0         ; compare R4 to 0
SKIP.EQ            ; if equal, skip the next instruction
ADDI R4, 1         ; this runs only if R4 ≠ 0
```

SKIP is 2 bytes: `F6` prefix + `3c` (BR family with condition code).

---

## Condition Codes

Used by BR, LBR, and SKIP families.  The condition code is encoded in the
low nibble of the opcode byte.

| Code | Value | Mnemonic | Condition |
|------|-------|----------|-----------|
| 0x0 | AL | **Always** | Unconditional |
| 0x1 | EQ | **Equal** | Z = 1 |
| 0x2 | NE | **Not Equal** | Z = 0 |
| 0x3 | CS | **Carry Set** | C = 1 |
| 0x4 | CC | **Carry Clear** | C = 0 |
| 0x5 | MI | **Minus** | N = 1 (negative) |
| 0x6 | PL | **Plus** | N = 0 (positive/zero) |
| 0x7 | VS | **Overflow Set** | V = 1 |
| 0x8 | VC | **Overflow Clear** | V = 0 |
| 0x9 | GT | **Greater Than** | G = 1 (unsigned, set by CMP) |
| 0xA | LE | **Less/Equal** | G = 0 |
| 0xB | BQ | **Branch if Q** | Q = 1 |
| 0xC | BNQ | **Branch if Not Q** | Q = 0 |
| 0xD | SAT | **Saturation** | S = 1 |
| 0xE | EF | **External Flags** | Any EF ≠ 0 |
| 0xF | NV | **Never** | Always false (useful as NOP) |

---

## CSR Register Map

| Addr | Name | Width | R/W | Description |
|------|------|-------|-----|-------------|
| `0x00` | **FLAGS** | 8 | RW | Packed flags register (see Flags section) |
| `0x01` | **PSEL** | 4 | RW | Program counter register selector |
| `0x02` | **XSEL** | 4 | RW | Data pointer register selector |
| `0x03` | **SPSEL** | 4 | RW | Stack pointer register selector |
| `0x04` | **IVT_BASE** | 64 | RW | Interrupt vector table base address |
| `0x05` | **D** | 8 | RW | 1802 D accumulator |
| `0x06` | **DF** | 1 | RW | DF flag (alias for Carry) |
| `0x07` | **Q** | 1 | RW | Q flip-flop |
| `0x08` | **T** | 8 | RW | T register (saved XSEL‖PSEL) |
| `0x09` | **IE** | 1 | RW | Interrupt enable (alias for I flag) |
| `0x0A` | **PRIV** | 1 | RW | Privilege level: 0=supervisor, 1=user.  Write is **supervisor-only**. |
| `0x0B` | **MPU_BASE** | 64 | RW | MPU base address (inclusive).  Write is **supervisor-only**. |
| `0x0C` | **MPU_LIMIT** | 64 | RW | MPU limit address (exclusive).  Write is **supervisor-only**. |
| | | | | |
| `0x10` | **SB** | 4 | RW | Tile bank selector |
| `0x11` | **SR** | 20 | RW | Tile row cursor |
| `0x12` | **SC** | 20 | RW | Tile column cursor |
| `0x13` | **SW** | 20 | RW | Tile stride width (default 1) |
| `0x14` | **TMODE** | 8 | RW | Tile element mode (see Tile Engine doc) |
| `0x15` | **TCTRL** | 8 | RW | Tile control register |
| `0x16` | **TSRC0** | 64 | RW | Tile source 0 address |
| `0x17` | **TSRC1** | 64 | RW | Tile source 1 address |
| `0x18` | **TDST** | 64 | RW | Tile destination address |
| `0x19` | **ACC0** | 64 | RW | Accumulator bits 63:0 |
| `0x1A` | **ACC1** | 64 | RW | Accumulator bits 127:64 |
| `0x1B` | **ACC2** | 64 | RW | Accumulator bits 191:128 |
| `0x1C` | **ACC3** | 64 | RW | Accumulator bits 255:192 |
| | | | | |
| `0x20` | **COREID** | 64 | R | Core ID (0..N−1, multicore) |
| `0x21` | **NCORES** | 64 | R | Total number of cores |
| `0x22` | **MBOX** | 64 | RW | Read: pending IPI mask; Write: send IPI to core N |
| `0x23` | **IPIACK** | 64 | W | Acknowledge IPI from core N |
| `0x24` | **IVEC_ID** | 8 | RW | Last interrupt/trap vector ID |
| `0x25` | **TRAP_ADDR** | 64 | R | Faulting address (bus fault) |
| | | | | |
| `0x30` | **MEGAPAD_SZ** | 64 | R | Memory size (returns 0) |
| `0x31` | **CPUID** | 64 | R | CPU ID: `0x4D503634_00010000` ("MP64" v1.0) |
| | | | | |
| `0x40` | **TSTRIDE_R** | 64 | RW | Row stride in bytes (2D tile addressing) |
| `0x41` | **TSTRIDE_C** | 64 | RW | Column stride in bytes |
| `0x42` | **TTILE_H** | 64 | RW | Tile height in rows (1–8) |
| `0x43` | **TTILE_W** | 64 | RW | Tile width in bytes per row (1–64) |
| | | | | |
| `0x60` | **BIST_CMD** | 64 | RW | Memory BIST: 0=idle, 1=full, 2=quick |
| `0x61` | **BIST_STATUS** | 64 | R | BIST result: 0=idle, 1=running, 2=pass, 3=fail |
| `0x62` | **BIST_FAIL_ADDR** | 64 | R | First failing address (if status=fail) |
| `0x63` | **BIST_FAIL_DATA** | 64 | R | Expected vs actual data at failing address |
| `0x64` | **TILE_SELFTEST** | 64 | RW | Write 1 to start; read: 0=idle, 1=running, 2=pass, 3=fail |
| `0x65` | **TILE_ST_DETAIL** | 64 | R | Which sub-test failed (bitmask) |
| `0x68` | **PERF_CYCLES** | 64 | R | Total core clock cycles since reset |
| `0x69` | **PERF_STALLS** | 64 | R | Cycles spent waiting for bus/memory |
| `0x6A` | **PERF_TILE_OPS** | 64 | R | Total MEX instructions completed |
| `0x6B` | **PERF_EXTMEM** | 64 | R | 64-bit external memory transfers |
| `0x6C` | **PERF_CTRL** | 64 | RW | Bit 0: enable counting, Bit 1: reset all |
| `0x70` | **ICACHE_CTRL** | 64 | RW | Bit 0: enable, Bit 1: invalidate all (auto-clear) |
| `0x71` | **ICACHE_HITS** | 64 | R | I-cache hit counter (since last invalidate) |
| `0x72` | **ICACHE_MISSES** | 64 | R | I-cache miss counter (since last invalidate) |

---

## Interrupt Vector Table

The IVT is an array of 64-bit handler addresses in memory, starting at
`IVT_BASE`.  Each entry is 8 bytes.

| Vector | ID | Name | Trigger |
|--------|----|------|---------|
| 0 | `IVEC_RESET` | Reset | Hardware/software reset |
| 1 | `IVEC_NMI` | NMI | Non-maskable interrupt |
| 2 | `IVEC_ILLEGAL_OP` | Illegal Opcode | Undefined instruction, double EXT |
| 3 | `IVEC_ALIGN_FAULT` | Alignment Fault | Misaligned memory access |
| 4 | `IVEC_DIV_ZERO` | Division by Zero | DIV/UDIV/MOD/UMOD with Rs=0 |
| 5 | `IVEC_BUS_FAULT` | Bus Fault | Access beyond RAM/MMIO bounds |
| 6 | `IVEC_SW_TRAP` | Software Trap | TRAP instruction |
| 7 | `IVEC_TIMER` | Timer | Timer compare-match (when IE=1) |
| 8 | `IVEC_UART` | UART | UART RX data available (when IE=1 and UART IRQ enabled) |
| 9 | `IVEC_NIC` | NIC | NIC RX frame available (when IE=1 and NIC IRQ enabled) |
| … | | | *(vectors 10–14 reserved)* |
| 15 | `IVEC_PRIV_FAULT` | Privilege Fault | User-mode code executed a supervisor-only instruction, wrote a protected CSR, or triggered an MPU violation (access outside [MPU_BASE, MPU_LIMIT) or to HBW) |

### Memory Protection Unit (MPU)

The MPU enforces address-range checks on **user-mode data accesses** (loads
and stores).  It is controlled by two supervisor-only CSRs:

| CSR | Description |
|-----|-------------|
| `MPU_BASE` (0x0B) | Lower bound of the allowed address window (inclusive) |
| `MPU_LIMIT` (0x0C) | Upper bound of the allowed address window (exclusive) |

**Rules** (evaluated only when `PRIV == 1`, i.e. user mode):

1. **MMIO** (`addr[63:32] == 0xFFFF_FF00`) — always allowed (device I/O is
   mediated by the kernel via port words).
2. **HBW** (High Block Window, `addr[31:20] >= 0xFFD` with `addr[63:32] == 0`)
   — always blocked for user mode regardless of MPU window.
3. **RAM** — allowed only if `MPU_BASE <= addr < MPU_LIMIT` (when
   `MPU_LIMIT > MPU_BASE`; if `MPU_LIMIT <= MPU_BASE` the MPU window is
   disabled and all RAM is accessible).

A violation fires `IVEC_PRIV_FAULT` (vector 15) with the faulting address
saved in `TRAP_ADDR`.  Supervisor mode (`PRIV == 0`) is never checked —
all addresses are accessible.

Micro-cores do not implement the MPU (they have no privilege model and run
exclusively in supervisor-equivalent mode).

### Trap Entry Sequence

When an interrupt or trap fires:

1. `push64(FLAGS | (PRIV << 8))` — save flags with privilege level in bit 8
2. `push64(PC)` — save program counter
3. `IVEC_ID ← vector_number`
4. `PC ← mem64(IVT_BASE + 8 × vector_number)` — jump to handler
5. `IE ← 0` — disable further interrupts
6. `PRIV ← 0` — escalate to supervisor mode

### Return from Interrupt

`RTI` reverses the process:
1. `PC ← pop64()` — restore program counter
2. `qword ← pop64()` — restore flags from bits 7:0, privilege from bit 8
3. `FLAGS ← qword[7:0]` (including IE, so interrupts re-enable)
4. `PRIV ← qword[8]` — restore privilege level

---

## Boot Sequence

On CPU reset:

1. All 16 GPRs set to 0
2. `PSEL ← 3` (PC = R3), `XSEL ← 2`, `SPSEL ← 15`
3. All flags cleared
4. D = 0, Q = 0, T = 0
5. Tile CSRs reset (SB=SR=SC=0, SW=1, TMODE=TCTRL=0)
6. ACC[0–3] = 0
7. IVT_BASE = 0
8. `PC ← entry_address` (default 0x0000_0000_0000_0000)
9. `R15 ← RAM_size` (stack pointer at top of RAM)
10. `R2 ← RAM_size` (secondary data pointer)
11. Fetch begins at PC
