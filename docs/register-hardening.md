# Register Hardening: REX Prefix + 32 GPRs

**Status:** Planned — RTL + emulator implementation pending  
**Motivation:** Enable more than 1 cooperative SEP task (Phase 8 maxed out
at R13 because all 16 registers were allocated)  
**Strategy:** x86-64-proven REX prefix via existing EXT family — zero extra
cycles, full backward compatibility

---

## 1  Summary

The current ISA packs register indices into 4-bit nibbles, limiting the
architecture to 16 GPRs.  Rather than redesigning the encoding, we burn
**4 of 13 free EXT prefix slots** (Family 0xF) to supply a 5th register
bit to the next instruction — exactly the approach AMD chose for x86-64.

Old code is **binary-compatible**: without a REX prefix, instructions
address R0–R15 as always.  With a 1-byte prefix, the same instruction can
reach R16–R31.

The T register widens from 8 to 16 bits so that MARK/RET/DIS can
save/restore 5-bit PSEL and XSEL, allowing any of R0–R31 to serve as
program counter or data pointer.

---

## 2  EXT Prefix Slot Allocation

```
Before:
  F0  EXT.IMM64     (used — 64-bit immediate)
  F1  —             (free)
  F2  —             (free)
  F3  —             (free)
  F4  —             (free)
  F5  —             (free)
  F6  EXT.SKIP      (used — conditional skip)
  F7  —             (free)
  F8  EXT.ETALU     (used — extended tile ALU)
  F9–FF             (free, 7 slots)

After:
  F0  EXT.IMM64     (used — 64-bit immediate)
  F1  REX.S         source reg high bit     ext_mod[0] → Rs[4]=1
  F2  REX.D         dest reg high bit       ext_mod[1] → Rd[4]=1
  F3  REX.DS        both src + dest hi      ext_mod[1:0] → Rd[4]=1, Rs[4]=1
  F4  REX.N         nibble reg high bit     ext_mod[2] → nib[4]=1
                                             (INC/DEC/SEP/SEX single-reg families)
  F5  REX.ND        nibble + dest hi        ext_mod[2,1] (for future use)
  F6  EXT.SKIP      (used — conditional skip)
  F7  —             (free — reserved for future)
  F8  EXT.ETALU     (used — extended tile ALU)
  F9–FF             (free, 7 slots)
```

The low 3 bits of `ext_mod` encode which register fields get extended:

| Bit | Field | Affected families |
|-----|-------|-------------------|
| 0   | Rs[4] | MEM, ALU, MULDIV, CSR (source register in `ibuf[1][3:0]`) |
| 1   | Rd[4] | MEM, ALU, MULDIV, CSR, IMM (dest register in `ibuf[1][7:4]`) |
| 2   | nib[4] | INC, DEC, SEP, SEX, CALL.L (register in opcode byte `ibuf[0][3:0]`) |

**Encoding rules:**
- REX + REX is illegal (same as EXT + EXT: `IVEC_ILLEGAL_OP`)
- REX + EXT.IMM64 is legal: `F2 F0 60 Rx 8B` = LDI R16+, imm64
  Wait — no. Double EXT is already illegal. So REX + IMM64 needs thought.
  **Resolution:** REX bits and EXT function coexist in the same
  `ext_mod[3:0]` latch.  REX occupies bits [2:0], the specific EXT
  function (IMM64, SKIP, ETALU) is identified by the full 4-bit value.
  Values 1–5 are the REX range; 0, 6, 8 are the existing functions.
  Since the bit patterns don't collide (EXT_IMM64=0 has no REX bits set,
  EXT_SKIP=6 is above the REX range, EXT_ETALU=8 likewise), this works
  cleanly without needing to stack two prefixes.
  **To use REX with IMM64**, allocate a combined value — but in practice
  LDI already has the register in a nibble in byte 2 (`60 Rx`), so REX.D
  (F2) + LDI already works: the high Rd bit extends `ibuf[2][7:4]`.
  No collision.

---

## 3  T Register Widening

### Current (8-bit)

```
T[7:0] = { XSEL[3:0], PSEL[3:0] }
```

Stored/restored by MARK, SAV, RET (1802), DIS.

### New (16-bit)

```
T[15:0] = { 3'b0, XSEL[4:0], 3'b0, PSEL[4:0] }
```

Each selector field is right-aligned in its byte, zero-padded.  This means:
- Old 8-bit T values (`0x32` = XSEL=3, PSEL=2) naturally zero-extend to
  `0x0302` under the new scheme.  **Not byte-compatible** but
  **functionally equivalent** since the old values only used bits [3:0]
  of each nibble.
- Memory format: MARK pushes 16 bits (in a 64-bit word, top 48 bits zero).
  RET/DIS pop 16 bits.  SAV stores 16 bits (2 bytes via `BUS_HALF`).

### Migration

The BIOS does not use MARK/SAV/RET(1802)/DIS — exclusively CALL.L/RET.L.
KDOS uses CALL.L/RET.L.  So **zero software changes** for the T widening.

The only consumer of the old T format would be hand-crafted 1802-heritage
code using raw MARK+SAV sequences.  The SEP-T@ BIOS word reads T via CSR,
which returns the new 16-bit value — callers parse it, not hard-code
offsets.

---

## 4  RTL Changelist

### 4.1  mp64_pkg.vh  (+ mp64_defs.vh mirror)

```
+ localparam [3:0] EXT_REX_S   = 4'd1;   // Rs[4] = 1
+ localparam [3:0] EXT_REX_D   = 4'd2;   // Rd[4] = 1
+ localparam [3:0] EXT_REX_DS  = 4'd3;   // Rs[4]=1, Rd[4]=1
+ localparam [3:0] EXT_REX_N   = 4'd4;   // nib[4] = 1
+ localparam [3:0] EXT_REX_ND  = 4'd5;   // nib[4]=1, Rd[4]=1
```

### 4.2  mp64_cpu.v

| Area | Change |
|------|--------|
| **Register file** | `reg [63:0] R [0:15]` → `R [0:31]` |
| **Selectors** | `reg [3:0] psel, xsel, spsel` → `reg [4:0]` |
| **dst/src** | `reg [3:0] dst_reg, src_reg` → `reg [4:0]` |
| **T register** | `reg [7:0] T` → `reg [15:0] T` |
| **Reset** | Clear R[0:31], `T <= 16'd0` |
| **REX wires** | Add combinational convenience wires: |

```verilog
// REX extension bits — active only when ext_active and ext_mod in REX range
wire rex_s = ext_active & ext_mod[0];   // source high bit
wire rex_d = ext_active & ext_mod[1];   // dest high bit
wire rex_n = ext_active & ext_mod[2];   // nibble high bit

wire [4:0] nib5  = {rex_n, nib};
wire [4:0] dst5  = {rex_d, ibuf[1][7:4]};
wire [4:0] src5  = {rex_s, ibuf[1][3:0]};
```

| Family | Decode changes |
|--------|----------------|
| **INC** (0x1) | `R[nib]` → `R[nib5]` |
| **DEC** (0x2) | `R[nib]` → `R[nib5]` |
| **MEM** (0x5) | `R[ibuf[1][7:4]]` → `R[dst5]`, `R[ibuf[1][3:0]]` → `R[src5]` |
| **IMM** (0x6) | `R[ibuf[1][7:4]]` → `R[dst5]` (dest only for LDI etc.) |
| **ALU** (0x7) | `R[ibuf[1][7:4]]` → `R[dst5]`, `R[ibuf[1][3:0]]` → `R[src5]` |
| **SEP** (0xA) | `psel <= nib` → `psel <= nib5` |
| **SEX** (0xB) | `xsel <= nib` → `xsel <= nib5` |
| **MULDIV** (0xC) | Both dst/src widened |
| **CSR** (0xD) | Widen `nib[2:0]` → `nib5[2:0]` (or more if CSR ops need R16+) |
| **MARK** | `T <= {3'b0, xsel, 3'b0, psel}` (16-bit) |
| **RET/DIS** | Extract `xsel <= bus_rdata[12:8]`, `psel <= bus_rdata[4:0]` |
| **SAV** | Store T as 16-bit (BUS_HALF) |
| **CALL.L** | `R[ibuf[1][3:0]]` → `R[src5]` (target register) |
| **RET.L** | No register nibble — unchanged |

### 4.3  mp64_cpu_micro.v

Micro-cores do **not** have T (stripped at line 12).  REX prefix support
is optional for micro-cores.  **Recommended:** add REX decode (same
mechanical widening of register indices).  Cost: ~100 extra LUTs per
micro-core for the wider register file (32×64 vs 16×64).  Can defer to
a follow-up if area is tight on Kintex-7.

### 4.4  Emulator (megapad64.py)

| Location | Change |
|----------|--------|
| `t_reg` init | Comment update |
| CSR T write mask | `& 0xFF` → `& 0xFFFF` |
| CSR T read | Already returns `self.t_reg` — no change |
| RET (1802) | Pop 16 bits: `xsel = (t >> 8) & 0x1F`, `psel = t & 0x1F` |
| DIS | Same as RET |
| MARK | `t = (xsel << 8) \| psel`, push 16 bits |
| SAV | `mem_write16(rx, t_reg)` (or keep as 8-bit store if SAV stays byte) |
| EXT decode | Recognize F1–F5 as REX, set `rex_s/rex_d/rex_n` flags |
| All reg accesses | Apply REX bits to register index |

### 4.5  Assembler (asm.py)

| Change | Description |
|--------|-------------|
| Register parser | Accept `R0`–`R31` (currently `R0`–`R15`) |
| REX emission | When Rd > 15 or Rs > 15 or Rn > 15, auto-emit REX prefix |
| Validation | REX + EXT.IMM64 is fine (different ext_mod values) |
| REX + EXT.SKIP | Would need to be disallowed (ext_mod conflict) |

### 4.6  ISA Reference (isa-reference.md)

- Document REX prefix values F1–F5
- Document widened T register format
- Update register count from 16 to 32
- Note backward compatibility

---

## 5  Cooperative Multitasking Impact

### Before (16 regs)

```
R0  = DMA/IRQ      R8  = scratch
R1  = IRQ PC        R9  = scratch
R2  = X (data ptr)  R10 = scratch
R3  = PC            R11 = scratch
R4  = call target   R12 = scratch
R5  = call return   R13 = Task 1 PC (only free register)
R6  = scratch       R14 = link register
R7  = scratch       R15 = SP
```

Tasks possible: **1** extra (R13 only).

### After (32 regs)

R0–R15 keep their current allocations.  R16–R31 are **all free** for:

- **SEP-based task PCs:** Each cooperative task needs exactly 1 register
  for its program counter.  R16–R31 = up to **16 extra tasks**.
- **Extra scratch/data pointers:** Tasks can use the remaining high
  registers for per-task working storage pointers.
- **SEX targets:** A task can have its own data pointer (e.g., SEX R20).

The BIOS `PAUSE` mechanism (`SEP Rn; ... SEP R3`) works identically for
R16+ — the SEP instruction with REX.N prefix selects the 5-bit register.

### Task overhead

| Item | Cost |
|------|------|
| SEP to task | 2 bytes (REX.N + SEP Rn) vs 1 byte for R0–R15 |
| SEP back to main | 1 byte (SEP R3, no prefix needed) |
| Context per task | 1 register (the PC) — state lives in memory |

---

## 6  Area Impact

| Component | Change | Estimated cost |
|-----------|--------|----------------|
| Register file | 16→32 × 64-bit | +1,024 flip-flops per big core |
| Selector wires | 4→5 bit muxing | +~50 LUTs per core |
| T register | 8→16 bits | +8 FFs |
| REX decode logic | 3 wires + AND gates | ~10 LUTs |
| **Total per big core** | | **~1,100 FFs, ~60 LUTs** |
| **4 big cores** | | **~4,400 FFs, ~240 LUTs** |

This is ~5% of the current ~74K FFs and <0.3% of the ~85K LUTs.
Trivially fits on Kintex-7.

---

## 7  Implementation Order

1. **mp64_pkg.vh + mp64_defs.vh** — add EXT_REX_* localparams
2. **mp64_cpu.v** — widen T, register file, selectors, REX wires
3. **mp64_cpu.v** — convert all register index expressions to use `nib5`/`dst5`/`src5`
4. **mp64_cpu.v** — update MARK/RET/DIS/SAV for 16-bit T
5. **mp64_cpu.v** — update reset to clear R[0:31]
6. **megapad64.py** — REX prefix handling + T widening
7. **asm.py** — R16–R31 support + auto REX emission
8. **tests** — verify R16+ load/store/ALU, SEP R16+, MARK/RET round-trip
9. **isa-reference.md** — document everything
10. **mp64_cpu_micro.v** — optional micro-core widening (defer if area-tight)

---

## 8  Backward Compatibility

| Scenario | Status |
|----------|--------|
| Existing binaries (R0–R15 only) | **100% compatible** — no REX prefix = 4-bit indices as before |
| BIOS (no MARK/SAV/RET-1802) | **No changes needed** |
| KDOS (CALL.L/RET.L only) | **No changes needed** |
| Hand-crafted 1802 MARK/RET code | **Binary-incompatible** — T format changed from 8 to 16 bits |
| CSR reads of T | **Value format changed** — callers must parse 16-bit `{0,XSEL[4:0],0,PSEL[4:0]}` |

The only breaking change is the T register format, and no existing
software in the project uses MARK/RET(1802) directly.
