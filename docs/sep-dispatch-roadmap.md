# SEP/SEX Dispatch Roadmap — Restoring 1802 Heritage to the BIOS

*Hybrid SEP/SEX register-dispatch and D-accumulator data flow for
hot paths, keeping `call.l`/`ret.l` for everything else.*

The Megapad-64 ISA already has `SEP Rn` (family 0xA) and `SEX Rn`
(family 0xB) wired up, the T register, MARK/SAV/RET, the D accumulator,
the MEMALU family (0x8), port I/O (0x9), and the Q flip-flop — all
inherited from the CDP1802.  Today the BIOS ignores all of it, using
modern `call.l`/`ret.l` and 64-bit register operations exclusively.

This roadmap brings the 1802 register-dispatch *and data-flow* model
back to life on the paths where it matters.  The production payoff is in
**bulk MMIO register access** — the byte-serialization pattern shared
by every DMA-capable peripheral: NIC, disk, framebuffer, and CRC.
Every time the BIOS writes a 64-bit DMA address to an MMIO register,
it currently burns ~16 instructions of shift-and-store through 64-bit
registers.  The 1802 `SEX`/`LDXA`/`SHR.D` model was *designed* for
exactly this kind of byte-sequential I/O.

> **Micro-core restriction:** Cluster micro-cores do **not** implement
> SEP (family 0xA) or SEX (family 0xB).  Executing either opcode on a
> micro-core traps as `ILLEGAL_OP`.  The MEMALU (0x8) and port I/O (0x9)
> families, plus the 1802 SYS sub-ops (RET/DIS/MARK/SAV/SEQ/REQ) and
> the D/T/Q registers, are likewise unavailable on micro-cores.  All
> work described in this roadmap applies exclusively to **full cores**
> (core 0 and secondary full cores).  Any code that may run on
> micro-cores must continue using `call.l`/`ret.l`.

---

## Background: How 1802 SEP Dispatch Works

On the CDP1802, any of the 16 registers can be the program counter.
`SEP Rn` sets `PSEL ← n`, so execution instantly continues at the
address in `R[n]`.  The previous PC register "freezes" at the byte after
the SEP instruction — it becomes the return address with zero memory
traffic.  The callee returns by doing `SEP R3` (or whichever register
the caller used), freezing its own PC for the next invocation.

The catch: after a routine returns via `SEP R3`, the register that was
its PC (say R4) now points at the instruction *after* the return-SEP
inside the routine, not the entry point.  Classic 1802 code solves this
with a 2-byte re-entry trampoline:

```asm
emit_char:                  ; R4 → here on first call
    st.b r8, r1            ; do the work
    sep  r3                 ; return (PSEL ← 3, R4 freezes at next byte)
    br   emit_char          ; re-entry: next SEP R4 lands here, loops back
```

**Cost per round-trip:** 2 bytes, 2 cycles (1+1).
**Current cost:** 3 bytes, 4 cycles (`call.l` = 2B/2c, `ret.l` = 1B/2c)
plus 2 memory accesses (push + pop on R15).

---

## Motivation

| Benefit | Impact |
|---------|--------|
| **SEX + D for bulk MMIO byte-serialization** | The dominant production hot path.  NIC, disk, and framebuffer all write 64-bit DMA addresses as 8 individual `st.b` calls through 64-bit shift chains (~16 instructions).  With the new `STXI` instruction (0x89), each `st.b + inc` pair collapses to a single byte.  Combined with `SEX Rn` + `SHR.D` + `STXI`, this compresses DMA serialization further — fewer instructions, fewer bytes, zero 64-bit register pressure. |
| **SEX + LDXA for packet/header parsing** | Any future IP/UDP stack above the NIC words will parse headers byte-by-byte.  `SEX Rn; LDXA; LDXA; LDXA...` is 1 byte per field read with auto-increment vs. 6 bytes (`ld.b` + `cmpi` + `inc`) today. |
| **SEP for zero-traffic dispatch** | SEP is register-only; no push/pop to R15.  Matters for leaf I/O routines called in tight loops and for cooperative task switching. |
| **Deterministic 1-cycle timing** | `call.l` timing varies if R15 targets slow memory.  SEP is always 1 cycle.  Critical for FPGA bring-up and cycle-counted delays. |
| **Smaller multicore stack zones** | Secondary cores need less RSP headroom when I/O uses SEP instead of stack-based calls. |
| **Tighter JIT output** | Non-inlineable words currently compile as 13-byte `ldi64 r11, <addr>; call.l r11` sequences.  SEP dispatch for NEXT opens the door to 1-byte word-exit. |
| **Activates dead ISA silicon** | Families 0x8, 0x9, 0xA, 0xB, plus D/T/Q registers are wired in RTL but never exercised.  This roadmap gives them a reason to exist. |
| **Q flip-flop as zero-cost semaphore** | Q can signal peripheral-busy state testable via `BR.BQ`/`BR.BNQ` — a register-only, memory-free synchronization primitive. |
| **Zero-overhead cooperative multitasking** | `SEP Rn` to switch to another task's register costs 1 cycle, zero memory, no context save.  Enables green threads cheaper than anything a conventional call/return architecture can do. |
| **MARK/SAV for smarter fault recovery** | Fault handlers can inspect the T register to know which SEP context was active when a fault hit, enabling targeted recovery instead of resetting everything. |

---

## Phased Plan

### Phase 0 — Audit and Test Harness

**Goal:** Establish baseline measurements and a safety net before
touching any code.

- [ ] Add a `test_sep_dispatch` case to `test_megapad64.py` that
  injects a minimal `SEP R4` / `SEP R3` round-trip into the emulator
  and asserts R4 freezes correctly and execution returns to R3.
- [ ] Add an I/O microbenchmark as a loadable Forth definition in
  `tools.f` (not the BIOS ROM):
  `: IO-BENCH  PERF-CYCLES@  4096 0 DO  42 EMIT  LOOP  PERF-CYCLES@  SWAP - . ;`
  This becomes the regression metric without consuming BIOS space.
- [ ] Verify `test_megapad64.py` and `test_system.py` pass cleanly as
  the pre-conversion baseline.
- [ ] Confirm the RTL (`rtl/core/mp64_cpu.v`) handles `PSEL` switching
  correctly — inspect the fetch stage for `PSEL`-derived PC muxing.

**Estimated effort:** ~1 day.

---

### Phase 1 — Leaf I/O Primitives (R4, R5, R6)

**Goal:** Convert the three hot leaf subroutines to SEP dispatch.
These are already loaded into dedicated registers at boot and called
from a single PC register (R3).  Perfect SEP candidates — no nesting,
no register pressure increase.

**Register allocation (unchanged):**

| Register | Role | SEP usage |
|----------|------|-----------|
| R3 | PC (PSEL=3) | Caller — freezes as return address |
| R4 | `emit_char` | `SEP R4` to call, `SEP R3` to return |
| R5 | `key_char` | `SEP R5` to call, `SEP R3` to return |
| R6 | `print_hex_byte` | `SEP R6` to call, `SEP R3` to return |

**Changes to each routine:**

```asm
; BEFORE (current):
emit_char:
    st.b r8, r1
    ret.l                       ; pop PC from R15

; AFTER:
emit_char:
    st.b r8, r1
    sep  r3                     ; return via register switch
    br   emit_char              ; re-entry trampoline
```

**Changes to call sites (879 → ~36 affected for R4, ~6 for R5, ~12 for R6):**

```asm
; BEFORE:
    call.l r4                   ; push PC, jump to R4

; AFTER:
    sep r4                      ; PSEL ← 4, no stack touch
```

**Routines that call R4/R5/R6 from deeper nesting** (e.g., `print_str`
calls `emit_char` in a loop) work identically — the SEP/SEP round-trip
is invisible to the rest of the call chain because R3 is preserved.

**Key constraint:** `print_hex_byte` (R6) currently calls `emit_char`
(R4) internally via `call.l r4`.  Under SEP, R6 *cannot* do `SEP R4`
directly because R6 is the current PC and doing `SEP R4` would freeze
R6 mid-routine.  Two options:

1. **Refactor `print_hex_byte` to inline the UART write** (`st.b r8, r1`)
   instead of calling emit_char.  Already only 1 instruction — trivial.
2. **Keep `call.l r4` inside R6's body** — it still works because `call.l`
   pushes to R15 regardless of PSEL.  But this sacrifices part of the
   benefit.

Option 1 is recommended.  `print_hex_byte` already does `st.b r8, r1`
effectively — it just routes through R4 for no good reason.

**Estimated effort:** ~2 hours.  ~54 `call.l r4/r5/r6` → `sep r4/r5/r6`
replacements (mechanical), plus 3 routine rewrites (small).

**Savings:** ~54 bytes of code, ~108 cycles per full banner print,
zero stack writes for the most frequent BIOS calls.

---

### Phase 2 — `print_str` as R10 Coroutine

**Goal:** Convert `print_str` to a SEP-based coroutine that
interleaves with the caller without stack frames.

**Current pattern:**
```asm
    ldi64 r10, str_banner
    ldi64 r11, print_str
    call.l r11                  ; pushes to R15, jumps
```

**SEP pattern — assign R10 as `print_str`'s PC register:**

```asm
; At boot:
    ldi64 r10, print_str

; Call site:
    ldi64 r10, print_str        ; reset entry (or skip if re-entrant)
    ldi64 r13, str_banner       ; string pointer in R13 (was R10)
    sep  r10                    ; PSEL ← 10, enter print_str

; print_str body:
print_str:
    ld.b r1, r13
    cmpi r1, 0
    breq .ps_done
    cmpi r1, 0x0A
    brne .ps_not_lf
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
.ps_not_lf:
    st.b r8, r1
    inc r13
    br   print_str
.ps_done:
    sep  r3
    br   print_str
```

**Trade-off:** Requires moving the string pointer from R10 to R13
(which is already documented as scratch), and R10 becomes the
`print_str` PC register.  This changes the calling convention for
string printing throughout the BIOS (~80 sites reference R10 for
strings).

**Recommendation:** Defer to Phase 4 or skip entirely.  The R4/R5/R6
conversions give 80% of the benefit.  This is high-churn for modest
gain.

**Estimated effort:** ~4 hours (many R10 references to audit).

---

### Phase 3 — Hybrid STC with SEP NEXT/EXIT (Option B) ✅ DONE

> **Status:** Implemented.  compile_call emits 10 bytes (sep r16 + XT),
> compile_ret emits 2 bytes (sep r17).  R16 = NEXT handler, R17 = EXIT
> handler.  Both initialised at boot for core 0 and secondary cores.
> JIT inline table, peephole, and bigram fusion untouched.
> bytes_saved baseline recalibrated from 13 → 10.
>
> **History:** Originally deferred because the only free register (R7)
> would have to serve as both NEXT (call dispatch) and EXIT (return
> dispatch), and there was no way to distinguish the two roles without
> a runtime flag check that kills performance.  The ISA team's decision
> to add 16 extended registers (R16–R31) eliminates this blocker:
> R16 = NEXT, R17 = EXIT — two dedicated handlers, no ambiguity.
>
> **Options considered and rejected:**
> - **Option A (Pure ITC):** Destroys JIT inline table, peephole
>   fusion, and bigram fusion.  Net code-size regression.  Dead.
> - **Option C (Return-only hook):** Zero savings — strictly inferior
>   to B now that R16/R17 are available.
> - **Option D (ldi32 shorter encoding):** Same 4-byte savings but no
>   dispatch hooks.  Worth revisiting independently if `ldi32` is
>   cheap in the ISA.

**Goal:** Shrink non-inlined compiled calls from 13 → 9 bytes while
preserving the JIT inline table, peephole optimizer, and bigram fusion
entirely.

#### Design — Hybrid STC with R16 (NEXT) and R17 (EXIT)

The key insight: with two dedicated registers, `sep r16` always means
"call through an inline XT" and `sep r17` always means "return from a
word."  No flags, no ambiguity.

**Register allocation (extended bank):**

| Register | Role | Handler code |
|----------|------|------|
| R16 | NEXT | Read 8-byte XT from caller's frozen R3, advance R3 past XT, push R3 as return addr on RSP, `sep` to target |
| R17 | EXIT | Pop return addr from RSP into R3, `sep r3` to resume caller |

**NEXT handler (R16):**
```asm
forth_next:                     ; R16 is PC here
    ; R3 froze at the byte after `sep r16` — pointing at the inline XT
    ldn  r11, r3                ; fetch 8-byte XT from R3's frozen position
    addi r3, 8                  ; advance R3 past the inline XT
    subi r15, 8                 ; push return address (R3) onto RSP
    str  r15, r3
    mov  r3, r11                ; R3 ← target XT
    sep  r3                     ; dispatch to target word
    br   forth_next             ; re-entry trampoline
```

**EXIT handler (R17):**
```asm
forth_exit:                     ; R17 is PC here
    ldn  r3, r15                ; pop return address from RSP
    addi r15, 8
    sep  r3                     ; resume caller
    br   forth_exit             ; re-entry trampoline
```

**Compiled call site (non-inlined word):**
```
A0 10                         ; sep r16              (1 byte)
<8 bytes LE addr>             ; inline XT            (8 bytes)
                              ; total: 9 bytes (was 13)
```

**Compiled return (`;`):**
```
A0 11                         ; sep r17              (1 byte, was ret.l = 1 byte)
```

#### What stays the same

- **JIT inline table:** All 17 entries emit raw instruction bytes
  directly — no call/return involved.  Completely untouched.
- **Peephole optimizer:** Literal folding, compact literals, TRUE/FALSE
  — all emit raw bytes.  Untouched.
- **Bigram fusion:** DUP+, SWAP OVER, etc. — raw byte sequences.
  Untouched.
- **Interpreted execution:** The outer interpreter continues using
  `call.l`/`ret.l` for dictionary dispatch.  SEP NEXT/EXIT is only
  for JIT-compiled code paths.

#### What changes

| Component | Current | After Phase 3 |
|-----------|---------|---------------|
| `compile_call` | 13 bytes: `ldi64 r11, <addr>; call.l r11` | 9 bytes: `sep r16` + 8-byte inline XT |
| `compile_ret` (`;`) | 1 byte: `ret.l` (0x0E) | 1 byte: `sep r17` (0xA011) — **wait, see note** |
| `bytes_saved` baseline | 13 bytes | 9 bytes (recalibrate `jit_compile_word`) |
| Boot init | — | `ldi64 r16, forth_next; ldi64 r17, forth_exit` |
| Secondary cores | — | Same R16/R17 init (per-core register file, no interference) |

> **Encoding note:** `sep r16` is 2 bytes (REX 0xF4 + 0xA0).
> `compile_ret` emitting `sep r17` is also 2 bytes — a +1 byte
> regression vs `ret.l`.  Call overhead is 10 bytes, savings = 3
> bytes/call — still worthwhile.  Confirmed in implementation.
>
> **ldi64 limitation:** `ldi64` already uses the 0xF0 EXT prefix for
> its own encoding, so `ldi64 r16, ...` is unrepresentable (double
> prefix conflict).  Boot init uses `ldi64 r11, addr; mov r16, r11`
> instead — 2 extra instructions, executed once at boot.

#### Savings analysis

| Metric | Current (STC) | Hybrid B (1-byte sep) | Hybrid B (2-byte sep) |
|--------|--------------|----------------------|----------------------|
| Non-inlined call | 13 bytes | 9 bytes (-4) | 10 bytes (-3) |
| Word return | 1 byte | 1 byte (=) | 2 bytes (+1) |
| Inlined primitive | 3–13 bytes | unchanged | unchanged |
| Peephole literal | 7 bytes | unchanged | unchanged |
| Bigram fusion | 6 bytes | unchanged | unchanged |
| Stack traffic/call | 2 × 8 bytes (push+pop) | 2 × 8 bytes (RSP in NEXT/EXIT) | same |
| Cycles per dispatch | 4 (call+ret) | ~8 (NEXT handler) + 1 (sep) | same |

For a typical 200-word compilation with ~60% inlined, ~80 non-inlined
calls: **240–320 bytes saved**.  All inlined paths are untouched.

The cycle cost of the NEXT handler (~5–6 instructions) is higher than
hardware `call.l`, so this is a **code density optimisation**, not a
speed optimisation.  The programmable dispatch points are the real win:
R16 and R17 can be instrumented for tracing, profiling, and
task-switch hooks with zero changes to compiled word bodies.

#### 3a — `compile_call` changes

`compile_call` currently emits 13 bytes:
```
F0 60 B0 <8 bytes LE>    ; ldi64 r11, <addr>   (11 bytes)
0D 0B                     ; call.l r11            (2 bytes)
```

After Phase 3 it emits 9 (or 10) bytes:
```
A0 10                     ; sep r16                (1 or 2 bytes)
<8 bytes LE addr>         ; inline XT              (8 bytes)
```

The `reloc_record` for the 8-byte address is still needed — same
complexity as today, just at a different offset.

#### 3b — JIT inline table awareness

The `jit_inline_table` entries are raw instruction bytes that get copied
to HERE.  These don't need to change — inlined primitives don't call or
return.  But the **fallback path** (`jit_cw_fallback`) emits the new
`sep r16` + XT sequence, and `compile_ret` emitted by `;` (semicolon)
emits `sep r17` instead of `ret.l`.

#### 3c — `bytes_saved` recalibration

The JIT's `bytes_saved` metric currently assumes `compile_call` emits
13 bytes.  The baseline shifts to 9 (or 10) bytes.  Update:
- `jit_compile_word` (line `addi r7, 13`) → `addi r7, 9`
- `jit_cw_done` (`ldi r1, 13; sub r1, r12`) → `ldi r1, 9; ...`

The inline table `bytes_saved` per entry shrinks (smaller baseline to
beat) but all entries that currently save > 4 bytes remain positive.
Entries saving exactly 1–3 bytes (if any) might go negative and should
be re-evaluated.

#### 3d — Dispatch hooks (bonus)

Because all non-inlined calls and returns flow through R16/R17, these
become natural instrumentation points:

- **Tracing:** R16's handler can log the XT being dispatched to a
  circular buffer before jumping.  R17's handler can log return
  addresses.  Enable/disable via a flag word (`SEP-TRACE ON/OFF`).
- **Profiling:** R16 increments a per-word call counter.  R17 reads
  `PERF-CYCLES@` to accumulate per-word cycle counts.
- **Task switching:** R17 checks a yield flag on every word return
  and context-switches if set — cooperative preemption at word
  granularity with zero compiled-code changes.

These hooks cost nothing when disabled (the flag check is a single
`ldn` + `breq` skip in the handler).  This replaces the need for a
separate Phase 4f trace unit.

#### Prerequisites

1. **ISA team:** Extended register file (R16–R31) implemented in RTL
2. **Emulator team:** `SEP Rn` for n ≥ 16 implemented in `megapad64.py`
3. **Confirm encoding:** `sep r16` instruction size (1 byte or 2 bytes)
   determines exact savings

**Estimated effort:** ~2 days once prerequisites land.  This is the
most architecturally significant remaining phase — it changes how
compiled Forth words dispatch.

---

### Phase 4 — Q Flip-Flop as I/O Semaphore

**Goal:** Use Q as a UART-busy signal between SEP-dispatched routines.

```asm
emit_char:
    seq                         ; Q ← 1 (UART busy)
    st.b r8, r1
    req                         ; Q ← 0 (UART idle)
    sep  r3
    br   emit_char
```

Any code can now test `BR.BNQ` to skip output if the UART is mid-write
(useful for multicore debug where two cores try to print simultaneously).
This is a 1-cycle, zero-memory synchronization mechanism.

**Estimated effort:** ~30 minutes (trivial once Phase 1 is done).

**Caveat:** Q is a single global bit — it only works as a semaphore for
one resource.  Fine for single-writer UART; not a general mutex.

---

### Phase 5 — Secondary Core SEP Dispatch

**Goal:** Give secondary cores (`secondary_core_entry`) the same SEP
I/O primitives, reducing their stack requirements.

Currently, secondary cores replicate the R4/R5/R6 setup:
```asm
secondary_core_entry:
    ...
    ldi64 r4, emit_char
    ldi64 r5, key_char
    ldi64 r6, print_hex_byte
```

After Phase 1, this code works unchanged — the SEP-converted routines
are position-independent (no absolute addresses in the trampoline).
The only action item is to verify that PSEL is 3 on secondary cores
(it is, per the ISA reset spec) and that `SEP R4` from a secondary core
doesn't interfere with core 0's R4.

**Each core has its own register file**, so `SEP R4` on core 1 switches
core 1's PSEL to 4 and uses core 1's R4 — no cross-core interference.
But R4 on each core must independently point to `emit_char`'s entry
(or the re-entry trampoline after first use).  Since each core runs
`ldi64 r4, emit_char` at startup, this is already correct.

**Estimated effort:** ~1 hour (mostly verification / testing).

---

### Phase 6 — SCRT (Standard Call and Return Technique)

**Goal:** Implement the classic 1802 SCRT as an optional calling
convention for deeper call chains, available alongside `call.l`/`ret.l`.

SCRT uses two dedicated registers as tiny "call" and "return" micro-
routines:

```asm
; R4 = SCRT_CALL routine
; R5 = SCRT_RET routine
;
; To call a subroutine at address ADDR:
;   SEP R4              ; enter CALL micro-routine
;   .dq ADDR            ; inline address follows in instruction stream
;
; SCRT_CALL:
;   MARK                ; T ← X||P, push T, X ← P
;   ... read inline address from caller's stream ...
;   ... load into target register, SEP to it ...
;
; SCRT_RET:
;   ... restore from T, SEP back to caller ...
```

**This conflicts with Phase 1's use of R4/R5 for emit/key.**
If SCRT is desired, the I/O primitives move to R6/R7/R8 and R4/R5
become the SCRT CALL/RET micro-engines.  This is a larger refactor.

**Recommendation:** Implement only if there's a desire to run classic
1802 software or build an 1802 compatibility layer.  For the Forth BIOS
alone, Phase 1 + Phase 3 give better results with less complexity.

**Estimated effort:** ~3 days (significant convention change).

---

### Phase 7 — SEX + D Accumulator for Byte Processing ✅ DONE

> **Status:** Complete.  16 routines converted to `sex` + `glo`/`ghi` +
> `stxi`/`stxd.d` chains: FILL, TFILL, CMOVE, MOVE (bwd + fwd),
> write_mmio_addr8_le, write_mmio_u32_le, w_disk_sec_store,
> w_disk_dma_store, compile_call, compile_literal, w_create,
> w_var_name_done, w_val_name_done, does_runtime, w_lstore.
> 4 bugs found and fixed (C++ missing STXI opcode, dead code in
> w_move_fwd, lsri imm4 overflow, VRAM bounds overflow in C++).
> All tests passing: 1687/1690 (3 skipped = DNS/network).

**Goal:** Use `SEX Rn` to set the data pointer, then exploit the MEMALU
family (0x8) and D accumulator for byte-level operations that currently
use 64-bit registers inefficiently.

Today XSEL=2 is set at boot and never changed.  The entire MEMALU
family — `LDXA`, `AND.X`, `ADD.X`, `SHR.D`, etc. — sits unused.

**This is the highest production-value phase in the roadmap.**  The
MMIO DMA byte-serialization pattern it targets appears in every
DMA-capable peripheral: NIC, disk, framebuffer, and CRC.

#### STXI / STXD.D Site Inventory

**Prerequisite completed:** `STXI` (0x89) and `STXD.D` (0x8B) are
implemented in RTL, emulator, and assembler (see SoC-hardening §0).
Each replaces the 2-instruction / 3-byte `st.b + inc` (or `st.b + dec`)
sequence with a single 1-byte instruction.

**~194 static `st.b + inc` pairs** and **~12 hot loops** across 38+
sites become single STXI instructions.  Full inventory:

| Site | Pairs saved | Frequency | Sub-phase |
|------|-------------|-----------|-----------|
| `compile_call` (13-byte code emission) | 13 | every non-inline word | 7e |
| `compile_literal` (16-byte emission) | 16 | every literal | 7e |
| CREATE / VARIABLE trampolines | ~52 | every definition | 7e |
| JIT compact/TRUE literal + lit_fold | 24 | peephole optimizer | 7e |
| Control flow (IF/ELSE/LOOP/UNTIL/WHILE/REPEAT/OF) | 42 across 14 routines | every control word | 7e |
| `write_mmio_addr8_le` | 7 | every NIC/disk DMA | 7a |
| `write_mmio_u16` / `write_mmio_u32` | 2–4 per call | NIC len, disk sector | 7b |
| CMOVE / FILL / MOVE / VSCROLL loops | 1 per iteration | core Forth words, hot loops | 7e |
| `compile_byte` / `compile_ret` / `C,` | 3 | very frequently | 7e |
| Autoboot prefix build + name copy | 7 + N | once at boot | 7e |
| Dictionary scanning (`find_word`, `parse_word`) | — (LDXA) | every word lookup | 7d |

**STXD.D:** 1 site — MOVE backward copy loop.  Low count but validates
the instruction exists for stack-grow-down and reverse-fill patterns.

**Two tiers of work:**

- **Tier 1 (§7a–7d):** Architectural transformations using `SEX Rn` +
  D accumulator + `GLO`/`GHI`/`SHR.D`/`LDXA` — the byte-serial I/O
  and scanning patterns that motivated this phase.
- **Tier 2 (§7e):** Mechanical STXI substitution — find every
  `st.b Rx, Ry; inc Rx` pair and replace with `stxi`.  No `SEX`
  needed, no architectural change, just smaller and faster code.
  ~153 static sites, dominated by compiler emit paths.

Tier 2 is lower risk and higher volume; Tier 1 is higher ROI per site
but requires care with `SEX`/`SEX R2` save-restore.  Both tiers can
proceed in parallel.

#### 7a — MMIO DMA Address Serialization (Primary Target)

Every DMA-capable peripheral requires writing a 64-bit address as 8
little-endian bytes to ascending MMIO offsets.  The BIOS currently does
this with a long chain of `mov`/`lsri`/`st.b`/`inc` through 64-bit
registers.  The pattern appears verbatim in:

- **`w_net_send`** and **`w_net_recv`** — NIC DMA address at offset +0x02 (~30 instructions each)
- **`disk_read_sectors`** — disk DMA address at offset +0x06, plus a 4-byte sector number (~25 instructions)
- **Framebuffer DMA** and **CRC data** words — same pattern

Here's what the NIC send path looks like today:
```asm
; CURRENT — w_net_send DMA address write (30+ instructions):
    mov r0, r11
    addi r0, 2              ; r0 → NIC+0x02
    mov r1, r9              ; r1 = DMA address
    st.b r0, r1             ; byte 0
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7             ; byte 1
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7             ; byte 2
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7             ; byte 3
    inc r0
    ldi r7, 0
    st.b r0, r7             ; bytes 4–7 (zero)
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
```

With SEX + D + MEMALU this becomes a shared helper:

```asm
; AFTER — write_mmio_addr: serialize R9 as 8 LE bytes via R(X)
;   Entry: R(X) already set via SEX to point at target MMIO offset
;   R9 = 64-bit DMA address to write
write_mmio_addr:
    glo  r9                 ; D ← R9[7:0]
    stxd                    ; M(R(X)) ← D; R(X)++  (byte 0)
    ghi  r9                 ; D ← R9[15:8]
    stxd                    ; byte 1
    ; For bytes 2-3, shift R9 and repeat:
    mov  r7, r9
    lsri r7, 16
    glo  r7
    stxd                    ; byte 2
    ghi  r7
    stxd                    ; byte 3
    ldi.d 0                 ; D ← 0  (upper 4 bytes are zero)
    stxd                    ; byte 4
    stxd                    ; byte 5
    stxd                    ; byte 6
    stxd                    ; byte 7
    sep  r3                 ; return
    br   write_mmio_addr
```

**Call site (e.g., `w_net_send`):**
```asm
w_net_send:
    ...
    ldi64 r11, 0xFFFF_FF00_0000_0402   ; NIC + 0x02
    sex  r11                            ; R(X) → MMIO target
    sep  r6                             ; call write_mmio_addr (reassign R6)
    sex  r2                             ; restore XSEL=2
    ...
```

**Impact:**  ~30 instructions → ~12.  Shared across NIC send, NIC recv,
disk read, disk write, framebuffer DMA, and CRC setup — **6+ call sites
immediately benefit** from one helper.  Zero 64-bit register pressure
on the serialization path.

#### 7b — Shared `write_mmio_u16` and `write_mmio_u32` Helpers

The same pattern recurs at smaller widths:
- **NIC frame length** — 16-bit write at NIC+0x0A
- **Disk sector number** — 32-bit write at Disk+0x02
- **Disk sector count** — 8-bit write at Disk+0x0E

Each gets a tiny D-accumulator helper:
```asm
write_mmio_u16:             ; serialize R1[15:0] via R(X)
    glo  r1
    stxd
    ghi  r1
    stxd
    sep  r3
    br   write_mmio_u16
```

#### 7c — `print_hex_byte` via D accumulator

`print_hex_byte` manually shifts and masks nibbles through 64-bit
register operations.  The D accumulator with `SHR.D` and `AND.X` is
purpose-built for exactly this:

```asm
; SEX+D version:
print_hex_byte:
    sex  r8                 ; R(X) = R8 (UART base) for output
    plo  r0                 ; D ← R1[7:0]  (save input byte)
    shr.d                   ; D >>= 4 (with 4× SHR.D)
    shr.d
    shr.d
    shr.d
    ; D = high nibble → convert to ASCII, output
    ...                     ; nibble-to-ASCII in D
    st.b r8, r1             ; or use STXD for auto-dec output
    sex  r2                 ; restore XSEL=2
    sep  r3                 ; return
    br   print_hex_byte
```

Combined with Phase 1 (R6 is already SEP-dispatched), the entire hex
output path runs in pure 1802 style.

#### 7d — Dictionary scanning with LDXA

`find_word` and `parse_word` do byte-by-byte string scanning:
```asm
    ld.b r1, r11            ; 2 bytes
    cmpi r1, 0x20           ; 3 bytes
    inc  r11                ; 1 byte  →  6 bytes per char
```

With SEX:
```asm
    sex  r11                ; 1 byte (once)
    ldxa                    ; 1 byte: D ← M(R(X)); R(X)++
    ; D now has the byte, R11 auto-incremented
```

That's 1 byte per character (after the initial SEX) vs. 6 bytes.
Dictionary lookup scans ~200 entries on a typical BIOS — the code size
and cycle savings are significant.

**Constraint:** `SEX` and all MEMALU ops are supervisor-only.  This is
fine for the BIOS but means user-mode Forth words can't use it.

#### 7e — Mechanical STXI Substitution (Tier 2)

Every `st.b Rx, Ry; inc Rx` pair in the BIOS where Rx is the
destination pointer becomes a single `stxi`.  No `SEX` change needed —
STXI writes `D[7:0]` to `M(R(X))` and increments R(X), but the value
in D must be set first.  For code-emission sites, the pattern is:

```asm
; CURRENT — compile_call emits a byte:
    ldi.d 0xE0              ; D ← opcode
    st.b r0, r1             ; M(R0) ← byte from R1 (wrong — actually from a register)
    inc  r0                 ; R0++
```

After reading the actual emit pattern more carefully, the BIOS emit
sites use `st.b Rdst, Rsrc; inc Rdst` where Rdst is the dictionary
pointer (HERE).  The STXI replacement requires:

1. `SEX Rdst` — point R(X) at HERE
2. `LDI.D <byte>` — load the byte into D
3. `STXI` — store D[7:0] to M(R(X)), R(X)++

For **single-byte emits** (e.g., `compile_ret`, `C,`), this is a wash
(3 instructions vs 3).  The win comes from **multi-byte sequences**
where the `SEX` is amortized:

```asm
; compile_call — 13 bytes to emit, current cost: 26 instructions (13× st.b+inc)
; With STXI: SEX + 13× (LDI.D + STXI) + SEX R2 = 28 instructions
;   BUT each instruction is 1–2 bytes vs 2–3, saving ~15 bytes of code.
;   And STXI is 1 cycle vs st.b(1)+inc(1) = 2 cycles per byte.
```

**Primary sites for Tier 2 conversion:**

| Routine | Bytes emitted | Current insns | After STXI | Cycle savings |
|---------|---------------|---------------|------------|---------------|
| `compile_call` | 13 | 26 (×`st.b+inc`) | 28 (SEX + 13×LDI.D+STXI + SEX R2) | 13 cycles (1 per byte) |
| `compile_literal` | 16 | 32 | 34 | 16 cycles |
| `CREATE` trampoline | ~26 | ~52 | ~54 | ~26 cycles |
| `VARIABLE` trampoline | ~26 | ~52 | ~54 | ~26 cycles |
| Control flow words | 3–6 each | 6–12 each | 8–14 each | 3–6 cycles each |
| `CMOVE` inner loop | 1/iter | 2/iter | 1/iter | 1 cycle/iter |
| `FILL` inner loop | 1/iter | 2/iter | 1/iter | 1 cycle/iter |
| `MOVE` fwd loop | 1/iter | 2/iter | 1/iter | 1 cycle/iter |
| `MOVE` bwd loop | 1/iter | 2/iter (st.b+dec) | 1/iter (STXD.D) | 1 cycle/iter |

The hot loops (CMOVE/FILL/MOVE) are the highest-value targets: the
per-iteration saving is small but the iteration count can be thousands.
The compiler emit sites save code size more than cycles (the `SEX`
preamble/postamble eats the instruction-count savings, but byte-count
shrinks and the per-byte cycle count halves).

**Strategy:** Convert CMOVE/FILL/MOVE loops first (highest cycle
impact, lowest risk — single `st.b+inc` → `stxi` inside a loop body).
Then convert compiler emit paths in order of frequency:
`compile_call` → `compile_literal` → control flow → CREATE/VARIABLE.

**Estimated effort:** ~3 days total for Phase 7.  Tier 1 (§7a–7d) ~2
days; Tier 2 (§7e) ~1 day.  The MMIO helpers are small but touch 6+
call sites across NIC, disk, and framebuffer subsystems.  Dictionary
scanning touches `find_word` and `parse_word` (~100 lines).  Mechanical
STXI substitution is volume work with low per-site risk.

---

### Phase 8 — Cooperative Multitasking via SEP (PAUSE) ✅ DONE

**Goal:** Implement zero-overhead cooperative multitasking using
SEP to flip between task contexts.

Classic 1802 Forth used SEP as the context-switch mechanism: each task
has a dedicated register holding its continuation address.  `SEP Rn`
switches to task N in 1 cycle with zero memory traffic — no context
save, no stack switch, no scheduler.

#### Current implementation (2-task model)

The production BIOS implements a 2-task model:

```
R3  = Task 0 PC (the REPL / main Forth loop)
R13 = Task 1 PC (a background worker)
```

PAUSE saves Task 0's DSP/RSP, loads Task 1's context, and does
`SEP R13`.  TASK-YIELD (Task 1 → Task 0) is `sep r3; ret.l` — 2
instructions.  This is fully operational with BACKGROUND, TASK-STOP,
and TASK-STATUS dictionary words.

#### Planned upgrade: 4-task round-robin (pending R16–R31)

With the extended register file, the cooperative multitasker expands
from 2 to 4 tasks using dedicated PC registers:

| Register | Task | Purpose |
|----------|------|------|
| R3 | Task 0 | REPL / main Forth loop — always active, non-negotiable |
| R13 | Task 1 | NIC — packet polling, TCP retransmit timers, ARP refresh |
| R18 | Task 2 | Crypto — KEM decapsulation, SHA-3 bulk hashing, long-running ops |
| R19 | Task 3 | User background — display refresh, disk prefetch, general `BACKGROUND` slot |

**Why 4 and not more:**
- Each task needs its own DSP zone (~256 bytes) and RSP zone (~256
  bytes).  4 tasks × 512 bytes = 2 KB.  Trivial.
- Round-robin scan overhead: ~2 cycles per inactive slot skipped.
  Worst case (3 empty): 6 extra cycles.  Typical (2–3 active): 0–2
  extra cycles.  Less than a single `call.l`.
- At 5–6 tasks you're fragmenting stack zones for diminishing returns
  and should use KDOS's software scheduler instead.
- The 2-task model forced NIC, crypto, and user code to compete for
  one slot.  4 slots eliminates that contention entirely.

**Round-robin PAUSE design:**
```asm
w_pause:
    ; ---- Save current task context (DSP, RSP) ----
    ; (same save sequence as today's 2-task PAUSE)
    ...
    ; ---- Advance to next active slot ----
    ldi64 r11, var_task_index
    ldn  r1, r11                ; current task index (0–3)
.next_slot:
    addi r1, 1
    andi r1, 3                  ; mod 4
    ; Load task_pc[r1] — array of 4 PC slots
    ldi64 r11, var_task_pcs
    mov  r0, r1
    lsli r0, 3                  ; ×8 byte offset
    add  r11, r0
    ldn  r13, r11               ; r13 = task_pcs[r1]
    cmpi r13, 0
    breq .next_slot             ; skip inactive slots
    ; ---- Store new task index ----
    ldi64 r11, var_task_index
    str  r11, r1
    ; ---- Load target task context (DSP, RSP) ----
    ...
    ; ---- SEP to target task's PC register ----
    ; Map index → register: 0→R3, 1→R13, 2→R18, 3→R19
    ; (dispatch table or small branch tree)
    ...
```

The scan loop is bounded — at most 3 iterations (can't skip all 4,
since at least Task 0 is always active).  The context save/restore
sequence (~16 instructions) is identical to today's 2-task model.

**TASK-YIELD** becomes task-generic — any task (1, 2, or 3) does
`sep r3` to yield back to the dispatcher, which advances to the
next active slot.

**New dictionary words:**

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BACKGROUND` | `( xt -- )` | Start xt as Task 1 (unchanged) |
| `BACKGROUND2` | `( xt -- )` | Start xt as Task 2 (crypto/general slot) |
| `BACKGROUND3` | `( xt -- )` | Start xt as Task 3 (user slot) |
| `TASK-STOP` | `( n -- )` | Stop task n (1–3); 0 is invalid |
| `TASK-STATUS` | `( n -- flag )` | Return active flag for task n |

Or, unify to `n BACKGROUND` where n selects the slot (0 = error,
1–3 = task slots).  API decision deferred to implementation.

#### Integration with existing infrastructure

- The `worker_xt_table` and IPI mechanism handle **preemptive** dispatch
  to secondary cores.  SEP PAUSE is **cooperative** dispatch on the
  *same* core — complementary, not competing.
- KDOS's timer-based preemption continues to work alongside this.
  `PAUSE` is an explicit yield point; the timer ISR is the involuntary
  one.
- The 4-task model uses R18/R19 from the extended bank — no conflict
  with R16/R17 (Phase 3 NEXT/EXIT).

**Estimated effort:** Current 2-task model: ✅ done.  Upgrade to 4-task
round-robin: ~1 day once R16–R31 land (the extra slots are mechanical;
the round-robin scan is the only new logic).

---

### Phase 9 — MARK/SAV for Fault Handler Recovery

**Goal:** Use the T register and MARK/SAV instructions to improve fault
handler diagnostics.

Currently, `bus_fault_handler` and `priv_fault_handler` re-initialize
R4/R5/R6/R8 from scratch because they can't trust register state after
a fault.  With SEP dispatch active, the T register provides a
lightweight diagnostic:

```asm
bus_fault_handler:
    ; T was saved by MARK at last SEP transition (if MARK was used),
    ; or we can read it via CSR to see which PSEL/XSEL was active.
    csrr r0, 0x08           ; read T register: upper nibble = XSEL,
                            ;                   lower nibble = PSEL
    ; Now we know which SEP context was running when the fault hit.
    ; Print it as part of the diagnostic:
    ldi64 r10, str_fault_ctx
    ...
```

This tells you "the fault happened while R6 was PC (print_hex_byte)"
vs. "the fault happened in main code (R3)" — useful for debugging
SEP-related issues without a full register dump.

**Enhancement for Phase 1+:** Add `MARK` to the SEP entry sequence of
each converted routine:
```asm
emit_char:
    mark                    ; T ← XSEL||PSEL (checkpoint)
    st.b r8, r1
    sep  r3
    br   emit_char
```

Cost: 1 extra byte and 2 cycles per call.  Benefit: T always reflects
the last SEP transition, making fault diagnostics automatic.

**Recommendation:** Add MARK only to debug builds or enable it via a
Forth flag (`SEP-TRACE ON/OFF`).  In production, skip it for the
cycle savings.

**Estimated effort:** ~2 hours.

---

### Phase 10 — Port I/O Bridge for Peripherals (North Star)

**Goal (aspirational):** Map DMA-capable peripherals to 1802-style port
I/O, enabling the most compact possible MMIO sequences.

Family 0x9 (`OUT 1`–`OUT 7`, `INP 1`–`INP 7`) is fully wired in the
RTL but currently has no port-to-MMIO mapping.  A port bridge in the SoC
fabric would let any peripheral appear as a single port number:

| Port | Peripheral | Use |
|------|-----------|-----|
| 1 | UART TX/RX | Character I/O |
| 2 | NIC DMA address | Byte-serial address write |
| 3 | Disk DMA address | Byte-serial address write |
| 4 | CRC data input | Stream bytes into CRC engine |
| 5 | Framebuffer cmd | Blit/DMA setup |

With port mapping, DMA address serialization becomes:
```asm
; NIC DMA address: serialize R9 as 8 LE bytes via port 2
    sex  r9                 ; R(X) = source of address bytes
    out  2                  ; port_out[2] ← M(R(X)); R(X)++
    out  2
    out  2
    out  2                  ; 4 bytes sent in 4 instructions
    out  2
    out  2
    out  2
    out  2                  ; 8 bytes total — no shift, no temp reg
```

That's **8 bytes, 8 cycles, zero register pressure** for the entire
DMA address write — vs. ~30 instructions today.

For UART, character output becomes the purest possible 1802:
```asm
    sex  r10                ; R(X) = string pointer
    out  1                  ; port_out[1] ← M(R(X)); R(X)++
    out  1                  ; 2 chars in 2 instructions, auto-inc
```

**Why this is aspirational:** It requires an RTL change to add a
port-address-to-MMIO bridge in the SoC interconnect.  The port I/O
decode logic exists in `mp64_cpu.v` but the bus fabric doesn't route
port writes to peripheral registers.  This is a hardware change, not
just a BIOS change.

**What it would enable:**
- DMA address writes collapse to `OUT n` chains — 1 byte per byte sent
- UART `emit_char` becomes a 3-instruction, 3-byte routine (SEX+OUT+SEP)
- CRC computation streams data via `OUT 4` with auto-increment
- String output loops can be unrolled to `OUT 1` chains for short
  fixed strings
- Combined with Phase 7's SEX+D for non-port paths, the entire I/O
  layer runs in pure 1802 style

**Estimated effort:** ~2 days RTL work (port bridge), ~1 day BIOS
refactor.  Listed here as an aspirational north star for where the
full 1802 restoration leads.

---

## Phase Summary

| Phase | Scope | Effort | Risk | Value | Status |
|-------|-------|--------|------|-------|--------|
| **0** | Audit + test harness | 1 day | None | Foundation | ✅ Done |
| **1** | R4/R5/R6 leaf SEP | 2 hours | Low | Medium — proves concept, removes stack traffic for leaf I/O | ✅ Done |
| **2** | `print_str` coroutine | 4 hours | Medium | Low — high churn, modest gain | ⏭ Skip |
| **3** | Hybrid STC: SEP NEXT/EXIT (R16/R17) | 2 days | Medium | High — 3 bytes/call saved, dispatch hooks for tracing/profiling | ✅ Done |
| **4** | Q semaphore | 30 min | None | Niche — useful for multicore debug | ✅ Done |
| **5** | Secondary core SEP | 1 hour | Low | Medium — smaller stack zones | ✅ Done |
| **6** | Full SCRT | 3 days | High | Low unless 1802 compat is a goal | ⏭ Skip |
| **7** | SEX + D byte processing | 2 days | Medium | **Highest** — compresses NIC/disk/FB DMA serialization by ~50%, activates MEMALU, tightens dict scan.  16 routines converted to `STXI`/`STXD.D` chains. | ✅ Done |
| **8** | Cooperative PAUSE | 4 hours | Low | Medium — zero-cost green threads on core 0.  2-task model done; 4-task round-robin (R18/R19) pending extended registers. | ✅ Done (2-task); ⏳ 4-task pending |
| **9** | MARK/SAV fault diagnostics | 2 hours | None | Low — debug aid, optional | ✅ Done |
| **10** | Port I/O bridge | 3 days | High | Collapses DMA writes to OUT chains via remap CSR at 0x880 | ✅ Done |

**Completed:** Phases 0, 1, 3, 4, 5, 7, 8, 9, 10 — all done and tested.
The SEP dispatch infrastructure, Q semaphore, hybrid STC JIT (R16/R17),
STXI byte processing, cooperative multitasking, fault diagnostics, and
port I/O bridge are all in production.

**Phase 3 details:** compile_call shrunk from 13 → 10 bytes
(sep r16 + 8-byte inline XT).  compile_ret changed from 1 byte (ret.l)
to 2 bytes (sep r17).  NEXT handler at R16 reads the inline XT,
pushes the return address onto RSP, and dispatches via sep r3.
EXIT handler at R17 pops RSP into R3 and resumes.  JIT inline table,
peephole optimizer, and bigram fusion are completely unchanged.
bytes_saved baseline recalibrated from 13 to 10.  does_runtime
updated to skip 2-byte compile_ret.  All 1697 tests passing.

**Skipped:** Phases 2, 6 — low value relative to churn.

**North star achieved:** Phase 10 completed the 1802 restoration at
the hardware level.  OUT/INP instructions now route through a
configurable remap CSR (MMIO 0x880–0x88F) to arbitrary MMIO device
registers, collapsing DMA address writes to `OUT n` chains.

---

## Register Map — Before and After

```
         CURRENT                  AFTER PHASE 1            AFTER PHASES 3+7+8
  R0   scratch / CSR      R0   scratch / CSR       R0   scratch / CSR
  R1   scratch / arg      R1   scratch / arg       R1   scratch / arg
  R2   ram_size (XSEL=2)  R2   ram_size (XSEL=2)   R2   ram_size (XSEL=2 default)
  R3   PC (PSEL=3)        R3   PC (PSEL=3)         R3   PC / Task 0 (PSEL=3)
  R4   → emit (call.l)    R4   → emit (SEP)        R4   → emit (SEP)
  R5   → key  (call.l)    R5   → key  (SEP)        R5   → key  (SEP)
  R6   → hex_byte         R6   → hex_byte (SEP)    R6   → hex_byte (SEP+D)
  R7   scratch             R7   scratch              R7   scratch
  R8   UART base           R8   UART base            R8   UART base
  R9   word pointer        R9   word pointer         R9   word pointer
  R10  string pointer      R10  string pointer       R10  string pointer
  R11  scratch / temp      R11  scratch / temp       R11  scratch / temp
  R12  scratch / counter   R12  scratch / counter    R12  scratch / counter
  R13  scratch / temp      R13  scratch / temp       R13  Task 1 PC (Phase 8)
  R14  DSP                 R14  DSP                  R14  DSP
  R15  RSP                 R15  RSP                  R15  RSP
  ---  (extended bank, Phase 3)  ---                  ---
  R16  —                   —                         → NEXT (SEP, Phase 3)
  R17  —                   —                         → EXIT (SEP, Phase 3)
  R18  —                   —                         Task 2 PC (Phase 8, 4-task)
  R19  —                   —                         Task 3 PC (Phase 8, 4-task)
  R20–R31  —               —                         available / scratch
```

---

## RTL / Emulator Considerations

- **Emulator (`megapad64.py`):** Must correctly implement `SEP Rn` as
  `PSEL ← n` with the next fetch from `R[n]`.  Verify R[old_PSEL]
  freezes at PC+1 (byte after the SEP instruction).
- **RTL (`mp64_cpu.v`):** The fetch stage already muxes PC from
  `R[PSEL]`.  SEP just writes PSEL — verify there's no 1-cycle stall
  or pipeline bubble on PSEL change.
- **FPGA (`synth_yosys_soc.tcl`):** No changes needed — SEP is already
  synthesized as part of family 0xA decode.
- **Micro-cores (`mp64_cpu_micro.v`):** No changes.  Micro-cores lack
  family 0xA/0xB decode entirely and will trap `ILLEGAL_OP` on SEP/SEX.
  Code that runs on micro-cores (e.g., tile-engine worker kernels
  dispatched via IPI) must not use SEP dispatch — keep `call.l`/`ret.l`
  for any shared routines that micro-cores may invoke.

---

## Testing Strategy

1. **`SEP-SMOKE`** — minimal round-trip: load R4, SEP R4, body does
   SEP R3, verify return.
2. **`IO-BENCH`** — cycle-count before/after for emit_char hot loop.
3. **`DMA-BENCH`** — cycle-count NIC NET-SEND before/after Phase 7.
   Measures the MMIO DMA address serialization path directly:
   `: DMA-BENCH  PERF-CYCLES@  256 0 DO  PAD 64 NET-SEND  LOOP  PERF-CYCLES@ SWAP - . ;`
4. **`DISK-BENCH`** — cycle-count `disk_read_sectors` for a
   multi-sector read before/after Phase 7.  Isolates the DMA setup
   overhead from actual disk I/O latency.
5. **Existing test suite** — `test_megapad64.py` covers UART output,
   dictionary operations, and JIT.  All must pass unchanged.
6. **Multicore smoke** — verify secondary cores can still print debug
   output after Phase 5.
7. **JIT regression** — `JIT-STATS` output and compiled code size
   must improve or hold steady after Phase 3.
