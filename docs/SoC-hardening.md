# SoC Hardening Roadmap

Status: in-progress (§0 STXI DONE, §2 string engine DONE, §3 dict engine DONE, §4e bitfield ALU DONE, §5 port I/O bridge DONE, §7 WOTS+ DONE, §9 bus timeout DONE, §10 BIOS lock guards DONE; §1 SHA-512 spec'd; Appendix B crypto ISA: CRC full migration DONE (MMIO removed), Field ALU ISA DONE (emulator + RTL + tests) — 53/58 items done)  
Last updated: 2026-03-09

---

## Guiding Principle: Don't Reinvent Commodity Silicon

Standard peripherals (USB, Ethernet, WiFi/BLE, audio codecs, SD/eMMC) are
solved problems available as $2–8 chips with decades of certification and
silicon maturity.  The FPGA SoC should expose thin MMIO shims (32–64 bytes
each, SPI/I2S/ULPI bridges) to talk to external chips — not burn fabric on
protocol state machines a commodity part does better.

The MMIO map should be filled with things that make *this specific
architecture* fast: Forth-native acceleration, tight-coupled crypto, and
primitives no off-the-shelf MCU provides.

---

## Topology Principle: Per-Core vs Shared

Chip configurations vary (core count, cluster count, micro-cores per
cluster are all parameters).  The topology rules below apply regardless
of the specific config.

**Rule of thumb — updated:** Crypto primitives that map naturally to the
core's existing register model (CRC, SHA-2, field arithmetic) belong
**in the core as ISA instructions** (see Appendix B, EXT.CRYPTO prefix FB).
The bus round-trip and serialisation cost of sharing is unacceptable when
multiple cores run parallel TLS or signature verification.  Accelerators
with large private state that doesn't fit core registers (SHA-3/Keccak
1600-bit state, AES key schedule, NTT polynomial arrays) or multi-phase
protocols (KEM) stay **shared MMIO** — software mutex via mailbox.
High-frequency per-word operations (string ops, dictionary search) are
ISA extensions in the core (see §2, §3).  Compression/decompression
and pattern matching are shared — long-running, one owner at a time.

### Uniform cluster wrapping

Multi-core clusters each get private engines via their internal arbiter.
Bare big cores would have to share through the congested main bus — an
unfair asymmetry.

**Fix: wrap every core in `mp64_cluster #(.N(1))`.**  When N=1 the
internal arbiter degenerates to a passthrough (zero overhead), but the
wrapper still provides the local address space for private engines.
Every core — big or micro — lives inside a cluster and gets identical
private accelerator access.

SoC bus master count stays the same, nothing else changes.

### The wrapper as a general-purpose per-core shell

Once every core has a wrapper, it becomes the natural home for
anything per-core that shouldn't traverse the main bus:

- **Trace tap** — per-core ring buffer (see §4f)
- **Per-core timer / watchdog** — local tick counting without
  contending on the shared timer
- **Local IPI slot** — fast-path inter-core signalling that skips
  the mailbox for intra-cluster messages
- **Performance counters** — cycle count, stall count, cache miss
  count; per-core by nature, useless if shared
- **MPU / privilege state** — already partly inside `mp64_cluster.v`;
  the wrapper is where this belongs

Area cost per cluster is modest (~150 LUTs + ~1 BRAM for trace +
timers + counters).  String and dictionary engines moved to ISA
extensions (see §2, §3) — no cluster arbiter ports needed.
The existing scratchpad already in each cluster costs more.
Not a resource concern on any target.

| Category           | Topology               | Rationale                            |
|--------------------|------------------------|--------------------------------------|
| CRC32/CRC64        | **Per-core ISA + cluster-shared (hw lock)** | 1-cycle combinational; MMIO removed, cluster arbiter for micro-cores |
| SHA-256/384/512    | **Per-core ISA (EXT.CRYPTO FB)** | 64–80 round compute; parallel TLS needs per-core |
| Field ALU (multi-prime) | **Per-core ISA (EXT.CRYPTO FB)** | Inner-loop field ops; 3-cycle bus overhead per op is 4× compute |
| AES-256-GCM, SHA-3/SHAKE, NTT, KEM, WOTS+ | Shared (1 instance) | Large state / multi-phase protocols |
| TRNG, RTC          | Shared                 | Singular noise source / global clock |
| String engine       | **Per-core ISA (EXT.STRING F9)** | 2–3 byte instruction; bus setup ≫ transfer |
| Dictionary search   | **Per-core ISA (EXT.DICT FA)** | 2–4 cycle FIND; bus latency kills it |
| Debug/trace unit    | **Per-core**           | Meaningless if interleaved           |
| Bitfield ops        | **Per-core ISA (C8–CF)** | Single-cycle; bus latency kills it |

---

## 0. STXI / STXD.D Instructions + IO OUT Bug Fix

**Priority: CRITICAL — do first, everything else depends on byte-store primitives**
**Topology: CPU-internal (no MMIO, no bus, zero-overhead ISA change)**
**Status: DONE** — RTL, emulator, assembler, and smoke tests completed.

### Problem

The ISA has no byte-width auto-increment store.  Every byte serialisation
in the BIOS is a 2-instruction / 3-byte sequence: `st.b rN, rS; inc rN`.
The Forth compiler alone has ~194 static instances and ~12 hot loops using
this pattern.  Every compiled word, every literal, every control-flow
structure, every CMOVE/FILL/MOVE, every MMIO DMA setup pays the tax.

Additionally, there is **a latent bug**: MEMALU sub-op 0xF is
double-booked.  IO OUT (FAM_IO, nib 1..7) sets `memalu_sub <= 4'hF`
then enters `CPU_MEMALU_RD`.  LDXA (opcode 0x8F) does the same.
In `CPU_MEMALU_RD` the `if (memalu_sub == 4'hF)` guard fires first
and runs the IO OUT path — LDXA is **dead code** that silently
executes IO OUT to whichever `io_port` was left over from the last
IO instruction.

### New instructions

| Mnemonic | Encoding | Semantics                              | Pipeline    |
|----------|----------|----------------------------------------|-------------|
| STXI     | 0x89     | `M(R(X)) ← D[7:0]; R(X) ← R(X) + 1`  | 1-cycle decode → CPU_MEM_WRITE |
| STXD.D   | 0x8B     | `M(R(X)) ← D[7:0]; R(X) ← R(X) - 1`  | 1-cycle decode → CPU_MEM_WRITE |

Both are 1-byte instructions in the MEMALU family (0x8).
They replace SDB.X (0x89) and SMB.X (0x8B) — subtract-with-borrow
variants that have **zero usage** anywhere in the BIOS or KDOS.
(The two 0x89/0x8B bytes in bios.asm are dictionary header data, not
instructions.)

### Why decode-time bypass

STXI/STXD.D are **pure writes** — they don't read M(R(X)) first.
Every other MEMALU op is read-modify-write (load from M(R(X)), operate,
put result in D).  Forcing a write-only op through the MEMALU_RD
pipeline would waste a bus cycle on a dummy read.

Instead, intercept at decode time (same level as IRX, SHR.D, SHL.D
which already get special-cased before the `default` arm) and jump
straight to `CPU_MEM_WRITE`:

```verilog
// In MEMALU decode, before the default:
4'h9: begin  // STXI — M(R(X)) ← D, R(X) ← R(X)+1
    effective_addr <= R[xsel];
    mem_data       <= {56'd0, D};
    bus_size       <= BUS_BYTE;
    R[xsel]        <= R[xsel] + 64'd1;
    cpu_state      <= CPU_MEM_WRITE;
end
4'hB: begin  // STXD.D — M(R(X)) ← D, R(X) ← R(X)-1
    effective_addr <= R[xsel];
    mem_data       <= {56'd0, D};
    bus_size       <= BUS_BYTE;
    R[xsel]        <= R[xsel] - 64'd1;
    cpu_state      <= CPU_MEM_WRITE;
end
```

Post-increment / post-decrement: `effective_addr` latches the old R(X)
before the ±1 update, matching 1802 convention (STXD was always
post-decrement).

### IO OUT bug fix (simultaneous)

Add a 1-bit `io_out_active` register.  In the FAM_IO decode for
nib 1..7, set `io_out_active <= 1'b1` instead of relying on
`memalu_sub == 4'hF`.  In `CPU_MEMALU_RD`, check `io_out_active`
instead.  Clear it in all other MEMALU entry paths.

This restores LDXA (0x8F) to correct operation: load M(R(X)) into D
and post-increment R(X), without accidentally triggering IO OUT.

### RTL implementation notes

- **Files touched:** `mp64_cpu.v` only
- **New registers:** 1 bit (`io_out_active`)
- **New decode cases:** 2 arms in the MEMALU `case(nib)` block
  (identical structure to existing IRX at 4'hE)
- **Changed condition:** `CPU_MEMALU_RD` guard from
  `memalu_sub == 4'hF` to `io_out_active`
- **SDB.X / SMB.X cases** in `CPU_MEMALU_RD` become unreachable but
  can be left as-is or removed — no functional impact either way
- **Zero new FSM states**, zero pipeline changes, zero timing impact
- **Instruction length:** already 1 byte (MEMALU family), no change
  to `instr_len` logic

### Emulator implementation notes

- Add two cases in the MEMALU dispatch:
  `0x9: mem[R[X]] = D & 0xFF; R[X] += 1`
  `0xB: mem[R[X]] = D & 0xFF; R[X] -= 1`
- Fix the IO OUT path to check a dedicated flag instead of
  `memalu_sub == 0xF`

### Assembler implementation notes

- Add `stxi` and `stxd.d` mnemonics mapping to 0x89 and 0x8B
- Remove `sdb.x` and `smb.x` mnemonics (or alias them to the new ops
  with a deprecation warning)

### BIOS impact audit

*Full site inventory and conversion plan in
[sep-dispatch-roadmap.md](sep-dispatch-roadmap.md), Phase 7.*

**~194 static `st.b + inc` pairs** and **~12 hot loops** across 38+
sites become single STXI instructions.  Highlights:

| Site | Pairs saved | Frequency |
|------|-------------|----------|
| `compile_call` (13-byte code emission) | 13 | every non-inline word |
| `compile_literal` (16-byte emission) | 16 | every literal |
| CREATE / VARIABLE trampolines | ~52 | every definition |
| JIT compact/TRUE literal + lit_fold | 24 | peephole optimizer |
| Control flow (IF/ELSE/LOOP/UNTIL/WHILE/REPEAT/OF) | 42 across 14 routines | every control word |
| `write_mmio_addr8_le` | 7 | every NIC/disk DMA |
| CMOVE / FILL / MOVE / VSCROLL loops | 1 per iteration | core Forth words, hot loops |
| `compile_byte` / `compile_ret` / `C,` | 3 | very frequently |
| Autoboot prefix build + name copy | 7 + N | once at boot |

STXD.D: 1 site — MOVE backward copy loop.  Low count but validates
the instruction exists for stack-grow-down and reverse-fill patterns.

---

## 1. SHA-256/512 Dual-Mode Upgrade

**Priority: high — next crypto task**  
**Topology: per-core ISA (EXT.CRYPTO FB, Appendix B)**

> **Note:** This section was originally written for a shared MMIO
> implementation.  Per the Appendix B decision, the unified SHA-2 engine
> moves into each full core as ISA instructions (SHA.INIT, SHA.ROUND,
> SHA.FINAL, etc.).  The RTL/emulator notes below describe the datapath
> design, which is the same regardless of whether the unit is reached via
> MMIO or ISA decode — only the control interface changes.  The MMIO
> shared instance may be retained temporarily for micro-core access.

### Current state

`mp64_sha256.v` implements FIPS 180-4 SHA-256 only.  64 bytes MMIO at
0x940–0x97F.  Clean FSM: IDLE → LOAD → ROUND → DONE (+ PAD).

### What changes

SHA-256 and SHA-512 are the same Merkle-Damgård construction with different
parameters:

| Parameter        | SHA-256        | SHA-512           |
|------------------|----------------|-------------------|
| Word width       | 32-bit         | 64-bit            |
| Block size       | 64 B (512 b)   | 128 B (1024 b)    |
| Rounds           | 64             | 80                |
| State H[]        | 8 × 32-bit     | 8 × 64-bit        |
| W schedule       | 16 × 32-bit    | 16 × 64-bit       |
| K constants      | 64 × 32-bit    | 80 × 64-bit       |
| Σ/σ rotations    | {2,13,22} etc  | {28,34,39} etc    |
| Digest           | 32 B           | 64 B              |
| Padding length   | 64-bit         | 128-bit           |

The FSM shape is unchanged.  Ch, Maj, Σ, σ have the same logical structure —
only rotation amounts and word width differ.

### Implementation plan

1. **Add a `mode` CTRL register** (like `mp64_sha3.v` already has for its
   4 modes).  INIT loads a different IV per mode.
2. **Widen datapath to 64-bit** — H[], W[], wa..wh all become `[63:0]`.
   In SHA-256 mode the upper 32 bits are dead (zero).
3. **Mux rotation amounts** — Σ/σ functions become `rotr64` with
   mode-selected constants.  A 2:1 mux per rotation amount.
4. **Expand din_buf 64→128 bytes**, din_ptr 7→8 bits.
5. **Expand digest 32→64 bytes**.
6. **Expand K table** — 64×32-bit → 80×64-bit (biggest ROM cost).
   Round counter to 7 bits (max 79).
7. **Padding** — length field goes 64→128-bit; two-block threshold
   shifts from byte 56 to byte 112.

### Modes (CTRL register, 2 bits)

| Mode | Algorithm | IV                | Rounds | Digest bytes |
|------|-----------|-------------------|--------|--------------|
| 0    | SHA-256   | FIPS 180-4 §5.3.3 | 64     | 32           |
| 1    | SHA-384   | FIPS 180-4 §5.3.4 | 80     | 48           |
| 2    | SHA-512   | FIPS 180-4 §5.3.5 | 80     | 64           |
| 3    | reserved  | —                 | —      | —            |

SHA-384 = SHA-512 with a different IV and truncated output.  Zero extra
logic beyond the constant table.

### MMIO window

Widen from 64 to 128 bytes (0x940–0x9BF) to fit 64-byte DOUT linearly,
matching the flat-read pattern in `mp64_sha3.v`.  *(CRC MMIO has been
fully removed — the 0x980 slot is freed.  See Appendix B §B.9.)*

Prefer widening over `result_sel` paging — the digest is the primary
output and shouldn't be behind a page flip.  SHA3 already sets this
precedent.

### Area estimate (7-series)

~300–400 extra FFs, ~150–200 extra LUTs.  Modest.

### Timing note

Critical path widens from 32-bit to 64-bit addition chains through T1.
If timing is tight, a pipeline register splitting T1 into two adds may be
needed; at one-round-per-cycle this is usually fine at 100 MHz.

### RTL implementation notes

The datapath design is the same whether accessed via MMIO or ISA decode:

- Copy `mp64_sha256.v` → rename module to `mp64_sha2` (covers 256/384/512).
- All `[31:0]` regs → `[63:0]`.  In mode==0, INIT zeroes upper bits; the
  existing round logic just operates on 64-bit words (upper bits stay 0).
- K table: `function [63:0] K; input [6:0] i;` — 80 entries.  For mode 0,
  only entries 0–63 are used (round_cnt never reaches 64+).
- Σ/σ mux: `wire [5:0] S0_r0 = (mode==0) ? 6'd2 : 6'd28;` etc.
  Feed into a single `rotr64` function.

For ISA integration (per Appendix B):

- Instantiate `mp64_sha2` inside `mp64_cpu.v` (tightly coupled, like
  the string engine and multiplier).
- Control interface: CPU decode triggers `sha_start` on SHA.ROUND/SHA.FINAL;
  SHA sub-module reads W from tile memory via CPU internal port.
- State (H[0..7]) maps to ACC0–ACC3; mode CSR at 0x82.
- No MMIO bus involvement for full cores.

For MMIO fallback (micro-cores, transition period):

- `addr` port widens from `[5:0]` to `[6:0]`.
- SoC decode change: `addr[11:7] == 5'b10010` (0x940–0x9BF).
- *(CRC decode removed — CRC is now ISA-only.)*

### Emulator implementation notes

For ISA path (full cores, target implementation per Appendix B):

- New dispatch case in `megapad64.py` for EXT.CRYPTO (FB) sub-ops
  0x10–0x1F.  Uses `hashlib.sha256()` / `hashlib.sha384()` /
  `hashlib.sha512()` internally.
- State in ACC0–ACC3; message block in tile memory at TSRC0.
- See Appendix B §B.4 for the full instruction set.

For MMIO fallback (micro-cores, transition):

- `devices.py` SHA256Device class: add `mode` register, branch on mode
  for `hashlib.sha256()` vs `hashlib.sha384()` vs `hashlib.sha512()`.
- Widen `digest` buffer to 64 bytes; DOUT reads at offset 0x18–0x57.
- Update MMIO dispatch range in `system.py`.
- *(CRC base address constant removed — CRC is now ISA-only.)*
- BIOS `SHA256-INIT` word: add optional mode write before CMD=INIT.
  Default mode=0 preserves backward compat.

---

## 2. Forth-Aware String Engine — ISA Extension (EXT.STRING, prefix F9)

**Priority: high — directly accelerates core Forth workloads**  
**Topology: CPU-internal (tightly-coupled sub-module, like MUL/DIV)**  
**Status: DONE** — RTL (`mp64_string.v`), emulator, assembler, BIOS
(CMOVE, CMOVE>, FILL, TFILL, MOVE), and 65 tests (39 RTL + 26 emulator) all passing.

**BIOS adoption status:**

| Forth word | HW sub-op | Status |
|------------|-----------|--------|
| CMOVE      | CMOVE (00)| ✅ uses `cmove` |
| CMOVE>     | CMOVE> (01)| ✅ uses `cmove>` |
| FILL       | BFILL (02)| ✅ uses `bfill` |
| MOVE       | CMOVE/CMOVE> | ✅ uses `cmove`/`cmove>` with overlap detection |
| COMPARE    | BCOMP (03)| ✅ uses `bcomp` with min-length prefix + length tie-break |
| *(SCAN)*   | BSRCH (04)| ⬜ **TODO** — BSRCH is `memchr` (single-byte); Forth SEARCH is substring search, needs wrapper or new sub-op |

Block-move/fill/compare instructions that understand Forth `CMOVE`,
`CMOVE>`, `FILL`, `COMPARE`, `SEARCH` semantics natively — encoded as
3-byte ISA instructions, not MMIO peripherals.

### Motivation

Bulk memory operations are among the most frequent hot loops in a Forth
system — dictionary copying, screen buffer fills, block transfers.
Today these run as CPU byte-loops.

**ISA approach:** operands live in GPRs (which we now have 32 of).
A single 3-byte instruction replaces the entire MMIO dance.  The CPU
stalls for the transfer duration (like DIV), using its existing bus
master port — no arbiter changes, no cluster wrapper ports.

### Why ISA instead of MMIO

| Aspect | MMIO peripheral | ISA extension |
|--------|----------------|---------------|
| Setup cost | 4–6 writes (~40 bytes) | 0 (operands in GPRs) |
| Instruction size | 1 byte (CMD write) + setup | 3 bytes total |
| Latency overhead | ~20 cycles setup + transfer | Transfer only |
| Bus contention | Needs arbiter port | Uses CPU's own master |
| Cluster wrapper | Arbiter + address decode | Nothing added |
| Code density | Poor for short ops | Excellent |

This follows the same principle as §4e (bitfield ops): operations that
complete in fewer cycles than the MMIO setup would take belong in the
ISA, not behind the bus.

### Encoding: EXT.STRING (prefix F9)

Three-byte instructions: `F9 <sub-op> <reg-byte>`.

The reg-byte encodes two 4-bit register fields: `DR` = `Rd[3:0] : Rs[3:0]`
or `DN` = `Rd[3:0] : Rn[3:0]`, depending on sub-op.  With a preceding
REX prefix, these extend to 5-bit register indices (R0–R31).

| Sub-op | Mnemonic | Encoding | Semantics |
|--------|----------|----------|-----------|
| 00 | CMOVE | F9 00 DR | Copy Rs→Rd, len=R0; forward (low→high) |
| 01 | CMOVE> | F9 01 DR | Copy Rs→Rd, len=R0; backward (high→low, overlap-safe) |
| 02 | BFILL | F9 02 DN | Fill Rd with D[7:0], len=Rn |
| 03 | BCOMP | F9 03 DR | Compare Rs vs Rd, len=R0; sets Z (equal) and G (greater) flags |
| 04 | BSRCH | F9 04 DR | Search Rd[0..R0-1] for D[7:0]; result offset in Rs, Z=found |
| 05–0F | *(reserved)* | | Future: word-width ops, pattern fill, etc. |

**Register conventions:**
- Rd = destination/haystack address register
- Rs = source/result register
- R0 = implicit length for CMOVE/CMOVE>/BCOMP/BSRCH (Forth TOS convention)
- Rn = explicit length register for BFILL
- D[7:0] = fill/search byte (accumulator, Forth-natural)

**Execution model:**
- CPU enters a multi-cycle stall state (like `CPU_MULDIV`).
- String sub-module issues bus reads/writes through the CPU's existing
  bus master — 64-bit aligned where possible, byte fix-up at head/tail.
- CMOVE increments; CMOVE> decrements from end (overlap-safe).
- BCOMP short-circuits on first mismatch.
- Rd, Rs, R0 are updated in-place after completion (post-transfer
  pointers, matching Forth `CMOVE` stack effect).

### Micro-core behaviour

Micro-cores lack the string sub-module.  F9 xx traps as `ILLEGAL_OP`.
Micro-core Forth kernels fall back to byte-loop definitions of CMOVE
et al. (already required for the no-hardware case).

### RTL implementation notes

- New sub-module `mp64_string.v`, instantiated inside `mp64_cpu.v`
  alongside the multiplier (tightly coupled, not bus-attached).
- Interface: `start`, `op[3:0]`, `src_addr`, `dst_addr`, `length`,
  `fill_byte`, `done`, `result`, bus master signals.
- Simple FSM: IDLE → TRANSFER (read/write per beat) → DONE.
- 64-bit aligned bulk path with byte fix-up at head/tail.
- CPU decode: when `ibuf[0] == 8'hF9`, enter `CPU_STRING` stall
  state.  On `string_done`, resume fetch.
- Area: ~200 LUTs + ~50 FFs per CPU instance.  No BRAM.
- Big-core only: gated out in micro-core config (`generate if`).

### Emulator implementation notes

- New dispatch case in `megapad64.py` CPU loop for opcode 0xF9.
- Sub-op switch: Python `memory[dst:dst+ln] = memory[src:src+ln]`
  (CMOVE), `bytearray` ops for COMPARE/SEARCH.
- Update GPRs in-place to match post-transfer state.
- Micro-core flag: raise `ILLEGAL_OP` trap if `self.is_micro`.

### C++ accelerator — DONE (2026-03-09)

All five EXT.STRING sub-ops (CMOVE, CMOVE>, BFILL, BCOMP, BSRCH)
execute natively in `mp64_accel.cpp::exec_string()`.  BFILL has a
`memset` fast-path when the target is contiguous RAM (not MMIO).
No Python fallback needed.  1715/1715 tests pass.

---

## 3. Forth Dictionary Search Engine — ISA Extension (EXT.DICT, prefix FA)

**Priority: high — unique competitive advantage**  
**Topology: CPU-internal (per-CPU BRAM hash table)**

A hardware-accelerated `FIND` encoded as a 2–3 byte ISA instruction,
replacing linked-list traversal with a 2-cycle hash lookup.

### Motivation

Dictionary search is the single most frequent operation during Forth
compilation and interpretation.  Every word typed at the console walks
the dictionary linked list — O(n) in vocabulary size.  With a ~500-word
BIOS dictionary, that's hundreds of byte-comparisons per lookup.

No other Forth system has this in hardware.

### Why ISA instead of MMIO

The original MMIO plan required writing a counted string into a DIN
buffer, then writing a CMD byte, then polling STATUS, then reading
XT_OUT — at least 35+ bytes of setup for a 2-cycle operation.

As an ISA instruction, DFIND is 3 bytes total: `FA 00 DR`.  The
counted-string address is already in a GPR; the XT result goes into
another GPR.  The CPU stalls for 2 cycles (hash + compare) — less
time than a single MMIO write would take through the bus.

This follows the same principle as §4e and §2: if the operation is
faster than the MMIO setup overhead, it belongs in the ISA.

### Why per-CPU BRAM (not shared)

If all threads are interpreting/compiling Forth, they all call `FIND`
on every word.  A single-ported hash table becomes a serialisation
point on the hottest path.

Each CPU has its own BRAM-backed hash table (~4 BRAM36).  INSERT
broadcasts to all copies via a lightweight sideband bus (infrequent;
during compilation only).  FIND is entirely CPU-local — zero bus
traffic, zero contention.

### Encoding: EXT.DICT (prefix FA)

Two- or three-byte instructions: `FA <sub-op>` or `FA <sub-op> <reg-byte>`.

| Sub-op | Mnemonic | Encoding | Semantics |
|--------|----------|----------|-----------|
| 00 | DFIND | FA 00 DR | Rs=counted-string addr → Rd=XT; Z=found |
| 01 | DINS  | FA 01 DR | Insert: Rs=name addr, Rd=XT to store |
| 02 | DDEL  | FA 02 0R | Delete entry by name at Rs |
| 03 | DCLR  | FA 03    | Clear entire hash table (2-byte instruction) |
| 04–0F | *(reserved)* | | Future: vocab select, iteration, stats |

**Register conventions:**
- Rs = address of counted string (name to find/insert/delete)
- Rd = XT result (DFIND) or XT to store (DINS)
- Z flag = found (DFIND), success (DINS/DDEL)
- Overflow flag = all ways full on INSERT (software falls back to
  linked-list for that word)

**Execution model:**
- DFIND: 2-cycle stall — cycle 1 hashes the name (read from memory
  via CPU bus master), cycle 2 reads all 4 ways and compares.
- DINS: 3–4 cycles — hash + find empty way + write entry.
- DDEL: 2 cycles — hash + invalidate matching entry.
- DCLR: 1 cycle — bulk-zero all valid bits.

### Hash table structure

- 256-entry, 4-way set-associative.
- Each entry: 32-bit hash + 64-bit NFA + 64-bit XT + 5-bit name_len
  + 31-byte name — fits in ~4 BRAM36 blocks per CPU.
- hash[7:0] selects set (64 sets × 4 ways).
- On FIND, all 4 ways are read and compared in parallel.
- On INSERT collision (all 4 ways full), overflow flag is set;
  software falls back to linked-list search for that word.

### INSERT broadcast

When any CPU executes DINS, the SoC fans the entry out to all other
CPUs' hash tables via a shared `dict_insert_broadcast` sideband bus
(hash + NFA + XT + name, active for 1 cycle).  Each CPU's dict engine
snoops the broadcast and writes the entry into its own table.

Broadcast is infrequent (only on new definitions during compilation),
so bus bandwidth is negligible.  A simple valid + ack handshake
prevents data loss if two CPUs INSERT on the same cycle.

### Open questions

- Vocabulary support: ALSO/ONLY search order means multiple logical
  tables or a priority chain.  Could use hash tag bits to encode
  vocabulary ID, or a small vocab-select CSR.
- Forgetting words (FORGET/MARKER) needs DDEL or bulk invalidation.
  DCLR + re-insert from linked list is the safe fallback.
- Hash function: FNV-1a or CRC-based?  Must be cheap in gates (~30
  LUTs) and deterministic across all copies.

### Micro-core behaviour

Micro-cores lack the dict BRAM.  FA xx traps as `ILLEGAL_OP`.
Micro-cores are not expected to run the Forth outer interpreter;
if needed, they use software linked-list FIND.

### RTL implementation notes

- New sub-module `mp64_dict.v`, instantiated inside `mp64_cpu.v`
  (tightly coupled, like the string engine and multiplier).
- Interface: `start`, `op[3:0]`, `name_addr`, `name_len`, `xt_in`,
  `done`, `xt_out`, `found`, `overflow`, bus master signals for
  name read, BRAM ports for hash table.
- 4 parallel comparators (one per way) for single-cycle match.
- INSERT broadcast: sideband port out (to SoC fabric) + snoop port
  in (from SoC fabric).  ~20 wires each direction.
- CPU decode: when `ibuf[0] == 8'hFA`, enter `CPU_DICT` stall state.
  On `dict_done`, resume fetch, write Rd and flags.
- Area: ~300 LUTs + ~4 BRAM36 per big-core instance.  Zero for
  micro-cores (gated out with `generate if`).

### Emulator implementation notes

- New dispatch case in `megapad64.py` CPU loop for opcode 0xFA.
- Backed by a Python `dict` mapping name_bytes → (NFA, XT).
  DFIND: `self.dict_table.get(name, None)` — O(1), accurate model.
  DINS: `self.dict_table[name] = (nfa, xt)`.
  DDEL: `del self.dict_table[name]`.
  DCLR: `self.dict_table.clear()`.
- Set Z flag and Rd register from result.
- Micro-core flag: raise `ILLEGAL_OP` trap if `self.is_micro`.
- Single instance per emulated CPU is fine (no contention in
  single-threaded Python).

### C++ accelerator — DONE (2026-03-09)

All four EXT.DICT sub-ops (DFIND, DINS, DDEL, DCLR) execute natively
in `mp64_accel.cpp::exec_dict()`.  The C++ `CPUState` carries the
64×4 hash table (`DictEntry dict_table[64][4]`) with inline FNV-1a
hashing — no Python fallback needed.  `accel_wrapper.py::_reset_state()`
calls `dict_clear()`.  1715/1715 tests pass.

**Implementation status across layers:**

| Layer | Status | Notes |
|-------|--------|-------|
| RTL (`mp64_dict.v`) | ✅ Complete | 490 lines, 4-way SA, FNV-1a, BRAM |
| Python emulator (`megapad64.py`) | ✅ Complete | 64×4 hash table, FNV-1a 32-bit |
| C++ accelerator (`mp64_accel.cpp`) | ✅ **Complete** | Native 64×4 hash table, FNV-1a 32-bit |
| Tests (`test_megapad64.py::test_ext_dict`) | ✅ Complete | 7 sub-tests, native C++ path |
| BIOS usage | ✅ Active | `find_word` → DFIND fast path; DINS on cache miss |

---

## 4. Other Accelerator Ideas & Committed ISA Extensions

§4e (Bitfield ALU) is a **committed ISA extension** with full encoding
in Appendix A.4 — it is not speculative.  The remaining items (4a–4d,
4f) are lower-priority ideas worth keeping on the radar.

### 4a. Crypto Pipeline Orchestrator (~32 bytes MMIO, shared)

A small config register that chains existing crypto blocks in hardware:
data flows CRC → SHA → AES without the CPU touching each byte.  Turns
three sequential MMIO-per-byte passes into one.

**RTL:** Small state machine with mux between existing peripheral data
ports.  Wire as an internal bus between SHA/AES/CRC rdata/wdata.
**Emulator:** Python class that calls the existing device objects in
sequence.

### 4b. Hardware Compress/Decompress — Deflate (~128 bytes MMIO, shared)

Huffman + LZ77 in hardware for inflate/deflate.  Every filesystem,
network protocol, and image format uses this.  Software zlib on any
CPU is painfully slow; hardware is 10–100×.

Input FIFO, output FIFO, dictionary window pointer, status.

**RTL:** Significant module (~3K–5K LUTs).  LZ77 sliding window in
BRAM (32 KB), Huffman tree in LUTs.  Well-documented open-source cores
exist (e.g., gzip-fpga).
**Emulator:** `zlib.compress()` / `zlib.decompress()` behind MMIO.
Trivial.

### 4c. Pattern Matching / Regex NFA Engine (~64 bytes MMIO, shared)

A small NFA engine: feed a compiled pattern + stream bytes, get
match/no-match.  Useful for parsing, protocol dispatch, search.
Almost no commercial chip offers this.

**RTL:** Bit-parallel NFA — one FF per NFA state, one cycle per input
byte.  Compile regex → state vector offline.  ~200 LUTs for 64-state
NFA.
**Emulator:** `re.match()` behind MMIO.

### 4d. Fixed-Point DSP / FIR Filter (~64–128 bytes MMIO, shared)

A multiply-accumulate array for audio/signal processing.  The ISA
lacks SIMD, so a hardware MAC with coefficient table does FIR/IIR/FFT
butterflies at wire speed.  Useful if audio or radio becomes a target.

**RTL:** Reuse DSP48E1 slices (Kintex-7 has 840).  Coefficient BRAM +
accumulator + control FSM.
**Emulator:** NumPy `convolve()` or manual MAC loop.

### 4e. Bitfield ALU — DONE (Family 0xC, sub-ops C8–CF) (2026-03-09)

**Status: DONE — C++ accelerator, Python emulator, RTL (`mp64_bitfield.v`), assembler, BIOS words, tests, micro-core tiering**  
**Topology: per-core ISA (MULDIV family 0xC, 1-cycle combinational)**

Single-cycle bit-manipulation operations encoded as sub-ops C8–CF in
the MULDIV family.  **Not MMIO** — single-cycle operations don't
survive the bus round-trip.  Wired into `mp64_alu.v` combinational
logic; 1-cycle execution (no stall, unlike MUL/DIV's 4 cycles).
See Appendix A.4 for the complete encoding spec.

Tiny footprint, big payoff for memory allocators, graphics blitters,
crypto, and Forth internals (dictionary hashing, bitmap free-lists).

#### Tier 1 — Universal, ~30 LUTs each

| Mnemonic | Operation | Semantics | Use case |
|----------|-----------|-----------|----------|
| POPCNT | Population count | D ← popcount(Rs) | Allocators, Hamming weight, crypto |
| CLZ | Count leading zeros | D ← 63 − Rs.bit_length() (0→64) | LOG2, normalisation, priority encode |
| CTZ | Count trailing zeros | D ← ctz(Rs) (0→64) | Find-first-set, allocator free-list |
| BITREV | Reverse bit order | D ← Rs[0]..Rs[63] | CRC, FFT butterfly, endian tricks |

#### Tier 2 — High-value, ~50–100 LUTs each

| Mnemonic | Operation | Semantics | Use case |
|----------|-----------|-----------|----------|
| BEXT | Bit extract (gather) | D ← pext(Rs, Rd) — collect bits at mask positions | Bitfield unpacking, pixel channel extract |
| BDEP | Bit deposit (scatter) | D ← pdep(Rs, Rd) — deposit bits at mask positions | Bitfield packing, Morton codes |
| RORI | Rotate right imm | D ← rotr(Rs, imm6) | Crypto rotations (SHA in software), hashing |
| BSWAP | Byte-swap | D ← endian_reverse(Rs) | Network byte order, file format parsing |

#### Encoding

All 8 ops fit as new sub-op values in the ALU family's function
select mux.  The ALU family already has unused function codes — these
slot in without any structural change to instruction decode.

Exact sub-op assignments TBD when the ALU function map is finalised,
but the pattern is: 2-byte instruction `7F <func>` where `<func>`
selects the bitfield operation, or reuse the existing ALU DR encoding
for ops that need two register operands (BEXT, BDEP).

RORI needs a 6-bit immediate for the rotation amount — either steal
from the reg-byte (top 2 bits unused in single-operand form) or use
a 3-byte encoding with an explicit immediate byte.

#### Area estimate

| Op | LUTs (64-bit, 7-series) | Notes |
|----|------------------------|-------|
| POPCNT | ~30 | Wallace tree adder |
| CLZ | ~30 | Priority encoder |
| CTZ | ~30 | BITREV + CLZ, or direct |
| BITREV | ~10 | Pure wiring (zero logic) |
| BEXT | ~80 | Iterative or parallel gather |
| BDEP | ~80 | Iterative or parallel scatter |
| RORI | ~10 | Barrel shifter (reuse existing) |
| BSWAP | ~0 | Pure wiring |
| **Total** | **~270** | Fits easily in any target |

BITREV and BSWAP are pure wire permutations — literally zero LUTs.
RORI reuses the existing barrel shifter with a rotate mode bit.
The real cost is BEXT/BDEP; if area is tight on micro-cores, those
two can be gated out (micro-cores unlikely to need scatter/gather).

#### RTL implementation notes

- Add cases to `mp64_alu.v` function select.  Wire results into
  existing ALU output mux — same pattern as ADD/SUB/AND/OR.
- POPCNT: `function [6:0] popcount; input [63:0] x;` — standard
  Wallace tree, well-optimised by synthesis.
- CLZ: priority encoder with `casez` on leading bits, or use
  `$clog2`-style tree.
- BEXT/BDEP: bit-serial loop (combinational unrolled) or RISC-V
  Zbs-style parallel prefix network.  Parallel is faster but wider.
- RORI: existing shifter + `rotate` control bit + imm6 source mux.
- Big-core: all 8 ops.  Micro-core: Tier 1 only (POPCNT, CLZ, CTZ,
  BITREV) — ~100 LUTs.  Tier 2 gated out with `generate if`.

#### Emulator implementation notes

- New sub-op cases in `megapad64.py` ALU dispatch:
  - `POPCNT`: `bin(x).count('1')`
  - `CLZ`: `64 - x.bit_length()` (handle 0 → 64)
  - `CTZ`: `(x & -x).bit_length() - 1` (handle 0 → 64)
  - `BITREV`: `int(f'{x:064b}'[::-1], 2)`
  - `BEXT`: RISC-V-style pext loop
  - `BDEP`: RISC-V-style pdep loop
  - `RORI`: `((x >> n) | (x << (64-n))) & MASK64`
  - `BSWAP`: `int.from_bytes(x.to_bytes(8, 'little'), 'big')`

#### BIOS word additions

The bitfield ALU enables three groups of new BIOS Forth words that
replace current software workarounds with single-instruction
primitives.

##### 1. Bitmap Pool Allocator (uses POPCNT, CTZ)

A lightweight fixed-slot allocator for pools of identical objects
(file descriptors, arena descriptors, network buffers).  Each pool
is a 64-bit bitmap word — one bit per slot.  This directly accelerates
the existing filesystem sector bitmap (§filesystem.md) and provides
a general-purpose facility for any bounded-size resource pool.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `POOL-ALLOC` | `( bitmap -- bitmap' index )` | Find lowest free bit via CTZ, set it, return updated bitmap and slot index.  Abort if bitmap = ~0 (pool full). |
| `POOL-FREE` | `( bitmap index -- bitmap' )` | Clear bit at *index*, return updated bitmap.  Abort if bit was already clear (double-free). |
| `POOL-COUNT` | `( bitmap -- n )` | Count allocated slots via POPCNT. |
| `POOL-FREE?` | `( bitmap -- n )` | Count free slots: `64 - POPCNT`. |
| `POOL-FULL?` | `( bitmap -- flag )` | True if all 64 slots occupied (`bitmap = ~0`). |

**Implementation sketch (Forth + inline bitfield ops):**

```forth
: POOL-ALLOC  ( bitmap -- bitmap' index )
  DUP INVERT            \ free-mask
  DUP 0= ABORT" pool full"
  CTZ                   \ index of lowest free bit
  TUCK  1 SWAP LSHIFT   \ ( index bitmap bit )
  OR  SWAP ;            \ set the bit, return bitmap' index

: POOL-FREE  ( bitmap index -- bitmap' )
  1 SWAP LSHIFT  INVERT  AND ;

: POOL-COUNT  ( bitmap -- n )  POPCNT ;
```

**Primary consumers:** FS sector bitmap scan (currently a
byte-at-a-time loop), arena descriptor pool, network RX ring slot
tracking.

##### 2. Dictionary Hash Helper (uses RORI)

The EXT.DICT hardware cache uses FNV-1a hashing internally.  Exposing
a matching software hash word lets Forth code pre-compute hashes for
batch lookups, compile-time constant folding, and hash-table data
structures outside the dictionary.  RORI provides the
rotate-XOR-accumulate pattern common to all high-quality non-crypto
hashes.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HASH-NAME` | `( c-addr u -- hash )` | FNV-1a hash of the counted string, matching the EXT.DICT internal algorithm.  Uses RORI for the multiply-by-prime step (shift-add approximation). |
| `HASH-STEP` | `( hash c -- hash' )` | Single-byte hash accumulate: XOR byte into hash, rotate-add.  Building block for user hash tables. |

**Implementation sketch:**

```forth
: HASH-STEP  ( hash c -- hash' )
  XOR  DUP 5 RORI  XOR ;        \ rotate-xor fold

: HASH-NAME  ( c-addr u -- hash )
  $811C9DC5  -ROT                 \ FNV offset basis
  OVER + SWAP DO
    I C@  HASH-STEP
  LOOP ;
```

**Primary consumers:** Compile-time hash pre-computation for `'` and
`[']`, user-level hash tables (e.g., environment query tables),
duplicate-word detection during `MARKER` cleanup.

##### 3. Network Byte-Order Words (uses BSWAP)

The BIOS currently contains ~14 manual big-endian byte-push sequences
(shift-mask chains for writing 16-bit and 32-bit values in network
order).  A single BSWAP instruction collapses each to one cycle.
These words belong alongside the existing NIC category
(NET-SEND / NET-RECV / NET-STATUS / NET-MAC@).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NTOH` | `( x -- x' )` | Network-to-host: 64-bit byte-swap via BSWAP. |
| `HTON` | `( x -- x' )` | Host-to-network: alias of NTOH (byte-swap is self-inverse). |
| `NTOH32` | `( x -- x' )` | 32-bit network-to-host: BSWAP then 32-bit right-shift. |
| `HTON32` | `( x -- x' )` | 32-bit host-to-network: alias of NTOH32. |
| `NTOH16` | `( x -- x' )` | 16-bit network-to-host: BSWAP then 48-bit right-shift. |
| `HTON16` | `( x -- x' )` | 16-bit host-to-network: alias of NTOH16. |

**Implementation sketch:**

```forth
: NTOH   ( x -- x' )  BSWAP ;
: HTON   NTOH ;
: NTOH32 ( x -- x' )  BSWAP  32 RSHIFT ;
: HTON32 NTOH32 ;
: NTOH16 ( x -- x' )  BSWAP  48 RSHIFT ;
: HTON16 NTOH16 ;
```

**Primary consumers:** NIC frame header parsing (EtherType, IP length,
TCP/UDP ports — all big-endian on wire), filesystem metadata
(superblock fields are little-endian but external file-format
interchange may need network order), SHA-256 hardware digest readback
(32-byte big-endian output at MMIO DOUT+0x18).

### 4f. Stack-Machine Debug / Trace Unit (~64 bytes MMIO, per-core)

Always-on flight recorder that snapshots DSP/RSP/TOS on every SEP
dispatch.  Zero performance overhead, invaluable for debugging.
Ring buffer in BRAM, readable via MMIO.

**Must be per-core** — interleaved traces from multiple cores are
useless.  Each core gets its own small ring buffer (~256 entries ×
24 bytes per entry, backed by ~1 BRAM36).

**RTL:** Tap the `sep_dispatch` signal in `mp64_cpu.v`.  On each pulse,
write {PC, TOS, DSP, RSP, timestamp} into a dual-port BRAM ring.  MMIO
reads drain from the read port.  ~1 BRAM36 per core.
**Emulator:** Append to a Python `deque(maxlen=256)` on each SEP
dispatch in `megapad64.py`.  Expose via a virtual MMIO read.

---

## 5. Port I/O Bridge (1802 OUT/INP → MMIO)

**Priority: high — collapses DMA byte-serialization to OUT chains**  
**Topology: shared (pure SoC-fabric routing, no per-core state)**  
**Status: DONE** — RTL remap CSR + combinational decode, emulator
`_exec_io` routing, C++ accelerator `port_map` + `sys_write8`/`sys_read8`,
byte-push registers (NIC DMA_PUSH, Disk DMA_PUSH, CRC DIN_BYTE,
FB BASE_PUSH), BIOS boot-time CSR init, PortBridgeCSR device class,
system integration, 11 tests.

*Cross-ref: SEP dispatch roadmap Phase 10.*

### Motivation

The BIOS 1802 heritage includes family 0x9 port I/O (`OUT 1`–`OUT 7`,
`INP 1`–`INP 7`).  The CPU already decodes these and generates bus
transactions — `OUT n` reads `M(R(X))`, auto-increments R(X), and
writes the byte to address `{MP64_MMIO_HI, 20'b0, port[2:0], 9'b0}`.
But nothing in the SoC fabric routes those addresses to useful
peripherals, so the instructions are dead silicon.

A port bridge maps each port number to a specific MMIO register, so
`OUT n` writes a byte directly to a peripheral with auto-increment —
the most compact possible byte-serial I/O.

### Why it fits here

The CPU-side decode already exists in `mp64_cpu.v` (the `io_port`,
`io_is_inp` registers, and the `effective_addr` computation at the
`CPU_MEMALU_RD` state).  The only missing piece is SoC-fabric routing:
a small address-remap table that translates the port-derived address
into the actual MMIO target register.

This is the same class of work as the other SoC-hardening items
(MMIO decode, bus fabric tweaks) — not a CPU change.

### Proposed port map

| Port | Target peripheral | Target register | Use |
|------|-------------------|-----------------|-----|
| 1    | UART              | TX data         | Character output |
| 2    | NIC               | DMA addr byte   | Byte-serial address write |
| 3    | Disk              | DMA addr byte   | Byte-serial address write |
| 4    | CRC               | Data input      | Stream bytes into CRC |
| 5    | SHA (current)     | DIN             | Stream bytes into hash |
| 6    | Framebuffer       | DMA cmd byte    | Blit/DMA setup |
| 7    | *(configurable)*  | Via remap CSR   | User-defined |

Port 7 as configurable (via a small CSR) allows future peripherals
or soft-peripherals without another RTL change.

### What it enables (BIOS impact)

DMA address serialization — currently ~30 instructions per 64-bit
address write — collapses to:

```asm
    sex  r9            ; R(X) = source of address bytes
    out  2             ; byte 0 → NIC DMA addr
    out  2             ; byte 1
    out  2             ; byte 2
    out  2             ; byte 3
    out  2             ; byte 4
    out  2             ; byte 5
    out  2             ; byte 6
    out  2             ; byte 7
```

8 bytes, 8 cycles, zero register pressure, auto-increment.  Benefits
NIC send/recv, disk read/write, framebuffer DMA, CRC — every
DMA-capable peripheral.

UART emit becomes 3 instructions total:
```asm
    sex  r10           ; R(X) = string pointer
    out  1             ; char to UART, auto-inc
    out  1             ; next char
```

### Current CPU address generation

`mp64_cpu.v` already computes: `effective_addr <= {MP64_MMIO_HI, 20'd0, io_port[2:0], 9'd0}`

This means port N generates address `0xFFFF_FF00_0000_0000 + N×0x200`.
In the SoC fabric (which decodes `bus_mmio_addr[11:0]`), this maps to
offset `N×0x200`: port 1 → 0x200, port 2 → 0x400, etc.

The port address currently hits existing peripherals by coincidence
(port 1 → 0x200 = Disk, port 2 → 0x400 = NIC) but targets the
*base* of each peripheral, not the specific DMA byte-input register.

### RTL implementation notes

Two options, both small:

**Option A — Address remap table in SoC fabric (~50 LUTs):**
Add a small combinational remap in `mp64_soc.v` between the bus
arbiter output and the MMIO peripheral decode.  When the address
matches the port-I/O pattern (`addr[11:9] != 0` and the access came
from a port-I/O bus transaction), substitute the target register
address from a 7-entry lookup table.

```verilog
// Port bridge remap (combinational)
wire is_port_io = (bus_mmio_addr[11:9] != 3'b000) && port_io_flag;
wire [11:0] remapped_addr = is_port_io ? port_remap[bus_mmio_addr[11:9]]
                                       : bus_mmio_addr;
```

The `port_io_flag` can be a sideband signal from the CPU (already
available — the CPU knows when it's executing an OUT/INP).

**Option B — Change the CPU's effective_addr formula:**
Instead of `{MMIO_HI, 20'd0, io_port, 9'd0}`, compute the actual
target address directly in the CPU based on a small CSR-loaded remap
table.  More flexible but slightly changes `mp64_cpu.v`.

**Recommendation:** Option A.  It keeps the CPU unchanged (port I/O
generates the same addresses it always has) and pushes the remap into
the SoC fabric where the other MMIO decode logic already lives.
~50 LUTs, zero pipeline impact.

### Emulator implementation notes

- `megapad64.py` already decodes OUT/INP family (0x9).  Add a
  `port_map` dict mapping port number → (device, register_offset).
- OUT N: read `memory[R[xsel]]`, increment R[xsel], write byte to
  `port_map[N].device` at the configured register offset.
- INP N: read byte from `port_map[N].device`, store to `memory[R[xsel]]`,
  write byte to D register.
- Default map mirrors the RTL port table above.
- Configurable port 7: writable via a virtual CSR or MMIO register.

---

## 7. WOTS+ Chain Accelerator (DMA-read, MMIO 0x8A0)

**Priority: high — dominant bottleneck in SPHINCS+ post-quantum signing**
**Topology: shared (wraps the existing SHA3/SHAKE engine)**
**Status: ✅ DONE (emulator + RTL spec + 8 Python tests + 38/38 RTL tests)**
**Origin: Akashic blockchain team request (2026-03-07)**

### 7.1  Problem Statement

SPHINCS+-SHAKE-128s signing is dominated by WOTS+ hash chains.  Each
chain iterates the T₁ tweakable-hash function 15 times sequentially,
where each step's 16-byte output feeds the next step's input:

```
step 0:  out₀ = SHAKE-256(PK.seed ‖ ADRS(hash=0) ‖ sk)       [16 B]
step 1:  out₁ = SHAKE-256(PK.seed ‖ ADRS(hash=1) ‖ out₀)     [16 B]
  ...
step 14: out₁₄ = SHAKE-256(PK.seed ‖ ADRS(hash=14) ‖ out₁₃)  [16 B]
```

A single SPX-SIGN performs **~2.2 million** SHAKE-256 calls, nearly
all inside WOTS chains (34 chains × 15 steps per WOTS instance,
~4,000 WOTS instances per signature).

Profiled cost per hash on the current STC emulator: **~2,818 cycles**,
of which only **~500 cycles (18%)** are actual Keccak-f[1600].  The
remaining **82% is Forth dispatch overhead**: per-hash ADRS mutation,
6 separate MMIO transactions (MODE!, INIT, UPDATE×3, FINAL), stack
juggling, CMOVE.  Total measured: ~4.7 billion cycles per SPX-SIGN.

### 7.2  Solution: Hardware Chain Sequencer

A small FSM that wraps the existing SHA3/SHAKE MMIO engine with a
counter and feedback loop.  The CPU programs context pointers once,
triggers the chain, and reads back a 16-byte result.  The sequencer
performs all intermediate SHAKE-256 calls internally — no CPU
instructions per step, no Forth dispatch, no MMIO round-trips.

**Key design decision: DMA read channel, no bus master.**

The accelerator needs to read 64 bytes of context from RAM at setup
time (16 B PK.seed + 32 B ADRS + 16 B input).  Rather than requiring
the CPU to copy this data into MMIO scratchpad registers, the
accelerator issues targeted read requests through a dedicated DMA read
port on the bus.  This port is:

- **Read-only** — no write channel to RAM.  Output stays in MMIO
  registers; the CPU reads it back via normal MMIO loads.
- **Bounded** — exactly 64 bytes per chain invocation, lowest bus
  priority, completes in ~64 cycles (negligible vs ~7,500 cycles of
  Keccak computation).
- **Not a bus master** — no arbitration for bus ownership, no
  cache coherency, no snoop.  The bus arbiter grants single-byte
  reads at lowest priority; CPU stalls only if both access the bus
  in the same cycle.

This avoids the full bus-master complexity that would be needed for
a coherent crypto core, while eliminating the ~30-instruction CPU
overhead of copying context into scratchpad registers.

### 7.3  MMIO Register Map (0x8A0–0x8BF, 32 bytes)

| Offset | Name          | R/W | Width | Description                           |
|--------|---------------|-----|-------|---------------------------------------|
| +0x00  | WOTS_SEED     | W   | 32b   | RAM address of PK.seed (16 bytes)     |
| +0x04  | WOTS_ADRS     | W   | 32b   | RAM address of ADRS (32 bytes)        |
| +0x08  | WOTS_INPUT    | W   | 32b   | RAM address of input (16 bytes)       |
| +0x0C  | WOTS_STEPS    | W   | 8b    | Chain length (1–15); steps to iterate |
| +0x0D  | WOTS_START    | W   | 8b    | Start step index (0–14)               |
| +0x0E  | WOTS_GO       | W   | 8b    | Write any value → begin chain         |
| +0x0E  | WOTS_STATUS   | R   | 8b    | 0=idle, 1=busy, 2=done               |
| +0x0F  | WOTS_CYCLES   | R   | 8b    | Cycle count of last chain (÷64)       |
| +0x10  | WOTS_DOUT[0]  | R   | 8b    | Result byte 0                         |
| …      | …             | R   | 8b    | …                                     |
| +0x1F  | WOTS_DOUT[15] | R   | 8b    | Result byte 15                        |

Total: 32 bytes.  Fits in the free slot at 0x8A0–0x8BF.

### 7.4  Operation Sequence

1. **CPU writes context pointers** (3 MMIO stores):
   ```
   pkseed_addr → WOTS_SEED
   adrs_addr   → WOTS_ADRS
   input_addr  → WOTS_INPUT
   ```

2. **CPU writes chain parameters** (2 byte stores):
   ```
   steps → WOTS_STEPS    (e.g. 15 for a full chain)
   start → WOTS_START    (e.g. 0)
   ```

3. **CPU writes WOTS_GO** — hardware begins:
   ```
   seed[16] ← DMA_READ(WOTS_SEED, 16)
   adrs[32] ← DMA_READ(WOTS_ADRS, 32)
   buf[16]  ← DMA_READ(WOTS_INPUT, 16)
   for i = 0 to WOTS_STEPS-1:
       adrs[28..31] ← (WOTS_START + i) as big-endian u32
       SHAKE-256.init(mode=3)      // SHAKE-256
       SHAKE-256.absorb(seed, 16)
       SHAKE-256.absorb(adrs, 32)
       SHAKE-256.absorb(buf, 16)
       SHAKE-256.finalize()
       buf ← SHAKE-256.squeeze(16)
   WOTS_DOUT[0..15] ← buf
   WOTS_STATUS ← 2 (done)
   ```

4. **CPU reads WOTS_DOUT[0..15]** (16 MMIO byte reads, or 2 × 64-bit
   aligned reads if the bus supports it).  No polling loop — the chain
   completes in a deterministic number of cycles that the CPU can
   count, or use an interrupt (see §7.8).

### 7.5  Cycle Budget

| Phase               | Cycles  | Notes                            |
|---------------------|---------|----------------------------------|
| DMA read setup      | ~64     | 64 bytes @ 1 byte/cycle          |
| Per-step Keccak     | ~500    | 24 rounds of Keccak-f[1600]      |
| Per-step absorb/pad | ~20     | XOR + padding                    |
| Per-step squeeze    | ~10     | Extract 16 bytes from state      |
| **Per step total**  | **~530** |                                  |
| **15-step chain**   | **~8,014** | 64 + 15 × 530                |
| **Current software** | **~42,275** | Measured on STC emulator     |
| **Speedup**         | **5.3×** | Per chain                       |

### 7.6  End-to-End Impact

| Operation     | Current (cycles) | With HW Chain | Speedup |
|---------------|-----------------|---------------|---------|
| WOTS-PK-GEN  | 1.53M           | ~358K         | 4.3×    |
| SPX-KEYGEN    | 591M            | ~184M         | 3.2×    |
| SPX-SIGN      | ~4,727M         | ~1,470M       | 3.2×    |

The 3.2× is less than the per-chain 5.3× because not all SHAKE calls
are in WOTS chains — FORS tree hashing, XMSS compression, and PRF
calls still use the regular SHA3 MMIO path.

### 7.7  RTL Design — `mp64_wots.v`

New sub-module, instantiated in `mp64_soc.v` alongside existing
peripherals.  Connects to the SHA3 engine's internal ports (not the
MMIO bus — direct wiring to `CryptoSHA3`'s absorb/squeeze interface).

#### FSM States

```
IDLE → DMA_SEED → DMA_ADRS → DMA_INPUT → STEP_ABSORB → STEP_KECCAK
     → STEP_SQUEEZE → STEP_CHECK → DONE → IDLE
```

| State        | Description                                        |
|--------------|----------------------------------------------------|
| IDLE         | Waiting for WOTS_GO write                          |
| DMA_SEED     | Read 16 bytes from RAM into `seed_reg[127:0]`      |
| DMA_ADRS     | Read 32 bytes from RAM into `adrs_reg[255:0]`      |
| DMA_INPUT    | Read 16 bytes from RAM into `buf_reg[127:0]`       |
| STEP_ABSORB  | Feed seed + adrs + buf (64 B) into SHAKE-256       |
| STEP_KECCAK  | Wait for Keccak-f[1600] to complete                |
| STEP_SQUEEZE | Extract 16 bytes from SHAKE state into buf_reg     |
| STEP_CHECK   | Increment step counter; if done → DONE, else loop  |
| DONE         | Latch buf_reg into WOTS_DOUT; set status=2         |

#### Internal Registers

```verilog
reg [127:0] seed_reg;      // PK.seed (16 bytes, loaded once)
reg [255:0] adrs_reg;      // ADRS (32 bytes, hash field mutated)
reg [127:0] buf_reg;       // Chain buffer (16 bytes, fed back)
reg [127:0] dout_reg;      // Output latch (readable via MMIO)
reg [3:0]   step_count;    // Current step (0–14)
reg [3:0]   step_limit;    // WOTS_STEPS
reg [3:0]   step_start;    // WOTS_START
reg [1:0]   status;        // 0=idle, 1=busy, 2=done
```

#### DMA Read Port

```verilog
output reg        dma_req,
output reg [31:0] dma_addr,
input  wire [7:0] dma_rdata,
input  wire       dma_ack
```

The bus arbiter grants `dma_ack` when no CPU access is in flight.
The accelerator reads one byte per granted cycle.  64 bytes total
at startup; no reads during Keccak computation.

#### SHA3 Engine Interface

The WOTS accelerator drives the existing SHA3 engine directly — not
through the MMIO bus.  Internal port signals:

```verilog
output reg        sha3_start,     // pulse to begin absorb/squeeze
output reg [7:0]  sha3_din,       // data byte to absorb
output reg        sha3_din_valid,
output reg        sha3_finalize,  // trigger padding + final permutation
input  wire [7:0] sha3_dout,      // squeezed output byte
input  wire       sha3_dout_valid,
input  wire       sha3_ready      // engine idle
```

During WOTS chain execution, the WOTS FSM has exclusive access to
the SHA3 engine.  The MMIO bus decode for SHA3 (0x780–0x7CF) returns
`busy` if the WOTS accelerator is active.  This is a simple mux
controlled by `wots_active`:

```verilog
assign sha3_mmio_blocked = (wots_status != 2'b00);
```

#### Area Estimate

| Component          | LUTs  | FFs  | BRAM |
|--------------------|-------|------|------|
| FSM + control      | ~40   | ~20  | 0    |
| seed_reg (128b)    | 0     | 128  | 0    |
| adrs_reg (256b)    | 0     | 256  | 0    |
| buf_reg (128b)     | 0     | 128  | 0    |
| dout_reg (128b)    | 0     | 128  | 0    |
| step counter (4b)  | ~5    | 4    | 0    |
| ADRS mutator       | ~20   | 0    | 0    |
| DMA addr counter   | ~15   | 32   | 0    |
| **Total**          | **~80** | **~696** | **0** |

Negligible vs the SHA3 engine itself (~2,000+ LUTs for Keccak).

### 7.8  Interrupt vs Polling

The chain completes in a deterministic number of cycles:
`64 + WOTS_STEPS × 530`.  For a typical 15-step chain, that's ~8,014
cycles.  Two options:

1. **Deterministic delay:** CPU executes ~8,000 cycles of other work
   (e.g., prepare the next chain's ADRS) then reads WOTS_DOUT.
   No polling, no interrupt.  Requires careful Forth scheduling.

2. **Interrupt on done:** The WOTS engine asserts a wire when
   `status` transitions to `2 (done)`.  Routed to a new IVT slot.
   The ISR reads WOTS_DOUT and signals the Forth task.
   Simpler software, ~50 cycle interrupt overhead.

**Recommendation:** Start with deterministic delay in the BIOS word.
Add interrupt support later if the Akashic team needs to overlap
chains with other computation.

### 7.9  Forth Interface (BIOS word)

```
WOTS-CHAIN-HW ( seed adrs input steps start -- )
```

Stores 5 parameters into MMIO registers, writes WOTS_GO, delays
for the deterministic cycle count, then copies 16 bytes from
WOTS_DOUT back to the input buffer (in-place update for chain
callers).

```forth
: WOTS-CHAIN-HW  ( seed adrs input steps start -- )
    WOTS_START C!                  \ byte store
    WOTS_STEPS C!                  \ byte store
    WOTS_INPUT !                   \ 32-bit store
    WOTS_ADRS  !                   \ 32-bit store
    WOTS_SEED  !                   \ 32-bit store
    1 WOTS_GO C!                   \ trigger
    \ Deterministic wait: 64 + steps × 530 cycles
    \ For now, simple poll (emulator will make this instant)
    BEGIN WOTS_STATUS C@ 2 = UNTIL
;
```

The Akashic team replaces their `_SPX-CHAIN` Forth word body with
a call to `WOTS-CHAIN-HW`, feature-detected via an MMIO probe of
`WOTS_STATUS` (reads 0 if present, 0xFF if absent).

### 7.10  Emulator Implementation

#### C++ (mp64_crypto.h)

New `WotsChain` struct alongside `CryptoSHA3`.  On `GO` write:
1. Read 64 bytes from CPU memory via `mem_read8()` (direct — no bus
   arbitration needed in emulator).
2. Loop `steps` times, calling `CryptoSHA3::reset()`, feeding bytes
   via `write8(0x08, ...)`, calling `finalize()`, reading digest.
3. Latch 16-byte result into `dout[16]`.
4. Add `64 + steps × 530` to cycle counter for accurate profiling.

All computation stays in C++ — zero Python callbacks.  This is the
entire point: the WOTS accelerator eliminates the 9.6M Python
round-trips that make SPHINCS+ signing slow in the emulator.

#### Python fallback (devices.py)

`WotsChainAccel` MMIO device with the same register interface.
Uses the existing Python `SHA3Device` internally.  Only used when
the C++ accelerator is not available.

#### Integration

- `mp64_accel.cpp`: add `WotsChain` to `CPUState`, handle MMIO
  offsets 0x8A0–0x8BF in `sys_read8`/`sys_write8`.
- `system.py`: create `WotsChainAccel`, register on bus.
- `accel_wrapper.py`: sync `WotsChain` state (seed/adrs/input
  pointers only — result is read from C++ state).

### 7.11  Future: Parallel SHAKE Engines

The 34 chains within a single WOTS-PK-GEN are independent.  A future
enhancement instantiates 2–4 SHAKE-256 engines behind the same
sequencer FSM, computing 2–4 chains simultaneously:

| Engines | WOTS-PK-GEN | SPX-SIGN @ 200 MHz | Extra LUTs |
|---------|-------------|---------------------|------------|
| 1       | ~358K       | 7.3s                | ~80        |
| 2       | ~190K       | ~3.9s               | ~2,100     |
| 4       | ~105K       | ~2.1s               | ~6,200     |

This is out of scope for v1,but the single-engine design is
structured to allow it: the DMA read port, MMIO registers, and FSM
all generalise to N engines with a round-robin scheduler.

---

## 8. MMIO Map After Changes

Proposed layout with SHA-256/512 widened and CRC relocated:

| Range         | Size   | Peripheral          | Status     |
|---------------|--------|---------------------|------------|
| 0x000–0x0FF   | 256 B  | UART                | existing   |
| 0x100–0x1FF   | 256 B  | Timer               | existing   |
| 0x200–0x2FF   | 256 B  | Disk                | existing   |
| 0x300–0x3FF   | 256 B  | SysInfo             | existing   |
| 0x400–0x4FF   | 256 B  | NIC                 | existing   |
| 0x500–0x6FF   | 512 B  | Mailbox             | existing   |
| 0x700–0x77F   | 128 B  | AES                 | existing   |
| 0x780–0x7DF   | 96 B   | SHA-3               | existing   |
| 0x800–0x81F   | 32 B   | TRNG                | existing   |
| 0x840–0x87F   | 64 B   | Field ALU           | **→ per-core ISA (Appendix B)** |
| 0x880–0x89F   | 32 B   | **Port I/O remap CSR** | **new**    |
| 0x8A0–0x8BF   | 32 B   | **WOTS+ Chain Accel** | **new (§7)** |
| 0x8C0–0x8FF   | 64 B   | NTT                 | existing   |
| 0x900–0x93F   | 64 B   | KEM                 | existing   |
| 0x940–0x9BF   | 128 B  | **SHA-256/384/512** | **→ per-core ISA (Appendix B)** |
| 0x9C0–0x9FF   | 64 B   | *(free)*            |            |
| 0xA00–0xA1F   | 32 B   | *(free — CRC removed)* | ✅ CRC is now ISA-only |
| 0xA20–0xA7F   | 96 B   | **Trace readout portal** | **new** |
| 0xA80–0xAFF   | 128 B  | *(free — future)*   |            |
| 0xB00–0xB1F   | 32 B   | RTC                 | existing   |
| 0xB20–0xFFF   | 1248 B | *(free)*            |            |

The string engine (§2) and dictionary search engine (§3) are **ISA
extensions** (EXT.STRING F9, EXT.DICT FA) — they live inside each CPU
as tightly-coupled sub-modules, like the multiplier.  They are not on
the MMIO bus at all and require no cluster wrapper ports.

**Crypto migration (Appendix B):** CRC migration is **complete** — MMIO
`mp64_crc.v` removed, all cores use ISA instructions (EXT.CRYPTO FB),
micro-cores share via cluster hardware-lock arbiter.  SHA-256/384/512
and Field ALU are next — their MMIO slots (0x840, 0x940) may be
retained temporarily for micro-core access or freed entirely once
migration is complete.  See Appendix B §B.9 for the phased plan.

The main MMIO map carries only shared peripherals (AES, SHA-3, NTT,
KEM, WOTS+, TRNG, RTC) plus bus infrastructure (mailbox, spinlocks,
port remap, trace portal).

The trace readout portal (0xA20) is a small shared MMIO window that
lets a debugger select a core ID and drain that core's per-core trace
ring buffer over the main bus.  The actual trace capture hardware lives
per-core inside each cluster.

The port I/O remap CSR (0x880) holds the 7-entry address translation
table for `OUT 1`–`OUT 7` / `INP 1`–`INP 7`.  Each entry is a 12-bit
MMIO target address.  Written once at boot by the BIOS.  The remap
logic itself is combinational in the SoC fabric — the CSR just stores
the table.

Plenty of room for future shared accelerators without touching the
upper half of the address space.

---

## 9. Bus Arbiter MMIO/MEM ACK Timeout — ✅ DONE

**Status: ✅ Implemented in RTL + emulator, fully tested (2026-03-07)**

### Problem

The bus arbiter (`mp64_bus.v`) waited for `mmio_ack` / `mem_ack`
indefinitely.  Any peripheral that failed to ACK — hardware bug,
unmapped address, or a gated peripheral (e.g. SHA3 locked by WOTS) —
caused a total system hang across all cores.

### RTL Solution

Two watchdog counters in `mp64_bus.v`:

| Domain | Counter | Timeout | Sentinel |
|--------|---------|---------|----------|
| MMIO   | 6-bit   | 63 cycles  | `0xDEAD_DEAD_DEAD_DEAD` |
| MEM    | 8-bit   | 255 cycles | `0xDEAD_DEAD_DEAD_DEAD` |

On timeout:
1. Bus returns sentinel data and asserts `bus_ack` to unblock the
   requesting core.
2. `bus_err` pulse fires (active for 1 cycle per timeout event).
3. `bus_err_sticky` latch set — persists until cleared via W1C write
   to `CSR_BUS_ERR` (address `0x5A`).
4. `IRQX_BUS` (priority 5: IPI > **bus** > timer > uart > nic) fires
   on each core receiving the timeout.

Files changed: `mp64_bus.v`, `mp64_pkg.vh` (`IRQX_BUS`, `CSR_BUS_ERR`),
`mp64_cpu.v` (`irq_bus` input), `mp64_soc.v` (wiring).

### Emulator Solution

- `devices.py`: `BusError` exception class; `DeviceBus.read8()` /
  `write8()` raise `BusError` on unmapped MMIO offsets (was: silent
  0xFF / drop).
- `system.py`: `_patched_read8()` / `_patched_write8()` catch
  `BusError` and convert to `TrapError(IVEC_BUS_FAULT)`.

### Tests

- **RTL:** `tb_bus_arbiter.v` — tests 8 (MMIO timeout) and 9
  (MEM timeout).  Verify sentinel data, bus_err pulse, sticky latch
  set, W1C clear, bus recovery.  38/38 total tests passing.
- **Python (TestBusTimeout, 6 tests):** unmapped read/write raises
  `BusError`, mapped device no error, CPU MMIO read/write traps to
  `IVEC_BUS_FAULT`, trap handler entry (flag_i cleared).

---

## 10. BIOS SHA3/WOTS Lock Guards + Diagnostic Words — ✅ DONE

**Status: ✅ Implemented in C++ accel + BIOS + tests (2026-03-07)**

### Problem

After the WOTS+ chain accelerator (§7) was added, the SHA3 coprocessor
can be locked while a WOTS chain is in progress.  If the BIOS `SHA3-INIT`
word ran during this window it would access a gated peripheral and either
hang (RTL) or bus-fault (emulator).  Similarly, `WOTS-CHAIN-HW` could
interfere if SHA3 was already busy from another caller.  There was no
firmware-level visibility into bus errors or accelerator status.

### Changes

#### C++ accelerator (`accel/mp64_crypto.h`)

`CryptoDevices::read8` now injects **bit 2** (`ext_locked`) into the
SHA3 STATUS register (offset 0x01 from SHA3\_BASE) whenever
`wots.status != 0`.  This matches the RTL `sha3_mmio_blocked` signal
so firmware sees a consistent lock indicator in both emulator and
hardware.

#### BIOS (`bios.asm`)

**New words:**

| Word | Behaviour |
|------|-----------|
| `SHA3-LOCKED?` | Read SHA3 STATUS bit 2 → Forth flag (0 / -1) |
| `WOTS-STATUS@` | Read WOTS+0x0E STATUS register (0=idle, 1=busy, 2=done) |
| `BUS-ERR@`     | CSRR 0x5A → push sticky bus-error latch |
| `BUS-ERR-CLR`  | Pop mask, CSRW 0x5A (W1C clear) |

**Lock guards:**

- `SHA3-INIT`: checks SHA3 STATUS bit 2 (ext\_locked); if set, prints
  `"SHA3 locked by WOTS\n"` and aborts without touching the device.
- `WOTS-CHAIN-HW`: checks SHA3 STATUS bit 1 (busy); if set, drops 5
  stack arguments and prints `"SHA3 busy — WOTS aborted\n"`.

**Bus fault handler upgrade:**

`bus_fault_handler` now appends `" ERR="` followed by the hex value
of `CSR_BUS_ERR` (0x5A) to its output, giving immediate visibility
into the sticky bus-error latch on any trap.

#### Dictionary entries

Four new entries chained after `d_wots_chain_hw`:
`d_sha3_locked` → `d_wots_status` → `d_bus_err_fetch` → `d_bus_err_clr`
→ `d_bist_full` (previously linked directly from `d_wots_chain_hw`).

### Tests (`tests/test_system.py` — `TestSHA3LockAndBusErr`, 8 tests)

| Test | Checks |
|------|--------|
| `test_sha3_ext_locked_when_wots_idle` | ext\_locked = 0 when WOTS idle |
| `test_sha3_ext_locked_after_wots` | ext\_locked = 1 when WOTS status ≠ 0 |
| `test_sha3_ext_locked_via_cpu_mmio` | MMIO read of SHA3 STATUS bit 2 via CPU |
| `test_wots_status_idle` | WOTS STATUS = 0 on init |
| `test_wots_status_done` | WOTS STATUS = 2 after chain completes |
| `test_bus_err_csr_read` | CSRR 0x5A returns 0 (no bus errors) |
| `test_bus_err_csr_write` | CSRW 0x5A (W1C) doesn't crash the CPU |
| `test_python_wots_status_lifecycle` | Python WotsChainAccel status 0 → 2 |

All 1,739 tests passing (3 skipped — network).

---

## Appendix A — Pre-Implementation ISA Details

> **This section is the working spec for ISA additions planned in this
> document.  It is NOT yet in `isa-reference.md` — move it there only
> after implementation is committed and tested.  Encodings may change.**

---

### A.1  EXT Prefix Slot Map (Family 0xF, post-REX)

```
F0  EXT.IMM64      (pre-existing)
F1  REX.S           source reg high bit           [committed]
F2  REX.D           dest reg high bit             [committed]
F3  REX.DS          both src + dest hi            [committed]
F4  REX.N           nibble reg high bit           [committed]
F5  REX.ND          nibble + dest hi              [committed]
F6  EXT.SKIP        (pre-existing)
F7  —               (free — reserved)
F8  EXT.ETALU       (pre-existing)
F9  EXT.STRING      block-move/fill/compare       [committed, §2]
FA  EXT.DICT        dictionary search             [committed, §3]
FB  EXT.CRYPTO      per-core crypto ops           [proposed, Appendix B]
FC  —               (free)
FD  —               (free)
FE  —               (free)
FF  —               (free)
```

**Budget:** 3 pre-existing + 5 REX + 2 committed + 1 proposed = 11 used, **5 free**.

---

### A.2  EXT.STRING — Block Memory Operations (prefix F9)

Three-byte instructions: `F9 <sub-op> <reg-byte>`.

The reg-byte encodes `Rd[3:0] : Rs[3:0]` (DR form) or
`Rd[3:0] : Rn[3:0]` (DN form).  Preceding REX prefix extends to
5-bit indices.

| Encoding | Mnemonic | Cycles | Semantics |
|----------|----------|--------|-----------|
| `F9 00 DR` | **CMOVE Rd, Rs** | N+2 | Copy M[Rs]→M[Rd], len=R0; forward.  Updates Rd, Rs, R0. |
| `F9 01 DR` | **CMOVE> Rd, Rs** | N+2 | Copy M[Rs]→M[Rd], len=R0; backward (overlap-safe).  Updates Rd, Rs, R0. |
| `F9 02 DN` | **BFILL Rd, Rn** | N+2 | Fill M[Rd] with D[7:0], len=Rn.  Updates Rd, Rn. |
| `F9 03 DR` | **BCOMP Rd, Rs** | 2–N | Compare M[Rs] vs M[Rd], len=R0.  Sets Z, G.  Short-circuits. |
| `F9 04 DR` | **BSRCH Rd, Rs** | 2–N | Search M[Rd..Rd+R0-1] for D[7:0].  Rs←offset, Z=found. |
| `F9 05`–`0F` | *(reserved)* | | Future: word-width, pattern fill, etc. |

**N** = byte count (from R0 or Rn).  Aligned 64-bit burst where possible.

**Pipeline:** CPU enters `CPU_STRING` stall state.  String sub-module
uses CPU's existing bus master.  On completion, resume fetch.

**Register effects (CMOVE example):**
```
Before: Rs=src_addr, Rd=dst_addr, R0=len
After:  Rs=src_addr+len, Rd=dst_addr+len, R0=0
```
Matches standard Forth CMOVE stack contract.

**Flags:**
- BCOMP: Z=1 if regions are identical; G=1 if M[Rd] > M[Rs] at first
  mismatch (unsigned byte comparison).
- BSRCH: Z=1 if byte found; Rs = offset of first match (or R0 if not
  found).
- CMOVE, CMOVE>, BFILL: no flag changes.

**Instruction length:** 3 bytes (or 4 with REX prefix).

**Micro-cores:** `ILLEGAL_OP` trap.  Software byte-loop fallback.

---

### A.3  EXT.DICT — Dictionary Search Operations (prefix FA)

Two- or three-byte instructions: `FA <sub-op>` or `FA <sub-op> <reg-byte>`.

| Encoding | Mnemonic | Cycles | Semantics |
|----------|----------|--------|-----------|
| `FA 00 DR` | **DFIND Rd, Rs** | 2 | Hash M[Rs] (counted string), look up in BRAM table.  Rd←XT, Z=found. |
| `FA 01 DR` | **DINS Rd, Rs** | 3–4 | Insert: Rs=name addr, Rd=XT to store.  Z=success, V=overflow. |
| `FA 02 0R` | **DDEL Rs** | 2 | Delete entry by name at Rs.  Z=found-and-deleted. |
| `FA 03` | **DCLR** | 1 | Clear entire hash table.  2-byte instruction. |
| `FA 04`–`0F` | *(reserved)* | | Future: vocab select, iteration, stats. |

**Pipeline:** CPU enters `CPU_DICT` stall state.  Dict sub-module
reads name bytes via CPU bus master (cycle 1: hash), then accesses
BRAM (cycle 2: 4-way parallel compare).  Resume fetch on `dict_done`.

**Hash table:** 256-entry, 4-way set-associative.  Per entry:
32-bit hash + 64-bit NFA + 64-bit XT + 5-bit name_len + 31-byte name.
~4 BRAM36 per big-core instance.

**INSERT broadcast:** On DINS, the SoC fans the entry to all other
CPUs via `dict_insert_broadcast` sideband (hash + NFA + XT + name,
1 cycle).  Valid + ack handshake prevents collision on simultaneous
DINS from two cores.

**Flags:**
- DFIND: Z=1 if found (Rd valid); Z=0 if miss (Rd undefined).
- DINS: Z=1 success; V=1 if all 4 ways full (overflow, software
  fallback).
- DDEL: Z=1 if entry was found and deleted.

**Instruction length:** 3 bytes (DFIND, DINS, DDEL) or 2 bytes (DCLR).
+1 byte with REX prefix where applicable.

**Micro-cores:** `ILLEGAL_OP` trap.  Software linked-list FIND fallback.

---

### A.4  Bitfield ALU Operations (MULDIV family 0xC, sub-ops C8–CF)

ALU family 0x7 is fully packed (70–7F).  MULDIV family 0xC uses only
C0–C7, leaving **C8–CF free** — 8 slots, exactly matching the 8
bitfield operations.  These are wired into `mp64_alu.v` combinational
logic despite sharing the MULDIV opcode space; they execute in
**1 cycle** (unlike MUL/DIV's 4 cycles).

Two-byte instructions: `Cx DR` (opcode + reg-byte).

#### Tier 1 — Universal (~30 LUTs each)

| Encoding | Mnemonic | Cycles | Flags | Semantics |
|----------|----------|--------|-------|-----------|
| `C8 DR` | **POPCNT Rd, Rs** | 1 | Z | `Rd ← popcount(Rs)` (0–64) |
| `C9 DR` | **CLZ Rd, Rs** | 1 | Z | `Rd ← count_leading_zeros(Rs)` (0=64, MSB set=0) |
| `CA DR` | **CTZ Rd, Rs** | 1 | Z | `Rd ← count_trailing_zeros(Rs)` (0=64, LSB set=0) |
| `CB DR` | **BITREV Rd, Rs** | 1 | — | `Rd ← bit_reverse(Rs)` (bit 0↔63, 1↔62, …) |

#### Tier 2 — High-value (~50–100 LUTs each)

| Encoding | Mnemonic | Cycles | Flags | Semantics |
|----------|----------|--------|-------|-----------|
| `CC DR` | **BEXT Rd, Rs** | 1 | Z | Bit extract (gather): collect bits of Rd at positions where Rs has 1s, pack right-justified. `Rd ← pext(Rd, Rs)` |
| `CD DR` | **BDEP Rd, Rs** | 1 | Z | Bit deposit (scatter): spread low bits of Rd into positions where Rs has 1s. `Rd ← pdep(Rd, Rs)` |
| `CE DR` | **BSWAP Rd, Rs** | 1 | — | Byte-swap (endian reverse): `Rd ← bswap64(Rs)` |
| `CF Rn imm8` | **RORI Rn, imm6** | 1 | — | Rotate right by immediate: `Rn ← rotr(Rn, imm8[5:0])`.  3-byte encoding. |

**RORI encoding note:** CF is the only 3-byte instruction in the MULDIV
family.  Byte 1 is `Rn[3:0] : 0000` (high nibble = dest register, low
nibble ignored/zero).  Byte 2 is the immediate; bits [5:0] are the
rotation amount (0–63), bits [7:6] are reserved (must be 0).  REX.N
extends Rn to 5 bits.

**Why MULDIV family, not EXT prefix:**  These are pure combinational
ALU functions — no state, no stalling, no bus access.  They don't need
a prefix byte.  Sharing the MULDIV opcode space is natural: the CPU
decode already routes 0xC to the register-register datapath.  The
1-cycle vs 4-cycle distinction is handled by not entering the `CPU_MULDIV`
stall state — the ALU result is ready combinationally and written back
in the same cycle (same as family 0x7).

**Flags:** POPCNT, CLZ, CTZ, BEXT, BDEP set Z if the result is zero.
BITREV, BSWAP, RORI set no flags (pure data movement).

**Micro-core availability:**
- Tier 1 (C8–CB): available on all cores (~100 LUTs total).
- Tier 2 (CC–CF): big-core only.  Micro-cores trap `ILLEGAL_OP` on
  CC–CF.  BEXT/BDEP are the expensive ones (~80 LUTs each); gating
  them saves area.  BSWAP and RORI could go either way — revisit
  during implementation.

#### RTL decode sketch

```verilog
// In mp64_cpu.v MULDIV family decode:
case (nib)
    // C0–C7: existing MUL/DIV (enter CPU_MULDIV stall)
    4'h0: begin /* MUL  */ ... cpu_state <= CPU_MULDIV; end
    ...
    4'h7: begin /* UMOD */ ... cpu_state <= CPU_MULDIV; end

    // C8–CF: bitfield ops (single-cycle, no stall)
    4'h8: begin R[rd] <= popcount64(R[rs]);          update_flags_z(popcount64(R[rs])); end
    4'h9: begin R[rd] <= clz64(R[rs]);               update_flags_z(clz64(R[rs]));      end
    4'hA: begin R[rd] <= ctz64(R[rs]);               update_flags_z(ctz64(R[rs]));      end
    4'hB: begin R[rd] <= bitrev64(R[rs]);            end
    4'hC: begin R[rd] <= pext64(R[rd], R[rs]);       update_flags_z(pext64(R[rd], R[rs])); end
    4'hD: begin R[rd] <= pdep64(R[rd], R[rs]);       update_flags_z(pdep64(R[rd], R[rs])); end
    4'hE: begin R[rd] <= bswap64(R[rs]);             end
    4'hF: begin R[rd] <= rotr64(R[rd], ibuf[2][5:0]); instr_len <= 3; end  // RORI, 3-byte
endcase
```

*(Actual implementation will use functions in `mp64_alu.v` and wire
results back; this sketch shows the decode intent.)*

#### Area budget (64-bit, 7-series)

| Sub-op | Op | LUTs | Notes |
|--------|----|------|-------|
| C8 | POPCNT | ~30 | Wallace tree adder |
| C9 | CLZ | ~30 | Priority encoder tree |
| CA | CTZ | ~30 | BITREV + CLZ, or direct priority encoder |
| CB | BITREV | ~0 | Pure wiring (synthesis optimises away) |
| CC | BEXT | ~80 | Parallel gather network |
| CD | BDEP | ~80 | Parallel scatter network |
| CE | BSWAP | ~0 | Pure wiring |
| CF | RORI | ~10 | Reuse existing barrel shifter + rotate-mode bit |
| | **Total** | **~260** | All targets; ~90 for micro-cores (Tier 1 only) |

---

### A.5  Complete Family 0xC Map (After Bitfield Addition)

| Opcode | Mnemonic | Bytes | Cycles | Category |
|--------|----------|-------|--------|----------|
| `C0 DR` | MUL Rd, Rs | 2 | 4 | Multiply |
| `C1 DR` | MULH Rd, Rs | 2 | 4 | Multiply |
| `C2 DR` | UMUL Rd, Rs | 2 | 4 | Multiply |
| `C3 DR` | UMULH Rd, Rs | 2 | 4 | Multiply |
| `C4 DR` | DIV Rd, Rs | 2 | 4 | Divide |
| `C5 DR` | UDIV Rd, Rs | 2 | 4 | Divide |
| `C6 DR` | MOD Rd, Rs | 2 | 4 | Divide |
| `C7 DR` | UMOD Rd, Rs | 2 | 4 | Divide |
| `C8 DR` | **POPCNT Rd, Rs** | 2 | 1 | Bitfield (Tier 1) |
| `C9 DR` | **CLZ Rd, Rs** | 2 | 1 | Bitfield (Tier 1) |
| `CA DR` | **CTZ Rd, Rs** | 2 | 1 | Bitfield (Tier 1) |
| `CB DR` | **BITREV Rd, Rs** | 2 | 1 | Bitfield (Tier 1) |
| `CC DR` | **BEXT Rd, Rs** | 2 | 1 | Bitfield (Tier 2) |
| `CD DR` | **BDEP Rd, Rs** | 2 | 1 | Bitfield (Tier 2) |
| `CE DR` | **BSWAP Rd, Rs** | 2 | 1 | Bitfield (Tier 2) |
| `CF Rn imm8` | **RORI Rn, imm6** | 3 | 1 | Bitfield (Tier 2) |

Family fully packed.  All 16 sub-ops allocated.

---

### A.6  Instruction Length Summary (New Additions)

| Instruction | Encoding | Bytes | +REX |
|-------------|----------|-------|------|
| CMOVE | F9 00 DR | 3 | 4 |
| CMOVE> | F9 01 DR | 3 | 4 |
| BFILL | F9 02 DN | 3 | 4 |
| BCOMP | F9 03 DR | 3 | 4 |
| BSRCH | F9 04 DR | 3 | 4 |
| DFIND | FA 00 DR | 3 | 4 |
| DINS | FA 01 DR | 3 | 4 |
| DDEL | FA 02 0R | 3 | 4 |
| DCLR | FA 03 | 2 | — |
| POPCNT | C8 DR | 2 | 3 |
| CLZ | C9 DR | 2 | 3 |
| CTZ | CA DR | 2 | 3 |
| BITREV | CB DR | 2 | 3 |
| BEXT | CC DR | 2 | 3 |
| BDEP | CD DR | 2 | 3 |
| BSWAP | CE DR | 2 | 3 |
| RORI | CF Rn imm8 | 3 | 4 |

**Total new instructions: 17** (5 string + 4 dict + 8 bitfield).

---

### A.7  Emulator Dispatch Reference

Quick-reference for `megapad64.py` implementation:

```python
# --- EXT.STRING (F9) ---
# ibuf[0]=F9, ibuf[1]=sub_op, ibuf[2]=DR
elif ext_op == 0xF9:
    sub = ibuf[1]
    rd, rs = (ibuf[2] >> 4) & 0xF, ibuf[2] & 0xF
    # apply REX extension to rd, rs
    length = R[0] if sub != 0x02 else R[rn]
    if sub == 0x00:    # CMOVE (forward)
        mem[R[rd]:R[rd]+length] = mem[R[rs]:R[rs]+length]
        R[rd] += length; R[rs] += length; R[0] = 0
    elif sub == 0x01:  # CMOVE> (backward)
        for i in range(length-1, -1, -1):
            mem[R[rd]+i] = mem[R[rs]+i]
        R[rd] += length; R[rs] += length; R[0] = 0
    elif sub == 0x02:  # BFILL
        mem[R[rd]:R[rd]+length] = bytes([D & 0xFF]) * length
        R[rd] += length; R[rn] = 0
    elif sub == 0x03:  # BCOMP
        a = mem[R[rd]:R[rd]+length]
        b = mem[R[rs]:R[rs]+length]
        Z = (a == b); G = (a > b)
    elif sub == 0x04:  # BSRCH
        haystack = mem[R[rd]:R[rd]+length]
        idx = haystack.find(D & 0xFF)
        Z = (idx >= 0); R[rs] = idx if idx >= 0 else length

# --- EXT.DICT (FA) ---
elif ext_op == 0xFA:
    sub = ibuf[1]
    rd, rs = (ibuf[2] >> 4) & 0xF, ibuf[2] & 0xF
    if sub == 0x00:    # DFIND
        name = read_counted_string(R[rs])
        entry = dict_table.get(name)
        Z = (entry is not None)
        if Z: R[rd] = entry.xt
    elif sub == 0x01:  # DINS
        name = read_counted_string(R[rs])
        dict_table[name] = DictEntry(nfa=R[rs], xt=R[rd])
        Z = 1  # V=1 if overflow
    elif sub == 0x02:  # DDEL
        name = read_counted_string(R[rs])
        Z = name in dict_table
        if Z: del dict_table[name]
    elif sub == 0x03:  # DCLR
        dict_table.clear()

# --- Bitfield (C8–CF) ---
# In MULDIV family dispatch, after C0–C7:
elif nib == 0x8:  R[rd] = bin(R[rs]).count('1')                    # POPCNT
elif nib == 0x9:  R[rd] = 64 - R[rs].bit_length() if R[rs] else 64  # CLZ
elif nib == 0xA:  R[rd] = ctz64(R[rs])                              # CTZ
elif nib == 0xB:  R[rd] = int(f'{R[rs]:064b}'[::-1], 2)            # BITREV
elif nib == 0xC:  R[rd] = pext64(R[rd], R[rs])                     # BEXT
elif nib == 0xD:  R[rd] = pdep64(R[rd], R[rs])                     # BDEP
elif nib == 0xE:  R[rd] = int.from_bytes(                           # BSWAP
                      R[rs].to_bytes(8,'little'), 'big')
elif nib == 0xF:                                                     # RORI
    imm6 = ibuf[2] & 0x3F
    R[rd] = ((R[rd] >> imm6) | (R[rd] << (64 - imm6))) & MASK64
```

*(Pseudocode — actual implementation will use proper masking,
REX-extended register indices, and flag updates.)*

---

## Appendix B — Core-Integrated Crypto ISA Extension (EXT.CRYPTO, prefix FB)

> **Status:** Pre-implementation design.  NOT in `isa-reference.md` yet.
> Encodings may change.  Move to ISA doc only after emulator + RTL
> implementation is committed and tested.

### B.0  Rationale: Why Move Crypto Into the Core

Five crypto primitives currently live as shared MMIO peripherals: CRC,
SHA-256, SHA-384, SHA-512, and the Field ALU (GF(2²⁵⁵−19) / multi-prime).
Moving them into per-core ISA instructions eliminates:

1. **Bus round-trip overhead** — MMIO writes cost 1 cycle bus request +
   1 cycle bus grant + 1 cycle device ack = 3 cycles minimum per register
   write.  A SHA-256 block requires ~16 data writes + 2 control writes =
   ~54 cycles of MMIO overhead on top of the 64-cycle compression.
2. **Bus contention** — shared peripherals serialise all 4 cores.  Under
   parallel TLS handshakes, this is a bottleneck.
3. **Software complexity** — spinlock/mailbox acquire-release around every
   crypto call, which also blocks other cores.

**What stays MMIO (shared):** AES-256-GCM, SHA-3/SHAKE, NTT, KEM, WOTS+
chain accelerator, TRNG.  These have state footprints (1600-bit Keccak state,
AES key schedule, NTT polynomial arrays) or algorithmic structures
(multi-phase KEM protocol) that don't map cleanly to core registers.

**Micro-cores:** All EXT.CRYPTO instructions trap as `ILLEGAL_OP`.
Micro-cores continue to use MMIO paths (or don't do crypto at all).

**Per-core area cost:**

| Unit | Gates/core | LUTs (est., 7-series) |
|------|-----------|----------------------|
| CRC32/CRC64 | ~2,500 | ~40 |
| SHA-2 unified (256/384/512) | ~30,000 | ~500 |
| Field ALU (multi-prime) | ~45,000 | ~750 + DSP48 |
| Decode delta | ~3,000 | ~50 |
| **Total per core** | **~80,500** | **~1,340 + DSP48** |
| **× 4 full cores** | **~322,000** | **~5,360 + DSP48** |

For comparison: ARM's Crypto Extension adds ~50K gates/core (AES+SHA
only).  The Field ALU is the largest single unit (~45K) but it already
exists — we're moving it, not creating it.  The shared MMIO register
interfaces (~3K gates each) are eliminated.  Net SoC delta is modest.

---

### B.1  EXT Prefix Slot Map (Updated)

```
F0  EXT.IMM64      (pre-existing)
F1  REX.S           source reg high bit           [committed]
F2  REX.D           dest reg high bit             [committed]
F3  REX.DS          both src + dest hi            [committed]
F4  REX.N           nibble reg high bit           [committed]
F5  REX.ND          nibble + dest hi              [committed]
F6  EXT.SKIP        (pre-existing)
F7  —               (free — reserved)
F8  EXT.ETALU       (pre-existing)
F9  EXT.STRING      block-move/fill/compare       [committed, §2]
FA  EXT.DICT        dictionary search             [committed, §3]
FB  EXT.CRYPTO      per-core crypto ops           [proposed, Appendix B]
FC  —               (free)
FD  —               (free)
FE  —               (free)
FF  —               (free)
```

**Budget:** 3 pre-existing + 5 REX + 2 committed + **1 proposed** = 11 used, **5 free**.

---

### B.2  Encoding: EXT.CRYPTO (prefix FB)

Two- or three-byte instructions: `FB <sub-op>` or `FB <sub-op> <arg>`.

The sub-op byte is divided into groups:

```
sub-op [7:4] = crypto unit:
    0x0_  CRC
    0x1_  SHA-2 (unified 256/384/512)
    0x2_  Field ALU
    0x3_–0xF_  reserved (future: AES-round, etc.)

sub-op [3:0] = operation within unit
```

---

### B.3  CRC Instructions (sub-ops 0x00–0x0F)

CRC state is a new per-core 64-bit CSR: **CRC_ACC** at CSR address
**0x80**.  This replaces the entire MMIO CRC device for cores that
have the crypto ISA extension.

| Encoding | Mnemonic | Bytes | Cycles | Description |
|----------|----------|-------|--------|-------------|
| `FB 00` | **CRC.INIT** | 2 | 1 | `CRC_ACC ← 0xFFFF_FFFF` (CRC32 init).  Clears state. |
| `FB 01 DR` | **CRC.B Rd, Rs** | 3 | 1 | Feed byte: `CRC_ACC ← crc_step(CRC_ACC, R[s][7:0])`.  Rd ← `CRC_ACC` (updated value). |
| `FB 02 DR` | **CRC.Q Rd, Rs** | 3 | 1 | Feed 8 bytes: `CRC_ACC ← crc_step×8(CRC_ACC, R[s][63:0])`.  Rd ← `CRC_ACC`. |
| `FB 03 DR` | **CRC.FIN Rd, Rs** | 3 | 1 | Finalize: `Rd ← CRC_ACC ^ mask`.  (mask = `0xFFFF_FFFF` for CRC32/C, all-ones for CRC64). |
| `FB 04 imm8` | **CRC.MODE imm8** | 3 | 1 | Set polynomial: `imm8[1:0]`: 0=CRC32, 1=CRC32C, 2=CRC64-ECMA.  Latches polynomial for subsequent CRC ops. |
| `FB 05`–`0F` | *(reserved)* | | | |

**CRC_MODE** is a 2-bit per-core register (alongside CRC_ACC).  Default
after reset: mode=0 (CRC32), CRC_ACC=0xFFFFFFFF.

**Pipeline:** Pure combinational.  CRC.B uses an 8-bit lookup table
(~40 LUTs).  CRC.Q uses an 8-byte-wide parallel CRC circuit (~300 LUTs).
No stalling — result available same cycle.

**New CSRs:**

| CSR Addr | Name | Width | R/W | Description |
|----------|------|-------|-----|-------------|
| `0x80` | **CRC_ACC** | 64 | RW | Running CRC accumulator |
| `0x81` | **CRC_MODE** | 2 | RW | Polynomial select (0/1/2) |

**Flags:** None modified.

**Example — CRC32 of a 512-byte sector:**

```asm
CRC.MODE  0           ; FB 04 00 — select CRC32
CRC.INIT              ; FB 00    — init to 0xFFFFFFFF
; R4 = src address, R5 = 64 (iterations for 512 bytes / 8 bytes each)
.loop:
    LDN  R6, R4       ; load 8 bytes
    CRC.Q R6, R6      ; FB 02 66 — feed 8 bytes, result in R6
    ADDI R4, 8
    DEC  R5
    BR.NE .loop
CRC.FIN R0, R0        ; FB 03 00 — finalize into R0
; R0 = CRC32 of sector
```

64 iterations × 1 cycle = 64 cycles for 512 bytes (vs ~118 cycles
via MMIO: 64 × ~1.85 cycles/write including bus overhead).

---

### B.4  SHA-2 Instructions (sub-ops 0x10–0x1F)

#### State mapping

SHA-2 working state is 8 words (a–h).  For SHA-256 these are 32-bit;
for SHA-384/512 they are 64-bit.  State maps to the **256-bit
accumulator** (ACC0–ACC3) plus **R16–R19** (4 GPRs):

| Register | SHA-256 | SHA-512/384 |
|----------|---------|-------------|
| ACC0 | `{h, g, f, e}` (4 × 32-bit packed) | `{b, a}` (2 × 64-bit) |
| ACC1 | `{d, c, b, a}` (4 × 32-bit packed) | `{d, c}` (2 × 64-bit) |
| ACC2 | *(unused, zeroed)* | `{f, e}` (2 × 64-bit) |
| ACC3 | *(unused, zeroed)* | `{h, g}` (2 × 64-bit) |

SHA-256 packs all 8 × 32-bit working variables into ACC0–ACC1 (256
bits).  SHA-512 uses the full 4 × 64-bit = 256 bits across ACC0–ACC3,
with the upper 4 working variables (e–h) in ACC2–ACC3.

#### Message schedule

The 16-word message schedule W[0..15] lives in **tile memory** at the
address given by TSRC0 CSR.  For SHA-256 this is 64 bytes (one tile).
For SHA-512 this is 128 bytes (two tiles, TSRC0 and TSRC0+64).

The CPU's SHA-2 unit reads W entries from tile memory during round
execution — no separate load step needed.

#### Instructions

| Encoding | Mnemonic | Bytes | Cycles | Description |
|----------|----------|-------|--------|-------------|
| `FB 10 imm8` | **SHA.INIT imm8** | 3 | 2 | Init hash state: `imm8[1:0]` selects mode (0=SHA-256, 1=SHA-384, 2=SHA-512).  Loads FIPS 180-4 IV into ACC0–ACC3.  Sets internal mode register. |
| `FB 11` | **SHA.ROUND** | 2 | 64/80 | Execute full compression: run 64 rounds (SHA-256) or 80 rounds (SHA-384/512) over message block at M[TSRC0].  Updates ACC0–ACC3 with intermediate hash.  Includes W schedule expansion. |
| `FB 12` | **SHA.PAD** | 2 | 2–3 | Apply FIPS 180-4 padding to partial block at M[TSRC0].  R0 = byte count in current block.  Writes pad bytes + 64/128-bit length to tile memory.  If two-block pad needed, sets C flag (caller must SHA.ROUND the first block, then SHA.ROUND the pad block). |
| `FB 13 DR` | **SHA.DIN Rd, Rs** | 3 | 1 | Append R[s][7:0] to message buffer at M[TSRC0 + R0].  R0 incremented.  If R0 reaches block size (64 or 128), auto-triggers SHA.ROUND and resets R0=0. |
| `FB 14 DR` | **SHA.DOUT Rd, Rs** | 3 | 1 | Read digest word: `Rd ← ACC_word[R[s] & 7]`.  Index 0–7 selects working variable a–h (i.e. the accumulated hash, big-endian word order). |
| `FB 15` | **SHA.FINAL** | 2 | 66–83 | Convenience: SHA.PAD + SHA.ROUND (+ second SHA.ROUND if two-block pad).  On completion, ACC0–ACC3 hold the final digest.  R0 preserved from before call. |
| `FB 16`–`1F` | *(reserved)* | | | Future: HMAC helpers, etc. |

**New CSRs:**

| CSR Addr | Name | Width | R/W | Description |
|----------|------|-------|-----|-------------|
| `0x82` | **SHA_MODE** | 2 | RW | 0=SHA-256, 1=SHA-384, 2=SHA-512 |
| `0x83` | **SHA_MSGLEN** | 128 | RW | Total message length in bits (for padding).  Two 64-bit CSR reads/writes (0x83 = low, 0x84 = high). |
| `0x84` | **SHA_MSGLEN_HI** | 64 | RW | Upper 64 bits of message length |

**Pipeline:** CPU enters `CPU_SHA` stall state on SHA.ROUND / SHA.FINAL.
The SHA sub-module reads W entries from tile memory via the CPU's
internal memory port (no bus contention — tile memory is per-core BRAM).
At 1 round/cycle: SHA-256 = 64 cycles, SHA-512 = 80 cycles.

**Flags:**
- SHA.PAD: C=1 if two-block pad required (message must be compressed
  before final pad block).
- SHA.ROUND: Z=1 when complete (always, as confirmation).
- SHA.INIT/DOUT/DIN: no flags modified.

**Example — SHA-256 of a 512-byte buffer:**

```asm
SHA.INIT 0             ; FB 10 00 — SHA-256 mode, load IV
; R4 = source address, R5 = 8 (blocks = 512/64)
.loop:
    ; Copy 64 bytes from M[R4] to tile at TSRC0
    ; (use CMOVE or tile LOADC)
    SHA.ROUND          ; FB 11 — 64 rounds, updates ACC
    ADDI R4, 64
    DEC  R5
    BR.NE .loop
SHA.FINAL              ; FB 15 — pad + final round(s)
SHA.DOUT R0, R0        ; FB 14 00 — read word 0 of digest
; ACC0–ACC1 hold the full 32-byte digest
```

8 blocks × 64 cycles + ~68 cycles (final) = ~580 cycles for 512 bytes.
Via MMIO: 512 byte-writes × ~3 cycles + 64 round-cycles + control
overhead ≈ ~1,600 cycles.  **~2.8× speedup.**

---

### B.5  Field ALU Instructions (sub-ops 0x20–0x2F)

#### State mapping

All Field ALU operands and results are 256-bit.  They map to:

- **Operand A:** ACC0–ACC3 (write ACC CSRs before issuing instruction)
- **Operand B:** Tile memory at M[TSRC0] (32 bytes, low half of tile)
- **Result:** Written back to ACC0–ACC3

For `MUL_RAW` (256×256→512-bit), the high 256 bits go to M[TDST]
(32 bytes).

#### Instructions

| Encoding | Mnemonic | Bytes | Cycles | Description |
|----------|----------|-------|--------|-------------|
| `FB 20` | **GF.ADD** | 2 | 1 | `ACC ← (ACC + M[TSRC0]) mod p` |
| `FB 21` | **GF.SUB** | 2 | 1 | `ACC ← (ACC − M[TSRC0]) mod p` |
| `FB 22` | **GF.MUL** | 2 | 1–4 | `ACC ← (ACC × M[TSRC0]) mod p`.  1 cycle for built-in primes, 4 for custom (Montgomery REDC). |
| `FB 23` | **GF.SQR** | 2 | 1–4 | `ACC ← ACC² mod p` |
| `FB 24` | **GF.INV** | 2 | ~767 | `ACC ← ACC^(p−2) mod p` (Fermat's little theorem) |
| `FB 25` | **GF.POW** | 2 | ~767 | `ACC ← ACC^(M[TSRC0]) mod p` (binary method, exponent from tile) |
| `FB 26` | **GF.MULR** | 2 | 1 | Raw 256×256→512: `{M[TDST], ACC} ← ACC × M[TSRC0]` (no reduction) |
| `FB 27` | **GF.MAC** | 2 | 1–4 | `ACC ← (ACC_prev + operand_a × M[TSRC0]) mod p`.  Uses internally latched previous ACC for accumulate. |
| `FB 28` | **GF.MACR** | 2 | 1 | Raw MAC: `{M[TDST], ACC} ← prev_512 + ACC × M[TSRC0]` |
| `FB 29` | **GF.CMOV Rd** | 3 | 1 | Constant-time conditional move: if `R[d] != 0`, ACC ← M[TSRC0]; else ACC unchanged.  No flags, no branch — constant-time for side-channel resistance. |
| `FB 2A` | **GF.CEQ** | 2 | 1 | Constant-time equality: Z=1 if ACC == M[TSRC0], Z=0 otherwise.  Constant-time (no early-exit). |
| `FB 2B imm8` | **GF.PRIME imm8** | 3 | 1 | Select prime: `imm8[1:0]` = 0: Curve25519 (2²⁵⁵−19), 1: secp256k1, 2: P-256 (NIST), 3: custom.  Latches reduction mode. |
| `FB 2C` | **GF.LDPRIME** | 2 | 1 | Load custom prime: `p ← ACC`, `p_inv ← M[TSRC0]`.  For Montgomery REDC with `prime_sel=3`. |
| `FB 2D` | **GF.X25519** | 2 | ~4335 | Full X25519 scalar multiply (RFC 7748): scalar from ACC, u-coordinate from M[TSRC0].  Result → ACC.  Forces `prime_sel=0` internally. |
| `FB 2E`–`2F` | *(reserved)* | | | |

**New CSRs:**

| CSR Addr | Name | Width | R/W | Description |
|----------|------|-------|-----|-------------|
| `0x85` | **GF_PRIME_SEL** | 2 | RW | Active prime: 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom |

**Pipeline:** Single-cycle ops (GF.ADD/SUB/MUL with built-in primes) are
combinational — no stall.  Multi-cycle ops (GF.INV, GF.POW, GF.X25519,
GF.MUL with custom prime) enter `CPU_GFALU` stall state.  The sub-module
reads operand B from tile memory via the CPU's internal port (same
mechanism as SHA.ROUND).

**Flags:**
- GF.CEQ: Z flag (constant-time).
- All others: no flags modified (crypto operations should not leak
  information through flags).

**Example — X25519 key exchange:**

```asm
; R0 = pointer to 32-byte private key
; R1 = pointer to 32-byte peer public key (u-coordinate)
; Load scalar into ACC0–ACC3
LDN R4, R0         ; load 8 bytes
CSRW ACC0, R4
ADDI R0, 8
LDN R4, R0
CSRW ACC1, R4
ADDI R0, 8
LDN R4, R0
CSRW ACC2, R4
ADDI R0, 8
LDN R4, R0
CSRW ACC3, R4
; Set TSRC0 to peer public key
CSRW TSRC0, R1
; Execute X25519
GF.X25519           ; FB 2D — ~4335 cycles
; Result in ACC0–ACC3 (32-byte shared secret)
CSRR R4, ACC0
STR  R8, R4        ; store to output buffer
; ... store ACC1–ACC3 similarly
```

~4,335 cycles total.  Via MMIO: same compute + ~56 cycles MMIO overhead
(8 writes operand_a + 8 writes operand_b + 1 write CMD + 8 reads result +
polling) ≈ ~4,391 cycles.  Modest savings for X25519 itself (compute-
dominated), but GF.MUL/GF.ADD in inner loops of Ed25519 signature
verification save ~3 cycles per operation — significant across hundreds
of field ops per signature.

---

### B.6  Complete EXT.CRYPTO Sub-Op Map

| Range | Unit | Count | Status |
|-------|------|-------|--------|
| `0x00–0x0F` | CRC32/CRC64 | 5 used, 11 reserved | Proposed |
| `0x10–0x1F` | SHA-2 (256/384/512) | 7 used, 9 reserved | Proposed |
| `0x20–0x2F` | Field ALU (multi-prime) | 14 used, 2 reserved | Proposed |
| `0x30–0xFF` | *(free — 208 slots)* | | Future |

**Total new instructions: 26** (5 CRC + 7 SHA-2 + 14 Field ALU).

---

### B.7  New CSR Summary

| CSR Addr | Name | Width | R/W | Description |
|----------|------|-------|-----|-------------|
| `0x80` | CRC_ACC | 64 | RW | Running CRC accumulator |
| `0x81` | CRC_MODE | 2 | RW | Polynomial select |
| `0x82` | SHA_MODE | 2 | RW | SHA-2 algorithm select |
| `0x83` | SHA_MSGLEN | 64 | RW | Message length (low) |
| `0x84` | SHA_MSGLEN_HI | 64 | RW | Message length (high) |
| `0x85` | GF_PRIME_SEL | 2 | RW | Active field prime |

CSR range 0x80–0x8F reserved for crypto.  6 used, 10 free.

---

### B.8  Instruction Length Summary (Crypto Additions)

| Instruction | Encoding | Bytes | +REX |
|-------------|----------|-------|------|
| CRC.INIT | FB 00 | 2 | — |
| CRC.B | FB 01 DR | 3 | 4 |
| CRC.Q | FB 02 DR | 3 | 4 |
| CRC.FIN | FB 03 DR | 3 | 4 |
| CRC.MODE | FB 04 imm8 | 3 | — |
| SHA.INIT | FB 10 imm8 | 3 | — |
| SHA.ROUND | FB 11 | 2 | — |
| SHA.PAD | FB 12 | 2 | — |
| SHA.DIN | FB 13 DR | 3 | 4 |
| SHA.DOUT | FB 14 DR | 3 | 4 |
| SHA.FINAL | FB 15 | 2 | — |
| GF.ADD | FB 20 | 2 | — |
| GF.SUB | FB 21 | 2 | — |
| GF.MUL | FB 22 | 2 | — |
| GF.SQR | FB 23 | 2 | — |
| GF.INV | FB 24 | 2 | — |
| GF.POW | FB 25 | 2 | — |
| GF.MULR | FB 26 | 2 | — |
| GF.MAC | FB 27 | 2 | — |
| GF.MACR | FB 28 | 2 | — |
| GF.CMOV | FB 29 DR | 3 | 4 |
| GF.CEQ | FB 2A | 2 | — |
| GF.PRIME | FB 2B imm8 | 3 | — |
| GF.LDPRIME | FB 2C | 2 | — |
| GF.X25519 | FB 2D | 2 | — |

Most crypto instructions are 2-byte (prefix + sub-op), with no register
operand — they implicitly use ACC and tile memory.  This is deliberate:
crypto operations are high-latency and operate on wide data, so the
2-register-nibble encoding space isn't useful.  Data is staged via
CSR writes and tile loads.

---

### B.9  MMIO Migration Plan

When the ISA extension is implemented, the shared MMIO peripherals for
CRC, SHA-256, and Field ALU become **redundant for full cores**.  The
migration is:

| Phase | Action | Status |
|-------|--------|--------|
| **1 — Coexistence** | Both MMIO and ISA paths exist.  BIOS crypto words detect core type (full vs micro) and dispatch accordingly.  Full cores use ISA; micro-cores use MMIO (via bus). | ✅ CRC done |
| **2 — MMIO deprecation** | Once all crypto BIOS words use ISA on full cores, the shared MMIO instances are only needed for micro-cores (if they do crypto at all).  If they don't, the MMIO blocks can be removed entirely. | ✅ CRC done |
| **3 — Area recovery** | Removing shared CRC + SHA-256 + Field ALU MMIO blocks saves ~8K gates + MMIO bus decode logic.  This partially offsets the per-core replication cost. | ✅ CRC done |

**CRC MMIO removal (DONE):** `mp64_crc.v` is no longer instantiated
anywhere.  The MMIO address at 0x980 is freed.  `CRCDevice` removed
from the emulator.  BIOS CRC words rewritten to use ISA instructions.
Micro-cores access CRC through the cluster-shared `mp64_crc_isa`
engine with a hardware-lock arbiter (CRC.INIT acquires, CRC.FIN
releases).  The old `mp64_crc.v` file remains on disk but is orphaned.

**MMIO addresses freed:**

| Range | Former Peripheral | New Status |
|-------|-------------------|------------|
| 0x840–0x87F | Field ALU | Free (or retained for micro-core access) |
| 0x940–0x9BF | SHA-256/384/512 | Free (or retained for micro-core access) |
| 0x980–0x9BF | CRC (original) | **Freed** — MMIO CRC fully removed |

The WOTS+ chain accelerator (§7, MMIO 0x8A0) remains shared — it's a
DMA-driven sequencer that chains SHA-3 rounds, not something that maps
to a core instruction.

---

### B.10  Topology Table (Updated)

| Category | Current | Proposed | Rationale |
|----------|---------|----------|-----------|
| CRC32/CRC64 | ~~Shared MMIO~~ **REMOVED** | **Per-core ISA + cluster-shared (hw lock)** | ✅ DONE — MMIO removed, cluster arbiter for micro-cores |
| SHA-256/384/512 | Shared MMIO (1 instance) | **Per-core ISA (EXT.CRYPTO)** | 64–80 round compute; parallel TLS needs per-core |
| Field ALU | Shared MMIO (1 instance) | **Per-core ISA (EXT.CRYPTO)** | Inner-loop field ops in Ed25519/X25519; 3-cycle bus overhead per op is 4× the compute |
| AES-256-GCM | Shared MMIO | Shared MMIO | Large key schedule; AES-NI style would need 240-byte state per core |
| SHA-3/SHAKE | Shared MMIO | Shared MMIO | 1600-bit Keccak state doesn't fit core registers |
| NTT | Shared MMIO | Shared MMIO | Polynomial array in dedicated BRAM |
| KEM | Shared MMIO | Shared MMIO | Multi-phase protocol atop NTT + SHA-3 |
| WOTS+ chain | Shared MMIO | Shared MMIO | DMA sequencer chaining SHA-3 |
| TRNG | Shared MMIO | Shared MMIO | Noise source is inherently singular |
| String engine | Per-core ISA (F9) | Per-core ISA (F9) | No change |
| Dict search | Per-core ISA (FA) | Per-core ISA (FA) | No change |
| Bitfield ops | Per-core ISA (C8–CF) | Per-core ISA (C8–CF) | No change |

---

### B.11  Emulator Dispatch Reference (Crypto)

```python
# --- EXT.CRYPTO (FB) ---
# ibuf[0]=FB, ibuf[1]=sub_op, ibuf[2]=DR (if 3-byte)
elif ext_op == 0xFB:
    sub = ibuf[1]
    unit = (sub >> 4) & 0xF
    op   = sub & 0xF

    if unit == 0x0:  # --- CRC ---
        if op == 0x0:    # CRC.INIT
            crc_acc = 0xFFFFFFFF if crc_mode < 2 else 0xFFFFFFFFFFFFFFFF
        elif op == 0x1:  # CRC.B
            rd, rs = decode_DR(ibuf[2])
            crc_acc = crc_update_byte(crc_acc, R[rs] & 0xFF, crc_mode)
            R[rd] = crc_acc
        elif op == 0x2:  # CRC.Q
            rd, rs = decode_DR(ibuf[2])
            for i in range(8):
                crc_acc = crc_update_byte(crc_acc, (R[rs] >> (i*8)) & 0xFF, crc_mode)
            R[rd] = crc_acc
        elif op == 0x3:  # CRC.FIN
            rd, rs = decode_DR(ibuf[2])
            mask = 0xFFFFFFFF if crc_mode < 2 else 0xFFFFFFFFFFFFFFFF
            R[rd] = crc_acc ^ mask
        elif op == 0x4:  # CRC.MODE
            crc_mode = ibuf[2] & 0x03

    elif unit == 0x1:  # --- SHA-2 ---
        if op == 0x0:    # SHA.INIT
            sha_mode = ibuf[2] & 0x03
            load_sha_iv(sha_mode)   # → ACC0–ACC3
            sha_msglen = 0
        elif op == 0x1:  # SHA.ROUND
            W = read_tile_as_words(TSRC0, sha_mode)
            sha_compress(ACC, W, sha_mode)  # 64 or 80 rounds
        elif op == 0x2:  # SHA.PAD
            sha_pad(TSRC0, R[0], sha_msglen, sha_mode)
        elif op == 0x5:  # SHA.FINAL
            sha_pad_and_compress(TSRC0, R[0], sha_msglen, sha_mode)
        # ... SHA.DIN, SHA.DOUT similarly

    elif unit == 0x2:  # --- Field ALU ---
        B = read_256bit_from_tile(TSRC0)
        A = (ACC3 << 192) | (ACC2 << 128) | (ACC1 << 64) | ACC0
        if op == 0x0:    # GF.ADD
            store_acc(field_add(A, B, prime))
        elif op == 0x1:  # GF.SUB
            store_acc(field_sub(A, B, prime))
        elif op == 0x2:  # GF.MUL
            store_acc(field_mul(A, B, prime))
        elif op == 0x3:  # GF.SQR
            store_acc(field_mul(A, A, prime))
        elif op == 0x4:  # GF.INV
            store_acc(field_inv(A, prime))
        elif op == 0x5:  # GF.POW
            store_acc(field_pow(A, B, prime))
        elif op == 0x6:  # GF.MULR
            result_512 = A * B
            store_acc(result_512 & MASK256)
            write_256bit_to_tile(TDST, result_512 >> 256)
        elif op == 0xD:  # GF.X25519
            store_acc(x25519(A, B))
        # ... etc.
```

*(Pseudocode — actual implementation will use proper 256-bit arithmetic,
REX-extended register indices for GF.CMOV, and CSR read/write for acc.)*

---

### B.12  Open Questions

1. **SHA-2 W schedule location:** This spec uses TSRC0 (tile memory)
   for the message block.  Alternative: use a dedicated 128-byte
   internal buffer (avoids tying up a tile slot during hashing).
   **Recommendation:** TSRC0 — it's already there, doesn't need new
   BRAM, and hashing rarely coincides with tile compute.  If it does,
   caller saves/restores TSRC0.

2. **CRC.Q byte order:** Should CRC.Q process R[s] bytes in LE order
   (byte 0 = bits 7:0 first) or memory order?  **Recommendation:** LE
   (native word order) — matches `LDN` which loads LE from memory.
   Software that needs big-endian CRC can BSWAP first.

3. **Field ALU DSP48 sharing:** Each core gets its own 256×256
   multiplier, which is the biggest resource cost (~40 DSP48 per core,
   ~160 total for 4 cores).  Alternative: share a single multiplier
   with round-robin access (saves ~120 DSP48 but adds 3-cycle latency
   per field op and serialises cores).  **Recommendation:** Per-core —
   the whole point of moving into the core is avoiding contention.
   If DSP48 budget is tight, the multiplier can be time-shared within
   a single core (the SHA unit doesn't need it simultaneously).

4. **AES-round instruction (future):** AES has the largest state
   (key schedule) but a single AES round instruction (`AESENC`,
   `AESDEC`, like x86 AES-NI) operating on a 128-bit state in
   ACC0–ACC1 with a round key from tile memory is feasible.  This
   would allow software-scheduled AES with ~14 AESENC instructions
   per block.  Deferred to a future appendix — the key schedule
   management adds software complexity.

5. **Interrupt behaviour during long operations:** GF.INV (~767
   cycles), GF.X25519 (~4335 cycles) — should these be interruptible?
   **Recommendation:** No.  These are the same order of magnitude as
   a SHA-3 Keccak-f (24 cycles × many absorbs) and the existing MMIO
   peripherals aren't interruptible either.  If preemption is needed,
   software breaks the operation into smaller field ops.

---

## Checklist — SoC Hardening Status

### §0 — STXI / STXD.D Instructions + IO OUT Bug Fix

- [x] ISA encoding designed (opcodes 89, 8B)
- [x] RTL implemented (`mp64_cpu.v` decode bypass)
- [x] Emulator implemented (`megapad64.py`)
- [x] Assembler support (`asm.py`)
- [x] IO OUT bug fixed (LDXA opcode 8F collision)
- [x] Smoke tests passing

### §1 — SHA-256/384/512 Unified Engine (→ per-core ISA, Appendix B)

- [x] Spec complete (modes, datapath design, area estimate)
- [x] RTL: per-core `mp64_sha2_isa` datapath (SHA-256 mode, 64-round) (2026-03-10)
- [x] RTL: integrate into `mp64_cpu.v` as tightly-coupled sub-module (2026-03-10)
- [x] RTL: ISA decode for SHA.INIT / SHA.ROUND / SHA.FINAL (FB 10–15) (2026-03-10)
- [x] RTL: testbench `tb_sha2_isa.v` — 7/7 NIST vectors passing (2026-03-10)
- [x] Emulator: SHA-2 ISA instructions (EXT.CRYPTO FB 1x) (2026-03-10)
- [x] BIOS: crypto words updated to use ISA path (full cores) (2026-03-10)
- [ ] MMIO fallback for micro-cores (if needed)
- [x] Tests (2026-03-10, 1717 passed / 35 skipped / 0 failures)

### §2 — Forth-Aware String Engine (EXT.STRING, prefix F9)

- [x] ISA encoding designed (F9 00–04)
- [x] RTL implemented (`mp64_string.v`)
- [x] Emulator implemented (`megapad64.py`)
- [x] Assembler support
- [x] BIOS words using hardware: CMOVE, CMOVE>, FILL, MOVE, COMPARE
- [x] Tests passing
- [x] C++ accelerator (`mp64_accel.cpp`) — native `exec_string()` (2026-03-09)

### §3 — Forth Dictionary Search Engine (EXT.DICT, prefix FA)

- [x] ISA encoding designed (FA 00–03)
- [x] RTL implemented (`mp64_dict.v`, 490 lines, 4-way SA, FNV-1a)
- [x] Emulator implemented (`megapad64.py`, Python dict fallback)
- [x] BIOS using hardware: `find_word` → DFIND fast path, DINS on miss
- [x] Tests passing (7 sub-tests)
- [x] C++ accelerator (`mp64_accel.cpp`) — native `exec_dict()` + dict_table (2026-03-09)

### §4 — Other Accelerator Ideas

**DONE (4e — Bitfield ALU, per-core ISA, C8–CF + RTL):** (2026-03-09)

- [x] 4e. Bitfield ALU — ISA encoding designed (Appendix A.4)
- [x] 4e. Bitfield ALU — RTL implemented (`mp64_bitfield.v`, Tier 1/2 generate, 49/49 assertions)
- [x] 4e. Bitfield ALU — emulator implementation (`megapad64.py _exec_muldiv` sub 0x8–0xF)
- [x] 4e. Bitfield ALU — C++ accelerator (`mp64_accel.cpp` case 0x8–0xF with __builtin intrinsics)
- [x] 4e. Bitfield ALU — tests (`test_bitfield_alu` — POPCNT/CLZ/CTZ/BITREV/BEXT/BDEP/RORI/BSWAP)
- [x] 4e. Bitfield ALU — assembler (`asm.py` MULDIV_SUB extended, RORI 3-byte form)
- [x] 4e. Bitfield ALU — BIOS words (POPCNT/CLZ/CTZ/BITREV/BSWAP/NTOH/HTON/NTOH32/HTON32/NTOH16/HTON16/POOL-ALLOC/POOL-FREE/POOL-COUNT)
- [x] 4e. Bitfield ALU — ISA reference updated (isa-reference.md Family 0xC)
- [x] 4e. Bitfield ALU — micro-core Tier 1/2 gating (Tier 1 local, Tier 2 traps)
- [x] 4e. Bitfield ALU — CPU integration (`mp64_cpu.v` + `mp64_cpu_micro.v` bf_active path)

**Ideas (not yet committed):**

- [ ] 4a. Crypto Pipeline Orchestrator — idea only
- [ ] 4b. Hardware Compress/Decompress (Deflate) — idea only
- [ ] 4c. Pattern Matching / Regex NFA Engine — idea only
- [ ] 4d. Fixed-Point DSP / FIR Filter — idea only
- [ ] 4f. Stack-Machine Debug / Trace Unit — idea only

### §5 — Port I/O Bridge (1802 OUT/INP → MMIO)

- [x] Spec complete (7-port remap CSR at 0x880)
- [x] RTL remap CSR + combinational decode
- [x] Emulator implemented
- [x] BIOS port map configured (ports 1–6 assigned)
- [x] Tests passing

### §7 — WOTS+ Chain Accelerator (MMIO 0x8A0)

- [x] Spec complete (MMIO register map, FSM, cycle budget)
- [x] RTL spec (`mp64_wots.v`)
- [x] Emulator implemented
- [x] BIOS Forth words (WOTS-CHAIN, WOTS-STATUS@)
- [x] Python tests (8 tests)
- [x] RTL tests (38/38)

### §8 — MMIO Map

- [x] Map documented and updated
- [x] Crypto ISA migration annotated (Field ALU, SHA-2, CRC → per-core)

### §9 — Bus Arbiter MMIO/MEM ACK Timeout

- [x] RTL implemented (watchdog counter, `0xDEAD_DEAD` sentinel)
- [x] Emulator implemented
- [x] Tests passing

### §10 — BIOS SHA3/WOTS Lock Guards + Diagnostic Words

- [x] C++ accelerator lock/unlock guards
- [x] BIOS diagnostic words (SHA3-STATUS@, WOTS-STATUS@, etc.)
- [x] Tests passing (8 tests)

### Appendix A — Pre-Implementation ISA Details

- [x] A.1: EXT prefix slot map (F9, FA, FB allocated)
- [x] A.2: EXT.STRING encoding spec (F9 00–04)
- [x] A.3: EXT.DICT encoding spec (FA 00–03)
- [x] A.4: Bitfield ALU encoding spec (C8–CF)
- [x] A.5: Complete Family 0xC map
- [x] A.6: Instruction length summary
- [x] A.7: Emulator dispatch pseudocode

### Appendix B — Core-Integrated Crypto ISA Extension (EXT.CRYPTO, prefix FB)

- [x] B.0: Rationale + per-core area budget
- [x] B.1: EXT prefix slot map updated (FB assigned)
- [x] B.2: Encoding scheme (sub-op high/low nibble split)
- [x] B.3: CRC ISA spec (5 instructions, 2 new CSRs)
- [x] B.4: SHA-2 ISA spec (7 instructions, 3 new CSRs)
- [x] B.5: Field ALU ISA spec (14 instructions, 1 new CSR)
- [x] B.6: Sub-op map (26 total instructions)
- [x] B.7: New CSR summary (0x80–0x85)
- [x] B.8: Instruction length summary
- [x] B.9: MMIO migration plan (3 phases)
- [x] B.10: Topology table updated
- [x] B.11: Emulator dispatch pseudocode
- [x] B.12: Open questions documented
- [x] Emulator: CRC ISA instructions (EXT.CRYPTO FB 0x–0F)
- [x] Emulator: SHA-2 ISA instructions (EXT.CRYPTO FB 1x) (2026-03-10)
- [x] Emulator: Field ALU ISA instructions (EXT.CRYPTO FB 2x) (2026-03-11)
- [x] RTL: per-core CRC datapath
- [x] RTL: per-core SHA-2 datapath (mp64_sha2_isa.v + tb, 7/7 NIST) (2026-03-10)
- [x] RTL: per-core Field ALU datapath (mp64_field_alu_isa.v + tb, 33/33) (2026-03-11)
- [x] RTL: instruction decode for FB prefix
- [x] C++ accelerator: EXT.CRYPTO dispatch
- [x] C++ accelerator: SHA-2 sub-ops (sha.init/din/dout/final) (2026-03-10)
- [x] BIOS: crypto words updated to use ISA path (CRC + SHA-2) (2026-03-10)
- [ ] MMIO shared instances: removal
- [x] Tests: CRC ISA
- [x] Tests: SHA-2 ISA (2026-03-10)
- [x] Tests: Field ALU ISA (Python 47/47, RTL 33/33) (2026-03-11)

### Overall Progress

| Category | Done | Remaining |
|----------|------|-----------|
| Spec / design | 12 | 0 |
| RTL | 10 | 3 |
| Emulator (Python) | 8 | 1 |
| C++ accelerator | 4 | 2 |
| BIOS | 6 | 2 |
| Tests | 9 | 1 |
| **Total** | **45** | **13** |