# SoC Hardening Roadmap

Status: in-progress (§1 crypto DONE, §2 string engine DONE, §5 port I/O bridge DONE)  
Last updated: 2025-07-17

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

**Rule of thumb:** if an accelerator is long-running and one-owner-at-a-time
(crypto engines, compression), sharing is fine — software mutex via
mailbox.  If it's high-frequency and called from every thread on every
Forth word (string ops, dictionary search), sharing creates serialisation
on the hottest paths and the accelerator needs replication.

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
| Crypto (SHA/AES/Field/NTT/KEM) | Shared (1 instance) | Long-running, one owner at a time |
| TRNG, CRC, RTC     | Shared                 | Stateless or rarely contested        |
| String engine       | **ISA extension (EXT.STRING F9)** | 2–3 byte instruction; bus setup ≫ transfer |
| Dictionary search   | **ISA extension (EXT.DICT FA)** | 2–4 cycle FIND; bus latency kills it |
| Debug/trace unit    | **Per-core**           | Meaningless if interleaved           |
| Bitfield ops        | **ISA extension, not MMIO** | Single-cycle; bus latency kills it |

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
**Topology: shared (1 instance)**

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
matching the flat-read pattern in `mp64_sha3.v`.  Relocate CRC from
0x980 to 0xA00 (CRC only uses ~32 bytes, plenty of room).

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

- Copy `mp64_sha256.v` → rename module to `mp64_sha2` (covers 256/384/512).
- All `[31:0]` regs → `[63:0]`.  In mode==0, INIT zeroes upper bits; the
  existing round logic just operates on 64-bit words (upper bits stay 0).
- K table: `function [63:0] K; input [6:0] i;` — 80 entries.  For mode 0,
  only entries 0–63 are used (round_cnt never reaches 64+).
- Σ/σ mux: `wire [5:0] S0_r0 = (mode==0) ? 6'd2 : 6'd28;` etc.
  Feed into a single `rotr64` function.
- `addr` port widens from `[5:0]` to `[6:0]`.
- SoC decode change: `addr[11:7] == 5'b10010` (0x940–0x9BF).
- Move CRC decode to `addr[11:5] == 7'b1010000` (0xA00–0xA1F).

### Emulator implementation notes

- `devices.py` SHA256Device class: add `mode` register, branch on mode
  for `hashlib.sha256()` vs `hashlib.sha384()` vs `hashlib.sha512()`.
- Widen `digest` buffer to 64 bytes; DOUT reads at offset 0x18–0x57.
- Update MMIO dispatch range in `system.py`.
- CRC base address constant update.
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

---

## 4. Other Interesting Accelerator Ideas

Lower priority, but worth keeping on the radar.

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

### 4e. Bitfield Accelerator — ALU extension (no EXT prefix, zero EXT slots)

Single-cycle bit-manipulation operations added as new ALU sub-ops in
`mp64_alu.v`.  **Not MMIO** — single-cycle operations don't survive
the bus round-trip.  **Not an EXT prefix** — these are regular ALU
function codes, encoded within the existing ALU family (0x7).

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
**Status: ☐ Not started**
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
| 0x840–0x87F   | 64 B   | Field ALU           | existing   |
| 0x880–0x89F   | 32 B   | **Port I/O remap CSR** | **new**    |
| 0x8A0–0x8BF   | 32 B   | **WOTS+ Chain Accel** | **new (§7)** |
| 0x8C0–0x8FF   | 64 B   | NTT                 | existing   |
| 0x900–0x93F   | 64 B   | KEM                 | existing   |
| 0x940–0x9BF   | 128 B  | **SHA-256/384/512** | **widened** |
| 0x9C0–0x9FF   | 64 B   | *(free)*            |            |
| 0xA00–0xA1F   | 32 B   | **CRC (relocated)** | **moved**  |
| 0xA20–0xA7F   | 96 B   | **Trace readout portal** | **new** |
| 0xA80–0xAFF   | 128 B  | *(free — future)*   |            |
| 0xB00–0xB1F   | 32 B   | RTC                 | existing   |
| 0xB20–0xFFF   | 1248 B | *(free)*            |            |

The string engine (§2) and dictionary search engine (§3) are **ISA
extensions** (EXT.STRING F9, EXT.DICT FA) — they live inside each CPU
as tightly-coupled sub-modules, like the multiplier.  They are not on
the MMIO bus at all and require no cluster wrapper ports.  The main
MMIO map only carries shared peripherals.

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
FB  —               (free)
FC  —               (free)
FD  —               (free)
FE  —               (free)
FF  —               (free)
```

**Budget:** 3 pre-existing + 5 REX + 2 committed = 10 used, **6 free**.

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