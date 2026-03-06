# SoC Hardening Roadmap

Status: planning  
Last updated: 2026-03-06

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

- **String engine** — cluster-local DMA for CMOVE/FILL/COMPARE
- **Dictionary search engine** — cluster-local hardware FIND
- **Trace tap** — per-core ring buffer (see §4f)
- **Per-core timer / watchdog** — local tick counting without
  contending on the shared timer
- **Local IPI slot** — fast-path inter-core signalling that skips
  the mailbox for intra-cluster messages
- **Performance counters** — cycle count, stall count, cache miss
  count; per-core by nature, useless if shared
- **MPU / privilege state** — already partly inside `mp64_cluster.v`;
  the wrapper is where this belongs

Area cost per cluster is modest (~500 LUTs + ~5 BRAMs for string +
dict + trace).  The existing scratchpad already in each cluster costs
more.  Not a resource concern on any target.

| Category           | Topology               | Rationale                            |
|--------------------|------------------------|--------------------------------------|
| Crypto (SHA/AES/Field/NTT/KEM) | Shared (1 instance) | Long-running, one owner at a time |
| TRNG, CRC, RTC     | Shared                 | Stateless or rarely contested        |
| String engine       | **Per-cluster**        | High-frequency from all threads      |
| Dictionary search   | **Per-cluster**        | Hottest path in Forth interp         |
| Debug/trace unit    | **Per-core**           | Meaningless if interleaved           |
| Bitfield ops        | **ISA extension, not MMIO** | Single-cycle; bus latency kills it |

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

## 2. Forth-Aware DMA / String Engine

**Priority: high — directly accelerates core Forth workloads**  
**Topology: per-cluster**

A hardware block-move/fill/compare accelerator that understands Forth
`CMOVE`, `CMOVE>`, `FILL`, `COMPARE`, `SEARCH` semantics natively.

### Motivation

Bulk memory operations are among the most frequent hot loops in a Forth
system — dictionary copying, screen buffer fills, block transfers.
Today these run as CPU byte-loops.  A dedicated engine retires one
operation per bus cycle without tying up the CPU pipeline.

### Why per-cluster

Every core does `CMOVE`/`FILL` frequently.  A single shared instance
means one core's 4 KB CMOVE blocks another's 16-byte FILL.

With uniform cluster wrapping (see Topology Principle above), every
core lives inside a cluster.  Each cluster instantiates a private
string engine accessed via the cluster-internal address space, never
hitting the main bus.

### Register map sketch (~64 bytes)

| Offset | Name     | Description                              |
|--------|----------|------------------------------------------|
| 0x00   | CMD      | 1=CMOVE, 2=CMOVE>, 3=FILL, 4=COMPARE, 5=SEARCH |
| 0x08   | STATUS   | busy / done / result                     |
| 0x10   | SRC      | source address (64-bit)                  |
| 0x18   | DST      | destination address (64-bit)             |
| 0x20   | LEN      | byte count                               |
| 0x28   | FILL_VAL | fill byte / search pattern               |
| 0x30   | RESULT   | COMPARE result (−1/0/1) or SEARCH offset |

### RTL implementation notes

- New module `mp64_string_engine.v`.  Simple FSM:
  IDLE → SETUP → TRANSFER (bus read → bus write per beat) → DONE.
- Transfer loop issues 64-bit aligned loads/stores where possible;
  byte fix-up at head/tail for unaligned addresses.
- `CMOVE` increments src/dst; `CMOVE>` decrements from end —
  overlapping-region safety matching Forth semantics.
- `COMPARE` reads byte pairs, short-circuits on first mismatch.
- Needs a bus master port (like CPU).  Wire into the cluster's
  internal round-robin arbiter.
- Cluster integration: instantiate inside `mp64_cluster.v` next to the
  scratchpad.  Cluster arbiter already handles N micro-core ports;
  add one more for the string engine.  Address decode: a new SPAD-like
  high-address sentinel routes to the engine instead of the bus.

### Emulator implementation notes

- New `StringEngineDevice` in `devices.py`.  CMD write triggers
  immediate Python `memory[dst:dst+len] = memory[src:src+len]` (or
  `bytearray` ops for COMPARE/SEARCH).  Set done flag same cycle.
- Single instance is fine for the emulator (no real contention
  in single-threaded Python; multi-cluster topology is a RTL concern).
- Wire into MMIO dispatch; expose as Forth words `HW-CMOVE`,
  `HW-FILL`, etc.  Could override standard `CMOVE` to use hardware
  when length exceeds a threshold (e.g., >8 bytes).

---

## 3. Forth Dictionary Search Engine

**Priority: high — unique competitive advantage**  
**Topology: per-cluster**

A hardware-accelerated `FIND` that replaces linked-list traversal with
a hash lookup, returning the xt in a few cycles.

### Motivation

Dictionary search is the single most frequent operation during Forth
compilation and interpretation.  Every word typed at the console walks
the dictionary linked list — O(n) in vocabulary size.  With a ~500-word
BIOS dictionary, that's hundreds of byte-comparisons per lookup.

No other Forth system has this in hardware.

### Why per-cluster

If all threads are interpreting/compiling Forth, they all call `FIND`
on every word.  A single-ported hash table becomes a serialisation
point on the hottest path.

With uniform cluster wrapping, every cluster gets its own BRAM-backed
hash table.  INSERT broadcasts to all copies (infrequent; during
compilation only).  FIND is entirely cluster-local — zero main-bus
traffic.

### Approach

Hardware hash table indexed by a hash of the word name.  On definition,
BIOS writes INSERT.  On lookup, software writes counted string to DIN;
engine hashes and returns matching NFA/xt (or "not found") in 2–4 cycles.

### Register map sketch (~64 bytes)

| Offset | Name    | Description                                |
|--------|---------|--------------------------------------------|
| 0x00   | CMD     | 1=INSERT, 2=FIND, 3=DELETE, 4=CLEAR        |
| 0x08   | STATUS  | busy / done / found flag                   |
| 0x10   | DIN     | byte input (counted string, max 31 chars)  |
| 0x18   | NFA_OUT | name-field address of match                |
| 0x20   | XT_OUT  | execution token of match                   |
| 0x28   | ENTRIES | current table occupancy                    |

### Open questions

- Table size vs BRAM cost.  256-entry table with 4-way associative
  lookup ≈ 2–4 BRAM blocks.  Collisions fall back to software.
- Vocabulary support: ALSO/ONLY search order means multiple tables
  or a priority chain.
- Forgetting words (FORGET/MARKER) needs DELETE or bulk invalidation.
- Hash function: FNV-1a or CRC-based?  Must be cheap in gates and
  deterministic across all copies.

### RTL implementation notes

- New module `mp64_dict_engine.v`.
- Core: 256-entry × (32-bit hash + 64-bit NFA + 64-bit XT + 5-bit
  name_len + 31-byte name) — fits in ~4 BRAM36 blocks per instance.
- 4-way set-associative: hash[7:0] selects set, 4 entries per set.
  On collision all 4 ways are checked in parallel (4 comparators).
- INSERT: hash the name, find an empty way in the set, write entry.
  If all 4 ways full, set an overflow flag — software falls back to
  linked-list search for that word.
- FIND: hash the name, read all 4 ways, compare names in parallel,
  output NFA/XT of first match.  2 cycles (hash + compare).
- DELETE: mark entry invalid by zeroing name_len.
- CLEAR: bulk-zero all valid bits (1 cycle, all BRAM write ports).
- Cluster integration: same as string engine — instantiate inside
  `mp64_cluster.v`, expose via cluster-local address range.
- INSERT broadcast: when any cluster's engine receives an INSERT, the
  SoC fans it out to all other cluster copies via a shared
  `dict_insert_broadcast` bus (hash + NFA + XT + name, active for
  1 cycle).  Each engine snoops the broadcast and writes the entry
  into its own table.  Infrequent (only on new definitions), so
  bus bandwidth is negligible.

### Emulator implementation notes

- New `DictSearchDevice` in `devices.py`.  Backed by a Python `dict`
  mapping name → (NFA, XT).
- INSERT: `self.table[name] = (nfa, xt)`.
- FIND: `self.table.get(name, None)`.  O(1) in Python, accurate model.
- Single instance is fine for emulator (no contention issue).
- Hook into BIOS `:` (colon) and `CREATE` to auto-INSERT.
- Hook `FIND` to try hardware lookup first, fall back to linked list
  on miss (collision overflow path).

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

### 4e. Bitmap / Bitfield Accelerator — **should be ISA extension**

Hardware popcount, CLZ, bit-reverse, bit-scatter/gather, bit-permute.
Tiny footprint, big payoff for memory allocators, graphics blitters,
and crypto.

**NOT MMIO** — single-cycle operations don't survive the bus round-trip.
These should be new ALU opcodes in `mp64_alu.v`.  Adding a `POPCNT`
and `CLZ` case to the existing ALU function mux is ~30 LUTs each.

**RTL:** Add cases to `mp64_alu.v` function select.  Wire result into
existing ALU output mux.
**Emulator:** New opcodes in `megapad64.py` CPU dispatch.  Python
`bin(x).count('1')` for popcount, `x.bit_length()` for CLZ, etc.

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

## 5. MMIO Map After Changes

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
| 0x880–0x8BF   | 64 B   | *(free)*            |            |
| 0x8C0–0x8FF   | 64 B   | NTT                 | existing   |
| 0x900–0x93F   | 64 B   | KEM                 | existing   |
| 0x940–0x9BF   | 128 B  | **SHA-256/384/512** | **widened** |
| 0x9C0–0x9FF   | 64 B   | *(free)*            |            |
| 0xA00–0xA1F   | 32 B   | **CRC (relocated)** | **moved**  |
| 0xA20–0xA7F   | 96 B   | **Trace readout portal** | **new** |
| 0xA80–0xAFF   | 128 B  | *(free — future)*   |            |
| 0xB00–0xB1F   | 32 B   | RTC                 | existing   |
| 0xB20–0xFFF   | 1248 B | *(free)*            |            |

With uniform cluster wrapping, the string engine and dictionary search
engine are **not on the main MMIO bus at all** — they live inside each
cluster wrapper at cluster-local addresses (same register layout,
accessed via the cluster's internal address space).  The main MMIO map
only carries shared peripherals.

The trace readout portal (0xA20) is a small shared MMIO window that
lets a debugger select a core ID and drain that core's per-core trace
ring buffer over the main bus.  The actual trace capture hardware lives
per-core inside each cluster.

Plenty of room for future shared accelerators without touching the
upper half of the address space.
