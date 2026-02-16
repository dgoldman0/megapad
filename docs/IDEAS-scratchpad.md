# Ideas Scratchpad

Working design notes for upcoming roadmap items.  These are *living
notes* — rough ideas, trade-offs, open questions — that will be folded
into proper documentation as each item is implemented.  Delete this file
when the items below are all done and documented.

---

## 1. Micro-Core CPU Variant (Roadmap Layer 6, Item 39)

### Goal

Design a stripped-down "MP64µ" core that is **ISA-compatible** at the
base instruction level but dramatically smaller, targeting:

- **No 64-bit hardware multiplier** — the current `mp64_cpu.v` infers
  ~16 DSP48 slices per core for the combinational 64×64→128 multiply.
  A micro-core replaces this with either (a) a multi-cycle shift-add
  multiplier (8–64 cycles, 0 DSPs), or (b) a shared multiplier bus
  that all micro-cores arbitrate for.
- **No I-cache** — the current 4 KiB direct-mapped cache per core uses
  4 BRAM36 each.  Micro-cores fetch directly from the bus (stalls more,
  but saves significant BRAM).  Alternative: a shared 2 KiB I-cache
  across a micro-core cluster.
- **No tile engine** — no MEX instructions, no tile CSRs, no 512-bit
  port.  Micro-cores are scalar-only.  They can still *read/write*
  tile memory as byte/word data, they just can't issue tile operations.
- **No FP16/BF16 ALU** — no `mp64_fp16_alu.v` instance.
- **No BIST controller** — memory BIST stays on the main core(s).
- **No performance counters** — or a minimal 1-counter (cycles only).
- **Reduced register file** — possibly 8 GPRs instead of 16 (the 1802
  heritage already uses selector CSRs, so 8 physical regs + selectors
  is viable; the ISA encoding uses 4 bits for reg, but micro-cores
  could mirror R8–R15 onto R0–R7 or fault).

### What Stays (Common Core)

These elements are **shared** between major and micro cores and should
be factored into a common RTL module or `include` file:

- Instruction decoder (all opcode families except MEX/EXT.MEX)
- ALU (64-bit add/sub/and/or/xor/shift — no multiply)
- Flag register (S I G P V N C Z) and flag-update logic
- Branch/skip logic (BR, LBR, SKIP, SEP, SEX)
- Interrupt controller (IVT, IE, push/pop, RTI)
- CSR read/write (at least: COREID, FLAGS, PSEL, SPSEL, XSEL)
- Memory load/store FSM (MEM_READ, MEM_WRITE states)
- IMM/LDI instruction paths
- IO instructions (INP/OUT for MMIO)

### What Diverges

| Feature | MP64 Major | MP64µ Micro |
|---------|-----------|-------------|
| Multiplier | 64×64→128, 1 cycle, ~16 DSP (~28K GE) | ❌ Illegal-op trap (0 DSP, 0 GE) |
| Divider | Iterative shift-subtract, 64 cycles (~4K GE) | ❌ Illegal-op trap |
| I-Cache | 4 KiB per core (~40K GE SRAM+tags) | None (shared 1–2 KiB cluster cache) |
| Tile engine | Full MEX + DMA + FP16 | ❌ Not present |
| BIST | March C−, checkerboard, addr | ❌ Not present |
| Perf counters | 4 × 64-bit | 1 × 64-bit (cycles only) |
| GPRs | 16 × 64-bit | 16 × 64-bit (same — keep ISA compat simple) |
| Pipeline | 2-stage IF/DEX with prefetch buffer | 1-stage fetch-execute (simpler) |

**Decision:** Keep 16 GPRs.  The ISA encodes 4-bit register fields
everywhere; halving it would require a decoder mode bit and creates
binary incompatibility.  The 16×64-bit register file is only 128 bytes
of LUTRAM — negligible.

### Integration & QoS — Cluster Topology

Micro-cores are **clustered**, not individually connected to the main
bus arbiter.  A cluster of 3–4 micro-cores shares an I-cache and a
local arbiter, presenting as a **single bus port** to the main arbiter.

#### Cluster Architecture

```
┌──────────── Micro-Cluster ─────────────┐
│                                         │
│  ┌────┐  ┌────┐  ┌────┐  ┌────┐       │
│  │ µ0 │  │ µ1 │  │ µ2 │  │ µ3 │       │
│  └──┬─┘  └──┬─┘  └──┬─┘  └──┬─┘       │
│     └───┬────┴───┬───┴───┬───┘         │
│      ┌──┴────────┴───────┴──┐          │
│      │  Cluster Arbiter (RR) │          │
│      │  + Shared I-Cache     │          │
│      │    (1–2 KiB)          │          │
│      └──────────┬────────────┘          │
└─────────────────┤
                  │  single bus port
═══════════════════╪════════════════════════
                Main Bus Arbiter
```

#### Two-Level QoS

**Level 1 — Main arbiter (hard QoS):**

The main bus arbiter uses **weighted round-robin** with per-port
bandwidth limit registers.  Each port is either a full core or a
micro-cluster — the arbiter doesn't know or care what's behind a port.
Because the arbiter physically controls bus grants, this is **hard
QoS** by definition.

- Full core port: `weight = 4` (needs bandwidth for I-cache refills,
  tile DMA, multiply operands).
- Micro-cluster port: `weight = 2` (lower per-port — shared I-cache
  absorbs most instruction fetches).
- BW limit registers: max grants per N-cycle window, configurable
  per port at runtime.
- Guarantee: a full core gets ≥ 2× the bus bandwidth of a
  micro-cluster, regardless of load.

The main arbiter enforces **isolation**: a runaway micro-cluster
cannot starve a full core because the bandwidth limit caps its
grants per time window.  No software cooperation required.

**Level 2 — Intra-cluster (soft, equal peers):**

Inside each cluster, a simple round-robin arbiter rotates among 3–4
micro-cores.  All are equal peers — no weighting.  The cluster arbiter
tags outgoing requests with a 2-bit micro-ID so bus responses route
back to the correct core.

The shared I-cache is the force multiplier: instruction fetches hit
locally (~85–90% hit rate for typical Forth inner loops at 1–2 KiB),
so the cluster's main-bus demand is almost entirely data traffic.
Four micro-cores sharing one bus port naturally self-throttle their
aggregate bandwidth.

#### Why Hard QoS Lives at the Main Arbiter

- **Few ports:** the main arbiter sees only **N ports** (e.g. 7 in
  the Full config: 4 full + 3 clusters), not 16 individual cores —
  arbitration stays single-cycle.
- **Physical control:** the arbiter holds the bus-grant signal; no
  software cooperation required.
- **Per-port BW limits** provide a hard ceiling — even if all 4 micros
  in a cluster fire uncacheable loads simultaneously, the cluster's
  bus port is rate-limited.
- **Intra-cluster** scheduling is purely local: a micro waiting for
  the cluster arbiter does not block any main-arbiter port.

#### Core ID Space

With up to 16 cores, use **4-bit** core IDs:

- IDs 0–3: full cores (or just ID 0 in the Standard config).
- IDs 4–15: micro-cores (3 clusters × 4, or 1 cluster × 3).
- Mailbox and IPI: extend to 4-bit addressing, parameterize
  `NUM_CORES`.
- Spinlocks: already parameterized — works as-is.

### Suggested Configurations

| Config | Full | Micro | Clusters | Bus Ports | Est. ASIC GE | Use Case |
|--------|:----:|:-----:|:--------:|:---------:|:------------:|----------|
| Minimal   | 1 | 0  | 0   | 1 | ~100K | Single-core embedded |
| Standard  | 1 | 3  | 1×3 | 2 | ~153K | IoT / control plane |
| Compute   | 4 | 0  | 0   | 4 | ~400K | Current design (tile-heavy) |
| **Full**  | **4** | **12** | **3×4** | **7** | **~598K** | **General-purpose + helpers** |

The **Full** config (4F+12µ) packs 16 cores into ~1.5× the gate area
of the 4-core Compute config, adding 12 auxiliary threads at ~50%
area overhead.

### SoC Total Size Estimate (Full Config)

| Block | GE | Notes |
|-------|:--:|-------|
| 4 full cores (w/ MUL, I-cache, tile, BIST) | ~400K | 4 × ~100K |
| 3 enriched clusters (4µ + shared MUL + scratchpad + barrier) | ~300K | 3 × ~100K (see Cluster Enrichment below) |
| Peripherals (AES, SHA3, CRC, TRNG, FieldALU, NIC, UART, timer, disk) | ~200K | Existing devices |
| Bus fabric + arbiter + mailbox + spinlocks | ~50K | 7-port weighted RR |
| **Total logic** | **~950K GE** | **Medium SoC** |

Plus 4 MiB SRAM (1 MiB system + 3 MiB HBW).  Comparable in
complexity to an ESP32 or a beefy RISC-V SoC with hardware crypto.
Not small (16-core, hardware crypto, tile engine), not large (no MMU,
no GPU pipeline, no cache coherency protocol).

On **Kintex-7 325T**: ~60–80K LUTs (30–40%), ~80 DSPs (10%), BRAM is
the squeeze point (~54% for 2 MiB internal).

**Scaling down:** drop to Standard config (1F+3µ, ~250K GE total
with peripherals) for a **small** SoC suitable for low-cost FPGA or
ASIC tapeout.

### Cluster Enrichment — Filling 6:1 → 4:1

A bare cluster of 4 micros is ~66K GE — well below the ~100K GE
target ("same scale as one full core").  Budget: ~34K GE.

| Addition | GE | What it does |
|----------|:--:|----|
| **Shared multiplier** (1× Booth-Wallace 64×64→128, arbitrated) | ~28K | MUL goes from illegal-op trap to "works, 4–8 cycles with arbitration wait".  Huge capability uplift.  Four micros time-share one multiplier — most real code doesn't MUL every cycle. |
| **Cluster scratchpad** (1 KiB dual-port SRAM) | ~5K | Fast local memory only the 4 micros see.  Work queues, message passing, shared buffers — no main-bus trip. |
| **Hardware barrier register** | ~1K | 4-bit arrive/depart flags + IRQ-on-all-arrived.  Intra-cluster sync without spinlocks or IPI. |
| **Total enrichment** | **~34K** | |
| **Enriched cluster total** | **~100K** | **= 1 full core footprint** |

The shared multiplier is the big win — it transforms the micro-cores
from "can't do math" to "can do math, just takes turns."  The
scratchpad + barrier make the cluster feel like a cooperative unit
rather than four isolated cores that happen to share a bus port.

### Open Questions

- Should micro-cores share crypto accelerators or have none?  Leaning
  toward *shared* — they can still issue MMIO reads/writes to AES/SHA3/
  etc., the bus handles arbitration naturally.
- Do micro-cores need their own stack region, or can they share from a
  common pool?  Leaning toward *own region* — the per-core stack base
  CSR already exists.
- Should we add a "core type" CSR so software can query whether it's
  running on a major or micro core?  Yes — `CSR_CORE_TYPE` returning
  0=major, 1=micro.  KDOS scheduler can use this to avoid dispatching
  tile kernels to micro-cores.

### Implementation Approach

1. ~~**Factor `mp64_cpu_common.v`**~~ — ✅ Done: `mp64_cpu_common.vh`
   with shared FSM states, ALU opcodes, `cond_eval`, `instr_len`.
2. ~~**Build `mp64_cpu_micro.v`**~~ — ✅ Done: ~1200 lines, 0 DSP,
   MUL/DIV trap as illegal opcode, no I-cache/tile/BIST.
3. ~~**Refactor `mp64_cpu.v`**~~ — ✅ Done: uses shared components,
   bus handshake fix applied to all 3 CPU variants.
4. **Parameterize `mp64_soc.v`** — `NUM_MAJOR_CORES`, `NUM_MICRO_CORES`,
   generate blocks per type, wire cluster topology.
5. **Build cluster module** — `mp64_cluster.v` with shared I-cache,
   shared multiplier (arbitrated), 1 KiB scratchpad, barrier register,
   round-robin among 3–4 micro-cores, single bus port output.
6. **Update main bus arbiter** — weighted round-robin, per-port BW
   limit registers, parameterized port count.
7. **Emulator side:** `megapad64.py` gets a `micro=True` flag that
   traps MUL/DIV.  `system.py` takes `num_major` and `num_micro`
   params, instantiates clusters.
8. **Tests:** extend `TestMicroCore` with cluster-level tests — shared
   I-cache hits, intra-cluster arbitration, QoS interaction.

### Size Comparison — ASIC Gate Estimates

The meaningful size comparison is in **ASIC gate equivalents** (GE,
where 1 GE = one 2-input NAND).  FPGA LUT/FF comparisons are
misleading because the multiplier hides in DSP48 hard macros and the
I-cache hides in BRAM — they look "free" on resource summaries but
dominate die area on silicon.

#### Where the Area Actually Goes

| Component | Major Core | Micro Core | Notes |
|-----------|:----------:|:----------:|-------|
| Register file (16×64b, 2R1W) | ~5K GE | ~5K GE | Identical |
| ALU (16-op, 64-bit) | ~3K GE | ~3K GE | Shared `mp64_alu.v` |
| Decoder / FSM | ~2.5K GE | ~1.5K GE | Major adds MULDIV/MEX/BIST states |
| Bus interface + memory FSM | ~1.5K GE | ~1.5K GE | Near-identical |
| Flags, IRQ, IO, selectors | ~1.5K GE | ~1.5K GE | Same interrupt controller |
| CSRs + perf counters + DMA | ~3.5K GE | ~0.5K GE | Major: +DMA ring +3 perf ctrs |
| **Core logic subtotal** | **~17K GE** | **~13K GE** | **Only 1.3× — noise** |
| Booth-Wallace 64×64→128 MUL | **~28K GE** | 0 | **2× entire micro core** |
| Iterative 64-bit divider | ~4K GE | 0 | Shift-subtract, 64 cycles |
| I-cache 4 KiB SRAM + tags | **~40K GE** | 0 | **3× entire micro core** |
| Tile / MEX / FP16 dispatch | ~8K GE | 0 | Ports + CSRs + mux |
| BIST controller | ~3K GE | 0 | March-C, checkerboard, addr |
| **Full total** | **~100K GE** | **~13K GE** | **~8:1** |

The core logic (register file + ALU + decoder + bus) is nearly the
same — the **hard blocks** dominate the major core:

- **Multiplier alone** (~28K GE) is larger than two entire micro-cores.
- **I-cache alone** (~40K GE) is larger than three entire micro-cores.
- On FPGA these hide in DSP48 and BRAM hard macros, making the
  LUT/FF ratio look deceptively close (~1.5:1).  That number is
  meaningless for area planning.

#### Cluster Overhead

Micro-cores are clustered (see Integration & QoS above).  The cluster
adds shared infrastructure:

| Cluster Component | GE | Notes |
|-------------------|:--:|-------|
| Shared I-cache (1–2 KiB) + tags | ~12K | 4× smaller than full core's cache |
| Cluster arbiter + response tag | ~2K | Round-robin, 2-bit micro-ID |
| **Cluster overhead** | **~14K** | Amortized across 3–4 micros |

A cluster of 4 micros: 4 × 13K + 14K = **~66K GE** → **16.5K GE per
thread**.

A cluster of 3 micros: 3 × 13K + 14K = **~53K GE** → **17.7K GE per
thread**.

#### Packing Ratios

| Metric | Ratio | Notes |
|--------|:-----:|-------|
| Raw core logic (no hard blocks) | 1.3:1 | Register file dominates both — noise |
| Full core vs bare micro | **8:1** | MUL + I-cache + DIV + tile dominate |
| Full core vs clustered micro (per thread) | **6:1** | Cluster overhead amortized |
| Gate area: 4F vs 4F+12µ | 1 : 1.5 | +50% area for +300% threads |

**Practical packing:** ≥ 4 micro-cores fit in the area of one full
core.  With aggressive synthesis (shared BRAM regfiles, gate-level
I-cache), 6–8:1 is plausible on an ASIC standard-cell flow.

#### FPGA vs ASIC — Why the Numbers Diverge

On **FPGA (Kintex-7)**, the apparent LUT/FF ratio is only ~1.5:1
because:

- The 64-bit multiplier maps to ~16 DSP48E1 slices (hard macros,
  zero LUTs — already on die whether used or not).
- The 4 KiB I-cache maps to 4 BRAM36K (hard macros, zero LUTs).
- What remains is just the FSM/decoder difference: ~600 extra LUTs.

This makes the major core look "cheap" on FPGA resource summaries.
But each DSP48 is equivalent to ~25–30K gates of custom silicon, and
each BRAM36 is ~250K GE.  Any ASIC translation must account for them.

#### Test Results

| Testbench          | Pass | Fail | Notes                            |
|--------------------|:----:|:----:|----------------------------------|
| tb_alu.v           | 53   | 0    | All 16 ALU ops + flag edge cases |
| tb_cpu_micro.v     | 11   | 0    | NOP/HALT/LDI/ALU/MEM/BR/TRAP/CSR/SEP |
| tb_cpu_smoke.v     | 19   | 0    | Regression — refactored major core |
| tb_opcodes.v       | 92   | 0    | All pass (EXT.SKIP fix: b15d918) |

---

## 2. Multi-Prime Field ALU (Roadmap Layer 6, Item 40)

### Goal

Make the Field ALU handle multiple elliptic curve primes, not just
$p = 2^{255} - 19$.  Without going overboard.

### Current State

- `mp64_field_alu.v` / `FieldALUDevice` hardcodes $p = 2^{255} - 19$.
- Reduction after multiply uses the special structure of this prime:
  the high 256 bits of a 512-bit product are folded back with `× 38`
  (since $2^{256} \equiv 38 \pmod{p}$).  This is fast and cheap.
- X25519 Montgomery ladder is optimized for this curve.

### Three-Part Improvement (from the user's screenshot)

#### 2a. Programmable Modulus

Support a small set of primes via a **prime select** register:

| Prime ID | Value | Curve(s) | Reduction Method |
|----------|-------|----------|------------------|
| 0 | $2^{255} - 19$ | Curve25519, Ed25519 | Fast: high×38 fold |
| 1 | secp256k1: $2^{256} - 2^{32} - 977$ | Bitcoin | Sparse: subtract shifted |
| 2 | P-256: $2^{256} - 2^{224} + 2^{192} + 2^{96} - 1$ | NIST P-256, TLS | NIST special form |
| 3 | (reserved for generic) | Arbitrary | Montgomery reduction |

**Register map addition:**
- `0x42  PRIME_SEL (W)` — select modulus from built-in table (0–3)
- `0x44  CUSTOM_P  (W)` — write 32 bytes for generic modulus (prime ID 3)

**RTL approach:** Dedicated reduction circuits for primes 0–2 (each
is a few hundred LUTs — the special structure makes them cheap).
Prime 3 uses the generic Montgomery reducer (more expensive, ~2K LUTs +
some DSPs for the Montgomery constant computation).

**Estimated cost:** ~2K additional LUTs, ~0 additional DSPs (the
existing 256-bit multiplier is reused).

#### 2b. Generic Montgomery Reduction Path

For `PRIME_SEL=3` (arbitrary prime), implement Montgomery reduction:

1. On `CUSTOM_P` write: precompute $R = 2^{256}$, $R^2 \bmod p$, and
   $p' = -p^{-1} \bmod R$ (the Montgomery constant).  This is a
   one-time cost (~500 cycles).
2. FMUL/FSQR use Montgomery form: $\text{REDC}(a \cdot b)$.
3. FADD/FSUB work directly (no Montgomery needed).
4. Results are in Montgomery domain; a final REDC converts out.

Slower than the specialized primes (~4× for FMUL due to REDC overhead),
but works for *any* 256-bit prime.

**Trade-off:** We're not trying to be a generic bignum coprocessor.
The Montgomery path is there for flexibility (e.g., testing new curves,
Ed448 with a 448-bit extension later), but the fast paths for the
three common primes handle 99% of real use.

#### 2c. Constant-Time Features for ECC

Required for side-channel resistance in real cryptographic use:

- **Conditional move/select (CMOV):** `RESULT = cond ? A : B` without
  branching.  Implemented as: `RESULT = (A XOR B) AND mask(cond) XOR B`.
  New mode 8: `FCMOV` — reads condition from low bit of OPERAND_B[0],
  selects between OPERAND_A and RESULT.
- **Constant-time inversion:** The current FINV uses Fermat's little
  theorem ($a^{p-2}$) with a fixed exponent — this is already constant-
  time for a given prime since the bit pattern of $p-2$ is fixed.  ✅
  Already OK.
- **No data-dependent early exits:** Verify that the X25519 Montgomery
  ladder (mode 0) always executes exactly 255 iterations regardless of
  scalar bits.  The current RTL does this.  ✅ Already OK.
- **Constant-time comparison:** New mode 9: `FCEQ` — returns 0 or 1
  in RESULT based on whether A == B, in constant time (bitwise OR
  reduction of XOR, then NOT of the OR-reduction's MSB propagation).

**New modes summary:**

| Mode | Name | Description |
|------|------|-------------|
| 8 | FCMOV | Conditional move: `cond ? A : RESULT_LO` |
| 9 | FCEQ | Constant-time equality: `A == B ? 1 : 0` |

### What This Unlocks

With these three improvements, the Field ALU becomes usable for:

- **Ed25519 signatures** (extended twisted Edwards, needs constant-time
  point addition over $2^{255}-19$)
- **secp256k1 ECDSA** (Bitcoin/Ethereum transaction signing)
- **P-256 ECDH/ECDSA** (TLS key exchange, code signing)
- **Future curves** via the generic Montgomery path

Without them, "reuse for Ed25519" stays a research exercise (quoting
the user's own assessment).

### Emulator Implementation

- `FieldALUDevice._execute()`: add `PRIME_SEL` register, table of
  primes, route FMUL/FSQR through the selected prime's reduction.
- Montgomery helpers: `_mont_REDC()`, `_mont_mul()`, precompute on
  `CUSTOM_P` write.
- `FCMOV` and `FCEQ` modes: trivial in Python.

### RTL Implementation

- `mp64_field_alu.v`: add `prime_sel[1:0]` register, mux between
  reduction circuits.  Add `custom_p` 256-bit register.
- New reduction modules: `mp64_field_reduce_secp.v`,
  `mp64_field_reduce_p256.v`, `mp64_field_reduce_mont.v`.
- `FCMOV`/`FCEQ` wired in the COMPUTE state (1 cycle each).

---

## 3. Memory Model Redesign (Roadmap Layer 6, Item 41)

### Goal

Overhaul the memory subsystem from a monolithic 1 MiB block to a
**banked 4 MiB** architecture with differentiated bandwidth tiers,
designed for future expansion with external memory.

### Current State

- `mp64_memory.v`: single 16384×512-bit array = 1 MiB.
- Dual-port: Port A (tile, 512-bit), Port B (CPU, 64-bit).
- No banking, no interleaving.
- External memory forwarding at `addr[63:20] != 0`.
- `mp64_defs.vh`: `INT_MEM_BYTES = 1048576`, `EXT_MEM_MAX = 16 MiB`.

### New Architecture: 4 Banks, 2 Tiers

```
Address Space (22 bits internal = 4 MiB):

  0x00_0000 ┌──────────────────────────┐
            │  Bank 0: Regular BW      │  1 MiB
            │  BIOS, OS, stacks, dict  │
            │  Single CPU port         │
  0x10_0000 ├──────────────────────────┤
            │  Bank 1: High BW         │  1 MiB
            │  Tile data, DMA buffers  │
            │  Dual-port (CPU + Tile)  │
  0x20_0000 ├──────────────────────────┤
            │  Bank 2: High BW         │  1 MiB
            │  Tile data, DMA buffers  │
            │  Dual-port (CPU + Tile)  │
  0x30_0000 ├──────────────────────────┤
            │  Bank 3: High BW         │  1 MiB
            │  Tile data, frame bufs   │
            │  Dual-port (CPU + Tile)  │
  0x40_0000 └──────────────────────────┘

            ... external memory fills here ...

  0xF000_0000  ┌──────────────────────┐
               │  Guard region        │  256 MiB
               │  (unmapped, faults)  │  reserved for future
  0xFFFF_FF00_ │  MMIO registers      │
  0000_0000    └──────────────────────┘
```

### Design Decisions

**Bank 0 — Regular Bandwidth:**
- Standard single-port BRAM (saves 1 BRAM36 port vs dual-port).
- This is where BIOS loads, KDOS compiles, stacks live, heap grows.
- CPU-only access; tile engine cannot directly address bank 0.
- If tile needs to process data from bank 0, software must copy to a
  high-BW bank first (explicit data movement, good for predictability).

**Banks 1–3 — High Bandwidth:**
- Each is a dual-port 16384×512-bit BRAM (same as current design, ×3).
- Port A: 512-bit tile access (one tile per cycle per bank).
- Port B: 64-bit CPU access (load/store, DMA setup).
- The tile arbiter distributes tile ops across banks based on address.
- **Pinned to end of internal address space** — when external memory
  is added, the high-BW banks stay at the *top* of physical memory.
  BIOS/OS knows that addresses ≥ `HBW_BASE` are always fast.

**Pinning Mechanism:**
- `HBW_BASE` is defined as `INT_MEM_TOTAL - 3 * BANK_SIZE`.
- With 4 MiB internal: `HBW_BASE = 0x10_0000`.
- If external memory expands the total to 64 MiB: regular BW fills
  `0x00_0000 – 0x3CF_FFFF` (61 MiB), and high-BW banks occupy
  `0x3D0_0000 – 0x3FF_FFFF` (top 3 MiB).
- Actually, thinking about this more carefully: the high-BW banks
  should have **fixed addresses** regardless of external memory size,
  and external memory fills the gap between bank 0's end and HBW_BASE.
  So the layout is:

```
  0x00_0000   Bank 0 (1 MiB, regular BW)
  0x10_0000   External memory (variable size, regular BW)
    ...
  END-3MiB    Bank 1 (1 MiB, high BW)
  END-2MiB    Bank 2 (1 MiB, high BW)
  END-1MiB    Bank 3 (1 MiB, high BW)
  END         Top of addressable memory
  Guard       256 MiB window before MMIO is unmapped
  MMIO        0xFFFF_FF00_0000_0000
```

Where `END` is configurable (default 4 MiB = 0x40_0000).  When
external memory is present, `END` grows.  The high-BW banks always
sit at `[END-3MiB, END)`.  This means their physical addresses change
when external memory size changes, but software uses symbolic names
(`HBW-BASE` KDOS constant) rather than hardcoded addresses.

**Alternative (simpler):** Pin HBW banks at fixed addresses
`0x0F00_0000 – 0x0FFF_FFFF` (top 16 MiB of a 256 MiB window) and
external memory fills `0x0010_0000 – 0x0EFF_FFFF`.  This gives a
permanent address for HBW regardless of external size, at the cost of
a hole in the address space when external memory is small.  **This is
probably better for software stability.**

### Guard Region (256 MiB)

Reserve addresses `0xF000_0000 – 0xFFFF_FEFF_FFFF_FFFF` as unmapped.
Any access faults.  Purpose:

- Prevent wild pointers from silently corrupting MMIO registers.
- Leave room for future memory-mapped peripherals.
- The gap between the top of addressable memory (≤256 MiB) and the
  MMIO base (`0xFFFF_FF00_0000_0000`) is effectively infinite — the
  guard region catches any access in the lower 4 GB above the physical
  memory ceiling.

### QoS Integration

- **Per-bank QoS:** Each high-BW bank can have its own bandwidth
  allocator.  E.g., core 0 gets priority on bank 1, core 1 on bank 2.
  This eliminates bus contention for tile-heavy workloads.
- **Bank affinity CSR:** `CSR_BANK_AFFINITY` per core — default bank
  for tile loads (so `TLOAD` automatically goes to the right bank
  without software address management).
- **Cross-bank tile ops:** If a tile op spans two banks, the hardware
  serializes the accesses (2 cycles instead of 1).  Software should
  align tiles to bank boundaries.

### `mp64_defs.vh` Changes

```verilog
`define INT_MEM_BYTES   (4 * 1024 * 1024)   // 4 MiB total
`define BANK_SIZE       (1024 * 1024)        // 1 MiB per bank
`define NUM_BANKS       4
`define REG_BW_BANKS    1                    // bank 0 only
`define HBW_BANKS       3                    // banks 1-3
`define HBW_BASE_ADDR   (`INT_MEM_BYTES - `HBW_BANKS * `BANK_SIZE)
`define GUARD_BASE      32'hF000_0000
`define EXT_MEM_MAX     (256 * 1024 * 1024)  // 256 MiB max
```

### RTL Modules

- **`mp64_memory_bank.v`** — single-bank module (configurable single-
  or dual-port via parameter).
- **`mp64_memory_subsys.v`** — instantiates 4 banks, address decoder,
  tile port arbiter across banks, CPU port mux, external memory
  forwarding.
- **`mp64_bus.v` updates** — address decoder checks bank assignment,
  routes to correct bank.  High-BW banks get direct tile port access.

### Emulator Changes

- `MegapadSystem.__init__()` takes `ram_kib=4096` by default.
- `mp64_memory.v` equivalent in Python: 4 separate `bytearray` banks.
- Address decode logic in `_read8`/`_write8` routes to correct bank.
- Tile engine load/store checks bank and uses appropriate path.
- `SysInfo` `MEM_SIZE` register reports total (bank0 + ext + HBW).

### BIOS/KDOS Changes

- `MEM-SIZE` now returns 4 MiB (or more with external).
- New words: `HBW-BASE` (returns address of first high-BW bank),
  `HBW-ALLOC` (allocate from high-BW region), `BANK@` (query which
  bank an address is in).
- Stack/dict/heap stay in bank 0.
- Buffer subsystem (`B.LOAD`) defaults to high-BW bank addresses.

### Resource Estimate

| Component | Current | New | Delta |
|-----------|---------|-----|-------|
| BRAM36 | ~240 | ~480 (+240) | 4× more internal RAM |
| LUTs (decoder) | ~200 | ~600 | Bank select logic |
| Tile arbiter | 1 port | 3-port mux | ~300 LUTs |

**Kintex-7 325T has 445 BRAM36.**  At 480 BRAM, we're at ~108% — so
4 MiB of *block RAM* may not fit.  Options:

1. Use BRAM for banks 0+1 (2 MiB, ~240 BRAM), banks 2+3 from external.
2. Use distributed RAM (LUTRAM) for bank 0 (wastes LUTs but saves BRAM).
3. Target larger FPGA (Kintex UltraScale/Virtex-7).
4. Make bank sizes configurable: 512 KiB default on 325T, 1 MiB on
   bigger FPGAs.
5. **Pragmatic:** 2 MiB BRAM (bank 0 + bank 1) + external for banks
   2–3.  This fits in 325T and matches the "regular BW + high BW"
   philosophy.

**Recommended for Kintex-7 325T:**
- Bank 0: 1 MiB BRAM (~120 BRAM36) — regular BW
- Bank 1: 1 MiB BRAM (~120 BRAM36) — high BW, tile port
- Banks 2–3: External memory (16+ MiB each) — regular BW with burst
- Total internal: 2 MiB BRAM = ~240 BRAM36 (54% of 445) ✅ fits
- The architecture *supports* 4× 1 MiB BRAM banks on larger FPGAs

---

## 4. Technology-Agnostic RTL (Roadmap Layer 6, Item 42)

### Goal

Refactor all FPGA-specific and ASIC-specific primitives behind clean
abstraction wrappers so the same core RTL builds on:

- Xilinx 7-series FPGAs (current target)
- Intel/Altera FPGAs (future)
- ASIC standard cell libraries (long-term)

### What Needs Wrapping

| Primitive | Current Implementation | Abstraction |
|-----------|----------------------|-------------|
| **Block RAM** | Inferred `(* ram_style = "block" *)` | `mp64_prim_ram.v` — parameterized by depth, width, ports |
| **PLL/MMCM** | None (uses raw `sys_clk`) | `mp64_prim_pll.v` — input freq → output freq |
| **Clock gating** | None | `mp64_prim_clkgate.v` — enable/disable clk domain |
| **IO buffers** | Direct port connection | `mp64_prim_iobuf.v` — tristate, pull-up, drive strength |
| **DSP multiply** | Verilog `*` (synthesis infers DSP48) | `mp64_prim_mul.v` — 64×64→128 with optional pipelining |
| **Reset sync** | Single `sys_rst_n` | `mp64_prim_rstsync.v` — async assert, sync deassert |
| **FIFO** | Hand-rolled in NIC/UART | `mp64_prim_fifo.v` — parameterized sync FIFO |
| **Dual-clock FIFO** | None (single clock domain) | `mp64_prim_async_fifo.v` — for future multi-clock |

### Directory Structure

```
fpga/
  rtl/
    core/           ← technology-independent core RTL
      mp64_cpu.v
      mp64_bus.v
      mp64_soc.v
      ...
    prim/            ← primitive wrappers (abstract interface)
      mp64_prim_ram.v        (parameterized SRAM)
      mp64_prim_mul.v        (multiplier)
      mp64_prim_pll.v        (clock generation)
      mp64_prim_clkgate.v    (clock gating cell)
      mp64_prim_iobuf.v      (IO pad)
      mp64_prim_rstsync.v    (reset synchronizer)
      mp64_prim_fifo.v       (sync FIFO)
    target/
      xilinx7/       ← Xilinx 7-series implementations
        mp64_prim_ram_xilinx7.v
        mp64_prim_mul_xilinx7.v
        mp64_prim_pll_xilinx7.v
      asic/           ← ASIC macro wrappers (future)
        mp64_prim_ram_asic.v
        mp64_prim_mul_asic.v
```

### Wrapper API Design

**RAM wrapper example:**
```verilog
module mp64_prim_ram #(
    parameter DEPTH     = 16384,
    parameter WIDTH     = 512,
    parameter NUM_PORTS = 2,    // 1=single, 2=true dual
    parameter INIT_FILE = ""
)(
    input  wire                     clk,
    // Port A
    input  wire                     a_en,
    input  wire                     a_wen,
    input  wire [$clog2(DEPTH)-1:0] a_addr,
    input  wire [WIDTH-1:0]         a_wdata,
    output reg  [WIDTH-1:0]         a_rdata,
    // Port B (optional, active when NUM_PORTS==2)
    input  wire                     b_en,
    input  wire                     b_wen,
    input  wire [$clog2(DEPTH)-1:0] b_addr,
    input  wire [WIDTH-1:0]         b_wdata,
    output reg  [WIDTH-1:0]         b_rdata
);
```

The Xilinx implementation uses `(* ram_style = "block" *)` and maps
to BRAM36.  The ASIC implementation instantiates a foundry SRAM macro
selected by the PDK integration script.

**Multiplier wrapper:**
```verilog
module mp64_prim_mul #(
    parameter A_WIDTH = 64,
    parameter B_WIDTH = 64,
    parameter PIPE_STAGES = 0   // 0=combinational, 1-3=pipelined
)(
    input  wire                         clk,
    input  wire                         valid,
    input  wire [A_WIDTH-1:0]           a,
    input  wire [B_WIDTH-1:0]           b,
    input  wire                         is_signed,
    output wire [A_WIDTH+B_WIDTH-1:0]   result,
    output wire                         result_valid
);
```

On FPGA: `PIPE_STAGES=0` maps to DSP48 inference.  On ASIC: might
want `PIPE_STAGES=2` with explicit Booth/Wallace tree.

### Implementation Phases

1. **Phase 1:** Create `prim/` directory, write wrapper modules with
   Xilinx-default implementations (functionally identical to current).
   Refactor `mp64_memory.v` to use `mp64_prim_ram`.  No functional
   change.
2. **Phase 2:** Refactor `mp64_cpu.v` multiply to use `mp64_prim_mul`.
   Refactor all inferred BRAMs across design.
3. **Phase 3:** Add `target/asic/` stubs (instantiate generic modules,
   leave ASIC macros as placeholders with `// TODO: foundry macro`).
4. **Phase 4:** Add clock gating and multi-clock domain support for
   power management.

### Impact on Existing Tests

All testbenches include `mp64_defs.vh`.  The primitive wrappers should
be simulation-compatible (no vendor IP instantiation under `` `ifdef
SIMULATION``).  Current Icarus Verilog flow should work unchanged.

---

## Implementation Priority

| Priority | Item | Status | Effort |
|----------|------|--------|--------|
| — | Micro-Core RTL (§1, steps 1–3) | ✅ Done (d169089, b15d918) | — |
| 1 | Memory Model (§3) | Not started | Large |
| 2 | Multi-Prime Field ALU (§2) | Not started | Medium |
| 3 | Tech-Agnostic RTL (§4) | Not started | Medium |
| 4 | Micro-Core SoC Integration (§1, steps 4–8) | Blocked on §3/§4 | Medium |

Suggested order: **§3 → §4 → §2 → §1.4–8** — memory model first
(biggest impact), then abstraction layer, then Field ALU (self-
contained win), then micro-core cluster integration (benefits from
all prior work: banked memory, clean primitives, parameterized bus).

---

## Random Notes

- The `mp64_extmem.v` module already handles external memory with burst
  support.  The memory redesign should keep this working and extend it
  to handle the "regular BW external fills gap" model.
- Consider a `BANK-COPY` DMA word that moves data between banks without
  CPU involvement (bank 0 → bank 1 for tile processing).
- The "pinned to end" model means KDOS `HBW-BASE` is a constant known
  at compile time (if internal-only) or queried from SysInfo (if
  external memory is present and shifts the HBW window).
- If we ever go multi-die / chiplet, the memory model naturally extends:
  each die has its own local HBW banks, external memory is shared.
- For the micro-core, consider whether KDOS should support
  **asymmetric multiprocessing** (AMP) where micro-cores run a stripped
  KDOS image, or just act as slaves that execute dispatched tasks only.
  Leaning toward *slave-only* — micro-cores don't run their own REPL or
  scheduler, they just wait for IPI, execute a word, and signal done.
