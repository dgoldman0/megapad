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
| Multiplier | 64×64→128, 1 cycle, ~16 DSP | Shift-add 8 cycles, 0 DSP |
| Divider | Inferred `/` `%` operators | Same shift-subtract, but slower |
| I-Cache | 4 KiB per core | None (or shared 1 KiB cluster cache) |
| Tile engine | Full MEX + DMA + FP16 | ❌ Not present |
| BIST | March C−, checkerboard, addr | ❌ Not present |
| Perf counters | 4 × 64-bit | 1 × 64-bit (cycles only) |
| GPRs | 16 × 64-bit | 16 × 64-bit (same — keep ISA compat simple) |
| Pipeline | 2-stage IF/DEX with prefetch buffer | 1-stage fetch-execute (simpler) |

**Decision:** Keep 16 GPRs.  The ISA encodes 4-bit register fields
everywhere; halving it would require a decoder mode bit and creates
binary incompatibility.  The 16×64-bit register file is only 128 bytes
of LUTRAM — negligible.

### Integration & QoS

Micro-cores connect to the same bus arbiter as major cores.  QoS
implications:

- **Bus bandwidth:** Micro-cores without I-cache will generate more
  bus traffic (every instruction fetch goes through the arbiter).
  QoS weights should default lower for micro-cores.
- **Suggested topology:** Major cores get `weight=4`, micro-cores get
  `weight=1`.  BW limits can throttle micro-cores if they saturate
  the bus.
- **Core ID space:** Current `NUM_CORES=4` with 2-bit IDs.  With mixed
  major+micro cores, expand to e.g. 3-bit IDs (up to 8 cores total).
  The bus arbiter RR scan must become a parameterized loop instead of
  the current `& 2'd3` mask.
- **IPI/Mailbox:** Micro-cores need IPI support.  The mailbox already
  supports per-core addressing; just extend `NUM_CORES`.
- **Spinlocks:** Already parameterized by core count; works as-is.

### Suggested Configurations

| Config | Major | Micro | Est. LUTs | Est. DSPs | Use Case |
|--------|-------|-------|-----------|-----------|----------|
| Balanced | 2 | 4 | ~160K | ~300 | General-purpose with helper cores |
| Compute | 4 | 0 | ~185K | ~620 | Current design (unchanged) |
| Dense | 1 | 8 | ~130K | ~170 | Many-core control plane |
| Minimal | 1 | 0 | ~70K | ~170 | Single-core embedded |

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

1. **Factor `mp64_cpu_common.v`** — extract the shared decoder, ALU,
   flags, branch, interrupt, CSR, and memory FSM into a common include
   or module.
2. **Build `mp64_cpu_micro.v`** — instantiate the common core, add
   the shift-add multiplier, wire MEX port to a stub (bus fault on
   MEX instructions), remove I-cache port.
3. **Refactor `mp64_cpu.v`** — make it also use `mp64_cpu_common.v`
   plus its tile engine, I-cache, BIST, and hardware multiplier.
4. **Parameterize `mp64_soc.v`** — `NUM_MAJOR_CORES`, `NUM_MICRO_CORES`,
   generate blocks for each type.
5. **Update bus arbiter** — generalize `NUM_CORES` width, default QoS
   weights by core type.
6. **Emulator side:** `megapad64.py` gets a `micro=True` flag that
   disables tile ops and uses a Python shift-add multiply (for cycle-
   accurate behavior modeling).  `system.py` `MegapadSystem()` takes
   `num_major` and `num_micro` params.
7. **Tests:** `TestMicroCore` class — verify all base ISA ops work,
   verify MEX faults, verify multiply produces correct results (slower),
   verify QoS interaction with major cores.

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

| Priority | Item | Dependency | Effort |
|----------|------|------------|--------|
| 1 | Memory Model (§3) | None | Large — touches memory, bus, SoC, emulator, BIOS, KDOS |
| 2 | Micro-Core (§1) | Benefits from §3 (banking) and §4 (primitives) | Large |
| 3 | Multi-Prime Field ALU (§2) | Independent | Medium |
| 4 | Tech-Agnostic RTL (§4) | Independent, but best done before §2 refactor | Medium |

Suggested order: **§4 → §3 → §2 → §1** — get the abstraction layer
right first, then redesign memory on top of clean primitives, then
the Field ALU (which touches fewer files), and finally the micro-core
(which is the most complex integration task and benefits from all the
prior work).

Alternatively: **§2 → §4 → §3 → §1** if we want an early visible
win (multi-prime ALU is self-contained and useful immediately).

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
