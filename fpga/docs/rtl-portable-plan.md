# RTL Portable Refactor — Plan

Ground-up rebuild of the Megapad-64 RTL for technology-agnostic,
FPGA/ASIC-portable Verilog.  The existing 13.5 K-line codebase is used
for **ideas and architecture only** — every module is rewritten fresh with
clean interfaces, proper parameterisation, and a consistent coding
standard.

---

## Current State (audit summary)

| Metric | Value |
|--------|-------|
| RTL files | 26 (`.v`) + 2 (`.vh`) |
| Total lines | 13,557 |
| FPGA primitives | 3 instances, all in `mp64_synth_top.v` |
| Vendor attributes | 1 (`(* ram_style = "block" *)` in `mp64_memory.v`) |
| Clock domains | 1 (`clk` @ 100 MHz) |
| Reset | Async active-low everywhere |
| Un-synthesisable code | `%` operator in `mp64_ntt.v` |
| Testbenches | 20 files, 9,546 lines |
| Target | Kintex-7 325T (Genesys 2) |

### What's already clean
- Bus protocol (valid/ready handshake) — no vendor bus IP
- All peripherals use uniform `req/ack` interface
- All crypto engines are pure combinational + FSM
- Module boundaries are well-defined

### What needs fixing
1. `mp64_synth_top.v` uses Xilinx 7-series IBUFDS + MMCME2_BASE + BUFG
2. `mp64_memory.v` has Xilinx `(* ram_style *)` attribute
3. `mp64_ntt.v` uses `%` operator (not synthesisable)
4. `mp64_cpu.v` is a 1,863-line monolith with a ~1,400-line always block
5. `mp64_tile.v` is 2,291 lines with massive combinational fan-out
6. No parameterisation of memory sizes, cache geometry, core count
7. `initial` blocks for ROM init (fine for FPGA, not for ASIC)
8. `CLOCK_HZ` hardcoded in defines header

---

## Architecture Decisions

### 1. Platform Wrapper Strategy

```
mp64_platform_xxx.v          ← per-target (Xilinx 7, ECP5, ASIC, sim)
  ├── clock generation        ← PLL/MMCM/passthrough
  ├── reset synchronisation   ← 4-stage sync chain
  ├── I/O buffers             ← IBUF/OBUF/pad cells
  └── instantiates mp64_top.v

mp64_top.v                   ← fully portable
  └── mp64_soc.v             ← everything below is tech-agnostic
```

- `mp64_top.v` receives `clk`, `rst_n` — no platform knowledge
- Platform files live in `fpga/platform/{xilinx7,ecp5,gowin,asic,sim}/`
- Each platform provides a single top-level wrapper

### 2. Reset Strategy
- Default: **synchronous active-low** (`rst_n`) — preferred for ASIC
- Every flop: `if (!rst_n) ... else ...` inside `always @(posedge clk)`
- No `negedge rst_n` in sensitivity lists
- Platform wrapper handles async-to-sync conversion externally
- Clean, deterministic reset with no async flop issues

### 3. Memory Abstraction

Replace raw `reg` arrays with wrapper modules:

```verilog
mp64_sram_sp #(.ADDR_W(14), .DATA_W(512))   // single-port
mp64_sram_dp #(.ADDR_W(14), .DATA_W(512))   // true dual-port
mp64_rom     #(.ADDR_W(8),  .DATA_W(32))    // ROM (init file)
```

- Behavioural default (infers via tool)
- Per-platform overrides via `ifdef` or separate file lists
- No `(* ram_style *)` in core code — that goes in the wrapper
- ROM uses `$readmemh` (portable) instead of `initial` loops

### 4. Clock Gating

```verilog
mp64_clkgate u_cg (.clk_in(clk), .enable(en), .clk_out(gated_clk));
```

- Behavioural default: `assign clk_out = clk & enable;` (sim)
- ASIC: maps to ICG cell
- FPGA: maps to BUFGCE or just AND gate

### 5. Parameterisation (top-level propagation)

```verilog
parameter CLOCK_HZ       = 100_000_000,
parameter NUM_CORES       = 4,
parameter NUM_CLUSTERS    = 3,
parameter CORES_PER_CLUSTER = 4,
parameter MEM_DEPTH       = 16384,       // per-bank rows (× 512 bits)
parameter MEM_BANKS       = 4,
parameter ICACHE_LINES    = 256,
parameter ICACHE_LINE_W   = 128          // bits per cache line
```

- All constants flow from `mp64_top` → `mp64_soc` → each module
- Modules use `localparam` for derived values only
- No more hardcoded constants in module bodies

---

## Build Phases

### Phase 1 — Foundation (primitives + wrappers)

Create the portable abstraction layer and two simplest modules.

| Step | Deliverable | Tests |
|------|-------------|-------|
| 1.1 | `mp64_pkg.vh` — portable parameter package | — |
| 1.2 | `mp64_sram_sp.v` — single-port SRAM wrapper | unit TB |
| 1.3 | `mp64_sram_dp.v` — dual-port SRAM wrapper | unit TB |
| 1.4 | `mp64_rom.v` — ROM wrapper (`$readmemh`) | unit TB |
| 1.5 | `mp64_clkgate.v` — clock-gate wrapper | unit TB |
| 1.6 | `mp64_rst_sync.v` — reset synchroniser | unit TB |
| 1.7 | `mp64_top.v` — portable top (clk/rst in, SoC out) | — |
| 1.8 | `mp64_platform_sim.v` — simulation wrapper | iverilog |

**Commit**: "Phase 1: portable primitives & wrappers"

### Phase 2 — Bus & Memory Subsystem

Rebuild the interconnect and memory from scratch.

| Step | Deliverable | Tests |
|------|-------------|-------|
| 2.1 | `mp64_bus.v` — parameterised arbiter (N ports, QoS) | `tb_bus_arbiter` |
| 2.2 | `mp64_memory.v` — memory subsystem using SRAM wrappers | `tb_memory` |
| 2.3 | `mp64_extmem.v` — external memory controller | `tb_extmem` |
| 2.4 | Bus + memory integration test | SoC-level smoke |

**Commit**: "Phase 2: bus & memory with SRAM wrappers"

### Phase 3 — CPU Core

Rewrite the CPU with cleaner pipeline staging.

| Step | Deliverable | Tests |
|------|-------------|-------|
| 3.1 | `mp64_alu.v` — ALU (combinational, clean interfaces) | `tb_alu` |
| 3.2 | `mp64_regfile.v` — 16×64-bit register file | unit TB |
| 3.3 | `mp64_decode.v` — instruction decoder (extracted) | unit TB |
| 3.4 | `mp64_cpu.v` — CPU core (refactored into stages) | `tb_cpu_smoke` |
| 3.5 | `mp64_icache.v` — I-cache using SRAM wrappers | `tb_icache` |
| 3.6 | Full ISA opcode regression | `tb_opcodes` |

**Commit**: "Phase 3: CPU core with separated decode/ALU/regfile"

### Phase 4 — Peripherals

Clean rewrites with parameterised baud/clock.

| Step | Deliverable | Tests |
|------|-------------|-------|
| 4.1 | `mp64_uart.v` — UART (CLK_HZ param, sync RX) | `tb_peripherals` |
| 4.2 | `mp64_timer.v` — timer | existing TB |
| 4.3 | `mp64_disk.v` — storage controller | existing TB |
| 4.4 | `mp64_nic.v` — NIC (SRAM wrapper for buffers) | `tb_nic` |
| 4.5 | `mp64_mailbox.v` — mailbox + spinlocks | `tb_mailbox` |

**Commit**: "Phase 4: peripherals with clean parameterisation"

### Phase 5 — Tile Engine & Multi-core

The largest rewrite — the 512-bit SIMD tile engine.

| Step | Deliverable | Tests |
|------|-------------|-------|
| 5.1 | `mp64_fp16_alu.v` — FP16/BF16 ALU | `tb_tile` subset |
| 5.2 | `mp64_tile.v` — tile engine (pipelined, not monolithic) | `tb_tile` |
| 5.3 | `mp64_cpu_micro.v` — micro-core (cluster scalar core) | `tb_cpu_micro` |
| 5.4 | `mp64_cluster.v` — cluster (N micro-cores + scratchpad) | `tb_cluster` |
| 5.5 | Multi-core smoke test (4 majors + 3 clusters) | `tb_multicore_smoke` |

**Commit**: "Phase 5: tile engine & multi-core cluster"

### Phase 6 — Crypto & Math Accelerators

| Step | Deliverable | Tests |
|------|-------------|-------|
| 6.1 | `mp64_aes.v` — AES-256-GCM | `tb_crypto` |
| 6.2 | `mp64_sha3.v` — SHA-3 / Keccak | `tb_crypto` |
| 6.3 | `mp64_crc.v` — CRC-32/64 | `tb_crypto` |
| 6.4 | `mp64_trng.v` — TRNG (sim model + platform hook) | `tb_trng` |
| 6.5 | `mp64_field_alu.v` — GF(2²⁵⁵−19) with proper multiplier | `tb_field_alu` |
| 6.6 | `mp64_ntt.v` — NTT with Barrett reduction (no `%`) | `tb_ntt` |
| 6.7 | `mp64_kem.v` — ML-KEM-512 | `tb_kem` |

**Commit**: "Phase 6: crypto & math accelerators (synthesisable)"

### Phase 7 — SoC Integration & Platform Wrappers

| Step | Deliverable | Tests |
|------|-------------|-------|
| 7.1 | `mp64_soc.v` — SoC top-level integration | `tb_mp64_soc` |
| 7.2 | `mp64_platform_xilinx7.v` — Xilinx 7-series wrapper | — |
| 7.3 | `mp64_platform_ecp5.v` — Lattice ECP5 wrapper | — |
| 7.4 | Full regression: all 20 testbenches green | CI |
| 7.5 | QoS arbiter regression | `tb_qos` |

**Commit**: "Phase 7: SoC integration, all TBs passing"

---

## Optimisation Strategies

### Area

| Technique | Target | Saving |
|-----------|--------|--------|
| **DSP inference** | Tile multipliers → DSP48 | ~40% LUT reduction in tile |
| **SRAM inference** | I-cache, NIC buffers, KEM buf → BRAM | ~50K FF → ~12 BRAM36 |
| **Resource sharing** | Tile mutually-exclusive multiply modes | 1200 DSP → ~500 DSP |
| **ROM packing** | NTT twiddle factors → BRAM ROM | ~16K FF → 2 BRAM18 |
| **Micro-core sharing** | Cluster MUL/DIV shared across 4 μ-cores | -3 multipliers/cluster |

### Timing

| Technique | Target | Benefit |
|-----------|--------|---------|
| **Pipeline registers** | Field ALU 256×256 multiply (2+ stages) | Close timing at 100 MHz |
| **Tile output pipelining** | Tile engine combinational fan-out | Cut critical path ~40% |
| **Register retiming** | Tile accumulator adder tree | Better Fmax |
| **Memory output registers** | SRAM wrappers with output FF | Cleaner BRAM timing |

### Power

| Technique | Target | Benefit |
|-----------|--------|---------|
| **Clock gating** | Idle tiles, idle clusters, idle crypto | ~30% dynamic power |
| **Operand isolation** | Tile multiplier inputs when not in use | Reduce switching |
| **Memory banking** | Only activate addressed bank | BRAM power |

---

## Coding Standard (new RTL)

1. **Verilog-2001** (universally supported)
2. **Synchronous reset** — `if (!rst_n)` inside `always @(posedge clk)`
3. **Non-blocking assigns** (`<=`) in all clocked blocks, no exceptions
4. **Complete `case`/`default`** — no inferred latches
5. **Named port connections** — `.port(signal)`, no positional
6. **One module per file**, filename = module name
7. **Parameters** for all magic numbers; `localparam` for derived values
8. **`_q` suffix** for registered outputs, `_d` for next-state combinational
9. **Max ~300 lines** per `always` block — split into sub-modules
10. **No vendor primitives** outside `mp64_platform_*.v` files
11. **No `initial` blocks** in synthesisable code (use `$readmemh` for ROM)
12. **No `%` or `/` operators** — use explicit hardware (barrel shift, LUT, etc.)

---

## File Organisation (new)

```
fpga/
├── rtl/
│   ├── pkg/
│   │   └── mp64_pkg.vh              — parameters, opcodes, CSR map
│   ├── prim/
│   │   ├── mp64_sram_sp.v           — single-port SRAM wrapper
│   │   ├── mp64_sram_dp.v           — dual-port SRAM wrapper
│   │   ├── mp64_rom.v               — ROM wrapper ($readmemh)
│   │   ├── mp64_clkgate.v           — clock-gate cell
│   │   └── mp64_rst_sync.v          — reset synchroniser
│   ├── core/
│   │   ├── mp64_alu.v               — 64-bit ALU
│   │   ├── mp64_regfile.v           — 16×64-bit register file
│   │   ├── mp64_decode.v            — instruction decoder
│   │   ├── mp64_cpu.v               — CPU top (fetch/decode/exec/wb)
│   │   └── mp64_icache.v            — I-cache
│   ├── tile/
│   │   ├── mp64_fp16_alu.v          — FP16/BF16 arithmetic
│   │   ├── mp64_tile.v              — 512-bit SIMD tile engine
│   │   └── mp64_tile_acc.v          — accumulator + adder tree
│   ├── cluster/
│   │   ├── mp64_cpu_micro.v         — micro-core (scalar)
│   │   └── mp64_cluster.v           — cluster (N micro + scratchpad)
│   ├── bus/
│   │   ├── mp64_bus.v               — bus arbiter (QoS, N-port)
│   │   └── mp64_mmio_decode.v       — MMIO address decoder
│   ├── mem/
│   │   ├── mp64_memory.v            — memory subsystem
│   │   └── mp64_extmem.v            — external memory controller
│   ├── periph/
│   │   ├── mp64_uart.v              — UART
│   │   ├── mp64_timer.v             — timer + compare-match
│   │   ├── mp64_disk.v              — storage (SPI-SD)
│   │   ├── mp64_nic.v               — network interface
│   │   └── mp64_mailbox.v           — mailbox + spinlocks
│   ├── crypto/
│   │   ├── mp64_aes.v               — AES-256-GCM
│   │   ├── mp64_sha3.v              — SHA-3 / Keccak
│   │   ├── mp64_crc.v               — CRC-32/64
│   │   ├── mp64_trng.v              — TRNG (platform-hookable)
│   │   ├── mp64_field_alu.v         — GF(p) field coprocessor
│   │   ├── mp64_ntt.v               — NTT engine (Barrett mult)
│   │   └── mp64_kem.v               — ML-KEM-512
│   ├── mp64_soc.v                   — SoC integration
│   └── mp64_top.v                   — portable top-level
├── platform/
│   ├── sim/
│   │   └── mp64_platform_sim.v      — simulation (passthrough clk)
│   ├── xilinx7/
│   │   ├── mp64_platform_xilinx7.v  — MMCM + IBUFDS + BUFG
│   │   └── mp64_sram_xilinx7.v      — BRAM override (optional)
│   ├── ecp5/
│   │   └── mp64_platform_ecp5.v     — ECP5 PLL
│   └── asic/
│       ├── mp64_platform_asic.v     — PLL IP / passthrough
│       ├── mp64_sram_asic.v         — foundry SRAM macro
│       └── mp64_clkgate_asic.v      — ICG cell
├── sim/
│   └── (testbenches — rebuilt to match new module boundaries)
├── constraints/
│   ├── genesys2.xdc
│   └── nexys_a7.xdc
└── docs/
    ├── fpga.md
    └── rtl-portable-plan.md          — this file
```

---

## Progress Tracking

| Phase | Status |
|-------|--------|
| 1 — Foundation | not started |
| 2 — Bus & Memory | not started |
| 3 — CPU Core | not started |
| 4 — Peripherals | not started |
| 5 — Tile & Multi-core | not started |
| 6 — Crypto & Math | not started |
| 7 — SoC Integration | not started |
