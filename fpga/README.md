# Megapad-64 FPGA Prototype

Synthesizable RTL for the Megapad-64 system-on-chip.

## Architecture

```
                    ┌─────────────────────────────────────────────────────┐
                    │                  Megapad-64 SoC                     │
                    │                                                     │
                    │  ┌──────────┐   ┌────────────┐   ┌──────────────┐  │
                    │  │ MP64 CPU │◄─►│  Bus Arbiter│◄─►│  Tile Engine │  │
                    │  │  64-bit  │   │  (priority) │   │  (MEX unit)  │  │
                    │  └──────────┘   └──────┬─────┘   └──────────────┘  │
                    │                        │                            │
                    │           ┌─────────────┼────────────┐              │
                    │           │             │            │              │
                    │    ┌──────▼──────┐ ┌────▼─────┐ ┌───▼──────────┐  │
                    │    │  Internal   │ │  MMIO    │ │  External    │  │
                    │    │  Memory     │ │  Bridge  │ │  Memory Ctrl │  │
                    │    │  1 MiB BRAM │ │          │ │  (HyperRAM/  │  │
                    │    │  dual-port  │ │          │ │   SDRAM)     │  │
                    │    │  512b wide  │ │          │ │              │  │
                    │    └─────────────┘ └────┬─────┘ └───┬──────────┘  │
                    │                         │           │              │
                    │           ┌──────┬──────┼──────┐    │   Ext pins  │
                    │           │      │      │      │    │     ║       │
                    │         ┌─▼─┐ ┌──▼─┐ ┌─▼──┐ ┌─▼─┐  │     ║       │
                    │         │UART│ │Timr│ │Disk│ │NIC│  │     ║       │
                    │         └─┬──┘ └────┘ └─┬──┘ └─┬─┘  │     ║       │
                    └───────────┼──────────────┼─────┼────┘     ║       │
                                │              │     │          ║
                           TX/RX pins     SD/SPI   MII/RMII  HyperRAM
```

## Target FPGAs

| Target | LUTs | BRAM | DSPs | Notes |
|--------|------|------|------|-------|
| **Xilinx Kintex-7 325T** | 203,800 | 4,005 KB | 840 | **Primary — Genesys 2** |
| **Xilinx Artix-7 200T** | 134,600 | 1,620 KB | 740 | Tight fit (legacy target) |
| **Xilinx Kintex-7 410T** | 254,200 | 5,663 KB | 1,540 | Comfortable headroom |
| **Lattice ECP5-85F** | 84,000 | 468 KB | — | Needs external SRAM |

Primary target: **Kintex-7 325T** (Digilent Genesys 2).

## Directory Structure

```
fpga/
├── README.md            ← this file
├── synth_genesys2.tcl   ← Vivado synthesis script
├── synth_yosys.tcl      ← Yosys synthesis script
├── constraints/
│   ├── genesys2.xdc     ← Genesys 2 (Kintex-7 325T) pin constraints
│   └── nexys_a7.xdc     ← Nexys A7 pin constraints
├── docs/
│   └── fpga.md          ← FPGA-specific documentation
└── build/               ← synthesis output (gitignored)

RTL source and testbenches are in the portable `rtl/` tree at the
project root.  See `rtl/README.md` or the project-level README for
the full module listing.
```

## Memory Architecture

The internal 1 MiB BRAM is organized as **dual-port with a 512-bit wide
tile port** (Port A) and a **64-bit CPU port** (Port B).  The tile engine
gets priority on Port A and can load/store entire 64-byte tiles in a
single cycle.  The CPU uses Port B for normal loads/stores with no
contention.

External memory (HyperRAM or SDRAM) connects through a cache/bridge that
translates 64-bit CPU accesses into burst transactions.  The tile engine
can also target external memory, but at reduced throughput (~8× slower
than internal BRAM).

## Resource Estimate (Manual Audit)

Automated synthesis requires Vivado (Yosys 0.33 cannot handle the 512-bit
SIMD tile engine or large BRAM arrays).  The following estimate was produced
by auditing every RTL module for register counts, multiplier widths, memory
arrays, and combinational logic.

### Kintex-7 325T (xc7k325tffg900-2) — Genesys 2

| Resource | Used (est.) | Available | Utilisation |
|----------|------------:|----------:|:-----------:|
| **LUTs** | ~145K–185K | 203,800 | 70–90% |
| **FFs** | ~220K | 326,080 | 67% |
| **BRAM36** | ~240 | 445 | 54% |
| **DSP48E1** | ~400–620 | 840 | 48–74% |

### Per-Module Breakdown

| Module | ×N | FFs | BRAM36 | DSP48 | Notes |
|--------|---:|----:|-------:|------:|-------|
| **mp64_cpu** | 4 | 18.4K | — | — | 16×64b GPRs, flags, pipeline |
| **mp64_icache** | 4 | 53.2K | — | — | 4 KiB data + tags (registers) |
| **mp64_tile** | 4 | 110.4K | — | 400–600 | 512b SIMD, accumulators, dot-product |
| **mp64_memory** | 1 | — | 228 | — | 16384×512b dual-port (1 MiB) |
| **mp64_aes** | 1 | 4.2K | — | — | AES-256-GCM, 15 round keys |
| **mp64_sha3** | 1 | 3.3K | — | — | Keccak-f[1600], 5×5×64 state |
| **mp64_crc** | 1 | ~0.2K | — | — | CRC-32 |
| **mp64_nic** | 1 | 13.6K | 6 | — | 1500B RX/TX buffers, data window |
| **mp64_bus** | 1 | ~0.5K | — | — | QoS arbiter, weights/counters |
| **mp64_uart** | 1 | ~0.3K | — | — | 16B TX/RX FIFOs |
| **mp64_timer** | 1 | ~0.3K | — | — | 32-bit timer + compare |
| **mp64_disk** | 1 | ~0.2K | 3 | — | SPI-SD + DMA buffer |
| **mp64_mailbox** | 1 | ~1.0K | — | — | 4 slots + 8 spinlocks |
| **mp64_extmem** | 1 | ~0.2K | — | — | External memory controller |
| **mp64_fp16_alu** | 4 | ~3.2K | — | — | FP16/FP32 convert + add |
| **mp64_trng** | 1 | ~0.6K | — | — | Ring-osc entropy + LFSR conditioner |
| **mp64_field_alu** | 1 | ~4.5K | — | 4–8 | GF(p) coprocessor, reuses shared multiplier |
| **mp64_ntt** | 1 | ~5.0K | 3 | 8–12 | 256-pt butterfly, twiddle ROM, 3×256×32b reg files |
| **mp64_kem** | 1 | ~0.8K | 3 | — | 3,296B buffer array, FSM, byte-stream port |

### DSP Usage Notes

The raw multiplier count across 4 tile engines is ~1,200–1,700 DSP48E1
(integer TMUL at 4 element widths + FP16 multipliers).  However, only
one `mode_ew` is active per tile at any time, so Vivado's resource-sharing
optimisation (`AreaOptimized_high` + retiming) should fold mutually
exclusive multipliers down to **~400–600 DSP48E1**.  The NTT engine adds
~8–12 DSPs for its modular butterfly multiplier, and the Field ALU adds
~4–8 DSPs for its 256-bit Montgomery multiplier core.  Total DSP budget:
**~420–620 DSP48E1** — well within the 840-slice budget.

The NTT engine's 3×256×32-bit register files (~24 Kbit each) could
optionally be inferred as BRAM36 slices (~3 blocks) to reduce FF pressure,
or kept as distributed registers for lower latency.  The KEM module's
3,296-byte buffer array similarly benefits from BRAM inference (~3 blocks).

### Synthesis Status

- **Yosys 0.33**: Cannot complete — Yosys flattens the tile engine's
  combinational `always @(*)` block into thousands of latches and lacks
  proper BRAM inference for the memory architecture.
- **Vivado**: Required for correct synthesis.  Build scripts and
  constraints are ready (`synth_genesys2.tcl`, `genesys2.xdc`).
- **Conclusion**: Design is expected to fit the K325T comfortably with
  Vivado synthesis.  DSP utilisation is the tightest resource but should
  be feasible with resource sharing.

## Building

```bash
# Vivado synthesis (Genesys 2 / Kintex-7 325T)
cd fpga
vivado -mode batch -source synth_genesys2.tcl

# Reports written to fpga/build/
#   utilisation.rpt        — LUT/FF/BRAM/DSP usage
#   utilisation_hier.rpt   — per-module breakdown
#   timing_summary.rpt     — WNS/TNS
```
