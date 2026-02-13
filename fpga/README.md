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
├── README.md           ← this file
├── rtl/
│   ├── mp64_soc.v      ← top-level SoC
│   ├── mp64_cpu.v       ← CPU core (fetch/decode/execute)
│   ├── mp64_tile.v      ← tile engine (MEX unit)
│   ├── mp64_memory.v    ← memory subsystem (BRAM + arbiter)
│   ├── mp64_extmem.v    ← external memory controller
│   ├── mp64_bus.v       ← bus arbiter + address decoder
│   ├── mp64_uart.v      ← UART peripheral
│   ├── mp64_timer.v     ← timer peripheral
│   ├── mp64_disk.v      ← storage controller (SPI-SD)
│   ├── mp64_nic.v       ← network interface
│   └── mp64_defs.vh     ← shared constants & parameters
├── sim/
│   └── tb_mp64_soc.v    ← testbench
├── constraints/
│   ├── genesys2.xdc     ← Genesys 2 (Kintex-7 325T) pin constraints
│   └── nexys_a7.xdc     ← Nexys A7 pin constraints (legacy)
└── synth_genesys2.tcl   ← Vivado synthesis script
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
