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

| Target | BRAM | Notes |
|--------|------|-------|
| **Xilinx Artix-7 200T** | 1,620 KB | Plenty for 1 MiB + FIFOs |
| **Lattice ECP5-85F** | 468 KB | Need external SRAM for full 1 MiB |
| **Xilinx Kintex-7 70T** | 2,700 KB | Comfortable margin |
| **Gowin GW5A** | 1,152 KB | Tight but feasible |

Primary target: **Artix-7 200T** (Digilent Nexys A7 or Arty A7-200T).

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
└── constraints/
    └── nexys_a7.xdc     ← Nexys A7 pin constraints
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

```
# Xilinx Vivado
vivado -mode batch -source build.tcl

# Open source (yosys + nextpnr for ECP5)
yosys -p "read_verilog rtl/*.v; synth_ecp5 -top mp64_soc" -o mp64.json
nextpnr-ecp5 --85k --json mp64.json --lpf constraints/ecp5.lpf --textcfg mp64.cfg
```
