# Megapad-64 FPGA Design Document

## 1  Overview

This document describes the FPGA implementation of the Megapad-64
system-on-chip.  The design targets the **Digilent Nexys A7-200T**
(Xilinx Artix-7 `xc7a200tsbg484-1`) as the primary prototype board.

### 1.1  Design Goals

| Goal | Target |
|------|--------|
| **Internal RAM** | 1 MiB (block RAM, single-cycle for CPU) |
| **Tile fast path** | 512-bit single-cycle read/write to BRAM |
| **External memory** | HyperRAM / SDRAM via PMOD, ≥ 4 MiB |
| **Clock** | 100 MHz system clock |
| **UART** | 115 200 baud, USB bridge on-board |
| **Storage** | SPI-SD on-board micro-SD slot |
| **NIC** | Ethernet PMOD (optional) |
| **CPU** | Full Megapad-64 ISA, single-issue multi-cycle |

---

## 2  Block Diagram

```
                        ┌──────────────────────────────────────────────────┐
                        │                 mp64_soc                         │
                        │                                                  │
  UART_RXD ────────────►│  ┌──────────┐        ┌───────────┐              │
  UART_TXD ◄────────────│  │ mp64_cpu │◄──────►│ mp64_bus  │              │
                        │  │          │  bus    │ arbiter & │              │
                        │  │ 16×64b   │        │ decoder   │              │
                        │  │ regs     │        └─────┬─────┘              │
                        │  │ flags    │              │                     │
                        │  │ CSR ──────────┐    ┌────┴────┐               │
                        │  └──────────┘    │    │         │               │
                        │                  ▼    ▼         ▼               │
                        │            ┌──────────┐   ┌──────────┐          │
                        │            │mp64_tile │   │ MMIO mux │          │
                        │            │ engine   │   │          │          │
                        │            │ 64×8 ALU │   │┌────────┐│          │
                        │            └────┬─────┘   ││ UART   ││──► TXD  │
                        │     512b   ┌────┘         ││ Timer  ││         │
                        │     Port A │    Port B    ││ Disk   ││──► SPI  │
                        │            ▼    (64b)     ││ NIC    ││──► PHY  │
                        │      ┌──────────────┐     │└────────┘│          │
                        │      │  mp64_memory │     └──────────┘          │
                        │      │  1 MiB BRAM  │                           │
                        │      │  dual-port   │                           │
                        │      └──────┬───────┘                           │
                        │             │ ext_fwd                           │
                        │             ▼                                   │
                        │      ┌──────────────┐     ┌──────────┐          │
                        │      │ mp64_extmem  │◄───►│ PHY pins │          │
                        │      │ controller   │     └──────────┘          │
                        │      └──────────────┘                           │
                        └──────────────────────────────────────────────────┘
```

---

## 3  Module Inventory

| Module | File | Lines | Description |
|--------|------|------:|-------------|
| `mp64_defs` | `rtl/mp64_defs.vh` | ~200 | Shared constants, ISA encoding, CSR map |
| `mp64_cpu` | `rtl/mp64_cpu.v` | ~450 | CPU core: fetch/decode/execute, 16 GPRs, flags |
| `mp64_bus` | `rtl/mp64_bus.v` | ~60 | Bus arbiter & MMIO/memory address decoder |
| `mp64_memory` | `rtl/mp64_memory.v` | ~150 | Dual-port 1 MiB BRAM (512-bit tile + 64-bit CPU) |
| `mp64_tile` | `rtl/mp64_tile.v` | ~300 | Tile engine (MEX), 64×8-bit SIMD lanes |
| `mp64_extmem` | `rtl/mp64_extmem.v` | ~150 | External memory controller (HyperRAM/SDRAM) |
| `mp64_uart` | `rtl/mp64_uart.v` | ~200 | UART 8N1, 16-byte FIFOs |
| `mp64_timer` | `rtl/mp64_timer.v` | ~120 | 32-bit timer, compare-match, auto-reload |
| `mp64_disk` | `rtl/mp64_disk.v` | ~250 | SPI-SD controller with DMA |
| `mp64_nic` | `rtl/mp64_nic.v` | ~220 | NIC with 1500-byte RX buffer, DMA |
| `mp64_soc` | `rtl/mp64_soc.v` | ~280 | Top-level SoC wiring |
| `tb_mp64_soc` | `sim/tb_mp64_soc.v` | ~220 | Testbench with PHY stub, UART monitor |

---

## 4  Memory Architecture

### 4.1  Internal BRAM (1 MiB)

Address range: `0x0000_0000` – `0x000F_FFFF`

Organisation: 16 384 rows × 512 bits (= 1 048 576 bytes)

**Port A** — Tile engine (512-bit wide):
- Single-cycle read/write of a full 64-byte tile
- Address in tile-row units (row = addr >> 6)
- Priority over Port B on collision

**Port B** — CPU (64-bit wide):
- Standard load/store (byte, half, word, dword)
- 131 072 × 64-bit view of the same physical RAM
- Stalls 1 cycle on collision with Port A

### 4.2  External Memory

Address range: `0x0010_0000` and above.

Two access modes:
1. **CPU single-beat**: 6–10 cycles latency per access
2. **Tile burst**: 8 beats × 64 bits = 512 bits in 14–18 cycles

Tile requests pre-empt CPU external accesses.

---

## 5  Tile Engine Detail

The tile engine implements the MEX instruction family with 64 parallel
8-bit ALU lanes (also configurable for 32×16, 16×32, 8×64 — 8-bit
is default).

### 5.1  Pipeline

| Cycle | Operation |
|------:|-----------|
| 1 | Load source A from BRAM (512-bit) |
| 2 | Load source B (BRAM, GPR broadcast, imm8 splat, or in-place) |
| 3 | Compute + Store result |

For **external** tile addresses, each load/store phase takes ~8–10
additional cycles for the burst transfer.

### 5.2  Operations

| Family | Functions |
|--------|-----------|
| **TALU** | ADD, SUB, AND, OR, XOR, MIN, MAX, ABS |
| **TMUL** | MUL, DOT |
| **TRED** | SUM, MIN, MAX, POPCNT, L1 |
| **TSYS** | TRANS, MOVBANK, LOADC, ZERO |

### 5.3  Reduction

Reduction operations (TRED) sum / min / max across all 64 lanes and
store the scalar result in the 256-bit accumulator (ACC0–ACC3 CSRs).

---

## 6  CPU Core

### 6.1  Architecture

- 16 × 64-bit general-purpose registers (R0–R15)
- Selectable PC (PSEL, default R3), data pointer (XSEL, R2), SP (SPSEL, R15)
- 8-bit flags: Z, C, N, V, P, G, I (interrupt enable), S (saturation)
- Vectored interrupts: 8 vectors, IVT base in CSR

### 6.2  Instruction Encoding

Variable-length, 1–11 bytes:

| Family (hi nibble) | Typical length | Example |
|---------------------|---------------:|---------|
| `0x0_` SYS | 1–2 | `NOP`, `CALL.L R6` |
| `0x1_` INC | 1 | `INC R5` |
| `0x2_` DEC | 1 | `DEC R5` |
| `0x3_` BR | 2 | `BR.EQ +12` |
| `0x4_` LBR | 3 | `LBR.AL -4096` |
| `0x5_` MEM | 3 | `LD R1, [R2+8]` |
| `0x6_` IMM | 3–10 | `LDI R1, 42` / EXT + 64-bit |
| `0x7_` ALU | 2 | `ADD R1, R2` |
| `0x8_` MEMALU | 1 | Legacy 1802 ops |
| `0x9_` IO | 1 | `OUT 4` / `INP 5` |
| `0xA_` SEP | 1 | `SEP R3` |
| `0xB_` SEX | 1 | `SEX R2` |
| `0xC_` MULDIV | 2 | `MUL R3, R4` |
| `0xD_` CSR | 3 | `CSRW TSRC0, R5` |
| `0xE_` MEX | 2–3 | `TALU.ADD.8` |
| `0xF_` EXT | 1 | Prefix modifier |

### 6.3  Execution Model

Single-issue, multi-cycle (no pipeline).  Simple instructions (INC,
SEP, ALU) complete in 3–4 cycles (fetch + decode + execute).  Memory
operations add bus latency.  MEX operations stall CPU until tile
engine completes.

A pipelined version (3-stage: IF → DE → EX) would be a Phase-2
optimisation once functional correctness is established.

---

## 7  Resource Estimates (Artix-7 200T)

| Resource | Used | Available | % |
|----------|-----:|----------:|---:|
| Block RAM (36 Kb) | 286 | 365 | 78% |
| Slice LUTs | ~8 000 | 134 600 | 6% |
| Slice Registers | ~4 000 | 269 200 | 1.5% |
| DSP48E1 | 2 | 740 | 0.3% |
| BUFG | 1 | 32 | 3% |

BRAM dominates — 1 MiB = 8 Mb = 228 × 36 Kb blocks (228 of 365,
≈ 62%).  The remaining 137 blocks provide tile FIFO, UART FIFOs,
NIC RX buffer, etc.

Logic utilisation is minimal at ~6% LUTs.  There is ample headroom
for cache, DMA engines, or a second CPU core.

---

## 8  Timing Budget

| Path | Target | Margin |
|------|--------|--------|
| CPU ALU (64-bit add) | 10 ns | ~4 ns |
| BRAM access (Port B, 64-bit) | 10 ns | ~3 ns |
| Tile 512-bit read | 10 ns | ~2 ns |
| MMIO register read | 10 ns | ~6 ns |
| External PHY I/O | 10 ns | ~1 ns (critical) |

The external memory PHY interface is the tightest path.  If timing
closure is difficult, the PHY can operate at sys_clk/2 (50 MHz).

---

## 9  Build & Simulation

### 9.1  Simulation (Icarus Verilog)

```bash
cd fpga/sim
iverilog -I ../rtl -o mp64_sim \
    ../rtl/mp64_cpu.v ../rtl/mp64_bus.v ../rtl/mp64_memory.v \
    ../rtl/mp64_tile.v ../rtl/mp64_extmem.v ../rtl/mp64_uart.v \
    ../rtl/mp64_timer.v ../rtl/mp64_disk.v ../rtl/mp64_nic.v \
    ../rtl/mp64_soc.v tb_mp64_soc.v
vvp mp64_sim
gtkwave mp64_soc.vcd
```

### 9.2  Synthesis (Vivado)

```tcl
create_project mp64 -part xc7a200tsbg484-1
add_files [glob rtl/*.v rtl/*.vh]
add_files -fileset constrs_1 constraints/nexys_a7.xdc
set_property top mp64_soc [current_fileset]
launch_runs synth_1 -jobs 8
launch_runs impl_1 -to_step write_bitstream
```

### 9.3  Programming

```bash
vivado -mode batch -source program.tcl
# or via hardware manager GUI
```

---

## 10  Future Work

1. **Pipelined CPU** — 3-stage (IF/DE/EX) for ~2× throughput.
2. **Instruction cache** — 4 KiB direct-mapped for fetch bandwidth.
3. **Tile double-buffering** — overlap load + compute for throughput.
4. **HDMI/VGA output** — framebuffer in external memory, scan-out engine.
5. **PS/2 keyboard** — direct input for standalone operation.
6. **Boot ROM** — small ROM with SPI bootstrap to load BIOS from SD.
7. **Multi-core** — dual CPU + shared tile engine (enough LUT headroom).
