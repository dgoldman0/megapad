# Megapad-64 System Architecture

This document describes the Megapad-64 computer system as a whole — how
the CPU, memory, and peripheral devices fit together, and how the software
layers (BIOS, KDOS, filesystem) build on top of the hardware.

---

## System Block Diagram

```
┌───────────────────────────────────────────────────────────┐
│                    Megapad-64 CPU (×4 cores)               │
│  16 × 64-bit GPRs    ┌──────────────────┐  ┌───────────┐ │
│  4 KiB I-Cache        │   Tile Engine    │  │ Perf Ctrs │ │
│  8-bit Flags          │  (MEX extension) │  │ (4 × 64b) │ │
│  256-bit Accumulator  │  FP16 / bf16     │  └───────────┘ │
│  Perf counters        │  DMA queue       │                │
│                       └──────────────────┘                │
└───────────────┬───────────────────────────────────────────┘
                │  64-bit data bus (weighted round-robin QoS)
    ┌───────────┴───────────────────────────┐
    │            Memory Map                  │
    │                                        │
    │  0x0000_0000 ┌──────────────────────┐ │
    │              │       RAM             │ │
    │              │   (1 MiB BRAM + BIST) │ │
    │              │                       │ │
    │              │  BIOS code + dict     │ │
    │              │  KDOS Forth dict      │ │
    │              │  Buffers & data       │ │
    │              │  FS cache (3 KB)      │ │
    │              │  Task stacks (2 KB)   │ │
    │              │         ↓ HERE        │ │
    │              │         ...           │ │
    │              │         ↑ SP          │ │
    │              │  Data stack (grows ↓) │ │
    │              │  Return stack         │ │
    │  0x000F_FFFF └──────────────────────┘ │
    │              ...                       │
    │  FFFF_FF00+  ┌──────────────────────┐ │
    │   0x0000     │  UART                │ │
    │   0x0100     │  Timer               │ │
    │   0x0200     │  Storage Controller  │ │
    │   0x0300     │  System Info (R/O)   │ │
    │   0x0400     │  NIC                 │ │
    │   0x0500     │  Mailbox (IPI)       │ │
    │   0x0600     │  Spinlock            │ │
    │   0x0700     │  AES-256-GCM         │ │
    │   0x0780     │  SHA-3 / SHAKE       │ │
    │   0x07C0     │  CRC32 / CRC64       │ │
    │   0x07E0     │  QoS Config          │ │
    │              └──────────────────────┘ │
    └───────────────────────────────────────┘
```

---

## Memory Map

The CPU uses a 64-bit flat address space.  RAM starts at address 0;
MMIO devices live at the top of the address space.

### RAM Region

| Address | Content |
|---------|---------|
| `0x0000_0000` | BIOS code (loaded at boot, ~20 KB) |
| `0x0000_4F00`+ | Forth dictionary grows upward from HERE |
| *(varies)* | KDOS code, buffer data, FS caches, task stacks |
| *(varies)* | Free space between HERE and SP |
| ← SP | Data stack grows downward from top of RAM |
| `RAM_SIZE` | Top of RAM (default 0x0010_0000 = 1 MiB) |

The BIOS sets `HERE` just past its own code.  As KDOS loads (via FSLOAD),
it compiles words and allocates data, advancing HERE.  The data stack lives
at the top of RAM and grows downward.  The return stack sits below the data
stack.

### MMIO Region

All MMIO registers live at base address `0xFFFF_FF00_0000_0000`.  Each
device occupies a small range:

| Device | Offset | Size | Description |
|--------|--------|------|-------------|
| **UART** | `+0x0000` | 16 bytes | Serial I/O (keyboard/terminal) |
| **Timer** | `+0x0100` | 16 bytes | 32-bit timer with compare-match |
| **Storage** | `+0x0200` | 16 bytes | Sector-based disk controller |
| **System Info** | `+0x0300` | 16 bytes | Read-only board ID and config |
| **NIC** | `+0x0400` | 128 bytes | Network interface controller |
| **Mailbox** | `+0x0500` | 16 bytes | Inter-core IPI (data + send + status + ack) |
| **Spinlock** | `+0x0600` | 64 bytes | Hardware spinlocks (16 locks, 4 bytes each) |
| **AES-256-GCM** | `+0x0700` | 64 bytes | Authenticated encryption accelerator |
| **SHA-3/SHAKE** | `+0x0780` | 64 bytes | Keccak hash / XOF accelerator |
| **CRC32/CRC64** | `+0x07C0` | 32 bytes | Fast CRC computation (8 bytes/cycle) |
| **QoS Config** | `+0x07E0` | 16 bytes | Global bus QoS quantum / weights |

Any access outside RAM and the MMIO aperture triggers a **bus fault**
(vector `IVEC_BUS_FAULT`).

---

## UART (Serial Port)

The UART provides terminal I/O — it's how the user types at the Forth REPL
and sees output.  It has a transmit buffer and a receive FIFO.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| TX_DATA | `+0x00` | W | Write a byte to transmit. |
| RX_DATA | `+0x01` | R | Read next byte from receive FIFO. |
| STATUS | `+0x02` | R | **bit 0:** TX ready (always 1).  **bit 1:** RX data available.  **bit 5:** TX empty. |
| CONTROL | `+0x03` | RW | **bit 0:** RX IRQ enable.  **bit 1:** TX IRQ enable. |
| BAUD_LO | `+0x04` | RW | Baud rate low byte (cosmetic — the emulated UART is always instant). |
| BAUD_HI | `+0x05` | RW | Baud rate high byte. |

**BIOS words:** `KEY` reads from RX_DATA (blocking), `KEY?` checks
STATUS bit 1, `EMIT` writes to TX_DATA.

---

## Timer

A 32-bit free-running counter with compare-match interrupt capability.
The counter increments once per CPU cycle.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| COUNT | `+0x00`–`+0x03` | R | 32-bit counter value (little-endian, 4 bytes). |
| COMPARE | `+0x04`–`+0x07` | RW | 32-bit compare-match value (LE). |
| CONTROL | `+0x08` | RW | **bit 0:** Timer enable.  **bit 1:** Compare-match IRQ enable.  **bit 2:** Auto-reload (reset counter on match). |
| STATUS | `+0x09` | RW | **bit 0:** Compare-match flag.  Write 1 to clear. |

When the counter reaches the compare value and CONTROL bits 0+1 are set
and interrupts are enabled (IE=1), the timer fires `IVEC_TIMER`.  With
auto-reload (bit 2), the counter resets to 0 on match, creating a periodic
interrupt.

KDOS uses this for **preemptive scheduling** — `PREEMPT-ON` configures a
50,000-cycle timer with auto-reload, and `YIELD?` checks the preemption
flag set by the timer handler.

---

## Storage Controller

A sector-based disk controller supporting DMA transfers.  Sector size is
**512 bytes**.  The disk image can hold up to 2048 sectors (1 MiB).

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **0x01:** READ, **0x02:** WRITE, **0x03:** STATUS, **0xFF:** FLUSH |
| STATUS | `+0x01` | R | **bit 0:** busy, **bit 1:** error, **bit 7:** device present |
| SECTOR | `+0x02`–`+0x05` | RW | 32-bit sector number (LE) |
| DMA_ADDR | `+0x06`–`+0x0D` | RW | 64-bit RAM address for DMA (LE) |
| SEC_COUNT | `+0x0E` | RW | Number of sectors to transfer (1–255) |
| DATA | `+0x0F` | RW | Byte-at-a-time data port (alternative to DMA) |

**Typical read sequence:**
1. Write sector number to SECTOR registers
2. Write RAM destination to DMA_ADDR registers
3. Write sector count to SEC_COUNT
4. Write `0x01` to CMD (READ)
5. Data appears in RAM at DMA_ADDR

**BIOS words:** `DISK-SEC!`, `DISK-DMA!`, `DISK-N!`, `DISK-READ`,
`DISK-WRITE`, `DISK@` (read status).

---

## System Info (Read-Only)

Static board identification registers.

| Register | Offset | Value | Description |
|----------|--------|-------|-------------|
| BOARD_ID | `+0x00`–`+0x03` | `"MP64"` | Board identifier string |
| VERSION_MAJ | `+0x04` | 1 | Major hardware version |
| VERSION_MIN | `+0x05` | 0 | Minor hardware version |
| MEM_SIZE | `+0x06`–`+0x07` | varies | Total RAM in KiB (16-bit LE) |
| STORAGE_PRESENT | `+0x08` | 0 or 1 | Is a disk attached? |
| UART_PRESENT | `+0x09` | 1 | Always present |
| NIC_PRESENT | `+0x0A` | 0 or 1 | Is a NIC attached? |

---

## NIC (Network Interface Controller)

An Ethernet-style network controller with a 1500-byte MTU.  Supports both
DMA and byte-at-a-time data transfer.  Default MAC address:
`02:4D:50:36:34:00`.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **0x01:** SEND, **0x02:** RECV, **0x03:** STATUS, **0x04:** RESET |
| STATUS | `+0x01` | R | **bit 0:** TX busy, **bit 1:** RX available, **bit 2:** link up, **bit 3:** error, **bit 7:** present |
| DMA_ADDR | `+0x02`–`+0x09` | RW | 64-bit DMA address (LE) |
| FRAME_LEN | `+0x0A`–`+0x0B` | RW | 16-bit frame length (LE) |
| IRQ_CTRL | `+0x0C` | RW | **bit 0:** RX IRQ enable, **bit 1:** TX IRQ enable |
| IRQ_STATUS | `+0x0D` | RW | **bit 0:** RX IRQ pending, **bit 1:** TX IRQ pending (W1C) |
| MAC_ADDR | `+0x0E`–`+0x13` | R | 6-byte MAC address |
| TX_COUNT | `+0x14`–`+0x15` | R | Frames sent (16-bit LE) |
| RX_COUNT | `+0x16`–`+0x17` | R | Frames received (16-bit LE) |
| DATA | `+0x20`–`+0x7F` | RW | 96-byte data window for byte-at-a-time I/O |

**BIOS words:** `NET-STATUS`, `NET-SEND`, `NET-RECV`, `NET-MAC@`.

**KDOS data ports** (§10) provide a higher-level frame routing layer on
top of the NIC — incoming frames are parsed and routed to bound buffers
based on source ID.

---

## Hardware Accelerators

The Megapad-64 includes several hardware accelerator blocks beyond the
base tile engine. These are part of the base design, not optional
extensions. See `docs/extended-tpu-spec.md` for full register maps,
encoding details, and implementation phases.

### Enhanced Tile Engine

The tile engine extends beyond the base TALU/TMUL/TRED/TSYS with:

- **TMUL/MAC family** — widening multiply (WMUL), fused multiply-add
  (FMA), lane-wise accumulate (MAC), 4-way dot product (DOTACC)
- **Saturating arithmetic** — TMODE bit 5 enables clamping on overflow
- **Rounding shifts** — TMODE bit 6 enables round-to-nearest on VSHR
- **Tile views** — SHUFFLE (arbitrary permutation), PACK/UNPACK (width
  conversion), row/col rotate/mirror (RROT)
- **Extended TALU** — per-lane VSHR, VSHL, VCLZ (via EXT.8 prefix)
- **Enhanced reductions** — sum-of-squares (SUMSQ), min/max with index
  (MINIDX/MAXIDX)
- **Strided/2D addressing** — TSTRIDE_R/C, TTILE_H/W CSRs + LOAD2D/STORE2D
  for non-contiguous tile loads (e.g., 8×8 patches from a 640-wide framebuffer)
- **FP16 / bfloat16** — 32-lane half-precision tile operations with
  FP32 accumulation for DOT/SUM/SUMSQ

All extended tile operations are implemented in both the emulator
(`megapad64.py`) and RTL (`fpga/rtl/mp64_tile.v`, `mp64_fp16_alu.v`),
with 53 tile testbench tests passing.

### Crypto Accelerators (MMIO)

| Block | Performance | Use Case |
|-------|-------------|----------|
| AES-256-GCM | 16 bytes / 12 cycles | Authenticated encryption for storage and network |
| SHA-3/SHAKE | 136 bytes / 41 cycles | Hashing, key derivation, XOF |
| CRC32/CRC64 | 8 bytes / cycle | Data integrity for disk sectors and network frames |

### Per-Core Infrastructure

| Feature | CSR Range | Description |
|---------|-----------|-------------|
| Tile DMA | 0x50–0x55 | Descriptor-ring DMA engine for async tile copies |
| QoS | 0x58–0x59 | Per-core bus priority weight and bandwidth limit |
| BIST | 0x60–0x63 | Memory self-test (March C−, checkerboard, addr-as-data) |
| Tile self-test | 0x64–0x65 | Datapath functional check (~200 cycles) |
| Perf counters | 0x68–0x6C | Cycles, stalls, tile ops, ext-mem beats |
| I-Cache | 0x70–0x72 | Instruction cache control, hit/miss counters |

---

## Software Architecture

### Layer Diagram

```
┌─────────────────────────────────────────────────┐
│  User Code / REPL                               │
│  (Forth words, scripts, interactive commands)    │
├─────────────────────────────────────────────────┤
│  KDOS v1.1  (kdos.f, 3,850 lines)              │
│  ┌───────────┬───────────┬────────────────────┐ │
│  │  Buffers  │  Kernels  │   Pipelines        │ │
│  │  (§2–§3)  │  (§4–§5)  │   (§6)             │ │
│  ├───────────┼───────────┼────────────────────┤ │
│  │  Storage  │ MP64FS    │  Doc Browser       │ │
│  │  (§7)     │ (§7.6)    │  (§7.7)            │ │
│  ├───────────┼───────────┼────────────────────┤ │
│  │ Scheduler │ Screens   │  Data Ports (NIC)  │ │
│  │  (§8)     │ (§9)      │  (§10)             │ │
│  ├───────────┴───────────┴────────────────────┤ │
│  │ Dashboard, Help, Startup, Bundles (§12–§15) │ │
│  └────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────┤
│  BIOS v1.0  (bios.asm, 9,895 lines)            │
│  Subroutine-threaded Forth, 265 dictionary words│
│  Disk I/O, FSLOAD, UART, timer, tile engine     │
├─────────────────────────────────────────────────┤
│  Megapad-64 Hardware                            │
│  4× CPU, RAM+BIST, UART, Timer, Storage, NIC,  │
│  Tile Engine+FP16, AES, SHA-3, CRC, DMA, QoS   │
└─────────────────────────────────────────────────┘
```

### Boot Sequence

The full boot process from power-on to the KDOS REPL:

1. **CPU reset** — PSEL=3, SPSEL=15, PC=0, SP=top of RAM
2. **BIOS initializes** — sets up IVT (bus fault handler), configures
   UART, initializes the Forth dictionary (HERE, LATEST, base number,
   compilation state)
3. **Disk detection** — BIOS checks `DISK@` status register bit 7
4. **If disk present:** BIOS reads the MP64FS directory and scans for
   the first file with type=3 (Forth)
   - Reads its data sectors into a RAM buffer
   - EVALUATEs each line via FSLOAD
5. **KDOS loads** — the Forth file (typically `kdos.f`) causes:
   - All 2,972 lines of KDOS to be read from disk
   - Each line is EVALUATE'd, compiling definitions into the dictionary
   - §14 startup code runs: prints banner, loads filesystem (`FS-LOAD`)
6. **REPL ready** — the outer interpreter (`QUIT`) awaits user input

**If no disk:** BIOS skips step 4, drops directly into the bare Forth
REPL.  KDOS can still be loaded via `--forth kdos.f` on the CLI (UART
injection), but without filesystem access.

### Memory Usage (Typical)

After a full KDOS boot with filesystem loaded:

| Region | Approximate Size | Contents |
|--------|-----------------|----------|
| BIOS code | ~20 KB | Machine code, IVT, boot logic |
| KDOS dictionary | ~40–50 KB | Compiled definitions, strings |
| Buffers | ~10 KB | 6 demo buffers, histogram bins |
| FS cache | ~3 KB | Superblock (512B) + bitmap (512B) + directory (2048B) |
| Task stacks | 2 KB | 8 × 256 bytes |
| Frame buffer | 1.5 KB | NIC frame receive buffer |
| **Total HERE** | ~80 KB | Leaves ~950 KB free for user data/code |

---

## Interrupt Model

The Megapad-64 supports a simple vectored interrupt scheme.  When an
interrupt or trap fires:

1. Current FLAGS and PC are pushed onto the stack
2. IE is cleared (masks further interrupts)
3. PC jumps to `mem64(IVT_BASE + 8 × vector_id)`
4. Handler runs
5. `RTI` restores PC and FLAGS (including IE)

**Currently used vectors:**

| Vector | Used By | Purpose |
|--------|---------|---------|
| `IVEC_BUS_FAULT` (5) | BIOS | Catches accesses beyond memory bounds; prints fault address and aborts |
| `IVEC_TIMER` (7) | KDOS scheduler | Sets `PREEMPT-FLAG` for cooperative preemption |
| `IVEC_DIV_ZERO` (4) | Hardware | Traps on division by zero |

---

## Tile Engine Integration

The tile engine is tightly integrated with the CPU via CSR registers and
the MEX instruction family.  Key concepts:

- **Tiles** are 64-byte aligned blocks in main RAM
- Operations run on **lanes** within a tile (64×8-bit, 32×16-bit, etc.)
- Source/destination addresses are set via CSRs (TSRC0, TSRC1, TDST)
- Results of reductions and dot products go to the **256-bit accumulator**

In KDOS, tile operations power the buffer subsystem (B.SUM, B.MIN, B.MAX,
B.ADD, B.SUB) and several kernels (kadd, ksum, kstats, knorm, kcorrelate).
See `docs/tile-engine.md` for a complete programming guide and
`docs/extended-tpu-spec.md` for the full enhanced tile engine, crypto,
DMA, and reliability specifications.

---

## File Summary

| Component | File | Lines | Role |
|-----------|------|-------|------|
| CPU emulator | `megapad64.py` | 2,516 | Full ISA + extended tile engine implementation |
| System glue | `system.py` | 474 | Quad-core SoC, MMIO, mailbox IPI, spinlocks |
| Devices | `devices.py` | 964 | UART, Timer, Storage, NIC, Mailbox, Spinlock, CRC |
| BIOS | `bios.asm` | 9,895 | Forth interpreter, boot, multicore, 265 dictionary words |
| OS | `kdos.f` | 3,850 | Buffers, kernels, TUI, FS, crypto, multicore dispatch |
| Assembler | `asm.py` | 788 | Two-pass macro assembler |
| CLI/Monitor | `cli.py` | 995 | Debug, inspect, boot |
| Disk tools | `diskutil.py` | 1,038 | Build/manage disk images |
| Tests | `test_megapad64.py` | 2,193 | 23 CPU + tile engine tests |
| Tests | `test_system.py` | 7,308 | 570 integration tests |
| FPGA RTL | `fpga/rtl/` | 7,242 | 13 Verilog modules (CPU, tile, FP16 ALU, SoC, peripherals) |
| FPGA tests | `fpga/sim/` | 3,930 | 8 testbenches (72 hardware tests) |
