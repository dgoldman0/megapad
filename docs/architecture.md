# Megapad-64 System Architecture

This document describes the Megapad-64 computer system as a whole — how
the CPU, memory, and peripheral devices fit together, and how the software
layers (BIOS, KDOS, filesystem) build on top of the hardware.

---

## System Block Diagram

```
┌───────────────────────────────────────────────────────────┐
│                    Megapad-64 CPU (×16 cores)             │
│  4 full cores         ┌──────────────────┐  ┌───────────┐ │
│  32 × 64-bit GPRs     │   Tile Engine    │  │ Perf Ctrs │ │
│  4 KiB I-Cache        │  (MEX extension) │  │ (4 × 64b) │ │
│  8-bit Flags          │  FP16 / bf16     │  └───────────┘ │
│  256-bit Accumulator  │  DMA queue       │                │
│  Perf counters        └──────────────────┘                │
│  + 3 micro-clusters (4 scalar μ-cores ea., shared MUL/DIV │
│    + tile/MEX engine, 1 KiB scratchpad, HW barrier)       │
└───────────────┬───────────────────────────────────────────┘
                │  64-bit data bus (weighted round-robin QoS)
    ┌───────────┴───────────────────────────┐
    │            Memory Map                  │
    │                                        │
    │  0x0000_0000 ┌──────────────────────┐  │
    │              │     Bank 0 (System)  │  │
    │              │   (1 MiB BRAM + BIST)│  │
    │              │                      │ │
    │              │  BIOS code + dict    │ │
    │              │  KDOS Forth dict     │ │
    │              │  Buffers & data      │ │
    │              │  FS cache (3 KB)     │ │
    │              │  Task stacks (2 KB)  │ │
    │              │         ↓ HERE       │ │
    │              │         ...          │ │
    │              │         ↑ SP         │ │
    │              │  Data stack (grows ↓)│ │
    │              │  Return stack        │ │
    │  0x000F_FFFF └──────────────────────┘ │
    │              ...                      │
    │  0xFFD0_0000 ┌──────────────────────┐ │
    │              │  Banks 1–3 (HBW Math)│ │
    │              │   3 MiB, high-BW     │ │
    │              │   tile/SIMD working  │ │
    │  0xFFFF_FFFF └──────────────────────┘ │
    │  FFFF_FF00+  ┌──────────────────────┐ │
    │   0x0000     │  UART                │ │
    │   0x0100     │  Timer               │ │
    │   0x0200     │  Storage Controller  │ │
    │   0x0300     │  System Info (R/O)   │ │
    │   0x0400     │  NIC                 │ │
    │   0x0500     │  Mailbox (IPI)       │ │
    │   0x0600     │  Spinlock            │ │
    │   0x0700     │  AES-256/128-GCM     │ │
    │   0x0780     │  SHA-3 / SHAKE       │ │
    │   0x07E0     │  QoS Config          │ │
    │   0x0800     │  TRNG                │ │
    │   0x08C0     │  NTT Engine          │ │
    │   0x0900     │  KEM (ML-KEM-512)    │ │
    │   0x0A00     │  Framebuffer         │ │
    │   0x0B00     │  RTC / System Clock  │ │
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
| `0x0000_0000` | **Bank 0** — BIOS code (loaded at boot, ~20 KB) |
| `0x0000_4F00`+ | Forth dictionary grows upward from HERE |
| *(varies)* | KDOS code, buffer data, FS caches, task stacks |
| *(varies)* | Free space between HERE and SP |
| ← SP | Data stack grows downward from top of Bank 0 |
| `RAM_SIZE` | Top of Bank 0 (default 0x0010_0000 = 1 MiB) |
| `0x0010_0000`+ | **External Memory** — up to ~4 GiB (userland dictionary + XMEM allocator; emulator default 16 MiB via `--extmem`) |
| `0xFF00_0000`–`0xFF3F_FFFF` | **VRAM** — 4 MiB dedicated framebuffer (double-buffered 1280×720 RGBA) |
| `0xFFD0_0000`–`0xFFFF_FFFF` | **Banks 1–3** — 3 MiB HBW math RAM for tile/SIMD working buffers |

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
| **UART Geometry** | `+0x0010` | 16 bytes | Terminal dimensions, resize status/request |
| **Timer** | `+0x0100` | 16 bytes | 32-bit timer with compare-match |
| **Storage** | `+0x0200` | 16 bytes | Sector-based disk controller |
| **System Info** | `+0x0300` | 96 bytes | Board ID, config, core topology, HBW, VRAM, cluster enable |
| **NIC** | `+0x0400` | 128 bytes | Network interface controller |
| **Mailbox** | `+0x0500` | 16 bytes | Inter-core IPI (data + send + status + ack) |
| **Spinlock** | `+0x0600` | 64 bytes | Hardware spinlocks (16 locks, 4 bytes each) |
| **AES-256/128-GCM** | `+0x0700` | 64 bytes | Authenticated encryption accelerator (AES-256 and AES-128) |
| **SHA-3/SHAKE** | `+0x0780` | 96 bytes | Keccak hash / XOF accelerator (SHA3-256, SHA3-512, SHAKE) |
| **QoS Config** | `+0x07E0` | 16 bytes | Global bus QoS quantum / weights |
| **TRNG** | `+0x0800` | 64 bytes | Hardware true random number generator |
| **Port I/O Bridge** | `+0x0880` | 16 bytes | Remap CSR — maps OUT N / INP N to configurable MMIO targets |
| **NTT Engine** | `+0x08C0` | 64 bytes | 256-point Number Theoretic Transform (ML-KEM/ML-DSA) |
| **KEM** | `+0x0900` | 64 bytes | ML-KEM-512 key encapsulation accelerator |
| **WOTS+ Chain Accel** | `+0x08A0` | 32 bytes | SPHINCS+ WOTS hash chain sequencer (wraps SHA3/SHAKE, DMA-read context) |
| **Framebuffer** | `+0x0A00` | 64 bytes | Tile-based framebuffer controller |
| **RTC / System Clock** | `+0x0B00` | 32 bytes | 64-bit ms uptime + ms epoch + calendar (sec/min/hour/day/mon/year/dow) + alarm IRQ |

Any access outside RAM and the MMIO aperture triggers a **bus fault**
(vector `IVEC_BUS_FAULT`).  In the RTL, the bus arbiter enforces
MMIO/MEM ACK timeouts (63/255 cycles); on timeout it returns sentinel
data (`0xDEAD_DEAD_DEAD_DEAD`), asserts `bus_err`, and fires `IRQX_BUS`.
In the emulator, unmapped MMIO offsets raise `BusError`, which the SoC
layer converts to `TrapError(IVEC_BUS_FAULT)`.

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
| TX_FLUSH | `+0x06` | W | Drain the TX ring buffer (triggers batch output callback). |
| TX_RING_BASE | `+0x08`–`+0x0F` | W | 64-bit LE pointer to the TX ring descriptor in RAM. |

**BIOS words:** `KEY` reads from RX_DATA (blocking), `KEY?` checks
STATUS bit 1, `EMIT` appends to a 4096-byte TX ring buffer in RAM
(flushed automatically when full, or explicitly via `TX-FLUSH`).

> **Hardware note:** The TX ring buffer is an *emulator-side* optimisation.
> On real hardware MMIO stores are single bus cycles, so the per-byte cost
> is negligible and the ring provides no speedup.  For FPGA/ASIC targets a
> DMA engine wired to TX_FLUSH would be needed to make the buffer useful.
> The layout (head + contiguous 4 KB data) is already DMA-friendly.

---

## UART Geometry (Terminal Dimensions)

The UART Geometry block lives within the UART address range and exposes
the terminal's column/row count as MMIO registers.  The host (emulator
display or real terminal) updates these on resize; firmware can also
request a resize and check whether it was accepted or denied.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| COLS | `+0x10`–`+0x11` | RW | 16-bit LE terminal column count. |
| ROWS | `+0x12`–`+0x13` | RW | 16-bit LE terminal row count. |
| STATUS | `+0x14` | RW | **bit 0:** `RESIZED` — set by host on resize (write 1 to clear).  **bit 1:** `REQ_DENIED` — host denied a firmware resize request (write 1 to clear). |
| CTRL | `+0x15` | RW | **bit 0:** `RESIZE_IE` — enable resize interrupt/notification.  **bit 1:** `REQ_RESIZE` — firmware sets to request resize; host clears after accept/deny. |
| REQ_COLS | `+0x16`–`+0x17` | RW | 16-bit LE requested columns (firmware writes before setting `REQ_RESIZE`). |
| REQ_ROWS | `+0x18`–`+0x19` | RW | 16-bit LE requested rows. |

**Host-initiated resize flow:**
1. Host updates COLS/ROWS, sets `STATUS.RESIZED = 1`.
2. Firmware polls `RESIZED?` → reads new COLS/ROWS, clears flag.

**Firmware-requested resize flow:**
1. Firmware writes REQ_COLS/REQ_ROWS, sets `CTRL.REQ_RESIZE = 1`.
2. Host reads request, attempts resize.
3. On success: host updates COLS/ROWS, clears `REQ_RESIZE`, sets `RESIZED`.
4. On failure: host clears `REQ_RESIZE`, sets `STATUS.REQ_DENIED`.

**BIOS words:** `COLS` ( -- n ), `ROWS` ( -- n ), `TERMSIZE` ( -- cols rows ),
`RESIZED?` ( -- flag ), `RESIZE-DENIED?` ( -- flag ),
`RESIZE-REQUEST` ( cols rows -- ).

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

KDOS uses this for **cooperative preemption checkpoints** — `PREEMPT-ON`
configures a 50,000-cycle timer with auto-reload, and `CORE-CHECKPOINT`
(`YIELD?`) checks the per-core flag set by the timer handler. Core 0 may
retire its current KDOS task; secondary one-shot workers acknowledge the
checkpoint without touching the core-0 scheduler.

---

## RTC / System Clock

A combined system clock peripheral providing:
- **64-bit monotonic uptime** counter in milliseconds since boot (read-only, free-running)
- **64-bit epoch** counter in milliseconds since the Unix epoch (read/write, settable)
- **Calendar** registers (second, minute, hour, day, month, year, day-of-week)
- **Alarm** interrupt on hour:minute:second match

Both 64-bit counters increment every millisecond (prescaled from the 100 MHz system clock).
Reading byte 0 of UPTIME (+0x00) or byte 0 of EPOCH (+0x08) **latches** the full 64-bit
value so that software can safely read the remaining bytes without tearing.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| UPTIME | `+0x00`–`+0x07` | R | 64-bit ms since boot.  Read +0x00 to latch. |
| EPOCH | `+0x08`–`+0x0F` | RW | 64-bit ms since Unix epoch.  Read +0x08 to latch; write byte-by-byte to set. |
| SEC | `+0x10` | RW | Seconds (0–59) |
| MIN | `+0x11` | RW | Minutes (0–59) |
| HOUR | `+0x12` | RW | Hours (0–23) |
| DAY | `+0x13` | RW | Day of month (1–31) |
| MON | `+0x14` | RW | Month (1–12) |
| YEAR_LO | `+0x15` | RW | Year low byte |
| YEAR_HI | `+0x16` | RW | Year high byte |
| DOW | `+0x17` | RW | Day of week (0=Sun … 6=Sat) |
| CTRL | `+0x18` | RW | **bit 0:** run/stop.  **bit 1:** alarm IRQ enable. |
| STATUS | `+0x19` | RW | **bit 0:** alarm flag (W1C).  **bit 1:** 1 Hz tick (W1C).  **bit 2:** 1 ms tick (W1C). |
| ALARM_S | `+0x1A` | RW | Alarm seconds |
| ALARM_M | `+0x1B` | RW | Alarm minutes |
| ALARM_H | `+0x1C` | RW | Alarm hours |

**BIOS words:** `MS@` (uptime ms), `EPOCH@` (epoch ms), `RTC@` (read calendar),
`RTC!` (set calendar), `RTC-CTRL!`, `RTC-ALARM!`, `RTC-ACK` (clear alarm flag).

IRQ vector: `IVEC_RTC` (16).

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
`DISK-WRITE`, `DISK-FLUSH`, `DISK@` (read status).

---

## System Info

Board identification and core-topology registers (12 × 64-bit aligned,
96 bytes total).  All registers are read-only except CLUSTER_EN.

| Register | Offset | Width | Default | Description |
|----------|--------|-------|---------|-------------|
| BOARD_ID_VER | `+0x00` | 64-bit | `0x4D503634_00020001` | `"MP64"` + version 2.1 |
| BANK0_SIZE | `+0x08` | 64-bit | 1 MiB | Bank 0 system RAM size in bytes |
| NUM_CORES | `+0x10` | 64-bit | varies | Total core count (full + micro) |
| CLUSTER_EN | `+0x18` | 64-bit | all-ones | Per-cluster enable mask (R/W) |
| HBW_BASE | `+0x20` | 64-bit | `0xFFD0_0000` | HBW math RAM base address |
| HBW_SIZE | `+0x28` | 64-bit | 3 MiB | HBW region size in bytes |
| INT_MEM_TOTAL | `+0x30` | 64-bit | 4 MiB | Total internal memory (all banks) |
| EXT_MEM_BASE | `+0x38` | 64-bit | `0x0010_0000` | External memory base address |
| EXT_MEM_SIZE | `+0x40` | 64-bit | varies | External memory size in bytes |
| NUM_FULL | `+0x48` | 64-bit | varies | Number of full (major) cores |
| VRAM_BASE | `+0x50` | 64-bit | `0xFF00_0000` | Dedicated VRAM base address |
| VRAM_SIZE | `+0x58` | 64-bit | 4 MiB | Dedicated VRAM size in bytes |

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
(`megapad64.py`) and RTL (`rtl/gpu/mp64_tile.v`, `rtl/gpu/mp64_fp16_alu.v`),
with 53 tile testbench tests passing.

### Crypto Accelerators

| Block | Performance | Use Case |
|-------|-------------|----------|
| AES-256/128-GCM | 16 bytes / 12 cycles | Authenticated encryption for storage and network |
| SHA-3/SHAKE | 136 bytes / 41 cycles | Hashing (SHA3-256, SHA3-512), key derivation, XOF |
| SHA-256 | 64 bytes / 64 cycles | TLS 1.3, HMAC-SHA256, HKDF (per-core ISA, no MMIO) |
| CRC32/CRC64 | 8 bytes / cycle | Data integrity (per-core ISA, no MMIO) |
| Field ALU | 1 FMUL / ~255 cycles | GF(2²⁵⁵−19) field arithmetic (8 modes incl. X25519, per-core ISA) |
| NTT Engine | 256-pt NTT / ~1280 cycles | Lattice crypto polynomial multiply (ML-KEM, ML-DSA) |
| KEM | keygen+encaps / ~500 cycles | ML-KEM-512 key encapsulation (FIPS 203) |
| TRNG | 64 bits / 2 cycles | Hardware true random number generator |

### Field ALU (GF(2²⁵⁵−19) Coprocessor)

A general-purpose field arithmetic unit implemented as per-core ISA
instructions (EXT.CRYPTO FB, sub-ops 0x20–0x2D).  Eight operation modes:

| Mode | Name | Description |
|------|------|-------------|
| 0 | X25519 | Full scalar multiplication (Montgomery ladder, ~255 iterations) |
| 1 | FADD | (a + b) mod p |
| 2 | FSUB | (a − b) mod p |
| 3 | FMUL | (a · b) mod p (shared 256-bit multiplier) |
| 4 | FSQR | a² mod p |
| 5 | FINV | a^(p−2) mod p (Fermat's little theorem) |
| 6 | FPOW | a^b mod p (general exponentiation) |
| 7 | MUL_RAW | Raw 256×256→512-bit multiply (no modular reduction) |

Operands are staged via CSR writes (ACC0–ACC3 for A, TSRC0 for B
address); results read back via CSR reads.  The ISA instructions are
synchronous — each completes in deterministic cycles with no polling.

Zero additional DSPs — reuses the existing shared 256-bit multiplier.
**BIOS words:** `GF-A!`, `GF-R@`, `GF-PRIME`, `LOAD-PRIME`,
`FADD`, `FSUB`, `FMUL`, `FSQR`, `FINV`, `FPOW`, `FMUL-RAW`, `FMUL-ADD-RAW`.
**KDOS words (§1.10):** `F+`, `F-`, `F*`.

### NTT Engine (Number Theoretic Transform)

A 256-point NTT accelerator at MMIO base `+0x08C0` for lattice-based
post-quantum cryptography (ML-KEM, ML-DSA).

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **1:** NTT_FWD, **2:** NTT_INV, **3:** NTT_PMUL, **4:** NTT_PADD |
| Q | `+0x08` | RW | Modulus (default 3329 for ML-KEM, 8380417 for ML-DSA) |
| IDX | `+0x10` | RW | Coefficient index (0–255), auto-increments on RESULT read |
| LOAD_A | `+0x18` | W | Write coefficient to polynomial A[IDX] |
| LOAD_B / RESULT | `+0x20` | RW | Write to B[IDX], read from work[IDX] |

Internal storage: 3 × 256 × 32-bit register files (poly_a, poly_b, work).
Cooley-Tukey butterfly with precomputed twiddle ROM (ω = 17 for q = 3329).
~1,280 cycles for forward/inverse NTT, ~256 cycles for PMUL/PADD.

**BIOS words:** `NTT-LOAD`, `NTT-STORE`, `NTT-FWD`, `NTT-INV`, `NTT-PMUL`,
`NTT-PADD`, `NTT-SETQ`, `NTT-STATUS@`, `NTT-WAIT`.
**KDOS word (§1.11):** `NTT-POLYMUL` (full polynomial multiply via NTT).

### KEM (ML-KEM-512 Key Encapsulation)

An ML-KEM-512 accelerator framework at MMIO base `+0x0900`.  Provides
hardware-managed key/ciphertext buffers and keygen/encaps/decaps operations.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **1:** KEYGEN, **2:** ENCAPS, **3:** DECAPS |
| BUF_SEL | `+0x08` | RW | Buffer select: 0=SEED(64B), 1=PK(800B), 2=SK(1632B), 3=CT(768B), 4=SS(32B) |
| DIN / DOUT | `+0x10` | RW | Byte-streaming data port (auto-increment index) |
| IDX_SET / BUF_SIZE | `+0x18` | RW | Write: set byte index; Read: selected buffer size |
| IDX | `+0x20` | R | Current byte index |

5 internal buffers (3,296 bytes total).  Current RTL has stub crypto
datapath (deterministic XOR fill); phase 2 will add real CRYSTALS-Kyber
polynomial arithmetic.

**BIOS words:** `KEM-SEL!`, `KEM-LOAD`, `KEM-STORE`, `KEM-KEYGEN`,
`KEM-ENCAPS`, `KEM-DECAPS`, `KEM-STATUS@`.
**KDOS words (§1.12–§1.13):** `KYBER-KEYGEN`, `KYBER-ENCAPS`,
`KYBER-DECAPS`, `PQ-EXCHANGE` (hybrid X25519 + ML-KEM).

### SHA-256 (SHA-2 — Per-Core ISA)

SHA-256 hashing is implemented as per-core ISA instructions via the
EXT.CRYPTO prefix (`FB`), not MMIO.  Each core has its own independent
SHA-2 engine with state held in crypto CSRs (0x82–0x84).

**Instructions:** `sha.init`, `sha.din`, `sha.final`, `sha.dout`
(see `docs/isa-reference.md` § EXT.CRYPTO for full encoding).

The engine implements full FIPS-180-4 SHA-256 compression with K
constants, Σ/σ/Ch/Maj functions, 16-entry W message schedule with
on-the-fly expansion, and automatic padding.  A 64-byte block buffer
in RAM (pointed to by TSRC0 CSR) accumulates input bytes; compression
runs automatically when the buffer fills or on `sha.final`.

**BIOS words:** `SHA256-INIT`, `SHA256-UPDATE`, `SHA256-FINAL`,
`SHA256-STATUS@`, `SHA256-DOUT@`.
**KDOS words:** `SHA256`, `HMAC-SHA256`, `HKDF-SHA256-EXTRACT`,
`HKDF-SHA256-EXPAND`.

### TRNG (True Random Number Generator)

A hardware TRNG at MMIO base `+0x0800`.  On FPGA: ring-oscillator entropy
source with LFSR conditioner and health monitoring.  In the emulator:
backed by `os.urandom()`.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| DATA | `+0x00` | R | 64-bit random value |
| STATUS | `+0x08` | R | **bit 0:** ready, **bit 1:** health OK |
| CONTROL | `+0x10` | RW | **bit 0:** enable, **bit 1:** reseed |
| SEED | `+0x18` | W | Manual seed input |

**BIOS words:** `RANDOM`, `RANDOM8`, `SEED-RNG`.

### Per-Core Infrastructure

| Feature | CSR Range | Description |
|---------|-----------|-------------|
| Tile DMA | 0x50–0x55 | Descriptor-ring DMA engine for async tile copies |
| QoS | 0x58–0x59 | Per-core bus priority weight and bandwidth limit |
| BIST | 0x60–0x63 | Memory self-test (March C−, checkerboard, addr-as-data) |
| Tile self-test | 0x64–0x65 | Datapath functional check (~200 cycles) |
| Perf counters | 0x68–0x6C | Cycles, stalls, tile ops, ext-mem beats |
| I-Cache | 0x70–0x72 | Instruction cache control, hit/miss counters |

### Micro-Core Architecture

Each micro-cluster contains 4 scalar micro-cores sharing a MUL/DIV unit,
a tile/MEX engine (round-robin arbitrated, +3 cycle overhead), 1 KiB
scratchpad, and a hardware barrier.  Micro-cores run the same 64-bit
native ISA as full cores **minus** the CDP1802-heritage features:

| Stripped Feature | Families / Sub-ops | Rationale |
|------------------|--------------------|-----------|
| D accumulator, Q flip-flop, T register | State | Saves ~17 FFs per core |
| MEMALU (LDX, OR.X, ADD.X, …) | Family 0x8 | All operate on D + M(R(X)) |
| Port I/O (OUT/INP) | Family 0x9 | 1802-style 7-port I/O |
| GLO / GHI / PLO / PHI | Family 0x6 sub 0xC–0xF | D ↔ GPR byte transfer |
| RET / DIS / MARK / SAV / SEQ / REQ | Family 0x0 sub 0x5–0xA | 1802 SCRT + Q |

All stripped opcodes trap as `ILLEGAL_OP` (interrupt vector 0x01).
CSR reads to D/DF/Q/T return 0; writes are silently ignored.

Micro-cores **retain**: INC, DEC, branch, long-branch, MEM (load/store),
IMM (arithmetic immediates), ALU, MUL/DIV (shared), tile/MEX (shared),
SEP, SEX, CSR (reduced set), and CALL.L / RET.L.

Estimated area savings: ~300 FFs / ~200 LUTs per micro-core vs a full core.

### Privilege Model

The Megapad-64 implements a two-level privilege model:

| Level | Value | Name | Context |
|-------|-------|------|---------|
| 0 | `PRIV=0` | **Supervisor** | BIOS, KDOS, interrupt/trap handlers |
| 1 | `PRIV=1` | **User** | Application code |

The current privilege level is stored in **CSR_PRIV** (address `0x0A`).
The CPU resets to supervisor mode (level 0).

#### Privilege Transitions

```
                ┌──────────────┐
                │  Supervisor  │  ←── Reset, TRAP, IRQ
                │   (priv=0)   │
                └──────┬───────┘
                       │ CSRW CSR_PRIV, 1
                       ▼
                ┌──────────────┐
                │    User      │  ←── Application code
                │   (priv=1)   │
                └──────┬───────┘
                       │ TRAP / IRQ / privilege fault
                       ▼
                ┌──────────────┐
                │  Supervisor  │  ←── Handler runs in supervisor mode
                │   (priv=0)   │
                └──────┬───────┘
                       │ RTI (restores saved privilege from bit 8 of flags qword)
                       ▼
                ┌──────────────┐
                │  Restored    │  ←── Returns to whatever level was saved
                └──────────────┘
```

#### Restricted Operations

The following instruction families and sub-operations are **supervisor-only**.
Executing them from user mode triggers an `IVEC_PRIV_FAULT` (vector 15):

| Category | Opcodes | Rationale |
|----------|---------|-----------|
| MEMALU (family 0x8) | LDX, OR.X, ADD.X, etc. | Operate on D + M(R(X)); 1802 heritage |
| Port I/O (family 0x9) | OUT 1–7, INP 1–7 | Direct hardware I/O access |
| SEP (family 0xA) | SEP Rn | Arbitrary PC register swap |
| SEX (family 0xB) | SEX Rn | Arbitrary data pointer swap |
| SYS sub-ops 0x5–0xA | RET, DIS, MARK, SAV, SEQ, REQ | 1802 SCRT + Q flip-flop |
| IMM sub-ops 0xC–0xF | GLO, GHI, PLO, PHI | D ↔ GPR byte transfer |

Additionally, **CSR writes** to the following registers are supervisor-only:

| CSR | Address | Reason |
|-----|---------|--------|
| CSR_PRIV | `0x0A` | Controls privilege level itself |
| CSR_IVT_BASE | `0x04` | Relocates interrupt vector table |
| CSR_IE | `0x09` | Enables/disables interrupts globally |
| CSR_BIST_CMD | `0x60` | Triggers memory self-test |
| CSR_ICACHE_CTRL | `0x70` | Cache enable/invalidate |

CSR **reads** are unrestricted — user code can freely inspect any CSR.

#### Privilege in Trap/Interrupt Context

When a TRAP instruction or hardware interrupt fires, the CPU:
1. Pushes a 64-bit flags qword with **bit 8 = current privilege level**
2. Pushes the 64-bit PC
3. Sets `priv_level ← 0` (escalate to supervisor)

When RTI executes, it restores the privilege level from bit 8 of the
popped flags qword.  This is backward-compatible: code written before
the privilege model pushes flags with bit 8 = 0 (supervisor), which is
the correct default for pre-privilege firmware.

---

## Software Architecture

### Layer Diagram

```
┌─────────────────────────────────────────────────┐
│  User Code / REPL                               │
│  (Forth words, scripts, interactive commands)    │
├─────────────────────────────────────────────────┤
│  KDOS v1.1  (kdos.f, 10,225 lines)             │
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
│  BIOS v1.0  (bios.asm, 14,524 lines)            │
│  Subroutine-threaded Forth, 360 dictionary words │
│  Disk I/O, FSLOAD, UART, timer, tile engine      │
├─────────────────────────────────────────────────┤
│  Megapad-64 Hardware                            │
│  4× CPU, RAM+BIST, UART, Timer, Storage, NIC,  │
│  Tile Engine+FP16, AES, SHA-3, SHA-256,          │
│  DMA, QoS, TRNG, Field ALU, NTT, KEM, FB         │
│  CRC: per-core ISA + cluster-shared (no MMIO)     │
└─────────────────────────────────────────────────┘
```

### JIT Compiler

The BIOS Forth compiler includes an optional **compile-time JIT** that
replaces `sep r16` + inline-XT call sequences with inlined native
machine code for 18 common primitives.  This is a *code-size* and
*runtime* optimisation: each inlined primitive saves the 10-byte STC
call overhead and eliminates the call/return cycle cost at execution
time.

Beyond simple primitive inlining, the JIT performs two additional
optimisations via a one-entry peephole lookback buffer:

- **Literal folding** — a small literal followed by an ALU word
  (`+`, `-`, `AND`, `OR`, `XOR`) is fused into a single immediate
  instruction (e.g. `3 +` → `addi r1, 3`), producing 7 bytes instead
  of 19.
- **Bigram peephole** — consecutive inlined primitives are checked
  against a 6-entry bigram table and replaced with fused sequences
  that eliminate redundant loads and stores (e.g. `DUP +` → `add r1,r1`,
  `DUP DROP` / `SWAP SWAP` → no-op).

**How it works:**

1. When `JIT-ON` has been executed, every word reference compiled by the
   outer interpreter or `EVALUATE` passes through `jit_compile_word`.
2. Before inlining the current word, `jit_compile_word` checks the
   peephole state (`var_jit_last_type/value/here`).  If the previous
   emission was a literal (type 1) and the current word is a foldable
   ALU op, the literal is rewound and a fused immediate sequence is
   emitted via `jit_emit_lit_fold`.  If the previous emission was an
   inlined primitive (type 2), the bigram table (`jit_bigram_table`) is
   scanned for a matching pair; on hit, the previous primitive's code
   is rewound and the fused body is emitted.
3. The compiler scans an 18-entry inline table (`jit_inline_table`) that
   maps dictionary entry addresses to pre-assembled native byte
   sequences (3–13 bytes each).
4. If a match is found, the native bytes are copied directly into the
   definition being compiled.  Otherwise, a normal `call.l` is emitted.
5. After inlining, the peephole state is updated so the next compilation
   step can check for further fusions.
6. Literals pass through `jit_compile_literal`, which emits compact
   8-byte sequences for values 0–255 and a 9-byte sequence for −1
   (`TRUE`), instead of the standard 16-byte `ldi64` + push.
7. When an IMMEDIATE word executes during compilation (`;`, `IF`, `DO`,
   etc.), the peephole state is flushed to prevent stale matches.

**Inlined primitives (18):** `DUP` `DROP` `SWAP` `OVER` `NIP` `2DROP`
`+` `-` `AND` `OR` `XOR` `INVERT` `NEGATE` `@` `!` `CELLS` `CELL+`
`>BODY`

**Bigram patterns (6):** `DUP +`, `SWAP DROP`, `DUP @`, `OVER +`,
`DUP DROP`, `SWAP SWAP`

**Performance:** 1.4×–2.1× speedup on primitive-heavy tight loops.
Compilation overhead during a full KDOS load is negligible (+0.8%).
A typical KDOS load fires ~512 literal folds, ~38 bigram peepholes,
and ~5100 primitive inlines, saving ~50 KB of compiled code.

JIT is **off by default** and does not affect words compiled before
`JIT-ON` is executed.  Use `JIT-STATS` to see how many primitives were
inlined, how many folds and peepholes fired, and how many bytes were
saved.

### 1802 Heritage Restoration

Several phases of recent work restored authentic CDP 1802 idioms to the
BIOS while improving performance and adding new capabilities.

#### SEP Dispatch (Phases 0–2, 4–5)

The BIOS threading model was migrated from a single ITC `NEXT` routine
to **SEP-based subroutine dispatch**:

- **R4** — `NEXT` (advance IP, fetch CFA, branch to it)
- **R5** — `ENTER` (push return address, enter a colon definition)
- **R6** — `EXIT` (pop return address, resume caller)

Each of these is a dedicated PC register switched with `SEP Rn`.  This
eliminates a `LBR` indirection on every Forth word dispatch, saving 3
machine cycles per call.  The C++ accelerator (`mp64_accel.cpp`)
recognises the `SEP R4/R5/R6` pattern and fast-paths it.

#### STXI Byte-Processing (Phase 7)

The new **STXI** instruction (opcode 0x89 — store via RX and increment)
and **STXD.D** (opcode 0x8B — store via RX and decrement with D) were
added to the ISA (RTL, emulator, assembler, and C++ accel).  16 BIOS
routines were converted from `STR RX / INC RX` pairs to single `STXI`
instructions:

`TYPE`, `S>NUMBER`, `NUMBER-PARSE`, `CMOVE`, `CMOVE>`, `FILL`, `PLACE`,
`+PLACE`, `COMPARE`, `SEARCH`, `CAPITALIZE`, `S-UPPER`, `DIGIT>CHAR`,
`UD/MOD-DIGIT`, `DUMP-ROW`, `FS-READ-SECTORS`

#### Cooperative Multitasking (Phase 8)

A lightweight cooperative multitasker was added to the BIOS:

- **R20** (REX-extended) is the task trampoline register
- `SEP R20` round-robin yields across up to 4 task slots
- Each task has independent data and return stacks
- A `task_cleanup` sentinel catches premature task exit

Eight dictionary words: **PAUSE**, **TASK-YIELD**, **BACKGROUND**,
**TASK-STOP**, **TASK?**, **BACKGROUND2**, **BACKGROUND3**,
**#TASKS**.

#### T-Register Fault Diagnostics (Phase 9)

The bus-fault handler now captures and displays the **T register**
(pre-interrupt X/P state), giving the programmer visibility into which
register pair was active when a fault occurred.  The MMIO routing in
`system.py` was also deduplicated (Phase 7 prep) to reduce dispatch
overhead.

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
   - All sections of KDOS to be read from disk
   - Each line is EVALUATE'd, compiling definitions into the dictionary
   - §14 startup code runs: prints banner, loads filesystem (`FS-LOAD`),
     initializes heap, loads `autoexec.f` (which chains `graphics.f`
     and `tools.f`), runs DHCP, enters userland memory isolation
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
| `IVEC_BUS_FAULT` (5) | BIOS | Catches accesses beyond memory bounds or unmapped MMIO offsets (bus timeout); prints fault address and aborts |
| `IVEC_TIMER` (7) | KDOS scheduler | Sets `PREEMPT-FLAG` for cooperative preemption |
| `IVEC_DIV_ZERO` (4) | Hardware | Traps on division by zero |
| `IVEC_RTC` (16) | Application | Fires on alarm match; cleared by writing 0x01 to STATUS (+0x19) |

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
| CPU emulator | `megapad64.py` | 3,002 | Full ISA + extended tile engine implementation |
| System glue | `system.py` | 991 | Quad-core SoC, MMIO, mailbox IPI, spinlocks |
| Devices | `devices.py` | 2,287 | UART, Timer, Storage, NIC, Mailbox, Spinlock, AES, SHA3, SHA256, TRNG, FieldALU, NTT, KEM, Framebuffer, RTC |
| BIOS | `bios.asm` | 14,524 | Forth interpreter, boot, multicore, 360 dictionary words |
| OS | `kdos.f` | 11,760 | Buffers, kernels, TUI, FS, crypto, networking, TLS 1.3, PQC, multicore |
| Tools | `tools.f` | 990 | ED line editor, SCROLL web client (HTTP/HTTPS/FTP/Gopher) |
| Assembler | `asm.py` | 792 | Two-pass macro assembler |
| CLI/Monitor | `cli.py` | 1,557 | Debug, inspect, boot, headless TCP server |
| Disk tools | `diskutil.py` | 1,162 | Build/manage disk images |
| Tests | `test_megapad64.py` | 2,193 | 23 CPU + tile engine tests |
| Tests | `test_system.py` | 24,033 | 1,592 integration tests (74 classes) |
| Tests | `test_networking.py` | 187 | 13 real-network tests |
| Tests | `test_fs_hardening.py` | — | 27 filesystem hardening tests |
| C++ accel | `mp64_accel.cpp` | 3,229 | Hot-path accelerator (NEXT/ALU/mem/STXI) |
| RTL | `rtl/` | ~25,000 | 30 portable Verilog modules + 12 target overrides |
| RTL tests | `rtl/sim/` | ~11,100 | 28 testbenches (~414 hardware assertions) |
