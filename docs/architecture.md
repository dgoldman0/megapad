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
│  32 × 64-bit GPRs     │  4 private tile  │  │ Perf Ctrs │ │
│  4 KiB I-Cache        │ engines (MEX)    │  │ (4 × 64b) │ │
│  8-bit Flags          │ ACC + 2,048-bit  │  └───────────┘ │
│  Perf counters        │ TACC per engine  │                │
│                       └──────────────────┘                │
│  + 3 micro-clusters (4 scalar μ-cores ea., shared MUL/DIV │
│    + one shared tile/ACC/TACC engine, scratchpad, barrier) │
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
    │   0x0780     │  SHA-3/SHAKE/Keccak  │ │
    │   0x07E0     │  Reserved            │ │
    │   0x0800     │  TRNG                │ │
    │   0x0880     │  Port I/O Bridge     │ │
    │   0x08A0     │  WOTS Chain          │ │
    │   0x08C0     │  NTT Engine          │ │
    │   0x0900     │  KEM (ML-KEM-512)    │ │
    │   0x0A00     │  Framebuffer         │ │
    │   0x0B00     │  RTC / System Clock  │ │
    │   0x0C00     │  PCM Audio Output    │ │
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
| `0x0000_0000` | **Bank 0** — generated BIOS image (size is build-specific) |
| `dict_free`+ | Forth dictionary grows upward from HERE |
| *(varies)* | KDOS core code, buffer data, FS caches, task stacks |
| *(varies)* | Free space between HERE and SP |
| ← SP | Data stack grows downward from top of Bank 0 |
| `RAM_SIZE` | Top of Bank 0 (default 0x0010_0000 = 1 MiB) |
| `0x0010_0000`+ | **External Memory** — up to ~4 GiB (userland dictionary + XMEM allocator; emulator default 128 MiB via `--extmem`) |
| `0xFF00_0000`–`0xFF3F_FFFF` | **VRAM** — 4 MiB dedicated framebuffer (double-buffered 1280×720 RGBA) |
| `0xFFD0_0000`–`0xFFFF_FFFF` | **Banks 1–3** — 3 MiB HBW math RAM for tile/SIMD working buffers |

The BIOS sets `HERE` just past its own code.  As `kdos.f` loads through
`FSLOAD`, it compiles the KDOS core and allocates its Bank 0 data, advancing
HERE.  Standard autoexec then redirects `HERE` to the XMEM userland zone
before loading `networking.f` and `tools.f`; those modules do not consume the
Bank 0 dictionary.  The data stack lives at the top of RAM and grows
downward.  The return stack sits below the data stack.

The emulator/CLI explicitly loads the generated BIOS image into Bank 0. The
current integrated-RTL FPGA measurement wrappers do not provision that Bank 0
image; the standalone `mp64_rom` synthesis target therefore measures ROM
contents but is not evidence of a bootable full SoC.

### MMIO Region

All MMIO registers live at base address `0xFFFF_FF00_0000_0000`.  Each
device occupies a small range:

| Device | Offset | Size | Description |
|--------|--------|------|-------------|
| **UART** | `+0x0000` | 16 bytes | Serial I/O (keyboard/terminal) |
| **UART Geometry** | `+0x0010` | 16 bytes | Terminal dimensions, resize status/request |
| **Timer** | `+0x0100` | 16 bytes | 32-bit timer with compare-match |
| **Storage** | `+0x0200` | 32 bytes | Checked sector controller with completion, precise result, media identity, and capacity registers |
| **System Info** | `+0x0300` | 112 bytes | Board ID, topology, memory layout, crypto capabilities, requester count |
| **NIC** | `+0x0400` | 128 bytes | Network interface controller |
| **Mailbox** | `+0x0500` | 16 bytes | Inter-core IPI (data + send + status + ack) |
| **Spinlock** | `+0x0600` | 64 bytes | 16 hardware locks, 4 bytes each; lock 8 is reserved by the checked MMIO crypto guard |
| **AES-256/128-GCM** | `+0x0700` | 64 bytes | Authenticated encryption accelerator (AES-256 and AES-128) |
| **SHA-3/SHAKE/raw Keccak** | `+0x0780` | 96 bytes | Checked hash/XOF streaming plus indexed caller-owned Keccak-f[1600] state |
| **Reserved** | `+0x07E0` | 16 bytes | No integrated QoS MMIO device; accesses fault |
| **TRNG** | `+0x0800` | 32 bytes | Checked hardware entropy source |
| **Port I/O Bridge** | `+0x0880` | 16 bytes | Remap CSR — maps OUT N / INP N to configurable MMIO targets |
| **WOTS Chain** | `+0x08A0` | 32 bytes | Qualified checked byte-only WOTS chain sequencer with 64-bit read-only Bank 0 context DMA |
| **NTT Engine** | `+0x08C0` | 64 bytes | 256-point Number Theoretic Transform (ML-KEM/ML-DSA) |
| **KEM** | `+0x0900` | 64 bytes | ML-KEM-512 key encapsulation accelerator |
| **Framebuffer** | `+0x0A00` | 64 bytes | Tile-based framebuffer controller |
| **RTC / System Clock** | `+0x0B00` | 32 bytes | 64-bit ms uptime + ms epoch + calendar (sec/min/hour/day/mon/year/dow) + alarm IRQ |
| **PCM Audio Output** | `+0x0C00` | 32 bytes | One-shot PCM16 DMA contract; emulator capture/playback implemented, physical DMA/I2S bridge pending |

The crypto register, ownership, and capability assignments are normative in
[`crypto-interface-contract.md`](crypto-interface-contract.md). System Info
extends through `+0x6F`. The qualified checkpoint-3 configuration reports
`CRYPTO_CAPS = 0xF`: bit 0 is reflected/raw CRC, bit 1 is checked SHA3/SHAKE
streaming, bit 2 is raw Keccak-f[1600], and bit 3 is the production WOTS
chain. The checked-in backends completed the real DMA, shared-Keccak,
checked-BIOS, and cross-backend qualification before publishing bit 3; source
presence alone does not advertise the feature. SHA3/SHAKE, raw Keccak, and WOTS share
one physical Keccak round service and the portable lock-8 guard. Checked CRC
continues to use topology-sized BIOS owner records and the cluster's CRC
transaction lock.

Any access outside RAM and the MMIO aperture triggers a **bus fault**
(vector `IVEC_BUS_FAULT`).  In the RTL, the bus arbiter uses 6-/8-bit
watchdogs with terminal counts 63/255 and response deadlines 64/256 clocks;
on timeout it completes the fabric response with sentinel data
(`0xDEAD_DEAD_DEAD_DEAD`) and an error sideband. READY, data, error, and the
faulting address follow the same latched requester through the full-core
data/I-cache mux and the cluster winner demultiplexer. CPU scalar and
instruction paths treat that qualified response as a synchronous,
unmaskable `IRQX_BUS`: sentinel data cannot reach a destination register or
cache line, normal completion side effects do not retire, and `TRAP_ADDR`
records the failed access. A failure while reading an RTI frame, writing a
trap frame, or loading its vector fails closed with interrupts disabled
instead of recursively faulting or accepting a corrupt frame. The sticky
`CSR_BUS_ERR` record remains available for reset-time diagnosis.
In the emulator, unmapped MMIO offsets raise `BusError`, which the SoC
layer converts to `TrapError(IVEC_BUS_FAULT)`.

---

## UART (Serial Port)

The UART provides terminal I/O — it is how the user types at the Forth REPL
and sees output. The table records the current emulator/native host facade used
by BIOS. Offsets `+00` through `+03` have corresponding RTL registers; the
batching offsets `+04` through `+0F` do not currently exist in the integrated
RTL and must not be treated as a common hardware contract.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| TX_DATA | `+0x00` | W | Write a byte to transmit. |
| RX_DATA | `+0x01` | R | Read next byte from receive FIFO. |
| STATUS | `+0x02` | R | **bit 0:** TX ready. **bit 1:** RX data available. **bit 5:** TX empty. The emulator reports ready and empty immediately; RTL derives bits 0 and 5 from its TX FIFO. |
| CONTROL | `+0x03` | RW | **bit 0:** RX IRQ enable.  **bit 1:** TX IRQ enable. |
| BAUD_LO | `+0x04` | RW | Stored emulator byte only; it does not pace TX or RX and has no RTL register. |
| BAUD_HI | `+0x05` | RW | Stored emulator byte only; it does not pace TX or RX and has no RTL register. |
| TX_FLUSH | `+0x06` | W | Drain the TX ring buffer (triggers batch output callback). |
| TX_RING_BASE | `+0x08`–`+0x0F` | W | 64-bit LE pointer to the TX ring descriptor in RAM. |

**BIOS words:** `KEY` reads from RX_DATA (blocking), `KEY?` checks STATUS bit
1, and the current `EMIT` appends only to a 4096-byte TX ring in RAM. The ring
is flushed automatically when full or explicitly through `TX-FLUSH`.

### Current physical-TX gap

Physical UART TX status is **OPEN**. The Python and native accelerator devices
implement `TX_FLUSH` and `TX_RING_BASE` by copying the BIOS ring into a host
batch; their TX path is untimed and their stored baud bytes are inert. The
emulator rich-terminal viewer consumes those in-process batches directly, so
successful emulator/native rendering is not evidence of physical-UART byte
delivery, baud timing, or line backpressure.

The integrated `mp64_uart` RTL has only `TX_DATA`, `RX_DATA`, `STATUS`, and
`CONTROL`. Its SoC instance fixes a real 8N1 serializer at 115,200 baud and has
no ring registers or DMA interface. Because BIOS `EMIT` never writes
`TX_DATA`, and both BIOS `TX-FLUSH` and `rich-terminal.f` ultimately write the
unimplemented `+06` register, the current physical RTL TX pin receives none of
that output. RTL `TX_EMPTY` is also only FIFO-empty; it may assert while the
shift register is still transmitting and is not a physical line-idle boundary.

Close this seam at 115,200 before changing rate. The preferred direction for
the unreleased architecture is a BIOS hardware path that polls `TX_READY` and
writes `TX_DATA`, while emulator batching remains a host implementation detail.
Retaining the guest-visible ring instead requires a real bounded DMA reader,
completion behavior, and fault semantics in RTL. Either design must first show
ordinary ANSI and APT bytes on the physical pin.

### Dual-rate direction

After physical TX works, a baseline 115,200 / fast 1,000,000 baud selector is
feasible without a clock change. At the current 100 MHz clock the integer
divisors are 868 (approximately 115,207 baud) and exactly 100. The current two
emulator baud bytes cannot represent either rate numerically, so they must not
be promoted as the control. Prefer an atomic two-profile selector; if arbitrary
rates are required later, use a sufficiently wide shadow divisor plus an
explicit apply operation.

Reset and ANSI negotiation use 115,200. A future APT extension must explicitly
advertise and accept the fast profile while still at 115,200, wait for a true
FIFO-and-shifter-idle boundary, then switch both endpoints. Close returns to
115,200 after its acknowledged idle boundary, and hard link reset always
restores 115,200. The current exact APT-1 offer/accept grammar contains no rate
field, so an unnegotiated divisor write is not compatible dual-rate support.

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
**512 bytes**.  The current MP64FS marker-1 format supports media through
65536 sectors (32 MiB); this filesystem-format ceiling is smaller than the
controller's u32 capacity encoding.

The normative command, completion, failure, ordering, and durability behavior
is defined in [Storage controller and checked block-I/O contract](storage-controller-contract.md).

FPGA builds expose the logical media window through the synthesizable
`DISK_TOTAL_SECTORS` parameter.  The portable top and SoC default it to the
canonical 65536 sectors; board integrations using a different window must
override the parameter so `TOTAL_SECTORS` remains truthful.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **0x01:** READ, **0x02:** WRITE, **0x03:** STATUS, **0x04:** RESET, **0xFF:** FLUSH |
| STATUS | `+0x01` | R/W1C | busy, terminal error, rejected-write, valid-result, media-change, write-protect, and present state |
| SECTOR | `+0x02`–`+0x05` | RW | 32-bit sector number (LE) |
| DMA_ADDR | `+0x06`–`+0x0D` | RW | 64-bit RAM address for DMA (LE) |
| SEC_COUNT | `+0x0E` | RW | Number of sectors to transfer (1–255) |
| DATA | `+0x0F` | RW | Legacy diagnostic byte port; outside the qualified checked path |
| TOTAL_SECTORS | `+0x11`–`+0x14` | R | Attached media sector count (u32 LE; zero when detached) |
| RESULT | `+0x15` | R | Precise terminal cause; bit 7 marks a possibly applied prefix |
| COMPLETE | `+0x16`–`+0x19` | R | Terminal completion generation (u32 LE) |
| MEDIA_GEN | `+0x1A`–`+0x1D` | R | Attachment identity generation (u32 LE) |
| CAPS | `+0x1E` | R | Backend capability bits |
| TRANSFERRED | `+0x1F` | R | Whole sectors completed by the terminal request |
| EXPECTED_MEDIA_GEN | `+0x20`–`+0x23` | RW | Required u32 attachment generation for conditional submission |
| GUARDED_CMD | `+0x24` | W | Atomically compare generation and submit READ/WRITE/FLUSH; reads as zero |

**Typical read sequence:**
1. Write sector number to SECTOR registers
2. Write RAM destination to DMA_ADDR registers
3. Write sector count to SEC_COUNT
4. Snapshot COMPLETE and write `0x01` to CMD (READ)
5. Wait boundedly for COMPLETE to change
6. Accept the data only when RESULT is zero and TRANSFERRED equals SEC_COUNT

Persistent block-device and volume handles use the guarded form: write their
captured generation to `EXPECTED_MEDIA_GEN`, snapshot `COMPLETE`, then write
READ, WRITE, or FLUSH to `GUARDED_CMD`.  A generation mismatch completes as
`MEDIA_REMOVED` with zero transferred sectors and no DMA or media effect.
`CAPS` bit 6 advertises this atomic guard.

**BIOS words:** production code uses `DISK-READ-CHECKED`,
`DISK-WRITE-CHECKED`, and `DISK-FLUSH-CHECKED`.  Raw `DISK-SEC!`,
`DISK-DMA!`, `DISK-N!`, `DISK-READ`, `DISK-WRITE`, and `DISK-FLUSH` remain
diagnostic compatibility words.  `DISK@` reads status, `DISK-SECTORS` reads
attached capacity, and `MP64FS-VALID?` validates the complete attached
filesystem before use.

---

## System Info

Board identification, topology, and capability registers in the exact
half-open range `[+0x00,+0x70)`. All registers are read-only except
CLUSTER_EN.

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
| CRYPTO_CAPS | `+0x60` | 64-bit | `0xF` | Bit 0: reflected/raw CRC; bit 1: checked SHA3/SHAKE; bit 2: raw Keccak-f[1600]; bit 3: production WOTS chain |
| NUM_BUS_PORTS | `+0x68` | 64-bit | varies | Exact weighted-arbiter requester count: full cores + clusters + NIC + disk + WOTS |

Byte reads return the corresponding little-endian byte. Halfword, word, and
qword accesses must be naturally aligned and wholly contained in the device
window; invalid spans fault before any prefix of a write is published. Writes
to CRYPTO_CAPS and NUM_BUS_PORTS are acknowledged and ignored. The requester
count includes three appended DMA requesters—NIC, disk, and WOTS—after the
full-core and microcluster ports. WOTS is appended after disk, so the existing
NIC and disk physical indices do not move. See
[`crypto-interface-contract.md`](crypto-interface-contract.md#capability-discovery)
for the independent capability-bit assignments.

---

## NIC (Network Interface Controller)

An Ethernet-style network controller with a 1514-byte maximum frame size
(14-byte Ethernet header plus a 1500-byte L3 MTU, without FCS).  Oversized
frames are rejected rather than truncated.  Frame transfer uses DMA; a
separate address-indexed diagnostic window is available for register-path
checks.  Default MAC address:
`02:4D:50:36:34:00`.

Host-side injection accepts complete, non-empty frames only.  Empty or
oversized injections are rejected and latch STATUS.error; later commands do
not clear that error.  CMD RESET is the sole error-clear operation.

The DMA frame excludes Ethernet FCS.  Host TAP networking and an FPGA's
external PHY/MAC adapter are responsible for generating and validating that
wire-level CRC; it is distinct from the IPv4 and UDP ones-complement checksums.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| CMD | `+0x00` | W | **0x01:** SEND, **0x02:** RECV, **0x03:** STATUS, **0x04:** RESET |
| STATUS | `+0x01` | R | **bit 0:** TX busy, **bit 1:** RX available, **bit 2:** link up, **bit 3:** error (sticky until RESET), **bit 4:** RX DMA busy, **bit 7:** present |
| DMA_ADDR | `+0x02`–`+0x09` | RW | 64-bit DMA address (LE) |
| FRAME_LEN | `+0x0A`–`+0x0B` | RW | 16-bit frame length (LE) |
| IRQ_CTRL | `+0x0C` | RW | **bit 0:** RX IRQ enable, **bit 1:** TX IRQ enable; masks the external IRQ line only |
| IRQ_STATUS | `+0x0D` | RW | **bit 0:** RX event pending, **bit 1:** TX event pending (W1C); events latch even while masked |
| MAC_ADDR | `+0x0E`–`+0x13` | R | 6-byte MAC address |
| TX_COUNT | `+0x14`–`+0x15` | R | Frames sent (16-bit LE) |
| RX_COUNT | `+0x16`–`+0x17` | R | Frames received (16-bit LE) |
| DATA | `+0x20`–`+0x7F` | RW | 96-byte address-indexed diagnostic window; unwritten/reset bytes read as zero |

**BIOS words:** `NET-STATUS`, `NET-SEND`, `NET-RECV`, `NET-MAC@`.
`NET-RECV` waits for STATUS bit 4 to clear before it reads `FRAME_LEN` or
returns access to the destination buffer.  Native devices complete the same
transaction synchronously, so software observes one completion contract.
RECV with no available frame publishes length zero; a duplicate RECV while
RX DMA is active is ignored.  SEND while TX is busy leaves the active transfer
unchanged and latches STATUS.error.

DATA offsets are independent registers, not aliases for a FIFO: repeated reads
of one offset return the same byte and do not advance a hidden cursor.  The
window is not a substitute frame path; `NET-SEND` and `NET-RECV` use DMA.

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
- **Full-width TACC** — one explicit 2,048-bit persistent lane accumulator per
  physical engine, with widened integer and binary32 feedback accumulation

The extended tile operations and TACC contract are implemented in the Python
oracle, native accelerator, strict-cycle model, and portable RTL.  The RTL
contains all four private full-core engines and all three cluster-shared
engines, with one TACC bank per engine.  Focused and composed simulation
cover the locked arithmetic, ownership, image, fault, reset, and counter
behavior.  Routed FPGA resource and timing acceptance is a separate,
unfinished physical-validation step and must not be inferred from functional
RTL simulation.

### Tile Engine Domains and Explicit TACC Control

The architectural topology is seven physical engines:

1. four private engines, one paired with each full core; and
2. three shared engines, one round-robin engine in each four-microcore
   cluster.

Every engine owns one legacy 256-bit ACC and one independent 2,048-bit TACC.
A full core owns all of its engine-facing context privately.  Each microcore
instead has private cursor, `TMODE`, `TCTRL`, source, destination, and stride
shadows, while ACC, TACC, and TACC lifecycle metadata are shared by the
cluster's physical engine.  The selected caller's shadows are sampled with its
granted MEX request.

TACC follows the chip's software-visible control ethos.  Software explicitly
claims, clears or loads, accumulates, stores, and releases it.  Hardware never
infers a lifetime, spills or evicts an owner, or blocks inside a claim.
`TACC.TRY` retires after one attempt; the caller reads caller-relative
`TACC_STATUS.MINE` and chooses retry, `PAUSE`, backoff, or abandonment.
Ownership reserves only the persistent bank, so nonowners retain stateless and
legacy-ACC MEX service.

Cluster admission, the chip-wide TACC image stage, and the seven-source
tile-memory port use deterministic equal round-robin service in this work.
Existing hard QoS remains visible and future software-programmable weights may
change service order, but no weight may change ownership, arithmetic, image,
fault, or retirement semantics.

The canonical TACC image is 256 bytes aligned to 64 bytes and transfers as
four 64-byte beats.  Only one image transfer owns the chip-wide staging image
at a time, although ordinary tile traffic may interleave between its beats.
External images further serialize into 32 PHY words.  With default one-cycle
responses, emulator Phase 1 measures 34 cycles through instruction-step
execution and 37 cycles through the registered strict-system path; internal
images measure 6 and 9 respectively.

Interrupts and traps preserve TACC.  Software saves dirty state and its format
before migrating an owner, then releases; same-core resumption may retain it.
The architectural reset contract wipes all seven domains on whole-SoC reset,
only the paired engine on full-core reset, and only the shared engine on
cluster disable/reset.  Individual microcore reset cancels only that caller's
work.  RTL verifies these scopes through named reset seams; the independent
seams remain tied inactive until a production reset controller is specified.
Supervisor `FORCE_RELEASE` is the explicit dead-owner recovery mechanism and
zeroizes the bank before another TACC operation is admitted.

### Crypto Accelerators

| Block | Performance | Use Case |
|-------|-------------|----------|
| AES-256/128-GCM | 16 bytes / 12 cycles | Authenticated encryption for storage and network |
| SHA-3/SHAKE/raw Keccak | One bounded 24-round shared Keccak service | Checked SHA3-256/512, SHAKE128/256, and raw Keccak-f[1600] |
| WOTS Chain | 0–15 shared Keccak permutations after one 64-byte context read | Production Winternitz chain primitive over a caller-owned Bank 0 context |
| SHA-256 | 64 bytes / 64 cycles | TLS 1.3, HMAC-SHA256, HKDF (per-core ISA, no MMIO) |
| CRC (32/64-bit tuples) | 8 bytes / feed | Data integrity (private full-core / cluster-shared ISA, no MMIO) |
| Field ALU | 1 FMUL / ~255 cycles | GF(2²⁵⁵−19) field arithmetic (8 modes incl. X25519, per-core ISA) |
| NTT Engine | 256-pt NTT / ~1280 cycles | Lattice crypto polynomial multiply (ML-KEM, ML-DSA) |
| KEM | keygen+encaps / ~500 cycles | ML-KEM-512 key encapsulation (FIPS 203) |
| TRNG | 64 bits / 2 cycles | Hardware true random number generator |

### SHA-3/SHAKE and raw Keccak-f[1600]

The shared Keccak front end occupies the exact half-open range
`[+0x0780,+0x07E0)`. Offsets in this table are relative to `+0x0780`:

| Offset | Register | Access | Contract |
|--------|----------|--------|----------|
| `+0x00` | CMD | byte write | Complete command byte; reads return zero |
| `+0x01` | STATUS | byte read | Phase in bits 1:0, owner class in bits 3:2; bits 7:4 are zero |
| `+0x02` | CTRL | byte read/write | Complete hash mode 0..3 |
| `+0x03` | ERROR | byte read | Stable device error code |
| `+0x08` | DIN | byte write | One streaming input byte |
| `+0x10..+0x4F` | DOUT | byte or aligned qword read | One stable 64-byte output window |
| `+0x50` | STATE_INDEX | byte read/write | Raw lane index 0..24; no auto-increment |
| `+0x58..+0x5F` | STATE_DATA | byte or aligned qword read/write | Selected little-endian 64-bit raw lane |

`+0x04..+0x07` and `+0x51..+0x57` are reserved. Halfword and word accesses,
misaligned qwords, wrong-direction accesses, and operations crossing a
register or the 96-byte aperture fault as one architectural access before
mutation. `STATUS`, `ERROR`, and `CTRL` remain responsive while the round
service is busy.

| CTRL | Construction | Rate | Output |
|------|--------------|------|--------|
| `0` | SHA3-256 | 136 bytes | fixed 32 bytes |
| `1` | SHA3-512 | 72 bytes | fixed 64 bytes |
| `2` | SHAKE128 | 168 bytes | extendable |
| `3` | SHAKE256 | 136 bytes | extendable |

The complete command values are `1` INIT, `3` FINAL, `4` NEXT, `6`
KECCAK_F1600, and `7` CLEAR. Values `0`, `2`, `5`, and `8..255` are
reserved; there is no command-5 32-byte sliding window. A full-rate `DIN`
write starts automatic permutation with bounded MMIO backpressure. `FINAL`
uses SHA3 delimiter `0x06` or SHAKE delimiter `0x1F`; `NEXT` advances one
sequential 64-byte SHAKE window. `CLEAR` aborts or acknowledges, wipes all
state and visible output, and releases the MMIO owner.

One accepted permutation/window operation leaves BUSY within 32 core clocks;
CLEAR reaches idle within 64. BIOS permits 64 acknowledged status reads for
a normal command and 128 for clear, with a terminal value on the last poll
winning. Timeout maps to checked status 5; a clear timeout retains lock 8 and
the software owner fields.

The packed phase values are 0 IDLE, 1 BUSY, 2 DONE, and 3 ERROR. Owner values
are 0 none, 1 MMIO sponge, 2 MMIO raw, and 3 WOTS. Thus the normal sponge
states are `0x04..0x07`, raw states are `0x08..0x0B`, and WOTS busy is
`0x0D`. While WOTS owns the round service, SHA `STATUS`, `ERROR`, and `CTRL`
reads remain responsive, but mutating SHA accesses are suppressed or rejected
without disturbing WOTS. Device error codes are 1 invalid command, 2
owner/phase conflict, 3 invalid mode, 4 invalid state index, 5 internal
round-service failure/timeout, and 6 unavailable feature.

The hardware window is always 64 bytes, but the public BIOS
`SHAKE-READ ( dst len -- status )` accepts only 0..32 bytes per call and
tracks a logical cursor across windows. Fixed `SHA3-FINAL` stages and
publishes exactly 32 bytes for SHA3-256 or 64 bytes for SHA3-512. The checked
BIOS status namespace is `0` OK, `1` UNSUPPORTED, `2` STATE/OWNER, `3`
RANGE, `4` PROTECTED, `5` TIMEOUT, and `6` HARDWARE/PROTOCOL. The public
surface is `SHA3-BEGIN`, `SHA3-UPDATE`, `SHA3-FINAL`, `SHAKE-FINAL`,
`SHAKE-READ`, `SHA3-CLEAR`, and `KECCAK-F1600`; `SHA3-STATUS@` and
`SHA3-MODE@` are diagnostic reads only.

Raw state consists of 25 little-endian 64-bit lanes with lane index
`x + 5*y`:

```text
memory[8 * (x + 5*y) + b] = state[x + 5*y][8*b +: 8]
```

`KECCAK-F1600 ( state-200 -- status )` qualifies the complete in-place
caller span, loads all lanes, performs exactly 24 rounds, stages all lanes,
clears the device, and then publishes the 200-byte result. It does not absorb,
pad, separate domains, squeeze, or reverse bytes. Failure leaves the caller
image unchanged.

### WOTS chain sequencer

The checkpoint-3 WOTS accelerator occupies the exact half-open byte range
`[+0x08A0,+0x08C0)`. It consumes one immutable 64-byte Bank 0 context:
16 bytes of `PK.seed`, 32 bytes of ADRS, and a 16-byte input node. Hardware
holds one 64-bit context address and never writes caller memory.

| Offset | Register | Access | Contract |
|--------|----------|--------|----------|
| `+0x00..+0x07` | CONTEXT_ADDR | byte read/write | Little-endian 64-bit physical address |
| `+0x08` | STEPS | byte read/write | Complete value 0..15 |
| `+0x09` | START | byte read/write | Complete value 0..15; nonzero work requires `START + STEPS <= 15` |
| `+0x0A` | CMD / STATUS | byte write/read | Commands 0 NOP, 1 GO, 2 CLEAR; status 0 IDLE, 1 BUSY, 2 DONE, 3 ERROR |
| `+0x0B` | ERROR | byte read | Stable terminal error code |
| `+0x0C..+0x0F` | CYCLES | byte read | Saturating little-endian 32-bit service count retained across CLEAR |
| `+0x10..+0x1F` | DOUT | byte read | Stable 16-byte terminal result |

Every WOTS register is byte-only. Wider, misaligned, crossing, reserved, or
wrong-direction accesses fault atomically before mutation. Programming bytes
change only in IDLE; DONE and ERROR remain stable until CLEAR. GO validates
steps, widened start/step geometry, the complete nonwrapping Bank 0 context
span, and—for nonzero work—shared Keccak ownership, in that order. Error
codes are 1 invalid command, 2 owner unavailable, 3 steps, 4 geometry, 5
context span/domain, 6 DMA target fault, 7 memory-response timeout, 8 local
request-accept timeout, and 9 internal protocol failure.

Each successful request performs exactly 64 ascending byte reads through a
real read-only main-bus requester. The requester is fixed at weight 1 with no
bandwidth cap, permits one accepted outstanding beat, and receives an
explicit `OK`, target-fault, memory-timeout, or protocol response. CLEAR may
withdraw an unaccepted beat; after acceptance it drains the terminal response
before returning IDLE. Zero steps still performs all 64 reads and returns the
input node unchanged without claiming Keccak.

For each nonzero step, the controller builds the selected SHAKE256 rate block,
overwrites ADRS bytes 28..31 with `START + step` in big-endian form, performs
one raw 24-round permutation on the sole shared Keccak service, and takes the
next 16-byte node. Success, failure, CLEAR, and reset scrub private context and
Keccak state before release or terminal publication.

The public checked BIOS boundary is
`WOTS-CHAIN ( context-64 start steps dst-16 -- status )`. It checks capability
and complete spans first, derives its bounded request and clear deadlines from
the read-only `NUM_BUS_PORTS`, uses `CSR_PERF_CYCLES` with exact save/enable/
restore semantics, stages all 16 output bytes, clears the device, and only
then publishes. Failure leaves the destination unchanged. A clear timeout
returns TIMEOUT and retains lock 8 and software ownership fail-closed until
machine reset. The complete state machine, deadlines, state construction, and
status mapping are normative in the
[`crypto-interface-contract.md`](crypto-interface-contract.md#wots-chain-contract).

Checkpoint 3 completed this hardware/BIOS primitive after the documented
qualification gate enabled capability bit 3; it is not the Akashic cutover.
Checkpoint 4 has replaced KDOS's private GPT IEEE loop with checked reflected
mode-4 hardware. Header buffers are one transaction; entry arrays carry raw
state through short resident-sector transactions and release before each next
disk read. Standard-vector diagnostics cover modes 0/1/2/4/5/6 and mode-5
raw finalization. Fresh final artifacts reproduced byte-for-byte, the ordered
focused matrix and full serial RTL sweep passed, and the approved Python
regression completed with 3,425 passed and three conditional live-network
skips. MegaPad checkpoint 4 is complete; a user-selected Akashic worktree may
adopt the checked CRC, raw-Keccak, and WOTS interfaces in a separately
authorized task.

### Portable MMIO crypto guard

The spinlock bank is an exact 64-byte aperture containing 16 locks. Lock 8 is
the crypto guard: acquire is `SPINLOCK_BASE + 0x20` and release is
`SPINLOCK_BASE + 0x21`. The checked BIOS acquires it internally and records
full-width `CRYPTO-OWNER-CORE` and `CRYPTO-OWNER-TASK` values. Owner
publication and removal occur in saved-interrupt-state critical sections, so
same-core task re-entry returns STATE/OWNER without releasing an outer
transaction. A failed hardware quiescence retains the owner fields and lock
fail-closed.

The main bus carries requester-valid plus the architectural global core ID
with the winning request and holds both stable through the response. A
full-core port supplies its full-core ID. A cluster port supplies
`CLUSTER_ID_BASE +` the latched winning microcore index, not the shared bus
port number. Cluster-internal SHA traffic and DMA requesters are invalid.
Invalid or out-of-range requesters receive acknowledged non-mutating spinlock
responses, with acquire reported busy. Spinlock owner storage covers the
complete `NUM_CORES` global topology independently of mailbox full-core
capacity.

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

### SHA-2 (Per-Core / Micro-Cluster ISA)

SHA-256/384/512 hashing is implemented with EXT.CRYPTO (`FB`) instructions,
not MMIO. Full cores have independent state; each micro-cluster arbitrates
one shared engine.

**Instructions:** `sha.init`, `sha.round`, `sha.pad`, `sha.din`,
`sha.dout`, `sha.final`, `sha.release`
(see `docs/isa-reference.md` § EXT.CRYPTO for full encoding).

The engine implements full FIPS-180-4 SHA-256 compression with K
constants, Σ/σ/Ch/Maj functions, 16-entry W message schedule with
on-the-fly expansion, and automatic padding. A TSRC0-addressed buffer is
64 bytes in SHA-256 mode and 128 bytes in SHA-384/512 mode. On a
micro-cluster, `sha.final` retains ownership through digest extraction and
scrubbing; bare `sha.release` is the sole handoff.

**BIOS words:** `SHA256-INIT`, `SHA256-UPDATE`, `SHA256-FINAL`,
`SHA256-CLEAR`; and `SHA512-INIT`, `SHA512-UPDATE`, `SHA512-FINAL`,
`SHA512-CLEAR`. `SHA2-SPAN-STATUS` provides a pure pre-`INIT` physical-span
and shared context-arena check. Both streaming layers use private per-core
contexts, validate complete caller spans against the union of both SHA-2 arenas,
stage output until after engine release, wipe on every terminal path, and
preserve the caller's ACC/TSRC0/interrupt transaction. SHA-256 reports
checked state, span, alias, and 64-bit length failures; SHA-512 additionally
maintains its full 128-bit length and validates its exact active marker,
bounded partial offset, byte alignment, and low-length/offset agreement.
**KDOS words:** `SHA256`, `SHA512`, `HMAC-SHA256`,
`HKDF-SHA256-EXTRACT`, `HKDF-SHA256-EXPAND`.
Those SHA-256-family KDOS words return the BIOS status unchanged so
networking and TLS key-schedule callers can fail closed.

### Checked Caller-Managed Spans

`CALLER-SPAN-STATUS ( addr len -- status )` is the protocol-neutral BIOS
boundary for qualifying a complete caller-managed byte span before a higher
layer reads or writes it. It returns `0` OK, `2` RANGE, or `3` PROTECTED and
does not inspect or mutate any named byte.

Zero length is unconditional OK and ignores the unused address. A nonempty
span must use nonnegative address and length cells, must not be null or wrap,
and must fit wholly in one Bank 0, external, HBW, or VRAM window advertised by
SysInfo. Bank 0 is further restricted to `[dict_free, caller-DSP-8)`, which
excludes the static BIOS/private footprint, live stacks, and the result cell.

The same conservative boundary is appropriate for both reads and writes
because it describes ordinary memory that may be caller-managed, rather than
all physically readable bytes. It intentionally rejects even readable static
BIOS storage. Passing the boundary proves geometry and platform protection
only: it does not prove allocation ownership, mutability, initialization,
lifetime, or freedom from aliases in another caller's allocation.

### TRNG (True Random Number Generator)

The TRNG occupies `+0x0800`–`+0x081F` and is shared by all cores. The
physical implementation is intended to use a conditioned entropy source
with health monitoring. The native emulator uses a staged 64-byte pool
filled from `std::random_device`, but accepts that provider only when it
reports positive entropy. Consumed bytes are erased. A provider exception or
health failure erases the pool and supplemental seed, clears usability, and
remains latched until explicit host reinitialization.

| Register | Offset | R/W | Description |
|----------|--------|-----|-------------|
| RAND8 | `+0x00` | R | One random byte; bus fault while unusable |
| RAND64 | `+0x08`–`+0x0F` | R | Eight independent random-byte lanes; a 64-bit little-endian load reads the complete value |
| STATUS | `+0x10` | R | **bit 0:** USABLE; every other bit is zero |
| SEED | `+0x18`–`+0x1F` | W | Supplement unread/future host-derived bytes; ignored while unusable |

The complete window remains decoded while disabled or unhealthy, so
software can always read a zero `STATUS` without falling through to another
device model. `RAND8` and `RAND64` fail closed with `IVEC_BUS_FAULT` when
`USABLE` is clear. `SEED` never substitutes for a healthy entropy source and
cannot recover a latched failure.

**BIOS words:** `RANDOM`, `RANDOM8`, `SEED-RNG`, `ENTROPY-FILL`,
`ENTROPY-READY?`. `ENTROPY-FILL` delegates destination qualification to the
shared `CALLER-SPAN-STATUS` policy. The first two random-read words are raw
and therefore propagate the device bus fault if entropy is unavailable;
`SEED-RNG` is a supplemental mix only.

`ENTROPY-FILL ( addr len -- status )` is the checked bulk boundary. It
returns `0` OK, `1` UNAVAILABLE, `2` RANGE, or `3` PROTECTED and retains no
state across calls. The complete nonnegative, nonwrapping destination is
qualified before the first read against Bank 0, external, HBW, and VRAM
geometry. Empty calls, including `(0,0)`, are no-ops and ignore their unused
address; nonempty addresses must be nonnegative and nonempty null is RANGE.
Bank 0 is additionally restricted to `[dict_free, caller-DSP-8)`, protecting
the entire static BIOS/private footprint, live stacks, and the returned-status
cell. This geometric protection boundary does not prove allocation ownership;
the caller must still provide a buffer it manages.

`ENTROPY-READY? ( -- flag )` hides the device register address and returns
canonical true only for exact `STATUS == 1`. Unavailable and reserved status
encodings both fail closed to false.

The boundary requires exact `USABLE == 1` immediately before every `RAND8`
and after the final byte. Initial unavailability leaves the destination
unchanged; detected post-start unavailability wipes the complete admitted
destination. A transition caused by a successfully delivered final byte is
therefore a failure, not a published result.

The one `RAND8` instruction private to `ENTROPY-FILL` has a PC-scoped
bus-fault recovery point. If usability changes in the narrow interval between
a successful status read and that data read, the handler resumes the checked
word with UNAVAILABLE so its ordinary complete-span wipe policy still runs.
No mutable recovery flag is shared between cores, and every unrelated bus
fault remains on the diagnostic path.

### Per-Core Infrastructure

| Feature | CSR Range | Description |
|---------|-----------|-------------|
| Tile DMA | 0x50–0x55 | Descriptor-ring DMA engine for async tile copies |
| QoS | 0x58–0x59 | Per-core CSR storage; not currently routed to the main-arbiter QoS sideband |
| BIST | 0x60–0x63 | Memory self-test (March C−, checkerboard, addr-as-data) |
| Tile self-test | 0x64–0x65 | Datapath functional check (~200 cycles) |
| Perf counters | 0x68–0x6C | Cycles, stalls, tile ops, ext-mem beats |
| I-Cache | 0x70–0x72 | Instruction cache control, hit/miss counters |

### Micro-Core Architecture

Each micro-cluster contains 4 scalar micro-cores sharing a MUL/DIV unit,
a tile/MEX engine (round-robin arbitrated, +3 cycle overhead), 1 KiB
scratchpad, and a hardware barrier.  Micro-cores run the same 64-bit
native ISA as full cores **minus** the CDP1802-heritage features:

Each microcore retains private shadows for `SB`, `SR`, `SC`, `SW`, `TMODE`,
`TCTRL`, `TSRC0`, `TSRC1`, `TDST`, `TSTRIDE_R`, `TSTRIDE_C`, `TTILE_H`, and
`TTILE_W`.  Legacy ACC and TACC are not shadowed: they are the shared
cluster-engine state, with caller-relative ownership reporting and
deterministic common admission.

| Stripped Feature | Families / Sub-ops | Rationale |
|------------------|--------------------|-----------|
| D accumulator, Q flip-flop, T register | State | Saves ~17 FFs per core |
| MEMALU (LDX, OR.X, ADD.X, …) | Family 0x8 | All operate on D + M(R(X)) |
| Port I/O (OUT/INP) | Family 0x9 | 1802-style 7-port I/O |
| GLO / GHI / PLO / PHI | Family 0x6 sub 0xC–0xF | D ↔ GPR byte transfer |
| RET / DIS / MARK / SAV / SEQ / REQ | Family 0x0 sub 0x5–0xA | 1802 SCRT + Q |

All stripped opcodes trap as `ILLEGAL_OP` (interrupt vector 0x02).
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
│  networking.f  (userland dictionary in XMEM)    │
│  Ethernet · IPv4 · UDP/TCP · TLS · Sockets      │
│  UDP-backed data-port transport                  │
├─────────────────────────────────────────────────┤
│  KDOS core  (kdos.f, Bank 0)                    │
│  ┌───────────┬───────────┬────────────────────┐ │
│  │  Buffers  │  Kernels  │   Pipelines        │ │
│  │  (§2–§3)  │  (§4–§5)  │   (§6)             │ │
│  ├───────────┼───────────┼────────────────────┤ │
│  │  Storage  │ MP64FS    │  Doc Browser       │ │
│  │  (§7)     │ (§7.6)    │  (§7.7)            │ │
│  ├───────────┼───────────┼────────────────────┤ │
│  │ Scheduler │ Screens   │  Data Port Core    │ │
│  │  (§8)     │ (§9)      │  (§10)             │ │
│  ├───────────┴───────────┴────────────────────┤ │
│  │ Dashboard, Help, Startup, Bundles (§12–§15) │ │
│  └────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────┤
│  BIOS  (bios.asm)                               │
│  Subroutine-threaded Forth, 481 dictionary words │
│  Disk I/O, FSLOAD, UART, timer, tile engine      │
├─────────────────────────────────────────────────┤
│  Megapad-64 Hardware                            │
│  4× CPU, RAM+BIST, UART, Timer, Storage, NIC,  │
│  Tile Engine+FP16, AES, SHA-3, SHA-256, WOTS,    │
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

Nine dictionary words: **PAUSE**, **TASK-YIELD**, **BACKGROUND**,
**TASK-STOP**, **TASK?**, **BACKGROUND2**, **BACKGROUND3**,
**#TASKS**, and **TASK-ID**.  `TASK-ID` exposes the executing cooperative
slot rather than the persistent round-robin cursor, allowing higher layers to
key coroutine-local state such as exception chains.

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
4. **If disk present:** BIOS validates sector 0 against the attached media,
   reads the derived MP64FS directory, and scans for
   the first file with type=3 (Forth)
   - Reads its data sectors into a RAM buffer
   - EVALUATEs each line via FSLOAD
5. **KDOS core loads** — `FSLOAD` reads and evaluates `kdos.f`, compiling
   the core dictionary into Bank 0.  Its startup code prints the banner,
   loads the filesystem (`FS-LOAD`), initializes the Bank 0 heap, and runs
   `autoexec.f` if present
6. **Standard userland loads** — `autoexec.f` enables JIT compilation,
   enters the XMEM userland dictionary, loads `networking.f` through the
   batched KDOS `REQUIRE` path, runs DHCP with a static fallback, loads
   `tools.f`, and disables JIT for interactive use.  `graphics.f` remains
   available for explicit loading
7. **REPL ready** — the outer interpreter (`QUIT`) awaits user input

**If no disk:** BIOS skips step 4, drops directly into the bare Forth
REPL.  The KDOS core can still be loaded via `--forth kdos.f` on the CLI
(UART injection), but without filesystem access, autoexec, or the networking
module.

### Memory Usage (Typical)

After a full KDOS boot with filesystem loaded:

| Region | Approximate Size | Contents |
|--------|-----------------|----------|
| Generated BIOS image | Build-dependent | Machine code, static dictionary, IVT, boot logic, and private storage |
| KDOS core dictionary | Build-dependent | Bank 0 definitions and strings from `kdos.f` |
| Userland dictionary | Build-dependent | `networking.f`, `tools.f`, and later user definitions in XMEM |
| Buffers | ~10 KB | 6 demo buffers, histogram bins |
| FS cache | ~7.5 KB | Superblock (512B) + bitmap (up to 1024B) + directory (6144B) |
| Task stacks | 2 KB | 8 × 256 bytes |
| Frame buffer | 1.5 KB | NIC frame receive buffer |
| Bank 0 headroom | Build-dependent | Core dictionary, heap, and stacks share the 1 MiB bank |

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
- Widened lane products may persist in the separate **2,048-bit TACC**
- Full cores have private engines; each microcluster shares one engine while
  retaining caller-private configuration shadows
- TACC lifetime and waiting policy are explicit software decisions

In KDOS, tile operations power the buffer subsystem (B.SUM, B.MIN, B.MAX,
B.ADD, B.SUB) and several kernels (kadd, ksum, kstats, knorm, kcorrelate).
See `docs/tile-engine.md` for a complete programming guide and
`docs/extended-tpu-spec.md` for the full enhanced tile engine, crypto,
DMA, and reliability specifications.

---

## File Summary

| Component | File | Lines | Role |
|-----------|------|-------|------|
| CPU emulator | `megapad64.py` | — | Full ISA + extended tile engine implementation |
| System glue | `system.py` | — | Heterogeneous SoC, MMIO, mailbox IPI, spinlocks, shared native execution state |
| Devices | `devices.py` | — | MMIO device/reference/proxy implementations, including checked WOTS and the Port I/O Bridge |
| BIOS | `bios.asm` | — | Forth interpreter, boot, multicore, 481 dictionary words |
| OS core | `kdos.f` | — | Bank 0 buffers, kernels, TUI, FS, crypto, module loading, PQC, multicore |
| Networking | `networking.f` | — | Userland Ethernet through TLS, sockets, and UDP data-port transport |
| Tools | `tools.f` | — | ED line editor, SCROLL web client (HTTP/HTTPS/FTP/Gopher) |
| Assembler | `asm.py` | — | Two-pass macro assembler |
| CLI/Monitor | `cli.py` | — | Debug, inspect, boot, headless TCP server |
| Disk tools | `diskutil.py` | — | Build/manage disk images |
| Tests | `tests/test_megapad64.py` | — | CPU + tile engine coverage |
| Tests | `tests/test_system.py` | — | System integration coverage |
| Tests | `tests/test_networking.py` | — | Real-network coverage |
| Tests | `tests/test_fs_hardening.py` | — | Filesystem hardening coverage |
| C++ accel | `accel/` | — | Multi-source native execution and system-state accelerator |
| RTL | `rtl/` | — | Portable Verilog modules and target overrides |
| RTL tests | `rtl/sim/` | — | Verilog testbenches |
