# Megapad-64 System Emulator

A complete system-level emulator for the Megapad-64 architecture: CPU,
memory-mapped I/O peripherals, a two-pass assembler, a Forth REPL BIOS,
and an interactive CLI monitor/debugger.

> **Branch:** `main`
> **Status:** Fully functional.  BIOS v1.0 source with a 481-word Forth
> dictionary targeting a 16-core heterogeneous SoC (4 full cores + 3×4
> micro-clusters) with
> seven physical tile engines, seven 2,048-bit full-width TACCs,
> 3 MiB HBW math RAM, mailbox IPI, spinlocks, extended tile execution
> (saturating, FP16/BF16, strided/2D, CRC, BIST), crypto accelerators
> (AES-256-GCM, SHA-3/SHAKE, TRNG, Field ALU, NTT, ML-KEM-512) plus a
> qualified checked WOTS chain with real Bank 0 DMA and shared
> Keccak, optional
> C++ CPU accelerator (63× speedup), pluggable NIC backends (loopback,
> UDP, TAP), full TCP/IP network stack through TLS 1.3, cooperative
> multitasking (4-task PAUSE/BACKGROUND/BACKGROUND2/BACKGROUND3), and
> broad Python and RTL qualification coverage.

---

## Quick Start

```bash
# Boot the Forth REPL directly (assembles .asm on the fly)
python cli.py --bios bios.asm

# Pre-compile to a .rom, then boot from binary
python cli.py --assemble bios.asm bios.rom
python cli.py --bios bios.rom

# With the C++ accelerator (recommended — 63× faster)
make accel          # one-time: build pybind11 extension
python cli.py --bios bios.asm --storage sample.img

# ~5× faster under PyPy (run 'make setup-pypy' once to install)
.pypy/bin/pypy3 cli.py --bios bios.asm --storage sample.img
```

When stdin is a terminal you get an interactive serial console — type Forth
expressions at the `> ` prompt:

```
Megapad-64 Forth BIOS v1.0
RAM: 00100000 bytes
 ok
> 3 4 + .
7  ok
> HEX CAFE . DECIMAL
CAFE  ok
> 0x2000 16 0xAB FILL  0x2000 16 DUMP
00002000: AB AB AB AB AB AB AB AB AB AB AB AB AB AB AB AB
 ok
> WORDS
CYCLES TZERO TTRANS TMAX TMIN TSUM TDOT TMUL TXOR TOR TAND TSUB TADD
TCTRL! TMODE! TDST! TSRC1! TSRC0! TFILL TVIEW TI FILL DUMP BYE WORDS
BASE DECIMAL HEX .S U. . CR KEY EMIT C, , ALLOT HERE C! C@ ! @ 0< 0=
> < = RSHIFT LSHIFT INVERT XOR OR AND 1- 1+ ABS NEGATE /MOD MOD / * -
+ PICK DEPTH 2DROP 2DUP TUCK NIP ROT OVER SWAP DROP DUP
 ok
> BYE
Bye!
```

When stdin is a **pipe**, the CLI feeds input one byte at a time and prints
all UART output to stdout, then exits on halt or EOF — ideal for scripting
and tests:

```bash
printf '6 7 * .\nBYE\n' | python cli.py --bios bios.rom
```

### CLI flags

| Flag | Default | Description |
|---|---|---|
| `--bios FILE` | — | Boot from `.asm` (assembled on the fly) or binary |
| `--assemble SRC OUT` | — | Assemble `SRC.asm` → `OUT.rom` and exit |
| `--ram KiB` | 1024 | RAM size in KiB |
| `--storage IMAGE` | — | Attach a block-device image file |
| `--forth FILE` | — | Inject Forth source via UART after BIOS boot |
| `--load FILE[@ADDR]` | — | Load raw binary into RAM (repeatable) |
| `--run` | off | Auto-boot and run immediately |
| `--cores N` | 1 | Number of full CPU cores (1–4) |
| `--clusters N` | 0 | Number of micro-core clusters (0–3, 4 cores each) |

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────┐
│                      cli.py                              │
│       Interactive monitor / debugger / console           │
└──────────────────────┬───────────────────────────────────┘
                       │
┌──────────────────────▼───────────────────────────────────┐
│                emulator/system.py                        │
│       MegapadSystem — 16-core heterogeneous SoC          │
│                                                          │
│  ┌──────────────┐    ┌────────────────────────────┐  │
│  │ megapad64.py │    │ devices.py                 │  │
│  │   CPU core   │    │ ┌──────┐ ┌─────┐ ┌─────────┐ │  │
│  │  32 × 64-bit  │◄──►│ │ UART │ │Timer│ │ Storage │ │  │
│  │  registers    │    │ └──────┘ └─────┘ └─────────┘ │  │
│  │  full ISA     │    │ ┌─────────┐ ┌───────┐        │  │
│  │  private tile │    │ │ SysInfo │ │Mailbox│  NIC   │  │
│  │               │    │ └─────────┘ └───────┘        │  │
│  │  extended ops │    │ ┌──────────┐ ┌─────┐         │  │
│  │  FP16/BF16   │    │ │ Spinlock │ │ CRC │ DevBus  │  │
│  └──────────────┘    │ └──────────┘ └─────┘         │  │
│                      │ ┌─────┐ ┌─────┐ ┌──────┐  │  │
│                      │ │ AES │ │ SHA3│ │ TRNG │  │  │
│                      │ └─────┘ └─────┘ └──────┘  │  │
│                      │ ┌───────────┐ ┌───┐ ┌───┐ │  │
│                      │ │ FieldALU  │ │NTT│ │KEM│ │  │
│                      │ └───────────┘ └───┘ └───┘ │  │
│                      └────────────────────────────┘  │
│                                                          │
│  3 × MicroCluster (4 μ-cores ea., shared MUL/DIV +       │
│    one tile/MEX/TACC engine, 1K scratchpad, HW barrier)   │
│  + 4 private full-core tile/MEX/TACC engines              │
│                                                          │
│          asm.py  — two-pass assembler                      │
└──────────────────────────────────────────────────────────┘

    bios.asm  — Forth BIOS v1.0, 481 words
    bios.rom                 — generated precompiled binary; size is build-specific
```

### Source files

| File | Lines | Role |
|---|---|---|
| `emulator/megapad64.py` | — | CPU core — 32×64-bit GPRs (R0–R31 via REX), all 16 instruction families, flags, CSRs, traps, tile engine, extended ops, FP16/BF16, STXI/STXD.D, micro-core variant (1802-heritage stripped) |
| `emulator/accel/` | — | Multi-source C++ execution kernel (pybind11), including host DBT support |
| `emulator/accel_wrapper.py` | — | Drop-in Python wrapper; `emulator/system.py` tries this first, falls back to `emulator/megapad64.py` |
| `asm.py` | — | Two-pass assembler — full mnemonic set, `ldi64`, `.ascii`, `.asciiz`, `.db`/`.dw`/`.dd`/`.dq`, SKIP |
| `emulator/devices.py` | — | MMIO device/reference/proxy implementations, including checked WOTS and the Port I/O Bridge |
| `nic_backends.py` | — | Pluggable NIC backends — Loopback, UDP tunnel, Linux TAP |
| `emulator/system.py` | — | 16-core heterogeneous SoC — four private full-core tile engines plus three cluster-shared engines, HBW math RAM, mailbox IPI, spinlocks, `run_batch()` C++ fast path |
| `cli.py` | — | CLI monitor with disassembler, breakpoints, console mode, pipe mode, `--assemble` |
| `bios.asm` | — | Forth BIOS v1.0 — subroutine-threaded interpreter, 481 built-in words (incl. multicore, micro-cluster, HBW, crypto, PQC, extended tile/TACC, I-cache, cooperative multitasking) |
| `tests/test_megapad64.py` | — | CPU + tile engine test suite |
| `tests/test_system.py` | — | System integration tests: devices, MMIO, BIOS, KDOS, multicore, micro-cluster, HBW, FS, crypto, PQC, network, extended tile, port I/O bridge, and bus timeout |
| `tests/test_networking.py` | — | Real-networking tests |
| `tests/test_fs_hardening.py` | — | Filesystem hardening tests |
| `setup_accel.py` | — | pybind11 build configuration for C++ extension |
| `bench_accel.py` | — | C++ vs Python speed comparison script |
| `Makefile` | — | Build, sequential test, and accelerator targets |
| `conftest.py` | — | Test fixtures, snapshot caching, live status reporting |
| `rtl/` | — | Portable Verilog modules and target overrides |
| `rtl/sim/` | — | Verilog testbenches |
| **Total** | **~60,000** | |

---

## Memory Map

### RAM Regions

| Address Range | Size | Description |
|---|---|---|
| `0x0000_0000` – `0x000F_FFFF` | 1 MiB | **Bank 0** — System RAM (BIOS + Forth dictionary) |
| `0x0010_0000` – `0xFEFF_FFFF` | up to ~4 GiB | **External Memory** — HyperRAM/SDRAM (userland + XMEM) |
| `0xFF00_0000` – `0xFF3F_FFFF` | 4 MiB | **VRAM** — Dedicated framebuffer (double-buffered 1280×720 RGBA) |
| `0xFFD0_0000` – `0xFFFF_FFFF` | 3 MiB | **Banks 1–3** — HBW math RAM (tile/SIMD working buffers) |

### MMIO Peripherals

All MMIO registers live at base `0xFFFF_FF00_0000_0000`:

| Offset | Size | Peripheral |
|---|---|---|
| `+0x0000` | 16 B | UART (serial console) |
| `+0x0100` | 16 B | Timer |
| `+0x0200` | 16 B | Storage controller |
| `+0x0300` | 112 B | System Info (board ID, topology, VRAM, crypto capabilities, bus ports) |
| `+0x0400` | 128 B | NIC (Network Interface) |
| `+0x0500` | 16 B | Mailbox (inter-core IPI) |
| `+0x0600` | 64 B | Spinlock (hardware mutexes) |
| `+0x0700` | 112 B | AES-256/128-GCM (authenticated encryption; key mode at `+0x073A`) |
| `+0x0780` | 96 B | SHA-3/SHAKE (hashing, key derivation) |
| `+0x07E0` | 16 B | Reserved; no integrated QoS MMIO device (access faults) |
| `+0x0800` | 32 B | TRNG (hardware entropy source) |
| `+0x0840` | 128 B | Free; Field ALU is ISA-native (`EXT.CRYPTO FB 20..2D`) |
| `+0x0880` | 16 B | Port I/O Bridge (remap CSR — maps OUT/INP to MMIO targets) |
| `+0x08A0` | 32 B | Qualified checked byte-only WOTS chain (64-bit read-only Bank 0 context DMA) |
| `+0x08C0` | 64 B | NTT Engine (256-point NTT/INTT) |
| `+0x0900` | 64 B | KEM Engine (ML-KEM-512) |
| `+0x0A00` | 64 B | Framebuffer controller |
| `+0x0B00` | 32 B | RTC / System Clock |
| `+0x0C00` | 32 B | PCM Audio Output (one-shot DMA + deterministic capture) |

> **Crypto ISA (no CRC/SHA-2 MMIO):** CRC and SHA-2 use EXT.CRYPTO (`FB`).
> Full cores keep state privately; each micro-core cluster shares CRC and
> SHA engines behind transaction locks. SHA.FINAL retains ownership and
> SHA.RELEASE performs the sole handoff. See `docs/isa-reference.md`
> § EXT.CRYPTO for the normative encodings and ownership rules.

The system layer intercepts any CPU memory operation (8/16/32/64-bit) that
falls in the MMIO aperture and routes it through the device bus; everything
else hits RAM.  Accesses to unmapped MMIO offsets raise `BusError`, which
the SoC layer converts to `TrapError(IVEC_BUS_FAULT)` — matching the RTL
bus arbiter timeout behaviour.

The native TRNG owns its complete `+0x0800`–`+0x081F` window even when it is
disabled or unhealthy. `STATUS` at `+0x10` exposes only bit 0 (`USABLE`);
reads from `RAND8` at `+0x00` or the eight-byte `RAND64` window at
`+0x08`–`+0x0F` raise a guest bus fault while that bit is clear. The native
model fills a wiped 64-byte pool only from a host `std::random_device` that
reports positive entropy. Source failures are caught, erase current and
pending material, and remain latched until explicit reinitialization.
Guest `SEED` writes at `+0x18`–`+0x1F` supplement unread or future
host-derived bytes; they are ignored while the device is unusable and never
restore it.

### BIOS memory layout (runtime)

```
0x00000  ┌──────────────────────┐
         │  BIOS code           │  ~5650 bytes
         │  dictionary entries  │
         │  strings / IVT / TIB │
         ├──────────────────────┤ ← dict_free
         │ CRC owner records    │  NUM_CORES × 16 bytes
         ├──────────────────────┤ ← kernel-data-end / HERE
         │  user dictionary     │  grows ↑
         │  (HERE advances)     │
         │          ...         │
         ├──────────────────────┤ ← ram_size / 2
         │  data stack (R14)    │  grows ↓
         │          ...         │
         ├──────────────────────┤ ← ram_size
         │  return stack (R15)  │  grows ↓
         └──────────────────────┘
```

---

## Peripherals

### UART (Serial Console)

| Offset | Name | R/W | Description |
|---|---|---|---|
| `+0x00` | TX_DATA | W | Write a byte → host output |
| `+0x01` | RX_DATA | R | Read next byte from input buffer |
| `+0x02` | STATUS | R | bit 0: TX ready (always 1), bit 1: RX data available |
| `+0x03` | CONTROL | RW | bit 0: RX IRQ enable, bit 1: TX IRQ enable |
| `+0x04` | BAUD_LO | RW | Baud rate low (cosmetic) |
| `+0x05` | BAUD_HI | RW | Baud rate high (cosmetic) |
| `+0x06` | TX_FLUSH | W | Drain the TX ring buffer (triggers batch callback) |
| `+0x08`–`+0x0F` | TX_RING_BASE | W | 64-bit LE pointer to the TX ring descriptor in RAM |

> **Hardware note:** The TX ring buffer is an *emulator-side* optimisation —
> it converts per-byte MMIO traps (expensive Python round-trips) into fast
> RAM writes plus a single flush.  On real hardware MMIO stores are single
> bus cycles, so the speedup disappears.  To be useful on FPGA/ASIC the SoC
> would need a DMA engine wired to TX_FLUSH that reads from the ring
> descriptor and feeds the UART TX FIFO.  The buffer layout is already
> DMA-friendly by design.

### Timer

32-bit free-running counter with compare-match support.

| Offset | Name | R/W | Description |
|---|---|---|---|
| `+0x00`–`+0x03` | COUNT | R | 32-bit counter (little-endian) |
| `+0x04`–`+0x07` | COMPARE | RW | Compare-match value |
| `+0x08` | CONTROL | RW | bit 0: enable, bit 1: IRQ enable, bit 2: auto-reload |
| `+0x09` | STATUS | RW | bit 0: match flag (write-1-to-clear) |

### Storage Controller

Sector-based block device backed by a host file.  Sector size is 512 bytes.

| Offset | Name | R/W | Description |
|---|---|---|---|
| `+0x00` | CMD | W | `0x01` READ, `0x02` WRITE, `0x03` STATUS, `0xFF` FLUSH |
| `+0x01` | STATUS | R | bit 0: busy, bit 1: error, bit 7: present |
| `+0x02`–`+0x05` | SECTOR | RW | 32-bit sector number |
| `+0x06`–`+0x0D` | DMA_ADDR | RW | 64-bit DMA address in RAM |
| `+0x0E` | SEC_COUNT | RW | Number of sectors to transfer |
| `+0x0F` | DATA | RW | Byte-at-a-time data port |

### System Info

Board identification and core-topology registers (14 × 64-bit aligned,
112 bytes). All read-only except CLUSTER_EN. Byte reads select the
corresponding little-endian byte; wider reads must be naturally aligned and
remain wholly inside the exact window.

| Offset | Name | Description |
|---|---|---|
| `+0x00` | BOARD_ID_VER | `0x4D503634_00020001` (“MP64” + v2.1) |
| `+0x08` | BANK0_SIZE | Bank 0 system RAM size in bytes |
| `+0x10` | NUM_CORES | Total core count (full + micro) |
| `+0x18` | CLUSTER_EN | Per-cluster enable mask (R/W) |
| `+0x20` | HBW_BASE | HBW math RAM base address |
| `+0x28` | HBW_SIZE | HBW region size in bytes |
| `+0x30` | INT_MEM_TOTAL | Total internal memory (all banks) |
| `+0x38` | EXT_MEM_BASE | External memory base address |
| `+0x40` | EXT_MEM_SIZE | External memory size in bytes |
| `+0x48` | NUM_FULL | Number of full (major) cores |
| `+0x50` | VRAM_BASE | Dedicated VRAM base address |
| `+0x58` | VRAM_SIZE | Dedicated VRAM size in bytes |
| `+0x60` | CRYPTO_CAPS | Independent qualified crypto capability bits; unassigned bits read zero |
| `+0x68` | NUM_BUS_PORTS | Exact requester-port count in the main weighted arbiter |

The checked-in checkpoint-3 configuration reports `CRYPTO_CAPS = 0xF`:
reflected/raw CRC, checked SHA3/SHAKE, raw Keccak-f[1600], and WOTS chain.
Bit 3 was published only after the real DMA, shared-service, BIOS, and
cross-backend qualification passed; source presence alone is not capability
evidence. `NUM_BUS_PORTS` is exactly the full-core ports plus microcluster
ports plus NIC, disk, and WOTS. WOTS is the read-only requester appended after
disk; existing NIC and disk indices remain unchanged.

### WOTS Chain

The WOTS device is the exact byte-only range `+0x08A0..+0x08BF`:

| Offset | Name | R/W | Description |
|---|---|---|---|
| `+0x00`–`+0x07` | CONTEXT_ADDR | RW8 | Little-endian 64-bit address of the exact 64-byte Bank 0 context |
| `+0x08` | STEPS | RW8 | Chain steps, 0..15 |
| `+0x09` | START | RW8 | Starting hash index, 0..15 |
| `+0x0A` | CMD / STATUS | W8/R8 | Commands NOP=0, GO=1, CLEAR=2; states IDLE=0, BUSY=1, DONE=2, ERROR=3 |
| `+0x0B` | ERROR | R8 | Stable terminal error 0..9 |
| `+0x0C`–`+0x0F` | CYCLES | R8 | Saturating little-endian 32-bit busy/abort-drain count |
| `+0x10`–`+0x1F` | DOUT | R8 | Stable 16-byte terminal node |

The context is `PK.seed[16] || ADRS[32] || node[16]`. A successful request
always issues exactly 64 ascending, read-only Bank 0 DMA bytes. Zero steps
returns the input node after that read without claiming Keccak; nonzero work
uses the same physical 24-round service as SHA3/SHAKE and raw Keccak. The
requester has fixed weight 1 and no bandwidth limit, with one explicit
accepted beat and classified terminal response at a time. CLEAR withdraws an
unaccepted request or drains an accepted beat before returning IDLE.

`WOTS-CHAIN ( context-64 start steps dst-16 -- status )` is the only public
BIOS entry. It checks capability and complete spans, computes bounded waits
from `NUM_BUS_PORTS`, stages the result, proves CLEAR reached IDLE, and only
then writes the caller destination. A checked failure publishes no destination
bytes; clear timeout retains crypto guard 8 fail-closed. See the
[numeric contract](docs/crypto-interface-contract.md#wots-chain-contract) for
the complete error map, deadline formulas, state construction, and cleanup
ordering.

Checkpoint 4 has replaced KDOS's private GPT IEEE CRC loop with checked mode-4
resident-buffer transactions and raw-state chaining that releases ownership
between disk reads. `.CRC-DIAG` now exercises all six standard finalized
vectors plus reflected raw finalization. Fresh native and BIOS builds were
reproduced byte-for-byte; the ordered focused gates, full serial RTL sweep,
and approved Python regression completed with 3,425 passed and three
environment-conditional live-network skips. The MegaPad gate is complete;
Akashic adoption is a separate task in a user-selected Akashic worktree.

---

## BIOS — Forth REPL (v1.0)

The BIOS is a **subroutine-threaded Forth interpreter** written entirely in
Megapad-64 assembly. It boots from address 0 and
provides an interactive REPL over UART.

### Boot sequence

1. Initialise RSP (R15 ← ram_size) and DSP (R14 ← ram_size / 2)
2. Check COREID (CSR 0x20) — secondary cores branch to worker loop
3. Set up UART base in R8, TX ring descriptor pointer in R19, subroutine pointers in R4/R5/R6.  Register the ring buffer with the UART (write R19 to TX_RING_BASE).
4. Enable timer, install IVT for bus fault handler
5. Initialise Forth variables, reserve and scrub `NUM_CORES × 16` bytes above
   `dict_free` for checked CRC owner records, then set `HERE` to the resulting
   kernel-data end
6. Print banner (`Megapad-64 Forth BIOS v1.0`, RAM size)
7. Auto-boot: if disk present, scan MP64FS for first Forth file, FSLOAD it
8. Enter the outer interpreter (`QUIT` loop)

### Outer interpreter

The `QUIT` loop prints `> `, reads a line into the TIB (terminal input
buffer), then tokenises and interprets:

1. **Parse** the next whitespace-delimited word
2. **Find** it in the dictionary (case-insensitive linked-list walk)
3. If found → compute code address, `CALL.L` the word
4. If not found → try **parse_number** (supports `-`, `0x` prefix, BASE)
5. If valid number → push onto data stack
6. Otherwise → print `<word> ?` and abort the line

### Register conventions

| Register | Role |
|---|---|
| R0 | scratch / CSR operand (R0-R7 are CSR-capable) |
| R1 | scratch / argument / return value |
| R2 | ram_size (set at boot, preserved throughout) |
| R3 | PC (PSEL = 3) |
| R4 | → `NEXT` (SEP dispatch: advance IP, fetch CFA, branch) |
| R5 | → `ENTER` (SEP dispatch: push return address, enter colon def) |
| R6 | → `EXIT` (SEP dispatch: pop return address, resume caller) |
| R7 | scratch |
| R8 | UART base address (`0xFFFF_FF00_0000_0000`) |
| R9–R12 | scratch / temp |
| R13 | Scratch / temp |
| R19 | TX ring buffer descriptor pointer (set at boot) |
| R14 | DSP — data stack pointer (grows downward) |
| R15 | RSP — return stack pointer (grows downward) |
| R16 | NEXT handler (`sep r16` = fetch inline XT, advance IP, branch) |
| R17 | EXIT handler (`sep r17` = pop return address from RSP, branch) |
| R20 | Task yield handler (cooperative multitasking; `SEP R20` yields) |

### Built-in words (481)

**Stack manipulation**
`DUP` `DROP` `SWAP` `OVER` `ROT` `NIP` `TUCK` `2DUP` `2DROP` `DEPTH` `PICK`
`-ROT` `?DUP` `2OVER` `2SWAP` `2ROT`

**Arithmetic**
`+` `-` `*` `UM*` `/` `MOD` `/MOD` `NEGATE` `ABS` `1+` `1-` `2*` `2/`
`MIN` `MAX` `CELLS` `CELL+`

**Logic & bitwise**
`AND` `OR` `XOR` `INVERT` `LSHIFT` `RSHIFT`

**Comparison** (true = −1, false = 0)
`=` `<` `>` `0=` `0<` `0>` `<>` `0<>` `>=` `<=` `U<` `U>` `WITHIN`

**Memory**
`@` `!` `C@` `C!` `W@` `W!` `L@` `L!` `+!` `OFF`
`ALLOT` `,` `C,` `HERE` `CMOVE` `MOVE` `FILL` `DUMP`

**I/O & display**
`EMIT` `KEY` `KEY?` `CR` `.` `U.` `.S` `WORDS` `BYE`
`HEX` `DECIMAL` `BASE` `SPACE` `SPACES` `TYPE` `ACCEPT` `.ZSTR` `TX-FLUSH`

**String & parsing**
`S"` `."` `WORD` `COUNT` `COMPARE` `CHAR` `[CHAR]` `UCHAR`

**Control flow**
`IF` `ELSE` `THEN` `BEGIN` `UNTIL` `WHILE` `REPEAT` `AGAIN`
`DO` `LOOP` `+LOOP` `I` `J` `LEAVE` `UNLOOP`

**Compilation & defining**
`:` `;` `EXIT` `VARIABLE` `CONSTANT` `VALUE` `TO` `CREATE` `DOES>`
`IMMEDIATE` `STATE` `[` `]` `LITERAL` `POSTPONE` `RECURSE`
`EXECUTE` `'` `FIND`

**Return stack**
`>R` `R>` `R@` `2>R` `2R>` `2R@`

**Input source & interpreter**
`SOURCE` `>IN` `EVALUATE` `>NUMBER` `QUIT`

**Comments**
`\` `(`

**System**
`BL` `TRUE` `FALSE` `LATEST` `LATEST!` `ABORT` `ABORT"` `TALIGN` `FSLOAD`
`DICT-INDEX!` `DICT-INDEX@` `DICT-ROLLBACK`

**Tile engine**
`TVIEW` `TFILL` `TSRC0!` `TSRC1!` `TDST!` `TMODE!` `TCTRL!` `TMODE@` `TCTRL@`
`TADD` `TSUB` `TAND` `TOR` `TXOR` `TMUL` `TDOT` `TSUM`
`TMIN` `TMAX` `TTRANS` `TZERO` `TPOPCNT` `TL1` `TEMIN` `TEMAX` `TABS`
`TSUMSQ` `TMINIDX` `TMAXIDX` `TWMUL` `TMAC` `TFMA` `TDOTACC`
`TAMAC` `TACC-TRY` `TACC-CLEAR` `TACC-LOAD` `TACC-STORE` `TACC-RELEASE`
`TACC-STATUS@` `TACC-CLAIM?`
`ACC@` `ACC1@` `ACC2@` `ACC3@` `TI` `CYCLES`

### Full-width TACC model

The emulator implements one persistent 2,048-bit tile accumulator for each
physical tile engine: full cores 0–3 each have a private engine and TACC,
while each four-microcore cluster shares one engine and TACC. Microcores keep
their tile configuration and address CSRs private; legacy ACC, TACC,
ownership, and status follow the shared physical engine. `OWNER` is always an
absolute core ID and `MINE` is shaped for the caller reading the status CSR.

Control is deliberately explicit. Software performs
`TRY → CLEAR/LOAD → TAMAC… → STORE if needed → RELEASE`; there is no hidden
claim, wait, spill, eviction, or release. A failed `TRY` retires normally.
Owning the persistent TACC does not reserve the engine, so nonowners may
continue ordinary stateless and legacy-ACC MEX work. A guest that wants to
wait chooses its own policy:

```forth
: TACC-ACQUIRE
  BEGIN TACC-CLAIM? 0= WHILE PAUSE REPEAT ;
```

The canonical image is always 256 bytes aligned to 64 bytes and is transferred
as four 64-byte beats. U8/U16 use all 256 bytes; U32/FP16/BF16 use the low
128 bytes and keep the high half zero. The format is metadata, not part of the
image, so context-switch code saves it separately. External-memory transfers
further serialize each beat into eight little-endian 64-bit PHY words.

Functional stepping and strict-system execution share architectural results
but expose their respective timing boundaries:

| Transfer | Functional/native step | Strict system |
|---|---:|---:|
| Internal uncontended image | 6 cycles | 9 cycles |
| External, one-cycle word response | 34 cycles, 28 stalls | 37 cycles, 31 stalls |
| External, two-cycle word response | 66 cycles, 60 stalls | 69 cycles, 63 stalls |

Tests can install deterministic external responses with
`set_external_phy_response_plan()`. The callback receives one
`ExternalPhyWordRequest` per launched word and returns
`ExternalPhyWordResponse(latency_cycles=N, error=...)`; returning `None`
models no response. Cycle 255 still succeeds, while a later or absent response
times out at 255 with the exact word address. LOAD is atomic. A failing STORE
retains its acknowledged eight-byte prefix, preserves the accumulator's prior
valid/dirty state, and can be followed by normal recovery.

**Performance counters**
`PERF-CYCLES` `PERF-STALLS` `PERF-TILEOPS` `PERF-EXTMEM` `PERF-RESET`

**CRC engine (ISA-native, EXT.CRYPTO)**
`CRYPTO-CAPS@` `CRC-MODE!` `CRC-RESET` `CRC-INIT!` `CRC-FEED`
`CRC-FEED-BYTE` `CRC@` `CRC-RAW-FINAL@` `CRC-FINAL@`

**SHA-256 engine (ISA-native, EXT.CRYPTO)**
`SHA256-INIT` `SHA256-UPDATE` `SHA256-FINAL` `SHA256-CLEAR`
`SHA2-SPAN-STATUS`

All four words use a private per-core streaming context and return checked
status (`0` OK, `1` STATE, `2` RANGE, `3` CONTEXT-ALIAS,
`4` LENGTH-OVERFLOW). Failed operations abort and wipe, and failed `FINAL`
does not publish a digest to a non-context destination.

`SHA2-SPAN-STATUS ( addr len -- status )` is a pure pre-`INIT` check shared
by SHA-256 and SHA-512. It returns only `0` OK, `2` RANGE, or `3`
CONTEXT-ALIAS after validating one complete physical window and the union of
both algorithms' all-core BIOS context arenas. An empty span returns zero.

**SHA-512 streaming (ISA-native, EXT.CRYPTO mode 2)**
`SHA512-INIT` `SHA512-UPDATE` `SHA512-FINAL` `SHA512-CLEAR`

All four words return a checked status (`0` OK, `1` STATE, `2` RANGE,
`3` CONTEXT-ALIAS, `4` LENGTH-OVERFLOW). KDOS also provides
`SHA512 ( addr len out -- status )` and symbolic constants for those values.
UPDATE and FINAL reject an active marker other than exactly one, an offset
outside 0..127, a non-byte-aligned low bit length, or a low-length
modulo-128 position that disagrees with the saved offset.

**Memory BIST**
`BIST-FULL` `BIST-QUICK` `BIST-STATUS` `BIST-FAIL-ADDR` `BIST-FAIL-DATA`

**Tile self-test**
`TILE-TEST` `TILE-TEST@` `TILE-DETAIL@`

**Stride / 2D addressing**
`TSTRIDE-R!` `TSTRIDE-R@` `TTILE-H!` `TTILE-W!` `TLOAD2D` `TSTORE2D`

**FP16 / BF16 modes**
`FP16-MODE` `BF16-MODE`

**Instruction cache**
`ICACHE-ON` `ICACHE-OFF` `ICACHE-INV` `ICACHE-HITS` `ICACHE-MISSES`

**NIC**
`NET-STATUS` `NET-SEND` `NET-RECV` `NET-MAC@`

**AES-256-GCM**
`AES-KEY!` `AES-IV!` `AES-AAD-LEN!` `AES-DATA-LEN!` `AES-CMD!`
`AES-STATUS@` `AES-DIN!` `AES-DOUT@` `AES-TAG@` `AES-TAG!`

**SHA-3 / SHAKE**
`SHA3-BEGIN` `SHA3-UPDATE` `SHA3-FINAL` `SHA3-STATUS@`
`SHAKE-FINAL` `SHA3-MODE@` `SHAKE-READ` `SHA3-CLEAR`
`KECCAK-F1600`

The checked words return the common status 0..6; `SHA3-STATUS@` and
`SHA3-MODE@` are diagnostic raw reads. The removed transaction and prototype
WOTS words have no aliases. The production checked
`WOTS-CHAIN ( context-64 start steps dst-16 -- status )` is a distinct BIOS
word, and the qualified checkpoint-3 configuration advertises all four bits
with `CRYPTO_CAPS = 0xF`.

**TRNG**
`RANDOM` `RANDOM8` `SEED-RNG` `ENTROPY-FILL` `ENTROPY-READY?` — the raw
random reads deliver a bus fault if the device is unusable; `SEED-RNG` is
supplemental and cannot make an unusable source healthy.
`ENTROPY-FILL ( addr len -- status )` provides the checked bulk boundary with
`0` OK, `1` UNAVAILABLE, `2` RANGE, and `3` PROTECTED.
`ENTROPY-READY? ( -- flag )` keeps the MMIO address private and returns
canonical true only for exact `STATUS == 1`.

The checked word accepts every empty span (including `(0,0)`) as a no-op.
Nonempty destinations must be nonnegative, non-null, and fit wholly, without
wrap, in one advertised Bank 0, external, HBW, or VRAM window. Bank 0 is
narrowed to `[kernel-data-end, caller-DSP-8)`, keeping the static BIOS/private
footprint, topology-sized CRC owner records, live stacks, and future result
cell out of reach. This geometry is a protection boundary rather than proof
that the caller owns an allocation.
Exact `USABLE == 1` is required before every byte and after completion. A
detected post-start loss wipes the entire admitted destination; an initial
loss writes nothing.

There is no BIOS transaction state between calls. The one data-read
instruction private to `ENTROPY-FILL` has a PC-scoped bus-fault recovery
point, so a health transition after the status check returns UNAVAILABLE and
uses the same complete-span wipe path. Unrelated bus faults remain diagnostic.
Health loss caused by a successfully delivered byte is caught by the
following status check, including after the final byte.

**Field ALU (GF(p) arithmetic)**
`FADD` `FSUB` `FMUL` `FSQR` `FINV` `FPOW` `FMUL-RAW`
`FCMOV` `FCEQ` `FMAC` `FMUL-ADD-RAW`
`GF-A!` `GF-R@` `GF-PRIME` `LOAD-PRIME`

These 15 raw words use addresses to 32-byte little-endian values; raw
multiply/MAC take separate low and high destinations, and `FCMOV` takes an
operand address plus a condition-byte address. See the
[BIOS reference](docs/bios-forth.md#field-alu--multi-prime-arithmetic-15-raw-words)
for persistent per-core state, canonical-input qualifications, and current
Python/native/RTL discrepancies.

**NTT Engine**
`NTT-LOAD` `NTT-STORE` `NTT-FWD` `NTT-INV` `NTT-PMUL`
`NTT-PADD` `NTT-SETQ` `NTT-IDX!` `NTT-STATUS@` `NTT-WAIT`

These are the 10 raw words in the checked-in dictionary chain. The executable
BIOS/Python-device contract and current RTL NTT differ in register layout,
transfer width, configurable-root behavior, and timing; see the
[BIOS reference](docs/bios-forth.md#ntt-engine-10-raw-words).

**KEM Engine (ML-KEM-512)**
`KEM-KEYGEN` `KEM-ENCAPS` `KEM-DECAPS` `KEM-SETQ`
`KEM-STATUS@` `KEM-PK@` `KEM-CT@`

**Disk / Storage**
`DISK@` `DISK-SEC!` `DISK-DMA!` `DISK-N!` `DISK-READ` `DISK-WRITE` `DISK-FLUSH`

**Timer & Interrupts**
`TIMER!` `TIMER-CTRL!` `TIMER-ACK` `EI!` `DI!` `ISR!`

**Multicore**
`COREID` `NCORES` `IPI-SEND` `IPI-STATUS` `IPI-ACK`
`MBOX!` `MBOX@` `SPIN@` `SPIN!` `WAKE-CORE` `CORE-STATUS`

**Cooperative Multitasking**
`PAUSE` `TASK-YIELD` `BACKGROUND` `TASK-STOP` `TASK?` `BACKGROUND2` `BACKGROUND3` `#TASKS`

### Dictionary structure

Each entry is a linked list node:

```
┌────────────┬───────┬──────────────┬─────────────────────────┐
│ link (8 B) │ flags │ name (N B)   │ trampoline code         │
│ → prev     │ (1 B) │ length-       │ ldi64 Rn, impl_addr    │
│   entry    │       │ prefixed     │ call.l Rn               │
│            │       │              │ ret.l                    │
└────────────┘───────┘──────────────┘─────────────────────────┘
```

- **Link**: 64-bit pointer to the previous entry (0 = end)
- **Flags**: low 5 bits = name length, bit 7 = immediate (unused in MVP)
- **Trampoline**: `ldi64` + `call.l` + `ret.l` — jumps to the native
  implementation. This is the subroutine-threaded call mechanism.

---

## CLI Monitor

When launched *without* `--bios`, the CLI drops into the `MP64>` monitor
prompt.  With `--bios`, the BIOS console is the primary interface, but the
monitor is still available for debugging.

### Loading

| Command | Description |
|---|---|
| `load <file> [addr]` | Load raw binary into RAM (default address 0) |
| `asm <file.asm> [addr]` | Assemble and load into RAM |
| `asm -e "ldi r1, 42; halt"` | Inline assembly (semicolons = newlines) |

### Execution

| Command | Description |
|---|---|
| `boot [addr]` | Cold boot: reset CPU, PC ← addr (default 0) |
| `reset` | Reset CPU state, keep RAM |
| `step [N]` | Single-step N instructions (default 1) |
| `run [max]` | Run up to max steps (default 1M) |
| `continue` / `c` | Resume from current PC |
| `console` | Enter raw UART console (Ctrl-] to exit) |

### Steps, cycles, and hardware time

A single-core emulator **step** is one retired MP64 guest instruction. It is
not one hardware clock. Step counts are therefore stable work measurements for
equivalent emulator configurations, while wall time also includes host CPU,
native-engine, device-model, and reporting overhead.

Keep the three timing domains separate:

| Domain | What advances it | What it proves |
|---|---|---|
| Retired guest steps | Completed architectural instructions | Stable software work for equivalent configurations |
| Emulator virtual cycles | The deterministic instruction/device timing model | Timer, device, scheduler, strict-system-cycle, and replay behavior inside the emulator |
| RTL or silicon clocks | Physical state-machine, pipeline, cache, bus, memory, and peripheral edges | Realized hardware CPI and wall time for one implementation |

The emulator's cycle counter applies its architectural timing model to those
instructions. It is not a claim of cycle parity with the current RTL: the RTL
has a multi-state fetch/decode/execute path, an instruction cache, shared-bus
arbitration, and physical memory latency that the functional/native execution
path does not reproduce globally. Hardware time must be projected from a
specific implementation's realized CPI:

```text
seconds = retired guest instructions * realized CPI / clock frequency
```

For example, the 2026-08-29 stored-source rich Desktop boot reached Desk entry
after 13.266 billion guest instructions. The following are arithmetic
scenarios, not timing claims:

| Realized CPI | 2 GHz | 4 GHz |
| ---: | ---: | ---: |
| 1.0 | 6.63 s | 3.32 s |
| 1.2 | 7.96 s | 3.98 s |
| 2.0 | 13.27 s | 6.63 s |
| 3.0 | 19.90 s | 9.95 s |
| 4.0 | 26.53 s | 13.27 s |

That emulator run sustained about 60.2 million guest instructions per host
second and took 220.34 seconds. A 2--4 GHz result would require a modernized
ASIC implementation with the corresponding pipeline, cache, SRAM, and memory
system; it must not be inferred by applying those clocks to the current
100 MHz FPGA target or by treating the emulator's modeled cycle/step ratio as
measured RTL CPI.

Likewise, a goal of roughly 1--2 **average** CPI applies to common hot scalar
work on a future MegaPad RTL/ASIC implementation, not to every instruction.
Cache misses, shared-bus service, XMEM, MMIO, calls and returns, division, and
accelerator operations can remain multi-cycle. The current portable RTL does
not establish that target: its buffered fetch/decode state flow gives a common
warm simple instruction a roughly four-clock floor, with longer execute and
memory paths.

Reaching the target is a hardware implementation change, not an architectural
reset. It may add an in-order pipeline, forwarding, prediction that cannot
leak side effects, and better caches or SRAM while preserving precise
retirement, non-speculative MMIO and shared effects, complete prefix/PSEL
semantics, interrupts at instruction boundaries, self-modifying-code
visibility, deterministic ordering, and the visible costs of Bank 0, XMEM,
and HBW. The architectural emulator can remain instruction-level; an optional
silicon timing model may be added separately when cycle-accurate comparison is
needed.

### Breakpoints

| Command | Description |
|---|---|
| `bp <addr>` | Set breakpoint |
| `bp` | List all breakpoints |
| `bpd <addr>` | Delete breakpoint |
| `bpd all` | Delete all |

### Inspection

| Command | Description |
|---|---|
| `regs` | All 32 registers + PC, SP, D, flags |
| `flags` | CPU flags: Z, C, N, V, P, G, I, S |
| `dump <addr> [len]` | Hex dump (default 256 bytes) |
| `disasm [addr] [count]` | Disassemble from addr (default PC) |
| `status` | Full system status |
| `devices` | List MMIO devices |
| `cycles` | Total CPU cycle count |

### Modification

| Command | Description |
|---|---|
| `setreg <reg> <val>` | Set register (`setreg r1 0xFF`, `setreg pc 0x100`) |
| `setmem <addr> <bytes>` | Write hex bytes (`setmem 0x100 48 65 6C`) |
| `send <text>` | Inject text into UART RX buffer |
| `uart` | Show UART buffer status |

### Storage & config

| Command | Description |
|---|---|
| `storage attach/detach/info/save` | Manage disk image |
| `ramsize [KiB]` | Show or change RAM size (recreates system) |
| `quit` / `exit` / `q` | Exit |

---

## Assembler

The assembler (`asm.py`) is a two-pass, label-resolving assembler that
covers the complete Megapad-64 ISA.

### Key features

- All 16 instruction families (SYS, INC, DEC, BR, LBR, MEM, IMM, ALU,
  MEMALU, I/O, SEP, SEX, MULDIV, CSR, MEX/tile, EXT)
- **`ldi64 Rn, value`** — full 64-bit immediate (11 bytes: EXT prefix +
  opcode + register + 8 LE bytes).  Required for MMIO addresses and large
  constants.
- **Labels** resolve in both passes; forward references work.
- **Directives**: `.db`, `.dw`, `.dd`, `.dq` (data), `.ascii`, `.asciiz`
  (strings), `.align`
- **Short branches** (`br`, `breq`, `brne`, `brcc`, `brcs`, `brgt`, `brle`)
  have ±127 byte range.  **Long branches** (`lbr`, `lbreq`, etc.) support
  ±32 KiB.  Out-of-range short branches produce an assembler error.

### Carry flag convention

After `CMP a, b` (which computes a − b):

| Condition | Flag | Branch taken | Branch not taken |
|---|---|---|---|
| a ≥ b (unsigned) | C = 1 | `brcs` | `brcc` |
| a < b (unsigned) | C = 0 | `brcc` | `brcs` |
| a > b (unsigned) | G = 1 | `brgt` | `brle` |
| a = b | Z = 1 | `breq` | `brne` |
| a < b (signed) | N ⊕ V | `brlt` | `brge` |

### CALL.L / RET.L

64-bit subroutine call/return via the return stack (R15):

```asm
    ldi64 r11, my_function
    call.l r11              ; push return addr, jump to r11
    ; ...continues here after ret.l

my_function:
    ; ...
    ret.l                   ; pop return addr, jump back
```

There are no `push64`/`pop64` instructions.  Manual stack operations use
`subi r14, 8` / `str r14, r1` (push) and `ldn r1, r14` / `addi r14, 8` (pop).

---

## Running Tests

All tests are run via the Makefile.

```bash
# C++ accelerator (recommended — 63× faster than PyPy)
make accel                                                 # build C++ extension
make test-accel                                            # ~23 s

# Or use the standard background runner:
make test                                                  # background, check with make test-status
make test-one K=TestKDOS                                   # single class
make test-one K=test_coreid_word                           # single test
```

Parallel checkouts can isolate the background PID, output, and live-status
files by exporting a runtime namespace before invoking test or monitor
targets:

```bash
export MP64_RUNTIME_NAMESPACE=megapad-concurrency
make test-one K=TestKDOS
make test-status
```

With no namespace, the historical `/tmp/megapad_test_*` paths remain
unchanged. Namespaced artifacts live in the UID-owned, mode-`0700` directory
`/tmp/megapad-runtime-<uid>-<namespace>/`.

| Runner | Parallelism | Approximate Time | Speedup |
|--------|-------------|-------------------|---------| 
| CPython | sequential | ~40 min | 1× |
| PyPy + xdist -n 8 | 8 workers | ~24 min | 1.7× |
| **CPython + C++ accel -n 8** | **8 workers** | **~23 s** | **104×** |

The C++ accelerator (`emulator/accel/`) reimplements the CPU step loop as a
multi-source pybind11 execution kernel. `emulator/system.py` imports it automatically
when available and falls back to pure Python if not. The accelerator handles
single-core and multicore execution (C++ for the active core, Python for device I/O
and MMIO dispatch).

PyPy's JIT gives **~5× speedup** on the pure-Python CPU loop; pytest-xdist
adds parallel execution across 8 workers.

The system tests exercise the full stack: devices, MMIO routing, the
Forth BIOS (all 481 words), KDOS (buffers, kernels, pipelines, scheduler,
filesystem, screens, data ports, multicore dispatch, network stack,
TLS 1.3, socket API, post-quantum crypto), extended tile engine
(saturating, rounding, FP16/BF16, strided/2D, SHUFFLE/PACK/RROT), CRC
engine, memory BIST, tile self-test, performance counters, multicore
SoC features (IPI, mailbox, spinlocks, barriers), Field ALU, NTT, KEM,
and real-network tests against a Linux TAP device (ARP, ICMP, UDP, TCP).

---

## HLE Graphics Acceleration

The C++ accelerator provides **High-Level Emulation (HLE)** for
performance-sensitive BIOS graphics words.  This is the same technique
used by game console emulators (PCSX2, Dolphin, RPCS3): when the CPU
calls a known BIOS function, the emulator intercepts the `CALL.L`
instruction and runs a native C++ implementation instead of interpreting
the assembly instruction-by-instruction.

**Key points:**

- HLE traps are **emulator-only** — they have no hardware equivalent.
  The BIOS assembly implementations are the ground truth; the C++
  versions must produce identical results.
- No MMIO registers, no device state, no RTL module.  The traps operate
  directly on the emulator's RAM buffer.
- If the C++ accelerator is not loaded, the BIOS assembly runs
  unmodified — correctness is never dependent on HLE.

### Mechanism

During BIOS assembly, `cli.py` records the entry address of each
HLE-eligible word.  At runtime, when the CPU executes `CALL.L Rn`, the
C++ core checks the target address against the hook table.  On a match
it runs the native implementation and skips the normal push/jump — the
caller sees an instant return with the correct stack effects.

### Current hooks

| Hook | BIOS word | Stack effect | Description |
|------|-----------|--------------|-------------|
| 1 | `RECT-FILL` | `( pixel-addr w h stride fg16 -- )` | Fill a w×h rectangle with a 16-bit colour |
| 2 | `BLIT-GLYPH` | `( glyph-addr pixel-addr stride fg16 -- )` | Render an 8×8 1-bpp glyph with foreground colour |
| 3 | `VRAM-COPY` | `( src dst stride w h -- )` | Copy a w×h byte rectangle between VRAM regions |
| 4 | `BLIT-STRING` | `( c-addr len pixel-addr stride fg16 font-base -- )` | Render a string of 8×8 glyphs, advancing 16 bytes/char |

All four words have full assembly implementations in `bios.asm` that
pass the test suite without the C++ accelerator.  The HLE versions
provide a ~100× speedup for framebuffer-intensive operations during
emulated execution.

---

## Example: Scripted Test via Pipe

```bash
$ printf '1 2 3 .S\nDROP DROP DROP\n100 7 /MOD . .\nBYE\n' \
    | python cli.py --bios bios.rom

Megapad-64 Forth BIOS v1.0
RAM: 00100000 bytes
 ok
> 1 2 3 .S
<3> 1 2 3  ok
> DROP DROP DROP
 ok
> 100 7 /MOD . .
14 2  ok
> BYE
Bye!
```

---

## Headless Mode (TCP Terminal Server)

The emulator can run as a headless TCP server, allowing remote access
to the Forth REPL without a local terminal.  The CPU runs in a background
thread; UART I/O is served over TCP.

```bash
# Start the headless server (default port 6464)
python cli.py --bios bios.asm --storage sample.img --headless

# Custom port
python cli.py --bios bios.asm --storage sample.img --headless --headless-port 7777

# Connect with built-in client
python cli.py --connect localhost:6464

# Or plain nc / telnet
nc localhost 6464
```

Server status (PID + port) is written to `/tmp/megapad_headless.json`.
Multiple clients can connect simultaneously; all see TX output and any
can send input.  Ctrl+] disconnects a client session.

This is the recommended way to interact with the emulator for production
testing, CI pipelines, or any scenario where the emulator should persist
across multiple interactive sessions.

---

## Project History

| Commit | Milestone |
|---|---|
| `32481a2` | Bytecode emulator + assembler + 61 CPU tests |
| `c3b9001` | Peripheral layer: UART, Timer, Storage, SysInfo |
| `8ef8f3b` | System emulator with unified memory map |
| `67a0e14` | CLI monitor with disassembler |
| `82043bc` | BIOS v0.1 — monitor shell |
| `ea04090` | Integration test suite (23 tests) |
| `220e2e2` | BIOS v0.2 — tile engine commands |
| `a5ffeba` | `--assemble` flag for `.rom` precompilation |
| `b879ff5` | **BIOS v0.3 — Forth MVP REPL**, 62 words, 42 system tests |
| `182ab06` | **BIOS v1.0** — 197 words, FSLOAD, KDOS v1.0 |
| `0efa9bb` | Quad-core FPGA SoC architecture |
| `3d053d0` | Emulator multicore support (round-robin, mailbox, spinlocks) |
| `9183f88` | BIOS multicore boot (11 words, worker loop, IPI handler) |
| `366aace` | **KDOS v1.1** — multicore dispatch (CORE-RUN, BARRIER, P.RUN-PAR) |
| `f5578b0` | **C++ CPU accelerator** — pybind11, 63× speedup, 754 tests (23 s) |
