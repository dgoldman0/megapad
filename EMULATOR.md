# Megapad-64 System Emulator

A complete system-level emulator for the Megapad-64 architecture: CPU,
memory-mapped I/O peripherals, a two-pass assembler, a Forth REPL BIOS,
and an interactive CLI monitor/debugger.

> **Branch:** `features/extended-tpu-impl`
> **Status:** Fully functional.  265-word BIOS v1.0 Forth system running on
> a quad-core emulated SoC with mailbox IPI, spinlocks, extended tile engine
> (saturating, FP16/BF16, strided/2D, CRC, BIST), and 593 tests passing.

---

## Quick Start

```bash
# Boot the Forth REPL directly (assembles .asm on the fly)
python cli.py --bios bios.asm

# Pre-compile to a .rom, then boot from binary
python cli.py --assemble bios.asm bios.rom
python cli.py --bios bios.rom

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
| `--cores N` | 1 | Number of CPU cores (1–4) |

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────┐
│                      cli.py  (995 lines)                 │
│       Interactive monitor / debugger / console           │
└──────────────────────┬───────────────────────────────────┘
                       │
┌──────────────────────▼───────────────────────────────────┐
│                    system.py  (474 lines)                 │
│       MegapadSystem — quad-core SoC, memory map          │
│                                                          │
│  ┌──────────────┐    ┌────────────────────────────────┐  │
│  │  megapad64.py │    │       devices.py  (1,376 lines) │  │
│  │   CPU core    │    │ ┌──────┐ ┌─────┐ ┌─────────┐ │  │
│  │  16 × 64-bit  │◄──►│ │ UART │ │Timer│ │ Storage │ │  │
│  │  registers    │    │ └──────┘ └─────┘ └─────────┘ │  │
│  │  full ISA     │    │ ┌─────────┐ ┌───────┐        │  │
│  │  tile engine  │    │ │ SysInfo │ │Mailbox│  NIC   │  │
│  │  (2516 lines) │    │ └─────────┘ └───────┘        │  │
│  │  extended ops │    │ ┌──────────┐ ┌─────┐         │  │
│  │  FP16/BF16   │    │ │ Spinlock │ │ CRC │ DevBus  │  │
│  └──────────────┘    │ └──────────┘ └─────┘         │  │
│                      └────────────────────────────────┘  │
│                                                          │
│          asm.py  (788 lines)  — two-pass assembler        │
└──────────────────────────────────────────────────────────┘

    bios.asm  (9,895 lines)  — Forth BIOS v1.0, 265 words
    bios.rom  (~22 KB)       — precompiled binary
```

### Source files

| File | Lines | Role |
|---|---|---|
| `megapad64.py` | 2,516 | CPU core — 16×64-bit GPRs, all 16 instruction families, flags, CSRs, traps, tile engine, extended ops, FP16/BF16 |
| `asm.py` | 788 | Two-pass assembler — full mnemonic set, `ldi64`, `.ascii`, `.asciiz`, `.db`/`.dw`/`.dd`/`.dq`, SKIP |
| `devices.py` | 1,376 | Peripherals — UART, Timer, Storage, SystemInfo, NIC, Mailbox (IPI), Spinlock, CRC, AES-256-GCM, SHA3-256 |
| `system.py` | 474 | Quad-core SoC glue — wires N CPU cores + DeviceBus, mailbox IPI, spinlocks, round-robin stepping |
| `cli.py` | 995 | CLI monitor with disassembler, breakpoints, console mode, pipe mode, `--assemble` |
| `bios.asm` | 9,895 | Forth BIOS v1.0 — subroutine-threaded interpreter, 265 built-in words (incl. multicore, extended tile, I-cache, AES, SHA3) |
| `test_megapad64.py` | 2,193 | CPU + tile engine test suite — 23 tests |
| `test_system.py` | 7,308 | System integration tests — 570 tests (devices, MMIO, BIOS, KDOS, multicore, FS, crypto, extended tile) |
| `Makefile` | 71 | Build & test targets — PyPy + xdist parallel runner |
| `fpga/rtl/` | ~8,200 | 14 Verilog RTL modules — CPU, tile engine, FP16 ALU, I-cache, SoC, peripherals |
| `fpga/sim/` | 3,930 | 8 Verilog testbenches — 72 hardware tests |
| **Total** | **~35,000** | |

---

## Memory Map

| Address Range | Size | Description |
|---|---|---|
| `0x0000_0000` – top of RAM | Configurable | RAM (default 1 MiB = 0x100000) |
| `0xFFFF_FF00_0000_0000` + `0x0000` | 256 B | UART (serial console) |
| `0xFFFF_FF00_0000_0000` + `0x0100` | 256 B | Timer |
| `0xFFFF_FF00_0000_0000` + `0x0200` | 256 B | Storage controller |
| `0xFFFF_FF00_0000_0000` + `0x0300` | 256 B | System info |
| `0xFFFF_FF00_0000_0000` + `0x0400` | 128 B | NIC (Network Interface) |
| `0xFFFF_FF00_0000_0000` + `0x0500` | 256 B | Mailbox (inter-core IPI) |
| `0xFFFF_FF00_0000_0000` + `0x0600` | 256 B | Spinlock (hardware mutexes) |
| `0xFFFF_FF00_0000_0000` + `0x0700` | 128 B | AES-256-GCM (authenticated encryption) |
| `0xFFFF_FF00_0000_0000` + `0x0780` | 64 B | SHA-3/SHAKE (hashing, key derivation) |
| `0xFFFF_FF00_0000_0000` + `0x07C0` | 64 B | CRC Engine (CRC32/CRC32C/CRC64) |

The system layer intercepts any CPU memory operation (8/16/32/64-bit) that
falls in the MMIO aperture and routes it through the device bus; everything
else hits RAM.

### BIOS memory layout (runtime)

```
0x00000  ┌──────────────────────┐
         │  BIOS code           │  ~5650 bytes
         │  dictionary entries  │
         │  strings / IVT / TIB │
         ├──────────────────────┤ ← dict_free / HERE
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

Read-only board identification.

| Offset | Name | Description |
|---|---|---|
| `+0x00`–`+0x03` | BOARD_ID | `"MP64"` (ASCII) |
| `+0x04` | VERSION | Board revision |
| `+0x05`–`+0x06` | MEM_SIZE | RAM size in KiB (LE 16-bit) |
| `+0x07` | FEATURES | bit 0: UART, bit 1: storage present |
| `+0x08` | STORAGE | `1` if storage attached, else `0` |

---

## BIOS — Forth REPL (v1.0)

The BIOS is a **subroutine-threaded Forth interpreter** written entirely in
Megapad-64 assembly (9,895 lines, ~22 KB).  It boots from address 0 and
provides an interactive REPL over UART.

### Boot sequence

1. Initialise RSP (R15 ← ram_size) and DSP (R14 ← ram_size / 2)
2. Check COREID (CSR 0x20) — secondary cores branch to worker loop
3. Set up UART base in R8, subroutine pointers in R4/R5/R6
4. Enable timer, install IVT for bus fault handler
5. Initialise Forth variables: STATE=0, BASE=10, HERE=dict_free, LATEST
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
| R4 | → `emit_char` subroutine |
| R5 | → `key_char` subroutine (blocking UART read) |
| R6 | → `print_hex_byte` subroutine |
| R7 | scratch |
| R8 | UART base address (`0xFFFF_FF00_0000_0000`) |
| R9–R13 | scratch / temp |
| R14 | DSP — data stack pointer (grows downward) |
| R15 | RSP — return stack pointer (grows downward) |

### Built-in words (265)

**Stack manipulation**
`DUP` `DROP` `SWAP` `OVER` `ROT` `NIP` `TUCK` `2DUP` `2DROP` `DEPTH` `PICK`
`-ROT` `?DUP` `2OVER` `2SWAP` `2ROT`

**Arithmetic**
`+` `-` `*` `/` `MOD` `/MOD` `NEGATE` `ABS` `1+` `1-` `2*` `2/`
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
`HEX` `DECIMAL` `BASE` `SPACE` `SPACES` `TYPE` `ACCEPT` `.ZSTR`

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
`BL` `TRUE` `FALSE` `LATEST` `ABORT` `ABORT"` `TALIGN` `FSLOAD`

**Tile engine**
`TVIEW` `TFILL` `TSRC0!` `TSRC1!` `TDST!` `TMODE!` `TCTRL!` `TMODE@` `TCTRL@`
`TADD` `TSUB` `TAND` `TOR` `TXOR` `TMUL` `TDOT` `TSUM`
`TMIN` `TMAX` `TTRANS` `TZERO` `TPOPCNT` `TL1` `TEMIN` `TEMAX` `TABS`
`TSUMSQ` `TMINIDX` `TMAXIDX` `TWMUL` `TMAC` `TFMA` `TDOTACC`
`ACC@` `ACC1@` `ACC2@` `ACC3@` `TI` `CYCLES`

**Performance counters**
`PERF-CYCLES` `PERF-STALLS` `PERF-TILEOPS` `PERF-EXTMEM` `PERF-RESET`

**CRC engine**
`CRC-POLY!` `CRC-INIT!` `CRC-FEED` `CRC@` `CRC-RESET` `CRC-FINAL`

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

**Disk / Storage**
`DISK@` `DISK-SEC!` `DISK-DMA!` `DISK-N!` `DISK-READ` `DISK-WRITE`

**Timer & Interrupts**
`TIMER!` `TIMER-CTRL!` `TIMER-ACK` `EI!` `DI!` `ISR!`

**Multicore**
`COREID` `NCORES` `IPI-SEND` `IPI-STATUS` `IPI-ACK`
`MBOX!` `MBOX@` `SPIN@` `SPIN!` `WAKE-CORE` `CORE-STATUS`

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
| `regs` | All 16 registers + PC, SP, D, flags |
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

```bash
# CPython (works out of the box)
python -m pytest test_megapad64.py -v                          # 23 CPU + tile tests
python -m pytest test_system.py -v --timeout=30                # 1184 integration tests
python -m pytest test_system.py test_megapad64.py -v --timeout=30  # all 1207

# PyPy + xdist (recommended — ~10× total speedup)
make setup-pypy        # one-time: downloads PyPy, installs pytest + xdist
make test              # PyPy + 8 parallel workers  (~4 min)
make test-seq          # PyPy sequential            (~8 min)
make test-quick        # PyPy, BIOS + CPU only      (~6 sec)
make test-one K=test_coreid_word   # single test with PyPy
```

PyPy's JIT gives **~5× speedup** on the CPU emulator loop; pytest-xdist
adds **parallel execution** across 8 workers for another ~2× improvement.
CPython works fine but takes ~40 minutes for the full suite.

The system tests exercise the full stack: devices, MMIO routing, the
Forth BIOS (all 265 words), KDOS (buffers, kernels, pipelines, scheduler,
filesystem, screens, data ports, multicore dispatch), extended tile engine
(saturating, rounding, FP16/BF16, strided/2D, SHUFFLE/PACK/RROT), CRC
engine, memory BIST, tile self-test, performance counters, and multicore
SoC features (IPI, mailbox, spinlocks, barriers).

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

## Project History

| Commit | Milestone |
|---|---|
| `3d321a9` | Instruction encoding spec (`ENCODING.html`) |
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
