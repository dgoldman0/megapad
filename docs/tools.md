# Megapad-64 Tools Reference

This document covers the four main host-side tools that make up the
Megapad-64 development environment:

1. **CLI & Debug Monitor** (`cli.py`) — boot, run, and inspect the system
2. **Assembler** (`asm.py`) — translate assembly source to machine code
3. **Disk Utility** (`diskutil.py`) — create and manage MP64FS disk images
4. **Test Suite** (`test_megapad64.py`, `test_system.py`) — verify everything works

All tools are pure Python 3 with no external dependencies.

---

## CLI & Debug Monitor

`cli.py` is the main entry point for running the Megapad-64 emulator.
It boots the system, provides a raw console connection to the BIOS/KDOS
REPL, and includes a full interactive debug monitor.

### Command-Line Flags

```bash
python cli.py [flags]
```

| Flag | Argument | Default | Description |
|------|----------|---------|-------------|
| `--bios` | FILE | — | BIOS binary (`.rom`) or assembly (`.asm`) to boot. Enters console mode. |
| `--forth` | FILE | — | Forth source file to inject via UART after BIOS boot. Repeatable. |
| `--storage` | FILE | — | MP64FS disk image to attach to the storage controller. |
| `--load` | FILE[@ADDR] | — | Load a raw binary into memory at ADDR (default 0). Repeatable. |
| `--assemble` | SRC OUT | — | Assemble SRC to OUT.rom and exit. No emulator started. |
| `--run` | — | off | Auto-boot and run after loading (non-BIOS mode). |
| `--ram` | KiB | 1024 | RAM size in kibibytes. |
| `--listing` / `-l` | — | off | Print assembler listing to stdout (assemble-only mode). |
| `--nic` | PORT | — | Enable NIC device with UDP passthrough on PORT. |
| `--nic-peer` | PORT | NIC+1 | UDP peer port for NIC communication. |

### Boot Modes

**BIOS + disk mode** (recommended) — use `--bios` + `--storage`:

```bash
python cli.py --bios bios.asm --storage sample.img
```

The assembler builds the BIOS, the CPU boots from address 0, detects the
disk, reads the MP64FS directory, and auto-loads the first Forth-type
file (typically `kdos.f`).  You land in the KDOS REPL with full
filesystem access.  Press **Ctrl+]** to escape to the debug monitor.

**BIOS + UART injection** (development) — use `--bios` + `--forth`:

```bash
python cli.py --bios bios.asm --forth kdos.f
```

The `--forth` files are injected line-by-line through the UART after
BIOS boot.  Useful for testing Forth source changes without rebuilding
the disk image.  No filesystem access in this mode.

> **Don't combine `--forth kdos.f` with `--storage`** — the BIOS will
> auto-load KDOS from the disk, and `--forth` would load it again.

**Non-BIOS mode** — use `--load` and optionally `--run`:

```bash
python cli.py --load myprogram.rom@0x0 --run
```

Loads a raw binary and starts execution.  Without `--run`, you get the
debug monitor immediately.

**Assemble-only mode** — use `--assemble`:

```bash
python cli.py --assemble myprogram.asm myprogram.rom
```

Assembles the source file, writes the binary, prints the byte count, and
exits.  No emulator is started.

Add `-l` for a listing that interleaves addresses, hex bytes, and source:

```bash
python cli.py --assemble myprogram.asm myprogram.rom -l
```

### Debug Monitor

The debug monitor is an interactive prompt (`MP64>`) for inspecting and
controlling the emulated machine.  In BIOS mode, reach it with **Ctrl+]**.
In non-BIOS mode, it's the default interface.

#### Execution Control

| Command | Syntax | Description |
|---------|--------|-------------|
| `boot` | `boot [addr]` | Reset CPU and set PC to *addr* (default 0) |
| `reset` | `reset` | Hard reset the CPU |
| `step` | `step [n]` | Step *n* instructions (default 1). Shows disassembly per step. |
| `run` | `run [max]` | Run until halt, idle, or breakpoint. Default limit: 1,000,000 steps. |
| `continue` | `continue` | Alias for `run` |
| `console` | `console` | Return to BIOS console mode (Ctrl+] to come back) |

#### Breakpoints

| Command | Syntax | Description |
|---------|--------|-------------|
| `bp` | `bp [addr]` | Set a breakpoint at *addr*, or list all breakpoints |
| `bpd` | `bpd [addr]` | Delete breakpoint at *addr*, or clear all breakpoints |

#### Inspection

| Command | Syntax | Description |
|---------|--------|-------------|
| `regs` | `regs` | Show all CPU registers (R0–R15, PC, SP, flags, cycles) |
| `flags` | `flags` | Detailed flag display (Z, C, N, V, P, G, I, S, D, Q, T) |
| `dump` | `dump [addr] [len]` | Hex + ASCII memory dump (default: 128 bytes) |
| `disasm` | `disasm [addr] [n]` | Disassemble *n* instructions (default: 16 from PC). Current PC marked with `>>>`. |
| `cycles` | `cycles` | Show total CPU cycle count |
| `status` | `status` | Full system status dump (CPU + all devices) |
| `devices` | `devices` | List all MMIO devices with base address and size |

#### Memory & Register Modification

| Command | Syntax | Description |
|---------|--------|-------------|
| `setreg` | `setreg <reg> <value>` | Set a register (R0–R15, PC, SP) to a value |
| `setmem` | `setmem <addr> <bytes...>` | Write bytes to memory |
| `setmem` | `setmem <addr> "string"` | Write a string to memory |

#### Loading & Assembly

| Command | Syntax | Description |
|---------|--------|-------------|
| `load` | `load <file> [addr]` | Load a binary file into memory |
| `asm` | `asm <file> [addr]` | Assemble a `.asm` file and load the result |
| `asm` | `asm -e "code" [addr]` | Assemble inline code (`;` separates lines) |

#### Device Interaction

| Command | Syntax | Description |
|---------|--------|-------------|
| `send` | `send <text>` | Send text + newline to UART RX (simulates keyboard input) |
| `uart` | `uart` | Show UART buffer status (TX/RX counts, control register) |
| `storage` | `storage <sub>` | Storage subcommands: attach, create, detach, info, flush |
| `nic` | `nic <sub>` | NIC subcommands: show, inject, send, reset |
| `ramsize` | `ramsize [KiB]` | Show or change RAM size |

#### Exiting

| Command | Description |
|---------|-------------|
| `quit` / `exit` / `q` | Exit the monitor and emulator |

### Address Parsing

All commands that accept addresses understand:
- **Hexadecimal**: `0xFF00`, `0x100`
- **Decimal**: `256`, `1024`
- **Register names**: `r0`–`r15`, `pc`, `sp`

---

## Assembler

The assembler (`asm.py`) translates Megapad-64 assembly source into raw
machine code.  It's a classic **two-pass assembler** — pass 1 collects
labels and computes sizes, pass 2 emits bytes with resolved addresses.

### Usage

From the command line:

```bash
python cli.py --assemble source.asm output.rom
```

From the debug monitor:

```
MP64> asm source.asm 0x0
MP64> asm -e "ldi r1, 42; halt" 0x0
```

In inline mode (`-e`), semicolons separate lines.

### Directives

| Directive | Syntax | Description |
|-----------|--------|-------------|
| `.org` | `.org <address>` | Set the program counter. Pads with zeros to reach the address. |
| `.db` | `.db val, val, ...` | Emit 8-bit bytes (comma-separated) |
| `.dw` | `.dw val, val, ...` | Emit 16-bit words (little-endian) |
| `.dd` | `.dd val, val, ...` | Emit 32-bit doublewords (little-endian) |
| `.dq` | `.dq val, val, ...` | Emit 64-bit quadwords (little-endian). Supports label references. |
| `.ascii` | `.ascii "string"` | Emit raw string bytes (no null terminator) |
| `.asciiz` | `.asciiz "string"` | Emit string bytes + null terminator |

### String Escapes

`.ascii` and `.asciiz` support these escape sequences:

| Escape | Meaning |
|--------|---------|
| `\n` | Newline (0x0A) |
| `\r` | Carriage return (0x0D) |
| `\t` | Tab (0x09) |
| `\0` | Null (0x00) |
| `\\` | Literal backslash |
| `\"` | Literal double-quote |
| `\xHH` | Arbitrary hex byte |

### Labels

Labels are identifiers followed by a colon.  They record the current
program counter for use as branch targets or data references.

```asm
start:          ; define label "start" at current PC
    ldi r1, 0
    inc r1
    breq done   ; branch to "done" if zero
    lbr start   ; long branch back to "start"
done:
    halt
```

### Comments

Everything from `;` to end-of-line is a comment (respects quoted strings):

```asm
    ldi r1, 42    ; load the answer
    .ascii "hello; world"  ; semicolons in strings are preserved
```

### SKIP Pseudo-Instruction

`SKIP` is a conditional skip-next-instruction that emits 2 bytes (an EXT
prefix + a short branch with condition code).  It's useful for branchless
conditionals:

```asm
    cmpi r1, 10
    SKIPNE          ; skip next instruction if not equal
    ldi r2, 0xFF   ; this only executes when r1 == 10
    ; continues here either way
```

All standard condition codes work: `EQ`, `NE`, `CS`, `CC`, `MI`, `PL`,
`VS`, `VC`, `GT`, `LE`, `BQ`, `BNQ`, `SAT`, `EF`.  The unconditional
`SKIP` always skips.

### Supported Instructions

The assembler supports all 16 instruction families of the Megapad-64 ISA.
See the ISA Reference for the full instruction list.

| Family | Examples |
|--------|----------|
| SYS | `NOP`, `HALT`, `RET`, `RTI`, `CALL.L`, `TRAP`, `EI`, `DI` |
| INC/DEC | `INC R3`, `DEC R7` |
| BR/LBR | `BREQ label`, `LBRNE label`, `BR label` |
| MEM | `LDN R1`, `STR R2`, `LD.B R3, R4`, `ST.W R5, R6` |
| IMM | `LDI R1, 42`, `LDI64 R2, 0x123456789`, `ADDI R3, 10` |
| ALU | `ADD R1, R2`, `SUB R3, R4`, `CMP R5, R6`, `MOV R7, R8` |
| MULDIV | `MUL R1, R2`, `DIV R3, R4`, `UMOD R5, R6` |
| I/O | `OUT1 R1`, `INP3 R2` |
| CSR | `CSRR R1, 0x14`, `CSRW 0x15, R2` |
| MEX | `T.ADD`, `T.DOT`, `T.SUM`, `T.ZERO`, `T.TRANS` |
| EXT | `LDI64` (via prefix), `SKIP` conditions |

### How Two-Pass Assembly Works

**Pass 1 — Label Collection:**
1. Strip comments from each line
2. Record label addresses in a symbol table
3. Compute instruction sizes (each mnemonic has a known byte count)
4. Process directives (`.org` adjusts PC, `.db`/`.dw`/etc. advance by data size)

**Pass 2 — Byte Emission:**
1. Iterate lines again, emitting machine code bytes
2. Resolve label references in branch targets and immediates
3. Compute relative branch offsets from current PC
4. Range-check offsets (8-bit for `BR`, 16-bit for `LBR`)

---

## Disk Utility

`diskutil.py` creates and manages **MP64FS** disk images — 1 MiB
flat filesystems with a superblock, allocation bitmap, and 64-entry
directory.  See the Filesystem Specification for the on-disk format.

### Command-Line Subcommands

```bash
python diskutil.py <subcommand> [args]
```

| Subcommand | Syntax | Description |
|------------|--------|-------------|
| `create` | `create <file>` | Create a new, formatted 1 MiB disk image |
| `inject` | `inject <image> <file> [--type TYPE]` | Add a host file to the disk image |
| `list` | `list <image>` | List all files (name, size, type) |
| `read` | `read <image> <name>` | Read a file from the image, print to stdout |
| `delete` | `delete <image> <name>` | Remove a file from the image |
| `info` | `info <image>` | Show filesystem info (superblock, free space) |
| `sample` | `sample` | Build the standard `sample.img` with all content |

### File Type Flags

Use `--type` with the `inject` subcommand:

| Type Name | Code | Description |
|-----------|------|-------------|
| `raw` | 1 | Raw binary data |
| `text` | 2 | Plain text |
| `forth` | 3 | Forth source (loadable with `LOAD` in KDOS) |
| `doc` | 4 | Documentation topic (browsable with `DOC`) |
| `data` | 5 | Structured application data |
| `tutorial` | 6 | Tutorial/lesson (walkable with `TUTORIAL`) |
| `bundle` | 7 | Pipeline bundle (loadable with `BUNDLE-LOAD`) |

### Example Workflows

**Create a custom disk image:**

```bash
# Create an empty formatted image
python diskutil.py create myimage.img

# Add the KDOS source (BIOS auto-boots the first Forth-type file)
python diskutil.py inject myimage.img kdos.f --type forth

# Add a documentation file
python diskutil.py inject myimage.img mydoc.txt --type doc

# Verify
python diskutil.py list myimage.img
```

**Build the standard sample image:**

```bash
python diskutil.py sample
# Creates sample.img with:
#   kdos.f,
#   10 documentation topics,
#   5 tutorials,
#   demo-data file,
#   demo-bundle (pipeline bundle)
```

### Python API

The `MP64FS` class can be used programmatically:

```python
from diskutil import MP64FS

fs = MP64FS()
fs.format()
fs.inject("hello.f", b': greet ." Hello!" CR ;\n', file_type=3)

for entry in fs.list_files():
    print(entry['name'], entry['used_bytes'], 'bytes')

content = fs.read_file("hello.f")
fs.delete_file("hello.f")
fs.save("myimage.img")
```

### The Sample Image Contents

`build_sample_image()` populates the image with:

| File | Type | Description |
|------|------|-------------|
| `kdos.f` | Forth | Complete KDOS operating system source (auto-booted by BIOS) |
| `getting-started` | Doc | Introduction and first steps |
| `buffers` | Doc | Buffer subsystem guide |
| `kernels` | Doc | Kernel registry guide |
| `pipelines` | Doc | Pipeline guide |
| `data-ports` | Doc | Data ingestion guide |
| `scheduler` | Doc | Task scheduler guide |
| `screens` | Doc | Interactive TUI guide (8 screens) |
| `storage` | Doc | MP64FS & storage guide |
| `tile-engine` | Doc | Tile engine overview |
| `reference` | Doc | Quick reference card (with DESCRIBE) |
| `hello-world` | Tutorial | First Forth program |
| `first-kernel` | Tutorial | Creating a kernel |
| `build-pipeline` | Tutorial | Building a pipeline |
| `data-ingest` | Tutorial | Setting up data ports |
| `custom-kernel` | Tutorial | Writing custom kernels |
| `demo-data` | Data | 256-byte test data file |
| `demo-bundle` | Bundle | Demo pipeline bundle (load with `BUNDLE-LOAD`) |

---

## Test Suite

The project has a comprehensive test suite with **678+ passing tests**
that cover every layer of the system.

### Test Files

| File | Tests | What It Covers |
|------|-------|----------------|
| `test_megapad64.py` | ~65 | CPU instruction set — all 16 families, integration tests (Fibonacci, subroutines, stack) |
| `test_system.py` | ~615 | Everything else — devices, MMIO, BIOS words, KDOS features, assembler, diskutil, filesystem, hardening |

### Test Classes in `test_system.py`

| Class | Coverage Area |
|-------|--------------|
| `TestUART` | UART TX callback, RX inject, TX drain |
| `TestTimer` | Timer tick counting |
| `TestDisk` | Disk image create, read/write sectors, no-image handling |
| `TestMMIO` | MMIO routing to UART and SystemInfo devices |
| `TestNIC` | NIC: MAC, frame inject/recv, DMA, reset, counters, MTU |
| `TestMMIOCPU` | CPU-level MMIO: UART TX, SystemInfo read, RAM passthrough |
| `TestBios` | All BIOS Forth words — arithmetic, stack, comparisons, memory, control flow, strings, variables, constants, colon definitions, I/O, timer, return stack, loops, CREATE/DOES>, EVALUATE, and more |
| `TestBIOSHardening` | FSLOAD edge cases (multi-sector, colon defs, dot-quote, nested evaluate, empty, comments-only, long line), error line context, stack underflow detection, EVALUATE depth limit, dictionary-full guard |
| `TestBranchRange` | Assembler branch range validation, SKIP instruction |
| `TestKDOS` | KDOS: buffers, kernels, pipelines, tasks, data ports, NIC ingestion, documentation browser, scheduler, tile engine, advanced kernels, TUI screens, filesystem commands, benchmarking |
| `TestKDOSHardening` | SCREENS TUI renders (all 8 screens), header/footer verification, disk-only boot end-to-end tests |
| `TestDiskUtil` | `diskutil.py`: format, inject, read, list, delete, sample image builder |
| `TestKDOSFilesystem` | End-to-end KDOS FS: FORMAT, MKFILE, DIR, CATALOG, CAT, RMFILE, RENAME, FWRITE/FREAD, FS-FREE, SAVE-BUFFER, DOC/DESCRIBE/TOPICS/LESSONS |

### Running Tests

```bash
# Full suite (recommended)
python -m pytest test_system.py test_megapad64.py -v --timeout=30

# Just KDOS tests
python -m pytest test_system.py -k "TestKDOS" --timeout=30

# Just BIOS tests
python -m pytest test_system.py -k "TestBios" --timeout=30

# CPU unit tests only
python -m pytest test_megapad64.py -v

# A specific test
python -m pytest test_system.py -k "test_buffer_create" -v

# With short tracebacks
python -m pytest test_system.py --tb=short --timeout=30
```

### Test Infrastructure

The test files include several helper functions:

| Helper | Description |
|--------|-------------|
| `make_cpu()` | Create a fresh `MegaPad64` CPU instance |
| `run_until_halt()` | Execute until the CPU halts or a step limit is reached |
| `capture_output()` | Attach an output capture callback to the UART; returns a byte list |
| `bytes_to_str()` | Convert a captured byte buffer to a printable string |

**KDOS snapshot caching:** The `TestKDOS` class uses pickle-based
snapshots (`.kdos_snapshot_*.pkl`) to avoid re-loading the entire KDOS
Forth source for every test method.  The first test in the class pays
the boot cost; subsequent tests restore from the cached snapshot.

---

## Project File Summary

| File | Lines | Purpose |
|------|-------|---------|
| `megapad64.py` | ~1,358 | CPU + tile engine emulator |
| `system.py` | ~300 | System integration (CPU + devices + memory map) |
| `cli.py` | ~992 | CLI, boot modes, debug monitor |
| `asm.py` | ~748 | Two-pass assembler (with listing output) |
| `devices.py` | ~720 | MMIO devices (UART, Timer, Storage, SystemInfo, NIC) |
| `datasources.py` | ~700 | Simulated data sources for NIC |
| `diskutil.py` | ~941 | MP64FS disk utility and image builder |
| `bios.asm` | ~8,287 | Forth BIOS (197 dictionary words, hardened) |
| `bios.rom` | 20,605B | Pre-assembled BIOS binary |
| `kdos.f` | ~2,778 | KDOS operating system (344 definitions) |
| `test_megapad64.py` | ~712 | CPU unit tests |
| `test_system.py` | ~5,258 | Integration test suite |
