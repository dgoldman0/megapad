# Megapad-64 System Emulator

A complete system-level emulator for the Megapad-64 architecture, including
CPU, memory-mapped I/O peripherals, a BIOS, and an interactive CLI debugger.

> **Status:** Core emulation is functional. No operating system, no graphics
> subsystem — just bare-metal hardware emulation and a monitor shell.

---

## Quick Start

```bash
# Boot with the BIOS (assembles from source automatically)
python cli.py --bios bios.asm

# Then inside the monitor:
MP64> boot
MP64> run
```

You'll see the BIOS banner, a storage probe, and an interactive `> ` prompt
from the BIOS shell running *inside* the emulated machine.

### Other launch examples

```bash
# Custom RAM size (512 KiB) + disk image
python cli.py --ram 512 --storage disk.img --bios bios.asm

# Load a raw binary at a specific address
python cli.py --load program.bin@0x1000

# Auto-boot and run immediately
python cli.py --bios bios.asm --run
```

---

## Architecture Overview

```
┌────────────────────────────────────────────────────────┐
│                     cli.py                             │
│            Interactive Monitor / Debugger               │
└────────────────────┬───────────────────────────────────┘
                     │
┌────────────────────▼───────────────────────────────────┐
│                   system.py                            │
│        MegapadSystem  (unified memory map)             │
│                                                        │
│   ┌───────────┐    ┌───────────────────────────────┐   │
│   │ megapad64  │    │         devices.py            │   │
│   │   CPU core │    │  ┌──────┐ ┌─────┐ ┌───────┐  │   │
│   │ 16×64-bit  │◄──►│  │ UART │ │Timer│ │Storage│  │   │
│   │   GPRs     │    │  └──────┘ └─────┘ └───────┘  │   │
│   │ full ISA   │    │  ┌────────┐                   │   │
│   └───────────┘    │  │SysInfo │   DeviceBus        │   │
│                     │  └────────┘                   │   │
│                     └───────────────────────────────┘   │
└────────────────────────────────────────────────────────┘
```

### File Inventory

| File | Lines | Role |
|---|---|---|
| `megapad64.py` | ~1300 | CPU core — 16×64-bit registers, all 16 instruction families, flags, CSRs, traps |
| `asm.py` | ~590 | Two-pass assembler — all mnemonics including `ldi64` for 64-bit immediates |
| `devices.py` | ~450 | Peripheral layer — UART, Timer, Storage, SystemInfo, DeviceBus |
| `system.py` | ~275 | System glue — wires CPU + DeviceBus, patches memory accessors for MMIO |
| `cli.py` | ~745 | Interactive CLI monitor with disassembler, breakpoints, UART console |
| `bios.asm` | ~310 | Bootstrap firmware — UART I/O, storage probe, interactive shell |
| `test_megapad64.py` | ~710 | CPU test suite (61 tests) |
| `test_system.py` | ~400 | System integration tests (23 tests) |

---

## Memory Map

| Address Range | Size | Description |
|---|---|---|
| `0x0000_0000_0000_0000` – top of RAM | Configurable | RAM (default 1 MiB) |
| `0xFFFF_FF00_0000_0000` – `+0x00FF` | 256 B | UART (serial console) |
| `0xFFFF_FF00_0000_0100` – `+0x01FF` | 256 B | Timer |
| `0xFFFF_FF00_0000_0200` – `+0x02FF` | 256 B | Storage controller |
| `0xFFFF_FF00_0000_0300` – `+0x03FF` | 256 B | System info |

All MMIO registers are byte-accessed. The system layer intercepts any CPU
memory operation (8/16/32/64-bit) that falls in the MMIO range and routes
it through the device bus; everything else hits RAM.

---

## Peripherals

### UART (Serial Console)

The primary I/O device. Connects the emulated CPU to the host terminal.

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

Sector-based block device backed by a host file.

| Offset | Name | R/W | Description |
|---|---|---|---|
| `+0x00` | CMD | W | `0x01` READ, `0x02` WRITE, `0x03` STATUS, `0xFF` FLUSH |
| `+0x01` | STATUS | R | bit 0: busy, bit 1: error, bit 7: present |
| `+0x02`–`+0x05` | SECTOR | RW | 32-bit sector number |
| `+0x06`–`+0x0D` | DMA_ADDR | RW | 64-bit DMA address in RAM |
| `+0x0E` | SEC_COUNT | RW | Number of sectors to transfer |
| `+0x0F` | DATA | RW | Byte-at-a-time data port |

Sector size is **512 bytes**. DMA transfers read/write directly to/from RAM,
bypassing MMIO.

### System Info

Read-only board identification.

| Offset | Name | Description |
|---|---|---|
| `+0x00`–`+0x03` | BOARD_ID | "MP64" (ASCII) |
| `+0x04` | VERSION | Board revision |
| `+0x05`–`+0x06` | MEM_SIZE | RAM size in KiB (little-endian 16-bit) |
| `+0x07` | FEATURES | bit 0: UART, bit 1: storage present |
| `+0x08` | STORAGE | `1` if storage attached, else `0` |

---

## CLI Monitor Commands

Launch the monitor with `python cli.py [options]`. All commands are
available at the `MP64>` prompt.

### Loading

| Command | Description |
|---|---|
| `load <file> [addr]` | Load a raw binary into RAM (default address 0) |
| `asm <file.asm> [addr]` | Assemble and load (source → machine code → RAM) |
| `asm -e "ldi r1, 42; halt"` | Inline assembly (semicolons separate lines) |

### Execution

| Command | Description |
|---|---|
| `boot [addr]` | Cold boot: reset CPU, set PC to addr (default 0) |
| `reset` | Reset CPU state without clearing RAM |
| `step [N]` | Execute N instructions (default 1), show state |
| `run [max]` | Run up to max steps (default 1M), stop on halt/idle/breakpoint |
| `continue` / `c` | Resume execution (alias for `run`) |

### Breakpoints

| Command | Description |
|---|---|
| `bp <addr>` | Set breakpoint at address |
| `bp` | List all breakpoints |
| `bpd <addr>` | Delete breakpoint |
| `bpd all` | Delete all breakpoints |

### Inspection

| Command | Description |
|---|---|
| `regs` | Dump all 16 registers + PC, SP, D, flags |
| `flags` | Show CPU flags (Z, C, N, V, P, G, I, S) |
| `dump <addr> [len]` | Hex dump of memory (default 256 bytes) |
| `disasm [addr] [count]` | Disassemble instructions (default from PC) |
| `status` | Full system status dump |
| `devices` | List registered MMIO devices |
| `cycles` | Show total CPU cycle count |

### Modification

| Command | Description |
|---|---|
| `setreg <reg> <value>` | Set a register (`setreg r1 0xFF`, `setreg pc 0x100`) |
| `setmem <addr> <hex bytes>` | Write bytes to memory (`setmem 0x100 48 65 6C`) |

### UART / Console

| Command | Description |
|---|---|
| `console` | Enter interactive console mode (Ctrl-] to exit) |
| `send <text>` | Inject text into UART RX buffer as keyboard input |
| `uart` | Show UART buffer status |

In **console mode**, your keystrokes go directly to the emulated UART and
CPU output appears in real time — just like a real serial terminal.

### Storage

| Command | Description |
|---|---|
| `storage attach <file>` | Attach (or create) a disk image |
| `storage detach` | Detach current image |
| `storage info` | Show image path, sector count, status |
| `storage save` | Flush image to disk |

### Configuration

| Command | Description |
|---|---|
| `ramsize [KiB]` | Show or change RAM size (recreates system) |
| `quit` / `exit` / `q` | Exit the monitor |

---

## BIOS

The included `bios.asm` is a minimal bootstrap that runs from address 0.
It is written in Megapad-64 assembly and assembled automatically by the
CLI when you use `--bios bios.asm`.

### Boot Sequence

1. Initialize stack pointer (R15 ← top of RAM)
2. Set up UART base address in R8
3. Load subroutine pointers into R4 (print_str), R5 (read_char), R6 (print_hex_byte)
4. Print banner: `Megapad-64 BIOS v0.1`
5. Probe storage via SysInfo, print `Storage: Y` or `Storage: N`
6. Print `Ready.` and enter the interactive shell

### BIOS Shell Commands

| Key | Action |
|---|---|
| `h` | Print help |
| `r` | Print PC and R9 as hex |
| `d` | Hex dump 16 bytes starting at R9 |
| `s` | Set R9 from two hex digits (e.g., `s4A` sets R9 = 0x4A) |
| `g` | Jump to address in R9 (CALL.L — returns to shell on RET.L) |
| `q` | Halt the CPU |

### Register Conventions

| Register | Role |
|---|---|
| R3 | Program counter (PSEL = 3) |
| R2 | Data pointer (XSEL = 2) |
| R8 | UART base address |
| R9 | User address (for dump/go/set) |
| R4 | → `print_str` subroutine |
| R5 | → `read_char` subroutine |
| R6 | → `print_hex_byte` subroutine |
| R15 | Stack pointer (SPSEL = 15) |

---

## Assembler Notes

The assembler (`asm.py`) supports the full ISA. A few things to keep in
mind when writing programs:

- **`ldi64 Rn, value`** — loads a full 64-bit immediate (uses EXT prefix +
  8-byte literal). Required for MMIO addresses.
- **`br` vs `lbr`** — short branches (`br`) have ±127 byte range. Use `lbr`
  (±32K) for longer jumps. The assembler will error if a short branch
  target is out of range.
- **R3 is the PC** — avoid using it as a general-purpose register.
- **CALL.L / RET.L** — 64-bit call/return via the stack. Load the target
  address into a register, then `call.l Rn`.
- **Carry flag convention** — CMP sets C = 1 when the first operand is ≥
  the second (unsigned, no-borrow convention). Use `brcc` to branch when
  less-than, `brcs` when greater-or-equal.

---

## Running Tests

```bash
# CPU-level tests (61 tests)
python test_megapad64.py

# System integration tests (23 tests)
python test_system.py
```

Both suites should report all tests passing.

---

## Example Session

```
$ python cli.py --bios bios.asm --ram 256

╔══════════════════════════════════════════════════════════╗
║          Megapad-64 System Monitor  v1.0                ║
║   Type 'help' for commands.  'quit' to exit.           ║
╚══════════════════════════════════════════════════════════╝
Assembled BIOS from 'bios.asm': 552 bytes
MP64> boot
  Booted. PC=0x0  SP=0x40000
MP64> run

Megapad-64 BIOS v0.1
Storage: N

Ready.
> 
  CPU idle after 112490 steps (waiting for input).
MP64> send h
MP64> run
h
h=help r=regs d=dump s=setaddr g=go q=quit
> 
  CPU idle after ... steps (waiting for input).
MP64> send q
MP64> run
q
Bye!
  CPU halted after 60 steps.
MP64> regs
  R0  = 0x0000000000000000   R1  = 0x0000000000000071
  R2  = 0x0000000000040000   R3  = 0x000000000000015f  <PC
  ...
MP64> quit
Goodbye.
```

---

## What's Next

This emulator provides the bare-metal foundation. Future work may include:

- **Operating system** — kernel, process management, syscalls
- **Graphics subsystem** — framebuffer device, display emulation
- **GUI frontend** — graphical debugger and display window
- **Keyboard / mouse** — additional input peripherals
- **Network** — emulated NIC
- **Extended storage** — filesystem support
