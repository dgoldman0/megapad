#!/usr/bin/env python3
"""
Megapad-64 System Monitor / CLI
=================================
Interactive command-line interface for the Megapad-64 system emulator.

Provides:
  - Machine configuration (RAM size, storage image)
  - Binary / assembly loading
  - Run / step / breakpoint execution
  - Register and memory inspection / modification
  - UART console passthrough
  - Disassembly
  - Storage management

Usage:
  python cli.py [--ram SIZE_KIB] [--storage IMAGE] [--load FILE@ADDR] [--bios FILE]
"""

from __future__ import annotations
import argparse
import cmd
import os
import sys
import readline
import shlex
import traceback
from typing import Optional

from megapad64 import Megapad64, HaltError, TrapError, u64, s64
from asm import assemble, AsmError
from system import MegapadSystem, MMIO_START
from devices import (
    MMIO_BASE, UART_BASE, TIMER_BASE, STORAGE_BASE, SYSINFO_BASE,
    SECTOR_SIZE,
)

# ---------------------------------------------------------------------------
#  Disassembler (basic — enough for debugging)
# ---------------------------------------------------------------------------

SYS_NAMES = {
    0: "IDL", 1: "NOP", 2: "HALT", 3: "RESET", 4: "RTI", 5: "RET",
    6: "DIS", 7: "MARK", 8: "SAV", 9: "SEQ", 0xA: "REQ", 0xB: "EI",
    0xC: "DI", 0xD: "CALL.L", 0xE: "RET.L", 0xF: "TRAP",
}

COND_NAMES = {
    0: "", 1: "EQ", 2: "NE", 3: "CS", 4: "CC", 5: "MI", 6: "PL",
    7: "VS", 8: "VC", 9: "GT", 0xA: "LE", 0xB: "BQ", 0xC: "BNQ",
    0xD: "SAT", 0xE: "EF", 0xF: "NV",
}

MEM_NAMES = {
    0: "LDN", 1: "LDA", 2: "LDX", 3: "LDXA", 4: "STR", 5: "STXD",
    6: "LD.B", 7: "ST.B", 8: "LD.H", 9: "ST.H", 0xA: "LD.W", 0xB: "ST.W",
    0xC: "LD.SB", 0xD: "LD.SH", 0xE: "LD.SW", 0xF: "LD.D",
}

ALU_NAMES = {
    0: "ADD", 1: "ADC", 2: "SUB", 3: "SBB", 4: "AND", 5: "OR",
    6: "XOR", 7: "CMP", 8: "MOV", 9: "NOT", 0xA: "NEG", 0xB: "SHL",
    0xC: "SHR", 0xD: "SAR", 0xE: "ROL", 0xF: "ROR",
}

MEMALU_NAMES = {
    0: "LDX", 1: "OR.X", 2: "AND.X", 3: "XOR.X", 4: "ADD.X", 5: "SD.X",
    6: "SHR.D", 7: "SM.X", 8: "ADC.X", 9: "SDB.X", 0xA: "SHRC.D",
    0xB: "SMB.X", 0xC: "SHL.D", 0xD: "SHLC.D", 0xE: "IRX", 0xF: "LDXA.D",
}

IMM_NAMES = {
    0: "LDI", 1: "LHI", 2: "ADDI", 3: "ANDI", 4: "ORI", 5: "XORI",
    6: "CMPI", 7: "SUBI", 8: "LSLI", 9: "LSRI", 0xA: "ASRI", 0xB: "ROLI",
    0xC: "GLO", 0xD: "GHI", 0xE: "PLO", 0xF: "PHI",
}

MULDIV_NAMES = {
    0: "MUL", 1: "MULH", 2: "UMUL", 3: "UMULH",
    4: "DIV", 5: "UDIV", 6: "MOD", 7: "UMOD",
}


def disasm_one(mem: bytearray | bytes, addr: int, mem_size: int) -> tuple[str, int]:
    """Disassemble one instruction at `addr`. Returns (text, byte_count)."""
    def rb(a):
        return mem[a % mem_size] if (a % mem_size) < len(mem) else 0

    b0 = rb(addr)
    f = (b0 >> 4) & 0xF
    n = b0 & 0xF

    if f == 0xF:
        # EXT prefix
        b1 = rb(addr + 1)
        f2 = (b1 >> 4) & 0xF
        n2 = b1 & 0xF
        inner, inner_len = _disasm_core(mem, addr + 1, mem_size, f2, n2)
        return f"EXT.{n} {inner}", 1 + inner_len

    return _disasm_core(mem, addr, mem_size, f, n)


def _disasm_core(mem, addr, mem_size, f, n) -> tuple[str, int]:
    def rb(a):
        return mem[a % mem_size] if (a % mem_size) < len(mem) else 0

    if f == 0x0:
        name = SYS_NAMES.get(n, f"SYS.{n:#x}")
        if n == 0xD:  # CALL.L Rn
            b1 = rb(addr + 1)
            return f"CALL.L R{b1 & 0xF}", 2
        return name, 1

    elif f == 0x1:
        return f"INC R{n}", 1

    elif f == 0x2:
        return f"DEC R{n}", 1

    elif f == 0x3:
        off = rb(addr + 1)
        if off & 0x80:
            off -= 256
        cc = COND_NAMES.get(n, f"?{n}")
        return f"BR{cc} {off:+d}", 2

    elif f == 0x4:
        hi = rb(addr + 1)
        lo = rb(addr + 2)
        off = (hi << 8) | lo
        if off & 0x8000:
            off -= 0x10000
        cc = COND_NAMES.get(n, f"?{n}")
        return f"LBR{cc} {off:+d}", 3

    elif f == 0x5:
        name = MEM_NAMES.get(n, f"MEM.{n:#x}")
        b1 = rb(addr + 1)
        rd = (b1 >> 4) & 0xF
        rs = b1 & 0xF
        if n == 0xF:  # LD.D Rd, [Rn+off8]
            off = rb(addr + 2)
            if off & 0x80:
                off -= 256
            return f"LD.D R{rd}, [R{rs}+{off}]", 3
        return f"{name} R{rd}, R{rs}", 2

    elif f == 0x6:
        name = IMM_NAMES.get(n, f"IMM.{n:#x}")
        b1 = rb(addr + 1)
        rn = (b1 >> 4) & 0xF
        if n <= 0x7:
            if n == 0x1:  # LHI
                lo = rb(addr + 2)
                hi = rb(addr + 3)
                return f"LHI R{rn}, {(hi << 8) | lo:#06x}", 4
            else:
                imm = rb(addr + 2)
                return f"{name} R{rn}, {imm:#04x}", 3
        elif n <= 0xB:
            imm4 = b1 & 0xF
            return f"{name} R{rn}, {imm4}", 2
        else:
            return f"{name} R{rn}", 2

    elif f == 0x7:
        name = ALU_NAMES.get(n, f"ALU.{n:#x}")
        b1 = rb(addr + 1)
        rd = (b1 >> 4) & 0xF
        rs = b1 & 0xF
        return f"{name} R{rd}, R{rs}", 2

    elif f == 0x8:
        name = MEMALU_NAMES.get(n, f"MEMALU.{n:#x}")
        return name, 1

    elif f == 0x9:
        if 1 <= n <= 7:
            return f"OUT{n}", 1
        elif 9 <= n <= 15:
            return f"INP{n - 8}", 1
        return f"IO.{n:#x}", 1

    elif f == 0xA:
        return f"SEP R{n}", 1

    elif f == 0xB:
        return f"SEX R{n}", 1

    elif f == 0xC:
        name = MULDIV_NAMES.get(n, f"MULDIV.{n:#x}")
        b1 = rb(addr + 1)
        rd = (b1 >> 4) & 0xF
        rs = b1 & 0xF
        return f"{name} R{rd}, R{rs}", 2

    elif f == 0xD:
        w = (n >> 3) & 1
        reg = n & 0x7
        csr_addr = rb(addr + 1)
        if w == 0:
            return f"CSRR R{reg}, {csr_addr:#04x}", 2
        else:
            return f"CSRW {csr_addr:#04x}, R{reg}", 2

    elif f == 0xE:
        ss = (n >> 2) & 3
        op = n & 3
        funct = rb(addr + 1) & 0x07
        op_names = {0: "TALU", 1: "TMUL", 2: "TRED", 3: "TSYS"}
        name = op_names.get(op, f"MEX.{op}")
        if ss == 1:
            reg = rb(addr + 2) & 0xF
            return f"{name}.{funct} R{reg}", 3
        return f"{name}.{funct}", 2

    return f"??? {rb(addr):#04x}", 1


# ---------------------------------------------------------------------------
#  CLI
# ---------------------------------------------------------------------------

class MegapadCLI(cmd.Cmd):
    """Interactive monitor for the Megapad-64 system."""

    intro = (
        "\n"
        "╔══════════════════════════════════════════════════════════╗\n"
        "║          Megapad-64 System Monitor  v1.0                 ║\n"
        "║   Type 'help' for commands.  'quit' to exit.             ║\n"
        "╚══════════════════════════════════════════════════════════╝\n"
    )
    prompt = "MP64> "

    def __init__(self, system: MegapadSystem):
        super().__init__()
        self.sys = system
        self.breakpoints: set[int] = set()
        self._console_mode = False

        # Wire UART TX to print in real time during run
        self.sys.uart.on_tx = self._uart_tx_handler

    def _uart_tx_handler(self, byte_val: int):
        """Print UART output to the host terminal in real time."""
        ch = chr(byte_val) if 0x20 <= byte_val < 0x7F or byte_val in (10, 13, 9) else '.'
        print(ch, end='', flush=True)

    # -- Parsing helpers --

    def _parse_addr(self, s: str) -> int:
        """Parse an address (hex with optional 0x prefix, or register name)."""
        s = s.strip().lower()
        if s.startswith("r") and s[1:].isdigit():
            return self.sys.cpu.regs[int(s[1:])]
        if s == "pc":
            return self.sys.cpu.pc
        if s == "sp":
            return self.sys.cpu.sp
        return int(s, 0)

    def _parse_int(self, s: str) -> int:
        return int(s.strip(), 0)

    # ================================================================
    #  Commands
    # ================================================================

    # -- Loading --

    def do_load(self, arg):
        """Load a binary file: load <file> [address]
        Address defaults to 0."""
        parts = shlex.split(arg)
        if not parts:
            print("Usage: load <file> [address]")
            return
        path = parts[0]
        addr = self._parse_addr(parts[1]) if len(parts) > 1 else 0
        try:
            with open(path, "rb") as f:
                data = f.read()
            self.sys.load_binary(addr, data)
            print(f"Loaded {len(data)} bytes from '{path}' at {addr:#x}")
        except Exception as e:
            print(f"Error: {e}")

    def do_asm(self, arg):
        """Assemble source and load: asm <file.asm> [address]
        Or inline:  asm -e "ldi r1, 42; halt" [address]"""
        parts = shlex.split(arg)
        if not parts:
            print("Usage: asm <file.asm> [addr]  OR  asm -e \"code\" [addr]")
            return

        if parts[0] == "-e":
            source = parts[1].replace(";", "\n") if len(parts) > 1 else ""
            addr = self._parse_addr(parts[2]) if len(parts) > 2 else 0
        else:
            path = parts[0]
            addr = self._parse_addr(parts[1]) if len(parts) > 1 else 0
            try:
                with open(path, "r") as f:
                    source = f.read()
            except Exception as e:
                print(f"Error reading '{path}': {e}")
                return

        try:
            code = assemble(source, addr)
            self.sys.load_binary(addr, code)
            print(f"Assembled {len(code)} bytes at {addr:#x}")
        except AsmError as e:
            print(f"Assembly error: {e}")
        except Exception as e:
            print(f"Error: {e}")

    # -- Storage --

    def do_storage(self, arg):
        """Storage commands:
          storage attach <image_file>   — attach/create a disk image
          storage detach                — detach current image
          storage info                  — show storage status
          storage save                  — flush image to disk"""
        parts = shlex.split(arg)
        if not parts:
            print("Usage: storage attach|detach|info|save [args]")
            return
        sub = parts[0].lower()
        if sub == "attach":
            if len(parts) < 2:
                print("Usage: storage attach <image_file>")
                return
            self.sys.storage.load_image(parts[1])
            self.sys.sysinfo.has_storage = True
            print(f"Storage attached: {parts[1]} ({self.sys.storage.total_sectors} sectors)")
        elif sub == "detach":
            self.sys.storage._image_data = bytearray()
            self.sys.storage.image_path = None
            self.sys.storage.status = 0
            self.sys.sysinfo.has_storage = False
            print("Storage detached.")
        elif sub == "info":
            s = self.sys.storage
            print(f"  Image: {s.image_path or 'none'}")
            print(f"  Sectors: {s.total_sectors}  ({s.total_sectors * SECTOR_SIZE} bytes)")
            print(f"  Present: {'yes' if s.status & 0x80 else 'no'}")
        elif sub == "save":
            self.sys.storage.save_image()
            print("Storage image saved.")
        else:
            print(f"Unknown storage command: {sub}")

    # -- Boot / Reset --

    def do_boot(self, arg):
        """Boot the system: boot [entry_address]
        Resets CPU, sets PC to entry address (default 0)."""
        addr = self._parse_addr(arg) if arg.strip() else 0
        self.sys.boot(addr)
        print(f"System booted. PC={addr:#x}, SP={self.sys.cpu.sp:#x}")

    def do_reset(self, arg):
        """Hard reset the CPU (same as boot 0)."""
        self.sys.boot(0)
        print("System reset.")

    # -- Execution --

    def do_step(self, arg):
        """Step N instructions: step [count]"""
        count = self._parse_int(arg) if arg.strip() else 1
        for _ in range(count):
            if self.sys.cpu.halted:
                print("CPU is halted.")
                break
            try:
                addr_before = self.sys.cpu.pc
                cycles = self.sys.step()
                text, _ = disasm_one(self.sys.cpu.mem, addr_before, self.sys.cpu.mem_size)
                print(f"  {addr_before:#010x}: {text}  ({cycles} cyc)")
            except HaltError:
                print("CPU halted.")
                break
            except TrapError as e:
                if self.sys.cpu.ivt_base != 0:
                    self.sys.cpu._trap(e.ivec_id)
                    print(f"Trap → IVEC {e.ivec_id:#x}")
                else:
                    print(f"Unhandled trap: {e}")
                    break

    def do_run(self, arg):
        """Run until halt/idle/breakpoint: run [max_steps]"""
        max_steps = self._parse_int(arg) if arg.strip() else 1_000_000
        total = 0
        for _ in range(max_steps):
            if self.sys.cpu.halted:
                print(f"\nCPU halted after {total} cycles.")
                break
            if self.sys.cpu.idle:
                if self.sys.uart.has_rx_data:
                    self.sys.cpu.idle = False
                else:
                    print(f"\nCPU idle after {total} cycles (waiting for input).")
                    print("  Use 'send <text>' to provide input, then 'run' to continue.")
                    break
            pc = self.sys.cpu.pc
            if pc in self.breakpoints:
                print(f"\nBreakpoint hit at {pc:#010x}")
                break
            try:
                total += self.sys.step()
            except HaltError:
                print(f"\nCPU halted after {total} cycles.")
                break
            except TrapError as e:
                if self.sys.cpu.ivt_base != 0:
                    self.sys.cpu._trap(e.ivec_id)
                else:
                    print(f"\nUnhandled trap: {e}")
                    break
        else:
            print(f"\nStopped after {max_steps} steps ({total} cycles).")

    def do_continue(self, arg):
        """Alias for 'run'."""
        self.do_run(arg)
    do_c = do_continue

    # -- Console mode --

    def do_console(self, arg):
        """Enter UART console mode. Type Ctrl-] to return to monitor.
        Characters you type are sent to the CPU's UART RX buffer."""
        print("=== UART Console (Ctrl-] to exit) ===")
        import termios, tty
        old = termios.tcgetattr(sys.stdin)
        try:
            tty.setraw(sys.stdin.fileno())
            self._console_mode = True
            while self._console_mode:
                # Run a batch of instructions
                if not self.sys.cpu.halted:
                    for _ in range(1000):
                        if self.sys.cpu.halted:
                            break
                        if self.sys.cpu.idle:
                            if self.sys.uart.has_rx_data:
                                self.sys.cpu.idle = False
                            else:
                                self.sys.bus.tick(1)
                                break
                        try:
                            self.sys.step()
                        except (HaltError, TrapError):
                            break

                # Check for user input (non-blocking)
                import select
                if select.select([sys.stdin], [], [], 0.01)[0]:
                    ch = sys.stdin.read(1)
                    if ch == '\x1d':  # Ctrl-]
                        break
                    self.sys.uart.inject_input(ch)
        finally:
            termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old)
            self._console_mode = False
            print("\n=== Back to monitor ===")

    # -- Breakpoints --

    def do_bp(self, arg):
        """Set breakpoint: bp <address>"""
        if not arg.strip():
            if self.breakpoints:
                print("Breakpoints:")
                for a in sorted(self.breakpoints):
                    print(f"  {a:#010x}")
            else:
                print("No breakpoints set.")
            return
        addr = self._parse_addr(arg)
        self.breakpoints.add(addr)
        print(f"Breakpoint set at {addr:#010x}")

    def do_bpd(self, arg):
        """Delete breakpoint: bpd <address|all>"""
        if arg.strip().lower() == "all":
            self.breakpoints.clear()
            print("All breakpoints cleared.")
            return
        addr = self._parse_addr(arg)
        self.breakpoints.discard(addr)
        print(f"Breakpoint at {addr:#010x} removed.")

    # -- Inspection --

    def do_regs(self, arg):
        """Show CPU registers."""
        print(self.sys.cpu.dump_regs())
        print(f"  Cycles: {self.sys.cpu.cycle_count}")

    def do_flags(self, arg):
        """Show CPU flags."""
        c = self.sys.cpu
        print(f"  Z={c.flag_z} C={c.flag_c} N={c.flag_n} V={c.flag_v} "
              f"P={c.flag_p} G={c.flag_g} I={c.flag_i} S={c.flag_s}")
        print(f"  D={c.d_reg:#04x}  Q={c.q_out}  T={c.t_reg:#04x}")

    def do_setreg(self, arg):
        """Set register: setreg <R0-R15|pc|sp> <value>"""
        parts = shlex.split(arg)
        if len(parts) < 2:
            print("Usage: setreg <reg> <value>")
            return
        reg_s = parts[0].lower()
        val = self._parse_int(parts[1])
        if reg_s == "pc":
            self.sys.cpu.pc = val
        elif reg_s == "sp":
            self.sys.cpu.sp = val
        elif reg_s.startswith("r") and reg_s[1:].isdigit():
            idx = int(reg_s[1:])
            if 0 <= idx <= 15:
                self.sys.cpu.regs[idx] = u64(val)
            else:
                print("Register must be R0-R15.")
                return
        else:
            print("Unknown register.")
            return
        print(f"  {reg_s.upper()} = {u64(val):#018x}")

    def do_dump(self, arg):
        """Hex dump memory: dump <address> [count]
        Count defaults to 128 bytes."""
        parts = shlex.split(arg)
        if not parts:
            print("Usage: dump <address> [count]")
            return
        addr = self._parse_addr(parts[0])
        count = self._parse_int(parts[1]) if len(parts) > 1 else 128

        for row_start in range(addr, addr + count, 16):
            hex_bytes = []
            ascii_chars = []
            for i in range(16):
                if row_start + i < addr + count:
                    b = self.sys.cpu.mem_read8(row_start + i)
                    hex_bytes.append(f"{b:02x}")
                    ascii_chars.append(chr(b) if 0x20 <= b < 0x7F else '.')
                else:
                    hex_bytes.append("  ")
                    ascii_chars.append(' ')
            hex_str = ' '.join(hex_bytes[:8]) + '  ' + ' '.join(hex_bytes[8:])
            print(f"  {row_start:#010x}: {hex_str}  |{''.join(ascii_chars)}|")

    def do_setmem(self, arg):
        """Set memory byte: setmem <address> <byte> [byte] ...
        Or: setmem <address> "string" """
        parts = shlex.split(arg)
        if len(parts) < 2:
            print("Usage: setmem <addr> <byte...> OR setmem <addr> \"string\"")
            return
        addr = self._parse_addr(parts[0])
        # Check if second arg is a quoted string
        if parts[1].startswith('"') or parts[1].startswith("'"):
            data = parts[1].encode("ascii", errors="replace")
            for i, b in enumerate(data):
                self.sys.cpu.mem_write8(addr + i, b)
            print(f"  Wrote {len(data)} bytes at {addr:#x}")
        else:
            for i, tok in enumerate(parts[1:]):
                self.sys.cpu.mem_write8(addr + i, self._parse_int(tok) & 0xFF)
            print(f"  Wrote {len(parts) - 1} bytes at {addr:#x}")

    def do_disasm(self, arg):
        """Disassemble: disasm [address] [count]
        Defaults to current PC, 16 instructions."""
        parts = shlex.split(arg)
        addr = self._parse_addr(parts[0]) if parts else self.sys.cpu.pc
        count = self._parse_int(parts[1]) if len(parts) > 1 else 16

        for _ in range(count):
            text, size = disasm_one(self.sys.cpu.mem, addr, self.sys.cpu.mem_size)
            # Show raw bytes
            raw = ' '.join(f"{self.sys.cpu.mem_read8(addr + i):02x}" for i in range(size))
            marker = ">>>" if addr == self.sys.cpu.pc else "   "
            print(f"  {marker} {addr:#010x}: {raw:<12s} {text}")
            addr += size

    def do_status(self, arg):
        """Show full system status (CPU + devices)."""
        print(self.sys.dump_state())

    def do_devices(self, arg):
        """Show device status."""
        for dev in self.sys.bus.devices:
            print(f"  [{dev.name}] base={dev.base:#06x} size={dev.size}")

    # -- Configuration --

    def do_ramsize(self, arg):
        """Show or set RAM size: ramsize [size_in_KiB]
        Changing RAM requires a reset."""
        if not arg.strip():
            print(f"  RAM: {self.sys.ram_size // 1024} KiB ({self.sys.ram_size} bytes)")
            return
        kib = self._parse_int(arg)
        new_size = kib * 1024
        self.sys = MegapadSystem(ram_size=new_size, storage_image=self.sys.storage.image_path)
        self.sys.uart.on_tx = self._uart_tx_handler
        print(f"  RAM resized to {kib} KiB. System recreated (use 'boot' to start).")

    # -- UART --

    def do_send(self, arg):
        """Send text to UART RX buffer: send <text>
        The CPU will see this as keyboard input."""
        if not arg:
            print("Usage: send <text>")
            return
        self.sys.uart.inject_input(arg + "\n")
        print(f"  Sent {len(arg) + 1} bytes to UART RX.")

    def do_uart(self, arg):
        """Show UART buffer status."""
        u = self.sys.uart
        print(f"  TX buffer: {len(u.tx_buffer)} bytes")
        print(f"  RX buffer: {len(u.rx_buffer)} bytes")
        print(f"  Control: {u.control:#04x}")

    # -- Misc --

    def do_cycles(self, arg):
        """Show total cycle count."""
        print(f"  {self.sys.cpu.cycle_count} cycles")

    def do_quit(self, arg):
        """Exit the monitor."""
        print("Goodbye.")
        return True
    do_exit = do_quit
    do_q = do_quit

    def do_EOF(self, arg):
        print()
        return self.do_quit(arg)

    def default(self, line):
        """Handle unknown commands gracefully."""
        print(f"Unknown command: {line.split()[0]!r}. Type 'help' for available commands.")

    def emptyline(self):
        """Don't repeat the last command on empty input."""
        pass


# ---------------------------------------------------------------------------
#  Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Megapad-64 System Monitor",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="Examples:\n"
               "  python cli.py --ram 1024 --storage disk.img\n"
               "  python cli.py --bios bios.bin --storage disk.img\n"
               "  python cli.py --load program.bin@0x100\n"
    )
    parser.add_argument("--ram", type=int, default=1024,
                        help="RAM size in KiB (default: 1024)")
    parser.add_argument("--storage", type=str, default=None,
                        help="Disk image file to attach")
    parser.add_argument("--load", type=str, action="append", default=[],
                        help="Load binary: FILE[@ADDR] (can repeat)")
    parser.add_argument("--bios", type=str, default=None,
                        help="BIOS binary or .asm file to load at address 0")
    parser.add_argument("--run", action="store_true",
                        help="Auto-boot and run after loading")
    args = parser.parse_args()

    # Create system
    sys_emu = MegapadSystem(
        ram_size=args.ram * 1024,
        storage_image=args.storage,
    )

    # Load files
    for spec in args.load:
        if "@" in spec:
            path, addr_s = spec.rsplit("@", 1)
            addr = int(addr_s, 0)
        else:
            path, addr = spec, 0
        with open(path, "rb") as f:
            data = f.read()
        sys_emu.load_binary(addr, data)
        print(f"Loaded {len(data)} bytes from '{path}' at {addr:#x}")

    # Load BIOS
    bios_loaded = False
    if args.bios:
        if args.bios.endswith(".asm"):
            with open(args.bios, "r") as f:
                source = f.read()
            try:
                code = assemble(source, 0)
                sys_emu.load_binary(0, code)
                print(f"Assembled BIOS from '{args.bios}': {len(code)} bytes")
                bios_loaded = True
            except AsmError as e:
                print(f"BIOS assembly error: {e}")
                sys.exit(1)
        else:
            with open(args.bios, "rb") as f:
                data = f.read()
            sys_emu.load_binary(0, data)
            print(f"Loaded BIOS from '{args.bios}': {len(data)} bytes")
            bios_loaded = True

    # Auto-boot: if --run is given, or if a BIOS was loaded
    if args.run or bios_loaded:
        sys_emu.boot(0)
        print("Booting..." if bios_loaded else "Auto-booting...")
        # Run until CPU halts or goes idle (waiting for input)
        for _ in range(1_000_000):
            if sys_emu.cpu.halted or sys_emu.cpu.idle:
                break
            try:
                sys_emu.step()
            except Exception as e:
                print(f"Error during run: {e}")
                break
        output = sys_emu.get_tx_output()
        if output:
            print(output)

    # Start CLI
    cli = MegapadCLI(sys_emu)
    try:
        cli.cmdloop()
    except KeyboardInterrupt:
        print("\nInterrupted. Goodbye.")


if __name__ == "__main__":
    main()
