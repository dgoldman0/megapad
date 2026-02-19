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
                [--display] [--scale N] [--extmem MiB]
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
    NIC_BASE, MBOX_BASE, SPINLOCK_BASE, SECTOR_SIZE,
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
        self._return_to_console = False

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
        max_steps = self._parse_int(arg) if arg.strip() else 10_000_000
        total = 0
        batch = 100_000
        while total < max_steps:
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
            if self.breakpoints:
                pc = self.sys.cpu.pc
                if pc in self.breakpoints:
                    print(f"\nBreakpoint hit at {pc:#010x}")
                    break
                # Fall back to step() when breakpoints are set
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
                try:
                    ran = self.sys.run_batch(min(batch, max_steps - total))
                    total += max(ran, 1)
                except HaltError:
                    print(f"\nCPU halted after {total} cycles.")
                    break
        else:
            print(f"\nStopped after {total} steps.")

    def do_continue(self, arg):
        """Alias for 'run'."""
        self.do_run(arg)
    do_c = do_continue

    # -- Console mode --

    def do_console(self, arg):
        """Return to BIOS console mode (Ctrl+] to come back here)."""
        self._return_to_console = True
        return True

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

    # -- NIC --

    def do_nic(self, arg):
        """NIC commands: nic [status|inject|send|reset]
        nic              — show NIC status
        nic inject <hex> — inject hex bytes as received frame
        nic send <hex>   — queue hex bytes as TX frame
        nic reset        — reset NIC state"""
        n = self.sys.nic
        parts = arg.strip().split(None, 1) if arg.strip() else []
        sub = parts[0].lower() if parts else 'status'

        if sub == 'status':
            link = 'up' if n.link_up else 'down'
            print(f"  NIC: link={link}  mac={n.mac.hex(':')}")
            print(f"  TX: {n.tx_count} frames sent   RX: {n.rx_count} received")
            print(f"  RX queue: {len(n.rx_queue)} frames pending")
            print(f"  Backend: {n.backend_name}")
            if n.backend and hasattr(n.backend, 'stats'):
                for k, v in n.backend.stats().items():
                    print(f"    {k}: {v}")
        elif sub == 'inject':
            if len(parts) < 2:
                print("Usage: nic inject <hex bytes>")
                return
            try:
                data = bytes(int(x, 16) for x in parts[1].split())
                n.inject_frame(data)
                print(f"  Injected {len(data)}-byte frame into RX queue.")
            except ValueError:
                print("  Error: invalid hex bytes.")
        elif sub == 'send':
            if len(parts) < 2:
                print("Usage: nic send <hex bytes>")
                return
            try:
                data = bytes(int(x, 16) for x in parts[1].split())
                n.frame_len = len(data)
                # Write to data port buffer and send
                n._data_buf = bytearray(data)
                n._data_pos = 0
                n._execute_cmd(0x01)
                print(f"  Sent {len(data)}-byte frame.")
            except ValueError:
                print("  Error: invalid hex bytes.")
        elif sub == 'reset':
            n._execute_cmd(0x04)
            print("  NIC reset.")
        else:
            print("Usage: nic [status|inject|send|reset]")

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

# ---------------------------------------------------------------------------
#  Headless TCP terminal server
# ---------------------------------------------------------------------------

_HEADLESS_PORT = 6464
_HEADLESS_STATUS = "/tmp/megapad_headless.json"
_HEADLESS_BATCH = 100_000


class HeadlessServer:
    """TCP terminal server — run the emulator headless with remote access.

    Boots the emulator, runs the CPU loop in a background thread, and
    serves UART I/O over a TCP socket.  Multiple clients can connect
    simultaneously; all see TX output and any can send input.

    Status info (PID, port) is written to /tmp/megapad_headless.json
    so ``--connect`` can auto-discover a running instance.
    """

    def __init__(self, sys_emu: MegapadSystem, port: int = _HEADLESS_PORT):
        self.sys_emu = sys_emu
        self.port = port
        self.clients: list = []            # connected client sockets
        self._lock = __import__("threading").Lock()
        self.running = False

    # -- public API -------------------------------------------------------

    def serve_forever(self):
        """Start the CPU thread and block on the accept loop."""
        import threading, signal, json, socket as _socket

        self.running = True

        # Wire UART TX → broadcast to all connected clients
        self.sys_emu.uart.on_tx = self._broadcast_byte

        # CPU loop in background thread
        self._cpu_thread = threading.Thread(
            target=self._cpu_loop, daemon=True, name="cpu-loop")
        self._cpu_thread.start()

        # TCP server
        self._srv = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM)
        self._srv.setsockopt(_socket.SOL_SOCKET, _socket.SO_REUSEADDR, 1)
        self._srv.bind(("0.0.0.0", self.port))
        self._srv.listen(8)
        self._srv.settimeout(1.0)     # so we can check self.running

        # Write status file
        status = {"pid": os.getpid(), "port": self.port}
        with open(_HEADLESS_STATUS, "w") as f:
            json.dump(status, f)

        # Handle SIGTERM / SIGINT gracefully
        def _shutdown(sig, frame):
            self.running = False
        signal.signal(signal.SIGTERM, _shutdown)
        signal.signal(signal.SIGINT, _shutdown)

        print(f"[headless] Listening on 0.0.0.0:{self.port}  "
              f"(PID {os.getpid()})")
        print(f"[headless] Connect with:  python cli.py --connect "
              f"localhost:{self.port}")
        print(f"[headless] Or plain:      nc localhost {self.port}")
        print(f"[headless] Status file:   {_HEADLESS_STATUS}")
        print(f"[headless] Ctrl+C to stop.")

        try:
            while self.running:
                try:
                    sock, addr = self._srv.accept()
                except OSError:
                    continue          # timeout — just re-check self.running
                print(f"[headless] Client connected: {addr[0]}:{addr[1]}")
                t = threading.Thread(
                    target=self._client_handler, args=(sock, addr),
                    daemon=True, name=f"client-{addr[0]}:{addr[1]}")
                t.start()
        finally:
            self._cleanup()

    # -- internals --------------------------------------------------------

    def _cpu_loop(self):
        """Run the CPU continuously in a background thread."""
        import time as _time
        while self.running:
            cpu = self.sys_emu.cpu
            if cpu.halted:
                _time.sleep(0.05)
                continue
            if (cpu.idle and not self.sys_emu.uart.has_rx_data
                    and not self.sys_emu.nic.rx_queue):
                _time.sleep(0.02)
                cpu.idle = False
                continue
            try:
                self.sys_emu.run_batch(_HEADLESS_BATCH)
            except HaltError:
                print("[headless] CPU halted.")
                break
            except Exception as e:
                print(f"[headless] CPU error: {e}")
                break

    def _broadcast_byte(self, b: int):
        """Send one TX byte to every connected client."""
        data = bytes([b])
        with self._lock:
            dead: list = []
            for sock in self.clients:
                try:
                    sock.sendall(data)
                except Exception:
                    dead.append(sock)
            for sock in dead:
                self.clients.remove(sock)
                try:
                    sock.close()
                except Exception:
                    pass

    def _client_handler(self, sock, addr):
        """Handle one TCP client — read → UART RX, UART TX → write."""
        import time as _time
        banner = (
            f"\r\n========================================\r\n"
            f"  Megapad-64 Headless Console\r\n"
            f"  Client: {addr[0]}:{addr[1]}\r\n"
            f"  RAM: {self.sys_emu.ram_size // 1024} KiB  "
            f"Cores: {self.sys_emu.num_cores}\r\n"
            f"  Ctrl+] to disconnect\r\n"
            f"========================================\r\n\r\n"
        )
        with self._lock:
            self.clients.append(sock)
        try:
            sock.sendall(banner.encode("utf-8"))
            sock.settimeout(0.5)
            while self.running:
                try:
                    data = sock.recv(4096)
                except OSError:
                    continue   # timeout — just loop
                if not data:
                    break
                for byte in data:
                    if byte == 0x1D:   # Ctrl+]
                        sock.sendall(
                            b"\r\n=== Disconnected ===\r\n")
                        return
                    self.sys_emu.uart.inject_input(bytes([byte]))
        except Exception:
            pass
        finally:
            with self._lock:
                if sock in self.clients:
                    self.clients.remove(sock)
            try:
                sock.close()
            except Exception:
                pass
            print(f"[headless] Client disconnected: {addr[0]}:{addr[1]}")

    def _cleanup(self):
        """Shut down server and all clients."""
        self.running = False
        with self._lock:
            for sock in self.clients:
                try:
                    sock.close()
                except Exception:
                    pass
            self.clients.clear()
        try:
            self._srv.close()
        except Exception:
            pass
        # Remove status file
        try:
            os.unlink(_HEADLESS_STATUS)
        except FileNotFoundError:
            pass
        print("[headless] Server stopped.")


def headless_connect(host: str, port: int):
    """Connect to a running headless instance as a raw terminal client.

    Wires stdin → TCP socket → emulator UART RX and
    emulator UART TX → TCP socket → stdout.
    Uses raw terminal mode for a real interactive experience.
    """
    import socket as _socket, select

    sock = _socket.socket(_socket.AF_INET, _socket.SOCK_STREAM)
    try:
        sock.connect((host, port))
    except ConnectionRefusedError:
        print(f"ERROR: Cannot connect to {host}:{port} — "
              f"is the headless server running?", file=sys.stderr)
        sys.exit(1)
    except OSError as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"Connected to {host}:{port}.  Ctrl+] to disconnect.")

    is_tty = os.isatty(sys.stdin.fileno())
    out_fd = sys.stdout.fileno()

    if is_tty:
        import termios, tty
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            _connect_loop(sock, fd, out_fd)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
            print()   # clean newline
    else:
        fd = sys.stdin.fileno()
        _connect_loop(sock, fd, out_fd)

    sock.close()


def _connect_loop(sock, in_fd: int, out_fd: int):
    """Raw select loop: stdin ↔ TCP socket."""
    import select
    sock.setblocking(False)
    try:
        while True:
            readable, _, _ = select.select([sock, in_fd], [], [], 0.1)
            for r in readable:
                if r is sock:
                    try:
                        data = sock.recv(4096)
                    except BlockingIOError:
                        continue
                    if not data:
                        os.write(out_fd, b"\r\n=== Server closed ===\r\n")
                        return
                    os.write(out_fd, data)
                else:
                    ch = os.read(in_fd, 1)
                    if not ch:
                        return
                    if ch == b'\x1d':    # Ctrl+]
                        return
                    sock.sendall(ch)
    except (KeyboardInterrupt, OSError):
        pass


# ---------------------------------------------------------------------------
#  Console mode — raw terminal ↔ UART
# ---------------------------------------------------------------------------

# Batch size: when the display thread handles keyboard input we can
# afford larger batches (fewer Python→C++ transitions, less poll
# overhead).  Without display, keep batches small so stdin latency
# stays low.
_BATCH_DISPLAY = 500_000
_BATCH_DEFAULT = 100_000


def run_console(sys_emu: MegapadSystem) -> bool:
    """Run the system with the host terminal wired directly to the UART.

    UART TX → stdout  (raw bytes, no translation)
    stdin   → UART RX (raw bytes, one keystroke at a time)

    Returns *True* if the user pressed Ctrl+] (escape to debug monitor),
    *False* if the CPU halted or an unrecoverable error occurred.
    """
    import select

    out_fd = sys.stdout.fileno()

    # Raw UART TX handler — write bytes directly to the terminal
    old_tx = sys_emu.uart.on_tx
    sys_emu.uart.on_tx = lambda b: os.write(out_fd, bytes([b]))

    is_tty = os.isatty(sys.stdin.fileno())

    if is_tty:
        return _console_raw(sys_emu, old_tx, out_fd)
    else:
        return _console_pipe(sys_emu, old_tx, out_fd)


def _console_raw(sys_emu: MegapadSystem, old_tx, out_fd) -> bool:
    """Console loop with raw terminal input (interactive TTY)."""
    import termios, tty, select

    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)

    try:
        batch = (_BATCH_DISPLAY
                 if sys_emu.uart._tx_listeners else _BATCH_DEFAULT)
        tty.setraw(fd)
        while True:
            # --- Run CPU in a batch until idle / halt ---------------
            if not sys_emu.cpu.halted and not (
                    sys_emu.cpu.idle and not sys_emu.uart.has_rx_data
                    and not sys_emu.nic.rx_queue):
                try:
                    sys_emu.run_batch(batch)
                except HaltError:
                    return False

            if sys_emu.cpu.halted:
                return False

            # --- Poll stdin (non-blocking) -------------------------
            timeout = 0.0 if not sys_emu.cpu.idle else 0.02
            if select.select([sys.stdin], [], [], timeout)[0]:
                ch = os.read(fd, 1)
                if ch == b'\x1d':          # Ctrl+]
                    return True
                if ch == b'\x03':          # Ctrl+C
                    return False
                if ch:
                    sys_emu.uart.inject_input(ch)
            elif sys_emu.cpu.idle:
                # Wake CPU so Forth polling loops (DHCP, ARP, PING)
                # can advance their timeout counters.
                sys_emu.cpu.idle = False
    except KeyboardInterrupt:
        return False
    finally:
        try:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        except Exception:
            pass
        sys_emu.uart.on_tx = old_tx


def _console_pipe(sys_emu: MegapadSystem, old_tx, out_fd) -> bool:
    """Console loop with piped/redirected stdin (non-interactive)."""
    import select

    fd = sys.stdin.fileno()

    try:
        while True:
            # --- Run CPU until idle / halt -------------------------
            if not sys_emu.cpu.halted and not (
                    sys_emu.cpu.idle and not sys_emu.uart.has_rx_data
                    and not sys_emu.nic.rx_queue):
                try:
                    sys_emu.run_batch(100_000)
                except HaltError:
                    return False

            if sys_emu.cpu.halted:
                return False

            if sys_emu.cpu.idle and not sys_emu.uart.has_rx_data and not sys_emu.nic.rx_queue:
                # Brief pause then wake CPU so Forth polling loops advance
                import time as _time
                _time.sleep(0.02)
                sys_emu.cpu.idle = False
                # Try to read from pipe (non-blocking via select)
                if select.select([sys.stdin], [], [], 0)[0]:
                    ch = os.read(fd, 1)
                    if not ch:                     # EOF
                        return False
                    if ch == b'\x1d':              # Ctrl+]
                        return True
                    sys_emu.uart.inject_input(ch)
    except (KeyboardInterrupt, OSError):
        return False
    finally:
        sys_emu.uart.on_tx = old_tx


# ---------------------------------------------------------------------------
#  Forth source injection
# ---------------------------------------------------------------------------

def _inject_forth_files(sys_emu: MegapadSystem, paths: list[str]):
    """Inject Forth source files through UART, line by line.

    Filters out pure comment lines (starting with \\) and blank lines
    to reduce the byte count.  Injects a full line at a time and uses
    run_batch() (C++ accelerated) instead of step().
    """
    out_fd = sys.stdout.fileno()
    old_tx = sys_emu.uart.on_tx
    sys_emu.uart.on_tx = lambda b: os.write(out_fd, bytes([b]))

    try:
        for path in paths:
            with open(path, "r") as f:
                source = f.read()
            lines = []
            for line in source.splitlines():
                stripped = line.strip()
                if not stripped:
                    continue
                if stripped.startswith('\\'):
                    continue
                lines.append(line)

            for line in lines:
                # Inject one full line + newline
                sys_emu.uart.inject_input((line + "\n").encode())
                # Run CPU until idle (processed the line)
                for _ in range(5_000):
                    if sys_emu.cpu.halted:
                        return
                    if sys_emu.cpu.idle and not sys_emu.uart.has_rx_data:
                        break
                    try:
                        sys_emu.run_batch(100_000)
                    except HaltError:
                        return
    finally:
        sys_emu.uart.on_tx = old_tx


# ---------------------------------------------------------------------------
#  Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Megapad-64 System Monitor",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="Examples:\n"
               "  python cli.py --bios bios.asm\n"
               "  python cli.py --bios bios.asm --forth kdos.f\n"
               "  python cli.py --bios bios.asm --storage disk.img\n"
               "  python cli.py --ram 1024 --storage disk.img\n"
               "  python cli.py --load program.bin@0x100 --run\n"
               "\n"
               "Headless mode (TCP terminal server):\n"
               "  python cli.py --bios bios.asm --storage disk.img --headless\n"
               "  python cli.py --connect localhost:6464\n"
    )
    parser.add_argument("--ram", type=int, default=1024,
                        help="RAM size in KiB (default: 1024)")
    parser.add_argument("--storage", type=str, default=None,
                        help="Disk image file to attach")
    parser.add_argument("--load", type=str, action="append", default=[],
                        help="Load binary: FILE[@ADDR] (can repeat)")
    parser.add_argument("--bios", type=str, default=None,
                        help="BIOS binary or .asm file — boots and enters console")
    parser.add_argument("--forth", type=str, action="append", default=[],
                        help="Forth source file to inject via UART after boot (can repeat)")
    parser.add_argument("--assemble", nargs=2, metavar=("SRC", "OUT"),
                        help="Assemble SRC.asm to OUT.rom and exit")
    parser.add_argument("--listing", "-l", action="store_true",
                        help="Print assembly listing (with --assemble)")
    parser.add_argument("--run", action="store_true",
                        help="Auto-boot and run after loading (non-BIOS mode)")
    parser.add_argument("--nic", type=int, default=None, metavar="PORT",
                        help="Enable NIC with UDP passthrough on PORT")
    parser.add_argument("--nic-peer-port", type=int, default=None, metavar="PORT",
                        help="UDP peer port for NIC passthrough (default: NIC+1)")
    parser.add_argument("--nic-tap", type=str, nargs='?', const='mp64tap0',
                        default=None, metavar="IFNAME",
                        help="Attach NIC to a Linux TAP device for real L2 "
                             "networking (default: mp64tap0)")
    parser.add_argument("--cores", type=int, default=1,
                        help="Number of full CPU cores (default: 1)")
    parser.add_argument("--clusters", type=int, default=0,
                        help="Number of micro-core clusters, 4 cores each (default: 0, max: 3)")
    parser.add_argument("--display", action="store_true",
                        help="Open a pygame window showing the framebuffer")
    parser.add_argument("--scale", type=int, default=2, metavar="N",
                        help="Pixel scale factor for display window (default: 2)")
    parser.add_argument("--extmem", type=int, default=16, metavar="MiB",
                        help="External memory size in MiB (default: 16, set 0 to disable)")

    # Headless mode
    parser.add_argument("--headless", action="store_true",
                        help="Run headless with a TCP terminal server "
                             "(connect with --connect or nc/telnet)")
    parser.add_argument("--headless-port", type=int, default=_HEADLESS_PORT,
                        metavar="PORT",
                        help=f"TCP port for headless server (default: {_HEADLESS_PORT})")
    parser.add_argument("--connect", type=str, default=None,
                        metavar="[HOST:]PORT",
                        help="Connect to a running headless instance "
                             "(e.g. localhost:6464)")
    args = parser.parse_args()

    # ---- Connect-only mode (no emulator needed) -----------------------
    if args.connect:
        spec = args.connect
        if ":" in spec:
            h, p = spec.rsplit(":", 1)
            host, port = (h or "localhost"), int(p)
        else:
            host, port = "localhost", int(spec)
        headless_connect(host, port)
        return

    # ---- Assemble-only mode -------------------------------------------
    if args.assemble:
        src_path, out_path = args.assemble
        with open(src_path, "r") as f:
            source = f.read()
        try:
            code = assemble(source, 0, listing=args.listing)
        except AsmError as e:
            print(f"Assembly error: {e}", file=sys.stderr)
            sys.exit(1)
        with open(out_path, "wb") as f:
            f.write(code)
        print(f"Assembled {src_path} → {out_path} ({len(code)} bytes)")
        return

    # Build NIC backend
    nic_backend = None
    if args.nic_tap:
        from nic_backends import TAPBackend, tap_available, setup_tap
        if not tap_available(args.nic_tap):
            print(f"ERROR: TAP device '{args.nic_tap}' is not accessible.",
                  file=sys.stderr)
            print(f"\nTo create it, run:\n", file=sys.stderr)
            print(setup_tap(args.nic_tap), file=sys.stderr)
            sys.exit(1)
        nic_backend = TAPBackend(tap_name=args.nic_tap)
        print(f"NIC attached to TAP device: {args.nic_tap}")

    # Create system
    sys_emu = MegapadSystem(
        ram_size=args.ram * 1024,
        storage_image=args.storage,
        nic_port=args.nic,
        nic_peer_port=args.nic_peer_port,
        nic_backend=nic_backend,
        num_cores=args.cores,
        num_clusters=args.clusters,
        ext_mem_size=args.extmem * (1 << 20),
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
                bios_loaded = True
            except AsmError as e:
                print(f"BIOS assembly error: {e}")
                sys.exit(1)
        else:
            with open(args.bios, "rb") as f:
                data = f.read()
            sys_emu.load_binary(0, data)
            bios_loaded = True

    # ---- BIOS mode: boot → console (the BIOS IS the interface) --------
    if bios_loaded:
        sys_emu.boot(0)

        # Start framebuffer display window if requested
        display = None
        if args.display:
            try:
                from display import FramebufferDisplay
                display = FramebufferDisplay(sys_emu, scale=args.scale)
                display.start()
                print("[display] Framebuffer window opened "
                      f"(scale={args.scale}x)")
            except ImportError as e:
                print(f"[display] pygame not available: {e}",
                      file=sys.stderr)
                print("[display] Install with: pip install pygame",
                      file=sys.stderr)

        # Inject Forth source files through UART before interactive console
        if args.forth:
            _inject_forth_files(sys_emu, args.forth)

        # ---- Headless mode: TCP server instead of interactive console --
        if args.headless:
            server = HeadlessServer(sys_emu, port=args.headless_port)
            server.serve_forever()
            if display is not None:
                display.stop()
            return

        while True:
            wants_monitor = run_console(sys_emu)
            if not wants_monitor or sys_emu.cpu.halted:
                break
            # Ctrl+] pressed — drop to debug monitor
            print("\n[Ctrl+] — debug monitor.  'console' to return, 'quit' to exit]")
            cli = MegapadCLI(sys_emu)
            cli.intro = ""  # No banner — the BIOS owns the screen
            try:
                cli.cmdloop()
            except KeyboardInterrupt:
                print()
            if cli._return_to_console and not sys_emu.cpu.halted:
                continue  # back to BIOS console
            break
        # Shut down display if running
        if display is not None:
            display.stop()
        print()  # clean newline on exit
        return

    # ---- Non-BIOS mode: optional auto-run, then debug monitor ----------
    if args.run:
        sys_emu.boot(0)
        print("Auto-booting...")
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

    cli = MegapadCLI(sys_emu)
    try:
        cli.cmdloop()
    except KeyboardInterrupt:
        print("\nInterrupted. Goodbye.")


if __name__ == "__main__":
    main()
