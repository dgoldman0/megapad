# Let's build an extended emulator with tile CSRs and a TSYS_TRANS instruction,
# plus a demo that transposes a 8x8 tile and prints before/after.

import re

class Megapad64CPU:
    def __init__(self, mem_size=65536):
        self.mem_size = mem_size
        self.mem = bytearray(mem_size)
        self.regs = [0] * 16  # R0-R15, 64-bit
        self.flags = {"Z": 0, "N": 0}  # minimal flags: Zero, Negative
        self.pc = 0            # program counter (index into instruction list)
        self.program = []      # list of decoded instructions
        self.halted = False
        self.output_buffer = []  # collected characters from OUT

        # CSRs relevant to Megapad / tile operations
        self.csrs = {
            "SB": 0,
            "SR": 0,
            "SC": 0,
            "SW": 1,  # stride width in tiles
            "TMODE": 0,
            "TCTRL": 0,
            "TSRC0": 0,
            "TSRC1": 0,
            "TDST": 0,
            "ACC0": 0,
            "ACC1": 0,
            "ACC2": 0,
            "ACC3": 0,
        }

    # ----- core state helpers -----

    def reset(self):
        self.mem = bytearray(self.mem_size)
        self.regs = [0] * 16
        self.flags = {"Z": 0, "N": 0}
        self.pc = 0
        self.halted = False
        self.output_buffer = []
        for k in self.csrs:
            self.csrs[k] = 0
        self.csrs["SW"] = 1  # default stride

    def load_program(self, program):
        """Load a list of decoded instructions."""
        self.program = program
        self.pc = 0
        self.halted = False

    def write_bytes(self, addr, data: bytes):
        """Write a sequence of bytes into Megapad memory."""
        end = addr + len(data)
        if end > self.mem_size:
            raise ValueError("Write beyond memory size")
        self.mem[addr:end] = data

    def read_byte(self, addr):
        if addr < 0 or addr >= self.mem_size:
            raise ValueError(f"Memory read out of range: {addr:#x}")
        return self.mem[addr]

    def write_byte(self, addr, value):
        if addr < 0 or addr >= self.mem_size:
            raise ValueError(f"Memory write out of range: {addr:#x}")
        self.mem[addr] = value & 0xFF

    @staticmethod
    def _u64(val):
        """Mask to 64 bits."""
        return val & ((1 << 64) - 1)

    def _set_flags_from_result(self, result):
        """Update Z and N based on 64-bit result."""
        result = self._u64(result)
        self.flags["Z"] = 1 if result == 0 else 0
        self.flags["N"] = 1 if (result >> 63) & 1 else 0

    # Megapad base: for simplicity we treat all banks as starting at 0 (single-bank model)
    def tile_addr(self):
        """Compute tile base address from SB, SR, SC, SW (64-byte tiles)."""
        sb = self.csrs["SB"]
        sr = self.csrs["SR"]
        sc = self.csrs["SC"]
        sw = self.csrs["SW"] if self.csrs["SW"] != 0 else 1
        # In full spec: MegapadBase(SB) + ((SR*SW)+SC)*64.
        # Here: single-bank, base 0.
        offset_tiles = sr * sw + sc
        addr = offset_tiles * 64
        if addr + 64 > self.mem_size:
            raise RuntimeError("Tile address out of range")
        return addr

    # ----- execution -----

    def step(self):
        """Execute one instruction."""
        if self.halted:
            return
        if self.pc < 0 or self.pc >= len(self.program):
            raise RuntimeError(f"PC out of program range: {self.pc}")

        instr = self.program[self.pc]
        op = instr["op"]
        args = instr["args"]
        pc_advanced = True

        # ---- basic core ops ----

        if op == "NOP":
            pass

        elif op == "HALT":
            self.halted = True

        # --- immediate / moves / ALU ---

        elif op == "LDI":
            rd, imm = args
            self.regs[rd] = self._u64(imm)
            self._set_flags_from_result(self.regs[rd])

        elif op == "MOV":
            rd, rs = args
            self.regs[rd] = self._u64(self.regs[rs])
            self._set_flags_from_result(self.regs[rd])

        elif op == "ADD":
            rd, rs = args
            self.regs[rd] = self._u64(self.regs[rd] + self.regs[rs])
            self._set_flags_from_result(self.regs[rd])

        elif op == "SUB":
            rd, rs = args
            self.regs[rd] = self._u64(self.regs[rd] - self.regs[rs])
            self._set_flags_from_result(self.regs[rd])

        elif op == "ADDI":
            rd, imm = args
            self.regs[rd] = self._u64(self.regs[rd] + imm)
            self._set_flags_from_result(self.regs[rd])

        elif op == "CMP":
            rd, rs = args
            result = self._u64(self.regs[rd] - self.regs[rs])
            self._set_flags_from_result(result)

        # --- scalar byte load/store ---

        elif op == "LOADB":
            rd, rs = args
            addr = self.regs[rs]
            byte = self.read_byte(addr)
            self.regs[rd] = self._u64(byte)
            self._set_flags_from_result(self.regs[rd])

        elif op == "STOREB":
            rd, rs = args
            addr = self.regs[rd]
            val = self.regs[rs] & 0xFF
            self.write_byte(addr, val)
            self._set_flags_from_result(val)

        # --- branches / jumps ---

        elif op == "BEQ":
            (target_pc,) = args
            if self.flags["Z"] == 1:
                self.pc = target_pc
                pc_advanced = False

        elif op == "BNE":
            (target_pc,) = args
            if self.flags["Z"] == 0:
                self.pc = target_pc
                pc_advanced = False

        elif op == "JMP":
            (target_pc,) = args
            self.pc = target_pc
            pc_advanced = False

        # --- basic I/O ---

        elif op == "OUT":
            port, rs = args
            if port == 1:
                ch = chr(self.regs[rs] & 0xFF)
                self.output_buffer.append(ch)
                print(ch, end="")   # live output to console

        elif op == "IN":
            port, rd = args
            if port == 1:
                import sys
                data = sys.stdin.read(1)
                if data:
                    self.regs[rd] = self._u64(ord(data[0]))
                else:
                    self.regs[rd] = 0
                self._set_flags_from_result(self.regs[rd])

        # --- CSR access ---

        elif op == "CSRR":
            rd, csr_name = args
            if csr_name not in self.csrs:
                raise RuntimeError(f"Unknown CSR {csr_name}")
            self.regs[rd] = self._u64(self.csrs[csr_name])
            self._set_flags_from_result(self.regs[rd])

        elif op == "CSRW":
            csr_name, rs = args
            if csr_name not in self.csrs:
                raise RuntimeError(f"Unknown CSR {csr_name}")
            self.csrs[csr_name] = self._u64(self.regs[rs])

        # --- Tile ops / MEX subset ---

        elif op == "FILL":
            # Fill current tile at cursor with byte pattern from TSRC1
            base = self.tile_addr()
            pattern = self.csrs["TSRC1"] & 0xFF
            for i in range(64):
                self.mem[base + i] = pattern
            # auto-advance SC/SR
            self.csrs["SC"] += 1
            if self.csrs["SC"] >= (self.csrs["SW"] or 1):
                self.csrs["SC"] = 0
                self.csrs["SR"] += 1

        elif op == "TSYS_TRANS":
            # Transpose 8x8 tile at TDST in place
            base = self.csrs["TDST"]
            if base % 64 != 0:
                raise RuntimeError("TSYS_TRANS tile base misaligned (must be multiple of 64)")
            if base + 64 > self.mem_size:
                raise RuntimeError("TSYS_TRANS out of memory range")
            tile = self.mem[base:base+64]
            new = bytearray(64)
            for r in range(8):
                for c in range(8):
                    new[c*8 + r] = tile[r*8 + c]
            self.mem[base:base+64] = new

        else:
            raise RuntimeError(f"Unknown opcode: {op}")

        if pc_advanced:
            self.pc += 1

    def run(self, max_steps=100000):
        """Run until HALT or step limit is reached."""
        steps = 0
        while not self.halted and steps < max_steps:
            self.step()
            steps += 1
        if steps >= max_steps:
            raise RuntimeError("Max steps exceeded (infinite loop?)")
        return "".join(self.output_buffer)


# ---------- Assembler with CSR & tile mnemonics ----------

REG_NAMES = {f"R{i}": i for i in range(16)}

CSR_NAMES = {
    "SB", "SR", "SC", "SW",
    "TMODE", "TCTRL",
    "TSRC0", "TSRC1", "TDST",
    "ACC0", "ACC1", "ACC2", "ACC3",
}


def parse_reg(token: str) -> int:
    token = token.strip().upper()
    # allow LOADB R1, [R0] / STOREB [R0], R1
    if token.startswith("[") and token.endswith("]"):
        token = token[1:-1].strip().upper()
    if token not in REG_NAMES:
        raise ValueError(f"Unknown register {token}")
    return REG_NAMES[token]


def parse_imm(token: str) -> int:
    token = token.strip()
    if token.lower().startswith("0x"):
        return int(token, 16)
    return int(token, 10)


def assemble(source: str):
    """Convert our tiny assembly into a list of instruction dicts.

    Instruction format: {"op": "LDI", "args": (...)}.
    Branch labels resolve to instruction indices.
    """
    lines = source.splitlines()

    # First pass: collect labels -> instruction index
    labels = {}
    pc = 0
    for line in lines:
        text = line.split(";", 1)[0].strip()
        if not text:
            continue
        if ":" in text:
            label, rest = text.split(":", 1)
            label = label.strip()
            labels[label] = pc
            rest = rest.strip()
            if rest:
                pc += 1
        else:
            pc += 1

    # Second pass: build instructions
    program = []
    pc = 0
    for line in lines:
        orig = line
        text = line.split(";", 1)[0].strip()
        if not text:
            continue
        if ":" in text:
            _, rest = text.split(":", 1)
            text = rest.strip()
            if not text:
                continue

        parts = re.split(r"\s+", text, maxsplit=1)
        mnemonic = parts[0].upper()
        ops_str = parts[1] if len(parts) > 1 else ""
        ops = [o.strip() for o in ops_str.split(",")] if ops_str else []

        def add(op, *args):
            program.append({"op": op, "args": args})

        if mnemonic == "NOP":
            add("NOP")

        elif mnemonic == "HALT":
            add("HALT")

        elif mnemonic == "LDI":
            rd = parse_reg(ops[0])
            imm = parse_imm(ops[1])
            add("LDI", rd, imm)

        elif mnemonic == "MOV":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("MOV", rd, rs)

        elif mnemonic == "ADD":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("ADD", rd, rs)

        elif mnemonic == "SUB":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("SUB", rd, rs)

        elif mnemonic == "ADDI":
            rd = parse_reg(ops[0])
            imm = parse_imm(ops[1])
            add("ADDI", rd, imm)

        elif mnemonic == "CMP":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("CMP", rd, rs)

        elif mnemonic == "LOADB":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("LOADB", rd, rs)

        elif mnemonic == "STOREB":
            rd = parse_reg(ops[0])
            rs = parse_reg(ops[1])
            add("STOREB", rd, rs)

        elif mnemonic == "BEQ":
            label = ops[0]
            target_pc = labels[label]
            add("BEQ", target_pc)

        elif mnemonic == "BNE":
            label = ops[0]
            target_pc = labels[label]
            add("BNE", target_pc)

        elif mnemonic == "JMP":
            label = ops[0]
            target_pc = labels[label]
            add("JMP", target_pc)

        elif mnemonic == "OUT":
            port = parse_imm(ops[0])
            rs = parse_reg(ops[1])
            add("OUT", port, rs)

        elif mnemonic == "IN":
            port = parse_imm(ops[0])
            rd = parse_reg(ops[1])
            add("IN", port, rd)

        elif mnemonic == "CSRR":
            rd = parse_reg(ops[0])
            csr_name = ops[1].upper()
            if csr_name not in CSR_NAMES:
                raise ValueError(f"Unknown CSR {csr_name} in line: {orig}")
            add("CSRR", rd, csr_name)

        elif mnemonic == "CSRW":
            csr_name = ops[0].upper()
            if csr_name not in CSR_NAMES:
                raise ValueError(f"Unknown CSR {csr_name} in line: {orig}")
            rs = parse_reg(ops[1])
            add("CSRW", csr_name, rs)

        elif mnemonic == "FILL":
            add("FILL")

        elif mnemonic in ("TSYS_TRANS", "TRANS"):
            # accept both TSYS_TRANS and TRANS as alias
            add("TSYS_TRANS")

        else:
            raise ValueError(f"Unknown mnemonic '{mnemonic}' in line: {orig}")

        pc += 1

    return program


# ---------- Hello-Megapad demo: tile transpose ----------

HELLO_MEGAPAD_PROGRAM = """
; "Hello Megapad": transpose an 8x8 tile in place using TSYS_TRANS
; TDST = base address of the tile

    LDI R0, 0           ; tile base at address 0
    CSRW TDST, R0       ; TDST <- 0

    ; Set TMODE to 8-bit lanes (not strictly required here, but illustrative)
    LDI R1, 0
    CSRW TMODE, R1

    TSYS_TRANS          ; transpose tile at TDST in place
    HALT
"""


def format_tile(mem, base):
    rows = []
    for r in range(8):
        row = []
        for c in range(8):
            row.append(mem[base + r*8 + c])
        rows.append(row)
    return rows


def print_tile_as_grid(rows):
    for r in rows:
        print(" ".join(f"{v:2d}" for v in r))


# Run the demo here and show before/after tile

cpu = Megapad64CPU(mem_size=1024)

# Initialize tile 0 with a simple gradient: value = row*8 + col
base = 0
tile_data = bytearray(64)
for r in range(8):
    for c in range(8):
        tile_data[r*8 + c] = r*8 + c
cpu.write_bytes(base, tile_data)

print("Initial tile (8x8, values row*8+col):")
before = format_tile(cpu.mem, base)
print_tile_as_grid(before)

# Assemble and run the Hello-Megapad program
prog = assemble(HELLO_MEGAPAD_PROGRAM)
cpu.load_program(prog)
cpu.run()

print("\nTile after TSYS_TRANS (transpose):")
after = format_tile(cpu.mem, base)
print_tile_as_grid(after)
