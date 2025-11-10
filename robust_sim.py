import re

class Megapad64CPU:
    def __init__(self, mem_size=65536):
        self.mem_size = mem_size
        self.mem = bytearray(mem_size)
        self.regs = [0] * 16  # R0-R15, 64-bit
        self.flags = {"Z": 0, "N": 0}  # Zero, Negative
        self.pc = 0
        self.program = []
        self.halted = False
        self.output_buffer = []
        self.pore_eject_log = []

        # CSRs: tile / MEX, simple conv taps, and a pore-eject pseudo-CSR
        self.csrs = {
            "SB": 0,
            "SR": 0,
            "SC": 0,
            "SW": 1,        # stride width in tiles
            "TMODE": 0,
            "TCTRL": 0,
            "TSRC0": 0,
            "TSRC1": 0,
            "TDST": 0,
            "ACC0": 0,
            "ACC1": 0,
            "ACC2": 0,
            "ACC3": 0,
            "K0": 0,        # conv tap 0 (signed int8)
            "K1": 0,        # conv tap 1
            "K2": 0,        # conv tap 2
            "PORE_EJECT": 0 # pseudo I/O register
        }

    # ---------- helpers ----------

    @staticmethod
    def _u64(val):
        return val & ((1 << 64) - 1)

    def _set_flags_from_result(self, result):
        result = self._u64(result)
        self.flags["Z"] = 1 if result == 0 else 0
        self.flags["N"] = 1 if (result >> 63) & 1 else 0

    def reset(self):
        self.mem = bytearray(self.mem_size)
        self.regs = [0] * 16
        self.flags = {"Z": 0, "N": 0}
        self.pc = 0
        self.halted = False
        self.output_buffer = []
        self.pore_eject_log = []
        for k in self.csrs:
            self.csrs[k] = 0
        self.csrs["SW"] = 1

    def load_program(self, program):
        self.program = program
        self.pc = 0
        self.halted = False

    def write_bytes(self, addr, data: bytes):
        end = addr + len(data)
        if end > self.mem_size:
            raise ValueError("Write beyond memory size")
        self.mem[addr:end] = data

    def read_byte(self, addr):
        if not (0 <= addr < self.mem_size):
            raise ValueError(f"Memory read out of range: {addr:#x}")
        return self.mem[addr]

    def write_byte(self, addr, value):
        if not (0 <= addr < self.mem_size):
            raise ValueError(f"Memory write out of range: {addr:#x}")
        self.mem[addr] = value & 0xFF

    def tile_addr(self):
        """Compute tile base from SR, SC, SW (single-bank model)."""
        sr = self.csrs["SR"]
        sc = self.csrs["SC"]
        sw = self.csrs["SW"] or 1
        offset_tiles = sr * sw + sc
        addr = offset_tiles * 64
        if addr + 64 > self.mem_size:
            raise RuntimeError("Tile address out of range")
        return addr

    # ---------- execution ----------

    def step(self):
        if self.halted:
            return
        if not (0 <= self.pc < len(self.program)):
            raise RuntimeError(f"PC out of range: {self.pc}")

        instr = self.program[self.pc]
        op = instr["op"]
        args = instr["args"]
        pc_advanced = True

        # scalar core
        if op == "NOP":
            pass

        elif op == "HALT":
            self.halted = True

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

        # branches (Z and N)
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

        elif op == "BPL":  # branch if result >= 0 (N == 0)
            (target_pc,) = args
            if self.flags["N"] == 0:
                self.pc = target_pc
                pc_advanced = False

        elif op == "JMP":
            (target_pc,) = args
            self.pc = target_pc
            pc_advanced = False

        # simple port I/O
        elif op == "OUT":
            port, rs = args
            if port == 1:
                ch = chr(self.regs[rs] & 0xFF)
                self.output_buffer.append(ch)
                print(ch, end="")

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

        # CSR moves
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
            val = self._u64(self.regs[rs])
            self.csrs[csr_name] = val
            if csr_name == "PORE_EJECT" and val != 0:
                msg = f"PORE_EJECT triggered with value {val}"
                self.pore_eject_log.append(msg)
                print("\n[MEGAPAD IO] " + msg)

        # tile / MEX-ish ops
        elif op == "FILL":
            base = self.tile_addr()
            pattern = self.csrs["TSRC1"] & 0xFF
            for i in range(64):
                self.mem[base + i] = pattern
            self.csrs["SC"] += 1
            if self.csrs["SC"] >= (self.csrs["SW"] or 1):
                self.csrs["SC"] = 0
                self.csrs["SR"] += 1

        elif op == "TSYS_TRANS":
            base = self.csrs["TDST"]
            if base % 64 != 0 or base + 64 > self.mem_size:
                raise RuntimeError("TSYS_TRANS misaligned or out of range")
            tile = self.mem[base:base+64]
            new = bytearray(64)
            for r in range(8):
                for c in range(8):
                    new[c*8 + r] = tile[r*8 + c]
            self.mem[base:base+64] = new

        elif op == "TSYS_CONV3":
            # 1-D conv per row: y[r,c] = x[r,c-1]*K0 + x[r,c]*K1 + x[r,c+1]*K2
            base_in = self.csrs["TSRC0"]
            base_out = self.csrs["TDST"]
            if base_in % 64 != 0 or base_out % 64 != 0:
                raise RuntimeError("TSYS_CONV3 misaligned")
            if base_in + 64 > self.mem_size or base_out + 64 > self.mem_size:
                raise RuntimeError("TSYS_CONV3 out of range")

            def s8(x):
                x &= 0xFF
                return x - 256 if x & 0x80 else x

            def sat_s8(x):
                if x > 127: return 127
                if x < -128: return -128
                return x

            k0 = s8(self.csrs["K0"])
            k1 = s8(self.csrs["K1"])
            k2 = s8(self.csrs["K2"])

            for r in range(8):
                for c in range(8):
                    x_m1 = s8(self.mem[base_in + r*8 + (c-1)]) if c-1 >= 0 else 0
                    x_0  = s8(self.mem[base_in + r*8 + c])
                    x_p1 = s8(self.mem[base_in + r*8 + (c+1)]) if c+1 < 8 else 0
                    acc = x_m1 * k0 + x_0 * k1 + x_p1 * k2
                    acc = sat_s8(acc)
                    self.mem[base_out + r*8 + c] = acc & 0xFF

        elif op == "TSYS_RELU_SUM":
            # ReLU and sum over tile at TSRC0; write sum -> ACC0
            base_in = self.csrs["TSRC0"]
            if base_in % 64 != 0 or base_in + 64 > self.mem_size:
                raise RuntimeError("TSYS_RELU_SUM misaligned or out of range")

            def s8(x):
                x &= 0xFF
                return x - 256 if x & 0x80 else x

            total = 0
            for i in range(64):
                v = s8(self.mem[base_in + i])
                if v < 0:
                    v = 0
                total += v
            self.csrs["ACC0"] = self._u64(total)
            self._set_flags_from_result(total)

        else:
            raise RuntimeError(f"Unknown op {op}")

        if pc_advanced:
            self.pc += 1

    def run(self, max_steps=100000):
        steps = 0
        while not self.halted and steps < max_steps:
            self.step()
            steps += 1
        if steps >= max_steps:
            raise RuntimeError("Max steps exceeded")
        return "".join(self.output_buffer)


# ---------- Assembler ----------

REG_NAMES = {f"R{i}": i for i in range(16)}
CSR_NAMES = {
    "SB", "SR", "SC", "SW",
    "TMODE", "TCTRL",
    "TSRC0", "TSRC1", "TDST",
    "ACC0", "ACC1", "ACC2", "ACC3",
    "K0", "K1", "K2",
    "PORE_EJECT",
}

def parse_reg(tok: str) -> int:
    tok = tok.strip().upper()
    if tok.startswith("[") and tok.endswith("]"):
        tok = tok[1:-1].strip().upper()
    if tok not in REG_NAMES:
        raise ValueError(f"Unknown register {tok}")
    return REG_NAMES[tok]

def parse_imm(tok: str) -> int:
    tok = tok.strip()
    if tok.lower().startswith("0x"):
        return int(tok, 16)
    return int(tok, 10)

def assemble(source: str):
    lines = source.splitlines()
    labels = {}
    pc = 0

    # pass 1: labels
    for line in lines:
        text = line.split(";",1)[0].strip()
        if not text:
            continue
        if ":" in text:
            label, rest = text.split(":",1)
            labels[label.strip()] = pc
            rest = rest.strip()
            if rest:
                pc += 1
        else:
            pc += 1

    # pass 2: instructions
    program = []
    pc = 0
    for line in lines:
        orig = line
        text = line.split(";",1)[0].strip()
        if not text:
            continue
        if ":" in text:
            _, rest = text.split(":",1)
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
            rd = parse_reg(ops[0]); imm = parse_imm(ops[1]); add("LDI", rd, imm)
        elif mnemonic == "MOV":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("MOV", rd, rs)
        elif mnemonic == "ADD":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("ADD", rd, rs)
        elif mnemonic == "SUB":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("SUB", rd, rs)
        elif mnemonic == "ADDI":
            rd = parse_reg(ops[0]); imm = parse_imm(ops[1]); add("ADDI", rd, imm)
        elif mnemonic == "CMP":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("CMP", rd, rs)
        elif mnemonic == "LOADB":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("LOADB", rd, rs)
        elif mnemonic == "STOREB":
            rd = parse_reg(ops[0]); rs = parse_reg(ops[1]); add("STOREB", rd, rs)
        elif mnemonic == "BEQ":
            label = ops[0]; add("BEQ", labels[label])
        elif mnemonic == "BNE":
            label = ops[0]; add("BNE", labels[label])
        elif mnemonic == "BPL":
            label = ops[0]; add("BPL", labels[label])
        elif mnemonic == "JMP":
            label = ops[0]; add("JMP", labels[label])
        elif mnemonic == "OUT":
            port = parse_imm(ops[0]); rs = parse_reg(ops[1]); add("OUT", port, rs)
        elif mnemonic == "IN":
            port = parse_imm(ops[0]); rd = parse_reg(ops[1]); add("IN", port, rd)
        elif mnemonic == "CSRR":
            rd = parse_reg(ops[0]); csr_name = ops[1].upper()
            if csr_name not in CSR_NAMES: raise ValueError(f"Unknown CSR {csr_name}")
            add("CSRR", rd, csr_name)
        elif mnemonic == "CSRW":
            csr_name = ops[0].upper()
            if csr_name not in CSR_NAMES: raise ValueError(f"Unknown CSR {csr_name}")
            rs = parse_reg(ops[1]); add("CSRW", csr_name, rs)
        elif mnemonic == "FILL":
            add("FILL")
        elif mnemonic in ("TSYS_TRANS","TRANS"):
            add("TSYS_TRANS")
        elif mnemonic in ("TSYS_CONV3","CONV3"):
            add("TSYS_CONV3")
        elif mnemonic in ("TSYS_RELU_SUM","RELU_SUM"):
            add("TSYS_RELU_SUM")
        else:
            raise ValueError(f"Unknown mnemonic {mnemonic} in line: {orig}")
        pc += 1

    return program

HELLO_BASECALLER = """
; Tiny "basecaller-like" pipeline over one tile.
; Raw signal: tile at 0
; Feature A:  tile at 64  (edge detector)
; Feature B:  tile at 128 (smoother)
; Decision: if scoreB >= scoreA -> trigger PORE_EJECT

    ; Tile addresses
    LDI R0, 0
    CSRW TSRC0, R0           ; input tile = 0

    LDI R1, 64
    CSRW TDST, R1            ; output tile A = 64

    ; Kernel A: [-1, 0, +1]
    LDI R2, 0xFF             ; -1 as int8
    CSRW K0, R2
    LDI R3, 0
    CSRW K1, R3
    LDI R4, 1
    CSRW K2, R4

    TSYS_CONV3               ; tile0 -> tile64

    ; Kernel B: [+1, +1, +1] on raw tile into tile128
    LDI R5, 0
    CSRW TSRC0, R5           ; back to input tile = 0

    LDI R6, 128
    CSRW TDST, R6            ; output tile B = 128

    LDI R7, 1
    CSRW K0, R7
    CSRW K1, R7
    CSRW K2, R7

    TSYS_CONV3               ; tile0 -> tile128

    ; Reduce feature A (tile64) with ReLU+sum
    CSRW TSRC0, R1           ; TSRC0 = 64
    TSYS_RELU_SUM
    CSRR R8, ACC0            ; scoreA

    ; Reduce feature B (tile128) with ReLU+sum
    CSRW TSRC0, R6           ; TSRC0 = 128
    TSYS_RELU_SUM
    CSRR R9, ACC0            ; scoreB

    ; Compare: if scoreB >= scoreA -> eject
    CMP R9, R8               ; result = R9 - R8
    BEQ no_eject             ; equal -> keep
    BPL do_eject             ; positive -> scoreB > scoreA -> eject
    JMP no_eject             ; negative -> keep

do_eject:
    LDI R10, 1
    CSRW PORE_EJECT, R10
    JMP done

no_eject:
    LDI R10, 0

done:
    HALT
"""

def format_tile_signed(mem, base):
    rows = []
    for r in range(8):
        row = []
        for c in range(8):
            b = mem[base + r*8 + c]
            row.append(b - 256 if b & 0x80 else b)
        rows.append(row)
    return rows

def demo_basecaller():
    cpu = Megapad64CPU(mem_size=1024)

    # Fake raw signal tile at 0:
    # left half low amplitude, right half high amplitude
    base_in = 0
    tile = bytearray(64)
    for r in range(8):
        for c in range(8):
            tile[r*8 + c] = 10 if c < 4 else 80
    cpu.write_bytes(base_in, tile)

    print("Raw signal tile (unsigned):")
    for r in range(8):
        row = [cpu.mem[base_in + r*8 + c] for c in range(8)]
        print(" ".join(f"{v:3d}" for v in row))

    prog = assemble(HELLO_BASECALLER)
    cpu.load_program(prog)
    cpu.run()

    print("\nFeature A (edge-detector) tile (signed):")
    featA = format_tile_signed(cpu.mem, 64)
    for r in featA:
        print(" ".join(f"{v:4d}" for v in r))

    print("\nFeature B (smoother) tile (signed):")
    featB = format_tile_signed(cpu.mem, 128)
    for r in featB:
        print(" ".join(f"{v:4d}" for v in r))

    print("\nScores:")
    print("scoreA (edge) from R8:", cpu.regs[8])
    print("scoreB (smooth) from R9:", cpu.regs[9])

    print("\nPore eject log:", cpu.pore_eject_log)


if __name__ == "__main__":
    demo_basecaller()

