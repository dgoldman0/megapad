"""
Megapad-64 Assembler
=====================
Translates assembly text into raw bytecode per ENCODING.html.

Supports:
  - Labels (terminated with ':')
  - All 16 instruction families
  - Immediate literals (decimal, hex with 0x prefix)
  - Comments (';' to end of line)
  - .org, .db, .dw, .dd, .dq directives

Usage:
  from asm import assemble
  bytecode = assemble(source_text)
"""

from __future__ import annotations
import re
from typing import Optional

# ---------------------------------------------------------------------------
#  Condition code names → 4-bit values
# ---------------------------------------------------------------------------
COND_MAP = {
    "al": 0x0, "":   0x0,
    "eq": 0x1, "z":  0x1,
    "ne": 0x2, "nz": 0x2,
    "cs": 0x3, "c":  0x3,
    "cc": 0x4, "nc": 0x4,
    "mi": 0x5,
    "pl": 0x6,
    "vs": 0x7,
    "vc": 0x8,
    "gt": 0x9,
    "le": 0xA,
    "bq": 0xB,
    "bnq": 0xC,
    "sat": 0xD,
    "ef": 0xE,
    "nv": 0xF,
}

# ALU sub-op map (family 0x7)
ALU_SUB = {
    "add": 0x0, "adc": 0x1, "sub": 0x2, "sbb": 0x3,
    "and": 0x4, "or":  0x5, "xor": 0x6, "cmp": 0x7,
    "mov": 0x8, "not": 0x9, "neg": 0xA, "shl": 0xB,
    "shr": 0xC, "sar": 0xD, "rol": 0xE, "ror": 0xF,
}

# MEMALU sub-op map (family 0x8)
MEMALU_SUB = {
    "ldx":    0x0, "or.x":  0x1, "and.x":  0x2, "xor.x":  0x3,
    "add.x":  0x4, "sd.x":  0x5, "shr.d":  0x6, "sm.x":   0x7,
    "adc.x":  0x8, "sdb.x": 0x9, "shrc.d": 0xA, "smb.x":  0xB,
    "shl.d":  0xC, "shlc.d":0xD, "irx":    0xE, "ldxa.d": 0xF,
}

# MEM sub-op map (family 0x5)
MEM_SUB = {
    "ldn":   0x0, "lda":   0x1, "ldxr":  0x2, "ldxar": 0x3,
    "str":   0x4, "stxd":  0x5, "ld.b":  0x6, "st.b":  0x7,
    "ld.h":  0x8, "st.h":  0x9, "ld.w":  0xA, "st.w":  0xB,
    "ld.sb": 0xC, "ld.sh": 0xD, "ld.sw": 0xE, "ld.d":  0xF,
}

# MUL/DIV sub-op map (family 0xC)
MULDIV_SUB = {
    "mul":  0x0, "mulh":  0x1, "umul":  0x2, "umulh": 0x3,
    "div":  0x4, "udiv":  0x5, "mod":   0x6, "umod":  0x7,
}

# ---------------------------------------------------------------------------
#  Parser helpers
# ---------------------------------------------------------------------------

def _parse_reg(tok: str) -> int:
    """Parse 'R0'-'R15' or 'r0'-'r15'. Returns register index."""
    tok = tok.strip().lower()
    if tok.startswith("r") and tok[1:].isdigit():
        n = int(tok[1:])
        if 0 <= n <= 15:
            return n
    raise ValueError(f"Invalid register: {tok!r}")

def _parse_imm(tok: str) -> int:
    """Parse an immediate value (decimal or 0x hex)."""
    tok = tok.strip()
    if tok.startswith("0x") or tok.startswith("0X"):
        return int(tok, 16)
    if tok.startswith("-"):
        return int(tok, 10)
    return int(tok, 0)

def _split_ops(rest: str) -> list[str]:
    """Split operand string by comma, trimming whitespace."""
    return [s.strip() for s in rest.split(",") if s.strip()]


def _parse_string(lineno: int, text: str) -> bytes:
    """Parse a double-quoted string literal with escape sequences."""
    text = text.strip()
    if not (text.startswith('"') and text.endswith('"')):
        raise AsmError(lineno, f"Expected quoted string, got: {text}")
    s = text[1:-1]
    result = bytearray()
    i = 0
    while i < len(s):
        if s[i] == '\\' and i + 1 < len(s):
            c = s[i + 1]
            if c == 'n':   result.append(0x0A)
            elif c == 'r': result.append(0x0D)
            elif c == 't': result.append(0x09)
            elif c == '0': result.append(0x00)
            elif c == '\\': result.append(0x5C)
            elif c == '"': result.append(0x22)
            elif c == 'x' and i + 3 < len(s):
                hex_str = s[i + 2 : i + 4]
                try:
                    result.append(int(hex_str, 16))
                except ValueError:
                    result.append(ord(c) & 0xFF)
                    i += 2
                    continue
                i += 4
                continue
            else: result.append(ord(c) & 0xFF)
            i += 2
        else:
            result.append(ord(s[i]) & 0xFF)
            i += 1
    return bytes(result)

# ---------------------------------------------------------------------------
#  Assembler
# ---------------------------------------------------------------------------

class AsmError(Exception):
    def __init__(self, line: int, msg: str):
        self.line = line
        super().__init__(f"Line {line}: {msg}")


def assemble(source: str, base_addr: int = 0, listing: bool = False) -> bytearray:
    """
    Two-pass assembler.
    Pass 1: collect labels, compute instruction sizes.
    Pass 2: emit bytecode with resolved addresses.
    If listing=True, print an address/hex/source listing to stdout.
    """

    lines = source.split("\n")
    # Pre-process: strip comments and whitespace
    cleaned: list[tuple[int, str]] = []
    for i, raw in enumerate(lines, 1):
        # Strip comments, but respect quoted strings
        result = []
        in_string = False
        for ch in raw:
            if ch == '"':
                in_string = not in_string
            if ch == ';' and not in_string:
                break
            result.append(ch)
        stripped = ''.join(result).strip()
        if stripped:
            cleaned.append((i, stripped))

    # ---- Pass 1: label collection and size computation ----
    labels: dict[str, int] = {}
    sizes: list[tuple[int, str, int]] = []  # (line_no, text, size_bytes)
    pc = base_addr

    for lineno, text in cleaned:
        # Check for label
        if text.endswith(":"):
            lbl = text[:-1].strip()
            if lbl in labels:
                raise AsmError(lineno, f"Duplicate label: {lbl}")
            labels[lbl] = pc
            continue

        # Directives
        lower = text.lower()
        if lower.startswith(".org"):
            val = _parse_imm(text[4:].strip())
            pc = val
            sizes.append((lineno, text, 0))
            continue
        if lower.startswith(".db"):
            vals = _split_ops(text[3:])
            pc += len(vals)
            sizes.append((lineno, text, len(vals)))
            continue
        if lower.startswith(".dw"):
            vals = _split_ops(text[3:])
            pc += len(vals) * 2
            sizes.append((lineno, text, len(vals) * 2))
            continue
        if lower.startswith(".dd"):
            vals = _split_ops(text[3:])
            pc += len(vals) * 4
            sizes.append((lineno, text, len(vals) * 4))
            continue
        if lower.startswith(".dq"):
            vals = _split_ops(text[3:])
            pc += len(vals) * 8
            sizes.append((lineno, text, len(vals) * 8))
            continue
        if lower.startswith(".asciiz"):
            s = _parse_string(lineno, text[7:].strip())
            pc += len(s) + 1
            sizes.append((lineno, text, len(s) + 1))
            continue
        if lower.startswith(".ascii"):
            s = _parse_string(lineno, text[6:].strip())
            pc += len(s)
            sizes.append((lineno, text, len(s)))
            continue

        sz = _instruction_size(lineno, text)
        sizes.append((lineno, text, sz))
        pc += sz

    total_size = pc - base_addr

    # ---- Pass 2: emit bytes ----
    code = bytearray()
    pc = base_addr
    listing_lines = []  # (addr, hex_bytes, source_text)

    for lineno, text, sz in sizes:
        start_pc = pc
        lower = text.lower()

        # Directives
        if lower.startswith(".org"):
            target = _parse_imm(text[4:].strip())
            # Pad with zeros to reach target
            while pc < target:
                code.append(0)
                pc += 1
            if listing:
                listing_lines.append((start_pc, "", f".org {target:#x}"))
            continue

        if lower.startswith(".db"):
            before = len(code)
            for tok in _split_ops(text[3:]):
                code.append(_parse_imm(tok) & 0xFF)
                pc += 1
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue
        if lower.startswith(".dw"):
            before = len(code)
            for tok in _split_ops(text[3:]):
                v = _parse_imm(tok) & 0xFFFF
                code.append(v & 0xFF)
                code.append((v >> 8) & 0xFF)
                pc += 2
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue
        if lower.startswith(".dd"):
            before = len(code)
            for tok in _split_ops(text[3:]):
                v = _parse_imm(tok) & 0xFFFFFFFF
                for b in range(4):
                    code.append((v >> (8*b)) & 0xFF)
                pc += 4
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue
        if lower.startswith(".dq"):
            before = len(code)
            for tok in _split_ops(text[3:]):
                tok_s = tok.strip()
                if tok_s in labels:
                    v = labels[tok_s]
                else:
                    v = _parse_imm(tok)
                for b in range(8):
                    code.append((v >> (8*b)) & 0xFF)
                pc += 8
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue
        if lower.startswith(".asciiz"):
            before = len(code)
            s = _parse_string(lineno, text[7:].strip())
            code.extend(s)
            code.append(0)
            pc += len(s) + 1
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue
        if lower.startswith(".ascii"):
            before = len(code)
            s = _parse_string(lineno, text[6:].strip())
            code.extend(s)
            pc += len(s)
            if listing:
                emb = code[before:before+8]
                hexstr = " ".join(f"{b:02X}" for b in emb)
                if len(code) - before > 8:
                    hexstr += " ..."
                listing_lines.append((start_pc, hexstr, text))
            continue

        emitted = _emit_instruction(lineno, text, pc, labels)
        assert len(emitted) == sz, f"Size mismatch line {lineno}: expected {sz}, got {len(emitted)}"
        if listing:
            hexstr = " ".join(f"{b:02X}" for b in emitted[:8])
            if len(emitted) > 8:
                hexstr += " ..."
            listing_lines.append((start_pc, hexstr, text))
        code.extend(emitted)
        pc += sz

    if listing:
        # Also capture labels from pass 1 for interleaving
        # Build reverse map: addr → label name
        addr_labels = {}
        for lbl, addr in labels.items():
            addr_labels.setdefault(addr, []).append(lbl)
        # Print listing with labels
        for addr, hexstr, src in listing_lines:
            for lbl in addr_labels.pop(addr, []):
                print(f"                    {lbl}:")
            if hexstr:
                print(f"  {addr:06X}  {hexstr:<24s}  {src}")
            else:
                print(f"  {addr:06X}  {'':24s}  {src}")
        # Print any remaining labels at end
        for addr in sorted(addr_labels):
            for lbl in addr_labels[addr]:
                print(f"                    {lbl}:")

    return code


# ---------------------------------------------------------------------------
#  Instruction size computation (pass 1)
# ---------------------------------------------------------------------------

def _instruction_size(lineno: int, text: str) -> int:
    """Compute the byte size of one assembly instruction."""
    mnem, rest = _split_mnemonic(text)
    mnem_lower = mnem.lower()

    # -- SYS family (0x0) --
    if mnem_lower in ("idl", "nop", "halt", "reset", "rti", "ret", "dis",
                      "mark", "sav", "seq", "req", "ei", "di", "ret.l", "trap"):
        return 1
    if mnem_lower == "call.l":
        return 2  # 0x0D + reg byte

    # -- INC / DEC --
    if mnem_lower == "inc": return 1
    if mnem_lower == "dec": return 1

    # -- SKIP (conditional skip next instruction) --
    if mnem_lower.startswith("skip"):
        return 2  # EXT prefix + BR opcode (no offset byte)

    # -- BR --
    if mnem_lower.startswith("br"):
        return 2  # opcode + offset

    # -- LBR --
    if mnem_lower.startswith("lbr"):
        return 3

    # -- MEM --
    if mnem_lower in MEM_SUB:
        if mnem_lower == "ld.d":
            return 3  # opcode + reg-byte + offset
        return 2

    # -- IMM --
    if mnem_lower == "ldi":
        return 3  # opcode + reg + imm8 (default; imm64 via EXT prefix adds more)
    if mnem_lower == "ldi64":
        return 11  # EXT prefix (1) + opcode (1) + reg (1) + imm64 (8)
    if mnem_lower == "lhi":
        return 4  # opcode + reg + imm16_lo + imm16_hi
    if mnem_lower in ("addi", "andi", "ori", "xori", "cmpi", "subi"):
        return 3  # opcode + reg + imm8
    if mnem_lower in ("lsli", "lsri", "asri", "roli"):
        return 2  # opcode + (Rn|imm4)
    if mnem_lower in ("glo", "ghi", "plo", "phi"):
        return 2  # opcode + reg

    # -- ALU --
    if mnem_lower in ALU_SUB:
        return 2

    # -- MEMALU --
    if mnem_lower in MEMALU_SUB:
        return 1

    # -- I/O --
    if mnem_lower.startswith("out") or mnem_lower.startswith("inp"):
        return 1

    # -- SEP / SEX --
    if mnem_lower == "sep": return 1
    if mnem_lower == "sex": return 1

    # -- MUL/DIV --
    if mnem_lower in MULDIV_SUB:
        return 2

    # -- CSR --
    if mnem_lower in ("csrr", "csrw"):
        return 2

    # -- MEX --
    if mnem_lower.startswith("t."):
        # Tile ops: 2 or 3 bytes depending on broadcast
        # For simplicity: 2 bytes default, 3 if has register arg
        ops = _split_ops(rest)
        for op in ops:
            if op.lower().startswith("r"):
                return 3
        return 2

    raise AsmError(lineno, f"Unknown mnemonic: {mnem!r}")


# ---------------------------------------------------------------------------
#  Instruction emission (pass 2)
# ---------------------------------------------------------------------------

def _split_mnemonic(text: str) -> tuple[str, str]:
    """Split 'MNEM operands' → (mnem, operands_str)."""
    parts = text.split(None, 1)
    if len(parts) == 1:
        return parts[0], ""
    return parts[0], parts[1]


def _resolve_imm_or_label(tok: str, labels: dict, pc: int, bits: int,
                           relative: bool = False) -> int:
    """Resolve a token that is either an immediate or a label reference."""
    tok = tok.strip()
    if tok in labels:
        val = labels[tok]
        if relative:
            val = val - pc
            # Range-check for signed relative offsets
            lo = -(1 << (bits - 1))
            hi = (1 << (bits - 1)) - 1
            if val < lo or val > hi:
                raise AsmError(0, f"Branch offset {val} out of range [{lo}, {hi}] "
                                  f"for target '{tok}' (try LBR instead of BR)")
        return val
    return _parse_imm(tok)


def _emit_instruction(lineno: int, text: str, pc: int,
                       labels: dict[str, int]) -> bytearray:
    """Emit bytecode for one instruction."""
    out = bytearray()
    mnem, rest = _split_mnemonic(text)
    mnem_lower = mnem.lower()
    ops = _split_ops(rest)

    # ---- SYS family (0x0N) ----
    sys_map = {
        "idl":   0x00, "nop":   0x01, "halt":  0x02, "reset": 0x03,
        "rti":   0x04, "ret":   0x05, "dis":   0x06, "mark":  0x07,
        "sav":   0x08, "seq":   0x09, "req":   0x0A, "ei":    0x0B,
        "di":    0x0C, "ret.l": 0x0E, "trap":  0x0F,
    }
    if mnem_lower in sys_map:
        out.append(sys_map[mnem_lower])
        return out

    if mnem_lower == "call.l":
        rn = _parse_reg(ops[0])
        out.append(0x0D)
        out.append(rn & 0xF)
        return out

    # ---- INC / DEC ----
    if mnem_lower == "inc":
        rn = _parse_reg(ops[0])
        out.append(0x10 | (rn & 0xF))
        return out
    if mnem_lower == "dec":
        rn = _parse_reg(ops[0])
        out.append(0x20 | (rn & 0xF))
        return out

    # ---- SKIP (conditional skip next instruction) ----
    if mnem_lower.startswith("skip"):
        cc_name = mnem_lower[4:]  # e.g., "skipeq" → "eq", "skip" → ""
        if cc_name.startswith("."):
            cc_name = cc_name[1:]  # "skip.eq" → "eq"
        if not cc_name:
            cc_name = "al"
        if cc_name not in COND_MAP:
            raise AsmError(lineno, f"Unknown skip condition: {cc_name!r}")
        cc = COND_MAP[cc_name]
        out.append(0xF6)        # EXT prefix, modifier=6
        out.append(0x30 | cc)   # BR family with condition code
        return out

    # ---- BR (short branch) ----
    if mnem_lower.startswith("br"):
        cc_name = mnem_lower[2:]  # e.g., "breq" → "eq", "br" → ""
        if cc_name not in COND_MAP:
            # Might be just "br" with condition as first operand
            if not cc_name:
                cc_name = "al"
            else:
                raise AsmError(lineno, f"Unknown branch condition: {cc_name!r}")
        cc = COND_MAP[cc_name]
        out.append(0x30 | cc)
        # Offset — relative to PC after the 2-byte instruction
        target = _resolve_imm_or_label(ops[0], labels, pc + 2, 8, relative=True)
        out.append(target & 0xFF)
        return out

    # ---- LBR (long branch) ----
    if mnem_lower.startswith("lbr"):
        cc_name = mnem_lower[3:]
        if not cc_name:
            cc_name = "al"
        if cc_name not in COND_MAP:
            raise AsmError(lineno, f"Unknown branch condition: {cc_name!r}")
        cc = COND_MAP[cc_name]
        out.append(0x40 | cc)
        target = _resolve_imm_or_label(ops[0], labels, pc + 3, 16, relative=True)
        out.append((target >> 8) & 0xFF)
        out.append(target & 0xFF)
        return out

    # ---- MEM family (0x5S) ----
    if mnem_lower in MEM_SUB:
        sub = MEM_SUB[mnem_lower]
        out.append(0x50 | sub)
        if mnem_lower == "ld.d":
            rd = _parse_reg(ops[0])
            rn = _parse_reg(ops[1])
            off = _parse_imm(ops[2]) if len(ops) > 2 else 0
            out.append((rd << 4) | (rn & 0xF))
            out.append(off & 0xFF)
        else:
            rd = _parse_reg(ops[0])
            rs = _parse_reg(ops[1]) if len(ops) > 1 else 0
            out.append((rd << 4) | (rs & 0xF))
        return out

    # ---- IMM family (0x6S) ----
    if mnem_lower == "ldi64":
        # EXT.IMM64: prefix 0xF0 + 0x60 + Rn + 8 bytes LE
        rn = _parse_reg(ops[0])
        imm = _resolve_imm_or_label(ops[1], labels, pc, 64)
        out.append(0xF0)  # EXT prefix, modifier=0
        out.append(0x60)  # LDI sub-op
        out.append((rn << 4) & 0xF0)
        for i in range(8):
            out.append((imm >> (8 * i)) & 0xFF)
        return out

    if mnem_lower == "ldi":
        rn = _parse_reg(ops[0])
        imm = _resolve_imm_or_label(ops[1], labels, pc, 8)
        out.append(0x60)
        out.append((rn << 4) & 0xF0)
        out.append(imm & 0xFF)
        return out

    if mnem_lower == "lhi":
        rn = _parse_reg(ops[0])
        imm = _parse_imm(ops[1]) & 0xFFFF
        out.append(0x61)
        out.append((rn << 4) & 0xF0)
        out.append(imm & 0xFF)
        out.append((imm >> 8) & 0xFF)
        return out

    imm_arith = {
        "addi": 0x62, "andi": 0x63, "ori": 0x64,
        "xori": 0x65, "cmpi": 0x66, "subi": 0x67,
    }
    if mnem_lower in imm_arith:
        rn = _parse_reg(ops[0])
        imm = _parse_imm(ops[1])
        out.append(imm_arith[mnem_lower])
        out.append((rn << 4) & 0xF0)
        out.append(imm & 0xFF)
        return out

    imm_shift = {"lsli": 0x68, "lsri": 0x69, "asri": 0x6A, "roli": 0x6B}
    if mnem_lower in imm_shift:
        rn = _parse_reg(ops[0])
        imm4 = _parse_imm(ops[1]) & 0xF
        out.append(imm_shift[mnem_lower])
        out.append(((rn & 0xF) << 4) | (imm4 & 0xF))
        return out

    dlo_hi = {"glo": 0x6C, "ghi": 0x6D, "plo": 0x6E, "phi": 0x6F}
    if mnem_lower in dlo_hi:
        rn = _parse_reg(ops[0])
        out.append(dlo_hi[mnem_lower])
        out.append((rn << 4) & 0xF0)
        return out

    # ---- ALU family (0x7S) ----
    if mnem_lower in ALU_SUB:
        sub = ALU_SUB[mnem_lower]
        rd = _parse_reg(ops[0])
        rs = _parse_reg(ops[1])
        out.append(0x70 | sub)
        out.append(((rd & 0xF) << 4) | (rs & 0xF))
        return out

    # ---- MEMALU family (0x8N) ----
    if mnem_lower in MEMALU_SUB:
        sub = MEMALU_SUB[mnem_lower]
        out.append(0x80 | sub)
        return out

    # ---- I/O (0x9N) ----
    if mnem_lower.startswith("out"):
        n = int(mnem_lower[3:])
        if not (1 <= n <= 7):
            raise AsmError(lineno, f"OUT port must be 1-7, got {n}")
        out.append(0x90 | n)
        return out
    if mnem_lower.startswith("inp"):
        n = int(mnem_lower[3:])
        if not (1 <= n <= 7):
            raise AsmError(lineno, f"INP port must be 1-7, got {n}")
        out.append(0x90 | (8 + n))
        return out

    # ---- SEP / SEX ----
    if mnem_lower == "sep":
        rn = _parse_reg(ops[0])
        out.append(0xA0 | (rn & 0xF))
        return out
    if mnem_lower == "sex":
        rn = _parse_reg(ops[0])
        out.append(0xB0 | (rn & 0xF))
        return out

    # ---- MUL/DIV (0xCS) ----
    if mnem_lower in MULDIV_SUB:
        sub = MULDIV_SUB[mnem_lower]
        rd = _parse_reg(ops[0])
        rs = _parse_reg(ops[1])
        out.append(0xC0 | sub)
        out.append(((rd & 0xF) << 4) | (rs & 0xF))
        return out

    # ---- CSR (0xD) ----
    if mnem_lower == "csrr":
        rn = _parse_reg(ops[0])
        csr_addr = _parse_imm(ops[1])
        # CSRR: W=0, reg in low 3 bits of N
        n_nibble = rn & 0x7
        out.append(0xD0 | n_nibble)
        out.append(csr_addr & 0xFF)
        return out
    if mnem_lower == "csrw":
        csr_addr = _parse_imm(ops[0])
        rn = _parse_reg(ops[1])
        # CSRW: W=1, reg in low 3 bits
        n_nibble = 0x8 | (rn & 0x7)
        out.append(0xD0 | n_nibble)
        out.append(csr_addr & 0xFF)
        return out

    # ---- MEX tile ops (0xE) ----
    if mnem_lower.startswith("t."):
        # Syntax: t.OP [reg]
        # e.g., t.add, t.mul, t.dot, t.sum, t.zero, t.trans
        sub_name = mnem_lower[2:]
        talu_ops = {"add": 0, "sub": 1, "and": 2, "or": 3, "xor": 4,
                    "min": 5, "max": 6, "abs": 7}
        tmul_ops = {"mul": 0, "dot": 1}
        tred_ops = {"sum": 0, "rmin": 1, "rmax": 2, "popcnt": 3, "l1": 4}
        tsys_ops = {"trans": 0, "zero": 4, "loadc": 3, "movbank": 2}

        if sub_name in talu_ops:
            funct = talu_ops[sub_name]
            ss = 0  # tile-tile
            if ops and ops[0].lower().startswith("r"):
                ss = 1  # broadcast
            n_nibble = (ss << 2) | 0x0
            out.append(0xE0 | n_nibble)
            out.append(funct & 0x07)
            if ss == 1:
                out.append(_parse_reg(ops[0]) & 0xF)
            return out
        elif sub_name in tmul_ops:
            funct = tmul_ops[sub_name]
            ss = 0
            if ops and ops[0].lower().startswith("r"):
                ss = 1
            n_nibble = (ss << 2) | 0x1
            out.append(0xE0 | n_nibble)
            out.append(funct & 0x07)
            if ss == 1:
                out.append(_parse_reg(ops[0]) & 0xF)
            return out
        elif sub_name in tred_ops:
            funct = tred_ops[sub_name]
            n_nibble = 0x2  # TRED, ss=00
            out.append(0xE0 | n_nibble)
            out.append(funct & 0x07)
            return out
        elif sub_name in tsys_ops:
            funct = tsys_ops[sub_name]
            n_nibble = 0x3  # TSYS
            out.append(0xE0 | n_nibble)
            out.append(funct & 0x07)
            return out
        else:
            raise AsmError(lineno, f"Unknown tile op: {sub_name!r}")

    raise AsmError(lineno, f"Unknown mnemonic: {mnem!r}")
