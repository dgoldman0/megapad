#!/usr/bin/env python3
"""Generate the full-core EXT.FP program and its expected machine state.

The program runs every FC operation through the ordinary instruction path:
operands come from registers loaded with LDI64, FPCSR is written with CSRW
before each operation, and each result and the FPCSR (or, for FCMP, FLAGS)
after it are stored to a results buffer.  It also covers REX-extended
registers, SKIP over three- and four-byte FC instructions, and traps for a
reserved encoding, a reserved dynamic mode, and an unassigned prefix, each
resumed by an RTI handler.

The expected state comes from executing the same bytes on the Python
emulator, whose FC semantics are shared/scalar_fp.py.  tb_cpu_fp.v compares
the registers, FPCSR, FLAGS, and the results buffer.

Regenerate from the repository root with:

    python3 rtl/sim/gen_cpu_fp_program.py

An optional argument names another output directory.
"""

from __future__ import annotations

from pathlib import Path
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from asm import assemble  # noqa: E402
from megapad64 import Megapad64, TrapError  # noqa: E402
from shared import ieee_fp, scalar_fp  # noqa: E402
from shared.ieee_fp import FP32, FP64  # noqa: E402


HERE = Path(__file__).resolve().parent
MEM_SIZE = 1 << 16
RESULTS = 0x8000
STACK = 0xF000
MASK64 = (1 << 64) - 1

# Operand registers per format: R4-R11 and R16-R23.
OPERAND_REGS = [4, 5, 6, 7, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23]


def _operands(fmt) -> list[int]:
    d = lambda v: ieee_fp.from_double(fmt, v)  # noqa: E731
    return [
        d(1.0), d(3.0), d(-2.5), 1, fmt.max_finite, fmt.infinity | 1,
        fmt.sign_bit, d(0.1), d(1e10), d(-7.75), fmt.infinity,
        fmt.canonical_nan, d(2.0 ** -20), (1 << 63) | 12345, d(65520.0),
        d(1.0 + 2.0 ** -20),
    ]


def _source() -> str:
    lines = [
        "    ldi r0, 0",
        "    csrw 0x00, r0",
    ]
    # R16-R31 are cleared with MOV: the RTL gives REX+LDI the ten-byte
    # EXT.IMM64 length (docs/megapad-full-float-plan.md §7).
    for reg in range(32):
        if reg not in (0, 3):
            lines.append(f"    ldi r{reg}, 0" if reg < 16 else f"    mov r{reg}, r0")
    lines += [
        f"    ldi64 r15, {STACK}",
        "    ldi64 r1, ivt",
        "    csrw 0x04, r1",
        f"    ldi64 r13, {RESULTS}",
    ]

    def record(value_reg: int = 1, csr: int = 0x0D) -> list[str]:
        return [
            f"    str r13, r{value_reg}",
            "    addi r13, 8",
            f"    csrr r1, {csr:#04x}",
            "    str r13, r1",
            "    addi r13, 8",
        ]

    for fmt, suffix in ((FP32, "s"), (FP64, "d")):
        for reg, value in zip(OPERAND_REGS, _operands(fmt)):
            # LDI64 cannot target R16-R31; those go through R1.
            if reg < 16:
                lines.append(f"    ldi64 r{reg}, {value}")
            else:
                lines += [f"    ldi64 r1, {value}", f"    mov r{reg}, r1"]
        pairs = [(4, 5), (6, 7), (8, 8), (9, 4), (10, 16), (11, 12),
                 (17, 18), (19, 20), (21, 22), (23, 16)]
        for code in list(range(0x09)) + list(range(0x10, 0x15)) + list(
                range(0x20, 0x3F)):
            if 0x20 <= code < 0x38 and code & 7 in (5, 6):
                continue
            op = (0x40 if suffix == "d" else 0x00) | code
            modes = range(5) if scalar_fp.uses_dynamic_rounding(op) else (0,)
            for mode in modes:
                a, b = pairs[(code + mode) % len(pairs)]
                if code in (0x38, 0x39):
                    b = 20  # an integer-looking operand
                # FC op DR with Rd = R1; a REX prefix reaches Rs >= R16.
                prefix = "0xF1, " if b >= 16 else ""
                encoding = f"{prefix}0xFC, {op:#04x}, {0x10 | (b & 0xF):#04x}"
                if scalar_fp.instruction_length(op) == 4:
                    rt = (a + b) % 32
                    encoding += f", {rt if rt not in (2, 3, 13, 15) else 5}"
                lines += [
                    f"    mov r1, r{a}",
                    f"    ldi r0, {mode}",
                    "    csrw 0x0D, r0",
                    f"    .db {encoding}",
                ]
                lines += record(csr=0x00 if code == 0x10 else 0x0D)

    # REX-extended destination, SKIP over FC, and traps resumed by RTI.
    lines += [
        "    ldi r0, 0",
        "    csrw 0x0D, r0",
        "    mov r24, r5",
        "    fma.d r24, r17, r18",
        "    fsqrt.d r25, r16",
        "    ldi r0, 0",
        "    cmpi r0, 0",
        "    skip.eq",
        "    fma.d r26, r17, r18",
        "    inc r12",
        "    skip.eq",
        "    fadd.d r26, r17",
        "    inc r12",
        "    skip.ne",
        "    fdiv.s r27, r5",
        "    .db 0xFC, 0x87, 0x12, 0x00",      # reserved format: traps
        "    inc r14",
        "    ldi r0, 5",
        "    csrw 0x0D, r0",
        "    fadd.d r4, r5",                    # reserved dynamic mode
        "    inc r14",
        "    ldi r0, 0",
        "    csrw 0x0D, r0",
        "    .db 0xF7",                          # unassigned prefix
        "    inc r14",
        "    csrr r1, 0x0D",
        "    halt",
        "handler:",
        "    inc r12",
        "    rti",
        "ivt:",
        "    .dq handler",
        "    .dq handler",
        "    .dq handler",
        "    .dq handler",
    ]
    return "\n".join(lines) + "\n"


def main() -> None:
    code = assemble(_source())
    cpu = Megapad64(mem_size=MEM_SIZE)
    cpu.load_bytes(0, code)
    cpu.pc = 0
    steps = 0
    while not cpu.halted:
        steps += 1
        if steps > 100_000:
            raise SystemExit("program did not halt")
        try:
            cpu.step()
        except TrapError as trap:
            cpu._trap(trap.ivec_id)
    results = bytes(cpu.mem[RESULTS:cpu.regs[13]])
    assert len(results) % 8 == 0 and results, "no results recorded"
    words = [int.from_bytes(results[i:i + 8], "little")
             for i in range(0, len(results), 8)]
    expected = ([cpu.regs[i] for i in range(32)]
                + [cpu.fpcsr, cpu.flags_pack(), len(words)] + words)

    out = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE
    (out / "cpu_fp_program.hex").write_text(
        "// Generated by gen_cpu_fp_program.py; do not edit.\n"
        + "".join(f"{byte:02x}\n" for byte in code))
    (out / "cpu_fp_expected.hex").write_text(
        "// Generated by gen_cpu_fp_program.py; do not edit.\n"
        "// R0-R31, FPCSR, FLAGS, result count, results\n"
        + "".join(f"{value & MASK64:016x}\n" for value in expected))
    print(f"{len(code)} program bytes, {len(words)} result words, "
          f"{steps} steps; traps counted in R12={cpu.regs[12]}, "
          f"R14={cpu.regs[14]}")


if __name__ == "__main__":
    main()
