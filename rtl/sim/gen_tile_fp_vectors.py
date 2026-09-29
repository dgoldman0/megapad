#!/usr/bin/env python3
"""Generate tile-operation golden vectors for tb_tile_fp.v.

Every expected value comes from executing the instruction on the Python
emulator, whose floating-point results come from the shared exact reference
(shared/ieee_fp.py, docs/floating-point.md).  The vectors cover TALU, TMUL,
and TRED in FP16 and BF16 with tile, broadcast, immediate, and in-place
sources and all four TCTRL states; float PACK/UNPACK; integer operand routing
for immediate and in-place sources; integer MIN/MAX under ACC_ACC; the
FP32/FP64 element-wise operations (TALU, MUL, MAC, FMA, FP32 WMUL) and raw
POPCNT that run on the FMA units; and the FLAGS.Z update of every case.  The
immediate byte 0x06 is left out because it is the illegal immediate TAMAC
form, which the TACC benches cover.

Regenerate from the repository root with:

    python3 rtl/sim/gen_tile_fp_vectors.py > rtl/sim/tile_fp_vectors.vec
"""

from __future__ import annotations

from pathlib import Path
import random
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from megapad64 import Megapad64  # noqa: E402
from shared import ieee_fp, tile_float  # noqa: E402


SRC0 = 0x100
SRC1 = 0x140
DST = 0x180
DST2 = 0x1C0
GPR = 7  # R3 is the default program counter

OP_TALU, OP_TMUL, OP_TRED, OP_TSYS = range(4)

# Comparison mask bits consumed by tb_tile_fp.v.
CHECK_DST = 1 << 0
CHECK_DST2 = 1 << 1
CHECK_ACC = (1 << 2, 1 << 3, 1 << 4, 1 << 5)
CHECK_TCTRL = 1 << 6
CHECK_Z = 1 << 7
CHECK_ALL = CHECK_DST | CHECK_DST2 | sum(CHECK_ACC) | CHECK_TCTRL | CHECK_Z


def _lanes(rng: random.Random, fmt: ieee_fp.Format) -> list[int]:
    specials = (
        0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
        fmt.canonical_nan, fmt.infinity | 1, fmt.sign_bit | fmt.infinity | 3,
        1, fmt.sign_bit | 1, fmt.max_finite, fmt.max_finite | fmt.sign_bit,
        (1 << fmt.fraction_bits) - 1, 1 << fmt.fraction_bits,
    )
    anchor = rng.randrange(1, fmt.exponent_field_max)
    lanes = []
    for _ in range(32):
        choice = rng.random()
        if choice < 0.12:
            lanes.append(rng.choice(specials))
        elif choice < 0.4:
            lanes.append(rng.getrandbits(16))
        else:
            exponent = min(fmt.exponent_field_max - 1,
                           max(0, anchor + rng.randint(-3, 3)))
            lanes.append((rng.getrandbits(1) << 15)
                         | (exponent << fmt.fraction_bits)
                         | rng.getrandbits(fmt.fraction_bits))
    return lanes


def _tile(fmt: ieee_fp.Format, lanes: list[int]) -> bytes:
    return bytes(tile_float.pack_lanes(fmt, lanes))


def _execute(ss: int, op: int, funct_byte: int, tmode: int, tctrl: int,
             gpr: int, acc: tuple[int, int, int, int], src0: bytes,
             src1: bytes, dst: bytes, dst2: bytes,
             flag_z: int = 0) -> Megapad64:
    cpu = Megapad64(mem_size=0x400)
    cpu.flag_z = flag_z
    program = bytes([0xE0 | (ss << 2) | op, funct_byte])
    if ss == 1:
        program += bytes([GPR])
    cpu.load_bytes(0, program)
    cpu.mem[SRC0:SRC0 + 64] = src0
    cpu.mem[SRC1:SRC1 + 64] = src1
    cpu.mem[DST:DST + 64] = dst
    cpu.mem[DST2:DST2 + 64] = dst2
    cpu.tmode = tmode
    cpu.tctrl = tctrl
    cpu.tsrc0 = SRC0
    cpu.tsrc1 = SRC1
    cpu.tdst = DST
    cpu.acc = list(acc)
    cpu.regs[GPR] = gpr
    cpu.pc = 0
    cpu.step()
    return cpu


def _row(name: str, ss: int, op: int, funct_byte: int, tmode: int,
         tctrl: int, gpr: int, acc: tuple[int, int, int, int], src0: bytes,
         src1: bytes, dst: bytes, dst2: bytes, check: int) -> str:
    cpu = _execute(ss, op, funct_byte, tmode, tctrl, gpr, acc, src0, src1,
                   dst, dst2)
    other = _execute(ss, op, funct_byte, tmode, tctrl, gpr, acc, src0, src1,
                     dst, dst2, flag_z=1)
    # An operation updates Z when its result no longer depends on the old Z.
    z_valid = int(cpu.flag_z == other.flag_z)
    z_value = cpu.flag_z if z_valid else 0

    def word(data: bytes) -> str:
        return f"{int.from_bytes(bytes(data), 'little'):0128x}"

    fields = [
        name, f"{ss:x}", f"{op:x}", f"{funct_byte:02x}", f"{tmode:02x}",
        f"{tctrl:02x}", f"{gpr:016x}",
        *(f"{value:016x}" for value in acc),
        word(src0), word(src1), word(dst), word(dst2),
        word(cpu.mem[DST:DST + 64]), word(cpu.mem[DST2:DST2 + 64]),
        *(f"{value:016x}" for value in cpu.acc),
        f"{cpu.tctrl & 0xFF:02x}", f"{z_valid:x}", f"{z_value:x}",
        f"{check:02x}",
    ]
    return " ".join(fields)


def _float_rows(rng: random.Random) -> list[str]:
    rows = []
    for fmt in (ieee_fp.FP16, ieee_fp.BF16):
        wide = ieee_fp.accumulation_format(fmt)

        def case(label: str, ss: int, op: int, funct_byte: int,
                 tctrl: int) -> None:
            tmode = fmt.ew | rng.choice((0x00, 0x10, 0x20, 0x40, 0x70))
            acc = tuple(
                ieee_fp.lane_convert(wide, fmt, _lanes(rng, fmt)[0])
                | (rng.getrandbits(32) << 32 if rng.random() < 0.3 else 0)
                for _ in range(4)
            )
            rows.append(_row(
                f"{fmt.name}_{label}", ss, op, funct_byte, tmode, tctrl,
                rng.getrandbits(64), acc,
                _tile(fmt, _lanes(rng, fmt)), _tile(fmt, _lanes(rng, fmt)),
                _tile(fmt, _lanes(rng, fmt)), bytes([0x5A]) * 64,
                CHECK_ALL,
            ))

        for funct in range(8):
            for ss in (0, 1, 3):
                for index in range(6 if ss < 3 else 3):
                    case(f"talu{funct}_ss{ss}_{index}", ss, OP_TALU, funct,
                         rng.randrange(4))
        for index in range(8):
            case(f"talu_imm_{index}", 2, OP_TALU, _immediate(rng),
                 rng.randrange(4))
            case(f"tmul_imm_{index}", 2, OP_TMUL, _immediate(rng),
                 rng.randrange(4))
            case(f"tred_imm_{index}", 2, OP_TRED, _immediate(rng),
                 rng.randrange(4))
        for funct in range(6):
            for ss in (0, 1, 3):
                for tctrl in range(4):
                    for index in range(2 if ss < 3 else 1):
                        case(f"tmul{funct}_ss{ss}_c{tctrl}_{index}", ss,
                             OP_TMUL, funct, tctrl)
        for funct in (0, 1, 2, 4, 5, 6, 7):
            for ss in (0, 3):
                for tctrl in range(4):
                    for index in range(3 if ss == 0 else 1):
                        case(f"tred{funct}_ss{ss}_c{tctrl}_{index}", ss,
                             OP_TRED, funct, tctrl)
        for funct in (5, 6):
            for index in range(6):
                case(f"tsys{funct}_{index}", 0, OP_TSYS, funct, 0)
    return rows


def _immediate(rng: random.Random) -> int:
    """A random immediate, avoiding the illegal immediate TAMAC byte."""

    value = rng.getrandbits(8)
    while value == 0x06:
        value = rng.getrandbits(8)
    return value


def _integer_routing_rows(rng: random.Random) -> list[str]:
    """Integer immediate and in-place operand routing.

    Accumulators start at zero and reductions take ACC_ZERO, so the RTL's
    64-bit integer accumulator publishes the same words as the oracle.
    """

    rows = []
    for tmode in (0x00, 0x10, 0x01, 0x11, 0x02):
        width = 1 << (tmode & 0x3)
        for ss in (2, 3):
            for op, functs in ((OP_TALU, range(8)), (OP_TMUL, (0, 3, 4)),
                               (OP_TRED, (0, 1, 2, 3, 6, 7))):
                for funct in functs:
                    funct_byte = _immediate(rng) if ss == 2 else funct
                    if ss == 2 and op == OP_TMUL:
                        while funct_byte & 0x7 in (6, 7):
                            funct_byte = _immediate(rng)
                    tiles = [bytes(rng.getrandbits(8) for _ in range(64))
                             for _ in range(3)]
                    tctrl = 2 if op == OP_TRED else rng.randrange(4)
                    rows.append(_row(
                        f"int_t{tmode:02x}_ss{ss}_op{op}_f{funct}",
                        ss, op, funct_byte, tmode, tctrl,
                        rng.getrandbits(8 * width), (0, 0, 0, 0),
                        tiles[0], tiles[1], tiles[2], bytes([0x5A]) * 64,
                        CHECK_DST | CHECK_DST2 | CHECK_ACC[0] | CHECK_TCTRL
                        | CHECK_Z,
                    ))
    return rows


def _named_rows() -> list[str]:
    """Pin the specific defects Phase 2 removes."""

    rows = []
    fp16 = ieee_fp.FP16
    sentinel = bytes([0x5A]) * 64
    zero_acc = (0, 0, 0, 0)

    def repeated(value: int) -> bytes:
        return value.to_bytes(2, "little") * 32

    # Largest-subnormal carry: 0x0017 * 0x5190 is a tie that rounds to 0x0400.
    rows.append(_row("fp16_subnormal_carry_mul", 0, OP_TMUL, 0, 4, 0, 0,
                     zero_acc, repeated(0x0017), repeated(0x5190),
                     sentinel, sentinel, CHECK_ALL))
    # Subnormal products are kept rather than flushed.
    rows.append(_row("fp16_subnormal_product", 0, OP_TMUL, 0, 4, 0, 0,
                     zero_acc, repeated(0x0001), repeated(0x3C00),
                     sentinel, sentinel, CHECK_ALL))
    # BF16 multiply rounds rather than truncates: 1.0078125^2.
    rows.append(_row("bf16_mul_rounds", 0, OP_TMUL, 0, 5, 0, 0, zero_acc,
                     repeated(0x3F81), repeated(0x3F81), sentinel, sentinel,
                     CHECK_ALL))
    # Fused multiply-add rounds once: 1.0009765625^2 - 1 keeps the low bit.
    one_plus = ieee_fp.from_double(fp16, 1.0 + 2.0 ** -10)
    minus_one = ieee_fp.from_double(fp16, -1.0)
    rows.append(_row("fp16_fma_single_rounding", 0, OP_TMUL, 4, 4, 0, 0,
                     zero_acc, repeated(one_plus), repeated(one_plus),
                     repeated(minus_one), sentinel, CHECK_ALL))
    # The pairwise tree drops the middle 1.0 in lane order.
    lanes = [ieee_fp.from_double(fp16, value)
             for value in (65504.0, 1.0, -65504.0)] + [0] * 29
    rows.append(_row("fp16_sum_tree_order", 0, OP_TRED, 0, 4, 2, 0, zero_acc,
                     bytes(tile_float.pack_lanes(fp16, lanes)), sentinel,
                     sentinel, sentinel, CHECK_ALL))
    # Signed zero ordering in TALU MIN/MAX.
    for funct in (5, 6):
        rows.append(_row(f"bf16_signed_zero_talu{funct}", 0, OP_TALU, funct,
                         5, 0, 0, zero_acc, repeated(0x0000),
                         repeated(0x8000), sentinel, sentinel, CHECK_ALL))
    # An all-NaN tile gives the canonical binary32 NaN and index 0.
    rows.append(_row("bf16_all_nan_maxidx", 0, OP_TRED, 7, 5, 0, 0,
                     (1, 2, 3, 4), repeated(0x7F95), sentinel, sentinel,
                     sentinel, CHECK_ALL))
    return rows


def _wide_lanes(rng: random.Random, fmt: ieee_fp.Format) -> list[int]:
    specials = (
        0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
        fmt.canonical_nan, fmt.infinity | 1, fmt.sign_bit | fmt.infinity | 3,
        1, fmt.sign_bit | 1, fmt.max_finite, fmt.max_finite | fmt.sign_bit,
        (1 << fmt.fraction_bits) - 1, 1 << fmt.fraction_bits,
    )
    anchor = rng.randrange(1, fmt.exponent_field_max)
    lanes = []
    for _ in range(64 // (fmt.width // 8)):
        choice = rng.random()
        if choice < 0.12:
            lanes.append(rng.choice(specials))
        elif choice < 0.35:
            lanes.append(rng.getrandbits(fmt.width))
        else:
            exponent = min(fmt.exponent_field_max - 1,
                           max(0, anchor + rng.randint(-3, 3)))
            lanes.append((rng.getrandbits(1) << (fmt.width - 1))
                         | (exponent << fmt.fraction_bits)
                         | rng.getrandbits(fmt.fraction_bits))
    return lanes


def _wide_float_rows(rng: random.Random) -> list[str]:
    """FP32/FP64 element-wise operations on the FMA units (Phase 4)."""

    rows = []
    for fmt in (ieee_fp.FP32, ieee_fp.FP64):

        def case(label: str, ss: int, op: int, funct_byte: int,
                 tctrl: int | None = None) -> None:
            tmode = fmt.ew | rng.choice((0x00, 0x10, 0x20, 0x40, 0x70))
            acc = tuple(rng.getrandbits(64) for _ in range(4))
            rows.append(_row(
                f"{fmt.name}_{label}", ss, op, funct_byte, tmode,
                rng.randrange(4) if tctrl is None else tctrl,
                rng.getrandbits(64), acc,
                _tile(fmt, _wide_lanes(rng, fmt)),
                _tile(fmt, _wide_lanes(rng, fmt)),
                _tile(fmt, _wide_lanes(rng, fmt)), bytes([0x5A]) * 64,
                CHECK_ALL,
            ))

        for funct in range(8):
            for ss in (0, 1, 3):
                for index in range(4 if ss < 3 else 2):
                    case(f"talu{funct}_ss{ss}_{index}", ss, OP_TALU, funct)
        tmul = (0, 2, 3, 4) if fmt is ieee_fp.FP32 else (0, 3, 4)
        for funct in tmul:
            for ss in (0, 1, 3):
                for index in range(4 if ss < 3 else 2):
                    case(f"tmul{funct}_ss{ss}_{index}", ss, OP_TMUL, funct)
        for index in range(6):
            case(f"talu_imm_{index}", 2, OP_TALU, _immediate(rng))
            case(f"tmul_imm_{index}", 2, OP_TMUL, _immediate(rng))
        # POPCNT publishes to the integer accumulator.  ACC_ZERO keeps these
        # rows off the RTL's narrower integer ACC_ACC and no-control paths
        # (docs/megapad-full-float-plan.md §7).
        for index in range(4):
            case(f"tred_popcnt_{index}", 0, OP_TRED, 3, tctrl=2)
    return rows


def _integer_extreme_rows(rng: random.Random) -> list[str]:
    """Integer MIN/MAX keep a running extreme under ACC_ACC (ACC0 only).

    The RTL's integer accumulator does not yet mirror the Python oracle's
    256-bit sign extension into ACC1-ACC3, so only ACC0 and TCTRL compare.
    """

    rows = []
    for tmode in (0x00, 0x10, 0x01, 0x11, 0x02, 0x12):
        for funct in (1, 2):
            for index in range(4):
                width = 1 << (tmode & 0x3)
                source = bytes(rng.getrandbits(8) for _ in range(64))
                old = rng.getrandbits(8 * width)
                if tmode & 0x10 and old >> (8 * width - 1):
                    old -= 1 << (8 * width)
                acc = (old & ((1 << 64) - 1), 0, 0, 0)
                rows.append(_row(
                    f"int_t{tmode:02x}_tred{funct}_{index}", 0, OP_TRED,
                    funct, tmode, 1, 0, acc, source, bytes(64), bytes(64),
                    bytes(64), CHECK_ACC[0] | CHECK_TCTRL))
    return rows


def main() -> None:
    rng = random.Random(0x7F16_0002)
    rows = (_named_rows() + _float_rows(rng) + _integer_extreme_rows(rng)
            + _integer_routing_rows(rng)
            + _wide_float_rows(random.Random(0x7F64_0004)))
    print("# Generated by rtl/sim/gen_tile_fp_vectors.py; do not edit.")
    print("# name ss op funct_byte tmode tctrl gpr acc0 acc1 acc2 acc3 "
          "src0 src1 dst dst2 exp_dst exp_dst2 exp_acc0 exp_acc1 exp_acc2 "
          "exp_acc3 exp_tctrl exp_z_valid exp_z check")
    print(f"# count {len(rows)}")
    for row in rows:
        print(row)


if __name__ == "__main__":
    main()
