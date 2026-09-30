#!/usr/bin/env python3
"""Generate EXT.8 VSEL, TCMP, TCVT, TDIV, and TSQRT vectors for tb_tile_ext.v.

Every expected value comes from executing the instruction on the Python
emulator, whose results come from the shared exact reference
(shared/ieee_fp.py, docs/floating-point.md §6).  The vectors cover VSEL and
every TCMP predicate in all twelve integer and float modes with tile,
broadcast, and (TCMP) in-place sources, TCVT for every legal format pair
under all four signedness and rounding settings, including the multi-tile
widening and narrowing regions, and TDIV (tile, broadcast, in-place) and
TSQRT in all four float formats.

Each row is:

    name ss funct_byte tmode gpr reads writes source src1 dst_init
    expected cycles

``source`` holds ``reads`` tiles from TSRC0 and ``expected`` the ``writes``
tiles the operation leaves at TDST; every other destination tile must keep
``dst_init``.  ``cycles`` is the §10 extra cycle count for TCVT, TDIV, and
TSQRT, or 0 for VSEL and TCMP.  Within every hex token, byte offset zero is
the least-significant byte.

Regenerate from the repository root with:

    python3 rtl/sim/gen_tile_ext_vectors.py > rtl/sim/tile_ext_vectors.vec
"""

from __future__ import annotations

from pathlib import Path
import random
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from megapad64 import Megapad64  # noqa: E402
from shared import ieee_fp, tile_float, tile_formats  # noqa: E402


SRC0 = 0x400
SRC1 = 0x800
DST = 0xC00
GPR = 7
MODES = (0x00, 0x10, 0x01, 0x11, 0x02, 0x12, 0x03, 0x13, 4, 5, 6, 7)


def _tile(rng: random.Random, tmode: int) -> bytes:
    lane_format = tile_formats.decode(tmode)
    bits = lane_format.lane_bits
    lanes = []
    for _ in range(lane_format.lanes):
        pick = rng.random()
        if lane_format.is_float:
            fmt = lane_format.float_format
            if pick < 0.2:
                lanes.append(rng.choice((
                    0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
                    fmt.canonical_nan, fmt.infinity | 1, 1, fmt.max_finite,
                    fmt.max_finite | fmt.sign_bit, 1 << fmt.fraction_bits,
                )))
            elif pick < 0.5:
                lanes.append(rng.getrandbits(bits))
            else:
                # Values near the integer range edges and in the middle.
                magnitude = rng.choice((0.5, 1.5, 2.5, 127.5, 255.0, 256.0,
                                        32767.5, 65536.0, 2.0 ** 31, 2.0 ** 63,
                                        2.0 ** 64, 3.0, 1e6))
                value = rng.choice((-1, 1)) * magnitude * rng.choice((1, 1, 0.75))
                lanes.append(ieee_fp.from_double(fmt, value))
        else:
            if pick < 0.25:
                lanes.append(rng.choice((0, 1, (1 << bits) - 1,
                                         1 << (bits - 1), (1 << (bits - 1)) - 1)))
            else:
                lanes.append(rng.getrandbits(bits))
    return bytes(tile_float.pack_bits(bits, lanes))


def _execute(ss: int, funct_byte: int, tmode: int, gpr: int, source: bytes,
             src1: bytes, dst_init: bytes) -> tuple[bytes, int]:
    cpu = Megapad64(mem_size=0x1000)
    program = bytes([0xF8, 0xE0 | (ss << 2), funct_byte])
    if ss == 1:
        program += bytes([GPR])
    cpu.load_bytes(0, program)
    cpu.mem[SRC0:SRC0 + len(source)] = source
    cpu.mem[SRC1:SRC1 + 64] = src1
    cpu.mem[DST:DST + 512] = dst_init * 8
    cpu.csr_write(0x14, tmode)
    cpu.tsrc0, cpu.tsrc1, cpu.tdst = SRC0, SRC1, DST
    cpu.regs[GPR] = gpr
    cpu.pc = 0
    cpu.step()
    return bytes(cpu.mem[DST:DST + 512]), 0


def _hex(data: bytes) -> str:
    return data[::-1].hex() or "0"


def _row(name: str, ss: int, funct_byte: int, tmode: int, gpr: int,
         source: bytes, src1: bytes, dst_init: bytes, writes: int,
         cycles: int) -> str:
    region, _ = _execute(ss, funct_byte, tmode, gpr, source, src1, dst_init)
    for index in range(writes, 8):
        assert region[64 * index:64 * (index + 1)] == dst_init, name
    reads = len(source) // 64
    return " ".join((
        name, f"{ss:x}", f"{funct_byte:02x}", f"{tmode:02x}", f"{gpr:016x}",
        str(reads), str(writes), _hex(source), _hex(src1), _hex(dst_init),
        _hex(region[:64 * writes]), str(cycles),
    ))


def _rows() -> list[str]:
    rng = random.Random(0x7E47_0006)
    rows = []
    predicates = ("eq", "ne", "lt", "le", "gt", "ge", "unord", "ord")
    for tmode in MODES:
        for ss in (0, 1):
            for index in range(3):
                source = _tile(rng, tmode)
                rows.append(_row(
                    f"vsel_m{tmode:02x}_ss{ss}_{index}", ss, 0x02, tmode,
                    rng.getrandbits(64), source, _tile(rng, tmode),
                    _tile(rng, tmode), 1, 0))
        for predicate, label in enumerate(predicates):
            for ss in (0, 1, 3):
                source = _tile(rng, tmode)
                # Half the lanes of source 1 repeat source 0, so every
                # predicate sees equal lanes.
                other = _tile(rng, tmode)
                src1 = source[:32] + other[32:]
                rows.append(_row(
                    f"tcmp_{label}_m{tmode:02x}_ss{ss}", ss,
                    predicate << 3 | 7, tmode, rng.getrandbits(64), source,
                    src1, src1 if ss == 3 else _tile(rng, tmode), 1, 0))
    for tmode in (4, 5, 6, 7):
        cycles = tile_formats.divide_extra_cycles(tile_formats.decode(tmode))
        for index in range(4):
            for ss in (0, 1, 3):
                source = _tile(rng, tmode)
                src1 = _tile(rng, tmode)
                rows.append(_row(
                    f"tdiv_m{tmode:02x}_ss{ss}_{index}", ss,
                    tile_formats.EXT_TDIV, tmode, rng.getrandbits(64), source,
                    src1, src1 if ss == 3 else _tile(rng, tmode), 1, cycles))
            source = _tile(rng, tmode)
            rows.append(_row(
                f"tsqrt_m{tmode:02x}_{index}", 0, tile_formats.EXT_TSQRT,
                tmode, 0, source, _tile(rng, tmode), _tile(rng, tmode), 1,
                cycles))
    widths = (1, 2, 4, 8, 2, 2, 4, 8)
    for source_ew in range(8):
        for target_ew in range(8):
            if source_ew == target_ew or (source_ew < 4 and target_ew < 4):
                continue
            k = max(widths[source_ew], widths[target_ew]) // min(
                widths[source_ew], widths[target_ew])
            reads = k if widths[target_ew] < widths[source_ew] else 1
            writes = k if widths[target_ew] > widths[source_ew] else 1
            for mode_bits in (0x00, 0x10, 0x40, 0x50):
                tmode = source_ew | mode_bits
                source = b"".join(_tile(rng, tmode) for _ in range(reads))
                rows.append(_row(
                    f"tcvt_{source_ew}to{target_ew}_b{mode_bits:02x}", 0,
                    target_ew << 4 | 6, tmode, 0, source, bytes(64),
                    bytes([0x5A]) * 64, writes, 4 + (k - 1)))
    return rows


def main() -> None:
    rows = _rows()
    print("# Generated by rtl/sim/gen_tile_ext_vectors.py; do not edit.")
    print("# name ss funct_byte tmode gpr reads writes source src1 dst_init "
          "expected cycles")
    print(f"# count {len(rows)}")
    for row in rows:
        print(row)


if __name__ == "__main__":
    main()
