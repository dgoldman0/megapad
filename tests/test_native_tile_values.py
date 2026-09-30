"""Shared native tile candidates against the independent Python lane oracle."""

from __future__ import annotations

import ctypes
import importlib
import platform
import random

import pytest

from shared import ieee_fp as fp, tile_float as tf, tile_formats as formats


ELEMENTWISE = {
    "add": tf.ADD, "subtract": tf.SUB,
    "bitwise_and": tf.AND, "bitwise_or": tf.OR, "bitwise_xor": tf.XOR,
    "minimum": tf.MIN, "maximum": tf.MAX, "absolute": tf.ABS,
}
FLOAT_OPERATIONS = (
    "fused_multiply_add", "widening_multiply", "dot", "dot_chunks",
    "sum", "sum_squares", "l1_norm", "reduction_minimum", "reduction_maximum",
    "minimum_index", "maximum_index", "divide", "square_root",
)
OPERATIONS = (*ELEMENTWISE, "multiply", *FLOAT_OPERATIONS, "select", "compare_mask", "convert")
UNARY = {
    "absolute", "sum", "sum_squares", "l1_norm", "reduction_minimum",
    "reduction_maximum", "minimum_index", "maximum_index", "square_root", "convert",
}


@pytest.fixture(params=("_mp64_accel", "_megaforth_native"))
def native_tile(request):
    module = importlib.import_module(request.param)
    assert module.TILE_VALUES_API_VERSION == 1
    return module


def _pack(width, lanes):
    return bytes(tf.pack_bits(width, lanes))


def _raw_tile(ew, variant="edge", *, size=64, seed=0):
    lane = formats.decode(ew)
    rng = random.Random(f"tile/{ew}/{variant}/{seed}/{size}")
    values = [rng.getrandbits(lane.lane_bits) for _ in range(size // lane.lane_bytes)]
    if variant == "edge":
        if lane.is_float:
            f = lane.float_format
            edge = (0, f.sign_bit, 1, f.sign_bit | 1, f.max_finite,
                    f.infinity, f.canonical_nan, f.sign_bit | f.infinity | 1)
        else:
            top = 1 << (lane.lane_bits - 1)
            edge = (0, 1, lane.lane_mask, top, top - 1, top + 1, 3, 7)
        values[:len(edge)] = edge
    return _pack(lane.lane_bits, values)


def _signed(value, bits):
    return value - (1 << bits) if value & (1 << (bits - 1)) else value


def _supports(operation, mode, argument=0):
    lane = formats.decode(mode)
    if lane is None:
        return False
    if operation == "convert":
        target = formats.decode(argument)
        return bool(target is not None and lane.ew != target.ew
                    and (lane.is_float or target.is_float))
    if operation == "compare_mask":
        return 0 <= argument <= 7
    if argument:
        return False
    if operation in ELEMENTWISE or operation in ("multiply", "select"):
        return True
    return bool(operation in FLOAT_OPERATIONS and lane.is_float
                and not (operation == "widening_multiply" and lane.ew == 7))


def _oracle(operation, mode, source0, source1=b"", destination=b"", argument=0):
    lane = formats.decode(mode)
    a = tf.unpack_bits(lane.lane_bits, source0)
    b = tf.unpack_bits(lane.lane_bits, source1)
    c = tf.unpack_bits(lane.lane_bits, destination)
    signed = bool(mode & formats.TMODE_SIGNED)
    if operation in ELEMENTWISE or operation == "multiply":
        if lane.is_float:
            values = (tf.multiply(lane.float_format, a, b) if operation == "multiply"
                      else tf.elementwise(lane.float_format, ELEMENTWISE[operation], a, b))
        else:
            values = []
            low, high = -(1 << (lane.lane_bits - 1)), (1 << (lane.lane_bits - 1)) - 1
            for index, raw_x in enumerate(a):
                raw_y = b[index] if b else 0
                x = _signed(raw_x, lane.lane_bits) if signed else raw_x
                y = _signed(raw_y, lane.lane_bits) if signed else raw_y
                if operation in ("add", "subtract"):
                    result = x + y if operation == "add" else x - y
                    if mode & formats.TMODE_SATURATE:
                        result = max(low, min(high, result)) if signed else max(0, min(lane.lane_mask, result))
                elif operation == "multiply":
                    result = x * y
                elif operation == "minimum":
                    result = min(x, y)
                elif operation == "maximum":
                    result = max(x, y)
                elif operation == "absolute":
                    result = abs(x)
                elif operation == "bitwise_and":
                    result = raw_x & raw_y
                elif operation == "bitwise_or":
                    result = raw_x | raw_y
                else:
                    result = raw_x ^ raw_y
                values.append(result & lane.lane_mask)
        return _pack(lane.lane_bits, values)
    if operation == "select":
        return _pack(lane.lane_bits, tf.select(lane, c, a, b))
    if operation == "compare_mask":
        return _pack(lane.lane_bits, tf.compare_mask(lane, argument, a, b, signed))
    if operation == "convert":
        target = formats.decode(argument)
        return _pack(target.lane_bits, tf.convert_region(
            lane, target, a, signed, bool(mode & formats.TMODE_ROUNDING),
        ))
    if operation == "divide":
        return _pack(lane.lane_bits, tf.divide(lane, a, b))
    if operation == "square_root":
        return _pack(lane.lane_bits, tf.square_root(lane, a))
    f = lane.float_format
    if operation == "fused_multiply_add":
        return _pack(f.width, tf.fused_multiply_add(f, a, b, c))
    if operation == "widening_multiply":
        return _pack(lane.accumulation.width, tf.widening_multiply(f, a, b))
    if operation == "dot":
        return tf.dot(f, a, b)
    if operation == "dot_chunks":
        return tuple(tf.dot_chunks(f, a, b))
    if operation == "sum":
        return tf.sum_lanes(f, a)
    if operation == "sum_squares":
        return tf.sum_squares(f, a)
    if operation == "l1_norm":
        return tf.l1_norm(f, a)
    largest = operation in ("reduction_maximum", "maximum_index")
    if operation in ("minimum_index", "maximum_index"):
        return tf.extreme_index(f, a, largest)
    return tf.extreme(f, a, largest)


def test_native_support_table_matches_the_deliberately_bounded_extraction(native_tile):
    for mode in range(128):
        for operation in OPERATIONS:
            arguments = range(8) if operation in ("convert", "compare_mask") else (0, 1)
            for argument in arguments:
                assert native_tile.tile_values_supported(operation, mode, argument) is _supports(
                    operation, mode, argument,
                ), (operation, mode, argument)
    for operation in ("transpose", "popcount", "multiply_accumulate", "unknown"):
        assert not native_tile.tile_values_supported(operation, 0)


@pytest.mark.parametrize("ew", range(8))
@pytest.mark.parametrize("variant", ("edge", "seeded"))
def test_supported_lane_operations_match_raw_python_values(native_tile, ew, variant):
    source0 = _raw_tile(ew, variant, seed=1)
    source1 = _raw_tile(ew, variant, seed=2)
    destination = _raw_tile(ew, variant, seed=3)
    flags = (0, 0x70) if ew >= 4 else (0, 0x10, 0x20, 0x30)
    for mode_flags in flags:
        mode = ew | mode_flags
        for operation in OPERATIONS:
            if operation in ("convert", "compare_mask") or not _supports(operation, mode):
                continue
            right = b"" if operation in UNARY else source1
            existing = destination if operation in ("select", "fused_multiply_add") else b""
            expected = _oracle(operation, mode, source0, right, existing)
            assert native_tile.tile_execute_values(operation, mode, source0, right, existing) == expected, (
                operation, mode, variant,
            )


@pytest.mark.parametrize("ew", range(8))
def test_all_compare_predicates_cover_nan_zero_signed_and_unsigned_order(native_tile, ew):
    left = _raw_tile(ew, seed=11)
    right = left[:32] + _raw_tile(ew, "seeded", seed=12)[32:]
    for signed in (0, formats.TMODE_SIGNED):
        mode = ew | signed
        for predicate in range(8):
            expected = _oracle("compare_mask", mode, left, right, argument=predicate)
            assert native_tile.tile_execute_values(
                "compare_mask", mode, left, right, b"", predicate,
            ) == expected


@pytest.mark.parametrize("source", range(8))
def test_every_admitted_conversion_covers_whole_region_and_rounding_controls(native_tile, source):
    source_format = formats.decode(source)
    for target in range(8):
        if not _supports("convert", source, target):
            continue
        target_format = formats.decode(target)
        ratio = formats.tcvt_ratio(source_format, target_format)
        reads = ratio if target_format.lane_bytes < source_format.lane_bytes else 1
        region = _raw_tile(source, size=64 * reads, seed=target)
        for flags in (0, 0x10, 0x40, 0x70):
            mode = source | flags
            expected = _oracle("convert", mode, region, argument=target)
            actual = native_tile.tile_execute_values("convert", mode, region, b"", b"", target)
            assert actual == expected, (source, target, flags)
            writes = ratio if target_format.lane_bytes > source_format.lane_bytes else 1
            assert len(actual) == 64 * writes


@pytest.mark.parametrize("ew", (4, 5, 6, 7))
def test_reductions_round_in_balanced_lane_order_with_pinned_cancellation(native_tile, ew):
    f = formats.decode(ew).float_format
    large = {4: 65504.0, 5: 1.5 * 2.0 ** 30,
             6: 1.5 * 2.0 ** 60, 7: 1.5 * 2.0 ** 53}[ew]
    small = 2.0 ** -24 if ew == 4 else 1.0
    values = [fp.from_double(f, x) for x in (large, small, -large, small)]
    values += [0] * (64 // (f.width // 8) - len(values))
    tile = _pack(f.width, values)
    # Each pair loses its small contribution before the two large values cancel.
    assert native_tile.tile_execute_values("sum", ew, tile) == 0
    ones = _pack(f.width, [fp.from_double(f, 1.0)] * len(values))
    assert native_tile.tile_execute_values("dot", ew, tile, ones) == 0


@pytest.mark.parametrize("ew", (4, 5, 6, 7))
def test_subnormal_zero_and_single_rounding_fma_bits_are_not_flushed(native_tile, ew):
    f = formats.decode(ew).float_format
    count = 64 // (f.width // 8)
    smallest = _pack(f.width, [1] * count)
    zeros = bytes(64)
    ones = _pack(f.width, [fp.from_double(f, 1.0)] * count)
    assert native_tile.tile_execute_values("add", ew, smallest, zeros) == smallest
    assert native_tile.tile_execute_values("multiply", ew, smallest, ones) == smallest
    assert native_tile.tile_execute_values("fused_multiply_add", ew, smallest, ones, zeros) == smallest
    assert native_tile.tile_execute_values("divide", ew, smallest, ones) == smallest
    # (1 + 2^-p) * (1 - 2^-p) - 1 must retain the exact fused residual.
    unit = fp.from_double(f, 1.0)
    a, b, c = unit + 1, unit - 2, unit | f.sign_bit
    left, right, addend = (_pack(f.width, [value] * count) for value in (a, b, c))
    expected = fp.fma(f, a, b, c)[0]
    assert expected != 0
    assert native_tile.tile_execute_values("fused_multiply_add", ew, left, right, addend) == _pack(
        f.width, [expected] * count,
    )


@pytest.mark.parametrize("ew", (4, 5, 6, 7))
def test_nan_extremes_keep_first_tie_and_signed_zero_order(native_tile, ew):
    lane = formats.decode(ew)
    f, wide = lane.float_format, lane.accumulation
    nan = f.sign_bit | f.infinity | 1
    all_nan = _pack(f.width, [nan] * lane.lanes)
    assert native_tile.tile_execute_values("minimum_index", ew, all_nan) == (0, wide.canonical_nan)
    assert native_tile.tile_execute_values("maximum_index", ew, all_nan) == (0, wide.canonical_nan)
    values = [nan, 0, f.sign_bit, f.sign_bit] + [nan] * (lane.lanes - 4)
    mixed = _pack(f.width, values)
    assert native_tile.tile_execute_values("minimum_index", ew, mixed) == (2, wide.sign_bit)
    assert native_tile.tile_execute_values("maximum_index", ew, mixed) == (1, 0)
    finite = _pack(f.width, [0] * lane.lanes)
    assert native_tile.tile_execute_values("minimum", ew, all_nan, finite) == _pack(
        f.width, [f.canonical_nan] * lane.lanes,
    )
    assert native_tile.tile_execute_values("absolute", ew, all_nan) == _pack(
        f.width, [nan & ~f.sign_bit] * lane.lanes,
    )


@pytest.mark.parametrize("mode", range(8, 16))
def test_reserved_formats_are_rejected_before_computation(native_tile, mode):
    with pytest.raises(ValueError):
        native_tile.tile_execute_values("add", mode, bytes(64), bytes(64))


@pytest.mark.parametrize("arguments", (
    ("add", 0, bytes(63), bytes(64)),
    ("add", 0, bytes(64), bytes(65)),
    ("add", 0, bytes(64), bytes(64), bytes(64)),
    ("fused_multiply_add", 7, bytes(64), bytes(64), bytes(63)),
    ("sum", 7, bytes(64), bytes(1)),
    ("convert", 7, bytes(64), b"", b"", 0),
    ("convert", 0, bytes(64), b"", b"", 1),
    ("widening_multiply", 7, bytes(64), bytes(64)),
    ("dot", 3, bytes(64), bytes(64)),
    ("compare_mask", 7, bytes(64), bytes(64), b"", 8),
))
def test_operand_geometry_and_unextracted_operations_fail_closed(native_tile, arguments):
    with pytest.raises(ValueError):
        native_tile.tile_execute_values(*arguments)


@pytest.mark.parametrize("bad", (True, 1.5, "7"))
def test_mode_requires_an_exact_host_integer(native_tile, bad):
    with pytest.raises(TypeError):
        native_tile.tile_execute_values("sum", bad, bytes(64))


@pytest.mark.parametrize("bad", (bytearray(64), memoryview(bytes(64)), [0] * 64))
def test_value_payloads_are_immutable_bytes(native_tile, bad):
    with pytest.raises(TypeError):
        native_tile.tile_execute_values("sum", 7, bad)


def test_native_value_boundary_uses_rne_and_restores_callers_rounding_mode(native_tile):
    if platform.system() != "Linux" or platform.machine().lower() not in ("x86_64", "amd64"):
        pytest.skip("the directed fenv fixture uses the Linux x86 FE_UPWARD constant")
    library = ctypes.CDLL(None)
    try:
        get_round, set_round = library.fegetround, library.fesetround
    except AttributeError:
        pytest.skip("host libc does not expose fenv rounding controls")
    get_round.argtypes, get_round.restype = [], ctypes.c_int
    set_round.argtypes, set_round.restype = [ctypes.c_int], ctypes.c_int
    original = get_round()
    upward = 0x800
    one = (0x3FF0000000000000).to_bytes(8, "little") * 8
    half_ulp = (0x3CA0000000000000).to_bytes(8, "little") * 8
    if original < 0 or set_round(upward):
        pytest.skip("host fenv does not admit FE_UPWARD")
    try:
        actual = native_tile.tile_execute_values("add", 7, one, half_ulp)
        after_success = get_round()
        with pytest.raises(ValueError):
            native_tile.tile_execute_values("add", 8, one, half_ulp)
        after_error = get_round()
    finally:
        set_round(original)
    assert actual == one
    assert after_success == after_error == upward
