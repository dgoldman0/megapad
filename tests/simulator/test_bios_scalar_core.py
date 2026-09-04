"""Focused BIOS scalar semantics needed by the KDOS allocator frontier."""

from __future__ import annotations

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ExecutionError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.stacks import StackUnderflow


def test_three_cell_rotations_match_bios_stack_order() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    for value in (11, 22, 33):
        context.data.push(value)
    runtime.execute("ROT", context=context)
    assert context.data.snapshot() == (22, 33, 11)

    runtime.execute("-ROT", context=context)
    assert context.data.snapshot() == (11, 22, 33)


@pytest.mark.parametrize(
    ("offset", "expected"),
    (
        (0, (10, 20, 30, 40)),
        (1, (10, 20, 40, 30)),
        (2, (10, 30, 40, 20)),
        (3, (20, 30, 40, 10)),
    ),
)
def test_roll_moves_the_selected_cell_to_the_top(
    offset: int,
    expected: tuple[int, ...],
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    for value in (10, 20, 30, 40, offset):
        context.data.push(value)

    runtime.execute("ROLL", context=context)

    assert context.data.snapshot() == expected


def test_roll_consumes_its_offset_before_a_guarded_underflow() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(99)
    context.data.push(2)

    with pytest.raises(StackUnderflow, match="data stack underflow"):
        runtime.execute("ROLL", context=context)

    assert context.data.snapshot() == (99,)


def test_allocator_logic_words_use_full_width_cells_and_signed_less_than() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0xF000 0x0FF0 OR "
        b"0xAAAA 0x0FF0 XOR "
        b"-1 0 < "
        b"-1 0 <> "
        b"0 0<> "
        b"7 0<>",
        context=context,
    )

    assert context.data.snapshot() == (
        0xFFF0,
        0xA55A,
        TRUE,
        TRUE,
        0,
        TRUE,
    )


@pytest.mark.parametrize(
    ("dividend", "divisor", "expected"),
    (
        (7, 3, 2),
        (-7, 3, -2),
        (7, -3, -2),
        (-7, -3, 2),
        ((1 << 63) - 1, 3, ((1 << 63) - 1) // 3),
    ),
)
def test_signed_division_is_exact_and_truncates_toward_zero(
    dividend: int,
    divisor: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(dividend)
    context.data.push(divisor)

    runtime.execute("/", context=context)

    assert context.data.snapshot() == (expected & MASK64,)


@pytest.mark.parametrize(
    ("dividend", "divisor"),
    ((1, 0), (-(1 << 63), -1)),
)
def test_signed_division_reports_architectural_traps(
    dividend: int,
    divisor: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(dividend)
    context.data.push(divisor)

    with pytest.raises(ExecutionError, match="division trapped"):
        runtime.execute("/", context=context)


@pytest.mark.parametrize(
    ("dividend", "divisor", "expected"),
    (
        (7, 3, 1),
        (-7, 3, -1),
        (7, -3, 1),
        (-7, -3, -1),
        ((1 << 63) - 1, -3, 1),
        (-(1 << 63), -1, 0),
    ),
)
def test_signed_modulo_is_exact_and_truncates_toward_zero(
    dividend: int,
    divisor: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(dividend)
    context.data.push(divisor)

    runtime.execute("MOD", context=context)

    assert context.data.snapshot() == (expected & MASK64,)


def test_signed_modulo_reports_zero_divisor() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(1)
    context.data.push(0)

    with pytest.raises(ExecutionError, match="modulo trapped"):
        runtime.execute("MOD", context=context)


@pytest.mark.parametrize(
    ("value", "expected"),
    (
        (0, 0),
        (1, MASK64),
        (MASK64, 1),
        (1 << 63, 1 << 63),
    ),
)
def test_negate_is_twos_complement_cell_negation(
    value: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(value)

    runtime.execute("NEGATE", context=context)

    assert context.data.snapshot() == (expected,)


@pytest.mark.parametrize(
    ("name", "left", "right", "expected"),
    (
        ("MIN", -1, 1, MASK64),
        ("MIN", 1, -1, MASK64),
        ("MIN", -(1 << 63), (1 << 63) - 1, 1 << 63),
        ("MAX", -1, 1, 1),
        ("MAX", 1, -1, 1),
        ("MAX", -(1 << 63), (1 << 63) - 1, (1 << 63) - 1),
    ),
)
def test_min_and_max_compare_signed_cells(
    name: str,
    left: int,
    right: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(left)
    context.data.push(right)

    runtime.execute(name, context=context)

    assert context.data.snapshot() == (expected,)


@pytest.mark.parametrize(
    ("value", "expected"),
    (
        (10, 5),
        (1, 0),
        (-1, MASK64),
        (-3, MASK64 - 1),
        (-(1 << 63), 0xC000_0000_0000_0000),
    ),
)
def test_two_divide_is_an_arithmetic_right_shift(
    value: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(value)

    runtime.execute("2/", context=context)

    assert context.data.snapshot() == (expected,)


def test_true_false_cr_and_talign_are_real_bios_words() -> None:
    runtime = MegaForthRuntime()
    original_here = runtime.dictionary.here

    runtime.evaluate(b"TRUE FALSE CR TALIGN")

    assert runtime.main_context.data.snapshot() == (TRUE, 0)
    assert runtime.drain_uart_output() == b"\r\n"
    assert runtime.dictionary.here == (original_here + 63) & ~63


@pytest.mark.parametrize(
    ("address", "expected"),
    (
        (0, 8),
        (0x1020_3040_5060_7080, 0x1020_3040_5060_7088),
        (MASK64, 7),
    ),
)
def test_cell_plus_advances_one_wrapping_machine_cell(
    address: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(address)

    runtime.execute("CELL+", context=context)

    assert context.data.snapshot() == (0xCAFE, expected)


def test_bank0_dictionary_reports_the_optional_user_interval_as_disabled() -> None:
    runtime = MegaForthRuntime()

    runtime.evaluate(b"DICT-BASE@ DICT-LIMIT@")

    assert runtime.dictionary_base == 0
    assert runtime.dictionary_limit == 0
    assert runtime.main_context.data.snapshot() == (0, 0)


def test_talign_rejects_growth_above_the_guarded_bank0_ceiling_atomically() -> None:
    memory = create_one_core_address_space(bank0_size=0x1_0010)
    runtime = MegaForthRuntime(memory=memory)

    def escape_dictionary_fault(_context) -> None:
        raise ExecutionError("guarded dictionary capacity")

    hook = runtime.define_primitive(
        "HOST-DICTIONARY-FAULT",
        escape_dictionary_fault,
    )
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    guarded_ceiling = runtime.main_context.data.pointer - 256
    runtime.dictionary.allot(
        guarded_ceiling + 1 - runtime.dictionary.here
    )
    here = runtime.dictionary.here

    with pytest.raises(ExecutionError, match="guarded dictionary capacity"):
        runtime.execute("TALIGN")

    assert runtime.dictionary.here == here
