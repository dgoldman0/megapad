"""Focused BIOS scalar semantics needed by the KDOS allocator frontier."""

from __future__ import annotations

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace
from simulator.runtime import MegaForthRuntime


def test_three_cell_rotations_match_bios_stack_order() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    for value in (11, 22, 33):
        context.data.push(value)
    runtime.execute("ROT", context=context)
    assert context.data.snapshot() == (22, 33, 11)

    runtime.execute("-ROT", context=context)
    assert context.data.snapshot() == (11, 22, 33)


def test_allocator_logic_words_use_full_width_cells_and_signed_less_than() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0xF000 0x0FF0 OR "
        b"-1 0 < "
        b"-1 0 <> "
        b"0 0<> "
        b"7 0<>",
        context=context,
    )

    assert context.data.snapshot() == (
        0xFFF0,
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


def test_max_preserves_current_unsigned_executable_bios_behavior() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(-1)
    context.data.push(1)

    runtime.execute("MAX", context=context)

    assert context.data.snapshot() == (MASK64,)


def test_true_false_cr_and_talign_are_real_bios_words() -> None:
    runtime = MegaForthRuntime()
    original_here = runtime.dictionary.here

    runtime.evaluate(b"TRUE FALSE CR TALIGN")

    assert runtime.main_context.data.snapshot() == (TRUE, 0)
    assert runtime.drain_uart_output() == b"\r\n"
    assert runtime.dictionary.here == (original_here + 63) & ~63


def test_bank0_dictionary_reports_the_optional_user_interval_as_disabled() -> None:
    runtime = MegaForthRuntime()

    runtime.evaluate(b"DICT-BASE@ DICT-LIMIT@")

    assert runtime.dictionary_base == 0
    assert runtime.dictionary_limit == 0
    assert runtime.main_context.data.snapshot() == (0, 0)


def test_talign_rejects_a_target_beyond_the_dictionary_region_atomically() -> None:
    memory = SparseAddressSpace(bank0_size=0x1_0010)
    runtime = MegaForthRuntime(memory=memory)
    bank0 = memory.regions[0]
    runtime.dictionary.allot(bank0.limit - runtime.dictionary.here - 1)
    here = runtime.dictionary.here

    with pytest.raises(OverflowError, match="memory region"):
        runtime.execute("TALIGN")

    assert runtime.dictionary.here == here
