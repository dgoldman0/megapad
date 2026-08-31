"""Source-level tests for the first BIOS-owned hosted primitives."""

from __future__ import annotations

import pytest

from shared.cells import FALSE, MASK64, TRUE
from simulator.memory import UnmappedAddressError
from simulator.runtime import MegaForthRuntime


def test_compiled_constant_parses_input_and_keeps_its_bound_execution_token() -> None:
    runtime = MegaForthRuntime()
    core_constant = runtime.find("CONSTANT")
    assert core_constant is not None
    assert core_constant.immediate is False

    result = runtime.evaluate(
        b": DEFINE-CONSTANT CONSTANT ;\n"
        b"123 CONSTANT CONSTANT\n"
        b"0xFEDCBA9876543210 DEFINE-CONSTANT ANSWER\n"
        b": READ-ANSWER ANSWER ;\n"
        b"7 DEFINE-CONSTANT ANSWER\n"
    )

    assert [word.name for word in result.definitions] == [
        b"DEFINE-CONSTANT",
        b"CONSTANT",
        b"ANSWER",
        b"READ-ANSWER",
        b"ANSWER",
    ]
    assert runtime.find("CONSTANT") is not core_constant

    live_constant = runtime.new_context()
    runtime.execute("CONSTANT", context=live_constant)
    assert live_constant.data.snapshot() == (123,)

    old_binding = runtime.new_context()
    runtime.execute("READ-ANSWER", context=old_binding)
    assert old_binding.data.snapshot() == (0xFEDC_BA98_7654_3210,)

    live_binding = runtime.new_context()
    runtime.execute("ANSWER", context=live_binding)
    assert live_binding.data.snapshot() == (7,)


def test_multiply_one_minus_and_rshift_follow_mp64_cell_rules() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0xFFFFFFFFFFFFFFFF 2 * "
        b"0 1- "
        b"0x8000000000000000 63 RSHIFT "
        b"0x8000000000000001 64 RSHIFT "
        b"0x8000000000000000 65 RSHIFT",
        context=context,
    )

    assert context.data.snapshot() == (
        MASK64 - 1,
        MASK64,
        1,
        0x8000_0000_0000_0001,
        0x4000_0000_0000_0000,
    )


def test_unsigned_and_signed_comparisons_cover_cell_boundaries() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0xFFFFFFFFFFFFFFFF 0 U> "
        b"0 0xFFFFFFFFFFFFFFFF U> "
        b"0xFFFFFFFFFFFFFFFF 0xFFFFFFFFFFFFFFFF U> "
        b"0x8000000000000000 0x7FFFFFFFFFFFFFFF <= "
        b"0x7FFFFFFFFFFFFFFF 0x8000000000000000 <= "
        b"0x7FFFFFFFFFFFFFFF 0x8000000000000000 >= "
        b"0x8000000000000000 0x7FFFFFFFFFFFFFFF >= "
        b"0x8000000000000000 0x8000000000000000 >=",
        context=context,
    )

    assert context.data.snapshot() == (
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        TRUE,
        FALSE,
        TRUE,
    )


def test_unaligned_fetch_store_and_plus_store_share_wrapping_guest_memory() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0x8877665544332211 3 ! "
        b"3 @ "
        b"0xFFFFFFFFFFFFFFFF 19 ! "
        b"2 19 +! "
        b"19 @",
        context=context,
    )

    assert runtime.memory.read_bytes(3, 8) == bytes.fromhex(
        "11 22 33 44 55 66 77 88"
    )
    assert context.data.snapshot() == (0x8877_6655_4433_2211, 1)


def test_byte_store_masks_to_the_low_byte_and_roundtrips_through_c_fetch() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b"0x1AB 7 C! 7 C@", context=context)

    assert runtime.memory.read_bytes(6, 3) == bytes.fromhex("00 AB 00")
    assert context.data.snapshot() == (0xAB,)


def test_fill_uses_addr_count_low_byte_order_in_the_shared_address_space() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b"32 5 0x1A5 FILL 32 @", context=context)

    assert runtime.memory.read_bytes(31, 7) == bytes.fromhex(
        "00 A5 A5 A5 A5 A5 00"
    )
    assert context.data.snapshot() == (0x0000_00A5_A5A5_A5A5,)


def test_memory_fault_in_a_colon_word_restores_internal_return_stack() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    runtime.evaluate(b": FAIL-FETCH 99 >R 0x100000 @ ; : STILL-USABLE 6 7 * ;")

    with pytest.raises(UnmappedAddressError):
        runtime.execute("FAIL-FETCH", context=context)

    assert context.returns.snapshot() == ()
    assert context.data.snapshot() == (0x100000,)
    runtime.execute("STILL-USABLE", context=context)
    assert context.data.snapshot() == (0x100000, 42)
    assert context.returns.snapshot() == ()


def test_count_fault_preserves_its_unread_counted_string_address() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0x100000)

    with pytest.raises(UnmappedAddressError):
        runtime.execute("COUNT", context=context)

    assert context.data.snapshot() == (0x100000,)
    assert context.returns.snapshot() == ()
