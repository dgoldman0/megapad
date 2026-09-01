"""Source-level tests for the first BIOS-owned hosted primitives."""

from __future__ import annotations

import pytest

from shared.cells import FALSE, MASK64, TRUE
from simulator.memory import (
    MMIO_BASE,
    CrossRegionAccessError,
    MMIOAccessError,
    UnmappedAddressError,
)
from simulator.platform import (
    BOARD_ID_VERSION,
    SYSINFO_OFFSET,
    create_one_core_address_space,
)
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


def test_off_zeroes_one_unaligned_cell_and_consumes_its_address() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    runtime.memory.write_bytes(
        2,
        bytes.fromhex("AA 11 22 33 44 55 66 77 88 BB"),
    )
    context.data.push(3)

    runtime.execute("OFF", context=context)

    assert runtime.memory.read_bytes(2, 10) == b"\xAA" + bytes(8) + b"\xBB"
    assert context.data.snapshot() == ()


def test_off_consumes_the_address_before_a_crossing_store_fault() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    bank0 = runtime.memory.regions[0]
    crossing = bank0.limit - 4
    sentinel = bytes.fromhex("11 22 33 44")
    runtime.memory.write_bytes(crossing, sentinel)
    context.data.push(crossing)

    with pytest.raises(CrossRegionAccessError):
        runtime.execute("OFF", context=context)

    assert runtime.memory.read_bytes(crossing, len(sentinel)) == sentinel
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_byte_store_masks_to_the_low_byte_and_roundtrips_through_c_fetch() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b"0x1AB 7 C! 7 C@", context=context)

    assert runtime.memory.read_bytes(6, 3) == bytes.fromhex("00 AB 00")
    assert context.data.snapshot() == (0xAB,)


def test_little_endian_word_and_long_memory_words_are_unaligned_and_masked() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0x1A2B3 3 W! 3 W@ "
        b"0x1FEDCBA98 9 L! 9 L@",
        context=context,
    )

    assert runtime.memory.read_bytes(2, 12) == bytes.fromhex(
        "00 B3 A2 00 00 00 00 98 BA DC FE 00"
    )
    assert context.data.snapshot() == (0xA2B3, 0xFEDC_BA98)


def test_little_endian_fetches_route_unaligned_sysinfo_as_separate_bytes() -> None:
    memory = create_one_core_address_space()
    runtime = MegaForthRuntime(memory=memory)
    address = MMIO_BASE + SYSINFO_OFFSET + 1

    with pytest.raises(MMIOAccessError, match="preflight"):
        memory.read16(address)

    assert runtime.evaluate(
        f"{address} W@ {address} L@".encode("ascii")
    ).semantic_steps > 0
    assert runtime.main_context.data.snapshot() == (
        (BOARD_ID_VERSION >> 8) & 0xFFFF,
        (BOARD_ID_VERSION >> 8) & 0xFFFF_FFFF,
    )


@pytest.mark.parametrize(("word", "width"), (("W@", 2), ("L@", 4)))
def test_little_endian_fetch_fault_preserves_its_address(
    word: str,
    width: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    bank0 = runtime.memory.regions[0]
    crossing = bank0.limit - width + 1
    context.data.push(crossing)

    with pytest.raises(UnmappedAddressError):
        runtime.execute(word, context=context)

    assert context.data.snapshot() == (crossing,)
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize(("word", "width"), (("W!", 2), ("L!", 4)))
def test_little_endian_store_fault_consumes_inputs_after_partial_write(
    word: str,
    width: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    bank0 = runtime.memory.regions[0]
    crossing = bank0.limit - width + 1
    sentinel = bytes((0x50 + index for index in range(width - 1)))
    runtime.memory.write_bytes(crossing, sentinel)
    context.data.push(0xFEDC_BA98_7654_3210)
    context.data.push(crossing)

    with pytest.raises(UnmappedAddressError):
        runtime.execute(word, context=context)

    expected = (0xFEDC_BA98_7654_3210).to_bytes(8, "little")[: width - 1]
    assert runtime.memory.read_bytes(crossing, width - 1) == expected
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


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
