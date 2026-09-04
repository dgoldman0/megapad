"""Initial hosted implementations of the public MegaForth core vocabulary."""

from __future__ import annotations

from typing import Callable

from shared.cells import CELL_BYTES, MASK64, forth_flag, s64, u64
from shared.storage import STORAGE_RESULT_TIMEOUT
from simulator.aes import (
    AES_AAD_LENGTH,
    AES_COMMAND,
    AES_DATA_INPUT,
    AES_DATA_LENGTH,
    AES_DATA_OUTPUT,
    AES_IV,
    AES_KEY,
    AES_KEY_MODE,
    AES_STATUS,
    AES_TAG,
)
from simulator.dictionary import MAX_NAME_BYTES
from simulator.errors import ExecutionError, ForthAbort
from simulator.entropy import (
    TRNG_RAND8,
    TRNG_RAND64,
    TRNG_SEED,
    TRNG_STATUS,
    TRNGUnavailableError,
)
from simulator.ir import Branch, BranchZero, Idle, Return, UartReadAttempt
from simulator.memory import MMIO_BASE, SparseAddressSpace
from simulator.mp64fs import validate_attached_mp64fs
from simulator.platform import (
    SYSINFO_CRYPTO_CAPS,
    SYSINFO_EXTERNAL_BASE,
    SYSINFO_EXTERNAL_SIZE,
    SYSINFO_HBW_BASE,
    SYSINFO_HBW_SIZE,
    SYSINFO_NUM_CORES,
    SYSINFO_NUM_FULL,
)
from simulator.rtc import (
    RTC_EPOCH,
    RTC_EPOCH_SIZE,
    RTC_UPTIME,
    RTC_UPTIME_SIZE,
)
from simulator.runtime import (
    CreatedDefinition,
    DirectiveKind,
    ExecutionContext,
    Invoke,
    MegaForthRuntime,
)
from simulator.sha3 import SHA3_CONTROL, SHA3_STATUS


_EVAL_TOKEN_BYTES = 256
_CLUSTER_SPAD_ADDRESS = 0xFFFF_FE00_0000_0000
_UNCONFIGURED_NETWORK_MAC_BYTES = 6
_ENTROPY_OK = 0
_ENTROPY_UNAVAILABLE = 1


def _dup(context: ExecutionContext) -> None:
    context.data.push(context.data.peek())


def _drop(context: ExecutionContext) -> None:
    context.data.pop()


def _swap(context: ExecutionContext) -> None:
    top = context.data.pop()
    below = context.data.pop()
    context.data.push(top)
    context.data.push(below)


def _over(context: ExecutionContext) -> None:
    context.data.push(context.data.peek(1))


def _nip(context: ExecutionContext) -> None:
    top = context.data.pop()
    context.data.pop()
    context.data.push(top)


def _tuck(context: ExecutionContext) -> None:
    top = context.data.pop()
    below = context.data.pop()
    context.data.push(top)
    context.data.push(below)
    context.data.push(top)


def _rotate(context: ExecutionContext) -> None:
    top = context.data.pop()
    middle = context.data.pop()
    bottom = context.data.pop()
    context.data.push(middle)
    context.data.push(top)
    context.data.push(bottom)


def _reverse_rotate(context: ExecutionContext) -> None:
    top = context.data.pop()
    middle = context.data.pop()
    bottom = context.data.pop()
    context.data.push(top)
    context.data.push(bottom)
    context.data.push(middle)


def _two_dup(context: ExecutionContext) -> None:
    below = context.data.peek(1)
    top = context.data.peek()
    context.data.push(below)
    context.data.push(top)


def _two_drop(context: ExecutionContext) -> None:
    context.data.pop()
    context.data.pop()


def _two_over(context: ExecutionContext) -> None:
    first = context.data.peek(3)
    second = context.data.peek(2)
    context.data.push(first)
    context.data.push(second)


def _two_swap(context: ExecutionContext) -> None:
    top = context.data.pop()
    third = context.data.pop()
    second = context.data.pop()
    bottom = context.data.pop()
    context.data.push(third)
    context.data.push(top)
    context.data.push(bottom)
    context.data.push(second)


def _question_dup(context: ExecutionContext) -> None:
    value = context.data.peek()
    if value != 0:
        context.data.push(value)


def _pick(context: ExecutionContext) -> None:
    offset = context.data.pop()
    context.data.push(context.data.peek(offset))


def _roll(context: ExecutionContext) -> None:
    offset = context.data.pop()
    if offset == 0:
        return
    selected = context.data.peek(offset)
    above = [context.data.pop() for _ in range(offset)]
    context.data.pop()
    for value in reversed(above):
        context.data.push(value)
    context.data.push(selected)


def _add(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left + right)


def _subtract(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left - right)


def _multiply(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(u64(left * right))


def _unsigned_multiply(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    product = left * right
    context.data.push(u64(product))
    context.data.push(u64(product >> 64))


def _byte_swap(context: ExecutionContext) -> None:
    value = context.data.pop()
    context.data.push(
        int.from_bytes(value.to_bytes(CELL_BYTES, "little"), "big")
    )


def _signed_divide(context: ExecutionContext) -> None:
    divisor = s64(context.data.pop())
    dividend = s64(context.data.pop())
    if divisor == 0 or (dividend == -(1 << 63) and divisor == -1):
        raise ExecutionError("signed division trapped on zero or overflow")
    quotient = abs(dividend) // abs(divisor)
    if (dividend < 0) != (divisor < 0):
        quotient = -quotient
    context.data.push(quotient)


def _signed_modulo(context: ExecutionContext) -> None:
    divisor = s64(context.data.pop())
    dividend = s64(context.data.pop())
    if divisor == 0:
        raise ExecutionError("signed modulo trapped on zero")
    quotient = abs(dividend) // abs(divisor)
    if (dividend < 0) != (divisor < 0):
        quotient = -quotient
    context.data.push(dividend - quotient * divisor)


def _signed_divmod(context: ExecutionContext) -> None:
    divisor = s64(context.data.pop())
    dividend = s64(context.data.pop())
    if divisor == 0 or (dividend == -(1 << 63) and divisor == -1):
        raise ExecutionError("signed /MOD trapped on zero or overflow")
    quotient = abs(dividend) // abs(divisor)
    if (dividend < 0) != (divisor < 0):
        quotient = -quotient
    context.data.push(dividend - quotient * divisor)
    context.data.push(quotient)


def _absolute(context: ExecutionContext) -> None:
    value = s64(context.data.pop())
    context.data.push(-value if value < 0 else value)


def _negate(context: ExecutionContext) -> None:
    context.data.replace_top(-context.data.peek())


def _minimum(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left if s64(left) <= s64(right) else right)


def _maximum(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left if s64(left) > s64(right) else right)


def _one_minus(context: ExecutionContext) -> None:
    context.data.push(u64(context.data.pop() - 1))


def _one_plus(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() + 1)


def _two_multiply(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() << 1)


def _two_divide(context: ExecutionContext) -> None:
    context.data.push(s64(context.data.pop()) >> 1)


def _and(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left & right)


def _or(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left | right)


def _xor(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left ^ right)


def _right_shift(context: ExecutionContext) -> None:
    count = context.data.pop()
    value = context.data.pop()
    context.data.push(value >> (count & 0x3F))


def _left_shift(context: ExecutionContext) -> None:
    count = context.data.pop()
    value = context.data.pop()
    context.data.push(u64(value << (count & 0x3F)))


def _invert(context: ExecutionContext) -> None:
    context.data.push(~context.data.pop())


def _equal(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(forth_flag(left == right))


def _not_equal(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(forth_flag(left != right))


def _zero_equal(context: ExecutionContext) -> None:
    context.data.push(forth_flag(context.data.pop() == 0))


def _zero_not_equal(context: ExecutionContext) -> None:
    context.data.push(forth_flag(context.data.pop() != 0))


def _zero_greater(context: ExecutionContext) -> None:
    context.data.push(forth_flag(s64(context.data.pop()) > 0))


def _zero_less(context: ExecutionContext) -> None:
    context.data.push(forth_flag(s64(context.data.pop()) < 0))


def _unsigned_less(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(forth_flag(left < right))


def _unsigned_greater(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(forth_flag(left > right))


def _signed_less(context: ExecutionContext) -> None:
    right = s64(context.data.pop())
    left = s64(context.data.pop())
    context.data.push(forth_flag(left < right))


def _signed_less_equal(context: ExecutionContext) -> None:
    right = s64(context.data.pop())
    left = s64(context.data.pop())
    context.data.push(forth_flag(left <= right))


def _signed_greater_equal(context: ExecutionContext) -> None:
    right = s64(context.data.pop())
    left = s64(context.data.pop())
    context.data.push(forth_flag(left >= right))


def _signed_greater(context: ExecutionContext) -> None:
    right = s64(context.data.pop())
    left = s64(context.data.pop())
    context.data.push(forth_flag(left > right))


def _within(context: ExecutionContext) -> None:
    high = context.data.pop()
    low = context.data.pop()
    value = context.data.pop()
    context.data.push(
        forth_flag(u64(value - low) < u64(high - low))
    )


def _fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    value = runtime.memory.read64(address)
    context.data.replace_top(value)


def _c_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    value = runtime.memory.read8(address)
    context.data.replace_top(value)


def _w_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    value = 0
    for offset in range(2):
        value |= runtime.memory.read8(u64(address + offset)) << (offset * 8)
    context.data.replace_top(value)


def _l_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    value = 0
    for offset in range(4):
        value |= runtime.memory.read8(u64(address + offset)) << (offset * 8)
    context.data.replace_top(value)


def _count(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    length = runtime.memory.read8(address)
    context.data.replace_top(address + 1)
    context.data.push(length)


def _ascii_upper(byte: int) -> int:
    if 0x61 <= byte <= 0x7A:
        return byte - 0x20
    return byte


def _find(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    counted_address = context.data.peek()
    length = runtime.memory.read8(counted_address)
    found = None

    # Hosted definitions retain canonical names in semantic metadata.  Walk
    # that live order explicitly so FIND still has the native linked search's
    # newest-first, length-first byte access behavior without interpreting the
    # semantic code slots as MP64 machine code.
    if 0 < length <= MAX_NAME_BYTES:
        for word in reversed(runtime.dictionary.words):
            if len(word.name) != length:
                continue
            for offset, candidate_byte in enumerate(word.name):
                query_byte = runtime.memory.read8(
                    u64(counted_address + 1 + offset)
                )
                if _ascii_upper(query_byte) != _ascii_upper(candidate_byte):
                    break
            else:
                found = word
                break

    context.data.require_push_capacity(1)
    if found is None:
        context.data.push(0)
        return
    context.data.replace_top(found.xt)
    context.data.push(1 if found.immediate else -1)


def _store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    value = context.data.pop()
    runtime.memory.write64(address, value)


def _off(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    runtime.memory.write64(address, 0)


def _on(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    runtime.memory.write64(address, MASK64)


def _two_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    second = context.data.pop()
    runtime.memory.write64(address, second)
    first = context.data.pop()
    runtime.memory.write64(u64(address + CELL_BYTES), first)


def _two_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.peek()
    first = runtime.memory.read64(u64(address + CELL_BYTES))
    context.data.replace_top(first)
    second = runtime.memory.read64(address)
    context.data.push(second)


def _c_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    value = context.data.pop()
    runtime.memory.write8(address, value)


def _w_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    value = context.data.pop()
    for offset in range(2):
        runtime.memory.write8(
            u64(address + offset),
            value >> (offset * 8),
        )


def _l_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    value = context.data.pop()
    for offset in range(4):
        runtime.memory.write8(
            u64(address + offset),
            value >> (offset * 8),
        )


def _plus_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    increment = context.data.pop()
    runtime.memory.write64(
        address,
        u64(runtime.memory.read64(address) + increment),
    )


def _fill(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = context.data.pop()
    length = context.data.pop()
    address = context.data.pop()
    runtime.memory.fill(address, length, value)


def _cmove(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    destination = context.data.pop()
    source = context.data.pop()
    runtime.memory.copy_forward(source, destination, length)


def _cmove_up(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    destination = context.data.pop()
    source = context.data.pop()
    runtime.memory.copy_backward(source, destination, length)


def _move(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    destination = context.data.pop()
    source = context.data.pop()
    runtime.memory.move(source, destination, length)


def _constant(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = context.data.pop()
    name = runtime.parse_required_input_word(b"CONSTANT")
    runtime.define_constant(name, value)


def _create(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    name = runtime.parse_required_input_word(b"CREATE")
    runtime.define_created(name)


def _variable(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    name = runtime.parse_required_input_word(b"VARIABLE")
    runtime.define_created(name, initial_body=bytes(CELL_BYTES))


def _here(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.dictionary.here)


def _latest(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.dictionary.latest)


def _dictionary_rollback(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    saved_latest = context.data.peek()
    saved_here = context.data.peek(1)
    runtime.rollback_dictionary(saved_here, saved_latest, context)
    context.data.pop()
    context.data.pop()


def _dictionary_fault_xt_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.set_dictionary_fault_xt(context.data.pop())


def _dictionary_index_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    slots = context.data.pop()
    base = context.data.pop()
    context.data.push(runtime.configure_dictionary_index(base, slots))


def _dictionary_index_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    state = runtime.dictionary_index_state
    context.data.push(state.base)
    context.data.push(state.slots)
    context.data.push(state.count)
    context.data.push(state.flags)


def _dictionary_base_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.dictionary_base)


def _dictionary_limit_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.dictionary_limit)


def _dictionary_bounds_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    limit = context.data.pop()
    base = context.data.pop()
    runtime.configure_dictionary_bounds(base, limit, context)


def _dictionary_bounds_off(
    runtime: MegaForthRuntime,
    _context: ExecutionContext,
) -> None:
    runtime.disable_dictionary_bounds()


def _tile_align(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.tile_align_dictionary(context)


def _tile_mode_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.tile.set_mode(context.data.pop())


def _tile_control_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.tile.set_control(context.data.pop())


def _tile_source0_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.tile.set_source0(context.data.pop())


def _tile_source1_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.tile.set_source1(context.data.pop())


def _tile_destination_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.tile.set_destination(context.data.pop())


def _tile_accumulator_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.tile.accumulator_word())


def _allot(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.allot_dictionary(context.data.pop(), context)


def _comma(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.comma_dictionary(context.data.pop(), context)


def _c_comma(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.c_comma_dictionary(context.data.pop(), context)


def _cells(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() << 3)


def _cell_plus(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() + CELL_BYTES)


def _depth(context: ExecutionContext) -> None:
    context.data.push(context.data.depth())


def _word(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    delimiter = context.data.pop()
    context.data.push(runtime.parse_word_to_dictionary_tail(delimiter))


def _char(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    token = runtime.parse_input_word()
    context.data.push(0 if not token else token[0])


def _stack_pointer_fetch(context: ExecutionContext) -> None:
    pointer = context.data.pointer
    context.data.push(pointer)


def _return_stack_pointer_fetch(context: ExecutionContext) -> None:
    context.data.push(context.returns.capture_pointer())


def _push_zero(context: ExecutionContext) -> None:
    context.data.push(0)


def _mpu_base_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.set_mpu_base(context.data.pop())


def _mpu_limit_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.set_mpu_limit(context.data.pop())


def _mpu_base_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.mpu_base)


def _mpu_limit_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.mpu_limit)


def _sysinfo_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    offset: int,
) -> None:
    context.data.push(runtime.memory.read64(MMIO_BASE + offset))


def _tick(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    name = runtime.parse_input_word()
    word = runtime.find(name) if name else None
    context.data.push(0 if word is None else word.xt)


def _to_body(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    xt = context.data.pop()
    try:
        word = runtime.dictionary.resolve(xt)
    except KeyError:
        raise ExecutionError(
            f">BODY requires a live CREATE-family execution token, got "
            f"0x{xt:016x}"
        ) from None
    if not isinstance(word.implementation, CreatedDefinition):
        raise ExecutionError(
            f">BODY requires a CREATE-family execution token, got {word.name!r}"
        )
    context.data.push(word.body_address)


def _compare(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    right_length = context.data.pop()
    right_address = context.data.pop()
    left_length = context.data.pop()
    left_address = context.data.pop()

    prefix_length = min(left_length, right_length)
    if prefix_length:
        left = runtime.memory.read_bytes(left_address, prefix_length)
        right = runtime.memory.read_bytes(right_address, prefix_length)
        if left != right:
            context.data.push(-1 if left < right else 1)
            return
    context.data.push((left_length > right_length) - (left_length < right_length))


def _abort(context: ExecutionContext) -> None:
    context.data.clear()
    context.returns.clear()
    raise ForthAbort("Forth ABORT", origin_context=context)


def _format_signed_cell(value: int, base: int) -> bytes:
    signed = s64(value)
    if signed < 0:
        return b"-" + _format_unsigned_cell(-signed, base)
    return _format_unsigned_cell(signed, base)


def _format_unsigned_cell(value: int, base: int) -> bytes:
    if not 2 <= base <= 36:
        raise ExecutionError(f"numeric output cannot render with base {base}")

    magnitude = u64(value)
    digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    rendered = bytearray()
    while True:
        magnitude, digit = divmod(magnitude, base)
        rendered.append(digits[digit])
        if magnitude == 0:
            break
    rendered.reverse()
    return bytes(rendered)


def _dot(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    rendered = _format_signed_cell(context.data.pop(), runtime.numeric_base)
    runtime.write_uart_bytes(rendered + b" ")


def _unsigned_dot(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    rendered = _format_unsigned_cell(context.data.pop(), runtime.numeric_base)
    runtime.write_uart_bytes(rendered + b" ")


def _carriage_return(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    runtime.write_uart_bytes(b"\r\n")


def _emit(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.write_uart_bytes(bytes((context.data.pop() & 0xFF,)))


def _type(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    address = context.data.pop()
    for _ in range(length):
        runtime.write_uart_bytes(bytes((runtime.memory.read8(address),)))
        address = u64(address + 1)


def _tx_flush(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    runtime.flush_uart_output()


def _terminal_size(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    cols, rows = runtime.terminal_size()
    context.data.push(cols)
    context.data.push(rows)


def _terminal_resize_request(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    rows = context.data.pop()
    cols = context.data.pop()
    runtime.request_terminal_resize(cols, rows)


def _space(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    runtime.write_uart_bytes(b" ")


def _key_query(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(forth_flag(runtime.uart_input_available))


def _timer_compare_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.timer.write_compare(context.data.pop())


def _timer_control_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.timer.write_control(context.data.pop())


def _epoch_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = 0
    for index in range(RTC_EPOCH_SIZE):
        value |= runtime.memory.read8(
            MMIO_BASE + RTC_EPOCH + index
        ) << (index * 8)
    context.data.push(value)


def _ms_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = 0
    for index in range(RTC_UPTIME_SIZE):
        value |= runtime.memory.read8(
            MMIO_BASE + RTC_UPTIME + index
        ) << (index * 8)
    context.data.push(value)


def _dot_zstr(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    while True:
        value = runtime.memory.read8(address)
        if value == 0:
            return
        runtime.write_uart_bytes(bytes((value,)))
        address = u64(address + 1)


def _uppercase_character(context: ExecutionContext) -> None:
    value = context.data.pop()
    if ord("a") <= value <= ord("z"):
        value -= ord("a") - ord("A")
    context.data.push(value)


def _semantic_noop(_context: ExecutionContext) -> None:
    """A source-visible capability toggle with no hosted native backend."""


def _i(context: ExecutionContext) -> None:
    context.data.push(context.returns.i())


def _j(context: ExecutionContext) -> None:
    context.data.push(context.returns.j())


def _execute(context: ExecutionContext) -> Invoke:
    return Invoke(context.data.pop())


def _crc_mode_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    status = runtime.crc.select_mode(
        runtime.guest_identity(context),
        context.data.pop(),
    )
    context.data.push(status)


def _crc_reset(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.crc.reset(runtime.guest_identity(context)))


def _crc_init_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    seed = context.data.pop()
    context.data.push(
        runtime.crc.seed(runtime.guest_identity(context), seed)
    )


def _crc_feed(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    cell = context.data.pop()
    context.data.push(
        runtime.crc.feed_cell(runtime.guest_identity(context), cell)
    )


def _crc_feed_byte(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    byte = context.data.pop()
    context.data.push(
        runtime.crc.feed_byte(runtime.guest_identity(context), byte)
    )


def _crc_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value, status = runtime.crc.fetch(runtime.guest_identity(context))
    context.data.push(value)
    context.data.push(status)


def _crc_raw_final_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    value, status = runtime.crc.raw_final(runtime.guest_identity(context))
    context.data.push(value)
    context.data.push(status)


def _crc_final_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.crc.final(runtime.guest_identity(context)))


def _aes_to_device(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    *,
    mmio_offset: int,
    length: int,
) -> None:
    address = context.data.pop()
    for index in range(length):
        value = runtime.memory.read8(u64(address + index))
        runtime.memory.write8(MMIO_BASE + mmio_offset + index, value)


def _aes_from_device(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    *,
    mmio_offset: int,
    length: int,
) -> None:
    address = context.data.pop()
    for index in range(length):
        value = runtime.memory.read8(MMIO_BASE + mmio_offset + index)
        runtime.memory.write8(u64(address + index), value)


def _aes_scalar_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    *,
    mmio_offset: int,
    width: int,
) -> None:
    value = context.data.pop()
    address = MMIO_BASE + mmio_offset
    if width == 1:
        runtime.memory.write8(address, value)
    elif width == 4:
        runtime.memory.write32(address, value)
    else:
        raise AssertionError("unsupported AES BIOS scalar width")


def _aes_status_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.memory.read8(MMIO_BASE + AES_STATUS))


def _caller_span_status(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    length = context.data.pop()
    address = context.data.pop()
    context.data.push(runtime.caller_span_status(context, address, length))


def _sha2_span_status(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    length = context.data.pop()
    address = context.data.pop()
    context.data.push(runtime.sha2.span_status(runtime.memory, address, length))


def _spin_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    lock_id = context.data.pop()
    if lock_id >= runtime.spinlocks.lock_count:
        raise ExecutionError("SPIN@ requires a lock ID from 0 through 15")
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(runtime.spinlocks.acquire(lock_id, core_id))


def _spin_release(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    lock_id = context.data.pop()
    if lock_id >= runtime.spinlocks.lock_count:
        raise ExecutionError("SPIN! requires a lock ID from 0 through 15")
    core_id, _task_id = runtime.guest_identity(context)
    runtime.spinlocks.release(lock_id, core_id)


def _wake_core_unavailable(context: ExecutionContext) -> None:
    core_id = context.data.peek()
    xt = context.data.peek(1)
    raise ExecutionError(
        "WAKE-CORE is unavailable in the one-core hosted profile: no "
        f"secondary core exists for core ID 0x{core_id:016x}; execution "
        f"token 0x{xt:016x} and core ID were not consumed"
    )


def _core_status(context: ExecutionContext) -> None:
    core_id = context.data.peek()
    if core_id != 0:
        raise ExecutionError(
            "CORE-STATUS accepts only core ID 0 in the one-core hosted "
            f"profile, got 0x{core_id:016x}; the operand was not consumed"
        )
    # This reports the secondary-worker dispatch slot, not whether the
    # primary CPU is currently executing.  Core zero has no such work queued.
    context.data.replace_top(0)


def _cluster_enable_store(context: ExecutionContext) -> None:
    mask = context.data.peek()
    if mask != 0:
        raise ExecutionError(
            "CLUSTER-EN! cannot enable micro-core clusters in the one-core "
            f"hosted profile, got mask 0x{mask:016x}; the mask was not "
            "consumed"
        )
    context.data.pop()


def _cluster_unavailable(
    context: ExecutionContext,
    word: str,
    *,
    operand: bool = False,
) -> None:
    suffix = ""
    if operand:
        value = context.data.peek()
        suffix = f"; operand 0x{value:016x} was not consumed"
    raise ExecutionError(
        f"{word} is unavailable without a hosted micro-core cluster{suffix}"
    )


def _cluster_spad(context: ExecutionContext) -> None:
    context.data.push(_CLUSTER_SPAD_ADDRESS)


def _net_send_unconfigured(context: ExecutionContext) -> None:
    # No local host-network port is configured for this runtime. Preserve the
    # BIOS stack effect without inspecting memory or fabricating delivery.
    context.data.pop()
    context.data.pop()


def _net_receive_unconfigured(context: ExecutionContext) -> None:
    # An unconfigured transport has no queued frame, but the destination is
    # still consumed exactly as it is by the native BIOS word.
    context.data.pop()
    context.data.push(0)


def _micro_question(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    core_id = context.data.peek()
    full_cores = runtime.memory.read64(MMIO_BASE + SYSINFO_NUM_FULL)
    context.data.replace_top(forth_flag(core_id >= full_cores))


def _disk_transfer_checked(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    *,
    write: bool,
    generation_bound: bool,
) -> None:
    if generation_bound:
        dma = context.data.peek(3)
        lba = context.data.peek(2)
        count = context.data.peek(1)
        generation = context.data.peek()
        consumed = 4
    else:
        dma = context.data.peek(2)
        lba = context.data.peek(1)
        count = context.data.peek()
        generation = None
        consumed = 3

    completed, status = _disk_transfer_value(
        runtime,
        context,
        dma,
        lba,
        count,
        write=write,
        generation=generation,
    )

    for _ in range(consumed):
        context.data.pop()
    context.data.push(completed)
    context.data.push(status)


def _disk_transfer_value(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    dma: int,
    lba: int,
    count: int,
    *,
    write: bool,
    generation: int | None,
) -> tuple[int, int]:
    """Run one value-based checked transfer through native lock two."""

    core_id, _task_id = runtime.guest_identity(context)
    if runtime.spinlocks.acquire(2, core_id) != 0:
        return 0, STORAGE_RESULT_TIMEOUT
    try:
        operation = (
            runtime.storage.write_checked
            if write
            else runtime.storage.read_checked
        )
        return operation(
            runtime.memory,
            dma,
            lba,
            count,
            generation=generation,
        )
    finally:
        runtime.spinlocks.release(2, core_id)


def _mp64fs_valid(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    def read_checked(dma: int, lba: int, count: int) -> tuple[int, int]:
        return _disk_transfer_value(
            runtime,
            context,
            dma,
            lba,
            count,
            write=False,
            generation=None,
        )

    context.data.push(validate_attached_mp64fs(runtime, context, read_checked))


def _disk_flush_checked(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    *,
    generation_bound: bool,
) -> None:
    generation = context.data.peek() if generation_bound else None
    core_id, _task_id = runtime.guest_identity(context)
    if runtime.spinlocks.acquire(2, core_id) != 0:
        status = STORAGE_RESULT_TIMEOUT
    else:
        try:
            status = runtime.storage.flush_checked(generation=generation)
        finally:
            runtime.spinlocks.release(2, core_id)

    if generation_bound:
        context.data.replace_top(status)
    else:
        context.data.push(status)


def _x25519_scalar_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.load_accumulator(core_id, address, runtime.memory)


def _x25519_point_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.set_operand_address(core_id, address)


def _x25519_go(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.x25519(core_id, runtime.memory)


def _x25519_status(context: ExecutionContext) -> None:
    context.data.push(2)


def _x25519_result_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.store_accumulator(core_id, address, runtime.memory)


_FieldMemoryOperation = Callable[[int, SparseAddressSpace], None]
_FieldUnaryOperation = Callable[[int], None]


def _field_accumulator_store(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.load_accumulator(core_id, address, runtime.memory)


def _field_result_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.store_accumulator(core_id, address, runtime.memory)


def _field_prime_select(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    selection = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.select_prime(core_id, selection)


def _field_load_prime(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    inverse_address = context.data.pop()
    prime_address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.load_accumulator(core_id, prime_address, runtime.memory)
    runtime.field.set_operand_address(core_id, inverse_address)
    runtime.field.latch_custom_prime(core_id, runtime.memory)


def _field_binary(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    operation: _FieldMemoryOperation,
) -> None:
    result_address = context.data.pop()
    operand_address = context.data.pop()
    accumulator_address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.load_accumulator(
        core_id,
        accumulator_address,
        runtime.memory,
    )
    runtime.field.set_operand_address(core_id, operand_address)
    operation(core_id, runtime.memory)
    runtime.field.store_accumulator(core_id, result_address, runtime.memory)


def _field_unary(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    operation: _FieldUnaryOperation,
) -> None:
    result_address = context.data.pop()
    accumulator_address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.load_accumulator(
        core_id,
        accumulator_address,
        runtime.memory,
    )
    operation(core_id)
    runtime.field.store_accumulator(core_id, result_address, runtime.memory)


def _field_raw(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    operation: _FieldMemoryOperation,
) -> None:
    high_address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.set_result_address(core_id, high_address)
    low_address = context.data.pop()
    operand_address = context.data.pop()
    accumulator_address = context.data.pop()
    runtime.field.load_accumulator(
        core_id,
        accumulator_address,
        runtime.memory,
    )
    runtime.field.set_operand_address(core_id, operand_address)
    operation(core_id, runtime.memory)
    runtime.field.store_accumulator(core_id, low_address, runtime.memory)


def _field_conditional_move(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    condition_address = context.data.pop()
    operand_address = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    runtime.field.set_operand_address(core_id, operand_address)
    condition = runtime.memory.read8(u64(condition_address)) != 0
    runtime.field.conditional_move(core_id, condition, runtime.memory)


def _ntt_set_modulus(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.ntt.set_modulus(context.data.pop())


def _ntt_set_index(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    runtime.ntt.set_index(context.data.pop())


def _ntt_load(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    selector = context.data.pop()
    address = context.data.pop()
    runtime.ntt.load(address, selector, runtime.memory)


def _ntt_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.ntt.store(context.data.pop(), runtime.memory)


def _ntt_status(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.ntt.status)


def _ntt_wait(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> Invoke | None:
    if runtime.ntt.status & 2:
        return None
    wait_word = runtime.find(b"NTT-WAIT")
    if wait_word is None:
        raise ExecutionError("NTT-WAIT disappeared during semantic dispatch")
    return Invoke(wait_word.xt)


def _kem_select(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.kem.select(context.data.pop())


def _kem_load(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    count = context.data.pop()
    address = context.data.pop()
    runtime.kem.load(address, count, runtime.memory)


def _kem_store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    count = context.data.pop()
    address = context.data.pop()
    runtime.kem.store(address, count, runtime.memory)


def _kem_status(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.kem.status)


def _sha256_init(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(runtime.sha2.sha256_init(core_id))


def _sha256_update(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    length = context.data.pop()
    source = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(
        runtime.sha2.sha256_update(
            core_id,
            source,
            length,
            runtime.memory,
        )
    )


def _sha256_final(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    destination = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(
        runtime.sha2.sha256_final(core_id, destination, runtime.memory)
    )


def _sha256_clear(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(runtime.sha2.sha256_clear(core_id))


def _sha512_init(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(runtime.sha2.sha512_init(core_id))


def _sha512_update(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    length = context.data.pop()
    source = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(
        runtime.sha2.sha512_update(
            core_id,
            source,
            length,
            runtime.memory,
        )
    )


def _sha512_final(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    destination = context.data.pop()
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(
        runtime.sha2.sha512_final(core_id, destination, runtime.memory)
    )


def _sha512_clear(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    core_id, _task_id = runtime.guest_identity(context)
    context.data.push(runtime.sha2.sha512_clear(core_id))


def _sha3_begin(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    status = runtime.sha3.begin(
        runtime.guest_identity(context),
        context.data.pop(),
        runtime.memory,
    )
    context.data.push(status)


def _sha3_update(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    source = context.data.pop()
    status = runtime.sha3.update(
        runtime.guest_identity(context),
        source,
        length,
        memory=runtime.memory,
        span_status=lambda address, count: runtime.caller_span_status(
            context,
            address,
            count,
        ),
    )
    context.data.push(status)


def _sha3_final(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    status = runtime.sha3.final(
        runtime.guest_identity(context),
        context.data.pop(),
        memory=runtime.memory,
        span_status=lambda address, count: runtime.caller_span_status(
            context,
            address,
            count,
        ),
    )
    context.data.push(status)


def _shake_final(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(
        runtime.sha3.shake_final(
            runtime.guest_identity(context),
            runtime.memory,
        )
    )


def _shake_read(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    length = context.data.pop()
    destination = context.data.pop()
    status = runtime.sha3.shake_read(
        runtime.guest_identity(context),
        destination,
        length,
        memory=runtime.memory,
        span_status=lambda address, count: runtime.caller_span_status(
            context,
            address,
            count,
        ),
    )
    context.data.push(status)


def _sha3_clear(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(
        runtime.sha3.clear(runtime.guest_identity(context), runtime.memory)
    )


def _sha3_status_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.memory.read8(MMIO_BASE + SHA3_STATUS))


def _sha3_mode_fetch(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    context.data.push(runtime.memory.read8(MMIO_BASE + SHA3_CONTROL))


def _keccak_f1600(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    status = runtime.sha3.keccak_f1600_checked(
        runtime.guest_identity(context),
        context.data.pop(),
        memory=runtime.memory,
        span_status=lambda address, count: runtime.caller_span_status(
            context,
            address,
            count,
        ),
    )
    context.data.push(status)


def _random(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = 0
    for index in range(8):
        byte = runtime.memory.read8(MMIO_BASE + TRNG_RAND64 + index)
        value |= byte << (index * 8)
    context.data.push(value)


def _random8(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.memory.read8(MMIO_BASE + TRNG_RAND8))


def _entropy_ready(runtime: MegaForthRuntime) -> bool:
    return runtime.entropy.read8(TRNG_STATUS) == 1


def _entropy_fill(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
) -> None:
    length = context.data.pop()
    address = context.data.pop()
    span_status = runtime.caller_span_status(context, address, length)
    if span_status != _ENTROPY_OK:
        context.data.push(span_status)
        return
    if length == 0:
        context.data.push(_ENTROPY_OK)
        return

    published = False
    for offset in range(length):
        if not _entropy_ready(runtime):
            break
        try:
            value = runtime.entropy.read8(TRNG_RAND8)
        except TRNGUnavailableError:
            break
        runtime.memory.write8(address + offset, value)
        published = True
    else:
        # A successful final RAND8 may itself latch the source unusable.  The
        # native contract checks once more before publishing success.
        if _entropy_ready(runtime):
            context.data.push(_ENTROPY_OK)
            return

    if published:
        runtime.memory.fill(address, length, 0)
    context.data.push(_ENTROPY_UNAVAILABLE)


def _seed_rng(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    value = context.data.pop()
    for index in range(8):
        runtime.memory.write8(
            MMIO_BASE + TRNG_SEED + index,
            value & 0xFF,
        )
        value >>= 8


def _push_diagnostic(context: ExecutionContext, value: int) -> None:
    context.data.push(value)


def _task_start_unavailable(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    word_name: str,
) -> None:
    xt = context.data.peek()
    if xt == 0:
        raise ExecutionError(f"{word_name} requires a nonzero execution token")
    try:
        runtime.dictionary.resolve(xt)
    except KeyError:
        raise ExecutionError(
            f"{word_name} requires a live execution token, got 0x{xt:016x}"
        ) from None
    raise ExecutionError(
        f"{word_name} is unavailable until cooperative task scheduling exists; "
        "the execution token was not consumed"
    )


def _task_stop_unavailable(context: ExecutionContext) -> None:
    slot = context.data.peek()
    if slot not in (1, 2, 3):
        raise ExecutionError("TASK-STOP slot must be 1, 2, or 3")
    raise ExecutionError(
        "TASK-STOP is unavailable until cooperative task scheduling exists; "
        "the slot was not consumed"
    )


def install_core(runtime: MegaForthRuntime) -> None:
    """Install only the words required by the first real source slice."""

    directives = (
        (b":", DirectiveKind.COLON),
        (b";", DirectiveKind.SEMICOLON),
        (b"IF", DirectiveKind.IF),
        (b"ELSE", DirectiveKind.ELSE),
        (b"THEN", DirectiveKind.THEN),
        (b"BEGIN", DirectiveKind.BEGIN),
        (b"UNTIL", DirectiveKind.UNTIL),
        (b"AGAIN", DirectiveKind.AGAIN),
        (b"WHILE", DirectiveKind.WHILE),
        (b"REPEAT", DirectiveKind.REPEAT),
        (b"EXIT", DirectiveKind.EXIT),
        (b">R", DirectiveKind.TO_R),
        (b"R>", DirectiveKind.R_FROM),
        (b"R@", DirectiveKind.R_FETCH),
        (b"2>R", DirectiveKind.TWO_TO_R),
        (b"2R>", DirectiveKind.TWO_R_FROM),
        (b"2R@", DirectiveKind.TWO_R_FETCH),
        (b"SP!", DirectiveKind.SP_STORE),
        (b"RP!", DirectiveKind.RP_STORE),
        (b"DO", DirectiveKind.DO),
        (b"?DO", DirectiveKind.QUESTION_DO),
        (b"LOOP", DirectiveKind.LOOP),
        (b"LEAVE", DirectiveKind.LEAVE),
        (b"UNLOOP", DirectiveKind.UNLOOP),
        (b"[']", DirectiveKind.BRACKET_TICK),
        (b"DOES>", DirectiveKind.DOES),
        (b"(", DirectiveKind.PAREN_COMMENT),
        (b"\\", DirectiveKind.BACKSLASH_COMMENT),
        (b"PROVIDED", DirectiveKind.PROVIDED),
        (b'."', DirectiveKind.DOT_QUOTE),
        (b'S"', DirectiveKind.S_QUOTE),
        (b'ABORT"', DirectiveKind.ABORT_QUOTE),
    )
    for name, kind in directives:
        runtime.define_directive(name, kind)
    runtime.define_directive(b"[", DirectiveKind.LEFT_BRACKET)
    runtime.define_directive(
        b"]",
        DirectiveKind.RIGHT_BRACKET,
        immediate=False,
    )

    primitives = (
        (b"DUP", _dup),
        (b"DROP", _drop),
        (b"SWAP", _swap),
        (b"OVER", _over),
        (b"NIP", _nip),
        (b"TUCK", _tuck),
        (b"ROT", _rotate),
        (b"-ROT", _reverse_rotate),
        (b"2DUP", _two_dup),
        (b"2DROP", _two_drop),
        (b"2OVER", _two_over),
        (b"2SWAP", _two_swap),
        (b"?DUP", _question_dup),
        (b"PICK", _pick),
        (b"ROLL", _roll),
        (b"+", _add),
        (b"-", _subtract),
        (b"*", _multiply),
        (b"/", _signed_divide),
        (b"MOD", _signed_modulo),
        (b"NEGATE", _negate),
        (b"ABS", _absolute),
        (b"MIN", _minimum),
        (b"MAX", _maximum),
        (b"1+", _one_plus),
        (b"1-", _one_minus),
        (b"2*", _two_multiply),
        (b"2/", _two_divide),
        (b"AND", _and),
        (b"OR", _or),
        (b"XOR", _xor),
        (b"LSHIFT", _left_shift),
        (b"RSHIFT", _right_shift),
        (b"INVERT", _invert),
        (b"=", _equal),
        (b"<>", _not_equal),
        (b"0=", _zero_equal),
        (b"0<>", _zero_not_equal),
        (b"0<", _zero_less),
        (b"0>", _zero_greater),
        (b"U<", _unsigned_less),
        (b"U>", _unsigned_greater),
        (b"<", _signed_less),
        (b"<=", _signed_less_equal),
        (b">=", _signed_greater_equal),
        (b">", _signed_greater),
        (b"@", lambda context: _fetch(runtime, context)),
        (b"C@", lambda context: _c_fetch(runtime, context)),
        (b"W@", lambda context: _w_fetch(runtime, context)),
        (b"L@", lambda context: _l_fetch(runtime, context)),
        (b"COUNT", lambda context: _count(runtime, context)),
        (b"FIND", lambda context: _find(runtime, context)),
        (b"!", lambda context: _store(runtime, context)),
        (b"OFF", lambda context: _off(runtime, context)),
        (b"C!", lambda context: _c_store(runtime, context)),
        (b"W!", lambda context: _w_store(runtime, context)),
        (b"L!", lambda context: _l_store(runtime, context)),
        (b"+!", lambda context: _plus_store(runtime, context)),
        (b"FILL", lambda context: _fill(runtime, context)),
        (b"CMOVE", lambda context: _cmove(runtime, context)),
        (b"CONSTANT", lambda context: _constant(runtime, context)),
        (b"TYPE", lambda context: _type(runtime, context)),
        (b"SPACE", lambda context: _space(runtime, context)),
        (b"CREATE", lambda context: _create(runtime, context)),
        (b"VARIABLE", lambda context: _variable(runtime, context)),
        (b"HERE", lambda context: _here(runtime, context)),
        (b"LATEST", lambda context: _latest(runtime, context)),
        (
            b"DICT-INDEX!",
            lambda context: _dictionary_index_store(runtime, context),
        ),
        (
            b"DICT-INDEX@",
            lambda context: _dictionary_index_fetch(runtime, context),
        ),
        (
            b"DICT-ROLLBACK",
            lambda context: _dictionary_rollback(runtime, context),
        ),
        (
            b"DICT-FAULT-XT!",
            lambda context: _dictionary_fault_xt_store(runtime, context),
        ),
        (
            b"DICT-BASE@",
            lambda context: _dictionary_base_fetch(runtime, context),
        ),
        (
            b"DICT-LIMIT@",
            lambda context: _dictionary_limit_fetch(runtime, context),
        ),
        (
            b"DICT-BOUNDS!",
            lambda context: _dictionary_bounds_store(runtime, context),
        ),
        (
            b"DICT-BOUNDS-OFF",
            lambda context: _dictionary_bounds_off(runtime, context),
        ),
        (b"TALIGN", lambda context: _tile_align(runtime, context)),
        (b"TMODE!", lambda context: _tile_mode_store(runtime, context)),
        (
            b"TCTRL!",
            lambda context: _tile_control_store(runtime, context),
        ),
        (
            b"TSRC0!",
            lambda context: _tile_source0_store(runtime, context),
        ),
        (
            b"TSRC1!",
            lambda context: _tile_source1_store(runtime, context),
        ),
        (
            b"TDST!",
            lambda context: _tile_destination_store(runtime, context),
        ),
        (b"TADD", lambda _context: runtime.tile.add()),
        (b"TSUB", lambda _context: runtime.tile.subtract()),
        (b"TMUL", lambda _context: runtime.tile.multiply()),
        (b"TDOT", lambda _context: runtime.tile.dot()),
        (b"TSUM", lambda _context: runtime.tile.sum()),
        (b"TMIN", lambda _context: runtime.tile.minimum()),
        (b"TMAX", lambda _context: runtime.tile.maximum()),
        (b"TSUMSQ", lambda _context: runtime.tile.sum_squares()),
        (b"FP16-MODE", lambda _context: runtime.tile.set_mode(4)),
        (b"BF16-MODE", lambda _context: runtime.tile.set_mode(5)),
        (
            b"ACC@",
            lambda context: _tile_accumulator_fetch(runtime, context),
        ),
        (b"ALLOT", lambda context: _allot(runtime, context)),
        (b",", lambda context: _comma(runtime, context)),
        (b"C,", lambda context: _c_comma(runtime, context)),
        (b"CELLS", _cells),
        (b"CELL+", _cell_plus),
        (b"DEPTH", _depth),
        (b"BL", lambda context: context.data.push(32)),
        (b"WORD", lambda context: _word(runtime, context)),
        (b"SP@", _stack_pointer_fetch),
        (b"RP@", _return_stack_pointer_fetch),
        (
            b"NCORES",
            lambda context: _sysinfo_fetch(runtime, context, SYSINFO_NUM_CORES),
        ),
        (
            b"N-FULL",
            lambda context: _sysinfo_fetch(runtime, context, SYSINFO_NUM_FULL),
        ),
        (
            b"MICRO?",
            lambda context: _micro_question(runtime, context),
        ),
        (
            b"HBW-BASE",
            lambda context: _sysinfo_fetch(runtime, context, SYSINFO_HBW_BASE),
        ),
        (
            b"HBW-SIZE",
            lambda context: _sysinfo_fetch(runtime, context, SYSINFO_HBW_SIZE),
        ),
        (
            b"EXT-MEM-BASE",
            lambda context: _sysinfo_fetch(
                runtime,
                context,
                SYSINFO_EXTERNAL_BASE,
            ),
        ),
        (
            b"EXT-MEM-SIZE",
            lambda context: _sysinfo_fetch(
                runtime,
                context,
                SYSINFO_EXTERNAL_SIZE,
            ),
        ),
        (
            b"CRYPTO-CAPS@",
            lambda context: _sysinfo_fetch(
                runtime,
                context,
                SYSINFO_CRYPTO_CAPS,
            ),
        ),
        (
            b"CALLER-SPAN-STATUS",
            lambda context: _caller_span_status(runtime, context),
        ),
        (
            b"SHA2-SPAN-STATUS",
            lambda context: _sha2_span_status(runtime, context),
        ),
        (b"SPIN@", lambda context: _spin_fetch(runtime, context)),
        (b"SPIN!", lambda context: _spin_release(runtime, context)),
        (b"WAKE-CORE", _wake_core_unavailable),
        (b"CORE-STATUS", _core_status),
        (b"CLUSTER-EN!", _cluster_enable_store),
        (b"CLUSTER-EN@", _push_zero),
        (
            b"BARRIER-ARRIVE",
            lambda context: _cluster_unavailable(context, "BARRIER-ARRIVE"),
        ),
        (
            b"BARRIER-STATUS",
            lambda context: _cluster_unavailable(context, "BARRIER-STATUS"),
        ),
        (b"SPAD", _cluster_spad),
        (b"NET-STATUS", _push_zero),
        (b"DISK@", lambda context: context.data.push(runtime.storage.status)),
        (
            b"DISK-SECTORS",
            lambda context: context.data.push(runtime.storage.total_sectors),
        ),
        (
            b"DISK-MEDIA-GEN",
            lambda context: context.data.push(runtime.storage.media_generation),
        ),
        (
            b"DISK-CAPS",
            lambda context: context.data.push(runtime.storage.capabilities),
        ),
        (
            b"DISK-READ-CHECKED",
            lambda context: _disk_transfer_checked(
                runtime,
                context,
                write=False,
                generation_bound=False,
            ),
        ),
        (
            b"DISK-WRITE-CHECKED",
            lambda context: _disk_transfer_checked(
                runtime,
                context,
                write=True,
                generation_bound=False,
            ),
        ),
        (
            b"DISK-FLUSH-CHECKED",
            lambda context: _disk_flush_checked(
                runtime,
                context,
                generation_bound=False,
            ),
        ),
        (
            b"DISK-READ-GEN-CHECKED",
            lambda context: _disk_transfer_checked(
                runtime,
                context,
                write=False,
                generation_bound=True,
            ),
        ),
        (
            b"DISK-WRITE-GEN-CHECKED",
            lambda context: _disk_transfer_checked(
                runtime,
                context,
                write=True,
                generation_bound=True,
            ),
        ),
        (
            b"DISK-FLUSH-GEN-CHECKED",
            lambda context: _disk_flush_checked(
                runtime,
                context,
                generation_bound=True,
            ),
        ),
        (
            b"MP64FS-VALID?",
            lambda context: _mp64fs_valid(runtime, context),
        ),
        (
            b"GF-A!",
            lambda context: _field_accumulator_store(runtime, context),
        ),
        (
            b"GF-R@",
            lambda context: _field_result_fetch(runtime, context),
        ),
        (
            b"GF-PRIME",
            lambda context: _field_prime_select(runtime, context),
        ),
        (
            b"LOAD-PRIME",
            lambda context: _field_load_prime(runtime, context),
        ),
        (
            b"FADD",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.add,
            ),
        ),
        (
            b"FSUB",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.subtract,
            ),
        ),
        (
            b"FMUL",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.multiply,
            ),
        ),
        (
            b"FSQR",
            lambda context: _field_unary(
                runtime,
                context,
                runtime.field.square,
            ),
        ),
        (
            b"FINV",
            lambda context: _field_unary(
                runtime,
                context,
                runtime.field.invert,
            ),
        ),
        (
            b"FPOW",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.power,
            ),
        ),
        (
            b"FMUL-RAW",
            lambda context: _field_raw(
                runtime,
                context,
                runtime.field.multiply_raw,
            ),
        ),
        (
            b"FCMOV",
            lambda context: _field_conditional_move(runtime, context),
        ),
        (
            b"FCEQ",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.equal,
            ),
        ),
        (
            b"FMAC",
            lambda context: _field_binary(
                runtime,
                context,
                runtime.field.multiply_accumulate,
            ),
        ),
        (
            b"FMUL-ADD-RAW",
            lambda context: _field_raw(
                runtime,
                context,
                runtime.field.multiply_add_raw,
            ),
        ),
        (
            b"NTT-SETQ",
            lambda context: _ntt_set_modulus(runtime, context),
        ),
        (
            b"NTT-IDX!",
            lambda context: _ntt_set_index(runtime, context),
        ),
        (b"NTT-LOAD", lambda context: _ntt_load(runtime, context)),
        (b"NTT-STORE", lambda context: _ntt_store(runtime, context)),
        (b"NTT-FWD", lambda _context: runtime.ntt.forward()),
        (b"NTT-INV", lambda _context: runtime.ntt.inverse()),
        (b"NTT-PMUL", lambda _context: runtime.ntt.pointwise_multiply()),
        (b"NTT-PADD", lambda _context: runtime.ntt.pointwise_add()),
        (b"NTT-STATUS@", lambda context: _ntt_status(runtime, context)),
        (b"NTT-WAIT", lambda context: _ntt_wait(runtime, context)),
        (b"KEM-SEL!", lambda context: _kem_select(runtime, context)),
        (b"KEM-LOAD", lambda context: _kem_load(runtime, context)),
        (b"KEM-STORE", lambda context: _kem_store(runtime, context)),
        (b"KEM-KEYGEN", lambda _context: runtime.kem.keygen()),
        (b"KEM-ENCAPS", lambda _context: runtime.kem.encapsulate()),
        (b"KEM-DECAPS", lambda _context: runtime.kem.decapsulate()),
        (b"KEM-STATUS@", lambda context: _kem_status(runtime, context)),
        (
            b"X25519-SCALAR!",
            lambda context: _x25519_scalar_store(runtime, context),
        ),
        (
            b"X25519-POINT!",
            lambda context: _x25519_point_store(runtime, context),
        ),
        (b"X25519-GO", lambda context: _x25519_go(runtime, context)),
        (b"X25519-WAIT", lambda _context: None),
        (b"X25519-STATUS@", _x25519_status),
        (
            b"X25519-RESULT@",
            lambda context: _x25519_result_fetch(runtime, context),
        ),
        (b"CRC-MODE!", lambda context: _crc_mode_store(runtime, context)),
        (b"CRC-RESET", lambda context: _crc_reset(runtime, context)),
        (b"CRC-INIT!", lambda context: _crc_init_store(runtime, context)),
        (b"CRC-FEED", lambda context: _crc_feed(runtime, context)),
        (
            b"CRC-FEED-BYTE",
            lambda context: _crc_feed_byte(runtime, context),
        ),
        (b"CRC@", lambda context: _crc_fetch(runtime, context)),
        (
            b"CRC-RAW-FINAL@",
            lambda context: _crc_raw_final_fetch(runtime, context),
        ),
        (
            b"CRC-FINAL@",
            lambda context: _crc_final_fetch(runtime, context),
        ),
        (
            b"AES-KEY!",
            lambda context: _aes_to_device(
                runtime,
                context,
                mmio_offset=AES_KEY,
                length=32,
            ),
        ),
        (
            b"AES-IV!",
            lambda context: _aes_to_device(
                runtime,
                context,
                mmio_offset=AES_IV,
                length=12,
            ),
        ),
        (
            b"AES-AAD-LEN!",
            lambda context: _aes_scalar_store(
                runtime,
                context,
                mmio_offset=AES_AAD_LENGTH,
                width=4,
            ),
        ),
        (
            b"AES-DATA-LEN!",
            lambda context: _aes_scalar_store(
                runtime,
                context,
                mmio_offset=AES_DATA_LENGTH,
                width=4,
            ),
        ),
        (
            b"AES-CMD!",
            lambda context: _aes_scalar_store(
                runtime,
                context,
                mmio_offset=AES_COMMAND,
                width=1,
            ),
        ),
        (b"AES-STATUS@", lambda context: _aes_status_fetch(runtime, context)),
        (
            b"AES-KEY-MODE!",
            lambda context: _aes_scalar_store(
                runtime,
                context,
                mmio_offset=AES_KEY_MODE,
                width=1,
            ),
        ),
        (
            b"AES-DIN!",
            lambda context: _aes_to_device(
                runtime,
                context,
                mmio_offset=AES_DATA_INPUT,
                length=16,
            ),
        ),
        (
            b"AES-DOUT@",
            lambda context: _aes_from_device(
                runtime,
                context,
                mmio_offset=AES_DATA_OUTPUT,
                length=16,
            ),
        ),
        (
            b"AES-TAG@",
            lambda context: _aes_from_device(
                runtime,
                context,
                mmio_offset=AES_TAG,
                length=16,
            ),
        ),
        (
            b"AES-TAG!",
            lambda context: _aes_to_device(
                runtime,
                context,
                mmio_offset=AES_TAG,
                length=16,
            ),
        ),
        (b"SHA256-INIT", lambda context: _sha256_init(runtime, context)),
        (b"SHA256-UPDATE", lambda context: _sha256_update(runtime, context)),
        (b"SHA256-FINAL", lambda context: _sha256_final(runtime, context)),
        (b"SHA256-CLEAR", lambda context: _sha256_clear(runtime, context)),
        (b"SHA512-INIT", lambda context: _sha512_init(runtime, context)),
        (b"SHA512-UPDATE", lambda context: _sha512_update(runtime, context)),
        (b"SHA512-FINAL", lambda context: _sha512_final(runtime, context)),
        (b"SHA512-CLEAR", lambda context: _sha512_clear(runtime, context)),
        (b"SHA3-BEGIN", lambda context: _sha3_begin(runtime, context)),
        (b"SHA3-UPDATE", lambda context: _sha3_update(runtime, context)),
        (b"SHA3-FINAL", lambda context: _sha3_final(runtime, context)),
        (b"SHAKE-FINAL", lambda context: _shake_final(runtime, context)),
        (b"SHAKE-READ", lambda context: _shake_read(runtime, context)),
        (b"SHA3-CLEAR", lambda context: _sha3_clear(runtime, context)),
        (
            b"SHA3-STATUS@",
            lambda context: _sha3_status_fetch(runtime, context),
        ),
        (
            b"SHA3-MODE@",
            lambda context: _sha3_mode_fetch(runtime, context),
        ),
        (
            b"KECCAK-F1600",
            lambda context: _keccak_f1600(runtime, context),
        ),
        (b"RANDOM", lambda context: _random(runtime, context)),
        (b"RANDOM8", lambda context: _random8(runtime, context)),
        (b"SEED-RNG", lambda context: _seed_rng(runtime, context)),
        (
            b"PERF-CYCLES",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.perf_cycles,
            ),
        ),
        (
            b"CYCLES",
            lambda context: _push_diagnostic(
                context,
                runtime.timer.counter,
            ),
        ),
        (
            b"TIMER!",
            lambda context: _timer_compare_store(runtime, context),
        ),
        (
            b"TIMER-CTRL!",
            lambda context: _timer_control_store(runtime, context),
        ),
        (b"TIMER-ACK", lambda _context: runtime.timer.acknowledge()),
        (
            b"PERF-STALLS",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.perf_stalls,
            ),
        ),
        (
            b"PERF-TILEOPS",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.perf_tileops,
            ),
        ),
        (
            b"PERF-EXTMEM",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.perf_extmem,
            ),
        ),
        (
            b"PERF-RESET",
            lambda _context: runtime.diagnostics.reset_performance(),
        ),
        (
            b"BIST-FULL",
            lambda _context: runtime.diagnostics.run_full_bist(),
        ),
        (
            b"BIST-QUICK",
            lambda _context: runtime.diagnostics.run_quick_bist(),
        ),
        (
            b"BIST-STATUS",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.bist_status,
            ),
        ),
        (
            b"BIST-FAIL-ADDR",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.bist_fail_address,
            ),
        ),
        (
            b"BIST-FAIL-DATA",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.bist_fail_data,
            ),
        ),
        (
            b"TILE-TEST",
            lambda _context: runtime.diagnostics.run_tile_test(),
        ),
        (
            b"TILE-TEST@",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.tile_status,
            ),
        ),
        (
            b"TILE-DETAIL@",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.tile_detail,
            ),
        ),
        (
            b"ICACHE-ON",
            lambda _context: runtime.diagnostics.enable_icache(),
        ),
        (
            b"ICACHE-OFF",
            lambda _context: runtime.diagnostics.disable_icache(),
        ),
        (
            b"ICACHE-INV",
            lambda _context: runtime.diagnostics.invalidate_icache(),
        ),
        (
            b"ICACHE-HITS",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.icache_hits,
            ),
        ),
        (
            b"ICACHE-MISSES",
            lambda context: _push_diagnostic(
                context,
                runtime.diagnostics.icache_misses,
            ),
        ),
        (b"COREID", _push_zero),
        (b"TASK-ID", _push_zero),
        (b"ENTER-USER", lambda _context: None),
        (b"SYS-EXIT", lambda _context: None),
        (b"PRIV@", _push_zero),
        (
            b"MPU-BASE!",
            lambda context: _mpu_base_store(runtime, context),
        ),
        (
            b"MPU-LIMIT!",
            lambda context: _mpu_limit_store(runtime, context),
        ),
        (
            b"MPU-BASE@",
            lambda context: _mpu_base_fetch(runtime, context),
        ),
        (
            b"MPU-LIMIT@",
            lambda context: _mpu_limit_fetch(runtime, context),
        ),
        (
            b"CL-PRIV!",
            lambda context: _cluster_unavailable(
                context,
                "CL-PRIV!",
                operand=True,
            ),
        ),
        (
            b"CL-PRIV@",
            lambda context: _cluster_unavailable(context, "CL-PRIV@"),
        ),
        (
            b"CL-MPU-BASE!",
            lambda context: _cluster_unavailable(
                context,
                "CL-MPU-BASE!",
                operand=True,
            ),
        ),
        (
            b"CL-MPU-LIMIT!",
            lambda context: _cluster_unavailable(
                context,
                "CL-MPU-LIMIT!",
                operand=True,
            ),
        ),
        (
            b"CL-MPU-BASE@",
            lambda context: _cluster_unavailable(context, "CL-MPU-BASE@"),
        ),
        (
            b"CL-MPU-LIMIT@",
            lambda context: _cluster_unavailable(context, "CL-MPU-LIMIT@"),
        ),
        (
            b"BACKGROUND",
            lambda context: _task_start_unavailable(
                runtime,
                context,
                "BACKGROUND",
            ),
        ),
        (
            b"BACKGROUND2",
            lambda context: _task_start_unavailable(
                runtime,
                context,
                "BACKGROUND2",
            ),
        ),
        (
            b"BACKGROUND3",
            lambda context: _task_start_unavailable(
                runtime,
                context,
                "BACKGROUND3",
            ),
        ),
        (b"TASK-STOP", _task_stop_unavailable),
        (b"'", lambda context: _tick(runtime, context)),
        (b"EVALUATE", lambda context: runtime.bios_evaluate(context)),
        (
            b"EVALUATE-CHECKED",
            lambda context: runtime.bios_evaluate_checked(context),
        ),
        (
            b"EVALUATE-FINISH",
            lambda context: runtime.bios_evaluate_finish(context),
        ),
        (
            b"EVALUATOR-RESET",
            lambda _context: runtime.bios_evaluator_reset(),
        ),
        (
            b"EVALUATOR-UNWIND",
            lambda context: runtime.bios_evaluator_unwind(context),
        ),
        (b">BODY", lambda context: _to_body(runtime, context)),
        (b"COMPARE", lambda context: _compare(runtime, context)),
        (b"JIT-ON", _semantic_noop),
        (b"JIT-OFF", _semantic_noop),
        (b"ABORT", _abort),
        (b"EMIT", lambda context: _emit(runtime, context)),
        (b"CR", lambda context: _carriage_return(runtime, context)),
        (b".", lambda context: _dot(runtime, context)),
        (b"U.", lambda context: _unsigned_dot(runtime, context)),
        (b"EPOCH@", lambda context: _epoch_fetch(runtime, context)),
        (b".ZSTR", lambda context: _dot_zstr(runtime, context)),
        (b"UCHAR", _uppercase_character),
        (b"TRUE", lambda context: context.data.push(-1)),
        (b"FALSE", _push_zero),
        (b"I", _i),
        (b"J", _j),
        (b"EXECUTE", _execute),
        (b"HEX", lambda context: runtime.set_numeric_base(16)),
        (b"DECIMAL", lambda context: runtime.set_numeric_base(10)),
    )
    for name, callback in primitives:
        runtime.define_primitive(name, callback)
        if name == b"EMIT":
            runtime.define_colon(
                b"KEY",
                (
                    UartReadAttempt(),
                    BranchZero(3),
                    Branch(5),
                    Idle(),
                    Branch(0),
                    Return(),
                ),
            )
            runtime.define_primitive(
                b"KEY?",
                lambda context: _key_query(runtime, context),
            )

    zero_cell = bytes(CELL_BYTES)
    eval_status = runtime.define_primitive(
        b"EVAL-STATUS",
        lambda context: runtime.bios_eval_status(context),
        initial_body=zero_cell,
    )
    eval_line = runtime.define_primitive(
        b"EVAL-LINE",
        lambda context: runtime.bios_eval_line(context),
        initial_body=zero_cell,
    )
    eval_column = runtime.define_primitive(
        b"EVAL-COLUMN",
        lambda context: runtime.bios_eval_column(context),
        initial_body=zero_cell,
    )
    eval_depth = runtime.define_primitive(
        b"EVAL-DEPTH",
        lambda context: runtime.bios_eval_depth(context),
        initial_body=zero_cell,
    )
    eval_throw = runtime.define_primitive(
        b"EVAL-THROW",
        lambda context: runtime.bios_eval_throw(context),
        initial_body=zero_cell,
    )
    eval_token = runtime.define_primitive(
        b"EVAL-TOKEN",
        lambda context: runtime.bios_eval_token(context),
        initial_body=bytes(_EVAL_TOKEN_BYTES),
    )
    runtime.bind_bios_evaluator(
        status_address=eval_status.body_address,
        line_address=eval_line.body_address,
        column_address=eval_column.body_address,
        depth_address=eval_depth.body_address,
        throw_address=eval_throw.body_address,
        token_address=eval_token.body_address,
    )

    base_word = runtime.define_created(
        b"BASE",
        initial_body=(10).to_bytes(CELL_BYTES, "little"),
    )
    runtime.bind_numeric_base_address(base_word.body_address)

    # Preserve every pre-integration hosted XT by appending the first
    # rich-terminal and geometry primitives after the complete older pseudo-BIOS.
    # Cross-backend absolute XTs are not portable.
    rich_terminal_primitives = (
        (b"UM*", _unsigned_multiply),
        (b"WITHIN", _within),
        (b"MOVE", lambda context: _move(runtime, context)),
        (b"MS@", lambda context: _ms_fetch(runtime, context)),
        (b"TX-FLUSH", lambda context: _tx_flush(runtime, context)),
        (b"COLS", lambda context: context.data.push(runtime.terminal_columns())),
        (b"ROWS", lambda context: context.data.push(runtime.terminal_rows())),
        (
            b"RESIZED?",
            lambda context: context.data.push(
                forth_flag(runtime.consume_terminal_resized())
            ),
        ),
        (b"TERMSIZE", lambda context: _terminal_size(runtime, context)),
        (
            b"RESIZE-DENIED?",
            lambda context: context.data.push(
                forth_flag(runtime.consume_terminal_resize_denied())
            ),
        ),
        (
            b"RESIZE-REQUEST",
            lambda context: _terminal_resize_request(runtime, context),
        ),
    )
    for name, callback in rich_terminal_primitives:
        runtime.define_primitive(name, callback)

    # Continue the same append-only integration frontier. Until a local
    # host-network port is configured, do not fabricate a queue or status.
    source_closure_primitives = (
        (b"BSWAP", _byte_swap),
        (b"NET-SEND", _net_send_unconfigured),
        (b"NET-RECV", _net_receive_unconfigured),
    )
    for name, callback in source_closure_primitives:
        runtime.define_primitive(name, callback)

    unconfigured_mac_address: int | None = None

    def net_mac_fetch(context: ExecutionContext) -> None:
        assert unconfigured_mac_address is not None
        context.data.push(unconfigured_mac_address)

    unconfigured_mac = runtime.define_primitive(
        b"NET-MAC@",
        net_mac_fetch,
        # All-zero bytes are stable ordinary memory for MAC-INIT's CMOVE, but
        # do not invent an interface identity before a port is configured.
        initial_body=bytes(_UNCONFIGURED_NETWORK_MAC_BYTES),
    )
    unconfigured_mac_address = unconfigured_mac.body_address

    runtime.define_primitive(
        b"ENTROPY-FILL",
        lambda context: _entropy_fill(runtime, context),
    )
    runtime.define_primitive(
        b"ENTROPY-READY?",
        lambda context: context.data.push(forth_flag(_entropy_ready(runtime))),
    )

    runtime.define_directive(b"[CHAR]", DirectiveKind.BRACKET_CHAR)
    runtime.define_primitive(b"CHAR", lambda context: _char(runtime, context))
    runtime.define_primitive(b"/MOD", _signed_divmod)
    for name, kind in (
        (b"[DEFINED]", DirectiveKind.BRACKET_DEFINED),
        (b"[UNDEFINED]", DirectiveKind.BRACKET_UNDEFINED),
        (b"[IF]", DirectiveKind.BRACKET_IF),
        (b"[ELSE]", DirectiveKind.BRACKET_ELSE),
        (b"[THEN]", DirectiveKind.BRACKET_THEN),
    ):
        runtime.define_directive(name, kind)

    for name, kind in (
        (b"CASE", DirectiveKind.CASE),
        (b"OF", DirectiveKind.OF),
        (b"ENDOF", DirectiveKind.ENDOF),
        (b"ENDCASE", DirectiveKind.ENDCASE),
    ):
        runtime.define_directive(name, kind)

    runtime.define_directive(b"RECURSE", DirectiveKind.RECURSE)
    runtime.define_directive(b"+LOOP", DirectiveKind.PLUS_LOOP)
    runtime.define_primitive(
        b"CMOVE>",
        lambda context: _cmove_up(runtime, context),
    )
    runtime.define_primitive(b"ON", lambda context: _on(runtime, context))
    runtime.define_primitive(b"2!", lambda context: _two_store(runtime, context))
    runtime.define_primitive(b"2@", lambda context: _two_fetch(runtime, context))

__all__ = ["install_core"]
