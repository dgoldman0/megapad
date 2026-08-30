"""Initial hosted implementations of the public MegaForth core vocabulary."""

from __future__ import annotations

from shared.cells import CELL_BYTES, forth_flag, s64, u64
from simulator.errors import ExecutionError, ForthAbort
from simulator.memory import MMIO_BASE
from simulator.platform import SYSINFO_NUM_CORES, SYSINFO_NUM_FULL
from simulator.runtime import (
    CreatedDefinition,
    DirectiveKind,
    ExecutionContext,
    Invoke,
    MegaForthRuntime,
)


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


def _question_dup(context: ExecutionContext) -> None:
    value = context.data.peek()
    if value != 0:
        context.data.push(value)


def _pick(context: ExecutionContext) -> None:
    offset = context.data.pop()
    context.data.push(context.data.peek(offset))


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


def _signed_divide(context: ExecutionContext) -> None:
    divisor = s64(context.data.pop())
    dividend = s64(context.data.pop())
    if divisor == 0 or (dividend == -(1 << 63) and divisor == -1):
        raise ExecutionError("signed division trapped on zero or overflow")
    quotient = abs(dividend) // abs(divisor)
    if (dividend < 0) != (divisor < 0):
        quotient = -quotient
    context.data.push(quotient)


def _minimum(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    # The BIOS documentation calls this signed, but the current MP64 `cmp`
    # branch uses the architectural unsigned G/LE conditions.  Preserve the
    # executable BIOS behavior used by unchanged source.
    context.data.push(left if left <= right else right)


def _maximum(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    # See _minimum: the checked-in BIOS currently compares these cells as
    # unsigned even though the reference describes signed MIN/MAX.
    context.data.push(left if left > right else right)


def _one_minus(context: ExecutionContext) -> None:
    context.data.push(u64(context.data.pop() - 1))


def _one_plus(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() + 1)


def _and(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left & right)


def _or(context: ExecutionContext) -> None:
    right = context.data.pop()
    left = context.data.pop()
    context.data.push(left | right)


def _right_shift(context: ExecutionContext) -> None:
    count = context.data.pop()
    value = context.data.pop()
    context.data.push(value >> (count & 0x3F))


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


def _fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    context.data.push(runtime.memory.read64(address))


def _c_fetch(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    context.data.push(runtime.memory.read8(context.data.pop()))


def _count(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    length = runtime.memory.read8(address)
    context.data.push(address + 1)
    context.data.push(length)


def _store(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    address = context.data.pop()
    value = context.data.pop()
    runtime.memory.write64(address, value)


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


def _tile_align(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.tile_align_dictionary(context)


def _allot(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.allot_dictionary(context.data.pop(), context)


def _comma(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.comma_dictionary(context.data.pop(), context)


def _c_comma(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    runtime.c_comma_dictionary(context.data.pop(), context)


def _cells(context: ExecutionContext) -> None:
    context.data.push(context.data.pop() << 3)


def _depth(context: ExecutionContext) -> None:
    context.data.push(context.data.depth())


def _word(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    delimiter = context.data.pop()
    context.data.push(runtime.parse_word_to_dictionary_tail(delimiter))


def _stack_pointer_fetch(context: ExecutionContext) -> None:
    pointer = context.data.pointer
    context.data.push(pointer)


def _return_stack_pointer_fetch(context: ExecutionContext) -> None:
    context.data.push(context.returns.capture_pointer())


def _push_zero(context: ExecutionContext) -> None:
    context.data.push(0)


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
    if not 2 <= base <= 36:
        raise ExecutionError(f". cannot render with numeric base {base}")

    signed = s64(value)
    magnitude = -signed if signed < 0 else signed
    digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    rendered = bytearray()
    while True:
        magnitude, digit = divmod(magnitude, base)
        rendered.append(digits[digit])
        if magnitude == 0:
            break
    if signed < 0:
        rendered.append(ord("-"))
    rendered.reverse()
    return bytes(rendered)


def _dot(runtime: MegaForthRuntime, context: ExecutionContext) -> None:
    rendered = _format_signed_cell(context.data.pop(), runtime.numeric_base)
    runtime.write_uart_bytes(rendered + b" ")


def _carriage_return(runtime: MegaForthRuntime, _context: ExecutionContext) -> None:
    runtime.write_uart_bytes(b"\r\n")


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
        (b'ABORT"', DirectiveKind.ABORT_QUOTE),
    )
    for name, kind in directives:
        runtime.define_directive(name, kind)

    primitives = (
        (b"DUP", _dup),
        (b"DROP", _drop),
        (b"SWAP", _swap),
        (b"OVER", _over),
        (b"ROT", _rotate),
        (b"-ROT", _reverse_rotate),
        (b"2DUP", _two_dup),
        (b"2DROP", _two_drop),
        (b"2OVER", _two_over),
        (b"?DUP", _question_dup),
        (b"PICK", _pick),
        (b"+", _add),
        (b"-", _subtract),
        (b"*", _multiply),
        (b"/", _signed_divide),
        (b"MIN", _minimum),
        (b"MAX", _maximum),
        (b"1+", _one_plus),
        (b"1-", _one_minus),
        (b"AND", _and),
        (b"OR", _or),
        (b"RSHIFT", _right_shift),
        (b"INVERT", _invert),
        (b"=", _equal),
        (b"<>", _not_equal),
        (b"0=", _zero_equal),
        (b"0<>", _zero_not_equal),
        (b"0<", _zero_less),
        (b"U<", _unsigned_less),
        (b"U>", _unsigned_greater),
        (b"<", _signed_less),
        (b"<=", _signed_less_equal),
        (b">=", _signed_greater_equal),
        (b">", _signed_greater),
        (b"@", lambda context: _fetch(runtime, context)),
        (b"C@", lambda context: _c_fetch(runtime, context)),
        (b"COUNT", lambda context: _count(runtime, context)),
        (b"!", lambda context: _store(runtime, context)),
        (b"+!", lambda context: _plus_store(runtime, context)),
        (b"FILL", lambda context: _fill(runtime, context)),
        (b"CMOVE", lambda context: _cmove(runtime, context)),
        (b"CONSTANT", lambda context: _constant(runtime, context)),
        (b"CREATE", lambda context: _create(runtime, context)),
        (b"VARIABLE", lambda context: _variable(runtime, context)),
        (b"HERE", lambda context: _here(runtime, context)),
        (b"LATEST", lambda context: _latest(runtime, context)),
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
        (b"TALIGN", lambda context: _tile_align(runtime, context)),
        (b"ALLOT", lambda context: _allot(runtime, context)),
        (b",", lambda context: _comma(runtime, context)),
        (b"C,", lambda context: _c_comma(runtime, context)),
        (b"CELLS", _cells),
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
        (b"COREID", _push_zero),
        (b"TASK-ID", _push_zero),
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
        (b">BODY", lambda context: _to_body(runtime, context)),
        (b"COMPARE", lambda context: _compare(runtime, context)),
        (b"JIT-ON", _semantic_noop),
        (b"JIT-OFF", _semantic_noop),
        (b"ABORT", _abort),
        (b".", lambda context: _dot(runtime, context)),
        (b"CR", lambda context: _carriage_return(runtime, context)),
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

__all__ = ["install_core"]
