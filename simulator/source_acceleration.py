"""Fail-closed semantic acceleration for the ordinary KDOS source path.

The accelerated words remain their exact source-compiled colon definitions in
the guest dictionary.  This module recognizes only pinned IR shapes and asks
the runtime to overlay their execution.  Filesystem lookup, extent reads,
``REQUIRE``/``PROVIDED`` registration, loader transactions, dictionary
rollback, evaluator calls, and allocation release all remain ordinary KDOS.

Only proven orchestration around the existing hosted evaluator and its loader
checksum is collapsed.  A mismatched or absent definition is left completely
untouched and continues through the normal semantic dispatcher.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass, fields

from shared.cells import MASK64, s64, u64
from simulator.dictionary import Word
from simulator.ir import Call, Literal, Operation
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    DirectiveDefinition,
    DirectiveKind,
    ExecutionContext,
    MegaForthRuntime,
    PrimitiveDefinition,
    ValueDefinition,
)
from simulator.source import SourceCursor


_SCAN_CHUNK_BYTES = 4096
_SEC_MEASURE_DIGEST = (
    "8baea05066c381040309e896a87a46325be8435e62345b982cafe782c6b9375f"
)
_SEC_ADVANCE_DIGEST = (
    "ce1b43e6121fb2ff09f93dd4d4236339ce0e6f5e6a8657d71f97ea679d804594"
)


# These directives only append or repair IR in an already-open named colon
# definition. They neither execute a guest word nor end the definition.
# Definition boundaries and the few admitted token/string directives are
# handled separately; interpretation-changing and conditional-compilation
# directives deliberately retain the KDOS CATCH path.
_COMPILE_ONLY_DIRECTIVES = frozenset(
    {
        DirectiveKind.IF,
        DirectiveKind.ELSE,
        DirectiveKind.THEN,
        DirectiveKind.BEGIN,
        DirectiveKind.UNTIL,
        DirectiveKind.AGAIN,
        DirectiveKind.WHILE,
        DirectiveKind.REPEAT,
        DirectiveKind.EXIT,
        DirectiveKind.TO_R,
        DirectiveKind.R_FROM,
        DirectiveKind.R_FETCH,
        DirectiveKind.TWO_TO_R,
        DirectiveKind.TWO_R_FROM,
        DirectiveKind.TWO_R_FETCH,
        DirectiveKind.DO,
        DirectiveKind.QUESTION_DO,
        DirectiveKind.LOOP,
        DirectiveKind.PLUS_LOOP,
        DirectiveKind.LEAVE,
        DirectiveKind.UNLOOP,
        DirectiveKind.DOES,
        DirectiveKind.SP_STORE,
        DirectiveKind.RP_STORE,
        DirectiveKind.CASE,
        DirectiveKind.OF,
        DirectiveKind.ENDOF,
        DirectiveKind.ENDCASE,
        DirectiveKind.RECURSE,
    }
)


_INTERPRET_SAFE_PRIMITIVE_NAMES = (
    b"DUP",
    b"DROP",
    b"SWAP",
    b"OVER",
    b"NIP",
    b"TUCK",
    b"ROT",
    b"-ROT",
    b"2DUP",
    b"2DROP",
    b"2OVER",
    b"2SWAP",
    b"?DUP",
    b"PICK",
    b"ROLL",
    b"+",
    b"-",
    b"*",
    b"/",
    b"MOD",
    b"NEGATE",
    b"ABS",
    b"MIN",
    b"MAX",
    b"1+",
    b"1-",
    b"2*",
    b"2/",
    b"AND",
    b"OR",
    b"XOR",
    b"LSHIFT",
    b"RSHIFT",
    b"INVERT",
    b"=",
    b"<>",
    b"0=",
    b"0<>",
    b"0<",
    b"0>",
    b"U<",
    b"U>",
    b"<",
    b"<=",
    b">=",
    b">",
    b"CELLS",
    b"CELL+",
)

_DEFINING_PRIMITIVE_BODY_BYTES = {
    b"CONSTANT": 0,
    b"VARIABLE": 8,
    b"VALUE": 8,
}

# A 255-byte checked line can contain at most 128 one-byte, space-separated
# tokens.  Reserve that complete possible data-stack growth when proving that
# a defining primitive cannot reach the stack-owned dictionary ceiling.
_CHECKED_LINE_STACK_RESERVE = 128 * 8


@dataclass(frozen=True, slots=True)
class SourceAccelerationReport:
    """Definitions accelerated and definitions unavailable to this build."""

    installed: tuple[bytes, ...]
    skipped: tuple[bytes, ...]


@dataclass(frozen=True, slots=True)
class _AccelerationSpec:
    name: bytes
    digest: str


# These digests cover the complete normalized IR for the exact KDOS words.
# Call operands include their bound target name, implementation kind, and
# immediacy, while address-specific Call XTs are deliberately excluded.
_SPECS = (
    _AccelerationSpec(
        b"EVALUATE-CHECKED",
        "8d4ec79f585cda10b03ef6e922f550b88e814115e487b484d0716b2fbe510e96",
    ),
    _AccelerationSpec(
        b"SOURCE-EVALUATE-CHECKED",
        "bdf5ce4dc6e01bbbb89fbdbae60793030c866576757f90178dcb5e79018ef935",
    ),
    _AccelerationSpec(
        b"_LD-STATUS-THROW",
        "fbd8aab9c2469e7979d2bd17f14e2fb047aa462bfd6d6364417d4d626c1f9669",
    ),
    _AccelerationSpec(
        b"_CRC-BUF-CHECKED",
        "fa1c9da01b4b0e1d3aca19390b9be2981e7410fad21c99eec791fef285814aaf",
    ),
    _AccelerationSpec(
        b"_LD-WALK",
        "49b1216e6664746d888e5ce8f77b1a3a2e8ef3ecd6ed779cd02290ea0b7ce7e6",
    ),
    _AccelerationSpec(
        b"_PS-LINE-LEN",
        "9ef000822d309fbb556936eb8176f7de70627f92dcdd2fe27baf1289790b01d9",
    ),
)


def normalized_colon_digest(runtime: MegaForthRuntime, word: Word) -> str:
    """Return an address-independent digest of one compiled colon body."""

    implementation = word.implementation
    if not isinstance(implementation, ColonDefinition):
        raise TypeError("normalized colon digest requires a colon definition")

    digest = hashlib.sha256()

    def add(value: bytes) -> None:
        digest.update(len(value).to_bytes(4, "little"))
        digest.update(value)

    add(word.name)
    add(str(len(implementation.operations)).encode("ascii"))
    for operation in implementation.operations:
        add(type(operation).__name__.encode("ascii"))
        if isinstance(operation, Call):
            target = runtime.dictionary.resolve(operation.xt)
            add(target.name)
            target_implementation = target.implementation
            add(type(target_implementation).__name__.encode("ascii"))
            if isinstance(target_implementation, ConstantDefinition):
                add(str(target_implementation.value).encode("ascii"))
            elif isinstance(target_implementation, DirectiveDefinition):
                add(target_implementation.kind.name.encode("ascii"))
            add(b"1" if target.immediate else b"0")
            continue
        for descriptor in fields(operation):
            add(descriptor.name.encode("ascii"))
            add(_field_bytes(getattr(operation, descriptor.name)))
    return digest.hexdigest()


def install_kdos_source_accelerators(
    runtime: MegaForthRuntime,
) -> SourceAccelerationReport:
    """Install every exact source-walking overlay available in ``runtime``."""

    installed: list[bytes] = []
    skipped: list[bytes] = []
    for spec in _SPECS:
        word = runtime.find(spec.name)
        if (
            word is None
            or not isinstance(word.implementation, ColonDefinition)
            or normalized_colon_digest(runtime, word) != spec.digest
        ):
            skipped.append(spec.name)
            continue

        try:
            if spec.name == b"EVALUATE-CHECKED":
                applicable, callback = _build_checked_line_accelerator(
                    runtime,
                    word,
                )
            elif spec.name == b"SOURCE-EVALUATE-CHECKED":
                applicable, callback = _build_checked_source_accelerator(
                    runtime,
                    word,
                )
            elif spec.name == b"_LD-STATUS-THROW":
                applicable, callback = _build_zero_status_translator()
            elif spec.name == b"_CRC-BUF-CHECKED":
                applicable, callback = _build_crc_buffer_accelerator(
                    runtime,
                    word,
                )
            elif spec.name == b"_LD-WALK":
                applicable, callback = _build_loader_walk_accelerator(runtime, word)
            elif spec.name == b"_PS-LINE-LEN":
                applicable, callback = _build_line_length_accelerator(runtime)
            else:  # pragma: no cover - the closed spec table makes this impossible.
                raise AssertionError(f"unknown source accelerator {spec.name!r}")
        except (KeyError, ValueError):
            skipped.append(spec.name)
            continue

        runtime.install_colon_accelerator(
            word,
            applicable=applicable,
            callback=callback,
        )
        installed.append(spec.name)

    return SourceAccelerationReport(tuple(installed), tuple(skipped))


def _build_checked_line_accelerator(
    runtime: MegaForthRuntime,
    word: Word,
):
    evaluate = _require_literal_word(runtime, word, 3, b"EVALUATE")
    if not isinstance(evaluate.implementation, PrimitiveDefinition):
        raise ValueError("KDOS EVALUATE-CHECKED does not bind primitive EVALUATE")
    eval_status = _require_call(runtime, word, 22, b"EVAL-STATUS").body_address
    compiler_state = runtime._require_bios_evaluator().compiler_state
    interpret_primitives = frozenset(
        primitive
        for name in _INTERPRET_SAFE_PRIMITIVE_NAMES
        if (primitive := _current_initial_primitive(runtime, name)) is not None
    )
    defining_primitives = {
        primitive: _DEFINING_PRIMITIVE_BODY_BYTES[name]
        for name in _DEFINING_PRIMITIVE_BODY_BYTES
        if (primitive := _current_initial_primitive(runtime, name)) is not None
    }

    def applicable(context: ExecutionContext) -> bool:
        if context.data.depth() < 2:
            return False
        if not _stack_has_room(context.data, 3) or not _stack_has_room(
            context.returns,
            4,
        ):
            return False
        length = context.data.peek()
        address = context.data.peek(1)
        if length > 255 or not _ordinary_span(runtime, address, length):
            return False

        if compiler_state.conditional_skip is not None:
            return False
        return _line_cannot_execute_guest(
            runtime,
            address,
            length,
            compiler_state.compiler,
            context,
            interpret_primitives,
            defining_primitives,
        )

    def callback(context: ExecutionContext) -> None:
        # The outer overlay has already charged the KDOS wrapper operation.
        # Dispatch the exact primitive XT captured by that wrapper so the
        # nested evaluator contributes its ordinary semantic step as well.
        runtime.execute(evaluate.xt, context=context)
        context.data.push(runtime.memory.read64(eval_status))

    return applicable, callback


def _build_zero_status_translator():
    def applicable(context: ExecutionContext) -> bool:
        return context.data.depth() >= 1 and context.data.peek() == 0

    def callback(context: ExecutionContext) -> None:
        context.data.pop()

    return applicable, callback


def _build_crc_buffer_accelerator(
    runtime: MegaForthRuntime,
    word: Word,
):
    feed_cell = _require_call(runtime, word, 6, b"CRC-FEED")
    feed_byte = _require_call(runtime, word, 26, b"CRC-FEED-BYTE")
    _require_initial_primitive(runtime, feed_cell)
    _require_initial_primitive(runtime, feed_byte)

    def applicable(context: ExecutionContext) -> bool:
        if context.data.depth() < 2:
            return False
        if context.data.backed and context.data.depth() >= context.data.capacity:
            return False
        length = context.data.peek()
        address = context.data.peek(1)
        return _ordinary_span(runtime, address, length)

    def callback(context: ExecutionContext) -> None:
        remaining = context.data.pop()
        address = context.data.pop()
        identity = runtime.guest_identity(context)
        while remaining:
            count = min(_SCAN_CHUNK_BYTES, remaining)
            status = runtime.crc.feed_bytes(
                identity,
                runtime.memory.read_bytes(address, count),
            )
            if status != 0:
                context.data.push(status)
                return
            address = u64(address + count)
            remaining -= count
        context.data.push(0)

    return applicable, callback


def _build_checked_source_accelerator(
    runtime: MegaForthRuntime,
    word: Word,
):
    measure = _require_call(runtime, word, 18, b"_SEC-MEASURE")
    if normalized_colon_digest(runtime, measure) != _SEC_MEASURE_DIGEST:
        raise ValueError("SOURCE-EVALUATE-CHECKED has a changed line measurer")
    evaluator = _require_call(runtime, word, 27, b"EVALUATE-CHECKED")
    advance = _require_call(runtime, word, 34, b"_SEC-ADVANCE")
    if normalized_colon_digest(runtime, advance) != _SEC_ADVANCE_DIGEST:
        raise ValueError("SOURCE-EVALUATE-CHECKED has a changed line advance")
    finish = _require_call(runtime, word, 40, b"EVALUATE-FINISH")

    cur = _require_call(runtime, word, 2, b"_SEC-CUR").body_address
    rem = _require_call(runtime, word, 0, b"_SEC-REM").body_address
    raw_len = _require_call(runtime, measure, 25, b"_SEC-RAW-LEN").body_address
    eval_len = _require_call(runtime, measure, 27, b"_SEC-EVAL-LEN").body_address
    line = _require_call(runtime, word, 5, b"_SEC-LINE").body_address
    eval_line = _require_call(runtime, word, 16, b"EVAL-LINE").body_address

    def applicable(context: ExecutionContext) -> bool:
        if context.data.depth() < 2:
            return False
        length = context.data.peek()
        address = context.data.peek(1)
        return s64(length) <= 0 or _ordinary_span(runtime, address, length)

    def callback(context: ExecutionContext) -> None:
        length = context.data.pop()
        runtime.memory.write64(rem, length)
        address = context.data.pop()
        runtime.memory.write64(cur, address)
        runtime.memory.write64(line, 0)

        while s64(runtime.memory.read64(rem)) > 0:
            line_number = u64(runtime.memory.read64(line) + 1)
            runtime.memory.write64(line, line_number)
            runtime.memory.write64(eval_line, line_number)

            current = runtime.memory.read64(cur)
            remaining = runtime.memory.read64(rem)
            measured = _line_length(runtime, current, remaining)
            runtime.memory.write64(raw_len, measured)
            runtime.memory.write64(eval_len, measured)
            evaluated = measured
            if s64(evaluated) > 0 and runtime.memory.read8(
                u64(current + evaluated - 1)
            ) == 13:
                evaluated = u64(evaluated - 1)
                runtime.memory.write64(eval_len, evaluated)

            if s64(evaluated) > 0:
                context.data.push(current)
                context.data.push(evaluated)
                runtime.execute(evaluator.xt, context=context)
                # Preserve the source DUP's bounded-stack behavior while
                # retaining the original status beneath the comparison.
                context.data.push(context.data.peek())
                status = context.data.pop()
                if status != 0:
                    return
                context.data.pop()

            # Re-read every cell after evaluation.  Nested source evaluation
            # and even ordinary source writes therefore perturb these global
            # walker variables exactly as they do in the KDOS definition.
            measured = runtime.memory.read64(raw_len)
            runtime.memory.write64(
                cur,
                u64(runtime.memory.read64(cur) + measured),
            )
            runtime.memory.write64(
                rem,
                u64(runtime.memory.read64(rem) - measured),
            )
            if s64(runtime.memory.read64(rem)) > 0:
                runtime.memory.write64(cur, u64(runtime.memory.read64(cur) + 1))
                runtime.memory.write64(rem, u64(runtime.memory.read64(rem) - 1))

        runtime.memory.write64(eval_line, runtime.memory.read64(line))
        runtime.execute(finish.xt, context=context)

    return applicable, callback


def _build_loader_walk_accelerator(
    runtime: MegaForthRuntime,
    word: Word,
):
    evaluator = _require_call(runtime, word, 65, b"EVALUATE-CHECKED")
    status_throw = _require_call(runtime, word, 66, b"_LD-STATUS-THROW")
    finish = _require_call(runtime, word, 88, b"EVALUATE-FINISH")
    if _require_call(runtime, word, 89, b"_LD-STATUS-THROW") is not status_throw:
        raise ValueError("_LD-WALK does not bind one stable status translator")

    buffer = _require_call(runtime, word, 0, b"LD-BUF").body_address
    size = _require_call(runtime, word, 7, b"LD-SZ").body_address
    cur = _require_call(runtime, word, 2, b"LD-CUR").body_address
    line_len = _require_call(runtime, word, 42, b"LD-LEN").body_address
    line = _require_call(runtime, word, 5, b"LD-LINE").body_address
    eval_line = _require_call(runtime, word, 16, b"EVAL-LINE").body_address

    def applicable(_context: ExecutionContext) -> bool:
        remaining = runtime.memory.read64(size)
        address = runtime.memory.read64(buffer)
        return s64(remaining) <= 0 or _ordinary_span(
            runtime,
            address,
            remaining,
        )

    def callback(context: ExecutionContext) -> None:
        runtime.memory.write64(cur, runtime.memory.read64(buffer))
        runtime.memory.write64(line, 0)

        while s64(runtime.memory.read64(size)) > 0:
            line_number = u64(runtime.memory.read64(line) + 1)
            runtime.memory.write64(line, line_number)
            runtime.memory.write64(eval_line, line_number)

            current = runtime.memory.read64(cur)
            remaining = runtime.memory.read64(size)
            measured = _line_length(runtime, current, remaining)
            runtime.memory.write64(line_len, measured)
            evaluated = measured
            if s64(evaluated) > 0 and runtime.memory.read8(
                u64(current + evaluated - 1)
            ) == 13:
                evaluated = u64(evaluated - 1)

            if s64(evaluated) > 0:
                context.data.push(current)
                context.data.push(evaluated)
                runtime.execute(evaluator.xt, context=context)
                runtime.execute(status_throw.xt, context=context)

            measured = runtime.memory.read64(line_len)
            runtime.memory.write64(
                cur,
                u64(runtime.memory.read64(cur) + measured),
            )
            runtime.memory.write64(
                size,
                u64(runtime.memory.read64(size) - measured),
            )
            if s64(runtime.memory.read64(size)) > 0:
                runtime.memory.write64(cur, u64(runtime.memory.read64(cur) + 1))
                runtime.memory.write64(size, u64(runtime.memory.read64(size) - 1))

        runtime.execute(finish.xt, context=context)
        runtime.execute(status_throw.xt, context=context)

    return applicable, callback


def _build_line_length_accelerator(runtime: MegaForthRuntime):
    def applicable(context: ExecutionContext) -> bool:
        if context.data.depth() < 2:
            return False
        remaining = context.data.peek()
        address = context.data.peek(1)
        return s64(remaining) <= 0 or _ordinary_span(
            runtime,
            address,
            remaining,
        )

    def callback(context: ExecutionContext) -> None:
        remaining = context.data.pop()
        address = context.data.pop()
        context.data.push(_line_length(runtime, address, remaining))

    return applicable, callback


def _line_length(
    runtime: MegaForthRuntime,
    address: int,
    remaining: int,
) -> int:
    """Implement the KDOS signed-index LF scan over one admitted RAM span."""

    if s64(remaining) <= 0:
        return 0
    offset = 0
    while offset < remaining:
        count = min(_SCAN_CHUNK_BYTES, remaining - offset)
        payload = runtime.memory.read_bytes(address + offset, count)
        newline = payload.find(b"\n")
        if newline >= 0:
            return offset + newline
        offset += count
    return remaining


def _line_cannot_execute_guest(
    runtime: MegaForthRuntime,
    address: int,
    length: int,
    compiler,
    context: ExecutionContext,
    interpret_primitives: frozenset[Word],
    defining_primitives: dict[Word, int],
) -> bool:
    """Prove that one checked line cannot enter arbitrary guest code."""

    cursor = SourceCursor(runtime.memory.read_bytes(address, length))
    compiler_name = None if compiler is None else compiler.name
    compile_mode = compiler is not None and compiler.compile_mode
    temporary = compiler is not None and compiler.temporary
    literal_bytes = 0 if compiler is None else len(compiler.literal_pool)
    while True:
        token = cursor.parse_word()
        if not token:
            return True
        if token.startswith(b"\\"):
            return True

        try:
            word = runtime.dictionary.find(token)
        except ValueError:
            # The evaluator reports an overlong/non-ASCII dictionary token as
            # an ordinary undefined status.  No guest code executes first.
            word = None
        if word is None:
            continue

        implementation = word.implementation
        if isinstance(implementation, DirectiveDefinition):
            kind = implementation.kind
            if kind is DirectiveKind.PAREN_COMMENT:
                cursor.skip_parenthesis_comment()
                continue
            if kind is DirectiveKind.BACKSLASH_COMMENT:
                return True
            if not word.immediate:
                return False
            if kind is DirectiveKind.COLON:
                if compiler_name is not None:
                    return False
                compiler_name = cursor.parse_word()
                if not compiler_name:
                    return False
                try:
                    width = runtime.dictionary.definition_size(compiler_name)
                except (TypeError, ValueError):
                    return False
                if runtime._dictionary_growth_rejection(width, context) is not None:
                    return False
                compile_mode = True
                temporary = False
                literal_bytes = 0
                continue
            if kind is DirectiveKind.SEMICOLON:
                if compiler_name is None or temporary or not compile_mode:
                    return False
                try:
                    width = (
                        runtime.dictionary.definition_size(compiler_name)
                        + literal_bytes
                    )
                except (TypeError, ValueError):
                    return False
                if runtime._dictionary_growth_rejection(width, context) is not None:
                    return False
                return _tail_is_comment_only(runtime, cursor)
            if kind is DirectiveKind.PROVIDED:
                cursor.parse_word()
                continue
            if kind is DirectiveKind.DOT_QUOTE:
                cursor.consume_byte()
                cursor.consume_until(ord('"'))
                continue
            if kind in (DirectiveKind.S_QUOTE, DirectiveKind.ABORT_QUOTE):
                if not compile_mode:
                    return False
                cursor.consume_byte(0x20)
                payload = cursor.consume_until(ord('"')).data
                if kind is DirectiveKind.S_QUOTE:
                    literal_bytes += len(payload) + 1
                continue
            if kind in (
                DirectiveKind.BRACKET_TICK,
                DirectiveKind.BRACKET_CHAR,
                DirectiveKind.TO,
            ):
                if not compile_mode:
                    return False
                cursor.parse_word()
                continue
            if (
                compiler_name is None
                or temporary
                or not compile_mode
                or kind not in _COMPILE_ONLY_DIRECTIVES
            ):
                return False
            continue

        # A normal non-immediate word is appended as one Call while compiling.
        if compiler_name is not None and compile_mode and not word.immediate:
            continue
        if word.immediate:
            return False

        if isinstance(implementation, (ConstantDefinition, ValueDefinition)):
            continue
        if isinstance(implementation, CreatedDefinition):
            if implementation.action is None:
                continue
            return False
        if isinstance(implementation, PrimitiveDefinition):
            body_bytes = defining_primitives.get(word)
            if body_bytes is not None:
                name = cursor.parse_word()
                if not name:
                    return False
                try:
                    width = (
                        runtime.dictionary.definition_size(name)
                        + body_bytes
                    )
                except (TypeError, ValueError):
                    return False
                if runtime._dictionary_growth_rejection(
                    width + _CHECKED_LINE_STACK_RESERVE,
                    context,
                ) is not None:
                    return False
                return _tail_is_comment_only(runtime, cursor)
            if word in interpret_primitives:
                continue
        return False


def _tail_is_comment_only(
    runtime: MegaForthRuntime,
    cursor: SourceCursor,
) -> bool:
    """Admit only comments after a definition publishes a new binding."""

    while True:
        token = cursor.parse_word()
        if not token:
            return True
        if token.startswith(b"\\"):
            return True
        try:
            word = runtime.dictionary.find(token)
        except ValueError:
            return False
        if word is None or not isinstance(
            word.implementation,
            DirectiveDefinition,
        ):
            return False
        kind = word.implementation.kind
        if kind is DirectiveKind.BACKSLASH_COMMENT:
            return True
        if kind is not DirectiveKind.PAREN_COMMENT:
            return False
        cursor.skip_parenthesis_comment()


def _ordinary_span(
    runtime: MegaForthRuntime,
    address: int,
    length: int,
) -> bool:
    if not 0 <= address <= MASK64 or not 0 <= length <= MASK64:
        return False
    if length == 0:
        return True
    if length > MASK64 - address + 1:
        return False
    limit = address + length
    return any(
        region.base <= address and limit <= region.limit
        for region in runtime.memory.regions
    )


def _stack_has_room(stack, count: int) -> bool:
    return not stack.backed or stack.depth() + count <= stack.capacity


def _require_call(
    runtime: MegaForthRuntime,
    word: Word,
    index: int,
    name: bytes,
) -> Word:
    implementation = word.implementation
    if not isinstance(implementation, ColonDefinition):
        raise ValueError(f"source accelerator requires colon word {word.name!r}")
    try:
        operation: Operation = implementation.operations[index]
    except IndexError as exc:
        raise ValueError(
            f"source accelerator call index escaped {word.name!r}"
        ) from exc
    if not isinstance(operation, Call):
        raise ValueError(
            f"source accelerator expected a call in {word.name!r} at {index}"
        )
    target = runtime.dictionary.resolve(operation.xt)
    if target.name != name:
        raise ValueError(
            f"source accelerator expected {name!r} in {word.name!r} at {index}"
        )
    return target


def _require_literal_word(
    runtime: MegaForthRuntime,
    word: Word,
    index: int,
    name: bytes,
) -> Word:
    implementation = word.implementation
    if not isinstance(implementation, ColonDefinition):
        raise ValueError(f"source accelerator requires colon word {word.name!r}")
    try:
        operation: Operation = implementation.operations[index]
    except IndexError as exc:
        raise ValueError(
            f"source accelerator literal index escaped {word.name!r}"
        ) from exc
    if not isinstance(operation, Literal):
        raise ValueError(
            f"source accelerator expected a literal in {word.name!r} at {index}"
        )
    try:
        target = runtime.dictionary.resolve(operation.value)
    except KeyError as exc:
        raise ValueError(
            f"source accelerator literal in {word.name!r} is not a live XT"
        ) from exc
    if target.name != name:
        raise ValueError(
            f"source accelerator expected {name!r} in {word.name!r} at {index}"
        )
    return target


def _require_initial_primitive(
    runtime: MegaForthRuntime,
    word: Word,
) -> None:
    for candidate in runtime.dictionary.words:
        if (
            candidate.name.upper() == word.name.upper()
            and isinstance(candidate.implementation, PrimitiveDefinition)
        ):
            if candidate is not word:
                raise ValueError(
                    f"source accelerator requires initial primitive {word.name!r}"
                )
            return
    raise ValueError(f"source accelerator requires primitive {word.name!r}")


def _current_initial_primitive(
    runtime: MegaForthRuntime,
    name: bytes,
) -> Word | None:
    current = runtime.find(name)
    if current is None or not isinstance(
        current.implementation,
        PrimitiveDefinition,
    ):
        return None
    try:
        _require_initial_primitive(runtime, current)
    except ValueError:
        return None
    return current


def _field_bytes(value: object) -> bytes:
    if isinstance(value, bytes):
        return b"bytes:" + value.hex().encode("ascii")
    if isinstance(value, bool):
        return b"bool:1" if value else b"bool:0"
    if isinstance(value, int):
        return b"int:" + str(value).encode("ascii")
    raise TypeError(f"unsupported semantic operation field {value!r}")


__all__ = [
    "SourceAccelerationReport",
    "install_kdos_source_accelerators",
    "normalized_colon_digest",
]
