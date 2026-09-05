"""Fail-closed semantic acceleration for the ordinary KDOS source path.

The accelerated words remain their exact source-compiled colon definitions in
the guest dictionary.  This module recognizes only pinned IR shapes and asks
the runtime to overlay their execution.  Filesystem lookup, extent reads,
``REQUIRE``/``PROVIDED`` registration, loader transactions, dictionary
rollback, evaluator calls, and allocation release all remain ordinary KDOS.

Only the byte-oriented orchestration around the existing hosted evaluator is
collapsed.  A mismatched or absent definition is left completely untouched and
continues through the normal semantic dispatcher.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass, fields

from shared.cells import MASK64, s64, u64
from simulator.dictionary import Word
from simulator.ir import Call, Operation
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    DirectiveDefinition,
    ExecutionContext,
    MegaForthRuntime,
)


_SCAN_CHUNK_BYTES = 4096
_SEC_MEASURE_DIGEST = (
    "8baea05066c381040309e896a87a46325be8435e62345b982cafe782c6b9375f"
)
_SEC_ADVANCE_DIGEST = (
    "ce1b43e6121fb2ff09f93dd4d4236339ce0e6f5e6a8657d71f97ea679d804594"
)


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
# immediacy, while address-specific XTs are deliberately excluded.
_SPECS = (
    _AccelerationSpec(
        b"SOURCE-EVALUATE-CHECKED",
        "bdf5ce4dc6e01bbbb89fbdbae60793030c866576757f90178dcb5e79018ef935",
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
            if spec.name == b"SOURCE-EVALUATE-CHECKED":
                applicable, callback = _build_checked_source_accelerator(
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

    cur = _required_body(runtime, b"_SEC-CUR")
    rem = _required_body(runtime, b"_SEC-REM")
    raw_len = _required_body(runtime, b"_SEC-RAW-LEN")
    eval_len = _required_body(runtime, b"_SEC-EVAL-LEN")
    line = _required_body(runtime, b"_SEC-LINE")
    eval_line = _required_body(runtime, b"EVAL-LINE")

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

    buffer = _required_body(runtime, b"LD-BUF")
    size = _required_body(runtime, b"LD-SZ")
    cur = _required_body(runtime, b"LD-CUR")
    line_len = _required_body(runtime, b"LD-LEN")
    line = _required_body(runtime, b"LD-LINE")
    eval_line = _required_body(runtime, b"EVAL-LINE")

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


def _required_body(runtime: MegaForthRuntime, name: bytes) -> int:
    word = runtime.find(name)
    if word is None:
        raise ValueError(f"source accelerator requires {name!r}")
    return word.body_address


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
