"""Focused checked BIOS entropy publication semantics."""

from __future__ import annotations

import pytest

from shared.cells import MASK64, TRUE
from simulator.entropy import (
    TRNG_RAND8,
    HostedTRNGService,
)
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime


SEED = bytes(range(32))
DESTINATION = EXTERNAL_BASE + 0x100


def _runtime(*, entropy_usable: bool = True) -> MegaForthRuntime:
    return MegaForthRuntime(
        memory=create_one_core_address_space(
            external_size=0x1000,
            entropy_seed=SEED,
            entropy_usable=entropy_usable,
        )
    )


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.new_context()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name, context=context)
    assert context.returns.snapshot() == ()
    return context.data.snapshot()


def test_entropy_fill_publishes_the_exact_hosted_stream() -> None:
    runtime = _runtime()
    reference = HostedTRNGService(SEED)
    expected = bytes(reference.read8(TRNG_RAND8) for _ in range(80))

    assert _execute(runtime, "ENTROPY-FILL", DESTINATION, len(expected)) == (0,)
    assert runtime.memory.read_bytes(DESTINATION, len(expected)) == expected
    assert runtime.entropy.pool_position == reference.pool_position
    assert runtime.entropy.refill_counter == reference.refill_counter


def test_entropy_ready_is_canonical_and_empty_fill_ignores_source_and_address() -> None:
    runtime = _runtime()

    assert _execute(runtime, "ENTROPY-READY?") == (TRUE,)
    runtime.entropy.latch_unusable()
    assert _execute(runtime, "ENTROPY-READY?") == (0,)
    position = runtime.entropy.pool_position
    assert _execute(runtime, "ENTROPY-FILL", MASK64, 0) == (0,)
    assert runtime.entropy.pool_position == position


def test_entropy_fill_rejects_range_and_protected_spans_before_consumption() -> None:
    runtime = _runtime()
    position = runtime.entropy.pool_position
    floor = runtime.dictionary.numeric_rollback_floor

    assert _execute(
        runtime,
        "ENTROPY-FILL",
        EXTERNAL_BASE + 0xFFF,
        2,
    ) == (2,)
    assert _execute(runtime, "ENTROPY-FILL", floor - 1, 1) == (3,)
    assert runtime.entropy.pool_position == position


def test_entropy_unavailable_before_publication_preserves_destination() -> None:
    runtime = _runtime(entropy_usable=False)
    original = b"destination"
    runtime.memory.write_bytes(DESTINATION, original)

    assert _execute(runtime, "ENTROPY-FILL", DESTINATION, len(original)) == (1,)
    assert runtime.memory.read_bytes(DESTINATION, len(original)) == original


def test_final_byte_health_loss_wipes_the_complete_admitted_span(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    runtime = _runtime()
    original_read8 = HostedTRNGService.read8
    random_reads = 0

    def read8_then_fail(service: HostedTRNGService, offset: int) -> int:
        nonlocal random_reads
        value = original_read8(service, offset)
        if service is runtime.entropy and offset == TRNG_RAND8:
            random_reads += 1
            if random_reads == 2:
                service.latch_unusable()
        return value

    monkeypatch.setattr(HostedTRNGService, "read8", read8_then_fail)
    runtime.memory.write_bytes(DESTINATION, b"\xA5" * 2)

    assert _execute(runtime, "ENTROPY-FILL", DESTINATION, 2) == (1,)
    assert random_reads == 2
    assert runtime.memory.read_bytes(DESTINATION, 2) == bytes(2)
