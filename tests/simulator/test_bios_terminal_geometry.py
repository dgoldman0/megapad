"""Focused public BIOS terminal geometry coverage for hosted sessions."""

from __future__ import annotations

import pytest

from rich_terminal import AdmissionStatus, EgressWatermarks, HostPortLimits
from shared.cells import TRUE
from simulator.errors import ExecutionError
from simulator.rich_terminal_host import (
    HostedTerminalGeometry,
    SemanticBatchStop,
    SimulatorSessionBackend,
)
from simulator.runtime import MegaForthRuntime


def _limits() -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=256,
            low_bytes=0,
            high_batches=4,
            low_batches=0,
        ),
        retained_publication_bytes=256,
        ingress_bytes=256,
        ingress_events=4,
        ingress_control_bytes=128,
        ingress_control_events=2,
        geometry_events=2,
    )


def _read_geometry(
    backend: SimulatorSessionBackend,
) -> tuple[int, int, int]:
    context = backend.runtime.new_context()
    result = backend.run_semantic_batch(entry="READ-GEOMETRY", context=context)
    assert result.stop_reason is SemanticBatchStop.COMPLETED
    return context.data.snapshot()


def _run_word(
    backend: SimulatorSessionBackend,
    name: str,
    *cells: int,
) -> tuple[int, ...]:
    context = backend.runtime.new_context()
    for cell in cells:
        context.data.push(cell)
    result = backend.run_semantic_batch(entry=name, context=context)
    assert result.stop_reason is SemanticBatchStop.COMPLETED
    return context.data.snapshot()


def test_unowned_runtime_exposes_fixed_legacy_geometry_and_consumes_resize() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": READ-GEOMETRY COLS ROWS RESIZED? ;")

    runtime.execute("READ-GEOMETRY")
    assert runtime.main_context.data.snapshot() == (80, 24, 0)
    runtime.main_context.data.clear()

    runtime.set_terminal_geometry(132, 43)
    runtime.execute("READ-GEOMETRY")
    assert runtime.main_context.data.snapshot() == (132, 43, TRUE)
    runtime.main_context.data.clear()
    runtime.execute("READ-GEOMETRY")
    assert runtime.main_context.data.snapshot() == (132, 43, 0)


def test_termsize_pushes_one_coherent_cols_rows_snapshot_in_bios_order() -> None:
    runtime = MegaForthRuntime()
    runtime.set_terminal_geometry(132, 43)

    runtime.execute("TERMSIZE")

    # ROWS is the top stack item, matching ( -- cols rows ).
    assert runtime.main_context.data.snapshot() == (132, 43)


def test_session_geometry_is_the_guest_visible_source_and_resize_is_read_once() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": READ-GEOMETRY COLS ROWS RESIZED? ;")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
        terminal_cols=96,
        terminal_rows=31,
    )

    assert _read_geometry(backend) == (96, 31, 0)

    backend.set_legacy_geometry(100, 40)
    assert backend.geometry.resized
    assert _read_geometry(backend) == (100, 40, TRUE)
    assert not backend.geometry.resized
    assert _read_geometry(backend) == (100, 40, 0)

    lease = backend.attach_rich_terminal(_limits())
    assert lease.submit_geometry(120, 50) is AdmissionStatus.ACCEPTED
    assert backend.geometry == HostedTerminalGeometry(100, 40, False)

    # The host event crosses before this guest call, so all three public words
    # observe one coherent newly resized geometry.
    assert _read_geometry(backend) == (120, 50, TRUE)
    assert backend.geometry == HostedTerminalGeometry(120, 50, False)
    assert _read_geometry(backend) == (120, 50, 0)

    with pytest.raises(RuntimeError, match="enhanced terminal lease owns"):
        backend.set_legacy_geometry(121, 51)

    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.set_legacy_geometry(90, 28)
    assert _read_geometry(backend) == (90, 28, TRUE)

    # UART_GEOM treats every accepted host update as a resize notification,
    # including an update that repeats the current dimensions.
    backend.set_legacy_geometry(90, 28)
    assert backend.geometry == HostedTerminalGeometry(90, 28, True)
    backend.close()

    # Detachment leaves one fixed legacy snapshot rather than an MMIO device
    # or a callback into the closed backend. Its outstanding sticky flag is
    # transferred and still clears exactly once.
    runtime.execute("READ-GEOMETRY")
    assert runtime.main_context.data.snapshot() == (90, 28, TRUE)
    runtime.main_context.data.clear()
    runtime.execute("READ-GEOMETRY")
    assert runtime.main_context.data.snapshot() == (90, 28, 0)


def test_owned_runtime_rejects_host_geometry_bypass() -> None:
    runtime = MegaForthRuntime()
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )

    with pytest.raises(ExecutionError, match="owning session boundary"):
        runtime.set_terminal_geometry(90, 30)

    assert backend.geometry.cols == 80
    assert backend.geometry.rows == 24
    backend.close()


def test_guest_resize_request_is_low16_atomic_replaceable_and_generation_safe() -> None:
    runtime = MegaForthRuntime()
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )

    assert backend.snapshot_resize_request() is None
    assert _run_word(
        backend,
        "RESIZE-REQUEST",
        0x1_1234,
        0x2_5678,
    ) == ()
    first = backend.snapshot_resize_request()
    assert first is not None
    first_generation, first_cols, first_rows = first
    assert (first_cols, first_rows) == (0x1234, 0x5678)

    # A second complete word replaces the request atomically. Zero is a valid
    # low-16-bit request value even though it is not a valid host geometry.
    assert _run_word(
        backend,
        "RESIZE-REQUEST",
        0x2_0000,
        0x3_FFFF,
    ) == ()
    replacement = backend.snapshot_resize_request()
    assert replacement is not None
    generation, cols, rows = replacement
    assert generation != first_generation
    assert (cols, rows) == (0, 0xFFFF)

    assert not backend.host_accept_resize_if_pending(
        first_generation,
        100,
        40,
    )
    assert not backend.host_deny_resize_if_pending(first_generation)
    assert backend.snapshot_resize_request() == replacement

    assert backend.host_accept_resize_if_pending(generation, 132, 43)
    assert backend.snapshot_resize_request() is None
    assert backend.geometry == HostedTerminalGeometry(132, 43, True, False)
    assert _run_word(backend, "RESIZED?") == (TRUE,)
    assert _run_word(backend, "RESIZED?") == (0,)
    backend.close()


def test_resize_and_denied_notifications_are_independent_clear_on_read_flags() -> None:
    runtime = MegaForthRuntime()
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )

    assert _run_word(backend, "RESIZE-REQUEST", 90, 30) == ()
    request = backend.snapshot_resize_request()
    assert request is not None
    generation, _, _ = request
    assert backend.host_deny_resize_if_pending(generation)
    assert backend.snapshot_resize_request() is None
    assert backend.geometry == HostedTerminalGeometry(80, 24, False, True)

    # A later request and acceptance set RESIZED without clearing the older
    # denial notification. Neither a new request nor completion clears STATUS.
    assert _run_word(backend, "RESIZE-REQUEST", 100, 40) == ()
    accepted = backend.snapshot_resize_request()
    assert accepted is not None
    accepted_generation, _, _ = accepted
    assert backend.host_accept_resize_if_pending(
        accepted_generation,
        100,
        40,
    )
    assert backend.geometry == HostedTerminalGeometry(100, 40, True, True)

    # Clearing one W1C-equivalent status must not consume the other.
    assert _run_word(backend, "RESIZE-DENIED?") == (TRUE,)
    assert backend.geometry == HostedTerminalGeometry(100, 40, True, False)
    assert _run_word(backend, "RESIZE-DENIED?") == (0,)
    assert _run_word(backend, "RESIZED?") == (TRUE,)
    assert backend.geometry == HostedTerminalGeometry(100, 40, False, False)
    assert _run_word(backend, "RESIZED?") == (0,)
    backend.close()
