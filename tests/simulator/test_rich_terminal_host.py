"""Focused rich-terminal host-port coverage for the semantic simulator."""

from __future__ import annotations

import pytest

from rich_terminal import AdmissionStatus, EgressWatermarks, HostPortLimits
from simulator.errors import ExecutionError, StepBudgetExceeded
from simulator.rich_terminal_host import (
    SemanticBatchResult,
    SemanticBatchStop,
    SimulatorSessionBackend,
)
from simulator.runtime import MegaForthRuntime


def _limits(
    *,
    high_bytes: int = 16,
    low_bytes: int = 4,
    high_batches: int = 4,
    low_batches: int = 1,
    retained_bytes: int = 16,
) -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=high_bytes,
            low_bytes=low_bytes,
            high_batches=high_batches,
            low_batches=low_batches,
        ),
        retained_publication_bytes=retained_bytes,
        ingress_bytes=8,
        ingress_events=4,
        ingress_control_bytes=2,
        ingress_control_events=1,
        geometry_events=2,
    )


def test_attach_drains_completed_preattach_output_to_the_legacy_sink() -> None:
    runtime = MegaForthRuntime()
    legacy_batches: list[bytes] = []
    runtime.write_uart_bytes(b"legacy-before-attach")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_batches.append,
    )

    lease = backend.attach_rich_terminal(_limits())

    assert legacy_batches == [b"legacy-before-attach"]
    assert runtime.uart_output == b""
    assert lease.poll_egress().delivery is None
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_attach_drain_callback_cannot_reenter_the_session_boundary() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": MARK 90 ;")
    runtime.write_uart_bytes(b"legacy-before-attach")
    callbacks: list[str] = []
    backend: SimulatorSessionBackend

    def legacy_sink(payload: bytes) -> None:
        assert payload == b"legacy-before-attach"
        for operation in (
            lambda: backend.run_semantic_batch(entry="MARK"),
            lambda: backend.attach_rich_terminal(_limits()),
        ):
            with pytest.raises(RuntimeError, match="cannot reenter"):
                operation()
            callbacks.append("rejected")

    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_sink,
    )

    lease = backend.attach_rich_terminal(_limits())

    assert callbacks == ["rejected", "rejected"]
    assert runtime.main_context.data.snapshot() == ()
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_attached_semantic_word_publishes_one_batch_without_legacy_leak() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b': FRAME ." rich-frame" ;')
    legacy_batches: list[bytes] = []
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_batches.append,
    )
    lease = backend.attach_rich_terminal(_limits())

    result = backend.run_semantic_batch(entry="FRAME")

    assert isinstance(result, SemanticBatchResult)
    assert result.semantic_steps > 0
    assert result.external_events_applied == 0
    assert result.stop_reason is SemanticBatchStop.COMPLETED
    assert runtime.uart_output == b""
    assert legacy_batches == []
    delivery = lease.poll_egress().delivery
    assert delivery is not None
    assert delivery.batch.attachment_epoch == lease.attachment_epoch
    assert delivery.batch.publication_sequence == 0
    assert delivery.batch.payload == b"rich-frame"
    assert delivery.release() is AdmissionStatus.ACCEPTED
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_owned_runtime_rejects_terminal_and_execution_bypasses() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": TAKE KEY DROP ; : MARK 93 ;")
    primitive_rejections: list[str] = []

    def attempt_host_uart_bypass(_context) -> None:
        for operation in (
            lambda: runtime.inject_uart_input(b"primitive-bypass"),
            runtime.drain_uart_output,
        ):
            try:
                operation()
            except ExecutionError:
                primitive_rejections.append("rejected")

    runtime.define_primitive("TRY-HOST-UART-BYPASS", attempt_host_uart_bypass)
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )
    with pytest.raises(ExecutionError, match="already has a session owner"):
        SimulatorSessionBackend(
            runtime,
            legacy_output_sink=lambda payload: None,
        )
    for private_hook in (
        "apply_uart_ingress_locked",
        "drain_completed_egress_locked",
        "discard_machine_egress_locked",
        "bind_machine_sink_locked",
    ):
        assert not hasattr(backend, private_hook)
    lease = backend.attach_rich_terminal(_limits())

    bypasses = (
        lambda: runtime.inject_uart_input(b"bypass"),
        lambda: runtime.discard_uart_input_tail(0),
        lambda: runtime.write_uart_bytes(b"bypass"),
        runtime.flush_uart_output,
        runtime.drain_uart_output,
        lambda: runtime.execute("MARK"),
        lambda: runtime.run_until_blocked("MARK"),
        lambda: runtime.evaluate(b"94 CONSTANT BYPASS"),
        lambda: runtime.bios_evaluate(runtime.new_context()),
        lambda: runtime.define_constant("BYPASS", 94),
        lambda: runtime.set_numeric_base(16),
    )
    for bypass in bypasses:
        with pytest.raises(ExecutionError, match="owning session boundary"):
            bypass()
    with pytest.raises(RuntimeError, match="enhanced terminal lease owns"):
        backend.inject_legacy_uart_input(b"legacy")
    assert runtime.uart_input == b""
    assert runtime.uart_output == b""
    assert runtime.find("BYPASS") is None
    assert runtime.numeric_base == 10

    primitive = backend.run_semantic_batch(entry="TRY-HOST-UART-BYPASS")
    assert primitive.stop_reason is SemanticBatchStop.COMPLETED
    assert primitive_rejections == ["rejected", "rejected"]
    assert runtime.uart_input == b""
    assert runtime.uart_output == b""

    assert lease.submit_ingress(b"K") is AdmissionStatus.ACCEPTED
    result = backend.run_semantic_batch(entry="TAKE")
    assert result.stop_reason is SemanticBatchStop.COMPLETED
    assert runtime.uart_input == b""
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()
    backend.close()

    with pytest.raises(RuntimeError, match="session backend is closed"):
        backend.run_semantic_batch(entry="MARK")

    runtime.execute("MARK")
    assert runtime.main_context.data.pop() == 93


def test_resize_precedes_ingress_and_close_removes_only_attachment_suffix(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": TAKE-ONE KEY DROP ;")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
        terminal_cols=80,
        terminal_rows=24,
    )
    backend.inject_legacy_uart_input(b"LP")
    lease = backend.attach_rich_terminal(_limits())
    ingress_geometry: list[tuple[int, int]] = []
    original_apply_ingress = backend._apply_uart_ingress_locked

    def record_ingress_geometry(epoch: int, payload: bytes) -> None:
        ingress_geometry.append((backend.geometry.cols, backend.geometry.rows))
        original_apply_ingress(epoch, payload)

    monkeypatch.setattr(
        backend,
        "_apply_uart_ingress_locked",
        record_ingress_geometry,
    )

    assert (backend.geometry.cols, backend.geometry.rows) == (80, 24)
    assert not backend.geometry.resized
    geometry_snapshot = backend.geometry
    assert not hasattr(geometry_snapshot, "apply")
    with pytest.raises(AttributeError):
        geometry_snapshot.cols = 1  # type: ignore[misc]
    assert (
        lease.submit_resize(b"R", cols=100, rows=40)
        is AdmissionStatus.ACCEPTED
    )
    assert runtime.uart_input == b"LP"

    applied = backend.run_semantic_batch(entry="TAKE-ONE")

    assert applied.stop_reason is SemanticBatchStop.COMPLETED
    assert applied.external_events_applied == 1
    assert ingress_geometry == [(100, 40)]
    assert (backend.geometry.cols, backend.geometry.rows) == (100, 40)
    assert backend.geometry.resized
    # TAKE-ONE consumed the first legacy byte.  The other legacy byte still
    # precedes the resize payload applied for this attachment.
    assert runtime.uart_input == b"PR"

    assert lease.close() is AdmissionStatus.ACCEPTED
    assert runtime.uart_input == b"P"

    consumed_legacy = backend.run_semantic_batch(entry="TAKE-ONE")
    assert consumed_legacy.stop_reason is SemanticBatchStop.COMPLETED
    assert runtime.uart_input == b""
    backend.close()


def test_key_idl_resumes_only_after_admitted_ingress_reaches_the_boundary() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": WAIT-KEY KEY EMIT ;")
    legacy_batches: list[bytes] = []
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_batches.append,
    )
    lease = backend.attach_rich_terminal(_limits())
    context = runtime.new_context()

    blocked = backend.run_semantic_batch(entry="WAIT-KEY", context=context)

    assert blocked.stop_reason is SemanticBatchStop.IDLE
    assert blocked.semantic_steps > 0
    assert blocked.external_events_applied == 0
    assert backend.suspended
    assert context.suspended

    assert lease.submit_geometry(90, 30) is AdmissionStatus.ACCEPTED
    still_blocked = backend.run_semantic_batch()
    assert still_blocked.stop_reason is SemanticBatchStop.IDLE
    assert still_blocked.semantic_steps == 0
    assert still_blocked.external_events_applied == 1
    assert backend.suspended
    assert (backend.geometry.cols, backend.geometry.rows) == (90, 30)
    assert runtime.uart_output == b""

    assert lease.submit_ingress(b"K") is AdmissionStatus.ACCEPTED
    assert runtime.uart_input == b""
    resumed = backend.run_semantic_batch()

    assert resumed.stop_reason is SemanticBatchStop.COMPLETED
    assert resumed.semantic_steps > 0
    assert resumed.external_events_applied == 1
    assert not backend.suspended
    assert not context.suspended
    assert context.data.snapshot() == ()
    assert runtime.uart_input == b""
    assert legacy_batches == []
    delivery = lease.poll_egress().delivery
    assert delivery is not None
    assert (delivery.batch.publication_sequence, delivery.batch.payload) == (
        0,
        b"K",
    )
    assert delivery.release() is AdmissionStatus.ACCEPTED
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_failed_resume_releases_the_backend_owned_suspension() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": WAIT-KEY KEY DROP ; : MARK 91 ;")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )
    lease = backend.attach_rich_terminal(_limits())
    context = runtime.new_context()

    blocked = backend.run_semantic_batch(entry="WAIT-KEY", context=context)
    assert blocked.stop_reason is SemanticBatchStop.IDLE
    context.data.push(0xBAD)
    assert lease.submit_ingress(b"K") is AdmissionStatus.ACCEPTED

    with pytest.raises(ExecutionError, match="data stack changed"):
        backend.run_semantic_batch()

    assert not backend.suspended
    assert not context.suspended
    assert context.reusable
    fresh = runtime.new_context()
    completed = backend.run_semantic_batch(entry="MARK", context=fresh)
    assert completed.stop_reason is SemanticBatchStop.COMPLETED
    assert fresh.data.snapshot() == (91,)
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_owned_suspension_can_be_cancelled_explicitly() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": WAIT-KEY KEY DROP ; : MARK 92 ;")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )
    context = runtime.new_context()

    blocked = backend.run_semantic_batch(entry="WAIT-KEY", context=context)
    assert blocked.stop_reason is SemanticBatchStop.IDLE
    assert backend.cancel_suspension()
    assert not backend.cancel_suspension()
    assert not backend.suspended
    assert context.reusable
    completed = backend.run_semantic_batch(entry="MARK", context=context)
    assert completed.stop_reason is SemanticBatchStop.COMPLETED
    assert context.data.snapshot() == (92,)
    backend.close()


def test_multi_idle_step_deltas_and_budget_remain_one_outer_dispatch() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": IDLE [ 0 C, ] ; : TWICE IDLE IDLE ;")
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )
    lease = backend.attach_rich_terminal(_limits())
    context = runtime.new_context()

    first = backend.run_semantic_batch(entry="TWICE", context=context)
    assert lease.submit_ingress(b"W") is AdmissionStatus.ACCEPTED
    second = backend.run_semantic_batch()
    third = backend.run_semantic_batch()

    assert [first.semantic_steps, second.semantic_steps, third.semantic_steps] == [
        2,
        3,
        2,
    ]
    assert [first.stop_reason, second.stop_reason, third.stop_reason] == [
        SemanticBatchStop.IDLE,
        SemanticBatchStop.IDLE,
        SemanticBatchStop.COMPLETED,
    ]
    assert [
        first.external_events_applied,
        second.external_events_applied,
        third.external_events_applied,
    ] == [0, 1, 0]
    assert not backend.suspended
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert lease.close() is AdmissionStatus.ACCEPTED
    assert runtime.uart_input == b""
    backend.close()

    budget_runtime = MegaForthRuntime()
    budget_runtime.evaluate(b": IDLE [ 0 C, ] ; : TWICE IDLE IDLE ;")
    budget_backend = SimulatorSessionBackend(
        budget_runtime,
        legacy_output_sink=lambda payload: None,
    )
    budget_lease = budget_backend.attach_rich_terminal(_limits())
    budget_context = budget_runtime.new_context()

    budget_first = budget_backend.run_semantic_batch(
        entry="TWICE",
        context=budget_context,
        step_budget=5,
    )
    assert budget_lease.submit_ingress(b"W") is AdmissionStatus.ACCEPTED
    budget_second = budget_backend.run_semantic_batch()
    assert (budget_first.semantic_steps, budget_second.semantic_steps) == (2, 3)

    with pytest.raises(StepBudgetExceeded) as caught:
        budget_backend.run_semantic_batch()

    assert caught.value.budget == 5
    assert not budget_backend.suspended
    assert budget_context.reusable
    assert budget_lease.close() is AdmissionStatus.ACCEPTED
    assert budget_runtime.uart_input == b""
    budget_backend.close()


def test_close_discards_pending_enhanced_output_and_restores_legacy_route(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b': OLD ." old" ; : NEW ." new" ;')
    legacy_batches: list[bytes] = []
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_batches.append,
    )
    lease = backend.attach_rich_terminal(_limits())

    with monkeypatch.context() as patch:
        patch.setattr(backend, "_drain_completed_egress_locked", lambda: None)
        completed = backend.run_semantic_batch(entry="OLD")

    assert completed.stop_reason is SemanticBatchStop.COMPLETED
    assert runtime.uart_output == b"old"
    assert legacy_batches == []
    assert lease.poll_egress().delivery is None

    assert lease.close() is AdmissionStatus.ACCEPTED
    assert runtime.uart_output == b""
    assert lease.submit_ingress(b"stale") is AdmissionStatus.STALE
    assert lease.poll_egress().status is AdmissionStatus.STALE

    post_close = backend.run_semantic_batch(entry="NEW")
    assert post_close.stop_reason is SemanticBatchStop.COMPLETED
    assert legacy_batches == [b"new"]
    backend.close()


def test_retained_overshoot_blocks_later_word_before_its_side_effects() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b': FIRST ." ABC" ; : OVERSHOOT ." XY" ; : MARK 77 ;'
    )
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=lambda payload: None,
    )
    lease = backend.attach_rich_terminal(
        _limits(
            high_bytes=4,
            low_bytes=0,
            high_batches=2,
            low_batches=0,
            retained_bytes=4,
        )
    )
    context = runtime.new_context()

    first = backend.run_semantic_batch(entry="FIRST", context=context)
    overshoot = backend.run_semantic_batch(entry="OVERSHOOT", context=context)

    assert first.stop_reason is SemanticBatchStop.COMPLETED
    # The word that produced the overshooting publication has completed; the
    # retained record governs admission of the next semantic word.
    assert overshoot.stop_reason is SemanticBatchStop.COMPLETED
    assert backend.rich_terminal_host.retained_publication is not None
    assert backend.rich_terminal_host.retained_publication.payload == b"XY"
    assert context.data.snapshot() == ()

    blocked = backend.run_semantic_batch(entry="MARK", context=context)
    assert blocked == SemanticBatchResult(
        semantic_steps=0,
        external_events_applied=0,
        stop_reason=SemanticBatchStop.HOST_BACKPRESSURE,
    )
    assert context.data.snapshot() == ()

    delivery = lease.poll_egress().delivery
    assert delivery is not None
    assert delivery.batch.payload == b"ABC"
    assert delivery.release() is AdmissionStatus.ACCEPTED

    resumed = backend.run_semantic_batch(entry="MARK", context=context)
    assert resumed.stop_reason is SemanticBatchStop.COMPLETED
    assert resumed.semantic_steps > 0
    assert context.data.snapshot() == (77,)
    retained_delivery = lease.poll_egress().delivery
    assert retained_delivery is not None
    assert (
        retained_delivery.batch.publication_sequence,
        retained_delivery.batch.payload,
    ) == (1, b"XY")
    assert retained_delivery.release() is AdmissionStatus.ACCEPTED
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()


def test_oversized_publication_latches_failure_without_ansi_leak() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b': TOO-LARGE ." 123456789" ; : MARK 88 ;')
    legacy_batches: list[bytes] = []
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_batches.append,
    )
    lease = backend.attach_rich_terminal(_limits(retained_bytes=8))
    context = runtime.new_context()

    failed = backend.run_semantic_batch(entry="TOO-LARGE", context=context)

    assert failed.stop_reason is SemanticBatchStop.TERMINAL_FAILURE
    assert failed.semantic_steps > 0
    assert legacy_batches == []
    assert runtime.uart_output == b""
    assert lease.poll_egress().delivery is None
    assert "9 bytes" in backend.rich_terminal_host.failure_reason

    stopped = backend.run_semantic_batch(entry="MARK", context=context)
    assert stopped == SemanticBatchResult(
        semantic_steps=0,
        external_events_applied=0,
        stop_reason=SemanticBatchStop.TERMINAL_FAILURE,
    )
    assert context.data.snapshot() == ()
    assert legacy_batches == []
    assert lease.close() is AdmissionStatus.ACCEPTED
    backend.close()
