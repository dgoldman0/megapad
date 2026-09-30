"""Callback publication batches own only the exact handles they issue."""

from __future__ import annotations

from dataclasses import replace
import threading

import pytest

from shared.hybrid_abi import CallbackExportV2
from simulator.interop_exports import CallbackExportError, CallbackExportHandle
from simulator.runtime import MegaForthRuntime


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    value = MegaForthRuntime(execution_backend=request.param)
    try:
        yield value
    finally:
        value.memory.mmio.audio.release_host_sink()


def descriptor(export_id=0, name="MIN"):
    return CallbackExportV2(export_id=export_id, name=name,
                            input_cells=1 if name == "ABS" else 2, output_cells=1)


def publication_state(runtime):
    context = runtime.main_context
    return (context.data.snapshot(), context.returns.snapshot(),
            context.data.pointer, context.returns.pointer,
            runtime.dictionary.here, runtime.dictionary.latest,
            runtime.diagnostics.semantic_cycles, runtime.uart_output)


def test_success_reuses_existing_handles_and_returns_input_order_once(runtime, monkeypatch):
    existing = runtime.bind_callback_export(descriptor(7, "OR"))
    engine = runtime._callback_exports
    original = engine._bind_descriptor
    bound = []

    def record(value):
        bound.append(value.export_id)
        return original(value)

    monkeypatch.setattr(engine, "_bind_descriptor", record)
    before = publication_state(runtime)
    with runtime.callback_export_registration((
        descriptor(2, "ABS"), descriptor(7, "OR"), descriptor(2, "ABS"), descriptor(1, "MAX"),
    )) as handles:
        assert type(handles) is tuple
        assert tuple(handle.export_id for handle in handles) == (2, 7, 2, 1)
        assert handles[0] is handles[2] and handles[1] is existing
        assert runtime.verify_callback_export(handles[0]) == descriptor(2, "ABS")
        assert publication_state(runtime) == before
    assert bound == [2, 7, 1]
    assert runtime.invoke_callback_export(handles[0], ((1 << 64) - 1,)).outputs == (1,)
    assert runtime.invoke_callback_export(existing, (1, 2)).outputs == (3,)


def test_empty_batch_is_valid_and_preserves_existing_registration(runtime):
    handle = runtime.bind_callback_export(descriptor())
    table = dict(runtime._callback_exports._exports)
    with runtime.callback_export_registration(()) as handles:
        assert handles == ()
        assert runtime.verify_callback_export(handle) == descriptor()
    assert runtime._callback_exports._exports == table


@pytest.mark.parametrize("failure_type", [ValueError, KeyboardInterrupt, SystemExit])
def test_exception_revokes_only_new_exact_handles_and_preserves_original_error(runtime, failure_type):
    existing = runtime.bind_callback_export(descriptor(0, "OR"))
    before = publication_state(runtime)
    failure = failure_type("publication failed")
    with pytest.raises(failure_type) as caught:
        with runtime.callback_export_registration((descriptor(0, "OR"), descriptor(1))) as handles:
            raise failure
    assert caught.value is failure
    assert publication_state(runtime) == before
    assert runtime.verify_callback_export(existing) == descriptor(0, "OR")
    assert set(runtime._callback_exports._exports) == {0}
    with pytest.raises(CallbackExportError, match="issued identity"):
        runtime.verify_callback_export(handles[1])
    replacement = runtime.bind_callback_export(descriptor(1))
    assert replacement is not handles[1]
    assert runtime.invoke_callback_export(existing, (3, 5)).outputs == (7,)


def test_later_binding_failure_rolls_back_already_issued_handles(runtime, monkeypatch):
    existing = runtime.bind_callback_export(descriptor(7, "OR"))
    engine = runtime._callback_exports
    original = engine._bind_descriptor
    issued = []
    failure = RuntimeError("after insertion")

    def fail_after_insert(value):
        handle = original(value)
        issued.append(handle)
        if value.export_id == 2:
            raise failure
        return handle

    monkeypatch.setattr(engine, "_bind_descriptor", fail_after_insert)
    with pytest.raises(RuntimeError) as caught:
        with runtime.callback_export_registration((descriptor(1), descriptor(2, "MAX"))):
            pytest.fail("partially admitted batch yielded to its caller")
    assert caught.value is failure
    assert set(engine._exports) == {7}
    assert runtime.verify_callback_export(existing) == descriptor(7, "OR")
    for handle in issued:
        with pytest.raises(CallbackExportError, match="issued identity"):
            runtime.verify_callback_export(handle)


@pytest.mark.parametrize("values,error", [
    ([], TypeError), ([descriptor()], TypeError),
    ((object(),), TypeError), ((descriptor(),) * 65, ValueError),
    ((descriptor(), descriptor(0, "MAX")), CallbackExportError),
])
def test_invalid_batches_fail_before_any_publication(runtime, monkeypatch, values, error):
    engine = runtime._callback_exports

    def unexpected(_value):
        pytest.fail("invalid batch reached binding publication")

    monkeypatch.setattr(engine, "_bind_descriptor", unexpected)
    with pytest.raises(error):
        with runtime.callback_export_registration(values):
            pytest.fail("invalid batch yielded")
    assert engine._exports == {} and engine._registration is None


def test_forged_later_descriptor_is_revalidated_before_first_binding(runtime):
    invalid = descriptor(1)
    object.__setattr__(invalid, "output_cells", 2)
    with pytest.raises(ValueError, match="arity"):
        with runtime.callback_export_registration((descriptor(), invalid)):
            pytest.fail("forged descriptor yielded")
    assert runtime._callback_exports._exports == {}


def test_nested_registration_public_bind_and_invocation_are_blocked_during_batch(runtime):
    existing = runtime.bind_callback_export(descriptor(0))
    cycles = runtime.diagnostics.semantic_cycles
    with runtime.callback_export_registration((descriptor(1, "MAX"),)) as handles:
        with pytest.raises(CallbackExportError, match="nested callback export registration"):
            with runtime.callback_export_registration(()):
                pytest.fail("nested registration yielded")
        with pytest.raises(CallbackExportError, match="during export registration"):
            runtime.bind_callback_export(descriptor(2))
        for handle in (existing, handles[0]):
            with pytest.raises(CallbackExportError, match="during export registration"):
                runtime.invoke_callback_export(handle, (1, 2))
        assert runtime.diagnostics.semantic_cycles == cycles
    assert set(runtime._callback_exports._exports) == {0, 1}
    assert runtime.invoke_callback_export(handles[0], (1, 2)).outputs == (2,)


def test_registration_is_rejected_inside_active_callback(runtime, monkeypatch):
    handle = runtime.bind_callback_export(descriptor())
    account = runtime._account_semantic_step
    attempted = []

    def account_and_attempt():
        account()
        with pytest.raises(CallbackExportError, match="during a callback"):
            with runtime.callback_export_registration((descriptor(1),)):
                pytest.fail("active callback admitted publication")
        attempted.append(True)

    monkeypatch.setattr(runtime, "_account_semantic_step", account_and_attempt)
    assert runtime.invoke_callback_export(handle, (3, 5)).outputs == (3,)
    assert attempted == [True]
    assert set(runtime._callback_exports._exports) == {0}


def test_owner_lock_covers_body_and_is_released_after_commit_and_rollback(runtime):
    lock = runtime._session_owner_lock

    def other_thread_can_acquire():
        acquired = []

        def attempt():
            held = lock.acquire(blocking=False)
            acquired.append(held)
            if held:
                lock.release()

        thread = threading.Thread(target=attempt)
        thread.start()
        thread.join(timeout=2)
        assert not thread.is_alive()
        return acquired == [True]

    with runtime.callback_export_registration((descriptor(),)):
        assert not other_thread_can_acquire()
    assert other_thread_can_acquire()
    with pytest.raises(ValueError):
        with runtime.callback_export_registration((descriptor(1),)):
            assert not other_thread_can_acquire()
            raise ValueError("rollback")
    assert other_thread_can_acquire()


def test_failed_rollback_preserves_original_error_cause_and_disables_engine(runtime, monkeypatch):
    engine = runtime._callback_exports
    existing = runtime.bind_callback_export(descriptor(7))
    failure = ValueError("native publication failed")
    original_cause = RuntimeError("original native cause")
    failure.__cause__ = original_cause

    def failed_cleanup(_registration):
        raise OSError("injected cleanup failure")

    monkeypatch.setattr(engine, "_rollback_registration", failed_cleanup)
    with pytest.raises(ValueError) as caught:
        with runtime.callback_export_registration((descriptor(),)):
            raise failure
    assert caught.value is failure and failure.__cause__ is original_cause
    assert any("rollback failed" in note and "injected cleanup failure" in note
               for note in failure.__notes__)
    assert engine._registration is None
    for operation in (
        lambda: runtime.bind_callback_export(descriptor(1)),
        lambda: runtime.verify_callback_export(existing),
        lambda: runtime.invoke_callback_export(existing, (1, 2)),
    ):
        with pytest.raises(CallbackExportError, match="rollback failed"):
            operation()
    with pytest.raises(CallbackExportError, match="rollback failed"):
        with runtime.callback_export_registration(()):
            pytest.fail("failed engine admitted a new batch")


def test_rollback_never_removes_a_replacement_with_same_numerical_id(runtime):
    engine = runtime._callback_exports
    existing = runtime.bind_callback_export(descriptor(7))
    failure = RuntimeError("publication failed")
    with pytest.raises(RuntimeError) as caught:
        with runtime.callback_export_registration((descriptor(),)) as handles:
            issued = engine._exports[0]
            forged_handle = CallbackExportHandle(0, handles[0]._owner)
            replacement = replace(issued, handle=forged_handle)
            engine._exports[0] = replacement
            raise failure
    assert caught.value is failure
    assert engine._exports[0] is replacement
    assert engine._exports[7].handle is existing
    assert engine._registration_failure is not None
    with pytest.raises(CallbackExportError, match="rollback failed"):
        runtime.verify_callback_export(forged_handle)


def test_commit_checks_replaced_preexisting_identity_and_fails_closed(runtime):
    engine = runtime._callback_exports
    existing = runtime.bind_callback_export(descriptor(7))
    with pytest.raises(CallbackExportError, match="binding identity changed") as caught:
        with runtime.callback_export_registration((descriptor(),)):
            replacement = replace(engine._exports[7])
            engine._exports[7] = replacement
    assert engine._exports[7] is replacement
    assert 0 not in engine._exports
    assert any("rollback failed" in note for note in caught.value.__notes__)
    with pytest.raises(CallbackExportError, match="rollback failed"):
        runtime.verify_callback_export(existing)
