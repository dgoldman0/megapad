"""Atomic host task publication and original-meter profile selection."""

from dataclasses import FrozenInstanceError, replace

import pytest

from simulator.foreign_runtime import ForeignTaskError, TaskAdapterRootPolicy
from simulator.ir import Call, Literal, Return
from simulator.runtime import YieldedExecution, _StepMeter
from tests.simulator.test_foreign_runtime import _Adapter, operation, runtime, signature
from tests.simulator.test_foreign_dispatch import (
    ObservedAdapter, adapter, register, runtime as dispatch_runtime,
)
from tests.simulator.foreign_reference import Return as MachineReturn


def test_batch_publishes_actual_foreign_body_and_exact_transitive_dependencies():
    result, transport = runtime(), _Adapter()
    engine = result._foreign_tasks
    child_operation, dynamic_operation = operation(), operation()
    with engine.registration_batch() as batch:
        child = batch.define_operation("CHILD", transport, child_operation, initial_body=b"\x01" * 32)
        dynamic = batch.define_operation("DYNAMIC", transport, dynamic_operation)
        middle = batch.define_colon("MIDDLE", (Call(child.xt), Return()))
        outer = batch.define_colon("OUTER", (Call(middle.xt), Return()))
        export = batch.capture_export(outer, signature(), dynamic_targets=(dynamic, child))
        lease = batch.body_lease(child)
        dependencies = batch.task_export_dependencies(export)
        assert len(dependencies) == 2
        assert any(item is child_operation for item in dependencies)
        assert any(item is dynamic_operation for item in dependencies)
    assert result.dictionary.is_body_lease_live(lease)
    assert lease.word is child and lease.body_limit - lease.body_address == 32
    assert result.memory.read_bytes(lease.body_address, 32) == b"\x01" * 32
    assert engine.require_definition(child).body_lease is lease
    assert engine.task_export_dependencies(export) == dependencies
    with pytest.raises(ForeignTaskError):
        engine.task_export_dependencies(replace(export))
    with pytest.raises(ForeignTaskError, match="stale"):
        batch.define_operation("LATE", transport, operation())


def test_modified_body_cannot_enter_and_repair_does_not_renew_reclaimed_lease():
    result, transport = runtime(), _Adapter()
    engine = result._foreign_tasks
    before = result.dictionary.checkpoint()
    with engine.registration_batch() as batch:
        word = batch.define_operation("CODE", transport, operation(), initial_body=b"abcd")
        lease = batch.body_lease(word)
    result.memory.write8(lease.body_address, ord("x"))
    with pytest.raises(ForeignTaskError, match="body"):
        engine.require_definition(word)
    result.memory.write8(lease.body_address, ord("a"))
    assert engine.require_definition(word).body_lease is lease
    result.dictionary.rollback(before)
    result.memory.write_bytes(lease.body_address, b"abcd")
    with pytest.raises((ForeignTaskError, KeyError)):
        engine.require_definition(word)
    assert not result.dictionary.is_body_lease_live(lease)


def test_valid_lease_field_mutation_cannot_redirect_sealed_body_evidence():
    result = runtime()
    with result._foreign_tasks.registration_batch() as batch:
        word = batch.define_operation("CODE", _Adapter(), operation(), initial_body=b"abcd")
        lease = batch.body_lease(word)
    object.__setattr__(lease, "body_address", lease.body_address + 1)
    with pytest.raises(ForeignTaskError, match="body"):
        result._foreign_tasks.require_definition(word)


def test_native_rollback_precedes_dictionary_rollback_and_preserves_prior_authority():
    result, transport = runtime(), _Adapter()
    engine = result._foreign_tasks
    previous = engine.define_operation("PRIOR", transport, operation())
    prior_export = engine.capture_export("DUP", signature(1, 2))
    here, captures = result.dictionary.here, engine._capture_state
    error, observed = KeyboardInterrupt("publication failed"), []
    with pytest.raises(KeyboardInterrupt) as caught:
        with engine.registration_batch() as batch:
            word = batch.define_operation("NEW", transport, operation(), initial_body=b"\x01" * 16)
            lease = batch.body_lease(word)

            def rollback():
                observed.append((result.dictionary.resolve(word.xt) is word,
                                 result.dictionary.is_body_lease_live(lease)))

            batch.on_rollback(rollback)
            batch.capture_export("DROP", signature(1, 0))
            raise error
    assert caught.value is error and observed == [(True, True)]
    assert result.dictionary.here == here and result.find("NEW") is None
    assert engine._capture_state is captures
    assert engine.require_definition(previous).word is previous
    assert engine.require_export(prior_export).descriptor is prior_export
    assert not result.dictionary.is_body_lease_live(lease)


def test_failed_rollback_preserves_primary_and_disables_further_task_publication():
    result, original = runtime(), KeyboardInterrupt("original failure")
    cleanup = RuntimeError("native cleanup failed")
    here = result.dictionary.here

    def rollback():
        raise cleanup

    with pytest.raises(KeyboardInterrupt) as caught:
        with result._foreign_tasks.registration_batch() as batch:
            batch.on_rollback(rollback)
            batch.define_operation("NEW", _Adapter(), operation(), initial_body=b"body")
            raise original
    assert caught.value is original and result.dictionary.here == here
    assert result.find("NEW") is None
    with pytest.raises(ForeignTaskError, match="rollback failed"):
        result._foreign_tasks.capture_export("DROP", signature(1, 0))


def test_forwarding_publication_failure_rolls_back_even_if_host_catches_it(monkeypatch):
    result, error = runtime(), RuntimeError("after publication")
    publish = result._define_public_dictionary_word
    here = result.dictionary.here

    def forwarding(*args, **kwargs):
        publish(*args, **kwargs)
        raise error

    monkeypatch.setattr(result, "_define_public_dictionary_word", forwarding)
    with pytest.raises(RuntimeError) as caught:
        with result._foreign_tasks.registration_batch() as batch:
            with pytest.raises(RuntimeError):
                batch.define_operation("NEW", _Adapter(), operation(), initial_body=b"body")
            batch._failure = None  # A public diagnostic cannot renew commit authority.
    assert caught.value is error and result.dictionary.here == here
    assert result.find("NEW") is None


@pytest.mark.parametrize("attempt", ["evaluate", "execute", "define", "capture", "nested"])
def test_batch_rejects_public_execution_and_other_publication(attempt):
    result = runtime()
    engine = result._foreign_tasks
    with engine.registration_batch() as batch:
        with pytest.raises((ForeignTaskError, RuntimeError)):
            if attempt == "evaluate":
                result.evaluate(b"77")
            elif attempt == "execute":
                result.execute("DUP")
            elif attempt == "define":
                result.define_constant("SIDE", 9)
            elif attempt == "capture":
                engine.capture_export("DROP", signature(1, 0))
            else:
                with engine.registration_batch():
                    pass
        batch.define_colon("SAFE", (Literal(3), Return()))
    assert result.main_context.data.snapshot() == () and result.find("SIDE") is None
    result.execute("SAFE")
    assert result.main_context.data.snapshot() == (3,)


def test_publication_hook_cannot_reenter_the_exact_batch(monkeypatch):
    result = runtime()
    publish = result._define_public_dictionary_word
    attempted = []
    with result._foreign_tasks.registration_batch() as batch:
        def forwarding(*args, **kwargs):
            with pytest.raises(ForeignTaskError, match="reentrant"):
                batch.capture_export("DROP", signature(1, 0))
            attempted.append(True)
            return publish(*args, **kwargs)

        monkeypatch.setattr(result, "_define_public_dictionary_word", forwarding)
        batch.define_operation("ONE", _Adapter(), operation())
    assert attempted == [True] and result.find("ONE") is not None


def test_batch_has_one_exact_rollback_participant():
    result = runtime()
    with result._foreign_tasks.registration_batch() as batch:
        with pytest.raises(TypeError):
            batch.on_rollback(object())
        batch.on_rollback(lambda: None)
        with pytest.raises(ForeignTaskError, match="one rollback"):
            batch.on_rollback(lambda: None)


def test_returned_batch_cannot_replace_the_enrolled_cleanup_evidence():
    result, original, called = runtime(), RuntimeError("publication"), []
    with pytest.raises(RuntimeError) as caught:
        with result._foreign_tasks.registration_batch() as batch:
            batch.on_rollback(lambda: called.append(True))
            batch._rollback = None
            batch._rollback_owned = lambda *args: called.append(False)
            raise original
    assert caught.value is original and called == [True]


def test_batch_cannot_publish_for_two_independent_adapter_owners():
    result = runtime()
    here = result.dictionary.here
    with pytest.raises(ForeignTaskError, match="one exact adapter"):
        with result._foreign_tasks.registration_batch() as batch:
            batch.define_operation("FIRST", _Adapter(), operation())
            batch.define_operation("SECOND", _Adapter(), operation())
    assert result.dictionary.here == here
    assert result.find("FIRST") is None and result.find("SECOND") is None


def test_changed_rollback_function_is_not_called_and_failure_is_sticky():
    result, original, called = runtime(), RuntimeError("publication"), []

    def rollback():
        return len(called)

    def replacement():
        called.append(True)

    with pytest.raises(RuntimeError) as caught:
        with result._foreign_tasks.registration_batch() as batch:
            batch.on_rollback(rollback)
            rollback.__code__ = replacement.__code__
            raise original
    assert caught.value is original and called == []
    with pytest.raises(ForeignTaskError, match="rollback failed"):
        result._foreign_tasks.capture_export("DUP", signature(1, 2))


def test_adapter_policy_contains_original_limits_and_requires_exact_live_owner(monkeypatch):
    result = dispatch_runtime()
    engine, policies, rejected = result._foreign_tasks, [], []
    engine.configure_limits(instruction_limit=11, callback_limit=7, entry_limit=3)
    original_begin = ObservedAdapter.begin

    def begin(self, *args, **kwargs):
        token, root_id = kwargs["root_token"], kwargs["root_id"]
        policy = engine.adapter_root_policy(self, token, root_id)
        policies.append(policy)
        for wrong in ((object(), token, root_id), (self, object(), root_id), (self, token, root_id + 1)):
            with pytest.raises(ForeignTaskError):
                engine.adapter_root_policy(*wrong)
            rejected.append(True)
        with pytest.raises(FrozenInstanceError):
            policy.entry_limit = 99
        return original_begin(self, *args, **kwargs)

    monkeypatch.setattr(ObservedAdapter, "begin", begin)
    inner = adapter(result)
    word, _ = register(result, inner, (MachineReturn(outputs=()),), observed=True)
    result.execute(word.xt)
    assert policies == [TaskAdapterRootPolicy(11, 7, 3)] and rejected == [True] * 3
    with pytest.raises(ForeignTaskError):
        engine.adapter_root_policy(inner, object(), 1)


@pytest.mark.parametrize("first", ["private", "task"])
def test_profile_exclusion_outlives_top_level_word_dispatches_and_never_consumes_inputs(first):
    result = dispatch_runtime()
    engine = result._foreign_tasks

    def private(context):
        engine.claim_machine_profile(result._active_dispatches[-1].meter, "private")

    result.define_primitive("PRIVATE", private)
    inner = adapter(result)
    register(result, inner, (MachineReturn(outputs=()),), name="TASK")
    with pytest.raises(ForeignTaskError, match="cannot mix"):
        result.evaluate(b"99 PRIVATE TASK" if first == "private" else b"99 TASK PRIVATE")
    assert result.main_context.data.snapshot() == (99,)
    assert len(inner.admitted_inputs) == (0 if first == "private" else 1)
    # A later independent public root receives a new original meter.
    result.execute("TASK" if first == "private" else "PRIVATE")
    assert result.main_context.data.snapshot() == (99,)


def test_private_profile_choice_survives_ordinary_host_quantum():
    result = dispatch_runtime()
    engine = result._foreign_tasks
    result.define_primitive("PRIVATE", lambda context: engine.claim_machine_profile(
        result._active_dispatches[-1].meter, "private"))
    inner = adapter(result)
    register(result, inner, (MachineReturn(outputs=()),), name="TASK")
    result.evaluate(b": BOTH PRIVATE 1 DROP TASK ;")
    state = result.run_until_blocked("BOTH", quantum_steps=3)
    assert type(state) is YieldedExecution
    with pytest.raises(ForeignTaskError, match="cannot mix"):
        for _ in range(12):
            assert type(state) is YieldedExecution
            state = result.resume_yielded(state.suspension)
        pytest.fail("task entry incorrectly renewed the original profile after a quantum")
    assert inner.admitted_inputs == ()


def test_profile_choice_requires_an_actual_original_dispatch():
    result = runtime()
    with pytest.raises(ForeignTaskError, match="active"):
        result._foreign_tasks.claim_machine_profile(_StepMeter(None, lambda: None), "task")
