"""Application capability claims require the actual prepared native task owner.

This activation suite is gated separately from the transport/session foundation.
All positive cases use the compiled extension; patches only remove authority.
"""

from contextlib import contextmanager
from types import SimpleNamespace

import pytest

from hybrid.runtime import HybridExecutionError, HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from hybrid.task_adapter import NativeTaskAdapter
from shared.foreign_abi import FOREIGN_ABI, FOREIGN_ABI_VERSION
from simulator.foreign_runtime import ForeignTaskEngine, _FunctionSeal
from simulator.ir import Idle, Return
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import MegaForthRuntime
from tests.test_native_task_adapter import machine_snapshot, native, register_callback


@pytest.fixture(params=("python", "native"))
def owner(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    hybrid = HybridRuntime.create(executor=request.param,
        geometry={"bank0_size": 65536, "external_size": 65536})
    runtime, base = hybrid.semantic, EXTERNAL_BASE + 0x1000
    runtime.configure_dictionary_bounds(base, EXTERNAL_BASE + 0x10000, runtime.main_context)
    runtime.allot_dictionary(base - runtime.dictionary.here, runtime.main_context)
    try:
        yield hybrid
    finally:
        hybrid.close()


@contextmanager
def session_for(hybrid, entry="ABS", **options):
    session = HybridSession(hybrid, entry, **options)
    machine = HybridSharedMachine(session)
    try:
        yield session, machine
    finally:
        session.close()


def task_flags(status):
    flags = status["runtime"]["capabilities"]
    return tuple(flags[name] for name in (
        "shared_task_exceptions", "callback_suspension", "composite_suspension"))


def test_actual_prepared_owner_advertises_task_abi_separately_from_private_profiles(owner):
    adapter = NativeTaskAdapter(owner)
    before = machine_snapshot(owner), adapter.last_receipt()
    with session_for(owner) as (_session, machine):
        status = machine.status()
        assert task_flags(status) == (True, True, True)
        assert status["runtime"]["capabilities"]["semantic_callbacks"] is True
        task = status["task_execution"]
        assert task["abi"] == FOREIGN_ABI and task["abi_version"] == FOREIGN_ABI_VERSION
        assert task["native_transport_revision"] == 2
        assert task["max_depth"] == 8 and task["prepared_only"] is True
        assert task["callback_executor"] == "python_reference"
        assert task["quantum_instructions"] is None
        assert task["registered_words"] == []
        assert task["instructions"] == task["callback_semantic_steps"] == 0
        # Task support does not rewrite the separate private ABI report.
        assert status["machine_execution"]["registered_abi_versions"] == []
        assert status["machine_execution"]["callback_exports"] == []
        assert status["machine_execution"]["callback_profile"] is None
        assert (machine_snapshot(owner), adapter.last_receipt()) == before
        task["registered_words"].append("UNISSUED")
        assert machine.status()["task_execution"]["registered_words"] == []
        assert task_flags(machine.status()) == (True, True, True)


@pytest.mark.parametrize("fake_projection", (False, True))
def test_absent_or_unissued_task_owner_cannot_advertise_capabilities(owner, fake_projection):
    if fake_projection:
        owner._task_adapter = SimpleNamespace(registered_words=("FAKE",))
    with session_for(owner) as (_session, machine):
        status = machine.status()
        assert task_flags(status) == (False, False, False)
        assert "task_execution" not in status


def test_older_revision2_without_parked_query_retains_synchronous_task_execution(owner, monkeypatch):
    with monkeypatch.context() as patches:
        patches.delattr(native.TaskRoutineRunnerV1, "validate_parked")
        adapter = NativeTaskAdapter(owner)
        word, _ = register_callback(owner, adapter)
    # Restoring the module attribute cannot retroactively add a route that was
    # absent when this exact adapter selected its native transport.
    owner.semantic.main_context.data.push(7)
    with session_for(owner, word.xt) as (session, machine):
        assert task_flags(machine.status()) == (True, False, False)
        session.boot()
        assert session.run_boundary().stop_reason.value == "completed"
        assert owner.semantic.main_context.data.snapshot() == (7, 7)
        assert owner.machine_instructions == 5 and owner.callback_semantic_steps == 1
        assert task_flags(machine.status()) == (True, False, False)


def test_finite_machine_quantum_rejects_unavailable_composite_before_owner_claim(owner, monkeypatch):
    with monkeypatch.context() as patches:
        patches.delattr(native.TaskRoutineRunnerV1, "validate_parked")
        adapter = NativeTaskAdapter(owner)
    before = machine_snapshot(owner), adapter.last_receipt()
    with pytest.raises(RuntimeError, match="composite|suspension|transport"):
        HybridSession(owner, "ABS", machine_quantum_instructions=1)
    assert owner.semantic._session_owner_token is None
    assert (machine_snapshot(owner), adapter.last_receipt()) == before


def test_finite_machine_quantum_requires_installed_task_owner_before_owner_claim(owner):
    before = machine_snapshot(owner)
    with pytest.raises(RuntimeError, match="composite|task|transport"):
        HybridSession(owner, "ABS", machine_quantum_instructions=1)
    assert owner.semantic._session_owner_token is None
    assert owner._task_adapter is None and machine_snapshot(owner) == before


@pytest.mark.parametrize("route,expected", (
    ("validate_parked", (True, False, False)),
    ("advance", (False, False, False)),
))
def test_replaced_native_routes_do_not_qualify_via_callable_or_true_result(owner, monkeypatch, route, expected):
    adapter = NativeTaskAdapter(owner)
    called = []

    def replacement(*args, **kwargs):
        called.append(True)
        return True

    with session_for(owner) as (_session, machine):
        before = machine_snapshot(owner), adapter.last_receipt()
        with monkeypatch.context() as patches:
            patches.setattr(native.TaskRoutineRunnerV1, route, replacement)
            assert task_flags(machine.status()) == expected
            assert called == []
        assert task_flags(machine.status()) == (True, True, True)
        assert (machine_snapshot(owner), adapter.last_receipt()) == before


@pytest.mark.parametrize("revision", (True, 1, 3))
def test_unrecognized_native_revision_cannot_select_task_adapter(owner, monkeypatch, revision):
    before = machine_snapshot(owner)
    monkeypatch.setattr(native, "_TASK_ROUTINE_TRANSPORT_REVISION", revision)
    with pytest.raises(RuntimeError, match="revision 2"):
        NativeTaskAdapter(owner)
    assert owner._task_adapter is None and machine_snapshot(owner) == before


def test_capability_reads_while_parked_do_not_rotate_or_validate_guest_authority(owner):
    runtime = owner.semantic
    runtime.define_colon("PAUSE", (Idle(), Return()))
    adapter = NativeTaskAdapter(owner)
    word, _ = register_callback(owner, adapter, "PAUSE", inputs=0, outputs=0)
    with session_for(owner, word.xt) as (session, machine):
        session.boot()
        assert session.run_boundary().stop_reason.value == "idle"
        frame = adapter._frames[-1]
        receipt = adapter.last_receipt()
        before = machine_snapshot(owner), runtime._foreign_tasks._task_root.ledger.semantic_steps
        for _ in range(3):
            assert task_flags(machine.status()) == (True, True, True)
            assert adapter._frames[-1] is frame
            assert adapter.last_receipt() is receipt
            assert (machine_snapshot(owner), runtime._foreign_tasks._task_root.ledger.semantic_steps) == before


def test_changed_installation_projection_is_not_an_alternate_capability_authority(owner):
    adapter = NativeTaskAdapter(owner)
    with session_for(owner) as (_session, machine):
        original = owner._task_authority
        before = machine_snapshot(owner), adapter.last_receipt()
        owner._task_authority = object()
        try:
            with pytest.raises(HybridExecutionError, match="authority changed"):
                machine.status()
        finally:
            owner._task_authority = original
        assert (machine_snapshot(owner), adapter.last_receipt()) == before


@pytest.mark.parametrize("marker,expected", (
    ("TASK_CALLBACK_ABI_VERSION", (False, False, False)),
    ("TASK_CALLBACK_SUSPENSION_VERSION", (True, False, False)),
    ("TASK_MACHINE_QUANTUM_VERSION", (True, True, False)),
))
def test_semantic_qualifications_require_exact_selected_versions(owner, monkeypatch, marker, expected):
    adapter = NativeTaskAdapter(owner)
    before = machine_snapshot(owner), adapter.last_receipt()
    with session_for(owner) as (_session, machine):
        with monkeypatch.context() as patches:
            patches.setattr(ForeignTaskEngine, marker, True)
            assert task_flags(machine.status()) == expected
        assert task_flags(machine.status()) == (True, True, True)
        assert (machine_snapshot(owner), adapter.last_receipt()) == before


def test_scheduler_function_body_change_is_not_qualified_by_unchanged_route(owner, monkeypatch):
    adapter = NativeTaskAdapter(owner)
    original = ForeignTaskEngine._machine_host_caller

    def replacement(self, frame):
        raise AssertionError("a capability read must never call scheduling helpers")

    with session_for(owner) as (_session, machine):
        before = machine_snapshot(owner), adapter.last_receipt()
        with monkeypatch.context() as patches:
            patches.setattr(original, "__code__", replacement.__code__)
            assert ForeignTaskEngine._machine_host_caller is original
            assert task_flags(machine.status()) == (True, True, False)
        assert task_flags(machine.status()) == (True, True, True)
        assert (machine_snapshot(owner), adapter.last_receipt()) == before


@pytest.mark.parametrize("kind,name,expected", (
    (ForeignTaskEngine, "root_for", (False, False, False)),
    (ForeignTaskEngine, "resume_suspension", (True, False, False)),
    (MegaForthRuntime, "_continue_foreign", (False, False, False)),
    (NativeTaskAdapter, "begin", (False, False, False)),
))
@pytest.mark.parametrize("replace_body", (False, True))
def test_adapter_installation_cannot_bless_preexisting_route_or_body_substitution(
    owner, monkeypatch, kind, name, expected, replace_body,
):
    original = getattr(kind, name)

    def replacement(*args, **kwargs):
        raise AssertionError("capability observations must never call a substituted route")

    before = machine_snapshot(owner)
    with monkeypatch.context() as patches:
        if replace_body:
            patches.setattr(original, "__code__", replacement.__code__)
        else:
            patches.setattr(kind, name, replacement)
        adapter = NativeTaskAdapter(owner)
        status = owner.task_execution_status
        assert tuple(status[key] for key in (
            "shared_task_exceptions", "callback_suspension", "composite_suspension")) == expected
        assert machine_snapshot(owner) == before and adapter._runner.last_receipt() is None


def test_capability_observation_does_not_invoke_changed_seal_verifier(owner, monkeypatch):
    adapter = NativeTaskAdapter(owner)
    before = machine_snapshot(owner), adapter.last_receipt()

    def replacement(self):
        raise AssertionError("read-only capability observation invoked a changed verifier")

    with monkeypatch.context() as patches:
        patches.setattr(_FunctionSeal.verify, "__code__", replacement.__code__)
        status = owner.task_execution_status
        assert tuple(status[key] for key in (
            "shared_task_exceptions", "callback_suspension", "composite_suspension")) == (False, False, False)
        assert (machine_snapshot(owner), adapter.last_receipt()) == before
