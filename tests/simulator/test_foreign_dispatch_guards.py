"""Consumed-route and lifetime checks for the synchronous task foundation."""

import pytest

from simulator import core_words, runtime as runtime_module
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_effects import TaskEffectGuard
from simulator.foreign_runtime import ForeignTaskError
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import ExecutionContext, YieldedExecution
from tests.simulator.foreign_reference import Callback, Reply, Return
from tests.simulator.test_foreign_dispatch import adapter, capture, register, runtime, signature
from shared.cells import u64


def on_first_callback(result, monkeypatch, effect):
    account = result._account_semantic_step
    seen = []

    def tick():
        account()
        if not seen and any(type(item) is ForeignContinuation
                            for item in result.main_context.returns.snapshot()):
            seen.append(True)
            effect(result._foreign_tasks._task_root)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    return seen


@pytest.mark.parametrize("replace_namespace", [False, True])
@pytest.mark.parametrize("raise_original", [False, True])
def test_accounting_corruption_cannot_refund_accepted_semantic_tick(monkeypatch, replace_namespace, raise_original):
    result, error = runtime(), KeyboardInterrupt("original accounting error")
    inner = adapter(result)
    export = capture(result, "DROP", inputs=1)
    word, _ = register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    evidence = []

    def corrupt(task):
        meter = task.ledger.meter
        evidence.append((meter, meter.__dict__, meter.steps))
        if replace_namespace:
            meter.__dict__ = dict(meter.__dict__, steps=-1)
        else:
            meter.steps = -1
        if raise_original:
            raise error

    seen = on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(KeyboardInterrupt if raise_original else ForeignTaskError) as caught:
        result.execute(word.xt)
    if raise_original:
        assert caught.value is error
    meter, namespace, charged = evidence[0]
    assert seen == [True] and meter.__dict__ is namespace and meter.steps == charged
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result.main_context.data.snapshot() == (7,)
    assert inner.active_invocations == () and inner.replies == ()


@pytest.mark.parametrize("route", ["binding", "class", "alias", "dispatch_alias"])
def test_late_effect_route_replacement_cannot_publish_an_ungranted_write(monkeypatch, route):
    result = runtime()
    result.evaluate(f": WRITE 65 {EXTERNAL_BASE} C! ;".encode())
    inner = adapter(result)
    export = capture(result, "WRITE")
    word, _ = register(result, inner, (Callback(export=export, arguments=()), Return(outputs=())))
    called = []

    def noop(*args):
        called.append(True)

    class Counterfeit:
        require_access = staticmethod(noop)

    class Probe(type):
        def __instancecheck__(cls, value):
            called.append(True)
            return False

    class FakeLiteral(metaclass=Probe):
        pass

    def corrupt(task):
        if route == "binding":
            result.main_context.data._task_effect_guard = None
        elif route == "class":
            monkeypatch.setattr(TaskEffectGuard, "require_access", noop)
        elif route == "alias":
            monkeypatch.setattr(core_words, "TaskEffectGuard", Counterfeit)
        else:
            monkeypatch.setattr(runtime_module, "Literal", FakeLiteral)

    on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert called == []
    assert result.memory.read8(EXTERNAL_BASE) == 0
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert inner.active_invocations == () and inner.replies == ()


def test_one_root_cannot_rebase_receipts_onto_an_independent_adapter():
    result = runtime()
    first, second = adapter(result), adapter(result)
    register(result, first, (Return(outputs=()),), name="FIRST")
    register(result, second, (Return(outputs=()),), name="SECOND")
    result.evaluate(b": BOTH FIRST SECOND ;")
    with pytest.raises(ForeignTaskError, match="one exact adapter"):
        result.execute("BOTH")
    assert first.admitted_inputs == ((1, ()),) and second.admitted_inputs == ()
    assert result._foreign_tasks.last_dispatch.entries == 1


def test_replaced_stack_restore_is_never_called_during_task_error_cleanup(monkeypatch):
    result, error = runtime(), KeyboardInterrupt("original host failure")
    inner = adapter(result)
    export = capture(result, "DROP", inputs=1)
    word, _ = register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    invoked = []

    def corrupt(task):
        monkeypatch.setattr(type(result.main_context.returns), "restore", lambda *args: invoked.append(True))
        raise error

    on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute(word.xt)
    assert caught.value is error and invoked == []
    assert result._active_dispatches == [] and not result.main_context.reusable
    assert inner.active_invocations == ()


@pytest.mark.parametrize("stack_name", ["data", "returns"])
def test_rejected_stack_setter_is_not_called_by_canonical_cleanup(monkeypatch, stack_name):
    result, error = runtime(), KeyboardInterrupt("original setter replacement")
    inner = adapter(result)
    export = capture(result, "DROP", inputs=1)
    word, _ = register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    called = []

    def corrupt(task):
        kind = type(getattr(result.main_context, stack_name))
        monkeypatch.setattr(kind, "__setattr__", lambda *args: called.append(True))
        raise error

    on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute(word.xt)
    assert caught.value is error and called == []
    assert result._active_dispatches == [] and not result.main_context.reusable
    assert inner.active_invocations == () and inner.replies == ()
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1


@pytest.mark.parametrize("field", ["close", "cleanup_safe", "context", "ledger", "closed"])
def test_rejected_root_projection_cannot_select_cleanup_or_poison_another_context(monkeypatch, field):
    result = runtime()
    other = ExecutionContext()
    inner = adapter(result)
    export = capture(result, "DROP", inputs=1)
    word, _ = register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    called = []

    def replacement(*args, **kwargs):
        called.append(True)

    def corrupt(task):
        setattr(task, field, other if field == "context" else True if field == "closed" else replacement)

    on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert called == [] and other.reusable and other.data.snapshot() == ()
    assert result.main_context.data.snapshot() == (7,)
    assert result._active_dispatches == [] and inner.active_invocations == ()
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1


def test_replaced_root_ledger_cannot_mask_original_accounting_error(monkeypatch):
    result, error = runtime(), KeyboardInterrupt("original ledger replacement")
    inner = adapter(result)
    export = capture(result, "DROP", inputs=1)
    word, _ = register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    called, evidence = [], []

    class Probe:
        def __getattribute__(self, name):
            called.append(name)
            raise AssertionError("replacement ledger must not be consumed")

    def corrupt(task):
        evidence.append((task.ledger.meter, task.ledger.meter.steps))
        task.ledger = Probe()
        raise error

    on_first_callback(result, monkeypatch, corrupt)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute(word.xt)
    assert caught.value is error and called == []
    assert evidence[0][0].steps == evidence[0][1]
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result.main_context.data.snapshot() == (7,)
    assert result._active_dispatches == [] and inner.active_invocations == ()


def test_child_throw_reaches_surviving_parent_foreign_return_without_replying_to_child():
    result = runtime(exceptions=True)
    result.evaluate(b": RAISE -17 THROW ;")
    inner = adapter(result)
    child_export = capture(result, "RAISE")
    child_operation = inner.register(signature(), (
        Callback(export=child_export, arguments=()), Return(outputs=())))
    child = result._foreign_tasks.define_operation("CHILD", inner, child_operation)
    parent_export = capture(result, "CATCH", inputs=1, outputs=1, dynamic=(child,))
    parent, _ = register(result, inner, (
        Callback(export=parent_export, arguments=(child.xt,), children=(child_operation,)),
        Return(outputs=(Reply(0),))), outputs=1, name="PARENT")
    result.main_context.data.push(99)
    result.execute(parent.xt)
    assert result.main_context.data.snapshot() == (99, u64(-17))
    assert inner.replies == ((1, 1, (u64(-17),)),)
    assert inner.active_invocations == ()
    assert result.main_context.returns.snapshot() == ()
    assert result.memory.read64(result.find("_TASK-HANDLERS").body_address) == 0
    report = result._foreign_tasks.last_dispatch
    assert report.entries == report.callbacks == 2 and report.machine_instructions == 3
    assert report.completed and report.cancelled


class ReceiptFailureAdapter:
    def __init__(self, inner, error):
        self.inner, self.error, self.fail = inner, error, False

    def begin(self, *args, **kwargs):
        return self.inner.begin(*args, **kwargs)

    def advance(self, *args, **kwargs):
        return self.inner.advance(*args, **kwargs)

    def reply(self, *args, **kwargs):
        return self.inner.reply(*args, **kwargs)

    def cancel_suffix(self, *args, **kwargs):
        return self.inner.cancel_suffix(*args, **kwargs)

    def cancel_all(self):
        return self.inner.cancel_all()

    def last_receipt(self):
        if self.fail:
            raise self.error
        return self.inner.last_receipt()


def test_cancelled_outer_quantum_releases_suspension_even_if_final_receipt_query_fails():
    result, error = runtime(), KeyboardInterrupt("receipt cleanup failed")
    inner = adapter(result)
    proxy = ReceiptFailureAdapter(inner, error)
    operation = inner.register(signature(), (Return(outputs=()),))
    result._foreign_tasks.define_operation("MACHINE", proxy, operation)
    result.evaluate(b": TAIL MACHINE 1 DROP 2 DROP ;")
    yielded = result.run_until_blocked("TAIL", quantum_steps=3)
    assert type(yielded) is YieldedExecution and inner.active_invocations == ()
    proxy.fail = True
    with pytest.raises(KeyboardInterrupt) as caught:
        result.cancel_suspension(yielded.suspension)
    assert caught.value is error
    assert result._suspended_execution is None and result._active_dispatches == []
    assert not result.main_context.suspended
    assert result.main_context.returns.snapshot() == ()
    with pytest.raises(ForeignTaskError, match="admission is disabled"):
        result.execute("MACHINE")
