"""Only admitted task accounting/adapter errors preserve host ABORT provenance."""

import pytest

from simulator.errors import ForthAbort
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_dispatch import TaskDispatchRoot
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.stacks import ReturnStack
from tests.simulator.foreign_reference import Callback, Reply, Return
from tests.simulator.test_foreign_dispatch import adapter, capture, register, signature


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return MegaForthRuntime(execution_backend=request.param,
        memory=create_one_core_address_space(external_size=65536, dense_backing=True))


@pytest.mark.parametrize("entry", ("execute", "evaluate"))
def test_task_accounting_abort_is_raw_and_preserves_guest_prefix(runtime, monkeypatch, entry):
    inner = adapter(runtime)
    export = capture(runtime, "DROP", 1, 0)
    word, _ = register(runtime, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(19)
    error = ForthAbort("task accounting host failure")
    account = runtime._account_semantic_step
    def fail():
        account()
        if any(type(item) is ForeignContinuation for item in context.returns.snapshot()):
            raise error
    monkeypatch.setattr(runtime, "_account_semantic_step", fail)
    with pytest.raises(ForthAbort) as caught:
        if entry == "execute":
            runtime.execute(word.xt)
        else:
            runtime.evaluate(b"MACHINE")
    assert caught.value is error and error.origin_context is None
    assert context.data.snapshot() == (99, 7)
    assert context.returns.snapshot() == (19,)
    assert inner.active_invocations == () and inner.replies == ()
    assert runtime._foreign_tasks.last_dispatch.semantic_steps == 1
    assert runtime._private_host_abort._error is None


@pytest.mark.parametrize("secondary", (False, True))
def test_task_adapter_abort_and_secondary_cleanup_keep_primary_provenance(runtime, secondary):
    inner = adapter(runtime)
    export = capture(runtime, "DROP", 1, 0)
    operation = inner.register(signature(), (Callback(export=export, arguments=(7,)), Return(outputs=())))
    primary = ForthAbort("adapter host failure")
    cleanup = ForthAbort("secondary adapter cleanup failure")
    class HostAdapter:
        def begin(self, *args, **kwargs):
            inner.begin(*args, **kwargs)
            raise primary
        def advance(self, *args, **kwargs):
            return inner.advance(*args, **kwargs)
        def reply(self, *args, **kwargs):
            return inner.reply(*args, **kwargs)
        def cancel_suffix(self, *args, **kwargs):
            return inner.cancel_suffix(*args, **kwargs)
        def cancel_all(self):
            result = inner.cancel_all()
            if secondary:
                raise cleanup
            return result
        def last_receipt(self):
            return inner.last_receipt()
    word = runtime._foreign_tasks.define_operation("MACHINE", HostAdapter(), operation)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(19)
    with pytest.raises(ForthAbort) as caught:
        runtime.execute(word.xt)
    assert caught.value is primary and primary.origin_context is None
    assert cleanup.origin_context is None
    assert context.data.snapshot() == (99,)
    assert context.returns.snapshot() == (19,)
    assert inner.active_invocations == ()
    assert runtime._private_host_abort._error is None


def test_actual_guest_abort_in_task_callback_still_resets_original_task(runtime):
    inner = adapter(runtime)
    export = capture(runtime, "ABORT")
    word, _ = register(runtime, inner, (Callback(export=export, arguments=()), Return(outputs=())))
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(19)
    with pytest.raises(ForthAbort) as caught:
        runtime.execute(word.xt)
    assert caught.value.origin_context is context
    assert context.data.snapshot() == context.returns.snapshot() == ()
    assert inner.active_invocations == ()
    assert runtime._private_host_abort._error is None


def test_task_host_abort_survives_stack_instance_namespace_replacement(runtime, monkeypatch):
    inner = adapter(runtime)
    export = capture(runtime, "DROP", 1, 0)
    word, _ = register(runtime, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(19)
    error = ForthAbort("task host failure after namespace replacement")
    replacements = []
    account = runtime._account_semantic_step
    namespace = vars(ReturnStack)["__dict__"]

    with monkeypatch.context() as patch:
        def fail():
            account()
            if any(type(item) is ForeignContinuation for item in context.returns.snapshot()):
                # Instance namespace replacement is supported by Python. Keep
                # the canonical routes and completed stack bytes intact so
                # normal error cleanup remains authorized through the new map.
                replacement = dict(namespace.__get__(context.returns, ReturnStack))
                namespace.__set__(context.returns, replacement)
                replacements.append(replacement)
                raise error

        patch.setattr(runtime, "_account_semantic_step", fail)
        with pytest.raises(ForthAbort) as caught:
            runtime.execute(word.xt)

    assert caught.value is error and error.origin_context is None
    assert len(replacements) == 1
    assert namespace.__get__(context.returns, ReturnStack) is replacements[0]
    assert context.data.snapshot() == (99, 7)
    assert context.returns.snapshot() == (19,)
    assert inner.active_invocations == () and inner.replies == ()
    assert runtime._private_host_abort._error is None


def test_task_issuer_cannot_be_installed_or_issued_from_an_unrelated_host_frame(runtime):
    with pytest.raises(RuntimeError, match="original engine construction"):
        runtime._private_host_abort.install_task_issuers(runtime._foreign_tasks, TaskDispatchRoot)
    error = ForthAbort("unissued host error")
    runtime._private_host_abort.issue_task(object(), error)
    def ordinary(context):
        raise error
    word = runtime.define_primitive("ORDINARY", ordinary)
    runtime.main_context.data.push(99)
    with pytest.raises(ForthAbort) as caught:
        runtime.execute(word.xt)
    assert caught.value is error and error.origin_context is runtime.main_context
    assert runtime.main_context.data.snapshot() == ()
