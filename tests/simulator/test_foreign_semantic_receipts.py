"""Issued cumulative callback work survives task completion and cleanup errors."""

from dataclasses import FrozenInstanceError, replace

import pytest

from simulator.errors import ForthAbort
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_runtime import ForeignTaskError
from simulator import foreign_runtime
from tests.simulator.foreign_reference import Callback, Return
from tests.simulator.test_foreign_dispatch import adapter, capture, runtime, signature


class SemanticReceiptAdapter:
    def __init__(self, inner, engine):
        self.inner, self.engine = inner, engine
        self.receipts = []
        self.total = 0
        self.latest = None
        self.error = None

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
        return self.inner.last_receipt()

    def settle_semantic_receipt(self, receipt):
        assert self.engine.task_semantic_receipt(self, receipt.root_token, receipt.root_id) is receipt
        self.receipts.append(receipt)
        if self.latest is not receipt:
            previous = self.latest.semantic_steps if (self.latest is not None
                and self.latest.root_token is receipt.root_token) else 0
            self.total += receipt.semantic_steps - previous
            self.latest = receipt
        if self.error is not None:
            raise self.error


def setup(*, callback="DROP", inputs=(7,), outputs=0, exceptions=False):
    result = runtime(exceptions=exceptions)
    inner = adapter(result)
    proxy = SemanticReceiptAdapter(inner, result._foreign_tasks)
    export = capture(result, callback, len(inputs), outputs)
    operation = inner.register(signature(), (Callback(export=export, arguments=inputs), Return(outputs=())))
    word = result._foreign_tasks.define_operation("MACHINE", proxy, operation)
    return result, inner, proxy, word


def test_completion_issues_exact_immutable_receipt_independent_of_native_work():
    result, inner, proxy, word = setup()
    result.execute(word.xt)
    receipt, = proxy.receipts
    assert receipt.sequence == 1 and receipt.semantic_steps == proxy.total == 1
    assert inner.last_receipt().root_instructions == 2
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result._foreign_tasks.task_semantic_receipt(proxy, receipt.root_token, receipt.root_id) is receipt
    with pytest.raises(FrozenInstanceError):
        receipt.semantic_steps = 999
    for arguments in ((object(), receipt.root_token, receipt.root_id),
                      (proxy, object(), receipt.root_id),
                      (proxy, receipt.root_token, receipt.root_id + 1)):
        with pytest.raises(ForeignTaskError):
            result._foreign_tasks.task_semantic_receipt(*arguments)
    copied = replace(receipt)
    assert copied is not result._foreign_tasks.task_semantic_receipt(proxy, copied.root_token, copied.root_id)


def test_valid_receipt_field_mutation_does_not_change_issued_work():
    result, _inner, proxy, word = setup()
    result.execute(word.xt)
    receipt = proxy.latest
    object.__setattr__(receipt, "semantic_steps", 999)
    with pytest.raises(ForeignTaskError, match="changed"):
        result._foreign_tasks.task_semantic_receipt(proxy, receipt.root_token, receipt.root_id)
    assert result._foreign_tasks.last_dispatch.semantic_steps == proxy.total == 1


@pytest.mark.parametrize("error", [KeyboardInterrupt("accounting delivery"), ForthAbort(b"host accounting")])
def test_settlement_forward_then_raise_retains_exact_receipt_and_still_cancels(error):
    result, inner, proxy, word = setup()
    proxy.error = error
    result.main_context.data.push(99)
    with pytest.raises(type(error)) as caught:
        result.execute(word.xt)
    assert caught.value is error
    if type(error) is ForthAbort:
        assert error.origin_context is None
    assert result.main_context.data.snapshot() == (99,)
    assert inner.active_invocations == () and result.main_context.returns.snapshot() == ()
    receipt = proxy.latest
    assert receipt.semantic_steps == 1 and result._foreign_tasks.last_dispatch.semantic_steps == 1
    proxy.error = None
    proxy.settle_semantic_receipt(result._foreign_tasks.task_semantic_receipt(
        proxy, receipt.root_token, receipt.root_id))
    assert proxy.receipts == [receipt, receipt] and proxy.total == 1
    with pytest.raises(ForeignTaskError, match="disabled"):
        result.execute(word.xt)


def test_original_guest_abort_keeps_its_origin_and_charged_callback_receipt():
    result, inner, proxy, word = setup(callback="ABORT", inputs=())
    result.main_context.data.push(99)
    with pytest.raises(ForthAbort):
        result.execute(word.xt)
    assert result.main_context.data.snapshot() == ()
    assert inner.active_invocations == ()
    assert proxy.total == proxy.latest.semantic_steps == 1


def test_original_host_failure_precedes_secondary_settlement_error(monkeypatch):
    result, inner, proxy, word = setup()
    original, secondary = KeyboardInterrupt("original tick"), ForthAbort(b"secondary settlement")
    proxy.error = secondary
    account = result._account_semantic_step

    def tick():
        account()
        if any(type(entry) is ForeignContinuation for entry in result.main_context.returns.snapshot()):
            raise original

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    result.main_context.data.push(99)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute(word.xt)
    assert caught.value is original
    assert result.main_context.data.snapshot() == (99, 7)
    assert inner.active_invocations == () and proxy.total == 1
    assert result._foreign_tasks.task_semantic_receipt(proxy, proxy.latest.root_token,
                                                      proxy.latest.root_id) is proxy.latest


def test_primary_suspension_publication_error_survives_finally_settlement_error(monkeypatch):
    result, inner, proxy, _word = setup()
    result.evaluate(b": TAIL MACHINE 1 DROP ;")
    result.main_context.data.push(99)
    original = KeyboardInterrupt("suspension publication")
    proxy.error = ForthAbort("secondary settlement")

    def fail_handle():
        raise original

    monkeypatch.setattr(result, "_allocate_suspension_handle", fail_handle)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.run_until_blocked("TAIL", quantum_steps=3)
    assert caught.value is original
    assert result.main_context.data.snapshot() == (99,)
    assert result._active_dispatches == [] and result._suspended_execution is None
    assert inner.active_invocations == () and proxy.total == 1


def test_rejected_seal_supports_route_is_not_called_during_cleanup(monkeypatch):
    result, inner, proxy, word = setup()
    account, called = result._account_semantic_step, []

    def replacement(*args):
        called.append(True)
        return False

    def tick():
        account()
        if any(type(entry) is ForeignContinuation for entry in result.main_context.returns.snapshot()):
            monkeypatch.setattr(foreign_runtime._AdapterSeal, "supports", replacement)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert called == [] and inner.active_invocations == ()
    assert proxy.total == 1 and result._active_dispatches == []


def test_changed_optional_hook_cannot_replace_mandatory_cancellation(monkeypatch):
    result, inner, proxy, word = setup()
    account, invoked = result._account_semantic_step, []

    def replacement(self, receipt):
        invoked.append(True)

    def tick():
        account()
        if any(type(entry) is ForeignContinuation for entry in result.main_context.returns.snapshot()):
            monkeypatch.setattr(SemanticReceiptAdapter, "settle_semantic_receipt", replacement)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert invoked == [] and inner.active_invocations == () and proxy.receipts == []
    record = result._foreign_tasks._semantic_receipt
    assert record.semantic_steps == 1
    assert result._foreign_tasks.task_semantic_receipt(proxy, record.root_token, record.root_id) is record.receipt


def test_repeated_cumulative_snapshots_are_idempotent_and_new_ticks_advance_sequence(monkeypatch):
    result = runtime()
    result.evaluate(b": TWO 7 DROP ;")
    inner = adapter(result)
    proxy = SemanticReceiptAdapter(inner, result._foreign_tasks)
    export = capture(result, "TWO")
    operation = inner.register(signature(), (Callback(export=export, arguments=()), Return(outputs=())))
    word = result._foreign_tasks.define_operation("MACHINE", proxy, operation)
    account, observed = result._account_semantic_step, []

    def tick():
        account()
        root = result._foreign_tasks._task_root
        if root is not None and root.active:
            receipt = result._foreign_tasks._publish_semantic_receipt(root)
            assert result._foreign_tasks._publish_semantic_receipt(root) is receipt
            proxy.settle_semantic_receipt(receipt)
            observed.append(receipt)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    result.execute(word.xt)
    assert [item.sequence for item in observed] == list(range(1, len(observed) + 1))
    assert [item.semantic_steps for item in observed] == list(range(1, len(observed) + 1))
    assert proxy.receipts[-1] is observed[-1]
    assert proxy.total == result._foreign_tasks.last_dispatch.semantic_steps
