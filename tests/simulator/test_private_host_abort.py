"""Trusted private host escapes retain identity across ordinary source guards."""

import pytest

from simulator.errors import ForthAbort
from simulator.ir import Call, Return
from simulator.runtime import ExecutionContext, MegaForthRuntime


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return MegaForthRuntime(execution_backend=request.param)


def seeded(context):
    context.data.push(77)
    context.returns.push(19)
    return context


def trusted(runtime, error, name="TRUSTED-HOST-ESCAPE"):
    def fail(context):
        context.returns.push(91)
        raise error
    word = runtime.define_primitive(name, fail)
    runtime._register_primitive_host_escape(word.implementation, fail)
    return word


def assert_preserved(runtime, context, error, origin):
    assert error.origin_context is origin
    assert context.data.snapshot() == (77,)
    assert context.returns.snapshot() == (19,)
    assert context.reusable
    assert runtime._private_host_abort._error is None


@pytest.mark.parametrize("entry", ("execute", "evaluate", "resume"))
@pytest.mark.parametrize("origin_kind", ("none", "caller", "other"))
def test_trusted_abort_uses_host_cleanup_without_binding_or_clearing(runtime, entry, origin_kind):
    context = seeded(runtime.main_context)
    origin = None if origin_kind == "none" else context if origin_kind == "caller" else object()
    error = ForthAbort("host boundary", origin_context=origin)
    word = trusted(runtime, error)
    if entry == "resume":
        noop = runtime.define_primitive("NOOP", lambda context: None)
        outer = runtime.define_colon("RUN", (Call(noop.xt), Call(word.xt), Return()))
        report = runtime.run_until_blocked(outer.xt, quantum_steps=1)
        assert report.suspension is not None
        invoke = lambda: runtime.resume_yielded(report.suspension)
    elif entry == "evaluate":
        invoke = lambda: runtime.evaluate(b"TRUSTED-HOST-ESCAPE")
    else:
        outer = runtime.define_colon("RUN", (Call(word.xt), Return()))
        invoke = lambda: runtime.execute(outer.xt)
    with pytest.raises(ForthAbort) as caught:
        invoke()
    assert caught.value is error
    assert_preserved(runtime, context, error, origin)


@pytest.mark.parametrize("outer_entry", ("execute", "evaluate", "resume"))
@pytest.mark.parametrize("inner_entry", ("execute", "evaluate"))
@pytest.mark.parametrize("different_context", (False, True))
def test_automatic_nested_propagation_preserves_each_context(runtime, outer_entry, inner_entry, different_context):
    context = seeded(runtime.main_context)
    inner = seeded(ExecutionContext()) if different_context else context
    error = ForthAbort("nested host escape")
    word = trusted(runtime, error)

    def forward(active):
        try:
            if inner_entry == "evaluate":
                runtime.evaluate(b"TRUSTED-HOST-ESCAPE", context=inner)
            else:
                runtime.execute(word.xt, context=inner)
        except ForthAbort:
            raise
    outer = runtime.define_primitive("FORWARD", forward)
    if outer_entry == "resume":
        noop = runtime.define_primitive("NOOP", lambda context: None)
        outer = runtime.define_colon("RUN", (Call(noop.xt), Call(outer.xt), Return()))
        report = runtime.run_until_blocked(outer.xt, quantum_steps=1)
        invoke = lambda: runtime.resume_yielded(report.suspension)
    elif outer_entry == "evaluate":
        invoke = lambda: runtime.evaluate(b"FORWARD")
    else:
        invoke = lambda: runtime.execute(outer.xt)
    with pytest.raises(ForthAbort) as caught:
        invoke()
    assert caught.value is error
    assert_preserved(runtime, context, error, None)
    if different_context:
        assert_preserved(runtime, inner, error, None)


@pytest.mark.parametrize("reuse", ("explicit_raise", "changed_traceback", "reentrant", "new_entry_then_bare"))
def test_caught_error_cannot_reuse_escape_authority(runtime, reuse):
    context = seeded(runtime.main_context)
    error = ForthAbort("reused")
    word = trusted(runtime, error)
    def ordinary(active):
        raise error
    ordinary_word = runtime.define_primitive("ORDINARY", ordinary)
    noop = runtime.define_primitive("NOOP", lambda context: None)

    def catch_and_reuse(active):
        try:
            runtime.execute(word.xt)
        except ForthAbort as caught:
            assert caught is error and error.origin_context is None
            assert context.data.snapshot() == (77,)
            if reuse == "explicit_raise":
                raise caught
            if reuse == "changed_traceback":
                BaseException.__dict__["__traceback__"].__set__(caught, None)
                raise
            if reuse == "reentrant":
                runtime.execute(ordinary_word.xt)
            runtime.execute(noop.xt)
            raise
    outer = runtime.define_primitive("CATCH-AND-REUSE", catch_and_reuse)
    with pytest.raises(ForthAbort) as caught:
        runtime.execute(outer.xt)
    assert caught.value is error
    assert error.origin_context is context
    assert context.data.snapshot() == context.returns.snapshot() == ()
    assert runtime._private_host_abort._error is None


def test_final_scope_retires_authority_before_later_reuse(runtime):
    context = seeded(runtime.main_context)
    error = ForthAbort("later reuse")
    word = trusted(runtime, error)
    with pytest.raises(ForthAbort):
        runtime.execute(word.xt)
    assert_preserved(runtime, context, error, None)
    def ordinary(active):
        raise error
    outer = runtime.define_primitive("ORDINARY", ordinary)
    with pytest.raises(ForthAbort) as caught:
        runtime.evaluate(b"ORDINARY")
    assert caught.value is error
    assert error.origin_context is context
    assert context.data.snapshot() == context.returns.snapshot() == ()


def test_caught_then_normal_return_leaves_no_authority(runtime):
    context = seeded(runtime.main_context)
    error = ForthAbort("caught")
    word = trusted(runtime, error)
    def swallow(active):
        try:
            runtime.execute(word.xt)
        except ForthAbort:
            pass
    outer = runtime.define_primitive("SWALLOW", swallow)
    runtime.execute(outer.xt)
    assert_preserved(runtime, context, error, None)


def test_provenance_uses_original_traceback_descriptor(runtime):
    class HostAbort(ForthAbort):
        def __getattribute__(self, name):
            if name == "__traceback__":
                raise AssertionError("custom exception traceback getter ran")
            return super().__getattribute__(name)
        def bind_origin(self, context):
            raise AssertionError("trusted host error was bound as guest ABORT")
    context = seeded(runtime.main_context)
    error = HostAbort("custom host exception")
    word = trusted(runtime, error)
    try:
        runtime.execute(word.xt)
    except HostAbort as caught:
        assert caught is error
    else:
        pytest.fail("host exception did not escape")
    assert_preserved(runtime, context, error, None)


def test_private_leaf_tick_preserves_raw_origin_before_outer_machine_boundary(runtime, monkeypatch):
    from shared.hybrid_abi import CallbackExportV2

    context = seeded(runtime.main_context)
    handle = runtime.bind_callback_export(CallbackExportV2(
        export_id=0, name="ABS", input_cells=1, output_cells=1,
    ))
    error = ForthAbort("private leaf host tick")
    account = runtime._account_semantic_step
    def fail():
        account()
        if runtime._callback_exports._active_context is not None:
            raise error
    monkeypatch.setattr(runtime, "_account_semantic_step", fail)
    with pytest.raises(ForthAbort) as caught:
        runtime.invoke_callback_export(handle, (7,))
    assert caught.value is error
    assert_preserved(runtime, context, error, None)
    assert runtime._private_host_abort._leaf_scope is None


@pytest.mark.parametrize("source", (b"ABORT", b"ORDINARY"))
def test_unregistered_abort_still_clears_the_guest_task(runtime, source):
    context = seeded(runtime.main_context)
    error = ForthAbort("ordinary")
    def ordinary(active):
        raise error
    runtime.define_primitive("ORDINARY", ordinary)
    with pytest.raises(ForthAbort) as caught:
        runtime.evaluate(source)
    if source == b"ORDINARY":
        assert caught.value is error
    assert caught.value.origin_context is context
    assert context.data.snapshot() == context.returns.snapshot() == ()
    assert runtime._private_host_abort._error is None
