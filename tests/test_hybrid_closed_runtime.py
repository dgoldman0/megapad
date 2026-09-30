"""Closed policy callbacks retain exact outer work and native V2 ownership."""

from __future__ import annotations

from dataclasses import replace

import pytest

pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridExecutionError, HybridRuntime
from shared.cells import MASK64
from shared.hybrid_abi import (
    BufferRuleV1, CallbackExportV3, CallbackSiteV3, RoutineDeclarationV3,
    RoutineImageV3,
)
from simulator.errors import StepBudgetExceeded
from simulator.interop_exports import (
    CallbackExportBudgetExceeded, CallbackExportError, CallbackExportResult,
)
from simulator.ir import Branch, BranchZero, Call, Literal, Return
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.test_hybrid_callbacks import _caller, _push, _returns, _v1_image


def _closed_image(*, name="H-CLAMP", policy="CLAMP", input_cells=3,
                  output_cells=1, max_semantic_steps=7, buffered=False,
                  effect="closed_integer_colon", export_id=17):
    prefix = ("mov r13, r4\nst.b r13, r6\nmov r4, r6\nmov r5, r7\nmov r6, r8"
              if buffered else "")
    source = f"""
        {prefix}
        mov r12, r3
    after_pc:
        addi r12, 0
    call:
        call.l r12
        ret.l
    stub:
        ret.l
    """
    labels = {}
    assemble(source, labels_out=labels)
    delta = labels["stub"] - labels["after_pc"]
    code = bytes(assemble(source.replace("addi r12, 0", f"addi r12, {delta}")))
    export = CallbackExportV3(
        export_id=export_id, name=policy, input_cells=input_cells,
        output_cells=output_cells, max_semantic_steps=max_semantic_steps, effect=effect,
    )
    return RoutineImageV3(
        name=name, code=code, entry_offset=0,
        input_cells=input_cells + (2 if buffered else 0), output_cells=output_cells,
        buffers=((BufferRuleV1(address_argument=0, length_argument=1, element_bytes=1,
                              max_bytes=4096, access="read_write"),) if buffered else ()),
        max_instructions=100, return_stack_cells=16,
        callbacks=(CallbackSiteV3(call_offset=labels["call"],
                                  stub_offset=labels["stub"], export=export),),
    )


@pytest.fixture(params=("python", "native"))
def hybrid(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    owner = HybridRuntime.create(
        executor=request.param,
        memory=create_one_core_address_space(bank0_size=65536, external_size=65536,
                                             dense_backing=True),
        dispatch_instruction_limit=1000,
    )
    owner.semantic.evaluate(b": CLAMP ROT MIN MAX ;")
    yield owner
    owner.close()


@pytest.mark.parametrize("arguments,expected", (
    ((9, 2, 7), 7), ((1, 2, 7), 2), ((5, 2, 7), 5),
    ((MASK64, MASK64 - 3, 7), MASK64),
))
def test_closed_clamp_matches_reference_with_exact_clock_domains(hybrid, arguments, expected):
    reference = MegaForthRuntime(execution_backend="python")
    reference.evaluate(b": CLAMP ROT MIN MAX ;")
    for value in arguments:
        reference.main_context.data.push(value)
    direct = reference.execute("CLAMP")
    word = hybrid.register_routine_v3(_closed_image())
    _push(hybrid, 0xCAFE, *arguments)
    returns = _returns(hybrid)
    semantic_cycles = hybrid.semantic.diagnostics.semantic_cycles
    timer = hybrid.semantic.timer.counter

    report = hybrid.execute_xt(word.xt)

    assert reference.main_context.data.snapshot() == (expected,)
    assert hybrid.semantic.main_context.data.snapshot() == (0xCAFE, expected)
    assert _returns(hybrid) == returns
    assert (report.machine_instructions, report.machine_cycles, report.transitions,
            report.machine_segments, report.callback_requests) == (5, 8, 1, 2, 1)
    assert report.callback_semantic_steps == direct.semantic_steps == 7
    assert report.semantic_result.semantic_steps == 8
    assert hybrid.semantic.diagnostics.semantic_cycles - semantic_cycles == 8
    assert hybrid.semantic.timer.counter - timer == 8
    assert type(hybrid.declaration_for(word)) is RoutineDeclarationV3
    assert type(hybrid.registered_routines[-1]) is RoutineImageV3
    assert hybrid.closed_callback_abi_available


def test_static_helper_charges_actual_path_in_original_meter(hybrid):
    helper = hybrid.semantic.define_colon("CLAMP-HELPER", (
        Call(hybrid.semantic.find("ROT").xt), Call(hybrid.semantic.find("MIN").xt),
        Call(hybrid.semantic.find("MAX").xt), Return(),
    ))
    hybrid.semantic.define_colon("CLAMP-OUTER", (Call(helper.xt), Return()))
    word = hybrid.register_routine_v3(_closed_image(policy="CLAMP-OUTER", max_semantic_steps=9))
    _push(hybrid, 9, 2, 7)
    report = hybrid.execute_xt(word.xt)
    assert hybrid.semantic.main_context.data.snapshot() == (7,)
    assert report.callback_semantic_steps == 9
    assert report.semantic_result.semantic_steps == 10


def test_forward_branch_uses_actual_remaining_work_not_proved_worst_case(hybrid):
    hybrid.semantic.define_colon("CHOOSE", (
        BranchZero(3), Literal(11), Branch(4), Literal(22), Return(),
    ))
    word = hybrid.register_routine_v3(_closed_image(
        policy="CHOOSE", input_cells=1, max_semantic_steps=4,
    ))
    _push(hybrid, 0)
    report = hybrid.execute_xt(word.xt, dispatch_callback_semantic_limit=3)
    assert report.callback_semantic_steps == 3
    assert hybrid.semantic.main_context.data.snapshot() == (22,)
    _push(hybrid, 1)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt, dispatch_callback_semantic_limit=3)
    assert caught.value.reason == "callback_semantic_limit"
    assert hybrid.semantic.main_context.data.snapshot() == (22, 1)


@pytest.mark.parametrize("remaining", (1, 3, 6))
def test_cumulative_limit_stops_inside_closed_callback_keeps_store_and_inputs(hybrid, remaining):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, 0xCAFE, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt, dispatch_callback_semantic_limit=remaining)

    assert caught.value.reason == "callback_semantic_limit"
    assert isinstance(caught.value.__cause__, CallbackExportBudgetExceeded)
    assert caught.value.result.version == 3
    assert _caller(hybrid) == before
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == b"\x09" + bytes(7)
    assert hybrid.callback_semantic_steps == remaining
    assert hybrid.machine_instructions == 8
    assert hybrid.machine_segments == hybrid.transitions == hybrid.callback_requests == 1
    # Cancellation released native ownership; an independent root may run.
    assert hybrid.execute_xt(word.xt).callback_semantic_steps == 7


def test_raw_source_tokens_share_closed_callback_allowance(hybrid):
    hybrid.register_routine_v3(_closed_image())
    hybrid._dispatch_callback_semantic_limit = 9
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.semantic.evaluate(b"9 2 7 H-CLAMP 1 2 7 H-CLAMP")
    assert caught.value.reason == "callback_semantic_limit"
    assert hybrid.callback_semantic_steps == 9
    assert hybrid.machine_instructions == 8
    assert hybrid.callback_requests == hybrid.transitions == 2
    assert hybrid.semantic.main_context.data.snapshot() == (7, 1, 2, 7)


def test_host_semantic_budget_is_not_replaced_by_closed_callback_error(hybrid):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    with pytest.raises(StepBudgetExceeded):
        hybrid.execute_xt(word.xt, step_budget=4)
    assert _caller(hybrid) == before
    assert hybrid.callback_semantic_steps == 3
    assert hybrid.machine_instructions == 8
    assert hybrid.semantic.memory.read8(EXTERNAL_BASE) == 9
    assert hybrid.execute_xt(word.xt).machine_instructions == 10


@pytest.mark.parametrize("error_type", (RuntimeError, CallbackExportError, CallbackExportBudgetExceeded))
def test_host_accounting_exception_retains_exact_identity_and_completed_prefix(hybrid, monkeypatch, error_type):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    error = (CallbackExportBudgetExceeded("callback_semantic_limit", 7, 7)
             if error_type is CallbackExportBudgetExceeded else error_type("host hook failed"))
    account = hybrid.semantic._account_semantic_step

    def fail():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            raise error

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "_account_semantic_step", fail)
        with pytest.raises(error_type) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is error
    assert _caller(hybrid) == before
    assert hybrid.callback_semantic_steps == 1
    assert hybrid.machine_instructions == 8
    assert hybrid.semantic.memory.read8(EXTERNAL_BASE) == 9
    assert hybrid.execute_xt(word.xt).callback_semantic_steps == 7


def test_closed_binding_survives_shadowing_but_rejects_exact_ir_mutation(hybrid):
    original = hybrid.semantic.find("CLAMP")
    word = hybrid.register_routine_v3(_closed_image())
    hybrid.semantic.evaluate(b": CLAMP DROP DROP DROP 99 ;")
    _push(hybrid, 9, 2, 7)
    assert hybrid.execute_xt(word.xt).callback_semantic_steps == 7
    assert hybrid.semantic.main_context.data.pop() == 7
    operation = original.implementation.operations[0]
    previous = operation.xt
    object.__setattr__(operation, "xt", hybrid.semantic.find("DROP").xt)
    _push(hybrid, 9, 2, 7)
    before = _caller(hybrid), hybrid.machine_instructions
    try:
        with pytest.raises(HybridExecutionError) as caught:
            hybrid.execute_xt(word.xt)
        assert caught.value.reason == "stale_export"
        assert (_caller(hybrid), hybrid.machine_instructions) == before
    finally:
        object.__setattr__(operation, "xt", previous)


def test_late_ir_substitution_is_rejected_after_tick_before_effect(hybrid, monkeypatch):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    policy = hybrid.semantic.find("CLAMP")
    operation = policy.implementation.operations[0]
    previous = operation.xt
    account = hybrid.semantic._account_semantic_step
    seen = []

    def mutate():
        account()
        if hybrid.semantic._callback_exports._active_context is not None and not seen:
            seen.append(True)
            object.__setattr__(operation, "xt", hybrid.semantic.find("DROP").xt)

    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", mutate)
    try:
        with pytest.raises(CallbackExportError):
            hybrid.execute_xt(word.xt)
        assert _caller(hybrid) == before
        assert hybrid.callback_semantic_steps == 1
        assert hybrid.machine_instructions == 8
        assert hybrid.semantic.memory.read8(EXTERNAL_BASE) == 9
    finally:
        object.__setattr__(operation, "xt", previous)


@pytest.mark.parametrize("action", ("source", "machine", "dictionary"))
def test_closed_policy_hooks_cannot_expand_profile(hybrid, monkeypatch, action):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v3(_closed_image())
    account = hybrid.semantic._account_semantic_step
    rejected = []

    def probe():
        account()
        if hybrid.semantic._callback_exports._active_context is None or rejected:
            return
        operations = {
            "source": lambda: hybrid.semantic.evaluate(b"999"),
            "machine": lambda: hybrid.semantic.execute(safe.xt),
            "dictionary": lambda: hybrid.semantic.dictionary.allot(1),
        }
        with pytest.raises((CallbackExportError, HybridExecutionError, RuntimeError)) as caught:
            operations[action]()
        rejected.append(caught.value)

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", probe)
    _push(hybrid, 9, 2, 7)
    report = hybrid.execute_xt(word.xt)
    assert len(rejected) == 1
    assert hybrid.semantic.main_context.data.snapshot() == (7,)
    assert report.callback_semantic_steps == 7 and report.machine_instructions == 5


def test_v3_leaf_and_callback_free_images_use_existing_transport(hybrid):
    leaf = hybrid.register_routine_v3(_closed_image(
        name="H-MAX", policy="MAX", input_cells=2, effect="integer_leaf", max_semantic_steps=1,
    ))
    plain = hybrid.register_routine_v3(RoutineImageV3(
        name="H-PLAIN", code=bytes(assemble("inc r4\nret.l")), entry_offset=0,
        input_cells=1, output_cells=1, buffers=(), max_instructions=10,
        return_stack_cells=8, callbacks=(),
    ))
    _push(hybrid, 9, 2)
    assert hybrid.execute_xt(leaf.xt).callback_semantic_steps == 1
    assert hybrid.execute_xt(plain.xt).callback_requests == 0
    assert hybrid.semantic.main_context.data.snapshot() == (10,)


def test_failed_closed_binding_publishes_no_word_control_lease_or_native_code(hybrid):
    image = _closed_image(max_semantic_steps=6)
    dictionary = hybrid.semantic.dictionary
    before = (dictionary.here, dictionary.latest, dictionary.words,
              hybrid._control_used, hybrid._issued_code_bytes, hybrid.registered_routines)
    with pytest.raises((CallbackExportError, ValueError)):
        hybrid.register_routine_v3(image)
    assert (dictionary.here, dictionary.latest, dictionary.words,
            hybrid._control_used, hybrid._issued_code_bytes, hybrid.registered_routines) == before
    assert hybrid.semantic.find(image.name) is None
    valid = hybrid.register_routine_v3(replace(image, callbacks=(replace(
        image.callbacks[0], export=replace(image.callbacks[0].export, max_semantic_steps=7),
    ),)))
    _push(hybrid, 9, 2, 7)
    assert hybrid.execute_xt(valid.xt).machine_instructions == 5


def test_v3_publication_failure_revokes_native_code_and_new_export_capture(hybrid, monkeypatch):
    runner = hybrid._runner
    error = RuntimeError("after native publication")
    published = []

    class ForwardThenFail:
        def __getattr__(self, name):
            return getattr(runner, name)

        def publish_code_v2(self, spec):
            runner.publish_code_v2(spec)
            published.append(spec)
            raise error

    dictionary = hybrid.semantic.dictionary
    exports = dict(hybrid.semantic._callback_exports._exports)
    before = (dictionary.here, dictionary.latest, dictionary.words,
              hybrid._control_used, hybrid._issued_code_bytes)
    with monkeypatch.context() as patch:
        patch.setattr(hybrid, "_runner", ForwardThenFail())
        with pytest.raises(RuntimeError) as caught:
            hybrid.register_routine_v3(_closed_image())
    assert caught.value is error
    assert len(published) == 1 and not runner.is_code_published_v2(published[0])
    assert (dictionary.here, dictionary.latest, dictionary.words,
            hybrid._control_used, hybrid._issued_code_bytes) == before
    assert hybrid.semantic._callback_exports._exports == exports
    assert hybrid.registered_routines == ()
    word = hybrid.register_routine_v3(_closed_image())
    _push(hybrid, 9, 2, 7)
    assert hybrid.execute_xt(word.xt).callback_semantic_steps == 7


@pytest.mark.parametrize("mutation", ("reset", "inflate", "foreign"))
@pytest.mark.parametrize("host_failure", (False, True))
def test_closed_tick_receipt_survives_meter_mutation_without_arithmetic_hooks(
    hybrid, monkeypatch, mutation, host_failure,
):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    account = hybrid.semantic._account_semantic_step
    arithmetic = []
    observed = []
    original = RuntimeError("host hook failed after replacing meter steps")

    class ForeignSteps:
        def __sub__(self, other):
            arithmetic.append(other)
            raise AssertionError("forged meter arithmetic must not execute")

    def mutate():
        account()
        active = hybrid.semantic._callback_exports._active
        if active is None or observed:
            return
        meter = active.meter
        observed.append((meter, meter.steps))
        meter.steps = {"reset": 0, "inflate": 100000, "foreign": ForeignSteps()}[mutation]
        if host_failure:
            raise original

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "_account_semantic_step", mutate)
        with pytest.raises(RuntimeError if host_failure else CallbackExportError) as caught:
            hybrid.execute_xt(word.xt)
    if host_failure:
        assert caught.value is original
    assert arithmetic == []
    assert len(observed) == 1
    meter, expected_steps = observed[0]
    assert type(meter.steps) is int and meter.steps == expected_steps == 2
    assert hybrid._allowances[meter].callback_semantic_steps == hybrid.callback_semantic_steps == 1
    assert _caller(hybrid) == before
    assert hybrid.machine_instructions == 8 and hybrid.callback_requests == 1
    assert hybrid.semantic.memory.read8(EXTERNAL_BASE) == 9
    assert hybrid.semantic._callback_exports._closed_accounting is None
    assert hybrid.semantic._callback_exports._registration_failure is not None


def test_closed_receipts_settle_each_callback_once_after_an_earlier_success(hybrid, monkeypatch):
    hybrid.register_routine_v3(_closed_image())
    hybrid.semantic.evaluate(b": TWICE 9 2 7 H-CLAMP DROP 9 2 7 H-CLAMP ;")
    account = hybrid.semantic._account_semantic_step
    observed = []

    def mutate_second():
        account()
        active = hybrid.semantic._callback_exports._active
        if active is not None and hybrid.callback_requests == 2 and not observed:
            observed.append((active.meter, active.meter.steps))
            active.meter.steps = 0

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", mutate_second)
    with pytest.raises(CallbackExportError):
        hybrid.execute("TWICE", dispatch_callback_semantic_limit=9)
    assert len(observed) == 1
    meter, expected_steps = observed[0]
    assert meter.steps == expected_steps
    assert hybrid._allowances[meter].callback_semantic_steps == 8
    assert hybrid.callback_semantic_steps == 8
    assert hybrid.callback_requests == hybrid.transitions == 2
    assert hybrid.machine_instructions == 8
    assert hybrid.semantic.main_context.data.snapshot() == (9, 2, 7)


@pytest.mark.parametrize("when", ("before", "after"))
@pytest.mark.parametrize("replace_meter", (False, True))
def test_closed_invocation_wrapper_failure_preserves_exact_work_and_original_error(
    hybrid, monkeypatch, when, replace_meter,
):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    invoke = hybrid.semantic.invoke_callback_export
    original = RuntimeError("forwarding callback wrapper failed")
    observed = []
    arithmetic = []

    class ForeignSteps:
        def __sub__(self, other):
            arithmetic.append(other)
            raise AssertionError("forged meter arithmetic must not execute")

    def forward(handle, arguments, **kwargs):
        if when == "after":
            invoke(handle, arguments, **kwargs)
        meter = hybrid._current_meter()
        observed.append((meter, meter.steps))
        if replace_meter:
            meter.steps = ForeignSteps()
        raise original

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "invoke_callback_export", forward)
        with pytest.raises(RuntimeError) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is original
    assert arithmetic == []
    assert len(observed) == 1 and observed[0][0].steps == observed[0][1]
    expected = 7 if when == "after" else 0
    assert hybrid.callback_semantic_steps == expected
    assert hybrid._allowances[observed[0][0]].callback_semantic_steps == expected
    assert hybrid.machine_instructions == 8
    assert _caller(hybrid) == before
    assert hybrid.semantic.memory.read8(EXTERNAL_BASE) == 9
    assert hybrid.semantic._callback_exports._closed_accounting is None


def test_closed_wrapper_cannot_report_success_without_real_completed_invocation(hybrid, monkeypatch):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    monkeypatch.setattr(hybrid.semantic, "invoke_callback_export",
                        lambda *_args, **_kwargs: CallbackExportResult((7,), 7))
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)
    assert caught.value.reason == "callback_accounting"
    assert hybrid.callback_semantic_steps == 0
    assert hybrid.machine_instructions == 8
    assert _caller(hybrid) == before
    assert hybrid._registration_failure is not None
    assert hybrid.semantic._callback_exports._closed_accounting is None


def test_closed_wrapper_cannot_turn_incomplete_work_into_success(hybrid, monkeypatch):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    invoke = hybrid.semantic.invoke_callback_export
    account = hybrid.semantic._account_semantic_step
    original = RuntimeError("partial callback")

    def fail():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            raise original

    def swallow(handle, arguments, **kwargs):
        try:
            return invoke(handle, arguments, **kwargs)
        except RuntimeError as error:
            assert error is original
            return CallbackExportResult((7,), 1)

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", fail)
    monkeypatch.setattr(hybrid.semantic, "invoke_callback_export", swallow)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)
    assert caught.value.reason == "callback_accounting"
    assert hybrid.callback_semantic_steps == 1
    assert hybrid.machine_instructions == 8
    assert _caller(hybrid) == before
    assert hybrid._registration_failure is not None


def test_closed_checkpoint_rejects_two_real_invocations_before_second_work(hybrid, monkeypatch):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    invoke = hybrid.semantic.invoke_callback_export

    def twice(handle, arguments, **kwargs):
        invoke(handle, arguments, **kwargs)
        return invoke(handle, arguments, **kwargs)

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "invoke_callback_export", twice)
        with pytest.raises(CallbackExportError, match="one exact callback invocation"):
            hybrid.execute_xt(word.xt)
    assert hybrid.callback_semantic_steps == 7
    assert hybrid.machine_instructions == 8
    assert _caller(hybrid) == before
    assert hybrid.semantic._callback_exports._closed_accounting is None
    assert hybrid.execute_xt(word.xt).callback_semantic_steps == 7


def test_missing_closed_receipt_fail_closes_bridge_without_masking_original_error(hybrid, monkeypatch):
    word = hybrid.register_routine_v3(_closed_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 9, 2, 7)
    before = _caller(hybrid)
    invoke = hybrid.semantic.invoke_callback_export
    original = RuntimeError("host failure after losing receipt ownership")

    def lose_receipt(handle, arguments, **kwargs):
        invoke(handle, arguments, **kwargs)
        hybrid.semantic._callback_exports._closed_accounting = None
        raise original

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "invoke_callback_export", lose_receipt)
        with pytest.raises(RuntimeError) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is original
    assert hybrid._registration_failure is not None
    assert any("accounting checkpoint" in note for note in original.__notes__)
    assert _caller(hybrid) == before
    assert hybrid.machine_instructions == 8
    with pytest.raises(HybridExecutionError, match="registration_cleanup"):
        hybrid.execute_xt(word.xt)
