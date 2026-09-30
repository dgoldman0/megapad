"""Synchronous v2 callbacks retain one semantic owner and cumulative ledgers."""

from __future__ import annotations

from copy import copy

import pytest

pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridExecutionError, HybridRuntime
from shared.cells import MASK64
from shared.hybrid_abi import BufferRuleV1, CallbackExportV2, CallbackSiteV2, RoutineImageV1, RoutineImageV2
from simulator.errors import IllegalInstructionFault, StepBudgetExceeded
from simulator.interop_exports import CallbackExportError
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime, YieldedExecution


EXPORT_NAMES = ("MIN", "MAX", "ABS", "AND", "OR", "XOR")


def _export(name):
    return CallbackExportV2(export_id=EXPORT_NAMES.index(name), name=name,
                            input_cells=1 if name == "ABS" else 2, output_cells=1)


def _image(name="H-CALL", *, callback="MIN", buffered=False, max_instructions=100):
    export = _export(callback)
    prefix = "mov r13, r4\nst.b r13, r6\nmov r4, r6\nmov r5, r7" if buffered else ""
    suffix = "str r13, r4" if buffered else ""
    source = f"""
        {prefix}
        mov r12, r3
    after_pc:
        addi r12, 0
    call:
        call.l r12
        {suffix}
        ret.l
    stub:
        ret.l
    """
    labels = {}
    assemble(source, labels_out=labels)
    delta = labels["stub"] - labels["after_pc"]
    code = bytes(assemble(source.replace("addi r12, 0", f"addi r12, {delta}")))
    buffers = (BufferRuleV1(address_argument=0, length_argument=1,
                           element_bytes=1, max_bytes=4096, access="read_write"),) if buffered else ()
    return RoutineImageV2(
        name=name, code=code, entry_offset=0,
        input_cells=4 if buffered else export.input_cells, output_cells=1,
        buffers=buffers, max_instructions=max_instructions, return_stack_cells=16,
        callbacks=(CallbackSiteV2(call_offset=labels["call"], stub_offset=labels["stub"], export=export),),
    )


def _v1_image(name="H-V1"):
    return RoutineImageV1(name=name, code=bytes(assemble("inc r4\nret.l")),
                          entry_offset=0, input_cells=1, output_cells=1,
                          buffers=(), max_instructions=10, return_stack_cells=8)


@pytest.fixture(params=("python", "native"))
def backend(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return request.param


@pytest.fixture
def make_hybrid(backend):
    owners = []

    def create(**limits):
        memory = create_one_core_address_space(bank0_size=65536, external_size=65536,
                                               dense_backing=True)
        owner = HybridRuntime.create(executor=backend, memory=memory,
                                     dispatch_instruction_limit=1000, **limits)
        owners.append(owner)
        return owner

    yield create
    for owner in reversed(owners):
        owner.close()


@pytest.fixture
def hybrid(make_hybrid):
    return make_hybrid()


def _push(hybrid, *values):
    for value in values:
        hybrid.semantic.main_context.data.push(value)
    return hybrid.semantic.main_context


def _returns(hybrid):
    stack = hybrid.semantic.main_context.returns
    return (stack.pointer, stack.snapshot(), dict(stack._continuations),
            stack._continuation_cookie, stack.pointer_capture_checkpoint(),
            hybrid.semantic.memory.read_bytes(stack.floor, stack.empty_pointer - stack.floor))


def _caller(hybrid):
    stack = hybrid.semantic.main_context.data
    return (stack.pointer, stack.snapshot(),
            hybrid.semantic.memory.read_bytes(stack.floor, stack.empty_pointer - stack.floor),
            _returns(hybrid))


def _counters(hybrid):
    return (hybrid.machine_instructions, hybrid.machine_cycles, hybrid.transitions,
            hybrid.callback_requests, hybrid.callback_semantic_steps)


@pytest.mark.parametrize("capability", ("version", "runner"))
def test_missing_v2_capability_retains_v1_without_partial_callback_publication(make_hybrid, monkeypatch, capability):
    import _mp64_accel as native

    attribute, value = (("HYBRID_CALLBACK_ABI_VERSION", -1)
                        if capability == "version" else ("RoutineRunnerV2", None))
    with monkeypatch.context() as patch:
        patch.setattr(native, attribute, value)
        hybrid = make_hybrid()
    word = hybrid.register_routine_v1(_v1_image())
    _push(hybrid, 7)
    assert hybrid.execute_xt(word.xt).machine_instructions == 2
    assert hybrid.semantic.main_context.data.snapshot() == (8,)
    dictionary = hybrid.semantic.dictionary
    before = (dictionary.here, dictionary.latest, dictionary.words, hybrid._control_used)
    with pytest.raises(RuntimeError, match="matching _mp64_accel v2"):
        hybrid.register_routine_v2(_image())
    assert (dictionary.here, dictionary.latest, dictionary.words, hybrid._control_used) == before
    assert hybrid.semantic.find("H-CALL") is None
    assert hybrid.execute_xt(word.xt).machine_instructions == 2
    assert hybrid.semantic.main_context.data.snapshot() == (9,)


@pytest.mark.parametrize("callback,arguments,expected", (
    ("MIN", (MASK64, 7), MASK64), ("MAX", (MASK64, 7), 7),
    ("ABS", (MASK64,), 1), ("ABS", (1 << 63,), 1 << 63),
    ("AND", (0xAA, 0x3C), 0x28), ("OR", (0xAA, 0x3C), 0xBE),
    ("XOR", (0xAA, 0x3C), 0x96),
))
def test_canonical_callbacks_match_direct_semantic_words_and_keep_clock_domains_separate(hybrid, callback, arguments, expected):
    direct = MegaForthRuntime(execution_backend=hybrid.executor)
    for value in arguments:
        direct.main_context.data.push(value)
    direct_result = direct.execute(callback)
    word = hybrid.register_routine_v2(_image(callback=callback))
    context = _push(hybrid, 0xCAFE, *arguments)
    before_returns = _returns(hybrid)
    cycles = hybrid.semantic.diagnostics.semantic_cycles
    timer = hybrid.semantic.timer.counter

    report = hybrid.execute_xt(word.xt)

    assert direct.main_context.data.snapshot() == (expected,)
    assert context.data.snapshot() == (0xCAFE, expected)
    assert _returns(hybrid) == before_returns
    assert report.callback_requests == report.callback_semantic_steps == direct_result.semantic_steps == 1
    assert report.machine_instructions == 5 and report.transitions == 1
    assert report.machine_segments == 2
    assert report.machine_cycles >= report.machine_instructions
    assert report.semantic_result.semantic_steps == 2
    assert hybrid.semantic.diagnostics.semantic_cycles - cycles == 2
    assert hybrid.semantic.timer.counter - timer == 2
    assert _counters(hybrid) == (5, report.machine_cycles, 1, 1, 1)


def test_private_callback_context_never_temporarily_borrows_outer_data_or_return_storage(hybrid, monkeypatch):
    word = hybrid.register_routine_v2(_image())
    context = _push(hybrid, 0xCAFE, 7, 3)
    context.returns.push(0x1122)
    retained = context.returns.capture_pointer()
    context.returns.pop()
    before = _caller(hybrid)
    observations = []
    account = hybrid.semantic._account_semantic_step

    def observe():
        account()
        private = hybrid.semantic._callback_exports._active_context
        if private is not None:
            observations.append((_caller(hybrid), private))

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", observe)
    hybrid.execute_xt(word.xt)

    assert len(observations) == 1 and observations[0][0] == before
    private = observations[0][1]
    assert private is not context
    assert private.data.capacity == private.returns.capacity == 8
    assert private.data._memory is private.returns._memory
    assert private.data._memory is not hybrid.semantic.memory
    assert _returns(hybrid) == before[-1]
    context.returns.set_pointer(retained)
    assert context.returns.snapshot() == (0x1122,)


def test_interpreted_compiled_execute_and_v1_calls_use_one_registered_authority(hybrid):
    hybrid.register_routine_v2(_image(name="H-MIN"))
    hybrid.register_routine_v1(_v1_image())
    report = hybrid.evaluate(
        b"7 3 H-MIN : POLICY H-MIN ; 9 4 POLICY 8 2 ' H-MIN EXECUTE 10 H-V1"
    )
    assert hybrid.semantic.main_context.data.snapshot() == (3, 4, 2, 11)
    assert report.machine_instructions == 17
    assert report.transitions == 4
    assert report.callback_requests == report.callback_semantic_steps == 3


def test_reports_count_segment_deltas_once_and_later_calls_do_not_include_prior_work(hybrid):
    word = hybrid.register_routine_v2(_image())
    _push(hybrid, 7, 3)
    first = hybrid.execute_xt(word.xt)
    _push(hybrid, 8, 2)
    second = hybrid.execute_xt(word.xt)
    assert first.machine_instructions == second.machine_instructions == 5
    assert first.callback_requests == second.callback_requests == 1
    assert first.callback_semantic_steps == second.callback_semantic_steps == 1
    assert first.transitions == second.transitions == 1
    assert first.machine_segments == second.machine_segments == 2
    assert hybrid.machine_segments == 4
    assert _counters(hybrid) == (10, first.machine_cycles + second.machine_cycles, 2, 2, 2)


@pytest.mark.parametrize("limit,expected_machine,expected_requests,expected_semantic", (
    (3, 3, 1, 0), (4, 4, 1, 1), (5, 5, 1, 1),
))
def test_machine_allowance_includes_callback_call_and_real_return(hybrid, limit, expected_machine, expected_requests, expected_semantic):
    word = hybrid.register_routine_v2(_image())
    context = _push(hybrid, 7, 3)
    before = _caller(hybrid)
    if limit == 5:
        report = hybrid.execute_xt(word.xt, machine_instruction_limit=limit)
        assert report.machine_instructions == 5
        assert context.data.snapshot() == (3,)
    else:
        with pytest.raises(HybridExecutionError) as caught:
            hybrid.execute_xt(word.xt, machine_instruction_limit=limit)
        assert caught.value.reason == "instruction_limit"
        assert _caller(hybrid) == before
        if limit == 4:
            assert caught.value.result.invocation_instructions == 4
            assert caught.value.result.instructions == 1
    assert hybrid.machine_instructions == expected_machine
    assert hybrid.callback_requests == expected_requests
    assert hybrid.callback_semantic_steps == expected_semantic
    assert hybrid.transitions == 1


def test_outer_semantic_budget_failure_cancels_pending_machine_and_keeps_outer_inputs(hybrid):
    word = hybrid.register_routine_v2(_image())
    _push(hybrid, 7, 3)
    before = _caller(hybrid)
    with pytest.raises(StepBudgetExceeded):
        hybrid.execute_xt(word.xt, step_budget=1)
    assert _caller(hybrid) == before
    assert hybrid.machine_instructions == 3
    assert hybrid.callback_requests == 1 and hybrid.callback_semantic_steps == 0
    assert hybrid.execute_xt(word.xt, step_budget=2).machine_instructions == 5


@pytest.mark.parametrize("limit_name,reason,requests", (
    ("dispatch_callback_limit", "callback_limit", 1),
    ("dispatch_callback_semantic_limit", "callback_semantic_limit", 2),
))
@pytest.mark.parametrize("resume_surface", ("wrapper", "raw"))
def test_lowered_callback_ledgers_survive_a_quantum_before_first_machine_entry(make_hybrid, limit_name, reason, requests, resume_surface):
    hybrid = make_hybrid()
    hybrid.register_routine_v2(_image(name="H-ABS", callback="ABS"))
    hybrid.evaluate(b": TWICE 0 DROP H-ABS H-ABS ;")
    context = _push(hybrid, MASK64)
    first = hybrid.run("TWICE", quantum_steps=1, **{limit_name: 1})
    assert isinstance(first.semantic_result, YieldedExecution)
    assert first.machine_instructions == first.callback_requests == first.callback_semantic_steps == 0
    suspension = first.semantic_result.suspension
    with pytest.raises(ValueError):
        hybrid.resume_yielded(suspension, **{limit_name: 2})
    result = first.semantic_result
    with pytest.raises(HybridExecutionError) as caught:
        for _ in range(20):
            assert isinstance(result, YieldedExecution)
            if resume_surface == "wrapper":
                result = hybrid.resume_yielded(result.suspension).semantic_result
            else:
                result = hybrid.semantic.resume_yielded(result.suspension)
        pytest.fail("host quanta renewed the callback allowance")
    assert caught.value.reason == reason
    assert context.data.snapshot() == (1,)
    assert context.returns.snapshot() == () and not context.suspended
    assert hybrid.machine_instructions == 8
    assert hybrid.callback_requests == requests and hybrid.callback_semantic_steps == 1
    assert hybrid.execute("H-ABS").callback_semantic_steps == 1


@pytest.mark.parametrize("limit_name,reason,requests", (
    ("dispatch_callback_limit", "callback_limit", 1),
    ("dispatch_callback_semantic_limit", "callback_semantic_limit", 2),
))
def test_raw_semantic_dispatch_uses_configured_callback_ledger(make_hybrid, limit_name, reason, requests):
    hybrid = make_hybrid(**{limit_name: 1})
    hybrid.register_routine_v2(_image(name="H-ABS", callback="ABS"))
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.semantic.evaluate(b"-1 H-ABS -2 H-ABS")
    assert caught.value.reason == reason
    assert hybrid.semantic.main_context.data.snapshot() == (1, MASK64 - 1)
    assert hybrid.machine_instructions == 8
    assert hybrid.callback_requests == requests and hybrid.callback_semantic_steps == 1


@pytest.mark.parametrize("limit_name", ("dispatch_callback_limit", "dispatch_callback_semantic_limit"))
@pytest.mark.parametrize("invalid", (0, True, 1.5))
def test_lowered_callback_limit_validation_precedes_machine_entry(hybrid, limit_name, invalid):
    word = hybrid.register_routine_v2(_image())
    _push(hybrid, 7, 3)
    before = _caller(hybrid), _counters(hybrid)
    with pytest.raises((TypeError, ValueError)):
        hybrid.execute_xt(word.xt, **{limit_name: invalid})
    assert (_caller(hybrid), _counters(hybrid)) == before


def test_shadowing_canonical_names_does_not_rebind_existing_callback_exports(hybrid):
    original = hybrid.semantic.find("MIN")
    word = hybrid.register_routine_v2(_image())
    hybrid.evaluate(b": MIN 999 ; : COMPILED H-CALL ;")
    unexpected = []
    hybrid.semantic.define_primitive("MIN", lambda context: unexpected.append(context))
    _push(hybrid, 7, 3)
    assert hybrid.execute("COMPILED").callback_semantic_steps == 1
    assert hybrid.semantic.main_context.data.snapshot() == (3,)
    assert unexpected == [] and hybrid.semantic.dictionary.resolve(original.xt) is original
    assert hybrid.semantic.dictionary.resolve(word.xt) is word


def test_replaced_canonical_implementation_is_stale_authority_not_a_host_callback(hybrid):
    word = hybrid.register_routine_v2(_image())
    canonical = hybrid.semantic.find("MIN")
    implementation = canonical.implementation
    callback = implementation.callback
    unexpected = []
    _push(hybrid, 7, 3)
    before = _caller(hybrid)
    object.__setattr__(implementation, "callback", lambda context: unexpected.append(context))
    try:
        with pytest.raises(HybridExecutionError) as caught:
            hybrid.execute_xt(word.xt)
        assert caught.value.reason == "stale_export"
        assert isinstance(caught.value.__cause__, CallbackExportError)
        assert _caller(hybrid) == before
        assert hybrid.callback_semantic_steps == 0 and unexpected == []
    finally:
        object.__setattr__(implementation, "callback", callback)


def test_tick_guard_failure_counts_admitted_callback_step_and_preserves_buffer_prefix(hybrid, monkeypatch):
    word = hybrid.register_routine_v2(_image(buffered=True))
    canonical = hybrid.semantic.find("MIN").implementation
    original_callback = canonical.callback
    context = _push(hybrid, 0xCAFE, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    account = hybrid.semantic._account_semantic_step
    unexpected = []

    def substitute():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            object.__setattr__(canonical, "callback", lambda context: unexpected.append(context))

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", substitute)
    try:
        with pytest.raises(CallbackExportError):
            hybrid.execute_xt(word.xt)
        assert _caller(hybrid) == before
        assert context.data.snapshot() == (0xCAFE, EXTERNAL_BASE, 8, 7, 3)
        assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == b"\x07" + bytes(7)
        assert (hybrid.machine_instructions, hybrid.transitions,
                hybrid.callback_requests, hybrid.callback_semantic_steps) == (7, 1, 1, 1)
        assert unexpected == []
    finally:
        object.__setattr__(canonical, "callback", original_callback)


def test_callback_time_code_mutation_cancels_before_return_and_discards_outputs(hybrid, monkeypatch):
    word = hybrid.register_routine_v2(_image(buffered=True))
    declaration = hybrid.declaration_for(word)
    context = _push(hybrid, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    stub_address = declaration.code_base + declaration.callbacks[0].stub_offset
    # The main data allocation spans Bank 0's whole lower half, including
    # dictionary code below the active stack. Permit exactly this intentional
    # byte change while still checking every active and inactive stack byte.
    assert context.data.floor <= stub_address < context.data.pointer
    expected_backing = bytearray(before[2])
    stub_offset = stub_address - context.data.floor
    assert expected_backing[stub_offset] != 0x01
    expected_backing[stub_offset] = 0x01
    account = hybrid.semantic._account_semantic_step

    def mutate():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            hybrid.semantic.memory.write8(stub_address, 0x01)

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", mutate)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)
    assert caught.value.reason == "stale_code"
    assert _caller(hybrid) == (before[0], before[1], bytes(expected_backing), before[3])
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == b"\x07" + bytes(7)
    assert hybrid.machine_instructions == 7
    assert hybrid.callback_requests == hybrid.callback_semantic_steps == 1


@pytest.mark.parametrize("replacement", ("owner", "guard"))
@pytest.mark.parametrize("when", ("before_entry", "callback_tick"))
def test_dictionary_authority_replacement_cannot_enter_or_resume_machine(hybrid, monkeypatch, replacement, when):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v2(_image(buffered=True))
    dictionary = hybrid.semantic.dictionary
    _push(hybrid, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    account = hybrid.semantic._account_semantic_step
    replaced = []

    with monkeypatch.context() as patch:
        def replace_authority():
            replaced.append(True)
            if replacement == "owner":
                patch.setattr(hybrid.semantic, "dictionary", copy(dictionary))
            else:
                patch.setattr(dictionary, "_mutation_guard", lambda operation: None)

        def substitute():
            account()
            if hybrid.semantic._callback_exports._active_context is not None and not replaced:
                replace_authority()

        if when == "before_entry":
            replace_authority()
        else:
            patch.setattr(hybrid.semantic, "_account_semantic_step", substitute)
        with pytest.raises((HybridExecutionError, CallbackExportError)) as caught:
            hybrid.execute_xt(word.xt)
    if replacement == "owner" and when == "callback_tick":
        assert isinstance(caught.value, CallbackExportError)
        assert "dictionary owner" in str(caught.value)
    else:
        assert isinstance(caught.value, HybridExecutionError)
        assert caught.value.reason == "stale_registration"
    assert replaced == [True] and _caller(hybrid) == before
    expected = 7 if when == "callback_tick" else 0
    assert hybrid.machine_instructions == expected
    assert hybrid.callback_requests == hybrid.callback_semantic_steps == bool(expected)
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == bytes((expected,)) + bytes(7)
    assert hybrid.execute_xt(safe.xt).machine_instructions == 2


def test_rollback_and_identical_code_xt_reuse_never_revive_old_registration(hybrid):
    dictionary = hybrid.semantic.dictionary
    checkpoint = dictionary.checkpoint()
    old = hybrid.register_routine_v2(_image())
    old_declaration = hybrid.declaration_for(old)
    dictionary.rollback(checkpoint)
    replacement = hybrid.register_routine_v2(_image(callback="MAX"))
    new_declaration = hybrid.declaration_for(replacement)
    assert replacement.xt == old.xt and replacement is not old
    assert old_declaration.code == new_declaration.code
    assert not dictionary.is_body_lease_live(old_declaration.allocation_lease)
    _push(hybrid, 7, 3)
    assert hybrid.execute_xt(replacement.xt).callback_semantic_steps == 1
    assert hybrid.semantic.main_context.data.snapshot() == (7,)

    stale = hybrid.semantic.define_primitive("STALE-CLOSURE", old.implementation.callback)
    _push(hybrid, 7, 3)
    before = _caller(hybrid), _counters(hybrid)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(stale.xt)
    assert caught.value.reason == "stale_registration"
    assert (_caller(hybrid), _counters(hybrid)) == before


@pytest.mark.parametrize("action", ("machine", "register", "close", "dictionary"))
def test_parked_callback_forbids_nested_machine_and_publication_before_effects(hybrid, monkeypatch, action):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v2(_image())
    _push(hybrid, 7, 3)
    account = hybrid.semantic._account_semantic_step
    blocked = []
    attempted = []
    before_here = hybrid.semantic.dictionary.here

    def probe():
        account()
        if hybrid.semantic._callback_exports._active_context is None or attempted:
            return
        attempted.append(True)
        operations = {
            "machine": lambda: hybrid.execute_xt(safe.xt),
            "register": lambda: hybrid.register_routine_v1(_v1_image("NOT-PUBLISHED")),
            "close": hybrid.close,
            "dictionary": lambda: hybrid.semantic.dictionary.allot(1),
        }
        with pytest.raises((HybridExecutionError, CallbackExportError, RuntimeError)) as caught:
            operations[action]()
        blocked.append(caught.value)

    monkeypatch.setattr(hybrid.semantic, "_account_semantic_step", probe)
    report = hybrid.execute_xt(word.xt)
    assert len(blocked) == 1
    assert hybrid.semantic.dictionary.here == before_here
    assert hybrid.semantic.find("NOT-PUBLISHED") is None and not hybrid.closed
    assert hybrid.semantic.main_context.data.snapshot() == (3,)
    assert report.machine_instructions == 5 and report.transitions == 1


def test_host_baseexception_cancels_frame_preserves_original_cause_and_allows_later_entry(hybrid, monkeypatch):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v2(_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    original = KeyboardInterrupt("callback host interruption")
    cause = RuntimeError("original cause")
    original.__cause__ = cause
    account = hybrid.semantic._account_semantic_step

    def interrupt():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            raise original

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "_account_semantic_step", interrupt)
        with pytest.raises(KeyboardInterrupt) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is original and caught.value.__cause__ is cause
    assert _caller(hybrid) == before
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == b"\x07" + bytes(7)
    assert hybrid.callback_requests == hybrid.callback_semantic_steps == 1
    assert hybrid.execute_xt(safe.xt).machine_instructions == 2
    assert hybrid.semantic.main_context.data.snapshot() == (EXTERNAL_BASE, 8, 7, 4)


def test_callback_instruction_fault_escapes_outer_guest_fault_translation_unchanged(hybrid, monkeypatch):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v2(_image(buffered=True))
    guest_faults = []
    hybrid.semantic._fault_xt = hybrid.semantic.define_primitive(
        "FAULT-SPY", lambda context: guest_faults.append(context),
    ).xt
    _push(hybrid, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    original = IllegalInstructionFault("private callback instruction fault")
    account = hybrid.semantic._account_semantic_step

    def fail_inside_callback():
        account()
        if hybrid.semantic._callback_exports._active_context is not None:
            raise original

    with monkeypatch.context() as patch:
        patch.setattr(hybrid.semantic, "_account_semantic_step", fail_inside_callback)
        with pytest.raises(IllegalInstructionFault) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is original
    assert guest_faults == []
    assert hybrid.semantic.uart_output == b""
    assert _caller(hybrid) == before
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == b"\x07" + bytes(7)
    assert hybrid.machine_instructions == 7
    assert hybrid.callback_requests == hybrid.callback_semantic_steps == 1
    assert hybrid.execute_xt(safe.xt).machine_instructions == 2


class _RaiseAfterSegment:
    def __init__(self, runner, phase, failure, *, receipt_failure=None):
        self.runner, self.phase, self.failure = runner, phase, failure
        self.receipt_failure = receipt_failure
        self.results = []
        self.raised = False

    def _forward(self, phase, operation, *args, **kwargs):
        result = operation(*args, **kwargs)
        self.results.append(result)
        if phase == self.phase:
            self.raised = True
            raise self.failure
        return result

    def begin_v2(self, *args, **kwargs):
        return self._forward("begin", self.runner.begin_v2, *args, **kwargs)

    def resume_callback(self, *args, **kwargs):
        return self._forward("resume", self.runner.resume_callback, *args, **kwargs)

    def last_segment_v2(self):
        if self.raised and self.receipt_failure is not None:
            raise self.receipt_failure
        return self.runner.last_segment_v2()

    def __getattr__(self, name):
        return getattr(self.runner, name)


@pytest.mark.parametrize("phase", ("begin", "resume"))
@pytest.mark.parametrize("receipt_fails", (False, True))
@pytest.mark.parametrize("error_type", (RuntimeError, TypeError, ValueError))
def test_forwarded_native_work_settles_even_when_host_result_delivery_raises(hybrid, monkeypatch, phase, receipt_fails, error_type):
    safe = hybrid.register_routine_v1(_v1_image())
    word = hybrid.register_routine_v2(_image(buffered=True))
    _push(hybrid, EXTERNAL_BASE, 8, 7, 3)
    before = _caller(hybrid)
    original = error_type("host delivery failed after completed native work")
    original.__cause__ = cause = LookupError("original delivery cause")
    receipt_error = RuntimeError("native accounting receipt unavailable") if receipt_fails else None
    proxy = _RaiseAfterSegment(hybrid._runner, phase, original, receipt_failure=receipt_error)
    with monkeypatch.context() as patch:
        patch.setattr(hybrid, "_runner", proxy)
        with pytest.raises(error_type) as caught:
            hybrid.execute_xt(word.xt)
    assert caught.value is original and caught.value.__cause__ is cause
    assert _caller(hybrid) == before
    assert len(proxy.results) == (1 if phase == "begin" else 2)
    completed = proxy.results[-1]
    assert completed.invocation_instructions == (7 if phase == "begin" else 10)
    expected_buffer = (7 if phase == "begin" else 3).to_bytes(8, "little")
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == expected_buffer
    assert hybrid.callback_semantic_steps == (phase == "resume")
    # Cleanup must also work if result delivery failed after the final return,
    # when no pending frame remains and the preceding token is already spent.
    assert proxy.runner.cancel_invocation() is None
    if receipt_fails:
        counters = _counters(hybrid)
        with pytest.raises(HybridExecutionError, match="registration_cleanup"):
            hybrid.execute_xt(safe.xt)
        assert _counters(hybrid) == counters
        assert any("accounting receipt unavailable" in note for note in original.__notes__)
    else:
        assert _counters(hybrid) == (
            completed.invocation_instructions, completed.invocation_cycles, 1, 1, phase == "resume",
        )
        assert hybrid.machine_segments == len(proxy.results)
        assert hybrid.execute_xt(safe.xt).machine_instructions == 2
        assert hybrid.semantic.main_context.data.snapshot() == (EXTERNAL_BASE, 8, 7, 4)


class _FailV2Publication:
    def __init__(self, runner, failure, *, cleanup_failure=None):
        self.runner, self.failure, self.cleanup_failure = runner, failure, cleanup_failure
        self.specs = []

    def publish_code_v2(self, spec):
        self.specs.append(spec)
        self.runner.publish_code_v2(spec)
        raise self.failure

    def revoke_code_v2(self, spec):
        if self.cleanup_failure is not None:
            raise self.cleanup_failure
        return self.runner.revoke_code_v2(spec)

    def __getattr__(self, name):
        return getattr(self.runner, name)


@pytest.mark.parametrize("cleanup_fails", (False, True))
def test_failed_v2_publication_rolls_back_new_exports_and_native_authority(hybrid, monkeypatch, cleanup_fails):
    existing = hybrid.register_routine_v2(_image(name="EXISTING"))
    semantic = hybrid.semantic
    assert semantic.configure_dictionary_index(EXTERNAL_BASE, 2048) == 0
    dictionary, index = semantic.dictionary, semantic.dictionary_index
    engine = semantic._callback_exports
    before = (dictionary.here, dictionary.latest, dictionary.words, index.state,
              semantic.memory.read_bytes(EXTERNAL_BASE, 2048 * 16), hybrid._control_used,
              tuple(engine._exports))
    failure = RuntimeError("publication failed after native ownership")
    cleanup = RuntimeError("native revocation failed") if cleanup_fails else None
    proxy = _FailV2Publication(hybrid._runner, failure, cleanup_failure=cleanup)
    with monkeypatch.context() as patch:
        patch.setattr(hybrid, "_runner", proxy)
        with pytest.raises(RuntimeError) as caught:
            hybrid.register_routine_v2(_image(name="FAILED", callback="MAX"))
    assert caught.value is failure
    assert semantic.find("FAILED") is None
    assert (dictionary.here, dictionary.latest, dictionary.words, index.state,
            semantic.memory.read_bytes(EXTERNAL_BASE, 2048 * 16), hybrid._control_used,
            tuple(engine._exports)) == before
    assert len(proxy.specs) == 1
    if cleanup_fails:
        _push(hybrid, 7, 3)
        with pytest.raises(HybridExecutionError, match="registration_cleanup"):
            hybrid.execute_xt(existing.xt)
    else:
        assert not hybrid._runner.is_code_published_v2(proxy.specs[0])
        _push(hybrid, 7, 3)
        assert hybrid.execute_xt(existing.xt).callback_semantic_steps == 1
        replacement = hybrid.register_routine_v2(_image(name="FAILED", callback="MAX"))
        _push(hybrid, 7, 3)
        assert hybrid.execute_xt(replacement.xt).callback_semantic_steps == 1
        assert semantic.main_context.data.snapshot() == (3, 7)
