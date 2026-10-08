"""Compiled scalar FP calls preserve the hosted service and dispatch boundary."""

from __future__ import annotations

import pytest


pytest.importorskip("_megaforth_native")

from shared import scalar_fp  # noqa: E402
from shared.cells import MASK64  # noqa: E402
from simulator.errors import ForthAbort, StepBudgetExceeded  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime, YieldedExecution  # noqa: E402
from simulator.scalar_float import HostedScalarFloatService  # noqa: E402
from simulator.stacks import DataStack, StackOverflow, StackUnderflow  # noqa: E402


ONE = 0x3FF0000000000000
HALF = 0x3FE0000000000000
ONE_AND_HALF = 0x3FF8000000000000
NX, DZ, NV = 1 << 4, 1 << 7, 1 << 8


def _runtimes(source=b"", *, page_size=4096):
    result = []
    for backend in ("python", "native"):
        runtime = MegaForthRuntime(
            execution_backend=backend,
            memory=create_one_core_address_space(page_size=page_size),
        )
        if source:
            runtime.evaluate(source, source_name="native-scalar-fp.f")
        # Successful hot-path checks start with real output storage even for
        # a zero-operand FPCSR@. Sparse output fallback has its own case below.
        runtime.memory.write_bytes(runtime.main_context.data.empty_pointer - 64,
                                   bytes(64))
        result.append(runtime)
    return result


def _observe(runtime, word="RUN", *, inputs=(), step_budget=None):
    context = runtime.main_context
    for value in inputs:
        context.data.push(value)
    before_steps = runtime.diagnostics.semantic_cycles
    before_native = runtime.native_execution_stats["semantic_steps"]
    outcome = None
    error = None
    try:
        outcome = runtime.execute(word, step_budget=step_budget)
    except Exception as caught:
        error = (type(caught), str(caught))
    observed = {
        "result_steps": None if outcome is None else outcome.semantic_steps,
        "counted_steps": runtime.diagnostics.semantic_cycles - before_steps,
        "data": context.data.snapshot(),
        "returns": context.returns.snapshot(),
        "sp": context.data.pointer,
        "rp": context.returns.pointer,
        "data_bytes": runtime.memory.read_bytes(context.data.empty_pointer - 64, 64),
        "return_bytes": runtime.memory.read_bytes(context.returns.empty_pointer - 64, 64),
        "fpcsr": runtime.scalar_float.fpcsr,
        "timer": (runtime.timer.counter, runtime.timer.status,
                  runtime.timer.irq_pending),
        "uart": runtime.uart_output,
        "error": error,
        "reusable": context.reusable,
        "suspended": context.suspended,
        "host_control_fault": context.host_control_fault,
    }
    return observed, runtime.native_execution_stats["semantic_steps"] - before_native


def _compare(runtimes, word="RUN", *, native_steps=None, **kwargs):
    reference, native = [_observe(runtime, word, **kwargs) for runtime in runtimes]
    assert native[0] == reference[0]
    assert reference[1] == 0
    if native_steps is not None:
        assert native[1] == native_steps
    return reference[0]


def _reject_service_call(*_args):
    raise AssertionError("compiled scalar FP exited to the Python service")


@pytest.mark.parametrize("name,shape,op", scalar_fp.BIOS_WORDS,
                         ids=[name for name, _, _ in scalar_fp.BIOS_WORDS])
def test_all_bios_words_run_directly_and_match_reference(name, shape, op):
    runtimes = _runtimes(b": RUN " + name.encode("ascii") + b" ;")
    runtimes[1].scalar_float._native_execute = _reject_service_call
    # Full arithmetic bit-pattern coverage belongs to the shared-kernel gate.
    # These patterns exercise this adapter's operand order, full-cell integer
    # inputs, narrow-source masking, NaNs and result/flag publication.
    patterns = (
        (0x3FF000003F800001, 0x4008000040400000, 0xBFF00000BF800000),
        (0x7FF000007F800001, 0x8000000080000000, 1),
        (MASK64, 1, 0x7FEFFFFFFFFFFFFF),
    )
    for rounding in range(5):
        for index, (left, right, addend) in enumerate(patterns):
            initial_csr = rounding | DZ
            if shape == "fetch":
                arguments = ()
                expected_stack, expected_csr = (initial_csr,), initial_csr
            elif shape == "store":
                arguments = ((0, MASK64, NV | 5)[index],)
                expected_stack = ()
                expected_csr = arguments[0] & scalar_fp.FPCSR_WRITE_MASK
            else:
                if shape == "unary":
                    arguments, operands = (left,), (left, left, 0)
                elif shape == "fma":
                    arguments, operands = (left, right, addend), (addend, left, right)
                else:
                    arguments, operands = (left, right), (left, right, 0)
                expected = scalar_fp.execute(op, *operands, initial_csr)
                expected_stack = (expected.value,)
                expected_csr = initial_csr | expected.flags
            for runtime in runtimes:
                runtime.main_context.data.clear()
                runtime.scalar_float.write_fpcsr(initial_csr)
            observed = _compare(runtimes, inputs=arguments, native_steps=2)
            assert observed["error"] is None, (name, rounding, index)
            assert observed["result_steps"] == 3
            assert observed["data"] == expected_stack
            assert observed["fpcsr"] == expected_csr


@pytest.mark.parametrize("operation,inputs,expected", [
    (b"F64+", (ONE, HALF), (ONE_AND_HALF,)),
    (b"FPCSR@", (), (DZ | 2,)),
    (b"FPCSR!", (MASK64,), ()),
])
@pytest.mark.parametrize("budget", [1, 2, 3])
def test_primitive_budget_keeps_call_tick_before_effect_tick(
    operation, inputs, expected, budget
):
    runtimes = _runtimes(b": RUN " + operation + b" ;")
    for runtime in runtimes:
        runtime.scalar_float.write_fpcsr(DZ | 2)
    observed = _compare(runtimes, inputs=inputs, step_budget=budget,
                        native_steps=0 if budget == 1 else 2)
    assert observed["counted_steps"] == budget
    assert observed["data"] == (inputs if budget == 1 else expected)
    assert observed["fpcsr"] == (
        scalar_fp.FPCSR_WRITE_MASK if operation == b"FPCSR!" and budget >= 2
        else DZ | 2
    )
    assert observed["returns"] == ()
    if budget < 3:
        assert observed["error"][0] is StepBudgetExceeded
    else:
        assert observed["error"] is None


@pytest.mark.parametrize("operation,inputs", [
    (b"F64SQRT", ()),
    (b"F64+", ()),
    (b"F64+", (ONE,)),
    (b"F64FMA", ()),
    (b"F64FMA", (ONE,)),
    (b"F64FMA", (ONE, HALF)),
    (b"FPCSR!", ()),
])
@pytest.mark.parametrize("rounding", [0, 5])
def test_missing_operands_keep_partial_pops_before_rounding_validation(
    operation, inputs, rounding
):
    runtimes = _runtimes(b": RUN " + operation + b" ;")
    for runtime in runtimes:
        runtime.scalar_float.write_fpcsr(rounding | DZ)
    observed = _compare(runtimes, inputs=inputs, native_steps=0)
    assert observed["error"] == (
        StackUnderflow, "data stack underflow during pop: requires 1 entry, has 0"
    )
    assert observed["data"] == ()
    assert observed["fpcsr"] == rounding | DZ
    assert observed["counted_steps"] == 2


def test_fpcsr_fetch_overflow_preserves_the_existing_cell_and_flags():
    runtimes = _runtimes(b": RUN FPCSR@ ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.data.empty_pointer
        context.data = DataStack((17,), memory=runtime.memory,
                                 floor=empty - 8, empty_pointer=empty)
        runtime.scalar_float.write_fpcsr(NX | 4)
    observed = _compare(runtimes, native_steps=0)
    assert observed["error"][0] is StackOverflow
    assert observed["data"] == (17,)
    assert observed["fpcsr"] == NX | 4
    assert observed["counted_steps"] == 2


def test_unary_fp_needs_no_extra_slot_on_a_full_one_cell_stack():
    runtimes = _runtimes(b": RUN F64SQRT ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.data.empty_pointer
        context.data = DataStack((ONE,), memory=runtime.memory,
                                 floor=empty - 8, empty_pointer=empty)
    runtimes[1].scalar_float._native_execute = _reject_service_call
    observed = _compare(runtimes, native_steps=2)
    assert observed["error"] is None
    assert observed["data"] == (ONE,)


@pytest.mark.parametrize("operation,inputs", [
    (b"F64+", (ONE, HALF)),
    (b"F64FMA", (ONE, HALF, ONE)),
])
def test_reserved_rounding_fault_callback_observes_consumed_operands(
    operation, inputs
):
    runtimes = _runtimes()
    records = [[], []]
    for runtime, calls in zip(runtimes, records):
        def record(context, *, runtime=runtime, calls=calls):
            calls.append((context.data.snapshot(), runtime.scalar_float.fpcsr,
                          runtime.diagnostics.semantic_cycles, runtime.timer.counter))

        callback = runtime.define_primitive("OBSERVE-FAULT", record)
        runtime.set_fault_xt(callback.xt)
        runtime.evaluate(b": RUN " + operation + b" ;")
        runtime.scalar_float.write_fpcsr(NX | 5)
    observed = _compare(runtimes, inputs=(77, *inputs), native_steps=0)
    assert records[0] == records[1]
    assert len(records[0]) == 1
    assert records[0][0][:2] == ((77, (-21) & MASK64), NX | 5)
    assert records[0][0][2] == records[0][0][3]
    assert observed["error"] == (
        ForthAbort, "reserved FPCSR.RM 5 for a dynamic mode"
    )
    assert observed["fpcsr"] == NX | 5
    assert observed["data"] == ()
    assert observed["uart"] == b"\r\n*** ILLEGAL INSTRUCTION CORE=00\r\n"


@pytest.mark.parametrize("operation,inputs,expected", [
    (b"F64TRUNC", (ONE_AND_HALF,), (ONE,)),
    (b"F64>S", (ONE_AND_HALF,), (1,)),
    (b"F64MIN", (ONE, HALF), (HALF,)),
])
def test_fixed_rounding_and_nonrounding_words_admit_reserved_global_mode(
    operation, inputs, expected
):
    runtimes = _runtimes(b": RUN " + operation + b" ;")
    for runtime in runtimes:
        runtime.scalar_float.write_fpcsr(7)
    runtimes[1].scalar_float._native_execute = _reject_service_call
    observed = _compare(runtimes, inputs=inputs, native_steps=2)
    assert observed["error"] is None
    assert observed["data"] == expected
    assert observed["fpcsr"] == (7 | NX if operation == b"F64>S" else 7)


def test_python_callback_observes_flags_and_reentry_uses_its_rounding_state():
    runtimes = _runtimes()
    records = [[], []]
    for runtime, calls in zip(runtimes, records):
        def observe(context, *, runtime=runtime, calls=calls):
            calls.append((context.data.snapshot(), runtime.scalar_float.fpcsr,
                          runtime.diagnostics.semantic_cycles, runtime.timer.counter))
            runtime.scalar_float.write_fpcsr(2)

        runtime.define_primitive("OBSERVE", observe)
        runtime.evaluate(
            b": RUN 1 S>F64 0 S>F64 F64/ DROP OBSERVE "
            b"1 S>F64 3 S>F64 F64/ DROP FPCSR@ ;"
        )
    runtimes[1].scalar_float._native_execute = _reject_service_call
    observed = _compare(runtimes)
    assert observed["error"] is None
    assert observed["data"] == (NX | 2,)
    assert observed["fpcsr"] == NX | 2
    assert records[0] == records[1]
    assert len(records[0]) == 1
    assert records[0][0][:2] == ((), DZ)
    assert records[0][0][2] == records[0][0][3]


@pytest.mark.parametrize("quantum", [1, 2, 5, 11])
def test_host_quantum_publishes_current_fpcsr_at_each_original_boundary(quantum):
    runtimes = _runtimes(
        b": RUN 1 S>F64 0 S>F64 F64/ DROP FPCSR@ "
        b"2 FPCSR! 1 S>F64 3 S>F64 F64/ DROP FPCSR@ ;"
    )
    traces = []
    for runtime in runtimes:
        result = runtime.run_until_blocked("RUN", quantum_steps=quantum,
                                           step_budget=200)
        trace = []
        for _ in range(100):
            context = runtime.main_context
            trace.append((type(result), result.semantic_steps,
                          runtime.scalar_float.fpcsr, context.data.snapshot(),
                          context.returns.snapshot(), runtime.timer.counter,
                          runtime.diagnostics.semantic_cycles))
            if not isinstance(result, YieldedExecution):
                break
            result = runtime.resume_yielded(result.suspension)
        else:
            pytest.fail("bounded FP fixture did not finish")
        assert runtime.main_context.data.snapshot() == (DZ, NX | 2)
        assert runtime.scalar_float.fpcsr == NX | 2
        traces.append(trace)
    assert traces[0] == traces[1]


@pytest.mark.parametrize("page_size", [4, 16])
def test_fma_handles_fragmented_stack_cells_without_service_calls(page_size):
    runtimes = _runtimes(b": RUN F64FMA ;", page_size=page_size)
    runtimes[1].scalar_float._native_execute = _reject_service_call
    observed = _compare(runtimes, inputs=(ONE, HALF, ONE), native_steps=2)
    assert observed["error"] is None
    assert observed["data"] == (ONE_AND_HALF,)


def test_absent_result_page_falls_back_before_flags_and_pops():
    runtimes = _runtimes(b": RUN F64/ ;")
    for runtime in runtimes:
        context = runtime.main_context
        context.data.push(0)
        context.data.push(0)
        for region in runtime.memory._regions:
            address = context.data.pointer
            if region.spec.base <= address < region.spec.base + region.spec.size:
                region.pages.pop((address - region.spec.base) // runtime.memory.page_size)
                break
        else:
            pytest.fail("the backed data stack must belong to a memory region")
    service = runtimes[1].scalar_float
    native_execute = service._native_execute
    calls = []

    def observe_service(*args):
        calls.append((runtimes[1].main_context.data.snapshot(), service.fpcsr))
        return native_execute(*args)

    service._native_execute = observe_service
    observed = _compare(runtimes, native_steps=0)
    assert observed["error"] is None
    assert observed["data"] == (0x7FF8000000000000,)
    assert observed["fpcsr"] == NV
    assert calls == [((), 0)]


@pytest.mark.parametrize("operation,inputs,body,original_stack", [
    (b"F64+", (ONE, HALF), b"2DROP 91", (ONE_AND_HALF,)),
    (b"FPCSR@", (), b"91", (DZ | 2,)),
    (b"FPCSR!", (5,), b"DROP 91", ()),
])
@pytest.mark.parametrize("shadow_kind", ["colon", "host-primitive"])
@pytest.mark.parametrize("prepare_first", [False, True])
def test_fp_admission_keeps_original_word_identity_after_shadowing(
    operation, inputs, body, original_stack, shadow_kind, prepare_first
):
    runtimes = _runtimes(b": ORIGINAL " + operation + b" ;")
    records = [[], []]
    for runtime in runtimes:
        runtime.scalar_float.write_fpcsr(DZ | 2)
    if prepare_first:
        assert _compare(runtimes, "ORIGINAL", inputs=inputs,
                        native_steps=2)["data"] == original_stack
        for runtime in runtimes:
            runtime.main_context.data.clear()
    for runtime, calls in zip(runtimes, records):
        if shadow_kind == "colon":
            runtime.evaluate(b": " + operation + b" " + body + b" ;")
        else:
            def shadow(context, *, calls=calls):
                calls.append(context.data.snapshot())
                for _ in inputs:
                    context.data.pop()
                context.data.push(91)

            runtime.define_primitive(operation, shadow)
        runtime.evaluate(b": CURRENT " + operation + b" ;")
        runtime.scalar_float.write_fpcsr(DZ | 2)
    observed = _compare(runtimes, "ORIGINAL", inputs=inputs, native_steps=2)
    assert observed["error"] is None
    assert observed["data"] == original_stack
    assert observed["fpcsr"] == (5 if operation == b"FPCSR!" else DZ | 2)
    assert records == [[], []]
    for runtime in runtimes:
        runtime.main_context.data.clear()
        runtime.scalar_float.write_fpcsr(DZ | 2)
    observed = _compare(runtimes, "CURRENT", inputs=inputs)
    assert observed["error"] is None
    assert observed["data"] == (91,)
    assert observed["fpcsr"] == DZ | 2
    assert records == ([[inputs], [inputs]] if shadow_kind == "host-primitive"
                       else [[], []])


def test_dictionary_rollback_discards_a_cached_fp_plan_at_reused_xt():
    runtimes = _runtimes()
    checkpoints = [runtime.dictionary.checkpoint() for runtime in runtimes]
    for runtime in runtimes:
        runtime.evaluate(b": RUN F64+ ;")
    tokens = [runtime.find("RUN").xt for runtime in runtimes]
    assert _compare(runtimes, inputs=(ONE, HALF), native_steps=2)["data"] == (
        ONE_AND_HALF,
    )
    for runtime, checkpoint, old_xt in zip(runtimes, checkpoints, tokens):
        runtime.main_context.data.clear()
        runtime.dictionary.rollback(checkpoint)
        runtime.evaluate(b": RUN 2DROP 91 ;")
        assert runtime.find("RUN").xt == old_xt
    observed = _compare(runtimes, inputs=(ONE, HALF))
    assert observed["error"] is None
    assert observed["data"] == (91,)


def test_compiled_fp_retains_the_service_captured_by_original_bios_callbacks():
    runtimes = _runtimes(
        b": RUN FPCSR@ 0 FPCSR! 1 S>F64 0 S>F64 F64/ DROP FPCSR@ ;"
    )
    owners = [runtime.scalar_float for runtime in runtimes]
    for runtime, original in zip(runtimes, owners):
        original.write_fpcsr(DZ | 2)
        replacement = HostedScalarFloatService()
        replacement.write_fpcsr(NX | 5)
        runtime.scalar_float = replacement
    owners[1]._native_execute = _reject_service_call
    observed = _compare(runtimes)
    assert observed["error"] is None
    assert observed["data"] == (DZ | 2, DZ)
    assert [owner.fpcsr for owner in owners] == [DZ, DZ]
    assert observed["fpcsr"] == NX | 5
