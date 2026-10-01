"""Machine routines run inside the semantic runtime on the caller's stacks."""

from __future__ import annotations

import pytest

from asm import assemble
from hybrid.manifest import BufferRule, CallbackSite, RoutineDeclaration
from hybrid.runtime import (
    HybridExecutionError,
    HybridRuntime,
    MachineAccessFault,
    MachineBudgetExceeded,
)
from simulator.errors import ForthAbort
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import ExecutionResult, YieldedExecution


CALLBACK = """
    mov r12, r3
after_pc:
    addi r12, 0
call:
    call.l r12
    add r4, r5
    ret.l
stub:
    ret.l
"""
EXCEPTIONS = b"""
VARIABLE HANDLER
: CATCH  SP@ >R HANDLER @ >R RP@ HANDLER ! EXECUTE R> HANDLER ! R> DROP 0 ;
: THROW  ?DUP IF HANDLER @ RP! R> HANDLER ! R> SWAP >R SP! DROP R> THEN ;
"""


def code(source):
    return bytes(assemble(source))


def callback_code(source=CALLBACK):
    """Assemble position-independent code whose r12 holds the stub address."""

    labels = {}
    assemble(source, labels_out=labels)
    delta = labels["stub"] - labels["after_pc"]
    final = source.replace("addi r12, 0", f"addi r12, {delta}")
    labels = {}
    raw = bytes(assemble(final, labels_out=labels))
    return raw, labels


def callback_routine(name, target, *, inputs=2, outputs=1, site_in=2, site_out=1):
    raw, labels = callback_code()
    return RoutineDeclaration(name, raw, input_cells=inputs, output_cells=outputs, callbacks=(
        CallbackSite(labels["call"], labels["stub"], target, site_in, site_out),))


@pytest.fixture(params=("python", "native"))
def hybrid(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    owner = HybridRuntime.create(executor=request.param,
                                 geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    yield owner
    owner.close()


def stack(hybrid):
    return hybrid.semantic.main_context.data.snapshot()


def test_a_routine_takes_and_gives_cells_on_the_data_stack(hybrid):
    hybrid.register(RoutineDeclaration("H+", code("add r4, r5\nret.l"), input_cells=2, output_cells=1))
    hybrid.semantic.evaluate(b"9 40 2 H+")
    assert stack(hybrid) == (9, 42)
    assert hybrid.semantic.main_context.returns.depth() == 0
    assert (hybrid.transitions, hybrid.callback_requests) == (1, 0)


def test_a_callback_runs_a_forth_word_on_the_callers_stacks(hybrid):
    runtime = hybrid.semantic
    hybrid.register(callback_routine("HCB", "PROBE"))
    # PROBE sees the routine's machine frames on the shared return stack.
    runtime.evaluate(b"VARIABLE SEEN : PROBE ( a b -- c ) RP@ SEEN ! + 2 * ;")
    runtime.evaluate(b": CALLER 3 4 HCB ; RP@ CALLER")
    frontier, result = stack(hybrid)
    assert result == (3 + 4) * 2 + 4
    seen = runtime.memory.read64(runtime.find("SEEN").body_address)
    # Caller continuation, routine sentinel, then the machine's return address.
    assert seen == frontier - 3 * 8
    assert hybrid.callback_requests == 1 and hybrid.parked is False


def test_a_callback_may_be_a_primitive(hybrid):
    hybrid.register(callback_routine("HCB", "+"))
    hybrid.semantic.evaluate(b"5 6 HCB")
    assert stack(hybrid) == (5 + 6 + 6,)


def test_a_callback_may_call_routines_including_its_own(hybrid):
    runtime = hybrid.semantic
    # HREC(a, b) = INNER(a, b) + b, and INNER recurses through HREC until b is 0.
    hybrid.register(callback_routine("HREC", "INNER"))
    runtime.evaluate(b": INNER ( a b -- c ) DUP 0= IF DROP ELSE 1- HREC THEN ;")
    runtime.evaluate(b"1 10 HREC")
    assert stack(hybrid) == (1 + 10 * 11 // 2,)
    assert hybrid.transitions == 11 and hybrid.callback_requests == 11
    assert runtime.main_context.returns.depth() == 0 and hybrid.parked is False


def test_a_nested_routine_returns_into_its_parked_caller(hybrid):
    runtime = hybrid.semantic
    hybrid.register(RoutineDeclaration("H+", code("add r4, r5\nret.l"), input_cells=2, output_cells=1))
    runtime.evaluate(b": INNER ( a b -- c ) 2DUP H+ NIP + ;")
    hybrid.register(callback_routine("HCB", "INNER"))
    runtime.evaluate(b"1 2 HCB")
    # INNER(1,2) = 1 + (1+2) = 4; the routine then adds its second argument.
    assert stack(hybrid) == (4 + 2,)
    assert hybrid.transitions == 2


def test_the_callback_target_is_bound_by_name_when_first_used(hybrid):
    runtime = hybrid.semantic
    hybrid.register(callback_routine("HCB", "LATER"))
    with pytest.raises(HybridExecutionError, match="LATER"):
        runtime.evaluate(b"1 2 HCB")
    assert hybrid.parked is False
    runtime.main_context.data.clear()
    runtime.evaluate(b": LATER * ; 3 4 HCB")
    assert stack(hybrid) == (3 * 4 + 4,)


def test_a_callback_must_leave_the_cells_its_site_declares(hybrid):
    runtime = hybrid.semantic
    runtime.evaluate(b": SLOPPY ( a b -- a b ) ;")
    hybrid.register(callback_routine("HCB", "SLOPPY"))
    with pytest.raises(HybridExecutionError, match="SLOPPY left 2 cells"):
        runtime.evaluate(b"1 2 HCB")
    assert hybrid.parked is False


def test_a_throw_from_a_callback_unwinds_past_the_machine(hybrid):
    runtime = hybrid.semantic
    runtime.evaluate(EXCEPTIONS)
    runtime.evaluate(b": BAD ( a b -- c ) 2DROP 7 THROW ;")
    hybrid.register(callback_routine("HCB", "BAD"))
    runtime.evaluate(b": TRY ( -- code ) 11 22 ['] HCB CATCH NIP NIP ;")
    runtime.evaluate(b"TRY")
    assert stack(hybrid) == (7,)
    # The abandoned entry does not block the next call.
    hybrid.register(RoutineDeclaration("H+", code("add r4, r5\nret.l"), input_cells=2, output_cells=1))
    runtime.evaluate(b"1 2 H+")
    assert stack(hybrid) == (7, 3)
    assert hybrid.parked is False


def test_buffers_named_by_arguments_are_the_only_memory_a_routine_reaches(hybrid):
    runtime = hybrid.semantic
    hybrid.register(RoutineDeclaration(
        "HSTORE", code("str r4, r6\nret.l"), input_cells=3,
        buffers=(BufferRule(0, 1, element_bytes=8, access="write"),)))
    address = EXTERNAL_BASE + 0x1000
    runtime.evaluate(f"{address} 1 77 HSTORE".encode())
    assert runtime.memory.read64(address) == 77
    with pytest.raises(MachineAccessFault, match="borrowed spans"):
        runtime.evaluate(f"{address} 0 99 HSTORE".encode())
    assert runtime.memory.read64(address) == 77


def test_an_illegal_control_transfer_enters_the_fault_callback(hybrid):
    runtime = hybrid.semantic
    hybrid.register(RoutineDeclaration("HJUMP", code("ldi64 r12, 0x1230\ncall.l r12\nret.l")))
    runtime.evaluate(b"VARIABLE CODE : ON-FAULT ( n -- ) CODE ! ; ' ON-FAULT FAULT-XT!")
    with pytest.raises(ForthAbort):
        runtime.evaluate(b"HJUMP")
    assert runtime.memory.read64(runtime.find("CODE").body_address) == (-21) & ((1 << 64) - 1)
    assert hybrid.parked is False


def test_a_routine_yields_its_host_turn_and_resumes(hybrid):
    runtime = hybrid.semantic
    hybrid.register(RoutineDeclaration("HCOUNT", code("loop:\nsubi r4, 1\nbrne loop\nret.l"),
                                       input_cells=1, output_cells=1))
    runtime.evaluate(b": RUN 1000 HCOUNT 5 + ;")
    result = runtime.run_until_blocked("RUN", machine_quantum_instructions=300)
    turns = 1
    while isinstance(result, YieldedExecution):
        result = runtime.resume_yielded(result.suspension)
        turns += 1
    assert isinstance(result, ExecutionResult)
    assert stack(hybrid) == (5,)
    assert turns == 7  # 2,001 instructions in turns of 300


def test_a_parked_routine_survives_a_semantic_suspension_in_its_callback(hybrid):
    runtime = hybrid.semantic
    runtime.evaluate(b": SLOW ( a b -- c ) 2000 0 DO LOOP + ;")
    hybrid.register(callback_routine("HCB", "SLOW"))
    runtime.evaluate(b": RUN 10 20 HCB ;")
    result = runtime.run_until_blocked("RUN", quantum_steps=500)
    yields = 0
    while isinstance(result, YieldedExecution):
        assert hybrid.parked
        result = runtime.resume_yielded(result.suspension)
        yields += 1
    assert yields > 0 and stack(hybrid) == (50,)
    assert hybrid.parked is False


def test_cancelling_a_suspension_abandons_its_machine_entries(hybrid):
    runtime = hybrid.semantic
    hybrid.register(RoutineDeclaration("HCOUNT", code("loop:\nsubi r4, 1\nbrne loop\nret.l"),
                                       input_cells=1, output_cells=1))
    runtime.evaluate(b": RUN 1000 HCOUNT ;")
    result = runtime.run_until_blocked("RUN", machine_quantum_instructions=100)
    assert isinstance(result, YieldedExecution) and hybrid.parked
    runtime.cancel_suspension(result.suspension)
    assert hybrid.parked is False
    runtime.evaluate(b"3 HCOUNT")
    assert stack(hybrid)[-1] == 0


def test_a_callback_may_evaluate_source(hybrid):
    runtime = hybrid.semantic
    runtime.evaluate(b': SUMS ( a b -- c ) S" + 1+" EVALUATE ;')
    hybrid.register(callback_routine("HCB", "SUMS"))
    runtime.evaluate(b"2 3 HCB")
    assert stack(hybrid) == (2 + 3 + 1 + 3,)


def test_a_routine_whose_body_was_reclaimed_cannot_run():
    owner = HybridRuntime.create(executor="python",
                                 geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    try:
        runtime = owner.semantic
        word = owner.register(RoutineDeclaration("HINC", code("inc r4\nret.l"),
                                                 input_cells=1, output_cells=1))
        # Moving HERE back over the body reclaims it while the word remains.
        runtime.allot_dictionary(word.body_address - runtime.dictionary.here, runtime.main_context)
        with pytest.raises(HybridExecutionError, match="reclaimed"):
            runtime.evaluate(b"5 HINC")
        assert owner.registered_routines == ()
    finally:
        owner.close()


def test_a_rolled_back_routine_frees_its_space_for_a_new_one():
    owner = HybridRuntime.create(executor="python",
                                 geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    try:
        runtime = owner.semantic
        dictionary = runtime.dictionary
        checkpoint = dictionary.checkpoint()
        word = owner.register(RoutineDeclaration("HINC", code("inc r4\nret.l"),
                                                 input_cells=1, output_cells=1))
        dictionary.rollback(checkpoint)
        again = owner.register(RoutineDeclaration("HINC", code("inc r4\ninc r4\nret.l"),
                                                  input_cells=1, output_cells=1))
        assert again.body_address == word.body_address
        runtime.evaluate(b"5 HINC")
        assert runtime.main_context.data.snapshot()[-1] == 7
    finally:
        owner.close()


def test_a_dispatch_machine_budget_stops_a_runaway_routine():
    owner = HybridRuntime.create(executor="python", machine_instruction_budget=50,
                                 geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    try:
        owner.register(RoutineDeclaration("HCOUNT", code("loop:\nsubi r4, 1\nbrne loop\nret.l"),
                                          input_cells=1, output_cells=1))
        with pytest.raises(MachineBudgetExceeded):
            owner.semantic.evaluate(b"1000 HCOUNT")
        assert owner.parked is False
        owner.semantic.evaluate(b"10 HCOUNT")
    finally:
        owner.close()


def test_a_routine_needs_its_argument_cells(hybrid):
    from simulator.stacks import StackUnderflow

    hybrid.register(RoutineDeclaration("H+", code("add r4, r5\nret.l"), input_cells=2, output_cells=1))
    with pytest.raises(StackUnderflow):
        hybrid.semantic.evaluate(b"1 H+")


@pytest.fixture
def native_hybrid():
    pytest.importorskip("_megaforth_native")
    owner = HybridRuntime.create(executor="native",
                                 geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    yield owner
    owner.close()


def _forbid_python_calls(monkeypatch):
    def refuse(*_args, **_kwargs):
        raise AssertionError("the native executor should call this routine itself")
    monkeypatch.setattr(HybridRuntime, "call", refuse)


def test_native_code_calls_a_leaf_routine_without_python(native_hybrid, monkeypatch):
    runtime = native_hybrid.semantic
    native_hybrid.register(RoutineDeclaration("H+", code("add r4, r5\nret.l"),
                                              input_cells=2, output_cells=1))
    runtime.evaluate(b": SUMS ( n -- total ) 0 SWAP 0 DO I H+ LOOP ;")
    runtime.evaluate(b"10 SUMS DROP")  # the first run plans the loop
    _forbid_python_calls(monkeypatch)
    runtime.main_context.data.clear()
    runtime.evaluate(b"100 SUMS")
    assert stack(native_hybrid) == (sum(range(100)),)
    assert native_hybrid.transitions >= 100


def test_a_natively_called_routine_reaches_its_borrowed_buffers(native_hybrid, monkeypatch):
    runtime = native_hybrid.semantic
    native_hybrid.register(RoutineDeclaration(
        "HSTORE", code("str r4, r6\nret.l"), input_cells=3,
        buffers=(BufferRule(0, 1, element_bytes=8, access="write"),)))
    address = EXTERNAL_BASE + 0x2000
    runtime.evaluate(f": FILL8 8 0 DO {address} I 8 * + 1 I HSTORE LOOP ;".encode())
    runtime.execute("FILL8")
    _forbid_python_calls(monkeypatch)
    runtime.execute("FILL8")
    assert [runtime.memory.read64(address + 8 * i) for i in range(8)] == list(range(8))


def test_a_natively_called_routine_that_yields_is_resumed_through_python(native_hybrid):
    runtime = native_hybrid.semantic
    native_hybrid.register(RoutineDeclaration("HCOUNT", code("loop:\nsubi r4, 1\nbrne loop\nret.l"),
                                              input_cells=1, output_cells=1))
    runtime.evaluate(b": RUN 1000 HCOUNT 5 + ;")
    result = runtime.run_until_blocked("RUN", machine_quantum_instructions=300)
    turns = 1
    while isinstance(result, YieldedExecution):
        result = runtime.resume_yielded(result.suspension)
        turns += 1
    assert isinstance(result, ExecutionResult) and stack(native_hybrid) == (5,)
    assert turns == 7 and native_hybrid.parked is False


def test_a_natively_called_routine_fault_reaches_the_fault_callback(native_hybrid):
    runtime = native_hybrid.semantic
    native_hybrid.register(RoutineDeclaration("HJUMP", code("ldi64 r12, 0x1230\ncall.l r12\nret.l")))
    runtime.evaluate(b"VARIABLE CODE : ON-FAULT ( n -- ) CODE ! ; ' ON-FAULT FAULT-XT!")
    runtime.evaluate(b": JUMPER 1 DROP HJUMP ;")
    with pytest.raises(ForthAbort):
        runtime.execute("JUMPER")
    assert runtime.memory.read64(runtime.find("CODE").body_address) == (-21) & ((1 << 64) - 1)


def test_a_reclaimed_routine_is_replanned_and_refused(native_hybrid):
    runtime = native_hybrid.semantic
    word = native_hybrid.register(RoutineDeclaration("HINC", code("inc r4\nret.l"),
                                                     input_cells=1, output_cells=1))
    runtime.evaluate(b": BUMP 1 HINC ;")
    runtime.execute("BUMP")
    runtime.allot_dictionary(word.body_address - runtime.dictionary.here, runtime.main_context)
    with pytest.raises(HybridExecutionError, match="reclaimed"):
        runtime.execute("BUMP")
