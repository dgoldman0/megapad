"""Declared machine calls share bytes while preserving semantic authority."""

from __future__ import annotations

import pytest


native = pytest.importorskip("_mp64_accel")

from asm import assemble  # noqa: E402
from hybrid.runtime import HybridExecutionError, HybridRuntime  # noqa: E402
from shared.cells import MASK64  # noqa: E402
from shared.hybrid_abi import BufferRuleV1, RoutineImageV1  # noqa: E402
from simulator.errors import ExecutionError  # noqa: E402
from simulator.memory import EXTERNAL_BASE, MMIO_BASE, MMIO_LIMIT  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import ExecutionContext, YieldedExecution  # noqa: E402
from simulator.stacks import DataStack, ReturnStack, StackOverflow, StackUnderflow  # noqa: E402


EXTERNAL_SIZE = 4096


def _image(name="H-INC", program="inc r4\nret.l", *, inputs=1, outputs=1,
           buffers=(), max_instructions=100, return_stack_cells=16):
    return RoutineImageV1(
        name=name,
        code=bytes(assemble(program)),
        entry_offset=0,
        input_cells=inputs,
        output_cells=outputs,
        buffers=buffers,
        max_instructions=max_instructions,
        return_stack_cells=return_stack_cells,
    )


def _buffer(*, access="read_write", element_bytes=1, max_bytes=EXTERNAL_SIZE):
    return BufferRuleV1(
        address_argument=0,
        length_argument=1,
        element_bytes=element_bytes,
        max_bytes=max_bytes,
        access=access,
    )


def _create(executor="python", *, dispatch_instruction_limit=1000):
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    memory = create_one_core_address_space(
        bank0_size=65536, external_size=EXTERNAL_SIZE, dense_backing=True,
    )
    return HybridRuntime.create(
        executor=executor,
        memory=memory,
        dispatch_instruction_limit=dispatch_instruction_limit,
    )


@pytest.fixture(params=("python", "native"))
def hybrid(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    runtime = _create(request.param)
    try:
        yield runtime
    finally:
        runtime.close()


def _push(runtime, *values, context=None):
    context = runtime.semantic.main_context if context is None else context
    for value in values:
        context.data.push(value)
    return context


def _return_state(runtime, context):
    stack = context.returns
    return (
        stack.pointer,
        stack.snapshot(),
        dict(stack._continuations),
        stack._continuation_cookie,
        stack.pointer_capture_checkpoint(),
        runtime.semantic.memory.read_bytes(
            stack.floor, stack.empty_pointer - stack.floor,
        ),
    )


def test_registration_publishes_exact_word_body_without_executing(hybrid):
    semantic = hybrid.semantic
    context = semantic.main_context
    before = context.data.snapshot(), context.returns.snapshot()
    word = hybrid.register_routine_v1(_image())
    declaration = hybrid.declaration_for(word)
    lease = semantic.dictionary.acquire_body_lease(word)

    assert semantic.find("H-INC") is word
    assert semantic.dictionary.resolve(word.xt) is word
    assert declaration.allocation_lease is lease
    assert declaration.allocation_generation == lease.allocation_serial
    assert declaration.body_base == lease.body_address
    assert declaration.body_size == lease.body_limit - lease.body_address
    assert declaration.code_base % 16 == declaration.code_size % 16 == 0
    assert declaration.body_base <= declaration.code_base
    assert declaration.code_base + declaration.code_size <= lease.body_limit
    assert semantic.memory.read_bytes(
        declaration.code_base, declaration.code_size,
    ) == declaration.code
    assert not any(
        region.base <= declaration.stack_base < region.limit
        for region in semantic.memory.regions
    )
    assert (context.data.snapshot(), context.returns.snapshot()) == before


def test_interpreted_compiled_and_execute_calls_use_the_same_registered_word(hybrid):
    word = hybrid.register_routine_v1(_image())
    report = hybrid.evaluate(
        b"1 H-INC : WRAPPED H-INC ; 2 WRAPPED 3 ' H-INC EXECUTE"
    )

    assert hybrid.semantic.main_context.data.snapshot() == (2, 3, 4)
    assert hybrid.semantic.main_context.returns.snapshot() == ()
    assert report.machine_instructions == 6
    assert report.machine_cycles >= report.machine_instructions
    assert report.transitions == 3
    assert report.semantic_result.semantic_steps > 0
    assert hybrid.semantic.dictionary.resolve(word.xt) is word


@pytest.mark.parametrize("arguments", ((), tuple(range(8))))
def test_zero_and_eight_cell_signatures_preserve_input_order_and_stack_prefix(
    hybrid, arguments,
):
    word = hybrid.register_routine_v1(_image(
        program="ret.l", inputs=len(arguments), outputs=len(arguments),
    ))
    context = _push(hybrid, 0xCAFE, *arguments)
    report = hybrid.execute_xt(word.xt)

    assert context.data.snapshot() == (0xCAFE, *arguments)
    assert report.machine_instructions == report.transitions == 1


def test_live_shadowing_keeps_compiled_and_numeric_bindings(hybrid):
    original = hybrid.register_routine_v1(_image())
    hybrid.evaluate(b": COMPILED H-INC ; : H-INC 100 + ;")
    lease = hybrid.declaration_for(original).allocation_lease
    assert hybrid.semantic.dictionary.is_body_lease_live(lease)

    hybrid.evaluate(b"1 H-INC 1 COMPILED")
    _push(hybrid, 1)
    report = hybrid.execute_xt(original.xt)

    assert hybrid.semantic.main_context.data.snapshot() == (101, 2, 2)
    assert report.machine_instructions == 2


def test_body_address_and_unknown_numbers_do_not_become_machine_execution_tokens(hybrid):
    word = hybrid.register_routine_v1(_image())
    context = _push(hybrid, 7)
    for token in (word.body_address, MASK64):
        with pytest.raises(KeyError, match="unknown execution token"):
            hybrid.execute_xt(token)
    assert context.data.snapshot() == (7,)


def test_stack_replacement_retains_unrelated_inactive_sp_and_rp_storage(hybrid):
    word = hybrid.register_routine_v1(_image())
    context = _push(hybrid, 11, 22, 33)
    saved_sp = context.data.pointer
    for _ in range(3):
        context.data.pop()
    context.returns.push(0x1234)
    context.returns.push(0x5678)
    saved_rp = context.returns.capture_pointer()
    context.returns.pop()
    context.returns.pop()
    before_returns = _return_state(hybrid, context)
    context.data.push(7)

    hybrid.execute_xt(word.xt)

    assert context.data.snapshot() == (8,)
    assert _return_state(hybrid, context) == before_returns
    context.data.set_pointer(saved_sp)
    context.returns.set_pointer(saved_rp)
    assert context.data.snapshot() == (8, 22, 33)
    assert context.returns.snapshot() == (0x1234, 0x5678)


def test_machine_call_keeps_live_loop_user_return_cells_and_continuation_identity(hybrid):
    word = hybrid.register_routine_v1(_image())
    observations = []

    def probe(context):
        before = _return_state(hybrid, context)
        hybrid.execute_xt(word.xt, context=context)
        after = _return_state(hybrid, context)
        observations.append((before, after))

    hybrid.semantic.define_primitive("PROBE", probe)
    report = hybrid.evaluate(
        b": INNER RP@ DROP PROBE ; "
        b": RUN 90 >R 0 3 0 DO INNER LOOP R> + ; RUN"
    )

    assert hybrid.semantic.main_context.data.snapshot() == (93,)
    assert hybrid.semantic.main_context.returns.snapshot() == ()
    assert len(observations) == 3
    assert all(before == after for before, after in observations)
    assert all(before[4] > 0 for before, _ in observations)
    assert report.machine_instructions == 6
    assert report.transitions == 3


def test_buffer_writes_are_shared_across_semantic_machine_and_host_access(hybrid):
    word = hybrid.register_routine_v1(_image(
        name="H-BUMP",
        program="ldn r6, r4\ninc r6\nstr r4, r6\nmov r4, r6\nret.l",
        inputs=2, outputs=1, buffers=(_buffer(element_bytes=8),),
    ))
    semantic = hybrid.semantic
    semantic.memory.write64(EXTERNAL_BASE, 41)
    _push(hybrid, EXTERNAL_BASE, 1)
    report = hybrid.execute_xt(word.xt)
    assert semantic.main_context.data.snapshot() == (42,)
    assert semantic.memory.read64(EXTERNAL_BASE) == 42
    assert report.machine_instructions == 5

    semantic.main_context.data.clear()
    hybrid.evaluate(f"99 {EXTERNAL_BASE} !".encode("ascii"))
    _push(hybrid, EXTERNAL_BASE, 1)
    hybrid.execute_xt(word.xt)
    assert semantic.main_context.data.snapshot() == (100,)
    assert semantic.memory.read64(EXTERNAL_BASE) == 100
    view = semantic.memory.dense_backing.buffer_at(EXTERNAL_BASE)
    try:
        assert int.from_bytes(view[:8], "little") == 100
    finally:
        view.release()


def test_failure_preserves_inputs_and_completed_shared_store_prefix(hybrid):
    word = hybrid.register_routine_v1(_image(
        name="H-PREFIX",
        program="ldi r6, 165\nst.b r4, r6\naddi r4, 8\nstr r4, r6\nret.l",
        inputs=2, outputs=0, buffers=(_buffer(),),
    ))
    context = _push(hybrid, 0xCAFE, EXTERNAL_BASE, 1)
    context.returns.push(77)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    error = caught.value
    assert isinstance(error, ExecutionError)
    assert error.result.exit_kind == "rejected_access"
    assert error.result.instructions == 3
    assert error.result.outputs == ()
    assert error.result.access_address == EXTERNAL_BASE + 8
    assert context.data.snapshot() == (0xCAFE, EXTERNAL_BASE, 1)
    assert context.returns.snapshot() == (77,)
    assert context.reusable
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 16) == b"\xa5" + bytes(15)


@pytest.mark.parametrize("access,program", (
    ("read", "st.b r4, r5\nret.l"),
    ("write", "ld.b r6, r4\nret.l"),
))
def test_buffer_permissions_are_checked_before_machine_data_effects(hybrid, access, program):
    word = hybrid.register_routine_v1(_image(
        program=program, inputs=2, outputs=0, buffers=(_buffer(access=access),),
    ))
    context = _push(hybrid, EXTERNAL_BASE, 8)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.result.exit_kind == "rejected_access"
    assert caught.value.result.instructions == 0
    assert context.data.snapshot() == (EXTERNAL_BASE, 8)
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == bytes(8)


@pytest.mark.parametrize("target", ("inactive_data", "inactive_return", "header", "code", "control", "mmio"))
def test_protected_allocations_cannot_be_borrowed_even_when_inactive(hybrid, target):
    word = hybrid.register_routine_v1(_image(
        program="ret.l", inputs=2, outputs=0, buffers=(_buffer(),),
    ))
    declaration = hybrid.declaration_for(word)
    context = hybrid.semantic.main_context
    address = {
        "inactive_data": context.data.floor + 64,
        "inactive_return": context.returns.floor + 64,
        "header": word.header_address,
        "code": declaration.code_base,
        "control": declaration.stack_base,
        "mmio": MMIO_BASE + 0x780,
    }[target]
    _push(hybrid, address, 1)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.reason
    assert caught.value.result is None
    assert context.data.snapshot() == (address, 1)
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize("address,count", (
    (EXTERNAL_BASE, MASK64),
    (EXTERNAL_BASE + EXTERNAL_SIZE - 4, 1),
    (MASK64 - 3, 1),
    (EXTERNAL_BASE, EXTERNAL_SIZE // 8 + 1),
))
def test_buffer_length_geometry_and_overflow_preflight_preserve_inputs(hybrid, address, count):
    word = hybrid.register_routine_v1(_image(
        program="ret.l", inputs=2, outputs=0,
        buffers=(_buffer(element_bytes=8),),
    ))
    context = _push(hybrid, address, count)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.result is None
    assert context.data.snapshot() == (address, count)
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, EXTERNAL_SIZE) == bytes(EXTERNAL_SIZE)


def test_unsupported_machine_fp_does_not_enter_semantic_fp_or_fault_callback(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="fadd.d r4, r5\nret.l", inputs=2, outputs=1,
    ))
    semantic = hybrid.semantic
    semantic.scalar_float.write_fpcsr(7)
    fault_calls = []
    fault = semantic.define_primitive("FAULT-PROBE", lambda context: fault_calls.append(True))
    semantic.set_fault_xt(fault.xt)
    context = _push(hybrid, 0x3FF0000000000000, 0x4000000000000000)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.result.exit_kind == "unsupported_instruction"
    assert caught.value.result.instructions == 0
    assert semantic.scalar_float.fpcsr == 7
    assert fault_calls == []
    assert semantic.uart_output == b""
    assert context.data.snapshot() == (0x3FF0000000000000, 0x4000000000000000)


def test_raw_code_mutation_is_rejected_before_cached_machine_entry(hybrid):
    word = hybrid.register_routine_v1(_image())
    declaration = hybrid.declaration_for(word)
    context = _push(hybrid, 5)
    hybrid.execute_xt(word.xt)
    assert context.data.snapshot() == (6,)
    hybrid.semantic.memory.write8(declaration.code_base, 0x01)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.reason == "stale_code"
    assert caught.value.result is None
    assert context.data.snapshot() == (6,)
    assert hybrid.semantic.dictionary.is_body_lease_live(declaration.allocation_lease)


def test_backward_allot_revokes_registration_even_with_original_bytes_and_here(hybrid):
    word = hybrid.register_routine_v1(_image())
    declaration = hybrid.declaration_for(word)
    original_here = hybrid.semantic.dictionary.here
    hybrid.evaluate(b"-1 ALLOT 1 ALLOT")
    assert hybrid.semantic.dictionary.here == original_here
    assert hybrid.semantic.dictionary.resolve(word.xt) is word
    assert hybrid.semantic.memory.read_bytes(
        declaration.code_base, declaration.code_size,
    ) == declaration.code
    context = _push(hybrid, 7)

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.reason == "stale_registration"
    assert caught.value.result is None
    assert context.data.snapshot() == (7,)


def test_reused_xt_and_identical_code_receive_fresh_registration_and_publication(hybrid):
    dictionary = hybrid.semantic.dictionary
    checkpoint = dictionary.checkpoint()
    image = _image()
    original = hybrid.register_routine_v1(image)
    old_declaration = hybrid.declaration_for(original)
    _push(hybrid, 1)
    hybrid.execute_xt(original.xt)
    dictionary.rollback(checkpoint)
    replacement = hybrid.register_routine_v1(image)
    new_declaration = hybrid.declaration_for(replacement)

    assert replacement.xt == original.xt
    assert replacement is not original
    assert new_declaration.code == old_declaration.code
    assert new_declaration.allocation_generation > old_declaration.allocation_generation
    assert new_declaration.registration_nonce is not old_declaration.registration_nonce
    assert not dictionary.is_body_lease_live(old_declaration.allocation_lease)
    assert dictionary.is_body_lease_live(new_declaration.allocation_lease)
    hybrid.execute_xt(replacement.xt)
    assert hybrid.semantic.main_context.data.snapshot() == (3,)


def test_semantic_and_machine_counters_remain_distinct(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="nop\nnop\nnop\ninc r4\nret.l",
    ))
    semantic = hybrid.semantic
    _push(hybrid, 1)
    before_steps = semantic.diagnostics.semantic_cycles
    before_timer = semantic.timer.counter
    report = hybrid.execute_xt(word.xt)

    assert report.machine_instructions == 5
    assert report.transitions == 1
    assert report.semantic_result.semantic_steps == 1
    assert semantic.diagnostics.semantic_cycles - before_steps == 1
    assert semantic.timer.counter - before_timer == 1


def test_input_underflow_precedes_machine_entry_and_preserves_existing_cells(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="st.b r4, r5\nret.l", inputs=2, outputs=0,
        buffers=(_buffer(),),
    ))
    context = _push(hybrid, EXTERNAL_BASE)
    with pytest.raises((StackUnderflow, HybridExecutionError)) as caught:
        hybrid.execute_xt(word.xt)

    if isinstance(caught.value, HybridExecutionError):
        assert caught.value.result is None
    assert context.data.snapshot() == (EXTERNAL_BASE,)
    assert hybrid.semantic.memory.read_bytes(EXTERNAL_BASE, 8) == bytes(8)


def test_output_capacity_preflight_preserves_custom_context_and_shared_bytes(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="ldi r6, 165\nst.b r4, r6\nret.l", inputs=2, outputs=3,
        buffers=(_buffer(),),
    ))
    memory = hybrid.semantic.memory
    context = ExecutionContext(
        data=DataStack(
            (EXTERNAL_BASE, 1), memory=memory,
            floor=EXTERNAL_BASE + 0x800, empty_pointer=EXTERNAL_BASE + 0x810,
        ),
        returns=ReturnStack(
            memory=memory,
            floor=EXTERNAL_BASE + 0x900, empty_pointer=EXTERNAL_BASE + 0x980,
        ),
    )
    before_sp = context.data.pointer
    with pytest.raises((StackOverflow, HybridExecutionError)) as caught:
        hybrid.execute_xt(word.xt, context=context)

    if isinstance(caught.value, HybridExecutionError):
        assert caught.value.result is None
    assert context.data.snapshot() == (EXTERNAL_BASE, 1)
    assert context.data.pointer == before_sp
    assert context.returns.snapshot() == ()
    assert memory.read_bytes(EXTERNAL_BASE, 8) == bytes(8)
    assert hybrid.semantic.main_context.data.snapshot() == ()


def test_nested_call_protects_inactive_stack_allocation_of_enclosing_context(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="st.b r4, r5\nret.l", inputs=2, outputs=0,
        buffers=(_buffer(),),
    ))
    memory = hybrid.semantic.memory
    outer = ExecutionContext(
        data=DataStack(
            memory=memory,
            floor=EXTERNAL_BASE + 0x800, empty_pointer=EXTERNAL_BASE + 0x840,
        ),
        returns=ReturnStack(
            memory=memory,
            floor=EXTERNAL_BASE + 0x900, empty_pointer=EXTERNAL_BASE + 0x980,
        ),
    )
    inactive_slot = outer.data.floor + 8
    memory.write8(inactive_slot, 0xA5)

    def enter_main_context(_context):
        _push(hybrid, inactive_slot, 1)
        hybrid.execute_xt(word.xt, context=hybrid.semantic.main_context)

    probe = hybrid.semantic.define_primitive("NESTED-CONTEXT", enter_main_context)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(probe.xt, context=outer)

    assert caught.value.result is None
    assert memory.read8(inactive_slot) == 0xA5
    assert outer.data.snapshot() == outer.returns.snapshot() == ()
    assert hybrid.semantic.main_context.data.snapshot() == (inactive_slot, 1)


def test_per_call_instruction_failure_does_not_publish_partial_register_outputs(hybrid):
    word = hybrid.register_routine_v1(_image(
        program="inc r4\ninc r4\nret.l", max_instructions=2,
    ))
    context = _push(hybrid, 7)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(word.xt)

    assert caught.value.reason == "instruction_limit"
    assert caught.value.result.exit_kind == "instruction_limit"
    assert caught.value.result.instructions == 2
    assert caught.value.result.outputs == ()
    assert context.data.snapshot() == (7,)


def test_return_on_final_allowed_machine_instruction_succeeds(hybrid):
    word = hybrid.register_routine_v1(_image(max_instructions=2))
    context = _push(hybrid, 7)
    report = hybrid.execute_xt(word.xt, machine_instruction_limit=2)
    assert report.machine_instructions == 2
    assert context.data.snapshot() == (8,)


@pytest.mark.parametrize("limit,error", ((0, ValueError), (True, TypeError), (1001, ValueError)))
def test_wrapper_budget_preflight_can_only_lower_session_allowance(hybrid, limit, error):
    word = hybrid.register_routine_v1(_image())
    context = _push(hybrid, 7)
    with pytest.raises(error):
        hybrid.execute_xt(word.xt, machine_instruction_limit=limit)
    assert context.data.snapshot() == (7,)


@pytest.mark.parametrize("nested_surface", ("hybrid", "semantic"))
def test_nested_evaluation_cannot_renew_outer_machine_allowance(hybrid, nested_surface):
    hybrid.register_routine_v1(_image())
    delegate = hybrid if nested_surface == "hybrid" else hybrid.semantic

    def nested(context):
        delegate.evaluate(b"H-INC H-INC", context=context)

    hybrid.semantic.define_primitive("NESTED", nested)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.evaluate(b"1 H-INC NESTED", machine_instruction_limit=4)

    assert caught.value.reason == "instruction_limit"
    assert caught.value.result is None or caught.value.result.instructions == 0
    assert hybrid.semantic.main_context.data.snapshot() == (3,)
    assert hybrid.semantic.main_context.returns.snapshot() == ()


def test_machine_allowance_survives_host_quanta_and_reports_count_each_slice(hybrid):
    hybrid.register_routine_v1(_image())
    hybrid.evaluate(b": THREE H-INC H-INC H-INC ;")
    context = _push(hybrid, 10)
    completed_instructions = completed_transitions = yields = 0

    with pytest.raises(HybridExecutionError) as caught:
        report = hybrid.run("THREE", quantum_steps=1, machine_instruction_limit=4)
        for _ in range(20):
            completed_instructions += report.machine_instructions
            completed_transitions += report.transitions
            assert isinstance(report.semantic_result, YieldedExecution)
            yields += 1
            report = hybrid.resume_yielded(report.semantic_result.suspension)
        pytest.fail("machine allowance was renewed by a host quantum")

    assert caught.value.reason == "instruction_limit"
    assert completed_instructions == 4
    assert completed_transitions == 2
    assert yields >= 2
    assert context.data.snapshot() == (12,)
    assert context.returns.snapshot() == ()
    assert not context.suspended

    # A later outer dispatch has its own allowance after the failed one settles.
    report = hybrid.execute("H-INC", machine_instruction_limit=2)
    assert report.machine_instructions == 2
    assert context.data.snapshot() == (13,)


@pytest.mark.parametrize("executor", ("python", "native"))
def test_raw_semantic_quantum_entry_still_enforces_configured_machine_allowance(executor):
    runtime = _create(executor, dispatch_instruction_limit=4)
    try:
        runtime.register_routine_v1(_image())
        runtime.semantic.evaluate(b": THREE H-INC H-INC H-INC ;")
        context = _push(runtime, 10)
        with pytest.raises(HybridExecutionError) as caught:
            result = runtime.semantic.run_until_blocked("THREE", quantum_steps=1)
            for _ in range(20):
                assert isinstance(result, YieldedExecution)
                result = runtime.semantic.resume_yielded(result.suspension)
            pytest.fail("raw semantic entry bypassed the configured machine allowance")

        assert caught.value.reason == "instruction_limit"
        assert context.data.snapshot() == (12,)
        assert context.returns.snapshot() == ()
    finally:
        runtime.close()


def test_close_revokes_raw_semantic_machine_entry_and_is_idempotent(hybrid):
    word = hybrid.register_routine_v1(_image())
    context = _push(hybrid, 7)
    hybrid.close()
    hybrid.close()

    with pytest.raises(HybridExecutionError) as caught:
        hybrid.semantic.execute(word.xt)

    assert caught.value.result is None
    assert context.data.snapshot() == (7,)
    with pytest.raises(HybridExecutionError):
        hybrid.register_routine_v1(_image(name="LATE"))


def test_existing_sparse_runtime_storage_is_not_silently_converted():
    memory = create_one_core_address_space(bank0_size=65536, external_size=EXTERNAL_SIZE)
    memory.write64(EXTERNAL_BASE, 0xA5)
    with pytest.raises((TypeError, ValueError)):
        HybridRuntime.create(executor="python", memory=memory)
    assert memory.dense_backing is None
    assert memory.read64(EXTERNAL_BASE) == 0xA5


@pytest.mark.parametrize("executor", ("python", "native"))
def test_documented_host_api_runs_checksum_over_semantically_written_shared_cells(executor):
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    runtime = HybridRuntime.create(
        geometry={"bank0_size": 65536, "external_size": EXTERNAL_SIZE},
        semantic_executor=executor,
        dispatch_instruction_limit=1_000_000,
    )
    try:
        word = runtime.register_routine_v1(
            name="HYB-CHECKSUM",
            code=bytes(assemble("""
                mov r6, r4
                ldi r4, 0
            again:
                ldn r7, r6
                add r4, r7
                addi r6, 8
                subi r5, 1
                brne again
                ret.l
            """)),
            entry_offset=0,
            input_cells=2,
            output_cells=1,
            buffers=(BufferRuleV1(
                address_argument=0, length_argument=1,
                element_bytes=8, max_bytes=65536, access="read",
            ),),
            max_instructions=65536,
            return_stack_cells=128,
        )
        runtime.semantic.define_constant("SAMPLES", EXTERNAL_BASE)

        report = runtime.evaluate(
            "3 SAMPLES ! 5 SAMPLES 8 + ! 7 SAMPLES 16 + ! "
            "SAMPLES 3 HYB-CHECKSUM",
            semantic_step_budget=1000,
            machine_instruction_budget=100000,
        )

        expected_bytes = b"".join(value.to_bytes(8, "little") for value in (3, 5, 7))
        assert runtime.semantic.find("HYB-CHECKSUM") is word
        assert runtime.executor == executor
        assert runtime.semantic.main_context.data.snapshot() == (15,)
        assert runtime.semantic.main_context.returns.snapshot() == ()
        assert runtime.semantic.memory.read_bytes(EXTERNAL_BASE, 24) == expected_bytes
        backing_view = runtime.semantic.memory.dense_backing.buffer_at(EXTERNAL_BASE)
        try:
            assert bytes(backing_view[:24]) == expected_bytes
        finally:
            backing_view.release()
        assert report.machine_instructions == 18
        assert report.transitions == 1
        assert 0 < report.semantic_result.semantic_steps <= 1000
    finally:
        runtime.close()


@pytest.mark.parametrize("stack_name", ("data", "returns"))
@pytest.mark.parametrize("first_use", ("semantic_wrapper", "machine_wrapper"))
def test_previously_used_custom_stack_allocation_stays_protected_between_dispatches(
    hybrid, stack_name, first_use,
):
    memory = hybrid.semantic.memory
    retained = ExecutionContext(
        data=DataStack(
            memory=memory,
            floor=EXTERNAL_BASE + 0x800, empty_pointer=EXTERNAL_BASE + 0x840,
        ),
        returns=ReturnStack(
            memory=memory,
            floor=EXTERNAL_BASE + 0x900, empty_pointer=EXTERNAL_BASE + 0x980,
        ),
    )
    if first_use == "semantic_wrapper":
        first = hybrid.semantic.define_constant("CONTEXT-SEEN", 7)
        hybrid.execute_xt(first.xt, context=retained)
        assert retained.data.pop() == 7
    else:
        first = hybrid.register_routine_v1(_image(
            name="H-NOOP", program="ret.l", inputs=0, outputs=0,
        ))
        hybrid.execute_xt(first.xt, context=retained)

    # The original context is still live but no longer active. Entire stack
    # allocations remain reserved, including slots below the current pointer.
    inactive_slot = getattr(retained, stack_name).floor + 8
    original_bytes = bytes.fromhex("1122334455667788")
    memory.write_bytes(inactive_slot, original_bytes)
    writer = hybrid.register_routine_v1(_image(
        name="H-WRITE", program="st.b r4, r5\nret.l",
        inputs=2, outputs=0, buffers=(_buffer(),),
    ))
    main = _push(hybrid, inactive_slot, 1)
    with pytest.raises(HybridExecutionError) as caught:
        hybrid.execute_xt(writer.xt)

    assert caught.value.result is None
    assert memory.read_bytes(inactive_slot, 8) == original_bytes
    assert retained.data.snapshot() == retained.returns.snapshot() == ()
    assert main.data.snapshot() == (inactive_slot, 1)

    # Reserving one context's allocation must not reserve unrelated external data.
    main.data.clear()
    _push(hybrid, EXTERNAL_BASE, 1)
    report = hybrid.execute_xt(writer.xt)
    assert report.machine_instructions == 2
    assert memory.read8(EXTERNAL_BASE) == 1
    assert memory.read_bytes(inactive_slot, 8) == original_bytes


def test_compiled_hybrid_buffer_routine_matches_standalone_mp64_execution(hybrid):
    word = hybrid.register_routine_v1(_image(
        name="H-BUMP-SUM",
        program="""
            mov r6, r4
            ldi r4, 0
        again:
            ldn r7, r6
            add r4, r7
            inc r7
            str r6, r7
            addi r6, 8
            subi r5, 1
            brne again
            ret.l
        """,
        inputs=2, outputs=1, buffers=(_buffer(element_bytes=8),),
    ))
    hybrid.evaluate(b": COMPILED-BUMP H-BUMP-SUM ;")
    declaration = hybrid.declaration_for(word)
    memory = hybrid.semantic.memory
    original_cells = (3, 5, 7)
    memory.write_bytes(
        EXTERNAL_BASE,
        b"".join(value.to_bytes(8, "little") for value in original_cells),
    )
    arguments = (EXTERNAL_BASE, len(original_cells))
    context = _push(hybrid, 0xCAFE, *arguments)

    # The ordinary interpreter uses independent storage at the exact same
    # numerical code, data, and private stack addresses. Only this reference
    # merges the external data and control spans into one architectural region.
    reference = native.CPUState()
    bank0 = next(region for region in memory.regions if region.base == 0)
    ram = bytearray(memory.read_bytes(0, bank0.size))
    external = bytearray(declaration.stack_base + declaration.stack_size - EXTERNAL_BASE)
    external[:EXTERNAL_SIZE] = memory.read_bytes(EXTERNAL_BASE, EXTERNAL_SIZE)
    reference.attach_mem(ram, len(ram))
    reference.attach_ext_mem(external, EXTERNAL_BASE, len(external))
    reference.icache_control_write(1)
    reference.psel, reference.xsel, reference.spsel = 3, 2, 15
    reference.set_reg(3, declaration.entry_pc)
    root_slot = declaration.stack_base + declaration.stack_size - 8
    reference.set_reg(15, root_slot)
    root_offset = root_slot - EXTERNAL_BASE
    external[root_offset:root_offset + 8] = MASK64.to_bytes(8, "little")
    for index, value in enumerate(arguments):
        reference.set_reg(4 + index, value)

    def unexpected_device(*_arguments):
        pytest.fail("the ordinary buffer routine reached a device callback")

    instructions = cycles = 0
    while reference.get_reg(3) != MASK64:
        assert instructions < declaration.max_instructions, "ordinary routine did not return"
        cycles += native.step_one(
            reference,
            mmio_read8=unexpected_device,
            mmio_write8=unexpected_device,
            on_output=unexpected_device,
            csr_read_override=None,
            mmio_start=MMIO_BASE,
            mmio_end=MMIO_LIMIT,
        )
        instructions += 1

    report = hybrid.execute("COMPILED-BUMP")

    assert reference.get_reg(4) == sum(original_cells) == 15
    assert context.data.snapshot() == (0xCAFE, reference.get_reg(4))
    assert context.returns.snapshot() == ()
    assert memory.read_bytes(EXTERNAL_BASE, EXTERNAL_SIZE) == bytes(external[:EXTERNAL_SIZE])
    assert tuple(memory.read64(EXTERNAL_BASE + index * 8) for index in range(3)) == (4, 6, 8)
    assert report.machine_instructions == instructions == 24
    assert report.machine_cycles == cycles
    assert report.transitions == 1
