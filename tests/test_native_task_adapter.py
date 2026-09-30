"""Real task transport publication and ordinary semantic stack integration."""

from dataclasses import replace
from pathlib import Path
import sys

import pytest

native = pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridRuntime
from hybrid.task_adapter import NativeTaskAdapter
from shared.cells import u64
from shared.foreign_abi import ForeignBudgetV1, ForeignSignatureV1, ForeignSpanV1
from shared.hybrid_abi import RoutineImageV1
from simulator.errors import ExecutionError
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_runtime import ForeignTaskBudgetExceeded, ForeignTaskError
from simulator.ir import Call, Literal, Return
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import YieldedExecution


EXCEPTIONS = Path(__file__).parent / "simulator" / "fixtures" / "kdos-exceptions-618-675.f"


@pytest.fixture(params=("python", "native"))
def owner(request):
    if getattr(native, "_TASK_ROUTINE_TRANSPORT_REVISION", 0) < 2:
        pytest.skip("native task transport revision 2 is required")
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    result = HybridRuntime.create(executor=request.param,
                                  geometry={"bank0_size": 65536, "external_size": 65536})
    # Core installation keeps its Bank-0 layout. Subsequent semantic bodies and
    # sealed machine code use an external dictionary disjoint from both full
    # main-stack allocations; the first external page remains ordinary data.
    runtime = result.semantic
    dictionary_base = EXTERNAL_BASE + 0x1000
    runtime.configure_dictionary_bounds(dictionary_base, EXTERNAL_BASE + 0x10000,
                                        runtime.main_context)
    runtime.allot_dictionary(dictionary_base - runtime.dictionary.here, runtime.main_context)
    adapter = NativeTaskAdapter(result)
    try:
        yield result, adapter
    finally:
        adapter.close()


def signature(inputs=0, outputs=0):
    return ForeignSignatureV1(input_cells=inputs, output_cells=outputs)


def task_grants(runtime):
    context = runtime.main_context
    result = tuple(ForeignSpanV1(base=stack.empty_pointer - 2048, size=2048,
                                 access="read_write")
                   for stack in (context.data, context.returns))
    handlers = runtime.find("_TASK-HANDLERS")
    if handlers is not None:
        result += (ForeignSpanV1(base=handlers.body_address, size=8, access="read_write"),)
    return result


def callback_code(*, prefix=""):
    # PC-relative addressing survives the adapter's dictionary allocation.
    source = prefix + "mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\nret.l\nstub:\nret.l"
    labels = {}
    assemble(source, labels_out=labels)
    source = source.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
    return bytes(assemble(source)), labels["call"], labels["stub"]


def register_simple(adapter, name="MACHINE", *, source="inc r4\nret.l", inputs=1,
                    outputs=1, grants=(), instructions=100):
    with adapter.registration_batch() as batch:
        return batch.define_operation(name, bytes(assemble(source)), signature(inputs, outputs),
            machine_grants=grants, max_instructions=instructions, max_callbacks=0)


def register_callback(owner, adapter, target="DUP", *, name="MACHINE", inputs=1,
                      outputs=2, prefix="", grants=(), dynamic=(), fault=None):
    code, call, stub = callback_code(prefix=prefix)
    with adapter.registration_batch() as batch:
        word = batch.define_operation(name, code, signature(inputs, outputs),
            machine_grants=grants, max_instructions=100, max_callbacks=1)
        export = batch.capture_export(target, signature(inputs, outputs),
            task_grants=task_grants(owner.semantic), dynamic_targets=dynamic, fault_target=fault)
        batch.set_callbacks(word, ((call, stub, export),))
    return word, export


def machine_snapshot(owner):
    cpu = owner._cpu
    return (tuple(cpu.get_reg(index) for index in range(32)), cpu.flags_pack(),
            cpu.cycle_count, cpu.icache_hits, cpu.icache_misses, cpu.icache_snapshot(),
            bytes(owner._control_buffer))


def assert_idle(owner, adapter):
    context = owner.semantic.main_context
    assert adapter.active_invocations == ()
    assert context.returns.snapshot() == ()
    assert context.returns.pointer == context.returns.empty_pointer
    assert context.reusable


def test_real_callback_uses_original_stack_owners_and_exact_native_cycle_receipt(owner):
    hybrid, adapter = owner
    runtime, cpu = hybrid.semantic, hybrid._cpu
    context = runtime.main_context
    original_data, original_returns = context.data, context.returns
    word, _ = register_callback(hybrid, adapter)
    operation = adapter.operation(word)
    assert operation.signature == signature(1, 2)
    context.data.push(99)
    context.data.push(7)
    before_cycles = cpu.cycle_count
    runtime.execute(word.xt)
    assert context.data is original_data and context.returns is original_returns
    assert context.data.snapshot() == (99, 7, 7)
    assert runtime.memory.read_bytes(context.data.pointer, 24) == b"".join(
        value.to_bytes(8, "little") for value in (7, 7, 99))
    receipt = adapter.last_receipt()
    report = runtime._foreign_tasks.last_dispatch
    assert receipt.state == "returned" and receipt.root_entries == 1
    assert receipt.root_instructions == report.machine_instructions == 5
    assert receipt.root_callbacks == report.callbacks == 1
    assert receipt.root_cycles == report.machine_cycles == cpu.cycle_count - before_cycles
    assert adapter.machine_instructions == 5 and adapter.machine_cycles == receipt.root_cycles
    assert adapter.callback_requests == 1
    assert report.semantic_steps == 1 and report.completed and not report.cancelled
    assert_idle(hybrid, adapter)


@pytest.mark.parametrize("delivery", (1, 2, 3, 4))
def test_native_result_delivery_failure_settles_prefix_before_cleanup_and_preserves_input_order(owner, delivery):
    hybrid, adapter = owner
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    word, _ = register_callback(hybrid, adapter)
    facade = hybrid._nested_runner.task_v1()
    facade._test_fail_marshalling_after_results(delivery)
    context.data.push(99)
    context.data.push(7)
    before = machine_snapshot(hybrid)
    with pytest.raises(MemoryError):
        runtime.execute(word.xt)
    receipt = adapter.last_receipt()
    report = runtime._foreign_tasks.last_dispatch
    expected_instructions = (0, 3, 3, 5)[delivery - 1]
    assert report.entries == receipt.root_entries == 1
    assert report.machine_instructions == receipt.root_instructions == expected_instructions
    assert report.machine_cycles == receipt.root_cycles
    assert report.callbacks == receipt.root_callbacks == int(delivery > 1)
    assert report.semantic_steps == int(delivery > 2)
    assert context.data.snapshot() == ((99, 7) if delivery == 1 else (99,))
    if delivery == 1:
        # Admission precedes both input consumption and native initialization.
        assert machine_snapshot(hybrid) == before
        assert receipt.invocation_started and receipt.instructions == receipt.cycles == 0
    assert report.cancelled and not report.completed
    assert_idle(hybrid, adapter)


@pytest.mark.parametrize("quantum", (None, 1))
@pytest.mark.parametrize("limit", ("instructions", "entries"))
def test_empty_native_chain_and_ordinary_quanta_retain_original_root_limits(owner, quantum, limit):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word = register_simple(adapter, source="ret.l", inputs=0, outputs=0)
    runtime.define_colon("TWICE", (Call(word.xt), Call(word.xt), Return()))
    runtime._foreign_tasks.configure_limits(**{
        "instruction_limit" if limit == "instructions" else "entry_limit": 1})
    with pytest.raises(ForeignTaskBudgetExceeded):
        current = runtime.run_until_blocked("TWICE", quantum_steps=quantum)
        for _ in range(20):
            assert type(current) is YieldedExecution
            assert adapter.active_invocations == ()
            current = runtime.resume_yielded(current.suspension)
        pytest.fail("a second task entry renewed the original root ceiling")
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.root_entries == report.entries == 1
    assert receipt.root_instructions == report.machine_instructions == 1
    assert receipt.root_callbacks == report.callbacks == 0
    assert_idle(hybrid, adapter)
    runtime.execute(word.xt)
    fresh = runtime._foreign_tasks.last_dispatch
    assert fresh.root_id > report.root_id and fresh.machine_instructions == fresh.entries == 1
    assert fresh.completed and not fresh.cancelled


def test_real_child_edge_returns_to_parked_parent_without_double_counting(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    code, call, stub = callback_code()
    with adapter.registration_batch() as batch:
        child = batch.define_operation("CHILD", bytes(assemble("inc r4\nret.l")),
            signature(1, 1), max_instructions=100, max_callbacks=0)
        policy = batch.define_colon("POLICY", (Call(child.xt), Return()))
        parent = batch.define_operation("PARENT", code, signature(1, 1),
            max_instructions=100, max_callbacks=1)
        export = batch.capture_export(policy, signature(1, 1), task_grants=task_grants(runtime))
        batch.set_callbacks(parent, ((call, stub, export),))
    before_cycles = hybrid._cpu.cycle_count
    runtime.main_context.data.push(99)
    runtime.main_context.data.push(41)
    runtime.execute(parent.xt)
    assert runtime.main_context.data.snapshot() == (99, 42)
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.invocation_instructions == 5
    assert receipt.root_instructions == report.machine_instructions == 7
    assert receipt.root_entries == report.entries == 2
    assert receipt.root_callbacks == report.callbacks == 1
    assert receipt.root_cycles == hybrid._cpu.cycle_count - before_cycles
    assert receipt.parent_invocation_id is None and receipt.depth == 1
    assert_idle(hybrid, adapter)


def test_cyclic_static_publication_is_allowed_but_active_registration_recursion_is_rejected(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    code, call, stub = callback_code()
    with adapter.registration_batch() as batch:
        first = batch.define_operation("FIRST", code, signature(),
            max_instructions=100, max_callbacks=1)
        second = batch.define_operation("SECOND", code, signature(),
            max_instructions=100, max_callbacks=1)
        to_second = batch.define_colon("TO-SECOND", (Call(second.xt), Return()))
        to_first = batch.define_colon("TO-FIRST", (Call(first.xt), Return()))
        export_second = batch.capture_export(to_second, signature(), task_grants=task_grants(runtime))
        export_first = batch.capture_export(to_first, signature(), task_grants=task_grants(runtime))
        batch.set_callbacks(first, ((call, stub, export_second),))
        batch.set_callbacks(second, ((call, stub, export_first),))
    assert adapter.operation(first) is not adapter.operation(second)
    with pytest.raises((ForeignTaskError, ValueError, RuntimeError)):
        runtime.execute(first.xt)
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.root_entries == report.entries == 2
    assert receipt.root_instructions == report.machine_instructions == 6
    assert receipt.root_callbacks == report.callbacks == 2
    assert report.cancelled and not report.completed
    assert_idle(hybrid, adapter)


def test_host_aborted_batch_restores_words_exports_and_shared_control_reservation(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    before = (runtime.dictionary.here, tuple(runtime.dictionary.words),
              tuple(runtime._foreign_tasks._exports), hybrid._control_used)
    failure = RuntimeError("abort host registration")
    with pytest.raises(RuntimeError) as caught:
        with adapter.registration_batch() as batch:
            discarded = batch.define_operation("DISCARDED", bytes(assemble("ret.l")),
                signature(), max_instructions=10, max_callbacks=0)
            batch.capture_export("DUP", signature(1, 2), task_grants=task_grants(runtime))
            raise failure
    assert caught.value is failure
    assert (runtime.dictionary.here, tuple(runtime.dictionary.words),
            tuple(runtime._foreign_tasks._exports), hybrid._control_used) == before
    assert runtime.find("DISCARDED") is None
    with pytest.raises((ForeignTaskError, ValueError, RuntimeError)):
        adapter.operation(discarded)
    live = register_simple(adapter, source="ret.l", inputs=0, outputs=0)
    runtime.execute(live.xt)
    assert adapter.last_receipt().root_entries == 1
    assert_idle(hybrid, adapter)


def test_real_machine_store_prefix_survives_denied_access_and_task_cancellation(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word = register_simple(adapter, source="ldi r6, 90\nst.b r4, r6\naddi r4, 8\nstr r4, r6\nret.l",
        inputs=1, outputs=0, grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    runtime.main_context.data.push(99)
    runtime.main_context.data.push(EXTERNAL_BASE)
    with pytest.raises(ForeignTaskError) as caught:
        runtime.execute(word.xt)
    assert caught.value.event.kind == "rejected_access"
    assert runtime.memory.read8(EXTERNAL_BASE) == 90
    assert runtime.memory.read64(EXTERNAL_BASE + 8) == 0
    assert runtime.main_context.data.snapshot() == (99,)
    assert adapter.last_receipt().root_instructions == 3
    assert runtime._foreign_tasks.last_dispatch.machine_instructions == 3
    assert_idle(hybrid, adapter)


def test_sealed_stub_entry_maps_native_profile_failure_after_zero_work_admission(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    code, call, stub = callback_code()
    with adapter.registration_batch() as batch:
        word = batch.define_operation("STUB-ENTRY", code, signature(1, 2), entry_offset=stub,
            max_instructions=100, max_callbacks=1)
        export = batch.capture_export("DUP", signature(1, 2), task_grants=task_grants(runtime))
        batch.set_callbacks(word, ((call, stub, export),))
    runtime.main_context.data.push(99)
    runtime.main_context.data.push(7)
    with pytest.raises(ForeignTaskError) as caught:
        runtime.execute(word.xt)
    assert caught.value.event.kind == "profile_rejected"
    assert type(caught.value.event.instruction_pc) is int
    assert runtime.main_context.data.snapshot() == (99,)
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.state == "failed" and receipt.root_entries == report.entries == 1
    assert receipt.root_instructions == receipt.root_cycles == report.machine_instructions == 0
    assert receipt.root_callbacks == report.callbacks == 0
    assert_idle(hybrid, adapter)


def test_real_throw_cancels_child_suffix_and_parent_request_then_reaches_outer_catch(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    runtime.evaluate(EXCEPTIONS.read_bytes(), source_name=str(EXCEPTIONS))
    runtime.evaluate(b": RAISE -17 THROW ;")
    child, _ = register_callback(hybrid, adapter, "RAISE", name="CHILD", inputs=0, outputs=0)
    runtime.define_colon("POLICY", (Call(child.xt), Return()))
    parent, _ = register_callback(hybrid, adapter, "POLICY", name="PARENT", inputs=0, outputs=0)
    runtime.evaluate(b": OUTER 99 ['] PARENT CATCH 5 ;")
    runtime.execute("OUTER")
    assert runtime.main_context.data.snapshot() == (99, u64(-17), 5)
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.root_entries == report.entries == 2
    assert receipt.root_instructions == report.machine_instructions == 6
    assert receipt.root_callbacks == report.callbacks == 2
    assert report.completed and report.cancelled
    assert runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address) == 0
    assert_idle(hybrid, adapter)


@pytest.mark.parametrize("fail_delivery", (False, True))
def test_child_throw_retires_only_its_suffix_and_retains_parent_reply_authority(owner, fail_delivery):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    runtime.evaluate(EXCEPTIONS.read_bytes(), source_name=str(EXCEPTIONS))
    runtime.evaluate(b": RAISE -17 THROW ;")
    code, call, stub = callback_code()
    with adapter.registration_batch() as batch:
        child = batch.define_operation("CHILD", code, signature(),
            max_instructions=100, max_callbacks=1)
        raising = batch.capture_export("RAISE", signature(), task_grants=task_grants(runtime))
        batch.set_callbacks(child, ((call, stub, raising),))
        policy = batch.define_colon("POLICY", (
            Literal(child.xt), Call(runtime.find("CATCH").xt), Return()))
        parent = batch.define_operation("PARENT", code, signature(0, 1),
            max_instructions=100, max_callbacks=1)
        catching = batch.capture_export(policy, signature(0, 1),
            task_grants=task_grants(runtime), dynamic_targets=(child,))
        batch.set_callbacks(parent, ((call, stub, catching),))
    runtime.main_context.data.push(99)
    if fail_delivery:
        adapter._runner._test_fail_next_cancel_delivery()
        with pytest.raises(MemoryError):
            runtime.execute(parent.xt)
        # Native retirement already happened before delivery failed. Recovery
        # must cancel the remaining parent, without replaying the retired child.
        assert adapter.active_invocations == ()
        assert not runtime.main_context.reusable
        assert runtime.main_context.data.snapshot() == (99, u64(-17))
    else:
        runtime.execute(parent.xt)
        assert runtime.main_context.data.snapshot() == (99, u64(-17))
        assert_idle(hybrid, adapter)
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.root_entries == report.entries == 2
    assert receipt.root_instructions == report.machine_instructions == (6 if fail_delivery else 8)
    assert receipt.root_callbacks == report.callbacks == 2
    assert report.cancelled
    assert report.completed is (not fail_delivery)


def test_late_invalid_callback_publication_rolls_back_native_code_and_dictionary_together(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    before = (runtime.dictionary.here, tuple(runtime.dictionary.words),
              tuple(runtime._foreign_tasks._exports), hybrid._control_used,
              hybrid._issued_code_bytes, hybrid._issued_child_edges)
    code, call, stub = callback_code()
    with pytest.raises((ValueError, ForeignTaskError)):
        with adapter.registration_batch() as batch:
            word = batch.define_operation("INVALID", code, signature(1, 2),
                max_instructions=100, max_callbacks=1)
            export = batch.capture_export("DUP", signature(1, 2), task_grants=task_grants(runtime))
            # Inside the two-byte CALL, rather than an instruction boundary.
            batch.set_callbacks(word, ((call + 1, stub, export),))
    assert (runtime.dictionary.here, tuple(runtime.dictionary.words),
            tuple(runtime._foreign_tasks._exports), hybrid._control_used,
            hybrid._issued_code_bytes, hybrid._issued_child_edges) == before
    valid, _ = register_callback(hybrid, adapter)
    runtime.main_context.data.push(7)
    runtime.execute(valid.xt)
    assert runtime.main_context.data.snapshot() == (7, 7)
    assert_idle(hybrid, adapter)


def test_shadowing_callback_and_machine_names_does_not_redirect_captured_authority(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    child = register_simple(adapter, "CHILD")
    runtime.define_colon("POLICY", (Call(child.xt), Return()))
    parent, _ = register_callback(hybrid, adapter, "POLICY", name="PARENT", inputs=1, outputs=1)
    runtime.define_colon("CHILD", (Literal(111), Return()))
    runtime.define_colon("POLICY", (Literal(222), Return()))
    runtime.main_context.data.push(41)
    runtime.execute(parent.xt)
    assert runtime.main_context.data.snapshot() == (42,)
    assert adapter.last_receipt().root_entries == 2
    assert adapter.last_receipt().root_instructions == 7
    assert_idle(hybrid, adapter)


def test_adapter_close_releases_shared_native_owner_and_is_idempotent(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word = register_simple(adapter)
    runtime.main_context.data.push(7)
    runtime.execute(word.xt)
    retained = adapter.last_receipt()
    facade, cpu, control = adapter._runner, hybrid._cpu, hybrid._control_buffer
    with pytest.raises(BufferError):
        control.extend(b"still pinned")
    adapter.close()
    adapter.close()
    assert adapter.active_invocations == ()
    assert adapter.last_receipt() is retained
    with pytest.raises((ForeignTaskError, RuntimeError)):
        adapter.operation(word)
    with pytest.raises(RuntimeError):
        facade.bind_root(99, 10, 0, entry_limit=1)
    # Closing this facade must release the common owner, including CPU mutation
    # exclusion and the control export retained by its other native views.
    control.extend(b"released")
    cpu.set_reg(4, 123)
    assert cpu.get_reg(4) == 123


@pytest.mark.parametrize("copy_operation", (False, True))
def test_idle_host_cannot_mint_task_root_or_copy_operation_authority(owner, copy_operation):
    hybrid, adapter = owner
    word = register_simple(adapter)
    operation = adapter.operation(word)
    if copy_operation:
        operation = replace(operation)
    before = machine_snapshot(hybrid)
    allowance = ForeignBudgetV1(invocation_instructions_remaining=100,
        root_instructions_remaining=100, invocation_callbacks_remaining=0,
        root_callbacks_remaining=0, quantum_instructions=0)
    with pytest.raises(ForeignTaskError):
        adapter.begin(operation, (7,), root_token=object(), root_id=1, budget=allowance)
    assert machine_snapshot(hybrid) == before
    assert adapter.last_receipt() is None and adapter.active_invocations == ()
    assert hybrid.semantic.main_context.data.snapshot() == ()


@pytest.mark.parametrize("stage", ("owner_projection", "event"))
def test_python_delivery_error_keeps_actual_native_return_receipt_and_exact_error(owner, stage):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word = register_simple(adapter)
    runtime.main_context.data.push(99)
    runtime.main_context.data.push(7)
    failure = KeyboardInterrupt("neutral task event delivery interrupted")
    fired = []
    target_code = (NativeTaskAdapter._project_owner if stage == "owner_projection"
                   else NativeTaskAdapter._event).__code__
    previous_trace = sys.gettrace()

    def interrupt_delivery(frame, event, argument):
        if (not fired and event == "call" and frame.f_code is target_code
                and frame.f_locals.get("self") is adapter):
            receipt = (adapter._settlement.receipt if stage == "owner_projection"
                       else frame.f_locals["receipt"])
            if receipt is not None and receipt.state == "returned":
                fired.append(True)
                raise failure
        return interrupt_delivery

    try:
        sys.settrace(interrupt_delivery)
        with pytest.raises(KeyboardInterrupt) as caught:
            runtime.execute(word.xt)
    finally:
        sys.settrace(previous_trace)
    assert fired == [True] and caught.value is failure
    receipt, report = adapter.last_receipt(), runtime._foreign_tasks.last_dispatch
    assert receipt.state == "returned"
    assert receipt.root_instructions == report.machine_instructions == adapter.machine_instructions == 2
    assert hybrid.machine_instructions == 2
    assert receipt.root_cycles == report.machine_cycles == adapter.machine_cycles
    assert receipt.root_entries == report.entries == 1
    assert runtime.main_context.data.snapshot() == (99,)
    assert report.cancelled and not report.completed
    assert_idle(hybrid, adapter)


@pytest.mark.parametrize("damage", ("sealed_bytes", "lease"))
def test_task_body_mutation_or_rollback_revokes_registration_before_machine_execution(owner, damage):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    checkpoint = runtime.dictionary.checkpoint()
    word = register_simple(adapter)
    before = machine_snapshot(hybrid)
    if damage == "sealed_bytes":
        address = word.body_address
        runtime.memory.write8(address, runtime.memory.read8(address) ^ 1)
    else:
        runtime.dictionary.rollback(checkpoint)
        runtime.dictionary_index.rebuild()
    if damage == "lease":
        with pytest.raises(KeyError) as caught:
            adapter.operation(word)
        assert caught.value.args == (f"unknown execution token 0x{word.xt:016x}",)
    else:
        with pytest.raises(ForeignTaskError):
            adapter.operation(word)
    assert machine_snapshot(hybrid) == before
    assert adapter.last_receipt() is None and adapter.active_invocations == ()


@pytest.mark.parametrize("task_first", (False, True))
def test_one_original_semantic_meter_cannot_mix_private_and_task_machine_profiles(owner, task_first):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    private = hybrid.register_routine_v1(RoutineImageV1(
        name="PRIVATE", code=bytes(assemble("ret.l")), entry_offset=0,
        input_cells=0, output_cells=0, buffers=(), return_stack_cells=16, max_instructions=10))
    task = register_simple(adapter, source="ret.l", inputs=0, outputs=0)
    first, second = (task, private) if task_first else (private, task)
    runtime.define_colon("MIXED", (Call(first.xt), Call(second.xt), Return()))
    before = hybrid.machine_instructions
    with pytest.raises(ExecutionError):
        hybrid.execute("MIXED")
    assert hybrid.machine_instructions - before == 1
    assert adapter.machine_instructions == int(task_first)
    assert adapter.active_invocations == ()
    assert runtime.main_context.returns.snapshot() == ()


@pytest.mark.parametrize("field", ("accounting", "cleanup", "authority", "closed"))
def test_replaced_task_authority_projections_cannot_hide_work_or_skip_original_native_close(owner, monkeypatch, field):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word, _ = register_callback(hybrid, adapter)
    context = runtime.main_context
    context.data.push(99)
    context.data.push(7)
    original = runtime._account_semantic_step
    failure = KeyboardInterrupt("task host hook failed after changing a projection")
    seen, unauthorized = [], []

    def replacement(*args):
        unauthorized.append(args)

    def account():
        original()
        if not seen and any(type(entry) is ForeignContinuation for entry in context.returns.snapshot()):
            seen.append(True)
            if field == "accounting":
                hybrid._task_accounting = replacement
                return
            elif field == "cleanup":
                hybrid._task_cleanup = replacement
            elif field == "authority":
                hybrid._task_authority = object()
            else:
                adapter._closed = True
            raise failure

    monkeypatch.setattr(runtime, "_account_semantic_step", account)
    control = hybrid._control_buffer
    with pytest.raises(ExecutionError if field == "accounting" else KeyboardInterrupt) as caught:
        runtime.execute(word.xt)
    assert seen == [True]
    if field != "accounting":
        assert caught.value is failure
    assert unauthorized == []
    assert adapter.machine_instructions == hybrid.machine_instructions == 3
    assert runtime._foreign_tasks.last_dispatch.machine_instructions == 3
    assert adapter.active_invocations == ()
    # Integrity rejection may be retained, but it must not suppress release.
    try:
        hybrid.close()
    except ExecutionError:
        assert hybrid.closed
    assert hybrid.closed and unauthorized == []
    control.extend(b"released by the original native owner")
