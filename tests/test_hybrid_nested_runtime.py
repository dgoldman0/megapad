"""V4 capture/publication gates before nested execution is enabled."""

from dataclasses import replace

import pytest

native = pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridExecutionError, HybridRuntime
from shared.hybrid_abi import RoutineImageV1
from shared.hybrid_nested import CallbackExportV4, CallbackSiteV4, RoutineImageV4
from simulator.errors import ExecutionError
from simulator.interop_exports import CallbackExportError
from simulator.ir import Call, Literal, Return


@pytest.fixture(params=("python", "native"))
def owner(request):
    if not hasattr(native, "RoutineSpecV3"):
        pytest.skip("native V3 publication foundation is required")
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    runtime = HybridRuntime.create(executor=request.param,
                                  geometry={"bank0_size": 65536, "external_size": 65536})
    yield runtime
    runtime.close()


def image(name="CHILD", routine_id=0, export=None, *, max_callbacks=1):
    if export is None:
        return RoutineImageV4(name=name, routine_id=routine_id, code=bytes(assemble("inc r4\nret.l")),
                              entry_offset=0, input_cells=1, output_cells=1, buffers=(),
                              return_stack_cells=16, max_instructions=10,
                              max_callback_requests=max_callbacks, callbacks=())
    program = "mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\nret.l\nstub:\nret.l"
    labels = {}
    assemble(program, labels_out=labels)
    program = program.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
    return RoutineImageV4(name=name, routine_id=routine_id, code=bytes(assemble(program)),
                          entry_offset=0, input_cells=export.input_cells, output_cells=export.output_cells,
                          buffers=(), return_stack_cells=16, max_instructions=10,
                          max_callback_requests=max_callbacks,
                          callbacks=(CallbackSiteV4(call_offset=labels["call"],
                                                   stub_offset=labels["stub"], export=export),))


def descriptor(name="POLICY", export_id=0, *, policy_id=7, steps=3, effect="closed_integer_nested"):
    return CallbackExportV4(name=name, export_id=export_id, policy_id=policy_id,
                            input_cells=1, output_cells=1, max_semantic_steps=steps, effect=effect)


def publish_pair(owner):
    child = owner._publish_nested_routine(image())
    policy = owner.semantic.define_colon("POLICY", (Call(child.xt), Return()))
    parent = owner._publish_nested_routine(image("PARENT", 1, descriptor()))
    return child, policy, parent


def state(owner):
    return (owner.semantic.dictionary.here, tuple(owner.semantic.dictionary.words),
            tuple(owner._registrations), owner._issued_code_bytes, owner._control_used,
            owner._issued_child_edges, tuple(owner.semantic._callback_exports._exports),
            tuple(owner.semantic._callback_exports._nested_calls))


def test_public_profile_and_source_execution_remain_unavailable(owner):
    before = state(owner)
    assert not owner.nested_callback_abi_available
    with pytest.raises(RuntimeError, match="fully qualified"):
        owner.register_routine_v4(image())
    assert state(owner) == before
    word = owner._publish_nested_routine(image())
    owner.semantic.main_context.data.push(41)
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(word.xt)
    assert caught.value.reason == "nested_unavailable"
    assert owner.semantic.main_context.data.snapshot() == (41,)
    assert owner.machine_instructions == owner.transitions == 0


def test_real_publication_retains_exact_child_edge_and_live_graph_proof(owner):
    child, policy, parent = publish_pair(owner)
    registration = owner._registrations[id(parent)]
    assert owner._nested_runner.is_code_published_v3(registration.spec)
    assert len(registration.child_edges) == owner._issued_child_edges == 1
    site, call_token, captured, edge = registration.child_edges[0]
    assert site == 0 and captured.word is child and edge is not None
    record = owner.semantic._callback_exports._nested_calls[call_token]
    assert record.word is policy and record.operation_index == 0
    handle = registration.exports[0][1]
    proof = owner.semantic.inspect_callback_export(handle)
    assert proof.max_semantic_steps == 3 and proof.max_machine_depth == 1
    root = next(proof for proof in registration.nested_graph.proof.routine_proofs if proof.routine_id == 1)
    assert root.max_machine_depth == 2 and root.callback_work_bound == 3
    assert {item.version for item in owner.registered_routines} == {4}
    assert owner.registered_routines[-1].routine_id == 1
    with pytest.raises(CallbackExportError, match="admitted nested chain"):
        owner.semantic.invoke_callback_export(handle, (41,))
    with pytest.raises(CallbackExportError, match="admitted nested chain"):
        owner.semantic.begin_closed_callback_accounting(handle)


def test_duplicate_routine_id_is_atomic_across_different_names(owner):
    owner._publish_nested_routine(image())
    before = state(owner)
    with pytest.raises(ValueError, match="routine ID"):
        owner._publish_nested_routine(image("OTHER", 0))
    assert state(owner) == before


def test_forged_routine_id_is_validated_before_duplicate_comparison(owner):
    owner._publish_nested_routine(image())
    calls = []
    class ForeignID:
        def __eq__(self, other):
            calls.append(other)
            raise AssertionError("foreign equality was invoked")
    candidate = image("OTHER", 1)
    object.__setattr__(candidate, "routine_id", ForeignID())
    before = state(owner)
    with pytest.raises(TypeError):
        owner._publish_nested_routine(candidate)
    assert calls == [] and state(owner) == before


@pytest.mark.parametrize("target_kind", ("legacy", "arbitrary"))
def test_only_explicit_v4_machine_targets_are_captured(owner, target_kind):
    if target_kind == "legacy":
        target = owner.register_routine_v1(RoutineImageV1(
            name="TARGET", code=bytes(assemble("ret.l")), entry_offset=0,
            input_cells=1, output_cells=1, buffers=(), return_stack_cells=16, max_instructions=10,
        ))
    else:
        target = owner.semantic.define_primitive("TARGET", lambda context: None)
    owner.semantic.define_colon("POLICY", (Call(target.xt), Return()))
    before = state(owner)
    with pytest.raises(ExecutionError):
        owner._publish_nested_routine(image("PARENT", 1, descriptor()))
    assert state(owner) == before


def test_closed_v4_effect_cannot_smuggle_a_machine_dependency(owner):
    child = owner._publish_nested_routine(image())
    owner.semantic.define_colon("POLICY", (Call(child.xt), Return()))
    before = state(owner)
    with pytest.raises(CallbackExportError, match="cannot capture a machine"):
        owner._publish_nested_routine(image("PARENT", 1, descriptor(effect="closed_integer_colon")))
    assert state(owner) == before


def test_shadowing_preserves_captured_original_machine_and_policy(owner):
    child, policy, parent = publish_pair(owner)
    handle = owner._registrations[id(parent)].exports[0][1]
    owner.semantic.define_colon("CHILD", (Literal(999), Return()))
    owner.semantic.define_colon("POLICY", (Return(),))
    assert owner.semantic.verify_callback_export(handle).name == "POLICY"
    captured = owner.semantic._callback_exports._binding(handle).closed
    assert captured.entry.word is policy and captured.machines[0].word is child


@pytest.mark.parametrize("damage", ("code", "ir", "signature", "control"))
def test_captured_child_values_and_policy_identity_are_revalidated(owner, damage):
    child, policy, parent = publish_pair(owner)
    handle = owner._registrations[id(parent)].exports[0][1]
    declaration = owner.declaration_for(child)
    if damage == "code":
        owner.semantic.memory.write8(declaration.code_base, 0)
    elif damage == "ir":
        object.__setattr__(policy.implementation, "operations", (Return(),))
    elif damage == "signature":
        object.__setattr__(declaration, "max_instructions", 9)
    else:
        object.__setattr__(declaration.control_lease, "generation", 999)
    with pytest.raises(ExecutionError):
        owner.semantic.verify_callback_export(handle)
    assert owner.machine_instructions == 0


def test_reused_call_object_at_two_positions_gets_two_native_edges(owner):
    child = owner._publish_nested_routine(image())
    operation = Call(child.xt)
    owner.semantic.define_colon("POLICY", (operation, operation, Return()))
    parent = owner._publish_nested_routine(image("PARENT", 1, descriptor(steps=5)))
    edges = owner._registrations[id(parent)].child_edges
    assert len(edges) == 2 and edges[0][1] is not edges[1][1] and edges[0][3] is not edges[1][3]
    assert {owner.semantic._callback_exports._nested_calls[item[1]].operation_index for item in edges} == {0, 1}


def test_helper_paths_deduplicate_one_static_call_without_unrolling(owner):
    child = owner._publish_nested_routine(image())
    helper = owner.semantic.define_colon("HELPER", (Call(child.xt), Return()))
    owner.semantic.define_colon("POLICY", (Call(helper.xt), Call(helper.xt), Return()))
    parent = owner._publish_nested_routine(image("PARENT", 1, descriptor(steps=9)))
    assert len(owner._registrations[id(parent)].child_edges) == 1


def test_transitive_eight_frame_graph_is_admitted_and_ninth_rejected_atomically(owner):
    previous = owner._publish_nested_routine(image())
    for depth in range(2, 9):
        policy_name = f"POLICY-{depth}"
        owner.semantic.define_colon(policy_name, (Call(previous.xt), Return()))
        export = descriptor(policy_name, depth - 2, policy_id=depth - 2, steps=3 * (depth - 1))
        previous = owner._publish_nested_routine(image(f"MACHINE-{depth}", depth - 1, export))
    graph = owner._registrations[id(previous)].nested_graph
    assert max(proof.max_machine_depth for proof in graph.proof.routine_proofs) == 8
    owner.semantic.define_colon("POLICY-9", (Call(previous.xt), Return()))
    before = state(owner)
    with pytest.raises(CallbackExportError, match="eight active machine"):
        owner._publish_nested_routine(image("MACHINE-9", 8, descriptor("POLICY-9", 7, policy_id=7, steps=24)))
    assert state(owner) == before


def test_child_callback_ir_cannot_be_recaptured_after_mutation(owner):
    policy = owner.semantic.define_colon("CHILD-POLICY", (Return(),))
    child = owner._publish_nested_routine(image("CHILD", 0,
        descriptor("CHILD-POLICY", 0, policy_id=0, steps=1, effect="closed_integer_colon")))
    object.__setattr__(policy.implementation, "operations", (Call(child.xt), Return()))
    owner.semantic.define_colon("POLICY", (Call(child.xt), Return()))
    before = state(owner)
    with pytest.raises(CallbackExportError, match="operation tuple changed"):
        owner._publish_nested_routine(image("PARENT", 1, descriptor(export_id=1, steps=10)))
    assert state(owner) == before


def test_forwarded_native_publication_failure_revokes_edges_exports_and_word(owner, monkeypatch):
    child = owner._publish_nested_routine(image())
    owner.semantic.define_colon("POLICY", (Call(child.xt), Return()))
    original_runner = owner._nested_runner
    failure = RuntimeError("publication forwarded before host failure")
    issued = []
    class Proxy:
        def __getattr__(self, name):
            return getattr(original_runner, name)
        def publish_code_v3(self, spec, edges):
            result = original_runner.publish_code_v3(spec, edges)
            issued.append((spec, result))
            raise failure
    before = state(owner)
    with monkeypatch.context() as patch:
        patch.setattr(owner, "_nested_runner", Proxy())
        with pytest.raises(RuntimeError) as caught:
            owner._publish_nested_routine(image("PARENT", 1, descriptor()))
    assert caught.value is failure and state(owner) == before
    assert len(issued) == 1 and not original_runner.is_code_published_v3(issued[0][0])
    assert owner._registration_failure is None
    parent = owner._publish_nested_routine(image("PARENT", 1, descriptor()))
    assert len(owner._registrations[id(parent)].child_edges) == 1


def test_legacy_and_v4_publications_share_one_owner_without_promoting_legacy_entry(owner):
    owner._publish_nested_routine(image())
    legacy = owner.register_routine_v1(RoutineImageV1(
        name="LEGACY", code=bytes(assemble("inc r4\nret.l")), entry_offset=0,
        input_cells=1, output_cells=1, buffers=(), return_stack_cells=16, max_instructions=10,
    ))
    owner.semantic.main_context.data.push(8)
    assert owner.execute(legacy.xt).machine_instructions == 2
    assert owner.semantic.main_context.data.snapshot() == (9,)


@pytest.mark.parametrize("helper_count,admitted", ((62, True), (63, False)))
def test_each_semantic_closure_counts_policy_core_and_machine_words_together(owner, helper_count, admitted):
    child = owner._publish_nested_routine(image())
    helpers = [owner.semantic.define_colon(f"HELPER-{index}", (Return(),))
               for index in range(helper_count)]
    owner.semantic.define_colon("POLICY", tuple(Call(word.xt) for word in helpers) + (Call(child.xt), Return()))
    candidate = image("PARENT", 1, descriptor(steps=2 * helper_count + 3))
    before = state(owner)
    if not admitted:
        with pytest.raises(CallbackExportError, match="64 Words"):
            owner._publish_nested_routine(candidate)
        assert state(owner) == before
    else:
        parent = owner._publish_nested_routine(candidate)
        proof = owner.semantic.inspect_callback_export(owner._registrations[id(parent)].exports[0][1])
        assert len(proof.policy_ids) + len(proof.core_names) + len(proof.routine_ids) == 64


def test_conflicting_root_ids_are_rejected_before_graph_map_overwrite(owner):
    from simulator.interop_nested import capture_nested_graph
    first = owner.semantic.define_colon("FIRST", (Return(),))
    second = owner.semantic.define_colon("SECOND", (Return(),))
    with pytest.raises(CallbackExportError, match="root export identity conflicts"):
        capture_nested_graph(owner.semantic._callback_exports, (
            (descriptor("FIRST", steps=1), first), (descriptor("SECOND", steps=1), second),
        ))


def test_older_native_surface_falls_back_to_legacy_owner(monkeypatch):
    with monkeypatch.context() as patch:
        patch.delattr(native, "RoutineSpecV3", raising=False)
        runtime = HybridRuntime.create(geometry={"bank0_size": 65536, "external_size": 4096})
        try:
            assert runtime._nested_runner is None
            word = runtime.register_routine_v1(RoutineImageV1(
                name="LEGACY", code=bytes(assemble("inc r4\nret.l")), entry_offset=0,
                input_cells=1, output_cells=1, buffers=(), return_stack_cells=16, max_instructions=10,
            ))
            runtime.semantic.main_context.data.push(5)
            assert runtime.execute(word.xt).machine_instructions == 2
            assert runtime.semantic.main_context.data.snapshot() == (6,)
        finally:
            runtime.close()


def test_failed_legacy_facade_factory_closes_constructed_v3_owner(monkeypatch):
    if not hasattr(native, "RoutineRunnerV3"):
        pytest.skip("native V3 publication foundation is required")
    original = native.RoutineRunnerV3
    failure = RuntimeError("facade factory failed")
    buffers, closed = [], []
    class FailedFactory:
        def __init__(self, cpu, base, control):
            self.inner = original(cpu, base, control)
            buffers.append(control)
        def legacy_v2(self):
            raise failure
        def close(self):
            self.inner.close()
            closed.append(True)
        def publish_code_v3(self, *args):
            return self.inner.publish_code_v3(*args)
        def revoke_code_v3(self, *args):
            return self.inner.revoke_code_v3(*args)
        def is_code_published_v3(self, *args):
            return self.inner.is_code_published_v3(*args)
    with monkeypatch.context() as patch:
        patch.setattr(native, "RoutineRunnerV3", FailedFactory)
        with pytest.raises(RuntimeError) as caught:
            HybridRuntime.create(geometry={"bank0_size": 65536, "external_size": 4096})
    assert caught.value is failure and closed == [True] and len(buffers) == 1
    buffers[0].extend(b"released")
