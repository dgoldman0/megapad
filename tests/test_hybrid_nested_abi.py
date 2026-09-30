"""V4 metadata proves bounded nested structure without granting entry authority."""

from dataclasses import FrozenInstanceError, replace
from pathlib import Path
import subprocess
import sys

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import BufferRuleV1, CallbackExportV3
from shared.hybrid_closed import (
    ClosedPolicyV3, PolicyBranchV3 as Branch, PolicyBranchZeroV3 as BranchZero,
    PolicyCallV3 as Call, PolicyCoreCallV3 as Core,
    PolicyReturnV3 as Return, prove_policies,
)
from shared.hybrid_nested import (
    CALLBACK_WORK_SATURATION, CallbackExportV4, CallbackRequestV4, CallbackSiteV4,
    ChildCallDeclarationV4, ClosedPolicyV4, MachineSegmentResultV4, PolicyBodyV4,
    PolicyMachineCallV4 as Machine, RoutineDeclarationV4, RoutineGraphNodeV4,
    RoutineImageV4, RoutineManifestV4, prove_nested_graph,
)


def _policy(**changes):
    fields = dict(policy_id=0, name="POLICY", input_cells=1, output_cells=1,
                  operations=(Machine(routine_id=1), Return()))
    fields.update(changes)
    return ClosedPolicyV4(**fields)


def _export(**changes):
    fields = dict(export_id=0, name="POLICY", input_cells=1, output_cells=1,
                  effect="closed_integer_nested", policy_id=0, max_semantic_steps=4)
    fields.update(changes)
    return CallbackExportV4(**fields)


def _leaf(**changes):
    fields = dict(export_id=1, name="ABS", input_cells=1, output_cells=1)
    fields.update(changes)
    return CallbackExportV4(**fields)


def _site(export=None, **changes):
    fields = dict(call_offset=0, stub_offset=8, export=_export() if export is None else export)
    fields.update(changes)
    return CallbackSiteV4(**fields)


def _node(**changes):
    fields = dict(routine_id=0, name="PARENT", input_cells=1, output_cells=1,
                  max_instructions=100, max_callback_requests=1, callbacks=(_site(),))
    fields.update(changes)
    return RoutineGraphNodeV4(**fields)


def _child(**changes):
    fields = dict(routine_id=1, name="CHILD", callbacks=(_site(_leaf()),))
    fields.update(changes)
    return _node(**fields)


def _proof(**changes):
    fields = dict(policies=(_policy(),), routines=(_node(), _child()),
                  exports=(_export(), _leaf()))
    fields.update(changes)
    return prove_nested_graph(**fields)


def _image(**changes):
    fields = dict(routine_id=0, name="PARENT", code=b"\x01" * 32, entry_offset=0,
                  input_cells=1, output_cells=1, buffers=(), return_stack_cells=16,
                  max_instructions=100, max_callback_requests=1, callbacks=(_site(),))
    fields.update(changes)
    return RoutineImageV4(**fields)


def _result(**changes):
    fields = dict(exit_kind="returned", instructions=1, cycles=2, invocation_id=2,
                  invocation_instructions=3, invocation_cycles=6, segment_id=3,
                  root_invocation_id=1, parent_invocation_id=1, depth=2,
                  invocation_started=False, chain_instructions=5, chain_cycles=10,
                  entry_pc=0x1000, pc=MASK64, outputs=(7,))
    fields.update(changes)
    return MachineSegmentResultV4(**fields)


def test_combined_dependency_order_separates_machine_and_semantic_depth():
    graph = _proof()
    assert [(node.kind, node.node_id) for node in graph.publication_order] == [
        ("routine", 1), ("policy", 0), ("routine", 0)]
    policy, = graph.policy_proofs
    assert (policy.min_semantic_steps, policy.max_semantic_steps) == (3, 4)
    assert (policy.required_input_cells, policy.net_data_cells, policy.max_return_cells) == (1, 0, 1)
    assert policy.max_machine_depth == 1 and policy.routine_ids == (1,)
    assert policy.max_data_depth(1) == 1
    assert graph.child_calls == (ChildCallDeclarationV4(policy_id=0, operation_index=0, routine_id=1),)
    child, parent = graph.routine_proofs
    assert (child.callback_work_bound, child.max_machine_depth, child.child_edge_count) == (1, 1, 0)
    assert (parent.callback_work_bound, parent.max_machine_depth, parent.child_edge_count) == (4, 2, 1)
    assert graph.child_edge_count == 1
    with pytest.raises(FrozenInstanceError):
        parent.max_machine_depth = 1
    assert not hasattr(graph, "word") and not hasattr(graph, "__dict__")


def test_conditional_paths_keep_independent_minimum_and_maximum_work():
    policy = _policy(input_cells=2, operations=(
        BranchZero(target=3), Machine(routine_id=1), Branch(target=4), Return(), Return()))
    export = _export(input_cells=2, max_semantic_steps=6)
    graph = _proof(policies=(policy,), exports=(export, _leaf()),
                   routines=(_node(callbacks=(_site(export),)), _child()))
    proof, = graph.policy_proofs
    assert (proof.min_semantic_steps, proof.max_semantic_steps) == (2, 6)
    assert (proof.required_input_cells, proof.net_data_cells, proof.max_data_depth(2)) == (2, -1, 2)


def test_shared_helper_is_charged_per_call_and_captured_once_per_site():
    helper = _policy(policy_id=2, name="HELPER")
    caller = _policy(operations=(Call(policy_id=2), Call(policy_id=2), Return()))
    export = _export(max_semantic_steps=11)
    graph = _proof(policies=(caller, helper), exports=(export, _leaf()), routines=(
        _node(callbacks=(_site(export), _site(export, call_offset=2, stub_offset=9))), _child()))
    proof = next(proof for proof in graph.policy_proofs if proof.policy_id == 0)
    assert (proof.min_semantic_steps, proof.max_semantic_steps, proof.max_return_cells) == (9, 11, 2)
    assert len(proof.child_calls) == 1
    assert graph.child_edge_count == 2


def test_captured_shadowed_names_do_not_retarget_static_ids():
    policy = PolicyBodyV4(policy_id=0, name="SAME", operations=(Machine(routine_id=1), Return()))
    graph = prove_nested_graph(policies=(policy,), exports=(), routines=(
        _node(routine_id=0, name="SAME", callbacks=()),
        _child(name="SAME", callbacks=()),
    ))
    assert graph.policy_proofs[0].routine_ids == (1,)


@pytest.mark.parametrize("count,accepted", [(8, True), (9, False)])
def test_full_machine_chain_depth_is_independent_of_semantic_return_depth(count, accepted):
    policies = tuple(_policy(policy_id=index, name=f"P{index}",
                             operations=(Machine(routine_id=index + 1), Return()))
                     for index in range(count - 1))
    exports = tuple(_export(export_id=index, policy_id=index, name=f"P{index}",
                             max_semantic_steps=4096) for index in range(count - 1))
    routines = tuple(_node(routine_id=index, name=f"R{index}",
                           callbacks=(_site(exports[index]),) if index < count - 1 else ())
                     for index in range(count))
    if not accepted:
        with pytest.raises(ValueError, match="eight active"):
            prove_nested_graph(policies=policies, exports=exports, routines=routines)
    else:
        graph = prove_nested_graph(policies=policies, exports=exports, routines=routines)
        assert max(proof.max_machine_depth for proof in graph.routine_proofs) == 8
        assert all(proof.max_return_cells == 1 for proof in graph.policy_proofs)
        assert graph.policy_proofs[-1].max_semantic_steps == 21


@pytest.mark.parametrize("operations", [
    (Machine(routine_id=0), Return()),
    (Return(), Machine(routine_id=0)),
])
def test_cycles_through_machine_reject_even_unreachable_calls_and_zero_callback_budget(operations):
    with pytest.raises(ValueError, match="acyclic"):
        _proof(policies=(_policy(operations=operations),), routines=(
            _node(max_callback_requests=0), _child()))


def test_unused_policy_cycle_is_rejected():
    policies = (PolicyBodyV4(policy_id=2, name="A", operations=(Call(policy_id=3), Return())),
                PolicyBodyV4(policy_id=3, name="B", operations=(Call(policy_id=2), Return())))
    with pytest.raises(ValueError, match="acyclic"):
        prove_nested_graph(policies=policies, routines=(), exports=())


def test_root_only_saturated_work_is_allowed_but_child_consumption_is_rejected():
    policy = _policy(operations=(Core(name="ABS"), Core(name="ABS"), Return()))
    export = _export(max_semantic_steps=5, effect="closed_integer_colon")
    root = _node(max_callback_requests=1024, max_instructions=10000, callbacks=(_site(export),))
    graph = prove_nested_graph(policies=(policy,), exports=(export,), routines=(root,))
    assert graph.routine_proofs[0].callback_work_bound == CALLBACK_WORK_SATURATION
    caller = PolicyBodyV4(policy_id=1, name="CALLER", operations=(Machine(routine_id=0), Return()))
    with pytest.raises(ValueError, match="4096"):
        prove_nested_graph(policies=(policy, caller), exports=(export,), routines=(root,))


@pytest.mark.parametrize("requests,instructions,root_limit,expected", [
    (0, 100, 1024, 0), (9, 2, 1024, 2), (9, 100, 3, 3), (1, 100, 1024, 1),
])
def test_machine_work_uses_all_three_callback_count_ceilings(requests, instructions, root_limit, expected):
    node = _child(max_callback_requests=requests, max_instructions=instructions)
    graph = prove_nested_graph(policies=(), exports=(_leaf(),), routines=(node,),
                               dispatch_callback_limit=root_limit)
    assert graph.routine_proofs[0].callback_work_bound == expected


def _edge_graph(calls, parents, sites=16):
    policy = _policy(input_cells=0, output_cells=0, operations=(Return(),) + (Machine(routine_id=63),) * calls)
    export = _export(input_cells=0, output_cells=0, max_semantic_steps=1)
    callbacks = tuple(_site(export, call_offset=index * 3, stub_offset=index * 3 + 2)
                      for index in range(sites))
    routines = tuple(_node(routine_id=index, name=f"R{index}", callbacks=callbacks,
                           max_callback_requests=0) for index in range(parents))
    return prove_nested_graph(policies=(policy,), exports=(export,), routines=routines + (
        _child(routine_id=63, callbacks=()),))


def test_site_specific_edge_limits_include_unreachable_calls_without_path_expansion():
    graph = _edge_graph(calls=256, parents=16)
    assert graph.child_edge_count == 65536
    assert len(graph.child_calls) == 256
    assert all(proof.child_edge_count == 4096 for proof in graph.routine_proofs if proof.routine_id != 63)
    with pytest.raises(ValueError, match="4096 child edges"):
        _edge_graph(calls=257, parents=1)
    with pytest.raises(ValueError, match="65536 child edges"):
        _edge_graph(calls=256, parents=17)


def test_closed_colon_effect_rejects_transitive_machine_dependency():
    export = _export(effect="closed_integer_colon")
    with pytest.raises(ValueError, match="cannot capture"):
        _proof(exports=(export, _leaf()), routines=(_node(callbacks=(_site(export),)), _child()))


@pytest.mark.parametrize("bad", [True, False, -1, 64, 1.0, "1", None])
def test_machine_ids_are_exact_bounded_integers(bad):
    with pytest.raises((TypeError, ValueError)):
        Machine(routine_id=bad)


@pytest.mark.parametrize("bad", [True, -1, 1025, 1.0, None])
def test_per_invocation_callback_ceiling_is_required_and_strict(bad):
    with pytest.raises((TypeError, ValueError)):
        _image(max_callback_requests=bad)


def test_old_profiles_do_not_accept_new_operations_descriptors_or_images():
    with pytest.raises(TypeError):
        ClosedPolicyV3(policy_id=0, name="NO", input_cells=1, output_cells=1,
                       operations=(Machine(routine_id=1), Return()))
    with pytest.raises(TypeError):
        prove_policies((_policy(),))
    with pytest.raises(TypeError):
        _site(CallbackExportV3(export_id=1, name="ABS", input_cells=1, output_cells=1))
    with pytest.raises((TypeError, ValueError)):
        _proof(policies=(_policy(version=3),))


def test_nested_forged_fields_are_revalidated_before_use():
    rule = BufferRuleV1(address_argument=0, length_argument=0, element_bytes=1,
                        max_bytes=8, access="read")
    object.__setattr__(rule, "address_argument", True)
    with pytest.raises(TypeError):
        _image(buffers=(rule,))
    operation = Machine(routine_id=1)
    object.__setattr__(operation, "routine_id", True)
    with pytest.raises(TypeError):
        _policy(operations=(operation, Return()))
    node = _child()
    object.__setattr__(node, "max_callback_requests", True)
    with pytest.raises(TypeError):
        _proof(routines=(_node(), node))
    export = _export()
    object.__setattr__(export, "policy_id", True)
    with pytest.raises(TypeError):
        _site(export)


def test_declaration_keeps_opaque_leases_and_requires_sealed_geometry():
    fields = dict(routine_id=0, name="PARENT", code=b"\x01" * 32, entry_offset=0,
                  input_cells=1, output_cells=1, buffers=(), return_stack_cells=16,
                  max_instructions=100, max_callback_requests=0, callbacks=(),
                  session_nonce=object(), registration_nonce=object(), allocation_lease=object(),
                  allocation_generation=1, control_lease=object(), control_generation=1,
                  body_base=0x1000, body_size=64, code_base=0x1010, stack_base=0x2000,
                  dispatch_instruction_limit=1000)
    value = RoutineDeclarationV4(**fields)
    assert value.graph_node().max_callback_requests == 0 and value.version == 4
    with pytest.raises(ValueError, match="aligned"):
        RoutineDeclarationV4(**dict(fields, code_base=0x1011))
    with pytest.raises(ValueError, match="disjoint"):
        RoutineDeclarationV4(**dict(fields, stack_base=0x1000))


def test_result_retains_exclusive_frame_and_inclusive_chain_counters():
    result = _result()
    assert result.version == 4 and result.chain_instructions == 5
    root = _result(invocation_id=1, parent_invocation_id=0, depth=1)
    assert root.root_invocation_id == root.invocation_id
    request = CallbackRequestV4(invocation_id=2, sequence=1, site=_site(), arguments=(7,))
    value = _result(exit_kind="callback_request", outputs=(), callback=request)
    assert value.callback is request
    for changes in (
        dict(depth=1), dict(depth=9), dict(parent_invocation_id=2),
        dict(invocation_id=3, parent_invocation_id=2, depth=2),
        dict(invocation_id=3, parent_invocation_id=1, depth=3),
        dict(root_invocation_id=True), dict(invocation_started=1),
        dict(invocation_started=True), dict(chain_instructions=2), dict(chain_cycles=7),
        dict(instructions=0, cycles=0),
    ):
        with pytest.raises((TypeError, ValueError)):
            _result(**changes)


def test_manifest_rechecks_declarative_name_uniqueness_and_required_routine_ids():
    parent = _image()
    child = _image(routine_id=1, name="CHILD", callbacks=(_site(_leaf()),))
    fields = dict(dispatch_instruction_limit=1000, dispatch_callback_limit=10,
                  dispatch_callback_semantic_limit=100, policies=(_policy(),),
                  exports=(_export(), _leaf()), routines=(parent, child))
    assert RoutineManifestV4(**fields).graph_proof().child_edge_count == 1
    with pytest.raises(ValueError, match="duplicate routine"):
        RoutineManifestV4(**dict(fields, routines=(parent, replace(child, name="parent"))))
    with pytest.raises(ValueError, match="collide"):
        RoutineManifestV4(**dict(fields, routines=(parent, replace(child, name="POLICY"))))
    with pytest.raises(ValueError, match="duplicate routine ID"):
        RoutineManifestV4(**dict(fields, routines=(parent, replace(child, routine_id=0))))


def test_cold_value_import_does_not_import_execution_owners():
    root = Path(__file__).resolve().parents[1]
    code = """
import builtins
original = builtins.__import__
def guarded(name, *args, **kwargs):
    if name.split('.')[0] in {'simulator', 'emulator', '_mp64_accel', '_megaforth_native'}:
        raise AssertionError(name)
    return original(name, *args, **kwargs)
builtins.__import__ = guarded
import shared.hybrid_nested
"""
    subprocess.run([sys.executable, "-c", code], cwd=root, check=True)
