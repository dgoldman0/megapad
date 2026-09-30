"""Closed-policy proof and metadata are bounded values, never Word authority."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
from pathlib import Path
import subprocess
import sys

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI_VERSION, HYBRID_CALLBACK_ABI_VERSION, HYBRID_CLOSED_ABI_VERSION,
    BufferRuleV1,
    CallbackExportV2, CallbackExportV3, CallbackSiteV2, CallbackSiteV3,
    CallbackRequestV2, CallbackRequestV3, MachineExitKindV2,
    MachineSegmentResultV3, RoutineDeclarationV3, RoutineImageV1, RoutineImageV2,
    RoutineImageV3, RoutineManifestV1, RoutineManifestV2, RoutineManifestV3,
)
from shared.hybrid_closed import (
    CORE_STACK_EFFECTS, ClosedPolicyV3, ClosedPolicyProofV3, PolicyBodyV3,
    PolicyLiteralV3 as Literal, PolicyCoreCallV3 as Core, PolicyCallV3 as Call,
    PolicyBranchV3 as Branch, PolicyBranchZeroV3 as BranchZero,
    PolicyReturnV3 as Return, prove_policies,
)


def _policy(**changes):
    values = dict(policy_id=0, name="CLAMP", input_cells=3, output_cells=1,
                  operations=(Core(name="ROT"), Core(name="MIN"), Core(name="MAX"), Return()))
    values.update(changes)
    return ClosedPolicyV3(**values)


def _export(**changes):
    values = dict(export_id=0, name="CLAMP", input_cells=3, output_cells=1,
                  max_semantic_steps=7, effect="closed_integer_colon")
    values.update(changes)
    return CallbackExportV3(**values)


def _site(**changes):
    values = dict(call_offset=0, stub_offset=8, export=_export())
    values.update(changes)
    return CallbackSiteV3(**values)


def _image(**changes):
    values = dict(name="H-CLAMP", code=b"\x01" * 32, entry_offset=0,
                  input_cells=3, output_cells=1, buffers=(), max_instructions=100,
                  return_stack_cells=16, callbacks=(_site(),))
    values.update(changes)
    return RoutineImageV3(**values)


def _manifest(**changes):
    values = dict(dispatch_instruction_limit=1000, dispatch_callback_limit=10,
                  dispatch_callback_semantic_limit=100, policies=(_policy(),),
                  exports=(_export(),), routines=(_image(),))
    values.update(changes)
    return RoutineManifestV3(**values)


def _request(**changes):
    values = dict(invocation_id=1, sequence=1, site=_site(), arguments=(8, 0, 5))
    values.update(changes)
    return CallbackRequestV3(**values)


def _result(**changes):
    values = dict(exit_kind="returned", instructions=1, cycles=2,
                  invocation_id=1, invocation_instructions=5, invocation_cycles=12,
                  entry_pc=0x1000, pc=MASK64, outputs=(5,))
    values.update(changes)
    return MachineSegmentResultV3(**values)


def _declaration(**changes):
    values = dict(name="H-CLAMP", code=b"\x01" * 32, entry_offset=0,
                  input_cells=3, output_cells=1, buffers=(), max_instructions=100,
                  return_stack_cells=16, callbacks=(_site(),),
                  session_nonce=object(), registration_nonce=object(),
                  allocation_lease=object(), allocation_generation=1,
                  control_lease=object(), control_generation=1,
                  body_base=0x1000, body_size=64, code_base=0x1010,
                  stack_base=0x2000, dispatch_instruction_limit=1000)
    values.update(changes)
    return RoutineDeclarationV3(**values)


def test_clamp_proof_includes_primitive_call_ticks_and_root_cookie():
    policy = _policy()
    proof, = prove_policies((policy,))
    assert (proof.required_input_cells, proof.net_data_cells, proof.peak_data_growth) == (3, -2, 0)
    assert (proof.min_semantic_steps, proof.max_semantic_steps, proof.max_return_cells) == (7, 7, 1)
    assert proof.policy_ids == (0,) and proof.core_names == ("MAX", "MIN", "ROT")
    assert proof.max_data_depth(3) == 3
    proof.validate_signature(3, 1, 7)
    with pytest.raises(ValueError, match="allowance"):
        proof.validate_signature(3, 1, 6)
    with pytest.raises(FrozenInstanceError):
        proof.max_semantic_steps = 1
    assert not hasattr(proof, "word") and not hasattr(policy, "__dict__")


@pytest.mark.parametrize("name,effect", tuple(CORE_STACK_EFFECTS.items()))
def test_canonical_primitive_effects_are_inferred_without_helper_signatures(name, effect):
    required, delta = effect
    body = PolicyBodyV3(policy_id=0, name="HELPER", operations=(Core(name=name), Return()))
    proof, = prove_policies((body,))
    assert (proof.required_input_cells, proof.net_data_cells) == (required, delta)
    assert proof.peak_data_growth == max(0, delta)
    assert proof.min_semantic_steps == proof.max_semantic_steps == 3
    proof.validate_signature(required, required + delta, 3)


def test_dependency_order_counts_shared_callee_once_but_charges_each_call():
    helper = PolicyBodyV3(policy_id=3, name="HELPER", operations=(Core(name="ABS"), Return()))
    caller = PolicyBodyV3(policy_id=1, name="CALLER",
                          operations=(Call(policy_id=3), Call(policy_id=3), Return()))
    proofs = prove_policies((caller, helper))
    assert tuple(proof.policy_id for proof in proofs) == (3, 1)
    proof = proofs[-1]
    assert proof.policy_ids == (1, 3) and proof.core_names == ("ABS",)
    assert proof.min_semantic_steps == proof.max_semantic_steps == 9
    assert proof.max_return_cells == 2
    proof.validate_signature(1, 1, 9)


def test_captured_shadowed_helpers_keep_distinct_ids_despite_equal_names():
    old = PolicyBodyV3(policy_id=1, name="HELPER", operations=(Core(name="ABS"), Return()))
    new = PolicyBodyV3(policy_id=2, name="HELPER",
                       operations=(Core(name="DUP"), Core(name="DROP"), Return()))
    caller = PolicyBodyV3(policy_id=0, name="CALLER",
                          operations=(Call(policy_id=1), Call(policy_id=2), Return()))
    proof = prove_policies((caller, old, new))[-1]
    assert proof.policy_ids == (0, 1, 2)
    assert proof.core_names == ("ABS", "DROP", "DUP")
    assert proof.min_semantic_steps == proof.max_semantic_steps == 11
    assert proof.max_return_cells == 2 and proof.peak_data_growth == 1
    proof.validate_signature(1, 1, 11)


def test_forward_branch_join_tracks_safe_depth_and_both_work_bounds():
    # (value flag -- value): true takes ABS and an explicit branch; false
    # returns immediately. Both paths retain one data cell at the join.
    body = _policy(input_cells=2, output_cells=1, operations=(
        BranchZero(target=3), Core(name="ABS"), Branch(target=4),
        Branch(target=4), Return(),
    ))
    proof, = prove_policies((body,))
    assert (proof.min_semantic_steps, proof.max_semantic_steps) == (3, 5)
    assert (proof.required_input_cells, proof.net_data_cells) == (2, -1)
    assert proof.max_data_depth(2) == 2


@pytest.mark.parametrize("operations,message", (
    ((BranchZero(target=3), Literal(value=1), Branch(target=3), Return()), "joins"),
    ((BranchZero(target=2), Return(), Literal(value=1), Return()), "return paths"),
    ((Literal(value=1),), "end in Return"),
))
def test_unsafe_control_shapes_are_rejected(operations, message):
    with pytest.raises(ValueError, match=message):
        prove_policies((PolicyBodyV3(policy_id=0, name="BAD", operations=operations),))


@pytest.mark.parametrize("count,accepted", ((8, True), (9, False)))
def test_eight_return_cells_include_root_continuation(count, accepted):
    bodies = tuple(PolicyBodyV3(policy_id=index, name=f"P{index}",
        operations=((Call(policy_id=index + 1), Return()) if index + 1 < count else (Return(),)))
        for index in range(count))
    if not accepted:
        with pytest.raises(ValueError, match="stack"):
            prove_policies(bodies)
    else:
        root = prove_policies(bodies)[-1]
        assert root.max_return_cells == 8 and root.max_semantic_steps == 15


def test_data_capacity_includes_original_arguments_and_temporary_growth():
    operations = (Core(name="DUP"), Core(name="DROP"), Return())
    proof, = prove_policies((PolicyBodyV3(policy_id=0, name="P", operations=operations),))
    assert proof.required_input_cells == 1 and proof.peak_data_growth == 1
    proof.validate_signature(7, 7)
    with pytest.raises(ValueError, match="capacity"):
        proof.validate_signature(8, 8)
    with pytest.raises(ValueError, match="underflow"):
        proof.validate_signature(0, 0)
    with pytest.raises(ValueError, match="output"):
        proof.validate_signature(1, 0)


@pytest.mark.parametrize("pairs,accepted", ((1365, True), (1366, False)))
def test_work_bound_uses_reference_tick_cost_not_ir_count(pairs, accepted):
    body = PolicyBodyV3(policy_id=0, name="WORK",
        operations=(Literal(value=0), Core(name="DROP")) * pairs + (Return(),))
    if accepted:
        proof, = prove_policies((body,))
        assert proof.max_semantic_steps == 4096
        assert proof.max_data_depth(0) == 1
    else:
        with pytest.raises(ValueError, match="4096 semantic steps"):
            prove_policies((body,))


@pytest.mark.parametrize("bodies", (
    (PolicyBodyV3(policy_id=0, name="SELF", operations=(Call(policy_id=0), Return())),),
    (PolicyBodyV3(policy_id=0, name="A", operations=(Call(policy_id=1), Return())),
     PolicyBodyV3(policy_id=1, name="B", operations=(Call(policy_id=0), Return()))),
    # An unreachable suffix is not an excuse to retain a cyclic capture graph.
    (PolicyBodyV3(policy_id=0, name="DEAD", operations=(Return(), Call(policy_id=0))),),
))
def test_recursion_is_rejected_in_the_complete_capture_graph(bodies):
    with pytest.raises(ValueError, match="acyclic"):
        prove_policies(bodies)


def test_missing_dependencies_duplicate_names_and_aggregate_storage_bounds():
    body = PolicyBodyV3(policy_id=0, name="A", operations=(Return(),))
    with pytest.raises(ValueError, match="undeclared"):
        prove_policies((replace(body, operations=(Call(policy_id=1), Return())),))
    with pytest.raises(ValueError, match="duplicate policy ID"):
        prove_policies((body, replace(body, name="B")))
    declaration = ClosedPolicyV3(policy_id=0, name="A", input_cells=0, output_cells=0,
                                 operations=(Return(),))
    with pytest.raises(ValueError, match="duplicate policy name"):
        prove_policies((declaration, replace(declaration, policy_id=1, name="a")))
    large = tuple(Return() for _ in range(2049))
    with pytest.raises(ValueError, match="4096 operations"):
        prove_policies((replace(body, operations=large),
                        replace(body, policy_id=1, name="B", operations=large)))
    with pytest.raises(ValueError, match="64 definitions"):
        prove_policies((body,) * 65)


def test_closure_word_count_includes_canonical_primitives_in_dead_suffixes():
    helpers = tuple(PolicyBodyV3(policy_id=index, name=f"H{index}", operations=(Return(),))
                    for index in range(1, 64))
    body = PolicyBodyV3(policy_id=0, name="ROOT", operations=(Return(),) +
        tuple(Call(policy_id=index) for index in range(1, 64)) + (Core(name="ABS"),))
    with pytest.raises(ValueError, match="64 Words"):
        prove_policies((body, *helpers))


@pytest.mark.parametrize("factory,arguments,error", (
    (Literal, {"value": True}, TypeError), (Literal, {"value": -1}, ValueError),
    (Literal, {"value": MASK64 + 1}, ValueError),
    (Core, {"name": "EXECUTE"}, ValueError), (Core, {"name": "abs"}, ValueError),
    (Call, {"policy_id": False}, TypeError), (Call, {"policy_id": 64}, ValueError),
    (Branch, {"target": True}, TypeError), (BranchZero, {"target": -1}, ValueError),
))
def test_operations_have_exact_bounded_fields(factory, arguments, error):
    with pytest.raises(error):
        factory(**arguments)


def test_revalidation_rejects_forged_values_subclasses_and_hidden_operations():
    class LiteralSubclass(Literal):
        pass

    for bad in (object(), LiteralSubclass(value=0)):
        with pytest.raises(TypeError, match="exact admitted"):
            _policy(operations=(Return(), bad))
    with pytest.raises(TypeError, match="tuple"):
        _policy(operations=[Return()])
    with pytest.raises(ValueError, match="later operation"):
        _policy(operations=(Branch(target=0), Return()))
    forged = Literal(value=0)
    object.__setattr__(forged, "value", True)
    body = PolicyBodyV3(policy_id=0, name="P", operations=(Return(),))
    object.__setattr__(body, "operations", (Return(), forged))
    with pytest.raises(TypeError, match="exact integer"):
        prove_policies((body,))


def test_complete_v3_manifest_derives_closed_export_and_keeps_native_exit_kinds():
    manifest = _manifest()
    assert manifest.version == 3 and manifest.policies[0].name == "CLAMP"
    assert _declaration().version == 3
    request = _request()
    result = _result(exit_kind="callback_request", outputs=(), callback=request)
    assert result.exit_kind is MachineExitKindV2.CALLBACK_REQUEST
    assert result.callback is request and result.version == 3
    leaf = _export(export_id=1, name="MIN", input_cells=2, max_semantic_steps=1,
                   effect="integer_leaf")
    assert _manifest(exports=(_export(), leaf)).exports[1] == leaf
    with pytest.raises(FrozenInstanceError):
        manifest.policies = ()


@pytest.mark.parametrize("changes,error", (
    ({"effect": "service"}, ValueError), ({"effect": b"integer_leaf"}, TypeError),
    ({"max_semantic_steps": True}, TypeError), ({"max_semantic_steps": 4097}, ValueError),
    ({"input_cells": 9}, ValueError), ({"output_cells": -1}, ValueError),
    ({"name": "HAS SPACE"}, ValueError), ({"export_id": 64}, ValueError),
))
def test_closed_exports_are_value_only_and_finite(changes, error):
    with pytest.raises(error):
        _export(**changes)


@pytest.mark.parametrize("factory", (_policy, _export, _site, _image, _manifest, _request, _result, _declaration))
def test_v3_values_require_version3_and_exact_abi(factory):
    for version in (1, 2, 4):
        with pytest.raises(ValueError, match="ABI version"):
            factory(version=version)
    with pytest.raises(TypeError, match="exact integer"):
        factory(version=True)
    with pytest.raises(ValueError, match="ABI identity"):
        factory(abi="foreign")


def test_v1_v2_types_do_not_admit_v3_metadata_or_broaden_leaf_catalog():
    assert (HYBRID_ABI_VERSION, HYBRID_CALLBACK_ABI_VERSION, HYBRID_CLOSED_ABI_VERSION) == (1, 2, 3)
    with pytest.raises(ValueError, match="canonical"):
        CallbackExportV2(export_id=0, name="CLAMP", input_cells=3, output_cells=1)
    with pytest.raises(TypeError, match="CallbackExportV2"):
        CallbackSiteV2(call_offset=0, stub_offset=8, export=_export())
    with pytest.raises(TypeError, match="CallbackSiteV2"):
        CallbackRequestV2(invocation_id=1, sequence=1, site=_site(), arguments=(1, 2, 3))
    with pytest.raises(TypeError, match="RoutineImageV1"):
        RoutineManifestV1(dispatch_instruction_limit=100, routines=(_image(),))
    with pytest.raises(TypeError, match="CallbackExportV2"):
        RoutineManifestV2(dispatch_instruction_limit=100, dispatch_callback_limit=1,
                          dispatch_callback_semantic_limit=100, exports=(_export(),), routines=())
    with pytest.raises(ValueError, match="canonical"):
        _export(name="DUP", input_cells=1, output_cells=2,
                max_semantic_steps=1, effect="integer_leaf")


@pytest.mark.parametrize("change,message", (
    ({"policies": ()}, "declared policy"),
    ({"policies": (_policy(name="h-clamp"),)}, "collide"),
    ({"policies": (_policy(input_cells=4, output_cells=2),)}, "arity"),
    ({"exports": (_export(max_semantic_steps=6),), "routines": ()}, "allowance"),
    ({"exports": (_export(export_id=1),)}, "undeclared callback export"),
))
def test_manifest_rejects_unproved_or_inconsistent_export_bindings(change, message):
    with pytest.raises(ValueError, match=message):
        _manifest(**change)


def test_nested_v3_values_are_revalidated_at_container_boundaries():
    export = _export()
    object.__setattr__(export, "max_semantic_steps", True)
    with pytest.raises(TypeError, match="exact integer"):
        _site(export=export)
    site = _site()
    object.__setattr__(site, "stub_offset", -1)
    with pytest.raises(ValueError, match="stub offset"):
        _image(callbacks=(site,))
    proof, = prove_policies((_policy(),))
    object.__setattr__(proof, "peak_data_growth", True)
    with pytest.raises(TypeError, match="exact integer"):
        proof.validate_signature(3, 1, 7)


@pytest.mark.parametrize("factory", (_image, _declaration))
@pytest.mark.parametrize("field", ("address_argument", "length_argument"))
def test_v3_buffer_fields_are_revalidated_before_comparison(factory, field):
    class ForbiddenComparison:
        def __lt__(self, other):
            raise AssertionError("forged metadata must not execute host comparisons")

        __gt__ = __lt__

    rule = BufferRuleV1(address_argument=0, length_argument=1, element_bytes=1,
                        max_bytes=8, access="read")
    object.__setattr__(rule, field, ForbiddenComparison())
    with pytest.raises(TypeError, match="exact integer"):
        factory(buffers=(rule,))


def test_v3_result_retains_callback_and_segment_counter_invariants():
    with pytest.raises(TypeError, match="CallbackRequestV3"):
        _result(exit_kind="callback_request", outputs=(), callback=None)
    with pytest.raises(ValueError, match="completed RET"):
        _result(instructions=0, cycles=0)
    with pytest.raises(ValueError, match="only callback request"):
        _result(callback=_request())
    with pytest.raises(ValueError, match="different invocation"):
        _result(exit_kind="callback_request", outputs=(), callback=_request(invocation_id=2))
    result = _result(exit_kind="instruction_limit", instructions=0, cycles=0, outputs=())
    assert result.invocation_instructions == 5 and result.callback is None


def test_policy_values_and_proof_import_with_every_backend_blocked():
    source = r'''
import importlib.abc
import sys
class RejectBackends(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in ('emulator', 'simulator', 'hybrid', '_mp64_accel', '_megaforth_native'):
            raise AssertionError('forbidden backend import: ' + fullname)
sys.meta_path.insert(0, RejectBackends())
from shared.hybrid_closed import ClosedPolicyV3, PolicyReturnV3, prove_policies
from shared.hybrid_abi import CallbackExportV3, RoutineManifestV3
policy = ClosedPolicyV3(policy_id=0, name='IDENTITY', input_cells=1, output_cells=1,
                        operations=(PolicyReturnV3(),))
proof, = prove_policies((policy,))
assert proof.max_semantic_steps == 1
export = CallbackExportV3(export_id=0, name='IDENTITY', input_cells=1, output_cells=1,
                          effect='closed_integer_colon')
assert RoutineManifestV3(dispatch_instruction_limit=1, dispatch_callback_limit=1,
    dispatch_callback_semantic_limit=1, exports=(export,), routines=(), policies=(policy,)).version == 3
'''
    completed = subprocess.run([sys.executable, "-c", source],
        cwd=Path(__file__).resolve().parents[1], text=True, capture_output=True, timeout=10)
    assert completed.returncode == 0, completed.stderr
