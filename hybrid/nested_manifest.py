"""V4 combined-graph loading, without publication or execution authority."""

from __future__ import annotations

import os
from pathlib import Path

from hybrid.manifest import (
    HybridManifestError, _callbacks_metadata, _integer, _object,
    _read_bounded, _read_manifest, _routine_metadata,
)
from shared.hybrid_abi import (
    CODE_ALIGNMENT, HYBRID_ABI, MAX_CALLBACK_EXPORTS, MAX_CODE_BYTES,
    MAX_DISPATCH_CALLBACKS, MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
    MAX_DISPATCH_INSTRUCTIONS, MAX_ROUTINES, MAX_TOTAL_CODE_BYTES,
)
from shared.hybrid_closed import (
    MAX_CLOSED_OPERATIONS, MAX_CLOSED_POLICIES, PolicyBranchV3, PolicyBranchZeroV3,
    PolicyCallV3, PolicyCoreCallV3, PolicyLiteralV3, PolicyReturnV3,
)
from shared.hybrid_nested import (
    HYBRID_NESTED_ABI_VERSION, CallbackExportV4, CallbackSiteV4, ClosedPolicyV4,
    PolicyMachineCallV4, RoutineGraphNodeV4, RoutineImageV4, RoutineManifestV4,
    prove_nested_graph,
)


_MANIFEST_FIELDS = frozenset((
    "abi", "version", "dispatch_instruction_limit", "dispatch_callback_limit",
    "dispatch_callback_semantic_limit", "policies", "exports", "routines",
))
_POLICY_FIELDS = frozenset(("policy_id", "name", "input_cells", "output_cells", "operations"))
_LEAF_EXPORT_FIELDS = frozenset((
    "export_id", "name", "input_cells", "output_cells", "effect", "max_semantic_steps",
))
_CLOSED_EXPORT_FIELDS = frozenset(("export_id", "effect", "policy_id", "max_semantic_steps"))
_ROUTINE_FIELDS = frozenset((
    "routine_id", "name", "image", "entry_offset", "input_cells", "output_cells", "buffers",
    "max_instructions", "return_stack_cells", "callbacks", "max_callback_requests",
))
_OPERATIONS = {
    "literal": (PolicyLiteralV3, frozenset(("op", "value"))),
    "call_core": (PolicyCoreCallV3, frozenset(("op", "name"))),
    "call_policy": (PolicyCallV3, frozenset(("op", "policy_id"))),
    "call_machine": (PolicyMachineCallV4, frozenset(("op", "routine_id"))),
    "branch": (PolicyBranchV3, frozenset(("op", "target"))),
    "branch_zero": (PolicyBranchZeroV3, frozenset(("op", "target"))),
    "return": (PolicyReturnV3, frozenset(("op",))),
}


def _policies_metadata(value: object) -> tuple[ClosedPolicyV4, ...]:
    if type(value) is not list or len(value) > MAX_CLOSED_POLICIES:
        raise HybridManifestError("policies must be an array of at most 64 declarations")
    policies = []
    identifiers, names, total = set(), set(), 0
    for index, raw in enumerate(value):
        fields = _object(raw, _POLICY_FIELDS, f"policy {index}")
        rows = fields["operations"]
        if type(rows) is not list:
            raise HybridManifestError("policy operations must be an array")
        total += len(rows)
        if total > MAX_CLOSED_OPERATIONS:
            raise HybridManifestError("policy table may contain at most 4096 operations")
        operations = []
        for operation_index, row in enumerate(rows):
            label = f"policy {index} operation {operation_index}"
            if type(row) is not dict or "op" not in row:
                raise HybridManifestError(f"{label} must be an object with an op field")
            name = row["op"]
            if type(name) is not str or name not in _OPERATIONS:
                raise HybridManifestError(f"{label} has an unsupported policy operation")
            operation_type, allowed = _OPERATIONS[name]
            arguments = _object(row, allowed, label)
            operations.append(operation_type(**{
                key: item for key, item in arguments.items() if key != "op"
            }))
        policy = ClosedPolicyV4(**{
            key: item for key, item in fields.items() if key != "operations"
        }, operations=tuple(operations))
        if policy.policy_id in identifiers:
            raise HybridManifestError("duplicate policy ID")
        if policy.name.upper() in names:
            raise HybridManifestError("duplicate policy name")
        identifiers.add(policy.policy_id)
        names.add(policy.name.upper())
        policies.append(policy)
    return tuple(policies)


def _exports_metadata(value: object, policies: tuple[ClosedPolicyV4, ...],
                      ) -> tuple[CallbackExportV4, ...]:
    if type(value) is not list or len(value) > MAX_CALLBACK_EXPORTS:
        raise HybridManifestError("exports must be an array of at most 64 descriptors")
    by_policy = {policy.policy_id: policy for policy in policies}
    identifiers, exports = set(), []
    for index, raw in enumerate(value):
        label = f"export {index}"
        if type(raw) is not dict or "effect" not in raw:
            raise HybridManifestError(f"{label} must be an object with an effect field")
        effect = raw["effect"]
        if type(effect) is not str or effect not in (
                "integer_leaf", "closed_integer_colon", "closed_integer_nested"):
            raise HybridManifestError("unsupported V4 callback effect")
        fields = _object(raw, _LEAF_EXPORT_FIELDS if effect == "integer_leaf"
                         else _CLOSED_EXPORT_FIELDS, label)
        if effect != "integer_leaf":
            identifier = _integer(fields["policy_id"], "policy ID", 0, MAX_CLOSED_POLICIES - 1)
            policy = by_policy.get(identifier)
            if policy is None:
                raise HybridManifestError("export names an undeclared policy ID")
            fields = dict(fields, name=policy.name, input_cells=policy.input_cells,
                          output_cells=policy.output_cells)
        export = CallbackExportV4(**fields)
        if export.export_id in identifiers:
            raise HybridManifestError("duplicate callback export ID")
        identifiers.add(export.export_id)
        exports.append(export)
    return tuple(exports)


def _parse_nested_manifest_v4(manifest_path: Path, decoded: object) -> RoutineManifestV4:
    root = _object(decoded, _MANIFEST_FIELDS, "nested manifest")
    if type(root["abi"]) is not str or root["abi"] != HYBRID_ABI:
        raise HybridManifestError("unsupported hybrid ABI identity")
    version = _integer(root["version"], "ABI version", HYBRID_NESTED_ABI_VERSION,
                       HYBRID_NESTED_ABI_VERSION)
    instruction_limit = _integer(root["dispatch_instruction_limit"], "dispatch instructions", 1,
                                 MAX_DISPATCH_INSTRUCTIONS)
    callback_limit = _integer(root["dispatch_callback_limit"], "dispatch callback requests", 1,
                              MAX_DISPATCH_CALLBACKS)
    semantic_limit = _integer(root["dispatch_callback_semantic_limit"],
                              "dispatch callback semantic steps", 1,
                              MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)
    policies = _policies_metadata(root["policies"])
    exports = _exports_metadata(root["exports"], policies)
    by_export = {export.export_id: export for export in exports}
    rows = root["routines"]
    if type(rows) is not list or len(rows) > MAX_ROUTINES:
        raise HybridManifestError("routines must be an array of at most 64 declarations")
    pending, graph_nodes = [], []
    names = {policy.name.upper() for policy in policies}
    for index, raw in enumerate(rows):
        row = _object(raw, _ROUTINE_FIELDS, f"routine {index}")
        fields, image = _routine_metadata({
            key: item for key, item in row.items()
            if key not in ("routine_id", "max_callback_requests", "callbacks")
        }, manifest_path.parent, index)
        fields["routine_id"] = _integer(row["routine_id"], "routine ID", 0, MAX_ROUTINES - 1)
        fields["max_callback_requests"] = _integer(
            row["max_callback_requests"], "per-call callback requests", 0, MAX_DISPATCH_CALLBACKS)
        fields["callbacks"] = _callbacks_metadata(
            row["callbacks"], by_export, index, site_type=CallbackSiteV4)
        if fields["name"].upper() in names:
            raise HybridManifestError("duplicate or colliding routine name")
        names.add(fields["name"].upper())
        graph_nodes.append(RoutineGraphNodeV4(**{
            name: fields[name] for name in RoutineGraphNodeV4.__dataclass_fields__
        }))
        pending.append((fields, image))

    # Resolve the complete graph, static signatures, effects, local work,
    # callback budgets and active-depth bounds before opening any image.
    prove_nested_graph(policies=policies, routines=tuple(graph_nodes), exports=exports,
                       dispatch_callback_limit=callback_limit)
    routines, total_bytes = [], 0
    for fields, image in pending:
        remaining = MAX_TOTAL_CODE_BYTES - total_bytes
        if remaining <= 0:
            raise HybridManifestError("total code image bytes exceed 16 MiB")
        code = _read_bounded(image, min(MAX_CODE_BYTES, remaining), "routine image")
        total_bytes += ((len(code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
        routines.append(RoutineImageV4(code=code, **fields))
    return RoutineManifestV4(
        abi=root["abi"], version=version, dispatch_instruction_limit=instruction_limit,
        dispatch_callback_limit=callback_limit, dispatch_callback_semantic_limit=semantic_limit,
        policies=policies, exports=exports, routines=tuple(routines),
    )


def _load_nested_manifest_v4(manifest_path: Path, decoded: object) -> RoutineManifestV4:
    """Normalize declaration failures for both public loader entry points."""
    try:
        return _parse_nested_manifest_v4(manifest_path, decoded)
    except (TypeError, ValueError) as exc:
        if type(exc) is HybridManifestError:
            raise
        raise HybridManifestError(str(exc)) from exc


def load_nested_manifest_v4(path: str | os.PathLike[str]) -> RoutineManifestV4:
    """Load one bounded V4 document and its images, never publish or execute.

    Actual instruction boundaries/encodings are proved by later publication.
    """
    return _load_nested_manifest_v4(*_read_manifest(path))


__all__ = ["HybridManifestError", "load_nested_manifest_v4"]
