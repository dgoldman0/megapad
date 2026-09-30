"""Bounded local manifest loading, with no registration or code execution."""

from __future__ import annotations

import json
import os
from pathlib import Path, PureWindowsPath
import stat

from shared.hybrid_abi import (
    HYBRID_ABI,
    HYBRID_ABI_VERSION,
    HYBRID_CALLBACK_ABI_VERSION,
    HYBRID_CLOSED_ABI_VERSION,
    CODE_ALIGNMENT,
    MAX_BUFFER_RULES,
    MAX_CALL_INSTRUCTIONS,
    MAX_CODE_BYTES,
    MAX_DISPATCH_INSTRUCTIONS,
    MAX_MANIFEST_BYTES,
    MAX_RETURN_STACK_CELLS,
    MAX_ROUTINES,
    MAX_SIGNATURE_CELLS,
    MAX_TOTAL_CODE_BYTES,
    MAX_CALLBACK_EXPORTS,
    MAX_CALLBACK_SITES,
    MAX_DISPATCH_CALLBACKS,
    MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
    BufferRuleV1,
    CallbackExportV2,
    CallbackExportV3,
    CallbackSiteV2,
    CallbackSiteV3,
    RoutineImageV1,
    RoutineImageV2,
    RoutineImageV3,
    RoutineManifestV1,
    RoutineManifestV2,
    RoutineManifestV3,
)
from shared.hybrid_closed import (
    MAX_CLOSED_POLICIES, MAX_CLOSED_OPERATIONS,
    ClosedPolicyV3, PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3,
    PolicyBranchV3, PolicyBranchZeroV3, PolicyReturnV3, prove_policies,
)
from shared.hybrid_nested import HYBRID_NESTED_ABI_VERSION, RoutineManifestV4


class HybridManifestError(ValueError):
    """A manifest or image failed validation before any publication."""


_MANIFEST_FIELDS = frozenset(("abi", "version", "dispatch_instruction_limit", "routines"))
_ROUTINE_FIELDS = frozenset((
    "name", "image", "entry_offset", "input_cells", "output_cells", "buffers",
    "max_instructions", "return_stack_cells",
))
_BUFFER_FIELDS = frozenset((
    "address_argument", "length_argument", "element_bytes", "max_bytes", "access",
))
_MANIFEST_V2_FIELDS = _MANIFEST_FIELDS | frozenset((
    "dispatch_callback_limit", "dispatch_callback_semantic_limit", "exports",
))
_ROUTINE_V2_FIELDS = _ROUTINE_FIELDS | frozenset(("callbacks",))
_EXPORT_FIELDS = frozenset(("export_id", "name", "input_cells", "output_cells"))
_CALLBACK_FIELDS = frozenset(("call_offset", "stub_offset", "export_id"))
_MANIFEST_V3_FIELDS = _MANIFEST_V2_FIELDS | frozenset(("policies",))
_POLICY_FIELDS = frozenset(("policy_id", "name", "input_cells", "output_cells", "operations"))
_LEAF_EXPORT_V3_FIELDS = _EXPORT_FIELDS | frozenset(("effect", "max_semantic_steps"))
_CLOSED_EXPORT_V3_FIELDS = frozenset(("export_id", "effect", "policy_id", "max_semantic_steps"))
_POLICY_OPERATIONS = {
    "literal": (PolicyLiteralV3, frozenset(("op", "value"))),
    "call_core": (PolicyCoreCallV3, frozenset(("op", "name"))),
    "call_policy": (PolicyCallV3, frozenset(("op", "policy_id"))),
    "branch": (PolicyBranchV3, frozenset(("op", "target"))),
    "branch_zero": (PolicyBranchZeroV3, frozenset(("op", "target"))),
    "return": (PolicyReturnV3, frozenset(("op",))),
}


def _object(value: object, fields: frozenset[str], label: str) -> dict:
    if type(value) is not dict:
        raise HybridManifestError(f"{label} must be an object")
    unknown = value.keys() - fields
    missing = fields - value.keys()
    if unknown:
        raise HybridManifestError(f"{label} has unknown fields: {', '.join(sorted(unknown))}")
    if missing:
        raise HybridManifestError(f"{label} is missing fields: {', '.join(sorted(missing))}")
    return value


def _integer(value: object, label: str, minimum: int, maximum: int) -> int:
    if type(value) is not int:
        raise HybridManifestError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise HybridManifestError(f"{label} must be in {minimum}..{maximum}")
    return value


def _unique_object(pairs: list[tuple[str, object]]) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise HybridManifestError(f"duplicate JSON field: {key}")
        result[key] = value
    return result


def _invalid_constant(value: str) -> None:
    raise HybridManifestError(f"nonfinite JSON number is unsupported: {value}")


def _read_bounded(path: Path, limit: int, label: str) -> bytes:
    """Read a regular file with a hard allocation cap, even if it grows.

    Nonblocking open lets us reject a FIFO without waiting for a writer. The
    opened descriptor is checked so symlinks cannot bypass the regular-file
    requirement between path inspection and the bounded read.
    """

    descriptor = None
    try:
        descriptor = os.open(path, os.O_RDONLY | getattr(os, "O_NONBLOCK", 0))
        info = os.fstat(descriptor)
        if not stat.S_ISREG(info.st_mode):
            raise HybridManifestError(f"{label} must be a regular local file: {path}")
        if info.st_size > limit:
            raise HybridManifestError(f"{label} exceeds {limit} bytes: {path}")
        with os.fdopen(descriptor, "rb") as stream:
            descriptor = None
            payload = stream.read(limit + 1)
        if len(payload) > limit:
            raise HybridManifestError(f"{label} exceeds {limit} bytes: {path}")
        return payload
    except OSError as exc:
        raise HybridManifestError(f"cannot read {label} {path}: {exc.strerror}") from exc
    finally:
        if descriptor is not None:
            os.close(descriptor)


def _image_path(value: object, parent: Path) -> Path:
    if type(value) is not str or not value or "\x00" in value:
        raise HybridManifestError("routine image must name a relative local path")
    path = Path(value)
    # Absolute and URI paths do not have the documented manifest-relative
    # meaning. Parent components are ordinary relative local paths.
    if path.is_absolute() or PureWindowsPath(value).drive or "://" in value:
        raise HybridManifestError("routine image must name a relative local path")
    image = parent / path
    try:
        os.fsencode(image)
    except UnicodeError as exc:
        raise HybridManifestError("routine image path must be filesystem encodable") from exc
    return image


def _routine_metadata(value: object, parent: Path, index: int) -> tuple[dict, Path]:
    row = _object(value, _ROUTINE_FIELDS, f"routine {index}")
    name = row["name"]
    if (type(name) is not str or not 1 <= len(name) <= 127
            or any(not 0x21 <= ord(char) <= 0x7E for char in name)):
        raise HybridManifestError(
            "routine name must contain 1..127 printable nonwhitespace ASCII bytes")
    image = _image_path(row["image"], parent)
    input_cells = _integer(row["input_cells"], "input cells", 0, MAX_SIGNATURE_CELLS)
    output_cells = _integer(row["output_cells"], "output cells", 0, MAX_SIGNATURE_CELLS)
    entry_offset = _integer(row["entry_offset"], "entry offset", 0, MAX_CODE_BYTES - 1)
    max_instructions = _integer(row["max_instructions"], "per-call instructions", 1,
                                MAX_CALL_INSTRUCTIONS)
    return_stack_cells = _integer(row["return_stack_cells"], "return stack cells", 1,
                                  MAX_RETURN_STACK_CELLS)
    rows = row["buffers"]
    if type(rows) is not list or len(rows) > MAX_BUFFER_RULES:
        raise HybridManifestError("buffers must be an array of at most 16 rules")
    buffers = []
    for buffer_index, raw in enumerate(rows):
        fields = _object(raw, _BUFFER_FIELDS, f"routine {index} buffer {buffer_index}")
        try:
            rule = BufferRuleV1(**fields)
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(str(exc)) from exc
        if max(rule.address_argument, rule.length_argument) >= input_cells:
            raise HybridManifestError("buffer rule argument is outside the input signature")
        buffers.append(rule)
    return {
        "name": name,
        "entry_offset": entry_offset,
        "input_cells": input_cells,
        "output_cells": output_cells,
        "buffers": tuple(buffers),
        "max_instructions": max_instructions,
        "return_stack_cells": return_stack_cells,
    }, image


def _read_manifest(path: str | os.PathLike[str]) -> tuple[Path, object]:
    manifest_path = Path(path)
    payload = _read_bounded(manifest_path, MAX_MANIFEST_BYTES, "manifest")
    try:
        decoded = json.loads(payload.decode("utf-8"), object_pairs_hook=_unique_object,
                             parse_constant=_invalid_constant)
    except HybridManifestError:
        raise
    except (UnicodeError, ValueError, RecursionError) as exc:
        raise HybridManifestError(f"invalid manifest JSON: {exc}") from exc
    return manifest_path, decoded


def _load_manifest_v1(manifest_path: Path, decoded: object) -> RoutineManifestV1:
    root = _object(decoded, _MANIFEST_FIELDS, "manifest")
    if type(root["abi"]) is not str or root["abi"] != HYBRID_ABI:
        raise HybridManifestError("unsupported hybrid ABI identity")
    version = _integer(root["version"], "ABI version", 1, HYBRID_ABI_VERSION)
    dispatch_limit = _integer(root["dispatch_instruction_limit"], "dispatch instructions",
                              1, MAX_DISPATCH_INSTRUCTIONS)
    rows = root["routines"]
    if type(rows) is not list or len(rows) > MAX_ROUTINES:
        raise HybridManifestError("routines must be an array of at most 64 declarations")

    # Finish structural validation and name checks before opening any image.
    pending = []
    names = set()
    for index, row in enumerate(rows):
        fields, image = _routine_metadata(row, manifest_path.parent, index)
        key = fields["name"].upper()
        if key in names:
            raise HybridManifestError(f"duplicate routine name: {fields['name']}")
        names.add(key)
        pending.append((fields, image))

    routines = []
    total_bytes = 0
    for fields, image in pending:
        remaining = MAX_TOTAL_CODE_BYTES - total_bytes
        if remaining <= 0:
            raise HybridManifestError("total code image bytes exceed 16 MiB")
        code = _read_bounded(image, min(MAX_CODE_BYTES, remaining), "routine image")
        total_bytes += ((len(code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
        try:
            routines.append(RoutineImageV1(code=code, **fields))
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"routine {fields['name']}: {exc}") from exc
    return RoutineManifestV1(abi=root["abi"], version=version,
                             dispatch_instruction_limit=dispatch_limit,
                             routines=tuple(routines))


def _exports_metadata(value: object) -> tuple[CallbackExportV2, ...]:
    if type(value) is not list or len(value) > MAX_CALLBACK_EXPORTS:
        raise HybridManifestError("exports must be an array of at most 64 descriptors")
    exports = []
    identifiers = set()
    for index, raw in enumerate(value):
        fields = _object(raw, _EXPORT_FIELDS, f"export {index}")
        try:
            export = CallbackExportV2(**fields)
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(str(exc)) from exc
        if export.export_id in identifiers:
            raise HybridManifestError(f"duplicate callback export ID: {export.export_id}")
        identifiers.add(export.export_id)
        exports.append(export)
    return tuple(exports)


def _callbacks_metadata(value: object, exports: dict[int, CallbackExportV2 | CallbackExportV3],
                        routine_index: int, *, site_type=CallbackSiteV2) -> tuple:
    if type(value) is not list or len(value) > MAX_CALLBACK_SITES:
        raise HybridManifestError("callbacks must be an array of at most 16 sites")
    callbacks = []
    occupied = set()
    for index, raw in enumerate(value):
        fields = _object(raw, _CALLBACK_FIELDS, f"routine {routine_index} callback {index}")
        export_id = _integer(fields["export_id"], "callback export ID", 0,
                             MAX_CALLBACK_EXPORTS - 1)
        export = exports.get(export_id)
        if export is None:
            raise HybridManifestError(f"undeclared callback export ID: {export_id}")
        try:
            site = site_type(call_offset=fields["call_offset"],
                             stub_offset=fields["stub_offset"], export=export)
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(str(exc)) from exc
        offsets = (site.call_offset, site.call_offset + 1, site.stub_offset)
        if any(offset in occupied for offset in offsets):
            raise HybridManifestError("callback site byte spans must not overlap or repeat")
        occupied.update(offsets)
        callbacks.append(site)
    return tuple(callbacks)


def _load_manifest_v2(manifest_path: Path, decoded: object) -> RoutineManifestV2:
    root = _object(decoded, _MANIFEST_V2_FIELDS, "manifest")
    if type(root["abi"]) is not str or root["abi"] != HYBRID_ABI:
        raise HybridManifestError("unsupported hybrid ABI identity")
    version = _integer(root["version"], "ABI version", HYBRID_CALLBACK_ABI_VERSION,
                       HYBRID_CALLBACK_ABI_VERSION)
    dispatch_limit = _integer(root["dispatch_instruction_limit"], "dispatch instructions", 1,
                              MAX_DISPATCH_INSTRUCTIONS)
    callback_limit = _integer(root["dispatch_callback_limit"], "dispatch callback requests", 1,
                              MAX_DISPATCH_CALLBACKS)
    semantic_limit = _integer(root["dispatch_callback_semantic_limit"],
                              "dispatch callback semantic steps", 1,
                              MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)
    exports = _exports_metadata(root["exports"])
    by_id = {export.export_id: export for export in exports}
    rows = root["routines"]
    if type(rows) is not list or len(rows) > MAX_ROUTINES:
        raise HybridManifestError("routines must be an array of at most 64 declarations")

    # No image is opened until every independent field in every row is valid.
    # Site spans can be compared here without inspecting instruction bytes.
    pending = []
    names = set()
    for index, raw in enumerate(rows):
        row = _object(raw, _ROUTINE_V2_FIELDS, f"routine {index}")
        fields, image = _routine_metadata(
            {key: value for key, value in row.items() if key != "callbacks"},
            manifest_path.parent, index,
        )
        fields["callbacks"] = _callbacks_metadata(row["callbacks"], by_id, index)
        key = fields["name"].upper()
        if key in names:
            raise HybridManifestError(f"duplicate routine name: {fields['name']}")
        names.add(key)
        pending.append((fields, image))

    routines = []
    total_bytes = 0
    for fields, image in pending:
        remaining = MAX_TOTAL_CODE_BYTES - total_bytes
        if remaining <= 0:
            raise HybridManifestError("total code image bytes exceed 16 MiB")
        code = _read_bounded(image, min(MAX_CODE_BYTES, remaining), "routine image")
        total_bytes += ((len(code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
        try:
            routines.append(RoutineImageV2(code=code, **fields))
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"routine {fields['name']}: {exc}") from exc
    try:
        return RoutineManifestV2(
            abi=root["abi"], version=version, dispatch_instruction_limit=dispatch_limit,
            dispatch_callback_limit=callback_limit,
            dispatch_callback_semantic_limit=semantic_limit,
            exports=exports, routines=tuple(routines),
        )
    except (TypeError, ValueError) as exc:
        raise HybridManifestError(str(exc)) from exc


def _policies_metadata(value: object) -> tuple[tuple[ClosedPolicyV3, ...], tuple]:
    if type(value) is not list or len(value) > MAX_CLOSED_POLICIES:
        raise HybridManifestError("policies must be an array of at most 64 declarations")
    policies = []
    total_operations = 0
    for index, raw in enumerate(value):
        row = _object(raw, _POLICY_FIELDS, f"policy {index}")
        rows = row["operations"]
        if type(rows) is not list:
            raise HybridManifestError("policy operations must be an array")
        total_operations += len(rows)
        if total_operations > MAX_CLOSED_OPERATIONS:
            raise HybridManifestError("policy table may contain at most 4096 operations")
        operations = []
        for operation_index, raw_operation in enumerate(rows):
            label = f"policy {index} operation {operation_index}"
            if type(raw_operation) is not dict:
                raise HybridManifestError(f"{label} must be an object")
            if "op" not in raw_operation:
                raise HybridManifestError(f"{label} is missing fields: op")
            name = raw_operation["op"]
            if type(name) is not str or name not in _POLICY_OPERATIONS:
                raise HybridManifestError(f"{label} has an unsupported policy operation")
            operation_type, fields = _POLICY_OPERATIONS[name]
            arguments = _object(raw_operation, fields, label)
            try:
                operations.append(operation_type(**{
                    key: item for key, item in arguments.items() if key != "op"
                }))
            except (TypeError, ValueError) as exc:
                raise HybridManifestError(f"{label}: {exc}") from exc
        try:
            policies.append(ClosedPolicyV3(
                **{key: item for key, item in row.items() if key != "operations"},
                operations=tuple(operations),
            ))
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"policy {index}: {exc}") from exc
    try:
        values = tuple(policies)
        proofs = prove_policies(values)
        by_id = {policy.policy_id: policy for policy in values}
        for proof in proofs:
            policy = by_id[proof.policy_id]
            proof.validate_signature(policy.input_cells, policy.output_cells)
        return values, proofs
    except (TypeError, ValueError) as exc:
        raise HybridManifestError(str(exc)) from exc


def _exports_v3_metadata(value: object, policies: tuple[ClosedPolicyV3, ...],
                         proofs: tuple) -> tuple[CallbackExportV3, ...]:
    if type(value) is not list or len(value) > MAX_CALLBACK_EXPORTS:
        raise HybridManifestError("exports must be an array of at most 64 descriptors")
    policies_by_id = {policy.policy_id: policy for policy in policies}
    proofs_by_id = {proof.policy_id: proof for proof in proofs}
    exports = []
    identifiers = set()
    for index, raw in enumerate(value):
        label = f"export {index}"
        if type(raw) is not dict:
            raise HybridManifestError(f"{label} must be an object")
        if "effect" not in raw:
            raise HybridManifestError(f"{label} is missing fields: effect")
        effect = raw["effect"]
        if type(effect) is not str or effect not in ("integer_leaf", "closed_integer_colon"):
            raise HybridManifestError("callback effect must be integer_leaf or closed_integer_colon")
        fields = _object(raw, _LEAF_EXPORT_V3_FIELDS if effect == "integer_leaf"
                         else _CLOSED_EXPORT_V3_FIELDS, label)
        if effect == "closed_integer_colon":
            policy_id = _integer(fields["policy_id"], "policy ID", 0, MAX_CLOSED_POLICIES - 1)
            policy = policies_by_id.get(policy_id)
            if policy is None:
                raise HybridManifestError(f"undeclared policy ID: {policy_id}")
            fields = {key: item for key, item in fields.items() if key != "policy_id"}
            fields.update(name=policy.name, input_cells=policy.input_cells,
                          output_cells=policy.output_cells)
        try:
            export = CallbackExportV3(**fields)
            if effect == "closed_integer_colon":
                proofs_by_id[policy_id].validate_signature(
                    policy.input_cells, policy.output_cells,
                    max_semantic_steps=export.max_semantic_steps,
                )
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"{label}: {exc}") from exc
        if export.export_id in identifiers:
            raise HybridManifestError(f"duplicate callback export ID: {export.export_id}")
        identifiers.add(export.export_id)
        exports.append(export)
    return tuple(exports)


def _load_manifest_v3(manifest_path: Path, decoded: object) -> RoutineManifestV3:
    root = _object(decoded, _MANIFEST_V3_FIELDS, "manifest")
    if type(root["abi"]) is not str or root["abi"] != HYBRID_ABI:
        raise HybridManifestError("unsupported hybrid ABI identity")
    version = _integer(root["version"], "ABI version", HYBRID_CLOSED_ABI_VERSION,
                       HYBRID_CLOSED_ABI_VERSION)
    dispatch_limit = _integer(root["dispatch_instruction_limit"], "dispatch instructions", 1,
                              MAX_DISPATCH_INSTRUCTIONS)
    callback_limit = _integer(root["dispatch_callback_limit"], "dispatch callback requests", 1,
                              MAX_DISPATCH_CALLBACKS)
    semantic_limit = _integer(root["dispatch_callback_semantic_limit"],
                              "dispatch callback semantic steps", 1,
                              MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)
    policies, proofs = _policies_metadata(root["policies"])
    exports = _exports_v3_metadata(root["exports"], policies, proofs)
    by_id = {export.export_id: export for export in exports}
    rows = root["routines"]
    if type(rows) is not list or len(rows) > MAX_ROUTINES:
        raise HybridManifestError("routines must be an array of at most 64 declarations")
    pending = []
    policy_names = {policy.name.upper() for policy in policies}
    names = set()
    for index, raw in enumerate(rows):
        row = _object(raw, _ROUTINE_V2_FIELDS, f"routine {index}")
        fields, image = _routine_metadata(
            {key: item for key, item in row.items() if key != "callbacks"},
            manifest_path.parent, index,
        )
        fields["callbacks"] = _callbacks_metadata(
            row["callbacks"], by_id, index, site_type=CallbackSiteV3,
        )
        key = fields["name"].upper()
        if key in policy_names:
            raise HybridManifestError(f"policy and routine names collide: {fields['name']}")
        if key in names:
            raise HybridManifestError(f"duplicate routine name: {fields['name']}")
        names.add(key)
        pending.append((fields, image))

    # Entire policy proofs, export budgets and every independent routine field
    # have passed before this first code-image read. Publication remains later.
    routines = []
    total_bytes = 0
    for fields, image in pending:
        remaining = MAX_TOTAL_CODE_BYTES - total_bytes
        if remaining <= 0:
            raise HybridManifestError("total code image bytes exceed 16 MiB")
        code = _read_bounded(image, min(MAX_CODE_BYTES, remaining), "routine image")
        total_bytes += ((len(code) + CODE_ALIGNMENT - 1) // CODE_ALIGNMENT) * CODE_ALIGNMENT
        try:
            routines.append(RoutineImageV3(code=code, **fields))
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"routine {fields['name']}: {exc}") from exc
    try:
        return RoutineManifestV3(
            abi=root["abi"], version=version, dispatch_instruction_limit=dispatch_limit,
            dispatch_callback_limit=callback_limit,
            dispatch_callback_semantic_limit=semantic_limit,
            policies=policies, exports=exports, routines=tuple(routines),
        )
    except (TypeError, ValueError) as exc:
        raise HybridManifestError(str(exc)) from exc


def load_manifest_v1(path: str | os.PathLike[str]) -> RoutineManifestV1:
    """Load only strict v1 metadata and images, without publishing or executing.

    No runtime, dictionary, registration callback, or execution engine is
    accepted by this interface. A later failure cannot publish earlier images.
    """

    return _load_manifest_v1(*_read_manifest(path))


def load_manifest_v2(path: str | os.PathLike[str]) -> RoutineManifestV2:
    """Load only strict v2 callback metadata and bounded local code images.

    The result grants no callback authority. Native publication still proves
    instruction boundaries and encodings against the complete sealed image.
    """

    return _load_manifest_v2(*_read_manifest(path))


def load_manifest_v3(path: str | os.PathLike[str]) -> RoutineManifestV3:
    """Validate closed declarative policy proofs before reading bounded images.

    Loading creates only immutable values. It neither imports a runtime nor
    publishes dictionary words, evaluates source or grants export authority.
    """

    return _load_manifest_v3(*_read_manifest(path))


def load_manifest(path: str | os.PathLike[str]) -> RoutineManifestV1 | RoutineManifestV2 | RoutineManifestV3 | RoutineManifestV4:
    """Select a strict loader from one bounded read of the declared version."""

    manifest_path, decoded = _read_manifest(path)
    if type(decoded) is not dict:
        raise HybridManifestError("manifest must be an object")
    if "version" not in decoded:
        raise HybridManifestError("manifest is missing fields: version")
    version = _integer(decoded["version"], "ABI version", HYBRID_ABI_VERSION,
                       HYBRID_NESTED_ABI_VERSION)
    if version == HYBRID_ABI_VERSION:
        return _load_manifest_v1(manifest_path, decoded)
    if version == HYBRID_CALLBACK_ABI_VERSION:
        return _load_manifest_v2(manifest_path, decoded)
    if version == HYBRID_CLOSED_ABI_VERSION:
        return _load_manifest_v3(manifest_path, decoded)
    from hybrid.nested_manifest import _load_nested_manifest_v4

    return _load_nested_manifest_v4(manifest_path, decoded)


__all__ = ["HybridManifestError", "load_manifest", "load_manifest_v1", "load_manifest_v2", "load_manifest_v3"]
