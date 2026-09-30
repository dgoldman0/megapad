"""Explicit V5 service metadata loading, without startup or execution access.

The generic manifest loader selects this module for version 5 documents.
All independent metadata is checked before any bounded local image is read.
Native publication remains responsible for instruction/encoding validation.
"""

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
from shared.hybrid_services import (
    HYBRID_SERVICE_ABI_VERSION, CallbackSiteV5, RoutineImageV5,
    RoutineManifestV5, ServiceExportV5,
)


_MANIFEST_FIELDS = frozenset((
    "abi", "version", "dispatch_instruction_limit", "dispatch_callback_limit",
    "dispatch_callback_semantic_limit", "exports", "routines",
))
_EXPORT_FIELDS = frozenset((
    "export_id", "name", "input_cells", "output_cells", "effect", "max_semantic_steps",
))
_ROUTINE_FIELDS = frozenset((
    "name", "image", "entry_offset", "input_cells", "output_cells", "buffers",
    "max_instructions", "return_stack_cells", "callbacks", "max_callback_requests",
))


def _exports_metadata(value: object) -> tuple[ServiceExportV5, ...]:
    if type(value) is not list or len(value) > MAX_CALLBACK_EXPORTS:
        raise HybridManifestError("exports must be an array of at most 64 descriptors")
    exports = []
    identifiers = set()
    for index, raw in enumerate(value):
        fields = _object(raw, _EXPORT_FIELDS, f"service export {index}")
        try:
            export = ServiceExportV5(**fields)
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(str(exc)) from exc
        if export.export_id in identifiers:
            raise HybridManifestError(f"duplicate service export ID: {export.export_id}")
        identifiers.add(export.export_id)
        exports.append(export)
    return tuple(exports)


def _load_service_manifest_v5(manifest_path: Path, decoded: object) -> RoutineManifestV5:
    root = _object(decoded, _MANIFEST_FIELDS, "service manifest")
    if type(root["abi"]) is not str or root["abi"] != HYBRID_ABI:
        raise HybridManifestError("unsupported hybrid ABI identity")
    version = _integer(root["version"], "ABI version", HYBRID_SERVICE_ABI_VERSION,
                       HYBRID_SERVICE_ABI_VERSION)
    instruction_limit = _integer(root["dispatch_instruction_limit"], "dispatch instructions", 1,
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

    # Check every independent field, descriptor/reference, path, name and site
    # overlap, including unused entries, before the first image open.
    pending = []
    names = set()
    for index, raw in enumerate(rows):
        row = _object(raw, _ROUTINE_FIELDS, f"routine {index}")
        fields, image = _routine_metadata(
            {key: value for key, value in row.items()
             if key not in ("callbacks", "max_callback_requests")},
            manifest_path.parent, index,
        )
        fields["max_callback_requests"] = _integer(
            row["max_callback_requests"], "per-call callback requests", 0,
            MAX_DISPATCH_CALLBACKS,
        )
        fields["callbacks"] = _callbacks_metadata(
            row["callbacks"], by_id, index, site_type=CallbackSiteV5,
        )
        name = fields["name"].upper()
        if name in names:
            raise HybridManifestError(f"duplicate routine name: {fields['name']}")
        names.add(name)
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
            routines.append(RoutineImageV5(code=code, **fields))
        except (TypeError, ValueError) as exc:
            raise HybridManifestError(f"routine {fields['name']}: {exc}") from exc
    try:
        return RoutineManifestV5(
            abi=root["abi"], version=version, dispatch_instruction_limit=instruction_limit,
            dispatch_callback_limit=callback_limit,
            dispatch_callback_semantic_limit=semantic_limit,
            exports=exports, routines=tuple(routines),
        )
    except (TypeError, ValueError) as exc:
        raise HybridManifestError(str(exc)) from exc


def load_service_manifest_v5(path: str | os.PathLike[str]) -> RoutineManifestV5:
    """Read and validate V5 metadata/images once; publish and execute nothing.

    This explicit loader does not advertise server/runtime service support.
    Its returned descriptors and result observations carry no owner authority.
    """

    return _load_service_manifest_v5(*_read_manifest(path))


__all__ = ["HybridManifestError", "load_service_manifest_v5"]
