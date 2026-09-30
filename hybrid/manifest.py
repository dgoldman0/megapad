"""Bounded local manifest loading, with no registration or code execution."""

from __future__ import annotations

import json
import os
from pathlib import Path, PureWindowsPath
import stat

from shared.hybrid_abi import (
    HYBRID_ABI,
    HYBRID_ABI_VERSION,
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
    BufferRuleV1,
    RoutineImageV1,
    RoutineManifestV1,
)


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


def load_manifest_v1(path: str | os.PathLike[str]) -> RoutineManifestV1:
    """Validate all local images and return immutable, unpublished values.

    No runtime, dictionary, registration callback, or execution engine is
    accepted by this interface. An invalid later image cannot publish earlier
    routines because there is no publication operation in this loader.
    """

    manifest_path = Path(path)
    payload = _read_bounded(manifest_path, MAX_MANIFEST_BYTES, "manifest")
    try:
        decoded = json.loads(payload.decode("utf-8"), object_pairs_hook=_unique_object,
                             parse_constant=_invalid_constant)
    except HybridManifestError:
        raise
    except (UnicodeError, ValueError, RecursionError) as exc:
        raise HybridManifestError(f"invalid manifest JSON: {exc}") from exc
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


__all__ = ["HybridManifestError", "load_manifest_v1"]
