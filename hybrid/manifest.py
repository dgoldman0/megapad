"""The manifest format for declared hybrid machine routines.

A manifest lists MP64 routine images, the cells each passes in r4-r11, the
arguments that name memory buffers, and the call sites where the machine runs
a named Forth word.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path


ABI = "megapad.hybrid.routines"
VERSION = 1
ACCESS_MODES = ("read", "write", "read_write")
REGISTER_CELLS = 8  # r4-r11 carry arguments, results and callback cells


class HybridManifestError(ValueError):
    """A manifest or routine declaration is malformed."""


def _count(value, label, *, maximum=None, minimum=0):
    if type(value) is not int:
        raise HybridManifestError(f"{label} must be an integer")
    if value < minimum or (maximum is not None and value > maximum):
        bound = f" through {maximum}" if maximum is not None else " or more"
        raise HybridManifestError(f"{label} must be {minimum}{bound}")
    return value


def _word_name(value, label):
    if type(value) is not str or not value or not value.isascii() or any(
            character.isspace() for character in value):
        raise HybridManifestError(f"{label} must be a nonempty ASCII word name")
    return value


@dataclass(frozen=True, slots=True)
class BufferRule:
    """An argument pair naming memory the routine may read or write.

    The span starts at the address argument and is the length argument times
    ``element_bytes`` long.
    """

    address_argument: int
    length_argument: int
    element_bytes: int = 1
    access: str = "read"
    max_bytes: int | None = None

    def __post_init__(self):
        _count(self.address_argument, "buffer address_argument", maximum=REGISTER_CELLS - 1)
        _count(self.length_argument, "buffer length_argument", maximum=REGISTER_CELLS - 1)
        _count(self.element_bytes, "buffer element_bytes", minimum=1)
        if self.access not in ACCESS_MODES:
            raise HybridManifestError("buffer access must be read, write or read_write")
        if self.max_bytes is not None:
            _count(self.max_bytes, "buffer max_bytes", minimum=1)


@dataclass(frozen=True, slots=True)
class CallbackSite:
    """A CALL.L whose target is a RET.L stub; the machine runs ``target`` there."""

    call_offset: int
    stub_offset: int
    target: str
    input_cells: int = 0
    output_cells: int = 0

    def __post_init__(self):
        _count(self.call_offset, "callback call_offset")
        _count(self.stub_offset, "callback stub_offset")
        _word_name(self.target, "callback target")
        _count(self.input_cells, "callback input_cells", maximum=REGISTER_CELLS)
        _count(self.output_cells, "callback output_cells", maximum=REGISTER_CELLS)


@dataclass(frozen=True, slots=True)
class RoutineDeclaration:
    """One machine routine and the Forth word that runs it."""

    name: str
    code: bytes
    entry_offset: int = 0
    input_cells: int = 0
    output_cells: int = 0
    buffers: tuple[BufferRule, ...] = ()
    callbacks: tuple[CallbackSite, ...] = ()

    def __post_init__(self):
        _word_name(self.name, "routine name")
        if type(self.code) is not bytes or not self.code:
            raise HybridManifestError("routine code must be nonempty bytes")
        _count(self.entry_offset, "entry_offset", maximum=len(self.code) - 1)
        _count(self.input_cells, "input_cells", maximum=REGISTER_CELLS)
        _count(self.output_cells, "output_cells", maximum=REGISTER_CELLS)
        if type(self.buffers) is not tuple or any(type(rule) is not BufferRule for rule in self.buffers):
            raise HybridManifestError("buffers must be a tuple of BufferRule")
        if type(self.callbacks) is not tuple or any(
                type(site) is not CallbackSite for site in self.callbacks):
            raise HybridManifestError("callbacks must be a tuple of CallbackSite")
        for rule in self.buffers:
            if max(rule.address_argument, rule.length_argument) >= self.input_cells:
                raise HybridManifestError(f"{self.name}: a buffer names an argument it does not take")
        for site in self.callbacks:
            if site.call_offset >= len(self.code) or site.stub_offset >= len(self.code):
                raise HybridManifestError(f"{self.name}: a callback site lies outside the code")


@dataclass(frozen=True, slots=True)
class RoutineManifest:
    routines: tuple[RoutineDeclaration, ...]

    def __post_init__(self):
        names = [routine.name.upper() for routine in self.routines]
        if len(set(names)) != len(names):
            raise HybridManifestError("routine names must be distinct")


def _fields(value, label, required, optional=()):
    if type(value) is not dict:
        raise HybridManifestError(f"{label} must be a JSON object")
    unknown = set(value) - set(required) - set(optional)
    if unknown:
        raise HybridManifestError(f"{label} has unknown fields: {', '.join(sorted(unknown))}")
    missing = [name for name in required if name not in value]
    if missing:
        raise HybridManifestError(f"{label} is missing: {', '.join(missing)}")
    return value


def _list(value, label):
    if type(value) is not list:
        raise HybridManifestError(f"{label} must be a JSON list")
    return value


def load_manifest(path: str | Path) -> RoutineManifest:
    """Read a manifest and the routine images it names, relative to it."""

    path = Path(path)
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise HybridManifestError(f"cannot read manifest {path}: {error}") from error
    _fields(document, "manifest", ("abi", "version", "routines"))
    if document["abi"] != ABI or document["version"] != VERSION:
        raise HybridManifestError(f"manifest must declare abi {ABI!r} version {VERSION}")
    routines = []
    for index, item in enumerate(_list(document["routines"], "routines")):
        label = f"routine {index}"
        _fields(item, label, ("name", "image"),
                ("entry_offset", "input_cells", "output_cells", "buffers", "callbacks"))
        image = item["image"]
        if type(image) is not str or not image:
            raise HybridManifestError(f"{label} image must be a path")
        image_path = (path.parent / image).resolve()
        try:
            code = image_path.read_bytes()
        except OSError as error:
            raise HybridManifestError(f"{label} image cannot be read: {error}") from error
        buffers = tuple(BufferRule(**_fields(rule, f"{label} buffer",
                                             ("address_argument", "length_argument"),
                                             ("element_bytes", "access", "max_bytes")))
                        for rule in _list(item.get("buffers", []), f"{label} buffers"))
        callbacks = tuple(CallbackSite(**_fields(site, f"{label} callback",
                                                 ("call_offset", "stub_offset", "target"),
                                                 ("input_cells", "output_cells")))
                          for site in _list(item.get("callbacks", []), f"{label} callbacks"))
        routines.append(RoutineDeclaration(
            name=item["name"], code=code,
            entry_offset=item.get("entry_offset", 0),
            input_cells=item.get("input_cells", 0),
            output_cells=item.get("output_cells", 0),
            buffers=buffers, callbacks=callbacks,
        ))
    return RoutineManifest(tuple(routines))


__all__ = [
    "ABI", "VERSION", "BufferRule", "CallbackSite", "HybridManifestError",
    "RoutineDeclaration", "RoutineManifest", "load_manifest",
]
