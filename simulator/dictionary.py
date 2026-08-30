"""Source-visible dictionary state for the hosted Forth simulator.

The semantic dictionary preserves the native dictionary's no-padding header
geometry::

    link cell (8 bytes), flags/length byte, name bytes, code field

The code field is not MP64 machine code.  Each definition reserves one cell at
its execution-token address as a semantic code slot.  The slot gives every
definition a stable, numeric guest address while its opaque implementation is
kept in host metadata.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from shared.cells import CELL_BYTES, MASK64


HEADER_FIXED_BYTES = CELL_BYTES + 1
MAX_NAME_BYTES = 0x7F
SEMANTIC_CODE_SLOT_BYTES = CELL_BYTES


def _address(value: int, *, label: str) -> int:
    if not isinstance(value, int):
        raise TypeError(f"{label} must be an integer")
    if not 0 <= value <= MASK64:
        raise ValueError(f"{label} is outside the uint64 address space")
    return value


def _name_bytes(name: bytes | str) -> bytes:
    if isinstance(name, str):
        try:
            raw = name.encode("ascii")
        except UnicodeEncodeError as exc:
            raise ValueError("dictionary names must be ASCII") from exc
    elif isinstance(name, bytes):
        raw = name
    else:
        raise TypeError("dictionary names must be bytes or str")

    if not raw:
        raise ValueError("dictionary names must not be empty")
    if len(raw) > MAX_NAME_BYTES:
        raise ValueError("dictionary names may contain at most 127 bytes")
    if not raw.isascii():
        raise ValueError("dictionary names must be ASCII")
    return raw


def _binding_key(name: bytes | str) -> bytes:
    return _name_bytes(name).upper()


@dataclass(frozen=True, slots=True)
class Word:
    """One semantic definition and its source-visible dictionary addresses."""

    name: bytes
    header_address: int
    xt: int
    immediate: bool
    implementation: Any = field(compare=False, repr=False)


@dataclass(frozen=True, slots=True)
class DictionaryCheckpoint:
    """Opaque, lineage-checked dictionary rollback point."""

    here: int
    latest: int
    _depth: int = field(repr=False)
    _tail: Word | None = field(repr=False)
    _owner: object = field(repr=False, compare=False)


class Dictionary:
    """An unbounded-by-count semantic dictionary in a uint64 address space."""

    def __init__(self, start_address: int = 0x1000) -> None:
        self._start_address = _address(start_address, label="start address")
        self._here = self._start_address
        self._definitions: list[Word] = []
        self._bindings: dict[bytes, list[Word]] = {}
        self._by_xt: dict[int, Word] = {}
        self._owner = object()

    @property
    def here(self) -> int:
        """Return the first free byte address in the active dictionary zone."""

        return self._here

    @property
    def latest(self) -> int:
        """Return the newest header address, or zero for an empty dictionary."""

        if not self._definitions:
            return 0
        return self._definitions[-1].header_address

    def define(
        self,
        name: bytes | str,
        implementation: Any = None,
        *,
        immediate: bool = False,
    ) -> Word:
        """Publish a definition and return its stable semantic word record."""

        raw_name = _name_bytes(name)
        allocation_size = (
            HEADER_FIXED_BYTES + len(raw_name) + SEMANTIC_CODE_SLOT_BYTES
        )
        if self._here > MASK64 - allocation_size:
            raise OverflowError("dictionary definition would wrap uint64 addresses")

        header_address = self._here
        xt = header_address + HEADER_FIXED_BYTES + len(raw_name)
        if xt == 0:
            # The non-wrapping checks and nonempty name make this unreachable,
            # but keep the execution-token invariant explicit at publication.
            raise OverflowError("dictionary definition produced execution token zero")

        word = Word(
            name=raw_name,
            header_address=header_address,
            xt=xt,
            immediate=bool(immediate),
            implementation=implementation,
        )
        key = raw_name.upper()
        self._definitions.append(word)
        self._bindings.setdefault(key, []).append(word)
        self._by_xt[xt] = word
        self._here = header_address + allocation_size
        return word

    def find(self, name: bytes | str) -> Word | None:
        """Return the newest case-insensitive binding for *name*, if present."""

        bindings = self._bindings.get(_binding_key(name))
        return bindings[-1] if bindings else None

    def resolve(self, xt: int) -> Word:
        """Resolve a live execution token or raise ``KeyError``."""

        token = _address(xt, label="execution token")
        try:
            return self._by_xt[token]
        except KeyError:
            raise KeyError(f"unknown execution token 0x{token:016x}") from None

    def checkpoint(self) -> DictionaryCheckpoint:
        """Capture the active contiguous dictionary state for later rollback."""

        tail = self._definitions[-1] if self._definitions else None
        return DictionaryCheckpoint(
            here=self._here,
            latest=self.latest,
            _depth=len(self._definitions),
            _tail=tail,
            _owner=self._owner,
        )

    def rollback(self, checkpoint: DictionaryCheckpoint) -> None:
        """Remove definitions after a checkpoint and restore prior bindings."""

        if not isinstance(checkpoint, DictionaryCheckpoint):
            raise TypeError("rollback requires a DictionaryCheckpoint")
        if checkpoint._owner is not self._owner:
            raise ValueError("checkpoint belongs to another dictionary")
        if checkpoint._depth > len(self._definitions):
            raise ValueError("checkpoint is not in the active dictionary history")
        if checkpoint._depth:
            if self._definitions[checkpoint._depth - 1] is not checkpoint._tail:
                raise ValueError("checkpoint is not in the active dictionary history")
        elif checkpoint._tail is not None:
            raise ValueError("invalid empty-dictionary checkpoint")

        removed = self._definitions[checkpoint._depth :]
        for word in reversed(removed):
            key = word.name.upper()
            bindings = self._bindings[key]
            if bindings[-1] is not word:
                raise RuntimeError("dictionary binding history is inconsistent")
            bindings.pop()
            if not bindings:
                del self._bindings[key]
            del self._by_xt[word.xt]

        del self._definitions[checkpoint._depth :]
        self._here = checkpoint.here


__all__ = [
    "Dictionary",
    "DictionaryCheckpoint",
    "HEADER_FIXED_BYTES",
    "MAX_NAME_BYTES",
    "SEMANTIC_CODE_SLOT_BYTES",
    "Word",
]
