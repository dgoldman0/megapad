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

from shared.cells import CELL_BYTES, MASK64, s64
from simulator.memory import SparseAddressSpace


HEADER_FIXED_BYTES = CELL_BYTES + 1
MAX_NAME_BYTES = 0x7F
IMMEDIATE_FLAG = 0x80
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

    @property
    def body_address(self) -> int:
        """Return the first byte following this word's semantic code slot."""

        return self.xt + SEMANTIC_CODE_SLOT_BYTES


@dataclass(frozen=True, slots=True)
class _DictionaryCheckpointSeal:
    """Canonical state that detects copied checkpoints with altered fields."""

    here: int
    latest: int
    depth: int
    tail: Word | None
    owner: object = field(compare=False)


@dataclass(frozen=True, slots=True)
class DictionaryCheckpoint:
    """Opaque, lineage-checked dictionary rollback point."""

    here: int
    latest: int
    _seal: _DictionaryCheckpointSeal = field(repr=False, compare=False)


class Dictionary:
    """An unbounded-by-count semantic dictionary in a uint64 address space."""

    def __init__(
        self,
        start_address: int = 0x1000,
        *,
        memory: SparseAddressSpace | None = None,
    ) -> None:
        self._start_address = _address(start_address, label="start address")
        if self._start_address == 0:
            raise ValueError(
                "dictionary start address must be nonzero because zero is "
                "the empty-link sentinel"
            )
        if memory is not None and not isinstance(memory, SparseAddressSpace):
            raise TypeError("memory must be a SparseAddressSpace or None")
        self._memory = memory
        self._active_limit = MASK64
        if memory is not None:
            containing_region = next(
                (
                    region
                    for region in memory.regions
                    if region.base <= self._start_address < region.limit
                ),
                None,
            )
            if containing_region is None:
                raise ValueError(
                    "dictionary start address must lie in mapped ordinary memory"
                )
            self._active_limit = containing_region.limit
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

    @property
    def latest_word(self) -> Word | None:
        """Return the newest live word, or ``None`` for an empty dictionary."""

        return self._definitions[-1] if self._definitions else None

    def define(
        self,
        name: bytes | str,
        implementation: Any = None,
        *,
        immediate: bool = False,
        initial_body: bytes = b"",
    ) -> Word:
        """Publish a definition and return its stable semantic word record."""

        raw_name = _name_bytes(name)
        if not isinstance(initial_body, bytes):
            raise TypeError("dictionary initial body must be bytes")
        allocation_size = (
            HEADER_FIXED_BYTES + len(raw_name) + SEMANTIC_CODE_SLOT_BYTES
            + len(initial_body)
        )
        allocation_limit = self._checked_advance(
            allocation_size,
            operation="dictionary definition",
        )

        header_address = self._here
        xt = header_address + HEADER_FIXED_BYTES + len(raw_name)
        if xt == 0:
            # The non-wrapping checks and nonempty name make this unreachable,
            # but keep the execution-token invariant explicit at publication.
            raise OverflowError("dictionary definition produced execution token zero")

        for live_word in self._definitions:
            if (
                header_address < live_word.body_address
                and live_word.header_address < allocation_limit
            ):
                raise ValueError(
                    "dictionary definition would overlap a live header or code slot"
                )

        link = self.latest
        flags_length = len(raw_name) | (IMMEDIATE_FLAG if immediate else 0)
        header = (
            link.to_bytes(CELL_BYTES, "little")
            + bytes((flags_length,))
            + raw_name
            + bytes(SEMANTIC_CODE_SLOT_BYTES)
            + initial_body
        )
        if self._memory is not None:
            # Publish metadata only after the complete guest-visible header has
            # passed memory preflight and been emitted contiguously.
            self._memory.write_bytes(header_address, header)

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
        self._here = allocation_limit
        return word

    def allot(self, delta_cell: int) -> None:
        """Move ``HERE`` by one signed-cell delta without touching memory."""

        if not isinstance(delta_cell, int):
            raise TypeError("ALLOT delta must be an integer cell")
        delta = s64(delta_cell)
        candidate = self._here + delta
        if candidate < self._start_address:
            raise OverflowError("ALLOT would move HERE below the dictionary start")
        if candidate > MASK64:
            raise OverflowError("ALLOT would wrap the uint64 address space")
        if candidate > self._active_limit:
            raise OverflowError("ALLOT would move HERE beyond its memory region")
        self._here = candidate

    def comma(self, cell: int) -> None:
        """Store one little-endian cell at ``HERE`` and advance atomically."""

        self._store_and_advance(cell, CELL_BYTES)

    def c_comma(self, cell: int) -> None:
        """Store the low byte of one cell at ``HERE`` and advance atomically."""

        self._store_and_advance(cell, 1)

    def write_transient(self, payload: bytes) -> int:
        """Write caller bytes at ``HERE`` without advancing the frontier.

        BIOS ``WORD`` owns this deliberately ephemeral dictionary-tail
        surface.  A later definition or comma operation may overwrite it.
        """

        if not isinstance(payload, bytes):
            raise TypeError("transient dictionary payload must be bytes")
        if self._memory is None:
            raise RuntimeError(
                "transient dictionary writes require a shared address space"
            )
        self._checked_advance(
            len(payload),
            operation="transient dictionary write",
        )
        self._memory.write_bytes(self._here, payload)
        return self._here

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
        seal = _DictionaryCheckpointSeal(
            here=self._here,
            latest=self.latest,
            depth=len(self._definitions),
            tail=tail,
            owner=self._owner,
        )
        return DictionaryCheckpoint(
            here=seal.here,
            latest=seal.latest,
            _seal=seal,
        )

    def rollback(self, checkpoint: DictionaryCheckpoint) -> None:
        """Remove definitions after a checkpoint and restore prior bindings."""

        if not isinstance(checkpoint, DictionaryCheckpoint):
            raise TypeError("rollback requires a DictionaryCheckpoint")
        seal = checkpoint._seal
        if seal.owner is not self._owner:
            raise ValueError("checkpoint belongs to another dictionary")
        if checkpoint.here != seal.here or checkpoint.latest != seal.latest:
            raise ValueError("checkpoint coordinates do not match its sealed state")
        if not self._start_address <= seal.here <= self._active_limit:
            raise ValueError("checkpoint HERE is outside the dictionary region")
        if not 0 <= seal.depth <= len(self._definitions):
            raise ValueError("checkpoint is not in the active dictionary history")
        if seal.here > self._here:
            raise ValueError("checkpoint HERE is ahead of the active dictionary")
        expected_latest = 0 if seal.tail is None else seal.tail.header_address
        if seal.latest != expected_latest:
            raise ValueError("checkpoint LATEST does not match its sealed tail")
        if seal.depth:
            if self._definitions[seal.depth - 1] is not seal.tail:
                raise ValueError("checkpoint is not in the active dictionary history")
        elif seal.tail is not None:
            raise ValueError("invalid empty-dictionary checkpoint")

        removed = self._definitions[seal.depth :]
        for word in reversed(removed):
            key = word.name.upper()
            bindings = self._bindings[key]
            if bindings[-1] is not word:
                raise RuntimeError("dictionary binding history is inconsistent")
            bindings.pop()
            if not bindings:
                del self._bindings[key]
            del self._by_xt[word.xt]

        del self._definitions[seal.depth :]
        self._here = seal.here

    def _checked_advance(self, width: int, *, operation: str) -> int:
        if self._here > MASK64 - width:
            raise OverflowError(f"{operation} would wrap uint64 addresses")
        candidate = self._here + width
        if candidate > self._active_limit:
            raise OverflowError(f"{operation} would exceed its memory region")
        return candidate

    def _store_and_advance(self, cell: int, width: int) -> None:
        if self._memory is None:
            raise RuntimeError("dictionary stores require a shared address space")
        candidate = self._checked_advance(width, operation="dictionary store")
        if width == CELL_BYTES:
            self._memory.write64(self._here, cell)
        else:
            self._memory.write8(self._here, cell)
        self._here = candidate


__all__ = [
    "Dictionary",
    "DictionaryCheckpoint",
    "HEADER_FIXED_BYTES",
    "IMMEDIATE_FLAG",
    "MAX_NAME_BYTES",
    "SEMANTIC_CODE_SLOT_BYTES",
    "Word",
]
