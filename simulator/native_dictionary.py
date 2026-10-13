"""The native backend's dictionary, behind the ``Dictionary`` interface.

With the native backend, ``_megaforth_native.NativeDictionary`` owns all
host-side dictionary metadata: definition order, bindings, the XT map,
``HERE`` and its zone, body allocations, the execution generation and the
caller-backed side index.  It writes guest headers through the same region
views as the native executor.  This module presents that state through the
exact interface of :class:`simulator.dictionary.Dictionary` and
:class:`simulator.dictionary_index.HostedDictionaryIndex`, which remain the
Python backend's reference implementations.

Python sees each definition through one cached :class:`Word` view, created on
first access, so identity-bound overlays and leases keep working.  A created
word's view carries a :class:`NativeCreatedDefinition`, whose ``DOES>``
action is stored natively as well.
"""

from __future__ import annotations

from typing import Any, Callable

from shared.cells import CELL_BYTES, MASK64, s64
from simulator.dictionary import (
    HEADER_FIXED_BYTES,
    SEMANTIC_CODE_SLOT_BYTES,
    BodyAllocationLease,
    DictionaryCheckpoint,
    Word,
    _DictionaryCheckpointSeal,
    _address,
    _name_bytes,
)
from simulator.dictionary_index import DictionaryIndexState
from simulator.memory import AddressClass, SparseAddressSpace
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    DirectiveDefinition,
    DoesBodyRef,
    PrimitiveDefinition,
    RoutineDefinition,
    ValueDefinition,
)


class NativeCreatedDefinition(CreatedDefinition):
    """A created word's mutable action, kept in step with its native record."""

    __slots__ = ("_native", "_xt", "_uid", "_action")

    @classmethod
    def _bind(
        cls,
        native: Any,
        xt: int,
        uid: int,
        action: DoesBodyRef | None,
    ) -> "NativeCreatedDefinition":
        self = object.__new__(cls)
        object.__setattr__(self, "_native", native)
        object.__setattr__(self, "_xt", xt)
        object.__setattr__(self, "_uid", uid)
        object.__setattr__(self, "_action", action)
        return self

    @property
    def action(self) -> DoesBodyRef | None:
        return self._action

    @action.setter
    def action(self, value: DoesBodyRef | None) -> None:
        object.__setattr__(self, "_action", value)
        if value is None:
            self._native.set_action(self._xt, self._uid, False, 0, 0)
        elif isinstance(value, DoesBodyRef):
            self._native.set_action(
                self._xt, self._uid, True, value.source_xt, value.entry_ip
            )
        else:
            # Not an action the native code can interpret: it only knows
            # that the word is no longer a plain created word.
            self._native.set_action(self._xt, self._uid, True, 0, 0)


def _classify(implementation: Any) -> tuple[int, int, int, bool, int, int, Any]:
    """Return the native kind, its data, and the object to keep for Python."""

    native = _extension()
    if isinstance(implementation, ColonDefinition):
        return native.KIND_COLON, 0, 0, False, 0, 0, implementation
    if isinstance(implementation, PrimitiveDefinition):
        return native.KIND_PRIMITIVE, 0, 0, False, 0, 0, implementation
    if isinstance(implementation, ConstantDefinition):
        return native.KIND_CONSTANT, implementation.value, 0, False, 0, 0, implementation
    if isinstance(implementation, ValueDefinition):
        return native.KIND_VALUE, 0, 0, False, 0, 0, implementation
    if isinstance(implementation, CreatedDefinition):
        action = implementation.action
        if action is None:
            return native.KIND_CREATED, 0, 0, False, 0, 0, None
        if isinstance(action, DoesBodyRef):
            return (
                native.KIND_CREATED, 0, 0, True,
                action.source_xt, action.entry_ip, None,
            )
        return native.KIND_CREATED, 0, 0, True, 0, 0, None
    if isinstance(implementation, DirectiveDefinition):
        return (
            native.KIND_DIRECTIVE, 0, implementation.kind.value, False, 0, 0,
            implementation,
        )
    if isinstance(implementation, RoutineDefinition):
        return native.KIND_ROUTINE, 0, 0, False, 0, 0, implementation
    return native.KIND_OPAQUE, 0, 0, False, 0, 0, implementation


_EXTENSION: Any = None


def _extension() -> Any:
    global _EXTENSION
    if _EXTENSION is None:
        import _megaforth_native

        _EXTENSION = _megaforth_native
    return _EXTENSION


def _regions(memory: SparseAddressSpace) -> tuple[list, list]:
    if memory.dense_backing is None:
        return [
            (region.spec.base, region.spec.size, region.pages)
            for region in memory._regions
        ], []
    return [], [
        (spec.base, spec.size, memory.dense_backing.buffer_at(spec.base))
        for spec in memory.regions
    ]


class NativeDictionary:
    """The ``Dictionary`` interface over ``_megaforth_native.NativeDictionary``."""

    def __init__(
        self,
        start_address: int = 0x1000,
        *,
        memory: SparseAddressSpace | None = None,
        mutation_guard: Callable[[str], None] | None = None,
    ) -> None:
        start = _address(start_address, label="start address")
        if start == 0:
            raise ValueError(
                "dictionary start address must be nonzero because zero is "
                "the empty-link sentinel"
            )
        if memory is not None and not isinstance(memory, SparseAddressSpace):
            raise TypeError("memory must be a SparseAddressSpace or None")
        if mutation_guard is not None and not callable(mutation_guard):
            raise TypeError("dictionary mutation guard must be callable or None")
        self._memory = memory
        self._mutation_guard = mutation_guard
        active_limit = MASK64
        regions: list = []
        dense: list = []
        external = None
        if memory is not None:
            containing_region = next(
                (
                    region
                    for region in memory.regions
                    if region.base <= start < region.limit
                ),
                None,
            )
            if containing_region is None:
                raise ValueError(
                    "dictionary start address must lie in mapped ordinary memory"
                )
            active_limit = containing_region.limit
            regions, dense = _regions(memory)
            external = next(
                (
                    region
                    for region in memory.regions
                    if region.kind is AddressClass.EXTERNAL
                ),
                None,
            )
        self._owner = object()
        self._leases: dict[int, BodyAllocationLease] = {}
        # Live views by XT. Every removal goes through this facade, which
        # drops the removed XTs, so a hit is always the live definition.
        self._by_xt: dict[int, Word] = {}
        self._native = _extension().NativeDictionary(
            start,
            active_limit,
            memory,
            regions,
            1 if memory is None else memory.page_size,
            dense,
            external is not None,
            0 if external is None else external.base,
            0 if external is None else external.limit,
        )

    @property
    def native(self) -> Any:
        """The native dictionary object, for other native components."""

        return self._native

    @property
    def execution_generation(self) -> int:
        """Invalidate execution plans after definition publication or removal."""

        return self._native.execution_generation

    @property
    def here(self) -> int:
        """Return the first free byte address in the active dictionary zone."""

        return self._native.here

    @property
    def start_address(self) -> int:
        """Return the immutable first address of the hosted Bank-0 dictionary."""

        return self._native.start_address

    @property
    def active_zone(self) -> tuple[int, int]:
        """Return the inclusive/exclusive storage interval selected for HERE."""

        return self._native.active_floor, self._native.active_limit

    @property
    def latest(self) -> int:
        """Return the newest header address, or zero for an empty dictionary."""

        return self._native.latest

    @property
    def latest_word(self) -> Word | None:
        """Return the newest live word, or ``None`` for an empty dictionary."""

        count = len(self._native)
        return self._view(count - 1) if count else None

    @property
    def words(self) -> tuple[Word, ...]:
        """Return the live definition order from oldest to newest."""

        views = self._native.views()
        return tuple(
            self._make_view(view) if type(view) is int else view for view in views
        )

    @property
    def numeric_rollback_floor(self) -> int:
        """Return the lowest HERE accepted by the source-visible rollback ABI."""

        return self._native.numeric_rollback_floor

    def protect_current_prefix_from_numeric_rollback(self) -> None:
        """Keep the current dictionary prefix below every numeric checkpoint."""

        self._guard_mutation("change the dictionary rollback floor")
        self._native.protect_current_prefix()

    def define(
        self,
        name: bytes | str,
        implementation: Any = None,
        *,
        immediate: bool = False,
        initial_body: bytes = b"",
    ) -> Word:
        """Publish a definition and return its stable semantic word record."""

        self._guard_mutation("define a dictionary word")
        raw_name = _name_bytes(name)
        if not isinstance(initial_body, bytes):
            raise TypeError("dictionary initial body must be bytes")
        kind, constant, directive, has_action, action_xt, action_ip, kept = (
            _classify(implementation)
        )
        index, header_address, xt, uid = self._native.define(
            raw_name, initial_body, bool(immediate), kind, constant, directive,
            has_action, action_xt, action_ip, kept,
        )
        if kept is None and isinstance(implementation, CreatedDefinition):
            implementation = NativeCreatedDefinition._bind(
                self._native, xt, uid, implementation.action
            )
        word = Word(
            name=raw_name,
            header_address=header_address,
            xt=xt,
            immediate=bool(immediate),
            implementation=implementation,
        )
        self._native.set_view(index, word)
        self._by_xt[xt] = word
        return word

    def definition_size(
        self,
        name: bytes | str,
        *,
        initial_body: bytes = b"",
    ) -> int:
        """Return the exact hosted byte span for one prospective definition."""

        raw_name = _name_bytes(name)
        if not isinstance(initial_body, bytes):
            raise TypeError("dictionary initial body must be bytes")
        return (
            HEADER_FIXED_BYTES
            + len(raw_name)
            + SEMANTIC_CODE_SLOT_BYTES
            + len(initial_body)
        )

    def allot(self, delta_cell: int) -> None:
        """Move ``HERE`` by one signed-cell delta without touching memory."""

        self._guard_mutation("allot dictionary storage")
        if not isinstance(delta_cell, int):
            raise TypeError("ALLOT delta must be an integer cell")
        self._native.allot(s64(delta_cell))

    def move_here(self, target: int, *, floor: int, limit: int) -> None:
        """Select one mapped dictionary zone and move ``HERE`` into it."""

        self._guard_mutation("move the dictionary frontier")
        target = _address(target, label="dictionary target")
        floor = _address(floor, label="dictionary zone floor")
        limit = _address(limit, label="dictionary zone limit")
        if floor >= limit:
            raise ValueError("dictionary zone must be nonempty")
        if not floor <= target <= limit:
            raise ValueError("dictionary target is outside the selected zone")
        if self._memory is not None and not any(
            region.base <= floor and limit <= region.limit
            for region in self._memory.regions
        ):
            raise ValueError(
                "dictionary zone must fit one mapped ordinary-memory region"
            )
        self._native.move_here(target, floor, limit)

    def comma(self, cell: int) -> None:
        """Store one little-endian cell at ``HERE`` and advance atomically."""

        self._guard_mutation("store into the dictionary")
        self._store_and_advance(cell, CELL_BYTES)

    def c_comma(self, cell: int) -> None:
        """Store the low byte of one cell at ``HERE`` and advance atomically."""

        self._guard_mutation("store into the dictionary")
        self._store_and_advance(cell, 1)

    def write_transient(self, payload: bytes) -> int:
        """Write caller bytes at ``HERE`` without advancing the frontier."""

        self._guard_mutation("write transient dictionary bytes")
        if not isinstance(payload, bytes):
            raise TypeError("transient dictionary payload must be bytes")
        if self._memory is None:
            raise RuntimeError(
                "transient dictionary writes require a shared address space"
            )
        return self._native.write_transient(payload)

    def acquire_body_lease(self, word: Word) -> BodyAllocationLease:
        """Return the live identity of *word*'s original body allocation."""

        if not isinstance(word, Word):
            raise TypeError("body lease requires a Word")
        if self.live_word(word.xt) is not word:
            raise ValueError("body lease requires this dictionary's exact live word")
        allocation = self._native.allocation(word.xt)
        if allocation is None:
            raise ValueError("word has no live nonempty initial body allocation")
        body_address, body_limit, serial = allocation
        lease = self._leases.get(word.xt)
        if lease is None or lease.allocation_serial != serial or lease.word is not word:
            lease = BodyAllocationLease(
                word=word,
                body_address=body_address,
                body_limit=body_limit,
                allocation_serial=serial,
                _owner=self._owner,
            )
            self._leases[word.xt] = lease
        return lease

    def is_body_lease_live(self, lease: object) -> bool:
        """Check exact allocation identity without minting or renewing it."""

        if (
            not isinstance(lease, BodyAllocationLease)
            or lease._owner is not self._owner
            or not isinstance(lease.word, Word)
            or self._leases.get(lease.word.xt) is not lease
        ):
            return False
        allocation = self._native.allocation(lease.word.xt)
        return (
            allocation is not None
            and allocation[2] == lease.allocation_serial
            and self.live_word(lease.word.xt) is lease.word
        )

    def find(self, name: bytes | str) -> Word | None:
        """Return the newest case-insensitive binding for *name*, if present."""

        if type(name) is not bytes:
            name = _name_bytes(name)
        found = self._native.find(name)
        if type(found) is int:
            return self._make_view(found)
        return found

    def resolve(self, xt: int) -> Word:
        """Resolve a live execution token or raise ``KeyError``."""

        if type(xt) is int:
            word = self._by_xt.get(xt)
            if word is not None:
                return word
        found = self._native.resolve(_address(xt, label="execution token"))
        if type(found) is int:
            return self._make_view(found)
        return found

    def live_word(self, xt: int) -> Word | None:
        """Return the live word for an execution token, or ``None``."""

        if type(xt) is not int or not 0 <= xt <= MASK64:
            return None
        word = self._by_xt.get(xt)
        if word is not None:
            return word
        found = self._native.live(xt)
        if type(found) is int:
            return self._make_view(found)
        return found

    def checkpoint(self) -> DictionaryCheckpoint:
        """Capture the active contiguous dictionary state for later rollback."""

        native = self._native
        depth = len(native)
        seal = _DictionaryCheckpointSeal(
            here=native.here,
            latest=native.latest,
            active_floor=native.active_floor,
            active_limit=native.active_limit,
            depth=depth,
            tail=self._view(depth - 1) if depth else None,
            owner=self._owner,
        )
        return DictionaryCheckpoint(
            here=seal.here,
            latest=seal.latest,
            _seal=seal,
        )

    def rollback(self, checkpoint: DictionaryCheckpoint) -> int:
        """Remove definitions after a checkpoint and restore prior bindings.

        Returns how many definitions were removed.
        """

        self._guard_mutation("roll back the dictionary")
        if not isinstance(checkpoint, DictionaryCheckpoint):
            raise TypeError("rollback requires a DictionaryCheckpoint")
        seal = checkpoint._seal
        if seal.owner is not self._owner:
            raise ValueError("checkpoint belongs to another dictionary")
        if checkpoint.here != seal.here or checkpoint.latest != seal.latest:
            raise ValueError("checkpoint coordinates do not match its sealed state")
        if not seal.active_floor <= seal.here <= seal.active_limit:
            raise ValueError("checkpoint HERE is outside the dictionary region")
        if self._memory is not None and not any(
            region.base <= seal.active_floor
            and seal.active_limit <= region.limit
            for region in self._memory.regions
        ):
            raise ValueError("checkpoint dictionary zone is outside mapped memory")
        native = self._native
        count = len(native)
        if not 0 <= seal.depth <= count:
            raise ValueError("checkpoint is not in the active dictionary history")
        if seal.here > native.here:
            raise ValueError("checkpoint HERE is ahead of the active dictionary")
        expected_latest = 0 if seal.tail is None else seal.tail.header_address
        if seal.latest != expected_latest:
            raise ValueError("checkpoint LATEST does not match its sealed tail")
        if seal.depth:
            if self._view(seal.depth - 1) is not seal.tail:
                raise ValueError("checkpoint is not in the active dictionary history")
        elif seal.tail is not None:
            raise ValueError("invalid empty-dictionary checkpoint")

        self._forget(native.publish_rollback(
            seal.depth, seal.here, True, seal.active_floor, seal.active_limit
        ))
        return count - seal.depth

    def rollback_to(self, saved_here: int, saved_latest: int) -> None:
        """Restore one source-visible contiguous dictionary checkpoint."""

        self._guard_mutation("roll back the dictionary")
        target_here = _address(saved_here, label="saved HERE")
        target_latest = _address(saved_latest, label="saved LATEST")
        self._forget(self._native.rollback_to(target_here, target_latest))

    def set_latest(self, latest: int) -> None:
        """Publish a known semantic ancestor without changing ``HERE``."""

        self._guard_mutation("publish the dictionary head")
        self._forget(
            self._native.set_latest(_address(latest, label="LATEST! target"))
        )

    def _guard_mutation(self, operation: str) -> None:
        if self._mutation_guard is not None:
            self._mutation_guard(operation)

    def _forget(self, removed_xts: list[int]) -> None:
        by_xt = self._by_xt
        for xt in removed_xts:
            by_xt.pop(xt, None)

    def _store_and_advance(self, cell: int, width: int) -> None:
        if self._memory is None:
            raise RuntimeError("dictionary stores require a shared address space")
        is_integer = isinstance(cell, int)
        self._native.store_and_advance(
            cell & MASK64 if is_integer else 0, width, is_integer
        )

    def _view(self, index: int) -> Word:
        view = self._native.view(index)
        return self._make_view(view) if type(view) is int else view

    def _make_view(self, index: int) -> Word:
        (
            name, header_address, xt, immediate, kind, constant, directive,
            has_action, action_xt, action_ip, uid, implementation,
        ) = self._native.record_info(index)
        native = _extension()
        if kind == native.KIND_CREATED:
            implementation = NativeCreatedDefinition._bind(
                self._native,
                xt,
                uid,
                DoesBodyRef(action_xt, action_ip) if has_action else None,
            )
        elif implementation is None and kind == native.KIND_CONSTANT:
            implementation = ConstantDefinition(constant)
        elif implementation is None and kind == native.KIND_VALUE:
            implementation = ValueDefinition()
        word = Word(
            name=name,
            header_address=header_address,
            xt=xt,
            immediate=immediate,
            implementation=implementation,
        )
        self._native.set_view(index, word)
        self._by_xt[xt] = word
        return word


class NativeDictionaryIndex:
    """The ``HostedDictionaryIndex`` interface over the native side index."""

    __slots__ = ("_dictionary", "_native")

    def __init__(
        self,
        memory: SparseAddressSpace,
        dictionary: NativeDictionary,
    ) -> None:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("dictionary index memory must be a SparseAddressSpace")
        if not isinstance(dictionary, NativeDictionary):
            raise TypeError("dictionary index requires a NativeDictionary")
        self._dictionary = dictionary
        self._native = dictionary.native

    @property
    def state(self) -> DictionaryIndexState:
        """Return the stable source-visible index diagnostics."""

        return DictionaryIndexState(*self._native.index_state())

    @property
    def notification(self) -> tuple[int, int]:
        """Return the armed ``(count, xt)``; an xt of zero is disarmed."""

        return self._native.index_notification()

    def arm_notification(self, count: int, xt: int) -> None:
        """Implement ``DICT-INDEX-NOTIFY!``; an xt of zero disarms."""

        _require_cell(count, label="dictionary index notification count")
        _require_cell(xt, label="dictionary index notification xt")
        self._native.index_arm(count, xt)

    def take_notification(self) -> int:
        """Disarm and return the armed xt once the count has reached its mark."""

        return self._native.index_take()

    def configure(self, base: int, slots: int) -> int:
        """Implement ``DICT-INDEX!`` and return BIOS status 0, 1, or 2."""

        _require_cell(base, label="dictionary index base")
        _require_cell(slots, label="dictionary index slot count")
        return self._native.index_configure(base, slots)

    def rebuild(self) -> int:
        """Clear and rebuild a bound table newest-first."""

        return self._native.index_rebuild()

    def publish(self, word: Word) -> None:
        """Upsert one newly published latest binding when a table is bound."""

        if not isinstance(word, Word):
            raise TypeError("dictionary index publication requires a Word")
        self._native.index_publish(word.name, word.header_address)


def _require_cell(value: int, *, label: str) -> None:
    if isinstance(value, bool) or not isinstance(value, int):
        raise TypeError(f"{label} must be a uint64 integer")
    if not 0 <= value <= MASK64:
        raise ValueError(f"{label} must be a uint64 integer")


__all__ = [
    "NativeCreatedDefinition",
    "NativeDictionary",
    "NativeDictionaryIndex",
]
