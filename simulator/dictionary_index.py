"""Caller-backed dictionary side index for the hosted BIOS profile.

The linked semantic dictionary remains authoritative.  This service preserves
the public ``DICT-INDEX!``/``DICT-INDEX@`` state and exact guest-visible slot
layout used by the executable BIOS while host dictionary lookup retains its
equivalent case-insensitive binding map.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared.cells import MASK64
from simulator.dictionary import Dictionary, Word
from simulator.memory import AddressClass, SparseAddressSpace


DICT_INDEX_BOUND = 1 << 0
DICT_INDEX_AUTHORITATIVE = 1 << 1
DICT_INDEX_BUILDING = 1 << 2
DICT_INDEX_SATURATED = 1 << 3

DICT_INDEX_SLOT_BYTES = 16
FNV1A32_OFFSET_BASIS = 0x811C_9DC5
FNV1A32_PRIME = 0x0100_0193


@dataclass(frozen=True, slots=True)
class DictionaryIndexState:
    """Stable values returned by ``DICT-INDEX@``."""

    base: int = 0
    slots: int = 0
    count: int = 0
    flags: int = 0


class HostedDictionaryIndex:
    """Maintain one bounded, caller-owned open-addressed side index."""

    __slots__ = ("_dictionary", "_memory", "_state")

    def __init__(
        self,
        memory: SparseAddressSpace,
        dictionary: Dictionary,
    ) -> None:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("dictionary index memory must be a SparseAddressSpace")
        if not isinstance(dictionary, Dictionary):
            raise TypeError("dictionary index requires a Dictionary")
        self._memory = memory
        self._dictionary = dictionary
        self._state = DictionaryIndexState()

    @property
    def state(self) -> DictionaryIndexState:
        """Return the stable source-visible index diagnostics."""

        return self._state

    def configure(self, base: int, slots: int) -> int:
        """Implement ``DICT-INDEX!`` and return BIOS status 0, 1, or 2."""

        self._require_cell(base, label="dictionary index base")
        self._require_cell(slots, label="dictionary index slot count")

        if slots == 0:
            if base != 0:
                return 1
            # BIOS disable leaves the old caller bytes untouched.
            self._state = DictionaryIndexState()
            return 0

        if base == 0 or base & (DICT_INDEX_SLOT_BYTES - 1):
            return 1
        if slots & (slots - 1):
            return 1
        if slots > MASK64 // DICT_INDEX_SLOT_BYTES:
            return 1

        span = slots * DICT_INDEX_SLOT_BYTES
        if base > MASK64 - span:
            return 1
        limit = base + span
        external = next(
            (
                region
                for region in self._memory.regions
                if region.kind is AddressClass.EXTERNAL
            ),
            None,
        )
        if (
            external is None
            or base < external.base
            or limit > external.limit
        ):
            return 1

        # Installation publishes the new geometry before rebuilding.  The
        # hosted one-core operation is synchronous, so BUILDING is not visible
        # across a dispatch boundary, but retain the same transition locally.
        self._state = DictionaryIndexState(
            base=base,
            slots=slots,
            count=0,
            flags=DICT_INDEX_BOUND | DICT_INDEX_BUILDING,
        )
        return self.rebuild()

    def rebuild(self) -> int:
        """Clear and rebuild a bound table newest-first."""

        state = self._state
        if not state.flags & DICT_INDEX_BOUND:
            return 0

        self._state = DictionaryIndexState(
            base=state.base,
            slots=state.slots,
            count=0,
            flags=DICT_INDEX_BOUND | DICT_INDEX_BUILDING,
        )
        self._memory.fill(
            state.base,
            state.slots * DICT_INDEX_SLOT_BYTES,
            0,
        )

        for word in reversed(self._dictionary.words):
            if not self._insert(word, upsert=False):
                current = self._state
                self._state = DictionaryIndexState(
                    base=current.base,
                    slots=current.slots,
                    count=current.count,
                    flags=DICT_INDEX_BOUND | DICT_INDEX_SATURATED,
                )
                return 2

        current = self._state
        self._state = DictionaryIndexState(
            base=current.base,
            slots=current.slots,
            count=current.count,
            flags=DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
        )
        return 0

    def publish(self, word: Word) -> None:
        """Upsert one newly published latest binding when a table is bound."""

        if not isinstance(word, Word):
            raise TypeError("dictionary index publication requires a Word")
        state = self._state
        if not state.flags & DICT_INDEX_BOUND:
            return

        was_saturated = bool(state.flags & DICT_INDEX_SATURATED)
        self._state = DictionaryIndexState(
            base=state.base,
            slots=state.slots,
            count=state.count,
            flags=(
                DICT_INDEX_BOUND
                | DICT_INDEX_BUILDING
                | (DICT_INDEX_SATURATED if was_saturated else 0)
            ),
        )
        inserted = self._insert(word, upsert=True)
        current = self._state
        flags = (
            DICT_INDEX_BOUND | DICT_INDEX_SATURATED
            if was_saturated or not inserted
            else DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE
        )
        self._state = DictionaryIndexState(
            base=current.base,
            slots=current.slots,
            count=current.count,
            flags=flags,
        )

    def _insert(self, word: Word, *, upsert: bool) -> bool:
        slot, exact = self._probe(word.name)
        if slot is None:
            return False
        if exact:
            if upsert:
                self._memory.write64(slot, word.header_address)
            return True

        name_hash = _uppercase_fnv1a32(word.name)
        metadata = name_hash | (len(word.name) << 32)
        # Match BIOS publication order: metadata first, entry pointer last.
        self._memory.write64(slot + 8, metadata)
        self._memory.write64(slot, word.header_address)
        state = self._state
        self._state = DictionaryIndexState(
            base=state.base,
            slots=state.slots,
            count=state.count + 1,
            flags=state.flags,
        )
        return True

    def _probe(self, name: bytes) -> tuple[int | None, bool]:
        state = self._state
        name_hash = _uppercase_fnv1a32(name)
        slot_index = name_hash & (state.slots - 1)
        for _ in range(state.slots):
            slot = state.base + slot_index * DICT_INDEX_SLOT_BYTES
            entry = self._memory.read64(slot)
            if entry == 0:
                return slot, False

            metadata = self._memory.read64(slot + 8)
            stored_hash = metadata & 0xFFFF_FFFF
            stored_length = (metadata >> 32) & 0x7F
            if (
                stored_hash == name_hash
                and stored_length == len(name)
                and self._entry_name_equal(entry, name)
            ):
                return slot, True
            slot_index = (slot_index + 1) & (state.slots - 1)
        return None, False

    def _entry_name_equal(self, entry: int, name: bytes) -> bool:
        flags_length = self._memory.read8(entry + 8)
        if flags_length & 0x7F != len(name):
            return False
        candidate = self._memory.read_bytes(entry + 9, len(name))
        return _uppercase_ascii(candidate) == _uppercase_ascii(name)

    @staticmethod
    def _require_cell(value: int, *, label: str) -> None:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be a uint64 integer")
        if not 0 <= value <= MASK64:
            raise ValueError(f"{label} must be a uint64 integer")


def _uppercase_ascii(value: bytes) -> bytes:
    return bytes(byte - 0x20 if 0x61 <= byte <= 0x7A else byte for byte in value)


def _uppercase_fnv1a32(name: bytes) -> int:
    result = FNV1A32_OFFSET_BASIS
    for byte in name:
        folded = byte - 0x20 if 0x61 <= byte <= 0x7A else byte
        result ^= folded
        result = (result * FNV1A32_PRIME) & 0xFFFF_FFFF
    return result


__all__ = [
    "DICT_INDEX_AUTHORITATIVE",
    "DICT_INDEX_BOUND",
    "DICT_INDEX_BUILDING",
    "DICT_INDEX_SATURATED",
    "DICT_INDEX_SLOT_BYTES",
    "DictionaryIndexState",
    "HostedDictionaryIndex",
]
