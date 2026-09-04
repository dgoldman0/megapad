"""Checked hosted SHA-256/SHA-512 streaming BIOS service.

MegaPad exposes SHA-2 through scoped EXT.CRYPTO instructions rather than an
MMIO aperture.  The hosted backend therefore models the public, per-core BIOS
transactions directly: complete physical-span qualification, context state,
length accounting, staged publication, and terminal cleanup.  It does not
model accumulator registers, interrupt masking, instruction latency, or the
shared micro-cluster engine.

Hash computation delegates to :mod:`hashlib`.  Dropping a hash object and
overwriting the hosted metadata establishes logical simulator cleanup, but
does not claim physical erasure of opaque CPython/OpenSSL allocations.
"""

from __future__ import annotations

from dataclasses import dataclass
import hashlib
from typing import Protocol

from shared.cells import MASK64
from simulator.memory import SparseAddressSpace


SHA2_STATUS_OK = 0
SHA2_STATUS_STATE = 1
SHA2_STATUS_RANGE = 2
SHA2_STATUS_CONTEXT_ALIAS = 3
SHA2_STATUS_LENGTH_OVERFLOW = 4

SHA256_CONTEXT_BYTES = 256
SHA512_CONTEXT_BYTES = 512

SHA256_ALGORITHM = "sha256"
SHA512_ALGORITHM = "sha512"


class _HashObject(Protocol):
    def update(self, payload: bytes) -> None: ...

    def digest(self) -> bytes: ...

    def copy(self) -> _HashObject: ...


@dataclass(frozen=True, slots=True)
class _Variant:
    name: str
    block_bytes: int
    digest_bytes: int
    length_bits: int


@dataclass(slots=True)
class _Context:
    active_marker: int
    bit_length_low: int
    bit_length_high: int
    partial_offset: int
    hasher: _HashObject | None
    stage: bytearray


_SHA256 = _Variant(SHA256_ALGORITHM, 64, 32, 64)
_SHA512 = _Variant(SHA512_ALGORITHM, 128, 64, 128)
_VARIANTS = {
    SHA256_ALGORITHM: _SHA256,
    SHA512_ALGORITHM: _SHA512,
}


class HostedSHA2Service:
    """Runtime-local checked SHA-2 contexts for the advertised full cores.

    Hosted contexts live outside guest memory, so the ordinary simulator
    profile has no caller-addressable context arena.  ``context_alias_ranges``
    is an optional composition/conformance seam for a profile that deliberately
    maps private arenas into its guest address space.  Geometry is always
    checked before aliasing, matching the BIOS status priority.
    """

    def __init__(
        self,
        *,
        core_count: int,
        context_alias_ranges: tuple[tuple[int, int], ...] = (),
    ) -> None:
        if (
            isinstance(core_count, bool)
            or not isinstance(core_count, int)
            or core_count <= 0
        ):
            raise ValueError("SHA-2 core count must be a positive integer")
        self._core_count = core_count
        self._context_alias_ranges = self._validate_alias_ranges(
            context_alias_ranges
        )
        self._contexts = {
            variant.name: [
                self._new_context(variant) for _ in range(core_count)
            ]
            for variant in _VARIANTS.values()
        }

    @property
    def core_count(self) -> int:
        return self._core_count

    @property
    def context_alias_ranges(self) -> tuple[tuple[int, int], ...]:
        """Return configured ``(base, exclusive_limit)`` private arenas."""

        return self._context_alias_ranges

    def span_status(
        self,
        memory: SparseAddressSpace,
        address: int,
        length: int,
    ) -> int:
        """Implement pure ``SHA2-SPAN-STATUS`` physical qualification."""

        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("SHA-2 memory must be a SparseAddressSpace")
        length = self._cell(length, label="SHA-2 span length")
        if length == 0:
            return SHA2_STATUS_OK
        address = self._cell(address, label="SHA-2 span address")

        end = address + length
        if end > MASK64 + 1:
            return SHA2_STATUS_RANGE
        if not any(
            region.base <= address and end <= region.limit
            for region in memory.regions
        ):
            return SHA2_STATUS_RANGE
        if any(
            address < alias_limit and alias_base < end
            for alias_base, alias_limit in self._context_alias_ranges
        ):
            return SHA2_STATUS_CONTEXT_ALIAS
        return SHA2_STATUS_OK

    def sha256_init(self, core_id: int) -> int:
        return self._init(_SHA256, core_id)

    def sha256_update(
        self,
        core_id: int,
        source: int,
        length: int,
        memory: SparseAddressSpace,
    ) -> int:
        return self._update(_SHA256, core_id, source, length, memory)

    def sha256_final(
        self,
        core_id: int,
        destination: int,
        memory: SparseAddressSpace,
    ) -> int:
        return self._final(_SHA256, core_id, destination, memory)

    def sha256_clear(self, core_id: int) -> int:
        return self._clear(_SHA256, core_id)

    def sha512_init(self, core_id: int) -> int:
        return self._init(_SHA512, core_id)

    def sha512_update(
        self,
        core_id: int,
        source: int,
        length: int,
        memory: SparseAddressSpace,
    ) -> int:
        return self._update(_SHA512, core_id, source, length, memory)

    def sha512_final(
        self,
        core_id: int,
        destination: int,
        memory: SparseAddressSpace,
    ) -> int:
        return self._final(_SHA512, core_id, destination, memory)

    def sha512_clear(self, core_id: int) -> int:
        return self._clear(_SHA512, core_id)

    def private_zeroized(
        self,
        algorithm: str,
        *,
        core_id: int = 0,
    ) -> bool:
        """Report whether one hosted logical context contains no live state."""

        variant = self._variant(algorithm)
        context = self._context(variant, core_id)
        return (
            context.active_marker == 0
            and context.bit_length_low == 0
            and context.bit_length_high == 0
            and context.partial_offset == 0
            and context.hasher is None
            and not any(context.stage)
        )

    def inject_context_metadata_for_test(
        self,
        algorithm: str,
        *,
        core_id: int = 0,
        active_marker: int | None = None,
        bit_length_low: int | None = None,
        bit_length_high: int | None = None,
        partial_offset: int | None = None,
    ) -> None:
        """Focused fault seam for checked-state and length-path evidence."""

        variant = self._variant(algorithm)
        context = self._context(variant, core_id)
        if active_marker is not None:
            if isinstance(active_marker, bool) or not isinstance(
                active_marker,
                int,
            ):
                raise TypeError("SHA-2 active marker must be an integer")
            context.active_marker = active_marker & MASK64
        if bit_length_low is not None:
            context.bit_length_low = self._cell(
                bit_length_low,
                label="SHA-2 low bit length",
            )
        if bit_length_high is not None:
            context.bit_length_high = self._cell(
                bit_length_high,
                label="SHA-2 high bit length",
            )
        if partial_offset is not None:
            context.partial_offset = self._cell(
                partial_offset,
                label="SHA-2 partial offset",
            )

    def _init(self, variant: _Variant, core_id: int) -> int:
        context = self._context(variant, core_id)
        self._wipe(context)
        context.hasher = hashlib.new(variant.name)
        context.active_marker = 1
        return SHA2_STATUS_OK

    def _update(
        self,
        variant: _Variant,
        core_id: int,
        source: int,
        length: int,
        memory: SparseAddressSpace,
    ) -> int:
        context = self._context(variant, core_id)
        state_status = self._state_status(variant, context)
        if state_status != SHA2_STATUS_OK:
            self._wipe(context)
            return state_status

        length = self._cell(length, label="SHA-2 update length")
        if length == 0:
            return SHA2_STATUS_OK
        span_status = self.span_status(memory, source, length)
        if span_status != SHA2_STATUS_OK:
            self._wipe(context)
            return span_status

        lengths = self._extended_length(variant, context, length)
        if lengths is None:
            self._wipe(context)
            return SHA2_STATUS_LENGTH_OVERFLOW
        new_low, new_high = lengths

        hasher = context.hasher
        assert hasher is not None
        candidate = hasher.copy()
        source = self._cell(source, label="SHA-2 update source")
        remaining = length
        cursor = source
        try:
            while remaining:
                chunk_length = min(remaining, memory.page_size)
                candidate.update(memory.read_bytes(cursor, chunk_length))
                cursor += chunk_length
                remaining -= chunk_length
        except Exception:
            self._wipe(context)
            raise

        context.hasher = candidate
        context.bit_length_low = new_low
        context.bit_length_high = new_high
        context.partial_offset = (new_low >> 3) & (
            variant.block_bytes - 1
        )
        return SHA2_STATUS_OK

    def _final(
        self,
        variant: _Variant,
        core_id: int,
        destination: int,
        memory: SparseAddressSpace,
    ) -> int:
        context = self._context(variant, core_id)
        state_status = self._state_status(variant, context)
        if state_status != SHA2_STATUS_OK:
            self._wipe(context)
            return state_status

        span_status = self.span_status(
            memory,
            destination,
            variant.digest_bytes,
        )
        if span_status != SHA2_STATUS_OK:
            self._wipe(context)
            return span_status

        hasher = context.hasher
        assert hasher is not None
        context.stage[:] = hasher.digest()
        destination = self._cell(
            destination,
            label="SHA-2 final destination",
        )
        try:
            memory.write_bytes(destination, context.stage)
        except Exception:
            self._wipe(context)
            raise
        self._wipe(context)
        return SHA2_STATUS_OK

    def _clear(self, variant: _Variant, core_id: int) -> int:
        self._wipe(self._context(variant, core_id))
        return SHA2_STATUS_OK

    @staticmethod
    def _extended_length(
        variant: _Variant,
        context: _Context,
        length: int,
    ) -> tuple[int, int] | None:
        delta_low = (length << 3) & MASK64
        delta_high = length >> 61
        low_total = context.bit_length_low + delta_low
        new_low = low_total & MASK64
        carry = int(low_total > MASK64)

        if variant.length_bits == 64:
            if delta_high or carry:
                return None
            return new_low, 0

        high_total = context.bit_length_high + delta_high + carry
        if high_total > MASK64:
            return None
        return new_low, high_total

    @staticmethod
    def _state_status(variant: _Variant, context: _Context) -> int:
        if context.active_marker != 1 or context.hasher is None:
            return SHA2_STATUS_STATE
        if variant.length_bits == 64 and context.bit_length_high != 0:
            return SHA2_STATUS_LENGTH_OVERFLOW
        if context.partial_offset >= variant.block_bytes:
            return SHA2_STATUS_STATE
        if context.bit_length_low & 7:
            return SHA2_STATUS_STATE
        expected_offset = (context.bit_length_low >> 3) & (
            variant.block_bytes - 1
        )
        if context.partial_offset != expected_offset:
            return SHA2_STATUS_STATE
        return SHA2_STATUS_OK

    def _context(self, variant: _Variant, core_id: int) -> _Context:
        if isinstance(core_id, bool) or not isinstance(core_id, int):
            raise TypeError("SHA-2 core ID must be an integer")
        if not 0 <= core_id < self._core_count:
            raise ValueError("SHA-2 core ID is outside the hosted profile")
        return self._contexts[variant.name][core_id]

    @staticmethod
    def _new_context(variant: _Variant) -> _Context:
        return _Context(0, 0, 0, 0, None, bytearray(variant.digest_bytes))

    @staticmethod
    def _wipe(context: _Context) -> None:
        context.active_marker = 0
        context.bit_length_low = 0
        context.bit_length_high = 0
        context.partial_offset = 0
        context.hasher = None
        context.stage[:] = bytes(len(context.stage))

    @staticmethod
    def _variant(algorithm: str) -> _Variant:
        if not isinstance(algorithm, str):
            raise TypeError("SHA-2 algorithm name must be a string")
        try:
            return _VARIANTS[algorithm]
        except KeyError as exc:
            raise ValueError("SHA-2 algorithm must be sha256 or sha512") from exc

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer cell")
        return value & MASK64

    @staticmethod
    def _validate_alias_ranges(
        ranges: tuple[tuple[int, int], ...],
    ) -> tuple[tuple[int, int], ...]:
        if not isinstance(ranges, tuple):
            raise TypeError("SHA-2 context alias ranges must be a tuple")
        checked: list[tuple[int, int]] = []
        for item in ranges:
            if not isinstance(item, tuple) or len(item) != 2:
                raise TypeError("each SHA-2 alias range must be a pair")
            base, limit = item
            if (
                isinstance(base, bool)
                or not isinstance(base, int)
                or isinstance(limit, bool)
                or not isinstance(limit, int)
            ):
                raise TypeError("SHA-2 alias bounds must be integers")
            if not 0 <= base < limit <= MASK64 + 1:
                raise ValueError("SHA-2 alias range must be nonempty uint64")
            checked.append((base, limit))
        checked.sort()
        if any(
            left_limit > right_base
            for (_left_base, left_limit), (right_base, _right_limit) in zip(
                checked,
                checked[1:],
            )
        ):
            raise ValueError("SHA-2 context alias ranges must not overlap")
        return tuple(checked)


__all__ = [
    "HostedSHA2Service",
    "SHA2_STATUS_CONTEXT_ALIAS",
    "SHA2_STATUS_LENGTH_OVERFLOW",
    "SHA2_STATUS_OK",
    "SHA2_STATUS_RANGE",
    "SHA2_STATUS_STATE",
    "SHA256_ALGORITHM",
    "SHA256_CONTEXT_BYTES",
    "SHA512_ALGORITHM",
    "SHA512_CONTEXT_BYTES",
]
