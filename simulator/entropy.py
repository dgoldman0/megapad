"""Deterministic hosted model of the MegaPad TRNG MMIO service.

This service preserves the executable TRNG window and pool lifecycle, but it
does **not** model hardware entropy and its output is **not cryptographically
secure**.  Each 64-byte refill is derived reproducibly as two SHA-256 digests
of a domain separator, the exact injected seed bytes, a 128-bit refill
counter, and a block index.  No host operating-system randomness is consulted.

The mutable pool, supplemental guest seed, counters, and failure latch all
belong to one :class:`HostedTRNGService` instance so independently constructed
runtimes cannot consume or perturb one another's streams.
"""

from __future__ import annotations

import hashlib


TRNG_OFFSET = 0x800
TRNG_SIZE = 0x20
TRNG_LIMIT = TRNG_OFFSET + TRNG_SIZE

TRNG_RAND8 = TRNG_OFFSET + 0x00
TRNG_RAND64 = TRNG_OFFSET + 0x08
TRNG_RAND64_LIMIT = TRNG_OFFSET + 0x10
TRNG_STATUS = TRNG_OFFSET + 0x10
TRNG_SEED = TRNG_OFFSET + 0x18
TRNG_SEED_LIMIT = TRNG_OFFSET + 0x20

TRNG_POOL_BYTES = 64
TRNG_SEED_BYTES = 8

# An exact, immutable development seed.  It spells
# ``MegaPad-hosted-TRNG-seed-v1`` followed by five NUL bytes.  This constant is
# deliberately public so recordings and tests can identify the default
# deterministic stream without mistaking it for host or hardware entropy.
DEFAULT_TRNG_SEED = bytes.fromhex(
    "4d6567615061642d686f737465642d54"
    "524e472d736565642d76310000000000"
)

_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))
_REFILL_DOMAIN = b"MegaPad hosted deterministic TRNG/SHA-256/v1\x00"
_MAX_REFILL_COUNTER = 1 << 128


class TRNGAccessError(ValueError):
    """One direct access does not belong to the hosted TRNG aperture."""

    def __init__(
        self,
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        self.offset = offset
        self.width = width
        self.write = write
        super().__init__(message)


class TRNGUnavailableError(RuntimeError):
    """A guest attempted to read random data while the service was unusable."""

    def __init__(self, *, offset: int) -> None:
        self.offset = offset
        super().__init__("hosted TRNG data source is unusable")


class HostedTRNGService:
    """One byte-addressed deterministic TRNG-window service.

    ``seed`` is a host-side deterministic-stream input, distinct from guest
    writes to the supplemental ``SEED`` register.  ``usable=False`` constructs
    the complete decoded window in its disabled, zeroized state.
    """

    __slots__ = (
        "_counter",
        "_enabled",
        "_health_failed",
        "_pool",
        "_pool_initialized",
        "_pool_position",
        "_seed",
        "_supplemental_seed",
    )

    def __init__(
        self,
        seed: bytes = DEFAULT_TRNG_SEED,
        *,
        usable: bool = True,
    ) -> None:
        seed = self._require_seed(seed)
        if not isinstance(usable, bool):
            raise TypeError("TRNG usable option must be a boolean")

        self._seed = seed
        self._counter = 0
        self._pool = bytearray(TRNG_POOL_BYTES)
        self._supplemental_seed = bytearray(TRNG_SEED_BYTES)
        self._pool_position = TRNG_POOL_BYTES
        self._enabled = usable
        self._pool_initialized = False
        self._health_failed = False
        if usable:
            self._refill_pool()

    @property
    def seed(self) -> bytes:
        """Return the exact host-injected deterministic seed bytes."""

        return self._seed

    @property
    def enabled(self) -> bool:
        """Report whether the host has enabled the deterministic source."""

        return self._enabled

    @property
    def usable(self) -> bool:
        """Return the value exposed by the low bit of ``STATUS``."""

        return (
            self._enabled
            and self._pool_initialized
            and not self._health_failed
        )

    @property
    def health_failed(self) -> bool:
        """Report whether the host-only unusable latch has fired."""

        return self._health_failed

    @property
    def refill_counter(self) -> int:
        """Return the index that the next successful 64-byte refill will use."""

        return self._counter

    @property
    def pool_position(self) -> int:
        """Return the next unread byte index, or 64 while zeroized/unusable."""

        return self._pool_position

    @property
    def unread_pool_bytes(self) -> int:
        """Return the number of bytes in the current checked pool."""

        if not self.usable:
            return 0
        return TRNG_POOL_BYTES - self._pool_position

    @property
    def zeroized_state(self) -> tuple[bool, bool]:
        """Return ``(pool_zero, supplemental_seed_zero)`` for focused checks."""

        return (
            not any(self._pool),
            not any(self._supplemental_seed),
        )

    def inject_seed(self, seed: bytes, *, usable: bool = True) -> None:
        """Host-reset this instance to an exact deterministic seed.

        This is the explicit host reinitialization boundary and may recover a
        latched service.  It is intentionally separate from guest ``SEED``
        writes, which can never restore usability.
        """

        seed = self._require_seed(seed)
        if not isinstance(usable, bool):
            raise TypeError("TRNG usable option must be a boolean")

        self._wipe_pool()
        self._wipe_supplemental_seed()
        self._seed = seed
        self._counter = 0
        self._pool_position = TRNG_POOL_BYTES
        self._enabled = usable
        self._pool_initialized = False
        self._health_failed = False
        if usable:
            self._refill_pool()

    def disable(self) -> None:
        """Host-disable and zeroize this instance's guest-visible state."""

        self._enabled = False
        self._pool_initialized = False
        self._pool_position = TRNG_POOL_BYTES
        self._wipe_pool()
        self._wipe_supplemental_seed()

    def latch_unusable(self) -> None:
        """Host-only test seam: latch failure and zeroize all pooled state."""

        self._mark_unusable()

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Admit supported-width spans wholly inside the decoded window."""

        if isinstance(offset, bool) or not isinstance(offset, int):
            raise TypeError("TRNG offset must be an integer")
        if isinstance(width, bool) or not isinstance(width, int):
            raise TypeError("TRNG width must be an integer")
        if width not in _INTEGER_WIDTHS:
            self._reject(
                "TRNG width must be 1, 2, 4, or 8 bytes",
                offset=offset,
                width=width,
                write=write,
            )
        if offset < TRNG_OFFSET or offset + width > TRNG_LIMIT:
            self._reject(
                "access is outside the exact TRNG MMIO window",
                offset=offset,
                width=width,
                write=write,
            )

    def read8(self, offset: int) -> int:
        """Read one byte; reserved and write-only bytes return zero."""

        self._require_byte_offset(offset, write=False)
        if offset == TRNG_RAND8 or TRNG_RAND64 <= offset < TRNG_RAND64_LIMIT:
            return self._next_byte(offset)
        if offset == TRNG_STATUS:
            return int(self.usable)
        return 0

    def write8(self, offset: int, value: int) -> None:
        """Write one byte; reserved and read-only bytes ignore the value."""

        self._require_byte_offset(offset, write=True)
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError("TRNG byte value must be an integer")
        if not 0 <= value <= 0xFF:
            raise ValueError("TRNG byte value must be in range 0..255")

        if not TRNG_SEED <= offset < TRNG_SEED_LIMIT or not self.usable:
            return

        seed_index = offset - TRNG_SEED
        pool_index = self._pool_position + seed_index
        if pool_index < TRNG_POOL_BYTES:
            self._pool[pool_index] ^= value
        else:
            self._supplemental_seed[seed_index] ^= value

    def _next_byte(self, offset: int) -> int:
        if not self.usable:
            raise TRNGUnavailableError(offset=offset)
        if self._pool_position >= TRNG_POOL_BYTES and not self._refill_pool():
            raise TRNGUnavailableError(offset=offset)

        result = self._pool[self._pool_position]
        self._pool[self._pool_position] = 0
        self._pool_position += 1

        # Match the executable service's checked-ahead pool.  A refill failure
        # after the final old-pool byte may latch STATUS false, but does not
        # retract the byte that was already valid and returned.
        if self.usable and self._pool_position >= TRNG_POOL_BYTES:
            self._refill_pool()
        return result

    def _refill_pool(self) -> bool:
        if not self._enabled or self._health_failed:
            return False

        candidate = bytearray(TRNG_POOL_BYTES)
        try:
            if self._counter >= _MAX_REFILL_COUNTER:
                raise OverflowError("hosted TRNG refill counter exhausted")
            seed_length = len(self._seed).to_bytes(8, "big")
            counter = self._counter.to_bytes(16, "big")
            for block_index in range(2):
                start = block_index * hashlib.sha256().digest_size
                candidate[start : start + hashlib.sha256().digest_size] = (
                    hashlib.sha256(
                        _REFILL_DOMAIN
                        + seed_length
                        + self._seed
                        + counter
                        + bytes((block_index,))
                    ).digest()
                )
        except Exception:
            candidate[:] = b"\x00" * len(candidate)
            self._mark_unusable()
            return False

        # The native service carries each pending SEED lane into all eight
        # corresponding positions of the next host-derived 64-byte pool.
        for index in range(TRNG_POOL_BYTES):
            candidate[index] ^= self._supplemental_seed[
                index % TRNG_SEED_BYTES
            ]
        self._wipe_supplemental_seed()
        self._pool[:] = candidate
        candidate[:] = b"\x00" * len(candidate)
        self._pool_position = 0
        self._pool_initialized = True
        self._counter += 1
        return True

    def _mark_unusable(self) -> None:
        self._pool_initialized = False
        self._health_failed = True
        self._pool_position = TRNG_POOL_BYTES
        self._wipe_pool()
        self._wipe_supplemental_seed()

    def _wipe_pool(self) -> None:
        self._pool[:] = b"\x00" * TRNG_POOL_BYTES

    def _wipe_supplemental_seed(self) -> None:
        self._supplemental_seed[:] = b"\x00" * TRNG_SEED_BYTES

    def _require_byte_offset(self, offset: int, *, write: bool) -> None:
        if isinstance(offset, bool) or not isinstance(offset, int):
            raise TypeError("TRNG offset must be an integer")
        if not TRNG_OFFSET <= offset < TRNG_LIMIT:
            self._reject(
                "byte access is outside the exact TRNG MMIO window",
                offset=offset,
                width=1,
                write=write,
            )

    @staticmethod
    def _require_seed(seed: bytes) -> bytes:
        if type(seed) is not bytes:
            raise TypeError("deterministic TRNG seed must be exact bytes")
        if len(seed) >= 1 << 64:
            raise ValueError("deterministic TRNG seed is too large")
        return seed

    @staticmethod
    def _reject(
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        raise TRNGAccessError(
            message,
            offset=offset,
            width=width,
            write=write,
        )


__all__ = [
    "DEFAULT_TRNG_SEED",
    "HostedTRNGService",
    "TRNGAccessError",
    "TRNGUnavailableError",
    "TRNG_LIMIT",
    "TRNG_OFFSET",
    "TRNG_POOL_BYTES",
    "TRNG_RAND8",
    "TRNG_RAND64",
    "TRNG_RAND64_LIMIT",
    "TRNG_SEED",
    "TRNG_SEED_BYTES",
    "TRNG_SEED_LIMIT",
    "TRNG_SIZE",
    "TRNG_STATUS",
]
