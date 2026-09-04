"""Checked CRC transaction service for the hosted one-core profile."""

from __future__ import annotations

from shared.cells import MASK64, u64
from shared.crc import (
    CRYPTO_CAP_CRC_REFLECT_RAW,
    CRC_MODE_IDS,
    CRC_REFLECTED_MODE_IDS,
    crc_feed_byte,
    crc_feed_cell,
    crc_final_value,
    crc_raw_value,
    crc_reset_value,
    crc_seed_value,
)


CRC_STATUS_OK = 0
CRC_STATUS_UNSUPPORTED = 1
CRC_STATUS_STATE = 2
CRC_STATUS_RANGE = 3

GuestIdentity = tuple[int, int]


class HostedCRCService:
    """Model the BIOS checked owner record above pure CRC value semantics.

    The initial simulator advertises one full guest core and one foreground
    task.  Host scratch :class:`ExecutionContext` objects are views of that
    same `(COREID,TASK-ID)=(0,0)` identity, not extra schedulable tasks.
    """

    def __init__(self, capabilities: int) -> None:
        if isinstance(capabilities, bool) or not isinstance(capabilities, int):
            raise TypeError("CRC capabilities must be a uint64 integer")
        if not 0 <= capabilities <= MASK64:
            raise ValueError("CRC capabilities must be a uint64 integer")
        if capabilities & ~CRYPTO_CAP_CRC_REFLECT_RAW:
            raise ValueError(
                "hosted CRC service cannot admit unimplemented capabilities"
            )
        self._capabilities = capabilities
        self._mode = 0
        self._accumulator = crc_reset_value(self._mode)
        self._owner: GuestIdentity | None = None

    @property
    def capabilities(self) -> int:
        return self._capabilities

    @property
    def mode(self) -> int:
        return self._mode

    @property
    def accumulator(self) -> int:
        return self._accumulator

    @property
    def owner(self) -> GuestIdentity | None:
        return self._owner

    def select_mode(self, identity: GuestIdentity, mode: int) -> int:
        """Validate and acquire without changing the accumulator."""

        mode = u64(mode)
        if mode not in CRC_MODE_IDS:
            return CRC_STATUS_RANGE
        if (
            mode in CRC_REFLECTED_MODE_IDS
            and not self._capabilities & CRYPTO_CAP_CRC_REFLECT_RAW
        ):
            return CRC_STATUS_UNSUPPORTED
        if self._owner is not None:
            return CRC_STATUS_STATE
        self._mode = mode
        self._owner = identity
        return CRC_STATUS_OK

    def reset(self, identity: GuestIdentity) -> int:
        if not self._is_owner(identity):
            return CRC_STATUS_STATE
        self._accumulator = crc_reset_value(self._mode)
        return CRC_STATUS_OK

    def seed(self, identity: GuestIdentity, seed: int) -> int:
        if not self._is_owner(identity):
            return CRC_STATUS_STATE
        self._accumulator = crc_seed_value(self._mode, seed)
        return CRC_STATUS_OK

    def feed_cell(self, identity: GuestIdentity, cell: int) -> int:
        if not self._is_owner(identity):
            return CRC_STATUS_STATE
        self._accumulator = crc_feed_cell(
            self._mode,
            self._accumulator,
            cell,
        )
        return CRC_STATUS_OK

    def feed_byte(self, identity: GuestIdentity, byte: int) -> int:
        if not self._is_owner(identity):
            return CRC_STATUS_STATE
        self._accumulator = crc_feed_byte(
            self._mode,
            self._accumulator,
            byte,
        )
        return CRC_STATUS_OK

    def fetch(self, identity: GuestIdentity) -> tuple[int, int]:
        if not self._is_owner(identity):
            return 0, CRC_STATUS_STATE
        # CRC-MODE! deliberately does not mutate CRC_ACC.  A mode change can
        # therefore expose high bits left by an earlier 64-bit transaction;
        # the native BIOS reads the complete CSR here without applying the
        # newly selected mode's width mask.
        return u64(self._accumulator), CRC_STATUS_OK

    def raw_final(self, identity: GuestIdentity) -> tuple[int, int]:
        if not self._capabilities & CRYPTO_CAP_CRC_REFLECT_RAW:
            # Capability status has priority.  BIOS still finalizes an exact
            # owner in a non-reflected mode solely to release its record.
            if self._is_owner(identity) and self._mode in (0, 1, 2):
                self._accumulator = crc_final_value(
                    self._mode,
                    self._accumulator,
                )
                self._owner = None
            return 0, CRC_STATUS_UNSUPPORTED
        if not self._is_owner(identity):
            return 0, CRC_STATUS_STATE
        self._accumulator = crc_raw_value(self._mode, self._accumulator)
        result = self._accumulator
        self._owner = None
        return result, CRC_STATUS_OK

    def final(self, identity: GuestIdentity) -> int:
        if not self._is_owner(identity):
            return 0
        self._accumulator = crc_final_value(self._mode, self._accumulator)
        result = self._accumulator
        self._owner = None
        return result

    def _is_owner(self, identity: GuestIdentity) -> bool:
        return self._owner == identity


__all__ = [
    "CRC_STATUS_OK",
    "CRC_STATUS_RANGE",
    "CRC_STATUS_STATE",
    "CRC_STATUS_UNSUPPORTED",
    "GuestIdentity",
    "HostedCRCService",
]
