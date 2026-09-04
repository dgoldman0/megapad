"""Hosted AES-128/256-GCM service for the MegaPad MMIO byte ABI.

The service mirrors the executable BIOS/native transaction protocol while
remaining synchronous and timing-free.  It is both the direct virtual-MMIO
target and the state reached by the hosted BIOS words; there is no parallel
word-only crypto implementation.
"""

from __future__ import annotations

from shared.aes import (
    AESBlockCipher,
    AES_BLOCK_BYTES,
    ghash_update,
    increment_gcm_counter,
)


AES_OFFSET = 0x700
AES_SIZE = 0x70
AES_LIMIT = AES_OFFSET + AES_SIZE

AES_STATUS_IDLE = 0
AES_STATUS_ACTIVE = 1
AES_STATUS_DONE = 2
AES_STATUS_FAILED = 3

AES_KEY = AES_OFFSET + 0x00
AES_IV = AES_OFFSET + 0x20
AES_AAD_LENGTH = AES_OFFSET + 0x30
AES_DATA_LENGTH = AES_OFFSET + 0x34
AES_COMMAND = AES_OFFSET + 0x38
AES_STATUS = AES_OFFSET + 0x39
AES_KEY_MODE = AES_OFFSET + 0x3A
AES_DATA_INPUT = AES_OFFSET + 0x40
AES_DATA_OUTPUT = AES_OFFSET + 0x50
AES_TAG = AES_OFFSET + 0x60

_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))


class AESAccessError(ValueError):
    """One direct access does not belong to the hosted AES aperture."""

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


class HostedAESService:
    """One byte-addressed, synchronous AES-GCM transaction engine."""

    __slots__ = (
        "_aad_length",
        "_aad_length_written_mask",
        "_aad_processed",
        "_cipher",
        "_command",
        "_counter",
        "_data_input",
        "_data_length",
        "_data_length_written_mask",
        "_data_output",
        "_data_processed",
        "_din_written",
        "_ghash_state",
        "_hash_subkey",
        "_iv",
        "_iv_written_mask",
        "_j0",
        "_key",
        "_key_mode",
        "_key_written_mask",
        "_status",
        "_tag",
        "_tag_written_mask",
    )

    def __init__(self) -> None:
        self._key = bytearray(32)
        self._iv = bytearray(12)
        self._data_input = bytearray(AES_BLOCK_BYTES)
        self._data_output = bytes(AES_BLOCK_BYTES)
        self._tag = bytearray(AES_BLOCK_BYTES)
        self._status = AES_STATUS_IDLE
        self._key_mode = 0
        self._command = 0
        self._aad_length = 0
        self._data_length = 0
        self._cipher: AESBlockCipher | None = None
        self._hash_subkey = 0
        self._counter = bytes(AES_BLOCK_BYTES)
        self._j0 = bytes(AES_BLOCK_BYTES)
        self._ghash_state = 0
        self._aad_processed = 0
        self._data_processed = 0
        self._din_written = 0
        self._clear_configuration_tracking()

    @property
    def status(self) -> int:
        return self._status

    @property
    def key_mode(self) -> int:
        return self._key_mode

    @property
    def aad_length(self) -> int:
        return self._aad_length

    @property
    def data_length(self) -> int:
        return self._data_length

    @property
    def aad_processed(self) -> int:
        return self._aad_processed

    @property
    def data_processed(self) -> int:
        return self._data_processed

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Admit the native AES aperture's aligned scalar access shapes."""

        if not isinstance(offset, int):
            raise TypeError("AES offset must be an integer")
        if not isinstance(width, int):
            raise TypeError("AES width must be an integer")
        if width not in _INTEGER_WIDTHS:
            self._reject(
                "AES width must be 1, 2, 4, or 8 bytes",
                offset=offset,
                width=width,
                write=write,
            )
        if offset < AES_OFFSET or offset + width > AES_LIMIT:
            self._reject(
                "access is outside the exact AES MMIO window",
                offset=offset,
                width=width,
                write=write,
            )
        if offset % width:
            self._reject(
                "AES access is not naturally aligned",
                offset=offset,
                width=width,
                write=write,
            )

    def read8(self, offset: int) -> int:
        """Read one register byte; write-only and reserved bytes read zero."""

        self._require_byte_offset(offset, write=False)
        local = offset - AES_OFFSET
        if local == 0x39:
            return self._status
        if local == 0x3A:
            return self._key_mode
        if 0x50 <= local < 0x60:
            return self._data_output[local - 0x50]
        if 0x60 <= local < 0x70:
            return self._tag[local - 0x60]
        return 0

    def write8(self, offset: int, value: int) -> None:
        """Apply one native byte callback after whole-access preflight."""

        self._require_byte_offset(offset, write=True)
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError("AES byte value must be an integer")
        if not 0 <= value <= 0xFF:
            raise ValueError("AES byte value must be in range 0..255")

        local = offset - AES_OFFSET
        if local < 0x20:
            self._begin_configuration_write()
            self._key[local] = value
            self._key_written_mask |= 1 << local
        elif 0x20 <= local < 0x2C:
            self._begin_configuration_write()
            index = local - 0x20
            self._iv[index] = value
            self._iv_written_mask |= 1 << index
        elif 0x30 <= local < 0x34:
            self._begin_configuration_write()
            index = local - 0x30
            self._aad_length = self._replace_u32_byte(
                self._aad_length,
                index,
                value,
            )
            self._aad_length_written_mask |= 1 << index
        elif 0x34 <= local < 0x38:
            self._begin_configuration_write()
            index = local - 0x34
            self._data_length = self._replace_u32_byte(
                self._data_length,
                index,
                value,
            )
            self._data_length_written_mask |= 1 << index
        elif local == 0x38:
            self._begin_configuration_write()
            self._command = value & 1
            self._start_gcm()
        elif local == 0x3A:
            self._begin_configuration_write()
            self._key_mode = value & 1
        elif 0x40 <= local < 0x50:
            index = local - 0x40
            if self._status != AES_STATUS_ACTIVE or index != self._din_written:
                self._latch_transaction_fault()
                return
            self._data_input[index] = value
            self._din_written += 1
            if self._din_written == AES_BLOCK_BYTES:
                self._din_written = 0
                self._process_block()
        elif 0x60 <= local < 0x70:
            self._begin_configuration_write()
            index = local - 0x60
            self._tag[index] = value
            self._tag_written_mask |= 1 << index

    def _start_gcm(self) -> None:
        decrypting = self._command != 0
        if not self._configuration_complete(decrypting):
            self._latch_transaction_fault()
            return

        self._clear_derived_state()
        self._data_input[:] = bytes(AES_BLOCK_BYTES)
        self._data_output = bytes(AES_BLOCK_BYTES)
        if not decrypting:
            self._tag[:] = bytes(AES_BLOCK_BYTES)
        self._status = AES_STATUS_ACTIVE

        key = bytes(self._key[:16] if self._key_mode else self._key)
        self._cipher = AESBlockCipher(key)
        hash_bytes = self._cipher.encrypt(bytes(AES_BLOCK_BYTES))
        self._hash_subkey = int.from_bytes(hash_bytes, "big")
        self._j0 = bytes(self._iv) + b"\x00\x00\x00\x01"
        self._counter = self._j0
        self._ghash_state = 0

        if self._aad_length == 0 and self._data_length == 0:
            self._finalize_tag()

    def _process_block(self) -> None:
        cipher = self._cipher
        if self._status != AES_STATUS_ACTIVE or cipher is None:
            self._latch_transaction_fault()
            return

        if self._aad_processed < self._aad_length:
            take = min(self._aad_length - self._aad_processed, AES_BLOCK_BYTES)
            block = bytes(self._data_input[:take]) + bytes(AES_BLOCK_BYTES - take)
            self._ghash_state = ghash_update(
                self._ghash_state,
                self._hash_subkey,
                block,
            )
            self._aad_processed += take
            self._data_output = bytes(AES_BLOCK_BYTES)
            self._data_input[:] = bytes(AES_BLOCK_BYTES)
            if (
                self._aad_processed == self._aad_length
                and self._data_length == 0
            ):
                self._finalize_tag()
            return

        if self._data_processed >= self._data_length:
            self._latch_transaction_fault()
            return

        take = min(self._data_length - self._data_processed, AES_BLOCK_BYTES)
        self._counter = increment_gcm_counter(self._counter)
        keystream = cipher.encrypt(self._counter)
        input_block = bytes(self._data_input[:take]) + bytes(
            AES_BLOCK_BYTES - take
        )
        output = bytes(
            input_block[index] ^ keystream[index]
            if index < take
            else 0
            for index in range(AES_BLOCK_BYTES)
        )
        self._data_output = output
        authenticated = output if self._command == 0 else input_block
        self._ghash_state = ghash_update(
            self._ghash_state,
            self._hash_subkey,
            authenticated,
        )
        self._data_processed += take
        self._data_input[:] = bytes(AES_BLOCK_BYTES)
        if self._data_processed == self._data_length:
            self._finalize_tag()

    def _finalize_tag(self) -> None:
        cipher = self._cipher
        if self._status != AES_STATUS_ACTIVE or cipher is None:
            self._latch_transaction_fault()
            return

        length_block = (
            (self._aad_length * 8).to_bytes(8, "big")
            + (self._data_length * 8).to_bytes(8, "big")
        )
        self._ghash_state = ghash_update(
            self._ghash_state,
            self._hash_subkey,
            length_block,
        )
        authentication = self._ghash_state.to_bytes(16, "big")
        encrypted_j0 = cipher.encrypt(self._j0)
        computed_tag = bytes(
            left ^ right for left, right in zip(authentication, encrypted_j0)
        )

        decrypting = self._command != 0
        if not decrypting:
            self._tag[:] = computed_tag
            self._status = AES_STATUS_DONE
        else:
            difference = 0
            for computed, expected in zip(computed_tag, self._tag):
                difference |= computed ^ expected
            self._status = (
                AES_STATUS_DONE if difference == 0 else AES_STATUS_FAILED
            )
            if difference:
                self._data_output = bytes(AES_BLOCK_BYTES)
        self._clear_completed_secrets(clear_tag=decrypting)

    def _begin_configuration_write(self) -> None:
        if self._status == AES_STATUS_ACTIVE:
            self._latch_transaction_fault()
        elif self._status in (AES_STATUS_DONE, AES_STATUS_FAILED):
            self._data_output = bytes(AES_BLOCK_BYTES)
            self._status = AES_STATUS_IDLE

    def _latch_transaction_fault(self) -> None:
        self._data_output = bytes(AES_BLOCK_BYTES)
        self._clear_completed_secrets(clear_tag=True)
        self._status = AES_STATUS_FAILED

    def _clear_derived_state(self) -> None:
        self._cipher = None
        self._hash_subkey = 0
        self._counter = bytes(AES_BLOCK_BYTES)
        self._j0 = bytes(AES_BLOCK_BYTES)
        self._ghash_state = 0
        self._aad_processed = 0
        self._data_processed = 0
        self._din_written = 0

    def _clear_completed_secrets(self, *, clear_tag: bool) -> None:
        self._key[:] = bytes(32)
        self._iv[:] = bytes(12)
        self._data_input[:] = bytes(AES_BLOCK_BYTES)
        self._clear_derived_state()
        if clear_tag:
            self._tag[:] = bytes(AES_BLOCK_BYTES)
        self._aad_length = 0
        self._data_length = 0
        self._command = 0
        self._key_mode = 0
        self._clear_configuration_tracking()

    def _clear_configuration_tracking(self) -> None:
        self._key_written_mask = 0
        self._iv_written_mask = 0
        self._aad_length_written_mask = 0
        self._data_length_written_mask = 0
        self._tag_written_mask = 0

    def _configuration_complete(self, decrypting: bool) -> bool:
        return (
            self._key_written_mask == 0xFFFF_FFFF
            and self._iv_written_mask == 0x0FFF
            and self._aad_length_written_mask == 0x0F
            and self._data_length_written_mask == 0x0F
            and (not decrypting or self._tag_written_mask == 0xFFFF)
        )

    def _require_byte_offset(self, offset: int, *, write: bool) -> None:
        if not isinstance(offset, int):
            raise TypeError("AES offset must be an integer")
        if not AES_OFFSET <= offset < AES_LIMIT:
            self._reject(
                "byte access is outside the exact AES MMIO window",
                offset=offset,
                width=1,
                write=write,
            )

    @staticmethod
    def _replace_u32_byte(current: int, index: int, value: int) -> int:
        shift = index * 8
        return (current & ~(0xFF << shift)) | (value << shift)

    @staticmethod
    def _reject(
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        raise AESAccessError(
            message,
            offset=offset,
            width=width,
            write=write,
        )


__all__ = [
    "AESAccessError",
    "AES_AAD_LENGTH",
    "AES_COMMAND",
    "AES_DATA_INPUT",
    "AES_DATA_LENGTH",
    "AES_DATA_OUTPUT",
    "AES_IV",
    "AES_KEY",
    "AES_KEY_MODE",
    "AES_LIMIT",
    "AES_OFFSET",
    "AES_SIZE",
    "AES_STATUS",
    "AES_STATUS_ACTIVE",
    "AES_STATUS_DONE",
    "AES_STATUS_FAILED",
    "AES_STATUS_IDLE",
    "AES_TAG",
    "HostedAESService",
]
