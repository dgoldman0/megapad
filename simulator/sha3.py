"""Synchronous SHA-3/SHAKE/raw-Keccak service for the hosted simulator.

The MMIO half mirrors the executable native byte ABI and terminal state while
deliberately collapsing accelerator latency.  The checked half models the BIOS
owner record and publication ordering around that same device object.  Direct
virtual-MMIO interference is therefore visible to checked words; there is no
parallel hashlib-only shortcut.
"""

from __future__ import annotations

from collections.abc import Callable

from shared.cells import MASK64
from shared.crypto_caps import (
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
)
from shared.keccak import KECCAK_LANES, keccak_f1600
from simulator.memory import MMIO_BASE, SparseAddressSpace


SHA3_OFFSET = 0x780
SHA3_SIZE = 0x60
SHA3_LIMIT = SHA3_OFFSET + SHA3_SIZE

SHA3_COMMAND = SHA3_OFFSET + 0x00
SHA3_STATUS = SHA3_OFFSET + 0x01
SHA3_CONTROL = SHA3_OFFSET + 0x02
SHA3_ERROR = SHA3_OFFSET + 0x03
SHA3_DATA_INPUT = SHA3_OFFSET + 0x08
SHA3_DATA_OUTPUT = SHA3_OFFSET + 0x10
SHA3_STATE_INDEX = SHA3_OFFSET + 0x50
SHA3_STATE_DATA = SHA3_OFFSET + 0x58

SHA3_PHASE_IDLE = 0
SHA3_PHASE_BUSY = 1
SHA3_PHASE_DONE = 2
SHA3_PHASE_ERROR = 3

SHA3_OWNER_NONE = 0
SHA3_OWNER_SPONGE = 1
SHA3_OWNER_RAW = 2
SHA3_OWNER_WOTS = 3

SHA3_ERROR_NONE = 0
SHA3_ERROR_INVALID_COMMAND = 1
SHA3_ERROR_CONFLICT = 2
SHA3_ERROR_INVALID_MODE = 3
SHA3_ERROR_INVALID_STATE_INDEX = 4
SHA3_ERROR_INTERNAL = 5
SHA3_ERROR_UNAVAILABLE = 6

CRYPTO_STATUS_OK = 0
CRYPTO_STATUS_UNSUPPORTED = 1
CRYPTO_STATUS_STATE = 2
CRYPTO_STATUS_RANGE = 3
CRYPTO_STATUS_PROTECTED = 4
CRYPTO_STATUS_TIMEOUT = 5
CRYPTO_STATUS_HARDWARE = 6

CALLER_SPAN_OK = 0
CALLER_SPAN_RANGE = 2
CALLER_SPAN_PROTECTED = 3

GuestIdentity = tuple[int, int]
SpanStatus = Callable[[int, int], int]

_RATES = (136, 72, 168, 136)
_OUTPUT_SIZES = (32, 64, 0, 0)
_DOMAIN_SEPARATORS = (0x06, 0x06, 0x1F, 0x1F)
_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))
_BYTE_READS = frozenset((0x00, 0x01, 0x02, 0x03, 0x08, 0x50))
_BYTE_WRITES = frozenset((0x00, 0x02, 0x08, 0x50))

_WIDE_NONE = 0
_WIDE_DOUT_READ = 1
_WIDE_STATE_READ = 2
_WIDE_STATE_WRITE = 3


class SHA3AccessError(ValueError):
    """One direct access does not belong to the SHA aperture contract."""

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


class HostedSHA3Service:
    """One raw MMIO device plus its checked BIOS transaction record."""

    def __init__(self, capabilities: int) -> None:
        if isinstance(capabilities, bool) or not isinstance(capabilities, int):
            raise TypeError("SHA capabilities must be a uint64 integer")
        if not 0 <= capabilities <= MASK64:
            raise ValueError("SHA capabilities must be a uint64 integer")
        admitted = CRYPTO_CAP_SHA3_STREAM | CRYPTO_CAP_KECCAK_F1600
        if capabilities & ~admitted:
            raise ValueError("SHA service received unrelated capability bits")

        self._capabilities = capabilities
        self._stream_available = bool(capabilities & CRYPTO_CAP_SHA3_STREAM)
        self._raw_available = bool(capabilities & CRYPTO_CAP_KECCAK_F1600)
        self._mode = 0
        self._state = [0] * KECCAK_LANES
        self._buffer = bytearray(max(_RATES))
        self._buffer_length = 0
        self._digest = bytearray(64)
        self._squeeze_cursor = 0
        self._state_index = 0
        self._phase = SHA3_PHASE_IDLE
        self._owner = SHA3_OWNER_NONE
        self._error = SHA3_ERROR_NONE
        self._wide_operation = _WIDE_NONE
        self._wide_base = 0
        self._wide_position = 0
        self._wide_bytes = bytearray(8)
        self._wide_error = SHA3_ERROR_NONE
        self._fail_next_operation = False
        self._fail_next_clear = False

        self._checked_owner: GuestIdentity | None = None
        self._checked_kind = SHA3_OWNER_NONE
        self._checked_mode = 0
        self._checked_phase = 0
        self._checked_window_offset = 0

    @property
    def capabilities(self) -> int:
        return self._capabilities

    @property
    def packed_status(self) -> int:
        return (self._owner << 2) | self._phase

    @property
    def mode(self) -> int:
        return self._mode

    @property
    def error(self) -> int:
        return self._error

    @property
    def checked_owner(self) -> GuestIdentity | None:
        return self._checked_owner

    @property
    def checked_window_offset(self) -> int:
        return self._checked_window_offset

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Admit only the native byte/qword SHA access shapes."""

        if not isinstance(offset, int):
            raise TypeError("SHA offset must be an integer")
        if not isinstance(width, int):
            raise TypeError("SHA width must be an integer")
        if width not in _INTEGER_WIDTHS:
            self._reject(
                "SHA width must be 1, 2, 4, or 8 bytes",
                offset=offset,
                width=width,
                write=write,
            )
        if offset < SHA3_OFFSET or offset + width > SHA3_LIMIT:
            self._reject(
                "access is outside the exact SHA MMIO window",
                offset=offset,
                width=width,
                write=write,
            )
        if offset % width:
            self._reject(
                "SHA access is not naturally aligned",
                offset=offset,
                width=width,
                write=write,
            )

        local = offset - SHA3_OFFSET
        if width == 1:
            valid = self._byte_access_valid(local, write=write)
        elif width == 8:
            valid = (
                local == 0x58
                if write
                else (0x10 <= local <= 0x48 or local == 0x58)
            )
        else:
            valid = False
        if not valid:
            self._reject(
                "access shape is not defined by the SHA MMIO ABI",
                offset=offset,
                width=width,
                write=write,
            )
        if width == 8:
            self._begin_wide_access(local, write=write)

    def read8(self, offset: int) -> int:
        """Read one already-admitted byte callback."""

        local = self._require_byte_access(offset, write=False)
        wide_value = self._consume_wide_read(local)
        if wide_value is not None:
            return wide_value
        if self._wide_operation != _WIDE_NONE:
            self._cancel_wide_access()

        if local in (0x00, 0x08):
            return 0
        if local == 0x01:
            return self.packed_status
        if local == 0x02:
            return self._mode if self._stream_available else 0
        if local == 0x03:
            return self._error
        if 0x10 <= local < 0x50:
            return self._read_output_byte(local - 0x10)
        if local == 0x50:
            return self._read_state_index()
        if 0x58 <= local < 0x60:
            return self._read_state_byte(local - 0x58)
        raise AssertionError("admitted SHA read has no callback")

    def write8(self, offset: int, value: int) -> None:
        """Apply one already-admitted byte callback."""

        local = self._require_byte_access(offset, write=True)
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError("SHA byte value must be an integer")
        if not 0 <= value <= 0xFF:
            raise ValueError("SHA byte value must be in range 0..255")

        if self._consume_wide_write(local, value):
            return
        if self._wide_operation != _WIDE_NONE:
            self._cancel_wide_access()

        if local == 0x00:
            self._write_command(value)
        elif local == 0x02:
            self._write_control(value)
        elif local == 0x08:
            self._write_input(value)
        elif local == 0x50:
            self._write_state_index(value)
        elif 0x58 <= local < 0x60:
            self._write_state_byte(local - 0x58, value)
        else:
            raise AssertionError("admitted SHA write has no callback")

    def begin(
        self,
        identity: GuestIdentity,
        mode: int,
        memory: SparseAddressSpace,
    ) -> int:
        """Implement checked ``SHA3-BEGIN`` against this raw device."""

        if not self._stream_available:
            return CRYPTO_STATUS_UNSUPPORTED
        if not 0 <= mode < 4:
            return CRYPTO_STATUS_RANGE
        if self._checked_owner is not None:
            return CRYPTO_STATUS_STATE

        self._checked_owner = identity
        self._checked_kind = SHA3_OWNER_SPONGE
        self._checked_mode = 0
        self._checked_phase = 0
        self._checked_window_offset = 0
        if self._read_device_status(memory) != 0:
            return self._fail_cleanup(CRYPTO_STATUS_HARDWARE, memory)

        memory.write8(MMIO_BASE + SHA3_CONTROL, mode)
        if self._read_device_status(memory) != 0:
            return self._fail_cleanup(self._mapped_device_error(), memory)
        memory.write8(MMIO_BASE + SHA3_COMMAND, 1)
        if self._read_device_status(memory) != 0x04:
            return self._fail_cleanup(self._mapped_device_error(), memory)

        self._checked_mode = mode
        self._checked_phase = 1
        return CRYPTO_STATUS_OK

    def update(
        self,
        identity: GuestIdentity,
        source: int,
        length: int,
        *,
        memory: SparseAddressSpace,
        span_status: SpanStatus,
    ) -> int:
        """Implement checked ``SHA3-UPDATE`` with complete input preflight."""

        if not self._stream_available:
            return CRYPTO_STATUS_UNSUPPORTED
        if not self._is_checked_owner(identity):
            return CRYPTO_STATUS_STATE
        if self._checked_kind != SHA3_OWNER_SPONGE or self._checked_phase != 1:
            return self._fail_cleanup(CRYPTO_STATUS_STATE, memory)
        if length & (1 << 63):
            return self._fail_cleanup(CRYPTO_STATUS_RANGE, memory)
        if length == 0:
            return CRYPTO_STATUS_OK

        checked = self._map_caller_span(span_status(source, length))
        if checked != CRYPTO_STATUS_OK:
            return self._fail_cleanup(checked, memory)
        payload = memory.read_bytes(source, length)
        for byte in payload:
            memory.write8(MMIO_BASE + SHA3_DATA_INPUT, byte)
        if self._read_device_status(memory) != 0x04:
            return self._fail_cleanup(self._mapped_device_error(), memory)
        return CRYPTO_STATUS_OK

    def final(
        self,
        identity: GuestIdentity,
        destination: int,
        *,
        memory: SparseAddressSpace,
        span_status: SpanStatus,
    ) -> int:
        """Implement fixed SHA3 finalization with staged publication."""

        if not self._stream_available:
            return CRYPTO_STATUS_UNSUPPORTED
        if not self._is_checked_owner(identity):
            return CRYPTO_STATUS_STATE
        if (
            self._checked_kind != SHA3_OWNER_SPONGE
            or self._checked_phase != 1
            or self._checked_mode >= 2
        ):
            return self._fail_cleanup(CRYPTO_STATUS_STATE, memory)

        output_length = 32 if self._checked_mode == 0 else 64
        checked = self._map_caller_span(
            span_status(destination, output_length)
        )
        if checked != CRYPTO_STATUS_OK:
            return self._fail_cleanup(checked, memory)

        memory.write8(MMIO_BASE + SHA3_COMMAND, 3)
        if self._read_device_status(memory) != 0x06:
            return self._fail_cleanup(self._mapped_device_error(), memory)
        staged = bytes(
            memory.read8(MMIO_BASE + SHA3_DATA_OUTPUT + index)
            for index in range(output_length)
        )
        clear_status = self._clear_hardware(memory)
        if clear_status != CRYPTO_STATUS_OK:
            return clear_status
        memory.write_bytes(destination, staged)
        self._release_checked()
        return CRYPTO_STATUS_OK

    def shake_final(self, identity: GuestIdentity, memory: SparseAddressSpace) -> int:
        """Finalize one checked SHAKE transaction and retain ownership."""

        if not self._stream_available:
            return CRYPTO_STATUS_UNSUPPORTED
        if not self._is_checked_owner(identity):
            return CRYPTO_STATUS_STATE
        if (
            self._checked_kind != SHA3_OWNER_SPONGE
            or self._checked_phase != 1
            or self._checked_mode < 2
        ):
            return self._fail_cleanup(CRYPTO_STATUS_STATE, memory)

        memory.write8(MMIO_BASE + SHA3_COMMAND, 3)
        if self._read_device_status(memory) != 0x06:
            return self._fail_cleanup(self._mapped_device_error(), memory)
        self._checked_phase = 2
        self._checked_window_offset = 0
        return CRYPTO_STATUS_OK

    def shake_read(
        self,
        identity: GuestIdentity,
        destination: int,
        length: int,
        *,
        memory: SparseAddressSpace,
        span_status: SpanStatus,
    ) -> int:
        """Publish the next checked 0..32 SHAKE bytes atomically per call."""

        if not self._stream_available:
            return CRYPTO_STATUS_UNSUPPORTED
        if not self._is_checked_owner(identity):
            return CRYPTO_STATUS_STATE
        if self._checked_kind != SHA3_OWNER_SPONGE or self._checked_phase != 2:
            return self._fail_cleanup(CRYPTO_STATUS_STATE, memory)
        if length & (1 << 63) or length > 32:
            return self._fail_cleanup(CRYPTO_STATUS_RANGE, memory)
        if length == 0:
            return CRYPTO_STATUS_OK

        checked = self._map_caller_span(span_status(destination, length))
        if checked != CRYPTO_STATUS_OK:
            return self._fail_cleanup(checked, memory)

        tentative = self._checked_window_offset
        staged = bytearray()
        while len(staged) < length:
            if tentative == 64:
                memory.write8(MMIO_BASE + SHA3_COMMAND, 4)
                if self._read_device_status(memory) != 0x06:
                    return self._fail_cleanup(self._mapped_device_error(), memory)
                tentative = 0
            take = min(length - len(staged), 64 - tentative)
            staged.extend(
                memory.read8(MMIO_BASE + SHA3_DATA_OUTPUT + tentative + index)
                for index in range(take)
            )
            tentative += take

        memory.write_bytes(destination, staged)
        self._checked_window_offset = tentative
        return CRYPTO_STATUS_OK

    def clear(self, identity: GuestIdentity, memory: SparseAddressSpace) -> int:
        """Implement checked, idempotent ``SHA3-CLEAR``."""

        if not (self._stream_available or self._raw_available):
            return CRYPTO_STATUS_UNSUPPORTED
        if self._checked_owner is None:
            if self._read_device_status(memory) != 0:
                return CRYPTO_STATUS_STATE
            self._reset_checked_logical()
            return CRYPTO_STATUS_OK
        if not self._is_checked_owner(identity):
            return CRYPTO_STATUS_STATE

        status = self._clear_hardware(memory)
        if status == CRYPTO_STATUS_OK:
            self._release_checked()
        return status

    def keccak_f1600_checked(
        self,
        identity: GuestIdentity,
        address: int,
        *,
        memory: SparseAddressSpace,
        span_status: SpanStatus,
    ) -> int:
        """Implement checked in-place raw Keccak with staged publication."""

        if not self._raw_available:
            return CRYPTO_STATUS_UNSUPPORTED
        checked = self._map_caller_span(span_status(address, 200))
        if checked != CRYPTO_STATUS_OK:
            return checked
        if self._checked_owner is not None:
            return CRYPTO_STATUS_STATE

        self._checked_owner = identity
        self._checked_kind = SHA3_OWNER_RAW
        self._checked_mode = 0
        self._checked_phase = 1
        self._checked_window_offset = 0
        if self._read_device_status(memory) != 0:
            return self._fail_cleanup(CRYPTO_STATUS_HARDWARE, memory)

        source = memory.read_bytes(address, 200)
        for lane_index in range(KECCAK_LANES):
            memory.write8(MMIO_BASE + SHA3_STATE_INDEX, lane_index)
            lane = int.from_bytes(
                source[lane_index * 8 : lane_index * 8 + 8],
                "little",
            )
            memory.write64(MMIO_BASE + SHA3_STATE_DATA, lane)
        memory.write8(MMIO_BASE + SHA3_COMMAND, 6)
        if self._read_device_status(memory) != 0x0A:
            return self._fail_cleanup(self._mapped_device_error(), memory)

        staged = bytearray()
        for lane_index in range(KECCAK_LANES):
            memory.write8(MMIO_BASE + SHA3_STATE_INDEX, lane_index)
            staged.extend(
                memory.read64(MMIO_BASE + SHA3_STATE_DATA).to_bytes(8, "little")
            )
        status = self._clear_hardware(memory)
        if status != CRYPTO_STATUS_OK:
            return status
        memory.write_bytes(address, staged)
        self._release_checked()
        return CRYPTO_STATUS_OK

    def inject_operation_failure_once(self) -> None:
        """Focused-test seam: fail the next permutation before publication."""

        self._fail_next_operation = True

    def inject_clear_failure_once(self) -> None:
        """Focused-test seam: make the next CLEAR end in internal error."""

        self._fail_next_clear = True

    def private_zeroized(self) -> bool:
        """Whether raw transaction material is erased."""

        return (
            not any(self._state)
            and not any(self._buffer)
            and not any(self._digest)
            and self._buffer_length == 0
            and self._squeeze_cursor == 0
            and self._state_index == 0
            and self._owner == SHA3_OWNER_NONE
            and self._phase == SHA3_PHASE_IDLE
            and self._error == SHA3_ERROR_NONE
            and self._wide_operation == _WIDE_NONE
        )

    def _rate(self) -> int:
        return _RATES[self._mode]

    def _wipe_raw(self, *, preserve_mode: bool) -> None:
        selected_mode = self._mode
        self._state[:] = [0] * KECCAK_LANES
        self._buffer[:] = bytes(len(self._buffer))
        self._digest[:] = bytes(len(self._digest))
        self._buffer_length = 0
        self._squeeze_cursor = 0
        self._state_index = 0
        self._phase = SHA3_PHASE_IDLE
        self._owner = SHA3_OWNER_NONE
        self._error = SHA3_ERROR_NONE
        self._fail_next_operation = False
        self._cancel_wide_access()
        self._mode = selected_mode if preserve_mode else 0

    def _record_error(self, code: int) -> None:
        self._error = code
        self._phase = SHA3_PHASE_ERROR

    def _reject_conflict(self) -> None:
        self._record_error(SHA3_ERROR_CONFLICT)

    def _complete_or_fail(self, action: Callable[[], None]) -> None:
        if self._fail_next_operation:
            self._fail_next_operation = False
            selected_mode = self._mode
            self._wipe_raw(preserve_mode=True)
            self._mode = selected_mode
            self._record_error(SHA3_ERROR_INTERNAL)
            return
        action()

    def _write_command(self, value: int) -> None:
        if value == 7:
            if self._fail_next_clear:
                self._fail_next_clear = False
                selected_mode = self._mode
                self._wipe_raw(preserve_mode=True)
                self._mode = selected_mode
                self._record_error(SHA3_ERROR_INTERNAL)
            else:
                self._wipe_raw(preserve_mode=True)
            return
        if value == 1:
            self._command_init()
        elif value == 3:
            self._command_final()
        elif value == 4:
            self._command_next()
        elif value == 6:
            self._command_raw()
        else:
            self._record_error(SHA3_ERROR_INVALID_COMMAND)

    def _command_init(self) -> None:
        if self._owner != SHA3_OWNER_NONE or self._phase != SHA3_PHASE_IDLE:
            self._reject_conflict()
            return
        if not self._stream_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        selected_mode = self._mode
        self._wipe_raw(preserve_mode=True)
        self._mode = selected_mode
        self._owner = SHA3_OWNER_SPONGE

    def _command_final(self) -> None:
        if self._owner == SHA3_OWNER_RAW:
            self._reject_conflict()
            return
        if not self._stream_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if self._owner != SHA3_OWNER_SPONGE or self._phase != SHA3_PHASE_IDLE:
            self._reject_conflict()
            return
        self._complete_or_fail(self._complete_final)

    def _command_next(self) -> None:
        if self._owner == SHA3_OWNER_RAW:
            self._reject_conflict()
            return
        if not self._stream_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if self._mode < 2:
            self._record_error(SHA3_ERROR_INVALID_MODE)
            return
        if self._owner != SHA3_OWNER_SPONGE or self._phase != SHA3_PHASE_DONE:
            self._reject_conflict()
            return
        self._complete_or_fail(self._complete_next)

    def _command_raw(self) -> None:
        if self._owner == SHA3_OWNER_SPONGE:
            self._reject_conflict()
            return
        if not self._raw_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if not (
            (self._owner == SHA3_OWNER_NONE and self._phase == SHA3_PHASE_IDLE)
            or (
                self._owner == SHA3_OWNER_RAW
                and self._phase in (SHA3_PHASE_IDLE, SHA3_PHASE_DONE)
            )
        ):
            self._reject_conflict()
            return
        if self._owner == SHA3_OWNER_NONE:
            self._owner = SHA3_OWNER_RAW
            self._state[:] = [0] * KECCAK_LANES
        self._complete_or_fail(self._complete_raw)

    def _write_control(self, value: int) -> None:
        if self._owner != SHA3_OWNER_NONE or self._phase != SHA3_PHASE_IDLE:
            self._reject_conflict()
            return
        if not self._stream_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if value > 3:
            self._record_error(SHA3_ERROR_INVALID_MODE)
            return
        self._mode = value
        self._error = SHA3_ERROR_NONE

    def _write_input(self, value: int) -> None:
        if self._owner == SHA3_OWNER_RAW:
            self._reject_conflict()
            return
        if not self._stream_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if self._owner != SHA3_OWNER_SPONGE or self._phase != SHA3_PHASE_IDLE:
            self._reject_conflict()
            return
        self._buffer[self._buffer_length] = value
        self._buffer_length += 1
        if self._buffer_length == self._rate():
            self._complete_or_fail(self._absorb_buffer)

    def _absorb_buffer(self) -> None:
        rate = self._rate()
        for lane_index in range(rate // 8):
            lane = int.from_bytes(
                self._buffer[lane_index * 8 : lane_index * 8 + 8],
                "little",
            )
            self._state[lane_index] ^= lane
        self._state[:] = keccak_f1600(self._state)
        self._buffer[:] = bytes(len(self._buffer))
        self._buffer_length = 0
        self._phase = SHA3_PHASE_IDLE

    def _complete_final(self) -> None:
        rate = self._rate()
        padded = bytearray(rate)
        padded[: self._buffer_length] = self._buffer[: self._buffer_length]
        padded[self._buffer_length] ^= _DOMAIN_SEPARATORS[self._mode]
        padded[-1] ^= 0x80
        for lane_index in range(rate // 8):
            lane = int.from_bytes(
                padded[lane_index * 8 : lane_index * 8 + 8],
                "little",
            )
            self._state[lane_index] ^= lane
        self._state[:] = keccak_f1600(self._state)
        rate_bytes = self._extract_rate()
        output_size = _OUTPUT_SIZES[self._mode] or 64
        self._digest[:] = bytes(64)
        self._digest[:output_size] = rate_bytes[:output_size]
        self._squeeze_cursor = 64
        self._buffer[:] = bytes(len(self._buffer))
        self._buffer_length = 0
        self._phase = SHA3_PHASE_DONE

    def _complete_next(self) -> None:
        rate = self._rate()
        current = self._extract_rate()
        tail = min(64, rate - self._squeeze_cursor)
        next_window = bytearray(64)
        next_window[:tail] = current[
            self._squeeze_cursor : self._squeeze_cursor + tail
        ]
        self._squeeze_cursor += tail
        if tail != 64:
            self._state[:] = keccak_f1600(self._state)
            current = self._extract_rate()
            head = 64 - tail
            next_window[tail:] = current[:head]
            self._squeeze_cursor = head
        self._digest[:] = next_window
        self._phase = SHA3_PHASE_DONE

    def _complete_raw(self) -> None:
        self._state[:] = keccak_f1600(self._state)
        self._phase = SHA3_PHASE_DONE

    def _extract_rate(self) -> bytes:
        payload = bytearray()
        for lane in self._state[: self._rate() // 8]:
            payload.extend(lane.to_bytes(8, "little"))
        return bytes(payload)

    def _state_context_legal(self) -> bool:
        return (
            self._owner == SHA3_OWNER_NONE and self._phase == SHA3_PHASE_IDLE
        ) or (
            self._owner == SHA3_OWNER_RAW
            and self._phase in (SHA3_PHASE_IDLE, SHA3_PHASE_DONE)
        )

    def _write_state_index(self, value: int) -> None:
        if self._owner == SHA3_OWNER_SPONGE:
            self._reject_conflict()
            return
        if not self._raw_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if not self._state_context_legal():
            self._reject_conflict()
            return
        if value > 24:
            self._record_error(SHA3_ERROR_INVALID_STATE_INDEX)
            return
        self._state_index = value

    def _read_state_index(self) -> int:
        if self._owner == SHA3_OWNER_SPONGE:
            self._reject_conflict()
            return 0
        if not self._raw_available:
            return 0
        if not self._state_context_legal():
            self._reject_conflict()
            return 0
        return self._state_index

    def _commit_state_byte(self, byte_index: int, value: int) -> None:
        if self._owner == SHA3_OWNER_NONE:
            self._owner = SHA3_OWNER_RAW
            self._phase = SHA3_PHASE_IDLE
            self._error = SHA3_ERROR_NONE
        shift = byte_index * 8
        mask = 0xFF << shift
        self._state[self._state_index] = (
            self._state[self._state_index] & ~mask
        ) | (value << shift)

    def _write_state_byte(self, byte_index: int, value: int) -> None:
        if self._owner == SHA3_OWNER_SPONGE:
            self._reject_conflict()
            return
        if not self._raw_available:
            self._record_error(SHA3_ERROR_UNAVAILABLE)
            return
        if not (
            (self._owner == SHA3_OWNER_NONE and self._phase == SHA3_PHASE_IDLE)
            or (
                self._owner == SHA3_OWNER_RAW
                and self._phase == SHA3_PHASE_IDLE
            )
        ):
            self._reject_conflict()
            return
        self._commit_state_byte(byte_index, value)

    def _read_state_byte(self, byte_index: int) -> int:
        if self._owner == SHA3_OWNER_SPONGE:
            self._reject_conflict()
            return 0
        if not self._raw_available:
            return 0
        if self._owner != SHA3_OWNER_RAW or self._phase not in (
            SHA3_PHASE_IDLE,
            SHA3_PHASE_DONE,
        ):
            self._reject_conflict()
            return 0
        return (self._state[self._state_index] >> (byte_index * 8)) & 0xFF

    def _read_output_byte(self, byte_index: int) -> int:
        if self._owner == SHA3_OWNER_RAW:
            self._reject_conflict()
            return 0
        if not self._stream_available:
            return 0
        if self._owner != SHA3_OWNER_SPONGE or self._phase != SHA3_PHASE_DONE:
            self._reject_conflict()
            return 0
        return self._digest[byte_index]

    def _begin_wide_access(self, local: int, *, write: bool) -> None:
        self._cancel_wide_access()
        self._wide_base = local
        self._wide_position = 0
        if write:
            self._wide_operation = _WIDE_STATE_WRITE
            self._classify_wide_state_write()
        else:
            self._wide_operation = (
                _WIDE_STATE_READ if local == 0x58 else _WIDE_DOUT_READ
            )
            self._classify_wide_read(local)

    def _classify_wide_read(self, local: int) -> None:
        self._wide_error = SHA3_ERROR_NONE
        self._wide_bytes[:] = bytes(8)
        if local == 0x58:
            if self._owner == SHA3_OWNER_SPONGE:
                self._wide_error = SHA3_ERROR_CONFLICT
                return
            if not self._raw_available:
                return
            if self._owner != SHA3_OWNER_RAW or self._phase not in (
                SHA3_PHASE_IDLE,
                SHA3_PHASE_DONE,
            ):
                self._wide_error = SHA3_ERROR_CONFLICT
                return
            self._wide_bytes[:] = self._state[self._state_index].to_bytes(
                8,
                "little",
            )
            return
        if self._owner == SHA3_OWNER_RAW:
            self._wide_error = SHA3_ERROR_CONFLICT
            return
        if not self._stream_available:
            return
        if self._owner != SHA3_OWNER_SPONGE or self._phase != SHA3_PHASE_DONE:
            self._wide_error = SHA3_ERROR_CONFLICT
            return
        start = local - 0x10
        self._wide_bytes[:] = self._digest[start : start + 8]

    def _classify_wide_state_write(self) -> None:
        self._wide_error = SHA3_ERROR_NONE
        self._wide_bytes[:] = bytes(8)
        if self._owner == SHA3_OWNER_SPONGE:
            self._wide_error = SHA3_ERROR_CONFLICT
        elif not self._raw_available:
            self._wide_error = SHA3_ERROR_UNAVAILABLE
        elif not (
            (self._owner == SHA3_OWNER_NONE and self._phase == SHA3_PHASE_IDLE)
            or (
                self._owner == SHA3_OWNER_RAW
                and self._phase == SHA3_PHASE_IDLE
            )
        ):
            self._wide_error = SHA3_ERROR_CONFLICT

    def _consume_wide_read(self, local: int) -> int | None:
        if self._wide_operation not in (_WIDE_DOUT_READ, _WIDE_STATE_READ):
            return None
        if local != self._wide_base + self._wide_position:
            return None
        value = self._wide_bytes[self._wide_position]
        self._wide_position += 1
        if self._wide_position == 8:
            terminal_error = self._wide_error
            self._cancel_wide_access()
            if terminal_error != SHA3_ERROR_NONE:
                self._record_error(terminal_error)
        return value

    def _consume_wide_write(self, local: int, value: int) -> bool:
        if (
            self._wide_operation != _WIDE_STATE_WRITE
            or local != 0x58 + self._wide_position
        ):
            return False
        self._wide_bytes[self._wide_position] = value
        self._wide_position += 1
        if self._wide_position == 8:
            terminal_error = self._wide_error
            lane = int.from_bytes(self._wide_bytes, "little")
            self._cancel_wide_access()
            if terminal_error != SHA3_ERROR_NONE:
                self._record_error(terminal_error)
            else:
                if self._owner == SHA3_OWNER_NONE:
                    self._owner = SHA3_OWNER_RAW
                    self._phase = SHA3_PHASE_IDLE
                    self._error = SHA3_ERROR_NONE
                self._state[self._state_index] = lane
        return True

    def _cancel_wide_access(self) -> None:
        self._wide_operation = _WIDE_NONE
        self._wide_base = 0
        self._wide_position = 0
        self._wide_error = SHA3_ERROR_NONE
        self._wide_bytes[:] = bytes(8)

    def _clear_hardware(self, memory: SparseAddressSpace) -> int:
        memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
        if self.packed_status == 0:
            return CRYPTO_STATUS_OK
        return self._mapped_device_error()

    def _fail_cleanup(
        self,
        first_status: int,
        memory: SparseAddressSpace,
    ) -> int:
        memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
        if self._read_device_status(memory) == 0:
            self._release_checked()
            return first_status
        return self._mapped_device_error()

    def _mapped_device_error(self) -> int:
        return (
            CRYPTO_STATUS_STATE
            if self._error == SHA3_ERROR_CONFLICT
            else CRYPTO_STATUS_HARDWARE
        )

    @staticmethod
    def _read_device_status(memory: SparseAddressSpace) -> int:
        return memory.read8(MMIO_BASE + SHA3_STATUS)

    def _is_checked_owner(self, identity: GuestIdentity) -> bool:
        return self._checked_owner == identity

    def _release_checked(self) -> None:
        self._checked_owner = None
        self._reset_checked_logical()

    def _reset_checked_logical(self) -> None:
        self._checked_kind = SHA3_OWNER_NONE
        self._checked_mode = 0
        self._checked_phase = 0
        self._checked_window_offset = 0

    @staticmethod
    def _map_caller_span(status: int) -> int:
        if status == CALLER_SPAN_OK:
            return CRYPTO_STATUS_OK
        if status == CALLER_SPAN_RANGE:
            return CRYPTO_STATUS_RANGE
        if status == CALLER_SPAN_PROTECTED:
            return CRYPTO_STATUS_PROTECTED
        raise ValueError("caller-span service returned an unknown status")

    @staticmethod
    def _byte_access_valid(local: int, *, write: bool) -> bool:
        if write:
            return local in _BYTE_WRITES or 0x58 <= local < 0x60
        return (
            local in _BYTE_READS
            or 0x10 <= local < 0x50
            or 0x58 <= local < 0x60
        )

    def _require_byte_access(self, offset: int, *, write: bool) -> int:
        if not isinstance(offset, int):
            raise TypeError("SHA offset must be an integer")
        local = offset - SHA3_OFFSET
        if not 0 <= local < SHA3_SIZE or not self._byte_access_valid(
            local,
            write=write,
        ):
            self._reject(
                "byte access is not defined by the SHA MMIO ABI",
                offset=offset,
                width=1,
                write=write,
            )
        return local

    @staticmethod
    def _reject(
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        raise SHA3AccessError(
            message,
            offset=offset,
            width=width,
            write=write,
        )


__all__ = [
    "CALLER_SPAN_OK",
    "CALLER_SPAN_PROTECTED",
    "CALLER_SPAN_RANGE",
    "CRYPTO_STATUS_HARDWARE",
    "CRYPTO_STATUS_OK",
    "CRYPTO_STATUS_PROTECTED",
    "CRYPTO_STATUS_RANGE",
    "CRYPTO_STATUS_STATE",
    "CRYPTO_STATUS_TIMEOUT",
    "CRYPTO_STATUS_UNSUPPORTED",
    "HostedSHA3Service",
    "SHA3AccessError",
    "SHA3_COMMAND",
    "SHA3_CONTROL",
    "SHA3_DATA_INPUT",
    "SHA3_DATA_OUTPUT",
    "SHA3_ERROR",
    "SHA3_LIMIT",
    "SHA3_OFFSET",
    "SHA3_SIZE",
    "SHA3_STATE_DATA",
    "SHA3_STATE_INDEX",
    "SHA3_STATUS",
]
