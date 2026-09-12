"""Backend-neutral one-shot PCM register and host-sink lifecycle.

Backends supply checked guest-memory callbacks and map these relative registers
through their own bus adapters. This model never imports a backend, owns guest
memory, advances time, or assumes a CPU or scheduler.
"""
from __future__ import annotations

from typing import Callable

AUDIO_OFFSET = 0xC00
AUDIO_SIZE = 0x20
AUDIO_LIMIT = AUDIO_OFFSET + AUDIO_SIZE

# ---------------------------------------------------------------------------
#  One-shot PCM Audio Output
# ---------------------------------------------------------------------------
# The audio device is deliberately buffer-oriented rather than a real-time
# sample FIFO.  A guest renders a complete signed-PCM buffer, describes it in
# the registers below, and submits it once.  The emulator copies the bytes
# immediately so later guest mutation cannot change an in-flight submission.
# A headless run always retains that capture for deterministic tests; an
# optional host sink may additionally play it.
#
# Register map (offsets from AUDIO_BASE):
#   0x00  CMD          (W)  1=SUBMIT, 2=STOP, 3=CLEAR
#   0x01  STATUS       (R)  bit0=busy, bit1=done, bit2=error,
#                            bit3=host playing, bit7=present
#   0x02  FORMAT       (RW) 1=signed 16-bit little-endian PCM
#   0x03  CHANNELS     (RW) 1=mono, 2=stereo
#   0x04..0x07 RATE    (RW) sample rate in Hz, little-endian u32
#   0x08..0x0F DMA     (RW) source byte address, little-endian u64
#   0x10..0x13 FRAMES  (RW) frame count, little-endian u32
#   0x14..0x17 GEN     (R)  successful submission generation, u32
#   0x18  ERROR        (R)  latched AUDIO_ERR_* code
#   0x19  CAPS         (R)  bit0=headless capture, bit1=managed host sink

AUDIO_FORMAT_S16LE = 1

AUDIO_CMD_SUBMIT = 1
AUDIO_CMD_STOP = 2
AUDIO_CMD_CLEAR = 3

AUDIO_ERR_NONE = 0
AUDIO_ERR_BUSY = 1
AUDIO_ERR_FORMAT = 2
AUDIO_ERR_CHANNELS = 3
AUDIO_ERR_RATE = 4
AUDIO_ERR_FRAMES = 5
AUDIO_ERR_CAPACITY = 6
AUDIO_ERR_MEMORY = 7
AUDIO_ERR_SINK = 8

AUDIO_MAX_CAPTURE_BYTES = 1024 * 1024


class AudioOutputModel:
    """One-shot signed-PCM output with deterministic headless capture.

    Capture succeeds independently of host playback so CI never depends on an
    audio server.  A host sink is attached only when all three lifecycle
    callbacks are present: ``on_submit(pcm, rate, channels)``, ``on_stop()``,
    and the cheap, non-blocking ``on_playing()`` query.  Submit may return
    ``False`` to reject playback; ``None`` and truthy values mean accepted.
    """

    def __init__(self, *, max_capture_bytes: int = AUDIO_MAX_CAPTURE_BYTES):
        if max_capture_bytes <= 0:
            raise ValueError("max_capture_bytes must be positive")
        self.max_capture_bytes = int(max_capture_bytes)
        self.format = AUDIO_FORMAT_S16LE
        self.channels = 1
        self.rate = 8000
        self.dma_addr = 0
        self.frames = 0
        self.generation = 0
        self.error = AUDIO_ERR_NONE
        self.busy = False
        self.done = False
        self.playing = False
        self.last_pcm = b""
        self.last_rate = 0
        self.last_channels = 0
        self.last_frames = 0
        self._mem_read: Callable[[int], int] | None = None
        self._mem_span_valid: Callable[[int, int], bool] | None = None
        self.on_submit: Callable[[bytes, int, int], object] | None = None
        self.on_stop: Callable[[], object] | None = None
        self.on_playing: Callable[[], bool] | None = None

    @property
    def byte_count(self) -> int:
        return self.frames * self.channels * 2

    @property
    def capabilities(self) -> int:
        return 0x01 | (0x02 if self._sink_attached() else 0)

    def _sink_attached(self) -> bool:
        return all(callable(callback) for callback in (
            self.on_submit, self.on_stop, self.on_playing))

    def _refresh_playing(self) -> None:
        """Observe natural completion without guessing on query failure."""
        if not self.playing:
            return
        if not self._sink_attached():
            self.error = AUDIO_ERR_SINK
            return
        try:
            still_playing = bool(self.on_playing())
        except Exception:
            # A failed query is not evidence that the owned voice stopped.
            self.error = AUDIO_ERR_SINK
            return
        if not still_playing:
            self.playing = False

    def _status(self) -> int:
        self._refresh_playing()
        status = 0x80
        if self.busy:
            status |= 0x01
        if self.done:
            status |= 0x02
        if self.error != AUDIO_ERR_NONE:
            status |= 0x04
        if self.playing:
            status |= 0x08
        return status

    @staticmethod
    def _replace_byte(value: int, offset: int, byte: int, width: int) -> int:
        shift = offset * 8
        mask = 0xFF << shift
        return ((value & ~mask) | ((byte & 0xFF) << shift)) & ((1 << width) - 1)

    def read8(self, offset: int) -> int:
        if offset == 0x01:
            return self._status()
        if offset == 0x02:
            return self.format
        if offset == 0x03:
            return self.channels
        if 0x04 <= offset <= 0x07:
            return (self.rate >> (8 * (offset - 0x04))) & 0xFF
        if 0x08 <= offset <= 0x0F:
            return (self.dma_addr >> (8 * (offset - 0x08))) & 0xFF
        if 0x10 <= offset <= 0x13:
            return (self.frames >> (8 * (offset - 0x10))) & 0xFF
        if 0x14 <= offset <= 0x17:
            return (self.generation >> (8 * (offset - 0x14))) & 0xFF
        if offset == 0x18:
            return self.error
        if offset == 0x19:
            return self.capabilities
        return 0

    def write8(self, offset: int, value: int):
        value &= 0xFF
        if offset == 0x00:
            self._execute(value)
        elif offset == 0x02:
            self.format = value
        elif offset == 0x03:
            self.channels = value
        elif 0x04 <= offset <= 0x07:
            self.rate = self._replace_byte(
                self.rate, offset - 0x04, value, 32)
        elif 0x08 <= offset <= 0x0F:
            self.dma_addr = self._replace_byte(
                self.dma_addr, offset - 0x08, value, 64)
        elif 0x10 <= offset <= 0x13:
            self.frames = self._replace_byte(
                self.frames, offset - 0x10, value, 32)

    def _validate(self) -> int:
        if self.busy:
            return AUDIO_ERR_BUSY
        if self.playing:
            return AUDIO_ERR_BUSY
        if self.format != AUDIO_FORMAT_S16LE:
            return AUDIO_ERR_FORMAT
        if self.channels not in (1, 2):
            return AUDIO_ERR_CHANNELS
        if not 8000 <= self.rate <= 192000:
            return AUDIO_ERR_RATE
        if self.frames <= 0:
            return AUDIO_ERR_FRAMES
        if self.byte_count > self.max_capture_bytes:
            return AUDIO_ERR_CAPACITY
        if self._mem_read is None or self._mem_span_valid is None:
            return AUDIO_ERR_MEMORY
        try:
            if not self._mem_span_valid(self.dma_addr, self.byte_count):
                return AUDIO_ERR_MEMORY
        except Exception:
            return AUDIO_ERR_MEMORY
        return AUDIO_ERR_NONE

    def _submit(self):
        self._refresh_playing()
        error = self._validate()
        if error != AUDIO_ERR_NONE:
            # A rejected descriptor must not destroy an earlier coherent
            # capture or turn an active host voice into reported silence.
            if self.error == AUDIO_ERR_NONE:
                self.error = error
            return

        self.busy = True
        self.error = AUDIO_ERR_NONE
        try:
            pcm = bytes(
                self._mem_read(self.dma_addr + i) & 0xFF
                for i in range(self.byte_count)
            )
        except Exception:
            self.busy = False
            self.error = AUDIO_ERR_MEMORY
            return

        # Publish a coherent capture before invoking any fallible host sink.
        self.last_pcm = pcm
        self.last_rate = self.rate
        self.last_channels = self.channels
        self.last_frames = self.frames
        self.generation = (self.generation + 1) & 0xFFFFFFFF
        self.busy = False
        self.done = True

        if self._sink_attached():
            try:
                accepted = self.on_submit(pcm, self.rate, self.channels)
            except Exception:
                # The callback may have started a voice before it failed.
                # Retain conservative ownership so STOP can still release it.
                self.playing = True
                self.error = AUDIO_ERR_SINK
                return
            if accepted is False:
                self.playing = False
                self.error = AUDIO_ERR_SINK
                return
            self.playing = True

    def _stop(self) -> bool:
        was_busy = self.busy
        if self.playing:
            if not self._sink_attached():
                self.error = AUDIO_ERR_SINK
                return False
            try:
                self.on_stop()
            except Exception:
                # Retain the complete pre-command ownership state.  A failed
                # callback is not proof that either DMA or the voice stopped.
                self.busy = was_busy
                self.error = AUDIO_ERR_SINK
                return False
        self.busy = False
        self.playing = False
        return True

    def _clear(self):
        if not self._stop():
            return
        self.error = AUDIO_ERR_NONE
        self.done = False
        self.last_pcm = b""
        self.last_rate = 0
        self.last_channels = 0
        self.last_frames = 0

    def reset(self):
        """Return the device to power-on state while preserving host wiring."""
        stop_failed = False
        if self.playing:
            if not callable(self.on_stop):
                stop_failed = True
            else:
                try:
                    self.on_stop()
                except Exception:
                    stop_failed = True
        self.format = AUDIO_FORMAT_S16LE
        self.channels = 1
        self.rate = 8000
        self.dma_addr = 0
        self.frames = 0
        self.generation = 0
        self.error = AUDIO_ERR_SINK if stop_failed else AUDIO_ERR_NONE
        self.busy = False
        self.done = False
        self.playing = stop_failed
        self.last_pcm = b""
        self.last_rate = 0
        self.last_channels = 0
        self.last_frames = 0

    def release_host_sink(self) -> bool:
        """Try host cleanup without discarding the last capture."""
        if self.playing:
            if not callable(self.on_stop):
                self.error = AUDIO_ERR_SINK
                return False
            try:
                self.on_stop()
            except Exception:
                self.error = AUDIO_ERR_SINK
                return False
        self.busy = False
        self.playing = False
        return True

    def _execute(self, command: int):
        if command == AUDIO_CMD_SUBMIT:
            self._submit()
        elif command == AUDIO_CMD_STOP:
            self._stop()
        elif command == AUDIO_CMD_CLEAR:
            self._clear()
