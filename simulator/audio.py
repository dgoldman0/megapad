"""One-core AudioOut mapping over the shared synchronous PCM model."""
from __future__ import annotations

from shared.audio_output import (
    AUDIO_LIMIT, AUDIO_MAX_CAPTURE_BYTES, AUDIO_OFFSET, AudioOutputModel,
)
from simulator.memory import SparseAddressSpace


class HostedAudioService(AudioOutputModel):
    def __init__(self, *, max_capture_bytes: int = AUDIO_MAX_CAPTURE_BYTES):
        super().__init__(max_capture_bytes=max_capture_bytes)

    def bind(self, memory: SparseAddressSpace) -> None:
        if self._mem_read is not None:
            raise RuntimeError("AudioOut is already bound")
        self._mem_read = memory.read8

        def valid_span(address: int, length: int) -> bool:
            # Qualification rejects MMIO, holes, wrapping, and region crossings
            # without reading bytes or materializing sparse pages.
            memory._qualify_ordinary_span(address, length)
            return True

        self._mem_span_valid = valid_span

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        if (width not in (1, 2, 4, 8)
                or not AUDIO_OFFSET <= offset < AUDIO_LIMIT
                or width > AUDIO_LIMIT - offset):
            raise ValueError("access escapes the AudioOut scalar MMIO window")

    def read8(self, offset: int) -> int:
        self.preflight(offset, 1, write=False)
        return super().read8(offset - AUDIO_OFFSET)

    def write8(self, offset: int, value: int) -> None:
        self.preflight(offset, 1, write=True)
        super().write8(offset - AUDIO_OFFSET, value)
