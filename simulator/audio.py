"""One-core AudioOut mapping over the shared synchronous PCM model."""
from __future__ import annotations

from shared.audio_output import (
    AUDIO_LIMIT, AUDIO_MAX_CAPTURE_BYTES, AUDIO_OFFSET, AudioOutputModel,
)
from simulator.memory import SparseAddressSpace


_CANONICAL_AUDIO_MEMORY_METHODS = tuple(
    (name, getattr(SparseAddressSpace, name))
    for name in (
        "read8", "_read_integer", "_qualify_ordinary_span", "read_bytes",
        "_resolve", "_region_at",
    )
)


class HostedAudioService(AudioOutputModel):
    def __init__(self, *, max_capture_bytes: int = AUDIO_MAX_CAPTURE_BYTES):
        super().__init__(max_capture_bytes=max_capture_bytes)

    def bind(self, memory: SparseAddressSpace) -> None:
        if self._mem_read is not None:
            raise RuntimeError("AudioOut is already bound")

        def valid_span(address: int, length: int) -> bool:
            # Qualification rejects MMIO, holes, wrapping, and region crossings
            # without reading bytes or materializing sparse pages.
            memory._qualify_ordinary_span(address, length)
            return True

        # Inherited block reads cannot stand in for a customized scalar read.
        # Keep subclasses and modified memory methods on the byte path.
        def bulk_eligible(address, count):
            if type(memory) is not SparseAddressSpace:
                return False
            for name, implementation in _CANONICAL_AUDIO_MEMORY_METHODS:
                callback = getattr(memory, name)
                if (getattr(callback, "__self__", None) is not memory
                        or getattr(callback, "__func__", None) is not implementation):
                    return False
            return True

        self._bind_memory(
            read_byte=memory.read8,
            span_valid=valid_span,
            read_span=memory.read_bytes if bulk_eligible(0, 0) else None,
            span_eligible=bulk_eligible,
        )

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
