"""Optional host playback sinks for the MegaPad PCM output device."""

from __future__ import annotations

import threading


class AudioSinkUnavailable(RuntimeError):
    """Raised when an optional host playback backend cannot be opened."""


class PygameAudioSink:
    """One-voice signed-PCM16 sink backed by ``pygame.mixer``.

    The device-facing callback remains synchronous: a submission is accepted
    only after the mixer has the requested format and a channel has started.
    Reconfiguration stops the previous voice.  Importing this module has no
    pygame or audio-server side effects; construction is the explicit opt-in.
    """

    def __init__(self, *, initial_rate: int = 8000, initial_channels: int = 1,
                 buffer_frames: int = 512, pygame_module=None):
        if buffer_frames <= 0:
            raise ValueError("buffer_frames must be positive")
        if pygame_module is None:
            try:
                import pygame as pygame_module
            except ImportError as exc:
                raise AudioSinkUnavailable("pygame is not installed") from exc
        self._pygame = pygame_module
        self._buffer_frames = int(buffer_frames)
        self._lock = threading.RLock()
        self._config: tuple[int, int] | None = None
        self._sound = None
        self._channel = None
        self._closed = False
        self._configure(initial_rate, initial_channels)

    @property
    def configuration(self) -> tuple[int, int] | None:
        return self._config

    def _configure(self, rate: int, channels: int) -> None:
        if not 8000 <= rate <= 192000:
            raise ValueError("sample rate must be 8000..192000 Hz")
        if channels not in (1, 2):
            raise ValueError("channels must be mono or stereo")
        if self._config == (rate, channels):
            return
        try:
            if self._pygame.mixer.get_init() is not None:
                self._pygame.mixer.quit()
            try:
                self._pygame.mixer.init(
                    frequency=rate,
                    size=-16,
                    channels=channels,
                    buffer=self._buffer_frames,
                    allowedchanges=0,
                )
            except TypeError:
                # Pygame 1.x lacks allowedchanges.  The exact-format check
                # below still prevents silent host-side resampling.
                self._pygame.mixer.init(
                    frequency=rate,
                    size=-16,
                    channels=channels,
                    buffer=self._buffer_frames,
                )
        except Exception as exc:
            self._config = None
            raise AudioSinkUnavailable(f"cannot initialize audio mixer: {exc}") from exc
        actual = self._pygame.mixer.get_init()
        if actual != (rate, -16, channels):
            self._pygame.mixer.quit()
            self._config = None
            raise AudioSinkUnavailable(
                f"mixer selected {actual!r}, expected {(rate, -16, channels)!r}"
            )
        self._config = (rate, channels)

    def submit(self, pcm: bytes, rate: int, channels: int) -> bool:
        """Start one immutable PCM16 buffer, replacing the previous voice."""
        with self._lock:
            if self._closed:
                return False
            self.stop()
            try:
                self._configure(rate, channels)
                sound = self._pygame.mixer.Sound(buffer=bytes(pcm))
                channel = sound.play()
            except Exception:
                return False
            if channel is None:
                return False
            self._sound = sound
            self._channel = channel
            return True

    def stop(self) -> None:
        """Stop the owned voice without shutting down the shared mixer."""
        with self._lock:
            if self._channel is not None:
                self._channel.stop()
                self._channel = None
                self._sound = None

    def is_playing(self) -> bool:
        """Return whether the owned voice remains active.

        This is the device status-path callback, so it deliberately performs
        only pygame's cheap channel-state query.  Query exceptions propagate:
        the device must conservatively retain voice ownership in that case.
        """
        with self._lock:
            if self._channel is None:
                return False
            if self._channel.get_busy():
                return True
            self._channel = None
            self._sound = None
            return False

    def close(self) -> None:
        """Release the voice and the mixer initialized by this sink."""
        with self._lock:
            if self._closed:
                return
            try:
                self.stop()
            except Exception:
                pass
            if self._pygame.mixer.get_init() is not None:
                self._pygame.mixer.quit()
            self._channel = None
            self._sound = None
            self._config = None
            self._closed = True
            # Quitting the mixer is the teardown ownership barrier even when
            # the narrower channel stop failed.  The device-facing STOP path
            # still propagates that failure and retains its handle for retry.

    def __enter__(self) -> "PygameAudioSink":
        return self

    def __exit__(self, exc_type, exc, traceback) -> None:
        self.close()
