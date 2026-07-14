"""Host playback adapter contracts without opening a real audio device."""

from audio_sinks import AudioSinkUnavailable, PygameAudioSink


class _Channel:
    def __init__(self):
        self.stops = 0
        self.busy = True
        self.raise_on_stop = None
        self.raise_on_query = None

    def stop(self):
        if self.raise_on_stop is not None:
            raise self.raise_on_stop
        self.stops += 1
        self.busy = False

    def get_busy(self):
        if self.raise_on_query is not None:
            raise self.raise_on_query
        return self.busy


class _Sound:
    def __init__(self, pcm, channel):
        self.pcm = pcm
        self._channel = channel

    def play(self):
        return self._channel


class _Mixer:
    def __init__(self):
        self.config = None
        self.init_calls = []
        self.quit_calls = 0
        self.sounds = []
        self.next_channel = _Channel()
        self.forced_config = None
        self.raise_on_init = None

    def get_init(self):
        return self.config

    def init(self, **kwargs):
        self.init_calls.append(kwargs)
        if self.raise_on_init is not None:
            raise self.raise_on_init
        self.config = self.forced_config or (
            kwargs["frequency"], kwargs["size"], kwargs["channels"])

    def quit(self):
        self.quit_calls += 1
        self.config = None

    def Sound(self, *, buffer):
        sound = _Sound(buffer, self.next_channel)
        self.sounds.append(sound)
        return sound


class _Pygame:
    def __init__(self):
        self.mixer = _Mixer()


def test_sink_initializes_exact_format_and_plays_an_immutable_copy():
    pygame = _Pygame()
    sink = PygameAudioSink(pygame_module=pygame)
    source = bytearray(b"\x01\x02\x03\x04")

    assert sink.submit(source, 8000, 1)
    source[:] = b"\xff" * 4

    assert sink.configuration == (8000, 1)
    assert pygame.mixer.sounds[-1].pcm == b"\x01\x02\x03\x04"


def test_sink_reconfigures_between_contracts_and_stops_previous_voice():
    pygame = _Pygame()
    first_channel = pygame.mixer.next_channel
    sink = PygameAudioSink(pygame_module=pygame)
    assert sink.submit(b"\0\0", 8000, 1)

    second_channel = _Channel()
    pygame.mixer.next_channel = second_channel
    assert sink.submit(b"\0\0\0\0", 16000, 2)

    assert first_channel.stops == 1
    assert sink.configuration == (16000, 2)
    sink.stop()
    assert second_channel.stops == 1


def test_sink_observes_natural_completion_and_releases_its_handles():
    pygame = _Pygame()
    channel = pygame.mixer.next_channel
    sink = PygameAudioSink(pygame_module=pygame)
    assert sink.submit(b"\0\0", 8000, 1)
    assert sink.is_playing()

    channel.busy = False

    assert not sink.is_playing()
    sink.stop()
    assert channel.stops == 0


def test_failed_stop_retains_the_voice_handle_for_query_and_retry():
    pygame = _Pygame()
    channel = pygame.mixer.next_channel
    sink = PygameAudioSink(pygame_module=pygame)
    assert sink.submit(b"\0\0", 8000, 1)
    channel.raise_on_stop = RuntimeError("driver failure")

    try:
        sink.stop()
    except RuntimeError as exc:
        assert "driver failure" in str(exc)
    else:
        raise AssertionError("failed stop was reported as success")
    assert sink.is_playing()

    channel.raise_on_stop = None
    sink.stop()
    assert channel.stops == 1


def test_sink_rejects_host_format_substitution():
    pygame = _Pygame()
    pygame.mixer.forced_config = (44100, -16, 2)

    try:
        PygameAudioSink(pygame_module=pygame)
    except AudioSinkUnavailable as exc:
        assert "expected" in str(exc)
    else:
        raise AssertionError("substituted format was accepted")


def test_sink_initialization_failure_is_an_explicit_unavailable_state():
    pygame = _Pygame()
    pygame.mixer.raise_on_init = RuntimeError("no device")

    try:
        PygameAudioSink(pygame_module=pygame)
    except AudioSinkUnavailable as exc:
        assert "no device" in str(exc)
    else:
        raise AssertionError("missing audio device was accepted")


def test_close_is_idempotent_and_rejects_later_submissions():
    pygame = _Pygame()
    sink = PygameAudioSink(pygame_module=pygame)
    assert sink.submit(b"\0\0", 8000, 1)

    sink.close()
    quits = pygame.mixer.quit_calls
    sink.close()

    assert pygame.mixer.quit_calls == quits
    assert sink.configuration is None
    assert not sink.submit(b"\0\0", 8000, 1)


def test_close_uses_mixer_shutdown_when_channel_stop_fails():
    pygame = _Pygame()
    channel = pygame.mixer.next_channel
    sink = PygameAudioSink(pygame_module=pygame)
    assert sink.submit(b"\0\0", 8000, 1)
    channel.raise_on_stop = RuntimeError("channel disappeared")

    sink.close()

    assert pygame.mixer.quit_calls == 1
    assert sink.configuration is None
    assert not sink.submit(b"\0\0", 8000, 1)
