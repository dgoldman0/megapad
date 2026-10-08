"""Contracts for the one-shot PCM audio output device."""

import pytest

from devices import (
    AUDIO_BASE,
    AUDIO_CMD_CLEAR,
    AUDIO_CMD_STOP,
    AUDIO_CMD_SUBMIT,
    AUDIO_ERR_CAPACITY,
    AUDIO_ERR_BUSY,
    AUDIO_ERR_CHANNELS,
    AUDIO_ERR_FORMAT,
    AUDIO_ERR_FRAMES,
    AUDIO_ERR_MEMORY,
    AUDIO_ERR_RATE,
    AUDIO_ERR_SINK,
    AUDIO_FORMAT_S16LE,
    AudioOutput,
)
from system import EXT_MEM_BASE, HBW_BASE, MMIO_START, VRAM_BASE, MegapadSystem


def _write_le(device, offset, value, size):
    for index in range(size):
        device.write8(offset + index, (value >> (8 * index)) & 0xFF)


def _configured_device(pcm=b"\x01\x02\x03\x04", *, frames=2, channels=1,
                       rate=8000, limit=1024):
    memory = bytearray(64)
    memory[8:8 + len(pcm)] = pcm
    device = AudioOutput(max_capture_bytes=limit)
    device._mem_read = lambda address: memory[address]
    device._mem_span_valid = lambda address, count: (
        0 <= address <= len(memory) and 0 <= count <= len(memory) - address)
    device.write8(0x02, AUDIO_FORMAT_S16LE)
    device.write8(0x03, channels)
    _write_le(device, 0x04, rate, 4)
    _write_le(device, 0x08, 8, 8)
    _write_le(device, 0x10, frames, 4)
    return device


def test_audio_defaults_are_headless_and_present():
    device = AudioOutput()

    assert device.read8(0x01) == 0x80
    assert device.read8(0x02) == AUDIO_FORMAT_S16LE
    assert device.read8(0x03) == 1
    assert device.read8(0x19) == 0x01


def test_submit_copies_guest_pcm_and_latches_metadata():
    device = _configured_device()

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.last_pcm == b"\x01\x02\x03\x04"
    assert device.last_rate == 8000
    assert device.last_channels == 1
    assert device.last_frames == 2
    assert device.generation == 1
    assert device.read8(0x01) == 0x82
    assert device.read8(0x18) == 0


def test_submit_uses_an_immutable_snapshot():
    memory = bytearray(range(32))
    device = AudioOutput()
    device._mem_read = lambda address: memory[address]
    device._mem_span_valid = lambda address, count: (
        0 <= address <= len(memory) and 0 <= count <= len(memory) - address)
    _write_le(device, 0x08, 4, 8)
    _write_le(device, 0x10, 4, 4)

    device.write8(0x00, AUDIO_CMD_SUBMIT)
    captured = device.last_pcm
    memory[4:12] = b"\xff" * 8

    assert captured == bytes(range(4, 12))
    assert device.last_pcm == captured


@pytest.mark.parametrize("result_type", [bytes, bytearray, memoryview])
def test_bulk_submit_copies_once_and_publishes_before_host_playback(result_type):
    device = _configured_device()
    memory = bytearray(b"\x01\x02\x03\x04")
    byte_reads = []
    span_reads = []
    sink_observations = []

    def read_span(address, count):
        span_reads.append((address, count, device.busy))
        return result_type(memory)

    device._bind_memory(
        read_byte=lambda address: byte_reads.append(address) or 0,
        span_valid=device._mem_span_valid,
        read_span=read_span,
    )
    device.on_submit = lambda pcm, rate, channels: sink_observations.append((
        pcm, rate, channels, device.last_pcm, device.last_rate,
        device.last_channels, device.last_frames, device.generation,
        device.busy, device.done,
    ))
    device.on_stop = lambda: None
    device.on_playing = lambda: True

    device.write8(0x00, AUDIO_CMD_SUBMIT)
    memory[:] = b"gone"

    assert span_reads == [(8, 4, True)]
    assert byte_reads == []
    assert sink_observations == [(
        b"\x01\x02\x03\x04", 8000, 1, b"\x01\x02\x03\x04",
        8000, 1, 2, 1, False, True,
    )]
    assert device.last_pcm == b"\x01\x02\x03\x04"
    assert device.read8(0x01) == 0x8A


@pytest.mark.parametrize("outcome", [
    b"short", b"too long", 6, [0] * 6, None, RuntimeError("read failed"),
])
def test_bulk_failure_preserves_capture_without_retry_or_sink(outcome):
    device = _configured_device()
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    byte_reads = []
    sink_calls = []

    def read_span(address, count):
        assert (address, count) == (8, 6)
        if isinstance(outcome, Exception):
            raise outcome
        return outcome

    device._bind_memory(
        read_byte=lambda address: byte_reads.append(address) or 0,
        span_valid=device._mem_span_valid,
        read_span=read_span,
    )
    device.frames = 3
    device.rate = 16000
    device.on_submit = lambda *args: sink_calls.append(args)
    device.on_stop = lambda: None
    device.on_playing = lambda: False

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.error == AUDIO_ERR_MEMORY
    assert device.busy is False
    assert device.done is True
    assert device.playing is False
    assert (device.last_pcm, device.last_rate, device.last_channels,
            device.last_frames, device.generation) == (
        b"\x01\x02\x03\x04", 8000, 1, 2, 1,
    )
    assert byte_reads == []
    assert sink_calls == []


@pytest.mark.parametrize("replacement", ["read", "validator", "during_preflight"])
def test_changed_memory_callbacks_use_masked_byte_reads(replacement):
    device = _configured_device()
    byte_reads = []
    span_reads = []

    def replacement_read(address):
        byte_reads.append(address)
        return -1

    def valid_span(address, count):
        if replacement == "during_preflight":
            device._mem_read = replacement_read
        return True

    device._bind_memory(
        read_byte=lambda address: byte_reads.append(address) or (address + 249),
        span_valid=valid_span,
        read_span=lambda address, count: span_reads.append((address, count)) or b"old!",
    )
    if replacement == "read":
        device._mem_read = replacement_read
    elif replacement == "validator":
        device._mem_span_valid = lambda address, count: True

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    expected = b"\x01\x02\x03\x04" if replacement == "validator" else b"\xff" * 4
    assert device.last_pcm == expected
    assert device.error == 0
    assert byte_reads == [8, 9, 10, 11]
    assert span_reads == []


@pytest.mark.parametrize("failure", ["format", "span", "exception"])
def test_bulk_binding_preserves_validation_before_memory_reads(failure):
    device = _configured_device()
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    reads = []

    def valid_span(address, count):
        if failure == "exception":
            raise RuntimeError("invalid memory")
        return failure != "span"

    device._bind_memory(
        read_byte=lambda address: reads.append(address) or 0,
        span_valid=valid_span,
        read_span=lambda address, count: reads.append((address, count)) or bytes(count),
    )
    if failure == "format":
        device.format = 99

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert reads == []
    assert device.error == (AUDIO_ERR_FORMAT if failure == "format" else AUDIO_ERR_MEMORY)
    assert device.last_pcm == b"\x01\x02\x03\x04"
    assert device.generation == 1
    assert device.done is True


def test_bulk_binding_survives_reset_and_release_preserves_capture():
    device = _configured_device()
    span_reads = []
    stops = []
    device._bind_memory(
        read_byte=lambda address: pytest.fail("unexpected byte read"),
        span_valid=device._mem_span_valid,
        read_span=lambda address, count: span_reads.append((address, count)) or b"data",
    )
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: stops.append(True)
    device.on_playing = lambda: True
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    device.reset()
    device.dma_addr = 8
    device.frames = 2
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert device.release_host_sink() is True

    assert span_reads == [(8, 4), (8, 4)]
    assert stops == [True, True]
    assert device.last_pcm == b"data"
    assert device.generation == 1
    assert device.done is True
    assert device.playing is False


def test_optional_sink_and_stop_callbacks_are_explicit_capabilities():
    submissions = []
    stops = []
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: submissions.append(
        (pcm, rate, channels))

    # Partial host wiring cannot advertise or start an unmanaged voice.
    assert device.read8(0x19) == 0x01
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert submissions == []

    device.write8(0x00, AUDIO_CMD_CLEAR)
    device.on_stop = lambda: stops.append(True)
    device.on_playing = lambda: True

    assert device.read8(0x19) == 0x03
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert submissions == [(b"\x01\x02\x03\x04", 8000, 1)]
    assert device.read8(0x01) == 0x8A

    device.write8(0x00, AUDIO_CMD_STOP)
    assert stops == [True]
    assert device.read8(0x01) == 0x82


def test_clear_releases_capture_but_preserves_generation():
    device = _configured_device()
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    device.write8(0x00, AUDIO_CMD_CLEAR)

    assert device.last_pcm == b""
    assert device.generation == 1
    assert device.read8(0x01) == 0x80


def test_invalid_contracts_fail_without_reading_guest_memory():
    cases = (
        (lambda d: d.write8(0x02, 99), AUDIO_ERR_FORMAT),
        (lambda d: d.write8(0x03, 3), AUDIO_ERR_CHANNELS),
        (lambda d: _write_le(d, 0x04, 7999, 4), AUDIO_ERR_RATE),
        (lambda d: _write_le(d, 0x10, 0, 4), AUDIO_ERR_FRAMES),
    )
    for mutate, expected in cases:
        device = _configured_device()
        reads = []
        device._mem_read = lambda address: reads.append(address) or 0
        mutate(device)
        device.write8(0x00, AUDIO_CMD_SUBMIT)
        assert device.read8(0x18) == expected
        assert device.read8(0x01) == 0x84
        assert reads == []


def test_capture_capacity_and_missing_memory_are_reported():
    device = _configured_device(frames=3, limit=4)
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert device.read8(0x18) == AUDIO_ERR_CAPACITY

    device = _configured_device()
    device._mem_read = None
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert device.read8(0x18) == AUDIO_ERR_MEMORY


def test_sink_failure_does_not_hide_the_deterministic_capture():
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: False
    device.on_stop = lambda: None
    device.on_playing = lambda: False

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.last_pcm == b"\x01\x02\x03\x04"
    assert device.generation == 1
    assert device.read8(0x18) == AUDIO_ERR_SINK
    assert device.read8(0x01) == 0x86


def test_throwing_submit_retains_conservative_voice_ownership():
    stops = []
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: (
        _ for _ in ()).throw(RuntimeError("late host failure"))
    device.on_stop = lambda: stops.append(True)
    device.on_playing = lambda: True

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.last_pcm == b"\x01\x02\x03\x04"
    assert device.read8(0x01) == 0x8E
    device.write8(0x00, AUDIO_CMD_STOP)
    assert stops == [True]
    assert device.playing is False


def test_reset_releases_playback_and_restores_power_on_state():
    stops = []
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: stops.append(True)
    device.on_playing = lambda: True
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    device.reset()

    assert stops == [True]
    assert device.read8(0x01) == 0x80
    assert device.read8(0x02) == AUDIO_FORMAT_S16LE
    assert device.read8(0x03) == 1
    assert device.rate == 8000
    assert device.frames == 0
    assert device.generation == 0
    assert device.last_pcm == b""


def test_reset_failure_preserves_observable_voice_ownership():
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: (_ for _ in ()).throw(RuntimeError("stuck"))
    device.on_playing = lambda: True
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    device.reset()

    assert device.generation == 0
    assert device.last_pcm == b""
    assert device.read8(0x01) == 0x8C
    assert device.error == AUDIO_ERR_SINK


def test_failed_release_retains_voice_ownership_and_capture():
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: (_ for _ in ()).throw(RuntimeError("gone"))
    device.on_playing = lambda: True
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.release_host_sink() is False

    assert device.playing is True
    assert device.done is True
    assert device.error == AUDIO_ERR_SINK
    assert device.last_pcm == b"\x01\x02\x03\x04"


def test_status_observes_natural_host_completion():
    active = [True]
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: active.__setitem__(0, False)
    device.on_playing = lambda: active[0]
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert device.read8(0x01) == 0x8A

    active[0] = False

    assert device.read8(0x01) == 0x82
    assert device.done is True


def test_stop_failure_and_clear_preserve_ownership_and_diagnostics():
    device = _configured_device()
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: (_ for _ in ()).throw(RuntimeError("stuck"))
    device.on_playing = lambda: True
    device.write8(0x00, AUDIO_CMD_SUBMIT)

    device.write8(0x00, AUDIO_CMD_STOP)
    assert device.read8(0x01) == 0x8E
    assert device.last_pcm == b"\x01\x02\x03\x04"

    device.write8(0x00, AUDIO_CMD_CLEAR)
    assert device.read8(0x01) == 0x8E
    assert device.done is True
    assert device.last_pcm == b"\x01\x02\x03\x04"


def test_busy_rejection_is_non_destructive():
    device = _configured_device()
    device.busy = True
    device.done = True
    device.playing = True
    device.on_submit = lambda pcm, rate, channels: True
    device.on_stop = lambda: None
    device.on_playing = lambda: True
    device.last_pcm = b"prior"

    device.write8(0x00, AUDIO_CMD_SUBMIT)

    assert device.busy is True
    assert device.done is True
    assert device.playing is True
    assert device.last_pcm == b"prior"
    assert device.error == AUDIO_ERR_BUSY


def test_system_routes_audio_mmio_and_dma_to_shared_memory():
    system = MegapadSystem(ram_size=1 << 20)
    system.load_binary(0x2000, b"\x00\x80\xff\x7f")
    base = MMIO_START + AUDIO_BASE

    system.cpu.mem_write8(base + 0x02, AUDIO_FORMAT_S16LE)
    system.cpu.mem_write8(base + 0x03, 1)
    for index, byte in enumerate((0x40, 0x1F, 0, 0)):  # 8000 Hz
        system.cpu.mem_write8(base + 0x04 + index, byte)
    for index in range(8):
        system.cpu.mem_write8(base + 0x08 + index, (0x2000 >> (8 * index)) & 0xFF)
    for index, byte in enumerate((2, 0, 0, 0)):
        system.cpu.mem_write8(base + 0x10 + index, byte)

    system.cpu.mem_write8(base, AUDIO_CMD_SUBMIT)

    assert system.audio.last_pcm == b"\x00\x80\xff\x7f"
    assert system.cpu.mem_read8(base + 0x01) == 0x82


def test_system_audio_copies_bounded_spans_in_each_physical_window(monkeypatch):
    span_reads = []
    byte_reads = []
    original_bind = AudioOutput._bind_memory

    def bind_memory(device, *, read_byte, span_valid, read_span=None,
                    span_eligible=None):
        assert read_span is not None

        def observe_span(address, count):
            span_reads.append((address, count))
            return read_span(address, count)

        def observe_byte(address):
            byte_reads.append(address)
            return read_byte(address)

        original_bind(device, read_byte=observe_byte, span_valid=span_valid,
                      read_span=observe_span, span_eligible=span_eligible)

    monkeypatch.setattr(AudioOutput, "_bind_memory", bind_memory)
    system = MegapadSystem(
        ram_size=1 << 20, hbw_size=4096, ext_mem_size=4096, vram_size=4096,
    )
    addresses = (
        system.ram_size - 4, HBW_BASE + 4092,
        EXT_MEM_BASE + 4092, VRAM_BASE + 4092,
    )
    device = system.audio
    device.frames = 2
    byte_reads.clear()

    for index, address in enumerate(addresses):
        pcm = bytes((index, 0x80, 0xFF, 0x7F))
        system.load_binary(address, pcm)
        device.dma_addr = address
        device.write8(0x00, AUDIO_CMD_SUBMIT)
        assert device.last_pcm == pcm
        assert device.generation == index + 1
        assert device.error == 0

    assert span_reads == [(address, 4) for address in addresses]
    assert byte_reads == []


@pytest.mark.parametrize("customization", ["subclass", "instance", "class"])
def test_initial_custom_system_reader_keeps_byte_visible_audio(customization, monkeypatch):
    reads = []

    def read_byte(system, address):
        reads.append(address)
        return 0x1FE

    options = dict(
        ram_size=1 << 20, num_cores=1, num_clusters=0,
        hbw_size=0, ext_mem_size=0, vram_size=0, worker_count=1,
    )
    if customization == "subclass":
        class CustomSystem(MegapadSystem):
            _raw_mem_read = read_byte

        system = CustomSystem(**options)
    elif customization == "instance":
        system = MegapadSystem.__new__(MegapadSystem)
        system._raw_mem_read = lambda address: read_byte(system, address)
        MegapadSystem.__init__(system, **options)
    else:
        monkeypatch.setattr(MegapadSystem, "_raw_mem_read", read_byte)
        system = MegapadSystem(**options)
    system.load_binary(0x2000, b"real")
    reads.clear()
    system.audio.dma_addr = 0x2000
    system.audio.frames = 2

    system.audio.write8(0, AUDIO_CMD_SUBMIT)

    assert system.audio.last_pcm == b"\xfe" * 4
    assert system.audio.error == 0
    assert reads == [0x2000, 0x2001, 0x2002, 0x2003]


def test_system_audio_rechecks_helpers_after_memory_binding():
    system = MegapadSystem(
        ram_size=1 << 20, num_cores=1, num_clusters=0,
        hbw_size=0, ext_mem_size=0, vram_size=0, worker_count=1,
    )
    system.load_binary(0x2000, b"real")
    device = system.audio
    device.dma_addr = 0x2000
    device.frames = 2
    device.write8(0, AUDIO_CMD_SUBMIT)
    window_calls = []

    def redirected_window(address, count):
        window_calls.append((address, count))
        return bytearray(b"fake"), 0

    system._raw_mem_window = redirected_window
    device.write8(0, AUDIO_CMD_SUBMIT)

    assert device.last_pcm == b"real"
    assert device.generation == 2
    assert device.error == 0
    # The replacement still serves validation, while the byte reader retains
    # its existing priority rules instead of following the redirected window.
    assert window_calls == [(0x2000, 4)]


def test_system_audio_preserves_scalar_priority_across_overlapping_apertures():
    system = MegapadSystem(
        ram_size=2 << 20, num_cores=1, num_clusters=0,
        hbw_size=0, ext_mem_size=4096, vram_size=0, worker_count=1,
    )
    address = EXT_MEM_BASE - 2
    system.load_binary(address, b"BANK")
    system.load_binary(EXT_MEM_BASE, b"EX")
    assert system._raw_mem_span_valid(address, 4)
    device = system.audio
    device.dma_addr = address
    device.frames = 2

    device.write8(0, AUDIO_CMD_SUBMIT)

    assert device.last_pcm == b"BAEX"
    assert device.generation == 1
    assert device.error == 0


def test_system_rejects_wrapping_mmio_and_u64_overflow_dma_spans():
    system = MegapadSystem(ram_size=1 << 20)
    device = system.audio
    device.frames = 1

    for address in (
        system.ram_size - 1,
        MMIO_START,
        (1 << 64) - 1,
    ):
        device.dma_addr = address
        device.write8(0x00, AUDIO_CMD_SUBMIT)
        assert device.error == AUDIO_ERR_MEMORY
        assert device.generation == 0

    system.load_binary(system.ram_size - 2, b"\x34\x12")
    device.dma_addr = system.ram_size - 2
    device.write8(0x00, AUDIO_CMD_SUBMIT)
    assert device.error == 0
    assert device.last_pcm == b"\x34\x12"


def test_dma_span_validator_uses_actual_physical_window_boundaries():
    system = MegapadSystem(
        ram_size=1 << 20,
        hbw_size=4096,
        ext_mem_size=4096,
        vram_size=4096,
    )
    valid = (
        (0, 2),
        (system.ram_size - 2, 2),
        (EXT_MEM_BASE, 2),
        (EXT_MEM_BASE + 4094, 2),
        (VRAM_BASE, 2),
        (VRAM_BASE + 4094, 2),
        (HBW_BASE, 2),
        (HBW_BASE + 4094, 2),
    )
    invalid = (
        (system.ram_size - 1, 2),
        (EXT_MEM_BASE + 4095, 2),
        (VRAM_BASE + 4095, 2),
        (HBW_BASE + 4095, 2),
        (MMIO_START, 2),
        ((1 << 64) - 1, 2),
    )

    assert all(system._raw_mem_span_valid(address, count)
               for address, count in valid)
    assert not any(system._raw_mem_span_valid(address, count)
                   for address, count in invalid)


def test_system_cold_boot_resets_audio_and_releases_host_voice():
    system = MegapadSystem(ram_size=1 << 20)
    stops = []
    system.audio.on_submit = lambda pcm, rate, channels: True
    system.audio.on_stop = lambda: stops.append(True)
    system.audio.on_playing = lambda: True
    system.load_binary(0x2000, b"\x00\x00")
    system.audio.dma_addr = 0x2000
    system.audio.frames = 1
    system.audio.write8(0x00, AUDIO_CMD_SUBMIT)

    system.boot()

    assert stops == [True]
    assert system.audio.generation == 0
    assert system.audio.last_pcm == b""
    assert system.audio.read8(0x01) == 0x80
