"""Contracts for the one-shot PCM audio output device."""

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
