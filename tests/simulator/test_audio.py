"""AudioOut uses actual hosted MMIO and checked ordinary-memory DMA."""
import pytest

from shared.audio_output import AUDIO_ERR_MEMORY, AUDIO_OFFSET, AUDIO_SIZE
from simulator.memory import MMIO_BASE, MMIOAccessError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime


BASE = MMIO_BASE + AUDIO_OFFSET


def test_public_status_probe_and_registers_are_present():
    memory = create_one_core_address_space()
    runtime = MegaForthRuntime(memory=memory)
    runtime.evaluate(b": PROBE 0xFFFFFF0000000C01 C@ ; PROBE")
    assert runtime.main_context.data.snapshot() == (0x80,)
    assert memory.read8(BASE + 0x19) == 1
    # Registers permit the same bytewise unaligned scalar accesses as the bus.
    assert memory.read32(BASE + 1) == 0x40010180


def test_submit_captures_checked_sparse_memory_and_retains_snapshot():
    memory = create_one_core_address_space()
    audio = memory.mmio.audio
    memory.write_bytes(0x8000, b"\x01\x02\x03\x04")
    memory.write64(BASE + 8, 0x8000)
    memory.write32(BASE + 16, 2)
    memory.write8(BASE, 1)
    memory.write8(0x8000, 0xFF)
    assert audio.last_pcm == b"\x01\x02\x03\x04"
    assert (audio.last_rate, audio.last_channels, audio.last_frames) == (8000, 1, 2)
    assert memory.read8(BASE + 1) == 0x82
    assert memory.read32(BASE + 20) == 1
    memory.write8(BASE, 3)
    assert audio.last_pcm == b""
    assert memory.read8(BASE + 1) == 0x80
    assert memory.read32(BASE + 20) == 1


@pytest.mark.parametrize("address", [0xFFFFE, 0x100000, BASE, (1 << 64) - 2])
def test_invalid_dma_preserves_prior_capture_without_bus_reads(address):
    memory = create_one_core_address_space()
    audio = memory.mmio.audio
    memory.write_bytes(0x8000, b"good")
    memory.write64(BASE + 8, 0x8000)
    memory.write32(BASE + 16, 2)
    memory.write8(BASE, 1)
    memory.write64(BASE + 8, address)
    memory.write8(BASE, 1)
    assert memory.read8(BASE + 24) == AUDIO_ERR_MEMORY
    assert audio.last_pcm == b"good"
    assert memory.read32(BASE + 20) == 1


def test_window_crossing_is_preflighted_and_diagnostic_retains_exact_address():
    memory = create_one_core_address_space()
    with pytest.raises(MMIOAccessError) as caught:
        memory.write16(BASE + AUDIO_SIZE - 1, 0xFFFF)
    assert caught.value.address == BASE + AUDIO_SIZE - 1
    assert caught.value.length == 2
    assert "address=0xffffff0000000c1f, length=2" in str(caught.value)
    assert memory.read8(BASE + 1) == 0x80
