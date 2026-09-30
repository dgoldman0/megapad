"""AudioOut uses actual hosted MMIO and checked ordinary-memory DMA."""
import pytest

from shared.audio_output import AUDIO_ERR_MEMORY, AUDIO_OFFSET, AUDIO_SIZE
from simulator.audio import HostedAudioService
from simulator.memory import MMIO_BASE, MMIOAccessError, SparseAddressSpace
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


def test_submit_reads_one_sparse_span_without_materializing_missing_pages(monkeypatch):
    span_reads = []
    original_bind = HostedAudioService._bind_memory

    def bind_memory(device, *, read_byte, span_valid, read_span=None,
                    span_eligible=None):
        assert read_span is not None

        def observe_span(address, count):
            span_reads.append((address, count))
            return read_span(address, count)

        original_bind(device, read_byte=read_byte, span_valid=span_valid,
                      read_span=observe_span, span_eligible=span_eligible)

    monkeypatch.setattr(HostedAudioService, "_bind_memory", bind_memory)
    memory = create_one_core_address_space()
    address = memory.page_size - 2
    count = memory.page_size + 4
    memory.write_bytes(address, b"ab")
    memory.write_bytes(address + count - 2, b"cd")
    assert memory.resident_page_count == 2
    memory.write64(BASE + 8, address)
    memory.write32(BASE + 16, count // 2)

    memory.write8(BASE, 1)

    assert span_reads == [(address, count)]
    assert memory.mmio.audio.last_pcm == b"ab" + bytes(memory.page_size) + b"cd"
    assert memory.resident_page_count == 2
    assert memory.read8(BASE + 1) == 0x82


@pytest.mark.parametrize("customization", ["subclass", "instance", "class", "integer"])
def test_initial_custom_memory_reader_keeps_byte_visible_audio(customization, monkeypatch):
    reads = []

    def read_byte(memory, address):
        reads.append(address)
        return 0x1FE

    if customization == "subclass":
        class CustomMemory(SparseAddressSpace):
            read8 = read_byte

        memory = CustomMemory()
    else:
        memory = SparseAddressSpace()
        if customization == "instance":
            memory.read8 = lambda address: read_byte(memory, address)
        elif customization == "class":
            monkeypatch.setattr(SparseAddressSpace, "read8", read_byte)
        else:
            memory._read_integer = lambda address, width: read_byte(memory, address)
    memory.write_bytes(0x8000, b"real")
    audio = HostedAudioService()
    audio.bind(memory)
    audio.dma_addr = 0x8000
    audio.frames = 2

    audio.write8(AUDIO_OFFSET, 1)

    assert audio.last_pcm == b"\xfe" * 4
    assert audio.error == 0
    assert reads == [0x8000, 0x8001, 0x8002, 0x8003]


def test_submit_rechecks_scalar_helpers_after_memory_binding():
    memory = create_one_core_address_space()
    memory.write_bytes(0x8000, b"real")
    audio = memory.mmio.audio
    audio.dma_addr = 0x8000
    audio.frames = 2
    audio.write8(AUDIO_OFFSET, 1)
    reads = []

    def changed_integer(address, width):
        reads.append((address, width))
        return 0x1FE

    memory._read_integer = changed_integer
    audio.write8(AUDIO_OFFSET, 1)

    assert audio.last_pcm == b"\xfe" * 4
    assert audio.generation == 2
    assert audio.error == 0
    assert reads == [(0x8000 + index, 1) for index in range(4)]


@pytest.mark.parametrize("bind_after_override", [False, True])
def test_custom_block_resolver_keeps_scalar_audio_reads(bind_after_override):
    memory = SparseAddressSpace()
    memory.write_bytes(0x8000, b"real")
    audio = HostedAudioService()
    if not bind_after_override:
        audio.bind(memory)
    original_resolve = memory._resolve
    operations = []

    def custom_resolve(address, length, *, operation):
        operations.append(operation)
        if operation == "read":
            raise RuntimeError("custom block reads are unavailable")
        return original_resolve(address, length, operation=operation)

    memory._resolve = custom_resolve
    if bind_after_override:
        audio.bind(memory)
    audio.dma_addr = 0x8000
    audio.frames = 2

    audio.write8(AUDIO_OFFSET, 1)

    assert audio.last_pcm == b"real"
    assert audio.generation == 1
    assert audio.error == 0
    assert operations == ["qualify"]


def test_submit_uses_checked_spans_at_each_hosted_region_end():
    memory = create_one_core_address_space(
        hbw_size=4096, external_size=4096, vram_size=4096,
    )
    audio = memory.mmio.audio
    memory.write32(BASE + 16, 2)

    for index, region in enumerate(memory.regions):
        address = region.limit - 4
        pcm = bytes((index, 0x80, 0xFF, 0x7F))
        memory.write_bytes(address, pcm)
        memory.write64(BASE + 8, address)
        memory.write8(BASE, 1)
        assert audio.last_pcm == pcm
        assert audio.generation == index + 1
        assert audio.error == 0

        # Even adjacent physical regions cannot form one audio DMA span.
        memory.write64(BASE + 8, address + 1)
        memory.write8(BASE, 1)
        assert audio.error == AUDIO_ERR_MEMORY
        assert audio.last_pcm == pcm
        assert audio.generation == index + 1


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
