"""Executable contract tests for the Megapad storage controller."""

from __future__ import annotations

import os
import subprocess
import sys

import pytest

from devices import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_COMPLETION,
    STORAGE_CAP_FLUSH,
    STORAGE_CAP_GEN_GUARD,
    STORAGE_CAP_MEDIA_GEN,
    STORAGE_CAP_PRECISE_RESULT,
    STORAGE_CAP_READ,
    STORAGE_CAP_WRITE,
    STORAGE_CMD_FLUSH,
    STORAGE_CMD_READ,
    STORAGE_CMD_RESET,
    STORAGE_CMD_STATUS,
    STORAGE_CMD_WRITE,
    STORAGE_RESULT_ADDRESS_OVERFLOW,
    STORAGE_RESULT_DMA_FAILURE,
    STORAGE_RESULT_DMA_INVALID,
    STORAGE_RESULT_FLUSH_FAILURE,
    STORAGE_RESULT_INVALID_COUNT,
    STORAGE_RESULT_LBA_RANGE,
    STORAGE_RESULT_MEDIA_FAILURE,
    STORAGE_RESULT_MEDIA_REMOVED,
    STORAGE_RESULT_NO_MEDIA,
    STORAGE_RESULT_OK,
    STORAGE_RESULT_PARTIAL,
    STORAGE_RESULT_RESET_ABORTED,
    STORAGE_RESULT_TIMEOUT,
    STORAGE_RESULT_UNSUPPORTED,
    STORAGE_RESULT_WRITE_PROTECTED,
    STORAGE_STATUS_BUSY,
    STORAGE_STATUS_ERROR,
    STORAGE_STATUS_MEDIA_CHANGED,
    STORAGE_STATUS_PRESENT,
    STORAGE_STATUS_REJECTED,
    STORAGE_STATUS_RESULT_VALID,
    STORAGE_STATUS_WRITE_PROTECTED,
    Storage,
)
from system import (
    EXT_MEM_BASE,
    HBW_BASE,
    HBW_SIZE,
    MMIO_START,
    VRAM_BASE,
    MegapadSystem,
)


def _write_le(device: Storage, offset: int, value: int, size: int) -> None:
    for index in range(size):
        device.write8(offset + index, (value >> (8 * index)) & 0xFF)


def _read_le(device: Storage, offset: int, size: int) -> int:
    return sum(device.read8(offset + index) << (8 * index)
               for index in range(size))


def _attached_device(tmp_path, *, sectors=4, fill=0):
    path = tmp_path / "storage.img"
    path.write_bytes(bytes([fill]) * sectors * SECTOR_SIZE)
    memory = bytearray(8 * SECTOR_SIZE)
    device = Storage(str(path))
    device._mem_read = lambda address: memory[address]
    device._mem_write = lambda address, value: memory.__setitem__(address, value)
    device._mem_span_valid = lambda address, count: (
        0 <= address <= len(memory)
        and 0 < count <= len(memory) - address
    )
    return device, memory, path


def _program(device: Storage, *, sector=0, dma=0, count=1) -> None:
    _write_le(device, 0x02, sector, 4)
    _write_le(device, 0x06, dma, 8)
    device.write8(0x0E, count)


def _guarded_command(device: Storage, command: int, generation: int) -> None:
    _write_le(device, 0x20, generation, 4)
    device.write8(0x24, command)


def test_extended_register_map_is_little_endian_and_capability_complete(tmp_path):
    device, _memory, _path = _attached_device(tmp_path)

    assert device.size == 0x25
    assert _read_le(device, 0x11, 4) == 4
    assert _read_le(device, 0x1A, 4) == 1
    assert device.read8(0x1E) == STORAGE_CAPS
    assert STORAGE_CAPS == (
        STORAGE_CAP_READ
        | STORAGE_CAP_WRITE
        | STORAGE_CAP_FLUSH
        | STORAGE_CAP_PRECISE_RESULT
        | STORAGE_CAP_COMPLETION
        | STORAGE_CAP_MEDIA_GEN
        | STORAGE_CAP_GEN_GUARD
    )

    _write_le(device, 0x20, 0x7856_3412, 4)
    assert _read_le(device, 0x20, 4) == 0x7856_3412
    assert device.read8(0x24) == 0

    device.completion = 0x7856_3412
    assert bytes(device.read8(offset) for offset in range(0x16, 0x1A)) == (
        b"\x12\x34\x56\x78"
    )
    assert device.read8(0x01) == (
        STORAGE_STATUS_PRESENT | STORAGE_STATUS_MEDIA_CHANGED
    )


def test_legacy_setup_data_and_dma_push_registers_retain_their_meanings(
    tmp_path,
):
    device, _memory, _path = _attached_device(tmp_path)
    _write_le(device, 0x02, 0x1234_5678, 4)
    _write_le(device, 0x06, 0x8877_6655_4433_2211, 8)
    device.write8(0x0E, 0)
    device.write8(0x0F, 0xA5)
    device.write8(0x0F, 0x5A)

    assert _read_le(device, 0x02, 4) == 0x1234_5678
    assert _read_le(device, 0x06, 8) == 0x8877_6655_4433_2211
    assert device.read8(0x0E) == 0
    assert device.read8(0x0F) == 0xA5
    assert device.read8(0x0F) == 0x5A

    device.dma_addr = 0
    device._dma_push_ctr = 0
    for value in (0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01):
        device.write8(0x10, value)
    assert device.dma_addr == 0x0102_0304_0506_0708


def test_media_generation_advances_once_per_successful_rebind(tmp_path):
    first = tmp_path / "first.img"
    second = tmp_path / "second.img"
    first.write_bytes(b"\x11" * SECTOR_SIZE)
    second.write_bytes(b"\x22" * SECTOR_SIZE)
    device = Storage(str(first))

    assert device.media_generation == 1
    device.load_image(str(first))
    assert device.media_generation == 2
    device.swap_image(str(second), save_current=False)
    assert device.media_generation == 3
    device.detach_image(save=False)
    assert device.media_generation == 4
    assert device.read8(0x01) & STORAGE_STATUS_MEDIA_CHANGED


def test_idle_media_change_preserves_setup_and_terminal_publication(tmp_path):
    first = tmp_path / "first.img"
    second = tmp_path / "second.img"
    first.write_bytes(b"\x11" * (2 * SECTOR_SIZE))
    second.write_bytes(b"\x22" * (3 * SECTOR_SIZE))
    device = Storage(str(first))
    device._mem_read = lambda _address: 0
    device._mem_write = lambda _address, _value: None
    device._mem_span_valid = lambda _address, _count: True

    device.write8(0x00, 0x7E)
    terminal = (device.result, device.completion, device.transferred)
    _program(device, sector=1, dma=0, count=1)
    device.write8(0x10, 0x34)
    _write_le(device, 0x20, 0x1234_5678, 4)

    device.swap_image(str(second), save_current=False)
    device.write8(0x10, 0x12)

    assert _read_le(device, 0x02, 4) == 1
    assert _read_le(device, 0x06, 8) == 0x1234
    assert device.read8(0x0E) == 1
    assert _read_le(device, 0x20, 4) == 0x1234_5678
    assert (device.result, device.completion, device.transferred) == terminal
    assert device.read8(0x01) & (
        STORAGE_STATUS_ERROR
        | STORAGE_STATUS_RESULT_VALID
        | STORAGE_STATUS_MEDIA_CHANGED
        | STORAGE_STATUS_PRESENT
    ) == (
        STORAGE_STATUS_ERROR
        | STORAGE_STATUS_RESULT_VALID
        | STORAGE_STATUS_MEDIA_CHANGED
        | STORAGE_STATUS_PRESENT
    )


def test_status_w1c_clears_only_sticky_bits(tmp_path):
    device, _memory, _path = _attached_device(tmp_path)
    device.rejected = True
    device.error = True
    device.result_valid = True
    device.write_protected = True

    device.write8(0x01, STORAGE_STATUS_REJECTED | STORAGE_STATUS_MEDIA_CHANGED)

    status = device.read8(0x01)
    assert not status & STORAGE_STATUS_REJECTED
    assert not status & STORAGE_STATUS_MEDIA_CHANGED
    assert status & STORAGE_STATUS_ERROR
    assert status & STORAGE_STATUS_RESULT_VALID
    assert status & STORAGE_STATUS_WRITE_PROTECTED
    assert status & STORAGE_STATUS_PRESENT


def test_successful_write_and_read_publish_one_completion_each(tmp_path):
    device, memory, _path = _attached_device(tmp_path, sectors=4)
    first = bytes((index * 3) & 0xFF for index in range(2 * SECTOR_SIZE))
    memory[32:32 + len(first)] = first
    _program(device, sector=1, dma=32, count=2)

    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.result == STORAGE_RESULT_OK
    assert device.completion == 1
    assert device.transferred == 2
    assert device.read8(0x01) & STORAGE_STATUS_RESULT_VALID
    assert not device.read8(0x01) & (STORAGE_STATUS_BUSY | STORAGE_STATUS_ERROR)
    assert bytes(device._image_data[SECTOR_SIZE:3 * SECTOR_SIZE]) == first

    memory[2048:2048 + len(first)] = b"\x00" * len(first)
    _program(device, sector=1, dma=2048, count=2)
    device.write8(0x00, STORAGE_CMD_READ)

    assert device.result == STORAGE_RESULT_OK
    assert device.completion == 2
    assert device.transferred == 2
    assert memory[2048:2048 + len(first)] == first


def test_guarded_commands_succeed_only_for_the_bound_generation(tmp_path):
    device, memory, path = _attached_device(tmp_path, sectors=4)
    generation = device.media_generation
    payload = bytes((index * 7) & 0xFF for index in range(SECTOR_SIZE))
    memory[:SECTOR_SIZE] = payload
    _program(device, sector=1, dma=0, count=1)

    _guarded_command(device, STORAGE_CMD_WRITE, generation)

    assert device.result == STORAGE_RESULT_OK
    assert device.transferred == 1
    assert bytes(device._image_data[SECTOR_SIZE:2 * SECTOR_SIZE]) == payload

    memory[SECTOR_SIZE:2 * SECTOR_SIZE] = b"\x00" * SECTOR_SIZE
    _program(device, sector=1, dma=SECTOR_SIZE, count=1)
    _guarded_command(device, STORAGE_CMD_READ, generation)

    assert device.result == STORAGE_RESULT_OK
    assert device.transferred == 1
    assert memory[SECTOR_SIZE:2 * SECTOR_SIZE] == payload

    _guarded_command(device, STORAGE_CMD_FLUSH, generation)
    assert device.result == STORAGE_RESULT_OK
    assert device.transferred == 0
    assert path.read_bytes()[SECTOR_SIZE:2 * SECTOR_SIZE] == payload


@pytest.mark.parametrize(
    "command",
    (STORAGE_CMD_READ, STORAGE_CMD_WRITE, STORAGE_CMD_FLUSH),
)
def test_stale_guarded_command_has_no_dma_media_or_flush_effect(
    tmp_path, command
):
    device, memory, _old_path = _attached_device(
        tmp_path, sectors=2, fill=0x31
    )
    memory[:] = b"\xA7" * len(memory)
    original_memory = bytes(memory)
    dma_reads = []
    dma_writes = []
    device._mem_read = (
        lambda address: dma_reads.append(address) or memory[address]
    )
    device._mem_write = (
        lambda address, value: dma_writes.append((address, value))
        or memory.__setitem__(address, value)
    )
    flush_calls = []
    original_save = device.save_image

    def record_flush():
        flush_calls.append(True)
        original_save()

    device.save_image = record_flush
    _program(device, sector=0, dma=0, count=1)
    captured_generation = device.media_generation
    _write_le(device, 0x20, captured_generation, 4)

    replacement_path = tmp_path / "replacement.img"
    replacement_path.write_bytes(b"\x42" * (2 * SECTOR_SIZE))
    device.swap_image(str(replacement_path), save_current=False)
    original_media = bytes(device._image_data)
    original_file = replacement_path.read_bytes()

    device.write8(0x24, command)

    assert device.result == STORAGE_RESULT_MEDIA_REMOVED
    assert device.transferred == 0
    assert device.completion == 1
    assert not device.busy
    assert bytes(memory) == original_memory
    assert dma_reads == []
    assert dma_writes == []
    assert bytes(device._image_data) == original_media
    assert replacement_path.read_bytes() == original_file
    assert flush_calls == []


def test_status_noop_idle_reset_and_unsupported_completion(tmp_path):
    device, _memory, _path = _attached_device(tmp_path)
    device.completion = 0xFFFF_FFFF

    before = (device.read8(0x01), device.result, device.completion)
    device.write8(0x00, STORAGE_CMD_STATUS)
    assert (device.read8(0x01), device.result, device.completion) == before

    device.write8(0x00, 0x7E)
    assert device.result == STORAGE_RESULT_UNSUPPORTED
    assert device.completion == 0
    assert device.read8(0x01) & (
        STORAGE_STATUS_ERROR | STORAGE_STATUS_RESULT_VALID
    ) == (STORAGE_STATUS_ERROR | STORAGE_STATUS_RESULT_VALID)

    device.write8(0x00, STORAGE_CMD_RESET)
    assert device.completion == 0
    assert device.result == STORAGE_RESULT_OK
    assert not device.read8(0x01) & (
        STORAGE_STATUS_ERROR | STORAGE_STATUS_RESULT_VALID
    )


def test_zero_count_is_not_coerced_and_fails_before_dma(tmp_path):
    device, _memory, _path = _attached_device(tmp_path)
    reads = []
    original = bytes(device._image_data)
    device._mem_read = lambda address: reads.append(address) or 0
    _program(device, count=0)

    assert device.read8(0x0E) == 0
    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.result == STORAGE_RESULT_INVALID_COUNT
    assert device.completion == 1
    assert device.transferred == 0
    assert reads == []
    assert bytes(device._image_data) == original


@pytest.mark.parametrize(
    ("mutate", "expected"),
    (
        (lambda d: setattr(d, "sector_num", d.total_sectors),
         STORAGE_RESULT_LBA_RANGE),
        (lambda d: setattr(d, "dma_addr", (1 << 64) - SECTOR_SIZE + 1),
         STORAGE_RESULT_ADDRESS_OVERFLOW),
        (lambda d: setattr(d, "_mem_span_valid", lambda address, count: False),
         STORAGE_RESULT_DMA_INVALID),
        (lambda d: setattr(d, "write_protected", True),
         STORAGE_RESULT_WRITE_PROTECTED),
    ),
)
def test_preflight_failures_do_not_read_memory_or_mutate_media(
    tmp_path, mutate, expected
):
    device, _memory, _path = _attached_device(tmp_path)
    reads = []
    original = bytes(device._image_data)
    device._mem_read = lambda address: reads.append(address) or 0xA5
    _program(device)
    mutate(device)

    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.result == expected
    assert device.completion == 1
    assert device.transferred == 0
    assert reads == []
    assert bytes(device._image_data) == original


def test_no_media_is_distinct_from_invalid_geometry():
    device = Storage()
    device.sec_count = 0

    device.write8(0x00, STORAGE_CMD_READ)

    assert device.result == STORAGE_RESULT_NO_MEDIA
    assert device.completion == 1
    assert device.read8(0x01) == (
        STORAGE_STATUS_ERROR | STORAGE_STATUS_RESULT_VALID
    )


def test_stalled_command_owns_immutable_snapshot_and_rejects_busy_writes(tmp_path):
    device, memory, _path = _attached_device(tmp_path, fill=0x5A)
    device.write8(0x01, STORAGE_STATUS_MEDIA_CHANGED)
    _program(device, sector=1, dma=64, count=1)
    _write_le(device, 0x20, 0x1234_5678, 4)
    device.inject_fault("start", command=STORAGE_CMD_READ, action="stall")

    device.write8(0x00, STORAGE_CMD_READ)
    snapshot = device.active_request
    assert snapshot == (STORAGE_CMD_READ, 1, 64, 1, device.media_generation)
    assert device.stalled
    assert device.read8(0x01) == (STORAGE_STATUS_PRESENT | STORAGE_STATUS_BUSY)

    for offset in range(0x02, 0x11):
        device.write8(offset, 0xA5)
    for offset in range(0x20, 0x24):
        device.write8(offset, 0x5A)
    device.write8(0x24, STORAGE_CMD_WRITE)
    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.active_request == snapshot
    assert device.completion == 0
    assert device.read8(0x01) & STORAGE_STATUS_REJECTED
    assert device.read8(0x02) == 1
    assert device.read8(0x06) == 64
    assert device.read8(0x0E) == 1
    assert _read_le(device, 0x20, 4) == 0x1234_5678

    # STATUS W1C remains meaningful during a command.  Writes to the read-only
    # result block are harmless and do not create a fresh rejection.
    device.write8(0x01, STORAGE_STATUS_REJECTED)
    assert not device.read8(0x01) & STORAGE_STATUS_REJECTED
    for offset in range(0x11, 0x20):
        device.write8(offset, 0x5A)
    assert not device.read8(0x01) & STORAGE_STATUS_REJECTED
    device.write8(0x02, 0xEE)
    assert device.read8(0x01) & STORAGE_STATUS_REJECTED

    assert device.release_stall()
    assert memory[64:64 + SECTOR_SIZE] == b"\x5A" * SECTOR_SIZE
    assert device.completion == 1
    assert device.result == STORAGE_RESULT_OK
    assert device.transferred == 1
    assert device.read8(0x01) & STORAGE_STATUS_REJECTED

    device.write8(0x01, STORAGE_STATUS_REJECTED)
    assert not device.read8(0x01) & STORAGE_STATUS_REJECTED


def test_reset_aborts_stalled_command_once_without_side_effects(tmp_path):
    device, memory, _path = _attached_device(tmp_path)
    memory[:SECTOR_SIZE] = b"\xC3" * SECTOR_SIZE
    original = bytes(device._image_data)
    _program(device)
    _write_le(device, 0x20, 0xCAFE_BABE, 4)
    device.inject_fault("accept", command=STORAGE_CMD_WRITE, action="stall")
    device.write8(0x00, STORAGE_CMD_WRITE)

    device.write8(0x00, STORAGE_CMD_RESET)

    assert not device.busy
    assert not device.stalled
    assert device.active_request is None
    assert device.completion == 1
    assert device.result == STORAGE_RESULT_RESET_ABORTED
    assert device.transferred == 0
    assert bytes(device._image_data) == original
    assert device.expected_media_generation == 0
    assert not device.release_stall()


def test_detach_aborts_active_request_and_cannot_redirect_to_new_media(tmp_path):
    old_path = tmp_path / "old.img"
    new_path = tmp_path / "new.img"
    old_path.write_bytes(b"\x11" * SECTOR_SIZE)
    new_path.write_bytes(b"\x22" * SECTOR_SIZE)
    memory = bytearray(b"\xCC" * SECTOR_SIZE)
    device = Storage(str(old_path))
    device._mem_read = lambda address: memory[address]
    device._mem_write = lambda address, value: memory.__setitem__(address, value)
    device._mem_span_valid = lambda address, count: address == 0 and count == 512
    _program(device)
    initial_generation = device.media_generation
    device.inject_fault("start", command=STORAGE_CMD_READ, action="stall")
    device.write8(0x00, STORAGE_CMD_READ)

    device.detach_image(save=False)

    assert device.completion == 1
    assert device.result == STORAGE_RESULT_MEDIA_REMOVED
    assert device.media_generation == initial_generation + 1
    assert not device.read8(0x01) & STORAGE_STATUS_PRESENT
    assert memory == b"\xCC" * SECTOR_SIZE

    device.load_image(str(new_path))
    assert device.media_generation == initial_generation + 2
    assert memory == b"\xCC" * SECTOR_SIZE
    assert not device.release_stall()


def test_read_dma_fault_reports_exact_completed_sector_and_prefix(tmp_path):
    device, memory, _path = _attached_device(tmp_path, sectors=2)
    first = bytes(index & 0xFF for index in range(SECTOR_SIZE))
    second = bytes((index + 17) & 0xFF for index in range(SECTOR_SIZE))
    device._image_data[:] = first + second
    memory[:2 * SECTOR_SIZE] = b"\xCC" * (2 * SECTOR_SIZE)
    _program(device, count=2)
    device.inject_fault(
        "dma",
        STORAGE_RESULT_DMA_FAILURE,
        command=STORAGE_CMD_READ,
        sector_index=1,
        byte_index=10,
    )

    device.write8(0x00, STORAGE_CMD_READ)

    assert device.result == (STORAGE_RESULT_PARTIAL | STORAGE_RESULT_DMA_FAILURE)
    assert device.completion == 1
    assert device.transferred == 1
    assert memory[:SECTOR_SIZE] == first
    assert memory[SECTOR_SIZE:SECTOR_SIZE + 10] == second[:10]
    assert memory[SECTOR_SIZE + 10:2 * SECTOR_SIZE] == (
        b"\xCC" * (SECTOR_SIZE - 10)
    )


def test_write_media_fault_reports_exact_completed_sector_and_prefix(tmp_path):
    device, memory, _path = _attached_device(tmp_path, sectors=2)
    first = b"\xA1" * SECTOR_SIZE
    second = b"\xB2" * SECTOR_SIZE
    memory[:2 * SECTOR_SIZE] = first + second
    _program(device, count=2)
    device.inject_fault(
        "media",
        STORAGE_RESULT_MEDIA_FAILURE,
        command=STORAGE_CMD_WRITE,
        sector_index=1,
        byte_index=10,
    )

    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.result == (
        STORAGE_RESULT_PARTIAL | STORAGE_RESULT_MEDIA_FAILURE
    )
    assert device.completion == 1
    assert device.transferred == 1
    assert device._image_data[:SECTOR_SIZE] == first
    assert device._image_data[SECTOR_SIZE:SECTOR_SIZE + 10] == second[:10]
    assert device._image_data[SECTOR_SIZE + 10:] == (
        b"\x00" * (SECTOR_SIZE - 10)
    )


def test_write_completion_fault_preserves_applied_count_and_uncertainty(tmp_path):
    device, memory, _path = _attached_device(tmp_path)
    memory[:SECTOR_SIZE] = b"\xD4" * SECTOR_SIZE
    _program(device)
    device.inject_fault(
        "write_complete",
        STORAGE_RESULT_MEDIA_FAILURE,
        command=STORAGE_CMD_WRITE,
    )

    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device._image_data[:SECTOR_SIZE] == b"\xD4" * SECTOR_SIZE
    assert device.result == (
        STORAGE_RESULT_PARTIAL | STORAGE_RESULT_MEDIA_FAILURE
    )
    assert device.transferred == 1
    assert device.completion == 1


def test_start_timeout_is_clean_and_one_shot(tmp_path):
    device, memory, _path = _attached_device(tmp_path, fill=0x44)
    memory[:] = b"\x99" * len(memory)
    _program(device)
    device.inject_fault(
        "start", STORAGE_RESULT_TIMEOUT, command=STORAGE_CMD_READ
    )

    device.write8(0x00, STORAGE_CMD_READ)
    assert device.result == STORAGE_RESULT_TIMEOUT
    assert device.transferred == 0
    assert memory == b"\x99" * len(memory)

    device.write8(0x00, STORAGE_CMD_READ)
    assert device.result == STORAGE_RESULT_OK
    assert device.completion == 2
    assert memory[:SECTOR_SIZE] == b"\x44" * SECTOR_SIZE


def test_flush_is_the_explicit_durability_boundary(tmp_path, monkeypatch):
    device, memory, path = _attached_device(tmp_path)
    memory[:SECTOR_SIZE] = b"\xE5" * SECTOR_SIZE
    _program(device)
    device.write8(0x00, STORAGE_CMD_WRITE)

    assert path.read_bytes()[:SECTOR_SIZE] == b"\x00" * SECTOR_SIZE
    calls = []
    monkeypatch.setattr("devices.os.fsync", lambda fd: calls.append(fd))

    device.write8(0x00, STORAGE_CMD_FLUSH)

    assert device.result == STORAGE_RESULT_OK
    assert device.completion == 2
    assert device.transferred == 0
    assert path.read_bytes()[:SECTOR_SIZE] == b"\xE5" * SECTOR_SIZE
    assert len(calls) == 1


def test_flush_faults_are_visible_and_host_oserror_is_contained(
    tmp_path, monkeypatch
):
    device, memory, path = _attached_device(tmp_path)
    memory[:SECTOR_SIZE] = b"\xF6" * SECTOR_SIZE
    _program(device)
    device.write8(0x00, STORAGE_CMD_WRITE)
    durable_before = path.read_bytes()

    device.inject_fault(
        "flush", STORAGE_RESULT_FLUSH_FAILURE, command=STORAGE_CMD_FLUSH
    )
    device.write8(0x00, STORAGE_CMD_FLUSH)
    assert device.result == STORAGE_RESULT_FLUSH_FAILURE
    assert path.read_bytes() == durable_before

    def fail_fsync(_fd):
        raise OSError("host unavailable")

    monkeypatch.setattr("devices.os.fsync", fail_fsync)
    device.write8(0x00, STORAGE_CMD_FLUSH)
    assert device.result == (
        STORAGE_RESULT_PARTIAL | STORAGE_RESULT_FLUSH_FAILURE
    )
    assert device.completion == 3

    with pytest.raises(OSError, match="host unavailable"):
        device.save_image()


def test_guest_flush_survives_process_exit_without_session_close(tmp_path):
    path = tmp_path / "cold.img"
    path.write_bytes(b"\x00" * SECTOR_SIZE)
    writer = r"""
import os
import sys
from devices import Storage, STORAGE_CMD_FLUSH, STORAGE_CMD_WRITE

path = sys.argv[1]
memory = bytearray(b'Z' * 512)
device = Storage(path)
device._mem_read = lambda address: memory[address]
device._mem_write = lambda address, value: memory.__setitem__(address, value)
device._mem_span_valid = lambda address, count: address == 0 and count == 512
device.sector_num = 0
device.dma_addr = 0
device.sec_count = 1
device.write8(0, STORAGE_CMD_WRITE)
assert device.result == 0
device.write8(0, STORAGE_CMD_FLUSH)
assert device.result == 0
os._exit(0)
"""
    reader = r"""
import pathlib
import sys
sys.stdout.buffer.write(pathlib.Path(sys.argv[1]).read_bytes()[:512])
"""

    subprocess.run([sys.executable, "-c", writer, str(path)], check=True)
    cold = subprocess.run(
        [sys.executable, "-c", reader, str(path)],
        check=True,
        stdout=subprocess.PIPE,
    )

    assert cold.stdout == b"Z" * SECTOR_SIZE


def test_system_storage_dma_uses_real_physical_window_boundaries(tmp_path):
    path = tmp_path / "system.img"
    path.write_bytes(b"\x7B" * SECTOR_SIZE)
    system = MegapadSystem(
        ram_size=4096,
        storage_image=str(path),
        hbw_size=4096,
        ext_mem_size=4096,
        vram_size=4096,
    )
    device = system.storage
    valid = (
        0,
        system.ram_size - SECTOR_SIZE,
        EXT_MEM_BASE,
        EXT_MEM_BASE + 4096 - SECTOR_SIZE,
        VRAM_BASE,
        VRAM_BASE + 4096 - SECTOR_SIZE,
        HBW_BASE,
        HBW_BASE + 4096 - SECTOR_SIZE,
    )
    invalid = (
        system.ram_size - SECTOR_SIZE + 1,
        EXT_MEM_BASE + 4096 - SECTOR_SIZE + 1,
        VRAM_BASE + 4096 - SECTOR_SIZE + 1,
        HBW_BASE + 4096 - SECTOR_SIZE + 1,
        MMIO_START,
    )

    for address in valid:
        _program(device, dma=address)
        device.write8(0x00, STORAGE_CMD_READ)
        assert device.result == STORAGE_RESULT_OK

    for address in invalid:
        _program(device, dma=address)
        device.write8(0x00, STORAGE_CMD_READ)
        assert device.result == STORAGE_RESULT_DMA_INVALID

    _program(device, dma=(1 << 64) - SECTOR_SIZE)
    device.write8(0x00, STORAGE_CMD_READ)
    assert device.result == STORAGE_RESULT_DMA_INVALID

    _program(device, dma=(1 << 64) - SECTOR_SIZE + 1)
    device.write8(0x00, STORAGE_CMD_READ)
    assert device.result == STORAGE_RESULT_ADDRESS_OVERFLOW


def test_configured_hbw_dma_does_not_alias_bank_zero(tmp_path):
    """Validation and routing use the same configured HBW boundary."""
    path = tmp_path / "expanded-hbw.img"
    path.write_bytes(b"\x7B" * SECTOR_SIZE)
    system = MegapadSystem(
        ram_size=4096,
        storage_image=str(path),
        hbw_size=HBW_SIZE + SECTOR_SIZE,
        ext_mem_size=0,
        vram_size=0,
    )
    device = system.storage
    extra_hbw_sector = HBW_BASE + HBW_SIZE
    system._shared_mem[:SECTOR_SIZE] = b"\xA5" * SECTOR_SIZE

    _program(device, dma=extra_hbw_sector)
    device.write8(0x00, STORAGE_CMD_READ)

    assert device.result == STORAGE_RESULT_OK
    assert system._hbw_mem[HBW_SIZE:HBW_SIZE + SECTOR_SIZE] == (
        b"\x7B" * SECTOR_SIZE
    )
    assert system._shared_mem[:SECTOR_SIZE] == b"\xA5" * SECTOR_SIZE

    system._hbw_mem[HBW_SIZE:HBW_SIZE + SECTOR_SIZE] = (
        b"\xC4" * SECTOR_SIZE
    )
    _program(device, dma=extra_hbw_sector)
    device.write8(0x00, STORAGE_CMD_WRITE)

    assert device.result == STORAGE_RESULT_OK
    assert device._image_data[:SECTOR_SIZE] == b"\xC4" * SECTOR_SIZE
    assert system._shared_mem[:SECTOR_SIZE] == b"\xA5" * SECTOR_SIZE


def test_system_cold_boot_resets_controller_but_preserves_media_identity(tmp_path):
    path = tmp_path / "boot.img"
    path.write_bytes(b"\x33" * SECTOR_SIZE)
    system = MegapadSystem(ram_size=4096, storage_image=str(path))
    device = system.storage
    generation = device.media_generation
    _program(device)
    device.inject_fault("start", command=STORAGE_CMD_READ, action="stall")
    device.write8(0x00, STORAGE_CMD_READ)
    assert device.busy

    system.boot()

    assert device.media_generation == generation
    assert device.read8(0x01) == STORAGE_STATUS_PRESENT
    assert device.completion == 0
    assert device.result == STORAGE_RESULT_OK
    assert device.active_request is None
    assert not device.stalled
