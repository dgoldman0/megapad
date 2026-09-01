"""Unchanged-source acceptance for the MP64FS descriptor pool lifecycle."""

from __future__ import annotations

import hashlib
import struct
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.storage import (
    STORAGE_CAPS,
    STORAGE_CAP_FLUSH,
    STORAGE_RESULT_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import (
    _entry_offset,
    _formatted_image,
    _write_entry,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_buffer_io import (
    _load_mp64fs_buffer_io,
    _load_mp64fs_buffer_io_service,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _cache_addresses,
    _diagnostics,
    _mount_snapshot,
    _store,
)
from tests.simulator.test_kdos_mp64fs_mutation import _mount
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-mp64fs-fd-pool-5515-5610.f"
)

FIRST_LINE = 5515
LAST_LINE = 5610
SLICE_SHA256 = (
    "16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78"
)
SLICE_GIT_BLOB = "e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9"

DEFINITIONS = (
    b"FD-MAX",
    b"FD-SLOT-SZ",
    b"FD-POOL",
    b"FD-SLOT",
    b"FD-ALLOC",
    b"(FCLOSE-NOFS)",
    b"FCLOSE",
    b"FD-FILL",
    b"OP-SLOT",
    b"(OPEN)",
    b"OPEN",
    b"F.SLOT",
    b"FFLUSH",
    b"(FCLOSE)",
)

FD_MAX = 16
FD_SLOT_SIZE = 72
FD_POOL_SIZE = FD_MAX * FD_SLOT_SIZE


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 3_397
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == (
        b'    ."  Loaded " LB-SLOT @ DIRENT 28 + L@ . '
        b'."  bytes from " NAMEBUF .ZSTR CR ;\n'
    )
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == (
        "\\ ── LOAD — load and execute a Forth source file "
        "─────────────────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_fd_pool(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_fd_pool(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_fd_pool(_load_mp64fs_buffer_io(image))


def _load_mp64fs_fd_pool_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_fd_pool(
        _load_mp64fs_buffer_io_service(storage)
    )


def _fd_snapshot(runtime: MegaForthRuntime, descriptor: int) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(descriptor + offset)
        for offset in range(0, 64, 8)
    )


def _open(runtime: MegaForthRuntime, name: str) -> int:
    context = runtime.new_context()
    runtime.evaluate(
        f"OPEN {name}".encode("ascii"),
        source_name=f"open-{name}",
        context=context,
    )
    values = context.data.snapshot()
    assert len(values) == 1
    assert context.returns.snapshot() == ()
    return values[0]


def test_fd_pool_slice_is_exact_and_installs_final_deferred_bindings() -> None:
    runtime = _load_mp64fs_buffer_io(_formatted_image())
    runtime.rtc.set_epoch_ms(0x0102_0304_0506_0708)
    epoch_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    mount_before = _mount_snapshot(runtime)

    _evaluate_mp64fs_fd_pool(runtime)

    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _constant(runtime, "FD-MAX") == FD_MAX
    assert _constant(runtime, "FD-SLOT-SZ") == FD_SLOT_SIZE
    pool = _execute(runtime, "FD-POOL")[0]
    assert runtime.memory.read_bytes(pool, FD_POOL_SIZE) == bytes(FD_POOL_SIZE)
    assert _execute(runtime, "FD-SLOT", 0) == (pool,)
    assert _execute(runtime, "FD-SLOT", FD_MAX - 1) == (
        pool + (FD_MAX - 1) * FD_SLOT_SIZE,
    )
    assert _variable(runtime, "OP-SLOT") == 0

    open_word = runtime.find("OPEN")
    open_target = runtime.find("(OPEN)")
    close_word = runtime.find("FCLOSE")
    close_target = runtime.find("(FCLOSE)")
    close_nofs = runtime.find("(FCLOSE-NOFS)")
    assert open_word is not None
    assert open_target is not None
    assert close_word is not None
    assert close_target is not None
    assert close_nofs is not None
    assert runtime.memory.read64(open_word.body_address) == open_target.xt
    assert runtime.memory.read64(close_word.body_address) == close_target.xt
    assert runtime.memory.read64(close_word.body_address) != close_nofs.xt
    assert sum(word.name == b"OPEN" for word in runtime.dictionary.words) == 1
    assert sum(word.name == b"FCLOSE" for word in runtime.dictionary.words) == 1

    assert _mount_snapshot(runtime) == mount_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == epoch_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_fd_pool_allocates_lowest_slots_and_reuses_without_clearing() -> None:
    runtime = _load_mp64fs_fd_pool()
    pool = _execute(runtime, "FD-POOL")[0]

    descriptors = [_execute(runtime, "FD-ALLOC")[0] for _ in range(FD_MAX)]
    assert descriptors == [
        pool + index * FD_SLOT_SIZE + 8 for index in range(FD_MAX)
    ]
    assert _execute(runtime, "FD-ALLOC") == (0,)
    assert tuple(
        runtime.memory.read64(pool + index * FD_SLOT_SIZE)
        for index in range(FD_MAX)
    ) == (MASK64,) * FD_MAX

    released = descriptors[5]
    for offset, value in enumerate(range(0x101, 0x109, 1)):
        runtime.memory.write64(released + offset * 8, value)
    payload_before = _fd_snapshot(runtime, released)
    assert _execute(runtime, "(FCLOSE-NOFS)", released) == ()
    assert runtime.memory.read64(released - 8) == 0
    assert _fd_snapshot(runtime, released) == payload_before

    assert _execute(runtime, "FD-ALLOC") == (released,)
    assert runtime.memory.read64(released - 8) == MASK64
    assert _fd_snapshot(runtime, released) == payload_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""


def test_open_populates_descriptors_and_reports_miss_and_exhaustion() -> None:
    image = _formatted_image(18)
    _write_entry(
        image,
        6,
        name=b"openme\0",
        start=14,
        count=2,
        used=700,
        entry_type=2,
        secondary_start=16,
        secondary_count=1,
    )
    runtime = _load_mp64fs_fd_pool(image)
    _mount(runtime)
    pool = _execute(runtime, "FD-POOL")[0]
    sentinel = 0xA1A2_A3A4_A5A6_A7A8
    runtime.memory.write64(pool + 64, sentinel)
    completion = runtime.storage.completion
    mount_before = _mount_snapshot(runtime)

    first = _open(runtime, "openme")
    assert first == pool + 8
    assert _fd_snapshot(runtime, first) == (
        14,
        2,
        700,
        0,
        6,
        16,
        1,
        sentinel,
    )
    assert runtime.memory.read64(pool) == MASK64
    assert _variable(runtime, "OP-SLOT") == 6
    assert runtime.drain_uart_output() == b""

    headers_before_miss = runtime.memory.read_bytes(pool, FD_POOL_SIZE)
    assert _open(runtime, "missing") == 0
    assert runtime.drain_uart_output() == b" Not found: missing\r\n"
    assert _variable(runtime, "OP-SLOT") == MASK64
    assert runtime.memory.read_bytes(pool, FD_POOL_SIZE) == headers_before_miss

    descriptors = [first]
    descriptors.extend(_open(runtime, "openme") for _ in range(FD_MAX - 1))
    assert descriptors == [
        pool + index * FD_SLOT_SIZE + 8 for index in range(FD_MAX)
    ]
    assert runtime.drain_uart_output() == b""
    assert _open(runtime, "openme") == 0
    assert runtime.drain_uart_output() == b" No free FD slots\r\n"
    assert _variable(runtime, "OP-SLOT") == 6
    assert all(
        runtime.memory.read64(pool + index * FD_SLOT_SIZE) == MASK64
        for index in range(FD_MAX)
    )
    assert runtime.storage.completion == completion
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_open_and_flush_guards_precede_parse_or_descriptor_access() -> None:
    runtime = _load_mp64fs_fd_pool()
    pool = _execute(runtime, "FD-POOL")[0]
    _store(runtime, "OP-SLOT", 0x777)

    runtime.evaluate(b"OPEN 41", source_name="open-no-filesystem")
    assert runtime.main_context.data.snapshot() == (0, 41)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    runtime.main_context.data.clear()
    assert _variable(runtime, "OP-SLOT") == 0x777
    assert runtime.memory.read_bytes(pool, FD_POOL_SIZE) == bytes(FD_POOL_SIZE)

    assert _execute(runtime, "FFLUSH", 123) == ()
    assert runtime.drain_uart_output() == b" FS not loaded\r\n"

    descriptor = _execute(runtime, "FD-ALLOC")[0]
    assert descriptor == pool + 8
    for offset, value in enumerate(range(0x201, 0x209, 1)):
        runtime.memory.write64(descriptor + offset * 8, value)
    payload_before = _fd_snapshot(runtime, descriptor)
    assert _execute(runtime, "FCLOSE", 0) == ()
    assert runtime.memory.read64(descriptor - 8) == MASK64
    assert _execute(runtime, "FCLOSE", descriptor) == ()
    assert runtime.memory.read64(descriptor - 8) == 0
    assert _fd_snapshot(runtime, descriptor) == payload_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_fflush_and_fclose_persist_used_then_reuse_or_discard() -> None:
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        4,
        name=b"record\0",
        start=14,
        count=1,
        used=3,
        entry_type=5,
    )
    struct.pack_into("<II", image, entry_offset + 36, 0x1020_3040, 0x5060_7080)
    runtime = _load_mp64fs_fd_pool(image)
    _mount(runtime)
    pool = _execute(runtime, "FD-POOL")[0]
    sentinel = 0xB1B2_B3B4_B5B6_B7B8
    runtime.memory.write64(pool + 64, sentinel)
    descriptor = _open(runtime, "record")
    assert descriptor == pool + 8
    assert _fd_snapshot(runtime, descriptor) == (
        14,
        1,
        3,
        0,
        4,
        0,
        0,
        sentinel,
    )
    _superblock, _bitmap, directory = _cache_addresses(runtime)

    used_with_high_bits = 0xAABB_CCDD_0000_0141
    runtime.memory.write64(descriptor + 16, used_with_high_bits)
    entry_before = runtime.memory.read_bytes(directory + 4 * 48, 48)
    media_before = runtime.storage.image_bytes
    completion = runtime.storage.completion
    assert _execute(runtime, "FFLUSH", descriptor) == ()

    assert runtime.storage.completion == completion + 3
    assert _diagnostics(runtime) == (0, 12, 0)
    expected_entry = bytearray(entry_before)
    struct.pack_into("<I", expected_entry, 28, 321)
    assert runtime.memory.read_bytes(directory + 4 * 48, 48) == (
        bytes(expected_entry)
    )
    expected_media = bytearray(media_before)
    expected_media[entry_offset : entry_offset + 48] = expected_entry
    assert runtime.storage.image_bytes == bytes(expected_media)
    assert runtime.memory.read64(descriptor - 8) == MASK64
    assert _fd_snapshot(runtime, descriptor)[2] == used_with_high_bits
    assert runtime.drain_uart_output() == b""

    runtime.memory.write64(descriptor + 16, 400)
    completion = runtime.storage.completion
    assert _execute(runtime, "FCLOSE", 0) == ()
    assert runtime.storage.completion == completion
    assert _execute(runtime, "FCLOSE", descriptor) == ()
    assert runtime.storage.completion == completion + 3
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.memory.read64(descriptor - 8) == 0
    retained = _fd_snapshot(runtime, descriptor)
    assert retained[2] == 400
    assert retained[7] == sentinel
    assert struct.unpack_from(
        "<I", runtime.storage.image_bytes, entry_offset + 28
    )[0] == 400

    reopened = _open(runtime, "record")
    assert reopened == descriptor
    assert _fd_snapshot(runtime, reopened) == (
        14,
        1,
        400,
        0,
        4,
        0,
        0,
        sentinel,
    )
    assert runtime.storage.completion == completion + 3
    runtime.memory.write64(reopened + 16, 450)
    cache_before_discard = runtime.memory.read_bytes(directory, 12 * 512)
    media_before_discard = runtime.storage.image_bytes
    completion = runtime.storage.completion
    _store(runtime, "FS-OK", 0)

    assert _execute(runtime, "FCLOSE", reopened) == ()

    assert runtime.memory.read64(reopened - 8) == 0
    assert _fd_snapshot(runtime, reopened)[2] == 450
    assert runtime.memory.read_bytes(directory, 12 * 512) == cache_before_discard
    assert runtime.storage.image_bytes == media_before_discard
    assert runtime.storage.completion == completion
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_fclose_flush_failure_retains_allocated_descriptor_and_metadata() -> None:
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        5,
        name=b"late\0",
        start=14,
        count=1,
        used=3,
        entry_type=5,
    )
    struct.pack_into("<II", image, entry_offset + 36, 0x1122_3344, 0x5566_7788)
    storage = HostedStorageService(
        image,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_mp64fs_fd_pool_service(storage)
    _mount(runtime)
    pool = _execute(runtime, "FD-POOL")[0]
    descriptor = _open(runtime, "late")
    assert descriptor == pool + 8
    runtime.memory.write64(descriptor + 16, 444)
    _superblock, _bitmap, directory = _cache_addresses(runtime)
    entry_before = runtime.memory.read_bytes(directory + 5 * 48, 48)
    media_before = storage.image_bytes
    completion = storage.completion
    descriptor_before = _fd_snapshot(runtime, descriptor)
    context = runtime.new_context()
    context.data.push(descriptor)

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("FCLOSE", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert storage.completion == completion + 2
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.memory.read64(descriptor - 8) == MASK64
    assert _fd_snapshot(runtime, descriptor) == descriptor_before

    expected_entry = bytearray(entry_before)
    struct.pack_into("<I", expected_entry, 28, 444)
    assert runtime.memory.read_bytes(directory + 5 * 48, 48) == (
        bytes(expected_entry)
    )
    expected_media = bytearray(media_before)
    expected_media[entry_offset : entry_offset + 48] = expected_entry
    assert storage.image_bytes == bytes(expected_media)
    assert runtime.spinlocks.owner(2) is None
