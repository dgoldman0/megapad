"""Unchanged-source acceptance for KDOS block devices and bounded volumes."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_GEN_GUARD,
    STORAGE_CMD_WRITE,
    STORAGE_RESULT_MEDIA_REMOVED,
)
from simulator.errors import SourceError
from simulator.memory import MMIO_BASE
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_kernels_pipelines import _load_kernels_pipelines
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-storage-block-volume-3755-4099.f"
)

FIRST_LINE = 3755
LAST_LINE = 4099
SLICE_SHA256 = (
    "e4d09d0801838fc9721ba68e39f2c5a5dbc139101c9c4a3489fb66cab9b248b1"
)
SLICE_GIT_BLOB = "efc0e23d5bab7ad71c101b65948b9e5e4ec2f8ad"
NEXT_SEAM_SHA256 = (
    "cfd4036c01d32a5dc4e7434651b8d434b467c812899231e2b691ed40e5e30a7b"
)
NEXT_SEAM_GIT_BLOB = "c687d9b0f2bf319b960774ed737b44b805a84f24"

DEFINITIONS = (
    b"SECTOR",
    b"DISK?",
    b"BLOCK-DEVICE-MAGIC",
    b"VOLUME-MAGIC",
    b"STORAGE-ABI",
    b"/BLOCK-DEVICE",
    b"/VOLUME",
    b"IOR-D-BLOCK",
    b"IOR-D-DEVICE",
    b"IOR-D-VOLUME",
    b"IOR-D-PARTITION",
    b"IOR-F-PARTIAL",
    b"IOR-F-RETRYABLE",
    b"IOR-F-STALE",
    b"IOR-F-CORRUPT",
    b"IOR-F-UNSUPPORTED",
    b"IOR-F-READONLY",
    b"IOR-C-BAD-DESCRIPTOR",
    b"IOR-C-STALE",
    b"IOR-C-RANGE",
    b"IOR-C-READONLY",
    b"IOR-C-CORRUPT",
    b"IOR-C-CAPACITY",
    b"IOR-C-WORKSPACE",
    b"IOR-C-UNSUPPORTED",
    b"IOR-C-BUSY",
    b"IOR-MAKE",
    b"IOR>RAW",
    b"IOR>CODE",
    b"IOR>DOMAIN",
    b"IOR>FLAGS",
    b"IOR-PARTIAL?",
    b"IOR-STALE?",
    b"BD-E-BAD-DESCRIPTOR",
    b"BD-E-NO-MEDIA",
    b"BD-E-UNSUPPORTED",
    b"BD-E-STALE",
    b"BD-E-INTERNAL",
    b"BD-E-RANGE",
    b"BD-E-READONLY",
    b"BD-E-BUSY",
    b"VOL-E-BAD-DESCRIPTOR",
    b"VOL-E-STALE",
    b"VOL-E-RANGE",
    b"VOL-E-READONLY",
    b"IOR-FROM-BLOCK-RESULT",
    b"BD.COOKIE",
    b"BD.MEDIA-GEN",
    b"BD.SECTOR-SIZE",
    b"BD.SECTORS",
    b"BD.CAPS",
    b"BD.FLAGS",
    b"BD.REFS",
    b"VOL.COOKIE",
    b"VOL.BD",
    b"VOL.BD-COOKIE",
    b"VOL.MEDIA-GEN",
    b"VOL.BASE",
    b"VOL.SECTORS",
    b"VOL.SECTOR-SIZE",
    b"VOL.FLAGS",
    b"VOL.SCHEME",
    b"VOL.INDEX",
    b"VOL-SCHEME-RAW",
    b"VOL-SCHEME-MBR",
    b"VOL-SCHEME-GPT",
    b"VOL-F-READONLY",
    b"BLOCK-RANGE?",
    b"STORAGE-COOKIE",
    b"STORAGE-COOKIE-NEXT",
    b"BD-REQUIRED-CAPS",
    b"BD-VALID?",
    b"BD-STALE?",
    b"BD-OPEN",
    b"BD-CLOSE",
    b"_BD-CHECK",
    b"BD-READ",
    b"BD-WRITE",
    b"BD-FLUSH",
    b"VOL-VALID?",
    b"VOL-STALE?",
    b"_VOL-CLEAR",
    b"_VOL-BD",
    b"_VOL-PTR",
    b"_VOL-BASE",
    b"_VOL-LEN",
    b"_VOL-SCHEME",
    b"_VOL-INDEX",
    b"VOL-SLICE",
    b"_VR-BD",
    b"_VR-VOL",
    b"VOL-RAW",
    b"VOL-CLOSE",
    b"_VOL-CHECK",
    b"VOL-READ",
    b"VOL-WRITE",
    b"VOL-FLUSH",
)

ZERO_VARIABLES = (
    b"STORAGE-COOKIE",
    b"_VOL-BD",
    b"_VOL-PTR",
    b"_VOL-BASE",
    b"_VOL-LEN",
    b"_VOL-SCHEME",
    b"_VOL-INDEX",
    b"_VR-BD",
    b"_VR-VOL",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 11_424
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"' p3-stats  pipe-thresh P.ADD\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_storage(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_storage(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_storage(_load_kernels_pipelines(runtime))


@pytest.fixture
def loaded_storage() -> MegaForthRuntime:
    return _load_storage()


def _variable(runtime: MegaForthRuntime, name: bytes | str) -> int:
    address = _execute(runtime, name)[0]
    return runtime.memory.read64(address)


def _constant(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return _execute(runtime, name)[0]


def _define_extent(
    runtime: MegaForthRuntime,
    name: str,
    size: int,
) -> int:
    runtime.evaluate(
        f"CREATE {name} {size} ALLOT".encode("ascii"),
        source_name=f"{name.lower()}-extent",
    )
    return _execute(runtime, name)[0]


def _cells(
    runtime: MegaForthRuntime,
    address: int,
    count: int,
) -> tuple[int, ...]:
    return tuple(runtime.memory.read64(address + index * 8) for index in range(count))


def _runtime_with_media(
    image: bytes,
    *,
    write_protected: bool = False,
    capabilities: int = STORAGE_CAPS,
) -> MegaForthRuntime:
    return _load_storage(
        MegaForthRuntime(
            storage=HostedStorageService(
                image,
                write_protected=write_protected,
                capabilities=capabilities,
            )
        )
    )


def test_storage_slice_is_exact_and_loads_without_touching_media(
    loaded_storage: MegaForthRuntime,
) -> None:
    runtime = loaded_storage
    assert len(DEFINITIONS) == 97
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert all(_variable(runtime, name) == 0 for name in ZERO_VARIABLES)
    assert _constant(runtime, "SECTOR") == SECTOR_SIZE
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""


def test_next_partition_source_stops_at_the_first_little_endian_fetch(
    loaded_storage: MegaForthRuntime,
) -> None:
    runtime = loaded_storage
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:4192])
    assert len(next_source) == 3_174
    assert next_source.count(b"\n") == 93
    assert hashlib.sha256(next_source).hexdigest() == NEXT_SEAM_SHA256
    assert _git_blob_id(next_source) == NEXT_SEAM_GIT_BLOB
    here_before = runtime.dictionary.here

    with pytest.raises(SourceError, match="unknown word b'L@'") as caught:
        runtime.evaluate(
            next_source,
            source_name=f"kdos.f@{MEGAPAD_REVISION}:4100-4192",
        )

    assert caught.value.location.line == 93
    assert caught.value.location.column == 37
    assert runtime.find("_MBR-TYPE") is runtime.dictionary.latest_word
    assert runtime.find("_MBR-BASE") is None
    assert runtime.find("PART-WORKSPACE-MIN") is not None
    assert runtime.dictionary.here > here_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_structured_ior_translation_splits_cause_and_partial_flag(
    loaded_storage: MegaForthRuntime,
) -> None:
    runtime = loaded_storage
    cases = (
        (0, 0),
        (4, 0x0001_0404),
        (7, 0x0201_0707),
        (10, 0x0201_0A0A),
        (11, 0x0401_0B0B),
        (0x87, 0x0301_0707),
        (0x8B, 0x0501_0B0B),
        (0x89, 0x0101_0909),
    )
    for status, expected in cases:
        assert _execute(runtime, "IOR-FROM-BLOCK-RESULT", status) == (expected,)
        assert _execute(runtime, "IOR>RAW", expected) == (expected & 0xFF,)
        assert _execute(runtime, "IOR>CODE", expected) == (
            (expected >> 8) & 0xFF,
        )
        assert _execute(runtime, "IOR>DOMAIN", expected) == (
            (expected >> 16) & 0xFF,
        )
        assert _execute(runtime, "IOR>FLAGS", expected) == (
            (expected >> 24) & 0xFF,
        )


def test_range_checks_and_cookie_wrap_keep_the_source_algorithms(
    loaded_storage: MegaForthRuntime,
) -> None:
    runtime = loaded_storage
    cases = (
        (0, 1, 1, TRUE),
        (1, 1, 2, TRUE),
        (2, 1, 2, 0),
        (0, 0, 10, 0),
        (0, 11, 10, 0),
        (MASK64 - 1, 1, MASK64, TRUE),
        (MASK64, 2, MASK64, 0),
    )
    for lba, count, length, expected in cases:
        assert _execute(runtime, "BLOCK-RANGE?", lba, count, length) == (
            expected,
        )

    assert _execute(runtime, "STORAGE-COOKIE-NEXT") == (1,)
    assert _execute(runtime, "STORAGE-COOKIE-NEXT") == (2,)
    cookie = _execute(runtime, "STORAGE-COOKIE")[0]
    runtime.memory.write64(cookie, MASK64)
    assert _execute(runtime, "STORAGE-COOKIE-NEXT") == (1,)


def test_block_open_failures_clear_the_target_and_keep_precise_precedence() -> None:
    absent = _load_storage()
    absent_bd = _define_extent(absent, "ABSENT-BD", 128)
    absent.memory.fill(absent_bd, 128, 0xA5)
    assert _execute(absent, "BD-OPEN", absent_bd) == (
        _constant(absent, "BD-E-NO-MEDIA"),
    )
    assert absent.memory.read_bytes(absent_bd, 128) == bytes(128)
    assert _variable(absent, "STORAGE-COOKIE") == 0

    unsupported = _runtime_with_media(
        bytes(SECTOR_SIZE),
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_GEN_GUARD,
    )
    unsupported_bd = _define_extent(unsupported, "UNSUPPORTED-BD", 128)
    unsupported.memory.fill(unsupported_bd, 128, 0xA5)
    assert _execute(unsupported, "BD-OPEN", unsupported_bd) == (
        _constant(unsupported, "BD-E-UNSUPPORTED"),
    )
    assert unsupported.memory.read_bytes(unsupported_bd, 128) == bytes(128)

    zero_capacity = _runtime_with_media(b"")
    empty_bd = _define_extent(zero_capacity, "EMPTY-BD", 128)
    zero_capacity.memory.fill(empty_bd, 128, 0xA5)
    assert _execute(zero_capacity, "BD-OPEN", empty_bd) == (
        _constant(zero_capacity, "BD-E-NO-MEDIA"),
    )
    assert zero_capacity.memory.read_bytes(empty_bd, 128) == bytes(128)
    assert _variable(zero_capacity, "STORAGE-COOKIE") == 1


def test_block_and_raw_volume_lifecycle_publish_fields_and_reference_counts() -> None:
    runtime = _runtime_with_media(bytes(8 * SECTOR_SIZE))
    block = _define_extent(runtime, "LIVE-BD", 128)
    volume = _define_extent(runtime, "LIVE-VOL", 144)

    assert _execute(runtime, "BD-OPEN", block) == (0,)
    magic = _constant(runtime, "BLOCK-DEVICE-MAGIC")
    assert _cells(runtime, block, 16) == (
        magic,
        1,
        1,
        1,
        1,
        runtime.storage.media_generation,
        SECTOR_SIZE,
        8,
        STORAGE_CAPS,
        0,
        1,
        0,
        0,
        0,
        0,
        0,
    )
    assert _execute(runtime, "BD-VALID?", block) == (TRUE,)
    assert _execute(runtime, "BD-STALE?", block) == (0,)

    assert _execute(runtime, "VOL-RAW", block, volume) == (0,)
    assert _execute(runtime, "VOL-VALID?", volume) == (TRUE,)
    assert runtime.memory.read64(block + 88) == 1
    block_snapshot = runtime.memory.read_bytes(block, 128)
    assert _execute(runtime, "BD-OPEN", block) == (
        _constant(runtime, "BD-E-BUSY"),
    )
    assert _execute(runtime, "BD-CLOSE", block) == (
        _constant(runtime, "BD-E-BUSY"),
    )
    assert runtime.memory.read_bytes(block, 128) == block_snapshot

    assert _execute(runtime, "VOL-CLOSE", volume) == (0,)
    assert runtime.memory.read_bytes(volume, 144) == bytes(144)
    assert runtime.memory.read64(block + 88) == 0
    assert _execute(runtime, "VOL-CLOSE", volume) == (0,)
    assert runtime.memory.read64(block + 88) == 0
    assert _execute(runtime, "BD-CLOSE", block) == (0,)
    assert runtime.memory.read_bytes(block, 128) == bytes(128)


def test_block_io_records_submitted_results_and_detects_replacement_media() -> None:
    image = b"".join(bytes((sector,)) * SECTOR_SIZE for sector in range(4))
    runtime = _runtime_with_media(image)
    block = _define_extent(runtime, "IO-BD", 128)
    dma = _define_extent(runtime, "IO-DMA", SECTOR_SIZE)
    assert _execute(runtime, "BD-OPEN", block) == (0,)

    assert _execute(runtime, "BD-READ", dma, 1, 1, block) == (1, 0)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == bytes((1,)) * SECTOR_SIZE
    assert _cells(runtime, block + 96, 4) == (0, 1, 1, 1)

    diagnostics = runtime.memory.read_bytes(block + 96, 32)
    assert _execute(runtime, "BD-READ", dma, 4, 1, block) == (
        0,
        _constant(runtime, "BD-E-RANGE"),
    )
    assert runtime.memory.read_bytes(block + 96, 32) == diagnostics

    dma_ior = _execute(runtime, "IOR-FROM-BLOCK-RESULT", 6)[0]
    assert _execute(runtime, "BD-READ", MMIO_BASE, 0, 1, block) == (
        0,
        dma_ior,
    )
    assert _cells(runtime, block + 96, 4) == (dma_ior, 0, 0, 1)

    runtime.memory.fill(dma, SECTOR_SIZE, 0xA6)
    assert _execute(runtime, "BD-WRITE", dma, 2, 1, block) == (1, 0)
    assert runtime.storage.image_bytes[
        2 * SECTOR_SIZE : 3 * SECTOR_SIZE
    ] == bytes((0xA6,)) * SECTOR_SIZE
    assert _execute(runtime, "BD-FLUSH", block) == (0,)
    assert _cells(runtime, block + 96, 4) == (0, 0, 2, 1)

    runtime.storage.set_write_protected(True)
    original_sector = runtime.storage.image_bytes[:SECTOR_SIZE]
    dynamic_ior = _execute(runtime, "IOR-FROM-BLOCK-RESULT", 8)[0]
    assert _execute(runtime, "BD-WRITE", dma, 0, 1, block) == (0, dynamic_ior)
    assert runtime.storage.image_bytes[:SECTOR_SIZE] == original_sector
    assert _cells(runtime, block + 96, 4) == (dynamic_ior, 0, 0, 1)

    prior_diagnostics = runtime.memory.read_bytes(block + 96, 32)
    runtime.storage.attach(bytes((0xCC,)) * (4 * SECTOR_SIZE))
    runtime.memory.fill(dma, SECTOR_SIZE, 0x5A)
    assert _execute(runtime, "BD-READ", dma, 0, 1, block) == (
        0,
        _constant(runtime, "BD-E-STALE"),
    )
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == (
        bytes((0x5A,)) * SECTOR_SIZE
    )
    assert runtime.memory.read_bytes(block + 96, 32) == prior_diagnostics


def test_generation_guard_closes_the_serialized_bd_acceptance_edge() -> None:
    replacement = bytes((0xE7,)) * (2 * SECTOR_SIZE)

    class SwapAtGuardedAcceptanceStorage(HostedStorageService):
        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            assert command == STORAGE_CMD_WRITE
            assert expected_generation == self.media_generation
            self.attach(replacement)

    storage = SwapAtGuardedAcceptanceStorage(bytes(2 * SECTOR_SIZE))
    runtime = _load_storage(MegaForthRuntime(storage=storage))
    block = _define_extent(runtime, "GUARDED-BD", 128)
    dma = _define_extent(runtime, "GUARDED-DMA", SECTOR_SIZE)
    runtime.memory.fill(dma, SECTOR_SIZE, 0xA9)
    assert _execute(runtime, "BD-OPEN", block) == (0,)

    dynamic_stale = _execute(runtime, "IOR-FROM-BLOCK-RESULT", 11)[0]
    assert _execute(runtime, "BD-WRITE", dma, 0, 1, block) == (
        0,
        dynamic_stale,
    )
    assert _execute(runtime, "IOR>DOMAIN", dynamic_stale) == (1,)
    assert _execute(runtime, "IOR-STALE?", dynamic_stale) == (TRUE,)
    assert storage.image_bytes == replacement
    assert storage.completion == 1
    assert storage.result == STORAGE_RESULT_MEDIA_REMOVED
    assert storage.transferred == 0


def test_source_readonly_check_precedes_stale_range_and_dma_validation() -> None:
    runtime = _runtime_with_media(
        bytes(2 * SECTOR_SIZE),
        write_protected=True,
    )
    block = _define_extent(runtime, "READONLY-BD", 128)
    assert _execute(runtime, "BD-OPEN", block) == (0,)
    assert runtime.memory.read64(block + 72) == 1
    runtime.storage.attach(
        bytes((0x77,)) * (2 * SECTOR_SIZE),
        write_protected=True,
    )

    assert _execute(runtime, "BD-WRITE", MMIO_BASE, MASK64, 0, block) == (
        0,
        _constant(runtime, "BD-E-READONLY"),
    )
    assert runtime.storage.completion == 0


def test_volume_reslice_is_transactional_and_io_is_relative_to_its_base() -> None:
    image = b"".join(bytes((sector,)) * SECTOR_SIZE for sector in range(4))
    runtime = _runtime_with_media(image)
    block = _define_extent(runtime, "SLICE-BD", 128)
    volume = _define_extent(runtime, "SLICE-VOL", 144)
    dma = _define_extent(runtime, "SLICE-DMA", SECTOR_SIZE)
    assert _execute(runtime, "BD-OPEN", block) == (0,)
    assert _execute(runtime, "VOL-SLICE", 1, 2, 0, 7, block, volume) == (0,)
    assert _execute(runtime, "VOL.BASE", volume) == (1,)
    assert _execute(runtime, "VOL.SECTORS", volume) == (2,)
    assert runtime.memory.read64(block + 88) == 1

    snapshot = runtime.memory.read_bytes(volume, 144)
    assert _execute(runtime, "VOL-SLICE", 3, 2, 0, 8, block, volume) == (
        _constant(runtime, "VOL-E-RANGE"),
    )
    assert runtime.memory.read_bytes(volume, 144) == snapshot
    assert runtime.memory.read64(block + 88) == 1

    assert _execute(runtime, "VOL-SLICE", 2, 2, 1, 4, block, volume) == (0,)
    assert _execute(runtime, "VOL.BASE", volume) == (2,)
    assert _execute(runtime, "VOL.SCHEME", volume) == (1,)
    assert _execute(runtime, "VOL.INDEX", volume) == (4,)
    assert runtime.memory.read64(block + 88) == 1

    assert _execute(runtime, "VOL-READ", dma, 0, 1, volume) == (1, 0)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == bytes((2,)) * SECTOR_SIZE
    assert _execute(runtime, "VOL-READ", dma, 0, 0, volume) == (
        0,
        _constant(runtime, "VOL-E-RANGE"),
    )
    assert _execute(runtime, "VOL-READ", dma, 2, 1, volume) == (
        0,
        _constant(runtime, "VOL-E-RANGE"),
    )

    runtime.memory.fill(dma, SECTOR_SIZE, 0xD3)
    assert _execute(runtime, "VOL-WRITE", dma, 1, 1, volume) == (1, 0)
    assert runtime.storage.image_bytes[
        3 * SECTOR_SIZE : 4 * SECTOR_SIZE
    ] == bytes((0xD3,)) * SECTOR_SIZE
    assert _execute(runtime, "VOL-FLUSH", volume) == (0,)
    assert _execute(runtime, "VOL-CLOSE", volume) == (0,)
    assert runtime.memory.read64(block + 88) == 0
