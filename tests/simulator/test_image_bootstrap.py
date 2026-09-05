"""Focused image-to-session-root coverage for the semantic simulator."""

from __future__ import annotations

import struct

import pytest

from shared.mp64fs import MP64FS_ENTRY_SIZE
from shared.storage import SECTOR_SIZE
from simulator.image_bootstrap import (
    ImageBootstrapError,
    blank_terminal_autoexec_invocation,
    prepare_image_bootstrap,
)
from simulator.platform import create_one_core_address_space
from simulator.storage import HostedStorageService


def _formatted_image(total_sectors: int = 20) -> bytearray:
    bitmap_sectors = (total_sectors + 4095) // 4096
    directory_start = 1 + bitmap_sectors
    data_start = directory_start + 12
    image = bytearray(total_sectors * SECTOR_SIZE)
    image[0:4] = b"MP64"
    struct.pack_into("<H", image, 4, 1)
    struct.pack_into("<I", image, 6, total_sectors)
    struct.pack_into(
        "<HHHHH",
        image,
        10,
        1,
        bitmap_sectors,
        directory_start,
        12,
        data_start,
    )
    image[20] = 128
    image[21] = 48
    for sector in range(data_start):
        _set_allocated(image, sector)
    return image


def _set_allocated(image: bytearray, sector: int) -> None:
    image[SECTOR_SIZE + sector // 8] |= 1 << (sector % 8)


def _directory_offset(image: bytearray) -> int:
    return struct.unpack_from("<H", image, 14)[0] * SECTOR_SIZE


def _write_entry(
    image: bytearray,
    slot: int,
    *,
    name: bytes,
    start: int,
    count: int,
    used: int,
    file_type: int,
    secondary_start: int = 0,
    secondary_count: int = 0,
) -> None:
    if not 0 < len(name) <= 23:
        raise ValueError("test filename must fit the BIOS autoboot token")
    entry = bytearray(MP64FS_ENTRY_SIZE)
    entry[: len(name)] = name
    struct.pack_into("<HHI", entry, 24, start, count, used)
    entry[32] = file_type
    entry[34] = 0xFF
    struct.pack_into("<HH", entry, 44, secondary_start, secondary_count)
    offset = _directory_offset(image) + slot * MP64FS_ENTRY_SIZE
    image[offset : offset + MP64FS_ENTRY_SIZE] = entry
    for sector in range(start, start + count):
        _set_allocated(image, sector)
    for sector in range(secondary_start, secondary_start + secondary_count):
        _set_allocated(image, sector)


def _write_payload(
    image: bytearray,
    payload: bytes,
    *,
    primary_start: int,
    primary_count: int,
    secondary_start: int = 0,
    secondary_count: int = 0,
) -> None:
    primary_bytes = primary_count * SECTOR_SIZE
    secondary_bytes = secondary_count * SECTOR_SIZE
    if len(payload) > primary_bytes + secondary_bytes:
        raise ValueError("test payload exceeds its extents")
    primary = payload[:primary_bytes]
    start = primary_start * SECTOR_SIZE
    image[start : start + len(primary)] = primary
    secondary = payload[primary_bytes:]
    start = secondary_start * SECTOR_SIZE
    image[start : start + len(secondary)] = secondary


def _boot_source() -> bytes:
    prefix = (
        b": DEFER CREATE ['] ABORT , DOES> @ EXECUTE ;\n"
        b": IS ' >BODY ! ;\n"
        b"VARIABLE AUTO-RUNS\n"
        b": _AUTOEXEC-RUN 1 AUTO-RUNS +! ;\n"
    )
    padding = b"\\ filler retained to force the second extent\n" * 11
    assert len(prefix + padding) > SECTOR_SIZE
    return (
        prefix
        + padding
        + b": SECOND-EXTENT-WORD 42 ;\n"
        + b"_AUTOEXEC-RUN\n"
        + b": EVALUATE-CHECKED 2DROP 99 ;\n"
        + b": EVALUATE-FINISH 99 ;\n"
    )


def _boot_image() -> bytearray:
    source = _boot_source()
    image = _formatted_image()
    _write_entry(
        image,
        0,
        name=b"notes.txt",
        start=15,
        count=1,
        used=4,
        file_type=2,
    )
    _write_payload(image, b"note", primary_start=15, primary_count=1)
    _write_entry(
        image,
        1,
        name=b"kdos.f",
        start=14,
        count=1,
        used=len(source),
        file_type=3,
        secondary_start=17,
        secondary_count=1,
    )
    _write_payload(
        image,
        source,
        primary_start=14,
        primary_count=1,
        secondary_start=17,
        secondary_count=1,
    )
    _write_entry(
        image,
        2,
        name=b"later.f",
        start=16,
        count=1,
        used=20,
        file_type=3,
    )
    _write_payload(
        image,
        b"THIS-MUST-NOT-RUN\n",
        primary_start=16,
        primary_count=1,
    )
    return image


def _stored_cell(runtime, name: bytes | str) -> int:
    word = runtime.find(name)
    assert word is not None
    runtime.execute(word.xt)
    address = runtime.main_context.data.pop()
    return runtime.memory.read64(address)


def test_autoexec_transform_preserves_every_offset_and_line_ending() -> None:
    source = (
        b": _AUTOEXEC-RUN 1 ;\r\n"
        b"  _AUTOEXEC-RUN\t\r\n"
        b"JIT-OFF\n"
    )
    transformed = blank_terminal_autoexec_invocation(source)

    assert len(transformed) == len(source)
    assert transformed.count(b"\n") == source.count(b"\n")
    assert transformed == (
        b": _AUTOEXEC-RUN 1 ;\r\n"
        b"               \t\r\n"
        b"JIT-OFF\n"
    )
    with pytest.raises(ImageBootstrapError, match="found 0"):
        blank_terminal_autoexec_invocation(b": _AUTOEXEC-RUN ;\n")
    with pytest.raises(ImageBootstrapError, match="found 2"):
        blank_terminal_autoexec_invocation(
            b"_AUTOEXEC-RUN\n_AUTOEXEC-RUN\n"
        )


def test_image_bootstrap_reads_both_extents_and_defers_normal_autoexec() -> None:
    storage = HostedStorageService(_boot_image())
    prepared = prepare_image_bootstrap(
        memory=create_one_core_address_space(),
        storage=storage,
    )
    runtime = prepared.runtime

    assert prepared.boot_filename == b"kdos.f"
    assert prepared.geometry.total_sectors == 20
    assert prepared.source_bytes == len(_boot_source())
    assert prepared.source_lines == _boot_source().count(b"\n")
    assert prepared.preparation_semantic_steps > 0
    assert prepared.source_accelerators == ()
    assert storage.completion == 5
    assert runtime.find(b"THIS-MUST-NOT-RUN") is None
    assert _stored_cell(runtime, b"AUTO-RUNS") == 0
    runtime.execute(b"SECOND-EXTENT-WORD")
    assert runtime.main_context.data.pop() == 42

    runtime.evaluate(
        b"VARIABLE SESSION-RUNS "
        b": SESSION-MARK 1 SESSION-RUNS +! ; "
        b"' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY",
        source_name="bind-session-entry.f",
    )
    runtime.execute(prepared.root_xt)
    assert _stored_cell(runtime, b"AUTO-RUNS") == 1
    assert _stored_cell(runtime, b"SESSION-RUNS") == 1
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_image_bootstrap_rejects_invalid_metadata_and_missing_forth_entry() -> None:
    invalid_bitmap = _boot_image()
    invalid_bitmap[SECTOR_SIZE] &= 0xFE
    with pytest.raises(ImageBootstrapError, match="metadata is invalid"):
        prepare_image_bootstrap(
            memory=create_one_core_address_space(),
            storage=HostedStorageService(invalid_bitmap),
        )

    no_forth = _formatted_image()
    _write_entry(
        no_forth,
        0,
        name=b"notes.txt",
        start=14,
        count=1,
        used=4,
        file_type=2,
    )
    _write_payload(no_forth, b"note", primary_start=14, primary_count=1)
    with pytest.raises(ImageBootstrapError, match="no occupied Forth"):
        prepare_image_bootstrap(
            memory=create_one_core_address_space(),
            storage=HostedStorageService(no_forth),
        )


def test_checked_source_failure_names_the_image_line_and_status() -> None:
    source = (
        b": DEFER CREATE ['] ABORT , DOES> @ EXECUTE ;\n"
        b": IS ' >BODY ! ;\n"
        b"NO-SUCH-WORD\n"
        b": _AUTOEXEC-RUN ;\n"
        b"_AUTOEXEC-RUN\n"
    )
    image = _formatted_image()
    _write_entry(
        image,
        0,
        name=b"broken.f",
        start=14,
        count=1,
        used=len(source),
        file_type=3,
    )
    _write_payload(image, source, primary_start=14, primary_count=1)

    with pytest.raises(
        ImageBootstrapError,
        match=r"broken\.f:3: status=1",
    ):
        prepare_image_bootstrap(
            memory=create_one_core_address_space(),
            storage=HostedStorageService(image),
        )
