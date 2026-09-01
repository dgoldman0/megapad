"""Unchanged-source acceptance for the KDOS legacy file abstraction."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from shared.storage import SECTOR_SIZE
from simulator.errors import ForthAbort
from simulator.runtime import ConstantDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _define_extent,
    _execute,
    _variable,
)
from tests.simulator.test_kdos_storage_compat import (
    _load_storage_compat,
    _patterned_image,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-file-abstraction-4804-5003.f"
)

FIRST_LINE = 4804
LAST_LINE = 5003
SLICE_SHA256 = (
    "b022f3514605371f527a1e823b78ea26b5b09dad44198b4936272eaef1bb091b"
)
SLICE_GIT_BLOB = "1f6546ff06f3db1a983f1eaf11f09172115823db"

DEFINITIONS = (
    b"FILE-COUNT",
    b"FILE-TABLE",
    b"FSCRATCH",
    b"F.START",
    b"F.MAX",
    b"F.USED",
    b"F.CURSOR",
    b"FDESC",
    b"FILE",
    b"FSEEK",
    b"FREWIND",
    b"FSIZE",
    b"FT-N",
    b"FTRUNCATE",
    b"FW-FD",
    b"FW-ADDR",
    b"FW-LEN",
    b"FW-REM",
    b"FW-POS",
    b"FW-DISK-SEC",
    b"FW-CHUNK",
    b"FW-HEAD",
    b"FW-FULL",
    b"FW-TAIL",
    b"FWRITE",
    b"FR-FD",
    b"FR-ADDR",
    b"FR-LEN",
    b"FR-REM",
    b"FR-POS",
    b"FR-DISK-SEC",
    b"FR-CHUNK",
    b"FR-HEAD",
    b"FR-FULL",
    b"FR-TAIL",
    b"FREAD",
    b"F.INFO",
    b"FILES",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 6_781
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    THEN ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        b"\\ =====================================================================\n"
    )
    return source


def _evaluate_file_abstraction(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_file_abstraction(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_file_abstraction(_load_storage_compat(image))


def _make_file(
    runtime: MegaForthRuntime,
    start_sector: int,
    max_sectors: int,
    name: str,
) -> int:
    runtime.evaluate(
        f"{start_sector} {max_sectors} FILE {name}".encode("ascii"),
        source_name=f"legacy-file-{name.lower()}",
    )
    return _execute(runtime, name)[0]


def _file_fields(runtime: MegaForthRuntime, descriptor: int) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(descriptor + offset)
        for offset in range(0, 32, 8)
    )


def test_file_abstraction_slice_is_exact_and_loads_without_io() -> None:
    runtime = _load_file_abstraction(_patterned_image(4))

    assert len(DEFINITIONS) == 38
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _variable(runtime, "FILE-COUNT") == 0
    file_table = _execute(runtime, "FILE-TABLE")[0]
    fscratch = _execute(runtime, "FSCRATCH")[0]
    # VARIABLE initializes the first cell. Virgin hosted memory supplies the
    # remaining ALLOT bytes; source does not explicitly clear either extent.
    assert runtime.memory.read_bytes(file_table, 8 * 8) == bytes(8 * 8)
    assert runtime.memory.read_bytes(fscratch, SECTOR_SIZE) == bytes(SECTOR_SIZE)
    for name in (
        "FDESC",
        "FT-N",
        "FW-FD",
        "FW-ADDR",
        "FW-LEN",
        "FW-REM",
        "FW-POS",
        "FW-CHUNK",
        "FR-FD",
        "FR-ADDR",
        "FR-LEN",
        "FR-REM",
        "FR-POS",
        "FR-CHUNK",
    ):
        assert _variable(runtime, name) == 0
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None
    assert _execute(runtime, "FILES") == ()
    assert runtime.drain_uart_output() == b" --- Files (0  ) ---\r\n"


def test_file_defining_word_builds_constants_and_caps_only_the_registry() -> None:
    runtime = _load_file_abstraction()
    runtime.evaluate(
        b": MAKE-FILE FILE ; "
        b": CONSTANT 99 ; "
        b"12 2 MAKE-FILE WRAPPED-FILE",
        source_name="compiled-legacy-file-definition",
    )
    wrapped_word = runtime.find(b"WRAPPED-FILE")
    assert wrapped_word is not None
    assert isinstance(wrapped_word.implementation, ConstantDefinition)
    wrapped = _execute(runtime, "WRAPPED-FILE")[0]
    descriptors = [wrapped] + [
        _make_file(runtime, index + 2, index + 1, f"LEGACY-FILE-{index}")
        for index in range(9)
    ]
    expected_geometry = [(12, 2)] + [
        (index + 2, index + 1) for index in range(9)
    ]

    assert _variable(runtime, "FILE-COUNT") == 8
    table = _execute(runtime, "FILE-TABLE")[0]
    assert tuple(
        runtime.memory.read64(table + index * 8) for index in range(8)
    ) == tuple(descriptors[:8])
    for descriptor, (start, maximum) in zip(
        descriptors,
        expected_geometry,
        strict=True,
    ):
        assert _file_fields(runtime, descriptor) == (
            start,
            maximum,
            0,
            0,
        )
    for index, descriptor in enumerate(descriptors[1:]):
        assert _execute(runtime, f"LEGACY-FILE-{index}") == (descriptor,)
    assert _variable(runtime, "FDESC") == descriptors[-1]
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""


def test_seek_rewind_size_and_unsigned_truncate_metadata() -> None:
    runtime = _load_file_abstraction()
    descriptor = _make_file(runtime, 4, 2, "META-FILE")

    assert _execute(runtime, "FSIZE", descriptor) == (0,)
    assert _execute(runtime, "FSEEK", 900, descriptor) == ()
    assert _file_fields(runtime, descriptor) == (4, 2, 0, 900)
    assert _execute(runtime, "FTRUNCATE", 700, descriptor) == ()
    assert _file_fields(runtime, descriptor) == (4, 2, 700, 700)

    # FTRUNCATE can extend used length without touching disk.
    assert _execute(runtime, "FTRUNCATE", 900, descriptor) == ()
    assert _file_fields(runtime, descriptor) == (4, 2, 900, 700)
    assert _execute(runtime, "FSEEK", 1_200, descriptor) == ()
    # Current executable MIN is unsigned, so -1 clamps to capacity.
    assert _execute(runtime, "FTRUNCATE", MASK64, descriptor) == ()
    assert _file_fields(runtime, descriptor) == (4, 2, 1_024, 1_024)
    assert _execute(runtime, "FREWIND", descriptor) == ()
    assert _execute(runtime, "FSIZE", descriptor) == (1_024,)
    assert _execute(runtime, "F.CURSOR", descriptor) == (0,)
    assert runtime.storage.completion == 0


def test_write_and_read_compose_head_full_and_tail_sector_paths() -> None:
    original = _patterned_image(10)
    runtime = _load_file_abstraction(original)
    descriptor = _make_file(runtime, 2, 5, "STREAM-FILE")
    source = _define_extent(runtime, "STREAM-SOURCE", 1_800)
    destination = _define_extent(runtime, "STREAM-DESTINATION", 1_800)
    payload = bytes((index * 37 + 11) & 0xFF for index in range(1_800))
    runtime.memory.write_bytes(source, payload)

    assert _execute(runtime, "FSEEK", 100, descriptor) == ()
    assert _execute(runtime, "FWRITE", source, len(payload), descriptor) == ()
    expected_image = bytearray(original)
    disk_start = 2 * SECTOR_SIZE + 100
    expected_image[disk_start : disk_start + len(payload)] = payload
    assert runtime.storage.image_bytes == bytes(expected_image)
    assert _file_fields(runtime, descriptor) == (2, 5, 1_900, 1_900)
    assert runtime.storage.completion == 5
    assert runtime.drain_uart_output() == b""

    runtime.memory.fill(destination, len(payload), 0)
    assert _execute(runtime, "FSEEK", 100, descriptor) == ()
    assert _execute(runtime, "FREAD", destination, len(payload), descriptor) == (
        len(payload),
    )
    assert runtime.memory.read_bytes(destination, len(payload)) == payload
    assert _execute(runtime, "F.CURSOR", descriptor) == (1_900,)
    assert runtime.storage.completion == 8

    runtime.memory.fill(destination, 300, 0xCC)
    assert _execute(runtime, "FSEEK", 1_850, descriptor) == ()
    assert _execute(runtime, "FREAD", destination, 300, descriptor) == (50,)
    assert runtime.memory.read_bytes(destination, 50) == payload[-50:]
    assert runtime.memory.read_bytes(destination + 50, 250) == bytes((0xCC,)) * 250
    assert _execute(runtime, "F.CURSOR", descriptor) == (1_900,)
    assert runtime.storage.completion == 9

    assert _execute(runtime, "FREAD", destination, 16, descriptor) == (0,)
    assert _execute(runtime, "F.CURSOR", descriptor) == (1_900,)
    assert runtime.storage.completion == 9
    assert _execute(runtime, "FSEEK", 100, descriptor) == ()
    assert _execute(runtime, "FREAD", destination, 0, descriptor) == (0,)
    assert _execute(runtime, "F.CURSOR", descriptor) == (100,)
    assert runtime.storage.completion == 9


def test_signed_capacity_and_eof_guards_reject_safe_high_bit_cases() -> None:
    runtime = _load_file_abstraction(_patterned_image(2))
    descriptor = _make_file(runtime, 0, 1 << 54, "HIGH-BIT-FILE")
    source = _define_extent(runtime, "HIGH-BIT-SOURCE", 1)
    destination = _define_extent(runtime, "HIGH-BIT-DESTINATION", 1)
    runtime.memory.write8(destination, 0xA5)

    # max_sectors * 512 is INT64_MIN as a cell. The signed `>` bounds check
    # therefore rejects even len=0 before FWRITE reaches its zero-length test.
    assert _execute(runtime, "FWRITE", source, 0, descriptor) == ()
    assert runtime.drain_uart_output() == b" FWRITE: out of space\r\n"
    assert _file_fields(runtime, descriptor) == (0, 1 << 54, 0, 0)
    assert runtime.storage.completion == 0

    assert _execute(runtime, "FTRUNCATE", 1 << 63, descriptor) == ()
    # The signed `<` EOF guard treats high-bit used length as negative.
    assert _execute(runtime, "FREAD", destination, 1, descriptor) == (0,)
    assert runtime.memory.read8(destination) == 0xA5
    assert _file_fields(runtime, descriptor) == (
        0,
        1 << 54,
        1 << 63,
        0,
    )
    assert runtime.storage.completion == 0


def test_write_capacity_boundary_and_zero_length_path() -> None:
    original = _patterned_image(5)
    runtime = _load_file_abstraction(original)
    descriptor = _make_file(runtime, 1, 2, "BOUNDED-FILE")
    source = _define_extent(runtime, "BOUNDED-SOURCE", 25)
    payload = bytes(range(25))
    runtime.memory.write_bytes(source, payload)
    assert _execute(runtime, "FSEEK", 1_000, descriptor) == ()

    assert _execute(runtime, "FWRITE", source, 25, descriptor) == ()
    assert runtime.drain_uart_output() == b" FWRITE: out of space\r\n"
    assert _file_fields(runtime, descriptor) == (1, 2, 0, 1_000)
    assert runtime.storage.image_bytes == original
    assert runtime.storage.completion == 0

    assert _execute(runtime, "FWRITE", source, 24, descriptor) == ()
    expected_image = bytearray(original)
    expected_image[3 * SECTOR_SIZE - 24 : 3 * SECTOR_SIZE] = payload[:24]
    assert runtime.storage.image_bytes == bytes(expected_image)
    assert _file_fields(runtime, descriptor) == (1, 2, 1_024, 1_024)
    assert runtime.storage.completion == 2

    assert _execute(runtime, "FWRITE", source, 0, descriptor) == ()
    assert _file_fields(runtime, descriptor) == (1, 2, 1_024, 1_024)
    assert runtime.storage.completion == 2
    assert runtime.drain_uart_output() == b""


def test_late_write_failure_keeps_earlier_sectors_but_not_metadata() -> None:
    original = _patterned_image(4)
    runtime = _load_file_abstraction(original)
    descriptor = _make_file(runtime, 2, 3, "PARTIAL-FILE")
    source = _define_extent(runtime, "PARTIAL-SOURCE", 1_200)
    payload = bytes((index * 19 + 5) & 0xFF for index in range(1_200))
    runtime.memory.write_bytes(source, payload)
    assert _execute(runtime, "FSEEK", 100, descriptor) == ()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        _execute(runtime, "FWRITE", source, len(payload), descriptor)

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert _file_fields(runtime, descriptor) == (2, 3, 0, 100)
    expected_image = bytearray(original)
    disk_start = 2 * SECTOR_SIZE + 100
    confirmed = (SECTOR_SIZE - 100) + SECTOR_SIZE
    expected_image[disk_start : disk_start + confirmed] = payload[:confirmed]
    assert runtime.storage.image_bytes == bytes(expected_image)
    assert runtime.storage.completion == 3
    assert _variable(runtime, "DISK-IO-STATUS") == 4
    assert _variable(runtime, "DISK-IO-COMPLETED") == 0
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-RANGE",
    )


def test_file_info_and_registry_listing_use_the_ordinary_uart_publisher() -> None:
    runtime = _load_file_abstraction()
    alpha = _make_file(runtime, 2, 3, "ALPHA-FILE")
    beta = _make_file(runtime, 7, 1, "BETA-FILE")
    assert _execute(runtime, "FTRUNCATE", 600, alpha) == ()
    assert _execute(runtime, "FSEEK", 19, alpha) == ()
    assert _execute(runtime, "FTRUNCATE", 100, beta) == ()
    assert _execute(runtime, "FSEEK", 4, beta) == ()

    assert _execute(runtime, "F.INFO", alpha) == ()
    assert runtime.drain_uart_output() == (
        b" [file  sec=2   max=3   used=600   cur=19  ]\r\n"
    )

    assert _execute(runtime, "FILES") == ()
    assert runtime.drain_uart_output() == (
        b" --- Files (2  ) ---\r\n"
        b"0  :  [file  sec=2   max=3   used=600   cur=19  ]\r\n"
        b"1  :  [file  sec=7   max=1   used=100   cur=4  ]\r\n"
    )
