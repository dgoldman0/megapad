"""Unchanged-source acceptance for the first MP64FS cache helpers."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64, TRUE
from shared.storage import SECTOR_SIZE
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_file_abstraction import _load_file_abstraction
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)
from tests.simulator.test_kdos_storage_compat import _patterned_image


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures") / "kdos-mp64fs-cache-5004-5134.f"
)

FIRST_LINE = 5004
LAST_LINE = 5134
SLICE_SHA256 = (
    "caf26787745bdf711a89130db7f8b30d45b0f9a63534b4ccb58a601bb2cea062"
)
SLICE_GIT_BLOB = "0789d11789aa0fd1b086f0370c7c57e84a61a956"

DEFINITIONS = (
    b"FS-MAX-FILES",
    b"FS-ENTRY-SIZE",
    b"FS-MAX-BMAP-SECTORS",
    b"FS-MAX-SECTORS",
    b"FS-SUPER",
    b"FS-BMAP",
    b"FS-DIR",
    b"FS-TOTAL",
    b"FS-BMAP-N",
    b"FS-DIR-START",
    b"FS-DSTART",
    b"CWD",
    b"BIT-MASK",
    b"BIT-FREE?",
    b"BIT-SET",
    b"BIT-CLR",
    b"FF-NEED",
    b"FF-START",
    b"FF-LEN",
    b"FIND-FREE",
    b"DIRENT",
    b"DE.SEC",
    b"DE.COUNT",
    b"DE.USED",
    b"DE.TYPE",
    b"DE.FLAGS",
    b"DE.PARENT",
    b"DE.MTIME",
    b"DE.CRC",
    b"DE.EXT1-SEC",
    b"DE.EXT1-CNT",
    b"FIND-FREE-SLOT",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 4_579
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    ELSE DROP THEN ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── Loading and syncing ──────────────────────────────────────────────\n".encode(
            "utf-8"
        )
    )
    return source


def _evaluate_mp64fs_cache(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_cache(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_cache(_load_file_abstraction(image))


def test_mp64fs_cache_slice_is_exact_and_loads_without_storage_io() -> None:
    runtime = _load_mp64fs_cache(_patterned_image(32))

    assert len(DEFINITIONS) == 32
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _constant(runtime, "FS-MAX-FILES") == 128
    assert _constant(runtime, "FS-ENTRY-SIZE") == 48
    assert _constant(runtime, "FS-MAX-BMAP-SECTORS") == 16
    assert _constant(runtime, "FS-MAX-SECTORS") == 65_536
    assert _variable(runtime, "FS-TOTAL") == 2_048
    assert _variable(runtime, "FS-BMAP-N") == 1
    assert _execute(runtime, "FS-DIR-START") == (2,)
    assert _execute(runtime, "FS-DSTART") == (14,)
    assert _variable(runtime, "CWD") == 255
    assert _variable(runtime, "FS-OK") == 0

    superblock = _execute(runtime, "FS-SUPER")[0]
    bitmap = _execute(runtime, "FS-BMAP")[0]
    directory = _execute(runtime, "FS-DIR")[0]
    # VARIABLE initializes one cell; virgin hosted memory supplies each ALLOT
    # tail. Only the operational cache windows, not source/dictionary padding,
    # are content-pinned.
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == bytes(SECTOR_SIZE)
    assert runtime.memory.read_bytes(bitmap, 16 * SECTOR_SIZE) == bytes(
        16 * SECTOR_SIZE
    )
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == bytes(
        12 * SECTOR_SIZE
    )
    for name in ("FF-NEED", "FF-START", "FF-LEN"):
        assert _variable(runtime, name) == 0
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None

    total = _execute(runtime, "FS-TOTAL")[0]
    bmap_count = _execute(runtime, "FS-BMAP-N")[0]
    runtime.memory.write64(total, 65_536)
    runtime.memory.write64(bmap_count, 16)
    assert _execute(runtime, "FS-DIR-START") == (17,)
    assert _execute(runtime, "FS-DSTART") == (29,)
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0


def test_bitmap_words_address_bits_across_the_complete_cache() -> None:
    runtime = _load_mp64fs_cache()
    bitmap = _execute(runtime, "FS-BMAP")[0]
    runtime.memory.write64(_execute(runtime, "FS-TOTAL")[0], 65_536)
    runtime.memory.write64(_execute(runtime, "FS-BMAP-N")[0], 16)
    sectors = (0, 7, 8, 4_095, 4_096, 65_535)

    for bit in range(8):
        assert _execute(runtime, "BIT-MASK", bit) == (1 << bit,)
    for sector in sectors:
        assert _execute(runtime, "BIT-FREE?", sector) == (TRUE,)
        assert _execute(runtime, "BIT-SET", sector) == ()
        assert _execute(runtime, "BIT-FREE?", sector) == (0,)

    assert runtime.memory.read8(bitmap) == 0x81
    assert runtime.memory.read8(bitmap + 1) == 0x01
    assert runtime.memory.read8(bitmap + 511) == 0x80
    assert runtime.memory.read8(bitmap + 512) == 0x01
    assert runtime.memory.read8(bitmap + 8_191) == 0x80

    for sector in (7, 4_096, 65_535):
        assert _execute(runtime, "BIT-CLR", sector) == ()
        assert _execute(runtime, "BIT-FREE?", sector) == (TRUE,)
    assert runtime.memory.read8(bitmap) == 0x01
    assert runtime.memory.read8(bitmap + 512) == 0
    assert runtime.memory.read8(bitmap + 8_191) == 0
    assert _execute(runtime, "BIT-FREE?", 6) == (TRUE,)
    assert _execute(runtime, "BIT-FREE?", 9) == (TRUE,)
    assert runtime.storage.completion == 0


def test_find_free_returns_the_first_complete_run_without_allocating_it() -> None:
    runtime = _load_mp64fs_cache()
    total = _execute(runtime, "FS-TOTAL")[0]
    runtime.memory.write64(total, 40)
    for sector in (14, 15, 18, *range(23, 40)):
        assert _execute(runtime, "BIT-SET", sector) == ()

    assert _execute(runtime, "FIND-FREE", 1) == (16,)
    assert (_variable(runtime, "FF-NEED"), _variable(runtime, "FF-START")) == (
        1,
        16,
    )
    assert _execute(runtime, "FIND-FREE", 3) == (19,)
    assert (
        _variable(runtime, "FF-NEED"),
        _variable(runtime, "FF-START"),
        _variable(runtime, "FF-LEN"),
    ) == (3, 19, 3)
    assert _execute(runtime, "FIND-FREE", 5) == (MASK64,)
    assert _variable(runtime, "FF-LEN") == 0

    # Search reports geometry only; it never claims the returned bits.
    for sector in (16, 17, 19, 20, 21, 22):
        assert _execute(runtime, "BIT-FREE?", sector) == (TRUE,)

    for sector in (16, 17, 19, 20, 21, 22):
        assert _execute(runtime, "BIT-SET", sector) == ()
    for sector in (37, 38, 39):
        assert _execute(runtime, "BIT-CLR", sector) == ()
    assert _execute(runtime, "FIND-FREE", 3) == (37,)
    for sector in (37, 38, 39):
        assert _execute(runtime, "BIT-FREE?", sector) == (TRUE,)
    assert runtime.storage.completion == 0


def test_directory_addresses_and_field_readers_decode_packed_little_endian() -> None:
    runtime = _load_mp64fs_cache()
    directory = _execute(runtime, "FS-DIR")[0]
    entry = _execute(runtime, "DIRENT", 37)[0]
    assert entry == directory + 37 * 48
    assert _execute(runtime, "DIRENT", 0) == (directory,)
    assert _execute(runtime, "DIRENT", 127) == (directory + 127 * 48,)

    runtime.memory.write_bytes(entry, b"sample.bin\0" + bytes(13))
    runtime.memory.write_bytes(entry + 24, bytes.fromhex("efbe3412efcdab89"))
    runtime.memory.write8(entry + 32, 10)
    runtime.memory.write8(entry + 33, 0x0D)
    runtime.memory.write8(entry + 34, 0xFF)
    runtime.memory.write8(entry + 35, 0)
    runtime.memory.write_bytes(entry + 36, bytes.fromhex("40302010efbeadde"))
    runtime.memory.write_bytes(entry + 44, bytes.fromhex("67452301"))

    assert _execute(runtime, "DE.SEC", entry) == (0xBEEF,)
    assert _execute(runtime, "DE.COUNT", entry) == (0x1234,)
    assert _execute(runtime, "DE.USED", entry) == (0x89ABCDEF,)
    assert _execute(runtime, "DE.TYPE", entry) == (10,)
    assert _execute(runtime, "DE.FLAGS", entry) == (0x0D,)
    assert _execute(runtime, "DE.PARENT", entry) == (0xFF,)
    assert _execute(runtime, "DE.MTIME", entry) == (0x10203040,)
    assert _execute(runtime, "DE.CRC", entry) == (0xDEADBEEF,)
    assert _execute(runtime, "DE.EXT1-SEC", entry) == (0x4567,)
    assert _execute(runtime, "DE.EXT1-CNT", entry) == (0x0123,)
    assert runtime.storage.completion == 0


def test_find_free_slot_returns_the_first_valid_cache_hole_and_caps_at_128() -> None:
    runtime = _load_mp64fs_cache()
    directory = _execute(runtime, "FS-DIR")[0]

    assert _execute(runtime, "FIND-FREE-SLOT") == (0,)

    def write_directory(slot: int) -> None:
        entry = directory + slot * 48
        runtime.memory.write_bytes(entry, bytes(48))
        runtime.memory.write_bytes(entry, f"D{slot:03d}\0".encode("ascii"))
        runtime.memory.write8(entry + 32, 8)
        runtime.memory.write8(entry + 34, 0xFF)

    write_directory(0)
    assert _execute(runtime, "FIND-FREE-SLOT") == (1,)
    write_directory(1)
    assert _execute(runtime, "FIND-FREE-SLOT") == (2,)

    for slot in range(128):
        write_directory(slot)
    assert _execute(runtime, "FIND-FREE-SLOT") == (MASK64,)
    runtime.memory.write_bytes(directory + 73 * 48, bytes(48))
    assert _execute(runtime, "FIND-FREE-SLOT") == (73,)
    assert runtime.storage.completion == 0
