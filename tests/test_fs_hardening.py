"""Tests for Phase 1+2 FS hardening: dynamic bitmap + better errors."""

import os
import struct
import tempfile

import pytest

from diskutil import (
    MP64FS, _compute_geometry, SECTOR_SIZE,
    FS_MARKER,
    FTYPE_FORTH, FTYPE_DATA, FTYPE_RAW,
)


# ── Phase 1: dynamic bitmap geometry ────────────────────────────────

class TestComputeGeometry:
    def test_1mib_default(self):
        bmap, dir_start, data_start = _compute_geometry(2048)
        assert bmap == 1
        assert dir_start == 2
        assert data_start == 14

    def test_2mib(self):
        bmap, dir_start, data_start = _compute_geometry(4096)
        assert bmap == 1
        assert dir_start == 2
        assert data_start == 14

    def test_4mib(self):
        bmap, dir_start, data_start = _compute_geometry(8192)
        assert bmap == 2
        assert dir_start == 3
        assert data_start == 15

    def test_formula_beyond_supported_runtime_cap(self):
        bmap, dir_start, data_start = _compute_geometry(16384)
        assert bmap == 4
        assert dir_start == 5
        assert data_start == 17

    def test_boundary_4097(self):
        """4097 sectors needs 2 bitmap sectors (4096 bits won't suffice)."""
        bmap, _, _ = _compute_geometry(4097)
        assert bmap == 2


class TestFormatSmallImage:

    def test_geometry(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        assert fs.bmap_sectors == 1
        assert fs.dir_start == 2
        assert fs.data_start == 14

    def test_superblock_values(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        assert struct.unpack_from("<H", fs.img, 12)[0] == 1   # bmap_sectors
        assert struct.unpack_from("<H", fs.img, 14)[0] == 2   # dir_start
        assert struct.unpack_from("<H", fs.img, 18)[0] == 14  # data_start
        assert struct.unpack_from("<H", fs.img, 4)[0] == FS_MARKER

    def test_metadata_sectors_allocated(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        bmap = fs._bmap
        # Sectors 0–13 should be allocated
        for s in range(14):
            byte_idx, bit_idx = divmod(s, 8)
            assert bmap[byte_idx] & (1 << bit_idx), f"sector {s} not allocated"
        # Sector 14 should be free
        byte_idx, bit_idx = divmod(14, 8)
        assert not (bmap[byte_idx] & (1 << bit_idx))

    def test_inject_read_roundtrip(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        fs.inject_file("test.f", b"hello world", ftype=FTYPE_FORTH)
        assert fs.read_file("test.f") == b"hello world"


class TestFormatLargeImage:
    """Multi-sector bitmap for >4096 sectors."""

    def test_4mib_geometry(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        assert fs.bmap_sectors == 2
        assert fs.dir_start == 3
        assert fs.data_start == 15

    def test_4mib_superblock(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        assert struct.unpack_from("<I", fs.img, 6)[0] == 8192
        assert struct.unpack_from("<H", fs.img, 12)[0] == 2
        assert struct.unpack_from("<H", fs.img, 14)[0] == 3
        assert struct.unpack_from("<H", fs.img, 18)[0] == 15
        assert struct.unpack_from("<H", fs.img, 4)[0] == FS_MARKER

    def test_4mib_metadata_allocated(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        bmap = fs._bmap
        # Sectors 0–14 should be allocated (data_start=15)
        for s in range(15):
            byte_idx, bit_idx = divmod(s, 8)
            assert bmap[byte_idx] & (1 << bit_idx), f"sector {s} not allocated"
        # Sector 15 should be free
        byte_idx, bit_idx = divmod(15, 8)
        assert not (bmap[byte_idx] & (1 << bit_idx))

    def test_inject_large_file(self):
        """Inject a file larger than 1 MiB into a 4 MiB image."""
        fs = MP64FS(total_sectors=8192)
        fs.format()
        data = bytes(range(256)) * 4096  # 1 MiB
        fs.inject_file("big.dat", data, ftype=FTYPE_DATA)
        readback = fs.read_file("big.dat")
        assert readback == data

    def test_free_sectors_correct(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        # Total data sectors = 8192 - 15 = 8177
        assert fs._count_free() == 8177

    def test_inject_near_boundary(self):
        """Inject files that span the bitmap sector boundary."""
        fs = MP64FS(total_sectors=8192)
        fs.format()
        # Use most of the space
        data = b"\xAA" * (8000 * SECTOR_SIZE)
        fs.inject_file("fill.dat", data, ftype=FTYPE_DATA)
        assert fs.read_file("fill.dat") == data
        remaining = fs._count_free()
        assert remaining == 8177 - 8000

    def test_rejects_media_above_runtime_cap(self):
        fs = MP64FS(total_sectors=8193)
        with pytest.raises(ValueError, match="at most 8192 sectors"):
            fs.format()

    def test_rejects_media_without_a_data_sector(self):
        fs = MP64FS(total_sectors=14)
        with pytest.raises(ValueError, match="too small"):
            fs.format()


class TestSaveLoadRoundtrip:
    def test_1mib_roundtrip(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        fs.inject_file("hello.f", b": greet ;", ftype=FTYPE_FORTH)

        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            fs.save(path)
            fs2 = MP64FS.load(path)
            assert fs2.bmap_sectors == 1
            assert fs2.dir_start == 2
            assert fs2.data_start == 14
            assert fs2.read_file("hello.f") == b": greet ;"
        finally:
            os.unlink(path)

    def test_4mib_roundtrip(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        fs.inject_file("test.f", b"hello", ftype=FTYPE_FORTH)

        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            fs.save(path)
            fs2 = MP64FS.load(path)
            assert fs2.bmap_sectors == 2
            assert fs2.dir_start == 3
            assert fs2.data_start == 15
            assert fs2.read_file("test.f") == b"hello"
        finally:
            os.unlink(path)


class TestGeometryValidation:
    def test_rejects_invalid_format_marker(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        struct.pack_into("<H", fs.img, 4, 99)
        with pytest.raises(ValueError, match="invalid format marker"):
            MP64FS(bytearray(fs.img))

    def test_rejects_geometry_larger_than_image(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        struct.pack_into("<I", fs.img, 6, 16384)
        with pytest.raises(ValueError, match="image sectors"):
            MP64FS(bytearray(fs.img))

    def test_rejects_shifted_directory(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        struct.pack_into("<H", fs.img, 14, 4)
        with pytest.raises(ValueError, match="directory start"):
            MP64FS(bytearray(fs.img))

    def test_cli_contains_malformed_attached_filesystem(self, capsys):
        """Host inspection reports invalid geometry without aborting the CLI."""
        from types import SimpleNamespace
        from cli import MegapadCLI

        fs = MP64FS(total_sectors=2048)
        fs.format()
        struct.pack_into("<H", fs.img, 14, 3)
        system = SimpleNamespace(
            storage=SimpleNamespace(_image_data=bytearray(fs.img)),
            uart=SimpleNamespace(on_tx=None),
        )

        cli = MegapadCLI(system)
        assert cli._get_fs() is None
        assert "Invalid filesystem:" in capsys.readouterr().out

    def test_cli_contains_unaligned_storage_attach(self, tmp_path, capsys):
        """An invalid host image cannot escape the interactive command."""
        from types import SimpleNamespace
        from cli import MegapadCLI
        from devices import Storage

        image = tmp_path / "unaligned.img"
        image.write_bytes(b"not a sector")
        system = SimpleNamespace(
            storage=Storage(),
            sysinfo=SimpleNamespace(has_storage=False),
            uart=SimpleNamespace(on_tx=None),
        )

        cli = MegapadCLI(system)
        cli.do_storage(f"attach {image}")
        assert system.sysinfo.has_storage is False
        assert "Storage attach failed:" in capsys.readouterr().out


# ── Phase 2: better error messages + largest_free_run ────────────────

class TestAllocErrorMessages:
    def test_no_space_reports_counts(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        # Fill up most of the disk (2034 data sectors, use 2020)
        data = b"\x00" * (2020 * SECTOR_SIZE)
        fs.inject_file("big.dat", data, ftype=FTYPE_DATA)
        # Try to allocate more than remaining
        with pytest.raises(RuntimeError, match=r"need 100 sectors.*only 14 free"):
            fs.inject_file("toobig.dat", b"\x00" * (100 * SECTOR_SIZE), ftype=FTYPE_DATA)

    def test_completely_full(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        # Fill all data sectors (2034)
        data = b"\x00" * (2034 * SECTOR_SIZE)
        fs.inject_file("full.dat", data, ftype=FTYPE_DATA)
        with pytest.raises(RuntimeError, match=r"need 1 sectors.*only 0 free"):
            fs.inject_file("one.dat", b"x", ftype=FTYPE_DATA)

    def test_fragmented_suggests_compact(self):
        """When total free >= needed but no contiguous run, mention compact."""
        fs = MP64FS(total_sectors=2048)
        fs.format()
        # Create scattered files to fragment the disk
        for i in range(50):
            fs.inject_file(f"f{i:02d}", b"\x00" * (20 * SECTOR_SIZE), ftype=FTYPE_DATA)
        # Delete every other file to create gaps
        for i in range(0, 50, 2):
            fs.delete_file(f"f{i:02d}")
        # Now there's lots of free space but fragmented into 20-sector runs
        free = fs._count_free()
        largest = fs.largest_free_run()
        assert free > 500
        assert largest <= 1034  # not one big run
        # Try to allocate more than largest run (and enough that 2 extents fail)
        need = free  # ask for almost all of it
        with pytest.raises(RuntimeError, match=r"compact|free"):
            fs.inject_file("huge.dat", b"\x00" * (need * SECTOR_SIZE), ftype=FTYPE_DATA)


class TestLargestFreeRun:
    def test_fresh_disk(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        # All 2034 data sectors are one contiguous run
        assert fs.largest_free_run() == 2034

    def test_after_inject(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        fs.inject_file("a.dat", b"\x00" * (100 * SECTOR_SIZE), ftype=FTYPE_DATA)
        # Remaining should be one run of 1934
        assert fs.largest_free_run() == 2034 - 100

    def test_fragmented(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        # Inject two files
        fs.inject_file("a.dat", b"\x00" * (100 * SECTOR_SIZE), ftype=FTYPE_DATA)
        fs.inject_file("b.dat", b"\x00" * (100 * SECTOR_SIZE), ftype=FTYPE_DATA)
        # Delete the first — leaves a 100-sector gap at the front
        fs.delete_file("a.dat")
        largest = fs.largest_free_run()
        # The tail run (1834) should be the largest, not the 100-sector gap
        assert largest == 2034 - 200

    def test_large_image(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        assert fs.largest_free_run() == 8192 - 15  # 8177


class TestInfoDict:
    def test_includes_new_fields(self):
        fs = MP64FS(total_sectors=2048)
        fs.format()
        info = fs.info()
        assert info["largest_free_run"] == 2034
        assert info["bmap_sectors"] == 1
        assert info["data_start"] == 14
        assert info["free_sectors"] == 2034

    def test_large_image_info(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        info = fs.info()
        assert info["bmap_sectors"] == 2
        assert info["total_sectors"] == 8192
        assert info["data_start"] == 15


class TestCompactLargeImage:
    def test_compact_preserves_geometry(self):
        fs = MP64FS(total_sectors=8192)
        fs.format()
        fs.inject_file("a.dat", b"AAA", ftype=FTYPE_DATA)
        fs.inject_file("b.dat", b"BBB", ftype=FTYPE_DATA)
        fs.delete_file("a.dat")
        moved = fs.compact()
        # b.dat should have been moved forward
        assert moved >= 1
        assert fs.bmap_sectors == 2
        assert fs.data_start == 15
        assert fs.read_file("b.dat") == b"BBB"
