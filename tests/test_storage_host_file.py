"""The host file that holds a storage device's media writes only what changed."""

from __future__ import annotations

import pytest

import shared.storage
from shared.storage import SECTOR_SIZE, HostImageFile, _runs


def _sector(value: int) -> bytes:
    return bytes((value,)) * SECTOR_SIZE


def _file(tmp_path, sectors: int, fill: int):
    path = tmp_path / "media.img"
    path.write_bytes(_sector(fill) * sectors)
    return path


@pytest.fixture
def fsyncs(monkeypatch):
    calls = []
    real = shared.storage.os.fsync

    def counting(fd):
        calls.append(fd)
        real(fd)

    monkeypatch.setattr(shared.storage.os, "fsync", counting)
    return calls


def test_a_flush_writes_only_the_marked_sectors_and_syncs_once(tmp_path, fsyncs):
    path = _file(tmp_path, 8, 0x00)
    image = bytearray(path.read_bytes())
    host = HostImageFile()
    host.media_attached(file_holds_media=True)
    for sector in (2, 3, 5, 6):
        image[sector * SECTOR_SIZE:(sector + 1) * SECTOR_SIZE] = _sector(0xA0 + sector)
    # Sector 5 changes without a mark, so the flush must leave it alone.
    host.mark(2, 2)
    host.mark(6)

    host.flush(path, image)

    on_disk = path.read_bytes()
    expected = bytearray(_sector(0x00) * 8)
    for sector in (2, 3, 6):
        expected[sector * SECTOR_SIZE:(sector + 1) * SECTOR_SIZE] = _sector(0xA0 + sector)
    assert on_disk == expected
    assert len(fsyncs) == 1


def test_a_flush_with_nothing_marked_leaves_the_file_alone(tmp_path, fsyncs):
    path = _file(tmp_path, 4, 0x11)
    image = bytearray(path.read_bytes())
    host = HostImageFile()
    host.media_attached(file_holds_media=True)
    host.flush(path, image)
    # The file's own bytes stay, since nothing is written.
    path.write_bytes(_sector(0x22) * 4)

    host.flush(path, image)

    assert path.read_bytes() == _sector(0x22) * 4
    assert fsyncs == []


@pytest.mark.parametrize("on_disk", (0, 2, 4, 6), ids=("missing", "shorter", "same", "longer"))
def test_media_the_file_may_not_hold_is_written_whole_once(tmp_path, fsyncs, on_disk):
    path = tmp_path / "media.img"
    if on_disk:
        path.write_bytes(_sector(0x33) * on_disk)
    image = bytearray(b"".join(_sector(0x40 + sector) for sector in range(4)))
    host = HostImageFile()
    host.media_attached(file_holds_media=False)

    host.flush(path, image)
    assert path.read_bytes() == image
    assert len(fsyncs) == 1

    # Written whole, the file holds the media: the next flush has nothing to do.
    host.flush(path, image)
    assert len(fsyncs) == 1


def test_a_file_that_went_missing_or_changed_size_is_written_whole(tmp_path):
    path = _file(tmp_path, 4, 0x00)
    image = bytearray(path.read_bytes())
    host = HostImageFile()
    host.media_attached(file_holds_media=True)
    image[0:SECTOR_SIZE] = _sector(0x55)
    host.mark(0)
    path.unlink()

    host.flush(path, image)
    assert path.read_bytes() == image

    path.write_bytes(image[:SECTOR_SIZE])
    image[SECTOR_SIZE:2 * SECTOR_SIZE] = _sector(0x66)
    host.mark(1)

    host.flush(path, image)
    assert path.read_bytes() == image


def test_a_failed_flush_keeps_its_marks_for_the_next(tmp_path, monkeypatch):
    path = _file(tmp_path, 4, 0x00)
    image = bytearray(path.read_bytes())
    host = HostImageFile()
    host.media_attached(file_holds_media=True)
    image[3 * SECTOR_SIZE:] = _sector(0x77)
    host.mark(3)
    calls = []
    real = shared.storage.os.fsync

    def failing_once(fd):
        calls.append(fd)
        if len(calls) == 1:
            raise OSError("host unavailable")
        real(fd)

    monkeypatch.setattr(shared.storage.os, "fsync", failing_once)
    with pytest.raises(OSError, match="host unavailable"):
        host.flush(path, image)
    path.write_bytes(_sector(0x00) * 4)

    host.flush(path, image)

    assert len(calls) == 2
    assert path.read_bytes() == image


def test_detached_media_has_nothing_to_write(tmp_path, fsyncs):
    path = _file(tmp_path, 2, 0x00)
    host = HostImageFile()
    host.media_attached(file_holds_media=False)
    host.mark(1)
    host.media_detached()

    host.flush(path, bytearray(_sector(0x99) * 2))

    assert path.read_bytes() == _sector(0x00) * 2
    assert fsyncs == []


def test_marked_sectors_are_written_as_runs_of_consecutive_sectors():
    assert list(_runs([])) == []
    assert list(_runs([5])) == [(5, 1)]
    assert list(_runs([1, 2, 3, 7, 8, 10])) == [(1, 3), (7, 2), (10, 1)]
