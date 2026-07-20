"""Contract tests for KDOS block devices, volumes, and partition discovery.

The partition images in this module are built byte-for-byte in Python.  The
tests therefore exercise the public KDOS discovery words against external
on-media structures rather than constructing fixtures with KDOS internals.
"""

from __future__ import annotations

import binascii
import os
import re
import struct
import tempfile
from collections.abc import Callable, Sequence

from devices import SECTOR_SIZE, STORAGE_CMD_WRITE
from tests.test_system import (
    KDOS_TEST_EXT_MEM_MIB,
    _KDOSTestBase,
    _next_line_chunk,
    capture_uart,
    make_system,
    uart_text,
)


BLOCK_DEVICE_SIZE = 128
VOLUME_SIZE = 144
PART_WORKSPACE_MIN = 5120


def _patterned_image(sectors: int) -> bytearray:
    """Return an image whose sector number is visible in every byte."""
    image = bytearray(sectors * SECTOR_SIZE)
    for lba in range(sectors):
        start = lba * SECTOR_SIZE
        image[start:start + SECTOR_SIZE] = bytes([lba & 0xFF]) * SECTOR_SIZE
    return image


def _put_mbr_entry(
    sector: bytearray,
    index: int,
    part_type: int,
    first_lba: int,
    sectors: int,
    *,
    bootable: bool = False,
) -> None:
    offset = 446 + index * 16
    sector[offset] = 0x80 if bootable else 0
    sector[offset + 1:offset + 4] = b"\x00\x02\x00"
    sector[offset + 4] = part_type
    sector[offset + 5:offset + 8] = b"\xFE\xFF\xFF"
    struct.pack_into("<II", sector, offset + 8, first_lba, sectors)


def _mbr_image(
    total_sectors: int = 256,
    entries: Sequence[tuple[int, int, int]] = (
        (0x83, 8, 16),
        (0x0C, 40, 24),
    ),
) -> bytearray:
    image = _patterned_image(total_sectors)
    mbr = bytearray(SECTOR_SIZE)
    for index, (part_type, first_lba, sectors) in enumerate(entries):
        _put_mbr_entry(
            mbr, index, part_type, first_lba, sectors,
            bootable=index == 0,
        )
    mbr[510:512] = b"\x55\xAA"
    image[:SECTOR_SIZE] = mbr
    return image


def _protective_mbr(total_sectors: int) -> bytes:
    mbr = bytearray(SECTOR_SIZE)
    _put_mbr_entry(
        mbr, 0, 0xEE, 1, min(total_sectors - 1, 0xFFFF_FFFF),
    )
    mbr[510:512] = b"\x55\xAA"
    return bytes(mbr)


def _gpt_header(
    *,
    current_lba: int,
    backup_lba: int,
    first_usable: int,
    last_usable: int,
    disk_guid: bytes,
    entries_lba: int,
    entry_count: int,
    entry_size: int,
    entries_crc: int,
) -> bytes:
    header = bytearray(SECTOR_SIZE)
    struct.pack_into(
        "<8sIIIIQQQQ16sQIII",
        header,
        0,
        b"EFI PART",
        0x0001_0000,
        92,
        0,
        0,
        current_lba,
        backup_lba,
        first_usable,
        last_usable,
        disk_guid,
        entries_lba,
        entry_count,
        entry_size,
        entries_crc,
    )
    struct.pack_into("<I", header, 16, binascii.crc32(header[:92]) & 0xFFFF_FFFF)
    return bytes(header)


def _gpt_image(
    total_sectors: int = 256,
    partitions: Sequence[tuple[int, int]] = ((40, 55), (80, 111)),
    *,
    entry_count: int = 32,
    duplicate_unique_guid: bool = False,
) -> bytearray:
    """Build a primary and backup GPT with valid header/table CRCs."""
    entry_size = 128
    table_bytes = entry_count * entry_size
    table_sectors = (table_bytes + SECTOR_SIZE - 1) // SECTOR_SIZE
    primary_entries_lba = 2
    backup_header_lba = total_sectors - 1
    backup_entries_lba = backup_header_lba - table_sectors
    first_usable = max(34, primary_entries_lba + table_sectors)
    last_usable = min(total_sectors - 34, backup_entries_lba - 1)
    if first_usable > last_usable:
        raise ValueError("image is too small for its GPT metadata")

    image = _patterned_image(total_sectors)
    image[:SECTOR_SIZE] = _protective_mbr(total_sectors)

    entries = bytearray(table_bytes)
    type_guids = (
        bytes.fromhex("A2A0D0EBE5B9334487C068B6B72699C7"),
        bytes.fromhex("AF3DC60F838472478E793D69D8477DE4"),
    )
    unique_guids = (
        bytes.fromhex("00112233445566778899AABBCCDDEEFF"),
        bytes.fromhex("102132435465768798A9BACBDCEDFE0F"),
    )
    for index, (first_lba, last_lba) in enumerate(partitions):
        offset = index * entry_size
        entries[offset:offset + 16] = type_guids[index % len(type_guids)]
        unique_index = 0 if duplicate_unique_guid else index % len(unique_guids)
        entries[offset + 16:offset + 32] = unique_guids[unique_index]
        struct.pack_into("<QQQ", entries, offset + 32, first_lba, last_lba, 0)
        name = f"part-{index + 1}".encode("utf-16le")
        entries[offset + 56:offset + 56 + len(name)] = name

    entries_crc = binascii.crc32(entries) & 0xFFFF_FFFF
    disk_guid = bytes.fromhex("78563412BC9AF0DE1122334455667788")
    primary_header = _gpt_header(
        current_lba=1,
        backup_lba=backup_header_lba,
        first_usable=first_usable,
        last_usable=last_usable,
        disk_guid=disk_guid,
        entries_lba=primary_entries_lba,
        entry_count=entry_count,
        entry_size=entry_size,
        entries_crc=entries_crc,
    )
    backup_header = _gpt_header(
        current_lba=backup_header_lba,
        backup_lba=1,
        first_usable=first_usable,
        last_usable=last_usable,
        disk_guid=disk_guid,
        entries_lba=backup_entries_lba,
        entry_count=entry_count,
        entry_size=entry_size,
        entries_crc=entries_crc,
    )

    primary_table = primary_entries_lba * SECTOR_SIZE
    backup_table = backup_entries_lba * SECTOR_SIZE
    image[primary_table:primary_table + table_bytes] = entries
    image[backup_table:backup_table + table_bytes] = entries
    image[SECTOR_SIZE:2 * SECTOR_SIZE] = primary_header
    image[backup_header_lba * SECTOR_SIZE:(backup_header_lba + 1) * SECTOR_SIZE] = (
        backup_header
    )
    return image


def _crc32_preserving_variant(data: bytes | bytearray) -> bytearray:
    """Return byte-different data with the same IEEE CRC-32.

    CRC-32 is linear over bit differences.  Sixty-four candidate bits in the
    final eight bytes therefore contain a nonzero dependency in the 32-bit
    output space.  Gaussian elimination finds that dependency deterministically.
    """
    original = bytearray(data)
    if len(original) < 8:
        raise ValueError("CRC-preserving variant requires at least eight bytes")
    original_crc = binascii.crc32(original) & 0xFFFF_FFFF
    basis: dict[int, tuple[int, int]] = {}
    tail = len(original) - 8

    for bit_index in range(64):
        candidate = original.copy()
        candidate[tail + bit_index // 8] ^= 1 << (bit_index % 8)
        influence = (
            (binascii.crc32(candidate) & 0xFFFF_FFFF) ^ original_crc
        )
        combination = 1 << bit_index
        while influence:
            pivot = influence.bit_length() - 1
            if pivot not in basis:
                basis[pivot] = (influence, combination)
                break
            prior_influence, prior_combination = basis[pivot]
            influence ^= prior_influence
            combination ^= prior_combination
        else:
            variant = original.copy()
            for selected_bit in range(64):
                if combination & (1 << selected_bit):
                    variant[tail + selected_bit // 8] ^= (
                        1 << (selected_bit % 8)
                    )
            assert variant != original
            assert binascii.crc32(variant) & 0xFFFF_FFFF == original_crc
            return variant

    raise AssertionError("failed to find the guaranteed CRC-32 dependency")


def _gpt_image_with_mirror_disagreement() -> bytearray:
    """Build CRC-valid GPT arrays that disagree byte-for-byte."""
    image = _gpt_image()
    total_sectors = len(image) // SECTOR_SIZE
    backup_header = (total_sectors - 1) * SECTOR_SIZE
    backup_entries_lba = struct.unpack_from("<Q", image, backup_header + 72)[0]
    entry_count = struct.unpack_from("<I", image, backup_header + 80)[0]
    entry_size = struct.unpack_from("<I", image, backup_header + 84)[0]
    declared_crc = struct.unpack_from("<I", image, backup_header + 88)[0]
    array_start = backup_entries_lba * SECTOR_SIZE
    array_end = array_start + entry_count * entry_size
    backup_array = _crc32_preserving_variant(image[array_start:array_end])
    assert binascii.crc32(backup_array) & 0xFFFF_FFFF == declared_crc
    image[array_start:array_end] = backup_array
    return image


def _marked_int(text: str, marker: str) -> int:
    matches = re.findall(re.escape(marker) + r"\s*(-?\d+)", text)
    if not matches:
        raise AssertionError(f"missing integer marker {marker!r} in:\n{text}")
    return int(matches[-1])


class TestKDOSBlockVolume(_KDOSTestBase):
    """Executable contract for the KDOS storage object layer."""

    def _write_image(self, image: bytes | bytearray) -> str:
        handle = tempfile.NamedTemporaryFile(suffix=".img", delete=False)
        try:
            handle.write(image)
            return handle.name
        finally:
            handle.close()
            self.addCleanup(
                lambda path=handle.name: (
                    os.path.exists(path) and os.unlink(path)
                )
            )

    def _run_live(
        self,
        lines: list[str],
        *,
        storage_image: str,
        configure: Callable | None = None,
        max_steps: int = 50_000_000,
    ):
        """Run from the shared KDOS snapshot while retaining the system.

        `_run_kdos` is used for ordinary cases.  This variant exists solely
        for the media-swap race test, where Python must replace the backing
        medium exactly when KDOS submits the guarded controller command.
        """
        mem_bytes, ext_mem_bytes, cpu_state = self._snapshot_data()
        system = make_system(
            ram_kib=1024,
            storage_image=storage_image,
            ext_mem_mib=KDOS_TEST_EXT_MEM_MIB,
        )
        uart = capture_uart(system)
        system.cpu.mem[:len(mem_bytes)] = mem_bytes
        system._ext_mem[:len(ext_mem_bytes)] = ext_mem_bytes
        self._restore_cpu_state(system.cpu, cpu_state)
        r19 = system.cpu.regs[19]
        if r19 and r19 < len(mem_bytes):
            system.uart._tx_ring_base = r19
        if configure is not None:
            configure(system)

        payload = ("\n".join(lines) + "\nBYE\n").encode()
        position = 0
        steps = 0
        while steps < max_steps:
            if system.cpu.halted:
                break
            if system.cpu.idle and not system.uart.has_rx_data:
                if position < len(payload):
                    chunk = _next_line_chunk(payload, position)
                    system.uart.inject_input(chunk)
                    position += len(chunk)
                else:
                    break
                continue
            batch = system.run_batch(min(100_000, max_steps - steps))
            steps += max(batch, 1)
        return system, uart_text(uart)

    def test_storage_open_preserves_its_public_stack_effect(self):
        """Successful singleton binding returns one ior and leaks no cells."""
        path = self._write_image(bytes(8 * SECTOR_SIZE))
        text = self._run_kdos([
            ('77 STORAGE-OPEN ." [[STORAGE-OPEN-IOR]]" . '
             '." [[STORAGE-OPEN-SENTINEL]]" .'),
            'SYSTEM-RAW-VOLUME VOL-CLOSE DROP SYSTEM-BD BD-CLOSE DROP',
        ], storage_image=path)
        self.assertEqual(_marked_int(text, "[[STORAGE-OPEN-IOR]]"), 0)
        self.assertEqual(_marked_int(text, "[[STORAGE-OPEN-SENTINEL]]"), 77)

    def test_block_device_open_io_exact_end_and_range_errors(self):
        image = _patterned_image(16)
        path = self._write_image(image)
        text = self._run_kdos([
            "CREATE T-BD /BLOCK-DEVICE ALLOT",
            "CREATE T-BUF SECTOR ALLOT",
            '/BLOCK-DEVICE ." [[ABI-BD-SIZE]]" .',
            '/VOLUME ." [[ABI-VOL-SIZE]]" .',
            'PART-WORKSPACE-MIN ." [[ABI-PART-WORK]]" .',
            'T-BD BD-OPEN ." [[BD-OPEN]]" .',
            'T-BD BD-VALID? ." [[BD-VALID]]" .',
            'T-BD BD.SECTOR-SIZE ." [[BD-SIZE]]" .',
            'T-BD BD.SECTORS ." [[BD-SECTORS]]" .',
            ('T-BUF 15 1 T-BD BD-READ SWAP '
             '." [[BD-END-C]]" . ." [[BD-END-I]]" .'),
            'T-BUF C@ ." [[BD-END-BYTE]]" .',
            ('T-BUF 16 1 T-BD BD-READ SWAP '
             '." [[BD-OOB-C]]" . DUP IOR>CODE ." [[BD-OOB-CODE]]" . '
             'IOR>DOMAIN ." [[BD-OOB-DOM]]" .'),
            ('T-BUF 0 0 T-BD BD-READ SWAP '
             '." [[BD-ZERO-C]]" . IOR>CODE ." [[BD-ZERO-CODE]]" .'),
            ('T-BUF 0xFFFFFFFFFFFFFFFF 2 T-BD BD-READ SWAP '
             '." [[BD-WRAP-C]]" . IOR>CODE ." [[BD-WRAP-CODE]]" .'),
            'T-BD BD-CLOSE ." [[BD-CLOSE]]" .',
        ], storage_image=path)

        self.assertEqual(_marked_int(text, "[[ABI-BD-SIZE]]"), BLOCK_DEVICE_SIZE)
        self.assertEqual(_marked_int(text, "[[ABI-VOL-SIZE]]"), VOLUME_SIZE)
        self.assertEqual(_marked_int(text, "[[ABI-PART-WORK]]"), PART_WORKSPACE_MIN)
        self.assertEqual(_marked_int(text, "[[BD-OPEN]]"), 0)
        self.assertEqual(_marked_int(text, "[[BD-VALID]]"), -1)
        self.assertEqual(_marked_int(text, "[[BD-SIZE]]"), SECTOR_SIZE)
        self.assertEqual(_marked_int(text, "[[BD-SECTORS]]"), 16)
        self.assertEqual(_marked_int(text, "[[BD-END-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[BD-END-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[BD-END-BYTE]]"), 15)
        self.assertEqual(_marked_int(text, "[[BD-OOB-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[BD-OOB-CODE]]"), 18)
        self.assertEqual(_marked_int(text, "[[BD-OOB-DOM]]"), 1)
        self.assertEqual(_marked_int(text, "[[BD-ZERO-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[BD-ZERO-CODE]]"), 18)
        self.assertEqual(_marked_int(text, "[[BD-WRAP-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[BD-WRAP-CODE]]"), 18)
        self.assertEqual(_marked_int(text, "[[BD-CLOSE]]"), 0)

    def test_block_device_write_read_flush_and_reference_lifetime(self):
        path = self._write_image(bytes(12 * SECTOR_SIZE))
        text = self._run_kdos([
            "CREATE T-BD /BLOCK-DEVICE ALLOT",
            "CREATE T-VOL /VOLUME ALLOT",
            "CREATE T-BUF SECTOR ALLOT",
            'T-BD BD-OPEN ." [[IO-OPEN]]" .',
            "T-BUF SECTOR 0xA6 FILL",
            ('T-BUF 11 1 T-BD BD-WRITE SWAP '
             '." [[IO-WRITE-C]]" . ." [[IO-WRITE-I]]" .'),
            'T-BD BD-FLUSH ." [[IO-FLUSH]]" .',
            "T-BUF SECTOR 0 FILL",
            ('T-BUF 11 1 T-BD BD-READ SWAP '
             '." [[IO-READ-C]]" . ." [[IO-READ-I]]" .'),
            'T-BUF C@ ." [[IO-BYTE]]" .',
            'T-BD T-VOL VOL-RAW ." [[IO-RAW]]" .',
            'T-VOL VOL-FLUSH ." [[IO-VOL-FLUSH]]" .',
            'T-BD BD-CLOSE IOR>CODE ." [[IO-BUSY-CODE]]" .',
            'T-VOL VOL-CLOSE ." [[IO-VOL-CLOSE]]" .',
            'T-BD BD-CLOSE ." [[IO-BD-CLOSE]]" .',
        ], storage_image=path)

        self.assertEqual(_marked_int(text, "[[IO-OPEN]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-WRITE-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[IO-WRITE-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-FLUSH]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-READ-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[IO-READ-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-BYTE]]"), 0xA6)
        self.assertEqual(_marked_int(text, "[[IO-RAW]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-VOL-FLUSH]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-BUSY-CODE]]"), 24)
        self.assertEqual(_marked_int(text, "[[IO-VOL-CLOSE]]"), 0)
        self.assertEqual(_marked_int(text, "[[IO-BD-CLOSE]]"), 0)
        with open(path, "rb") as handle:
            handle.seek(11 * SECTOR_SIZE)
            self.assertEqual(handle.read(SECTOR_SIZE), bytes([0xA6]) * SECTOR_SIZE)

    def test_raw_and_bounded_volume_translation_and_ranges(self):
        path = self._write_image(_patterned_image(20))
        text = self._run_kdos([
            "CREATE T-BD /BLOCK-DEVICE ALLOT",
            "CREATE T-RAW /VOLUME ALLOT",
            "CREATE T-SLICE /VOLUME ALLOT",
            "CREATE T-BUF SECTOR ALLOT",
            'T-BD BD-OPEN ." [[VOL-OPEN]]" .',
            'T-BD T-RAW VOL-RAW ." [[VOL-RAW]]" .',
            'T-RAW VOL-VALID? ." [[VOL-RAW-VALID]]" .',
            'T-RAW VOL.BASE ." [[VOL-RAW-BASE]]" .',
            'T-RAW VOL.SECTORS ." [[VOL-RAW-LEN]]" .',
            'T-RAW VOL.SCHEME ." [[VOL-RAW-SCHEME]]" .',
            ('T-BUF 7 1 T-RAW VOL-READ SWAP '
             '." [[VOL-RAW-C]]" . ." [[VOL-RAW-I]]" .'),
            'T-BUF C@ ." [[VOL-RAW-BYTE]]" .',
            '5 3 VOL-SCHEME-MBR 2 T-BD T-SLICE VOL-SLICE ." [[VOL-SLICE]]" .',
            'T-SLICE VOL-VALID? ." [[VOL-SLICE-VALID]]" .',
            'T-SLICE VOL.BASE ." [[VOL-SLICE-BASE]]" .',
            'T-SLICE VOL.SECTORS ." [[VOL-SLICE-LEN]]" .',
            "T-BUF SECTOR 0 FILL",
            ('T-BUF 2 1 T-SLICE VOL-READ SWAP '
             '." [[VOL-END-C]]" . ." [[VOL-END-I]]" .'),
            'T-BUF C@ ." [[VOL-END-BYTE]]" .',
            "T-BUF SECTOR 0xD4 FILL",
            ('T-BUF 1 1 T-SLICE VOL-WRITE SWAP '
             '." [[VOL-WRITE-C]]" . ." [[VOL-WRITE-I]]" .'),
            "T-BUF SECTOR 0 FILL",
            ('T-BUF 1 1 T-SLICE VOL-READ SWAP '
             '." [[VOL-WRITTEN-C]]" . ." [[VOL-WRITTEN-I]]" .'),
            'T-BUF C@ ." [[VOL-WRITTEN-BYTE]]" .',
            ('T-BUF 3 1 T-SLICE VOL-READ SWAP '
             '." [[VOL-OOB-C]]" . IOR>CODE ." [[VOL-OOB-CODE]]" .'),
            ('T-BUF 0 0 T-SLICE VOL-READ SWAP '
             '." [[VOL-ZERO-C]]" . IOR>CODE ." [[VOL-ZERO-CODE]]" .'),
            'T-SLICE VOL-CLOSE DROP T-RAW VOL-CLOSE DROP T-BD BD-CLOSE DROP',
        ], storage_image=path)

        self.assertEqual(_marked_int(text, "[[VOL-OPEN]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-RAW]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-VALID]]"), -1)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-BASE]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-LEN]]"), 20)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-SCHEME]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-RAW-BYTE]]"), 7)
        self.assertEqual(_marked_int(text, "[[VOL-SLICE]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-SLICE-VALID]]"), -1)
        self.assertEqual(_marked_int(text, "[[VOL-SLICE-BASE]]"), 5)
        self.assertEqual(_marked_int(text, "[[VOL-SLICE-LEN]]"), 3)
        self.assertEqual(_marked_int(text, "[[VOL-END-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[VOL-END-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-END-BYTE]]"), 7)
        self.assertEqual(_marked_int(text, "[[VOL-WRITE-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[VOL-WRITE-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-WRITTEN-C]]"), 1)
        self.assertEqual(_marked_int(text, "[[VOL-WRITTEN-I]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-WRITTEN-BYTE]]"), 0xD4)
        self.assertEqual(_marked_int(text, "[[VOL-OOB-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-OOB-CODE]]"), 18)
        self.assertEqual(_marked_int(text, "[[VOL-ZERO-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[VOL-ZERO-CODE]]"), 18)

    def test_guarded_volume_write_fails_closed_on_acceptance_race(self):
        original = self._write_image(bytes(8 * SECTOR_SIZE))
        replacement_bytes = bytes([0x3C]) * (8 * SECTOR_SIZE)
        replacement = self._write_image(replacement_bytes)
        guarded_effects = {"swapped": False, "dma_reads": 0, "dma_writes": 0}

        def configure(system):
            accept = system.storage._accept_command
            mem_read = system.storage._mem_read
            mem_write = system.storage._mem_write

            def count_mem_read(address):
                guarded_effects["dma_reads"] += 1
                return mem_read(address)

            def count_mem_write(address, value):
                guarded_effects["dma_writes"] += 1
                return mem_write(address, value)

            def swap_before_accept(command, *, expected_generation=None):
                if (
                    not guarded_effects["swapped"]
                    and command == STORAGE_CMD_WRITE
                    and expected_generation is not None
                ):
                    guarded_effects["swapped"] = True
                    system.storage.swap_image(replacement, save_current=False)
                return accept(command, expected_generation=expected_generation)

            system.storage._mem_read = count_mem_read
            system.storage._mem_write = count_mem_write
            system.storage._accept_command = swap_before_accept

        system, text = self._run_live([
            "CREATE T-BD /BLOCK-DEVICE ALLOT",
            "CREATE T-VOL /VOLUME ALLOT",
            "CREATE T-BUF SECTOR ALLOT",
            'T-BD BD-OPEN ." [[STALE-OPEN]]" .',
            'T-BD T-VOL VOL-RAW ." [[STALE-RAW]]" .',
            "T-BUF SECTOR 0xA5 FILL",
            ('T-BUF 2 1 T-VOL VOL-WRITE SWAP '
             '." [[STALE-C]]" . DUP IOR>RAW ." [[STALE-RAW-CODE]]" . '
             'DUP IOR-STALE? ." [[STALE-FLAG]]" . ." [[STALE-IOR]]" .'),
            'T-VOL VOL-STALE? ." [[STALE-VOL]]" .',
            'T-BD BD-STALE? ." [[STALE-BD]]" .',
            'T-VOL VOL-FLUSH DUP IOR>RAW ." [[STALE-FLUSH-RAW]]" . '
            'IOR-STALE? ." [[STALE-FLUSH-FLAG]]" .',
        ], storage_image=original, configure=configure)

        self.assertTrue(guarded_effects["swapped"])
        self.assertEqual(_marked_int(text, "[[STALE-OPEN]]"), 0)
        self.assertEqual(_marked_int(text, "[[STALE-RAW]]"), 0)
        self.assertEqual(_marked_int(text, "[[STALE-C]]"), 0)
        self.assertEqual(_marked_int(text, "[[STALE-RAW-CODE]]"), 11)
        self.assertEqual(_marked_int(text, "[[STALE-FLAG]]"), -1)
        self.assertNotEqual(_marked_int(text, "[[STALE-IOR]]"), 0)
        self.assertEqual(_marked_int(text, "[[STALE-VOL]]"), -1)
        self.assertEqual(_marked_int(text, "[[STALE-BD]]"), -1)
        self.assertEqual(_marked_int(text, "[[STALE-FLUSH-RAW]]"), 11)
        self.assertEqual(_marked_int(text, "[[STALE-FLUSH-FLAG]]"), -1)
        self.assertEqual(guarded_effects["dma_reads"], 0)
        self.assertEqual(guarded_effects["dma_writes"], 0)
        self.assertEqual(bytes(system.storage._image_data), replacement_bytes)
        with open(replacement, "rb") as handle:
            self.assertEqual(handle.read(), replacement_bytes)

    def _scan(self, image: bytes | bytearray, scanner: str, max_volumes: int = 4):
        path = self._write_image(image)
        text = self._run_kdos([
            "CREATE P-BD /BLOCK-DEVICE ALLOT",
            f"CREATE P-VOLS {max_volumes * VOLUME_SIZE} ALLOT",
            "CREATE P-WORK PART-WORKSPACE-MIN ALLOT",
            'P-BD BD-OPEN ." [[PART-OPEN]]" .',
            (f"P-BD P-VOLS {max_volumes} P-WORK PART-WORKSPACE-MIN {scanner} "
             'SWAP ." [[PART-COUNT]]" . DUP IOR>CODE ." [[PART-CODE]]" . '
             'DUP IOR>DOMAIN ." [[PART-DOMAIN]]" . ." [[PART-IOR]]" .'),
            'P-VOLS VOL.BASE ." [[PART-BASE0]]" .',
            'P-VOLS VOL.SECTORS ." [[PART-LEN0]]" .',
            'P-VOLS VOL.SCHEME ." [[PART-SCHEME0]]" .',
            'P-VOLS VOL.INDEX ." [[PART-INDEX0]]" .',
            'P-VOLS VOL-VALID? ." [[PART-VALID0]]" .',
            'P-VOLS /VOLUME + VOL.BASE ." [[PART-BASE1]]" .',
            'P-VOLS /VOLUME + VOL.SECTORS ." [[PART-LEN1]]" .',
            'P-VOLS /VOLUME + VOL.SCHEME ." [[PART-SCHEME1]]" .',
            'P-VOLS /VOLUME + VOL.INDEX ." [[PART-INDEX1]]" .',
            'P-VOLS /VOLUME + VOL-VALID? ." [[PART-VALID1]]" .',
        ], storage_image=path)
        return text

    def _assert_scan_rejected(
        self,
        image: bytes | bytearray,
        scanner: str,
        expected_code: int,
    ) -> str:
        text = self._scan(image, scanner)
        self.assertEqual(_marked_int(text, "[[PART-COUNT]]"), 0)
        self.assertNotEqual(_marked_int(text, "[[PART-IOR]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-CODE]]"), expected_code)
        self.assertEqual(_marked_int(text, "[[PART-DOMAIN]]"), 4)
        self.assertEqual(_marked_int(text, "[[PART-VALID0]]"), 0)
        return text

    def test_valid_mbr_discovery_and_part_scan_dispatch(self):
        image = _mbr_image()
        for scanner in ("MBR-SCAN", "PART-SCAN"):
            with self.subTest(scanner=scanner):
                text = self._scan(image, scanner)
                self.assertEqual(_marked_int(text, "[[PART-OPEN]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-COUNT]]"), 2)
                self.assertEqual(_marked_int(text, "[[PART-IOR]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-BASE0]]"), 8)
                self.assertEqual(_marked_int(text, "[[PART-LEN0]]"), 16)
                self.assertEqual(_marked_int(text, "[[PART-SCHEME0]]"), 1)
                self.assertEqual(_marked_int(text, "[[PART-INDEX0]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-VALID0]]"), -1)
                self.assertEqual(_marked_int(text, "[[PART-BASE1]]"), 40)
                self.assertEqual(_marked_int(text, "[[PART-LEN1]]"), 24)
                self.assertEqual(_marked_int(text, "[[PART-SCHEME1]]"), 1)
                self.assertEqual(_marked_int(text, "[[PART-INDEX1]]"), 1)
                self.assertEqual(_marked_int(text, "[[PART-VALID1]]"), -1)

    def test_invalid_mbr_is_rejected_without_partial_volume_publication(self):
        image = _mbr_image(entries=((0x83, 8, 16), (0x0C, 250, 16)))
        text = self._assert_scan_rejected(image, "MBR-SCAN", 20)
        self.assertEqual(_marked_int(text, "[[PART-BASE0]]"), 0)

    def test_mbr_overlap_and_extended_container_refusal_are_transactional(self):
        cases = (
            (
                "overlap",
                _mbr_image(entries=((0x83, 8, 16), (0x0C, 16, 24))),
                20,
            ),
            ("extended", _mbr_image(entries=((0x0F, 8, 32),)), 23),
        )
        for label, image, code in cases:
            with self.subTest(policy=label):
                self._assert_scan_rejected(image, "MBR-SCAN", code)

    def test_part_scan_falls_back_to_one_raw_identity_volume(self):
        text = self._scan(_patterned_image(20), "PART-SCAN")
        self.assertEqual(_marked_int(text, "[[PART-OPEN]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-COUNT]]"), 1)
        self.assertEqual(_marked_int(text, "[[PART-IOR]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-BASE0]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-LEN0]]"), 20)
        self.assertEqual(_marked_int(text, "[[PART-SCHEME0]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-INDEX0]]"), 0)
        self.assertEqual(_marked_int(text, "[[PART-VALID0]]"), -1)

    def test_valid_gpt_discovery_and_part_scan_dispatch(self):
        image = _gpt_image()
        for scanner in ("GPT-SCAN", "PART-SCAN"):
            with self.subTest(scanner=scanner):
                text = self._scan(image, scanner)
                self.assertEqual(_marked_int(text, "[[PART-OPEN]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-COUNT]]"), 2)
                self.assertEqual(_marked_int(text, "[[PART-IOR]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-BASE0]]"), 40)
                self.assertEqual(_marked_int(text, "[[PART-LEN0]]"), 16)
                self.assertEqual(_marked_int(text, "[[PART-SCHEME0]]"), 2)
                self.assertEqual(_marked_int(text, "[[PART-INDEX0]]"), 0)
                self.assertEqual(_marked_int(text, "[[PART-VALID0]]"), -1)
                self.assertEqual(_marked_int(text, "[[PART-BASE1]]"), 80)
                self.assertEqual(_marked_int(text, "[[PART-LEN1]]"), 32)
                self.assertEqual(_marked_int(text, "[[PART-SCHEME1]]"), 2)
                self.assertEqual(_marked_int(text, "[[PART-INDEX1]]"), 1)
                self.assertEqual(_marked_int(text, "[[PART-VALID1]]"), -1)

    def test_invalid_gpt_crc_is_rejected_without_partial_volumes(self):
        image = _gpt_image()
        image[SECTOR_SIZE + 16] ^= 0x80
        text = self._assert_scan_rejected(image, "GPT-SCAN", 20)
        self.assertEqual(_marked_int(text, "[[PART-BASE0]]"), 0)

    def test_gpt_reserved_header_tail_must_be_zero(self):
        image = _gpt_image()
        total_sectors = len(image) // SECTOR_SIZE
        image[SECTOR_SIZE + 100] = 0x5A
        image[(total_sectors - 1) * SECTOR_SIZE + 100] = 0x5A
        self._assert_scan_rejected(image, "GPT-SCAN", 20)

    def test_gpt_identity_overlap_and_mirror_disagreement_are_transactional(self):
        cases = (
            ("duplicate-guid", _gpt_image(duplicate_unique_guid=True)),
            (
                "overlap",
                _gpt_image(partitions=((40, 63), (56, 79))),
            ),
            ("mirror-disagreement", _gpt_image_with_mirror_disagreement()),
        )
        for label, image in cases:
            with self.subTest(policy=label):
                self._assert_scan_rejected(image, "GPT-SCAN", 20)

    def test_partition_scan_enforces_capacity_and_workspace_before_publish(self):
        image = _mbr_image()
        path = self._write_image(image)
        text = self._run_kdos([
            "CREATE P-BD /BLOCK-DEVICE ALLOT",
            f"CREATE P-VOLS {2 * VOLUME_SIZE} ALLOT",
            "CREATE P-WORK PART-WORKSPACE-MIN ALLOT",
            "P-BD BD-OPEN DROP",
            ('P-BD P-VOLS 1 P-WORK PART-WORKSPACE-MIN MBR-SCAN SWAP '
             '." [[CAP-COUNT]]" . IOR>CODE ." [[CAP-CODE]]" .'),
            ('P-BD P-VOLS 2 P-WORK PART-WORKSPACE-MIN MBR-SCAN SWAP '
             '." [[VALID-COUNT]]" . ." [[VALID-IOR]]" .'),
            'P-VOLS VOL-VALID? ." [[VALID-BEFORE-WORK]]" .',
            ('P-BD P-VOLS 2 P-WORK PART-WORKSPACE-MIN 1- MBR-SCAN SWAP '
             '." [[WORK-COUNT]]" . IOR>CODE ." [[WORK-CODE]]" .'),
            'P-VOLS VOL-VALID? ." [[ATOMIC-VOL]]" .',
            'P-BD BD.REFS @ ." [[ATOMIC-REFS]]" .',
        ], storage_image=path)
        self.assertEqual(_marked_int(text, "[[CAP-COUNT]]"), 0)
        self.assertEqual(_marked_int(text, "[[CAP-CODE]]"), 21)
        self.assertEqual(_marked_int(text, "[[VALID-COUNT]]"), 2)
        self.assertEqual(_marked_int(text, "[[VALID-IOR]]"), 0)
        self.assertEqual(_marked_int(text, "[[VALID-BEFORE-WORK]]"), -1)
        self.assertEqual(_marked_int(text, "[[WORK-COUNT]]"), 0)
        self.assertEqual(_marked_int(text, "[[WORK-CODE]]"), 22)
        self.assertEqual(_marked_int(text, "[[ATOMIC-VOL]]"), 0)
        self.assertEqual(_marked_int(text, "[[ATOMIC-REFS]]"), 0)
