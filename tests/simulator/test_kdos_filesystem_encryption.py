"""Unchanged-source acceptance for KDOS MP64FS file encryption."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.mp64fs import MP64FS_ENTRY_SIZE
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_FLUSH,
    STORAGE_RESULT_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_application_loading import (
    _evaluate_application_loading,
    _load_application_loading,
)
from tests.simulator.test_kdos_mp64fs_fd_pool import _fd_snapshot, _open
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
)
from tests.simulator.test_kdos_mp64fs_load import _load_mp64fs_load_service
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
    / "kdos-filesystem-encryption-6060-6200.f"
)

FIRST_LINE = 6060
LAST_LINE = 6200
SLICE_BYTES = 5_298
SLICE_SHA256 = (
    "35a8f33b51da4e3a319f193e0c709a876207f940923637d0f56b0f8160c7f574"
)
SLICE_GIT_BLOB = "ed442875e780976b10553721137e515e3742ddcb"

SOURCE_LEDGER = (
    ("CREATE", b"FS-KEY"),
    ("CREATE", b"FS-IV"),
    ("CONSTANT", b"F-ENC-FLAG"),
    ("VARIABLE", b"_FE-DESC"),
    ("VARIABLE", b"_FE-USED"),
    ("VARIABLE", b"_FE-PAD"),
    ("VARIABLE", b"_FE-SECS"),
    ("VARIABLE", b"_FE-BUF1"),
    ("VARIABLE", b"_FE-BUF2"),
    (":", b"FS-KEY!"),
    (":", b"_FE-MKIV"),
    (":", b"ENCRYPTED?"),
    (":", b"_FE-SET-ENC"),
    (":", b"_FE-CLR-ENC"),
    (":", b"FENCRYPT"),
    (":", b"FDECRYPT"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)
SCRATCH_VARIABLES = (
    "_FE-DESC",
    "_FE-USED",
    "_FE-PAD",
    "_FE-SECS",
    "_FE-BUF1",
    "_FE-BUF2",
)

FILE_SLOT = 6
FILE_START = 14
FILE_SECTORS = 2
FILE_FLAGS = 0xA1
ENCRYPTED_FLAGS = FILE_FLAGS | 4
KEY = bytes(range(32))
WRONG_KEY = bytes((0xFF,)) * 32
SLOT_IV = bytes((FILE_SLOT,)) + bytes(11)
PLAINTEXT = bytes((index * 17 + 3) & 0xFF for index in range(600))
PADDED_PLAINTEXT = PLAINTEXT + bytes(608 - len(PLAINTEXT))

# Independent cryptography AESGCM oracle for KEY, SLOT_IV, and the zero-padded
# 608-byte plaintext above.  The final 16 bytes are the authentication tag.
CIPHERTEXT_AND_TAG = bytes.fromhex(
    "45dfa9da1fa74421fe8465be40f4256607f3a303334f1367b5057f696552ae97"
    "54ff8d2095d1607f568356f7aa588d3c61c06b2737f93870b7ce4b41d25dbdf2"
    "8b58b0793145f71df725d4f09272c13302dae7d94193389b65e4d85c40d57196"
    "ee57271c16d91dabb22273f013750ad209ac0f106e57c7c04644f990924acbf27"
    "589d87d3b146c926c24582f1cd26890c23ba811264c193f897f393c54fb444db0"
    "8c25a5fcd4b4e7145b5bd4de1e2789a01d1ea7f2fbc826a8fcdf0b7b0d4fa79d"
    "257d6e822e2b636a16aed7de38ee85ae3c6f57433d5829049818f78cc0818a5d"
    "09328c4550a5fa8591b3a376fae9dc1223d29bc145832b5eb8a7fa717788fd612"
    "fa599233ce54ef8f4ee33bc8c1baeadab413677dd876fc111f5171c871e4403b5"
    "8b5c821f7c701072eed96cb25647f17e16263eb7eb2f1be467ab06071569b55aa"
    "569349658c086a7bea0013db44f173595c9b8d45640e37191241dec1fd1b3f458"
    "a4dc69ba7ee49ee75a4d733b1e642c493c983c4f901ecee9e40db02b8d9cb4dc"
    "f38c41ce9eece5b6597e75fe3e38345a404ed35415a0e2a0f429f0447dc9f5d6"
    "e09e2c9b3b67b8886ce3f00c383ff03cf4a742110b460c952fcf358325815197d"
    "188d3cce4d809a900ce35f8acc2418fa5ee182857aa5fb9c7349f3b82b315e144"
    "385bcd6ca80d7a87a4915760b821f1cc897b88cd74da35e3ed0f426f9ae9af323"
    "82ff32dd68b72187739ddb2c43670c2d8e0ff978f13098c57c7a9b2217c510e85"
    "5fa6614723fa1d64caff5a28c97ea1da29d1b7385885ef3f4802f874d0e11b822"
    "42c5fb7ef371024bb03f04db839e30cba506b45935947ce11962c25804ba0dea0"
    "9a89682021487e71d0f5"
)
CIPHERTEXT_TAG_SHA256 = (
    "300da0ed8bc26ab59c8c4c824bb20e30404e4244a796b35726d9d33f2a8fab82"
)
ENCRYPTED_SPAN = CIPHERTEXT_AND_TAG + bytes(
    FILE_SECTORS * SECTOR_SIZE - len(CIPHERTEXT_AND_TAG)
)
ENCRYPTED_SPAN_SHA256 = (
    "4ba2699209001201da30cf949e7faa663317e63c707f8affab3e62e739d9f4b6"
)
PLAINTEXT_SPAN = PLAINTEXT + bytes(
    FILE_SECTORS * SECTOR_SIZE - len(PLAINTEXT)
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ =====================================================================\n"
    return source


def _evaluate_filesystem_encryption(
    runtime: MegaForthRuntime,
) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_filesystem_encryption(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_filesystem_encryption(_load_application_loading(image))


def _load_filesystem_encryption_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_filesystem_encryption(
        _evaluate_application_loading(_load_mp64fs_load_service(storage))
    )


def _canonical_image(*, encrypted: bool) -> tuple[bytearray, int]:
    # This source only addresses F.START/F.MAX.  Keep the admitted behavior on
    # one contiguous primary extent rather than implying secondary support.
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        FILE_SLOT,
        name=b"secret.bin\0",
        start=FILE_START,
        count=FILE_SECTORS,
        used=len(PLAINTEXT),
        entry_type=5,
    )
    image[entry_offset + 33] = ENCRYPTED_FLAGS if encrypted else FILE_FLAGS
    start = FILE_START * SECTOR_SIZE
    image[start : start + FILE_SECTORS * SECTOR_SIZE] = (
        ENCRYPTED_SPAN if encrypted else PLAINTEXT_SPAN
    )
    return image, entry_offset


def _install_key(
    runtime: MegaForthRuntime,
    key: bytes,
    *,
    name: str,
) -> int:
    source = runtime.define_created(name, initial_body=key)
    assert _execute(runtime, "FS-KEY!", source.body_address) == ()
    key_address = _execute(runtime, "FS-KEY")[0]
    assert runtime.memory.read_bytes(key_address, 32) == key
    return key_address


def _cache_with_flags(
    cache: tuple[object, ...],
    flags: int,
    *,
    slot: int = FILE_SLOT,
) -> tuple[object, ...]:
    changed = list(cache)
    directory = bytearray(changed[5])
    directory[slot * MP64FS_ENTRY_SIZE + 33] = flags
    changed[5] = bytes(directory)
    return tuple(changed)


def _file_span(media: bytes) -> bytes:
    start = FILE_START * SECTOR_SIZE
    return media[start : start + FILE_SECTORS * SECTOR_SIZE]


def test_filesystem_encryption_slice_is_exact_and_load_time_pure() -> None:
    runtime = _load_application_loading(_formatted_image())
    _execute(runtime, "MPU-BASE!", 0x1111)
    _execute(runtime, "MPU-LIMIT!", 0x2222)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime = _evaluate_filesystem_encryption(runtime)

    assert len(SOURCE_LEDGER) == 16
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _constant(runtime, "F-ENC-FLAG") == 4

    sized_bodies = (
        ("FS-KEY", "FS-IV", 32),
        ("FS-IV", "F-ENC-FLAG", 12),
        ("_FE-DESC", "_FE-USED", 8),
        ("_FE-USED", "_FE-PAD", 8),
        ("_FE-PAD", "_FE-SECS", 8),
        ("_FE-SECS", "_FE-BUF1", 8),
        ("_FE-BUF1", "_FE-BUF2", 8),
        ("_FE-BUF2", "FS-KEY!", 8),
    )
    assert sum(size for _name, _following, size in sized_bodies) == 92
    for name, following, size in sized_bodies:
        word = runtime.find(name)
        next_word = runtime.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == size

    # VARIABLE initializes its cells.  FS-KEY and FS-IV are bare ALLOT bodies,
    # so this test deliberately makes no source-level zero-initialization claim.
    assert all(_variable(runtime, name) == 0 for name in SCRATCH_VARIABLES)
    assert (runtime.mpu_base, runtime.mpu_limit) == (0x1111, 0x2222)
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_key_copy_slot_iv_and_flag_updates_are_exact_and_preserve_other_bits() -> None:
    image, _entry_offset = _canonical_image(encrypted=False)
    runtime = _load_filesystem_encryption(image)
    _mount(runtime)
    descriptor = _open(runtime, "secret.bin")
    assert descriptor != 0
    descriptor_before = _fd_snapshot(runtime, descriptor)
    completion_before = runtime.storage.completion
    media_before = runtime.storage.image_bytes

    key_address = _execute(runtime, "FS-KEY")[0]
    runtime.memory.write_bytes(key_address, bytes((0xCC,)) * 32)
    key_source = runtime.define_created(
        "EXACT-FS-KEY-SOURCE",
        initial_body=KEY + b"source-tail",
    )
    assert _execute(runtime, "FS-KEY!", key_source.body_address) == ()
    assert runtime.memory.read_bytes(key_address, 32) == KEY
    assert runtime.memory.read_bytes(key_source.body_address, 43) == (
        KEY + b"source-tail"
    )

    iv_address = _execute(runtime, "FS-IV")[0]
    runtime.memory.write_bytes(iv_address, bytes((0xA5,)) * 12)
    assert _execute(runtime, "_FE-MKIV", descriptor) == ()
    assert runtime.memory.read_bytes(iv_address, 12) == SLOT_IV

    entry = _execute(runtime, "DIRENT", FILE_SLOT)[0]
    entry_before = runtime.memory.read_bytes(entry, MP64FS_ENTRY_SIZE)
    assert entry_before[33] == FILE_FLAGS
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (0,)
    assert _execute(runtime, "_FE-SET-ENC", descriptor) == ()
    expected_set = bytearray(entry_before)
    expected_set[33] = ENCRYPTED_FLAGS
    assert runtime.memory.read_bytes(entry, MP64FS_ENTRY_SIZE) == bytes(
        expected_set
    )
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (TRUE,)

    assert _execute(runtime, "_FE-CLR-ENC", descriptor) == ()
    assert runtime.memory.read_bytes(entry, MP64FS_ENTRY_SIZE) == entry_before
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (0,)
    assert _fd_snapshot(runtime, descriptor) == descriptor_before
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b""


def test_real_encrypt_decrypt_and_same_slot_reencrypt_match_external_oracle() -> None:
    plain_image, _entry_offset = _canonical_image(encrypted=False)
    encrypted_image, _encrypted_entry_offset = _canonical_image(encrypted=True)
    expected_plain_media = bytes(plain_image)
    expected_encrypted_media = bytes(encrypted_image)
    assert len(CIPHERTEXT_AND_TAG) == 624
    assert hashlib.sha256(CIPHERTEXT_AND_TAG).hexdigest() == (
        CIPHERTEXT_TAG_SHA256
    )
    assert hashlib.sha256(ENCRYPTED_SPAN).hexdigest() == ENCRYPTED_SPAN_SHA256

    runtime = _load_filesystem_encryption(plain_image)
    _mount(runtime)
    _install_key(runtime, KEY, name="ROUNDTRIP-FS-KEY")
    descriptor = _open(runtime, "secret.bin")
    assert descriptor != 0
    fd_before = _fd_snapshot(runtime, descriptor)
    cache_plain = _mount_snapshot(runtime)[:6]
    cache_encrypted = _cache_with_flags(cache_plain, ENCRYPTED_FLAGS)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion

    assert _execute(runtime, "FENCRYPT", descriptor) == (0,)

    assert _file_span(runtime.storage.image_bytes) == ENCRYPTED_SPAN
    assert runtime.storage.image_bytes == expected_encrypted_media
    assert _mount_snapshot(runtime)[:6] == cache_encrypted
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (TRUE,)
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert runtime.storage.completion == completion_before + 5
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.memory.read_bytes(_execute(runtime, "FS-IV")[0], 12) == SLOT_IV
    assert _variable(runtime, "_FE-DESC") == descriptor
    assert _variable(runtime, "_FE-USED") == len(PLAINTEXT)
    assert _variable(runtime, "_FE-PAD") == len(PADDED_PLAINTEXT)
    assert _variable(runtime, "_FE-SECS") == FILE_SECTORS

    encrypted_state = (
        runtime.storage.image_bytes,
        _mount_snapshot(runtime)[:6],
        _fd_snapshot(runtime, descriptor),
        _execute(runtime, "HEAP-FREE-BYTES")[0],
        runtime.storage.completion,
    )
    assert _execute(runtime, "FENCRYPT", descriptor) == (0,)
    assert (
        runtime.storage.image_bytes,
        _mount_snapshot(runtime)[:6],
        _fd_snapshot(runtime, descriptor),
        _execute(runtime, "HEAP-FREE-BYTES")[0],
        runtime.storage.completion,
    ) == encrypted_state

    assert _execute(runtime, "FDECRYPT", descriptor) == (0,)

    assert _file_span(runtime.storage.image_bytes) == PLAINTEXT_SPAN
    assert runtime.storage.image_bytes == expected_plain_media
    assert _mount_snapshot(runtime)[:6] == cache_plain
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (0,)
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert runtime.storage.completion == completion_before + 10
    assert _diagnostics(runtime) == (0, 12, 0)

    # Slot-derived IVs have no generation component.  Re-encrypting the same
    # plaintext in the same directory slot therefore repeats nonce and bytes.
    assert _execute(runtime, "FENCRYPT", descriptor) == (0,)
    assert runtime.storage.image_bytes == expected_encrypted_media
    assert hashlib.sha256(_file_span(runtime.storage.image_bytes)).hexdigest() == (
        ENCRYPTED_SPAN_SHA256
    )
    assert _mount_snapshot(runtime)[:6] == cache_encrypted
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert runtime.storage.completion == completion_before + 15
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_wrong_key_auth_failure_preserves_ciphertext_flag_cache_and_descriptor() -> None:
    plain_image, _entry_offset = _canonical_image(encrypted=False)
    runtime = _load_filesystem_encryption(plain_image)
    _mount(runtime)
    _install_key(runtime, KEY, name="CORRECT-FS-KEY")
    descriptor = _open(runtime, "secret.bin")
    assert descriptor != 0
    assert _execute(runtime, "FENCRYPT", descriptor) == (0,)
    assert _file_span(runtime.storage.image_bytes) == ENCRYPTED_SPAN

    _install_key(runtime, WRONG_KEY, name="WRONG-FS-KEY")
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    fd_before = _fd_snapshot(runtime, descriptor)
    cache_before = _mount_snapshot(runtime)[:6]
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    assert _execute(runtime, "FDECRYPT", descriptor) == (MASK64,)

    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _mount_snapshot(runtime)[:6] == cache_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before + 1
    assert _diagnostics(runtime) == (0, FILE_SECTORS, 0)
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (TRUE,)
    assert runtime.memory.read_bytes(_execute(runtime, "FS-IV")[0], 12) == SLOT_IV
    assert runtime.drain_uart_output() == b""


def test_noops_empty_files_and_capacity_guard_do_not_allocate_or_touch_media() -> None:
    image = _formatted_image(18)
    entries = (
        (3, b"plain\0", 14, 16, FILE_FLAGS),
        (4, b"empty\0", 15, 0, FILE_FLAGS),
        (5, b"encempty\0", 16, 0, ENCRYPTED_FLAGS),
        (6, b"tight\0", 17, 497, FILE_FLAGS),
    )
    for slot, name, sector, used, flags in entries:
        offset = _write_entry(
            image,
            slot,
            name=name,
            start=sector,
            count=1,
            used=used,
            entry_type=5,
        )
        image[offset + 33] = flags
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = b"P" * 16 + bytes(496)
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = b"T" * 497 + bytes(15)

    runtime = _load_filesystem_encryption(image)
    _mount(runtime)
    descriptors = {
        name: _open(runtime, name)
        for name in ("plain", "empty", "encempty", "tight")
    }
    assert all(descriptors.values())
    fd_before = {
        name: _fd_snapshot(runtime, descriptor)
        for name, descriptor in descriptors.items()
    }
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    cache_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    assert _execute(runtime, "FDECRYPT", descriptors["plain"]) == (0,)
    assert _execute(runtime, "FENCRYPT", descriptors["empty"]) == (0,)
    assert _execute(runtime, "FDECRYPT", descriptors["encempty"]) == (0,)
    assert _execute(runtime, "FENCRYPT", descriptors["tight"]) == (MASK64,)

    assert runtime.drain_uart_output() == (
        b" FENCRYPT: insufficient space\r\n"
    )
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert {
        name: _fd_snapshot(runtime, descriptor)
        for name, descriptor in descriptors.items()
    } == fd_before
    assert _mount_snapshot(runtime) == cache_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert _execute(runtime, "ENCRYPTED?", descriptors["plain"]) == (0,)
    assert _execute(runtime, "ENCRYPTED?", descriptors["empty"]) == (0,)
    assert _execute(runtime, "ENCRYPTED?", descriptors["encempty"]) == (TRUE,)
    assert _variable(runtime, "_FE-DESC") == descriptors["tight"]
    assert _variable(runtime, "_FE-USED") == 497
    assert _variable(runtime, "_FE-PAD") == SECTOR_SIZE
    assert _variable(runtime, "_FE-SECS") == 0


def _install_second_dma_allocation_failure(runtime: MegaForthRuntime) -> None:
    runtime.evaluate(
        b"' DMA-ALLOCATE CONSTANT REAL-DMA-ALLOCATE "
        b"VARIABLE TEST-DMA-ALLOCATIONS "
        b": DMA-ALLOCATE "
        b"1 TEST-DMA-ALLOCATIONS +! "
        b"TEST-DMA-ALLOCATIONS @ 2 = IF DROP 0 -1 EXIT THEN "
        b"REAL-DMA-ALLOCATE EXECUTE ;",
        source_name="second-dma-allocation-failure",
    )


@pytest.mark.parametrize(
    ("operation", "encrypted"),
    (("FENCRYPT", False), ("FDECRYPT", True)),
)
def test_second_dma_allocation_failure_leaves_extra_zero_but_restores_heap(
    operation: str,
    encrypted: bool,
) -> None:
    image, _entry_offset = _canonical_image(encrypted=encrypted)
    runtime = _load_application_loading(image)
    _install_second_dma_allocation_failure(runtime)
    runtime = _evaluate_filesystem_encryption(runtime)
    _mount(runtime)
    _install_key(runtime, KEY, name=f"{operation}-ALLOC-FAIL-KEY")
    descriptor = _open(runtime, "secret.bin")
    assert descriptor != 0
    fd_before = _fd_snapshot(runtime, descriptor)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    cache_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    assert _execute(runtime, operation, descriptor) == (0, MASK64)

    assert _variable(runtime, "TEST-DMA-ALLOCATIONS") == 2
    assert _variable(runtime, "_FE-BUF1") != 0
    assert _variable(runtime, "_FE-BUF2") == 0
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _mount_snapshot(runtime) == cache_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_flush_unsupported_aborts_after_payload_and_flag_with_both_buffers_leaked() -> None:
    plain_image, _entry_offset = _canonical_image(encrypted=False)
    expected_encrypted_image, _expected_entry_offset = _canonical_image(
        encrypted=True
    )
    storage = HostedStorageService(
        plain_image,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_filesystem_encryption_service(storage)
    _mount(runtime)
    _install_key(runtime, KEY, name="LATE-FLUSH-FS-KEY")
    descriptor = _open(runtime, "secret.bin")
    assert descriptor != 0
    fd_before = _fd_snapshot(runtime, descriptor)
    cache_before = _mount_snapshot(runtime)[:6]
    expected_cache = _cache_with_flags(cache_before, ENCRYPTED_FLAGS)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = storage.completion
    context = runtime.main_context
    assert context.data.snapshot() == ()
    context.data.push(descriptor)

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("FENCRYPT", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert storage.completion == completion_before + 4
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert storage.image_bytes == bytes(expected_encrypted_image)
    assert _file_span(storage.image_bytes) == ENCRYPTED_SPAN
    assert _mount_snapshot(runtime)[:6] == expected_cache
    assert _fd_snapshot(runtime, descriptor) == fd_before
    assert _execute(runtime, "ENCRYPTED?", descriptor) == (TRUE,)

    first = _variable(runtime, "_FE-BUF1")
    second = _variable(runtime, "_FE-BUF2")
    assert first != 0
    assert second != 0
    assert first != second
    for address in (first, second):
        assert runtime.memory.read64(address - 16) == (
            FILE_SECTORS * SECTOR_SIZE
        )
        assert runtime.memory.read64(address - 8) == 0xA110_CA7E_DEAD_BEEF
    assert _execute(runtime, "HEAP-FREE-BYTES") == (
        heap_before - 2 * (FILE_SECTORS * SECTOR_SIZE + 24),
    )
    assert runtime.spinlocks.owner(2) is None
