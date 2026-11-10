"""Unchanged-source acceptance for the KDOS module system."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64, TRUE, u64
from shared.storage import SECTOR_SIZE
from simulator.crc import CRC_STATUS_OK
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    ExecutionContext,
    MegaForthRuntime,
)
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_hash_tables import _load_hash_tables
from tests.simulator.test_kdos_mp64fs_lifecycle import _store
from tests.simulator.test_kdos_mp64fs_load import _HostSource, _select_source
from tests.simulator.test_kdos_mp64fs_mutation import _mount
from tests.simulator.test_kdos_pipeline_bundles import _registry_state
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-module-system-9615-10108.f"
)

FIRST_LINE = 9615
FIXTURE_LAST_LINE = 10108
LAST_LINE = 10107
FIXTURE_BYTES = 16_229
FIXTURE_SHA256 = (
    "cf818fbb9facbe2a06a825a6ba3cd6738b73d97793f423829b4386bc89b03a9d"
)
FIXTURE_GIT_BLOB = "881609bc61944004c8f8442eb22a527f132ebbd8"
SLICE_BYTES = 16_157
SLICE_SHA256 = (
    "e3b84f6fbb348c2275c2e4339bdb92f398e621c437dcc975949611d0e367d265"
)
SLICE_GIT_BLOB = "7b616252e40b75bdd9fa82fbc5fc6c0f2c47ae64"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 2_487
MODULE_LOCK_ID = 5
INLINE_BUCKETS = 16
MODULE_NODE_HEADER_BYTES = 32
MODULE_ID_MAX = 246
MODULE_FILE_BIT = 1 << 32
MODULE_FILE_KEY_BYTES = 24
XMEM_POISON_BYTES = 128

SOURCE_LEDGER = (
    ("CONSTANT", b"_MOD-INLINE-BUCKETS", 0),
    ("CREATE", b"_MOD-INLINE", 128),
    ("CREATE", b"_MOD-REG", 48),
    (":", b"_MOD-BUCKETS", 0),
    (":", b"_MOD-BUCKET-COUNT", 0),
    (":", b"_MOD-COUNT", 0),
    (":", b"_MOD-BUCKETS-HEAP?", 0),
    (":", b"_MOD-LOCK", 0),
    (":", b"_MOD-FILES", 0),
    (":", b"_MOD-ENTRIES", 0),
    ("CONSTANT", b"/MOD-NODE", 0),
    (":", b"_MN-NEXT", 0),
    (":", b"_MN-PROV", 0),
    (":", b"_MN-HASH", 0),
    (":", b"_MN-LEN", 0),
    (":", b"_MN-ID", 0),
    ("CONSTANT", b"_MOD-FILE-BIT", 0),
    (":", b"_MOD-TALLY", 0),
    ("DEFER", b"_MOD-ALLOCATE", CELL_BYTES),
    ("DEFER", b"_MOD-FREE", CELL_BYTES),
    ("CONSTANT", b"_MOD-E-NOMEM", 0),
    ("CONSTANT", b"_MOD-E-BAD-ID", 0),
    ("CONSTANT", b"_MOD-EVAL-LINE-MAX", 0),
    ("CONSTANT", b"_MOD-ID-MAX", 0),
    (":", b"_MOD-HASH", 0),
    (":", b"_MOD-BUCKET", 0),
    ("VARIABLE", b"_MF-A", CELL_BYTES),
    ("VARIABLE", b"_MF-U", CELL_BYTES),
    ("VARIABLE", b"_MF-H", CELL_BYTES),
    (":", b"_MOD-FIND-LOCKED", 0),
    (":", b"_MOD-FIND", 0),
    (":", b"_MOD-INSERT-HASHED", 0),
    (":", b"_MOD-INSERT", 0),
    ("VARIABLE", b"_MG-NEW", CELL_BYTES),
    ("VARIABLE", b"_MG-N", CELL_BYTES),
    ("VARIABLE", b"_MG-NODE", CELL_BYTES),
    ("VARIABLE", b"_MG-NEXT", CELL_BYTES),
    (":", b"_MOD-GROW-TARGET", 0),
    (":", b"_MOD-MAYBE-GROW", 0),
    ("VARIABLE", b"_MOD-GROW-PENDING", CELL_BYTES),
    ("VARIABLE", b"_MOD-GROW-READY", CELL_BYTES),
    (":", b"_MOD-TRY-PENDING-GROWTH", 0),
    (":", b"_MOD-ADOPT", 0),
    (":", b"_MOD-JOIN", 0),
    ("VARIABLE", b"_MU-TARGET", CELL_BYTES),
    ("VARIABLE", b"_MU-LINK", CELL_BYTES),
    (":", b"_MOD-UNLINK-LOCKED", 0),
    ("VARIABLE", b"_MRB-NODE", CELL_BYTES),
    (":", b"_MOD-ROLLBACK-FRAME", 0),
    (":", b"_MOD-COMMIT-FRAME", 0),
    (":", b"_MOD-AFTER-RELEASE", 0),
    ("CREATE", b"_PS-TAG", 9),
    (":", b"_PS-MATCH8?", 0),
    (":", b"_PS-SKIP-WS", 0),
    (":", b"_PS-TOKEN-LEN", 0),
    ("VARIABLE", b"_PS-PTR", CELL_BYTES),
    ("VARIABLE", b"_PS-REM", CELL_BYTES),
    ("VARIABLE", b"_PS-LINE-U", CELL_BYTES),
    (":", b"_PS-LINE-LEN", 0),
    (":", b"_MOD-PRESCAN", 0),
    (":", b"_MOD-PARSE-ID", 0),
    (":", b"PROVIDED-SPAN", 0),
    (":", b"PROVIDED", 0),
    (":", b"MODULE?", 0),
    ("CREATE", b"_MOD-FILE-KEY", MODULE_FILE_KEY_BYTES),
    (":", b"_MOD-FILE-KEY?", 0),
    (":", b"_MOD-FILE-HASH", 0),
    (":", b"_MOD-FILE-LOADED?", 0),
    (":", b"_MOD-FILE-NOTE", 0),
    (":", b"_MOD-READ-WALK", 0),
    (":", b"_MOD-LOAD-BODY", 0),
    (":", b"REQUIRE", 0),
    ("VARIABLE", b"_ML-NODE", CELL_BYTES),
    (":", b"_MOD-LIST-BODY", 0),
    (":", b"MODULES", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
ZERO_VARIABLES = tuple(
    name for definer, name, _body in SOURCE_LEDGER if definer == "VARIABLE"
)
CONSTANTS = {
    "_MOD-INLINE-BUCKETS": INLINE_BUCKETS,
    "/MOD-NODE": MODULE_NODE_HEADER_BYTES,
    "_MOD-E-NOMEM": u64(-4100),
    "_MOD-E-BAD-ID": u64(-4101),
    "_MOD-EVAL-LINE-MAX": 255,
    "_MOD-ID-MAX": MODULE_ID_MAX,
    "_MOD-FILE-BIT": MODULE_FILE_BIT,
}


def _verified_slice() -> bytes:
    fixture = FIXTURE.read_bytes()
    assert len(fixture) == FIXTURE_BYTES
    assert fixture.count(b"\n") == FIXTURE_LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(fixture).hexdigest() == FIXTURE_SHA256
    assert _git_blob_id(fixture) == FIXTURE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert fixture == b"".join(lines[FIRST_LINE - 1 : FIXTURE_LAST_LINE])
    boundary = b"\\ =====================================================================\n"
    assert lines[LAST_LINE - 1] == b"\n"
    assert lines[LAST_LINE] == boundary
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa714  Startup\n"
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_module_system(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_module_system() -> MegaForthRuntime:
    return _evaluate_module_system(_load_hash_tables())


def _body(runtime: MegaForthRuntime, name: str | bytes) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _span(runtime: MegaForthRuntime, name: str, payload: bytes) -> int:
    return runtime.define_created(name, initial_body=payload).body_address


def _stack_eval(runtime: MegaForthRuntime, source: bytes) -> tuple[int, ...]:
    assert runtime.main_context.data.snapshot() == ()
    runtime.evaluate(source, source_name="module-stack-eval.f")
    result = runtime.main_context.data.snapshot()
    runtime.main_context.data.clear()
    assert runtime.main_context.returns.snapshot() == ()
    return result


def _fnv1a32(payload: bytes) -> int:
    value = 0x811C_9DC5
    for byte in payload:
        value ^= byte
        value = (value * 0x0100_0193) & 0xFFFF_FFFF
    return value


def _find(runtime: MegaForthRuntime, address: int, length: int) -> int:
    return _execute(runtime, "_MOD-FIND", address, length)[0]


def _loader_globals(runtime: MegaForthRuntime) -> tuple[int, ...]:
    return tuple(
        _variable(runtime, name)
        for name in (
            "LD-BUF",
            "LD-SZ",
            "LD-CUR",
            "LD-LEN",
            "LD-LINE",
            "CWD",
        )
    )


def _run_guarded_source(runtime: MegaForthRuntime, source: bytes) -> int:
    address, status = _execute(runtime, "ALLOCATE", len(source))
    assert status == 0
    runtime.memory.write_bytes(address, source)
    _execute(runtime, "_LD-SAVE")
    _store(runtime, "LD-BUF", address)
    _store(runtime, "LD-SZ", len(source))
    walker = runtime.find("_LD-WALK-GUARDED")
    assert walker is not None
    result = _execute(runtime, "CATCH", walker.xt)
    assert len(result) == 1
    return result[0]


def _set_prescan_source(
    runtime: MegaForthRuntime,
    name: str,
    source: bytes,
) -> int:
    address = _span(runtime, name, source)
    _store(runtime, "LD-BUF", address)
    _store(runtime, "LD-SZ", len(source))
    return address


def _module_image(files: tuple[tuple[bytes, bytes], ...]) -> bytearray:
    image = _formatted_image(14 + len(files))
    for index, (name, source) in enumerate(files):
        assert len(source) <= SECTOR_SIZE
        sector = 14 + index
        _write_entry(
            image,
            6 + index,
            name=name + b"\0",
            start=sector,
            count=1,
            used=len(source),
            entry_type=3,
        )
        allocation = source + bytes(SECTOR_SIZE - len(source))
        image[sector * SECTOR_SIZE : (sector + 1) * SECTOR_SIZE] = allocation
    return image


def _module_counts(runtime: MegaForthRuntime) -> tuple[int, int, int]:
    """Module names, loaded files, and the entries growth follows."""

    return tuple(
        _execute(runtime, name)[0]
        for name in ("_MOD-COUNT", "_MOD-FILES", "_MOD-ENTRIES")
    )  # type: ignore[return-value]


def _file_key(source: _HostSource, identity: tuple[int, int]) -> bytes:
    return b"".join(
        value.to_bytes(8, "little") for value in (source.address, *identity)
    )


def _file_entry(runtime: MegaForthRuntime, key: bytes) -> int:
    file_hash = _fnv1a32(key) | MODULE_FILE_BIT
    node = runtime.memory.read64(_execute(runtime, "_MOD-BUCKET", file_hash)[0])
    while node:
        if (
            runtime.memory.read64(node + 16) == file_hash
            and runtime.memory.read64(node + 24) == len(key)
            and runtime.memory.read_bytes(
                node + MODULE_NODE_HEADER_BYTES,
                len(key),
            )
            == key
        ):
            return node
        node = runtime.memory.read64(node)
    return 0


def test_module_slice_is_exact_linked_initialized_and_load_effects_are_exact() -> None:
    runtime = _load_hash_tables()
    runtime.inject_uart_input(b"\x00M")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    assert runtime.spinlocks.acquire(3, 0) == 0
    crc_identity = (0, 0)
    assert runtime.crc.select_mode(crc_identity, 5) == CRC_STATUS_OK
    assert runtime.crc.seed(crc_identity, 0x1234_5678) == CRC_STATUS_OK
    assert runtime.crc.feed_byte(crc_identity, 0xA5) == CRC_STATUS_OK
    crc_before = (
        runtime.crc.mode,
        runtime.crc.accumulator,
        runtime.crc.owner,
    )
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    xmem_here_before = _variable(runtime, "XMEM-HERE")
    xmem_floor_before = _variable(runtime, "XMEM-FLOOR")
    assert xmem_here_before == xmem_floor_before
    assert _variable(runtime, "XMEM-FL") == 0
    runtime.memory.fill(xmem_here_before, XMEM_POISON_BYTES, 0xA5)
    free_list_before = tuple(
        _variable(runtime, name) for name in ("FL-NEED", "FL-PREV", "FL-CURR")
    )
    heap_before = tuple(
        _variable(runtime, name)
        for name in ("HEAP-BASE", "HEAP-FREE", "HEAP-INIT")
    )
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    registry_before = _registry_state(runtime)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_module_system(runtime)

    assert len(SOURCE_LEDGER) == 75
    assert sum(definer == ":" for definer, _name, _body in SOURCE_LEDGER) == 46
    assert sum(
        definer == "VARIABLE" for definer, _name, _body in SOURCE_LEDGER
    ) == 16
    assert sum(definer == "CREATE" for definer, _name, _body in SOURCE_LEDGER) == 4
    assert sum(definer == "DEFER" for definer, _name, _body in SOURCE_LEDGER) == 2
    assert sum(
        definer == "CONSTANT" for definer, _name, _body in SOURCE_LEDGER
    ) == 7
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 859
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == 353
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((definer, _name, body_span), word) in enumerate(
        zip(SOURCE_LEDGER, published)
    ):
        assert runtime.memory.read64(word.header_address) == prior_header
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following - word.body_address == body_span
        expected_type = (
            ColonDefinition
            if definer == ":"
            else ConstantDefinition
            if definer == "CONSTANT"
            else CreatedDefinition
        )
        assert isinstance(word.implementation, expected_type)
        prior_header = word.header_address

    for name, value in CONSTANTS.items():
        assert _constant(runtime, name) == value
    assert tuple(_variable(runtime, name) for name in ZERO_VARIABLES) == (
        0,
    ) * len(ZERO_VARIABLES)
    inline = _body(runtime, "_MOD-INLINE")
    registry = _body(runtime, "_MOD-REG")
    assert runtime.memory.read_bytes(inline, 128) == bytes(128)
    assert tuple(runtime.memory.read64(registry + offset) for offset in range(0, 48, 8)) == (
        inline,
        INLINE_BUCKETS,
        0,
        0,
        MODULE_LOCK_ID,
        0,
    )
    assert runtime.memory.read_bytes(
        _body(runtime, "_MOD-FILE-KEY"),
        MODULE_FILE_KEY_BYTES,
    ) == b"\xA5" * MODULE_FILE_KEY_BYTES
    assert runtime.memory.read_bytes(_body(runtime, "_PS-TAG"), 9) == b"PROVIDED\0"
    assert runtime.memory.read64(_body(runtime, "_MOD-ALLOCATE")) == runtime.find(
        "DMA-ALLOCATE"
    ).xt  # type: ignore[union-attr]
    assert runtime.memory.read64(_body(runtime, "_MOD-FREE")) == runtime.find(
        "DMA-FREE"
    ).xt  # type: ignore[union-attr]
    for hook, target in (
        ("_LD-TXN-COMMIT", "_MOD-COMMIT-FRAME"),
        ("_LD-TXN-ROLLBACK", "_MOD-ROLLBACK-FRAME"),
        ("_LD-TXN-AFTER-RELEASE", "_MOD-AFTER-RELEASE"),
    ):
        target_word = runtime.find(target)
        assert target_word is not None
        assert runtime.memory.read64(_body(runtime, hook)) == target_word.xt

    # The slice takes no extended memory: the loader's own stack of
    # sources and directories lies in the dictionary, with its frames.
    assert _variable(runtime, "XMEM-HERE") == xmem_here_before
    assert _variable(runtime, "XMEM-FLOOR") == xmem_floor_before
    assert _variable(runtime, "XMEM-FL") == 0
    assert tuple(
        _variable(runtime, name) for name in ("FL-NEED", "FL-PREV", "FL-CURR")
    ) == free_list_before
    assert runtime.memory.read_bytes(xmem_here_before, XMEM_POISON_BYTES) == (
        b"\xA5" * XMEM_POISON_BYTES
    )
    assert tuple(
        _variable(runtime, name)
        for name in ("HEAP-BASE", "HEAP-FREE", "HEAP-INIT")
    ) == heap_before
    assert _registry_state(runtime) == registry_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.spinlocks.owners == locks_before
    assert (
        runtime.crc.mode,
        runtime.crc.accumulator,
        runtime.crc.owner,
    ) == crc_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.timer.counter > counter_before
    assert runtime.uart_input == b"\x00M"
    assert runtime.drain_uart_output() == b"retained-output"


def test_exact_ids_are_stable_case_sensitive_duplicate_neutral_and_bounded() -> None:
    runtime = _load_module_system()
    module_id = b"Alpha.Module"
    source = _span(runtime, "MODULE-ID-SOURCE", module_id)
    expected_hash = _fnv1a32(module_id)
    heap_uninitialized = _variable(runtime, "HEAP-INIT")

    assert _execute(runtime, "_MOD-HASH", source, len(module_id)) == (
        expected_hash,
    )
    assert _execute(runtime, "PROVIDED-SPAN", source, len(module_id)) == ()
    assert heap_uninitialized == 0
    assert _variable(runtime, "HEAP-INIT") == 1
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    node = _find(runtime, source, len(module_id))
    assert node != 0
    bucket = _execute(runtime, "_MOD-BUCKET", expected_hash)[0]
    assert bucket == _body(runtime, "_MOD-INLINE") + (
        expected_hash % INLINE_BUCKETS
    ) * CELL_BYTES
    assert runtime.memory.read64(bucket) == node
    assert runtime.memory.read64(node) == 0
    assert runtime.memory.read64(node + 8) == 0
    assert runtime.memory.read64(node + 16) == expected_hash
    assert runtime.memory.read64(node + 24) == len(module_id)
    assert runtime.memory.read_bytes(node + MODULE_NODE_HEADER_BYTES, len(module_id)) == module_id

    runtime.memory.write_bytes(source, b"X" * len(module_id))
    query = _span(runtime, "MODULE-ID-QUERY", module_id)
    lower = _span(runtime, "MODULE-ID-LOWER", module_id.lower())
    assert _find(runtime, query, len(module_id)) == node
    assert _find(runtime, lower, len(module_id)) == 0
    assert _stack_eval(runtime, b"MODULE? Alpha.Module") == (TRUE,)
    assert _stack_eval(runtime, b"MODULE? alpha.module") == (0,)

    heap_before = _execute(runtime, "HEAP-FREE-BYTES")
    assert _execute(runtime, "PROVIDED-SPAN", query, len(module_id)) == ()
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_before
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert _find(runtime, query, len(module_id)) == node

    allocation_requests: list[int] = []

    def fail_module_allocation(context: ExecutionContext) -> None:
        allocation_requests.append(context.data.pop())
        context.data.push(0)
        context.data.push(MASK64)

    failure = runtime.define_primitive(
        "FAIL-MODULE-ALLOCATION",
        fail_module_allocation,
    )
    runtime.evaluate(
        b"' " + failure.name + b" IS _MOD-ALLOCATE",
        source_name="module-allocation-failure-seam.f",
    )
    assert _execute(runtime, "PROVIDED-SPAN", query, len(module_id)) == ()
    assert allocation_requests == []

    missing_id = b"Missing.Module"
    missing = _span(runtime, "MISSING-MODULE-ID", missing_id)
    runtime.evaluate(
        b": TRY-MISSING-MODULE "
        + str(missing).encode("ascii")
        + b" "
        + str(len(missing_id)).encode("ascii")
        + b" PROVIDED-SPAN ;",
        source_name="module-oom-wrapper.f",
    )
    action = runtime.find("TRY-MISSING-MODULE")
    assert action is not None
    assert _execute(runtime, "CATCH", action.xt) == (u64(-4100),)
    assert allocation_requests == [MODULE_NODE_HEADER_BYTES + len(missing_id)]
    assert _find(runtime, missing, len(missing_id)) == 0
    assert _execute(runtime, "_MOD-COUNT") == (1,)

    too_long = _span(runtime, "TOO-LONG-MODULE-ID", b"L" * 247)
    assert _execute(runtime, "_MOD-INSERT", 0, 0) == (0, 0, u64(-4101))
    assert _execute(runtime, "_MOD-INSERT", too_long, 247) == (
        0,
        0,
        u64(-4101),
    )
    assert allocation_requests == [MODULE_NODE_HEADER_BYTES + len(missing_id)]
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None


def test_growth_rehashes_stable_nodes_and_failed_growth_remains_retryable() -> None:
    runtime = _load_module_system()
    scratch = _span(runtime, "MODULE-GROWTH-ID", bytes(16))
    nodes: dict[bytes, int] = {}

    for index in range(33):
        module_id = f"growth.{index:02d}".encode("ascii")
        runtime.memory.write_bytes(scratch, module_id)
        assert _execute(runtime, "PROVIDED-SPAN", scratch, len(module_id)) == ()
        nodes[module_id] = _find(runtime, scratch, len(module_id))

    inline = _body(runtime, "_MOD-INLINE")
    buckets = _execute(runtime, "_MOD-BUCKETS")[0]
    assert _execute(runtime, "_MOD-COUNT") == (33,)
    assert _execute(runtime, "_MOD-BUCKET-COUNT") == (32,)
    assert _execute(runtime, "_MOD-BUCKETS-HEAP?") == (1,)
    assert buckets != inline
    assert _execute(runtime, "_MOD-GROW-TARGET") == (0,)
    assert _variable(runtime, "_MOD-GROW-PENDING") == 0
    assert any(runtime.memory.read64(inline + index * 8) for index in range(16))
    for module_id, original_node in nodes.items():
        runtime.memory.write_bytes(scratch, module_id)
        assert _find(runtime, scratch, len(module_id)) == original_node
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None

    retry = _load_module_system()
    retry.evaluate(
        b": FAIL-ONLY-MODULE-GROWTH "
        b"  DUP 256 = IF DROP 0 -1 ELSE DMA-ALLOCATE THEN ; "
        b"' FAIL-ONLY-MODULE-GROWTH IS _MOD-ALLOCATE",
        source_name="module-growth-failure-seam.f",
    )
    retry_scratch = _span(retry, "RETRY-MODULE-ID", bytes(16))
    first_node = 0
    for index in range(33):
        module_id = f"retry.{index:02d}".encode("ascii")
        retry.memory.write_bytes(retry_scratch, module_id)
        assert _execute(
            retry,
            "PROVIDED-SPAN",
            retry_scratch,
            len(module_id),
        ) == ()
        if index == 0:
            first_node = _find(retry, retry_scratch, len(module_id))

    assert _execute(retry, "_MOD-COUNT") == (33,)
    assert _execute(retry, "_MOD-BUCKET-COUNT") == (16,)
    assert _execute(retry, "_MOD-BUCKETS-HEAP?") == (0,)
    assert _variable(retry, "_MOD-GROW-PENDING") == 1
    retry.memory.write_bytes(retry_scratch, b"retry.32")
    assert _find(retry, retry_scratch, len(b"retry.32")) != 0

    retry.evaluate(
        b"' DMA-ALLOCATE IS _MOD-ALLOCATE",
        source_name="module-growth-retry-seam.f",
    )
    assert _execute(retry, "_MOD-TRY-PENDING-GROWTH") == ()
    assert _execute(retry, "_MOD-BUCKET-COUNT") == (32,)
    assert _execute(retry, "_MOD-BUCKETS-HEAP?") == (1,)
    assert _variable(retry, "_MOD-GROW-PENDING") == 0
    retry.memory.write_bytes(retry_scratch, b"retry.00")
    assert _find(retry, retry_scratch, len(b"retry.00")) == first_node
    assert retry.spinlocks.owner(MODULE_LOCK_ID) is None


def test_loader_frame_commit_and_rollback_cover_all_provisional_ids() -> None:
    runtime = _load_module_system()
    loader_before = _loader_globals(runtime)
    successful = (
        b"PROVIDED txn.primary\n"
        b"PROVIDED txn.alias\n"
        b": TXN-COMMITTED-WORD 42 ;\n"
    )

    assert _run_guarded_source(runtime, successful) == 0

    assert _variable(runtime, "_LD-SP") == 0
    assert _loader_globals(runtime) == loader_before
    assert _execute(runtime, "_MOD-COUNT") == (2,)
    assert _execute(runtime, "TXN-COMMITTED-WORD") == (42,)
    for index, module_id in enumerate((b"txn.primary", b"txn.alias")):
        query = _span(runtime, f"TXN-COMMIT-QUERY-{index}", module_id)
        node = _find(runtime, query, len(module_id))
        assert node != 0
        assert runtime.memory.read64(node + 8) == 0

    heap_before_failure = _execute(runtime, "HEAP-FREE-BYTES")
    failing = (
        b"PROVIDED txn.failed\n"
        b"PROVIDED txn.failed-alias\n"
        b": TXN-LEFT-BEHIND 77 ;\n"
        b"-77 THROW\n"
    )

    assert _run_guarded_source(runtime, failing) == u64(-77)

    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_before_failure
    assert _execute(runtime, "_MOD-COUNT") == (2,)
    assert runtime.find("TXN-LEFT-BEHIND") is None
    for index, module_id in enumerate((b"txn.failed", b"txn.failed-alias")):
        query = _span(runtime, f"TXN-ROLLBACK-QUERY-{index}", module_id)
        assert _find(runtime, query, len(module_id)) == 0

    retry = b"PROVIDED txn.failed\nPROVIDED txn.failed-alias\n"
    assert _run_guarded_source(runtime, retry) == 0
    assert _execute(runtime, "_MOD-COUNT") == (4,)
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None


def test_prescan_uses_exact_uppercase_blank_tokens_and_line_limit() -> None:
    runtime = _load_module_system()
    ordinary = (
        b"\tPROVIDED tab-is-not-leading-space\n"
        b"prefix PROVIDED not-first\n"
        b"provided lowercase-is-not-prescanned\n"
        b"   PROVIDED exact.id trailing-token\n"
        b"PROVIDED later.id\n"
    )
    base = _set_prescan_source(runtime, "PRESCAN-ORDINARY", ordinary)

    address, length, found = _execute(runtime, "_MOD-PRESCAN")

    assert found == TRUE
    assert runtime.memory.read_bytes(address, length) == b"exact.id"
    assert base <= address < base + len(ordinary)

    absent = b"provided lower\nword PROVIDED middle\n"
    _set_prescan_source(runtime, "PRESCAN-ABSENT", absent)
    assert _execute(runtime, "_MOD-PRESCAN") == (0, 0, 0)

    missing_id = b"PROVIDED   \nPROVIDED later\n"
    _set_prescan_source(runtime, "PRESCAN-MISSING-ID", missing_id)
    missing_address, missing_length, missing_found = _execute(
        runtime,
        "_MOD-PRESCAN",
    )
    assert missing_found == TRUE
    assert missing_length == 0
    assert missing_address != 0

    maximum = b"PROVIDED " + b"M" * MODULE_ID_MAX + b"\n"
    _set_prescan_source(runtime, "PRESCAN-MAXIMUM", maximum)
    max_address, max_length, max_found = _execute(runtime, "_MOD-PRESCAN")
    assert max_found == TRUE
    assert max_length == MODULE_ID_MAX
    assert runtime.memory.read_bytes(max_address, max_length) == b"M" * MODULE_ID_MAX

    overlong = b"PROVIDED " + b"L" * (MODULE_ID_MAX + 1) + b"\n"
    _set_prescan_source(runtime, "PRESCAN-OVERLONG", overlong)
    assert _execute(runtime, "_MOD-PRESCAN") == (0, 0, TRUE)

    crlf = b"PROVIDED id\r\n"
    _set_prescan_source(runtime, "PRESCAN-CRLF", crlf)
    cr_address, cr_length, cr_found = _execute(runtime, "_MOD-PRESCAN")
    assert cr_found == TRUE
    assert runtime.memory.read_bytes(cr_address, cr_length) == b"id"


def test_require_releases_every_frame_when_prescan_registration_is_out_of_memory() -> None:
    module_source = (
        b"PROVIDED oom.module\n"
        b": OOM-SOURCE-MUST-NOT-EXECUTE 1 ;\n"
    )
    image = _module_image(((b"oom.f", module_source),))
    runtime = _load_module_system()
    runtime.storage.attach(image)
    _mount(runtime)
    media_before = runtime.storage.image_bytes
    loader_before = _loader_globals(runtime)
    runtime.evaluate(
        b": FAIL-MODULE-NODE-ALLOCATION DROP 0 -1 ; "
        b"' FAIL-MODULE-NODE-ALLOCATION IS _MOD-ALLOCATE",
        source_name="module-prescan-oom-seam.f",
    )

    result = _stack_eval(runtime, b"' REQUIRE CATCH oom.f")

    assert result == (u64(-4100),)
    assert runtime.find("OOM-SOURCE-MUST-NOT-EXECUTE") is None
    assert _stack_eval(runtime, b"MODULE? oom.module") == (0,)
    assert _execute(runtime, "_MOD-COUNT") == (0,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None
    assert runtime.spinlocks.owner(2) is None

    runtime.evaluate(
        b"' DMA-ALLOCATE IS _MOD-ALLOCATE",
        source_name="module-prescan-oom-retry-seam.f",
    )
    runtime.evaluate(b"REQUIRE oom.f", source_name="module-prescan-oom-retry.f")
    assert _execute(runtime, "OOM-SOURCE-MUST-NOT-EXECUTE") == (1,)
    assert _stack_eval(runtime, b"MODULE? oom.module") == (TRUE,)
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None
    assert runtime.spinlocks.owner(2) is None


def test_require_breaks_an_uppercase_self_cycle_and_skips_duplicate_evaluation() -> None:
    module_source = (
        b"PROVIDED cycle.demo\r\n"
        b"VARIABLE CYCLE-LOAD-COUNT 0 CYCLE-LOAD-COUNT !\r\n"
        b"1 CYCLE-LOAD-COUNT +!\r\n"
        b"REQUIRE cycle.f\r\n"
        b"1 CYCLE-LOAD-COUNT +!\r\n"
    )
    image = _module_image(((b"cycle.f", module_source),))
    runtime = _load_module_system()
    runtime.storage.attach(image)
    _mount(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"REQUIRE cycle.f", source_name="require-cycle-first.f")

    assert _variable(runtime, "CYCLE-LOAD-COUNT") == 2
    # The file requires itself and is still read once: its own REQUIRE
    # finds the file's record, made before the first line was evaluated.
    assert runtime.storage.completion == completion_before + 1
    assert _module_counts(runtime) == (1, 1, 2)
    assert _stack_eval(runtime, b"MODULE? cycle.demo") == (TRUE,)
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    here_after_first = runtime.dictionary.here
    heap_after_first = _execute(runtime, "HEAP-FREE-BYTES")
    xmem_after_first = (
        _variable(runtime, "XMEM-HERE"),
        _variable(runtime, "XMEM-FL"),
    )

    runtime.evaluate(b"REQUIRE cycle.f", source_name="require-cycle-second.f")

    assert _variable(runtime, "CYCLE-LOAD-COUNT") == 2
    assert runtime.storage.completion == completion_before + 1
    assert runtime.dictionary.here == here_after_first
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_after_first
    assert (
        _variable(runtime, "XMEM-HERE"),
        _variable(runtime, "XMEM-FL"),
    ) == xmem_after_first
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert runtime.storage.image_bytes == media_before

    assert _execute(runtime, "MODULES") == ()
    output = runtime.drain_uart_output()
    assert output == (
        b" Loaded modules:\r\n"
        b"  cycle.demo\r\n"
        b"1  module(s)\r\n"
    )
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None
    assert runtime.spinlocks.owner(1) is None


def test_nested_dependency_joins_parent_dictionary_and_registry_rollback() -> None:
    parent_source = (
        b"PROVIDED parent.failed\n"
        b"REQUIRE child.f\n"
        b": PARENT-LEFT-BEHIND 91 ;\n"
        b"-77 THROW\n"
    )
    child_source = (
        b"PROVIDED child.committed\n"
        b": CHILD-COMMITTED-WORD 73 ;\n"
    )
    image = _module_image(
        (
            (b"parent.f", parent_source),
            (b"child.f", child_source),
        )
    )
    runtime = _load_module_system()
    runtime.storage.attach(image)
    _mount(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")
    dictionary_before = (runtime.dictionary.here, runtime.dictionary.latest)
    media_before = runtime.storage.image_bytes

    result = _stack_eval(runtime, b"' REQUIRE CATCH parent.f")

    assert result == (u64(-77),)
    assert _stack_eval(runtime, b"MODULE? parent.failed") == (0,)
    assert _stack_eval(runtime, b"MODULE? child.committed") == (0,)
    assert _execute(runtime, "_MOD-COUNT") == (0,)
    assert runtime.find("CHILD-COMMITTED-WORD") is None
    assert runtime.find("PARENT-LEFT-BEHIND") is None
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_before
    assert (
        runtime.dictionary.here,
        runtime.dictionary.latest,
    ) == dictionary_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert runtime.storage.image_bytes == media_before
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None
    assert runtime.spinlocks.owner(2) is None


def test_require_reads_a_loaded_file_once_and_counts_names_apart_from_files() -> None:
    runtime = _load_module_system()
    runtime.evaluate(
        b"VARIABLE C-LOADS VARIABLE ALIAS-RAN VARIABLE BARE-LOADS "
        b"VARIABLE PLAIN-LOADS",
        source_name="require-counters.f",
    )
    identities = {
        b"a.f": (0x0A, 1),
        b"lib/b.f": (0x0B, 1),
        b"c.f": (0x0C, 1),
        b"alias.f": (0x0D, 1),
        b"plain.f": (0x0E, 1),
    }
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        {
            b"a.f": (
                b"PROVIDED mod.a\n"
                b"REQUIRE lib/b.f\n"
                b"REQUIRE c.f\n"
                b": A-WORD 101 ;\n"
            ),
            b"lib/b.f": b"PROVIDED mod.b\nREQUIRE ../c.f\n: B-WORD 102 ;\n",
            b"c.f": b"PROVIDED mod.c\n1 C-LOADS +!\n: C-WORD 103 ;\n",
            b"alias.f": b"PROVIDED mod.c\n1 ALIAS-RAN +!\n",
            b"bare.f": b"PROVIDED mod.bare\n1 BARE-LOADS +!\n",
            b"plain.f": b"1 PLAIN-LOADS +!\n",
        },
        identities=identities,
    )
    _select_source(runtime, source)

    runtime.evaluate(b"REQUIRE a.f", source_name="require-host-first.f")

    assert _execute(runtime, "A-WORD") == (101,)
    assert _execute(runtime, "B-WORD") == (102,)
    assert _execute(runtime, "C-WORD") == (103,)
    assert _variable(runtime, "C-LOADS") == 1
    # a.f's own REQUIRE of c.f finds the file b.f loaded and does not read.
    assert source.resolves() == [b"a.f", b"lib/b.f", b"../c.f", b"c.f"]
    assert source.reads() == [b"a.f", b"lib/b.f", b"c.f"]
    assert _module_counts(runtime) == (3, 3, 6)

    here_after_first = runtime.dictionary.here
    heap_after_first = _execute(runtime, "HEAP-FREE-BYTES")
    runtime.evaluate(
        b"REQUIRE a.f REQUIRE lib/b.f REQUIRE c.f",
        source_name="require-host-again.f",
    )

    assert len(source.resolves()) == 7
    assert source.reads() == [b"a.f", b"lib/b.f", b"c.f"]
    assert runtime.dictionary.here == here_after_first
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_after_first
    assert _module_counts(runtime) == (3, 3, 6)

    # A second file of a loaded module is read once to find that out.
    runtime.evaluate(b"REQUIRE alias.f", source_name="require-alias.f")
    assert source.reads()[3:] == [b"alias.f"]
    assert _variable(runtime, "ALIAS-RAN") == 0
    assert _module_counts(runtime) == (3, 4, 7)
    runtime.evaluate(b"REQUIRE alias.f", source_name="require-alias-again.f")
    assert source.reads()[3:] == [b"alias.f"]

    # A file whose source gives no identity is read every time, and its
    # PROVIDED line still keeps it from running twice.
    runtime.evaluate(
        b"REQUIRE bare.f REQUIRE bare.f",
        source_name="require-without-identity.f",
    )
    assert source.reads()[4:] == [b"bare.f", b"bare.f"]
    assert _variable(runtime, "BARE-LOADS") == 1
    assert _module_counts(runtime) == (4, 4, 8)

    # A file with no PROVIDED line names no module, so it runs every time.
    runtime.evaluate(
        b"REQUIRE plain.f REQUIRE plain.f",
        source_name="require-without-provided.f",
    )
    assert source.reads()[6:] == [b"plain.f", b"plain.f"]
    assert _variable(runtime, "PLAIN-LOADS") == 2
    assert _module_counts(runtime) == (4, 4, 8)

    for path in (b"a.f", b"lib/b.f", b"c.f", b"alias.f"):
        key = _file_key(source, identities[path])
        entry = _file_entry(runtime, key)
        assert entry != 0
        assert runtime.memory.read64(entry + 8) == 0
        # No name lookup finds a file's entry, even by its exact bytes.
        query = _span(runtime, f"FILE-KEY-{identities[path][0]:02X}", key)
        assert _find(runtime, query, len(key)) == 0
    assert _file_entry(runtime, _file_key(source, identities[b"plain.f"])) == 0

    assert _execute(runtime, "MODULES") == ()
    lines = runtime.drain_uart_output().split(b"\r\n")
    assert lines[0] == b" Loaded modules:"
    assert sorted(lines[1:-2]) == [
        b"  mod.a",
        b"  mod.b",
        b"  mod.bare",
        b"  mod.c",
    ]
    assert lines[-2:] == [b"4  module(s)", b""]
    assert source.live_tokens == {}
    assert source.directory == ()
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None
    assert runtime.spinlocks.owner(1) is None


def test_a_failed_require_forgets_the_files_it_noted() -> None:
    runtime = _load_module_system()
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        {
            b"parent.f": (
                b"PROVIDED host.parent\n"
                b"REQUIRE child.f\n"
                b"-77 THROW\n"
            ),
            b"child.f": b"PROVIDED host.child\n: HOST-CHILD 73 ;\n",
        },
        identities={b"parent.f": (1, 7), b"child.f": (2, 7)},
    )
    _select_source(runtime, source)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")
    dictionary_before = (runtime.dictionary.here, runtime.dictionary.latest)

    result = _stack_eval(runtime, b"' REQUIRE CATCH parent.f")

    assert result == (u64(-77),)
    assert _module_counts(runtime) == (0, 0, 0)
    assert runtime.find("HOST-CHILD") is None
    assert _execute(runtime, "HEAP-FREE-BYTES") == heap_before
    assert (
        runtime.dictionary.here,
        runtime.dictionary.latest,
    ) == dictionary_before
    assert source.live_tokens == {}
    assert source.directory == ()
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0

    runtime.evaluate(b"REQUIRE child.f", source_name="require-after-failure.f")

    assert source.reads() == [b"parent.f", b"child.f", b"child.f"]
    assert _execute(runtime, "HOST-CHILD") == (73,)
    assert _module_counts(runtime) == (1, 1, 2)
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None


def test_file_identities_belong_to_the_source_that_gave_them() -> None:
    runtime = _load_module_system()
    first = _HostSource(
        runtime,
        "HOST-FIRST",
        {b"x.f": b"PROVIDED host.x\n: HOST-X 1 ;\n"},
        identities={b"x.f": (5, 5)},
    )
    second = _HostSource(
        runtime,
        "HOST-SECOND",
        {b"y.f": b"PROVIDED host.y\n: HOST-Y 2 ;\n"},
        identities={b"y.f": (5, 5)},
    )

    _select_source(runtime, first)
    runtime.evaluate(b"REQUIRE x.f", source_name="require-first-source.f")
    _select_source(runtime, second)
    runtime.evaluate(b"REQUIRE y.f", source_name="require-second-source.f")

    assert second.reads() == [b"y.f"]
    assert _execute(runtime, "HOST-X") == (1,)
    assert _execute(runtime, "HOST-Y") == (2,)
    assert _module_counts(runtime) == (2, 2, 4)

    runtime.evaluate(b"REQUIRE y.f", source_name="require-second-again.f")
    _select_source(runtime, first)
    runtime.evaluate(b"REQUIRE x.f", source_name="require-first-again.f")

    assert first.reads() == [b"x.f"]
    assert second.reads() == [b"y.f"]
    assert _module_counts(runtime) == (2, 2, 4)


def test_registry_growth_follows_names_and_files_together() -> None:
    runtime = _load_module_system()
    files = {
        f"m{index:02d}.f".encode("ascii"): (
            f"PROVIDED grow.{index:02d}\n".encode("ascii")
        )
        for index in range(17)
    }
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        files,
        identities={
            name: (index + 1, 9) for index, name in enumerate(files)
        },
    )
    _select_source(runtime, source)

    for index, name in enumerate(files):
        runtime.evaluate(b"REQUIRE " + name, source_name="require-growth.f")
        if index == 15:
            # Sixteen names would not fill sixteen buckets twice over;
            # with their files they do, exactly.
            assert _module_counts(runtime) == (16, 16, 32)
            assert _execute(runtime, "_MOD-BUCKET-COUNT") == (16,)

    assert _module_counts(runtime) == (17, 17, 34)
    assert _execute(runtime, "_MOD-BUCKET-COUNT") == (32,)
    assert _execute(runtime, "_MOD-BUCKETS-HEAP?") == (1,)
    assert _variable(runtime, "_MOD-GROW-PENDING") == 0
    for index, name in enumerate(files):
        assert _stack_eval(
            runtime,
            f"MODULE? grow.{index:02d}".encode("ascii"),
        ) == (TRUE,)
        assert _file_entry(runtime, _file_key(source, (index + 1, 9))) != 0
        runtime.evaluate(b"REQUIRE " + name, source_name="require-grown.f")
    assert source.reads() == list(files)
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None


def test_require_rolls_back_the_name_when_the_file_note_is_out_of_memory() -> None:
    module_source = b"PROVIDED note.module\n: NOTE-WORD 1 ;\n"
    image = _module_image(((b"note.f", module_source),))
    runtime = _load_module_system()
    runtime.storage.attach(image)
    _mount(runtime)
    loader_before = _loader_globals(runtime)
    file_entry_bytes = MODULE_NODE_HEADER_BYTES + MODULE_FILE_KEY_BYTES
    assert MODULE_NODE_HEADER_BYTES + len(b"note.module") != file_entry_bytes
    runtime.evaluate(
        b": FAIL-FILE-NOTE DUP "
        + str(file_entry_bytes).encode("ascii")
        + b" = IF DROP 0 -1 ELSE DMA-ALLOCATE THEN ; "
        b"' FAIL-FILE-NOTE IS _MOD-ALLOCATE",
        source_name="module-file-note-oom-seam.f",
    )

    result = _stack_eval(runtime, b"' REQUIRE CATCH note.f")

    assert result == (u64(-4100),)
    assert runtime.find("NOTE-WORD") is None
    assert _stack_eval(runtime, b"MODULE? note.module") == (0,)
    assert _module_counts(runtime) == (0, 0, 0)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert runtime.spinlocks.owner(MODULE_LOCK_ID) is None

    runtime.evaluate(
        b"' DMA-ALLOCATE IS _MOD-ALLOCATE",
        source_name="module-file-note-retry-seam.f",
    )
    runtime.evaluate(b"REQUIRE note.f", source_name="module-file-note-retry.f")
    assert _execute(runtime, "NOTE-WORD") == (1,)
    assert _module_counts(runtime) == (1, 1, 2)

    # KDOS's own filesystem gives an identity too: the second REQUIRE
    # finds the file by its directory entry and reads no data.
    completion_after_load = runtime.storage.completion
    runtime.evaluate(b"REQUIRE note.f", source_name="module-file-note-again.f")
    assert runtime.storage.completion == completion_after_load
    assert _module_counts(runtime) == (1, 1, 2)
    assert _loader_globals(runtime) == loader_before
