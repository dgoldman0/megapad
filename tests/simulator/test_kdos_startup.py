"""Unchanged-source acceptance for the final KDOS startup block."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import TRUE
from shared.storage import SECTOR_SIZE
from simulator.dictionary_index import DictionaryIndexState
from simulator.runtime import ColonDefinition, CreatedDefinition, MegaForthRuntime
from tests.simulator.test_bios_mp64fs import _formatted_image
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_module_system import (
    _body,
    _execute,
    _load_module_system,
    _loader_globals,
    _module_image,
    _stack_eval,
)
from tests.simulator.test_kdos_storage_block_volume import _variable


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-startup-9854-9894.f"

FIRST_LINE = 9854
LAST_LINE = 9894
FIXTURE_BYTES = 1_410
FIXTURE_SHA256 = (
    "468983d02d94ed94b7accc8b98f5f60ef1b28c4e397a167d0be95ad785d5f4ae"
)
FIXTURE_GIT_BLOB = "5a95a4dafdeec003d706381d8ea9b5ec93d0ccd0"
KDOS_BYTES = 341_355
KDOS_SHA256 = (
    "99e71114ed141c14522d687a3bef3110ead94de7b0a055ae693c135a94772fb8"
)

HOSTED_WORD_FIXED_BYTES = 17
HOSTED_STATIC_GROWTH = 71
LATE_DICTIONARY_RESERVE = 32_768
ALLOCATOR_HEADER_BYTES = 24
AUTOEXEC_NAME = b"autoexec.f"
DEFINITIONS = (b"_AUTOEXEC-NAME", b"_AUTOEXEC-RUN")
SOURCE_LEDGER = (
    ("CREATE", b"_AUTOEXEC-NAME", len(AUTOEXEC_NAME)),
    (":", b"_AUTOEXEC-RUN", 0),
)

STARTUP_BANNER = (
    b"\r\n"
    + b"-" * 60
    + b"\r\n"
    + b"  KDOS v1.1 \xe2\x80\x94 Kernel Dashboard OS\r\n"
    + b"-" * 60
    + b"\r\n"
    + b" Type HELP for commands, HELP <word> for details.\r\n"
    + b" Type SCREENS for interactive TUI (or N SCREEN for screen N).\r\n"
    + b" Type TOPICS or LESSONS for documentation.\r\n"
)


def _verified_slice() -> bytes:
    fixture = FIXTURE.read_bytes()
    assert len(fixture) == FIXTURE_BYTES
    assert fixture.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(fixture).hexdigest() == FIXTURE_SHA256
    assert _git_blob_id(fixture) == FIXTURE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert len(complete_kdos) == KDOS_BYTES
    assert hashlib.sha256(complete_kdos).hexdigest() == KDOS_SHA256
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert len(lines) == LAST_LINE
    assert lines[FIRST_LINE - 2] == b"\n"
    assert fixture == b"".join(lines[FIRST_LINE - 1 :])
    assert fixture.startswith(b"\\ ===========================================")
    assert fixture.endswith(b"JIT-OFF\nCR\n")
    return fixture


def _evaluate_startup(runtime: MegaForthRuntime):
    result = runtime.evaluate(
        _verified_slice(),
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}"
        ),
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return result


def _aligned_tile(address: int) -> int:
    return (address + 63) & -64


def _expected_heap_size(runtime: MegaForthRuntime, heap_base: int) -> int:
    memory_size = _execute(runtime, "MEM-SIZE")[0]
    return memory_size // 2 - 4_096 - heap_base - ALLOCATOR_HEADER_BYTES


def _assert_startup_words(
    runtime: MegaForthRuntime,
    *,
    here_before: int,
    latest_before: int,
    index_before: DictionaryIndexState,
) -> int:
    aligned_here = _aligned_tile(here_before)
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 27
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == 10
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_STATIC_GROWTH
    assert runtime.dictionary.here == aligned_here + HOSTED_STATIC_GROWTH
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS

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
        expected_type = CreatedDefinition if definer == "CREATE" else ColonDefinition
        assert isinstance(word.implementation, expected_type)
        assert runtime.find(word.name) is word
        prior_header = word.header_address

    assert published[0].header_address == aligned_here
    assert runtime.dictionary.latest == published[-1].header_address
    index_after = runtime.dictionary_index.state
    assert index_after.base == index_before.base
    assert index_after.slots == index_before.slots
    assert index_after.flags == index_before.flags
    assert index_after.count == index_before.count + len(SOURCE_LEDGER)
    assert runtime.memory.read_bytes(_body(runtime, "_AUTOEXEC-NAME"), 10) == (
        AUTOEXEC_NAME
    )
    return aligned_here


def _assert_restored_startup_frames(
    runtime: MegaForthRuntime,
    loader_before: tuple[int, ...],
) -> None:
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_REQ-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert runtime.spinlocks.owner(5) is None
    assert runtime.spinlocks.owner(2) is None


def test_startup_slice_is_exact_and_no_disk_load_effects_are_exact() -> None:
    runtime = _load_module_system()
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    index_before = runtime.dictionary_index.state
    namebuf_before = runtime.memory.read_bytes(_body(runtime, "NAMEBUF"), 24)
    loader_before = _loader_globals(runtime)
    xmem_before = tuple(
        _variable(runtime, name)
        for name in ("XMEM-HERE", "XMEM-FLOOR", "XMEM-FL")
    )
    registry_before = tuple(
        _execute(runtime, name)[0]
        for name in (
            "_MOD-BUCKETS",
            "_MOD-BUCKET-COUNT",
            "_MOD-COUNT",
            "_MOD-BUCKETS-HEAP?",
        )
    )
    storage_before = (runtime.storage.image_bytes, runtime.storage.completion)
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    assert _variable(runtime, "HEAP-INIT") == 0
    assert runtime.drain_uart_output() == b""

    _evaluate_startup(runtime)

    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    aligned_here = _assert_startup_words(
        runtime,
        here_before=here_before,
        latest_before=latest_before,
        index_before=index_before,
    )
    heap_base = aligned_here + LATE_DICTIONARY_RESERVE
    assert _variable(runtime, "HEAP-INIT") == 1
    assert _variable(runtime, "HEAP-BASE") == heap_base
    assert _variable(runtime, "HEAP-FREE") == heap_base
    assert runtime.memory.read64(heap_base) == 0
    assert runtime.memory.read64(heap_base + 8) == _expected_heap_size(
        runtime,
        heap_base,
    )
    assert runtime.memory.read64(heap_base + 16) == 0
    assert _variable(runtime, "A-SIZE") == 16
    assert _variable(runtime, "A-PREV") == 0
    assert _variable(runtime, "A-CURR") == heap_base
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "ULAND") == 0
    assert _variable(runtime, "U-INIT-DONE") == 0
    assert runtime.memory.read_bytes(_body(runtime, "NAMEBUF"), 24) == namebuf_before
    assert tuple(
        _variable(runtime, name)
        for name in ("XMEM-HERE", "XMEM-FLOOR", "XMEM-FL")
    ) == xmem_before
    assert tuple(
        _execute(runtime, name)[0]
        for name in (
            "_MOD-BUCKETS",
            "_MOD-BUCKET-COUNT",
            "_MOD-COUNT",
            "_MOD-BUCKETS-HEAP?",
        )
    ) == registry_before
    assert (runtime.storage.image_bytes, runtime.storage.completion) == storage_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.timer.counter > counter_before
    _assert_restored_startup_frames(runtime, loader_before)
    assert runtime.drain_uart_output() == STARTUP_BANNER + b"\r\n"


def test_startup_attached_invalid_media_reports_and_continues() -> None:
    runtime = _load_module_system()
    runtime.storage.attach(bytearray(SECTOR_SIZE))
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    index_before = runtime.dictionary_index.state
    loader_before = _loader_globals(runtime)

    _evaluate_startup(runtime)

    _assert_startup_words(
        runtime,
        here_before=here_before,
        latest_before=latest_before,
        index_before=index_before,
    )
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "HEAP-INIT") == 1
    assert runtime.storage.completion == 1
    assert runtime.drain_uart_output() == (
        STARTUP_BANNER + b" Invalid MP64FS\r\n\r\n"
    )
    _assert_restored_startup_frames(runtime, loader_before)


def test_startup_loads_valid_mp64fs_and_tolerates_missing_autoexec() -> None:
    runtime = _load_module_system()
    image = _formatted_image()
    runtime.storage.attach(image)
    media_before = runtime.storage.image_bytes
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    index_before = runtime.dictionary_index.state
    loader_before = _loader_globals(runtime)
    runtime.memory.fill(_body(runtime, "NAMEBUF"), 24, 0xA5)

    _evaluate_startup(runtime)

    _assert_startup_words(
        runtime,
        here_before=here_before,
        latest_before=latest_before,
        index_before=index_before,
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "HEAP-INIT") == 1
    assert runtime.storage.completion == 6
    assert runtime.memory.read_bytes(_body(runtime, "NAMEBUF"), 24) == (
        AUTOEXEC_NAME + bytes(14)
    )
    assert _execute(runtime, "_MOD-COUNT") == (0,)
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == (
        STARTUP_BANNER + b" MP64FS loaded\r\n\r\n"
    )
    _assert_restored_startup_frames(runtime, loader_before)


def test_startup_runs_autoexec_through_the_module_loader_once() -> None:
    autoexec_source = b"PROVIDED boot.autoexec\n1 BOOT-HIT +!\n"
    runtime = _load_module_system()
    runtime.evaluate(
        b"VARIABLE BOOT-HIT 0 BOOT-HIT !",
        source_name="startup-autoexec-observer.f",
    )
    image = _module_image(((AUTOEXEC_NAME, autoexec_source),))
    runtime.storage.attach(image)
    media_before = runtime.storage.image_bytes
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    index_before = runtime.dictionary_index.state
    loader_before = _loader_globals(runtime)
    runtime.memory.fill(_body(runtime, "NAMEBUF"), 24, 0xA5)

    _evaluate_startup(runtime)

    _assert_startup_words(
        runtime,
        here_before=here_before,
        latest_before=latest_before,
        index_before=index_before,
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "HEAP-INIT") == 1
    assert runtime.storage.completion == 7
    assert _variable(runtime, "BOOT-HIT") == 1
    assert _stack_eval(runtime, b"MODULE? boot.autoexec") == (TRUE,)
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert runtime.memory.read_bytes(_body(runtime, "NAMEBUF"), 24) == (
        AUTOEXEC_NAME + bytes(14)
    )
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == (
        STARTUP_BANNER
        + b" MP64FS loaded\r\n"
        + b" Running autoexec.f...\r\n\r\n"
    )
    _assert_restored_startup_frames(runtime, loader_before)

    _execute(runtime, "_AUTOEXEC-RUN")
    assert _variable(runtime, "BOOT-HIT") == 1
    assert _execute(runtime, "_MOD-COUNT") == (1,)
    assert runtime.drain_uart_output() == b" Running autoexec.f...\r\n"
    _assert_restored_startup_frames(runtime, loader_before)
