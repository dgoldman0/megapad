"""Unchanged-source acceptance for KDOS application loading and ANSI output."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import u64
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CMD_READ,
    STORAGE_RESULT_MEDIA_REMOVED,
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
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
)
from tests.simulator.test_kdos_mp64fs_load import (
    _install_loader_trace,
    _load_mp64fs_load,
    _load_mp64fs_load_service,
    _loader_globals,
    _sector_allocation,
    _seed_loader_globals,
)
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
    / "kdos-application-loading-5945-6059.f"
)

FIRST_LINE = 5945
LAST_LINE = 6059
SLICE_BYTES = 2_892
SLICE_SHA256 = (
    "1c671d6f3677d9fb65e7c5b20a6af1b3d10323b28b5abb10d827cd80a58e5bb2"
)
SLICE_GIT_BLOB = "c95aa1a3385d10587ed42292328b0c7c323e702f"

SOURCE_LEDGER = (
    (":", b"_APP-MPU-ON"),
    (":", b"_APP-MPU-OFF"),
    (":", b"APP-EVAL"),
    (":", b"_APP-LOAD-WALK"),
    (":", b"APP-LOAD"),
    (":", b"ESC"),
    (":", b"CSI"),
    (":", b".N"),
    (":", b"SGR"),
    (":", b"RESET-COLOR"),
    (":", b"DIM"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)


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


def _evaluate_application_loading(
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


def _load_application_loading(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_application_loading(_load_mp64fs_load(image))


def _assert_loader_released(
    runtime: MegaForthRuntime,
    *,
    heap_before: int,
    globals_before: tuple[int, ...],
    expected_mpu: tuple[int, int] = (0, 0),
) -> None:
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == globals_before
    assert (runtime.mpu_base, runtime.mpu_limit) == expected_mpu
    assert runtime.privilege_level == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_application_slice_is_exact_and_load_time_pure() -> None:
    runtime = _load_mp64fs_load()
    _execute(runtime, "MPU-BASE!", 0x1111)
    _execute(runtime, "MPU-LIMIT!", 0x2222)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    runtime = _evaluate_application_loading(runtime)

    assert len(SOURCE_LEDGER) == 11
    assert all(definer == ":" for definer, _name in SOURCE_LEDGER)
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert runtime.mpu_base == 0x1111
    assert runtime.mpu_limit == 0x2222
    assert runtime.privilege_level == 0
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_mpu_helpers_and_app_eval_use_then_clear_the_application_window() -> None:
    runtime = _load_application_loading()
    external_end = _constant(runtime, "EXT-MEM-BASE") + _constant(
        runtime,
        "EXT-MEM-SIZE",
    )

    _execute(runtime, "MPU-BASE!", 0x1111)
    _execute(runtime, "MPU-LIMIT!", 0x2222)

    assert _execute(runtime, "_APP-MPU-ON") == ()
    assert runtime.mpu_base == 0
    assert runtime.mpu_limit == external_end
    assert _execute(runtime, "_APP-MPU-OFF") == ()
    assert runtime.mpu_base == 0
    assert runtime.mpu_limit == 0

    runtime.evaluate(
        b"VARIABLE APP-SEEN-BASE VARIABLE APP-SEEN-LIMIT "
        b"VARIABLE APP-SEEN-PRIV",
        source_name="app-eval-observers",
    )
    source = (
        b"MPU-BASE@ APP-SEEN-BASE ! "
        b"MPU-LIMIT@ APP-SEEN-LIMIT ! "
        b"PRIV@ APP-SEEN-PRIV ! "
        b": APP-EVALUATED 42 ; 73"
    )
    source_word = runtime.define_created(
        "APP-EVAL-SOURCE",
        initial_body=source,
    )

    assert _execute(
        runtime,
        "APP-EVAL",
        source_word.body_address,
        len(source),
    ) == (73,)

    assert _variable(runtime, "APP-SEEN-BASE") == 0
    assert _variable(runtime, "APP-SEEN-LIMIT") == external_end
    assert _variable(runtime, "APP-SEEN-PRIV") == 0
    assert _execute(runtime, "APP-EVALUATED") == (42,)
    assert runtime.mpu_base == 0
    assert runtime.mpu_limit == 0
    assert runtime.privilege_level == 0
    assert runtime.drain_uart_output() == b""

    missing = b"missing-app-eval-token"
    missing_word = runtime.define_created(
        "MISSING-APP-EVAL-SOURCE",
        initial_body=missing,
    )
    assert _execute(
        runtime,
        "APP-EVAL",
        missing_word.body_address,
        len(missing),
    ) == ()
    assert _variable(runtime, "EVAL-STATUS") == 1
    assert runtime.mpu_base == runtime.mpu_limit == 0
    assert runtime.drain_uart_output() == (
        b"missing-app-eval-token ? (not found)\n"
    )


def test_caught_app_eval_throw_leaves_the_mpu_window_enabled() -> None:
    runtime = _load_application_loading()
    external_end = _constant(runtime, "EXT-MEM-BASE") + _constant(
        runtime,
        "EXT-MEM-SIZE",
    )
    source = b"-91 THROW"
    source_word = runtime.define_created(
        "THROWING-APP-EVAL-SOURCE",
        initial_body=source,
    )
    runtime.evaluate(
        (
            f": RUN-THROWING-APP-EVAL {source_word.body_address} "
            f"{len(source)} APP-EVAL ;"
        ).encode("ascii"),
        source_name="throwing-app-eval-wrapper",
    )
    wrapper = runtime.find("RUN-THROWING-APP-EVAL")
    assert wrapper is not None

    assert _execute(runtime, "CATCH", wrapper.xt) == (u64(-91),)

    # APP-EVAL has no guarded cleanup.  CATCH resumes past EVALUATE before
    # SYS-EXIT and _APP-MPU-OFF can run, leaving the otherwise inert window.
    assert runtime.mpu_base == 0
    assert runtime.mpu_limit == external_end
    assert _variable(runtime, "EVAL-DEPTH") == 1
    assert runtime.privilege_level == 0
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""

    assert _execute(runtime, "EVALUATOR-UNWIND", 0) == ()
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _execute(runtime, "_APP-MPU-OFF") == ()
    assert runtime.mpu_limit == 0


def test_app_load_runs_lf_terminated_source_and_nested_load_then_restores() -> None:
    image = _formatted_image(20)
    app_source = (
        b"\\ p\n" * 125
        + b": APP-HEAD 101 ;\n"
        b"VARIABLE APP-LOAD-SEEN-LIMIT\n"
        b"VARIABLE APP-LOAD-SEEN-PRIV\n"
        b"MPU-LIMIT@ APP-LOAD-SEEN-LIMIT !\n"
        b"PRIV@ APP-LOAD-SEEN-PRIV !\n"
        b"LOAD inner.f\n"
        b": APP-TAIL 303 ;\n"
    )
    assert SECTOR_SIZE < len(app_source) <= 2 * SECTOR_SIZE
    app_allocation = _sector_allocation(app_source, 2)
    inner_source = b": INNER-FROM-APP 202 ;\n"
    _write_entry(
        image,
        6,
        name=b"app.f\0",
        start=14,
        count=1,
        used=len(app_source),
        entry_type=3,
        secondary_start=17,
        secondary_count=1,
    )
    _write_entry(
        image,
        7,
        name=b"inner.f\0",
        start=15,
        count=1,
        used=len(inner_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = app_allocation[:SECTOR_SIZE]
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = app_allocation[SECTOR_SIZE:]
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(
        inner_source,
        1,
    )

    runtime = _load_application_loading(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"APP-LOAD app.f", source_name="nested-app-load")

    assert _execute(runtime, "APP-HEAD") == (101,)
    assert _execute(runtime, "INNER-FROM-APP") == (202,)
    assert _execute(runtime, "APP-TAIL") == (303,)
    # Image-backed cumulative fixtures use the admitted no-XMEM profile, so
    # this exercises the source's Bank-0-only branch as well as the external
    # branch covered by APP-EVAL above.
    assert _variable(runtime, "APP-LOAD-SEEN-LIMIT") == _constant(
        runtime,
        "MEM-SIZE",
    )
    assert _variable(runtime, "APP-LOAD-SEEN-PRIV") == 0
    assert runtime.find("LOAD-PADDING-RAN") is None
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 1313
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:6] == mount_before[:6]
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.storage.completion == completion_before + 3
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_app_load_cleanly_rejects_absent_missing_and_empty_files() -> None:
    runtime = _load_application_loading()
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    expected_mpu = (0x1111, 0x2222)
    _execute(runtime, "MPU-BASE!", expected_mpu[0])
    _execute(runtime, "MPU-LIMIT!", expected_mpu[1])

    runtime.evaluate(b"APP-LOAD 41", source_name="app-load-no-filesystem")
    assert runtime.main_context.data.snapshot() == (41,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _loader_globals(runtime) == globals_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    assert (runtime.mpu_base, runtime.mpu_limit) == expected_mpu

    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"empty.f\0",
        start=14,
        count=1,
        used=0,
        entry_type=3,
    )
    runtime.storage.attach(image)
    _mount(runtime)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]

    runtime.evaluate(b"APP-LOAD missing.f", source_name="app-load-missing")
    assert runtime.drain_uart_output() == b" Not found: missing.f\r\n"
    runtime.evaluate(b"APP-LOAD empty.f", source_name="app-load-empty")
    assert runtime.drain_uart_output() == b" Empty file\r\n"
    runtime.evaluate(
        b"APP-LOAD pkg/missing.f",
        source_name="app-load-literal-slash-miss",
    )
    assert runtime.drain_uart_output() == b" Not found: pkg/missing.f\r\n"

    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
        expected_mpu=expected_mpu,
    )
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before


def test_app_load_allocation_failure_restores_without_reading_or_mpu() -> None:
    source = b": APP-MUST-NOT-LOAD 99 ;\n"
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"oom.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(source, 1)

    runtime = _load_mp64fs_load(image)
    runtime.evaluate(b": ALLOCATE DROP 0 -1 ;", source_name="failing-allocate")
    _evaluate_application_loading(runtime)
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    expected_mpu = (0x3333, 0x4444)
    _execute(runtime, "MPU-BASE!", expected_mpu[0])
    _execute(runtime, "MPU-LIMIT!", expected_mpu[1])
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"APP-LOAD oom.f", source_name="app-allocation-failure")

    assert runtime.drain_uart_output() == b" File buffer allocation failed\r\n"
    assert runtime.find("APP-MUST-NOT-LOAD") is None
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
        expected_mpu=expected_mpu,
    )
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before


def test_app_load_throw_cleans_up_and_the_loader_remains_reusable() -> None:
    bad_source = (
        b": BEFORE-APP-THROW 17 ;\n"
        b"-77 THROW\n"
        b": AFTER-APP-THROW 99 ;\n"
    )
    good_source = b": APP-LOAD-RECOVERED 55 ;\n"
    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"bad.f\0",
        start=14,
        count=1,
        used=len(bad_source),
        entry_type=3,
    )
    _write_entry(
        image,
        7,
        name=b"good.f\0",
        start=15,
        count=1,
        used=len(good_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(
        bad_source,
        1,
    )
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(
        good_source,
        1,
    )

    runtime = _load_application_loading(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"' APP-LOAD CATCH bad.f", source_name="caught-app-throw")

    assert runtime.main_context.data.snapshot() == (u64(-77),)
    runtime.main_context.data.clear()
    assert runtime.find("BEFORE-APP-THROW") is not None
    assert runtime.find("AFTER-APP-THROW") is None
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    assert runtime.drain_uart_output() == b""

    runtime.evaluate(b"APP-LOAD good.f", source_name="app-after-caught-throw")

    assert _execute(runtime, "APP-LOAD-RECOVERED") == (55,)
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 2313
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:6] == mount_before[:6]
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.storage.completion == completion_before + 2
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b""


def test_app_load_ignores_undefined_and_unfinished_evaluator_state() -> None:
    unchecked_source = (
        b": BEFORE-APP-UNDEFINED 1 ;\n"
        b"missing-app-load-token\n"
        b": AFTER-APP-UNDEFINED 2 ;\n"
    )
    unfinished_source = b": APP-LEFT-OPEN 123\n"
    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"unchecked.f\0",
        start=14,
        count=1,
        used=len(unchecked_source),
        entry_type=3,
    )
    _write_entry(
        image,
        7,
        name=b"unfinished.f\0",
        start=15,
        count=1,
        used=len(unfinished_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(
        unchecked_source,
        1,
    )
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(
        unfinished_source,
        1,
    )

    runtime = _load_application_loading(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion
    media_before = runtime.storage.image_bytes

    runtime.evaluate(
        b"APP-LOAD unchecked.f",
        source_name="unchecked-app-status",
    )

    assert _execute(runtime, "BEFORE-APP-UNDEFINED") == (1,)
    assert _execute(runtime, "AFTER-APP-UNDEFINED") == (2,)
    assert _variable(runtime, "EVAL-STATUS") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 13
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    assert runtime.drain_uart_output() == (
        b"missing-app-load-token ? (not found)\n"
    )

    runtime.evaluate(
        b"APP-LOAD unfinished.f",
        source_name="unfinished-app-state",
    )

    assert runtime.find("APP-LEFT-OPEN") is None
    assert _variable(runtime, "EVAL-STATUS") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 1313
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    assert _execute(runtime, "EVALUATE-FINISH") == (4,)
    assert _execute(runtime, "EVALUATOR-RESET") == ()
    assert runtime.storage.completion == completion_before + 2
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b""


def test_app_load_read_abort_precedes_guard_and_mpu_setup() -> None:
    source = b"\\ p\n" * 125 + b": APP-MUST-NOT-REACH 42 ;\n"
    assert SECTOR_SIZE < len(source) <= 2 * SECTOR_SIZE
    allocation = _sector_allocation(source, 2)
    image = _formatted_image(20)
    _write_entry(
        image,
        6,
        name=b"split.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
        secondary_start=17,
        secondary_count=1,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = allocation[:SECTOR_SIZE]
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = allocation[SECTOR_SIZE:]

    class SwapOnSecondApplicationRead(HostedStorageService):
        def __init__(self, payload: bytes | bytearray) -> None:
            super().__init__(payload)
            self.armed = False
            self.read_acceptances = 0

        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            if not self.armed or command != STORAGE_CMD_READ:
                return
            assert expected_generation == self.media_generation
            self.read_acceptances += 1
            if self.read_acceptances == 2:
                self.attach(self.image_bytes)

    storage = SwapOnSecondApplicationRead(image)
    runtime = _evaluate_application_loading(
        _load_mp64fs_load_service(storage)
    )
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    expected_mpu = (0x5555, 0x6666)
    _execute(runtime, "MPU-BASE!", expected_mpu[0])
    _execute(runtime, "MPU-LIMIT!", expected_mpu[1])
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = storage.image_bytes
    completion_before = storage.completion
    storage.armed = True

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"APP-LOAD split.f",
            source_name="app-second-extent-stale",
        )

    loaded_buffer = _variable(runtime, "LD-BUF")
    assert runtime.memory.read_bytes(loaded_buffer, SECTOR_SIZE) == allocation[
        :SECTOR_SIZE
    ]
    assert _variable(runtime, "LD-SZ") == len(source)
    assert _variable(runtime, "LD-CUR") == globals_before[2]
    assert _variable(runtime, "LD-LEN") == globals_before[3]
    assert _variable(runtime, "_LD-SP") == 56
    assert _variable(runtime, "CWD") == 0xFF
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    assert _execute(runtime, "HEAP-FREE-BYTES")[0] < heap_before
    assert (runtime.mpu_base, runtime.mpu_limit) == expected_mpu
    expected_ior = _execute(
        runtime,
        "IOR-FROM-BLOCK-RESULT",
        STORAGE_RESULT_MEDIA_REMOVED,
    )[0]
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_MEDIA_REMOVED,
        0,
        expected_ior,
    )
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:2] == mount_before[:2]
    assert mount_after[3:6] == mount_before[3:6]
    assert storage.read_acceptances == 2
    assert storage.completion == completion_before + 2
    assert storage.image_bytes == media_before
    assert runtime.find("APP-MUST-NOT-REACH") is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert runtime.spinlocks.owner(2) is None


def test_unterminated_app_source_executes_sector_padding_through_next_lf() -> None:
    used_source = b": APP-REAL 1 ; "
    padding_source = b": APP-PADDING-RAN 2 ;\n"
    allocation = used_source + padding_source
    allocation += bytes(SECTOR_SIZE - len(allocation))
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"padded.f\0",
        start=14,
        count=1,
        used=len(used_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = allocation

    runtime = _load_application_loading(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    globals_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion

    runtime.evaluate(b"APP-LOAD padded.f", source_name="app-padding-defect")

    assert _execute(runtime, "APP-REAL") == (1,)
    assert _execute(runtime, "APP-PADDING-RAN") == (2,)
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 13
    _assert_loader_released(
        runtime,
        heap_before=heap_before,
        globals_before=globals_before,
    )
    assert runtime.storage.completion == completion_before + 1
    assert runtime.drain_uart_output() == b""


def test_ansi_helpers_emit_each_decimal_branch_and_canonical_sequences() -> None:
    runtime = _load_application_loading()

    for value, expected in (
        (-12, b"-12"),
        (0, b"0"),
        (9, b"9"),
        (10, b"10"),
        (99, b"99"),
        (100, b"100"),
        (999, b"999"),
        (1000, b"1000 "),
    ):
        assert _execute(runtime, ".N", value) == ()
        assert runtime.drain_uart_output() == expected

    assert _execute(runtime, "ESC") == ()
    assert runtime.drain_uart_output() == b"\x1b"
    assert _execute(runtime, "CSI") == ()
    assert runtime.drain_uart_output() == b"\x1b["
    assert _execute(runtime, "SGR", 31) == ()
    assert runtime.drain_uart_output() == b"\x1b[31m"
    assert _execute(runtime, "RESET-COLOR") == ()
    assert runtime.drain_uart_output() == b"\x1b[0m"
    assert _execute(runtime, "DIM") == ()
    assert runtime.drain_uart_output() == b"\x1b[2m"
