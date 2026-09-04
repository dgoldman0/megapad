"""Unchanged-source acceptance for MP64FS free-space reporting."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import TRUE
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_cat import _load_mp64fs_cat
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
    _store,
)
from tests.simulator.test_kdos_mp64fs_mutation import _mount
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-mp64fs-free-5437-5471.f"
)

FIRST_LINE = 5437
LAST_LINE = 5471
SLICE_SHA256 = (
    "6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c"
)
SLICE_GIT_BLOB = "1884c81ba2b8aa48082d472250f13a2265fd1def"

DEFINITIONS = (
    b"LF-BEST",
    b"LF-RUN",
    b"FS-LARGEST-FREE",
    b"FS-FREE",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 984
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    LOOP DROP ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── SAVE-BUFFER — save buffer data to a named file "
        "──────────────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_free(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_free(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_free(_load_mp64fs_cat(image))


def test_free_space_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_cat(_formatted_image())
    runtime.rtc.set_epoch_ms(0x0102_0304_0506_0708)
    before = _mount_snapshot(runtime)
    epoch_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    _evaluate_mp64fs_free(runtime)

    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _variable(runtime, "LF-BEST") == 0
    assert _variable(runtime, "LF-RUN") == 0
    assert _mount_snapshot(runtime) == before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == epoch_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_fs_free_absent_filesystem_preserves_largest_run_scratch() -> None:
    runtime = _load_mp64fs_free()
    _store(runtime, "LF-BEST", 0x111)
    _store(runtime, "LF-RUN", 0x222)

    assert _execute(runtime, "FS-FREE") == ()

    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _variable(runtime, "LF-BEST") == 0x111
    assert _variable(runtime, "LF-RUN") == 0x222
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_free_space_report_uses_cached_global_fragmentation_state() -> None:
    image = _formatted_image(20)
    _write_entry(
        image,
        0,
        name=b"root\0",
        start=14,
        count=1,
        used=1,
        entry_type=1,
    )
    _write_entry(
        image,
        1,
        name=b"docs\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    _write_entry(
        image,
        2,
        name=b"child\0",
        start=17,
        count=2,
        used=1,
        entry_type=1,
        parent=1,
    )
    runtime = _load_mp64fs_free(image)
    _mount(runtime)
    _store(runtime, "CWD", 1)
    _store(runtime, "LF-BEST", 0x111)
    _store(runtime, "LF-RUN", 0x222)
    before = _mount_snapshot(runtime)
    completion = runtime.storage.completion

    assert _execute(runtime, "FS-LARGEST-FREE") == (2,)
    assert _variable(runtime, "LF-BEST") == 2
    assert _variable(runtime, "LF-RUN") == 1
    assert runtime.storage.completion == completion
    assert runtime.drain_uart_output() == b""

    runtime.storage.attach(_formatted_image(20))
    replacement = runtime.storage.image_bytes
    assert _execute(runtime, "FS-FREE") == ()

    assert runtime.drain_uart_output() == (
        b"3  free sectors (1536  bytes)\r\n"
        b" Largest contiguous: 2  sectors\r\n"
        b"3  files, 128  max\r\n"
    )
    assert _variable(runtime, "LF-BEST") == 2
    assert _variable(runtime, "LF-RUN") == 1
    assert _variable(runtime, "FS-OK") == TRUE
    assert _mount_snapshot(runtime) == before
    assert runtime.storage.image_bytes == replacement
    assert runtime.storage.completion == completion
    assert _diagnostics(runtime) == (0, 12, 0)

    assert _execute(runtime, "HEX") == ()
    assert _execute(runtime, "FS-FREE") == ()
    assert runtime.drain_uart_output() == (
        b"3  free sectors (600  bytes)\r\n"
        b" Largest contiguous: 2  sectors\r\n"
        b"3  files, 80  max\r\n"
    )
    assert runtime.storage.completion == completion
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.spinlocks.owner(2) is None
