"""Unchanged-source acceptance for KDOS pipeline bundles."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64
from simulator.ir import Call, Literal, Return
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    MegaForthRuntime,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_interactive_screens import (
    _address,
    _store_variable,
)
from tests.simulator.test_kdos_ports_dashboard_help import (
    _load_ports_dashboard_help,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-pipeline-bundles-8944-9122.f"
)

FIRST_LINE = 8944
FIXTURE_LAST_LINE = 9122
LAST_LINE = 9121
FIXTURE_BYTES = 5_873
FIXTURE_SHA256 = (
    "8791e5eecef059d052ecd8b69976317857c41c29ae475e18cc53d79761d8b922"
)
FIXTURE_GIT_BLOB = "3690e82c7a15e69fa69c84186fdda0caa5937d42"
SLICE_BYTES = 5_801
SLICE_SHA256 = (
    "370c6c6d17470ae7ea0c8a94ca5ede4ddcae04a8c9e0badcb007cc5358ef919f"
)
SLICE_GIT_BLOB = "a7f49a7d29bbfa61d043dae73854924e74f4b2f8"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 832

SOURCE_LEDGER = (
    ("CONSTANT", b"FTYPE-BUNDLE", 0),
    ("VARIABLE", b"BDL-ACTIVE", CELL_BYTES),
    ("VARIABLE", b"BDL-DRY", CELL_BYTES),
    ("VARIABLE", b"BDL-VER", CELL_BYTES),
    ("VARIABLE", b"BDL-NBUFS", CELL_BYTES),
    ("VARIABLE", b"BDL-NKERNS", CELL_BYTES),
    ("VARIABLE", b"BDL-NPIPES", CELL_BYTES),
    ("VARIABLE", b"BDL-SCHED-P", CELL_BYTES),
    ("VARIABLE", b"BDL-SCHED-I", CELL_BYTES),
    ("VARIABLE", b"BDL-SCHED-F", CELL_BYTES),
    ("VARIABLE", b"BDL-POL-PERM", CELL_BYTES),
    ("VARIABLE", b"BDL-POL-RET", CELL_BYTES),
    ("VARIABLE", b"BDL-POL-EXP", CELL_BYTES),
    ("VARIABLE", b"BDL-SCR-DEF", CELL_BYTES),
    ("VARIABLE", b"BDL-SCR-MASK", CELL_BYTES),
    (":", b"BDL-RESET", 0),
    (":", b"BDL-BEGIN", 0),
    (":", b"BDL-BUF", 0),
    (":", b"BDL-KERN", 0),
    (":", b"BDL-PIPE", 0),
    (":", b"BDL-SCHED", 0),
    (":", b"BDL-POLICY", 0),
    (":", b"BDL-SCREEN", 0),
    (":", b"BDL-END", 0),
    (":", b"BUNDLE-LOAD", 0),
    (":", b"BUNDLE-INFO", 0),
    (":", b".BUNDLE", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
INITIALIZERS = {
    "BDL-ACTIVE": 0,
    "BDL-DRY": 0,
    "BDL-VER": 0,
    "BDL-NBUFS": 0,
    "BDL-NKERNS": 0,
    "BDL-NPIPES": 0,
    "BDL-SCHED-P": MASK64,
    "BDL-SCHED-I": 0,
    "BDL-SCHED-F": 0,
    "BDL-POL-PERM": 0,
    "BDL-POL-RET": 0,
    "BDL-POL-EXP": 3,
    "BDL-SCR-DEF": 1,
    "BDL-SCR-MASK": 255,
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
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa718  Ring Buffer Primitives\n"
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_pipeline_bundles(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_pipeline_bundles() -> MegaForthRuntime:
    return _evaluate_pipeline_bundles(_load_ports_dashboard_help())


def _registry_state(runtime: MegaForthRuntime) -> tuple[object, ...]:
    return (
        _variable(runtime, "BUF-COUNT"),
        _variable(runtime, "BUF-HEAD"),
        _variable(runtime, "KERN-COUNT"),
        runtime.memory.read_bytes(_address(runtime, "KERN-TABLE"), 32 * 8),
        _variable(runtime, "PIPE-COUNT"),
        runtime.memory.read_bytes(_address(runtime, "PIPE-TABLE"), 8 * 8),
    )


def test_pipeline_bundle_slice_is_exact_linked_initialized_and_load_time_pure() -> None:
    runtime = _load_ports_dashboard_help()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    _store_variable(runtime, "TIME-SLICE", 0x1234_5678)
    _store_variable(runtime, "SCREEN-ID", 6)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    registry_before = _registry_state(runtime)
    applied_before = (
        _variable(runtime, "TIME-SLICE"),
        _variable(runtime, "SCREEN-ID"),
    )
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_pipeline_bundles(runtime)

    assert len(SOURCE_LEDGER) == 27
    assert sum(
        definer == "CONSTANT" for definer, _name, _body in SOURCE_LEDGER
    ) == 1
    assert sum(
        definer == "VARIABLE" for definer, _name, _body in SOURCE_LEDGER
    ) == 14
    assert sum(definer == ":" for definer, _name, _body in SOURCE_LEDGER) == 12
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 261
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == 112
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
        expected_type = {
            "CONSTANT": ConstantDefinition,
            "VARIABLE": CreatedDefinition,
            ":": ColonDefinition,
        }[definer]
        assert isinstance(word.implementation, expected_type)
        prior_header = word.header_address

    assert _constant(runtime, "FTYPE-BUNDLE") == 7
    assert {
        name: _variable(runtime, name) for name in INITIALIZERS
    } == INITIALIZERS
    assert _variable(runtime, "NSCREENS") == 9
    assert _variable(runtime, "BDL-SCR-MASK") == (1 << 8) - 1
    assert _variable(runtime, "BDL-SCR-MASK") != (1 << 9) - 1
    assert _registry_state(runtime) == registry_before
    assert (
        _variable(runtime, "TIME-SLICE"),
        _variable(runtime, "SCREEN-ID"),
    ) == applied_before
    assert runtime.timer.counter > counter_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.spinlocks.owners == locks_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"


def test_bundle_reset_begin_configuration_and_live_end_are_state_exact() -> None:
    runtime = _load_pipeline_bundles()
    for name in INITIALIZERS:
        _store_variable(runtime, name, 0xA5A5_A5A5_A5A5_A5A5)
    _store_variable(runtime, "BDL-DRY", 0xD00D)

    assert _execute(runtime, "BDL-RESET") == ()
    assert {
        name: _variable(runtime, name)
        for name in INITIALIZERS
        if name != "BDL-DRY"
    } == {
        name: value for name, value in INITIALIZERS.items() if name != "BDL-DRY"
    }
    assert _variable(runtime, "BDL-DRY") == 0xD00D

    _store_variable(runtime, "BDL-DRY", 0)
    assert _execute(runtime, "BDL-BEGIN", 4) == ()
    assert _variable(runtime, "BDL-ACTIVE") == 1
    assert _variable(runtime, "BDL-VER") == 4
    assert _execute(runtime, "BDL-SCHED", 2, 12_345, 3) == ()
    assert _execute(runtime, "BDL-POLICY", 3, 9, 2) == ()
    assert _execute(runtime, "BDL-SCREEN", 4, 0x55) == ()
    _store_variable(runtime, "TIME-SLICE", 777)
    _store_variable(runtime, "SCREEN-ID", 8)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )

    assert _execute(runtime, "BDL-END") == ()

    assert runtime.drain_uart_output() == (
        b"\r\n Bundle v4  loaded: 0  bufs 0  kerns 0  pipes\r\n"
    )
    assert _variable(runtime, "BDL-ACTIVE") == 0
    assert _variable(runtime, "BDL-DRY") == 0
    assert _variable(runtime, "BDL-VER") == 4
    assert (
        _variable(runtime, "BDL-SCHED-P"),
        _variable(runtime, "BDL-SCHED-I"),
        _variable(runtime, "BDL-SCHED-F"),
    ) == (2, 12_345, 3)
    assert (
        _variable(runtime, "BDL-POL-PERM"),
        _variable(runtime, "BDL-POL-RET"),
        _variable(runtime, "BDL-POL-EXP"),
    ) == (3, 9, 2)
    assert (
        _variable(runtime, "BDL-SCR-DEF"),
        _variable(runtime, "BDL-SCR-MASK"),
    ) == (4, 0x55)
    assert _variable(runtime, "TIME-SLICE") == 12_345
    assert _variable(runtime, "SCREEN-ID") == 4
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before


def test_dry_bundle_declarations_only_publish_transient_word_names() -> None:
    runtime = _load_pipeline_bundles()
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    registry_before = _registry_state(runtime)
    media_before = runtime.storage.image_bytes
    _store_variable(runtime, "BDL-DRY", 1)
    _store_variable(runtime, "TIME-SLICE", 777)
    _store_variable(runtime, "SCREEN-ID", 8)

    result = runtime.evaluate(
        b"2 BDL-BEGIN\n"
        b"0 1 3 BDL-BUF dry-buffer\n"
        b"1 1 2 3 BDL-KERN dry-kernel\n"
        b"2 BDL-PIPE dry-pipe\n"
        b"4 123 3 BDL-SCHED\n"
        b"1 8 2 BDL-POLICY\n"
        b"5 170 BDL-SCREEN\n"
        b"BDL-END\n",
        source_name="dry-pipeline-bundle.f",
    )

    assert result.definitions == ()
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert runtime.dictionary.words == words_before
    assert runtime.memory.read_bytes(here_before, 10) == b"\x08dry-pipe\x00"
    assert _registry_state(runtime) == registry_before
    assert runtime.storage.image_bytes == media_before
    for name in ("dry-buffer", "dry-kernel", "dry-pipe"):
        assert runtime.find(name) is None
    assert (
        _variable(runtime, "BDL-NBUFS"),
        _variable(runtime, "BDL-NKERNS"),
        _variable(runtime, "BDL-NPIPES"),
    ) == (1, 1, 1)
    assert _variable(runtime, "BDL-ACTIVE") == 0
    assert _variable(runtime, "BDL-DRY") == 1
    assert _variable(runtime, "TIME-SLICE") == 777
    assert _variable(runtime, "SCREEN-ID") == 8
    assert runtime.drain_uart_output() == (
        b" --- Bundle v2  ---\r\n"
        b"   Buffers  : 1 \r\n"
        b"   Kernels  : 1 \r\n"
        b"   Pipelines: 1 \r\n"
        b"   Schedule : pipe 4   interval=123   auto  repeat\r\n"
        b"   Policy   : perms=1  ret=8  exp=2 \r\n"
        b"   Screen   : default=5  mask=170 \r\n"
    )


def test_tiny_live_bundle_uses_the_ordinary_buffer_kernel_pipeline_paths() -> None:
    runtime = _load_pipeline_bundles()
    buffer_count = _variable(runtime, "BUF-COUNT")
    kernel_count = _variable(runtime, "KERN-COUNT")
    pipeline_count = _variable(runtime, "PIPE-COUNT")
    time_slice = _variable(runtime, "TIME-SLICE")
    _store_variable(runtime, "SCREEN-ID", 8)
    media_before = runtime.storage.image_bytes

    result = runtime.evaluate(
        b"1 BDL-BEGIN\n"
        b"0 1 1 BDL-BUF tiny-bundle-buffer\n"
        b"1 1 2 1 BDL-KERN tiny-bundle-kernel\n"
        b"1 BDL-PIPE tiny-bundle-pipe\n"
        b"BDL-END\n",
        source_name="tiny-live-pipeline-bundle.f",
    )

    assert tuple(word.name for word in result.definitions) == (
        b"tiny-bundle-buffer",
        b"tiny-bundle-kernel",
        b"tiny-bundle-pipe",
    )
    buffer_descriptor = _constant(runtime, "tiny-bundle-buffer")
    kernel_descriptor = _constant(runtime, "tiny-bundle-kernel")
    pipeline_descriptor = _constant(runtime, "tiny-bundle-pipe")
    assert (
        _execute(runtime, "B.TYPE", buffer_descriptor),
        _execute(runtime, "B.WIDTH", buffer_descriptor),
        _execute(runtime, "B.LEN", buffer_descriptor),
    ) == ((0,), (1,), (1,))
    assert (
        _execute(runtime, "K.IN", kernel_descriptor),
        _execute(runtime, "K.OUT", kernel_descriptor),
        _execute(runtime, "K.FOOT", kernel_descriptor),
        _execute(runtime, "K.FLAGS", kernel_descriptor),
    ) == ((1,), (1,), (2,), (1,))
    assert _execute(runtime, "P.CAP", pipeline_descriptor) == (1,)
    assert _execute(runtime, "P.COUNT", pipeline_descriptor) == (0,)
    assert _variable(runtime, "BUF-COUNT") == buffer_count + 1
    assert _execute(runtime, "BUF-NTH", 0) == (buffer_descriptor,)
    assert _variable(runtime, "KERN-COUNT") == kernel_count + 1
    assert runtime.memory.read64(
        _address(runtime, "KERN-TABLE") + kernel_count * CELL_BYTES
    ) == kernel_descriptor
    assert _variable(runtime, "PIPE-COUNT") == pipeline_count + 1
    assert runtime.memory.read64(
        _address(runtime, "PIPE-TABLE") + pipeline_count * CELL_BYTES
    ) == pipeline_descriptor
    assert (
        _variable(runtime, "BDL-NBUFS"),
        _variable(runtime, "BDL-NKERNS"),
        _variable(runtime, "BDL-NPIPES"),
    ) == (1, 1, 1)
    assert _variable(runtime, "BDL-ACTIVE") == 0
    assert _variable(runtime, "TIME-SLICE") == time_slice
    assert _variable(runtime, "SCREEN-ID") == 1
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == (
        b"\r\n Bundle v1  loaded: 1  bufs 1  kerns 1  pipes\r\n"
    )


def test_bundle_info_uses_its_bound_load_wrapper_and_preserves_throw_state() -> None:
    runtime = _load_ports_dashboard_help()
    mapped_sources = {
        b"normal": (
            b"7 BDL-BEGIN\n"
            b"0 1 2 BDL-BUF info-buffer\n"
            b"1 1 2 0 BDL-KERN info-kernel\n"
            b"1 BDL-PIPE info-pipe\n"
            b"BDL-END\n"
        ),
        b"failing": (
            b"9 BDL-BEGIN\n"
            b"0 1 2 BDL-BUF partial-buffer\n"
            b"-77 THROW\n"
            b"BDL-END\n"
        ),
    }
    load_names: list[bytes] = []

    def mapped_load(context) -> None:
        name = runtime.parse_required_input_word(b"test LOAD")
        load_names.append(name)
        runtime.evaluate(
            mapped_sources[name],
            source_name=f"mapped-bundle:{name.decode('ascii')}",
            context=context,
        )

    load = runtime.define_primitive("LOAD", mapped_load)
    runtime = _evaluate_pipeline_bundles(runtime)
    dry = runtime.find("BDL-DRY")
    store = runtime.find("!")
    bundle_load = runtime.find("BUNDLE-LOAD")
    bundle_info = runtime.find("BUNDLE-INFO")
    assert dry is not None
    assert store is not None
    assert bundle_load is not None
    assert bundle_info is not None
    assert isinstance(bundle_load.implementation, ColonDefinition)
    assert isinstance(bundle_info.implementation, ColonDefinition)
    assert bundle_load.implementation.operations == (
        Literal(0),
        Call(dry.xt),
        Call(store.xt),
        Call(load.xt),
        Return(),
    )
    assert bundle_info.implementation.operations == (
        Literal(1),
        Call(dry.xt),
        Call(store.xt),
        Call(load.xt),
        Literal(0),
        Call(dry.xt),
        Call(store.xt),
        Return(),
    )

    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    registry_before = _registry_state(runtime)
    normal = runtime.evaluate(
        b"BUNDLE-INFO normal",
        source_name="normal-bundle-info.f",
    )

    assert normal.definitions == ()
    assert load_names == [b"normal"]
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert runtime.dictionary.words == words_before
    assert _registry_state(runtime) == registry_before
    for name in ("info-buffer", "info-kernel", "info-pipe"):
        assert runtime.find(name) is None
    assert _variable(runtime, "BDL-DRY") == 0
    assert _variable(runtime, "BDL-ACTIVE") == 0
    assert (
        _variable(runtime, "BDL-NBUFS"),
        _variable(runtime, "BDL-NKERNS"),
        _variable(runtime, "BDL-NPIPES"),
    ) == (1, 1, 1)
    assert runtime.drain_uart_output() == (
        b" --- Bundle v7  ---\r\n"
        b"   Buffers  : 1 \r\n"
        b"   Kernels  : 1 \r\n"
        b"   Pipelines: 1 \r\n"
        b"   Schedule : (none)\r\n"
        b"   Policy   : perms=0  ret=0  exp=3 \r\n"
        b"   Screen   : default=1  mask=255 \r\n"
    )

    caught = runtime.evaluate(
        b"' BUNDLE-INFO CATCH failing",
        source_name="caught-failing-bundle-info.f",
    )

    assert caught.definitions == ()
    assert runtime.main_context.data.snapshot() == (MASK64 - 76,)
    runtime.main_context.data.clear()
    assert runtime.main_context.returns.snapshot() == ()
    assert load_names == [b"normal", b"failing"]
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert runtime.dictionary.words == words_before
    assert _registry_state(runtime) == registry_before
    assert runtime.find("partial-buffer") is None
    assert runtime.memory.read_bytes(here_before, 16) == b"\x0epartial-buffer\x00"
    assert _variable(runtime, "BDL-DRY") == 1
    assert _variable(runtime, "BDL-ACTIVE") == 1
    assert _variable(runtime, "BDL-VER") == 9
    assert (
        _variable(runtime, "BDL-NBUFS"),
        _variable(runtime, "BDL-NKERNS"),
        _variable(runtime, "BDL-NPIPES"),
    ) == (1, 0, 0)
    assert runtime.drain_uart_output() == b""


def test_dot_bundle_reports_reset_and_positive_tracking_state() -> None:
    runtime = _load_pipeline_bundles()
    assert _execute(runtime, "BDL-RESET") == ()
    assert _execute(runtime, ".BUNDLE") == ()
    assert runtime.drain_uart_output() == (
        b" --- Current Bundle ---\r\n"
        b"   (no bundle loaded)\r\n"
    )

    assert _execute(runtime, "BDL-BEGIN", 5) == ()
    _store_variable(runtime, "BDL-NBUFS", 2)
    _store_variable(runtime, "BDL-NKERNS", 3)
    _store_variable(runtime, "BDL-NPIPES", 4)
    assert _execute(runtime, "BDL-SCHED", 2, 99, 3) == ()
    assert _execute(runtime, "BDL-POLICY", 3, 7, 2) == ()
    assert _execute(runtime, "BDL-SCREEN", 4, 85) == ()
    assert _execute(runtime, "BDL-END") == ()
    runtime.drain_uart_output()

    assert _execute(runtime, ".BUNDLE") == ()
    assert runtime.drain_uart_output() == (
        b" --- Current Bundle ---\r\n"
        b"   Version  : 5 \r\n"
        b"   Buffers  : 2 \r\n"
        b"   Kernels  : 3 \r\n"
        b"   Pipelines: 4 \r\n"
        b"   Schedule :  pipe 2  interval=99  [auto] [repeat]\r\n"
        b"   Policy   :  RO SYS ret=7  exp=2 \r\n"
        b"   Screen   : default=4  mask=85 \r\n"
    )


def test_version_zero_is_applied_but_dot_bundle_reports_no_loaded_bundle() -> None:
    runtime = _load_pipeline_bundles()
    _store_variable(runtime, "TIME-SLICE", 777)
    _store_variable(runtime, "SCREEN-ID", 8)

    assert _execute(runtime, "BDL-BEGIN", 0) == ()
    assert _execute(runtime, "BDL-SCHED", 0, 4_321, 0) == ()
    assert _execute(runtime, "BDL-SCREEN", 6, 1) == ()
    assert _execute(runtime, "BDL-END") == ()
    assert runtime.drain_uart_output() == (
        b"\r\n Bundle v0  loaded: 0  bufs 0  kerns 0  pipes\r\n"
    )
    assert _variable(runtime, "BDL-ACTIVE") == 0
    assert _variable(runtime, "BDL-VER") == 0
    assert _variable(runtime, "TIME-SLICE") == 4_321
    assert _variable(runtime, "SCREEN-ID") == 6

    assert _execute(runtime, ".BUNDLE") == ()
    assert runtime.drain_uart_output() == (
        b" --- Current Bundle ---\r\n"
        b"   (no bundle loaded)\r\n"
    )
