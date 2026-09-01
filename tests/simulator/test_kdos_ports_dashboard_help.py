"""Unchanged-source acceptance for KDOS ports, dashboard, and help."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64
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
from tests.simulator.test_kdos_screen_tail import _load_screen_tail
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-ports-dashboard-help-8569-8944.f"
)

FIRST_LINE = 8569
FIXTURE_LAST_LINE = 8944
LAST_LINE = 8943
FIXTURE_BYTES = 15_774
FIXTURE_SHA256 = (
    "90af3e5c11bd7501b0a69f58163ce8be01f68ee543365cf2d388e97707ac9ce5"
)
FIXTURE_GIT_BLOB = "01ff09721f5601602c66c1ab42af76fc7dad0b87"
SLICE_BYTES = 15_702
SLICE_SHA256 = (
    "0fff19ac85b6b0ff1261e587a1a0d7462035ac2f453229f58236af37e465a713"
)
SLICE_GIT_BLOB = "7f5cd3054b3936f5e0561cbd53395da0af50d309"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 4_264
FRAME_BUFFER_BODY_BYTES = 1_507
PORT_TABLE_BODY_BYTES = 2_048
HELP_STRING_BODY_BYTES = 23

SOURCE_LEDGER = (
    ("CONSTANT", b"/FRAME-HDR", 0),
    ("VARIABLE", b"FRAME-BUF", FRAME_BUFFER_BODY_BYTES),
    ("VARIABLE", b"PORT-TABLE", PORT_TABLE_BODY_BYTES),
    ("VARIABLE", b"ROUTE-BUF", CELL_BYTES),
    (":", b"PORT-SLOT", 0),
    (":", b"PORT!", 0),
    (":", b"PORT@", 0),
    (":", b"UNPORT", 0),
    (":", b"FRAME-SRC", 0),
    (":", b"FRAME-TYPE", 0),
    (":", b"FRAME-SEQ", 0),
    (":", b"FRAME-LEN", 0),
    (":", b"FRAME-DATA", 0),
    (":", b".FRAME", 0),
    (":", b"PORTS", 0),
    (":", b"PORT-STATS", 0),
    (":", b"HRULE", 0),
    (":", b"THIN-RULE", 0),
    (":", b".MEM", 0),
    (":", b"MEM-REPORT", 0),
    (":", b"DASHBOARD", 0),
    (":", b"STATUS", 0),
    ("VARIABLE", b"HW-FOUND", CELL_BYTES),
    ("VARIABLE", b"HW-CSTR", HELP_STRING_BODY_BYTES),
    (":", b"HELP-WORD", 0),
    (":", b".HELP-ALL", 0),
    (":", b"HELP", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)


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
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_ports_dashboard_help(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_ports_dashboard_help() -> MegaForthRuntime:
    return _evaluate_ports_dashboard_help(_load_screen_tail())


def test_ports_dashboard_help_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_screen_tail()
    _store_variable(runtime, "PORT-COUNT", 7)
    _store_variable(runtime, "PORT-RX", 8)
    _store_variable(runtime, "PORT-DROP", 9)
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_ports_dashboard_help(runtime)

    assert len(SOURCE_LEDGER) == 27
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 211
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == 3_594
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

    assert _constant(runtime, "/FRAME-HDR") == 6
    frame_buffer = _address(runtime, "FRAME-BUF")
    assert runtime.memory.read_bytes(frame_buffer, CELL_BYTES) == bytes(CELL_BYTES)
    assert runtime.memory.read_bytes(
        frame_buffer + CELL_BYTES,
        FRAME_BUFFER_BODY_BYTES - CELL_BYTES,
    ) == b"\xA5" * (FRAME_BUFFER_BODY_BYTES - CELL_BYTES)
    assert runtime.memory.read_bytes(
        _address(runtime, "PORT-TABLE"),
        PORT_TABLE_BODY_BYTES,
    ) == bytes(PORT_TABLE_BODY_BYTES)
    assert _variable(runtime, "ROUTE-BUF") == 0
    assert _variable(runtime, "HW-FOUND") == 0
    help_string = _address(runtime, "HW-CSTR")
    assert runtime.memory.read_bytes(help_string, CELL_BYTES) == bytes(CELL_BYTES)
    assert runtime.memory.read_bytes(
        help_string + CELL_BYTES,
        HELP_STRING_BODY_BYTES - CELL_BYTES,
    ) == b"\xA5" * (HELP_STRING_BODY_BYTES - CELL_BYTES)
    help_word = runtime.find("HELP-WORD")
    assert help_word is not None
    assert help_string + HELP_STRING_BODY_BYTES == help_word.header_address
    assert help_string + 1 + 23 == help_word.header_address + 1

    assert _variable(runtime, "PORT-COUNT") == 7
    assert _variable(runtime, "PORT-RX") == 8
    assert _variable(runtime, "PORT-DROP") == 9
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


def test_data_ports_bind_rebind_unbind_and_expose_zero_binding_drift() -> None:
    runtime = _load_ports_dashboard_help()
    first = runtime.define_created("PORT-BUFFER-A", initial_body=bytes(8))
    second = runtime.define_created("PORT-BUFFER-B", initial_body=bytes(8))

    assert _execute(runtime, "PORT@", 0) == (0,)
    assert _execute(runtime, "PORT!", first.body_address, 0) == ()
    assert _execute(runtime, "PORT@", 0) == (first.body_address,)
    assert _variable(runtime, "PORT-COUNT") == 1

    assert _execute(runtime, "PORT!", second.body_address, 0) == ()
    assert _execute(runtime, "PORT@", 0) == (second.body_address,)
    assert _variable(runtime, "PORT-COUNT") == 1
    assert _execute(runtime, "PORT!", first.body_address, 255) == ()
    assert _variable(runtime, "PORT-COUNT") == 2

    assert _execute(runtime, "UNPORT", 255) == ()
    assert _execute(runtime, "PORT@", 255) == (0,)
    assert _variable(runtime, "PORT-COUNT") == 1
    assert _execute(runtime, "UNPORT", 255) == ()
    assert _variable(runtime, "PORT-COUNT") == 1

    assert _execute(runtime, "PORT!", 0, 0) == ()
    assert _execute(runtime, "PORT@", 0) == (0,)
    assert _variable(runtime, "PORT-COUNT") == 1
    assert _execute(runtime, "UNPORT", 0) == ()
    assert _variable(runtime, "PORT-COUNT") == 1

    assert _execute(runtime, "PORT!", 0, 7) == ()
    assert _execute(runtime, "PORT@", 7) == (0,)
    assert _variable(runtime, "PORT-COUNT") == 2
    assert _execute(runtime, "PORT!", 0, 7) == ()
    assert _variable(runtime, "PORT-COUNT") == 3
    assert _execute(runtime, "UNPORT", 7) == ()
    assert _variable(runtime, "PORT-COUNT") == 3


def test_data_port_slot_arithmetic_exposes_unchecked_dictionary_boundaries() -> None:
    runtime = _load_ports_dashboard_help()
    table = _address(runtime, "PORT-TABLE")
    route = runtime.find("ROUTE-BUF")
    assert route is not None

    assert _execute(runtime, "PORT-SLOT", 0) == (table,)
    assert _execute(runtime, "PORT-SLOT", 255) == (table + 255 * CELL_BYTES,)
    assert _execute(runtime, "PORT-SLOT", 256) == (route.header_address,)
    assert _execute(runtime, "PORT-SLOT", (1 << 64) - 1) == (table - CELL_BYTES,)

    assert _execute(runtime, "NET-RX?") == (0,)
    for absent in (
        "POLL",
        "INGEST",
        "RECV-FRAME",
        "ROUTE-FRAME",
        "PORT-SEND",
        "PORT-SEND-SLICE",
    ):
        assert runtime.find(absent) is None


def test_data_port_frame_header_accessors_and_publishers_are_byte_exact() -> None:
    runtime = _load_ports_dashboard_help()
    frame = _address(runtime, "FRAME-BUF")
    runtime.memory.write_bytes(frame, bytes((7, 3, 0xE8, 0x03, 0xD2, 0x04)))

    assert _execute(runtime, "FRAME-SRC") == (7,)
    assert _execute(runtime, "FRAME-TYPE") == (3,)
    assert _execute(runtime, "FRAME-SEQ") == (1_000,)
    assert _execute(runtime, "FRAME-LEN") == (1_234,)
    assert _execute(runtime, "FRAME-DATA") == (frame + 6,)
    assert _execute(runtime, ".FRAME") == ()
    assert runtime.drain_uart_output() == (
        b" src=7  type=3  seq=1000  len=1234 \r\n"
    )

    _store_variable(runtime, "PORT-RX", 12)
    _store_variable(runtime, "PORT-DROP", 4)
    assert _execute(runtime, "PORT-STATS") == ()
    assert runtime.drain_uart_output() == b" ports=0  rx=12  drop=4 "


def test_data_port_listing_scans_the_complete_declared_id_domain() -> None:
    runtime = _load_ports_dashboard_help()
    descriptor = runtime.define_created("LISTED-PORT-BUFFER", initial_body=bytes(8))
    assert _execute(runtime, "PORT!", descriptor.body_address, 255) == ()
    _store_variable(runtime, "PORT-RX", 2)
    _store_variable(runtime, "PORT-DROP", 1)

    assert _execute(runtime, "PORTS") == ()
    assert runtime.drain_uart_output() == (
        b" --- Ports (1  ) ---\r\n"
        + b"   src=255   -> buf @"
        + str(descriptor.body_address).encode("ascii")
        + b" \r\n"
        + b"   rx=2  drop=1 \r\n"
    )


def test_dashboard_rules_and_status_are_focused_byte_publishers() -> None:
    runtime = _load_ports_dashboard_help()
    assert _execute(runtime, "HRULE") == ()
    assert runtime.drain_uart_output() == b"-" * 60 + b"\r\n"
    assert _execute(runtime, "THIN-RULE") == ()
    assert runtime.drain_uart_output() == b"." * 40 + b"\r\n"

    here = runtime.dictionary.here
    counts = {
        name: _variable(runtime, name)
        for name in (
            "BUF-COUNT",
            "KERN-COUNT",
            "PIPE-COUNT",
            "TASK-COUNT",
            "FILE-COUNT",
            "PORT-COUNT",
        )
    }
    assert _execute(runtime, "STATUS") == ()
    assert runtime.drain_uart_output() == (
        b" KDOS v1.1 | cores=1 "
        + b" bufs=" + str(counts["BUF-COUNT"]).encode() + b" "
        + b" kerns=" + str(counts["KERN-COUNT"]).encode() + b" "
        + b" pipes=" + str(counts["PIPE-COUNT"]).encode() + b" "
        + b" tasks=" + str(counts["TASK-COUNT"]).encode() + b" "
        + b" files=" + str(counts["FILE-COUNT"]).encode() + b" "
        + b" ports=" + str(counts["PORT-COUNT"]).encode() + b" "
        + b" disk= no  HERE=" + str(here).encode() + b" \r\n"
    )


def test_help_specific_lookup_exposes_broken_related_word_counter() -> None:
    runtime = _load_ports_dashboard_help()
    runtime.evaluate(b"HELP STATUS", source_name="help-existing-word")
    output = runtime.drain_uart_output()

    assert _variable(runtime, "HW-FOUND") == MASK64
    assert b"\x1b[32m Found: \x1b[0mSTATUS" in output
    assert b"Related words:" in output
    assert b"(none)" in output
    assert b"(0  related)" in output
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    runtime.evaluate(b"HELP NO-SUCH-WORD", source_name="help-missing-word")
    output = runtime.drain_uart_output()
    assert _variable(runtime, "HW-FOUND") == 0
    assert b"\x1b[31m Not found: \x1b[0mNO-SUCH-WORD" in output
    assert b"(none)" in output
    assert b"(0  related)" in output


def test_full_help_reference_is_a_stable_ordinary_source_publisher() -> None:
    runtime = _load_ports_dashboard_help()
    runtime.evaluate(b"HELP", source_name="full-help-reference")
    output = runtime.drain_uart_output()

    assert len(output) == 7_431
    assert hashlib.sha256(output).hexdigest() == (
        "c1d44c8970fa800f943db3e9b081cdaaf642af429c6cf4f9df27bcc63a2f1d07"
    )
    for advertised_absent in (
        b"POLL",
        b"INGEST",
        b"BDL-BEGIN",
        b"BUNDLE-LOAD",
    ):
        assert advertised_absent in output
        assert runtime.find(advertised_absent) is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
