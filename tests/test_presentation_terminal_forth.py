"""Focused source-load test for the optional APT-1 guest module.

The presentation client is intentionally not part of KDOS.  These tests load
the production source into userland and exercise its caller-owned attachment,
CELL snapshot, deterministic retained discovery, input, resize, and close
boundaries without installing the module in KDOS.
"""

from __future__ import annotations

import re
from pathlib import Path

from tests.test_system import (
    KDOS_TEST_EXT_MEM_MIB,
    _KDOSTestBase,
    _next_line_chunk,
    capture_uart,
    make_system,
)
from presentation_terminal.server import (
    PresentationTerminalCore,
    TerminalConfig,
    TerminalState,
)


PROJECT_ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = PROJECT_ROOT / "presentation-terminal.f"

# This is a watchdog for a single source module and one short Forth word, not
# a design budget.  The native single-core scheduler normally finishes far
# below it; a regression that spins will still terminate deterministically.
SOURCE_LOAD_MAX_STEPS = 250_000_000
RUN_BATCH_STEPS = 100_000


def _source_lines(path: Path) -> list[str]:
    """Return significant source lines for direct REPL loading."""
    lines: list[str] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("\\"):
            continue
        lines.append(line)
    return lines


def _between(raw: bytes, start: int, end: int) -> bytes:
    start_at = raw.find(bytes((start,)))
    assert start_at >= 0, f"missing output marker {start:#x}"
    end_at = raw.find(bytes((end,)), start_at + 1)
    assert end_at >= 0, f"missing output marker {end:#x}"
    return raw[start_at + 1 : end_at]


class TestPresentationTerminalForth(_KDOSTestBase):
    """Exercise the optional module without installing it into KDOS."""

    def test_inert_load_probe_and_preopen_close(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(
            ram_kib=1024,
            ext_mem_mib=KDOS_TEST_EXT_MEM_MIB,
        )
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines.extend(
            [
                "CREATE PT-TEST-RX 8192 ALLOT",
                "CREATE PT-TEST-TX 8192 ALLOT",
                "CREATE PT-TEST-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-TEST-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-TEST-SESSION",
                "  PT-TEST-SESSION-STORAGE 7 + -8 AND ;",
                ": PT-TEST-FIRST-BOUNDARY",
                "  30 EMIT",
                "  PT-TEST-RX 8192 PT-TEST-TX 8192",
                "  PT-TEST-EVENT PT-EVENT-SIZE PT-TEST-SESSION PT-INIT .",
                "  PT-TEST-SESSION PT-STATE@ . PT-STREAM-OWNED? .",
                "  31 EMIT",
                "  28 EMIT",
                "  PT-TEST-SESSION PT-START .",
                "  PT-TEST-SESSION PT-STATE@ .",
                "  PT-TEST-SESSION PT-OWNS? .",
                "  29 EMIT",
                "  26 EMIT",
                "  0 PT-TEST-SESSION PT-CLOSE .",
                "  PT-TEST-SESSION PT-STATE@ . PT-STREAM-OWNED? .",
                "  25 EMIT ;",
                "PT-TEST-FIRST-BOUNDARY",
                "BYE",
            ]
        )
        payload = ("\n".join(lines) + "\n").encode()
        position = 0
        steps = 0

        while steps < SOURCE_LOAD_MAX_STEPS:
            if system.cpu.halted:
                break
            if system.cpu.idle and not system.uart.has_rx_data:
                if position >= len(payload):
                    break
                chunk = _next_line_chunk(payload, position)
                system.uart.inject_input(chunk)
                position += len(chunk)
                continue
            executed = system.run_batch(
                min(RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS - steps)
            )
            steps += max(executed, 1)

        raw = bytes(uart)
        text = raw.decode("utf-8", errors="replace")
        self.assertEqual(position, len(payload), "test source was not fully fed")
        self.assertTrue(
            system.cpu.halted,
            f"source-load test exceeded its {SOURCE_LOAD_MAX_STEPS:,}-step watchdog",
        )
        self.assertNotIn(" ? (not found)", text)
        for diagnostic in (
            "Dictionary full",
            "dictionary overflow",
            "Stack underflow",
            "Stack overflow",
            "Return stack overflow",
            "nested definition",
            "*** BUS FAULT",
            "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, text)

        init = _between(raw, 30, 31)
        start = _between(raw, 28, 29)
        close = _between(raw, 26, 25)
        probe = re.compile(
            rb"^\x1b\]9999;APT1;P;[0-9A-F]{16};CELL1\x1b\\0 1 -1 $"
        )

        self.assertNotIn(b"\x1b]9999;APT1;P;", raw[: raw.find(bytes((30,)))])
        self.assertEqual(init, b"0 0 0 ")
        self.assertRegex(start, probe)
        self.assertEqual(close, b"0 0 0 ")

    def test_real_core_snapshot_key_resize_and_synchronized_close(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(
            ram_kib=1024,
            ext_mem_mib=KDOS_TEST_EXT_MEM_MIB,
        )
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines.extend(
            [
                "CREATE PT-TEST-RX 8192 ALLOT",
                "CREATE PT-TEST-TX 8192 ALLOT",
                "CREATE PT-TEST-INCOMING-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-TEST-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-TEST-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-TEST-SESSION",
                "  PT-TEST-SESSION-STORAGE 7 + -8 AND ;",
                "VARIABLE PT-TEST-INIT-S",
                "VARIABLE PT-TEST-DISCOVER-S",
                "VARIABLE PT-TEST-START-S",
                "VARIABLE PT-TEST-ACTIVE-S",
                "VARIABLE PT-TEST-TX-S",
                "VARIABLE PT-TEST-RESULT-S",
                "VARIABLE PT-TEST-EVENT-S",
                "VARIABLE PT-TEST-EVENT-TYPE",
                "VARIABLE PT-TEST-EVENT-REV",
                "VARIABLE PT-TEST-EVENT-V0",
                "VARIABLE PT-TEST-EVENT-V1",
                "VARIABLE PT-TEST-EVENT-V2",
                "VARIABLE PT-TEST-EVENT-V3",
                "VARIABLE PT-TEST-RESIZE-S",
                "VARIABLE PT-TEST-RESIZE-TYPE",
                "VARIABLE PT-TEST-RESIZE-V0",
                "VARIABLE PT-TEST-RESIZE-V1",
                "VARIABLE PT-TEST-RESIZE-V2",
                "VARIABLE PT-TEST-CLOSE-S",
                "VARIABLE PT-TEST-CLOSE-WAIT-S",
                "VARIABLE PT-TEST-RET-STATE",
                "VARIABLE PT-TEST-RET-CAPS-U",
                "VARIABLE PT-TEST-PHASE",
                ": PT-TEST-TX-STATUS",
                "  PT-TEST-TX-S @ OR PT-TEST-TX-S ! ;",
                ": PT-TEST-WAIT-ACTIVE",
                "  BEGIN",
                "    PT-TEST-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-TEST-SESSION PT-ACTIVE? IF PT-S-OK EXIT THEN",
                "    PT-TEST-SESSION PT-STATE@ PT-ST-ANSI =",
                "      IF PT-S-UNSUPPORTED EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-TEST-WAIT-RESULT",
                "  BEGIN",
                "    PT-TEST-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-TEST-SESSION PT-SNAPSHOT-NEEDED? 0= IF PT-S-OK EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-TEST-WAIT-EVENT",
                "  BEGIN",
                "    PT-TEST-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-TEST-EVENT PT-TEST-SESSION PT-EVENT-POLL",
                "    IF EXIT THEN",
                "    DUP PT-S-OK <> IF EXIT THEN DROP",
                "    YIELD",
                "  AGAIN ;",
                ": PT-TEST-WAIT-ANSI",
                "  BEGIN",
                "    PT-TEST-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-TEST-SESSION PT-STATE@ PT-ST-ANSI = IF PT-S-OK EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-TEST-BEGIN-MARK",
                "  80 EMIT 84 EMIT 66 EMIT 69 EMIT",
                "  71 EMIT 73 EMIT 78 EMIT 33 EMIT ;",
                ": PT-TEST-RESULT-MARK",
                "  80 EMIT 84 EMIT 82 EMIT 69 EMIT",
                "  83 EMIT 85 EMIT 76 EMIT 84 EMIT 32 EMIT ;",
                ": PT-TEST-REPORT",
                "  PT-TEST-RESULT-MARK",
                "  PT-TEST-INIT-S @ . PT-TEST-DISCOVER-S @ .",
                "  PT-TEST-START-S @ .",
                "  PT-TEST-ACTIVE-S @ . PT-TEST-TX-S @ .",
                "  PT-TEST-RESULT-S @ . PT-TEST-EVENT-S @ .",
                "  PT-TEST-EVENT-TYPE @ . PT-TEST-EVENT-REV @ .",
                "  PT-TEST-EVENT-V0 @ . PT-TEST-EVENT-V1 @ .",
                "  PT-TEST-EVENT-V2 @ . PT-TEST-EVENT-V3 @ .",
                "  PT-TEST-RESIZE-S @ . PT-TEST-RESIZE-TYPE @ .",
                "  PT-TEST-RESIZE-V0 @ . PT-TEST-RESIZE-V1 @ .",
                "  PT-TEST-RESIZE-V2 @ .",
                "  PT-TEST-RET-STATE @ . PT-TEST-RET-CAPS-U @ .",
                "  PT-TEST-CLOSE-S @ . PT-TEST-CLOSE-WAIT-S @ .",
                "  PT-TEST-SESSION PT-STATE@ . PT-STREAM-OWNED? .",
                "  TX-FLUSH ;",
                ": PT-TEST-SEND-INITIAL-SNAPSHOT",
                "  0 PT-TEST-TX-S !",
                "  2 2 2 4 PT-TEST-SESSION PT-SNAPSHOT-BEGIN",
                "    PT-TEST-TX-STATUS",
                "  0 0 2 PT-TEST-SESSION PT-SPAN-BEGIN PT-TEST-TX-STATUS",
                "  65 7 0 1 PT-TEST-SESSION PT-CELL PT-TEST-TX-STATUS",
                "  66 2 0 8 PT-TEST-SESSION PT-CELL PT-TEST-TX-STATUS",
                "  1 0 2 PT-TEST-SESSION PT-SPAN-BEGIN PT-TEST-TX-STATUS",
                "  67 4 0 0 PT-TEST-SESSION PT-CELL PT-TEST-TX-STATUS",
                "  32 7 1 32 PT-TEST-SESSION PT-CELL PT-TEST-TX-STATUS",
                "  1 1 1 PT-TEST-SESSION PT-CURSOR PT-TEST-TX-STATUS",
                "  PT-TEST-SESSION PT-TX-COMMIT PT-TEST-TX-STATUS ;",
                ": PT-TEST-SEND-16-CELLS",
                "  16 0 DO",
                "    DUP I + 7 0 0 PT-TEST-SESSION PT-CELL PT-TEST-TX-STATUS",
                "  LOOP DROP ;",
                ": PT-TEST-SEND-RESIZED-SNAPSHOT",
                "  0 PT-TEST-TX-S !",
                "  16 2 2 32 PT-TEST-SESSION PT-SNAPSHOT-BEGIN",
                "    PT-TEST-TX-STATUS",
                "  0 0 16 PT-TEST-SESSION PT-SPAN-BEGIN PT-TEST-TX-STATUS",
                "  65 PT-TEST-SEND-16-CELLS",
                "  1 0 16 PT-TEST-SESSION PT-SPAN-BEGIN PT-TEST-TX-STATUS",
                "  81 PT-TEST-SEND-16-CELLS",
                "  1 15 1 PT-TEST-SESSION PT-CURSOR PT-TEST-TX-STATUS",
                "  PT-TEST-SESSION PT-TX-COMMIT PT-TEST-TX-STATUS ;",
                ": PT-TEST-RECEIVE-RESULT",
                "  PT-TEST-TX-S @ ?DUP IF PT-TEST-RESULT-S ! EXIT THEN",
                "  PT-TEST-WAIT-RESULT PT-TEST-RESULT-S ! ;",
                ": PT-TEST-RECEIVE-EVENT",
                "  PT-TEST-RESULT-S @ ?DUP IF PT-TEST-EVENT-S ! EXIT THEN",
                "  PT-TEST-WAIT-EVENT DUP PT-TEST-EVENT-S !",
                "  PT-S-OK <> IF EXIT THEN",
                "  PT-TEST-EVENT PT-EVENT-TYPE@ PT-TEST-EVENT-TYPE !",
                "  PT-TEST-EVENT PT-EVENT-REVISION@ PT-TEST-EVENT-REV !",
                "  PT-TEST-EVENT PT-EVENT-VALUE0@ PT-TEST-EVENT-V0 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE1@ PT-TEST-EVENT-V1 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE2@ PT-TEST-EVENT-V2 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE3@ PT-TEST-EVENT-V3 ! ;",
                ": PT-TEST-RECEIVE-RESIZE",
                "  PT-TEST-WAIT-EVENT DUP PT-TEST-RESIZE-S !",
                "  PT-S-OK <> IF EXIT THEN",
                "  PT-TEST-EVENT PT-EVENT-TYPE@ PT-TEST-RESIZE-TYPE !",
                "  PT-TEST-EVENT PT-EVENT-VALUE0@ PT-TEST-RESIZE-V0 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE1@ PT-TEST-RESIZE-V1 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE2@ PT-TEST-RESIZE-V2 ! ;",
                ": PT-TEST-WAIT-RETAINED",
                "  BEGIN",
                "    PT-TEST-SESSION PT-SERVICE PT-S-OK <> IF",
                "      PT-RET-ST-INACTIVE EXIT THEN",
                "    PT-TEST-SESSION PT-RETAINED-STATE@ DUP",
                "      PT-RET-ST-AVAILABLE =",
                "    OVER PT-RET-ST-CELL-ONLY = OR IF EXIT THEN DROP",
                "    YIELD",
                "  AGAIN ;",
                ": PT-TEST-CAPTURE-RETAINED",
                "  PT-TEST-WAIT-RETAINED DUP PT-TEST-RET-STATE !",
                "  PT-RET-ST-AVAILABLE <> IF 0 PT-TEST-RET-CAPS-U ! EXIT THEN",
                "  PT-TEST-SESSION PT-RETAINED-CAPS@",
                "  NIP PT-TEST-RET-CAPS-U ! ;",
                ": PT-TEST-DO-CLOSE",
                "  0 PT-TEST-SESSION PT-CLOSE DUP PT-TEST-CLOSE-S !",
                "  PT-S-OK <> IF EXIT THEN",
                "  PT-TEST-WAIT-ANSI PT-TEST-CLOSE-WAIT-S ! ;",
                ": PT-TEST-HAPPY",
                "  -9 PT-TEST-ACTIVE-S ! -9 PT-TEST-TX-S !",
                "  -9 PT-TEST-RESULT-S ! -9 PT-TEST-EVENT-S !",
                "  -9 PT-TEST-EVENT-TYPE ! -9 PT-TEST-EVENT-REV !",
                "  -9 PT-TEST-EVENT-V0 ! -9 PT-TEST-EVENT-V1 !",
                "  -9 PT-TEST-EVENT-V2 ! -9 PT-TEST-EVENT-V3 !",
                "  -9 PT-TEST-RESIZE-S ! -9 PT-TEST-RESIZE-TYPE !",
                "  -9 PT-TEST-RESIZE-V0 ! -9 PT-TEST-RESIZE-V1 !",
                "  -9 PT-TEST-RESIZE-V2 !",
                "  -9 PT-TEST-RET-STATE ! -9 PT-TEST-RET-CAPS-U !",
                "  -9 PT-TEST-CLOSE-S ! -9 PT-TEST-CLOSE-WAIT-S !",
                "  -9 PT-TEST-DISCOVER-S !",
                "  PT-TEST-RX 8192 PT-TEST-TX 8192",
                "  PT-TEST-INCOMING-EVENT PT-EVENT-SIZE PT-TEST-SESSION",
                "    PT-INIT PT-TEST-INIT-S !",
                "  PT-TEST-SESSION PT-RETAINED-DISCOVER",
                "    PT-TEST-DISCOVER-S !",
                "  PT-TEST-BEGIN-MARK PT-TEST-PHASE . TX-FLUSH",
                "  PT-TEST-SESSION PT-START PT-TEST-START-S !",
                "  PT-TEST-WAIT-ACTIVE PT-TEST-ACTIVE-S !",
                "  2 PT-TEST-PHASE !",
                "  PT-TEST-SEND-INITIAL-SNAPSHOT",
                "  PT-TEST-RECEIVE-RESULT",
                "  3 PT-TEST-PHASE !",
                "  PT-TEST-RECEIVE-EVENT",
                "  4 PT-TEST-PHASE !",
                "  PT-TEST-RECEIVE-RESIZE",
                "  PT-TEST-SEND-RESIZED-SNAPSHOT",
                "  PT-TEST-RECEIVE-RESULT",
                "  PT-TEST-CAPTURE-RETAINED",
                "  5 PT-TEST-PHASE !",
                "  PT-TEST-DO-CLOSE",
                "  6 PT-TEST-PHASE !",
                "  PT-TEST-SESSION PT-STATE@ PT-ST-ANSI = IF PT-TEST-REPORT THEN",
                "  7 PT-TEST-PHASE ! ;",
                "PT-TEST-HAPPY BYE",
            ]
        )
        payload = ("\n".join(lines) + "\n").encode()
        position = 0
        steps = 0
        terminal_cursor: int | None = None
        begin_marker = b"PTBEGIN!"
        terminal_ansi = bytearray()
        terminal_views = []
        key_sent = False
        resize_sent = False
        core = PresentationTerminalCore(
            TerminalConfig(
                max_payload=256,
                max_transaction_bytes=640,
                terminal_receive_credit=1_024,
                max_cells=32,
                max_feed_bytes=4_096,
                max_cols=16,
                max_rows=2,
                cols=2,
                rows=2,
            ),
            attachment_epoch=1,
            session_id_factory=lambda: 0x0123456789ABCDEF,
        )

        def pump_terminal() -> None:
            nonlocal terminal_cursor, key_sent, resize_sent
            current = bytes(uart)
            if terminal_cursor is None:
                marker_at = current.find(begin_marker)
                if marker_at < 0:
                    return
                terminal_cursor = marker_at + len(begin_marker)
            while terminal_cursor < len(current):
                end = min(terminal_cursor + 4_096, len(current))
                result = core.feed_machine(current[terminal_cursor:end])
                terminal_cursor = end
                terminal_ansi.extend(result.ansi_bytes)
                terminal_views.extend(result.views)
                for outbound in result.outbound:
                    system.uart.inject_input(outbound.payload)
                    if outbound.result_transaction_id is not None:
                        core.settle_result_delivery(
                            outbound.result_transaction_id
                        )
                if result.views and not key_sent:
                    key = core.send_key(ord("x"), modifiers=1)
                    assert key is not None
                    system.uart.inject_input(key.payload)
                    key_sent = True
                    resize = core.send_resize(16, 2)
                    assert resize is not None
                    system.uart.inject_input(resize.payload)
                    resize_sent = True

        while steps < SOURCE_LOAD_MAX_STEPS:
            pump_terminal()
            if system.cpu.halted:
                break
            if system.cpu.idle and not system.uart.has_rx_data and position < len(payload):
                chunk = _next_line_chunk(payload, position)
                system.uart.inject_input(chunk)
                position += len(chunk)
            batch_limit = 10_000 if terminal_cursor is not None else RUN_BATCH_STEPS
            executed = system.run_batch(
                min(batch_limit, SOURCE_LOAD_MAX_STEPS - steps)
            )
            steps += executed if executed else batch_limit
        pump_terminal()

        raw = bytes(uart)
        text = raw.decode("utf-8", errors="replace")
        compile_diagnostics = [
            line
            for line in text.splitlines()
            if any(
                diagnostic in line
                for diagnostic in (
                    "compile-only word",
                    "nested definition",
                    "branch out of range",
                    "control-flow",
                )
            )
        ]
        missing_words = [
            line
            for line in text.splitlines()
            if "not found" in line.lower() or "undefined" in line.lower()
        ]
        phase_match = re.search(rb"PTBEGIN!([0-9]+) ", raw)
        phase = (
            system.cpu.mem_read64(int(phase_match.group(1)))
            if phase_match is not None
            else -1
        )
        self.assertEqual(position, len(payload), "test source was not fully fed")
        self.assertTrue(
            system.cpu.halted,
            "full session exceeded its "
            f"{SOURCE_LOAD_MAX_STEPS:,}-step watchdog "
            f"(core={core.state.value}, views={len(terminal_views)}, "
            f"key_sent={key_sent}, source={position}/{len(payload)}, "
            f"cpu_idle={system.cpu.idle}, rx_pending={system.uart.rx_pending}, "
            f"phase={phase}, result={b'PTRESULT ' in raw}, "
            f"missing_words={missing_words[-8:]}, "
            f"compile_diagnostics={compile_diagnostics[-8:]}, "
            f"tail={raw[-160:].hex()})",
        )
        self.assertNotIn(" ? (not found)", text)
        for diagnostic in (
            "Dictionary full",
            "dictionary overflow",
            "Stack underflow",
            "Stack overflow",
            "Return stack overflow",
            "nested definition",
            "*** BUS FAULT",
            "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, text)

        expected = (
            b"PTRESULT 0 0 0 0 0 0 0 512 1 120 1 0 1 "
            b"0 515 16 2 1 3 0 0 0 0 0 "
        )
        self.assertIn(expected, raw)
        self.assertTrue(key_sent)
        self.assertTrue(resize_sent)
        self.assertEqual(core.state, TerminalState.ANSI)
        self.assertEqual(len(terminal_views), 2)
        initial_view, resized_view = terminal_views
        self.assertEqual(initial_view.revision, 1)
        self.assertEqual(
            tuple(cell.codepoint for row in initial_view.cells for cell in row),
            (ord("A"), ord("B"), ord("C"), ord(" ")),
        )
        self.assertEqual((resized_view.cols, resized_view.rows), (16, 2))
        self.assertEqual(resized_view.revision, 1)
        self.assertEqual(
            tuple(cell.codepoint for row in resized_view.cells for cell in row),
            tuple(range(ord("A"), ord("A") + 32)),
        )
        self.assertIn(expected, terminal_ansi)
