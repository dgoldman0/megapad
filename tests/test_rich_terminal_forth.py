"""Focused source-load test for the optional APT-1 guest module.

The rich-terminal client is intentionally not part of KDOS.  These tests load
the production source into userland and exercise its caller-owned attachment,
CELL snapshot, deterministic retained discovery, input, resize, and close
boundaries without installing the module in KDOS.
"""

from __future__ import annotations

import re
import struct
from pathlib import Path

from tests.test_system import (
    KDOS_TEST_EXT_MEM_MIB,
    _KDOSTestBase,
    _next_line_chunk,
    capture_uart,
    make_system,
)
from rich_terminal.server import (
    RichTerminalCore,
    TerminalConfig,
    TerminalState,
)
from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_model import RetainedFeature, RetainedPolicy
from rich_terminal.retained_wire import RetainedMessageType


PROJECT_ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = PROJECT_ROOT / "rich-terminal.f"

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


def _core_retained_policy() -> RetainedPolicy:
    """Small CORE-only policy that admits the guest owner/region slice."""
    return RetainedPolicy(
        features=RetainedFeature.CORE,
        max_owner_records=4,
        max_live_owners=2,
        max_regions=8,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=4,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=512,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=512,
    )


class TestRichTerminalForth(_KDOSTestBase):
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

    def test_collection_control_writer_emits_exact_target_forth_bytes(self) -> None:
        """Pack one minimum STX1 control through the production guest word."""
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
                "CREATE PT-COL-RX 8192 ALLOT",
                "CREATE PT-COL-TX 8192 ALLOT",
                "CREATE PT-COL-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-COL-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-COL-SESSION PT-COL-SESSION-STORAGE 7 + -8 AND ;",
                "CREATE PT-COL-CONTENT 72 ALLOT",
                "VARIABLE PT-COL-INIT-S",
                "VARIABLE PT-COL-FEATURE-S",
                "VARIABLE PT-COL-SHORT-S",
                "VARIABLE PT-COL-WRITE-S",
                "VARIABLE PT-COL-ALIAS-S",
                ": PT-COL-CONTENT!",
                "  PT-COL-CONTENT 72 0 FILL",
                "  0x31585453 PT-COL-CONTENT L!",
                "  1 PT-COL-CONTENT 4 + W!",
                "  1 PT-COL-CONTENT 8 + _PT-U64!",
                "  1 PT-COL-CONTENT 16 + L!",
                "  1 PT-COL-CONTENT 20 + L!",
                "  1 PT-COL-CONTENT 32 + L!",
                "  1 PT-COL-CONTENT 36 + L! ;",
                ": PT-COL-PRIME",
                "  PT-COL-CONTENT!",
                "  PT-COL-RX 8192 PT-COL-TX 8192",
                "    PT-COL-EVENT PT-EVENT-SIZE PT-COL-SESSION",
                "    PT-INIT PT-COL-INIT-S !",
                "  PT-ST-ACTIVE PT-COL-SESSION _PT.S.STATE !",
                "  152 PT-COL-SESSION _PT.S.PEER-MAX-PAY !",
                "  0x4142434445464748 PT-COL-SESSION _PT.S.SESSION-ID !",
                "  9 PT-COL-SESSION _PT.S.EPOCH !",
                "  -1 PT-COL-SESSION _PT.S.RET-ENABLED? !",
                "  _PT-RD-AVAILABLE PT-COL-SESSION _PT.S.RET-STATE !",
                "  0x301 PT-COL-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  -1 PT-COL-SESSION _PT.S.TX-OPEN? !",
                "  _PT-TX-PRESENT PT-COL-SESSION _PT.S.TX-KIND !",
                "  PT-CELL-NONE PT-COL-SESSION _PT.S.TX-CELL-MODE !",
                "  PT-RET-DELTA PT-COL-SESSION _PT.S.TX-RET-MODE !",
                "  1 PT-COL-SESSION _PT.S.TX-RET-OPS !",
                "  192 PT-COL-SESSION _PT.S.TX-RET-BYTES ! ;",
                ": PT-COL-ARGS",
                "  0x0102030405060708 0x1112131415161718",
                "  0x2122232425262728 PT-CONTROL-TEXT-AREA",
                "  PT-CONTROL-VISIBLE PT-CONTROL-ENABLED OR",
                "    PT-CONTROL-SELECTED OR",
                "  -7 0x3132333435363738 0 0",
                "  0x01020304 0x11121314 0xA1A2A3A4 0xB1B2B3B4",
                "  0 0 0 0 ;",
                ": PT-COL-WRITE  ( content-a content-u -- status )",
                "  >R >R PT-COL-ARGS R> R> PT-COL-SESSION",
                "  PT-CONTROL-DEFINE ;",
                ": PT-COL-BEGIN-MARK",
                "  80 EMIT 84 EMIT 67 EMIT 79 EMIT 76 EMIT",
                "  66 EMIT 69 EMIT 71 EMIT 73 EMIT 78 EMIT 33 EMIT ;",
                ": PT-COL-END-MARK",
                "  80 EMIT 84 EMIT 67 EMIT 79 EMIT 76 EMIT",
                "  69 EMIT 78 EMIT 68 EMIT 33 EMIT ;",
                ": PT-COL-RUN",
                "  PT-COL-PRIME",
                "  0x101 PT-COL-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  PT-COL-CONTENT 72 PT-COL-WRITE PT-COL-FEATURE-S !",
                "  0x301 PT-COL-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  PT-COL-CONTENT 71 PT-COL-WRITE PT-COL-SHORT-S !",
                "  PT-COL-BEGIN-MARK TX-FLUSH",
                "  PT-COL-CONTENT 72 PT-COL-WRITE PT-COL-WRITE-S !",
                "  TX-FLUSH",
                "  PT-COL-END-MARK TX-FLUSH",
                "  PT-COL-SESSION _PT.S.TX-A @ 72",
                "    PT-COL-WRITE PT-COL-ALIAS-S !",
                '  S" PTCOLSTATUS " TYPE',
                "  PT-COL-INIT-S @ . PT-COL-FEATURE-S @ .",
                "  PT-COL-SHORT-S @ . PT-COL-WRITE-S @ .",
                "  PT-COL-ALIAS-S @ . DEPTH . TX-FLUSH ;",
                "PT-COL-RUN BYE",
            ]
        )
        program = ("\n".join(lines) + "\n").encode()
        position = 0
        steps = 0

        while steps < SOURCE_LOAD_MAX_STEPS:
            if system.cpu.halted:
                break
            if system.cpu.idle and not system.uart.has_rx_data:
                if position >= len(program):
                    break
                chunk = _next_line_chunk(program, position)
                system.uart.inject_input(chunk)
                position += len(chunk)
                continue
            executed = system.run_batch(
                min(RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS - steps)
            )
            steps += max(executed, 1)

        raw = bytes(uart)
        text = raw.decode("utf-8", errors="replace")
        self.assertEqual(position, len(program), "test source was not fully fed")
        self.assertTrue(
            system.cpu.halted,
            "collection writer byte oracle exceeded its "
            f"{SOURCE_LOAD_MAX_STEPS:,}-step watchdog",
        )
        self.assertNotIn(" ? (not found)", text)
        for diagnostic in (
            "Dictionary full",
            "dictionary overflow",
            "Stack underflow",
            "Stack overflow",
            "Return stack overflow",
            "nested definition",
            "branch out of range",
            "control-flow",
            "*** BUS FAULT",
            "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, text)

        content = struct.pack(
            "<IHHQIIIIIIIIQQII",
            0x31585453,
            1,
            0,
            1,
            1,
            1,
            0,
            0,
            1,
            1,
            0,
            0,
            0,
            0,
            0,
            0,
        )
        self.assertEqual(len(content), 72)
        control = struct.pack(
            "<QQQHHiQQIIIIIIII",
            0x0102030405060708,
            0x1112131415161718,
            0x2122232425262728,
            5,
            0x0B,
            -7,
            0x3132333435363738,
            0,
            0,
            0x01020304,
            0x11121314,
            0xA1A2A3A4,
            0xB1B2B3B4,
            0,
            0,
            len(content),
        ) + content
        expected = encode_frame(
            Frame(
                RetainedMessageType.CONTROL_DEFINE,
                0x4142434445464748,
                0,
                9,
                control,
            ),
            max_payload=152,
        )
        begin = raw.index(b"PTCOLBEGIN!") + len(b"PTCOLBEGIN!")
        end = raw.index(b"PTCOLEND!", begin)
        status = re.search(
            rb"PTCOLSTATUS ((?:-?[0-9]+ ){6})",
            raw[end:],
        )
        self.assertIsNotNone(status)
        self.assertEqual(status.group(1), b"0 4 3 0 3 0 ")
        self.assertEqual(raw[begin:end], expected)

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
                "VARIABLE PT-TEST-EVENT-VALUE0",
                "VARIABLE PT-TEST-EVENT-VALUE1",
                "VARIABLE PT-TEST-EVENT-VALUE2",
                "VARIABLE PT-TEST-EVENT-VALUE3",
                "VARIABLE PT-TEST-RESIZE-S",
                "VARIABLE PT-TEST-RESIZE-TYPE",
                "VARIABLE PT-TEST-RESIZE-VALUE0",
                "VARIABLE PT-TEST-RESIZE-VALUE1",
                "VARIABLE PT-TEST-RESIZE-VALUE2",
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
                "  PT-TEST-EVENT-VALUE0 @ . PT-TEST-EVENT-VALUE1 @ .",
                "  PT-TEST-EVENT-VALUE2 @ . PT-TEST-EVENT-VALUE3 @ .",
                "  PT-TEST-RESIZE-S @ . PT-TEST-RESIZE-TYPE @ .",
                "  PT-TEST-RESIZE-VALUE0 @ . PT-TEST-RESIZE-VALUE1 @ .",
                "  PT-TEST-RESIZE-VALUE2 @ .",
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
                "  PT-TEST-EVENT PT-EVENT-VALUE0@ PT-TEST-EVENT-VALUE0 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE1@ PT-TEST-EVENT-VALUE1 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE2@ PT-TEST-EVENT-VALUE2 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE3@ PT-TEST-EVENT-VALUE3 ! ;",
                ": PT-TEST-RECEIVE-RESIZE",
                "  PT-TEST-WAIT-EVENT DUP PT-TEST-RESIZE-S !",
                "  PT-S-OK <> IF EXIT THEN",
                "  PT-TEST-EVENT PT-EVENT-TYPE@ PT-TEST-RESIZE-TYPE !",
                "  PT-TEST-EVENT PT-EVENT-VALUE0@ PT-TEST-RESIZE-VALUE0 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE1@ PT-TEST-RESIZE-VALUE1 !",
                "  PT-TEST-EVENT PT-EVENT-VALUE2@ PT-TEST-RESIZE-VALUE2 ! ;",
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
                "  -9 PT-TEST-EVENT-VALUE0 ! -9 PT-TEST-EVENT-VALUE1 !",
                "  -9 PT-TEST-EVENT-VALUE2 ! -9 PT-TEST-EVENT-VALUE3 !",
                "  -9 PT-TEST-RESIZE-S ! -9 PT-TEST-RESIZE-TYPE !",
                "  -9 PT-TEST-RESIZE-VALUE0 ! -9 PT-TEST-RESIZE-VALUE1 !",
                "  -9 PT-TEST-RESIZE-VALUE2 !",
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
        core = RichTerminalCore(
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

    def test_real_core_owner_region_present_and_legacy_cell_interleave(self) -> None:
        """Drive the production guest writers through one CORE-only terminal."""
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
                "CREATE PT-RICH-RX 8192 ALLOT",
                "CREATE PT-RICH-TX 8192 ALLOT",
                "CREATE PT-RICH-INCOMING PT-EVENT-SIZE ALLOT",
                "CREATE PT-RICH-COMPLETION PT-COMPLETION-SIZE ALLOT",
                "CREATE PT-RICH-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-RICH-SESSION PT-RICH-SESSION-STORAGE 7 + -8 AND ;",
                "VARIABLE PT-RICH-ERROR",
                "VARIABLE PT-RICH-PHASE",
                "VARIABLE PT-RICH-WANT-KIND",
                "VARIABLE PT-RICH-WANT-REQUEST",
                "VARIABLE PT-RICH-WANT-REVISION",
                ": PT-RICH-STATUS+ PT-RICH-ERROR @ OR PT-RICH-ERROR ! ;",
                ": PT-RICH-WAIT-ACTIVE",
                "  BEGIN",
                "    PT-RICH-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-RICH-SESSION PT-ACTIVE? IF PT-S-OK EXIT THEN YIELD",
                "  AGAIN ;",
                ": PT-RICH-WAIT-SNAPSHOT",
                "  BEGIN",
                "    PT-RICH-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-RICH-SESSION PT-SNAPSHOT-NEEDED? 0= IF PT-S-OK EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-RICH-WAIT-RETAINED",
                "  BEGIN",
                "    PT-RICH-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-RICH-SESSION PT-RETAINED-STATE@ DUP",
                "    PT-RET-ST-AVAILABLE = IF DROP PT-S-OK EXIT THEN",
                "    PT-RET-ST-CELL-ONLY = IF PT-S-UNSUPPORTED EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-RICH-WAIT-COMPLETION",
                "  BEGIN",
                "    PT-RICH-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-RICH-COMPLETION PT-RICH-SESSION PT-COMPLETION-POLL",
                "    IF EXIT THEN",
                "    DUP PT-S-OK <> IF EXIT THEN DROP YIELD",
                "  AGAIN ;",
                ": PT-RICH-CHECK-COMPLETION",
                "  PT-RICH-WANT-REVISION ! PT-RICH-WANT-REQUEST !",
                "  PT-RICH-WANT-KIND !",
                "  PT-RICH-COMPLETION PT-COMPLETION-KIND@",
                "    PT-RICH-WANT-KIND @ <> PT-RICH-STATUS+",
                "  PT-RICH-COMPLETION PT-COMPLETION-REQUEST@",
                "    PT-RICH-WANT-REQUEST @ <> PT-RICH-STATUS+",
                "  PT-RICH-COMPLETION PT-COMPLETION-STATUS@ 0<>",
                "    PT-RICH-STATUS+",
                "  PT-RICH-COMPLETION PT-COMPLETION-REVISION@",
                "    PT-RICH-WANT-REVISION @ <> PT-RICH-STATUS+ ;",
                ": PT-RICH-SNAPSHOT",
                "  2 2 2 4 PT-RICH-SESSION PT-SNAPSHOT-BEGIN PT-RICH-STATUS+",
                "  0 0 2 PT-RICH-SESSION PT-SPAN-BEGIN PT-RICH-STATUS+",
                "  65 7 0 0 PT-RICH-SESSION PT-CELL PT-RICH-STATUS+",
                "  66 7 0 0 PT-RICH-SESSION PT-CELL PT-RICH-STATUS+",
                "  1 0 2 PT-RICH-SESSION PT-SPAN-BEGIN PT-RICH-STATUS+",
                "  67 7 0 0 PT-RICH-SESSION PT-CELL PT-RICH-STATUS+",
                "  68 7 0 0 PT-RICH-SESSION PT-CELL PT-RICH-STATUS+",
                "  0 0 0 PT-RICH-SESSION PT-CURSOR PT-RICH-STATUS+",
                "  PT-RICH-SESSION PT-TX-COMMIT PT-RICH-STATUS+ ;",
                ": PT-RICH-REPLACE-START",
                "  2 2 0 0 1 88 PT-CELL-NONE PT-RET-REPLACE-START",
                "    PT-RICH-SESSION PT-PRESENT-BEGIN PT-RICH-STATUS+",
                "  1 1 1 0 0 2 2 0 1 PT-RICH-SESSION",
                "    PT-REGION-DEFINE PT-RICH-STATUS+",
                "  PT-COMMIT PT-RICH-SESSION PT-PRESENT-COMMIT",
                "    PT-RICH-STATUS+ ;",
                ": PT-RICH-REPLACE-REVEAL",
                "  2 2 0 0 0 0 PT-CELL-NONE PT-RET-REPLACE-CONTINUE",
                "    PT-RICH-SESSION PT-PRESENT-BEGIN PT-RICH-STATUS+",
                "  PT-COMMIT-AND-REVEAL PT-RICH-SESSION PT-PRESENT-COMMIT",
                "    PT-RICH-STATUS+ ;",
                ": PT-RICH-CELL-DELTA",
                "  2 2 1 1 PT-RICH-SESSION PT-TX-BEGIN PT-RICH-STATUS+",
                "  0 0 1 PT-RICH-SESSION PT-SPAN-BEGIN PT-RICH-STATUS+",
                "  90 2 0 1 PT-RICH-SESSION PT-CELL PT-RICH-STATUS+",
                "  0 0 1 PT-RICH-SESSION PT-CURSOR PT-RICH-STATUS+",
                "  PT-RICH-SESSION PT-TX-COMMIT PT-RICH-STATUS+ ;",
                ": PT-RICH-SEND-DROP",
                "  BEGIN",
                "    1 1 PT-RICH-SESSION PT-OWNER-DROP DUP",
                "    PT-S-WOULD-BLOCK = IF",
                "      DROP PT-RICH-SESSION PT-SERVICE",
                "      DUP PT-S-OK <> IF EXIT THEN DROP YIELD",
                "    ELSE EXIT THEN",
                "  AGAIN ;",
                ": PT-RICH-WAIT-ANSI",
                "  BEGIN",
                "    PT-RICH-SESSION PT-SERVICE DUP PT-S-OK <> IF EXIT THEN DROP",
                "    PT-RICH-SESSION PT-STATE@ PT-ST-ANSI = IF PT-S-OK EXIT THEN",
                "    YIELD",
                "  AGAIN ;",
                ": PT-RICH-BEGIN-MARK",
                "  80 EMIT 84 EMIT 82 EMIT 73 EMIT 67 EMIT 72 EMIT 33 EMIT",
                "  PT-RICH-PHASE . TX-FLUSH ;",
                ": PT-RICH-REPORT",
                "  80 EMIT 84 EMIT 82 EMIT 69 EMIT 80 EMIT 79 EMIT 82 EMIT 84 EMIT",
                "  32 EMIT PT-RICH-ERROR @ . PT-RICH-SESSION PT-STATE@ .",
                "  PT-STREAM-OWNED? . TX-FLUSH ;",
                ": PT-RICH-RUN",
                "  0 PT-RICH-ERROR ! 0 PT-RICH-PHASE !",
                "  PT-RICH-RX 8192 PT-RICH-TX 8192 PT-RICH-INCOMING",
                "    PT-EVENT-SIZE PT-RICH-SESSION PT-INIT PT-RICH-STATUS+",
                "  PT-RICH-SESSION PT-RETAINED-DISCOVER PT-RICH-STATUS+",
                "  PT-RICH-BEGIN-MARK",
                "  PT-RICH-SESSION PT-START PT-RICH-STATUS+",
                "  PT-RICH-WAIT-ACTIVE PT-RICH-STATUS+ 1 PT-RICH-PHASE !",
                "  PT-RICH-SNAPSHOT PT-RICH-WAIT-SNAPSHOT PT-RICH-STATUS+",
                "  PT-RICH-WAIT-RETAINED PT-RICH-STATUS+ 2 PT-RICH-PHASE !",
                "  1 1 1 0 0 0 0 0 0 PT-RICH-SESSION",
                "    PT-OWNER-OPEN PT-RICH-STATUS+",
                "  PT-RICH-WAIT-COMPLETION PT-RICH-STATUS+",
                "  PT-COMPLETE-RET PT-REQUEST-OWNER-OPEN 1",
                "    PT-RICH-CHECK-COMPLETION",
                "  PT-RICH-REPLACE-START PT-RICH-WAIT-COMPLETION",
                "    PT-RICH-STATUS+",
                "  PT-COMPLETE-TX PT-REQUEST-PRESENT-COMMIT 2",
                "    PT-RICH-CHECK-COMPLETION",
                "  PT-RICH-REPLACE-REVEAL PT-RICH-WAIT-COMPLETION",
                "    PT-RICH-STATUS+",
                "  PT-COMPLETE-TX PT-REQUEST-PRESENT-COMMIT 3",
                "    PT-RICH-CHECK-COMPLETION 3 PT-RICH-PHASE !",
                "  PT-RICH-CELL-DELTA PT-RICH-WAIT-COMPLETION",
                "    PT-RICH-STATUS+",
                "  PT-COMPLETE-TX PT-REQUEST-TX-COMMIT 4",
                "    PT-RICH-CHECK-COMPLETION",
                "  PT-RICH-SEND-DROP PT-RICH-STATUS+",
                "  PT-RICH-WAIT-COMPLETION PT-RICH-STATUS+",
                "  PT-COMPLETE-TX PT-REQUEST-OWNER-DROP 5",
                "    PT-RICH-CHECK-COMPLETION 4 PT-RICH-PHASE !",
                "  0 PT-RICH-SESSION PT-CLOSE PT-RICH-STATUS+",
                "  PT-RICH-WAIT-ANSI PT-RICH-STATUS+",
                "  PT-RICH-REPORT 5 PT-RICH-PHASE ! ;",
                "PT-RICH-RUN BYE",
            ]
        )
        payload = ("\n".join(lines) + "\n").encode()
        position = 0
        steps = 0
        terminal_cursor: int | None = None
        begin_marker = b"PTRICH!"
        terminal_views = []
        core = RichTerminalCore(
            TerminalConfig(
                max_payload=256,
                max_transaction_bytes=512,
                terminal_receive_credit=4_096,
                max_cells=4,
                max_feed_bytes=4_096,
                max_cols=2,
                max_rows=2,
                cols=2,
                rows=2,
            ),
            attachment_epoch=1,
            retained_policy=_core_retained_policy(),
            session_id_factory=lambda: 0x0123456789ABCDEF,
        )

        def pump_terminal() -> None:
            nonlocal terminal_cursor
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
                terminal_views.extend(result.views)
                for outbound in result.outbound:
                    system.uart.inject_input(outbound.payload)
                    if outbound.result_transaction_id is not None:
                        core.settle_result_delivery(outbound.result_transaction_id)
                    if outbound.lifecycle_result is not None:
                        core.settle_lifecycle_result_delivery(
                            outbound.lifecycle_result
                        )

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
        phase_match = re.search(rb"PTRICH!([0-9]+) ", raw)
        phase = (
            system.cpu.mem_read64(int(phase_match.group(1)))
            if phase_match is not None
            else -1
        )
        self.assertEqual(position, len(payload), "test source was not fully fed")
        self.assertTrue(
            system.cpu.halted,
            "retained writer source test exceeded its "
            f"{SOURCE_LOAD_MAX_STEPS:,}-step watchdog "
            f"(core={core.state.value}, revision={core.model_revision}, "
            f"views={len(terminal_views)}, phase={phase}, "
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

        self.assertIn(b"PTREPORT 0 0 0 ", raw)
        self.assertEqual(core.state, TerminalState.ANSI)
        self.assertGreaterEqual(len(terminal_views), 3)
