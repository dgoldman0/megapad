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

    def test_resource_lifecycle_bytes_completions_credit_and_reset_gate(self) -> None:
        """Exercise the complete immutable-resource guest lifecycle boundary."""
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
                "CREATE PT-RES-RX 8192 ALLOT",
                "CREATE PT-RES-TX 8192 ALLOT",
                "CREATE PT-RES-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-RES-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-RES-SESSION PT-RES-SESSION-STORAGE 7 + -8 AND ;",
                "CREATE PT-RES-SHA3 32 ALLOT",
                "CREATE PT-RES-PIXEL 4 ALLOT",
                "CREATE PT-RES-RESULT 48 ALLOT",
                "CREATE PT-RES-CREDIT 8 ALLOT",
                "CREATE PT-RES-RESET 16 ALLOT",
                "CREATE PT-RES-MARK-BUFFER 8 ALLOT",
                "CREATE PT-RES-COMPLETIONS PT-COMPLETION-SIZE 10 * ALLOT",
                "CREATE PT-RES-SCRATCH PT-COMPLETION-SIZE ALLOT",
                "CREATE PT-RES-STATUSES 36 8 * ALLOT",
                "CREATE PT-RES-FACTS 13 8 * ALLOT",
                "VARIABLE PT-RES-COMP-I",
                "VARIABLE PT-RES-POLL-FAIL",
                "VARIABLE PT-RES-POLL-S",
                "VARIABLE PT-RES-POLL-HAS",
                "VARIABLE PT-RES-NONE-S",
                "VARIABLE PT-RES-NONE-HAS",
                "VARIABLE PT-RES-STATUS-I",
                "VARIABLE PT-RES-FACT-I",
                "VARIABLE PT-RES-RR-REQUEST",
                "VARIABLE PT-RES-RR-STATUS",
                "VARIABLE PT-RES-RR-ITEM",
                "VARIABLE PT-RES-RR-ACCEPTED",
                ": PT-RES-STATUS!  ( status -- )",
                "  PT-RES-STATUSES PT-RES-STATUS-I @ 8 * + !",
                "  PT-RES-STATUS-I @ 1+ PT-RES-STATUS-I ! ;",
                ": PT-RES-FACT!  ( value -- )",
                "  PT-RES-FACTS PT-RES-FACT-I @ 8 * + !",
                "  PT-RES-FACT-I @ 1+ PT-RES-FACT-I ! ;",
                ": PT-RES-MARK  ( u -- )",
                "  PT-RES-MARK-BUFFER _PT-U64!",
                "  PT-RES-MARK-BUFFER 8 TYPE TX-FLUSH ;",
                ": PT-RES-SHA3!",
                "  0x0706050403020100 PT-RES-SHA3 _PT-U64!",
                "  0x1716151413121110 PT-RES-SHA3 8 + _PT-U64!",
                "  0x2726252423222120 PT-RES-SHA3 16 + _PT-U64!",
                "  0x3736353433323130 PT-RES-SHA3 24 + _PT-U64!",
                "  0xDDCCBBAA PT-RES-PIXEL L! ;",
                ": PT-RES-PRIME",
                "  PT-RES-COMPLETIONS PT-COMPLETION-SIZE 10 * 0 FILL",
                "  PT-RES-SCRATCH PT-COMPLETION-SIZE 0 FILL",
                "  PT-RES-STATUSES 36 8 * 0 FILL",
                "  PT-RES-FACTS 13 8 * 0 FILL",
                "  PT-RES-COMP-I OFF PT-RES-POLL-FAIL OFF",
                "  PT-RES-STATUS-I OFF PT-RES-FACT-I OFF",
                "  PT-RES-NONE-S OFF PT-RES-NONE-HAS OFF",
                "  PT-RES-SHA3!",
                "  PT-RES-RX 8192 PT-RES-TX 8192",
                "    PT-RES-EVENT PT-EVENT-SIZE PT-RES-SESSION",
                "    PT-INIT PT-RES-STATUS!",
                "  PT-ST-ACTIVE PT-RES-SESSION _PT.S.STATE !",
                "  256 PT-RES-SESSION _PT.S.PEER-MAX-PAY !",
                "  4096 PT-RES-SESSION _PT.S.PEER-MAX-TX !",
                "  2048 PT-RES-SESSION _PT.S.PEER-GRANT !",
                "  2048 PT-RES-SESSION _PT.S.PEER-INITIAL !",
                "  0 PT-RES-SESSION _PT.S.PEER-SENT !",
                "  0 PT-RES-SESSION _PT.S.TX-SEQ !",
                "  0x4142434445464748 PT-RES-SESSION _PT.S.SESSION-ID !",
                "  9 PT-RES-SESSION _PT.S.EPOCH !",
                "  7 PT-RES-SESSION _PT.S.REVISION !",
                "  -1 PT-RES-SESSION _PT.S.RET-ENABLED? !",
                "  _PT-RD-AVAILABLE PT-RES-SESSION _PT.S.RET-STATE !",
                "  0x5 PT-RES-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  8 PT-RES-SESSION _PT.S.RET-CAPS 28 + L!",
                "  4 PT-RES-SESSION _PT.S.RET-CAPS 44 + L!",
                "  64 PT-RES-SESSION _PT.S.RET-CAPS 56 + _PT-U64!",
                "  1 PT-RES-SESSION _PT.S.RET-FORMATS L!",
                "  1 PT-RES-SESSION _PT.S.RET-FORMATS 4 + L!",
                "  PT-RESOURCE-RGBA8 PT-RES-SESSION _PT.S.RET-FORMATS 8 + L!",
                "  4 PT-RES-SESSION _PT.S.RET-FORMATS 12 + L!",
                "  4 PT-RES-SESSION _PT.S.RET-FORMATS 16 + L! ;",
                ": PT-RES-BEGIN-ARGS  ( resource -- ... )",
                "  0x0102030405060708 0x1112131415161718 ROT",
                "  PT-RESOURCE-RGBA8 1 1 0 4 ;",
                ": PT-RES-BEGIN  ( resource -- status )",
                "  PT-RES-BEGIN-ARGS PT-RES-SHA3 32 PT-RES-SESSION",
                "  PT-RESOURCE-BEGIN ;",
                ": PT-RES-ITEM-ARGS  ( resource -- owner generation resource )",
                "  0x0102030405060708 0x1112131415161718 ROT ;",
                ": PT-RES-SAVE-COMPLETION",
                "  PT-RES-COMPLETIONS",
                "    PT-RES-COMP-I @ PT-COMPLETION-SIZE * +",
                "    PT-RES-SESSION PT-COMPLETION-POLL",
                "  PT-RES-POLL-HAS ! PT-RES-POLL-S !",
                "  PT-RES-POLL-S @ PT-S-OK <> IF",
                "    PT-RES-POLL-FAIL @ 1+ PT-RES-POLL-FAIL !",
                "  THEN",
                "  PT-RES-POLL-HAS @ 0= IF",
                "    PT-RES-POLL-FAIL @ 1+ PT-RES-POLL-FAIL !",
                "  THEN",
                "  PT-RES-COMP-I @ 1+ PT-RES-COMP-I ! ;",
                ": PT-RES-POLL-NONE",
                "  PT-RES-SCRATCH PT-COMPLETION-SIZE 0 FILL",
                "  PT-RES-SCRATCH PT-RES-SESSION PT-COMPLETION-POLL",
                "  PT-RES-NONE-HAS ! PT-RES-NONE-S !",
                "  PT-RES-NONE-S @ PT-S-OK <> IF",
                "    PT-RES-POLL-FAIL @ 1+ PT-RES-POLL-FAIL !",
                "  THEN",
                "  PT-RES-NONE-HAS @ IF",
                "    PT-RES-POLL-FAIL @ 1+ PT-RES-POLL-FAIL !",
                "  THEN ;",
                ": PT-RES-RET-RESULT  ( request status item accepted -- status )",
                "  PT-RES-RR-ACCEPTED ! PT-RES-RR-ITEM !",
                "  PT-RES-RR-STATUS ! PT-RES-RR-REQUEST !",
                "  PT-RES-RESULT 48 0 FILL",
                "  PT-RES-RR-REQUEST @ PT-RES-RESULT W!",
                "  PT-RES-RR-STATUS @ PT-RES-RESULT 2 + W!",
                "  0x0102030405060708 PT-RES-RESULT 8 + _PT-U64!",
                "  0x1112131415161718 PT-RES-RESULT 16 + _PT-U64!",
                "  PT-RES-RR-ITEM @ PT-RES-RESULT 24 + _PT-U64!",
                "  7 PT-RES-RESULT 32 + _PT-U64!",
                "  PT-RES-RR-ACCEPTED @ PT-RES-RESULT 40 + _PT-U64!",
                "  PT-RES-SESSION _PT-RX-S !",
                "  PT-RES-RESULT _PT-RX-P !",
                "  _PT-M-RET-RESULT _PT-RX-TYPE !",
                "  48 _PT-RX-LEN ! 0 _PT-RX-SEQNO !",
                "  PT-RES-SESSION _PT-DISPATCH-RET-RESULT ;",
                ": PT-RES-CREDIT-ONE-SHORT",
                "  2243 PT-RES-CREDIT _PT-U64!",
                "  PT-RES-SESSION _PT-RX-S !",
                "  PT-RES-CREDIT _PT-RX-P !",
                "  _PT-M-CREDIT _PT-RX-TYPE !",
                "  8 _PT-RX-LEN ! 0 _PT-RX-SEQNO !",
                "  PT-RES-SESSION _PT-DISPATCH-CREDIT ;",
                ": PT-RES-CREDIT-COVER",
                "  2244 PT-RES-CREDIT _PT-U64!",
                "  PT-RES-SESSION _PT-RX-S !",
                "  PT-RES-CREDIT _PT-RX-P !",
                "  _PT-M-CREDIT _PT-RX-TYPE !",
                "  8 _PT-RX-LEN ! 0 _PT-RX-SEQNO !",
                "  PT-RES-SESSION _PT-DISPATCH-CREDIT ;",
                ": PT-RES-SOFT-RESET",
                "  PT-RES-RESET 16 0 FILL",
                "  10 PT-RES-RESET L!",
                "  7 PT-RES-RESET 8 + _PT-U64!",
                "  PT-RES-SESSION _PT-RX-S !",
                "  PT-RES-RESET _PT-RX-P !",
                "  _PT-M-SOFT-RESET-REQUEST _PT-RX-TYPE !",
                "  16 _PT-RX-LEN ! 0 _PT-RX-SEQNO !",
                "  PT-RES-SESSION _PT-DISPATCH-SOFT-RESET ;",
                ": PT-RES-RUN",
                "  PT-RES-PRIME",
                "  0x3130303047454252 PT-RES-MARK",
                "  0x2122232425262728 PT-RES-BEGIN-ARGS",
                "    PT-RES-SHA3 31 PT-RES-SESSION PT-RESOURCE-BEGIN",
                "    PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-BEGIN-ARGS",
                "    PT-RES-SESSION _PT.S.TX-A @ 32 PT-RES-SESSION",
                "    PT-RESOURCE-BEGIN PT-RES-STATUS!",
                "  0x1 PT-RES-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  0x2122232425262728 PT-RES-BEGIN PT-RES-STATUS!",
                "  0x5 PT-RES-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  0x2122232425262728 PT-RES-BEGIN PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS PT-RES-SESSION",
                "    PT-RESOURCE-DROP PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-BEGIN PT-RET-OK",
                "    0x2122232425262728 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS 0",
                "    PT-RES-PIXEL 4 PT-RES-SESSION PT-RESOURCE-CHUNK",
                "    PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x2122232425262728 PT-RES-ITEM-ARGS 0",
                "    0 0 PT-RES-SESSION PT-RESOURCE-CHUNK PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS 0",
                "    PT-RES-SESSION _PT.S.TX-A @ 4 PT-RES-SESSION",
                "    PT-RESOURCE-CHUNK PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS 0",
                "    PT-RES-PIXEL 4 PT-RES-SESSION PT-RESOURCE-CHUNK",
                "    PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS PT-RES-SESSION",
                "    PT-RESOURCE-COMMIT PT-RES-STATUS!",
                "  PT-RES-POLL-NONE",
                "  PT-RES-CREDIT-ONE-SHORT PT-RES-STATUS!",
                "  PT-RES-POLL-NONE",
                "  PT-RES-CREDIT-COVER PT-RES-STATUS!",
                "  0x2122232425262728 PT-RES-ITEM-ARGS PT-RES-SESSION",
                "    PT-RESOURCE-COMMIT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x2122232425262728 PT-RES-ITEM-ARGS PT-RES-SESSION",
                "    PT-RESOURCE-COMMIT PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-COMMIT PT-RET-OK",
                "    0x2122232425262728 4 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x2122232425262728 PT-RES-ITEM-ARGS PT-RES-SESSION",
                "    PT-RESOURCE-DROP PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-DROP PT-RET-OK",
                "    0x2122232425262728 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x292A2B2C2D2E2F30 PT-RES-BEGIN PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-BEGIN PT-RET-OK",
                "    0x292A2B2C2D2E2F30 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x292A2B2C2D2E2F30 PT-RES-ITEM-ARGS 9 PT-RES-SESSION",
                "    PT-RESOURCE-ABORT PT-RES-STATUS!",
                "  0x292A2B2C2D2E2F30 PT-RES-ITEM-ARGS",
                "    PT-RESOURCE-ABORT-CALLER-CANCEL PT-RES-SESSION",
                "    PT-RESOURCE-ABORT PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-ABORT PT-RET-ABORTED",
                "    0x292A2B2C2D2E2F30 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x3132333435363738 PT-RES-BEGIN PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-BEGIN PT-RET-OK",
                "    0x3132333435363738 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x3132333435363738 PT-RES-ITEM-ARGS 0",
                "    PT-RES-PIXEL 4 PT-RES-SESSION PT-RESOURCE-CHUNK",
                "    PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-CHUNK PT-RET-INVALID",
                "    0x3132333435363738 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  0x393A3B3C3D3E3F40 PT-RES-BEGIN PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  0x393A3B3C3D3E3F40 PT-RES-BEGIN PT-RES-STATUS!",
                "  PT-REQUEST-RESOURCE-BEGIN PT-RET-OK",
                "    0x393A3B3C3D3E3F40 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SAVE-COMPLETION",
                "  PT-RES-SOFT-RESET PT-RES-STATUS!",
                "  PT-RES-SESSION _PT.S.EPOCH @ PT-RES-FACT!",
                "  PT-RES-SESSION _PT.S.RESET-PENDING? @ PT-RES-FACT!",
                "  0x32303030444C4852 PT-RES-MARK",
                "  PT-REQUEST-RESOURCE-ABORT PT-RET-ABORTED",
                "    0x393A3B3C3D3E3F40 0 PT-RES-RET-RESULT PT-RES-STATUS!",
                "  PT-RES-SESSION _PT-APPLY-PENDING-RESET PT-RES-STATUS!",
                "  PT-RES-SESSION _PT.S.EPOCH @ PT-RES-FACT!",
                "  PT-RES-SESSION _PT.S.RESET-PENDING? @ PT-RES-FACT!",
                "  0x333030304C4F5052 PT-RES-MARK",
                "  PT-RES-SAVE-COMPLETION",
                "  PT-RES-SESSION _PT-APPLY-PENDING-RESET PT-RES-STATUS!",
                "  PT-RES-SESSION _PT.S.EPOCH @ PT-RES-FACT!",
                "  PT-RES-SESSION _PT.S.RESET-PENDING? @ PT-RES-FACT!",
                "  PT-RES-SESSION _PT.S.REVISION @ PT-RES-FACT!",
                "  PT-RES-SESSION _PT.S.STATE @ PT-RES-FACT!",
                "  DEPTH PT-RES-STATUS!",
                "  PT-RES-COMP-I @ PT-RES-FACT!",
                "  PT-RES-POLL-FAIL @ PT-RES-FACT!",
                "  PT-RES-NONE-S @ PT-RES-FACT!",
                "  PT-RES-NONE-HAS @ PT-RES-FACT!",
                "  PT-RES-STATUS-I @ PT-RES-FACT!",
                "  0x34303030444E4552 PT-RES-MARK",
                "  0x3530303053545352 PT-RES-MARK",
                "  PT-RES-STATUSES PT-RES-STATUS-I @ 8 * TYPE",
                "  0x3630303045545352 PT-RES-MARK",
                "  0x37303030504D4352 PT-RES-MARK",
                "  PT-RES-COMPLETIONS",
                "    PT-RES-COMP-I @ PT-COMPLETION-SIZE * TYPE",
                "  0x38303030454D4352 PT-RES-MARK",
                "  0x3930303054434652 PT-RES-MARK",
                "  PT-RES-FACTS PT-RES-FACT-I @ 8 * TYPE",
                "  0x3031303045434652 PT-RES-MARK ;",
                "PT-RES-RUN BYE",
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
            "resource lifecycle byte oracle exceeded its "
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

        owner = 0x0102030405060708
        generation = 0x1112131415161718
        resource1 = 0x2122232425262728
        resource2 = 0x292A2B2C2D2E2F30
        resource3 = 0x3132333435363738
        resource4 = 0x393A3B3C3D3E3F40
        digest = struct.pack(
            "<QQQQ",
            0x0706050403020100,
            0x1716151413121110,
            0x2726252423222120,
            0x3736353433323130,
        )

        def resource_begin(resource: int) -> bytes:
            return struct.pack(
                "<QQQIIIIQ32s",
                owner,
                generation,
                resource,
                1,
                1,
                1,
                0,
                4,
                digest,
            )

        def resource_item(resource: int) -> bytes:
            return struct.pack("<QQQ", owner, generation, resource)

        outgoing = [
            (RetainedMessageType.RESOURCE_BEGIN, resource_begin(resource1), 9),
            (
                RetainedMessageType.RESOURCE_CHUNK,
                struct.pack("<QQQQ", owner, generation, resource1, 0)
                + bytes.fromhex("aabbccdd"),
                9,
            ),
            (RetainedMessageType.RESOURCE_COMMIT, resource_item(resource1), 9),
            (RetainedMessageType.RESOURCE_DROP, resource_item(resource1), 9),
            (RetainedMessageType.RESOURCE_BEGIN, resource_begin(resource2), 9),
            (
                RetainedMessageType.RESOURCE_ABORT,
                struct.pack("<QQQH6x", owner, generation, resource2, 0),
                9,
            ),
            (RetainedMessageType.RESOURCE_BEGIN, resource_begin(resource3), 9),
            (
                RetainedMessageType.RESOURCE_CHUNK,
                struct.pack("<QQQQ", owner, generation, resource3, 0)
                + bytes.fromhex("aabbccdd"),
                9,
            ),
            (RetainedMessageType.RESOURCE_BEGIN, resource_begin(resource4), 9),
            (
                RetainedMessageType.RESOURCE_ABORT,
                struct.pack("<QQQH6x", owner, generation, resource4, 1),
                9,
            ),
        ]
        expected_before_hold = b"".join(
            encode_frame(
                Frame(
                    message_type,
                    0x4142434445464748,
                    sequence,
                    epoch,
                    payload,
                ),
                max_payload=256,
            )
            for sequence, (message_type, payload, epoch) in enumerate(outgoing)
        )
        expected_ack = encode_frame(
            Frame(
                0x0008,
                0x4142434445464748,
                len(outgoing),
                10,
                struct.pack("<IHH", 10, 0, 0),
            ),
            max_payload=256,
        )

        begin_marker = b"RBEG0001"
        hold_marker = b"RHLD0002"
        poll_marker = b"RPOL0003"
        end_marker = b"REND0004"
        status_marker = b"RSTS0005"
        status_end_marker = b"RSTE0006"
        completion_marker = b"RCMP0007"
        completion_end_marker = b"RCME0008"
        facts_marker = b"RFCT0009"
        facts_end_marker = b"RFCE0010"
        begin = raw.index(begin_marker) + len(begin_marker)
        hold_at = raw.index(hold_marker, begin)
        hold = hold_at + len(hold_marker)
        poll_at = raw.index(poll_marker, hold)
        poll = poll_at + len(poll_marker)
        end_at = raw.index(end_marker, poll)
        end = end_at + len(end_marker)
        self.assertEqual(raw[begin:hold_at], expected_before_hold)
        self.assertEqual(raw[hold:poll_at], b"")
        self.assertEqual(raw[poll:end_at], expected_ack)

        expected_statuses = (
            0,
            3,
            3,
            4,
            0,
            1,
            0,
            1,
            3,
            3,
            0,
            1,
            0,
            0,
            1,
            0,
            0,
            0,
            0,
            0,
            0,
            3,
            0,
            0,
            0,
            0,
            0,
            0,
            1,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        )
        status_begin = raw.index(status_marker, end) + len(status_marker)
        status_end = raw.index(status_end_marker, status_begin)
        self.assertEqual(
            raw[status_begin:status_end],
            struct.pack(f"<{len(expected_statuses)}q", *expected_statuses),
        )

        completion_rows = (
            (0x1000, 0, resource1, 0),
            (0x1001, 0, resource1, 4),
            (0x1002, 0, resource1, 4),
            (0x1003, 0, resource1, 0),
            (0x1000, 0, resource2, 0),
            (0x000C, 7, resource2, 0),
            (0x1000, 0, resource3, 0),
            (0x1001, 1, resource3, 0),
            (0x1000, 0, resource4, 0),
            (0x000C, 7, resource4, 0),
        )
        expected_completions = b"".join(
            struct.pack(
                "<10Q",
                2,
                request,
                status,
                0,
                0,
                7,
                owner,
                generation,
                resource,
                accepted,
            )
            for request, status, resource, accepted in completion_rows
        )
        completion_begin = raw.index(completion_marker, status_end) + len(
            completion_marker
        )
        completion_end = raw.index(completion_end_marker, completion_begin)
        self.assertEqual(raw[completion_begin:completion_end], expected_completions)

        expected_facts = (9, -1, 9, -1, 10, 0, 0, 4, 10, 0, 0, 0, 36)
        facts_begin = raw.index(facts_marker, completion_end) + len(facts_marker)
        facts_end = raw.index(facts_end_marker, facts_begin)
        self.assertEqual(
            raw[facts_begin:facts_end],
            struct.pack(f"<{len(expected_facts)}q", *expected_facts),
        )

    def test_series_writers_emit_exact_native_cell_bytes_and_accounting(self) -> None:
        """Pack both timestamp modes through the universal SERIES ABI."""
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
                "CREATE PT-SER-RX 8192 ALLOT",
                "CREATE PT-SER-TX 8192 ALLOT",
                "CREATE PT-SER-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-SER-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-SER-SESSION PT-SER-SESSION-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-UNIFORM-A-STORAGE 16 7 + ALLOT",
                ": PT-SER-UNIFORM-A PT-SER-UNIFORM-A-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-UNIFORM-B-STORAGE 16 7 + ALLOT",
                ": PT-SER-UNIFORM-B PT-SER-UNIFORM-B-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-EXPLICIT-A-STORAGE 32 7 + ALLOT",
                ": PT-SER-EXPLICIT-A PT-SER-EXPLICIT-A-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-EXPLICIT-B-STORAGE 32 7 + ALLOT",
                ": PT-SER-EXPLICIT-B PT-SER-EXPLICIT-B-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-EXPLICIT-BAD-STORAGE 32 7 + ALLOT",
                ": PT-SER-EXPLICIT-BAD PT-SER-EXPLICIT-BAD-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-MAX-SAMPLES-STORAGE 40 7 + ALLOT",
                ": PT-SER-MAX-SAMPLES PT-SER-MAX-SAMPLES-STORAGE 7 + -8 AND ;",
                "CREATE PT-SER-MARK-BUFFER 8 ALLOT",
                "CREATE PT-SER-STATUSES 20 8 * ALLOT",
                "CREATE PT-SER-FACTS 9 8 * ALLOT",
                "VARIABLE PT-SER-STATUS-I",
                "VARIABLE PT-SER-FACT-I",
                ": PT-SER-STATUS!  ( status -- )",
                "  PT-SER-STATUSES PT-SER-STATUS-I @ 8 * + !",
                "  PT-SER-STATUS-I @ 1+ PT-SER-STATUS-I ! ;",
                ": PT-SER-FACT!  ( value -- )",
                "  PT-SER-FACTS PT-SER-FACT-I @ 8 * + !",
                "  PT-SER-FACT-I @ 1+ PT-SER-FACT-I ! ;",
                ": PT-SER-MARK  ( u -- )",
                "  PT-SER-MARK-BUFFER _PT-U64!",
                "  PT-SER-MARK-BUFFER 8 TYPE TX-FLUSH ;",
                ": PT-SER-DATA!",
                "  -7 PT-SER-UNIFORM-A !",
                "  0x1122334455667788 PT-SER-UNIFORM-A 8 + !",
                "  9 PT-SER-UNIFORM-B !",
                "  -10 PT-SER-UNIFORM-B 8 + !",
                "  1000 PT-SER-EXPLICIT-A !",
                "  -11 PT-SER-EXPLICIT-A 8 + !",
                "  2000 PT-SER-EXPLICIT-A 16 + !",
                "  22 PT-SER-EXPLICIT-A 24 + !",
                "  3000 PT-SER-EXPLICIT-B !",
                "  -33 PT-SER-EXPLICIT-B 8 + !",
                "  4000 PT-SER-EXPLICIT-B 16 + !",
                "  44 PT-SER-EXPLICIT-B 24 + !",
                "  5000 PT-SER-EXPLICIT-BAD !",
                "  1 PT-SER-EXPLICIT-BAD 8 + !",
                "  5000 PT-SER-EXPLICIT-BAD 16 + !",
                "  2 PT-SER-EXPLICIT-BAD 24 + !",
                "  1 PT-SER-MAX-SAMPLES !",
                "  2 PT-SER-MAX-SAMPLES 8 + !",
                "  3 PT-SER-MAX-SAMPLES 16 + !",
                "  4 PT-SER-MAX-SAMPLES 24 + !",
                "  5 PT-SER-MAX-SAMPLES 32 + ! ;",
                ": PT-SER-PRIME",
                "  PT-SER-STATUSES 20 8 * 0 FILL",
                "  PT-SER-FACTS 9 8 * 0 FILL",
                "  PT-SER-STATUS-I OFF PT-SER-FACT-I OFF",
                "  PT-SER-DATA!",
                "  PT-SER-RX 8192 PT-SER-TX 8192",
                "    PT-SER-EVENT PT-EVENT-SIZE PT-SER-SESSION",
                "    PT-INIT PT-SER-STATUS!",
                "  PT-ST-ACTIVE PT-SER-SESSION _PT.S.STATE !",
                "  128 PT-SER-SESSION _PT.S.PEER-MAX-PAY !",
                "  4096 PT-SER-SESSION _PT.S.PEER-MAX-TX !",
                "  4096 PT-SER-SESSION _PT.S.PEER-GRANT !",
                "  4096 PT-SER-SESSION _PT.S.PEER-INITIAL !",
                "  0 PT-SER-SESSION _PT.S.PEER-SENT !",
                "  0 PT-SER-SESSION _PT.S.TX-SEQ !",
                "  0x4142434445464748 PT-SER-SESSION _PT.S.SESSION-ID !",
                "  9 PT-SER-SESSION _PT.S.EPOCH !",
                "  -1 PT-SER-SESSION _PT.S.RET-ENABLED? !",
                "  _PT-RD-AVAILABLE PT-SER-SESSION _PT.S.RET-STATE !",
                "  0x19 PT-SER-SESSION _PT.S.RET-CAPS 8 + _PT-U64!",
                "  8 PT-SER-SESSION _PT.S.RET-CAPS 36 + L!",
                "  7 PT-SER-SESSION _PT.S.RET-CAPS 40 + L!",
                "  1024 PT-SER-SESSION _PT.S.RET-CAPS 48 + _PT-U64!",
                "  4 PT-SER-SESSION _PT.S.RET-FORMATS 28 + L!",
                "  8 PT-SER-SESSION _PT.S.RET-FORMATS 32 + L!",
                "  64 PT-SER-SESSION _PT.S.RET-FORMATS 40 + _PT-U64!",
                "  -1 PT-SER-SESSION _PT.S.TX-OPEN? !",
                "  _PT-TX-PRESENT PT-SER-SESSION _PT.S.TX-KIND !",
                "  PT-CELL-NONE PT-SER-SESSION _PT.S.TX-CELL-MODE !",
                "  PT-RET-DELTA PT-SER-SESSION _PT.S.TX-RET-MODE !",
                "  7 PT-SER-SESSION _PT.S.TX-RET-OPS !",
                "  640 PT-SER-SESSION _PT.S.TX-RET-BYTES ! ;",
                ": PT-SER-OWNER  ( -- owner generation )",
                "  0x0102030405060708 0x1112131415161718 ;",
                ": PT-SER-RUN",
                "  PT-SER-PRIME",
                "  0x3130303047454253 PT-SER-MARK",
                "  PT-SER-OWNER 0x2122232425262728 4 2 0",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728 4",
                "    PT-SERIES-TIMESTAMP-EXPLICIT 1",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728 0",
                "    PT-SERIES-TIMESTAMP-UNIFORM 50",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728 9",
                "    PT-SERIES-TIMESTAMP-UNIFORM 50",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728 2 1000",
                "    PT-SER-UNIFORM-A 16 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 1000",
                "    0 0 PT-SER-SESSION PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 1000",
                "    PT-SER-UNIFORM-A 12 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 1000",
                "    PT-SER-SESSION _PT.S.TX-A @ 16 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 1000",
                "    PT-SER-MAX-SAMPLES 40 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x292A2B2C2D2E2F30",
                "    PT-SERIES-TIMESTAMP-EXPLICIT 0",
                "    PT-SER-EXPLICIT-BAD 32 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-SESSION _PT.S.TX-RET-OPS-DONE @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.TX-RET-BYTES-DONE @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.TX-SEQ @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.PEER-SENT @ PT-SER-FACT!",
                "  PT-SER-OWNER 0x2122232425262728 4",
                "    PT-SERIES-TIMESTAMP-UNIFORM 50",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x292A2B2C2D2E2F30 4",
                "    PT-SERIES-TIMESTAMP-EXPLICIT 0",
                "    PT-SER-SESSION PT-SERIES-DEFINE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 1000",
                "    PT-SER-UNIFORM-A 16 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728",
                "    PT-SERIES-TIMESTAMP-UNIFORM 2000",
                "    PT-SER-UNIFORM-B 16 PT-SER-SESSION",
                "    PT-SERIES-REPLACE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x292A2B2C2D2E2F30",
                "    PT-SERIES-TIMESTAMP-EXPLICIT 0",
                "    PT-SER-EXPLICIT-A 32 PT-SER-SESSION",
                "    PT-SERIES-APPEND PT-SER-STATUS!",
                "  PT-SER-OWNER 0x292A2B2C2D2E2F30",
                "    PT-SERIES-TIMESTAMP-EXPLICIT 0",
                "    PT-SER-EXPLICIT-B 32 PT-SER-SESSION",
                "    PT-SERIES-REPLACE PT-SER-STATUS!",
                "  PT-SER-OWNER 0x2122232425262728 PT-SER-SESSION",
                "    PT-SERIES-DROP PT-SER-STATUS!",
                "  PT-SER-OWNER 0x292A2B2C2D2E2F30 PT-SER-SESSION",
                "    PT-SERIES-DROP PT-SER-STATUS!",
                "  PT-SER-SESSION _PT.S.TX-RET-OPS-DONE @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.TX-RET-BYTES-DONE @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.TX-SEQ @ PT-SER-FACT!",
                "  PT-SER-SESSION _PT.S.PEER-SENT @ PT-SER-FACT!",
                "  DEPTH PT-SER-STATUS!",
                "  PT-SER-STATUS-I @ PT-SER-FACT!",
                "  0x32303030444E4553 PT-SER-MARK",
                "  0x3330303053545353 PT-SER-MARK",
                "  PT-SER-STATUSES PT-SER-STATUS-I @ 8 * TYPE",
                "  0x3430303045545353 PT-SER-MARK",
                "  0x3530303054434653 PT-SER-MARK",
                "  PT-SER-FACTS PT-SER-FACT-I @ 8 * TYPE",
                "  0x3630303045434653 PT-SER-MARK ;",
                "PT-SER-RUN BYE",
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
            "series writer byte oracle exceeded its "
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

        owner = 0x0102030405060708
        generation = 0x1112131415161718
        series1 = 0x2122232425262728
        series2 = 0x292A2B2C2D2E2F30

        def definition(series: int, mode: int, interval: int) -> bytes:
            return struct.pack(
                "<QQQIIQ",
                owner,
                generation,
                series,
                4,
                mode,
                interval,
            )

        def samples(
            series: int,
            message_type: RetainedMessageType,
            mode: int,
            first_timestamp: int,
            cells: bytes,
        ) -> tuple[RetainedMessageType, bytes]:
            stride = 8 if mode == 1 else 16
            return message_type, struct.pack(
                "<QQQIIQ",
                owner,
                generation,
                series,
                len(cells) // stride,
                mode,
                first_timestamp,
            ) + cells

        uniform_a = struct.pack("<qq", -7, 0x1122334455667788)
        uniform_b = struct.pack("<qq", 9, -10)
        explicit_a = struct.pack("<QqQq", 1000, -11, 2000, 22)
        explicit_b = struct.pack("<QqQq", 3000, -33, 4000, 44)
        operations = [
            (
                RetainedMessageType.SERIES_DEFINE,
                definition(series1, 1, 50),
            ),
            (
                RetainedMessageType.SERIES_DEFINE,
                definition(series2, 0, 0),
            ),
            samples(
                series1,
                RetainedMessageType.SERIES_APPEND,
                1,
                1000,
                uniform_a,
            ),
            samples(
                series1,
                RetainedMessageType.SERIES_REPLACE,
                1,
                2000,
                uniform_b,
            ),
            samples(
                series2,
                RetainedMessageType.SERIES_APPEND,
                0,
                0,
                explicit_a,
            ),
            samples(
                series2,
                RetainedMessageType.SERIES_REPLACE,
                0,
                0,
                explicit_b,
            ),
            (
                RetainedMessageType.SERIES_DROP,
                struct.pack("<QQQ", owner, generation, series1),
            ),
        ]
        expected = b"".join(
            encode_frame(
                Frame(
                    message_type,
                    0x4142434445464748,
                    sequence,
                    9,
                    payload,
                ),
                max_payload=128,
            )
            for sequence, (message_type, payload) in enumerate(operations)
        )

        begin_marker = b"SBEG0001"
        end_marker = b"SEND0002"
        status_marker = b"SSTS0003"
        status_end_marker = b"SSTE0004"
        facts_marker = b"SFCT0005"
        facts_end_marker = b"SFCE0006"
        begin = raw.index(begin_marker) + len(begin_marker)
        end_at = raw.index(end_marker, begin)
        end = end_at + len(end_marker)
        self.assertEqual(raw[begin:end_at], expected)

        expected_statuses = (
            0,
            3,
            3,
            3,
            3,
            3,
            3,
            3,
            3,
            3,
            3,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            3,
            0,
        )
        status_begin = raw.index(status_marker, end) + len(status_marker)
        status_end = raw.index(status_end_marker, status_begin)
        self.assertEqual(
            raw[status_begin:status_end],
            struct.pack(f"<{len(expected_statuses)}q", *expected_statuses),
        )

        expected_facts = (0, 0, 0, 0, 7, 640, 7, 640, 20)
        facts_begin = raw.index(facts_marker, status_end) + len(facts_marker)
        facts_end = raw.index(facts_end_marker, facts_begin)
        self.assertEqual(
            raw[facts_begin:facts_end],
            struct.pack(f"<{len(expected_facts)}q", *expected_facts),
        )

    def test_typed_object_writers_emit_exact_bytes_and_accounting(self) -> None:
        """Pack every universal typed OBJECT writer and generic mutation."""
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
                "CREATE PT-OBJ-RX 8192 ALLOT",
                "CREATE PT-OBJ-TX 8192 ALLOT",
                "CREATE PT-OBJ-EVENT PT-EVENT-SIZE ALLOT",
                "CREATE PT-OBJ-SESSION-STORAGE PT-SESSION-SIZE 7 + ALLOT",
                ": PT-OBJ-SESSION PT-OBJ-SESSION-STORAGE 7 + -8 AND ;",
                "CREATE PT-OBJ-POINTS-STORAGE 32 7 + ALLOT",
                ": PT-OBJ-POINTS PT-OBJ-POINTS-STORAGE 7 + -8 AND ;",
                "CREATE PT-OBJ-MAX-POINTS-STORAGE 48 7 + ALLOT",
                ": PT-OBJ-MAX-POINTS PT-OBJ-MAX-POINTS-STORAGE 7 + -8 AND ;",
                "CREATE PT-OBJ-UNIT 3 ALLOT",
                "CREATE PT-OBJ-BAD-UNIT 1 ALLOT",
                "CREATE PT-OBJ-MARK-BUFFER 8 ALLOT",
                "CREATE PT-OBJ-STATUSES 34 8 * ALLOT",
                "CREATE PT-OBJ-FACTS 9 8 * ALLOT",
                "VARIABLE PT-OBJ-STATUS-I",
                "VARIABLE PT-OBJ-FACT-I",
                ": PT-OBJ-STATUS!  ( status -- )",
                "  PT-OBJ-STATUSES PT-OBJ-STATUS-I @ 8 * + !",
                "  PT-OBJ-STATUS-I @ 1+ PT-OBJ-STATUS-I ! ;",
                ": PT-OBJ-FACT!  ( value -- )",
                "  PT-OBJ-FACTS PT-OBJ-FACT-I @ 8 * + !",
                "  PT-OBJ-FACT-I @ 1+ PT-OBJ-FACT-I ! ;",
                ": PT-OBJ-MARK  ( u -- )",
                "  PT-OBJ-MARK-BUFFER _PT-U64!",
                "  PT-OBJ-MARK-BUFFER 8 TYPE TX-FLUSH ;",
                ": PT-OBJ-OWNER  ( -- owner generation )",
                "  0x0102030405060708 0x1112131415161718 ;",
                ": PT-OBJ-COMMON-V  ( object visible -- common-prefix )",
                "  >R >R PT-OBJ-OWNER R>",
                "  0x6162636465666768 0",
                "  0x01020304 0x11121314 0xA1A2A3A4 0xB1B2B3B4 -7 R> ;",
                ": PT-OBJ-COMMON  ( object -- common-prefix )",
                "  PT-OBJECT-VISIBLE PT-OBJ-COMMON-V ;",
                ": PT-OBJ-GROUP  ( object -- common-prefix session )",
                "  PT-OBJ-COMMON PT-OBJ-SESSION ;",
                ": PT-OBJ-POLYLINE  ( object -- polyline-args )",
                "  PT-OBJ-COMMON 0x01000000 1 2 3 255",
                "  PT-POLYLINE-CLOSED PT-OBJ-POINTS 32 PT-OBJ-SESSION ;",
                ": PT-OBJ-IMAGE  ( object -- image-args )",
                "  PT-OBJ-COMMON 0x696A6B6C6D6E6F70",
                "  PT-IMAGE-FIT-COVER 200 PT-OBJ-SESSION ;",
                ": PT-OBJ-READOUT  ( object -- readout-args )",
                "  PT-OBJ-COMMON 1 2 3 4 5 6 7 8",
                "  PT-READOUT-FIXED 2 -123 100",
                "  PT-OBJ-UNIT 3 PT-OBJ-SESSION ;",
                ": PT-OBJ-METER  ( object -- meter-args )",
                "  PT-OBJ-COMMON 10 11 12 13 14 15 16 17",
                "  PT-METER-VERTICAL PT-METER-SHOW-VALUE",
                "  -100 100 -25 PT-OBJ-SESSION ;",
                ": PT-OBJ-STATUS  ( object -- status-args )",
                "  PT-OBJ-COMMON 20 21 22 23 24 25 26 27",
                "  -1 PT-STATUS-DIAMOND PT-OBJ-SESSION ;",
                ": PT-OBJ-PLOT  ( object -- plot-args )",
                "  PT-OBJ-COMMON 0x7172737475767778 -10 20",
                "  30 31 32 33 34 35 36 37",
                "  PT-PLOT-FILL-TO-MINIMUM PT-PLOT-DRAW-POINTS OR",
                "  PT-OBJ-SESSION ;",
                ": PT-OBJ-WAVEFORM  ( object -- waveform-args )",
                "  PT-OBJ-COMMON 0x7172737475767778 -1000 1000",
                "  40 41 42 43 44 45 46 47 0",
                "  PT-WAVEFORM-DRAW-ZERO-LINE PT-OBJ-SESSION ;",
                ": PT-OBJ-FEATURES!  ( features -- )",
                "  PT-OBJ-SESSION _PT.S.RET-CAPS 8 + _PT-U64! ;",
                ": PT-OBJ-IMAGE-FORMAT!  ( format -- )",
                "  PT-OBJ-SESSION _PT.S.RET-FORMATS 8 + L! ;",
                ": PT-OBJ-GLYPH-MAX!  ( bytes -- )",
                "  PT-OBJ-SESSION _PT.S.RET-FORMATS 24 + L! ;",
                ": PT-OBJ-DATA!",
                "  0 PT-OBJ-POINTS !",
                "  0x11111111 PT-OBJ-POINTS 8 + !",
                "  0x88888888 PT-OBJ-POINTS 16 + !",
                "  0xFFFFFFFF PT-OBJ-POINTS 24 + !",
                "  0 PT-OBJ-MAX-POINTS !",
                "  1 PT-OBJ-MAX-POINTS 8 + !",
                "  2 PT-OBJ-MAX-POINTS 16 + !",
                "  3 PT-OBJ-MAX-POINTS 24 + !",
                "  4 PT-OBJ-MAX-POINTS 32 + !",
                "  5 PT-OBJ-MAX-POINTS 40 + !",
                "  0xC2 PT-OBJ-UNIT C!",
                "  0xB5 PT-OBJ-UNIT 1+ C!",
                "  0x73 PT-OBJ-UNIT 2 + C!",
                "  0xC2 PT-OBJ-BAD-UNIT C! ;",
                ": PT-OBJ-PRIME",
                "  PT-OBJ-STATUSES 34 8 * 0 FILL",
                "  PT-OBJ-FACTS 9 8 * 0 FILL",
                "  PT-OBJ-STATUS-I OFF PT-OBJ-FACT-I OFF",
                "  PT-OBJ-DATA!",
                "  PT-OBJ-RX 8192 PT-OBJ-TX 8192",
                "    PT-OBJ-EVENT PT-EVENT-SIZE PT-OBJ-SESSION",
                "    PT-INIT PT-OBJ-STATUS!",
                "  PT-ST-ACTIVE PT-OBJ-SESSION _PT.S.STATE !",
                "  128 PT-OBJ-SESSION _PT.S.PEER-MAX-PAY !",
                "  4096 PT-OBJ-SESSION _PT.S.PEER-MAX-TX !",
                "  8192 PT-OBJ-SESSION _PT.S.PEER-GRANT !",
                "  8192 PT-OBJ-SESSION _PT.S.PEER-INITIAL !",
                "  0 PT-OBJ-SESSION _PT.S.PEER-SENT !",
                "  0 PT-OBJ-SESSION _PT.S.TX-SEQ !",
                "  0x090A0B0C0D0E0F10 PT-OBJ-SESSION _PT.S.SESSION-ID !",
                "  9 PT-OBJ-SESSION _PT.S.EPOCH !",
                "  -1 PT-OBJ-SESSION _PT.S.RET-ENABLED? !",
                "  _PT-RD-AVAILABLE PT-OBJ-SESSION _PT.S.RET-STATE !",
                "  0x1F PT-OBJ-FEATURES!",
                "  32 PT-OBJ-SESSION _PT.S.RET-CAPS 32 + L!",
                "  4 PT-OBJ-SESSION _PT.S.RET-CAPS 36 + L!",
                "  19 PT-OBJ-SESSION _PT.S.RET-CAPS 40 + L!",
                "  4096 PT-OBJ-SESSION _PT.S.RET-CAPS 48 + _PT-U64!",
                "  PT-RESOURCE-RGBA8 PT-OBJ-IMAGE-FORMAT!",
                "  2 PT-OBJ-SESSION _PT.S.RET-FORMATS 20 + L!",
                "  16 PT-OBJ-GLYPH-MAX!",
                "  2 PT-OBJ-SESSION _PT.S.RET-FORMATS 28 + L!",
                "  8 PT-OBJ-SESSION _PT.S.RET-FORMATS 32 + L!",
                "  64 PT-OBJ-SESSION _PT.S.RET-FORMATS 40 + _PT-U64!",
                "  64 PT-OBJ-SESSION _PT.S.RET-FORMATS 48 + _PT-U64!",
                "  -1 PT-OBJ-SESSION _PT.S.TX-OPEN? !",
                "  _PT-TX-PRESENT PT-OBJ-SESSION _PT.S.TX-KIND !",
                "  PT-CELL-NONE PT-OBJ-SESSION _PT.S.TX-CELL-MODE !",
                "  PT-RET-DELTA PT-OBJ-SESSION _PT.S.TX-RET-MODE !",
                "  19 PT-OBJ-SESSION _PT.S.TX-RET-OPS !",
                "  2390 PT-OBJ-SESSION _PT.S.TX-RET-BYTES ! ;",
                ": PT-OBJ-INVALIDS",
                "  0x1D PT-OBJ-FEATURES!",
                "  0x2122232425262728 PT-OBJ-GROUP",
                "    PT-GROUP-DEFINE PT-OBJ-STATUS!",
                "  0x1B PT-OBJ-FEATURES!",
                "  0x3132333435363738 PT-OBJ-IMAGE",
                "    PT-IMAGE-DEFINE PT-OBJ-STATUS!",
                "  0x1F PT-OBJ-FEATURES!",
                "  0 PT-OBJ-IMAGE-FORMAT!",
                "  0x3132333435363738 PT-OBJ-IMAGE",
                "    PT-IMAGE-DEFINE PT-OBJ-STATUS!",
                "  PT-RESOURCE-RGBA8 PT-OBJ-IMAGE-FORMAT!",
                "  0x17 PT-OBJ-FEATURES!",
                "  PT-OBJ-OWNER 0x393A3B3C3D3E3F40 -456 PT-OBJ-SESSION",
                "    PT-OBJECT-SET-VALUE PT-OBJ-STATUS!",
                "  0x0F PT-OBJ-FEATURES!",
                "  0x5152535455565758 PT-OBJ-PLOT",
                "    PT-PLOT-DEFINE PT-OBJ-STATUS!",
                "  0x1F PT-OBJ-FEATURES!",
                "  0x2122232425262728 2 PT-OBJ-COMMON-V PT-OBJ-SESSION",
                "    PT-GROUP-DEFINE PT-OBJ-STATUS!",
                "  0x292A2B2C2D2E2F30 PT-OBJ-COMMON",
                "    0x01000000 1 2 3 255 PT-POLYLINE-CLOSED",
                "    PT-OBJ-MAX-POINTS 48 PT-OBJ-SESSION",
                "    PT-POLYLINE-DEFINE PT-OBJ-STATUS!",
                "  0x292A2B2C2D2E2F30 PT-OBJ-COMMON",
                "    0x01000000 1 2 3 255 PT-POLYLINE-CLOSED",
                "    PT-OBJ-POINTS 1+ 32 PT-OBJ-SESSION",
                "    PT-POLYLINE-DEFINE PT-OBJ-STATUS!",
                "  6 PT-OBJ-GLYPH-MAX!",
                "  0x393A3B3C3D3E3F40 PT-OBJ-READOUT",
                "    PT-READOUT-DEFINE PT-OBJ-STATUS!",
                "  16 PT-OBJ-GLYPH-MAX!",
                "  0x393A3B3C3D3E3F40 PT-OBJ-COMMON 1 2 3 4 5 6 7 8",
                "    PT-READOUT-FIXED 2 -123 100",
                "    PT-OBJ-BAD-UNIT 1 PT-OBJ-SESSION",
                "    PT-READOUT-DEFINE PT-OBJ-STATUS!",
                "  0x4142434445464748 PT-OBJ-COMMON",
                "    10 11 12 13 14 15 16 17",
                "    PT-METER-VERTICAL PT-METER-SHOW-VALUE",
                "    10 10 10 PT-OBJ-SESSION",
                "    PT-METER-DEFINE PT-OBJ-STATUS!",
                "  PT-OBJ-OWNER 0x3132333435363738 2 PT-OBJ-SESSION",
                "    PT-OBJECT-SET-VISIBILITY PT-OBJ-STATUS! ;",
                ": PT-OBJ-VALIDS",
                "  0x2122232425262728 PT-OBJ-GROUP",
                "    PT-GROUP-DEFINE PT-OBJ-STATUS!",
                "  0x2122232425262728 PT-OBJ-GROUP",
                "    PT-GROUP-REPLACE PT-OBJ-STATUS!",
                "  0x292A2B2C2D2E2F30 PT-OBJ-POLYLINE",
                "    PT-POLYLINE-DEFINE PT-OBJ-STATUS!",
                "  0x292A2B2C2D2E2F30 PT-OBJ-POLYLINE",
                "    PT-POLYLINE-REPLACE PT-OBJ-STATUS!",
                "  0x3132333435363738 PT-OBJ-IMAGE",
                "    PT-IMAGE-DEFINE PT-OBJ-STATUS!",
                "  0x3132333435363738 PT-OBJ-IMAGE",
                "    PT-IMAGE-REPLACE PT-OBJ-STATUS!",
                "  0x393A3B3C3D3E3F40 PT-OBJ-READOUT",
                "    PT-READOUT-DEFINE PT-OBJ-STATUS!",
                "  0x393A3B3C3D3E3F40 PT-OBJ-READOUT",
                "    PT-READOUT-REPLACE PT-OBJ-STATUS!",
                "  0x4142434445464748 PT-OBJ-METER",
                "    PT-METER-DEFINE PT-OBJ-STATUS!",
                "  0x4142434445464748 PT-OBJ-METER",
                "    PT-METER-REPLACE PT-OBJ-STATUS!",
                "  0x494A4B4C4D4E4F50 PT-OBJ-STATUS",
                "    PT-STATUS-DEFINE PT-OBJ-STATUS!",
                "  0x494A4B4C4D4E4F50 PT-OBJ-STATUS",
                "    PT-STATUS-REPLACE PT-OBJ-STATUS!",
                "  0x5152535455565758 PT-OBJ-PLOT",
                "    PT-PLOT-DEFINE PT-OBJ-STATUS!",
                "  0x5152535455565758 PT-OBJ-PLOT",
                "    PT-PLOT-REPLACE PT-OBJ-STATUS!",
                "  0x595A5B5C5D5E5F60 PT-OBJ-WAVEFORM",
                "    PT-WAVEFORM-DEFINE PT-OBJ-STATUS!",
                "  0x595A5B5C5D5E5F60 PT-OBJ-WAVEFORM",
                "    PT-WAVEFORM-REPLACE PT-OBJ-STATUS!",
                "  PT-OBJ-OWNER 0x393A3B3C3D3E3F40 -456 PT-OBJ-SESSION",
                "    PT-OBJECT-SET-VALUE PT-OBJ-STATUS!",
                "  PT-OBJ-OWNER 0x3132333435363738 PT-OBJECT-HIDDEN",
                "    PT-OBJ-SESSION PT-OBJECT-SET-VISIBILITY PT-OBJ-STATUS!",
                "  PT-OBJ-OWNER 0x2122232425262728 PT-OBJ-SESSION",
                "    PT-OBJECT-DROP PT-OBJ-STATUS! ;",
                ": PT-OBJ-RUN",
                "  PT-OBJ-PRIME",
                "  0x313030304745424F PT-OBJ-MARK",
                "  PT-OBJ-INVALIDS",
                "  PT-OBJ-SESSION _PT.S.TX-RET-OPS-DONE @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.TX-RET-BYTES-DONE @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.TX-SEQ @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.PEER-SENT @ PT-OBJ-FACT!",
                "  PT-OBJ-VALIDS",
                "  PT-OBJ-OWNER 0x494A4B4C4D4E4F50 PT-OBJ-SESSION",
                "    PT-OBJECT-DROP PT-OBJ-STATUS!",
                "  PT-OBJ-SESSION _PT.S.TX-RET-OPS-DONE @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.TX-RET-BYTES-DONE @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.TX-SEQ @ PT-OBJ-FACT!",
                "  PT-OBJ-SESSION _PT.S.PEER-SENT @ PT-OBJ-FACT!",
                "  DEPTH PT-OBJ-STATUS!",
                "  PT-OBJ-STATUS-I @ PT-OBJ-FACT!",
                "  0x32303030444E454F PT-OBJ-MARK",
                "  0x333030305354534F PT-OBJ-MARK",
                "  PT-OBJ-STATUSES PT-OBJ-STATUS-I @ 8 * TYPE",
                "  0x343030304554534F PT-OBJ-MARK",
                "  0x353030305443464F PT-OBJ-MARK",
                "  PT-OBJ-FACTS PT-OBJ-FACT-I @ 8 * TYPE",
                "  0x363030304543464F PT-OBJ-MARK ;",
                "PT-OBJ-RUN BYE",
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
            "typed object writer byte oracle exceeded its "
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

        owner = 0x0102030405060708
        generation = 0x1112131415161718
        region = 0x6162636465666768
        group = 0x2122232425262728
        polyline = 0x292A2B2C2D2E2F30
        image = 0x3132333435363738
        readout = 0x393A3B3C3D3E3F40
        meter = 0x4142434445464748
        status = 0x494A4B4C4D4E4F50
        plot = 0x5152535455565758
        waveform = 0x595A5B5C5D5E5F60
        resource = 0x696A6B6C6D6E6F70
        series = 0x7172737475767778

        def common(object_id: int, kind: int) -> bytes:
            return struct.pack(
                "<QQQHHiQQIIII",
                owner,
                generation,
                object_id,
                kind,
                1,
                -7,
                region,
                0,
                0x01020304,
                0x11121314,
                0xA1A2A3A4,
                0xB1B2B3B4,
            )

        unit = "\N{MICRO SIGN}s".encode("utf-8")
        point_bytes = struct.pack(
            "<IIII",
            0,
            0x11111111,
            0x88888888,
            0xFFFFFFFF,
        )
        definitions = (
            (group, 1, b""),
            (
                polyline,
                2,
                struct.pack("<II4BI", 2, 0x01000000, 1, 2, 3, 255, 1)
                + point_bytes,
            ),
            (image, 3, struct.pack("<QIB3x", resource, 2, 200)),
            (
                readout,
                5,
                struct.pack(
                    "<8BIIqqII",
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    1,
                    2,
                    -123,
                    100,
                    len(unit),
                    0,
                )
                + unit,
            ),
            (
                meter,
                6,
                struct.pack(
                    "<8BIIqqqQ",
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    1,
                    1,
                    -100,
                    100,
                    -25,
                    0,
                ),
            ),
            (
                status,
                7,
                struct.pack(
                    "<8BqIIQ",
                    20,
                    21,
                    22,
                    23,
                    24,
                    25,
                    26,
                    27,
                    -1,
                    2,
                    0,
                    0,
                ),
            ),
            (
                plot,
                8,
                struct.pack(
                    "<Qqq8BII",
                    series,
                    -10,
                    20,
                    30,
                    31,
                    32,
                    33,
                    34,
                    35,
                    36,
                    37,
                    3,
                    0,
                ),
            ),
            (
                waveform,
                9,
                struct.pack(
                    "<Qqq8BqII",
                    series,
                    -1000,
                    1000,
                    40,
                    41,
                    42,
                    43,
                    44,
                    45,
                    46,
                    47,
                    0,
                    1,
                    0,
                ),
            ),
        )
        operations: list[tuple[RetainedMessageType, bytes]] = []
        for object_id, kind, body in definitions:
            payload = common(object_id, kind) + body
            operations.extend(
                (
                    (RetainedMessageType.OBJECT_DEFINE, payload),
                    (RetainedMessageType.OBJECT_REPLACE, payload),
                )
            )
        operations.extend(
            (
                (
                    RetainedMessageType.OBJECT_SET_VALUE,
                    struct.pack("<QQQq", owner, generation, readout, -456),
                ),
                (
                    RetainedMessageType.OBJECT_SET_VISIBILITY,
                    struct.pack("<QQQB7x", owner, generation, image, 0),
                ),
                (
                    RetainedMessageType.OBJECT_DROP,
                    struct.pack("<QQQ", owner, generation, group),
                ),
            )
        )
        self.assertEqual(len(operations), 19)
        expected = b"".join(
            encode_frame(
                Frame(
                    message_type,
                    0x090A0B0C0D0E0F10,
                    sequence,
                    9,
                    payload,
                ),
                max_payload=128,
            )
            for sequence, (message_type, payload) in enumerate(operations)
        )
        self.assertEqual(len(expected), 2390)

        begin_marker = b"OBEG0001"
        end_marker = b"OEND0002"
        status_marker = b"OSTS0003"
        status_end_marker = b"OSTE0004"
        facts_marker = b"OFCT0005"
        facts_end_marker = b"OFCE0006"
        begin = raw.index(begin_marker) + len(begin_marker)
        end_at = raw.index(end_marker, begin)
        end = end_at + len(end_marker)
        self.assertEqual(raw[begin:end_at], expected)

        expected_statuses = (
            (0,) + (4,) * 5 + (3,) * 7 + (0,) * 19 + (3, 0)
        )
        status_begin = raw.index(status_marker, end) + len(status_marker)
        status_end = raw.index(status_end_marker, status_begin)
        self.assertEqual(
            raw[status_begin:status_end],
            struct.pack(f"<{len(expected_statuses)}q", *expected_statuses),
        )

        expected_facts = (0, 0, 0, 0, 19, 2390, 19, 2390, 34)
        facts_begin = raw.index(facts_marker, status_end) + len(facts_marker)
        facts_end = raw.index(facts_end_marker, facts_begin)
        self.assertEqual(
            raw[facts_begin:facts_end],
            struct.pack(f"<{len(expected_facts)}q", *expected_facts),
        )

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
