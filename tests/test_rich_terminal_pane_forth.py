"""Real target-Forth pane publication and additive discovery byte oracles."""

from __future__ import annotations

import re
import struct

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_wire import RetainedMessageType
from tests.test_rich_terminal_forth import (
    MODULE_PATH,
    RUN_BATCH_STEPS,
    SOURCE_LOAD_MAX_STEPS,
    _source_lines,
)
from tests.test_system import (
    KDOS_TEST_EXT_MEM_MIB,
    _KDOSTestBase,
    _next_line_chunk,
    capture_uart,
    make_system,
)


class TestPaneForth(_KDOSTestBase):
    def test_pane_writers_negotiate_validate_and_emit_exact_bytes(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        title = "Pâne".encode()
        invalid_titles = (b"\n", b"\x7f", b"\xc2\x85", b"\xe2\x80\xa8", b"\xc2")
        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines += [
            "CREATE PN-RX 8192 ALLOT",
            "CREATE PN-TX 8192 ALLOT",
            "CREATE PN-EVENT PT-EVENT-SIZE ALLOT",
            "CREATE PN-STORAGE PT-SESSION-SIZE 7 + ALLOT",
            ": PN-S PN-STORAGE 7 + -8 AND ;",
            "CREATE PN-TITLE 200 ALLOT",
            "CREATE PN-BAD-TITLE 3 ALLOT",
            "CREATE PN-CAPS 64 ALLOT",
            "CREATE PN-FORMATS 64 ALLOT",
            "CREATE PN-STATUSES 40 8 * ALLOT",
            "VARIABLE PN-STATUS-I",
            ": PN-STATUS! PN-STATUSES PN-STATUS-I @ 8 * + !",
            "  1 PN-STATUS-I +! ;",
        ]
        for variable in (
            "PARENT", "VISIBLE", "REGION", "X", "Y", "COLS", "ROWS",
            "STATE", "TITLE-A", "TITLE-U", "OUTER-COLS",
        ):
            lines.append(f"VARIABLE PN-{variable}")
        lines += [
            ": PN-DEFAULTS",
            "  0 PN-PARENT ! 1 PN-VISIBLE !",
            "  0x7172737475767778 PN-REGION !",
            "  1 PN-X ! 1 PN-Y ! 8 PN-COLS ! 4 PN-ROWS !",
            "  PT-PANE-FOCUSED PN-STATE !",
            f"  PN-TITLE PN-TITLE-A ! {len(title)} PN-TITLE-U !",
            "  10 PN-OUTER-COLS ! ;",
            ": PN-ARGS",
            "  0x0102030405060708 0x1112131415161718",
            "  0x2122232425262728 0x6162636465666768 PN-PARENT @",
            "  -2 3 PN-OUTER-COLS @ 6 -7 PN-VISIBLE @",
            "  PN-REGION @ PN-X @ PN-Y @ PN-COLS @ PN-ROWS @",
            "  PN-STATE @ PN-TITLE-A @ PN-TITLE-U @ PN-S ;",
            ": PN-DEFINE PN-ARGS PT-PANE-DEFINE PN-STATUS! ;",
            ": PN-FEATURES! PN-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
            ": PN-INIT",
            "  0 PN-STATUS-I !",
            "  PN-RX 8192 PN-TX 8192 PN-EVENT PT-EVENT-SIZE PN-S",
            "    PT-INIT PN-STATUS!",
            "  PT-ST-ACTIVE PN-S _PT.S.STATE !",
            "  128 PN-S _PT.S.PEER-MAX-PAY !",
            "  4096 PN-S _PT.S.PEER-MAX-TX !",
            "  8192 PN-S _PT.S.PEER-GRANT !",
            "  8192 PN-S _PT.S.PEER-INITIAL !",
            "  0x4142434445464748 PN-S _PT.S.SESSION-ID !",
            "  9 PN-S _PT.S.EPOCH !",
            "  -1 PN-S _PT.S.RET-ENABLED? !",
            "  _PT-RD-AVAILABLE PN-S _PT.S.RET-STATE !",
            "  0x801 PN-FEATURES!",
            "  -1 PN-S _PT.S.TX-OPEN? !",
            "  _PT-TX-PRESENT PN-S _PT.S.TX-KIND !",
            "  PT-CELL-NONE PN-S _PT.S.TX-CELL-MODE !",
            "  PT-RET-DELTA PN-S _PT.S.TX-RET-MODE !",
            "  3 PN-S _PT.S.TX-RET-OPS !",
            f"  {3 * (144 + len(title))} PN-S _PT.S.TX-RET-BYTES !",
        ]
        lines += [f"  {byte} PN-TITLE {offset} + C!" for offset, byte in enumerate(title)]
        lines += ["  PN-DEFAULTS ;", ": PN-RUN PN-INIT", "  30 EMIT TX-FLUSH"]
        # Every failed call is bracketed as a single expected-empty byte span.
        lines += ["  1 PN-FEATURES! PN-DEFINE", "  0x801 PN-FEATURES!"]
        invalid_mutations = [
            "1 PN-PARENT !", "0 PN-REGION !",
            "0x6162636465666768 PN-REGION !", "2 PN-STATE !",
            "0 PN-VISIBLE !", "-1 PN-X !", "0 PN-COLS !",
            "11 PN-COLS !", "6 PN-ROWS !",
            "2 PN-OUTER-COLS !", "PN-TX PN-TITLE-A !",
            "PN-S PN-TITLE-A !", "0 PN-TITLE-A !",
            "0 PN-TITLE-U !", "200 PN-TITLE-U !",
        ]
        for mutation in invalid_mutations:
            lines += [f"  PN-DEFAULTS {mutation} PN-DEFINE"]
        for bad_title in invalid_titles:
            lines += ["  PN-DEFAULTS"]
            lines += [
                f"  {byte} PN-BAD-TITLE {offset} + C!"
                for offset, byte in enumerate(bad_title)
            ]
            lines += [
                f"  PN-BAD-TITLE PN-TITLE-A ! {len(bad_title)} PN-TITLE-U !",
                "  PN-DEFINE",
            ]
        lines += [
            "  31 EMIT TX-FLUSH",
            "  PN-S _PT.S.TX-RET-OPS-DONE @ PN-STATUS!",
            "  PN-S _PT.S.TX-RET-BYTES-DONE @ PN-STATUS!",
            "  PN-S _PT.S.TX-SEQ @ PN-STATUS!",
            "  PN-DEFAULTS",
            "  28 EMIT TX-FLUSH PN-DEFINE",
            "  PN-ARGS PT-PANE-REPLACE PN-STATUS!",
            "  1 PN-OUTER-COLS ! 0 PN-X ! 0 PN-Y ! 1 PN-COLS !",
            "  PN-ARGS PT-PANE-REPLACE PN-STATUS!",
            "  TX-FLUSH 29 EMIT TX-FLUSH",
            "  PN-S _PT.S.TX-RET-OPS-DONE @ PN-STATUS!",
            "  PN-S _PT.S.TX-RET-BYTES-DONE @ PN-STATUS!",
            "  _PT-PN-TITLE-A @ PN-STATUS!",
            "  _PT-PN-TITLE-U @ PN-STATUS!",
            "  DEPTH PN-STATUS!",
            '  S" PNSTATUS " TYPE',
            "  PN-STATUS-I @ 0 DO PN-STATUSES I 8 * + @ . LOOP",
            "  TX-FLUSH ;",
            "PN-RUN",
            # Construct CORE+PANES discovery with no controls, instruments,
            # glyph capacity or pane-specific title ceiling.
            ": PN-CAP-SETUP",
            "  PN-CAPS 64 0 FILL PN-FORMATS 64 0 FILL",
            "  0x31544552 PN-CAPS L!",
            "  0x801 PN-CAPS 8 + _PT-U64!",
            "  2 PN-CAPS 16 + L! 1 PN-CAPS 20 + L!",
            "  3 PN-CAPS 24 + L! 4 PN-CAPS 32 + L!",
            "  8 PN-CAPS 40 + L! 304 PN-CAPS 48 + _PT-U64!",
            "  2 PN-FORMATS L! 1 PN-FORMATS 4 + L!",
            "  128 PN-FORMATS 48 + _PT-U64!",
            "  1 PN-S _PT.S.COLS ! 1 PN-S _PT.S.ROWS !",
            "  64 PN-S _PT.S.CLIENT-MAX-PAY !",
            "  104 PN-S _PT.S.PEER-MAX-PAY !",
            "  144 PN-S _PT.S.TX-U !",
            "  64 _PT-RX-LEN ! PN-CAPS _PT-RX-P ! ;",
            ": PN-CAP-RUN PN-CAP-SETUP",
            '  S" PNCAPS " TYPE',
            "  PN-S _PT-RET-CAPS-VALID? .",
            "  1 PN-CAPS 24 + L! PN-S _PT-RET-CAPS-VALID? .",
            "  2 PN-CAPS 24 + L! PN-S _PT-RET-CAPS-VALID? .",
            "  103 PN-S _PT.S.PEER-MAX-PAY !",
            "  PN-S _PT-RET-CAPS-VALID? .",
            "  104 PN-S _PT.S.PEER-MAX-PAY !",
            "  303 PN-CAPS 48 + _PT-U64!",
            "  PN-S _PT-RET-CAPS-VALID? .",
            "  304 PN-CAPS 48 + _PT-U64!",
            "  0 PN-CAPS 32 + L! PN-S _PT-RET-CAPS-VALID? .",
            "  4 PN-CAPS 32 + L!",
            "  PN-CAPS PN-S _PT.S.RET-CAPS 64 MOVE",
            "  PN-FORMATS _PT-RX-P ! PN-S _PT-RET-FORMATS-VALID? .",
            "  0 PN-FORMATS 48 + _PT-U64!",
            "  PN-S _PT-RET-FORMATS-VALID? .",
            "  DEPTH . TX-FLUSH ;",
            "PN-CAP-RUN BYE",
        ]
        program = ("\n".join(lines) + "\n").encode()
        position = steps = 0
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
            executed = system.run_batch(min(RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS - steps))
            steps += max(executed, 1)

        raw = bytes(uart)
        text = raw.decode("utf-8", errors="replace")
        self.assertEqual(position, len(program), "pane test source was not fully fed")
        self.assertTrue(system.cpu.halted, "pane source-load watchdog exceeded")
        for diagnostic in (
            " ? (not found)", "Dictionary full", "dictionary overflow",
            "Stack underflow", "Stack overflow", "Return stack overflow",
            "nested definition", "branch out of range", "control-flow",
            "*** BUS FAULT", "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, text)
        empty_begin = raw.index(bytes((30,))) + 1
        empty_end = raw.index(bytes((31,)), empty_begin)
        self.assertEqual(raw[empty_begin:empty_end], b"")

        common = struct.pack(
            "<QQQHHiQQiiII", 0x0102030405060708, 0x1112131415161718,
            0x2122232425262728, 10, 1, -7, 0x6162636465666768, 0,
            -2, 3, 10, 6,
        )
        body = struct.pack(
            "<IHHQiiIIII", 0x31454E50, 1, 1, 0x7172737475767778,
            1, 1, 8, 4, len(title), 0,
        ) + title
        expected = b"".join(
            encode_frame(Frame(kind, 0x4142434445464748, sequence, 9, common + body), max_payload=128)
            for sequence, kind in enumerate((
                RetainedMessageType.OBJECT_DEFINE, RetainedMessageType.OBJECT_REPLACE,
            ))
        )
        # The title stays valid metadata when the existing content occupies
        # the top row and even when the pane is too narrow to paint a title.
        narrow_common = struct.pack(
            "<QQQHHiQQiiII", 0x0102030405060708, 0x1112131415161718,
            0x2122232425262728, 10, 1, -7, 0x6162636465666768, 0,
            -2, 3, 1, 6,
        )
        narrow_body = struct.pack(
            "<IHHQiiIIII", 0x31454E50, 1, 1, 0x7172737475767778,
            0, 0, 1, 4, len(title), 0,
        ) + title
        expected += encode_frame(Frame(
            RetainedMessageType.OBJECT_REPLACE, 0x4142434445464748,
            2, 9, narrow_common + narrow_body,
        ), max_payload=128)
        valid_begin = raw.index(bytes((28,)), empty_end) + 1
        valid_end = valid_begin + len(expected)
        self.assertEqual(raw[valid_begin:valid_end], expected)
        self.assertEqual(raw[valid_end:valid_end + 1], bytes((29,)))
        statuses = [0, 4] + [3] * (len(invalid_mutations) + len(invalid_titles))
        statuses += [0, 0, 0, 0, 0, 0, 3, len(expected), 0, 0, 0]
        match = re.search(rb"PNSTATUS ((?:-?[0-9]+ ){%d})" % len(statuses), raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(v) for v in match.group(1).split()], statuses)
        self.assertRegex(raw, rb"PNCAPS -1 0 -1 0 0 0 -1 0 0 ")
