"""Real target-Forth structural STX1 walking and typed grid-role admission."""

from __future__ import annotations

import re
import struct

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_wire import RetainedMessageType
from tests.test_rich_terminal_forth import MODULE_PATH, RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS, _source_lines
from tests.test_system import KDOS_TEST_EXT_MEM_MIB, _KDOSTestBase, _next_line_chunk, capture_uart, make_system


def grid_content(*, typed: bool, revision: int = 1, styled: bool = False) -> bytes:
    header = struct.pack("<IHHQIIIIIIIIQQII", 0x31585453, 1, 0, revision,
                         1, 15, 0, 0, 1, 15, 3, 1, 1, 0, 0, 0)
    items = b""
    for key, role, text in ((1, 4, b"12"), (2, 5, b"=A1"), (3, 6, b"#ERR")):
        runs = struct.pack("<IIHH", 0, 2, 4, 0) if key == 1 and styled else b""
        items += struct.pack("<QIIIIHHII", key, 0, (key - 1) * 5, 1, 5,
                             role if typed else 1, int(key == 1), len(text), int(bool(runs)))
        items += text + runs
    return header + items


GRID_CONTENTS = (grid_content(typed=False), grid_content(typed=True), grid_content(typed=True, revision=2))
# Style-run semantics remain host-owned.  This extra input exercises bounded
# traversal before unsupported admission, and is never a successful publication.
GRID_STYLE_CONTENT = grid_content(typed=True, styled=True)
GRID_SOURCE_BODIES = (*GRID_CONTENTS, GRID_STYLE_CONTENT)


def grid_expected_frames() -> tuple[bytes, ...]:
    result = []
    for index, body in enumerate(GRID_CONTENTS):
        prefix = struct.pack("<QQQHHiQQIiiIIIII", 0x0102030405060708, 0x1112131415161718,
                             18, 6, 3, -7, 0x6162636465666768, 0, 0, -2, 3, 15, 1, 0, 0, len(body))
        result.append(encode_frame(Frame(
            RetainedMessageType.CONTROL_DEFINE if index == 0 else RetainedMessageType.CONTROL_REPLACE,
            0x4142434445464748, index, 9, prefix + body,
        ), max_payload=512))
    return tuple(result)


def grid_harness() -> bytes:
    lines = [
        "CREATE GC-RX 8192 ALLOT CREATE GC-TX 8192 ALLOT",
        "CREATE GC-EVENT PT-EVENT-SIZE ALLOT",
        "CREATE GC-STORAGE PT-SESSION-SIZE 7 + ALLOT",
        ": GC-S GC-STORAGE 7 + -8 AND ;",
        "CREATE GC-CONTENT 512 ALLOT",
    ]
    for i, body in enumerate(GRID_SOURCE_BODIES):
        lines += [f"CREATE GC-BODY-{i} {len(body)} ALLOT"]
    for name in ("INIT-STATUS", "STATUS", "CONTENT-A", "CONTENT-U", "OPS", "BYTES"):
        lines.append(f"VARIABLE GC-{name}")
    lines += [
        ": GC-ARGS 0x0102030405060708 0x1112131415161718 18",
        "  PT-CONTROL-TEXT-GRID 3 -7 0x6162636465666768 0 0 -2 3 15 1",
        "  0 0 0 0 GC-CONTENT-A @ GC-CONTENT-U @ GC-S ;",
        ": GC-DEFINE GC-ARGS PT-CONTROL-DEFINE GC-STATUS ! TX-FLUSH ;",
        ": GC-REPLACE GC-ARGS PT-CONTROL-REPLACE GC-STATUS ! TX-FLUSH ;",
        ": GC-FEATURES! GC-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
        ": GC-INITIALIZE",
        "  GC-RX 8192 GC-TX 8192 GC-EVENT PT-EVENT-SIZE GC-S",
        "  PT-INIT GC-INIT-STATUS ! PT-ST-ACTIVE GC-S _PT.S.STATE !",
        "  512 GC-S _PT.S.PEER-MAX-PAY ! 4096 GC-S _PT.S.PEER-MAX-TX !",
        "  8192 GC-S _PT.S.PEER-GRANT ! 8192 GC-S _PT.S.PEER-INITIAL !",
        "  0x4142434445464748 GC-S _PT.S.SESSION-ID ! 9 GC-S _PT.S.EPOCH !",
        "  -1 GC-S _PT.S.RET-ENABLED? !",
        "  _PT-RD-AVAILABLE GC-S _PT.S.RET-STATE ! 0x8301 GC-FEATURES!",
        "  -1 GC-S _PT.S.TX-OPEN? ! _PT-TX-PRESENT GC-S _PT.S.TX-KIND !",
        "  PT-CELL-NONE GC-S _PT.S.TX-CELL-MODE !",
        "  PT-RET-DELTA GC-S _PT.S.TX-RET-MODE !",
        "  3 GC-S _PT.S.TX-RET-OPS !",
        f"  {sum(map(len, grid_expected_frames()))} GC-S _PT.S.TX-RET-BYTES !",
    ]
    for i, data in enumerate(GRID_SOURCE_BODIES):
        lines += [f"  {byte} GC-BODY-{i} {offset} + C!" for offset, byte in enumerate(data)]
    lines += ["  ;"]
    for i, body in enumerate(GRID_SOURCE_BODIES):
        lines += [f": GC-CASE-{i} GC-BODY-{i} GC-CONTENT {len(body)} MOVE",
                  f"  GC-CONTENT GC-CONTENT-A ! {len(body)} GC-CONTENT-U ! ;"]
    return ("\n".join(lines) + "\n").encode()


class TestGridCellsForth(_KDOSTestBase):
    def test_grid_role_admission_validates_complete_bounded_structure(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[:len(memory)] = memory
        system._ext_mem[:len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]
        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH), grid_harness().decode()]
        lines += [
            "CREATE GC-RESULTS 80 8 * ALLOT VARIABLE GC-RESULT-I",
            ": GC-SAVE GC-RESULTS GC-RESULT-I @ 8 * + ! 1 GC-RESULT-I +! ;",
            ": GC-TRY GC-DEFINE GC-STATUS @ GC-SAVE ;",
            ": GC-RUN 0 GC-RESULT-I ! GC-INITIALIZE GC-INIT-STATUS @ GC-SAVE",
            "  30 EMIT TX-FLUSH 0x301 GC-FEATURES! GC-CASE-3 GC-TRY",
            "  GC-REPLACE GC-STATUS @ GC-SAVE",
            "  0x8101 GC-FEATURES! GC-CASE-0 GC-TRY",
            "  0x8301 GC-FEATURES!",
        ]
        invalid = [
            "0 GC-CONTENT L!", "2 GC-CONTENT 4 + W!", "1 GC-CONTENT 6 + W!",
            "71 GC-CONTENT-U !", "0 GC-CONTENT 40 + L!", "4 GC-CONTENT 40 + L!",
            "0xFFFFFFFF GC-CONTENT 40 + L!", "0 GC-CONTENT 96 + W!",
            "7 GC-CONTENT 96 + W!", "0xFFFFFFFF GC-CONTENT 100 + L!",
            "0xFFFFFFFF GC-CONTENT 104 + L!", "107 GC-CONTENT-U !",
            "121 GC-CONTENT-U !", f"{len(GRID_CONTENTS[1])-1} GC-CONTENT-U !",
            f"{len(GRID_CONTENTS[1])+1} GC-CONTENT-U !",
            "GC-TX GC-CONTENT-A !", "GC-S GC-CONTENT-A !",
        ]
        lines += [f"  GC-CASE-1 {mutation} GC-TRY" for mutation in invalid]
        lines += ["  GC-CASE-3 121 GC-CONTENT-U ! GC-TRY"]
        # The complete malformed structure wins over unsupported role admission.
        lines += [
            "  0x301 GC-FEATURES! GC-CASE-1 4 GC-CONTENT 40 + L! GC-TRY",
            "  31 EMIT TX-FLUSH GC-S _PT.S.TX-RET-OPS-DONE @ GC-SAVE",
            "  GC-S _PT.S.TX-RET-BYTES-DONE @ GC-SAVE",
            "  GC-S _PT.S.TX-SEQ @ GC-SAVE",
            "  GC-S PT-RET-GRID-CELLS? GC-SAVE 28 EMIT TX-FLUSH",
            "  GC-CASE-0 GC-TRY 0x8301 GC-FEATURES!",
            "  GC-CASE-1 GC-REPLACE GC-STATUS @ GC-SAVE",
            "  GC-CASE-2 GC-REPLACE GC-STATUS @ GC-SAVE",
            "  29 EMIT TX-FLUSH GC-S PT-RET-GRID-CELLS? GC-SAVE",
            "  GC-S _PT.S.TX-RET-OPS-DONE @ GC-SAVE",
            "  GC-S _PT.S.TX-RET-BYTES-DONE @ GC-SAVE",
            "  _PT-GC-A @ GC-SAVE _PT-GC-P @ GC-SAVE _PT-GC-END @ GC-SAVE",
            "  _PT-CT-CONTENT-A @ GC-SAVE DEPTH GC-SAVE",
            '  S" GCRESULTS " TYPE',
            "  GC-RESULT-I @ 0 DO GC-RESULTS I 8 * + @ . LOOP TX-FLUSH ;",
            "GC-RUN",
            "CREATE GC-CAPS 64 ALLOT",
            ": GC-CAP-RUN GC-CAPS 64 0 FILL 0x31544552 GC-CAPS L!",
            "  0x8301 GC-CAPS 8 + _PT-U64!",
            "  2 GC-CAPS 16 + L! 1 GC-CAPS 20 + L!",
            "  1 GC-CAPS 24 + L! 4 GC-CAPS 32 + L!",
            "  8 GC-CAPS 40 + L! 352 GC-CAPS 48 + _PT-U64!",
            "  1 GC-S _PT.S.COLS ! 1 GC-S _PT.S.ROWS !",
            "  64 GC-S _PT.S.CLIENT-MAX-PAY ! 152 GC-S _PT.S.PEER-MAX-PAY !",
            "  192 GC-S _PT.S.TX-U ! 64 _PT-RX-LEN ! GC-CAPS _PT-RX-P !",
            '  S" GCCAPS " TYPE GC-S _PT-RET-CAPS-VALID? .',
            "  0x8101 GC-CAPS 8 + _PT-U64! GC-S _PT-RET-CAPS-VALID? .",
            "  0x8201 GC-CAPS 8 + _PT-U64! GC-S _PT-RET-CAPS-VALID? .",
            "  0x18301 GC-CAPS 8 + _PT-U64! GC-S _PT-RET-CAPS-VALID? .",
            "  0x301 GC-CAPS 8 + _PT-U64! GC-S _PT-RET-CAPS-VALID? .",
            "  DEPTH . TX-FLUSH ;", "GC-CAP-RUN BYE",
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
        self.assertEqual(position, len(program))
        self.assertTrue(system.cpu.halted, "GRID_CELLS source-load watchdog exceeded")
        for diagnostic in (b" ? (not found)", b"Dictionary full", b"dictionary overflow",
                           b"Stack underflow", b"Stack overflow", b"Return stack overflow",
                           b"nested definition", b"branch out of range", b"control-flow",
                           b"*** BUS FAULT", b"*** PRIVILEGE FAULT"):
            self.assertNotIn(diagnostic, raw)
        empty_begin = raw.index(bytes((30,))) + 1
        empty_end = raw.index(bytes((31,)), empty_begin)
        self.assertEqual(raw[empty_begin:empty_end], b"")
        expected = b"".join(grid_expected_frames())
        valid_begin = raw.index(bytes((28,)), empty_end) + 1
        self.assertEqual(raw[valid_begin:valid_begin + len(expected)], expected)
        self.assertEqual(raw[valid_begin + len(expected):valid_begin + len(expected) + 1], bytes((29,)))
        statuses = [0, 4, 4, 4] + [3] * (len(invalid) + 2) + [0, 0, 0, 0]
        statuses += [0, 0, 0, -1, 3, len(expected), 0, 0, 0, 0, 0]
        match = re.search(rb"GCRESULTS ((?:-?[0-9]+ ){%d})" % len(statuses), raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(v) for v in match.group(1).split()], statuses)
        self.assertRegex(raw, rb"GCCAPS -1 0 0 0 -1 0 ")
