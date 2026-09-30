"""Real Forth publication of validated INTEGER, CHOICE, and TEXT field content."""

from __future__ import annotations

import re
import struct

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_wire import RetainedMessageType
from tests.test_rich_terminal_forth import MODULE_PATH, RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS, _source_lines
from tests.test_system import KDOS_TEST_EXT_MEM_MIB, _KDOSTestBase, _next_line_chunk, capture_uart, make_system


FIELD_HEADER = struct.Struct("<IHHQIIiiIIiiIIqqqqII")
FIELD_LABEL = b"Gain"
FIELD_BODIES = (
    FIELD_HEADER.pack(0x31434446, 1, 1, 7, 0, 0, 0, 0, 4, 1, 5, 0, 7, 1, -2, -10, 10, 3, 0, 0),
    FIELD_HEADER.pack(0x31434446, 1, 2, 8, 0, 0, 0, 0, 4, 1, 5, 0, 7, 1, 42, 0, 0, 0, 2, 0)
    + struct.pack("<qII", -7, 3, 0) + b"Off" + struct.pack("<qII", 42, 2, 0) + b"On",
    FIELD_HEADER.pack(0x31434446, 1, 3, 9, 0, 0, 0, 0, 4, 1, 5, 0, 7, 1, 0, 0, 0, 0, 0, 3) + "hé".encode(),
    FIELD_HEADER.pack(0x31434446, 1, 1, 10, 1, 0, 0, 0, 0, 0, 0, 0, 12, 1, 8, -10, 10, 3, 0, 0),
)


def field_expected_frames() -> tuple[bytes, ...]:
    result = []
    for index, body in enumerate(FIELD_BODIES):
        label = FIELD_LABEL if index < 3 else b""
        prefix = struct.pack(
            "<QQQHHiQQIiiIIIII", 0x0102030405060708, 0x1112131415161718,
            18, 13, 11, -7, 0x6162636465666768, 0, 0,
            -2, 3, 12, 1, len(label), 0, len(body),
        )
        result.append(encode_frame(Frame(
            RetainedMessageType.CONTROL_DEFINE if index == 0 else RetainedMessageType.CONTROL_REPLACE,
            0x4142434445464748, index, 9, prefix + label + body,
        ), max_payload=256))
    return tuple(result)


def field_harness() -> bytes:
    lines = [
        "CREATE FH-RX 8192 ALLOT CREATE FH-TX 8192 ALLOT",
        "CREATE FH-EVENT PT-EVENT-SIZE ALLOT",
        "CREATE FH-STORAGE PT-SESSION-SIZE 7 + ALLOT",
        ": FH-S FH-STORAGE 7 + -8 AND ;",
        "CREATE FH-LABEL 4 ALLOT CREATE FH-CONTENT 512 ALLOT",
    ]
    for i, body in enumerate(FIELD_BODIES):
        lines += [f"CREATE FH-BODY-{i} {len(body)} ALLOT"]
    for name in ("INIT-STATUS", "STATUS", "STATE", "PARENT", "ORDER", "COLS", "ROWS", "LABEL-A", "LABEL-U", "SHORTCUT-A", "SHORTCUT-U", "CONTENT-A", "CONTENT-U", "OPS", "BYTES"):
        lines.append(f"VARIABLE FH-{name}")
    lines += [
        ": FH-DEFAULTS 11 FH-STATE ! 0 FH-PARENT ! 0 FH-ORDER !",
        "  12 FH-COLS ! 1 FH-ROWS ! FH-LABEL FH-LABEL-A ! 4 FH-LABEL-U !",
        "  0 FH-SHORTCUT-A ! 0 FH-SHORTCUT-U !",
        "  FH-CONTENT FH-CONTENT-A ! FH-BODY-0 FH-CONTENT 96 MOVE",
        "  96 FH-CONTENT-U ! ;",
        ": FH-ARGS 0x0102030405060708 0x1112131415161718 18",
        "  PT-CONTROL-FIELD FH-STATE @ -7 0x6162636465666768",
        "  FH-PARENT @ FH-ORDER @ -2 3 FH-COLS @ FH-ROWS @",
        "  FH-LABEL-A @ FH-LABEL-U @ FH-SHORTCUT-A @ FH-SHORTCUT-U @",
        "  FH-CONTENT-A @ FH-CONTENT-U @ FH-S ;",
        ": FH-DEFINE FH-ARGS PT-CONTROL-DEFINE FH-STATUS ! TX-FLUSH ;",
        ": FH-REPLACE FH-ARGS PT-CONTROL-REPLACE FH-STATUS ! TX-FLUSH ;",
        ": FH-FEATURES! FH-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
        ": FH-INITIALIZE",
        "  FH-RX 8192 FH-TX 8192 FH-EVENT PT-EVENT-SIZE FH-S",
        "  PT-INIT FH-INIT-STATUS ! PT-ST-ACTIVE FH-S _PT.S.STATE !",
        "  256 FH-S _PT.S.PEER-MAX-PAY ! 4096 FH-S _PT.S.PEER-MAX-TX !",
        "  8192 FH-S _PT.S.PEER-GRANT ! 8192 FH-S _PT.S.PEER-INITIAL !",
        "  0x4142434445464748 FH-S _PT.S.SESSION-ID ! 9 FH-S _PT.S.EPOCH !",
        "  -1 FH-S _PT.S.RET-ENABLED? !",
        "  _PT-RD-AVAILABLE FH-S _PT.S.RET-STATE ! 0x4101 FH-FEATURES!",
        "  -1 FH-S _PT.S.TX-OPEN? ! _PT-TX-PRESENT FH-S _PT.S.TX-KIND !",
        "  PT-CELL-NONE FH-S _PT.S.TX-CELL-MODE !",
        "  PT-RET-DELTA FH-S _PT.S.TX-RET-MODE !",
        "  4 FH-S _PT.S.TX-RET-OPS !",
        f"  {sum(map(len, field_expected_frames()))} FH-S _PT.S.TX-RET-BYTES !",
    ]
    for name, data in (("LABEL", FIELD_LABEL), *((f"BODY-{i}", body) for i, body in enumerate(FIELD_BODIES))):
        lines += [f"  {byte} FH-{name} {offset} + C!" for offset, byte in enumerate(data)]
    lines += ["  FH-DEFAULTS ;"]
    for i, body in enumerate(FIELD_BODIES):
        lines += [f": FH-CASE-{i} FH-DEFAULTS FH-BODY-{i} FH-CONTENT {len(body)} MOVE",
                  f"  {len(body)} FH-CONTENT-U !"]
        if i == 3:
            lines += ["  0 FH-LABEL-A ! 0 FH-LABEL-U !"]
        lines += ["  ;"]
    return ("\n".join(lines) + "\n").encode()


class TestFieldForth(_KDOSTestBase):
    def test_field_canonical_content_aliasing_geometry_and_publication(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[:len(memory)] = memory
        system._ext_mem[:len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]
        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH), field_harness().decode()]
        lines += [
            "CREATE FH-RESULTS 120 8 * ALLOT VARIABLE FH-RESULT-I",
            ": FH-SAVE FH-RESULTS FH-RESULT-I @ 8 * + ! 1 FH-RESULT-I +! ;",
            ": FH-TRY FH-DEFINE FH-STATUS @ FH-SAVE ;",
            ": FH-RUN 0 FH-RESULT-I ! FH-INITIALIZE FH-INIT-STATUS @ FH-SAVE",
            "  30 EMIT TX-FLUSH 0x101 FH-FEATURES! FH-TRY",
            "  0x4001 FH-FEATURES! FH-TRY 0x4101 FH-FEATURES!",
        ]
        invalid = [
            (0, "1 FH-PARENT !"), (0, "1 FH-ORDER !"),
            (0, "0 FH-COLS !"), (0, "35 FH-STATE !"), (0, "9 FH-STATE !"),
            (0, "FH-LABEL FH-SHORTCUT-A ! 1 FH-SHORTCUT-U !"),
            (0, "FH-S FH-CONTENT-A !"), (0, "FH-TX FH-CONTENT-A !"),
            (0, "FH-CONTENT FH-LABEL-A !"), (0, "95 FH-CONTENT-U !"),
            (0, "177 FH-CONTENT-U !"),
            (0, "0 FH-CONTENT L!"), (0, "2 FH-CONTENT 4 + W!"),
            (0, "4 FH-CONTENT 6 + W!"), (0, "0 FH-CONTENT 8 + _PT-U64!"),
            (0, "2 FH-CONTENT 16 + L!"), (0, "1 FH-CONTENT 20 + L!"),
            (0, "-1 FH-CONTENT 24 + L!"), (0, "0 FH-CONTENT 32 + L!"),
            (0, "0 FH-LABEL-A ! 0 FH-LABEL-U !"),
            (0, "-1 FH-CONTENT 40 + L!"), (0, "6 FH-CONTENT 40 + L!"),
            (0, "1 FH-CONTENT 44 + L!"), (0, "3 FH-CONTENT 40 + L!"),
            (0, "0 FH-CONTENT 48 + L!"),
            (0, "-11 FH-CONTENT 56 + _PT-U64!"),
            (0, "11 FH-CONTENT 56 + _PT-U64!"),
            (0, "0 FH-CONTENT 80 + _PT-U64!"),
            (0, "-1 FH-CONTENT 80 + _PT-U64!"),
            (0, "1 FH-CONTENT 88 + L!"), (0, "1 FH-CONTENT 92 + L!"),
            (0, "97 FH-CONTENT-U !"),
            (1, "1 FH-CONTENT 64 + _PT-U64!"),
            (1, "0 FH-CONTENT 88 + L!"), (1, "1 FH-CONTENT 92 + L!"),
            (1, "99 FH-CONTENT 56 + _PT-U64!"),
            (1, "42 FH-CONTENT 96 + _PT-U64!"),
            (1, "1 FH-CONTENT 108 + L!"), (1, "0 FH-CONTENT 104 + L!"),
            (1, "99 FH-CONTENT 104 + L!"), (1, "10 FH-CONTENT 112 + C!"),
            (1, "132 FH-CONTENT-U !"),
            (2, "1 FH-CONTENT 56 + _PT-U64!"),
            (2, "1 FH-CONTENT 88 + L!"), (2, "2 FH-CONTENT 92 + L!"),
            (2, "10 FH-CONTENT 96 + C!"),
            (2, "194 FH-CONTENT 96 + C! 133 FH-CONTENT 97 + C!"),
            (2, "226 FH-CONTENT 96 + C! 128 FH-CONTENT 97 + C! 168 FH-CONTENT 98 + C!"),
            (2, "194 FH-CONTENT 98 + C!"),
            (3, "1 FH-CONTENT 24 + L!"),
        ]
        lines += [f"  FH-CASE-{case} {mutation} FH-TRY" for case, mutation in invalid]
        lines += [
            "  31 EMIT TX-FLUSH FH-S _PT.S.TX-RET-OPS-DONE @ FH-SAVE",
            "  FH-S _PT.S.TX-RET-BYTES-DONE @ FH-SAVE",
            "  FH-S _PT.S.TX-SEQ @ FH-SAVE 28 EMIT TX-FLUSH",
            "  FH-CASE-0 FH-TRY FH-CASE-1 FH-REPLACE FH-STATUS @ FH-SAVE",
            "  FH-CASE-2 FH-REPLACE FH-STATUS @ FH-SAVE",
            "  FH-CASE-3 FH-REPLACE FH-STATUS @ FH-SAVE",
            "  29 EMIT TX-FLUSH FH-S _PT.S.TX-RET-OPS-DONE @ FH-SAVE",
            "  FH-S _PT.S.TX-RET-BYTES-DONE @ FH-SAVE",
            "  _PT-CT-CONTENT-A @ FH-SAVE _PT-FD-A @ FH-SAVE",
            "  _PT-FD-P @ FH-SAVE _PT-FD-PREV @ FH-SAVE DEPTH FH-SAVE",
            '  S" FHRESULTS " TYPE',
            "  FH-RESULT-I @ 0 DO FH-RESULTS I 8 * + @ . LOOP TX-FLUSH ;",
            "FH-RUN",
            "CREATE FH-CAPS 64 ALLOT CREATE FH-FORMATS 64 ALLOT",
            ": FH-CAP-SETUP FH-CAPS 64 0 FILL FH-FORMATS 64 0 FILL",
            "  0x31544552 FH-CAPS L! 0x4101 FH-CAPS 8 + _PT-U64!",
            "  2 FH-CAPS 16 + L! 1 FH-CAPS 20 + L!",
            "  1 FH-CAPS 24 + L! 4 FH-CAPS 32 + L!",
            "  8 FH-CAPS 40 + L! 376 FH-CAPS 48 + _PT-U64!",
            "  2 FH-FORMATS L! 1 FH-FORMATS 4 + L!",
            "  128 FH-FORMATS 48 + _PT-U64!",
            "  1 FH-S _PT.S.COLS ! 1 FH-S _PT.S.ROWS !",
            "  64 FH-S _PT.S.CLIENT-MAX-PAY !",
            "  176 FH-S _PT.S.PEER-MAX-PAY ! 216 FH-S _PT.S.TX-U !",
            "  64 _PT-RX-LEN ! FH-CAPS _PT-RX-P ! ;",
            ": FH-CAP-RUN FH-CAP-SETUP",
            '  S" FHCAPS " TYPE FH-S _PT-RET-CAPS-VALID? .',
            "  175 FH-S _PT.S.PEER-MAX-PAY ! FH-S _PT-RET-CAPS-VALID? .",
            "  176 FH-S _PT.S.PEER-MAX-PAY !",
            "  215 FH-S _PT.S.TX-U ! FH-S _PT-RET-CAPS-VALID? .",
            "  216 FH-S _PT.S.TX-U !",
            "  375 FH-CAPS 48 + _PT-U64! FH-S _PT-RET-CAPS-VALID? .",
            "  376 FH-CAPS 48 + _PT-U64!",
            "  55 FH-S _PT.S.CLIENT-MAX-PAY ! FH-S _PT-RET-CAPS-VALID? .",
            "  64 FH-S _PT.S.CLIENT-MAX-PAY !",
            "  0x4001 FH-CAPS 8 + _PT-U64! FH-S _PT-RET-CAPS-VALID? .",
            "  0x8101 FH-CAPS 8 + _PT-U64! FH-S _PT-RET-CAPS-VALID? .",
            "  0x4101 FH-CAPS 8 + _PT-U64! FH-S _PT-RET-CAPS-VALID? .",
            "  FH-CAPS FH-S _PT.S.RET-CAPS 64 MOVE",
            "  FH-FORMATS _PT-RX-P ! FH-S _PT-RET-FORMATS-VALID? .",
            "  DEPTH . TX-FLUSH ;", "FH-CAP-RUN BYE",
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
        self.assertTrue(system.cpu.halted, "FIELD source-load watchdog exceeded")
        for diagnostic in (
            b" ? (not found)", b"Dictionary full", b"dictionary overflow", b"Stack underflow",
            b"Stack overflow", b"Return stack overflow", b"nested definition",
            b"branch out of range", b"control-flow", b"*** BUS FAULT", b"*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, raw)
        empty_begin = raw.index(bytes((30,))) + 1
        empty_end = raw.index(bytes((31,)), empty_begin)
        self.assertEqual(raw[empty_begin:empty_end], b"")
        expected = b"".join(field_expected_frames())
        valid_begin = raw.index(bytes((28,)), empty_end) + 1
        self.assertEqual(raw[valid_begin:valid_begin + len(expected)], expected)
        self.assertEqual(raw[valid_begin + len(expected):valid_begin + len(expected) + 1], bytes((29,)))
        statuses = [0, 4, 4] + [3] * len(invalid) + [0, 0, 0] + [0] * 4
        statuses += [4, len(expected), 0, 0, 0, 0, 0]
        match = re.search(rb"FHRESULTS ((?:-?[0-9]+ ){%d})" % len(statuses), raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(v) for v in match.group(1).split()], statuses)
        self.assertRegex(raw, rb"FHCAPS -1 0 0 0 0 0 0 -1 -1 0 ")
