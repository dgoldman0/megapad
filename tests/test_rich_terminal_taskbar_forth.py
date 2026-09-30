"""Taskbar typed-control publication from the complete target Forth module."""

from __future__ import annotations

import re
import struct

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_wire import RetainedMessageType
from tests.test_rich_terminal_forth import (
    MODULE_PATH, RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS, _source_lines,
)
from tests.test_system import (
    KDOS_TEST_EXT_MEM_MIB, _KDOSTestBase, _next_line_chunk, capture_uart, make_system,
)


TASKBAR_LABEL = "Musíc".encode()
TASKBAR_SHORTCUT = b"F1"
TASKBAR_SESSION = 0x4142434445464748


def taskbar_expected_frames() -> tuple[bytes, ...]:
    result = []
    for sequence, (message, identity, kind, state, z, parent, order, x, y, cols, label, shortcut) in enumerate((
        (RetainedMessageType.CONTROL_DEFINE, 17, 10, 3, -7, 0, 0, -2, 3, 20, b"", b""),
        (RetainedMessageType.CONTROL_DEFINE, 18, 11, 35, 0, 17, 0, 4, 0, 10, TASKBAR_LABEL, TASKBAR_SHORTCUT),
        (RetainedMessageType.CONTROL_DEFINE, 19, 12, 3, 0, 17, 1, 0, 0, 4, TASKBAR_LABEL, b""),
        (RetainedMessageType.CONTROL_REPLACE, 18, 11, 11, 0, 17, 0, 4, 0, 10, TASKBAR_LABEL, TASKBAR_SHORTCUT),
    )):
        payload = struct.pack(
            "<QQQHHiQQIiiIIIII", 0x0102030405060708, 0x1112131415161718,
            identity, kind, state, z, 0x6162636465666768, parent,
            order, x, y, cols, 1, len(label), len(shortcut), 0,
        ) + label + shortcut
        result.append(encode_frame(Frame(message, TASKBAR_SESSION, sequence, 9, payload), max_payload=256))
    result.append(encode_frame(Frame(
        RetainedMessageType.CONTROL_DROP, TASKBAR_SESSION, 4, 9,
        struct.pack("<QQQ", 0x0102030405060708, 0x1112131415161718, 18),
    ), max_payload=256))
    return tuple(result)


def taskbar_harness() -> bytes:
    lines = [
        "CREATE TB-RX 8192 ALLOT", "CREATE TB-TX 8192 ALLOT",
        "CREATE TB-EVENT PT-EVENT-SIZE ALLOT",
        "CREATE TB-STORAGE PT-SESSION-SIZE 7 + ALLOT",
        ": TB-S TB-STORAGE 7 + -8 AND ;",
        "CREATE TB-LABEL 200 ALLOT", "CREATE TB-SHORTCUT 200 ALLOT",
        "CREATE TB-BAD 3 ALLOT",
    ]
    for name in ("INIT-STATUS", "STATUS", "ID", "KIND", "STATE", "Z", "PARENT", "ORDER", "X", "Y", "COLS", "ROWS", "LABEL-A", "LABEL-U", "SHORTCUT-A", "SHORTCUT-U", "CONTENT-A", "CONTENT-U", "OPS", "BYTES"):
        lines.append(f"VARIABLE TB-{name}")
    lines += [
        ": TB-DEFAULTS 18 TB-ID ! PT-CONTROL-TASK TB-KIND !",
        "  35 TB-STATE ! 0 TB-Z ! 17 TB-PARENT ! 0 TB-ORDER !",
        "  4 TB-X ! 0 TB-Y ! 10 TB-COLS ! 1 TB-ROWS !",
        f"  TB-LABEL TB-LABEL-A ! {len(TASKBAR_LABEL)} TB-LABEL-U !",
        f"  TB-SHORTCUT TB-SHORTCUT-A ! {len(TASKBAR_SHORTCUT)} TB-SHORTCUT-U !",
        "  0 TB-CONTENT-A ! 0 TB-CONTENT-U ! ;",
        ": TB-BAR-ARGS TB-DEFAULTS 17 TB-ID ! PT-CONTROL-TASKBAR TB-KIND !",
        "  3 TB-STATE ! -7 TB-Z ! 0 TB-PARENT ! -2 TB-X ! 3 TB-Y !",
        "  20 TB-COLS ! 0 TB-LABEL-A ! 0 TB-LABEL-U !",
        "  0 TB-SHORTCUT-A ! 0 TB-SHORTCUT-U ! ;",
        ": TB-LAUNCHER-ARGS TB-DEFAULTS 19 TB-ID !",
        "  PT-CONTROL-LAUNCHER TB-KIND ! 3 TB-STATE ! 1 TB-ORDER !",
        "  0 TB-X ! 4 TB-COLS ! 0 TB-SHORTCUT-A ! 0 TB-SHORTCUT-U ! ;",
        ": TB-ARGS 0x0102030405060708 0x1112131415161718 TB-ID @",
        "  TB-KIND @ TB-STATE @ TB-Z @ 0x6162636465666768",
        "  TB-PARENT @ TB-ORDER @ TB-X @ TB-Y @ TB-COLS @ TB-ROWS @",
        "  TB-LABEL-A @ TB-LABEL-U @ TB-SHORTCUT-A @ TB-SHORTCUT-U @",
        "  TB-CONTENT-A @ TB-CONTENT-U @ TB-S ;",
        ": TB-DEFINE TB-ARGS PT-CONTROL-DEFINE TB-STATUS ! TX-FLUSH ;",
        ": TB-REPLACE TB-ARGS PT-CONTROL-REPLACE TB-STATUS ! TX-FLUSH ;",
        ": TB-DROP 0x0102030405060708 0x1112131415161718 18 TB-S",
        "  PT-CONTROL-DROP TB-STATUS ! TX-FLUSH ;",
        ": TB-FEATURES! TB-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
        ": TB-INITIALIZE",
        "  TB-RX 8192 TB-TX 8192 TB-EVENT PT-EVENT-SIZE TB-S",
        "    PT-INIT TB-INIT-STATUS !",
        "  PT-ST-ACTIVE TB-S _PT.S.STATE !",
        "  256 TB-S _PT.S.PEER-MAX-PAY ! 4096 TB-S _PT.S.PEER-MAX-TX !",
        "  8192 TB-S _PT.S.PEER-GRANT ! 8192 TB-S _PT.S.PEER-INITIAL !",
        f"  {TASKBAR_SESSION} TB-S _PT.S.SESSION-ID ! 9 TB-S _PT.S.EPOCH !",
        "  -1 TB-S _PT.S.RET-ENABLED? !",
        "  _PT-RD-AVAILABLE TB-S _PT.S.RET-STATE ! 0x2101 TB-FEATURES!",
        "  -1 TB-S _PT.S.TX-OPEN? ! _PT-TX-PRESENT TB-S _PT.S.TX-KIND !",
        "  PT-CELL-NONE TB-S _PT.S.TX-CELL-MODE !",
        "  PT-RET-DELTA TB-S _PT.S.TX-RET-MODE !",
        "  5 TB-S _PT.S.TX-RET-OPS !",
        f"  {sum(map(len, taskbar_expected_frames()))} TB-S _PT.S.TX-RET-BYTES !",
        "  TB-LABEL 200 65 FILL TB-SHORTCUT 200 66 FILL",
    ]
    for name, data in (("LABEL", TASKBAR_LABEL), ("SHORTCUT", TASKBAR_SHORTCUT)):
        lines += [f"  {byte} TB-{name} {offset} + C!" for offset, byte in enumerate(data)]
    lines += ["  TB-DEFAULTS ;"]
    return ("\n".join(lines) + "\n").encode()


class TestTaskbarForth(_KDOSTestBase):
    def test_taskbar_writers_shape_feature_gate_and_exact_bytes(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]
        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH), taskbar_harness().decode()]
        lines += [
            "CREATE TB-RESULTS 100 8 * ALLOT", "VARIABLE TB-RESULT-I",
            ": TB-SAVE TB-RESULTS TB-RESULT-I @ 8 * + ! 1 TB-RESULT-I +! ;",
            ": TB-TRY TB-DEFINE TB-STATUS @ TB-SAVE ;",
            ": TB-RUN 0 TB-RESULT-I ! TB-INITIALIZE",
            "  TB-INIT-STATUS @ TB-SAVE 30 EMIT TX-FLUSH",
            "  0x101 TB-FEATURES! TB-BAR-ARGS TB-TRY",
            "  TB-DEFAULTS TB-TRY TB-LAUNCHER-ARGS TB-TRY",
            "  0x2001 TB-FEATURES! TB-DEFAULTS TB-TRY",
            "  0x2101 TB-FEATURES!",
        ]
        invalid = [
            ("TB-DEFAULTS", "0 TB-PARENT !"), ("TB-DEFAULTS", "1 TB-Z !"),
            ("TB-DEFAULTS", "-1 TB-X !"), ("TB-DEFAULTS", "1 TB-Y !"),
            ("TB-DEFAULTS", "0 TB-COLS !"), ("TB-DEFAULTS", "2 TB-ROWS !"),
            ("TB-DEFAULTS", "-1 TB-ORDER !"),
            ("TB-DEFAULTS", "43 TB-STATE !"),  # selected + minimized
            ("TB-DEFAULTS", "9 TB-STATE !"), ("TB-DEFAULTS", "10 TB-STATE !"),
            ("TB-DEFAULTS", "7 TB-STATE !"), ("TB-DEFAULTS", "19 TB-STATE !"),
            ("TB-DEFAULTS", "0 TB-LABEL-A ! 0 TB-LABEL-U !"),
            ("TB-DEFAULTS", "TB-BAD TB-CONTENT-A ! 1 TB-CONTENT-U !"),
            ("TB-DEFAULTS", "TB-TX TB-LABEL-A !"),
            ("TB-DEFAULTS", "TB-S TB-SHORTCUT-A !"),
            ("TB-DEFAULTS", "TB-LABEL TB-SHORTCUT-A !"),
            ("TB-BAR-ARGS", "1 TB-PARENT !"), ("TB-BAR-ARGS", "1 TB-ORDER !"),
            ("TB-BAR-ARGS", "2 TB-ROWS !"), ("TB-BAR-ARGS", "35 TB-STATE !"),
            ("TB-BAR-ARGS", "TB-LABEL TB-LABEL-A ! 1 TB-LABEL-U !"),
            ("TB-LAUNCHER-ARGS", "35 TB-STATE !"),
            ("TB-LAUNCHER-ARGS", "11 TB-STATE !"),
        ]
        lines += [f"  {setup} {mutation} TB-TRY" for setup, mutation in invalid]
        bad_strings = (b"\n", b"\x7f", b"\xc2\x85", b"\xe2\x80\xa8", b"\xe2\x80\xa9", b"\xc2")
        for name in ("LABEL", "SHORTCUT"):
            for bad in bad_strings:
                lines += ["  TB-DEFAULTS"]
                lines += [f"  {byte} TB-BAD {offset} + C!" for offset, byte in enumerate(bad)]
                lines += [f"  TB-BAD TB-{name}-A ! {len(bad)} TB-{name}-U ! TB-TRY"]
        lines += [
            "  31 EMIT TX-FLUSH TB-S _PT.S.TX-RET-OPS-DONE @ TB-SAVE",
            "  TB-S _PT.S.TX-RET-BYTES-DONE @ TB-SAVE",
            "  TB-S _PT.S.TX-SEQ @ TB-SAVE 28 EMIT TX-FLUSH",
            "  TB-BAR-ARGS TB-TRY TB-DEFAULTS TB-TRY TB-LAUNCHER-ARGS TB-TRY",
            "  TB-DEFAULTS 11 TB-STATE ! TB-REPLACE TB-STATUS @ TB-SAVE",
            "  TB-DROP TB-STATUS @ TB-SAVE 29 EMIT TX-FLUSH",
            "  TB-S _PT.S.TX-RET-OPS-DONE @ TB-SAVE",
            "  TB-S _PT.S.TX-RET-BYTES-DONE @ TB-SAVE",
            "  _PT-CT-LABEL-A @ TB-SAVE _PT-CT-SHORTCUT-A @ TB-SAVE",
            "  _PT-CT-CONTENT-A @ TB-SAVE DEPTH TB-SAVE",
            '  S" TBRESULTS " TYPE',
            "  TB-RESULT-I @ 0 DO TB-RESULTS I 8 * + @ . LOOP TX-FLUSH ;",
            "TB-RUN",
            "CREATE TB-CAPS 64 ALLOT CREATE TB-FORMATS 64 ALLOT",
            ": TB-CAP-SETUP TB-CAPS 64 0 FILL TB-FORMATS 64 0 FILL",
            "  0x31544552 TB-CAPS L! 0x2101 TB-CAPS 8 + _PT-U64!",
            "  2 TB-CAPS 16 + L! 1 TB-CAPS 20 + L!",
            "  1 TB-CAPS 24 + L! 4 TB-CAPS 32 + L!",
            "  8 TB-CAPS 40 + L! 280 TB-CAPS 48 + _PT-U64!",
            "  2 TB-FORMATS L! 1 TB-FORMATS 4 + L!",
            "  128 TB-FORMATS 48 + _PT-U64!",
            "  1 TB-S _PT.S.COLS ! 1 TB-S _PT.S.ROWS !",
            "  64 TB-S _PT.S.CLIENT-MAX-PAY !",
            "  80 TB-S _PT.S.PEER-MAX-PAY ! 120 TB-S _PT.S.TX-U !",
            "  64 _PT-RX-LEN ! TB-CAPS _PT-RX-P ! ;",
            ": TB-CAP-RUN TB-CAP-SETUP",
            '  S" TBCAPS " TYPE TB-S _PT-RET-CAPS-VALID? .',
            "  0x2001 TB-CAPS 8 + _PT-U64! TB-S _PT-RET-CAPS-VALID? .",
            "  0x2100 TB-CAPS 8 + _PT-U64! TB-S _PT-RET-CAPS-VALID? .",
            "  0x12101 TB-CAPS 8 + _PT-U64! TB-S _PT-RET-CAPS-VALID? .",
            "  0x2101 TB-CAPS 8 + _PT-U64! TB-S _PT-RET-CAPS-VALID? .",
            "  TB-CAPS TB-S _PT.S.RET-CAPS 64 MOVE",
            "  TB-FORMATS _PT-RX-P ! TB-S _PT-RET-FORMATS-VALID? .",
            "  DEPTH . TX-FLUSH ;", "TB-CAP-RUN BYE",
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
        self.assertTrue(system.cpu.halted, "taskbar source-load watchdog exceeded")
        for diagnostic in (
            b" ? (not found)", b"Dictionary full", b"dictionary overflow",
            b"Stack underflow", b"Stack overflow", b"Return stack overflow",
            b"nested definition", b"branch out of range", b"control-flow",
            b"*** BUS FAULT", b"*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, raw)
        empty_begin = raw.index(bytes((30,))) + 1
        empty_end = raw.index(bytes((31,)), empty_begin)
        self.assertEqual(raw[empty_begin:empty_end], b"")
        expected = b"".join(taskbar_expected_frames())
        valid_begin = raw.index(bytes((28,)), empty_end) + 1
        self.assertEqual(raw[valid_begin:valid_begin + len(expected)], expected)
        self.assertEqual(raw[valid_begin + len(expected):valid_begin + len(expected) + 1], bytes((29,)))
        statuses = [0] + [4] * 4 + [3] * (len(invalid) + 2 * len(bad_strings))
        statuses += [0, 0, 0] + [0] * 5 + [5, len(expected), 0, 0, 0, 0]
        match = re.search(rb"TBRESULTS ((?:-?[0-9]+ ){%d})" % len(statuses), raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(v) for v in match.group(1).split()], statuses)
        self.assertRegex(raw, rb"TBCAPS -1 0 0 0 -1 -1 0 ")
