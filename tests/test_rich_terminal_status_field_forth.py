"""Real machine-code Forth STATUS_FIELD writers and discovery byte oracles."""

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


class TestStatusFieldForth(_KDOSTestBase):
    def test_status_field_writers_validate_discovery_and_emit_exact_bytes(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        label, value = b"CPU", "Prêt".encode()
        bad_strings = (b"\n", b"\x7f", b"\xc2\x85", b"\xe2\x80\xa8", b"\xe2\x80\xa9", b"\xc2")
        specs = (
            (RetainedMessageType.OBJECT_DEFINE, 4, label, value),
            (RetainedMessageType.OBJECT_REPLACE, 4, label, value),
            (RetainedMessageType.OBJECT_REPLACE, 0, b"", value),
            (RetainedMessageType.OBJECT_REPLACE, 10, label, b""),
            (RetainedMessageType.OBJECT_REPLACE, 0, b"", b""),
        )
        expected = b""
        for sequence, (message, split, label_bytes, value_bytes) in enumerate(specs):
            prefix = struct.pack(
                "<QQQHHiQQiiII", 0x0102030405060708, 0x1112131415161718,
                0x2122232425262728, 11, 1, -7, 0x6162636465666768, 17,
                -2, 3, 10, 1,
            )
            body = struct.pack(
                "<IHHIIIIQ", 0x31465453, 1, 1, 2, split,
                len(label_bytes), len(value_bytes), 0,
            ) + label_bytes + value_bytes
            expected += encode_frame(Frame(
                message, 0x4142434445464748, sequence, 9, prefix + body,
            ), max_payload=128)

        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines += [
            "CREATE SF-RX 8192 ALLOT", "CREATE SF-TX 8192 ALLOT",
            "CREATE SF-EVENT PT-EVENT-SIZE ALLOT",
            "CREATE SF-STORAGE PT-SESSION-SIZE 7 + ALLOT",
            ": SF-S SF-STORAGE 7 + -8 AND ;",
            "CREATE SF-LABEL 200 ALLOT", "CREATE SF-VALUE 200 ALLOT",
            "CREATE SF-BAD 3 ALLOT", "CREATE SF-CAPS 64 ALLOT",
            "CREATE SF-FORMATS 64 ALLOT", "CREATE SF-STATUSES 80 8 * ALLOT",
            "VARIABLE SF-STATUS-I",
            ": SF-STATUS! SF-STATUSES SF-STATUS-I @ 8 * + !",
            "  1 SF-STATUS-I +! ;",
        ]
        for name in ("COLS", "ROWS", "SPLIT", "SEVERITY", "STATE", "LABEL-A", "LABEL-U", "VALUE-A", "VALUE-U"):
            lines.append(f"VARIABLE SF-{name}")
        lines += [
            ": SF-DEFAULTS 10 SF-COLS ! 1 SF-ROWS ! 4 SF-SPLIT !",
            "  PT-SEVERITY-SUCCESS SF-SEVERITY !",
            "  PT-STATUS-FIELD-EMPHASIZED SF-STATE !",
            f"  SF-LABEL SF-LABEL-A ! {len(label)} SF-LABEL-U !",
            f"  SF-VALUE SF-VALUE-A ! {len(value)} SF-VALUE-U ! ;",
            ": SF-ARGS 0x0102030405060708 0x1112131415161718",
            "  0x2122232425262728 0x6162636465666768 17",
            "  -2 3 SF-COLS @ SF-ROWS @ -7 PT-OBJECT-VISIBLE",
            "  SF-SPLIT @ SF-SEVERITY @ SF-STATE @",
            "  SF-LABEL-A @ SF-LABEL-U @ SF-VALUE-A @ SF-VALUE-U @ SF-S ;",
            ": SF-DEFINE SF-ARGS PT-STATUS-FIELD-DEFINE SF-STATUS! ;",
            ": SF-REPLACE SF-ARGS PT-STATUS-FIELD-REPLACE SF-STATUS! ;",
            ": SF-FEATURES! SF-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
            ": SF-INIT 0 SF-STATUS-I !",
            "  SF-RX 8192 SF-TX 8192 SF-EVENT PT-EVENT-SIZE SF-S",
            "    PT-INIT SF-STATUS!",
            "  PT-ST-ACTIVE SF-S _PT.S.STATE !",
            "  128 SF-S _PT.S.PEER-MAX-PAY !",
            "  4096 SF-S _PT.S.PEER-MAX-TX !",
            "  8192 SF-S _PT.S.PEER-GRANT !",
            "  8192 SF-S _PT.S.PEER-INITIAL !",
            "  0x4142434445464748 SF-S _PT.S.SESSION-ID !",
            "  9 SF-S _PT.S.EPOCH !",
            "  -1 SF-S _PT.S.RET-ENABLED? !",
            "  _PT-RD-AVAILABLE SF-S _PT.S.RET-STATE !",
            "  0x1001 SF-FEATURES!",
            "  -1 SF-S _PT.S.TX-OPEN? !",
            "  _PT-TX-PRESENT SF-S _PT.S.TX-KIND !",
            "  PT-CELL-NONE SF-S _PT.S.TX-CELL-MODE !",
            "  PT-RET-DELTA SF-S _PT.S.TX-RET-MODE !",
            f"  {len(specs)} SF-S _PT.S.TX-RET-OPS !",
            f"  {len(expected)} SF-S _PT.S.TX-RET-BYTES !",
            "  SF-LABEL 200 65 FILL SF-VALUE 200 66 FILL",
        ]
        for name, data in (("LABEL", label), ("VALUE", value)):
            lines += [f"  {byte} SF-{name} {offset} + C!" for offset, byte in enumerate(data)]
        lines += ["  SF-DEFAULTS ;", ": SF-RUN SF-INIT", "  30 EMIT TX-FLUSH"]
        lines += ["  1 SF-FEATURES! SF-DEFINE", "  0x1001 SF-FEATURES!"]
        invalid = [
            "0 SF-COLS !", "0 SF-ROWS !", "2 SF-ROWS !",
            "-1 SF-SPLIT !", "11 SF-SPLIT !", "0 SF-SPLIT !", "10 SF-SPLIT !",
            "5 SF-SEVERITY !", "-1 SF-SEVERITY !", "2 SF-STATE !",
            "SF-TX SF-LABEL-A !", "SF-S SF-LABEL-A !",
            "SF-TX SF-VALUE-A !", "SF-S SF-VALUE-A !",
            "0 SF-LABEL-A !", "0 SF-VALUE-A !",
            "0 SF-LABEL-U !", "0 SF-VALUE-U !",
            "200 SF-LABEL-U !", "200 SF-VALUE-U !",
            "0x100000000 SF-LABEL-U !", "0x100000000 SF-VALUE-U !",
        ]
        lines += [f"  SF-DEFAULTS {mutation} SF-DEFINE" for mutation in invalid]
        for name in ("LABEL", "VALUE"):
            for bad in bad_strings:
                lines += ["  SF-DEFAULTS"]
                lines += [f"  {byte} SF-BAD {offset} + C!" for offset, byte in enumerate(bad)]
                lines += [
                    f"  SF-BAD SF-{name}-A ! {len(bad)} SF-{name}-U !",
                    "  SF-DEFINE",
                ]
        lines += [
            "  31 EMIT TX-FLUSH",
            "  SF-S _PT.S.TX-RET-OPS-DONE @ SF-STATUS!",
            "  SF-S _PT.S.TX-RET-BYTES-DONE @ SF-STATUS!",
            "  SF-S _PT.S.TX-SEQ @ SF-STATUS!",
            "  SF-DEFAULTS 28 EMIT TX-FLUSH SF-DEFINE SF-REPLACE",
            "  0 SF-SPLIT ! 0 SF-LABEL-A ! 0 SF-LABEL-U ! SF-REPLACE",
            "  SF-DEFAULTS 10 SF-SPLIT !",
            "  0 SF-VALUE-A ! 0 SF-VALUE-U ! SF-REPLACE",
            "  0 SF-SPLIT ! 0 SF-LABEL-A ! 0 SF-LABEL-U ! SF-REPLACE",
            "  TX-FLUSH 29 EMIT TX-FLUSH",
            "  SF-S _PT.S.TX-RET-OPS-DONE @ SF-STATUS!",
            "  SF-S _PT.S.TX-RET-BYTES-DONE @ SF-STATUS!",
            "  _PT-SF-LABEL-A @ SF-STATUS! _PT-SF-VALUE-A @ SF-STATUS!",
            "  _PT-SF-TEXT-A @ SF-STATUS! DEPTH SF-STATUS!",
            '  S" SFSTATUS " TYPE',
            "  SF-STATUS-I @ 0 DO SF-STATUSES I 8 * + @ . LOOP TX-FLUSH ;",
            "SF-RUN",
            ": SF-CAP-SETUP SF-CAPS 64 0 FILL SF-FORMATS 64 0 FILL",
            "  0x31544552 SF-CAPS L! 0x1001 SF-CAPS 8 + _PT-U64!",
            "  2 SF-CAPS 16 + L! 1 SF-CAPS 20 + L!",
            "  1 SF-CAPS 24 + L! 4 SF-CAPS 32 + L!",
            "  8 SF-CAPS 40 + L! 296 SF-CAPS 48 + _PT-U64!",
            "  2 SF-FORMATS L! 1 SF-FORMATS 4 + L!",
            "  128 SF-FORMATS 48 + _PT-U64!",
            "  1 SF-S _PT.S.COLS ! 1 SF-S _PT.S.ROWS !",
            "  64 SF-S _PT.S.CLIENT-MAX-PAY !",
            "  96 SF-S _PT.S.PEER-MAX-PAY ! 136 SF-S _PT.S.TX-U !",
            "  64 _PT-RX-LEN ! SF-CAPS _PT-RX-P ! ;",
            ": SF-CAP-RUN SF-CAP-SETUP",
            '  S" SFCAPS " TYPE SF-S _PT-RET-CAPS-VALID? .',
            "  95 SF-S _PT.S.PEER-MAX-PAY ! SF-S _PT-RET-CAPS-VALID? .",
            "  96 SF-S _PT.S.PEER-MAX-PAY !",
            "  135 SF-S _PT.S.TX-U ! SF-S _PT-RET-CAPS-VALID? .",
            "  136 SF-S _PT.S.TX-U !",
            "  295 SF-CAPS 48 + _PT-U64! SF-S _PT-RET-CAPS-VALID? .",
            "  296 SF-CAPS 48 + _PT-U64!",
            "  0 SF-CAPS 32 + L! SF-S _PT-RET-CAPS-VALID? .",
            "  4 SF-CAPS 32 + L! SF-S _PT-RET-CAPS-VALID? .",
            "  SF-CAPS SF-S _PT.S.RET-CAPS 64 MOVE",
            "  SF-FORMATS _PT-RX-P ! SF-S _PT-RET-FORMATS-VALID? .",
            "  0 SF-FORMATS 48 + _PT-U64! SF-S _PT-RET-FORMATS-VALID? .",
            "  DEPTH . TX-FLUSH ;", "SF-CAP-RUN BYE",
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
        decoded = raw.decode("utf-8", errors="replace")
        self.assertEqual(position, len(program), "status field source was not fully fed")
        self.assertTrue(system.cpu.halted, "status field source-load watchdog exceeded")
        for diagnostic in (
            " ? (not found)", "Dictionary full", "dictionary overflow",
            "Stack underflow", "Stack overflow", "Return stack overflow",
            "nested definition", "branch out of range", "control-flow",
            "*** BUS FAULT", "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, decoded)
        empty_begin = raw.index(bytes((30,))) + 1
        empty_end = raw.index(bytes((31,)), empty_begin)
        self.assertEqual(raw[empty_begin:empty_end], b"")
        valid_begin = raw.index(bytes((28,)), empty_end) + 1
        valid_end = valid_begin + len(expected)
        self.assertEqual(raw[valid_begin:valid_end], expected)
        self.assertEqual(raw[valid_end:valid_end + 1], bytes((29,)))
        statuses = [0, 4] + [3] * (len(invalid) + 2 * len(bad_strings))
        statuses += [0, 0, 0] + [0] * len(specs)
        statuses += [len(specs), len(expected), 0, 0, 0, 0]
        match = re.search(rb"SFSTATUS ((?:-?[0-9]+ ){%d})" % len(statuses), raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(v) for v in match.group(1).split()], statuses)
        self.assertRegex(raw, rb"SFCAPS -1 0 0 0 0 -1 -1 0 0 ")
