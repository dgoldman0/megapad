"""Receive FIELD adjustment frames through the production target-Forth module.

The wire vectors are assembled independently of the retained Python codec and
enter the real guest frame parser, descriptor publisher and public accessors.
"""

from __future__ import annotations

import re
import struct

from rich_terminal.apt1 import Frame, encode_frame
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


_SESSION_ID = 0x4142434445464748
_FIELD_FEATURES = 0x4101  # CORE + CONTROLS + FIELDS, without COLLECTIONS.


def _payload(
    *, kind: int = 11, revision: int = 9, content_revision: int = 7,
    adjustment: int = 1, owner: int = 11, generation: int = 22,
    control: int = 33, modifiers: int = 5, reserved: int = 0,
    tail: bytes | None = None,
) -> bytes:
    if tail is None:
        tail = struct.pack("<Qq", content_revision, adjustment)
    return struct.pack(
        "<QQQHHIQ", owner, generation, control, kind, modifiers, reserved, revision,
    ) + tail


def _store_bytes(name: str, data: bytes) -> list[str]:
    """Use little-endian stores without asking guest code to encode the oracle."""
    lines = [f"CREATE {name} {len(data)} ALLOT", f": {name}-INIT"]
    whole = len(data) // 8 * 8
    for offset in range(0, whole, 8):
        value = int.from_bytes(data[offset : offset + 8], "little")
        lines.append(f"  0x{value:016X} {name} {offset} + _PT-U64!")
    for offset in range(whole, len(data)):
        lines.append(f"  {data[offset]} {name} {offset} + C!")
    lines.append(";")
    return lines


class TestFieldForthInput(_KDOSTestBase):
    def test_adjust_receive_validates_wire_and_preserves_signed_accessors(self) -> None:
        # name, payload, negotiated features, initial state, valid, descriptor
        # suffix (tail bytes, content revision, adjustment, key, offset, x, y).
        cases = [
            (f"adjust_{index}", _payload(adjustment=delta), _FIELD_FEATURES, 3,
             True, [16, 7, delta, 0, 0, 0, 0])
            for index, delta in enumerate((1, -1, (1 << 63) - 1, -(1 << 63)))
        ]
        cases += [
            ("revision_one", _payload(content_revision=1), _FIELD_FEATURES, 3,
             True, [16, 1, 1, 0, 0, 0, 0]),
            ("existing_place", _payload(kind=2, tail=struct.pack("<QQII", 7, 99, 3, 0)),
             0x301, 3, True, [24, 7, 0, 99, 3, 0, 0]),
            ("existing_activate", _payload(kind=1, tail=b""), 0x101, 3,
             True, [0, 0, 0, 0, 0, 0, 0]),
            ("existing_scroll", _payload(kind=4, tail=struct.pack("<hhI", -3, 4, 0)),
             0x301, 3, True, [8, 0, 0, 0, 0, -3, 4]),
        ]
        rejected = [
            ("zero_adjustment", _payload(adjustment=0), _FIELD_FEATURES, 3),
            ("zero_content_revision", _payload(content_revision=0), _FIELD_FEATURES, 3),
            ("short_tail", _payload()[:-1], _FIELD_FEATURES, 3),
            ("trailing_byte", _payload() + b"\x00", _FIELD_FEATURES, 3),
            ("absent_tail", _payload()[:40], _FIELD_FEATURES, 3),
            ("keyed_tail_length", _payload() + bytes(8), _FIELD_FEATURES, 3),
            ("without_fields", _payload(), 0x301, 3),
            ("without_controls", _payload(), 0x4001, 3),
            ("stale_model_revision", _payload(revision=8), _FIELD_FEATURES, 3),
            ("future_model_revision", _payload(revision=10), _FIELD_FEATURES, 3),
            ("zero_owner", _payload(owner=0), _FIELD_FEATURES, 3),
            ("zero_generation", _payload(generation=0), _FIELD_FEATURES, 3),
            ("zero_control", _payload(control=0), _FIELD_FEATURES, 3),
            ("unknown_modifiers", _payload(modifiers=0x40), _FIELD_FEATURES, 3),
            ("nonzero_reserved", _payload(reserved=1), _FIELD_FEATURES, 3),
            ("unknown_kind", _payload(kind=12), _FIELD_FEATURES, 3),
            ("resyncing_state", _payload(), _FIELD_FEATURES, 4),
        ]
        cases.extend((*case, False, []) for case in rejected)

        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines += [
            "CREATE FI-RX 8192 ALLOT",
            "CREATE FI-TX 8192 ALLOT",
            "CREATE FI-EVENT-BYTES PT-EVENT-SIZE ALLOT",
            "CREATE FI-DESCRIPTOR PT-EVENT-SIZE ALLOT",
            "CREATE FI-STORAGE PT-SESSION-SIZE 7 + ALLOT",
            ": FI-S FI-STORAGE 7 + -8 AND ;",
            "VARIABLE FI-INIT-STATUS VARIABLE FI-STATUS VARIABLE FI-CONSUMED",
            "VARIABLE FI-FEATURES VARIABLE FI-STATE",
            ": FI-INIT",
            "  FI-RX 8192 FI-TX 8192 FI-EVENT-BYTES PT-EVENT-SIZE FI-S",
            "    PT-INIT FI-INIT-STATUS !",
            "  FI-STATE @ FI-S _PT.S.STATE !",
            "  64 FI-S _PT.S.CLIENT-MAX-PAY !",
            "  256 FI-S _PT.S.PEER-MAX-PAY !",
            "  4096 FI-S _PT.S.LOCAL-GRANT !",
            f"  0x{_SESSION_ID:X} FI-S _PT.S.SESSION-ID !",
            "  4 FI-S _PT.S.EPOCH ! 9 FI-S _PT.S.REVISION !",
            "  -1 FI-S _PT.S.RET-ENABLED? !",
            "  _PT-RD-AVAILABLE FI-S _PT.S.RET-STATE !",
            "  FI-FEATURES @ FI-S _PT.S.RET-CAPS 8 + _PT-U64!",
            "  FI-DESCRIPTOR PT-EVENT-SIZE 0 FILL ;",
            ": FI-RECEIVE",
            "  FI-S _PT.S.BIN-U !",
            "  FI-S _PT-BIN-A FI-S _PT.S.BIN-U @ MOVE",
            "  FI-S _PT-TRY-FRAME FI-CONSUMED ! FI-STATUS ! ;",
            ": FI-DESCRIBE",
            "  FI-DESCRIPTOR FI-S PT-EVENT-POLL SWAP . .",
            "  FI-S _PT.S.EVENT-PENDING @ .",
            "  FI-DESCRIPTOR PT-EVENT-TYPE@ .",
            "  FI-DESCRIPTOR PT-EVENT-REVISION@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-OWNER@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-GENERATION@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-ID@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-KIND@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-MODIFIERS@ .",
            "  FI-DESCRIPTOR PT-EVENT-DATA@ NIP .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-CONTENT-REVISION@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-ADJUSTMENT@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-ITEM-KEY@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-OFFSET@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-WHEEL-X@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-WHEEL-Y@ . ;",
            ": FI-REPORT",
            "  FI-INIT-STATUS @ . FI-STATUS @ . FI-CONSUMED @ .",
            "  FI-S PT-STATE@ . FI-S _PT.S.EVENT-PENDING @ .",
            "  FI-S _PT.S.RX-SEQ @ . FI-S _PT.S.LOCAL-RECEIVED @ .",
            "  FI-STATUS @ 0= IF FI-DESCRIBE THEN",
            "  DEPTH . CR TX-FLUSH ;",
        ]
        expected: dict[str, list[int]] = {}
        for index, (name, payload, features, initial_state, valid, tail_values) in enumerate(cases):
            encoded = encode_frame(Frame(0x0205, _SESSION_ID, 0, 4, payload))
            frame_name = f"FI-FRAME-{index}"
            lines += _store_bytes(frame_name, encoded)
            lines += [
                f": FI-CASE-{index}",
                f"  {features} FI-FEATURES ! {initial_state} FI-STATE ! FI-INIT",
                f"  {frame_name}-INIT {frame_name} {len(encoded)} FI-RECEIVE",
                f'  S" FIRESULT_{name} " TYPE FI-REPORT ;',
                f"FI-CASE-{index}",
            ]
            expected[name] = [0, 0 if valid else 2, -1, 3 if valid else 6,
                              -1 if valid else 0, 1, len(encoded)]
            if valid:
                kind = int.from_bytes(payload[24:26], "little")
                expected[name] += [0, -1, 0, 0x0205, 9, 11, 22, 33, kind, 5, *tail_values]
            expected[name].append(0)  # Balanced target data stack.

        # Guard public accessors against descriptors with a different event
        # family, wrong tail length, or non-adjustment control kind.
        lines += [
            ": FI-GUARD-SETUP",
            "  FI-DESCRIPTOR PT-EVENT-SIZE 0 FILL",
            "  PT-EVENT-CONTROL FI-DESCRIPTOR !",
            "  PT-CONTROL-ADJUST FI-DESCRIPTOR 40 + !",
            "  FI-EVENT-BYTES FI-DESCRIPTOR 48 + !",
            "  16 FI-DESCRIPTOR 56 + !",
            "  7 FI-EVENT-BYTES _PT-U64!",
            "  -5 FI-EVENT-BYTES 8 + _PT-U64! ;",
            ": FI-GUARD-READ",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-CONTENT-REVISION@ .",
            "  FI-DESCRIPTOR PT-CONTROL-EVENT-ADJUSTMENT@ . ;",
            ": FI-GUARDS",
            '  S" FIGUARDS " TYPE',
            "  FI-GUARD-SETUP FI-GUARD-READ",
            "  24 FI-DESCRIPTOR 56 + ! FI-GUARD-READ",
            "  FI-GUARD-SETUP PT-EVENT-KEY FI-DESCRIPTOR ! FI-GUARD-READ",
            "  FI-GUARD-SETUP PT-CONTROL-ACTIVATE FI-DESCRIPTOR 40 + ! FI-GUARD-READ",
            "  FI-GUARD-SETUP 0 FI-DESCRIPTOR 48 + ! FI-GUARD-READ",
            "  DEPTH . CR TX-FLUSH ;",
            "FI-GUARDS BYE",
        ]
        source = ("\n".join(lines) + "\n").encode()
        position = steps = 0
        while steps < SOURCE_LOAD_MAX_STEPS:
            if system.cpu.halted:
                break
            if system.cpu.idle and not system.uart.has_rx_data:
                if position >= len(source):
                    break
                chunk = _next_line_chunk(source, position)
                system.uart.inject_input(chunk)
                position += len(chunk)
                continue
            steps += max(system.run_batch(min(RUN_BATCH_STEPS, SOURCE_LOAD_MAX_STEPS - steps)), 1)

        raw = bytes(uart)
        text = raw.decode("utf-8", errors="replace")
        self.assertEqual(position, len(source), "field input test source was not fully fed")
        self.assertTrue(system.cpu.halted, "field input source-load watchdog exceeded")
        for diagnostic in (
            " ? (not found)", "Dictionary full", "dictionary overflow",
            "Stack underflow", "Stack overflow", "Return stack overflow",
            "nested definition", "branch out of range", "control-flow",
            "*** BUS FAULT", "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, text)
        for name, values in expected.items():
            matches = re.findall(rb"FIRESULT_" + name.encode() + rb" ((?:-?\d+ )+)\r?\n", raw)
            self.assertEqual(len(matches), 1, f"missing/duplicate result {name}: {text[-12000:]}")
            self.assertEqual([int(value) for value in matches[0].split()], values, name)
        guards = re.findall(rb"FIGUARDS ((?:-?\d+ )+)\r?\n", raw)
        self.assertEqual(len(guards), 1)
        self.assertEqual([int(value) for value in guards[0].split()], [7, -5, 0, 0, 0, 0, 0, 0, 0, 0, 0])
