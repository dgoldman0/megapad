"""The guest module checks VECTOR's point budget and the owner's share of it."""

from __future__ import annotations

import re

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


class TestVectorBudgetForth(_KDOSTestBase):
    def test_formats_and_owner_quotas_check_the_point_budget(self) -> None:
        memory, ext_memory, cpu_state = self._snapshot_data()
        system = make_system(ram_kib=1024, ext_mem_mib=KDOS_TEST_EXT_MEM_MIB)
        uart = capture_uart(system)
        system.cpu.mem[: len(memory)] = memory
        system._ext_mem[: len(ext_memory)] = ext_memory
        self._restore_cpu_state(system.cpu, cpu_state)
        system.uart._tx_ring_base = system.cpu.regs[19]

        lines = ["ENTER-USERLAND", *_source_lines(MODULE_PATH)]
        lines += [
            "CREATE VB-STORAGE PT-SESSION-SIZE 7 + ALLOT",
            ": VB-S VB-STORAGE 7 + -8 AND ;",
            "CREATE VB-FORMATS 64 ALLOT",
            # CORE and VECTOR, one object, a 512-byte transaction; formats
            # allow four points a line.
            ": VB-SETUP VB-S PT-SESSION-SIZE 0 FILL VB-FORMATS 64 0 FILL",
            "  0x03 VB-S _PT.S.RET-CAPS 8 + _PT-U64!",
            "  1 VB-S _PT.S.RET-CAPS 24 + L! 1 VB-S _PT.S.RET-CAPS 32 + L!",
            "  512 VB-S _PT.S.RET-CAPS 48 + _PT-U64!",
            "  256 VB-S _PT.S.PEER-MAX-PAY !",
            "  2 VB-FORMATS L! 1 VB-FORMATS 4 + L! 4 VB-FORMATS 20 + L!",
            "  64 _PT-RX-LEN ! VB-FORMATS _PT-RX-P ! ;",
            ": VB-TOTAL! ( n -- ) VB-FORMATS 56 + _PT-U64! ;",
            ": VB-VALID ( -- ) VB-S _PT-RET-FORMATS-VALID? . ;",
            ": VB-QUOTA ( points -- ) VB-S _PT-OO-S !",
            "  0 _PT-OO-REGIONS ! 0 _PT-OO-RESOURCES ! 0 _PT-OO-OBJECTS !",
            "  0 _PT-OO-SERIES ! 0 _PT-OO-RESOURCE-BYTES ! 0 _PT-OO-UTF8-BYTES !",
            "  0 _PT-OO-SAMPLE-SLOTS ! _PT-OO-PATH-POINTS !",
            "  _PT-OWNER-QUOTAS? . ;",
            ": VB-RUN VB-SETUP",
            '  S" VBFORMATS " TYPE',
            "  0 VB-TOTAL! VB-VALID 3 VB-TOTAL! VB-VALID 4 VB-TOTAL! VB-VALID",
            # Without VECTOR the budget and the line limit must both be zero.
            "  0x01 VB-S _PT.S.RET-CAPS 8 + _PT-U64! VB-VALID",
            "  0 VB-FORMATS 20 + L! VB-VALID 0 VB-TOTAL! VB-VALID",
            # The owner's share fits the budget the formats advertise.
            "  VB-FORMATS VB-S _PT.S.RET-FORMATS 64 MOVE",
            '  S" VBQUOTA " TYPE 0 VB-QUOTA 1 VB-QUOTA',
            "  6 VB-FORMATS 56 + _PT-U64!",
            "  VB-FORMATS VB-S _PT.S.RET-FORMATS 64 MOVE",
            "  6 VB-QUOTA 7 VB-QUOTA",
            "  DEPTH . TX-FLUSH ;",
            "VB-RUN BYE",
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
        self.assertEqual(position, len(program), "vector budget source was not fully fed")
        self.assertTrue(system.cpu.halted, "vector budget source-load watchdog exceeded")
        for diagnostic in (
            " ? (not found)", "Dictionary full", "Stack underflow",
            "Stack overflow", "*** BUS FAULT", "*** PRIVILEGE FAULT",
        ):
            self.assertNotIn(diagnostic, decoded)
        # VECTOR needs a positive budget at least one line's points; without
        # VECTOR the line limit and the budget are zero.
        self.assertRegex(raw, rb"VBFORMATS 0 0 -1 0 0 -1 ")
        # Before the budget is installed there is no room; then six points fit
        # and a seventh does not.
        match = re.search(rb"VBQUOTA (-?[0-9]+) (-?[0-9]+) (-?[0-9]+) (-?[0-9]+) (-?[0-9]+) ", raw)
        self.assertIsNotNone(match)
        self.assertEqual([int(value) for value in match.groups()], [-1, 0, -1, 0, 0])
