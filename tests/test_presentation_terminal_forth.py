"""Focused source-load test for the optional APT-1 guest module.

The presentation client is intentionally not part of KDOS.  This test loads
the production source into userland, then exercises only the first ownership
boundary: inert load, caller-owned initialization, probe publication, and
pre-OPEN cancellation back to ANSI.
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
