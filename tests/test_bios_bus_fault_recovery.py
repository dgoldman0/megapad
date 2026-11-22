"""The BIOS recovers from a bus fault as it recovers from an instruction fault.

After its report, a bus fault on core 0 returns to the prompt through
ABORT: both stacks are reset, STATE is back to interpreting, and the
callback installed with QUIT-XT! runs, so KDOS can drop the exception
frames that lived on the abandoned return stack. A worker core ends its
job and idles. The checked words that recognize their own bus faults are
covered with them; these cases fault through an ordinary load from an
address outside the memory map or past every device in the MMIO aperture.
"""

from __future__ import annotations

import os
import re

from asm import assemble
from system import MegapadSystem


UNMAPPED = 0x0020_0000   # past Bank 0, with no external memory
_REPORT = re.compile(r"<(-?\d+) >")
_BIOS_CODE: bytes | None = None


def _bios_code() -> bytes:
    global _BIOS_CODE
    if _BIOS_CODE is None:
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        with open(os.path.join(root, "bios.asm"), encoding="utf-8") as source:
            _BIOS_CODE = assemble(source.read())
    return _BIOS_CODE


class _Machine:
    """The emulator's machine running the BIOS on NUM_CORES full cores."""

    def __init__(self, num_cores: int = 1) -> None:
        # One host lane runs every guest core, so their interleaving is the
        # same on every run.
        self.system = MegapadSystem(
            ram_size=1 << 20, num_cores=num_cores, worker_count=1,
        )
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        self.system.load_binary(0, _bios_code())
        self.system.boot()
        self.run([': REPORT ( x -- ) ." <" . ." >" ;'])

    def run(self, lines: list[str]) -> tuple[list[int], str]:
        data = "".join(line + "\n" for line in lines).encode()
        start = len(self._sent)
        position = 0
        steps = 0
        while steps < 200_000_000:
            cpu = self.system.cpu
            if cpu.halted:
                break
            if cpu.idle and not self.system.uart.has_rx_data:
                if position >= len(data):
                    break
                end = data.find(b"\n", position) + 1 or len(data)
                self.system.uart.inject_input(data[position:end])
                position = end
                continue
            steps += max(self.system.run_batch(100_000), 1)
        assert position == len(data), "the emulator did not take every line"
        text = bytes(self._sent[start:]).decode("latin-1")
        return [int(value) for value in _REPORT.findall(text)], text


def test_core_zero_returns_to_the_prompt_through_abort():
    machine = _Machine()
    reports, _ = machine.run([
        "VARIABLE QUITS  0 QUITS !",
        ": COUNTED ( -- ) 1 QUITS +! ;  ' COUNTED QUIT-XT!",
        f": BOOM ( -- ) {UNMAPPED} @ DROP ; IMMEDIATE",
        "RP@ REPORT",
    ])
    (prompt_rp,) = reports

    reports, text = machine.run([
        f"1 2 3 {UNMAPPED} @",
        "DEPTH REPORT QUITS @ REPORT RP@ REPORT",
    ])
    assert f"*** BUS FAULT @ {UNMAPPED:08X}" in text
    # The data stack is empty, the QUIT-XT! callback ran once, and the
    # return stack is where the prompt keeps it.
    assert reports == [0, 1, prompt_rp]

    # A fault while compiling leaves the machine interpreting.
    reports, _ = machine.run([
        ": HALF 1 2 BOOM ;",
        "STATE @ REPORT QUITS @ REPORT 5 7 + REPORT",
    ])
    assert reports == [0, 2, 12]

    # Repeated faults neither grow the return stack nor lose definitions.
    reports, text = machine.run([
        f"{UNMAPPED} @",
        f"{UNMAPPED} @",
        f"{UNMAPPED} @",
        "RP@ REPORT QUITS @ REPORT DEPTH REPORT 21 2 * REPORT",
    ])
    assert text.count("*** BUS FAULT @") == 3
    assert reports == [prompt_rp, 5, 0, 42]


def test_an_access_past_the_mmio_devices_faults_and_recovers():
    # The address lies 2^32 bytes past the mailbox, beyond every device's
    # window: a guest bus fault, like any other outside the memory map.
    machine = _Machine()
    reports, text = machine.run([
        "HEX FFFFFF0100000500 DECIMAL CONSTANT PAST-MAILBOX",
        "1 2 PAST-MAILBOX C@",
        "DEPTH REPORT 5 7 + REPORT",
    ])
    assert "*** BUS FAULT @ 00000500" in text
    assert reports == [0, 12]


def test_a_worker_ends_its_job_and_stays_usable():
    machine = _Machine(num_cores=2)
    reports, text = machine.run([
        "CREATE MARK 1 ALLOT  0 MARK C!",
        f": BUSBAD ( -- ) {UNMAPPED} @ DROP 99 MARK C! ;",
        ": GOOD ( -- ) 66 MARK C! ;",
        # Wait, within a bound, for core 1 to clear its job slot.
        ": SETTLE ( -- ) 200000 0 DO 1 CORE-STATUS 0= IF LEAVE THEN LOOP ;",
        "' BUSBAD 1 WAKE-CORE SETTLE",
        "1 CORE-STATUS REPORT MARK C@ REPORT",
        "' GOOD 1 WAKE-CORE SETTLE",
        "MARK C@ REPORT 1 CORE-STATUS REPORT DEPTH REPORT",
    ])
    assert f"*** BUS FAULT @ {UNMAPPED:08X}" in text
    # No restart: the banner printed once, at boot, before these lines.
    assert "Megapad-64 Forth BIOS" not in text
    # The job stopped at its fault, the core went idle, and it takes the
    # next job; core 0 kept its session.
    assert reports == [0, 0, 66, 0, 0]
    for _ in range(50_000):
        if machine.system.cores[1].idle:
            break
        machine.system.step()
    assert machine.system.cores[1].idle
