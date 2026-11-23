"""The BIOS's worker cores take signals, end jobs and set up their stacks.

A worker takes a job only from a full core. Any other signal, here the test
chiplet's doorbell, is a wake-up that it acknowledges, whether it sleeps or
runs a job when the signal comes, and it never runs its last job again.
Core 0's sleep acknowledges every pending sender, IDs above 7 included.
ABORT, ABORT" and QUIT on a worker end its job as a fault does: the worker
takes its own stacks back, clears its slot and idles, and core 0 keeps its
session and its stacks. Boot derives every full core's stacks from COREID
and System Info, so the BIOS comes up from the RTL's reset, every register
zero.

Every machine runs the real BIOS on the emulator, with one host lane for
every guest core, so their interleaving is the same on every run.
"""

from __future__ import annotations

import os
import re

import pytest

from asm import assemble
from system import EXT_MEM_BASE, MMIO_START, MegapadSystem


MBOX_SEND = MMIO_START + 0x508
WINDOW = MMIO_START + 0xE00
CTRL, STATUS = 0x80, 0x88
RANGE_SEL, RANGE_BASE, RANGE_LIMIT, RANGE_RIGHTS = 0xA0, 0xA8, 0xB0, 0xB8
BOOT_ADDR, BOOT_LEN = 0xD8, 0xE0
ENABLE, DOORBELL_EN, READY = 1 << 0, 1 << 3, 1 << 2
READ, WRITE = 1, 2
EXT_SIZE = 0x4000
IMAGE = EXT_MEM_BASE
COMMAND = EXT_MEM_BASE + 0x100
REPLY = EXT_MEM_BASE + 0x1000
SERVED = REPLY + 8          # the test chiplet's count of commands served
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
MASK64 = (1 << 64) - 1
MIB = 1 << 20
ZONE = 0x10000              # each full core's stack zone
DATA_BELOW_TOP = 0x8000     # a worker's data stack starts this far down

_REPORT = re.compile(r"<(-?\d+) >")
_BIOS: tuple[bytes, dict[str, int]] | None = None


def _bios() -> tuple[bytes, dict[str, int]]:
    global _BIOS
    if _BIOS is None:
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        labels: dict[str, int] = {}
        with open(os.path.join(root, "bios.asm"), encoding="utf-8") as source:
            code = assemble(source.read(), labels_out=labels)
        _BIOS = code, labels
    return _BIOS


class _Machine:
    """The BIOS on the emulator, driven through core 0's terminal."""

    def __init__(self, *, full_cores: int = 2, clusters: int = 0,
                 bank0: int = MIB, cleared_registers: bool = False,
                 chiplet: bool = True) -> None:
        self.system = MegapadSystem(
            ram_size=bank0, num_cores=full_cores, num_clusters=clusters,
            hbw_size=0, ext_mem_size=EXT_SIZE, vram_size=0, worker_count=1,
        )
        self.native = self.system._native_system
        self.port_id = self.system.first_chiplet_port_id
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        code, self.labels = _bios()
        self.system.load_binary(0, code)
        self.system.boot()
        if cleared_registers:
            # The RTL clears every register at reset (rtl/core/mp64_cpu.v).
            for cpu in self.system.cores[:full_cores]:
                for register in range(32):
                    cpu.regs[register] = 0
        self.run([
            ': REPORT ( x -- ) ." <" . ." >" ;',
            # Wait, within a bound, for a core to clear its job slot.
            ": SETTLE ( core -- ) 400000 0 DO DUP CORE-STATUS 0= IF"
            " LEAVE THEN LOOP DROP ;",
        ])
        if chiplet:
            self._bring_up_chiplet()
            self.run([
                f"{MBOX_SEND} CONSTANT MSEND",
                f"{SERVED} CONSTANT SERVED",
                f": RING ( -- ) {self.port_id} MSEND C! ;",
            ])

    @property
    def text(self) -> str:
        return bytes(self._sent).decode("latin-1")

    def run(self, lines: list[str]) -> tuple[list[int], str]:
        data = "".join(line + "\n" for line in lines).encode()
        start = len(self._sent)
        position = 0
        steps = 0
        cpu = self.system.cpu
        while steps < 200_000_000:
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
        assert position == len(data), "core 0 did not take every line"
        text = bytes(self._sent[start:]).decode("latin-1")
        return [int(value) for value in _REPORT.findall(text)], text

    def settle(self, batches: int = 60, steps: int = 20_000) -> None:
        """Run the machine on without input: the workers and the chiplet."""
        for _ in range(batches):
            self.system.run_batch(steps)

    def reports_since(self, start: int) -> list[int]:
        return [int(value) for value in _REPORT.findall(self.text[start:])]

    def served(self) -> int:
        return self.system._ext_mem[SERVED - EXT_MEM_BASE]

    def pending(self, core: int) -> int:
        return int(self.native.ipi_pending_mask(core))

    def slot(self, core: int) -> int:
        address = self.labels["worker_xt_table"] + 8 * core
        return int.from_bytes(
            bytes(self.system.cpu.mem[address:address + 8]), "little")

    def in_ipi_handler(self, core: int) -> bool:
        pc = self.system.cores[core].pc
        return self.labels["ipi_handler"] <= pc < self.labels["worker_xt_table"]

    def _put(self, address: int, *words: int) -> None:
        offset = address - EXT_MEM_BASE
        for index, word in enumerate(words):
            self.system._ext_mem[offset + 8 * index:offset + 8 * index + 8] = (
                (word & MASK64).to_bytes(8, "little"))

    def _bring_up_chiplet(self) -> None:
        """Release the test chiplet from the host, as test_lm1_chiplet does."""
        cpu = self.system.cpu
        self.system.attach_test_chiplet(0)
        # The probe address lies in a range lent for reading only.
        self._put(IMAGE, BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
        self._put(COMMAND, 0)
        for selector, base, limit, rights in (
            (0, IMAGE, IMAGE + 0x100, READ),
            (1, COMMAND, COMMAND + 0x900, READ),
            (2, REPLY, REPLY + 0x1000, READ | WRITE),
        ):
            cpu.mem_write64(WINDOW + RANGE_SEL, selector)
            cpu.mem_write64(WINDOW + RANGE_BASE, base)
            cpu.mem_write64(WINDOW + RANGE_LIMIT, limit)
            cpu.mem_write64(WINDOW + RANGE_RIGHTS, rights)
        cpu.mem_write64(WINDOW + BOOT_ADDR, IMAGE)
        cpu.mem_write64(WINDOW + BOOT_LEN, 40)
        cpu.mem_write64(WINDOW + CTRL, ENABLE | DOORBELL_EN)
        for _ in range(40):
            self.native._service_chiplet_ports()
            if cpu.mem_read64(WINDOW + STATUS) & READY:
                return
        raise AssertionError("the test chiplet never became ready")


def _zone_top(core: int, bank0: int = MIB) -> int:
    return min(bank0, MIB) - core * ZONE


# ---------------------------------------------------------------------------
#  Signals
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("full_cores", [2, 4])
def test_a_doorbell_wakes_a_sleeping_worker_and_its_job_stays_done(
        full_cores):
    # The port's ID is 2 with two full cores and 4 with four. The old
    # handler ran the last job again at each doorbell from ID 2 and never
    # acknowledged ID 4, taking the core straight back into the handler.
    machine = _Machine(full_cores=full_cores)
    reports, _ = machine.run([
        "VARIABLE RUNS  0 RUNS !",
        ": RING-ONCE ( -- ) 1 RUNS +! RING ;",
        "' RING-ONCE 1 WAKE-CORE 1 SETTLE",
    ])
    # The job rang and returned; the chiplet answers some rounds later,
    # while the worker sleeps, by ringing it back.
    machine.settle()
    reports, _ = machine.run([
        "RUNS @ REPORT 1 CORE-STATUS REPORT DEPTH REPORT",
    ])
    assert reports == [1, 0, 0]
    assert machine.served() == 1
    assert machine.pending(1) == 0
    worker = machine.system.cores[1]
    assert worker.idle and not machine.in_ipi_handler(1)

    # It still takes jobs from core 0.
    reports, _ = machine.run([
        "' RING-ONCE 1 WAKE-CORE 1 SETTLE",
    ])
    machine.settle()
    reports, _ = machine.run(["RUNS @ REPORT 1 CORE-STATUS REPORT"])
    assert reports == [2, 0]
    assert machine.served() == 2
    assert machine.pending(1) == 0


@pytest.mark.parametrize("full_cores", [2, 4])
def test_a_doorbell_during_a_job_leaves_the_job_running(full_cores):
    # The job keeps interrupts off until the chiplet's doorbell is pending,
    # so the handler runs inside the job when it turns them back on.
    machine = _Machine(full_cores=full_cores)
    bit = 1 << machine.port_id
    reports, _ = machine.run([
        "VARIABLE RUNS  0 RUNS !  VARIABLE DONE  0 DONE !",
        ": HOLD ( -- ) 1 RUNS +! DI! RING"
        f" BEGIN IPI-STATUS {bit} AND UNTIL EI! 1 DONE +! ;",
        "' HOLD 1 WAKE-CORE 1 SETTLE",
        "RUNS @ REPORT DONE @ REPORT 1 CORE-STATUS REPORT",
    ])
    assert reports == [1, 1, 0]
    machine.settle()
    reports, _ = machine.run(["RUNS @ REPORT DONE @ REPORT"])
    assert reports == [1, 1]
    assert machine.served() == 1
    assert machine.pending(1) == 0
    assert machine.system.cores[1].idle and not machine.in_ipi_handler(1)


def test_core_zero_acknowledges_a_doorbell_above_the_status_byte():
    # With two clusters the port's ID is 9, beyond the eight IDs
    # MBOX_STATUS shows. Core 0's sleep reads the whole mask, so the
    # doorbell cannot hold its later sleeps awake.
    machine = _Machine(full_cores=1, clusters=2)
    assert machine.port_id == 9
    machine.run([
        "VARIABLE WAITS  0 WAITS !",
        # Ring the chiplet, then sleep until its doorbell wakes core 0.
        ": RING-AND-WAIT ( -- ) RING BEGIN 1 WAITS +! 100000 IDLE-MS"
        " SERVED C@ UNTIL ;",
    ])
    start = len(machine.text)
    # run() hands back the machine once core 0 sleeps inside IDLE-MS.
    machine.run(["RING-AND-WAIT WAITS @ REPORT"])
    machine.settle()
    assert machine.reports_since(start) == [1]
    assert machine.served() == 1
    assert machine.pending(0) == 0
    # Core 0 sleeps at the prompt again, as run() relies on to go on.
    reports, _ = machine.run(["2 3 + REPORT"])
    assert reports == [5]
    assert machine.system.cpu.idle


# ---------------------------------------------------------------------------
#  Aborts
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    "ending",
    ["ABORT", 'TRUE ABORT" worker gave up"', "QUIT"],
    ids=["abort", "abort-quote", "quit"],
)
def test_an_abort_on_a_worker_ends_its_job_and_core_zero_keeps_its_stacks(
        ending):
    machine = _Machine(full_cores=2, chiplet=False)
    reports, _ = machine.run([
        "CREATE MARK 1 ALLOT  0 MARK C!",
        f": ENDER ( -- ) 7 MARK C! 1 2 3 {ending} 99 MARK C! ;",
        ": GOOD ( -- ) 66 MARK C! ;",
        "RP@ REPORT",
    ])
    (prompt_rp,) = reports

    # Core 0 holds three values on its data stack across the worker's job.
    reports, text = machine.run([
        "11 22 33 ' ENDER 1 WAKE-CORE 1 SETTLE",
        "1 CORE-STATUS REPORT MARK C@ REPORT DEPTH REPORT RP@ REPORT",
    ])
    assert reports == [0, 7, 3, prompt_rp]
    if "ABORT\"" in ending:
        assert "worker gave up" in text

    worker = machine.system.cores[1]
    machine.settle(batches=5)
    assert worker.idle
    assert worker.regs[15] == _zone_top(1)
    assert worker.regs[14] == _zone_top(1) - DATA_BELOW_TOP

    # The worker takes the next job, and core 0's values are intact.
    reports, _ = machine.run([
        "' GOOD 1 WAKE-CORE 1 SETTLE",
        "MARK C@ REPORT 1 CORE-STATUS REPORT REPORT REPORT REPORT"
        " DEPTH REPORT",
    ])
    assert reports == [66, 0, 33, 22, 11, 0]


# ---------------------------------------------------------------------------
#  Stacks
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    ("full_cores", "bank0"),
    [(1, MIB), (2, MIB), (4, MIB), (2, MIB // 2), (2, 2 * MIB)],
    ids=["1-core", "2-core", "4-core", "2-core-512k", "2-core-2m"],
)
def test_the_bios_boots_every_full_core_from_cleared_registers(
        full_cores, bank0):
    machine = _Machine(full_cores=full_cores, bank0=bank0,
                       cleared_registers=True, chiplet=False)
    top = _zone_top(0, bank0)
    core0 = machine.system.cpu
    # At the prompt core 0's data stack is empty, at half its zone top.
    assert core0.regs[2] == top
    assert core0.regs[14] == top // 2
    for core in range(1, full_cores):
        worker = machine.system.cores[core]
        assert worker.idle
        assert worker.regs[2] == _zone_top(core, bank0)
        assert worker.regs[15] == _zone_top(core, bank0)
        assert worker.regs[14] == _zone_top(core, bank0) - DATA_BELOW_TOP

    # Each worker runs a job on those stacks.
    lines = [
        "CREATE MARKS 4 ALLOT  MARKS 4 0 FILL",
        ": MARKED ( -- ) COREID 10 + MARKS COREID + C! ;",
    ]
    for core in range(1, full_cores):
        lines.append(f"' MARKED {core} WAKE-CORE {core} SETTLE")
    lines.append("MARKS 1+ C@ REPORT MARKS 2 + C@ REPORT MARKS 3 + C@ REPORT"
                 " DEPTH REPORT")
    reports, _ = machine.run(lines)
    marks = [10 + core if core < full_cores else 0 for core in (1, 2, 3)]
    assert reports == marks + [0]
