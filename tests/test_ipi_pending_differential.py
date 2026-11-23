"""IPI-PENDING and CHIP-WAIT see a chiplet port above ID 7 on both machines.

IPI-STATUS reads the 8-bit MBOX_STATUS, which shows the senders with IDs 0
to 7, while CSR MBOX holds a core's whole 64-bit pending mask (finding 8 of
docs/multicore-simulator-plan.md). On the standard die the full cores and
the microcores take IDs 0 to 15, so LM1's port has ID 16, and a wait that
reads MBOX_STATUS never sees its signal. IPI-PENDING reads the whole mask:
the BIOS's word from CSR MBOX, the simulator's from its native mailbox,
which steps the chiplets first, as it does before a read of MBOX_STATUS, so
a core that polls for a port's signal makes progress.

The simulator runs no microcores, so both machines here give one full core
sixteen chiplet ports and put the test chiplet on the last, whose ID is 16
as LM1's is on the standard die. The test chiplet serves one command for
each doorbell and rings back the participant that rang
(shared/accel/chiplet_test_module.cpp). The same Forth brings it up, rings
it and polls for its answer on the emulator and on both of the simulator's
executors, and each must print the expected text: first with the BIOS's
words alone, then with KDOS and chiplet.f loaded, through CHIP-RING and
CHIP-WAIT.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from asm import assemble
from simulator.platform import Topology, create_address_space
from simulator.runtime import MegaForthRuntime
from system import EXT_MEM_BASE, MMIO_START, MegapadSystem

ROOT = Path(__file__).resolve().parents[1]
PORTS = 16
PORT = PORTS - 1
PORT_ID = 1 + PORT              # the IDs of the ports follow the one core's
EXT_SIZE = 0x4000
WINDOW = MMIO_START + 0xE00
MBOX = MMIO_START + 0x500
IMAGE = EXT_MEM_BASE
COMMAND = EXT_MEM_BASE + 0x100
REPLY = EXT_MEM_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
ENABLE, DOORBELL_EN, READY = 1 << 0, 1 << 3, 1 << 2
EXECUTORS = ("native", "python")

# The chiplet's boot image and an empty command, the port selected, its
# ranges lent and the chiplet released; then a ring and a poll of the whole
# mask, which ends once the chiplet has answered.
BRING_UP = f"""
{WINDOW} CONSTANT CP
{MBOX} CONSTANT MBOX
: CP! ( x offset -- ) CP + ! ;
: CP@ ( offset -- x ) CP + @ ;
: LEND ( base limit rights slot -- )
    0xA0 CP!  ROT 0xA8 CP!  SWAP 0xB0 CP!  0xB8 CP! ;
: BRING-UP ( -- )
    {BOOT_MAGIC} {IMAGE} !  {COMMAND} {IMAGE + 8} !  {REPLY} {IMAGE + 16} !
    {COMMAND} {IMAGE + 24} !  0 {IMAGE + 32} !  0 {COMMAND} !
    {PORT} CP 8 + !
    {IMAGE} {IMAGE + 0x100} 1 0 LEND
    {COMMAND} {COMMAND + 0x900} 1 1 LEND
    {REPLY} {REPLY + 0x1000} 3 2 LEND
    {IMAGE} 0xD8 CP!  40 0xE0 CP!
    {ENABLE | DOORBELL_EN} 0x80 CP!
    BEGIN 0x88 CP@ {READY} AND UNTIL ;
: SERVED ( -- n ) {REPLY + 8} C@ ;
: RING ( -- ) {PORT_ID} MBOX 8 + C! ;
: ASK ( -- ) RING BEGIN IPI-PENDING {1 << PORT_ID} AND UNTIL ;
""".strip().splitlines()

# The answer shows in the whole mask and not in MBOX_STATUS, and goes when
# core 0 acknowledges it.
BIOS_CASE = [
    "BRING-UP",
    f"60 EMIT 60 EMIT ASK IPI-PENDING . IPI-STATUS . SERVED ."
    f" {PORT_ID} IPI-ACK IPI-PENDING . 62 EMIT 62 EMIT",
]
BIOS_EXPECTED = f"{1 << PORT_ID} 0 1 0 ".encode()

# chiplet.f's CHIP-WAIT finds the port's signal, the port's ID from System
# Info, and acknowledges it.
CHIPLET_CASE = [
    "BRING-UP",
    f"60 EMIT 60 EMIT {PORT} CHIP-PRESENT? . {PORT} CHIP-ID ."
    f" {PORT} CHIP-RING {PORT} CHIP-WAIT SERVED . IPI-PENDING . 62 EMIT 62 EMIT",
]
CHIPLET_EXPECTED = f"-1 {PORT_ID} 1 0 ".encode()


def _source_lines(path: Path) -> list[bytes]:
    return [
        line for line in path.read_bytes().splitlines()
        if line.strip() and not line.strip().startswith(b"\\")
    ]


def _between(output: bytes) -> bytes:
    start = output.rfind(b"<<")
    end = output.find(b">>", start)
    assert start >= 0 and end > start, output[-400:]
    return output[start + 2:end]


class _Emulator:
    """The BIOS on the emulator's one full core, typed at its prompt."""

    def __init__(self, *, kdos: bool) -> None:
        with open(ROOT / "bios.asm", encoding="utf-8") as source:
            bios = assemble(source.read())
        self.system = MegapadSystem(
            ram_size=1 << 20, num_cores=1, num_clusters=0,
            ext_mem_size=EXT_SIZE, worker_count=1, chiplet_ports=PORTS,
        )
        assert self.system.first_chiplet_port_id + PORT == PORT_ID
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        self.system.load_binary(0, bios)
        self.system.boot()
        self.system.attach_test_chiplet(PORT)
        if kdos:
            self.type(_source_lines(ROOT / "kdos.f")
                      + _source_lines(ROOT / "chiplet.f"))

    def type(self, lines: list[bytes], *, step_limit: int = 600_000_000) -> bytes:
        data = b"".join(line + b"\n" for line in lines)
        start = len(self._sent)
        position = 0
        steps = 0
        uart = self.system.uart
        cpu = self.system.cpu
        while steps < step_limit and not cpu.halted:
            if cpu.idle and not uart.has_rx_data:
                if position >= len(data):
                    break
                end = data.find(b"\n", position) + 1 or len(data)
                uart.inject_input(data[position:end])
                position = end
                continue
            steps += max(self.system.run_batch(100_000), 1)
        assert position == len(data), "the emulator did not take every line"
        return bytes(self._sent[start:])

    def run(self, lines: list[str]) -> bytes:
        self.type([line.encode() for line in BRING_UP])
        # A wait that never sees its signal fails at the limit.
        return _between(self.type([line.encode() for line in lines],
                                  step_limit=20_000_000))


class _Simulator:
    """The simulator's one full core with the same sixteen ports."""

    def __init__(self, executor: str, *, kdos: bool) -> None:
        memory = create_address_space(
            topology=Topology(full_cores=1), bank0_size=1 << 20,
            external_size=EXT_SIZE, chiplet_ports=PORTS,
        )
        self.runtime = MegaForthRuntime(memory=memory, execution_backend=executor)
        assert self.runtime.chiplet_ports.first_port_id + PORT == PORT_ID
        self.runtime.chiplet_ports.attach_test_chiplet(PORT)
        if kdos:
            self.runtime.evaluate((ROOT / "kdos.f").read_bytes(), source_name="kdos.f")
            self.runtime.evaluate((ROOT / "chiplet.f").read_bytes(),
                                  source_name="chiplet.f")
        self.runtime.drain_uart_output()

    def run(self, lines: list[str]) -> bytes:
        # A wait that never sees its signal fails at the budget.
        self.runtime.evaluate("\n".join(BRING_UP).encode())
        for line in lines:
            self.runtime.evaluate(line.encode(), step_budget=20_000_000)
        return _between(self.runtime.drain_uart_output())


def test_ipi_pending_sees_a_port_above_the_status_byte_on_the_emulator():
    assert _Emulator(kdos=False).run(BIOS_CASE) == BIOS_EXPECTED


@pytest.mark.parametrize("executor", EXECUTORS)
def test_ipi_pending_sees_a_port_above_the_status_byte_on_the_simulator(executor):
    assert _Simulator(executor, kdos=False).run(BIOS_CASE) == BIOS_EXPECTED


def test_chip_wait_takes_a_port_above_the_status_byte_on_the_emulator():
    assert _Emulator(kdos=True).run(CHIPLET_CASE) == CHIPLET_EXPECTED


@pytest.mark.parametrize("executor", EXECUTORS)
def test_chip_wait_takes_a_port_above_the_status_byte_on_the_simulator(executor):
    assert _Simulator(executor, kdos=True).run(CHIPLET_CASE) == CHIPLET_EXPECTED
