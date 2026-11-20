"""The BIOS words of borrowed tile work, on the emulator and the simulator.

TAMAC-SCALAR runs TAMAC's broadcast form, TAMAC-SCALARS runs it over
consecutive tiles with a vector of scalars, and TACC-SEND moves a 256-byte
image through the accumulator into memory or a chiplet port's window,
answering with a status where the store takes a bus fault
(docs/BIOS-DICTIONARY.md, "Borrowed tile work"). The BIOS words run on the
emulator's machine and the hosted simulator's words on the simulator; each
case gives both the same Forth and requires the same reports, memory,
tile registers and chiplet memory. Tiles, scalars and images sit in Bank 0
at 0x40000, which neither machine's dictionary or stacks reach.
"""

from __future__ import annotations

import os
import random
import re

import pytest

import _megaforth_native  # noqa: F401  (the simulator's TACC is its code)
from asm import assemble
from shared import ieee_fp
from simulator.errors import ForthAbort
from simulator.memory import EXTERNAL_BASE, MMIO_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.tacc import TACCBusFault
from system import MegapadSystem


EXT_SIZE = 0x4000
TILES = 0x40000          # sources, eight tiles
SCALARS = 0x41000        # a packed scalar vector
INITIAL = 0x41400        # an image TACC-LOAD starts from
STORED = 0x41800         # where TACC-STORE puts the result
SENT = 0x41C00           # where TACC-SEND puts its copy
IMAGE = 256
TILE = 64
OWNER_NONE = 31

WINDOW = 0x0100_0000_0000_0000
LENT = 0x1000            # the test chiplet lends its 0x1000..0x1FFF
PORT_WINDOW = MMIO_BASE + 0xE00
MAILBOX = MMIO_BASE + 0x500
PORT_ID = 1              # the simulator's; the emulator's is read below
BOOT_IMAGE = EXTERNAL_BASE
COMMAND = EXTERNAL_BASE + 0x100
REPLY = EXTERNAL_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
COPY_FROM_LENT = 0x100
READY = 1 << 2
ENABLE, DOORBELL_EN, BORROW_EN = 1, 1 << 3, 1 << 5

FORMATS = (
    pytest.param(0x00, id="u8"),
    pytest.param(0x10, id="s8"),
    pytest.param(0x01, id="u16"),
    pytest.param(0x11, id="s16"),
    pytest.param(0x02, id="u32"),
    pytest.param(0x12, id="s32"),
    pytest.param(0x04, id="fp16"),
    pytest.param(0x05, id="bf16"),
    pytest.param(0x06, id="fp32"),
    pytest.param(0x07, id="fp64"),
)
LANE_BYTES = {0: 1, 1: 2, 2: 4, 4: 2, 5: 2, 6: 4, 7: 8}

_REPORT = re.compile(r"<(-?\d+) >")
_DEFINITIONS = [
    ': REPORT ( x -- ) ." <" . ." >" ;',
    "VARIABLE KEPT",
    ": KEEP ( code -- ) KEPT ! ;",
]


def _reports(text: str) -> list[int]:
    return [int(value) for value in _REPORT.findall(text)]


_BIOS_CODE: bytes | None = None


def _bios_code() -> bytes:
    global _BIOS_CODE
    if _BIOS_CODE is None:
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        with open(os.path.join(root, "bios.asm"), encoding="utf-8") as source:
            _BIOS_CODE = assemble(source.read())
    return _BIOS_CODE


class _Emulator:
    """The emulator's machine running the BIOS, with the test chiplet."""

    def __init__(self) -> None:
        self.system = MegapadSystem(ram_size=1 << 20, ext_mem_size=EXT_SIZE)
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        self.system.load_binary(0, _bios_code())
        self.system.boot()
        self.system.attach_test_chiplet(0)
        self.port_id = self.system.first_chiplet_port_id
        self.run(_DEFINITIONS)

    def run(self, lines: list[str]) -> list[int]:
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
        return _reports(bytes(self._sent[start:]).decode("latin-1"))

    def write(self, address: int, data: bytes) -> None:
        self.system.load_binary(address, data)

    def read(self, address: int, length: int) -> bytes:
        if EXTERNAL_BASE <= address < EXTERNAL_BASE + EXT_SIZE:
            offset = address - EXTERNAL_BASE
            return bytes(self.system._ext_mem[offset:offset + length])
        return bytes(self.system.cpu.mem[address:address + length])

    def registers(self) -> tuple[int, int]:
        cpu = self.system.cpu
        return cpu.tsrc0, cpu.tdst


class _Simulator:
    """The hosted simulator, with the test chiplet behind one port."""

    def __init__(self) -> None:
        memory = create_one_core_address_space(
            bank0_size=1 << 20, external_size=EXT_SIZE, chiplet_ports=1)
        self.runtime = MegaForthRuntime(memory=memory)
        self.runtime.chiplet_ports.attach_test_chiplet(0)
        self.port_id = PORT_ID
        self.run(_DEFINITIONS)

    def run(self, lines: list[str]) -> list[int]:
        # The BIOS abandons a line that faults and reads the next one.
        for line in lines:
            try:
                self.runtime.evaluate(line.encode(), step_budget=50_000_000)
            except ForthAbort:
                pass
        return _reports(self.runtime.drain_uart_output().decode("latin-1"))

    def write(self, address: int, data: bytes) -> None:
        self.runtime.memory.write_bytes(address, data)

    def read(self, address: int, length: int) -> bytes:
        return self.runtime.memory.read_bytes(address, length)

    def registers(self) -> tuple[int, int]:
        tile = self.runtime.tile
        return tile.source0, tile.destination


@pytest.fixture(scope="module")
def machines():
    return _Emulator(), _Simulator()


def _both(machines, lines: list[str]) -> list[int]:
    emulator, simulator = machines
    reports = emulator.run(lines)
    assert simulator.run(lines) == reports
    return reports


# Each case starts with the accumulator free and TMODE zero, whatever an
# earlier case left.
FREE = "TACC-CLAIM? DROP TACC-RELEASE 0 TMODE! 0 FAULT-XT!"


def _write(machines, address: int, data: bytes) -> None:
    for machine in machines:
        machine.write(address, data)


def _same_memory(machines, address: int, length: int = IMAGE) -> bytes:
    emulator, simulator = machines
    stored = emulator.read(address, length)
    assert simulator.read(address, length) == stored
    return stored


def _lanes(rng: random.Random, mode: int, count: int) -> bytes:
    """COUNT lanes of MODE's format, floats including the special values."""

    ew = mode & 0xF
    width = LANE_BYTES[ew]
    if ew <= 3:
        return bytes(rng.getrandbits(8) for _ in range(count * width))
    fmt = ieee_fp.FORMAT_BY_EW[ew]
    special = (0, fmt.sign_bit, fmt.infinity, fmt.canonical_nan, 1)
    lanes = []
    for _ in range(count):
        if rng.random() < 0.2:
            value = rng.choice(special)
        else:
            value = ieee_fp.from_double(fmt, rng.uniform(-3.0, 3.0))
        lanes.append(value.to_bytes(width, "little"))
    return b"".join(lanes)


# ---------------------------------------------------------------------------
#  The dictionary
# ---------------------------------------------------------------------------

def test_the_words_append_after_boot_id():
    labels: dict[str, int] = {}
    with open(os.path.join(os.path.dirname(os.path.dirname(
            os.path.abspath(__file__))), "bios.asm"), encoding="utf-8") as source:
        code = bytes(assemble(source.read(), labels_out=labels))

    def link(entry: str) -> int:
        return int.from_bytes(code[labels[entry]:labels[entry] + 8], "little")

    assert labels["latest_entry"] == labels["d_tacc_send"]
    assert link("d_tacc_send") == labels["d_tamac_scalars"]
    assert link("d_tamac_scalars") == labels["d_tamac_scalar"]
    assert link("d_tamac_scalar") == labels["d_boot_id"]
    # TAMAC-SCALAR takes x into R7 and runs the broadcast form, E5 06 07.
    start = labels["w_tamac_scalar"]
    expected = assemble("ldn r7, r14\naddi r14, 8\nt.amac r7\nret.l")
    assert code[start:start + len(expected)] == expected
    # The bus-fault handler resumes TACC-SEND right after its store.
    store = assemble("t.acc.store")
    resume = labels[".tacc_send_store_return"]
    assert code[resume - len(store):resume] == store
    handler = labels["bus_fault_handler"]
    assert resume.to_bytes(8, "little") in code[handler:handler + 160]


def test_simulator_words_follow_the_bios_order():
    runtime = MegaForthRuntime()
    assert tuple(word.name for word in runtime.dictionary.words[-3:]) == (
        b"TAMAC-SCALAR", b"TAMAC-SCALARS", b"TACC-SEND")


# ---------------------------------------------------------------------------
#  TAMAC-SCALAR and TAMAC-SCALARS
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("mode", FORMATS)
def test_tamac_scalar_and_scalars_agree_in_every_format(machines, mode):
    rng = random.Random(f"borrowed/{mode}")
    ew = mode & 0xF
    tiles = 5
    _write(machines, INITIAL, bytes(rng.getrandbits(8) for _ in range(IMAGE)))
    _write(machines, TILES, _lanes(rng, mode, tiles * TILE // LANE_BYTES[ew]))
    _write(machines, SCALARS, _lanes(rng, mode, tiles))
    poisoned = int.from_bytes(_lanes(rng, mode, 8 // LANE_BYTES[ew]), "little")
    reports = _both(machines, [
        FREE,
        f"TACC-CLAIM? REPORT {mode} TMODE! {INITIAL} TSRC0! TACC-LOAD",
        f"{TILES} TSRC0! 0x{poisoned:X} TAMAC-SCALAR",
        f"{TILES} {SCALARS} {tiles} TAMAC-SCALARS",
        "TACC-STATUS@ REPORT",
        f"{STORED} TDST! TACC-STORE TACC-RELEASE TACC-STATUS@ REPORT",
    ])
    assert reports[0] == -1
    assert reports[2] == OWNER_NONE << 16
    _same_memory(machines, STORED)
    emulator, simulator = machines
    # TSRC0 was left at the last tile.
    assert simulator.registers() == emulator.registers()
    assert emulator.registers()[0] == TILES + TILE * (tiles - 1)


def test_tamac_scalars_over_no_tiles_does_nothing(machines):
    emulator, simulator = machines
    reports = _both(machines, [
        FREE,
        f"{SENT} TSRC0! TACC-STATUS@ REPORT",
        f"{TILES} {SCALARS} 0 TAMAC-SCALARS TACC-STATUS@ REPORT DEPTH REPORT",
    ])
    # No tile, so not even the ownership check runs.
    assert reports == [OWNER_NONE << 16, OWNER_NONE << 16, 0]
    assert simulator.registers()[0] == emulator.registers()[0] == SENT


def test_tamac_scalars_faults_as_its_tamac_does(machines):
    emulator, simulator = machines
    reports = _both(machines, [
        FREE,
        "0 KEPT ! ' KEEP FAULT-XT!",
        "TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR",
        # A format that does not match the latched one is illegal.
        f"0x10 TMODE! {TILES} {SCALARS} 2 TAMAC-SCALARS",
        "KEPT @ REPORT  0 KEPT !  0 TMODE!",
        # A misaligned tile is an alignment fault at its address.
        f"{TILES + 8} {SCALARS} 3 TAMAC-SCALARS",
        "KEPT @ REPORT  0 FAULT-XT!",
    ])
    assert reports == [-21, -23]
    assert simulator.registers()[0] == emulator.registers()[0] == TILES + 8


def test_a_bus_fault_stops_tamac_scalars_at_its_tile(machines):
    emulator, simulator = machines
    # Two tiles end external memory; the third has no route.
    tiles = EXTERNAL_BASE + EXT_SIZE - 2 * TILE
    _write(machines, tiles, bytes(range(2 * TILE)))
    _write(machines, SCALARS, bytes([3, 5, 7, 9]))
    _both(machines, [FREE, "TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR"])
    line = f"{tiles} {SCALARS} 4 TAMAC-SCALARS"
    # The BIOS reports the bus fault and returns to its prompt; the
    # simulator fails the dispatch.
    emulator.run([line])
    with pytest.raises(TACCBusFault) as fault:
        simulator.runtime.evaluate(line.encode())
    assert fault.value.address == tiles + 2 * TILE
    assert simulator.registers()[0] == emulator.registers()[0] == tiles + 2 * TILE
    # The two tiles before it accumulated on both.
    reports = _both(machines, [
        f"TACC-STATUS@ REPORT {STORED} TDST! TACC-STORE TACC-RELEASE"])
    assert reports == [0b1111]
    _same_memory(machines, STORED)


# ---------------------------------------------------------------------------
#  TACC-SEND
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("mode", [0x01, 0x04, 0x07], ids=["u16", "fp16", "fp64"])
def test_tacc_send_moves_the_formats_image_between_memories(machines, mode):
    rng = random.Random(f"send/{mode}")
    _write(machines, INITIAL, bytes(rng.getrandbits(8) for _ in range(IMAGE)))
    _write(machines, SENT, bytes([0xA5]) * IMAGE)
    reports = _both(machines, [
        FREE,
        f"TACC-CLAIM? DROP {mode} TMODE! TACC-CLEAR",
        f"{INITIAL} {SENT} TACC-SEND REPORT DEPTH REPORT",
        "TACC-STATUS@ REPORT TACC-RELEASE",
    ])
    assert reports[:2] == [0, 0]
    sent = _same_memory(machines, SENT)
    active = {1: 256, 4: 128, 7: 64}[mode & 0xF]
    initial = _same_memory(machines, INITIAL)
    assert sent == initial[:active] + bytes(IMAGE - active)
    emulator, simulator = machines
    assert simulator.registers() == emulator.registers() == (INITIAL, SENT)


def _bring_up(machines, ctrl: int) -> None:
    """The test chiplet running behind port 0, CTRL as given."""

    words = (BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
    for machine in machines:
        machine.write(BOOT_IMAGE, b"".join(w.to_bytes(8, "little") for w in words))
    for machine in machines:
        machine.run([
            f"0x{PORT_WINDOW:X} CONSTANT CP  0x{MAILBOX:X} CONSTANT MBOX",
            ": CP! ( x offset -- ) CP + ! ;  : CP@ ( offset -- x ) CP + @ ;",
            ": LEND ( base limit rights slot -- ) "
            "0xA0 CP!  ROT 0xA8 CP!  SWAP 0xB0 CP!  0xB8 CP! ;",
            f"{BOOT_IMAGE} {BOOT_IMAGE + 0x100} 1 0 LEND",
            f"{BOOT_IMAGE} 0xD8 CP!  40 0xE0 CP!",
            f"{COMMAND} {COMMAND + 0x900} 1 1 LEND",
            f"{REPLY} {REPLY + 0x1000} 3 2 LEND",
            f"{ctrl} 0x80 CP!",
            f": WAIT-READY BEGIN 0x88 CP@ {READY} AND UNTIL ; WAIT-READY",
            f": RING {machine.port_id} MBOX 8 + C! ;",
            f": WAIT-ANSWER BEGIN MBOX 9 + C@ {1 << machine.port_id} AND UNTIL ;",
            f": ACK {machine.port_id} MBOX 0x0A + C! ;",
        ])


def _lent(machine, words: int = 32) -> bytes:
    """The start of the test chiplet's lent memory, by its copy command."""

    machine.write(COMMAND, (COPY_FROM_LENT | words).to_bytes(8, "little"))
    machine.run(["RING WAIT-ANSWER ACK"])
    return machine.read(REPLY + 24, 8 * words)


@pytest.fixture(scope="module")
def linked():
    pair = (_Emulator(), _Simulator())
    _bring_up(pair, ENABLE | DOORBELL_EN | BORROW_EN)
    return pair


@pytest.mark.parametrize("mode", [0x00, 0x07], ids=["u8-four-units", "fp64-one-unit"])
def test_tacc_send_stores_into_a_chiplets_memory(linked, mode):
    rng = random.Random(f"window/{mode}")
    image = bytes(rng.getrandbits(8) for _ in range(IMAGE))
    _write(linked, INITIAL, image)
    reports = _both(linked, [
        FREE,
        f"TACC-CLAIM? DROP {mode} TMODE! TACC-CLEAR",
        f"{INITIAL} {WINDOW + LENT} TACC-SEND REPORT",
        "TACC-STATUS@ REPORT TACC-RELEASE",
    ])
    assert reports[0] == 0
    active = 64 if mode == 0x07 else IMAGE
    expected = image[:active] + bytes(IMAGE - active)
    emulator, simulator = linked
    assert _lent(emulator) == _lent(simulator) == expected


def test_a_store_the_window_does_not_take_returns_status_6(linked):
    emulator, simulator = linked
    _write(linked, INITIAL, bytes(range(IMAGE)))
    reports = _both(linked, [
        FREE,
        "TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR",
        # Past the ports built: the store has no route.
        f"{INITIAL} {WINDOW + (2 << 48) + LENT} TACC-SEND REPORT",
        # Outside the 4 KiB the test chiplet lends: it refuses the write.
        f"{INITIAL} {WINDOW + 0x8000} TACC-SEND REPORT",
        # Close the port to MegaPad's writes: the bridge refuses.
        f"{ENABLE | DOORBELL_EN} 0x80 CP!",
        f"{INITIAL} {WINDOW + LENT} TACC-SEND REPORT DEPTH REPORT",
        "TACC-STATUS@ REPORT",
        f"{ENABLE | DOORBELL_EN | BORROW_EN} 0x80 CP! TACC-RELEASE",
    ])
    # The accumulator keeps the loaded image, valid and clean.
    assert reports[:4] == [6, 6, 6, 0]
    assert reports[4] == 0b111


def test_send_faults_other_than_the_store_stay_faults(linked):
    reports = _both(linked, [
        FREE,
        "0 KEPT ! ' KEEP FAULT-XT!",
        f"{INITIAL} {SENT} TACC-SEND REPORT",
        "KEPT @ REPORT  0 KEPT !",
        f"TACC-CLAIM? DROP 0 TMODE! {INITIAL} {SENT + 8} TACC-SEND REPORT",
        "KEPT @ REPORT  0 KEPT !",
        f"{INITIAL + 0x20} {SENT} TACC-SEND REPORT",
        "KEPT @ REPORT  0 FAULT-XT! TACC-RELEASE",
    ])
    # The unowned load is illegal, and a misaligned store or load image an
    # alignment fault; none reports a status.
    assert reports == [-21, -23, -23]
