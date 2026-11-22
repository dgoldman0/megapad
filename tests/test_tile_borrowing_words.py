"""The BIOS words of borrowed tile work, on the emulator and the simulator.

TAMAC-SCALAR runs TAMAC's broadcast form and TAMAC-SCALARS runs it over
consecutive tiles with a vector of scalars. TACC-SEND stores what the
accumulator holds and TACC-RELAY moves a 256-byte image through it, each
into memory or a chiplet port's window, and both return a status for
every failure instead of trapping (docs/BIOS-DICTIONARY.md, "Borrowed tile
work"). The BIOS words run on the emulator's machine and the hosted
simulator's words on the simulator, over one memory map; each case gives
both the same Forth and requires the same reports, memory, tile registers
and chiplet memory. Tiles, scalars and images sit in Bank 0 at 0x40000,
which neither machine's dictionary or stacks reach. External memory's
transport failures, status 7, exist only on the emulator, where a case
injects them.
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
from megapad64 import ExternalPhyWordResponse
from system import HBW_SIZE, MegapadSystem, VRAM_DEFAULT_SIZE


EXT_SIZE = 0x4000
TILES = 0x40000          # sources, eight tiles
SCALARS = 0x41000        # a packed scalar vector
INITIAL = 0x41400        # an image TACC-LOAD starts from
STORED = 0x41800         # where TACC-STORE puts the result
SENT = 0x41C00           # where TACC-SEND and TACC-RELAY put their copy
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
        # The emulator's memory map: the checked image words route by it.
        memory = create_one_core_address_space(
            bank0_size=1 << 20, external_size=EXT_SIZE, hbw_size=HBW_SIZE,
            vram_size=VRAM_DEFAULT_SIZE, chiplet_ports=1)
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

    # Later words append after these; the chain from the newest entry still
    # reaches TACC-RELAY.  The system test pins which entry is newest.
    chain = set()
    address = labels["latest_entry"]
    while address and address not in chain:
        chain.add(address)
        address = int.from_bytes(code[address:address + 8], "little")
    assert labels["d_tacc_relay"] in chain
    assert link("d_tacc_relay") == labels["d_tacc_send"]
    assert link("d_tacc_send") == labels["d_tamac_scalars"]
    assert link("d_tamac_scalars") == labels["d_tamac_scalar"]
    assert link("d_tamac_scalar") == labels["d_boot_id"]
    # TAMAC-SCALAR takes x into R7 and runs the broadcast form, E5 06 07.
    start = labels["w_tamac_scalar"]
    expected = assemble("ldn r7, r14\naddi r14, 8\nt.amac r7\nret.l")
    assert code[start:start + len(expected)] == expected
    # The bus-fault handler resumes each checked word right after the
    # instruction that faulted.
    handler = labels["bus_fault_handler"]
    for site, instruction in ((".tacc_send_store_return", "t.acc.store"),
                              (".tacc_relay_load_return", "t.acc.load"),
                              (".tacc_relay_store_return", "t.acc.store")):
        resume = labels[site]
        encoded = assemble(instruction)
        assert code[resume - len(encoded):resume] == encoded
        assert resume.to_bytes(8, "little") in code[handler:handler + 200]


def test_simulator_words_follow_the_bios_order():
    runtime = MegaForthRuntime()
    names = [word.name for word in runtime.dictionary.words]
    first = names.index(b"TAMAC-SCALAR")
    assert tuple(names[first:first + 4]) == (
        b"TAMAC-SCALAR", b"TAMAC-SCALARS", b"TACC-SEND", b"TACC-RELAY")


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
#  TACC-SEND and TACC-RELAY
# ---------------------------------------------------------------------------

OK, NOT_OWNER, INVALID, FORMAT, MISALIGNED, NO_ROUTE, REFUSED, TRANSPORT = range(8)


@pytest.mark.parametrize("mode", [0x01, 0x04, 0x07], ids=["u16", "fp16", "fp64"])
def test_tacc_relay_moves_the_formats_image_between_memories(machines, mode):
    rng = random.Random(f"relay/{mode}")
    _write(machines, INITIAL, bytes(rng.getrandbits(8) for _ in range(IMAGE)))
    _write(machines, SENT, bytes([0xA5]) * IMAGE)
    reports = _both(machines, [
        FREE,
        f"TACC-CLAIM? DROP {mode} TMODE! TACC-CLEAR",
        f"{INITIAL} {SENT} TACC-RELAY REPORT DEPTH REPORT",
        "TACC-STATUS@ REPORT TACC-RELEASE",
    ])
    assert reports[:2] == [OK, 0]
    sent = _same_memory(machines, SENT)
    active = {1: 256, 4: 128, 7: 64}[mode & 0xF]
    initial = _same_memory(machines, INITIAL)
    assert sent == initial[:active] + bytes(IMAGE - active)
    emulator, simulator = machines
    assert simulator.registers() == emulator.registers() == (INITIAL, SENT)


def test_tacc_send_stores_what_tacc_holds(machines):
    rng = random.Random("send")
    _write(machines, TILES, bytes(rng.getrandbits(8) for _ in range(4 * TILE)))
    _write(machines, SCALARS, bytes([3, 5, 7, 9]))
    _write(machines, SENT, bytes([0xA5]) * IMAGE)
    reports = _both(machines, [
        FREE,
        f"TACC-CLAIM? DROP 0x10 TMODE! TACC-CLEAR {TILES} {SCALARS} 4 TAMAC-SCALARS",
        f"{STORED} TDST! TACC-STORE {TILES} TDST!",
        f"{SENT} TACC-SEND REPORT DEPTH REPORT TACC-STATUS@ REPORT",
        "TACC-RELEASE",
    ])
    # SEND stores what STORE stores and, like it, leaves TACC clean.
    assert reports == [OK, 0, 0b111 | (0 << 5) | (1 << 9)]
    assert _same_memory(machines, SENT) == _same_memory(machines, STORED)
    emulator, simulator = machines
    assert simulator.registers()[1] == emulator.registers()[1] == SENT


def test_every_failure_before_the_instructions_is_a_status_that_changes_nothing(
        machines):
    emulator, simulator = machines
    _write(machines, INITIAL, bytes(range(IMAGE)))
    _write(machines, SENT, bytes([0x5A]) * IMAGE)
    _both(machines, [FREE])
    window = WINDOW + LENT
    cases = [
        # Nobody owns TACC yet.
        (f"{SENT} TACC-SEND", NOT_OWNER),
        (f"{INITIAL} {SENT} TACC-RELAY", NOT_OWNER),
        ("TACC-CLAIM? DROP 0 TMODE!", None),
        # Owned, but holding no image.
        (f"{SENT} TACC-SEND", INVALID),
        # A TMODE that names no TACC format: U64 and a reserved code.
        (f"3 TMODE! {INITIAL} {SENT} TACC-RELAY 0 TMODE!", FORMAT),
        (f"0x4B TMODE! {INITIAL} {SENT} TACC-RELAY 0 TMODE!", FORMAT),
        (f"{INITIAL} TSRC0! TACC-LOAD", None),
        (f"{SENT + 8} TACC-SEND", MISALIGNED),
        (f"{INITIAL + 0x20} {SENT} TACC-RELAY", MISALIGNED),
        (f"{INITIAL} {SENT + 0x10} TACC-RELAY", MISALIGNED),
        # Alignment is checked before any route.
        (f"{INITIAL} {0x0020_0001} TACC-RELAY", MISALIGNED),
        # No route: past external memory, across Bank 0's end, into the
        # device window, past the end of the address space, a source in a
        # window, a store past its window or beyond the ports built.
        (f"{0x0020_0000} TACC-SEND", NO_ROUTE),
        (f"{(1 << 20) - 0xC0} TACC-SEND", NO_ROUTE),
        (f"0x{MMIO_BASE + 0x300:X} TACC-SEND", NO_ROUTE),
        ("0xFFFFFFFFFFFFFFC0 TACC-SEND", NO_ROUTE),
        (f"{window} {SENT} TACC-RELAY", NO_ROUTE),
        (f"{INITIAL} {WINDOW + (1 << 48) - 0x80} TACC-RELAY", NO_ROUTE),
        (f"{0x0020_0000} {SENT} TACC-RELAY", NO_ROUTE),
        (f"{INITIAL} {WINDOW + (2 << 48) + LENT} TACC-RELAY", NO_ROUTE),
    ]
    for line, expected in cases:
        if expected is None:
            _both(machines, [line])
            continue
        before = _both(machines, ["TACC-STATUS@ REPORT"])
        registers = emulator.registers()
        reports = _both(machines, [f"{line} REPORT DEPTH REPORT",
                                   "TACC-STATUS@ REPORT"])
        assert reports == [expected, 0, before[0]], line
        assert simulator.registers() == emulator.registers() == registers, line
        assert _same_memory(machines, SENT) == bytes([0x5A]) * IMAGE, line
    _both(machines, ["TACC-RELEASE"])


def test_the_checked_words_count_only_the_instructions_they_ran(machines):
    _write(machines, INITIAL, bytes(range(IMAGE)))
    reports = _both(machines, [
        FREE,
        "TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR PERF-TILEOPS",
        f"{SENT} TACC-SEND DROP {INITIAL} {SENT} TACC-RELAY DROP",
        f"{SENT + 1} TACC-SEND DROP {0x0020_0000} {SENT} TACC-RELAY DROP",
        "PERF-TILEOPS SWAP - REPORT TACC-RELEASE",
    ])
    # A store, then a load and a store; the two failures ran nothing.
    assert reports == [3]


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
def test_the_checked_words_store_into_a_chiplets_memory(linked, mode):
    rng = random.Random(f"window/{mode}")
    image = bytes(rng.getrandbits(8) for _ in range(IMAGE))
    _write(linked, INITIAL, image)
    active = 64 if mode == 0x07 else IMAGE
    expected = image[:active] + bytes(IMAGE - active)
    emulator, simulator = linked
    reports = _both(linked, [
        FREE,
        f"TACC-CLAIM? DROP {mode} TMODE! TACC-CLEAR",
        f"{INITIAL} {WINDOW + LENT} TACC-RELAY REPORT",
    ])
    assert reports == [OK]
    assert _lent(emulator) == _lent(simulator) == expected
    # TACC-SEND stores the same image, now in TACC, a unit further on.
    reports = _both(linked, [f"{WINDOW + LENT + 0x100} TACC-SEND REPORT",
                             "TACC-RELEASE"])
    assert reports == [OK]
    assert _lent(emulator, 64)[256:] == _lent(simulator, 64)[256:] == expected


def test_a_window_that_does_not_take_the_store_gives_status_6(linked):
    _write(linked, INITIAL, bytes(range(IMAGE)))
    _write(linked, STORED, bytes([0xC3]) * IMAGE)
    reports = _both(linked, [
        FREE,
        "TACC-CLAIM? DROP 0x10 TMODE! TACC-CLEAR",
        # Outside the 4 KiB the test chiplet lends: it refuses the write.
        f"{WINDOW + 0x8000} TACC-SEND REPORT",
        f"{INITIAL} {WINDOW + 0x8000} TACC-RELAY REPORT",
        # The relay loaded before its store failed: TACC holds that image,
        # valid and clean.
        f"TACC-STATUS@ REPORT {STORED} TACC-SEND REPORT",
        # A port closed to MegaPad's writes: its bridge refuses.
        f"{ENABLE | DOORBELL_EN} 0x80 CP!",
        f"{WINDOW + LENT} TACC-SEND REPORT {INITIAL} {WINDOW + LENT} TACC-RELAY REPORT",
        f"{ENABLE | DOORBELL_EN | BORROW_EN} 0x80 CP! DEPTH REPORT TACC-RELEASE",
    ])
    assert reports == [REFUSED, REFUSED, 0b111 | (1 << 9), OK, REFUSED,
                       REFUSED, 0]
    initial = _same_memory(linked, INITIAL)
    assert _same_memory(linked, STORED) == initial


def test_external_memory_failures_give_status_7_on_the_emulator(machines):
    """The simulator has no external-memory transport, so only the emulator,
    with an injected failure, reaches status 7."""

    emulator, simulator = machines
    external = EXTERNAL_BASE + 0x2000
    _write(machines, external, bytes(range(IMAGE)))
    _write(machines, INITIAL, bytes([0x11]) * IMAGE)
    _both(machines, [FREE, f"TACC-CLAIM? DROP 0 TMODE! {INITIAL} TSRC0! TACC-LOAD"])
    lines = [
        # The load fails: TACC keeps the image it held.
        f"{external} {SENT} TACC-RELAY REPORT TACC-STATUS@ REPORT",
        # The store fails, after the relay's load of a Bank 0 image.
        f"{INITIAL} {external} TACC-RELAY REPORT",
        f"{external + 0x100} TACC-SEND REPORT DEPTH REPORT",
    ]
    emulator.system.cpu.set_external_phy_response_plan(
        lambda request: ExternalPhyWordResponse(error=True))
    try:
        reports = emulator.run(lines)
    finally:
        emulator.system.cpu.set_external_phy_response_plan(None)
    assert reports == [TRANSPORT, 0b111, TRANSPORT, TRANSPORT, 0]
    # Without that failure the simulator's external memory takes them.
    assert simulator.run(lines) == [OK, 0b111, OK, OK, 0]
    _both(machines, ["TACC-RELEASE"])
