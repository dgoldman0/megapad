"""The full-width TACC words in the hosted simulator, against the emulator.

The simulator's TACC is native code (simulator/accel/tacc_service.h), so
the native extension must be built before this gate. Each case runs the
same TACC instructions on the Python emulator, the architectural oracle,
and the matching words on the simulator, over one memory map: Bank 0,
external memory, HBW and VRAM at the same addresses. They must leave the
same accumulator image, TACC_STATUS and memory, and raise the same fault at
the same address. Stores into a chiplet port's window go through one port
model on each side, and the chiplet records what it was sent.
"""

from __future__ import annotations

import random

import pytest

import _megaforth_native  # noqa: F401  (the simulator's TACC is its code)
from asm import assemble
from emulator.megapad64 import (
    CSR_TMODE,
    IVEC_ALIGN_FAULT,
    IVEC_BUS_FAULT,
    IVEC_ILLEGAL_OP,
    Megapad64 as PythonMegapad64,
    TrapError,
)
from shared import ieee_fp
from simulator.errors import AlignmentFault, ForthAbort, IllegalInstructionFault
from simulator.memory import EXTERNAL_BASE, HBW_BASE, MMIO_BASE, VRAM_BASE
from simulator.platform import create_address_space
from simulator.runtime import MegaForthRuntime
from simulator.tacc import TACCBusFault


BANK0 = 1 << 20
EXT_SIZE = 0x4000
HBW_SIZE = 0x1000
VRAM_SIZE = 0x1000
CODE = 0x100
DATA = 0x40000
IMAGE = 256
TILE = 64
OWNER_NONE = 31
WINDOW = 0x0100_0000_0000_0000
WINDOW_BYTES = 1 << 48
MASK64 = (1 << 64) - 1

CTRL = 0x80
ENABLE = 1 << 0
BORROW_EN = 1 << 5
OK, REFUSED, MEM_ERROR, CANCELLED = range(4)

INSTRUCTIONS = {
    "tacc_try": "t.acc.try",
    "tacc_clear": "t.acc.clear",
    "tacc_load": "t.acc.load",
    "tacc_store": "t.acc.store",
    "tacc_release": "t.acc.release",
    "tamac": "t.amac",
    "tamac_scalar": "t.amac r7",
}

# Every TACC format: integer formats in both signednesses, the float ones.
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


class Pair:
    """The emulator's oracle and the simulator, over one memory map."""

    def __init__(self, *, ports: int = 0) -> None:
        cpu = PythonMegapad64(mem_size=BANK0)
        cpu.attach_ext_mem(bytearray(EXT_SIZE), EXTERNAL_BASE, EXT_SIZE)
        cpu.attach_hbw(bytearray(HBW_SIZE), HBW_BASE, HBW_SIZE)
        cpu.attach_vram(bytearray(VRAM_SIZE), VRAM_BASE, VRAM_SIZE)
        self.cpu = cpu
        memory = create_address_space(
            bank0_size=BANK0,
            external_size=EXT_SIZE,
            hbw_size=HBW_SIZE,
            vram_size=VRAM_SIZE,
            chiplet_ports=ports,
        )
        self.runtime = MegaForthRuntime(memory=memory)
        self.tile = self.runtime.tile
        self.tacc = self.tile.tacc
        self.oracle_link = None
        self.oracle_chiplet = None
        self.hosted_chiplet = None

    # -- the chiplet port each side stores into ---------------------------

    def attach_chiplets(self, *, ctrl: int = ENABLE | BORROW_EN,
                        answer: int = OK) -> None:
        """A manual chiplet behind port 0 on each side, answering ANSWER."""

        native = _megaforth_native
        self.oracle_link = native.ChipletLink(
            bytearray(EXT_SIZE), EXTERNAL_BASE, core_count=1, port_count=1,
        )
        self.oracle_chiplet = native.ManualChiplet()
        self.oracle_link.attach_manual_chiplet(0, self.oracle_chiplet)
        self.oracle_link.write64(CTRL, ctrl)
        link = self.oracle_link

        def lent_write(port, chiplet_addr, data, zeros):
            return link.lent_write(
                WINDOW + port * WINDOW_BYTES + chiplet_addr, data, zeros)

        self.cpu._tacc_window_route_hook = link.route
        self.cpu._tacc_link_write_hook = lent_write

        self.hosted_chiplet = native.ManualChiplet()
        hosted_link = self.runtime.chiplet_ports.link
        hosted_link.attach_manual_chiplet(0, self.hosted_chiplet)
        hosted_link.write64(CTRL, ctrl)
        self.set_answer(answer)

    def set_answer(self, answer: int) -> None:
        self.oracle_chiplet.lent_code = answer
        self.hosted_chiplet.lent_code = answer

    # -- memory --------------------------------------------------------------

    def write(self, address: int, data: bytes) -> None:
        self.cpu.load_bytes(address, data)
        self.runtime.memory.write_bytes(address, data)

    def read(self, address: int, length: int) -> tuple[bytes, bytes]:
        oracle = bytearray()
        for index in range(length):
            buffer, offset, _ = self.cpu._resolve_addr(address + index)
            oracle.append(buffer[offset])
        return bytes(oracle), self.runtime.memory.read_bytes(address, length)

    # -- one instruction on each side ------------------------------------------

    def run(self, word: str, *, mode: int = 0, source0: int = 0,
            source1: int = 0, destination: int = 0, scalar: int = 0):
        """WORD on both sides; each side's fault as (kind, address) or None."""

        cpu = self.cpu
        cpu.csr_write(CSR_TMODE, mode)
        cpu.tsrc0, cpu.tsrc1, cpu.tdst = source0, source1, destination
        cpu.regs[7] = scalar & MASK64
        cpu.trap_addr = 0
        cpu.load_bytes(CODE, assemble(INSTRUCTIONS[word]))
        cpu._icache_invalidate_all(reset_statistics=False)
        cpu.pc = CODE
        oracle = None
        try:
            cpu.step()
        except TrapError as trap:
            kind = {IVEC_ILLEGAL_OP: "illegal", IVEC_ALIGN_FAULT: "alignment",
                    IVEC_BUS_FAULT: "bus"}[trap.ivec_id]
            oracle = (kind, None if kind == "illegal" else cpu.trap_addr)

        tile = self.tile
        tile.set_mode(mode)
        tile.set_source0(source0)
        tile.set_source1(source1)
        tile.set_destination(destination)
        hosted = None
        try:
            if word == "tamac_scalar":
                tile.tamac_scalar(scalar)
            else:
                getattr(tile, word)()
        except IllegalInstructionFault:
            hosted = ("illegal", None)
        except AlignmentFault as fault:
            hosted = ("alignment", fault.address)
        except TACCBusFault as fault:
            hosted = ("bus", fault.address)
        return oracle, hosted

    def same(self, word: str, **registers):
        """Run WORD and require both sides to agree; return the fault."""

        oracle, hosted = self.run(word, **registers)
        assert hosted == oracle, word
        self.assert_same_state()
        return oracle

    def assert_same_state(self) -> None:
        assert self.tacc.bank == bytes(self.cpu.tacc)
        assert self.tacc.status == self.cpu._tacc_status()


def _owned(pair: Pair, mode: int, image: bytes | None = None) -> None:
    """Claim the accumulator on both sides and give it MODE's format."""

    assert pair.same("tacc_try") is None
    if image is None:
        assert pair.same("tacc_clear", mode=mode) is None
    else:
        pair.write(DATA + 0x2000, image)
        assert pair.same("tacc_load", mode=mode, source0=DATA + 0x2000) is None


def _lanes(rng: random.Random, mode: int, count: int = 64) -> bytes:
    """COUNT bytes of lanes for MODE: random integers, or float lanes that
    include zeros, subnormals, infinities and NaNs."""

    ew = mode & 0xF
    if ew <= 3:
        return bytes(rng.getrandbits(8) for _ in range(count))
    fmt = ieee_fp.FORMAT_BY_EW[ew]
    width = fmt.width // 8
    special = (0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
               fmt.canonical_nan, 1, fmt.sign_bit | 1)
    lanes = []
    for _ in range(count // width):
        pick = rng.random()
        if pick < 0.15:
            value = rng.choice(special)
        elif pick < 0.5:
            value = ieee_fp.from_double(fmt, rng.uniform(-4.0, 4.0))
        else:
            value = rng.getrandbits(fmt.width)
        lanes.append(value.to_bytes(width, "little"))
    return b"".join(lanes)


# ---------------------------------------------------------------------------
#  The lifecycle and every format's arithmetic
# ---------------------------------------------------------------------------

def test_the_lifecycle_moves_through_the_same_status_words():
    pair = Pair()
    assert pair.tacc.status == OWNER_NONE << 16
    pair.same("tacc_try")
    assert pair.tacc.status == 0b11
    pair.same("tacc_try")  # a second claim by the owner changes nothing
    pair.same("tacc_clear", mode=0x11)
    pair.write(DATA, bytes(range(64)))
    pair.same("tamac", mode=0x11, source0=DATA, source1=DATA)
    pair.same("tacc_store", destination=DATA + 0x400)
    oracle, hosted = pair.read(DATA + 0x400, IMAGE)
    assert hosted == oracle
    pair.same("tacc_release")
    assert pair.tacc.status == OWNER_NONE << 16


@pytest.mark.parametrize("mode", FORMATS)
def test_tamac_in_every_format_matches_the_oracle(mode):
    rng = random.Random(f"tacc/{mode}")
    pair = Pair()
    # Start from a loaded image, which LOAD trims to the format.
    _owned(pair, mode, image=bytes(rng.getrandbits(8) for _ in range(IMAGE)))
    for step in range(6):
        a = DATA + 0x100 * step
        b = a + 0x40
        pair.write(a, _lanes(rng, mode))
        pair.write(b, _lanes(rng, mode))
        pair.same("tamac", mode=mode, source0=a, source1=b)
        # The broadcast form takes only the scalar's low lane-width bits.
        pair.same("tamac_scalar", mode=mode, source0=b,
                  scalar=int.from_bytes(_lanes(rng, mode, 8), "little"))
    # TMODE's saturation and rounding bits are not part of the format.
    pair.same("tamac", mode=mode | 0x60, source0=DATA, source1=DATA + 0x40)
    pair.write(DATA + 0x800, bytes([0xA5]) * IMAGE)
    pair.same("tacc_store", destination=DATA + 0x800)
    oracle, hosted = pair.read(DATA + 0x800, IMAGE)
    assert hosted == oracle


@pytest.mark.parametrize("mode", FORMATS)
def test_load_keeps_only_the_formats_active_bytes(mode):
    pair = Pair()
    _owned(pair, mode, image=bytes(range(IMAGE)))
    pair.same("tacc_store", destination=DATA + 0x400)
    oracle, hosted = pair.read(DATA + 0x400, IMAGE)
    assert hosted == oracle


@pytest.mark.parametrize(
    "where",
    [pytest.param(EXTERNAL_BASE + 0x1000, id="external"),
     pytest.param(HBW_BASE + 0x400, id="hbw"),
     pytest.param(VRAM_BASE + 0x400, id="vram")],
)
def test_images_and_sources_in_every_memory(where):
    rng = random.Random(f"memory/{where}")
    pair = Pair()
    pair.write(where, bytes(rng.getrandbits(8) for _ in range(IMAGE)))
    _owned(pair, 0x01)
    pair.same("tacc_load", mode=0x01, source0=where)
    pair.same("tamac", mode=0x01, source0=where, source1=where + 0x40)
    pair.same("tacc_store", destination=where + 0x100)
    oracle, hosted = pair.read(where + 0x100, IMAGE)
    assert hosted == oracle


# ---------------------------------------------------------------------------
#  Faults
# ---------------------------------------------------------------------------

def test_every_protected_operation_needs_the_owner():
    pair = Pair()
    for word in ("tacc_clear", "tacc_load", "tacc_store", "tacc_release",
                 "tamac", "tamac_scalar"):
        assert pair.same(word, source0=DATA, source1=DATA,
                         destination=DATA) == ("illegal", None)


def test_store_and_tamac_need_a_valid_accumulator():
    pair = Pair()
    pair.same("tacc_try")
    for word in ("tacc_store", "tamac", "tamac_scalar"):
        assert pair.same(word, source0=DATA, source1=DATA,
                         destination=DATA) == ("illegal", None)


@pytest.mark.parametrize("mode", [0x03, 0x13, 0x08, 0x0F, 0x4B])
def test_formats_that_are_not_tacc_formats_trap(mode):
    pair = Pair()
    pair.same("tacc_try")
    assert pair.same("tacc_clear", mode=mode) == ("illegal", None)
    assert pair.same("tacc_load", mode=mode, source0=DATA) == ("illegal", None)
    _owned(pair, 0x00)
    assert pair.same("tamac", mode=mode, source0=DATA,
                     source1=DATA) == ("illegal", None)


@pytest.mark.parametrize(
    ("latched", "mode"),
    [pytest.param(0x00, 0x10, id="signedness"),
     pytest.param(0x00, 0x01, id="width"),
     pytest.param(0x05, 0x04, id="bf16-fp16"),
     pytest.param(0x06, 0x07, id="fp32-fp64")],
)
def test_tamac_must_match_the_latched_format(latched, mode):
    pair = Pair()
    _owned(pair, latched)
    assert pair.same("tamac", mode=mode, source0=DATA,
                     source1=DATA) == ("illegal", None)
    assert pair.same("tamac_scalar", mode=mode, source0=DATA,
                     scalar=3) == ("illegal", None)


def test_signedness_is_no_part_of_a_float_format():
    pair = Pair()
    _owned(pair, 0x04)
    assert pair.same("tamac", mode=0x14, source0=DATA, source1=DATA) is None


@pytest.mark.parametrize(
    ("word", "registers", "fault"),
    [
        pytest.param("tacc_load", {"source0": DATA + 0x20},
                     ("alignment", DATA + 0x20), id="load-misaligned"),
        pytest.param("tacc_store", {"destination": DATA + 0x08},
                     ("alignment", DATA + 0x08), id="store-misaligned"),
        pytest.param("tamac", {"source0": DATA + 1, "source1": DATA + 2},
                     ("alignment", DATA + 1), id="tamac-first-misaligned"),
        pytest.param("tamac", {"source0": DATA, "source1": DATA + 0x30},
                     ("alignment", DATA + 0x30), id="tamac-second-misaligned"),
        pytest.param("tamac", {"source0": 0x0020_0000, "source1": DATA + 4},
                     ("alignment", DATA + 4),
                     id="tamac-alignment-before-routing"),
        pytest.param("tacc_load", {"source0": BANK0 - 0xC0},
                     ("bus", BANK0), id="load-crosses-bank0"),
        pytest.param("tacc_store",
                     {"destination": EXTERNAL_BASE + EXT_SIZE - 0x40},
                     ("bus", EXTERNAL_BASE + EXT_SIZE), id="store-crosses-external"),
        pytest.param("tacc_store", {"destination": 0x0020_0000},
                     ("bus", 0x0020_0000), id="store-unrouted"),
        pytest.param("tacc_load", {"source0": MMIO_BASE + 0x300},
                     ("bus", MMIO_BASE + 0x300), id="load-from-mmio"),
        pytest.param("tacc_store", {"destination": MMIO_BASE - 0x40},
                     ("bus", MMIO_BASE), id="store-into-mmio"),
        pytest.param("tacc_load", {"source0": MASK64 + 1 - 0x80},
                     ("bus", 0), id="load-wraps"),
        pytest.param("tamac", {"source0": DATA, "source1": BANK0 + 0x200000},
                     ("bus", BANK0 + 0x200000), id="tamac-source-unrouted"),
        pytest.param("tamac_scalar", {"source0": MMIO_BASE},
                     ("bus", MMIO_BASE), id="broadcast-source-mmio"),
        pytest.param("tacc_store", {"destination": WINDOW + 0x1000},
                     ("bus", WINDOW + 0x1000), id="store-without-ports"),
    ],
)
def test_faults_match_the_oracle_and_change_nothing(word, registers, fault):
    pair = Pair()
    _owned(pair, 0x00, image=bytes(range(IMAGE)))
    pair.same("tamac", source0=DATA, source1=DATA)  # dirty
    before = pair.tacc.bank, pair.tacc.status
    assert pair.same(word, **registers) == fault
    assert (pair.tacc.bank, pair.tacc.status) == before


def test_a_fault_counts_no_tile_operation():
    pair = Pair()
    diagnostics = pair.runtime.diagnostics
    start = diagnostics.perf_tileops
    _owned(pair, 0x00)
    assert diagnostics.perf_tileops == start + 2
    pair.same("tacc_store", destination=DATA + 1)
    pair.same("tacc_clear", mode=0x03)
    assert diagnostics.perf_tileops == start + 2
    pair.same("tamac", source0=DATA, source1=DATA)
    assert diagnostics.perf_tileops == start + 3


# ---------------------------------------------------------------------------
#  Stores into memory a chiplet lends
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    ("mode", "units", "zeros"),
    [pytest.param(0x00, 4, 0, id="u8-four-units"),
     pytest.param(0x02, 2, 2, id="u32-two-units"),
     pytest.param(0x04, 2, 2, id="fp16-two-units"),
     pytest.param(0x07, 1, 3, id="fp64-one-unit")],
)
def test_a_window_store_sends_the_formats_units(mode, units, zeros):
    pair = Pair(ports=1)
    pair.attach_chiplets()
    _owned(pair, mode, image=bytes((7 * i + 1) & 0xFF for i in range(IMAGE)))
    pair.same("tamac", mode=mode, source0=DATA, source1=DATA + 0x40)
    assert pair.same("tacc_store", destination=WINDOW + 0x1000) is None
    assert pair.hosted_chiplet.lent_writes == pair.oracle_chiplet.lent_writes
    (address, data, sent_zeros), = pair.hosted_chiplet.lent_writes
    assert (address, len(data), sent_zeros) == (0x1000, units * TILE, zeros)
    assert data == pair.tacc.bank[:units * TILE]
    assert not pair.tacc.dirty


@pytest.mark.parametrize(
    "answer", [REFUSED, MEM_ERROR, CANCELLED],
    ids=["refused", "memory-error", "cancelled"],
)
def test_an_answer_but_ok_is_a_bus_fault_at_the_image(answer):
    pair = Pair(ports=1)
    pair.attach_chiplets(answer=answer)
    _owned(pair, 0x00)
    pair.same("tamac", source0=DATA, source1=DATA)
    before = pair.tacc.bank, pair.tacc.status
    assert pair.same("tacc_store", destination=WINDOW + 0x1040) == (
        "bus", WINDOW + 0x1040)
    assert (pair.tacc.bank, pair.tacc.status) == before
    assert pair.tacc.dirty
    assert pair.hosted_chiplet.lent_writes == pair.oracle_chiplet.lent_writes


def test_a_port_closed_to_borrowing_refuses_without_the_chiplet():
    pair = Pair(ports=1)
    pair.attach_chiplets(ctrl=ENABLE)
    _owned(pair, 0x00)
    assert pair.same("tacc_store", destination=WINDOW) == ("bus", WINDOW)
    assert pair.hosted_chiplet.lent_writes == []
    assert pair.oracle_chiplet.lent_writes == []


@pytest.mark.parametrize(
    ("word", "registers", "fault"),
    [
        pytest.param("tacc_store",
                     {"destination": WINDOW + WINDOW_BYTES - 0xC0},
                     ("bus", WINDOW + WINDOW_BYTES), id="store-past-the-window"),
        pytest.param("tacc_store",
                     {"destination": WINDOW + WINDOW_BYTES + 0x1000},
                     ("bus", WINDOW + WINDOW_BYTES + 0x1000),
                     id="store-beyond-the-ports"),
        pytest.param("tacc_load", {"source0": WINDOW + 0x1000},
                     ("bus", WINDOW + 0x1000), id="load-from-a-window"),
        pytest.param("tamac", {"source0": WINDOW + 0x1000, "source1": DATA},
                     ("bus", WINDOW + 0x1000), id="tamac-source-in-a-window"),
        pytest.param("tacc_store", {"destination": WINDOW + 0x1010},
                     ("alignment", WINDOW + 0x1010), id="misaligned-window-store"),
    ],
)
def test_the_windows_take_aligned_store_images_alone(word, registers, fault):
    pair = Pair(ports=1)
    pair.attach_chiplets()
    _owned(pair, 0x00)
    assert pair.same(word, **registers) == fault
    assert pair.hosted_chiplet.lent_writes == []
    assert pair.oracle_chiplet.lent_writes == []


# ---------------------------------------------------------------------------
#  The words, from Forth
# ---------------------------------------------------------------------------

def _forth(runtime: MegaForthRuntime, source: str) -> bytes:
    runtime.evaluate(source.encode())
    return runtime.uart_output


def test_the_words_run_a_two_step_kernel_from_forth():
    runtime = MegaForthRuntime()
    memory = runtime.memory
    a0, b0, a1, b1, destination = (DATA + offset for offset in
                                   (0, 0x40, 0x80, 0xC0, 0x400))
    for address, value in ((a0, 2), (b0, 3), (a1, 4), (b1, 5)):
        memory.write_bytes(address, bytes([value]) * TILE)
    text = _forth(runtime, f"""
        : U8-2MAC ( a0 b0 a1 b1 dst -- flag )
          TACC-CLAIM? 0= IF 2DROP 2DROP DROP FALSE EXIT THEN
          >R 0 TMODE! TACC-CLEAR
          TSRC1! TSRC0! TAMAC TSRC1! TSRC0! TAMAC
          R> TDST! TACC-STORE TACC-RELEASE TRUE ;
        TACC-STATUS@ .
        {a0} {b0} {a1} {b1} {destination} U8-2MAC .
        TACC-STATUS@ . DEPTH .
    """)
    assert text.split() == [str(OWNER_NONE << 16).encode(), b"-1",
                            str(OWNER_NONE << 16).encode(), b"0"]
    image = memory.read_bytes(destination, IMAGE)
    assert [int.from_bytes(image[i:i + 4], "little")
            for i in range(0, IMAGE, 4)] == [26] * 64


def test_tacc_faults_reach_the_fault_callback_with_their_codes():
    runtime = MegaForthRuntime()
    runtime.evaluate(b"VARIABLE KEPT : KEEP ( code -- ) KEPT ! ; ' KEEP FAULT-XT!")
    # The callback returns, so the runtime reports the fault and aborts.
    with pytest.raises(ForthAbort):
        runtime.evaluate(b"TACC-CLEAR")
    runtime.evaluate(b"KEPT @ .")
    with pytest.raises(ForthAbort):
        runtime.evaluate(f"TACC-CLAIM? DROP {DATA + 8} TSRC0! TACC-LOAD".encode())
    runtime.evaluate(b"KEPT @ .")
    text = runtime.uart_output.decode()
    lines = text.replace("\r", "").split("\n")
    assert "*** ILLEGAL INSTRUCTION CORE=00" in lines
    assert f"*** ALIGNMENT FAULT @ {DATA + 8:016X} CORE=00" in lines
    assert text.index("-21 ") < text.index("-23 ")


def test_a_bus_fault_fails_the_dispatch_at_the_reported_address():
    runtime = MegaForthRuntime()
    runtime.evaluate(b"TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR")
    runtime.tile.set_destination(MMIO_BASE)
    with pytest.raises(TACCBusFault) as fault:
        runtime.evaluate(b"TACC-STORE")
    assert fault.value.address == MMIO_BASE
    assert fault.value.length == IMAGE
