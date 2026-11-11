"""Chiplet ports in the hosted one-core profile.

The ports are the native model the emulator uses, so the native extension
must be built before this gate. Forth source brings the test chiplet up in
the order docs/chiplet-link.md gives, hands it a command through lent
memory, and polls the mailbox for its answer.
"""

from __future__ import annotations

import pytest

import _megaforth_native  # noqa: F401  (the ports are its model)
import simulator.chiplet
from shared.memory_backing import DenseMemoryBacking
from simulator.chiplet import (
    CHIPLET_OFFSET,
    MAILBOX_ACK,
    MAILBOX_OFFSET,
    MAILBOX_SEND,
    MAILBOX_STATUS,
    HostedChipletPorts,
)
from simulator.memory import EXTERNAL_BASE, MMIO_BASE, MMIOAccessError
from simulator.platform import (
    SYSINFO_CHIPLET_PORTS,
    create_one_core_address_space,
)
from simulator.runtime import MegaForthRuntime


EXT_SIZE = 0x4000
WINDOW = MMIO_BASE + CHIPLET_OFFSET
PORT_COUNT = 0x00
CTRL = 0x80
STATUS = 0x88
FAULT_ADDR = 0x90
BOOT_LEN = 0xE0
RESET_HOLD = 1 << 1
READY = 1 << 2
FAULT = 1 << 4
PORT_ID = 1
PORT_WORD = 1 | (PORT_ID << 8) | (8 << 16)

IMAGE = EXTERNAL_BASE
COMMAND = EXTERNAL_BASE + 0x100
REPLY = EXTERNAL_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
MASK64 = (1 << 64) - 1
COMMAND_WORD = 0x0000_5EED_0000_0002


def _memory(*, dense=False, ports=1, external=EXT_SIZE):
    backing = None
    if dense:
        backing = DenseMemoryBacking(
            [(0, 1 << 20), (EXTERNAL_BASE, external)])
    return create_one_core_address_space(
        bank0_size=1 << 20,
        external_size=external,
        dense_backing=backing,
        chiplet_ports=ports,
    )


def _put(memory, addr, *words):
    memory.write_bytes(
        addr, b"".join((word & MASK64).to_bytes(8, "little") for word in words))


BRING_UP = f"""
{WINDOW} CONSTANT CP
{MMIO_BASE + MAILBOX_OFFSET} CONSTANT MBOX
: CP! ( x offset -- ) CP + ! ;
: CP@ ( offset -- x ) CP + @ ;
: LEND ( base limit rights slot -- )
    0xA0 CP!  ROT 0xA8 CP!  SWAP 0xB0 CP!  0xB8 CP! ;
: IDENTITY ( index -- x ) 0xC8 CP!  0xD0 CP@ ;
: WAIT-READY ( -- ) BEGIN 0x88 CP@ {READY} AND UNTIL ;
: RING ( -- ) {PORT_ID} MBOX 8 + C! ;
: WAIT-ANSWER ( -- ) BEGIN MBOX 9 + C@ {1 << PORT_ID} AND UNTIL ;
: ACK ( -- ) {PORT_ID} MBOX 0x0A + C! ;
: BRING-UP ( -- id )
    0 IDENTITY
    {IMAGE} {IMAGE + 0x100} 1 0 LEND
    {IMAGE} 0xD8 CP!  40 0xE0 CP!
    {COMMAND} {COMMAND + 0x900} 1 1 LEND
    {REPLY} {REPLY + 0x1000} 3 2 LEND
    9 0x80 CP!
    WAIT-READY ;
: ASK ( -- reply ) RING WAIT-ANSWER ACK {REPLY} @ ;
""".encode()


def _prepared(**options):
    memory = _memory(**options)
    _put(memory, IMAGE, BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
    _put(memory, COMMAND, COMMAND_WORD, 0xAAA, 0xBBB)
    runtime = MegaForthRuntime(memory=memory)
    runtime.chiplet_ports.attach_test_chiplet(0)
    runtime.evaluate(BRING_UP)
    return memory, runtime


def _run(runtime, word):
    context = runtime.new_context()
    runtime.execute(word, context=context)
    return context.data.snapshot()


# ---------------------------------------------------------------------------
#  The profile's ports, window and System Info
# ---------------------------------------------------------------------------

def test_the_profile_has_no_ports_unless_asked():
    memory = create_one_core_address_space(external_size=EXT_SIZE)
    assert memory.read64(MMIO_BASE + SYSINFO_CHIPLET_PORTS) == 0
    assert MegaForthRuntime(memory=memory).chiplet_ports is None
    with pytest.raises(MMIOAccessError):
        memory.read64(WINDOW + PORT_COUNT)
    with pytest.raises(MMIOAccessError):
        memory.read8(MMIO_BASE + MAILBOX_STATUS)


def test_system_info_repeats_the_windows_port_count():
    memory = _memory()
    assert memory.read64(MMIO_BASE + SYSINFO_CHIPLET_PORTS) == PORT_WORD
    assert memory.read64(WINDOW + PORT_COUNT) == PORT_WORD
    assert memory.read64(WINDOW + CTRL) == RESET_HOLD
    two = _memory(ports=2)
    assert two.read64(WINDOW + PORT_COUNT) == 2 | (PORT_ID << 8) | (8 << 16)


def test_the_window_holds_what_a_64_bit_write_gives_it():
    memory = _memory()
    memory.write64(WINDOW + BOOT_LEN, 0x0123_4567_89AB_CDEF)
    assert memory.read64(WINDOW + BOOT_LEN) == 0x0123_4567_89AB_CDEF


@pytest.mark.parametrize(
    ("read", "offset"),
    [("read8", 0x80), ("read16", 0x80), ("read32", 0x80), ("read64", 0x84),
     ("read64", 0xFC)],
)
def test_the_window_refuses_every_other_shape(read, offset):
    memory = _memory()
    with pytest.raises(MMIOAccessError):
        getattr(memory, read)(WINDOW + offset)
    with pytest.raises(MMIOAccessError):
        memory.write32(WINDOW + CTRL, 0)
    assert memory.read64(WINDOW + CTRL) == RESET_HOLD


def test_ports_need_the_native_extension(monkeypatch):
    def absent():
        raise RuntimeError("chiplet ports require a current _megaforth_native")

    monkeypatch.setattr(simulator.chiplet, "_native_extension", absent)
    with pytest.raises(RuntimeError, match="_megaforth_native"):
        create_one_core_address_space(chiplet_ports=1)
    assert create_one_core_address_space().mmio.chiplet is None


@pytest.mark.parametrize("count", [0, -1, 256, True, 1.5])
def test_an_impossible_port_count_is_refused(count):
    with pytest.raises((TypeError, ValueError)):
        HostedChipletPorts(count)


# ---------------------------------------------------------------------------
#  The mailbox for the one core and its ports
# ---------------------------------------------------------------------------

def test_the_core_rings_a_port_and_acknowledges_its_signal():
    memory = _memory()
    table = memory.mmio.chiplet.mailbox
    status = MMIO_BASE + MAILBOX_STATUS

    memory.write8(MMIO_BASE + MAILBOX_SEND, PORT_ID)
    assert table.lowest_source(PORT_ID) == 0
    assert memory.read8(status) == 0

    table.send(PORT_ID, 0)
    assert memory.read8(status) == 1 << PORT_ID
    memory.write8(MMIO_BASE + MAILBOX_ACK, PORT_ID)
    assert memory.read8(status) == 0

    # A signal that names no participant, or the core itself, is ignored.
    memory.write8(MMIO_BASE + MAILBOX_SEND, 0)
    memory.write8(MMIO_BASE + MAILBOX_SEND, 9)
    assert table.pending_mask(0) == 0


def test_the_cores_data_slot_holds_its_bytes():
    memory = _memory()
    memory.write64(MMIO_BASE + MAILBOX_OFFSET, 0x1122_3344_5566_7788)
    assert memory.read64(MMIO_BASE + MAILBOX_OFFSET) == 0x1122_3344_5566_7788
    assert memory.read8(MMIO_BASE + MAILBOX_OFFSET + 1) == 0x77
    with pytest.raises(MMIOAccessError):
        memory.read16(MMIO_BASE + MAILBOX_OFFSET + 0x0F)
    with pytest.raises(MMIOAccessError):
        memory.read8(MMIO_BASE + MAILBOX_OFFSET + 0x10)


# ---------------------------------------------------------------------------
#  Forth source brings the chiplet up and works with it
# ---------------------------------------------------------------------------

def _outcome(**options):
    memory, runtime = _prepared(**options)
    identity = _run(runtime, "BRING-UP")
    reply = _run(runtime, "ASK")
    ports = runtime.chiplet_ports
    assert identity == (0x4348_4950_0000_0001,)
    assert reply == (~COMMAND_WORD & MASK64,)
    assert memory.read64(REPLY) == ~COMMAND_WORD & MASK64
    assert memory.read8(REPLY + 8) == 1
    assert memory.read64(REPLY + 16) == 1          # the probe was refused
    assert memory.read64(REPLY + 24) == 0xAAA
    assert memory.read64(REPLY + 32) == 0xBBB
    assert memory.read64(WINDOW + STATUS) & (READY | FAULT) == READY | FAULT
    assert memory.read64(WINDOW + FAULT_ADDR) == COMMAND
    assert ports.mailbox.pending_mask(0) == 0
    assert ports.mailbox.lowest_source(PORT_ID) is None
    assert not ports.work_waits
    return memory.read_bytes(EXTERNAL_BASE, EXT_SIZE)


def test_forth_brings_the_chiplet_up_and_gets_its_answer():
    _outcome()


def test_dense_and_sparse_memory_give_the_same_outcome():
    assert _outcome(dense=True) == _outcome()


def test_a_second_command_is_served_and_counted():
    memory, runtime = _prepared()
    _run(runtime, "BRING-UP")
    _run(runtime, "ASK")
    _put(memory, COMMAND, 0x77)
    assert _run(runtime, "ASK") == (~0x77 & MASK64,)
    assert memory.read8(REPLY + 8) == 2


def test_the_chiplet_steps_when_a_dispatch_returns():
    memory, runtime = _prepared()
    runtime.evaluate(
        f": RELEASE ( -- ) {IMAGE} 0xD8 CP!  40 0xE0 CP!  9 0x80 CP! ;\n"
        .encode())
    _run(runtime, "RELEASE")
    # No register was read after the release: the step at the dispatch's
    # return tried to fetch the image, which no range lends, and the bridge
    # recorded the refusal.
    assert not runtime.chiplet_ports.work_waits
    assert memory.read64(WINDOW + FAULT_ADDR) == IMAGE


def test_without_external_memory_nothing_is_lent():
    memory = _memory(external=0)
    runtime = MegaForthRuntime(memory=memory)
    runtime.chiplet_ports.attach_test_chiplet(0)
    memory.write64(WINDOW + 0xA8, 0)
    memory.write64(WINDOW + 0xB0, MASK64)
    memory.write64(WINDOW + 0xB8, 3)
    memory.write64(WINDOW + 0xE0, 40)
    memory.write64(WINDOW + CTRL, 1)
    status = memory.read64(WINDOW + STATUS)
    assert status & FAULT and not status & READY
    assert (memory.read64(WINDOW + 0x98) >> 4) & 0xF == 3


def test_the_ports_state_is_saved_and_restored():
    memory, runtime = _prepared()
    _run(runtime, "BRING-UP")
    ports = runtime.chiplet_ports
    saved = ports.save()
    memory.write64(WINDOW + CTRL, RESET_HOLD)
    assert ports.restore(saved)
    assert memory.read64(WINDOW + CTRL) == 9
    assert _run(runtime, "ASK") == (~COMMAND_WORD & MASK64,)
    assert not ports.restore(saved[:-1])
