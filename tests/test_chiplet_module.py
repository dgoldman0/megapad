"""The test chiplet behind a port: a whole chiplet model on the link.

shared/accel/chiplet_test_module.cpp implements the module interface of
shared/accel/chiplet_module.h. These cases bring it up in the order
docs/chiplet-link.md gives, exchange work with it through lent memory and
doorbells, stop it, reset it, save and restore it, and load the same source
built alone as a module library. Both extensions must be built before this
gate.
"""

from __future__ import annotations

import importlib
import os
import shutil
import subprocess
from pathlib import Path

import pytest


EXT_BASE = 0x0020_0000
EXT_SIZE = 0x2000

IMAGE = EXT_BASE                    # five words, lent read-only
COMMAND = EXT_BASE + 0x100          # the command page, lent read-only
REPLY = EXT_BASE + 0x1000           # the reply page, lent read and write
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")

PORT_SEL = 0x08
CTRL = 0x80
STATUS = 0x88
FAULT_ADDR = 0x90
FAULT_INFO = 0x98
RANGE_SEL = 0xA0
RANGE_BASE = 0xA8
RANGE_LIMIT = 0xB0
RANGE_RIGHTS = 0xB8
ID_INDEX = 0xC8
ID_DATA = 0xD0
BOOT_ADDR = 0xD8
BOOT_LEN = 0xE0

ENABLE = 1 << 0
RESET_HOLD = 1 << 1
STOP_REQ = 1 << 2
DOORBELL_EN = 1 << 3

PRESENT = 1 << 0
STOPPED = 1 << 1
READY = 1 << 2
FAULT = 1 << 4
ID_DONE = 1 << 5

READ, WRITE = 1, 2
REFUSED = 1
NOT_LENT = 4
CORES = 4
PORT_ID = CORES
MASK64 = (1 << 64) - 1

SOURCE = Path(__file__).resolve().parents[1] / "shared" / "accel"


@pytest.fixture(params=("_mp64_accel", "_megaforth_native"))
def native(request):
    return importlib.import_module(request.param)


class Machine:
    """MegaPad's side of the link, brought up as the specification orders."""

    def __init__(self, native, *, attach="builtin", module_path=None):
        self.native = native
        self.memory = bytearray(EXT_SIZE)
        self.link = native.ChipletLink(self.memory, EXT_BASE, CORES, 1, 8)
        if attach == "builtin":
            self.link.attach_test_chiplet(0)
        elif attach == "loaded":
            self.link.load_chiplet_module(0, str(module_path))

    def put(self, addr, *words):
        offset = addr - EXT_BASE
        for index, word in enumerate(words):
            self.memory[offset + 8 * index:offset + 8 * index + 8] = (
                (word & MASK64).to_bytes(8, "little"))

    def word(self, addr):
        offset = addr - EXT_BASE
        return int.from_bytes(self.memory[offset:offset + 8], "little")

    def lend(self, index, base, limit, rights):
        self.link.write64(RANGE_SEL, index)
        self.link.write64(RANGE_BASE, base)
        self.link.write64(RANGE_LIMIT, limit)
        self.link.write64(RANGE_RIGHTS, rights)

    def identity(self, index):
        self.link.write64(ID_INDEX, index)
        assert self.link.read64(STATUS) & ID_DONE
        return self.link.read64(ID_DATA)

    def bring_up(self, *, magic=BOOT_MAGIC, boot_len=40, lend_image=True):
        # 1. Read the identity record.
        assert self.identity(0) == 0x4348_4950_0000_0001
        # 2. Load the image into a range lent read-only.
        self.put(IMAGE, magic, COMMAND, REPLY, COMMAND, 0)
        if lend_image:
            self.lend(0, IMAGE, IMAGE + 0x100, READ)
        # 3. Name the image.
        self.link.write64(BOOT_ADDR, IMAGE)
        self.link.write64(BOOT_LEN, boot_len)
        # 4. Install the ranges.
        self.lend(1, COMMAND, COMMAND + 0x900, READ)
        self.lend(2, REPLY, REPLY + 0x1000, READ | WRITE)
        # 5. Release the chiplet with the port open.
        self.link.write64(CTRL, ENABLE | DOORBELL_EN)

    def run(self, limit=1000):
        steps = 0
        while self.link.step():
            steps += 1
            assert steps < limit, "the chiplet never came to rest"
        return steps

    def command(self, core, word, *burst):
        self.put(COMMAND, word, *burst)
        assert self.link.mailbox.send(core, PORT_ID)

    def status(self):
        return self.link.read64(STATUS)


@pytest.fixture
def machine(native):
    return Machine(native)


def test_the_identity_record_names_the_contract_and_the_device(machine):
    assert machine.status() & PRESENT
    assert machine.identity(0) == 0x4348_4950_0000_0001
    assert machine.identity(1) == int.from_bytes(b"TESTCHIP", "little")
    assert machine.identity(2) == 0


def test_the_chiplet_fetches_its_image_and_raises_ready(machine):
    machine.bring_up()
    assert machine.status() & READY == 0
    assert machine.link.work_waits()
    machine.run()
    assert machine.status() & READY
    assert machine.status() & (STOPPED | FAULT) == 0
    assert not machine.link.work_waits()


@pytest.mark.parametrize(
    "fault",
    [{"magic": 0x1234}, {"boot_len": 32}, {"lend_image": False}],
)
def test_a_bad_image_leaves_the_chiplet_down(machine, fault):
    machine.bring_up(**fault)
    machine.run()
    assert machine.status() & READY == 0
    refused = "lend_image" in fault
    assert bool(machine.status() & FAULT) == refused
    if refused:
        assert machine.link.read64(FAULT_ADDR) == IMAGE
        assert (machine.link.read64(FAULT_INFO) >> 8) & 0xFF == 4
    # A doorbell for a chiplet that never came up stays pending.
    machine.link.mailbox.send(0, PORT_ID)
    machine.run()
    assert machine.link.mailbox.lowest_source(PORT_ID) == 0


def test_a_command_crosses_in_lent_memory_and_is_answered(machine):
    machine.bring_up()
    machine.run()
    mailbox = machine.link.mailbox

    machine.command(2, 0x1122_3344_5566_7700)
    assert machine.link.work_waits()
    machine.run()

    assert machine.word(REPLY) == ~0x1122_3344_5566_7700 & MASK64
    assert machine.memory[REPLY - EXT_BASE + 8] == 1
    # The probe write was refused, and the chiplet reported the code.
    assert machine.word(REPLY + 16) == REFUSED
    assert machine.status() & FAULT
    assert machine.link.read64(FAULT_ADDR) == COMMAND
    info = machine.link.read64(FAULT_INFO)
    assert info & 4 and (info >> 4) & 0xF == NOT_LENT
    assert machine.word(COMMAND) == 0x1122_3344_5566_7700
    # The chiplet rang the core that rang it, and took the core's doorbell.
    assert mailbox.pending_mask(2) == 1 << PORT_ID
    assert mailbox.port_pending(2)
    assert not mailbox.any_pending(PORT_ID)


def test_a_command_carries_a_burst(machine):
    machine.bring_up()
    machine.run()
    burst = [0xA000 + index for index in range(255)]
    machine.command(0, 0xFF, *burst)
    machine.run()
    assert [machine.word(REPLY + 24 + 8 * index)
            for index in range(255)] == burst
    assert machine.word(REPLY + 24 + 8 * 255) == 0


def test_senders_are_served_lowest_first_and_each_is_rung(machine):
    machine.bring_up()
    machine.run()
    mailbox = machine.link.mailbox
    machine.put(COMMAND, 0x40)
    mailbox.send(3, PORT_ID)
    mailbox.send(1, PORT_ID)
    machine.link.set_budget(0, 8)

    assert machine.link.step()
    assert mailbox.pending_mask(1) == 1 << PORT_ID
    assert mailbox.pending_mask(3) == 0
    machine.run()
    assert mailbox.pending_mask(3) == 1 << PORT_ID
    assert machine.memory[REPLY - EXT_BASE + 8] == 2


def test_a_doorbell_rung_before_ready_is_served_after_boot(machine):
    machine.bring_up()
    machine.command(0, 0x77)
    machine.run()
    assert machine.status() & READY
    assert machine.word(REPLY) == ~0x77 & MASK64
    assert machine.link.mailbox.pending_mask(0) == 1 << PORT_ID


@pytest.mark.parametrize("budget", [1, 2, 3, 5, 64])
def test_the_result_is_the_same_under_every_budget(native, budget):
    def outcome(step_budget):
        machine = Machine(native)
        machine.link.set_budget(0, step_budget)
        machine.bring_up()
        machine.run()
        machine.command(1, 0x0303_0303_0000_0003, 7, 8, 9)
        machine.link.mailbox.send(3, PORT_ID)
        steps = machine.run()
        return bytes(machine.memory), machine.link.mailbox.words(), steps

    memory, pending, steps = outcome(budget)
    reference_memory, reference_pending, reference_steps = outcome(1000)
    assert memory == reference_memory
    assert pending == reference_pending
    assert steps >= reference_steps
    if budget == 1:
        # Two commands of eight link operations each, one a step.
        assert steps >= 15


def test_a_stopped_chiplet_takes_no_doorbell_until_resumed(machine):
    machine.bring_up()
    machine.run()
    mailbox = machine.link.mailbox

    machine.link.write64(CTRL, ENABLE | DOORBELL_EN | STOP_REQ)
    machine.run()
    assert machine.status() & (READY | STOPPED) == READY | STOPPED

    machine.command(0, 0x55)
    machine.run()
    assert mailbox.lowest_source(PORT_ID) == 0
    assert machine.word(REPLY) == 0
    assert not machine.link.work_waits()

    machine.link.write64(CTRL, ENABLE | DOORBELL_EN)
    machine.run()
    assert machine.status() & STOPPED == 0
    assert machine.word(REPLY) == ~0x55 & MASK64
    assert not mailbox.any_pending(PORT_ID)


def test_stopping_abandons_the_command_in_hand(machine):
    machine.bring_up()
    machine.run()
    machine.link.set_budget(0, 2)
    machine.command(0, 0x66)
    assert machine.link.step()          # read the command, wrote the reply
    machine.link.write64(CTRL, ENABLE | DOORBELL_EN | STOP_REQ)
    machine.run()
    assert machine.status() & STOPPED
    assert machine.word(REPLY) == ~0x66 & MASK64
    assert machine.word(REPLY + 16) == 0
    assert machine.link.mailbox.pending_mask(0) == 0


def test_a_reset_chiplet_boots_again_and_finds_its_doorbell(machine):
    machine.bring_up()
    machine.run()
    machine.command(2, 0x99)

    machine.link.write64(CTRL, ENABLE | DOORBELL_EN | RESET_HOLD)
    assert machine.status() & READY == 0
    assert not machine.link.step()
    assert machine.link.mailbox.lowest_source(PORT_ID) == 2
    assert machine.word(REPLY) == 0

    machine.link.write64(CTRL, ENABLE | DOORBELL_EN)
    machine.run()
    assert machine.status() & READY
    assert machine.word(REPLY) == ~0x99 & MASK64
    # The count of commands served starts again with the chiplet.
    assert machine.memory[REPLY - EXT_BASE + 8] == 1


def test_a_chiplet_saved_in_mid_command_finishes_the_same(native):
    def start():
        machine = Machine(native)
        machine.link.set_budget(0, 1)
        machine.bring_up()
        machine.run()
        machine.command(1, 0x0000_0BAD_F00D_0004, 1, 2, 3, 4)
        machine.link.mailbox.send(2, PORT_ID)
        for _ in range(4):
            assert machine.link.step()
        return machine

    first = start()
    saved = first.link.save()
    memory = bytes(first.memory)
    pending = first.link.mailbox.words()
    first.run()

    second = Machine(native)
    second.memory[:] = memory
    assert second.link.mailbox.restore_words(pending)
    assert second.link.restore(saved)
    second.run()

    assert bytes(second.memory) == bytes(first.memory)
    assert second.link.mailbox.words() == first.link.mailbox.words()
    for offset in range(0, 0x100, 8):
        assert second.link.read64(offset) == first.link.read64(offset)
    assert first.word(REPLY + 24 + 24) == 4


def test_a_state_from_another_module_is_refused(native, machine):
    manual = native.ChipletLink(bytearray(EXT_SIZE), EXT_BASE, CORES, 1, 8)
    chiplet = native.ManualChiplet()
    chiplet.blob = [9, 9, 9]
    manual.attach_manual_chiplet(0, chiplet)
    manual.write64(CTRL, ENABLE)

    assert not machine.link.restore(manual.save())
    # The refusal leaves the port holding its chiplet in reset.
    assert machine.link.read64(CTRL) & RESET_HOLD


# ---------------------------------------------------------------------------
#  The same chiplet as a module library loaded at run time
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def module_library(tmp_path_factory):
    compiler = os.environ.get("CXX") or shutil.which("g++") or shutil.which("c++")
    assert compiler, "a C++ compiler is needed to build the module library"
    library = tmp_path_factory.mktemp("chiplet") / "test_chiplet.so"
    subprocess.run(
        [compiler, "-std=c++17", "-O2", "-fPIC", "-shared",
         "-fvisibility=hidden", "-Wall", "-Wextra", "-I", str(SOURCE),
         str(SOURCE / "chiplet_test_module.cpp"), "-o", str(library)],
        check=True,
    )
    return library


def test_a_loaded_module_behaves_as_the_built_in_one(native, module_library):
    def outcome(**attach):
        machine = Machine(native, **attach)
        machine.link.set_budget(0, 3)
        machine.bring_up()
        machine.run()
        machine.command(1, 0x0000_0000_C0DE_0006, *range(100, 106))
        machine.link.mailbox.send(0, PORT_ID)
        machine.run()
        return (bytes(machine.memory), machine.link.mailbox.words(),
                machine.link.save(), machine.identity(1))

    loaded = outcome(attach="loaded", module_path=module_library)
    assert loaded == outcome(attach="builtin")
    assert loaded[3] == int.from_bytes(b"TESTCHIP", "little")


def test_a_library_that_is_no_module_is_refused(native, tmp_path):
    machine = Machine(native, attach=None)
    with pytest.raises(RuntimeError):
        machine.link.load_chiplet_module(0, str(tmp_path / "absent.so"))
    with pytest.raises(RuntimeError, match="mp64_chiplet_create"):
        machine.link.load_chiplet_module(0, "libm.so.6")
    assert machine.status() & PRESENT == 0
