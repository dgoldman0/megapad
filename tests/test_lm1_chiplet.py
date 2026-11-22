"""LM-1's chiplet module behind a port of the emulator and of the simulator.

LM-1's repository builds its C++ model as a module library that implements
shared/accel/chiplet_module.h (chiplet/ and design/chiplet-module.md
there). These cases attach that library to port 0 of the emulator's
machine and of the hosted simulator, and bring LM-1 up in the order
docs/chiplet-link.md gives: read the identity record, lend the image
read-only, name it and the boot argument, release. LM-1 fetches its image
from MegaPad's memory through the port bridge and runs it.

The image is a small LM-1 program. It rings MegaPad's core 0 once it runs.
Then for each doorbell it compares the memory LM-1 lends MegaPad with the
copy of an accumulator image that its own image carries, and rings back the
participant that rang when the two agree, or BAD when they do not. MegaPad
stores that accumulator image into LM-1's lent memory with TACC.STORE, rings
LM-1 and waits for the answer, so the answer shows that the boot fetch, the
store and the doorbells in both directions all worked through LM-1's own
code. The port's state, LM-1's module with it, is then saved and taken back
by a fresh machine, where LM-1 answers again from the restored memory.

LM-1 is found at LM1_ROOT, which the workspace's test selection sets, or
beside this checkout. Its binding (emu/lm1/chiplet.py) builds the library
against this checkout's chiplet_module.h whenever the library is older than
its sources or the header, and reads all of them to decide, so the test
selection reruns these cases when any of them changes.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

MEGAPAD = Path(__file__).resolve().parents[1]
LM1 = (Path(os.environ["LM1_ROOT"]) if os.environ.get("LM1_ROOT")
       else MEGAPAD.parent / "lm1")
if not (LM1 / "emu" / "lm1" / "chiplet.py").is_file():
    pytest.skip("no LM-1 checkout at %s; LM1_ROOT names one" % LM1,
                allow_module_level=True)
sys.path.insert(0, str(LM1 / "emu"))
# The library this machine loads is built against this checkout's header,
# whichever MegaPad the caller's environment names.
os.environ["MEGAPAD_ROOT"] = str(MEGAPAD)

from lm1 import chiplet as lm1_chiplet  # noqa: E402
from lm1.decode import Op, encode_b, encode_i, encode_r, encode_s, encode_x  # noqa: E402
from lm1.word import tag_fixnum  # noqa: E402

from asm import assemble  # noqa: E402
from megapad64 import (  # noqa: E402
    CSR_IPIACK, CSR_MBOX, EW_U8, IVEC_BUS_FAULT, TrapError,
)
from system import EXT_MEM_BASE, MMIO_START, MegapadSystem  # noqa: E402

LIBRARY = lm1_chiplet.ensure_library()

# The port's registers, as offsets in the register window.
ID_INDEX, ID_DATA = 0xC8, 0xD0
CTRL, STATUS = 0x80, 0x88
RANGE_SEL, RANGE_BASE, RANGE_LIMIT, RANGE_RIGHTS = 0xA0, 0xA8, 0xB0, 0xB8
BOOT_ADDR, BOOT_LEN, BOOT_ARG = 0xD8, 0xE0, 0xE8
ENABLE, RESET_HOLD, DOORBELL_EN, BORROW_EN = 1 << 0, 1 << 1, 1 << 3, 1 << 5
READY, PRESENT = 1 << 2, 1 << 0
READ = 1

CHIPLET_WINDOW = 0x0100_0000_0000_0000
LENT = 0x4000                   # LM-1 lends MegaPad 256 bytes here
CONFIG = "lend=%#x:0x100" % LENT
EXT_SIZE = 0x4000
UP, BAD = 0, 1                  # participants LM-1 rings: core 0, core 1
CONTROL_BLOCK = 0x0BAD_C0DE     # the boot argument; no queues yet
MASK64 = (1 << 64) - 1

# The accumulator image MegaPad stores, U8, all 256 bytes active.
TACC_IMAGE = bytes((7 * i + 3) & 0xFF for i in range(256))
EXPECT = [int.from_bytes(TACC_IMAGE[8 * w:8 * w + 8], "little") for w in range(32)]


def lm1_image() -> list[int]:
    """The LM-1 program and its copy of the accumulator image, as words."""
    out, inn = tag_fixnum(1), tag_fixnum(0)     # queues 1 and 0 of tile 0
    code_words = 12
    expect_at = 8 * code_words                  # from the load base, 0
    code = [
        encode_i(Op.LI, 2, 0, out),
        encode_i(Op.LI, 4, 0, inn),
        encode_i(Op.LI, 3, 0, tag_fixnum(UP)),
        encode_s(Op.SEND, 2, 3, 0),             # LM-1 runs: ring core 0
        encode_i(Op.RECV, 5, 4, 0),             # 4 wait: fixnum(the source)
        encode_i(Op.LI32, 6, 0, 0), LENT,       # the lent memory
        encode_i(Op.LI32, 7, 0, 0), expect_at,  # the copy
        encode_i(Op.LI, 8, 0, tag_fixnum(32)),
        encode_i(Op.LDR, 9, 6, 0),              # 10 compare a word
        encode_i(Op.LDR, 10, 7, 0),
        encode_r(Op.ARITH_RAW, 11, 9, 10, 1),   # r11 = r9 - r10
        encode_b(Op.BR_COND, 11, 5, 4),         # BR.EQ r11: the same, to 17
        encode_i(Op.LI, 12, 0, tag_fixnum(BAD)),
        encode_s(Op.SEND, 2, 12, 0),            # a word differs: ring BAD
        encode_b(Op.BR, 0, 0, -12),             # back to wait
        encode_i(Op.ADD_FIX_IMM, 6, 6, 8),      # 17 the next words
        encode_i(Op.ADD_FIX_IMM, 7, 7, 8),
        encode_i(Op.ADD_FIX_IMM, 8, 8, tag_fixnum(-1) & 0xFFFF),
        encode_b(Op.BR_COND, 8, 4, -10),        # BR.FIX.GT r8: to 10
        encode_s(Op.SEND, 2, 5, 0),             # all agree: ring the source
        encode_b(Op.BR, 0, 0, -18),             # back to wait
    ]
    assert len(code) <= 2 * code_words
    code += [0] * (2 * code_words - len(code))
    words = [code[i] | code[i + 1] << 32 for i in range(0, len(code), 2)]
    return words + EXPECT


IMAGE_WORDS = lm1_image()


def test_the_image_compares_what_it_was_given():
    """The program itself, on LM-1's model alone through LM-1's own
    interface: given the image in its lent memory it rings back the source,
    and given anything else it rings BAD."""
    for lent, answer in ((TACC_IMAGE, 5), (bytes(256), BAD)):
        model = lm1_chiplet.Model("mem=64K")
        model.write(0, 0, b"".join(word.to_bytes(8, "little") for word in IMAGE_WORDS))
        model.write(0, LENT, lent)
        model.queue_push(0, 0, tag_fixnum(5))
        result = model.run(0, 2000)
        assert result.stop == lm1_chiplet.STOP_IDLE
        assert model.queue(0, 1) == [tag_fixnum(UP), tag_fixnum(answer)]


# ---------------------------------------------------------------------------
#  The emulator's machine
# ---------------------------------------------------------------------------

WINDOW = MMIO_START + 0xE00
IMAGE = EXT_MEM_BASE
PROGRAM = 0x200


class Emulated:
    """A machine of two full cores with LM-1 on port 0."""

    def __init__(self):
        system = MegapadSystem(ram_size=1 << 16, num_cores=2, num_clusters=0,
                               hbw_size=0, ext_mem_size=EXT_SIZE, vram_size=0)
        system.load_binary(0, assemble("spin:\n    br spin\n"))
        system.boot(entry=0)
        system.attach_chiplet(0, LIBRARY, CONFIG)
        self.system = system
        self.native = system._native_system
        self.cpu = system.cpu
        self.port_id = system.first_chiplet_port_id

    def reg(self, offset):
        return self.cpu.mem_read64(WINDOW + offset)

    def set_reg(self, offset, value):
        self.cpu.mem_write64(WINDOW + offset, value)

    def bring_up(self):
        """docs/chiplet-link.md's order, then steps until READY."""
        assert self.reg(STATUS) & PRESENT
        self.set_reg(ID_INDEX, 0)
        assert self.reg(ID_DATA) == 0x4348_4950_0000_0001
        self.set_reg(ID_INDEX, 1)
        assert self.reg(ID_DATA) == int.from_bytes(b"LM1", "little")
        for i, word in enumerate(IMAGE_WORDS):
            offset = IMAGE - EXT_MEM_BASE + 8 * i
            self.system._ext_mem[offset:offset + 8] = word.to_bytes(8, "little")
        self.set_reg(RANGE_SEL, 0)
        self.set_reg(RANGE_BASE, IMAGE)
        self.set_reg(RANGE_LIMIT, IMAGE + 8 * len(IMAGE_WORDS))
        self.set_reg(RANGE_RIGHTS, READ)
        self.set_reg(BOOT_ADDR, IMAGE)
        self.set_reg(BOOT_LEN, 8 * len(IMAGE_WORDS))
        self.set_reg(BOOT_ARG, CONTROL_BLOCK)
        self.set_reg(CTRL, ENABLE | DOORBELL_EN | BORROW_EN)
        for _ in range(40):
            self.native._service_chiplet_ports()
            if self.reg(STATUS) & READY:
                break
        assert self.reg(STATUS) & READY
        for _ in range(4):
            self.native._service_chiplet_ports()

    def install(self, source, core=0):
        """Code at PROGRAM, its instruction cache lines primed, as
        tests/test_chiplet_tacc_store.py places it."""
        code = bytes(assemble(source))
        self.system.load_binary(PROGRAM, code)
        cpu = self.system.cores[core]
        valid, tags, data = cpu._cs.icache_snapshot()
        valid, tags, data = bytearray(valid), list(tags), bytearray(data)
        for line in range(PROGRAM & ~0xF, PROGRAM + len(code), 16):
            index = (line >> 4) & 0xFF
            valid[index] = 1
            tags[index] = line >> 12
            data[index * 16:index * 16 + 16] = cpu.mem[line:line + 16]
        cpu._cs.icache_restore(bytes(valid), tags, bytes(data))
        cpu.pc = PROGRAM
        cpu.halted = False
        cpu.idle = False

    def store(self, destination):
        """One TACC.STORE of TACC_IMAGE from core 0."""
        cpu = self.cpu
        cpu.tmode = EW_U8
        for instruction in ("t.acc.try", "t.acc.clear"):
            self.install(instruction)
            cpu.step()
        cpu.tacc = TACC_IMAGE
        cpu.tacc_dirty = True
        cpu.tdst = destination
        self.install("t.acc.store")
        cpu.step()

    def ask(self):
        """Core 0 takes LM-1's waiting doorbell, rings LM-1 and waits for its
        answer; returns core 0's pending mask on the answer."""
        for core in self.system.cores:
            core.halted = True
        self.install(f"""
            ldi64 r1, {self.port_id}
            csrw {CSR_IPIACK}, r1
            csrw {CSR_MBOX}, r1
        wait:
            bref answered
            br wait
        answered:
            csrr r6, {CSR_MBOX}
            csrw {CSR_IPIACK}, r1
            halt
        """)
        for _ in range(400):
            if self.cpu.halted:
                return self.cpu.regs[6]
            self.system.run_batch(64)
        bad = self.native.ipi_pending_mask(1) >> self.port_id & 1
        raise AssertionError("LM-1 never answered core 0" +
                             ("; it rang BAD: the lent memory differs" if bad else ""))


def test_emulator_boots_lm1_and_takes_its_store():
    machine = Emulated()
    machine.bring_up()
    native = machine.native
    # LM-1's program rang core 0 once it ran.
    assert native.ipi_pending_mask(0) == 1 << machine.port_id
    # A store outside what LM-1 lends is refused by LM-1, a bus fault at
    # the store's base.
    with pytest.raises(TrapError) as fault:
        machine.store(CHIPLET_WINDOW + LENT + 0x100)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert machine.cpu.trap_addr == CHIPLET_WINDOW + LENT + 0x100
    machine.store(CHIPLET_WINDOW + LENT)
    assert machine.ask() == 1 << machine.port_id
    assert native.ipi_pending_mask(1) == 0, "LM-1 rang BAD"
    assert not native.chiplet_work_waits


def test_emulator_saves_and_restores_lm1():
    first = Emulated()
    first.bring_up()
    first.store(CHIPLET_WINDOW + LENT)
    assert first.ask() == 1 << first.port_id
    saved = first.native.chiplet_save()

    second = Emulated()
    assert second.native.chiplet_restore(saved)
    assert second.reg(CTRL) == ENABLE | DOORBELL_EN | BORROW_EN
    assert second.reg(STATUS) & READY
    # The restored LM-1 waits in RECV with the stored image in its memory.
    assert second.ask() == 1 << second.port_id
    assert second.native.chiplet_save() != saved   # it answered once more
    assert first.ask() == 1 << first.port_id
    assert second.native.chiplet_save() == first.native.chiplet_save()

    third = Emulated()
    assert not third.native.chiplet_restore(saved[:-1])
    assert third.reg(CTRL) & RESET_HOLD


# ---------------------------------------------------------------------------
#  The hosted simulator
# ---------------------------------------------------------------------------

def _hosted():
    from simulator.memory import EXTERNAL_BASE
    from simulator.platform import create_one_core_address_space
    from simulator.runtime import MegaForthRuntime

    memory = create_one_core_address_space(bank0_size=1 << 20, external_size=EXT_SIZE,
                                           chiplet_ports=1)
    runtime = MegaForthRuntime(memory=memory)
    runtime.chiplet_ports.attach_module(0, LIBRARY, CONFIG)
    return runtime, EXTERNAL_BASE


class Hosted:
    """The hosted simulator's one core with LM-1 on its one port."""

    def __init__(self):
        self.runtime, self.external = _hosted()
        self.ports = self.runtime.chiplet_ports
        self.link = self.ports.link
        self.mailbox = self.ports.mailbox
        self.port_id = self.ports.first_port_id

    def bring_up(self):
        link = self.link
        assert link.read64(STATUS) & PRESENT
        link.write64(ID_INDEX, 0)
        assert link.read64(ID_DATA) == 0x4348_4950_0000_0001
        image = b"".join(word.to_bytes(8, "little") for word in IMAGE_WORDS)
        self.runtime.memory.write_bytes(self.external, image)
        for offset, value in ((RANGE_SEL, 0), (RANGE_BASE, self.external),
                              (RANGE_LIMIT, self.external + len(image)),
                              (RANGE_RIGHTS, READ), (BOOT_ADDR, self.external),
                              (BOOT_LEN, len(image)), (BOOT_ARG, CONTROL_BLOCK),
                              (CTRL, ENABLE | DOORBELL_EN | BORROW_EN)):
            link.write64(offset, value)
        for _ in range(40):
            self.ports.step()
            if link.read64(STATUS) & READY:
                break
        assert link.read64(STATUS) & READY
        while self.ports.step():
            pass

    def store(self):
        """TACC-STORE of TACC_IMAGE into LM-1's lent memory."""
        data = self.external + 0x2000
        self.runtime.memory.write_bytes(data, TACC_IMAGE)
        tile = self.runtime.tile
        tile.set_mode(EW_U8)
        tile.tacc_try()
        tile.set_source0(data)
        tile.tacc_load()
        tile.set_destination(CHIPLET_WINDOW + LENT)
        tile.tacc_store()

    def ask(self):
        """The core takes LM-1's waiting doorbell, rings LM-1, and steps the
        port until LM-1 answers."""
        core = 0
        self.mailbox.acknowledge(core, self.port_id)
        assert self.mailbox.send(core, self.port_id)
        for _ in range(40):
            self.ports.step()
            if self.mailbox.pending_mask(core) >> self.port_id & 1:
                return True
        return False


def test_simulator_boots_lm1_and_takes_its_store():
    machine = Hosted()
    machine.bring_up()
    assert machine.mailbox.pending_mask(0) == 1 << machine.port_id
    machine.store()
    assert machine.ask(), "LM-1 never answered: the lent memory differs, or no doorbell"
    assert not machine.ports.work_waits


def test_simulator_saves_and_restores_lm1():
    first = Hosted()
    first.bring_up()
    first.store()
    assert first.ask()
    saved = first.ports.save()

    second = Hosted()
    assert second.ports.restore(saved)
    assert second.link.read64(STATUS) & READY
    # The mailbox is the machine's, so the restored LM-1's answer is new.
    assert second.ask()
    assert first.ask()
    assert second.ports.save() == first.ports.save()
    assert not Hosted().ports.restore(saved[:-1])
