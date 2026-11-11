"""The chiplet ports inside the emulator's machine.

The register window answers at MMIO offset 0xE00, from guest code on native
cores and from cores that Python runs, and takes aligned 64-bit accesses
alone. A chiplet attached to a port works between scheduler rounds. Guest
programs here bring the test chiplet up in the order docs/chiplet-link.md
gives, hand it a command through lent memory, and wait for its answer by the
flag branch and by sleeping until its doorbell.
"""

from __future__ import annotations

import shutil
import subprocess
import os
from pathlib import Path

import pytest

from asm import assemble
from megapad64 import CSR_IPIACK, CSR_MBOX, IVEC_BUS_FAULT, TrapError
from system import EXT_MEM_BASE, MMIO_START, MegapadSystem


WINDOW = MMIO_START + 0xE00
PORT_COUNT = 0x00
PORT_SEL = 0x08
EXT_CORE_QOS = 0x10
CTRL = 0x80
STATUS = 0x88
FAULT_ADDR = 0x90
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
DOORBELL_EN = 1 << 3
PRESENT = 1 << 0
READY = 1 << 2
FAULT = 1 << 4
READ, WRITE = 1, 2

EXT_SIZE = 0x4000
IMAGE = EXT_MEM_BASE
COMMAND = EXT_MEM_BASE + 0x100
REPLY = EXT_MEM_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
MASK64 = (1 << 64) - 1
PROGRAM = 0x200

SOURCE = Path(__file__).resolve().parents[1] / "shared" / "accel"
_SPIN = assemble("spin:\n    br spin\n")


def _machine(*, full_cores=1, clusters=0, ext=EXT_SIZE, **options):
    system = MegapadSystem(
        ram_size=1 << 16,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        ext_mem_size=ext,
        vram_size=0,
        **options,
    )
    system.load_binary(0, _SPIN)
    system.boot(entry=0)
    return system


def _put(system, addr, *words):
    offset = addr - EXT_MEM_BASE
    for index, word in enumerate(words):
        system._ext_mem[offset + 8 * index:offset + 8 * index + 8] = (
            (word & MASK64).to_bytes(8, "little"))


def _word(system, addr):
    offset = addr - EXT_MEM_BASE
    return int.from_bytes(system._ext_mem[offset:offset + 8], "little")


def _start(system, source, *, core=0):
    """Point one core at *source* and halt the others."""
    system.load_binary(PROGRAM, assemble(source))
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    cpu = system.cores[core]
    cpu.pc = PROGRAM
    cpu.halted = False
    return cpu


def _run(system, cpu, *, rounds=400, quantum=64, by_step=False):
    """Run in short batches, so the chiplet steps between them, to a halt."""
    for _ in range(rounds):
        if cpu.halted:
            return
        if by_step:
            system.step()
        else:
            system.run_batch(quantum)
    raise AssertionError("the guest program never halted")


# ---------------------------------------------------------------------------
#  The register window
# ---------------------------------------------------------------------------

def test_the_window_answers_a_core_that_python_runs():
    system = _machine(full_cores=2)
    cpu = system.cpu
    first_id = system.first_chiplet_port_id
    assert cpu.mem_read64(WINDOW + PORT_COUNT) == 1 | (first_id << 8) | (8 << 16)
    assert cpu.mem_read64(WINDOW + CTRL) == RESET_HOLD
    assert (cpu.mem_read64(WINDOW + STATUS) >> 16) & 0xFF == first_id

    cpu.mem_write64(WINDOW + BOOT_ADDR, 0x1122_3344_5566_7788)
    assert cpu.mem_read64(WINDOW + BOOT_ADDR) == 0x1122_3344_5566_7788
    cpu.mem_write64(WINDOW + EXT_CORE_QOS, 7)
    assert system.cores[1].mem_read64(WINDOW + EXT_CORE_QOS) == 7
    assert system._native_system.chiplet_port_count_word == (
        1 | (first_id << 8) | (8 << 16))


@pytest.mark.parametrize(
    ("access", "offset"),
    [("mem_read8", 0x80), ("mem_read16", 0x80), ("mem_read32", 0x80),
     ("mem_read64", 0x84), ("mem_read64", 0xFC), ("mem_read8", 0xFF)],
)
def test_any_other_shape_of_read_is_a_bus_fault(access, offset):
    system = _machine()
    cpu = system.cpu
    with pytest.raises(TrapError) as fault:
        getattr(cpu, access)(WINDOW + offset)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == WINDOW + offset


@pytest.mark.parametrize(
    ("access", "offset"),
    [("mem_write8", 0x80), ("mem_write16", 0x80), ("mem_write32", 0x80),
     ("mem_write64", 0x81)],
)
def test_any_other_shape_of_write_is_a_bus_fault(access, offset):
    system = _machine()
    cpu = system.cpu
    with pytest.raises(TrapError) as fault:
        getattr(cpu, access)(WINDOW + offset, 0)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.mem_read64(WINDOW + CTRL) == RESET_HOLD


def test_a_machine_without_ports_has_no_window():
    system = _machine(chiplet_ports=0)
    with pytest.raises(TrapError) as fault:
        system.cpu.mem_read64(WINDOW + PORT_COUNT)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert system._native_system.chiplet_port_count_word == 0
    with pytest.raises(IndexError):
        system.attach_test_chiplet(0)


def test_guest_code_reads_and_writes_the_window():
    system = _machine()
    cpu = _start(system, f"""
        ldi64 r9, {WINDOW + PORT_COUNT}
        ldn r4, r9
        ldi64 r9, {WINDOW + BOOT_LEN}
        ldi64 r1, 0x0BAD_F00D_CAFE_0040
        str r9, r1
        ldn r5, r9
        ldi64 r9, {WINDOW + CTRL}
        ldn r6, r9
        halt
    """)
    _run(system, cpu)
    first_id = system.first_chiplet_port_id
    assert cpu.regs[4] == 1 | (first_id << 8) | (8 << 16)
    assert cpu.regs[5] == 0x0BAD_F00D_CAFE_0040
    assert cpu.regs[6] == RESET_HOLD


@pytest.mark.parametrize("load", ["ld.b", "ld.h", "ld.w"])
def test_guest_code_takes_a_bus_fault_on_a_narrow_access(load):
    system = _machine()
    cpu = _start(system, f"""
        ldi64 r9, {WINDOW + CTRL}
        {load} r4, r9
        halt
    """)
    cpu.step()
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == WINDOW + CTRL


def test_a_port_reports_its_chiplet_and_loses_it_on_detach():
    system = _machine()
    cpu = system.cpu
    assert cpu.mem_read64(WINDOW + STATUS) & PRESENT == 0
    system.attach_test_chiplet(0)
    assert cpu.mem_read64(WINDOW + STATUS) & PRESENT
    cpu.mem_write64(WINDOW + ID_INDEX, 1)
    assert cpu.mem_read64(WINDOW + ID_DATA) == int.from_bytes(
        b"TESTCHIP", "little")
    system.detach_chiplet(0)
    assert cpu.mem_read64(WINDOW + STATUS) & PRESENT == 0
    with pytest.raises(IndexError):
        system.detach_chiplet(1)


# ---------------------------------------------------------------------------
#  A guest program brings the chiplet up and works with it
# ---------------------------------------------------------------------------

def _wr(offset, value):
    return (f"    ldi64 r9, {WINDOW + offset}\n"
            f"    ldi64 r1, {value}\n"
            f"    str r9, r1\n")


def _lend(index, base, limit, rights):
    return (_wr(RANGE_SEL, index) + _wr(RANGE_BASE, base) +
            _wr(RANGE_LIMIT, limit) + _wr(RANGE_RIGHTS, rights))


def _bring_up(port_id):
    """The order docs/chiplet-link.md gives, then one command."""
    return (
        # 1. Read the identity record.
        _wr(ID_INDEX, 0) +
        f"    ldi64 r9, {WINDOW + ID_DATA}\n"
        f"    ldn r4, r9\n" +
        # 2. The image is in memory already; lend it read-only.
        _lend(0, IMAGE, IMAGE + 0x100, READ) +
        # 3. Name the image.
        _wr(BOOT_ADDR, IMAGE) + _wr(BOOT_LEN, 40) +
        # 4. Install the ranges.
        _lend(1, COMMAND, COMMAND + 0x900, READ) +
        _lend(2, REPLY, REPLY + 0x1000, READ | WRITE) +
        # 5. Release the chiplet with the port open, and wait for ready.
        _wr(CTRL, ENABLE | DOORBELL_EN) +
        f"    ldi64 r9, {WINDOW + STATUS}\n"
        f"wait_ready:\n"
        f"    ldn r5, r9\n"
        f"    andi r5, {READY}\n"
        f"    breq wait_ready\n"
        # Ring the chiplet for the command in lent memory.
        f"    ldi64 r1, {port_id}\n"
        f"    csrw {CSR_MBOX}, r1\n"
    )


def _finish(port_id):
    """Acknowledge the chiplet's doorbell and fetch its reply."""
    return (
        f"    csrr r6, {CSR_MBOX}\n"
        f"    ldi64 r1, {port_id}\n"
        f"    csrw {CSR_IPIACK}, r1\n"
        f"    ldi64 r9, {REPLY}\n"
        f"    ldn r7, r9\n"
        f"    halt\n"
    )


def _wait_by_flag(port_id):
    return (_bring_up(port_id) +
            "wait_flag:\n"
            "    bref answered\n"
            "    br wait_flag\n"
            "answered:\n" + _finish(port_id))


def _wait_asleep(port_id):
    # A core that sleeps with interrupts off wakes on the IPI line and runs
    # on from the instruction after IDL.
    return _bring_up(port_id) + "    idl\n" + _finish(port_id)


def _prepare(system, command=0x00C0_FFEE_0000_0003):
    _put(system, IMAGE, BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
    _put(system, COMMAND, command, 0x111, 0x222, 0x333)
    return command


def _check(system, cpu, command):
    port_id = system.first_chiplet_port_id
    assert cpu.regs[4] == 0x4348_4950_0000_0001
    assert cpu.regs[6] == 1 << port_id
    assert cpu.regs[7] == ~command & MASK64
    assert _word(system, REPLY) == ~command & MASK64
    assert system._ext_mem[REPLY - EXT_MEM_BASE + 8] == 1
    assert _word(system, REPLY + 16) == 1          # the probe was refused
    assert [_word(system, REPLY + 24 + 8 * index)
            for index in range(3)] == [0x111, 0x222, 0x333]
    native = system._native_system
    assert native.ipi_pending_mask(cpu.core_id) == 0
    assert not native.ef_line(cpu.core_id)
    assert native.chiplet_port_doorbell(0) is None
    assert cpu.mem_read64(WINDOW + STATUS) & (READY | FAULT) == READY | FAULT
    assert cpu.mem_read64(WINDOW + FAULT_ADDR) == COMMAND
    assert not native.chiplet_work_waits


@pytest.mark.parametrize("program", [_wait_by_flag, _wait_asleep])
@pytest.mark.parametrize("by_step", [False, True])
def test_a_guest_brings_the_chiplet_up_and_gets_its_answer(program, by_step):
    system = _machine()
    system.attach_test_chiplet(0)
    command = _prepare(system)
    cpu = _start(system, program(system.first_chiplet_port_id))
    _run(system, cpu, by_step=by_step, rounds=4000 if by_step else 400)
    _check(system, cpu, command)


def test_a_sleeping_machine_stays_awake_for_a_working_chiplet():
    system = _machine()
    system.attach_test_chiplet(0, budget=1)
    _prepare(system)
    assert system.idle_wake_delay_s() is None
    cpu = _start(system, _wait_asleep(system.first_chiplet_port_id))
    # Run until the core sleeps on IDL with its command rung.
    for _ in range(400):
        system.run_batch(16)
        if cpu.idle or cpu.halted:
            break
    assert cpu.idle or cpu.halted
    _run(system, cpu)
    assert system.idle_wake_delay_s() is None
    assert cpu.regs[7] == ~0x00C0_FFEE_0000_0003 & MASK64


def _outcome(*, full_cores=1, clusters=0, quantum=64, budget=None,
             program=_wait_by_flag, **attach):
    system = _machine(full_cores=full_cores, clusters=clusters)
    if "module_path" in attach:
        system.attach_chiplet(0, attach["module_path"], budget=budget)
    else:
        system.attach_test_chiplet(0, budget=budget)
    command = _prepare(system)
    cpu = _start(system, program(system.first_chiplet_port_id))
    _run(system, cpu, quantum=quantum, rounds=4000)
    _check(system, cpu, command)
    return bytes(system._ext_mem), cpu.regs[4], cpu.regs[7]


@pytest.mark.parametrize(
    "shape",
    [dict(full_cores=2), dict(full_cores=4), dict(full_cores=1, clusters=1),
     dict(quantum=7), dict(quantum=1000), dict(budget=1), dict(budget=3),
     dict(program=_wait_asleep), dict(program=_wait_asleep, budget=2)],
)
def test_the_outcome_is_the_same_on_every_machine_shape(shape):
    assert _outcome(**shape) == _outcome()
    assert _outcome(**shape) == _outcome(**shape)


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


def test_a_chiplet_loaded_from_a_library_gives_the_same_outcome(module_library):
    assert _outcome(module_path=module_library) == _outcome()


def test_the_ports_state_is_saved_and_restored_with_its_chiplet():
    system = _machine()
    system.attach_test_chiplet(0)
    command = _prepare(system)
    cpu = _start(system, _wait_by_flag(system.first_chiplet_port_id))
    _run(system, cpu)
    native = system._native_system
    saved = native.chiplet_save()

    cpu.mem_write64(WINDOW + CTRL, RESET_HOLD)
    cpu.mem_write64(WINDOW + BOOT_LEN, 0)
    assert native.chiplet_restore(saved)
    assert cpu.mem_read64(WINDOW + CTRL) == ENABLE | DOORBELL_EN
    assert cpu.mem_read64(WINDOW + BOOT_LEN) == 40
    assert cpu.mem_read64(WINDOW + STATUS) & READY
    assert not native.chiplet_restore(saved[:-1])
    _check(system, cpu, command)
