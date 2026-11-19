"""TACC.STORE into memory a chiplet lends.

A store whose image lies wholly inside a chiplet port's window sends the
image's active 64-byte units to that port, with the rest of the 256 bytes as
units of zeros, and retires on the chiplet's answer (docs/chiplet-link.md,
sections 3 and 4; docs/tile-engine.md). These cases run it on the
emulator's machine in the instruction-batched and strict-cycle models,
through the native path and the Python oracle, and read back what the test
chiplet holds with its copy command. The oracle's glue is also driven with
hooks of the test's own, so its route and its faults are checked without a
machine.
"""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    CSR_IPIACK,
    CSR_MBOX,
    EW_FP64,
    EW_U8,
    IVEC_BUS_FAULT,
    IVEC_PRIV_FAULT,
    Megapad64 as PythonMegapad64,
    TACC_IMAGE_BYTES,
    TrapError,
)
from system import EXT_MEM_BASE, MMIO_START, MegapadSystem


WINDOW = MMIO_START + 0xE00
CTRL = 0x80
STATUS = 0x88
RANGE_SEL = 0xA0
RANGE_BASE = 0xA8
RANGE_LIMIT = 0xB0
RANGE_RIGHTS = 0xB8
BOOT_ADDR = 0xD8
BOOT_LEN = 0xE0
ENABLE = 1 << 0
DOORBELL_EN = 1 << 3
BORROW_EN = 1 << 5
READY = 1 << 2
READ, WRITE = 1, 2
OK, REFUSED, MEM_ERROR = 0, 1, 2

EXT_SIZE = 0x4000
IMAGE = EXT_MEM_BASE
COMMAND = EXT_MEM_BASE + 0x100
REPLY = EXT_MEM_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
MASK64 = (1 << 64) - 1

CHIPLET_WINDOW = 0x0100_0000_0000_0000
WINDOW_BYTES = 1 << 48
LENT = 0x1000                   # the test chiplet's lent memory
STORE_AT = CHIPLET_WINDOW + LENT
COPY_FROM_LENT = 0x100          # the test chiplet's copy command
PROGRAM = 0x200

U8_IMAGE = bytes(range(TACC_IMAGE_BYTES))
FP64_IMAGE = bytes([0xD6]) * 64 + bytes(TACC_IMAGE_BYTES - 64)


class Machine:
    """One full core per engine asked for and the test chiplet, running."""

    def __init__(self, *, ctrl=ENABLE | DOORBELL_EN | BORROW_EN,
                 full_cores=1):
        self.system = MegapadSystem(
            ram_size=1 << 16,
            num_cores=full_cores,
            num_clusters=0,
            hbw_size=0,
            ext_mem_size=EXT_SIZE,
            vram_size=0,
        )
        self.system.load_binary(0, assemble("spin:\n    br spin\n"))
        self.system.boot(entry=0)
        self.system.attach_test_chiplet(0)
        cpu = self.system.cpu
        self.put(IMAGE, BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
        ranges = ((IMAGE, IMAGE + 0x100, READ),
                  (COMMAND, COMMAND + 0x900, READ),
                  (REPLY, REPLY + 0x1000, READ | WRITE))
        for index, (base, limit, rights) in enumerate(ranges):
            cpu.mem_write64(WINDOW + RANGE_SEL, index)
            cpu.mem_write64(WINDOW + RANGE_BASE, base)
            cpu.mem_write64(WINDOW + RANGE_LIMIT, limit)
            cpu.mem_write64(WINDOW + RANGE_RIGHTS, rights)
        cpu.mem_write64(WINDOW + BOOT_ADDR, IMAGE)
        cpu.mem_write64(WINDOW + BOOT_LEN, 40)
        cpu.mem_write64(WINDOW + CTRL, ctrl)
        for _ in range(4):
            self.system._native_system._service_chiplet_ports()
        assert cpu.mem_read64(WINDOW + STATUS) & READY

    def put(self, addr, *words):
        offset = addr - EXT_MEM_BASE
        for index, word in enumerate(words):
            self.system._ext_mem[offset + 8 * index:offset + 8 * index + 8] = (
                (word & MASK64).to_bytes(8, "little"))

    def owned_tacc(self, image, format_ew=EW_U8, *, core=0):
        """Claim the core's TACC and give it a valid, dirty image."""
        cpu = self.system.cores[core]
        cpu.tmode = format_ew
        for instruction in ("t.acc.try", "t.acc.clear"):
            self.install(instruction, core=core)
            cpu.step()
        cpu.tacc = image
        cpu.tacc_dirty = True
        return cpu

    def install(self, source, *, core=0):
        """Place code at PROGRAM, with its instruction cache lines primed."""
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
        return code

    def lent(self, words=32):
        """What the test chiplet lends, fetched back by its copy command."""
        self.put(COMMAND, COPY_FROM_LENT | words)
        port_id = self.system.first_chiplet_port_id
        for core in self.system.cores:
            core.halted = True
        cpu = self.system.cpu
        self.install(f"""
            ldi64 r1, {port_id}
            csrw {CSR_MBOX}, r1
        wait:
            bref answered
            br wait
        answered:
            ldi64 r1, {port_id}
            csrw {CSR_IPIACK}, r1
            halt
        """)
        for _ in range(400):
            if cpu.halted:
                break
            self.system.run_batch(16)
        assert cpu.halted
        start = REPLY - EXT_MEM_BASE + 24
        return bytes(self.system._ext_mem[start:start + 8 * words])


def _tacc_state(cpu):
    return (bytes(cpu.tacc), int(cpu.tacc_owner), bool(cpu.tacc_valid),
            bool(cpu.tacc_dirty), int(cpu.tacc_format_ew),
            bool(cpu.tacc_busy))


# ---------------------------------------------------------------------------
#  The emulator's machine
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    ("format_ew", "image", "cycles"),
    [pytest.param(EW_U8, U8_IMAGE, 7, id="u8-four-units"),
     pytest.param(EW_FP64, FP64_IMAGE, 4, id="fp64-one-unit")],
)
def test_a_store_into_the_window_reaches_the_chiplet(format_ew, image,
                                                     cycles):
    machine = Machine()
    cpu = machine.owned_tacc(bytes([0xEE]) * 256)
    cpu.tdst = STORE_AT
    machine.install("t.acc.store")
    assert cpu.step() == 7
    assert machine.lent(32) == bytes([0xEE]) * 256

    cpu = machine.owned_tacc(image, format_ew)
    cpu.tdst = STORE_AT
    machine.install("t.acc.store")
    # Two base cycles, one per unit sent and one for the answer.
    assert cpu.step() == cycles
    assert not cpu.tacc_dirty and cpu.tacc_valid
    # The chiplet wrote the zeros over the rest of the 256 bytes.
    assert machine.lent(32) == image
    assert cpu.perf_extmem == 0


def test_a_store_into_the_window_in_the_strict_cycle_model():
    machine = Machine()
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = STORE_AT
    code = machine.install("t.acc.store")
    cycles_before = int(cpu.cycle_count)

    # The stage is granted at cycle 1; four units take cycles 2 to 5 and
    # the answer arrives at cycle 6.
    held = machine.system.run_cycle_batch(3, max_instructions=1)
    assert held.instructions_executed == 0
    transport = machine.system._native_system._tacc_transport_snapshot()
    assert transport["stage"]["active"]
    assert transport["stage"]["owner_engine_id"] == 0
    engine = transport["engines"][0]
    assert (engine["link_port"], engine["link_chiplet_addr"],
            engine["link_units"], engine["link_answer_cycle"]) == (
        0, LENT, 4, 6)
    assert transport["port"]["grant_count"] == 0
    assert cpu.tacc_dirty

    retired = machine.system.run_cycle_batch(4, max_instructions=1)
    assert retired.instructions_executed == 1
    assert retired.stop_cycle == 7
    assert int(cpu.cycle_count) - cycles_before == 7
    assert cpu.pc == PROGRAM + len(code)
    assert not cpu.tacc_dirty
    transport = machine.system._native_system._tacc_transport_snapshot()
    assert not transport["stage"]["active"]
    assert transport["engines"][0]["phase"] == "complete"
    assert machine.lent(32) == U8_IMAGE


def test_the_link_store_holds_the_stage_that_every_engine_shares():
    machine = Machine(full_cores=2)
    link_core = machine.owned_tacc(U8_IMAGE, core=0)
    link_core.tdst = STORE_AT
    memory_core = machine.owned_tacc(bytes([0x5A]) * 256, core=1)
    memory_core.tdst = 0x800
    for core in (0, 1):
        machine.install("t.acc.store", core=core)

    machine.system.run_cycle_batch(3, max_instructions=2)
    transport = machine.system._native_system._tacc_transport_snapshot()
    assert transport["stage"]["owner_engine_id"] == 0
    assert transport["engines"][1]["phase"] == "waiting_stage"

    for _ in range(40):
        if link_core.pc != PROGRAM and memory_core.pc != PROGRAM:
            break
        machine.system.run_cycle_batch(4, max_instructions=2)
    transport = machine.system._native_system._tacc_transport_snapshot()
    assert transport["stage"]["grant_count"] == 2
    # The answer at cycle 6 frees the stage; the waiting engine takes it at
    # the next edge.
    assert transport["engines"][1]["stage_grant_cycle"] == 7
    assert bytes(memory_core.mem[0x800:0x900]) == bytes([0x5A]) * 256
    assert machine.lent(32) == U8_IMAGE


@pytest.mark.parametrize("strict", [False, True])
def test_a_closed_port_makes_the_store_a_bus_fault(strict):
    machine = Machine(ctrl=ENABLE | DOORBELL_EN)
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = STORE_AT
    before = _tacc_state(cpu)
    code = machine.install("t.acc.store")
    if strict:
        # The bridge's refusal is the answer, which arrives at cycle 6.
        faulted = machine.system.run_cycle_batch(6, max_instructions=1)
        assert faulted.instructions_executed == 0
        assert faulted.per_core_cycles == (6,)
        assert cpu.pc == PROGRAM + len(code)
    else:
        with pytest.raises(TrapError) as fault:
            cpu.step()
        assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == STORE_AT
    assert _tacc_state(cpu) == before
    assert machine.lent(32) == bytes(256)


def test_a_store_past_the_window_faults_at_its_end():
    machine = Machine()
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = CHIPLET_WINDOW + WINDOW_BYTES - 0xC0
    machine.install("t.acc.store")
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == CHIPLET_WINDOW + WINDOW_BYTES


def test_a_store_beyond_the_ports_built_has_no_route():
    machine = Machine()
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = CHIPLET_WINDOW + WINDOW_BYTES + LENT
    machine.install("t.acc.store")
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == CHIPLET_WINDOW + WINDOW_BYTES + LENT


def test_a_load_from_the_window_is_a_bus_fault():
    machine = Machine()
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tsrc0 = STORE_AT
    before = _tacc_state(cpu)
    machine.install("t.acc.load")
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == STORE_AT
    assert _tacc_state(cpu) == before


def test_user_mode_reaches_the_window_under_its_mpu():
    machine = Machine()
    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = STORE_AT + 0x100
    machine.install("t.acc.store")
    cpu.priv_level = 1
    cpu.mpu_base = CHIPLET_WINDOW
    cpu.mpu_limit = CHIPLET_WINDOW + LENT + 0x200
    # The oracle takes a user-mode TACC operation, and the shared port
    # model carries its write.
    assert cpu.step() == 7
    cpu.priv_level = 0
    assert machine.lent(64)[256:] == U8_IMAGE

    cpu = machine.owned_tacc(U8_IMAGE)
    cpu.tdst = STORE_AT + 0x200
    machine.install("t.acc.store")
    cpu.priv_level = 1
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == STORE_AT + 0x200


# ---------------------------------------------------------------------------
#  The oracle's glue, with hooks of the test's own
# ---------------------------------------------------------------------------

class Port:
    """A stand-in for the shared port model: one port, one window."""

    def __init__(self, answer=OK):
        self.answer = answer
        self.writes = []

    def route(self, addr, size):
        if not CHIPLET_WINDOW <= addr < CHIPLET_WINDOW + WINDOW_BYTES:
            return None
        end = CHIPLET_WINDOW + WINDOW_BYTES
        if addr + size > end:
            return (-1, end)
        return (0, addr - CHIPLET_WINDOW)

    def write(self, port, chiplet_addr, data, zeros):
        self.writes.append((port, chiplet_addr, data, zeros))
        return self.answer


def _oracle(port=None, *, format_ew=EW_U8, image=U8_IMAGE):
    cpu = PythonMegapad64(mem_size=0x1000)
    if port is not None:
        cpu._tacc_window_route_hook = port.route
        cpu._tacc_link_write_hook = port.write
    cpu.tmode = format_ew
    cpu.load_bytes(0, assemble("t.acc.try\nt.acc.clear"))
    cpu.pc = 0
    cpu.step()
    cpu.step()
    cpu.tacc[:] = image
    cpu.tacc_dirty = True
    return cpu


def _step(cpu, source):
    cpu.load_bytes(0x100, assemble(source))
    cpu._icache_invalidate_all(reset_statistics=False)
    cpu.pc = 0x100
    return cpu.step()


def test_the_oracle_sends_the_active_units_through_its_hook():
    port = Port()
    cpu = _oracle(port, format_ew=EW_FP64, image=FP64_IMAGE)
    cpu.tdst = STORE_AT
    assert _step(cpu, "t.acc.store") == 4
    assert port.writes == [(0, LENT, FP64_IMAGE[:64], 3)]
    assert not cpu.tacc_dirty


def test_the_oracle_faults_a_store_the_chiplet_refuses():
    port = Port(answer=REFUSED)
    cpu = _oracle(port)
    cpu.tdst = STORE_AT
    before = (bytes(cpu.tacc), cpu.tacc_valid, cpu.tacc_dirty)
    with pytest.raises(TrapError) as fault:
        _step(cpu, "t.acc.store")
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert fault.value.fault_cycles == 7
    assert cpu.trap_addr == STORE_AT
    assert (bytes(cpu.tacc), cpu.tacc_valid, cpu.tacc_dirty) == before
    assert not cpu.tacc_busy


def test_the_oracle_faults_an_image_past_the_window():
    port = Port()
    cpu = _oracle(port)
    cpu.tdst = CHIPLET_WINDOW + WINDOW_BYTES - 0x40
    with pytest.raises(TrapError) as fault:
        _step(cpu, "t.acc.store")
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == CHIPLET_WINDOW + WINDOW_BYTES
    assert port.writes == []


@pytest.mark.parametrize(
    ("setup", "source"),
    [pytest.param("tsrc0", "t.acc.load", id="load"),
     pytest.param("tsrc1", "t.mac", id="tamac-source")],
)
def test_the_oracle_routes_nothing_but_a_store_into_a_window(setup, source):
    port = Port()
    cpu = _oracle(port)
    setattr(cpu, setup, STORE_AT)
    with pytest.raises(TrapError) as fault:
        _step(cpu, source)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == STORE_AT
    assert port.writes == []


def test_an_oracle_with_no_ports_has_no_windows():
    cpu = _oracle()
    cpu.tdst = STORE_AT
    with pytest.raises(TrapError) as fault:
        _step(cpu, "t.acc.store")
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == STORE_AT
