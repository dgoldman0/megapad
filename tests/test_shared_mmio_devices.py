"""The shared mailbox and spinlock models, as both backends reach them.

shared/accel/mailbox.h and shared/accel/spinlock_bank.h are written once
(docs/multicore-simulator-plan.md, decision 4). The simulator attaches them
to its platform table, and the emulator's native system owns them, its cores
reaching them directly and the host through the Python bus. These cases
drive both with the same seeded streams of requester-tagged byte accesses
and require every read, every data word and every pending bit to agree with
a reference written here from the devices' register maps (EMULATOR.md,
docs/bios-forth.md). Both extensions must be built before this gate.
"""

from __future__ import annotations

import random

import pytest

import _megaforth_native as native
from asm import assemble
from emulator.devices import MBOX_BASE, SPINLOCK_BASE
from emulator.system import MMIO_START, MegapadSystem
from megapad64 import IVEC_BUS_FAULT, TrapError


MAILBOX_BYTES = 0x10
SPINLOCK_BYTES = 0x40
LOCKS = 16
MASK64 = (1 << 64) - 1
# Full cores, clusters of four microcores, and chiplet ports.
TOPOLOGIES = [(1, 0, 0), (2, 0, 0), (4, 0, 0), (2, 0, 1), (4, 0, 1),
              (4, 3, 1)]


class _ReferenceMailbox:
    """The mailbox's rules, one requester-tagged byte at a time."""

    def __init__(self, cores: int, ports: int):
        self.cores = cores
        self.participants = cores + ports
        self.data = [0] * cores
        self.pending = [0] * self.participants

    def read8(self, requester: int, offset: int) -> int:
        if not 0 <= requester < self.cores:
            return 0
        if offset < 8:
            return (self.data[requester] >> (8 * offset)) & 0xFF
        if offset == 0x09:
            return self.pending[requester] & 0xFF
        return 0

    def write8(self, requester: int, offset: int, value: int) -> None:
        if not 0 <= requester < self.cores:
            return
        if offset < 8:
            shift = 8 * offset
            word = self.data[requester] & ~(0xFF << shift) & MASK64
            self.data[requester] = word | (value << shift)
        elif offset == 0x08:
            if value < self.participants and value != requester:
                if value < self.cores:
                    self.data[value] = self.data[requester]
                self.pending[value] |= 1 << requester
        elif offset == 0x0A and value < self.participants:
            self.pending[requester] &= ~(1 << value)


class _ReferenceSpinlocks:
    """The spinlocks' rules: depthless ownership by the requesting core."""

    def __init__(self, cores: int):
        self.cores = cores
        self.owner = [-1] * LOCKS

    def read8(self, requester: int, offset: int) -> int:
        if offset % 4:
            return 0
        if not 0 <= requester < self.cores:
            return 1
        lock = offset // 4
        if self.owner[lock] in (-1, requester):
            self.owner[lock] = requester
            return 0
        return 1

    def write8(self, requester: int, offset: int) -> None:
        lock = offset // 4
        if offset % 4 == 1 and self.owner[lock] == requester >= 0:
            self.owner[lock] = -1


def _mailbox_stream(seed: int, cores: int, ports: int):
    """Byte accesses that stay inside the window, from every requester and
    from one that names no core, with SEND and ACK naming every ID."""

    generator = random.Random(seed)
    participants = cores + ports
    for _ in range(4_000):
        requester = generator.randrange(cores + 1)
        offset = generator.randrange(MAILBOX_BYTES)
        if generator.random() < 0.45:
            yield ("read", requester, offset, None)
        elif offset in (0x08, 0x0A):
            yield ("write", requester, offset,
                   generator.randrange(participants + 2))
        else:
            yield ("write", requester, offset, generator.randrange(256))


def _emulator(full_cores: int, clusters: int, ports: int) -> MegapadSystem:
    return MegapadSystem(
        ram_size=1 << 16, num_cores=full_cores, num_clusters=clusters,
        hbw_size=0, ext_mem_size=0x4000 if ports else 0, vram_size=0,
        chiplet_ports=ports,
    )


class _SimulatorMailbox:
    def __init__(self, full_cores: int, clusters: int, ports: int):
        cores = full_cores + 4 * clusters
        self.table = native.MailboxPending(cores, ports)
        self.mailbox = native.Mailbox(self.table)
        self.platform = native.PlatformTable()
        self.platform.attach("mailbox", self.mailbox, [(0x500, 0x510)])

    def read8(self, requester, offset):
        self.platform.requester = requester
        return self.platform.read8(0x500 + offset)

    def write8(self, requester, offset, value):
        self.platform.requester = requester
        self.platform.write8(0x500 + offset, value)

    def pending(self, participant):
        return self.table.pending_mask(participant)

    def data(self, core):
        return self.mailbox.data(core)


class _EmulatorMailbox:
    def __init__(self, full_cores: int, clusters: int, ports: int):
        self.system = _emulator(full_cores, clusters, ports)
        self.cores = self.system.num_cores
        assert self.system.first_chiplet_port_id == self.cores

    def read8(self, requester, offset):
        return self.system.bus.read8(
            MBOX_BASE + offset, requester_id=requester)

    def write8(self, requester, offset, value):
        self.system.bus.write8(
            MBOX_BASE + offset, value, requester_id=requester)

    def pending(self, participant):
        if participant < self.cores:
            return self.system.mailbox.pending[participant]
        # A port is offered its lowest pending source alone.
        port = participant - self.cores
        source = self.system._native_system.chiplet_port_doorbell(port)
        return None if source is None else source

    def data(self, core):
        return self.system.mailbox.data[core]


def _lowest(mask: int):
    return (mask & -mask).bit_length() - 1 if mask else None


@pytest.mark.parametrize("backend", [_SimulatorMailbox, _EmulatorMailbox],
                         ids=["simulator", "emulator"])
@pytest.mark.parametrize("full_cores,clusters,ports", TOPOLOGIES)
@pytest.mark.parametrize("seed", range(3))
def test_the_mailbox_answers_by_its_register_map(
        backend, full_cores, clusters, ports, seed):
    cores = full_cores + 4 * clusters
    model = backend(full_cores, clusters, ports)
    reference = _ReferenceMailbox(cores, ports)
    answered = 0
    for kind, requester, offset, value in _mailbox_stream(seed, cores, ports):
        if kind == "read":
            expected = reference.read8(requester, offset)
            assert model.read8(requester, offset) == expected, (
                requester, offset)
            answered += expected != 0
        else:
            reference.write8(requester, offset, value)
            model.write8(requester, offset, value)
        for core in range(cores):
            assert model.pending(core) == reference.pending[core]
            assert model.data(core) == reference.data[core]
    for port in range(cores, cores + ports):
        expected = reference.pending[port]
        if backend is _EmulatorMailbox:
            expected = _lowest(expected)
        assert model.pending(port) == expected
    assert answered


@pytest.mark.parametrize("full_cores,clusters", [(1, 0), (2, 0), (4, 0),
                                                 (2, 1), (4, 3)])
@pytest.mark.parametrize("seed", range(3))
def test_the_spinlocks_answer_by_their_register_map(
        full_cores, clusters, seed):
    cores = full_cores + 4 * clusters
    generator = random.Random(seed)
    simulator = native.SpinlockBank(core_count=cores)
    emulator = _emulator(full_cores, clusters, 0)
    reference = _ReferenceSpinlocks(cores)
    for _ in range(4_000):
        requester = generator.randrange(cores + 1)
        offset = generator.randrange(SPINLOCK_BYTES)
        if generator.random() < 0.5:
            expected = reference.read8(requester, offset)
            assert simulator.read8(0x600 + offset, requester=requester) == (
                expected)
            assert emulator.bus.read8(
                SPINLOCK_BASE + offset, requester_id=requester) == expected
        else:
            value = generator.randrange(256)
            reference.write8(requester, offset)
            simulator.write8(0x600 + offset, value, requester=requester)
            emulator.bus.write8(
                SPINLOCK_BASE + offset, value, requester_id=requester)
        assert simulator.owners == tuple(
            None if owner < 0 else owner for owner in reference.owner)
        assert emulator.spinlock.owner == reference.owner


def _steps(system, source: str, count: int) -> None:
    cpu = system.cpu
    cpu.load_bytes(0, assemble(source))
    cpu.pc = 0
    for _ in range(count):
        cpu.step()


def test_a_native_core_reaches_both_windows_without_the_python_bus():
    system = _emulator(2, 0, 0)
    for device in (system.mailbox, system.spinlock):
        system.bus.devices.remove(device)
    _steps(system, f"""
        ldi64 r1, {MMIO_START + MBOX_BASE}
        ldi64 r2, 0x1122334455667788
        str r1, r2
        ldi r7, 1
        mov r9, r1
        addi r9, 8
        st.b r9, r7
        ldi64 r5, {MMIO_START + SPINLOCK_BASE + 8 * 4}
        ld.b r6, r5
    """, 9)
    assert system.mailbox.data == [0x1122334455667788] * 2
    assert system.mailbox.pending == [0, 1]
    assert system.cores[1].irq_ipi
    assert system.cpu.regs[6] == 0
    assert system.spinlock.owner[8] == 0


@pytest.mark.parametrize(
    ("offset", "operation"),
    [(MBOX_BASE + 1, "ld.h r1, r2"), (MBOX_BASE + 4, "ldn r1, r2"),
     (MBOX_BASE + 0xC, "str r2, r1"), (SPINLOCK_BASE + 0x3C, "ldn r1, r2"),
     (SPINLOCK_BASE + 2, "st.w r2, r1")],
)
def test_a_native_core_faults_on_a_shape_the_window_refuses(
        offset, operation):
    system = _emulator(2, 0, 0)
    address = MMIO_START + offset
    with pytest.raises(TrapError) as fault:
        _steps(system, f"ldi64 r2, {address}\n{operation}\n", 2)
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert system.cpu.trap_addr == address
    assert system.mailbox.pending == [0, 0]
    assert system.spinlock.owner == [-1] * LOCKS


def test_the_spinlock_window_admits_aligned_accesses_inside_it():
    bank = native.SpinlockBank(core_count=2)
    for offset, width in ((0x600, 1), (0x600, 8), (0x638, 8), (0x63E, 2)):
        bank.preflight(offset, width, write=False)
    for offset, width in ((0x5FF, 1), (0x640, 1), (0x63C, 8), (0x601, 2),
                          (0x600, 3)):
        with pytest.raises(ValueError):
            bank.preflight(offset, width, write=True)
    with pytest.raises(ValueError):
        bank.read8(0x640)


def test_the_bank_keeps_the_hosted_api():
    bank = native.SpinlockBank(core_count=2)
    assert (bank.core_count, bank.lock_count) == (2, 16)
    assert bank.acquire(2, 1) == 0
    assert bank.acquire(2, 1) == 0               # depthless reacquisition
    assert bank.acquire(2, 0) == 1
    assert bank.owner(2) == 1
    bank.release(2, 0)                           # not the owner
    assert bank.owner(2) == 1
    bank.release(2, 1)
    assert bank.owner(2) is None
    for bad in (16, -1, 1 << 70):
        with pytest.raises(ValueError):
            bank.acquire(bad, 0)
    with pytest.raises(ValueError):
        bank.acquire(0, 2)
    with pytest.raises(TypeError):
        bank.acquire(True, 0)
    with pytest.raises(ValueError):
        native.SpinlockBank(core_count=0)
