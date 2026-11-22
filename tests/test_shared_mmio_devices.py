"""The shared mailbox and spinlock models against the emulator's devices.

shared/accel/mailbox.h and shared/accel/spinlock_bank.h are written once
for both backends. The simulator attaches them now, and the emulator moves
onto them from its Python MailboxDevice and SpinlockDevice
(docs/multicore-simulator-plan.md, decision 4). These cases drive the shared
models, as the simulator's extension builds them, and the emulator's
standalone devices with the same seeded streams of requester-tagged byte
accesses, and require every read and every pending bit to agree. Both
extensions must be built before this gate.
"""

from __future__ import annotations

import random

import pytest

import _megaforth_native as native
from emulator.devices import MailboxDevice, SpinlockDevice


MAILBOX_BYTES = 0x10
SPINLOCK_BYTES = 0x40


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
            yield ("write", requester, offset, generator.randrange(participants + 2))
        else:
            yield ("write", requester, offset, generator.randrange(256))


@pytest.mark.parametrize("cores,ports", [(1, 0), (2, 0), (4, 0), (4, 2), (16, 1)])
@pytest.mark.parametrize("seed", range(3))
def test_the_shared_mailbox_answers_as_the_emulators(cores, ports, seed):
    pending = native.MailboxPending(cores, ports)
    shared = native.Mailbox(pending)
    table = native.PlatformTable()
    table.attach("mailbox", shared, [(0x500, 0x510)])
    emulated = MailboxDevice(num_cores=cores, num_ports=ports)
    answered = 0
    for kind, requester, offset, value in _mailbox_stream(seed, cores, ports):
        table.requester = requester
        if kind == "read":
            expected = emulated.read8_for_requester(requester, offset)
            assert table.read8(0x500 + offset) == expected, (requester, offset)
            answered += expected != 0
        else:
            emulated.write8_for_requester(requester, offset, value)
            table.write8(0x500 + offset, value)
        for participant in range(cores + ports):
            assert pending.pending_mask(participant) == emulated.pending[participant]
        for core in range(cores):
            assert shared.data(core) == emulated.data[core]
    assert answered


@pytest.mark.parametrize("cores", [1, 2, 4, 16])
@pytest.mark.parametrize("seed", range(3))
def test_the_shared_spinlocks_answer_as_the_emulators(cores, seed):
    generator = random.Random(seed)
    shared = native.SpinlockBank(core_count=cores)
    emulated = SpinlockDevice(num_cores=cores)
    for _ in range(4_000):
        requester = generator.randrange(cores + 1)
        offset = generator.randrange(SPINLOCK_BYTES)
        if generator.random() < 0.5:
            expected = emulated.read8_for_requester(requester, offset)
            assert shared.read8(0x600 + offset, requester=requester) == expected
        else:
            value = generator.randrange(256)
            emulated.write8_for_requester(requester, offset, value)
            shared.write8(0x600 + offset, value, requester=requester)
        assert shared.owners == tuple(
            None if owner < 0 else owner for owner in emulated.owner)


def test_the_spinlock_window_admits_aligned_accesses_inside_it():
    bank = native.SpinlockBank(core_count=2)
    for offset, width in ((0x600, 1), (0x600, 8), (0x638, 8), (0x63E, 2)):
        bank.preflight(offset, width, write=False)
    for offset, width in ((0x5FF, 1), (0x640, 1), (0x63C, 8), (0x601, 2), (0x600, 3)):
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
