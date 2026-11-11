"""The mailbox's participant table, shared by the emulator and the simulator.

The cases follow rtl/sim/tb_mailbox_ports.v: chiplet ports are mailbox
participants beside the cores, a core's pending mask is indexed by ID, the
IPI line follows every pending bit and the external-flag line follows the
bits that ports set. Both extensions must be built before this gate.
"""

from __future__ import annotations

import importlib

import pytest


PORT0 = 16
PORT1 = 17


@pytest.fixture(params=("_mp64_accel", "_megaforth_native"))
def table_type(request):
    return importlib.import_module(request.param).MailboxPending


@pytest.fixture
def table(table_type):
    """Four cores and two ports at IDs 16 and 17, as in the RTL bench."""
    return table_type(4, 2, PORT0)


def _ipi_lines(table):
    return [table.any_pending(core) for core in range(table.core_count)]


def _ef_lines(table):
    return [table.port_pending(core) for core in range(table.core_count)]


def test_ports_follow_the_cores_unless_placed_higher(table_type):
    packed = table_type(16, 2)
    assert packed.first_port_id == 16
    assert packed.port_id(1) == 17
    one_core = table_type(1, 1)
    assert one_core.first_port_id == 1
    assert table_type(4, 2, PORT0).first_port_id == PORT0


@pytest.mark.parametrize(
    "arguments",
    [(0, 0), (257, 0), (4, -1), (4, 1, 3), (4, 2, 255)],
)
def test_impossible_layouts_are_refused(table_type, arguments):
    with pytest.raises(ValueError):
        table_type(*arguments)


def test_a_core_rings_a_port_and_no_line_rises(table):
    assert table.send(2, PORT0)
    assert table.any_pending(PORT0)
    assert table.lowest_source(PORT0) == 2
    assert _ipi_lines(table) == [False] * 4
    assert _ef_lines(table) == [False] * 4


def test_a_port_is_offered_its_lowest_source_first(table):
    table.send(2, PORT0)
    table.send(0, PORT0)
    assert table.lowest_source(PORT0) == 0
    assert table.acknowledge(PORT0, 0)
    assert table.lowest_source(PORT0) == 2
    assert table.acknowledge(PORT0, 2)
    assert not table.any_pending(PORT0)
    assert table.lowest_source(PORT0) is None


def test_a_port_rings_a_core_on_both_lines(table):
    assert table.send(PORT0, 1)
    assert _ipi_lines(table) == [False, True, False, False]
    assert _ef_lines(table) == [False, True, False, False]
    assert table.pending_mask(1) == 1 << PORT0
    assert table.pending_mask(1) & 0xFF == 0

    assert table.acknowledge(1, PORT0)
    assert _ipi_lines(table) == [False] * 4
    assert _ef_lines(table) == [False] * 4


def test_the_external_flag_follows_the_ports_bits_alone(table):
    table.send(0, 3)
    assert _ipi_lines(table) == [False, False, False, True]
    assert _ef_lines(table) == [False] * 4
    assert table.pending_mask(3) == 0b1

    table.send(PORT1, 3)
    assert table.pending_mask(3) == (1 << PORT1) | 0b1
    assert _ef_lines(table) == [False, False, False, True]

    assert table.acknowledge(3, PORT1)
    assert _ipi_lines(table) == [False, False, False, True]
    assert _ef_lines(table) == [False] * 4
    assert table.acknowledge(3, 0)
    assert _ipi_lines(table) == [False] * 4


def test_ports_ring_each_other(table):
    assert table.send(PORT1, PORT0)
    assert table.lowest_source(PORT0) == PORT1
    assert table.acknowledge(PORT0, PORT1)
    assert not table.any_pending(PORT0)

    assert table.send(1, PORT1)
    assert table.lowest_source(PORT1) == 1
    assert not table.any_pending(PORT0)


@pytest.mark.parametrize("target", [4, 15, 18, 255, -1])
def test_a_signal_that_names_no_participant_is_ignored(table, target):
    assert not table.send(0, target)
    assert not table.send(PORT0, target)
    assert not table.send(target, 0)
    assert table.words() == [0] * 6


def test_a_signal_to_oneself_is_ignored(table):
    assert not table.send(PORT0, PORT0)
    assert not table.send(2, 2)
    assert table.words() == [0] * 6


def test_a_repeated_signal_joins_the_pending_one(table):
    assert table.send(1, PORT0)
    assert table.send(1, PORT0)
    assert table.acknowledge(PORT0, 1)
    assert not table.any_pending(PORT0)
    assert not table.acknowledge(PORT0, 1)


def test_a_signal_sent_after_delivery_stays_pending(table):
    table.send(1, PORT0)
    assert table.acknowledge(PORT0, 1)
    assert table.send(1, PORT0)
    assert table.any_pending(PORT0)
    assert table.acknowledge(PORT0, 1)
    assert not table.any_pending(PORT0)


def test_an_acknowledgement_of_an_absent_signal_changes_nothing(table):
    table.send(0, 1)
    assert not table.acknowledge(1, 2)
    assert not table.acknowledge(1, 99)
    assert table.pending_mask(1) == 0b1


def test_clearing_a_target_drops_its_signals_alone(table):
    table.send(0, PORT0)
    table.send(PORT1, PORT0)
    table.send(PORT0, 2)
    table.clear_target(PORT0)
    assert not table.any_pending(PORT0)
    assert table.pending_mask(2) == 1 << PORT0


def test_the_table_survives_a_save_and_restore(table, table_type):
    table.send(0, PORT0)
    table.send(PORT1, 3)
    saved = table.words()

    restored = table_type(4, 2, PORT0)
    assert restored.restore_words(saved)
    assert restored.lowest_source(PORT0) == 0
    assert restored.pending_mask(3) == 1 << PORT1
    assert not table_type(4, 1, PORT0).restore_words(saved)


def test_ids_from_64_up_are_tracked_outside_the_mask(table_type):
    table = table_type(70, 1)
    assert table.send(69, 0)
    assert table.send(70, 0)
    assert table.pending_mask(0) == 0
    assert table.any_pending(0)
    assert table.port_pending(0)
    assert table.lowest_source(0) == 69
    assert table.acknowledge(0, 69)
    assert table.lowest_source(0) == 70


# ---------------------------------------------------------------------------
#  The table inside the emulator's machine
# ---------------------------------------------------------------------------

from asm import assemble  # noqa: E402
from megapad64 import CSR_IPIACK, CSR_MBOX  # noqa: E402
from system import MegapadSystem  # noqa: E402


_SPIN = assemble("spin:\n    br spin\n")
MBOX_SEND = 0x08
MBOX_STATUS = 0x09
MBOX_ACK = 0x0A


def _machine(*, full_cores=2, clusters=0, **options):
    system = MegapadSystem(
        ram_size=1 << 16,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        vram_size=0,
        **options,
    )
    system.load_binary(0, _SPIN)
    system.boot(entry=0)
    return system


def _run_on(system, core_index, source, *, address=0x200, budget=200):
    """Run *source* on one core while every other core is halted."""
    system.load_binary(address, assemble(source))
    for core in system.cores:
        core.halted = True
        core.idle = False
    core = system.cores[core_index]
    core.pc = address
    core.halted = False
    system.run_batch(budget)
    return core


def test_a_machine_has_one_port_after_its_cores_by_default():
    system = _machine(full_cores=2)
    assert system.num_chiplet_ports == 1
    assert system.first_chiplet_port_id == 2
    assert system.mailbox.num_ports == 1

    clustered = _machine(full_cores=4, clusters=3)
    assert clustered.first_chiplet_port_id == 16

    bare = _machine(full_cores=2, chiplet_ports=0)
    assert bare.num_chiplet_ports == 0
    assert not bare._native_system.ipi_send(0, 2)


def test_port_ids_must_fit_beside_the_cores():
    with pytest.raises(ValueError):
        _machine(full_cores=2, chiplet_ports=255)
    with pytest.raises(ValueError):
        _machine(full_cores=2, chiplet_ports=-1)


def test_a_core_rings_a_port_by_csr_and_the_port_takes_it():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id

    core = _run_on(system, 1, f"""
        ldi64 r1, {port_id}
        csrw {CSR_MBOX}, r1
        halt
    """)
    assert native.chiplet_port_doorbell(0) == 1
    assert not core.irq_ipi
    assert not system.cores[0].irq_ipi
    assert system.mailbox.pending == [0, 0]

    assert not native.chiplet_port_take(0, 0)
    assert native.chiplet_port_take(0, 1)
    assert native.chiplet_port_doorbell(0) is None


def test_a_core_rings_a_port_by_mmio_without_a_data_copy():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id
    system.mailbox.data[0] = 0x1234

    system.mailbox.write8_for_requester(0, MBOX_SEND, port_id)

    assert native.chiplet_port_doorbell(0) == 0
    assert system.mailbox.data == [0x1234, 0]


def test_a_port_is_offered_its_lowest_core_first():
    system = _machine(full_cores=3)
    native = system._native_system
    port_id = system.first_chiplet_port_id
    assert native.ipi_send(2, port_id)
    assert native.ipi_send(0, port_id)
    assert native.chiplet_port_doorbell(0) == 0
    assert native.chiplet_port_take(0, 0)
    assert native.chiplet_port_doorbell(0) == 2


def test_a_ports_signal_shows_at_its_id_in_the_csr_mask():
    system = _machine(full_cores=4, clusters=3)
    native = system._native_system
    assert system.first_chiplet_port_id == 16

    assert native.chiplet_port_send(0, 1)
    assert system.cores[1].irq_ipi
    assert native.ef_line(1)
    assert not native.ef_line(0)
    assert native.ipi_pending_mask(1) == 1 << 16
    # The 8-bit status register holds the pending bits of IDs 0 to 7.
    assert system.mailbox.read8_for_requester(1, MBOX_STATUS) == 0

    core = _run_on(system, 1, f"""
        csrr r4, {CSR_MBOX}
        ldi64 r1, 16
        csrw {CSR_IPIACK}, r1
        csrr r5, {CSR_MBOX}
        halt
    """)
    assert core.regs[4:6] == [1 << 16, 0]
    assert not core.irq_ipi
    assert not native.ef_line(1)


def test_a_core_acknowledges_a_port_by_mmio():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id

    assert native.chiplet_port_send(0, 0)
    assert system.mailbox.read8_for_requester(0, MBOX_STATUS) == 1 << port_id
    system.mailbox.write8_for_requester(0, MBOX_ACK, port_id)

    assert not system.cores[0].irq_ipi
    assert not native.ef_line(0)
    assert native.ipi_pending_mask(0) == 0


def test_the_external_flag_follows_the_ports_signal_alone():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id

    assert native.ipi_send(1, 0)
    assert system.cores[0].irq_ipi
    assert not native.ef_line(0)

    assert native.chiplet_port_send(0, 0)
    assert native.ef_line(0)
    assert native.ipi_pending_mask(0) == (1 << port_id) | 0b10

    assert native.ipi_ack(0, port_id)
    assert not native.ef_line(0)
    assert system.cores[0].irq_ipi
    assert native.ipi_ack(0, 1)
    assert not system.cores[0].irq_ipi


def test_a_ports_repeated_signal_is_one_signal():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id

    assert native.chiplet_port_send(0, 1)
    assert native.chiplet_port_send(0, 1)
    assert native.ipi_ack(1, port_id)
    assert not native.ef_line(1)
    assert not native.ipi_ack(1, port_id)


def test_a_port_reaches_a_microcore():
    system = _machine(full_cores=1, clusters=1)
    native = system._native_system
    assert system.first_chiplet_port_id == 5

    assert native.chiplet_port_send(0, 3)
    assert native.ef_line(3)
    assert native.ipi_pending_mask(3) == 1 << 5
    assert native.ipi_ack(3, 5)
    assert not native.ef_line(3)


@pytest.mark.parametrize("target", [1, 2, 99, -1])
def test_a_ports_signal_to_no_participant_or_itself_is_ignored(target):
    system = _machine(full_cores=1)
    native = system._native_system
    assert system.first_chiplet_port_id == 1
    assert not native.chiplet_port_send(0, target)
    assert not native.chiplet_port_send(1, 0)
    assert native.ipi_pending_snapshot() == [0]


_EF_PROBE = """
    ldi64 r4, 0
    bref flagged
    halt
flagged:
    ldi64 r4, 1
    halt
"""


def test_a_core_tests_a_ports_signal_with_the_flag_branch():
    system = _machine(full_cores=2)
    native = system._native_system
    port_id = system.first_chiplet_port_id

    assert _run_on(system, 1, _EF_PROBE).regs[4] == 0

    assert native.chiplet_port_send(0, 1)
    assert system.cores[1]._cs.chiplet_flag
    assert system.cores[1].eval_cond(0xE)
    assert _run_on(system, 1, _EF_PROBE).regs[4] == 1
    # The branch reads the line and leaves the signal pending.
    assert native.ipi_pending_mask(1) == 1 << port_id

    assert native.ipi_ack(1, port_id)
    assert _run_on(system, 1, _EF_PROBE).regs[4] == 0


def test_a_cores_signal_leaves_the_flag_branch_untaken():
    system = _machine(full_cores=2)
    assert system._native_system.ipi_send(0, 1)
    assert _run_on(system, 1, _EF_PROBE).regs[4] == 0


def test_the_ef_flags_field_still_takes_the_flag_branch():
    system = _machine(full_cores=2)
    system.cores[1].ef_flags = 1
    assert _run_on(system, 1, _EF_PROBE).regs[4] == 1


def test_a_small_core_reads_and_acknowledges_a_ports_signal_by_csr():
    system = _machine(full_cores=1, clusters=1)
    native = system._native_system
    port_id = system.first_chiplet_port_id
    small = 2                               # a small core's ID
    assert native.chiplet_port_send(0, small)

    core = _run_on(system, small, f"""
        csrr r4, {CSR_MBOX}
        ldi64 r1, {port_id}
        csrw {CSR_IPIACK}, r1
        csrr r5, {CSR_MBOX}
        halt
    """)
    assert core.core_id == small
    assert core.regs[4:6] == [1 << port_id, 0]
    assert not native.ef_line(small)
    assert not core.irq_ipi
