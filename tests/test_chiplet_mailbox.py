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
