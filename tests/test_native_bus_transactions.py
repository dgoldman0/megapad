"""Native main-bus transaction and arbitration oracles.

These tests select the integrated ``mp64_soc`` contract, not every feature of
the reusable ``mp64_bus`` module.  The SoC presents four full-core ports, three
cluster ports, NIC DMA, disk DMA, and an appended WOTS read requester. Existing
NIC/disk indices stay fixed; WOTS is always the final physical port, with
immutable weight 1 and no bandwidth throttle. Other ports implement the same
1..255 weighted round-robin and 65536-cycle bandwidth epochs as the RTL.

The reset credit, held-request bubble, target decode, and timeout edges below
come from ``rtl/bus/mp64_bus.v``. The sparse oracle covers the non-power-of-two
ten-port wrap without reproducing the old truncated intermediate sum.
"""

from __future__ import annotations

import pytest

import _mp64_accel
from accel_wrapper import NativeSystemState
from system import MegapadSystem


BusFault = _mp64_accel.BusFault
BusOperation = _mp64_accel.BusOperation
BusOrderingMetadata = _mp64_accel.BusOrderingMetadata
BusRequest = _mp64_accel.BusRequest
BusResponseCode = _mp64_accel.BusResponseCode
BusTarget = _mp64_accel.BusTarget
BusWidth = _mp64_accel.BusWidth

MMIO_ADDRESS = 0xFFFF_FF00_0000_0108
MEMORY_ADDRESS = 0x0000_0000_0000_0108


def _request(
    owner,
    requester_id: int,
    issue_sequence: int,
    *,
    ready_cycle: int = 0,
    operation=BusOperation.WRITE,
    address: int = MEMORY_ADDRESS,
    width=BusWidth.BYTE,
    write_data: int = 0,
    port_io: bool = False,
    main_port_id: int | None = None,
):
    if main_port_id is None:
        main_port_id = owner.main_bus_port_for_requester(requester_id)
    return BusRequest(
        requester_id=requester_id,
        ready_cycle=ready_cycle,
        operation=operation,
        address=address,
        width=width,
        ordering=BusOrderingMetadata(
            main_port_id=main_port_id,
            issue_sequence=issue_sequence,
            port_io=port_io,
        ),
        write_data=write_data,
    )


def _advance_to(owner, cycle: int) -> None:
    owner.advance_system_to(cycle)


def _complete_write(
    owner,
    grant,
    cycle: int,
    *,
    read_value=None,
    fault=BusFault.NONE,
    target_effects_committed: bool = True,
):
    _advance_to(owner, cycle)
    return owner._main_bus_complete(
        grant.grant_sequence,
        read_value=read_value,
        fault=fault,
        target_effects_committed=target_effects_committed,
    )


def test_integrated_soc_topology_keeps_requester_identity_separate_from_ports():
    owner = NativeSystemState(4, 16, 10)
    nic = owner.NIC_DMA_REQUESTER_ID
    disk = owner.DISK_DMA_REQUESTER_ID
    wots = owner.WOTS_DMA_REQUESTER_ID

    assert owner.main_bus_port_count == 10
    assert [
        owner.main_bus_port_for_requester(requester)
        for requester in (
            0, 1, 2, 3, 4, 7, 8, 11, 12, 15, nic, disk, wots
        )
    ] == [0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 8, 9]

    # The compatibility constructor can still derive fixed four-core clusters.
    assert NativeSystemState(2, 6).main_bus_port_count == 6
    with pytest.raises(ValueError, match="exactly match"):
        NativeSystemState(4, 16, 9)
    with pytest.raises(ValueError, match="exactly match"):
        NativeSystemState(4, 16, 11)
    with pytest.raises(IndexError, match="outside"):
        owner.main_bus_port_for_requester(16)


def test_request_grant_and_result_preserve_explicit_transaction_metadata():
    owner = NativeSystemState(4, 16, 10)
    request = _request(
        owner,
        12,
        37,
        ready_cycle=5,
        operation=BusOperation.WRITE,
        address=MEMORY_ADDRESS,
        width=BusWidth.WORD,
        write_data=0xFEDC_BA98_7654_3210,
    )

    assert owner._main_bus_next_arbitration_cycle([request]) == 5
    assert owner._main_bus_try_grant([request]) is None
    _advance_to(owner, 5)
    grant = owner._main_bus_try_grant([request])

    assert grant.request is not request
    assert grant.request.requester_id == 12
    assert grant.request.ready_cycle == 5
    assert grant.request.operation == BusOperation.WRITE
    assert grant.request.address == MEMORY_ADDRESS
    assert grant.request.width == BusWidth.WORD
    assert grant.request.write_data == 0xFEDC_BA98_7654_3210
    assert grant.request.ordering.main_port_id == 6
    assert grant.request.ordering.issue_sequence == 37
    assert not grant.request.ordering.port_io
    assert grant.grant_sequence == 1
    assert grant.grant_cycle == 5
    assert grant.target == BusTarget.MEMORY
    assert (
        grant.timeout_cycle
        == 5 + owner.MAIN_BUS_MEMORY_TIMEOUT_CYCLES
    )

    result = _complete_write(owner, grant, 6)
    assert result.grant.grant_sequence == grant.grant_sequence
    assert result.completion_cycle == 6
    assert result.read_value is None
    assert result.fault == BusFault.NONE
    assert result.response_code == BusResponseCode.OK
    assert result.target_effects_committed

    with pytest.raises(AttributeError):
        request.address = 0


def test_reset_credit_and_round_robin_match_the_ten_port_rtl_trace():
    owner = NativeSystemState(4, 16, 10)
    owner.attach_mem(bytearray(0x200), 0x200)
    requesters = [
        0,
        1,
        2,
        3,
        4,
        8,
        12,
        owner.NIC_DMA_REQUESTER_ID,
        owner.DISK_DMA_REQUESTER_ID,
        owner.WOTS_DMA_REQUESTER_ID,
    ]
    pending = [
        _request(
            owner,
            requester,
            1,
            operation=(
                BusOperation.READ
                if requester == owner.WOTS_DMA_REQUESTER_ID
                else BusOperation.WRITE
            ),
        )
        for requester in reversed(requesters)
    ]
    trace = []
    cycle = 0

    for expected_port in range(10):
        grant = owner._main_bus_try_grant(pending)
        trace.append(
            (
                grant.grant_cycle,
                grant.request.ordering.main_port_id,
                grant.request.requester_id,
            )
        )
        assert grant.request.ordering.main_port_id == expected_port
        pending = [
            request
            for request in pending
            if request.ordering.main_port_id != expected_port
        ]
        cycle += 1
        _complete_write(
            owner,
            grant,
            cycle,
            read_value=(
                0
                if grant.request.requester_id
                == owner.WOTS_DMA_REQUESTER_ID
                else None
            ),
        )
        cycle += 1
        _advance_to(owner, cycle)

    # Wrap from appended WOTS port 9 back to core port 0.
    wrapped = _request(owner, 0, 2, ready_cycle=cycle)
    grant = owner._main_bus_try_grant([wrapped])
    trace.append(
        (
            grant.grant_cycle,
            grant.request.ordering.main_port_id,
            grant.request.requester_id,
        )
    )

    assert trace == [
        (0, 0, 0),
        (2, 1, 1),
        (4, 2, 2),
        (6, 3, 3),
        (8, 4, 4),
        (10, 5, 8),
        (12, 6, 12),
        (14, 7, owner.NIC_DMA_REQUESTER_ID),
        (16, 8, owner.DISK_DMA_REQUESTER_ID),
        (18, 9, owner.WOTS_DMA_REQUESTER_ID),
        (20, 0, 0),
    ]


def test_reset_scan_starts_at_port_one_when_port_zero_is_absent():
    owner = NativeSystemState(4, 16, 10)
    port_two = _request(owner, 2, 1)
    port_one = _request(owner, 1, 1)

    grant = owner._main_bus_try_grant([port_two, port_one])

    assert grant.request.ordering.main_port_id == 1


def test_sparse_ten_port_wrap_remains_work_conserving_after_disk():
    owner = NativeSystemState(4, 16, 10)
    disk = owner.DISK_DMA_REQUESTER_ID
    nic = owner.NIC_DMA_REQUESTER_ID

    first_disk = owner._main_bus_try_grant([
        _request(owner, disk, 1)
    ])
    assert first_disk.request.ordering.main_port_id == 8
    _complete_write(owner, first_disk, 1)

    held_disk = _request(owner, disk, 2, ready_cycle=2)
    _advance_to(owner, 2)
    assert owner._main_bus_try_grant([held_disk]) is None
    _advance_to(owner, 3)
    second_disk = owner._main_bus_try_grant([held_disk])
    assert second_disk.request.ordering.main_port_id == 8
    _complete_write(owner, second_disk, 4)

    # From last_grant=8, a lone port-7 request must use otherwise-idle
    # capacity immediately.  No reserved slot or secondary bias may stall it.
    lone_nic = _request(owner, nic, 1, ready_cycle=5)
    _advance_to(owner, 5)
    nic_grant = owner._main_bus_try_grant([lone_nic])
    assert nic_grant.grant_cycle == 5
    assert nic_grant.request.ordering.main_port_id == 7


def test_main_bus_round_robin_state_is_independent_of_core_batch_cursor():
    owner = NativeSystemState(4, 16, 10)
    owner.scheduler_cursor = 3

    grant = owner._main_bus_try_grant([
        _request(owner, 0, 1),
        _request(owner, 1, 1),
    ])

    assert grant.request.ordering.main_port_id == 0
    assert owner.scheduler_cursor == 3


def test_active_grant_holds_payload_and_prevents_preemption():
    owner = NativeSystemState(2, 2, 5)
    first = _request(
        owner,
        0,
        1,
        width=BusWidth.DOUBLEWORD,
        write_data=0x0123_4567_89AB_CDEF,
    )
    later = _request(owner, 1, 1, ready_cycle=1)
    grant = owner._main_bus_try_grant([first])

    _advance_to(owner, 5)
    assert owner._main_bus_try_grant([later]) is None
    assert owner._main_bus_next_arbitration_cycle([later]) is None
    active = owner._main_bus_snapshot().active_grant
    assert active.grant_sequence == grant.grant_sequence
    assert active.request.address == MEMORY_ADDRESS
    assert active.request.width == BusWidth.DOUBLEWORD
    assert active.request.write_data == 0x0123_4567_89AB_CDEF

    owner._main_bus_complete(
        grant.grant_sequence,
        target_effects_committed=True,
    )
    _advance_to(owner, 6)
    later_grant = owner._main_bus_try_grant([later])
    assert later_grant.grant_cycle == 6
    assert later_grant.request.requester_id == 1


def test_same_held_port_gets_one_idle_edge_but_another_port_does_not():
    same_owner = NativeSystemState(1, 1, 4)
    first = same_owner._main_bus_try_grant(
        [_request(same_owner, 0, 1)]
    )
    _complete_write(same_owner, first, 1)
    same_requester = _request(
        same_owner,
        0,
        2,
        ready_cycle=2,
    )

    _advance_to(same_owner, 2)
    assert same_owner._main_bus_try_grant([same_requester]) is None
    snapshot = same_owner._main_bus_snapshot()
    assert not snapshot.served_last
    assert snapshot.earliest_arbitration_cycle == 3
    assert same_owner._main_bus_next_arbitration_cycle([same_requester]) == 3
    _advance_to(same_owner, 3)
    assert (
        same_owner._main_bus_try_grant([same_requester]).grant_cycle
        == 3
    )

    other_owner = NativeSystemState(1, 1, 4)
    first = other_owner._main_bus_try_grant(
        [_request(other_owner, 0, 1)]
    )
    _complete_write(other_owner, first, 1)
    nic_request = _request(
        other_owner,
        other_owner.NIC_DMA_REQUESTER_ID,
        1,
        ready_cycle=2,
    )
    _advance_to(other_owner, 2)
    nic_grant = other_owner._main_bus_try_grant([nic_request])
    assert nic_grant.grant_cycle == 2
    assert nic_grant.request.ordering.main_port_id == 1

    # Requester identity does not bypass the physical-port guard: two
    # micro-cores in one cluster still share main-bus port 4.
    cluster_owner = NativeSystemState(4, 16, 10)
    first_cluster_core = cluster_owner._main_bus_try_grant(
        [_request(cluster_owner, 4, 1)]
    )
    _complete_write(cluster_owner, first_cluster_core, 1)
    next_cluster_core = _request(
        cluster_owner,
        5,
        2,
        ready_cycle=2,
    )
    _advance_to(cluster_owner, 2)
    assert cluster_owner._main_bus_try_grant([next_cluster_core]) is None
    _advance_to(cluster_owner, 3)
    cluster_grant = cluster_owner._main_bus_try_grant([next_cluster_core])
    assert cluster_grant.request.requester_id == 5
    assert cluster_grant.request.ordering.main_port_id == 4


def test_timeout_edges_target_decode_sentinel_and_sticky_fault_match_rtl():
    owner = NativeSystemState(1, 1, 4)
    request = _request(
        owner,
        0,
        1,
        operation=BusOperation.READ,
        address=MMIO_ADDRESS,
    )
    grant = owner._main_bus_try_grant([request])

    assert grant.target == BusTarget.MMIO
    assert grant.timeout_cycle == 64
    assert owner.main_bus_timeout_cycle == 64
    with pytest.raises(ValueError, match="active main bus timeout"):
        owner.advance_system_to(65)
    assert owner.system_cycles == 0
    _advance_to(owner, 63)
    with pytest.raises(ValueError, match="timeout cycle"):
        owner._main_bus_complete(
            grant.grant_sequence,
            fault=BusFault.MMIO_TIMEOUT,
        )
    assert (
        owner._main_bus_snapshot().active_grant.grant_sequence
        == grant.grant_sequence
    )

    _advance_to(owner, 64)
    result = owner._main_bus_complete(
        grant.grant_sequence,
        fault=BusFault.MMIO_TIMEOUT,
    )
    assert result.completion_cycle == 64
    assert result.read_value == owner.MAIN_BUS_TIMEOUT_SENTINEL
    assert result.fault == BusFault.MMIO_TIMEOUT
    assert result.response_code == BusResponseCode.TARGET_FAULT
    assert not result.target_effects_committed
    assert owner.main_bus_timeout_cycle is None
    assert owner._main_bus_snapshot().sticky_bus_errors == [1, 0, 0, 0]

    # The RTL recognizes only address[63:32] == 0xFFFF_FF00 as MMIO.
    memory_owner = NativeSystemState(1, 1, 4)
    memory_request = _request(
        memory_owner,
        0,
        1,
        operation=BusOperation.READ,
        address=0xFFFF_FF01_0000_0108,
    )
    memory_grant = memory_owner._main_bus_try_grant([memory_request])
    assert memory_grant.target == BusTarget.MEMORY
    assert (
        memory_grant.timeout_cycle
        == memory_owner.MAIN_BUS_MEMORY_TIMEOUT_CYCLES
    )


def test_ack_on_terminal_timeout_edge_wins_and_read_completion_is_validated():
    owner = NativeSystemState(1, 1, 4)
    request = _request(
        owner,
        0,
        1,
        operation=BusOperation.READ,
        address=MEMORY_ADDRESS,
        width=BusWidth.HALF,
    )
    grant = owner._main_bus_try_grant([request])

    with pytest.raises(ValueError, match="follow"):
        owner._main_bus_complete(
            grant.grant_sequence,
            read_value=0x1234,
        )
    _advance_to(owner, grant.timeout_cycle)
    with pytest.raises(ValueError, match="requires"):
        owner._main_bus_complete(grant.grant_sequence)
    with pytest.raises(ValueError, match="match"):
        owner._main_bus_complete(
            grant.grant_sequence + 1,
            read_value=0x1234,
        )

    result = owner._main_bus_complete(
        grant.grant_sequence,
        read_value=0x1234,
        target_effects_committed=True,
    )
    assert result.completion_cycle == grant.timeout_cycle
    assert result.read_value == 0x1234
    assert result.fault == BusFault.NONE
    assert result.response_code == BusResponseCode.OK
    assert owner._main_bus_snapshot().sticky_bus_errors == [0, 0, 0, 0]


def test_weighted_round_robin_honors_three_beat_burst_and_weight_255():
    owner = NativeSystemState(2, 2, 5)
    owner._main_bus_set_port_weight(0, 255)
    assert owner._main_bus_snapshot().weights[0] == 255
    owner._main_bus_set_port_weight(0, 3)

    # Move past port 0's special reset credit so the configured burst begins
    # at an ordinary round boundary.
    prime = owner._main_bus_try_grant([_request(owner, 1, 1)])
    assert prime.request.ordering.main_port_id == 1
    _complete_write(owner, prime, 1)
    _advance_to(owner, 2)

    next_issue = {0: 1, 1: 2}
    trace = []
    while len(trace) < 4:
        pending = [
            _request(owner, requester, next_issue[requester])
            for requester in (0, 1)
        ]
        grant = owner._main_bus_try_grant(pending)
        if grant is None:
            _advance_to(owner, owner.system_cycles + 1)
            continue
        requester = grant.request.requester_id
        trace.append(grant.request.ordering.main_port_id)
        next_issue[requester] += 1
        _complete_write(owner, grant, owner.system_cycles + 1)
        if len(trace) < 4:
            _advance_to(owner, owner.system_cycles + 1)

    assert trace == [0, 0, 0, 1]


def test_bandwidth_epoch_and_wots_fixed_qos_are_explicit():
    owner = NativeSystemState(1, 1, 4)
    wots_port = owner.main_bus_port_for_requester(
        owner.WOTS_DMA_REQUESTER_ID
    )

    owner._main_bus_set_port_weight(wots_port, 255)
    owner._main_bus_set_port_bandwidth_limit(wots_port, 0xFFFF)
    owner._main_bus_set_port_bandwidth_limit(0, 1)
    snapshot = owner._main_bus_snapshot()
    assert snapshot.weights == [1, 1, 1, 1]
    assert snapshot.bandwidth_limits == [1, 0, 0, 0]
    assert snapshot.fixed_weight_one_unlimited == [0, 0, 0, 1]

    first = owner._main_bus_try_grant([_request(owner, 0, 1)])
    _complete_write(owner, first, 1)
    snapshot = owner._main_bus_snapshot()
    assert snapshot.bandwidth_counts == [1, 0, 0, 0]

    second = _request(owner, 0, 2, ready_cycle=2)
    assert owner._main_bus_next_arbitration_cycle([second]) == (
        owner.MAIN_BUS_QOS_EPOCH_CYCLES
    )
    _advance_to(owner, owner.MAIN_BUS_QOS_EPOCH_CYCLES)
    grant = owner._main_bus_try_grant([second])
    assert grant.grant_cycle == owner.MAIN_BUS_QOS_EPOCH_CYCLES
    snapshot = owner._main_bus_snapshot()
    assert snapshot.qos_epoch_start_cycle == owner.MAIN_BUS_QOS_EPOCH_CYCLES
    assert snapshot.bandwidth_counts == [0, 0, 0, 0]


def test_wots_bus_validation_and_success_response_classification():
    owner = NativeSystemState(1, 1, 4)
    memory = bytearray(64)
    memory[63] = 0xA5
    owner.attach_mem(memory, len(memory))
    wots = owner.WOTS_DMA_REQUESTER_ID

    invalid = (
        (
            _request(owner, wots, 1, address=0),
            "read-only",
        ),
        (
            _request(
                owner,
                wots,
                1,
                operation=BusOperation.READ,
                address=0,
                width=BusWidth.HALF,
            ),
            "byte-wide",
        ),
        (
            _request(
                owner,
                wots,
                1,
                operation=BusOperation.READ,
                address=len(memory),
            ),
            "Bank 0",
        ),
        (
            _request(
                owner,
                wots,
                1,
                operation=BusOperation.READ,
                address=MMIO_ADDRESS,
            ),
            "Bank 0",
        ),
    )
    for request, message in invalid:
        with pytest.raises(ValueError, match=message):
            owner._main_bus_try_grant([request])

    request = _request(
        owner,
        wots,
        1,
        operation=BusOperation.READ,
        address=63,
    )
    grant = owner._main_bus_try_grant([request])
    _advance_to(owner, 1)
    result = owner._main_bus_complete(
        grant.grant_sequence,
        read_value=memory[63],
        target_effects_committed=True,
    )

    assert result.response_code == BusResponseCode.OK
    assert owner._wots_classify_bus_result(result) == BusResponseCode.OK


def test_wots_response_codes_distinguish_target_timeout_and_protocol():
    def owner_and_grant():
        owner = NativeSystemState(1, 1, 4)
        memory = bytearray(1)
        owner.attach_mem(memory, len(memory))
        grant = owner._main_bus_try_grant([
            _request(
                owner,
                owner.WOTS_DMA_REQUESTER_ID,
                1,
                operation=BusOperation.READ,
                address=0,
            )
        ])
        return owner, grant

    target_owner, target_grant = owner_and_grant()
    _advance_to(target_owner, 1)
    target = target_owner._main_bus_complete(
        target_grant.grant_sequence,
        fault=BusFault.TARGET_FAULT,
    )
    assert target.response_code == BusResponseCode.TARGET_FAULT
    assert target_owner._wots_classify_bus_result(target) == (
        BusResponseCode.TARGET_FAULT
    )
    assert target_owner._main_bus_snapshot().sticky_bus_errors == [
        0, 0, 0, 1
    ]

    timeout_owner, timeout_grant = owner_and_grant()
    _advance_to(timeout_owner, timeout_grant.timeout_cycle)
    timeout = timeout_owner._main_bus_complete(
        timeout_grant.grant_sequence,
        fault=BusFault.MEMORY_TIMEOUT,
    )
    assert timeout.response_code == BusResponseCode.MEMORY_TIMEOUT
    assert timeout_owner._wots_classify_bus_result(timeout) == (
        BusResponseCode.MEMORY_TIMEOUT
    )

    protocol_owner, protocol_grant = owner_and_grant()
    _advance_to(protocol_owner, 1)
    protocol = protocol_owner._main_bus_complete(
        protocol_grant.grant_sequence,
        read_value=0,
        target_effects_committed=False,
    )
    assert protocol.response_code == BusResponseCode.OK
    assert protocol_owner._wots_classify_bus_result(protocol) == (
        BusResponseCode.PROTOCOL
    )


def test_invalid_port_snapshots_and_replayed_sequences_are_transactional():
    owner = NativeSystemState(4, 16, 10)
    core_four = _request(owner, 4, 1)
    core_five = _request(owner, 5, 2)
    baseline = owner._main_bus_snapshot()

    with pytest.raises(ValueError, match="duplicate ports"):
        owner._main_bus_try_grant([core_four, core_five])
    with pytest.raises(ValueError, match="physical port"):
        owner._main_bus_try_grant([
            _request(owner, 4, 1, main_port_id=5)
        ])
    with pytest.raises(ValueError, match="only a full core"):
        owner._main_bus_try_grant([
            _request(owner, 4, 1, port_io=True)
        ])
    with pytest.raises(ValueError, match="byte-wide"):
        owner._main_bus_try_grant([
            _request(
                owner,
                owner.NIC_DMA_REQUESTER_ID,
                1,
                width=BusWidth.HALF,
            )
        ])

    unchanged = owner._main_bus_snapshot()
    assert unchanged.next_grant_sequence == baseline.next_grant_sequence
    assert unchanged.active_grant is None
    assert unchanged.last_issue_sequences == baseline.last_issue_sequences

    first = owner._main_bus_try_grant([_request(owner, 0, 1)])
    _complete_write(owner, first, 1)
    _advance_to(owner, 2)
    with pytest.raises(ValueError, match="must advance"):
        owner._main_bus_try_grant([_request(owner, 0, 1)])


def test_versioned_reset_snapshot_replays_the_initial_tie_deterministically():
    owner = NativeSystemState(2, 2, 5)
    first = owner._main_bus_try_grant([
        _request(owner, 1, 1),
        _request(owner, 0, 1),
    ])
    _complete_write(owner, first, 1)

    owner._main_bus_reset()
    snapshot = owner._main_bus_snapshot()
    assert snapshot.schema_version == 2
    assert snapshot.port_count == 5
    assert snapshot.last_grant == 0
    assert snapshot.reset_port_zero_credit
    assert snapshot.next_grant_sequence == 1
    assert snapshot.earliest_arbitration_cycle == 1
    assert not snapshot.served_last
    assert snapshot.last_arbitration_cycle is None
    assert snapshot.active_grant is None
    assert snapshot.last_issue_sequences == [0, 0, 0, 0, 0]
    assert snapshot.sticky_bus_errors == [0, 0, 0, 0, 0]
    assert snapshot.weights == [1, 1, 1, 1, 1]
    assert snapshot.bandwidth_limits == [0, 0, 0, 0, 0]
    assert snapshot.bandwidth_counts == [0, 0, 0, 0, 0]
    assert snapshot.fixed_weight_one_unlimited == [0, 0, 0, 0, 1]
    assert snapshot.weight_remaining == 1
    assert snapshot.qos_epoch_start_cycle == 1

    replay = owner._main_bus_try_grant([
        _request(owner, 1, 1, ready_cycle=1),
        _request(owner, 0, 1, ready_cycle=1),
    ])
    assert replay.grant_sequence == 1
    assert replay.grant_cycle == 1
    assert replay.request.ordering.main_port_id == 0


def test_cold_boot_resets_owned_main_bus_state():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    owner = system._native_system
    active = owner._main_bus_try_grant([_request(owner, 0, 1)])
    assert active is not None
    assert owner._main_bus_snapshot().active_grant is not None

    system.boot(entry=0)

    snapshot = owner._main_bus_snapshot()
    assert snapshot.active_grant is None
    assert snapshot.next_grant_sequence == 1
    assert snapshot.reset_port_zero_credit
    assert snapshot.sticky_bus_errors == [0, 0, 0, 0]


@pytest.mark.parametrize(
    "execute",
    [
        lambda system: system.step(),
        lambda system: system.run_batch_stats(1),
    ],
    ids=["step", "run_batch_stats"],
)
def test_execution_rejects_active_grant_before_guest_mutation(execute):
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, b"\x11")  # INC R1
    system.boot(entry=0)
    owner = system._native_system
    grant = owner._main_bus_try_grant([
        _request(
            owner,
            0,
            1,
            operation=BusOperation.READ,
            address=MEMORY_ADDRESS,
        )
    ])
    before = (
        system.cpu.pc,
        system.cpu.regs[1],
        system.cpu.cycle_count,
        owner.native_batch_runs,
        bytes(system.cpu.mem),
    )

    with pytest.raises(
        RuntimeError,
        match="active main-bus grants require cycle-bounded native execution",
    ):
        execute(system)

    assert (
        system.cpu.pc,
        system.cpu.regs[1],
        system.cpu.cycle_count,
        owner.native_batch_runs,
        bytes(system.cpu.mem),
    ) == before
    assert (
        owner._main_bus_snapshot().active_grant.grant_sequence
        == grant.grant_sequence
    )


def test_direct_native_batch_rejects_active_grant_before_guest_mutation():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, b"\x11")  # INC R1
    system.boot(entry=0)
    owner = system._native_system
    grant = owner._main_bus_try_grant([
        _request(
            owner,
            0,
            1,
            operation=BusOperation.READ,
            address=MEMORY_ADDRESS,
        )
    ])
    cpu = system.cpu
    callback_sets = [(
        cpu._mmio_read8,
        cpu._mmio_write8,
        cpu._do_output,
        getattr(cpu, "_csr_read_override", None),
    )]
    before = (
        cpu.pc,
        cpu.regs[1],
        cpu.cycle_count,
        owner.native_batch_runs,
        bytes(cpu.mem),
    )

    with pytest.raises(
        RuntimeError,
        match="active main-bus grants require cycle-bounded native execution",
    ):
        owner.run_full_core_batch(
            1,
            callback_sets,
            system._prepare_native_system_batch,
            system._settle_native_core_continuation,
            system._settle_native_core_dispatch_error,
            system._settle_native_system_round,
            1000,
            False,
        )

    assert (
        cpu.pc,
        cpu.regs[1],
        cpu.cycle_count,
        owner.native_batch_runs,
        bytes(cpu.mem),
    ) == before
    assert (
        owner._main_bus_snapshot().active_grant.grant_sequence
        == grant.grant_sequence
    )
