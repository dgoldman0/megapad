"""Timed arbitration contract for the seven physical tile engines.

Landing 1.5b exposes a narrow diagnostic seam on ``NativeSystem`` so these
tests can present a complete same-edge candidate set without making host call
order architectural:

* ``_tacc_transport_start(...)`` queues one image transfer;
* ``_tacc_transport_stage_try_grant(cycle)`` arbitrates the shared image;
* ``_tile_memory_port_submit(...)`` queues ordinary 64-byte tile traffic;
* ``_tile_memory_port_try_grant(cycle)`` arbitrates the physical tile port;
* ``_tile_memory_port_complete(...)`` returns its registered acknowledgement;
* ``_tacc_transport_cancel(...)`` cancels one operation token; and
* ``_tacc_transport_{snapshot,restore,reset}`` provide a validated diagnostic
  checkpoint.

The hooks model hardware-visible edges.  They are deliberately not a second
functional execution API, and the older first-caller-wins
``_tacc_image_stage_acquire`` helper is not used as a contention oracle.
Production has seven requestors; configurable verification systems compact
the same contract to their instantiated full-core and cluster engines.
"""

from __future__ import annotations

from copy import deepcopy

import pytest

from megapad64 import EW_U8, EW_U32, TACC_IMAGE_BYTES
from system import HBW_BASE, MegapadSystem


_ENGINE_OWNER_CORE_IDS = (0, 1, 2, 3, 4, 8, 12)
_ENGINE_IDS = tuple(range(7))
_TRANSPORT_FIELDS = {
    "schema_version",
    "engine_count",
    "stage",
    "port",
    "engines",
}


def _system(
    *,
    full_cores: int = 4,
    clusters: int = 3,
) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=4096,
        ext_mem_size=0,
        vram_size=0,
    )


def _owner_core_id(
    system: MegapadSystem,
    engine_id: int,
) -> int:
    if engine_id < system.num_full_cores:
        return engine_id
    cluster_index = engine_id - system.num_full_cores
    return int(system.clusters[cluster_index].cores[0].core_id)


def _epochs(
    system: MegapadSystem,
    engine_id: int,
    owner_core_id: int | None = None,
) -> tuple[int, int]:
    if engine_id < system.num_full_cores:
        return int(system.cores[engine_id].tacc_epoch), 0

    cluster_index = engine_id - system.num_full_cores
    cluster = system.clusters[cluster_index]
    if owner_core_id is None:
        owner_core_id = int(cluster.cores[0].core_id)
    local_core_id = owner_core_id - int(cluster.cores[0].core_id)
    engine_epoch = int(
        cluster._shared_engine_snapshot()["tacc_epoch"]
    )
    caller_epoch = int(
        system._native_system._cluster_tacc_caller_epochs_snapshot(
            cluster_index
        )[local_core_id]
    )
    return engine_epoch, caller_epoch


def _image(engine_id: int, operation_token: int) -> bytes:
    value = 1 + ((engine_id * 17 + operation_token) % 0xFE)
    return bytes([value]) * TACC_IMAGE_BYTES


def _start_store(
    system: MegapadSystem,
    engine_id: int,
    *,
    operation_token: int = 1,
    ready_cycle: int = 0,
) -> dict:
    engine_epoch, caller_epoch = _epochs(system, engine_id)
    return system._native_system._tacc_transport_start(
        engine_id,
        _owner_core_id(system, engine_id),
        "store",
        0x400 + engine_id * 0x100,
        EW_U8,
        False,
        engine_epoch,
        caller_epoch,
        operation_token,
        ready_cycle,
        _image(engine_id, operation_token),
    )


def _start_load(
    system: MegapadSystem,
    engine_id: int,
    *,
    operation_token: int = 1,
    ready_cycle: int = 0,
    format_ew: int = EW_U32,
) -> dict:
    owner_core_id = _owner_core_id(system, engine_id)
    engine_epoch, caller_epoch = _epochs(
        system,
        engine_id,
        owner_core_id,
    )
    return system._native_system._tacc_transport_start(
        engine_id,
        owner_core_id,
        "load",
        0x400 + engine_id * 0x100,
        format_ew,
        False,
        engine_epoch,
        caller_epoch,
        operation_token,
        ready_cycle,
        bytes(TACC_IMAGE_BYTES),
    )


def _cancel(
    system: MegapadSystem,
    engine_id: int,
    *,
    operation_token: int = 1,
) -> bool:
    engine_epoch, caller_epoch = _epochs(system, engine_id)
    return bool(
        system._native_system._tacc_transport_cancel(
            engine_id,
            operation_token,
            engine_epoch,
            caller_epoch,
        )
    )


def _submit_read(
    system: MegapadSystem,
    engine_id: int,
    *,
    operation_token: int,
    ready_cycle: int,
    address: int,
    owner_core_id: int | None = None,
) -> dict:
    if owner_core_id is None:
        owner_core_id = _owner_core_id(system, engine_id)
    engine_epoch, caller_epoch = _epochs(
        system,
        engine_id,
        owner_core_id,
    )
    return system._native_system._tile_memory_port_submit(
        engine_id,
        owner_core_id,
        operation_token,
        engine_epoch,
        caller_epoch,
        ready_cycle,
        address,
        "read",
        b"",
    )


def _grant_engine(grant: dict) -> int:
    return int(grant["request"]["engine_id"])


def _complete_write_grant(native, grant: dict) -> dict:
    return native._tile_memory_port_complete(
        grant["grant_sequence"],
        grant["grant_cycle"] + 1,
    )


def _complete_active_store(
    native,
    *,
    engine_id: int,
    first_port_cycle: int,
) -> int:
    """Complete four uncontended beats and return the terminal ACK cycle."""
    grant_cycle = first_port_cycle
    completion_cycle = first_port_cycle
    for _ in range(4):
        grant = native._tile_memory_port_try_grant(grant_cycle)
        assert grant is not None
        assert _grant_engine(grant) == engine_id
        completion = _complete_write_grant(native, grant)
        assert completion is not None
        completion_cycle = int(completion["completion_cycle"])
        grant_cycle = completion_cycle + 1
    return completion_cycle


def _engine(snapshot: dict, engine_id: int) -> dict:
    matches = [
        engine
        for engine in snapshot["engines"]
        if engine["engine_id"] == engine_id
    ]
    assert len(matches) == 1
    return matches[0]


def test_transport_reset_snapshot_exposes_seven_fixed_engine_ids():
    system = _system()
    snapshot = dict(system._native_system._tacc_transport_snapshot())

    assert set(snapshot) == _TRANSPORT_FIELDS
    assert snapshot["schema_version"] == 1
    assert snapshot["engine_count"] == 7
    assert tuple(
        engine["engine_id"] for engine in snapshot["engines"]
    ) == _ENGINE_IDS
    assert all(
        engine["owner_core_id"] is None
        for engine in snapshot["engines"]
    )
    assert {
        engine["phase"] for engine in snapshot["engines"]
    } == {"idle"}

    stage = snapshot["stage"]
    assert not stage["active"]
    assert stage["owner_engine_id"] is None
    assert stage["last_grant_engine_id"] is None
    assert stage["grant_count"] == 0

    port = snapshot["port"]
    assert all(request is None for request in port["pending"])
    assert port["active_grant"] is None
    assert port["last_grant_engine_id"] is None
    assert port["grant_count"] == 0
    assert tuple(port["grant_counts"]) == (0,) * 7
    assert tuple(port["last_issue_sequences"]) == (0,) * 7


@pytest.mark.parametrize(
    (
        "full_cores",
        "clusters",
        "expected_owner_core_ids",
    ),
    (
        pytest.param(1, 1, (0, 1), id="one-full-one-cluster"),
        pytest.param(3, 0, (0, 1, 2), id="three-private-full"),
        pytest.param(2, 2, (0, 1, 2, 6), id="two-full-two-cluster"),
    ),
)
def test_configurable_topology_compacts_engine_ids_and_owner_mapping(
    full_cores: int,
    clusters: int,
    expected_owner_core_ids: tuple[int, ...],
):
    system = _system(
        full_cores=full_cores,
        clusters=clusters,
    )
    native = system._native_system
    engine_count = full_cores + clusters
    engine_ids = tuple(range(engine_count))
    snapshot = dict(native._tacc_transport_snapshot())

    assert snapshot["engine_count"] == engine_count
    assert tuple(
        engine["engine_id"] for engine in snapshot["engines"]
    ) == engine_ids
    assert len(snapshot["port"]["pending"]) == engine_count
    assert len(snapshot["port"]["grant_counts"]) == engine_count
    assert (
        len(snapshot["port"]["last_issue_sequences"])
        == engine_count
    )
    assert len(snapshot["stage"]["grant_counts"]) == engine_count
    assert tuple(
        _owner_core_id(system, engine_id)
        for engine_id in engine_ids
    ) == expected_owner_core_ids

    ready_cycle = 10
    for engine_id in reversed(engine_ids):
        queued = _start_store(
            system,
            engine_id,
            ready_cycle=ready_cycle,
        )
        assert queued["engine_id"] == engine_id
        assert (
            queued["owner_core_id"]
            == expected_owner_core_ids[engine_id]
        )

    assert native._tacc_transport_stage_try_grant(
        ready_cycle
    ) is None
    first_grant = native._tacc_transport_stage_try_grant(
        ready_cycle + 1
    )
    assert first_grant is not None
    assert first_grant["engine_id"] == 0
    assert first_grant["owner_core_id"] == 0


def test_ordinary_cluster_request_preserves_the_actual_microcaller():
    system = _system(full_cores=2, clusters=2)
    cluster = system.clusters[1]
    caller = cluster.cores[2]
    engine_id = system.num_full_cores + 1

    request = _submit_read(
        system,
        engine_id,
        operation_token=1,
        ready_cycle=5,
        address=0x800,
        owner_core_id=int(caller.core_id),
    )

    assert request["engine_id"] == engine_id
    assert request["owner_core_id"] == caller.core_id
    assert request["caller_epoch"] == (
        system._native_system._cluster_tacc_caller_epochs_snapshot(1)[2]
    )


@pytest.mark.parametrize(
    "submission_order",
    (
        pytest.param((6, 4, 2, 0, 5, 3, 1), id="scrambled"),
        pytest.param((6, 5, 4, 3, 2, 1, 0), id="reverse"),
    ),
)
def test_image_stage_uses_candidate_set_equal_rr_not_host_arrival(
    submission_order: tuple[int, ...],
):
    system = _system()
    native = system._native_system
    ready_cycle = 20

    for engine_id in submission_order:
        queued = _start_store(
            system,
            engine_id,
            ready_cycle=ready_cycle,
        )
        assert queued["phase"] == "waiting_stage"
        assert queued["engine_id"] == engine_id
        assert (
            queued["owner_core_id"]
            == _ENGINE_OWNER_CORE_IDS[engine_id]
        )

    # Every request is captured at R.  Even the first host caller cannot win
    # until the registered arbitration edge at R+1.
    assert native._tacc_transport_stage_try_grant(ready_cycle) is None

    observed = []
    stage_cycle = ready_cycle + 1
    for expected_engine in _ENGINE_IDS:
        grant = native._tacc_transport_stage_try_grant(stage_cycle)
        assert grant is not None
        assert grant["ready_cycle"] == ready_cycle
        assert grant["grant_cycle"] == stage_cycle
        assert grant["engine_id"] == expected_engine
        observed.append(grant["engine_id"])

        stage = native._tacc_transport_snapshot()["stage"]
        assert stage["active"]
        assert stage["owner_engine_id"] == expected_engine
        assert stage["last_grant_engine_id"] == expected_engine
        assert stage["grant_count"] == expected_engine + 1

        completion_cycle = _complete_active_store(
            native,
            engine_id=expected_engine,
            first_port_cycle=stage_cycle + 1,
        )
        stage_cycle = completion_cycle + 1

    assert observed == list(_ENGINE_IDS)

    # The cursor is cyclic, not a fixed priority encoder: after engine 6,
    # simultaneously eligible engines 6 and 0 grant engine 0.
    wrap_ready = stage_cycle
    for engine_id in (6, 0):
        _start_store(
            system,
            engine_id,
            operation_token=2,
            ready_cycle=wrap_ready,
        )
    assert native._tacc_transport_stage_try_grant(wrap_ready) is None
    wrapped = native._tacc_transport_stage_try_grant(wrap_ready + 1)
    assert wrapped is not None
    assert wrapped["engine_id"] == 0


def test_image_stage_tenure_spans_exactly_four_acknowledged_beats():
    system = _system()
    native = system._native_system
    _start_store(system, 0)
    _start_store(system, 3)

    assert native._tacc_transport_stage_try_grant(0) is None
    stage_grant = native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    assert stage_grant["engine_id"] == 0

    port_cycle = 2
    for beat_index in range(1, 5):
        # A ready second transfer cannot replace the owner between beats.
        assert native._tacc_transport_stage_try_grant(port_cycle) is None
        port_grant = native._tile_memory_port_try_grant(port_cycle)
        assert port_grant is not None
        assert _grant_engine(port_grant) == 0
        completion = _complete_write_grant(native, port_grant)
        assert completion is not None

        snapshot = native._tacc_transport_snapshot()
        owner = _engine(snapshot, 0)
        assert owner["beat_index"] == beat_index
        if beat_index < 4:
            assert snapshot["stage"]["active"]
            assert snapshot["stage"]["owner_engine_id"] == 0
            assert snapshot["stage"]["beat_index"] == beat_index
            assert _engine(snapshot, 3)["phase"] == "waiting_stage"
        else:
            assert not snapshot["stage"]["active"]
            assert owner["phase"] == "complete"

        port_cycle = int(completion["completion_cycle"]) + 1

    next_stage = native._tacc_transport_stage_try_grant(port_cycle)
    assert next_stage is not None
    assert next_stage["engine_id"] == 3


def test_independent_tile_port_interleaves_stateless_traffic_between_beats():
    system = _system()
    native = system._native_system
    _start_store(system, 0)
    _start_store(system, 4)

    stage_grant = native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    assert stage_grant["engine_id"] == 0
    assert native._tacc_transport_snapshot()["port"]["grant_count"] == 0

    observed_port_order = []
    tacc_grant = native._tile_memory_port_try_grant(2)
    assert tacc_grant is not None
    observed_port_order.append(_grant_engine(tacc_grant))
    first_completion = _complete_write_grant(native, tacc_grant)
    assert first_completion is not None

    cycle = int(first_completion["completion_cycle"])
    for ordinary_engine, expected_tacc_beat in zip(
        (1, 2, 3),
        (2, 3, 4),
    ):
        _submit_read(
            system,
            ordinary_engine,
            operation_token=1,
            ready_cycle=cycle,
            address=0xC00 + ordinary_engine * 0x40,
        )
        assert native._tile_memory_port_try_grant(cycle) is None

        ordinary_grant = native._tile_memory_port_try_grant(cycle + 1)
        assert ordinary_grant is not None
        assert _grant_engine(ordinary_grant) == ordinary_engine
        observed_port_order.append(_grant_engine(ordinary_grant))
        ordinary_completion = native._tile_memory_port_complete(
            ordinary_grant["grant_sequence"],
            ordinary_grant["grant_cycle"] + 1,
            bytes([ordinary_engine]) * 64,
        )
        assert ordinary_completion is not None

        while_tacc_waits = native._tacc_transport_snapshot()
        assert while_tacc_waits["stage"]["active"]
        assert while_tacc_waits["stage"]["owner_engine_id"] == 0
        assert while_tacc_waits["stage"]["beat_index"] == (
            expected_tacc_beat - 1
        )
        assert native._tacc_transport_stage_try_grant(
            ordinary_completion["completion_cycle"]
        ) is None

        tacc_cycle = int(ordinary_completion["completion_cycle"]) + 1
        tacc_grant = native._tile_memory_port_try_grant(tacc_cycle)
        assert tacc_grant is not None
        assert _grant_engine(tacc_grant) == 0
        observed_port_order.append(_grant_engine(tacc_grant))
        tacc_completion = _complete_write_grant(native, tacc_grant)
        assert tacc_completion is not None
        cycle = int(tacc_completion["completion_cycle"])

    assert observed_port_order == [0, 1, 0, 2, 0, 3, 0]
    snapshot = native._tacc_transport_snapshot()
    assert not snapshot["stage"]["active"]
    assert snapshot["stage"]["last_grant_engine_id"] == 0
    assert snapshot["stage"]["grant_count"] == 1
    assert snapshot["port"]["last_grant_engine_id"] == 0
    assert snapshot["port"]["grant_count"] == 7
    assert tuple(snapshot["port"]["grant_counts"]) == (
        4,
        1,
        1,
        1,
        0,
        0,
        0,
    )

    next_stage = native._tacc_transport_stage_try_grant(cycle + 1)
    assert next_stage is not None
    assert next_stage["engine_id"] == 4
    assert native._tacc_transport_snapshot()["port"]["grant_count"] == 7


@pytest.mark.parametrize(
    "ordinary_first",
    (
        pytest.param(True, id="ordinary-before-tacc"),
        pytest.param(False, id="tacc-before-ordinary"),
    ),
)
def test_same_engine_waiting_stage_and_ordinary_beat_are_order_independent(
    ordinary_first: bool,
):
    system = _system()
    native = system._native_system

    if ordinary_first:
        _submit_read(
            system,
            0,
            operation_token=10,
            ready_cycle=0,
            address=0x800,
        )
        _start_store(system, 0, operation_token=11)
    else:
        _start_store(system, 0, operation_token=11)
        _submit_read(
            system,
            0,
            operation_token=10,
            ready_cycle=0,
            address=0x800,
        )

    stage_grant = native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    assert stage_grant["engine_id"] == 0
    ordinary_grant = native._tile_memory_port_try_grant(1)
    assert ordinary_grant is not None
    assert not ordinary_grant["request"]["image_transfer"]
    ordinary_completion = native._tile_memory_port_complete(
        ordinary_grant["grant_sequence"],
        ordinary_grant["grant_cycle"] + 1,
        bytes(64),
    )
    assert ordinary_completion is not None

    image_grant = native._tile_memory_port_try_grant(
        ordinary_completion["completion_cycle"] + 1
    )
    assert image_grant is not None
    assert image_grant["request"]["image_transfer"]


def test_ineligible_and_cancelled_requests_do_not_consume_rr_turns():
    stage_system = _system()
    stage_native = stage_system._native_system
    _start_store(stage_system, 1, ready_cycle=100)
    _start_store(stage_system, 2, ready_cycle=0)
    _start_store(stage_system, 3, ready_cycle=0)
    assert _cancel(stage_system, 2)

    assert stage_native._tacc_transport_stage_try_grant(0) is None
    stage_grant = stage_native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    assert stage_grant["engine_id"] == 3
    stage = stage_native._tacc_transport_snapshot()["stage"]
    assert stage["last_grant_engine_id"] == 3
    assert stage["grant_count"] == 1

    port_system = _system()
    port_native = port_system._native_system
    _start_store(port_system, 2)
    stage_grant = port_native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    assert stage_grant["engine_id"] == 2
    assert _cancel(port_system, 2)

    cancelled = port_native._tacc_transport_snapshot()
    assert all(
        request is None for request in cancelled["port"]["pending"]
    )
    assert cancelled["port"]["last_grant_engine_id"] is None
    assert cancelled["port"]["grant_count"] == 0

    _submit_read(
        port_system,
        3,
        operation_token=1,
        ready_cycle=100,
        address=0x800,
    )
    _submit_read(
        port_system,
        4,
        operation_token=1,
        ready_cycle=2,
        address=0x840,
    )
    assert port_native._tile_memory_port_try_grant(2) is None
    port_grant = port_native._tile_memory_port_try_grant(3)
    assert port_grant is not None
    assert _grant_engine(port_grant) == 4
    port = port_native._tacc_transport_snapshot()["port"]
    assert port["last_grant_engine_id"] == 4
    assert port["grant_count"] == 1
    assert tuple(port["grant_counts"]) == (
        0,
        0,
        0,
        0,
        1,
        0,
        0,
    )


@pytest.mark.parametrize(
    "address",
    (
        pytest.param(0x400, id="internal-ram"),
        pytest.param(HBW_BASE, id="hbw"),
    ),
)
def test_internal_and_hbw_beats_use_registered_grant_and_ack_timing(
    address: int,
):
    system = _system()
    native = system._native_system
    ready_cycle = 40
    _submit_read(
        system,
        0,
        operation_token=1,
        ready_cycle=ready_cycle,
        address=address,
    )

    assert native._tile_memory_port_try_grant(ready_cycle) is None
    grant = native._tile_memory_port_try_grant(ready_cycle + 1)
    assert grant is not None
    assert grant["request"]["ready_cycle"] == ready_cycle
    assert grant["grant_cycle"] == ready_cycle + 1

    with pytest.raises(ValueError, match="follow its grant by one cycle"):
        native._tile_memory_port_complete(
            grant["grant_sequence"],
            grant["grant_cycle"],
            bytes(64),
        )

    completion = native._tile_memory_port_complete(
        grant["grant_sequence"],
        grant["grant_cycle"] + 1,
        bytes(64),
    )
    assert completion is not None
    assert completion["completion_cycle"] == ready_cycle + 2

    native._tacc_transport_reset()
    transfer_ready = 100
    _start_store(system, 0, ready_cycle=transfer_ready)
    assert native._tacc_transport_stage_try_grant(transfer_ready) is None
    stage_grant = native._tacc_transport_stage_try_grant(
        transfer_ready + 1
    )
    assert stage_grant is not None
    assert stage_grant["grant_cycle"] == transfer_ready + 1

    # The stage grant captures beat zero; the independent physical port still
    # requires its own registered edge before it can grant that beat.
    assert native._tile_memory_port_try_grant(
        stage_grant["grant_cycle"]
    ) is None
    beat_grant = native._tile_memory_port_try_grant(
        stage_grant["grant_cycle"] + 1
    )
    assert beat_grant is not None
    assert beat_grant["request"]["ready_cycle"] == (
        stage_grant["grant_cycle"]
    )
    beat_completion = _complete_write_grant(native, beat_grant)
    assert beat_completion["completion_cycle"] == transfer_ready + 3


def test_u32_load_discards_inactive_image_beats_but_keeps_four_beat_tenure():
    system = _system()
    native = system._native_system
    _start_load(system, 0)
    assert native._tacc_transport_stage_try_grant(1) is not None

    grant_cycle = 2
    for beat_index in range(4):
        grant = native._tile_memory_port_try_grant(grant_cycle)
        assert grant is not None
        completion = native._tile_memory_port_complete(
            grant["grant_sequence"],
            grant["grant_cycle"] + 1,
            bytes([0x40 + beat_index]) * 64,
        )
        assert completion is not None
        grant_cycle = completion["completion_cycle"] + 1

    snapshot = native._tacc_transport_snapshot()
    image = bytes(_engine(snapshot, 0)["image"])
    assert image[:64] == bytes([0x40]) * 64
    assert image[64:128] == bytes([0x41]) * 64
    assert image[128:] == bytes(128)
    assert _engine(snapshot, 0)["beat_index"] == 4
    assert not snapshot["stage"]["active"]


def test_arbitration_cycles_cannot_move_backward():
    system = _system()
    native = system._native_system
    _start_store(system, 0, ready_cycle=5)
    assert native._tacc_transport_stage_try_grant(5) is None
    with pytest.raises(ValueError, match="cannot move backward"):
        native._tacc_transport_stage_try_grant(4)

    native._tacc_transport_reset()
    _submit_read(
        system,
        0,
        operation_token=1,
        ready_cycle=5,
        address=0x800,
    )
    assert native._tile_memory_port_try_grant(5) is None
    with pytest.raises(ValueError, match="cannot move backward"):
        native._tile_memory_port_try_grant(4)


def test_transport_snapshot_restore_is_deep_atomic_and_resettable():
    system = _system()
    native = system._native_system
    _start_store(system, 0)
    _start_store(system, 5)
    stage_grant = native._tacc_transport_stage_try_grant(1)
    assert stage_grant is not None
    active_port_grant = native._tile_memory_port_try_grant(2)
    assert active_port_grant is not None
    _submit_read(
        system,
        3,
        operation_token=1,
        ready_cycle=2,
        address=0x800,
    )

    checkpoint = deepcopy(
        dict(native._tacc_transport_snapshot())
    )
    assert checkpoint["stage"]["owner_engine_id"] == 0
    assert checkpoint["port"]["active_grant"] is not None
    assert _engine(checkpoint, 5)["phase"] == "waiting_stage"

    detached = deepcopy(checkpoint)
    detached["stage"]["owner_engine_id"] = 6
    detached["engines"][0]["phase"] = "complete"
    assert dict(native._tacc_transport_snapshot()) == checkpoint

    completion = _complete_write_grant(native, active_port_grant)
    assert completion is not None
    assert dict(native._tacc_transport_snapshot()) != checkpoint
    native._tacc_transport_restore(checkpoint)
    restored = dict(native._tacc_transport_snapshot())
    restored_grant = restored["port"]["active_grant"]
    assert restored_grant is not None
    assert (
        restored_grant["grant_sequence"]
        != active_port_grant["grant_sequence"]
    )
    semantic_checkpoint = deepcopy(checkpoint)
    semantic_checkpoint["port"]["active_grant"]["grant_sequence"] = (
        restored_grant["grant_sequence"]
    )
    assert restored == semantic_checkpoint

    # A completion from the abandoned post-checkpoint timeline cannot alias
    # the remapped restored grant.
    before_stale_ack = deepcopy(restored)
    assert native._tile_memory_port_complete(
        active_port_grant["grant_sequence"],
        active_port_grant["grant_cycle"] + 1,
    ) is None
    assert dict(native._tacc_transport_snapshot()) == before_stale_ack

    malformed = deepcopy(checkpoint)
    malformed["engine_count"] = 6
    before_rejected_restore = dict(native._tacc_transport_snapshot())
    with pytest.raises(ValueError, match="configured topology"):
        native._tacc_transport_restore(malformed)
    assert (
        dict(native._tacc_transport_snapshot())
        == before_rejected_restore
    )

    native._tacc_transport_reset()
    reset = dict(native._tacc_transport_snapshot())
    assert not reset["stage"]["active"]
    assert reset["stage"]["owner_engine_id"] is None
    assert reset["stage"]["last_grant_engine_id"] is None
    assert reset["stage"]["grant_count"] == 0
    assert all(request is None for request in reset["port"]["pending"])
    assert reset["port"]["active_grant"] is None
    assert reset["port"]["last_grant_engine_id"] is None
    assert reset["port"]["grant_count"] == 0
    assert tuple(reset["port"]["grant_counts"]) == (0,) * 7
    assert tuple(reset["port"]["last_issue_sequences"]) == (0,) * 7
    assert {
        engine["phase"] for engine in reset["engines"]
    } == {"idle"}
