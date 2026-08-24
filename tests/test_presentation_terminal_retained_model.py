"""Focused policy and owner-ledger tests for the RETAINED-1 domain."""

from __future__ import annotations

from types import MappingProxyType

import pytest

from presentation_terminal.apt1 import STRUCTURAL_MAX_PAYLOAD, UINT32_MAX
from presentation_terminal.presentation_model import PresentationGeometry
from presentation_terminal.retained_model import (
    ItemNamespace,
    OwnerDropDisposition,
    OwnerIdentity,
    OwnerLedger,
    OwnerLedgerError,
    OwnerLedgerErrorCode,
    OwnerOpenDisposition,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)


ALL_FEATURES = (
    RetainedFeature.CORE
    | RetainedFeature.VECTOR
    | RetainedFeature.RGBA_IMAGE
    | RetainedFeature.INSTRUMENT
    | RetainedFeature.SERIES
    | RetainedFeature.CADENCE
)


def _policy(**changes) -> RetainedPolicy:
    values = {
        "features": ALL_FEATURES,
        "max_owner_records": 4,
        "max_live_owners": 3,
        "max_regions": 8,
        "max_resources": 4,
        "max_objects": 16,
        "max_series": 4,
        "max_operations_per_transaction": 12,
        "max_resource_chunk_bytes": 64,
        "max_retained_transaction_bytes": 4096,
        "total_resource_bytes": 4096,
        "image_format": 1,
        "max_image_width": 16,
        "max_image_height": 16,
        "max_path_points": 8,
        "max_label_bytes": 32,
        "max_samples_per_append": 4,
        "max_history_per_series": 8,
        "minimum_presentation_interval_us": 16_667,
        "total_sample_slots": 24,
        "total_utf8_bytes": 192,
        "client_to_terminal_max_payload": 256,
        "terminal_to_client_max_payload": 64,
        "base_max_transaction_bytes": 8192,
    }
    values.update(changes)
    return RetainedPolicy(**values)


def _identity(owner_id: int, generation: int = 1) -> OwnerIdentity:
    return OwnerIdentity(0x0123456789ABCDEF, 3, owner_id, generation)


def _quotas(**changes) -> OwnerQuotas:
    values = {
        "regions": 2,
        "resources": 1,
        "objects": 4,
        "series": 1,
        "resource_bytes": 1024,
        "utf8_bytes": 48,
        "sample_slots": 8,
    }
    values.update(changes)
    return OwnerQuotas(**values)


def _ledger(policy: RetainedPolicy | None = None) -> OwnerLedger:
    return OwnerLedger(
        session_id=0x0123456789ABCDEF,
        presentation_epoch=3,
        policy=_policy() if policy is None else policy,
    )


def test_policy_validates_all_advertised_families_and_exact_geometry_bytes():
    policy = _policy()

    # 216 + rows * (52 + 8 * cols), including every complete frame header.
    assert policy.validate_geometry(PresentationGeometry(8, 4, 9)) == 680

    with pytest.raises(ValueError, match="row exceeds inbound payload"):
        policy.validate_geometry(PresentationGeometry(31, 1))

    with pytest.raises(ValueError, match="transaction maximum"):
        _policy(max_retained_transaction_bytes=335)
    with pytest.raises(ValueError, match="INSTRUMENT object"):
        _policy(client_to_terminal_max_payload=135, max_path_points=6)
    with pytest.raises(ValueError, match="resource bytes"):
        _policy(total_resource_bytes=1000)
    with pytest.raises(ValueError, match="between 0 and 1048576"):
        _policy(client_to_terminal_max_payload=STRUCTURAL_MAX_PAYLOAD + 1)
    with pytest.raises(ValueError, match="between 0 and 1048576"):
        _policy(terminal_to_client_max_payload=STRUCTURAL_MAX_PAYLOAD + 1)
    with pytest.raises(ValueError, match=f"between 0 and {UINT32_MAX}"):
        _policy(base_max_transaction_bytes=UINT32_MAX + 1)


def test_policy_rejects_feature_dependency_and_absent_family_capacity():
    with pytest.raises(ValueError, match="SERIES requires INSTRUMENT"):
        _policy(features=RetainedFeature.CORE | RetainedFeature.SERIES)

    with pytest.raises(ValueError, match="must be zero"):
        RetainedPolicy(
            features=RetainedFeature.CORE,
            max_owner_records=1,
            max_live_owners=1,
            max_regions=1,
            max_resources=0,
            max_objects=1,
            max_series=0,
            max_operations_per_transaction=1,
            max_resource_chunk_bytes=0,
            max_retained_transaction_bytes=248,
            total_resource_bytes=0,
            image_format=0,
            max_image_width=0,
            max_image_height=0,
            max_path_points=0,
            max_label_bytes=0,
            max_samples_per_append=0,
            max_history_per_series=0,
            minimum_presentation_interval_us=0,
            total_sample_slots=0,
            total_utf8_bytes=0,
            client_to_terminal_max_payload=64,
            terminal_to_client_max_payload=64,
            base_max_transaction_bytes=248,
        )

    with pytest.raises(TypeError, match="must not be bool"):
        _policy(features=True)


def test_owner_open_reserves_full_quota_and_is_exactly_idempotent():
    ledger = _ledger()
    owner = _identity(10, 7)
    quotas = _quotas()

    before = ledger.state
    prepared = ledger.prepare_open(owner, quotas)
    assert ledger.state is before
    assert prepared.disposition is OwnerOpenDisposition.OPENED

    ledger.install_prepared(prepared)
    opened = ledger.state
    assert ledger.require_live(owner).quotas == quotas
    assert opened.reservations.live_owners == 1
    assert opened.reservations.regions == quotas.regions
    assert opened.reservations.resource_bytes == quotas.resource_bytes
    assert isinstance(opened.records, MappingProxyType)
    with pytest.raises(TypeError):
        opened.records[99] = ledger.require_live(owner)

    assert ledger.open(owner, quotas) is OwnerOpenDisposition.IDEMPOTENT
    assert ledger.state is opened

    with pytest.raises(OwnerLedgerError) as changed:
        ledger.open(owner, _quotas(objects=5))
    assert changed.value.code is OwnerLedgerErrorCode.INVALID
    assert ledger.state is opened


def test_prepared_install_is_bound_to_exact_ledger_and_source_state():
    ledger = _ledger()
    first = ledger.prepare_open(_identity(1), _quotas())
    delayed = ledger.prepare_open(_identity(2), _quotas())
    foreign_ledger = _ledger()
    foreign = foreign_ledger.prepare_open(_identity(3), _quotas())

    ledger.install_prepared(first)
    installed = ledger.state

    with pytest.raises(RuntimeError, match="stale or foreign"):
        ledger.install_prepared(delayed)
    assert ledger.state is installed
    assert ledger.record(2) is None

    with pytest.raises(RuntimeError, match="stale or foreign"):
        ledger.install_prepared(foreign)
    assert ledger.state is installed
    assert ledger.record(3) is None


def test_individually_valid_aggregate_overcommit_is_atomic_no_capacity():
    ledger = _ledger()
    first = _quotas(resource_bytes=2300)
    second = _quotas(resource_bytes=2300)
    ledger.open(_identity(1), first)
    before = ledger.state

    with pytest.raises(OwnerLedgerError) as caught:
        ledger.open(_identity(2), second)

    assert caught.value.code is OwnerLedgerErrorCode.NO_CAPACITY
    assert ledger.state is before
    assert ledger.record(2) is None
    assert ledger.state.reservations.resource_bytes == 2300

    with pytest.raises(OwnerLedgerError) as individually_invalid:
        ledger.open(_identity(3), _quotas(regions=9))
    assert individually_invalid.value.code is OwnerLedgerErrorCode.INVALID
    assert ledger.state is before


def test_exact_scope_generation_drop_tombstone_and_reopen_lifecycle():
    ledger = _ledger()
    owner = _identity(22, 4)
    quotas = _quotas()
    ledger.open(owner, quotas)

    advance = ledger.prepare_item_id(owner, ItemNamespace.OBJECT, 12)
    assert ledger.require_live(owner).high_water.object == 0
    ledger.install_prepared(advance)
    assert ledger.require_live(owner).high_water.object == 12

    before_drop = ledger.state
    prepared = ledger.prepare_drop(owner)
    assert ledger.state is before_drop
    assert prepared.disposition is OwnerDropDisposition.DROPPED
    ledger.install_prepared(prepared)

    tombstone = ledger.record(22)
    assert tombstone is not None and not tombstone.live
    assert tombstone.high_water.object == 12
    assert ledger.state.reservations.live_owners == 0
    assert ledger.drop(owner) is OwnerDropDisposition.IDEMPOTENT

    for generation in (3, 5):
        with pytest.raises(OwnerLedgerError) as stale_drop:
            ledger.drop(_identity(22, generation))
        assert stale_drop.value.code is OwnerLedgerErrorCode.STALE_OWNER

    newer = _identity(22, 5)
    assert ledger.open(newer, quotas) is OwnerOpenDisposition.REOPENED
    assert ledger.require_live(newer).high_water.object == 0
    with pytest.raises(OwnerLedgerError) as old_authority:
        ledger.require_live(owner)
    assert old_authority.value.code is OwnerLedgerErrorCode.STALE_OWNER


def test_item_ids_are_monotonic_per_namespace_and_only_install_consumes():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())

    prepared_object = ledger.prepare_item_id(owner, ItemNamespace.OBJECT, 8)
    assert ledger.require_live(owner).high_water.object == 0
    ledger.install_prepared(prepared_object)

    with pytest.raises(OwnerLedgerError) as duplicate:
        ledger.prepare_item_id(owner, ItemNamespace.OBJECT, 8)
    assert duplicate.value.code is OwnerLedgerErrorCode.DUPLICATE_ID

    prepared_region = ledger.prepare_item_id(owner, ItemNamespace.REGION, 8)
    ledger.install_prepared(prepared_region)
    record = ledger.require_live(owner)
    assert record.high_water.object == 8
    assert record.high_water.region == 8


def test_multiple_item_ids_prepare_and_install_as_one_atomic_candidate():
    ledger = _ledger()
    first = _identity(1)
    second = _identity(2)
    ledger.open(first, _quotas())
    ledger.open(second, _quotas())
    before = ledger.state

    prepared = ledger.prepare_item_ids(
        (
            (first, ItemNamespace.OBJECT, 2),
            (first, ItemNamespace.OBJECT, 5),
            (first, ItemNamespace.REGION, 7),
            (second, ItemNamespace.SERIES, 4),
        )
    )

    assert ledger.state is before
    ledger.install_prepared(prepared)
    assert ledger.require_live(first).high_water.object == 5
    assert ledger.require_live(first).high_water.region == 7
    assert ledger.require_live(second).high_water.series == 4


def test_batch_item_id_duplicate_or_stale_owner_rolls_back_whole_candidate():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    ledger.install_prepared(
        ledger.prepare_item_id(owner, ItemNamespace.OBJECT, 5)
    )
    before = ledger.state

    with pytest.raises(OwnerLedgerError) as duplicate:
        ledger.prepare_item_ids(
            (
                (owner, ItemNamespace.OBJECT, 6),
                (owner, ItemNamespace.OBJECT, 6),
            )
        )
    assert duplicate.value.code is OwnerLedgerErrorCode.DUPLICATE_ID
    assert ledger.state is before
    assert ledger.require_live(owner).high_water.object == 5

    stale = OwnerIdentity(
        owner.session_id, owner.presentation_epoch, owner.owner_id, 2
    )
    with pytest.raises(OwnerLedgerError) as stale_owner:
        ledger.prepare_item_ids(
            (
                (owner, ItemNamespace.REGION, 1),
                (stale, ItemNamespace.SERIES, 1),
            )
        )
    assert stale_owner.value.code is OwnerLedgerErrorCode.STALE_OWNER
    assert ledger.state is before
    assert ledger.require_live(owner).high_water.region == 0


def test_owner_record_capacity_counts_tombstones_without_blocking_new_generation():
    policy = _policy(max_owner_records=2, max_live_owners=2)
    ledger = _ledger(policy)
    first = _identity(1)
    second = _identity(2)
    ledger.open(first, _quotas())
    ledger.drop(first)
    ledger.open(second, _quotas())

    with pytest.raises(OwnerLedgerError) as full:
        ledger.open(_identity(3), _quotas())
    assert full.value.code is OwnerLedgerErrorCode.NO_CAPACITY

    # Reusing the existing record with a strictly newer generation needs no
    # third record and is admitted within the two-live-owner bound.
    assert ledger.open(_identity(1, 2), _quotas()) is OwnerOpenDisposition.REOPENED
    ledger.drop(second)
    with pytest.raises(OwnerLedgerError) as still_full:
        ledger.open(_identity(3), _quotas())
    assert still_full.value.code is OwnerLedgerErrorCode.NO_CAPACITY


def test_owner_scope_includes_session_and_presentation_epoch():
    ledger = _ledger()
    live = _identity(7, 1)
    ledger.open(live, _quotas())

    for stale in (
        OwnerIdentity(live.session_id + 1, live.presentation_epoch, 7, 1),
        OwnerIdentity(live.session_id, live.presentation_epoch + 1, 7, 1),
    ):
        with pytest.raises(OwnerLedgerError) as caught:
            ledger.require_live(stale)
        assert caught.value.code is OwnerLedgerErrorCode.STALE_OWNER


def test_absent_feature_quota_is_rejected_without_mutation():
    core_policy = RetainedPolicy(
        features=RetainedFeature.CORE,
        max_owner_records=2,
        max_live_owners=2,
        max_regions=4,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=2,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=512,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_label_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=64,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=512,
    )
    ledger = _ledger(core_policy)
    before = ledger.state

    with pytest.raises(OwnerLedgerError) as caught:
        ledger.open(_identity(1), _quotas(resources=1))

    assert caught.value.code is OwnerLedgerErrorCode.INVALID
    assert ledger.state is before
