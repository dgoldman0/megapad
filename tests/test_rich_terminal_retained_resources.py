"""Focused state-machine tests for retained RGBA resource authority."""

from __future__ import annotations

import hashlib
from types import MappingProxyType

import pytest

from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_resources import (
    RGBAResource,
    ResourceDeclaration,
    ResourceFormat,
    ResourceStoreError,
    ResourceStoreErrorCode,
    ResourceUsage,
    RetainedResourceStore,
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
        "max_glyph_run_bytes": 32,
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
        "resources": 2,
        "objects": 4,
        "series": 1,
        "resource_bytes": 64,
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


def _begin(
    store: RetainedResourceStore,
    owner: OwnerIdentity,
    resource_id: int,
    data: bytes,
    *,
    width: int = 2,
    height: int = 2,
    flags: int = 0,
    digest: bytes | None = None,
):
    return store.begin(
        owner,
        resource_id=resource_id,
        format=ResourceFormat.RGBA8,
        width=width,
        height=height,
        flags=flags,
        byte_length=len(data),
        digest=hashlib.sha3_256(data).digest() if digest is None else digest,
    )


def _commit_resource(
    store: RetainedResourceStore,
    owner: OwnerIdentity,
    resource_id: int,
    data: bytes,
) -> RGBAResource:
    _begin(store, owner, resource_id, data)
    store.append(owner, resource_id, 0, data)
    return store.commit(owner, resource_id)


def test_ordered_upload_publishes_only_verified_immutable_resource_state():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    initial = store.state

    view = _begin(store, owner, 7, data)

    assert store.state is initial
    assert not store.state.resources
    assert view.accepted_bytes == 0
    assert store.usage(owner) == ResourceUsage(resources=1, bytes=16)
    assert ledger.require_live(owner).high_water.resource == 7

    first = bytearray(data[:5])
    assert store.append(owner, 7, 0, first) == 5
    first[0] = 0xFF
    assert store.state is initial
    assert store.append(owner, 7, 5, memoryview(data)[5:]) == 16

    resource = store.commit(owner, 7)

    assert store.upload is None
    assert isinstance(resource._backing, bytearray)
    assert resource.read(0, len(data)) == data
    assert isinstance(resource.read(0, len(data)), bytes)
    assert resource.digest == hashlib.sha3_256(data).digest()
    assert resource.width == 2
    assert resource.height == 2
    assert store.resource(owner, 7) is resource
    assert store.usage(owner) == ResourceUsage(resources=1, bytes=16)
    assert store.state.usage[(owner.owner_id, owner.owner_generation)] == ResourceUsage(
        resources=1,
        bytes=16,
    )
    assert isinstance(store.state.resources, MappingProxyType)
    assert isinstance(store.state.usage, MappingProxyType)
    with pytest.raises(TypeError):
        store.state.resources[(owner.owner_id, owner.owner_generation, 8)] = resource
    with pytest.raises(TypeError):
        store.state.usage[(owner.owner_id, owner.owner_generation)] = ResourceUsage()


@pytest.mark.parametrize(
    ("termination", "expected_code"),
    (
        ("abort", None),
        ("bad_chunk", ResourceStoreErrorCode.INVALID),
        ("incomplete_commit", ResourceStoreErrorCode.INVALID),
        ("bad_digest", ResourceStoreErrorCode.BAD_CONTENT),
    ),
)
def test_upload_termination_releases_staging_but_consumes_resource_high_water(
    termination,
    expected_code,
):
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    digest = b"\x00" * 32 if termination == "bad_digest" else None
    _begin(store, owner, 5, data, digest=digest)

    if termination == "abort":
        store.append(owner, 5, 0, data[:4])
        store.abort(owner, 5, 0)
    else:
        if termination == "bad_chunk":
            store.append(owner, 5, 0, data[:4])
            operation = lambda: store.append(owner, 5, 6, data[4:8])
        elif termination == "incomplete_commit":
            store.append(owner, 5, 0, data[:4])
            operation = lambda: store.commit(owner, 5)
        else:
            store.append(owner, 5, 0, data)
            operation = lambda: store.commit(owner, 5)
        with pytest.raises(ResourceStoreError) as caught:
            operation()
        assert caught.value.code is expected_code

    assert store.upload is None
    assert not store.state.resources
    assert store.usage(owner) == ResourceUsage()
    assert ledger.require_live(owner).high_water.resource == 5

    with pytest.raises(ResourceStoreError) as duplicate:
        _begin(store, owner, 5, data)
    assert duplicate.value.code is ResourceStoreErrorCode.DUPLICATE_ID
    assert store.upload is None


def test_wrong_upload_owner_or_resource_preserves_the_exact_open_upload():
    ledger = _ledger()
    owner = _identity(1)
    other = _identity(2)
    ledger.open(owner, _quotas())
    ledger.open(other, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    _begin(store, owner, 7, data)
    store.append(owner, 7, 0, data[:4])
    opened = store.upload

    with pytest.raises(ResourceStoreError) as wrong_owner:
        store.append(other, 0, 999, b"")
    assert wrong_owner.value.code is ResourceStoreErrorCode.STALE_OWNER
    assert store.upload == opened

    with pytest.raises(ResourceStoreError) as wrong_resource:
        store.commit(owner, 8)
    assert wrong_resource.value.code is ResourceStoreErrorCode.INVALID
    assert store.upload == opened

    store.append(owner, 7, 4, data[4:])
    assert store.commit(owner, 7).read(0, len(data)) == data


def test_undefined_abort_reason_preserves_upload_until_a_valid_abort():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    _begin(store, owner, 3, data)
    store.append(owner, 3, 0, data[:6])
    opened = store.upload

    with pytest.raises(ResourceStoreError) as caught:
        store.abort(owner, 3, 3)
    assert caught.value.code is ResourceStoreErrorCode.INVALID
    assert store.upload == opened
    assert store.usage(owner) == ResourceUsage(resources=1, bytes=16)

    assert store.abort(owner, 3, 2) == opened
    assert store.upload is None
    assert store.usage(owner) == ResourceUsage()


def test_flags_and_image_policy_fail_before_consuming_resource_id():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas(resource_bytes=256))
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))

    with pytest.raises(ResourceStoreError) as flags:
        _begin(store, owner, 4, data, flags=1)
    assert flags.value.code is ResourceStoreErrorCode.INVALID
    assert ledger.require_live(owner).high_water.resource == 0
    assert store.upload is None

    oversized = bytes(17 * 4)
    with pytest.raises(ResourceStoreError) as policy:
        _begin(store, owner, 4, oversized, width=17, height=1)
    assert policy.value.code is ResourceStoreErrorCode.INVALID
    assert ledger.require_live(owner).high_water.resource == 0
    assert store.upload is None

    _begin(store, owner, 4, data)
    assert ledger.require_live(owner).high_water.resource == 4
    store.abort(owner, 4, 0)


def test_consumed_resource_id_precedes_invalid_replay_content():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    _begin(store, owner, 4, data)
    store.abort(owner, 4, 0)

    with pytest.raises(ResourceStoreError) as duplicate:
        _begin(store, owner, 4, data, flags=1, digest=b"bad")

    assert duplicate.value.code is ResourceStoreErrorCode.DUPLICATE_ID
    assert store.upload is None


def test_owner_count_and_byte_quotas_are_atomic_and_do_not_consume_rejected_ids():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas(resources=1))
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    first = _commit_resource(store, owner, 1, data)

    with pytest.raises(ResourceStoreError) as count_quota:
        _begin(store, owner, 2, data)
    assert count_quota.value.code is ResourceStoreErrorCode.NO_CAPACITY
    assert ledger.require_live(owner).high_water.resource == 1
    assert store.state.resources[(owner.owner_id, owner.owner_generation, 1)] is first

    store.drop(owner, 1, in_use=False)
    _begin(store, owner, 2, data)
    store.abort(owner, 2, 0)

    byte_limited = _identity(2)
    ledger.open(byte_limited, _quotas(resources=1, resource_bytes=15))
    with pytest.raises(ResourceStoreError) as byte_quota:
        _begin(store, byte_limited, 1, data)
    assert byte_quota.value.code is ResourceStoreErrorCode.NO_CAPACITY
    assert ledger.require_live(byte_limited).high_water.resource == 0
    assert store.upload is None


def test_in_use_drop_is_atomic_and_successful_drop_retains_namespace_high_water():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    resource = _commit_resource(store, owner, 9, data)
    before = store.state

    with pytest.raises(ResourceStoreError) as in_use:
        store.drop(owner, 9, in_use=True)
    assert in_use.value.code is ResourceStoreErrorCode.IN_USE
    assert store.state is before
    assert store.resource(owner, 9) is resource

    assert store.drop(owner, 9, in_use=False) is resource
    assert store.usage(owner) == ResourceUsage()
    assert ledger.require_live(owner).high_water.resource == 9
    with pytest.raises(ResourceStoreError) as duplicate:
        _begin(store, owner, 9, data)
    assert duplicate.value.code is ResourceStoreErrorCode.DUPLICATE_ID


def test_owner_retirement_is_exact_provenance_checked_and_stale_safe():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    _commit_resource(store, owner, 1, data)

    stale = store.prepare_owner_retirement(owner)
    _commit_resource(store, owner, 2, data)
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_owner_retirement(stale)
    assert len(store.state.resources) == 2

    ledger.drop(owner)
    foreign_store = RetainedResourceStore(ledger)
    foreign = foreign_store.prepare_owner_retirement(owner)
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_owner_retirement(foreign)
    assert len(store.state.resources) == 2

    with pytest.raises(ResourceStoreError) as wrong_generation:
        store.prepare_owner_retirement(_identity(1, 2))
    assert wrong_generation.value.code is ResourceStoreErrorCode.STALE_OWNER

    before = store.state
    prepared = store.prepare_owner_retirement(owner)
    assert store.state is before
    assert not prepared.state.resources
    assert not prepared.state.usage
    store.install_owner_retirement(prepared)
    assert store.state is prepared.state
    assert not store.state.resources
    assert not store.state.usage


def test_prepared_resource_changes_publish_only_after_exact_install():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))

    begin = store.prepare_begin(
        owner,
        resource_id=1,
        format=ResourceFormat.RGBA8,
        width=2,
        height=2,
        flags=0,
        byte_length=len(data),
        digest=hashlib.sha3_256(data).digest(),
    )
    assert store.upload is None
    assert ledger.require_live(owner).high_water.resource == 0
    store.install_prepared(begin)
    assert store.upload == begin.upload
    assert ledger.require_live(owner).high_water.resource == 1

    first = store.prepare_append(owner, 1, 0, data[:4])
    competing = store.prepare_append(owner, 1, 0, data[:8])
    assert store.upload is not None and store.upload.accepted_bytes == 0
    store.install_prepared(first)
    assert store.upload is not None and store.upload.accepted_bytes == 4
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_prepared(competing)

    with pytest.raises(ResourceStoreError) as rejected:
        store.prepare_append(owner, 1, 7, data[4:8])
    assert rejected.value.code is ResourceStoreErrorCode.INVALID
    assert rejected.value.prepared is not None
    assert store.upload is not None and store.upload.accepted_bytes == 4
    store.install_prepared(rejected.value.prepared)
    assert store.upload is None
    assert store.usage(owner) == ResourceUsage()


def test_prepared_resource_change_is_bound_to_owner_ledger_state():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    prepared = store.prepare_begin(
        owner,
        resource_id=1,
        format=ResourceFormat.RGBA8,
        width=2,
        height=2,
        flags=0,
        byte_length=len(data),
        digest=hashlib.sha3_256(data).digest(),
    )

    ledger.open(_identity(2), _quotas())
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_prepared(prepared)
    assert store.upload is None
    assert ledger.require_live(owner).high_water.resource == 0


def test_public_rgba_resource_construction_cannot_bypass_digest_validation():
    owner = _identity(1)
    data = bytes(range(16))
    valid = ResourceDeclaration(
        1,
        ResourceFormat.RGBA8,
        2,
        2,
        len(data),
        hashlib.sha3_256(data).digest(),
    )
    assert RGBAResource(owner, valid, data).read(0, len(data)) == data

    invalid = ResourceDeclaration(
        1,
        ResourceFormat.RGBA8,
        2,
        2,
        len(data),
        b"\x00" * 32,
    )
    with pytest.raises(ValueError, match="digest is inconsistent"):
        RGBAResource(owner, invalid, data)
    with pytest.raises(ValueError, match="digest is inconsistent"):
        RGBAResource(owner, invalid, data, _verification=object())
    with pytest.raises(TypeError, match="data must be bytes"):
        RGBAResource(owner, valid, bytearray(data))
