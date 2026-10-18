"""Focused state-machine tests for retained RGBA resource authority."""

from __future__ import annotations

import io
from dataclasses import FrozenInstanceError
from types import MappingProxyType

import pytest
from PIL import Image

from rich_terminal.retained_model import (
    IMAGE_FORMATS,
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
        "image_formats": IMAGE_FORMATS,
        "max_image_width": 16,
        "max_image_height": 16,
        "max_path_points": 8,
        "max_glyph_run_bytes": 32,
        "max_samples_per_append": 4,
        "max_history_per_series": 8,
        "minimum_presentation_interval_us": 16_667,
        "total_sample_slots": 24,
        "total_utf8_bytes": 192,
        "total_path_points": 32,
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
    format=ResourceFormat.RGBA8,
):
    return store.begin(
        owner,
        resource_id=resource_id,
        format=format,
        width=width,
        height=height,
        flags=flags,
        byte_length=len(data),
    )


# A 3 by 2 picture with transparency, and its bytes in each file format.
PICTURE = bytes(
    channel
    for pixel in (
        (255, 0, 0, 255), (0, 255, 0, 255), (0, 0, 255, 255),
        (255, 255, 0, 128), (0, 255, 255, 0), (255, 255, 255, 255),
    )
    for channel in pixel
)


def _file(format_name: str, pixels: bytes = PICTURE, size=(3, 2)) -> bytes:
    image = Image.frombytes("RGBA", size, pixels)
    if format_name == "JPEG":
        image = image.convert("RGB")
    out = io.BytesIO()
    image.save(out, format=format_name, **({"quality": 100} if format_name == "JPEG" else {}))
    return out.getvalue()


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
    assert resource.read(0, len(data)) == data
    assert isinstance(resource.read(0, len(data)), bytes)
    readonly = resource._readonly_view()
    assert readonly.readonly
    with pytest.raises(TypeError):
        readonly[0] = 0xFF
    with pytest.raises(TypeError):
        readonly.obj[0] = 0xFF
    readonly.release()
    assert resource.read(0, len(data)) == data
    assert resource.format is ResourceFormat.RGBA8
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
        ("bad_file", ResourceStoreErrorCode.BAD_CONTENT),
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
    # Sixteen bytes that are no QOI file, declared as a 2 by 2 picture.
    file_format = ResourceFormat.QOI if termination == "bad_file" else ResourceFormat.RGBA8
    _begin(store, owner, 5, data, format=file_format)

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
        _begin(store, owner, 4, data, flags=1, format=9)

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
    assert prepared.owner == owner
    with pytest.raises(FrozenInstanceError):
        prepared.owner = _identity(2)
    assert store.state is before
    assert not prepared.state.resources
    assert not prepared.state.usage
    store.install_owner_retirement(prepared)
    assert store.state is prepared.state
    assert prepared._source_state is None
    assert prepared._source_owner_state is None
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
    )
    with pytest.raises(FrozenInstanceError):
        begin.accepted_bytes = 99
    assert store.upload is None
    assert ledger.require_live(owner).high_water.resource == 0
    store.install_prepared(begin)
    assert store.upload == begin.upload
    assert begin._new_upload is None
    assert begin._source_state is None
    assert ledger.require_live(owner).high_water.resource == 1

    first = store.prepare_append(owner, 1, 0, data[:4])
    competing = store.prepare_append(owner, 1, 0, data[:8])
    assert store.upload is not None and store.upload.accepted_bytes == 0
    store.install_prepared(first)
    assert store.upload is not None and store.upload.accepted_bytes == 4
    assert first._chunk is None
    assert first._source_state is None
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_prepared(first)
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_prepared(competing)

    with pytest.raises(ResourceStoreError) as rejected:
        store.prepare_append(owner, 1, 7, data[4:8])
    assert rejected.value.code is ResourceStoreErrorCode.INVALID
    assert rejected.value.prepared is not None
    assert store.upload is not None and store.upload.accepted_bytes == 4
    store.install_prepared(rejected.value.prepared)
    assert store.upload is None
    assert rejected.value.prepared._source_state is None
    assert rejected.value.prepared._new_upload is None
    assert store.usage(owner) == ResourceUsage()


def test_prepared_commit_pixels_are_unreadable_until_writable_alias_is_retired():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = bytes(range(16))
    _begin(store, owner, 1, data)
    store.append(owner, 1, 0, data)

    prepared = store.prepare_commit(owner, 1)
    assert prepared.resource is not None
    with pytest.raises(RuntimeError, match="not committed"):
        prepared.resource.read(0, len(data))
    with pytest.raises(RuntimeError, match="not committed"):
        prepared.resource._readonly_view()

    resource = prepared.resource
    store.install_prepared(prepared)
    assert resource.read(0, len(data)) == data
    assert store.resource(owner, 1) is resource


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
    )

    ledger.open(_identity(2), _quotas())
    with pytest.raises(RuntimeError, match="stale or foreign"):
        store.install_prepared(prepared)
    assert store.upload is None
    assert ledger.require_live(owner).high_water.resource == 0


def test_public_rgba_resource_construction_holds_exactly_the_decoded_pixels():
    owner = _identity(1)
    data = bytes(range(16))
    valid = ResourceDeclaration(1, ResourceFormat.RGBA8, 2, 2, len(data))
    assert RGBAResource(owner, valid, data).read(0, len(data)) == data
    with pytest.raises(ValueError, match="length is inconsistent"):
        RGBAResource(owner, valid, data[:12])
    with pytest.raises(TypeError, match="data must be bytes"):
        RGBAResource(owner, valid, bytearray(data))
    # A file's declaration sends the file but holds its decoded pixels.
    qoi = ResourceDeclaration(2, ResourceFormat.QOI, 2, 2, 9)
    assert (qoi.byte_length, qoi.decoded_bytes) == (9, 16)
    assert RGBAResource(owner, qoi, data).byte_length == 16
    with pytest.raises(ValueError, match="raw resource byte length"):
        ResourceDeclaration(3, ResourceFormat.RGBA8, 2, 2, 9)


@pytest.mark.parametrize("format_name", ("QOI", "PNG", "JPEG"))
def test_a_file_upload_is_decoded_at_commit_into_rgba_pixels(format_name):
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = _file(format_name)

    _begin(store, owner, 1, data, width=3, height=2, format=ResourceFormat[format_name])
    # The owner is charged the decoded picture, not the file.
    assert store.usage(owner) == ResourceUsage(resources=1, bytes=24)
    for offset in range(0, len(data), 64):
        store.append(owner, 1, offset, data[offset:offset + 64])
    resource = store.commit(owner, 1)

    pixels = resource.read(0, 24)
    assert (resource.width, resource.height, resource.byte_length) == (3, 2, 24)
    assert resource.format is ResourceFormat.RGBA8
    if format_name == "JPEG":
        # Lossy, and opaque.
        assert all(pixels[index] == 255 for index in range(3, 24, 4))
    else:
        assert pixels == PICTURE
    assert store.usage(owner) == ResourceUsage(resources=1, bytes=24)
    assert store.drop(owner, 1, in_use=False) is resource
    assert store.usage(owner) == ResourceUsage()


@pytest.mark.parametrize(
    ("declared", "case"),
    (
        ("PNG", "other_size"),
        ("PNG", "other_format"),
        ("PNG", "not_a_picture"),
        ("QOI", "truncated"),
        ("PNG", "truncated"),
        ("JPEG", "truncated"),
    ),
)
def test_a_file_that_is_not_its_declared_picture_fails_with_bad_content(
    declared, case
):
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    data = {
        "other_size": _file(declared, PICTURE[:16], (2, 2)),
        "other_format": _file("QOI"),
        # Cut inside the first pixels; Pillow's QOI decoder fails here
        # with IndexError rather than OSError.
        "truncated": _file(declared)[:16],
        "not_a_picture": b"no picture here",
    }[case]
    _begin(store, owner, 4, data, width=3, height=2,
           format=ResourceFormat[declared])
    for offset in range(0, len(data), 64):
        store.append(owner, 4, offset, data[offset:offset + 64])

    with pytest.raises(ResourceStoreError) as caught:
        store.commit(owner, 4)
    assert caught.value.code is ResourceStoreErrorCode.BAD_CONTENT
    assert caught.value.prepared is None or store.upload is None
    assert store.upload is None
    assert not store.state.resources
    assert store.usage(owner) == ResourceUsage()
    assert ledger.require_live(owner).high_water.resource == 4


def test_only_advertised_formats_are_taken():
    ledger = _ledger(_policy(image_formats=ResourceFormat.RGBA8.bit))
    owner = _identity(1)
    ledger.open(owner, _quotas())
    store = RetainedResourceStore(ledger)
    with pytest.raises(ResourceStoreError) as caught:
        _begin(store, owner, 1, _file("QOI"), width=3, height=2,
               format=ResourceFormat.QOI)
    assert caught.value.code is ResourceStoreErrorCode.INVALID
    assert store.upload is None
    _commit_resource(store, owner, 1, bytes(range(16)))

    for formats, message in (
        (ResourceFormat.QOI.bit, "requires raw RGBA8"),
        (IMAGE_FORMATS | 0x10, "does not define"),
    ):
        with pytest.raises(ValueError, match=message):
            _policy(image_formats=formats)
    with pytest.raises(ValueError, match="zero without RGBA_IMAGE"):
        _policy(
            features=ALL_FEATURES & ~RetainedFeature.RGBA_IMAGE,
            image_formats=1, max_resources=0, max_resource_chunk_bytes=0,
            total_resource_bytes=0, max_image_width=0, max_image_height=0,
        )


def test_a_file_charges_its_decoded_size_against_the_owner_quota():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas(resource_bytes=255))
    store = RetainedResourceStore(ledger)
    # One colour compresses to a few bytes, but decodes to 8 * 8 * 4 = 256.
    data = _file("QOI", bytes((10, 20, 30, 255)) * 64, (8, 8))
    assert len(data) < 64
    with pytest.raises(ResourceStoreError) as caught:
        _begin(store, owner, 1, data, width=8, height=8, format=ResourceFormat.QOI)
    assert caught.value.code is ResourceStoreErrorCode.NO_CAPACITY
    assert ledger.require_live(owner).high_water.resource == 0
