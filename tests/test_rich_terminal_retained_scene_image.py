"""Focused exact-resource graph tests for renderer-neutral IMAGE objects."""

from __future__ import annotations

import hashlib

import pytest

from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    ResourceFormat,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition,
    ImageBody,
    ImageFit,
    ObjectBounds,
    ObjectDefinition,
    ObjectKind,
    RegionDefinition,
    RetainedMode,
    RetainedSceneModel,
    SceneErrorCode,
    SceneModelError,
)
from rich_terminal.update_authority import (
    TerminalGeometry,
    TerminalUpdateAuthority,
    TransactionFamily,
)


SESSION_ID = 0x0123456789ABCDEF
EPOCH = 3
GEOMETRY = TerminalGeometry(8, 4, 0)
RGBA = bytes((12, 34, 56, 255))


def _policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE | RetainedFeature.RGBA_IMAGE,
        max_owner_records=2,
        max_live_owners=2,
        max_regions=4,
        max_resources=6,
        max_objects=8,
        max_series=0,
        max_operations_per_transaction=8,
        max_resource_chunk_bytes=16,
        max_retained_transaction_bytes=1024,
        total_resource_bytes=96,
        image_format=1,
        max_image_width=2,
        max_image_height=2,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=128,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=1024,
    )


def _quotas() -> OwnerQuotas:
    return OwnerQuotas(
        regions=2,
        resources=3,
        objects=4,
        series=0,
        resource_bytes=48,
        utf8_bytes=0,
        sample_slots=0,
    )


def _owner(owner_id: int = 1, generation: int = 1) -> OwnerIdentity:
    return OwnerIdentity(SESSION_ID, EPOCH, owner_id, generation)


def _domain(*, second_owner: bool = False):
    clock = TerminalUpdateAuthority(
        presentation_epoch=EPOCH,
        revision=1,
        transaction_high_water=1,
    )
    owners = OwnerLedger(
        session_id=SESSION_ID,
        presentation_epoch=EPOCH,
        policy=_policy(),
    )
    owner = _owner()
    owners.open(owner, _quotas())
    other = _owner(2)
    if second_owner:
        owners.open(other, _quotas())
    resources = RetainedResourceStore(owners)
    scene = RetainedSceneModel(
        clock=clock,
        owners=owners,
        resources=resources,
        geometry=GEOMETRY,
    )
    return clock, owners, resources, owner, other, scene


def _commit_resource(
    resources: RetainedResourceStore,
    owner: OwnerIdentity,
    resource_id: int,
    data: bytes = RGBA,
) -> None:
    resources.begin(
        owner,
        resource_id=resource_id,
        format=ResourceFormat.RGBA8,
        width=1,
        height=1,
        flags=0,
        byte_length=len(data),
        digest=hashlib.sha3_256(data).digest(),
    )
    resources.append(owner, resource_id, 0, data)
    resources.commit(owner, resource_id)


def _region(
    owner: OwnerIdentity,
    region_id: int = 1,
) -> RegionDefinition:
    return RegionDefinition(owner, region_id, 0, 0, 8, 4, 0, True, True, 0)


def _image(
    owner: OwnerIdentity,
    resource_id: int,
    *,
    object_id: int = 1,
    region_id: int = 1,
) -> ObjectDefinition:
    return ObjectDefinition(
        owner=owner,
        object_id=object_id,
        region_id=region_id,
        parent_object_id=0,
        bounds=ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF),
        z_order=0,
        visible=True,
        body=ImageBody(resource_id, ImageFit.CONTAIN, 192),
    )


def _begin(
    clock: TerminalUpdateAuthority,
    scene: RetainedSceneModel,
    transaction_id: int,
    mode: RetainedMode,
) -> None:
    lease = clock.reserve(TransactionFamily.PRESENT, transaction_id, clock.revision)
    scene.begin(lease, mode, GEOMETRY)


def _install(
    clock: TerminalUpdateAuthority,
    scene: RetainedSceneModel,
    disposition: CommitDisposition,
) -> None:
    prepared = scene.prepare_commit(disposition)
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)


def test_image_is_a_scene_value_and_reference_scan_covers_hidden_then_active():
    clock, _owners, resources, owner, other, scene = _domain()
    _commit_resource(resources, owner, 1)

    image = ImageBody(1, ImageFit.COVER, 127)
    assert ObjectKind.IMAGE == 3
    assert image.fit is ImageFit.COVER
    with pytest.raises(TypeError, match="fit must not be bool"):
        ImageBody(1, True, 255)

    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_object(_image(owner, 1))
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert prepared._resource_state is resources.state
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)

    assert scene.state.hidden is not None
    assert scene.resource_referenced(owner, 1)
    assert not scene.resource_referenced(other, 1)

    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    assert scene.state.hidden is None
    assert scene.resource_referenced(owner, 1)

    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.drop_object(owner, 1)
    _install(clock, scene, CommitDisposition.COMMIT)
    assert not scene.resource_referenced(owner, 1)


def test_image_reference_cannot_borrow_another_exact_owners_resource():
    clock, _owners, resources, owner, other, scene = _domain(second_owner=True)
    _commit_resource(resources, owner, 1)
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(other))

    with pytest.raises(SceneModelError) as caught:
        scene.define_object(_image(other, 1))

    assert caught.value.code is SceneErrorCode.GRAPH
    assert "exact-owner resource" in caught.value.detail
    scene.reject()
    clock.settle_result(2)


@pytest.mark.parametrize("dropped_resource", (1, 2))
def test_prepare_checks_image_references_in_both_projected_scene_planes(
    dropped_resource,
):
    clock, _owners, resources, owner, _other, scene = _domain()
    _commit_resource(resources, owner, 1)
    _commit_resource(resources, owner, 2)

    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_object(_image(owner, 1))
    _install(clock, scene, CommitDisposition.COMMIT)
    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)

    _begin(clock, scene, 4, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner, 2))
    scene.define_object(_image(owner, 2, object_id=2, region_id=2))

    # Simulate a broken lifecycle coordinator: preparation must audit both the
    # unchanged active plane and the new committed-hidden candidate.
    resources.drop(owner, dropped_resource, in_use=False)
    with pytest.raises(SceneModelError) as caught:
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert caught.value.code is SceneErrorCode.GRAPH
    scene.reject()
    clock.settle_result(4)


def test_prepared_scene_is_bound_to_the_exact_resource_store_state():
    clock, _owners, resources, owner, _other, scene = _domain()
    _commit_resource(resources, owner, 1)
    _commit_resource(resources, owner, 2)
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_object(_image(owner, 1))
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)

    resources.drop(owner, 2, in_use=False)
    assert prepared._resource_state is not resources.state
    with pytest.raises(RuntimeError, match="stale or foreign"):
        scene.install_prepared(prepared)

    scene.reject()
    clock.settle_result(2)
