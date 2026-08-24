"""Focused immutable scene-target tests for RETAINED-1."""

from __future__ import annotations

import pytest

from presentation_terminal.presentation_model import (
    PresentationClock,
    PresentationGeometry,
    TransactionFamily,
)
from presentation_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from presentation_terminal.retained_scene import (
    CommitDisposition,
    GroupBody,
    LabelBody,
    MeterBody,
    ObjectBounds,
    ObjectDefinition,
    PlotBody,
    Point,
    PolylineBody,
    RGBA,
    RebuildRequirement,
    ReadoutBody,
    ReadoutFormat,
    RegionDefinition,
    RetainedMode,
    RetainedSceneModel,
    SceneErrorCode,
    SceneModelError,
    SeriesDefinition,
    StatusBody,
    TimestampMode,
    WaveformBody,
)


SESSION = 0x0123456789ABCDEF
GEOMETRY = PresentationGeometry(20, 10, 0)
WHITE = RGBA(255, 255, 255, 255)
BLACK = RGBA(0, 0, 0, 255)
GREEN = RGBA(32, 220, 96, 255)


def _policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=(
            RetainedFeature.CORE
            | RetainedFeature.VECTOR
            | RetainedFeature.INSTRUMENT
            | RetainedFeature.SERIES
            | RetainedFeature.CADENCE
        ),
        max_owner_records=4,
        max_live_owners=4,
        max_regions=12,
        max_resources=0,
        max_objects=32,
        max_series=8,
        max_operations_per_transaction=16,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=4096,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=16,
        max_label_bytes=64,
        max_samples_per_append=8,
        max_history_per_series=16,
        minimum_presentation_interval_us=16_667,
        total_sample_slots=64,
        total_utf8_bytes=256,
        client_to_terminal_max_payload=512,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=8192,
    )


def _owner(owner_id: int = 7, generation: int = 2) -> OwnerIdentity:
    return OwnerIdentity(SESSION, 3, owner_id, generation)


def _quotas(**changes) -> OwnerQuotas:
    values = {
        "regions": 4,
        "resources": 0,
        "objects": 16,
        "series": 4,
        "resource_bytes": 0,
        "utf8_bytes": 128,
        "sample_slots": 32,
    }
    values.update(changes)
    return OwnerQuotas(**values)


def _domain(*, quotas: OwnerQuotas | None = None):
    clock = PresentationClock(
        presentation_epoch=3, revision=1, transaction_high_water=1
    )
    owners = OwnerLedger(
        session_id=SESSION, presentation_epoch=3, policy=_policy()
    )
    identity = _owner()
    owners.open(identity, _quotas() if quotas is None else quotas)
    scene = RetainedSceneModel(clock=clock, owners=owners, geometry=GEOMETRY)
    return clock, owners, identity, scene


def _region(owner: OwnerIdentity, *, generation: int = 0) -> RegionDefinition:
    return RegionDefinition(owner, 1, 0, 0, 20, 10, 0, True, True, generation)


def _object(owner: OwnerIdentity, object_id: int, body, *, parent: int = 0):
    return ObjectDefinition(
        owner=owner,
        object_id=object_id,
        region_id=1,
        parent_object_id=parent,
        bounds=ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF),
        z_order=object_id,
        visible=True,
        body=body,
    )


def _begin(clock, scene, transaction_id: int, mode: RetainedMode, geometry=GEOMETRY):
    lease = clock.reserve(TransactionFamily.PRESENT, transaction_id, clock.revision)
    scene.begin(lease, mode, geometry)
    return lease


def _install(scene, clock, disposition: CommitDisposition):
    prepared = scene.prepare_commit(disposition)
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    return result


def _stage_soundlab_target(scene: RetainedSceneModel, owner: OwnerIdentity, points):
    scene.define_region(_region(owner))
    scene.define_series(SeriesDefinition(owner, 1, 8, TimestampMode.EXPLICIT, 0))
    scene.define_series(SeriesDefinition(owner, 2, 8, TimestampMode.UNIFORM, 1000))
    scene.define_object(_object(owner, 1, GroupBody()))
    scene.define_object(
        _object(owner, 2, PolylineBody(points, 0x01000000, GREEN), parent=1)
    )
    scene.define_object(_object(owner, 3, LabelBody(WHITE, 0, 1, "Level"), parent=1))
    scene.define_object(
        _object(
            owner,
            4,
            ReadoutBody(WHITE, BLACK, ReadoutFormat.FIXED, 1, -125, 10, " dB"),
            parent=1,
        )
    )
    scene.define_object(
        _object(owner, 5, MeterBody(GREEN, BLACK, False, True, -600, 0, -125), parent=1)
    )
    scene.define_object(_object(owner, 6, StatusBody(BLACK, GREEN, 1, 0), parent=1))
    scene.define_object(
        _object(owner, 7, PlotBody(1, -600, 0, GREEN, BLACK, True, False), parent=1)
    )
    scene.define_object(
        _object(owner, 8, WaveformBody(2, -32768, 32767, WHITE, BLACK, 0, True), parent=1)
    )


def test_real_soundlab_definition_target_is_hidden_then_atomically_revealed():
    clock, owners, owner, scene = _domain()
    caller_points = [Point(0, 0), Point(0xFFFFFFFF, 0xFFFFFFFF)]
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    _stage_soundlab_target(scene, owner, caller_points)

    before = scene.state
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is before
    assert owners.require_live(owner).high_water.object == 0
    result = scene.install_prepared(prepared)
    assert result.revision == 2
    clock.settle_result(2)

    hidden = scene.state.hidden
    assert hidden is not None
    assert not scene.state.retained_visible
    assert not scene.state.active.owners
    owner_scene = hidden.owners[owner.owner_id]
    assert owner_scene.usage.regions == 1
    assert owner_scene.usage.objects == 8
    assert owner_scene.usage.series == 2
    assert owner_scene.usage.utf8_bytes == len("Level".encode()) + len("-12.5 dB".encode())
    assert owner_scene.usage.sample_slots == 16
    assert owners.require_live(owner).high_water.region == 1
    assert owners.require_live(owner).high_water.object == 8
    assert owners.require_live(owner).high_water.series == 2

    # The caller list is not retained and the committed mappings reject writes.
    caller_points.append(Point(1, 1))
    polyline = owner_scene.objects[2].body
    assert isinstance(polyline, PolylineBody) and len(polyline.points) == 2
    with pytest.raises(TypeError):
        hidden.owners[99] = owner_scene

    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)
    assert scene.state.revision == 3
    assert scene.state.retained_visible
    assert scene.state.active is hidden
    assert scene.state.hidden is None


def test_dependency_or_quota_rejection_leaves_scene_and_id_ledger_unchanged():
    clock, owners, owner, scene = _domain(quotas=_quotas(objects=1))
    initial_scene = scene.state
    initial_ledger = owners.state
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)

    with pytest.raises(SceneModelError) as missing_region:
        scene.define_object(_object(owner, 1, GroupBody()))
    assert missing_region.value.code is SceneErrorCode.GRAPH
    assert scene.state is initial_scene
    assert owners.state is initial_ledger
    result = scene.reject()
    assert (result.revision, result.succeeded) == (1, False)
    clock.settle_result(2)

    _begin(clock, scene, 3, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_object(_object(owner, 1, GroupBody()))
    with pytest.raises(SceneModelError) as over_quota:
        scene.define_object(_object(owner, 2, LabelBody(WHITE, 0, 0, "extra")))
    assert over_quota.value.code is SceneErrorCode.QUOTA
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is initial_scene
    assert owners.state is initial_ledger
    assert owners.require_live(owner).high_water.object == 0
    scene.reject()
    clock.settle_result(3)


def test_replace_start_cannot_skip_the_required_continue_reveal_boundary():
    clock, owners, owner, scene = _domain()
    initial_scene = scene.state
    initial_ledger = owners.state
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))

    with pytest.raises(SceneModelError, match="only CONTINUE may reveal"):
        scene.prepare_commit(CommitDisposition.COMMIT_AND_REVEAL)

    assert scene.state is initial_scene
    assert owners.state is initial_ledger
    assert owners.require_live(owner).high_water.region == 0
    scene.reject()
    clock.settle_result(2)


def test_prepared_definition_is_invisible_and_abort_consumes_no_item_ids():
    clock, owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)

    assert not scene.state.hidden
    assert owners.require_live(owner).high_water.region == 0
    scene.abort()

    assert clock.revision == 1
    assert clock.outstanding_result is None
    assert owners.require_live(owner).high_water.region == 0
    with pytest.raises(RuntimeError, match="stale or foreign"):
        scene.install_prepared(prepared)


def test_prepared_transaction_is_frozen_against_later_operation_loss():
    clock, owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)

    with pytest.raises(SceneModelError, match="prepared and frozen"):
        scene.define_series(
            SeriesDefinition(owner, 1, 4, TimestampMode.EXPLICIT, 0)
        )
    with pytest.raises(RuntimeError, match="stale or foreign"):
        scene.install_prepared(prepared)

    assert owners.require_live(owner).high_water.region == 0
    assert owners.require_live(owner).high_water.series == 0
    scene.reject()
    clock.settle_result(2)


def test_layout_target_is_copy_on_write_and_active_quota_is_not_double_charged():
    clock, owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_object(_object(owner, 1, GroupBody()))
    _install(scene, clock, CommitDisposition.COMMIT)
    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)
    active = scene.state.active
    active_owner = active.owners[owner.owner_id]

    resized = PresentationGeometry(20, 10, 1)
    scene.require_layout(resized)
    _begin(clock, scene, 4, RetainedMode.LAYOUT_START, resized)
    scene.replace_region(_region(owner, generation=1))
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)

    assert scene.state.active is active
    assert active.owners[owner.owner_id] is active_owner
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    hidden_owner = scene.state.hidden.owners[owner.owner_id]
    assert hidden_owner is not active_owner
    assert hidden_owner.objects[1] is active_owner.objects[1]
    assert hidden_owner.usage == active_owner.usage
    assert owners.state.reservations.objects == _quotas().objects

    _begin(clock, scene, 5, RetainedMode.LAYOUT_CONTINUE, resized)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)
    assert scene.state.retained_visible
    assert scene.state.active.owners[owner.owner_id].regions[1].geometry_generation == 1


def test_layout_reveal_rejects_any_surviving_stale_region():
    clock, _owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    _install(scene, clock, CommitDisposition.COMMIT)
    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)

    resized = PresentationGeometry(20, 10, 1)
    scene.require_layout(resized)
    _begin(clock, scene, 4, RetainedMode.LAYOUT_START, resized)
    _install(scene, clock, CommitDisposition.COMMIT)
    _begin(clock, scene, 5, RetainedMode.LAYOUT_CONTINUE, resized)
    with pytest.raises(SceneModelError) as stale:
        scene.prepare_commit(CommitDisposition.COMMIT_AND_REVEAL)
    assert stale.value.code is SceneErrorCode.BOUNDS
    assert scene.state.active.owners[owner.owner_id].regions[1].geometry_generation == 0
    scene.reject()
    clock.settle_result(5)


def test_resize_before_first_reveal_discards_hidden_but_stays_replace_required():
    clock, owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    _install(scene, clock, CommitDisposition.COMMIT)
    assert scene.state.hidden is not None
    assert not scene.state.retained_initialized

    newest = PresentationGeometry(16, 8, 1)
    scene.require_layout(newest)

    assert scene.state.hidden is None
    assert scene.state.requirement is RebuildRequirement.REPLACE
    assert not scene.state.retained_visible
    _begin(clock, scene, 3, RetainedMode.REPLACE_START, newest)
    with pytest.raises(SceneModelError) as stale_geometry:
        scene.define_region(_region(owner))
    assert stale_geometry.value.code is SceneErrorCode.BOUNDS
    assert owners.require_live(owner).high_water.region == 1
    scene.reject()
    clock.settle_result(3)


def test_readout_rounding_is_exact_and_preserves_negative_rounded_zero():
    fixed_tie = ReadoutBody(WHITE, BLACK, ReadoutFormat.FIXED, 0, -5, 10, "")
    rounded_zero = ReadoutBody(WHITE, BLACK, ReadoutFormat.FIXED, 0, -1, 100, " V")
    percent = ReadoutBody(
        WHITE, BLACK, ReadoutFormat.PERCENT, 1, (1 << 63) - 1, (1 << 63) - 1, ""
    )

    assert fixed_tie.formatted_bytes(64) == b"-1"
    assert rounded_zero.formatted_bytes(64) == b"-0 V"
    assert percent.formatted_bytes(64) == b"100.0%"


def test_readout_supports_caller_bounded_precision_above_python_digit_ceiling():
    precision = 5000
    body = ReadoutBody(
        WHITE, BLACK, ReadoutFormat.FIXED, precision, 1, 3, ""
    )

    rendered = body.formatted_bytes(precision + 2)

    assert len(rendered) == precision + 2
    assert rendered.startswith(b"0." + b"3" * 32)
    assert rendered.endswith(b"3" * 32)


@pytest.mark.parametrize(
    ("factory", "message"),
    (
        (lambda: ReadoutBody(WHITE, BLACK, True, 0, 1, 1, ""), "must not be bool"),
        (lambda: SeriesDefinition(_owner(), 1, 1, True, 0), "must not be bool"),
    ),
)
def test_wire_enums_reject_boolean_aliases(factory, message):
    with pytest.raises(TypeError, match=message):
        factory()

    clock, _owners, _owner_identity, scene = _domain()
    lease = clock.reserve(TransactionFamily.PRESENT, 2, 1)
    with pytest.raises(SceneModelError, match="must not be bool"):
        scene.begin(lease, True, GEOMETRY)
    clock.abort(lease)

    _begin(clock, scene, 3, RetainedMode.REPLACE_START)
    with pytest.raises(SceneModelError, match="must not be bool"):
        scene.prepare_commit(True)
    scene.reject()
    clock.settle_result(3)


def test_series_capacity_not_current_samples_consumes_owner_slots():
    clock, owners, owner, scene = _domain(quotas=_quotas(sample_slots=8))
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_series(SeriesDefinition(owner, 1, 8, TimestampMode.EXPLICIT, 0))

    with pytest.raises(SceneModelError) as capacity:
        scene.define_series(SeriesDefinition(owner, 2, 1, TimestampMode.UNIFORM, 1))
    assert capacity.value.code is SceneErrorCode.QUOTA
    assert owners.require_live(owner).high_water.series == 0
    scene.reject()
    clock.settle_result(2)
