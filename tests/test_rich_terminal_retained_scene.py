"""Focused immutable scene-target tests for RETAINED-1."""

from __future__ import annotations

from dataclasses import replace

import pytest

import rich_terminal.retained_scene as retained_scene_module
from rich_terminal.update_authority import (
    TerminalUpdateAuthority,
    TerminalGeometry,
    TransactionFamily,
)
from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_scene import (
    CommitDisposition,
    ExplicitSamples,
    GroupBody,
    GlyphRunBody,
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
    Sample,
    SceneErrorCode,
    SceneModelError,
    SeriesDefinition,
    StatusBody,
    TimestampMode,
    UniformSamples,
    WaveformBody,
)


SESSION = 0x0123456789ABCDEF
GEOMETRY = TerminalGeometry(20, 10, 0)
WHITE = RGBA(255, 255, 255, 255)
BLACK = RGBA(0, 0, 0, 255)
GREEN = RGBA(32, 220, 96, 255)


def test_glyph_run_body_accepts_only_physically_supported_cell_attribute_bits():
    assert GlyphRunBody(WHITE, BLACK, 0x6F, "draw").attributes == 0x6F
    with pytest.raises(ValueError, match="unsupported GLYPH_RUN bits"):
        GlyphRunBody(WHITE, BLACK, 0x10, "draw")
    with pytest.raises(ValueError, match="unsupported GLYPH_RUN bits"):
        GlyphRunBody(WHITE, BLACK, 0x80, "draw")


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
        max_glyph_run_bytes=64,
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


def _domain(
    *,
    quotas: OwnerQuotas | None = None,
    policy: RetainedPolicy | None = None,
):
    clock = TerminalUpdateAuthority(
        presentation_epoch=3, revision=1, transaction_high_water=1
    )
    owners = OwnerLedger(
        session_id=SESSION,
        presentation_epoch=3,
        policy=_policy() if policy is None else policy,
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


def _stage_complete_target(scene: RetainedSceneModel, owner: OwnerIdentity, points):
    scene.define_region(_region(owner))
    scene.define_series(SeriesDefinition(owner, 1, 8, TimestampMode.EXPLICIT, 0))
    scene.define_series(SeriesDefinition(owner, 2, 8, TimestampMode.UNIFORM, 1000))
    scene.define_object(_object(owner, 1, GroupBody()))
    scene.define_object(
        _object(owner, 2, PolylineBody(points, 0x01000000, GREEN), parent=1)
    )
    scene.define_object(_object(owner, 3, GlyphRunBody(WHITE, BLACK, 0, "Level"), parent=1))
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


def _reveal_complete_target(clock, scene, owner):
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    _stage_complete_target(scene, owner, (Point(0, 0), Point(0xFFFFFFFF, 0xFFFFFFFF)))
    _install(scene, clock, CommitDisposition.COMMIT)
    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)


def test_complete_definition_target_is_hidden_then_atomically_revealed():
    clock, owners, owner, scene = _domain()
    caller_points = [Point(0, 0), Point(0xFFFFFFFF, 0xFFFFFFFF)]
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    _stage_complete_target(scene, owner, caller_points)

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


def test_live_replace_start_restarts_hidden_target_without_disturbing_active_scene():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    active = scene.state.active

    _begin(clock, scene, 4, RetainedMode.REPLACE_START)
    scene.define_region(replace(_region(owner), region_id=2))
    scene.define_object(
        replace(
            _object(owner, 9, GlyphRunBody(WHITE, BLACK, 0, "older")),
            region_id=2,
        )
    )
    _install(scene, clock, CommitDisposition.COMMIT)

    older_hidden = scene.state.hidden
    assert older_hidden is not None
    assert scene.state.active is active
    assert set(older_hidden.owners[owner.owner_id].regions) == {2}
    assert set(older_hidden.owners[owner.owner_id].objects) == {9}

    _begin(clock, scene, 5, RetainedMode.REPLACE_START)
    scene.define_region(replace(_region(owner), region_id=3))
    scene.define_object(
        replace(
            _object(owner, 10, GlyphRunBody(WHITE, BLACK, 0, "newest")),
            region_id=3,
        )
    )
    _install(scene, clock, CommitDisposition.COMMIT)

    newest_hidden = scene.state.hidden
    assert newest_hidden is not None and newest_hidden is not older_hidden
    assert scene.state.active is active
    assert set(newest_hidden.owners[owner.owner_id].regions) == {3}
    assert set(newest_hidden.owners[owner.owner_id].objects) == {10}
    assert owners.require_live(owner).high_water.region == 3
    assert owners.require_live(owner).high_water.object == 10

    _begin(clock, scene, 6, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)

    assert scene.state.active is newest_hidden
    assert scene.state.hidden is None
    assert set(scene.state.active.owners[owner.owner_id].regions) == {3}
    assert set(scene.state.active.owners[owner.owner_id].objects) == {10}


def test_replace_start_promotes_layout_requirement_to_replacement_pending():
    clock, _owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    resized = TerminalGeometry(20, 10, 1)
    scene.require_layout(resized)
    assert scene.state.requirement is RebuildRequirement.LAYOUT

    _begin(clock, scene, 4, RetainedMode.REPLACE_START, resized)
    scene.define_region(replace(_region(owner, generation=1), region_id=2))
    _install(scene, clock, CommitDisposition.COMMIT)

    assert scene.state.hidden is not None
    assert scene.state.requirement is RebuildRequirement.REPLACE


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
        scene.define_object(_object(owner, 2, GlyphRunBody(WHITE, BLACK, 0, "extra")))
    assert over_quota.value.code is SceneErrorCode.QUOTA
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is initial_scene
    assert owners.state is initial_ledger
    assert owners.require_live(owner).high_water.object == 0
    scene.reject()
    clock.settle_result(3)


def test_operation_limit_admits_exact_boundary_then_rejects_next_operation():
    policy = replace(_policy(), max_operations_per_transaction=1)
    clock, owners, owner, scene = _domain(policy=policy)
    initial_scene = scene.state
    initial_ledger = owners.state
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))

    with pytest.raises(SceneModelError, match="operation count") as capacity:
        scene.define_object(_object(owner, 1, GroupBody()))

    assert capacity.value.code is SceneErrorCode.QUOTA
    assert scene.state is initial_scene
    assert owners.state is initial_ledger
    assert owners.require_live(owner).high_water.region == 0
    assert owners.require_live(owner).high_water.object == 0
    scene.reject()
    clock.settle_result(2)


def test_zero_glyph_capacity_rejects_even_an_empty_background_paint():
    policy = replace(
        _policy(),
        features=RetainedFeature.CORE,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_series=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
    )
    quotas = _quotas(objects=1, series=0, utf8_bytes=0, sample_slots=0)
    clock, _owners, owner, scene = _domain(policy=policy, quotas=quotas)
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))

    with pytest.raises(SceneModelError, match="glyph runs were not advertised") as caught:
        scene.define_object(_object(owner, 1, GlyphRunBody(WHITE, BLACK, 0, "")))

    assert caught.value.code is SceneErrorCode.FEATURE
    scene.reject()
    clock.settle_result(2)


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

    resized = TerminalGeometry(20, 10, 1)
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


def test_flat_glyph_staging_reuses_one_private_map_and_freezes_once(monkeypatch):
    object_count = 64
    policy = replace(
        _policy(),
        max_objects=object_count,
        max_operations_per_transaction=object_count + 1,
        total_utf8_bytes=object_count,
    )
    clock, _owners, owner, scene = _domain(
        policy=policy,
        quotas=_quotas(objects=object_count, utf8_bytes=object_count),
    )
    frozen_object_counts = []
    original_make_owner_scene = scene._make_owner_scene

    def counted_make_owner_scene(owner_identity, regions, objects, series, controls):
        frozen_object_counts.append(len(objects))
        return original_make_owner_scene(
            owner_identity,
            regions,
            objects,
            series,
            controls,
        )

    text_calls = 0
    original_text_bytes = retained_scene_module._text_bytes

    def counted_text_bytes(name, text):
        nonlocal text_calls
        text_calls += 1
        return original_text_bytes(name, text)

    monkeypatch.setattr(scene, "_make_owner_scene", counted_make_owner_scene)
    monkeypatch.setattr(retained_scene_module, "_text_bytes", counted_text_bytes)

    source = scene.state
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    staging = scene._staging
    assert staging is not None
    mutable_owner = staging.owners[owner.owner_id]
    mutable_objects = mutable_owner.objects

    for object_id in range(1, object_count + 1):
        scene.define_object(
            _object(owner, object_id, GlyphRunBody(WHITE, BLACK, 0, "x"))
        )
        assert staging.owners[owner.owner_id] is mutable_owner
        assert mutable_owner.objects is mutable_objects

    assert scene.state is source
    assert frozen_object_counts == []
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert frozen_object_counts == [object_count]
    assert text_calls <= 8 * object_count

    hidden = prepared.state.hidden
    assert hidden is not None
    frozen_owner = hidden.owners[owner.owner_id]
    assert frozen_owner.usage.objects == object_count
    assert frozen_owner.usage.utf8_bytes == object_count
    with pytest.raises(TypeError):
        frozen_owner.objects[object_count + 1] = frozen_owner.objects[object_count]
    with pytest.raises(TypeError):
        hidden.owners[owner.owner_id + 1] = frozen_owner
    # The prepared mapping owns a copy rather than a live proxy over the
    # transaction builder retained by the prepared capability.
    mutable_objects[object_count + 1] = frozen_owner.objects[object_count]
    assert object_count + 1 not in frozen_owner.objects
    del mutable_objects[object_count + 1]

    repeated = scene.prepare_commit(CommitDisposition.COMMIT)
    assert repeated.state.hidden is hidden
    assert frozen_object_counts == [object_count]

    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    assert scene.state.hidden is hidden
    assert frozen_object_counts == [object_count]


def test_owner_retirement_removes_exact_authority_from_active_and_hidden_atomically():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)

    resized = TerminalGeometry(20, 10, 1)
    scene.require_layout(resized)
    _begin(clock, scene, 4, RetainedMode.LAYOUT_START, resized)
    scene.replace_region(_region(owner, generation=1))
    _install(scene, clock, CommitDisposition.COMMIT)
    source_scene = scene.state
    source_ledger = owners.state
    assert owner.owner_id in source_scene.active.owners
    assert source_scene.hidden is not None
    assert owner.owner_id in source_scene.hidden.owners

    lease = clock.reserve(TransactionFamily.OWNER_DROP, 5, 4)
    prepared = scene.prepare_owner_retirement(lease, owner)

    assert scene.state is source_scene
    assert owners.state is source_ledger
    assert prepared.state.revision == 5
    assert owner.owner_id not in prepared.state.active.owners
    assert prepared.state.hidden is not None
    assert owner.owner_id not in prepared.state.hidden.owners
    assert not prepared.ledger.state.records[owner.owner_id].live
    assert prepared.ledger.state.reservations.live_owners == 0

    result = scene.install_owner_retirement(prepared)
    assert result.revision == 5
    assert scene.state is prepared.state
    assert owners.state is prepared.ledger.state
    assert source_scene.active.owners[owner.owner_id].owner == owner
    assert source_scene.hidden.owners[owner.owner_id].owner == owner
    clock.settle_result(5)

    # Dropping the exact tombstone remains a successful revisioned no-op for
    # the scene planes; the already-empty immutable targets are shared.
    tombstone_scene = scene.state
    tombstone_ledger = owners.state
    repeated_lease = clock.reserve(TransactionFamily.OWNER_DROP, 6, 5)
    repeated = scene.prepare_owner_retirement(repeated_lease, owner)
    assert repeated.state.active is tombstone_scene.active
    assert repeated.state.hidden is tombstone_scene.hidden
    assert repeated.ledger.state is tombstone_ledger
    repeated_result = scene.install_owner_retirement(repeated)
    assert repeated_result.revision == 6


def test_layout_reveal_rejects_any_surviving_stale_region():
    clock, _owners, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    _install(scene, clock, CommitDisposition.COMMIT)
    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(scene, clock, CommitDisposition.COMMIT_AND_REVEAL)

    resized = TerminalGeometry(20, 10, 1)
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

    newest = TerminalGeometry(16, 8, 1)
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


def test_value_and_visibility_delta_is_one_immutable_commit():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    old = scene.state.active
    old_owner = old.owners[owner.owner_id]
    high_water = owners.require_live(owner).high_water

    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.set_object_value(owner, 4, -99)
    scene.set_object_value(owner, 5, -100)
    scene.set_object_value(owner, 6, 0)
    scene.set_object_visibility(owner, 2, False)

    assert scene.state.active is old
    _install(scene, clock, CommitDisposition.COMMIT)
    current = scene.state.active.owners[owner.owner_id]
    assert scene.state.revision == 4
    assert current.objects[1] is old_owner.objects[1]
    assert current.objects[2].visible is False
    assert current.objects[4].body.value == -99
    assert current.objects[5].body.value == -100
    assert current.objects[6].body.value == 0
    assert old_owner.objects[2].visible is True
    assert old_owner.objects[4].body.value == -125
    assert current.usage.utf8_bytes == len(b"Level") + len(b"-9.9 dB")
    assert owners.require_live(owner).high_water == high_water


def test_object_replace_and_ordered_drops_recompute_target_usage_without_reusing_ids():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    source = scene.state.active
    high_water = owners.require_live(owner).high_water

    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_object(
        _object(owner, 3, GlyphRunBody(WHITE, BLACK, 0, "L"), parent=1)
    )
    _install(scene, clock, CommitDisposition.COMMIT)

    replaced = scene.state.active.owners[owner.owner_id]
    assert replaced.objects[3].body.text == "L"
    assert replaced.usage.utf8_bytes == len(b"L") + len(b"-12.5 dB")
    assert source.owners[owner.owner_id].objects[3].body.text == "Level"
    assert owners.require_live(owner).high_water == high_water

    _begin(clock, scene, 5, RetainedMode.DELTA)
    # Final-graph validation deliberately permits the parent and dependency
    # providers to disappear before their dependents in the same transaction.
    for object_id in range(1, 9):
        scene.drop_object(owner, object_id)
    scene.drop_series(owner, 2)
    scene.drop_series(owner, 1)
    scene.drop_region(owner, 1)
    _install(scene, clock, CommitDisposition.COMMIT)

    emptied = scene.state.active.owners[owner.owner_id]
    assert not emptied.regions
    assert not emptied.objects
    assert not emptied.series
    assert emptied.usage.regions == 0
    assert emptied.usage.objects == 0
    assert emptied.usage.series == 0
    assert emptied.usage.utf8_bytes == 0
    assert emptied.usage.sample_slots == 0
    assert owners.require_live(owner).high_water == high_water


@pytest.mark.parametrize(
    "drop",
    (
        lambda scene, owner: scene.drop_region(owner, 1),
        lambda scene, owner: scene.drop_object(owner, 1),
        lambda scene, owner: scene.drop_series(owner, 1),
    ),
)
def test_drop_defers_surviving_reference_rejection_to_final_graph(drop):
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    source = scene.state.active
    ledger = owners.state

    _begin(clock, scene, 4, RetainedMode.DELTA)
    drop(scene, owner)
    with pytest.raises(SceneModelError) as invalid:
        scene.prepare_commit(CommitDisposition.COMMIT)

    assert invalid.value.code is SceneErrorCode.GRAPH
    assert scene.state.active is source
    assert owners.state is ledger
    scene.reject()
    clock.settle_result(4)


def test_object_replace_requires_same_type_and_drop_requires_exact_owner():
    clock, _owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)

    _begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError) as wrong_type:
        scene.replace_object(_object(owner, 3, GroupBody(), parent=1))
    assert wrong_type.value.code is SceneErrorCode.STATE
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.drop_object(owner, 3)
    scene.reject()
    clock.settle_result(4)

    _begin(clock, scene, 5, RetainedMode.DELTA)
    with pytest.raises(SceneModelError) as stale_owner:
        scene.drop_region(_owner(generation=owner.owner_generation + 1), 1)
    assert stale_owner.value.code is SceneErrorCode.AUTHORITY
    scene.reject()
    clock.settle_result(5)


def test_value_or_visibility_failure_poison_delta_without_partial_change():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    old = scene.state.active
    old_ledger = owners.state

    _begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError) as meter:
        scene.set_object_value(owner, 5, 1)
    assert meter.value.code is SceneErrorCode.BOUNDS
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.set_object_visibility(owner, 2, False)
    assert scene.state.active is old
    assert owners.state is old_ledger
    scene.reject()
    clock.settle_result(4)

    _begin(clock, scene, 5, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="visibility must be bool"):
        scene.set_object_visibility(owner, 2, 1)
    scene.reject()
    clock.settle_result(5)


def test_readout_value_recomputes_complete_utf8_usage_before_staging():
    clock, owners, owner, scene = _domain(quotas=_quotas(utf8_bytes=13))
    _reveal_complete_target(clock, scene, owner)
    old = scene.state.active

    _begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError) as quota:
        scene.set_object_value(owner, 4, -(1 << 63))

    assert quota.value.code is SceneErrorCode.QUOTA
    assert scene.state.active is old
    assert owners.require_live(owner).quotas.utf8_bytes == 13
    scene.reject()
    clock.settle_result(4)


def test_readout_value_text_bound_failure_is_sticky_before_staged_object_mutation():
    policy = replace(_policy(), max_glyph_run_bytes=8)
    clock, owners, owner, scene = _domain(policy=policy)
    _reveal_complete_target(clock, scene, owner)
    old = scene.state.active

    _begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="readout exceeds UTF-8 byte bound") as bound:
        scene.set_object_value(owner, 4, -(1 << 63))

    assert bound.value.code is SceneErrorCode.QUOTA
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.set_object_visibility(owner, 3, False)
    assert scene.state.active is old
    assert old.owners[owner.owner_id].objects[4].body.value == -125
    assert owners.require_live(owner).high_water.object == 8
    scene.reject()
    clock.settle_result(4)


def test_explicit_and_uniform_appends_copy_batches_and_evict_exact_oldest():
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    explicit_input = [Sample(10, 1), Sample(20, 2)]
    uniform_input = [3, 4, 5]
    explicit = ExplicitSamples(explicit_input)
    uniform = UniformSamples(100, uniform_input)

    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.append_series(owner, 1, explicit)
    scene.append_series(owner, 2, uniform)
    _install(scene, clock, CommitDisposition.COMMIT)
    explicit_input.append(Sample(30, 99))
    uniform_input.append(99)

    owner_scene = scene.state.active.owners[owner.owner_id]
    assert owner_scene.series[1].samples == (Sample(10, 1), Sample(20, 2))
    assert tuple(sample.timestamp_us for sample in owner_scene.series[2].samples) == (
        100,
        1100,
        2100,
    )

    _begin(clock, scene, 5, RetainedMode.DELTA)
    scene.append_series(
        owner,
        1,
        ExplicitSamples(tuple(Sample(timestamp, timestamp) for timestamp in range(30, 110, 10))),
    )
    _install(scene, clock, CommitDisposition.COMMIT)
    history = scene.state.active.owners[owner.owner_id].series[1].samples
    assert tuple(sample.timestamp_us for sample in history) == tuple(range(30, 110, 10))
    assert len(history) == 8
    assert scene.state.active.owners[owner.owner_id].usage.sample_slots == 16
    assert owners.require_live(owner).high_water.series == 2


def test_series_replace_changes_only_history_not_definition_or_capacity():
    clock, _owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.append_series(owner, 1, ExplicitSamples((Sample(10, 1), Sample(20, 2))))
    scene.append_series(owner, 2, UniformSamples(100, (3, 4)))
    _install(scene, clock, CommitDisposition.COMMIT)

    _begin(clock, scene, 5, RetainedMode.DELTA)
    scene.replace_series(owner, 1, ExplicitSamples((Sample(500, -1),)))
    scene.replace_series(owner, 2, UniformSamples(5000, (9, 8, 7)))
    _install(scene, clock, CommitDisposition.COMMIT)

    explicit = scene.state.active.owners[owner.owner_id].series[1]
    uniform = scene.state.active.owners[owner.owner_id].series[2]
    assert (explicit.history_capacity, explicit.timestamp_mode, explicit.uniform_interval_us) == (
        8,
        TimestampMode.EXPLICIT,
        0,
    )
    assert explicit.samples == (Sample(500, -1),)
    assert (uniform.history_capacity, uniform.timestamp_mode, uniform.uniform_interval_us) == (
        8,
        TimestampMode.UNIFORM,
        1000,
    )
    assert tuple(sample.timestamp_us for sample in uniform.samples) == (5000, 6000, 7000)


@pytest.mark.parametrize(
    "mutation",
    (
        lambda scene, owner: scene.append_series(
            owner, 1, ExplicitSamples((Sample(10, 1),))
        ),
        lambda scene, owner: scene.append_series(owner, 1, UniformSamples(10, (1,))),
        lambda scene, owner: scene.append_series(
            owner, 2, UniformSamples((1 << 64) - 500, (1, 2))
        ),
    ),
)
def test_bad_series_append_is_sticky_and_preserves_committed_history(mutation):
    clock, owners, owner, scene = _domain()
    _reveal_complete_target(clock, scene, owner)
    _begin(clock, scene, 4, RetainedMode.DELTA)
    scene.append_series(owner, 1, ExplicitSamples((Sample(10, 1),)))
    _install(scene, clock, CommitDisposition.COMMIT)
    old = scene.state.active
    old_ledger = owners.state

    _begin(clock, scene, 5, RetainedMode.DELTA)
    with pytest.raises(SceneModelError):
        mutation(scene, owner)
    with pytest.raises(SceneModelError, match="was rejected"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state.active is old
    assert owners.state is old_ledger
    scene.reject()
    clock.settle_result(5)
