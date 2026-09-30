"""Pane authority, viewport, capacity and atomic scene invariants."""

from dataclasses import replace

import pytest

from rich_terminal.retained_model import (
    OwnerIdentity, OwnerLedger, OwnerQuotas, RetainedFeature, RetainedPolicy,
)
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ObjectBounds, ObjectDefinition, ObjectKind, PaneBody,
    RegionDefinition, RetainedMode, RetainedSceneModel, SceneModelError,
)
from rich_terminal.update_authority import (
    TerminalGeometry, TerminalUpdateAuthority, TransactionFamily,
)


GEOMETRY = TerminalGeometry(20, 10, 0)
OWNER = OwnerIdentity(19, 0, 1, 1)


def policy(**changes):
    values = dict(
        features=RetainedFeature.CORE | RetainedFeature.PANES,
        max_owner_records=2, max_live_owners=2, max_regions=6,
        max_resources=0, max_objects=8, max_series=0,
        max_operations_per_transaction=16, max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=4096, total_resource_bytes=0,
        image_format=0, max_image_width=0, max_image_height=0,
        max_path_points=0, max_glyph_run_bytes=0, max_samples_per_append=0,
        max_history_per_series=0, minimum_presentation_interval_us=0,
        total_sample_slots=0, total_utf8_bytes=256,
        client_to_terminal_max_payload=512, terminal_to_client_max_payload=64,
        base_max_transaction_bytes=4096,
    )
    values.update(changes)
    return RetainedPolicy(**values)


def pane(**changes):
    values = dict(
        owner=OWNER, object_id=1, region_id=1, parent_object_id=0,
        bounds=ObjectBounds(0, 0, 20, 10), z_order=0, visible=True,
        body=PaneBody(2, ObjectBounds(1, 1, 18, 8), "Pad", True),
    )
    values.update(changes)
    return ObjectDefinition(**values)


def regions():
    chrome = RegionDefinition(OWNER, 1, 0, 0, 20, 10,
                              0, 0, 20, 10, 0, True, True, 0)
    content = RegionDefinition(OWNER, 2, 1, 1, 18, 8,
                               1, 1, 18, 8, 1, True, True, 0)
    return chrome, content


def domain(*, selected_policy=None, utf8_quota=256):
    clock = TerminalUpdateAuthority(
        presentation_epoch=0, revision=1, transaction_high_water=1,
    )
    owners = OwnerLedger(session_id=19, presentation_epoch=0,
                         policy=selected_policy or policy())
    owners.open(OWNER, OwnerQuotas(6, 0, 8, 0, 0, utf8_quota, 0))
    scene = RetainedSceneModel(clock=clock, owners=owners,
                              resources=RetainedResourceStore(owners),
                              geometry=GEOMETRY)
    return clock, owners, scene


def begin(clock, scene, transaction_id=2, mode=RetainedMode.REPLACE_START):
    lease = clock.reserve(TransactionFamily.PRESENT, transaction_id, clock.revision)
    scene.begin(lease, mode, GEOMETRY)


def install(clock, scene, disposition=CommitDisposition.COMMIT_AND_REVEAL):
    result = scene.install_prepared(scene.prepare_commit(disposition))
    clock.settle_result(result.transaction_id)


def reveal(*, content=None, definition=None, utf8_quota=256):
    clock, owners, scene = domain(utf8_quota=utf8_quota)
    begin(clock, scene)
    chrome, default_content = regions()
    scene.define_region(chrome)
    scene.define_region(content or default_content)
    scene.define_object(definition or pane())
    install(clock, scene, CommitDisposition.COMMIT)
    begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    install(clock, scene)
    return clock, owners, scene


def test_panes_are_core_only_and_have_exact_policy_floors():
    assert RetainedFeature.PANES == 1 << 11
    assert pane().kind is ObjectKind.PANE
    assert policy(client_to_terminal_max_payload=104,
                  max_retained_transaction_bytes=304, max_regions=2).features == (
                      RetainedFeature.CORE | RetainedFeature.PANES)
    for changes, message in (
        ({"client_to_terminal_max_payload": 103}, "104-byte"),
        ({"max_retained_transaction_bytes": 303}, "transaction maximum"),
        ({"max_objects": 0}, "object capacity"),
        ({"total_utf8_bytes": 0}, "UTF-8 capacity"),
        ({"max_regions": 1}, "at least two regions"),
    ):
        with pytest.raises(ValueError, match=message):
            policy(**changes)


@pytest.mark.parametrize("title", ["\0", "\t", "\n", "\r", "\x7f", "\x85", "\x9f", "\u2028", "\u2029", "\ud800"])
def test_title_requires_clean_single_line_scalar_text(title):
    with pytest.raises(ValueError):
        PaneBody(2, ObjectBounds(1, 1, 18, 8), title)


@pytest.mark.parametrize("changes", [
    {"parent_object_id": 3},
    {"visible": False},
    {"body": PaneBody(1, ObjectBounds(1, 1, 18, 8), "Pad")},
    {"body": PaneBody(2, ObjectBounds(-1, 1, 18, 8), "Pad")},
    {"body": PaneBody(2, ObjectBounds(1, 1, 20, 8), "Pad")},
])
def test_pane_envelope_rejects_invalid_geometry_focus_and_relationship(changes):
    with pytest.raises(ValueError):
        pane(**changes)


def test_title_metadata_never_requires_reserved_header_or_shrinks_content():
    assert pane(body=PaneBody(2, ObjectBounds(0, 0, 20, 10), "Pad"))
    assert pane(bounds=ObjectBounds(0, 0, 2, 10),
                body=PaneBody(2, ObjectBounds(0, 1, 2, 8), "Pad"))


def test_large_pane_bounds_use_exact_arithmetic():
    wide = pane(bounds=ObjectBounds(2**31 - 1, -2**31, 2**32 - 1, 10),
                body=PaneBody(2, ObjectBounds(1, 1, 2**32 - 2, 8), "Wide"))
    assert wide.bounds.cell_right > 2**32


def test_reveal_and_replace_account_title_bytes_without_a_glyph_capacity():
    clock, owners, scene = reveal()
    original = scene.state
    assert original.active.owners[1].usage.objects == 1
    assert original.active.owners[1].usage.utf8_bytes == 3
    begin(clock, scene, 4, RetainedMode.DELTA)
    updated = pane(body=PaneBody(2, ObjectBounds(1, 1, 18, 8), "茶", False))
    scene.replace_object(updated)
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is original
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    assert scene.state.active.owners[1].objects[1] == updated
    assert scene.state.active.owners[1].usage.utf8_bytes == 3
    assert owners.require_live(OWNER).high_water.object == 1


def test_pane_title_obeys_owner_utf8_quota():
    clock, _, scene = domain(utf8_quota=2)
    begin(clock, scene)
    for region in regions():
        scene.define_region(region)
    before = scene.state
    with pytest.raises(SceneModelError, match="UTF-8-byte"):
        scene.define_object(pane())
    assert scene.state is before


def test_pane_title_obeys_frame_capacity_even_with_available_utf8_quota():
    # The policy must also admit the 172-byte CELL row at this geometry.
    clock, _, scene = domain(selected_policy=policy(client_to_terminal_max_payload=172))
    begin(clock, scene)
    for region in regions():
        scene.define_region(region)
    with pytest.raises(SceneModelError, match="payload or transaction capacity"):
        scene.define_object(pane(body=PaneBody(2, ObjectBounds(1, 1, 18, 8), "x" * 69)))


def test_unadvertised_panes_are_rejected_before_model_mutation():
    old_policy = policy(features=RetainedFeature.CORE, total_utf8_bytes=0)
    clock, _, scene = domain(selected_policy=old_policy, utf8_quota=0)
    begin(clock, scene)
    for region in regions():
        scene.define_region(region)
    with pytest.raises(SceneModelError, match="PANE feature was not advertised"):
        scene.define_object(pane())
    assert not scene.state.active.owners


@pytest.mark.parametrize("change", [
    dict(clipped=False, clip_x=0, clip_y=0, clip_cols=0, clip_rows=0),
    dict(logical_x=0, logical_cols=19, clip_x=0, clip_cols=19),
])
def test_content_region_requires_explicit_contained_clip_at_final_commit(change):
    clock, _, scene = domain()
    begin(clock, scene)
    chrome, content = regions()
    scene.define_region(chrome)
    scene.define_region(replace(content, **change))
    scene.define_object(pane())
    before = scene.state
    with pytest.raises(SceneModelError, match="explicit clip|exceeds its content"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is before


def test_hidden_and_fully_clipped_content_are_explicit_independent_states():
    _, content = regions()
    content = replace(content, visible=False, clip_x=0, clip_y=0,
                      clip_cols=0, clip_rows=0)
    _, _, scene = reveal(content=content)
    owner = scene.state.active.owners[1]
    assert owner.objects[1].visible
    assert not owner.regions[2].visible


def test_duplicate_pane_binding_rejects_whole_commit():
    clock, _, scene = reveal()
    before = scene.state
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.define_object(pane(object_id=2))
    with pytest.raises(SceneModelError, match="multiple panes"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is before


def test_dropped_content_region_rejects_atomically_but_joint_drop_succeeds():
    clock, _, scene = reveal()
    before = scene.state
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.drop_region(OWNER, 2)
    with pytest.raises(SceneModelError, match="absent exact-owner content region"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is before

    clock, _, scene = reveal()
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.drop_region(OWNER, 2)
    scene.drop_object(OWNER, 1)
    install(clock, scene, CommitDisposition.COMMIT)
    assert scene.state.active.owners[1].usage.objects == 0
    assert scene.state.active.owners[1].usage.utf8_bytes == 0


def test_region_clip_can_be_repaired_in_same_transaction_as_pane_resize():
    clock, _, scene = reveal()
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_object(pane(body=PaneBody(2, ObjectBounds(2, 1, 17, 8), "Pad")))
    _, content = regions()
    scene.replace_region(replace(content, clip_x=2, clip_cols=17))
    install(clock, scene, CommitDisposition.COMMIT)
    assert scene.state.active.owners[1].regions[2].clip_x == 2


def test_focused_pane_cannot_hide_and_unfocused_hide_preserves_content_visibility():
    clock, _, scene = reveal()
    before = scene.state
    begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="focused PANE must be visible"):
        scene.set_object_visibility(OWNER, 1, False)
    assert scene.state is before

    clock, _, scene = reveal(definition=pane(body=PaneBody(2, ObjectBounds(1, 1, 18, 8), "Pad")))
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.set_object_visibility(OWNER, 1, False)
    install(clock, scene, CommitDisposition.COMMIT)
    assert not scene.state.active.owners[1].objects[1].visible
    assert scene.state.active.owners[1].regions[2].visible


def test_content_relationship_never_resolves_across_owner_identity():
    clock, owners, scene = domain(selected_policy=policy(max_regions=8))
    foreign = replace(OWNER, owner_id=2)
    owners.open(foreign, OwnerQuotas(2, 0, 0, 0, 0, 0, 0))
    begin(clock, scene)
    scene.define_region(regions()[0])
    scene.define_region(replace(regions()[1], owner=foreign))
    # An existing region under another owner cannot satisfy the reference.
    with pytest.raises(SceneModelError, match="content region must be defined"):
        scene.define_object(pane())


def test_content_must_paint_after_chrome_to_preserve_control_hit_authority():
    clock, _, scene = domain()
    begin(clock, scene)
    chrome, content = regions()
    scene.define_region(chrome)
    scene.define_region(replace(content, z_order=-1))
    scene.define_object(pane())
    with pytest.raises(SceneModelError, match="paint after its chrome region"):
        scene.prepare_commit(CommitDisposition.COMMIT)

    # Equal z orders use region identity, matching ordinary region ordering.
    _, _, valid = reveal(content=replace(content, z_order=chrome.z_order))
    assert valid.state.active.owners[1].objects[1].kind is ObjectKind.PANE
