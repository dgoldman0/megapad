"""Taskbar publication, fixed slot geometry, quotas, and input authority."""

from dataclasses import replace

import pytest

from tests import test_rich_terminal_semantic_scene as helpers
from rich_terminal.retained_model import OwnerIdentity, OwnerLedger, OwnerQuotas, RetainedFeature
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ControlDefinition, ControlKind, ControlState, ObjectBounds,
    RetainedMode, RetainedSceneModel, SceneErrorCode, SceneModelError,
)
from rich_terminal.update_authority import TerminalUpdateAuthority


LIVE = ControlState.VISIBLE | ControlState.ENABLED


def _domain(*, taskbars=True, object_quota=12, utf8_quota=192, **policy_changes):
    clock = TerminalUpdateAuthority(
        presentation_epoch=helpers.EPOCH, revision=1, transaction_high_water=1,
    )
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    features = RetainedFeature.CORE | RetainedFeature.CONTROLS
    if taskbars:
        features |= RetainedFeature.TASKBARS
    policy = replace(helpers._policy(), features=features, max_glyph_run_bytes=0,
                     **policy_changes)
    ledger = OwnerLedger(session_id=helpers.SESSION_ID,
                         presentation_epoch=helpers.EPOCH, policy=policy)
    ledger.open(owner, OwnerQuotas(1, 0, object_quota, 0, 0, utf8_quota, 0))
    scene = RetainedSceneModel(clock=clock, owners=ledger,
                              resources=RetainedResourceStore(ledger),
                              geometry=helpers.GEOMETRY)
    return clock, ledger, owner, scene


def _root(owner, **changes):
    return replace(ControlDefinition(
        owner, 1, ControlKind.TASKBAR, LIVE, 20, 1, 0, 0,
        ObjectBounds(0, 11, 24, 1), "", "",
    ), **changes)


def _entry(owner, control_id=3, **changes):
    return replace(ControlDefinition(
        owner, control_id, ControlKind.TASK, LIVE | ControlState.SELECTED,
        0, 1, 1, 1, ObjectBounds(5, 0, 8, 1), "Pad λ", "A-P",
    ), **changes)


def _entries(owner):
    return (
        _entry(owner, 2, kind=ControlKind.LAUNCHER, state=LIVE, order=0,
               bounds=ObjectBounds(0, 0, 4, 1), label="Apps", shortcut="M"),
        _entry(owner),
        _entry(owner, 4, state=LIVE | ControlState.MINIMIZED, order=2,
               bounds=ObjectBounds(14, 0, 10, 1), label="Lab", shortcut=""),
    )


def _visible():
    clock, ledger, owner, scene = _domain()
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(_root(owner))
    for entry in _entries(owner):
        scene.define_control(entry)
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    helpers._install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    return clock, ledger, owner, scene


def _reject(clock, scene):
    result = scene.reject()
    clock.settle_result(result.transaction_id)


def test_taskbar_uses_existing_quotas_and_freezes_guest_slots():
    _, ledger, owner, scene = _visible()
    committed = scene.state.active.owners[owner.owner_id]
    assert committed.usage.objects == 4
    assert committed.usage.utf8_bytes == len("AppsMPad λA-PLab".encode("utf-8"))
    assert ledger.require_live(owner).high_water.control == 4
    assert committed.controls[3].bounds == ObjectBounds(5, 0, 8, 1)
    assert scene.require_interactable_control(owner, 2).kind is ControlKind.LAUNCHER
    assert scene.require_interactable_control(owner, 3).state & ControlState.SELECTED
    assert scene.require_interactable_control(owner, 4).state & ControlState.MINIMIZED
    with pytest.raises(SceneModelError, match="not activatable"):
        scene.require_interactable_control(owner, 1)
    with pytest.raises(TypeError):
        committed.controls[5] = committed.controls[3]


def test_selection_transfer_validates_final_graph_and_replaces_label_utf8_exactly():
    clock, _, owner, scene = _visible()
    before = scene.state
    controls = before.active.owners[owner.owner_id].controls
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    # The transient staging graph has two selected tasks. Only the complete
    # graph is published, allowing either order for an atomic focus transfer.
    scene.replace_control(replace(controls[4], state=LIVE | ControlState.SELECTED,
                                  label="Sound Lab", shortcut="A-S"))
    scene.replace_control(replace(controls[3], state=LIVE | ControlState.MINIMIZED))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    after = scene.state.active.owners[owner.owner_id]
    assert after.controls[4].state == LIVE | ControlState.SELECTED
    assert after.controls[3].state == LIVE | ControlState.MINIMIZED
    assert after.usage.utf8_bytes == len("AppsMPad λA-PSound LabA-S".encode("utf-8"))
    assert before.active.owners[owner.owner_id].controls[4].label == "Lab"


@pytest.mark.parametrize("change", [
    {"bounds": ObjectBounds(6, 0, 8, 1)}, {"order": 7},
    {"parent_control_id": 2}, {"region_id": 2},
])
def test_entry_replacement_cannot_change_geometry_or_hierarchy(change):
    clock, _, owner, scene = _visible()
    before = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="only state, label, and shortcut"):
        scene.replace_control(replace(before.active.owners[owner.owner_id].controls[3], **change))
    _reject(clock, scene)
    assert scene.state is before


def test_root_replacement_changes_state_but_not_geometry():
    clock, _, owner, scene = _visible()
    before = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="only the control state"):
        scene.replace_control(_root(owner, bounds=ObjectBounds(1, 11, 23, 1)))
    _reject(clock, scene)
    assert scene.state is before
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    scene.replace_control(_root(owner, state=ControlState.VISIBLE))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    for target in (2, 3, 4):
        with pytest.raises(SceneModelError, match="interactive TASKBAR"):
            scene.require_interactable_control(owner, target)


@pytest.mark.parametrize("fault", ["hidden_overlap", "duplicate_order", "selected"])
def test_bad_final_graph_preserves_active_scene_and_control_high_water(fault):
    clock, ledger, owner, scene = _visible()
    before = scene.state
    high_water = ledger.require_live(owner).high_water
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    if fault == "selected":
        scene.replace_control(replace(before.active.owners[owner.owner_id].controls[4],
                                      state=LIVE | ControlState.SELECTED))
        message = "multiple selected tasks"
    else:
        entry = _entry(owner, 5, state=ControlState(0), order=3,
                       bounds=ObjectBounds(4, 0, 1, 1))
        if fault == "hidden_overlap":
            entry = replace(entry, bounds=ObjectBounds(3, 0, 1, 1))
            message = "child bounds overlap"
        else:
            entry = replace(entry, order=0)
            message = "sibling order is duplicated"
        scene.define_control(entry)
    with pytest.raises(SceneModelError, match=message):
        scene.prepare_commit(CommitDisposition.COMMIT)
    _reject(clock, scene)
    assert scene.state is before
    assert ledger.require_live(owner).high_water == high_water
    # Rejected new IDs remain usable, and an abutting slot is valid.
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    scene.define_control(_entry(owner, 5, state=LIVE, order=3,
                                bounds=ObjectBounds(4, 0, 1, 1)))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert ledger.require_live(owner).high_water.control == 5


@pytest.mark.parametrize("change,message", [
    ({"bounds": ObjectBounds(23, 0, 2, 1)}, "exceed TASKBAR"),
    ({"parent_control_id": 3}, "parent must be a live TASKBAR"),
    ({"region_id": 2}, "region must be defined"),
])
def test_invalid_dependency_rejects_before_reserving_new_identity(change, message):
    clock, ledger, owner, scene = _visible()
    before = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match=message):
        scene.define_control(_entry(owner, 5, state=LIVE, order=3, **change))
    _reject(clock, scene)
    assert scene.state is before
    assert ledger.require_live(owner).high_water.control == 4


def test_root_drop_requires_removing_all_children_atomically():
    clock, _, owner, scene = _visible()
    before = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.drop_control(owner, 1)
    with pytest.raises(SceneModelError, match="parent must be a live TASKBAR"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    _reject(clock, scene)
    assert scene.state is before
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    for control_id in (1, 2, 3, 4):
        scene.drop_control(owner, control_id)
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    after = scene.state.active.owners[owner.owner_id]
    assert not after.controls
    assert after.usage.objects == after.usage.utf8_bytes == 0


@pytest.mark.parametrize("target,state", [(2, ControlState(0)), (4, ControlState.VISIBLE)])
def test_hidden_or_disabled_entry_cannot_activate(target, state):
    clock, _, owner, scene = _visible()
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    current = scene.state.active.owners[owner.owner_id].controls[target]
    scene.replace_control(replace(current, state=state))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    with pytest.raises(SceneModelError, match="hidden or disabled"):
        scene.require_interactable_control(owner, target)


def test_unadvertised_taskbars_do_not_consume_identity_or_usage():
    clock, ledger, owner, scene = _domain(taskbars=False)
    before = scene.state
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    with pytest.raises(SceneModelError, match="TASKBARS was not advertised"):
        scene.define_control(_root(owner))
    _reject(clock, scene)
    assert scene.state is before
    assert ledger.require_live(owner).high_water.control == 0


@pytest.mark.parametrize("limits", [{"object_quota": 1}, {"utf8_quota": 1}])
def test_task_entry_uses_existing_object_and_utf8_reservations(limits):
    clock, ledger, owner, scene = _domain(**limits)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(_root(owner))
    with pytest.raises(SceneModelError) as error:
        scene.define_control(_entry(owner))
    assert error.value.code is SceneErrorCode.QUOTA
    _reject(clock, scene)
    assert ledger.require_live(owner).high_water.control == 0


@pytest.mark.parametrize("limits,label_bytes", [
    ({"client_to_terminal_max_payload": 256}, 174),
    ({"client_to_terminal_max_payload": 4096,
      "max_retained_transaction_bytes": 3144,
      "total_utf8_bytes": 4096, "utf8_quota": 4096}, 2862),
])
def test_task_text_must_fit_actual_payload_and_transaction_bounds(limits, label_bytes):
    clock, _, owner, scene = _domain(**limits)
    # The 24x12 fallback needs a 204-byte row payload and 3144 complete
    # transaction bytes. These policies admit it; the task's label plus its
    # three-byte shortcut exceeds the targeted record bound by exactly one.
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(_root(owner))
    with pytest.raises(SceneModelError, match="payload or transaction capacity"):
        scene.define_control(_entry(owner, label="x" * label_bytes))


def test_task_slot_containment_uses_wide_endpoint_arithmetic():
    clock, _, owner, scene = _domain()
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(_root(owner, bounds=ObjectBounds(-(1 << 31), 11, (1 << 32) - 1, 1)))
    scene.define_control(_entry(owner, state=LIVE,
                                bounds=ObjectBounds((1 << 31) - 1, 0, 1 << 31, 1)))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert scene.state.hidden.owners[owner.owner_id].controls[3].bounds.cell_right == (1 << 32) - 1
