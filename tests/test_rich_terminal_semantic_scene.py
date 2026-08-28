"""Focused immutable scene tests for the first semantic-control family."""

from __future__ import annotations

from dataclasses import replace

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_scene import (
    CommitDisposition,
    ControlDefinition,
    ControlKind,
    ControlState,
    GlyphRunBody,
    ObjectBounds,
    ObjectDefinition,
    RGBA,
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


SESSION_ID = 0x1020304050607080
EPOCH = 3
GEOMETRY = TerminalGeometry(24, 12, 0)
FULL_BOUNDS = ObjectBounds(0, 0, UINT32_MAX, 0x18000000)


def _policy(*, object_capacity: int = 16) -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE | RetainedFeature.CONTROLS,
        max_owner_records=2,
        max_live_owners=2,
        max_regions=4,
        max_resources=0,
        max_objects=object_capacity,
        max_series=0,
        max_operations_per_transaction=16,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=4096,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=32,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=256,
        client_to_terminal_max_payload=512,
        terminal_to_client_max_payload=128,
        base_max_transaction_bytes=8192,
    )


def _domain(*, object_quota: int = 12, object_capacity: int = 16):
    clock = TerminalUpdateAuthority(
        presentation_epoch=EPOCH,
        revision=1,
        transaction_high_water=1,
    )
    owner = OwnerIdentity(SESSION_ID, EPOCH, 7, 2)
    ledger = OwnerLedger(
        session_id=SESSION_ID,
        presentation_epoch=EPOCH,
        policy=_policy(object_capacity=object_capacity),
    )
    ledger.open(
        owner,
        OwnerQuotas(1, 0, object_quota, 0, 0, 192, 0),
    )
    scene = RetainedSceneModel(clock=clock, owners=ledger, geometry=GEOMETRY)
    return clock, ledger, owner, scene


def _begin(clock, scene, transaction_id: int, mode: RetainedMode):
    lease = clock.reserve(TransactionFamily.PRESENT, transaction_id, clock.revision)
    scene.begin(lease, mode, GEOMETRY)


def _install(clock, scene, disposition: CommitDisposition):
    prepared = scene.prepare_commit(disposition)
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)


def _region(owner):
    return RegionDefinition(owner, 1, 0, 0, 24, 12, 3, True, True, 0)


def _control(
    owner,
    control_id: int,
    kind: ControlKind,
    *,
    parent: int = 0,
    order: int = 0,
    state: ControlState = ControlState.VISIBLE | ControlState.ENABLED,
    label: str = "",
    shortcut: str = "",
):
    return ControlDefinition(
        owner=owner,
        control_id=control_id,
        kind=kind,
        state=state,
        z_order=20 if kind is ControlKind.MENU_BAR else 0,
        region_id=1,
        parent_control_id=parent,
        order=order,
        bounds=FULL_BOUNDS if kind is ControlKind.MENU_BAR else None,
        label=label,
        shortcut=shortcut,
    )


def _complete_menu(scene, owner):
    scene.define_region(_region(owner))
    scene.define_control(_control(owner, 1, ControlKind.MENU_BAR))
    scene.define_control(
        _control(
            owner,
            2,
            ControlKind.MENU,
            parent=1,
            label="File",
            state=(
                ControlState.VISIBLE
                | ControlState.ENABLED
                | ControlState.OPEN
                | ControlState.SELECTED
            ),
        )
    )
    scene.define_control(
        _control(
            owner,
            3,
            ControlKind.MENU_ITEM,
            parent=2,
            label="New note",
            shortcut="Ctrl+N",
        )
    )
    scene.define_control(
        _control(
            owner,
            4,
            ControlKind.MENU_SEPARATOR,
            parent=2,
            order=1,
            state=ControlState.VISIBLE,
        )
    )
    scene.define_control(
        _control(
            owner,
            5,
            ControlKind.MENU_ITEM,
            parent=2,
            order=2,
            label="Close",
            shortcut="Ctrl+W",
        )
    )


def test_semantic_menu_is_accounted_frozen_and_replaced_as_control_state():
    clock, ledger, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    _complete_menu(scene, owner)
    _install(clock, scene, CommitDisposition.COMMIT)

    hidden = scene.state.hidden
    assert hidden is not None
    owner_scene = hidden.owners[owner.owner_id]
    assert owner_scene.usage.objects == 5
    assert owner_scene.usage.utf8_bytes == len(
        "FileNew noteCtrl+NCloseCtrl+W".encode("utf-8")
    )
    assert ledger.require_live(owner).high_water.control == 5
    with pytest.raises(TypeError):
        owner_scene.controls[6] = owner_scene.controls[5]

    _begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    _install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    assert scene.require_interactable_control(owner, 2).label == "File"
    assert scene.require_interactable_control(owner, 3).label == "New note"
    with pytest.raises(SceneModelError, match="not activatable"):
        scene.require_interactable_control(owner, 1)
    _begin(clock, scene, 4, RetainedMode.DELTA)
    current = scene.state.active.owners[owner.owner_id].controls[5]
    scene.replace_control(replace(current, state=current.state | ControlState.CHECKED))
    _install(clock, scene, CommitDisposition.COMMIT)
    assert scene.state.active.owners[owner.owner_id].controls[5].state & ControlState.CHECKED

    _begin(clock, scene, 5, RetainedMode.DELTA)
    menu = scene.state.active.owners[owner.owner_id].controls[2]
    scene.replace_control(replace(menu, state=menu.state & ~ControlState.OPEN))
    _install(clock, scene, CommitDisposition.COMMIT)
    assert scene.require_interactable_control(owner, 2).label == "File"
    with pytest.raises(SceneModelError, match="closed menu"):
        scene.require_interactable_control(owner, 3)


def test_control_tree_rejects_wrong_parent_duplicate_order_and_two_open_menus():
    clock, _, owner, scene = _domain()
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_control(_control(owner, 1, ControlKind.MENU_BAR))
    with pytest.raises(SceneModelError) as wrong_parent:
        scene.define_control(
            _control(owner, 2, ControlKind.MENU_ITEM, parent=1, label="Impossible")
        )
    assert wrong_parent.value.code is SceneErrorCode.GRAPH
    rejected = scene.reject()
    clock.settle_result(rejected.transaction_id)

    _begin(clock, scene, 3, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_control(_control(owner, 1, ControlKind.MENU_BAR))
    open_state = ControlState.VISIBLE | ControlState.ENABLED | ControlState.OPEN
    scene.define_control(
        _control(owner, 2, ControlKind.MENU, parent=1, order=0, label="File", state=open_state)
    )
    scene.define_control(
        _control(owner, 3, ControlKind.MENU, parent=1, order=1, label="Edit", state=open_state)
    )
    with pytest.raises(SceneModelError, match="multiple open menus"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    rejected = scene.reject()
    clock.settle_result(rejected.transaction_id)

    _begin(clock, scene, 4, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_control(_control(owner, 1, ControlKind.MENU_BAR))
    scene.define_control(
        _control(owner, 2, ControlKind.MENU, parent=1, order=0, label="File")
    )
    scene.define_control(
        _control(owner, 3, ControlKind.MENU, parent=1, order=0, label="Edit")
    )
    with pytest.raises(SceneModelError, match="sibling order is duplicated"):
        scene.prepare_commit(CommitDisposition.COMMIT)
    rejected = scene.reject()
    clock.settle_result(rejected.transaction_id)


def test_controls_and_graphical_objects_share_the_declared_object_quota():
    clock, _, owner, scene = _domain(object_quota=2, object_capacity=2)
    _begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(_region(owner))
    scene.define_control(_control(owner, 1, ControlKind.MENU_BAR))
    scene.define_object(
        ObjectDefinition(
            owner=owner,
            object_id=1,
            region_id=1,
            parent_object_id=0,
            bounds=ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
            z_order=0,
            visible=True,
            body=GlyphRunBody(
                RGBA(255, 255, 255, 255),
                RGBA(0, 0, 0, 255),
                0,
                "",
            ),
        )
    )
    with pytest.raises(SceneModelError) as exhausted:
        scene.define_control(
            _control(owner, 2, ControlKind.MENU, parent=1, label="File")
        )
    assert exhausted.value.code is SceneErrorCode.QUOTA


def test_control_values_enforce_renderer_owned_child_geometry_and_clean_text():
    owner = OwnerIdentity(SESSION_ID, EPOCH, 7, 2)
    with pytest.raises(ValueError, match="renderer-owned geometry"):
        replace(
            _control(owner, 2, ControlKind.MENU, parent=1, label="File"),
            bounds=FULL_BOUNDS,
        )
    with pytest.raises(ValueError, match="control character"):
        _control(owner, 2, ControlKind.MENU, parent=1, label="Fi\tle")
    with pytest.raises(ValueError, match="requires no label|carries no label"):
        _control(
            owner,
            2,
            ControlKind.MENU_SEPARATOR,
            parent=1,
            label="wrong",
            state=ControlState.VISIBLE,
        )
