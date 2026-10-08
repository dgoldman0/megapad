"""Capability-gated typed grid roles retain the STX1 authority and bounds."""

from dataclasses import FrozenInstanceError, replace

import pytest

from tests import test_rich_terminal_semantic_scene as helpers
from rich_terminal.retained_model import OwnerIdentity, OwnerLedger, OwnerQuotas, RetainedFeature
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ControlDefinition, ControlKind, ControlState, ObjectBounds,
    RetainedMode, RetainedSceneModel, SceneModelError,
)
from rich_terminal.semantic_content import (
    GRID_DATA_ROLES, SemanticContentFlag, SemanticTextContent, SemanticTextItem,
    SemanticTextRole, SemanticTextState, StyleRun, TextStyle,
)
from rich_terminal.update_authority import TerminalUpdateAuthority


LIVE = ControlState.VISIBLE | ControlState.ENABLED
BASE_FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.CONTROL_COLLECTIONS
FEATURES = BASE_FEATURES | RetainedFeature.GRID_CELLS


def item(key=2, row=1, column=0, role=SemanticTextRole.NUMBER, **changes):
    return SemanticTextItem(**(dict(item_key=key, row=row, column=column,
                                    row_span=1, column_span=2, role=role,
                                    state=SemanticTextState(0), text="1,234.50") | changes))


def content(**changes):
    return SemanticTextContent(**(dict(
        content_revision=7, rows=3, columns=6,
        viewport_row=0, viewport_column=0, viewport_rows=3, viewport_columns=6,
        flags=SemanticContentFlag.READ_ONLY, primary_key=3, primary_offset=0,
        anchor_key=0, anchor_offset=0, items=(
            item(1, 0, 0, SemanticTextRole.COLUMN_HEADER, column_span=6, text="Header"),
            item(state=SemanticTextState.CURRENT),
            item(3, 1, 2, SemanticTextRole.FORMULA, text="42.0"),
            item(4, 1, 4, SemanticTextRole.ERROR, text="#ERR"),
            item(5, 2, 0, SemanticTextRole.CONTENT, text="Tea"),
        )) | changes))


def grid(owner, **changes):
    return ControlDefinition(**(dict(owner=owner, control_id=1, kind=ControlKind.TEXT_GRID,
                                     state=LIVE, z_order=2, region_id=1,
                                     parent_control_id=0, order=0, bounds=ObjectBounds(0, 4, 24, 4),
                                     label="", shortcut="", content=content()) | changes))


def domain(*, typed=True, object_quota=12, utf8_quota=192):
    clock = TerminalUpdateAuthority(presentation_epoch=helpers.EPOCH, revision=1,
                                     transaction_high_water=1)
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    policy = replace(helpers._policy(control_collections=True),
                     features=FEATURES if typed else BASE_FEATURES, max_glyph_run_bytes=0)
    ledger = OwnerLedger(session_id=helpers.SESSION_ID, presentation_epoch=helpers.EPOCH,
                         policy=policy)
    ledger.open(owner, OwnerQuotas(1, 0, object_quota, 0, 0, utf8_quota, 0))
    scene = RetainedSceneModel(clock=clock, owners=ledger,
                              resources=RetainedResourceStore(ledger), geometry=helpers.GEOMETRY)
    return clock, ledger, owner, scene


def plain_content():
    value = content()
    return replace(value, items=tuple(replace(cell, role=SemanticTextRole.CONTENT)
                                      if cell.role in GRID_DATA_ROLES else cell
                                      for cell in value.items))


def visible(*, selected_content=None, **kwargs):
    clock, ledger, owner, scene = domain(**kwargs)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(grid(owner, content=selected_content or content()))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    helpers._install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    return clock, ledger, owner, scene


def position(scene, owner, key, **changes):
    return scene.require_text_position(owner, 1, **(dict(grid_allowed=True,
                                                        content_revision=7,
                                                        item_key=key, scalar_offset=0) | changes))


def test_typed_roles_add_one_cached_capability_summary_without_changing_quotas():
    value = content()
    assert value.requires_grid_cells
    assert not plain_content().requires_grid_cells
    assert GRID_DATA_ROLES == frozenset((SemanticTextRole.CONTENT, SemanticTextRole.NUMBER,
                                        SemanticTextRole.FORMULA, SemanticTextRole.ERROR))
    assert not value.text_area_compatible
    with pytest.raises(FrozenInstanceError):
        value.requires_grid_cells = False
    _, ledger, owner, scene = visible()
    usage = scene.state.active.owners[7].usage
    assert usage.objects == 1 + len(value.items)
    assert usage.utf8_bytes == sum(len(cell.text.encode()) for cell in value.items)
    assert ledger.policy.features == FEATURES


@pytest.mark.parametrize("role", [SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR])
def test_even_offscreen_typed_role_requires_opt_in(role):
    value = content(items=(item(role=role, row=2),), primary_key=2,
                    viewport_rows=1, viewport_columns=6)
    assert value.requires_grid_cells
    clock, ledger, owner, scene = domain(typed=False)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    before = scene.state
    with pytest.raises(SceneModelError, match="GRID_CELLS was not advertised"):
        scene.define_control(grid(owner, content=value))
    result = scene.reject()
    clock.settle_result(result.transaction_id)
    assert scene.state is before
    assert ledger.require_live(owner).high_water.control == 0
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(grid(owner, content=plain_content()))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert ledger.require_live(owner).high_water.control == 1


def test_legacy_content_grids_remain_valid_under_old_profile():
    _, _, owner, scene = visible(typed=False, selected_content=plain_content())
    assert position(scene, owner, 2).role is SemanticTextRole.CONTENT
    assert not scene.state.active.owners[7].controls[1].content.requires_grid_cells


def test_all_data_roles_allow_whole_cell_selection_even_when_content_is_readonly():
    _, _, owner, scene = visible()
    for key, role in ((2, SemanticTextRole.NUMBER), (3, SemanticTextRole.FORMULA),
                      (4, SemanticTextRole.ERROR), (5, SemanticTextRole.CONTENT)):
        assert position(scene, owner, key).role is role
        with pytest.raises(SceneModelError, match="selectable content"):
            position(scene, owner, key, scalar_offset=1)
    with pytest.raises(SceneModelError, match="selectable content"):
        position(scene, owner, 1)


@pytest.mark.parametrize("role", tuple(GRID_DATA_ROLES))
def test_unavailable_typed_or_plain_items_cannot_be_selected(role):
    value = content(items=(item(role=role, state=SemanticTextState.UNAVAILABLE),), primary_key=0)
    _, _, owner, scene = visible(selected_content=value)
    with pytest.raises(SceneModelError, match="selectable content"):
        position(scene, owner, 2)


def test_typed_grid_selection_preserves_revision_and_current_identity_rules():
    clock, _, owner, scene = visible()
    value = scene.state.active.owners[7].controls[1].content
    assert value.primary_key == 3 and value.items[1].state & SemanticTextState.CURRENT
    with pytest.raises(SceneModelError, match="superseded"):
        position(scene, owner, 3, content_revision=6)
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    changed = replace(value, items=tuple(replace(cell, role=SemanticTextRole.NUMBER)
                                        if cell.item_key == 3 else cell for cell in value.items))
    with pytest.raises(SceneModelError, match="newer content revision"):
        scene.replace_control(grid(owner, content=changed))
    result = scene.reject()
    clock.settle_result(result.transaction_id)
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    scene.replace_control(grid(owner, content=replace(changed, content_revision=8)))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert position(scene, owner, 3, content_revision=8).role is SemanticTextRole.NUMBER


@pytest.mark.parametrize("role", [SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR])
def test_text_area_rejects_grid_role_even_if_other_text_area_constraints_hold(role):
    value = content(rows=1, columns=6, viewport_rows=1,
                    primary_key=0, items=(item(row=0, role=role, column_span=6, text="42"),))
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    assert not value.text_area_compatible
    with pytest.raises(ValueError, match="full-row"):
        grid(owner, kind=ControlKind.TEXT_AREA, content=value)


def test_grid_still_rejects_style_runs_multiple_current_cells_and_offsets():
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    value = content()
    styled = replace(value, items=tuple(replace(cell, runs=(StyleRun(0, 1, TextStyle.NUMBER),))
                                        if cell.item_key == 2 else cell for cell in value.items))
    with pytest.raises(ValueError, match="no style runs"):
        grid(owner, content=styled)
    two_current = replace(value, items=tuple(replace(cell, state=SemanticTextState.CURRENT)
                                             if cell.item_key == 3 else cell for cell in value.items))
    with pytest.raises(ValueError, match="more than one current"):
        grid(owner, content=two_current)
    with pytest.raises(ValueError, match="zero offsets"):
        grid(owner, content=replace(value, primary_offset=1))


@pytest.mark.parametrize("changes", [{"row": 3}, {"column": 6}, {"column_span": 7}, {"row_span": 3}])
def test_typed_role_keeps_canonical_item_bounds(changes):
    with pytest.raises(ValueError, match="bounds"):
        content(items=(item(**changes),), primary_key=2)


def test_typed_role_does_not_parse_numbers_or_evaluate_formula_text():
    # Display values originate in the guest; role metadata never inspects
    # numeric spelling or treats the text as a host expression.
    values = (item(role=SemanticTextRole.NUMBER, text="unknown — 茶"),
              item(3, 1, 2, SemanticTextRole.FORMULA, text="=SUM(A1:A9)"),
              item(4, 1, 4, SemanticTextRole.ERROR, text="Waiting"))
    _, _, _, scene = visible(selected_content=content(items=values))
    assert scene.state.active.owners[7].controls[1].content.items == values


@pytest.mark.parametrize("quotas,match", [({"object_quota": 5}, "object usage"),
                                          ({"utf8_quota": 24}, "UTF-8-byte")])
def test_typed_roles_keep_ordinary_collection_quota_charges(quotas, match):
    clock, _, owner, scene = domain(**quotas)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    with pytest.raises(SceneModelError, match=match):
        scene.define_control(grid(owner))
