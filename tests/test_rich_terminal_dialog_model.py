"""DIALOG and BUTTON roots, modal regions, and TEXT field positions in the
retained scene (RETAINED-1 sections 9 to 9.4)."""

from dataclasses import replace

import pytest

from tests import test_rich_terminal_semantic_scene as helpers
from tests.test_rich_terminal_field_model import field, text_content
from rich_terminal.retained_model import OwnerIdentity, OwnerLedger, OwnerQuotas, RetainedFeature
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ControlDefinition, ControlKind, ControlState, ObjectBounds,
    RegionDefinition, RetainedMode, RetainedSceneModel, SceneModelError,
)
from rich_terminal.semantic_dialogs import ButtonContent, ButtonFlag, DialogContent, DialogFlag
from rich_terminal.semantic_fields import FieldRect
from rich_terminal.update_authority import TerminalUpdateAuthority


LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = (RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.FIELDS
            | RetainedFeature.DIALOGS)
NONE = FieldRect(0, 0, 0, 0)


def dialog_content(**changes):
    return DialogContent(**(dict(content_revision=1, flags=DialogFlag(0), severity=0,
                                 title_bounds=FieldRect(1, 0, 18, 1),
                                 message_bounds=FieldRect(1, 1, 18, 2),
                                 message="Delete notes.md?\nIt cannot be undone.") | changes))


def dialog(owner, **changes):
    return ControlDefinition(**(dict(owner=owner, control_id=10, kind=ControlKind.DIALOG,
                                     state=ControlState.VISIBLE, z_order=0, region_id=2,
                                     parent_control_id=0, order=0,
                                     bounds=ObjectBounds(0, 0, 20, 6), label="Delete",
                                     shortcut="", content=dialog_content()) | changes))


def button(owner, **changes):
    return ControlDefinition(**(dict(owner=owner, control_id=11, kind=ControlKind.BUTTON,
                                     state=LIVE, z_order=1, region_id=2, parent_control_id=0,
                                     order=0, bounds=ObjectBounds(2, 4, 8, 1), label="Delete",
                                     shortcut="", content=ButtonContent(ButtonFlag.DESTRUCTIVE))
                                | changes))


def modal_region(owner, **changes):
    # Rows 2 to 7 and columns 2 to 21 of the 24 by 12 surface, above region 1.
    return RegionDefinition(**(dict(owner=owner, region_id=2, logical_x=2, logical_y=2,
                                    logical_cols=20, logical_rows=6, clip_x=0, clip_y=0,
                                    clip_cols=0, clip_rows=0, z_order=10, visible=True,
                                    clipped=False, geometry_generation=0, modal=True)
                               | changes))


def domain(*, features=FEATURES, object_quota=12, utf8_quota=256):
    clock = TerminalUpdateAuthority(presentation_epoch=helpers.EPOCH, revision=1,
                                     transaction_high_water=1)
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    policy = replace(helpers._policy(), features=features, max_glyph_run_bytes=0)
    ledger = OwnerLedger(session_id=helpers.SESSION_ID, presentation_epoch=helpers.EPOCH,
                         policy=policy)
    ledger.open(owner, OwnerQuotas(2, 0, object_quota, 0, 0, utf8_quota, 0))
    scene = RetainedSceneModel(clock=clock, owners=ledger,
                              resources=RetainedResourceStore(ledger), geometry=helpers.GEOMETRY)
    return clock, ledger, owner, scene


def shown(build, **kwargs):
    """Commit and reveal one target that ``build(owner, scene)`` defines."""
    clock, ledger, owner, scene = domain(**kwargs)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    build(owner, scene)
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    helpers._install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    return clock, ledger, owner, scene


def with_dialog(owner, scene):
    scene.define_region(helpers._region(owner))
    scene.define_control(field(owner, content=text_content()))
    scene.define_region(modal_region(owner))
    scene.define_control(dialog(owner))
    scene.define_control(button(owner))


def test_a_dialog_and_its_button_are_roots_that_charge_slots_and_text():
    _, _, owner, scene = shown(with_dialog)
    usage = scene.state.active.owners[7].usage
    assert usage.objects == 3
    message = "Delete notes.md?\nIt cannot be undone."
    assert usage.utf8_bytes == len("text") + len("Gain") + len("Delete") * 2 + len(message.encode())
    assert scene.require_interactable_control(owner, 11).kind is ControlKind.BUTTON
    with pytest.raises(SceneModelError, match="not activatable"):
        scene.require_interactable_control(owner, 10)


def test_controls_beneath_a_modal_region_are_inactive_until_it_hides():
    clock, _, owner, scene = shown(with_dialog)
    with pytest.raises(SceneModelError, match="beneath an open modal region"):
        scene.require_field_position(owner, 1, content_revision=1, item_key=0, scalar_offset=0)
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_region(modal_region(owner, visible=False))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert scene.require_field_position(
        owner, 1, content_revision=1, item_key=0, scalar_offset=4).text == "text"


def test_a_control_outside_the_modal_barrier_stays_active():
    def build(owner, scene):
        scene.define_region(helpers._region(owner))
        # Row 10 lies below the modal rectangle's rows 2 to 7.
        scene.define_control(field(owner, bounds=ObjectBounds(1, 10, 20, 1),
                                   content=text_content()))
        scene.define_region(modal_region(owner))
    _, _, owner, scene = shown(build)
    assert scene.require_field_position(owner, 1, content_revision=1, item_key=0,
                                        scalar_offset=0)


def test_a_modal_region_beneath_the_control_does_not_make_it_inactive():
    def build(owner, scene):
        scene.define_region(helpers._region(owner))
        scene.define_control(field(owner, content=text_content()))
        scene.define_region(modal_region(owner, z_order=-5))
    _, _, owner, scene = shown(build)
    assert scene.require_field_position(owner, 1, content_revision=1, item_key=0,
                                        scalar_offset=0)


def test_a_fully_clipped_modal_region_has_no_barrier():
    def build(owner, scene):
        scene.define_region(helpers._region(owner))
        scene.define_control(field(owner, content=text_content()))
        scene.define_region(modal_region(owner, clipped=True))
    _, _, owner, scene = shown(build)
    assert scene.require_field_position(owner, 1, content_revision=1, item_key=0,
                                        scalar_offset=0)


def test_a_disabled_button_does_not_activate():
    def build(owner, scene):
        scene.define_region(helpers._region(owner))
        scene.define_control(button(owner, region_id=1, state=ControlState.VISIBLE))
    _, _, owner, scene = shown(build)
    with pytest.raises(SceneModelError, match="hidden or disabled"):
        scene.require_interactable_control(owner, 11)


@pytest.mark.parametrize("make", [
    lambda owner: dialog(owner, region_id=1),
    lambda owner: button(owner, region_id=1),
])
def test_dialogs_and_buttons_require_the_dialogs_feature(make):
    clock, _, owner, scene = domain(features=FEATURES & ~RetainedFeature.DIALOGS)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    with pytest.raises(SceneModelError, match="DIALOGS was not advertised"):
        scene.define_control(make(owner))


def test_a_modal_region_requires_the_dialogs_feature():
    clock, _, owner, scene = domain(features=FEATURES & ~RetainedFeature.DIALOGS)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    with pytest.raises(SceneModelError, match="MODAL region requires DIALOGS"):
        scene.define_region(modal_region(owner))


@pytest.mark.parametrize("changes", [
    {"parent_control_id": 1}, {"order": 1}, {"bounds": None}, {"shortcut": "D"},
    {"content": None}, {"content": ButtonContent()},
    {"state": LIVE}, {"state": ControlState.VISIBLE | ControlState.SELECTED},
    {"label": "Bad\x85"}, {"label": "Bad "},
    {"label": ""},                                     # the title slot needs a title
    {"content": dialog_content(title_bounds=FieldRect(0, 0, 21, 1))},  # past the root
    {"content": dialog_content(message_bounds=FieldRect(1, 0, 18, 2))},  # over the title
])
def test_dialog_shape_rejects_wrong_ancestry_state_text_or_geometry(changes):
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    with pytest.raises((TypeError, ValueError)):
        dialog(owner, **changes)


def test_an_untitled_notice_has_no_title_slot():
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    notice = dialog(owner, label="", bounds=ObjectBounds(0, 0, 20, 1), content=dialog_content(
        flags=DialogFlag.NOTICE, severity=4, title_bounds=NONE,
        message_bounds=FieldRect(0, 0, 20, 1), message="Could not save"))
    assert notice.content.notice and notice.content.lines == ("Could not save",)


@pytest.mark.parametrize("changes", [
    {"parent_control_id": 1}, {"order": 1}, {"bounds": None}, {"label": ""},
    {"content": None}, {"content": dialog_content()},
    {"state": LIVE | ControlState.OPEN}, {"state": LIVE | ControlState.CHECKED},
    {"state": ControlState.SELECTED}, {"shortcut": "Bad\x85"},
])
def test_button_shape_rejects_wrong_ancestry_state_text_or_content(changes):
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    with pytest.raises((TypeError, ValueError)):
        button(owner, **changes)


def test_a_button_may_change_its_label_shortcut_and_flags_in_place():
    clock, _, owner, scene = shown(with_dialog)
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_control(button(owner, label="Keep", shortcut="Esc",
                                 content=ButtonContent(ButtonFlag.CANCEL),
                                 state=LIVE | ControlState.SELECTED))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    current = scene.require_interactable_control(owner, 11)
    assert (current.label, current.shortcut, current.content.cancel) == ("Keep", "Esc", True)
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="BUTTON replacement"):
        scene.replace_control(button(owner, bounds=ObjectBounds(2, 5, 8, 1)))


def test_a_dialog_keeps_its_title_and_needs_a_newer_revision_for_new_content():
    clock, _, owner, scene = shown(with_dialog)
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="newer content revision"):
        scene.replace_control(dialog(owner, content=dialog_content(message="Other")))
    clock2, _, owner2, scene2 = shown(with_dialog)
    helpers._begin(clock2, scene2, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="only state and semantic content"):
        scene2.replace_control(dialog(owner2, label="Remove"))
    clock3, _, owner3, scene3 = shown(with_dialog)
    helpers._begin(clock3, scene3, 4, RetainedMode.DELTA)
    scene3.replace_control(dialog(owner3, content=dialog_content(
        content_revision=2, severity=3, message="Delete notes.md?")))
    helpers._install(clock3, scene3, CommitDisposition.COMMIT)
    active = scene3.state.active.owners[7].controls[10]
    assert active.content.severity == 3 and active.content.lines == ("Delete notes.md?",)


def test_a_dialog_that_does_not_fit_the_payload_is_refused():
    clock, _, owner, scene = domain()
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    long = "x" * 400  # 80 + 6 + 56 + 400 bytes pass the 512-byte payload maximum
    with pytest.raises(SceneModelError):
        scene.define_control(dialog(owner, region_id=1, content=dialog_content(
            message_bounds=FieldRect(1, 1, 18, 1), message=long)))
