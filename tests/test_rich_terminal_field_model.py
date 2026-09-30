"""FIELD authority, replacement, quota and revision-bound adjustment rules."""

from dataclasses import replace

import pytest

from tests import test_rich_terminal_semantic_scene as helpers
from rich_terminal.retained_model import OwnerIdentity, OwnerLedger, OwnerQuotas, RetainedFeature
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ControlDefinition, ControlKind, ControlState, ObjectBounds,
    RetainedMode, RetainedSceneModel, SceneModelError,
)
from rich_terminal.semantic_fields import FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect
from rich_terminal.update_authority import TerminalUpdateAuthority


LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.FIELDS


def content(**changes):
    return FieldContent(**(dict(content_revision=1, kind=FieldKind.INTEGER,
                               flags=FieldFlag(0), label_bounds=FieldRect(0, 0, 5, 1),
                               value_bounds=FieldRect(5, 0, 15, 1), value=3,
                               minimum=-10, maximum=10, step=2) | changes))


def choices(**changes):
    return content(**(dict(kind=FieldKind.CHOICE, value=-7, minimum=0,
                           maximum=0, step=0,
                           choices=(FieldChoice(-7, "茶"), FieldChoice(14, "Long"))) | changes))


def text_content(**changes):
    return content(**(dict(kind=FieldKind.TEXT, value=0, minimum=0,
                           maximum=0, step=0, text="text") | changes))


def field(owner, **changes):
    return ControlDefinition(**(dict(owner=owner, control_id=1, kind=ControlKind.FIELD,
                                     state=LIVE, z_order=-2, region_id=1, parent_control_id=0,
                                     order=0, bounds=ObjectBounds(1, 4, 20, 1),
                                     label="Gain", shortcut="", content=content()) | changes))


def domain(*, enabled=True, object_quota=12, utf8_quota=192, **policy_changes):
    clock = TerminalUpdateAuthority(presentation_epoch=helpers.EPOCH, revision=1,
                                     transaction_high_water=1)
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    policy = replace(helpers._policy(), **(dict(features=FEATURES if enabled else FEATURES & ~RetainedFeature.FIELDS,
                                                max_glyph_run_bytes=0) | policy_changes))
    ledger = OwnerLedger(session_id=helpers.SESSION_ID, presentation_epoch=helpers.EPOCH,
                         policy=policy)
    ledger.open(owner, OwnerQuotas(1, 0, object_quota, 0, 0, utf8_quota, 0))
    scene = RetainedSceneModel(clock=clock, owners=ledger,
                              resources=RetainedResourceStore(ledger), geometry=helpers.GEOMETRY)
    return clock, ledger, owner, scene


def visible(*, definition_changes=None, **kwargs):
    clock, ledger, owner, scene = domain(**kwargs)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(field(owner, **(definition_changes or {})))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    helpers._install(clock, scene, CommitDisposition.COMMIT_AND_REVEAL)
    return clock, ledger, owner, scene


def reject(clock, scene):
    result = scene.reject()
    clock.settle_result(result.transaction_id)


def test_field_is_a_root_control_with_explicit_label_and_value_slots():
    _, ledger, owner, scene = visible()
    definition = scene.require_interactable_control(owner, 1)
    assert definition.kind is ControlKind.FIELD
    assert definition.bounds == ObjectBounds(1, 4, 20, 1)
    assert scene.require_field_control(owner, 1, content_revision=1, adjustable=True) == definition
    assert ledger.require_live(owner).high_water.control == 1
    assert scene.state.active.owners[7].usage.objects == 1
    assert scene.state.active.owners[7].usage.utf8_bytes == 4


@pytest.mark.parametrize("changes", [
    {"parent_control_id": 1}, {"order": 1}, {"bounds": None}, {"shortcut": "A"},
    {"content": None}, {"label": ""}, {"label": "Bad\x85"}, {"label": "Bad\u2028"},
    {"state": LIVE | ControlState.OPEN}, {"state": LIVE | ControlState.CHECKED},
    {"state": LIVE | ControlState.MINIMIZED}, {"state": ControlState.SELECTED},
    {"content": content(value_bounds=FieldRect(4, 0, 15, 1))},
])
def test_field_shape_rejects_wrong_ancestry_state_or_slot_contract(changes):
    owner = OwnerIdentity(helpers.SESSION_ID, helpers.EPOCH, 7, 2)
    with pytest.raises((TypeError, ValueError)):
        field(owner, **changes)


def test_unlabeled_field_uses_canonical_empty_label_slot():
    _, _, owner, scene = visible(definition_changes=dict(
        label="", content=content(label_bounds=FieldRect(0, 0, 0, 0),
                                  value_bounds=FieldRect(0, 0, 20, 1))))
    assert scene.require_field_control(owner, 1).label == ""


def test_changed_content_requires_new_revision_and_preserves_active_state_until_commit():
    clock, _, owner, scene = visible()
    original = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="newer content revision"):
        scene.replace_control(field(owner, content=content(value=5)))
    reject(clock, scene)
    assert scene.state is original
    helpers._begin(clock, scene, 5, RetainedMode.DELTA)
    replacement = field(owner, content=choices(content_revision=2))
    scene.replace_control(replacement)
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is original
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    assert scene.state.active.owners[7].controls[1] == replacement
    assert scene.state.active.owners[7].usage.objects == 3
    assert scene.state.active.owners[7].usage.utf8_bytes == 11
    with pytest.raises(SceneModelError, match="superseded"):
        scene.require_field_control(owner, 1, content_revision=1, adjustable=True)
    assert scene.require_field_control(owner, 1, content_revision=2, adjustable=True) == replacement


@pytest.mark.parametrize("changes", [
    {"label": "Mode"}, {"bounds": ObjectBounds(2, 4, 20, 1)},
    {"z_order": 2}, {"region_id": 2},
])
def test_field_replacement_keeps_label_identity_and_outer_geometry(changes):
    clock, _, owner, scene = visible()
    before = scene.state
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="only state and semantic content"):
        scene.replace_control(field(owner, **changes))
    reject(clock, scene)
    assert scene.state is before


def test_state_only_replacement_does_not_require_a_new_content_revision():
    clock, _, owner, scene = visible()
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_control(field(owner, state=LIVE | ControlState.SELECTED))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert scene.require_field_control(owner, 1).content.content_revision == 1


@pytest.mark.parametrize("changes", [{"content": content(flags=FieldFlag.READ_ONLY)},
                                      {"state": ControlState.VISIBLE}, {"state": ControlState(0)}])
def test_activation_and_adjustment_reject_readonly_disabled_and_hidden_fields(changes):
    _, _, owner, scene = visible(definition_changes=changes)
    for check in (lambda: scene.require_interactable_control(owner, 1),
                  lambda: scene.require_field_control(owner, 1, content_revision=1, adjustable=True)):
        with pytest.raises(SceneModelError):
            check()


def test_text_field_accepts_activation_but_not_adjustment():
    _, _, owner, scene = visible(definition_changes={"content": text_content()})
    assert scene.require_interactable_control(owner, 1).content.text == "text"
    with pytest.raises(SceneModelError, match="does not accept ADJUST"):
        scene.require_field_control(owner, 1, content_revision=1, adjustable=True)


@pytest.mark.parametrize("kwargs", [{"adjustable": True}, {"content_revision": 0},
                                    {"content_revision": True}, {"content_revision": 2},
                                    {"adjustable": 1}])
def test_field_intent_requires_canonical_current_revision(kwargs):
    _, _, owner, scene = visible()
    with pytest.raises(SceneModelError):
        scene.require_field_control(owner, 1, **kwargs)


def test_hidden_region_and_wrong_owner_generation_cannot_authorize_field_intent():
    clock, _, owner, scene = visible()
    with pytest.raises(SceneModelError):
        scene.require_field_control(replace(owner, owner_generation=3), 1)
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.replace_region(replace(helpers._region(owner), visible=False))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    with pytest.raises(SceneModelError, match="region is not visible"):
        scene.require_field_control(owner, 1)


@pytest.mark.parametrize("object_quota,utf8_quota,match", [(2, 192, "object usage"), (12, 10, "UTF-8-byte")])
def test_choice_records_charge_each_slot_and_exact_label_bytes_atomically(object_quota, utf8_quota, match):
    clock, ledger, owner, scene = domain(object_quota=object_quota, utf8_quota=utf8_quota)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    before = scene.state
    with pytest.raises(SceneModelError, match=match):
        scene.define_control(field(owner, content=choices()))
    reject(clock, scene)
    assert scene.state is before
    assert ledger.require_live(owner).high_water.control == 0
    helpers._begin(clock, scene, 3, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    scene.define_control(field(owner))
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    assert ledger.require_live(owner).high_water.control == 1


def test_dropping_field_releases_label_choice_text_and_slots():
    clock, _, owner, scene = visible(definition_changes={"content": choices()})
    helpers._begin(clock, scene, 4, RetainedMode.DELTA)
    scene.drop_control(owner, 1)
    helpers._install(clock, scene, CommitDisposition.COMMIT)
    usage = scene.state.active.owners[7].usage
    assert usage.objects == usage.utf8_bytes == 0


def test_feature_admission_is_independent_from_existing_collections():
    clock, _, owner, scene = domain(enabled=False)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    with pytest.raises(SceneModelError, match="FIELDS was not advertised"):
        scene.define_control(field(owner))


@pytest.mark.parametrize("options,text_size", [
    ({"client_to_terminal_max_payload": 244}, 65),
    ({"client_to_terminal_max_payload": 4096, "max_retained_transaction_bytes": 3144,
      "total_utf8_bytes": 4096, "utf8_quota": 4096}, 2765),
])
def test_field_complete_payload_and_transaction_fit_are_independent_of_utf8_quota(options, text_size):
    clock, _, owner, scene = domain(**options)
    helpers._begin(clock, scene, 2, RetainedMode.REPLACE_START)
    scene.define_region(helpers._region(owner))
    with pytest.raises(SceneModelError, match="payload or transaction capacity"):
        scene.define_control(field(owner, content=text_content(text="x" * text_size)))
