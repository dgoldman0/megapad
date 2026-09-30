"""Structured status text remains bounded, explicit, and transaction-owned."""

from dataclasses import replace

import pytest

from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import (
    CommitDisposition, GroupBody, ObjectBounds, ObjectDefinition, ObjectKind,
    RetainedMode, SceneModelError, StatusFieldBody, StatusSeverity,
)
from tests.test_rich_terminal_pane_model import (
    OWNER, begin, domain, install, policy as base_policy, regions,
)


def policy(**changes):
    return base_policy(**(dict(features=RetainedFeature.CORE | RetainedFeature.STATUS_FIELDS)
                          | changes))


def field(**changes):
    values = dict(owner=OWNER, object_id=1, region_id=1, parent_object_id=0,
                  bounds=ObjectBounds(-2, 4, 20, 1), z_order=0, visible=True,
                  body=StatusFieldBody("State", "Ready", 8))
    return ObjectDefinition(**(values | changes))


def start(*, selected_policy=None, utf8_quota=256):
    clock, owners, scene = domain(selected_policy=selected_policy or policy(),
                                  utf8_quota=utf8_quota)
    begin(clock, scene)
    scene.define_region(regions()[0])
    return clock, owners, scene


def reveal(**kwargs):
    clock, owners, scene = start(**kwargs)
    scene.define_object(field())
    install(clock, scene, CommitDisposition.COMMIT)
    begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    install(clock, scene)
    return clock, owners, scene


def test_status_fields_have_core_only_capacity_and_exact_empty_payload_floors():
    assert RetainedFeature.STATUS_FIELDS == 1 << 12
    assert field().kind is ObjectKind.STATUS_FIELD
    assert policy(max_regions=1, client_to_terminal_max_payload=96,
                  max_retained_transaction_bytes=296).features == (
                      RetainedFeature.CORE | RetainedFeature.STATUS_FIELDS)
    for changes, message in (
        ({"client_to_terminal_max_payload": 95}, "96-byte"),
        ({"max_retained_transaction_bytes": 295}, "transaction maximum"),
        ({"max_objects": 0}, "object capacity"),
        ({"total_utf8_bytes": 0}, "UTF-8 capacity"),
    ):
        with pytest.raises(ValueError, match=message):
            policy(**changes)


@pytest.mark.parametrize("text", ["\0", "\t", "\n", "\r", "\x7f", "\x85", "\x9f", "\u2028", "\u2029", "\ud800"])
@pytest.mark.parametrize("slot", ["label", "value"])
def test_each_text_slot_requires_clean_unicode_scalar_line(text, slot):
    with pytest.raises(ValueError):
        StatusFieldBody(**(dict(label="Key", value="Value", label_cols=4) | {slot: text}))


@pytest.mark.parametrize("changes", [
    {"label_cols": -1}, {"label_cols": True}, {"label_cols": 2**32},
    {"severity": -1}, {"severity": 5}, {"severity": True},
    {"severity": 1.5}, {"emphasized": 1}, {"label": b"bytes"},
    {"value": None},
])
def test_semantic_fields_reject_reserved_values_and_accidental_coercions(changes):
    with pytest.raises((TypeError, ValueError)):
        StatusFieldBody(**(dict(label="Key", value="Value", label_cols=4) | changes))


@pytest.mark.parametrize("body,bounds", [
    (StatusFieldBody("Key", "Value", 4), ObjectBounds(0, 0, 10, 2)),
    (StatusFieldBody("Key", "", 11), ObjectBounds(0, 0, 10, 1)),
    (StatusFieldBody("Key", "Value", 10), ObjectBounds(0, 0, 10, 1)),
])
def test_status_envelope_rejects_reflow_or_missing_slots(body, bounds):
    with pytest.raises(ValueError):
        field(body=body, bounds=bounds)


def test_empty_slots_are_canonical_and_reserved_label_space_is_explicit():
    with pytest.raises(ValueError, match="label columns"):
        StatusFieldBody("Key", "", 0)
    for body in (StatusFieldBody("", "Ready", 0),
                 StatusFieldBody("", "Ready", 5),
                 StatusFieldBody("Label", "", 20),
                 StatusFieldBody("", "", 20)):
        assert field(body=body).body == body
    assert StatusFieldBody("K", "V", 1, 3).severity is StatusSeverity.WARNING


def test_unicode_bytes_are_counted_across_both_slots_without_glyph_capacity():
    clock, owners, scene = reveal()
    assert scene.state.active.owners[1].usage.utf8_bytes == 10
    old_state = scene.state
    begin(clock, scene, 4, RetainedMode.DELTA)
    updated = field(body=StatusFieldBody("茶", "e\u0301", 4, StatusSeverity.SUCCESS, True))
    scene.replace_object(updated)
    prepared = scene.prepare_commit(CommitDisposition.COMMIT)
    assert scene.state is old_state
    result = scene.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    owner = scene.state.active.owners[1]
    assert owner.usage.objects == 1
    assert owner.usage.utf8_bytes == 6
    assert owner.objects[1] == updated
    assert owners.require_live(OWNER).high_water.object == 1
    begin(clock, scene, 5, RetainedMode.DELTA)
    scene.drop_object(OWNER, 1)
    install(clock, scene, CommitDisposition.COMMIT)
    assert scene.state.active.owners[1].usage.objects == 0
    assert scene.state.active.owners[1].usage.utf8_bytes == 0


def test_combined_text_owner_quota_rejects_atomically_and_allows_retry_after_abort():
    clock, owners, scene = start(utf8_quota=9)
    before = scene.state
    with pytest.raises(SceneModelError, match="UTF-8-byte"):
        scene.define_object(field())
    assert scene.state is before
    assert owners.require_live(OWNER).high_water.object == 0
    scene.abort()
    begin(clock, scene, 3)
    scene.define_region(regions()[0])
    scene.define_object(field(body=StatusFieldBody("State", "OK", 8)))
    install(clock, scene, CommitDisposition.COMMIT)
    assert owners.require_live(OWNER).high_water.object == 1


def test_object_slot_quota_is_not_bypassed_by_status_text():
    clock, owners, scene = start()
    for object_id in range(1, 9):
        scene.define_object(field(object_id=object_id, body=StatusFieldBody("", "", 0)))
    with pytest.raises(SceneModelError, match="object usage"):
        scene.define_object(field(object_id=9))
    assert not scene.state.active.owners


def test_status_text_obeys_frame_limit_even_with_available_aggregate_quota():
    _, _, scene = start(selected_policy=policy(client_to_terminal_max_payload=172))
    with pytest.raises(SceneModelError, match="payload or transaction capacity"):
        scene.define_object(field(body=StatusFieldBody("L", "x" * 76, 1)))


def test_status_text_obeys_complete_transaction_limit():
    # This geometry needs 2336 bytes for its CELL baseline; bound text so the
    # complete status frame plus BEGIN/COMMIT crosses that independent limit.
    selected = policy(client_to_terminal_max_payload=4096,
                      max_retained_transaction_bytes=2336,
                      total_utf8_bytes=4096)
    _, _, scene = start(selected_policy=selected, utf8_quota=4096)
    with pytest.raises(SceneModelError, match="payload or transaction capacity"):
        scene.define_object(field(body=StatusFieldBody("L", "x" * 2040, 1)))


def test_unadvertised_status_fields_reject_before_mutation():
    _, _, scene = start(selected_policy=policy(features=RetainedFeature.CORE,
                                              total_utf8_bytes=0), utf8_quota=0)
    with pytest.raises(SceneModelError, match="STATUS_FIELD feature was not advertised"):
        scene.define_object(field())
    assert not scene.state.active.owners


def test_group_parent_uses_existing_vector_authority_without_new_ancestry_rules():
    clock, _, scene = start(selected_policy=policy(
        features=RetainedFeature.CORE | RetainedFeature.STATUS_FIELDS | RetainedFeature.VECTOR,
        max_path_points=2))
    scene.define_object(field(body=GroupBody(), bounds=ObjectBounds(0, 0, 20, 10)))
    scene.define_object(field(object_id=2, parent_object_id=1))
    install(clock, scene, CommitDisposition.COMMIT)
    begin(clock, scene, 3, RetainedMode.REPLACE_CONTINUE)
    install(clock, scene)
    assert scene.state.active.owners[1].objects[2].parent_object_id == 1
    assert not scene.state.active.owners[1].controls


def test_status_fields_do_not_acquire_numeric_set_value_semantics():
    clock, _, scene = reveal()
    before = scene.state
    begin(clock, scene, 4, RetainedMode.DELTA)
    with pytest.raises(SceneModelError, match="OBJECT_SET_VALUE requires"):
        scene.set_object_value(OWNER, 1, 10)
    assert scene.state is before


def test_hide_retains_text_and_object_quota():
    clock, _, scene = reveal()
    begin(clock, scene, 4, RetainedMode.DELTA)
    scene.set_object_visibility(OWNER, 1, False)
    install(clock, scene, CommitDisposition.COMMIT)
    owner = scene.state.active.owners[1]
    assert not owner.objects[1].visible
    assert owner.usage.utf8_bytes == 10
    assert owner.usage.objects == 1
