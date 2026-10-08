"""FIELD events preserve exact authority and bounded driver retention."""

from dataclasses import replace

import pytest

from tests import test_rich_terminal_semantic_driver as helpers
from rich_terminal.apt1 import IncrementalFrameDecoder, MessageType
from rich_terminal.driver import DriverStatus
from rich_terminal.retained_model import OwnerLedger, OwnerQuotas, RetainedFeature
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_scene import (
    CommitDisposition, ControlDefinition, ControlKind, ControlState, ObjectBounds,
    RetainedMode, RetainedSceneModel,
)
from rich_terminal.retained_wire import ControlEventKind, decode_control_event
from rich_terminal.semantic_fields import FieldContent, FieldFlag, FieldKind, FieldRect
from rich_terminal.update_authority import TransactionFamily


FIELD_ID = 4
CONTENT_REVISION = 7


def _active_field_core(*, read_only=False):
    core = helpers._active_core()
    policy = replace(helpers._policy(controls=True),
                     features=RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.FIELDS)
    old_owner = core.retained_state.active.owners[helpers.OWNER_ID]
    owner = old_owner.owner
    owners = OwnerLedger(session_id=helpers.SESSION_ID, presentation_epoch=0, policy=policy)
    owners.open(owner, OwnerQuotas(1, 0, 4, 0, 0, 64, 0))
    scene = RetainedSceneModel(clock=core._clock, owners=owners,
                              resources=RetainedResourceStore(owners),
                              geometry=core.retained_state.geometry)
    content = FieldContent(CONTENT_REVISION, FieldKind.INTEGER,
                           FieldFlag.READ_ONLY if read_only else FieldFlag(0),
                           FieldRect(0, 0, 0, 0), FieldRect(0, 0, 2, 1),
                           value=3, minimum=0, maximum=100, step=2)
    definition = ControlDefinition(owner, FIELD_ID, ControlKind.FIELD,
                                   ControlState.VISIBLE | ControlState.ENABLED,
                                   0, 1, 0, 0, ObjectBounds(0, 0, 2, 1), "", "", content)
    clock = core._clock
    lease = clock.reserve(TransactionFamily.PRESENT, 12, clock.revision)
    scene.begin(lease, RetainedMode.REPLACE_START, scene.state.geometry)
    scene.define_region(old_owner.regions[1])
    scene.define_control(definition)
    result = scene.install_prepared(scene.prepare_commit(CommitDisposition.COMMIT))
    clock.settle_result(result.transaction_id)
    lease = clock.reserve(TransactionFamily.PRESENT, 13, clock.revision)
    scene.begin(lease, RetainedMode.REPLACE_CONTINUE, scene.state.geometry)
    result = scene.install_prepared(scene.prepare_commit(CommitDisposition.COMMIT_AND_REVEAL))
    clock.settle_result(result.transaction_id)
    core._retained_model = scene
    core._owner_ledger = owners
    core._session_retained_policy = policy
    return core


def _adjust(driver, core, **changes):
    fields = dict(model_revision=core.model_revision, event_kind=ControlEventKind.ADJUST,
                  content_revision=CONTENT_REVISION, adjustment=-2)
    fields.update(changes)
    return driver.send_control_event(helpers.OWNER_ID, helpers.OWNER_GENERATION,
                                     FIELD_ID, **fields)


def _events(driver):
    decoder = IncrementalFrameDecoder(helpers.SESSION_ID, max_payload=512)
    return tuple(frame for pending in driver._pending
                 for frame in decoder.feed(pending.record.payload))


def test_adjust_retains_one_exact_96_byte_frame_without_mutating_guest_value():
    core = _active_field_core()
    driver = helpers._driver(core)
    before = core.retained_state
    assert _adjust(driver, core) is DriverStatus.PROGRESS
    assert driver.pending_outbound_bytes == 96
    assert driver.pending_outbound_events == 1
    frame, = _events(driver)
    assert frame.message_type == MessageType.CONTROL_EVENT
    assert len(frame.payload) == 56
    event = decode_control_event(frame.payload)
    assert event.event_kind is ControlEventKind.ADJUST
    assert (event.model_revision, event.content_revision, event.adjustment) == (13, 7, -2)
    assert (event.item_key, event.scalar_offset, event.wheel_x, event.wheel_y) == (0, 0, 0, 0)
    assert core.retained_state is before
    assert before.active.owners[helpers.OWNER_ID].controls[FIELD_ID].content.value == 3


def test_field_activate_keeps_existing_40_byte_payload():
    core = _active_field_core()
    driver = helpers._driver(core)
    assert driver.send_control_event(helpers.OWNER_ID, helpers.OWNER_GENERATION,
                                     FIELD_ID, model_revision=core.model_revision) is DriverStatus.PROGRESS
    frame, = _events(driver)
    assert len(frame.payload) == 40
    assert driver.pending_outbound_bytes == 80
    assert decode_control_event(frame.payload).event_kind is ControlEventKind.ACTIVATE


def test_adjust_uses_fields_capability_without_requiring_collections():
    core = _active_field_core()
    assert not core._session_retained_policy.features & RetainedFeature.CONTROL_COLLECTIONS
    driver = helpers._driver(core)
    assert _adjust(driver, core) is DriverStatus.PROGRESS
    core._session_retained_policy = replace(core._session_retained_policy,
                                            features=RetainedFeature.CORE | RetainedFeature.CONTROLS)
    assert _adjust(driver, core) is DriverStatus.INVALID
    assert driver.pending_outbound_events == 1


def test_adjust_preflights_complete_frame_before_consuming_credit(monkeypatch):
    core = _active_field_core()
    driver = helpers._driver(core)
    before = core.retained_state
    before_sent = core._server_data_sent
    checks = []
    monkeypatch.setattr(driver, "_can_retain", lambda size, events: checks.append((size, events)) or False)
    assert _adjust(driver, core) is DriverStatus.BACKPRESSURED
    assert checks == [(96, 1)]
    assert core._server_data_sent == before_sent
    assert driver.pending_outbound_events == driver.pending_outbound_bytes == 0
    assert core.retained_state is before


def test_credit_backpressure_does_not_queue_or_rebind_field_revision_on_retry():
    core = _active_field_core()
    core._server_data_grant = 95
    core._server_data_sent = 0
    driver = helpers._driver(core)
    assert _adjust(driver, core) is DriverStatus.BACKPRESSURED
    assert driver.pending_outbound_events == driver.pending_outbound_bytes == 0
    assert core._server_data_sent == 0
    scene = core._retained_model
    clock = core._clock
    current = scene.state.active.owners[helpers.OWNER_ID].controls[FIELD_ID]
    lease = clock.reserve(TransactionFamily.PRESENT, 14, clock.revision)
    scene.begin(lease, RetainedMode.DELTA, scene.state.geometry)
    scene.replace_control(replace(current, content=replace(current.content, content_revision=8, value=5)))
    result = scene.install_prepared(scene.prepare_commit(CommitDisposition.COMMIT))
    clock.settle_result(result.transaction_id)
    core._server_data_grant = 8192
    assert _adjust(driver, core) is DriverStatus.INVALID
    assert driver.pending_outbound_events == 0
    assert _adjust(driver, core, content_revision=8) is DriverStatus.PROGRESS
    frame, = _events(driver)
    assert decode_control_event(frame.payload).content_revision == 8


@pytest.mark.parametrize("changes", [
    {"adjustment": 0}, {"adjustment": True}, {"adjustment": 1 << 63},
    {"content_revision": 0}, {"content_revision": 6},
    {"model_revision": 12}, {"item_key": 1}, {"wheel_y": 1},
])
def test_invalid_field_intent_preserves_scene_and_emits_no_frame(changes):
    core = _active_field_core()
    driver = helpers._driver(core)
    before = core.retained_state
    assert _adjust(driver, core, **changes) is DriverStatus.INVALID
    assert driver.pending_outbound_events == driver.pending_outbound_bytes == 0
    assert core.retained_state is before


def test_read_only_field_rejects_activate_and_adjust_without_host_mutation():
    core = _active_field_core(read_only=True)
    driver = helpers._driver(core)
    assert _adjust(driver, core) is DriverStatus.INVALID
    assert driver.send_control_event(helpers.OWNER_ID, helpers.OWNER_GENERATION,
                                     FIELD_ID, model_revision=core.model_revision) is DriverStatus.INVALID
    assert driver.pending_outbound_events == 0
