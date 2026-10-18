"""A button the physical view shows activates through the acknowledged
frame, a press on a dialog's body goes nowhere while a wheel over a dialog
that is not a notice scrolls it, and a control beneath an open modal region
is refused by the terminal (RETAINED-1 sections 9, 9.1 and 9.3)."""

from dataclasses import replace
import json

import pytest

pygame = pytest.importorskip("pygame")

from tests import test_rich_terminal_field_server as field_helpers
from tests import test_rich_terminal_semantic_server as wire_helpers
from tests import test_rich_terminal_taskbar_server as taskbar_helpers
from tests import test_rich_terminal_taskbar_viewer_input as viewer_helpers
from tests import test_session_viewer as keyboard_helpers
from tests.test_rich_terminal_field_viewer_input import _FieldSession
from emulator.shared_session import SharedMachine
from rich_terminal.apt1 import (
    FrameEncoder, IncrementalFrameDecoder, MessageType, Offer, OpenRequest,
    encode_open, encode_probe, parse_negotiation,
)
from rich_terminal.pygame_view import ControlHitTarget, composite_draw_plane_result
from rich_terminal.retained_model import OwnerQuotas, RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_view import project_composite_draw_plane
from rich_terminal.retained_wire import (
    ControlEventKind, ControlWireDefinition, OwnerOpen, PresentDisposition,
    PresentRetainedMode, RegionWireDefinition, RetainedMessageType,
    encode_control_definition, encode_owner_open, encode_region_definition,
    encode_ret_query,
)
from rich_terminal.semantic_dialogs import ButtonContent, ButtonFlag, DialogContent, DialogFlag
from rich_terminal.semantic_fields import FieldRect
from rich_terminal.server import RichTerminalCore, TerminalSessionError
from session_viewer import _GuestKeyboardForwarder, _PointerRouter, _RetainedDisplayState
from shared_session import SessionServer

LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.FIELDS \
    | RetainedFeature.DIALOGS


def _open_core():
    core = RichTerminalCore(
        wire_helpers._config(), attachment_epoch=9,
        retained_policy=replace(wire_helpers._policy(), features=FEATURES),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    offer = parse_negotiation(core.feed_machine(encode_probe(nonce)).outbound[0].payload)
    assert isinstance(offer, Offer)
    encoder = FrameEncoder(offer.session_id, max_payload=512)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=512)
    ready = encoder.encode(MessageType.CLIENT_READY,
                           wire_helpers._READY.pack(0, 512, 0, 8192, 256, 0, 0x3F))
    wire_helpers._consume(decoder, core.feed_machine(
        encode_open(OpenRequest(nonce, offer.session_id, 512, 8192))
        + ready + wire_helpers._snapshot_frames(encoder)))
    core.settle_result_delivery(1)
    wire_helpers._consume(decoder, core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())))
    opened = core.feed_machine(encoder.encode(
        RetainedMessageType.OWNER_OPEN,
        encode_owner_open(OwnerOpen(7, 1, OwnerQuotas(2, 0, 8, 0, 0, 256, 0)))))
    wire_helpers._consume(decoder, opened)
    marker = next(outbound.lifecycle_result for outbound in opened.outbound
                  if outbound.lifecycle_result is not None)
    core.settle_lifecycle_result_delivery(marker)
    return core, encoder, decoder


def _visible_core(*, modal: bool, flags=DialogFlag.NOTICE):
    core, encoder, decoder = _open_core()
    base = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, 3)
    # The bottom row, above region 1; MODAL when asked.
    overlay = RegionWireDefinition(7, 1, 2, 0, 1, 2, 1, 0, 0, 0, 0, 5,
                                   0x5 if modal else 0x1)
    definitions = (
        # A text field beneath the overlay's row, and a button on the top row.
        field_helpers._definition(field_helpers._content(
            field_helpers.FieldKind.TEXT)),
        ControlWireDefinition(7, 1, 2, ControlKind.BUTTON, LIVE, 0, 1, 0, 0,
                              ObjectBounds(1, 0, 1, 1), "OK", "",
                              ButtonContent(ButtonFlag.DEFAULT)),
        ControlWireDefinition(7, 1, 3, ControlKind.DIALOG, ControlState.VISIBLE, 0, 2, 0, 0,
                              ObjectBounds(0, 0, 2, 1), "", "",
                              DialogContent(1, flags, 2, FieldRect(0, 0, 0, 0),
                                            FieldRect(0, 0, 2, 1), "Saved")),
    )
    operations = (
        (RetainedMessageType.REGION_DEFINE, encode_region_definition(base)),
        (RetainedMessageType.REGION_DEFINE, encode_region_definition(overlay)),
        *((RetainedMessageType.CONTROL_DEFINE, encode_control_definition(item))
          for item in definitions),
    )
    taskbar_helpers._present(core, encoder, decoder, 2, operations,
                             mode=PresentRetainedMode.REPLACE_START)
    taskbar_helpers._present(core, encoder, decoder, 3,
                             mode=PresentRetainedMode.REPLACE_CONTINUE,
                             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _environment(font, *, modal: bool, flags=DialogFlag.NOTICE):
    core, _, decoder = _visible_core(modal=modal, flags=flags)
    session = _FieldSession(core, decoder)
    machine = SharedMachine(session)
    machine._reset_generation = viewer_helpers.GENERATION
    server = SessionServer(machine, "/tmp/unused-dialog-input.sock")
    server._display_holder = viewer_helpers.CONNECTION
    client = viewer_helpers._JsonClient(server)
    keyboard = _GuestKeyboardForwarder(
        keyboard_helpers._FakePygame(), client,
        generation=viewer_helpers.GENERATION, display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=40, cell_height=20)
    offer, result = viewer_helpers._offer_and_hits(core, font)
    viewer_helpers._ack(session, server, keyboard, state, offer, result)
    return core, session, client, pointer, result


@pytest.mark.parametrize("modal", [False, True])
def test_a_shown_button_activates_and_a_notice_body_takes_nothing(font, modal):
    core, session, client, pointer, result = _environment(font, modal=modal)
    button = next(target for target in result.hit_targets
                  if isinstance(target, ControlHitTarget) and target.kind is ControlKind.BUTTON)
    assert button.identity.control_id == 2
    before = core.retained_state
    assert pointer.button_down(1, (60, 10), (80, 40))
    assert pointer.button_up(1, (60, 10), (80, 40))
    assert [(event.event_kind, event.control_id) for event in session.events] == [
        (ControlEventKind.ACTIVATE, 2)]
    # The notice covers the bottom row: a press there goes nowhere.
    assert not pointer.button_down(1, (20, 30), (80, 40))
    assert not pointer.button_up(1, (20, 30), (80, 40))
    assert len(session.events) == 1
    assert core.retained_state is before


def test_a_wheel_over_a_dialog_scrolls_it_and_a_press_goes_nowhere(font):
    core, session, client, pointer, result = _environment(
        font, modal=True, flags=DialogFlag(0))
    before = core.retained_state
    assert not pointer.button_down(1, (20, 30), (80, 40))
    assert not pointer.button_up(1, (20, 30), (80, 40))
    assert session.events == []
    # A host wheel step up is an APT detent down.
    assert pointer.wheel(0, -1, (20, 30), (80, 40))
    assert [(event.event_kind, event.control_id, event.wheel_y)
            for event in session.events] == [(ControlEventKind.SCROLL, 3, 1)]
    assert core.retained_state is before


def test_a_wheel_over_a_notice_goes_nowhere(font):
    core, session, client, pointer, result = _environment(font, modal=False)
    assert not pointer.wheel(0, -1, (20, 30), (80, 40))
    assert session.events == []


def test_the_terminal_scrolls_only_a_dialog_that_is_not_a_notice():
    core, _, _ = _visible_core(modal=True, flags=DialogFlag(0))
    assert core.send_control_event(7, 1, 3, model_revision=core.model_revision,
                                   event_kind=ControlEventKind.SCROLL,
                                   wheel_y=1) is not None
    core, _, _ = _visible_core(modal=True)
    with pytest.raises(TerminalSessionError, match="a notice takes no input"):
        core.send_control_event(7, 1, 3, model_revision=core.model_revision,
                                event_kind=ControlEventKind.SCROLL, wheel_y=1)
    # A button scrolls nothing.
    with pytest.raises(TerminalSessionError, match="RET_CONTROL_COLLECTIONS"):
        core.send_control_event(7, 1, 2, model_revision=core.model_revision,
                                event_kind=ControlEventKind.SCROLL, wheel_y=1)


def test_the_terminal_refuses_a_control_beneath_an_open_modal_region():
    core, _, _ = _visible_core(modal=True)
    with pytest.raises(TerminalSessionError, match="beneath an open modal region"):
        core.send_control_event(7, 1, 1, model_revision=core.model_revision,
                                event_kind=ControlEventKind.PLACE, content_revision=7,
                                item_key=0, scalar_offset=0)
    plane = project_composite_draw_plane(core.output_view)[1]
    assert plane.regions[0].inactive_controls == frozenset({1})


def test_without_the_modal_flag_the_covered_field_still_takes_positions():
    core, _, _ = _visible_core(modal=False)
    outbound = core.send_control_event(
        7, 1, 1, model_revision=core.model_revision, event_kind=ControlEventKind.PLACE,
        content_revision=7, item_key=0, scalar_offset=0)
    assert outbound is not None
