"""PNE1 binary ingress through real negotiation, commit, projection and offers."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_server as wire_helpers
from rich_terminal.apt1 import (
    FrameEncoder, IncrementalFrameDecoder, MessageType, Offer, OpenRequest,
    encode_open, encode_probe, parse_negotiation,
)
from rich_terminal.display_cadence import DisplayCadenceScheduler
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ObjectBounds, PaneBody
from rich_terminal.retained_view import (
    PaneDraw, project_composite_draw_plane, retained_draw_control_ids,
    retained_draw_key,
)
from rich_terminal.retained_wire import (
    ObjectWireDefinition, PresentDisposition, PresentRetainedMode,
    RegionWireDefinition, RetainedMessageType,
    decode_ret_caps, encode_object_definition, encode_region_definition,
    encode_ret_query,
)
from rich_terminal.server import RichTerminalCore, TerminalSessionError
from shared.session import OutputSnapshotRows, TerminalDisplayOffer
from shared_session import display_offer_from_wire, display_offer_to_wire


def _open_core(*, panes=True, controls=False):
    # Keep PANES genuinely independent of CONTROL and GLYPH_RUN admission.
    features = RetainedFeature.CORE
    if panes:
        features |= RetainedFeature.PANES
    if controls or not panes:
        features |= RetainedFeature.CONTROLS
    policy = replace(wire_helpers._policy(), features=features)
    core = RichTerminalCore(
        wire_helpers._config(), attachment_epoch=9, retained_policy=policy,
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    offer_result = core.feed_machine(encode_probe(nonce))
    offer = parse_negotiation(offer_result.outbound[0].payload)
    assert isinstance(offer, Offer)
    encoder = FrameEncoder(offer.session_id, max_payload=512)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=512)
    request = OpenRequest(nonce, offer.session_id, 512, 8192)
    ready = encoder.encode(
        MessageType.CLIENT_READY,
        wire_helpers._READY.pack(0, 512, 0, 8192, 256, 0, 0x3F),
    )
    opened = core.feed_machine(
        encode_open(request) + ready + wire_helpers._snapshot_frames(encoder)
    )
    frames = wire_helpers._consume(decoder, opened)
    initial = next(frame for frame in frames if frame.message_type == MessageType.TX_RESULT)
    assert wire_helpers._TX_RESULT.unpack(initial.payload) == (1, 0, 0, 1)
    core.settle_result_delivery(1)
    discovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    frames = wire_helpers._consume(decoder, discovered)
    caps = next(frame for frame in frames
                if frame.message_type == RetainedMessageType.RET_CAPS)
    assert decode_ret_caps(caps.payload).features == features
    assert core.retained_enabled
    wire_helpers._open_owner(core, encoder, decoder)
    return core, encoder, decoder


def _pane(title="Pad", *, content_bounds=None):
    return ObjectWireDefinition(
        7, 1, 1, 1, 0, ObjectBounds(0, 0, 2, 2), 0, True,
        PaneBody(2, content_bounds or ObjectBounds(0, 1, 2, 1), title, True),
    )


def _regions():
    return (
        RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, 3),
        RegionWireDefinition(7, 1, 2, 0, 1, 2, 1, 0, 1, 2, 1, 1, 3),
    )


def _region_operations():
    return tuple((RetainedMessageType.REGION_DEFINE, encode_region_definition(region))
                 for region in _regions())


def _present(core, encoder, decoder, transaction_id, mode, operations=(),
             *, disposition=PresentDisposition.COMMIT, expected_status=0):
    before_revision = core.model_revision
    result = core.feed_machine(wire_helpers._present_frames(
        encoder, transaction_id=transaction_id, base_revision=before_revision,
        retained_mode=mode, disposition=disposition, operations=operations,
    ))
    frames = wire_helpers._consume(decoder, result)
    completion = next(frame for frame in frames
                      if frame.message_type == MessageType.TX_RESULT)
    expected_revision = before_revision + (expected_status == 0)
    assert wire_helpers._TX_RESULT.unpack(completion.payload) == (
        transaction_id, expected_status, 0, expected_revision,
    )
    core.settle_result_delivery(transaction_id)
    return result


def _visible_core(*, controls=False):
    core, encoder, decoder = _open_core(controls=controls)
    _present(
        core, encoder, decoder, 2, PresentRetainedMode.REPLACE_START,
        (*_region_operations(),
         (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(_pane()))),
    )
    assert core.retained_state.hidden is not None
    assert not core.retained_state.retained_visible
    _present(core, encoder, decoder, 3, PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


def test_negotiated_pane_binary_commit_reaches_exact_immutable_display_offer():
    core, _, _ = _visible_core()
    committed = core.output_view
    assert isinstance(committed, CompositeTerminalView)
    assert committed.revision == 3
    assert committed.retained.active.owners[7].objects[1].body == _pane().body
    assert committed.retained.active.owners[7].usage.utf8_bytes == 3

    cadence = DisplayCadenceScheduler(policy=core.retained_policy,
                                      monotonic_us=lambda: 0)
    cadence.replace_session(committed.cell.attachment_epoch,
                            committed.cell.session_id, committed)
    offered = cadence.service()
    assert offered is committed
    scope, plane = project_composite_draw_plane(offered)
    offer = TerminalDisplayOffer(
        1, scope, OutputSnapshotRows().snapshot(offered.cell), plane,
    )
    assert cadence.displayed_revision is None
    pane = offer.retained.regions[0].draws[0]
    assert isinstance(pane, PaneDraw)
    assert (pane.object_id, pane.content_region_id, pane.title, pane.focused) == (
        1, 2, "Pad", True,
    )
    assert pane.content_bounds == ObjectBounds(0, 1, 2, 1)
    assert retained_draw_key(pane) == ("object", 1)
    assert retained_draw_control_ids(pane) == frozenset()
    assert offer.cell.cols == offer.cell.rows == 2
    assert display_offer_from_wire(display_offer_to_wire(offer)) == offer
    cadence.acknowledge(offered)
    assert cadence.displayed_revision == 3


@pytest.mark.parametrize("fault", ["version", "trailing", "clip"])
def test_invalid_pane_delta_rejects_atomically_and_retained_only_retry_succeeds(fault):
    core, encoder, decoder = _visible_core()
    before = core.retained_state
    before_output = core.output_view
    invalid = bytearray(encode_object_definition(_pane("Invalid")))
    if fault == "version":
        struct.pack_into("<H", invalid, 68, 2)
    elif fault == "trailing":
        invalid += b"\0"
    else:
        # Individually valid body; the committed content region cannot fit it.
        invalid = encode_object_definition(_pane("Invalid", content_bounds=ObjectBounds(0, 0, 1, 1)))
    _present(
        core, encoder, decoder, 4, PresentRetainedMode.DELTA,
        ((RetainedMessageType.OBJECT_REPLACE, encode_object_definition(_pane("Staged"))),
         (RetainedMessageType.OBJECT_REPLACE, invalid)),
        expected_status=2,
    )
    assert core.retained_state is before
    assert core.output_view is before_output
    assert core.model_revision == 3
    assert core.active
    _present(core, encoder, decoder, 5, PresentRetainedMode.DELTA,
             ((RetainedMessageType.OBJECT_REPLACE,
               encode_object_definition(_pane("Recovered"))),))
    assert core.model_revision == 4
    assert core.retained_state.active.owners[7].objects[1].body.title == "Recovered"


def test_old_profile_rejects_unadvertised_pane_and_preserves_complete_cell_fallback():
    core, encoder, decoder = _open_core(panes=False)
    fallback = core.view
    before = core.retained_state
    _present(
        core, encoder, decoder, 2, PresentRetainedMode.REPLACE_START,
        (*_region_operations(),
         (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(_pane()))),
        expected_status=2,
    )
    assert core.retained_state is before
    assert core.view.cells == fallback.cells
    assert core.model_revision == 1

    # A producer obeying the old capability set emits its existing regions and
    # complete CELL plane.  It can continue after the retained-only rejection.
    _present(core, encoder, decoder, 3, PresentRetainedMode.REPLACE_START,
             _region_operations())
    _present(core, encoder, decoder, 4, PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    _, plane = project_composite_draw_plane(core.output_view)
    assert plane.retained_visible
    assert all(not region.draws for region in plane.regions)
    assert core.view.cells == fallback.cells
    assert core.active


def test_pane_identity_creates_no_control_target_and_raw_pointer_keeps_current_revision():
    core, _, decoder = _visible_core(controls=True)
    with pytest.raises(TerminalSessionError, match="not interactable"):
        core.send_control_event(7, 1, 1, model_revision=core.model_revision)
    raw = core.send_pointer(0, 0, buttons=1)
    assert raw is not None
    frames = decoder.feed(raw.payload)
    assert len(frames) == 1
    assert frames[0].message_type == MessageType.POINTER
    x, y, buttons, changed, modifiers, kind, wheel_x, wheel_y, revision = struct.unpack(
        "<iiHHHHhhQ", frames[0].payload,
    )
    assert (x, y, buttons, changed, modifiers, kind, wheel_x, wheel_y, revision) == (
        0, 0, 1, 1, 0, 1, 0, 0, 3,
    )
    assert not core.retained_state.active.owners[7].controls
