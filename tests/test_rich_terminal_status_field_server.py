"""STF1 ingress through negotiated production state and immutable offers."""

from dataclasses import replace
import json
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
from rich_terminal.retained_scene import ObjectBounds, StatusFieldBody, StatusSeverity
from rich_terminal.retained_view import (
    StatusFieldDraw, project_composite_draw_plane, retained_draw_control_ids,
    retained_draw_key,
)
from rich_terminal.retained_wire import (
    ObjectWireDefinition, PresentDisposition, PresentRetainedMode,
    RegionWireDefinition, RetainedMessageType, decode_ret_caps,
    encode_object_definition, encode_region_definition, encode_ret_query,
)
from rich_terminal.server import RichTerminalCore, TerminalSessionError
from shared.session import OutputSnapshotRows, TerminalDisplayOffer
from shared_session import display_offer_from_wire, display_offer_to_wire


def _open_core(*, status_fields=True, controls=False):
    # The ordinary path deliberately has neither CONTROLS nor GLYPH_RUNS:
    # status strings must reserve and charge UTF-8 through their own feature.
    features = RetainedFeature.CORE
    if status_fields:
        features |= RetainedFeature.STATUS_FIELDS
    if controls or not status_fields:
        features |= RetainedFeature.CONTROLS
    core = RichTerminalCore(
        wire_helpers._config(), attachment_epoch=9,
        retained_policy=replace(wire_helpers._policy(), features=features),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    offered = core.feed_machine(encode_probe(nonce))
    offer = parse_negotiation(offered.outbound[0].payload)
    assert isinstance(offer, Offer)
    encoder = FrameEncoder(offer.session_id, max_payload=512)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=512)
    ready = encoder.encode(
        MessageType.CLIENT_READY,
        wire_helpers._READY.pack(0, 512, 0, 8192, 256, 0, 0x3F),
    )
    opened = core.feed_machine(
        encode_open(OpenRequest(nonce, offer.session_id, 512, 8192))
        + ready + wire_helpers._snapshot_frames(encoder)
    )
    frames = wire_helpers._consume(decoder, opened)
    initial = next(frame for frame in frames if frame.message_type == MessageType.TX_RESULT)
    assert wire_helpers._TX_RESULT.unpack(initial.payload) == (1, 0, 0, 1)
    core.settle_result_delivery(1)
    discovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    caps = next(frame for frame in wire_helpers._consume(decoder, discovered)
                if frame.message_type == RetainedMessageType.RET_CAPS)
    assert decode_ret_caps(caps.payload).features == features
    assert core.retained_enabled
    wire_helpers._open_owner(core, encoder, decoder)
    return core, encoder, decoder


def _field(value="✓", *, label="温", severity=StatusSeverity.SUCCESS,
           emphasized=True):
    return ObjectWireDefinition(
        7, 1, 1, 1, 0, ObjectBounds(0, 1, 2, 1), 0, True,
        StatusFieldBody(label, value, 1, severity, emphasized),
    )


def _region_operation():
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, 3)
    return RetainedMessageType.REGION_DEFINE, encode_region_definition(region)


def _present(core, encoder, decoder, transaction_id, mode, operations=(),
             *, disposition=PresentDisposition.COMMIT, expected_status=0):
    before_revision = core.model_revision
    result = core.feed_machine(wire_helpers._present_frames(
        encoder, transaction_id=transaction_id, base_revision=before_revision,
        retained_mode=mode, disposition=disposition, operations=operations,
    ))
    completion = next(frame for frame in wire_helpers._consume(decoder, result)
                      if frame.message_type == MessageType.TX_RESULT)
    assert wire_helpers._TX_RESULT.unpack(completion.payload) == (
        transaction_id, expected_status, 0,
        before_revision + (expected_status == 0),
    )
    core.settle_result_delivery(transaction_id)


def _visible_core(*, controls=False):
    core, encoder, decoder = _open_core(controls=controls)
    _present(
        core, encoder, decoder, 2, PresentRetainedMode.REPLACE_START,
        (_region_operation(),
         (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(_field()))),
    )
    assert core.retained_state.hidden is not None
    assert not core.retained_state.retained_visible
    _present(core, encoder, decoder, 3, PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


def _offer(offer_id, composite):
    scope, plane = project_composite_draw_plane(composite)
    return TerminalDisplayOffer(
        offer_id, scope, OutputSnapshotRows().snapshot(composite.cell), plane,
    )


def test_status_only_negotiation_charges_both_utf8_strings_and_projects_exact_offer():
    core, _, _ = _visible_core()
    committed = core.output_view
    assert isinstance(committed, CompositeTerminalView)
    assert committed.revision == 3
    owner = committed.retained.active.owners[7]
    assert owner.objects[1].body == _field().body
    assert owner.usage.utf8_bytes == len("温✓".encode("utf-8"))
    assert owner.usage.objects == 1
    cadence = DisplayCadenceScheduler(policy=core.retained_policy,
                                      monotonic_us=lambda: 0)
    cadence.replace_session(committed.cell.attachment_epoch,
                            committed.cell.session_id, committed)
    offered = cadence.service()
    assert offered is committed
    offer = _offer(1, offered)
    field = offer.retained.regions[0].draws[0]
    assert isinstance(field, StatusFieldDraw)
    assert (field.bounds, field.label, field.value, field.label_cols,
            field.severity, field.emphasized) == (
        ObjectBounds(0, 1, 2, 1), "温", "✓", 1, StatusSeverity.SUCCESS, True,
    )
    assert retained_draw_key(field) == ("object", 1)
    assert retained_draw_control_ids(field) == frozenset()
    assert display_offer_from_wire(json.loads(json.dumps(display_offer_to_wire(offer)))) == offer
    assert cadence.displayed_revision is None
    cadence.acknowledge(offered)
    assert cadence.displayed_revision == 3


def test_status_replacement_crosses_delta_transport_without_mutating_acknowledged_offer():
    core, encoder, decoder = _visible_core()
    original = core.output_view
    cadence = DisplayCadenceScheduler(policy=core.retained_policy,
                                      monotonic_us=lambda: 0)
    cadence.replace_session(original.cell.attachment_epoch,
                            original.cell.session_id, original)
    assert cadence.service() is original
    base = _offer(1, original)
    cadence.acknowledge(original)
    _present(core, encoder, decoder, 4, PresentRetainedMode.DELTA, (
        (RetainedMessageType.OBJECT_REPLACE,
         encode_object_definition(_field("!", severity=StatusSeverity.WARNING,
                                         emphasized=False))),
    ))
    updated = core.output_view
    cadence.submit(updated)
    assert cadence.service() is updated
    offered = _offer(2, updated)
    wire = display_offer_to_wire(offered, base=base)
    assert wire["base_offer_id"] == 1
    assert "draws" not in wire["retained"]["regions"][0]
    assert [(draw["kind"], draw["object_id"]) for draw in
            wire["retained"]["regions"][0]["changed"]] == [("status_field", 1)]
    assert display_offer_from_wire(json.loads(json.dumps(wire)), base) == offered
    assert base.retained.regions[0].draws[0].value == "✓"
    assert original.retained.active.owners[7].usage.utf8_bytes == 6
    assert updated.retained.active.owners[7].usage.utf8_bytes == 4
    assert offered.cell == base.cell
    assert cadence.displayed_revision == 3
    cadence.acknowledge(updated)
    assert cadence.displayed_revision == 4


@pytest.mark.parametrize("fault", [
    "version", "reserved", "flags", "severity", "split", "rows",
    "length", "utf8", "trailing",
])
def test_bad_status_body_rejects_whole_staged_transaction_and_valid_retry_succeeds(fault):
    core, encoder, decoder = _visible_core()
    before = core.retained_state
    before_output = core.output_view
    invalid = bytearray(encode_object_definition(_field("Invalid")))
    # Offsets are an independent STF1 oracle after the 64-byte OBJECT prefix.
    edits = {
        "version": ("<H", 68, 2), "reserved": ("<Q", 88, 1),
        "flags": ("<H", 70, 2), "severity": ("<I", 72, 5),
        "split": ("<I", 76, 3), "rows": ("<I", 60, 2),
        "length": ("<I", 80, 100),
    }
    if fault in edits:
        fmt, offset, value = edits[fault]
        struct.pack_into(fmt, invalid, offset, value)
    elif fault == "utf8":
        invalid[96] = 0xFF
    else:
        invalid += b"\0"
    _present(core, encoder, decoder, 4, PresentRetainedMode.DELTA, (
        (RetainedMessageType.OBJECT_REPLACE, encode_object_definition(_field("Staged"))),
        (RetainedMessageType.OBJECT_REPLACE, invalid),
    ), expected_status=2)
    assert core.retained_state is before
    assert core.output_view is before_output
    assert core.model_revision == 3
    assert core.active
    _present(core, encoder, decoder, 5, PresentRetainedMode.DELTA, (
        (RetainedMessageType.OBJECT_REPLACE, encode_object_definition(_field("Recovered"))),
    ))
    assert core.model_revision == 4
    assert core.retained_state.active.owners[7].objects[1].body.value == "Recovered"


def test_status_utf8_over_owner_reservation_rejects_atomically():
    core, encoder, decoder = _visible_core()
    before = core.retained_state
    before_output = core.output_view
    # OWNER_OPEN reserved 128 bytes. The three-byte label plus this value
    # exceeds that reservation while remaining within the negotiated frame.
    _present(core, encoder, decoder, 4, PresentRetainedMode.DELTA, (
        (RetainedMessageType.OBJECT_REPLACE,
         encode_object_definition(_field("x" * 126))),
    ), expected_status=2)
    assert core.retained_state is before
    assert core.output_view is before_output
    assert core.active
    _present(core, encoder, decoder, 5, PresentRetainedMode.DELTA, (
        (RetainedMessageType.OBJECT_REPLACE,
         encode_object_definition(_field("x" * 125))),
    ))
    assert core.retained_state.active.owners[7].usage.utf8_bytes == 128


def test_unadvertised_status_field_preserves_cell_fallback_and_accepts_next_present():
    core, encoder, decoder = _open_core(status_fields=False)
    fallback = core.view
    before = core.retained_state
    _present(core, encoder, decoder, 2, PresentRetainedMode.REPLACE_START, (
        _region_operation(),
        (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(_field())),
    ), expected_status=2)
    assert core.retained_state is before
    assert core.view.cells == fallback.cells
    assert core.model_revision == 1
    _present(core, encoder, decoder, 3, PresentRetainedMode.REPLACE_START,
             (_region_operation(),))
    _present(core, encoder, decoder, 4, PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    _, plane = project_composite_draw_plane(core.output_view)
    assert plane.retained_visible
    assert all(not region.draws for region in plane.regions)
    assert core.view.cells == fallback.cells
    assert core.active


def test_status_identity_has_no_control_authority_and_pointer_keeps_exact_cell_revision():
    core, _, decoder = _visible_core(controls=True)
    with pytest.raises(TerminalSessionError, match="not interactable"):
        core.send_control_event(7, 1, 1, model_revision=core.model_revision)
    raw = core.send_pointer(1, 1, buttons=1)
    assert raw is not None
    frames = decoder.feed(raw.payload)
    assert len(frames) == 1
    assert frames[0].message_type == MessageType.POINTER
    assert struct.unpack("<iiHHHHhhQ", frames[0].payload) == (
        1, 1, 1, 1, 0, 1, 0, 0, 3,
    )
    assert not core.retained_state.active.owners[7].controls
