"""The RETAINED-1 additions for pictures, from wire to reference renderer.

APT-1-RETAINED-1 section 8.1 lets a client ask how many pixels a cell
rectangle covers, section 11.3 gives IMAGE a BACKDROP flag, and OWNER_RESIZE
may give resource space back down to the resources in use.  These tests cover
the codec, the owner ledger, the terminal core, the projected view, the viewer
wire, and the reference renderer.  The guest module's side is in the Forth
tests.
"""

from __future__ import annotations

import hashlib
import struct
from dataclasses import replace

import pytest

from rich_terminal.apt1 import UINT32_MAX, MessageType
from rich_terminal.appearance import REFERENCE_APPEARANCE
from rich_terminal.retained_model import (
    OwnerLedgerError,
    OwnerLedgerErrorCode,
    OwnerResizeDisposition,
)
from rich_terminal.retained_scene import ImageBody, ImageFit, ObjectBounds
from rich_terminal.retained_view import project_composite_draw_plane
from rich_terminal.retained_wire import (
    IMAGE_FLAG_BACKDROP,
    ObjectWireDefinition,
    PixelsAnswer,
    PixelsQuery,
    ResourceBegin,
    ResourceChunk,
    ResourceCommit,
    ResourceDrop,
    RetainedMessageType,
    RetainedWireError,
    RetainedWireErrorCode,
    RetStatus,
    decode_object_definition,
    decode_pixels,
    decode_pixels_query,
    decode_ret_result,
    encode_object_definition,
    encode_owner_open,
    encode_pixels,
    encode_pixels_query,
    encode_resource_begin,
    encode_resource_chunk,
    encode_resource_commit,
    encode_resource_drop,
)
from rich_terminal.server import TerminalSessionError
from shared_session import _retained_draw_from_wire, _retained_draw_to_wire
from tests.test_rich_terminal_pygame_view import (
    _PixelFont,
    _image_draw,
    _image_manifest,
    _image_plane,
)
from tests.test_rich_terminal_retained_model import _identity, _ledger, _quotas
from tests.test_rich_terminal_retained_view import (
    _composite,
    _object,
    _owner,
    _owner_scene,
    _region,
    _resource,
)
from tests.test_rich_terminal_server import (
    CREDIT,
    _image_policy,
    _open_retained_core,
    _owner_lifecycle,
    _owner_open,
    _settle_lifecycle,
)

# _open_retained_core grants the terminal this much client receive credit.
_INITIAL_CLIENT_GRANT = 512


def _frames(core, decoder, data):
    sent = core.feed_machine(data)
    return sent, [
        frame for outbound in sent.outbound for frame in decoder.feed(outbound.payload)
    ]


def _ask(core, encoder, decoder, query: PixelsQuery):
    return _frames(
        core,
        decoder,
        encoder.encode(RetainedMessageType.PIXELS_QUERY, encode_pixels_query(query)),
    )


def _exhaust_client_grant(core, decoder) -> None:
    """Spend the client's whole grant on key events, as typing would."""

    for _ in range(64):
        key = core.send_key(ord("x"))
        if key is None:
            return
        decoder.feed(key.payload)
    raise AssertionError("the client grant never ran out")


# --- Wire ---------------------------------------------------------------


def test_pixels_query_and_answer_have_exact_little_endian_layouts():
    query = PixelsQuery(7, -3, 2, 40, 12)
    raw = encode_pixels_query(query)
    assert raw == struct.pack("<QiiII", 7, -3, 2, 40, 12)
    assert decode_pixels_query(raw) == query

    answer = PixelsAnswer(7, 5, -3, 2, 40, 12, 440, 252)
    raw = encode_pixels(answer)
    assert raw == struct.pack("<QQiiIIII", 7, 5, -3, 2, 40, 12, 440, 252)
    assert decode_pixels(raw) == answer
    assert answer.answers(query) and answer.known
    assert not replace(answer, query_id=8).answers(query)
    assert not replace(answer, cols=41).answers(query)
    assert not PixelsAnswer(7, 5, -3, 2, 40, 12, 0, 0).known


def test_pixels_messages_reject_zero_ids_extents_and_half_known_sizes():
    for raw in (
        struct.pack("<QiiII", 0, 0, 0, 1, 1),
        struct.pack("<QiiII", 1, 0, 0, 0, 1),
        struct.pack("<QiiII", 1, 0, 0, 1, 0),
    ):
        with pytest.raises(RetainedWireError) as caught:
            decode_pixels_query(raw)
        assert caught.value.code is RetainedWireErrorCode.SCALAR
    for width, height in ((9, 0), (0, 9)):
        with pytest.raises(RetainedWireError) as caught:
            decode_pixels(struct.pack("<QQiiIIII", 1, 0, 0, 0, 1, 1, width, height))
        assert caught.value.code is RetainedWireErrorCode.SCALAR
    with pytest.raises(RetainedWireError) as short:
        decode_pixels_query(bytes(23))
    assert short.value.code is RetainedWireErrorCode.PAYLOAD


def test_image_backdrop_is_bit_zero_of_the_image_flags_byte():
    image = ObjectWireDefinition(
        1,
        1,
        1,
        1,
        0,
        ObjectBounds(0, 0, 4, 2),
        0,
        True,
        ImageBody(9, ImageFit.CONTAIN, 255, backdrop=True),
    )
    payload = encode_object_definition(image)
    assert payload[64:] == struct.pack("<QIBB2x", 9, 1, 255, IMAGE_FLAG_BACKDROP)
    assert decode_object_definition(payload) == image
    plain = replace(image, body=ImageBody(9, ImageFit.CONTAIN, 255))
    assert encode_object_definition(plain)[77] == 0
    with pytest.raises(TypeError):
        ImageBody(9, ImageFit.CONTAIN, 255, backdrop=1)


# --- Owner ledger -------------------------------------------------------


def test_owner_resize_gives_resource_space_back_only_down_to_usage():
    ledger = _ledger()
    owner = _identity(1)
    ledger.open(owner, _quotas(resources=3, resource_bytes=2048))
    before = ledger.state

    smaller = _quotas(resources=1, resource_bytes=512)
    assert (
        ledger.resize(owner, smaller, resources_in_use=(1, 512))
        is OwnerResizeDisposition.RESIZED
    )
    after = ledger.state
    assert ledger.require_live(owner).quotas == smaller
    assert after.reservations.resources == before.reservations.resources - 2
    assert after.reservations.resource_bytes == before.reservations.resource_bytes - 1536

    refusals = (
        # Below the resources in use.
        (_quotas(resources=0, resource_bytes=512), (1, 512)),
        (_quotas(resources=1, resource_bytes=511), (1, 512)),
        # Scene quotas bound each target, so they never shrink.
        (_quotas(resources=1, resource_bytes=512, objects=3), (1, 512)),
        # Without the resources in use, resource quotas only grow.
        (_quotas(resources=0, resource_bytes=0), None),
    )
    for quotas, in_use in refusals:
        with pytest.raises(OwnerLedgerError) as caught:
            ledger.resize(owner, quotas, resources_in_use=in_use)
        assert caught.value.code is OwnerLedgerErrorCode.INVALID
        assert ledger.state is after


# --- Terminal core: the pixel question ----------------------------------


def test_pixels_query_answers_from_the_selected_sink_cell_size():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    assert core.sink_cell_pixels is None

    query = PixelsQuery(3, -2, 1, 40, 12)
    _, frames = _ask(core, encoder, decoder, query)
    assert [frame.message_type for frame in frames] == [
        RetainedMessageType.PIXELS,
        MessageType.CREDIT,
    ]
    answer = decode_pixels(frames[0].payload)
    assert answer == PixelsAnswer(3, core.geometry_generation, -2, 1, 40, 12, 0, 0)

    core.select_sink_cell_pixels((11, 21))
    _, frames = _ask(core, encoder, decoder, PixelsQuery(4, 0, 0, 40, 12))
    assert decode_pixels(frames[0].payload).known
    assert (
        decode_pixels(frames[0].payload).pixel_width,
        decode_pixels(frames[0].payload).pixel_height,
    ) == (440, 252)

    # A count past u32 is answered as the u32 maximum.
    _, frames = _ask(core, encoder, decoder, PixelsQuery(5, 0, 0, UINT32_MAX, 1))
    assert decode_pixels(frames[0].payload).pixel_width == UINT32_MAX

    # The client returns credit for what it read, as a real client does.
    _frames(
        core,
        decoder,
        encoder.encode(MessageType.CREDIT, CREDIT.pack(_INITIAL_CLIENT_GRANT + 4096)),
    )
    core.select_sink_cell_pixels(None)
    _, frames = _ask(core, encoder, decoder, PixelsQuery(6, 0, 0, 2, 2))
    assert not decode_pixels(frames[0].payload).known

    for bad in ((0, 21), (11, 0), (11,), [11, 21]):
        with pytest.raises((TypeError, ValueError)):
            core.select_sink_cell_pixels(bad)


def test_pixels_query_is_skipped_without_rgba_image():
    core, encoder, decoder = _open_retained_core()
    core.select_sink_cell_pixels((11, 21))
    _, frames = _ask(core, encoder, decoder, PixelsQuery(1, 0, 0, 2, 2))
    assert [frame.message_type for frame in frames] == [MessageType.CREDIT]


@pytest.mark.parametrize(
    "payload",
    (
        struct.pack("<QiiII", 0, 0, 0, 1, 1),
        struct.pack("<QiiII", 1, 0, 0, 0, 1),
        bytes(23),
    ),
)
def test_a_malformed_pixels_query_fails_the_session(payload):
    core, encoder, _decoder = _open_retained_core(retained_policy=_image_policy())
    with pytest.raises(TerminalSessionError, match="invalid PIXELS_QUERY"):
        core.feed_machine(encoder.encode(RetainedMessageType.PIXELS_QUERY, payload))


def test_pixels_query_waits_for_an_outstanding_lifecycle_result():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    _frames(
        core,
        decoder,
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(_owner_open())),
    )
    with pytest.raises(TerminalSessionError, match="outstanding lifecycle completion"):
        _ask(core, encoder, decoder, PixelsQuery(1, 0, 0, 2, 2))


def test_an_answer_without_room_is_owed_and_holds_other_data_until_credit():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    core.select_sink_cell_pixels((8, 16))
    _exhaust_client_grant(core, decoder)

    _, frames = _ask(core, encoder, decoder, PixelsQuery(9, 0, 0, 3, 2))
    assert [frame.message_type for frame in frames] == [MessageType.CREDIT]
    # Nothing else ordinary goes out ahead of the owed answer.
    assert core.send_key(ord("y")) is None
    assert core.send_focus(True) is None

    _, frames = _frames(
        core,
        decoder,
        encoder.encode(MessageType.CREDIT, CREDIT.pack(_INITIAL_CLIENT_GRANT + 4096)),
    )
    assert [frame.message_type for frame in frames] == [RetainedMessageType.PIXELS]
    assert decode_pixels(frames[0].payload) == PixelsAnswer(
        9, core.geometry_generation, 0, 0, 3, 2, 24, 32
    )
    key = core.send_key(ord("y"))
    assert key is not None
    decoder.feed(key.payload)


def test_a_second_question_while_one_is_owed_fails_the_session():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    _exhaust_client_grant(core, decoder)
    _ask(core, encoder, decoder, PixelsQuery(1, 0, 0, 2, 2))
    with pytest.raises(TerminalSessionError, match="still owed"):
        _ask(core, encoder, decoder, PixelsQuery(2, 0, 0, 2, 2))


def test_a_soft_reset_cancels_an_owed_answer():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    _exhaust_client_grant(core, decoder)
    _ask(core, encoder, decoder, PixelsQuery(1, 0, 0, 2, 2))
    request = core.request_soft_reset()
    decoder.feed(request.payload)

    _, frames = _frames(
        core,
        decoder,
        encoder.encode(MessageType.CREDIT, CREDIT.pack(_INITIAL_CLIENT_GRANT + 4096)),
    )
    assert frames == []
    # A question the terminal consumes after asking for the reset is skipped.
    _, frames = _ask(core, encoder, decoder, PixelsQuery(2, 0, 0, 2, 2))
    assert [frame.message_type for frame in frames] == [MessageType.CREDIT]


# --- Terminal core: giving resource space back --------------------------


def _upload_resource(core, encoder, decoder, resource_id, pixels, width, height):
    digest = hashlib.sha3_256(pixels).digest()
    requests = (
        (
            RetainedMessageType.RESOURCE_BEGIN,
            encode_resource_begin(
                ResourceBegin(7, 1, resource_id, 1, width, height, 0, len(pixels), digest)
            ),
        ),
        (
            RetainedMessageType.RESOURCE_CHUNK,
            encode_resource_chunk(ResourceChunk(7, 1, resource_id, 0, pixels)),
        ),
        (
            RetainedMessageType.RESOURCE_COMMIT,
            encode_resource_commit(ResourceCommit(7, 1, resource_id)),
        ),
    )
    for request, payload in requests:
        sent, frames = _frames(core, decoder, encoder.encode(request, payload))
        if frames[0].message_type == RetainedMessageType.RET_RESULT:
            assert decode_ret_result(frames[0].payload).status is RetStatus.OK
        _settle_lifecycle(core, sent)


def test_owner_resize_gives_resource_space_back_down_to_committed_usage():
    core, encoder, decoder = _open_retained_core(retained_policy=_image_policy())
    open_, resize = RetainedMessageType.OWNER_OPEN, RetainedMessageType.OWNER_RESIZE
    assert _owner_lifecycle(
        core, encoder, decoder, open_, _owner_open(resources=3, objects=1, resource_bytes=48)
    ) is RetStatus.OK
    _upload_resource(core, encoder, decoder, 1, bytes(range(16)), 2, 2)

    for request in (
        _owner_open(resources=0, objects=1, resource_bytes=16),
        _owner_open(resources=1, objects=1, resource_bytes=15),
        _owner_open(resources=1, objects=0, resource_bytes=16),
    ):
        assert _owner_lifecycle(core, encoder, decoder, resize, request) is RetStatus.INVALID
    assert core.owner_state.records[7].quotas.resources == 3

    assert _owner_lifecycle(
        core, encoder, decoder, resize, _owner_open(resources=1, objects=1, resource_bytes=16)
    ) is RetStatus.OK
    state = core.owner_state
    assert (state.records[7].quotas.resources, state.reservations.resource_bytes) == (1, 16)

    sent, frames = _frames(
        core,
        decoder,
        encoder.encode(
            RetainedMessageType.RESOURCE_DROP,
            encode_resource_drop(ResourceDrop(7, 1, 1)),
        ),
    )
    assert decode_ret_result(frames[0].payload).status is RetStatus.OK
    _settle_lifecycle(core, sent)
    assert _owner_lifecycle(
        core, encoder, decoder, resize, _owner_open(resources=0, objects=1, resource_bytes=0)
    ) is RetStatus.OK
    assert core.owner_state.reservations.resources == 0
    assert core.capacity_denials == 0


# --- View, viewer wire and reference renderer ---------------------------


def test_projection_carries_the_backdrop_to_the_image_draw():
    owner = _owner(5, 7)
    image = _object(owner, 1, 1, ImageBody(4, ImageFit.CONTAIN, 255, backdrop=True))
    _, plane = project_composite_draw_plane(
        _composite(
            [_owner_scene(owner, [_region(owner, 1)], [image])],
            resources=(_resource(owner, 4, bytes((1, 2, 3, 255))),),
        )
    )
    (draw,) = plane.regions[0].draws
    assert draw.backdrop


def test_viewer_wire_round_trips_the_backdrop():
    draw = replace(_image_draw(3, ImageFit.CONTAIN), backdrop=True)
    wire = _retained_draw_to_wire(draw)
    assert wire["backdrop"] is True
    assert _retained_draw_from_wire(wire, "draw") == draw
    del wire["backdrop"]
    with pytest.raises(ValueError, match="not exact"):
        _retained_draw_from_wire(wire, "draw")


def test_backdrop_paints_the_renderer_background_under_bars_and_transparency():
    pygame = pytest.importorskip("pygame")
    from rich_terminal.pygame_view import composite_draw_plane

    source = pygame.Surface((2, 1), flags=pygame.SRCALPHA)
    source.set_at((0, 0), (220, 30, 40, 255))
    source.set_at((1, 0), (0, 0, 0, 0))
    manifest = _image_manifest(1, 2, 1)
    surface = REFERENCE_APPEARANCE.surface

    for backdrop, bar, clear in (
        (False, (2, 4, 6), (2, 4, 6)),
        (True, surface, surface),
    ):
        destination = pygame.Surface((6, 6))
        destination.fill((2, 4, 6))
        draw = replace(_image_draw(1, ImageFit.CONTAIN), backdrop=backdrop)
        composite_draw_plane(
            pygame,
            destination,
            _image_plane((draw,), (manifest,)),
            _PixelFont(pygame),
            6,
            6,
            resource_surfaces={manifest.key: source},
        )
        # Contain centres a 6 by 3 target: rows 0 and 5 are letterbox bars.
        assert tuple(destination.get_at((3, 0)))[:3] == bar
        assert tuple(destination.get_at((3, 5)))[:3] == bar
        assert tuple(destination.get_at((1, 2)))[:3] == (220, 30, 40)
        assert tuple(destination.get_at((4, 2)))[:3] == clear


def test_opaque_cell_coverage_counts_backdrop_images_only():
    pygame = pytest.importorskip("pygame")
    from rich_terminal.pygame_view import opaque_cell_coverage

    manifest = _image_manifest(1, 1, 1)
    plain = _image_draw(1, ImageFit.CONTAIN, bounds=ObjectBounds(0, 0, 2, 1))
    for backdrop, expected in ((False, b"\x00\x00"), (True, b"\x01\x01")):
        plane = _image_plane(
            (replace(plain, backdrop=backdrop),), (manifest,), cell_cols=2
        )
        assert opaque_cell_coverage(pygame, plane, 2, 1, 4, 8) == bytearray(expected)
