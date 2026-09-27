"""Runtime oracle for the guest module's CONTROL_EVENT parser.

The complete production ``rich-terminal.f`` runs on the hosted simulator.  A
caller-owned session is placed directly in ACTIVE state with a chosen
RETAINED-1 feature set, one CONTROL_EVENT payload from the Python encoder is
dispatched through the module's own validator, and the typed PT accessors read
it back.  This pins the byte-level agreement between the terminal encoder and
the guest decoder for every event kind without a full retained session.
"""

from __future__ import annotations

import hashlib
import re

import pytest

from rich_terminal.retained_wire import (
    ControlEvent,
    ControlEventKind,
    encode_control_event,
)
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS,
    SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source,
)


CONTROLS = 0x100
CONTROL_COLLECTIONS = 0x200
REVISION = 17
REPORT_BEGIN = 30
REPORT_END = 31

HARNESS_SOURCE = b"""
CREATE CEI-RX _PT-CONTROL-RESERVE _PT-HDR + 32 + ALLOT
CREATE CEI-TX _PT-OPEN-BYTES ALLOT
CREATE CEI-EVENT PT-EVENT-SIZE ALLOT
CREATE CEI-S-STORAGE PT-SESSION-SIZE 7 + ALLOT
CREATE CEI-POLL PT-EVENT-SIZE ALLOT
CREATE CEI-PAYLOAD 64 ALLOT
: CEI-S  CEI-S-STORAGE 7 + -8 AND ;
VARIABLE CEI-FEATURES
VARIABLE CEI-INIT-S
VARIABLE CEI-DISPATCH-S
VARIABLE CEI-POLL-S
VARIABLE CEI-POLL-HAS
: CEI-BOOT  ( features -- )
  CEI-FEATURES !
  CEI-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
  CEI-TX _PT-OPEN-BYTES CEI-EVENT PT-EVENT-SIZE CEI-S
  PT-INIT CEI-INIT-S !
  PT-ST-ACTIVE CEI-S _PT.S.STATE !
  TRUE CEI-S _PT.S.RET-ENABLED? !
  _PT-RD-AVAILABLE CEI-S _PT.S.RET-STATE !
  CEI-FEATURES @ CEI-S _PT.S.RET-CAPS 8 + _PT-U64!
  17 CEI-S _PT.S.REVISION ! ;
: CEI-DISPATCH  ( length -- )
  _PT-RX-LEN !
  CEI-PAYLOAD _PT-RX-P !
  _PT-M-CONTROL-EVENT _PT-RX-TYPE !
  1 _PT-RX-SEQNO !
  CEI-S _PT-RX-S !
  _PT-RX-LEN @ _PT-HDR + _PT-RX-TOTAL !
  CEI-S _PT-DISPATCH-CONTROL-EVENT CEI-DISPATCH-S !
  CEI-DISPATCH-S @ PT-S-OK = IF
    CEI-POLL CEI-S PT-EVENT-POLL CEI-POLL-HAS ! CEI-POLL-S !
  ELSE
    -1 CEI-POLL-S ! 0 CEI-POLL-HAS !
  THEN
  30 EMIT
  CEI-INIT-S @ . CEI-DISPATCH-S @ . CEI-POLL-S @ . CEI-POLL-HAS @ .
  CEI-POLL PT-EVENT-TYPE@ .
  CEI-POLL PT-EVENT-REVISION@ .
  CEI-POLL PT-CONTROL-EVENT-OWNER@ .
  CEI-POLL PT-CONTROL-EVENT-GENERATION@ .
  CEI-POLL PT-CONTROL-EVENT-ID@ .
  CEI-POLL PT-CONTROL-EVENT-KIND@ .
  CEI-POLL PT-CONTROL-EVENT-MODIFIERS@ .
  CEI-POLL PT-CONTROL-EVENT-CONTENT-REVISION@ .
  CEI-POLL PT-CONTROL-EVENT-ITEM-KEY@ .
  CEI-POLL PT-CONTROL-EVENT-OFFSET@ .
  CEI-POLL PT-CONTROL-EVENT-WHEEL-X@ .
  CEI-POLL PT-CONTROL-EVENT-WHEEL-Y@ .
  31 EMIT TX-FLUSH ;
"""

REPORT_FIELDS = (
    "init",
    "dispatch",
    "poll",
    "has",
    "type",
    "revision",
    "owner",
    "generation",
    "control",
    "kind",
    "modifiers",
    "content_revision",
    "item_key",
    "offset",
    "wheel_x",
    "wheel_y",
)


@pytest.fixture(scope="module")
def runtime():
    module_source = _rich_terminal_module_source()
    digest = hashlib.sha256(module_source).hexdigest()
    loaded = _load_exceptions()
    loaded.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + module_source,
        source_name=f"one-core-uart-lock-shims+rich-terminal.f:{digest}:complete",
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    loaded.evaluate(HARNESS_SOURCE, source_name="control-event-input.f")
    loaded.drain_uart_output()
    return loaded


def _dispatch(runtime, payload: bytes, *, features: int) -> dict[str, int]:
    """Boot one ACTIVE session, dispatch one payload, and read it back."""

    assert len(payload) <= 64
    lines = [f"{features} CEI-BOOT"]
    lines.extend(
        f"{byte} CEI-PAYLOAD {index} + C!" for index, byte in enumerate(payload)
    )
    lines.append(f"{len(payload)} CEI-DISPATCH")
    runtime.evaluate(("\n".join(lines) + "\n").encode(), source_name="case.f")
    output = runtime.drain_uart_output()
    begin = output.rindex(bytes((REPORT_BEGIN,)))
    end = output.index(bytes((REPORT_END,)), begin)
    values = [int(token) for token in re.findall(rb"-?\d+", output[begin + 1 : end])]
    assert runtime.main_context.data.snapshot() == ()
    return dict(zip(REPORT_FIELDS, values, strict=True))


def _event(kind: ControlEventKind, **fields) -> ControlEvent:
    return ControlEvent(7, 3, 11, kind, 0x01, REVISION, **fields)


def test_place_extend_and_scroll_round_trip_through_the_guest_parser(runtime):
    both = CONTROLS | CONTROL_COLLECTIONS
    place = _dispatch(
        runtime,
        encode_control_event(
            _event(
                ControlEventKind.PLACE,
                content_revision=41,
                item_key=5,
                scalar_offset=12,
            )
        ),
        features=both,
    )
    assert place["init"] == 0
    assert place["dispatch"] == 0
    assert (place["poll"], place["has"]) == (0, -1)
    assert place["type"] == 0x0205
    assert place["revision"] == REVISION
    assert (place["owner"], place["generation"], place["control"]) == (7, 3, 11)
    assert (place["kind"], place["modifiers"]) == (2, 1)
    assert (place["content_revision"], place["item_key"], place["offset"]) == (
        41,
        5,
        12,
    )
    assert (place["wheel_x"], place["wheel_y"]) == (0, 0)

    extend = _dispatch(
        runtime,
        encode_control_event(
            _event(
                ControlEventKind.EXTEND,
                content_revision=41,
                item_key=6,
                scalar_offset=0,
            )
        ),
        features=both,
    )
    assert (extend["dispatch"], extend["kind"]) == (0, 3)
    assert (extend["item_key"], extend["offset"]) == (6, 0)

    follow = _dispatch(
        runtime,
        encode_control_event(
            _event(
                ControlEventKind.FOLLOW,
                content_revision=41,
                item_key=7,
                scalar_offset=3,
            )
        ),
        features=both,
    )
    assert (follow["dispatch"], follow["kind"]) == (0, 5)
    assert (follow["content_revision"], follow["item_key"], follow["offset"]) == (
        41,
        7,
        3,
    )

    scroll = _dispatch(
        runtime,
        encode_control_event(_event(ControlEventKind.SCROLL, wheel_x=-1, wheel_y=3)),
        features=both,
    )
    assert (scroll["dispatch"], scroll["kind"]) == (0, 4)
    assert (scroll["wheel_x"], scroll["wheel_y"]) == (-1, 3)
    # Position readers never interpret another kind's tail.
    assert (scroll["content_revision"], scroll["item_key"], scroll["offset"]) == (
        0,
        0,
        0,
    )

    activate = _dispatch(
        runtime,
        encode_control_event(_event(ControlEventKind.ACTIVATE)),
        features=CONTROLS,
    )
    assert (activate["dispatch"], activate["kind"]) == (0, 1)
    assert (activate["content_revision"], activate["wheel_y"]) == (0, 0)


def _place_bytes() -> bytes:
    return encode_control_event(
        _event(
            ControlEventKind.PLACE,
            content_revision=41,
            item_key=5,
            scalar_offset=12,
        )
    )


def _with(payload: bytes, offset: int, replacement: bytes) -> bytes:
    raw = bytearray(payload)
    raw[offset : offset + len(replacement)] = replacement
    return bytes(raw)


@pytest.mark.parametrize(
    ("payload", "features"),
    (
        # Positioned kinds need RET_CONTROL_COLLECTIONS as well as RET_CONTROLS.
        (_place_bytes(), CONTROLS),
        # Each kind has one exact length.
        (_place_bytes()[:40], CONTROLS | CONTROL_COLLECTIONS),
        (
            encode_control_event(_event(ControlEventKind.ACTIVATE)) + bytes(24),
            CONTROLS | CONTROL_COLLECTIONS,
        ),
        # Tail reserved bytes, a zero content revision or item key, and a
        # scroll without detents are not canonical.
        (_with(_place_bytes(), 60, b"\x01"), CONTROLS | CONTROL_COLLECTIONS),
        (_with(_place_bytes(), 40, bytes(8)), CONTROLS | CONTROL_COLLECTIONS),
        (_with(_place_bytes(), 48, bytes(8)), CONTROLS | CONTROL_COLLECTIONS),
        (
            _with(
                encode_control_event(_event(ControlEventKind.SCROLL, wheel_y=1)),
                40,
                bytes(4),
            ),
            CONTROLS | CONTROL_COLLECTIONS,
        ),
        # The event must still name the session's current model revision.
        (
            _with(_place_bytes(), 32, (REVISION + 1).to_bytes(8, "little")),
            CONTROLS | CONTROL_COLLECTIONS,
        ),
        # Unknown kinds remain invalid.
        (
            _with(_place_bytes(), 24, (6).to_bytes(2, "little")),
            CONTROLS | CONTROL_COLLECTIONS,
        ),
        # FOLLOW needs RET_CONTROL_COLLECTIONS, like the other positions.
        (
            _with(_place_bytes(), 24, (5).to_bytes(2, "little")),
            CONTROLS,
        ),
    ),
)
def test_guest_parser_rejects_noncanonical_positioned_events(runtime, payload, features):
    report = _dispatch(runtime, payload, features=features)

    assert report["init"] == 0
    # PT-S-SESSION-LOST: the module fails the session rather than guessing.
    assert report["dispatch"] == 2
    assert report["has"] == 0
