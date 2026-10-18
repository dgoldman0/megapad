"""The complete guest module's DIALOG and BUTTON writers on Python and native
simulator executors: canonical publication through the host codec, and the
record-local refusals the module owns (RETAINED-1 sections 9.1, 9.3, 9.4)."""

from __future__ import annotations

import struct

import pytest

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_wire import (
    RetainedMessageType, decode_control_definition, encode_control_definition,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    KDOS_SHIMS, SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source, _stored_cell,
)


DIALOGS = 0x20101  # CORE, CONTROLS, and DIALOGS
DLG1 = struct.Struct("<IHHQIIiiIIiiII")
MESSAGE = "Delete notes.md?\nIt cannot be undone.".encode()
# (kind, state, label, shortcut, cols, rows, content)
CASES = (
    (15, 1, b"Delete", b"", 20, 6,
     DLG1.pack(0x31474C44, 1, 0, 7, 3, len(MESSAGE), 1, 0, 18, 1, 1, 1, 18, 2) + MESSAGE),
    (15, 1, b"", b"", 20, 1,
     DLG1.pack(0x31474C44, 1, 1, 8, 4, 14, 0, 0, 0, 0, 0, 0, 20, 1) + b"Could not save"),
    (16, 0x0B, b"Delete", b"D", 8, 1, struct.pack("<IHH", 0x314E5442, 1, 6)),
)


def dialog_expected_frames() -> tuple[bytes, ...]:
    frames = []
    for index, (kind, state, label, shortcut, cols, rows, body) in enumerate(CASES):
        prefix = struct.pack(
            "<QQQHHiQQIiiIIIII", 0x0102030405060708, 0x1112131415161718,
            18 + index, kind, state, 4, 0x6162636465666768, 0, 0,
            2, 3, cols, rows, len(label), len(shortcut), len(body),
        )
        frames.append(encode_frame(Frame(
            RetainedMessageType.CONTROL_DEFINE, 0x4142434445464748, index, 9,
            prefix + label + shortcut + body,
        ), max_payload=256))
    return tuple(frames)


def dialog_harness() -> bytes:
    lines = [
        "CREATE DH-RX 8192 ALLOT CREATE DH-TX 8192 ALLOT",
        "CREATE DH-EVENT PT-EVENT-SIZE ALLOT",
        "CREATE DH-STORAGE PT-SESSION-SIZE 7 + ALLOT",
        ": DH-S DH-STORAGE 7 + -8 AND ;",
        "CREATE DH-LABEL 16 ALLOT CREATE DH-SHORTCUT 16 ALLOT CREATE DH-CONTENT 512 ALLOT",
    ]
    for name in ("INIT-STATUS", "STATUS", "ID", "KIND", "STATE", "PARENT", "ORDER", "COLS",
                 "ROWS", "LABEL-U", "SHORTCUT-U", "CONTENT-U"):
        lines.append(f"VARIABLE DH-{name}")
    for index, (_, _, label, shortcut, _, _, body) in enumerate(CASES):
        lines.append(f"CREATE DH-BODY-{index} {len(body)} ALLOT")
        lines.append(f"CREATE DH-LABEL-{index} {max(1, len(label))} ALLOT")
        lines.append(f"CREATE DH-SHORTCUT-{index} {max(1, len(shortcut))} ALLOT")
    lines += [
        "\\ An empty string is passed canonically, as address zero.",
        ": DH-SPAN ( a u -- a' u ) DUP 0= IF NIP 0 SWAP THEN ;",
        ": DH-ARGS 0x0102030405060708 0x1112131415161718 DH-ID @",
        "  DH-KIND @ DH-STATE @ 4 0x6162636465666768",
        "  DH-PARENT @ DH-ORDER @ 2 3 DH-COLS @ DH-ROWS @",
        "  DH-LABEL DH-LABEL-U @ DH-SPAN DH-SHORTCUT DH-SHORTCUT-U @ DH-SPAN",
        "  DH-CONTENT DH-CONTENT-U @ DH-S ;",
        ": DH-DEFINE DH-ARGS PT-CONTROL-DEFINE DH-STATUS ! TX-FLUSH ;",
        ": DH-FEATURES! DH-S _PT.S.RET-CAPS 8 + _PT-U64! ;",
        ": DH-INITIALIZE",
        "  DH-RX 8192 DH-TX 8192 DH-EVENT PT-EVENT-SIZE DH-S",
        "  PT-INIT DH-INIT-STATUS ! PT-ST-ACTIVE DH-S _PT.S.STATE !",
        "  256 DH-S _PT.S.PEER-MAX-PAY ! 4096 DH-S _PT.S.PEER-MAX-TX !",
        "  8192 DH-S _PT.S.PEER-GRANT ! 8192 DH-S _PT.S.PEER-INITIAL !",
        "  0x4142434445464748 DH-S _PT.S.SESSION-ID ! 9 DH-S _PT.S.EPOCH !",
        "  -1 DH-S _PT.S.RET-ENABLED? !",
        f"  _PT-RD-AVAILABLE DH-S _PT.S.RET-STATE ! {DIALOGS:#x} DH-FEATURES!",
        "  -1 DH-S _PT.S.TX-OPEN? ! _PT-TX-PRESENT DH-S _PT.S.TX-KIND !",
        "  PT-CELL-NONE DH-S _PT.S.TX-CELL-MODE !",
        "  PT-RET-DELTA DH-S _PT.S.TX-RET-MODE !",
        f"  {len(CASES)} DH-S _PT.S.TX-RET-OPS !",
        f"  {sum(map(len, dialog_expected_frames()))} DH-S _PT.S.TX-RET-BYTES !",
    ]
    for index, (_, _, label, shortcut, _, _, body) in enumerate(CASES):
        for name, data in ((f"BODY-{index}", body), (f"LABEL-{index}", label),
                           (f"SHORTCUT-{index}", shortcut)):
            lines += [f"  {byte} DH-{name} {offset} + C!" for offset, byte in enumerate(data)]
    lines += ["  ;"]
    for index, (kind, state, label, shortcut, cols, rows, body) in enumerate(CASES):
        lines += [
            f": DH-CASE-{index} {18 + index} DH-ID ! {kind} DH-KIND ! {state} DH-STATE !",
            f"  0 DH-PARENT ! 0 DH-ORDER ! {cols} DH-COLS ! {rows} DH-ROWS !",
            f"  DH-LABEL-{index} DH-LABEL {len(label)} MOVE {len(label)} DH-LABEL-U !",
            f"  DH-SHORTCUT-{index} DH-SHORTCUT {len(shortcut)} MOVE {len(shortcut)} DH-SHORTCUT-U !",
            f"  DH-BODY-{index} DH-CONTENT {len(body)} MOVE {len(body)} DH-CONTENT-U ! ;",
        ]
    return ("\n".join(lines) + "\n").encode()


# Each mutation follows its case and must leave PT-CONTROL-DEFINE refusing
# the record as INVALID (3) before any byte is sent.
INVALID = [
    (0, "1 DH-PARENT !"), (0, "1 DH-ORDER !"), (0, "0 DH-COLS !"),
    (0, "3 DH-STATE !"), (0, "2 DH-SHORTCUT-U !"), (0, "55 DH-CONTENT-U !"),
    (0, "0 DH-CONTENT L!"), (0, "2 DH-CONTENT 4 + W!"), (0, "2 DH-CONTENT 6 + W!"),
    (0, "0 DH-CONTENT 8 + _PT-U64!"), (0, "5 DH-CONTENT 16 + L!"),
    (0, "1 DH-CONTENT 20 + L!"),                 # message length mismatch
    (0, "0 DH-LABEL-U !"),                       # a title slot without a title
    (0, "20 DH-CONTENT 32 + L!"),                # the title slot passes the root
    (0, "0 DH-CONTENT 36 + L!"),                 # an empty title slot
    (0, "0 DH-CONTENT 44 + L!"),                 # the message slot over the title
    (0, "1 DH-CONTENT 52 + L!"),                 # two lines in one row
    (0, "9 DH-CONTENT 60 + C!"),                 # a tab in the message
    (0, "194 DH-CONTENT 60 + C! 133 DH-CONTENT 61 + C!"),     # C1 NEL
    (0, "226 DH-CONTENT 60 + C! 128 DH-CONTENT 61 + C! 168 DH-CONTENT 62 + C!"),
    (0, "255 DH-CONTENT 60 + C!"),               # not UTF-8
    (1, "0 DH-CONTENT 48 + L!"),                 # a message without its slot
    (1, "1 DH-CONTENT 24 + L!"),                 # a title slot with no title
    (2, "0 DH-LABEL-U !"), (2, "9 DH-CONTENT-U !"), (2, "0 DH-CONTENT L!"),
    (2, "2 DH-CONTENT 4 + W!"), (2, "8 DH-CONTENT 6 + W!"),
    (2, "5 DH-CONTENT 6 + W!"),                  # destructive and default
    (2, "0x13 DH-STATE !"), (2, "1 DH-PARENT !"), (2, "9 DH-STATE !"),
]


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_dialog_publication_and_refusals(execution_backend: str) -> None:
    runtime = _load_exceptions(MegaForthRuntime(execution_backend=execution_backend))
    runtime.evaluate(KDOS_SHIMS + _rich_terminal_module_source(),
                     source_name="kdos-shims+rich-terminal.f:complete",
                     step_budget=SIMULATOR_SOURCE_MAX_STEPS)
    runtime.evaluate(dialog_harness(), source_name="dialog-control-publication.f")
    runtime.execute("DH-INITIALIZE")
    assert _stored_cell(runtime, "DH-INIT-STATUS") == 0
    assert runtime.drain_uart_output() == b""

    # Without DIALOGS a session has no dialog or button to send.
    for case in range(len(CASES)):
        runtime.evaluate(f"0x101 DH-FEATURES! DH-CASE-{case} DH-DEFINE".encode())
        assert _stored_cell(runtime, "DH-STATUS") == 4
        assert runtime.drain_uart_output() == b""

    for case, mutation in INVALID:
        runtime.evaluate(f"{DIALOGS:#x} DH-FEATURES! DH-CASE-{case} {mutation} DH-DEFINE".encode())
        assert (_stored_cell(runtime, "DH-STATUS"), case, mutation) == (3, case, mutation)
        assert runtime.drain_uart_output() == b""
        assert runtime.main_context.data.snapshot() == ()

    for index, frame in enumerate(dialog_expected_frames()):
        runtime.evaluate(f"{DIALOGS:#x} DH-FEATURES! DH-CASE-{index} DH-DEFINE".encode())
        assert _stored_cell(runtime, "DH-STATUS") == 0
        assert runtime.drain_uart_output() == frame
        payload = frame[40:]
        definition = decode_control_definition(payload)
        assert int(definition.kind) == CASES[index][0]
        assert encode_control_definition(definition) == payload
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        for name in ("_PT-CT-LABEL-A", "_PT-CT-CONTENT-A", "_PT-FD-A", "_PT-DL-LINES"):
            assert _stored_cell(runtime, name) == 0
