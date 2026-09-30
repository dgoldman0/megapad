"""Complete production-source PANE publication on both hosted executors."""

from __future__ import annotations

import pytest

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_scene import ObjectBounds, PaneBody
from rich_terminal.retained_wire import (
    ObjectWireDefinition,
    RetainedMessageType,
    decode_object_definition,
    encode_object_definition,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS,
    SIMULATOR_SOURCE_MAX_STEPS,
    SourceCase,
    _rich_terminal_module_source,
    _stored_cell,
)


PANE_CASE = SourceCase(
    source_name="pane-complete-module-publication.f",
    entry_word=b"PS-PUBLISH",
    harness=b"""
CREATE PS-RX 8192 ALLOT
CREATE PS-TX 8192 ALLOT
CREATE PS-EVENT PT-EVENT-SIZE ALLOT
CREATE PS-STORAGE PT-SESSION-SIZE 7 + ALLOT
: PS-S PS-STORAGE 7 + -8 AND ;
CREATE PS-TITLE 5 ALLOT
VARIABLE PS-INIT-STATUS
VARIABLE PS-STATUS
VARIABLE PS-OPS
VARIABLE PS-BYTES
: PS-INITIALIZE
  PS-RX 8192 PS-TX 8192 PS-EVENT PT-EVENT-SIZE PS-S
    PT-INIT PS-INIT-STATUS !
  80 PS-TITLE C! 195 PS-TITLE 1 + C! 162 PS-TITLE 2 + C!
  110 PS-TITLE 3 + C! 101 PS-TITLE 4 + C!
  PT-ST-ACTIVE PS-S _PT.S.STATE !
  128 PS-S _PT.S.PEER-MAX-PAY !
  4096 PS-S _PT.S.PEER-MAX-TX !
  8192 PS-S _PT.S.PEER-GRANT !
  8192 PS-S _PT.S.PEER-INITIAL !
  0x4142434445464748 PS-S _PT.S.SESSION-ID !
  9 PS-S _PT.S.EPOCH !
  -1 PS-S _PT.S.RET-ENABLED? !
  _PT-RD-AVAILABLE PS-S _PT.S.RET-STATE !
  -1 PS-S _PT.S.TX-OPEN? !
  _PT-TX-PRESENT PS-S _PT.S.TX-KIND !
  PT-CELL-NONE PS-S _PT.S.TX-CELL-MODE !
  PT-RET-DELTA PS-S _PT.S.TX-RET-MODE !
  1 PS-S _PT.S.TX-RET-OPS !
  149 PS-S _PT.S.TX-RET-BYTES ! ;
: PS-CALL
  0x0102030405060708 0x1112131415161718
  0x2122232425262728 0x6162636465666768 0
  -2 3 1 6 -7 PT-OBJECT-VISIBLE
  0x7172737475767778 0 0 1 4 PT-PANE-FOCUSED
  PS-TITLE 5 PS-S PT-PANE-DEFINE PS-STATUS ! TX-FLUSH ;
: PS-UNSUPPORTED
  1 PS-S _PT.S.RET-CAPS 8 + _PT-U64! PS-CALL ;
: PS-PUBLISH
  0x801 PS-S _PT.S.RET-CAPS 8 + _PT-U64! PS-CALL ;
""",
)


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_pane_publication_uses_host_codec(
    execution_backend: str,
) -> None:
    runtime = _load_exceptions(
        MegaForthRuntime(execution_backend=execution_backend),
    )
    assert runtime.execution_backend == execution_backend
    catch_word = runtime.find("CATCH")
    result = runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + _rich_terminal_module_source(),
        source_name="one-core-uart-lock-shims+rich-terminal.f:complete",
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    assert result.definitions[-1].name == b"PT-TX-ABORT"
    assert runtime.find("CATCH") is catch_word
    assert runtime.find("PT-PANE-DEFINE") is not None
    assert runtime.find("PT-PANE-REPLACE") is not None
    runtime.evaluate(PANE_CASE.harness, source_name=PANE_CASE.source_name)
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    runtime.execute("PS-INITIALIZE")
    assert _stored_cell(runtime, "PS-INIT-STATUS") == 0
    assert runtime.drain_uart_output() == b""
    runtime.execute("PS-UNSUPPORTED")
    assert _stored_cell(runtime, "PS-STATUS") == 4
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    runtime.execute(PANE_CASE.entry_word)
    assert _stored_cell(runtime, "PS-STATUS") == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert _stored_cell(runtime, "_PT-PN-TITLE-A") == 0
    assert _stored_cell(runtime, "_PT-PN-TITLE-U") == 0
    assert _stored_cell(runtime, "_TASK-HANDLERS") == 0

    # Geometry is unchanged even though this title cannot be painted: the
    # pane is one column wide and content starts at its first row.
    definition = ObjectWireDefinition(
        owner_id=0x0102030405060708,
        owner_generation=0x1112131415161718,
        object_id=0x2122232425262728,
        region_id=0x6162636465666768,
        parent_object_id=0,
        bounds=ObjectBounds(-2, 3, 1, 6),
        z_order=-7,
        visible=True,
        body=PaneBody(
            content_region_id=0x7172737475767778,
            content_bounds=ObjectBounds(0, 0, 1, 4),
            title="Pâne",
            focused=True,
        ),
    )
    payload = encode_object_definition(definition)
    assert decode_object_definition(payload) == definition
    assert runtime.drain_uart_output() == encode_frame(
        Frame(
            RetainedMessageType.OBJECT_DEFINE,
            0x4142434445464748,
            0,
            9,
            payload,
        ),
        max_payload=128,
    )
    runtime.evaluate(
        b"PS-S _PT.S.TX-RET-OPS-DONE @ PS-OPS ! "
        b"PS-S _PT.S.TX-RET-BYTES-DONE @ PS-BYTES !",
        source_name="pane-accounting-observation.f",
    )
    assert _stored_cell(runtime, "PS-OPS") == 1
    assert _stored_cell(runtime, "PS-BYTES") == 149
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
