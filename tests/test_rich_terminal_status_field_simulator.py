"""Complete production-source STATUS_FIELD publication on both hosted executors."""

from __future__ import annotations

import pytest

from rich_terminal.apt1 import Frame, encode_frame
from rich_terminal.retained_scene import ObjectBounds, StatusFieldBody
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
    _rich_terminal_module_source,
    _stored_cell,
)


STATUS_FIELD_HARNESS = b"""
CREATE SS-RX 8192 ALLOT
CREATE SS-TX 8192 ALLOT
CREATE SS-EVENT PT-EVENT-SIZE ALLOT
CREATE SS-STORAGE PT-SESSION-SIZE 7 + ALLOT
: SS-S SS-STORAGE 7 + -8 AND ;
CREATE SS-LABEL 3 ALLOT
CREATE SS-VALUE 5 ALLOT
VARIABLE SS-INIT-STATUS
VARIABLE SS-STATUS
VARIABLE SS-OPS
VARIABLE SS-BYTES
: SS-INITIALIZE
  SS-RX 8192 SS-TX 8192 SS-EVENT PT-EVENT-SIZE SS-S
    PT-INIT SS-INIT-STATUS !
  67 SS-LABEL C! 80 SS-LABEL 1 + C! 85 SS-LABEL 2 + C!
  80 SS-VALUE C! 114 SS-VALUE 1 + C! 195 SS-VALUE 2 + C!
  170 SS-VALUE 3 + C! 116 SS-VALUE 4 + C!
  PT-ST-ACTIVE SS-S _PT.S.STATE !
  128 SS-S _PT.S.PEER-MAX-PAY !
  4096 SS-S _PT.S.PEER-MAX-TX !
  8192 SS-S _PT.S.PEER-GRANT !
  8192 SS-S _PT.S.PEER-INITIAL !
  0x4142434445464748 SS-S _PT.S.SESSION-ID !
  9 SS-S _PT.S.EPOCH !
  -1 SS-S _PT.S.RET-ENABLED? !
  _PT-RD-AVAILABLE SS-S _PT.S.RET-STATE !
  -1 SS-S _PT.S.TX-OPEN? !
  _PT-TX-PRESENT SS-S _PT.S.TX-KIND !
  PT-CELL-NONE SS-S _PT.S.TX-CELL-MODE !
  PT-RET-DELTA SS-S _PT.S.TX-RET-MODE !
  2 SS-S _PT.S.TX-RET-OPS !
  288 SS-S _PT.S.TX-RET-BYTES ! ;
: SS-ARGS
  0x0102030405060708 0x1112131415161718
  0x2122232425262728 0x6162636465666768 17
  -2 3 10 1 -7 PT-OBJECT-VISIBLE
  4 PT-SEVERITY-SUCCESS PT-STATUS-FIELD-EMPHASIZED
  SS-LABEL 3 SS-VALUE 5 SS-S ;
: SS-UNSUPPORTED
  1 SS-S _PT.S.RET-CAPS 8 + _PT-U64!
  SS-ARGS PT-STATUS-FIELD-DEFINE SS-STATUS ! TX-FLUSH ;
: SS-PUBLISH
  0x1001 SS-S _PT.S.RET-CAPS 8 + _PT-U64!
  SS-ARGS PT-STATUS-FIELD-DEFINE SS-STATUS ! TX-FLUSH ;
: SS-REPLACE
  SS-ARGS PT-STATUS-FIELD-REPLACE SS-STATUS ! TX-FLUSH ;
"""


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_status_field_publication_uses_host_codec(
    execution_backend: str,
) -> None:
    runtime = _load_exceptions(MegaForthRuntime(execution_backend=execution_backend))
    assert runtime.execution_backend == execution_backend
    catch_word = runtime.find("CATCH")
    result = runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + _rich_terminal_module_source(),
        source_name="one-core-uart-lock-shims+rich-terminal.f:complete",
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    assert result.definitions[-1].name == b"PT-TX-ABORT"
    assert runtime.find("CATCH") is catch_word
    assert runtime.find("PT-STATUS-FIELD-DEFINE") is not None
    assert runtime.find("PT-STATUS-FIELD-REPLACE") is not None
    runtime.evaluate(STATUS_FIELD_HARNESS, source_name="status-field-publication.f")
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    runtime.execute("SS-INITIALIZE")
    assert _stored_cell(runtime, "SS-INIT-STATUS") == 0
    assert runtime.drain_uart_output() == b""
    runtime.execute("SS-UNSUPPORTED")
    assert _stored_cell(runtime, "SS-STATUS") == 4
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    definition = ObjectWireDefinition(
        owner_id=0x0102030405060708,
        owner_generation=0x1112131415161718,
        object_id=0x2122232425262728,
        region_id=0x6162636465666768,
        parent_object_id=17,
        bounds=ObjectBounds(-2, 3, 10, 1),
        z_order=-7,
        visible=True,
        body=StatusFieldBody(
            label="CPU", value="Prêt", label_cols=4, severity=2, emphasized=True,
        ),
    )
    payload = encode_object_definition(definition)
    assert decode_object_definition(payload) == definition
    for sequence, (word, message) in enumerate((
        ("SS-PUBLISH", RetainedMessageType.OBJECT_DEFINE),
        ("SS-REPLACE", RetainedMessageType.OBJECT_REPLACE),
    )):
        runtime.execute(word)
        assert _stored_cell(runtime, "SS-STATUS") == 0
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        for name in ("_PT-SF-LABEL-A", "_PT-SF-LABEL-U", "_PT-SF-VALUE-A", "_PT-SF-VALUE-U", "_PT-SF-TEXT-A", "_PT-SF-TEXT-U", "_TASK-HANDLERS"):
            assert _stored_cell(runtime, name) == 0
        assert runtime.drain_uart_output() == encode_frame(
            Frame(message, 0x4142434445464748, sequence, 9, payload),
            max_payload=128,
        )
    runtime.evaluate(
        b"SS-S _PT.S.TX-RET-OPS-DONE @ SS-OPS ! "
        b"SS-S _PT.S.TX-RET-BYTES-DONE @ SS-BYTES !",
        source_name="status-field-accounting-observation.f",
    )
    assert _stored_cell(runtime, "SS-OPS") == 2
    assert _stored_cell(runtime, "SS-BYTES") == 288
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
