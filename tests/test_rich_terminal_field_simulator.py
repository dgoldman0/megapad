"""Complete module FIELD publication on Python and native simulator executors."""

from __future__ import annotations

import pytest

from rich_terminal.retained_wire import decode_control_definition, encode_control_definition
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS, SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source, _stored_cell,
)
from tests.test_rich_terminal_field_forth import field_expected_frames, field_harness


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_field_publication_uses_host_codec(execution_backend: str) -> None:
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
    runtime.evaluate(field_harness(), source_name="field-control-publication.f")
    assert runtime.drain_uart_output() == b""
    runtime.execute("FH-INITIALIZE")
    assert _stored_cell(runtime, "FH-INIT-STATUS") == 0
    assert runtime.drain_uart_output() == b""

    for case in range(3):
        runtime.evaluate(f"0x101 FH-FEATURES! FH-CASE-{case} FH-DEFINE".encode())
        assert _stored_cell(runtime, "FH-STATUS") == 4
        assert runtime.drain_uart_output() == b""
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()

    expected = field_expected_frames()
    for index, frame in enumerate(expected):
        writer = "FH-DEFINE" if index == 0 else "FH-REPLACE"
        runtime.evaluate(f"0x4101 FH-FEATURES! FH-CASE-{index} {writer}".encode())
        assert _stored_cell(runtime, "FH-STATUS") == 0
        assert runtime.drain_uart_output() == frame
        payload = frame[40:]
        definition = decode_control_definition(payload)
        assert int(definition.kind) == 13
        assert encode_control_definition(definition) == payload
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        for name in (
            "_PT-CT-LABEL-A", "_PT-CT-CONTENT-A", "_PT-CT-TA", "_PT-FD-A",
            "_PT-FD-END", "_PT-FD-P", "_PT-FD-PREV", "_PT-FD-RECT", "_TASK-HANDLERS",
        ):
            assert _stored_cell(runtime, name) == 0

    runtime.evaluate(
        b"FH-S _PT.S.TX-RET-OPS-DONE @ FH-OPS ! "
        b"FH-S _PT.S.TX-RET-BYTES-DONE @ FH-BYTES !",
        source_name="field-accounting-observation.f",
    )
    assert _stored_cell(runtime, "FH-OPS") == 4
    assert _stored_cell(runtime, "FH-BYTES") == sum(map(len, expected))
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
