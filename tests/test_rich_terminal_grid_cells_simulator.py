"""Complete-module grid-role publication through both hosted executors."""

from __future__ import annotations

import pytest

from rich_terminal.retained_wire import decode_control_definition, encode_control_definition
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS, SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source, _stored_cell,
)
from tests.test_rich_terminal_grid_cells_forth import grid_expected_frames, grid_harness


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_grid_cell_roles_use_host_codec(execution_backend: str) -> None:
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
    runtime.evaluate(grid_harness(), source_name="grid-cell-role-publication.f")
    assert runtime.drain_uart_output() == b""
    runtime.execute("GC-INITIALIZE")
    assert _stored_cell(runtime, "GC-INIT-STATUS") == 0
    runtime.evaluate(b"0x301 GC-FEATURES! GC-CASE-3 GC-DEFINE")
    assert _stored_cell(runtime, "GC-STATUS") == 4
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    expected = grid_expected_frames()
    for index, frame in enumerate(expected):
        features = "0x301" if index == 0 else "0x8301"
        writer = "GC-DEFINE" if index == 0 else "GC-REPLACE"
        runtime.evaluate(f"{features} GC-FEATURES! GC-CASE-{index} {writer}".encode())
        assert _stored_cell(runtime, "GC-STATUS") == 0
        assert runtime.drain_uart_output() == frame
        payload = frame[40:]
        definition = decode_control_definition(payload)
        assert int(definition.kind) == 6
        assert [int(item.role) for item in definition.content.items] == ([1, 1, 1] if index == 0 else [4, 5, 6])
        assert all(not item.runs for item in definition.content.items)
        assert encode_control_definition(definition) == payload
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        for name in ("_PT-CT-CONTENT-A", "_PT-CT-TA", "_PT-GC-A", "_PT-GC-END", "_PT-GC-P", "_TASK-HANDLERS"):
            assert _stored_cell(runtime, name) == 0

    runtime.evaluate(
        b"GC-S _PT.S.TX-RET-OPS-DONE @ GC-OPS ! "
        b"GC-S _PT.S.TX-RET-BYTES-DONE @ GC-BYTES !",
        source_name="grid-cell-accounting-observation.f",
    )
    assert _stored_cell(runtime, "GC-OPS") == 3
    assert _stored_cell(runtime, "GC-BYTES") == sum(map(len, expected))
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
