"""Taskbar publication through complete Forth on both hosted executors."""

from __future__ import annotations

import pytest

from rich_terminal.retained_wire import decode_control_definition, encode_control_definition
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    ONE_CORE_UART_LOCK_SHIMS,
    SIMULATOR_SOURCE_MAX_STEPS,
    _rich_terminal_module_source,
    _stored_cell,
)
from tests.test_rich_terminal_taskbar_forth import taskbar_expected_frames, taskbar_harness


@pytest.mark.parametrize("execution_backend", ("python", "native"))
def test_complete_module_taskbar_publication_uses_host_codec(execution_backend: str) -> None:
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
    runtime.evaluate(taskbar_harness(), source_name="taskbar-control-publication.f")
    assert runtime.drain_uart_output() == b""
    runtime.execute("TB-INITIALIZE")
    assert _stored_cell(runtime, "TB-INIT-STATUS") == 0
    assert runtime.drain_uart_output() == b""

    for shape in ("TB-BAR-ARGS", "TB-DEFAULTS", "TB-LAUNCHER-ARGS"):
        runtime.evaluate(f"0x101 TB-FEATURES! {shape} TB-DEFINE".encode())
        assert _stored_cell(runtime, "TB-STATUS") == 4
        assert runtime.drain_uart_output() == b""
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()

    expected_frames = taskbar_expected_frames()
    for index, command in enumerate((
        b"0x2101 TB-FEATURES! TB-BAR-ARGS TB-DEFINE",
        b"TB-DEFAULTS TB-DEFINE",
        b"TB-LAUNCHER-ARGS TB-DEFINE",
        b"TB-DEFAULTS 11 TB-STATE ! TB-REPLACE",
        b"TB-DROP",
    )):
        runtime.evaluate(command)
        assert _stored_cell(runtime, "TB-STATUS") == 0
        assert runtime.drain_uart_output() == expected_frames[index]
        if index < 4:
            payload = expected_frames[index][40:]
            decoded = decode_control_definition(payload)
            assert int(decoded.kind) == (10, 11, 12, 11)[index]
            assert encode_control_definition(decoded) == payload
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        for name in ("_PT-CT-LABEL-A", "_PT-CT-SHORTCUT-A", "_PT-CT-CONTENT-A", "_PT-CT-TA", "_TASK-HANDLERS"):
            assert _stored_cell(runtime, name) == 0

    runtime.evaluate(
        b"TB-S _PT.S.TX-RET-OPS-DONE @ TB-OPS ! "
        b"TB-S _PT.S.TX-RET-BYTES-DONE @ TB-BYTES !",
        source_name="taskbar-accounting-observation.f",
    )
    assert _stored_cell(runtime, "TB-OPS") == len(expected_frames)
    assert _stored_cell(runtime, "TB-BYTES") == sum(map(len, expected_frames))
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
