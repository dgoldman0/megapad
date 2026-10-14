"""Native plans for words with Python execution overlays.

A word with an overlay is planned like any colon word, so when its overlay
declines, Python can hand the body to the native executor. Native code never
enters such a plan on its own: calls compile to stops and ``EXECUTE``
declines, so every entry still meets the overlay first. Both cases must
match the Python backend exactly.
"""

from __future__ import annotations

import hashlib

import pytest

pytest.importorskip("_megaforth_native")

from simulator.source_acceleration import install_kdos_source_accelerators  # noqa: E402
from tests.simulator.test_kdos_module_system import _load_module_system  # noqa: E402


@pytest.fixture
def runtimes(monkeypatch):
    pair = []
    for backend in ("python", "native"):
        monkeypatch.setenv("MEGAFORTH_EXECUTOR", backend)
        runtime = _load_module_system()
        install_kdos_source_accelerators(runtime)
        pair.append(runtime)
    return pair


def _digest(runtime) -> str:
    digest = hashlib.sha256()
    for region in runtime.memory._regions:
        for index in sorted(region.pages):
            page = bytes(region.pages[index])
            if any(page):
                digest.update(index.to_bytes(8, "little") + page)
    return digest.hexdigest()


def _run(runtime, word: str, source: bytes):
    buffer = runtime.define_created("OVERLAID-SOURCE", initial_body=source)
    runtime.main_context.data.push(buffer.body_address)
    runtime.main_context.data.push(len(source))
    native_before = runtime.native_execution_stats["semantic_steps"]
    result = runtime.execute(word)
    observation = (
        result.semantic_steps,
        runtime.main_context.data.snapshot(),
        runtime.main_context.returns.snapshot(),
        runtime.drain_uart_output(),
        runtime.dictionary.here,
        _digest(runtime),
    )
    return observation, runtime.native_execution_stats["semantic_steps"] - native_before


def test_a_declined_overlay_runs_its_body_natively(runtimes) -> None:
    # Interpreting a colon word makes EVALUATE-CHECKED's overlay decline, so
    # each line runs the complete KDOS body under CATCH.
    source = b": SQ DUP * ;\n" + b"3 SQ DROP\n" * 8
    (expected, _), (observed, native_steps) = (
        _run(runtime, "SOURCE-EVALUATE-CHECKED", source) for runtime in runtimes
    )
    assert observed == expected
    assert native_steps > 0


def test_native_execute_never_enters_an_overlaid_plan() -> None:
    import _megaforth_native as native
    from simulator.stacks import Continuation

    def program(overlaid: bool):
        page = bytearray(4096)
        built = native.NativeProgram([(0, 4096, {0: page})], 4096, Continuation)
        # Word 2 would run natively from its first operation; word 1 EXECUTEs it.
        built.install(2, [(native.OP_LITERAL, 7, 0), (native.OP_RETURN, 0, 0)], overlaid)
        built.install(1, [(native.OP_LITERAL, 2, 0), (native.OP_EXECUTE, 0, 0),
                          (native.OP_RETURN, 0, 0)])
        return built.run(1, 0, (0, 2048, 2048), (3072, 4096, 4096, 0), {}, 20)

    ordinary = program(False)
    overlaid = program(True)
    # Without an overlay, EXECUTE enters word 2, runs its literal and returns
    # into word 1, stopping at the root return Python owns.
    assert ordinary[:3] == (1, 2, 5)
    # With one, EXECUTE stops before any effect so Python meets the overlay.
    assert overlaid[:3] == (1, 1, 1)
    assert overlaid[3] == 2040
