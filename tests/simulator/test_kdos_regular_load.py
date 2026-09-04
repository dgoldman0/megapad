"""Moderate CLI-like acceptance for an ordinary semantic KDOS load."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64, TRUE
from simulator.dictionary_index import (
    DICT_INDEX_AUTHORITATIVE,
    DICT_INDEX_BOUND,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_bios_mp64fs import _formatted_image
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
KDOS_LINES = 9_894
KDOS_BYTES = 343_551
KDOS_SHA256 = (
    "b9e6ab1f3fa6331d14db4c94b7ed6978b78b2acd45c311fdecf566dcce4e00ae"
)
SUBMITTED_LINES = 6_681
SUBMITTED_PAYLOAD_BYTES = 215_630
CLI_UART_BYTES = 222_311
MAX_SUBMITTED_LINE = 99
CANONICAL_EXTERNAL_BYTES = 128 << 20
CANONICAL_HBW_BYTES = 3 << 20
CANONICAL_VRAM_BYTES = 4 << 20

STARTUP_BANNER = (
    b"\r\n"
    + b"-" * 60
    + b"\r\n"
    + b"  KDOS v1.1 \xe2\x80\x94 Kernel Dashboard OS\r\n"
    + b"-" * 60
    + b"\r\n"
    + b" Type HELP for commands, HELP <word> for details.\r\n"
    + b" Type SCREENS for interactive TUI (or N SCREEN for screen N).\r\n"
    + b" Type TOPICS or LESSONS for documentation.\r\n"
)

REPRESENTATIVE_WORDS = (
    b".R",
    b"CRC32-STR",
    b"XMEM-INIT",
    b"ALLOCATE",
    b"BUFFER",
    b"KERNEL",
    b"PIPELINE",
    b"FS-LOAD",
    b"LOAD",
    b"DOC",
    b"WORDS-LIKE",
    b"SPAWN",
    b"PREEMPT-ON",
    b"CORE-RUN",
    b"SCREENS",
    b"PORT!",
    b"RING",
    b"HASHTABLE",
    b"REQUIRE",
    b"_AUTOEXEC-RUN",
)


def _verified_kdos() -> bytes:
    source = KDOS_SOURCE.read_bytes()
    assert len(source) == KDOS_BYTES
    assert source.count(b"\n") == KDOS_LINES
    assert hashlib.sha256(source).hexdigest() == KDOS_SHA256
    assert _git_blob_id(source) == KDOS_GIT_BLOB
    assert source.startswith(b"\\ ===========================================")
    assert source.endswith(b"JIT-OFF\nCR\n")
    return source


def _submitted_lines(source: bytes) -> tuple[tuple[int, bytes], ...]:
    lines = tuple(
        (line_number, line)
        for line_number, line in enumerate(source.splitlines(), start=1)
        if line.strip() and not line.strip().startswith(b"\\")
    )
    assert len(lines) == SUBMITTED_LINES
    payload_bytes = sum(len(line) for _line_number, line in lines)
    assert payload_bytes == SUBMITTED_PAYLOAD_BYTES
    assert payload_bytes + len(lines) == CLI_UART_BYTES
    assert max(len(line) for _line_number, line in lines) == MAX_SUBMITTED_LINE
    return lines


def _evaluate_checked_line(
    runtime: MegaForthRuntime,
    *,
    evaluator_xt: int,
    line_cell: int,
    source_address: int,
    line_number: int,
    source: bytes,
) -> None:
    runtime.memory.write64(line_cell, line_number)
    runtime.memory.write_bytes(source_address, source)
    runtime.main_context.data.push(source_address)
    runtime.main_context.data.push(len(source))
    runtime.execute(evaluator_xt)
    status = runtime.main_context.data.pop()
    assert status == 0, (
        f"checked EVALUATE failed at kdos.f:{line_number}: "
        f"status={status}, source={source!r}"
    )


def _execute(
    runtime: MegaForthRuntime,
    name: bytes | str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name)
    result = context.data.snapshot()
    context.data.clear()
    assert context.returns.snapshot() == ()
    return result


def _variable(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return runtime.memory.read64(_execute(runtime, name)[0])


def _constant(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return _execute(runtime, name)[0]


def test_complete_kdos_loads_once_and_runs_representative_subsystems() -> None:
    """Feed the ordinary file like the CLI, then cross section seams."""

    memory = create_one_core_address_space(
        external_size=CANONICAL_EXTERNAL_BYTES,
        hbw_size=CANONICAL_HBW_BYTES,
        vram_size=CANONICAL_VRAM_BYTES,
    )
    runtime = MegaForthRuntime(memory=memory)
    core_words = runtime.dictionary.words
    checked_evaluator = runtime.find("EVALUATE-CHECKED")
    evaluator_finish = runtime.find("EVALUATE-FINISH")
    evaluator_line = runtime.find("EVAL-LINE")
    assert checked_evaluator is not None
    assert evaluator_finish is not None
    assert evaluator_line is not None
    checked_evaluator_xt = checked_evaluator.xt
    evaluator_finish_xt = evaluator_finish.xt
    # Keep the reusable physical-line buffer below the hosted dictionary
    # floor, away from its definitions and the high Bank-0 stack arenas.
    source_address = runtime.dictionary.start_address - 256
    assert source_address >= 0
    runtime.memory.fill(source_address, 256, 0)
    image = _formatted_image()
    runtime.storage.attach(image)
    media_before = runtime.storage.image_bytes

    for line_number, line in _submitted_lines(_verified_kdos()):
        _evaluate_checked_line(
            runtime,
            evaluator_xt=checked_evaluator_xt,
            line_cell=evaluator_line.body_address,
            source_address=source_address,
            line_number=line_number,
            source=line,
        )
    runtime.execute(evaluator_finish_xt)
    assert runtime.main_context.data.pop() == 0

    loaded_words = runtime.dictionary.words[len(core_words) :]
    assert len(core_words) == 319
    assert len(loaded_words) == 1_460
    assert loaded_words[0].name == b".R"
    assert tuple(word.name for word in loaded_words[-2:]) == (
        b"_AUTOEXEC-NAME",
        b"_AUTOEXEC-RUN",
    )
    assert runtime.dictionary.words[: len(core_words)] == core_words
    assert all(runtime.find(name) is not None for name in REPRESENTATIVE_WORDS)
    assert runtime.find(b"<interpret-if>") is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    index = runtime.dictionary_index.state
    assert index.flags == DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE
    assert index.slots == 65_536
    unique_bindings = {word.name.upper() for word in runtime.dictionary.words}
    assert len(runtime.dictionary.words) - len(unique_bindings) == 7
    assert index.count == len(unique_bindings)
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "HEAP-INIT") == 1
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "BUF-COUNT") == 6
    assert _variable(runtime, "KERN-COUNT") == 23
    assert _variable(runtime, "PIPE-COUNT") == 3
    assert _variable(runtime, "NSCREENS") == 9
    assert _execute(runtime, "HBW-FREE") == (CANONICAL_HBW_BYTES,)
    assert _execute(runtime, "XMEM-FREE")[0] > 0
    assert _execute(runtime, "HEAP-VERIFY") == (TRUE,)
    assert _execute(runtime, "_MOD-COUNT") == (0,)
    assert runtime.storage.completion == 6
    assert runtime.storage.image_bytes == media_before
    assert all(owner is None for owner in runtime.spinlocks.owners)
    assert runtime.drain_uart_output() == (
        STARTUP_BANNER + b" MP64FS loaded\r\n\r\n"
    )

    _evaluate_checked_line(
        runtime,
        evaluator_xt=checked_evaluator_xt,
        line_cell=evaluator_line.body_address,
        source_address=source_address,
        line_number=KDOS_LINES + 1,
        source=b": POST-BOOT 6 7 * ; POST-BOOT",
    )
    assert runtime.main_context.data.snapshot() == (42,)
    runtime.main_context.data.clear()
    runtime.execute(evaluator_finish_xt)
    assert runtime.main_context.data.pop() == 0

    address, status = _execute(runtime, "ALLOCATE", 37)
    assert status == 0
    runtime.memory.write_bytes(address, b"ordinary-kdos-allocation")
    assert runtime.memory.read_bytes(address, 24) == b"ordinary-kdos-allocation"
    assert _execute(runtime, "FREE", address) == ()

    crc_data = runtime.define_created(
        "REGULAR-LOAD-CRC-DATA",
        initial_body=b"123456789",
    )
    assert _execute(runtime, "CRC32-STR", crc_data.body_address, 9) == (
        0xFC89_1918,
    )

    runtime.evaluate(
        b"4 2 RING REGULAR-LOAD-RING",
        source_name="regular-load-ring.f",
    )
    ring = _constant(runtime, "REGULAR-LOAD-RING")
    source = runtime.define_created(
        "REGULAR-LOAD-RING-SOURCE",
        initial_body=b"KDOS",
    )
    destination = runtime.define_created(
        "REGULAR-LOAD-RING-DESTINATION",
        initial_body=bytes(4),
    )
    assert _execute(runtime, "RING-PUSH", source.body_address, ring) == (MASK64,)
    assert _execute(runtime, "RING-COUNT", ring) == (1,)
    assert _execute(runtime, "RING-POP", destination.body_address, ring) == (
        MASK64,
    )
    assert runtime.memory.read_bytes(destination.body_address, 4) == b"KDOS"
    assert _execute(runtime, "RING-EMPTY?", ring) == (MASK64,)
    assert _execute(runtime, "MODULES") == ()
    assert runtime.drain_uart_output() == (
        b" Loaded modules:\r\n0  module(s)\r\n"
    )
    assert all(owner is None for owner in runtime.spinlocks.owners)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
