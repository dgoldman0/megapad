"""Shared production-source oracles for the two MegaPad execution backends.

These tests deliberately stop before a complete ``rich-terminal.f`` load or
live terminal session.  They extract one contiguous pre-session prefix from
the authoritative module and execute the same source-defined APT encoder on
the exact emulator and hosted simulator.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass
from pathlib import Path

import pytest

from rich_terminal.apt1 import (
    Frame,
    MessageType,
    OpenRequest,
    Probe,
    encode_frame,
    encode_open,
    encode_probe,
    parse_negotiation,
)
from simulator.runtime import MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
BIOS_SOURCE = REPOSITORY_ROOT / "bios.asm"
RICH_TERMINAL_SOURCE = REPOSITORY_ROOT / "rich-terminal.f"

PREFIX_START = b"0 CONSTANT PT-S-OK"
PREFIX_END = b"\nVARIABLE _PT-CTL-REASON"

# These are watchdogs for one BIOS boot and roughly 1,170 lines of inert
# definitions, not broadened qualification budgets.  The accelerated emulator
# and semantic simulator normally finish far below them.
EMULATOR_BOOT_MAX_STEPS = 2_000_000
EMULATOR_SOURCE_MAX_STEPS = 40_000_000
EMULATOR_RUN_BATCH_STEPS = 100_000
SIMULATOR_SOURCE_MAX_STEPS = 1_000_000

FRAME_BEGIN = 30
FRAME_END = 31
STATUS_BEGIN = 29
STATUS_END = 28

SESSION_ID = 0x4142_4344_4546_4748
PRESENTATION_EPOCH = 9
LOCAL_GRANT = 0x1122_3344_5566_7788

CREDIT_SCENARIO_SOURCE = b"""
CREATE DBC-RX _PT-CONTROL-RESERVE _PT-HDR + 32 + ALLOT
CREATE DBC-TX _PT-OPEN-BYTES ALLOT
CREATE DBC-EVENT PT-EVENT-SIZE ALLOT
CREATE DBC-S-STORAGE PT-SESSION-SIZE 7 + ALLOT
: DBC-S  DBC-S-STORAGE 7 + -8 AND ;
VARIABLE DBC-INIT-S
VARIABLE DBC-CREDIT-S
: DBC-RUN
  DBC-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
  DBC-TX _PT-OPEN-BYTES DBC-EVENT PT-EVENT-SIZE DBC-S
  PT-INIT DBC-INIT-S !
  8 DBC-S _PT.S.PEER-MAX-PAY !
  0x4142434445464748 DBC-S _PT.S.SESSION-ID !
  9 DBC-S _PT.S.EPOCH !
  0x1122334455667788 DBC-S _PT.S.LOCAL-GRANT !
  30 EMIT TX-FLUSH
  DBC-S _PT-SEND-CREDIT DBC-CREDIT-S !
  31 EMIT TX-FLUSH
  29 EMIT
  DBC-INIT-S @ . DBC-CREDIT-S @ . DEPTH .
  28 EMIT TX-FLUSH ;
"""

FIXED_NONCE = 0x0102_0304_0506_0708
CLIENT_MAX_PAYLOAD = 32
CLIENT_RECEIVE_CREDIT = 72

NEGOTIATION_SCENARIO_SOURCE = b"""
CREATE DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 32 + ALLOT
CREATE DBN-TX _PT-OPEN-BYTES ALLOT
CREATE DBN-EVENT PT-EVENT-SIZE ALLOT
CREATE DBN-EXTRA 16 ALLOT
CREATE DBN-S-STORAGE PT-SESSION-SIZE 7 + ALLOT
: DBN-S  DBN-S-STORAGE 7 + -8 AND ;
: DBN-ARGS
  DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
  DBN-TX _PT-OPEN-BYTES DBN-EVENT PT-EVENT-SIZE ;
: DBN-RUN
  17 EMIT
  DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 31 +
    DBN-TX _PT-OPEN-BYTES DBN-EVENT PT-EVENT-SIZE DBN-S PT-INIT .
  DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
    DBN-TX _PT-OPEN-BYTES 1- DBN-EVENT PT-EVENT-SIZE DBN-S PT-INIT .
  DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
    DBN-TX _PT-OPEN-BYTES DBN-EVENT PT-EVENT-SIZE 1- DBN-S PT-INIT .
  DBN-ARGS DBN-S 1+ PT-INIT .
  DBN-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
    DBN-RX _PT-OPEN-BYTES DBN-EVENT PT-EVENT-SIZE DBN-S PT-INIT .
  DBN-ARGS DBN-S PT-INIT .
  DBN-EXTRA 16 DBN-S PT-STORAGE-DISJOINT? .
  DBN-RX 1 DBN-S PT-STORAGE-DISJOINT? .
  DEPTH .
  18 EMIT TX-FLUSH
  0x0102030405060708 DBN-S _PT.S.NONCE !
  0x4142434445464748 DBN-S _PT.S.SESSION-ID !
  32 DBN-S _PT.S.CLIENT-MAX-PAY !
  72 DBN-S _PT.S.LOCAL-GRANT !
  19 EMIT TX-FLUSH
  DBN-S _PT-SEND-PROBE
  DBN-S _PT-SEND-OPEN
  20 EMIT TX-FLUSH
  21 EMIT
  DBN-S PT-START .
  DBN-S PT-STATE@ . DBN-S PT-OWNS? . DEPTH .
  22 EMIT TX-FLUSH ;
"""

# The production encoder correctly takes KDOS's global UART lock.  This
# focused fixture intentionally does not load KDOS, so both backends receive
# the same narrow definitions over BIOS SPIN@/SPIN!.  This retains the real
# uncontended lock boundary without importing KDOS's scheduler-aware LOCK
# wrappers.  The shims are not part of the extracted production prefix and
# make no multicore or full-module claim.
ONE_CORE_UART_LOCK_SHIMS = b"""
: UART-ACQUIRE  BEGIN 1 SPIN@ 0= UNTIL ;
: UART-RELEASE  1 SPIN! ;
"""


@dataclass(frozen=True, slots=True)
class CreditObservation:
    frame: bytes
    status: bytes


@dataclass(frozen=True, slots=True)
class SourceCase:
    source_name: str
    harness: bytes
    entry_word: bytes


@dataclass(frozen=True, slots=True)
class SourceRun:
    output: bytes
    steps: int


CREDIT_CASE = SourceCase(
    source_name="dual-backend-credit.f",
    harness=CREDIT_SCENARIO_SOURCE,
    entry_word=b"DBC-RUN",
)

NEGOTIATION_CASE = SourceCase(
    source_name="dual-backend-negotiation.f",
    harness=NEGOTIATION_SCENARIO_SOURCE,
    entry_word=b"DBN-RUN",
)


def _rich_terminal_credit_prefix() -> bytes:
    """Extract the exact current module prefix through ``_PT-SEND-CREDIT``."""

    source = RICH_TERMINAL_SOURCE.read_bytes()
    assert source.count(PREFIX_START) == 1
    assert source.count(PREFIX_END) == 1
    start = source.index(PREFIX_START)
    end = source.index(PREFIX_END, start)
    prefix = source[start:end]
    assert prefix.endswith(b"FALSE _PT-F-S @ _PT-FRAME-SEND ;\n")
    return prefix


def _between_unique(raw: bytes, start: int, end: int) -> bytes:
    start_marker = bytes((start,))
    end_marker = bytes((end,))
    assert raw.count(start_marker) == 1, (
        f"expected one {start:#x} output marker, got {raw.count(start_marker)}"
    )
    assert raw.count(end_marker) == 1, (
        f"expected one {end:#x} output marker, got {raw.count(end_marker)}"
    )
    start_at = raw.index(start_marker)
    end_at = raw.index(end_marker, start_at + 1)
    return raw[start_at + 1 : end_at]


def _observe(raw: bytes) -> CreditObservation:
    return CreditObservation(
        frame=_between_unique(raw, FRAME_BEGIN, FRAME_END),
        status=_between_unique(raw, STATUS_BEGIN, STATUS_END),
    )


def _run_simulator(prefix: bytes, case: SourceCase) -> SourceRun:
    runtime = MegaForthRuntime()
    source_result = runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + prefix,
        source_name="rich-terminal.f:PT-S-OK.._PT-SEND-CREDIT",
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    harness_result = runtime.evaluate(case.harness, source_name=case.source_name)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""

    execution_result = runtime.execute(case.entry_word)

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return SourceRun(
        output=runtime.drain_uart_output(),
        steps=(
            source_result.semantic_steps
            + harness_result.semantic_steps
            + execution_result.semantic_steps
        ),
    )


def _boot_emulator():
    # Keep exact-machine imports out of the simulator-only collection path so
    # its fast target neither builds nor requires the C++ accelerator.
    from asm import assemble
    from emulator.system import MegapadSystem

    code = assemble(BIOS_SOURCE.read_text(encoding="utf-8"))
    system = MegapadSystem(
        ram_size=1 << 20,
        storage_image=None,
        num_cores=1,
        ext_mem_size=0,
    )
    output: list[int] = []
    system.uart.on_tx = output.append
    system.load_binary(0, code)
    system.boot()

    steps = 0
    while steps < EMULATOR_BOOT_MAX_STEPS:
        if system.cpu.halted or system.cpu.idle:
            break
        executed = system.run_batch(
            min(EMULATOR_RUN_BATCH_STEPS, EMULATOR_BOOT_MAX_STEPS - steps)
        )
        steps += max(executed, 1)

    assert system.cpu.idle, "BIOS did not reach its input wait"
    assert not system.cpu.halted
    output.clear()
    return system, output


def _run_emulator(prefix: bytes, case: SourceCase) -> SourceRun:
    system, output = _boot_emulator()
    payload = (
        ONE_CORE_UART_LOCK_SHIMS
        + prefix
        + b"\n"
        + case.harness
        + case.entry_word
        + b"\nBYE\n"
    )
    position = 0
    steps = 0

    while steps < EMULATOR_SOURCE_MAX_STEPS:
        if system.cpu.halted:
            break
        if system.cpu.idle and not system.uart.has_rx_data:
            if position >= len(payload):
                break
            newline = payload.find(b"\n", position)
            limit = len(payload) if newline < 0 else newline + 1
            system.uart.inject_input(payload[position:limit])
            position = limit
            continue
        executed = system.run_batch(
            min(EMULATOR_RUN_BATCH_STEPS, EMULATOR_SOURCE_MAX_STEPS - steps)
        )
        steps += max(executed, 1)

    raw = bytes(output)
    diagnostic = raw.decode("utf-8", errors="replace")
    assert position == len(payload), "production prefix was not fully fed"
    assert system.cpu.halted, (
        "production-prefix oracle exceeded its source watchdog; " + diagnostic[-2000:]
    )
    for rejected in (
        " ? (not found)",
        "Dictionary full",
        "dictionary overflow",
        "Stack underflow",
        "Stack overflow",
        "Return stack overflow",
        "nested definition",
        "*** BUS FAULT",
        "*** PRIVILEGE FAULT",
    ):
        assert rejected not in diagnostic
    return SourceRun(output=raw, steps=steps)


@pytest.mark.parametrize(
    "run_backend",
    (_run_simulator, _run_emulator),
    ids=("simulator", "emulator"),
)
def test_production_credit_encoder_matches_wire_oracle(run_backend) -> None:
    prefix = _rich_terminal_credit_prefix()
    observation = _observe(run_backend(prefix, CREDIT_CASE).output)
    expected = CreditObservation(
        frame=encode_frame(
            Frame(
                MessageType.CREDIT,
                SESSION_ID,
                0,
                PRESENTATION_EPOCH,
                struct.pack("<Q", LOCAL_GRANT),
            ),
            max_payload=8,
        ),
        status=b"0 0 0 ",
    )

    assert observation == expected


@pytest.mark.parametrize(
    "run_backend",
    (_run_simulator, _run_emulator),
    ids=("simulator", "emulator"),
)
def test_production_initialization_and_negotiation_start_match_oracle(
    run_backend,
) -> None:
    prefix = _rich_terminal_credit_prefix()
    output = run_backend(prefix, NEGOTIATION_CASE).output

    initialization = _between_unique(output, 17, 18)
    fixed_records = _between_unique(output, 19, 20)
    public_start = _between_unique(output, 21, 22)

    assert initialization == b"3 3 3 3 3 0 -1 0 0 "
    assert fixed_records == (
        encode_probe(FIXED_NONCE)
        + encode_open(
            OpenRequest(
                nonce=FIXED_NONCE,
                session_id=SESSION_ID,
                client_max_payload=CLIENT_MAX_PAYLOAD,
                client_receive_credit=CLIENT_RECEIVE_CREDIT,
            )
        )
    )

    dynamic_probe = public_start[:38]
    parsed = parse_negotiation(dynamic_probe)
    assert isinstance(parsed, Probe)
    assert parsed.nonce != 0
    assert public_start[38:] == b"0 1 -1 0 "
