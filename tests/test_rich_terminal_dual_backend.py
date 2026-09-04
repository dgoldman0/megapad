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
    Offer,
    Probe,
    encode_frame,
    encode_offer,
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
UART_OFFER_PREFIX_END = (
    b"\n\\ ====================================================================="
    b"\n\\  Input payload validation"
)

# These are watchdogs for one BIOS boot and at most roughly 1,320 lines of
# inert definitions, not broadened qualification budgets.  The accelerated
# emulator and semantic simulator normally finish far below them.
EMULATOR_BOOT_MAX_STEPS = 2_000_000
EMULATOR_SOURCE_MAX_STEPS = 40_000_000
EMULATOR_RUN_BATCH_STEPS = 100_000
SIMULATOR_SOURCE_MAX_STEPS = 1_000_000

FRAME_BEGIN = 30
FRAME_END = 31
STATUS_BEGIN = 29
STATUS_END = 28

OPENING_STATE_BEGIN = 11
OPENING_STATE_END = 12
OPENING_PROBE_BEGIN = 23
OPENING_PROBE_END = 24
OPENING_OPEN_BEGIN = 25
OPENING_OPEN_END = 26

SESSION_ID = 0x4142_4344_4546_4748
PRESENTATION_EPOCH = 9
LOCAL_GRANT = 0x1122_3344_5566_7788

OFFER_MAX_PAYLOAD = 32
OFFER_MAX_TRANSACTION = 244
OFFER_RECEIVE_CREDIT = 244
OFFER_COLS = 2
OFFER_ROWS = 1
OPEN_CLIENT_MAX_PAYLOAD = 32
OPEN_CLIENT_RECEIVE_CREDIT = 72
OPEN_MAX_TEXT = 20

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

UART_OFFER_SCENARIO_SOURCE = b"""
CREATE DBR-RX _PT-CONTROL-RESERVE _PT-HDR + 32 + ALLOT
CREATE DBR-TX _PT-OPEN-BYTES ALLOT
CREATE DBR-EVENT PT-EVENT-SIZE ALLOT
CREATE DBR-S-STORAGE PT-SESSION-SIZE 7 + ALLOT
: DBR-S  DBR-S-STORAGE 7 + -8 AND ;
VARIABLE DBR-INIT-S
VARIABLE DBR-START-S
VARIABLE DBR-OFFER?
: DBR-INITIALIZE
  DBR-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
  DBR-TX _PT-OPEN-BYTES DBR-EVENT PT-EVENT-SIZE DBR-S
  PT-INIT DBR-INIT-S ! ;
: DBR-START
  23 EMIT TX-FLUSH
  DBR-S PT-START DBR-START-S !
  24 EMIT TX-FLUSH ;
: DBR-ACCEPT
  25 EMIT TX-FLUSH
  BEGIN DBR-S _PT-READ-BYTE WHILE REPEAT
  DBR-S _PT-SCAN-OFFER IF
    DBR-S _PT-ACCEPT-OFFER TRUE
  ELSE
    DROP FALSE
  THEN DBR-OFFER? !
  26 EMIT TX-FLUSH ;
: DBR-REPORT
  11 EMIT
  DBR-INIT-S @ . DBR-START-S @ . DBR-OFFER? @ .
  DBR-S PT-STATE@ . PT-STREAM-OWNED? . DBR-S PT-OWNS? .
  DBR-S _PT.S.SESSION-ID @ U.
  DBR-S _PT.S.PEER-MAX-PAY @ . DBR-S _PT.S.PEER-MAX-TX @ .
  DBR-S _PT.S.PEER-GRANT @ . DBR-S _PT.S.PEER-INITIAL @ .
  DBR-S _PT.S.COLS @ . DBR-S _PT.S.ROWS @ .
  DBR-S _PT.S.CLIENT-MAX-PAY @ . DBR-S _PT.S.LOCAL-GRANT @ .
  DBR-S _PT.S.MAX-TEXT @ . DBR-S _PT.S.SNAPSHOT? @ .
  DBR-S _PT.S.PROBES @ . DBR-S _PT.S.TX-SEQ @ .
  DBR-S _PT.S.RX-SEQ @ . DBR-S _PT.S.BIN-U @ .
  DBR-S _PT.S.LEGACY-U @ . DEPTH .
  12 EMIT TX-FLUSH ;
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


@dataclass(frozen=True, slots=True)
class OpeningObservation:
    probe: bytes
    open_request: bytes
    state: bytes


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


def _rich_terminal_uart_offer_prefix() -> bytes:
    """Extract the exact current module prefix through ``_PT-READ-BYTE``."""

    source = RICH_TERMINAL_SOURCE.read_bytes()
    assert source.count(PREFIX_START) == 1
    assert source.count(UART_OFFER_PREFIX_END) == 1
    start = source.index(PREFIX_START)
    end = source.index(UART_OFFER_PREFIX_END, start)
    prefix = source[start:end]
    assert prefix.endswith(
        b"_PT-RD-S @ _PT.S.BIN-U @ 1+ _PT-RD-S @ _PT.S.BIN-U !\n"
        b"    TRUE ;\n"
    )
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


def _offer_for_probe(probe_record: bytes) -> bytes:
    parsed = parse_negotiation(probe_record)
    assert isinstance(parsed, Probe)
    assert parsed.nonce != 0
    assert probe_record == encode_probe(parsed.nonce)
    return encode_offer(
        Offer(
            nonce=parsed.nonce,
            session_id=SESSION_ID,
            max_payload=OFFER_MAX_PAYLOAD,
            max_transaction=OFFER_MAX_TRANSACTION,
            terminal_receive_credit=OFFER_RECEIVE_CREDIT,
            cols=OFFER_COLS,
            rows=OFFER_ROWS,
        )
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


def _run_uart_offer_simulator(prefix: bytes) -> OpeningObservation:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + prefix,
        source_name="rich-terminal.f:PT-S-OK.._PT-READ-BYTE",
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    runtime.evaluate(
        UART_OFFER_SCENARIO_SOURCE,
        source_name="dual-backend-uart-offer.f",
    )
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""

    runtime.execute("DBR-INITIALIZE")
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""

    runtime.execute("DBR-START")
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    probe = _between_unique(
        runtime.drain_uart_output(),
        OPENING_PROBE_BEGIN,
        OPENING_PROBE_END,
    )

    runtime.inject_uart_input(_offer_for_probe(probe))
    runtime.execute("DBR-ACCEPT")
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.uart_input == b""
    open_request = _between_unique(
        runtime.drain_uart_output(),
        OPENING_OPEN_BEGIN,
        OPENING_OPEN_END,
    )

    runtime.execute("DBR-REPORT")
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    state = _between_unique(
        runtime.drain_uart_output(),
        OPENING_STATE_BEGIN,
        OPENING_STATE_END,
    )
    return OpeningObservation(probe=probe, open_request=open_request, state=state)


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


def _assert_no_emulator_diagnostics(raw: bytes) -> None:
    diagnostic = raw.decode("utf-8", errors="replace")
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


def _feed_emulator_lines(system, payload: bytes) -> int:
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

    assert position == len(payload), "production prefix was not fully fed"
    assert system.cpu.idle, "production prefix did not return to BIOS input wait"
    assert not system.cpu.halted
    assert not system.uart.has_rx_data
    return steps


def _run_emulator_input_to_idle(system, payload: bytes, *, phase: str) -> int:
    assert system.cpu.idle
    assert not system.cpu.halted
    assert not system.uart.has_rx_data
    system.uart.inject_input(payload)

    steps = 0
    while steps < EMULATOR_BOOT_MAX_STEPS:
        if system.cpu.halted or (
            system.cpu.idle and not system.uart.has_rx_data
        ):
            break
        executed = system.run_batch(
            min(EMULATOR_RUN_BATCH_STEPS, EMULATOR_BOOT_MAX_STEPS - steps)
        )
        steps += max(executed, 1)

    assert not system.cpu.halted, f"emulator halted during {phase}"
    assert system.cpu.idle, f"emulator did not return to input wait during {phase}"
    assert not system.uart.has_rx_data, f"emulator left unread input during {phase}"
    return steps


def _halt_emulator(system) -> None:
    assert system.cpu.idle
    assert not system.uart.has_rx_data
    system.uart.inject_input(b"BYE\n")
    steps = 0
    while steps < EMULATOR_BOOT_MAX_STEPS and not system.cpu.halted:
        executed = system.run_batch(
            min(EMULATOR_RUN_BATCH_STEPS, EMULATOR_BOOT_MAX_STEPS - steps)
        )
        steps += max(executed, 1)
    assert system.cpu.halted, "BIOS did not halt after the phased oracle"


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


def _run_uart_offer_emulator(prefix: bytes) -> OpeningObservation:
    system, output = _boot_emulator()
    _feed_emulator_lines(
        system,
        ONE_CORE_UART_LOCK_SHIMS
        + prefix
        + b"\n"
        + UART_OFFER_SCENARIO_SOURCE,
    )
    _assert_no_emulator_diagnostics(bytes(output))
    output.clear()
    idle_stacks = tuple(system.cpu.regs[14:16])

    _run_emulator_input_to_idle(
        system,
        b"DBR-INITIALIZE\n",
        phase="rich-terminal initialization",
    )
    _assert_no_emulator_diagnostics(bytes(output))
    assert tuple(system.cpu.regs[14:16]) == idle_stacks
    output.clear()

    _run_emulator_input_to_idle(
        system,
        b"DBR-START\n",
        phase="rich-terminal start",
    )
    start_output = bytes(output)
    _assert_no_emulator_diagnostics(start_output)
    assert tuple(system.cpu.regs[14:16]) == idle_stacks
    probe = _between_unique(
        start_output,
        OPENING_PROBE_BEGIN,
        OPENING_PROBE_END,
    )
    output.clear()

    offer = _offer_for_probe(probe)
    _run_emulator_input_to_idle(
        system,
        b"DBR-ACCEPT\n" + offer,
        phase="rich-terminal OFFER acceptance",
    )
    accept_output = bytes(output)
    _assert_no_emulator_diagnostics(accept_output)
    assert tuple(system.cpu.regs[14:16]) == idle_stacks
    open_request = _between_unique(
        accept_output,
        OPENING_OPEN_BEGIN,
        OPENING_OPEN_END,
    )
    output.clear()

    _run_emulator_input_to_idle(
        system,
        b"DBR-REPORT\n",
        phase="rich-terminal opening report",
    )
    report_output = bytes(output)
    _assert_no_emulator_diagnostics(report_output)
    assert tuple(system.cpu.regs[14:16]) == idle_stacks
    state = _between_unique(
        report_output,
        OPENING_STATE_BEGIN,
        OPENING_STATE_END,
    )
    output.clear()

    _halt_emulator(system)
    _assert_no_emulator_diagnostics(bytes(output))
    return OpeningObservation(probe=probe, open_request=open_request, state=state)


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


@pytest.mark.parametrize(
    "run_backend",
    (_run_uart_offer_simulator, _run_uart_offer_emulator),
    ids=("simulator", "emulator"),
)
def test_production_uart_offer_reaches_opening_with_exact_open_request(
    run_backend,
) -> None:
    prefix = _rich_terminal_uart_offer_prefix()
    observation = run_backend(prefix)

    assert len(observation.probe) == 38
    parsed_probe = parse_negotiation(observation.probe)
    assert isinstance(parsed_probe, Probe)
    assert parsed_probe.nonce != 0
    assert observation.probe == encode_probe(parsed_probe.nonce)

    expected_open = OpenRequest(
        nonce=parsed_probe.nonce,
        session_id=SESSION_ID,
        client_max_payload=OPEN_CLIENT_MAX_PAYLOAD,
        client_receive_credit=OPEN_CLIENT_RECEIVE_CREDIT,
    )
    assert observation.open_request == encode_open(expected_open)
    assert parse_negotiation(observation.open_request) == expected_open

    expected_state = (
        0,
        0,
        -1,
        2,
        -1,
        -1,
        SESSION_ID,
        OFFER_MAX_PAYLOAD,
        OFFER_MAX_TRANSACTION,
        OFFER_RECEIVE_CREDIT,
        OFFER_RECEIVE_CREDIT,
        OFFER_COLS,
        OFFER_ROWS,
        OPEN_CLIENT_MAX_PAYLOAD,
        OPEN_CLIENT_RECEIVE_CREDIT,
        OPEN_MAX_TEXT,
        -1,
        1,
        0,
        0,
        0,
        0,
        0,
    )
    assert observation.state == (
        b" ".join(str(value).encode("ascii") for value in expected_state) + b" "
    )
