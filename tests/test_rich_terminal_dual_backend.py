"""Shared production-source oracles for the two MegaPad execution backends.

These tests extract contiguous prefixes from the authoritative module and
execute its source-defined APT path on the exact emulator and hosted
simulator. The shared byte oracles remain paired across both backends; the
simulator selector additionally proves the first real driver handshake and
synchronized close through the unchanged pre-``CATCH`` boundary without
claiming a complete module load. A separate compile-and-invalid-call oracle
uses the real KDOS exception closure to cross ``CATCH`` through the resource
entry points, stopping before CELL/PRESENT transaction construction.
"""

from __future__ import annotations

import hashlib
import struct
from dataclasses import dataclass
from pathlib import Path

import pytest

from shared.cells import MASK64
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
from rich_terminal.driver import DriverLimits, DriverStatus, RichTerminalDriver
from rich_terminal.server import TerminalConfig, TerminalState
from rich_terminal.transport import (
    AdmissionStatus,
    EgressWatermarks,
    HostPortLimits,
)
from simulator.rich_terminal_host import (
    HostedTerminalGeometry,
    SemanticBatchStop,
    SimulatorSessionBackend,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions


REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
BIOS_SOURCE = REPOSITORY_ROOT / "bios.asm"
RICH_TERMINAL_SOURCE = REPOSITORY_ROOT / "rich-terminal.f"

PREFIX_START = b"0 CONSTANT PT-S-OK"
PREFIX_END = b"\nVARIABLE _PT-CTL-REASON"
UART_OFFER_PREFIX_END = (
    b"\n\\ ====================================================================="
    b"\n\\  Input payload validation"
)
PRE_CATCH_PREFIX_END = (
    b"\n\\ Stack: owner generation resource format width height flags byte-length"
)
RESOURCE_WRAPPER_PREFIX_END = (
    b"\n\\ ====================================================================="
    b"\n\\  Shared CELL-1/PRESENT transaction builder"
)

# These are watchdogs for one BIOS boot and the selected contiguous definition
# prefixes, not broadened qualification budgets. The accelerated emulator and
# semantic simulator normally finish far below them.
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

LIVE_HANDSHAKE_SCENARIO_SOURCE = b"""
CREATE DBL-RX _PT-CONTROL-RESERVE _PT-HDR + 32 + ALLOT
CREATE DBL-TX _PT-OPEN-BYTES ALLOT
CREATE DBL-EVENT PT-EVENT-SIZE ALLOT
CREATE DBL-S-STORAGE PT-SESSION-SIZE 7 + ALLOT
CREATE DBL-POLL-EVENT PT-EVENT-SIZE ALLOT
CREATE DBL-POLL-COMPLETION PT-COMPLETION-SIZE ALLOT
: DBL-S  DBL-S-STORAGE 7 + -8 AND ;
VARIABLE DBL-INIT-S
VARIABLE DBL-START-S
VARIABLE DBL-SERVICE-S
VARIABLE DBL-STATE
VARIABLE DBL-ACTIVE
VARIABLE DBL-OWNS
VARIABLE DBL-SESSION-ID
VARIABLE DBL-PEER-MAX-PAY
VARIABLE DBL-PEER-MAX-TX
VARIABLE DBL-PEER-GRANT
VARIABLE DBL-COLS
VARIABLE DBL-ROWS
VARIABLE DBL-CLIENT-MAX-PAY
VARIABLE DBL-LOCAL-GRANT
VARIABLE DBL-MAX-TEXT
VARIABLE DBL-TX-SEQ
VARIABLE DBL-RX-SEQ
VARIABLE DBL-POLL-EVENT-S
VARIABLE DBL-POLL-EVENT?
VARIABLE DBL-POLL-COMPLETION-S
VARIABLE DBL-POLL-COMPLETION?
VARIABLE DBL-CLOSE-S
: DBL-BOOT
  DBL-RX _PT-CONTROL-RESERVE _PT-HDR + 32 +
  DBL-TX _PT-OPEN-BYTES DBL-EVENT PT-EVENT-SIZE DBL-S
  PT-INIT DUP DBL-INIT-S ! IF EXIT THEN
  DBL-S PT-START DBL-START-S !
  DBL-S PT-STATE@ DBL-STATE ! ;
: DBL-SERVICE
  DBL-S PT-SERVICE DBL-SERVICE-S !
  DBL-S PT-STATE@ DBL-STATE !
  DBL-S PT-ACTIVE? DBL-ACTIVE !
  DBL-S PT-OWNS? DBL-OWNS !
  DBL-S _PT.S.SESSION-ID @ DBL-SESSION-ID !
  DBL-S _PT.S.PEER-MAX-PAY @ DBL-PEER-MAX-PAY !
  DBL-S _PT.S.PEER-MAX-TX @ DBL-PEER-MAX-TX !
  DBL-S _PT.S.PEER-GRANT @ DBL-PEER-GRANT !
  DBL-S _PT.S.COLS @ DBL-COLS !
  DBL-S _PT.S.ROWS @ DBL-ROWS !
  DBL-S _PT.S.CLIENT-MAX-PAY @ DBL-CLIENT-MAX-PAY !
  DBL-S _PT.S.LOCAL-GRANT @ DBL-LOCAL-GRANT !
  DBL-S _PT.S.MAX-TEXT @ DBL-MAX-TEXT !
  DBL-S _PT.S.TX-SEQ @ DBL-TX-SEQ !
  DBL-S _PT.S.RX-SEQ @ DBL-RX-SEQ ! ;
: DBL-POLL-EMPTY
  DBL-POLL-EVENT DBL-S PT-EVENT-POLL
  DBL-POLL-EVENT? ! DBL-POLL-EVENT-S !
  DBL-POLL-COMPLETION DBL-S PT-COMPLETION-POLL
  DBL-POLL-COMPLETION? ! DBL-POLL-COMPLETION-S ! ;
: DBL-CLOSE
  7 DBL-S PT-CLOSE DBL-CLOSE-S !
  DBL-S PT-STATE@ DBL-STATE !
  DBL-S PT-ACTIVE? DBL-ACTIVE !
  DBL-S PT-OWNS? DBL-OWNS !
  DBL-S _PT.S.TX-SEQ @ DBL-TX-SEQ !
  DBL-S _PT.S.RX-SEQ @ DBL-RX-SEQ ! ;
"""

RESOURCE_WRAPPER_SCENARIO_SOURCE = b"""
VARIABLE DBX-BEGIN-S
VARIABLE DBX-CHUNK-S
VARIABLE DBX-COMMIT-S
VARIABLE DBX-DROP-S
VARIABLE DBX-ABORT-S
: DBX-POISON-RANGES
  -1 _PT-RSA ! -1 _PT-RSU ! -1 _PT-RSS !
  -1 _PT-RA ! -1 _PT-RU ! -1 _PT-RB ! -1 _PT-RV ! ;
: DBX-BEGIN
  -1 _PT-RBG-PIXELS ! DBX-POISON-RANGES
  1 2 3 4 5 6 7 8 9 10 11 PT-RESOURCE-BEGIN DBX-BEGIN-S ! ;
: DBX-CHUNK
  -1 _PT-RCH-PAYLOAD-U ! -1 _PT-RCH-FRAME-U ! -1 _PT-RCH-END !
  -1 _PT-RCH-SENT ! -1 _PT-RCH-WATERMARK ! DBX-POISON-RANGES
  1 2 3 4 5 6 7 PT-RESOURCE-CHUNK DBX-CHUNK-S ! ;
: DBX-OTHER
  1 2 3 5 PT-RESOURCE-COMMIT DBX-COMMIT-S !
  1 2 3 5 PT-RESOURCE-DROP DBX-DROP-S !
  1 2 3 0 5 PT-RESOURCE-ABORT DBX-ABORT-S ! ;
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


def _rich_terminal_pre_catch_prefix() -> bytes:
    """Extract the exact module prefix to the first missing bare dependency."""

    source = RICH_TERMINAL_SOURCE.read_bytes()
    assert source.count(PREFIX_START) == 1
    assert source.count(PRE_CATCH_PREFIX_END) == 1
    start = source.index(PREFIX_START)
    end = source.index(PRE_CATCH_PREFIX_END, start)
    prefix = source[start:end]
    assert prefix.endswith(
        b"    0 _PT-RBG-PIXELS !\n"
        b"    0 _PT-RSA ! 0 _PT-RSU ! 0 _PT-RSS !\n"
        b"    0 _PT-RA ! 0 _PT-RU ! 0 _PT-RB ! 0 _PT-RV ! ;\n"
    )
    return prefix


def _rich_terminal_resource_wrapper_prefix() -> bytes:
    """Extract through resource wrappers, before transaction construction."""

    source = RICH_TERMINAL_SOURCE.read_bytes()
    assert source.count(PREFIX_START) == 1
    assert source.count(RESOURCE_WRAPPER_PREFIX_END) == 1
    start = source.index(PREFIX_START)
    end = source.index(RESOURCE_WRAPPER_PREFIX_END, start)
    prefix = source[start:end]
    assert prefix.endswith(
        b"    _PT-RAB-REASON @ _PT-RAB-S @ _PT-RESOURCE-ABORT-TRACKED ;\n"
    )
    return prefix


def _stored_cell(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return runtime.memory.read64(word.body_address)


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


def test_simulator_real_kdos_catch_loads_resource_wrappers_before_transactions(
) -> None:
    prefix = _rich_terminal_resource_wrapper_prefix()
    prefix_digest = hashlib.sha256(prefix).hexdigest()
    runtime = _load_exceptions()
    catch_word = runtime.find("CATCH")
    assert catch_word is not None
    result = runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + prefix,
        source_name=(
            "one-core-uart-lock-shims+"
            f"rich-terminal.f:{prefix_digest}:PT-S-OK..pre-transaction"
        ),
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    runtime.evaluate(
        RESOURCE_WRAPPER_SCENARIO_SOURCE,
        source_name="dual-backend-resource-wrapper-boundary.f",
    )

    assert runtime.find("CATCH") is catch_word
    assert runtime.find("THROW") is not None
    assert all(
        runtime.find(name) is not None
        for name in (
            "PT-RESOURCE-BEGIN",
            "PT-RESOURCE-CHUNK",
            "PT-RESOURCE-COMMIT",
            "PT-RESOURCE-DROP",
            "PT-RESOURCE-ABORT",
        )
    )
    assert result.definitions[-1].name == b"PT-RESOURCE-ABORT"
    assert runtime.find("_PT-BEGIN-ARGS?") is None
    assert runtime.find("PT-TX-BEGIN") is None
    assert runtime.find("PT-SNAPSHOT-BEGIN") is None
    task_handlers = runtime.find("_TASK-HANDLERS")
    assert task_handlers is not None

    runtime.execute("DBX-BEGIN")
    assert _stored_cell(runtime, "DBX-BEGIN-S") == 3
    assert runtime.memory.read64(task_handlers.body_address) == 0
    assert all(
        _stored_cell(runtime, name) == 0
        for name in (
            "_PT-RBG-S",
            "_PT-RBG-OWNER",
            "_PT-RBG-GENERATION",
            "_PT-RBG-ITEM",
            "_PT-RBG-FORMAT",
            "_PT-RBG-WIDTH",
            "_PT-RBG-HEIGHT",
            "_PT-RBG-FLAGS",
            "_PT-RBG-LENGTH",
            "_PT-RBG-DIGEST-A",
            "_PT-RBG-DIGEST-U",
            "_PT-RBG-PIXELS",
            "_PT-RSA",
            "_PT-RSU",
            "_PT-RSS",
            "_PT-RA",
            "_PT-RU",
            "_PT-RB",
            "_PT-RV",
        )
    )

    runtime.execute("DBX-CHUNK")
    assert _stored_cell(runtime, "DBX-CHUNK-S") == 3
    assert runtime.memory.read64(task_handlers.body_address) == 0
    assert all(
        _stored_cell(runtime, name) == 0
        for name in (
            "_PT-RCH-S",
            "_PT-RCH-OWNER",
            "_PT-RCH-GENERATION",
            "_PT-RCH-ITEM",
            "_PT-RCH-OFFSET",
            "_PT-RCH-DATA-A",
            "_PT-RCH-DATA-U",
            "_PT-RCH-PAYLOAD-U",
            "_PT-RCH-FRAME-U",
            "_PT-RCH-END",
            "_PT-RCH-SENT",
            "_PT-RCH-WATERMARK",
            "_PT-RSA",
            "_PT-RSU",
            "_PT-RSS",
            "_PT-RA",
            "_PT-RU",
            "_PT-RB",
            "_PT-RV",
        )
    )

    runtime.execute("DBX-OTHER")
    assert tuple(
        _stored_cell(runtime, name)
        for name in ("DBX-COMMIT-S", "DBX-DROP-S", "DBX-ABORT-S")
    ) == (3, 3, 3)
    assert runtime.memory.read64(task_handlers.body_address) == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_simulator_real_driver_handshake_and_close_reach_pre_catch_frontier(
    request: pytest.FixtureRequest,
) -> None:
    prefix = _rich_terminal_pre_catch_prefix()
    prefix_digest = hashlib.sha256(prefix).hexdigest()
    runtime = MegaForthRuntime()
    runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + prefix,
        source_name=(
            "one-core-uart-lock-shims+"
            f"rich-terminal.f:{prefix_digest}:PT-S-OK..pre-CATCH"
        ),
        step_budget=SIMULATOR_SOURCE_MAX_STEPS,
    )
    runtime.evaluate(
        LIVE_HANDSHAKE_SCENARIO_SOURCE,
        source_name="dual-backend-live-handshake.f",
    )

    assert runtime.find("PT-SERVICE") is not None
    assert all(
        runtime.find(name) is not None
        for name in (
            "PT-CLOSE",
            "PT-EVENT-POLL",
            "PT-COMPLETION-POLL",
            "PT-OWNER-OPEN",
            "PT-OWNER-DROP",
        )
    )
    assert runtime.find("_PT-RESOURCE-BEGIN-SCRUB") is not None
    assert runtime.find("PT-RESOURCE-BEGIN") is None
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    host_limits = HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=8_192,
            low_bytes=1_024,
            high_batches=16,
            low_batches=2,
        ),
        retained_publication_bytes=4_608,
        ingress_bytes=8_192,
        ingress_events=16,
        ingress_control_bytes=4_096,
        ingress_control_events=8,
        geometry_events=2,
    )
    terminal_config = TerminalConfig(
        max_payload=256,
        max_transaction_bytes=512,
        terminal_receive_credit=1_024,
        max_cells=4,
        max_feed_bytes=4_608,
        max_cols=4,
        max_rows=2,
        cols=2,
        rows=2,
    )
    legacy_output: list[bytes] = []
    ansi_output: list[bytes] = []
    views = []
    backend = SimulatorSessionBackend(
        runtime,
        legacy_output_sink=legacy_output.append,
    )
    request.addfinalizer(backend.close)
    driver = RichTerminalDriver.attach(
        backend,
        host_limits,
        terminal_config,
        DriverLimits(4_096, 8),
        ansi_sink=ansi_output.append,
        view_sink=views.append,
        session_id_factory=lambda: SESSION_ID,
    )

    def close_driver() -> None:
        assert driver.close() is AdmissionStatus.ACCEPTED

    request.addfinalizer(close_driver)

    assert driver.core.state is TerminalState.ANSI
    assert backend.rich_terminal_host.pending_geometry_events == 1
    boot = backend.run_semantic_batch(entry="DBL-BOOT")
    assert boot.stop_reason is SemanticBatchStop.COMPLETED
    assert boot.semantic_steps > 0
    assert boot.external_events_applied == 1
    assert _stored_cell(runtime, "DBL-INIT-S") == 0
    assert _stored_cell(runtime, "DBL-START-S") == 0
    assert _stored_cell(runtime, "DBL-STATE") == 1
    assert driver.core.state is TerminalState.ANSI
    assert not backend.suspended
    assert backend.rich_terminal_host.pending_geometry_events == 0
    assert backend.geometry == HostedTerminalGeometry(cols=2, rows=2, resized=True)
    assert backend.rich_terminal_host.accepted_egress_bytes == 38
    assert backend.rich_terminal_host.accepted_egress_batches == 1

    probe = driver.service()
    assert probe.status is DriverStatus.PROGRESS
    assert (
        probe.machine_batches,
        probe.outbound_records,
        probe.ansi_bytes,
        probe.views,
    ) == (1, 1, 0, 0)
    assert driver.core.state is TerminalState.PROBING
    assert driver.core.machine_publications_received == 1
    assert driver.core.machine_publication_bytes_received == 38
    assert driver.core.frames_received == 0
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0
    assert backend.rich_terminal_host.pending_ingress_bytes == 92
    assert backend.rich_terminal_host.pending_ingress_events == 1

    opening = backend.run_semantic_batch(entry="DBL-SERVICE")
    assert opening.stop_reason is SemanticBatchStop.COMPLETED
    assert opening.semantic_steps > 0
    assert opening.external_events_applied == 1
    assert _stored_cell(runtime, "DBL-SERVICE-S") == 0
    assert _stored_cell(runtime, "DBL-STATE") == 2
    assert not backend.suspended
    assert runtime.uart_input == b""
    assert backend.rich_terminal_host.pending_ingress_bytes == 0
    assert backend.rich_terminal_host.pending_ingress_events == 0
    assert backend.rich_terminal_host.accepted_egress_bytes == 73
    assert backend.rich_terminal_host.accepted_egress_batches == 1

    opened = driver.service()
    assert opened.status is DriverStatus.PROGRESS
    assert (
        opened.machine_batches,
        opened.outbound_records,
        opened.ansi_bytes,
        opened.views,
    ) == (1, 1, 0, 0)
    assert driver.core.state is TerminalState.OPENING
    assert driver.core.machine_publications_received == 2
    assert driver.core.machine_publication_bytes_received == 111
    assert driver.core.frames_received == 0
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0
    assert backend.rich_terminal_host.pending_ingress_bytes == 72
    assert backend.rich_terminal_host.pending_ingress_events == 1

    active = backend.run_semantic_batch(entry="DBL-SERVICE")
    assert active.stop_reason is SemanticBatchStop.COMPLETED
    assert active.semantic_steps > 0
    assert active.external_events_applied == 1
    assert _stored_cell(runtime, "DBL-SERVICE-S") == 0
    assert _stored_cell(runtime, "DBL-STATE") == 3
    assert _stored_cell(runtime, "DBL-ACTIVE") == MASK64
    assert _stored_cell(runtime, "DBL-OWNS") == MASK64
    assert _stored_cell(runtime, "DBL-SESSION-ID") == SESSION_ID
    assert _stored_cell(runtime, "DBL-PEER-MAX-PAY") == 256
    assert _stored_cell(runtime, "DBL-PEER-MAX-TX") == 512
    assert _stored_cell(runtime, "DBL-PEER-GRANT") == 1_024
    assert _stored_cell(runtime, "DBL-COLS") == 2
    assert _stored_cell(runtime, "DBL-ROWS") == 2
    assert _stored_cell(runtime, "DBL-CLIENT-MAX-PAY") == 32
    assert _stored_cell(runtime, "DBL-LOCAL-GRANT") == 72
    assert _stored_cell(runtime, "DBL-MAX-TEXT") == 20
    assert _stored_cell(runtime, "DBL-TX-SEQ") == 1
    assert _stored_cell(runtime, "DBL-RX-SEQ") == 1
    assert not backend.suspended
    assert runtime.uart_input == b""
    assert driver.core.state is TerminalState.OPENING
    assert backend.rich_terminal_host.pending_ingress_bytes == 0
    assert backend.rich_terminal_host.pending_ingress_events == 0
    assert backend.rich_terminal_host.accepted_egress_bytes == 72
    assert backend.rich_terminal_host.accepted_egress_batches == 1

    ready = driver.service()
    assert ready.status is DriverStatus.PROGRESS
    assert (
        ready.machine_batches,
        ready.outbound_records,
        ready.ansi_bytes,
        ready.views,
    ) == (1, 0, 0, 0)
    assert driver.core.state is TerminalState.ACTIVE
    assert driver.core.active
    assert driver.core.session_id == SESSION_ID
    assert driver.core.machine_publications_received == 3
    assert driver.core.machine_publication_bytes_received == 183
    assert driver.core.frames_received == 1
    assert driver.core.frame_bytes_received == 72
    assert driver.core.frames_received_by_type == {
        int(MessageType.CLIENT_READY): 1,
    }
    assert driver.core.decoder_buffered_bytes == 0
    assert driver.core.max_text_bytes == 20
    assert driver.core.output_view is None
    assert runtime.uart_input == b""
    assert runtime.uart_output == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert driver.pending_outbound_bytes == 0
    assert driver.pending_outbound_events == 0
    assert driver.failure_reason is None
    assert backend.rich_terminal_host.failure_reason is None
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0
    assert backend.rich_terminal_host.pending_ingress_bytes == 0
    assert backend.rich_terminal_host.pending_ingress_events == 0
    assert legacy_output == []
    assert ansi_output == []
    assert views == []

    polled = backend.run_semantic_batch(entry="DBL-POLL-EMPTY")
    assert polled.stop_reason is SemanticBatchStop.COMPLETED
    assert polled.semantic_steps > 0
    assert polled.external_events_applied == 0
    assert _stored_cell(runtime, "DBL-POLL-EVENT-S") == 0
    assert _stored_cell(runtime, "DBL-POLL-EVENT?") == 0
    assert _stored_cell(runtime, "DBL-POLL-COMPLETION-S") == 0
    assert _stored_cell(runtime, "DBL-POLL-COMPLETION?") == 0
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0

    closing = backend.run_semantic_batch(entry="DBL-CLOSE")
    assert closing.stop_reason is SemanticBatchStop.COMPLETED
    assert closing.semantic_steps > 0
    assert closing.external_events_applied == 0
    assert _stored_cell(runtime, "DBL-CLOSE-S") == 0
    assert _stored_cell(runtime, "DBL-STATE") == 5
    assert _stored_cell(runtime, "DBL-ACTIVE") == 0
    assert _stored_cell(runtime, "DBL-OWNS") == MASK64
    assert _stored_cell(runtime, "DBL-TX-SEQ") == 2
    assert _stored_cell(runtime, "DBL-RX-SEQ") == 1
    assert driver.core.state is TerminalState.ACTIVE
    assert backend.rich_terminal_host.accepted_egress_bytes == 56
    assert backend.rich_terminal_host.accepted_egress_batches == 1

    closed = driver.service()
    assert closed.status is DriverStatus.PROGRESS
    assert (
        closed.machine_batches,
        closed.outbound_records,
        closed.ansi_bytes,
        closed.views,
    ) == (1, 1, 0, 0)
    assert driver.core.state is TerminalState.ANSI
    assert not driver.core.active
    assert driver.core.session_id is None
    assert driver.core.machine_publications_received == 4
    assert driver.core.machine_publication_bytes_received == 239
    assert driver.core.frames_received == 2
    assert driver.core.frame_bytes_received == 128
    assert driver.core.frames_received_by_type == {
        int(MessageType.CLOSE): 1,
        int(MessageType.CLIENT_READY): 1,
    }
    assert driver.core.output_view is None
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0
    assert backend.rich_terminal_host.pending_ingress_bytes == 48
    assert backend.rich_terminal_host.pending_ingress_events == 1

    settled = backend.run_semantic_batch(entry="DBL-SERVICE")
    assert settled.stop_reason is SemanticBatchStop.COMPLETED
    assert settled.semantic_steps > 0
    assert settled.external_events_applied == 1
    assert _stored_cell(runtime, "DBL-SERVICE-S") == 0
    assert _stored_cell(runtime, "DBL-STATE") == 0
    assert _stored_cell(runtime, "DBL-ACTIVE") == 0
    assert _stored_cell(runtime, "DBL-OWNS") == 0
    assert _stored_cell(runtime, "DBL-TX-SEQ") == 2
    assert _stored_cell(runtime, "DBL-RX-SEQ") == 2
    assert driver.core.state is TerminalState.ANSI
    assert runtime.uart_input == b""
    assert runtime.uart_output == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert driver.pending_outbound_bytes == 0
    assert driver.pending_outbound_events == 0
    assert driver.failure_reason is None
    assert backend.rich_terminal_host.failure_reason is None
    assert backend.rich_terminal_host.accepted_egress_bytes == 0
    assert backend.rich_terminal_host.accepted_egress_batches == 0
    assert backend.rich_terminal_host.pending_ingress_bytes == 0
    assert backend.rich_terminal_host.pending_ingress_events == 0
    assert legacy_output == []
    assert ansi_output == []
    assert views == []
