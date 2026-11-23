"""The BIOS's cooperative task slots on the emulator and on the simulator.

The same KDOS programs run on both machines (docs/multicore-simulator-plan.md,
stage 5): BACKGROUND, BACKGROUND2 and BACKGROUND3 start a slot, PAUSE runs
the slots in turn from the foreground, TASK-YIELD gives the foreground its
turn back, TASK-STOP ends a slot, and TASK?, #TASKS and TASK-ID report them.
The emulator runs the BIOS's scheduler, which switches stacks and program
counters through SEP R20; the simulator runs each slot as a context of core
0 (simulator/tasks.py). Each case prints between markers what the slots
did: their order, their stacks across switches, TASK-ID, KDOS's CATCH
chains, one for each slot, and the CRC unit's owner, which the BIOS records
as (COREID, TASK-ID). The emulator must print the case's expected text and
the simulator what the emulator printed, on one core and on two, where the
other core sleeps in KDOS's worker loop and, in one case, runs a job while
core 0's slots run. As in tests/test_multicore_differential.py, whose
machines these are, each case also runs on a second native simulator and on
the Python executor, and the three simulators must agree exactly.

The cases come from tests/test_system.py's cooperative multitasking and
KDOS CATCH tests and from Akashic's coroutine tests. They run in order on
one machine of each kind, so a case that depends on where PAUSE's
round-robin scan stands first sets it: a slot that PAUSE runs alone leaves
the scan's cursor at that slot.

PAUSE in a slot, TASK-YIELD in the foreground, and either word on a worker
core take the illegal-operation fault on both machines, where the machine
used to crash: KDOS throws -21 to the innermost CATCH, and with none the
fault's report ends the line, or the worker's job, as an ABORT does. An
abort or an uncaught fault in a slot ends the slot together with the line
that paused, and TASK-ID answers 0 again (finding 15 of the plan). These
cases run on fresh machines of their own, since each ends a line.
"""

from __future__ import annotations

import pytest

from simulator.errors import ExecutionError, ForthAbort, SourceError
from tests.test_multicore_differential import (
    _BETWEEN,
    _PROGRAM_COUNTER,
    _Emulator,
    _Simulator,
    _bios,
    _kdos,
    case_word,
)


# name: (minimum cores, definitions, body, what the body prints)
CASES = {
    "words": (1, b"", b"""
1 TASK? . 2 TASK? . 3 TASK? . 0 TASK? . #TASKS . PAUSE PAUSE TASK-ID .""",
              b"0 0 0 0 0 0 "),
    "one-shot": (1, b"""
VARIABLE TS-ONE
: TS-ONE-WORK ( -- ) 42 TS-ONE ! ;
""", b"""0 TS-ONE ! ['] TS-ONE-WORK BACKGROUND 1 TASK? . #TASKS . TS-ONE @ .
PAUSE 1 TASK? . #TASKS . TS-ONE @ .""", b"1 1 0 0 0 42 "),
    "round-robin": (1, b"""
CREATE TS-LOG 16 CELLS ALLOT  VARIABLE TS-N
: TS-LOG! ( n -- ) TS-N @ CELLS TS-LOG + !  1 TS-N +! ;
: TS-R1 ( -- ) BEGIN 1 TS-LOG! TASK-YIELD AGAIN ;
: TS-R2 ( -- ) BEGIN 2 TS-LOG! TASK-YIELD AGAIN ;
: TS-R3 ( -- ) BEGIN 3 TS-LOG! TASK-YIELD AGAIN ;
: TS-SHOW ( -- ) TS-N @ 0 ?DO I CELLS TS-LOG + @ . LOOP  0 TS-N ! ;
""", b"""['] TS-R3 BACKGROUND3 PAUSE 3 TASK-STOP  0 TS-N !
['] TS-R2 BACKGROUND2 ['] TS-R3 BACKGROUND3  PAUSE PAUSE PAUSE  TS-SHOW
['] TS-R1 BACKGROUND  PAUSE PAUSE PAUSE PAUSE  TS-SHOW
2 TASK-STOP  PAUSE PAUSE PAUSE  TS-SHOW
1 TASK-STOP 3 TASK-STOP  PAUSE  TS-SHOW #TASKS .""",
                    b"2 3 2 3 1 2 3 1 3 1 0 "),
    # The loop's parameters stay on the slot's return stack across yields.
    "yields": (1, b"""
VARIABLE TS-SUM
: TS-YWORK ( -- ) 0  5 0 DO I + TASK-YIELD LOOP  TS-SUM ! ;
""", b"""0 TS-SUM ! ['] TS-YWORK BACKGROUND
PAUSE PAUSE PAUSE PAUSE PAUSE  TS-SUM @ . 1 TASK? .
PAUSE  TS-SUM @ . 1 TASK? .""", b"0 1 10 0 "),
    # A slot that stops itself runs on to its next yield, and no further.
    "stop": (1, b"""
VARIABLE TS-COUNT
: TS-LOOP ( -- ) BEGIN 1 TS-COUNT +! TASK-YIELD AGAIN ;
: TS-SELF ( -- ) 1 TS-COUNT +!  2 TASK-STOP  1 TS-COUNT +!  TASK-YIELD  100 TS-COUNT +! ;
""", b"""0 TS-COUNT ! ['] TS-LOOP BACKGROUND PAUSE PAUSE 1 TASK-STOP PAUSE
TS-COUNT @ . 1 TASK? .
0 TS-COUNT ! ['] TS-SELF BACKGROUND2 2 TASK? . PAUSE 2 TASK? . TS-COUNT @ .
PAUSE TS-COUNT @ .""", b"2 0 1 0 2 2 "),
    # BACKGROUND over a yielded slot starts the new word afresh.
    "restart": (1, b"""
VARIABLE TS-STATE
: TS-OLD ( -- ) 1 TS-STATE !  TASK-YIELD  2 TS-STATE ! ;
: TS-NEW ( -- ) 10 TS-STATE ! ;
""", b"""0 TS-STATE ! ['] TS-OLD BACKGROUND3 PAUSE TS-STATE @ .
['] TS-NEW BACKGROUND3 PAUSE TS-STATE @ . 3 TASK? . PAUSE TS-STATE @ .""",
                b"1 10 0 10 "),
    "stacks": (1, b"""
VARIABLE TS-KSUM
: TS-KWORK ( -- ) 1 2 3 TASK-YIELD 4 TASK-YIELD + + + TS-KSUM ! ;
""", b"""0 TS-KSUM ! 11 22 33 ['] TS-KWORK BACKGROUND PAUSE PAUSE PAUSE
. . . TS-KSUM @ . 1 TASK? .""", b"33 22 11 10 0 "),
    "task-id": (1, b"""
CREATE TS-IDS 4 CELLS ALLOT
: TS-ID1 ( -- ) TASK-ID 1 CELLS TS-IDS + ! ;
: TS-ID2 ( -- ) TASK-ID 2 CELLS TS-IDS + ! ;
: TS-ID3 ( -- ) TASK-ID 3 CELLS TS-IDS + ! ;
""", b"""TS-IDS 4 CELLS 255 FILL  TASK-ID TS-IDS !
['] TS-ID1 BACKGROUND ['] TS-ID2 BACKGROUND2 ['] TS-ID3 BACKGROUND3  PAUSE PAUSE PAUSE
4 0 DO I CELLS TS-IDS + @ . LOOP  TASK-ID . #TASKS .""", b"0 1 2 3 0 0 "),
    # KDOS keeps a CATCH chain for each slot: the foreground's and a slot's
    # frames stay live across the switches between them.
    "catch-interleave": (1, b"""
VARIABLE _CTX-FG-R VARIABLE _CTX-BG-R
VARIABLE _CTX-FG-H VARIABLE _CTX-BG-H VARIABLE _CTX-BG-ID
: _CTX-BG-INNER  TASK-ID _CTX-BG-ID ! TASK-YIELD 22 THROW ;
: _CTX-BG-WORK  HANDLER _CTX-BG-H ! ['] _CTX-BG-INNER CATCH _CTX-BG-R ! ;
: _CTX-FG-INNER
  HANDLER _CTX-FG-H !
  ['] _CTX-BG-WORK BACKGROUND
  PAUSE
  11 THROW ;
""", b"""['] _CTX-FG-INNER CATCH _CTX-FG-R !  PAUSE
_CTX-FG-R @ . _CTX-BG-R @ . _CTX-BG-ID @ .  _CTX-FG-H @ _CTX-BG-H @ <> .""",
                         b"11 22 1 -1 "),
    "catch-slots": (1, b"""
VARIABLE _SLOT-R1 VARIABLE _SLOT-R2 VARIABLE _SLOT-R3
VARIABLE _SLOT-H1 VARIABLE _SLOT-H2 VARIABLE _SLOT-H3
: _SLOT-I1  TASK-YIELD 101 THROW ;
: _SLOT-I2  TASK-YIELD 202 THROW ;
: _SLOT-I3  TASK-YIELD 303 THROW ;
: _SLOT-W1  HANDLER _SLOT-H1 ! ['] _SLOT-I1 CATCH _SLOT-R1 ! ;
: _SLOT-W2  HANDLER _SLOT-H2 ! ['] _SLOT-I2 CATCH _SLOT-R2 ! ;
: _SLOT-W3  HANDLER _SLOT-H3 ! ['] _SLOT-I3 CATCH _SLOT-R3 ! ;
""", b"""['] _SLOT-W1 BACKGROUND ['] _SLOT-W2 BACKGROUND2 ['] _SLOT-W3 BACKGROUND3
PAUSE PAUSE PAUSE PAUSE PAUSE PAUSE
_SLOT-R1 @ . _SLOT-R2 @ . _SLOT-R3 @ .
_SLOT-H1 @ _SLOT-H2 @ <> . _SLOT-H2 @ _SLOT-H3 @ <> . _SLOT-H1 @ _SLOT-H3 @ <> .""",
                    b"101 202 303 -1 -1 -1 "),
    # A stopped or replaced slot never unwinds its frames, so KDOS clears
    # the slot's chain when it starts or stops one.
    "catch-stop-reuse": (1, b"""
VARIABLE _REUSE-BEFORE VARIABLE _REUSE-RESULT VARIABLE _REUSE-AFTER
: _REUSE-OLD-INNER  TASK-YIELD  -71 THROW ;
: _REUSE-OLD-WORK  ['] _REUSE-OLD-INNER CATCH DROP ;
: _REUSE-NEW-INNER  -72 THROW ;
: _REUSE-NEW-WORK
  HANDLER @ _REUSE-BEFORE !
  ['] _REUSE-NEW-INNER CATCH _REUSE-RESULT !
  HANDLER @ _REUSE-AFTER ! ;
""", b"""['] _REUSE-OLD-WORK BACKGROUND PAUSE 1 TASK-STOP ['] _REUSE-NEW-WORK BACKGROUND PAUSE
_REUSE-BEFORE @ . _REUSE-RESULT @ . _REUSE-AFTER @ . 1 TASK? .""", b"0 -72 0 0 "),
    "catch-replace": (1, b"""
VARIABLE _REPLACE-BEFORE VARIABLE _REPLACE-RESULT VARIABLE _REPLACE-AFTER
: _REPLACE-OLD-INNER  TASK-YIELD  -81 THROW ;
: _REPLACE-OLD-WORK  ['] _REPLACE-OLD-INNER CATCH DROP ;
: _REPLACE-NEW-INNER  -82 THROW ;
: _REPLACE-NEW-WORK
  HANDLER @ _REPLACE-BEFORE !
  ['] _REPLACE-NEW-INNER CATCH _REPLACE-RESULT !
  HANDLER @ _REPLACE-AFTER ! ;
""", b"""['] _REPLACE-OLD-WORK BACKGROUND2 PAUSE ['] _REPLACE-NEW-WORK BACKGROUND2 PAUSE
_REPLACE-BEFORE @ . _REPLACE-RESULT @ . _REPLACE-AFTER @ . 2 TASK? .""",
                      b"0 -82 0 0 "),
    # The CRC unit's owner is (COREID, TASK-ID): a slot cannot take or feed
    # the foreground's transaction, nor the foreground a slot's.
    "crc-owner": (1, b"""
VARIABLE TS-C1 VARIABLE TS-C2 VARIABLE TS-C3
: TS-CSLOT ( -- ) 0 CRC-MODE! TS-C1 !  TASK-YIELD
  0 CRC-MODE! TS-C2 !  CRC-RESET TS-C3 !  CRC-FINAL@ DROP ;
: TS-COWN ( -- ) 0 CRC-MODE! TS-C1 !  TASK-YIELD  CRC-FINAL@ DROP ;
""", b"""0 CRC-MODE! .  ['] TS-CSLOT BACKGROUND  PAUSE  CRC-FINAL@ DROP  PAUSE
TS-C1 @ . TS-C2 @ . TS-C3 @ .
['] TS-COWN BACKGROUND2  PAUSE  TS-C1 @ .  5 CRC-FEED .  0 CRC-MODE! .  PAUSE
0 CRC-MODE! .  CRC-FINAL@ DROP""", b"0 2 0 0 0 2 2 0 "),
    # A slot's data stack lies below the dictionary, so the checked span
    # rule, which ends a Bank 0 span below the caller's stack, refuses every
    # Bank 0 buffer a slot names; one in external memory passes.
    "spans": (1, b"""
CREATE TS-BUF 16 ALLOT  VARIABLE TS-XBUF  16 XMEM-ALLOT TS-XBUF !
VARIABLE TS-S1  VARIABLE TS-S2
: TS-SPANS ( -- ) TS-BUF 16 CALLER-SPAN-STATUS TS-S1 !
  TS-XBUF @ 16 CALLER-SPAN-STATUS TS-S2 ! ;
""", b"""TS-BUF 16 CALLER-SPAN-STATUS . TS-XBUF @ 16 CALLER-SPAN-STATUS .
['] TS-SPANS BACKGROUND PAUSE TS-S1 @ . TS-S2 @ .""", b"0 0 3 0 "),
    # Core 1 runs a job while core 0's slot runs.
    "slots-with-worker": (2, b"""
VARIABLE TS-WCOUNT  VARIABLE TS-WSLOT
: TS-WJOB ( -- ) 2000 0 DO 1 TS-WCOUNT +! LOOP ;
: TS-WBG ( -- ) BEGIN 1 TS-WSLOT +! TASK-YIELD AGAIN ;
""", b"""0 TS-WCOUNT ! 0 TS-WSLOT ! ['] TS-WJOB 1 CORE-RUN ['] TS-WBG BACKGROUND
50 0 DO PAUSE LOOP  1 TASK-STOP  1 CORE-WAIT  TS-WCOUNT @ . TS-WSLOT @ .""",
                          b"2000 50 "),
    # PAUSE in a slot and TASK-YIELD in the foreground take the
    # illegal-operation fault, which KDOS throws to the CATCH around each:
    # the slot runs on to its end, and the foreground keeps its line.
    "misuse-caught": (1, b"""
VARIABLE TS-MR1  VARIABLE TS-MR2  VARIABLE TS-MC
: TS-MPAUSE ( -- ) 1 TS-MC +!  ['] PAUSE CATCH TS-MR1 !  10 TS-MC +! ;
: TS-MYIELD ( -- ) ['] TASK-YIELD CATCH TS-MR2 ! ;
""", b"""0 TS-MC ! ['] TS-MPAUSE BACKGROUND PAUSE
TS-MR1 @ . TS-MC @ . 1 TASK? . TASK-ID .
TS-MYIELD TS-MR2 @ . TASK-ID . #TASKS .""", b"-21 11 0 0 -21 0 0 "),
    # A worker core has no slots: a job's PAUSE or TASK-YIELD faults on its
    # core, inside a CATCH or, with none, ending the job after the report.
    "misuse-workers": (2, b"""
VARIABLE TS-WR  VARIABLE TS-WC
: TS-WP ( -- ) ['] PAUSE CATCH TS-WR ! ;
: TS-WY ( -- ) ['] TASK-YIELD CATCH TS-WR +! ;
: TS-WU ( -- ) 1 TS-WC +!  PAUSE  10 TS-WC +! ;
""", b"""0 TS-WR ! 0 TS-WC !  ['] TS-WP 1 CORE-RUN 1 CORE-WAIT
['] TS-WY 1 CORE-RUN 1 CORE-WAIT  TS-WR @ .
['] TS-WU 1 CORE-RUN 1 CORE-WAIT  TS-WC @ . 1 CORE-STATUS . TASK-ID .""",
                       b"-42 \r\n*** ILLEGAL INSTRUCTION CORE=01\r\n1 0 0 "),
}


@pytest.fixture(scope="module")
def machines():
    """Machines kept for the module, so that each case runs on the state
    the cases before it left: the emulator, and three simulators of each
    topology, the native executor twice and the Python one once."""

    bios, kdos = _bios(), _kdos()
    built = {}

    def machine(kind: str, cores: int, copy: int = 0):
        key = (kind, cores, copy)
        if key not in built:
            built[key] = (
                _Emulator(cores, bios, kdos) if kind == "emulator"
                else _Simulator(cores, kind, kdos)
            )
        return built[key]

    return machine


@pytest.mark.parametrize("name,cores", [
    (name, cores) for name, (minimum, *_rest) in CASES.items()
    for cores in (1, 2) if cores >= minimum
])
def test_the_task_slots_agree_with_the_emulator(machines, name, cores):
    _minimum, setup, body, expected = CASES[name]
    word = case_word("slots-" + name)
    assert machines("emulator", cores).run_case(word, setup, body) == expected
    first = machines("native", cores).run_case(word, setup, body)
    assert first.text == expected
    assert machines("native", cores, copy=1).run_case(word, setup, body) == first
    assert machines("python", cores).run_case(word, setup, body) == first


# An abort in slot 1 after one yield, and in slot 2 before any; then what
# the foreground sees, and a PAUSE that must find no slot to resume.
ABORT_SETUP = b"""VARIABLE TS-RUNS
: TS-QUITTER ( -- ) 1 TS-RUNS +! TASK-YIELD 10 TS-RUNS +! TRUE ABORT" slot gave up" ;
: TS-EARLY ( -- ) 100 TS-RUNS +! TRUE ABORT" slot gave up" ;"""
ABORT_LINES = (
    b"0 TS-RUNS ! ' TS-QUITTER BACKGROUND PAUSE PAUSE",
    b"' TS-EARLY BACKGROUND2 PAUSE",
)
ABORT_REPORT = (b"60 EMIT 60 EMIT TASK-ID . 1 TASK? . 2 TASK? . TS-RUNS @ . "
                b"PAUSE TS-RUNS @ . #TASKS . 62 EMIT 62 EMIT")
ABORT_EXPECTED = b"0 0 0 111 111 0 "


def _between(output: bytes) -> bytes:
    found = _BETWEEN.search(output)
    assert found is not None, output[-400:]
    return found.group(1)


@pytest.mark.parametrize("executor", ("native", "python"))
def test_an_abort_in_a_slot_ends_the_slot_on_the_simulator(executor):
    runtime = _Simulator(1, executor, _kdos()).runtime
    runtime.evaluate(ABORT_SETUP)
    for line in ABORT_LINES:
        with pytest.raises(ForthAbort):
            runtime.evaluate(line)
        assert runtime.drain_uart_output() == b"slot gave up"
    runtime.evaluate(ABORT_REPORT)
    assert _between(runtime.drain_uart_output()) == ABORT_EXPECTED


def test_an_abort_in_a_slot_ends_the_slot_on_the_emulator():
    emulator = _Emulator(1, _bios(), _kdos())
    emulator.type(ABORT_SETUP.splitlines())
    for line in ABORT_LINES:
        assert b"slot gave up" in emulator.type([line])
    assert _between(emulator.type([ABORT_REPORT])) == ABORT_EXPECTED


# PAUSE in a slot with no CATCH, then TASK-YIELD in the foreground: each
# fault is reported and ends its line, the first ending the slot with it;
# then what the foreground sees, and a PAUSE that finds no slot.
MISUSE_SETUP = b"""VARIABLE TS-C
: TS-INNER ( -- ) 1 TS-C +! PAUSE 1 TS-C +! ;"""
MISUSE_LINES = (
    b"0 TS-C ! ' TS-INNER BACKGROUND PAUSE",
    b"TASK-YIELD 2 TS-C !",
)
MISUSE_FAULT = b"\r\n*** ILLEGAL INSTRUCTION CORE=00\r\n"
MISUSE_REPORT = (b"60 EMIT 60 EMIT TS-C @ . 1 TASK? . TASK-ID . #TASKS . "
                 b"PAUSE TS-C @ . 62 EMIT 62 EMIT")
MISUSE_EXPECTED = b"1 0 0 0 1 "


@pytest.mark.parametrize("executor", ("native", "python"))
def test_an_uncaught_misuse_of_the_slot_words_faults_on_the_simulator(executor):
    runtime = _Simulator(1, executor, _kdos()).runtime
    runtime.evaluate(MISUSE_SETUP)
    for line in MISUSE_LINES:
        with pytest.raises(ForthAbort):
            runtime.evaluate(line)
        assert runtime.drain_uart_output() == MISUSE_FAULT
    runtime.evaluate(MISUSE_REPORT)
    assert _between(runtime.drain_uart_output()) == MISUSE_EXPECTED


def test_an_uncaught_misuse_of_the_slot_words_faults_on_the_emulator():
    emulator = _Emulator(1, _bios(), _kdos())
    emulator.type(MISUSE_SETUP.splitlines())
    for line in MISUSE_LINES:
        # The BIOS's report also names the program counter.
        assert MISUSE_FAULT in _PROGRAM_COUNTER.sub(b"", emulator.type([line]))
    assert _between(emulator.type([MISUSE_REPORT])) == MISUSE_EXPECTED


# A diagnostic that ends a line and QUIT, each in a slot, end the slot with
# the line, and the foreground keeps the data stack it had when it paused,
# 1 2, while the slot's 7 8 go with the slot. The BIOS prints the
# diagnostic; the simulator's evaluator raises it to its host. Then what the
# foreground sees, and a PAUSE that finds no slot.
QUIT_SETUP = b"""VARIABLE TS-AFTER
: TS-BADDO ( -- ) 7 8 S" DO" EVALUATE ;
: TS-QUITS ( -- ) 7 8 QUIT ;
: TS-SHOW ( -- ) DEPTH DUP . 0 ?DO . LOOP ;"""
QUIT_CASES = {
    "diagnostic": b"0 TS-AFTER ! 1 2 ' TS-BADDO BACKGROUND PAUSE 9 TS-AFTER !",
    "quit": b"0 TS-AFTER ! 1 2 ' TS-QUITS BACKGROUND PAUSE 9 TS-AFTER !",
}
QUIT_REPORT = (b"60 EMIT 60 EMIT TASK-ID . 1 TASK? . TS-AFTER @ . TS-SHOW "
               b"PAUSE #TASKS . 62 EMIT 62 EMIT")
QUIT_EXPECTED = b"0 0 0 2 2 1 0 "


@pytest.mark.parametrize("case", tuple(QUIT_CASES))
@pytest.mark.parametrize("executor", ("native", "python"))
def test_a_diagnostic_or_quit_in_a_slot_ends_the_slot_on_the_simulator(
        executor, case):
    runtime = _Simulator(1, executor, _kdos()).runtime
    runtime.evaluate(QUIT_SETUP)
    runtime.drain_uart_output()
    with pytest.raises(SourceError if case == "diagnostic" else ForthAbort):
        runtime.evaluate(QUIT_CASES[case])
    runtime.evaluate(QUIT_REPORT)
    assert _between(runtime.drain_uart_output()) == QUIT_EXPECTED


@pytest.mark.parametrize("case", tuple(QUIT_CASES))
def test_a_diagnostic_or_quit_in_a_slot_ends_the_slot_on_the_emulator(case):
    emulator = _Emulator(1, _bios(), _kdos())
    emulator.type(QUIT_SETUP.splitlines())
    said = emulator.type([QUIT_CASES[case]])
    assert (b"compile-only word" in said) == (case == "diagnostic")
    assert _between(emulator.type([QUIT_REPORT])) == QUIT_EXPECTED


# TASK-STOP and TASK? refuse a number that names no slot without taking it:
# the BIOS says so and ends the line through QUIT, keeping the data stack,
# and the simulator's words fail to their host. KDOS's TASK-STOP duplicates
# the number before the BIOS's word sees it, so two copies stay. Slot 1,
# which yielded once before the refusals, then runs on as before.
SLOT_NUMBER_SETUP = b"""VARIABLE TS-N
: TS-COUNTER ( -- ) BEGIN 1 TS-N +! TASK-YIELD AGAIN ;
: TS-SHOW ( -- ) DEPTH DUP . 0 ?DO . LOOP ;
0 TS-N ! ' TS-COUNTER BACKGROUND PAUSE"""
SLOT_NUMBER_REFUSALS = (
    (b"0 TASK-STOP", b"2 0 0 "),
    (b"4 TASK-STOP", b"2 4 4 "),
    (b"5 TASK-STOP", b"2 5 5 "),
    (b"7 TASK-STOP", b"2 7 7 "),
    (b"-1 TASK-STOP", b"2 -1 -1 "),
    (b"4 TASK?", b"1 4 "),
    (b"7 TASK?", b"1 7 "),
    (b"-1 TASK?", b"1 -1 "),
)
SLOT_NUMBER_SHOW = b"60 EMIT 60 EMIT TS-SHOW 62 EMIT 62 EMIT"
SLOT_NUMBER_AFTER = (b"60 EMIT 60 EMIT 0 TASK? . PAUSE TS-N @ . 1 TASK? . "
                     b"TASK-ID . 62 EMIT 62 EMIT")
SLOT_NUMBER_AFTER_EXPECTED = b"0 2 1 0 "


@pytest.mark.parametrize("executor", ("native", "python"))
def test_the_slot_words_refuse_a_number_naming_no_slot_on_the_simulator(executor):
    runtime = _Simulator(1, executor, _kdos()).runtime
    runtime.evaluate(SLOT_NUMBER_SETUP)
    runtime.drain_uart_output()
    for line, left in SLOT_NUMBER_REFUSALS:
        with pytest.raises(ExecutionError, match="slot must be"):
            runtime.evaluate(line)
        runtime.evaluate(SLOT_NUMBER_SHOW)
        assert _between(runtime.drain_uart_output()) == left, line
    runtime.evaluate(SLOT_NUMBER_AFTER)
    assert _between(runtime.drain_uart_output()) == SLOT_NUMBER_AFTER_EXPECTED


def test_the_slot_words_refuse_a_number_naming_no_slot_on_the_emulator():
    emulator = _Emulator(1, _bios(), _kdos())
    emulator.type(SLOT_NUMBER_SETUP.splitlines())
    for line, left in SLOT_NUMBER_REFUSALS:
        assert b"no such task slot" in emulator.type([line]), line
        assert _between(emulator.type([SLOT_NUMBER_SHOW])) == left, line
    assert _between(emulator.type([SLOT_NUMBER_AFTER])) == SLOT_NUMBER_AFTER_EXPECTED
