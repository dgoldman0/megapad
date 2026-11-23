"""The BIOS's cooperative task slots on core 0 of the hosted simulator.

BACKGROUND, BACKGROUND2 and BACKGROUND3 start a word in slot 1, 2 or 3,
PAUSE runs the next active slot from the foreground until the slot calls
TASK-YIELD or its word returns, TASK-STOP ends a slot, and TASK?, #TASKS and
TASK-ID report them (simulator/tasks.py, docs/multicore-simulator-plan.md,
stage 5). tests/test_task_slots_differential.py compares the slots with the
emulator's BIOS, KDOS loaded; the cases here run on the simulator's own
vocabulary and cover what only the simulator can show or where it departs
from the BIOS on purpose: where the slots' stacks lie and a slot's stack
overflow, a slot's abort and the words used from the wrong context without
KDOS's CATCH, dictionary rollback, and slots on a machine with several
cores. Every case runs on both executors.
"""

from __future__ import annotations

import pytest

from simulator.errors import ExecutionError, ForthAbort
from simulator.platform import Topology, create_address_space
from simulator.runtime import MegaForthRuntime
from simulator.stacks import StackOverflow
from simulator.tasks import SLOT_CELLS

EXECUTORS = ("python", "native")


def _runtime(executor: str, cores: int = 1) -> MegaForthRuntime:
    if cores == 1:
        return MegaForthRuntime(execution_backend=executor)
    memory = create_address_space(topology=Topology(full_cores=cores))
    return MegaForthRuntime(
        memory=memory, execution_backend=executor, turn_quantum_steps=1_000,
    )


def _cell(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(runtime.find(name).body_address)


def _said(runtime: MegaForthRuntime, source: bytes) -> bytes:
    runtime.evaluate(source)
    return runtime.drain_uart_output()


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_slot_words_start_run_report_and_stop_the_slots(executor):
    runtime = _runtime(executor)
    said = _said(runtime, b"""VARIABLE C 0 C !  VARIABLE SEEN
: BG ( -- ) BEGIN 1 C +! TASK-ID SEEN ! TASK-YIELD AGAIN ;
#TASKS . 0 TASK? . PAUSE
' BG BACKGROUND2 #TASKS . 2 TASK? . PAUSE PAUSE PAUSE C @ . SEEN @ . TASK-ID .
2 TASK-STOP #TASKS . 2 TASK? .
: ONE ( -- ) 42 C ! ; ' ONE BACKGROUND3 PAUSE C @ . 3 TASK? .
: RUN ( -- ) 11 22 33 ['] BG BACKGROUND PAUSE PAUSE 1 TASK-STOP . . . ; RUN
""")
    assert said == b"0 0 1 1 3 2 0 0 0 42 0 33 22 11 "
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.tasks.active == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_each_slot_runs_on_stacks_of_32_cells_below_the_dictionary(executor):
    runtime = _runtime(executor)
    runtime.evaluate(b"""CREATE SPS 4 CELLS ALLOT  CREATE RPS 4 CELLS ALLOT
: NOTE ( slot -- ) DUP CELLS SPS + SP@ SWAP !  CELLS RPS + RP@ SWAP ! ;
: N1 ( -- ) 1 NOTE ;  : N2 ( -- ) 2 NOTE ;  : N3 ( -- ) 3 NOTE ;
' N1 BACKGROUND ' N2 BACKGROUND2 ' N3 BACKGROUND3 PAUSE PAUSE PAUSE""")
    base = runtime.tasks.base
    assert base + 3 * 2 * SLOT_CELLS * 8 == runtime.dictionary.start_address
    for slot in (1, 2, 3):
        floor = base + (slot - 1) * 2 * SLOT_CELLS * 8
        data = runtime.memory.read64(runtime.find("SPS").body_address + 8 * slot)
        returns = runtime.memory.read64(runtime.find("RPS").body_address + 8 * slot)
        assert floor <= data < floor + SLOT_CELLS * 8
        assert floor + SLOT_CELLS * 8 <= returns < floor + 2 * SLOT_CELLS * 8

    # A slot holds 32 cells; the 33rd overflows, which ends the slot. FULL's
    # stack peaks at WANT cells: WANT - 1 values and DEPTH's address.
    runtime.evaluate(b"""VARIABLE WANT  VARIABLE DEPTH
: FULL ( -- ) WANT @ 1- 0 DO I LOOP DEPTH ! ;""")
    for peak, fits in ((SLOT_CELLS, True), (SLOT_CELLS + 1, False)):
        runtime.evaluate(str(peak).encode() + b" WANT !  0 DEPTH !  ' FULL BACKGROUND")
        if fits:
            runtime.evaluate(b"PAUSE")
            assert _cell(runtime, "DEPTH") == peak - 2
        else:
            with pytest.raises(StackOverflow):
                runtime.evaluate(b"PAUSE")
            assert _cell(runtime, "DEPTH") == 0
        assert _said(runtime, b"1 TASK? . TASK-ID .") == b"0 0 "
        runtime.main_context.data.clear()


@pytest.mark.parametrize("executor", EXECUTORS)
def test_an_abort_in_a_slot_ends_the_slot_and_the_line_that_paused(executor):
    # As on the machine, where the BIOS's QUIT ends the slot that was
    # running, the slot ends with the line and TASK-ID is 0 again.
    runtime = _runtime(executor)
    runtime.evaluate(b"""VARIABLE AFTER  0 AFTER !
: QUITTER ( -- ) TRUE ABORT" slot gave up" ;
' QUITTER BACKGROUND2""")
    with pytest.raises(ForthAbort):
        runtime.evaluate(b"PAUSE 1 AFTER !")
    assert runtime.drain_uart_output() == b"slot gave up"
    assert _cell(runtime, "AFTER") == 0
    assert _said(runtime, b"2 TASK? . #TASKS . TASK-ID . PAUSE 7 .") == b"0 0 0 7 "


@pytest.mark.parametrize("executor", EXECUTORS)
def test_pause_in_a_slot_and_task_yield_in_the_foreground_fault(executor):
    # The BIOS's words refuse both with the same fault: PAUSE would take the
    # running slot's stacks for the foreground's, and TASK-YIELD would return
    # to a PAUSE that never ran.
    runtime = _runtime(executor)
    runtime.evaluate(b"""VARIABLE C  0 C !
: INNER ( -- ) 1 C +! PAUSE 1 C +! ;
' INNER BACKGROUND""")
    with pytest.raises(ForthAbort):
        runtime.evaluate(b"PAUSE")
    assert runtime.drain_uart_output() == b"\r\n*** PAUSE OR TASK-YIELD REFUSED CORE=00\r\n"
    assert _said(runtime, b"C @ . 1 TASK? . TASK-ID .") == b"1 0 0 "

    with pytest.raises(ForthAbort):
        runtime.evaluate(b"TASK-YIELD 2 C !")
    assert runtime.drain_uart_output() == b"\r\n*** PAUSE OR TASK-YIELD REFUSED CORE=00\r\n"
    assert _cell(runtime, "C") == 1


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_worker_that_reaches_pause_or_task_yield_faults(executor):
    # As the BIOS's words refuse them: PAUSE on a worker would run one of
    # core 0's slots there, racing core 0 for the slot's records.
    runtime = _runtime(executor, cores=2)
    runtime.evaluate(b"""VARIABLE C  0 C !
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
: P-JOB ( -- ) 1 C +! PAUSE 10 C +! ;
: Y-JOB ( -- ) 100 C +! TASK-YIELD 1000 C +! ;""")
    for job in (b"P-JOB", b"Y-JOB"):
        runtime.evaluate(b"' " + job + b" 1 WAKE-CORE 1 WAIT-FOR")
        assert runtime.drain_uart_output() == b"\r\n*** PAUSE OR TASK-YIELD REFUSED CORE=01\r\n"
    assert _cell(runtime, "C") == 101
    assert runtime.cores.job(1) == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_rollback_is_refused_while_a_slot_runs_inside_the_words_it_removes(executor):
    runtime = _runtime(executor)
    runtime.evaluate(b"""VARIABLE SAVED-HERE  VARIABLE SAVED-LATEST  VARIABLE C
HERE SAVED-HERE !  LATEST SAVED-LATEST !
: FOREVER ( -- ) BEGIN 1 C +! TASK-YIELD AGAIN ;
: ONCE ( -- ) 1 C +! ;""")
    rollback = b"SAVED-HERE @ SAVED-LATEST @ DICT-ROLLBACK"
    # A slot that has yielded inside FOREVER, and one that will start ONCE.
    runtime.evaluate(b"' FOREVER BACKGROUND PAUSE ' ONCE BACKGROUND3")
    with pytest.raises(ExecutionError, match="task slot 1 runs"):
        runtime.evaluate(rollback)
    runtime.evaluate(b"1 TASK-STOP")
    with pytest.raises(ExecutionError, match="task slot 3 runs"):
        runtime.evaluate(rollback)
    runtime.evaluate(b"3 TASK-STOP")
    runtime.evaluate(rollback)
    assert runtime.find("FOREVER") is None
    assert runtime.find("ONCE") is None


@pytest.mark.parametrize("executor", EXECUTORS)
def test_slots_run_on_core_zero_whichever_core_starts_them(executor):
    runtime = _runtime(executor, cores=2)
    runtime.evaluate(b"""VARIABLE WTID  VARIABLE STID  VARIABLE SCORE
: SLOTWORD ( -- ) TASK-ID STID !  COREID SCORE ! ;
: WJOB ( -- ) TASK-ID WTID !  ['] SLOTWORD BACKGROUND ;
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
-1 WTID ! -1 STID ! -1 SCORE !
' WJOB 1 WAKE-CORE  1 WAIT-FOR  1 TASK? . PAUSE  1 TASK? .""")
    assert runtime.drain_uart_output() == b"1 0 "
    assert (_cell(runtime, "WTID"), _cell(runtime, "STID"), _cell(runtime, "SCORE")) == (0, 1, 0)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_other_cores_take_their_turns_while_a_slot_runs(executor):
    # A slot runs inside core 0's PAUSE, which cannot detach, so core 0's
    # turns end in place there and the worker's job runs to its end while
    # the slot does, before PAUSE returns.
    runtime = _runtime(executor, cores=2)
    runtime.evaluate(b"""VARIABLE WCOUNT  VARIABLE SCOUNT
: WJOB ( -- ) 2000 0 DO 1 WCOUNT +! LOOP ;
: SLOW ( -- ) 20000 0 DO 1 SCOUNT +! LOOP ;
: GO ( -- ) ['] WJOB 1 WAKE-CORE ['] SLOW BACKGROUND PAUSE ;
0 WCOUNT ! 0 SCOUNT ! GO""")
    assert _cell(runtime, "SCOUNT") == 20000
    assert _cell(runtime, "WCOUNT") == 2000
    assert runtime.cores.job(1) == 0
