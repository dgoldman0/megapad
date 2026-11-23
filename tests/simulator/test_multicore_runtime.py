"""The hosted simulator's full cores: their rounds and the BIOS worker loop.

A runtime made over a topology of two to four full cores runs them in
rounds (simulator/cores.py, docs/multicore-simulator-plan.md, sections 4.2
to 4.4): each turn runs to its quantum, an IDL, a busy spinlock or the end
of its work, and a worker runs the jobs a full core sends it. Every case
here runs on both executors, and the outcomes must agree to the step.
"""

from __future__ import annotations

import pytest

from simulator.errors import ExecutionError
from simulator.platform import Topology, create_address_space
from simulator.runtime import (
    BlockedExecution,
    IdleWake,
    MegaForthRuntime,
    YieldedExecution,
)

EXECUTORS = ("python", "native")
QUANTUM = 1_000


def _runtime(cores: int, executor: str, *, quantum: int = QUANTUM,
             chiplet_ports: int = 0) -> MegaForthRuntime:
    memory = create_address_space(
        topology=Topology(full_cores=cores),
        external_size=0x4000 if chiplet_ports else 0,
        chiplet_ports=chiplet_ports,
    )
    return MegaForthRuntime(
        memory=memory, execution_backend=executor, turn_quantum_steps=quantum,
    )


def _cell(runtime: MegaForthRuntime, name: str, index: int = 0) -> int:
    return runtime.memory.read64(runtime.find(name).body_address + 8 * index)


def _output(runtime: MegaForthRuntime) -> bytes:
    return runtime.drain_uart_output()


SETUP = b"""
CREATE MARKS 4 CELLS ALLOT  MARKS 4 CELLS 0 FILL
VARIABLE COUNTER  0 COUNTER !
: MARK! ( n -- ) COREID CELLS MARKS + ! ;
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
: WAIT-ALL ( -- ) NCORES 1 ?DO I WAIT-FOR LOOP ;
"""


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_runtime_admits_one_to_four_full_cores_and_no_cluster(executor):
    for cores in (1, 2, 3, 4):
        runtime = _runtime(cores, executor)
        assert (runtime.cores is None) == (cores == 1)
        runtime.evaluate(b"VARIABLE N  NCORES N !  VARIABLE F  N-FULL F !")
        assert (_cell(runtime, "N"), _cell(runtime, "F")) == (cores, cores)
    memory = create_address_space(topology=Topology(full_cores=2, clusters=1))
    with pytest.raises(ValueError, match="no microcore cluster"):
        MegaForthRuntime(memory=memory, execution_backend=executor)


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_each_worker_runs_its_job_on_its_own_stacks(executor, cores):
    runtime = _runtime(cores, executor)
    runtime.evaluate(SETUP + b"""
CREATE SPS 4 CELLS ALLOT  CREATE RPS 4 CELLS ALLOT
: JOB ( -- ) COREID 10 + MARK!  SP@ COREID CELLS SPS + !  RP@ COREID CELLS RPS + ! ;
: MAIN ( -- ) NCORES 1 ?DO ['] JOB I WAKE-CORE LOOP WAIT-ALL ;
""")
    runtime.execute("MAIN")
    for core in range(1, cores):
        top = 0x100000 - core * 0x10000
        assert _cell(runtime, "MARKS", core) == 10 + core
        assert top - 0x10000 < _cell(runtime, "SPS", core) <= top - 0x8000
        assert top - 0x8000 < _cell(runtime, "RPS", core) <= top
        assert runtime.cores.job(core) == 0
        assert runtime.cores.state(core) == "idle"


@pytest.mark.parametrize("executor", EXECUTORS)
def test_wake_core_refuses_the_caller_and_absent_cores_without_consuming(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(b": JOB ;")
    for target in (0, 2, 1 << 63):
        context = runtime.new_context()
        context.data.push(runtime.find("JOB").xt)
        context.data.push(target)
        with pytest.raises(ExecutionError, match="full core other than the caller"):
            runtime.execute("WAKE-CORE", context=context)
        assert context.data.snapshot() == (runtime.find("JOB").xt, target)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_an_ipi_send_from_a_full_core_is_a_job_and_others_are_wakeups(executor):
    runtime = _runtime(3, executor, chiplet_ports=1)
    runtime.evaluate(SETUP + b"""
: JOB ( -- ) 7 MARK! ;
: MAIN ( -- ) ['] JOB 1 IPI-SEND 1 WAIT-FOR  2 MBOX@ ;
""")
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == 7
    assert runtime.cores.job(1) == 0
    # The worker took the job and acknowledged its sender.
    assert runtime.memory.mmio.mailbox.table.pending_mask(1) == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_mailbox_words_read_and_write_the_callers_own_state(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
VARIABLE SEEN  VARIABLE WORD1
: JOB ( -- ) IPI-STATUS SEEN !  MBOX@ WORD1 !  0 IPI-ACK  IPI-STATUS 1 CELLS MARKS + ! ;
: MAIN ( -- ) 1234 MBOX!  ['] JOB 1 WAKE-CORE 1 WAIT-FOR  MBOX@ 2 CELLS MARKS + ! ;
""")
    runtime.execute("MAIN")
    # The worker's handler took core 0's signal before the job ran.
    assert _cell(runtime, "SEEN") == 0
    assert _cell(runtime, "WORD1") == runtime.find("JOB").xt
    assert _cell(runtime, "MARKS", 2) == runtime.find("JOB").xt


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_a_spinlock_serializes_a_shared_counter(executor, cores):
    runtime = _runtime(cores, executor)
    runtime.evaluate(SETUP + b"""
: ADDS ( -- ) 500 0 DO BEGIN 5 SPIN@ 0= UNTIL COUNTER @ 1+ COUNTER ! 5 SPIN! LOOP ;
: MAIN ( -- ) NCORES 1 ?DO ['] ADDS I WAKE-CORE LOOP ADDS WAIT-ALL ;
""")
    runtime.execute("MAIN")
    assert _cell(runtime, "COUNTER") == 500 * cores
    assert all(runtime.spinlocks.owner(lock) is None for lock in range(16))


@pytest.mark.parametrize("executor", EXECUTORS)
def test_rounds_visit_awake_cores_in_order_and_each_turn_keeps_its_quantum(executor):
    runtime = _runtime(3, executor, quantum=200)
    runtime.evaluate(SETUP + b"""
CREATE TRACE 64 ALLOT  VARIABLE AT  0 AT !
: NOTE ( -- ) COREID TRACE AT @ + C!  1 AT +! ;
: BUSY ( -- ) 3 0 DO NOTE 400 0 DO LOOP LOOP ;
: MAIN ( -- ) ['] BUSY 1 WAKE-CORE ['] BUSY 2 WAKE-CORE BUSY WAIT-ALL ;
""")
    runtime.execute("MAIN")
    trace = runtime.memory.read_bytes(runtime.find("TRACE").body_address, 9)
    # Between notes each BUSY spends about two 200-step turns, so the cores
    # take their notes in rounds: core 0's turn, then core 1's, then core 2's.
    assert trace == bytes([0, 1, 2, 0, 1, 2, 0, 1, 2])
    turns = [runtime.cores.scheduler.turns(core) for core in range(3)]
    assert min(turns) >= 6


@pytest.mark.parametrize("executor", EXECUTORS)
def test_every_turn_of_a_round_starts_at_the_rounds_time(executor):
    runtime = _runtime(3, executor)
    runtime.evaluate(SETUP + b"""
CREATE CLOCKS 4 CELLS ALLOT
: STAMP ( -- ) CYCLES COREID CELLS CLOCKS + ! ;
: MAIN ( -- ) ['] STAMP 1 WAKE-CORE ['] STAMP 2 WAKE-CORE WAIT-ALL ;
""")
    runtime.execute("MAIN")
    first, second = _cell(runtime, "CLOCKS", 1), _cell(runtime, "CLOCKS", 2)
    # Both jobs began in the round after core 0's turn sent them.
    assert first == second


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_faulting_job_reports_its_core_and_leaves_the_worker_usable(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: BAD ( -- ) 1 MARK!  1 0 /  2 MARK! ;
: GOOD ( -- ) 3 MARK! ;
: MAIN ( -- ) ['] BAD 1 WAKE-CORE 1 WAIT-FOR ;
""")
    _output(runtime)
    runtime.execute("MAIN")
    assert b"*** DIVIDE BY ZERO CORE=01" in _output(runtime)
    assert _cell(runtime, "MARKS", 1) == 1
    runtime.evaluate(b": MAIN2 ['] GOOD 1 WAKE-CORE 1 WAIT-FOR ;")
    runtime.execute("MAIN2")
    assert _cell(runtime, "MARKS", 1) == 3


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("ending", ("ABORT", 'TRUE ABORT" worker gave up"'))
def test_an_abort_ends_the_job_and_runs_the_quit_callback_on_the_worker(executor, ending):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + f"""
VARIABLE QUITS  0 QUITS !  VARIABLE QUIT-CORE  -1 QUIT-CORE !
: COUNTED ( -- ) 1 QUITS +!  COREID QUIT-CORE ! ;  ' COUNTED QUIT-XT!
: ENDER ( -- ) 7 MARK! 1 2 3 {ending} 99 MARK! ;
: MAIN ( -- ) 11 22 33 ['] ENDER 1 WAKE-CORE 1 WAIT-FOR ;
""".encode())
    _output(runtime)
    context = runtime.main_context
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == 7
    assert (_cell(runtime, "QUITS"), _cell(runtime, "QUIT-CORE")) == (1, 1)
    assert runtime.cores.job(1) == 0
    assert context.data.snapshot()[-3:] == (11, 22, 33)
    if "ABORT\"" in ending:
        assert b"worker gave up" in _output(runtime)


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize(
    "body",
    (
        b"5 ,", b"8 ALLOT", b"S\" 1 2 +\" EVALUATE", b"KEY DROP", b"KEY? DROP",
        # ' parses the next word of core 0's input when it runs.
        b"' DUP DROP",
    ),
)
def test_a_worker_reaching_core_zeros_services_takes_the_illegal_fault(executor, body):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b": JOB ( -- ) 1 MARK! " + body + b" 2 MARK! ;"
                     + b" : MAIN ( -- ) ['] JOB 1 WAKE-CORE 1 WAIT-FOR ;")
    here = runtime.dictionary.here
    _output(runtime)
    runtime.execute("MAIN")
    assert b"*** ILLEGAL INSTRUCTION CORE=01" in _output(runtime)
    assert _cell(runtime, "MARKS", 1) == 1
    assert runtime.dictionary.here == here


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_workers_ipi_ends_core_zeros_sleep_and_core_zero_takes_it(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: RING ( -- ) 5 MARK!  0 0 IPI-SEND ;
: SLEEPER ( -- ) ['] RING 1 WAKE-CORE  MS@ 1000000 + IDLE-UNTIL  IPI-STATUS 9 MARK! ;
""")
    result = runtime.run_until_blocked("SLEEPER")
    # Core 0 slept, the worker rang it in the same round, and its next turn
    # woke: nothing needs the host's wake.
    while isinstance(result, BlockedExecution):
        assert runtime.idle_wake_due
        receipt = runtime.deliver_idle_wake(result.suspension, IdleWake.INTERRUPT)
        result = runtime.resume(result.suspension, receipt)
    assert _cell(runtime, "MARKS", 1) == 5
    assert _cell(runtime, "MARKS", 0) == 9
    assert runtime.memory.mmio.mailbox.table.pending_mask(0) == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_workers_run_while_the_host_evaluates_a_line(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b": LONG ( -- ) 3000 0 DO 1 COUNTER +! LOOP ;")
    # The host's evaluation is core 0's outer interpreter: the worker runs
    # its job in the rounds between core 0's turns, so the line's wait ends.
    runtime.evaluate(b"' LONG 1 WAKE-CORE  1 WAIT-FOR  COUNTER @ MARK!",
                     step_budget=1_000_000)
    assert _cell(runtime, "COUNTER") == 3000
    assert _cell(runtime, "MARKS", 0) == 3000
    assert runtime.cores.job(1) == 0
    assert [core["state"] for core in runtime.core_status()] == ["idle", "idle"]


@pytest.mark.parametrize("executor", EXECUTORS)
def test_core_zeros_sleep_in_an_evaluated_line_ends_at_a_workers_ipi(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b": RING ( -- ) 500 0 DO LOOP  5 MARK!  0 0 IPI-SEND ;")
    runtime.evaluate(b"' RING 1 WAKE-CORE  MS@ 1000000 + IDLE-UNTIL  IPI-STATUS 9 + MARK!")
    assert _cell(runtime, "MARKS", 1) == 5
    # Core 0 woke at the worker's IPI and took it, as idle_until does.
    assert _cell(runtime, "MARKS", 0) == 9
    assert runtime.memory.mmio.mailbox.table.pending_mask(0) == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_an_evaluated_idl_that_nothing_can_end_fails_and_frees_core_zero(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP)
    # No input is queued and no other core is awake to signal core 0.
    with pytest.raises(ExecutionError, match="no other core awake"):
        runtime.evaluate(b"KEY DROP")
    runtime.evaluate(b"7 MARK!")
    assert _cell(runtime, "MARKS", 0) == 7
    assert [core["state"] for core in runtime.core_status()] == ["idle", "idle"]


@pytest.mark.parametrize("executor", EXECUTORS)
def test_input_given_during_a_round_arrives_at_its_end(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
VARIABLE SEEN
: WATCH ( -- ) BEGIN KEY? 0= WHILE REPEAT  CYCLES SEEN !  KEY MARK! ;
""")
    start = runtime.timer.counter
    # The host's quantum ends a tenth of the way into core 0's turn.
    result = runtime.run_until_blocked("WATCH", quantum_steps=QUANTUM // 10)
    assert isinstance(result, YieldedExecution)
    runtime.inject_uart_input(b"Z")
    assert runtime.uart_input_pending == 1
    while isinstance(result, YieldedExecution):
        result = runtime.resume_yielded(result.suspension)
    assert _cell(runtime, "MARKS", 0) == ord("Z")
    # The input reached the FIFO when the round ended, after core 0's whole
    # turn of QUANTUM steps, and not where the host's quantum stopped.
    assert _cell(runtime, "SEEN") >= start + QUANTUM


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_workers_run_while_a_guest_evaluate_waits_for_them(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: LONG ( -- ) 3000 0 DO 1 COUNTER +! LOOP ;
: LINE ( -- ) S" ' LONG 1 WAKE-CORE  1 WAIT-FOR  COUNTER @ MARK!" EVALUATE ;
""")
    # The wait runs inside the guest EVALUATE, which cannot detach to the
    # host: core 0's turns end in place and the worker takes its turns.
    runtime.execute("LINE", step_budget=1_000_000)
    assert _cell(runtime, "COUNTER") == 3000
    assert _cell(runtime, "MARKS", 0) == 3000
    assert runtime.cores.scheduler.turns(1) >= 2


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_sleep_inside_a_guest_evaluate_ends_core_zeros_turn(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: LONG ( -- ) 3000 0 DO 1 COUNTER +! LOOP ;
VARIABLE NAPS
: NAP-WAIT ( -- ) BEGIN 1 CORE-STATUS WHILE 1 NAPS +! MS@ 1000 + IDLE-UNTIL REPEAT ;
: LINE ( -- ) S" ' LONG 1 WAKE-CORE  NAP-WAIT  COUNTER @ MARK!" EVALUATE ;
""")
    runtime.execute("LINE", step_budget=1_000_000)
    assert _cell(runtime, "MARKS", 0) == 3000
    # Each sleep ended core 0's turn and gave the worker its turn, so core 0
    # slept about once a round instead of spinning through each turn.
    assert 1 <= _cell(runtime, "NAPS") <= runtime.cores.scheduler.turns(1) + 1


def _nested_program(executor: str):
    runtime = _runtime(4, executor, quantum=300)
    runtime.evaluate(SETUP + b"""
: WORK ( -- ) 30 0 DO COREID 1+ COUNTER +! 50 0 DO LOOP LOOP ;
: SPREAD ( -- ) NCORES 1 ?DO ['] WORK I WAKE-CORE LOOP ;
: LINE ( -- ) S" SPREAD WORK WAIT-ALL" EVALUATE ;
""")
    result = runtime.execute("LINE")
    memory = runtime.memory.read_bytes(0, 0x100000)
    steps = [runtime.cores.steps(core) for core in range(4)]
    return result.semantic_steps, steps, memory, runtime.timer.counter


def test_turns_inside_a_guest_evaluate_are_the_same_twice_and_on_both_executors():
    first = _nested_program("native")
    assert _nested_program("native") == first
    assert _nested_program("python") == first


@pytest.mark.parametrize("executor", EXECUTORS)
def test_run_rounds_lets_workers_finish_after_core_zeros_dispatch(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: LONG ( -- ) 5000 0 DO 1 COUNTER +! LOOP ;
: KICK ( -- ) ['] LONG 1 WAKE-CORE ;
""")
    runtime.execute("KICK")
    assert runtime.cores.job(1) != 0
    rounds = runtime.run_rounds()
    assert rounds > 1
    assert _cell(runtime, "COUNTER") == 5000
    assert runtime.cores.job(1) == 0
    assert runtime.run_rounds() == 0


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_rollback_is_refused_while_a_suspended_job_runs_inside_it(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
VARIABLE SAVED-HERE  VARIABLE SAVED-LATEST
HERE SAVED-HERE !  LATEST SAVED-LATEST !
: FOREVER ( -- ) BEGIN 1 COUNTER +! AGAIN ;
: KICK ( -- ) ['] FOREVER 1 WAKE-CORE ;
""")
    runtime.execute("KICK")
    runtime.run_rounds(max_rounds=2)
    with pytest.raises(ExecutionError, match="suspended job"):
        runtime.evaluate(b"SAVED-HERE @ SAVED-LATEST @ DICT-ROLLBACK")


def _program(executor: str, cores: int):
    runtime = _runtime(cores, executor, quantum=300)
    runtime.evaluate(SETUP + b"""
CREATE TRACE 256 ALLOT  VARIABLE AT  0 AT !
: NOTE ( -- ) BEGIN 6 SPIN@ 0= UNTIL COREID TRACE AT @ + C! 1 AT +! 6 SPIN! ;
: WORK ( -- ) 20 0 DO NOTE COREID 40 * 0 ?DO LOOP LOOP ;
: MAIN ( -- ) NCORES 1 ?DO ['] WORK I WAKE-CORE LOOP WORK WAIT-ALL ;
""")
    result = runtime.execute("MAIN")
    memory = runtime.memory.read_bytes(0, 0x100000)
    steps = [runtime.cores.steps(core) for core in range(cores)]
    return result.semantic_steps, steps, memory, runtime.timer.counter


@pytest.mark.parametrize("cores", (2, 4))
def test_a_run_is_the_same_twice_and_on_both_executors(cores):
    first = _program("native", cores)
    assert _program("native", cores) == first
    assert _program("python", cores) == first


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_session_runs_the_workers_while_core_zero_waits_for_input(executor):
    from simulator.session import SimulatorMachineSession

    runtime = _runtime(2, executor)
    runtime.evaluate(SETUP + b"""
: LONG ( -- ) 20000 0 DO 1 COUNTER +! LOOP ;
: ENTRY ( -- ) ['] LONG 1 WAKE-CORE  KEY MARK! ;
""")
    with SimulatorMachineSession(runtime, "ENTRY", cols=20, rows=4) as session:
        session.boot()
        session.run_until_idle()
        # Core 0 waits in KEY; the worker ran its job to its end meanwhile.
        assert session.idle and not session.halted
        assert _cell(runtime, "COUNTER") == 20000
        assert runtime.cores.job(1) == 0
        status = runtime.core_status()
        assert [core["state"] for core in status] == ["asleep", "idle"]
        assert status[1]["semantic_steps"] > 20000
        session.send_text(b"A")
        session.run_until_idle()
        assert session.halted
        assert _cell(runtime, "MARKS", 0) == ord("A")
