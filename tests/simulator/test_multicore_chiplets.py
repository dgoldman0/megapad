"""Chiplet ports on the simulator's full cores.

A chiplet steps at the end of each round, before a core's read of a port
register or of its MBOX_STATUS, and, while every core sleeps, until it
rests or rings a core (docs/multicore-simulator-plan.md, section 4.7). A
worker takes a chiplet's signal as its IPI handler does: at once, when one
of its own accesses steps the chiplet into raising it, while core 0 keeps a
signal pending until it acknowledges it or sleeps. The test chiplet serves
one command for each doorbell and rings back the participant that rang
(shared/accel/chiplet_test_module.cpp). The ports' IDs follow the cores.
"""

from __future__ import annotations

import pytest

import _megaforth_native  # noqa: F401  (the ports are its model)
from simulator.chiplet import CHIPLET_OFFSET, MAILBOX_OFFSET
from simulator.memory import EXTERNAL_BASE, MMIO_BASE
from simulator.platform import Topology, create_address_space
from simulator.runtime import BlockedExecution, IdleWake, MegaForthRuntime

EXECUTORS = ("python", "native")
EXT_SIZE = 0x4000
WINDOW = MMIO_BASE + CHIPLET_OFFSET
MBOX = MMIO_BASE + MAILBOX_OFFSET
READY = 1 << 2
IMAGE = EXTERNAL_BASE
COMMAND = EXTERNAL_BASE + 0x100
REPLY = EXTERNAL_BASE + 0x1000
BOOT_MAGIC = int.from_bytes(b"TESTBOOT", "little")
MASK64 = (1 << 64) - 1
COMMAND_WORD = 0x0000_5EED_0000_0002


def _bring_up(port_id: int, control: int) -> bytes:
    return f"""
{WINDOW} CONSTANT CP
{MBOX} CONSTANT MBOX
: CP! ( x offset -- ) CP + ! ;
: CP@ ( offset -- x ) CP + @ ;
: LEND ( base limit rights slot -- )
    0xA0 CP!  ROT 0xA8 CP!  SWAP 0xB0 CP!  0xB8 CP! ;
: WAIT-READY ( -- ) BEGIN 0x88 CP@ {READY} AND UNTIL ;
: BRING-UP ( -- )
    {IMAGE} {IMAGE + 0x100} 1 0 LEND
    {IMAGE} 0xD8 CP!  40 0xE0 CP!
    {COMMAND} {COMMAND + 0x900} 1 1 LEND
    {REPLY} {REPLY + 0x1000} 3 2 LEND
    {control} 0x80 CP!
    WAIT-READY ;
: RING ( -- ) {port_id} MBOX 8 + C! ;
: SERVED ( -- n ) {REPLY + 8} C@ ;
\\ Ring, then poll the mailbox status, which steps the chiplet, until it
\\ has served the command; then the status as the caller sees it.
: ASK-POLLING ( -- status ) SERVED RING BEGIN MBOX 9 + C@ DROP SERVED OVER <> UNTIL
    DROP MBOX 9 + C@ ;
\\ Ring, then poll memory alone, which steps nothing.
: ASK-QUIETLY ( -- ) SERVED RING BEGIN SERVED OVER <> UNTIL DROP ;
CREATE MARKS 4 CELLS ALLOT  MARKS 4 CELLS 0 FILL
: MARK! ( n -- ) COREID CELLS MARKS + ! ;
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
""".encode()


ENABLE, DOORBELL_EN, BORROW_EN = 1, 1 << 3, 1 << 5
# Port 0's window into the memory its chiplet lends, and the test chiplet's
# lent range within it; a Bank 0 image for TACC to relay there.
LENT_WINDOW = 0x0100_0000_0000_0000 + 0x1000
INITIAL = 0x41400
COPY_FROM_LENT = 0x100


def _runtime(cores: int, executor: str, *, quantum: int = 1_000,
             borrow: bool = False) -> MegaForthRuntime:
    memory = create_address_space(
        topology=Topology(full_cores=cores),
        bank0_size=1 << 20,
        external_size=EXT_SIZE,
        chiplet_ports=1,
    )
    memory.write_bytes(IMAGE, b"".join(
        (word & MASK64).to_bytes(8, "little")
        for word in (BOOT_MAGIC, COMMAND, REPLY, COMMAND, 0)
    ))
    memory.write_bytes(COMMAND, b"".join(
        (word & MASK64).to_bytes(8, "little") for word in (COMMAND_WORD, 0xAAA, 0xBBB)
    ))
    runtime = MegaForthRuntime(
        memory=memory, execution_backend=executor, turn_quantum_steps=quantum,
    )
    runtime.chiplet_ports.attach_test_chiplet(0)
    control = ENABLE | DOORBELL_EN | (BORROW_EN if borrow else 0)
    runtime.evaluate(_bring_up(cores, control))
    runtime.execute("BRING-UP")
    return runtime


def _cell(runtime: MegaForthRuntime, name: str, index: int = 0) -> int:
    return runtime.memory.read64(runtime.find(name).body_address + 8 * index)


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_the_ports_ids_follow_the_cores(executor, cores):
    runtime = _runtime(cores, executor)
    assert runtime.chiplet_ports.first_port_id == cores
    assert (runtime.chiplet_ports.port_count_word >> 8) & 0xFF == cores
    assert runtime.memory.mmio.mailbox.table.is_participant(cores)


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_a_worker_takes_the_answer_its_own_access_raises(executor, cores):
    runtime = _runtime(cores, executor)
    runtime.evaluate(b": JOB ( -- ) ASK-POLLING MARK! ;"
                     b" : MAIN ( -- ) ['] JOB 1 WAKE-CORE 1 WAIT-FOR ;")
    runtime.execute("MAIN")
    # The read that stepped the chiplet into ringing core 1 back saw the
    # worker take the signal, as its IPI handler does.
    assert _cell(runtime, "MARKS", 1) == 0
    assert runtime.memory.mmio.mailbox.table.pending_mask(1) == 0
    assert runtime.memory.read64(REPLY) == ~COMMAND_WORD & MASK64


@pytest.mark.parametrize("executor", EXECUTORS)
def test_core_zero_keeps_the_answer_pending_until_it_acknowledges(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(b": MAIN ( -- ) ASK-POLLING MARK! ;")
    runtime.execute("MAIN")
    port_bit = 1 << 2
    assert _cell(runtime, "MARKS", 0) == port_bit
    assert runtime.memory.mmio.mailbox.table.pending_mask(0) == port_bit


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_chiplet_steps_at_the_end_of_each_round(executor):
    runtime = _runtime(2, executor)
    # The job polls memory alone, so only the rounds' ends step the chiplet.
    runtime.evaluate(b": JOB ( -- ) ASK-QUIETLY 7 MARK! ;"
                     b" : MAIN ( -- ) ['] JOB 1 WAKE-CORE 1 WAIT-FOR ;")
    runtime.execute("MAIN", step_budget=2_000_000)
    assert _cell(runtime, "MARKS", 1) == 7
    assert runtime.memory.read64(REPLY) == ~COMMAND_WORD & MASK64


@pytest.mark.parametrize("executor", EXECUTORS)
def test_while_every_core_sleeps_the_chiplet_steps_until_it_rings(executor):
    runtime = _runtime(2, executor)
    # One unit of work a step: the round's end alone cannot serve a command.
    runtime.chiplet_ports.set_budget(0, 1)
    runtime.evaluate(b": SLEEPY ( -- ) RING  MS@ 1000000 + IDLE-UNTIL  IPI-STATUS MARK! ;")
    result = runtime.run_until_blocked("SLEEPY")
    wakes = 0
    while isinstance(result, BlockedExecution):
        # The chiplet served the command and rang core 0 while every core
        # slept, so core 0's wake rule already holds.
        assert runtime.idle_wake_due
        receipt = runtime.deliver_idle_wake(result.suspension, IdleWake.INTERRUPT)
        result = runtime.resume(result.suspension, receipt)
        wakes += 1
    assert wakes == 1
    # Core 0's sleep took the signal that ended it, as idle_until does.
    assert _cell(runtime, "MARKS", 0) == 0
    assert runtime.memory.read64(REPLY) == ~COMMAND_WORD & MASK64


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_a_worker_relays_a_tacc_image_into_the_chiplets_memory(executor, cores):
    runtime = _runtime(cores, executor, borrow=True)
    image = bytes((index * 7 + 3) & 0xFF for index in range(256))
    runtime.memory.write_bytes(INITIAL, image)
    runtime.evaluate(f"""
: RJOB ( -- ) TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR
    {INITIAL} {LENT_WINDOW} TACC-RELAY MARK! TACC-RELEASE ;
: MAIN ( -- ) ['] RJOB 1 WAKE-CORE 1 WAIT-FOR ;
""".encode())
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == 0  # TACC-RELAY's OK
    # The chiplet copies the start of its lent memory into its reply.
    runtime.memory.write_bytes(COMMAND, (COPY_FROM_LENT | 32).to_bytes(8, "little"))
    runtime.evaluate(b": COPY-BACK ( -- ) ASK-POLLING DROP ;")
    runtime.execute("COPY-BACK")
    assert runtime.memory.read_bytes(REPLY + 24, 256) == image


def _chiplet_program(executor: str, cores: int):
    runtime = _runtime(cores, executor, quantum=300)
    runtime.evaluate(b"""
: JOB ( -- ) ASK-POLLING MARK! ASK-QUIETLY ;
: MAIN ( -- ) NCORES 1 ?DO ['] JOB I WAKE-CORE I WAIT-FOR LOOP ASK-QUIETLY ;
""")
    result = runtime.execute("MAIN")
    memory = runtime.memory.read_bytes(0, 0x100000) + runtime.memory.read_bytes(
        EXTERNAL_BASE, EXT_SIZE)
    steps = [runtime.cores.steps(core) for core in range(cores)]
    return result.semantic_steps, steps, memory, runtime.timer.counter


@pytest.mark.parametrize("cores", (2, 4))
def test_a_run_with_a_chiplet_is_the_same_twice_and_on_both_executors(cores):
    first = _chiplet_program("native", cores)
    assert _chiplet_program("native", cores) == first
    assert _chiplet_program("python", cores) == first
