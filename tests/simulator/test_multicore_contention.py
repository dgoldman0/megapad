"""Full cores that contend for the locks the BIOS's services take.

The BIOS's disk words poll lock 2 up to 1,048,576 times before they report
a timeout. On the simulator a disk word that finds another core holding
the lock ends its turn and runs again at the core's next turn, so the
holder runs meanwhile, until the BIOS's bound of polls, one a turn
(docs/multicore-simulator-plan.md, stage 4).
"""

from __future__ import annotations

import pytest

import simulator.runtime
from shared.storage import STORAGE_RESULT_TIMEOUT
from simulator.platform import Topology, create_address_space
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService

EXECUTORS = ("python", "native")
QUANTUM = 1_000
SECTOR = bytes((index * 5 + 1) & 0xFF for index in range(512))

SETUP = b"""
CREATE MARKS 4 CELLS ALLOT  MARKS 4 CELLS 0 FILL
: MARK! ( n -- ) COREID CELLS MARKS + ! ;
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
CREATE BUF 512 ALLOT  BUF 512 0 FILL
: READER ( -- ) BUF 0 1 DISK-READ-CHECKED MARK! DROP ;
\\ Core 0 holds lock 2 for several of its turns while core 1 reads.
: HOLDER ( turns -- ) 2 SPIN@ DROP  ['] READER 1 WAKE-CORE
    0 ?DO 1000 0 DO LOOP LOOP  2 SPIN!  1 WAIT-FOR ;
"""


def _runtime(executor: str) -> MegaForthRuntime:
    memory = create_address_space(topology=Topology(full_cores=2))
    runtime = MegaForthRuntime(
        memory=memory,
        execution_backend=executor,
        turn_quantum_steps=QUANTUM,
        storage=HostedStorageService(SECTOR),
    )
    runtime.evaluate(SETUP)
    return runtime


def _cell(runtime: MegaForthRuntime, name: str, index: int = 0) -> int:
    return runtime.memory.read64(runtime.find(name).body_address + 8 * index)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_disk_word_waits_across_turns_while_another_core_holds_lock_two(executor):
    runtime = _runtime(executor)
    runtime.evaluate(b": MAIN ( -- ) 4 HOLDER ;")
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == 0
    buffer = runtime.find("BUF").body_address
    assert runtime.memory.read_bytes(buffer, 512) == SECTOR
    assert runtime.spinlocks.owner(2) is None
    # The worker polled once a turn while core 0 held the lock.
    assert runtime.cores.scheduler.turns(1) >= 4


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_bios_bound_of_polls_ends_in_its_timeout(executor, monkeypatch):
    monkeypatch.setattr(simulator.runtime, "LOCK_POLL_BOUND", 3)
    runtime = _runtime(executor)
    runtime.evaluate(b": MAIN ( -- ) 8 HOLDER ;")
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == STORAGE_RESULT_TIMEOUT
    buffer = runtime.find("BUF").body_address
    assert runtime.memory.read_bytes(buffer, 512) == bytes(512)


# ---------------------------------------------------------------------------
#  The checked SHA-3 service's lock 8, and the bank's MMIO window
# ---------------------------------------------------------------------------
#
# The BIOS's checked SHA-3 words take lock 8 once and report the STATE
# status, 2, while another core holds it, rather than waiting; the
# simulator's service takes the same lock in the same bank.

SHA_SETUP = b"""
VARIABLE BUSY  VARIABLE FIRST  VARIABLE SECOND
CREATE DIGEST 64 ALLOT
: HOLD8 ( -- ) 8 SPIN@ MARK!  3000 0 DO LOOP  8 SPIN! ;
: MAIN ( -- ) ['] HOLD8 1 WAKE-CORE  1500 0 DO LOOP
    0 SHA3-BEGIN BUSY !  1 WAIT-FOR
    0 SHA3-BEGIN FIRST !  DIGEST SHA3-FINAL SECOND ! ;
"""


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_checked_sha3_transaction_takes_the_banks_lock_8(executor):
    runtime = _runtime(executor)
    runtime.evaluate(SHA_SETUP)
    runtime.execute("MAIN")
    assert _cell(runtime, "MARKS", 1) == 0  # core 1 took lock 8
    # While core 1 held it, core 0's transaction could not begin.
    assert _cell(runtime, "BUSY") == 2
    assert (_cell(runtime, "FIRST"), _cell(runtime, "SECOND")) == (0, 0)
    assert runtime.spinlocks.owner(8) is None
    # A transaction holds lock 8 as its core from BEGIN to its end.
    runtime.evaluate(b"0 SHA3-BEGIN FIRST !")
    assert runtime.spinlocks.owner(8) == 0
    runtime.evaluate(b"DIGEST SHA3-FINAL SECOND !")
    assert runtime.spinlocks.owner(8) is None


MMIO_LOCK_5 = 0xFFFF_FF00_0000_0600 + 4 * 5


@pytest.mark.parametrize("executor", EXECUTORS)
def test_the_banks_window_serves_each_core_as_its_requester(executor):
    runtime = _runtime(executor)
    runtime.evaluate(f"""
VARIABLE SEEN
: RAW-HOLD ( -- ) {MMIO_LOCK_5} C@ MARK!  3000 0 DO LOOP  1 {MMIO_LOCK_5 + 1} C! ;
: MAIN ( -- ) ['] RAW-HOLD 1 WAKE-CORE  1500 0 DO LOOP
    5 SPIN@ SEEN !  1 WAIT-FOR  5 SPIN@ MARK!  5 SPIN! ;
""".encode())
    runtime.execute("MAIN")
    # Core 1's raw acquire took lock 5 as core 1, so core 0's SPIN@ found
    # it busy; core 1's raw release freed it.
    assert _cell(runtime, "MARKS", 1) == 0
    assert _cell(runtime, "SEEN") == 1
    assert _cell(runtime, "MARKS", 0) == 0
    assert runtime.spinlocks.owner(5) is None
