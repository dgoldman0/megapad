"""Each full core's own tile engine, accumulator, TACC and FPCSR.

On the machine every full core has its own tile engine with its
accumulator, its own full-width TACC and its own FPCSR (docs/multicore-
simulator-plan.md, section 4.2). On the simulator the runtime's services
hold the running core's state, and each turn switch puts one core's away
and brings the next core's in (simulator/cores.py). The jobs here burn
steps between setting a register and using it, so that with a small turn
quantum every core's turns interleave with the others' at that point.
"""

from __future__ import annotations

import pytest

from simulator.platform import Topology, create_address_space
from simulator.runtime import MegaForthRuntime

EXECUTORS = ("python", "native")
QUANTUM = 300

SETUP = b"""
CREATE MARKS 4 CELLS ALLOT  MARKS 4 CELLS 0 FILL
: MARK! ( n -- ) COREID CELLS MARKS + ! ;
: WAIT-FOR ( core -- ) BEGIN DUP CORE-STATUS 0= UNTIL DROP ;
: WAIT-ALL ( -- ) NCORES 1 ?DO I WAIT-FOR LOOP ;
: BURN ( -- ) 120 0 DO LOOP ;
\\ Longer than a turn: another core's turn always comes in between.
: LONG-BURN ( -- ) 600 0 DO LOOP ;
\\ Four 64-byte tiles a core: two sources, a destination, and spare.
CREATE TILES 1024 ALLOT  TILES 1024 0 FILL
: TILE ( n -- addr ) 64 * COREID 256 * + TILES + ;
"""


def _runtime(cores: int, executor: str) -> MegaForthRuntime:
    memory = create_address_space(topology=Topology(full_cores=cores))
    runtime = MegaForthRuntime(
        memory=memory, execution_backend=executor, turn_quantum_steps=QUANTUM,
    )
    runtime.evaluate(SETUP)
    return runtime


def _cell(runtime: MegaForthRuntime, name: str, index: int = 0) -> int:
    return runtime.memory.read64(runtime.find(name).body_address + 8 * index)


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_each_core_keeps_its_own_tile_registers_across_turns(executor, cores):
    runtime = _runtime(cores, executor)
    runtime.evaluate(b"""
: FILL-SOURCES ( -- ) 0 TILE 64 COREID 1+ FILL  1 TILE 64 2 FILL ;
\\ Each pass sets the core's registers, lets the other cores run their
\\ turns, then adds: the sum is right only with the core's own registers.
: TJOB ( -- ) FILL-SOURCES 0  20 0 DO
    0 TMODE!  0 TILE TSRC0!  1 TILE TSRC1!  2 TILE TDST!
    BURN TADD  2 TILE 63 + C@ COREID 3 + = IF 1+ THEN
  LOOP MARK! ;
: MAIN ( -- ) NCORES 1 ?DO ['] TJOB I WAKE-CORE LOOP TJOB WAIT-ALL ;
""")
    runtime.execute("MAIN")
    assert [_cell(runtime, "MARKS", core) for core in range(cores)] == [20] * cores


@pytest.mark.parametrize("executor", EXECUTORS)
def test_each_core_has_its_own_accumulator(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(b"""
\\ TSUM leaves a tile's sum in the core's ACC0.
: AJOB ( -- ) 0 TILE 64 COREID 1+ FILL  0 TMODE!  0 TILE TSRC0!
    TSUM LONG-BURN ACC@ MARK! ;
: MAIN ( -- ) ['] AJOB 1 WAKE-CORE AJOB 1 WAIT-FOR ;
""")
    runtime.execute("MAIN")
    assert (_cell(runtime, "MARKS", 0), _cell(runtime, "MARKS", 1)) == (64, 128)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_each_core_has_its_own_fpcsr(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(b"""
: FJOB ( -- ) COREID 1+ FPCSR!  LONG-BURN  FPCSR@ MARK! ;
: MAIN ( -- ) ['] FJOB 1 WAKE-CORE FJOB 1 WAIT-FOR ;
""")
    runtime.execute("MAIN")
    assert (_cell(runtime, "MARKS", 0), _cell(runtime, "MARKS", 1)) == (1, 2)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_each_core_accumulates_in_its_own_tacc(executor):
    runtime = _runtime(2, executor)
    # Aligned tiles a core, below every core's stacks: two sources and the
    # 256-byte image the accumulator stores.
    runtime.evaluate(b"""
: AREA ( n -- addr ) 64 * COREID 4096 * + 262144 + ;
: TJOB ( -- ) 0 AREA 64 COREID 1+ FILL  1 AREA 64 2 FILL
    TACC-CLAIM? DROP 0 TMODE! TACC-CLEAR  0 AREA TSRC0! 1 AREA TSRC1! TAMAC
    LONG-BURN  4 AREA TDST! TACC-STORE TACC-RELEASE  4 AREA L@ MARK! ;
: MAIN ( -- ) ['] TJOB 1 WAKE-CORE TJOB 1 WAIT-FOR ;
""")
    runtime.execute("MAIN")
    # Each lane holds the core's own product: 1 x 2 on core 0, 2 x 2 on
    # core 1, though core 1 cleared and accumulated in between.
    assert (_cell(runtime, "MARKS", 0), _cell(runtime, "MARKS", 1)) == (2, 4)


@pytest.mark.parametrize("executor", EXECUTORS)
def test_a_tile_result_on_a_worker_equals_core_zeros(executor):
    runtime = _runtime(2, executor)
    runtime.evaluate(b"""
: SOURCES ( -- ) 0 TILE 64 0 DO I 3 * OVER I + C! LOOP DROP
    1 TILE 64 0 DO I 5 + OVER I + C! LOOP DROP ;
: RJOB ( -- ) SOURCES 0 TMODE! 0 TILE TSRC0! 1 TILE TSRC1! 2 TILE TDST!
    TMUL TDOT ACC@ MARK! ;
: MAIN ( -- ) ['] RJOB 1 WAKE-CORE RJOB 1 WAIT-FOR ;
""")
    runtime.execute("MAIN")
    tiles = runtime.find("TILES").body_address
    assert _cell(runtime, "MARKS", 1) == _cell(runtime, "MARKS", 0)
    assert (runtime.memory.read_bytes(tiles + 256 + 128, 64)
            == runtime.memory.read_bytes(tiles + 128, 64))


def _engine_program(executor: str):
    runtime = _runtime(4, executor)
    runtime.evaluate(b"""
: FILL-SOURCES ( -- ) 0 TILE 64 COREID 1+ FILL  1 TILE 64 2 FILL ;
: EJOB ( -- ) FILL-SOURCES COREID FPCSR!  10 0 DO
    0 TMODE! 0 TILE TSRC0! 1 TILE TSRC1! 2 TILE TDST! BURN TADD TSUM
  LOOP ACC@ FPCSR@ + MARK! ;
: MAIN ( -- ) NCORES 1 ?DO ['] EJOB I WAKE-CORE LOOP EJOB WAIT-ALL ;
""")
    result = runtime.execute("MAIN")
    memory = runtime.memory.read_bytes(0, 0x100000)
    steps = [runtime.cores.steps(core) for core in range(4)]
    return result.semantic_steps, steps, memory, runtime.timer.counter


def test_a_run_with_engines_is_the_same_twice_and_on_both_executors():
    first = _engine_program("native")
    assert _engine_program("native") == first
    assert _engine_program("python") == first


# ---------------------------------------------------------------------------
#  Services keyed by the calling core
# ---------------------------------------------------------------------------
#
# SHA-2 and CRC keep a transaction for each core (CRC for each core and
# task), so a core's hash survives the other cores' hashes in between.

HASH_SETUP = b"""
CREATE MSGS 64 ALLOT  CREATE DIGESTS 128 ALLOT
: MY-MSG ( -- addr ) COREID 16 * MSGS + ;
: HJOB ( -- ) MY-MSG 16 COREID 65 + FILL
    SHA256-INIT DROP  MY-MSG 16 SHA256-UPDATE DROP  LONG-BURN
    COREID 32 * DIGESTS + SHA256-FINAL MARK! ;
: MAIN ( -- ) NCORES 1 ?DO ['] HJOB I WAKE-CORE LOOP HJOB WAIT-ALL ;
"""


@pytest.mark.parametrize("executor", EXECUTORS)
@pytest.mark.parametrize("cores", (2, 4))
def test_each_core_keeps_its_own_sha256_transaction(executor, cores):
    import hashlib

    runtime = _runtime(cores, executor)
    runtime.evaluate(HASH_SETUP)
    runtime.execute("MAIN")
    digests = runtime.find("DIGESTS").body_address
    for core in range(cores):
        assert _cell(runtime, "MARKS", core) == 0
        expected = hashlib.sha256(bytes([65 + core]) * 16).digest()
        assert runtime.memory.read_bytes(digests + 32 * core, 32) == expected


CRC_JOB = b"""
: CJOB ( -- ) 0 CRC-MODE! DROP CRC-RESET DROP
    8 0 DO COREID 1+ I * CRC-FEED LOOP  LONG-BURN  CRC-FINAL@ MARK! ;
"""


def test_each_core_keeps_its_own_crc_transaction():
    runtime = _runtime(4, "native")
    runtime.evaluate(CRC_JOB + b": MAIN ( -- ) NCORES 1 ?DO ['] CJOB I WAKE-CORE LOOP CJOB WAIT-ALL ;")
    runtime.execute("MAIN")
    interleaved = [_cell(runtime, "MARKS", core) for core in range(4)]
    # The same feeds one core at a time, with no other core in between.
    alone = _runtime(4, "native")
    alone.evaluate(CRC_JOB + b": ONE ( core -- ) ['] CJOB SWAP WAKE-CORE ;")
    for core in range(1, 4):
        alone.evaluate(f"{core} ONE {core} WAIT-FOR".encode())
    alone.execute("CJOB")
    assert interleaved == [_cell(alone, "MARKS", core) for core in range(4)]
    assert len(set(interleaved)) == 4
