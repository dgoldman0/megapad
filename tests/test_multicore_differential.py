"""KDOS's multicore words on the emulator and on the simulator, one to four
full cores.

The same KDOS source runs on both machines (docs/multicore-simulator-plan.md,
stage 3): the emulator runs the BIOS's worker loop on its cores in
lock-step, and the simulator runs its cores in rounds. For a program whose
outcome its synchronization fixes, through the worker slots, spinlocks,
IPIs and per-core buffers, the two must print the same text. Each case
prints what it owns, its buffers and the final worker slots, between
markers; per-core steps and cycles stay out of it, as section 10 of the
simulator contract leaves them. The BIOS's fault reports name the program
counter, which the simulator does not have, so the comparison drops it.
Each case also runs on a second native simulator and on the Python
executor, and the three simulators must agree exactly: memory, output, the
clock and each core's semantic steps.

KDOS's ALL-CORES-WAIT, ALL-FULL-WAIT, BARRIER and SCHED-ALL count with a
plain DO from 1 to the core count, which never ends on one core; their
cases run on two and four cores. KDOS's MSG-SEND stores its arguments in
shared variables before it takes lock 7, so senders on several cores at
once race on the emulator; the messages case sends from one worker at a
time. The BIOS's MICRO? answers 255 for true where the simulator's answers
-1, so the sysinfo case compares its truth (section 9 of the plan records
both defects).
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from pathlib import Path

import pytest

from asm import assemble
from simulator.platform import Topology, create_address_space
from simulator.runtime import (
    BlockedExecution,
    IdleWake,
    MegaForthRuntime,
    YieldedExecution,
)
from system import MegapadSystem


ROOT = Path(__file__).resolve().parents[1]
EXTERNAL_MIB = 64
_PROGRAM_COUNTER = re.compile(rb" PC=[0-9A-F]{16}")
_BETWEEN = re.compile(rb"<<(.*?)>>", re.S)

# name: (minimum cores, definitions, body)
CASES = {
    "sysinfo": (1, b"", b"""NCORES . N-FULL . N-FULL-CORES . COREID .
0 MICRO? 0<> . 1 MICRO? 0<> . NCORES MICRO? 0<> ."""),
    "cores": (1, b"", b"CORES"),
    "dispatch": (2, b"""
CREATE DMARKS 4 CELLS ALLOT  DMARKS 4 CELLS 0 FILL
: DJOB ( -- ) COREID 10 + COREID CELLS DMARKS + ! ;
""", b"""NCORES 1 ?DO ['] DJOB I CORE-RUN LOOP  ALL-CORES-WAIT
4 0 DO I CELLS DMARKS + @ . LOOP  BARRIER  ALL-FULL-WAIT"""),
    "wait": (1, b"""
CREATE WMARKS 4 CELLS ALLOT  WMARKS 4 CELLS 0 FILL
: WJOB ( -- ) 500 0 DO LOOP COREID 1+ COREID CELLS WMARKS + ! ;
""", b"""NCORES 1 ?DO ['] WJOB I CORE-RUN LOOP  NCORES 1 ?DO I CORE-WAIT LOOP
4 0 DO I CELLS WMARKS + @ . LOOP"""),
    "lock": (1, b"""
VARIABLE LCOUNT
: LJOB ( -- ) 300 0 DO 12 LOCK LCOUNT @ 1+ LCOUNT ! 12 UNLOCK LOOP ;
""", b"""0 LCOUNT !  NCORES 1 ?DO ['] LJOB I CORE-RUN LOOP  LJOB
NCORES 1 ?DO I CORE-WAIT LOOP  LCOUNT @ ."""),
    "pipeline": (1, b"""
CREATE PMARKS 4 CELLS ALLOT  PMARKS 4 CELLS 0 FILL
: PSTEP0 ( -- ) COREID 1+ 0 CELLS PMARKS + ! ;
: PSTEP1 ( -- ) COREID 1+ 1 CELLS PMARKS + ! ;
: PSTEP2 ( -- ) COREID 1+ 2 CELLS PMARKS + ! ;
: PSTEP3 ( -- ) COREID 1+ 3 CELLS PMARKS + ! ;
4 PIPELINE DPIPE
' PSTEP0 DPIPE P.ADD  ' PSTEP1 DPIPE P.ADD  ' PSTEP2 DPIPE P.ADD  ' PSTEP3 DPIPE P.ADD
""", b"""DPIPE P.RUN-PAR  4 0 DO I CELLS PMARKS + @ . LOOP"""),
    "queues": (1, b"""
VARIABLE QSUM
: QJOB ( -- ) COREID 1+ QSUM +! ;
""", b"""0 QSUM !  NCORES 0 ?DO ['] QJOB I RQ-PUSH ['] QJOB I RQ-PUSH LOOP
NCORES 0 ?DO I RQ-COUNT . LOOP  NCORES 0 ?DO I SCHED-CORE LOOP  QSUM @ ."""),
    "sched-all": (2, b"""
VARIABLE SSUM
: SJOB ( -- ) COREID 1+ 12 LOCK SSUM @ + SSUM ! 12 UNLOCK ;
""", b"""0 SSUM !  NCORES 0 ?DO ['] SJOB I RQ-PUSH ['] SJOB I RQ-PUSH LOOP
SCHED-ALL  SSUM @ ."""),
    # A message MSG-RECV takes leaves the receiving core's ID beneath its
    # type, sender and payload (section 6 of the simulator contract).
    "messages": (1, b"""
: MJOB ( -- ) MSG-USER COREID 100 * 0 MSG-SEND DROP ;
""", b"""NCORES 1 ?DO ['] MJOB I CORE-RUN I CORE-WAIT LOOP
BEGIN MSG-RECV WHILE . . . DROP REPEAT 2DROP DROP"""),
    "catch": (1, b"""
VARIABLE CRESULT
: THROWER ( -- ) 42 THROW ;
: CJOB ( -- ) ['] THROWER CATCH CRESULT ! ;
""", b"""NCORES 1 ?DO 0 CRESULT ! ['] CJOB I CORE-RUN I CORE-WAIT CRESULT @ . LOOP"""),
    "fault": (1, b"""
CREATE FMARKS 4 CELLS ALLOT  FMARKS 4 CELLS 0 FILL
: FJOB ( -- ) 1 COREID CELLS FMARKS + !  1 0 / DROP  2 COREID CELLS FMARKS + ! ;
: AJOB ( -- ) 3 COREID CELLS FMARKS + !  TRUE ABORT" worker gave up"
  4 COREID CELLS FMARKS + ! ;
""", b"""NCORES 1 ?DO ['] FJOB I CORE-RUN I CORE-WAIT LOOP  4 0 DO I CELLS FMARKS + @ . LOOP
NCORES 1 ?DO ['] AJOB I CORE-RUN I CORE-WAIT LOOP  4 0 DO I CELLS FMARKS + @ . LOOP"""),
    "wake": (1, b"""
VARIABLE IWOKE
: IJOB ( -- ) 0 0 IPI-SEND ;
""", b"""0 IWOKE !  NCORES 1 > IF
['] IJOB 1 CORE-RUN  MS@ 60000 + IDLE-UNTIL  1 IWOKE !  1 CORE-WAIT
THEN  IWOKE @ .  IPI-STATUS ."""),
}


# What each case of this module prints last: the worker slots, each
# nonzero while its core has a job.
WORKER_SLOTS = b"NCORES 0 ?DO I CORE-STATUS 0<> . LOOP"


def case_word(name: str) -> bytes:
    return b"_CASE-" + name.upper().encode()


def _definition(word: bytes, body: bytes, epilogue: bytes) -> bytes:
    """The case's word: BODY, then EPILOGUE, printed between markers."""

    lines = body.strip().splitlines()
    return (b": " + word + b" ( -- ) 60 EMIT 60 EMIT\n"
            + b"\n".join(lines) + b"\n" + epilogue + b" 62 EMIT 62 EMIT ;")


def _bios() -> bytes:
    with open(ROOT / "bios.asm", encoding="utf-8") as source:
        return assemble(source.read())


def _kdos() -> bytes:
    return (ROOT / "kdos.f").read_bytes()


class _Emulator:
    """The BIOS and KDOS on the emulator's cores, typed at core 0's
    prompt."""

    def __init__(self, cores: int, bios: bytes, kdos: bytes) -> None:
        self.system = MegapadSystem(
            ram_size=1 << 20, num_cores=cores, num_clusters=0,
            ext_mem_size=EXTERNAL_MIB << 20, worker_count=1,
        )
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        self.system.load_binary(0, bios)
        self.system.boot()
        lines = [
            line for line in kdos.splitlines()
            if line.strip() and not line.strip().startswith(b"\\")
        ]
        self.type(lines + [b"CHAR < EMIT CHAR K EMIT CHAR > EMIT"],
                  step_limit=600_000_000)
        assert b"<K>" in self._sent

    def type(self, lines: list[bytes], *, step_limit: int = 100_000_000) -> bytes:
        data = b"".join(line + b"\n" for line in lines)
        start = len(self._sent)
        position = 0
        steps = 0
        uart = self.system.uart
        cpu = self.system.cpu
        while steps < step_limit:
            if cpu.halted:
                break
            if cpu.idle and not uart.has_rx_data:
                if position >= len(data):
                    break
                end = data.find(b"\n", position) + 1 or len(data)
                uart.inject_input(data[position:end])
                position = end
                continue
            steps += max(self.system.run_batch(100_000), 1)
        assert position == len(data), "the emulator did not take every line"
        return bytes(self._sent[start:])

    def run_case(self, word: bytes, setup: bytes, body: bytes,
                 epilogue: bytes = b"") -> bytes:
        """Type SETUP and the case's word, then run it: what it printed."""

        definitions = (setup.strip().splitlines()
                       + _definition(word, body, epilogue).splitlines())
        self.type([line for line in definitions if line.strip()])
        return _report(self.type([word]))

    def run(self, name: str) -> bytes:
        _cores, setup, body = CASES[name]
        return self.run_case(case_word(name), setup, body, WORKER_SLOTS)


@dataclass(frozen=True)
class _Run:
    """What one case left on a simulator: the text it printed, and the
    machine state that the simulators must agree on exactly."""

    text: bytes
    core0_steps: int
    cores: tuple[tuple[object, ...], ...]
    counter: int
    memory: bytes


class _Simulator:
    """KDOS on the simulator's cores, each case's word a dispatch of core
    0's that runs the machine as a session would."""

    def __init__(self, cores: int, executor: str, kdos: bytes) -> None:
        memory = create_address_space(
            topology=Topology(full_cores=cores),
            external_size=EXTERNAL_MIB << 20,
        )
        self.runtime = MegaForthRuntime(memory=memory, execution_backend=executor)
        self.runtime.evaluate(kdos, source_name="kdos.f")
        self.runtime.drain_uart_output()

    def run_case(self, word: bytes, setup: bytes, body: bytes,
                 epilogue: bytes = b"") -> _Run:
        """Evaluate SETUP and the case's word, then run it as a session
        would: what it printed and the state it left."""

        runtime = self.runtime
        runtime.evaluate(setup + b"\n" + _definition(word, body, epilogue))
        runtime.drain_uart_output()
        result = runtime.run_until_blocked(word)
        steps = result.semantic_steps
        while isinstance(result, (BlockedExecution, YieldedExecution)):
            if isinstance(result, YieldedExecution):
                result = runtime.resume_yielded(result.suspension)
            else:
                assert runtime.idle_wake_due, "every core sleeps with nothing to wake it"
                receipt = runtime.deliver_idle_wake(result.suspension, IdleWake.INTERRUPT)
                result = runtime.resume(result.suspension, receipt)
            steps += result.semantic_steps
        runtime.run_rounds()
        return _Run(
            text=_report(runtime.drain_uart_output()),
            core0_steps=steps,
            cores=tuple(tuple(core.values()) for core in runtime.core_status()),
            counter=runtime.timer.counter,
            memory=_memory_digest(runtime.memory),
        )

    def run(self, name: str) -> _Run:
        _cores, setup, body = CASES[name]
        return self.run_case(case_word(name), setup, body, WORKER_SLOTS)


def _memory_digest(memory) -> bytes:
    """A digest of every ordinary region's contents, read a mebibyte at a
    time; a mebibyte of zeros contributes nothing, so untouched external
    memory costs only its reading."""

    chunk_size = 1 << 20
    zeros = bytes(chunk_size)
    digest = hashlib.blake2b(digest_size=32)
    for region in memory.regions:
        for offset in range(0, region.size, chunk_size):
            length = min(chunk_size, region.size - offset)
            chunk = memory.read_bytes(region.base + offset, length)
            if chunk != zeros[:length]:
                digest.update((region.base + offset).to_bytes(8, "little"))
                digest.update(chunk)
    return digest.digest()


def _report(output: bytes) -> bytes:
    found = _BETWEEN.search(output)
    assert found is not None, output[-400:]
    return _PROGRAM_COUNTER.sub(b"", found.group(1))


@pytest.fixture(scope="module")
def sources():
    return _bios(), _kdos()


@pytest.fixture(scope="module")
def machines(sources):
    """Machines kept for the module, so that each case runs on the state
    the cases before it left: the emulator, and three simulators of each
    topology, the native executor twice and the Python one once."""

    bios, kdos = sources
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


@pytest.mark.parametrize("cores", (1, 2, 4))
@pytest.mark.parametrize("name", tuple(CASES))
def test_kdos_multicore_words_agree_with_the_emulator(machines, cores, name):
    minimum, _setup, _body = CASES[name]
    if cores < minimum:
        pytest.skip(f"KDOS's {name} words never end on one core")
    expected = machines("emulator", cores).run(name)
    first = machines("native", cores).run(name)
    assert first.text == expected
    # The simulator is deterministic: the same case on the same history
    # leaves the same memory, output, clock and per-core steps, run again
    # and on the other executor.
    assert machines("native", cores, copy=1).run(name) == first
    assert machines("python", cores).run(name) == first
