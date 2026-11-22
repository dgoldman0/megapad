"""Managed data's conformance tests, through the real code at both ends.

docs/managed-data.md section 15 lists the tests.  MegaPad's side is its
chiplet system module, chiplet.f, loaded after KDOS; LM1's side is LM1's
chiplet module, whose stand-in for LM1's runtime serves managed data
(design/chiplet-module.md in the lm1 repository, decision A).  Each case
boots a fresh machine, loads KDOS and chiplet.f, brings LM1 up behind port 0
with CHIP-BOOT, lets managed data start with OBJ-START, and then runs Forth
that uses the OBJ- words.  The same Forth runs on the emulator's machine
and on the hosted simulator, and each case requires the same reports from
both.  A report is a number printed between angle brackets.

The module's test hooks (lm1_chiplet.h) reach the model the port attached
through LM1's binding: a hold kept ACTIVE, a word written as LM1 would
write it, and a thread of LM1 code started at an address.

Cases that need a second MegaPad core run on the emulator alone, since the
hosted simulator has one core until the multicore plan's stage 3.

The emulator's clock follows its cycles and advances to a sleeping core's
deadline when every core sleeps; the simulator's clock advances by a fixed
step each time MS@ reads it, so that deadlines pass in both.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

import pytest

MEGAPAD = Path(__file__).resolve().parents[1]
LM1 = (Path(os.environ["LM1_ROOT"]) if os.environ.get("LM1_ROOT")
       else MEGAPAD.parent / "lm1")
if not (LM1 / "emu" / "lm1" / "chiplet.py").is_file():
    pytest.skip("no LM-1 checkout at %s; LM1_ROOT names one" % LM1,
                allow_module_level=True)
sys.path.insert(0, str(LM1 / "emu"))
os.environ["MEGAPAD_ROOT"] = str(MEGAPAD)

from lm1 import chiplet as lm1_chiplet  # noqa: E402

from asm import assemble  # noqa: E402
from simulator.errors import ForthAbort  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from system import MegapadSystem  # noqa: E402

LIBRARY = lm1_chiplet.ensure_library()

EXT_SIZE = 16 << 20
KDOS = (MEGAPAD / "kdos.f").read_text(encoding="utf-8")
CHIPLET = (MEGAPAD / "chiplet.f").read_text(encoding="utf-8")
# LM1's stand-in, sized small: a 64 KiB first block and 1 KiB regions.
BASE_CONFIG = "runtime=1,mem=64K,heap_block=0x10000,region_bytes=0x400"
MASK64 = (1 << 64) - 1

_REPORT = re.compile(r"<(-?\d+) >")
_DEFINITIONS = [
    ': REPORT ( x -- ) ." <" . ." >" ;',
]


def _source_lines(text: str) -> list[str]:
    """The lines of a source file a REPL needs: comments and blanks go."""
    lines = []
    for line in text.splitlines():
        stripped = line.strip()
        if stripped and not stripped.startswith("\\"):
            lines.append(line)
    return lines


_BIOS: bytes | None = None


def _bios() -> bytes:
    global _BIOS
    if _BIOS is None:
        _BIOS = assemble((MEGAPAD / "bios.asm").read_text(encoding="utf-8"))
    return _BIOS


_HOOK_IDS = iter(range(1, 1 << 30))


class _Machine:
    """What both machines offer a case."""

    lm1: lm1_chiplet.Model

    def run(self, lines: list[str]) -> list[int]:
        raise NotImplementedError

    def reports(self, lines: list[str]) -> list[int]:
        return self.run(lines)


class Emulated(_Machine):
    """The emulator's machine: BIOS, KDOS and chiplet.f on CORES full
    cores, with LM1 on port 0."""

    name = "emulator"

    def __init__(self, config: str = "", cores: int = 2):
        self.hook_id = next(_HOOK_IDS)
        system = MegapadSystem(ram_size=1 << 20, num_cores=cores, num_clusters=0,
                               ext_mem_size=EXT_SIZE)
        self._out = bytearray()
        system.uart.on_tx = self._out.append
        system.load_binary(0, _bios())
        system.boot()
        system.attach_chiplet(0, LIBRARY, "%s,hook_id=%d%s" % (
            BASE_CONFIG, self.hook_id, "," + config if config else ""))
        self.system = system
        self.lm1 = lm1_chiplet.attached_model(self.hook_id)
        assert self.lm1 is not None
        self._feed(_source_lines(KDOS))
        self._feed(_source_lines(CHIPLET) + _DEFINITIONS)

    def _feed(self, lines: list[str], max_steps: int = 2_000_000_000) -> str:
        data = "".join(line + "\n" for line in lines).encode()
        start = len(self._out)
        position = 0
        steps = 0
        system = self.system
        while steps < max_steps:
            cpu = system.cpu
            if cpu.halted:
                break
            if system.all_idle_or_halted and not system.uart.has_rx_data:
                if cpu.wake_ms == 0:
                    # Core 0 waits for input at the prompt.
                    if position >= len(data):
                        break
                    end = data.find(b"\n", position) + 1 or len(data)
                    system.uart.inject_input(data[position:end])
                    position = end
                    continue
                # A core sleeps until a deadline: time passes to it.
                delay = system.idle_wake_delay_s()
                system.advance_idle_time(0.001 if delay is None else max(delay, 1e-5))
                continue
            steps += max(system.run_batch(100_000), 1)
        assert position == len(data), "the emulator did not take every line"
        return bytes(self._out[start:]).decode("latin-1")

    def run(self, lines: list[str]) -> list[int]:
        return [int(v) for v in _REPORT.findall(self._feed(lines))]

    def settle(self, rounds: int = 20) -> None:
        """Let LM1 work while MegaPad's cores wait at the prompt."""
        for _ in range(rounds):
            self.system.run_batch(10_000)
            self.system.native._service_chiplet_ports(True) if hasattr(
                self.system, "native") else None


class Hosted(_Machine):
    """The hosted simulator: KDOS and chiplet.f on its one core, with LM1
    on its one port."""

    name = "simulator"

    def __init__(self, config: str = ""):
        self.hook_id = next(_HOOK_IDS)
        memory = create_one_core_address_space(bank0_size=1 << 20, external_size=EXT_SIZE,
                                               chiplet_ports=1)
        runtime = MegaForthRuntime(memory=memory)
        runtime.chiplet_ports.attach_module(0, LIBRARY, "%s,hook_id=%d%s" % (
            BASE_CONFIG, self.hook_id, "," + config if config else ""))
        self.lm1 = lm1_chiplet.attached_model(self.hook_id)
        assert self.lm1 is not None
        # MS@ moves on 20 microseconds at each read, so a wait's deadline
        # passes while the guest polls.
        now = [0]

        def monotonic_ns() -> int:
            now[0] += 20_000
            return now[0]

        runtime.rtc.bind_monotonic_clock(monotonic_ns)
        self.runtime = runtime
        runtime.evaluate(KDOS.encode(), source_name="kdos.f")
        runtime.evaluate(CHIPLET.encode(), source_name="chiplet.f")
        for line in _DEFINITIONS:
            runtime.evaluate(line.encode())
        runtime.drain_uart_output()

    def run(self, lines: list[str]) -> list[int]:
        for line in lines:
            try:
                self.runtime.evaluate(line.encode(), step_budget=400_000_000)
            except ForthAbort:
                pass
        return [int(v) for v in _REPORT.findall(
            self.runtime.drain_uart_output().decode("latin-1"))]

    def settle(self, rounds: int = 20) -> None:
        for _ in range(rounds):
            self.runtime.chiplet_ports.step()


BACKENDS = [pytest.param(Emulated, id="emulator"), pytest.param(Hosted, id="simulator")]

# Bring LM1 up and let managed data start; report the start's status.
START = [
    "0 0 0 CHIP-BOOT REPORT",
    "0 OBJ-START REPORT",
]


def started(backend, config: str = "") -> _Machine:
    machine = backend(config)
    assert machine.run(START) == [0, 0], "LM1 did not come up, or managed data did not start"
    return machine


# ---------------------------------------------------------------------------
#  1. Start
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_01_start(backend):
    machine = started(backend)
    got = machine.run([
        "OBJ-READY? REPORT  _OBJ-ATTACH-STATUS @ REPORT",
        "_HOLD@ REPORT",
        "_OBJ-LB @ LB.WINDOW-LIMIT + @ 0<> REPORT",
        "_OBJ-LB @ LB.HOLD-LIMIT + @ 0<> REPORT",
        "_OBJ-LB @ LB.HOLD-GAP + @ 0<> REPORT",
        "_SH-CONS @ 0<> _SH-VECTOR @ 0<> AND _SH-BYTES @ 0<> AND _SH-STRING @ 0<> AND REPORT",
        "_SH-CONS @ _SH-VECTOR @ <> _SH-BYTES @ _SH-STRING @ <> AND "
        "_SH-CONS @ _SH-BYTES @ <> AND _SH-VECTOR @ _SH-STRING @ <> AND REPORT",
        ": T1-HEADS 0 NCORES 0 DO I _CE * LB.CORES + _OBJ-LB @ + CE.LOG-HEAD + @ OR LOOP ; "
        "T1-HEADS REPORT",
    ])
    assert got == [-1, 0, 0, -1, -1, -1, -1, -1, 0]
    assert machine.lm1.md(lm1_chiplet.MD_PHASE) == 3   # serving


# ---------------------------------------------------------------------------
#  Helpers in Forth
# ---------------------------------------------------------------------------

def describe(word: str, tagged: list[int], raw: list[int], name: bytes) -> tuple[str, int]:
    """A CREATE line laying out a shape description (managed-data.md
    section 8) named WORD, and the description's length in bytes."""
    cells = [len(tagged), len(raw), len(name)] + tagged + raw
    padded = name + bytes(-len(name) % 8)
    cells += [int.from_bytes(padded[i:i + 8], "little") for i in range(0, len(padded), 8)]
    return ("CREATE %s %s" % (word, " ".join("%d ," % c for c in cells)),
            8 * (3 + len(tagged) + len(raw)) + len(name))


KIND_ANY, KIND_FIXNUM, KIND_STRING, KIND_CELL = 0, 1, 6, 16

HELPERS = [
    "CREATE BUF 256 ALLOT",
    "VARIABLE H  VARIABLE H2  VARIABLE H3  VARIABLE R0  VARIABLE R1  VARIABLE SH  VARIABLE N0",
    "VARIABLE MOST",
    # Hold until the stand-in has set HELD.
    ": WAITHELD BEGIN _HOLD@ H-HELD AND 0= WHILE 0 _CHIP-ACK REPEAT ;",
    ": EPOCH _HOLD@ 3 RSHIFT ;",
    # Steps of LM1 while core 0 polls the mailbox.
    ": STEPS 0 ?DO 0 _CHIP-ACK LOOP ;",
]

# A record of two tagged fields, a fixnum and a string, and one raw cell.
PAIR_DESC, PAIR_LEN = describe("D-PAIR", [KIND_FIXNUM, KIND_STRING], [KIND_CELL], b"pair")
# A record of one tagged field of any kind.
ONE_DESC, ONE_LEN = describe("D-ONE", [KIND_ANY], [], b"one")


def ready(backend, config: str = "", extra: list[str] | None = None) -> _Machine:
    machine = started(backend, config)
    assert machine.run(HELPERS + [PAIR_DESC, ONE_DESC] + (extra or [])) == []
    return machine


# The record of test 2: 7, "abc" and 2^64 - 1, owned by (1, 1).
MAKE_PAIR = [
    "D-PAIR %d OBJ-SHAPE REPORT SH !" % PAIR_LEN,
    ": MAKE2 OBJ-OPEN THROW SH @ OBJ-RECORD THROW 7 INT>OBJ THROW OVER 1 OBJ! THROW "
    "S\" abc\" OBJ-STRING THROW OVER 2 OBJ! THROW -1 OVER 0 OBJ-RAW! THROW "
    "DUP 1 1 OBJ-HANDLE THROW H ! R0 ! OBJ-CLOSE ;",
    "MAKE2",
    # Its fields, read in a new window: 7, a string "abc", 2^64 - 1, and
    # whether the slot holds the reference OBJ-GET gave.
    ": CHECK2 OBJ-OPEN THROW H @ OBJ-GET THROW DUP R1 ! DUP 1 OBJ@ OBJ>INT THROW REPORT "
    "DUP 2 OBJ@ OBJ>REF THROW DUP OBJ-SHAPE@ THROW _SH-STRING @ = REPORT "
    "BUF 64 OBJ>SPAN THROW REPORT BUF @ 0xFFFFFF AND REPORT "
    "0 OBJ-RAW@ THROW REPORT H @ 0xFFFFFFFF AND _SLOT @ R1 @ = REPORT OBJ-CLOSE ;",
]
PAIR_READS = [7, -1, 3, int.from_bytes(b"abc", "little"), -1, -1]


# ---------------------------------------------------------------------------
#  2 and 3. Handles survive collections
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_02_handle_survives_young_collection(backend):
    machine = ready(backend)
    got = machine.run(MAKE_PAIR + ["EPOCH REPORT 0 OBJ-COLLECT REPORT EPOCH REPORT", "CHECK2"])
    assert got[0] == 0, "OBJ-SHAPE"
    epoch = got[1]
    assert got[2:] == [0, epoch + 1] + PAIR_READS


@pytest.mark.parametrize("backend", BACKENDS)
def test_03_handle_survives_old_collection(backend):
    machine = ready(backend)
    got = machine.run(MAKE_PAIR + [
        "0 OBJ-COLLECT REPORT 0 OBJ-COLLECT REPORT 0 OBJ-COLLECT REPORT 0 OBJ-COLLECT REPORT",
        "CHECK2 R1 @ N0 !",
        "2 OBJ-COLLECT REPORT",
        "CHECK2 R1 @ N0 @ <> REPORT",
    ])
    assert got == [0] + [0] * 4 + PAIR_READS + [0] + PAIR_READS + [-1]
    assert machine.lm1.md(lm1_chiplet.MD_MOVED) > 0


# ---------------------------------------------------------------------------
#  4. A store into an old object is seen by the next young collection
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_04_store_into_old_object(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKEA OBJ-OPEN THROW SH @ OBJ-RECORD THROW 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKEA 0 OBJ-COLLECT REPORT",
        # B is young and in core 0's region: storing 42 into it needs no
        # entry; storing B into old A does.
        ": STOREB OBJ-OPEN THROW SH @ OBJ-RECORD THROW 42 INT>OBJ THROW OVER 1 OBJ! THROW "
        "DUP R0 ! H @ OBJ-GET THROW 1 OBJ! THROW OBJ-CLOSE ;",
        "STOREB _REC CR.TAIL + @ REPORT",
        "0 OBJ-COLLECT REPORT",
        ": CHECK4 OBJ-OPEN THROW H @ OBJ-GET THROW 1 OBJ@ DUP R1 ! OBJ>REF THROW "
        "1 OBJ@ OBJ>INT THROW REPORT R1 @ R0 @ <> REPORT "
        "H @ 0xFFFFFFFF AND _SLOT @ R0 @ <> REPORT OBJ-CLOSE ;",
        "CHECK4",
        "_ENT CE.LOG-HEAD + @ _REC CR.TAIL + @ = REPORT",
    ])
    # Two log entries: A's handle and B stored into A.  B moved out of its
    # region, and no word names its old place.
    assert got == [0, 0, 2, 0, 42, -1, -1, -1]


# ---------------------------------------------------------------------------
#  7. No window opens during a hold
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_07_no_window_opens_during_a_hold(backend):
    machine = ready(backend)
    assert machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKEA OBJ-OPEN THROW SH @ OBJ-RECORD THROW 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKEA OC.BASE OC@ R0 ! EPOCH REPORT",
    ]) == [0, 0]
    wakes = machine.lm1.md(lm1_chiplet.MD_WAKES)
    machine.lm1.md_hold(lm1_chiplet.MD_HOLD_ONLY, 500_000)
    got = machine.run([
        "WAITHELD EPOCH REPORT OBJ-OPEN REPORT EPOCH REPORT OC.SEEN OC@ REPORT",
        "OC.BASE OC@ _ENT CE.NEXT-BASE + @ = REPORT OC.BASE OC@ R0 @ <> REPORT",
        ": MAKE7 SH @ OBJ-RECORD THROW _OBJ-ADDR DUP _ENT CE.NEXT-BASE + @ U< 0= "
        "SWAP _ENT CE.NEXT-LIMIT + @ U< AND REPORT ; MAKE7 OBJ-CLOSE",
    ])
    # The open waited out the hold, saw the new epoch, took the region LM1
    # left it during the hold, and made its object there.
    assert got == [0, 0, 1, 1, -1, -1, -1]
    # Core 0's window word read WAITING at the hold's end, and LM1 rang it.
    assert machine.lm1.md(lm1_chiplet.MD_WAKES) == wakes + 1


# ---------------------------------------------------------------------------
#  8, 9 and 10. Owners, marks and stale handles
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_08_closing_an_owner_releases_its_handles(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        "VARIABLE HP VARIABLE HQ VARIABLE HR VARIABLE HQ6",
        ": ONE SH @ OBJ-RECORD THROW ;",
        ": MAKE8 OBJ-OPEN THROW ONE 5 1 OBJ-HANDLE THROW HP ! ONE DUP 5 1 OBJ-HANDLE THROW HQ ! "
        "6 1 OBJ-HANDLE THROW HQ6 ! ONE 5 1 OBJ-HANDLE THROW HR ! OBJ-CLOSE ;",
        "MAKE8 1 OBJ-COLLECT REPORT OBJ-COLLECTED DROP N0 !",
        "5 1 OBJ-OWNER-RELEASE REPORT 1 OBJ-COLLECT REPORT OBJ-COLLECTED DROP N0 @ SWAP - REPORT",
        ": CHECK8 OBJ-OPEN THROW HP @ OBJ-GET NIP REPORT HQ @ OBJ-GET NIP REPORT "
        "HR @ OBJ-GET NIP REPORT HQ6 @ OBJ-GET REPORT OBJ-SHAPE@ THROW SH @ = REPORT OBJ-CLOSE ;",
        "CHECK8",
    ])
    stale = -1103
    # Each record of one tagged field is 24 bytes: header, count, field.
    assert got == [0, 0, 0, 0, 48, stale, stale, stale, 0, -1]


@pytest.mark.parametrize("backend", BACKENDS)
def test_09_a_mark_releases_a_words_own_handles(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": ONE SH @ OBJ-RECORD THROW ;",
        ": MAKE9 OBJ-OPEN THROW ONE 7 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKE9 7 1 OBJ-OWNER-MARK N0 !",
        ": MORE9 OBJ-OPEN THROW ONE 7 1 OBJ-HANDLE THROW H2 ! ONE 7 1 OBJ-HANDLE THROW H3 ! OBJ-CLOSE ;",
        "MORE9 N0 @ 7 1 OBJ-OWNER-RELEASE-TO REPORT",
        ": CHECK9 OBJ-OPEN THROW H2 @ OBJ-GET NIP REPORT H3 @ OBJ-GET NIP REPORT "
        "H @ OBJ-GET NIP REPORT OBJ-CLOSE ;",
        "CHECK9",
    ])
    assert got == [0, 0, -1103, -1103, 0]


@pytest.mark.parametrize("backend", BACKENDS)
def test_10_stale_handles_are_found(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": ONE SH @ OBJ-RECORD THROW ;",
        ": MAKE10 OBJ-OPEN THROW ONE DUP R0 ! 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKE10 H @ OBJ-DROP REPORT",
        ": AGAIN10 OBJ-OPEN THROW ONE DUP R1 ! 1 1 OBJ-HANDLE THROW H2 ! OBJ-CLOSE ;",
        "AGAIN10 H @ 0xFFFFFFFF AND H2 @ 0xFFFFFFFF AND = REPORT",
        ": CHECK10 OBJ-OPEN THROW H @ OBJ-GET NIP REPORT H2 @ OBJ-GET THROW R1 @ = REPORT "
        "0 OBJ-GET NIP REPORT 1000000 OBJ-GET NIP REPORT "
        "R0 @ H @ OBJ-SET REPORT H2 @ 0xFFFFFFFF AND _SLOT @ R1 @ = REPORT OBJ-CLOSE ;",
        "CHECK10 H @ OBJ-DROP REPORT H2 @ OBJ-DROP REPORT H2 @ OBJ-DROP REPORT",
    ])
    stale = -1103
    assert got == [0, 0, -1, stale, -1, stale, stale, stale, -1, stale, 0, stale]


# ---------------------------------------------------------------------------
#  11 and 12. The table grows; a handle re-pointed
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_11_the_handle_table_grows(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        "CREATE HS 80 CELLS ALLOT",
        ": MAKE11 OBJ-OPEN THROW _HD-FIRST 1+ 0 DO SH @ OBJ-RECORD THROW I INT>OBJ THROW OVER 1 OBJ! THROW "
        "1 1 OBJ-HANDLE THROW HS I CELLS + ! LOOP OBJ-CLOSE ;",
        "MAKE11 _HD-SEGS REPORT",
        "2 OBJ-COLLECT REPORT",
        ": CHECK11 OBJ-OPEN THROW 0 _HD-FIRST 1+ 0 DO HS I CELLS + @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW "
        "I = IF 1+ THEN LOOP REPORT OBJ-CLOSE ;",
        "CHECK11 _HD-FIRST REPORT",
    ])
    assert got == [0, 2, 0, 65, 64]


@pytest.mark.parametrize("backend", BACKENDS)
def test_12_repointing_a_handle_is_seen(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": ONE SH @ OBJ-RECORD THROW ;",
        ": MAKEC OBJ-OPEN THROW ONE 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKEC",
        ": SETD OBJ-OPEN THROW ONE 99 INT>OBJ THROW OVER 1 OBJ! THROW DUP R0 ! H @ OBJ-SET THROW OBJ-CLOSE ;",
        "SETD 0 OBJ-COLLECT REPORT OBJ-COLLECTED DROP REPORT",
        ": CHECK12 OBJ-OPEN THROW H @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW REPORT OBJ-CLOSE ;",
        "CHECK12",
    ])
    # The heap holds D alone, 24 bytes; C is not counted.
    assert got == [0, 0, 24, 99]


# ---------------------------------------------------------------------------
#  13. A full store log
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_13_a_full_store_log(backend):
    # Core 0's log is made at its first entry, of OBJ-LOG-ENTRIES entries.
    machine = ready(backend, extra=["8 OBJ-LOG-ENTRIES !"])
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKEV OBJ-OPEN THROW OBJ-NIL 32 OBJ-VECTOR THROW 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKEV 0 OBJ-COLLECT REPORT _REC CR.TAIL + @ N0 !",
        # 32 young records, each stored into an element of old V: each store
        # is an entry, and a log of 8 entries fills four times.
        ": FILL13 0 MOST ! OBJ-OPEN THROW 32 0 DO SH @ OBJ-RECORD THROW I INT>OBJ THROW OVER 1 OBJ! THROW "
        "H @ OBJ-GET THROW I 1+ OBJ! THROW OC.TAIL OC@ _OBJ-LOG-HEAD@ - MOST @ MAX MOST ! LOOP OBJ-CLOSE ;",
        "FILL13 _REC CR.TAIL + @ N0 @ - REPORT MOST @ REPORT OC.CAP OC@ REPORT",
        "0 OBJ-COLLECT REPORT",
        ": CHECK13 OBJ-OPEN THROW 0 32 0 DO H @ OBJ-GET THROW I 1+ OBJ@ OBJ>REF THROW 1 OBJ@ OBJ>INT THROW "
        "I = IF 1+ THEN LOOP REPORT OBJ-CLOSE ;",
        "CHECK13 _ENT CE.LOG-HEAD + @ _REC CR.TAIL + @ = REPORT",
    ])
    assert got[:2] == [0, 0]
    # The 32 stores counted 32 entries, and TAIL never ran more than the
    # ring's 8 entries ahead of LOG_HEAD.
    tail, most, capacity = got[2:5]
    assert (tail, capacity) == (32, 8)
    assert 4 <= most <= 8
    assert got[5:] == [0, 32, -1]


# ---------------------------------------------------------------------------
#  18. Regions after a hold
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_18_regions_after_a_hold(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKE18 OBJ-OPEN THROW SH @ OBJ-RECORD THROW 5 INT>OBJ THROW OVER 1 OBJ! THROW "
        "1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKE18 OC.BASE OC@ R0 ! OC.LIMIT OC@ R1 !",
        "0 OBJ-COLLECT REPORT",
        "OC.SERIAL OC@ N0 !",
        ": NEXT18 OBJ-OPEN THROW SH @ OBJ-RECORD THROW _OBJ-ADDR DUP _ENT CE.NEXT-BASE + @ U< 0= "
        "SWAP _ENT CE.NEXT-LIMIT + @ U< AND REPORT OC.SERIAL OC@ N0 @ > REPORT "
        "OC.BASE OC@ _REC CR.REGION + @ = REPORT "
        "H @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW REPORT OBJ-CLOSE ;",
        "NEXT18",
    ])
    # The object lies in the region LM1 wrote during the hold, a grant the
    # core took after the hold, not the region it made objects in before.
    assert got == [0, 0, -1, -1, -1, 5]


# ---------------------------------------------------------------------------
#  19 and 20. Raw words, bytes and numbers
# ---------------------------------------------------------------------------

RAW_DESC, RAW_LEN = describe("D-RAW", [KIND_ANY], [KIND_CELL], b"raw")


@pytest.mark.parametrize("backend", BACKENDS)
def test_19_raw_words_and_bytes_are_never_references(backend):
    machine = ready(backend, extra=[RAW_DESC])
    got = machine.run([
        "D-RAW %d OBJ-SHAPE REPORT SH !" % RAW_LEN,
        "D-ONE %d OBJ-SHAPE REPORT H3 !" % ONE_LEN,
        # Z is held by nothing; R's raw word and a bytevector's bytes hold
        # Z's address with tag 001.
        ": MAKE19 OBJ-OPEN THROW H3 @ OBJ-RECORD THROW _OBJ-ADDR 1 OR DUP R0 ! "
        "SH @ OBJ-RECORD THROW TUCK 0 OBJ-RAW! THROW 1 1 OBJ-HANDLE THROW H ! "
        "R0 @ BUF ! BUF 8 OBJ-BYTES THROW 1 1 OBJ-HANDLE THROW H2 ! OBJ-CLOSE ;",
        "MAKE19 1 OBJ-COLLECT REPORT OBJ-COLLECTED DROP REPORT",
        "2 OBJ-COLLECT REPORT OBJ-COLLECTED DROP REPORT",
        ": CHECK19 OBJ-OPEN THROW H @ OBJ-GET THROW 0 OBJ-RAW@ THROW R0 @ = REPORT "
        "H2 @ OBJ-GET THROW BUF 16 OBJ>SPAN THROW REPORT BUF @ R0 @ = REPORT OBJ-CLOSE ;",
        "CHECK19",
    ])
    # R is 32 bytes (header, count, field, raw word) and the bytevector of 8
    # bytes 24; Z's 24 are not counted.
    assert got == [0, 0, 0, 56, 0, 56, -1, 8, -1]


NUMBER_DESC, NUMBER_LEN = describe("D-NUM", [KIND_FIXNUM], [KIND_CELL], b"num")


@pytest.mark.parametrize("backend", BACKENDS)
def test_20_numbers_round_trip(backend):
    machine = ready(backend, extra=[NUMBER_DESC])
    low, high = -(1 << 62), (1 << 62) - 1
    got = machine.run([
        "D-NUM %d OBJ-SHAPE REPORT SH !" % NUMBER_LEN,
        ": MAKE20 OBJ-OPEN THROW SH @ OBJ-RECORD THROW 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
        "MAKE20",
        ": PUT ( n -- ) OBJ-OPEN THROW INT>OBJ DUP REPORT 0= IF H @ OBJ-GET THROW 1 OBJ! THROW ELSE DROP THEN OBJ-CLOSE ;",
        ": GET ( -- ) OBJ-OPEN THROW H @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW REPORT OBJ-CLOSE ;",
        ": RAW ( x -- ) OBJ-OPEN THROW H @ OBJ-GET THROW 0 OBJ-RAW! THROW OBJ-CLOSE ;",
        ": GETRAW ( -- ) OBJ-OPEN THROW H @ OBJ-GET THROW 0 OBJ-RAW@ THROW REPORT OBJ-CLOSE ;",
        "%d PUT 2 OBJ-COLLECT REPORT GET" % low,
        "%d PUT 2 OBJ-COLLECT REPORT GET" % high,
        "0 PUT 2 OBJ-COLLECT REPORT GET",
        "%d PUT GET" % (1 << 62),
        "%d RAW 2 OBJ-COLLECT REPORT GETRAW" % -(1 << 63),
        "-1 RAW 2 OBJ-COLLECT REPORT GETRAW",
        "1 RAW 2 OBJ-COLLECT REPORT GETRAW",
    ])
    assert got == [0, 0, 0, low, 0, 0, high, 0, 0, 0, -1107, 0,
                   0, -(1 << 63), 0, -1, 0, 1]


# ---------------------------------------------------------------------------
#  21 and 22. Forged references
# ---------------------------------------------------------------------------

# A record A of one tagged field, held by (1, 1); a word with tag 001 that
# names external memory outside the shared heap, which nothing lends.
MAKE_A = [
    "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
    ": MAKEA OBJ-OPEN THROW SH @ OBJ-RECORD THROW DUP R0 ! 1 1 OBJ-HANDLE THROW H ! OBJ-CLOSE ;",
    "MAKEA R0 @ REPORT R0 @ _OBJ-ADDR 16 + REPORT",
    "EXT-MEM-BASE EXT-MEM-SIZE + 4096 - 1 OR DUP N0 ! REPORT",
]


def _step_lm1(machine: _Machine, steps: int = 50) -> None:
    """Steps of LM1 while core 0 polls the mailbox."""
    machine.run(["%d STEPS" % steps])


@pytest.mark.parametrize("backend", BACKENDS)
def test_21_a_forged_reference_is_refused(backend):
    machine = ready(backend)
    shape, ref, field, forged = machine.run(MAKE_A)
    assert shape == 0
    lm1 = machine.lm1
    lm1.md_write(field, forged)
    _step_lm1(machine)
    # LM1 code loads the field with LD: r2 <- field 1 of the record in r1.
    from lm1.decode import Op, encode_i, encode_x
    code = [encode_i(Op.LD, 2, 1, 1), encode_x(Op.HALT_NOP, 0)]
    lm1.write(0, 0x1000, (code[0] | code[1] << 32).to_bytes(8, "little"))
    lm1.set_reg(0, 0, 1, ref)
    lm1.set_reg(0, 0, 2, 0x55)
    lm1.thread_set(0, 0, lm1_chiplet.THREAD_PC, 0x1000)
    lm1.thread_set(0, 0, lm1_chiplet.THREAD_HALTED, 0)
    _step_lm1(machine)
    fault = lm1.first_fault()
    assert (fault.stop, fault.trap) == (lm1_chiplet.STOP_TRAP, 0x61)   # TRAP_SHARED_REF
    assert lm1.reg(0, 0, 2) == 0x55, "the load left its destination as it was"
    got = machine.run([
        # The bridge refused nothing: the named memory was never accessed.
        "0 CHIP-FAULT@ REPORT REPORT",
        "1 OBJ-COLLECT REPORT OBJ-NOTICE-KIND @ REPORT OBJ-NOTICES @ REPORT",
        "OBJ-OPEN REPORT OBJ-ENGINE REPORT REPORT",
    ])
    # LM1 reported corrupt data, set FAILED, and the heap is lost.
    assert got == [0, 0, -1102, 2, 1, -1102, 1, 0]
    assert lm1.md(lm1_chiplet.MD_PHASE) == 4
    assert lm1.md(lm1_chiplet.MD_HOLD) & 4


@pytest.mark.parametrize("backend", BACKENDS)
def test_22_megapad_refuses_a_bad_reference_from_lm1(backend):
    machine = ready(backend)
    shape, ref, field, forged = machine.run(MAKE_A)
    machine.lm1.md_write(field, forged)
    _step_lm1(machine)
    got = machine.run([
        ": READ22 OBJ-OPEN THROW H @ OBJ-GET THROW 1 OBJ@ DUP N0 @ = REPORT OBJ>REF NIP REPORT OBJ-CLOSE ;",
        "READ22 OBJ-READY? REPORT",
    ])
    # Until recovery's stage 1 there is no evidence latch: the heap serves on.
    assert got == [-1, -1108, -1]


# ---------------------------------------------------------------------------
#  23, 24 and 25. Resets, overdue holds and overdue windows
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("backend", BACKENDS)
def test_23_a_reset_outside_a_hold(backend):
    machine = ready(backend)
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKE23 OBJ-OPEN THROW SH @ OBJ-RECORD THROW 1 1 OBJ-HANDLE THROW H ! "
        "SH @ OBJ-RECORD THROW 2 1 OBJ-HANDLE THROW H2 ! OBJ-CLOSE ;",
        "MAKE23 OBJ-MEMORY 0> REPORT",
        # MegaPad memory outside the function's: a block KDOS gave the test.
        "64 XMEM-ALLOT R1 ! R1 @ 64 0x5A FILL",
        "0 CHIP-RESET",
        "OBJ-OPEN REPORT H @ OBJ-GET NIP REPORT 1 1 OBJ-OWNER-RELEASE REPORT "
        "2 1 OBJ-OWNER-RELEASE REPORT OBJ-MEMORY REPORT OBJ-ENGINE REPORT REPORT",
        ": CANARY 0 64 0 DO R1 @ I + C@ 0x5A = IF 1+ THEN LOOP ; CANARY REPORT",
    ])
    assert got == [0, -1, -1102, -1102, 0, 0, 0, 6, 0, 64]


@pytest.mark.parametrize("backend", BACKENDS)
def test_24_an_overdue_hold(backend):
    # HOLD_LIMIT of 100 microseconds: a hold ACTIVE past 200 is overdue.
    machine = ready(backend, "hold_limit=100000")
    machine.lm1.md_hold(lm1_chiplet.MD_HOLD_ONLY, lm1_chiplet.MD_FOR_EVER)
    got = machine.run([
        "WAITHELD _HOLD@ H-ACTIVE AND 0<> REPORT OBJ-OPEN REPORT",
        "OBJ-ENGINE REPORT REPORT _P-CTRL 0 CHIP@ _CTRL-RESET-HOLD AND 0<> REPORT OBJ-MEMORY REPORT",
    ])
    # The open found the hold overdue (cause 4); the port is held in reset,
    # and with no window open the function's memory went back to KDOS.
    assert got == [-1, -1102, 4, 0, -1, 0]
    assert machine.lm1.link(lm1_chiplet.LINK_BOOT) == lm1_chiplet.BOOT_RESET


@pytest.mark.parametrize("backend", BACKENDS)
def test_25_an_overdue_window_is_reported(backend):
    machine = ready(backend)
    assert machine.run(["OBJ-OPEN REPORT EPOCH N0 !"]) == [0]
    machine.lm1.md_hold(lm1_chiplet.MD_HOLD_ONLY, 0)
    got = machine.run([
        # Inside the window: the hold waits for it, and LM1 reports it once
        # it has stayed open WINDOW_LIMIT, a millisecond, after HELD.
        ": WAIT25 0 BEGIN 1+ _CHIP-SERVICE OBJ-NOTICES @ OVER 1000000 = OR UNTIL DROP ;",
        "WAITHELD WAIT25 OBJ-NOTICES @ REPORT OBJ-NOTICE-KIND @ REPORT "
        "OBJ-NOTICE-ARG @ 0xFFFF AND REPORT OBJ-NOTICE-ARG @ 16 RSHIFT 1000000 < 0= REPORT "
        "_HOLD@ H-ACTIVE AND REPORT EPOCH N0 @ = REPORT",
        "OBJ-CLOSE",
        ": WAITEND 0 BEGIN 1+ 0 _CHIP-ACK EPOCH N0 @ <> OVER 1000000 = OR UNTIL DROP ;",
        "WAITEND EPOCH N0 @ - REPORT",
    ])
    assert got == [1, 1, 0, -1, 0, -1, 1]


# ---------------------------------------------------------------------------
#  26. Writes from both chips are whole
# ---------------------------------------------------------------------------

TWO_DESC, TWO_LEN = describe("D-TWO", [KIND_ANY, KIND_ANY], [], b"two")


@pytest.mark.parametrize("backend", BACKENDS)
def test_26_writes_from_both_chips_are_whole(backend):
    machine = ready(backend, extra=[TWO_DESC])
    got = machine.run([
        "D-TWO %d OBJ-SHAPE REPORT SH !" % TWO_LEN,
        "D-ONE %d OBJ-SHAPE REPORT H3 !" % ONE_LEN,
        "VARIABLE X VARIABLE Y VARIABLE HDR VARIABLE BAD VARIABLE SEENX VARIABLE SEEN1",
        ": MAKE26 OBJ-OPEN THROW H3 @ OBJ-RECORD THROW DUP X ! 1 1 OBJ-HANDLE THROW DROP "
        "H3 @ OBJ-RECORD THROW DUP Y ! 1 1 OBJ-HANDLE THROW DROP "
        "SH @ OBJ-RECORD THROW DUP R0 ! 1 1 OBJ-HANDLE THROW H ! "
        "X @ R0 @ 1 OBJ! THROW 111 INT>OBJ THROW R0 @ 2 OBJ! THROW "
        "R0 @ _OBJ-ADDR @ HDR ! OBJ-CLOSE ;",
        "MAKE26 X @ REPORT Y @ REPORT R0 @ _OBJ-ADDR REPORT",
    ])
    assert got[:2] == [0, 0]
    x, y, at = got[2:5]
    lm1 = machine.lm1
    # The writes outlast the reads: one of each a step, for far longer.
    lm1.md_churn(at + 16, y, x, 10_000_000)
    lm1.md_churn(at + 24, 222 << 1, 111 << 1, 10_000_000)
    got = machine.run([
        ": READ26 0 BAD ! 0 SEENX ! 0 SEEN1 ! 2000 0 DO OBJ-OPEN THROW 0 _CHIP-ACK "
        "H @ OBJ-GET THROW DUP 1 OBJ@ DUP X @ = IF 1 SEENX +! DROP ELSE Y @ <> IF 1 BAD +! THEN THEN "
        "DUP 2 OBJ@ DUP 111 2* = IF 1 SEEN1 +! DROP ELSE 222 2* <> IF 1 BAD +! THEN THEN "
        "_OBJ-ADDR @ HDR @ <> IF 1 BAD +! THEN OBJ-POLL IF 1 BAD +! THEN OBJ-CLOSE LOOP ;",
        "READ26 BAD @ REPORT SEENX @ 0> REPORT SEENX @ 2000 < REPORT SEEN1 @ 0> REPORT SEEN1 @ 2000 < REPORT",
    ])
    # Every word read was one of the values written, both values of each
    # field were seen, and the header never changed.
    assert got == [0, -1, -1, -1, -1]


# ---------------------------------------------------------------------------
#  27. Shapes
# ---------------------------------------------------------------------------

BAD_DESC, BAD_LEN = describe("D-BAD", [15], [], b"bad")


@pytest.mark.parametrize("backend", BACKENDS)
def test_27_shapes(backend):
    machine = ready(backend, extra=[BAD_DESC])
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        "D-ONE %d OBJ-SHAPE REPORT SH @ = REPORT" % ONE_LEN,
        "D-BAD %d OBJ-SHAPE REPORT DROP OBJ-STATUS REPORT" % BAD_LEN,
    ])
    assert got == [0, 0, -1, -1106, 0x101]


# ---------------------------------------------------------------------------
#  Several cores: 5, 6, 14, 15, 16 and 17, on the emulator
# ---------------------------------------------------------------------------
#
#  Core 1 runs a word as a BIOS worker (CORE-RUN), and LM1 rings it for its
#  own pair's replies.  Until the multicore plan's stage 2 (MegaPad branch
#  multicore-sim, 4ea4bc91), a worker's IPI handler takes a doorbell from a
#  port it finds among IDs 0 to 3 for a new job, the last one again.  These
#  cases run on two full cores, where the port is ID 2, and LM1 rings core 1
#  only for replies that arrive while its job runs, so the job that the
#  handler records is the one running, and the worker clears it when the
#  job returns.  A doorbell to a worker that sleeps between jobs, or a port
#  with an ID above 3, needs stage 2.

CORE1 = [
    "VARIABLE C1-IOR  VARIABLE C1-LIVE  VARIABLE C1-DONE",
    ": C1-COLLECT2 2 OBJ-COLLECT C1-IOR ! OBJ-COLLECTED DROP C1-LIVE ! TRUE C1-DONE ! ;",
    ": C1-COLLECT1 1 OBJ-COLLECT C1-IOR ! OBJ-COLLECTED DROP C1-LIVE ! TRUE C1-DONE ! ;",
    ": C1 ( xt -- ) FALSE C1-DONE ! 1 CORE-RUN ;",
    ": C1-WAIT 0 BEGIN 1+ 0 _CHIP-ACK C1-DONE @ OVER 2000000 = OR UNTIL DROP ;",
]


def two_cores(extra: list[str] | None = None) -> _Machine:
    machine = ready(Emulated, extra=CORE1 + (extra or []))
    return machine


def test_05_a_moving_collection_waits_while_a_window_is_open():
    machine = two_cores()
    got = machine.run(MAKE_PAIR + [
        "OBJ-OPEN REPORT H @ OBJ-GET THROW R1 ! EPOCH N0 !",
        "' C1-COLLECT2 C1 WAITHELD",
        "_HOLD@ H-ACTIVE AND REPORT EPOCH N0 @ = REPORT R1 @ 1 OBJ@ OBJ>INT THROW REPORT",
        "200 STEPS _HOLD@ H-ACTIVE AND REPORT EPOCH N0 @ = REPORT",
        "OBJ-CLOSE C1-WAIT C1-IOR @ REPORT EPOCH N0 @ - REPORT",
        "CHECK2",
    ])
    assert got == [0, 0, 0, -1, 7, 0, -1, 0, 1] + PAIR_READS


def test_06_an_object_lives_while_only_a_window_uses_it():
    machine = two_cores()
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        ": MAKE6 OBJ-OPEN THROW SH @ OBJ-RECORD THROW DUP 1 1 OBJ-HANDLE THROW H ! "
        "SH @ OBJ-RECORD THROW 9 INT>OBJ THROW OVER 1 OBJ! THROW SWAP 1 OBJ! THROW OBJ-CLOSE ;",
        "MAKE6 1 OBJ-COLLECT REPORT OBJ-COLLECTED DROP N0 !",
        ": TAKE6 OBJ-OPEN THROW H @ OBJ-GET THROW DUP 1 OBJ@ OBJ>REF THROW R1 ! OBJ-NIL SWAP 1 OBJ! THROW ;",
        "TAKE6 ' C1-COLLECT1 C1 WAITHELD",
        ": HOLD6 0 2000 0 DO 0 _CHIP-ACK R1 @ 1 OBJ@ OBJ>INT THROW 9 = IF 1+ THEN LOOP ;",
        "HOLD6 REPORT OBJ-CLOSE C1-WAIT C1-IOR @ REPORT N0 @ C1-LIVE @ - REPORT",
    ])
    # Y, 24 bytes, lived while core 0's window used it, and the collection
    # that ended after the close did not count it.
    assert got == [0, 0, 2000, 0, 24]


def test_14_several_cores():
    machine = two_cores()
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        "CREATE HS0 8 CELLS ALLOT CREATE HS1 8 CELLS ALLOT VARIABLE C1-MOVED",
        ": MAKE-ON ( hs id -- ) 8 0 DO SH @ OBJ-RECORD THROW I INT>OBJ THROW OVER 1 OBJ! THROW "
        "OVER 1 OBJ-HANDLE THROW 2 PICK I CELLS + ! LOOP 2DROP ;",
        # Core 1 makes its records and keeps its window open a while after
        # core 0 has closed, watching that nothing moves meanwhile.
        ": C1-MAKE OBJ-OPEN DROP HS1 2 MAKE-ON HS1 @ OBJ-GET DROP R0 ! 0 C1-MOVED ! "
        "200000 0 DO _HOLD@ H-ACTIVE AND IF 1 C1-MOVED +! THEN LOOP "
        "HS1 @ OBJ-GET DROP R0 @ <> IF 1 C1-MOVED +! THEN OBJ-CLOSE TRUE C1-DONE ! ;",
        "' C1-MAKE C1",
        ": MAKE14 OBJ-OPEN THROW HS0 1 MAKE-ON OBJ-CLOSE ;",
        "MAKE14 2 OBJ-COLLECT REPORT C1-WAIT C1-MOVED @ REPORT",
        ": CHECK14 OBJ-OPEN THROW 0 8 0 DO HS0 I CELLS + @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW I = IF 1+ THEN "
        "HS1 I CELLS + @ OBJ-GET THROW 1 OBJ@ OBJ>INT THROW I = IF 1+ THEN LOOP REPORT OBJ-CLOSE ;",
        "CHECK14",
        ": DRAINED 0 NCORES 0 DO I _CE * LB.CORES + _OBJ-LB @ + CE.LOG-HEAD + @ I _REC-OF CR.TAIL + @ = "
        "IF 1+ THEN LOOP ; DRAINED REPORT",
    ])
    assert got == [0, 0, 0, 16, 2]


def test_15_nested_windows():
    machine = two_cores()
    got = machine.run(MAKE_PAIR + [
        "OBJ-OPEN REPORT OBJ-OPEN REPORT OBJ-CLOSE _REC CR.WINDOW + @ REPORT EPOCH N0 !",
        "' C1-COLLECT2 C1 WAITHELD 200 STEPS",
        "_HOLD@ H-ACTIVE AND REPORT EPOCH N0 @ = REPORT",
        "OBJ-CLOSE _REC CR.WINDOW + @ REPORT C1-WAIT C1-IOR @ REPORT EPOCH N0 @ - REPORT",
        "CHECK2",
    ])
    assert got == [0, 0, 0, 1, 0, -1, 0, 0, 1] + PAIR_READS


def test_16_polls():
    machine = two_cores()
    got = machine.run(MAKE_PAIR + [
        "OBJ-OPEN REPORT H @ OBJ-GET THROW R0 ! EPOCH N0 !",
        "OBJ-POLL REPORT EPOCH N0 @ = REPORT",
        "' C1-COLLECT2 C1 WAITHELD OBJ-POLL REPORT EPOCH N0 @ - REPORT",
        "H @ OBJ-GET THROW DUP R0 @ <> REPORT 1 OBJ@ OBJ>INT THROW REPORT OBJ-CLOSE C1-WAIT C1-IOR @ REPORT",
    ])
    assert got == [0, 0, 0, -1, -1, 1, -1, 7, 0]


def test_17_a_region_request_during_a_hold():
    machine = two_cores()
    got = machine.run([
        "D-ONE %d OBJ-SHAPE REPORT SH !" % ONE_LEN,
        "VARIABLE BEFORE",
        ": FILL17 OBJ-OPEN THROW BEGIN OC.LIMIT OC@ OC.BUMP OC@ - 48 > WHILE SH @ OBJ-RECORD THROW DROP REPEAT ;",
        "FILL17 OC.BASE OC@ R0 ! EPOCH N0 ! OC.SERIAL OC@ BEFORE !",
        "' C1-COLLECT2 C1 WAITHELD _HOLD@ H-ACTIVE AND REPORT",
        # 64 elements: more than the region's remainder.
        "OBJ-NIL 64 OBJ-VECTOR REPORT _OBJ-ADDR OC.BASE OC@ = REPORT "
        "OC.SERIAL OC@ BEFORE @ > REPORT _HOLD@ H-ACTIVE AND REPORT EPOCH N0 @ = REPORT",
        "OBJ-CLOSE C1-WAIT C1-IOR @ REPORT EPOCH N0 @ - REPORT",
    ])
    assert got == [0, 0, 0, -1, -1, 0, -1, 0, 1]
