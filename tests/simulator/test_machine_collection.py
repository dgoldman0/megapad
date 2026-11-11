"""A machine that nothing refers to any longer is freed.

The native extension's classes own Python references that lead back to the
machine: the dictionary owns every definition and view, the line walker owns
the dictionary, and the program holds the memory through its tile guard.
Each shows Python's collector what it owns (simulator/accel/collected.h), and
without that a dropped machine stays for the life of the process.

The first tests drop whole machines after each owner has done real work.
The others build one owner alone and make a single reference of its lead
back to it.  A reference that an ordinary machine never routes through a
cycle must be visited all the same, and only a cycle through it shows a
missing visit.
"""

from __future__ import annotations

import gc
import weakref

import pytest

from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.stacks import Continuation

PAGE = 4096
CONFIGURATIONS = (
    pytest.param("python", False, id="python"),
    pytest.param("native", False, id="native-sparse"),
    pytest.param("native", True, id="native-dense"),
)
TILES = (
    b"CREATE TA 64 ALLOT CREATE TB 64 ALLOT CREATE TD 64 ALLOT "
    b": FILL-TILES 64 0 DO I 3 * TA I + C! I 7 * 5 + TB I + C! LOOP ; FILL-TILES "
    b": RUN FP16-MODE TA TSRC0! TB TSRC1! TD TDST! 3 0 DO TADD TMUL LOOP ; "
)


def _machine(backend: str, dense: bool) -> MegaForthRuntime:
    if backend == "native":
        pytest.importorskip("_megaforth_native")
    return MegaForthRuntime(
        execution_backend=backend,
        memory=create_one_core_address_space(dense_backing=True if dense else None),
    )


def _ran_natively(runtime: MegaForthRuntime, backend: str, before: int = 0) -> None:
    steps = runtime.native_execution_stats["semantic_steps"]
    if backend == "native":
        assert steps > before, "the source must reach the native program"
    else:
        assert steps == 0


def _run_definitions(runtime: MegaForthRuntime, backend: str) -> None:
    runtime.evaluate(
        b": SQUARE DUP * ; 7 CONSTANT SEVEN CREATE BUF 16 ALLOT "
        b": RUN 0 50 0 DO I SQUARE + SEVEN + LOOP BUF ! ; RUN",
        source_name="collection.f",
    )
    _ran_natively(runtime, backend)
    # Python has asked for these definitions, so the dictionary caches views.
    assert runtime.find(b"RUN") is runtime.find(b"RUN")
    assert runtime.find(b"BUF").body_address


def _run_tiles(runtime: MegaForthRuntime, backend: str) -> None:
    runtime.evaluate(TILES, source_name="collection-tiles.f")
    before = runtime.native_execution_stats["semantic_steps"]
    runtime.execute("RUN")
    # The program asks its tile guard at the first tile word of a run.
    _ran_natively(runtime, backend, before)
    assert runtime.diagnostics.perf_tileops == 6


def _live(kind: type) -> int:
    """The instances of KIND that the process holds, reachable or not."""

    return sum(type(candidate) is kind for candidate in gc.get_objects())


def _dropped(backend: str, dense: bool, use) -> tuple[weakref.ref, weakref.ref]:
    runtime = _machine(backend, dense)
    use(runtime, backend)
    return weakref.ref(runtime), weakref.ref(runtime.memory)


@pytest.mark.parametrize("use", (_run_definitions, _run_tiles),
                         ids=("definitions", "tiles"))
@pytest.mark.parametrize("backend, dense", CONFIGURATIONS)
def test_a_dropped_machine_is_freed(backend: str, dense: bool, use) -> None:
    gc.collect()
    before = _live(MegaForthRuntime)
    runtime, memory = _dropped(backend, dense, use)
    gc.collect()
    # The collector found it unreachable, and it is gone.
    assert runtime() is None
    assert memory() is None
    assert _live(MegaForthRuntime) == before


@pytest.mark.parametrize("backend, dense", CONFIGURATIONS)
def test_no_machine_of_many_stays(backend: str, dense: bool) -> None:
    gc.collect()
    before = _live(MegaForthRuntime)
    for _ in range(40):
        _dropped(backend, dense, _run_definitions)
    gc.collect()
    assert _live(MegaForthRuntime) == before


def test_a_machine_whose_lines_the_walker_evaluated_is_freed(monkeypatch) -> None:
    pytest.importorskip("_megaforth_native")
    from simulator.source_acceleration import install_kdos_source_accelerators
    from tests.simulator.test_kdos_module_system import _load_module_system
    from tests.simulator.test_native_walker import _evaluate, _walker_stats

    def walked() -> tuple[weakref.ref, weakref.ref]:
        runtime = _load_module_system()
        install_kdos_source_accelerators(runtime)
        before = _walker_stats(runtime)
        outcome = _evaluate(
            runtime, "WALKED", b": DOUBLED DUP + ;\n21 DOUBLED\n: STILL-OPEN 1\n")
        # The last line leaves its definition open, which the status reports.
        assert outcome[0][0] == "returned" and outcome[1] == (42, 4)
        assert _walker_stats(runtime)[0] > before[0], "the walker must run lines"
        return weakref.ref(runtime), weakref.ref(runtime.memory)

    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "native")
    gc.collect()
    before = _live(MegaForthRuntime)
    runtime, memory = walked()
    gc.collect()
    assert runtime() is None
    assert memory() is None
    assert _live(MegaForthRuntime) == before


# ── One owner alone ─────────────────────────────────────────────────────


class _Keeper:
    """Something that keeps its owner and can be referred to weakly."""

    def __init__(self, owner=None) -> None:
        self.owner = owner


class _Leased:
    """A writable buffer of one page that keeps its owner.  Python calls
    ``__buffer__`` from version 3.12 on."""

    def __init__(self) -> None:
        self.data = bytearray(PAGE)
        self.owner = None

    def __buffer__(self, flags: int) -> memoryview:
        return memoryview(self.data)


def _freed(kind: type, build) -> bool:
    """Whether one collection frees the instance of KIND that BUILD makes in
    a cycle and lets go.

    BUILD may return a weak reference to a Python object of that cycle.  The
    collector clears it on finding the cycle unreachable, which takes a visit
    of every reference in it.  That the cycle was broken as well shows only
    in the count of instances: a weak reference is cleared for an unreachable
    object whether or not the object goes.
    """

    enabled = gc.isenabled()
    # Off, so that the cycle lives until the collection below.
    gc.disable()
    try:
        gc.collect()
        before = _live(kind)
        probe = build()
        assert _live(kind) == before + 1, "a cycle lives until it is collected"
        gc.collect()
        return (probe is None or probe() is None) and _live(kind) == before
    finally:
        if enabled:
            gc.enable()


@pytest.fixture
def native():
    return pytest.importorskip("_megaforth_native")


def test_a_program_is_freed_with_the_type_of_its_continuations(native) -> None:
    def build():
        class Kind:
            pass

        Kind.program = native.NativeProgram((), PAGE, Kind)
        return weakref.ref(Kind)

    assert _freed(native.NativeProgram, build)


def test_a_program_is_freed_with_its_tile_guard(native) -> None:
    def build():
        keeper = _Keeper(native.NativeProgram((), PAGE, Continuation))
        keeper.owner.bind_tile_guard(lambda: keeper is not None)
        return weakref.ref(keeper)

    assert _freed(native.NativeProgram, build)


def test_a_program_is_freed_with_its_pages(native) -> None:
    def build():
        pages = {}
        keeper = _Keeper(native.NativeProgram([(0, PAGE, pages)], PAGE, Continuation))
        pages["keeper"] = keeper
        return weakref.ref(keeper)

    assert _freed(native.NativeProgram, build)


def test_a_program_is_freed_with_the_buffer_it_leases(native) -> None:
    def build():
        leased = _Leased()
        leased.owner = native.NativeProgram(
            (), PAGE, Continuation, dense_regions=[(0, PAGE, leased)])
        return weakref.ref(leased)

    assert _freed(native.NativeProgram, build)


def test_a_program_that_is_its_own_tile_guard_is_freed(native) -> None:
    """A bound method cannot break a cycle, so the program has to."""

    def build():
        program = native.NativeProgram((), PAGE, Continuation)
        program.bind_tile_guard(program.clear)

    assert _freed(native.NativeProgram, build)


def _dictionary(native, memory, *, pages=None, leased=None):
    sparse = [] if leased is not None else [(0, 16 * PAGE, {} if pages is None else pages)]
    dense = [] if leased is None else [(0, PAGE, leased)]
    return native.NativeDictionary(0x100, PAGE, memory, sparse, PAGE, dense, False, 0, 0)


def _define(native, dictionary, implementation) -> int:
    index, _header, _xt, _uid = dictionary.define(
        b"WORD", b"", False, native.KIND_OPAQUE, 0, 0, False, 0, 0, implementation)
    return index


def test_a_dictionary_is_freed_with_its_memory(native) -> None:
    def build():
        keeper = _Keeper()
        keeper.owner = _dictionary(native, keeper)
        return weakref.ref(keeper)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_is_freed_with_an_implementation(native) -> None:
    def build():
        keeper = _Keeper(_dictionary(native, object()))
        _define(native, keeper.owner, keeper)
        return weakref.ref(keeper)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_is_freed_with_a_cached_view(native) -> None:
    def build():
        keeper = _Keeper(_dictionary(native, object()))
        index = _define(native, keeper.owner, None)
        keeper.owner.set_view(index, keeper)
        assert keeper.owner.view(index) is keeper
        return weakref.ref(keeper)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_that_implements_its_own_word_is_freed(native) -> None:
    def build():
        dictionary = _dictionary(native, object())
        _define(native, dictionary, dictionary.record_info)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_that_is_its_own_cached_view_is_freed(native) -> None:
    def build():
        dictionary = _dictionary(native, object())
        dictionary.set_view(_define(native, dictionary, None), dictionary.view)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_is_freed_with_its_pages(native) -> None:
    def build():
        pages = {}
        keeper = _Keeper(_dictionary(native, object(), pages=pages))
        pages["keeper"] = keeper
        return weakref.ref(keeper)

    assert _freed(native.NativeDictionary, build)


def test_a_dictionary_is_freed_with_the_buffer_it_leases(native) -> None:
    def build():
        leased = _Leased()
        leased.owner = _dictionary(native, object(), leased=leased)
        return weakref.ref(leased)

    assert _freed(native.NativeDictionary, build)


class _FailingMemory:
    """Memory outside the dictionary's regions, whose reads fail on demand."""

    def __init__(self) -> None:
        self.fail = True

    def read64(self, address: int) -> int:
        if self.fail:
            raise LookupError(f"no cell at 0x{address:x}")
        return 0


OUTSIDE = 1 << 40


def _walker(native, dictionary, *, remaining_cell: int = 0x800):
    """A line walker over DICTIONARY whose input has no line left: the cell
    that counts the remaining bytes holds zero, or lies outside the regions
    when REMAINING_CELL does."""

    from simulator.runtime import DirectiveKind

    return native.NativeLineWalker(
        dictionary,
        {kind.name: kind.value for kind in DirectiveKind
         if kind is not DirectiveKind.NONAME},
        [],
        native.WALK_SOURCE,
        (remaining_cell, 0x808, 0x810, 0x818, 0x820, 0x828),
        (0x830, 0x838, 0x840, 0x848),
        0, 0, 10, 0, PAGE,
    )


def _walk(walker, location):
    """One walk that Python has blocked, with a definition open at LOCATION
    or with none open."""

    compiler = None if location is None else (b"OPEN", location, True, False, [], [], b"")
    return walker.walk((0x2000, 0x3000, 0x3000), (0x3000, 0x4000, 0x4000, 1),
                       0, compiler, True, -1, 0, PAGE, None)


def test_a_walker_is_freed_with_the_dictionary_it_walks(native) -> None:
    def build():
        dictionary = _dictionary(native, object())
        walker = _walker(native, dictionary)
        _define(native, dictionary, walker.stats)
        return weakref.ref(walker)

    assert _freed(native.NativeLineWalker, build)


def test_a_walker_is_freed_with_the_location_of_its_open_definition(native) -> None:
    def build():
        keeper = _Keeper(_walker(native, _dictionary(native, object())))
        outcome = _walk(keeper.owner, keeper)
        assert outcome[0] == 0 and outcome[7][1] is keeper
        return weakref.ref(keeper)

    assert _freed(native.NativeLineWalker, build)


def test_a_walker_that_is_its_own_location_is_freed(native) -> None:
    def build():
        walker = _walker(native, _dictionary(native, object()))
        assert _walk(walker, walker.stats)[0] == 0

    assert _freed(native.NativeLineWalker, build)


def _failed_walk(native, location):
    """A walker that keeps the state of a walk that failed with a definition
    open at LOCATION, after a later walk with none open."""

    memory = _FailingMemory()
    walker = _walker(native, _dictionary(native, memory), remaining_cell=OUTSIDE)
    with pytest.raises(LookupError):
        _walk(walker, location)
    memory.fail = False
    assert _walk(walker, None)[7] is None
    return walker


def test_a_walker_is_freed_with_the_state_of_a_failed_walk(native) -> None:
    def build():
        keeper = _Keeper()
        keeper.owner = _failed_walk(native, keeper)
        return weakref.ref(keeper)

    assert _freed(native.NativeLineWalker, build)


def test_a_walker_that_is_in_its_own_failed_state_is_freed(native) -> None:
    def build():
        memory = _FailingMemory()
        walker = _walker(native, _dictionary(native, memory), remaining_cell=OUTSIDE)
        with pytest.raises(LookupError):
            _walk(walker, walker.stats)
        memory.fail = False
        assert _walk(walker, None)[7] is None

    assert _freed(native.NativeLineWalker, build)


def test_a_failed_walk_hands_its_state_over_once(native) -> None:
    walker = _failed_walk(native, "somewhere")
    state = walker.take_error_state()
    assert state[7][1] == "somewhere"
    assert walker.take_error_state() is None


def test_a_collection_during_a_rollback_meets_only_live_records(native) -> None:
    """Releasing a removed record's implementation may run Python code, and
    the collector may then traverse the dictionary the record is leaving.
    The dictionary therefore holds only its remaining records by then."""

    sizes = []

    class Collects:
        def __del__(self) -> None:
            sizes.append(len(dictionary))
            gc.collect()

    dictionary = _dictionary(native, object())
    kept = _Keeper()
    _define(native, dictionary, kept)
    for _ in range(3):
        _define(native, dictionary, Collects())
    assert len(dictionary) == 4 and not sizes
    dictionary.publish_rollback(1, dictionary.here, False, 0, 0)
    assert sizes == [1, 1, 1]
    assert dictionary.record_info(0)[11] is kept
