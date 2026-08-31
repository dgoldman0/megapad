"""Unchanged KDOS userland partitioning and dynamic dictionary zones."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import u64
from simulator.dictionary_index import (
    DICT_INDEX_AUTHORITATIVE,
    DICT_INDEX_BOUND,
)
from simulator.errors import ForthAbort, SourceError
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_dictionary_index import (
    CANONICAL_INDEX_BYTES,
    _load_dictionary_index,
    _table_probe,
)
from tests.simulator.test_kdos_x25519 import _execute
from tests.simulator.test_kdos_xmem import (
    CANONICAL_EXTERNAL_SIZE,
    _pointer,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-userland-2425-2574.f"

FIRST_LINE = 2425
LAST_LINE = 2574
SLICE_SHA256 = (
    "525ef47587fe671593eb0161da47ed3b79c4bb78e6fdce16b91cb1ff5bfdb208"
)
SLICE_GIT_BLOB = "9d5081372d090b8f865cca64d969c62361da2c8f"
DEFINITIONS = (
    b"ULAND",
    b"SYS-HERE-SAVE",
    b"U-DICT-HERE",
    b"U-DICT-BASE",
    b"U-DICT-LIMIT",
    b"U-INIT-DONE",
    b"U-XMEM-RESERVE",
    b"_U-AVAILABLE",
    b"U-ZONE-SIZE",
    b"U-XMEM-RESERVE!",
    b"_U-XMEM-FREE-SPAN-CHECK",
    b"USERLAND-INIT",
    b"ENTER-USERLAND",
    b"LEAVE-USERLAND",
    b"U-HERE",
    b"U-USED",
    b"U-FREE",
    b".USERLAND",
)
BIOS_WORDS = (
    b"DICT-BOUNDS!",
    b"DICT-BOUNDS-OFF",
    b"DICT-BASE@",
    b"DICT-LIMIT@",
)

CANONICAL_USER_BASE = EXTERNAL_BASE + CANONICAL_INDEX_BYTES
CANONICAL_EXTERNAL_END = EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE
CANONICAL_AVAILABLE = CANONICAL_EXTERNAL_END - CANONICAL_USER_BASE
CANONICAL_RESERVE = CANONICAL_AVAILABLE // 2
CANONICAL_USER_LIMIT = CANONICAL_EXTERNAL_END - CANONICAL_RESERVE


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 6_504
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_userland(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_userland(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_userland(_load_dictionary_index(runtime))


@pytest.fixture
def loaded_userland() -> MegaForthRuntime:
    return _load_userland()


def _runtime_with_external_size(size: int) -> MegaForthRuntime:
    return _load_userland(
        MegaForthRuntime(memory=create_one_core_address_space(external_size=size))
    )


def _catch(runtime: MegaForthRuntime, name: str) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    word = runtime.find(name)
    assert word is not None
    context.data.push(word.xt)
    runtime.execute("CATCH", step_budget=250_000)
    result = context.data.snapshot()
    context.data.clear()
    assert context.returns.snapshot() == ()
    return result


def test_userland_slice_is_exact_and_load_time_only_binds_the_free_hook(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    assert len(DEFINITIONS) == 18
    for name in DEFINITIONS + BIOS_WORDS:
        assert runtime.find(name) is not None

    for name in DEFINITIONS[:8]:
        assert _pointer(runtime, name.decode("ascii")) == 0
    assert _execute(runtime, "DICT-BASE@") == (0,)
    assert _execute(runtime, "DICT-LIMIT@") == (0,)
    assert runtime.dictionary.active_zone[1] == runtime.memory.regions[0].limit
    assert runtime.dictionary_index_state.count == len(
        {word.name.upper() for word in runtime.dictionary.words}
    )
    deferred = runtime.find("_XMEM-FREE-SPAN-CHECK")
    hook = runtime.find("_U-XMEM-FREE-SPAN-CHECK")
    assert deferred is not None
    assert hook is not None
    assert runtime.memory.read64(deferred.body_address) == hook.xt


def test_bounds_validate_geometry_consume_inputs_and_preserve_old_pair(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    external_end = EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE
    old_base = EXTERNAL_BASE + 3
    old_limit = EXTERNAL_BASE + 0x123
    invalid = (
        ("ZERO-BASE", 0, 1),
        ("ZERO-LIMIT", EXTERNAL_BASE, 0),
        ("EMPTY", EXTERNAL_BASE, EXTERNAL_BASE),
        ("REVERSED", EXTERNAL_BASE + 1, EXTERNAL_BASE),
        ("BELOW", EXTERNAL_BASE - 1, EXTERNAL_BASE + 1),
        ("PAST", external_end - 1, external_end + 1),
    )
    definitions = [b"VARIABLE _U-FAULT-DEPTH"]
    definitions.append(
        b": _U-RECORD-BOUNDS-FAULT DEPTH _U-FAULT-DEPTH ! -8 THROW ;"
    )
    definitions.extend(
        f": _U-BAD-{name} {base} {limit} DICT-BOUNDS! ;".encode("ascii")
        for name, base, limit in invalid
    )
    runtime.evaluate(b"\n".join(definitions) + b"\n", source_name="bounds-cases")
    previous_fault_xt = runtime.dictionary_fault_xt
    recorder = runtime.find("_U-RECORD-BOUNDS-FAULT")
    assert recorder is not None
    assert _execute(runtime, "DICT-FAULT-XT!", recorder.xt) == ()

    # No alignment or allocator-ownership rule exists at this BIOS seam.
    assert _execute(runtime, "DICT-BOUNDS!", old_base, old_limit) == ()
    assert _execute(runtime, "DICT-BASE@") == (old_base,)
    assert _execute(runtime, "DICT-LIMIT@") == (old_limit,)

    for name, _base, _limit in invalid:
        assert _catch(runtime, f"_U-BAD-{name}") == (u64(-8),)
        assert _pointer(runtime, "_U-FAULT-DEPTH") == 0
        assert _execute(runtime, "DICT-BASE@") == (old_base,)
        assert _execute(runtime, "DICT-LIMIT@") == (old_limit,)

    assert _execute(runtime, "DICT-FAULT-XT!", previous_fault_xt) == ()
    assert _execute(
        runtime,
        "DICT-BOUNDS!",
        external_end - 1,
        external_end,
    ) == ()
    assert _execute(runtime, "DICT-BASE@") == (external_end - 1,)
    assert _execute(runtime, "DICT-LIMIT@") == (external_end,)
    assert _execute(runtime, "DICT-BOUNDS!", 0, 0) == ()
    assert _execute(runtime, "DICT-BASE@") == (0,)
    assert _execute(runtime, "DICT-LIMIT@") == (0,)


def test_canonical_init_seals_equal_capacity_without_moving_system_here(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    system_here = runtime.dictionary.here
    assert _pointer(runtime, "XMEM-HERE") == CANONICAL_USER_BASE

    assert _execute(runtime, "USERLAND-INIT") == ()

    assert runtime.dictionary.here == system_here
    assert _pointer(runtime, "U-INIT-DONE") == 1
    assert _pointer(runtime, "_U-AVAILABLE") == CANONICAL_AVAILABLE
    assert _pointer(runtime, "U-DICT-BASE") == CANONICAL_USER_BASE
    assert _pointer(runtime, "U-DICT-HERE") == CANONICAL_USER_BASE
    assert _pointer(runtime, "U-DICT-LIMIT") == CANONICAL_USER_LIMIT
    assert _execute(runtime, "U-ZONE-SIZE") == (CANONICAL_RESERVE,)
    assert _pointer(runtime, "XMEM-HERE") == CANONICAL_USER_LIMIT
    assert _pointer(runtime, "XMEM-FLOOR") == CANONICAL_USER_LIMIT
    assert _execute(runtime, "XMEM-FREE") == (CANONICAL_RESERVE,)
    assert _execute(runtime, "DICT-BASE@") == (0,)
    assert _execute(runtime, "DICT-LIMIT@") == (0,)
    assert _execute(runtime, ".USERLAND") == ()
    status = runtime.drain_uart_output()
    assert b"Mode  = system\r\n" in status
    assert f"Base  = {CANONICAL_USER_BASE} \r\n".encode("ascii") in status
    assert f"Limit = {CANONICAL_USER_LIMIT} \r\n".encode("ascii") in status
    assert f"Free  = {CANONICAL_RESERVE} bytes\r\n".encode("ascii") in status
    assert f"XMEM reserve = {CANONICAL_RESERVE} bytes\r\n".encode("ascii") in status

    state = tuple(
        _pointer(runtime, name)
        for name in (
            "U-INIT-DONE",
            "U-DICT-BASE",
            "U-DICT-HERE",
            "U-DICT-LIMIT",
            "XMEM-HERE",
            "XMEM-FLOOR",
        )
    )
    assert _execute(runtime, "USERLAND-INIT") == ()
    assert tuple(
        _pointer(runtime, name)
        for name in (
            "U-INIT-DONE",
            "U-DICT-BASE",
            "U-DICT-HERE",
            "U-DICT-LIMIT",
            "XMEM-HERE",
            "XMEM-FLOOR",
        )
    ) == state

    assert _execute(runtime, "XMEM-ALLOT", 64) == (CANONICAL_USER_LIMIT,)
    assert _execute(runtime, "XMEM-RESET") == ()
    assert _pointer(runtime, "XMEM-HERE") == CANONICAL_USER_LIMIT
    assert _pointer(runtime, "XMEM-FLOOR") == CANONICAL_USER_LIMIT


def test_rebinding_bounds_around_external_here_updates_the_write_ceiling(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    zone_base = CANONICAL_EXTERNAL_END - 0x100
    runtime.evaluate(
        b": _U-WRITE-AFTER-REBIND 0x1122334455667788 , ;\n"
        b": _U-INVALID-EXTERNAL-REBIND 0 1 DICT-BOUNDS! ;\n",
        source_name="bounds-rebind-writer",
    )
    system_here = runtime.dictionary.here

    assert _execute(runtime, "DICT-BOUNDS!", zone_base, zone_base + 64) == ()
    assert _execute(
        runtime,
        "ALLOT",
        u64(zone_base + 60 - runtime.dictionary.here),
    ) == ()
    assert runtime.dictionary.active_zone == (zone_base, zone_base + 64)

    assert _execute(runtime, "DICT-BOUNDS!", zone_base, zone_base + 128) == ()
    assert runtime.dictionary.active_zone == (zone_base, zone_base + 128)
    assert _execute(runtime, "_U-WRITE-AFTER-REBIND") == ()
    assert runtime.memory.read64(zone_base + 60) == 0x1122_3344_5566_7788
    assert runtime.dictionary.here == zone_base + 68

    old_state = (
        runtime.dictionary_base,
        runtime.dictionary_limit,
        runtime.dictionary.active_zone,
        runtime.dictionary.here,
    )
    assert _catch(runtime, "_U-INVALID-EXTERNAL-REBIND") == (u64(-8),)
    assert (
        runtime.dictionary_base,
        runtime.dictionary_limit,
        runtime.dictionary.active_zone,
        runtime.dictionary.here,
    ) == old_state

    external_here = runtime.dictionary.here
    assert _execute(runtime, "DICT-BOUNDS-OFF") == ()
    assert runtime.dictionary.here == external_here
    assert runtime.dictionary.active_zone == (zone_base, zone_base + 128)
    assert _execute(runtime, "DICT-BOUNDS!", zone_base, zone_base + 128) == ()
    assert _execute(runtime, "DICT-BOUNDS!", 0, 0) == ()
    assert runtime.dictionary.here == external_here
    assert runtime.dictionary.active_zone == (zone_base, zone_base + 128)
    assert _execute(
        runtime,
        "ALLOT",
        u64(system_here - runtime.dictionary.here),
    ) == ()
    assert runtime.dictionary.here == system_here
    assert runtime.dictionary.active_zone[1] == runtime.memory.regions[0].limit


def test_enter_leave_preserve_one_linked_dictionary_across_both_zones(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    system_here = runtime.dictionary.here

    assert _execute(runtime, "ENTER-USERLAND") == ()
    assert runtime.dictionary.here == CANONICAL_USER_BASE
    assert runtime.dictionary.active_zone == (
        CANONICAL_USER_BASE,
        CANONICAL_USER_LIMIT,
    )
    assert _pointer(runtime, "ULAND") == 1
    assert runtime.dictionary_base == CANONICAL_USER_BASE
    assert runtime.dictionary_limit == CANONICAL_USER_LIMIT
    entry_here = runtime.dictionary.here
    assert _execute(runtime, "ENTER-USERLAND") == ()
    assert runtime.dictionary.here == entry_here

    runtime.evaluate(b": U-PARSE-WORD BL WORD ;\n", source_name="user-word")
    transient_here = runtime.dictionary.here
    runtime.evaluate(b"U-PARSE-WORD external-token\n", source_name="user-token")
    transient = runtime.main_context.data.pop()
    assert transient == transient_here
    assert runtime.dictionary.here == transient_here
    assert runtime.memory.read_bytes(transient, 16) == b"\x0eexternal-token\0"

    runtime.evaluate(b"CREATE U-CROSS-DATA 8 ALLOT\n", source_name="user-create")
    data_word = runtime.find("U-CROSS-DATA")
    assert data_word is not None
    assert _execute(runtime, "U-CROSS-DATA") == (data_word.body_address,)
    unaligned_here = runtime.dictionary.here
    assert _execute(runtime, "TALIGN") == ()
    assert runtime.dictionary.here == (unaligned_here + 63) & ~63
    assert _execute(runtime, "U-HERE") == (runtime.dictionary.here,)
    assert _execute(runtime, "U-USED") == (
        runtime.dictionary.here - CANONICAL_USER_BASE,
    )
    assert _execute(runtime, "U-FREE") == (
        CANONICAL_USER_LIMIT - runtime.dictionary.here,
    )

    runtime.evaluate(b": U-CROSS 11 ;\n", source_name="user-cross-retained")
    retained = runtime.find("U-CROSS")
    assert retained is not None
    assert CANONICAL_USER_BASE <= retained.header_address < CANONICAL_USER_LIMIT
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_count = runtime.dictionary_index_state.count

    runtime.evaluate(
        b": u-cross 99 ;\n: U-ROLLBACK-TEMP 88 ;\n",
        source_name="user-cross-rollback",
    )
    assert _execute(runtime, "U-CROSS") == (99,)
    assert _execute(runtime, "DICT-ROLLBACK", saved_here, saved_latest) == ()
    assert runtime.find("U-CROSS") is retained
    assert runtime.find("U-ROLLBACK-TEMP") is None
    assert _execute(runtime, "U-CROSS") == (11,)
    assert runtime.dictionary_index_state.count == saved_count
    assert runtime.dictionary_index_state.flags == (
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE
    )
    assert _table_probe(runtime, b"u-cross")[1] == retained.header_address

    user_here = runtime.dictionary.here
    assert _execute(runtime, "LEAVE-USERLAND") == ()
    assert runtime.dictionary.here == system_here
    assert runtime.dictionary.active_zone[1] == runtime.memory.regions[0].limit
    assert _pointer(runtime, "U-DICT-HERE") == user_here
    assert _pointer(runtime, "ULAND") == 0
    assert runtime.dictionary_base == runtime.dictionary_limit == 0
    assert _execute(runtime, "U-CROSS") == (11,)
    assert _execute(runtime, "U-HERE") == (user_here,)
    assert _execute(runtime, "U-USED") == (user_here - CANONICAL_USER_BASE,)
    assert _execute(runtime, "U-FREE") == (CANONICAL_USER_LIMIT - user_here,)
    assert _execute(runtime, "LEAVE-USERLAND") == ()
    assert runtime.dictionary.here == system_here

    runtime.evaluate(b": SYS-AFTER-U 22 ;\n", source_name="system-after-user")
    system_word = runtime.find("SYS-AFTER-U")
    assert system_word is not None
    assert system_word.header_address == system_here
    assert runtime.memory.read64(system_word.header_address) == retained.header_address

    assert _execute(runtime, "ENTER-USERLAND") == ()
    assert runtime.dictionary.here == user_here
    runtime.evaluate(b": U-AFTER-SYS 33 ;\n", source_name="user-after-system")
    external_word = runtime.find("U-AFTER-SYS")
    assert external_word is not None
    assert runtime.memory.read64(external_word.header_address) == system_word.header_address
    assert _execute(runtime, "SYS-AFTER-U") == (22,)
    assert _execute(runtime, "U-AFTER-SYS") == (33,)


def test_exact_limit_allot_succeeds_and_next_store_faults_atomically(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    assert _execute(runtime, "ENTER-USERLAND") == ()
    runtime.evaluate(b": _U-COMMA-OVER 123 , ;\n", source_name="comma-over")
    limit = _pointer(runtime, "U-DICT-LIMIT")
    runtime.memory.write8(limit, 0xA5)

    free = _execute(runtime, "U-FREE")[0]
    assert _execute(runtime, "ALLOT", free) == ()
    assert runtime.dictionary.here == limit
    assert _catch(runtime, "_U-COMMA-OVER") == (u64(-8),)
    assert runtime.dictionary.here == limit
    assert runtime.memory.read8(limit) == 0xA5
    assert runtime.dictionary_base == _pointer(runtime, "U-DICT-BASE")
    assert runtime.dictionary_limit == limit

    assert _execute(runtime, "LEAVE-USERLAND") == ()
    assert _pointer(runtime, "U-DICT-HERE") == limit
    assert runtime.dictionary_base == runtime.dictionary_limit == 0


def test_explicit_reserve_rounds_once_and_policy_becomes_immutable() -> None:
    runtime = _load_userland()
    runtime.main_context.data.push(u64(-1))
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("U-XMEM-RESERVE!", step_budget=250_000)
    assert runtime.drain_uart_output() == b"Invalid XMEM reserve"
    assert runtime.main_context.data.snapshot() == ()
    assert _pointer(runtime, "U-XMEM-RESERVE") == 0
    assert _pointer(runtime, "U-INIT-DONE") == 0

    assert _execute(runtime, "U-XMEM-RESERVE!", 17) == ()
    assert _pointer(runtime, "U-XMEM-RESERVE") == 17
    assert _execute(runtime, "USERLAND-INIT") == ()
    limit = _pointer(runtime, "U-DICT-LIMIT")
    assert CANONICAL_EXTERNAL_END - limit == 32
    state = (
        _pointer(runtime, "U-DICT-BASE"),
        limit,
        _pointer(runtime, "XMEM-HERE"),
        _pointer(runtime, "XMEM-FLOOR"),
    )

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.main_context.data.push(64)
        runtime.execute("U-XMEM-RESERVE!", step_budget=250_000)
    assert runtime.drain_uart_output() == b"Userland partition already initialized"
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert (
        _pointer(runtime, "U-DICT-BASE"),
        _pointer(runtime, "U-DICT-LIMIT"),
        _pointer(runtime, "XMEM-HERE"),
        _pointer(runtime, "XMEM-FLOOR"),
    ) == state


def test_absent_and_too_small_xmem_leave_partition_publication_off() -> None:
    absent = _runtime_with_external_size(0)
    system_here = absent.dictionary.here
    assert _execute(absent, "USERLAND-INIT") == ()
    assert _execute(absent, "ENTER-USERLAND") == ()
    assert absent.drain_uart_output() == b"No ext mem -- userland disabled\r\n"
    assert absent.dictionary.here == system_here
    assert _pointer(absent, "U-INIT-DONE") == 0
    assert _pointer(absent, "ULAND") == 0
    assert absent.dictionary_base == absent.dictionary_limit == 0

    tiny = _runtime_with_external_size(16)
    before = (
        tiny.dictionary.here,
        _pointer(tiny, "XMEM-HERE"),
        _pointer(tiny, "XMEM-FLOOR"),
    )
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        tiny.execute("USERLAND-INIT", step_budget=250_000)
    assert tiny.drain_uart_output() == b"Insufficient ext mem for userland dictionary"
    assert _pointer(tiny, "_U-AVAILABLE") == 16
    assert _pointer(tiny, "U-INIT-DONE") == 0
    assert _pointer(tiny, "U-DICT-BASE") == 0
    assert _pointer(tiny, "U-DICT-HERE") == 0
    assert _pointer(tiny, "U-DICT-LIMIT") == 0
    assert tiny.dictionary_base == tiny.dictionary_limit == 0
    assert (
        tiny.dictionary.here,
        _pointer(tiny, "XMEM-HERE"),
        _pointer(tiny, "XMEM-FLOOR"),
    ) == before


@pytest.mark.parametrize(
    ("external_size", "user_base", "user_limit", "reserve"),
    (
        (17, EXTERNAL_BASE, EXTERNAL_BASE + 1, 16),
        (1_024, EXTERNAL_BASE, EXTERNAL_BASE + 512, 512),
        (2_048, EXTERNAL_BASE + 16, EXTERNAL_BASE + 1_024, 1_024),
    ),
)
def test_small_present_geometries_follow_capacity_instead_of_a_fixed_zone(
    external_size: int,
    user_base: int,
    user_limit: int,
    reserve: int,
) -> None:
    runtime = _runtime_with_external_size(external_size)
    assert _execute(runtime, "USERLAND-INIT") == ()
    assert _pointer(runtime, "U-INIT-DONE") == 1
    assert _pointer(runtime, "U-DICT-BASE") == user_base
    assert _pointer(runtime, "U-DICT-HERE") == user_base
    assert _pointer(runtime, "U-DICT-LIMIT") == user_limit
    assert _pointer(runtime, "XMEM-HERE") == user_limit
    assert _pointer(runtime, "XMEM-FLOOR") == user_limit
    assert _execute(runtime, "XMEM-FREE") == (reserve,)
    assert runtime.dictionary_base == runtime.dictionary_limit == 0

    if external_size == 17:
        assert user_limit % 16 == 1
        assert _execute(runtime, "XMEM-ALLOT", 1) == (user_limit,)
        assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 17
        assert _execute(runtime, "XMEM-RESET") == ()
        assert _pointer(runtime, "XMEM-HERE") == user_limit

        assert _execute(runtime, "ENTER-USERLAND") == ()
        assert _execute(runtime, "U-FREE") == (1,)
        assert _execute(runtime, "ALLOT", 1) == ()
        assert runtime.dictionary.here == user_limit
        assert _execute(runtime, "LEAVE-USERLAND") == ()
        assert _pointer(runtime, "U-DICT-HERE") == user_limit


def test_free_hook_accepts_preinit_boundary_and_rejects_dictionary_overlap(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    preinit = _execute(runtime, "XMEM-ALLOT", 32)[0]
    assert _execute(runtime, "ENTER-USERLAND") == ()
    assert preinit + 32 == _pointer(runtime, "U-DICT-BASE")

    assert _execute(runtime, "XMEM-FREE-BLOCK", preinit, 32) == ()
    assert _execute(runtime, "XMEM-ALLOT", 32) == (preinit,)

    free_list = _pointer(runtime, "XMEM-FL")
    overlap = _pointer(runtime, "U-DICT-LIMIT") - 16
    runtime.memory.write64(overlap, 0x1122_3344_5566_7788)
    runtime.memory.write64(overlap + 8, 0x8877_6655_4433_2211)
    metadata = runtime.memory.read_bytes(overlap, 16)
    context = runtime.main_context
    context.data.push(overlap)
    context.data.push(16)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("XMEM-FREE-BLOCK", step_budget=250_000)
    assert runtime.drain_uart_output() == b"XMEM-FREE: user dictionary overlap"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.memory.read_bytes(overlap, 16) == metadata
    assert _pointer(runtime, "XMEM-FL") == free_list


def test_preinit_status_exposes_the_source_reserve_reporting_discrepancy(
    loaded_userland: MegaForthRuntime,
) -> None:
    runtime = loaded_userland
    assert _execute(runtime, ".USERLAND") == ()
    output = runtime.drain_uart_output()
    assert b"Userland:\r\n" in output
    assert b"Mode  = system\r\n" in output
    assert b"Used  = 0 bytes\r\n" in output
    assert b"Free  = 0 bytes\r\n" in output
    assert (
        f"XMEM reserve = {CANONICAL_EXTERNAL_END} bytes\r\n".encode("ascii")
        in output
    )


def test_next_contiguous_frontier_compiles_arena_then_stops_at_left_bracket(
    loaded_userland: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[2574:2796])
    assert next_source.startswith(b"\n")
    assert next_source.endswith(
        ": IDLE  ( -- )  [ 0 C, ] ;  \\ IDL opcode — yield CPU until next interrupt\n".encode()
    )

    with pytest.raises(SourceError, match="unknown word") as caught:
        loaded_userland.evaluate(
            next_source,
            source_name="kdos.f:2575-2796",
        )
    assert caught.value.location.line == 222
    assert caught.value.location.column == 16
    assert caught.value.message == "unknown word b'['"
