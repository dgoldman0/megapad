"""Focused Python/native parity tests for the full-core EXT.DICT cache."""

from __future__ import annotations

from collections import Counter

import pytest

from accel_wrapper import Megapad64 as NativeMegapad64
from asm import assemble
from devices import MMIO_BASE
from megapad64 import (
    DICT_NUM_SETS,
    DICT_NUM_WAYS,
    IVEC_PRIV_FAULT,
    Megapad64 as PythonMegapad64,
    TrapError,
)
from system import MegapadSystem


FULL_CORE_TYPES = (
    pytest.param(PythonMegapad64, id="python"),
    pytest.param(NativeMegapad64, id="native"),
)

_INSTRUCTIONS = (
    assemble("dins r0, r9"),
    assemble("dfind r1, r9"),
    assemble("ddel r0, r9"),
    assemble("dclr"),
    assemble("dupd r0, r9"),
)
assert all(len(instruction) == 3 for instruction in _INSTRUCTIONS)
assert _INSTRUCTIONS[-1] == bytes((0xFA, 0x04, 0x09))

_DINS_PC = 0
_DFIND_PC = 3
_DDEL_PC = 6
_DCLR_PC = 9
_DUPD_PC = 12
_CODE = b"".join(_INSTRUCTIONS)
_NAME_BASE = 0x1000
_NAME_STRIDE = 0x40
_STRICT_MUTATOR_PC = 0x200
_SPAN_CODE_BASE = 0x100
_SPAN_NAME = b"\x00Span\xff"

# Every name hashes to set 0x5e when all eight low FNV-1a bits are used.
_SET_COLLIDERS = (
    b"K00044",
    b"K00109",
    b"K00192",
    b"K00350",
    b"K00431",
    b"K00585",
)

# These names all have FNV low-six bits 0x1e, but occupy four distinct
# low-eight-bit sets.  Retaining all eight proves the native path did not
# accidentally preserve the old 64-set mask.
_SAME_LOW6_NAMES = (
    b"S00707",
    b"S00772",
    b"S00026",
    b"S00413",
    b"S00080",
    b"S00134",
    b"S00329",
    b"S00651",
)


def _fnv1a_32(name: bytes) -> int:
    value = 0x811C9DC5
    for byte in name:
        value = ((value ^ byte) * 0x01000193) & 0xFFFF_FFFF
    return value


def _new_cpu(cpu_type):
    cpu = cpu_type(mem_size=0x4000)
    cpu.load_bytes(0, _CODE)
    return cpu


def _prime_instruction_cache(
    system: MegapadSystem,
    address: int,
    size: int,
) -> None:
    """Keep the strict-cycle oracle focused on EXT.DICT replay."""
    if size <= 0:
        return
    first_line = address & ~0xF
    last_line = (address + size - 1) & ~0xF
    for cpu in system.cores[:system.num_full_cores]:
        valid_bytes, tags, data_bytes = cpu._cs.icache_snapshot()
        valid = bytearray(valid_bytes)
        tags = list(tags)
        data = bytearray(data_bytes)
        line_address = first_line
        while line_address <= last_line:
            index = (line_address >> 4) & 0xFF
            valid[index] = 1
            tags[index] = line_address >> 12
            data_offset = index * 16
            data[data_offset:data_offset + 16] = cpu.mem[
                line_address:line_address + 16
            ]
            line_address += 16
        cpu._cs.icache_restore(bytes(valid), tags, bytes(data))


def _new_cycle_system(mutator: bytes) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=0x4000,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.load_binary(0, _CODE)
    system.load_binary(_STRICT_MUTATOR_PC, mutator)
    system.boot(entry=0)
    _prime_instruction_cache(system, 0, len(_CODE))
    _prime_instruction_cache(
        system,
        _STRICT_MUTATOR_PC,
        len(mutator),
    )
    return system


def _seed_names(cpu, names: tuple[bytes, ...]) -> dict[bytes, int]:
    addresses = {}
    for index, name in enumerate(names):
        address = _NAME_BASE + index * _NAME_STRIDE
        cpu.load_bytes(address, bytes((len(name),)) + name)
        addresses[name] = address
    return addresses


def _execute(cpu, pc: int, name_address: int = 0, xt: int | None = None) -> int:
    cpu.halted = False
    cpu.idle = False
    cpu.regs[9] = name_address
    if xt is not None:
        cpu.regs[0] = xt
    cpu.pc = pc
    return cpu.step()


def _insert(cpu, name: bytes, address: int, xt: int) -> int:
    cpu.flag_z = 0
    cpu.flag_v = 1
    cycles = _execute(cpu, _DINS_PC, address, xt)
    assert cycles == len(name) + 3
    assert (cpu.flag_z, cpu.flag_v) == (1, 0)
    return cycles


def _find(cpu, name: bytes, address: int) -> int | None:
    cpu.regs[1] = 0xBAD0_BAD0_BAD0_BAD0
    cpu.flag_v = 1
    cycles = _execute(cpu, _DFIND_PC, address)
    assert cycles == len(name) + 3
    assert cpu.flag_v == 0
    if cpu.flag_z:
        return int(cpu.regs[1])
    assert cpu.regs[1] == 0
    return None


def _run_dict_name_span_workload(cpu_type, region: str) -> tuple:
    cpu = _new_cpu(cpu_type)
    payload = bytes((0xE0 | len(_SPAN_NAME),)) + _SPAN_NAME
    if region == "bank0":
        address = _NAME_BASE
        cpu.load_bytes(address, payload)
    else:
        aperture = bytearray(0x80)
        address = 0x10_0000 + 0x20
        aperture[0x20:0x20 + len(payload)] = payload
        cpu.attach_ext_mem(aperture, 0x10_0000, len(aperture))

    inserted = _insert(cpu, _SPAN_NAME, address, 0xCAFE)
    found = _find(cpu, _SPAN_NAME, address)
    return inserted, found, cpu.cycle_count, cpu.flags_pack()


@pytest.mark.parametrize("region", ("bank0", "external"))
def test_dict_name_span_contiguous_memory_matches_python(region: str) -> None:
    observed = tuple(
        _run_dict_name_span_workload(cpu_type, region)
        for cpu_type in (PythonMegapad64, NativeMegapad64)
    )
    assert observed == (
        (len(_SPAN_NAME) + 3, 0xCAFE, 2 * (len(_SPAN_NAME) + 3), 0x01),
    ) * 2


def test_dict_name_span_empty_name_keeps_one_byte_path() -> None:
    observed = []
    for cpu_type in (PythonMegapad64, NativeMegapad64):
        cpu = _new_cpu(cpu_type)
        cpu.load_bytes(_NAME_BASE, b"\xe0")
        inserted = _insert(cpu, b"", _NAME_BASE, 0xC0DE)
        found = _find(cpu, b"", _NAME_BASE)
        observed.append(
            (inserted, found, cpu.cycle_count, cpu.flags_pack())
        )

    assert observed == [(3, 0xC0DE, 6, 0x01)] * 2


def _run_cross_span_name_workload(cpu_type, layout: str) -> tuple:
    cpu = cpu_type(mem_size=0x4000)
    cpu.load_bytes(_SPAN_CODE_BASE, _CODE)
    if layout == "bank0-wrap":
        address = len(cpu.mem) - 1
        cpu.mem[-1] = 0xE0 | len(_SPAN_NAME)
        cpu.mem[:len(_SPAN_NAME)] = _SPAN_NAME
    else:
        aperture = bytearray(0x20)
        cpu.attach_ext_mem(aperture, 0x10_0000, len(aperture))
        address = 0x10_0000 + len(aperture) - 1
        aperture[-1] = 0xE0 | len(_SPAN_NAME)
        bank0_offset = (address + 1) % len(cpu.mem)
        cpu.mem[bank0_offset:bank0_offset + len(_SPAN_NAME)] = _SPAN_NAME

    insert_cycles = _execute(
        cpu, _SPAN_CODE_BASE + _DINS_PC, address, 0xBEEF
    )
    cpu.regs[1] = 0
    find_cycles = _execute(cpu, _SPAN_CODE_BASE + _DFIND_PC, address)
    return (
        insert_cycles,
        find_cycles,
        cpu.regs[1],
        cpu.flag_z,
        cpu.cycle_count,
    )


@pytest.mark.parametrize("layout", ("bank0-wrap", "external-end"))
def test_dict_name_span_cross_boundary_fallback_matches_python(
    layout: str,
) -> None:
    expected_cycles = len(_SPAN_NAME) + 3
    observed = tuple(
        _run_cross_span_name_workload(cpu_type, layout)
        for cpu_type in (PythonMegapad64, NativeMegapad64)
    )
    assert observed == (
        (expected_cycles, expected_cycles, 0xBEEF, 1, 2 * expected_cycles),
    ) * 2


def test_dict_name_span_mmio_fallback_reads_each_byte_at_same_cost() -> None:
    direct = _new_cpu(NativeMegapad64)
    direct.load_bytes(
        _NAME_BASE,
        bytes((len(_SPAN_NAME),)) + _SPAN_NAME,
    )
    _insert(direct, _SPAN_NAME, _NAME_BASE, 0xA55A)
    assert _find(direct, _SPAN_NAME, _NAME_BASE) == 0xA55A

    fallback = _new_cpu(NativeMegapad64)
    address = MMIO_BASE + 0x10_000
    payload = bytes((0xE0 | len(_SPAN_NAME),)) + _SPAN_NAME
    reads: list[int] = []

    def read_mmio(byte_address: int) -> int:
        reads.append(byte_address)
        return payload[byte_address - address]

    fallback._mmio_read8 = read_mmio
    _insert(fallback, _SPAN_NAME, address, 0xA55A)
    assert _find(fallback, _SPAN_NAME, address) == 0xA55A
    expected_reads = [address + index for index in range(len(payload))]
    assert reads == expected_reads * 2
    assert fallback.cycle_count == direct.cycle_count == (
        2 * (len(_SPAN_NAME) + 3)
    )


def test_dict_name_span_user_fallback_checks_each_mpu_byte() -> None:
    cpu = _new_cpu(NativeMegapad64)
    address = _NAME_BASE
    cpu.load_bytes(
        address,
        bytes((len(_SPAN_NAME),)) + _SPAN_NAME,
    )
    cpu.regs[9] = address
    cpu.pc = _DFIND_PC
    cpu.priv_level = 1
    cpu.mpu_base = address
    cpu.mpu_limit = address + 3

    with pytest.raises(TrapError) as raised:
        cpu.step()

    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == address + 3
    assert cpu.cycle_count == 0


def test_dict_name_span_strict_replay_stays_bytewise_and_checkpointed() -> None:
    system = _new_cycle_system(assemble("dfind r1, r9"))
    cpu = system.cpu
    address = _NAME_BASE
    cpu.load_bytes(
        address,
        bytes((len(_SPAN_NAME),)) + _SPAN_NAME,
    )
    _insert(cpu, _SPAN_NAME, address, 0x5AA5)
    cpu.regs[9] = address
    cpu.pc = _STRICT_MUTATOR_PC
    cycles_before = cpu.cycle_count
    saw_suspension = False

    for _ in range(64):
        result = system.run_cycle_batch(1, max_instructions=1)
        if result.instructions_executed:
            break
        saw_suspension = True
        assert result.system_stop_reason == "cycle_limit"
        assert cpu.pc == _STRICT_MUTATOR_PC
        assert cpu.cycle_count == cycles_before
        assert system._native_system.cycle_execution_pending
    else:
        pytest.fail("sliced EXT.DICT lookup did not retire")

    assert saw_suspension
    assert cpu.regs[1] == 0x5AA5
    assert cpu.flag_z == 1
    assert cpu.cycle_count > cycles_before + len(_SPAN_NAME) + 3
    assert not system._native_system.cycle_execution_pending


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_fnv_low_eight_bits_select_256_sets(cpu_type):
    assert (DICT_NUM_SETS, DICT_NUM_WAYS) == (256, 4)
    hashes = tuple(_fnv1a_32(name) for name in _SAME_LOW6_NAMES)
    assert {value & 0x3F for value in hashes} == {0x1E}
    set_counts = Counter(value & 0xFF for value in hashes)
    assert len(set_counts) == 4
    assert set(set_counts.values()) == {2}

    cpu = _new_cpu(cpu_type)
    addresses = _seed_names(cpu, _SAME_LOW6_NAMES)
    expected = {}
    for index, name in enumerate(_SAME_LOW6_NAMES, start=1):
        xt = 0x1000 + index
        _insert(cpu, name, addresses[name], xt)
        assert (cpu.flag_z, cpu.flag_v) == (1, 0)
        expected[name] = xt

    assert {
        name: _find(cpu, name, addresses[name])
        for name in _SAME_LOW6_NAMES
    } == expected


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_dins_replaces_round_robin_and_matching_update_does_not_move_cursor(
    cpu_type,
):
    assert {_fnv1a_32(name) & 0xFF for name in _SET_COLLIDERS} == {0x5E}
    cpu = _new_cpu(cpu_type)
    addresses = _seed_names(cpu, _SET_COLLIDERS)

    for index, name in enumerate(_SET_COLLIDERS[:4]):
        _insert(cpu, name, addresses[name], 0x2000 + index)

    # Updating way 1 must leave the full-set victim at way 0.
    updated_name = _SET_COLLIDERS[1]
    _insert(cpu, updated_name, addresses[updated_name], 0xBEEF)
    replacement = _SET_COLLIDERS[4]
    _insert(cpu, replacement, addresses[replacement], 0x3004)
    assert (cpu.flag_z, cpu.flag_v) == (1, 0)

    assert _find(cpu, _SET_COLLIDERS[0], addresses[_SET_COLLIDERS[0]]) is None
    assert _find(cpu, updated_name, addresses[updated_name]) == 0xBEEF
    assert _find(cpu, _SET_COLLIDERS[2], addresses[_SET_COLLIDERS[2]]) == 0x2002
    assert _find(cpu, _SET_COLLIDERS[3], addresses[_SET_COLLIDERS[3]]) == 0x2003
    assert _find(cpu, replacement, addresses[replacement]) == 0x3004

    # The replacement advanced the cursor to way 1.
    second_replacement = _SET_COLLIDERS[5]
    _insert(cpu, second_replacement, addresses[second_replacement], 0x3005)
    assert _find(cpu, updated_name, addresses[updated_name]) is None
    assert _find(cpu, second_replacement, addresses[second_replacement]) == 0x3005


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_ddel_preserves_cursor_and_first_invalid_insert_restarts_from_that_way(
    cpu_type,
):
    cpu = _new_cpu(cpu_type)
    addresses = _seed_names(cpu, _SET_COLLIDERS)
    for index, name in enumerate(_SET_COLLIDERS[:4]):
        _insert(cpu, name, addresses[name], 0x4000 + index)

    if cpu_type is PythonMegapad64:
        assert cpu._dict_next_victim[0x5E] == 0
    deleted = _SET_COLLIDERS[1]
    cpu.flag_v = 1
    cycles = _execute(cpu, _DDEL_PC, addresses[deleted])
    assert cycles == len(deleted) + 3
    assert (cpu.flag_z, cpu.flag_v) == (1, 1)
    if cpu_type is PythonMegapad64:
        assert cpu._dict_next_victim[0x5E] == 0

    # A miss changes only Z as well.
    cpu.flag_v = 1
    cycles = _execute(cpu, _DDEL_PC, addresses[deleted])
    assert cycles == len(deleted) + 3
    assert (cpu.flag_z, cpu.flag_v) == (0, 1)
    if cpu_type is PythonMegapad64:
        assert cpu._dict_next_victim[0x5E] == 0

    first_fill = _SET_COLLIDERS[4]
    _insert(cpu, first_fill, addresses[first_fill], 0x5004)
    second_fill = _SET_COLLIDERS[5]
    _insert(cpu, second_fill, addresses[second_fill], 0x5005)

    assert _find(cpu, _SET_COLLIDERS[0], addresses[_SET_COLLIDERS[0]]) == 0x4000
    assert _find(cpu, deleted, addresses[deleted]) is None
    assert _find(cpu, _SET_COLLIDERS[2], addresses[_SET_COLLIDERS[2]]) is None
    assert _find(cpu, _SET_COLLIDERS[3], addresses[_SET_COLLIDERS[3]]) == 0x4003
    assert _find(cpu, first_fill, addresses[first_fill]) == 0x5004
    assert _find(cpu, second_fill, addresses[second_fill]) == 0x5005


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_dupd_only_updates_a_resident_line_without_moving_cursor(cpu_type):
    cpu = _new_cpu(cpu_type)
    addresses = _seed_names(cpu, _SET_COLLIDERS)
    for index, name in enumerate(_SET_COLLIDERS[:4]):
        _insert(cpu, name, addresses[name], 0x6000 + index)

    missing = _SET_COLLIDERS[4]
    cpu.flag_z = 1
    cpu.flag_v = 1
    cycles = _execute(cpu, _DUPD_PC, addresses[missing], 0xDEAD)
    assert cycles == len(missing) + 3
    assert (cpu.flag_z, cpu.flag_v) == (0, 0)
    assert _find(cpu, missing, addresses[missing]) is None

    resident = _SET_COLLIDERS[2]
    cpu.flag_z = 0
    cpu.flag_v = 1
    cycles = _execute(cpu, _DUPD_PC, addresses[resident], 0xCAFE)
    assert cycles == len(resident) + 3
    assert (cpu.flag_z, cpu.flag_v) == (1, 0)

    replacement = _SET_COLLIDERS[5]
    _insert(cpu, replacement, addresses[replacement], 0x7005)
    assert _find(cpu, _SET_COLLIDERS[0], addresses[_SET_COLLIDERS[0]]) is None
    assert _find(cpu, _SET_COLLIDERS[1], addresses[_SET_COLLIDERS[1]]) == 0x6001
    assert _find(cpu, resident, addresses[resident]) == 0xCAFE
    assert _find(cpu, _SET_COLLIDERS[3], addresses[_SET_COLLIDERS[3]]) == 0x6003
    assert _find(cpu, missing, addresses[missing]) is None
    assert _find(cpu, replacement, addresses[replacement]) == 0x7005


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
@pytest.mark.parametrize("clear_kind", ("dclr", "reset"))
def test_dclr_and_reset_clear_entries_and_replacement_cursors(
    cpu_type,
    clear_kind,
):
    cpu = _new_cpu(cpu_type)
    addresses = _seed_names(cpu, _SET_COLLIDERS[:5])
    for index, name in enumerate(_SET_COLLIDERS[:5]):
        _insert(cpu, name, addresses[name], 0x8000 + index)
    if cpu_type is PythonMegapad64:
        assert cpu._dict_next_victim[0x5E] == 1

    if clear_kind == "dclr":
        cpu.flag_z = 1
        cpu.flag_v = 1
        assert _execute(cpu, _DCLR_PC) == 67
        assert (cpu.flag_z, cpu.flag_v) == (1, 1)
    else:
        cpu._reset_state()
        assert (cpu.flag_z, cpu.flag_v) == (0, 0)

    if cpu_type is PythonMegapad64:
        assert not any(cpu._dict_next_victim)

    for name in _SET_COLLIDERS[:5]:
        assert _find(cpu, name, addresses[name]) is None

    # Both backends repopulate the cleared set from its canonical empty state.
    for index, name in enumerate(_SET_COLLIDERS[:4]):
        _insert(cpu, name, addresses[name], 0x9000 + index)
    replacement = _SET_COLLIDERS[4]
    _insert(cpu, replacement, addresses[replacement], 0x9004)
    assert _find(cpu, _SET_COLLIDERS[0], addresses[_SET_COLLIDERS[0]]) is None
    assert _find(cpu, _SET_COLLIDERS[1], addresses[_SET_COLLIDERS[1]]) == 0x9001
    assert _find(cpu, replacement, addresses[replacement]) == 0x9004


@pytest.mark.parametrize(
    ("mutator", "source_register"),
    (
        pytest.param(
            assemble("ddel r0, r9"),
            9,
            id="unprefixed",
        ),
        pytest.param(
            assemble("ddel r16, r25"),
            25,
            id="rex-prefixed",
        ),
    ),
)
def test_strict_cycle_replay_restores_dictionary_before_mutator_retirement(
    mutator: bytes,
    source_register: int,
) -> None:
    """A sliced DDEL must still observe its original resident binding."""
    system = _new_cycle_system(mutator)
    cpu = system.cpu
    names = _SET_COLLIDERS[:2]
    addresses = _seed_names(cpu, names)
    _insert(cpu, names[0], addresses[names[0]], 0xA000)
    _insert(cpu, names[1], addresses[names[1]], 0xA001)

    cpu.regs[source_register] = addresses[names[0]]
    cpu.flag_z = 0
    cpu.flag_v = 1
    cpu.pc = _STRICT_MUTATOR_PC
    cycles_before = cpu.cycle_count
    saw_suspension = False

    for _ in range(64):
        result = system.run_cycle_batch(1, max_instructions=1)
        if result.instructions_executed:
            assert result.instructions_executed == 1
            break
        saw_suspension = True
        assert result.system_stop_reason == "cycle_limit"
        assert cpu.pc == _STRICT_MUTATOR_PC
        assert cpu.cycle_count == cycles_before
        assert (cpu.flag_z, cpu.flag_v) == (0, 1)
        assert system._native_system.cycle_execution_pending
    else:
        pytest.fail("sliced EXT.DICT mutator did not retire")

    assert saw_suspension
    assert not system._native_system.cycle_execution_pending
    assert cpu.pc == _STRICT_MUTATOR_PC + len(mutator)
    # If a sliced DDEL were replayed without restoring the cache, its first
    # attempt would delete the line and its retiring replay would report a
    # miss. Retiring with Z=1 therefore observes the checkpointed table.
    assert (cpu.flag_z, cpu.flag_v) == (1, 1)
    assert cpu.cycle_count > cycles_before
    assert _find(cpu, names[0], addresses[names[0]]) is None
    assert _find(cpu, names[1], addresses[names[1]]) == 0xA001


def test_strict_cycle_dclr_restores_then_commits_the_complete_cache() -> None:
    """DCLR's large checkpoint survives one bounded rollback and replay."""
    mutator = assemble("dclr")
    system = _new_cycle_system(mutator)
    cpu = system.cpu
    names = _SET_COLLIDERS[:2]
    addresses = _seed_names(cpu, names)
    _insert(cpu, names[0], addresses[names[0]], 0xB000)
    _insert(cpu, names[1], addresses[names[1]], 0xB001)

    cpu.flag_z = 1
    cpu.flag_v = 1
    cpu.pc = _STRICT_MUTATOR_PC
    cycles_before = cpu.cycle_count

    suspended = system.run_cycle_batch(1, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert suspended.system_stop_reason == "cycle_limit"
    assert cpu.pc == _STRICT_MUTATOR_PC
    assert cpu.cycle_count == cycles_before
    assert (cpu.flag_z, cpu.flag_v) == (1, 1)
    assert system._native_system.cycle_execution_pending

    retired = system.run_cycle_batch(66, max_instructions=1)

    assert retired.instructions_executed == 1
    assert retired.per_core_cycles == (67,)
    assert cpu.pc == _STRICT_MUTATOR_PC + len(mutator)
    assert cpu.cycle_count - cycles_before == 67
    assert (cpu.flag_z, cpu.flag_v) == (1, 1)
    assert not system._native_system.cycle_execution_pending
    assert _find(cpu, names[0], addresses[names[0]]) is None
    assert _find(cpu, names[1], addresses[names[1]]) is None
