"""Black-box strict-cycle contracts for the timed tile-memory fabric.

The lower-level transport tests prove the standalone arbiter.  These tests
start real full-core instructions through ``MegapadSystem.run_cycle_batch``
and use the transport snapshot only as a diagnostic observation point.  A
warm instruction cache keeps main-bus fetch traffic out of the tile-only
timeline.
"""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    CSR_TACC_CTL,
    EW_FP16,
    EW_U8,
    TACC_IMAGE_BYTES,
    TACC_OWNER_NONE,
)
from system import MegapadSystem


_BROADCAST_FP16_RMIN_R7 = bytes((0xE6, 0x01, 0x07))
_FP16_ONE_TILE = b"\x00\x3c" * 32
_FP32_ONE = 0x3F80_0000


def _system(*, cores: int = 1) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.boot(entry=0)
    return system


def _prime_instruction_cache(
    system: MegapadSystem,
    address: int,
    size: int,
) -> None:
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


def _install(
    system: MegapadSystem,
    address: int,
    code: bytes,
) -> None:
    system.load_binary(address, code)
    _prime_instruction_cache(system, address, len(code))


def _claim_and_clear(
    system: MegapadSystem,
    core_index: int = 0,
) -> None:
    cpu = system.cores[core_index]
    cpu.tmode = EW_U8
    for instruction in ("t.acc.try", "t.acc.clear"):
        code = assemble(instruction)
        _install(system, 0, code)
        cpu.pc = 0
        cpu.halted = False
        cpu.idle = False
        cpu.step()


def _engine(snapshot: dict, engine_id: int = 0) -> dict:
    matches = [
        engine
        for engine in snapshot["engines"]
        if int(engine["engine_id"]) == engine_id
    ]
    assert len(matches) == 1
    return dict(matches[0])


def _counter_state(cpu) -> tuple[int, int, int, int]:
    return (
        int(cpu.cycle_count),
        int(cpu.perf_cycles),
        int(cpu.perf_stalls),
        int(cpu.perf_tileops),
    )


def _persistent_tacc_state(cpu) -> tuple[object, ...]:
    """TACC state that must not expose a staged LOAD or partial STORE."""
    return (
        bytes(cpu.tacc),
        int(cpu.tacc_owner),
        bool(cpu.tacc_valid),
        bool(cpu.tacc_dirty),
        int(cpu.tacc_format_ew),
        int(cpu.tacc_format_signed),
        int(cpu.tacc_epoch),
    )


def test_ordinary_tile_read_uses_registered_request_grant_ack_edges():
    system = _system()
    cpu = system.cpu
    source = 0x400
    cpu.tmode = EW_FP16
    cpu.tsrc0 = source
    cpu.mem[source:source + 64] = _FP16_ONE_TILE
    cpu.acc[0] = 0xA5A5_A5A5
    cpu.regs[7] = 0

    # The assembler does not yet spell TRED broadcast forms.  E6/01/R7 is
    # the canonical SS=1 FP MIN encoding and performs one ordinary tile read.
    code = _BROADCAST_FP16_RMIN_R7 + assemble("halt")
    _install(system, 0, code)
    cpu.pc = 0
    before = _counter_state(cpu)

    captured = system.run_cycle_batch(1, max_instructions=1)

    assert captured.instructions_executed == 0
    assert captured.system_cycles_advanced == 1
    assert captured.per_core_cycles == (0,)
    assert cpu.pc == 0
    assert cpu.acc[0] == 0xA5A5_A5A5
    assert _counter_state(cpu) == before

    at_r_plus_one = dict(
        system._native_system._tacc_transport_snapshot()
    )
    request = at_r_plus_one["port"]["pending"][0]
    assert request is not None
    assert request["engine_id"] == 0
    assert request["owner_core_id"] == 0
    assert request["ready_cycle"] == 0
    assert request["direction"] == "read"
    assert request["address"] == source
    assert not request["image_transfer"]
    assert at_r_plus_one["port"]["active_grant"] is None
    assert at_r_plus_one["port"]["grant_count"] == 0

    acknowledged = system.run_cycle_batch(1, max_instructions=1)

    assert acknowledged.instructions_executed == 1
    assert acknowledged.system_cycles_advanced == 1
    assert acknowledged.stop_cycle == 2
    assert acknowledged.per_core_cycles == (2,)
    assert cpu.pc == len(_BROADCAST_FP16_RMIN_R7)
    assert cpu.acc[0] == _FP32_ONE
    assert (
        cpu.cycle_count - before[0],
        cpu.perf_cycles - before[1],
        cpu.perf_stalls - before[2],
        cpu.perf_tileops - before[3],
    ) == (2, 2, 1, 1)

    at_r_plus_two = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert all(
        request is None
        for request in at_r_plus_two["port"]["pending"]
    )
    assert at_r_plus_two["port"]["active_grant"] is None
    assert at_r_plus_two["port"]["last_grant_engine_id"] == 0
    assert at_r_plus_two["port"]["grant_count"] == 1
    assert tuple(at_r_plus_two["port"]["grant_counts"]) == (1,)


def test_ordinary_tile_write_commits_once_at_its_registered_ack():
    system = _system()
    cpu = system.cpu
    source = 0x400
    destination = 0x600
    source_tile = bytes(range(64))
    sentinel = bytes([0xCC]) * 64
    expected = bytes((value + 1) & 0xFF for value in source_tile)
    cpu.tmode = EW_U8
    cpu.tsrc0 = source
    cpu.tdst = destination
    cpu.regs[7] = 1
    cpu.mem[source:source + 64] = source_tile
    cpu.mem[destination:destination + 64] = sentinel
    code = assemble("t.add r7\nhalt")
    _install(system, 0, code)
    cpu.pc = 0
    before = _counter_state(cpu)

    captured_read = system.run_cycle_batch(1, max_instructions=1)
    assert captured_read.instructions_executed == 0
    assert bytes(cpu.mem[destination:destination + 64]) == sentinel
    first = system._native_system._tacc_transport_snapshot()
    read_request = first["port"]["pending"][0]
    assert read_request is not None
    assert read_request["ready_cycle"] == 0
    assert read_request["direction"] == "read"
    assert read_request["address"] == source
    assert not read_request["image_transfer"]

    read_ack = system.run_cycle_batch(1, max_instructions=1)
    assert read_ack.instructions_executed == 0
    assert bytes(cpu.mem[destination:destination + 64]) == sentinel
    second = system._native_system._tacc_transport_snapshot()
    write_request = second["port"]["pending"][0]
    assert write_request is not None
    assert write_request["ready_cycle"] == 2
    assert write_request["direction"] == "write"
    assert write_request["address"] == destination
    assert bytes(write_request["data"]) == expected
    assert not write_request["image_transfer"]
    assert second["port"]["grant_count"] == 1

    before_write_ack = system.run_cycle_batch(1, max_instructions=1)
    assert before_write_ack.instructions_executed == 0
    assert bytes(cpu.mem[destination:destination + 64]) == sentinel

    write_ack = system.run_cycle_batch(1, max_instructions=1)
    assert write_ack.instructions_executed == 1
    assert write_ack.stop_cycle == 4
    assert write_ack.per_core_cycles == (4,)
    assert bytes(cpu.mem[destination:destination + 64]) == expected
    assert (
        cpu.cycle_count - before[0],
        cpu.perf_cycles - before[1],
        cpu.perf_stalls - before[2],
        cpu.perf_tileops - before[3],
    ) == (4, 4, 3, 1)
    completed = system._native_system._tacc_transport_snapshot()
    assert completed["port"]["grant_count"] == 2
    assert tuple(completed["port"]["grant_counts"]) == (2,)
    assert all(
        request is None
        for request in completed["port"]["pending"]
    )

    # Any replay or delayed completion after retirement would overwrite this
    # host marker.  Retiring the following HALT must leave it untouched.
    late_write_marker = bytes([0x5D]) * 64
    cpu.mem[destination:destination + 64] = late_write_marker
    halted = system.run_cycle_batch(1, max_instructions=1)
    assert halted.instructions_executed == 1
    assert bytes(
        cpu.mem[destination:destination + 64]
    ) == late_write_marker
    after_halt = system._native_system._tacc_transport_snapshot()
    assert after_halt["port"]["grant_count"] == 2
    assert tuple(after_halt["port"]["grant_counts"]) == (2,)
    assert (
        after_halt["port"]["last_issue_sequences"]
        == completed["port"]["last_issue_sequences"]
    )
    assert after_halt["port"]["active_grant"] is None
    assert all(
        request is None
        for request in after_halt["port"]["pending"]
    )


@pytest.mark.parametrize(
    "direction",
    (
        pytest.param("load", id="load"),
        pytest.param("store", id="store"),
    ),
)
def test_tacc_transfer_holds_stage_for_four_registered_beats(
    direction: str,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    source = 0x400
    destination = 0x800
    incoming = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0xA5]) * TACC_IMAGE_BYTES
    sentinel = bytes([0xCC]) * TACC_IMAGE_BYTES

    if direction == "load":
        cpu.tacc = old_image
        cpu.tacc_dirty = True
        cpu.tsrc0 = source
        cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
        instruction = "t.acc.load"
    else:
        cpu.tacc = incoming
        cpu.tacc_dirty = True
        cpu.tdst = destination
        cpu.mem[destination:destination + TACC_IMAGE_BYTES] = sentinel
        instruction = "t.acc.store"

    code = assemble(instruction)
    _install(system, 0, code)
    cpu.pc = 0
    before = _counter_state(cpu)
    persistent_before = _persistent_tacc_state(cpu)
    observed_beats = []

    # Request capture is cycle 0, image-stage grant is cycle 1, and the four
    # port ACKs are cycles 3, 5, 7, and 9.  Stop one cycle before the terminal
    # ACK and prove that no architectural LOAD result or clean STORE state is
    # visible early.
    for expected_cycle in range(1, 9):
        sliced = system.run_cycle_batch(1, max_instructions=1)
        assert sliced.instructions_executed == 0
        assert sliced.system_cycles_advanced == 1
        assert sliced.stop_cycle == expected_cycle
        assert sliced.per_core_cycles == (0,)

        snapshot = dict(
            system._native_system._tacc_transport_snapshot()
        )
        engine = _engine(snapshot)
        acknowledged_beats = max(0, (expected_cycle - 1) // 2)
        observed_beats.append(int(engine["beat_index"]))

        assert engine["beat_index"] == acknowledged_beats
        assert snapshot["port"]["grant_count"] == acknowledged_beats
        assert tuple(snapshot["port"]["grant_counts"]) == (
            acknowledged_beats,
        )
        if expected_cycle == 1:
            assert engine["phase"] == "waiting_stage"
            assert not snapshot["stage"]["active"]
            assert snapshot["stage"]["grant_count"] == 0
        else:
            assert engine["phase"] == "active"
            assert snapshot["stage"]["active"]
            assert snapshot["stage"]["owner_engine_id"] == 0
            assert snapshot["stage"]["beat_index"] == acknowledged_beats
            assert snapshot["stage"]["grant_count"] == 1
            assert tuple(snapshot["stage"]["grant_counts"]) == (1,)

        assert cpu.pc == 0
        assert _counter_state(cpu) == before
        assert cpu.tacc_busy
        assert not cpu.tacc_force_pending
        assert cpu.tacc_dirty
        assert _persistent_tacc_state(cpu) == persistent_before
        if direction == "store":
            acknowledged_bytes = acknowledged_beats * 64
            assert bytes(
                cpu.mem[destination:destination + acknowledged_bytes]
            ) == incoming[:acknowledged_bytes]
            assert bytes(
                cpu.mem[
                    destination + acknowledged_bytes:
                    destination + TACC_IMAGE_BYTES
                ]
            ) == sentinel[acknowledged_bytes:]

    assert observed_beats == [0, 0, 1, 1, 2, 2, 3, 3]

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.system_cycles_advanced == 1
    assert terminal.stop_cycle == 9
    assert terminal.per_core_cycles == (9,)
    assert cpu.pc == len(code)
    assert (
        cpu.cycle_count - before[0],
        cpu.perf_cycles - before[1],
        cpu.perf_stalls - before[2],
        cpu.perf_tileops - before[3],
    ) == (9, 9, 3, 1)
    assert cpu.tacc_valid
    assert not cpu.tacc_busy
    assert not cpu.tacc_force_pending
    assert not cpu.tacc_dirty
    if direction == "load":
        assert bytes(cpu.tacc) == incoming
    else:
        assert bytes(
            cpu.mem[destination:destination + TACC_IMAGE_BYTES]
        ) == incoming

    completed = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert not completed["stage"]["active"]
    assert completed["stage"]["grant_count"] == 1
    assert completed["port"]["grant_count"] == 4
    assert tuple(completed["port"]["grant_counts"]) == (4,)


def test_load_keeps_image_and_format_atomic_through_third_ack():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    source = 0x400
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    active_image = bytes(range(128))
    incoming = active_image + bytes([0xEE]) * 128
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tmode = EW_FP16
    cpu.tsrc0 = source
    cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
    code = assemble("t.acc.load")
    _install(system, 0, code)
    cpu.pc = 0
    persistent_before = _persistent_tacc_state(cpu)
    counters_before = _counter_state(cpu)

    through_ack_three = system.run_cycle_batch(
        7,
        max_instructions=1,
    )

    assert through_ack_three.instructions_executed == 0
    assert through_ack_three.system_cycles_advanced == 7
    assert through_ack_three.per_core_cycles == (0,)
    assert cpu.pc == 0
    assert cpu.tacc_busy
    assert _persistent_tacc_state(cpu) == persistent_before
    assert _counter_state(cpu) == counters_before

    staged = system._native_system._tacc_transport_snapshot()
    staged_engine = _engine(staged)
    assert staged["stage"]["active"]
    assert staged["stage"]["owner_engine_id"] == 0
    assert staged["stage"]["format_ew"] == EW_FP16
    assert not staged["stage"]["format_signed"]
    assert staged["stage"]["beat_index"] == 3
    assert staged_engine["beat_index"] == 3
    assert staged_engine["format_ew"] == EW_FP16
    assert not staged_engine["format_signed"]
    assert bytes(staged_engine["image"][:128]) == active_image
    assert bytes(staged_engine["image"][128:]) == bytes(128)

    terminal = system.run_cycle_batch(2, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.system_cycles_advanced == 2
    assert terminal.stop_cycle == 9
    assert terminal.per_core_cycles == (9,)
    assert bytes(cpu.tacc[:128]) == active_image
    assert bytes(cpu.tacc[128:]) == bytes(128)
    assert cpu.tacc_owner == persistent_before[1]
    assert cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert cpu.tacc_format_ew == EW_FP16
    assert cpu.tacc_format_signed == 0
    assert not cpu.tacc_busy
    assert cpu.tacc_epoch == persistent_before[6]
    assert (
        cpu.cycle_count - counters_before[0],
        cpu.perf_cycles - counters_before[1],
        cpu.perf_stalls - counters_before[2],
        cpu.perf_tileops - counters_before[3],
    ) == (9, 9, 3, 1)


def test_force_release_waits_for_fourth_load_ack_then_wipes():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    source = 0x400
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    incoming = bytes([0xC3]) * TACC_IMAGE_BYTES
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tsrc0 = source
    cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
    code = assemble("t.acc.load")
    _install(system, 0, code)
    cpu.pc = 0
    initial_epoch = int(cpu.tacc_epoch)
    persistent_before = _persistent_tacc_state(cpu)
    counters_before = _counter_state(cpu)

    first_ack = system.run_cycle_batch(3, max_instructions=1)

    assert first_ack.instructions_executed == 0
    assert first_ack.stop_cycle == 3
    assert _engine(
        system._native_system._tacc_transport_snapshot()
    )["beat_index"] == 1

    cpu.csr_write(CSR_TACC_CTL, 1)

    assert cpu.tacc_busy
    assert cpu.tacc_force_pending
    assert cpu.tacc_owner == persistent_before[1]
    assert _persistent_tacc_state(cpu) == persistent_before

    through_ack_three = system.run_cycle_batch(4, max_instructions=1)

    assert through_ack_three.instructions_executed == 0
    assert through_ack_three.stop_cycle == 7
    assert through_ack_three.per_core_cycles == (0,)
    assert cpu.pc == 0
    assert cpu.tacc_busy
    assert cpu.tacc_force_pending
    assert _persistent_tacc_state(cpu) == persistent_before
    assert _counter_state(cpu) == counters_before
    staged = system._native_system._tacc_transport_snapshot()
    assert staged["stage"]["active"]
    assert staged["stage"]["beat_index"] == 3
    assert _engine(staged)["beat_index"] == 3

    before_ack_four = system.run_cycle_batch(1, max_instructions=1)

    assert before_ack_four.instructions_executed == 0
    assert before_ack_four.stop_cycle == 8
    assert cpu.tacc_busy
    assert cpu.tacc_force_pending
    assert _persistent_tacc_state(cpu) == persistent_before

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.stop_cycle == 9
    assert terminal.per_core_cycles == (9,)
    assert cpu.pc == len(code)
    assert not any(cpu.tacc)
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert not cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    assert not cpu.tacc_force_pending
    assert cpu.tacc_epoch == initial_epoch + 1
    assert (
        cpu.cycle_count - counters_before[0],
        cpu.perf_cycles - counters_before[1],
        cpu.perf_stalls - counters_before[2],
        cpu.perf_tileops - counters_before[3],
    ) == (9, 9, 3, 1)
    completed = system._native_system._tacc_transport_snapshot()
    assert not completed["stage"]["active"]
    assert completed["port"]["grant_count"] == 4


@pytest.mark.parametrize(
    "direction",
    (
        pytest.param("load", id="load"),
        pytest.param("store", id="store"),
    ),
)
def test_tacc_transfer_is_invariant_to_bounded_call_partition(
    direction: str,
):
    image = bytes(range(TACC_IMAGE_BYTES))
    source = 0x400
    destination = 0x800

    def configured():
        system = _system()
        cpu = system.cpu
        _claim_and_clear(system)
        cpu.tacc_dirty = True
        if direction == "load":
            cpu.tacc = bytes([0xA5]) * TACC_IMAGE_BYTES
            cpu.tsrc0 = source
            cpu.mem[source:source + TACC_IMAGE_BYTES] = image
            code = assemble("t.acc.load")
        else:
            cpu.tacc = image
            cpu.tdst = destination
            cpu.mem[
                destination:destination + TACC_IMAGE_BYTES
            ] = bytes([0xCC]) * TACC_IMAGE_BYTES
            code = assemble("t.acc.store")
        _install(system, 0, code)
        cpu.pc = 0
        return system, code

    whole, code = configured()
    sliced, _ = configured()
    whole_before = _counter_state(whole.cpu)
    sliced_before = _counter_state(sliced.cpu)

    uninterrupted = whole.run_cycle_batch(9, max_instructions=1)
    partitions = (1, 2, 1, 3, 2)
    partitioned = [
        sliced.run_cycle_batch(cycles, max_instructions=1)
        for cycles in partitions
    ]

    assert uninterrupted.instructions_executed == 1
    assert uninterrupted.system_cycles_advanced == 9
    assert uninterrupted.stop_cycle == 9
    assert uninterrupted.per_core_cycles == (9,)
    assert sum(
        result.instructions_executed
        for result in partitioned
    ) == 1
    assert sum(
        result.system_cycles_advanced
        for result in partitioned
    ) == 9
    assert sum(
        result.per_core_cycles[0]
        for result in partitioned
    ) == 9

    def final_signature(system: MegapadSystem) -> tuple[object, ...]:
        cpu = system.cpu
        return (
            int(cpu.pc),
            _persistent_tacc_state(cpu),
            bool(cpu.tacc_busy),
            bool(cpu.tacc_force_pending),
            _counter_state(cpu),
            bytes(cpu.mem),
            int(system._native_system.system_cycles),
            bool(system._native_system.cycle_execution_pending),
            system._native_system._tacc_transport_snapshot(),
        )

    assert whole.cpu.pc == len(code)
    assert sliced.cpu.pc == len(code)
    assert final_signature(sliced) == final_signature(whole)
    assert (
        whole.cpu.cycle_count - whole_before[0],
        whole.cpu.perf_cycles - whole_before[1],
        whole.cpu.perf_stalls - whole_before[2],
        whole.cpu.perf_tileops - whole_before[3],
    ) == (9, 9, 3, 1)
    assert (
        sliced.cpu.cycle_count - sliced_before[0],
        sliced.cpu.perf_cycles - sliced_before[1],
        sliced.cpu.perf_stalls - sliced_before[2],
        sliced.cpu.perf_tileops - sliced_before[3],
    ) == (9, 9, 3, 1)


def test_ordinary_tile_traffic_interleaves_without_releasing_tacc_stage():
    system = _system(cores=2)
    tacc_core, ordinary_core = system.cores
    _claim_and_clear(system, 0)
    tacc_image = bytes(range(TACC_IMAGE_BYTES))
    tacc_destination = 0x800
    ordinary_source = 0x500

    tacc_core.tacc = tacc_image
    tacc_core.tacc_dirty = True
    tacc_core.tdst = tacc_destination
    tacc_core.mem[
        tacc_destination:tacc_destination + TACC_IMAGE_BYTES
    ] = bytes([0xCC]) * TACC_IMAGE_BYTES

    ordinary_core.tmode = EW_FP16
    ordinary_core.tsrc0 = ordinary_source
    ordinary_core.mem[
        ordinary_source:ordinary_source + 64
    ] = _FP16_ONE_TILE
    ordinary_core.acc[0] = 0xDEAD_BEEF
    ordinary_core.regs[7] = 0

    tacc_code = assemble("t.acc.store")
    ordinary_code = (
        assemble("nop\nnop")
        + _BROADCAST_FP16_RMIN_R7
    )
    ordinary_pc = 0x100
    _install(system, 0, tacc_code)
    _install(system, ordinary_pc, ordinary_code)
    tacc_core.pc = 0
    ordinary_core.pc = ordinary_pc
    before_tacc = _counter_state(tacc_core)
    before_ordinary = _counter_state(ordinary_core)

    grant_order = []
    previous_grant_count = 0
    tacc_retired = False
    ordinary_retired = False
    for _ in range(12):
        system.run_cycle_batch(1, max_instructions=2)
        snapshot = dict(
            system._native_system._tacc_transport_snapshot()
        )
        grant_count = int(snapshot["port"]["grant_count"])
        if grant_count != previous_grant_count:
            assert grant_count == previous_grant_count + 1
            grant_order.append(
                int(snapshot["port"]["last_grant_engine_id"])
            )
            previous_grant_count = grant_count
        if snapshot["stage"]["active"]:
            assert snapshot["stage"]["owner_engine_id"] == 0
        if not tacc_retired and tacc_core.pc == len(tacc_code):
            tacc_retired = True
            tacc_core.halted = True
        if (
            not ordinary_retired
            and ordinary_core.pc
            == ordinary_pc + len(ordinary_code)
        ):
            ordinary_retired = True
            ordinary_core.halted = True
        if tacc_retired and ordinary_retired:
            break
    else:
        pytest.fail("timed tile instructions did not retire")

    # Core 1's request is captured after beat zero and uses the otherwise idle
    # registered gap before beat one.  The image stage never changes owner.
    assert grant_order == [0, 1, 0, 0, 0]
    snapshot = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert tuple(snapshot["port"]["grant_counts"]) == (4, 1)
    assert tuple(snapshot["stage"]["grant_counts"]) == (1, 0)
    assert not snapshot["stage"]["active"]
    assert bytes(
        tacc_core.mem[
            tacc_destination:tacc_destination + TACC_IMAGE_BYTES
        ]
    ) == tacc_image
    assert ordinary_core.acc[0] == _FP32_ONE
    assert (
        tacc_core.cycle_count - before_tacc[0],
        tacc_core.perf_cycles - before_tacc[1],
        tacc_core.perf_stalls - before_tacc[2],
        tacc_core.perf_tileops - before_tacc[3],
    ) == (9, 9, 3, 1)
    assert (
        ordinary_core.cycle_count - before_ordinary[0],
        ordinary_core.perf_cycles - before_ordinary[1],
        ordinary_core.perf_stalls - before_ordinary[2],
        ordinary_core.perf_tileops - before_ordinary[3],
    ) == (4, 4, 1, 1)


def test_same_frontier_full_core_image_transfers_use_stage_equal_rr():
    system = _system(cores=2)
    first_core, second_core = system.cores
    _claim_and_clear(system, 0)
    _claim_and_clear(system, 1)
    first_image = bytes([0x31]) * TACC_IMAGE_BYTES
    second_image = bytes([0x72]) * TACC_IMAGE_BYTES
    first_destination = 0x600
    second_destination = 0x800
    first_core.tacc = first_image
    first_core.tacc_dirty = True
    first_core.tdst = first_destination
    second_core.tacc = second_image
    second_core.tacc_dirty = True
    second_core.tdst = second_destination
    first_core.mem[
        first_destination:first_destination + TACC_IMAGE_BYTES
    ] = bytes([0xCC]) * TACC_IMAGE_BYTES
    second_core.mem[
        second_destination:second_destination + TACC_IMAGE_BYTES
    ] = bytes([0xDD]) * TACC_IMAGE_BYTES

    code = assemble("t.acc.store")
    second_pc = 0x100
    _install(system, 0, code)
    _install(system, second_pc, code)
    first_core.pc = 0
    second_core.pc = second_pc
    first_before = _counter_state(first_core)
    second_before = _counter_state(second_core)

    # Reverse host dispatch order.  The stage must still choose engine 0 from
    # the complete same-frontier candidate set, then engine 1 after tenure.
    system._native_system.scheduler_cursor = 1
    captured = system.run_cycle_batch(1, max_instructions=2)
    assert captured.instructions_executed == 0
    initial = system._native_system._tacc_transport_snapshot()
    assert _engine(initial, 0)["phase"] == "waiting_stage"
    assert _engine(initial, 1)["phase"] == "waiting_stage"
    assert _engine(initial, 0)["ready_cycle"] == 0
    assert _engine(initial, 1)["ready_cycle"] == 0
    assert not initial["stage"]["active"]

    stage_order = []
    stage_grant_cycles = []
    port_order = []
    previous_stage_grants = 0
    previous_port_grants = 0
    first_retired = False
    second_retired = False
    for _ in range(18):
        system.run_cycle_batch(1, max_instructions=2)
        snapshot = system._native_system._tacc_transport_snapshot()
        stage_grants = int(snapshot["stage"]["grant_count"])
        port_grants = int(snapshot["port"]["grant_count"])
        if stage_grants != previous_stage_grants:
            assert stage_grants == previous_stage_grants + 1
            stage_order.append(
                int(snapshot["stage"]["last_grant_engine_id"])
            )
            stage_grant_cycles.append(
                int(snapshot["stage"]["grant_cycle"])
            )
            previous_stage_grants = stage_grants
        if port_grants != previous_port_grants:
            assert port_grants == previous_port_grants + 1
            port_order.append(
                int(snapshot["port"]["last_grant_engine_id"])
            )
            previous_port_grants = port_grants
        if snapshot["stage"]["active"]:
            expected_owner = 0 if stage_grants == 1 else 1
            assert snapshot["stage"]["owner_engine_id"] == expected_owner
        if not first_retired and first_core.pc == len(code):
            first_retired = True
            first_core.halted = True
        if (
            not second_retired
            and second_core.pc == second_pc + len(code)
        ):
            second_retired = True
            second_core.halted = True
        if first_retired and second_retired:
            break
    else:
        pytest.fail("same-frontier image transfers did not retire")

    # Beat four releases engine 0's tenure at cycle 9.  Registered stage
    # admission gives the already-waiting engine 1 its grant at cycle 10,
    # preserving a mandatory release-to-regrant bubble.
    assert int(system._native_system.system_cycles) == 18
    assert stage_order == [0, 1]
    assert stage_grant_cycles == [1, 10]
    assert port_order == [0, 0, 0, 0, 1, 1, 1, 1]
    final = system._native_system._tacc_transport_snapshot()
    assert tuple(final["stage"]["grant_counts"]) == (1, 1)
    assert tuple(final["port"]["grant_counts"]) == (4, 4)
    assert not final["stage"]["active"]
    assert bytes(
        first_core.mem[
            first_destination:first_destination + TACC_IMAGE_BYTES
        ]
    ) == first_image
    assert bytes(
        second_core.mem[
            second_destination:second_destination + TACC_IMAGE_BYTES
        ]
    ) == second_image
    assert not first_core.tacc_dirty
    assert not second_core.tacc_dirty
    assert (
        first_core.cycle_count - first_before[0],
        first_core.perf_cycles - first_before[1],
        first_core.perf_stalls - first_before[2],
        first_core.perf_tileops - first_before[3],
    ) == (9, 9, 3, 1)
    assert (
        second_core.cycle_count - second_before[0],
        second_core.perf_cycles - second_before[1],
        second_core.perf_stalls - second_before[2],
        second_core.perf_tileops - second_before[3],
    ) == (18, 18, 12, 1)


def test_tacc_store_preflights_complete_span_before_beat_zero():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    cpu.tacc = bytes([0xD2]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.tdst = system.ram_size - 128
    code = assemble("t.acc.store")
    _install(system, 0, code)
    cpu.pc = 0
    before_memory = bytes(cpu.mem)
    before_image = bytes(cpu.tacc)
    before = _counter_state(cpu)

    faulted = system.run_cycle_batch(2, max_instructions=1)

    assert faulted.instructions_executed == 0
    assert faulted.system_cycles_advanced == 2
    assert faulted.per_core_cycles == (2,)
    assert cpu.pc == len(code)
    assert cpu.trap_addr == system.ram_size
    assert bytes(cpu.mem) == before_memory
    assert bytes(cpu.tacc) == before_image
    assert cpu.tacc_valid
    assert cpu.tacc_dirty
    assert (
        cpu.cycle_count - before[0],
        cpu.perf_cycles - before[1],
        cpu.perf_stalls - before[2],
        cpu.perf_tileops - before[3],
    ) == (2, 2, 0, 0)

    transport = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert not transport["stage"]["active"]
    assert transport["stage"]["grant_count"] == 0
    assert transport["port"]["grant_count"] == 0
    assert all(
        request is None
        for request in transport["port"]["pending"]
    )
    assert _engine(transport)["phase"] == "idle"


def test_strict_cycle_rejects_noncheckpointable_custom_span_policy():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    cpu.tsrc0 = 0x400
    cpu._tacc_span_validator = lambda _start, _size, _write: None
    code = assemble("t.acc.load")
    _install(system, 0, code)
    cpu.pc = 0
    before = _persistent_tacc_state(cpu)

    with pytest.raises(
        ValueError,
        match="strict-cycle TACC image transfers.*custom span validator",
    ):
        system.run_cycle_batch(9, max_instructions=1)

    assert cpu.pc == 0
    assert not cpu.tacc_busy
    assert _persistent_tacc_state(cpu) == before
    transport = system._native_system._tacc_transport_snapshot()
    assert not transport["stage"]["active"]
    assert transport["stage"]["grant_count"] == 0
    assert transport["port"]["grant_count"] == 0


def test_reset_preserves_acknowledged_store_prefix_and_cancels_suffix():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    destination = 0x800
    image = bytes(range(TACC_IMAGE_BYTES))
    sentinel = bytes([0xCC]) * TACC_IMAGE_BYTES
    cpu.tacc = image
    cpu.tacc_dirty = True
    cpu.tdst = destination
    cpu.mem[destination:destination + TACC_IMAGE_BYTES] = sentinel
    code = assemble("t.acc.store")
    halt_pc = 0x100
    _install(system, 0, code)
    _install(system, halt_pc, assemble("halt"))
    cpu.pc = 0
    initial_epoch = int(cpu.tacc_epoch)

    through_ack_two = system.run_cycle_batch(5, max_instructions=1)

    assert through_ack_two.instructions_executed == 0
    assert through_ack_two.stop_cycle == 5
    assert bytes(cpu.mem[destination:destination + 128]) == image[:128]
    assert bytes(
        cpu.mem[destination + 128:destination + TACC_IMAGE_BYTES]
    ) == sentinel[128:]
    staged = system._native_system._tacc_transport_snapshot()
    assert staged["stage"]["active"]
    assert staged["stage"]["beat_index"] == 2
    third = staged["port"]["pending"][0]
    assert third is not None
    assert third["direction"] == "write"
    assert third["beat_index"] == 2
    assert third["ready_cycle"] == 5

    # Preserve a completion identity from the abandoned timeline so the
    # post-reset rejection also proves it cannot issue a late suffix write.
    old_grant = system._native_system._tile_memory_port_try_grant(6)
    assert old_grant is not None
    assert old_grant["request"]["direction"] == "write"
    assert old_grant["request"]["beat_index"] == 2

    system.boot(entry=halt_pc)

    expected_memory = image[:128] + sentinel[128:]
    assert bytes(
        cpu.mem[destination:destination + TACC_IMAGE_BYTES]
    ) == expected_memory
    assert not any(cpu.tacc)
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert not cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    assert cpu.tacc_epoch > initial_epoch
    reset = system._native_system._tacc_transport_snapshot()
    assert not reset["stage"]["active"]
    assert reset["port"]["active_grant"] is None
    assert all(request is None for request in reset["port"]["pending"])

    stale = system._native_system._tile_memory_port_complete(
        old_grant["grant_sequence"],
        old_grant["grant_cycle"] + 1,
    )

    assert stale is None
    assert bytes(
        cpu.mem[destination:destination + TACC_IMAGE_BYTES]
    ) == expected_memory
    assert not system._native_system.cycle_execution_pending


def test_reset_cancels_staged_load_and_rejects_its_late_ack():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    source = 0x400
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    incoming = bytes([0xC3]) * TACC_IMAGE_BYTES
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tsrc0 = source
    cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
    code = assemble("t.acc.load")
    halt_pc = 0x100
    _install(system, 0, code)
    _install(system, halt_pc, assemble("halt"))
    cpu.pc = 0
    initial_epoch = int(cpu.tacc_epoch)

    suspended = system.run_cycle_batch(7, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert bytes(cpu.tacc) == old_image
    before_reset = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert before_reset["stage"]["active"]
    assert before_reset["stage"]["beat_index"] == 3
    fourth = before_reset["port"]["pending"][0]
    assert fourth is not None
    assert fourth["image_transfer"]
    assert fourth["beat_index"] == 3
    assert fourth["ready_cycle"] == 7

    # Materialize the registered fourth-beat grant so an ACK can arrive after
    # reset.  The diagnostic grant hook is the smallest available way to
    # inject a stale internal-memory completion outside the running batch.
    old_grant = system._native_system._tile_memory_port_try_grant(8)
    assert old_grant is not None
    assert old_grant["request"]["beat_index"] == 3
    old_epoch = old_grant["request"]["engine_epoch"]

    system.boot(entry=halt_pc)

    reset = dict(
        system._native_system._tacc_transport_snapshot()
    )
    assert not reset["stage"]["active"]
    assert reset["port"]["active_grant"] is None
    assert all(request is None for request in reset["port"]["pending"])
    assert not any(cpu.tacc)
    assert not cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert int(cpu.tacc_epoch) > initial_epoch
    assert int(cpu.tacc_epoch) != old_epoch

    stale = system._native_system._tile_memory_port_complete(
        old_grant["grant_sequence"],
        old_grant["grant_cycle"] + 1,
        bytes([0xEE]) * 64,
    )

    assert stale is None
    assert not any(cpu.tacc)
    assert not cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert not system._native_system.cycle_execution_pending


def test_diagnostic_transport_mutation_rejects_suspended_instruction():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    source = 0x400
    cpu.tsrc0 = source
    cpu.mem[source:source + TACC_IMAGE_BYTES] = bytes(
        range(TACC_IMAGE_BYTES)
    )
    code = assemble("t.acc.load")
    _install(system, 0, code)
    cpu.pc = 0

    captured = system.run_cycle_batch(1, max_instructions=1)
    assert captured.instructions_executed == 0
    before = dict(system._native_system._tacc_transport_snapshot())
    assert _engine(before)["phase"] == "waiting_stage"

    with pytest.raises(
        RuntimeError,
        match="core instruction is suspended",
    ):
        system._native_system._tacc_transport_reset()
    with pytest.raises(
        RuntimeError,
        match="core instruction is suspended",
    ):
        system._native_system._tacc_transport_restore(before)

    assert (
        dict(system._native_system._tacc_transport_snapshot())
        == before
    )
