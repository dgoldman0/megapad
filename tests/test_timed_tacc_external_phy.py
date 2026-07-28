"""Strict-cycle contracts for external TACC image PHY transfers."""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    EXTERNAL_PHY_TIMEOUT_CYCLES,
    EW_U8,
    IVEC_BUS_FAULT,
    TACC_IMAGE_BYTES,
    ExternalPhyWordRequest,
    ExternalPhyWordResponse,
    TrapError,
)
from system import EXT_MEM_BASE, MegapadSystem


EXT_SIZE = 512


def _system(*, fill: int = 0) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=EXT_SIZE,
        vram_size=0,
        worker_count=1,
    )
    system.boot(entry=0)
    system._ext_mem[:] = bytes([fill]) * EXT_SIZE
    return system


def _prime_instruction_cache(
    system: MegapadSystem,
    address: int,
    size: int,
) -> None:
    first_line = address & ~0xF
    last_line = (address + size - 1) & ~0xF
    cpu = system.cpu
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


def _install(system: MegapadSystem, instruction: str) -> bytes:
    code = bytes(assemble(instruction))
    system.load_binary(0, code)
    _prime_instruction_cache(system, 0, len(code))
    system.cpu.pc = 0
    return code


def _claim_and_clear(system: MegapadSystem) -> None:
    cpu = system.cpu
    cpu.tmode = EW_U8
    for instruction in ("t.acc.try", "t.acc.clear"):
        _install(system, instruction)
        cpu.step()


def _counters(cpu) -> tuple[int, int, int, int, int]:
    return (
        int(cpu.cycle_count),
        int(cpu.perf_cycles),
        int(cpu.perf_stalls),
        int(cpu.perf_tileops),
        int(cpu.perf_extmem),
    )


def _counter_delta(
    cpu,
    before: tuple[int, int, int, int, int],
) -> tuple[int, int, int, int, int]:
    after = _counters(cpu)
    return tuple(
        current - original
        for current, original in zip(after, before)
    )


def _tacc_state(cpu) -> tuple[object, ...]:
    return (
        bytes(cpu.tacc),
        int(cpu.tacc_owner),
        bool(cpu.tacc_valid),
        bool(cpu.tacc_dirty),
        int(cpu.tacc_format_ew),
        int(cpu.tacc_format_signed),
        bool(cpu.tacc_busy),
        bool(cpu.tacc_force_pending),
        int(cpu.tacc_epoch),
    )


def _assert_transport_terminal(
    system: MegapadSystem,
    *,
    port_grants: int,
) -> None:
    transport = system._native_system._tacc_transport_snapshot()
    assert not transport["stage"]["active"]
    assert transport["stage"]["grant_count"] == 1
    assert transport["port"]["active_grant"] is None
    assert transport["port"]["grant_count"] == port_grants
    assert all(
        request is None
        for request in transport["port"]["pending"]
    )
    assert transport["engines"][0]["phase"] == "canceled"


def _expected_requests(
    direction: str,
    image: bytes,
) -> list[ExternalPhyWordRequest]:
    requests = []
    for index in range(32):
        offset = index * 8
        requests.append(
            ExternalPhyWordRequest(
                direction=direction,
                beat_index=index // 8,
                word_index=index % 8,
                address=EXT_MEM_BASE + offset,
                write_data=(
                    int.from_bytes(image[offset:offset + 8], "little")
                    if direction == "store"
                    else None
                ),
            )
        )
    return requests


def test_native_phy_bridge_compacts_responses_and_raises_without_mutation():
    system = _system()
    cpu = system.cpu
    observed: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        observed.append(request)
        return ExternalPhyWordResponse(latency_cycles=2, error=True)

    cpu.set_external_phy_response_plan(response_plan)
    request = ExternalPhyWordRequest(
        direction="store",
        beat_index=2,
        word_index=5,
        address=EXT_MEM_BASE + 2 * 64 + 5 * 8,
        write_data=0x8877_6655_4433_2211,
    )
    before = (
        _tacc_state(cpu),
        _counters(cpu),
        int(cpu.pc),
        int(cpu.trap_addr),
    )

    response = cpu._native_external_phy_response(
        request.direction,
        request.beat_index,
        request.word_index,
        request.address,
        request.write_data,
    )

    assert response == (2, True)
    assert observed == [request]
    with pytest.raises(TrapError) as raised:
        cpu._native_external_phy_fault(
            request.direction,
            request.beat_index,
            request.word_index,
            request.address,
            request.write_data,
            "error",
        )
    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert raised.value.external_phy_fault == "error"
    assert raised.value.external_phy_request == request
    assert (
        _tacc_state(cpu),
        _counters(cpu),
        int(cpu.pc),
        int(cpu.trap_addr),
    ) == before


@pytest.mark.parametrize(
    "direction",
    (
        pytest.param("load", id="load"),
        pytest.param("store", id="store"),
    ),
)
def test_default_external_transfer_retires_on_cycle_37(
    direction: str,
):
    system = _system(fill=0xCC)
    cpu = system.cpu
    _claim_and_clear(system)
    image = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0xA5]) * TACC_IMAGE_BYTES
    if direction == "load":
        system._ext_mem[:TACC_IMAGE_BYTES] = image
        cpu.tacc = old_image
        cpu.tsrc0 = EXT_MEM_BASE
        instruction = "t.acc.load"
    else:
        cpu.tacc = image
        cpu.tdst = EXT_MEM_BASE
        instruction = "t.acc.store"
    cpu.tacc_dirty = True
    code = _install(system, instruction)
    before = _counters(cpu)

    before_final_word = system.run_cycle_batch(
        36,
        max_instructions=1,
    )

    assert before_final_word.instructions_executed == 0
    assert before_final_word.system_cycles_advanced == 36
    assert before_final_word.stop_cycle == 36
    assert before_final_word.per_core_cycles == (0,)
    assert cpu.pc == 0
    assert cpu.tacc_dirty
    if direction == "load":
        assert bytes(cpu.tacc) == old_image
    else:
        assert bytes(system._ext_mem[:248]) == image[:248]
        assert bytes(system._ext_mem[248:TACC_IMAGE_BYTES]) == (
            bytes([0xCC]) * 8
        )

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.system_cycles_advanced == 1
    assert terminal.stop_cycle == 37
    assert terminal.per_core_cycles == (37,)
    assert cpu.pc == len(code)
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    if direction == "load":
        assert bytes(cpu.tacc) == image
    else:
        assert bytes(
            system._ext_mem[:TACC_IMAGE_BYTES]
        ) == image
    assert _counter_delta(cpu, before) == (37, 37, 31, 1, 32)


@pytest.mark.parametrize(
    "direction",
    (
        pytest.param("load", id="load"),
        pytest.param("store", id="store"),
    ),
)
def test_two_cycle_phy_responses_retime_transfer_and_preserve_word_order(
    direction: str,
):
    system = _system(fill=0xCC)
    cpu = system.cpu
    _claim_and_clear(system)
    image = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        return ExternalPhyWordResponse(latency_cycles=2)

    cpu.set_external_phy_response_plan(response_plan)
    if direction == "load":
        system._ext_mem[:TACC_IMAGE_BYTES] = image
        cpu.tacc = old_image
        cpu.tsrc0 = EXT_MEM_BASE
        instruction = "t.acc.load"
    else:
        cpu.tacc = image
        cpu.tdst = EXT_MEM_BASE
        instruction = "t.acc.store"
    cpu.tacc_dirty = True
    code = _install(system, instruction)
    before = _counters(cpu)

    before_terminal_response = system.run_cycle_batch(
        68,
        max_instructions=1,
    )

    assert before_terminal_response.instructions_executed == 0
    assert before_terminal_response.stop_cycle == 68
    assert requests == _expected_requests(direction, image)
    assert cpu.pc == 0
    assert cpu.tacc_dirty
    if direction == "load":
        assert bytes(cpu.tacc) == old_image
    else:
        assert bytes(system._ext_mem[:248]) == image[:248]
        assert bytes(system._ext_mem[248:TACC_IMAGE_BYTES]) == (
            bytes([0xCC]) * 8
        )

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.stop_cycle == 69
    assert terminal.per_core_cycles == (69,)
    assert cpu.pc == len(code)
    assert requests == _expected_requests(direction, image)
    assert _counter_delta(cpu, before) == (69, 69, 63, 1, 32)
    if direction == "load":
        assert bytes(cpu.tacc) == image
    else:
        assert bytes(
            system._ext_mem[:TACC_IMAGE_BYTES]
        ) == image


def test_load_error_reports_exact_word_and_keeps_staging_private():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    incoming = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0xA5]) * TACC_IMAGE_BYTES
    system._ext_mem[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tsrc0 = EXT_MEM_BASE
    before_state = _tacc_state(cpu)
    before = _counters(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        return ExternalPhyWordResponse(
            error=(
                request.beat_index == 1
                and request.word_index == 3
            )
        )

    cpu.set_external_phy_response_plan(response_plan)
    code = _install(system, "t.acc.load")

    faulted = system.run_cycle_batch(15, max_instructions=1)

    request = ExternalPhyWordRequest(
        direction="load",
        beat_index=1,
        word_index=3,
        address=EXT_MEM_BASE + 64 + 3 * 8,
        write_data=None,
    )
    assert faulted.instructions_executed == 0
    assert faulted.per_core_instructions == (0,)
    assert faulted.per_core_cycles == (15,)
    assert faulted.system_cycles_advanced == 15
    assert faulted.stop_cycle == 15
    assert requests == _expected_requests("load", incoming)[:12]
    assert requests[-1] == request
    assert cpu.pc == len(code)
    assert cpu.trap_addr == request.address
    assert _tacc_state(cpu) == before_state
    assert _counter_delta(cpu, before) == (15, 15, 11, 0, 11)
    assert int(system._native_system.system_cycles) == 15
    _assert_transport_terminal(system, port_grants=2)


@pytest.mark.parametrize(
    "terminal_response",
    (
        pytest.param(None, id="no-response"),
        pytest.param(
            ExternalPhyWordResponse(
                latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES + 1,
            ),
            id="late-response",
        ),
    ),
)
def test_external_timeout_reports_exact_word_at_255_cycles(
    terminal_response,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    incoming = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    system._ext_mem[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tsrc0 = EXT_MEM_BASE
    before_state = _tacc_state(cpu)
    before = _counters(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(request: ExternalPhyWordRequest):
        requests.append(request)
        if request.beat_index == 0 and request.word_index == 2:
            return terminal_response
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    code = _install(system, "t.acc.load")

    faulted = system.run_cycle_batch(259, max_instructions=1)

    request = ExternalPhyWordRequest(
        direction="load",
        beat_index=0,
        word_index=2,
        address=EXT_MEM_BASE + 2 * 8,
        write_data=None,
    )
    assert faulted.instructions_executed == 0
    assert faulted.per_core_instructions == (0,)
    assert faulted.per_core_cycles == (259,)
    assert faulted.system_cycles_advanced == 259
    assert faulted.stop_cycle == 259
    assert requests == _expected_requests("load", incoming)[:3]
    assert requests[-1] == request
    assert cpu.pc == len(code)
    assert cpu.trap_addr == request.address
    assert _tacc_state(cpu) == before_state
    assert _counter_delta(cpu, before) == (259, 259, 256, 0, 2)
    assert int(system._native_system.system_cycles) == 259
    _assert_transport_terminal(system, port_grants=1)


def test_response_on_255_cycle_deadline_completes_full_transfer():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    incoming = bytes(range(TACC_IMAGE_BYTES))
    old_image = bytes([0x5A]) * TACC_IMAGE_BYTES
    system._ext_mem[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = old_image
    cpu.tacc_dirty = True
    cpu.tsrc0 = EXT_MEM_BASE
    before = _counters(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        if request.beat_index == 0 and request.word_index == 0:
            return ExternalPhyWordResponse(
                latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES,
            )
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    code = _install(system, "t.acc.load")

    before_terminal_response = system.run_cycle_batch(
        290,
        max_instructions=1,
    )

    assert before_terminal_response.instructions_executed == 0
    assert before_terminal_response.system_cycles_advanced == 290
    assert before_terminal_response.stop_cycle == 290
    assert before_terminal_response.per_core_cycles == (0,)
    assert cpu.pc == 0
    assert bytes(cpu.tacc) == old_image
    assert cpu.tacc_dirty

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.system_cycles_advanced == 1
    assert terminal.stop_cycle == 291
    assert terminal.per_core_cycles == (291,)
    assert cpu.pc == len(code)
    assert requests == _expected_requests("load", incoming)
    assert bytes(cpu.tacc) == incoming
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    assert _counter_delta(cpu, before) == (291, 291, 285, 1, 32)
    assert int(system._native_system.system_cycles) == 291


@pytest.mark.parametrize("dirty", (False, True), ids=("clean", "dirty"))
def test_store_error_preserves_acknowledged_prefix_and_dirty(dirty: bool):
    system = _system(fill=0xCC)
    cpu = system.cpu
    _claim_and_clear(system)
    image = bytes(range(TACC_IMAGE_BYTES))
    cpu.tacc = image
    cpu.tacc_dirty = dirty
    cpu.tdst = EXT_MEM_BASE
    before_state = _tacc_state(cpu)
    before = _counters(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        return ExternalPhyWordResponse(
            error=(
                request.beat_index == 1
                and request.word_index == 2
            )
        )

    cpu.set_external_phy_response_plan(response_plan)
    code = _install(system, "t.acc.store")

    faulted = system.run_cycle_batch(14, max_instructions=1)

    acknowledged_prefix = (8 + 2) * 8
    request = ExternalPhyWordRequest(
        direction="store",
        beat_index=1,
        word_index=2,
        address=EXT_MEM_BASE + acknowledged_prefix,
        write_data=int.from_bytes(
            image[acknowledged_prefix:acknowledged_prefix + 8],
            "little",
        ),
    )
    assert faulted.instructions_executed == 0
    assert faulted.per_core_instructions == (0,)
    assert faulted.per_core_cycles == (14,)
    assert faulted.system_cycles_advanced == 14
    assert faulted.stop_cycle == 14
    assert requests[-1] == request
    assert len(requests) == 11
    assert cpu.pc == len(code)
    assert cpu.trap_addr == request.address
    assert bytes(
        system._ext_mem[:acknowledged_prefix]
    ) == image[:acknowledged_prefix]
    assert bytes(
        system._ext_mem[acknowledged_prefix:]
    ) == bytes([0xCC]) * (EXT_SIZE - acknowledged_prefix)
    assert _tacc_state(cpu) == before_state
    assert _counter_delta(cpu, before) == (14, 14, 10, 0, 10)
    assert int(system._native_system.system_cycles) == 14
    _assert_transport_terminal(system, port_grants=2)


def test_reset_cancels_long_store_word_and_preserves_only_acked_prefix():
    system = _system(fill=0xCC)
    cpu = system.cpu
    _claim_and_clear(system)
    image = bytes(range(TACC_IMAGE_BYTES))
    sentinel = bytes([0xCC]) * TACC_IMAGE_BYTES
    cpu.tacc = image
    cpu.tacc_dirty = True
    cpu.tdst = EXT_MEM_BASE
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        if request.beat_index == 0 and request.word_index == 2:
            return ExternalPhyWordResponse(latency_cycles=10)
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    _install(system, "t.acc.store")

    suspended = system.run_cycle_batch(5, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert suspended.system_cycles_advanced == 5
    assert suspended.stop_cycle == 5
    assert suspended.per_core_cycles == (0,)
    assert requests == _expected_requests("store", image)[:3]
    assert bytes(system._ext_mem[:16]) == image[:16]
    assert bytes(
        system._ext_mem[16:TACC_IMAGE_BYTES]
    ) == sentinel[16:]
    active = system._native_system._tacc_transport_snapshot()
    assert active["stage"]["active"]
    assert active["stage"]["beat_index"] == 0
    assert active["port"]["active_grant"] is not None

    system.boot(entry=0x100)

    reset = system._native_system._tacc_transport_snapshot()
    assert not reset["stage"]["active"]
    assert reset["port"]["active_grant"] is None
    assert all(
        request is None
        for request in reset["port"]["pending"]
    )
    assert not system._native_system.cycle_execution_pending

    # Cross the abandoned word's original response frontier.  Reset removed
    # that response journal, so neither it nor any suffix word may commit.
    system.advance_system_cycles(20)

    assert requests == _expected_requests("store", image)[:3]
    assert bytes(system._ext_mem[:16]) == image[:16]
    assert bytes(
        system._ext_mem[16:TACC_IMAGE_BYTES]
    ) == sentinel[16:]
    after_due = system._native_system._tacc_transport_snapshot()
    assert not after_due["stage"]["active"]
    assert after_due["port"]["active_grant"] is None
    assert all(
        request is None
        for request in after_due["port"]["pending"]
    )


def test_faulted_external_transfer_releases_engine_for_recovery():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    initial = bytes([0xA5]) * TACC_IMAGE_BYTES
    recovery = bytes(
        (index * 7 + 3) & 0xFF
        for index in range(TACC_IMAGE_BYTES)
    )
    system._ext_mem[:TACC_IMAGE_BYTES] = recovery
    cpu.tacc = initial
    cpu.tacc_dirty = True
    cpu.tsrc0 = EXT_MEM_BASE
    fault_requests: list[ExternalPhyWordRequest] = []

    def fault_first_word(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        fault_requests.append(request)
        return ExternalPhyWordResponse(error=True)

    cpu.set_external_phy_response_plan(fault_first_word)
    fault_code = _install(system, "t.acc.load")
    before_fault = _counters(cpu)

    faulted = system.run_cycle_batch(3, max_instructions=1)

    expected_first = _expected_requests("load", recovery)[0]
    assert faulted.instructions_executed == 0
    assert faulted.system_cycles_advanced == 3
    assert faulted.stop_cycle == 3
    assert faulted.per_core_cycles == (3,)
    assert fault_requests == [expected_first]
    assert cpu.pc == len(fault_code)
    assert cpu.trap_addr == EXT_MEM_BASE
    assert bytes(cpu.tacc) == initial
    assert cpu.tacc_dirty
    assert _counter_delta(cpu, before_fault) == (3, 3, 0, 0, 0)
    _assert_transport_terminal(system, port_grants=1)

    recovery_requests: list[ExternalPhyWordRequest] = []

    def complete_recovery(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        recovery_requests.append(request)
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(complete_recovery)
    recovery_code = _install(system, "t.acc.load")
    before_recovery = _counters(cpu)

    retired = system.run_cycle_batch(37, max_instructions=1)

    assert retired.instructions_executed == 1
    assert retired.system_cycles_advanced == 37
    assert retired.stop_cycle == 40
    assert retired.per_core_cycles == (37,)
    assert cpu.pc == len(recovery_code)
    assert recovery_requests == _expected_requests("load", recovery)
    assert bytes(cpu.tacc) == recovery
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    assert _counter_delta(
        cpu,
        before_recovery,
    ) == (37, 37, 31, 1, 32)
    recovered = system._native_system._tacc_transport_snapshot()
    assert not recovered["stage"]["active"]
    assert recovered["stage"]["grant_count"] == 2
    assert recovered["port"]["active_grant"] is None
    assert recovered["port"]["grant_count"] == 5
    assert all(
        request is None
        for request in recovered["port"]["pending"]
    )


@pytest.mark.parametrize(
    "direction",
    (
        pytest.param("load", id="load"),
        pytest.param("store", id="store"),
    ),
)
def test_external_transfer_is_invariant_to_bounded_call_partition(
    direction: str,
):
    image = bytes(range(TACC_IMAGE_BYTES))

    def configured():
        system = _system(fill=0xCC)
        cpu = system.cpu
        _claim_and_clear(system)
        requests: list[ExternalPhyWordRequest] = []

        def response_plan(
            request: ExternalPhyWordRequest,
        ) -> ExternalPhyWordResponse:
            requests.append(request)
            return ExternalPhyWordResponse()

        cpu.set_external_phy_response_plan(response_plan)
        if direction == "load":
            system._ext_mem[:TACC_IMAGE_BYTES] = image
            cpu.tacc = bytes([0xA5]) * TACC_IMAGE_BYTES
            cpu.tsrc0 = EXT_MEM_BASE
            instruction = "t.acc.load"
        else:
            cpu.tacc = image
            cpu.tdst = EXT_MEM_BASE
            instruction = "t.acc.store"
        cpu.tacc_dirty = True
        code = _install(system, instruction)
        return system, code, requests

    whole, code, whole_requests = configured()
    sliced, _, sliced_requests = configured()
    whole_before = _counters(whole.cpu)
    sliced_before = _counters(sliced.cpu)

    uninterrupted = whole.run_cycle_batch(37, max_instructions=1)
    partitions = (1, 5, 9, 2, 8, 12)
    partitioned = [
        sliced.run_cycle_batch(cycles, max_instructions=1)
        for cycles in partitions
    ]

    assert uninterrupted.instructions_executed == 1
    assert uninterrupted.system_cycles_advanced == 37
    assert uninterrupted.stop_cycle == 37
    assert uninterrupted.per_core_cycles == (37,)
    assert sum(
        result.instructions_executed
        for result in partitioned
    ) == 1
    assert sum(
        result.system_cycles_advanced
        for result in partitioned
    ) == 37
    assert sum(
        result.per_core_cycles[0]
        for result in partitioned
    ) == 37
    expected_requests = _expected_requests(direction, image)
    assert whole_requests == expected_requests
    assert sliced_requests == expected_requests

    def signature(system: MegapadSystem) -> tuple[object, ...]:
        cpu = system.cpu
        return (
            int(cpu.pc),
            _tacc_state(cpu),
            _counters(cpu),
            bytes(system._ext_mem),
            int(system._native_system.system_cycles),
            bool(system._native_system.cycle_execution_pending),
            system._native_system._tacc_transport_snapshot(),
        )

    assert whole.cpu.pc == len(code)
    assert sliced.cpu.pc == len(code)
    assert signature(sliced) == signature(whole)
    assert _counter_delta(
        whole.cpu,
        whole_before,
    ) == (37, 37, 31, 1, 32)
    assert _counter_delta(
        sliced.cpu,
        sliced_before,
    ) == (37, 37, 31, 1, 32)
