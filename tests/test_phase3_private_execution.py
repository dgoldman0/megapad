"""Phase 3 element-2 private full-core command and yield contracts."""

from __future__ import annotations

from collections.abc import Iterable

import pytest

from accel_wrapper import NativeSystemState
from asm import assemble
from system import MegapadSystem


LINE_BYTES = 16
CSR_MBOX = 0x22


def _make_owner(
    code: bytes,
    *,
    worker_count: int = 1,
    full_core_count: int = 1,
    all_core_count: int = 0,
    address: int = 0,
) -> tuple[NativeSystemState, bytearray, tuple]:
    owner = NativeSystemState(
        full_core_count,
        all_core_count,
        worker_count=worker_count,
    )
    memory = bytearray(4096)
    memory[address:address + len(code)] = code
    owner.attach_mem(memory, len(memory))
    cores = tuple(
        owner.core(index)
        for index in range(full_core_count)
    )
    for core in cores:
        core.psel = 3
        core.xsel = 2
        core.spsel = 2
        core.set_reg(3, address)
        core.halted = False
        core.idle = False
        core.ext_modifier = -1
        core.icache_reset()
    return owner, memory, cores


def _prime_instruction_cache(
    cores: Iterable,
    memory: bytearray,
    address: int,
    size: int,
) -> None:
    if size <= 0:
        return
    first_line = address & ~(LINE_BYTES - 1)
    last_line = (
        address + size - 1
    ) & ~(LINE_BYTES - 1)
    for core in cores:
        valid_bytes, tags, data_bytes = core.icache_snapshot()
        valid = bytearray(valid_bytes)
        tags = list(tags)
        data = bytearray(data_bytes)
        line_address = first_line
        while line_address <= last_line:
            index = (line_address >> 4) & 0xFF
            valid[index] = 1
            tags[index] = line_address >> 12
            data_offset = index * LINE_BYTES
            for byte_offset in range(LINE_BYTES):
                data[data_offset + byte_offset] = memory[
                    line_address + byte_offset
                ]
            line_address += LINE_BYTES
        core.icache_restore(
            bytes(valid),
            tags,
            bytes(data),
        )


def _private_state(core) -> tuple:
    return (
        tuple(core.get_reg(index) for index in range(32)),
        core.psel,
        core.xsel,
        core.spsel,
        core.flag_z,
        core.flag_c,
        core.flag_n,
        core.flag_v,
        core.flag_p,
        core.flag_g,
        core.flag_i,
        core.flag_s,
        core.d_reg,
        core.q_out,
        core.t_reg,
        core.halted,
        core.idle,
        core.cycle_count,
        core.ext_modifier,
        core.icache_enabled,
        core.icache_hits,
        core.icache_misses,
        core.icache_snapshot(),
    )


def _run(owner: NativeSystemState, commands: list[tuple]) -> list[dict]:
    return [
        dict(result)
        for result in
        owner._run_private_full_core_commands(commands)
    ]


def _architectural_result(result: dict) -> tuple:
    return (
        result["schema_version"],
        result["command_sequence"],
        result["wave_epoch"],
        result["start_pc"],
        result["end_pc"],
        result["steps_executed"],
        result["total_cycles"],
        result["stop_reason"],
        result["trap_id"],
        result["interrupt_vector"],
    )


def test_cold_instruction_fetch_yields_without_private_mutation() -> None:
    owner, _memory, (core,) = _make_owner(
        assemble("nop\nhalt"),
        worker_count=2,
    )
    before = _private_state(core)

    [result] = _run(owner, [(1, 0, 10)])

    assert result | {
        "thread_token": 0,
    } == {
        "schema_version": 1,
        "command_sequence": 1,
        "wave_epoch": 1,
        "lane_index": 1,
        "core_index": 0,
        "thread_token": 0,
        "start_pc": 0,
        "end_pc": 0,
        "steps_executed": 0,
        "total_cycles": 0,
        "stop_reason": "icache_boundary",
        "trap_id": None,
        "interrupt_vector": None,
    }
    assert result["thread_token"] > 0
    assert _private_state(core) == before


def test_disabled_instruction_cache_yields_without_private_mutation() -> None:
    code = assemble("inc r1\nhalt")
    owner, memory, (core,) = _make_owner(code)
    _prime_instruction_cache((core,), memory, 0, len(code))
    core.icache_enabled = 0
    before = _private_state(core)

    [result] = _run(owner, [(0, 0, 10)])

    assert result["stop_reason"] == "icache_boundary"
    assert result["steps_executed"] == 0
    assert result["start_pc"] == result["end_pc"] == 0
    assert _private_state(core) == before


def test_cross_line_instruction_requires_every_fetched_byte_cached() -> None:
    address = 15
    code = assemble("ldi r1, 0x2a")
    assert len(code) == 3
    owner, memory, (core,) = _make_owner(
        code,
        address=address,
    )
    _prime_instruction_cache(
        (core,), memory, 0, LINE_BYTES)
    before = _private_state(core)

    [boundary] = _run(owner, [(0, 0, 1)])

    assert boundary["stop_reason"] == "icache_boundary"
    assert boundary["steps_executed"] == 0
    assert _private_state(core) == before

    _prime_instruction_cache(
        (core,), memory, LINE_BYTES, LINE_BYTES)
    [completed] = _run(owner, [(0, 0, 1)])

    assert completed["stop_reason"] == "instruction_limit"
    assert completed["steps_executed"] == 1
    assert completed["total_cycles"] == 1
    assert completed["start_pc"] == address
    assert completed["end_pc"] == address + len(code)
    assert core.get_reg(1) == 0x2A
    assert core.icache_misses == 0


@pytest.mark.parametrize(
    ("source", "encoded_length"),
    [
        ("inc r1", 1),
        ("add r1, r2", 2),
        ("ldi r1, 0x2a", 3),
        ("lhi r1, 0x1234", 4),
        ("ldi64 r1, 0x0102030405060708", 11),
        ("rori r1, 4", 3),
        ("inc r16", 2),
    ],
)
def test_private_decoder_admits_each_encoded_length_without_overread(
    source: str,
    encoded_length: int,
) -> None:
    code = assemble(source)
    assert len(code) == encoded_length
    address = LINE_BYTES - encoded_length
    owner, memory, (core,) = _make_owner(
        code,
        address=address,
    )
    core.set_reg(1, 0x8000_0000_0000_0001)
    core.set_reg(2, 3)
    _prime_instruction_cache(
        (core,), memory, 0, LINE_BYTES)

    [result] = _run(owner, [(0, 0, 1)])

    assert result["stop_reason"] == "instruction_limit"
    assert result["steps_executed"] == 1
    assert result["end_pc"] == LINE_BYTES
    assert core.icache_misses == 0


@pytest.mark.parametrize(
    ("condition_is_true", "expected_pc", "expected_cycles"),
    [
        (False, 2, 2),
        (True, 5, 3),
    ],
)
def test_prefixed_skip_uses_only_its_exact_cached_target_peek(
    condition_is_true: bool,
    expected_pc: int,
    expected_cycles: int,
) -> None:
    code = assemble("skip.eq\nldi r1, 99\ninc r2")
    owner, memory, (core,) = _make_owner(code)
    core.flag_z = int(condition_is_true)
    _prime_instruction_cache(
        (core,), memory, 0, len(code))

    [result] = _run(owner, [(0, 0, 1)])

    assert result["stop_reason"] == "instruction_limit"
    assert result["steps_executed"] == 1
    assert result["total_cycles"] == expected_cycles
    assert result["end_pc"] == expected_pc
    assert core.get_reg(1) == 0
    assert core.get_reg(2) == 0


def test_taken_skip_yields_if_its_target_size_byte_is_not_cached() -> None:
    code = assemble("skip.eq\nldi r1, 99")
    address = LINE_BYTES - len(assemble("skip.eq"))
    owner, memory, (core,) = _make_owner(
        code,
        address=address,
    )
    core.flag_z = 1
    _prime_instruction_cache(
        (core,), memory, 0, LINE_BYTES)
    before = _private_state(core)

    [result] = _run(owner, [(0, 0, 1)])

    assert result["stop_reason"] == "icache_boundary"
    assert result["steps_executed"] == 0
    assert _private_state(core) == before


@pytest.mark.parametrize(
    ("code", "expected_reason"),
    [
        (assemble("shl.d"), "instruction_limit"),
        (assemble("ldx"), "shared_instruction"),
        (bytes((0xF7, 0x01)), "shared_instruction"),
        (bytes((0xF0, 0xF1, 0x01)), "shared_instruction"),
        (bytes((0xFC, 0x01)), "shared_instruction"),
    ],
)
def test_memalu_and_reserved_prefixes_fail_closed(
    code: bytes,
    expected_reason: str,
) -> None:
    owner, memory, (core,) = _make_owner(code)
    _prime_instruction_cache(
        (core,), memory, 0, len(code))
    before_private = _private_state(core)
    before_memory = bytes(memory)

    [result] = _run(owner, [(0, 0, 1)])

    assert result["stop_reason"] == expected_reason
    assert result["steps_executed"] == (
        1 if expected_reason == "instruction_limit" else 0
    )
    if expected_reason == "shared_instruction":
        assert _private_state(core) == before_private
    assert bytes(memory) == before_memory


def test_one_two_and_four_lanes_use_one_private_reference_engine() -> None:
    code = assemble(
        """
loop:
    inc r1
    br loop
"""
    )
    signatures = {}
    for worker_count, lane_index in (
        (1, 0),
        (2, 1),
        (4, 3),
    ):
        owner, memory, (core,) = _make_owner(
            code,
            worker_count=worker_count,
        )
        _prime_instruction_cache(
            (core,), memory, 0, len(code))
        [result] = _run(
            owner,
            [(lane_index, 0, 2_001)],
        )
        signatures[worker_count] = (
            _architectural_result(result),
            _private_state(core),
        )

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1][0][5:8] == (
        2_001,
        3_001,
        "instruction_limit",
    )


@pytest.mark.parametrize("worker_count", [2, 4])
def test_every_configured_lane_has_stable_persistent_identity(
    worker_count: int,
) -> None:
    code = assemble(
        """
loop:
    inc r1
    br loop
"""
    )
    owner, memory, cores = _make_owner(
        code,
        worker_count=worker_count,
        full_core_count=worker_count,
    )
    _prime_instruction_cache(
        cores, memory, 0, len(code))
    before = dict(owner._private_worker_diagnostics())
    before_lanes = [
        dict(lane) for lane in before["lanes"]
    ]

    assert before["wave_epoch"] == 0
    assert before["next_command_sequence"] == 1
    assert before["wave_active"] is False
    assert before_lanes[0]["thread_token"] == 0
    assert all(
        lane["thread_token"] > 0
        for lane in before_lanes[1:]
    )
    assert all(
        lane["completed_commands"] == 0
        for lane in before_lanes
    )
    assert all(
        lane["completed_steps"] == 0
        for lane in before_lanes
    )

    commands = [
        (lane_index, lane_index, 20)
        for lane_index in range(worker_count)
    ]
    first = _run(owner, commands)
    first_tokens = tuple(
        result["thread_token"] for result in first
    )
    assert len(set(first_tokens)) == worker_count
    assert all(token > 0 for token in first_tokens)

    second = _run(owner, commands)
    assert tuple(
        result["thread_token"] for result in second
    ) == first_tokens

    after = dict(owner._private_worker_diagnostics())
    after_lanes = [
        dict(lane) for lane in after["lanes"]
    ]
    assert after["wave_epoch"] == 2
    assert after["next_command_sequence"] == (
        1 + 2 * worker_count
    )
    assert after["wave_active"] is False
    assert tuple(
        lane["thread_token"] for lane in after_lanes
    ) == first_tokens
    assert tuple(
        lane["completed_commands"] for lane in after_lanes
    ) == (2,) * worker_count
    assert tuple(
        lane["completed_steps"] for lane in after_lanes
    ) == (40,) * worker_count


def test_helper_mailboxes_survive_rapid_back_to_back_reposts() -> None:
    code = assemble("nop")
    owner, memory, cores = _make_owner(
        code,
        worker_count=4,
        full_core_count=4,
    )
    _prime_instruction_cache(
        cores, memory, 0, len(code))
    commands = [
        (lane_index, lane_index, 0)
        for lane_index in range(4)
    ]

    for _ in range(1_000):
        results = _run(owner, commands)
        assert all(
            result["stop_reason"] == "instruction_limit"
            for result in results
        )

    diagnostics = dict(
        owner._private_worker_diagnostics())
    assert diagnostics["wave_epoch"] == 1_000
    assert tuple(
        dict(lane)["completed_commands"]
        for lane in diagnostics["lanes"]
    ) == (1_000, 1_000, 1_000, 1_000)
    assert tuple(
        dict(lane)["completed_steps"]
        for lane in diagnostics["lanes"]
    ) == (0, 0, 0, 0)


def test_partial_helper_sets_survive_alternating_reposts() -> None:
    code = assemble("nop")
    owner, memory, cores = _make_owner(
        code,
        worker_count=4,
        full_core_count=4,
    )
    _prime_instruction_cache(
        cores, memory, 0, len(code))

    lane_tokens = {}
    for wave in range(500):
        commands = (
            [(1, 1, 0), (3, 3, 0)]
            if wave % 2 == 0
            else [(2, 2, 0)]
        )
        results = _run(owner, commands)
        for result in results:
            lane = result["lane_index"]
            token = result["thread_token"]
            if lane in lane_tokens:
                assert token == lane_tokens[lane]
            else:
                lane_tokens[lane] = token
        assert all(
            result["steps_executed"] == 0
            for result in results
        )

    diagnostics = dict(
        owner._private_worker_diagnostics())
    assert diagnostics["wave_epoch"] == 500
    assert tuple(
        dict(lane)["completed_commands"]
        for lane in diagnostics["lanes"]
    ) == (0, 250, 250, 250)
    assert tuple(
        dict(lane)["completed_steps"]
        for lane in diagnostics["lanes"]
    ) == (0, 0, 0, 0)
    assert set(lane_tokens) == {1, 2, 3}


def test_results_preserve_submission_order_for_different_job_sizes() -> None:
    code = assemble(
        """
loop:
    inc r1
    br loop
"""
    )
    owner, memory, cores = _make_owner(
        code,
        worker_count=4,
        full_core_count=4,
    )
    _prime_instruction_cache(
        cores, memory, 0, len(code))
    commands = [
        (3, 0, 4_000),
        (2, 1, 3),
        (1, 2, 2_000),
        (0, 3, 1),
    ]

    results = _run(owner, commands)

    assert [
        (result["lane_index"], result["core_index"])
        for result in results
    ] == [
        (lane_index, core_index)
        for lane_index, core_index, _budget in commands
    ]
    assert [
        result["command_sequence"]
        for result in results
    ] == [1, 2, 3, 4]
    assert [
        result["steps_executed"]
        for result in results
    ] == [4_000, 3, 2_000, 1]


@pytest.mark.parametrize(
    (
        "source",
        "expected_reason",
        "expected_steps",
        "expected_cycles",
        "expected_trap",
    ),
    [
        (
            "mul r1, r2\nhalt",
            "halted",
            2,
            5,
            None,
        ),
        ("idl", "idle", 1, 1, None),
        ("trap", "trap", 0, 0, 6),
        ("reset", "reset", 0, 0, None),
    ],
)
def test_private_architectural_terminal_results_are_exact(
    source: str,
    expected_reason: str,
    expected_steps: int,
    expected_cycles: int,
    expected_trap: int | None,
) -> None:
    code = assemble(source)
    owner, memory, (core,) = _make_owner(
        code,
        worker_count=2,
    )
    core.set_reg(1, 6)
    core.set_reg(2, 7)
    _prime_instruction_cache(
        (core,), memory, 0, len(code))

    [result] = _run(owner, [(1, 0, 10)])

    assert result["stop_reason"] == expected_reason
    assert result["steps_executed"] == expected_steps
    assert result["total_cycles"] == expected_cycles
    assert result["trap_id"] == expected_trap
    if source.startswith("mul"):
        assert core.get_reg(1) == 42


def test_helper_survives_guest_trap_and_accepts_followup_command() -> None:
    code = assemble("trap\ninc r1")
    owner, memory, (core,) = _make_owner(
        code,
        worker_count=2,
    )
    _prime_instruction_cache(
        (core,), memory, 0, len(code))

    [trapped] = _run(owner, [(1, 0, 10)])
    assert trapped["stop_reason"] == "trap"
    assert trapped["end_pc"] == 1

    [followup] = _run(owner, [(1, 0, 1)])
    assert followup["stop_reason"] == "instruction_limit"
    assert followup["steps_executed"] == 1
    assert core.get_reg(1) == 1
    assert followup["thread_token"] == trapped["thread_token"]


@pytest.mark.parametrize(
    ("source", "expected_vector", "start_idle"),
    [
        ("ipi", 8, True),
        ("timer", 7, False),
    ],
)
def test_eligible_interrupt_yields_before_private_progress(
    source: str,
    expected_vector: int,
    start_idle: bool,
) -> None:
    code = assemble("inc r1")
    owner, memory, (core,) = _make_owner(
        code,
        worker_count=2,
    )
    _prime_instruction_cache(
        (core,), memory, 0, len(code))
    core.flag_i = 1
    core.idle = start_idle
    if source == "ipi":
        owner.set_ipi_line(0, True)
    else:
        core.timer_irq_pending = True
    before = _private_state(core)

    [result] = _run(owner, [(1, 0, 100)])

    assert result["stop_reason"] == "interrupt_boundary"
    assert result["steps_executed"] == 0
    assert result["total_cycles"] == 0
    assert result["interrupt_vector"] == expected_vector
    assert result["trap_id"] is None
    assert _private_state(core) == before
    if source == "ipi":
        assert owner.ipi_line(0)
    else:
        assert core.timer_irq_pending


@pytest.mark.parametrize(
    ("instruction", "register_one"),
    [
        ("st.b r1, r2", 128),
        (f"csrw {CSR_MBOX}, r1", 1),
        ("csrr r4, 0x31", 0),
        ("call.l r1", 128),
        ("ei", 0),
        ("t.add", 0),
        ("crc.init", 0),
    ],
)
def test_shared_or_callback_capable_instruction_yields_before_mutation(
    instruction: str,
    register_one: int,
) -> None:
    code = assemble(f"nop\n{instruction}\nhalt")
    owner, memory, (core, _peer) = _make_owner(
        code,
        worker_count=2,
        full_core_count=2,
    )
    core.set_reg(1, register_one)
    core.set_reg(2, 0xAA)
    core.set_reg(4, 0x55)
    _prime_instruction_cache(
        (core,), memory, 0, len(code))
    memory_before = bytes(memory)
    router_before = (
        owner.ipi_pending_mask(0),
        owner.ipi_pending_mask(1),
    )

    [result] = _run(owner, [(1, 0, 10)])

    assert result["stop_reason"] == "shared_instruction"
    assert result["steps_executed"] == 1
    assert result["total_cycles"] == 1
    assert result["start_pc"] == 0
    assert result["end_pc"] == 1
    assert core.cycle_count == 1
    assert core.icache_misses == 0
    assert bytes(memory) == memory_before
    assert (
        owner.ipi_pending_mask(0),
        owner.ipi_pending_mask(1),
    ) == router_before
    assert core.get_reg(1) == register_one
    assert core.get_reg(2) == 0xAA
    assert core.get_reg(4) == 0x55


def test_zero_budget_is_an_explicit_noop_command() -> None:
    code = assemble("inc r1")
    owner, memory, (core,) = _make_owner(
        code,
        worker_count=2,
    )
    _prime_instruction_cache(
        (core,), memory, 0, len(code))
    before = _private_state(core)

    [result] = _run(owner, [(1, 0, 0)])

    assert result["stop_reason"] == "instruction_limit"
    assert result["steps_executed"] == 0
    assert result["total_cycles"] == 0
    assert result["start_pc"] == result["end_pc"] == 0
    assert _private_state(core) == before


def test_active_event_horizon_rejects_wave_without_progress() -> None:
    code = assemble("inc r1")
    owner, memory, (core,) = _make_owner(
        code,
        worker_count=2,
    )
    _prime_instruction_cache(
        (core,), memory, 0, len(code))
    owner.set_event_deadline(0, 10)
    before = _private_state(core)
    diagnostics_before = dict(
        owner._private_worker_diagnostics())

    with pytest.raises(
        RuntimeError,
        match="active event horizon",
    ):
        _run(owner, [(1, 0, 10)])

    assert _private_state(core) == before
    assert dict(
        owner._private_worker_diagnostics()
    ) == diagnostics_before
    owner.clear_event_deadline(0)


def test_suspended_cycle_execution_rejects_wave_without_progress() -> None:
    code = assemble("mul r1, r2\nhalt")
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=2,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    system.cpu.regs[1] = 6
    system.cpu.regs[2] = 7
    _prime_instruction_cache(
        (system.cpu._cs,),
        system.cpu.mem,
        0,
        len(code),
    )
    first = system.run_cycle_batch(
        3, max_instructions=10)
    assert first.instructions_executed == 0
    assert system._native_system.cycle_execution_pending
    before = _private_state(system.cpu._cs)
    cycles_before = system._native_system.system_cycles
    diagnostics_before = dict(
        system._native_system._private_worker_diagnostics()
    )

    with pytest.raises(
        RuntimeError,
        match="suspended cycle operation",
    ):
        _run(
            system._native_system,
            [(1, 0, 10)],
        )

    assert _private_state(system.cpu._cs) == before
    assert system._native_system.system_cycles == cycles_before
    assert dict(
        system._native_system._private_worker_diagnostics()
    ) == diagnostics_before


def test_invalid_wave_is_rejected_before_any_core_progress() -> None:
    code = assemble("inc r1")
    owner, memory, cores = _make_owner(
        code,
        worker_count=2,
        full_core_count=2,
    )
    _prime_instruction_cache(
        cores, memory, 0, len(code))
    before = tuple(_private_state(core) for core in cores)
    diagnostics_before = dict(
        owner._private_worker_diagnostics())

    invalid_waves = [
        ([(2, 0, 1)], ValueError, "lane_index"),
        ([(-1, 0, 1)], ValueError, "lane_index"),
        ([(0, 2, 1)], ValueError, "core_index"),
        ([(0, -1, 1)], ValueError, "core_index"),
        ([(0, 0, -1)], ValueError, "max_steps"),
        (
            [(0, 0, 1 << 31)],
            ValueError,
            "max_steps",
        ),
        (
            [(0, 0, 1), (0, 1, 1)],
            ValueError,
            "two commands to one lane",
        ),
        (
            [(0, 0, 1), (1, 0, 1)],
            ValueError,
            "one core twice",
        ),
        (
            [(True, 0, 1)],
            TypeError,
            "lane_index",
        ),
        (
            [(0, 0, "1")],
            TypeError,
            "max_steps",
        ),
        (
            [(0, 0)],
            ValueError,
            "must contain",
        ),
    ]
    for commands, error_type, match in invalid_waves:
        with pytest.raises(error_type, match=match):
            _run(owner, commands)

    assert tuple(
        _private_state(core) for core in cores
    ) == before
    assert dict(
        owner._private_worker_diagnostics()
    ) == diagnostics_before


def test_microcore_index_cannot_enter_private_full_core_protocol() -> None:
    owner, memory, (full_core,) = _make_owner(
        assemble("nop"),
        worker_count=2,
        full_core_count=1,
        all_core_count=5,
    )
    _prime_instruction_cache(
        (full_core,), memory, 0, 1)

    with pytest.raises(ValueError, match="core_index"):
        _run(owner, [(1, 1, 1)])
