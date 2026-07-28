"""Cycle-bounded TACC recovery and chip-wide transfer-stage contracts."""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    EW_BF16,
    EW_FP16,
    EW_U16,
    EW_U32,
    EW_U8,
    TACC_IMAGE_BYTES,
    TACC_OWNER_NONE,
)
from system import MegapadSystem


_STAGE_FIELDS = {
    "schema_version",
    "engine_count",
    "active",
    "direction",
    "owner_engine_id",
    "owner_core_id",
    "engine_epoch",
    "caller_epoch",
    "stage_epoch",
    "base_address",
    "format_ew",
    "format_signed",
    "beat_index",
    "image",
    "last_grant_engine_id",
    "grant_sequence",
}


def _system(
    *,
    full_cores: int = 1,
    clusters: int = 0,
) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    original_load_binary = system.load_binary

    def load_binary(address: int, data: bytes | bytearray) -> None:
        original_load_binary(address, data)
        if not data:
            return
        first_line = address & ~0xF
        last_line = (address + len(data) - 1) & ~0xF
        for cpu in system.cores[:full_cores]:
            valid_bytes, tags, data_bytes = cpu._cs.icache_snapshot()
            valid = bytearray(valid_bytes)
            tags = list(tags)
            cache_data = bytearray(data_bytes)
            line_address = first_line
            while line_address <= last_line:
                index = (line_address >> 4) & 0xFF
                valid[index] = 1
                tags[index] = line_address >> 12
                data_offset = index * 16
                cache_data[data_offset:data_offset + 16] = cpu.mem[
                    line_address:line_address + 16
                ]
                line_address += 16
            cpu._cs.icache_restore(
                bytes(valid),
                tags,
                bytes(cache_data),
            )

    # Host code replacement is intentionally non-coherent in production.
    # This file primes exact bytes explicitly so it measures only TACC timing.
    system.load_binary = load_binary
    return system


def _tacc_status(cpu) -> dict[str, int | bool]:
    value = cpu.csr_read(CSR_TACC_STATUS)
    return {
        "claimed": bool(value & (1 << 0)),
        "mine": bool(value & (1 << 1)),
        "valid": bool(value & (1 << 2)),
        "dirty": bool(value & (1 << 3)),
        "busy": bool(value & (1 << 4)),
        "force_pending": bool(value & (1 << 9)),
        "owner": (value >> 16) & 0x1F,
    }


def _tacc_domain(cpu) -> dict[str, object]:
    return {
        "tacc": bytes(cpu.tacc),
        "tacc_owner": int(cpu.tacc_owner),
        "tacc_valid": bool(cpu.tacc_valid),
        "tacc_dirty": bool(cpu.tacc_dirty),
        "tacc_format_ew": int(cpu.tacc_format_ew),
        "tacc_format_signed": int(cpu.tacc_format_signed),
        "tacc_busy": bool(cpu.tacc_busy),
        "tacc_force_pending": bool(cpu.tacc_force_pending),
        "tacc_epoch": int(cpu.tacc_epoch),
    }


def _architectural_state(cpu) -> dict[str, object]:
    """Capture every architected CPU field TAMAC could accidentally disturb."""
    return {
        "regs": tuple(cpu.regs[index] for index in range(32)),
        "selectors": (cpu.psel, cpu.xsel, cpu.spsel),
        "acc": tuple(cpu.acc[index] for index in range(4)),
        "flags": (
            cpu.flag_z,
            cpu.flag_c,
            cpu.flag_n,
            cpu.flag_v,
            cpu.flag_p,
            cpu.flag_g,
            cpu.flag_i,
            cpu.flag_s,
        ),
        "scalar": (cpu.d_reg, cpu.q_out, cpu.t_reg),
        "tile_cursor": (cpu.sb, cpu.sr, cpu.sc, cpu.sw),
        "tile": (
            cpu.tmode,
            cpu.tctrl,
            cpu.tsrc0,
            cpu.tsrc1,
            cpu.tdst,
            cpu.tstride_r,
            cpu.tstride_c,
            cpu.ttile_h,
            cpu.ttile_w,
        ),
        "tacc": _tacc_domain(cpu),
        "interrupt": (
            cpu.ivt_base,
            cpu.ivec_id,
            cpu.trap_addr,
            cpu.ef_flags,
        ),
        "execution": (
            cpu.pc,
            bool(cpu.halted),
            bool(cpu.idle),
            cpu.cycle_count,
        ),
        "performance": (
            bool(cpu.perf_enable),
            cpu.perf_cycles,
            cpu.perf_stalls,
            cpu.perf_tileops,
            cpu.perf_extmem,
        ),
        "protection": (cpu.priv_level, cpu.mpu_base, cpu.mpu_limit),
        "crypto": (
            cpu.crc_acc,
            cpu.crc_mode,
            cpu.sha_mode,
            cpu.sha_msglen_lo,
            cpu.sha_msglen_hi,
            cpu.gf_prime_sel,
        ),
        "ext_modifier": cpu._ext_modifier,
        "memory": bytes(cpu.mem),
    }


def _claim_and_clear(
    system: MegapadSystem,
    *,
    ew: int = EW_U8,
) -> None:
    cpu = system.cpu
    cpu.tmode = ew
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        cpu.pc = 0
        cpu.step()


def _install_tamac(system: MegapadSystem, *, halt_address: int = 2) -> None:
    cpu = system.cpu
    cpu.tsrc0 = 0x200
    cpu.tsrc1 = 0x240
    cpu.mem[0x200:0x240] = bytes([2]) * 64
    cpu.mem[0x240:0x280] = bytes([3]) * 64
    system.load_binary(0, assemble("t.amac"))
    system.load_binary(halt_address, assemble("halt"))
    cpu.pc = 0


@pytest.mark.parametrize(
    ("instruction", "expected_cycles"),
    [
        pytest.param("t.acc.try", 2, id="try-2"),
        pytest.param("t.acc.clear", 2, id="clear-2"),
        pytest.param("t.acc.release", 2, id="release-2"),
        pytest.param("t.acc.load", 9, id="load-9"),
        pytest.param("t.acc.store", 9, id="store-9"),
    ],
)
def test_cycle_api_locks_lifecycle_and_transfer_latencies(
    instruction: str,
    expected_cycles: int,
):
    system = _system()
    cpu = system.cpu
    if instruction != "t.acc.try":
        _claim_and_clear(system)
    if instruction == "t.acc.load":
        cpu.tsrc0 = 0x200
        cpu.mem[0x200:0x300] = bytes(range(TACC_IMAGE_BYTES))
    elif instruction == "t.acc.store":
        cpu.tdst = 0x300
        cpu.tacc = bytes([0xA5]) * TACC_IMAGE_BYTES
        cpu.tacc_dirty = True

    code = assemble(instruction)
    system.load_binary(0, code)
    cpu.pc = 0
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops

    result = system.run_cycle_batch(
        expected_cycles,
        max_instructions=1,
    )

    assert result.instructions_executed == 1
    assert result.per_core_instructions == (1,)
    assert result.per_core_cycles == (expected_cycles,)
    assert result.system_cycles_advanced == expected_cycles
    assert cpu.pc == len(code)
    assert cpu.cycle_count - cycles_before == expected_cycles
    assert cpu.perf_cycles - perf_before == expected_cycles
    assert cpu.perf_tileops == tileops_before + 1
    assert not _tacc_status(cpu)["busy"]
    assert not system._native_system.cycle_execution_pending


@pytest.mark.parametrize(
    ("ew", "instruction", "expected_cycles"),
    [
        pytest.param(EW_U8, "t.amac", 7, id="u8-tile-7"),
        pytest.param(EW_U8, "t.amac r7", 6, id="u8-broadcast-6"),
        pytest.param(EW_U16, "t.amac", 5, id="u16-tile-5"),
        pytest.param(EW_U16, "t.amac r7", 4, id="u16-broadcast-4"),
        pytest.param(EW_U16, "t.amac inplace", 5, id="u16-inplace-5"),
        pytest.param(EW_U32, "t.amac", 4, id="u32-tile-4"),
        pytest.param(EW_U32, "t.amac r7", 3, id="u32-broadcast-3"),
        pytest.param(EW_FP16, "t.amac", 7, id="fp16-tile-7"),
        pytest.param(EW_FP16, "t.amac r7", 6, id="fp16-broadcast-6"),
        pytest.param(EW_BF16, "t.amac", 7, id="bf16-tile-7"),
        pytest.param(EW_BF16, "t.amac r7", 6, id="bf16-broadcast-6"),
    ],
)
def test_cycle_api_locks_normal_tamac_latencies(
    ew: int,
    instruction: str,
    expected_cycles: int,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system, ew=ew)
    if ew == EW_FP16:
        raw_one = 0x3C00
    elif ew == EW_BF16:
        raw_one = 0x3F80
    else:
        raw_one = 1
    if ew in (EW_FP16, EW_BF16):
        source = raw_one.to_bytes(2, "little") * 32
    else:
        source = bytes([raw_one]) * 64
    cpu.mem[0x200:0x240] = source
    cpu.mem[0x240:0x280] = source
    cpu.mem[0x280:0x2C0] = source
    cpu.tsrc0 = 0x200
    cpu.tsrc1 = 0x240
    cpu.tdst = 0x280
    cpu.regs[7] = raw_one

    code = assemble(instruction)
    system.load_binary(0, code)
    cpu.pc = 0
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops

    result = system.run_cycle_batch(
        expected_cycles,
        max_instructions=1,
    )

    assert result.instructions_executed == 1
    assert result.per_core_instructions == (1,)
    assert result.per_core_cycles == (expected_cycles,)
    assert result.system_cycles_advanced == expected_cycles
    assert cpu.pc == len(code)
    assert cpu.cycle_count - cycles_before == expected_cycles
    assert cpu.perf_cycles - perf_before == expected_cycles
    assert cpu.perf_tileops == tileops_before + 1
    assert cpu.tacc_dirty
    assert not _tacc_status(cpu)["busy"]


def test_admitted_u8_tamac_partition_matches_one_shot_architecture():
    sliced = _system()
    whole = _system()
    for system in (sliced, whole):
        _claim_and_clear(system)
        _install_tamac(system)
        system.cpu.regs[5] = 0xA5A5_5A5A_DEAD_BEEF

    sliced_calls = 0
    sliced_fallback = sliced.cpu._step_python_fallback

    def observe_sliced_fallback(*, strict_tacc_epoch: bool = False):
        nonlocal sliced_calls
        sliced_calls += 1
        return sliced_fallback(strict_tacc_epoch=strict_tacc_epoch)

    sliced.cpu._step_python_fallback = observe_sliced_fallback
    whole_calls = 0
    whole_fallback = whole.cpu._step_python_fallback

    def observe_whole_fallback(*, strict_tacc_epoch: bool = False):
        nonlocal whole_calls
        whole_calls += 1
        return whole_fallback(strict_tacc_epoch=strict_tacc_epoch)

    whole.cpu._step_python_fallback = observe_whole_fallback
    sliced_cycles_before = sliced.cpu.cycle_count
    whole_cycles_before = whole.cpu.cycle_count
    sliced_tileops_before = sliced.cpu.perf_tileops
    whole_tileops_before = whole.cpu.perf_tileops

    before_terminal = sliced.run_cycle_batch(6, max_instructions=1)

    assert before_terminal.instructions_executed == 0
    assert before_terminal.per_core_cycles == (0,)
    assert before_terminal.system_cycles_advanced == 6
    assert sliced_calls == 0
    assert _tacc_status(sliced.cpu)["busy"]
    assert sliced._native_system.cycle_execution_pending

    terminal = sliced.run_cycle_batch(1, max_instructions=1)
    uninterrupted = whole.run_cycle_batch(7, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.per_core_instructions == (1,)
    assert terminal.per_core_cycles == (7,)
    assert terminal.system_cycles_advanced == 1
    assert uninterrupted.instructions_executed == 1
    assert uninterrupted.per_core_instructions == (1,)
    assert uninterrupted.per_core_cycles == (7,)
    assert uninterrupted.system_cycles_advanced == 7
    assert sliced_calls == 1
    assert whole_calls == 1
    assert sliced.cpu.cycle_count - sliced_cycles_before == 7
    assert whole.cpu.cycle_count - whole_cycles_before == 7
    assert sliced.cpu.perf_tileops == sliced_tileops_before + 1
    assert whole.cpu.perf_tileops == whole_tileops_before + 1
    assert int(sliced._native_system.system_cycles) == 7
    assert int(whole._native_system.system_cycles) == 7
    assert _architectural_state(sliced.cpu) == _architectural_state(whole.cpu)
    assert not sliced._native_system.cycle_execution_pending
    assert not whole._native_system.cycle_execution_pending


def test_misaligned_load_validation_fault_never_publishes_busy():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    cpu.tacc = bytes([0x5A]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.tsrc0 = 0x201
    code = assemble("t.acc.load")
    system.load_binary(0, code)
    cpu.pc = 0
    before_tacc = _tacc_domain(cpu)
    before_memory = bytes(cpu.mem)
    before_regs = tuple(cpu.regs[index] for index in range(32))
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    original_read8 = cpu.mem_read8
    source_reads = 0

    def observe_source(address: int) -> int:
        nonlocal source_reads
        if 0x201 <= address < 0x201 + TACC_IMAGE_BYTES:
            source_reads += 1
        return original_read8(address)

    cpu.mem_read8 = observe_source
    before_terminal = system.run_cycle_batch(1, max_instructions=1)

    assert before_terminal.instructions_executed == 0
    assert before_terminal.per_core_cycles == (0,)
    assert before_terminal.system_cycles_advanced == 1
    assert not _tacc_status(cpu)["busy"]
    assert _tacc_domain(cpu) == before_tacc
    assert source_reads == 0
    assert system._native_system.cycle_execution_pending

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 0
    assert terminal.per_core_instructions == (0,)
    assert terminal.per_core_cycles == (2,)
    assert terminal.system_cycles_advanced == 1
    assert cpu.pc == len(code)
    assert cpu.trap_addr == 0x201
    assert cpu.cycle_count - cycles_before == 2
    assert cpu.perf_cycles - perf_before == 2
    assert cpu.perf_tileops == tileops_before
    assert _tacc_domain(cpu) == before_tacc
    assert not _tacc_status(cpu)["busy"]
    assert source_reads == 0
    assert bytes(cpu.mem) == before_memory
    assert all(
        cpu.regs[index] == before_regs[index]
        for index in range(32)
        if index != cpu.psel
    )
    assert not system._native_system.cycle_execution_pending


@pytest.mark.parametrize("instruction", ("t.acc.load", "t.acc.store"))
def test_cycle_image_preflight_faults_outside_user_mpu_without_a_beat(
    instruction: str,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    base = 0x200
    cpu.tsrc0 = base
    cpu.tdst = base
    cpu.tacc = bytes([0xD2]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.mem[base:base + TACC_IMAGE_BYTES] = bytes(
        [0xA5]
    ) * TACC_IMAGE_BYTES
    code = assemble(instruction)
    system.load_binary(0, code)
    cpu.pc = 0
    before_tacc = _tacc_domain(cpu)
    before_memory = bytes(
        cpu.mem[base:base + TACC_IMAGE_BYTES]
    )
    cycles_before = cpu.cycle_count
    tileops_before = cpu.perf_tileops
    cpu.priv_level = 1
    cpu.mpu_base = base
    cpu.mpu_limit = base + 128

    faulted = system.run_cycle_batch(2, max_instructions=1)

    assert faulted.instructions_executed == 0
    assert faulted.per_core_cycles == (2,)
    assert faulted.system_cycles_advanced == 2
    assert cpu.pc == len(code)
    assert cpu.trap_addr == base + 128
    assert cpu.cycle_count - cycles_before == 2
    assert cpu.perf_tileops == tileops_before
    assert _tacc_domain(cpu) == before_tacc
    assert bytes(
        cpu.mem[base:base + TACC_IMAGE_BYTES]
    ) == before_memory
    transport = system._native_system._tacc_transport_snapshot()
    assert transport["stage"]["grant_count"] == 0
    assert transport["port"]["grant_count"] == 0
    assert not system._native_system.cycle_execution_pending


@pytest.mark.parametrize("instruction", ("t.acc.load", "t.acc.store"))
def test_cycle_image_transfer_allows_user_span_inside_active_mpu(
    instruction: str,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    base = 0x200
    image = bytes(range(TACC_IMAGE_BYTES))
    cpu.tsrc0 = base
    cpu.tdst = base
    cpu.tacc = image
    cpu.tacc_dirty = True
    cpu.mem[base:base + TACC_IMAGE_BYTES] = image
    code = assemble(instruction)
    system.load_binary(0, code)
    cpu.pc = 0
    cpu.priv_level = 1
    cpu.mpu_base = base
    cpu.mpu_limit = base + TACC_IMAGE_BYTES

    retired = system.run_cycle_batch(9, max_instructions=1)

    assert retired.instructions_executed == 1
    assert retired.per_core_cycles == (9,)
    assert retired.system_cycles_advanced == 9
    assert cpu.pc == len(code)
    assert not cpu.tacc_busy
    transport = system._native_system._tacc_transport_snapshot()
    assert transport["stage"]["grant_count"] == 1
    assert transport["port"]["grant_count"] == 4


def test_unowned_clear_validation_fault_is_one_cycle_without_busy():
    system = _system()
    cpu = system.cpu
    code = assemble("t.acc.clear")
    system.load_binary(0, code)
    cpu.pc = 0
    before = _tacc_domain(cpu)
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops

    result = system.run_cycle_batch(1, max_instructions=1)

    assert result.instructions_executed == 0
    assert result.per_core_instructions == (0,)
    assert result.per_core_cycles == (1,)
    assert result.system_cycles_advanced == 1
    assert cpu.pc == len(code)
    assert cpu.cycle_count - cycles_before == 1
    assert cpu.perf_cycles - perf_before == 1
    assert cpu.perf_tileops == tileops_before
    # The complete faulting instruction is consumed, but the engine itself
    # remains byte-for-byte untouched and never publishes BUSY.
    assert _tacc_domain(cpu) == before
    assert not _tacc_status(cpu)["busy"]
    assert not system._native_system.cycle_execution_pending


def test_force_pending_wipes_load_at_fourth_ack_terminal_boundary():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    cpu.tacc = bytes([0x5A]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    source = 0x200
    cpu.tsrc0 = source
    cpu.mem[source:source + TACC_IMAGE_BYTES] = (
        bytes([0xC3]) * TACC_IMAGE_BYTES
    )
    code = assemble("t.acc.load")
    system.load_binary(0, code)
    cpu.pc = 0
    initial_epoch = cpu.tacc_epoch
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    before_terminal = system.run_cycle_batch(5, max_instructions=1)

    assert before_terminal.instructions_executed == 0
    assert before_terminal.per_core_cycles == (0,)
    assert before_terminal.system_cycles_advanced == 5
    assert _tacc_status(cpu)["busy"]

    cpu.csr_write(CSR_TACC_CTL, 1)

    pending = _tacc_status(cpu)
    assert pending["busy"]
    assert pending["force_pending"]

    terminal = system.run_cycle_batch(4, max_instructions=1)

    assert terminal.instructions_executed == 1
    assert terminal.per_core_instructions == (1,)
    assert terminal.per_core_cycles == (9,)
    assert terminal.system_cycles_advanced == 4
    assert cpu.pc == len(code)
    assert cpu.cycle_count - cycles_before == 9
    assert cpu.perf_cycles - perf_before == 9
    assert cpu.perf_tileops == tileops_before + 1
    assert _tacc_status(cpu) == {
        "claimed": False,
        "mine": False,
        "valid": False,
        "dirty": False,
        "busy": False,
        "force_pending": False,
        "owner": TACC_OWNER_NONE,
    }
    assert cpu.tacc_epoch == initial_epoch + 1
    assert not any(cpu.tacc)
    assert not system._native_system._tacc_image_stage_snapshot()["active"]
    assert not system._native_system.cycle_execution_pending


def test_reset_callback_drops_terminal_tamac_without_state_resurrection():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    _install_tamac(system)
    cpu.regs[5] = 0xA5A5_5A5A_DEAD_BEEF
    initial_epoch = cpu.tacc_epoch
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    reset_address = cpu.tsrc0
    original_read8 = cpu.mem_read8
    callback_count = 0

    def reset_during_source_read(address: int) -> int:
        nonlocal callback_count
        if address == reset_address:
            callback_count += 1
            cpu._reset_state_in_memory_scope()
        return original_read8(address)

    cpu.mem_read8 = reset_during_source_read
    before_terminal = system.run_cycle_batch(6, max_instructions=1)

    assert before_terminal.instructions_executed == 0
    assert before_terminal.per_core_cycles == (0,)
    assert before_terminal.system_cycles_advanced == 6
    assert _tacc_status(cpu)["busy"]
    assert callback_count == 0

    terminal = system.run_cycle_batch(1, max_instructions=1)

    assert terminal.instructions_executed == 0
    assert terminal.per_core_instructions == (0,)
    assert terminal.per_core_cycles == (0,)
    assert terminal.system_cycles_advanced == 1
    assert callback_count == 1
    assert cpu.pc == 0
    assert cpu.regs[5] == 0
    assert cpu.cycle_count == cycles_before
    assert cpu.perf_cycles == perf_before
    assert cpu.perf_tileops == tileops_before
    assert _tacc_status(cpu) == {
        "claimed": False,
        "mine": False,
        "valid": False,
        "dirty": False,
        "busy": False,
        "force_pending": False,
        "owner": TACC_OWNER_NONE,
    }
    assert cpu.tacc_epoch == initial_epoch + 1
    assert not any(cpu.tacc)
    assert not system._native_system.cycle_execution_pending


@pytest.mark.parametrize(
    "execution_surface",
    ("step", "core-batch", "system-batch"),
)
def test_unbounded_reset_callback_cancels_tamac_without_retirement(
    execution_surface: str,
):
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    _install_tamac(system)
    cpu.regs[5] = 0xA5A5_5A5A_DEAD_BEEF
    initial_epoch = cpu.tacc_epoch
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    reset_address = cpu.tsrc0
    original_read8 = cpu.mem_read8
    callback_count = 0

    def reset_during_source_read(address: int) -> int:
        nonlocal callback_count
        if address == reset_address:
            callback_count += 1
            cpu._reset_state_in_memory_scope()
        return original_read8(address)

    cpu.mem_read8 = reset_during_source_read
    if execution_surface == "step":
        assert cpu.step() == 0
    elif execution_surface == "core-batch":
        stats = cpu.run_steps_stats(1)
        assert stats.steps_executed == 0
        assert stats.total_cycles == 0
        assert stats.stop_reason == 0
    else:
        stats = system.run_batch_stats(1)
        assert stats.instructions_executed == 0
        assert stats.per_core_instructions == (0,)
        assert stats.per_core_cycles == (0,)

    assert callback_count == 1
    assert cpu.pc == 0
    assert cpu.regs[5] == 0
    assert cpu.cycle_count == cycles_before
    assert cpu.perf_cycles == perf_before
    assert cpu.perf_tileops == tileops_before
    assert _tacc_status(cpu) == {
        "claimed": False,
        "mine": False,
        "valid": False,
        "dirty": False,
        "busy": False,
        "force_pending": False,
        "owner": TACC_OWNER_NONE,
    }
    assert cpu.tacc_epoch == initial_epoch + 1
    assert not any(cpu.tacc)


def test_strict_cycle_try_publishes_busy_before_terminal_owner():
    system = _system()
    cpu = system.cpu
    system.load_binary(0, assemble("t.acc.try\nhalt"))
    cpu.pc = 0

    suspended = system.run_cycle_batch(1, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert suspended.system_cycles_advanced == 1
    assert _tacc_status(cpu) == {
        "claimed": False,
        "mine": False,
        "valid": False,
        "dirty": False,
        "busy": True,
        "force_pending": False,
        "owner": TACC_OWNER_NONE,
    }

    retired = system.run_cycle_batch(1, max_instructions=1)

    assert retired.instructions_executed == 1
    assert retired.system_cycles_advanced == 1
    assert _tacc_status(cpu) == {
        "claimed": True,
        "mine": True,
        "valid": False,
        "dirty": False,
        "busy": False,
        "force_pending": False,
        "owner": cpu.core_id,
    }


def test_long_tamac_accepts_force_between_slices_and_wipes_at_terminal():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    _install_tamac(system)
    initial_epoch = cpu.tacc_epoch

    suspended = system.run_cycle_batch(3, max_instructions=1)

    assert suspended.instructions_executed == 0
    active = _tacc_status(cpu)
    assert active["busy"]
    assert active["mine"]
    assert not active["force_pending"]

    cpu.csr_write(CSR_TACC_CTL, 1)

    pending = _tacc_status(cpu)
    assert pending["busy"]
    assert pending["force_pending"]
    assert pending["owner"] == cpu.core_id

    retired = system.run_cycle_batch(4, max_instructions=1)

    assert retired.instructions_executed == 1
    terminal = _tacc_status(cpu)
    assert not terminal["claimed"]
    assert not terminal["busy"]
    assert not terminal["force_pending"]
    assert terminal["owner"] == TACC_OWNER_NONE
    assert cpu.tacc_epoch == initial_epoch + 1
    assert not any(cpu.tacc)


def test_boot_cancels_suspended_tamac_without_a_late_commit():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    _install_tamac(system, halt_address=0x80)
    initial_epoch = cpu.tacc_epoch

    suspended = system.run_cycle_batch(2, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert _tacc_status(cpu)["busy"]

    system.boot(entry=0x80)

    reset_state = _tacc_status(cpu)
    assert not reset_state["claimed"]
    assert not reset_state["busy"]
    assert reset_state["owner"] == TACC_OWNER_NONE
    assert cpu.tacc_epoch > initial_epoch
    assert not any(cpu.tacc)

    system.run_cycle_batch(16, max_instructions=1)

    final_state = _tacc_status(cpu)
    assert not final_state["claimed"]
    assert not final_state["valid"]
    assert not final_state["busy"]
    assert final_state["owner"] == TACC_OWNER_NONE
    assert not any(cpu.tacc)


def test_arbitrary_callback_error_is_one_shot_and_drops_continuation():
    system = _system()
    cpu = system.cpu
    _claim_and_clear(system)
    _install_tamac(system)
    before = _tacc_domain(cpu)
    original_read8 = cpu.mem_read8
    injected = ValueError("injected TACC source callback failure")
    callback_count = 0

    def fail_source_once(address: int) -> int:
        nonlocal callback_count
        if address == cpu.tsrc0:
            callback_count += 1
            raise injected
        return original_read8(address)

    cpu.mem_read8 = fail_source_once
    suspended = system.run_cycle_batch(1, max_instructions=1)
    assert suspended.instructions_executed == 0
    assert _tacc_status(cpu)["busy"]

    with pytest.raises(ValueError) as raised:
        system.run_cycle_batch(6, max_instructions=1)
    assert raised.value is injected
    assert callback_count == 1

    terminal = _tacc_domain(cpu)
    assert terminal["tacc"] == before["tacc"]
    assert terminal["tacc_owner"] == before["tacc_owner"]
    assert terminal["tacc_valid"] == before["tacc_valid"]
    assert terminal["tacc_dirty"] == before["tacc_dirty"]
    assert terminal["tacc_format_ew"] == before["tacc_format_ew"]
    assert terminal["tacc_format_signed"] == before["tacc_format_signed"]
    assert not terminal["tacc_busy"]
    assert not terminal["tacc_force_pending"]
    assert terminal["tacc_epoch"] == before["tacc_epoch"]

    system.run_cycle_batch(16, max_instructions=1)

    assert callback_count == 1
    assert not _tacc_status(cpu)["busy"]


def _publish_busy_full_engine(cpu, image: bytes) -> int:
    cpu.tacc = image
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc_format_signed = 0
    cpu.tacc_busy = True
    cpu.tacc_force_pending = False
    return int(cpu.tacc_epoch)


def _publish_busy_cluster_engine(cluster, cpu, image: bytes) -> int:
    cluster.load_shared_engine_state(cpu)
    cpu.tacc = image
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc_format_signed = 0
    cpu.tacc_busy = True
    cpu.tacc_force_pending = False
    assert cluster.store_shared_engine_state(cpu)
    return int(cluster._shared_engine_snapshot()["tacc_epoch"])


def test_chip_wide_image_stage_schema_validation_and_targeted_cancellation():
    system = _system(full_cores=4, clusters=3)
    native = system._native_system
    initial = dict(native._tacc_image_stage_snapshot())

    assert set(initial) == _STAGE_FIELDS
    assert initial["schema_version"] == 1
    assert initial["engine_count"] == 7
    assert initial["active"] is False
    assert initial["direction"] == "none"
    assert initial["owner_engine_id"] is None
    assert initial["owner_core_id"] is None
    assert initial["image"] == bytes(TACC_IMAGE_BYTES)

    detached = dict(initial)
    detached["image"] = bytes([0xA5]) * TACC_IMAGE_BYTES
    assert dict(native._tacc_image_stage_snapshot()) == initial

    malformed = dict(initial)
    malformed["beat_index"] = 1
    with pytest.raises(ValueError):
        native._tacc_image_stage_restore(malformed)
    assert dict(native._tacc_image_stage_snapshot()) == initial

    full = system.cores[2]
    full_image = bytes([0x32]) * TACC_IMAGE_BYTES
    full_epoch = _publish_busy_full_engine(full, full_image)
    accepted, full_stage_epoch = native._tacc_image_stage_acquire(
        full.core_id,
        "store",
        0x400,
        EW_U8,
        False,
        full_epoch,
        0,
        full_image,
    )
    assert accepted
    assert native.cycle_execution_pending

    full_stage = dict(native._tacc_image_stage_snapshot())
    assert full_stage["active"]
    assert full_stage["owner_engine_id"] == 2
    assert full_stage["owner_core_id"] == full.core_id == 2
    assert full_stage["stage_epoch"] == full_stage_epoch
    assert full_stage["image"] == full_image

    native._cancel_tacc_image_stage_for_core(1)
    assert dict(native._tacc_image_stage_snapshot()) == full_stage
    assert native.cycle_execution_pending
    native._cancel_tacc_image_stage_for_core(full.core_id)
    assert not native._tacc_image_stage_snapshot()["active"]
    assert not native.cycle_execution_pending

    cluster = system.clusters[1]
    micro_owner, other_micro = cluster.cores[:2]
    assert micro_owner.core_id == 8
    assert other_micro.core_id == 9
    micro_image = bytes([0x58]) * TACC_IMAGE_BYTES
    micro_engine_epoch = _publish_busy_cluster_engine(
        cluster,
        micro_owner,
        micro_image,
    )
    caller_epochs = list(
        native._cluster_tacc_caller_epochs_snapshot(1)
    )
    micro_caller_epoch = int(caller_epochs[0])
    accepted, micro_stage_epoch = native._tacc_image_stage_acquire(
        micro_owner.core_id,
        "store",
        0x600,
        EW_U8,
        False,
        micro_engine_epoch,
        micro_caller_epoch,
        micro_image,
    )
    assert accepted
    assert native.cycle_execution_pending

    micro_stage = dict(native._tacc_image_stage_snapshot())
    assert micro_stage["active"]
    assert micro_stage["owner_engine_id"] == 5
    assert micro_stage["owner_core_id"] == micro_owner.core_id == 8
    assert micro_stage["stage_epoch"] == micro_stage_epoch
    assert micro_stage["caller_epoch"] == micro_caller_epoch

    changed_owner = dict(micro_stage)
    changed_owner["owner_core_id"] = other_micro.core_id
    with pytest.raises(ValueError):
        native._tacc_image_stage_restore(changed_owner)
    assert dict(native._tacc_image_stage_snapshot()) == micro_stage

    cluster.cancel_tacc_caller(other_micro.core_id)
    assert dict(native._tacc_image_stage_snapshot()) == micro_stage
    assert native.cycle_execution_pending
    cluster.cancel_tacc_caller(micro_owner.core_id)

    cancelled = dict(native._tacc_image_stage_snapshot())
    assert not cancelled["active"]
    assert cancelled["direction"] == "none"
    assert cancelled["owner_engine_id"] is None
    assert cancelled["owner_core_id"] is None
    assert cancelled["stage_epoch"] == micro_stage_epoch + 1
    assert not native.cycle_execution_pending


def test_image_stage_updates_roll_back_atomically_and_global_reset_clears_rr():
    system = _system()
    cpu = system.cpu
    native = system._native_system
    initial = dict(native._tacc_image_stage_snapshot())
    engine_epoch = _publish_busy_full_engine(
        cpu,
        bytes([0x31]) * TACC_IMAGE_BYTES,
    )

    with pytest.raises(ValueError, match="exactly 256 bytes"):
        native._tacc_image_stage_acquire(
            cpu.core_id,
            "load",
            0x400,
            EW_U8,
            False,
            engine_epoch,
            0,
            bytes(TACC_IMAGE_BYTES - 1),
        )
    assert dict(native._tacc_image_stage_snapshot()) == initial
    assert not native.cycle_execution_pending

    with pytest.raises(ValueError, match="stale engine epoch"):
        native._tacc_image_stage_acquire(
            cpu.core_id,
            "load",
            0x400,
            EW_U8,
            False,
            engine_epoch + 1,
            0,
            bytes(TACC_IMAGE_BYTES),
        )
    assert dict(native._tacc_image_stage_snapshot()) == initial
    assert not native.cycle_execution_pending

    accepted, load_stage_epoch = native._tacc_image_stage_acquire(
        cpu.core_id,
        "load",
        0x400,
        EW_U8,
        False,
        engine_epoch,
        0,
        bytes(TACC_IMAGE_BYTES),
    )
    assert accepted
    assert native.cycle_execution_pending

    load_first_image = (
        bytes([0x81]) * 64
        + bytes(TACC_IMAGE_BYTES - 64)
    )
    assert native._tacc_image_stage_update(
        0,
        cpu.core_id,
        load_stage_epoch,
        engine_epoch,
        0,
        1,
        load_first_image,
    )
    after_first = dict(native._tacc_image_stage_snapshot())
    assert after_first["beat_index"] == 1
    assert after_first["image"] == load_first_image

    load_second_image = (
        bytes([0x81]) * 64
        + bytes([0x82]) * 64
        + bytes(TACC_IMAGE_BYTES - 128)
    )
    assert native._tacc_image_stage_update(
        0,
        cpu.core_id,
        load_stage_epoch,
        engine_epoch,
        0,
        2,
        load_second_image,
    )
    after_second = dict(native._tacc_image_stage_snapshot())
    assert after_second["beat_index"] == 2
    assert after_second["image"] == load_second_image

    malformed_active = dict(after_second)
    malformed_active["image"] = malformed_active["image"][:-1]
    with pytest.raises(ValueError, match="exactly 256 bytes"):
        native._tacc_image_stage_restore(malformed_active)
    assert dict(native._tacc_image_stage_snapshot()) == after_second

    rewritten_prefix = dict(after_second)
    rewritten_prefix["beat_index"] = 1
    rewritten_prefix["image"] = (
        bytes([0x91]) * 64
        + bytes(TACC_IMAGE_BYTES - 64)
    )
    with pytest.raises(ValueError, match="acknowledged prefix"):
        native._tacc_image_stage_restore(rewritten_prefix)
    assert dict(native._tacc_image_stage_snapshot()) == after_second

    native._tacc_image_stage_restore(after_first)
    assert dict(native._tacc_image_stage_snapshot()) == after_first
    assert native.cycle_execution_pending
    assert native._tacc_image_stage_release(
        0,
        cpu.core_id,
        load_stage_epoch,
        engine_epoch,
        0,
    )
    assert not native._tacc_image_stage_snapshot()["active"]
    assert not native.cycle_execution_pending

    store_image = bytes([0xA5]) * TACC_IMAGE_BYTES
    engine_epoch = _publish_busy_full_engine(cpu, store_image)
    accepted, store_stage_epoch = native._tacc_image_stage_acquire(
        cpu.core_id,
        "store",
        0x600,
        EW_U8,
        False,
        engine_epoch,
        0,
        store_image,
    )
    assert accepted
    assert native.cycle_execution_pending
    store_initial = dict(native._tacc_image_stage_snapshot())

    with pytest.raises(ValueError, match="exactly 256 bytes"):
        native._tacc_image_stage_update(
            0,
            cpu.core_id,
            store_stage_epoch,
            engine_epoch,
            0,
            1,
            store_image[:-1],
        )
    assert dict(native._tacc_image_stage_snapshot()) == store_initial

    assert not native._tacc_image_stage_update(
        0,
        cpu.core_id,
        store_stage_epoch,
        engine_epoch + 1,
        0,
        1,
        store_image,
    )
    assert dict(native._tacc_image_stage_snapshot()) == store_initial

    for beat_index in range(1, 5):
        assert native._tacc_image_stage_update(
            0,
            cpu.core_id,
            store_stage_epoch,
            engine_epoch,
            0,
            beat_index,
            store_image,
        )
        stage = dict(native._tacc_image_stage_snapshot())
        assert stage["beat_index"] == beat_index
        assert stage["image"] == store_image

    store_complete = dict(native._tacc_image_stage_snapshot())
    store_rollback = dict(store_complete)
    store_rollback["beat_index"] = 2
    native._tacc_image_stage_restore(store_rollback)
    assert dict(native._tacc_image_stage_snapshot()) == store_rollback

    rewritten_store = dict(store_rollback)
    rewritten_store["image"] = bytes([0x5A]) * TACC_IMAGE_BYTES
    with pytest.raises(ValueError, match="cannot rewrite"):
        native._tacc_image_stage_restore(rewritten_store)
    assert dict(native._tacc_image_stage_snapshot()) == store_rollback
    assert native.cycle_execution_pending

    before_reset = dict(native._tacc_image_stage_snapshot())
    assert before_reset["last_grant_engine_id"] == 0
    assert before_reset["grant_sequence"] == 2
    system.boot(entry=0x300)

    reset = dict(native._tacc_image_stage_snapshot())
    assert not reset["active"]
    assert reset["direction"] == "none"
    assert reset["owner_engine_id"] is None
    assert reset["owner_core_id"] is None
    assert reset["stage_epoch"] == before_reset["stage_epoch"] + 1
    assert reset["last_grant_engine_id"] is None
    assert reset["grant_sequence"] == 0
    assert reset["image"] == bytes(TACC_IMAGE_BYTES)
    assert not native.cycle_execution_pending


def test_image_stage_epoch_saturates_instead_of_aliasing_stale_tokens():
    system = _system()
    cpu = system.cpu
    native = system._native_system
    maximum = (1 << 64) - 1
    inactive = dict(native._tacc_image_stage_snapshot())
    inactive["stage_epoch"] = maximum - 1
    native._tacc_image_stage_restore(inactive)

    image = bytes([0xA5]) * TACC_IMAGE_BYTES
    engine_epoch = _publish_busy_full_engine(cpu, image)
    accepted, token = native._tacc_image_stage_acquire(
        cpu.core_id,
        "store",
        0x400,
        EW_U8,
        False,
        engine_epoch,
        0,
        image,
    )
    assert accepted
    assert token == maximum
    native._cancel_tacc_image_stage_for_core(cpu.core_id)

    cancelled = dict(native._tacc_image_stage_snapshot())
    assert not cancelled["active"]
    assert cancelled["stage_epoch"] == maximum
    assert not native.cycle_execution_pending
    with pytest.raises(OverflowError, match="tenure counter overflow"):
        native._tacc_image_stage_acquire(
            cpu.core_id,
            "store",
            0x400,
            EW_U8,
            False,
            engine_epoch,
            0,
            image,
        )

    stale_system = _system()
    stale_cpu = stale_system.cpu
    stale_native = stale_system._native_system
    stale_inactive = dict(stale_native._tacc_image_stage_snapshot())
    stale_inactive["stage_epoch"] = maximum - 1
    stale_native._tacc_image_stage_restore(stale_inactive)
    stale_engine_epoch = _publish_busy_full_engine(stale_cpu, image)
    accepted, stale_token = stale_native._tacc_image_stage_acquire(
        stale_cpu.core_id,
        "store",
        0x400,
        EW_U8,
        False,
        stale_engine_epoch,
        0,
        image,
    )
    assert accepted
    assert stale_token == maximum
    assert stale_native.cycle_execution_pending
    stale_cpu._cs.tacc_reset()
    current_engine_epoch = _publish_busy_full_engine(stale_cpu, image)

    with pytest.raises(OverflowError, match="tenure counter overflow"):
        stale_native._tacc_image_stage_acquire(
            stale_cpu.core_id,
            "store",
            0x400,
            EW_U8,
            False,
            current_engine_epoch,
            0,
            image,
        )

    stale_replacement = dict(stale_native._tacc_image_stage_snapshot())
    assert not stale_replacement["active"]
    assert stale_replacement["stage_epoch"] == maximum
    assert not stale_native.cycle_execution_pending
