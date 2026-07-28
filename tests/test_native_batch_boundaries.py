"""Progress accounting at native batch control-flow boundaries."""

from __future__ import annotations

import pytest

import _mp64_accel
from accel_wrapper import Megapad64 as NativeMegapad64
from asm import assemble
from megapad64 import (
    EW_FP16,
    EW_U8,
    HaltError,
    IVEC_ALIGN_FAULT,
    IVEC_ILLEGAL_OP,
    IVEC_SW_TRAP,
    TACC_CANONICAL_NAN,
    Megapad64 as PythonMegapad64,
    TrapError,
)
from system import MegapadSystem


MMIO_START = 0xFFFF_FF00_0000_0000
MMIO_END = 0xFFFF_FF80_0000_0000
NOP_THEN_DOUBLE_EXT = bytes((0x01, 0xF0, 0xF0))


def _raw_run(cpu: NativeMegapad64, max_steps: int):
    return _mp64_accel.run_steps(
        cpu._cs,
        mmio_read8=cpu._mmio_read8,
        mmio_write8=cpu._mmio_write8,
        on_output=cpu._do_output,
        csr_read_override=None,
        mmio_start=MMIO_START,
        mmio_end=MMIO_END,
        max_steps=max_steps,
    )


def _install_vector(cpu, ivec_id: int, handler: int = 0x300) -> int:
    ivt_base = 0x100
    stack_top = 0xF00
    cpu.ivt_base = ivt_base
    cpu.regs[cpu.spsel] = stack_top
    cpu.mem[ivt_base + ivec_id * 8:ivt_base + ivec_id * 8 + 8] = (
        handler.to_bytes(8, "little")
    )
    return handler


def _reset_visible_state(cpu) -> tuple:
    return (
        tuple(cpu.regs),
        (cpu.psel, cpu.xsel, cpu.spsel),
        (
            cpu.flag_z,
            cpu.flag_c,
            cpu.flag_n,
            cpu.flag_v,
            cpu.flag_p,
            cpu.flag_g,
            cpu.flag_i,
            cpu.flag_s,
        ),
        (cpu.d_reg, cpu.q_out, cpu.t_reg),
        (cpu.sb, cpu.sr, cpu.sc, cpu.sw),
        (
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
        tuple(cpu.acc),
        (
            bytes(cpu.tacc),
            cpu.tacc_owner,
            cpu.tacc_valid,
            cpu.tacc_dirty,
            cpu.tacc_format_ew,
            cpu.tacc_format_signed,
            cpu.tacc_busy,
            cpu.tacc_force_pending,
            cpu.tacc_epoch,
        ),
        (cpu.ivt_base, cpu.ivec_id, cpu.trap_addr, cpu.ef_flags),
        (cpu.halted, cpu.idle, cpu.priv_level, cpu.mpu_base, cpu.mpu_limit),
        (
            cpu.cycle_count,
            cpu.perf_enable,
            cpu.perf_cycles,
            cpu.perf_stalls,
            cpu.perf_tileops,
            cpu.perf_extmem,
        ),
        (cpu.crc_acc, cpu.crc_mode, cpu._ext_modifier),
        bytes(cpu.mem),
    )


def _seed_reset_state(cpu) -> None:
    cpu.load_bytes(0, bytes((0x01, 0x03)))  # NOP; RESET
    for index in range(32):
        cpu.regs[index] = 0x1000 + index
    cpu.pc = 0
    cpu.flags_unpack(0xFF)
    cpu.d_reg = 0xA5
    cpu.q_out = 1
    cpu.t_reg = 0xBEEF
    cpu.sb, cpu.sr, cpu.sc, cpu.sw = 3, 4, 5, 6
    cpu.tmode = 3
    cpu.tctrl = 7
    cpu.tsrc0, cpu.tsrc1, cpu.tdst = 0x200, 0x300, 0x400
    cpu.tstride_r = 11
    cpu.tstride_c = 13
    cpu.ttile_h = 2
    cpu.ttile_w = 7
    cpu.acc = [0x11, 0x22, 0x33, 0x44]
    cpu.ivt_base = 0x500
    cpu.ivec_id = 9
    cpu.trap_addr = 0x777
    cpu.ef_flags = 0xA
    cpu.priv_level = 1
    cpu.mpu_base = 0x100
    cpu.mpu_limit = 0x900
    cpu.cycle_count = 5
    cpu.perf_cycles = 7
    cpu.perf_stalls = 8
    cpu.perf_tileops = 9
    cpu.perf_extmem = 10
    cpu.crc_acc = 0x1234
    cpu.crc_mode = 2
    cpu._ext_modifier = -1


def test_raw_batch_reports_prefix_before_pending_trap():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, NOP_THEN_DOUBLE_EXT)
    cpu.pc = 0

    result = _raw_run(cpu, max_steps=2)

    assert (
        result.steps_executed,
        result.total_cycles,
        result.stop_reason,
        result.trap_id,
    ) == (1, 1, 5, IVEC_ILLEGAL_OP)
    assert cpu.pc == len(NOP_THEN_DOUBLE_EXT)
    assert cpu.cycle_count == 1


def test_structured_batch_reports_native_multicycle_progress():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, assemble("mul r1, r2"))
    cpu.regs[1] = 6
    cpu.regs[2] = 7
    cpu.pc = 0

    stats = cpu.run_steps_stats(max_steps=1)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (1, 4, 0)
    assert cpu.regs[1] == 42
    assert cpu.cycle_count == 4

    compatibility_cpu = NativeMegapad64(mem_size=4096)
    compatibility_cpu.load_bytes(0, assemble("mul r1, r2"))
    compatibility_cpu.regs[1] = 6
    compatibility_cpu.regs[2] = 7
    compatibility_cpu.pc = 0
    assert compatibility_cpu.run_steps(max_steps=1) == (1, 0)


def test_structured_batch_composes_native_prefix_and_python_fallback_cycles():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, assemble("nop\nt.sum"))
    cpu.mem[0x100:0x140] = bytes(range(64))
    cpu.tsrc0 = 0x100
    cpu.pc = 0

    stats = cpu.run_steps_stats(max_steps=2)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (2, 2, 0)
    assert cpu.cycle_count == 2


def test_tacc_batch_crosses_native_lifecycle_and_tamac_without_a_boundary():
    cpu = NativeMegapad64(mem_size=4096)
    source0 = 0x400
    source1 = 0x440
    destination = 0x480
    cpu.mem[source0:source0 + 64] = bytes([2]) * 64
    cpu.mem[source1:source1 + 64] = bytes([3]) * 64
    cpu.mem[destination:destination + 64] = bytes([0xA5]) * 64
    cpu.tmode = EW_U8
    cpu.tctrl = 0x5A
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu.tdst = destination
    cpu.acc = [
        0x1111_1111_1111_1111,
        0x2222_2222_2222_2222,
        0x3333_3333_3333_3333,
        0x4444_4444_4444_4444,
    ]
    program = assemble("nop\nt.acc.try\nt.acc.clear\nt.amac\nnop")
    cpu.load_bytes(0, program)
    cpu.pc = 0
    legacy_before = (
        tuple(cpu.acc),
        cpu.tmode,
        cpu.tctrl,
        cpu.tsrc0,
        cpu.tsrc1,
        cpu.tdst,
        bytes(cpu.mem[destination:destination + 64]),
    )

    def forbid_fallback():
        pytest.fail("legal TACC batch instruction entered Python fallback")

    cpu._step_python_fallback = forbid_fallback

    stats = cpu.run_steps_stats(max_steps=5)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (5, 13, 0)
    assert cpu.pc == len(program)
    assert cpu.cycle_count == 13
    assert cpu.perf_cycles == 13
    assert cpu.perf_tileops == 3
    assert (
        tuple(cpu.acc),
        cpu.tmode,
        cpu.tctrl,
        cpu.tsrc0,
        cpu.tsrc1,
        cpu.tdst,
        bytes(cpu.mem[destination:destination + 64]),
    ) == legacy_before
    assert bytes(cpu.tacc) == (6).to_bytes(4, "little") * 64
    assert (
        cpu.tacc_owner,
        cpu.tacc_valid,
        cpu.tacc_dirty,
        cpu.tacc_format_ew,
        cpu.tacc_format_signed,
        cpu.tacc_busy,
        cpu.tacc_force_pending,
        cpu.tacc_epoch,
    ) == (cpu.core_id, True, True, EW_U8, 0, False, False, 0)


def test_exceptional_fp_tacc_fallback_is_one_explicit_batch_boundary():
    cpu = NativeMegapad64(mem_size=4096)
    source0 = 0x400
    source1 = 0x440
    fp16_nan = (0x7E01).to_bytes(2, "little")
    fp16_one = (0x3C00).to_bytes(2, "little")
    cpu.mem[source0:source0 + 64] = fp16_nan + fp16_one * 31
    cpu.mem[source1:source1 + 64] = fp16_one * 32
    cpu.tmode = EW_FP16
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu._cs.tacc_restore({
        "tacc": bytes(256),
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": False,
        "tacc_format_ew": EW_FP16,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 9,
    })
    program = assemble("nop\nt.amac\nnop")
    cpu.load_bytes(0, program)
    cpu.pc = 0
    original_fallback = cpu._step_python_fallback
    fallback_calls = 0

    def counted_fallback(*args, **kwargs):
        nonlocal fallback_calls
        fallback_calls += 1
        return original_fallback(*args, **kwargs)

    cpu._step_python_fallback = counted_fallback

    first = cpu.run_steps_stats(max_steps=3)

    assert (
        first.steps_executed,
        first.total_cycles,
        first.stop_reason,
    ) == (2, 8, 0)
    assert fallback_calls == 1
    assert cpu.pc == len(assemble("nop\nt.amac"))
    assert cpu.cycle_count == 8
    assert cpu.perf_cycles == 8
    assert cpu.perf_tileops == 1
    assert int.from_bytes(bytes(cpu.tacc[:4]), "little") == TACC_CANONICAL_NAN
    assert cpu.tacc_dirty
    assert not cpu.tacc_busy

    second = cpu.run_steps_stats(max_steps=1)

    assert (
        second.steps_executed,
        second.total_cycles,
        second.stop_reason,
    ) == (1, 1, 0)
    assert fallback_calls == 1
    assert cpu.pc == len(program)
    assert cpu.cycle_count == 9


def test_tacc_batch_preflight_trap_preserves_native_prefix_metadata():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.tmode = EW_U8
    cpu.tsrc0 = 0x201
    cpu._cs.tacc_restore({
        "tacc": bytes([0x5A]) * 256,
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": True,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 11,
    })
    before_tacc = dict(cpu._cs.tacc_snapshot())
    program = assemble("nop\nt.acc.load")
    cpu.load_bytes(0, program)
    cpu.pc = 0
    original_fallback = cpu._step_python_fallback
    fallback_calls = 0

    def counted_fallback(*args, **kwargs):
        nonlocal fallback_calls
        fallback_calls += 1
        return original_fallback(*args, **kwargs)

    cpu._step_python_fallback = counted_fallback

    with pytest.raises(TrapError) as raised:
        cpu.run_steps_stats(max_steps=2)

    error = raised.value
    assert error.ivec_id == IVEC_ALIGN_FAULT
    assert error.steps_executed == 2
    assert error.native_prefix_steps == 1
    assert error.native_prefix_cycles == 1
    assert fallback_calls == 1
    assert cpu.pc == len(program)
    assert cpu.trap_addr == 0x201
    assert cpu.cycle_count == 3
    assert cpu.perf_cycles == 3
    assert cpu.perf_tileops == 0
    assert dict(cpu._cs.tacc_snapshot()) == before_tacc


def test_structured_batch_composes_software_trap_cycles():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, bytes((0x01, 0x0F)))  # NOP; SYS.TRAP
    handler = _install_vector(cpu, IVEC_SW_TRAP)
    cpu.pc = 0

    stats = cpu.run_steps_stats(max_steps=2)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (2, 4, 0)
    assert cpu.pc == handler
    assert cpu.cycle_count == 4


def test_structured_batch_preserves_elapsed_fault_cost_not_counter_delta():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, NOP_THEN_DOUBLE_EXT)
    handler = _install_vector(cpu, IVEC_ILLEGAL_OP)
    cpu.pc = 0

    stats = cpu.run_steps_stats(max_steps=2)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (2, 2, 0)
    assert cpu.pc == handler
    assert cpu.cycle_count == 1


def test_structured_batch_preserves_prefix_cycles_across_reset():
    cpu = NativeMegapad64(mem_size=4096)
    _seed_reset_state(cpu)

    stats = cpu.run_steps_stats(max_steps=2)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (2, 2, 0)
    assert cpu.cycle_count == 7


@pytest.mark.parametrize(
    ("state_name", "stop_reason"),
    (
        pytest.param("halted", 1, id="halted"),
        pytest.param("idle", 2, id="idle"),
    ),
)
def test_structured_batch_reports_zero_progress_when_already_stopped(
    state_name: str,
    stop_reason: int,
) -> None:
    cpu = NativeMegapad64(mem_size=4096)
    setattr(cpu, state_name, True)

    stats = cpu.run_steps_stats(max_steps=100)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (0, 0, stop_reason)


@pytest.mark.parametrize(
    ("instruction", "stop_reason"),
    (
        pytest.param("halt", 1, id="halt"),
        pytest.param("idl", 2, id="idle"),
    ),
)
def test_structured_batch_counts_instruction_before_stopped_boundary(
    instruction: str,
    stop_reason: int,
) -> None:
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, assemble(instruction))
    cpu.pc = 0

    stats = cpu.run_steps_stats(max_steps=2)

    assert (
        stats.steps_executed,
        stats.total_cycles,
        stats.stop_reason,
    ) == (1, 1, stop_reason)

    compatibility_cpu = NativeMegapad64(mem_size=4096)
    compatibility_cpu.load_bytes(0, assemble(instruction))
    compatibility_cpu.pc = 0
    assert compatibility_cpu.run_steps(max_steps=2) == (1, stop_reason)


def test_wrapper_counts_prefix_and_delivers_pending_trap():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, NOP_THEN_DOUBLE_EXT)
    handler = _install_vector(cpu, IVEC_ILLEGAL_OP)
    stack_top = cpu.regs[cpu.spsel]
    cpu.pc = 0

    assert cpu.run_steps(max_steps=2) == (2, 0)
    assert cpu.pc == handler
    assert cpu.ivec_id == IVEC_ILLEGAL_OP
    assert cpu.regs[cpu.spsel] == stack_top - 16
    assert cpu.cycle_count == 1
    assert cpu.perf_cycles == 1


def test_no_ivt_trap_carries_native_prefix_progress():
    cpu = NativeMegapad64(mem_size=4096)
    cpu.load_bytes(0, NOP_THEN_DOUBLE_EXT)
    cpu.pc = 0

    with pytest.raises(TrapError) as raised:
        cpu.run_steps(max_steps=2)

    error = raised.value
    assert error.ivec_id == IVEC_ILLEGAL_OP
    assert error.steps_executed == 2
    assert error.native_prefix_steps == 1
    assert error.native_prefix_cycles == 1
    assert cpu.pc == len(NOP_THEN_DOUBLE_EXT)


def test_system_clock_advances_only_completed_prefix_when_batch_trap_has_no_ivt():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, NOP_THEN_DOUBLE_EXT)
    system.cpu.pc = 0
    system.timer.control = 1

    assert system.run_batch(2) == 2
    assert system._native_system.system_cycles == 1
    assert system.timer.counter == 1
    assert system.cpu.pc == len(NOP_THEN_DOUBLE_EXT)


def test_step_settles_prior_core_time_before_later_unhandled_trap():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    trap_address = 0x100
    system.load_binary(0, assemble("nop"))
    system.load_binary(trap_address, bytes((0xF0, 0xF0)))
    system.cores[1].pc = trap_address
    system.timer.control = 1

    with pytest.raises(TrapError) as raised:
        system.step()

    assert raised.value.ivec_id == IVEC_ILLEGAL_OP
    assert system.cores[0].pc == 1
    assert system._native_system.system_cycles == 1
    assert system.timer.counter == 1


def test_system_batch_never_fabricates_progress_for_halt_exception():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )

    def raise_halt(_max_steps):
        raise HaltError("no structured progress")

    system.cpu.run_steps_stats = raise_halt

    with pytest.raises(HaltError, match="no structured progress"):
        system.run_batch_stats(1)

    assert system._native_system.system_cycles == 0


def test_multicore_batch_settles_prior_progress_before_halt_exception():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("mul r1, r2\nhalt"))
    system.timer.control = 1

    def raise_halt(_max_steps):
        raise HaltError("no structured progress")

    system.cores[1].run_steps_stats = raise_halt

    with pytest.raises(HaltError, match="no structured progress"):
        system.run_batch_stats(1_000)

    assert system.cores[0].cycle_count == 5
    assert system._native_system.system_cycles == 5
    assert system.timer.counter == 5
    assert system._scheduler_cursor == 1


@pytest.mark.parametrize(
    "method_name",
    (
        "run_steps_stats",
        "_run_steps_stats_in_memory_scope",
    ),
)
def test_system_batch_preserves_class_level_core_batch_overrides(
    monkeypatch,
    method_name: str,
) -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("nop"))
    system.cpu.pc = 0
    cpu_type = type(system.cpu)
    original = getattr(cpu_type, method_name)
    calls = []

    def counted(self, max_steps):
        calls.append(max_steps)
        return original(self, max_steps)

    monkeypatch.setattr(cpu_type, method_name, counted)

    stats = system.run_batch_stats(1)

    assert not stats.native_scheduler
    assert stats.instructions_executed == 1
    assert calls == [1]


def test_native_system_loop_settles_complete_frontier_before_callback_error():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("mul r1, r2\nhalt"))
    system.load_binary(0x40, assemble("out1"))
    system.boot(entry=0)
    system.cores[1].pc = 0x40
    system.cores[1].regs[system.cores[1].xsel] = 0x80
    system.cores[1].mem[0x80] = 0xA5
    system.timer.control = 1
    failure = RuntimeError("later native callback failed")

    def raise_callback_error(_port, _value):
        raise failure

    system.cores[1].on_output = raise_callback_error
    native_runs_before = system._native_system.native_batch_runs

    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(1_000)

    assert raised.value is failure
    assert system._native_system.native_batch_runs == native_runs_before + 1
    # P3-D1 gathers the complete private frontier, then commits one pending
    # coordinator instruction per core in cyclic order. Core 0's cold MUL
    # commits before core 1's failing callback, but its later HALT is beyond
    # that frontier and is not speculatively executed.
    assert system.cores[0].cycle_count == 4
    assert not system.cores[0].halted
    assert system._native_system.system_cycles == 4
    assert system.timer.counter == 4
    assert system._scheduler_cursor == 1


def test_system_batch_finishes_its_budget_after_a_python_fallback_boundary():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("t.sum\nnop"))
    system.cpu.mem[0x100:0x140] = bytes(range(64))
    system.cpu.tsrc0 = 0x100
    system.cpu.pc = 0

    stats = system.run_batch_stats(2)

    assert stats.instructions_executed == 2
    assert stats.system_cycles_advanced == 2
    assert stats.per_core_instructions == (2,)
    assert stats.per_core_cycles == (2,)
    assert system._native_system.system_cycles == 2


def test_reset_after_native_prefix_matches_python_state_and_count():
    oracle = PythonMegapad64(mem_size=4096)
    native = NativeMegapad64(mem_size=4096)
    _seed_reset_state(oracle)
    _seed_reset_state(native)

    assert oracle.step() == 1
    assert oracle.step() == 1
    assert native.run_steps(max_steps=2) == (2, 0)
    assert _reset_visible_state(native) == _reset_visible_state(oracle)


def test_software_trap_cycle_and_state_match_python_oracle():
    oracle = PythonMegapad64(mem_size=4096)
    native = NativeMegapad64(mem_size=4096)
    for cpu in (oracle, native):
        cpu.load_bytes(0, bytes((0x0F,)))  # SYS.TRAP
        _install_vector(cpu, IVEC_SW_TRAP)
        cpu.pc = 0

    assert oracle.step() == 3
    assert native.step() == 3
    assert _reset_visible_state(native) == _reset_visible_state(oracle)


def test_software_trap_after_batch_prefix_matches_python_oracle():
    oracle = PythonMegapad64(mem_size=4096)
    native = NativeMegapad64(mem_size=4096)
    for cpu in (oracle, native):
        cpu.load_bytes(0, bytes((0x01, 0x0F)))  # NOP; SYS.TRAP
        _install_vector(cpu, IVEC_SW_TRAP)
        cpu.pc = 0

    assert oracle.step() == 1
    assert oracle.step() == 3
    assert native.run_steps(max_steps=2) == (2, 0)
    assert _reset_visible_state(native) == _reset_visible_state(oracle)
