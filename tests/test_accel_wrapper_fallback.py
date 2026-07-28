"""State synchronization around accelerated-to-Python instruction fallback."""

from __future__ import annotations

import pytest

import megapad64 as python_oracle
from accel_wrapper import Megapad64
from asm import assemble
from megapad64 import (
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    EW_BF16,
    EW_U8,
    IVEC_BUS_FAULT,
    TACC_OWNER_NONE,
    TrapError,
)
from system import MegapadSystem


@pytest.mark.parametrize(
    ("initial_size", "replacement_size", "source_addr", "written", "expected"),
    (
        pytest.param(128, 1024, 512, 64, 64, id="grow"),
        pytest.param(1024, 128, 96, 32, 0, id="shrink"),
    ),
)
def test_cached_python_fallback_tracks_replacement_memory_geometry(
    initial_size: int,
    replacement_size: int,
    source_addr: int,
    written: int,
    expected: int,
) -> None:
    cpu = Megapad64(mem_size=initial_size)
    cached_fallback = cpu._get_fallback()

    replacement = bytearray(replacement_size)
    replacement[source_addr:source_addr + written] = bytes([1]) * written
    cpu.mem = replacement

    cpu.tmode = EW_U8
    cpu.tctrl = 0
    cpu.tsrc0 = source_addr
    cpu.load_bytes(0, assemble("t.sum"))
    cpu.pc = 0

    cpu.step()

    assert cpu.acc[0] == expected
    assert cpu._py_fallback is cached_fallback
    assert cached_fallback.mem is replacement
    assert cached_fallback.mem_size == replacement_size


def _bf16_overflow_reduction_cpu(
    instruction_name: str = "t.sum",
) -> tuple[Megapad64, bytes]:
    cpu = Megapad64(mem_size=1024)
    src0 = 0x100
    max_finite_bf16 = (0x7F7F).to_bytes(2, "little")

    cpu.mem[src0:src0 + 64] = max_finite_bf16 * 32
    instruction = assemble(instruction_name)
    cpu.load_bytes(0, instruction)
    cpu.tmode = EW_BF16
    cpu.tctrl = 0x2
    cpu.tsrc0 = src0
    cpu.acc = [
        0x1111_1111_1111_1111,
        0x2222_2222_2222_2222,
        0x3333_3333_3333_3333,
        0x4444_4444_4444_4444,
    ]
    cpu.pc = 0
    return cpu, instruction


def test_exceptional_python_fallback_synchronizes_faulting_state_back(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    cpu, instruction = _bf16_overflow_reduction_cpu()
    injected_error = ValueError("injected FP32 conversion failure")

    def fail_conversion(_value: float) -> int:
        raise injected_error

    monkeypatch.setattr(python_oracle, "_fp32_to_bits", fail_conversion)

    with pytest.raises(ValueError) as raised:
        cpu.step()

    assert raised.value is injected_error
    fallback = cpu._get_fallback()
    assert cpu.pc == len(instruction)
    assert cpu.pc == fallback.pc
    assert cpu.cycle_count == fallback.cycle_count == 0
    assert list(cpu.acc) == list(fallback.acc) == [0, 0, 0, 0]
    assert cpu.tctrl == fallback.tctrl == 0


@pytest.mark.parametrize(
    ("instruction_name", "batched"),
    [
        pytest.param("t.sum", False, id="sum-step"),
        pytest.param("t.sum", True, id="sum-run-steps"),
        pytest.param("t.sumsq", False, id="sumsq-step"),
        pytest.param("t.sumsq", True, id="sumsq-run-steps"),
    ],
)
def test_bf16_overflow_fallback_synchronizes_infinity_result(
    instruction_name: str,
    batched: bool,
) -> None:
    cpu, instruction = _bf16_overflow_reduction_cpu(instruction_name)

    if batched:
        assert cpu.run_steps(max_steps=1) == (1, 0)
    else:
        assert cpu.step() == 1

    fallback = cpu._get_fallback()
    assert cpu.pc == fallback.pc == len(instruction)
    assert cpu.cycle_count == fallback.cycle_count == 1
    assert list(cpu.acc) == list(fallback.acc) == [0x7F80_0000, 0, 0, 0]
    assert cpu.tctrl == fallback.tctrl == 0


def test_fallback_callback_runtime_error_is_not_native_control_flow():
    cpu = Megapad64(mem_size=1024)
    source = 0x100
    cpu.mem[source:source + 64] = bytes([1]) * 64
    instruction = assemble("t.load2d")
    cpu.load_bytes(0, instruction)
    cpu.tmode = EW_U8
    cpu.sb = 0
    cpu.sr = source // 64
    cpu.sc = 0
    cpu.sw = 1
    cpu.tstride_r = 1
    cpu.ttile_h = 1
    cpu.ttile_w = 1
    cpu.tdst = 0x200
    cpu.pc = 0
    callback_error = RuntimeError("HALT")
    original_read = cpu.mem_read8

    def fail_read(address):
        if source <= address < source + 64:
            raise callback_error
        return original_read(address)

    cpu.mem_read8 = fail_read

    with pytest.raises(RuntimeError) as raised:
        cpu.run_steps(max_steps=1)

    assert raised.value is callback_error
    assert cpu.pc == len(instruction)


@pytest.mark.parametrize(
    "message",
    [
        pytest.param("HALT", id="halt-control-string"),
        pytest.param("TRAP:BUS_FAULT", id="trap-control-string"),
    ],
)
@pytest.mark.parametrize(
    "execution",
    [
        pytest.param("step", id="direct-step"),
        pytest.param("core-batch", id="core-batch"),
        pytest.param("system-batch", id="system-batch"),
    ],
)
def test_native_tacc_callback_control_strings_propagate_by_identity(
    message: str,
    execution: str,
) -> None:
    system = None
    if execution == "system-batch":
        system = MegapadSystem(
            ram_size=4096,
            num_cores=1,
            num_clusters=0,
            hbw_size=0,
            ext_mem_size=0,
            vram_size=0,
            worker_count=1,
        )
        cpu = system.cpu
    else:
        cpu = Megapad64(mem_size=4096)

    source0 = 0x400
    source1 = 0x440
    cpu.mem[source0:source0 + 64] = bytes([2]) * 64
    cpu.mem[source1:source1 + 64] = bytes([3]) * 64
    cpu.tmode = EW_U8
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu._cs.tacc_restore({
        "tacc": bytes(256),
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": False,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 7,
    })
    instruction = assemble("t.amac")
    if system is None:
        cpu.load_bytes(0, instruction)
    else:
        system.load_binary(0, instruction)
    cpu.pc = 0
    before_tacc = dict(cpu._cs.tacc_snapshot())
    cycles_before = cpu.cycle_count
    perf_cycles_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    original_read8 = cpu.mem_read8
    injected = RuntimeError(message)
    callback_count = 0

    def fail_first_source_read(address: int) -> int:
        nonlocal callback_count
        if address == source0:
            callback_count += 1
            assert cpu.tacc_busy
            raise injected
        return original_read8(address)

    cpu.mem_read8 = fail_first_source_read

    with pytest.raises(RuntimeError) as raised:
        if execution == "step":
            cpu.step()
        elif execution == "core-batch":
            cpu.run_steps_stats(max_steps=1)
        else:
            assert system is not None
            system.run_batch_stats(1)

    assert raised.value is injected
    assert callback_count == 1
    assert cpu.pc == len(instruction)
    assert cpu.cycle_count == cycles_before
    assert cpu.perf_cycles == perf_cycles_before
    assert cpu.perf_tileops == tileops_before
    assert dict(cpu._cs.tacc_snapshot()) == before_tacc
    assert not cpu.tacc_busy


@pytest.mark.parametrize(
    "execution",
    [
        pytest.param("step", id="direct-step"),
        pytest.param("core-batch", id="core-batch"),
    ],
)
def test_tacc_honors_class_level_memory_route_overrides(
    execution: str,
) -> None:
    class RoutedMegapad64(Megapad64):
        def mem_read8(self, address: int) -> int:
            if address == self._fault_address:
                self._callback_count += 1
                assert self.tacc_busy
                raise self._injected
            return super().mem_read8(address)

    cpu = RoutedMegapad64(mem_size=4096)
    source0 = 0x400
    source1 = 0x440
    cpu.mem[source0:source0 + 64] = bytes([2]) * 64
    cpu.mem[source1:source1 + 64] = bytes([3]) * 64
    cpu.tmode = EW_U8
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu._cs.tacc_restore({
        "tacc": bytes(256),
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": False,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 13,
    })
    instruction = assemble("t.amac")
    cpu.load_bytes(0, instruction)
    cpu.pc = 0
    cpu._fault_address = source0
    cpu._callback_count = 0
    cpu._injected = ValueError("class-level TACC route")
    before_tacc = dict(cpu._cs.tacc_snapshot())

    with pytest.raises(ValueError) as raised:
        if execution == "step":
            cpu.step()
        else:
            cpu.run_steps_stats(max_steps=1)

    assert raised.value is cpu._injected
    assert cpu._callback_count == 1
    assert cpu.pc == len(instruction)
    assert cpu.cycle_count == 0
    assert cpu.perf_cycles == 0
    assert cpu.perf_tileops == 0
    assert dict(cpu._cs.tacc_snapshot()) == before_tacc
    assert not cpu.tacc_busy


def test_system_tacc_callback_trap_reports_exact_fault_cycles() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cpu = system.cpu
    source0 = 0x400
    source1 = 0x440
    cpu.mem[source0:source0 + 64] = bytes([2]) * 64
    cpu.mem[source1:source1 + 64] = bytes([3]) * 64
    cpu.tmode = EW_U8
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu._cs.tacc_restore({
        "tacc": bytes(256),
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": False,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 11,
    })
    system.load_binary(0, assemble("t.amac"))
    cpu.pc = 0
    before_tacc = dict(cpu._cs.tacc_snapshot())
    cycles_before = cpu.cycle_count
    perf_cycles_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops
    original_read8 = cpu.mem_read8

    def fault_first_source_read(address: int) -> int:
        if address == source0:
            assert cpu.tacc_busy
            raise TrapError(IVEC_BUS_FAULT)
        return original_read8(address)

    cpu.mem_read8 = fault_first_source_read

    stats = system.run_batch_stats(1)

    assert stats.instructions_executed == 1
    assert stats.per_core_instructions == (1,)
    assert stats.per_core_cycles == (2,)
    assert stats.system_cycles_advanced == 2
    assert cpu.pc == len(assemble("t.amac"))
    assert cpu.trap_addr == source0
    assert cpu.cycle_count - cycles_before == 2
    assert cpu.perf_cycles - perf_cycles_before == 2
    assert cpu.perf_tileops == tileops_before
    assert dict(cpu._cs.tacc_snapshot()) == before_tacc
    assert not cpu.tacc_busy


@pytest.mark.parametrize("exceptional", (False, True))
def test_fallback_transports_complete_tacc_state_on_every_exit(exceptional):
    cpu = Megapad64(mem_size=1024, core_id=2, num_cores=4)
    cpu._cs.tacc_restore({
        "tacc": bytes([0x11]) * 256,
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": False,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 7,
    })
    fallback = cpu._get_fallback()
    injected = RuntimeError("injected fallback completion")

    def replace_complete_tacc_state():
        fallback.tacc[:] = bytes(range(256))
        fallback.tacc_owner = cpu.core_id
        fallback.tacc_valid = True
        fallback.tacc_dirty = True
        fallback.tacc_format_ew = EW_U8
        fallback.tacc_format_signed = 1
        fallback.tacc_busy = False
        fallback.tacc_force_pending = False
        fallback.tacc_epoch = 8
        if exceptional:
            raise injected
        return 9

    fallback.step = replace_complete_tacc_state
    if exceptional:
        with pytest.raises(RuntimeError) as raised:
            cpu._step_python_fallback()
        assert raised.value is injected
    else:
        assert cpu._step_python_fallback() == 9

    state = dict(cpu._cs.tacc_snapshot())
    assert state == {
        "tacc": bytes(range(256)),
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": True,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 1,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 8,
    }


def test_fallback_epoch_guard_discards_result_after_reset_callback():
    cpu = Megapad64(mem_size=1024)
    cpu._cs.tacc_restore({
        "tacc": bytes([0x44]) * 256,
        "tacc_owner": cpu.core_id,
        "tacc_valid": True,
        "tacc_dirty": True,
        "tacc_format_ew": EW_U8,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": 12,
    })
    fallback = cpu._get_fallback()

    def reset_then_publish_stale_result():
        cpu._cs.tacc_reset()
        fallback.tacc[:] = bytes([0xA5]) * 256
        fallback.tacc_owner = cpu.core_id
        fallback.tacc_valid = True
        fallback.tacc_dirty = True
        fallback.tacc_format_ew = EW_U8
        fallback.tacc_format_signed = 0
        fallback.tacc_epoch = 12
        return 1

    fallback.step = reset_then_publish_stale_result

    assert cpu._step_python_fallback() == 1

    state = dict(cpu._cs.tacc_snapshot())
    assert state["tacc_epoch"] == 13
    assert state["tacc_owner"] == TACC_OWNER_NONE
    assert not state["tacc_valid"]
    assert not state["tacc_dirty"]
    assert not any(state["tacc"])
    assert fallback.tacc_epoch == 13
    assert fallback.tacc_owner == TACC_OWNER_NONE
    assert not any(fallback.tacc)


@pytest.mark.parametrize("fault_at_completion", (False, True))
def test_direct_accelerated_tamac_publishes_busy_before_callbacks(
    fault_at_completion: bool,
) -> None:
    cpu = Megapad64(mem_size=4096)
    cpu.tmode = EW_U8
    for instruction in ("t.acc.try", "t.acc.clear"):
        cpu.load_bytes(0, assemble(instruction))
        cpu._cs.icache_reset()
        cpu.pc = 0
        cpu.step()

    source0 = 0x100
    source1 = 0x140
    cpu.tsrc0 = source0
    cpu.tsrc1 = source1
    cpu.mem[source0:source0 + 64] = bytes([2]) * 64
    cpu.mem[source1:source1 + 64] = bytes([3]) * 64
    cpu.load_bytes(0, assemble("t.amac"))
    cpu._cs.icache_reset()
    cpu.pc = 0
    original_read8 = cpu.mem_read8
    injected = RuntimeError("injected direct TAMAC callback failure")
    callback_count = 0

    def force_during_source_read(address: int) -> int:
        nonlocal callback_count
        if address == source0 and callback_count == 0:
            callback_count += 1
            active = cpu.csr_read(CSR_TACC_STATUS)
            assert active & (1 << 4)
            assert active & (1 << 1)
            assert (active >> 16) & 0x1F == cpu.core_id

            cpu.csr_write(CSR_TACC_CTL, 1)
            pending = cpu.csr_read(CSR_TACC_STATUS)
            assert pending & (1 << 4)
            assert pending & (1 << 9)
            if fault_at_completion:
                raise injected
        return original_read8(address)

    cpu.mem_read8 = force_during_source_read
    if fault_at_completion:
        with pytest.raises(RuntimeError) as raised:
            cpu.step()
        assert raised.value is injected
    else:
        cpu.step()

    assert callback_count == 1
    terminal = cpu.csr_read(CSR_TACC_STATUS)
    assert terminal & 0x3FF == 0
    assert (terminal >> 16) & 0x1F == TACC_OWNER_NONE
    assert not any(cpu.tacc)
