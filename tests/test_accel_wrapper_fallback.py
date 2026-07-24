"""State synchronization around accelerated-to-Python instruction fallback."""

from __future__ import annotations

import pytest

import megapad64 as python_oracle
from accel_wrapper import Megapad64
from asm import assemble
from megapad64 import EW_BF16, EW_U8


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
