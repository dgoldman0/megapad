"""Differential oracles for the native MEX implementation.

The pure-Python CPU is the executable ISA oracle.  Every test runs the same
assembled instruction and initial state through the Python and accelerated
CPUs, then compares the complete MEX-visible result.  The comparison includes
both destination tiles where an operation can widen, all four accumulators,
flags, TCTRL, PC advancement, halt behaviour, and cycle accounting.

These tests intentionally exercise boundary values.  Native safety probes that
could crash the interpreter run in a subprocess so a bad accelerator produces
an ordinary, diagnostic pytest failure rather than taking down the test worker.
"""

from __future__ import annotations

import json
import subprocess
import sys
import textwrap
from collections.abc import Callable
from typing import Any, Literal

import pytest

import _mp64_accel
from accel_wrapper import Megapad64 as NativeMegapad64
from asm import assemble
from megapad64 import (
    CLUSTER_SPAD_ADDR,
    EW_BF16,
    EW_FP16,
    EW_U16,
    EW_U32,
    EW_U64,
    EW_U8,
    HaltError,
    IVEC_ALIGN_FAULT,
    IVEC_BUS_FAULT,
    IVEC_ILLEGAL_OP,
    MASK64,
    TACC_CANONICAL_NAN,
    TACC_IMAGE_BYTES,
    TACC_OWNER_NONE,
    Megapad64 as PythonMegapad64,
    TrapError,
    _float_to_bf16,
    _float_to_fp16,
)


MEM_SIZE = 4096
SRC0 = 0x400
SRC1 = 0x500
DST0 = 0x600
DST1 = DST0 + 64

CPUFactory = type[PythonMegapad64] | type[NativeMegapad64]
Watchers = dict[str, Callable[[], bytes]]
Dispatch = Literal["native", "fallback"]


def _elem_bytes(ew: int) -> int:
    return 2 if ew in (EW_FP16, EW_BF16) else 1 << ew


def _lane_count(ew: int) -> int:
    return 64 // _elem_bytes(ew)


def _integer_tile(ew: int, values: list[int]) -> bytes:
    elem_bytes = _elem_bytes(ew)
    lanes = _lane_count(ew)
    mask = (1 << (8 * elem_bytes)) - 1
    repeated = [values[i % len(values)] & mask for i in range(lanes)]
    return b"".join(value.to_bytes(elem_bytes, "little") for value in repeated)


def _integer_accumulator_image(ew: int, values: list[int]) -> bytes:
    accumulator_bytes = 4 if ew == EW_U8 else 8
    lanes = _lane_count(ew)
    mask = (1 << (8 * accumulator_bytes)) - 1
    active = b"".join(
        (values[lane % len(values)] & mask).to_bytes(
            accumulator_bytes,
            "little",
        )
        for lane in range(lanes)
    )
    return active + bytes(TACC_IMAGE_BYTES - len(active))


def _fp_accumulator_image(values: list[int]) -> bytes:
    active = b"".join(
        (values[lane % len(values)] & 0xFFFF_FFFF).to_bytes(4, "little")
        for lane in range(32)
    )
    return active + bytes(TACC_IMAGE_BYTES - len(active))


def _floating_tile(ew: int, values: list[float]) -> bytes:
    encode = _float_to_fp16 if ew == EW_FP16 else _float_to_bf16
    lanes = _lane_count(ew)
    repeated = [values[i % len(values)] for i in range(lanes)]
    return b"".join(encode(value).to_bytes(2, "little") for value in repeated)


def _floating_tile_exact(ew: int, values: list[float]) -> bytes:
    """Encode explicit lanes and zero-fill every lane the caller omitted."""
    lanes = _lane_count(ew)
    assert len(values) <= lanes
    return _floating_tile(ew, values + [0.0] * (lanes - len(values)))


def _watch_bank(cpu: Any, addr: int, size: int = 64) -> Callable[[], bytes]:
    return lambda: bytes(cpu.mem[(addr + offset) % cpu.mem_size]
                         for offset in range(size))


def _seed_common_state(
    cpu: Any,
    *,
    tmode: int,
    src0: bytes,
    src1: bytes,
    dst0: bytes | None = None,
    dst1: bytes | None = None,
    tctrl: int = 0,
    acc: tuple[int, int, int, int] = (
        0x1111_1111_1111_1111,
        0x2222_2222_2222_2222,
        0x3333_3333_3333_3333,
        0x4444_4444_4444_4444,
    ),
) -> Watchers:
    """Install distinct operands and observable sentinel state."""
    assert len(src0) == len(src1) == 64
    dst0 = bytes([0xA5]) * 64 if dst0 is None else dst0
    dst1 = bytes([0x5A]) * 64 if dst1 is None else dst1
    assert len(dst0) == len(dst1) == 64

    cpu.tmode = tmode
    cpu.tctrl = tctrl
    cpu.tsrc0 = SRC0
    cpu.tsrc1 = SRC1
    cpu.tdst = DST0
    cpu.acc = list(acc)
    cpu.flags_unpack(0b1010_1100)

    cpu.mem[SRC0:SRC0 + 64] = src0
    cpu.mem[SRC1:SRC1 + 64] = src1
    cpu.mem[DST0:DST0 + 64] = dst0
    cpu.mem[DST1:DST1 + 64] = dst1

    return {
        "src0": _watch_bank(cpu, SRC0),
        "src1": _watch_bank(cpu, SRC1),
        "dst0": _watch_bank(cpu, DST0),
        "dst1": _watch_bank(cpu, DST1),
    }


def _restore_tacc_state(
    cpu: Any,
    *,
    image: bytes | bytearray,
    owner: int | None = None,
    valid: bool = True,
    dirty: bool = True,
    ew: int = EW_U8,
    signed: int = 0,
    epoch: int = 17,
) -> None:
    image = bytes(image)
    assert len(image) == TACC_IMAGE_BYTES
    owner = cpu.core_id if owner is None else owner
    state = {
        "tacc": image,
        "tacc_owner": owner,
        "tacc_valid": valid,
        "tacc_dirty": dirty,
        "tacc_format_ew": ew,
        "tacc_format_signed": signed,
        "tacc_busy": False,
        "tacc_force_pending": False,
        "tacc_epoch": epoch,
    }
    if isinstance(cpu, NativeMegapad64):
        cpu._cs.tacc_restore(state)
        return
    cpu.tacc[:] = image
    for name, value in state.items():
        if name != "tacc":
            setattr(cpu, name, value)


def _snapshot(cpu: Any, watchers: Watchers) -> dict[str, Any]:
    """Capture all state and memory that MEX execution may observably change."""
    return {
        "regs": tuple(cpu.regs[index] for index in range(32)),
        "selectors": (cpu.psel, cpu.xsel, cpu.spsel),
        "acc": tuple(cpu.acc),
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
        "flags_packed": cpu.flags_pack(),
        "scalar": (cpu.d_reg, cpu.q_out, cpu.t_reg),
        "cursor": (cpu.sb, cpu.sr, cpu.sc, cpu.sw),
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
        "tacc": (
            bytes(cpu.tacc),
            cpu.tacc_owner,
            bool(cpu.tacc_valid),
            bool(cpu.tacc_dirty),
            cpu.tacc_format_ew,
            cpu.tacc_format_signed,
            bool(cpu.tacc_busy),
            bool(cpu.tacc_force_pending),
            cpu.tacc_epoch,
        ),
        "interrupt": (cpu.ivt_base, cpu.ivec_id, cpu.trap_addr, cpu.ef_flags),
        "pc": cpu.pc,
        "halted": bool(cpu.halted),
        "idle": bool(cpu.idle),
        "cycle_count": cpu.cycle_count,
        "performance": (
            cpu.perf_enable,
            cpu.perf_cycles,
            cpu.perf_stalls,
            cpu.perf_tileops,
            cpu.perf_extmem,
        ),
        "bist": (
            cpu.bist_status,
            cpu.bist_fail_addr,
            cpu.bist_fail_data,
            cpu.tile_selftest,
            cpu.tile_st_detail,
        ),
        "icache": (
            cpu.icache_enabled,
            cpu.icache_hits,
            cpu.icache_misses,
        ),
        "protection": (cpu.priv_level, cpu.mpu_base, cpu.mpu_limit),
        "identity": (cpu.core_id, cpu.num_cores),
        "ext_modifier": cpu._ext_modifier,
        "memory:bank0": bytes(cpu.mem),
        **{f"memory:{name}": read() for name, read in watchers.items()},
    }


def _state_differences(
    oracle: dict[str, Any],
    native: dict[str, Any],
) -> list[str]:
    oracle_flat = _flatten(oracle)
    native_flat = _flatten(native)
    assert oracle_flat.keys() == native_flat.keys()
    return [
        f"{key}: {_format_difference(oracle_flat[key], native_flat[key])}"
        for key in oracle_flat
        if oracle_flat[key] != native_flat[key]
    ]


def _install_dispatch_probe(
    cpu: NativeMegapad64,
    expected_dispatch: Dispatch,
    watchers: Watchers,
) -> list[dict[str, Any]]:
    """Record the exact state handed to Python, or forbid fallback entirely."""
    original_fallback = cpu._step_python_fallback
    pre_fallback: list[dict[str, Any]] = []

    def instrumented_fallback() -> int:
        pre_fallback.append(_snapshot(cpu, watchers))
        if expected_dispatch == "native":
            pytest.fail("native-designated MEX instruction entered Python fallback")
        return original_fallback()

    cpu._step_python_fallback = instrumented_fallback
    return pre_fallback


def _assert_dispatch(
    expected_dispatch: Dispatch,
    before_mex: dict[str, Any],
    pre_fallback: list[dict[str, Any]],
) -> None:
    expected_count = 1 if expected_dispatch == "fallback" else 0
    assert len(pre_fallback) == expected_count, (
        f"expected {expected_dispatch} dispatch, observed "
        f"{len(pre_fallback)} Python fallback call(s)"
    )
    if expected_dispatch == "fallback":
        differences = _state_differences(before_mex, pre_fallback[0])
        assert not differences, (
            "native MEX mutated state before handing the instruction to "
            "Python fallback:\n  " + "\n  ".join(differences)
        )


def _execute_one_then_halt(
    cpu_type: CPUFactory,
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    expected_dispatch: Dispatch | None = None,
) -> dict[str, Any]:
    cpu = cpu_type(mem_size=MEM_SIZE)
    watchers = setup(cpu)
    program = assemble(f"{instruction}\nhalt")
    cpu.load_bytes(0, program)
    cpu.pc = 0

    before_mex = _snapshot(cpu, watchers)
    pre_fallback: list[dict[str, Any]] = []
    if cpu_type is NativeMegapad64:
        assert expected_dispatch is not None
        pre_fallback = _install_dispatch_probe(cpu, expected_dispatch, watchers)

    mex_cycles = cpu.step()
    if cpu_type is NativeMegapad64:
        assert expected_dispatch is not None
        _assert_dispatch(expected_dispatch, before_mex, pre_fallback)
    after_mex = _snapshot(cpu, watchers)

    halt_cycles = cpu.step()
    after_halt = _snapshot(cpu, watchers)

    with pytest.raises(HaltError):
        cpu.step()

    return {
        "instruction_bytes": bytes(program),
        "mex_cycles": mex_cycles,
        "after_mex": after_mex,
        "halt_cycles": halt_cycles,
        "after_halt": after_halt,
        "post_halt_exception": HaltError.__name__,
    }


def _format_difference(oracle: Any, native: Any) -> str:
    if isinstance(oracle, bytes) and isinstance(native, bytes):
        mismatch = next(
            (i for i, (left, right) in enumerate(zip(oracle, native))
             if left != right),
            min(len(oracle), len(native)),
        )
        start = max(0, mismatch - 4)
        end = min(max(len(oracle), len(native)), mismatch + 5)
        return (
            f"first byte mismatch at +{mismatch}: "
            f"oracle[{start}:{end}]={oracle[start:end].hex()} "
            f"native[{start}:{end}]={native[start:end].hex()}"
        )
    return f"oracle={oracle!r}, native={native!r}"


@pytest.mark.parametrize(
    "raw",
    [
        pytest.param("e107", id="reserved-tmul-function"),
        pytest.param("e126", id="noncanonical-tamac"),
        pytest.param("e906", id="illegal-tamac-immediate"),
        pytest.param("f8e307", id="reserved-lifecycle-function"),
        pytest.param("f8e322", id="noncanonical-lifecycle"),
        pytest.param("f8e70200", id="noncanonical-lifecycle-selector"),
    ],
)
def test_invalid_tacc_namespace_reaches_python_before_native_mutation(
    raw: str,
) -> None:
    cpu = NativeMegapad64(mem_size=MEM_SIZE)
    watchers = _seed_common_state(
        cpu,
        tmode=EW_U8,
        src0=bytes((index * 3 + 1) & 0xFF for index in range(64)),
        src1=bytes((index * 5 + 7) & 0xFF for index in range(64)),
    )
    _restore_tacc_state(
        cpu,
        image=bytes((index * 11 + 9) & 0xFF for index in range(256)),
    )
    cpu.load_bytes(0, bytes.fromhex(raw))
    cpu.pc = 0
    before = _snapshot(cpu, watchers)
    pre_fallback: list[dict[str, Any]] = []

    def stop_at_fallback() -> int:
        pre_fallback.append(_snapshot(cpu, watchers))
        return 97

    cpu._step_python_fallback = stop_at_fallback

    assert cpu.step() == 97
    assert pre_fallback == [before]
    assert _snapshot(cpu, watchers) == before


@pytest.mark.parametrize("immediate", [0x07, 0x0E, 0x86])
def test_non_tacc_immediate_tmul_stays_native(immediate: int) -> None:
    cpu = NativeMegapad64(mem_size=MEM_SIZE)
    source = bytes([3]) * 64
    watchers = _seed_common_state(
        cpu,
        tmode=EW_U8,
        src0=source,
        src1=bytes(64),
    )
    _restore_tacc_state(
        cpu,
        image=bytes((index * 17 + 3) & 0xFF for index in range(256)),
        ew=EW_U8,
    )
    cpu.load_bytes(0, bytes((0xE9, immediate)))
    cpu.pc = 0
    before = _snapshot(cpu, watchers)
    pre_fallback = _install_dispatch_probe(
        cpu,
        "native",
        watchers,
    )

    assert cpu.step() == 2
    _assert_dispatch("native", before, pre_fallback)
    assert bytes(cpu.mem[DST0:DST0 + 64]) == bytes(
        [(3 * immediate) & 0xFF]
    ) * 64
    assert _snapshot(cpu, watchers)["tacc"] == before["tacc"]


def _flatten(value: Any, prefix: str = "") -> dict[str, Any]:
    if not isinstance(value, dict):
        return {prefix: value}
    flattened: dict[str, Any] = {}
    for key, child in value.items():
        child_prefix = f"{prefix}.{key}" if prefix else key
        flattened.update(_flatten(child, child_prefix))
    return flattened


def _assert_native_matches_oracle(
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    expected_dispatch: Dispatch,
) -> dict[str, Any]:
    oracle = _execute_one_then_halt(PythonMegapad64, instruction, setup)
    native = _execute_one_then_halt(
        NativeMegapad64,
        instruction,
        setup,
        expected_dispatch=expected_dispatch,
    )
    differences = _state_differences(oracle, native)
    assert not differences, (
        f"native MEX diverged for {instruction!r}:\n  "
        + "\n  ".join(differences)
    )
    return oracle


def _execute_tacc_sequence(
    cpu_type: CPUFactory,
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    repeats: int,
    expected_dispatch: Dispatch | None = None,
) -> dict[str, Any]:
    cpu = cpu_type(mem_size=MEM_SIZE)
    watchers = setup(cpu)
    encoded = bytes(assemble(instruction))
    cpu.load_bytes(0, encoded * repeats)
    cpu.pc = 0
    before = _snapshot(cpu, watchers)
    pre_fallback: list[dict[str, Any]] = []
    if cpu_type is NativeMegapad64 and expected_dispatch is not None:
        pre_fallback = _install_dispatch_probe(
            cpu,
            expected_dispatch,
            watchers,
        )

    cycles = []
    states = []
    for _ in range(repeats):
        before_step = _snapshot(cpu, watchers)
        fallback_count = len(pre_fallback)
        cycles.append(cpu.step())
        if cpu_type is NativeMegapad64 and expected_dispatch is not None:
            _assert_dispatch(
                expected_dispatch,
                before_step,
                pre_fallback[fallback_count:],
            )
        states.append(_snapshot(cpu, watchers))

    return {
        "encoded": encoded,
        "before": before,
        "cycles": tuple(cycles),
        "states": tuple(states),
    }


def _assert_tacc_sequence_matches_oracle(
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    repeats: int = 1,
    expected_dispatch: Dispatch,
) -> dict[str, Any]:
    oracle = _execute_tacc_sequence(
        PythonMegapad64,
        instruction,
        setup,
        repeats=repeats,
    )
    native = _execute_tacc_sequence(
        NativeMegapad64,
        instruction,
        setup,
        repeats=repeats,
        expected_dispatch=expected_dispatch,
    )
    assert native["cycles"] == oracle["cycles"]
    for step_index, (oracle_state, native_state) in enumerate(
        zip(oracle["states"], native["states"], strict=True),
        start=1,
    ):
        differences = _state_differences(oracle_state, native_state)
        assert not differences, (
            f"native TACC diverged for {instruction!r}, step {step_index}:\n  "
            + "\n  ".join(differences)
        )
    return oracle


def _execute_trapping_tacc(
    cpu_type: CPUFactory,
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    expected_dispatch: Dispatch | None = None,
) -> dict[str, Any]:
    cpu = cpu_type(mem_size=MEM_SIZE)
    watchers = setup(cpu)
    encoded = bytes(assemble(instruction))
    cpu.load_bytes(0, encoded)
    cpu.pc = 0
    before = _snapshot(cpu, watchers)
    pre_fallback: list[dict[str, Any]] = []
    if cpu_type is NativeMegapad64 and expected_dispatch is not None:
        pre_fallback = _install_dispatch_probe(
            cpu,
            expected_dispatch,
            watchers,
        )

    with pytest.raises(TrapError) as raised:
        cpu.step()

    if cpu_type is NativeMegapad64 and expected_dispatch is not None:
        _assert_dispatch(expected_dispatch, before, pre_fallback)
    return {
        "encoded": encoded,
        "ivec_id": raised.value.ivec_id,
        "before": before,
        "after": _snapshot(cpu, watchers),
    }


def _assert_trapping_tacc_matches_oracle(
    instruction: str,
    setup: Callable[[Any], Watchers],
    *,
    expected_dispatch: Dispatch | None = None,
) -> dict[str, Any]:
    oracle = _execute_trapping_tacc(
        PythonMegapad64,
        instruction,
        setup,
    )
    native = _execute_trapping_tacc(
        NativeMegapad64,
        instruction,
        setup,
        expected_dispatch=expected_dispatch,
    )
    assert native["ivec_id"] == oracle["ivec_id"]
    differences = _state_differences(oracle["after"], native["after"])
    assert not differences, (
        f"native TACC trap diverged for {instruction!r}:\n  "
        + "\n  ".join(differences)
    )
    return oracle


def _assert_tacc_retirement(
    before: dict[str, Any],
    after: dict[str, Any],
    *,
    instruction_bytes: int,
    cycles: int,
) -> None:
    assert after["pc"] == before["pc"] + instruction_bytes
    assert after["cycle_count"] - before["cycle_count"] == cycles
    before_perf = before["performance"]
    after_perf = after["performance"]
    assert after_perf[0] == before_perf[0]
    assert after_perf[1] - before_perf[1] == cycles
    assert after_perf[2] == before_perf[2]
    assert after_perf[3] == before_perf[3] + 1
    assert after_perf[4] == before_perf[4]


def _assert_tacc_fault_accounting(
    before: dict[str, Any],
    after: dict[str, Any],
    *,
    instruction_bytes: int,
    cycles: int,
) -> None:
    assert after["pc"] == before["pc"] + instruction_bytes
    assert after["cycle_count"] - before["cycle_count"] == cycles
    before_perf = before["performance"]
    after_perf = after["performance"]
    assert after_perf[0] == before_perf[0]
    assert after_perf[1] - before_perf[1] == cycles
    assert after_perf[2:] == before_perf[2:]


def _assert_tacc_legacy_isolation(
    before: dict[str, Any],
    after: dict[str, Any],
    *,
    allow_destination_write: bool = False,
    allow_trap_state: bool = False,
) -> None:
    psel = before["selectors"][0]
    assert tuple(
        value for index, value in enumerate(after["regs"]) if index != psel
    ) == tuple(
        value for index, value in enumerate(before["regs"]) if index != psel
    )
    for key in (
        "selectors",
        "acc",
        "flags",
        "flags_packed",
        "scalar",
        "cursor",
        "tile",
        "bist",
        "protection",
        "identity",
    ):
        assert after[key] == before[key], key
    if not allow_trap_state:
        assert after["interrupt"] == before["interrupt"]
    for key in ("memory:src0", "memory:src1"):
        assert after[key] == before[key], key
    if not allow_destination_write:
        for key in ("memory:dst0", "memory:dst1"):
            assert after[key] == before[key], key


@pytest.mark.parametrize(
    ("case", "instruction", "ew", "signed", "expected_cycles"),
    [
        pytest.param(
            "try",
            "t.acc.try",
            EW_U8,
            0,
            2,
            id="try-claims-free-engine",
        ),
        pytest.param(
            "try-owned",
            "t.acc.try",
            EW_U8,
            0,
            2,
            id="try-is-idempotent-for-owner",
        ),
        pytest.param(
            "clear",
            "t.acc.clear",
            EW_U16,
            1,
            2,
            id="clear-latches-signed-u16",
        ),
        pytest.param(
            "load",
            "t.acc.load",
            EW_U32,
            1,
            6,
            id="load-canonicalizes-u32-image",
        ),
        pytest.param(
            "store",
            "t.acc.store",
            EW_U16,
            1,
            6,
            id="store-clears-dirty",
        ),
        pytest.param(
            "release",
            "t.acc.release",
            EW_U8,
            0,
            2,
            id="release-wipes-and-bumps-epoch",
        ),
    ],
)
def test_tacc_lifecycle_and_transfers_dispatch_natively(
    case: str,
    instruction: str,
    ew: int,
    signed: int,
    expected_cycles: int,
) -> None:
    load_image = bytes((index * 5 + 3) & 0xFF for index in range(256))
    stored_image = bytes((index * 7 + 11) & 0xFF for index in range(256))

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=ew | (signed << 4),
            src0=bytes([0x31]) * 64,
            src1=bytes([0x42]) * 64,
        )
        if case == "try":
            _restore_tacc_state(
                cpu,
                image=bytes(TACC_IMAGE_BYTES),
                owner=TACC_OWNER_NONE,
                valid=False,
                dirty=False,
                ew=0,
                signed=0,
            )
        elif case == "load":
            _restore_tacc_state(
                cpu,
                image=bytes([0xA5]) * TACC_IMAGE_BYTES,
                ew=EW_U8,
            )
            cpu.mem[SRC0:SRC0 + TACC_IMAGE_BYTES] = load_image
        elif case == "store":
            _restore_tacc_state(
                cpu,
                image=stored_image,
                ew=ew,
                signed=signed,
            )
        else:
            _restore_tacc_state(
                cpu,
                image=bytes([0xA5]) * TACC_IMAGE_BYTES,
                ew=EW_U8,
            )
        return watchers

    result = _assert_tacc_sequence_matches_oracle(
        instruction,
        setup,
        expected_dispatch="native",
    )
    before = result["before"]
    after = result["states"][0]
    assert result["cycles"] == (expected_cycles,)
    _assert_tacc_retirement(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=expected_cycles,
    )
    _assert_tacc_legacy_isolation(
        before,
        after,
        allow_destination_write=case == "store",
    )

    image, owner, valid, dirty, latched_ew, latched_signed, busy, pending, epoch = (
        after["tacc"]
    )
    assert not busy
    assert not pending
    if case == "try":
        assert owner == 0
        assert not valid
        assert not dirty
        assert image == bytes(TACC_IMAGE_BYTES)
    elif case == "try-owned":
        assert after["tacc"] == before["tacc"]
    elif case == "clear":
        assert (owner, valid, dirty) == (0, True, True)
        assert (latched_ew, latched_signed) == (EW_U16, 1)
        assert image == bytes(TACC_IMAGE_BYTES)
    elif case == "load":
        assert (owner, valid, dirty) == (0, True, False)
        assert (latched_ew, latched_signed) == (EW_U32, 1)
        assert image == load_image[:128] + bytes(128)
    elif case == "store":
        assert (owner, valid, dirty) == (0, True, False)
        assert after["memory:bank0"][DST0:DST0 + 256] == stored_image
    else:
        assert owner == TACC_OWNER_NONE
        assert not valid
        assert not dirty
        assert (latched_ew, latched_signed) == (0, 0)
        assert image == bytes(TACC_IMAGE_BYTES)
        assert epoch == before["tacc"][-1] + 1


@pytest.mark.parametrize(
    (
        "ew",
        "signed",
        "instruction",
        "source_a_values",
        "source_b_values",
        "initial_values",
        "repeats",
        "expected_cycles",
    ),
    [
        pytest.param(
            EW_U8,
            0,
            "t.amac",
            [0xFF, 0x80, 1, 0],
            [0xFF, 2, 0xFF, 0],
            [0xFFFF_FFF0, 0x8000_0000, 0xFFFF_FFFF, 0],
            2,
            7,
            id="u8-unsigned-tile-wrap-repeat",
        ),
        pytest.param(
            EW_U8,
            1,
            "t.amac r7",
            [0x80, 0x7F, 0xFF, 1],
            [0x80],
            [0x8000_0000, 0x7FFF_FFFF, 1, 0xFFFF_FFFF],
            1,
            6,
            id="u8-signed-broadcast-extremes",
        ),
        pytest.param(
            EW_U16,
            0,
            "t.amac r7",
            [0xFFFF, 0x8000, 1, 0],
            [0xFFFF],
            [0xFFFF_FFFF_FFFF_FFF0, 0, 1, MASK64],
            2,
            4,
            id="u16-unsigned-broadcast-wrap-repeat",
        ),
        pytest.param(
            EW_U16,
            1,
            "t.amac",
            [0x8000, 0x7FFF, 0xFFFF, 1],
            [0xFFFF, 2, 3, 0x8000],
            [MASK64, 0x7FFF_FFFF_FFFF_FFFF, 1, 0],
            1,
            5,
            id="u16-signed-tile-extremes",
        ),
        pytest.param(
            EW_U32,
            0,
            "t.amac",
            [0xFFFF_FFFF, 0x8000_0000, 1, 0],
            [0xFFFF_FFFF, 2, 0xFFFF_FFFF, 0],
            [MASK64, 0x8000_0000_0000_0000, 1, 0],
            2,
            4,
            id="u32-unsigned-tile-wrap-repeat",
        ),
        pytest.param(
            EW_U32,
            1,
            "t.amac inplace",
            [0x8000_0000, 0x7FFF_FFFF, 0xFFFF_FFFF, 1],
            [0xFFFF_FFFF, 2, 3, 0x8000_0000],
            [MASK64, 0x7FFF_FFFF_FFFF_FFFF, 1, 0],
            1,
            4,
            id="u32-signed-inplace-extremes",
        ),
    ],
)
def test_integer_tacc_tamac_dispatches_natively_with_exact_widening(
    ew: int,
    signed: int,
    instruction: str,
    source_a_values: list[int],
    source_b_values: list[int],
    initial_values: list[int],
    repeats: int,
    expected_cycles: int,
) -> None:
    source_a = _integer_tile(ew, source_a_values)
    source_b = _integer_tile(ew, source_b_values)
    initial_image = _integer_accumulator_image(ew, initial_values)

    def setup(cpu: Any) -> Watchers:
        if instruction.endswith("inplace"):
            watchers = _seed_common_state(
                cpu,
                tmode=ew | (signed << 4),
                src0=source_b,
                src1=bytes([0xD7]) * 64,
                dst0=source_a,
            )
        else:
            watchers = _seed_common_state(
                cpu,
                tmode=ew | (signed << 4),
                src0=source_a,
                src1=source_b,
            )
        if "r7" in instruction:
            source_mask = (1 << (8 << ew)) - 1
            cpu.regs[7] = (
                0xA5A5_5A5A_0000_0000
                | (source_b_values[0] & source_mask)
            )
        _restore_tacc_state(
            cpu,
            image=initial_image,
            ew=ew,
            signed=signed,
        )
        return watchers

    result = _assert_tacc_sequence_matches_oracle(
        instruction,
        setup,
        repeats=repeats,
        expected_dispatch="native",
    )
    assert result["cycles"] == (expected_cycles,) * repeats
    previous = result["before"]
    for after in result["states"]:
        _assert_tacc_retirement(
            previous,
            after,
            instruction_bytes=len(result["encoded"]),
            cycles=expected_cycles,
        )
        previous = after
    _assert_tacc_legacy_isolation(result["before"], result["states"][-1])

    source_bits = 8 << ew
    source_mask = (1 << source_bits) - 1
    accumulator_bits = 32 if ew == EW_U8 else 64
    accumulator_mask = (1 << accumulator_bits) - 1
    lane_a = source_a_values[0] & source_mask
    lane_b = source_b_values[0] & source_mask
    if signed:
        sign_bit = 1 << (source_bits - 1)
        if lane_a & sign_bit:
            lane_a -= 1 << source_bits
        if lane_b & sign_bit:
            lane_b -= 1 << source_bits
    expected_lane0 = (
        initial_values[0] + repeats * lane_a * lane_b
    ) & accumulator_mask
    lane_bytes = accumulator_bits // 8
    final_image = result["states"][-1]["tacc"][0]
    assert int.from_bytes(final_image[:lane_bytes], "little") == expected_lane0
    if ew == EW_U32:
        assert final_image[128:] == bytes(128)


@pytest.mark.parametrize(
    (
        "ew",
        "instruction",
        "source_a_values",
        "source_b_values",
        "expected_cycles",
        "expected_lane0",
    ),
    [
        pytest.param(
            EW_FP16,
            "t.amac",
            [0x0001, 0x8000, 0x3C00, 0xBC00],
            [0x3C00, 0x4000, 0x3800, 0x0001],
            7,
            None,
            id="fp16-finite-subnormal-signed-zero",
        ),
        pytest.param(
            EW_BF16,
            "t.amac r7",
            [0x0001, 0x8000, 0x3F80, 0xBF80],
            [0x3F00],
            6,
            None,
            id="bf16-finite-subnormal-signed-zero-broadcast",
        ),
        pytest.param(
            EW_FP16,
            "t.amac inplace",
            [0x7E01, 0x3C00, 0x4000, 0x0000],
            [0x3C00, 0x3C00, 0x3C00, 0x3C00],
            7,
            TACC_CANONICAL_NAN,
            id="fp16-nan-inplace",
        ),
        pytest.param(
            EW_BF16,
            "t.amac",
            [0x7F80, 0x3F80, 0x4000, 0x0000],
            [0x0000, 0x3F80, 0x3F80, 0x3F80],
            7,
            TACC_CANONICAL_NAN,
            id="bf16-infinity-times-zero",
        ),
    ],
)
def test_fp_tacc_tamac_retains_bit_exact_transactional_fallback(
    ew: int,
    instruction: str,
    source_a_values: list[int],
    source_b_values: list[int],
    expected_cycles: int,
    expected_lane0: int | None,
) -> None:
    source_a = _integer_tile(ew, source_a_values)
    source_b = _integer_tile(ew, source_b_values)
    initial_image = _fp_accumulator_image(
        [0, 0x8000_0000, 0x3F80_0001, 0xBF80_0000]
    )

    def setup(cpu: Any) -> Watchers:
        if instruction.endswith("inplace"):
            watchers = _seed_common_state(
                cpu,
                tmode=ew,
                src0=source_b,
                src1=bytes([0xD7]) * 64,
                dst0=source_a,
            )
        else:
            watchers = _seed_common_state(
                cpu,
                tmode=ew,
                src0=source_a,
                src1=source_b,
            )
        if "r7" in instruction:
            cpu.regs[7] = 0xA5A5_5A5A_0000_0000 | source_b_values[0]
        _restore_tacc_state(cpu, image=initial_image, ew=ew)
        return watchers

    result = _assert_tacc_sequence_matches_oracle(
        instruction,
        setup,
        expected_dispatch="fallback",
    )
    before = result["before"]
    after = result["states"][0]
    assert result["cycles"] == (expected_cycles,)
    _assert_tacc_retirement(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=expected_cycles,
    )
    _assert_tacc_legacy_isolation(before, after)
    assert after["tacc"][0][128:] == bytes(128)
    if expected_lane0 is not None:
        assert int.from_bytes(after["tacc"][0][:4], "little") == expected_lane0


@pytest.mark.parametrize(
    ("case", "instruction", "expected_ivec", "expected_trap_addr"),
    [
        pytest.param(
            "misaligned-load",
            "t.acc.load",
            IVEC_ALIGN_FAULT,
            SRC0 + 1,
            id="load-alignment-before-read",
        ),
        pytest.param(
            "crossing-store",
            "t.acc.store",
            IVEC_BUS_FAULT,
            MEM_SIZE,
            id="store-complete-span-before-write",
        ),
        pytest.param(
            "second-tamac-source",
            "t.amac",
            IVEC_BUS_FAULT,
            MEM_SIZE,
            id="tamac-all-sources-before-first-read",
        ),
        pytest.param(
            "unowned-tamac",
            "t.amac",
            IVEC_ILLEGAL_OP,
            0xBADC_0DE,
            id="tamac-ownership-before-read",
        ),
        pytest.param(
            "format-mismatch",
            "t.amac",
            IVEC_ILLEGAL_OP,
            0xBADC_0DE,
            id="tamac-format-before-read",
        ),
    ],
)
def test_tacc_preflight_faults_before_any_memory_or_state_mutation(
    case: str,
    instruction: str,
    expected_ivec: int,
    expected_trap_addr: int,
) -> None:
    initial_image = bytes((index * 13 + 5) & 0xFF for index in range(256))

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=bytes([2]) * 64,
            src1=bytes([3]) * 64,
        )
        _restore_tacc_state(cpu, image=initial_image, ew=EW_U8)
        cpu.trap_addr = 0xBADC_0DE
        accesses: list[int] = []
        watchers["callback-count"] = lambda: len(accesses).to_bytes(8, "little")
        if case == "misaligned-load":
            cpu.tsrc0 = SRC0 + 1
            original_read8 = cpu.mem_read8

            def counting_read8(address: int) -> int:
                accesses.append(address)
                return original_read8(address)

            cpu.mem_read8 = counting_read8
        elif case == "crossing-store":
            cpu.tdst = MEM_SIZE - 128
            original_write8 = cpu.mem_write8

            def counting_write8(address: int, value: int) -> None:
                accesses.append(address)
                original_write8(address, value)

            cpu.mem_write8 = counting_write8
        else:
            if case == "unowned-tamac":
                _restore_tacc_state(
                    cpu,
                    image=bytes(TACC_IMAGE_BYTES),
                    owner=TACC_OWNER_NONE,
                    valid=False,
                    dirty=False,
                    ew=0,
                )
            elif case == "format-mismatch":
                cpu.tmode = EW_U16
            elif case != "second-tamac-source":
                raise AssertionError(f"unknown preflight case: {case}")
            cpu.tsrc1 = MEM_SIZE
            original_read8 = cpu.mem_read8

            def counting_read8(address: int) -> int:
                accesses.append(address)
                return original_read8(address)

            cpu.mem_read8 = counting_read8
        return watchers

    result = _assert_trapping_tacc_matches_oracle(
        instruction,
        setup,
        expected_dispatch="fallback",
    )
    before = result["before"]
    after = result["after"]
    assert result["ivec_id"] == expected_ivec
    assert after["interrupt"][2] == expected_trap_addr
    assert after["memory:callback-count"] == bytes(8)
    assert after["tacc"] == before["tacc"]
    assert after["memory:bank0"] == before["memory:bank0"]
    _assert_tacc_fault_accounting(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=2,
    )
    _assert_tacc_legacy_isolation(
        before,
        after,
        allow_trap_state=True,
    )


@pytest.mark.parametrize(
    ("source_address", "expected_ivec"),
    [
        pytest.param(
            SRC1 + 1,
            IVEC_ALIGN_FAULT,
            id="misaligned-second-source",
        ),
        pytest.param(
            CLUSTER_SPAD_ADDR,
            IVEC_BUS_FAULT,
            id="cluster-scratchpad-source",
        ),
    ],
)
def test_native_tamac_source_preflight_delegates_without_access(
    source_address: int,
    expected_ivec: int,
) -> None:
    initial_image = bytes((index * 13 + 5) & 0xFF for index in range(256))

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=bytes([2]) * 64,
            src1=bytes([3]) * 64,
        )
        _restore_tacc_state(cpu, image=initial_image, ew=EW_U8)
        cpu.tsrc1 = source_address
        return watchers

    result = _assert_trapping_tacc_matches_oracle(
        "t.amac",
        setup,
        expected_dispatch="fallback",
    )
    before = result["before"]
    after = result["after"]
    assert result["ivec_id"] == expected_ivec
    assert after["interrupt"][2] == source_address
    assert after["tacc"] == before["tacc"]
    assert after["memory:bank0"] == before["memory:bank0"]
    _assert_tacc_fault_accounting(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=2,
    )
    _assert_tacc_legacy_isolation(
        before,
        after,
        allow_trap_state=True,
    )


def test_tacc_load_second_beat_fault_is_atomic_with_exact_metadata() -> None:
    initial_image = bytes([0x5A]) * TACC_IMAGE_BYTES

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=bytes([0xC3]) * 64,
            src1=bytes([0xD4]) * 64,
        )
        cpu.mem[SRC0:SRC0 + TACC_IMAGE_BYTES] = bytes([0xC3]) * 256
        _restore_tacc_state(cpu, image=initial_image, ew=EW_U8)
        original_read8 = cpu.mem_read8
        reads: list[int] = []

        def faulting_read8(address: int) -> int:
            reads.append(address)
            if address == SRC0 + 64:
                assert cpu.tacc_busy
                cpu.trap_addr = address
                raise TrapError(IVEC_BUS_FAULT)
            return original_read8(address)

        cpu.mem_read8 = faulting_read8
        watchers["callback-count"] = lambda: len(reads).to_bytes(8, "little")
        return watchers

    result = _assert_trapping_tacc_matches_oracle(
        "t.acc.load",
        setup,
        expected_dispatch="fallback",
    )
    before = result["before"]
    after = result["after"]
    assert result["ivec_id"] == IVEC_BUS_FAULT
    assert after["interrupt"][2] == SRC0 + 64
    assert int.from_bytes(after["memory:callback-count"], "little") == 65
    assert after["tacc"] == before["tacc"]
    assert after["memory:bank0"] == before["memory:bank0"]
    _assert_tacc_fault_accounting(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=4,
    )
    _assert_tacc_legacy_isolation(
        before,
        after,
        allow_trap_state=True,
    )


@pytest.mark.parametrize("dirty", [False, True], ids=["clean", "dirty"])
def test_tacc_store_second_beat_fault_preserves_acknowledged_prefix_and_state(
    dirty: bool,
) -> None:
    initial_image = bytes(range(TACC_IMAGE_BYTES))
    destination_before = bytes([0xCC]) * TACC_IMAGE_BYTES

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=bytes([0x31]) * 64,
            src1=bytes([0x42]) * 64,
        )
        cpu.mem[DST0:DST0 + TACC_IMAGE_BYTES] = destination_before
        _restore_tacc_state(
            cpu,
            image=initial_image,
            dirty=dirty,
            ew=EW_U8,
        )
        original_write8 = cpu.mem_write8
        writes: list[int] = []

        def faulting_write8(address: int, value: int) -> None:
            writes.append(address)
            if address == DST0 + 73:
                assert cpu.tacc_busy
                cpu.trap_addr = address
                raise TrapError(IVEC_BUS_FAULT)
            original_write8(address, value)

        cpu.mem_write8 = faulting_write8
        watchers["callback-count"] = lambda: len(writes).to_bytes(8, "little")
        return watchers

    result = _assert_trapping_tacc_matches_oracle(
        "t.acc.store",
        setup,
        expected_dispatch="fallback",
    )
    before = result["before"]
    after = result["after"]
    assert result["ivec_id"] == IVEC_BUS_FAULT
    assert after["interrupt"][2] == DST0 + 64
    assert int.from_bytes(after["memory:callback-count"], "little") == 74
    assert after["tacc"] == before["tacc"]
    expected_destination = (
        initial_image[:64] + destination_before[64:]
    )
    assert (
        after["memory:bank0"][DST0:DST0 + TACC_IMAGE_BYTES]
        == expected_destination
    )
    _assert_tacc_fault_accounting(
        before,
        after,
        instruction_bytes=len(result["encoded"]),
        cycles=4,
    )
    _assert_tacc_legacy_isolation(
        before,
        after,
        allow_destination_write=True,
        allow_trap_state=True,
    )


@pytest.mark.parametrize(
    ("region", "attach_name", "base"),
    [
        pytest.param("hbw", "attach_hbw", 0x1_0000, id="hbw"),
        pytest.param("ext", "attach_ext_mem", 0x2_0000, id="ext"),
        pytest.param("vram", "attach_vram", 0x3_0000, id="vram"),
    ],
)
@pytest.mark.parametrize(
    "operation",
    [
        pytest.param("t.amac", id="tamac-read"),
        pytest.param("t.acc.load", id="image-load"),
        pytest.param("t.acc.store", id="image-store"),
    ],
)
def test_native_tacc_uses_preflight_resolved_attached_memory(
    region: str,
    attach_name: str,
    base: int,
    operation: str,
) -> None:
    source_a = _integer_tile(EW_U16, [1, 2, 0xFFFF, 0x8000])
    source_b = _integer_tile(EW_U16, [3, 4, 2, 2])
    load_image = bytes((index * 9 + 5) & 0xFF for index in range(256))
    stored_image = bytes((index * 7 + 11) & 0xFF for index in range(256))

    def setup(cpu: Any) -> Watchers:
        aperture = bytearray([0xA5] * TACC_IMAGE_BYTES)
        getattr(cpu, attach_name)(
            aperture,
            base,
            len(aperture),
        )
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U16,
            src0=bytes([0x31]) * 64,
            src1=bytes([0x42]) * 64,
        )
        watchers[region] = lambda: bytes(aperture)
        if operation == "t.amac":
            aperture[:64] = source_a
            aperture[64:128] = source_b
            cpu.tsrc0 = base
            cpu.tsrc1 = base + 64
            _restore_tacc_state(
                cpu,
                image=bytes(TACC_IMAGE_BYTES),
                ew=EW_U16,
            )
        elif operation == "t.acc.load":
            aperture[:] = load_image
            cpu.tsrc0 = base
            _restore_tacc_state(
                cpu,
                image=bytes([0xCC]) * TACC_IMAGE_BYTES,
                ew=EW_U8,
            )
        else:
            cpu.tdst = base
            _restore_tacc_state(
                cpu,
                image=stored_image,
                ew=EW_U16,
            )
        return watchers

    _assert_tacc_sequence_matches_oracle(
        operation,
        setup,
        expected_dispatch="native",
    )


@pytest.mark.parametrize(
    ("ew", "tile_a", "tile_b"),
    [
        pytest.param(
            EW_U8,
            _integer_tile(EW_U8, [0, 1, 17, 250]),
            _integer_tile(EW_U8, [3, 5, 9, 11]),
            id="u8-64-lanes",
        ),
        pytest.param(
            EW_U16,
            _integer_tile(EW_U16, [0, 1, 0x1234, 0xFF00]),
            _integer_tile(EW_U16, [7, 11, 0x101, 0xF0]),
            id="u16-32-lanes",
        ),
        pytest.param(
            EW_U32,
            _integer_tile(EW_U32, [0, 1, 0x1234_5678, 0xFFFF_0000]),
            _integer_tile(EW_U32, [13, 17, 0x101, 0xF0]),
            id="u32-16-lanes",
        ),
        pytest.param(
            EW_U64,
            _integer_tile(
                EW_U64,
                [0, 1, 0x1234_5678_9ABC_DEF0, 0xFFFF_FFFF_FFFF_0000],
            ),
            _integer_tile(EW_U64, [19, 23, 0x101, 0xF0]),
            id="u64-8-lanes",
        ),
        pytest.param(
            EW_FP16,
            _floating_tile(EW_FP16, [-8.0, -0.5, 1.0, 31.0]),
            _floating_tile(EW_FP16, [0.5, 2.0, -3.0, 1.0]),
            id="fp16-full-32-lanes",
        ),
        pytest.param(
            EW_BF16,
            _floating_tile(EW_BF16, [-8.0, -0.5, 1.0, 31.0]),
            _floating_tile(EW_BF16, [0.5, 2.0, -3.0, 1.0]),
            id="bf16-full-32-lanes",
        ),
    ],
)
def test_talu_add_covers_every_legal_element_width(
    ew: int,
    tile_a: bytes,
    tile_b: bytes,
) -> None:
    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(cpu, tmode=ew, src0=tile_a, src1=tile_b)

    result = _assert_native_matches_oracle(
        "t.add", setup, expected_dispatch="native"
    )
    assert len(result["after_mex"]["memory:dst0"]) == 64


@pytest.mark.parametrize(
    ("case_id", "instruction", "tmode", "tile_a", "tile_b"),
    [
        pytest.param(
            "unsigned-saturating-add",
            "t.add",
            EW_U64 | 0x20,
            _integer_tile(EW_U64, [MASK64, MASK64 - 1, 10, 0]),
            _integer_tile(EW_U64, [1, 8, 20, MASK64]),
            id="unsigned-saturating-add",
        ),
        pytest.param(
            "unsigned-saturating-sub",
            "t.sub",
            EW_U64 | 0x20,
            _integer_tile(EW_U64, [MASK64, 1, 0, 20]),
            _integer_tile(EW_U64, [1, 2, 1, 10]),
            id="unsigned-saturating-sub",
        ),
        pytest.param(
            "signed-saturating-add",
            "t.add",
            EW_U64 | 0x10 | 0x20,
            _integer_tile(EW_U64, [(1 << 63) - 1, 1 << 63, -7, 7]),
            _integer_tile(EW_U64, [1, -1, -9, 9]),
            id="signed-saturating-add",
        ),
        pytest.param(
            "signed-saturating-sub",
            "t.sub",
            EW_U64 | 0x10 | 0x20,
            _integer_tile(EW_U64, [1 << 63, (1 << 63) - 1, -7, 7]),
            _integer_tile(EW_U64, [1, -1, 9, -9]),
            id="signed-saturating-sub",
        ),
        pytest.param(
            "signed-abs-including-int64-min",
            "t.abs",
            EW_U64 | 0x10,
            _integer_tile(EW_U64, [1 << 63, -9, -1, 7]),
            _integer_tile(EW_U64, [0]),
            id="signed-abs-including-int64-min",
        ),
        pytest.param(
            "unsigned-rounded-right-shift",
            "t.vshr",
            EW_U64 | 0x40,
            _integer_tile(EW_U64, [MASK64, 7, 8, 9]),
            _integer_tile(EW_U64, [1, 1, 2, 3]),
            id="unsigned-rounded-right-shift",
        ),
        pytest.param(
            "signed-rounded-right-shift",
            "t.vshr",
            EW_U64 | 0x10 | 0x40,
            _integer_tile(
                EW_U64,
                [(1 << 63) - 1, -17, -16, 8],
            ),
            _integer_tile(EW_U64, [1, 2, 2, 3]),
            id="signed-rounded-right-shift",
        ),
        pytest.param(
            "unsigned-wrapping-multiply",
            "t.mul",
            EW_U64,
            _integer_tile(EW_U64, [MASK64, 1 << 63, 7, 11]),
            _integer_tile(EW_U64, [2, 3, 5, 13]),
            id="unsigned-wrapping-multiply",
        ),
        pytest.param(
            "signed-wrapping-multiply",
            "t.mul",
            EW_U64 | 0x10,
            _integer_tile(
                EW_U64,
                [-(1 << 63), (1 << 63) - 1, -3, 11],
            ),
            _integer_tile(EW_U64, [-1, 2, 7, 13]),
            id="signed-wrapping-multiply",
        ),
    ],
)
def test_ew_u64_mode_boundaries(
    case_id: str,
    instruction: str,
    tmode: int,
    tile_a: bytes,
    tile_b: bytes,
) -> None:
    del case_id

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=tmode,
            src0=tile_a,
            src1=tile_b,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="native"
    )


@pytest.mark.parametrize(
    "ew",
    [
        pytest.param(EW_U8, id="u8"),
        pytest.param(EW_U16, id="u16"),
        pytest.param(EW_U32, id="u32"),
        pytest.param(EW_U64, id="u64"),
    ],
)
def test_integer_fma_is_elementwise_and_preserves_accumulators(ew: int) -> None:
    values_a = _integer_tile(ew, [2, 3, 5, 7])
    values_b = _integer_tile(ew, [11, 13, 17, 19])
    existing = _integer_tile(ew, [23, 29, 31, 37])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=values_a,
            src1=values_b,
            dst0=existing,
        )

    _assert_native_matches_oracle(
        "t.fma", setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    "ew",
    [
        pytest.param(EW_U8, id="u8"),
        pytest.param(EW_U16, id="u16"),
        pytest.param(EW_U32, id="u32"),
        pytest.param(EW_U64, id="u64"),
    ],
)
def test_integer_dot_replaces_the_full_256_bit_accumulator(ew: int) -> None:
    if ew == EW_U64:
        values_a = _integer_tile(ew, [MASK64, MASK64 - 1])
        values_b = _integer_tile(ew, [MASK64, 3])
    else:
        values_a = _integer_tile(ew, [1, 2, 3, 4])
        values_b = _integer_tile(ew, [5, 7, 11, 13])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=values_a,
            src1=values_b,
            tctrl=0,
        )

    _assert_native_matches_oracle(
        "t.dot", setup, expected_dispatch="fallback"
    )


def test_integer_dotacc_populates_all_four_chunk_accumulators() -> None:
    values_a = _integer_tile(EW_U16, list(range(1, 33)))
    values_b = _integer_tile(EW_U16, [1, 2, 3, 4])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U16,
            src0=values_a,
            src1=values_b,
            tctrl=0x3,
        )

    _assert_native_matches_oracle(
        "t.dotacc", setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    "instruction",
    [
        pytest.param("t.sum", id="sum"),
        pytest.param("t.rmin", id="min"),
        pytest.param("t.rmax", id="max"),
        pytest.param("t.popcnt", id="popcnt"),
        pytest.param("t.l1", id="l1"),
        pytest.param("t.sumsq", id="sumsq"),
        pytest.param("t.minidx", id="minidx"),
        pytest.param("t.maxidx", id="maxidx"),
    ],
)
def test_integer_reduction_opcode_and_accumulator_state(instruction: str) -> None:
    source = _integer_tile(
        EW_U16,
        [-7, 4, -2, 11, 0, -13, 9, 3],
    )

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U16 | 0x10,
            src0=source,
            src1=bytes(64),
            tctrl=0x3,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


def test_u64_reduction_carries_into_the_full_256_bit_accumulator() -> None:
    source = _integer_tile(EW_U64, [MASK64])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U64,
            src0=source,
            src1=bytes(64),
            tctrl=0,
        )

    _assert_native_matches_oracle(
        "t.sum", setup, expected_dispatch="fallback"
    )


def test_signed_u64_dot_extremes_use_the_python_256_bit_oracle() -> None:
    source_a = _integer_tile(
        EW_U64,
        [-(1 << 63), (1 << 63) - 1, -1, 2],
    )
    source_b = _integer_tile(EW_U64, [-1, 2, -(1 << 63), -3])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U64 | 0x10,
            src0=source_a,
            src1=source_b,
            tctrl=0x3,
        )

    _assert_native_matches_oracle(
        "t.dot", setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    ("instruction", "values"),
    [
        pytest.param(
            "t.sum",
            [(1 << 63) - 1, 1, -(1 << 63), -1],
            id="signed-sum-overflow-boundaries",
        ),
        pytest.param(
            "t.l1",
            [-(1 << 63), -1, 0, 1],
            id="signed-l1-int64-min",
        ),
        pytest.param(
            "t.sumsq",
            [-(1 << 63), (1 << 63) - 1, -1, 1],
            id="signed-sumsq-wide-result",
        ),
    ],
)
def test_signed_u64_reduction_extremes_are_transactional_fallbacks(
    instruction: str,
    values: list[int],
) -> None:
    source = _integer_tile(EW_U64, values)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U64 | 0x10,
            src0=source,
            src1=bytes(64),
            tctrl=0x3,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    "ew",
    [
        pytest.param(EW_FP16, id="fp16"),
        pytest.param(EW_BF16, id="bf16"),
    ],
)
def test_fp_wmul_writes_every_lane_across_both_destination_tiles(ew: int) -> None:
    # Lane 31 is unique so a truncated 16-lane implementation cannot pass.
    values_a = [float(index + 1) for index in range(32)]
    values_b = [0.5] * 31 + [-3.0]
    source_a = _floating_tile(ew, values_a)
    source_b = _floating_tile(ew, values_b)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
        )

    oracle = _assert_native_matches_oracle(
        "t.wmul", setup, expected_dispatch="native"
    )
    assert oracle["after_mex"]["memory:dst1"][-4:] != bytes(4)


@pytest.mark.parametrize(
    ("ew", "large"),
    [
        pytest.param(EW_FP16, 65504.0, id="fp16"),
        pytest.param(EW_BF16, 8192.0, id="bf16"),
    ],
)
def test_finite_fp_dot_uses_oracle_precision_on_adversarial_order(
    ew: int,
    large: float,
) -> None:
    # A float accumulator loses the middle 1.0; Python's double-precision
    # evaluation preserves it before the architectural FP32 conversion.
    source_a = _floating_tile_exact(ew, [large, 1.0, -large])
    source_b = _floating_tile_exact(ew, [large, 1.0, large])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
            tctrl=0x2,
        )

    oracle = _assert_native_matches_oracle(
        "t.dot", setup, expected_dispatch="native"
    )
    assert oracle["after_mex"]["acc"] == (0x3F80_0000, 0, 0, 0)


@pytest.mark.parametrize(
    ("ew", "large"),
    [
        pytest.param(EW_FP16, 65504.0, id="fp16"),
        pytest.param(EW_BF16, 8192.0, id="bf16"),
    ],
)
def test_finite_fp_dotacc_uses_all_distinct_chunks_at_oracle_precision(
    ew: int,
    large: float,
) -> None:
    source_a_values: list[float] = []
    source_b_values: list[float] = []
    for residual in (1.0, 2.0, 3.0, 4.0):
        source_a_values.extend([large, residual, -large, 0, 0, 0, 0, 0])
        source_b_values.extend([large, 1.0, large, 0, 0, 0, 0, 0])
    source_a = _floating_tile_exact(ew, source_a_values)
    source_b = _floating_tile_exact(ew, source_b_values)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
            tctrl=0x2,
        )

    oracle = _assert_native_matches_oracle(
        "t.dotacc", setup, expected_dispatch="native"
    )
    assert oracle["after_mex"]["acc"] == (
        0x3F80_0000,
        0x4000_0000,
        0x4040_0000,
        0x4080_0000,
    )


@pytest.mark.parametrize(
    ("instruction", "ew", "values", "expected_acc0"),
    [
        pytest.param(
            "t.sum",
            EW_FP16,
            [65504.0, 2 ** -24, -65504.0],
            0x3380_0000,
            id="fp16-sum-cancellation",
        ),
        pytest.param(
            "t.sum",
            EW_BF16,
            [float(2 ** 30), 1.0, float(-(2 ** 30))],
            0x3F80_0000,
            id="bf16-sum-cancellation",
        ),
        pytest.param(
            "t.sumsq",
            EW_FP16,
            [4096.0, 1.0, 1.0, 1.0],
            0x4B80_0002,
            id="fp16-sumsq-rounding",
        ),
        pytest.param(
            "t.sumsq",
            EW_BF16,
            [4096.0, 1.0, 1.0, 1.0],
            0x4B80_0002,
            id="bf16-sumsq-rounding",
        ),
    ],
)
def test_fp_sum_and_sumsq_use_transactional_python_fallback(
    instruction: str,
    ew: int,
    values: list[float],
    expected_acc0: int,
) -> None:
    source = _floating_tile_exact(ew, values)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source,
            src1=bytes(64),
            tctrl=0x2,
        )

    oracle = _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )
    assert oracle["after_mex"]["acc"][0] == expected_acc0


@pytest.mark.parametrize(
    "instruction",
    [
        pytest.param("t.rmin", id="rmin"),
        pytest.param("t.rmax", id="rmax"),
        pytest.param("t.minidx", id="minidx"),
        pytest.param("t.maxidx", id="maxidx"),
    ],
)
@pytest.mark.parametrize(
    "ew",
    [
        pytest.param(EW_FP16, id="fp16"),
        pytest.param(EW_BF16, id="bf16"),
    ],
)
def test_finite_fp_min_max_reductions_remain_native(
    instruction: str,
    ew: int,
) -> None:
    source = _floating_tile_exact(ew, [3.0, -2.0, 5.0, 1.0])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source,
            src1=bytes(64),
            tctrl=0x2,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="native"
    )


@pytest.mark.parametrize(
    "instruction",
    [
        pytest.param("t.rmin", id="rmin"),
        pytest.param("t.rmax", id="rmax"),
        pytest.param("t.minidx", id="minidx"),
        pytest.param("t.maxidx", id="maxidx"),
    ],
)
@pytest.mark.parametrize(
    ("ew", "nan_bits"),
    [
        pytest.param(EW_FP16, 0xFD55, id="fp16-negative-nan"),
        pytest.param(EW_BF16, 0x7F95, id="bf16-payload-nan"),
    ],
)
def test_nonfinite_fp_min_max_reductions_use_transactional_fallback(
    instruction: str,
    ew: int,
    nan_bits: int,
) -> None:
    source = nan_bits.to_bytes(2, "little") * _lane_count(ew)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source,
            src1=bytes(64),
            tctrl=0x2,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    ("instruction", "ew", "first", "second", "expected_bits"),
    [
        pytest.param(op, ew, first, second, expected, id=case_id)
        for op in ("t.min", "t.max")
        for ew, sign_bit, width_name in (
            (EW_FP16, 0x8000, "fp16"),
            (EW_BF16, 0x8000, "bf16"),
        )
        for first, second, expected, order_name in (
            (0.0, -0.0, 0x0000, "positive-first"),
            (-0.0, 0.0, sign_bit, "negative-first"),
        )
        for case_id in (f"{width_name}-{op[2:]}-{order_name}",)
    ],
)
def test_fp_min_max_preserve_first_operand_signed_zero(
    instruction: str,
    ew: int,
    first: float,
    second: float,
    expected_bits: int,
) -> None:
    source_a = _floating_tile(ew, [first])
    source_b = _floating_tile(ew, [second])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
        )

    oracle = _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="native"
    )
    assert int.from_bytes(
        oracle["after_mex"]["memory:dst0"][:2], "little"
    ) == expected_bits


@pytest.mark.parametrize(
    ("instruction", "ew", "nan_bits", "canonical_bits"),
    [
        pytest.param(
            instruction,
            ew,
            nan_bits,
            canonical_bits,
            id=f"{width_name}-{instruction[2:]}",
        )
        for instruction in ("t.min", "t.max")
        for ew, nan_bits, canonical_bits, width_name in (
            (EW_FP16, 0x7D55, 0x7E00, "fp16"),
            (EW_BF16, 0x7F95, 0x7FC0, "bf16"),
        )
    ],
)
def test_fp_min_max_canonicalize_nan_without_fallback(
    instruction: str,
    ew: int,
    nan_bits: int,
    canonical_bits: int,
) -> None:
    source_a = nan_bits.to_bytes(2, "little") * _lane_count(ew)
    source_b = _floating_tile(ew, [1.0])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
        )

    oracle = _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="native"
    )
    assert int.from_bytes(
        oracle["after_mex"]["memory:dst0"][:2], "little"
    ) == canonical_bits


@pytest.mark.parametrize(
    ("instruction", "ew"),
    [
        pytest.param(
            instruction,
            ew,
            id=f"{width_name}-{instruction[2:]}-infinite-source",
        )
        for instruction in (
            "t.add",
            "t.sub",
            "t.mul",
            "t.dot",
            "t.wmul",
            "t.mac",
            "t.fma",
            "t.dotacc",
        )
        for ew, width_name in ((EW_FP16, "fp16"), (EW_BF16, "bf16"))
    ],
)
def test_nonfinite_fp_arithmetic_source_fallbacks_exactly_once(
    instruction: str,
    ew: int,
) -> None:
    source_a = _floating_tile_exact(ew, [float("inf")])
    source_b = _floating_tile_exact(ew, [1.0])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
            tctrl=0x2,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    ("instruction", "source_b_bits", "result_kind"),
    [
        pytest.param("t.add", 0x7F7F, "tile", id="add"),
        pytest.param("t.sub", 0xFF7F, "tile", id="sub"),
        pytest.param("t.mul", 0x7F7F, "tile", id="mul"),
        pytest.param("t.wmul", 0x7F7F, "wide", id="wmul"),
        pytest.param("t.dot", 0x7F7F, "dot", id="dot-acc-zero"),
        pytest.param("t.mac", 0x7F7F, "tile", id="mac"),
        pytest.param("t.fma", 0x7F7F, "tile", id="fma"),
        pytest.param(
            "t.dotacc",
            0x7F7F,
            "dotacc",
            id="dotacc-acc-zero",
        ),
    ],
)
def test_fp32_range_bf16_overflow_fallback_completes_as_infinity(
    instruction: str,
    source_b_bits: int,
    result_kind: str,
) -> None:
    max_finite = (0x7F7F).to_bytes(2, "little") * _lane_count(EW_BF16)
    source_b = (
        source_b_bits.to_bytes(2, "little") * _lane_count(EW_BF16)
    )

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_BF16,
            src0=max_finite,
            src1=source_b,
            dst0=max_finite,
            tctrl=0x2 if result_kind in ("dot", "dotacc") else 0,
        )

    oracle = _assert_native_matches_oracle(
        instruction,
        setup,
        expected_dispatch="fallback",
    )
    after_mex = oracle["after_mex"]
    bf16_infinity_tile = (0x7F80).to_bytes(2, "little") * 32
    fp32_infinity_tile = (0x7F80_0000).to_bytes(4, "little") * 16

    if result_kind == "tile":
        assert after_mex["memory:dst0"] == bf16_infinity_tile
    elif result_kind == "wide":
        assert after_mex["memory:dst0"] == fp32_infinity_tile
        assert after_mex["memory:dst1"] == fp32_infinity_tile
    elif result_kind == "dot":
        assert after_mex["acc"] == (0x7F80_0000, 0, 0, 0)
    else:
        assert result_kind == "dotacc"
        assert after_mex["acc"] == (0x7F80_0000,) * 4


def test_bf16_dotacc_later_chunk_overflow_completes_every_chunk() -> None:
    one = (0x3F80).to_bytes(2, "little")
    max_finite = (0x7F7F).to_bytes(2, "little")
    source = one * 8 + max_finite * 8 + one * 16

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_BF16,
            src0=source,
            src1=source,
            tctrl=0x2,
        )

    oracle = _assert_native_matches_oracle(
        "t.dotacc",
        setup,
        expected_dispatch="fallback",
    )
    assert oracle["after_mex"]["acc"] == (
        0x4100_0000,
        0x7F80_0000,
        0x4100_0000,
        0x4100_0000,
    )


@pytest.mark.parametrize(
    ("source_a_bits", "source_b_bits", "expected_bits"),
    [
        pytest.param(0x7F7F, 0x7B00, 0x7F80, id="positive"),
        pytest.param(0xFF7F, 0xFB00, 0xFF80, id="negative"),
    ],
)
def test_native_bf16_rounding_overflow_produces_signed_infinity(
    source_a_bits: int,
    source_b_bits: int,
    expected_bits: int,
) -> None:
    source_a = source_a_bits.to_bytes(2, "little") * 32
    source_b = source_b_bits.to_bytes(2, "little") * 32

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_BF16,
            src0=source_a,
            src1=source_b,
        )

    oracle = _assert_native_matches_oracle(
        "t.add",
        setup,
        expected_dispatch="native",
    )
    assert oracle["after_mex"]["memory:dst0"] == (
        expected_bits.to_bytes(2, "little") * 32
    )


@pytest.mark.parametrize(
    ("instruction", "ew"),
    [
        pytest.param(
            instruction,
            ew,
            id=f"{width_name}-{instruction[2:]}-infinite-destination",
        )
        for instruction in ("t.mac", "t.fma")
        for ew, width_name in ((EW_FP16, "fp16"), (EW_BF16, "bf16"))
    ],
)
def test_nonfinite_fp_mac_destination_fallbacks_exactly_once(
    instruction: str,
    ew: int,
) -> None:
    source_a = _floating_tile_exact(ew, [2.0])
    source_b = _floating_tile_exact(ew, [3.0])
    destination = _floating_tile_exact(ew, [float("inf")])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
            dst0=destination,
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


def test_bf16_add_nan_payload_uses_python_fallback_encoding() -> None:
    source_a = (0x7F8D).to_bytes(2, "little") + bytes(62)
    source_b = (0x7FDF).to_bytes(2, "little") + bytes(62)

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_BF16,
            src0=source_a,
            src1=source_b,
        )

    oracle = _assert_native_matches_oracle(
        "t.add", setup, expected_dispatch="fallback"
    )
    assert int.from_bytes(
        oracle["after_mex"]["memory:dst0"][:2], "little"
    ) == 0x7FCD


@pytest.mark.parametrize(
    ("instruction", "ew"),
    [
        pytest.param(
            instruction,
            ew,
            id=f"{width_name}-{instruction[2:]}-nonfinite-accacc",
        )
        for instruction in ("t.dot", "t.dotacc")
        for ew, width_name in ((EW_FP16, "fp16"), (EW_BF16, "bf16"))
    ],
)
def test_nonfinite_fp_accacc_fallbacks_exactly_once(
    instruction: str,
    ew: int,
) -> None:
    source_a = _floating_tile_exact(ew, [1.0])
    source_b = _floating_tile_exact(ew, [1.0])

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=ew,
            src0=source_a,
            src1=source_b,
            tctrl=0x1,
            acc=(0x7F80_0000, 0x7FC0_0000, 0xFF80_0000, 0),
        )

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


@pytest.mark.parametrize(
    ("instruction", "expected_bytes"),
    [
        pytest.param("t.trans", b"\xE3\x00", id="funct0-trans"),
        pytest.param("t.shuffle", b"\xE3\x01", id="funct1-shuffle"),
        pytest.param("t.movbank", b"\xE3\x02", id="funct2-movbank"),
        pytest.param("t.loadc", b"\xE3\x03", id="funct3-loadc"),
        pytest.param("t.zero", b"\xE3\x04", id="funct4-zero"),
        pytest.param("t.pack", b"\xE3\x05", id="funct5-pack"),
        pytest.param("t.unpack", b"\xE3\x06", id="funct6-unpack"),
        pytest.param("t.rrot 5", b"\xE3\x07\x05", id="funct7-rrot"),
    ],
)
def test_tsys_encoding_and_public_behavior_use_explicit_fallback(
    instruction: str,
    expected_bytes: bytes,
) -> None:
    assert bytes(assemble(instruction)) == expected_bytes
    source0 = bytes((index * 3 + 1) & 0xFF for index in range(64))
    source1 = bytes(63 - index for index in range(64))
    destination = bytes((index * 5 + 7) & 0xFF for index in range(64))
    cursor = bytes((index * 7 + 9) & 0xFF for index in range(64))

    def setup(cpu: Any) -> Watchers:
        tmode = EW_U16 if instruction == "t.pack" else EW_U8
        watchers = _seed_common_state(
            cpu,
            tmode=tmode,
            src0=source0,
            src1=source1,
            dst0=destination,
        )
        # Python's architectural cursor is (SR*SW+SC)*64 in bank SB.
        cpu.sb = 0
        cpu.sr = 10
        cpu.sc = 0
        cpu.sw = 1
        cursor_addr = 10 * 64
        cpu.mem[cursor_addr:cursor_addr + 64] = cursor
        watchers["cursor"] = _watch_bank(cpu, cursor_addr)
        return watchers

    _assert_native_matches_oracle(
        instruction, setup, expected_dispatch="fallback"
    )


def test_common_run_stops_after_mex_halt_and_reports_cycles() -> None:
    source_a = _floating_tile(EW_FP16, [2.0, 3.0, 5.0, 7.0])
    source_b = _floating_tile(EW_FP16, [11.0, 13.0, 17.0, 19.0])

    def run(cpu_type: CPUFactory) -> dict[str, Any]:
        cpu = cpu_type(mem_size=MEM_SIZE)
        watchers = _seed_common_state(
            cpu,
            tmode=EW_FP16,
            src0=source_a,
            src1=source_b,
        )
        program = assemble("t.wmul\nhalt\nnop")
        cpu.load_bytes(0, program)
        cpu.pc = 0
        before_mex = _snapshot(cpu, watchers)
        pre_fallback: list[dict[str, Any]] = []
        if cpu_type is NativeMegapad64:
            pre_fallback = _install_dispatch_probe(cpu, "native", watchers)
        reported_cycles = cpu.run(max_steps=20)
        if cpu_type is NativeMegapad64:
            _assert_dispatch("native", before_mex, pre_fallback)
        return {
            "reported_cycles": reported_cycles,
            "state": _snapshot(cpu, watchers),
        }

    oracle = run(PythonMegapad64)
    native = run(NativeMegapad64)
    assert oracle["reported_cycles"] == oracle["state"]["cycle_count"]
    differences = _state_differences(oracle, native)
    assert not differences, (
        "accelerated run() did not preserve MEX cycle/stop semantics:\n  "
        + "\n  ".join(differences)
    )


@pytest.mark.parametrize(
    ("instruction", "expected_dispatch"),
    [
        pytest.param("t.add", "native", id="native-talu"),
        pytest.param("t.sum", "fallback", id="fallback-reduction"),
    ],
)
def test_run_steps_preserves_native_and_fallback_dispatch(
    instruction: str,
    expected_dispatch: Dispatch,
) -> None:
    source_a = _integer_tile(EW_U64, [MASK64, 7, 11, 13])
    source_b = _integer_tile(EW_U64, [1, 3, 5, 17])
    program = assemble(f"{instruction}\nhalt")

    def setup(cpu: Any) -> Watchers:
        return _seed_common_state(
            cpu,
            tmode=EW_U64,
            src0=source_a,
            src1=source_b,
            tctrl=0x2,
        )

    oracle_cpu = PythonMegapad64(mem_size=MEM_SIZE)
    oracle_watchers = setup(oracle_cpu)
    oracle_cpu.load_bytes(0, program)
    oracle_cpu.pc = 0
    oracle_cpu.step()

    native_cpu = NativeMegapad64(mem_size=MEM_SIZE)
    native_watchers = setup(native_cpu)
    native_cpu.load_bytes(0, program)
    native_cpu.pc = 0
    before_mex = _snapshot(native_cpu, native_watchers)
    pre_fallback = _install_dispatch_probe(
        native_cpu, expected_dispatch, native_watchers
    )

    steps_executed, stop_reason = native_cpu.run_steps(max_steps=1)

    assert (steps_executed, stop_reason) == (1, 0)
    _assert_dispatch(expected_dispatch, before_mex, pre_fallback)
    differences = _state_differences(
        _snapshot(oracle_cpu, oracle_watchers),
        _snapshot(native_cpu, native_watchers),
    )
    assert not differences, (
        f"run_steps diverged for {instruction!r}:\n  "
        + "\n  ".join(differences)
    )


def test_run_steps_preserves_native_prefix_before_python_fallback() -> None:
    source_a = _integer_tile(EW_U64, [1, 2, 3, 4])
    source_b = _integer_tile(EW_U64, [5, 6, 7, 8])
    program = assemble("nop\nt.sum\nhalt")

    def setup(cpu: Any) -> Watchers:
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U64,
            src0=source_a,
            src1=source_b,
            tctrl=0x2,
        )
        cpu.load_bytes(0, program)
        cpu.pc = 0
        return watchers

    oracle_cpu = PythonMegapad64(mem_size=MEM_SIZE)
    oracle_watchers = setup(oracle_cpu)
    assert oracle_cpu.step() == 1
    after_native_prefix = _snapshot(oracle_cpu, oracle_watchers)

    raw_cpu = NativeMegapad64(mem_size=MEM_SIZE)
    raw_watchers = setup(raw_cpu)
    raw_result = _mp64_accel.run_steps(
        raw_cpu._cs,
        mmio_read8=raw_cpu._mmio_read8,
        mmio_write8=raw_cpu._mmio_write8,
        on_output=raw_cpu._do_output,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
        max_steps=2,
    )

    assert (
        raw_result.steps_executed,
        raw_result.total_cycles,
        raw_result.stop_reason,
    ) == (1, 1, 3)
    differences = _state_differences(
        after_native_prefix,
        _snapshot(raw_cpu, raw_watchers),
    )
    assert not differences, (
        "raw run_steps did not stop transactionally at the fallback boundary:\n  "
        + "\n  ".join(differences)
    )

    assert oracle_cpu.step() == 1
    after_fallback = _snapshot(oracle_cpu, oracle_watchers)

    native_cpu = NativeMegapad64(mem_size=MEM_SIZE)
    native_watchers = setup(native_cpu)
    pre_fallback = _install_dispatch_probe(
        native_cpu, "fallback", native_watchers
    )

    assert native_cpu.run_steps(max_steps=2) == (2, 0)
    _assert_dispatch("fallback", after_native_prefix, pre_fallback)
    differences = _state_differences(
        after_fallback,
        _snapshot(native_cpu, native_watchers),
    )
    assert not differences, (
        "wrapper lost prefix progress or diverged after Python fallback:\n  "
        + "\n  ".join(differences)
    )


@pytest.mark.parametrize(
    ("region", "attach_name", "base"),
    [
        pytest.param("hbw", "attach_hbw", 0x1_0000, id="hbw"),
        pytest.param("ext", "attach_ext_mem", 0x2_0000, id="ext"),
        pytest.param("vram", "attach_vram", 0x3_0000, id="vram"),
    ],
)
@pytest.mark.parametrize(
    "access",
    [
        pytest.param("read", id="read"),
        pytest.param("write", id="write"),
    ],
)
@pytest.mark.parametrize(
    ("offset", "complete_tile"),
    [
        pytest.param(64, True, id="exact-final-tile"),
        pytest.param(65, False, id="63-byte-tail"),
    ],
)
def test_attached_region_tail_requires_a_complete_64_byte_tile(
    region: str,
    attach_name: str,
    base: int,
    access: str,
    offset: int,
    complete_tile: bool,
) -> None:
    aperture_template = bytearray(
        (index * 3 + 1) & 0xFF for index in range(128)
    )
    source0 = bytes((index * 5 + 7) & 0xFF for index in range(64))
    source1 = bytes((index * 7 + 11) & 0xFF for index in range(64))
    destination = bytes([0xA5] * 64)

    def setup(cpu: Any) -> Watchers:
        # Seed the modulo-alias in Bank0.  An aperture tail that incorrectly
        # falls through is therefore both observable and destructive.
        cpu.mem[:] = bytes(
            (index * 13 + 0x3D) & 0xFF for index in range(MEM_SIZE)
        )
        aperture = bytearray(aperture_template)
        getattr(cpu, attach_name)(aperture, base, len(aperture))
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=source0,
            src1=source1,
            dst0=destination,
        )
        if access == "read":
            cpu.tsrc0 = base + offset
        else:
            cpu.tdst = base + offset
        watchers[region] = lambda: bytes(aperture)
        return watchers

    oracle = _assert_native_matches_oracle(
        "t.add", setup, expected_dispatch="native"
    )
    if access == "read":
        actual = oracle["after_mex"]["memory:dst0"]
        aperture_source = (
            bytes(aperture_template[64:128]) if complete_tile else bytes(64)
        )
        expected = bytes(
            (left + right) & 0xFF
            for left, right in zip(aperture_source, source1)
        )
        assert actual == expected
    elif complete_tile:
        assert (
            oracle["after_mex"][f"memory:{region}"][64:]
            != bytes(aperture_template[64:])
        )
    else:
        assert (
            oracle["after_mex"][f"memory:{region}"]
            == bytes(aperture_template)
        )


@pytest.mark.parametrize(
    "access",
    [
        pytest.param("read", id="read"),
        pytest.param("write", id="write"),
    ],
)
def test_main_memory_final_63_bytes_are_not_a_wrapping_tile(
    access: str,
) -> None:
    tail_address = MEM_SIZE - 63
    initial_memory = bytes(
        (index * 11 + 0x27) & 0xFF for index in range(MEM_SIZE)
    )
    source0 = bytes((index * 3 + 1) & 0xFF for index in range(64))
    source1 = bytes((index * 5 + 9) & 0xFF for index in range(64))

    def setup(cpu: Any) -> Watchers:
        cpu.mem[:] = initial_memory
        watchers = _seed_common_state(
            cpu,
            tmode=EW_U8,
            src0=source0,
            src1=source1,
        )
        if access == "read":
            cpu.tsrc0 = tail_address
        else:
            cpu.tdst = tail_address
        watchers["main_tail"] = lambda: bytes(cpu.mem[tail_address:])
        return watchers

    oracle = _assert_native_matches_oracle(
        "t.add", setup, expected_dispatch="native"
    )
    if access == "read":
        assert oracle["after_mex"]["memory:dst0"] == source1
    else:
        assert (
            oracle["after_mex"]["memory:main_tail"]
            == initial_memory[tail_address:]
        )


@pytest.mark.parametrize(
    ("region", "attach_name", "base"),
    [
        pytest.param("hbw", "attach_hbw", 0x1_0020, id="hbw"),
        pytest.param("ext", "attach_ext_mem", 0x2_0040, id="ext"),
        pytest.param("vram", "attach_vram", 0x3_0060, id="vram"),
    ],
)
@pytest.mark.parametrize(
    "access",
    [
        pytest.param("ld.h r1, r2", id="load-halfword"),
        pytest.param("st.h r1, r2", id="store-halfword"),
    ],
)
@pytest.mark.parametrize(
    ("boundary", "start_delta", "expected_load"),
    [
        pytest.param("enter", -1, 0xAACC, id="enter-aperture"),
        pytest.param("exit", 1, 0xCCBB, id="exit-aperture"),
    ],
)
def test_scalar_halfword_crosses_aperture_tail_byte_by_byte(
    region: str,
    attach_name: str,
    base: int,
    access: str,
    boundary: str,
    start_delta: int,
    expected_load: int,
) -> None:
    """Mirror system.py's byte routing at a scalar aperture boundary."""
    program_address = 0x80
    start_address = base + start_delta
    bank0_address = (
        start_address if boundary == "enter" else start_address + 1
    ) % 256

    def run(cpu_type: CPUFactory) -> dict[str, Any]:
        cpu = cpu_type(mem_size=256)
        cpu.mem[:] = bytes((index * 7 + 5) & 0xFF for index in range(256))
        aperture = bytearray([0xAA, 0xBB])
        getattr(cpu, attach_name)(aperture, base, len(aperture))

        original_read8 = cpu.mem_read8
        original_write8 = cpu.mem_write8

        def mapped_read8(addr: int) -> int:
            address = addr & MASK64
            if base <= address < base + len(aperture):
                return aperture[address - base]
            return original_read8(address)

        def mapped_write8(addr: int, value: int) -> None:
            address = addr & MASK64
            if base <= address < base + len(aperture):
                aperture[address - base] = value & 0xFF
            else:
                original_write8(address, value)

        def mapped_read16(addr: int) -> int:
            return (
                mapped_read8(addr)
                | (mapped_read8((addr + 1) & MASK64) << 8)
            )

        def mapped_write16(addr: int, value: int) -> None:
            mapped_write8(addr, value)
            mapped_write8((addr + 1) & MASK64, value >> 8)

        cpu.mem_read8 = mapped_read8
        cpu.mem_write8 = mapped_write8
        cpu.mem_read16 = mapped_read16
        cpu.mem_write16 = mapped_write16

        cpu.regs[1] = start_address
        cpu.regs[2] = start_address
        if access.startswith("ld"):
            cpu.mem[bank0_address] = 0xCC
        else:
            cpu.regs[2] = 0xCCDD
        cpu.load_bytes(program_address, assemble(access))
        cpu.pc = program_address
        cycles = cpu.step()
        return {
            "cycles": cycles,
            "state": _snapshot(cpu, {region: lambda: bytes(aperture)}),
        }

    oracle = run(PythonMegapad64)
    native = run(NativeMegapad64)
    differences = _state_differences(oracle, native)
    assert not differences, (
        f"scalar {access} crossed the {region} {boundary} boundary "
        "differently:\n  "
        + "\n  ".join(differences)
    )
    if access.startswith("ld"):
        assert oracle["state"]["regs"][1] == expected_load
    elif boundary == "enter":
        assert oracle["state"][f"memory:{region}"] == b"\xCC\xBB"
        assert oracle["state"]["memory:bank0"][bank0_address] == 0xDD
    else:
        assert oracle["state"][f"memory:{region}"] == b"\xAA\xDD"
        assert oracle["state"]["memory:bank0"][bank0_address] == 0xCC


def test_attached_regions_support_exact_tiles_near_uint64_max() -> None:
    hbw_base = MASK64 - 191
    ext_base = MASK64 - 127
    vram_base = MASK64 - 63
    source0_template = bytearray((index * 3 + 1) & 0xFF for index in range(64))
    source1_template = bytearray((index * 5 + 7) & 0xFF for index in range(64))
    destination_template = bytearray([0xA5] * 64)

    def setup(cpu: Any) -> Watchers:
        source0 = bytearray(source0_template)
        source1 = bytearray(source1_template)
        destination = bytearray(destination_template)
        cpu.attach_hbw(source0, hbw_base, 64)
        cpu.attach_ext_mem(source1, ext_base, 64)
        cpu.attach_vram(destination, vram_base, 64)
        cpu.tmode = EW_U8
        cpu.tctrl = 0
        cpu.tsrc0 = hbw_base
        cpu.tsrc1 = ext_base
        cpu.tdst = vram_base
        cpu.acc = [1, 2, 3, 4]
        cpu.flags_unpack(0b1010_1100)
        return {
            "hbw": lambda: bytes(source0),
            "ext": lambda: bytes(source1),
            "vram": lambda: bytes(destination),
        }

    oracle = _assert_native_matches_oracle(
        "t.add", setup, expected_dispatch="native"
    )
    assert oracle["after_mex"]["memory:vram"] != bytes(destination_template)


@pytest.mark.parametrize(
    ("region", "attach_name"),
    [
        pytest.param("hbw", "attach_hbw", id="hbw"),
        pytest.param("ext", "attach_ext_mem", id="ext"),
        pytest.param("vram", "attach_vram", id="vram"),
    ],
)
@pytest.mark.parametrize(
    "access",
    [
        pytest.param("read", id="read"),
        pytest.param("write", id="write"),
    ],
)
def test_near_uint64_max_address_cannot_overflow_aperture_bounds(
    region: str,
    attach_name: str,
    access: str,
) -> None:
    """A failed range check must not turn into an out-of-bounds host pointer."""
    script = textwrap.dedent(
        f"""
        import json
        import resource

        from accel_wrapper import Megapad64
        from asm import assemble
        from megapad64 import MASK64

        resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
        cpu = Megapad64(mem_size=512)
        aperture = bytearray(range(64))
        cpu.{attach_name}(aperture, 0, 64)
        cpu.mem[:] = bytes((index * 11 + 3) & 0xff for index in range(512))
        cpu.tmode = 0
        cpu.mem[128:192] = bytes([1]) * 64
        cpu.mem[192:256] = bytes([2]) * 64
        cpu.mem[256:320] = bytes([0xA5]) * 64
        if {access!r} == "read":
            cpu.tsrc0 = MASK64 - 31
            cpu.tsrc1 = 128
            cpu.tdst = 256
        else:
            cpu.tsrc0 = 128
            cpu.tsrc1 = 192
            cpu.tdst = MASK64 - 31
        cpu.load_bytes(0, assemble("t.add\\nhalt"))
        cpu.pc = 0

        def forbid_fallback():
            raise AssertionError("overflowing aperture address left native path")

        cpu._step_python_fallback = forbid_fallback
        mex_cycles = cpu.step()
        print(json.dumps({{
            "mex_cycles": mex_cycles,
            "pc": cpu.pc,
            "cycle_count": cpu.cycle_count,
            "acc": list(cpu.acc),
            "flags": cpu.flags_pack(),
            "tctrl": cpu.tctrl,
            "mem": bytes(cpu.mem).hex(),
            "aperture": bytes(aperture).hex(),
        }}, sort_keys=True))
        """
    )
    completed = subprocess.run(
        [sys.executable, "-c", script],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert completed.returncode == 0, (
        f"native MEX crashed during overflowing {region} {access} "
        f"(returncode={completed.returncode}, stderr={completed.stderr!r})"
    )
    native = json.loads(completed.stdout)

    cpu = PythonMegapad64(mem_size=512)
    aperture = bytearray(range(64))
    getattr(cpu, attach_name)(aperture, 0, 64)
    cpu.mem[:] = bytes((index * 11 + 3) & 0xFF for index in range(512))
    cpu.tmode = 0
    cpu.mem[128:192] = bytes([1]) * 64
    cpu.mem[192:256] = bytes([2]) * 64
    cpu.mem[256:320] = bytes([0xA5]) * 64
    if access == "read":
        cpu.tsrc0 = MASK64 - 31
        cpu.tsrc1 = 128
        cpu.tdst = 256
    else:
        cpu.tsrc0 = 128
        cpu.tsrc1 = 192
        cpu.tdst = MASK64 - 31
    cpu.load_bytes(0, assemble("t.add\nhalt"))
    cpu.pc = 0
    mex_cycles = cpu.step()
    oracle = {
        "mex_cycles": mex_cycles,
        "pc": cpu.pc,
        "cycle_count": cpu.cycle_count,
        "acc": list(cpu.acc),
        "flags": cpu.flags_pack(),
        "tctrl": cpu.tctrl,
        "mem": bytes(cpu.mem).hex(),
        "aperture": bytes(aperture).hex(),
    }
    assert native == oracle


def test_halt_exception_class_is_the_public_oracle() -> None:
    """Guard the helper itself: both public CPUs expose the same halt error."""
    for cpu_type in (PythonMegapad64, NativeMegapad64):
        cpu = cpu_type(mem_size=128)
        cpu.load_bytes(0, assemble("halt"))
        cpu.step()
        with pytest.raises(HaltError):
            cpu.step()
