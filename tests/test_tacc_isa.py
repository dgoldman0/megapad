"""Executable contract tests for the full-width tile accumulator ISA."""

from __future__ import annotations

import struct

import pytest

from asm import AsmError, assemble
from cli import disasm_one
from megapad64 import (
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    EW_BF16,
    EW_FP16,
    EW_U8,
    EW_U16,
    EW_U32,
    IVEC_ALIGN_FAULT,
    IVEC_BUS_FAULT,
    IVEC_ILLEGAL_OP,
    IVEC_PRIV_FAULT,
    TACC_CANONICAL_NAN,
    TACC_IMAGE_BYTES,
    TACC_OWNER_NONE,
    Megapad64,
    TrapError,
    _tacc_fp32_add_product,
)


CODE_BASE = 0
SOURCE_A = 0x400
SOURCE_B = 0x480
IMAGE_A = 0x800
IMAGE_B = 0xA00


def _load_instruction(cpu: Megapad64, source: str) -> bytearray:
    code = assemble(source)
    cpu.load_bytes(CODE_BASE, code)
    cpu._icache_invalidate_all(reset_statistics=False)
    cpu.pc = CODE_BASE
    return code


def _step(cpu: Megapad64, source: str) -> int:
    _load_instruction(cpu, source)
    return cpu.step()


def _tacc_snapshot(cpu: Megapad64) -> tuple:
    return (
        bytes(cpu.tacc),
        cpu.tacc_owner,
        cpu.tacc_valid,
        cpu.tacc_dirty,
        cpu.tacc_format_ew,
        cpu.tacc_format_signed,
        cpu.tacc_busy,
        cpu.tacc_force_pending,
    )


def _claim_and_clear(cpu: Megapad64, ew: int, signed: int = 0) -> None:
    cpu.tmode = ew | ((signed & 1) << 4)
    code = _load_instruction(cpu, "t.acc.try\nt.acc.clear")
    assert bytes(code) == bytes.fromhex("f8e302f8e303")
    assert cpu.step() == 2
    assert cpu.step() == 2


def _write_elements(
    cpu: Megapad64,
    base: int,
    values: list[int],
    element_bits: int,
) -> None:
    element_bytes = element_bits // 8
    mask = (1 << element_bits) - 1
    for lane, value in enumerate(values):
        offset = base + lane * element_bytes
        cpu.mem[offset:offset + element_bytes] = (
            value & mask
        ).to_bytes(element_bytes, "little")


def _read_tacc_lane(cpu: Megapad64, lane: int, lane_bits: int) -> int:
    lane_bytes = lane_bits // 8
    offset = lane * lane_bytes
    return int.from_bytes(cpu.tacc[offset:offset + lane_bytes], "little")


@pytest.mark.parametrize(
    ("source", "encoding", "text"),
    [
        ("t.amac", "e106", "t.amac"),
        ("t.amac r7", "e50607", "t.amac r7"),
        ("t.amac inplace", "ed06", "t.amac inplace"),
        ("t.acc.try", "f8e302", "t.acc.try"),
        ("t.acc.clear", "f8e303", "t.acc.clear"),
        ("t.acc.load", "f8e304", "t.acc.load"),
        ("t.acc.store", "f8e305", "t.acc.store"),
        ("t.acc.release", "f8e306", "t.acc.release"),
    ],
)
def test_canonical_assembly_and_disassembly(
    source: str,
    encoding: str,
    text: str,
) -> None:
    code = assemble(source)
    assert bytes(code) == bytes.fromhex(encoding)
    assert disasm_one(code, 0, len(code)) == (text, len(code))


def test_assembler_sizes_lifecycle_labels_and_rejects_ambiguous_forms() -> None:
    labels = {}
    assemble("t.acc.try\nafter:", labels_out=labels)
    assert labels["after"] == 3

    with pytest.raises(AsmError, match="R0-R15"):
        assemble("t.amac r16")
    with pytest.raises(AsmError, match="expects"):
        assemble("t.amac 7")
    with pytest.raises(AsmError, match="does not take operands"):
        assemble("t.acc.clear r1")


@pytest.mark.parametrize(
    ("raw", "end_pc", "fault_cycles"),
    [
        (bytes.fromhex("e107"), 2, 2),       # reserved TMUL function 7
        (bytes.fromhex("e126"), 2, 2),       # noncanonical TAMAC function
        (bytes.fromhex("e906"), 2, 2),       # illegal immediate selector
        (bytes.fromhex("f8e307"), 3, 1),     # reserved lifecycle function 7
        (bytes.fromhex("f8e322"), 3, 1),     # noncanonical lifecycle function
        (bytes.fromhex("f8e70200"), 4, 1),   # noncanonical lifecycle selector
    ],
)
def test_reserved_and_noncanonical_encodings_trap_after_complete_decode(
    raw: bytes,
    end_pc: int,
    fault_cycles: int,
) -> None:
    cpu = Megapad64(mem_size=4096)
    cpu.load_bytes(0, raw)
    cpu.trap_addr = 0xBADC0DE
    before = _tacc_snapshot(cpu)

    with pytest.raises(TrapError) as raised:
        cpu.step()

    assert raised.value.ivec_id == IVEC_ILLEGAL_OP
    assert cpu.pc == end_pc
    assert cpu.trap_addr == 0xBADC0DE
    assert _tacc_snapshot(cpu) == before
    assert cpu.perf_tileops == 0
    assert cpu.cycle_count == fault_cycles
    assert cpu.perf_cycles == fault_cycles


@pytest.mark.parametrize("immediate", [0x07, 0x0E, 0x86])
def test_non_tacc_ss2_immediates_retain_legacy_execution(
    immediate: int,
) -> None:
    cpu = Megapad64(mem_size=4096)
    cpu.tmode = EW_U8
    cpu.tsrc0 = SOURCE_A
    cpu.tdst = IMAGE_A
    cpu.mem[SOURCE_A:SOURCE_A + 64] = bytes([3]) * 64
    cpu.load_bytes(0, bytes((0xE9, immediate)))
    before = _tacc_snapshot(cpu)

    assert cpu.step() == 2
    assert cpu.mem[IMAGE_A] == ((immediate * 3) & 0xFF)
    assert _tacc_snapshot(cpu) == before


def test_skip_treats_reserved_lifecycle_encoding_as_three_bytes() -> None:
    cpu = Megapad64(mem_size=4096)
    program = assemble("skip")
    program.extend(bytes.fromhex("f8e307"))
    program.extend(assemble("inc r1"))
    cpu.load_bytes(0, program)

    assert cpu.step() == 3
    assert cpu.pc == 5
    assert cpu.step() == 1
    assert cpu.regs[1] == 1


def test_lifecycle_status_and_locked_base_cycles() -> None:
    cpu = Megapad64(mem_size=4096, core_id=3)
    assert cpu.csr_read(CSR_TACC_STATUS) == (TACC_OWNER_NONE << 16)
    assert cpu.csr_read(CSR_TACC_CTL) == 0

    assert _step(cpu, "t.acc.try") == 2
    status = cpu.csr_read(CSR_TACC_STATUS)
    assert status & 0x3 == 0x3
    assert (status >> 16) & 0x1F == 3
    assert not cpu.tacc_valid

    before = _tacc_snapshot(cpu)
    assert _step(cpu, "t.acc.try") == 2
    assert _tacc_snapshot(cpu) == before

    cpu.tmode = EW_U16 | (1 << 4)
    assert _step(cpu, "t.acc.clear") == 2
    status = cpu.csr_read(CSR_TACC_STATUS)
    assert status & (1 << 2)
    assert status & (1 << 3)
    assert (status >> 5) & 0x7 == EW_U16
    assert (status >> 8) & 1 == 1
    assert bytes(cpu.tacc) == bytes(TACC_IMAGE_BYTES)

    assert _step(cpu, "t.acc.release") == 2
    assert cpu.csr_read(CSR_TACC_STATUS) == (TACC_OWNER_NONE << 16)
    assert bytes(cpu.tacc) == bytes(TACC_IMAGE_BYTES)
    assert cpu.perf_tileops == 4


def test_failed_try_retires_normally_without_mutating_foreign_state() -> None:
    cpu = Megapad64(mem_size=4096, core_id=2)
    cpu.tacc_owner = 7
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc[:] = bytes([0xA5]) * TACC_IMAGE_BYTES
    before = _tacc_snapshot(cpu)

    assert _step(cpu, "t.acc.try") == 2

    assert _tacc_snapshot(cpu) == before
    assert cpu.csr_read(CSR_TACC_STATUS) & 1
    assert not (cpu.csr_read(CSR_TACC_STATUS) & (1 << 1))
    assert cpu.perf_tileops == 1
    assert cpu.perf_cycles == 2


@pytest.mark.parametrize("ew", [3, 6, 7])
@pytest.mark.parametrize(
    ("instruction", "fault_cycles"),
    [
        ("t.acc.clear", 1),
        ("t.acc.load", 2),
        ("t.amac", 2),
    ],
)
def test_unsupported_modes_fault_before_reads_or_mutation(
    ew: int,
    instruction: str,
    fault_cycles: int,
) -> None:
    cpu = Megapad64(mem_size=4096)
    if instruction == "t.amac":
        _claim_and_clear(cpu, EW_U8)
    else:
        assert _step(cpu, "t.acc.try") == 2
    cpu.tmode = ew
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B
    cpu.trap_addr = 0xCAFE
    before = _tacc_snapshot(cpu)
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    tileops_before = cpu.perf_tileops

    def forbidden_read(_addr: int) -> int:
        raise AssertionError("invalid format must fault before source reads")

    cpu.mem_read8 = forbidden_read
    code = _load_instruction(cpu, instruction)
    with pytest.raises(TrapError) as raised:
        cpu.step()

    assert raised.value.ivec_id == IVEC_ILLEGAL_OP
    assert cpu.pc == len(code)
    assert cpu.trap_addr == 0xCAFE
    assert _tacc_snapshot(cpu) == before
    assert cpu.cycle_count - cycles_before == fault_cycles
    assert cpu.perf_cycles - perf_before == fault_cycles
    assert cpu.perf_tileops == tileops_before


def test_force_release_is_privileged_and_honors_terminal_boundary() -> None:
    cpu = Megapad64(mem_size=4096, core_id=1)
    _claim_and_clear(cpu, EW_U8)
    cpu.trap_addr = 0x1234
    before = _tacc_snapshot(cpu)
    cpu.priv_level = 1

    with pytest.raises(TrapError) as raised:
        cpu.csr_write(CSR_TACC_CTL, 1)

    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == 0x1234
    assert _tacc_snapshot(cpu) == before

    cpu.priv_level = 0
    cpu.tacc_busy = True
    cpu.csr_write(CSR_TACC_CTL, 1)
    assert cpu.tacc_force_pending
    assert cpu.tacc_owner == 1
    cpu._tacc_finish_atomic_operation()
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert bytes(cpu.tacc) == bytes(TACC_IMAGE_BYTES)

    _claim_and_clear(cpu, EW_U16)
    cpu.csr_write(CSR_TACC_CTL, 1 | (0xFFFF << 1))
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert not cpu.tacc_valid


@pytest.mark.parametrize(
    ("ew", "source_bits", "accumulator_bits", "raw_one", "expected_cycles"),
    [
        (EW_U8, 8, 32, 1, 7),
        (EW_U16, 16, 64, 1, 5),
        (EW_U32, 32, 64, 1, 4),
        (EW_FP16, 16, 32, 0x3C00, 7),
        (EW_BF16, 16, 32, 0x3F80, 7),
    ],
)
def test_tile_tamac_layout_result_and_cycle_table(
    ew: int,
    source_bits: int,
    accumulator_bits: int,
    raw_one: int,
    expected_cycles: int,
) -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, ew)
    lane_count = 512 // source_bits
    _write_elements(cpu, SOURCE_A, [raw_one] * lane_count, source_bits)
    _write_elements(cpu, SOURCE_B, [raw_one] * lane_count, source_bits)
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B

    assert _step(cpu, "t.amac") == expected_cycles

    expected = 0x3F800000 if ew in (EW_FP16, EW_BF16) else 1
    assert all(
        _read_tacc_lane(cpu, lane, accumulator_bits) == expected
        for lane in range(lane_count)
    )
    active = lane_count * (accumulator_bits // 8)
    assert bytes(cpu.tacc[active:]) == bytes(TACC_IMAGE_BYTES - active)
    assert cpu.tacc_dirty


@pytest.mark.parametrize(
    ("ew", "source_bits", "accumulator_bits", "raw_value", "cycles"),
    [
        (EW_U8, 8, 32, 3, 6),
        (EW_U16, 16, 64, 3, 4),
        (EW_U32, 32, 64, 3, 3),
        (EW_FP16, 16, 32, 0x4000, 6),
        (EW_BF16, 16, 32, 0x4000, 6),
    ],
)
def test_broadcast_tamac_uses_only_low_active_gpr_element(
    ew: int,
    source_bits: int,
    accumulator_bits: int,
    raw_value: int,
    cycles: int,
) -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, ew)
    lane_count = 512 // source_bits
    _write_elements(cpu, SOURCE_A, [raw_value] * lane_count, source_bits)
    cpu.tsrc0 = SOURCE_A
    scalar = 0x3C00 if ew == EW_FP16 else (
        0x3F80 if ew == EW_BF16 else 2
    )
    cpu.regs[7] = 0xDEAD_BEEF_0000_0000 | scalar

    assert _step(cpu, "t.amac r7") == cycles

    if ew in (EW_FP16, EW_BF16):
        expected = 0x40000000
    else:
        expected = 6
    assert _read_tacc_lane(cpu, 0, accumulator_bits) == expected
    assert _read_tacc_lane(cpu, lane_count - 1, accumulator_bits) == expected


def test_signed_integer_wrap_and_inplace_sources_are_exact() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U16, signed=1)
    _write_elements(cpu, IMAGE_A, [0xFFFF] + [0] * 31, 16)
    _write_elements(cpu, SOURCE_A, [3] + [0] * 31, 16)
    _write_elements(cpu, SOURCE_B, [0x7777] * 32, 16)
    cpu.tdst = IMAGE_A
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B
    cpu.tacc[0:8] = (1).to_bytes(8, "little")

    assert _step(cpu, "t.amac inplace") == 5

    assert _read_tacc_lane(cpu, 0, 64) == ((1 - 3) & ((1 << 64) - 1))
    assert cpu.mem[IMAGE_A:IMAGE_A + 2] == bytes.fromhex("ffff")
    assert cpu.acc == [0, 0, 0, 0]


def test_u32_product_and_accumulator_wrap_are_modulo_64_bits() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U32)
    _write_elements(cpu, SOURCE_A, [0xFFFF_FFFF] + [0] * 15, 32)
    _write_elements(cpu, SOURCE_B, [0xFFFF_FFFF] + [0] * 15, 32)
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B
    cpu.tacc[0:8] = (0xFFFF_FFFF_FFFF_FFFF).to_bytes(8, "little")

    assert _step(cpu, "t.amac") == 4
    assert _read_tacc_lane(cpu, 0, 64) == 0xFFFF_FFFE_0000_0000


@pytest.mark.parametrize(
    ("acc", "a", "b", "ew", "expected"),
    [
        (0x00000000, 0x3C00, 0x4000, EW_FP16, 0x40000000),
        (0x3F800000, 0x3980, 0x3980, EW_BF16, 0x3F800000),
        (0x3F800001, 0x3980, 0x3980, EW_BF16, 0x3F800002),
        (0x00000000, 0x0001, 0x3F80, EW_BF16, 0x00010000),
        (0x00000000, 0x7C00, 0x0000, EW_FP16, TACC_CANONICAL_NAN),
        (0x7F800000, 0xFC00, 0x3C00, EW_FP16, TACC_CANONICAL_NAN),
        (0x7FC01234, 0x3C00, 0x3C00, EW_FP16, TACC_CANONICAL_NAN),
        (0x80000000, 0x8000, 0x3C00, EW_FP16, 0x80000000),
    ],
)
def test_exact_fp_oracle_edges(
    acc: int,
    a: int,
    b: int,
    ew: int,
    expected: int,
) -> None:
    assert _tacc_fp32_add_product(acc, a, b, ew) == expected


def test_fp_tamac_canonicalizes_nan_and_preserves_inactive_zeroes() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_FP16)
    _write_elements(cpu, SOURCE_A, [0x7E01] + [0x3C00] * 31, 16)
    _write_elements(cpu, SOURCE_B, [0x3C00] * 32, 16)
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B
    cpu.tacc[128:] = bytes([0xFF]) * 128

    assert _step(cpu, "t.amac") == 7
    assert _read_tacc_lane(cpu, 0, 32) == TACC_CANONICAL_NAN
    assert bytes(cpu.tacc[128:]) == bytes(128)


def test_load_store_canonical_image_and_preserve_cursor_csrs() -> None:
    cpu = Megapad64(mem_size=4096, core_id=2)
    _claim_and_clear(cpu, EW_U32, signed=1)
    image = bytes(range(256))
    cpu.mem[IMAGE_A:IMAGE_A + 256] = image
    cpu.tsrc0 = IMAGE_A
    cpu.tdst = IMAGE_B
    cpu.sb, cpu.sr, cpu.sc, cpu.sw = 1, 2, 3, 4
    cursors = (cpu.tsrc0, cpu.tdst, cpu.sb, cpu.sr, cpu.sc, cpu.sw)

    assert _step(cpu, "t.acc.load") == 6
    assert bytes(cpu.tacc[:128]) == image[:128]
    assert bytes(cpu.tacc[128:]) == bytes(128)
    assert cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert cpu.tacc_format_ew == EW_U32
    assert cpu.tacc_format_signed == 1
    assert (cpu.tsrc0, cpu.tdst, cpu.sb, cpu.sr, cpu.sc, cpu.sw) == cursors

    cpu.tacc[0:8] = (0x0123_4567_89AB_CDEF).to_bytes(8, "little")
    cpu.tacc_dirty = True
    assert _step(cpu, "t.acc.store") == 6
    assert (
        cpu.mem[IMAGE_B:IMAGE_B + 8]
        == bytes.fromhex("efcdab8967452301")
    )
    assert bytes(cpu.mem[IMAGE_B + 128:IMAGE_B + 256]) == bytes(128)
    assert not cpu.tacc_dirty
    assert (cpu.tsrc0, cpu.tdst, cpu.sb, cpu.sr, cpu.sc, cpu.sw) == cursors


def test_load_uses_routed_attached_memory() -> None:
    cpu = Megapad64(mem_size=4096)
    external_base = 0x10_0000
    external = bytearray(512)
    external[:256] = bytes([0xA6]) * 256
    cpu.attach_ext_mem(external, external_base, len(external))
    _claim_and_clear(cpu, EW_U8)
    cpu.tsrc0 = external_base

    assert _step(cpu, "t.acc.load") == 34
    assert bytes(cpu.tacc) == bytes([0xA6]) * 256
    assert cpu.perf_stalls == 28
    assert cpu.perf_extmem == 32


def test_load_fault_is_atomic_and_reports_faulting_beat() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    cpu.tacc[:] = bytes([0x5A]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.tsrc0 = IMAGE_A
    cpu.mem[IMAGE_A:IMAGE_A + 256] = bytes([0xC3]) * 256
    before = _tacc_snapshot(cpu)
    tileops_before = cpu.perf_tileops
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles
    original_read8 = cpu.mem_read8

    def faulting_read8(addr: int) -> int:
        if addr == IMAGE_A + 64:
            cpu.trap_addr = addr
            raise TrapError(IVEC_BUS_FAULT)
        return original_read8(addr)

    cpu.mem_read8 = faulting_read8
    code = _load_instruction(cpu, "t.acc.load")

    with pytest.raises(TrapError) as raised:
        cpu.step()

    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.pc == len(code)
    assert cpu.trap_addr == IMAGE_A + 64
    assert _tacc_snapshot(cpu) == before
    assert cpu.perf_tileops == tileops_before
    assert cpu.cycle_count - cycles_before == 4
    assert cpu.perf_cycles - perf_before == 4


@pytest.mark.parametrize("dirty", [False, True])
def test_store_transport_fault_preserves_preinstruction_dirty_state(
    dirty: bool,
) -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    cpu.tacc[:] = bytes(range(256))
    cpu.tacc_dirty = dirty
    cpu.tdst = IMAGE_A
    original_write8 = cpu.mem_write8
    cycles_before = cpu.cycle_count
    perf_before = cpu.perf_cycles

    def faulting_write8(addr: int, value: int) -> None:
        if addr == IMAGE_A + 73:
            cpu.trap_addr = addr
            raise TrapError(IVEC_BUS_FAULT)
        original_write8(addr, value)

    cpu.mem_write8 = faulting_write8
    code = _load_instruction(cpu, "t.acc.store")

    with pytest.raises(TrapError) as raised:
        cpu.step()

    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.pc == len(code)
    assert cpu.trap_addr == IMAGE_A + 64
    assert bytes(cpu.mem[IMAGE_A:IMAGE_A + 64]) == bytes(range(64))
    assert bytes(cpu.mem[IMAGE_A + 64:IMAGE_A + 128]) == bytes(64)
    assert cpu.tacc_dirty is dirty
    assert cpu.tacc_valid
    assert cpu.cycle_count - cycles_before == 4
    assert cpu.perf_cycles - perf_before == 4


def test_complete_span_permission_preflight_precedes_store_writes() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    cpu.tacc[:] = bytes([0xD2]) * 256
    cpu.tdst = IMAGE_A
    before_memory = bytes(cpu.mem[IMAGE_A:IMAGE_A + 256])
    before_tacc = _tacc_snapshot(cpu)
    cycles_before = cpu.cycle_count
    seen = []

    def deny_routed_span(start: int, size: int, write: bool):
        seen.append((start, size, write))
        return IVEC_PRIV_FAULT, start + 128

    cpu._tacc_span_validator = deny_routed_span
    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.acc.store")
    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == IMAGE_A + 128
    assert seen == [(IMAGE_A, 256, True)]
    assert bytes(cpu.mem[IMAGE_A:IMAGE_A + 256]) == before_memory
    assert _tacc_snapshot(cpu) == before_tacc
    assert cpu.cycle_count - cycles_before == 2


@pytest.mark.parametrize("instruction", ("t.acc.load", "t.acc.store"))
def test_user_image_preflight_applies_complete_active_mpu_window(
    instruction: str,
) -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    cpu.tacc[:] = bytes([0xD2]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.tsrc0 = IMAGE_A
    cpu.tdst = IMAGE_A
    cpu.mem[IMAGE_A:IMAGE_A + TACC_IMAGE_BYTES] = bytes(
        [0xA5]
    ) * TACC_IMAGE_BYTES
    before_memory = bytes(
        cpu.mem[IMAGE_A:IMAGE_A + TACC_IMAGE_BYTES]
    )
    before_tacc = _tacc_snapshot(cpu)
    cycles_before = cpu.cycle_count
    cpu.priv_level = 1
    cpu.mpu_base = IMAGE_A
    cpu.mpu_limit = IMAGE_A + 128

    with pytest.raises(TrapError) as raised:
        _step(cpu, instruction)

    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == IMAGE_A + 128
    assert bytes(
        cpu.mem[IMAGE_A:IMAGE_A + TACC_IMAGE_BYTES]
    ) == before_memory
    assert _tacc_snapshot(cpu) == before_tacc
    assert cpu.cycle_count - cycles_before == 2


@pytest.mark.parametrize("instruction", ("t.acc.load", "t.acc.store"))
def test_user_image_preflight_rejects_hbw_before_any_beat(
    instruction: str,
) -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    hbw_base = 0x1_0000
    hbw = bytearray([0xA5]) * TACC_IMAGE_BYTES
    cpu.attach_hbw(hbw, hbw_base, len(hbw))
    cpu.tacc[:] = bytes([0xD2]) * TACC_IMAGE_BYTES
    cpu.tacc_dirty = True
    cpu.tsrc0 = hbw_base
    cpu.tdst = hbw_base
    before_hbw = bytes(hbw)
    before_tacc = _tacc_snapshot(cpu)
    cycles_before = cpu.cycle_count
    cpu.priv_level = 1

    with pytest.raises(TrapError) as raised:
        _step(cpu, instruction)

    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert cpu.trap_addr == hbw_base
    assert bytes(hbw) == before_hbw
    assert _tacc_snapshot(cpu) == before_tacc
    assert cpu.cycle_count - cycles_before == 2


def test_image_preflight_rejects_alignment_span_and_mmio_before_writes() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    cpu.tacc[:] = bytes([0x11]) * 256

    cpu.tdst = IMAGE_A + 1
    code = _load_instruction(cpu, "t.acc.store")
    before = bytes(cpu.mem)
    with pytest.raises(TrapError) as raised:
        cpu.step()
    assert raised.value.ivec_id == IVEC_ALIGN_FAULT
    assert cpu.trap_addr == IMAGE_A + 1
    assert cpu.pc == len(code)
    assert bytes(cpu.mem) == before

    cpu.tdst = 4096 - 128
    code = _load_instruction(cpu, "t.acc.store")
    before = bytes(cpu.mem)
    with pytest.raises(TrapError) as raised:
        cpu.step()
    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == 4096
    assert cpu.pc == len(code)
    assert bytes(cpu.mem) == before

    mmio_backing = bytearray(512)
    cpu.attach_vram(mmio_backing, cpu.MMIO_START, len(mmio_backing))
    cpu.tdst = cpu.MMIO_START
    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.acc.store")
    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == cpu.MMIO_START
    assert bytes(mmio_backing) == bytes(len(mmio_backing))


def test_ownership_format_and_source_faults_precede_reads_and_mutation() -> None:
    cpu = Megapad64(mem_size=4096, core_id=1)
    cpu.tacc_owner = 2
    cpu.tacc_valid = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc[:] = bytes([0x44]) * 256
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B
    reads = 0
    original_read8 = cpu.mem_read8

    def counting_read8(addr: int) -> int:
        nonlocal reads
        reads += 1
        return original_read8(addr)

    cpu.mem_read8 = counting_read8
    before = _tacc_snapshot(cpu)
    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.amac")
    assert raised.value.ivec_id == IVEC_ILLEGAL_OP
    assert reads == 0
    assert _tacc_snapshot(cpu) == before

    cpu.tacc_owner = 1
    cpu.tmode = EW_U16
    before = _tacc_snapshot(cpu)
    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.amac")
    assert raised.value.ivec_id == IVEC_ILLEGAL_OP
    assert reads == 0
    assert _tacc_snapshot(cpu) == before

    cpu.tmode = EW_U8
    cpu.tsrc1 = 4096 - 32
    before = _tacc_snapshot(cpu)
    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.amac")
    assert raised.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == 4096
    assert reads == 0
    assert _tacc_snapshot(cpu) == before


def test_trap_handler_saves_complete_instruction_return_pc() -> None:
    cpu = Megapad64(mem_size=4096)
    cpu.ivt_base = 0x100
    cpu.mem_write64(cpu.ivt_base + 8 * IVEC_ILLEGAL_OP, 0x300)
    cpu.sp = 0xF00
    cpu.load_bytes(0, bytes.fromhex("f8e307"))

    total = cpu.run(max_steps=1)

    assert total == 1
    assert cpu.pc == 0x300
    assert cpu.mem_read64(cpu.sp) == 3


def test_reset_zeroizes_tacc_and_restores_free_owner() -> None:
    cpu = Megapad64(mem_size=4096, core_id=3)
    _claim_and_clear(cpu, EW_U16, signed=1)
    cpu.tacc[:] = bytes([0xFE]) * 256
    cpu.tacc_force_pending = True
    epoch_before = cpu.tacc_epoch

    cpu._reset_state()

    assert bytes(cpu.tacc) == bytes(256)
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert not cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert not cpu.tacc_busy
    assert not cpu.tacc_force_pending
    assert cpu.tacc_format_ew == 0
    assert cpu.tacc_format_signed == 0
    assert cpu.tacc_epoch == epoch_before + 1


def test_tamac_does_not_mutate_legacy_acc_or_configuration() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_U8)
    _write_elements(cpu, SOURCE_A, [2] * 64, 8)
    _write_elements(cpu, SOURCE_B, [3] * 64, 8)
    cpu.tsrc0, cpu.tsrc1, cpu.tdst = SOURCE_A, SOURCE_B, IMAGE_A
    cpu.sb, cpu.sr, cpu.sc, cpu.sw = 3, 4, 5, 6
    cpu.tctrl = 0xFFFF
    cpu.acc = [1, 2, 3, 4]
    before = (
        cpu.tsrc0,
        cpu.tsrc1,
        cpu.tdst,
        cpu.sb,
        cpu.sr,
        cpu.sc,
        cpu.sw,
        cpu.tmode,
        cpu.tctrl,
        tuple(cpu.acc),
    )

    assert _step(cpu, "t.amac") == 7
    assert (
        cpu.tsrc0,
        cpu.tsrc1,
        cpu.tdst,
        cpu.sb,
        cpu.sr,
        cpu.sc,
        cpu.sw,
        cpu.tmode,
        cpu.tctrl,
        tuple(cpu.acc),
    ) == before


def test_csr_encoding_observes_status_and_force_release() -> None:
    cpu = Megapad64(mem_size=4096, core_id=0)
    _claim_and_clear(cpu, EW_U8)
    cpu.regs[1] = 1
    code = _load_instruction(
        cpu,
        f"csrr r0, {CSR_TACC_STATUS}\ncsrw {CSR_TACC_CTL}, r1",
    )

    assert cpu.step() == 1
    assert cpu.regs[0] == cpu.csr_read(CSR_TACC_STATUS)
    assert cpu.step() == 1
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert cpu.pc == len(code)


def test_fp32_lane_bits_are_little_endian_in_image() -> None:
    cpu = Megapad64(mem_size=4096)
    _claim_and_clear(cpu, EW_FP16)
    _write_elements(cpu, SOURCE_A, [0x3C00] + [0] * 31, 16)
    _write_elements(cpu, SOURCE_B, [0x4000] + [0] * 31, 16)
    cpu.tsrc0 = SOURCE_A
    cpu.tsrc1 = SOURCE_B

    _step(cpu, "t.amac")

    assert cpu.tacc[:4] == struct.pack("<I", 0x40000000)
