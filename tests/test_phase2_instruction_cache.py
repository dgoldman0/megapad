"""Phase 2 element-7 functional instruction-cache contracts."""

from __future__ import annotations

import pytest

import _mp64_accel
from accel_wrapper import (
    Megapad64 as NativeMegapad64,
    Megapad64Micro as NativeMegapad64Micro,
)
from asm import assemble
from megapad64 import (
    CSR_ICACHE_CTRL,
    CSR_ICACHE_HITS,
    CSR_ICACHE_MISSES,
    Megapad64 as PythonMegapad64,
    Megapad64Micro as PythonMegapad64Micro,
)
from system import MegapadSystem


FULL_CORE_TYPES = (
    pytest.param(NativeMegapad64, id="native"),
    pytest.param(PythonMegapad64, id="python"),
)
MICRO_CORE_TYPES = (
    pytest.param(NativeMegapad64Micro, id="native"),
    pytest.param(PythonMegapad64Micro, id="python"),
)

LINE_BYTES = 16
LINE_COUNT = 256


def _execute_at(cpu, address: int) -> int:
    cpu.halted = False
    cpu.idle = False
    cpu.pc = address
    return cpu.step()


def _cache_state(cpu) -> tuple[int, int, int]:
    return (
        cpu.csr_read(CSR_ICACHE_CTRL),
        cpu.csr_read(CSR_ICACHE_HITS),
        cpu.csr_read(CSR_ICACHE_MISSES),
    )


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_256_by_16_direct_map_records_one_cold_miss_then_one_hit_per_line(
    cpu_type,
):
    cpu = cpu_type(mem_size=LINE_COUNT * LINE_BYTES)
    nop = assemble("nop")[0]
    for index in range(LINE_COUNT):
        line_base = index * LINE_BYTES
        cpu.mem[line_base] = nop
        cpu.mem[line_base + 8] = nop

    for index in range(LINE_COUNT):
        _execute_at(cpu, index * LINE_BYTES)

    assert _cache_state(cpu) == (1, 0, LINE_COUNT)

    for index in range(LINE_COUNT):
        _execute_at(cpu, index * LINE_BYTES + 8)

    assert _cache_state(cpu) == (1, LINE_COUNT, LINE_COUNT)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_counters_track_aligned_eight_byte_fetch_requests(cpu_type):
    cpu = cpu_type(mem_size=64)
    cpu.load_bytes(0, assemble("nop"))

    _execute_at(cpu, 0)

    # A one-byte instruction performs one lookup, not one lookup per byte
    # copied into the cache line.
    assert _cache_state(cpu) == (1, 0, 1)

    crossing = assemble("ldi r1, 0x2a")
    assert len(crossing) == 3
    cpu.load_bytes(6, crossing)
    cpu.csr_write(CSR_ICACHE_CTRL, 3)

    _execute_at(cpu, 6)

    # Bytes 6-7 use the first aligned response; byte 8 needs the other
    # eight-byte response from the same already-filled 16-byte line.
    assert cpu.regs[1] == 0x2A
    assert _cache_state(cpu) == (1, 1, 1)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_control_values_preserve_or_invalidate_exactly_as_encoded(cpu_type):
    cpu = cpu_type(mem_size=64)
    nop = assemble("nop")[0]
    cpu.mem[0] = nop
    cpu.mem[8] = nop
    _execute_at(cpu, 0)
    _execute_at(cpu, 8)
    assert _cache_state(cpu) == (1, 1, 1)

    cpu.csr_write(CSR_ICACHE_CTRL, 0)
    assert _cache_state(cpu) == (0, 1, 1)

    cpu.csr_write(CSR_ICACHE_CTRL, 1)
    assert _cache_state(cpu) == (1, 1, 1)

    cpu.csr_write(CSR_ICACHE_CTRL, 2)
    assert _cache_state(cpu) == (0, 0, 0)
    _execute_at(cpu, 0)
    assert _cache_state(cpu) == (0, 0, 0)

    cpu.csr_write(CSR_ICACHE_CTRL, 3)
    assert _cache_state(cpu) == (1, 0, 0)
    _execute_at(cpu, 0)
    assert _cache_state(cpu) == (1, 0, 1)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_disabled_cache_bypasses_stale_line_without_counting(cpu_type):
    cpu = cpu_type(mem_size=64)
    old_opcode = assemble("inc r1")[0]
    new_opcode = assemble("inc r2")[0]
    cpu.mem[0] = old_opcode
    _execute_at(cpu, 0)
    assert (cpu.regs[1], cpu.regs[2]) == (1, 0)
    assert _cache_state(cpu) == (1, 0, 1)

    cpu.csr_write(CSR_ICACHE_CTRL, 0)
    cpu.mem[0] = new_opcode
    _execute_at(cpu, 0)
    _execute_at(cpu, 0)

    assert (cpu.regs[1], cpu.regs[2]) == (1, 2)
    assert _cache_state(cpu) == (0, 0, 1)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_direct_map_tag_includes_address_bits_above_bank_zero(cpu_type):
    high_address = 1 << 20
    cpu = cpu_type(mem_size=high_address + LINE_BYTES)
    cpu.mem[0] = assemble("inc r1")[0]
    cpu.mem[high_address] = assemble("inc r2")[0]

    _execute_at(cpu, 0)
    _execute_at(cpu, high_address)
    _execute_at(cpu, 0)

    # Both addresses use index zero. A tag truncated to Bank-0 address bits
    # would execute the low opcode at the high address instead of conflicting.
    assert (cpu.regs[1], cpu.regs[2]) == (2, 1)
    assert _cache_state(cpu) == (1, 0, 3)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_shared_backing_mutation_stays_stale_per_core_until_invalidation(
    cpu_type,
):
    backing = bytearray(512)
    target = 0x100
    backing[target] = assemble("inc r1")[0]
    first = cpu_type(mem_size=len(backing), core_id=0, num_cores=2)
    second = cpu_type(mem_size=len(backing), core_id=1, num_cores=2)
    first.mem = backing
    second.mem = backing

    _execute_at(first, target)
    _execute_at(second, target)
    backing[target] = assemble("inc r2")[0]

    _execute_at(first, target)
    _execute_at(second, target)
    assert (first.regs[1], first.regs[2]) == (2, 0)
    assert (second.regs[1], second.regs[2]) == (2, 0)

    first.csr_write(CSR_ICACHE_CTRL, 3)
    _execute_at(first, target)
    _execute_at(second, target)
    assert (first.regs[1], first.regs[2]) == (2, 1)
    assert (second.regs[1], second.regs[2]) == (3, 0)

    second.csr_write(CSR_ICACHE_CTRL, 3)
    _execute_at(second, target)
    assert (second.regs[1], second.regs[2]) == (3, 1)


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_completed_scalar_store_invalidates_own_matching_line(cpu_type):
    target = 0x100
    store_address = 0x200
    cpu = cpu_type(mem_size=0x400)
    cpu.load_bytes(target, assemble("inc r5"))
    cpu.load_bytes(store_address, assemble("st.b r1, r2"))

    _execute_at(cpu, target)
    assert (cpu.regs[5], cpu.regs[6]) == (1, 0)

    cpu.regs[1] = target
    cpu.regs[2] = assemble("inc r6")[0]
    _execute_at(cpu, store_address)
    misses_before_refetch = cpu.csr_read(CSR_ICACHE_MISSES)

    _execute_at(cpu, target)

    assert (cpu.regs[5], cpu.regs[6]) == (1, 1)
    assert cpu.csr_read(CSR_ICACHE_MISSES) == misses_before_refetch + 1


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_cross_line_store_invalidates_each_touched_private_line(cpu_type):
    first_target = 0x0F
    second_target = 0x10
    store_address = 0x100
    cpu = cpu_type(mem_size=0x400)
    cpu.load_bytes(first_target, assemble("inc r7"))
    cpu.load_bytes(second_target, assemble("inc r8"))
    cpu.load_bytes(store_address, assemble("str r1, r2"))

    _execute_at(cpu, first_target)
    _execute_at(cpu, second_target)
    assert (cpu.regs[7], cpu.regs[8]) == (1, 1)

    replacement = bytes(
        (assemble("inc r5")[0], assemble("inc r6")[0])
    ) + bytes(6)
    cpu.regs[1] = first_target
    cpu.regs[2] = int.from_bytes(replacement, "little")
    _execute_at(cpu, store_address)
    misses_before_refetch = cpu.csr_read(CSR_ICACHE_MISSES)

    _execute_at(cpu, first_target)
    _execute_at(cpu, second_target)

    assert (cpu.regs[5], cpu.regs[6]) == (1, 1)
    assert cpu.csr_read(CSR_ICACHE_MISSES) == misses_before_refetch + 2


def test_accelerator_write_invalidates_every_touched_private_line():
    hook_target = 0x600
    data_stack = 0x900
    return_stack = 0xA00
    first_target = 0x111F
    second_target = 0x1120
    cpu = NativeMegapad64(mem_size=0x2000)
    cpu.load_bytes(0, assemble("call.l r4"))
    cpu.load_bytes(first_target, assemble("inc r1"))
    cpu.load_bytes(second_target, assemble("inc r2"))
    cpu.mem[hook_target] = assemble("nop")[0]
    cpu.regs[4] = hook_target
    cpu.regs[14] = data_stack
    cpu.regs[15] = return_stack
    color = (
        assemble("inc r5")[0]
        | (assemble("inc r6")[0] << 8)
    )
    for index, value in enumerate(
        (color, 1, 1, 2, first_target)
    ):
        start = data_stack + index * 8
        cpu.mem[start:start + 8] = value.to_bytes(8, "little")
    cpu.register_accel_hook(hook_target, 1, 1)

    _execute_at(cpu, first_target)
    _execute_at(cpu, second_target)
    assert (cpu.regs[1], cpu.regs[2]) == (1, 1)

    _execute_at(cpu, 0)
    misses_before_refetch = cpu.csr_read(CSR_ICACHE_MISSES)
    _execute_at(cpu, first_target)
    _execute_at(cpu, second_target)

    assert bytes(cpu.mem[first_target:second_target + 1]) == color.to_bytes(
        2, "little"
    )
    assert (cpu.regs[5], cpu.regs[6]) == (1, 1)
    assert cpu.csr_read(CSR_ICACHE_MISSES) == misses_before_refetch + 2


@pytest.mark.parametrize("cpu_type", FULL_CORE_TYPES)
def test_guest_reset_enables_invalidates_and_zeros_cache_counters(cpu_type):
    target = 0x100
    reset_address = 0x200
    cpu = cpu_type(mem_size=0x400)
    cpu.load_bytes(target, assemble("inc r1"))
    cpu.load_bytes(reset_address, assemble("reset"))
    _execute_at(cpu, target)
    cpu.mem[target] = assemble("inc r2")[0]

    _execute_at(cpu, reset_address)

    assert _cache_state(cpu) == (1, 0, 0)
    _execute_at(cpu, target)
    assert (cpu.regs[1], cpu.regs[2]) == (0, 1)
    assert _cache_state(cpu) == (1, 0, 1)


@pytest.mark.parametrize("cpu_type", MICRO_CORE_TYPES)
def test_microcore_has_no_instruction_cache_or_cache_csr_effect(cpu_type):
    cpu = cpu_type(mem_size=64, core_id=1, num_cores=2)
    cpu.load_bytes(0, assemble("inc r1"))

    _execute_at(cpu, 0)
    cpu.mem[0] = assemble("inc r2")[0]
    _execute_at(cpu, 0)
    cpu.csr_write(CSR_ICACHE_CTRL, 3)

    assert (cpu.regs[1], cpu.regs[2]) == (1, 1)
    assert _cache_state(cpu) == (0, 0, 0)


def test_strict_cold_refill_uses_two_journaled_dwords_and_pins_opcode():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("nop\nhalt"))
    system.boot(entry=0)
    owner = system._native_system
    before = owner._main_bus_snapshot()

    first = system.run_cycle_batch(1, max_instructions=1)

    assert first.instructions_executed == 0
    assert owner.cycle_execution_pending
    pending = owner._cycle_pending_bus_requests()
    assert len(pending) == 1
    assert pending[0].operation == _mp64_accel.BusOperation.READ
    assert pending[0].width == _mp64_accel.BusWidth.DOUBLEWORD
    assert pending[0].address == 8
    assert pending[0].ordering.issue_sequence == 2
    assert owner._main_bus_snapshot().next_grant_sequence == (
        before.next_grant_sequence + 1
    )
    assert _cache_state(system.cpu) == (1, 0, 0)

    # The official host-loading seam refuses to alter an in-flight
    # instruction. A raw backing-buffer mutation cannot change the first
    # already-journaled fetch beat either.
    with pytest.raises(RuntimeError, match="cycle execution is suspended"):
        system.load_binary(0, assemble("inc r1"))
    system.cpu.mem[0] = assemble("inc r1")[0]

    retired = 0
    for _ in range(4):
        result = system.run_cycle_batch(1, max_instructions=1)
        retired += result.instructions_executed
        if retired:
            break

    assert retired == 1
    assert system.cpu.regs[1] == 0
    assert system.cpu.pc == 1
    assert _cache_state(system.cpu) == (1, 0, 1)
    after = owner._main_bus_snapshot()
    assert after.last_issue_sequences[0] - before.last_issue_sequences[0] == 2
    assert after.next_grant_sequence - before.next_grant_sequence == 2
