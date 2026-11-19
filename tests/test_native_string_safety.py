"""Host-memory safety at native EXT.STRING fast-path boundaries."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64, TrapError
from asm import assemble


MASK64 = (1 << 64) - 1


def test_bfill_length_wrap_cannot_enter_host_memset():
    cpu = Megapad64(mem_size=64)
    cpu.load_bytes(0, assemble("bfill r1, r2"))
    cpu.pc = 0
    cpu.regs[1] = 63
    # 63 + (2**64 - 62) wraps to 1.  The former addition-based check
    # therefore admitted this span into memset far beyond the 64-byte buffer.
    cpu.regs[2] = MASK64 - 61
    cpu.d_reg = 0xA5
    cpu.priv_level = 1
    cpu.mpu_base = 0
    cpu.mpu_limit = 1
    memory_before = bytes(cpu.mem)

    with pytest.raises(TrapError):
        cpu.step()

    assert cpu.trap_addr == 63
    assert bytes(cpu.mem) == memory_before


def test_user_bfill_cannot_use_host_memset_to_bypass_mpu():
    cpu = Megapad64(mem_size=64)
    cpu.load_bytes(0, assemble("bfill r1, r2"))
    cpu.pc = 0
    cpu.regs[1] = 32
    cpu.regs[2] = 4
    cpu.d_reg = 0xA5
    cpu.priv_level = 1
    cpu.mpu_base = 0
    cpu.mpu_limit = 16
    memory_before = bytes(cpu.mem)

    with pytest.raises(TrapError):
        cpu.step()

    assert cpu.trap_addr == 32
    assert bytes(cpu.mem) == memory_before


def test_bfill_crossing_the_end_of_memory_faults_beyond_it():
    cpu = Megapad64(mem_size=64)
    cpu.load_bytes(16, assemble("bfill r1, r2"))
    cpu.pc = 16
    cpu.regs[1] = 63
    cpu.regs[2] = 2
    cpu.d_reg = 0x5A
    memory_before = bytes(cpu.mem)

    with pytest.raises(TrapError):
        cpu.step()

    # The fill leaves the 64-byte buffer only through the scalar path, which
    # faults at the first byte outside the map: no host memset runs past the
    # buffer, and nothing wraps onto address 0.
    assert cpu.trap_addr == 64
    assert len(cpu.mem) == 64
    assert bytes(cpu.mem[:63]) == memory_before[:63]
