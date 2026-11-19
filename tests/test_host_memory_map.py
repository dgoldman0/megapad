"""Host-side loads and reads follow the memory map.

The emulator's loaders and raw accessors reach only memory the map holds: a
byte outside it raises ValueError, which names the address and the map's
regions, and a load writes nothing then. The disassembler, which reads
Bank 0, shows an instruction that needs a byte beyond it as unmapped.
"""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64 as NativeMegapad64
from cli import disasm_one
from megapad64 import CSR_TILE_SELFTEST, Megapad64 as PythonMegapad64
from system import EXT_MEM_BASE, MegapadSystem


RAM = 1 << 20
EXT_SIZE = 0x1000


def _system() -> MegapadSystem:
    return MegapadSystem(
        ram_size=RAM, num_cores=1, num_clusters=0, hbw_size=0,
        ext_mem_size=EXT_SIZE, vram_size=0,
    )


def test_a_binary_load_runs_from_one_region_into_the_next():
    system = _system()
    system.load_binary(RAM - 4, b"ABCDEFGH")
    assert bytes(system._shared_mem[RAM - 4:]) == b"ABCD"
    assert bytes(system._ext_mem[:4]) == b"EFGH"


@pytest.mark.parametrize(
    ("addr", "first_outside"),
    [pytest.param(EXT_MEM_BASE + EXT_SIZE - 2, EXT_MEM_BASE + EXT_SIZE,
                  id="past-the-end"),
     pytest.param(0x0100_0000_0000_0000, 0x0100_0000_0000_0000,
                  id="far-above")],
)
def test_a_binary_load_outside_the_map_writes_nothing(addr, first_outside):
    system = _system()
    before = (bytes(system._shared_mem), bytes(system._ext_mem))
    with pytest.raises(ValueError) as error:
        system.load_binary(addr, b"xyzw")
    assert f"at {first_outside:#018x}" in str(error.value)
    assert "RAM [0x0, 0x100000), external [0x100000, 0x101000)" in str(
        error.value)
    assert (bytes(system._shared_mem), bytes(system._ext_mem)) == before


def test_raw_access_outside_the_map_raises():
    system = _system()
    with pytest.raises(ValueError, match="outside the memory map"):
        system._raw_mem_read(0x0100_0000_0000_0000)
    with pytest.raises(ValueError, match="outside the memory map"):
        system._raw_mem_write(EXT_MEM_BASE + EXT_SIZE, 1)
    system._raw_mem_write(RAM - 1, 0x5A)
    assert system._raw_mem_read(RAM - 1) == 0x5A


@pytest.mark.parametrize("cls", [PythonMegapad64, NativeMegapad64])
def test_load_bytes_outside_the_map_writes_nothing(cls):
    cpu = cls(mem_size=64)
    cpu.load_bytes(60, b"1234")
    with pytest.raises(ValueError, match="at 0x0000000000000040"):
        cpu.load_bytes(62, b"5678")
    assert bytes(cpu.mem[60:64]) == b"1234"
    # A host load is no guest access: it leaves the trap address alone.
    assert cpu.trap_addr == 0


def test_the_tile_self_test_needs_its_scratch_in_bank_0():
    cpu = PythonMegapad64(mem_size=RAM)
    cpu.csr_write(CSR_TILE_SELFTEST, 1)
    assert cpu.tile_selftest == 2
    small = PythonMegapad64(mem_size=0x1000)
    with pytest.raises(ValueError, match="tile self-test scratch"):
        small.csr_write(CSR_TILE_SELFTEST, 1)


def test_the_disassembler_shows_bytes_beyond_memory_as_unmapped():
    assert disasm_one(b"\x01", 0, 1) == ("NOP", 1)
    assert disasm_one(b"\x01", 5, 1) == ("(unmapped at 0x5)", 1)
