"""Full-core strict-system FP qualification, including real BIOS word bodies.

These are bounded architectural checks, not solver or throughput benchmarks.
They exercise the shared-clock scheduler separately from standalone CPU.step.
"""

from __future__ import annotations

from pathlib import Path

import pytest

pytest.importorskip("_mp64_accel")

from asm import assemble  # noqa: E402
from emulator.megapad64 import CSR_FPCSR, CSR_ICACHE_CTRL  # noqa: E402
from emulator.session import MachineSession  # noqa: E402
from emulator.system import MegapadSystem  # noqa: E402
from shared import scalar_fp  # noqa: E402
from tests.test_native_cycle_execution import _prime_instruction_cache, _system  # noqa: E402


MASK64 = (1 << 64) - 1
NX, NV = 1 << 4, 1 << 8
ONE, TWO, THREE = 0x3FF0000000000000, 0x4000000000000000, 0x4008000000000000
THIRD, ROOT_TWO = 0x3FD5555555555555, 0x3FF6A09E667F3BCD


@pytest.fixture
def systems(monkeypatch):
    owned = []

    def create(code, *, cores=4, worker_count=1, cold=False, ram_size=None):
        if ram_size is None:
            system = _system(code, cores=cores, worker_count=worker_count,
                             cold_instruction_cache=cold)
        else:
            system = MegapadSystem(ram_size=ram_size, num_cores=cores,
                                   num_clusters=0, hbw_size=0, ext_mem_size=0,
                                   vram_size=0, worker_count=worker_count)
            system.load_binary(0, code)
            system.boot(entry=0)
            if not cold:
                _prime_instruction_cache(system, 0, len(code))
        owned.append(system)

        def forbidden(*_args, **_kwargs):
            pytest.fail("strict full-core FP entered the Python ISA fallback")

        for cpu in system.cores:
            monkeypatch.setattr(cpu, "_step_python_fallback", forbidden)
            monkeypatch.setattr(cpu, "_step_python_fallback_in_memory_scope", forbidden)
        return system

    yield create
    for system in reversed(owned):
        MachineSession(system).close()


def _operation_source(shape, op):
    if shape == "fetch":
        return "csrr r1, 0x0D"
    if shape == "store":
        return "csrw 0x0D, r1"
    fmt = "d" if op >> 6 else "s"
    code = op & 63
    simple = {0: "fadd", 1: "fsub", 2: "fmul", 3: "fdiv", 4: "fsqrt",
              5: "fmin", 6: "fmax", 7: "fma", 0x11: "feq", 0x12: "flt",
              0x13: "fle", 0x14: "fclass"}
    if code in simple:
        mnemonic = f"{simple[code]}.{fmt}"
    elif 0x20 <= code < 0x28:
        mnemonic = f"frnd.{fmt}.{('rne', 'rtz', 'rdn', 'rup', 'rmm')[code & 7]}"
    elif 0x28 <= code < 0x38:
        integer = "l" if code < 0x30 else "lu"
        mnemonic = f"fcvt.{integer}.{fmt}.rtz"
    else:
        conversions = {
            0x38: (fmt, "l"), 0x39: (fmt, "lu"),
            0x3A: (fmt, "s" if fmt == "d" else "d"),
            0x3B: ("h", fmt), 0x3C: (fmt, "h"),
            0x3D: ("b", fmt), 0x3E: (fmt, "b"),
        }
        target, source = conversions[code]
        mnemonic = f"fcvt.{target}.{source}"
    return f"{mnemonic} r1, r6" + (", r7" if shape == "fma" else "")


def _start(cpu, address):
    cpu.pc = address
    cpu.halted = False
    cpu.idle = False
    cpu.flag_i = False
    cpu.perf_enable = 1


@pytest.mark.parametrize("cores", [1, 2, 4])
@pytest.mark.parametrize("cold", [False, True], ids=["warm-fetch", "cold-fetch"])
def test_all_52_bios_operations_retire_in_strict_full_core_system(systems, cores, cold):
    assert len(scalar_fp.BIOS_WORDS) == 52
    instructions = [assemble(_operation_source(shape, op))
                    for _name, shape, op in scalar_fp.BIOS_WORDS]
    # Each independent operation has its own stop, so no host mutation can
    # happen while a previous instruction or bus request remains suspended.
    code = b"".join(bytes(instruction) + b"\x02" + bytes(16 - len(instruction) - 1)
                    for instruction in instructions)
    system = systems(code, cores=cores, worker_count=cores, cold=cold)
    for index, ((name, shape, op), instruction) in enumerate(zip(scalar_fp.BIOS_WORDS, instructions)):
        expected = []
        before_cycles = []
        before_performance = []
        for lane, cpu in enumerate(system.cores):
            _start(cpu, index * 16)
            if cold:
                cpu.csr_write(CSR_ICACHE_CTRL, 3)
            initial_csr = lane | NV
            cpu.csr_write(CSR_FPCSR, initial_csr)
            left = 0x3FF000003F800001 + lane
            right = 0x4008000040400000 + lane
            addend = 0xBFF00000BF800000 + lane
            if shape == "fetch":
                cpu.regs[1] = MASK64
                expected.append((initial_csr, initial_csr))
            elif shape == "store":
                cpu.regs[1] = MASK64 - lane
                expected.append((MASK64 - lane, (MASK64 - lane) & 0x1F7))
            else:
                rd, rs, rt = ((addend, left, right) if shape == "fma" else
                              (right, right, 0) if shape == "unary" else
                              (left, right, 0))
                cpu.regs[1], cpu.regs[6], cpu.regs[7] = rd, rs, rt
                outcome = scalar_fp.execute(op, rd, rs, rt, initial_csr)
                expected.append((outcome.value, initial_csr | outcome.flags))
                # The assembler, BIOS metadata and native adapter must agree
                # on the actual architectural operation byte.
                assert bytes(instruction[:2]) == bytes((0xFC, op)), name
            before_cycles.append(cpu.cycle_count)
            before_performance.append(cpu.perf_cycles)

        result = system.run_cycle_batch(512, max_instructions=cores * 2 + 1)
        assert system.all_halted, name
        assert not system._native_system.cycle_execution_pending, name
        assert result.native_continuations == 0, name
        assert result.per_core_instructions == (2,) * cores, name
        base_cycles = 2 if op is None else scalar_fp.extra_cycles(op) + 2
        for lane, cpu in enumerate(system.cores):
            assert (cpu.regs[1], cpu.csr_read(CSR_FPCSR)) == expected[lane], (name, lane)
            assert cpu.pc == index * 16 + len(instruction) + 1, (name, lane)
            elapsed = cpu.cycle_count - before_cycles[lane]
            assert elapsed == result.per_core_cycles[lane], (name, lane)
            assert cpu.perf_cycles - before_performance[lane] == elapsed, (name, lane)
            assert elapsed >= base_cycles, (name, lane)
            if not cold:
                assert elapsed == base_cycles, (name, lane)


def _snapshot(system):
    return (
        tuple((tuple(cpu.regs), cpu.flags_pack(), cpu.csr_read(CSR_FPCSR),
               cpu.cycle_count, cpu.perf_cycles, cpu.perf_stalls,
               cpu.icache_hits, cpu.icache_misses, cpu.halted)
              for cpu in system.cores),
        bytes(system.cpu.mem[0x400:0x480]),
        system._native_system.system_cycles,
    )


@pytest.mark.parametrize("workers", [1, 2, 4])
@pytest.mark.parametrize("cold", [False, True], ids=["warm-fetch", "cold-fetch"])
def test_four_core_sliced_fp_with_store_prefix_matches_whole_system(systems, workers, cold):
    code = assemble("str r9, r8\nfdiv.d r1, r6\nstr r10, r1\n"
                    "fsqrt.d r11, r12\nstr r13, r11\nhalt")
    sliced = systems(code, worker_count=workers, cold=cold)
    whole = systems(code, worker_count=workers, cold=cold)
    for system in (sliced, whole):
        for lane, cpu in enumerate(system.cores):
            _start(cpu, 0)
            cpu.csr_write(CSR_FPCSR, 0)
            cpu.regs[1], cpu.regs[6] = ONE, THREE
            cpu.regs[11], cpu.regs[12] = 0, TWO
            cpu.regs[8], cpu.regs[9] = 0xA500 + lane, 0x400 + lane * 32
            cpu.regs[10], cpu.regs[13] = cpu.regs[9] + 8, cpu.regs[9] + 16

    complete = whole.run_cycle_batch(1024, max_instructions=64)
    assert whole.all_halted
    assert complete.native_continuations == 0
    instructions = [0] * 4
    cycles = [0] * 4
    saw_suspension = False
    saw_published_prefix_before_fp = False
    for index in range(256):
        result = sliced.run_cycle_batch((1, 2, 3, 5)[index % 4], max_instructions=64)
        assert result.native_continuations == 0
        saw_suspension |= sliced._native_system.cycle_execution_pending
        for lane, cpu in enumerate(sliced.cores):
            instructions[lane] += result.per_core_instructions[lane]
            cycles[lane] += result.per_core_cycles[lane]
            marker = int.from_bytes(cpu.mem[0x400 + lane * 32:0x408 + lane * 32], "little")
            saw_published_prefix_before_fp |= marker == 0xA500 + lane and cpu.regs[1] == ONE
        if sliced.all_halted:
            break
    else:
        pytest.fail("strict sliced FP program exceeded its bounded cycle slices")
    assert saw_suspension
    assert saw_published_prefix_before_fp
    assert tuple(instructions) == complete.per_core_instructions == (6,) * 4
    assert tuple(cycles) == complete.per_core_cycles
    assert _snapshot(sliced) == _snapshot(whole)
    for lane, cpu in enumerate(sliced.cores):
        address = 0x400 + lane * 32
        assert tuple(int.from_bytes(cpu.mem[at:at + 8], "little")
                     for at in (address, address + 8, address + 16)) == (0xA500 + lane, THIRD, ROOT_TWO)
        assert cpu.csr_read(CSR_FPCSR) == NX


@pytest.fixture(scope="module")
def assembled_bios():
    labels = {}
    source = Path(__file__).resolve().parents[1] / "bios.asm"
    code = bytes(assemble(source.read_text(), labels_out=labels))
    return code, labels


def test_real_bios_fp_words_on_four_strict_full_cores(systems, assembled_bios):
    """Call the real dictionary code fields; no source boot/solver claim."""

    bios, labels = assembled_bios
    trampoline = (len(bios) + 255) & ~255
    ram_size = max(256 << 10, (trampoline + 0x5000 + 0xFFFF) & ~0xFFFF)
    system = systems(bios, cores=4, worker_count=4, cold=True, ram_size=ram_size)
    cases = (
        ("F64+", "d_f64plus", (0x3FF8000000000000, 0x4004000000000000), 0x4010000000000000, 0),
        ("F64/", "d_f64slash", (ONE, THREE), THIRD, NX),
        ("F64FMA", "d_f64fma", (TWO, THREE, 0x4010000000000000), 0x4024000000000000, 0),
        ("F64SQRT", "d_f64sqrt", (TWO,), ROOT_TWO, NX),
    )
    for name, label, inputs, expected, flags in cases:
        header = labels[label]
        assert bios[header + 8] == len(name)
        assert bios[header + 9:header + 9 + len(name)] == name.encode("ascii")
        entry = header + 9 + len(name)
        system.load_binary(trampoline, assemble(f"ldi64 r11, {entry}\ncall.l r11\nhalt"))
        frontiers = []
        for lane, cpu in enumerate(system.cores):
            _start(cpu, trampoline)
            cpu.csr_write(CSR_ICACHE_CTRL, 3)
            cpu.csr_write(CSR_FPCSR, 0)
            data_empty = ram_size - 0x2000 - lane * 0x100
            return_empty = ram_size - 0x100 - lane * 0x100
            cpu.regs[14] = data_empty - 8 * len(inputs)
            cpu.regs[15] = return_empty
            for index, value in enumerate(reversed(inputs)):
                address = cpu.regs[14] + index * 8
                cpu.mem[address:address + 8] = value.to_bytes(8, "little")
            frontiers.append((data_empty, return_empty))
        result = system.run_cycle_batch(4096, max_instructions=128)
        assert system.all_halted, name
        assert result.native_continuations == 0, name
        assert not system._native_system.cycle_execution_pending, name
        for lane, cpu in enumerate(system.cores):
            data_empty, return_empty = frontiers[lane]
            assert cpu.regs[14] == data_empty - 8, (name, lane)
            assert cpu.regs[15] == return_empty, (name, lane)
            assert int.from_bytes(cpu.mem[data_empty - 8:data_empty], "little") == expected, (name, lane)
            assert cpu.csr_read(CSR_FPCSR) == flags, (name, lane)
