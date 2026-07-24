"""Transactional and host-safe accelerator-hook behavior."""

from __future__ import annotations

import pytest

from accel_wrapper import HaltError, Megapad64
from asm import assemble


MASK64 = (1 << 64) - 1
HOOK_TARGET = 0x600
DATA_STACK = 0x7000
RETURN_STACK = 0x7800
MEM_SIZE = 0x10000
MMIO_START = 0xFFFF_FF00_0000_0000


def _make_hook_cpu(
    hook_id: int,
    popped_cells: list[int],
    *,
    source: str = "call.l r4\nhalt",
) -> Megapad64:
    cpu = Megapad64(mem_size=MEM_SIZE)
    cpu.load_bytes(0, assemble(source))
    cpu.pc = 0
    cpu.regs[4] = HOOK_TARGET
    cpu.regs[14] = DATA_STACK
    cpu.regs[15] = RETURN_STACK
    for index, value in enumerate(popped_cells):
        start = DATA_STACK + index * 8
        cpu.mem[start:start + 8] = (value & MASK64).to_bytes(8, "little")
    cpu.register_accel_hook(HOOK_TARGET, hook_id)
    return cpu


def _assert_fast_path(
    cpu: Megapad64,
    *,
    cells: int,
    expected_cycles: int,
) -> None:
    assert cpu.step() == expected_cycles
    assert cpu.pc == 2
    assert cpu.regs[14] == DATA_STACK + cells * 8
    assert cpu.regs[15] == RETURN_STACK
    assert cpu.cycle_count == expected_cycles


def _assert_declined(
    cpu: Megapad64,
    *,
    assert_memory_unchanged: bool = False,
) -> None:
    if assert_memory_unchanged:
        memory_before = bytes(cpu.mem)

    assert cpu.step() == 2
    assert cpu.pc == HOOK_TARGET
    assert cpu.regs[14] == DATA_STACK
    assert cpu.regs[15] == RETURN_STACK - 8
    assert cpu.mem[RETURN_STACK - 8:RETURN_STACK] == (2).to_bytes(8, "little")
    assert cpu.cycle_count == 2

    if assert_memory_unchanged:
        # Ignore the architectural CALL.L return-stack push.
        memory_after = bytearray(cpu.mem)
        memory_after[RETURN_STACK - 8:RETURN_STACK] = (
            memory_before[RETURN_STACK - 8:RETURN_STACK]
        )
        assert bytes(memory_after) == memory_before


def test_rect_fill_writes_odd_rgb565_addresses_as_little_endian_bytes():
    destination = 0x1001
    cpu = _make_hook_cpu(
        1,
        # RECT-FILL pop order: color, h, w, stride, address.
        [0xBEEF, 1, 2, 4, destination],
    )
    cpu.mem[destination - 1:destination + 5] = b"\xA5" * 6

    _assert_fast_path(cpu, cells=5, expected_cycles=21)

    assert cpu.mem[destination - 1:destination + 5] == (
        b"\xA5\xEF\xBE\xEF\xBE\xA5"
    )


def test_blit_glyph_writes_selected_odd_pixels_without_typed_stores():
    glyph_address = 0x1800
    pixel_address = 0x2001
    stride = 32
    foreground = 0x1234
    cpu = _make_hook_cpu(
        2,
        # BLIT-GLYPH pop order: foreground, stride, pixels, glyph.
        [foreground, stride, pixel_address, glyph_address],
    )
    cpu.mem[glyph_address:glyph_address + 8] = b"\xA1" + b"\x00" * 7
    for row in range(8):
        start = pixel_address + row * stride
        cpu.mem[start - 1:start + 17] = b"\xA5" * 18

    _assert_fast_path(cpu, cells=4, expected_cycles=121)

    row = bytearray(b"\xA5" * 16)
    for column in (0, 2, 7):
        row[column * 2:column * 2 + 2] = b"\x34\x12"
    assert cpu.mem[pixel_address:pixel_address + 16] == row
    assert cpu.mem[pixel_address - 1] == 0xA5
    assert cpu.mem[pixel_address + 16] == 0xA5
    for other_row in range(1, 8):
        start = pixel_address + other_row * stride
        assert cpu.mem[start:start + 16] == b"\xA5" * 16


def test_blit_glyph_reads_font_bytes_from_hbw_routing():
    glyph_address = 0x9000
    pixel_address = 0x2000
    cpu = _make_hook_cpu(
        2,
        [0xF800, 16, pixel_address, glyph_address],
    )
    hbw_font = bytearray(b"\x80" + b"\x00" * 7)
    cpu.attach_hbw(hbw_font, glyph_address, len(hbw_font))

    _assert_fast_path(cpu, cells=4, expected_cycles=121)

    assert cpu.mem[pixel_address:pixel_address + 2] == b"\x00\xF8"
    assert cpu.mem[pixel_address + 2:pixel_address + 16] == b"\x00" * 14


def test_blit_string_writes_odd_rgb565_pixels_without_typed_stores():
    chars_address = 0x2800
    font_base = 0x3000
    pixel_address = 0x4001
    stride = 32
    foreground = 0x07E0
    glyph_address = font_base + (ord("A") - 0x20) * 8
    cpu = _make_hook_cpu(
        4,
        # BLIT-STRING pop order: font, foreground, stride, pixels, len, chars.
        [font_base, foreground, stride, pixel_address, 1, chars_address],
    )
    cpu.mem[chars_address] = ord("A")
    cpu.mem[glyph_address:glyph_address + 8] = b"\x81" + b"\x00" * 7
    for row in range(8):
        start = pixel_address + row * stride
        cpu.mem[start - 1:start + 17] = b"\x5A" * 18

    _assert_fast_path(cpu, cells=6, expected_cycles=131)

    row = bytearray(b"\x5A" * 16)
    row[0:2] = b"\xE0\x07"
    row[14:16] = b"\xE0\x07"
    assert cpu.mem[pixel_address:pixel_address + 16] == row
    assert cpu.mem[pixel_address - 1] == 0x5A
    assert cpu.mem[pixel_address + 16] == 0x5A


def test_vram_copy_accepts_direct_nonoverlapping_rows_and_counts_safely():
    source = 0x1000
    destination = 0x1100
    stride = 8
    cpu = _make_hook_cpu(
        3,
        # VRAM-COPY pop order: h, w, stride, destination, source.
        [2, 4, stride, destination, source],
    )
    cpu.mem[source:source + 4] = b"ABCD"
    cpu.mem[source + stride:source + stride + 4] = b"WXYZ"

    _assert_fast_path(cpu, cells=5, expected_cycles=35)

    assert cpu.mem[destination:destination + 4] == b"ABCD"
    assert cpu.mem[destination + stride:destination + stride + 4] == b"WXYZ"


@pytest.mark.parametrize(
    ("hook_id", "popped_cells"),
    (
        pytest.param(
            1,
            [0xFFFF, 1, (1 << 63) - 1, 2, 0x1000],
            id="rect-cycle-overflow",
        ),
        pytest.param(
            1,
            [0xFFFF, 1, 1 << 63, 2, 0x1000],
            id="rect-unsigned-high-bit-count",
        ),
        pytest.param(
            1,
            [0xFFFF, 1, 4097, 2, 0x1000],
            id="rect-over-geometry-budget",
        ),
        pytest.param(
            3,
            [1, (1 << 63) - 1, 4, 0x9000, 0x8000],
            id="copy-cycle-overflow",
        ),
        pytest.param(
            4,
            [0x3000, 0xFFFF, 32, 0x4000, 4097, 0x2800],
            id="string-over-geometry-budget",
        ),
    ),
)
def test_oversized_geometry_declines_without_stack_or_memory_mutation(
    hook_id: int,
    popped_cells: list[int],
):
    cpu = _make_hook_cpu(hook_id, popped_cells)

    _assert_declined(cpu, assert_memory_unchanged=True)


def test_aperture_crossing_halfword_declines_instead_of_clamping():
    destination = MEM_SIZE - 1
    cpu = _make_hook_cpu(1, [0xCAFE, 1, 1, 2, destination])

    _assert_declined(cpu, assert_memory_unchanged=True)


def test_higher_priority_aperture_inside_row_declines_transactionally():
    destination = 0x1000
    cpu = _make_hook_cpu(1, [0xCAFE, 1, 2, 4, destination])
    vram = bytearray(b"\xA5\xA5")
    cpu.attach_vram(vram, destination + 2, len(vram))

    _assert_declined(cpu, assert_memory_unchanged=True)

    assert vram == b"\xA5\xA5"


def test_mmio_destination_declines_even_when_backed_by_direct_vram():
    cpu = _make_hook_cpu(1, [0xCAFE, 1, 1, 2, MMIO_START])
    vram = bytearray(b"\xA5\xA5")
    cpu.attach_vram(vram, MMIO_START, len(vram))

    _assert_declined(cpu, assert_memory_unchanged=True)

    assert vram == b"\xA5\xA5"


def test_unknown_hook_id_declines_to_ordinary_call():
    cpu = _make_hook_cpu(99, [])

    _assert_declined(cpu, assert_memory_unchanged=True)


def test_user_mode_hook_declines_instead_of_bypassing_mpu_routing():
    cpu = _make_hook_cpu(1, [0xCAFE, 1, 1, 2, 0x1000])
    cpu.priv_level = 1
    cpu.mpu_base = 0
    cpu.mpu_limit = MEM_SIZE

    _assert_declined(cpu, assert_memory_unchanged=True)


def test_partial_row_overlap_falls_through_to_bios_ordered_copy():
    source = 0x1200
    destination = source + 1
    guest_copy = f"""
        call.l r4
        halt
    .org {HOOK_TARGET}
        ldn r10, r14
        addi r14, 8
        ldn r9, r14
        addi r14, 8
        ldn r7, r14
        addi r14, 8
        ldn r1, r14
        addi r14, 8
        ldn r0, r14
        addi r14, 8
    copy_byte:
        cmpi r9, 0
        breq copy_done
        ld.b r2, r0
        st.b r1, r2
        inc r0
        inc r1
        dec r9
        br copy_byte
    copy_done:
        ret.l
    """
    cpu = _make_hook_cpu(
        3,
        [1, 4, 4, destination, source],
        source=guest_copy,
    )
    cpu.mem[source:source + 5] = bytes((1, 2, 3, 4, 5))

    assert cpu.step() == 2
    assert cpu.pc == HOOK_TARGET
    assert cpu.regs[14] == DATA_STACK
    assert cpu.mem[source:source + 5] == bytes((1, 2, 3, 4, 5))

    with pytest.raises(HaltError):
        while True:
            cpu.step()

    # The BIOS byte loop copies left-to-right, so each write becomes the next
    # read.  memmove would instead have produced 1,1,2,3,4.
    assert cpu.mem[source:source + 5] == bytes((1, 1, 1, 1, 1))


def test_hook_registration_fails_fast_from_execution_callback():
    cpu = Megapad64(mem_size=64)
    cpu.load_bytes(0, assemble("out1"))
    cpu.pc = 0
    rejected: list[str] = []

    def register_during_output(_port: int, _value: int) -> None:
        with pytest.raises(
            RuntimeError,
            match=(
                "^CPUState accelerator hooks cannot be changed "
                "while CPUState is in use$"
            ),
        ):
            cpu.register_accel_hook(HOOK_TARGET, 1)
        rejected.append("registration")

    cpu.on_output = register_during_output

    assert cpu.step() == 1
    assert rejected == ["registration"]
    assert cpu._cs.accel_hook_count == 0
