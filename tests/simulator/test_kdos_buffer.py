"""Contiguous unchanged-source acceptance for the KDOS Buffer subsystem."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from simulator.errors import ForthAbort, SourceError
from simulator.memory import HBW_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_idle import _load_idle
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_arena import _load_arena
from tests.simulator.test_kdos_hbw import CANONICAL_HBW_SIZE
from tests.simulator.test_kdos_x25519 import _execute
from tests.simulator.test_kdos_xmem import CANONICAL_EXTERNAL_SIZE, _pointer


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-buffer-2797-2985.f"

FIRST_LINE = 2797
LAST_LINE = 2985
SLICE_SHA256 = (
    "eb4d6d1bf072f854c667e86f428f49370bde4cd06e4770bd095d5f549906b2f1"
)
SLICE_GIT_BLOB = "302d66b01d699f840e9195853c3d991a9c90c94b"
DEFINITIONS = (
    b"BUF-COUNT",
    b"BUF-HEAD",
    b"(BUF-REG)",
    b"BUF-NTH",
    b"B.TYPE",
    b"B.WIDTH",
    b"B.LEN",
    b"B.DATA",
    b"BDESC",
    b"BUFFER",
    b"HBW-BUFFER",
    b"XBUFFER",
    b"B.BYTES",
    b"B.TILES",
    b"B.FILL",
    b"B.ZERO",
    b"B.INFO",
    b"B.PREVIEW",
    b"BUFFERS",
    b"AB-AR",
    b"AB-DESC",
    b"(AR-UNREG-BUFS)",
    b"ARENA-DESTROY",
    b"ARENA-BUFFER",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 7_191
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == (
        b": IDLE  ( -- )  [ 0 C, ] ;  \\ IDL opcode \xe2\x80\x94 "
        b"yield CPU until next interrupt\n"
    )
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_buffer(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_buffer(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    chained = _load_arena(runtime)
    _load_idle(chained)
    return _evaluate_buffer(chained)


@pytest.fixture
def loaded_buffer() -> MegaForthRuntime:
    return _load_buffer()


def _descriptor(runtime: MegaForthRuntime, address: int) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(address + offset) for offset in range(0, 32, 8)
    )


def test_buffer_slice_is_exact_and_publishes_the_complete_ledger(
    loaded_buffer: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer
    assert len(DEFINITIONS) == 24
    for name in DEFINITIONS:
        assert runtime.find(name) is not None
    assert sum(word.name == b"ARENA-DESTROY" for word in runtime.dictionary.words) == 2
    assert _pointer(runtime, "BUF-COUNT") == 0
    assert _pointer(runtime, "BUF-HEAD") == 0
    assert _pointer(runtime, "BDESC") == 0
    assert _pointer(runtime, "AB-AR") == 0
    assert _pointer(runtime, "AB-DESC") == 0
    assert runtime.uart_output == b""


def test_next_contiguous_definition_stops_at_the_first_tile_mmio_word(
    loaded_buffer: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:3000])
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    with pytest.raises(SourceError, match="unknown word b'TMODE!'") as caught:
        runtime.evaluate(
            next_source,
            source_name=f"kdos.f@{MEGAPAD_REVISION}:2986-3000",
        )

    assert next_source.count(b"\n") == 15
    assert caught.value.location.line == 15
    assert caught.value.location.column == 6
    assert runtime.find("B.SUM") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_bank0_buffer_layout_registry_and_byte_operations(
    loaded_buffer: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer
    descriptor = runtime.dictionary.here
    result = runtime.evaluate(
        b"0 1 70 BUFFER BANK-BUF",
        source_name="bank-buffer",
    )

    assert tuple(word.name for word in result.definitions) == (b"BANK-BUF",)
    assert _execute(runtime, "BANK-BUF") == (descriptor,)
    data = _execute(runtime, "B.DATA", descriptor)[0]
    assert data == (descriptor + 32 + 63) & -64
    assert _descriptor(runtime, descriptor) == (0, 1, 70, data)
    assert _execute(runtime, "B.TYPE", descriptor) == (0,)
    assert _execute(runtime, "B.WIDTH", descriptor) == (1,)
    assert _execute(runtime, "B.LEN", descriptor) == (70,)
    assert _execute(runtime, "B.BYTES", descriptor) == (70,)
    assert _execute(runtime, "B.TILES", descriptor) == (2,)

    link = _pointer(runtime, "BUF-HEAD")
    assert _pointer(runtime, "BUF-COUNT") == 1
    assert runtime.memory.read64(link) == descriptor
    assert runtime.memory.read64(link + 8) == 0
    assert link == data + 70
    assert _execute(runtime, "BUF-NTH", 0) == (descriptor,)

    assert _execute(runtime, "B.FILL", 0xA5, descriptor) == ()
    assert runtime.memory.read_bytes(data, 70) == bytes((0xA5,)) * 70
    assert runtime.memory.read64(link) == descriptor
    assert _execute(runtime, "B.ZERO", descriptor) == ()
    assert runtime.memory.read_bytes(data, 70) == bytes(70)
    assert runtime.memory.read64(link) == descriptor


def test_hbw_and_xmem_constructors_route_data_but_keep_descriptors_registered() -> None:
    runtime = _load_buffer(
        MegaForthRuntime(
            memory=create_one_core_address_space(
                external_size=CANONICAL_EXTERNAL_SIZE,
                hbw_size=CANONICAL_HBW_SIZE,
            )
        )
    )
    hbw_before = _pointer(runtime, "HBW-HERE")
    xmem_before = _pointer(runtime, "XMEM-HERE")
    result = runtime.evaluate(
        b"2 8 9 HBW-BUFFER HB-BUF\n3 4 17 XBUFFER X-BUF\n",
        source_name="routed-buffers",
    )

    assert tuple(word.name for word in result.definitions) == (b"HB-BUF", b"X-BUF")
    hb_descriptor = _execute(runtime, "HB-BUF")[0]
    x_descriptor = _execute(runtime, "X-BUF")[0]
    hb_data = (hbw_before + 63) & -64
    x_data = (xmem_before + 63) & -64

    assert hbw_before == HBW_BASE
    assert _descriptor(runtime, hb_descriptor) == (2, 8, 9, hb_data)
    assert _descriptor(runtime, x_descriptor) == (3, 4, 17, x_data)
    assert _pointer(runtime, "HBW-HERE") == hb_data + 72
    assert _pointer(runtime, "XMEM-HERE") == x_data + 80
    assert _pointer(runtime, "BUF-COUNT") == 2
    assert _execute(runtime, "BUF-NTH", 0) == (x_descriptor,)
    assert _execute(runtime, "BUF-NTH", 1) == (hb_descriptor,)

    head = _pointer(runtime, "BUF-HEAD")
    previous = runtime.memory.read64(head + 8)
    assert runtime.memory.read64(head) == x_descriptor
    assert runtime.memory.read64(previous) == hb_descriptor
    assert runtime.memory.read64(previous + 8) == 0


def test_xbuffer_records_the_bump_frontier_even_when_allocation_reuses_a_block(
    loaded_buffer: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer
    recycled = _execute(runtime, "XMEM-ALLOT", 64)[0]
    assert _execute(runtime, "XMEM-FREE-BLOCK", recycled, 64) == ()
    bump_frontier = _pointer(runtime, "XMEM-HERE")
    assert bump_frontier == recycled + 64
    assert _pointer(runtime, "XMEM-FL") == recycled

    runtime.evaluate(b"0 1 64 XBUFFER RECYCLED-X", source_name="recycled-xbuffer")
    descriptor = _execute(runtime, "RECYCLED-X")[0]

    # Unchanged KDOS stores XMEM-HERE before XMEM-ALLOT, then drops the
    # allocator's returned address.  A free-list hit therefore leaves this
    # descriptor pointing at the bump frontier instead of the reused block.
    assert _descriptor(runtime, descriptor) == (0, 1, 64, bump_frontier)
    assert _pointer(runtime, "XMEM-HERE") == bump_frontier
    assert _pointer(runtime, "XMEM-FL") == 0
    assert _execute(runtime, "B.DATA", descriptor) == (bump_frontier,)
    assert recycled != bump_frontier
    assert _execute(runtime, "XMEM-ALLOT", 16) == (bump_frontier,)


def test_hbw_constructor_failure_leaves_its_partial_descriptor_published() -> None:
    runtime = _load_buffer(
        MegaForthRuntime(
            memory=create_one_core_address_space(hbw_size=64),
        )
    )
    descriptor = runtime.dictionary.here
    hbw_before = _pointer(runtime, "HBW-HERE")

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"0 8 9 HBW-BUFFER TOO-LARGE",
            source_name="partial-hbw-buffer",
        )

    assert runtime.drain_uart_output() == b"HBW overflow"
    assert runtime.dictionary.here == descriptor + 32
    assert _descriptor(runtime, descriptor) == (0, 8, 9, hbw_before)
    assert _pointer(runtime, "BDESC") == descriptor
    assert _pointer(runtime, "HBW-HERE") == hbw_before
    assert _pointer(runtime, "BUF-COUNT") == 0
    assert _pointer(runtime, "BUF-HEAD") == 0
    assert runtime.find("TOO-LARGE") is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_arena_buffer_uses_eight_byte_alignment_and_destroy_only_unlinks_it(
    loaded_buffer: MegaForthRuntime,
) -> None:
    runtime = loaded_buffer
    runtime.evaluate(b"0 1 8 BUFFER ORDINARY-BUF", source_name="ordinary-buffer")
    ordinary = _execute(runtime, "ORDINARY-BUF")[0]
    ordinary_link = _pointer(runtime, "BUF-HEAD")

    arena, status = _execute(runtime, "ARENA-NEW", 128, 1)
    assert status == 0
    arena_base = _descriptor(runtime, arena)[0]
    runtime.evaluate(
        f"2 8 3 {arena} ARENA-BUFFER ARENA-BUF-1".encode("ascii"),
        source_name="arena-buffer-1",
    )
    descriptor = _execute(runtime, "ARENA-BUF-1")[0]
    data = _execute(runtime, "B.DATA", descriptor)[0]
    first_arena_link = _pointer(runtime, "BUF-HEAD")

    assert descriptor == arena_base
    assert data == arena_base + 32
    assert data % 8 == 0
    assert data % 64 == 32
    assert _descriptor(runtime, descriptor) == (2, 8, 3, data)
    assert _execute(runtime, "ARENA-USED", arena) == (56,)
    assert _pointer(runtime, "BUF-COUNT") == 2
    assert runtime.memory.read64(first_arena_link) == descriptor
    assert runtime.memory.read64(first_arena_link + 8) == ordinary_link

    runtime.evaluate(b"0 1 8 BUFFER ORDINARY-BUF-2", source_name="ordinary-buffer-2")
    ordinary_2 = _execute(runtime, "ORDINARY-BUF-2")[0]
    ordinary_2_link = _pointer(runtime, "BUF-HEAD")
    runtime.evaluate(
        f"1 8 1 {arena} ARENA-BUFFER ARENA-BUF-2".encode("ascii"),
        source_name="arena-buffer-2",
    )
    descriptor_2 = _execute(runtime, "ARENA-BUF-2")[0]
    second_arena_link = _pointer(runtime, "BUF-HEAD")
    assert _execute(runtime, "ARENA-USED", arena) == (96,)
    assert _pointer(runtime, "BUF-COUNT") == 4
    assert runtime.memory.read64(second_arena_link + 8) == ordinary_2_link
    assert runtime.memory.read64(ordinary_2_link + 8) == first_arena_link
    assert runtime.memory.read64(first_arena_link + 8) == ordinary_link

    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _pointer(runtime, "BUF-COUNT") == 2
    assert _pointer(runtime, "BUF-HEAD") == ordinary_2_link
    assert runtime.memory.read64(ordinary_2_link + 8) == ordinary_link
    assert _execute(runtime, "BUF-NTH", 0) == (ordinary_2,)
    assert _execute(runtime, "BUF-NTH", 1) == (ordinary,)
    assert runtime.memory.read64(second_arena_link) == descriptor_2
    assert runtime.memory.read64(second_arena_link + 8) == ordinary_2_link
    assert runtime.memory.read64(first_arena_link) == descriptor
    assert runtime.memory.read64(first_arena_link + 8) == ordinary_link
    assert runtime.find("ARENA-BUF-1") is not None
    assert runtime.find("ARENA-BUF-2") is not None
    assert _execute(runtime, "ARENA-BUF-1") == (descriptor,)
    assert _execute(runtime, "ARENA-BUF-2") == (descriptor_2,)


def test_info_preview_and_registry_listing_render_current_base_and_fixed_tile() -> None:
    runtime = _load_buffer(
        MegaForthRuntime(
            memory=create_one_core_address_space(
                external_size=CANONICAL_EXTERNAL_SIZE,
                hbw_size=CANONICAL_HBW_SIZE,
            )
        )
    )
    runtime.evaluate(b"0 1 8 HBW-BUFFER DISPLAY-BUF", source_name="display-buffer")
    descriptor = _execute(runtime, "DISPLAY-BUF")[0]
    data = _execute(runtime, "B.DATA", descriptor)[0]
    runtime.memory.write_bytes(data, bytes(range(64)))

    assert _execute(runtime, "B.INFO", descriptor) == ()
    assert runtime.drain_uart_output() == (
        f" [buf  t=0   w=1   n=8   tiles=1   @{data}  ]\r\n".encode("ascii")
    )

    assert _execute(runtime, "B.PREVIEW", descriptor) == ()
    expected_preview = b"".join(
        b"".join(
            f"{value} ".encode("ascii") for value in range(start, start + 16)
        )
        + b"\r\n"
        for start in range(0, 64, 16)
    )
    assert runtime.drain_uart_output() == expected_preview

    assert _execute(runtime, "BUFFERS") == ()
    assert runtime.drain_uart_output() == (
        b" --- Buffers (1  ) ---\r\n"
        + f"0  :  [buf  t=0   w=1   n=8   tiles=1   @{data}  ]\r\n".encode(
            "ascii"
        )
    )
