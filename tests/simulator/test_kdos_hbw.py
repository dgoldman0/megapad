"""Contiguous unchanged-source acceptance for the KDOS HBW allocator."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from simulator.errors import ForthAbort
from simulator.memory import HBW_BASE, MASK64, MMIO_BASE
from simulator.platform import (
    SYSINFO_HBW_BASE,
    SYSINFO_HBW_SIZE,
    create_one_core_address_space,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_hybrid import _load_hybrid
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-hbw-2044-2108.f"

FIRST_LINE = 2044
LAST_LINE = 2108
SLICE_SHA256 = (
    "5fc825c8588b85a499ee34e7fc142b8bba7e74d7efb481bde4183c93476444c9"
)
SLICE_GIT_BLOB = "2d9704f542181bbf91eaead01d5b6ea7a1f9cff0"
DEFINITIONS = (
    b"HBW-HERE",
    b"HBW-LIMIT",
    b"HBW-INIT",
    b"HBW-ALLOT",
    b"HBW-ALLOT?",
    b"HBW-TALIGN",
    b"HBW-RESET",
    b"HBW-FREE",
    b".HBW",
)
BIOS_WORDS = (b"HBW-BASE", b"HBW-SIZE")

CANONICAL_HBW_SIZE = 3 * (1 << 20)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2_448
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_hbw(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_hbw(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    if runtime is None:
        memory = create_one_core_address_space(hbw_size=CANONICAL_HBW_SIZE)
        runtime = MegaForthRuntime(memory=memory)
    return _evaluate_hbw(_load_hybrid(runtime))


@pytest.fixture
def loaded_hbw() -> MegaForthRuntime:
    return _load_hbw()


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _pointer(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(_body(runtime, name))


def test_hbw_slice_is_exact_and_initializes_complete_ledger(
    loaded_hbw: MegaForthRuntime,
) -> None:
    assert len(DEFINITIONS) == 9
    for name in DEFINITIONS + BIOS_WORDS:
        assert loaded_hbw.find(name) is not None

    here = loaded_hbw.find("HBW-HERE")
    limit = loaded_hbw.find("HBW-LIMIT")
    assert here is not None
    assert limit is not None
    assert limit.header_address - here.body_address == 8
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE
    assert _pointer(loaded_hbw, "HBW-LIMIT") == (
        HBW_BASE + CANONICAL_HBW_SIZE
    )
    assert _execute(loaded_hbw, "HBW-FREE") == (CANONICAL_HBW_SIZE,)
    assert loaded_hbw.uart_output == b""


def test_hbw_bios_words_read_the_bound_sysinfo_geometry() -> None:
    hbw_size = 0x12_340
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(hbw_size=hbw_size)
    )

    assert runtime.memory.read64(MMIO_BASE + SYSINFO_HBW_BASE) == HBW_BASE
    assert runtime.memory.read64(MMIO_BASE + SYSINFO_HBW_SIZE) == hbw_size
    assert _execute(runtime, "HBW-BASE") == (HBW_BASE,)
    assert _execute(runtime, "HBW-SIZE") == (hbw_size,)


def test_hbw_checked_allocation_alignment_exact_limit_and_failure_atomicity(
    loaded_hbw: MegaForthRuntime,
) -> None:
    assert _execute(loaded_hbw, "HBW-ALLOT", 17) == (HBW_BASE,)
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE + 17

    assert _execute(loaded_hbw, "HBW-TALIGN") == ()
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE + 64

    assert _execute(loaded_hbw, "HBW-ALLOT?", 64) == (HBW_BASE + 64, 0)
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE + 128

    remaining = CANONICAL_HBW_SIZE - 128
    assert _execute(loaded_hbw, "HBW-ALLOT?", remaining) == (
        HBW_BASE + 128,
        0,
    )
    assert _pointer(loaded_hbw, "HBW-HERE") == (
        HBW_BASE + CANONICAL_HBW_SIZE
    )
    assert _execute(loaded_hbw, "HBW-FREE") == (0,)

    assert _execute(loaded_hbw, "HBW-ALLOT?", 1) == (0, MASK64)
    assert _pointer(loaded_hbw, "HBW-HERE") == (
        HBW_BASE + CANONICAL_HBW_SIZE
    )
    assert _execute(loaded_hbw, "HBW-ALLOT?", 0) == (
        HBW_BASE + CANONICAL_HBW_SIZE,
        0,
    )


def test_hbw_aborting_overflow_preserves_pointer_and_clears_task_stack(
    loaded_hbw: MegaForthRuntime,
) -> None:
    assert _execute(loaded_hbw, "HBW-ALLOT", 64) == (HBW_BASE,)
    before = _pointer(loaded_hbw, "HBW-HERE")
    loaded_hbw.main_context.data.push(CANONICAL_HBW_SIZE)

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        loaded_hbw.execute("HBW-ALLOT", step_budget=250_000)

    assert loaded_hbw.drain_uart_output() == b"HBW overflow"
    assert loaded_hbw.main_context.data.snapshot() == ()
    assert loaded_hbw.main_context.returns.snapshot() == ()
    assert _pointer(loaded_hbw, "HBW-HERE") == before


def test_hbw_reset_reclaims_addresses_without_wiping_memory(
    loaded_hbw: MegaForthRuntime,
) -> None:
    assert _execute(loaded_hbw, "HBW-ALLOT", 1) == (HBW_BASE,)
    loaded_hbw.memory.write8(HBW_BASE, 0xA5)
    assert _execute(loaded_hbw, "HBW-TALIGN") == ()
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE + 64

    assert _execute(loaded_hbw, "HBW-RESET") == ()
    assert _pointer(loaded_hbw, "HBW-HERE") == HBW_BASE
    assert _execute(loaded_hbw, "HBW-FREE") == (CANONICAL_HBW_SIZE,)
    assert loaded_hbw.memory.read8(HBW_BASE) == 0xA5


def test_hbw_pointer_is_shared_across_contexts_but_runtime_local(
    loaded_hbw: MegaForthRuntime,
) -> None:
    peer = loaded_hbw.new_context()
    peer.data.push(32)
    loaded_hbw.execute("HBW-ALLOT", context=peer, step_budget=250_000)
    assert peer.data.snapshot() == (HBW_BASE,)
    assert peer.returns.snapshot() == ()
    assert _execute(loaded_hbw, "HBW-ALLOT", 16) == (HBW_BASE + 32,)

    separate = _load_hbw()
    assert _pointer(separate, "HBW-HERE") == HBW_BASE


def test_hbw_status_renders_live_source_owned_counters(
    loaded_hbw: MegaForthRuntime,
) -> None:
    assert _execute(loaded_hbw, "HBW-ALLOT", 80) == (HBW_BASE,)
    assert _execute(loaded_hbw, ".HBW") == ()
    assert loaded_hbw.drain_uart_output() == (
        b" HBW Math RAM:\r\n"
        b"   Base = 4291821568 \r\n"
        b"   Size = 3145728  bytes\r\n"
        b"   Used = 80  bytes\r\n"
        b"   Free = 3145648  bytes\r\n"
    )


def test_absent_hbw_profile_rejects_every_allocation() -> None:
    runtime = _load_hbw(
        MegaForthRuntime(memory=create_one_core_address_space(hbw_size=0))
    )

    assert _execute(runtime, "HBW-BASE") == (0,)
    assert _execute(runtime, "HBW-SIZE") == (0,)
    assert _pointer(runtime, "HBW-HERE") == 0
    assert _pointer(runtime, "HBW-LIMIT") == 0
    assert _execute(runtime, "HBW-ALLOT?", 1) == (0, MASK64)
    assert _execute(runtime, "HBW-ALLOT?", 0) == (0, MASK64)
    runtime.main_context.data.push(0)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("HBW-ALLOT", step_budget=250_000)
    assert runtime.drain_uart_output() == b"HBW unavailable"
    assert _pointer(runtime, "HBW-HERE") == 0
    assert _pointer(runtime, "HBW-LIMIT") == 0
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_hbw_request_wrap_and_unchecked_alignment_discrepancies_are_visible(
) -> None:
    wrapping = _load_hbw(
        MegaForthRuntime(memory=create_one_core_address_space(hbw_size=128))
    )

    assert _execute(wrapping, "HBW-ALLOT?", MASK64) == (HBW_BASE, 0)
    assert _pointer(wrapping, "HBW-HERE") == HBW_BASE - 1
    assert _execute(wrapping, "HBW-FREE") == (129,)

    unaligned = _load_hbw(
        MegaForthRuntime(memory=create_one_core_address_space(hbw_size=65))
    )
    assert _execute(unaligned, "HBW-ALLOT?", 65) == (HBW_BASE, 0)
    assert _execute(unaligned, "HBW-TALIGN") == ()
    assert _pointer(unaligned, "HBW-HERE") == HBW_BASE + 128
    assert _execute(unaligned, "HBW-FREE") == (MASK64 - 62,)
