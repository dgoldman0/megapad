"""Allocation identity is independent of word lookup and executable bytes."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace

import pytest

from shared.cells import u64
from simulator.dictionary import Dictionary
from simulator.memory import EXTERNAL_BASE, SparseAddressSpace
from simulator.runtime import MegaForthRuntime


def _dictionary() -> tuple[Dictionary, SparseAddressSpace]:
    memory = SparseAddressSpace(bank0_size=0x8000, external_size=0x2000)
    return Dictionary(start_address=0x1000, memory=memory), memory


def test_lease_is_exact_initial_body_identity_and_not_a_copyable_capability() -> None:
    dictionary, _ = _dictionary()
    word = dictionary.define("CODE", initial_body=b"initial image")
    lease = dictionary.acquire_body_lease(word)
    initial_limit = dictionary.here

    assert lease.word is word
    assert lease.body_address == word.body_address
    assert lease.body_limit == initial_limit
    assert lease.body_limit - lease.body_address == len(b"initial image")
    assert dictionary.acquire_body_lease(word) is lease
    assert dictionary.is_body_lease_live(lease)
    with pytest.raises(FrozenInstanceError):
        lease.body_limit += 1  # type: ignore[misc]

    for forged in (
        None,
        object(),
        word,
        replace(lease),
        replace(lease, word=None),
        replace(lease, word=object()),
        replace(lease, word=replace(word)),
        replace(lease, body_limit=lease.body_limit + 8),
        replace(lease, allocation_serial=lease.allocation_serial + 1),
    ):
        assert not dictionary.is_body_lease_live(forged)
    with pytest.raises(ValueError, match="exact live word"):
        dictionary.acquire_body_lease(replace(word))
    with pytest.raises(TypeError, match="requires a Word"):
        dictionary.acquire_body_lease(word.xt)  # type: ignore[arg-type]

    foreign, _ = _dictionary()
    foreign_word = foreign.define("CODE", initial_body=b"initial image")
    foreign_lease = foreign.acquire_body_lease(foreign_word)
    assert foreign_word == word
    assert not foreign.is_body_lease_live(lease)
    assert not dictionary.is_body_lease_live(foreign_lease)
    with pytest.raises(ValueError, match="exact live word"):
        dictionary.acquire_body_lease(foreign_word)

    dictionary.comma(123)
    dictionary.allot(16)
    assert dictionary.is_body_lease_live(lease)
    assert dictionary.acquire_body_lease(word).body_limit == initial_limit


def test_empty_initial_body_cannot_lease_later_allot_storage() -> None:
    dictionary, _ = _dictionary()
    word = dictionary.define("EMPTY")
    dictionary.comma(123)
    dictionary.allot(16)

    with pytest.raises(ValueError, match="nonempty initial body"):
        dictionary.acquire_body_lease(word)


def test_unrelated_publication_shadowing_and_raw_stores_preserve_allocation() -> None:
    dictionary, memory = _dictionary()
    word = dictionary.define("CODE", initial_body=b"first image")
    lease = dictionary.acquire_body_lease(word)
    generation = dictionary.execution_generation

    dictionary.define("UNRELATED", initial_body=b"other image")
    shadow = dictionary.define("code", initial_body=b"new image")
    memory.write_bytes(word.body_address, b"other image")

    assert dictionary.execution_generation == generation + 2
    assert dictionary.find("CODE") is shadow
    assert dictionary.resolve(word.xt) is word
    assert memory.read_bytes(word.body_address, 11) == b"other image"
    assert dictionary.is_body_lease_live(lease)
    assert dictionary.acquire_body_lease(word) is lease


@pytest.mark.parametrize("rewind", ("allot", "move_here", "rollback_to"))
def test_partial_body_reclaim_does_not_revive_when_frontier_and_bytes_return(
    rewind: str,
) -> None:
    dictionary, memory = _dictionary()
    retained = dictionary.define("RETAINED", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    word = dictionary.define("CODE", initial_body=b"abcdefgh")
    lease = dictionary.acquire_body_lease(word)
    old_here = dictionary.here
    generation = dictionary.execution_generation
    target = old_here - 1

    if rewind == "allot":
        dictionary.allot(u64(-1))
    elif rewind == "move_here":
        floor, limit = dictionary.active_zone
        dictionary.move_here(target, floor=floor, limit=limit)
    else:
        dictionary.rollback_to(target, word.header_address)

    assert dictionary.here == target
    assert dictionary.resolve(word.xt) is word
    assert dictionary.is_body_lease_live(retained_lease)
    assert not dictionary.is_body_lease_live(lease)
    # Reclaiming the body changes what native plans may call, and a
    # rollback also republishes the definition list.
    assert dictionary.execution_generation == generation + 1 + (rewind == "rollback_to")

    dictionary.allot(1)
    memory.write_bytes(word.body_address, b"abcdefgh")
    assert dictionary.here == old_here
    assert not dictionary.is_body_lease_live(lease)
    with pytest.raises(ValueError, match="no live nonempty initial body"):
        dictionary.acquire_body_lease(word)


def test_reclamation_before_first_acquisition_cannot_mint_a_replacement_lease() -> None:
    dictionary, _ = _dictionary()
    word = dictionary.define("CODE", initial_body=bytes(16))
    dictionary.allot(u64(-1))
    dictionary.allot(1)

    assert dictionary.resolve(word.xt) is word
    with pytest.raises(ValueError, match="no live nonempty initial body"):
        dictionary.acquire_body_lease(word)


def test_zero_forward_and_adjacent_rewinds_preserve_body_and_generation() -> None:
    dictionary, _ = _dictionary()
    word = dictionary.define("CODE", initial_body=bytes(16))
    lease = dictionary.acquire_body_lease(word)
    generation = dictionary.execution_generation
    floor, limit = dictionary.active_zone

    dictionary.allot(0)
    dictionary.allot(32)
    dictionary.allot(u64(-32))
    dictionary.move_here(lease.body_limit, floor=floor, limit=limit)
    dictionary.move_here(lease.body_limit + 32, floor=floor, limit=limit)
    dictionary.move_here(lease.body_limit, floor=floor, limit=limit)
    dictionary.write_transient(b"")

    assert dictionary.is_body_lease_live(lease)
    assert dictionary.execution_generation == generation


def test_leaving_lower_or_higher_arena_preserves_bodies_until_that_zone_reopens() -> None:
    dictionary, _ = _dictionary()
    bank0 = dictionary.define("BANK0", initial_body=bytes(16))
    bank0_lease = dictionary.acquire_body_lease(bank0)
    bank0_here = dictionary.here
    bank0_floor, bank0_limit = dictionary.active_zone
    external_limit = EXTERNAL_BASE + 0x2000
    dictionary.move_here(EXTERNAL_BASE, floor=EXTERNAL_BASE, limit=external_limit)
    external = dictionary.define("EXTERNAL", initial_body=bytes(16))
    external_lease = dictionary.acquire_body_lease(external)

    dictionary.move_here(bank0_here, floor=bank0_floor, limit=bank0_limit)
    assert dictionary.is_body_lease_live(bank0_lease)
    assert dictionary.is_body_lease_live(external_lease)

    dictionary.move_here(
        external_lease.body_limit - 1,
        floor=EXTERNAL_BASE,
        limit=external_limit,
    )
    assert dictionary.is_body_lease_live(bank0_lease)
    assert not dictionary.is_body_lease_live(external_lease)
    assert dictionary.resolve(external.xt) is external


def test_rewind_and_shrink_preserves_excluded_body_until_bounds_expand() -> None:
    dictionary, _ = _dictionary()
    dictionary.allot(0x3000)
    excluded = dictionary.define("HIGH", initial_body=bytes(16))
    excluded_lease = dictionary.acquire_body_lease(excluded)
    dictionary.move_here(0x5000, floor=0x1000, limit=0x8000)

    dictionary.move_here(0x3000, floor=0x1000, limit=0x3500)
    lower = dictionary.define("LOW", initial_body=bytes(8))
    lower_lease = dictionary.acquire_body_lease(lower)
    assert dictionary.is_body_lease_live(excluded_lease)

    dictionary.move_here(dictionary.here, floor=0x1000, limit=0x8000)
    assert not dictionary.is_body_lease_live(excluded_lease)
    assert dictionary.is_body_lease_live(lower_lease)
    assert dictionary.resolve(excluded.xt) is excluded


def test_rollback_and_identical_xt_reuse_get_a_new_allocation_identity() -> None:
    dictionary, memory = _dictionary()
    retained = dictionary.define("RETAINED", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    checkpoint = dictionary.checkpoint()
    original = dictionary.define("CODE", initial_body=b"abcdefgh")
    old_lease = dictionary.acquire_body_lease(original)
    original_bytes = memory.read_bytes(
        original.header_address, dictionary.here - original.header_address
    )

    dictionary.rollback(checkpoint)
    assert not dictionary.is_body_lease_live(old_lease)
    assert dictionary.is_body_lease_live(retained_lease)
    replacement = dictionary.define("CODE", initial_body=b"abcdefgh")
    new_lease = dictionary.acquire_body_lease(replacement)

    assert replacement.xt == original.xt
    assert replacement is not original
    assert memory.read_bytes(
        replacement.header_address, len(original_bytes)
    ) == original_bytes
    assert new_lease.allocation_serial > old_lease.allocation_serial
    assert dictionary.is_body_lease_live(new_lease)
    assert not dictionary.is_body_lease_live(old_lease)
    with pytest.raises(ValueError, match="exact live word"):
        dictionary.acquire_body_lease(original)


def test_latest_removal_revokes_without_moving_here() -> None:
    dictionary, _ = _dictionary()
    retained = dictionary.define("RETAINED", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    removed = dictionary.define("CODE", initial_body=bytes(16))
    removed_lease = dictionary.acquire_body_lease(removed)
    old_here = dictionary.here

    dictionary.set_latest(retained.header_address)

    assert dictionary.here == old_here
    assert dictionary.is_body_lease_live(retained_lease)
    assert not dictionary.is_body_lease_live(removed_lease)
    with pytest.raises(ValueError, match="exact live word"):
        dictionary.acquire_body_lease(removed)


def test_opaque_cross_zone_rollback_revokes_removed_external_body_only() -> None:
    dictionary, _ = _dictionary()
    retained = dictionary.define("BANK0", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    checkpoint = dictionary.checkpoint()
    dictionary.move_here(
        EXTERNAL_BASE, floor=EXTERNAL_BASE, limit=EXTERNAL_BASE + 0x2000
    )
    removed = dictionary.define("EXTERNAL", initial_body=bytes(16))
    removed_lease = dictionary.acquire_body_lease(removed)

    dictionary.rollback(checkpoint)

    assert dictionary.here == checkpoint.here
    assert dictionary.is_body_lease_live(retained_lease)
    assert not dictionary.is_body_lease_live(removed_lease)


@pytest.mark.parametrize(
    "failure",
    ("allot", "zone", "capacity", "checkpoint", "numeric", "latest"),
)
def test_failed_mutation_preflight_preserves_live_allocations(failure: str) -> None:
    dictionary, _ = _dictionary()
    retained = dictionary.define("RETAINED", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    word = dictionary.define("CODE", initial_body=bytes(16))
    lease = dictionary.acquire_body_lease(word)
    checkpoint = dictionary.checkpoint()
    before = (
        dictionary.here,
        dictionary.active_zone,
        dictionary.words,
        dictionary.execution_generation,
    )

    with pytest.raises((ValueError, OverflowError)):
        if failure == "allot":
            dictionary.allot(u64(-dictionary.here))
        elif failure == "zone":
            dictionary.move_here(word.body_address, floor=0x1000, limit=0x9000)
        elif failure == "capacity":
            dictionary.define("HUGE", initial_body=bytes(0x8000))
        elif failure == "checkpoint":
            dictionary.rollback(replace(checkpoint, here=word.body_address))
        elif failure == "numeric":
            dictionary.rollback_to(word.header_address + 1, retained.header_address)
        else:
            dictionary.set_latest(word.xt)

    assert (
        dictionary.here,
        dictionary.active_zone,
        dictionary.words,
        dictionary.execution_generation,
    ) == before
    assert dictionary.is_body_lease_live(retained_lease)
    assert dictionary.is_body_lease_live(lease)


def test_failed_header_overlap_leaves_excluded_initial_body_live() -> None:
    dictionary, _ = _dictionary()
    word = dictionary.define("ORIGINAL-LONG-NAME", initial_body=bytes(16))
    lease = dictionary.acquire_body_lease(word)
    dictionary.move_here(
        word.header_address,
        floor=word.header_address,
        limit=word.body_address,
    )
    assert dictionary.is_body_lease_live(lease)

    with pytest.raises(ValueError, match="overlap a live header"):
        dictionary.define("NEW", initial_body=bytes(1))

    assert dictionary.is_body_lease_live(lease)
    assert dictionary.resolve(word.xt) is word


@pytest.mark.parametrize("removal", ("rollback_to", "set_latest"))
def test_failed_guest_link_validation_does_not_revoke_body(removal: str) -> None:
    dictionary, memory = _dictionary()
    retained = dictionary.define("RETAINED", initial_body=bytes(8))
    retained_lease = dictionary.acquire_body_lease(retained)
    removed = dictionary.define("CODE", initial_body=bytes(16))
    removed_lease = dictionary.acquire_body_lease(removed)
    memory.write64(retained.header_address, removed.header_address)

    with pytest.raises(ValueError, match="link history is inconsistent"):
        if removal == "rollback_to":
            dictionary.rollback_to(removed.header_address, retained.header_address)
        else:
            dictionary.set_latest(retained.header_address)

    assert dictionary.is_body_lease_live(retained_lease)
    assert dictionary.is_body_lease_live(removed_lease)
    assert dictionary.resolve(removed.xt) is removed


@pytest.mark.parametrize("emission", ("define", "comma", "c_comma", "transient"))
def test_allocator_emission_revokes_before_writing_even_identical_bytes(
    emission: str, monkeypatch: pytest.MonkeyPatch,
) -> None:
    dictionary, memory = _dictionary()
    unrelated = dictionary.define("RETAINED", initial_body=bytes(8))
    unrelated_lease = dictionary.acquire_body_lease(unrelated)
    word = dictionary.define("CODE", initial_body=bytes(128))
    lease = dictionary.acquire_body_lease(word)
    if emission == "define":
        payload = (
            word.header_address.to_bytes(8, "little")
            + b"\x05FRESH" + bytes(8) + b"new"
        )
        method = "write_bytes"
    elif emission == "comma":
        payload, method = b"\xa5" * 8, "write64"
    elif emission == "c_comma":
        payload, method = b"\xa5", "write8"
    else:
        payload, method = b"identical", "write_bytes"
    memory.write_bytes(word.body_address, payload)

    # Arrange an already-overlapping frontier so the emission barrier itself
    # is tested; a public rewind would revoke the lease before reaching it.
    monkeypatch.setattr(dictionary, "_here", word.body_address)
    original_write = getattr(memory, method)
    writes = []

    def observed_write(address: int, value: object) -> None:
        assert not dictionary.is_body_lease_live(lease)
        assert dictionary.is_body_lease_live(unrelated_lease)
        assert memory.read_bytes(address, len(payload)) == payload
        writes.append(address)
        original_write(address, value)

    monkeypatch.setattr(memory, method, observed_write)
    if emission == "define":
        fresh = dictionary.define("FRESH", initial_body=b"new")
        assert dictionary.is_body_lease_live(dictionary.acquire_body_lease(fresh))
    elif emission == "comma":
        dictionary.comma(int.from_bytes(payload, "little"))
    elif emission == "c_comma":
        dictionary.c_comma(payload[0])
    else:
        assert dictionary.write_transient(payload) == word.body_address

    assert writes == [word.body_address]
    assert dictionary.resolve(word.xt) is word
    assert not dictionary.is_body_lease_live(lease)
    assert memory.read_bytes(word.body_address, len(payload)) == payload


@pytest.mark.parametrize("emission", ("define", "comma", "c_comma", "transient"))
def test_failed_overlapping_emission_preflight_preserves_body_identity(
    emission: str, monkeypatch: pytest.MonkeyPatch,
) -> None:
    dictionary, memory = _dictionary()
    word = dictionary.define("CODE", initial_body=bytes(32))
    lease = dictionary.acquire_body_lease(word)
    monkeypatch.setattr(dictionary, "_here", word.body_address)

    if emission == "define":
        with pytest.raises(TypeError, match="initial body must be bytes"):
            dictionary.define("FRESH", initial_body=bytearray(8))  # type: ignore[arg-type]
    elif emission == "comma":
        with pytest.raises(TypeError, match="stored value"):
            dictionary.comma(object())  # type: ignore[arg-type]
    elif emission == "c_comma":
        with pytest.raises(TypeError, match="stored value"):
            dictionary.c_comma(object())  # type: ignore[arg-type]
    else:
        with pytest.raises(TypeError, match="payload must be bytes"):
            dictionary.write_transient(bytearray(8))  # type: ignore[arg-type]

    assert dictionary.is_body_lease_live(lease)
    assert dictionary.here == word.body_address
    assert memory.read_bytes(word.body_address, 32) == bytes(32)


def test_memory_span_preflight_failure_precedes_overlapping_emission_revocation(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    dictionary, memory = _dictionary()
    word = dictionary.define("CODE", initial_body=bytes(32))
    lease = dictionary.acquire_body_lease(word)
    monkeypatch.setattr(dictionary, "_here", word.body_address)

    def reject_span(address: int, length: int) -> None:
        assert address == word.body_address
        assert length == 8
        raise ValueError("span preflight rejected")

    monkeypatch.setattr(memory, "_qualify_ordinary_span", reject_span)
    with pytest.raises(ValueError, match="span preflight rejected"):
        dictionary.write_transient(bytes(8))

    assert dictionary.is_body_lease_live(lease)
    assert dictionary.here == word.body_address


def test_source_allot_revokes_through_runtime_move_here_path() -> None:
    runtime = MegaForthRuntime(execution_backend="python")
    word = runtime.define_primitive(
        "CODE", lambda context: None, initial_body=bytes(8)
    )
    lease = runtime.dictionary.acquire_body_lease(word)

    runtime.evaluate(b"-1 ALLOT 1 ALLOT")

    assert runtime.dictionary.resolve(word.xt) is word
    assert runtime.dictionary.here == lease.body_limit
    assert not runtime.dictionary.is_body_lease_live(lease)
