from __future__ import annotations

import pytest

from simulator.dictionary import Dictionary, Word
from simulator.memory import SparseAddressSpace


def _assert_two_words_live(
    dictionary: Dictionary,
    original: Word,
    replacement: Word,
    *,
    here: int,
) -> None:
    assert dictionary.here == here
    assert dictionary.latest_word is replacement
    assert dictionary.find("PANEL") is replacement
    assert dictionary.resolve(original.xt) is original
    assert dictionary.resolve(replacement.xt) is replacement


def test_numeric_rollback_accepts_contiguous_ancestor_with_allot_gap() -> None:
    memory = SparseAddressSpace(bank0_size=0x10000)
    dictionary = Dictionary(start_address=0x4000, memory=memory)
    original = dictionary.define("PANEL", implementation="original")
    saved_here = dictionary.here
    dictionary.allot(0x20)
    replacement = dictionary.define("panel", implementation="replacement")
    transient = dictionary.define("TRANSIENT", implementation="discarded")
    active_here = dictionary.here
    stale_bytes = memory.read_bytes(saved_here, active_here - saved_here)

    dictionary.rollback_to(saved_here, original.header_address)

    assert dictionary.here == saved_here
    assert dictionary.latest_word is original
    assert dictionary.find("PANEL") is original
    assert dictionary.find("TRANSIENT") is None
    assert dictionary.resolve(original.xt) is original
    with pytest.raises(KeyError):
        dictionary.resolve(replacement.xt)
    with pytest.raises(KeyError):
        dictionary.resolve(transient.xt)
    assert memory.read_bytes(saved_here, len(stale_bytes)) == stale_bytes


def test_numeric_rollback_accepts_empty_ancestor_and_current_noop() -> None:
    dictionary = Dictionary(start_address=0x4000)
    original = dictionary.define("FIRST", implementation="original")
    dictionary.rollback_to(dictionary.here, original.header_address)

    assert dictionary.latest_word is original
    assert dictionary.resolve(original.xt) is original

    dictionary.rollback_to(0x4000, 0)

    assert dictionary.here == 0x4000
    assert dictionary.latest == 0
    assert dictionary.find("FIRST") is None
    with pytest.raises(KeyError):
        dictionary.resolve(original.xt)


@pytest.mark.parametrize(
    ("target", "message"),
    (
        ("advance", "ahead of the active dictionary"),
        ("unknown-latest", "not a live dictionary ancestor"),
        ("inside-removed-header", "removed dictionary header"),
        ("retained-in-reclaimed-interval", "retained dictionary header"),
    ),
)
def test_numeric_rollback_rejects_invalid_pairs_atomically(
    target: str,
    message: str,
) -> None:
    dictionary = Dictionary(start_address=0x4000)
    original = dictionary.define("PANEL", implementation="original")
    replacement = dictionary.define("panel", implementation="replacement")
    active_here = dictionary.here

    if target == "advance":
        saved_here = active_here + 1
        saved_latest = replacement.header_address
    elif target == "unknown-latest":
        saved_here = original.body_address
        saved_latest = original.xt
    elif target == "inside-removed-header":
        saved_here = replacement.header_address + 1
        saved_latest = original.header_address
    else:
        saved_here = original.header_address
        saved_latest = original.header_address

    with pytest.raises(ValueError, match=message):
        dictionary.rollback_to(saved_here, saved_latest)

    _assert_two_words_live(
        dictionary,
        original,
        replacement,
        here=active_here,
    )


def test_numeric_rollback_rejects_here_outside_dictionary_region_atomically() -> None:
    dictionary = Dictionary(start_address=0x4000)
    live = dictionary.define("LIVE", implementation="live")
    active_here = dictionary.here

    with pytest.raises(ValueError, match="outside the dictionary region"):
        dictionary.rollback_to(0x3FFF, 0)

    assert dictionary.here == active_here
    assert dictionary.latest_word is live
    assert dictionary.find("LIVE") is live
    assert dictionary.resolve(live.xt) is live


def test_numeric_rollback_rejects_a_truncated_guest_link_chain_atomically() -> None:
    memory = SparseAddressSpace(bank0_size=0x10000)
    dictionary = Dictionary(start_address=0x4000, memory=memory)
    original = dictionary.define("PANEL", implementation="original")
    replacement = dictionary.define("panel", implementation="replacement")
    active_here = dictionary.here
    memory.write64(replacement.header_address, 0)

    with pytest.raises(ValueError, match="link history is inconsistent"):
        dictionary.rollback_to(replacement.header_address, original.header_address)

    _assert_two_words_live(
        dictionary,
        original,
        replacement,
        here=active_here,
    )


def test_numeric_rollback_floor_protects_a_sealed_dictionary_prefix() -> None:
    dictionary = Dictionary(start_address=0x4000)
    protected = dictionary.define("PANEL", implementation="protected")
    dictionary.protect_current_prefix_from_numeric_rollback()
    floor = dictionary.numeric_rollback_floor
    transient = dictionary.define("panel", implementation="transient")
    active_here = dictionary.here

    with pytest.raises(ValueError, match="protected dictionary prefix"):
        dictionary.rollback_to(protected.header_address, 0)

    _assert_two_words_live(
        dictionary,
        protected,
        transient,
        here=active_here,
    )

    dictionary.rollback_to(floor, protected.header_address)
    assert dictionary.here == floor
    assert dictionary.latest_word is protected
    assert dictionary.find("PANEL") is protected
    with pytest.raises(KeyError):
        dictionary.resolve(transient.xt)
