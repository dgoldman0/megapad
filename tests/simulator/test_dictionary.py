from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.dictionary import (
    Dictionary,
    HEADER_FIXED_BYTES,
    SEMANTIC_CODE_SLOT_BYTES,
)


def test_definition_uses_native_no_padding_header_geometry() -> None:
    dictionary = Dictionary(start_address=0x2000)

    word = dictionary.define(b"DUP", implementation="primitive", immediate=True)

    assert word.name == b"DUP"
    assert word.header_address == 0x2000
    assert word.xt == word.header_address + 9 + len(word.name)
    assert HEADER_FIXED_BYTES == 9
    assert word.immediate is True
    assert word.implementation == "primitive"
    assert dictionary.latest == word.header_address
    assert dictionary.here == word.xt + SEMANTIC_CODE_SLOT_BYTES
    assert word.xt != 0


def test_lookup_is_ascii_case_insensitive_and_newest_binding_wins() -> None:
    dictionary = Dictionary()
    old = dictionary.define(b"Render", implementation="old")
    new = dictionary.define("rENDER", implementation="new")

    assert dictionary.find(b"RENDER") is new
    assert dictionary.find("render") is new
    assert dictionary.resolve(old.xt) is old
    assert dictionary.resolve(new.xt) is new


def test_stored_execution_token_keeps_its_compiled_binding_after_shadowing() -> None:
    dictionary = Dictionary()
    original = dictionary.define("DRAW", implementation=lambda: "original")
    compiled_call_xt = original.xt

    replacement = dictionary.define("draw", implementation=lambda: "replacement")

    assert dictionary.find("DRAW") is replacement
    assert dictionary.resolve(compiled_call_xt) is original
    assert dictionary.resolve(compiled_call_xt).implementation() == "original"


def test_rollback_removes_new_execution_tokens_and_restores_shadowed_binding() -> None:
    dictionary = Dictionary(start_address=0x4000)
    original = dictionary.define("OPEN", implementation="original")
    checkpoint = dictionary.checkpoint()
    replacement = dictionary.define("open", implementation="replacement")
    transient = dictionary.define("CLOSE", implementation="transient")

    dictionary.rollback(checkpoint)

    assert dictionary.find("OPEN") is original
    assert dictionary.find("CLOSE") is None
    assert dictionary.resolve(original.xt) is original
    with pytest.raises(KeyError):
        dictionary.resolve(replacement.xt)
    with pytest.raises(KeyError):
        dictionary.resolve(transient.xt)
    assert dictionary.here == checkpoint.here
    assert dictionary.latest == checkpoint.latest == original.header_address


def test_definition_rejects_uint64_address_wrap_without_publication() -> None:
    allocation_size = HEADER_FIXED_BYTES + 1 + SEMANTIC_CODE_SLOT_BYTES
    dictionary = Dictionary(start_address=MASK64 - allocation_size + 1)
    checkpoint = dictionary.checkpoint()

    with pytest.raises(OverflowError, match="wrap uint64"):
        dictionary.define("X")

    assert dictionary.here == checkpoint.here
    assert dictionary.latest == 0
    assert dictionary.find("X") is None


def test_name_validation_preserves_the_seven_bit_header_length() -> None:
    dictionary = Dictionary()

    with pytest.raises(ValueError, match="ASCII"):
        dictionary.define("café")
    with pytest.raises(ValueError, match="127"):
        dictionary.define(b"X" * 128)
