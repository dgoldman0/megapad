"""Independent FDC1 oracles and immutable typed-value/slot constraints."""

from dataclasses import FrozenInstanceError, replace
import struct

import pytest

from rich_terminal.semantic_content import SemanticContentError, SemanticContentErrorCode
from rich_terminal.semantic_fields import (
    FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect,
    decode_field_content, encode_field_content,
)


I64_MIN = -(1 << 63)
I64_MAX = (1 << 63) - 1


def integer(**changes):
    return FieldContent(**(dict(content_revision=7, kind=FieldKind.INTEGER,
                               flags=FieldFlag(0), label_bounds=FieldRect(0, 0, 5, 1),
                               value_bounds=FieldRect(5, 0, 15, 1), value=3,
                               minimum=-10, maximum=10, step=2) | changes))


def choice(**changes):
    return integer(**(dict(kind=FieldKind.CHOICE, value=-7, minimum=0, maximum=0,
                           step=0, choices=(FieldChoice(-7, "茶"), FieldChoice(14, "Long")))
                      | changes))


def text(**changes):
    return integer(**(dict(kind=FieldKind.TEXT, value=0, minimum=0, maximum=0,
                           step=0, text="e\u0301 ✓") | changes))


def header(kind=1, revision=7, flags=0, value=3, minimum=-10, maximum=10, step=2,
           choice_count=0, text_bytes=0):
    return struct.pack("<IHHQIIiiIIiiIIqqqqII", 0x31434446, 1, kind, revision,
                       flags, 0, 0, 0, 5, 1, 5, 0, 15, 1,
                       value, minimum, maximum, step, choice_count, text_bytes)


def test_independent_integer_header_has_exact_96_byte_layout():
    assert len(header()) == 96
    assert encode_field_content(integer()) == header()
    assert decode_field_content(header()) == integer()


def test_choice_records_preserve_signed_values_order_and_utf8_byte_lengths():
    expected = header(kind=2, value=-7, minimum=0, maximum=0, step=0, choice_count=2)
    expected += struct.pack("<qII", -7, 3, 0) + "茶".encode()
    expected += struct.pack("<qII", 14, 4, 0) + b"Long"
    assert encode_field_content(choice()) == expected
    assert decode_field_content(expected) == choice()
    assert choice().text_bytes == choice().utf8_bytes == 7
    assert choice().object_slots == 3
    assert choice().wire_bytes == len(expected)


def test_text_has_exact_trailing_utf8_and_zero_numeric_fields():
    raw = "e\u0301 ✓".encode()
    expected = header(kind=3, value=0, minimum=0, maximum=0, step=0, text_bytes=len(raw)) + raw
    assert encode_field_content(text()) == expected
    assert decode_field_content(expected) == text()
    assert text().text_bytes == len(raw)
    assert text().object_slots == 1
    assert text().wire_bytes == len(expected)
    assert integer().text_bytes == 0
    assert integer().object_slots == 1
    assert integer().wire_bytes == 96


@pytest.mark.parametrize("kind_factory", [integer, choice, text])
def test_read_only_and_adjustable_are_derived_from_declared_kind_and_flags(kind_factory):
    value = kind_factory()
    assert not value.read_only
    assert value.is_adjustable == (value.kind is not FieldKind.TEXT)
    readonly = replace(value, flags=FieldFlag.READ_ONLY)
    assert readonly.read_only
    assert not readonly.is_adjustable
    assert decode_field_content(encode_field_content(readonly)) == readonly


@pytest.mark.parametrize("value", [I64_MIN, -1, 0, I64_MAX])
def test_integer_full_signed_range_and_unaligned_current_values_are_supported(value):
    content = integer(value=value, minimum=I64_MIN, maximum=I64_MAX, step=I64_MAX)
    assert decode_field_content(encode_field_content(content)) == content


def test_single_value_integer_range_is_valid_with_positive_step():
    assert integer(value=I64_MIN, minimum=I64_MIN, maximum=I64_MIN, step=1)


@pytest.mark.parametrize("changes", [
    {"minimum": 4}, {"maximum": 2}, {"minimum": 4, "maximum": 2},
    {"step": 0}, {"step": -1}, {"step": 1 << 63}, {"value": I64_MIN - 1},
    {"value": I64_MAX + 1}, {"value": True}, {"value": 1.5},
    {"content_revision": 0}, {"content_revision": 1 << 64}, {"content_revision": True},
    {"kind": 0}, {"kind": 4}, {"kind": True}, {"kind": 1.0},
    {"flags": 2}, {"flags": True}, {"flags": -1},
    {"choices": (FieldChoice(3, "Three"),)}, {"text": "x"},
    {"value_bounds": FieldRect(0, 0, 0, 0)}, {"label_bounds": (0, 0, 5, 1)},
])
def test_integer_or_common_metadata_rejects_invalid_types_and_noncanonical_values(changes):
    with pytest.raises((TypeError, ValueError)):
        integer(**changes)


@pytest.mark.parametrize("changes", [
    {"choices": ()}, {"choices": (FieldChoice(-7, "A"), FieldChoice(-7, "B"))},
    {"value": 3}, {"minimum": -1}, {"maximum": 1}, {"step": 1}, {"text": "x"},
    {"choices": ((-7, "wrong type"),)},
])
def test_choice_domain_is_nonempty_unique_and_contains_current_value(changes):
    with pytest.raises((TypeError, ValueError)):
        choice(**changes)


def test_choice_value_extremes_and_duplicate_display_labels_are_valid():
    content = choice(value=I64_MIN, choices=[FieldChoice(I64_MIN, "A"), FieldChoice(I64_MAX, "A")])
    assert isinstance(content.choices, tuple)
    assert decode_field_content(encode_field_content(content)) == content
    with pytest.raises(FrozenInstanceError):
        content.value = 1


@pytest.mark.parametrize("changes", [
    {"value": 1}, {"minimum": -1}, {"maximum": 1}, {"step": 1},
    {"choices": (FieldChoice(0, "Zero"),)},
])
def test_text_kind_has_only_its_text_value(changes):
    with pytest.raises(ValueError):
        text(**changes)


@pytest.mark.parametrize("bad", ["\x00", "\x1f", "\x7f", "\x85", "\x9f", "\u2028", "\u2029", "\ud800"])
def test_clean_single_line_scalar_contract_covers_all_three_label_or_text_sources(bad):
    with pytest.raises(ValueError):
        FieldChoice(1, bad)
    with pytest.raises(ValueError):
        text(text=bad)
    with pytest.raises(ValueError):
        integer().validate_geometry(cols=20, rows=1, label=bad)


def test_empty_text_is_valid_but_empty_choice_label_is_not():
    assert text(text="").text_bytes == 0
    with pytest.raises(ValueError, match="nonempty"):
        FieldChoice(0, "")


@pytest.mark.parametrize("values", [
    (1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1),
    (0, 0, -1, 1), (-(1 << 31) - 1, 0, 1, 1), ((1 << 31), 0, 1, 1),
    (0, 0, 1 << 32, 1), (True, 0, 1, 1),
])
def test_rectangles_reject_noncanonical_empty_and_out_of_width_values(values):
    with pytest.raises((TypeError, ValueError)):
        FieldRect(*values)


def test_rectangles_keep_exact_wide_endpoints_and_canonical_empty_metadata():
    empty = FieldRect(0, 0, 0, 0)
    assert empty.empty and empty.as_tuple() == (0, 0, 0, 0)
    wide = FieldRect((1 << 31) - 1, -1, (1 << 32) - 1, 2)
    assert wide.right == (1 << 31) - 1 + (1 << 32) - 1
    assert wide.bottom == 1
    assert not wide.empty


def test_geometry_label_presence_matches_its_explicit_rectangle():
    integer().validate_geometry(cols=20, rows=1, label="Gain")
    with pytest.raises(ValueError, match="exactly when"):
        integer().validate_geometry(cols=20, rows=1, label="")
    unlabeled = integer(label_bounds=FieldRect(0, 0, 0, 0), value_bounds=FieldRect(0, 0, 20, 1))
    unlabeled.validate_geometry(cols=20, rows=1, label="")
    with pytest.raises(ValueError, match="exactly when"):
        unlabeled.validate_geometry(cols=20, rows=1, label="Gain")


@pytest.mark.parametrize("changes", [
    {"label_bounds": FieldRect(-1, 0, 5, 1)},
    {"label_bounds": FieldRect(0, -1, 5, 1)},
    {"value_bounds": FieldRect(5, 0, 16, 1)},
    {"value_bounds": FieldRect(5, 1, 15, 1)},
    {"value_bounds": FieldRect(4, 0, 15, 1)},
    {"value_bounds": FieldRect((1 << 31) - 1, 0, (1 << 32) - 1, 1)},
])
def test_geometry_rejects_outside_or_overlapping_slots_without_wrapping(changes):
    with pytest.raises(ValueError):
        integer(**changes).validate_geometry(cols=20, rows=1, label="Gain")


def test_touching_and_vertically_separated_slots_do_not_overlap():
    integer().validate_geometry(cols=20, rows=1, label="Gain")
    content = integer(label_bounds=FieldRect(0, 0, 20, 1), value_bounds=FieldRect(0, 1, 20, 1))
    content.validate_geometry(cols=20, rows=2, label="Gain")


@pytest.mark.parametrize("offset,fmt,value,code", [
    (0, "<I", 0, SemanticContentErrorCode.ENUM),
    (4, "<H", 2, SemanticContentErrorCode.ENUM),
    (6, "<H", 4, SemanticContentErrorCode.ENUM),
    (8, "<Q", 0, SemanticContentErrorCode.CONSISTENCY),
    (16, "<I", 2, SemanticContentErrorCode.RESERVED),
    (20, "<I", 1, SemanticContentErrorCode.RESERVED),
    (32, "<I", 0, SemanticContentErrorCode.CONSISTENCY),
    (52, "<I", 0, SemanticContentErrorCode.CONSISTENCY),
    (80, "<q", 0, SemanticContentErrorCode.CONSISTENCY),
    (88, "<I", 0xFFFFFFFF, SemanticContentErrorCode.PAYLOAD),
    (92, "<I", 0xFFFFFFFF, SemanticContentErrorCode.PAYLOAD),
])
def test_noncanonical_header_fields_have_deterministic_rejection(offset, fmt, value, code):
    raw = bytearray(header())
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(SemanticContentError) as error:
        decode_field_content(raw)
    assert error.value.code is code


@pytest.mark.parametrize("offset,fmt,value", [
    (108, "<I", 1),       # first choice reserved
    (104, "<I", 0),      # empty first label changes record alignment
    (104, "<I", 1000),   # missing label bytes
    (115, "<q", -7),     # duplicate second value
])
def test_malformed_choice_records_are_rejected(offset, fmt, value):
    raw = bytearray(encode_field_content(choice()))
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(SemanticContentError):
        decode_field_content(raw)


@pytest.mark.parametrize("tail", [b"\xff", b"\xed\xa0\x80", b"\x00", b"\xc2\x85", b"\xe2\x80\xa8"])
def test_wire_text_and_choice_labels_use_strict_scalar_decoding(tail):
    for raw in (
        header(kind=3, value=0, minimum=0, maximum=0, step=0, text_bytes=len(tail)) + tail,
        header(kind=2, value=1, minimum=0, maximum=0, step=0, choice_count=1)
        + struct.pack("<qII", 1, len(tail), 0) + tail,
    ):
        with pytest.raises(SemanticContentError) as error:
            decode_field_content(raw)
        assert error.value.code is SemanticContentErrorCode.SCALAR


@pytest.mark.parametrize("factory", [integer, choice, text])
def test_exact_payload_lengths_reject_truncation_and_appended_bytes(factory):
    raw = encode_field_content(factory())
    for malformed in (raw[:95], raw[:-1], raw + b"\0"):
        with pytest.raises(SemanticContentError):
            decode_field_content(malformed)
    assert decode_field_content(memoryview(raw)) == factory()


def test_decoder_requires_bytes_like_input_and_encoder_requires_content():
    with pytest.raises(SemanticContentError):
        decode_field_content(96)
    with pytest.raises(TypeError):
        encode_field_content({})


def test_display_text_uses_declared_value_kind_and_choice_identity():
    for value in (I64_MIN, I64_MAX):
        assert integer(value=value, minimum=I64_MIN, maximum=I64_MAX).display_text == str(value)
    assert choice().display_text == "茶"
    assert choice(value=14).display_text == "Long"
    assert text().display_text == "e\u0301 ✓"
