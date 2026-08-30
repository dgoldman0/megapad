"""Focused byte-oracle tests for the hosted source cursor."""

from __future__ import annotations

import pytest

from simulator.source import SourceBuffer, SourceCursor, SourceLine


def test_source_buffers_are_bytes_only_and_snapshot_mutable_inputs() -> None:
    mutable = bytearray(b"one\n")
    source = SourceBuffer(mutable, name="buffer.f")
    mutable[0] = ord("X")

    assert source.data == b"one\n"
    with pytest.raises(TypeError, match="bytes-like"):
        SourceBuffer("one\n", name="buffer.f")  # type: ignore[arg-type]


def test_line_iteration_matches_kdos_lf_and_optional_cr_rules() -> None:
    source = SourceBuffer(b"one\r\n\nthree\rfour\nlast\r", name="lines.f")

    assert [
        (line.data, line.number, line.offset, line.source_name)
        for line in source
    ] == [
        (b"one", 1, 0, "lines.f"),
        (b"", 2, 5, "lines.f"),
        (b"three\rfour", 3, 6, "lines.f"),
        (b"last", 4, 17, "lines.f"),
    ]

    assert [line.data for line in SourceBuffer(b"final\n")] == [b"final"]
    assert list(SourceBuffer(b"")) == []


def test_source_line_rejects_lf_because_cursors_are_line_local() -> None:
    with pytest.raises(ValueError, match="cannot contain LF"):
        SourceLine(b"one\ntwo")


def test_parse_word_delimits_only_ascii_space_and_records_diagnostics() -> None:
    line = SourceLine(
        b"  alpha\tbeta  gamma\rdelta",
        source_name="tokens.f",
        number=4,
        offset=100,
    )
    cursor = line.cursor()

    assert cursor.parse_word() == b"alpha\tbeta"
    assert cursor.column == 12  # The trailing space remains at >IN.
    assert cursor.offset == 112
    assert cursor.last_token is not None
    assert cursor.last_token.data == b"alpha\tbeta"
    assert cursor.last_token.location.source_name == "tokens.f"
    assert cursor.last_token.location.offset == 102
    assert cursor.last_token.location.line == 4
    assert cursor.last_token.location.column == 2

    assert cursor.parse_word() == b"gamma\rdelta"
    assert cursor.last_token is not None
    assert cursor.last_token.column == 14
    assert cursor.parse_word() == b""
    assert cursor.last_token is None


def test_explicit_cursor_seek_preserves_a_past_end_to_in_value() -> None:
    cursor = SourceCursor(b"word", source_name="seek.f", line_number=2, offset=9)
    cursor.seek(20)

    assert cursor.parse_word() == b""
    assert cursor.column == 20
    assert cursor.offset == 29
    assert cursor.location.line == 2
    assert cursor.remaining == b""


def test_raw_delimiter_scan_reports_whether_it_was_terminated() -> None:
    cursor = SourceCursor(
        b'raw bytes" tail', source_name="quote.f", line_number=3, offset=40
    )

    value = cursor.consume_until(ord('"'))
    assert value.data == b"raw bytes"
    assert value.terminated is True
    assert value.delimiter == ord('"')
    assert value.location.offset == 40
    assert value.location.line == 3
    assert value.location.column == 0
    assert cursor.parse_word() == b"tail"

    missing = SourceCursor(b"unterminated").consume_until(ord('"'))
    assert missing.data == b"unterminated"
    assert missing.terminated is False


def test_consume_byte_is_raw_and_does_not_advance_on_mismatch() -> None:
    cursor = SourceCursor(b' "value"')

    assert cursor.consume_byte(ord('"')) is None
    assert cursor.column == 0
    assert cursor.consume_byte(ord(" ")) == ord(" ")
    assert cursor.consume_byte(ord('"')) == ord('"')
    assert cursor.consume_until(ord('"')).data == b"value"
    assert cursor.at_end is True


def test_parenthesis_comments_nest_within_the_current_line() -> None:
    cursor = SourceCursor(b"( outer (inner (deep) done) tail ) next")

    assert cursor.parse_word() == b"("
    assert cursor.skip_parenthesis_comment() is True
    assert cursor.parse_word() == b"next"

    unmatched = SourceCursor(b"( outer (inner) still open")
    assert unmatched.parse_word() == b"("
    assert unmatched.skip_parenthesis_comment() is False
    assert unmatched.at_end is True


def test_backslash_comment_consumes_the_remainder_of_one_line() -> None:
    cursor = SourceCursor(b"value \\comment words")

    assert cursor.parse_word() == b"value"
    token = cursor.parse_word()
    assert token == b"\\comment"
    assert token.startswith(b"\\")
    assert cursor.skip_backslash_comment() == b" words"
    assert cursor.at_end is True
    assert cursor.parse_word() == b""
