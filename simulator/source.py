"""Line-local byte cursors for the hosted Forth evaluator.

This module intentionally stops below syntax trees.  It models the BIOS input
surface: whole source buffers become logical lines, and each line has a
mutable ``>IN``-like byte cursor used by words that parse their own input.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterator

from shared.cells import MASK64


_ByteSource = bytes | bytearray | memoryview
ASCII_SPACE = 0x20


def _freeze_bytes(value: _ByteSource, *, argument: str) -> bytes:
    if isinstance(value, bytes):
        return value
    if isinstance(value, (bytearray, memoryview)):
        return bytes(value)
    raise TypeError(f"{argument} must be bytes-like, not {type(value).__name__}")


def _check_name(value: str) -> None:
    if not isinstance(value, str):
        raise TypeError(f"source name must be str, not {type(value).__name__}")


def _check_delimiter(value: int) -> None:
    if not isinstance(value, int) or not 0 <= value <= 0xFF:
        raise ValueError("delimiter must be an integer byte (0..255)")


@dataclass(frozen=True, slots=True)
class SourceLocation:
    """A source byte position with one-based line and zero-based column."""

    source_name: str
    offset: int
    line: int
    column: int

    @property
    def name(self) -> str:
        return self.source_name


@dataclass(frozen=True, slots=True)
class SourceSpan:
    """A byte slice and the diagnostic location of its first byte."""

    data: bytes
    location: SourceLocation

    @property
    def source_name(self) -> str:
        return self.location.source_name

    @property
    def offset(self) -> int:
        return self.location.offset

    @property
    def line(self) -> int:
        return self.location.line

    @property
    def column(self) -> int:
        return self.location.column

    @property
    def end_offset(self) -> int:
        return self.offset + len(self.data)


@dataclass(frozen=True, slots=True)
class DelimitedBytes:
    """Result of a raw delimiter scan.

    ``terminated`` distinguishes a delimiter consumed from an unterminated
    value that merely reached the end of the logical line.
    """

    data: bytes
    location: SourceLocation
    delimiter: int
    terminated: bool


@dataclass(frozen=True, slots=True)
class SourceLine:
    """One LF-delimited physical source line, without its CR/LF ending."""

    data: bytes
    source_name: str = "<input>"
    number: int = 1
    offset: int = 0

    def __post_init__(self) -> None:
        data = _freeze_bytes(self.data, argument="line data")
        if b"\n" in data:
            raise ValueError("SourceLine data cannot contain LF")
        _check_name(self.source_name)
        if self.number < 1:
            raise ValueError("source line number must be at least one")
        if self.offset < 0:
            raise ValueError("source line offset cannot be negative")
        object.__setattr__(self, "data", data)

    @property
    def name(self) -> str:
        return self.source_name

    @property
    def line(self) -> int:
        return self.number

    @property
    def location(self) -> SourceLocation:
        return SourceLocation(self.source_name, self.offset, self.number, 0)

    def cursor(self) -> SourceCursor:
        return SourceCursor(self)


@dataclass(frozen=True, slots=True)
class SourceBuffer:
    """An immutable byte source with repeatable physical-line iteration."""

    data: bytes
    name: str = "<input>"

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "data", _freeze_bytes(self.data, argument="source data")
        )
        _check_name(self.name)

    def lines(self) -> Iterator[SourceLine]:
        """Yield KDOS-compatible LF lines with an optional final CR removed.

        A trailing LF terminates the preceding line; it does not create a
        phantom additional line.  Empty lines between LF bytes are retained
        so diagnostic line numbers continue to match the original buffer.
        """

        start = 0
        number = 1
        size = len(self.data)
        while start < size:
            newline = self.data.find(b"\n", start)
            raw_end = size if newline < 0 else newline
            content_end = raw_end
            if content_end > start and self.data[content_end - 1] == 0x0D:
                content_end -= 1
            yield SourceLine(
                self.data[start:content_end],
                source_name=self.name,
                number=number,
                offset=start,
            )
            if newline < 0:
                return
            start = newline + 1
            number += 1

    def __iter__(self) -> Iterator[SourceLine]:
        return self.lines()


def iter_source_lines(
    data: _ByteSource, *, name: str = "<input>"
) -> Iterator[SourceLine]:
    """Iterate logical source lines without first constructing an AST."""

    return SourceBuffer(data, name=name).lines()


class SourceCursor:
    """A mutable, line-local byte cursor analogous to BIOS ``>IN``."""

    __slots__ = ("_column", "_last_token", "_line")

    def __init__(
        self,
        source: SourceLine | _ByteSource,
        *,
        source_name: str = "<input>",
        line_number: int = 1,
        offset: int = 0,
    ) -> None:
        if isinstance(source, SourceLine):
            if source_name != "<input>" or line_number != 1 or offset != 0:
                raise TypeError("metadata belongs on the supplied SourceLine")
            self._line = source
        else:
            self._line = SourceLine(
                source,
                source_name=source_name,
                number=line_number,
                offset=offset,
            )
        self._column = 0
        self._last_token: SourceSpan | None = None

    @property
    def data(self) -> bytes:
        return self._line.data

    @property
    def source_name(self) -> str:
        return self._line.source_name

    @property
    def name(self) -> str:
        return self.source_name

    @property
    def line(self) -> int:
        return self._line.number

    @property
    def line_number(self) -> int:
        return self.line

    @property
    def line_offset(self) -> int:
        return self._line.offset

    @property
    def column(self) -> int:
        return self._column

    @property
    def offset(self) -> int:
        return self.line_offset + self._column

    @property
    def location(self) -> SourceLocation:
        return SourceLocation(self.source_name, self.offset, self.line, self.column)

    @property
    def last_token(self) -> SourceSpan | None:
        return self._last_token

    @property
    def at_end(self) -> bool:
        return self._column >= len(self.data)

    @property
    def remaining(self) -> bytes:
        return self.data[self._column :]

    def seek(self, column: int) -> None:
        """Set the ``>IN``-like column.

        Positions beyond the line are retained because source can assign
        ``>IN`` directly and the BIOS treats such positions as end-of-line.
        """

        if not isinstance(column, int):
            raise TypeError("source column must be an integer")
        if column < 0:
            raise ValueError("source column cannot be negative")
        self._column = column
        self._last_token = None

    def consume_byte(self, expected: int | None = None) -> int | None:
        """Consume one raw byte, optionally only when it equals *expected*."""

        if expected is not None:
            _check_delimiter(expected)
        if self.at_end:
            return None
        value = self.data[self._column]
        if expected is not None and value != expected:
            return None
        self._column += 1
        return value

    def parse_word(self) -> bytes:
        """Parse the next BIOS word, returning ``b""`` at end-of-line.

        Only ASCII space (0x20) is skipped and delimits a word.  Tabs, CR, and
        every other byte are token content.  LF never appears here: line
        iteration owns the physical-line boundary.  As in the BIOS, the
        trailing space remains at ``>IN`` until the next parse skips it.
        """

        size = len(self.data)
        position = self._column
        if position > size:
            self._last_token = None
            return b""

        while position < size and self.data[position] == ASCII_SPACE:
            position += 1
        if position == size:
            self._column = position
            self._last_token = None
            return b""

        start = position
        while position < size and self.data[position] != ASCII_SPACE:
            position += 1
        self._column = position

        token = self.data[start:position]
        location = SourceLocation(
            self.source_name,
            self.line_offset + start,
            self.line,
            start,
        )
        self._last_token = SourceSpan(token, location)
        return token

    def parse_delimited_word(self, delimiter: int) -> DelimitedBytes:
        """Parse one ``WORD`` token using an arbitrary byte delimiter.

        Leading delimiters are skipped.  A trailing delimiter is consumed,
        while an unterminated token advances to the end of this physical
        line.  Unlike :meth:`parse_word`, this does not update diagnostic
        token state because BIOS ``WORD`` publishes a separate transient
        counted string at ``HERE``.
        """

        value, next_column = self.preview_delimited_word(delimiter)
        self._column = next_column
        return value

    def preview_delimited_word(
        self,
        delimiter: int,
    ) -> tuple[DelimitedBytes, int]:
        """Scan a ``WORD`` token without committing the input column.

        BIOS publishes ``>IN`` only after its transient dictionary write
        succeeds.  Returning the proposed next column lets that caller keep
        source position atomic with its external capacity check and write.
        """

        if not isinstance(delimiter, int) or not 0 <= delimiter <= MASK64:
            raise ValueError("WORD delimiter must be a uint64 cell")
        size = len(self.data)
        position = self._column
        if position > size:
            return (
                DelimitedBytes(b"", self.location, delimiter, False),
                position,
            )

        while position < size and self.data[position] == delimiter:
            position += 1
        start = position
        location = SourceLocation(
            self.source_name,
            self.line_offset + start,
            self.line,
            start,
        )
        while position < size and self.data[position] != delimiter:
            position += 1

        terminated = position < size
        end = position
        if terminated:
            position += 1
        return (
            DelimitedBytes(
                self.data[start:end],
                location,
                delimiter,
                terminated,
            ),
            position,
        )

    def consume_until(self, delimiter: int) -> DelimitedBytes:
        """Consume raw bytes through *delimiter*, without skipping anything.

        The returned data excludes the delimiter.  When it is present the
        cursor advances past it and ``terminated`` is true.  Otherwise the
        cursor advances to end-of-line and ``terminated`` is false, making an
        unterminated string visible to the evaluator rather than implicit.
        """

        _check_delimiter(delimiter)
        start = self._column
        location = self.location
        if start >= len(self.data):
            return DelimitedBytes(b"", location, delimiter, False)

        end = self.data.find(bytes((delimiter,)), start)
        if end < 0:
            self._column = len(self.data)
            return DelimitedBytes(self.data[start:], location, delimiter, False)

        self._column = end + 1
        return DelimitedBytes(self.data[start:end], location, delimiter, True)

    def skip_parenthesis_comment(self) -> bool:
        """Skip a BIOS ``(`` comment after its opening token was parsed.

        Parentheses nest even when adjacent to other bytes.  The BIOS confines
        the scan to the current input line, so false reports an unmatched
        opening parenthesis after consuming the remainder of this line.
        """

        depth = 1
        position = self._column
        size = len(self.data)
        while position < size:
            value = self.data[position]
            position += 1
            if value == 0x28:
                depth += 1
            elif value == 0x29:
                depth -= 1
                if depth == 0:
                    self._column = position
                    return True
        if self._column <= size:
            self._column = size
        return False

    def skip_backslash_comment(self) -> bytes:
        """Consume and return the bytes after a backslash-comment token."""

        if self.at_end:
            return b""
        skipped = self.data[self._column :]
        self._column = len(self.data)
        return skipped


__all__ = [
    "ASCII_SPACE",
    "DelimitedBytes",
    "SourceBuffer",
    "SourceCursor",
    "SourceLine",
    "SourceLocation",
    "SourceSpan",
    "iter_source_lines",
]
