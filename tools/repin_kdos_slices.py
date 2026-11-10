#!/usr/bin/env python3
"""Bring the simulator tests' kdos.f pins back in step after kdos.f changes.

The hosted-simulator tests pin kdos.f by position.  Each takes a slice of the
file, kept as a copy in tests/simulator/fixtures/kdos-<part>-<first>-<last>.f,
and holds the slice's line range, SHA-256 and git blob id, and usually its
byte length.  Some hold further line numbers, and a few pin the whole file.
An edit that adds or removes lines moves every later slice: its fixture has
to be cut again under a new name, and every line number after the edit has
to move with it.  This tool does that work.

    python3 tools/repin_kdos_slices.py           # after editing kdos.f
    python3 tools/repin_kdos_slices.py --check   # change nothing; exit 1 if stale

Procedure.  The base revision (--base, default HEAD) gives the old kdos.f,
the fixtures and the committed tests.  The working tree gives the new kdos.f.

1. `git diff` of the two versions of kdos.f gives the unchanged blocks, and
   a base line inside one has a new number.  A range that starts on the
   first line of a rewritten hunk starts where the hunk's new text starts,
   and one that ends on a hunk's last line ends where the new text ends.
   Any other line in a hunk has no new number until --place OLD=NEW gives
   it one.
2. Each fixture is cut again from the new file, between the new numbers of
   its first and last line, and renamed with `git mv`.  A slice whose text
   is the same keeps its hashes and its definition list.  A slice whose text
   changed gets its byte, hash and blob pins from the new text, and the
   summary names it, because its definition list, its counts and its
   behavior tests need a person.
3. Each pin in a pin file is found in the committed text by the rules below
   and set, in the working tree, to the value the new kdos.f gives.  A pin
   file is a tracked Python file that names a fixture or holds the whole
   file's hash, blob id, byte length or line count.  Every new value comes
   from the committed value, so a second run changes nothing and a run after
   a further edit starts from the same base.  The rest of a test keeps
   whatever the working tree has.

What counts as a pin:

- a fixture file name, anywhere in the text;
- `NAME = <int>` where NAME ends in FIRST_LINE or LAST_LINE;
- an integer written as an index or a slice bound of the line list a test
  makes from KDOS_SOURCE with splitlines();
- the two integers that follow a fixture in a tuple, a list or a call, when
  they are that fixture's range;
- a SHA-256 or git blob id of a range the file pins: a fixture it names, a
  slice of the line list between constant bounds, or the whole file;
- a byte length (`NAME_BYTES = <int>`, `len(x) == <int>`) or a line count
  (`NAME_LINES = <int>`, `x.count(b"\\n") == <int>`,
  `len(x.splitlines()) == <int>`) equal to that of such a range;
- a `kdos.f:<first>-<last>` label, or a comment that says "line N",
  "lines A through B" or "A–B", when the numbers are a range or line pins
  of the same file.

The summary lists what is left to a person: slices whose text changed, new
source that lies in no slice, pins on lines the edit rewrote, labels and
comments no pin confirms, and the pins that follow what kdos.f does when it
loads (BEHAVIOR_NAMES below), which only a run of the tests can set.

Exit status: 0 when everything is in step; 1 when --check finds a stale
fixture or pin, or when a pin has no new value; 2 when the base or the
working tree cannot be used.

Limits:

- The line map is git's.  Where an edit adds text between two slices, git
  cannot say which slice the text belongs to.  The summary shows the place
  and the --place arguments for either answer; give --check the same ones.
- Every value starts from the base revision.  A pin changed by hand in the
  working tree is set back to what the base and the map give.  A pin whose
  line the working tree rewrote before its last pin, or removed, is reported
  and not judged.
- A byte length or line count is recognized by its value.  In a test of a
  changed slice, another integer of the same form and value would follow
  the slice's; review that test's diff.
- A pin written in another form is not seen.  The tests check their own
  pins, so one the tool missed fails its test.
- A test or a fixture that the base revision does not hold is left alone.
- Prose in docs/ and simulator/README.md that quotes line ranges and hashes
  is not maintained.  Much of it describes earlier revisions.
"""

from __future__ import annotations

import argparse
import ast
import bisect
import difflib
import hashlib
import io
import posixpath
import re
import subprocess
import sys
import tokenize
from dataclasses import dataclass, field
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
KDOS = "kdos.f"

# The rules that say what a pin is.  Each names a form the tests use; a
# number in any other form is left alone.
FIXTURE_NAME = re.compile(
    r"(?<![\w-])kdos-(?P<part>[a-z0-9-]+?)-(?P<first>\d+)-(?P<last>\d+)\.f(?!\w)"
)
LINE_CONSTANT = re.compile(r"(?:^|_)(FIRST|LAST)_LINE$")
BYTE_CONSTANT = re.compile(r"_BYTES$")
COUNT_CONSTANT = re.compile(r"_LINES$")
LINE_LIST_ORIGIN = "KDOS_SOURCE"
LABEL = re.compile(r"kdos\.f(?:@\{[^{}]*\})?:(\d+)-(\d+)")
COMMENT_LINES = re.compile(r"\blines?\s+(\d+)(?:\s*(?:through|to|-|–)\s*(\d+))?")
COMMENT_RANGE = re.compile(r"(?<![\w.])(\d+)–(\d+)(?![\w.])")

# Pins that follow what kdos.f does when it loads: the lines the loader
# submits, the words it defines, the dictionary's bindings.  The tool never
# changes them.  It lists them when kdos.f changed.
BEHAVIOR_NAMES = frozenset({
    "SUBMITTED_LINES",
    "SUBMITTED_PAYLOAD_BYTES",
    "CLI_UART_BYTES",
    "PACKED_KDOS_BYTES",
    "MAX_SUBMITTED_LINE",
    "CORE_WORDS",
    "KDOS_WORDS",
    "UNIQUE_BINDINGS",
})
BEHAVIOR_ASSERT = re.compile(r"\blen\((?:core_words|loaded_words)\) == \d[\d_]*")
# Names the revision in source names.  No check reads it.
REVISION_NAME = "MEGAPAD_REVISION"

INTEGER = r"\d[\d_]*"
HASHES = (
    ("SHA-256 hash", r"[0-9a-f]{64}", lambda data: hashlib.sha256(data).hexdigest()),
    ("git blob id", r"[0-9a-f]{40}", lambda data: blob_id(data)),
)
# Every kind of pin, as the summary counts them.
KINDS = {
    "fixture name": "fixture names",
    "line number": "line numbers",
    "SHA-256 hash": "SHA-256 hashes",
    "git blob id": "git blob ids",
    "byte length": "byte lengths",
    "line count": "line counts",
    "label": "line numbers in labels",
    "comment": "line numbers in comments",
}
HUNK = re.compile(rb"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")
# Why a pin has no new value when it only waits for a range to get one.
NO_RANGE = "has no new range yet"


class RepinError(Exception):
    """The base revision or the working tree is not something the tool can use."""


def git(root: Path, *arguments: str) -> bytes:
    done = subprocess.run(
        ("git", "-C", str(root), *arguments), stdout=subprocess.PIPE, check=False
    )
    if done.returncode:
        raise RepinError(f"git {' '.join(arguments)} failed")
    return done.stdout


def blob_id(data: bytes) -> str:
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


def line_count(data: bytes) -> int:
    return data.count(b"\n")


def digits(value: int, like: str, *, grouped: bool = False) -> str:
    """Write VALUE as LIKE was written: with underscores or without."""
    if "_" in like or (grouped and len(like) < 4):
        return f"{value:_}"
    return str(value)


def counted(count: int, noun: str) -> str:
    return f"{count} {noun}" if count == 1 else f"{count} {noun}s"


@dataclass(frozen=True)
class Hunk:
    """One rewritten stretch of kdos.f.  With a count of zero, the first line
    is the one the insertion or removal comes before."""

    old_first: int
    old_count: int
    new_first: int
    new_count: int
    header: str = ""


class LineMap:
    """Base line numbers of kdos.f to new ones."""

    def __init__(self, old: list[bytes], new: list[bytes], hunks: list[Hunk],
                 placed: dict[int, int]) -> None:
        self.old, self.new, self.hunks, self.placed = old, new, hunks, placed
        self._starts: list[int] = []
        self._blocks: list[tuple[int, int, int]] = []
        old_at = new_at = 1
        for hunk in (*hunks, Hunk(len(old) + 1, 0, len(new) + 1, 0)):
            length = hunk.old_first - old_at
            same = (old[old_at - 1 : hunk.old_first - 1]
                    == new[new_at - 1 : hunk.new_first - 1])
            if length < 0 or length != hunk.new_first - new_at or not same:
                raise RepinError(
                    f"git diff's hunks do not match {KDOS} line for line")
            if length:
                self._starts.append(old_at)
                self._blocks.append((old_at, new_at, length))
            old_at = hunk.old_first + hunk.old_count
            new_at = hunk.new_first + hunk.new_count

    def where(self, line: int, side: str = "") -> int | None:
        """The new number of base line LINE, or None when the edit took it.

        SIDE says what the line bounds: "first" or "last" for an end of a
        range, "" for a line on its own, which survives a rewrite only of
        that one line into one line.
        """
        if line in self.placed:
            return self.placed[line]
        at = bisect.bisect_right(self._starts, line) - 1
        if at >= 0:
            old_first, new_first, length = self._blocks[at]
            if line < old_first + length:
                return new_first + line - old_first
        for hunk in self.hunks:
            if not hunk.old_first <= line < hunk.old_first + hunk.old_count:
                continue
            starts = line == hunk.old_first
            ends = line == hunk.old_first + hunk.old_count - 1
            new_last = hunk.new_first + hunk.new_count - 1
            if side == "first" and starts and hunk.new_first <= len(self.new):
                return hunk.new_first
            if side == "last" and ends and new_last >= 1:
                return new_last
            if not side and starts and ends and hunk.new_count == 1:
                return hunk.new_first
        return None


def read_line_map(root: Path, base: str, placed: dict[int, int]) -> LineMap:
    try:
        git(root, "rev-parse", "--verify", "--quiet", f"{base}^{{commit}}")
    except RepinError:
        raise RepinError(f"--base {base}: not a commit of this repository") from None
    old = git(root, "show", f"{base}:{KDOS}").splitlines(keepends=True)
    new = (root / KDOS).read_bytes().splitlines(keepends=True)
    diff = git(
        root, "diff", "--no-ext-diff", "--no-textconv", "--no-color", "--text",
        "--unified=0", "--inter-hunk-context=0", "--diff-algorithm=myers",
        "--indent-heuristic", base, "--", KDOS,
    )
    hunks = []
    for line in diff.splitlines():
        match = HUNK.match(line)
        if match is None:
            continue
        old_count = 1 if match[2] is None else int(match[2])
        new_count = 1 if match[4] is None else int(match[4])
        # With a count of zero, git names the line the change comes after.
        hunks.append(Hunk(
            int(match[1]) + (not old_count), old_count,
            int(match[3]) + (not new_count), new_count, match[0].decode(),
        ))
    for line, target in placed.items():
        if not (1 <= line <= len(old) and 1 <= target <= len(new)):
            raise RepinError(
                f"--place {line}={target}: the base has {len(old)} lines and "
                f"the working tree {len(new)}")
    return LineMap(old, new, hunks, placed)


@dataclass
class Span:
    """Lines of kdos.f that a test pins as one text."""

    first: int
    last: int
    new_first: int | None
    new_last: int | None
    what: str

    def old_text(self, lines: LineMap) -> bytes:
        return b"".join(lines.old[self.first - 1 : self.last])

    def new_text(self, lines: LineMap) -> bytes | None:
        first, last = self.new_first, self.new_last
        if first is None or last is None or not 1 <= first <= last <= len(lines.new):
            return None
        return b"".join(lines.new[first - 1 : last])

    def bounds(self) -> tuple[int, int, int | None, int | None]:
        return self.first, self.last, self.new_first, self.new_last


@dataclass
class Slice:
    """A fixture at the base, and where its slice lies in the new kdos.f."""

    path: str
    part: str
    span: Span
    old_text: bytes
    new_text: bytes | None
    sound: bool  # the base's fixture holds the lines its name gives
    pin_files: list[str] = field(default_factory=list)
    was: str = ""  # how the working tree stood when the run began

    @property
    def name(self) -> str:
        return posixpath.basename(self.path)

    @property
    def new_name(self) -> str | None:
        if self.new_text is None:
            return None
        return f"kdos-{self.part}-{self.span.new_first}-{self.span.new_last}.f"

    @property
    def new_path(self) -> str | None:
        if self.new_text is None:
            return None
        return posixpath.join(posixpath.dirname(self.path), self.new_name)

    @property
    def changed(self) -> bool:
        return self.new_text is not None and self.new_text != self.old_text

    def state(self, root: Path) -> str:
        """How the working tree's fixture stands against the plan."""
        old = root / self.path
        if not self.sound:
            return "unsound"
        if self.new_path is None:
            return "unplaced" if old.is_file() else "by hand"
        new = root / self.new_path
        if not new.is_file() or new.read_bytes() != self.new_text:
            return "stale"
        moved_away = self.new_path == self.path or not old.is_file()
        return "in step" if moved_away else "stale"


def read_slices(root: Path, base: str, lines: LineMap) -> list[Slice]:
    slices = []
    for entry in git(root, "ls-tree", "-r", "-z", base).split(b"\0"):
        meta, _, path = entry.decode().partition("\t")
        match = FIXTURE_NAME.fullmatch(posixpath.basename(path))
        if match is None:
            continue
        first, last = int(match["first"]), int(match["last"])
        span = Span(first, last, lines.where(first, "first"),
                    lines.where(last, "last"), posixpath.basename(path))
        old_text = span.old_text(lines)
        sound = blob_id(old_text) == meta.split()[2]
        slices.append(Slice(path, match["part"], span, old_text,
                            span.new_text(lines) if sound else None, sound))
    if len({each.name for each in slices}) != len(slices):
        raise RepinError("two fixtures at the base share one file name")
    for each in slices:
        each.was = each.state(root)
    return sorted(slices, key=lambda each: each.span.bounds()[:2])


@dataclass
class Site:
    """One pin in a committed file: where it is and what it should say."""

    row: int
    start: int
    end: int
    kind: str
    old: str
    new: str | None  # None: the tool cannot tell what belongs here
    mask: str  # every value a pin of this kind can take
    why: str = ""  # for a pin with no new value
    current: str | None = None  # the working tree's value; None: line gone

    @property
    def state(self) -> str:
        if self.current is None:
            return "gone"
        if self.new is None:
            return "unplaced" if self.current == self.old else "by hand"
        return "in step" if self.current == self.new else "stale"


@dataclass
class PinFile:
    path: str
    committed: list[str]
    sites: list[Site] = field(default_factory=list)
    spans: list[Span] = field(default_factory=list)  # every range it pins
    list_spans: list[Span] = field(default_factory=list)  # lines[a:b] ranges
    notes: list[tuple[int, str]] = field(default_factory=list)
    behavior: list[tuple[int, str]] = field(default_factory=list)
    names_revision: bool = False
    working: list[str] | None = None
    result: list[str] | None = None


def split_lines(text: str) -> list[str]:
    """Split at line feeds only, as the Python parser counts lines."""
    return re.findall(r"[^\n]*\n|[^\n]+", text)


def is_integer(node: ast.AST | None) -> bool:
    return isinstance(node, ast.Constant) and type(node.value) is int


def mentions(node: ast.AST, name: str) -> bool:
    return any(isinstance(each, ast.Name) and each.id == name
               for each in ast.walk(node))


def assignment(node: ast.AST) -> tuple[str | None, ast.AST | None]:
    """The name and value of `NAME = value`, or (None, None)."""
    if isinstance(node, ast.Assign) and len(node.targets) == 1:
        target, value = node.targets[0], node.value
    elif isinstance(node, ast.AnnAssign):
        target, value = node.target, node.value
    else:
        return None, None
    return (target.id, value) if isinstance(target, ast.Name) else (None, None)


def evaluate(node: ast.AST | None, values: dict[str, int]) -> int | None:
    """Evaluate a bound made of names, integers, plus and minus."""
    if is_integer(node):
        return node.value
    if isinstance(node, ast.Name):
        return values.get(node.id)
    if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Sub)):
        left, right = evaluate(node.left, values), evaluate(node.right, values)
        if left is None or right is None:
            return None
        return left + right if isinstance(node.op, ast.Add) else left - right
    return None


class PinFinder:
    """Find the pins of one committed file and what each should say now."""

    def __init__(self, path: str, text: str, slices: dict[str, Slice],
                 lines: LineMap) -> None:
        self.file = PinFile(path, split_lines(text))
        self.slices = slices
        self.lines = lines
        self.tree = ast.parse(text)
        self.nodes = list(ast.walk(self.tree))
        self.anchors: dict[int, str] = {}  # each pinned base line and its side
        self.taken: dict[int, list[tuple[int, int]]] = {}
        # Module-level integers, at the base and as they will be.
        self.old_values: dict[str, int] = {}
        self.new_values: dict[str, int] = {}
        self.measures: list[tuple[ast.Constant, str]] = []

    def find(self) -> PinFile:
        self.fixture_names()
        self.constants()
        self.line_list_bounds()
        self.fixture_ranges()
        self.file.spans.append(Span(
            1, len(self.lines.old), 1, len(self.lines.new), f"all of {KDOS}"))
        self.hashes()
        self.lengths_and_counts()
        self.labels()
        self.comments()
        self.file.behavior += (
            (row + 1, line.strip()) for row, line in enumerate(self.file.committed)
            if BEHAVIOR_ASSERT.search(line))
        self.file.behavior.sort()
        return self.file

    def add(self, row: int, start: int, end: int, kind: str, new: str | None,
            mask: str, why: str = "") -> None:
        taken = self.taken.setdefault(row, [])
        if any(start < other_end and other_start < end
               for other_start, other_end in taken):
            return
        taken.append((start, end))
        old = self.file.committed[row][start:end]
        self.file.sites.append(Site(row, start, end, kind, old, new, mask, why))

    def add_number(self, row: int, start: int, end: int, kind: str,
                   target: int | None, why: str, *, grouped: bool = False) -> None:
        old = self.file.committed[row][start:end]
        if re.fullmatch(INTEGER, old):
            new = None if target is None else digits(target, old, grouped=grouped)
            self.add(row, start, end, kind, new, INTEGER, why)

    def add_line(self, row: int, start: int, end: int, line: int, side: str,
                 kind: str = "line number", offset: int = 0) -> int | None:
        """A site that holds base line LINE, written as LINE + OFFSET."""
        self.anchors.setdefault(line, side)
        target = self.lines.where(line, side)
        self.add_number(row, start, end, kind,
                        None if target is None else target + offset,
                        f"base line {line} was rewritten or removed")
        return target

    def place(self, node: ast.AST) -> tuple[int, int, int]:
        """The row and character columns of a one-line node."""
        row = node.lineno - 1
        encoded = self.file.committed[row].encode("utf-8")
        return (row, len(encoded[: node.col_offset].decode("utf-8")),
                len(encoded[: node.end_col_offset].decode("utf-8")))

    def note(self, row: int, message: str) -> None:
        self.file.notes.append((row + 1, message))

    # The rules, in the order of the module docstring.

    def fixture_names(self) -> None:
        for row, line in enumerate(self.file.committed):
            for match in FIXTURE_NAME.finditer(line):
                each = self.slices.get(match[0])
                if each is None:
                    self.note(row, f"names {match[0]}, which the base does not hold")
                    continue
                if self.file.path not in each.pin_files:
                    each.pin_files.append(self.file.path)
                self.add(row, match.start(), match.end(), "fixture name",
                         each.new_name, rf"kdos-{re.escape(each.part)}-\d+-\d+\.f",
                         f"the slice {NO_RANGE}")
                self.anchors.setdefault(each.span.first, "first")
                self.anchors.setdefault(each.span.last, "last")
                if each.span not in self.file.spans:
                    self.file.spans.append(each.span)

    def constants(self) -> None:
        module_level = {id(node) for node in self.tree.body}
        seen: set[str] = set()
        for node in self.nodes:
            name, value = assignment(node)
            self.file.names_revision |= name == REVISION_NAME
            if name is None or not is_integer(value):
                continue
            row, start, end = self.place(value)
            if name in BEHAVIOR_NAMES:
                self.file.behavior.append((row + 1, self.file.committed[row].strip()))
                continue
            new_value: int | None = value.value
            line = LINE_CONSTANT.search(name)
            if line:
                new_value = self.add_line(row, start, end, value.value,
                                          line[1].lower())
            elif name.endswith("_LINE"):
                self.note(row, f"{name} ends in _LINE but is not a FIRST_LINE "
                               "or LAST_LINE pin; left as it is")
            elif BYTE_CONSTANT.search(name):
                self.measures.append((value, "byte length"))
            elif COUNT_CONSTANT.search(name):
                self.measures.append((value, "line count"))
            if id(node) not in module_level:
                continue
            if name in seen:  # bound twice: no single value to evaluate with
                self.old_values.pop(name, None)
                self.new_values.pop(name, None)
            else:
                seen.add(name)
                self.old_values[name] = value.value
                if new_value is not None:
                    self.new_values[name] = new_value

    def line_lists(self, scope: list[ast.AST]) -> set[str]:
        """The names a function binds to kdos.f's lines."""
        holders, lists = set(), set()
        for node in scope:
            name, value = assignment(node)
            if name and value is not None and mentions(value, LINE_LIST_ORIGIN):
                holders.add(name)
        for node in scope:
            name, value = assignment(node)
            if (name and isinstance(value, ast.Call)
                    and isinstance(value.func, ast.Attribute)
                    and value.func.attr == "splitlines"):
                receiver = value.func.value
                if mentions(receiver, LINE_LIST_ORIGIN) or (
                        isinstance(receiver, ast.Name) and receiver.id in holders):
                    lists.add(name)
        return lists

    def line_list_bounds(self) -> None:
        traced: set[int] = set()
        for function in self.nodes:
            if not isinstance(function, (ast.FunctionDef, ast.AsyncFunctionDef)):
                continue
            scope = list(ast.walk(function))
            lists = self.line_lists(scope)
            for node in scope:
                if (isinstance(node, ast.Subscript) and id(node) not in traced
                        and isinstance(node.value, ast.Name)
                        and node.value.id in lists):
                    traced.add(id(node))
                    self.line_list_subscript(node)
        for node in self.nodes:
            if (isinstance(node, ast.Subscript) and id(node) not in traced
                    and isinstance(node.value, ast.Name)
                    and re.search(r"(?:^|_)lines$", node.value.id)):
                inside = node.slice
                bounds = ([inside.lower, inside.upper]
                          if isinstance(inside, ast.Slice) else [inside])
                if any(is_integer(bound) for bound in bounds):
                    self.note(node.lineno - 1,
                              f"{node.value.id}[...] holds an integer, but the "
                              f"list is not traced to {LINE_LIST_ORIGIN}; "
                              "left as it is")

    def line_list_subscript(self, node: ast.Subscript) -> None:
        inside = node.slice
        if not isinstance(inside, ast.Slice):
            if is_integer(inside):  # lines[n] is line n + 1
                self.add_line(*self.place(inside), inside.value + 1, "", offset=-1)
            return
        # lines[low:high] is lines low + 1 through high.
        old_low, new_low, old_high, new_high = 0, 0, len(self.lines.old), len(
            self.lines.new)
        if inside.lower is not None:
            old_low = evaluate(inside.lower, self.old_values)
            new_low = evaluate(inside.lower, self.new_values)
        if inside.upper is not None:
            old_high = evaluate(inside.upper, self.old_values)
            new_high = evaluate(inside.upper, self.new_values)
        if is_integer(inside.lower):
            target = self.add_line(*self.place(inside.lower), old_low + 1,
                                   "first", offset=-1)
            new_low = None if target is None else target - 1
        if is_integer(inside.upper):
            new_high = self.add_line(*self.place(inside.upper), old_high, "last")
        if old_low is None or old_high is None or old_low >= old_high:
            return
        span = Span(old_low + 1, old_high,
                    None if new_low is None else new_low + 1, new_high,
                    f"lines {old_low + 1}-{old_high}")
        if all(each.bounds() != span.bounds() for each in self.file.spans):
            self.file.spans.append(span)
            self.file.list_spans.append(span)

    def fixture_ranges(self) -> None:
        """`FIXTURE, first, last` in a tuple, a list or a call."""
        named: dict[str, Slice] = {}
        for node in self.tree.body:
            name, _value = assignment(node)
            text = "".join(self.file.committed[node.lineno - 1 : node.end_lineno])
            found = {match[0] for match in FIXTURE_NAME.finditer(text)}
            if name and len(found) == 1 and found <= self.slices.keys():
                named[name] = self.slices[found.pop()]
        for node in self.nodes:
            if isinstance(node, (ast.Tuple, ast.List)):
                items = node.elts
            elif isinstance(node, ast.Call):
                items = node.args
            else:
                continue
            for at, item in enumerate(items[:-2]):
                first, last = items[at + 1], items[at + 2]
                if not (isinstance(item, ast.Name) and item.id in named
                        and is_integer(first) and is_integer(last)):
                    continue
                span = named[item.id].span
                if (first.value, last.value) != (span.first, span.last):
                    self.note(first.lineno - 1,
                              f"{first.value}, {last.value} follow {item.id} "
                              "but are not its range; left as they are")
                    continue
                self.add_line(*self.place(first), first.value, "first")
                self.add_line(*self.place(last), last.value, "last")

    def measured(self, metric) -> dict[object, list[Span]]:
        """The file's ranges by what METRIC makes of their base text."""
        table: dict[object, list[Span]] = {}
        for span in self.file.spans:
            table.setdefault(metric(span.old_text(self.lines)), []).append(span)
        return table

    def new_measure(self, spans: list[Span], metric) -> tuple[object, str]:
        values = set()
        for span in spans:
            text = span.new_text(self.lines)
            if text is None:
                return None, f"{span.what} {NO_RANGE}"
            values.add(metric(text))
        if len(values) > 1:
            return None, "it pins ranges that no longer agree: " + ", ".join(
                span.what for span in spans)
        return values.pop(), ""

    def hashes(self) -> None:
        for kind, mask, metric in HASHES:
            table = self.measured(metric)
            pattern = re.compile(rf"(?<![0-9A-Za-z]){mask}(?![0-9A-Za-z])")
            for row, line in enumerate(self.file.committed):
                for match in pattern.finditer(line):
                    if match[0] in table:
                        new, why = self.new_measure(table[match[0]], metric)
                        self.add(row, *match.span(), kind, new, mask, why)

    def lengths_and_counts(self) -> None:
        for node in self.nodes:
            if not (isinstance(node, ast.Compare) and len(node.ops) == 1
                    and isinstance(node.ops[0], (ast.Eq, ast.NotEq))):
                continue
            pair = (node.left, node.comparators[0])
            for call, literal in (pair, pair[::-1]):
                if not (is_integer(literal) and isinstance(call, ast.Call)):
                    continue
                function, arguments = call.func, call.args
                inner = arguments[0] if len(arguments) == 1 else None
                splits = (isinstance(inner, ast.Call)
                          and isinstance(inner.func, ast.Attribute)
                          and inner.func.attr == "splitlines")
                if isinstance(function, ast.Name) and function.id == "len":
                    if isinstance(inner, ast.Name):
                        self.measures.append((literal, "byte length"))
                    elif splits:
                        self.measures.append((literal, "line count"))
                elif (isinstance(function, ast.Attribute)
                      and function.attr == "count"
                      and isinstance(inner, ast.Constant) and inner.value == b"\n"):
                    self.measures.append((literal, "line count"))
        metrics = {"byte length": len, "line count": line_count}
        tables = {kind: self.measured(metric) for kind, metric in metrics.items()}
        for literal, kind in self.measures:
            if literal.value in tables[kind]:
                value, why = self.new_measure(
                    tables[kind][literal.value], metrics[kind])
                self.add_number(*self.place(literal), kind, value, why,
                                grouped=True)

    def confirmed(self, first: int, last: int) -> tuple | None:
        """Where a range that a label or comment gives now lies, when a
        range or two line pins of this file confirm it."""
        for span in self.file.spans:
            if (span.first, span.last) == (first, last):
                return span.new_first, span.new_last
        if first in self.anchors and last in self.anchors:
            return (self.lines.where(first, "first"),
                    self.lines.where(last, "last"))
        return None

    def mention(self, row: int, column: int, match: re.Match, kind: str) -> bool:
        """Place the one or two line numbers MATCH holds, if confirmed."""
        why = "its base line was rewritten or removed"
        if match[2] is None:
            line = int(match[1])
            if line not in self.anchors:
                return False
            start, end = match.span(1)
            self.add_number(row, column + start, column + end, kind,
                            self.lines.where(line, self.anchors[line]), why)
            return True
        targets = self.confirmed(int(match[1]), int(match[2]))
        if targets is None:
            return False
        for group, target in zip((1, 2), targets):
            start, end = match.span(group)
            self.add_number(row, column + start, column + end, kind, target, why)
        return True

    def labels(self) -> None:
        for row, line in enumerate(self.file.committed):
            for match in LABEL.finditer(line):
                if not self.mention(row, 0, match, "label"):
                    self.note(row, f"label {match[0]} matches no range this "
                                   "file pins; left as it is")

    def comments(self) -> None:
        text = "".join(self.file.committed)
        for token in tokenize.generate_tokens(io.StringIO(text).readline):
            if token.type != tokenize.COMMENT:
                continue
            row, column = token.start[0] - 1, token.start[1]
            for match in COMMENT_LINES.finditer(token.string):
                if not self.mention(row, column, match, "comment"):
                    self.note(row, f'comment says "{match[0]}", which no pin '
                                   "of this file confirms; left as it is")
            for match in COMMENT_RANGE.finditer(token.string):
                self.mention(row, column, match, "comment")


def row_pattern(line: str, sites: list[Site]) -> re.Pattern[str]:
    """LINE through its last pin, with each pin opened to any value."""
    parts, at = [], 0
    for site in sites:
        parts += [re.escape(line[at : site.start]), f"({site.mask})"]
        at = site.end
    return re.compile("".join(parts) + r"(?!\w)", re.DOTALL)


def carry(file: PinFile) -> None:
    """Set every pin in the working lines and record what each one held.

    A pin's line is found in the working tree by aligning the two versions
    of the file.  A line the working tree changed still counts when it reads
    as the committed line does through its last pin, whatever values the
    pins hold.
    """
    committed, working = file.committed, file.working
    rows: dict[int, list[Site]] = {}
    for site in sorted(file.sites, key=lambda site: (site.row, site.start)):
        rows.setdefault(site.row, []).append(site)
    patterns = {row: row_pattern(committed[row], sites)
                for row, sites in rows.items()}
    found: dict[int, int] = {}
    if committed == working:
        found = {row: row for row in rows}
    else:
        matcher = difflib.SequenceMatcher(None, committed, working, autojunk=False)
        for tag, old_from, old_to, new_from, new_to in matcher.get_opcodes():
            at = new_from  # a rewritten block is searched in order
            for row in range(old_from, old_to):
                if row not in rows or tag == "delete":
                    continue
                if tag == "equal":
                    candidates = range(new_from + row - old_from, new_to)[:1]
                else:
                    candidates = range(at, new_to)
                hit = next((index for index in candidates
                            if patterns[row].match(working[index])), None)
                if hit is not None:
                    found[row], at = hit, hit + 1
    file.result = list(working)
    for row, index in found.items():
        line = working[index]
        match = patterns[row].match(line)
        parts, at = [], 0
        for group, site in enumerate(rows[row], start=1):
            site.current = match[group]
            parts += [line[at : match.start(group)],
                      site.current if site.new is None else site.new]
            at = match.end(group)
        file.result[index] = "".join(parts) + line[at:]


def read_pin_files(root: Path, base: str, slices: list[Slice],
                   lines: LineMap) -> tuple[list[PinFile], list[str]]:
    whole = b"".join(lines.old)
    pattern = "|".join((
        r"kdos-[a-z0-9-]+-[0-9]+-[0-9]+\.f", "KDOS_BYTES", "KDOS_LINES",
        *(metric(whole) for _kind, _mask, metric in HASHES),
    ))
    listed = subprocess.run(
        ("git", "-C", str(root), "grep", "-l", "-z", "-I", "-E", "-e", pattern,
         base, "--", "*.py"),
        stdout=subprocess.PIPE, check=False,
    ).stdout.decode()
    by_name = {each.name: each for each in slices}
    files, problems = [], []
    for entry in sorted(filter(None, listed.split("\0"))):
        path = entry[len(base) + 1 :]
        try:
            text = git(root, "show", f"{base}:{path}").decode("utf-8")
            file = PinFinder(path, text, by_name, lines).find()
        except (SyntaxError, ValueError, tokenize.TokenError) as error:
            problems.append(f"{path}: the base's text does not parse ({error})")
            continue
        if not (file.sites or file.notes or file.behavior):
            continue
        if (root / path).is_file():
            with open(root / path, encoding="utf-8", newline="") as handle:
                file.working = split_lines(handle.read())
            carry(file)
        files.append(file)
    return files, problems


def uncovered_source(lines: LineMap, slices: list[Slice]) -> list[str]:
    """New source that KDOS loads and that lies in no slice."""
    placed = [each for each in slices if each.new_text is not None]
    if len(placed) < len(slices):
        return []  # an unplaced slice leaves its own lines uncovered

    def covered(line: int) -> bool:
        return any(each.span.new_first <= line <= each.span.new_last
                   for each in placed)

    def blank(line: int) -> bool:
        return not lines.new[line - 1].strip()

    def loads(line: int) -> bool:
        return not blank(line) and not lines.new[line - 1].lstrip().startswith(b"\\")

    reports = []
    for hunk in lines.hunks:
        run: list[int] = []
        for line in (*range(hunk.new_first, hunk.new_first + hunk.new_count), 0):
            if line and not covered(line):
                run.append(line)
                continue
            while run and blank(run[-1]):
                run.pop()
            while run and blank(run[0]):
                run.pop(0)
            if any(loads(each) for each in run):
                before = max((each for each in placed
                              if each.span.new_last < run[0]),
                             key=lambda each: each.span.new_last, default=None)
                after = min((each for each in placed
                             if each.span.new_first > run[-1]),
                            key=lambda each: each.span.new_first, default=None)
                text = (f"new line {run[0]} lies" if len(run) == 1
                        else f"new lines {run[0]}-{run[-1]} lie") + " in no slice"
                if before is not None:
                    text += (f"\n        to end {before.name} there: "
                             f"--place {before.span.last}={run[-1]}")
                if after is not None:
                    text += (f"\n        to start {after.name} there: "
                             f"--place {after.span.first}={run[0]}")
                reports.append(text)
            run = []
    return reports


def stray_fixtures(root: Path, slices: list[Slice]) -> list[str]:
    """Fixture-named files that are neither the base's nor this run's."""
    known = {each.path for each in slices}
    known |= {each.new_path for each in slices if each.new_path}
    strays = []
    for folder in sorted({posixpath.dirname(each.path) for each in slices}):
        strays += (
            posixpath.join(folder, path.name)
            for path in sorted((root / folder).glob("kdos-*.f"))
            if FIXTURE_NAME.fullmatch(path.name)
            and posixpath.join(folder, path.name) not in known
        )
    return strays


def earlier_copy(root: Path, each: Slice, slices: list[Slice]) -> str | None:
    """The file an earlier run made of a slice whose base file is gone.

    It is a stray of the same part, in the index as `git mv` left it, that
    holds the base text, or the only such stray when one slice of the part
    lacks its file.
    """
    indexed = set(git(root, "ls-files", "-z").decode().split("\0"))
    strays = [path for path in stray_fixtures(root, slices)
              if FIXTURE_NAME.fullmatch(posixpath.basename(path))["part"] == each.part
              and path in indexed]
    same = [path for path in strays if (root / path).read_bytes() == each.old_text]
    lacking = [other for other in slices
               if other.part == each.part and other.was == "stale"
               and not (root / other.path).exists()
               and not (root / other.new_path).exists()]
    if same:
        return same[0]
    return strays[0] if len(strays) == 1 and len(lacking) == 1 else None


def move_fixtures(root: Path, slices: list[Slice]) -> list[str]:
    """Rename and rewrite the stale fixtures; return what could not be done."""
    waiting = [each for each in slices if each.was == "stale"]
    moving = True
    while waiting and moving:
        moving = False
        for each in list(waiting):
            new = root / each.new_path
            source = each.path
            if not (root / source).exists():
                source = earlier_copy(root, each, slices)
            if source not in (None, each.new_path):
                if new.exists():
                    continue  # another slice's old file may still move away
                git(root, "mv", source, each.new_path)
            created = not new.exists()
            if created or new.read_bytes() != each.new_text:
                new.write_bytes(each.new_text)
            if created:
                git(root, "add", each.new_path)
            waiting.remove(each)
            moving = True
    return [f"{each.path}: cannot become {each.new_name}, which already exists"
            for each in waiting]


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Bring the simulator tests' kdos.f pins back in step "
                    "after kdos.f changes.",
        epilog="The module docstring gives the procedure, the rules that say "
               "what a pin is, and the limits.",
    )
    parser.add_argument(
        "--base", default="HEAD", metavar="REVISION",
        help="revision whose kdos.f, fixtures and tests agree (default: HEAD)",
    )
    parser.add_argument(
        "--check", action="store_true",
        help="change nothing; exit 1 if a fixture or a pin is stale",
    )
    parser.add_argument(
        "--place", action="append", default=[], metavar="OLD=NEW",
        help="put base line OLD at new line NEW, for a line the edit rewrote "
             "or a slice that should end or start elsewhere; repeatable",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="list every hunk, every renamed fixture and every file changed",
    )
    return parser


def section(title: str, items: list[str]) -> None:
    if items:
        print(title)
        for item in items:
            print(f"    {item}")


def report(root: Path, base: str, lines: LineMap, slices: list[Slice],
           files: list[PinFile], problems: list[str], *, check: bool,
           verbose: bool) -> int:
    """Print the summary; return the exit status."""
    sites = [(file, site) for file in files for site in file.sites]
    stale_files = [file for file in files
                   if file.working is not None and file.result != file.working]
    stale_slices = [each for each in slices if each.was == "stale"]

    if lines.hunks:
        print(f"{KDOS}: {len(lines.old)} lines at {base}, {len(lines.new)} in "
              f"the working tree, {counted(len(lines.hunks), 'hunk')}")
        shown = lines.hunks if verbose else lines.hunks[:6]
        for hunk in shown:
            print(f"    {hunk.header}")
        if len(shown) < len(lines.hunks):
            print(f"    ... and {len(lines.hunks) - len(shown)} more")
    else:
        print(f"{KDOS}: as at {base}, {len(lines.old)} lines")
    for line, target in sorted(lines.placed.items()):
        print(f"    base line {line} placed at new line {target}")

    changed = [each for each in slices if each.changed]
    moved = [each for each in slices
             if each.new_path not in (None, each.path) and not each.changed]
    print(f"Slices: {len(slices)} fixtures at {base}" + (
        f": {len(changed)} with changed text, {len(moved)} moved with the "
        "same text" if lines.hunks else ""))
    if verbose:
        for each in slices:
            if each.new_path not in (None, each.path):
                print(f"    {each.name} -> {each.new_name}")
    print(f"Pins: {len(sites)} in {counted(len(files), 'file')}")
    for kind, plural in KINDS.items():
        total = sum(site.kind == kind for _file, site in sites)
        stale = sum(site.kind == kind and site.state == "stale"
                    for _file, site in sites)
        if total:
            print(f"    {total:4} {plural}"
                  + (f", {stale} {'stale' if check else 'set'}" if stale else ""))
    if verbose:
        for file in stale_files:
            stale = sum(site.state == "stale" for site in file.sites)
            print(f"    {file.path}: {counted(stale, 'pin')}")

    person = [
        f"{each.name} -> {each.new_name}: the text changed; check its "
        "definition list, counts and behavior tests in\n        "
        + ", ".join(each.pin_files)
        for each in changed
    ]
    for file in files:
        own = [each.span for each in changed if file.path in each.pin_files]
        for span in file.list_spans:
            text = span.new_text(lines)
            if text not in (None, span.old_text(lines)) and not any(
                    each.first <= span.first and span.last <= each.last
                    for each in own):
                person.append(f"{file.path}: {span.what} of the base changed "
                              f"text, now {span.new_first}-{span.new_last}; "
                              "check what the test expects of them")
    person += uncovered_source(lines, slices)
    # What follows has no new value, so the run cannot finish it.
    blocked = []
    for each in slices:
        if each.was == "unplaced":
            ends = [line for line, new in ((each.span.first, each.span.new_first),
                                           (each.span.last, each.span.new_last))
                    if new is None] or [each.span.first, each.span.last]
            blocked.append(
                f"{each.name}: the edit rewrote or removed an end of the slice; "
                "say where it lies now with "
                + " ".join(f"--place {line}=NEW" for line in ends))
    blocked += (f"{each.path} does not hold lines {each.span.first}-"
                f"{each.span.last} of {base}'s {KDOS}; left alone"
                for each in slices if each.was == "unsound")
    # A pin that only waits for a range is listed with the range's own ends.
    blocked += (f"{file.path}:{site.row + 1}: {site.kind} {site.old}: {site.why}"
                for file, site in sites
                if site.state == "unplaced" and not site.why.endswith(NO_RANGE))
    blocked += problems
    section("Needs a person:", person + blocked)

    behavior = [f"{file.path}:{row}: {text}"
                for file in files for row, text in file.behavior]
    if lines.hunks:
        section("Left alone, because they follow what kdos.f does when it "
                "loads:", behavior)
    elif behavior:
        print(f"Left alone: {counted(len(behavior), 'pin')} that follow what "
              "kdos.f does when it loads")
    revisions = sum(file.names_revision for file in files)
    if revisions:
        print(f"Left alone: {REVISION_NAME} in {counted(revisions, 'file')}. "
              "It names a revision whose line numbers the source names give; "
              "only a later commit can name this one.")

    reported = [f"{file.path}:{row}: {text}"
                for file in files for row, text in file.notes]
    for file in files:
        if file.working is None:
            reported.append(f"{file.path}: gone from the working tree")
            continue
        gone = [str(site.row + 1) for site in file.sites
                if site.state == "gone" and site.new != site.old]
        if gone:
            reported.append(f"{file.path}: {counted(len(gone), 'pin')} on lines "
                            "the working tree rewrote or removed (base lines "
                            f"{', '.join(gone)}); not judged")
        by_hand = sum(site.state == "by hand" for site in file.sites)
        if by_hand:
            reported.append(f"{file.path}: {counted(by_hand, 'pin')} with no "
                            "new value, already edited by hand; not judged")
    reported += (f"{each.path}: no new range, and gone from the working tree; "
                 "not judged" for each in slices if each.was == "by hand")
    reported += (f"{path}: not a fixture of {base} or of this run; left alone"
                 for path in stray_fixtures(root, slices))
    section("Reported, not changed:", reported)

    stale = bool(stale_slices or stale_files)
    work = (f"{counted(len(stale_slices), 'fixture')} and "
            f"{counted(len(stale_files), 'file')}")
    if check and stale:
        print(f"Stale: {work}. Run tools/repin_kdos_slices.py to bring them "
              "in step.")
    elif not check:
        print(f"Brought in step: {work}." if stale else "Nothing to change.")
    if blocked:
        print("Some pins have no new value yet; see what needs a person.")
    elif check and not stale:
        print("In step.")
    return 1 if blocked or (check and stale) else 0


def main(argv: list[str] | None = None, root: Path = ROOT) -> int:
    arguments = build_parser().parse_args(argv)
    base = arguments.base
    try:
        placed = {}
        for item in arguments.place:
            if not re.fullmatch(r"\d+=\d+", item):
                raise RepinError(f"--place {item}: expected OLD=NEW line numbers")
            placed[int(item.split("=")[0])] = int(item.split("=")[1])
        lines = read_line_map(root, base, placed)
        slices = read_slices(root, base, lines)
        files, problems = read_pin_files(root, base, slices, lines)
        if not arguments.check:
            problems += move_fixtures(root, slices)
            for file in files:
                if file.working is not None and file.result != file.working:
                    with open(root / file.path, "w", encoding="utf-8",
                              newline="") as out:
                        out.write("".join(file.result))
    except (RepinError, OSError) as error:
        print(f"repin_kdos_slices: {error}", file=sys.stderr)
        return 2
    return report(root, base, lines, slices, files, problems,
                  check=arguments.check, verbose=arguments.verbose)


if __name__ == "__main__":
    sys.exit(main())
