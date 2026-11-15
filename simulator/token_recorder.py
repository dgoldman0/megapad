"""Record what the interpreter does with each source file as it loads.

A recorder attached to a runtime watches KDOS load files from source.  For
each file it notes, in order, the words the interpreter found, the numbers
it converted, the text each word took from its line when it ran, and where
conditional compilation skipped.  That record, written out by
``shared.token_form``, is the file's token form: ``TOKEN-REPLAY`` runs it
through the interpreter's own steps without the text.

The record is made by the interpreter that loads the source, so no second
reader of Forth exists.  A file is the buffer one walk of KDOS's loader or of
its checked source evaluator covers: ``_LD-WALK`` (``LOAD``, ``REQUIRE``) and
``SOURCE-EVALUATE-CHECKED``.  The recorder learns where a walk begins from a
hook on those two words and that it ended cleanly from ``EVALUATE-FINISH``;
the lines between are the BIOS evaluator's lines that lie in the walk's
buffer.

A line reaches its file's record from one of two places, and a file's lines
may come from both in any order.  The runtime's reference evaluator calls
the taps below as it takes each token.  The native line walker evaluates the
lines it admits without the reference evaluator, and reports what it did
with each of their tokens when its walk returns
(docs/simulator-native-evaluator.md).  Both feed the same items, from which
``shared.token_form`` writes the file.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass, field

from shared.token_form import TokenFormError, TokenWriter, pack_token_file
from simulator.dictionary import Word


@dataclass(slots=True)
class RecordedUnit:
    """One source file as the interpreter took it."""

    kind: str
    depth: int
    address: int
    source: bytes
    items: list = field(default_factory=list)
    finished: bool = False
    unfit: str | None = None
    line: int = 0
    unlinks: int = 0
    _item_line: int = 0

    @property
    def digest(self) -> str:
        return hashlib.sha256(self.source).hexdigest()

    # -- the taps the runtime calls for one of this file's lines ----------

    def found_word(self, runtime, word: Word, token: bytes, state) -> bool:
        from simulator.runtime import DirectiveDefinition, DirectiveKind

        cursor = state.cursor
        start = cursor.column
        item = self._found(word.xt, token)
        latest = runtime.dictionary.latest
        ran = runtime._evaluate_found_word(word, state)
        if ran and runtime.dictionary.latest != latest:
            defined = runtime.dictionary.latest_word
            if defined is not None:
                self.defined(defined.xt)
        if ran and state.cursor is cursor:
            implementation = word.implementation
            self._took(
                item,
                cursor.data[start : cursor.column],
                isinstance(implementation, DirectiveDefinition)
                and implementation.kind is DirectiveKind.PAREN_COMMENT,
            )
        return ran

    def found_number(self, value: int) -> None:
        self._mark_line()
        self.items.append(("number", value))

    def word(self, xt: int, token: bytes, taken: bytes | None, comment: bool) -> None:
        """A word the native line walker found and disposed of: TAKEN is
        the input its cursor moved over while the word ran, or None when
        the word was compiled as a call."""

        item = self._found(xt, token)
        if taken is not None:
            self._took(item, taken, comment)

    def defined(self, xt: int) -> None:
        """The word before this completed a definition, which is LATEST and
        has the execution token XT.  If the file uses that word later, it
        takes its slot here."""

        self.items.append(("defined", xt))

    def _found(self, xt: int, token: bytes) -> list:
        self._mark_line()
        item = ["word", xt, token, None]
        self.items.append(item)
        return item

    @staticmethod
    def _took(item: list, text: bytes, comment: bool) -> None:
        # A comment's text does nothing when replayed; the word stays,
        # because running an immediate word resets what the BIOS compiler
        # may fuse with the code before it.
        if text and not comment:
            item[3] = text

    def skip_ended(self) -> None:
        self.items.append(("skipped",))

    def _mark_line(self) -> None:
        if self._item_line != self.line:
            self.items.append(("line", self.line))
            self._item_line = self.line

    # -- the file -----------------------------------------------------------

    def module_name(self) -> bytes:
        """The name the file PROVIDES, as KDOS's prescan would find it: the
        word after a PROVIDED that begins a line."""

        line_start = True
        for item in self.items:
            if item[0] == "line":
                line_start = True
                continue
            if item[0] == "word" and line_start and item[2].upper() == b"PROVIDED":
                text = item[3] or b""
                parts = text.split(b" ")
                names = [part for part in parts if part]
                return names[0] if names else b""
            line_start = False
        return b""

    def token_file(self, *, boot_id: int = 0, source_beside: bool = False) -> bytes:
        """This file's token form, built for the system whose BOOT-ID is
        BOOT_ID.  Raises TokenFormError when the file has none."""

        if not self.finished:
            raise TokenFormError("the file's load did not finish")
        if self.unfit is not None:
            raise TokenFormError(self.unfit)
        writer = TokenWriter()
        # One slot for each definition a name was bound to: a name defined
        # again later in the load is another word, at another address.
        used = {item[1] for item in self.items if item[0] == "word"}
        for item in self.items:
            kind = item[0]
            if kind == "word":
                _kind, xt, token, text = item
                writer.word(xt, token, text)
            elif kind == "defined":
                key = item[1]
                if key in used and not writer.has_slot(key):
                    writer.defined(key)
            elif kind == "number":
                writer.number(item[1])
            elif kind == "line":
                writer.line(item[1])
            else:
                writer.skipped()
        records = writer.finish()
        return pack_token_file(
            records,
            writer.slots,
            source=self.source,
            lines=self.source.count(b"\n") + (not self.source.endswith(b"\n")),
            module_name=self.module_name(),
            boot_id=boot_id,
            source_beside=source_beside,
        )


class TokenRecorder:
    """Records every source file a runtime loads while it is attached."""

    # Whether the native line walker evaluates the lines it admits and
    # reports them.  Without it the walker hands every line of a recorded
    # runtime to the reference evaluator: the recording against which the
    # walker's reports are checked, byte for byte.
    native_tap = True

    def __init__(self) -> None:
        self.units: list[RecordedUnit] = []
        self._open: list[RecordedUnit] = []
        self._runtime = None

    def attach(self, runtime) -> None:
        """Start recording RUNTIME's loads.  Call before KDOS loads; the
        walk hooks are bound once KDOS has defined its loader."""

        if runtime.token_recorder is not None:
            raise RuntimeError("the runtime already has a token recorder")
        runtime.token_recorder = self
        self._runtime = runtime

    def bind_walks(self) -> None:
        """Hook the two KDOS words whose walk is one file.

        A hook notes where a walk begins and changes nothing else: it
        stands before the word's overlay, when the word has one, and the
        overlay then decides and runs as it does with no recorder."""

        runtime = self._runtime
        evaluator = runtime._require_bios_evaluator()
        memory = runtime.memory

        def cell(name: bytes) -> int:
            word = runtime.find(name)
            if word is None:
                raise RuntimeError(f"KDOS has no {name.decode()}")
            return word.body_address

        buffer_cell = cell(b"LD-BUF")
        size_cell = cell(b"LD-SZ")

        def loader_walk(_context) -> None:
            self._begin(
                "loader",
                len(evaluator.frames),
                memory.read64(buffer_cell),
                memory.read64(size_cell),
            )

        def source_walk(context) -> None:
            if context.data.depth() >= 2:
                self._begin(
                    "source",
                    len(evaluator.frames),
                    context.data.peek(1),
                    context.data.peek(),
                )

        def hooked(hook, overlay):
            def applicable(context) -> bool:
                hook(context)
                return overlay is not None and overlay.applicable(context)

            return applicable

        def never(_context) -> None:
            raise AssertionError("a recording hook is not an accelerator")

        for name, hook in (
            (b"_LD-WALK", loader_walk),
            (b"SOURCE-EVALUATE-CHECKED", source_walk),
        ):
            word = runtime.find(name)
            if word is None:
                raise RuntimeError(f"KDOS has no {name.decode()}")
            overlay = runtime._colon_accelerators.get(word.xt)
            if overlay is not None and overlay.word is not word:
                overlay = None
            runtime.install_colon_accelerator(
                word,
                applicable=hooked(hook, overlay),
                callback=never if overlay is None else overlay.callback,
            )

    # -- events ---------------------------------------------------------------

    def _begin(self, kind: str, depth: int, address: int, length: int) -> None:
        # A walk at this depth or above means every deeper walk has ended.
        while self._open and self._open[-1].depth >= depth:
            self._open.pop()
        if length <= 0 or length >= 1 << 32:
            return
        try:
            source = self._runtime.memory.read_bytes(address, length)
        except Exception:
            return
        unit = RecordedUnit(
            kind,
            depth,
            address,
            bytes(source),
            unlinks=self._runtime._dictionary_unlinks,
        )
        self.units.append(unit)
        self._open.append(unit)

    def line(self, address: int, length: int, depth: int, number: int):
        """The file a BIOS evaluator line belongs to, or None."""

        while self._open and self._open[-1].depth > depth:
            self._open.pop()
        if not self._open:
            return None
        unit = self._open[-1]
        if unit.depth != depth or unit.finished:
            return None
        if not unit.address <= address <= unit.address + len(unit.source) - length:
            return None
        unit.line = number
        return unit

    def finished(self, depth: int, status: int) -> None:
        """EVALUATE-FINISH ran at DEPTH: the walk there ended."""

        while self._open and self._open[-1].depth > depth:
            self._open.pop()
        if not self._open or self._open[-1].depth != depth:
            return
        unit = self._open.pop()
        if status == 0:
            unit.finished = True
        if self._runtime._dictionary_unlinks != unit.unlinks:
            # TOKEN-REPLAY ends when entries leave the dictionary under it.
            unit.unfit = "the load took entries out of the dictionary"


__all__ = ["RecordedUnit", "TokenRecorder"]
