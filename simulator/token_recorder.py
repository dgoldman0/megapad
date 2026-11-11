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
buffer.  While a recorder is attached, every source line is evaluated by the
runtime's reference path.
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
    # Its place among the files the recording loaded, in the order their
    # loads began, and the places of the files that were loading then.
    index: int = 0
    enclosing: tuple[int, ...] = ()
    # True when the loader that evaluated the text told KDOS so, with
    # LOAD-STAMP-SOURCE: the text is then part of the stamp of the load.
    announced: bool = False
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
        self._mark_line()
        item = ["word", word, token, None]
        self.items.append(item)
        latest = runtime.dictionary.latest
        ran = runtime._evaluate_found_word(word, state)
        if ran and runtime.dictionary.latest != latest:
            # The record completed a definition.  If the file uses that
            # word later, it takes its slot here, from LATEST.
            defined = runtime.dictionary.latest_word
            if defined is not None:
                self.items.append(("defined", defined))
        if ran:
            implementation = word.implementation
            comment = (
                isinstance(implementation, DirectiveDefinition)
                and implementation.kind is DirectiveKind.PAREN_COMMENT
            )
            # A comment's text does nothing when replayed; the word stays,
            # because running an immediate word resets what the BIOS
            # compiler may fuse with the code before it.
            if not comment and state.cursor is cursor:
                text = cursor.data[start : cursor.column]
                if text:
                    item[3] = text
        return ran

    def found_number(self, value: int) -> None:
        self._mark_line()
        self.items.append(("number", value))

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

    @property
    def text_identity(self) -> int:
        """The identity KDOS takes into the stamp of a load for this text."""

        from shared.boot_identity import text_identity

        return text_identity(self.source)

    def token_file(self, *, stamp: int = 0, source_beside: bool = False) -> bytes:
        """This file's token form, to be used where the load's stamp stands
        at STAMP when the file's load begins.  Raises TokenFormError when
        the file has none."""

        if not self.finished:
            raise TokenFormError("the file's load did not finish")
        if self.unfit is not None:
            raise TokenFormError(self.unfit)
        writer = TokenWriter()
        # One slot for each definition a name was bound to: a name defined
        # again later in the load is another word, at another address.
        used = {item[1].xt for item in self.items if item[0] == "word"}
        for item in self.items:
            kind = item[0]
            if kind == "word":
                _kind, word, token, text = item
                writer.word(word.xt, token, text)
            elif kind == "defined":
                key = item[1].xt
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
            stamp=stamp,
            source_beside=source_beside,
        )


class TokenRecorder:
    """Records every source file a runtime loads while it is attached."""

    def __init__(self) -> None:
        self.units: list[RecordedUnit] = []
        self._open: list[RecordedUnit] = []
        self._announced: tuple[int, int] | None = None
        self._runtime = None

    def attach(self, runtime) -> None:
        """Start recording RUNTIME's loads.  Call before KDOS loads; the
        walk hooks are bound once KDOS has defined its loader."""

        if runtime.token_recorder is not None:
            raise RuntimeError("the runtime already has a token recorder")
        runtime.token_recorder = self
        self._runtime = runtime

    def bind_walks(self) -> None:
        """Hook the two KDOS words whose walk is one file."""

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

        def announcement(context) -> bool:
            # KDOS takes this text into the stamp of the load once the
            # stamp has started.  The walk that follows is its file.
            if context.data.depth() >= 2:
                self._announced = (context.data.peek(1), context.data.peek())
            return False

        def loader_walk(_context) -> bool:
            self._begin(
                "loader",
                len(evaluator.frames),
                memory.read64(buffer_cell),
                memory.read64(size_cell),
            )
            return False

        def source_walk(context) -> bool:
            if context.data.depth() >= 2:
                self._begin(
                    "source",
                    len(evaluator.frames),
                    context.data.peek(1),
                    context.data.peek(),
                )
            return False

        def never(_context) -> None:
            raise AssertionError("a recording hook is not an accelerator")

        for name, hook in (
            (b"LOAD-STAMP-SOURCE", announcement),
            (b"_LD-WALK", loader_walk),
            (b"SOURCE-EVALUATE-CHECKED", source_walk),
        ):
            word = runtime.find(name)
            if word is None:
                raise RuntimeError(f"KDOS has no {name.decode()}")
            runtime.install_colon_accelerator(word, applicable=hook, callback=never)

    def stamps(self, boot: int, texts: dict[int, bytes] | None = None) -> list[int]:
        """The stamp of the load where each recorded file's load began, on
        a machine whose BOOT-ID is BOOT that loads the same files in the
        same order.  TEXTS gives, by place, the text a file has there when
        it differs from the one recorded: a boot script that the image
        builder writes again.

        KDOS starts the stamp at the first token file it meets, with the
        source files that are loading then, and takes in every file whose
        load begins later.  This is that stamp for a machine on which no
        source file ends its load before the first token file begins.  A
        text that no loader announced with LOAD-STAMP-SOURCE is no file of
        the load, and the stamp passes it by.
        """

        from shared.boot_identity import stamp_start, stamp_take, text_identity

        stamp = stamp_start(boot)
        result = []
        for unit in self.units:
            result.append(stamp)
            if unit.announced:
                text = (texts or {}).get(unit.index, unit.source)
                stamp = stamp_take(stamp, text_identity(text))
        return result

    # -- events ---------------------------------------------------------------

    def _begin(self, kind: str, depth: int, address: int, length: int) -> None:
        announced = self._announced == (address, length)
        self._announced = None
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
            index=len(self.units),
            enclosing=tuple(open_unit.index for open_unit in self._open),
            announced=announced,
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
