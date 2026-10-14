"""A host-side record of what screen changes cost, for diagnosis.

``ScreenChangeProfile`` writes two JSON-lines files into a directory. Each
record carries ``time.monotonic_ns()`` stamps, so it can be lined up with a
client's own trace of the same run:

- ``presents.jsonl``: each PRESENT_BEGIN the guest sends, with its CELL and
  retained modes and counts and its declared bytes;
- ``guest-words.jsonl``: for each tenth of a second, the guest steps charged
  to each word. At every host boundary the steps run since the last one are
  charged to the word the guest stopped in and to every colon word that
  called it, so a word's total includes everything it calls.

Recording changes nothing the guest or the terminal does.
"""

from __future__ import annotations

import json
import time
from collections import Counter
from pathlib import Path
from typing import Callable, Sequence

from rich_terminal.retained_wire import PresentBegin

PRESENTS_FILE = "presents.jsonl"
GUEST_WORDS_FILE = "guest-words.jsonl"
# The most frequent call chains kept for each interval, and how many words
# of each, innermost first; every word's own total is kept in full. A
# deeper profile asks for more, or for all (None).
CHAINS_KEPT = 40
CHAIN_DEPTH = 8


class ScreenChangeProfile:
    """Write the presents and guest-word records of one server run."""

    def __init__(
        self,
        directory: Path,
        *,
        interval_ns: int = 100_000_000,
        clock: Callable[[], int] = time.monotonic_ns,
        chains_kept: int | None = CHAINS_KEPT,
        chain_depth: int | None = CHAIN_DEPTH,
    ) -> None:
        if isinstance(interval_ns, bool) or not isinstance(interval_ns, int):
            raise TypeError("interval_ns must be an integer")
        if interval_ns <= 0:
            raise ValueError("interval_ns must be positive")
        if not callable(clock):
            raise TypeError("clock must be callable")
        for name, value in (("chains_kept", chains_kept), ("chain_depth", chain_depth)):
            if value is None:
                continue
            if isinstance(value, bool) or not isinstance(value, int):
                raise TypeError(f"{name} must be an integer or None")
            if value <= 0:
                raise ValueError(f"{name} must be positive")
        self._chains_kept = chains_kept
        self._chain_depth = chain_depth
        directory = Path(directory)
        directory.mkdir(parents=True, exist_ok=True)
        self._presents = (directory / PRESENTS_FILE).open("a", encoding="utf-8")
        self._words = (directory / GUEST_WORDS_FILE).open("a", encoding="utf-8")
        self._interval_ns = interval_ns
        self._clock = clock
        self._start_ns = clock()
        self._steps = 0
        self._samples = 0
        self._inclusive: Counter[str] = Counter()
        self._leaf: Counter[str] = Counter()
        self._chains: Counter[str] = Counter()

    def record_present(self, begin: PresentBegin) -> None:
        """Record one PRESENT_BEGIN as the guest declared it."""

        record = {
            "t_ns": self._clock(),
            "transaction": begin.transaction_id,
            "base_revision": begin.base_revision,
            "bytes": begin.declared_transaction_bytes,
            "cell_mode": begin.cell_mode.name,
            "cell_spans": begin.cell_span_count,
            "cells": begin.cell_count,
            "retained_mode": begin.retained_mode.name,
            "retained_operations": begin.retained_operation_count,
        }
        self._presents.write(json.dumps(record) + "\n")
        self._presents.flush()

    def sample_guest(self, chain: Sequence[str], steps: int) -> None:
        """Charge STEPS to CHAIN: the word the guest stopped in, then the
        colon words that called it, innermost first."""

        if chain and steps > 0:
            self._steps += steps
            self._samples += 1
            self._leaf[chain[0]] += steps
            for word in set(chain):
                self._inclusive[word] += steps
            self._chains[" < ".join(chain[:self._chain_depth])] += steps
        now = self._clock()
        if now - self._start_ns >= self._interval_ns:
            self._write_interval(now)

    def close(self) -> None:
        """Write the interval in progress and close both files."""

        if self._words.closed:
            return
        self._write_interval(self._clock())
        self._words.close()
        self._presents.close()

    def _write_interval(self, now: int) -> None:
        if self._steps:
            record = {
                "start_ns": self._start_ns,
                "t_ns": now,
                "steps": self._steps,
                "samples": self._samples,
                "inclusive": dict(self._inclusive),
                "leaf": dict(self._leaf),
                "chains": dict(self._chains.most_common(self._chains_kept)),
            }
            self._words.write(json.dumps(record) + "\n")
            self._words.flush()
        self._start_ns = now
        self._steps = 0
        self._samples = 0
        self._inclusive = Counter()
        self._leaf = Counter()
        self._chains = Counter()
