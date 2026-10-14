"""The host-side screen change profile: presents and guest words."""

from __future__ import annotations

import json

import pytest

from rich_terminal.retained_wire import CellMode, PresentBegin, PresentRetainedMode
from shared.screen_profile import GUEST_WORDS_FILE, PRESENTS_FILE, ScreenChangeProfile
from simulator.runtime import MegaForthRuntime
from simulator.session import SimulatorMachineSession


class _Clock:
    def __init__(self) -> None:
        self.now = 1_000

    def __call__(self) -> int:
        return self.now


def _lines(path):
    return [json.loads(line) for line in path.read_text().splitlines()]


def test_a_present_is_written_as_the_guest_declared_it(tmp_path) -> None:
    clock = _Clock()
    profile = ScreenChangeProfile(tmp_path, clock=clock)
    clock.now = 5_000
    profile.record_present(PresentBegin(
        5, 4, 0, 160, 80, 24, 0, 0, 0,
        CellMode.NONE, PresentRetainedMode.REPLACE_START,
    ))
    profile.close()
    assert _lines(tmp_path / PRESENTS_FILE) == [{
        "t_ns": 5_000, "transaction": 5, "base_revision": 4, "bytes": 160,
        "cell_mode": "NONE", "cell_spans": 0, "cells": 0,
        "retained_mode": "REPLACE_START", "retained_operations": 0,
    }]


def test_guest_steps_are_charged_to_each_word_once_an_interval(tmp_path) -> None:
    clock = _Clock()
    profile = ScreenChangeProfile(tmp_path, interval_ns=100, clock=clock)
    clock.now = 1_010
    profile.sample_guest(["LEAF", "MID", "ROOT"], 10)
    # A word that calls itself is charged once.
    profile.sample_guest(["MID", "ROOT", "MID"], 5)
    # Steps with nowhere to go, and a boundary with no steps, charge nothing.
    profile.sample_guest([], 7)
    profile.sample_guest(["LEAF", "ROOT"], 0)
    assert (tmp_path / GUEST_WORDS_FILE).read_text() == ""
    clock.now = 1_100
    profile.sample_guest(["LEAF", "ROOT"], 3)
    # An interval with no steps writes nothing.
    clock.now = 1_250
    profile.sample_guest([], 4)
    clock.now = 1_260
    profile.sample_guest(["ROOT"], 2)
    profile.close()
    first, last = _lines(tmp_path / GUEST_WORDS_FILE)
    assert first == {
        "start_ns": 1_000, "t_ns": 1_100, "steps": 18, "samples": 3,
        "inclusive": {"LEAF": 13, "MID": 15, "ROOT": 18},
        "leaf": {"LEAF": 13, "MID": 5},
        "chains": {"LEAF < MID < ROOT": 10, "MID < ROOT < MID": 5, "LEAF < ROOT": 3},
    }
    assert (last["start_ns"], last["t_ns"], last["steps"], last["inclusive"]) == (
        1_250, 1_260, 2, {"ROOT": 2})


def test_a_deeper_profile_keeps_more_and_longer_chains(tmp_path) -> None:
    clock = _Clock()
    profile = ScreenChangeProfile(tmp_path, interval_ns=100, clock=clock,
                                  chains_kept=None, chain_depth=None)
    chain = [f"W{i}" for i in range(12)]
    for index in range(50):
        profile.sample_guest([f"LEAF{index}", *chain], 1)
    profile.close()
    record, = _lines(tmp_path / GUEST_WORDS_FILE)
    assert len(record["chains"]) == 50
    assert all(len(chain.split(" < ")) == 13 for chain in record["chains"])
    # The defaults keep forty chains of eight words.
    short = ScreenChangeProfile(tmp_path / "short", interval_ns=100, clock=clock)
    for index in range(50):
        short.sample_guest([f"LEAF{index}", *chain], 1)
    short.close()
    record, = _lines(tmp_path / "short" / GUEST_WORDS_FILE)
    assert len(record["chains"]) == 40
    assert all(len(chain.split(" < ")) == 8 for chain in record["chains"])


@pytest.mark.parametrize("limit", (0, -1, True, 1.5))
def test_chain_limits_must_be_positive_integers(tmp_path, limit) -> None:
    with pytest.raises((TypeError, ValueError)):
        ScreenChangeProfile(tmp_path, chains_kept=limit)
    with pytest.raises((TypeError, ValueError)):
        ScreenChangeProfile(tmp_path, chain_depth=limit)


@pytest.mark.parametrize(("interval", "error"), ((0, ValueError), (True, TypeError)))
def test_an_interval_must_be_a_positive_integer(tmp_path, interval, error) -> None:
    with pytest.raises(error):
        ScreenChangeProfile(tmp_path, interval_ns=interval)


def test_a_simulator_session_charges_its_batches_to_the_words_running(tmp_path) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": INNER BEGIN 1 DROP AGAIN ; : OUTER INNER ;")
    profile = ScreenChangeProfile(tmp_path, interval_ns=1)
    with SimulatorMachineSession(
        runtime, "OUTER", cols=8, rows=2,
        semantic_quantum_steps=100, screen_profile=profile,
    ) as session:
        session.boot()
        for _ in range(3):
            session.run_boundary()
    records = _lines(tmp_path / GUEST_WORDS_FILE)
    steps = sum(record["steps"] for record in records)
    assert steps >= 300
    assert sum(record["inclusive"]["OUTER"] for record in records) == steps
    assert sum(record["leaf"].get("INNER", 0) for record in records) == steps
