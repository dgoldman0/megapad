"""How the BIOS ends a line from inside a word, on core 0 and on a worker.

A diagnostic that ends a line from inside a word, such as a compile-only
word met in interpretation state, and QUIT both pass through forth_quit, as
ABORT does: the return stack is reset, the QUIT-XT! callback runs and every
EVALUATE the line nested ends, while the data stack stays (finding 16 of
docs/multicore-simulator-plan.md). Until then the diagnostics branched
straight to the interpreter's loop, keeping each abandoned return frame and
EVALUATE level, so that sixteen of them inside EVALUATE used up its depth
for the rest of the session; and on a worker the same branch started a
second outer interpreter, which left core 0 waiting on the job for ever. A
worker's job now ends as its aborts do. tests/test_task_slots_differential.py
compares the endings inside a background slot with the simulator.
"""

from __future__ import annotations

from tests.test_bios_worker_signals import _Machine


DIAGNOSTIC = 'S" DO" EVALUATE'


def test_a_diagnostic_keeps_the_data_stack_and_frees_the_line():
    machine = _Machine(full_cores=1, chiplet=False)
    reports, _ = machine.run([
        f": DIAG ( -- ) {DIAGNOSTIC} ;",
        ': OK ( -- ) S" 2 3 + REPORT" EVALUATE ;',
        "RP@ REPORT",
    ])
    (prompt_rp,) = reports

    # The line ends after the diagnostic; the data stack stays.
    reports, text = machine.run(["1 2 DIAG 99 REPORT", "DEPTH REPORT REPORT REPORT"])
    assert "compile-only word" in text
    assert reports == [2, 2, 1]

    # Each diagnostic inside EVALUATE frees that EVALUATE's level and the
    # return frames under it, so far more of them than EVALUATE's sixteen
    # levels leave it working and the return stack where it was.
    reports, text = machine.run(["DIAG"] * 40 + ["OK", "RP@ REPORT"])
    assert text.count("compile-only word") == 40
    assert "depth limit" not in text
    assert reports == [5, prompt_rp]


def test_quit_keeps_the_data_stack_and_ends_the_evaluate_it_ran_in():
    machine = _Machine(full_cores=1, chiplet=False)
    reports, _ = machine.run([
        ': QUITTER ( -- ) S" 3 4 QUIT 5" EVALUATE ;',
        ': OK ( -- ) S" 2 3 + REPORT" EVALUATE ;',
        "RP@ REPORT",
    ])
    (prompt_rp,) = reports
    reports, _ = machine.run(["QUITTER 99 REPORT", "DEPTH REPORT REPORT REPORT"])
    assert reports == [2, 4, 3]
    reports, _ = machine.run(["QUITTER"] * 20 + ["DEPTH REPORT", "OK", "RP@ REPORT"])
    assert reports == [40, 5, prompt_rp]


def test_a_diagnostic_on_a_worker_ends_its_job():
    machine = _Machine(full_cores=2, chiplet=False)
    reports, text = machine.run([
        "CREATE MARK 1 ALLOT  0 MARK C!",
        # DO is immediate; executed outside a definition it is compile-only.
        ": DIAG-JOB ( -- ) 7 MARK C! ['] DO EXECUTE 99 MARK C! ;",
        ": GOOD ( -- ) 66 MARK C! ;",
        "' DIAG-JOB 1 WAKE-CORE 1 SETTLE",
        "1 CORE-STATUS REPORT MARK C@ REPORT DEPTH REPORT",
    ])
    assert "compile-only word" in text
    assert reports == [0, 7, 0]
    # The worker takes the next job.
    reports, _ = machine.run([
        "' GOOD 1 WAKE-CORE 1 SETTLE",
        "MARK C@ REPORT 1 CORE-STATUS REPORT",
    ])
    assert reports == [66, 0]
