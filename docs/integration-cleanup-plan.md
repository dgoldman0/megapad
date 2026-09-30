# Integration cleanup plan

Accepted 2026-09-30. Branch `integration/unified-runtime-rich-desk`, paired
with Akashic `feature/rich-desk-producers`, whose own steps are in Akashic
`docs/rich-terminal/RICH-DESK-CLEANUP-PLAN.md`. Neither branch merges into
main or is pushed until the final step passes; both repositories are then
pushed together.

## Why

The unified runtime, hybrid mode and rich Desk terminal work arrived as 134
commits made in one day in a separate environment. A read-only review found
the direction sound and several parts ready: the exact emulator value kernels,
the unified launcher and shared session, and the checked rich-terminal object
families. It also found:

- A hybrid call between Forth and machine code costs several milliseconds,
  thousands of times the work it performs. Much of that appears to be
  whole-program re-verification on every call. Hybrid mode was never run on a
  real Desk workload with machine routines.
- Same-day compatibility layers: five manifest schemas, four native routine
  transports, capability probing with fallback to older runners, a legacy
  embedder path and deprecated root server scripts.
- Fixed limits with no interface reason, such as a 65,536-word dictionary
  ceiling for service admission and an exact nested depth of 8 that both
  sides must agree on or nesting silently turns off.
- Qualification only with a dummy display and in-process dispatch. Three
  Unix-socket tests were skipped and the physical Desktop journey never ran.
- About 2 MB of raw benchmark JSON containing sandbox paths, and reports of a
  dropped AES transfer experiment.

Hybrid and unified execution are core goals. The aim is to make them work
well, not to remove them.

## Rules

AGENTS.md governs this work. In particular: keep no legacy or compatibility
layer unless it is a temporary bridge; bound capacity by caller-provided
storage or a real interface requirement; speed up only by removing provably
redundant work, stating the proof for each removal; the real device comes
first; the physical Desktop journey is the regression gate; heavyweight runs
go one at a time; commit each coherent slice once it is green.

## Steps

1. **Baseline.** Build both extensions and run the recorded gates on this
   machine: unified application, simulator under the Python and native
   executors, rich-terminal dual, and the three socket tests skipped before.
   Record any failure before changing code. Done on Python 3.13: 2,468
   application, 2 × 2,569 simulator, 8 dual and 1,811 further rich-terminal
   tests passed. The one failure, a socket test stepping an idle guest, had
   failed on main since the idle work and is corrected.
2. **Measure a hybrid call.** Attribute the time of a machine-to-Forth
   callback and a Forth-to-machine routine call under both executors. Done
   with the native executor, 64 iterations of four FP operations: direct
   semantic FP took 0.3 ms and machine code without callbacks 0.4 ms. A
   scalar-service callback took about 6.2 ms and ran about 127,000 Python
   calls, nearly all re-verifying class, module and function seals and
   scanning namespaces. An integer leaf callback took about 134 µs, mostly
   rebuilding and revalidating frozen records and the registration on every
   crossing.
3. **One format, one transport.** Replace manifest schemas 1–5 with one
   schema and the routine transports V1, V2, V3 and task with one. Remove
   capability probing, fallback to older runners, legacy facades and the
   legacy embedder subclass path.
4. **Remove whole-program re-checks from the call path.** Keep checks that
   guest execution can trigger: a forgotten or reused word, stale code,
   budgets and receipts. Remove per-call scans of the dictionary, of Python
   modules, classes and functions, and interpreter-frame inspection, since
   nothing the guest does can change those. Each removal records why it
   guards nothing the guest can reach.
5. **Native crossings.** When both sides are native, keep a call and its
   return in native code instead of passing through a Python dispatcher.
6. **Caller-bounded limits.** Replace fixed ceilings (dictionary words,
   namespace keys, nesting depth, instruction and callback ceilings, edge and
   publication counts) with limits the caller supplies or real structural
   bounds. Keep only limits an interface requires, such as the eight
   register-passed arguments.
7. **Real hybrid workload.** Run Desk in hybrid mode with real machine
   routines and compare it with simulator mode.
8. **Launcher migration.** Once Akashic's tools start MegaPad through
   `megapad.py` or the packaged servers, delete the root `session_server.py`
   and `simulator_server.py` forwarders.
9. **Capacity negotiation.** Designed with the owner before implementation:
   a request-and-answer step through which a producer asks the terminal for
   more retained space and receives an approval or a denial, replacing silent
   fallback.
10. **Docs.** Remove the raw benchmark JSON and sandbox paths, and remove the
    dropped AES experiment reports; their reasons stay in commit history.
    Update commit IDs cited in docs to the re-authored IDs. Fold completed
    plans into current documentation.
11. **Small fixes.** Move test-only failure hooks out of production builds,
    restore the lost cancellation diagnostic in
    `simulator/rich_terminal_host.py`, release the GIL during long hybrid
    machine runs, and count `perf_cycles` for F9, FA and FB as the Python
    reference does.
12. **Final gates.** Rerun step 1's gates, run the physical Desktop journey
    once through Akashic's `physical_desktop_acceptance.py`, and run Akashic's
    numeric suites against this branch. Then merge into main and push both
    repositories together.

## Provenance

The imported commits were re-authored as Daniel with a
`Co-Authored-By: Codex <codex@openai.com>` trailer. Trees, parents and dates
are unchanged, and commit IDs cited in messages were remapped. The original
history is kept at `refs/imports/codex-2026-09-30/main`, and the old-to-new
commit map is `.git/imports/codex-2026-09-30/commit-map.txt` in the main
checkout. Docs still cite the original IDs until step 10.
