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
   legacy embedder subclass path. The new runner keeps test-only failure
   hooks out of the production build and releases the GIL during long
   machine runs. A machine routine calls a Forth word the way the chip does,
   as an ordinary call on the caller's own data and return stacks. The
   manifest names the word at each call site and how many cells go in and
   come out, and the stack depth is checked when the word returns. Any word
   may be named, but nothing is chosen at run time. The private callback
   stacks and the closed-callback proofs go: they are a wall the chip does
   not have, they stop a callback from ever becoming a plain native call,
   and budgets and the depth check already bound a callback. Done: one
   manifest format, one native routine runner whose CALL.L and RET.L use the
   Forth return stack, and callbacks on the caller's stacks. The old Python
   layers, about 37,000 lines with their tests, and the four native
   transports are deleted. The runner has no test hooks and releases the
   GIL after its first 4,096 instructions. The design is in
   `hybrid-runtime.md`.
4. **Remove whole-program re-checks from the call path.** Keep checks that
   guest execution can trigger: a forgotten or reused word, stale code,
   budgets and receipts. Remove per-call scans of the dictionary, of Python
   modules, classes and functions, and interpreter-frame inspection, since
   nothing the guest does can change those. Each removal records why it
   guards nothing the guest can reach. Done: a call checks only that the
   routine's body allocation is still live. Code seals, parked-state
   comparisons, dictionary and namespace scans and frame inspection are
   gone. Forth writes over code now behave as on the chip, through the
   instruction cache.
5. **Native crossings.** When both sides are native, keep a call and its
   return in native code instead of passing through a Python dispatcher.
   Done for routines without callback sites: the native executor calls them
   directly, 0.45 us per call against 19 us before. Callbacks still pass
   through the Python dispatcher, about 40 us per round trip with the native
   executor.
6. **Caller-bounded limits.** Replace fixed ceilings (dictionary words,
   namespace keys, nesting depth, instruction and callback ceilings, edge and
   publication counts) with limits the caller supplies or real structural
   bounds. Keep only limits an interface requires, such as the eight
   register-passed arguments. Done: those ceilings left with the old code.
   What remains is the eight register cells and whole I-cache lines of code.
7. **Real hybrid workload.** Run Desk in hybrid mode with real machine
   routines and compare it with simulator mode. Finding: Desk has no machine
   code to call. Akashic and KDOS are entirely Forth; on the chip the only
   machine code is the BIOS and compiler output, which is the separate
   native dictionary design.
8. **Launcher migration.** Once Akashic's tools start MegaPad through
   `megapad.py` or the packaged servers, delete the root `session_server.py`
   and `simulator_server.py` forwarders. Done.
9. **Capacity negotiation.** A request-and-answer step through which a
   producer asks the terminal for more retained space and receives an
   approval or a denial, replacing silent fallback. On a denial that part
   stays CELL, and a record says which part fell back, how much space it
   asked for and how much it had; nothing is drawn on screen. Done:
   `OWNER_RESIZE` grows a live owner's reservation within the budget the
   terminal shares between its owners, or answers `NO_CAPACITY` and leaves
   it unchanged. The terminal counts its refusals and keeps the last one
   (request, owner, quotas asked and held) in its session status. Akashic's
   producer opens an owner with what its first frame needs and asks to grow
   it as frames grow; its own record is described in Akashic's cleanup plan,
   step 8.
10. **Docs.** Remove the raw benchmark JSON and sandbox paths, and remove the
    dropped AES experiment reports; their reasons stay in commit history.
    Update commit IDs cited in docs to the re-authored IDs. Fold completed
    plans into current documentation.
11. **Small fixes.** Move test-only failure hooks out of production builds,
    restore the lost cancellation diagnostic in
    `simulator/rich_terminal_host.py`, release the GIL during long hybrid
    machine runs, and count `perf_cycles` for F9, FA and FB as the Python
    reference does. Done for the diagnostic and `perf_cycles`. The failure
    hooks and the GIL release live in the routine runners that step 3
    replaces, so they are done there.
12. **Tests that already failed.** After everything else, fix the tests that
    also fail on main, together with Akashic's. Done here: the phase-0 oracle
    test in `test_native_cycle_execution.py` pinned schema version 20 while
    six later benchmark changes had moved the oracle to 26; it now pins 26.
13. **Final gates.** Rerun step 1's gates, run the physical Desktop journey
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
