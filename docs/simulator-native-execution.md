# Native semantic execution

Native acceleration was developed on `simulator-runtime`, starting from
rich-terminal vertical `c10058b`. It is independent of the MP64 emulator's
C++/DBT extension. The Python semantic dispatcher remains the correctness
reference and owns operations outside each admitted native interval.

**Current checkpoint — September 16, 2026:** Native dispatch/memory and host
boundary changes are physically qualified. With identical Akashic `de6a6aa`
production sources, isolated typing feedback fell from 2.435 s to 1.413 s
(42%); all 19 individually scheduled characters at 5 characters/second were
retained. This remains far above the 100 ms working target. The complete
ordinary X11 Desk/menu journey at Akashic `a730baa` and MegaPad `efed68a` also
passed: 18 milestones, 21 inputs, 27 post-flip ACKs, both CELL fallback gates,
154.121 s total, and 437.512 MiB aggregate peak RSS. See paired Akashic
`docs/rich-terminal/TYPING-PERFORMANCE-20260916.md` and
`local_testing/evidence/typing-20260916.md` for matched timings and bindings.

**September 13 integration checkpoint:** The `rich-interaction-fixes` work
is merged into both local mains. Hosted identity reads and cell increments
remain native, while the shared viewer and paired Akashic projection correct
menu layering and reduce repeated rich publication work. Native remains opt-in.
The September 8 measurements and failure description below are historical
provenance; the complete current Desktop journey passes, including Sound Lab.

Both C++ extensions were force-rebuilt sequentially in MegaPad main. Native
parity, popup compositor/input and emulator/simulator shared-source selectors
passed all 127 checks; paired Akashic passed 395 focused checks. A fresh ordinary
source-mode physical Desktop run from Akashic `c0cb351` and MegaPad `598ca0c`
passed on X11, including Pad and Daybook interactions and extra View/Go popup
checks: 33 physical ACKs, 21 inputs and 18 captured milestones in 224.407s.
That elapsed time includes the extended journey and is not a matched-workload
comparison with earlier runs. Peak physical-run RSS was 438.512 MiB; the
3.5 GiB cap and 900-second watchdog were unchanged.

Both old worktrees were removed after their merged ancestry, clean state,
preserved artifacts and lack of active process references were rechecked.
The full integration ledger is paired Akashic
`local_testing/evidence/rich-main-integration-20260913.md`; fresh physical
evidence is under its `local_testing/out/rich-main-integration-20260913/`.
Preservation and build/test records remain in the workspace archive
`worktree-retirement-archives/2026-09-13-rich-interaction/`.

## Typing execution follow-up — September 16, 2026

A profiled ordinary Pad character performed 49.6 million native semantic
steps and spent 1.76 s inside native execution, plus 0.15 s in settlement.
That observer-enabled run is attribution evidence, not unprofiled latency.
The simulator is a material bottleneck alongside repeated guest validation.

The executor now reuses exact qualified page spans within each bounded native
interval, avoids zeroing scratch that is initialized before use, uses
unaligned-safe full-cell loads/stores on little-endian hosts, and retains the
current plan pointer until the execution token changes. Memory remains the
same shared sparse backing. Step counts, timer advancement, fallback faults,
continuation cookies, and the 8,192-step native interval are unchanged.

The Python/native parity selector passes 96 cases, including sub-cell pages,
unaligned access, partial final regions, missing-page writes, host page
replacement between intervals, and observable stack scratch results.
`bench_native_dispatch.py` runs four bounded kernels sequentially and records
all trial times, semantic counts, results, and the native binary hash. Initial
median speedups are 3.05x arithmetic, 2.68x field reads/calls, 2.01x counted
loop/calls, and 2.82x scattered reads, with identical steps and results.
These kernels do not establish physical typing latency.

The associated input correction distinguishes temporary model/result waits
from invalid input. The viewer retains bounded keyboard intentions through
ordinary frame transitions and binds them only when a complete current frame
has been physically ACKed. Frame-specific control activations are discarded;
reset, lease loss, attachment/presentation/geometry change, or revision
regression clears the keyboard queue. The viewer/driver/core selector passes
106 cases. A physical ordinary Desktop rerun before this executor change
showed all 19 individually scheduled characters at 5 characters/second; its
single-character latency was still 2.435 s. The paired Akashic typing ledger
records physical results as they become available.

The following host-boundary slice retains all semantic owner boundaries and
full suspension-mutation checks. Return snapshots decode their already-read
cells in one loop, including stale continuation removal, instead of a Python
call and generator entry per stack slot. Explicit OS handoffs now follow
Python's configured thread-switch interval rather than every tiny native
batch. The owner still releases its lock and checks input/progress at every
original boundary; guest work counts and virtual timers are unchanged. Idle
waits continue using the existing condition-variable path. Stack, native,
shared-session and clock selectors pass 154 checks in 1.68 s.

## Interpreter superinstructions — September 16, 2026

Plan installation recognizes literal/constant plus simple arithmetic,
bitwise, shift, or comparison operations, and `DUP` followed by a conditional
branch. These pairs share one dispatch and operand load. Every original IR
index remains present, including entry directly into the second operation.
A pair executes only if its entire semantic allowance and transient stack
storage are available; otherwise the original first instruction executes.
Popped literal/duplicate bytes remain in shared memory. No step, clock,
scheduler, callback, or fault boundary is removed.

The native equivalence selector passes 185 cases, including every budget
boundary of a branching literal/constant example, signed/shift edges,
temporary stack overflow, underflow, missing-page allocation, fragmented
stack cells, and branches entering a pair's original second instruction.
Kernel timing is recorded separately from physical Desktop qualification.

Stack operations also reuse a qualified page for the whole input/output
span, loading cells directly into local operands and writing results directly
to the same shared bytes. This removes repeated address resolution per cell.
Cross-page, sparse, and fragmented spans retain scalar handling; cached
pointers never survive a native interval. The expanded parity selector passes
194 cases, including multi-cell operations at different page alignments.

Prepared plans record Python-owned instruction positions. Reaching one now
continues directly in Python instead of making a zero-progress C++ call with
the same stack metadata. This map shares dictionary-generation invalidation
with the native plan, and cached plans no longer allocate a preparation work
list on each entry. Profiling distinguishes these `skipped:` boundaries from
actual `empty:` native calls. The focused selector passes 195 cases.

Identity-bound `EXECUTE` now enters an already prepared colon target in C++,
using the same continuation cookie and original return IP as a static call.
Cold targets, primitives, source accelerators, invalid tokens and transient
failures retain reference dispatch. Dynamic calls remain two semantic ticks;
dictionary invalidation also discards their available target plans. This is
an interpreter call fast path, not machine-code generation or a source cache.
`bench_native_dispatch.py` includes a bounded dynamic-call kernel in addition
to the original four workloads.

The sequential native execution/loop/memory/clock, stack snapshot, session
clock and shared-session selector passes 462 cases in 4.38 s. Dynamic-call
checks cover every budget boundary, multiple targets in one native interval,
cold/primitive/invalid targets, return-stack overflow, word shadowing and
dictionary rollback with XT reuse.

## Build and select

```
make simulator-accel
MEGAFORTH_EXECUTOR=native make test-simulator SIMULATOR_TEST_PATH=tests/simulator/test_native_execution.py
```

`MegaForthRuntime(execution_backend="python" | "native" | "auto")` selects the
executor explicitly. When omitted, `MEGAFORTH_EXECUTOR` supplies the selection;
the current qualification default is `python`. `native` requires the extension
and fails clearly when it is missing. `auto` uses the extension when installed
and otherwise uses Python. Runtime status exposes the actual executor and
native work counters; these are separate from semantic diagnostic counters.

No Forth source cache, terminal-specific replacement, guest timing weakening,
new applet path, or enlarged watchdog is part of this work.

Set `MEGAFORTH_NATIVE_PROFILE=1` for diagnostic exit counts and elapsed native
call/settlement nanoseconds in `native_execution_stats["profile"]`. Counts name
the original unexecuted IR boundary (and target word for calls), including
zero-progress attempts. An exit at a two-step operation with only one remaining
step may reflect the allowance rather than unsupported coverage. Profiling is
off by default; profiled wall times are diagnostic, not throughput evidence.
Snapshot counters at identical boundaries: the CELL helper's native counters
cover begin/cursor/commit as well as the narrower timed row-write interval.

## Admitted native intervals

The native compiler preserves the existing semantic IR indices and execution
tokens. Native code performs arithmetic, branches, ordinary scalar memory,
data-stack operations, and nested colon calls using the same sparse guest
pages. There is no copied address space or unobservable shadow data stack.
Native calls write the same return-continuation cookies as Python, and export
changed metadata even for popped slots. Dictionary publication/rollback,
DOES> installation, and source-accelerator installation invalidate plans.
Only identity-bound original BIOS primitives receive native implementations.

Unsupported operations and unsafe or faulting spans return at their original
IR boundary before any step or effect of that operation. Python then owns its
normal fault ordering and partial effects. Native now includes `>R`, `R>`, `R@`
and ordinary continuations from preceding intervals. It checks continuation
type and raw cookie, preserves type deletion even for equal-cookie user pushes,
and leaves root/fault/stale continuation handling in Python. Counted
`DO`/`?DO`/`LOOP`/`+LOOP`/`UNLOOP` operations and identity-bound `I`/`J`
now use the same native ordered return stack. `+LOOP` preserves the BIOS
modular equality rule, including zero increments and limit crossings.
Identity-bound `TRUE`, `FALSE`, `CELLS`, `UM*`, `COREID`, `TASK-ID`, and `CELL+`
also stay native.
Pair return-stack operations, unprepared/non-colon dynamic execution,
stack-pointer operations, and device/service access remain in Python. Successful
native prefixes settle their exact watchdog, diagnostic, and timer counts
before the next Python operation or host boundary. Native execution does not
invent IDL, reset a budget, or admit external events between guest boundaries.

The extension retains the GIL only for bounded execution intervals. Custom
clock callbacks, custom semantic service types, or custom stack/memory types
remain on the reference path so their observable behavior is preserved.
See `simulator/accel/API.md` for the internal boundary.

Host suspension snapshots decode the complete active stack span in bulk from
the already-qualified sparse backing. Return snapshots still validate every
active continuation cookie and remove stale metadata; resumed execution still
compares every data and return entry. Stack bounds, host quanta, and raw-memory
mutation detection are unchanged. This improvement also serves the Python
executor; custom stack and memory types retain scalar snapshot dispatch.

Dictionary publication maintains the highest live header/code-slot end. A
new definition starting at or above that bound cannot overlap an existing
header, so forward source loading avoids a full dictionary scan per word.
Lower-address publication retains the original complete overlap check,
including the new initial body, and rollback recomputes the live bound.
This changes source-preparation cost without caching any compiled Forth.

## Profile-guided follow-up — September 12, 2026

The full ordinary source-mode Desktop journey passes after the loop, snapshot,
scalar-memory and dictionary-publication improvements. Both source checkouts
were clean at launch (MegaPad `08b37d5`, Akashic `fb4e5d9`). The same canonical
journey completed 20 post-flip offer ACKs, 14 authorized inputs, 11 visible
milestones and both CELL fallback gates. Outer time fell from 511.819576s to
185.862142s. First complete Desk ACK fell from 204.060423s to 71.895276s;
Desk-to-Pad editing from 54.811594s to 18.246871s; and Desk-to-Daybook task
insertion from 131.867425s to 43.034026s. These are unprofiled same-profile
observations on a shared host, not balanced pinned-host benchmark trials.

The profile guided each slice: frequent loop/primitive exits, repeated scalar
snapshot reads, redundant native page lookup per byte, then 616 million
header-end lookups while publishing 35,106 words. After maintaining the live
header bound, the cold preparation profile recorded 84,641 header-end lookups
and the same definitions and 11,035,530 preparation steps. Profiled timings
are diagnostic and excluded from the physical speed comparison.

Focused sequential checks passed, including a separate ASan/UBSan native
extension with 161 passing memory/execution/loop cases. Final physical sampled
aggregate RSS peaked at about 402 MiB, with at least 10.36 GiB system memory
available and peak one-minute load 8.64 on 16 CPUs. Native remains opt-in and
no feature work was started. The paired Akashic evidence ledger is
`local_testing/evidence/simulator-native-followup-20260912.md`, with raw profile,
benchmark, frame, revision, sanitizer and resource bindings.

## Return-stack slice qualification — September 12, 2026

The existing bounded CELL helper measured 1.696597s before versus 0.262753s
after, a 6.457x improvement over the previous native executor. The after
Python/native pair measured 4.545609s / 0.262753s (17.300x), preserving the same
2,432,950 row-feed steps, 560 cells, committed frames and timer state. Successful
native entries across the wider exercise fell from 176,413 to 6,185, with
average intervals increasing from 13.0 to 400.2 steps. The optional profile
confirms return-stack exits are gone; TRUE, UM*, loop control and CRC-FEED remain
frequent boundaries. Their counts are not wall-time attribution.

Sequential focused runtime, differential, stack, clock, source-overlay,
CATCH/THROW, session, dual-backend terminal and audio checks passed. The
physical run at MegaPad `4ac8c60` / Akashic `9c2977b` then passed all eleven
milestones, twenty post-flip offer ACKs and fourteen revision-bound inputs.
Initial/final CELL fallback passed. The first complete Desk ACK was 204.060423s,
Daybook task-added was 335.927848s, and the final Sound Lab ACK was 497.698337s.
The runner finished at 509.028191s; outer elapsed time was 511.819576s.

The new Desk-to-Daybook interval is 131.867425s versus the earlier native
213.876463s and emulator 91.962938s. Interactive execution still has room to
improve. These historical physical runs are operational comparisons on shared
hosts and different revisions, not controlled matched-head benchmarks.
The full run used the unchanged 900s watchdog, ordinary checked cold source,
and monitored resources: maximum sampled aggregate RSS 370,696 KiB and minimum
system available memory about 9.8 GiB. No new Forth cache or timing shortcut was
introduced. The complete artifact/validation ledger is paired Akashic
`local_testing/evidence/simulator-improvements-20260912.md`.

## Initial qualification and measurement — September 8, 2026

Sequential checks at the first native slice:

- 115 clock-batching cases passed in 0.19 seconds against repeated single ticks.
- 103 existing runtime, execution-quantum, real CATCH/THROW, memory, and source
  acceleration cases passed with native execution selected, in 10.24 seconds.
- 28 session/server plus native differential cases passed in 1.53 seconds.
  The 20 differential cases compare exact steps, stacks, pointers, retained
  stack bytes, memory, errors, dictionary state, and fallback side effects.

The existing Akashic production-source CELL fixture was run sequentially on
Python and native executors: 280x2 actual cells inside caller bounds 280x84,
with its unchanged 3,000,000-step per-row watchdog. Akashic source is `78608e7`
(the later `2a90108` commit adds evidence only). Both cases committed the same
560 cells, revision 2, 4,848 decoded frame bytes, and 2,432,950 feed steps.

| Interval | Python | Initial native | Observed ratio |
| --- | ---: | ---: | ---: |
| Unified CELL feed | 4.555204 s | 1.789607 s | 2.545x |
| Initial direct PT snapshot rows | 0.448448 s | 0.043848 s | 10.227x |
| Complete small fixture | 6.077742 s | 2.452930 s | 2.478x |

These are one sequential kernel comparison on a shared host, not a full
Desktop speed claim. Raw output is in paired Akashic
`local_testing/out/close-20260908-cell-feed/native-first.jsonl`. The first
native implementation still crosses into Python frequently during the range
predicates used by unified publication. Return-stack operations are a possible
future coverage improvement; no further optimization is part of this slice.

The reproducible `bench_semantic_cell_feed.py` helper binds repository/source
and extension hashes, enforces unchanged fixture bounds, and checks Python/native
frame, timer, memory-result and semantic-work equivalence. Run it from this
checkout with the paired Akashic checkout selected explicitly:

```
python bench_semantic_cell_feed.py --akashic-root ../akashic --output /tmp/semantic-cell-feed.json
```

Its sequential verification pair measured 4.112512 s versus 1.845714 s for the
unified feed (2.228x), and 0.380753 s versus 0.042558 s for initial PT rows
(8.947x). Both pairs are recorded; they establish useful kernel improvement,
not a full Desktop throughput result.

The prior Python simulator reached the first complete rich Desk ACK at
897.181846 seconds and timed out at 900 seconds. The accelerated emulator's
corresponding ACK was 486.098092 seconds and its full journey passed. Detailed
source bindings and timing boundaries are in paired Akashic
`local_testing/evidence/rich-desktop-speed-decision-20260908.md`.

## Physical Desktop result and integration limitations

The ordinary source-mode `desktop-apt1` run used Akashic `2a90108` (same Forth
as `78608e7`) and MegaPad `3ea7bdc`, clean at launch, with the native executor,
real X11 viewer and the existing 900-second deadline. No cached source or
expanded watchdog was used.

| Exact physical ACK milestone | Python simulator | Native simulator | Earlier emulator |
| --- | ---: | ---: | ---: |
| First complete rich Desk | 897.181846 s | 383.162947 s | 486.098092 s |
| Pad edited | Not reached | 469.611251 s | 524.156537 s |
| Daybook task added | Not reached | 597.039410 s | 578.061030 s |

Native reached the first Desk 2.342x sooner than Python. It reached actual Pad
and Daybook interactions, with revision-authorized input after physical ACKs.
However, Desk-to-Daybook-task took 213.876463 s versus the emulator's 91.962938 s;
the emulator caught up during subsequent interactions. The older emulator
baseline predates the small Akashic CELL validation correction, so this is a
practical comparison, not a matched-head backend benchmark.

The simulator completed 10 milestones, 18 acknowledged offers and 14 inputs,
then paused with `MMIOAccessError: MMIO service rejected read preflight` during
the later ordinary Sound Lab stage. Its graph initialization probes AudioOut,
which the simulator platform does not expose. A focused AudioOut status read
at `0xFFFFFF0000000C01` fails identically in Python and native configurations.
The exact live failing address was not captured; this is a source-supported
cause candidate, not a replay of the live fault. Diagnostics were saved and
the failed runner interrupted. Full simulator acceptance did not pass.

The user authorized merging this measured state with its limitations recorded.
Native remains optional and Python remains the default. The emulator retains
the successful full-journey baseline; native is useful for startup and focused
fixtures but is not established as a faster complete Desktop path. AudioOut
coverage, failure propagation to the acceptance harness, and further native
coverage remain follow-up work, not requirements silently claimed complete.
The complete revision/timing/artifact ledger is paired Akashic
`local_testing/evidence/rich-desktop-native-simulator-checkpoint-20260908.md`.
