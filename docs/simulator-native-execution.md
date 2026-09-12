# Native semantic execution

Native acceleration was developed on `simulator-runtime`, starting from
rich-terminal vertical `c10058b`. It is independent of the MP64 emulator's
C++/DBT extension. The Python semantic dispatcher remains the correctness
reference and owns operations outside each admitted native interval.

**Current checkpoint — September 12, 2026:** The isolated
`simulator-improvements` branch passes the full ordinary source-mode rich
Desktop journey on the native simulator, including Sound Lab. Scalar
return-stack operations and ordinary preexisting continuations now stay native;
AudioOut uses the shared PCM model. Native remains opt-in. The September 8
measurements and failure description below are historical provenance.

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
Identity-bound `TRUE`, `FALSE`, `CELLS`, and `UM*` also stay native.
Pair return-stack operations, dynamic execution, stack-pointer operations, and
device/service access remain in Python. Successful
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
