# Native semantic execution

Native acceleration was developed on `simulator-runtime`, starting from
rich-terminal vertical `c10058b`. It is independent of the MP64 emulator's
C++/DBT extension. The Python semantic dispatcher remains the correctness
reference and owns operations outside each admitted native interval.

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

## First admitted interval

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
normal fault ordering and partial effects. Existing/root/fault continuations,
return-stack operations, counted loops, dynamic execution, stack-pointer
operations, and device/service access initially remain in Python. Successful
native prefixes settle their exact watchdog, diagnostic, and timer counts
before the next Python operation or host boundary. Native execution does not
invent IDL, reset a budget, or admit external events between guest boundaries.

The extension retains the GIL only for bounded execution intervals. Custom
clock callbacks, custom semantic service types, or custom stack/memory types
remain on the reference path so their observable behavior is preserved.
See `simulator/accel/API.md` for the internal boundary.

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
