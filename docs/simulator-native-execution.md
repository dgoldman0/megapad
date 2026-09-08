# Native semantic execution

Native acceleration is being qualified on `simulator-runtime`, starting from
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
predicates used by unified publication; native return-stack operations are
the next bounded coverage candidate. Do not merge this execution-engine work
back solely on this first kernel result: preserve differential correctness
and require substantial improvements at the ordinary physical Desktop
checkpoint and complete interaction journey.

The prior Python simulator reached the first complete rich Desk ACK at
897.181846 seconds and timed out at 900 seconds. The accelerated emulator's
corresponding ACK was 486.098092 seconds and its full journey passed. Detailed
source bindings and timing boundaries are in paired Akashic
`local_testing/evidence/rich-desktop-speed-decision-20260908.md`.
