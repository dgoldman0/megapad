# Shared native tile value qualification

The emulator and native-selected simulator now use one C++ value kernel for
admitted TALU and MUL formats, floating MAC/FMA, widening multiply, dot and
reductions, and extended select/compare/divide/square-root/conversion.
Integer non-MUL multiply/reduction paths and transpose retain their existing
implementations. Hosted full-width TACC remains unsupported.

The kernel accepts immutable bytes and returns bounded value candidates.
Adapters retain source-read order, sequential destination writes, partial
fault effects, ACC/TCTRL/Z publication, TACC ownership, timing and accounting.
Floating operations preserve the caller's host environment and establish
round-to-nearest with subnormals within a coarse arithmetic-only scope.
The Python tile and exact integer FP models remain independent oracles.

## Admission cost and paired results

Every hosted operation rechecks service, memory, register and reference-helper
identities. The first extraction performed these checks in Python and regressed
the measured case. Its attribution identified the identity loop as the new
dominant cost. A bounded binding-layer guard now compares those exact identities
in C++, including late overrides, without invoking user equality or attribute
hooks. It caches no mutation generation. Direct callers can retain the Python
guard, and customized contexts retain Python value execution.

All runs use 4,096 FP64 TADD operations, eight lanes per operation, fresh
subprocesses, three unprofiled trials and one discarded warmup. Attribution is
separate and setup is excluded from the workload interval.

| Selected executor | Pre-extraction median | First extraction, Python guard | Native value and identity guard |
| --- | ---: | ---: | ---: |
| Python control | 85.201 ms | 81.491 ms | 79.478 ms |
| Native semantic | 83.986 ms | 88.786 ms | 65.133 ms |

The final native median is 1.29× the baseline throughput for this bounded
case. The unchanged Python control also varied by about 6.7%, so this is a
small-sample host measurement, not a precise general speedup estimate.
All runs produced 12,292 semantic steps, 4,096 tile operations, an empty stack
and output SHA-256
`ef7471373af4863191e0115203bb4ebe33028ba5e764fe726198692a375bd33c`.
No Desktop, full-program, or modeled-hardware speedup follows from this result.

- [`runtime-tile-baseline-2026-09-30.json`](runtime-tile-baseline-2026-09-30.json)
- [`runtime-tile-python-guard-2026-09-30.json`](runtime-tile-python-guard-2026-09-30.json)
- [`runtime-tile-native-2026-09-30.json`](runtime-tile-native-2026-09-30.json)

## Qualification

Both native extensions built sequentially with GCC. Make-selected gates passed:

- 1,478 shared-value, hosted-service, existing architectural oracle and FP64
  portability checks; one existing illegal-FP64-WMUL case is deliberately
  skipped because its trap is checked separately.
- 119 architectural tile/TACC, memory-arbitration, scheduler, scalar FP and
  private-execution checks.
- After adding the native guard, all 140 value checks and 46 guard checks,
  plus the 118 hosted adapter checks passed. Guards cover late replacement,
  restoration, class/module aliases, custom attribute routing and hostile
  dictionary keys. The adapter gate includes aliasing and failed later writes.

The gate also exposed and fixed a pre-existing Python 3.12 `math.fma`
availability bug. The reference now falls back to its exact integer FMA;
40 portability checks cover both availability branches. The separate
NumPy-dependent IEEE suite could not collect in this Python environment.
