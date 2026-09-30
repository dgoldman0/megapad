# Unified runtime hotspot baseline — 2026-09-30

Measured the unchanged runtime at `ca66ad7` in a separate detached worktree,
using its preserved native binaries and the new benchmark harness. The report
records every source hash, binary hash, host/interpreter identity, guest result,
unprofiled sample, and separate attribution sample. Only the harness and its
tests were untracked; runtime sources were unchanged.

## Method

```sh
MP64_RUNTIME_NAMESPACE=unified-profile-baseline python bench_runtime_hotspots.py \
  --iterations 64 --trials 3 --warmup 1 --attribution \
  --output runtime-hotspots-before.json
```

Each executor/workload pair runs in a fresh process with a 30-second watchdog.
Every trial prepares a fresh runtime outside the timed interval. Discarded
warmups warm process code, not retained guest or JIT state. Attribution repeats
the action separately under cProfile; it is excluded from timing medians.
Tests use the repository Make targets. The 19 harness tests passed.

| Workload | Executor | Median wall ms | Median process ms |
|---|---|---:|---:|
| fp64 | emulator-native | 20.257 | 20.256 |
| fp64 | simulator-python | 4.243 | 4.242 |
| fp64 | simulator-native | 3.577 | 3.576 |
| tile-fp64 | simulator-python | 1.344 | 1.343 |
| tile-fp64 | simulator-native | 1.580 | 1.579 |
| sha3 | simulator-python | 102.974 | 101.232 |
| sha3 | simulator-native | 114.437 | 114.436 |
| audio | simulator-python | 207.966 | 207.965 |
| audio | simulator-native | 191.241 | 191.240 |
| source-load | simulator-python | 1.432 | 1.431 |
| source-load | simulator-native | 1.279 | 1.278 |

These are bounded mechanism measurements, not cross-mode speed rankings:
architectural instructions and semantic steps are different workloads. Small
millisecond cases are sensitive to host noise. Emulator coverage is currently
scalar FP only; unsupported combinations are explicit in the report.

## Decisions

Scalar FP runs four binary64 operations per iteration and checks known result
bits and sticky flags. The native emulator still crosses into Python once per
FP instruction and synchronizes CPU state. Hosted native execution still exits
for each FP word. This supports extracting an exact shared value kernel first,
then admitting canonical compiled FP words to native semantic execution.

The audio case submits 64 checked 4096-byte PCM buffers through guest MMIO.
It checks immutable captured bytes, generation and metadata. Its byte-reader
cost supports a qualified bulk copy with a callback-preserving fallback.

SHA3 hashes a 256-byte message per iteration and verifies hashlib’s independent
digest. Tile FP64 performs an eight-lane add and checks all output bytes.
Synthetic loading compiles simple definitions and validates their count and
execution after timing; it is not a full KDOS load profile. These provide
controls and candidates for later extraction, not authority to replace services
without their own compatibility gates.

Full KDOS loading, Desktop interaction, physical display and audio playback
remain unmeasured by this harness. Executor default promotion remains deferred.
Peak RSS is the subprocess lifetime high-water mark, including preparation; it
is not isolated workload allocation.

Raw evidence: [runtime-hotspots-baseline-2026-09-30.json](runtime-hotspots-baseline-2026-09-30.json).
