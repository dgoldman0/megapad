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

## Shared exact scalar kernel

The same 64-iteration FP case was repeated with the exact C++ kernel and
adapters, with three unprofiled trials and separate attribution. These are
small, fresh-instance kernels; the simulator still crosses the Python word
boundary for each FP operation in this slice.

| Executor | Before wall ms | Kernel wall ms | Before / after |
|---|---:|---:|---:|
| emulator-native | 20.257 | 0.144 | 140.44× |
| simulator-python | 4.243 | 3.895 | 1.09× |
| simulator-native | 3.577 | 2.155 | 1.66× |

Emulator attribution drops all 256 scalar FP Python fallbacks and the associated
per-operation synchronization calls to zero. The Python simulator control is
unchanged; its timing movement illustrates host variability. Architectural
cycles, known result bits and flags remain equal. Native semantic FP still
returns to Python for each BIOS word, making that boundary the next target.

The source manifest includes unrelated pending audio adapter edits, which this
FP case does not execute. This comparison supports the scalar path only, with
no Desktop, full program, or hardware timing claim.

Raw evidence: [runtime-hotspots-fp-kernel-2026-09-30.json](runtime-hotspots-fp-kernel-2026-09-30.json).

## Qualified PCM span capture

The original 64 × 4096-byte guest MMIO submission case now uses one checked
span copy per buffer on canonical hosted memory. The method keeps captured
bytes immutable and preserves generation, metadata and error publication.

| Executor | Before wall ms | Bulk capture wall ms | Before / after |
|---|---:|---:|---:|
| simulator-python | 207.966 | 1.037 | 200.54× |
| simulator-native | 191.241 | 1.102 | 173.54× |

This isolates headless submission overhead, not audio rendering or host playback.
Both paths previously called Python for each of 262,144 sample bytes. The new
per-submission checks retain byte dispatch for customized callbacks/helpers or
overlapping architectural apertures. Emulator audio correctness is tested, but
its transfer latency is not measured in this harness.

All 57 audio tests pass, including late callback replacement, sparse page
boundaries/holes, all emulator windows, partial aperture overlap, malformed
bulk returns, capture immutability, failure publication and sink lifecycle.
Raw evidence: [runtime-hotspots-audio-2026-09-30.json](runtime-hotspots-audio-2026-09-30.json).
