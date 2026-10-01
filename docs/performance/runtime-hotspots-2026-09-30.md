# Unified runtime hotspot baseline — 2026-09-30

Measured the unchanged runtime at `ca66ad7` in a separate detached worktree,
using its preserved native binaries and the new benchmark harness. The report
records every source hash, binary hash, host/interpreter identity, guest result,
unprofiled sample, and separate attribution sample. Only the harness and its
tests were untracked; runtime sources were unchanged. The raw JSON reports of
these runs remain in the repository history.

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

The harness now also accepts eight opt-in residual-cost probes without changing
its original defaults: `aes-gcm32`, `page-hot`, `page-scattered`,
`page-crossing`, `continuation-short`, `continuation-long`, `ntt-compute`, and
`ntt-transfer`. Their independent checks cover complete AES ciphertext/tag,
page-layout checksums and unchanged source bytes, continuation retained-state
parity, and a direct modular DFT. Each has an explicit iteration bound; the
continuation cases also bound host resumes. The harness gates passed 105 checks.
Adding these probes establishes no new performance result or extraction priority
until their unprofiled and attribution runs are recorded.

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

## Direct semantic FP calls

Original compiled BIOS FP words now execute inside the native semantic interval,
with one authoritative FPCSR settled before returning to host accounting. The
same case, trial count and validation checks produced these wall medians:

| Executor | Baseline ms | Value kernel ms | Direct semantic FP ms |
|---|---:|---:|---:|
| simulator-python | 4.243 | 3.895 | 3.583 |
| simulator-native | 3.577 | 2.155 | 0.119 |

Native attribution now accounts for 1,603 semantic steps in one productive
native entry. No scalar operation crosses the service boundary in this loop.
Focused tests prove this independently with a service hook that fails if called.
The reference timing continues to vary; small absolute timings should not be
read as a precision ranking or a full-program result.


## Shared Keccak permutation

The emulator's existing Keccak-f[1600] permutation now lives in a shared native
value kernel. Native-selected hosted SHA3 uses it for absorb, finalize, squeeze
and raw operations; ownership, padding, byte transfers and publication remain
with the same service. The Python oracle remains independent.

| Executor | Before wall ms | Native permutation wall ms | Before / after |
|---|---:|---:|---:|
| simulator-python | 102.974 | 101.956 | 1.01× |
| simulator-native | 114.437 | 51.404 | 2.23× |

Each case still hashes 64 × 256-byte messages and checks exact hashlib digests
and guest status. Native routing is also proven with observed native calls in
raw/SHA3/SHAKE tests. Byte MMIO dispatch remains and is a candidate for later
qualification; this slice does not bypass its ownership or effect ordering.
The native emulator already used this arithmetic, so no emulator throughput
gain is claimed from moving it.

Validation: 86 direct-kernel/native-device/differential checks, 47 hosted KDOS
SHA3 checks in each executor, and 26 native WOTS dependency checks pass.
The native-selected source gate also exposed an older FaultAbort continuation
classification bug, reproduced at the untouched baseline and fixed separately
in `957d272` with three focused regressions.


## Representative KDOS source-loading controls

The existing `bench_simulator_kdos_load.py` completed three fresh-process,
unprofiled trials per executor on the preserved baseline and the runtime now
committed as `bb50f9b`. Each subprocess had a 90-second wall watchdog. Runtime
and fixture preparation are reported separately from the source-load interval.

All twelve runs passed the existing source hash, dictionary/publication, startup
transcript, stack and representative-word checks. Each loaded 6,684 submitted
lines, 222,397 packed bytes and 1,461 KDOS words, charging 36,116 semantic steps.

| Executor | Baseline median source-load ms | Current median source-load ms |
|---|---:|---:|
| python | 394.331 | 401.111 |
| native | 377.572 | 364.361 |

This workload checks loading compatibility outside the extracted hot kernels.
No compiler change was made, and timing movement is not attributed to the FP,
audio or Keccak changes. These trials do not include Desktop modules or input
interaction. Desktop qualification remains open;
executor default promotion is still deferred.


## Diagnostic source and compositor attribution

Separate cProfile runs of the existing full KDOS harness passed its exact
source, dictionary, transcript, stack and storage checks in both executors.
The attribution run recorded source provenance, checked harness results and
function costs. The profiler covers setup and validation too;
none of these wall times replace the unprofiled controls above. Parsing and
checked scalar memory operations remain visible Python costs in both
executors: `parse_word` has about 0.14 s cumulative attributed time in each
run, while `_write_integer` has about 0.21 s. Setup also includes disk format.
Cumulative functions overlap and must not be added together. Native/Python
reentry can distort profiler call counts; the harness's checked submitted-line
and semantic-step counts remain the work authority. This evidence does not
justify a new compiler or memory extraction without a targeted paired gate.

The existing ready/typed Desktop and 12-offer typing-sequence tests also passed
through Make with diagnostic profiling. Every pixel and hit map matches the
complete reference. All 11 later typing offers take the partial-repaint path,
each with damage below one quarter of the full frame. These are checked-in
MegaPad fixtures; no external project sources were accessed or changed.
The run recorded their hashes and attribution.

That profile includes test collection, decoding, both optimized rendering and
full-reference rendering. It attributes 0.368 s across 12 incremental compose
calls and 1.650 s across 17 full compose calls, but their workloads differ and
these numbers are not a speedup ratio. Wire-to-offer decoding is also visible
(0.807 s across 42 calls, including collection/setup). These costs identify
places to inspect if a live trace confirms they dominate. Captured offers do
not measure guest input-to-frame latency, physical presentation, or a complete
live Desktop session. Those acceptance and default-promotion gates remain open.
