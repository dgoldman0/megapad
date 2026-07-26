# Megapad deterministic concurrency: Phase 4 plan

**Started:** 2026-07-26

**Status:** Elements 1 and 2 of 6 complete; Element 3 not started

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Phase 3 handoff:** `docs/megapad-concurrency-phase3-handoff.md`

**Phase 3 snapshot:** `e0e75e844d8dce113670a642f59dde3d19999ba2`

## Purpose

Phase 4 is the safe-throughput optimization phase for the deterministic
concurrency architecture completed in Phase 3. It begins by measuring the
existing implementation without changing guest-visible behavior, then removes
demonstrated host costs in bounded milestones. Every optimization remains
subordinate to the Phase 3 architecture and equivalence oracles.

The Phase 3 handoff is the normative starting point. Its frozen architecture
decisions, P3-D1 through P3-D19 contentions, exact workload oracles, resource
warnings, and sanitizer evidence must be read before changing scheduler,
failure, timing, replay, host-access, or cache-observation behavior.

## Fixed phase structure

Phase 4 is divided into exactly six elements. These identifiers and scopes are
stable; a discovery can require a clearly reported corrective commit at the
current milestone, but it does not create a new element or sub-element.

| Element | Scope | Status |
|---|---|---|
| 1 | Measurement and attribution | Complete |
| 2 | Scheduler/frontier fast path | Complete |
| 3 | Longer proven-private execution | Pending |
| 4 | Host decode/JIT-style cache | Pending |
| 5 | Shared/MMIO/DMA optimization | Pending |
| 6 | System closure/final validation/handoff | Pending |

## Architecture-preservation gates

Every Phase 4 element must pass the following gates before its milestone
commit:

- One lane remains the thread-free reference. Supported host lane counts
  remain exactly one, two, and four, with physical lane identity and helper
  completion order excluded from guest-visible behavior.
- Complete logical frontiers, global cyclic commit order, equal round-robin
  service among simultaneously eligible peers, hard eligibility, and
  work-conserving reuse of unused capacity remain unchanged.
- Helpers execute only proven-private work. Shared effects, callbacks,
  arbitration, externally visible ordering, and final stop selection remain
  coordinator-owned unless a separately reviewed architecture change is
  explicitly approved.
- The distinct unbounded and strict-cycle contracts remain distinct.
  Optimization may not infer reduced-core strict timing, widen a strict helper
  command without an exact latency proof, or move an event, DMA, bus,
  interrupt, or resumable boundary across its established frontier.
- Callback failures, partial-progress settlement, replay release boundaries,
  authoritative stop cycles, public accounting, and cluster grant/loss
  behavior retain the Phase 3 contracts and contention ledger.
- Full-core private guest instruction caches retain their intentional
  noncoherence. Any host decode cache must derive identity from the bytes
  architecturally visible through the guest cache path, not from fresher raw
  backing memory.
- No optimization may depend on speculative architectural mutation or rollback
  unless that design is separately approved and supplied with exact failure
  oracles.
- For every affected workload, one-, two-, and four-lane runs must remain
  exactly equal in canonical state, behavior, ordered public accounting,
  cycles and stop state, and applicable event, DMA, or replay results.
- Host-only diagnostics and optimization metadata must not enter snapshots,
  canonical hashes, replay formats, public guest accounting, scheduling
  choices, virtual time, or stop selection.
- Tests and benchmarks remain sequential and resource-monitored. Checked-in
  step limits are not raised, and the large Phase 3 selections identified in
  the handoff are not repeated without explicit approval.

## Element 1 measurement contract

Element 1 establishes evidence before performance-sensitive behavior changes.
It adds versioned, opt-in host profiling to the existing native system and
extends the existing concurrency benchmark rather than creating a competing
measurement framework.

- Timed benchmark samples run with instrumentation disabled. A separate
  profiled replay or probe gathers attribution counters so profiler overhead
  is not presented as application throughput.
- Profiling is disabled by default, explicitly reset at the start of a probe,
  and frozen before its snapshot is reported. The report names its schema,
  workload, lane width, instruction budget, and relevant build/source
  provenance.
- Counters describe stable coordinator concepts: batch preparation, scheduler
  rounds, logical sub-frontiers, physical private cohorts or waves, private
  commands and retired steps, private stop reasons, coordinator-boundary
  settlements, and round settlement. Timers may measure the corresponding
  host wall time.
- Timing scopes may be nested. Batch time includes its descendants; a sum of
  per-lane execution time may exceed cohort wall time when helpers overlap.
  Derived residuals are diagnostic hypotheses, not an additive causal
  partition.
- The uninstrumented path may pay only a minimal disabled check. Element 1
  acceptance includes evidence that enabling, stopping, and reading profiling
  does not alter the architectural oracle.
- Benchmark output records measured structure as well as rate: commands,
  private steps, logical frontiers, physical waves, coordinator boundaries,
  scheduler rounds, and useful work per boundary. This distinguishes
  boundary-density costs from private-execution costs.

## Evidence discipline

Phase 4 performance claims are workload-specific. The established Phase 3
private-compute, shared-memory, MMIO-poll, timer-interrupt, and legacy
storage/display scenarios remain the comparison set, with strict NIC/disk DMA
used where its contract is affected.

Each optimization milestone must record:

- the clean source revision and native artifact identity;
- the exact command, benchmark schema, workload parameters, lane counts,
  warmups, repeats, and instruction or byte budget;
- wall time and peak resident memory for material benchmark or test processes;
- before/after throughput and attribution values from comparable builds;
- exact architectural equivalence results for every affected lane width; and
- any limitation that prevents a result from supporting a broader claim.

Small profiling runs may guide implementation, but a clean, reproducible
comparison is required before claiming an improvement. Noise, a single lucky
repeat, aggregate CPU utilization, or command counts alone are not proof of
speedup. A regression in one workload is not hidden by a gain in another.

## Element 1 completion record

Element 1 was implemented at
`1a211f8407d9c016aeb1862e4648ce85edbebe43`. The implementation adds a
versioned host-profile schema to the native owner, advances the concurrency
benchmark to report schema 9, and supplies focused lifecycle, scope,
cross-lane, callback, and noninterference oracles.

The profile is active only inside an explicitly started unbounded native
system batch. Direct private diagnostics and strict-cycle helper waves do not
populate it. Profile start and stop reject callback reentry while a native
batch is active, so a frozen generation cannot be reset or mutated by older
timers. Opt-in command telemetry is carried in sidecars whose lifetime covers
the complete worker wave; the unprofiled `PrivateCoreResult` layout and
per-instruction loop remain unchanged.

### Clean attribution report

The authoritative bounded report was generated from a clean implementation
revision:

```text
python bench_phase0_concurrency.py \
  --quick \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios private_compute,shared_memory,mmio_poll \
  --host-profile \
  --output build/phase4-e1-baseline.json
```

| Field | Value |
|---|---|
| Repository revision | `1a211f8407d9c016aeb1862e4648ce85edbebe43` |
| Repository dirty flag | `false` |
| Report schema | `megapad.phase0-concurrency-baseline`, version 9 |
| Canonical-state schema | `megapad.phase0-canonical-state`, version 9 |
| Generated | `2026-07-26T19:45:26.794838+00:00` |
| JSON size | 9,936,582 bytes |
| JSON SHA-256 | `b1ee2853c801a4d01597136af51e129c0d19721f06daea4eb880a61294807329` |
| Fixture-manifest SHA-256 | `eae0a8a09637c6e6651353f85113331f04a605d818604300174d615de63eddfb` |
| Native artifact | `_mp64_accel.cpython-313-x86_64-linux-gnu.so`, 2,187,488 bytes |
| Native artifact SHA-256 | `ee41c3af45e2842fc64d0b8b7345c93e28ed38ca9e5d74b4d7471e8e6a66511e` |
| Native ELF build ID | `0328d64e2fe07bacc40970e0b91520f10f73e66f` |
| Benchmark resource use | 2.49 seconds; 87,300 KiB peak; no swap |

All 24 report-level validation booleans are true. Each of the nine host
profile probes has all 29 reconciliation checks true. Timed samples run with
profiling disabled; the separate accounting replay is profiled and still
matches the timed behavior oracle. One-, two-, and four-lane canonical state,
behavior, and ordered public accounting are exactly equal for every selected
workload. The automatically included strict NIC/disk DMA probe also remains
cross-width equivalent.

| Workload | Canonical-state SHA-256 | Behavior SHA-256 | Ordered public-accounting SHA-256 |
|---|---|---|---|
| Private compute | `f691d8bf557d03805e0f5284a6a3bb5a88c838bba4f5382e132ca3394143b118` | `cfc1df2ca938eecef32007583b054fb992b3a9483b94f08eae6cc14e41c1ff96` | `f0fd0542ec7dd0846e1761e4d7e909e372f5cbb5c1559da4d2a553c372f20e2a` |
| Shared memory | `ee11a0fa939863b87a76d9a79175af5095ce1359a454f501ef1ddbc3e143e02f` | `546afdfa1c6018959aefb893d7e7e8760626b7c4fff5700f2033e254f926ea4d` | `f0fd0542ec7dd0846e1761e4d7e909e372f5cbb5c1559da4d2a553c372f20e2a` |
| MMIO poll | `554c06d61b9d1172dc6d32cca6d8fd29b1571b98ecfa8b29775d2cef4f589595` | `9a4457cd66849c6a9bb3af74602efac31fa7fe9b72471d23a6b6efcfc2ab6fe4` | `f0fd0542ec7dd0846e1761e4d7e909e372f5cbb5c1559da4d2a553c372f20e2a` |

Uninstrumented single-sample throughput from the bounded report is:

| Workload | 1 lane MIPS | 2 lanes MIPS / relative | 4 lanes MIPS / relative |
|---|---:|---:|---:|
| Private compute | 49.741 | 66.200 / 1.331 | 73.862 / 1.485 |
| Shared memory | 1.753 | 0.680 / 0.388 | 0.610 / 0.348 |
| MMIO poll | 1.096 | 0.501 / 0.457 | 0.545 / 0.498 |

These one-repeat quick rates are diagnostic, not a stable performance
acceptance threshold. Their architectural observations and the structural
profile counts are the Element 1 oracle.

### Structural attribution

The selected workloads separate long useful private spans from
boundary-dense execution:

| Workload | Private steps / command | Zero-step commands | Logical sub-frontiers | Worker commands | Coordinator boundaries | Waves at 1 / 2 / 4 lanes |
|---|---:|---:|---:|---:|---:|---:|
| Private compute | 961.500 | 4 / 104 (3.85%) | 26 | 104 | 4 | 104 / 52 / 26 |
| Shared memory | 1.496 | 20,100 / 40,100 (50.12%) | 10,025 | 40,100 | 40,000 | 40,100 / 20,050 / 10,025 |
| MMIO poll | 1.496 | 20,100 / 40,100 (50.12%) | 10,025 | 40,100 | 40,000 | 40,100 / 20,050 / 10,025 |

The nested host timers from the profiled accounting replay further localize
the cost. Values below are milliseconds and include profiling overhead; the
columns overlap and must not be added into a causal partition.

| Workload / lanes | Batch total | Worker waves | Worker wait | Checkpoint capture | Coordinator boundary | Python MMIO callbacks |
|---|---:|---:|---:|---:|---:|---:|
| Shared / 1 | 71.385 | 38.960 | 0.856 | 13.573 | 10.980 | 0 |
| Shared / 2 | 165.299 | 126.580 | 80.143 | 16.139 | 14.785 | 0 |
| Shared / 4 | 152.927 | 112.302 | 70.255 | 17.538 | 16.737 | 0 |
| MMIO / 1 | 117.487 | 41.062 | 0.901 | 14.539 | 52.352 | 30.367 |
| MMIO / 2 | 211.474 | 129.446 | 80.972 | 16.476 | 56.810 | 30.996 |
| MMIO / 4 | 191.810 | 113.448 | 74.368 | 15.786 | 55.671 | 29.401 |

This establishes the initial Phase 4 priority without claiming exact causal
percentages:

- Boundary density, not cache refill, distinguishes the slow workloads:
  shared and MMIO issue roughly 386 times as many worker commands as private
  compute for the same 100,000-instruction target, and half do no private work.
- The multilaned regression is concentrated in the worker-wave protocol.
  Coordinator wait rises from less than one millisecond at one lane to roughly
  70--81 milliseconds at two/four lanes even though physical wave count falls.
  Wave posting, waiting, and gathering are therefore the first Element 2
  targets.
- Per-command rollback checkpoints are also material, contributing roughly
  14--18 measured milliseconds in the boundary-dense replays. Removing or
  narrowing them requires preserving the established callback-failure and
  strict candidate-set semantics.
- Round absorption measures roughly 3.2--4.0 milliseconds in these same
  replays. Its transactional copies remain a valid Element 2 target, but this
  probe does not identify them as the dominant first cost.
- Python MMIO callbacks contribute roughly 29--31 milliseconds at every lane
  width. They explain part of MMIO's absolute cost but not its multilaned
  collapse; broader MMIO work remains in Element 5.

### Uninstrumented hot-path comparison

The private workload was also rerun with the exact Phase 3 full-budget
settings and host profiling disabled:

```text
python bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios private_compute \
  --instructions 2m \
  --repeats 3 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 1024 \
  --output build/phase4-e1-disabled-private.json
```

The clean Phase 4 Element 1 result is 46.982 / 72.248 / 109.468 MIPS at
one/two/four lanes. The Phase 3 handoff recorded 48.197 / 66.724 / 96.416
MIPS. The mixed -2.5% / +8.3% / +13.5% movement does not show a systematic
disabled-profile tax. This report has all 24 validations true, is 17,942,077
bytes, and has SHA-256
`c29b1df7bd7c6c43527d200b2f3593452e81a542252e5c17af9b03e0b2a98a59`.
It ran in 1.70 seconds with 120,292 KiB peak and no swap.

### Regression and sanitizer evidence

All gates were foreground, sequential, and resource-monitored:

| Gate | Result | Peak RSS | Interpretation |
|---|---:|---:|---|
| Affected optimized selection | 135 passed | 79,152 KiB | Private execution, worker pool, coordinator, cycle execution, Phase 3 benchmark, and all new profile oracles |
| Focused ASan/UBSan profile gate | 3 passed | 2,304,288 KiB | No address or undefined-behavior finding in cross-lane sidecars, callback reentry rejection, or scope exclusion |
| Focused TSan profile gate | 3 passed | 1,836,972 KiB | No race report in the same low-budget profile selection |

Generated JSON remains ignored. Its exact commands, clean source revision,
artifact identity, sizes, hashes, and resource measurements are preserved
above so the evidence can be reproduced without adding large generated files
to Git.

## Element 2 implementation record

Element 2 targets the measured per-wave posting, sleeping, collection, and
unnecessary rollback-copy costs without changing a logical frontier. Its
implementation milestone is
`e4918e99c314ac1984885e1360a4bc3c75769ba8`.

The worker protocol now publishes `POSTED` mailboxes before unlocking and
wakes helpers only after the mutex is released. The coordinator no longer
broadcasts an impossible `IDLE` work notification after collection. A
mutex-protected outstanding-helper count lets only the final posted helper
wake the sole coordinator waiter. Mailbox state, the count, and the completion
predicate remain under the same mutex; no lock-free slot reads, timing-driven
spins, or adaptive scheduling choice was introduced.

The unbounded full-core scheduler now performs one read-only preclassification
under the existing sub-frontier admission. A command already at an I-cache or
shared/coordinator boundary returns an equivalent zero-progress result without
entering the worker pool. A command whose first instruction is proven private
carries that single-use proof to its worker, where the full command continues
under the existing private executor. Every actual worker result is remapped to
its original cohort position, all peer-private work is still gathered before
the first cyclic settlement or failure selection, and bypassed work is never
credited to a physical lane, command sequence, wave epoch, or helper thread.

This shortcut is deliberately limited to unbounded full cores. Strict-cycle
waves retain their established simultaneous one-instruction contract.
Microcores retain worker-side coherent shared-RAM classification and cluster
revalidation. Direct private diagnostics retain their existing public worker
API and accounting. No admission survives into a coordinator or Python
callback.

The whole-command rollback checkpoint remains intact. Its capture is delayed
past validation, pending-interrupt and halted/idle exits, and read-only
classification, but occurs immediately before the first admitted
`step_one`. An unexpected failure after any private mutation therefore still
restores the complete command prefix; no speculative guest execution or
partial checkpoint was introduced.

Native host-profile schema 2 and benchmark report schema 10 distinguish
frontier routing waves and commands, actual full-core preclassification,
worker-bypassed commands and reasons, actual worker waves and commands, and
the fast-path wall scope. Worker counters continue to reconcile exactly with
worker-pool diagnostics. The planned-minus-actual wave count reports whole
physical cohorts that never entered the pool instead of pretending a bypass
used a helper lane.

A dirty-tree 100,000-instruction development probe, used only to decide
whether a larger queued-frontier rewrite was justified, produced:

| Workload | Element 1 quick, 1 / 2 / 4 lanes | Element 2 development, 1 / 2 / 4 lanes |
|---|---:|---:|
| Private compute | 49.741 / 66.200 / 73.862 | 47.66 / 59.07 / 94.09 |
| Shared memory | 1.753 / 0.680 / 0.610 | 1.94 / 1.12 / 1.32 |
| MMIO poll | 1.096 / 0.501 / 0.545 | 1.14 / 0.76 / 0.94 |

These one-repeat rates are not milestone evidence. They do show a sufficiently
large target-workload improvement to stop before the higher-risk queued
frontier, generation/spin, or cross-frontier designs. In the profiled replay,
shared and MMIO routing each contained 40,100 commands; 20,100 were
coordinator-bypassed and 20,000 entered workers. Actual pool waves fell from
the Element 1 counts of 40,100 / 20,050 / 10,025 to
20,000 / 10,000 / 5,000 at one/two/four lanes. Checkpoint captures fell from
40,100 to 20,000. Shared worker-wait time fell from
0.856 / 80.143 / 70.255 milliseconds to approximately
0.416 / 40.576 / 23.632 milliseconds; MMIO wait fell from
0.901 / 80.972 / 74.368 to approximately
0.455 / 43.371 / 28.320 milliseconds.

### Clean acceptance comparison

The acceptance comparison built a detached clean worktree at the Element 1
snapshot and ran the same command there and at the clean Element 2
implementation:

```text
python3 bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios private_compute,shared_memory,mmio_poll \
  --instructions 500k \
  --repeats 5 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 1024 \
  --host-profile
```

The before invocation ran from
`/tmp/megapad-p4e2-baseline.fMZeaG` and appended
`--output /home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-concurrency/build/phase4-e2-before.json`.
The after invocation ran from the isolated concurrency worktree and appended
`--output build/phase4-e2-after.json`. Timed samples remained unprofiled.
Profiling was enabled only for each separate accounting replay.

| Field | Element 1 before | Element 2 after |
|---|---|---|
| Repository revision | `c5553d46e643ed60d21e06fb67d2bbfda15b6000` | `e4918e99c314ac1984885e1360a4bc3c75769ba8` |
| Repository dirty flag | `false` | `false` |
| Report schema | version 9 | version 10 |
| Canonical-state schema | version 9 | version 9 |
| Native/profile-probe schema | version 1 / version 1 | version 2 / version 2 |
| Generated | `2026-07-26T20:25:31.238307+00:00` | `2026-07-26T20:25:58.395249+00:00` |
| JSON size | 21,226,783 bytes | 21,238,647 bytes |
| JSON SHA-256 | `e7cf1f6cc33e8b5d2cf04c61e31f9348e5b209fd5fc1a2aa4ae985b1e68ddd8b` | `9791eadc2018fe0e0eeb30087e58732659749f18adc5f18366bb792f2c72531d` |
| Fixture-manifest SHA-256 | `b3867065c27ffd638315552453de988003d7b9e77b2f5801262fd4ce87c6436f` | `b3867065c27ffd638315552453de988003d7b9e77b2f5801262fd4ce87c6436f` |
| Native artifact size | 2,187,488 bytes | 2,195,768 bytes |
| Native artifact SHA-256 | `ee41c3af45e2842fc64d0b8b7345c93e28ed38ca9e5d74b4d7471e8e6a66511e` | `5c6987c997d976938344caffb305ded753ac3614cb0dfbe656572e33ae8b3ccf` |
| Native ELF build ID | `0328d64e2fe07bacc40970e0b91520f10f73e66f` | `5f9012e30b0f0855377e245aa88007589497d79b` |
| Benchmark resource use | 29.15 seconds; 140,644 KiB peak; no swap | 20.68 seconds; 150,128 KiB peak; no swap |

Both reports have all 24 report-level validations true. Every one of the nine
Element 1 probes has all 29 schema-1 reconciliations true; every Element 2
probe has all 37 schema-2 reconciliations true. Before/after canonical state,
behavior, and ordered public-accounting hashes match for every corresponding
workload and lane width. Strict NIC/disk DMA behavior and state also remain
cross-width and before/after exact.

Median uninstrumented aggregate throughput is:

| Workload | Lanes | Before MIPS | After MIPS | Change |
|---|---:|---:|---:|---:|
| Private compute | 1 | 41.800 | 44.103 | +5.5% |
| Private compute | 2 | 63.243 | 63.026 | -0.3% |
| Private compute | 4 | 95.696 | 103.250 | +7.9% |
| Shared memory | 1 | 1.629 | 1.767 | +8.5% |
| Shared memory | 2 | 0.652 | 1.018 | +56.1% |
| Shared memory | 4 | 0.640 | 1.243 | +94.3% |
| MMIO poll | 1 | 1.051 | 1.116 | +6.2% |
| MMIO poll | 2 | 0.525 | 0.723 | +37.8% |
| MMIO poll | 4 | 0.518 | 0.885 | +70.9% |

The private two-lane control moved by -0.3%, while the other private controls
moved positively; this does not indicate a systematic private-compute
regression. The short strict-DMA throughput medians moved by
+3.2% / +1.9% / -5.2% at one/two/four lanes. Element 2 does not claim a DMA
speedup, and the strict path retains exact state, behavior, service trace,
virtual-cycle cost, and focused strict-cycle oracles.

The profiled shared and MMIO replays each route 200,500 logical commands.
Element 1 sent all 200,500 to workers and captured 200,500 checkpoints.
Element 2 bypasses 100,500 proven zero-progress commands, posts 100,000
commands, and captures 100,000 checkpoints. Planned/actual pool waves change
from 200,500/200,500 to 200,500/100,000 at one lane, from
100,250/100,250 to 100,250/50,000 at two lanes, and from 50,125/50,125 to
50,125/25,000 at four lanes.

| Workload | Lanes | Before worker wait | After worker wait | Change |
|---|---:|---:|---:|---:|
| Shared memory | 1 | 4.502 ms | 2.242 ms | -50.2% |
| Shared memory | 2 | 385.292 ms | 217.125 ms | -43.6% |
| Shared memory | 4 | 415.957 ms | 167.197 ms | -59.8% |
| MMIO poll | 1 | 4.532 ms | 2.543 ms | -43.9% |
| MMIO poll | 2 | 382.623 ms | 224.696 ms | -41.3% |
| MMIO poll | 4 | 367.483 ms | 162.399 ms | -55.8% |

These nested profile timings include instrumentation and are diagnostic, but
the independent uninstrumented medians confirm the retained optimization.
The detached baseline worktree was clean, copied no generated artifacts into
Git, and was removed after the two reports and their identities were captured.

### Regression and sanitizer close

The clean implementation revision passes a 117-test sequential affected
selection in 1.67 seconds at 81,500 KiB peak with no swap. It covers
the complete logical frontier, equal QoS, cyclic commits, callback-failure
prefixes, reduced-core arbitration, direct private execution, worker
lifecycle, profile reconciliation, and report schema. New fixtures preserve a
later peer's private prefix across an earlier immediate-boundary callback
failure at every lane width and exercise 500 alternating partial-helper
reposts. Two independent read-only audits found no race, lost wakeup,
deadlock, checkpoint-containment, ordering, lifetime, capacity, schema, or
reconciliation blocker.

The focused sanitizer close is also complete. ASan/UBSan selected 13
immediate-boundary, checkpoint, complete-frontier, partial-repost, and profile
tests; all passed in 68.90 seconds at 2,307,656 KiB peak with no sanitizer
finding or swap. The same 13 tests passed under TSan in 36.73 seconds at
1,837,472 KiB peak with no race report or swap. The affected ordinary
selection and both sanitizer runs were foreground and sequential.

Element 2 is complete. The generated before/after JSON reports remain ignored;
their exact source and artifact identities, hashes, sizes, parameters, and
resource measurements are recorded above. Element 3 begins from the
`e4918e9` implementation and this evidence snapshot.

## Design-contention ledger

This ledger contains only decisions already required by Phase 4 work. New
contentions are added when an implementation choice actually arises, not
speculatively.

| ID | Contention | Phase 4 decision | Claim boundary and revisit trigger |
|---|---|---|---|
| P4-D1 | Profiling can itself perturb host execution, and exposing its state through architectural serialization or scheduling would make diagnostics guest-visible. | Instrumentation is host-only, opt-in, disabled for timed samples, and excluded from architectural state, snapshots, canonical hashes, replay, public accounting, virtual time, stop selection, and scheduling decisions. A separate profiled replay supplies attribution evidence. | Profile timings are diagnostic host observations, not architecture or a causal proof. Reopen the counter set or measurement method when it cannot distinguish a demonstrated cost, but never silently broaden profiling into guest-visible state. |
| P4-D2 | A single universal speedup threshold would reward only favorable workloads and could conceal regressions or equivalence failures elsewhere. | Require exact architectural equivalence, then judge benefit with workload-specific before/after measurements. No universal performance threshold is imposed. | A change may be retained only with an honestly stated, reproducible benefit and disclosed tradeoffs across affected workloads. Exact equivalence is mandatory regardless of speed; changing an architectural oracle requires a separately approved architecture decision, not a performance exemption. |
| P4-D3 | Skipping a worker for an immediate boundary can save the dominant protocol cost, but an early coordinator commit or failure could expose an incomplete peer-private frontier. Runtime-adaptive routing could also let host timing influence behavior. | Preclassify only unbounded full-core commands under the retained logical-frontier admission. Synthesize only proven zero-progress results, gather every remaining worker result, preserve original cohort position and global cyclic settlement, and keep the choice independent of timing, helper readiness, or completion order. Report bypasses separately from physical worker work. | The claim covers the existing read-only full-core classifier and zero-progress interrupt, halted/idle, I-cache, and shared boundaries only. Strict-cycle, microcore, speculative execution, cross-frontier fusion, and callback settlement remain unchanged. Revisit complete-frontier lane queues only if later measurements show this bounded fast path insufficient. |
| P4-D4 | Copying a complete CPU checkpoint for a command that exits before its first private instruction is wasted work, but moving capture past guest mutation would weaken whole-command failure containment. | Perform validation and read-only first-instruction classification before capture, then take the unchanged full checkpoint immediately before the first admitted guest `step_one` and retain it until command completion. | This removes checkpoints only from zero-mutation exits. It does not authorize partial checkpoints, checkpoint deletion for progressing commands, or mutation followed by speculative rollback. Reopen only with an injectable failure oracle that proves an equally strong containment boundary. |

Changes to these decisions must update this ledger and the corresponding
evidence in the same milestone. A green test suite or faster benchmark alone
does not broaden the architectural claim.

## Element completion boundaries

- **Element 1** completes when the versioned host-only profiler, benchmark
  probe, exact noninterference tests, and a clean attribution baseline are
  committed.
- **Element 2** completes when measured scheduler/frontier overhead is reduced
  without changing logical frontier formation, cyclic credit, commit order, or
  public accounting.
- **Element 3** completes when private execution spans are safely lengthened
  using proof rather than speculative guest mutation, with cross-width and
  failure-boundary evidence.
- **Element 4** completes when host decode/JIT-style reuse is implemented with
  correct guest-cache observation and invalidation identity, and is measured
  independently from guest BIOS compilation behavior.
- **Element 5** completes when demonstrated shared, MMIO, and DMA host costs
  are reduced while preserving callback, bus, arbitration, beat-order, and
  strict-cycle contracts.
- **Element 6** completes when the final one/two/four-lane oracle matrix,
  sequential sanitizer and resource-safe regression gates, refreshed
  benchmarks, design-contention review, and versioned Phase 4 handoff are
  committed.
