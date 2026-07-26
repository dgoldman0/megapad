# Megapad deterministic concurrency: Phase 4 plan

**Started:** 2026-07-26

**Status:** Elements 1 through 4 of 6 complete; Element 5 not started

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
| 3 | Longer proven-private execution | Complete |
| 4 | Host decode/JIT-style cache | Complete |
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

## Element 3 implementation record

Element 3 begins with the mixed-topology one-instruction clamp established by
Phase 3 decision P3-D10. The initial implementation applied that clamp whenever
the machine contained any microcore, even when one logical participant was the
only executable or coordinator reservation in the gathered subfrontier. Other
configured or zero-credit peers cannot act in that subfrontier, so there is no
peer instruction fetch, shared callback, code write, arbitration result, or
interrupt assertion to cross. The existing private runner can therefore
consume the participant's remaining bounded round credit while continuing to
classify every instruction immediately before mutation and stopping at its own
first shared, routed, cache, trap, reset, halt, idle, or uncertain boundary.

The optimization is deliberately narrower than an all-full participant test.
Even if every current participant is a full core, a configured mixed topology
has versioned one-instruction callback and exception-prefix ordering under
P3-D10. Widening one full prefix independently could discover a cyclic-earlier
later boundary in the same gathered frontier as a cyclic-later immediate
boundary, reversing their previous callback order. A peer callback could also
assert an interrupt after a widened command had already run past the prior
subfrontier boundary. Every multi-participant mixed subfrontier therefore
remains capped to one instruction, including zero-credit cluster probes.
Strict-cycle execution is unchanged.

The retained single-active-microcore benchmark was prepared independently at
`ba778f5dde7bef030ec4ba611c643b53f1d1d825`. A read-only audit then found that
its per-sample systems contain Python callback cycles and persistent native
worker pools, while schema 4 neither controlled cyclic collection around the
timed interval nor forced teardown after each sample. Its first timing report
is therefore discarded; its architectural hashes remain valid but are not
used as the Element 3 baseline.

The correction at `0cb0f25764635b256e230fa9a9eac4d8b7b54c1c` advances the
report to schema 5 without changing Phase 2 state schema 3. Every sample now
collects before timing, disables cyclic GC during timing, restores the caller's
GC state, and collects after releasing all system references so helper-pool
joins cannot contaminate a later sample. The report requires the complete
one/two/four-lane comparison, strengthens schema-2 host-profile
reconciliations, and tests source/artifact provenance and native-owner
teardown. Timed samples remain unprofiled.

New focused oracles cover sole full-core and sole microcore 2,000-instruction
spans across one, two, and four lanes; a multi-participant frontier shrinking
to one survivor that consumes its longer remaining credit; coherent
self-modifying code after a private microcore prefix; retained prefix and
original exception identity at a failing microcore coordinator callback; and
negative active-mixed fixtures that preserve the versioned callback order and
stop before a peer-asserted interrupt. The focused gate has seven passing
tests. Broader regression, sanitizer, and clean after-evidence remain before
Element 3 completion.

### Element 3 clean baseline

The replacement clean baseline uses detached revision
`0cb0f25764635b256e230fa9a9eac4d8b7b54c1c`, report schema 5, state schema 3,
500,000 instructions, five timed repeats, one 100,000-instruction warmup, a
separate host-profile replay, and one/two/four lanes. It was generated at
`2026-07-26T21:07:09.922518+00:00`. The repository dirty flag is `false`.

The 243,506-byte JSON has SHA-256
`4df63ea43df20189eb069c5525c2783c4e330da11453c00d9bdbf81ebf6cfb3f`.
The 2,195,768-byte native artifact has SHA-256
`5c6987c997d976938344caffb305ded753ac3614cb0dfbe656572e33ae8b3ccf`
and ELF build ID `5f9012e30b0f0855377e245aa88007589497d79b`. The run took
30.25 seconds, peaked at 30,020 KiB, and used no swap.

Every report, worker, host-profile, timing-hygiene, and cross-lane validation
is true. Canonical state, behavior, and ordered public accounting are exact
across lane widths, with hashes
`acefb4be60d898244b47d2a9254fd8a7405593e6c6014843d1ea2f8f1c036e5a`,
`4882f2009791627b60bf1039bb9a53de2c2512931a97fa0b31599f6723e70501`,
and `2bc0689dd18e20a33870e568d5661133f8ea14a22d39971c880e4c5bc4a8d807`.
Median throughput is 498,192 / 316,922 / 231,306 instructions per second at
one/two/four lanes. Each profiled width reports exactly 500,000 logical
subfrontiers, worker commands, and checkpoint captures for 500,000 private
steps; this is the structural cost Element 3 is intended to remove.

### Element 3 regression and sanitizer gate

Two independent read-only audits found no correctness blocker in the
sole-participant widening. They confirmed that every successive instruction is
still classified before mutation, the unchanged whole-command checkpoint is
captured immediately before the first mutation, pending interrupts are checked
before participation and command formation, and returned progress remains
bounded by admitted credit. Their two actionable findings were the dynamic
multi-participant-to-sole coverage gap and the benchmark lifecycle flaw; both
are closed above.

The seven-test focused Element 3 file passes in 0.08 seconds. Its complete
foreground command takes 0.60 seconds at 44,280 KiB peak with no swap. A
123-test affected selection passes in 1.60 seconds; the full command,
including one native rebuild, takes 35.91 seconds at 1,254,072 KiB peak with
no swap. Two focused strict-cycle separation oracles pass in 0.08 seconds,
with a 0.61-second command peak of 43,840 KiB and no swap.

The focused sanitizer close is also green. ASan/UBSan passes ten
sole-span, shrinking-frontier, self-modification, callback-prefix,
active-peer, code-observation, and ordered-shared-commit tests in 0.22 seconds
with no sanitizer finding. Its isolated build-and-test command takes 67.07
seconds at 2,308,036 KiB peak with no swap. The identical ten tests pass under
TSan in 0.24 seconds with no race report; its command takes 39.28 seconds at
1,837,660 KiB peak with no swap. All ordinary and sanitizer tests ran
foreground and sequentially.

The implementation is committed at
`42c2d1f20cd66c2e573217ee67a3aa6fb40453e5`. The worktree and repository
provenance were clean before the matched after-report was captured.

### Element 3 clean comparison

The after-report uses the exact schema-5 harness and parameters recorded for
the corrected baseline. It was generated at
`2026-07-26T21:15:38.157511+00:00` from clean implementation revision
`42c2d1f20cd66c2e573217ee67a3aa6fb40453e5`.

The 243,549-byte after JSON has SHA-256
`fe69655ef1aaf975b05ab565318e71cdc5a225d289fcfc2e4fbcdf1ba73edfda`.
Its 2,195,768-byte native artifact has SHA-256
`a7ec7abd68cdb13b985503c041a0bcde808827d3fdccdc656e8f900782d1bb20`
and ELF build ID `3ba3c7b629a26f93b460bf0f34f61de84276cfb8`. The complete
after-report command takes 0.63 seconds, peaks at 29,732 KiB, and uses no
swap.

Every before and after report-level, worker-level, host-profile,
timing-hygiene, and cross-lane validation is true. Canonical state, behavior,
and ordered public-accounting hashes match before/after and across all lane
widths:

- canonical state:
  `acefb4be60d898244b47d2a9254fd8a7405593e6c6014843d1ea2f8f1c036e5a`
- behavior oracle:
  `4882f2009791627b60bf1039bb9a53de2c2512931a97fa0b31599f6723e70501`
- ordered public accounting:
  `2bc0689dd18e20a33870e568d5661133f8ea14a22d39971c880e4c5bc4a8d807`

Median unprofiled throughput is:

| Host lanes | Before MIPS | After MIPS | Factor | Change |
|---:|---:|---:|---:|---:|
| 1 | 0.498 | 20.711 | 41.6x | +4,057% |
| 2 | 0.317 | 20.058 | 63.3x | +6,229% |
| 4 | 0.231 | 19.565 | 84.6x | +8,358% |

Every width still performs 500 scheduler rounds and 500,000 per-instruction
private classifications. Logical subfrontiers, worker commands, worker waves,
and checkpoint captures each fall from 500,000 to 500: one bounded
1,000-instruction command per round. The optimization therefore removes
99.9% of the synchronous handoffs and full checkpoints without weakening
per-instruction boundary classification.

This is a narrow single-runnable-microcore result, not a multicore scaling
claim. The higher configured lane counts have idle helpers on this workload;
their much larger factors reflect removal of the old per-instruction pool
protocol penalty. The generated before/after reports remain ignored, while
their source revisions, artifact identities, report hashes, parameters, and
resource measurements are durable above.

Element 3 is complete. Element 4 began from this evidence snapshot.

## Element 4 implementation record

Element 4 adds a host-only private decode/admission cache. It does not add
translated execution, a guest-visible JIT, or a second instruction executor.
The authoritative native fetch, decode, and `step_one` paths remain in place.
The implementation was preserved at
`7fb777c2daeb775ecc3ba64a31fa14ded790d7c8`; its clean benchmark exposed a
full-core regression, so the bounded correction was committed separately at
`11d59b9334cb1a1f992d7d47f9bfda6d5c0f0543`.

Each core owns a fixed 128-entry direct-mapped host table. A valid entry
records that private admission was proved for its instruction address and
exact complete encoding, with a 16-byte identity capacity larger than the
current maximum instruction. A hit must match the address and every recorded
byte before it can authorize private execution. Full cores validate against
the bytes and tags currently resident in that core's guest instruction cache,
preserving intentional stale backing-memory behavior. Microcores validate
through their current mapped-memory observation path.

Privilege, routed-fetch, pending modifier, guest-cache enable, and other live
admission gates remain outside the cached result. Dynamic `EXT.SKIP` is not
cached because flags and skipped-target residency can change independently of
its encoding. Strict-cycle execution uses the direct classifier. A successful
microcore classification carries a single-use proof to the immediately
following `step_one`; the proof checks the same core, micro profile, and
unchanged program counter, and a shared structural predicate prevents any
Python-oracle-owned encoding from producing it.

The cache is per-core, host-only, and outside architectural snapshots,
execution checkpoints, replay, hashes, and public accounting. Reset, complete
guest-cache invalidation, and explicit guest-cache restore clear it. A
completed full-core store clears host entries only if the existing
tag-aware invalidation actually removes a resident matching guest line; a
nonresident or same-index/different-tag store preserves them. Every later hit
still revalidates the current guest-cache tags and complete bytes.

The first implementation used the cache for both frontier admission and every
full-core worker instruction. Exact validation was sound, but a full core then
paid one complete host identity check and performed the ordinary decode
anyway. The correction retains full-core reuse at unbounded coordinator
frontiers and restores the cheaper resident-byte classifier inside full-core
worker spans. Microcores retain per-step reuse because their proof removes the
otherwise duplicated Python-oracle eligibility decode. This selection is
static by core profile and execution contract; it does not depend on lane
count, helper completion, or measured host timing.

Native host-profile schema 3 adds private and frontier cache
lookup/hit/miss counts plus micro proof reuse. The Phase 0 report advances from
schema 10 to 11, the single-active-microcore report from 5 to 6, and the
instruction-cache report from 2 to 3. Architectural state schemas do not
change.

### Element 4 oracle and sanitizer close

The focused three-test Element 4 file covers variable-length ordinary and
prefixed instructions against the Python full- and microcore oracles, full
guest-cache noncoherence and explicit restore, cross-line final-byte mutation,
direct-map tag eviction/refill, disabled-cache behavior, and same-index
different-tag store invalidation. Every case is exact across one, two, and
four configured host lanes. The variable-length micro fixture rejects any
Python fallback.

The final candidate passes 130 affected tests sequentially. The main
108-test selection passes in 1.38 test seconds; its complete command takes
1.90 seconds, peaks at 86,696 KiB, and uses no swap. The 22-test architectural
guest-cache selection passes in 0.08 test seconds; its command takes
0.60 seconds, peaks at 43,716 KiB, and uses no swap. The final focused
same-index/different-tag selection passes in 0.06 test seconds with a
0.55-second command peak of 44,144 KiB and no swap.

The isolated six-test cache/profile ASan/UBSan gate passes in 1.57 test
seconds with no finding; build and execution take 70.01 seconds, peak at
2,311,280 KiB, and use no swap. The same six tests pass under TSan in
2.64 test seconds with no race report; build and execution take 37.92 seconds,
peak at 1,838,804 KiB, and use no swap. The optimized native rebuild takes
32.12 seconds, peaks at 1,254,472 KiB, and uses no swap. All ordinary and
sanitizer tests ran foreground and sequentially.

Two independent read-only audits found no cache-identity, instruction-length,
proof-lifetime, Python-oracle-subset, guest-cache noncoherence, checkpoint,
snapshot, race, or compile blocker. The final correction audit additionally
confirmed that the host table follows all established `CPUState` fields,
cross-line validation checks the exact guest tags and bytes, full reuse is
frontier-only, micro proof reuse remains immediate, and tag-aware store
invalidation is sound.

### Element 4 clean comparison

The full-core comparison uses:

```text
python3 bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios private_compute \
  --instructions 2m \
  --repeats 5 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 512 \
  --host-profile \
  --output build/phase4-e4-final.json
```

The clean baseline at
`e1b8052993e266d85f7bdd725cb9673aa3aaf134` is
`build/phase4-e4-before.json`, report schema 10, 10,500,453 bytes, SHA-256
`82d981abd3ebb17b3a53af6a3eab42e7709e6c36ff9fbba233528e27e2a5454f`.
It was generated at `2026-07-26T21:21:52.907013+00:00`. Its native artifact
is 2,195,768 bytes, SHA-256
`a7ec7abd68cdb13b985503c041a0bcde808827d3fdccdc656e8f900782d1bb20`,
with ELF build ID `3ba3c7b629a26f93b460bf0f34f61de84276cfb8`.

The clean final report at
`11d59b9334cb1a1f992d7d47f9bfda6d5c0f0543` is report schema 11,
10,502,916 bytes, SHA-256
`cb5a8fa8a48652fed40a117f47a824561a24bd90e5bd71c7dd07995424d55b73`.
It was generated at `2026-07-26T22:13:57.734580+00:00`. Its native artifact
is 2,200,000 bytes, SHA-256
`4740c6fdbe45fa33a188428383a6f7fb6873622015bf02fd15483620eceb133c`,
with ELF build ID `03210a2c96b77ff303927d48fb20c7099a640daf`.
The command takes 1.56 seconds, peaks at 91,896 KiB, and uses no swap.

Median unprofiled aggregate throughput is:

| Host lanes | Before MIPS | First implementation MIPS | Final MIPS | Final change |
|---:|---:|---:|---:|---:|
| 1 | 41.228 | 35.058 | 41.078 | -0.4% |
| 2 | 56.925 | 53.379 | 64.916 | +14.0% |
| 4 | 72.575 | 76.079 | 106.723 | +47.1% |

The first clean implementation report is retained as rejected performance
evidence: revision `7fb777c`, schema 11, 10,502,972 bytes, SHA-256
`2680b538d6513112395ae57838c1a577434fc5a773574c50e0be239f1f005783`.
Its artifact is 2,200,208 bytes, SHA-256
`0791c3ab8393e171c0c16168bac060540c61264f46d9c6fd35cf70942357dba0`,
with build ID `3dae68243335806aa68487075b26e63c1f60a700`. The command took
1.97 seconds, peaked at 92,024 KiB, and used no swap. The one- and two-lane
regressions caused the per-worker full-cache design to be corrected rather
than rationalized away.

The final one-lane control is effectively baseline-neutral. The positive
two- and four-lane observations are recorded, but Element 4 does not infer a
general full-core scaling gain from them. Every width retains 500 scheduler
rounds, 501 logical subfrontiers, 2,000 worker commands and checkpoints, and
1,999,996 private steps. Each profiled replay has 2,004 full-frontier cache
lookups, 1,992 hits, and 12 misses; full workers perform zero cache lookups and
retain 1,997,996 direct per-instruction classifications.

The independent single-active-microcore comparison uses:

```text
python3 bench_phase2_microcore.py \
  --instructions 500000 \
  --worker-counts 1,2,4 \
  --repeats 5 \
  --warmups 1 \
  --warmup-instructions 100000 \
  --host-profile \
  --output build/phase4-e4-micro-final.json
```

The clean Element 3 baseline is report schema 5, 243,549 bytes, SHA-256
`fe69655ef1aaf975b05ab565318e71cdc5a225d289fcfc2e4fbcdf1ba73edfda`.
The clean final is report schema 6, 245,563 bytes, SHA-256
`0d4afebe431464e76ab49e45e458af740241b7111ce9874275b2d08dfb6a9dad`;
it was generated at `2026-07-26T22:14:03.672438+00:00` from revision
`11d59b9` and the same final native artifact identified above. The complete
command takes 0.52 seconds, peaks at 30,088 KiB, and uses no swap.

| Host lanes | Before MIPS | First implementation MIPS | Final MIPS | Final change |
|---:|---:|---:|---:|---:|
| 1 | 20.711 | 23.572 | 25.284 | +22.1% |
| 2 | 20.058 | 22.912 | 24.206 | +20.7% |
| 4 | 19.565 | 22.320 | 23.950 | +22.4% |

The first implementation micro report at `7fb777c` is 245,588 bytes with
SHA-256
`d6130af0842eac4377a0d59eb39c1b170c744e6816c851e6e1740e9935f52a1d`.
Its gains were valid, but the implementation was not accepted until the
independent full-core regression was removed. In the final micro profiled
replay, every width records 500,000 private classifications and cache
lookups, 499,998 hits, two misses, 500,000 proof reuses, 500,000 private
steps, and only 500 commands and checkpoints.

All full and micro report-level, timed-sample, profile-reconciliation, and
cross-lane validations are true. Full widths share canonical-state hash
`f8e787ebfcf846f3e3f53f9261ca95bdb77069b1460eda167e41eacd7bce195a`,
behavior hash
`00654569ab49f6b6c22cef69f9924e14f37453b953841ec618727333ce2a6e3b`,
and ordered public-accounting hash
`40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4`.
Micro widths retain the Element 3 canonical, behavior, and public-accounting
hashes recorded above.

The dedicated guest instruction-cache oracle was also refreshed from clean
revision `11d59b9`:

```text
python3 bench_phase2_icache.py \
  --instructions 200000 \
  --repeats 3 \
  --warmups 1 \
  --output build/phase4-e4-icache-final.json
```

The schema-3 report is 64,181 bytes, SHA-256
`ffb96c1b57644492254b49576c2600031f7ab1c5e280f4ede83bb54dd85a32cc`.
All backend, hot/disabled architectural, hook-identity, suspended-load, and
timing-hygiene validations pass. Canonical state remains
`b0bb4266f1fd36a73722cbadad17e2f109eac3f77579b4cc3e44f74de35ccffe`
and benchmark architectural state remains
`ae14e15754764700096c4305e88b433888518040d5dc1aa291cf7a93700502fd`.
Hot and disabled medians are 89.185 and 44.026 MIPS, a diagnostic 1.957x
ratio. The command takes 0.11 seconds, peaks at 32,760 KiB, and uses no swap.

Generated reports remain ignored. Their clean source revisions, native
artifacts, schemas, exact commands, hashes, sizes, validation outcomes, and
resource measurements are durable above. Element 4 is complete. Element 5's
implementation baseline is `11d59b9`; this versioned evidence snapshot is its
orientation record.

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
| P4-D5 | The Phase 3 mixed-topology clamp serializes even a sole logical participant, but independently widening one member of a true mixed frontier can reverse callback order, change exception-visible peer prefixes, or cross a peer-asserted interrupt. A longer command also enlarges the prefix restored by an unexpected internal worker failure. | Permit the existing bounded private command to consume remaining round credit only when the current mixed-topology subfrontier has exactly one executable or coordinator participant. Keep every multi-participant mixed subfrontier at one instruction and retain the unchanged whole-command checkpoint. | The proof is structural and unbounded-only: no other reservation can act in that subfrontier, every instruction is classified before mutation, and all existing own-command boundaries remain. Supported coordinator callback failures retain the completed prefix; only an unexpected internal helper failure restores a longer command instead of one former subfrontier. Reopen multi-participant fusion only with a non-mutating common-span proof covering every participant, callback and interrupt ordering, and exact one/two/four-lane failure evidence. Reopen rollback granularity only if internal-failure partial progress becomes a public contract or gains an injectable oracle. |
| P4-D6 | Exact-byte host admission plans are safest, but a full core that validates an entire identity and then performs the unchanged native decode pays two protocols per instruction. A weaker identity, speculative translated execution, or timing-adaptive selection would either weaken the cache-observation contract or expand Element 4 materially. | Keep complete byte-validated plans. Use them at unbounded full-core frontier admission and on every eligible microcore worker step, where the single-use proof removes a duplicated Python-oracle eligibility decode. Use the established direct classifier inside full-core worker spans and all strict-cycle work. Clear full-core plans on reset, complete invalidation, restore, or an actual resident matching-line invalidation, but not on unrelated stores. | This is an admission cache, not translated execution. Full per-step reuse may be reconsidered only with a genuine decoded executor or a separately proved O(1) guest-cache identity generation covering refill, invalidation, rollback, checkpoint restore, explicit cache restore, and noncoherent host mutation. Runtime host timing, lane width, and helper readiness must never select the path. |

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
