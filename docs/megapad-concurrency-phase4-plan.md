# Megapad deterministic concurrency: Phase 4 plan

**Started:** 2026-07-26

**Status:** Element 1 of 6 complete; Element 2 not started

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
| 2 | Scheduler/frontier fast path | Pending |
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

## Design-contention ledger

This ledger contains only decisions already required by Phase 4 work. New
contentions are added when an implementation choice actually arises, not
speculatively.

| ID | Contention | Phase 4 decision | Claim boundary and revisit trigger |
|---|---|---|---|
| P4-D1 | Profiling can itself perturb host execution, and exposing its state through architectural serialization or scheduling would make diagnostics guest-visible. | Instrumentation is host-only, opt-in, disabled for timed samples, and excluded from architectural state, snapshots, canonical hashes, replay, public accounting, virtual time, stop selection, and scheduling decisions. A separate profiled replay supplies attribution evidence. | Profile timings are diagnostic host observations, not architecture or a causal proof. Reopen the counter set or measurement method when it cannot distinguish a demonstrated cost, but never silently broaden profiling into guest-visible state. |
| P4-D2 | A single universal speedup threshold would reward only favorable workloads and could conceal regressions or equivalence failures elsewhere. | Require exact architectural equivalence, then judge benefit with workload-specific before/after measurements. No universal performance threshold is imposed. | A change may be retained only with an honestly stated, reproducible benefit and disclosed tradeoffs across affected workloads. Exact equivalence is mandatory regardless of speed; changing an architectural oracle requires a separately approved architecture decision, not a performance exemption. |

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
