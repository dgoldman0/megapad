# Megapad deterministic concurrency: Phase 4 plan

**Started:** 2026-07-26

**Status:** Element 1 of 6 in progress

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
| 1 | Measurement and attribution | In progress |
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
