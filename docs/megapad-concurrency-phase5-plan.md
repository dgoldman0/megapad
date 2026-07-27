# Megapad deterministic concurrency: Phase 5 rollout plan

**Started:** 2026-07-27

**Status:** Complete — all four fixed elements are implemented and verified

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Phase 4 snapshot:** `7aebccf395a430b9af9ee1a5e34d5b91d47ee8e3`

## Purpose

Phase 5 turns the deterministic scheduler completed in Phases 3 and 4 into
the normal production execution path. It does not reopen the architecture,
add a new scheduler, or require downstream Akashic validation. The rollout
selects a fixed host lane width at machine construction, exposes that choice
through production entry points, deletes the superseded Python batch
scheduler, and closes with bounded MegaPad-native evidence.

This project is unreleased. Obsolete phase bridges are removed once their
production replacement is proved; they are not retained as legacy APIs.

## Fixed phase structure

Phase 5 contains exactly four elements. Discoveries can require a clearly
reported corrective commit at the current milestone, but do not create new
elements or sub-elements.

| Element | Scope | Status |
|---|---|---|
| 1 | Rollout policy | Complete |
| 2 | Production entry-point integration | Complete |
| 3 | Production scheduler consolidation | Complete |
| 4 | Bounded closure and handoff | Complete |

## Preserved architecture

- Supported resolved host lane widths remain exactly one, two, and four.
  Lane zero is the inline coordinator; one lane remains the helper-free
  reference.
- Helpers execute only proven-private work. Shared effects, callbacks,
  arbitration, global cyclic commit order, and final stop selection remain
  coordinator-owned.
- Complete logical frontiers are gathered before shared settlement.
  Physical lane identity, completion order, and host timing remain excluded
  from guest state, snapshots, hashes, replay, virtual time, accounting, and
  scheduling decisions.
- Hard QoS continues to determine must/may eligibility and reserved
  entitlement. Simultaneously eligible peers retain equal round-robin order,
  and unused reservation remains work-conserving.
- The selected lane width is immutable for a machine lifetime. There is no
  timing-adaptive resizing or migration.
- Unbounded and strict-cycle contracts remain distinct. Rollout does not add
  reduced-core strict timing or broaden strict helper eligibility.

## Element 1: rollout policy

The production `MegapadSystem` facade uses `worker_count=None` to request
automatic lane selection. The low-level native `SystemState` retains its
one-lane default so direct construction remains an explicit reference seam.

Automatic selection is resolved once at construction:

1. configured guest execution cores select one lane for one core, two lanes
   for two cores, and four lanes for three or more cores;
2. process CPU affinity, falling back to the host CPU count, selects the same
   tier: one CPU maps to one lane, two map to two lanes, and three or more map
   to four lanes; and
3. the resolved width is the lower of the guest and host tier selections.

An explicit one, two, or four always wins and is never silently clamped.
This lets constrained hosts request the reference path and lets a caller
deliberately oversubscribe for testing without hidden policy changes.
Because there is no supported three-lane width, an affinity mask containing
exactly three CPUs selects four lanes and can oversubscribe by one; callers
that want to avoid that can explicitly select one or two.

### P5-D1: automatic selection versus a fixed global default

| Contention | Decision | Claim boundary |
|---|---|---|
| A global four-lane default would create idle helpers for a one-core machine, while a permanent one-lane default would leave production concurrency opt-in. Host-load-adaptive resizing would make resource use unstable and complicate diagnosis. | Resolve once from configured guest concurrency and the affinity-aware host tier on the fixed 1/2/4 ladder. Preserve explicit widths exactly. | The host may choose a different width on a differently constrained process, and the three-CPU tier may oversubscribe by one, but prior one/two/four equivalence proves guest results do not change. Selection never consults runtime timing, helper readiness, or guest state, and never changes after construction. |

Element 1 completes when pure policy tests, facade/native default tests, fixed
override tests, and worker lifecycle tests pass sequentially and the decision
is committed.

### Element 1 completion record

The complete Phase 5 policy file and the retained Phase 3 worker-pool
contracts pass 33 tests in 0.11 test seconds. The foreground command takes
0.58 seconds, peaks at 43,632 KiB RSS, and reports no process swap. It covers
the pure selection table, affinity precedence, automatic topology widths,
explicit overrides, the immutable facade property, the low-level one-lane
default, invalid values, helper lifecycle, teardown, rapid reposting, and
one/two/four-lane equivalence.

## Element 2: production entry-point integration

Element 2 exposes `auto`, one, two, and four lanes through:

- the main CLI;
- `MachineSession.from_bios`;
- the shared-session server;
- development scenario JSON; and
- machine reconstruction such as interactive RAM resizing.

The public spelling is `lanes`; no duplicate `workers` alias or
environment-variable configuration was added. Every entry point reports or
retains the resolved width rather than silently reverting to auto.

### P5-D2: one public spelling and reconstruction custody

| Contention | Decision | Claim boundary |
|---|---|---|
| The implementation calls these objects workers internally, while users configure host execution lanes. Adding both names or an environment override would create multiple precedence paths, and machine reconstruction could silently discard an explicit width. | Use `--lanes` and scenario `machine.lanes`; use `lanes` in `MachineSession.from_bios`; translate once to the existing internal `worker_count`. RAM resize preserves the resolved width exactly. | `auto` is resolved only by `MegapadSystem`. Entry points accept only `auto`, 1, 2, or 4, and do not consult environment variables. Reconfiguration requires constructing a new machine. |

### Element 2 completion record

The main CLI now exposes `--lanes`; shared sessions expose and report the
same setting; `MachineSession` carries the request; development scenarios
accept and record `machine.lanes`; and interactive RAM resizing preserves the
resolved width. The focused five-test entry-point selection passes in
0.34 test seconds. The foreground command takes 0.85 seconds, peaks at
324,560 KiB RSS, and reports no process swap. The scenario control uses an
explicit four-lane session, so the result covers actual helper creation rather
than only mocked argument propagation.

## Element 3: production scheduler consolidation

The native generalized scheduler now owns every ordinary positive batch,
including full and reduced cores. The former Python per-core batch loop was
reachable only when tests monkeypatched core batch methods. Element 3:

- makes every positive ordinary batch use the native scheduler;
- removes the method-identity gate and superseded Python chunk loop;
- removes serial convenience runners with no production callers;
- retains exact interactive `step()`, the `run_batch()` return-shape adapter,
  Python continuation/error/round settlement, and the real full-core-only
  strict-cycle boundary; and
- replaces compatibility-preservation tests with fail-closed production
  ownership tests where coverage remains useful.

### P5-D3: one production scheduler versus method-override compatibility

| Contention | Decision | Claim boundary |
|---|---|---|
| Preserving per-core batch monkeypatches kept a second Python scheduler alive and allowed test-only method identity to change production execution. Retaining serial facade runners would also preserve an unused competing loop. | Every positive `MegapadSystem` batch enters the native system scheduler. Remove the method-identity gates, Python per-core scheduler, `MegapadSystem.run()`, and `run_until_halt()`. | Interactive `step()` remains a separate debugger/display operation. Standalone core APIs remain available. Native coordinator callbacks still settle Python-only instructions, traps, resets, shared effects, and errors. Strict-cycle execution remains restricted to full-core-only topologies. |

### Element 3 completion record

The complete batch-boundary and native bus-transaction files pass 38 tests
in 0.12 test seconds. The foreground command takes 0.62 seconds, peaks at
43,672 KiB RSS, and reports no process swap. A second five-test topology and
ownership selection passes in 0.25 test seconds; its command takes 0.72
seconds, peaks at 68,452 KiB RSS, and reports no process swap. Together these
cover native ownership despite instance/class method replacement, continuation
and error settlement, active-grant rejection, exact strict-cycle topology
rejection, full/reduced-core budget ownership, re-entry rejection, and
one/two/four-lane frontier equivalence.

## Element 4: bounded closure

Closure uses only bounded MegaPad-native tests and a modest rollout
performance confirmation. It does not include Akashic, unrestricted system
tests, large persistence tests, broad sanitizer selections, or a repeat of
the Phase 3/4 high-memory gates.

The final handoff records:

- exact focused commands and resource use;
- default, explicit-one, and cross-width behavior;
- entry-point propagation and scheduler-path consolidation;
- the resolved lane policy and diagnostic escape hatch;
- remaining architectural limitations inherited from Phase 4; and
- the clean completion revision and local merge/push boundary.

All test commands remain foreground, sequential, and resource-monitored.
Tests that instantiate helper workers require approval before execution.

### P5-D4: bounded rollout closure versus repeated broad qualification

| Contention | Decision | Claim boundary |
|---|---|---|
| Repeating the Phase 3/4 comprehensive, sanitizer, and downstream gates would consume substantially more memory, disk, and time than the Python-level rollout changes justify. Omitting performance evidence entirely could still hide accidental reference-only production behavior. | Close with focused production-entry, scheduler-ownership, cross-width, strict-event/DMA, reduced-core, and clock oracles plus a modest four-core private-compute control. | Phase 5 makes no new sanitizer, unrestricted-suite, Akashic/SR2, persistence, framebuffer-stress, bulk-DMA, or universal-performance claim. Earlier evidence remains historical rather than being relabeled as Phase 5 evidence. |

### Element 4 completion record

The complete Phase 5 rollout-policy file passes 17 tests in 0.05 test seconds.
Its foreground command takes 0.61 seconds, peaks at 43,500 KiB RSS, and
reports no process swap. The final 14-case integration/oracle selection passes
in 0.37 test seconds; its command takes 0.87 seconds, peaks at 195,612 KiB
RSS, and reports no process swap.

The modest four-core private-compute control uses 500,000 instructions, three
timed repeats, one 100,000-instruction warmup per lane-width case,
one/two/four lanes, and a 512-byte strict-DMA side probe. It passes every
architectural validation and reports median aggregate throughput of 42.57,
65.31, and 107.38 MIPS. Four lanes are 2.52 times the one-lane reference for
this fixture. The command takes 0.96 seconds, peaks at 69,020 KiB RSS, and
reports no process swap. These short medians confirm rollout rather than
establishing a new universal performance baseline.

The durable completion and resumption record is
`docs/megapad-concurrency-phase5-handoff.md`.
