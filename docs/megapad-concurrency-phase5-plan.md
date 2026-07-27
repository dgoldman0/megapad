# Megapad deterministic concurrency: Phase 5 rollout plan

**Started:** 2026-07-27

**Status:** Element 1 of 4 complete; Element 2 not started

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
| 2 | Production entry-point integration | Pending |
| 3 | Production scheduler consolidation | Pending |
| 4 | Bounded closure and handoff | Pending |

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
   one/two/four capacity ceiling; and
3. the resolved width is the lower of the guest target and host ceiling.

An explicit one, two, or four always wins and is never silently clamped.
This lets constrained hosts request the reference path and lets a caller
deliberately oversubscribe for testing without hidden policy changes.

### P5-D1: automatic selection versus a fixed global default

| Contention | Decision | Claim boundary |
|---|---|---|
| A global four-lane default would create idle helpers for a one-core machine, while a permanent one-lane default would leave production concurrency opt-in. Host-load-adaptive resizing would make resource use unstable and complicate diagnosis. | Resolve once from configured guest concurrency and affinity-aware host capacity on the fixed 1/2/4 ladder. Preserve explicit widths exactly. | The host may choose a different width on a differently constrained process, but prior one/two/four equivalence proves guest results do not change. Selection never consults runtime timing, helper readiness, or guest state, and never changes after construction. |

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

Element 2 will expose `auto`, one, two, and four lanes through:

- the main CLI;
- `MachineSession.from_bios`;
- the shared-session server;
- development scenario JSON; and
- machine reconstruction such as interactive RAM resizing.

The public spelling will be `lanes`; no duplicate `workers` alias or
environment-variable configuration will be added. Every entry point will
report or retain the resolved width rather than silently reverting to auto.

## Element 3: production scheduler consolidation

The native generalized scheduler already owns every ordinary positive batch,
including full and reduced cores. The remaining Python per-core batch loop is
reachable only when tests monkeypatch core batch methods. Element 3 will:

- make every positive ordinary batch use the native scheduler;
- remove the method-identity gate and superseded Python chunk loop;
- remove serial convenience runners with no production callers;
- retain exact interactive `step()`, the `run_batch()` return-shape adapter,
  Python continuation/error/round settlement, and the real full-core-only
  strict-cycle boundary; and
- replace compatibility-preservation tests with fail-closed production
  ownership tests where coverage remains useful.

## Element 4: bounded closure

Closure uses only bounded MegaPad-native tests and a modest rollout
performance confirmation. It does not include Akashic, unrestricted system
tests, large persistence tests, broad sanitizer selections, or a repeat of
the Phase 3/4 high-memory gates.

The final handoff will record:

- exact focused commands and resource use;
- default, explicit-one, and cross-width behavior;
- entry-point propagation and scheduler-path consolidation;
- the resolved lane policy and diagnostic escape hatch;
- remaining architectural limitations inherited from Phase 4; and
- the clean completion revision and local merge/push boundary.

All test commands remain foreground, sequential, and resource-monitored.
Tests that instantiate helper workers require approval before execution.
