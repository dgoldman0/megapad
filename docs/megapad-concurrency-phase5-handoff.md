# Megapad deterministic-concurrency handoff: Phase 5 snapshot

**Snapshot date:** 2026-07-27

**Status:** Phase 5 complete within the rollout and evidence boundaries below

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Phase 4 base:** `7aebccf395a430b9af9ee1a5e34d5b91d47ee8e3`

**Implementation revision before closure:** `7f90d2c1364a4d5ebca13b026b33c3ae860d04bb`

**Completion revision:** the commit containing this handoff

## Purpose and custody

Phase 5 makes the deterministic one/two/four-lane scheduler the normal
production execution path. It resolves a fixed lane width at machine
construction, exposes that choice through the supported entry points, removes
the superseded Python batch scheduler, and closes with bounded MegaPad-native
evidence.

The normative Phase 5 decision ledger is
`docs/megapad-concurrency-phase5-plan.md`. The Phase 3 and Phase 4 handoffs
remain the frozen architecture and optimization history; Phase 5 does not
rewrite their decisions.

All work is local Git work. No push is part of this snapshot. At closure,
local `main` remains at the Phase 4 base and this feature branch contains the
four Phase 5 element commits. Recover the exact completion commit with:

```text
git log -1 -- docs/megapad-concurrency-phase5-handoff.md
```

## Fixed four-element completion

| Element | Commit | Outcome |
|---|---|---|
| 1 — rollout policy | `23cb876` | Added immutable affinity-aware automatic selection on the fixed one/two/four-lane ladder while preserving explicit widths and the low-level one-lane default. |
| 2 — production entry points | `0fc60be` | Added `lanes` to CLI, shared sessions, `MachineSession`, and development scenarios; preserved the resolved width across RAM reconstruction. |
| 3 — scheduler consolidation | `7f90d2c` | Routed every positive production batch through the native system scheduler and removed the method-identity gate, Python per-core scheduler, and unused serial facade runners. |
| 4 — bounded closure | containing commit | Added the production-default differential, final evidence, and this handoff. |

No elements or sub-elements were added during execution.

## Production lane policy

The public request is `auto`, `1`, `2`, or `4` lanes. Internally, `auto` is
represented by `worker_count=None` and resolved exactly once:

1. one advertised guest execution core targets one lane, two target two
   lanes, and three or more target four lanes;
2. process CPU affinity, falling back to host CPU count, selects one lane for
   one CPU, two lanes for two CPUs, and four lanes for three or more CPUs; and
3. the lower of the guest and host selections becomes the immutable machine
   width.

An explicit one, two, or four is never silently clamped. `--lanes 1` is the
diagnostic escape hatch and helper-free architectural reference. The
low-level `NativeSystemState` also retains one lane as its default.
Because the supported set has no three-lane width, exactly three available
host CPUs select four lanes and may oversubscribe by one. An explicit one or
two avoids that when desired.

The resolved value is available as `MegapadSystem.worker_count`. Native worker
pool diagnostics expose the fixed width, auxiliary worker count, liveness,
launch count, and inline-reference status. Physical lane identity and timing
remain outside guest-visible state and scheduling decisions.

## Production integration and ownership

- Main CLI: `--lanes {auto,1,2,4}`.
- Shared-session server: the same option, with the resolved width reported.
- `MachineSession.from_bios`: `lanes=None|1|2|4`.
- Development scenarios: `machine.lanes`, with the resolved value included in
  the report.
- Interactive RAM resizing: reconstructs the machine with its already
  resolved width.

There is one production ordinary-batch scheduler. Every positive
`MegapadSystem.run_batch()` or `run_batch_stats()` call enters the generalized
native system scheduler for full and reduced cores. Per-core method
monkeypatches cannot select a second scheduler.

Interactive `step()` remains available for debugger, display, and console
flows. The native batch adapter and coordinator callbacks remain responsible
for Python-only instruction continuations, traps, resets, callback errors,
shared-device clocking, UART drainage, interrupt delivery, and final result
shaping. Strict-cycle execution remains a separate full-core-only contract.

## Final bounded evidence

Every command ran in the foreground and sequentially. No pytest-xdist worker,
parallel test agent, or concurrent test terminal was used. Test counts overlap
and must not be added together.

### Rollout-policy file

```text
/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p5e4-rollout \
  make test-sequential TEST_PATH='tests/test_phase5_rollout.py'
```

Result: 17 passed in 0.05 test seconds; 0.61 seconds foreground,
43,500 KiB peak RSS, and no process swap.

This includes the integration differential that forces an auto-capable host
policy, resolves a four-core production machine to four lanes, and compares
its reported batch accounting and fixture-relevant per-core state with
explicit one-lane execution.

### Final production/oracle selection

```text
/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p5e4-closure \
  make test-sequential \
  TEST_PATH='tests/test_phase5_rollout.py tests/test_session.py tests/test_phase3_worker_pool.py tests/test_phase3_coordinator_execution.py tests/test_phase3_event_execution.py tests/test_native_microcore.py tests/test_phase4_device_clocking.py tests/test_native_batch_boundaries.py' \
  K='production_default_matches_explicit_one_lane_reference or cli_ramsize_preserves_machine_configuration or cli_propagates_explicit_execution_lanes or session_server_propagates_memory_and_lane_policy or json_scenario_runner or fixed_worker_modes_preserve_the_one_worker_reference_result or complete_logical_frontier_is_lane_width_independent or strict_event_dma_replay_is_one_two_four_lane_and_slice_exact or all_advertised_cores_share_the_native_scheduler_budget or system_native_clock_avoids_proxy_double_ticks_and_keeps_extensions or system_batch_ignores_instance_core_batch_replacement or system_batch_ignores_class_level_core_batch_replacements or native_system_loop_settles_complete_frontier_before_callback_error'
```

Result: 14 passed and 132 deselected in 0.37 test seconds; 0.87 seconds
foreground, 195,612 KiB peak RSS, and no process swap.

This selection covers all production entry points, explicit-width
reconstruction, an actual four-lane BIOS scenario, production default versus
the one-lane reference, private and shared-frontier one/two/four equivalence,
strict event/DMA replay, reduced-core scheduling, system-clock ownership, and
native scheduler ownership across method replacement and callback failure.

### Modest performance confirmation

```text
/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p5e4-bench \
  python3 bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios private_compute \
  --instructions 500k \
  --repeats 3 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 512
```

| Lanes | Median aggregate MIPS | Relative to one lane |
|---:|---:|---:|
| 1 | 42.57 | 1.00x |
| 2 | 65.31 | 1.53x |
| 4 | 107.38 | 2.52x |

Every report-level architectural validation passed. The strict NIC/disk
side probe remained deterministic at every width and reported 2.011 virtual
cycles per payload byte. The command took 0.96 seconds, peaked at
69,020 KiB RSS, and used no process swap. No generated JSON was retained.

This is a short rollout confirmation, not a replacement for the longer
Phase 4 baseline and not a universal speed claim.

## Preserved architecture and contention custody

- Hard QoS determines which requests must or may be served and their reserved
  entitlement. Simultaneously eligible peers retain equal round-robin order,
  and unused reservation remains work-conserving. No secondary host bias was
  added.
- Helpers execute only proven-private work. Shared effects, callbacks,
  arbitration, global cyclic commit order, and final stop selection remain
  coordinator-owned.
- Complete logical frontiers are gathered before shared settlement.
- P3-D1/P3-D9 callback-failure behavior remains honest: gathered peer-private
  progress and successful earlier ordered commits are retained. Phase 5 does
  not claim transactional rollback.
- `CLUSTER_EN` still resets to all ones.
- The selected lane width is immutable for the machine lifetime and never
  adapts to host timing or guest behavior.

## Deliberate exclusions and remaining limitations

- Shared, MMIO, timer, and legacy frontiers remain coordinator-bound and do
  not generally scale positively with lane count.
- Reduced-core strict timing remains unsupported.
- The host admission cache remains an exact-byte validated cache, not a JIT
  or translated executor.
- External replay remains scoped host-ingress/release-boundary replay rather
  than a whole-machine transcript.
- Complete machine snapshots remain unavailable for the authoritative native
  timeline because the current format omits required shared state.
- Phase 5 did not rerun sanitizer builds, unrestricted `tests/test_system.py`,
  large persistence/framebuffer tests, FPGA flows, or Akashic SR2. The
  earlier one-core SR2 evidence is historical and is not relabeled as a
  Phase 5 result.
- The closure performance result is specific to the four-core private-compute
  fixture. It does not establish bulk-DMA or whole-system application
  throughput.

Historical high-memory gates and large downstream instruction budgets retain
their prior approval requirements. Future tests and benchmarks must remain
sequential.

## Resume and transfer boundary

Phase 5 is ready for deliberate local integration. The isolated feature
branch is the authoritative completion snapshot; local `main` is intentionally
unchanged, and nothing has been pushed. Before any later transfer, verify the
branch tip and worktree cleanliness, then choose whether to fast-forward
`main`.
