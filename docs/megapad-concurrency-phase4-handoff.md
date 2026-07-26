# Megapad deterministic-concurrency handoff: Phase 4 snapshot

**Snapshot date:** 2026-07-26

**Status:** Phase 4 complete within the architecture and evidence boundaries
below

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Implementation revision:** `cfdf13a4f21a2f6f400132cc0b6e9776daf3eed1`

**Final clean evidence revision:** `ec7f37f63954cc45266251ea5d7b31172e582df8`

**Phase 3 snapshot:** `e0e75e844d8dce113670a642f59dde3d19999ba2`

## Purpose and custody

This is the versioned completion and resumption point for Phase 4 of the
deterministic-concurrency project. Phase 4 measured the Phase 3 architecture,
removed demonstrated host costs through narrow structural proofs, and closed
with clean one/two/four-lane, sanitizer, benchmark, and contention evidence.

The Phase 3 handoff remains a frozen historical snapshot. Its P3-D1 through
P3-D19 decisions remain normative. The complete Phase 4 implementation and
decision record is
`docs/megapad-concurrency-phase4-plan.md`; its P4-D1 through P4-D8 ledger is
also normative.

All work remains local Git work. Nothing in this branch was pushed as part of
Phase 4. The completion snapshot is the commit containing this file and can be
recovered with:

```text
git log -1 -- docs/megapad-concurrency-phase4-handoff.md
```

Generated benchmark JSON is intentionally ignored. Exact commands, source and
artifact identities, report sizes and hashes, results, and resource use are
recorded below so the evidence can be reproduced without committing roughly
41 MiB of generated state per comprehensive report.

## Completion boundary

Phase 4 now provides:

- versioned host-only attribution for batches, scheduler rounds, logical
  subfrontiers, worker waves, private execution, coordinator boundaries,
  checkpoints, decode-cache activity, and Python callback time;
- zero-progress full-core frontier bypass and deferred checkpoint capture
  without changing complete-frontier gathering or failure visibility;
- longer bounded private execution for the sole runnable participant in a
  mixed topology, while every multi-participant frontier remains conservative;
- a fixed-size, byte-validated host admission cache whose full-core identity
  follows the architecturally visible guest I-cache and whose microcore proof
  is single use;
- registration-ordered device clock dispatch that omits inherited no-op hooks
  and native-clock-owned proxies;
- a narrow complete-full-core ordinary-pass GIL scope that retains fresh
  per-core mapping and execution guards and restores the GIL for Python
  settlement; and
- final resource-bounded ordinary, ASan/UBSan, TSan, full-workload,
  microcore, and instruction-cache evidence.

Phase completion does not mean that the admission cache is a JIT, that shared
or MMIO workloads scale positively with lane count, that strict DMA has a bulk
transfer path, or that callback failure is transactional. Those boundaries
remain explicit below.

## Frozen architecture

- Supported host lane counts remain exactly one, two, and four. Lane zero is
  the coordinator, and one lane is the thread-free reference behavior.
- Helpers execute only work proven private. Shared effects, callbacks,
  arbitration, externally visible commit order, and final stop selection
  remain coordinator-owned.
- A complete logical frontier is gathered across all required physical
  cohorts before shared settlement. Physical lane identity, helper completion
  timing, and cohort count remain architecturally invisible.
- Hard QoS determines which requests must or may be served and their reserved
  entitlement. Simultaneously eligible peers retain equal round-robin order.
  Unused reserved capacity remains work-conserving. No secondary weights,
  aging, or host-timing biases were added.
- Unbounded and strict-cycle execution retain distinct contracts. Strict
  helper work remains full-core-only, cache-resident, callback-free, and
  exactly one cycle.
- Ordered callback failure retains the complete gathered private frontier and
  all successful earlier ordered commits. Phase 4 does not claim speculative
  whole-frontier rollback.
- Full-core guest I-cache noncoherence remains intentional. Host admission
  reuse validates against the exact bytes and tags architecturally visible
  through that guest cache, not fresher backing memory.
- Host profiles, cache entries, proofs, lane diagnostics, and timing data are
  excluded from snapshots, canonical hashes, replay, public accounting,
  virtual time, scheduling choices, and stop selection.
- External replay remains scoped host-ingress and release-boundary replay, not
  a complete machine transcript.
- `CLUSTER_EN` still resets to all ones. Hard-QoS hardware defaults still use
  equal weights and unlimited zero bandwidth limits.

## Phase 4 commit chain

The 15 Phase 4 commits before this completion handoff are:

| Element | Commit | Outcome |
|---|---|---|
| 1 | `1a211f8407d9c016aeb1862e4648ce85edbebe43` | Added opt-in, host-only concurrency attribution and report schema support. |
| 1 | `c5553d46e643ed60d21e06fb67d2bbfda15b6000` | Versioned the clean attribution baseline and first optimization priorities. |
| 2 | `e4918e99c314ac1984885e1360a4bc3c75769ba8` | Added zero-progress frontier bypass and deferred first-mutation checkpoints. |
| 2 | `e735f8fd6b9ee9089941526aea0fd1ad573cb86a` | Versioned scheduler/frontier performance and equivalence evidence. |
| 3 | `ba778f5dde7bef030ec4ba611c643b53f1d1d825` | Added the sole-active-microcore comparison harness. |
| 3 | `0cb0f25764635b256e230fa9a9eac4d8b7b54c1c` | Removed host-timed instability from that harness. |
| 3 | `42c2d1f20cd66c2e573217ee67a3aa6fb40453e5` | Lengthened private execution only for a sole mixed-topology participant. |
| 3 | `e1b8052993e266d85f7bdd725cb9673aa3aaf134` | Versioned the private-span performance evidence. |
| 4 | `7fb777c2daeb775ecc3ba64a31fa14ded790d7c8` | Added exact byte-validated private decode admission caching. |
| 4 | `11d59b9334cb1a1f992d7d47f9bfda6d5c0f0543` | Removed the first design's full-core per-step validation regression. |
| 4 | `cc01f211f2ca382c2641f841f256f47dc51d9789` | Versioned accepted, rejected, microcore, and I-cache evidence. |
| 5 | `045e95b967937f2057786074ab9fe01b62aea808` | Avoided inherited no-op and native-owned Python device clock callbacks. |
| 5 | `97c466691935954e46dda376a30dbb08c071f413` | Coalesced GIL transitions across eligible ordinary cyclic passes. |
| 5 | `cfdf13a4f21a2f6f400132cc0b6e9776daf3eed1` | Bounded a test-local UART geometry producer that had reached roughly 14.3 GiB RSS. |
| 5 | `ec7f37f63954cc45266251ea5d7b31172e582df8` | Versioned Element 5 decisions and clean evidence. |

The Element 6 completion commit is the commit containing this file. Every
implementation and evidence commit has a detailed multi-paragraph message.

## Final regression and sanitizer evidence

Every command below ran in the foreground and sequentially. No pytest-xdist
worker ran. Counts overlap and are not additive.

The Phase 4-owned matrix was:

```text
/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p4e6-focused \
  make test-sequential \
  TEST_PATH="tests/test_phase4_host_profile.py \
tests/test_phase4_private_spans.py \
tests/test_phase4_decode_cache.py \
tests/test_phase4_device_clocking.py \
tests/test_phase4_coordinator_scope.py"
```

It passed 24 tests in 0.56 test seconds. The complete command took
1.14 seconds, peaked at 75,008 KiB, and reported no process swap.

The final Phase 3 contract regression used:

```text
P4E6_ORDINARY_PATHS="tests/test_phase3_benchmark.py \
tests/test_phase3_worker_pool.py \
tests/test_phase3_private_execution.py \
tests/test_phase3_coordinator_execution.py \
tests/test_phase3_reduced_core_execution.py \
tests/test_phase3_event_execution.py \
tests/test_native_batch_boundaries.py \
tests/test_concurrency_handoff.py \
tests/test_accel_buffer_binding.py"

P4E6_ORDINARY_K="test_phase3_benchmark_compares_one_two_and_four_lanes_exactly \
or test_fixed_worker_modes_preserve_the_one_worker_reference_result \
or test_helper_mailboxes_survive_rapid_back_to_back_reposts \
or test_partial_helper_sets_survive_alternating_reposts \
or test_complete_logical_frontier_is_lane_width_independent \
or test_shared_boundaries_do_not_create_a_secondary_qos_weight \
or test_unused_early_credit_flows_forward_in_the_same_round \
or test_callback_failure_preserves_the_complete_private_frontier \
or test_immediate_boundary_bypass_retains_every_peer_private_prefix \
or test_exact_cycle_ceiling_does_not_mask_callback_failure \
or test_shared_ram_commits_in_frozen_cyclic_frontier_order \
or test_hot_private_trap_reset_prefix_is_settled_once \
or test_repeated_cluster_contention_retains_equal_round_credit \
or test_hard_ineligible_request_releases_credit_forward \
or test_mixed_shared_commits_are_coordinator_only_and_stable \
or test_zero_retirement_settlement_cannot_publish_a_cluster_grant \
or test_reduced_callback_failure_preserves_prefixes_without_a_grant \
or test_strict_event_dma_replay_is_one_two_four_lane_and_slice_exact \
or test_live_staged_ingress_replays_at_the_same_post_batch_boundary \
or test_between_batch_live_ingress_replays_before_the_next_batch \
or test_cycle_and_event_limits_win_ties_with_instruction_cap_across_lanes \
or test_native_system_loop_settles_complete_frontier_before_callback_error \
or test_uart_geometry_host_updates_progress_during_native_execution \
or test_buffer_acquisition_callback_can_join_execution_without_lock_cycle \
or test_buffer_release_callback_can_join_execution_without_lock_cycle"

/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p4e6-ordinary \
  make test-sequential \
  TEST_PATH="$P4E6_ORDINARY_PATHS" \
  K="$P4E6_ORDINARY_K"
```

It passed 40 selected tests with 222 deselected in 0.94 test seconds. The
complete command took 1.50 seconds, peaked at 69,988 KiB, and reported no
process swap. The selection covers worker lifetime and reposting, complete
frontiers, equal QoS and work conservation, callback and terminal prefixes,
cyclic shared settlement, mixed and cluster arbitration, strict timing and
DMA, event/replay ordering, live-ingress liveness, and mapping callback joins.

The sanitizer selection used:

```text
P4E6_SAN_PATHS="tests/test_phase4_host_profile.py \
tests/test_phase4_private_spans.py \
tests/test_phase4_decode_cache.py \
tests/test_phase4_device_clocking.py \
tests/test_phase4_coordinator_scope.py \
tests/test_phase3_private_execution.py \
tests/test_phase3_coordinator_execution.py \
tests/test_phase3_reduced_core_execution.py \
tests/test_phase3_event_execution.py \
tests/test_accel_buffer_binding.py"

P4E6_SAN_K="test_phase4_host_profile_is_opt_in_and_reconciles_accounting \
or test_partial_helper_sets_survive_alternating_reposts \
or test_immediate_boundary_bypass_retains_every_peer_private_prefix \
or test_callback_failure_preserves_the_complete_private_frontier \
or test_reduced_callback_failure_preserves_prefixes_without_a_grant \
or test_strict_event_dma_replay_is_one_two_four_lane_and_slice_exact \
or test_sole_mixed_topology_participant_uses_long_private_spans \
or test_full_decode_identity_follows_guest_icache_and_restore \
or test_complete_native_mmio_pass_is_lane_width_independent \
or test_python_mmio_can_join_live_ingress_during_coalesced_pass \
or test_system_native_clock_avoids_proxy_double_ticks_and_keeps_extensions \
or test_buffer_acquisition_callback_can_join_execution_without_lock_cycle \
or test_buffer_release_callback_can_join_execution_without_lock_cycle"

/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p4e6-asan \
  make test-sanitize \
  SANITIZER=address-undefined \
  SANITIZE_TEST_PATHS="$P4E6_SAN_PATHS" \
  K="$P4E6_SAN_K"

/usr/bin/time -v env MP64_RUNTIME_NAMESPACE=p4e6-tsan \
  make test-sanitize \
  SANITIZER=thread \
  SANITIZE_TEST_PATHS="$P4E6_SAN_PATHS" \
  K="$P4E6_SAN_K"
```

| Gate | Result | Complete time | Peak RSS | Interpretation |
|---|---:|---:|---:|---|
| ASan/UBSan | 20 passed, 191 deselected | 77.01 s | 2,314,656 KiB | No address or undefined-behavior finding; leak detection remains disabled |
| TSan | 20 passed, 191 deselected | 39.01 s | 1,841,288 KiB | No race report |

Both isolated builds imported only their sanitizer-specific extension, used no
process swap, and left the optimized in-tree extension intact.

## Final comprehensive evidence

The final all-workload command was:

```text
/usr/bin/time -v python3 bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --scenarios all \
  --instructions 2m \
  --repeats 3 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 1024 \
  --host-profile \
  --output build/phase4-concurrency-final.json
```

Provenance:

| Field | Value |
|---|---|
| Repository revision | `ec7f37f63954cc45266251ea5d7b31172e582df8` |
| Repository dirty flag | `false` |
| Report schema | `megapad.phase0-concurrency-baseline`, version 11 |
| Generated | `2026-07-26T23:36:14.980401+00:00` |
| JSON size | 43,178,477 bytes |
| JSON SHA-256 | `5108677cd5d01d6f984ef48cad110e71606cce878df22c2029ba3eb45cb481a5` |
| Fixture-manifest SHA-256 | `b3867065c27ffd638315552453de988003d7b9e77b2f5801262fd4ce87c6436f` |
| Native artifact | `_mp64_accel.cpython-313-x86_64-linux-gnu.so`, 2,205,032 bytes |
| Native artifact SHA-256 | `f5096aa589925b6972e5393de41bb1a47cbf60cfdcf2152605b9f4ce684c2822` |
| ELF build ID | `1bd1bb20c9e20d87e7c657da7ab4c264017d3990` |
| Resource use | 70.71 seconds; 241,728 KiB peak; no process swap |

Every report-level validation is true. Timed samples are deterministic,
worker-width accounting replays match every timed repeat, all configured
private lanes participate where required, host-profile probes reconcile at
one/two/four lanes, event journals are quiescent, device-clock accounting
matches native rounds and cycles, and strict DMA is cross-width and
one-shot/sliced exact.

Final median aggregate throughput and the same-workload-and-budget Phase 3
comparison are:

| Scenario | Lanes | Phase 3 MIPS | Phase 4 MIPS | Change |
|---|---:|---:|---:|---:|
| Private compute | 1 | 48.197 | 41.522 | -13.8% |
| Private compute | 2 | 66.724 | 64.958 | -2.6% |
| Private compute | 4 | 96.416 | 110.689 | +14.8% |
| Shared memory | 1 | 1.703 | 1.960 | +15.1% |
| Shared memory | 2 | 0.630 | 0.971 | +54.2% |
| Shared memory | 4 | 0.554 | 1.280 | +130.9% |
| MMIO poll | 1 | 1.039 | 1.222 | +17.6% |
| MMIO poll | 2 | 0.474 | 0.747 | +57.4% |
| MMIO poll | 4 | 0.449 | 0.956 | +113.0% |
| Timer interrupt | 1 | 9.725 | 10.490 | +7.9% |
| Timer interrupt | 2 | 8.683 | 9.825 | +13.2% |
| Timer interrupt | 4 | 8.579 | 10.164 | +18.5% |
| Legacy storage/display | 1 | 2.572 | 2.268 | -11.8% |
| Legacy storage/display | 2 | 1.006 | 0.966 | -3.9% |
| Legacy storage/display | 4 | 0.956 | 1.281 | +34.0% |

This table is deliberately not summarized as a universal speedup. The final
historical pairing is lower for one-lane private compute and one/two-lane
legacy orchestration. Clean milestone controls varied materially between
runs, and the legacy fixture is sequential, but the negative observations are
retained rather than hidden. The strongest repeatable Phase 4 gains are the
shared/MMIO reductions and four-lane private throughput.

Strict NIC/disk DMA medians are:

| Lanes | Phase 3 payload B/s | Phase 4 payload B/s | Change | Virtual cycles/byte |
|---:|---:|---:|---:|---:|
| 1 | 129,300 | 137,059 | +6.0% | 2.00537109375 |
| 2 | 127,653 | 133,340 | +4.5% | 2.00537109375 |
| 4 | 131,518 | 146,035 | +11.0% | 2.00537109375 |

The final report and the Phase 3 report have identical canonical-state,
behavior, and ordered-public-accounting hashes for every corresponding
scenario and width:

| Scenario | Canonical state | Behavior | Ordered public accounting |
|---|---|---|---|
| Private compute | `f8e787ebfcf846f3e3f53f9261ca95bdb77069b1460eda167e41eacd7bce195a` | `00654569ab49f6b6c22cef69f9924e14f37453b953841ec618727333ce2a6e3b` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| Shared memory | `19d3316fa5fde1f242c7a26c8c22e57640fda2aea45fef5d135c3eda8592c83a` | `de5fc6eabb0847c9d14b9c5e274602de9cdb73bfcf7a1f3fcbbfb7813b118370` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| MMIO poll | `7aa840a56d314757d693e35e6f0c8302bd03e534f55d9bf130c908bb03789cb0` | `d13d153f567415a065dd28a26263bf2f978fae45aaa5cbe4acae84dd613e2ec2` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| Timer interrupt | `2136c6d783ddb012bb3f67e7846bbee0a4c6652aed97072251ae436cfd9d79ff` | `ac24c0d978873bf857b2fcea16636c4c6638446c31e525b959d73ffa66847aa0` | `86c2dff1a449934942f463d7ba4755711262a0e5f770bafa9328f0a3442971b5` |
| Legacy storage/display | `4e403d12514d577152a8c5856311c15aab469962a20e3b8d02b32e0f2e6fdd26` | `1f5dfd136e6300fc7db252e369713287c8c6b21866e7a3b8b183a23b7324accd` | `59e3f66d435a4b75dc63941913131a584f83a5dee035f81f9ab83fbba96366f6` |

Strict-DMA timed behavior remains
`b3ea98ff2ee4a15bc56eb1b337e62b211f4d240271150865de7b1f88f61552d6`;
state excluding batch-boundary count remains
`c1addd0d8c6de2b9ec9c974f6633ddd974be11f829453238e0aaeb4800c0b941`.

## Final specialized controls

Both specialized reports were generated from clean source revision
`ec7f37f63954cc45266251ea5d7b31172e582df8` and the same 2,205,032-byte
native artifact used by the comprehensive report. Its SHA-256 is
`f5096aa589925b6972e5393de41bb1a47cbf60cfdcf2152605b9f4ce684c2822`
and its ELF build ID is `1bd1bb20c9e20d87e7c657da7ab4c264017d3990`.

The single-active-microcore command was:

```text
/usr/bin/time -v python3 bench_phase2_microcore.py \
  --instructions 500000 \
  --worker-counts 1,2,4 \
  --repeats 5 \
  --warmups 1 \
  --warmup-instructions 100000 \
  --host-profile \
  --output build/phase4-microcore-final.json
```

The schema-6 report was generated cleanly at
`2026-07-26T23:36:22.145283+00:00`, is 245,581 bytes, and has SHA-256
`6962f76a7f29269422b2f60fc97736033692e86e6a75bb9ea60cd0a92e1d7813`.
The command took 0.54 seconds, peaked at 30,116 KiB, and used no process swap.
One/two/four-lane medians are 25.604/24.404/23.508 MIPS. Every width retains:

- canonical state:
  `acefb4be60d898244b47d2a9254fd8a7405593e6c6014843d1ea2f8f1c036e5a`
- behavior:
  `4882f2009791627b60bf1039bb9a53de2c2512931a97fa0b31599f6723e70501`
- ordered public accounting:
  `2bc0689dd18e20a33870e568d5661133f8ea14a22d39971c880e4c5bc4a8d807`

The instruction-cache command was:

```text
/usr/bin/time -v python3 bench_phase2_icache.py \
  --instructions 200000 \
  --repeats 3 \
  --warmups 1 \
  --output build/phase4-icache-final.json
```

The schema-3 report was generated cleanly at
`2026-07-26T23:36:28.521947+00:00`, is 64,170 bytes, and has SHA-256
`0b6673ae9df6a934584a1cfa3bcdfdb44921b7b72a30ec30ad5b790ee28a0a45`.
The command took 0.12 seconds, peaked at 32,820 KiB, and used no process swap.
Hot and disabled medians are 88.536 and 45.592 MIPS; the report's median
paired hot/disabled ratio is a diagnostic 1.966x. All backend, hot/disabled
architecture, final-byte mutation,
suspended-load, and timing-hygiene validations pass. Canonical state remains
`b0bb4266f1fd36a73722cbadad17e2f109eac3f77579b4cc3e44f74de35ccffe`;
benchmark architectural state remains
`ae14e15754764700096c4305e88b433888518040d5dc1aa291cf7a93700502fd`.

## Design-contention custody

No Phase 4 contention was silently resolved by performance work. The complete
P4-D1 through P4-D8 ledger remains in the Phase 4 plan:

| IDs | Custodied decision |
|---|---|
| P4-D1, D2 | Profiling is host-only and throughput claims are workload-specific, never architectural or universal. |
| P4-D3, D4 | Zero-progress bypass and checkpoint deferral occur only before guest mutation and retain complete-frontier failure visibility. |
| P4-D5 | Longer mixed-topology spans require a sole executable/coordinator participant; multi-participant frontiers remain one instruction. |
| P4-D6 | Admission reuse uses complete guest-visible byte identity; it is not translated execution. |
| P4-D7 | Effective Python clock participation is classified at registration and preserves active-hook order/failure prefix. |
| P4-D8 | Only a complete all-full-core ordinary pass coalesces its host GIL transition; each core retains its own mapping scope. |

Two closure clarifications are part of that custody:

1. The P4-D8 coordinator timer does not have an identical before/after scope.
   The old timer began before each per-core memory acquisition and GIL
   transition; the optimized timer begins after the pass-wide GIL release.
   It remains diagnostic, while unprofiled throughput and equivalence support
   the optimization.
2. P4-D6 clears host plans on explicit guest-I-cache restore. Ordinary
   execution-checkpoint rollback intentionally retains the host table, and
   every later hit revalidates the restored guest-cache tags and exact bytes.

P3-D1 and P3-D9 remain particularly important: a later callback failure keeps
the gathered peer-private frontier and earlier ordered shared commits. That is
the honest current emulator behavior, not transactional failure atomicity.
Any future choice of rollback or provably boundary-free predecode must update
the ledger and exact one/two/four-lane failure oracles.

## Deliberate exclusions and remaining limitations

- Shared, MMIO, timer, and legacy frontiers remain coordinator-bound and do
  not generally scale positively with lane count. Phase 4 reduced host cost;
  it did not change their architecture to force scaling.
- The host decode table is an admission cache, not a translated executor or
  JIT. Full-core workers still execute the authoritative native decoder.
- Strict DMA covers a short 1 KiB payload per NIC/disk endpoint under default
  equal eligibility. Hard-QoS transitions, unused-reservation borrowing,
  active-display overlap, and bulk bandwidth remain unproved.
- Reduced-core strict timing remains unsupported.
- Device-clock participation recognizes class-level `Device.tick` overrides
  at registration. Instance-level monkeypatches, later class mutation, and
  direct mutation of `bus.devices` are unsupported.
- The GIL fast path excludes partial or closed passes, micro and mixed
  topology, cluster work, private trap/reset results, nested mapping scopes,
  strict-cycle work, round settlement, and cross-round fusion.
- External-ingress replay remains scoped, not whole-machine replay.
- Before the producer cap, the UART-geometry `run_batch` parameter stalled
  under TSan without a race report. Both capped parameters pass ordinary
  execution, but the E6 sanitizer selection intentionally omits this case, so
  no post-cap TSan result is claimed.
- Final Phase 3/4 has not been rerun through Akashic SR2. Only the Phase 2
  snapshot retains the byte-identical 493-assertion comparison. Its checked-in
  0.8--1.6-billion instruction limits require fresh approval before repetition.
- Full FPGA synthesis, timing closure, and place-and-route remain outside this
  software-emulator phase.

Resource exclusions remain explicit:

- do not repeat the historical approximately 10.49 GiB optimized selection,
  7.54 GiB and 4.56 GiB supplemental gates, or 5.27 GiB broad ASan selection
  without approval;
- do not run unrestricted `tests/test_system.py`, large framebuffer stress,
  persistence, or Akashic gates without approval;
- keep every test suite and benchmark sequential; do not use pytest-xdist,
  `Promise.all`, parallel agents, or multiple terminal sessions to run them;
- obtain approval before any test that spawns workers, has an unusually large
  step budget, or may use more than 4 GiB of memory; and
- disk space was 97% used with approximately 7.1 GiB free at closure.

The earlier UART producer's approximately 14.3 GiB run is not a current
ordinary-test warning: the test-local producer is now capped, and the complete
34-test concurrency-handoff file measured approximately 44 MiB. The historical
resource figures remain recorded so an unbounded producer is not reintroduced.

## Resume point

The next phase or maintenance branch should begin only from the commit
containing this handoff, after deliberate transfer from the isolated worktree.
No push is part of this snapshot.

The safest next choices are:

1. add strict-DMA hard-QoS transition, unused-reservation borrowing, and
   active-display-overlap oracles before any bulk-DMA optimization;
2. make an explicit architecture decision before changing P3-D1 callback
   partial-progress behavior;
3. pursue shared/MMIO frontier-density reduction only with a proof that
   preserves complete-frontier gathering, cyclic order, callbacks,
   interrupts, and failure prefixes; or
4. pursue translated execution only as a separately reviewed executor with
   exact guest-cache identity, invalidation, checkpoint, and rollback proofs.

The local branch is ready for the user's review and eventual merge/push
decision.
