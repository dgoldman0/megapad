# Megapad deterministic-concurrency handoff: Phase 3 snapshot

**Snapshot date:** 2026-07-26

**Status:** Phase 3 complete within the architecture and evidence boundaries
below

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Implementation revision:** `ab3d1dab9d06b6a304ff293d257e8c592e3f5ccb`

**Phase 2 base:** `79174458cfd8a0b0369381bc8ee8641a636abe9b`

## Purpose and custody

This is the versioned completion and resumption point for Phase 3 of the
deterministic-concurrency project. It records the implemented scope, exact
commit chain, behavioral and sanitizer evidence, performance results, design
contentions, and remaining boundaries. The Phase 2 handoff remains a frozen
historical snapshot and was not rewritten.

All Phase 3 work is local Git work. Nothing in this branch was pushed. The
isolated worktree protected both this work and unrelated work in the main
workspace through multiple session and system crashes. The completion snapshot
is the commit containing this file; it can always be recovered with:

```text
git log -1 -- docs/megapad-concurrency-phase3-handoff.md
```

Generated benchmark JSON is intentionally ignored rather than committed. Its
commands, source revision, fixture hashes, native-artifact identity, and exact
file hashes are recorded below so the evidence can be reproduced or compared
without carrying a large generated artifact in Git.

## Completion boundary

Phase 3 now provides:

- a one-, two-, or four-lane persistent native worker pool owned by one
  `SystemState`, with lane zero on the coordinator thread;
- conservative cache-resident private execution on helper lanes, with every
  shared, uncertain, callback-capable, or strict-timing boundary returned to
  the coordinator;
- deterministic full-core and mixed full/reduced-core synchronous frontiers,
  equal-credit work-conserving scheduling, ordered shared-effect commit, and
  cluster arbitration;
- a narrow strict-cycle helper subset restricted to proven one-cycle private
  full-core instructions, while events, DMA, bus targets, interrupts, stop
  selection, and multicycle work stay coordinator-owned;
- versioned UART, NIC, and geometry host-ingress recording/replay with explicit
  release phase and positive batch boundary;
- exact one/two/four-lane equivalence oracles over captured architectural
  state, behavior, ordered public accounting, strict DMA results, and stop
  state; and
- isolated sequential ASan/UBSan and TSan build/test entry points.

Phase completion means this correctness-first design is implemented and
versioned. It does not mean every instruction runs concurrently, every workload
is faster, unrestricted whole-machine replay exists, or all outstanding
architectural contentions have been permanently resolved. Phase 4 remains the
safe-throughput optimization stage.

## Frozen architecture decisions

- `worker_count` is the total host-execution lane count, including coordinator
  lane zero. Supported values are exactly 1, 2, and 4. One lane is the
  thread-free architectural reference.
- Helpers execute only work proven private under the relevant API. Every shared
  effect and every guest-visible ordering decision remains coordinator-owned.
  Physical lane identity, completion order, command count, and cohort count are
  host diagnostics, never architectural inputs.
- A complete logical frontier is gathered across as many physical cohorts as
  necessary before shared commit. Worker width therefore cannot alter
  guest-visible frontier membership or cyclic commit order.
- Hard QoS determines must/may eligibility and reserved entitlement.
  Simultaneously eligible peers use equal round-robin ordering. Unused reserved
  capacity is work-conserving. There are no secondary weights, aging rules, or
  adaptive biases.
- The integrated SoC resets the hardware weight registers to all ones and
  bandwidth limits to zero, meaning unlimited. A generic weighted RTL
  testbench mode is not architectural policy.
- The advertised topology remains four full cores with global IDs 0--3 and
  three four-microcore clusters whose microcores have global IDs 4--15.
  `CLUSTER_EN` resets to all ones. An explicit clear holds a cluster in reset;
  re-enabling releases its reduced cores from PC zero.
- The unbounded and strict-cycle APIs retain distinct timing contracts.
  Equality is established across worker widths within each API, not between
  the two APIs.
- Strict helper execution is full-core-only and limited to preclassified,
  cache-resident, callback-free instructions with an exact one-cycle cost.
  Reduced-core strict timing remains unselected rather than inferred from
  Python fallback costs.
- External replay covers timestamped host ingress and its release boundary.
  The resumable bus journal separately guarantees exactly-once target effects.
  Neither mechanism is advertised as a complete machine transcript.
- Full-core private guest I-caches remain intentionally noncoherent across
  other cores, clusters, DMA, and host writes. Visibility outside writer-local
  invalidation requires an explicit guest or host invalidation.

The complete normative contracts are in
`docs/megapad-concurrency-phase3-plan.md`. That plan's P3-D1 through P3-D19
ledger is part of this snapshot and must be read before changing scheduler,
failure, timing, replay, or host-access behavior.

## Phase 3 commit chain

The seven Phase 3 commits after the Phase 2 snapshot are:

| Milestone | Commit | Outcome |
|---|---|---|
| Test-safety prerequisite | `fa80bc9542a3622738dc45f56417a3fc031557c3` | Made Make-driven testing sequential and mutually exclusive before persistent helpers were introduced. |
| Element 1 | `8752ef855e9b1037cdce0f533abe1fb2b3e45b2b` | Added the persistent 1/2/4-lane worker-pool substrate and lifecycle/ownership contract. |
| Element 2 | `6ecf5445a7a7007317a9f263e1109a399961bcc0` | Added the typed cache-resident private full-core execution protocol. |
| Element 3 | `62912918fbaef1bca12c3ee7051e8f8af33cb867` | Integrated full-core logical frontiers, equal credit, and ordered coordinator settlement. |
| Element 4 | `20dce3141ee47e836a388408fa1e5b65f155ae0d` | Generalized the coordinator to reduced cores and deterministic cluster arbitration. |
| Element 5 | `e9c4db48db18bc1eda2a6f849bdb3af7469a58c3` | Integrated the strict private subset, deterministic events/DMA, scoped ingress replay, and authoritative stop handling. |
| Element 6 | `ab3d1dab9d06b6a304ff293d257e8c592e3f5ccb` | Completed cross-width equivalence, public accounting, sanitizer tooling, benchmark validation, and refreshed Phase 2 evidence. |

Each commit has a detailed multi-paragraph message. Later corrective findings
were committed at the current milestone rather than hidden in new element
identifiers or left as uncommitted crash-sensitive state.

## Checkpoint regression evidence

All checkpoint test processes recorded here were foreground, sequential, and
mutually exclusive. Counts overlap between checkpoints and are not additive.

| Element | Final owned gate | Measured resource note |
|---|---:|---|
| 1 | 166 passed | Build approximately 1.12 GiB; test peaks approximately 41--62 MiB |
| 2 | 297 passed | Build approximately 1.15 GiB; test peak approximately 92 MiB |
| 3 | 349 passed | Build approximately 1.16 GiB; test peak approximately 94.5 MiB |
| 4 | 201 passed | Build approximately 1.17 GiB; two test peaks approximately 49 and 55.4 MiB |
| 5 | 324 passed | Build approximately 1.18 GiB; test peak approximately 72.1 MiB |
| 6 optimized selection | 155 passed | 62.56 seconds; unexpectedly approximately 10.49 GiB peak |

Element 3 also had a 216-test supplemental gate that peaked at approximately
4.56 GiB. Element 4 had a 34-test supplemental handoff gate that peaked at
approximately 7.54 GiB. Neither was repeated after later narrow corrections;
the affected paths were covered by the bounded focused gates. The Element 6
optimized selection included the 1,024- and 2,048-pixel framebuffer
render-race cases, but no single cause for its 10.49 GiB peak is claimed.

The three large selections above exceeded the normal resource boundary. Do not
repeat them, the full `tests/test_concurrency_handoff.py`, or unrestricted
`tests/test_system.py` without explicit approval. The checked-in instruction
limits must not be raised.

## Element 6 architectural equivalence

The final report was generated from a clean implementation revision:

```text
python3 bench_phase0_concurrency.py \
  --cores 4 \
  --worker-counts 1,2,4 \
  --instructions 2m \
  --repeats 3 \
  --warmups 1 \
  --warmup-instructions 100k \
  --strict-dma-bytes 1024 \
  --output build/phase3-concurrency-final.json
```

Provenance:

| Field | Value |
|---|---|
| Repository revision | `ab3d1dab9d06b6a304ff293d257e8c592e3f5ccb` |
| Repository dirty flag | `false` |
| Report schema | `megapad.phase0-concurrency-baseline`, version 8 |
| Canonical-state schema | `megapad.phase0-canonical-state`, version 9 |
| Generated | `2026-07-26T18:46:36.260283+00:00` |
| JSON size | 43,047,923 bytes |
| JSON SHA-256 | `0136bd9c2f2c91d63c64eeef74bd7be86651d8b6516d8a31ccdedbcf2c354311` |
| Fixture-manifest SHA-256 | `b3867065c27ffd638315552453de988003d7b9e77b2f5801262fd4ce87c6436f` |
| Native artifact | `_mp64_accel.cpython-313-x86_64-linux-gnu.so`, 2,153,816 bytes |
| Native artifact SHA-256 | `58e691a7292fe377c030faebdd72dac9d75c7a2dc576093fdf91b738f07b2fa9` |
| Native ELF build ID | `b57ba3a224ea73e05c4879b0fe6182c0e3afd1d6` |
| Benchmark resource use | 1 minute 41.28 seconds; 240,348 KiB peak; no swap |

All 22 report validation booleans are true. For each of the five workloads,
the one-, two-, and four-lane runs have identical canonical-state, behavior,
and ordered-public-accounting hashes. Timed repeats are deterministic, the
accounting replay matches every timed repeat, native dispatch/continuation/
stop accounting is internally consistent, ingress journals are quiescent, and
every required private lane participates.

Exact workload oracles:

| Workload | Canonical-state SHA-256 | Behavior SHA-256 | Ordered public-accounting SHA-256 |
|---|---|---|---|
| Private compute | `f8e787ebfcf846f3e3f53f9261ca95bdb77069b1460eda167e41eacd7bce195a` | `00654569ab49f6b6c22cef69f9924e14f37453b953841ec618727333ce2a6e3b` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| Shared memory | `19d3316fa5fde1f242c7a26c8c22e57640fda2aea45fef5d135c3eda8592c83a` | `de5fc6eabb0847c9d14b9c5e274602de9cdb73bfcf7a1f3fcbbfb7813b118370` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| MMIO poll | `7aa840a56d314757d693e35e6f0c8302bd03e534f55d9bf130c908bb03789cb0` | `d13d153f567415a065dd28a26263bf2f978fae45aaa5cbe4acae84dd613e2ec2` | `40ad99de3434614bbf53d868b356eec88365398c48c05e039d2871229aca37d4` |
| Timer interrupt | `2136c6d783ddb012bb3f67e7846bbee0a4c6652aed97072251ae436cfd9d79ff` | `ac24c0d978873bf857b2fcea16636c4c6638446c31e525b959d73ffa66847aa0` | `86c2dff1a449934942f463d7ba4755711262a0e5f770bafa9328f0a3442971b5` |
| Legacy storage/display orchestration | `4e403d12514d577152a8c5856311c15aab469962a20e3b8d02b32e0f2e6fdd26` | `1f5dfd136e6300fc7db252e369713287c8c6b21866e7a3b8b183a23b7324accd` | `59e3f66d435a4b75dc63941913131a584f83a5dee035f81f9ab83fbba96366f6` |

The strict NIC/disk DMA probe also passed cross-width equality:

| Field | Value |
|---|---|
| Timed canonical-state SHA-256 | `8f1409f86584e5081f67619de6f6b005818330bb4702f510671be6a332dbbd04` |
| Timed behavior SHA-256 | `b3ea98ff2ee4a15bc56eb1b337e62b211f4d240271150865de7b1f88f61552d6` |
| Sliced behavior SHA-256 | `599f30dcc5030e5db58c36eb95bc6868be1cb5a2e2dfe99744bdae969bc56fb4` |
| Timed public-result SHA-256 | `31cb4e6bbf1e0f25119091c36e92603b0db72822d49f06e99a3d97e069d62bcf` |
| Sliced public-result SHA-256 | `c32229ccc416873fc6b78287f3bc373ac2432460fc50cd79b4a59a733faa525d` |
| One-shot/sliced normalized-state SHA-256 | `c1addd0d8c6de2b9ec9c974f6633ddd974be11f829453238e0aaeb4800c0b941` |
| Equal-round-robin service trace | 2,054 grants; SHA-256 `c0c64ebff6b5452c127e9980f3e12f93cf8b5e559dc6e356e9e7153891558d36` |
| Virtual cycles per payload byte | 2.00537109375 at every width |
| Median MiB/s, one/two/four lanes | 0.123310 / 0.121739 / 0.125425 |
| One-lane-relative byte rate | 1.000000 / 0.987261 / 1.017153 |

The one-shot and one-cycle-sliced strict probes deliberately have different
positive call-boundary counts and different public-result segmentation.
Cross-width equality is exact within the one-shot pattern and independently
exact within the sliced pattern, including each pattern's ordered public
results. The cross-pattern state comparison normalizes only the
invocation-sensitive completed batch-boundary count and then produces the
normalized-state hash above. It does not claim identical public-result
segmentation, that arbitrary host call segmentation is architectural state, or
that unbounded and strict timing are equivalent.

## Performance interpretation

The same report records diagnostic median throughput and process CPU
utilization:

| Workload | 1 lane MIPS / CPU | 2 lanes MIPS / relative / CPU | 4 lanes MIPS / relative / CPU |
|---|---:|---:|---:|
| Private compute | 48.197 / 99.97% | 66.724 / 1.384 / 167.08% | 96.416 / 2.000 / 260.92% |
| Shared memory | 1.703 / 99.83% | 0.630 / 0.370 / 121.55% | 0.554 / 0.325 / 211.27% |
| MMIO poll | 1.039 / 99.93% | 0.474 / 0.457 / 118.33% | 0.449 / 0.432 / 193.46% |
| Timer interrupt | 9.725 / 99.96% | 8.683 / 0.893 / 113.00% | 8.579 / 0.882 / 145.38% |
| Legacy storage/display | 2.572 / 100.00% | 1.006 / 0.391 / 121.72% | 0.956 / 0.372 / 207.64% |

The private-compute case establishes useful host overlap: process CPU
utilization exceeds 100%, every configured lane receives eligible work, and
four-lane throughput is about twice the one-lane reference. Per-lane command
counts alone are not treated as proof of simultaneous execution.

The other workloads expose substantial multilaned overhead but do not by
themselves attribute it to any one mechanism or support a universal speedup
claim. Logical-frontier allocation, shared-boundary classification,
GIL/coordinator transitions, and legacy orchestration are Phase 4 profiling
targets; their individual contributions have not yet been measured. Any
optimization must preserve the hashes and design decisions.

## Sanitizer and race evidence

The public tooling targets accept only:

```text
make test-sanitize SANITIZER=address-undefined
make test-sanitize SANITIZER=thread
```

Those bare commands run the broader checked-in default file set. They are
entry-point examples, not the bounded 16- and 15-test reproduction commands,
and must not be run without resource approval. The exact bounded selections
used at the completion checkpoint were:

```text
make test-sanitize \
  SANITIZER=address-undefined \
  SANITIZE_TEST_PATHS="tests/test_phase3_worker_pool.py tests/test_phase3_private_execution.py tests/test_accel_buffer_binding.py" \
  K="test_worker_count_rejects_nonfixed_integer_values or test_helper_mailboxes_survive_rapid_back_to_back_reposts or test_buffer_acquisition_callback_can_join_execution_without_lock_cycle or test_buffer_release_callback_can_join_execution_without_lock_cycle"

make test-sanitize \
  SANITIZER=thread \
  SANITIZE_TEST_PATHS="tests/test_phase3_private_execution.py tests/test_phase3_coordinator_execution.py tests/test_phase3_reduced_core_execution.py tests/test_phase3_event_execution.py tests/test_concurrency_handoff.py tests/test_accel_buffer_binding.py" \
  K="test_helper_mailboxes_survive_rapid_back_to_back_reposts or test_callback_failure_preserves_the_complete_private_frontier or test_exact_cycle_ceiling_does_not_mask_callback_failure or test_reduced_callback_failure_preserves_prefixes_without_a_grant or test_live_staged_ingress_replays_at_the_same_post_batch_boundary or test_between_batch_live_ingress_replays_before_the_next_batch or (test_uart_geometry_host_updates_progress_during_native_execution and step) or test_buffer_acquisition_callback_can_join_execution_without_lock_cycle or test_buffer_release_callback_can_join_execution_without_lock_cycle"
```

Even these bounded commands measured approximately 2.18 GiB and 1.74 GiB
respectively. They must remain sequential and mutually exclusive.

Each target enters the existing foreground supervisor before building, rejects
overlap with another owned test run, disables pytest-xdist, and imports only
the extension built below `build/sanitizers/<mode>/`. ASan/UBSan preloads ASan
and `libstdc++` into the Python child in the required order; leak detection is
disabled, so the evidence covers address and undefined-behavior findings but
not leak detection. TSan disables ASLR for only the instrumented child with
`setarch <arch> -R`; an unsupported host fails rather than being labeled a
pass. Neither path overwrites the optimized in-tree extension.

Sequential evidence:

| Gate | Result | Peak RSS | Interpretation |
|---|---:|---:|---|
| Broad ASan/UBSan Phase 3 plus selected host/native race cases | 149 passed, 84 deselected; 95.68 seconds of tests | 5,522,520 KiB (approximately 5.27 GiB) | No ASan/UBSan finding, but too large for routine repetition |
| Final bounded ASan/UBSan gate | 16 passed, 106 deselected | 2,287,908 KiB (approximately 2.18 GiB) | Invalid worker values, 1,000 mailbox reposts, and acquisition/release callback joins |
| Direct TSan mailbox diagnostic | 1 passed | 124,500 KiB | Confirmed corrected TSan launch/runtime before the bounded gate |
| Final bounded TSan gate | 15 passed, 200 deselected; 4.70 seconds of tests | 1,829,256 KiB (approximately 1.74 GiB) | Mailbox repost, callback failures, two ingress cases, UART `step`, and acquisition/release callback joins; no TSan race report |
| Final optimized selected gate | 155 passed, 149 deselected; 62.56 seconds | 11,000,796 KiB (approximately 10.49 GiB) | Green optimized regression gate; resource peak requires approval to repeat |
| Final Make/sanitizer entry-point oracle | 2 passed, 14 deselected | 37,748 KiB | Validates isolation, sequential supervision, and rejection of an uninstrumented public mode |

The bounded ASan/UBSan selection is exactly the invalid-worker-value
parameterization, `test_helper_mailboxes_survive_rapid_back_to_back_reposts`,
and all main/HBW/external-memory/VRAM acquisition and release callback/join
parameterizations. The bounded TSan selection adds the complete-private-
frontier, exact-cycle-ceiling, and reduced-core callback-failure tests; the
live-staged and between-batch ingress tests; and only the UART geometry
`step` parameterization, plus the mailbox and buffer callback/join cases.

Two instrumentation limitations are part of the evidence:

1. The first ASan launch aborted in the loader's `__cxa_throw` interceptor
   because ASan was loaded before the C++ runtime. The target was corrected to
   preload ASan and then `libstdc++`; the broad and bounded passes above are
   post-fix.
2. An earlier TSan selection passed seven of 19 tests and then made no progress
   for more than two minutes in
   `test_uart_geometry_host_updates_progress_during_native_execution[batch]`.
   It emitted no TSan race report and was interrupted. The `step`
   parameterization passed under TSan, while both parameterizations pass under
   optimized and ASan/UBSan execution. The continuous-`run_batch` observation
   is therefore a TSan-only instrumented liveness limitation, not an
   ordinary-runtime pass and not an ordinary-runtime failure.

The broad ASan/UBSan and final optimized selections must not be repeated
without explicit resource approval. Use the bounded named selections for
routine sanitizer confirmation.

## Refreshed Phase 2 I-cache evidence

The current cache oracle was regenerated from the same clean implementation:

```text
python3 bench_phase2_icache.py \
  --instructions 200000 \
  --repeats 3 \
  --warmups 1 \
  --output build/phase3-icache.json
```

| Field | Value |
|---|---|
| Report schema | `megapad.phase2-instruction-cache-baseline`, version 2 |
| State schema | unchanged at version 1 |
| Generated | `2026-07-26T18:46:48.763054+00:00` |
| JSON size | 63,421 bytes |
| JSON SHA-256 | `88525684922dbaa1367c36345312aca1717ca3742d91d549c5b8cbfe4017207c` |
| Fixture-manifest SHA-256 | `3274c4af98a805a6f16705a4d7a72a73a09e01b2fa0d97555ae5b469dbe339d4` |
| Canonical-state SHA-256 | `b0bb4266f1fd36a73722cbadad17e2f109eac3f77579b4cc3e44f74de35ccffe` |
| Behavior SHA-256 | `50647ad585bbecd06c1643ecc04d65a7266ee9fae38ac0c318424d01b71a6de4` |
| Benchmark architectural-state SHA-256 | `ae14e15754764700096c4305e88b433888518040d5dc1aa291cf7a93700502fd` |
| Hot-cache median | 91,201,722.887 instructions/s |
| Disabled-cache median | 46,937,081.276 instructions/s |
| Hot/disabled ratio | 1.9353672098 |
| Resource use | 32,712 KiB peak; no swap |

The canonical state is exactly the Phase 2 value. The Phase 2 behavior hash was
`a4881ca5a590f9503fb3cd81fa7f55b6808164b1c37071f426671d4cd6446013`;
the refreshed hash differs solely because the behavior oracle includes evidence
metadata and that command changed from `pytest -n 1` to the supervised,
worker-free Make target. Report schema 1 became 2 to make that provenance
change explicit. The state schema and machine behavior did not drift.

## Akashic SR2 compatibility custody

The independent Akashic comparison was performed against the Phase 2 base
revision, not against final Phase 3:

| Field | Value |
|---|---|
| Frozen Akashic revision | `c4585f4afd444fdc3db55b9b659103a77bc9fe9d` |
| Old MegaPad revision | `a10ae4464d01ed308b460765f3bd64ff51dc433f` |
| Compared new MegaPad revision | `79174458cfd8a0b0369381bc8ee8641a636abe9b` |
| Raw pressure transcript SHA-256, old and new | `81d88a73840ad28085c3f428f057927a93048579db2c555d8a9a7777aff7f809` |
| Frozen Akashic archive SHA-256 | `effb550133e221ecbfd05ae517adb286024c42b0cc8b64f838441d9e00526d2a` |
| Old MegaPad archive SHA-256 | `4683ca82f03343c94e1d1574d2156cb6914442026ee103b6e0198413db6f870a` |
| Generated pressure image SHA-256 | `9d4ee1e9c89776b4e44242a118de3f85e18b0c16e380e3eab35022d250783ac` |

The byte-identical pressure result was:

```text
steps 60 148 30
peak 2
pool-bytes 47000
connection-bytes 7776
pressure-bytes 55400
PASS 493
```

The old emulator retired 1,325,092,021 checked guest instructions in 19.63
seconds, approximately 67.503 Msteps/s. The Phase 2 emulator was observed at
roughly half that wall throughput, but its exact new timing was not preserved,
so no invented value is recorded. The all-clear is narrow one-core SR2
compatibility for revision `7917445`; it is not evidence that final Phase 3
was rerun against Akashic, and it does not exercise multicore concurrency.

Do not rerun the Akashic pressure gate without explicit approval. Its
checked-in limit is 1.6 billion instructions; the other SR2 gates also have
large 0.8--1.2-billion limits and must run sequentially.

## Design-contention custody

Phase completion does not erase the choices discovered during implementation.
The authoritative, versioned record is the full P3-D1 through P3-D19 table in
`docs/megapad-concurrency-phase3-plan.md`. It records the competing behaviors,
the honest behavior currently implemented, the claim boundary, and the trigger
for reopening each decision.

For orientation, the ledger covers:

| IDs | Contention held for later review |
|---|---|
| P3-D1, D6, D8, D9, D13 | Callback failure, retained private prefixes, coordinator accounting, and the deliberate absence of speculative whole-frontier rollback |
| P3-D2, D7 | Complete logical frontiers, equal retained credit, forward-only work conservation, and physical-lane invisibility |
| P3-D3, D5, D15, D16, D18 | Distinct unbounded/strict timing contracts, interrupt and stop precedence, the narrow one-cycle helper subset, and unselected reduced-core strict timing |
| P3-D4, D10 | Removal of the serial mixed path and the explicit synchronous-frontier code-observation interleaving |
| P3-D11, D12 | Ordinary-before-cluster phase barrier, frozen hard eligibility, equal round-robin selection, loser credit, and cross-resource donation |
| P3-D14 | Routed reduced-core fetch, side-effecting MMIO decode-window rejection, and unsupported concurrent direct core-field access |
| P3-D17, D19 | Scoped ingress replay, intentionally incomplete nondeterminism coverage, and host-arrival release/batch linearization |

In particular, P3-D1 preserves the exact issue previously called out: an
ordered callback failure keeps the complete gathered private frontier and all
earlier shared commits. That is an honest functional emulator behavior, not a
claim of transactional failure atomicity. A future decision can choose
rollback, provably boundary-free predecode, or retain the synchronous model,
but it must update the ledger and its one/two/four-lane tests rather than
silently revising history.

## Deliberate exclusions and remaining limitations

- The architectural hashes prove equality only for their documented captured
  scope. The report lists 17 unbound native-state exclusions; equality is not
  inferred for those fields.
- Unbounded and strict-cycle APIs are not cycle-equivalent. Strict reduced-core
  timing is unsupported.
- Callback failure is not transactional across the complete guest frontier.
  The exact retained-prefix semantics are P3-D1 and P3-D9.
- External-ingress replay is not unrestricted machine replay. Deterministic
  entropy, backend outcomes, initial state, and the positive host-call sequence
  still matter.
- Direct concurrent access to exposed core properties/registers during an
  active native system batch is unsupported.
- Reduced-core instruction-fetch windows that may touch side-effecting MMIO are
  rejected before decode.
- The worker pool accelerates only proven private work. Shared-heavy,
  MMIO-heavy, and legacy orchestration workloads currently regress as lane
  count increases.
- Lane command counts prove participation, not simultaneity. Useful overlap is
  supported separately by CPU utilization and wall throughput.
- The strict DMA probe covers NIC and disk peers with default equal eligibility
  but not hard-QoS transitions, reservation borrowing, or active display
  overlap.
- The storage/display benchmark remains a sequential legacy diagnostic. It
  does not claim DMA/display/core overlap.
- The final Phase 3 revision has not been rerun through Akashic SR2. Only the
  Phase 2 base has the byte-identical 493-assertion comparison.
- Full FPGA synthesis, timing closure, and place-and-route remain outside this
  software-emulator phase.

## Resume point

Phase 4 should begin from the commit containing this handoff, after deliberate
review or transfer from the isolated worktree. Its first goal should be
throughput optimization under the existing one-lane reference and exact
one/two/four-lane hashes:

1. profile shared-boundary classification, logical-frontier allocation, GIL
   transitions, and coordinator continuations;
2. reduce overhead without changing global cyclic commit, hard eligibility,
   equal round-robin, or work-conserving credit;
3. widen private segments only with a proof that no shared or callback-capable
   boundary is crossed;
4. add a hard-QoS transition/unused-reservation strict DMA oracle before
   optimizing bulk DMA; and
5. rerun bounded equivalence first, then clean benchmarks, with larger
   sanitizer, Akashic, or framebuffer stress only after explicit resource
   approval.

No push is part of this handoff. The user can decide when the local branch is
ready to move into the main checkout and publish.
