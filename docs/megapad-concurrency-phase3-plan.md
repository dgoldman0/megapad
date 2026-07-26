# Megapad deterministic concurrency: Phase 3 plan

**Started:** 2026-07-26

**Status:** Elements 1–3 of 6 complete; Element 4 has not started

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

## Fixed phase structure

Phase 3 is divided into six elements. These identifiers are stable; discoveries
may produce corrective commits, but they do not create new phase elements.

| Element | Scope | Status |
|---|---|---|
| 1 | Persistent worker-pool lifecycle and fixed 1/2/4-lane configuration | Complete |
| 2 | Private full-core execution commands, results, and deterministic yield boundaries | Complete |
| 3 | Full-core coordinator integration and ordered shared-effect commit | Complete |
| 4 | Reduced-core and cluster integration | Not started |
| 5 | DMA, external events, record/replay, and deterministic stop handling | Not started |
| 6 | One/two/four-lane equivalence, sanitizer stress, refreshed benchmarks, and final handoff | Not started |

## Design-contention ledger

This ledger records choices that are necessary for an honest intermediate
implementation but are not silently promoted into permanent architecture.
Every entry names the alternative, the behavior actually implemented, its
present claim boundary, and the condition for reopening it.

| ID | Contention | Phase 3 interim decision | Honest boundary and revisit trigger |
|---|---|---|---|
| P3-D1 | An ordered shared callback can fail after peers have already executed private prefixes. The old sequential chunk scheduler left later peers untouched; exact preservation would require whole-frontier rollback or provably boundary-free predecode. | Gather the same complete logical private frontier for one, two, and four lanes before beginning any shared commit. On an ordered callback failure, every gathered peer-private prefix remains committed and its elapsed private time is settled. | This intentionally replaces a host-exception partial-progress artifact; it does not claim transactional callback failure. Reopen before release if callback atomicity becomes architectural, or if a non-speculative predecode proof or explicitly approved rollback design becomes available. |
| P3-D2 | A logical sub-frontier can contain more cores than physical host lanes. Committing after each lane-sized cohort would make worker count guest-visible. | Buffer every physical cohort, then merge and commit the complete logical sub-frontier in global cyclic order. Advance virtual time once per completed scheduler round by the maximum complete per-core round cycles, never by a sum of cohort or core totals. Production failures identify the logical core, not its incidental physical lane. | Physical lane width and helper completion order are host diagnostics only. This is a fixed determinism invariant, not an optimization choice. |
| P3-D3 | Unbounded `run_batch` uses a coarse architectural scheduler, while `run_cycle_batch` owns strict ready-cycle and main-bus arbitration. Mixing the two models during worker integration would create a new timing contract. | Element 3 keeps unbounded batches in cyclic coordinator order and leaves strict ready-time/bus ordering to the existing cycle API. | Element 3 claims deterministic architectural batching, not new cycle accuracy. Revisit their convergence with the event/DMA work in Element 5 and the equivalence evidence in Element 6. |
| P3-D4 | Full cores can use private workers before reduced cores and cluster resources can. | A topology containing any microcores remains on the established serial native coordinator through Element 3. | This is explicitly incomplete machine concurrency, not a claim that the advertised 16-core topology is parallel. Element 4 removes this gate. |
| P3-D5 | A pending enabled interrupt can either force a private zero-progress boundary or be ignored until the old serial chunk ends. An asserted line can also become eligible when an instruction enables interrupts. | Helpers retire no instruction past that boundary. `EI` remains a coordinator instruction, so an already-asserted line is observed before another private command. The coordinator performs end-of-round interrupt settlement and then recomputes runnable work; strict event-time acceptance is not invented here. | This improves the honesty of the private boundary without claiming exact asynchronous timing. Timer, IPI, external-event, and deterministic stop integration is revisited in Element 5. |
| P3-D6 | Longer private lookahead could use speculative writes and rollback, while the approved first design calls for safe bounded segments. | Element 3 mutates only callback-free, cache-resident private state in place and stops at the first classified shared or uncertain boundary. It adds no speculative write log or rollback path. | Performance is secondary to deterministic state in this milestone. Reopen only as a separately reviewed optimization with one/two/four-lane differential evidence. |
| P3-D7 | Treating each cache/shared yield as a fresh scheduler credit would make boundary density a secondary QoS weight and would expose helper-wave count through public dispatch statistics. Up-front reservations can also strand budget when an earlier core stops before using its provisional share. | Each core retains its equal round credit across as many deterministic sub-frontiers as needed. Cache refill and ordinary shared instructions keep the logical raw dispatch open; true fallback/trap/reset boundaries close it. Unused terminal or interrupt-shortened credit flows only forward to later peers in the same frozen cyclic round, including a peer whose initial reservation was zero, and no peer exceeds the common quantum. Residual credit never wraps backward. | This is the direct implementation of equal-weight, work-conserving QoS and preserves the established serial scheduler: cache residency, callback density, and host lane count cannot buy extra guest service. Reopen only with an architectural scheduling change, not as a performance shortcut. |
| P3-D8 | Prefix-aware callback-error settlement could be implemented by changing the exposed native scheduler callback from two arguments to four, but that would turn an internal integration need into a low-level callable-contract break and could mask the original guest callback exception with `TypeError`. | Preserve the existing two-argument settlement callable. It returns boundary-local progress; the native coordinator validates that result and composes it with the retained private prefix. | `NativeSystemState.run_full_core_batch` remains an exposed internal seam, not a stable public API, but Element 3 does not gratuitously break it. Revisit only through an explicit versioned native API change. |
| P3-D9 | A host settlement can report accounting so large that the completed sub-frontier cannot be represented. Rewinding the whole round's counters after cores or earlier shared boundaries have mutated is inconsistent, while exact guest rollback is outside the first design. | Validate each sub-frontier into temporary scheduler/result state and publish it only after every aggregate, per-core, continuation, and remaining-budget check succeeds. A failing callback still absorbs every exactly representable private prefix before rethrowing the original object, including progress that reaches the exact signed-cycle ceiling. If absorption is itself unrepresentable, the accounting error necessarily takes precedence. Never rewind the round cursor or outcome behind already published state. | This makes scheduler absorption transactional, not guest execution speculative. A truly atomic invalid-host-callback boundary would require the separately reviewed rollback design excluded by P3-D6. |

Changes to an interim decision must update this table and its tests in the same
milestone. A green test suite alone is not permission to erase the contention
or broaden the architectural claim.

## Element 1 contract

- `worker_count` means the total host-execution lane count, including lane zero
  on the coordinator/caller thread. The only supported values are 1, 2, and 4.
- One lane is the exact inline, thread-free reference configuration.
- Two and four lanes own one and three persistent native helper threads,
  respectively. Element 1 helpers remain dormant; they execute no guest work.
- The pool belongs to one native `SystemState`. Warm boot and guest-visible
  reset do not recreate it, and host lane configuration is not architectural
  state or part of snapshot/oracle hashes.
- Destruction joins helpers before cores, mappings, exporters, devices, and
  scheduler state unwind.
- A helper-bearing system must be constructed after any process fork; carrying
  a live native pool across `fork()` is unsupported.
- Before Element 2 submits commands, ownership must guarantee that pool
  destruction remains on the coordinator thread and can never self-join a
  helper.

Element 1 makes no simultaneous-execution or speedup claim. Its acceptance
boundary is lifecycle correctness plus exact one/two/four-lane equality while
all guest execution remains on the established one-worker scheduler.

## Element 2 contract

- A private wave uses fixed, typed per-lane mailboxes. Each command names one
  full core, one configured lane, and a bounded instruction count; a wave may
  use a lane or core at most once.
- Lane zero runs on the coordinator/caller thread through the same private
  executor used by the persistent helper lanes. Helpers retain stable host
  thread identities across waves. Monotonic command sequences and wave epochs
  identify work, while results are collected in submission order.
- A private command may fetch only from bytes already resident in that core's
  enabled private guest instruction cache. Cold, disabled, or incomplete
  instruction spans yield without falling through to shared memory.
- The admitted subset is deliberately conservative: local SYS, INC/DEC,
  branch/skip, long branch, immediate, ALU, private-only MEMALU, SEP/SEX,
  multiply/divide and bitfield instructions, with at most one ordinary
  register modifier. Shared memory, stack, call/return, I/O, CSR, MEX,
  callback-capable extensions, reserved encodings, and uncertain prefix
  combinations yield before the instruction.
- A taken SKIP must also find the exact target-size byte in cache. The private
  classifier decodes only the bytes that the established executor will fetch;
  it neither speculates across a missing line nor rolls execution back after a
  predictable shared boundary.
- Enabled pending IPI or timer input yields an explicit zero-progress
  interrupt boundary. Halt, idle, guest trap, guest reset, cache boundary,
  shared-instruction boundary, and instruction-limit completion likewise have
  typed results.
- One coordinator-owned mapping admission remains active through the complete
  wave and result collection. Every participating host thread owns a separate
  shared-memory lock and CPU execution guard derived from that admission, so a
  helper cannot prematurely release or transfer the global boundary.
- Active bus grants, suspended cycle execution, pending external events, and
  active event horizons reject private entry before any command is posted.
  Reduced cores, clusters, DMA, and event delivery remain outside this
  full-core-only protocol.
- Commands, mailboxes, and native execution contain no Python objects,
  functions, or callbacks. A command-level checkpoint is retained only to
  contain an unexpected internal failure; ordinary deterministic yields do not
  copy or restore full core state per instruction.

Element 2 is an internal execution seam, not a production scheduler change.
It does not advance the shared clock, commit shared effects, choose runnable
cores, or claim application throughput. Element 3 will issue private commands
from the full-core coordinator and commit their ordered shared boundaries.

## Element 3 contract

- Homogeneous full-core batches, including a sole runnable full core, use one
  coordinator-owned equal-credit scheduler round. A topology containing any
  microcore remains on the established serial path through Element 4.
- Every runnable core receives one cyclic credit position under the existing
  1,000-instruction quantum. The aggregate budget can provision a later
  position with zero initial credit; unused credit from an earlier stopped
  core activates or extends later positions work-conservingly without
  exceeding the common quantum or wrapping backward. Credit persists across
  cache, shared-memory, I/O, and CSR sub-frontiers until it is exhausted, the
  core becomes terminal, or an enabled interrupt blocks it.
- A physical wave may cover only one lane-sized cohort, but no shared effect is
  committed until every cohort in that logical sub-frontier has completed.
  Private prefixes are merged and coordinator boundaries are committed in the
  frozen global cyclic reservation order.
- Python callbacks, continuations, UART draining, interrupt delivery, and
  shared-clock mutation remain coordinator-only. Helper command completion
  order and physical wave count are diagnostic, not architectural.
- One scheduler round advances virtual time by the maximum accumulated
  per-core cycles in that round. The cursor is derived from the last
  progressing reservation in cyclic order, independent of whether progress
  arrived in a private prefix or coordinator suffix.
- Cache refill and ordinary shared instructions do not create extra public
  dispatches or stop reasons. Established fallback, trap, reset, halt, idle,
  and instruction-limit boundaries retain their logical statistics.
- On an ordered callback failure, all private prefixes from the already
  completed sub-frontier and all earlier coordinator commits remain visible;
  their maximum exactly representable elapsed time is settled before the
  original exception is re-raised. No later shared boundary is committed. An
  unrepresentable host-supplied accounting result takes precedence because no
  honest virtual-time settlement exists.
- Enabled pending interrupts block further private progress. `EI` is
  conservatively coordinator-owned so a line asserted while masked is accepted
  before the next guest instruction. Exact event-time behavior remains Element
  5 work.

## Evidence discipline

Test suites remain sequential and resource-monitored. Standard Makefile test
targets must not create pytest-xdist workers; `test-sequential` is the
foreground entry point for focused evidence. Performance comparisons and the
full architectural equivalence matrix belong to Element 6, after helper
dispatch and deterministic commit ordering exist.

## Element 1 evidence

The checkpoint compiled the native extension with C++17 and explicit pthread
compile/link flags. The measured build peak was approximately 1.12 GiB RSS.
All tests ran through one owned foreground pytest process at a time:

- 17 Element 1 pool lifecycle, validation, ownership, reset, and exact
  one/two/four-lane reference tests;
- 14 test-process ownership and sequential-Makefile tests;
- 133 native ownership, scheduler, Phase 0 handoff, and batch-boundary tests;
  and
- the retained Phase 2 instruction-cache and all-core cluster oracles.

The 166 tests passed. Focused test peaks ranged from approximately 41 MiB to
62 MiB RSS. Element 1 helpers remained dormant throughout, so this evidence
does not claim simultaneous guest execution or throughput improvement.

## Element 2 evidence

The native extension rebuilt successfully after the private executor and
mapping-admission changes. The measured build peak was approximately 1.15 GiB
RSS, with no swap activity. The final focused gate ran in one foreground
pytest process and covered:

- 41 Element 2 command, cache-residency, decode-length, yield, result,
  persistent-helper, interrupt, rejection, and validation tests;
- all 17 Element 1 worker-pool lifecycle and reference tests;
- all 54 native cycle, DMA, interrupt, external-event, and resumable-execution
  tests;
- all 63 native buffer attachment, framebuffer, ownership, and lock-order
  tests;
- all 99 native system-state and batch-boundary tests; and
- all 23 retained Phase 2 instruction-cache and versioned-oracle tests.

The 297 tests passed sequentially with an approximately 92 MiB peak RSS and no
swap activity. The gate verifies that helper lanes execute the same cache-only
private protocol and preserve the old ownership and scheduler boundaries. It
does not substitute for the production integration, one/two/four-lane
architectural equivalence matrix, sanitizer stress, or performance
measurements assigned to Elements 3 and 6.

## Element 3 evidence

The production coordinator rebuilt successfully with the same C++17/pthread
configuration. The measured final build peak was approximately 1.16 GiB RSS
with no swap activity; its only compiler diagnostic was the pre-existing
unused `exec_field` warning. The final owned foreground gate covered:

- all 18 coordinator integration oracles, including complete-frontier
  one/two/four-lane equality, repeated ordered shared commits, forward-only
  work-conserving credit, zero-reservation activation, callback failure,
  exact-cycle-ceiling absorption, EI/interrupt accounting, hot trap/reset
  settlement, cold halt/idle, and the mixed-topology gate;
- all 42 private execution and 17 persistent worker-pool tests;
- all 99 native system-state and batch-boundary tests;
- all 23 retained Phase 2 instruction-cache and versioned-oracle tests;
- all 54 native cycle-execution and 18 native bus-transaction tests;
- all 15 native microcore and cluster-oracle tests; and
- all 63 accelerator buffer, mapping, framebuffer, ownership, and lock-order
  tests.

The 349 tests passed sequentially in one pytest process at approximately
94.5 MiB peak RSS with no swap activity. Three independent final read-only
audits found no remaining scheduler, failure-transaction, or high-severity
oracle blocker after their findings were corrected.

A supplemental pre-final-audit gate of 216 MEX, concurrency-handoff, native
string-safety, and display-concurrency tests also passed. It unexpectedly
peaked at approximately 4.56 GiB RSS, so it was not repeated after the later
narrow callback-absorption fix; none of those suites exercises that path.
Comparable gates require explicit resource approval going forward.

Element 3 establishes deterministic homogeneous full-core integration and
ordered coordinator commit. It does not yet claim parallel reduced cores,
strict event/DMA integration, record/replay closure, sanitizer completion, or
the final performance/equivalence results assigned to Elements 4–6.
