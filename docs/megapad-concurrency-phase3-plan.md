# Megapad deterministic concurrency: Phase 3 plan

**Started:** 2026-07-26

**Status:** Elements 1–5 of 6 complete; Element 6 in progress

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
| 4 | Reduced-core and cluster integration | Complete |
| 5 | DMA, external events, record/replay, and deterministic stop handling | Complete |
| 6 | One/two/four-lane equivalence, sanitizer stress, refreshed benchmarks, and final handoff | In progress |

## Design-contention ledger

This ledger records choices that are necessary for an honest intermediate
implementation but are not silently promoted into permanent architecture.
Every entry names the alternative, the behavior actually implemented, its
present claim boundary, and the condition for reopening it.

| ID | Contention | Phase 3 interim decision | Honest boundary and revisit trigger |
|---|---|---|---|
| P3-D1 | An ordered shared callback can fail after peers have already executed private prefixes. The old sequential chunk scheduler left later peers untouched; exact preservation would require whole-frontier rollback or provably boundary-free predecode. | Gather the same complete logical private frontier for one, two, and four lanes before beginning any shared commit. On an ordered callback failure, every gathered peer-private prefix remains committed and its elapsed private time is settled. | This intentionally replaces a host-exception partial-progress artifact; it does not claim transactional callback failure. Reopen before release if callback atomicity becomes architectural, or if a non-speculative predecode proof or explicitly approved rollback design becomes available. |
| P3-D2 | A logical sub-frontier can contain more cores than physical host lanes. Committing after each lane-sized cohort would make worker count guest-visible. | Buffer every physical cohort, then merge and commit the complete logical sub-frontier in global cyclic order. Advance virtual time once per completed scheduler round by the maximum complete per-core round cycles, never by a sum of cohort or core totals. Production failures identify the logical core, not its incidental physical lane. | Physical lane width and helper completion order are host diagnostics only. This is a fixed determinism invariant, not an optimization choice. |
| P3-D3 | Unbounded `run_batch` uses a coarse architectural scheduler, while `run_cycle_batch` owns strict ready-cycle and main-bus arbitration. Mixing the two models during worker integration would create a new timing contract. | Unbounded batches retain cyclic frontier accounting, while strict batches retain ready-cycle and bus arbitration. Both may use the same persistent pool only for work proven safe within their own timing contract. | Phase 3 deliberately does not make the two APIs cycle-equivalent. P3-D15 records the narrow strict helper subset, and Element 6 compares worker widths within each API rather than comparing timing across APIs. Reopen convergence only with an approved architectural timing model. |
| P3-D4 | Full cores can use private workers before reduced cores and cluster resources can. | A topology containing any microcores remains on the established serial native coordinator through Element 3. Element 4 removes that temporary gate and uses the one generalized frontier coordinator for every advertised execution core. | This was explicitly incomplete machine concurrency through Element 3, not a claim that the advertised 16-core topology was parallel. The serial mixed production path is deleted rather than retained as legacy. |
| P3-D5 | A pending enabled interrupt can either force a private zero-progress boundary or be ignored until the old serial chunk ends. An asserted line can also become eligible when an instruction enables interrupts. | Helpers retire no instruction past that boundary. `EI` remains a coordinator instruction, so an already-asserted line is observed before another private command. The unbounded coordinator settles interrupts at its deterministic round boundary; strict execution accepts them at the exact ready-cycle/event frontier recorded by P3-D18. | This preserves the intentionally distinct timing contracts in P3-D3. P3-D18 and P3-D19 close strict event-time and host-ingress ordering; unbounded execution does not acquire strict asynchronous timing by implication. |
| P3-D6 | Longer private lookahead could use speculative writes and rollback, while the approved first design calls for safe bounded segments. | Element 3 mutates only callback-free, cache-resident private state in place and stops at the first classified shared or uncertain boundary. It adds no speculative write log or rollback path. | Performance is secondary to deterministic state in this milestone. Reopen only as a separately reviewed optimization with one/two/four-lane differential evidence. |
| P3-D7 | Treating each cache/shared yield as a fresh scheduler credit would make boundary density a secondary QoS weight and would expose helper-wave count through public dispatch statistics. Up-front reservations can also strand budget when an earlier core stops before using its provisional share. | Each core retains its equal round credit across as many deterministic sub-frontiers as needed. Cache refill and ordinary shared instructions keep the logical raw dispatch open; true fallback/trap/reset boundaries close it. Unused terminal or interrupt-shortened credit flows only forward to later peers in the same frozen cyclic round, including a peer whose initial reservation was zero, and no peer exceeds the common quantum. Residual credit never wraps backward. | This is the direct implementation of the established serial scheduler's equal-weight, work-conserving QoS accounting: cache residency, callback density, and host lane count cannot buy extra guest service. It does not override the explicitly changed mixed code-observation interleaving in P3-D10. Reopen only with an architectural scheduling change, not as a performance shortcut. |
| P3-D8 | Prefix-aware callback-error settlement could be implemented by changing the exposed native scheduler callback from two arguments to four, but that would turn an internal integration need into a low-level callable-contract break and could mask the original guest callback exception with `TypeError`. | Preserve the existing two-argument settlement callable. It returns boundary-local progress; the native coordinator validates that result and composes it with the retained private prefix. | `NativeSystemState.run_full_core_batch` remains an exposed internal seam, not a stable public API, but Element 3 does not gratuitously break it. Revisit only through an explicit versioned native API change. |
| P3-D9 | A host settlement can report accounting so large that the completed sub-frontier cannot be represented. Rewinding the whole round's counters after cores or earlier shared boundaries have mutated is inconsistent, while exact guest rollback is outside the first design. | Validate each sub-frontier into temporary scheduler/result state and publish it only after every aggregate, per-core, continuation, and remaining-budget check succeeds. A failing callback still absorbs every exactly representable private prefix before rethrowing the original object, including progress that reaches the exact signed-cycle ceiling. If absorption is itself unrepresentable, the accounting error necessarily takes precedence. Never rewind the round cursor or outcome behind already published state. | This makes scheduler absorption transactional, not guest execution speculative. A truly atomic invalid-host-callback boundary would require the separately reviewed rollback design excluded by P3-D6. |
| P3-D10 | A reduced core has no private instruction cache. If a cyclic-earlier core reaches a shared write to the reduced core's current opcode, the old serial scheduler commits the write before the later core runs; a complete parallel logical frontier can gather the old opcode first. | Mixed topologies use deterministic synchronous frontiers: every admitted core may retire at most one private instruction before any ordered shared commit. Thus a reduced core can retire the old current opcode once in the same gathered frontier, and must observe the write on the following frontier. The exact current-opcode and following-opcode cases are both versioned across one, two, and four lanes. | This is an explicit interim interleaving, not a claim that the old serial artifact or an RTL same-cycle rule has been preserved. Reopen after choosing among synchronous-frontier semantics, cyclic boundary preclassification, or a separately reviewed dependency/rollback design. |
| P3-D11 | Element 3 described one global cyclic commit order, while the established mixed coordinator settled ordinary boundaries before selecting cluster winners. An earlier ordinary or cluster BUS effect can also invalidate a captured later request. | Preserve the mixed ordinary-before-cluster phase barrier. Settle ordinary boundaries cyclically, then recapture all live cluster requests and freeze hard eligibility, candidate membership, and equal round-robin choices together. Before executing each selected request, revalidate that selected core's runnable state, exact PC/encoding, and current hard eligibility. If it changed, defer only the selected core; frozen nonselected losses and donations stand. Later cluster commits do not retroactively rerun the frozen arbitration. | This retains the established mixed phase barrier, prevents execution of a stale selected instruction, and gives arbitration one synchronous snapshot independent of commit order. A unified cross-class cyclic order or commit-sensitive re-arbitration is an architectural change and requires its own oracle and review. |
| P3-D12 | A cluster arbitration loser can either forfeit its whole round position or retain equal service credit; a later zero-reservation request may also be the selected peer. | Every frozen nonselected request is a loss, including a hard-ineligible group with no selected peer. A loss closes exactly that fallback dispatch with zero retirement, cycles, and grants. The loser retains residual equal round credit and retries after any frontier or coordinator progress. Only an unchanged all-zero frontier invokes the no-spin rule and releases stranded credit forward. A selected later zero-credit request borrows exactly one forward unit from a cyclic-earlier frozen loser, including across resources; larger residual credit remains with the donor. | This implements hard eligibility plus equal round-robin ordering and work conservation without adding secondary weights. Reopen only with an explicit QoS architecture change. |
| P3-D13 | A Python CRC/SHA continuation can acquire a cluster lock or mutate shared-engine state and then raise before native grant publication. | Snapshot the complete cluster state immediately before every selected continuation. Publish validated scheduler accounting and then commit the preflighted grant; on any failure before commit completion, restore that cluster snapshot and rethrow the original exception. Earlier resource commits remain. | External guest-memory writes performed by a failing Python continuation remain subject to P3-D1's nontransactional callback boundary. The narrower guarantee here is that a failed winner cannot orphan cluster ownership, engine state, grant counts, or grant sequence. |
| P3-D14 | Native reduced-core scalar fetch reads raw mapped memory, while the compatibility oracle routes scratchpad and MMIO instruction fetch through Python. Direct Python field access can also race a helper while the batch has released the GIL. | A routed decode window yields from the helper. The coordinator classifies each actually consumed byte through the same scratchpad-versus-mapped-RAM rule as the Python oracle, including instructions crossing either scratchpad boundary, then executes only through that oracle. Any possible 16-byte decode window touching MMIO is explicitly unsupported in a native system batch because classification could itself have read side effects; it fails before decoding or granting. Concurrent direct `CPUState`/wrapper property or register access, read or write, from another host thread during an active native system batch is unsupported; coordinator continuations remain the supported access path. | Routed-fetch handling is a correctness boundary, not a performance claim. Revisit side-effect-safe MMIO fetch and direct-access hardening before exposing concurrent host control as supported APIs; the latter requires ownership-aware bindings rather than blanket rejection that would also block coordinator fallback. |
| P3-D15 | Strict cycle execution can remain a serial island, workerize complete multicycle instructions speculatively, or parallelize only work whose complete commit window is already one cycle. | The strict scheduler submits only preclassified, cache-resident, callback-free full-core instructions with a proven exact one-cycle cost. It gathers only the cyclic prefix before the first actionable resumable/coordinator instruction, executes physical cohorts through the same persistent pool, and publishes accounting in frozen cyclic order. A candidate-set checkpoint restores only private core state if an unexpected helper failure occurs before publication. DMA, bus targets, events, interrupts, journals, stop choices, multicycle work, traps, and fallbacks remain coordinator-only. | This preserves the existing one-cycle visibility window and serial failure boundary; the exceptional checkpoint cannot undo shared state because admitted commands cannot create any. It is real helper execution but not a claim that every strict-cycle instruction is parallel. Widen only with a reviewed latency proof and event/DMA differential oracles, not architectural speculation and rollback. |
| P3-D16 | Extending strict ready-cycle execution to reduced cores requires choosing cluster-resource latencies, same-cycle scratchpad visibility, and arbitration between multiple microcores sharing one physical main-bus port. | Element 5 retains the explicit full-core-only strict API and its pre-mutation rejection for any micro-core cluster. Mixed full/reduced execution remains available through the deterministic unbounded frontier coordinator established in Element 4. | Rejecting an unselected timing contract is more honest than deriving one from Python fallback costs. Reopen when the reduced cluster has an approved ready-cycle/latency contract and RTL-backed same-cycle oracles. |
| P3-D17 | “Record/replay” can mean the internal exactly-once bus journal, timestamped host-ingress replay, or unrestricted whole-machine replay. Entropy refills and NIC backend send outcomes are not presently journaled. | Version and transactionally install the UART/NIC/geometry ingress history into a fresh journal, preserve ingress sequence and delivery order, include rejected NIC attempts and both conditional and unconditional geometry responses, and seal every supported live façade. Keep the resumable bus-effect journal internal and describe it separately. Whole-run equality additionally requires identical deterministic entropy and backend behavior. | Element 5 claims external-ingress replay plus exactly-once suspended-instruction replay, not a complete machine transcript or snapshot. Reopen unrestricted replay only when every guest-visible nondeterministic source and egress outcome is recorded. |
| P3-D18 | An instruction can reach its retirement frontier at the same time as an instruction cap, caller cycle limit, event horizon, or terminal machine state. An early terminal shortcut can otherwise expose a pre-retirement clock or throw after committed core state; a core awakened at an in-call event can likewise retain a stale pre-idle ready timestamp. | Rebase unsuspended newly runnable work to the frontier that woke it. An armed instruction-stop frontier must then be reached before terminal classification. Event horizons retain precedence over tied caller cycle limits; a caller cycle limit retains the existing tie over an instruction cap; otherwise the instruction cap precedes all-halted/all-idle. A post-call event that invalidates `all_idle` reports `external_ingress`; an active unbounded machine that cannot retire reports `no_progress`, never an instruction cap it did not reach. | Ready-cycle and stop precedence are architectural and versioned across one, two, and four lanes. Reopen only through an explicit public timing/stop-contract change. |
| P3-D19 | A cycle stamp alone cannot distinguish input seen by the scheduler, input staged while a call was running, and input arriving after that call returned. Treating all three as scheduler-visible lets replay run past a live `all_idle` boundary. | Every record names `scheduler`, `before_batch`, or `after_batch` release phase plus a positive batch ordinal where applicable. Future exact events enter the scheduler at `(cycle, sequence)`. Immediate between-call events release before the next positive execution boundary and do not inflate that call's applied count. Concurrent staged events release after the current boundary, do count there, and can replace a now-stale `all_idle` with `external_ingress`. Explicit out-of-call clock progression exposes the earliest deferred pre-boundary cycle and cannot cross it; positive calls enforce release at the recorded boundary cycle. | Replay requires the same initial state, explicit clock progression, and sequence and arguments of positive `step`, unbounded-batch, or strict-cycle calls. It does not promise to contain a deliberately divergent execution call before that call mutates otherwise valid guest state. The phase is host-handoff metadata, not guest hardware state. Reopen only with a different public host-arrival linearization contract or a full call-transcript format. |

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
  microcore remains on the established serial path through Element 3.
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

## Element 4 contract

- The production frontier coordinator ranges over the complete global
  `execution_cores` topology. Full and reduced cores share one frozen cyclic
  reservation order, one mapping admission across all physical cohorts, one
  equal-credit round, and one coordinator settlement path. The old serial
  mixed scheduler and its sole-full-core special case are removed.
- The exposed `_run_private_full_core_commands` diagnostic remains deliberately
  full-core-only. Its internal typed commands/results and the production worker
  executor are profile-generic; reduced cores enter them only through the
  system coordinator.
- Reduced-core helpers admit only proven local supervisor instructions:
  IDL/NOP/HALT/DI, INC/DEC, ordinary short and long branches, immediates,
  scalar ALU, SEP/SEX, and Tier-1 bit operations, with one conservative
  ordinary modifier. EI, memory/MEMALU/I/O, CSR, MEX, MUL/DIV, CRC/SHA,
  reset/trap/return, stripped D-register operations, Tier-2 bits, extended
  engines, nonzero privilege, double/reserved prefixes, and EXT.SKIP remain
  coordinator or Python-oracle boundaries.
- Scratchpad-routed reduced-core instruction fetch never uses a raw native
  bank-zero alias. It yields from the helper, routes each actually decoded byte
  between the owning scratchpad and mapped RAM exactly like the Python oracle,
  and executes through that oracle. Any possible 16-byte decode window touching
  MMIO fails explicitly before decode because a classification read could have
  side effects. Reserved CRC/SHA operation codes likewise bypass cluster
  arbitration and reach their architectural illegal-operation trap.
- A mixed logical sub-frontier admits at most one instruction per reservation.
  Every physical cohort completes under the same mapping admission before the
  admission and its classification lease are released. Only then may the
  coordinator invoke Python or commit shared state. The same-frontier code
  observation rule is the explicit interim decision in P3-D10.
- Ordinary noncluster boundaries commit first in frozen cyclic order. The
  coordinator then recaptures live cluster requests, groups them by
  cluster/resource, freezes the equal round-robin choices, preflights the exact
  initially fundable grant set and aggregate grant sequence, and revalidates
  each selected request immediately before settlement. A changed opcode, PC,
  runnable state, or hard eligibility defers that selected core. Frozen
  membership, nonselected losses, and their credit donations do not change in
  later commit order.
- Cluster losers retire no instruction and consume no resource grant. They
  close one logical fallback dispatch, retain residual credit, and retry after
  progress. This includes a hard-ineligible request from a group with no
  selected peer. An unchanged all-zero request set closes without spinning;
  any released credit flows only forward. Funding a selected zero-credit
  request transfers exactly one unit from a cyclic-earlier frozen loser,
  including across cluster resources.
- A selected continuation commits the arbiter only after successful
  coordinator settlement and scheduler validation. The cluster checkpoint is
  restored on failure, so CRC/SHA ownership and engine state cannot be orphaned
  by a failing winner. Earlier commits and every gathered private prefix retain
  the failure semantics recorded by P3-D1 and P3-D9.
- Direct concurrent host-thread access, read or write, to exposed core fields
  or registers during an active native batch is not supported. Strict cycle
  execution, event-time acceptance, DMA/event integration, and record/replay
  closure remain Element 5 work.

## Element 5 contract

- Exact cycle execution retains one scheduler and one boundary order: advance
  cycle-driven devices, complete the already sampled bus target, apply
  timestamped external ingress, snapshot interrupt eligibility, then dispatch
  cores. NIC and disk DMA remain byte-wide equal peers on their physical
  ports; helper width cannot affect grant, token, completion, or fault order.
- At one strict scheduler cycle, the worker pool may execute only a cyclic
  prefix of full-core instructions proven cache-resident, callback-free, and
  exactly one cycle. Every physical cohort completes before scheduler
  accounting is published in cyclic order; an unexpected prepublication
  failure restores the complete private candidate set. Any prefix, shared
  access, multicycle cost, suspended operation, trap, or fallback stays on
  the established resumable coordinator path.
- The public diagnostic private-wave API and unbounded scheduler still reject
  active grants, suspended cycle work, pending timestamped events, and event
  horizons. Scheduler-owned strict commands are the sole internal exception;
  they cannot invoke Python or shared effects.
- The external-ingress recording format has an explicit schema version and
  records absolute cycle, contiguous ingress sequence, event kind, immutable
  payload, arguments, release phase, and positive batch boundary where
  applicable. Future exact events are scheduler-visible; immediate
  between-call input releases before the next positive boundary; input staged
  during a call releases after that boundary. Replay validates the complete
  recording before mutation, requires a fresh journal, batch-boundary history,
  and clean strict timeline, installs every owning queue atomically, enforces
  recorded boundary cycles when the same positive call sequence and arguments
  are replayed, prevents explicit clock progression across a deferred
  pre-boundary cycle, and permanently rejects later live ingress for that
  machine.
- The ingress recording is not a complete machine snapshot. Reproducing an
  entire run also requires the same image, initial state, deterministic
  entropy, and deterministic NIC backend outcomes. The internal bus journal
  separately guarantees exactly-once target effects while one instruction is
  suspended and replayed.
- Stop results name the authoritative stop cycle. An instruction-cap frontier
  is drained before all-halted/all-idle is considered, already owned bus
  targets finish without admitting new guest work, event-horizon ties retain
  precedence, and unhandled interrupts identify the first invalid core/vector
  without mutating it. A core awakened at an in-call frontier begins at that
  frontier rather than its stale pre-idle ready cycle. A post-call ingress
  handoff that wakes an otherwise all-idle machine reports
  `external_ingress`. Unbounded native results publish their actual
  instruction-limit, all-halted, all-idle, or no-progress reason and stop
  cycle.
- Strict reduced-core timing remains unsupported as recorded by P3-D16. This
  is not a regression in mixed architectural execution: the Element 4
  unbounded coordinator remains the production path for the advertised
  full/reduced topology.

## Element 6 contract

- The final architectural reference remains one total host-execution lane.
  Two- and four-lane results must match it for the same initial state and
  public invocation sequence: complete captured architectural state, ordered
  shared traces, per-core and authoritative system cycles, instruction and
  dispatch accounting, stop reason/cycle, interrupt and external-ingress
  results, and the documented callback-failure state.
- Host-only worker identity, command counts, physical cohort count, wall time,
  process CPU utilization, and helper completion order remain outside every
  architectural hash. Lane diagnostics must nevertheless prove that each
  configured lane receives eligible private work; host CPU utilization and
  timing are reported separately as evidence of useful overlap, not as
  architecture.
- The existing versioned Phase 0 workload harness is refreshed rather than
  forked into a duplicate benchmark. Its report gains an explicit 1/2/4
  `worker_count` dimension, physical-lane provenance, cross-width equivalence
  validation, complete current ingress-journal diagnostics, fixture hashes,
  and a schema bump. Guest core count and host lane count remain separate
  axes.
- Sanitizer artifacts are built outside the normal in-tree extension and run
  through the same mutually exclusive foreground supervisor as ordinary
  tests. Address/undefined-behavior and thread-sanitizer evidence are separate;
  neither may be inferred from an ordinary optimized run, and an unsupported
  sanitizer runtime must be reported rather than treated as a pass.
- Performance is diagnostic. The final report uses a clean optimized artifact,
  fixed workload and instruction budgets, discarded warmups, repeated samples,
  exact artifact and fixture provenance, guarded cross-width state equality,
  and one-lane-relative ratios. No minimum speedup or universal scalability
  claim is added.
- The completion handoff versions the exact commit chain, overlapping
  checkpoint evidence, final equivalence and sanitizer gates, benchmark
  hashes, prior Akashic SR2 compatibility observation, all design contentions,
  and remaining limitations. Generated JSON remains reproducible evidence
  rather than a large tracked artifact.

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

## Element 4 evidence

The generalized production coordinator rebuilt successfully with the same
C++17/pthread configuration. The measured final build peak was approximately
1.17 GiB RSS with no swap activity; its only compiler diagnostic was the
pre-existing unused `exec_field` warning. The final foreground evidence ran
sequentially in two owned pytest processes:

- 33 reduced-core and cluster integration oracles covering retained Phase 2
  traces, mixed-frontier code observation, coordinator-only settlement, MEX,
  MUL/DIV, CRC/SHA ownership, hard eligibility, equal round-robin contention,
  cross-resource forward credit, exact prefix/MEX request identity,
  scratchpad-boundary routing, MMIO-window rejection, reserved operations,
  callback failure, and one/two/four-lane equality; and
- 168 coordinator, private executor, worker-pool, native microcore, cluster
  oracle, and native system-state regression tests.

The 201 tests passed. Their measured peaks were approximately 49 MiB and
55.4 MiB RSS, respectively, with no swap activity. Two independent final
read-only audits reviewed the frozen-arbitration, hard-eligibility,
work-conservation, failure-accounting, routed-fetch, and decision-ledger
boundaries after the discovered blockers were corrected.

A supplemental 34-test concurrency-handoff gate also passed during Element 4,
but unexpectedly peaked at approximately 7.54 GiB RSS. It was not repeated
after the final narrow arbitration corrections; those paths are covered by the
focused reduced-core and native-system gates above. Repeating that handoff gate
requires explicit resource approval.

Element 4 establishes deterministic mixed full/reduced-core execution and
cluster-resource arbitration. It does not yet claim strict event-time/DMA
integration, record/replay closure, sanitizer completion, or final application
throughput; those remain Elements 5 and 6.

## Element 5 evidence

The strict-cycle helper integration and versioned ingress replay rebuilt
successfully with the established C++17/pthread configuration. The final build
peaked at approximately 1.18 GiB RSS with no swap activity; its only compiler
diagnostic was the pre-existing unused `exec_field` warning. The dedicated
Element 5 file contributed 25 event, DMA, stop-boundary, replay-validation,
release-phase, clock-horizon, façade-sealing, counter-wrap, and
one/two/four-lane oracles.

The final owned foreground gate covered:

- all 25 Element 5 event-execution oracles;
- all 54 native strict-cycle execution tests;
- all 110 private, coordinator, reduced-core, and persistent worker-pool tests;
- all 94 native system-state and bus-transaction tests; and
- all 41 snapshot, display-concurrency, native-microcore, and cluster-oracle
  regressions.

The 324 tests passed sequentially in one pytest process at approximately
72.1 MiB peak RSS with no swap activity. Three independent read-only audits
reviewed the strict one-cycle classifier and rollback boundary, stop
precedence, replay queues and validation, clock horizons, geometry transition
coverage, façade sealing, and the decision-ledger claims. Their concrete
findings were corrected before this evidence run, and the final audits found no
remaining blocker.

Element 5 establishes deterministic strict-cycle helper participation for the
proven one-cycle private subset, coordinator-owned event/DMA ordering,
exactly-once suspended target effects, scoped external-ingress replay, and
authoritative stop reasons and cycles. Replay remains deliberately bounded by
P3-D17 and P3-D19; strict reduced-core timing remains deliberately unsupported
by P3-D16. Sanitizer stress, the final one/two/four-lane equivalence package,
refreshed performance measurements, and the versioned completion handoff
remain Element 6 work.
