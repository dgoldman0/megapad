# Megapad deterministic concurrency: Phase 3 plan

**Started:** 2026-07-26

**Status:** Elements 1–2 of 6 complete; Element 3 has not started

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

## Fixed phase structure

Phase 3 is divided into six elements. These identifiers are stable; discoveries
may produce corrective commits, but they do not create new phase elements.

| Element | Scope | Status |
|---|---|---|
| 1 | Persistent worker-pool lifecycle and fixed 1/2/4-lane configuration | Complete |
| 2 | Private full-core execution commands, results, and deterministic yield boundaries | Complete |
| 3 | Full-core coordinator integration and ordered shared-effect commit | Not started |
| 4 | Reduced-core and cluster integration | Not started |
| 5 | DMA, external events, record/replay, and deterministic stop handling | Not started |
| 6 | One/two/four-lane equivalence, sanitizer stress, refreshed benchmarks, and final handoff | Not started |

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
