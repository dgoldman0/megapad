# Megapad deterministic concurrency: Phase 3 plan

**Started:** 2026-07-26

**Status:** Element 1 of 6 complete; Element 2 has not started

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

## Fixed phase structure

Phase 3 is divided into six elements. These identifiers are stable; discoveries
may produce corrective commits, but they do not create new phase elements.

| Element | Scope | Status |
|---|---|---|
| 1 | Persistent worker-pool lifecycle and fixed 1/2/4-lane configuration | Complete |
| 2 | Private full-core execution commands, results, and deterministic yield boundaries | Not started |
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
