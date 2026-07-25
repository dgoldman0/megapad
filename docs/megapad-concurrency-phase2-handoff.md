# Megapad deterministic-concurrency handoff: Phase 2 snapshot

**Snapshot date:** 2026-07-25

**Status:** Phase 2 complete within the frozen architecture boundaries below; Phase 3 has not started

**Branch:** `feature/megapad-deterministic-concurrency`

**Isolated worktree:** `.worktrees/megapad-concurrency`

**Oracle revision:** `16acec50ef2bd74f25848eb4af6335d2ccbc62e0`

**Initial reviewed revision:** `bf0d05defc4102151ad83c9d9b30822e02dd0a96`

## Purpose and custody

This is the versioned resumption point for the concurrency project after
completion of Phase 2. It records the implemented scope, architectural
decisions, evidence, and remaining boundaries without importing or modifying
the workspace-root `MEGAPAD_CONCURRENCY_HANDOFF.md`. That orientation document
remains local, read-only source material.

All work in this snapshot is local Git work. Nothing was pushed. The isolated
worktree was used so commits could be recovered after session and system
crashes without touching the main checkout.

## Frozen architecture decisions

- The advertised topology remains four full cores with global IDs 0--3, plus
  three four-microcore clusters whose microcores have global IDs 4--15.
- Hard QoS determines which requests must or may be served and their reserved
  entitlement. Simultaneously eligible physical peers are ordered by equal
  round-robin. Unused reserved capacity remains work-conserving.
- There are no best-effort weights, aging rules, adaptive priorities, or other
  secondary ordering biases in the emulator contract.
- The integrated SoC ties QoS programming off, resets its hardware weight
  registers to all ones, and resets bandwidth limits to zero, meaning
  unlimited. A generic weighted RTL testbench mode is not architectural policy.
- `CLUSTER_EN` resets to all ones. Every configured cluster is enabled after
  construction; explicit clears hold a cluster in reset, and a later enable
  releases its reduced cores from PC zero.
- Full cores have private, noncoherent guest instruction caches. A core's own
  completed write invalidates its matching private line. Other cores, cluster
  writers, DMA, and direct host writes do not snoop it; visibility requires
  explicit guest or host invalidation.
- Host scheduling must never choose a guest-visible winner. The one-worker
  transaction model orders shared effects now; deterministic host workers are
  Phase 3 work.

## Phase status

Phase 0 pinned behavior, explicit exclusions, deterministic state/oracle
hashes, and diagnostic baselines in `bench_phase0_concurrency.py` and its test
suite. Phase 1 moved scheduling, mappings, shared time, requester identity, and
architecturally singleton native devices under one `SystemState`, ending with
native one-worker batch scheduling in `99a2979`.

Phase 2 was kept at seven elements:

| Element | Outcome | Commits |
|---|---|---|
| 1 | Added immutable nine-port main-bus transactions and equal round-robin arbitration; repaired the RTL nine-port scan and made legacy runners reject an active grant safely. | `c76e86b`, `a04c9fa`, `6bab640` |
| 2 | Added resumable, cycle-bounded full-core execution with checkpoints, exactly-once access journals, persistent grants, and explicit event/instruction/cycle stop boundaries. | `4669ba8` |
| 3 | Integrated deterministic timers, interrupts, and journaled external events; made incomplete legacy snapshots fail closed and made reset boundaries timeline-safe. | `06f553d`, `c82c521` |
| 4 | Added resumable NIC and disk DMA endpoints and then routed production DMA through them with equal peer ordering, exact terminal publication, reset safety, and a versioned strict-contention oracle. | `64494d0`, `5c9f30c`, `b9d06fc`, `90f712c` |
| 5 | Gave every microcore stable native ownership, accelerated the bounded scalar subset behind transactional fallback, fixed the Python REX oracle, removed a test ingress race, and versioned the retained single-active-microcore baseline. | `599b8fb`, `c17af40`, `cd580e4`, `bd701b0`, `e8ca274` |
| 6 | Brought all 16 advertised cores under the native scheduler; added cluster-local arbitration and canonical shared CRC, SHA, tile/MEX compatibility, and scratchpad state; wired cluster enable/reset behavior; and versioned the all-core cluster oracle. | `0bc6144`, `4805966`, `e33eaad`, `67f8bea`, `b7c668b`, `8ff3beb`, `359ab74` |
| 7 | Implemented the full-core private guest I-cache, safe accelerator-hook identity checks, strict host-mutation boundaries, writer-local invalidation, ACK-safe/fair RTL tile routing, synthesis/evidence repairs, and the versioned cache oracle. | `267c8e0`, `99d3243`, `538c109`, `3320e47`, `16acec5` |

Two RTL exception defects discovered after element 4 were repaired as a
separate corrective checkpoint rather than folded into a later element:
`f7e8327` fixed trap-frame entry/return and `79e17b9` separated illegal-op and
divide-by-zero vectors.

## Phase 2 element 7 oracle

`bench_phase2_icache.py` is both a behavior oracle and a diagnostic benchmark.
Its hashes include the complete fixture manifest and deterministic virtual
cycle state, but exclude host timing. It covers:

- native/Python cache geometry, control, reset, same-line hits, disabled
  bypass, private noncoherence, and explicit invalidation;
- full physical tags through a same-index conflict between address zero and
  `0x100000`;
- strict two-beat refill journaling, rejection of an official load during a
  suspended instruction, and deliberately noncoherent official loading after
  suspension;
- exact accelerator-hook code-span identity, including a mutation of the last
  registered byte; and
- architectural terminal-state equivalence between a prewarmed cache and a
  disabled cache, with every timed mode starting from a fresh CPU.

The final default report was generated from the clean oracle revision:

```text
python3 bench_phase2_icache.py \
  --output /tmp/megapad-phase2-icache.json
```

Configuration and results:

| Field | Value |
|---|---|
| Generated | `2026-07-25T21:56:07.103227+00:00` |
| Host | Python 3.13.7, Linux 6.8.0-124-generic x86_64, glibc 2.39 |
| Sampling | 1 warmup, 3 measured repeats, 200,000 instructions per mode, alternating order |
| Canonical-state SHA-256 | `b0bb4266f1fd36a73722cbadad17e2f109eac3f77579b4cc3e44f74de35ccffe` |
| Behavior-oracle SHA-256 | `a4881ca5a590f9503fb3cd81fa7f55b6808164b1c37071f426671d4cd6446013` |
| Hot-cache median | 91,110,270 instructions/s |
| Disabled-cache median | 45,322,926 instructions/s |
| Hot/disabled median ratio | 2.0102468545 |

All four determinism booleans and all six validation booleans were true. The
ratio is diagnostic only; there is no performance pass threshold.

The exact loaded accelerator was
`_mp64_accel.cpython-313-x86_64-linux-gnu.so`, 1,946,240 bytes, with SHA-256
`f9215bdb63ed7c7abf2dd14336a752f340793d413d079a11387e7af06f336498`
and ELF build ID `ec8510fe0b7b105d743426169d33a729cdf19985`.
The generated JSON is intentionally not versioned; the command above
reconstructs it with current provenance.

## Final sequential evidence

Tests were kept sequential throughout. The final element-7 checkpoint passed:

- 23 Python/native cache and versioned-oracle tests;
- 75 adjacent accelerator-hook, string-safety, and strict-cycle safety tests;
- 86 RTL private-I-cache assertions;
- 8 RTL full-core local bus-mux assertions;
- 41 RTL tile-port arbitration assertions;
- 58 legacy tile assertions and 34 delayed-ACK/payload assertions;
- 7 integrated tile-write/I-cache assertions;
- 7 SHA-2 assertions;
- 29 mailbox assertions;
- 35 reduced-cluster assertions;
- 28 CPU smoke assertions;
- 5 full-SoC smoke assertions;
- full-SoC Icarus elaboration;
- default-parameter Yosys hierarchy/source-closure validation; and
- Genesys 2 Tcl syntax and canonical source-path validation.

The final oracle review independently confirmed that hot/disabled semantic
equivalence, full fixture hashing, repository provenance, exact native-artifact
provenance, evidence routing, and bounded claims were all present. The final
RTL review confirmed packed SHA-2 word ordering and source-manifest closure.

## Deliberate exclusions and remaining limitations

- Phase 3 has not started. There is no persistent deterministic host worker
  pool and no claim of simultaneous host execution yet.
- Cluster contention is deterministic at the implemented cooperative
  boundaries, but the element-6 oracle does not claim full same-cycle RTL
  latency for every reduced-core shared resource.
- The guest I-cache is intentionally noncoherent across cores, clusters, DMA,
  and host writes. Direct backing-buffer mutation remains an explicit unsafe,
  noncoherent host seam.
- A general host decoded/JIT cache is absent and remains Phase 4 work.
  Registered accelerator hooks instead snapshot and verify their exact guest
  code span.
- Native BIST currently reports an immediate pass while Python and RTL BIST
  paths perform destructive memory checks. Cross-backend BIST postconditions
  are not claimed.
- RTL `LOAD2D`/`STORE2D` external unified-memory routing remains unresolved,
  and the current external tile PHY path truncates addresses above 32 bits.
- Full FPGA synthesis, timing closure, and place-and-route were not run.
  Source closure, hierarchy, elaboration, and Tcl paths were checked.
- The original mailbox, wide-MMIO, and full-core tile/MEX source conflicts
  remain architecture/errata boundaries unless a separate decision supersedes
  them. Concurrency work must not silently choose new guest-visible behavior.
- Legacy native snapshot format v1 is rejected for the new system rather than
  pretending it can restore clocks, grants, journals, continuations, and
  authoritative shared devices.

## Resume point

The next planned milestone is **Phase 3: deterministic worker pool**. Begin from
the commit containing this file on `feature/megapad-deterministic-concurrency`;
`git log -1 -- docs/megapad-concurrency-phase2-handoff.md` identifies the
snapshot commit.

Phase 3 should add persistent native workers only for independently executable
segments, retain a one-worker reference path, and serialize every shared commit
through the deterministic coordinator. The acceptance comparison remains
one-, two-, and four-worker equality of architectural state, ordered shared
trace, virtual completion cycles, and stop/exception state. Record/replay and
sanitizer stress belong to that phase; they must not be replaced by throughput
alone.

Continue to run smoke, integration, persistence, and sanitizer suites
sequentially. Do not push this local branch until the work is deliberately
moved back to the main checkout and reviewed for publication.
