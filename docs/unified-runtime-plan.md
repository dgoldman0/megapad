# Unified MegaPad application and native execution plan

Started: 2026-09-30

Status: The unified emulator/simulator/hybrid application is implemented and
qualified locally; hybrid mode now uses the single routine path described in
[`hybrid-runtime.md`](hybrid-runtime.md). Shared native scalar
FP, tile values, Keccak, checked SHA3 input transfers, NTT bulk transfers and
bulk audio are qualified. The hardened AES transfer candidate showed no gain
against a fresh scalar baseline, so existing AES routing is retained. Native
page access and longer continuation intervals need no further rewrite on
current evidence. Kernel/source/captured-frame evidence and strict multicore
FP/timing checks are recorded. Prepared Desktop journeys are complete in the
documented direct-dispatch scope, with Python requiring expanded diagnostic
deadlines. Final selected acceptance of the original implementation passed on `0205541`:
2,466 application checks with three environment-dependent socket skips, 2,569
simulator checks under each explicit executor, and eight production-source
rich-terminal checks. The pinned retained frontend changes are integrated and
qualified locally. Production semantic sessions default to required native
execution; explicit CLI/environment choices and Python embedding defaults remain
available. The approved local scope is complete within these documented limits.

Branch: `feature/unified-runtime`

Reviewed base: `a0175487815afb3fa2003014eabf1b1e2c0ab5a1`

## Goal

Deliver one MegaPad application and build with emulator, simulator, and
hybrid execution modes. Reuse the two existing execution engines and common
session/frontend infrastructure. Move measured hot computation and runtime
bookkeeping from Python to native code while retaining exact reference
models and the existing compatibility contracts.

The user authorized local branch work, beginning with a local commit of this
plan. The 2026-09-30 continuation authorizes work through the remaining roadmap
with regular local commits. Implementation and evidence stay in reviewable
slices on this branch. Reconcile reviewed parallel MegaPad changes on an
isolated branch without modifying the other team's active worktree. Publishing
and final integration into main await authorization; Akashic remains untouched.

## Starting evidence

- `_mp64_accel` already owns production MP64 batch execution, the native
  system scheduler, DBT, and several device models. Ordinary production
  emulation is not a Python instruction loop.
- `_megaforth_native` already executes semantic arithmetic, branches,
  ordinary memory, colon calls, loops, and selected bulk primitives. Its
  page maps and continuation metadata still contain Python objects and its
  bounded native intervals hold the GIL.
- Scalar FP32/FP64 `FC` instructions in the accelerated emulator rewind into
  the Python oracle through `EXT_ISA_FALLBACK`, with CPU-state synchronization.
  Hosted scalar FP uses the same Python value model. This is the strongest
  concrete shared numerical acceleration candidate.
- Hosted tile operations still use Python services and value operations.
  The emulator already has substantial native tile computation suitable for
  carefully extracting backend-neutral value kernels.
- Both server programs use `SessionServer`. `SimulatorMachineSession`
  inherits terminal functionality from `MachineSession`, but the common root
  session modules still import architectural implementation. The frontend
  boundary is only partly extracted.
- The simulator dictionary gives each word an eight-byte semantic code slot
  whose implementation is host metadata. Its return stack uses typed
  continuation cookies. Neither is an executable native dictionary image.
- Hosted memory is sparse Python page backing; the emulator's native memory
  router uses pinned region buffers. Equal address geometry does not make
  these interchangeable backing stores.
- Native semantic sessions currently default to 65,536-step host quanta;
  Python reference sessions use 8,192. Previous Desktop profiles show both
  significant native guest work and host frame/boundary costs. The September
  29 partial-repaint implementation and September 30 idle changes must be
  present in any new baseline; historical timings are not current benchmarks.

Source evidence: `docs/simulator-contract.md`,
`docs/simulator-native-execution.md`, `simulator/accel/API.md`,
`docs/single-core-execution-kernel-plan.md`,
`docs/megapad-full-float-plan.md`, `docs/viewer-partial-repaint.md`, and
`docs/megapad-idle-plan.md`.

## Locked architecture decisions

1. **One application, two execution engines.** Emulator and semantic engines
   remain separate implementations. Hybrid coordinates them; it does not
   introduce a second MP64 decoder in `simulator/`.
2. **Composition selects execution.** A top-level application/composition
   layer owns mode selection and hybrid transitions. Preserve
   `emulator -> shared <- simulator`; shared codecs/value kernels do not
   import either backend or decide which engine runs.
3. **Mode, implementation, and timing are distinct.** Product mode is
   emulator/simulator/hybrid. Python/native/DBT describe implementation.
   Emulator cycle policy and RTC policy are separate. Status must report
   actual capabilities and execution, including unsupported features.
4. **Build both engines coherently.** Provide one user build command and a
   common source revision for the native extensions and Python adapters.
   Keep focused development builds possible. A native-production default
   requires explicit selection and parity qualification; the first launcher
   slice preserves existing defaults.
5. **Python remains useful.** Retain independent Python oracles, tests,
   configuration, diagnostics, and cold orchestration. Porting file counts
   is not a performance objective.
6. **One owner of hybrid state.** A hybrid session owns one backing memory
   image and one instance of each admitted service's state. Both execution
   views share these through explicit interfaces. Whole-address-space copies
   at each transition are not the design.
7. **Explicit machine entry.** Begin with declared machine routines and
   executable regions. Unknown, stale, or invalid XTs remain errors. Semantic
   dispatch never guesses that an arbitrary address is executable code.
8. **A versioned transition ABI.** Specify registers, selectors, stack
   geometry, preserved state, return trampolines, continuation ownership,
   errors, suspension, and reentry before hybrid execution is enabled.
   Preserve the ordered return stack, user cells, loop frames, and `RP!`
   captures. `R14`/`R15` compatibility alone is insufficient.
9. **Mutation has explicit consequences.** Define dictionary publication,
   rollback, XT reuse, code writes, semantic-plan invalidation, and native
   code identity together. Preserve the MP64 instruction cache's intentional
   noncoherence; backing-memory writes do not automatically make new bytes
   instruction-visible. Bind registrations to allocation/dictionary
   generations. Unsupported mutation must fail with a defined result.
10. **Three clock domains remain distinct.** Semantic work/timer accounting,
    MP64 cycles, and RTC uptime/epoch retain explicit ownership. Interactive
    RTC may follow host monotonic time. Define hybrid event admission and
    clock advancement without treating semantic steps as hardware cycles.
11. **Claims follow the selected profile.** The initial hybrid is one-core
    functional interoperability with bounded machine routines. It does not
    claim full-system cycle equivalence, multicore races, arbitrary ROM boot,
    or native snapshot compatibility.
12. **Native computation preserves observable behavior.** Shared kernels
    own values; adapters retain memory effects, ownership, transactions,
    faults, clocks, and publication. Preserve partial effects and fault order,
    FP rounding/flags, NaNs, subnormals, reduction order, and fused operations.
13. **Native state has a deliberate lifetime.** GIL-free execution requires
    native-owned or safely pinned memory and metadata plus mutation exclusion.
    Removing the GIL while traversing mutable Python dictionaries is not an
    admissible optimization. Host input is admitted at declared boundaries.
14. **Hybrid machine-code support and guest JIT are separate milestones.**
    Existing semantic `JIT-ON`/`JIT-OFF` remain their documented no-ops until
    native compiler/dictionary integration is implemented and qualified.
15. **General binary compatibility needs native authority.** A later hybrid
    configuration supporting arbitrary native dictionaries and binaries
    should use a native machine image as authority and accelerate qualified
    regions semantically. The initial semantic profile cannot silently
    promote itself into that capability or hot-switch at an arbitrary point.
16. **Evidence governs optimization.** Profile current representative work,
    change one coherent mechanism, and compare equivalent unprofiled runs.
    Record slower results and remaining limitations. Kernel improvements do
    not by themselves establish Desktop or full-program speedups.
17. **Reference precedence remains intact.** Locked source-compatibility
    decisions control where specified; architectural differentials cover the
    remaining admitted observations. Host diagnostics, generated code, and
    cache generations stay outside canonical guest state and snapshots.

## Phase 1 — Application and build consolidation

Preserve execution semantics while establishing the user-facing entry point.

### 1A. Unified launcher and build

- Add a single shared-session launcher with an explicit mode selector for
  implemented backends. Mode-specific help and argument validation must
  retain existing memory, storage, terminal, clock, and lane controls.
- Keep backend selection lazy enough that top-level help and invalid-mode
  diagnostics do not need compiled extensions or start a session.
- Consolidate server argument construction into callable interfaces. Reuse
  existing preparation/owner paths; do not duplicate boot or execution logic.
- Add one build target that builds both native extensions sequentially and
  fails when either fails. Existing focused targets remain available.
- Retain existing launchers as temporary entry points while callers migrate.
  Do not advertise hybrid as executable before its acceptance gate passes.
- Qualify argument forwarding, selected-backend startup and shutdown,
  mode-specific failures, help, and existing server behavior with focused
  sequential tests. This slice needs no full Desktop performance claim.

### 1B. Common session boundary

- Extract terminal/session configuration, display ownership, shared control
  protocol, and lifecycle interfaces from architectural construction.
- Move backend-specific construction behind emulator/simulator adapters.
- Preserve socket ownership, terminal acknowledgements, input ordering,
  media claims, reset/close behavior, and runtime namespace isolation.
- Unify capability/status reporting and make the native production selection
  explicit after qualification. Reference execution remains selectable.
- Migrate supported consumers and remove obsolete entry-point bridges when
  the dependency cluster is complete. Avoid parallel permanent APIs.
- Package server implementations in `emulator.server`, `simulator.server`,
  and `hybrid.server`. Keep the old root server scripts as temporary
  `main`-only forwarders until their separately maintained external callers
  migrate. This repository does not modify Akashic; retaining those script
  entrypoints is an explicit external compatibility boundary, not a second
  implementation or a permanent programmatic API.

Gate: both existing modes run through one application with unchanged
source-visible behavior and honest mode-specific capabilities.

## Phase 2 — Current workload profiles

Establish bounded, reproducible baselines for source loading, Desktop
interaction, scalar FP, tile computation, cryptographic services, and audio
transfers. Reuse the current benchmark and observation infrastructure.

Record source/native-binary identities, actual executor, guest work, host
wall/process time, native entry and exit counts, Python fallback reasons,
state-synchronization cost, frame processing, and peak memory as appropriate.
Profile attribution separately from unprofiled throughput/latency trials.
Use small deterministic kernels before a more expensive representative run.

Gate: every proposed optimization names a measured workload and an
independently checkable compatibility result. The observed scalar-FP
fallback mechanism justifies preparing its extraction before all workload
families have been profiled.

### 2B. Multicore timing qualification — math-team finding

The user supplied a math-team report on 2026-09-30: instruction-batched
multicore execution makes worker wake appear near 130,000 cycles, versus about
1,000 under the shared-clock model; four-core explicit work scales only
1.3–1.4× under modeled bus contention; scalar FP64 previously blocked the
strict-mode solver. Those workload figures are reported observations, not
independently reproduced solver measurements in this branch. Work stays within
MegaPad; the team's separately edited Akashic sources are outside scope.

The exact native FC kernel removes the known full-core FP Python fallback. A
real one-core SystemState strict-cycle division check already proves 30+1-cycle
retirement. Extend this to 1/2/4 full cores, distinct FPCSR state, cold and warm
instruction fetches, sliced versus whole execution, shared bus traffic and
representative BIOS F64 word calls. Full-core strict execution and clustered
microcore support remain separate capabilities. Qualify the actual solver only
when its reproducer is available; kernel tests do not establish solver support.

Separately, make timing-model identity explicit in run results/status and
benchmark evidence. Instruction-batched functional cycle accounting must not
be presented as shared-clock wake latency or multicore scaling evidence. Add a
bounded wake/IPI and contention fixture that reports instructions, per-core
work, elapsed model cycles, selected timing model and host wall time separately.
Use the strict shared-clock path for modeled latency/scaling comparisons; retain
the fast path's established functional behavior and documented accounting.

The gate is reproducible wake/accounting classification, strict FP retirement
and cross-core state parity, and honest measured contention. Moving work into
C++ improves host execution cost; it must not erase modeled bus waits or force
a nominal fourfold result. Any remaining contention optimization needs its own
architectural equivalence and paired evidence before being credited.

## Phase 3 — Shared native computation and hot runtime state

### 3A. Scalar floating point

- Introduce a backend-neutral native FP value kernel for the existing scalar
  operation contract, with exact result bits, relations, and exception flags.
- Retain `shared/ieee_fp.py` and `shared/scalar_fp.py` as independent oracles.
- Wire emulator `FC` directly to the native kernel, removing whole-CPU
  Python fallback for admitted operations while preserving instruction
  length, traps, FPCSR state, and cycles.
- Wire hosted FP to the same value kernel; admit hot words directly to native
  semantic execution where identity, state, and failure ordering allow it.
- Qualify all modes, edge classes, and seeded differential cases. A host
  double implementation alone is insufficient for the complete contract.
  Cover reserved rounding modes, signaling/quiet NaNs, signed zero, tininess
  after rounding, FMA/FMS single rounding, conversion saturation, FP32 upper
  bit clearing, sticky flags, and instruction fault-PC behavior.

### 3B. Tile and other value services

- Extract reusable native tile arithmetic from the emulator and connect
  hosted services without importing architectural CPU state.
- Preserve per-format legality, fixed reduction structure, TACC accumulator
  representation, and adapter-owned memory/fault effects.
- Profile-driven follow-ups include AES/GHASH, Keccak, field arithmetic,
  ML-KEM, and the existing generic NTT. Generic NTT and standardized PQ NTT
  retain distinct contracts. SHA-2 hashing already delegates to `hashlib`;
  measure surrounding transfers before replacing computation.

### 3C. Boundaries and transfers

- Move measured page lookup and continuation bookkeeping into suitable
  native structures, preserving observable stack bytes and mutation checks.
- Extend native semantic coverage according to actual exit profiles.
- Add qualified bulk audio/memory transfers; retain effect-preserving
  fallback for special mappings and faults. Reuse existing storage spans.
- Profile the current incremental compositor and terminal projection/wire
  pipeline before selecting native raster or serialization work.
- Keep parsing/source compilation lower priority unless loading profiles
  make it a leading cost. Treat host SIMD and other acceleration as optional
  exact kernels behind the same contracts, not altered guest arithmetic.

Gate for each slice: equivalent results and effects, applicable timing and
accounting parity, focused regression coverage, and workload-specific paired
performance evidence. No universal speedup is promised.

## Phases 4 and 5 — Hybrid execution

Hybrid mode runs Forth semantically and declared MP64 routines on a native
core that shares its memory and its return stack. The first implementation
grew five manifest versions and four native transports. The integration
cleanup replaced them with one manifest, one routine runner and callbacks
that run on the caller's stacks. The current design is in
[`hybrid-runtime.md`](hybrid-runtime.md), and the cleanup record is in
[`integration-cleanup-plan.md`](integration-cleanup-plan.md). Running
compiled Forth or BIOS code as machine code is the separate staged design in
[`hybrid-native-dictionary-plan.md`](hybrid-native-dictionary-plan.md).

### Local completion and deferred scope

The approved local work is complete at the qualified activation checkpoint
`0205541`. Both native engines rebuilt successfully and the final application,
Python-selected simulator, native-selected simulator and production-source
rich-terminal gates passed serially. The
[acceptance report](unified-runtime-acceptance-2026-09-30.md) and its JSON record
retain exact selectors, source and binary identities, outcomes and the three
existing socket skips. The current plan and ledger reflect those results.
Existing benchmark reports retain their original measured revisions; this
functional regression gate does not remeasure them. Remote publication and
integration into main remain outside the current local-work authorization.

The following work remains explicitly deferred:

- Implementation of the staged native dictionary/compiler plan, including
  execution-map/module stages, compiler overlays, defining-word/JIT integration,
  native image authority and arbitrary executable dictionaries. The requested
  design is complete; semantic `JIT-ON`/`JIT-OFF` keep their existing behavior.
- The external math solver's latency/scaling reproducer. Its original reported
  figures remain unverified here; local strict FP and timing fixtures do not
  establish solver behavior, and Akashic remains untouched.
- General self-modifying/native images, complete cross-profile snapshots,
  multicore hybrid execution and live conversion of state authority. New
  optimization work also requires current measured benefit; the declined AES
  candidate does not become a required implementation task.

### Continuation checkpoints — 2026-09-30

The following entries are historical intermediate checkpoints. The current
status and implementation ledger include their later completed follow-ups.

- Shared native tile values and per-operation identity admission are qualified.
  The first Python-guard extraction regressed the FP64 add probe; moving the
  exact checks into the binding reduced its median from the 83.986 ms baseline
  to 65.133 ms for 4,096 operations. Python control variation and exact scope
  are recorded in
  [`performance/runtime-tile-values-2026-09-30.md`](performance/runtime-tile-values-2026-09-30.md).
- Removed the final root session import shim after migrating the benchmark's
  current/legacy layout selection. Its ownership/provenance gates passed.
- Added a MegaPad-owned bounded prepared-image Desktop harness. Its 28 checks
  include small production simulator/Python, simulator/native and hybrid/native
  journeys. The prepared eight-step Desktop journey subsequently passed with
  simulator/native (31.111 seconds to ready, 36.340 seconds overall, 12 offers,
  clean shutdown and original image preserved). Other mode/executor runs are
  pending; direct dispatch and SDL dummy do not qualify socket transport or
  physical output.
- Qualified checked SHA3 input routing: 59 new checks, 47 hosted checks per
  executor and 86 native/differential checks passed. For 256 transactions of
  256 bytes, median host wall time fell from 392.772 to 292.855 ms with Python
  and from 220.225 to 116.830 ms with native semantic execution. See
  [`performance/runtime-sha3-input-transfer-2026-09-30.md`](performance/runtime-sha3-input-transfer-2026-09-30.md).

## Validation and work discipline

- Work only in this isolated branch/worktree; preserve main and other active
  worktrees. Record plan changes before implementing a changed contract.
- Use the repository Makefile test entry points. Run tests and heavy builds
  sequentially; read-only review can be delegated independently.
- Set `MP64_RUNTIME_NAMESPACE` for this worktree. Use temporary disk images
  and distinct sockets for smoke tests; never attach another session's media.
- Keep correctness, host performance, guest timing, and physical-display
  acceptance as separate evidence categories. Use the relevant existing
  gates rather than claiming a unit test proves a complete application.
- Preserve existing caller resource bounds, watchdogs, semantic budgets, and
  architectural scheduling cadence. Broaden a test run only to resolve a
  concrete risk or satisfy the applicable integration gate.
- Commit the plan first, then coherent implementation slices with the checks
  actually run and their outcomes recorded below. Local commits are the
  checkpoint; no remote publication is part of this authorization.

## Implementation ledger

This table records the latest qualified state. The dated implementation notes
below preserve individual checkpoints; their references to then-pending gates
are historical unless repeated in the current status or deferred-scope section.
Test counts describe their named selectors and must not be summed as distinct
coverage.

| Slice | Status | Evidence |
|---|---|---|
| Plan | Locked | Read-only source review at the base above; local commit `e95bcc9` |
| 1A — unified launcher/build | Complete | Both engines built; 32 launcher/bootstrap checks, 20 native-selected bootstrap checks, 11 emulator lifecycle checks |
| 1B — common session boundary | Extraction complete; production native default and pinned frontend integration qualified | Original boundary gates plus 30 default-selection cases and the 317-check application gate; merged frontend passed 1,346 checks with three socket skips and another 30 default-selection checks; all three native-selected prepared Desktop modes passed |
| 2 — workload profiles | Kernels, KDOS controls/attribution and prepared Desktop subsets recorded | 56 Desktop harness checks; eight-step emulator/native, simulator/native and hybrid/native journeys passed through direct session dispatch; Python completed the identical assertions in 758.105 seconds under explicit expanded diagnostic deadlines, exceeding the standard step bound |
| 2B — math-team timing qualification | Timing identity, strict multicore FP and bounded wake/contention qualified; external solver unavailable | 8 timing-model cases; 13 strict FP cases; 19 timing harness cases and 48 measured cases |
| 3A — native scalar FP | Shared exact kernel and direct semantic words complete | 200 kernel/machine/adapter checks; 399 direct-FP/native/reference checks; paired FP measurements |
| 3B/3C — remaining native extraction | Bulk audio, shared Keccak, SHA3 and NTT transfer work qualified; AES candidate declined on current measurements | Existing oracle gates plus 109 NTT checks; 200 AES candidate checks with no measured speed benefit; retained raw comparisons |
| 4–5 — hybrid execution | Replaced in the integration cleanup by one manifest, one routine runner and callbacks on the caller's stacks | [`hybrid-runtime.md`](hybrid-runtime.md) and the cleanup commits |
| Native dictionary/compiler | Requested design deliverable complete | Separate staged plan preserves current JIT behavior and does not imply compiler implementation |
| Final unified acceptance | Qualified locally on `0205541` | Both extensions rebuilt; 2,466 application passes with three socket skips; 2,569 simulator passes under each explicit executor; eight rich-terminal passes; exact selectors and identities in the acceptance report, counts overlap |

### Phase 1B server consumer migration — 2026-09-30

The emulator and simulator server implementations now live in
`emulator/server.py` and `simulator/server.py`. The unified launcher,
hybrid parser, prepared Desktop harness and in-repository server tests use
those package interfaces. The emulator still resolves its default BIOS from
the repository root; parser behavior, executor selection and owner lifecycle
are unchanged. Current user commands use `megapad.py --mode MODE`.

The root `session_server.py` and `simulator_server.py` forwarders were
removed once Akashic's tools started MegaPad through `megapad.py`. Internal
helper imports and monkeypatches use the canonical package modules.

`shared_session.py` remains the canonical common protocol/owner module.
The architectural monitor `cli.py` and flat architectural module aliases
are separate supported surfaces; removing them is not part of this server
migration. The earlier sections below preserve the history of the initially
qualified launcher and session extraction.

The focused gate passed 210 checks with one known AF_UNIX skip: launcher cold
imports and legacy script help, emulator server policy/default BIOS, simulator
server preparation, production executor selection, package dependency
direction, existing Desktop preparation and all hybrid session profiles.
This change makes no new socket or physical-display acceptance claim.

### Phase 1A implementation and validation — 2026-09-30

`megapad.py` selects emulator or simulator and delegates unchanged backend
arguments to their existing server lifecycle. Emulator is the default.
Top-level help and unavailable-mode errors load no backend; selected-mode
help needs no native extension. Hybrid remains an invalid selection.

`emulator.server` exposes a parser factory and `main(argv=None)`.
`megapad.py --mode simulator --executor python|native|auto` passes an explicit choice
through image preparation into the runtime used by the live session. Omission
retains the existing environment/default behavior; this does not mutate the
process environment. Missing required native execution fails before boot
source or autoexec runs.

The architectural system import in `session.py` now occurs only in
`MachineSession.from_bios`. A fresh-process test prepares a real MP64FS fixture
and runs its semantic root with emulator-extension imports forbidden. The
remaining Python architectural imports and frontend inheritance are Phase 1B
work; this slice makes no claim of complete package separation.

`make build` builds both extensions through sequential recursive recipes.
`make serve ARGS='...'` invokes the new application; the existing interactive
`make run` target retains its behavior. Existing separate server launchers
remain available while consumers migrate.

Validation used CPython 3.12.14 with the existing pybind11/pytest environment,
`VENV_PY` set to that interpreter, and
`MP64_RUNTIME_NAMESPACE=unified-runtime`. Builds and test commands ran
sequentially:

- `CC=gcc CXX=g++ make build`: both engines built successfully from this
  worktree. The interpreter's default compiler was unavailable (`clang++`);
  selecting installed GCC resolved the environment issue.
- `make test-simulator` with `SIMULATOR_TEST_PATH` selecting
  `tests/test_unified_launcher.py`,
  `tests/simulator/test_simulator_server.py`, and
  `tests/simulator/test_image_bootstrap.py`: **32 passed**.
- The same simulator server/bootstrap files with
  `MEGAFORTH_EXECUTOR=native`: **20 passed**. These overlap the preceding
  selector and are not 20 additional distinct tests.
- `make test-sequential TEST_PATH=tests/test_session.py` with the selector
  `session_server or machine_session_boots_interacts_and_captures or
  machine_session_starts_its_clock_at_a_given_time or
  machine_session_owns_injected_nic_backend or machine_session_close`:
  **11 passed, 32 deselected**. This includes real BIOS boot, Forth evaluation,
  capture, clock construction, device ownership, and existing server policy.
- `git diff --check`: passed.

The execution engines, arithmetic, guest scheduling, display implementation,
and source workloads are unchanged. This is launcher/lifecycle qualification;
no performance improvement or complete physical Desktop acceptance is claimed.


### Phase 1B implementation and validation — 2026-09-30

Extracted `shared.session.TerminalSession` and its configuration/capture
values from architectural construction. `emulator.session.MachineSession`
and `simulator.session.SimulatorMachineSession` now inherit it directly.
Common close cleanup is shared; emulator storage save, UART restoration,
audio/NIC release, and simulator backend detachment retain their previous
ordering and failure behavior.

`shared_session.SharedSessionOwner` now owns only common control, phase
observation, locking, display/input authority, and lifecycle interfaces.
`emulator.shared_session.SharedMachine` retains architectural execution and
diagnostics; the simulator owner is its sibling. Their existing run loops,
IDL/wake rules, units of work, host cadence, and reset behavior are unchanged.
Server/client socket and display-lease code remain in the common module.

Terminal policy decoding moved to `shared.session_options`; the simulator
server no longer imports the emulator server. In-repository frontend and
backend callers use canonical modules. A small root `session` alias remains
for the benchmark's historical `--runtime-root` loader and preserves canonical
module identity, following the package contract. The former architectural
`shared_session.SharedMachine` export is removed. Legacy server script entry
points still reach the same owners and are retained during launcher migration.

Both status variants expose the shared `runtime` descriptor: actual mode and
executor, accounting and step-request units, timer/RTC policy, and supported
machine-code/diagnostic/reset/profile actions. Python service fallbacks are
still possible under native execution. This does not turn instruction batching
into strict cycle-bounded execution or semantic steps into hardware cycles.
The hosted RTC exposes its binding policy without sampling the clock.
Consolidating terminal status also corrected an existing simulator diagnostic:
`frame_bytes_by_type` previously returned frame counts; it now reports actual
byte counters and has a real CELL-session regression check.

Validation used the same CPython 3.12.14 environment and runtime namespace as
Phase 1A, with sequential Make invocations:

- `CC=gcc CXX=g++ make test-sequential`, selecting `test_session`,
  `test_shared_session`, `test_session_viewer`, `test_terminal_text_cells`,
  the four `test_rich_terminal_semantic_{session_input,shared_input,shared_wire,
  viewer_input}` files, `test_backend_package_layout`,
  `test_rich_terminal_vertical_contract`, and `test_runtime_consumers`:
  **277 passed, 2 skipped**. This covers BIOS interaction, display offers and
  acknowledgments, stale input generations, retained resource leases, reset
  failures, close cleanup, media/runtime claims, idle wakeups, backpressure,
  phase accounting, viewer rendering, and detailed/lightweight status.
- `make test-simulator`, selecting `test_session_boundary`,
  `test_unified_launcher`, and simulator session/shared-session/clock/server/
  image-bootstrap files: **49 passed, 1 skipped**. Fresh processes block the
  emulator package, architectural import aliases, both native extensions,
  assembler, and pygame while importing frontend help or running a real
  Python session through the control dispatcher. The direct path exercises
  paused boot, KEY/IDL, output, rejected stale input, accepted input/resume,
  completion, close, and reacquisition of the same runtime.
- `MEGAFORTH_EXECUTOR=native make test-simulator` over simulator
  session/shared-session/clock/server/image-bootstrap files: **34 passed**.
  These overlap the preceding test set and are not additional unique tests.
- After finalizing the temporary root alias, the three package-layout checks
  were rerun and passed. `git diff --check` also passed.
- Socket-dependent tests skip because this execution environment returns
  `PermissionError` for AF_UNIX creation. Socket ownership source is unchanged;
  actual socket transport/reconnection is not newly qualified here. The new
  cold-import lifecycle check runs through direct dispatch regardless.

Explicit `--executor native` remains available and tested. Automatic native
production-default promotion is still deferred: this session refactor and
its focused checks do not constitute representative workload qualification.
Phase 2 must record current baselines before changing defaults or selecting
additional hot-path extraction. No new performance or full Desktop acceptance
claim is made by this slice.

### Phase 3A shared value kernel — 2026-09-30

Both extensions compile `shared/accel/scalar_fp.cpp`. The kernel uses bounded
integer arithmetic for exact FP32/64 arithmetic, FMA/FMS, square root,
conversions, comparisons and classification. No host floating-point rounding
mode or third-party big-integer package is required. The Python value models
remain independent. Build object directories are isolated between extensions
so their optimization/sanitizer flags cannot reuse the same shared object.

Full-core emulator FC instructions now call the kernel directly, preserving
full-tail validation, fault PC, REX/PC register aliasing, sticky flags, cycle
charges, strict-cycle retirement, and intentional I-cache noncoherence.
Microcore oracle/fault policy remains unchanged. Native-selected hosted FP
binds the same value kernel; Python selection retains the reference model.
An obsolete native extension fails clearly in required-native mode and falls
back in auto mode. Direct semantic FP dispatch is the next slice.

Validation through sequential Make targets: 198 existing/new kernel and machine
checks passed, covering all legal operations/modes, directed and seeded values,
all FP16/BF16 widening bit patterns, FMA exponent extremes and machine state.
Two additional service-bypass/stale-build checks passed. All 53 hosted scalar
word tests passed in Python selection and again with native selected. Both
extensions built with GCC; no RTL change or new RTL parity claim is involved.
The paired bounded FP evidence is recorded in the performance report.

### Phase 3C qualified audio transfers — 2026-09-30

The shared audio model accepts a backend-qualified synchronous span reader.
After existing descriptor validation it rechecks exact scalar/validator
identities and backend method/span eligibility. Canonical ordinary spans copy
once to immutable PCM; custom or replaced methods retain byte-level dispatch.
The emulator additionally excludes spans intersecting overlapping apertures,
whose byte-priority routing can differ from an accepted Bank0 span. Sparse
hosted reads preserve absent-page zeros without allocating pages.

No capture/generation is published on a short, invalid or failed bulk read,
and no retry or sink call follows that failure. Existing sink/reset/close
ordering remains intact. All 57 focused audio tests passed through sequential
Make execution. Read-only independent review prompted late-helper replacement
and aperture-overlap regressions before commit. Paired hosted measurements
are recorded in the performance report; physical playback is not claimed.

### Phase 3A direct semantic FP calls — 2026-09-30

Compiled calls to original BIOS FP and FPCSR words now use two-tick native
operations. Plans bind original Word identity and the service captured by BIOS
closures; public attribute replacement, shadowing and XT reuse do not retarget
those calls. Each native interval imports and settles one FPCSR cell before
clocks or fallback. The raw semantic API is versioned with this new state.

Invalid operation/RM descriptors, insufficient operands, stack capacity and
unavailable output backing decline without effects or ticks. Python executes
the original operation with its partial pops, fault callback and budget order.
Direct primitive execution and primitive XTs reached via EXECUTE retain the
service path; no additional execution profiles are silently admitted.

All 399 selected checks passed through `make test-simulator`: the 104 new
direct-FP cases, the existing 228 native executor cases, 53 hosted scalar word
cases and 14 shared kernel/service cases. Coverage includes all BIOS words,
rounding/flags, tiny budgets, stack faults/retained bytes, missing/fragmented
backing, fault observers, callbacks/quanta and original service/word identities.
Independent read-only review found no additional issue. The native extension
built successfully and paired timings are recorded in the performance report.

### Phase 3B shared Keccak values — 2026-09-30

Extracted the existing architectural Keccak-f[1600] permutation into
`shared/accel/keccak.{h,cpp}` and linked it into both extensions. The native
round scratch still receives volatile erasure. The immutable Python value
boundary validates exactly 25 uint64 lanes, preserves sequence iteration order
and clears its local copied state on success or exceptions. The independent
Python oracle is unchanged.

Native-selected hosted SHA3 binds this kernel without changing device owner,
MMIO, padding, buffers, staged publication or fault cleanup. Explicitly injected
permutations, service subclasses and prior oracle overrides retain their
implementation. A new explicit Python runtime using the same platform clears
a previous runtime-selected value executor; guest CLEAR retains the binding.

Both extensions built and 86 kernel/device/differential checks passed, plus
47 hosted SHA3 checks in each backend and 26 WOTS borrowing/timing checks.
Read-only review caught a custom-Sequence iteration edge before final build.
Native source qualification exposed a pre-existing fault-continuation bug;
`c8cc5a5` fixes it independently after baseline reproduction and three focused
regressions. Paired SHA3 timing and exact-output evidence is in the performance
report. Python byte-level transfer costs remain; this is not a complete crypto
service or Desktop performance claim.

### Phase 2 representative source-loading controls — 2026-09-30

The existing full KDOS source-loading benchmark passed three fresh-process
trials per executor at the preserved baseline and at the runtime committed as
`a3384e8`, with 90-second per-process watchdogs. All 12 runs passed exact source,
startup transcript, dictionary, stack and representative-word validation. Each
performed 36,116 semantic steps while loading 1,461 KDOS definitions. The raw
reports retain preparation versus loading time, source/binary identities and
peak memory. No compiler optimization or loading speedup is claimed; this is
a representative compatibility/control workload. Desktop and source-loading
attribution remain open, as does executor default promotion.

### Phase 4 shared backing foundation — 2026-09-30

`DenseMemoryBacking` owns a fixed buffer for each ordinary guest region.
Independent pinned views keep each buffer alive and prevent resizing. The
existing semantic address-space type accepts this owner explicitly; its sparse
default remains unchanged. Native semantic execution receives direct dense
descriptors and retains independent buffer leases. Complete range, stack and
geometry checks precede pointer access, and dense mappings cannot physically
alias each other. The native API is version 2 so stale binaries fail admission.

The semantic extension built successfully. All 56 new dense owner/adapter tests
and 664 existing memory, platform, stack, native execution, bulk memory,
suspension and FP regressions passed. Independent review verified pin lifetime
and checked access ordering. This establishes shared bytes, not hybrid calls;
the launcher remains disabled until registration, runner and session gates pass.

### Phase 4 allocation and declaration validation — 2026-09-30

Dictionary body leases now bind the exact live Word, original initial-body
extent and a monotonic allocation serial. Allocator writes, reclamation,
rollback and zone reopening revoke overlapping leases before reuse; unrelated
definitions and ordinary stores do not. Reusing the XT and identical bytes
cannot revive a revoked lease. All 66 new lease and existing dictionary/rollback
checks passed, including inactive zones and forged lease objects.

The hybrid routine owner uses these leases to tell when a routine's code
has been reclaimed.

### Phase 2B timing identity — 2026-09-30

System batch results now identify `instruction_batched` versus
`strict_shared_clock`, including zero-budget and host-backpressure returns.
Shared session status reports its actual execution policy; semantic sessions
report `semantic`. Only strict results advertise modeled shared-clock latency.
The Phase 0 benchmark carries this identity into serialized accounting, and
the session documentation explains round time, strict bus time and host time.
No scheduler behavior or existing cycle accounting changed in this slice.

All eight focused model tests passed, along with shared-session and Phase 3
benchmark regressions in a 221-pass combined foundation gate. Two socket tests
were skipped because this environment cannot create their sockets. These tags
prevent a functional round clock from being presented as strict wake latency;
they do not reproduce the unavailable math solver or establish physical RTL
timing. Strict FP and bounded wake/contention evidence are separate gates.

### Phase 2B strict multicore scalar FP — 2026-09-30

The existing native scalar FP implementation passes 13 additional strict-system
cases. All 52 BIOS scalar/FPCSR operations execute on one, two and four full
cores with warm and cold instruction fetch, checked against the independent
Python value oracle. Four-core division/square-root with store prefixes matches
whole-run state when split at strict cycle boundaries across one, two and four
host lanes. Four actual assembled BIOS bodies (`F64+`, `F64/`, `F64FMA`,
`F64SQRT`) also produce exact stacks, bits and flags on four cores.

The tests poison Python fallback and require zero native continuations. All 13
passed with 27 existing native FP, 43 private execution and 79 hybrid-runner
cases (162 total). This qualifies full-core strict FP retirement, including
provisional execution and bus replay. FP remains coordinator/interpreter work;
this does not claim private worker/DBT lowering, micro-core strict support, or
execution of the math team's unavailable solver.

### Phase 2B bounded wake and contention evidence — 2026-09-30

`bench_execution_timing.py` now provides local integer fixtures for masked-IPI
wake, fixed-total private compute and disjoint-cell shared-bus traffic. It
records the two timing models separately, with guest cores and host lanes as
independent axes. Strict latency observations come from a separate one-cycle
replay whose completed state/counts must match unobserved timed trials. Fast
execution retains its large batch and makes no shared-clock latency claim.

All 19 harness cases and 48 fresh-process measured cases passed. Sixteen host
lane comparisons preserved guest state and model clocks. Strict compute scales
4.00× in guest cycles from one to four cores; the memory fixture scales 2.49×.
The minimal masked-IPI marker appears 1–3 modeled cycles after assertion at the
replay's resolution. These local kernels neither reproduce the BIOS worker
protocol nor establish the reported solver ratios. The method and summary
are in `docs/performance/execution-timing-benchmark.md`; the raw samples, hashes
and full limits remain in the repository history. No modeled bus wait was
removed or scheduler behavior changed to obtain these results.

### Phase 2 source and captured-frame attribution — 2026-09-30

Separate cProfile runs of full KDOS loading passed existing source, dictionary,
transcript, stack and media validation in both semantic executors. Three
existing Make-selected compositor tests passed exact pixel/hit-map comparison
for ready/typed Desktop captures and all 12 typing offers. The 11 later offers
used partial repaint with damage below one quarter of the frame each.

Raw diagnostic reports retain source/fixture hashes and clearly include setup,
collection and reference costs. They are not throughput samples or live
Desktop latency measurements. Parsing, scalar memory checks, frame composition
and wire decoding remain visible Python work, but each further extraction still
needs a targeted compatibility and paired-performance gate. No external project
was accessed or modified. Live Desktop and executor default promotion remain
open; recorded offers cannot establish those outcomes.
