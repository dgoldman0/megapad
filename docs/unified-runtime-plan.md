# Unified MegaPad application and native execution plan

Started: 2026-09-30

Status: plan locked for implementation; no hybrid execution is implemented.

Branch: `feature/unified-runtime`

Reviewed base: `a0175487815afb3fa2003014eabf1b1e2c0ab5a1`

## Goal

Deliver one MegaPad application and build with emulator, simulator, and
hybrid execution modes. Reuse the two existing execution engines and common
session/frontend infrastructure. Move measured hot computation and runtime
bookkeeping from Python to native code while retaining exact reference
models and the existing compatibility contracts.

The user authorized local branch work, beginning with a local commit of this
plan. Implementation and its evidence are committed in reviewable slices on
this branch. Publishing, merging, and modifying the separate rich-terminal
worktree are outside this work.

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

## Phase 4 — Initial hybrid execution

Write and commit the transition ABI and capability profile before execution
changes. Resolve these concrete questions in that contract:

- the shared backing representation and its sparse/dense adapters;
- ordinary-memory admission, aliasing, mapping lifetime, and code regions;
- machine entry register/selector initialization and preserved state;
- root return, nested calls, continuation-cookie/trampoline ownership;
- per-call and cumulative execution limits, traps, cancellation, and close;
- admitted services and one authoritative FP/tile/device state;
- separate work counters, RTC behavior, and event-release boundaries;
- code publication, I-cache visibility, rollback, and XT lifetime.

Implement a declared single-core routine that accepts and updates shared
buffers, returns results on the agreed stack, and uses the existing MP64
engine. Ordinary semantic execution resumes at the original caller boundary.
The first slice may reject callbacks, suspension, or services that have not
yet received a contract; these rejections must occur at a defined boundary.

Gate: pure-MP64 versus hybrid differential routines cover memory effects,
results, return-stack integrity, invalid entries, instruction faults, budgets,
and repeated transitions. The unknown-XT error path remains intact.

## Phase 5 — Expanded hybrid interoperability

Add machine-to-semantic callbacks, nested transitions, exceptions,
suspension/wake, and selected service access in separate qualified slices.
Then specify native compiler/dictionary integration for guest JIT, machine
modules, and code introspection. Preserve source-visible CREATE/DOES>, body
addresses, immediate-word behavior, and rollback for each admitted profile.

General native images, arbitrary self-modification, complete snapshots, and
multicore hybrid execution need explicit capability and state-mapping work.
They are later deliverables, not implied results of Phase 4.

Gate: each new capability has functional cross-mode evidence and explicit
limits. Machine-level claims continue to require the architectural oracle.

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

| Slice | Status | Evidence |
|---|---|---|
| Plan | Locked | Read-only source review at the base above; initial plan commit |
| 1A — unified launcher/build | Pending | |
| 1B — common session boundary | Pending | |
| 2 — workload profiles | Pending | |
| 3A — native scalar FP | Pending | |
| 3B/3C — remaining native extraction | Pending, profile-driven | |
| 4 — initial hybrid ABI and execution | Pending | |
| 5 — expanded interoperability | Pending | |
