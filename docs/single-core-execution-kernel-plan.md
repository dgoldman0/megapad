# MegaPad single-core execution-kernel replacement plan

**Started:** 2026-08-27

**Status:** Active — construction qualification only

**Branch:** `single-core-execution-kernel`

**Isolated worktree:** `.worktrees/megapad-single-core-execution-kernel`

**Correct DBT baseline:** `72e1122adaac3de2bc23d235e58063cd179d43ce`

**External comparison snapshots:** `main` at
`b399bd066d9ecdc87bf445dee6fd8e615255e61d`; `rich-terminal-vertical` at
`3f339eb486cd9bcc52fd14a735ae218fd7abc219`

## Purpose

This branch replaces the exact-single full-core execution kernel in place. It
does not introduce a second engine, a versioned runtime selector, or a legacy
compatibility path. Git history and the independent `main` and
`rich-terminal-vertical` worktrees provide the comparison boundary; the
production source tree retains only the implementation moving toward release.

The existing DBT is architecturally correct and modestly faster, but its
one-memory-operation blocks are too short to amortize admission, generated-code
entry, interrupt polling, return, and settlement. At the same time, storage and
round settlement expose larger host costs outside translation. The replacement
therefore separates machine coordination, MP64 guest semantics, portable block
construction, and host-specific code generation before rebuilding the hot
path around the workload actually observed.

This is an execution-kernel replacement, not a ground-up rewrite of device
models, public machine state, strict-cycle behavior, snapshots, replay, or the
Python-facing emulator. Those areas change only where a measured execution
boundary requires a narrowly reviewed integration change.

## Starting evidence

The following evidence is distilled here because the detailed diagnostic
reports are intentionally untracked under `/tmp`.

| Observation | Baseline evidence | Consequence |
|---|---:|---|
| Canonical architectural work | 95,760,826 instructions; 111,708,049 cycles | Performance work may not alter retired work or timing |
| Current end-to-end DBT result | About 2.5% lower paired wall time on the noisy current host; an earlier cleaner comparison was about 4.5% higher throughput | The path is useful evidence, not yet a sufficient production return |
| Native compilation | About 0.027 seconds for 25,890 blocks | Compiler speed and arena allocation are no longer primary targets |
| Native publication | About 0.004 seconds | Further publication tuning cannot provide the expected gain |
| Native entry grain | 28.7 million entries; 2.462 guest instructions per entry | The block ABI and surrounding boundaries dominate |
| Round settlement | 95,761 logical rounds; 95,953 Python calls; about 11–12% of native-batch time | Preserve cadence while removing no-event language crossings |
| Source-load storage | 494 sectors, 252,928 bytes, 15 commands; equivalent immediate replay median 0.472 seconds | Add a bounded canonical synchronous span path without weakening strict DMA |
| Scalar rejection pressure | 12.57 million hits, 52.6% of all rejection-cache hits | Memory shape is the central translation boundary |
| Cross-line hot pair | About 4.36 million `LDN R13,R13` executions paired with about 4.39 million unconditional branches | Multi-line identity is a required generic acceptance case |

The storage replay establishes the scale of the byte-state-machine cost; it is
not by itself an end-to-end savings claim. Profile wall timers are diagnostic
and inclusive. Clean performance claims require paired unprofiled runs when
the project-wide qualification gate permits them.

## Non-negotiable architecture

- The exact instruction count, cycle count, trap state, callback order, public
  accounting, and final machine state remain unchanged.
- The 1,000-instruction unbounded scheduling cadence remains unchanged. Native
  settlement may optimize a boundary but may not remove or enlarge it.
- Full-core guest I-cache noncoherence remains intentional. Block identity is
  derived from architecturally resident guest-cache bytes, tags, and host-only
  generations rather than fresher backing memory.
- Unbounded and strict-cycle execution remain distinct. No unbounded fast path
  silently weakens bus beats, arbitration, injected faults, stalls, re-entry,
  media-generation checks, or exact event timing.
- Host diagnostics, cache generations, block metadata, and generated code stay
  outside snapshots, replay, canonical hashes, guest accounting, scheduling
  decisions, and virtual time.
- Caller-provided storage and block bounds remain authoritative. The rewrite
  does not introduce arbitrary fixed capacities when an existing caller bound
  or measured span is the actual limit.
- MP64 instruction semantics have one owner. Full and micro cores may use
  specialized execution policies, but they do not acquire copied decoders or
  divergent definitions of an instruction.
- Replaced code is deleted. Temporary extraction bridges are permitted only
  within the active element and are removed when that element commits.
- The Python extension remains one `_mp64_accel` module. Source separation does
  not create plugin loading, runtime backend registration, or parallel engine
  APIs.

## Target ownership and source shape

The final names may be refined as dependencies become concrete, but the
ownership boundaries are normative:

```text
accel/
  bindings/
    module.cpp                 pybind registration only
    python_callbacks.cpp       GIL-owning callback adapters

  machine/
    state.h/.cpp               system-owned native state
    memory.h/.cpp              region selection and bounded span proofs
    scheduler.h/.cpp           logical rounds and dispatch custody
    settlement.h/.cpp          clocks, interrupts, UART boundary decisions

  cpu/mp64/
    state.h                    architectural full/micro CPU state
    decode.h + decode_impl.h   sole MP64 decoder; inlined reader template
    semantics.h                shared instruction definitions and effects
    interpreter.h              inlined authoritative decoded effects
    icache.h/.cpp              guest I-cache behavior and identity generations
    block_ir.h/.cpp            bounded multi-memory blocks and exit descriptors
    block_cache.h/.cpp         admission, rejection, and identity revalidation

  dbt/
    backend.h                  narrow host-code ownership contract
    executable_arena.h/.cpp    bounded W^X code ownership
    x86_64/
      emitter.h/.cpp           x86-64 byte emission
      lowering.cpp             MP64 block IR to x86-64 machine code

  mp64_*.h                     existing device models, moved only when separately justified
```

Only machine-code emission and its host ABI belong under `dbt/x86_64`.
Portable native C++ execution remains under `cpu/mp64` or `machine`; it is not
x86-64 code merely because the host compiler targets x86-64. No empty future
backend directories or speculative abstraction framework are added. The
generic `dbt` layer remains deliberately small: MP64 block IR, identity,
admission, and cache policy stay with the guest architecture until a real
second consumer proves a broader abstraction.

Source separation is not itself a speed claim. Small hot helpers may remain in
internal headers when measured inlining requires it. Cold validation, error,
profile, binding, and device paths should not remain in the hot execution
translation unit. Cross-translation-unit optimization or LTO is considered
only from measured evidence, not used to conceal an overly chatty interface.

The extraction order must respect present coupling. `CPUState` currently mixes
architectural state, Python buffer ownership, borrowed devices, I-cache state,
and DBT handles, while x86-64 lowering encodes live `CPUState` member offsets.
The executable arena must also outlive every published code handle, and
strict-cycle replay calls the same authoritative instruction semantics. These
seams are moved deliberately; the plan does not authorize a bulk textual split
that forks state or semantics merely to produce the target directory tree.

## Core data contracts

### Resolved memory span

The machine memory layer selects a region once and returns a bounded resolved
span containing the host pointer, available extent, selected priority, access
policy, and any generation needed for safe reuse. It proves guest-address
non-wrap and higher-priority aperture nonintersection algebraically. Exact
bytewise resolution remains the slow path for Bank 0 aliases, aperture edges,
wrap, MMIO, privilege, and other exceptional shapes.

### Decoded instruction and block IR

The sole decoder produces a compact instruction representation consumed by the
authoritative decoded executor and the DBT lowering backend. A block is bounded
by caller-owned storage and architectural exits, not by a special one-line or
two-line type. It can describe multiple checked loads, a controlled terminal
store, exact fetch and cycle effects, interrupt observation points, and a
completed-prefix side exit.

The profiled cross-line `LDN R13,R13` plus unconditional branch is the first
multi-line acceptance motif, not an app-specific opcode fusion. Common Forth
stack sequences with multiple loads and a terminal store are the first
multi-memory acceptance motifs.

### Guest-cache identity

Block identity records the bounded set of resident guest I-cache lines it
observed. A matching tag and host-only fill generation provides the hot O(1)
proof. A refill or restored state performs the exact byte comparison once and
either refreshes the generation binding or discards the plan. Invalidation,
rollback, restore, and generation wrap conservatively prevent stale execution.

### Block exit

Decoded and generated blocks return one shared exit description: completed
instruction and cycle counts, exit reason, next architectural PC/selector
state, interrupt or timing boundary, and any store range requiring guest-cache
invalidation. This replaces backend-specific post-return settlement and lets
the scheduler handle one explicit contract.

### Host backend

The x86-64 backend lowers block IR without owning MP64 decoding, guest-cache
identity, scheduling, or Python callbacks. It keeps profitable guest registers,
PC, and flags live across the block and materializes them at exact exits. Direct
continuation is considered only after blocks are long enough to amortize it;
the reverted C++ successor-probe loop is not restored.

## Fixed implementation elements

| Element | Scope | Status |
|---|---|---|
| 1 | Source ownership and build decomposition | Complete |
| 2 | Bounded synchronous storage transfer | Complete |
| 3 | Native no-event round settlement | Complete |
| 4 | Algebraic memory foundation | Complete |
| 5 | Authoritative decoded execution kernel | In progress |
| 6 | Multi-line and multi-memory block construction | Pending |
| 7 | x86-64 lowering and direct continuation | Pending |
| 8 | Consolidation and acceptance | Pending |

### Element 1 — Source ownership and build decomposition

- Teach the extension build to compile multiple source files without adding a
  second build system.
- Extract the bounded executable arena and x86-64 emitter behind narrow
  internal headers. Keep lowering in the monolith until the stable MP64 JIT
  frame exists; do not expose the present mixed `CPUState` merely to move it.
- Move code without changing behavior, and delete each old definition in the
  same slice that installs its new owner.
- Update build provenance and source-audit wording that currently assumes one
  `accel/mp64_accel.cpp` translation unit.
- End with one extension module, no duplicate implementations, and a materially
  smaller hot execution translation unit.

Completion evidence (2026-08-27): the extension build now compiles the module,
bounded W^X arena, and x86-64 emitter as three translation units. The former
arena and emitter definitions were deleted from `mp64_accel.cpp`; the tiny
code-handle operations remain inline at their hot call sites. The extension
built and imported successfully, and the focused arena publication/execution
and maximal emitted-register-block selectors passed serially (2 passed, 55
deselected). This construction check is not a performance qualification claim.

### Element 2 — Bounded synchronous storage transfer

- Add a caller-bounded span operation for the canonical immediate controller
  path after the existing one-window validation succeeds.
- Use it only when callbacks and controller state prove the ordinary
  non-reentrant, no-fault, no-stall configuration.
- Retain the byte-state machine for strict-cycle execution, injected faults,
  wrapped callbacks, re-entry observers, unusual mappings, and media changes.
- Preserve command completion, transferred-sector accounting, and memory
  mutation order at the public command boundary.

Completion evidence (2026-08-27): ordinary guarded storage commands issued by
an active native system batch can now copy one caller-sized transfer through
the already-proved physical memory window while holding the batch's mapping
ownership. Exact scalar callback identity, current media/request identity, no
fault history, ordinary timing mode, standard media ownership, and token-space
headroom are all required. Direct host commands, strict-cycle DMA, faults and
stalls, wrappers, observers, subclasses, and unusual configurations retain the
byte FSM. Successful bulk transfers preserve DATA publication, completion,
sector progress, and byte-equivalent DMA token advancement. The focused guest
guarded span path, ordinary callback fallback, and configured-HBW routing
selectors passed serially (3 passed, 32 deselected). Performance qualification
remains deferred.

### Element 3 — Native no-event round settlement

- Extract settlement ownership from Python callback adaptation.
- At every unchanged 1,000-instruction boundary, advance native clock and
  device state directly when the topology has no concrete Python-clocked
  device.
- Enter Python only for an actual continuation requiring Python custody or
  batch-end UART delivery.
- Preserve timer/IPI ordering, callback exceptions, public round counts, and
  exact cycle advancement.

Completion evidence (2026-08-27): the system clock and typed unbounded-round
request now live under `accel/machine/settlement.*`, replacing the former
four-position boolean settlement call inside the unbounded scheduler. At each
unchanged 1,000-instruction exact-single boundary, the scheduler uses the same
mapping lease, clock validation, and native timer/framebuffer/RTC/crypto tick
order directly when no enabled timer or IPI can be delivered. Timer/IPI
boundaries, exception prefixes, native DMA frontiers, and batch-end UART
publication retain Python custody; strict-cycle execution is unchanged.

Admission is recomputed for every batch and requires the canonical system,
bus, tick driver, timer proxy, callback methods, singleton topology, and an
empty Python-clocked-device list. The bus clock topology is frozen for the
duration of an admitted direct-settlement batch so that proof cannot become
stale during a callback or GIL-free native interval; fallback batches retain
their existing ability to extend the clock topology. Host diagnostics now
distinguish logical settlement boundaries, direct native settlements, and
Python settlement calls. Focused
positive checks proved three direct boundaries plus one UART boundary for a
2,503-instruction singleton batch, timer and IPI delivery at their existing
boundaries, exact `[1000, 1000, 1]` fallback ticks for a Python-clocked
extension, fallback topology extension, one batch-end UART publication, and
exact native device cycles (11 passed serially). Full performance qualification
remains deferred.

### Element 4 — Algebraic memory foundation

- Replace repeated per-byte scalar contiguity resolution with the resolved-span
  contract.
- Share the proof with instruction fill, stack traffic, authoritative scalar
  operations, block preflight, and bounded storage where their access policies
  are identical.
- Preserve modulo Bank 0 aliases and every overlap-priority edge through the
  exact slow path.
- Remove redundant prove-then-resolve pairs from the hot path.

First-slice evidence (2026-08-27): `accel/machine/memory.h` now owns the pure,
allocation-free guest-memory map and resolved-span algebra, while Python buffer
ownership and mapping leases remain in the extension bindings. Scalar and
supervisor-byte priority are explicit policies rather than an accidental
property of call order. Natural-width scalar reads and writes select and prove
one direct extent; aperture crossings, Bank 0 aliases, guest-address wrap, and
other non-contiguous cases retain exact bytewise resolution. Instruction fills
and stack traffic inherit the scalar improvement through their authoritative
accessors. The extension built successfully, and focused scalar aperture-edge,
Bank 0 wrap, instruction-fill, shared-mapping, and uncontended execution checks
passed serially (24 passed). Accelerator and DBT preflight consolidation remains
part of this element; this evidence does not mark it complete or claim an
end-to-end performance result.

Completion evidence (2026-08-27): accelerator hooks and exact-single DBT
preflight now consume the same resolved-span contract. The parallel
`DirectMemoryRegion`/`AccelAccessModel` resolver family and its hand-written
higher-priority intersection scans were deleted. Each of the four current DBT
memory shapes obtains its host pointer from its single admission proof while
retaining the supervisor-only, no-bus, full-span MMIO rejection, bounded
Bank 0, and Bank-0-only lowering gates. Accelerator drawing and copying retain
their all-rows-before-mutation transaction boundary, but reuse admitted input
and font spans instead of resolving each byte again.

The ordinary and strict-bus byte paths now also use the explicit
supervisor-byte policy after their existing MMIO, bus, HBW privilege, and MPU
checks. DMA, TACC, and storage remain outside this resolver because their
target-selection and observation contracts differ; this is deliberate policy
separation, not a legacy implementation. The extension built successfully,
and focused accelerator-hook, leading native read, terminal native store,
long-call/return, and aperture-edge oracle checks passed serially (24 passed).
Full performance qualification remains deferred.

### Element 5 — Authoritative decoded execution kernel

- Establish the sole decoder, compact instruction representation, execution
  policy seams, and shared exit descriptor.
- Specialize exact-single full-core execution without copying MP64 semantics or
  retaining the giant universal `step_one` control path.
- Keep microcore oracle/fallback behavior and strict-cycle ownership intact.
- Delete superseded decode and decoded-block execution code as each family
  moves to the new kernel.

First-slice evidence (2026-08-27): the condition-code vocabulary, prefix
register selection, sign extension, flag packing/evaluation/update rules, and
complete immediate/register ALU effects now have one inline owner in
`accel/cpu/mp64/semantics.h`. Their former monolithic definitions were deleted;
the universal interpreter and exact-single decoded executor consume the same
implementation without exposing or relocating the mixed `CPUState`. The
extension built successfully, and focused exact-single/generic/Python state,
microcore REX/scalar, and native SUBI flag comparisons passed serially (6
passed). Decoder and exit ownership remain in progress.

Second-slice evidence (2026-08-27): `accel/cpu/mp64/decode.h` and its internal
template implementation now own the sole semantic decoder for ordinary
architectural execution and exact-single block construction. The compact
16-byte decoded record carries the semantic operation, resolved registers,
immediate, encoded length, cycle cost, and traits. Architectural `fetch8` and
observational guest-I-cache readers instantiate that same decoder directly, so
the hot path does not pay a function-pointer call per byte. Prefix observation,
illegal double-prefix timing, incomplete observational reads, and specialized
F9--FB engine handoff remain explicit decode outcomes.

The migrated `step_one` instruction families and the decoded C++ fallback now
execute one shared effect function. x86-64 lowering and block validation also
dispatch on semantic operations rather than independently interpreting raw
opcode families; raw subops remain only where their encoded condition or ALU
selector is architecturally meaningful. The former DBT-only instruction record
and decoder, and the duplicated migrated cases in `step_one`, were deleted.
Exact-single admission remains a separate policy over the shared decoded form
and preserves the pre-existing admitted instruction set; policy-only scanners
for microcore routing, SKIP sizing, and specialized systems do not execute a
second definition of those semantics. Fetch-hit accounting is derived from the
instruction start and encoded length rather than stored as parallel metadata.

The extension rebuilt successfully after the semantic-lowering conversion,
and the focused Python/microcore oracle, encoded-length and cross-line decode,
decoded/native execution, direct load/store, CALL.L/RET.L, branch, SEP, and
strict multicycle-boundary spine passed serially (26 passed). Shared block-exit
ownership and the remaining specialized interpreter families are still part of
this element; this is construction evidence, not broad qualification or a
performance claim.

Third-slice evidence (2026-08-27):
`accel/cpu/mp64/interpreter.h` now owns the authoritative decoded effect switch
behind a compile-time machine-operations policy. MP64 keeps register, branch,
flag, CALL.L/RET.L stack ordering, and privilege semantics; the local adapter
supplies only existing memory/callback and accelerator-hook boundaries while
`CPUState` and `StepCallbacks` remain coupled inside the monolith. This is an
in-place ownership extraction, not a second interpreter API or selectable
execution route.

The template and its local adapter are forced inline because an initial build
exposed that ordinary header extraction created a new 5.4-KiB out-of-line call
per decoded guest instruction. Final object inspection found no decoded-effect
or adapter symbol and no relocation to such a boundary, restoring the prior
hot-path call shape. The deleted monolithic switch was not retained. The
extension rebuilt successfully, and the focused semantic/native, memory,
CALL.L/RET.L, branch, SEP, strict-cycle, accepted-hook, and byte-MMIO spine
passed serially (29 passed). Retirement and shared block-exit ownership remain
in progress.

### Element 6 — Multi-line and multi-memory block construction

- Build caller-bounded blocks across the resident guest-cache lines needed by
  the decoded instruction stream.
- Prove the hot cross-line `LDN+BR` motif and ordinary Forth stack/load/store
  motifs through generic identity and memory rules.
- Replace repeated positive and negative identity byte comparisons with the
  generation/revalidation contract.
- Measure admission, exits, block length, memory shape, and decoded execution
  before enabling generated lowering.

### Element 7 — x86-64 lowering and direct continuation

- Lower the proved block IR into the retained bounded W^X arena.
- Keep guest registers, flags, fetch accounting, and PC live across useful
  spans and materialize exact completed prefixes at exits.
- Add direct native continuation only where the block/exit profile proves that
  it removes more work than it introduces.
- Delete the present one-memory generated ABI, old block cache, rejection path,
  and unused chaining substrate once the replacement owns production entry.

### Element 8 — Consolidation and acceptance

- Extract Python buffer leases, callback construction, snapshot codecs, and
  pybind registration only after their machine/CPU owners have stable native
  headers; avoid a catch-all header that recreates the monolith.
- Remove temporary extraction bridges, dead profile fields, stale comments,
  unused includes, and the monolithic source file after its final owner moves.
- Audit the final target shape against the ownership rules above; no old/new
  engine selector or duplicate semantics may remain.
- Refresh focused documentation and the external-worktree comparison commands.
- Run broad qualification only after the project-wide rich-terminal vertical
  and normal resource gates permit it.

## Construction-time validation policy

While the vertical is being built, validation stays on the happy path and at
seconds scale:

- build the extension sequentially and import it;
- run focused structural selectors for the files or ABI just moved;
- run deterministic byte-oracle units for memory identity, block exits, or
  storage spans touched by the current slice;
- run one narrow end-to-end happy-path selector when a slice crosses a real
  boundary; and
- use short profiled motifs derived from the retained evidence rather than a
  cold BIOS+KDOS source load.

Do not run smoke, broad integration, persistence, exact-full-core, cold source
load, Desktop, sustained-cadence, live-viewer, full-renderer, enlarged-step, or
worker-spawning qualification while the rich-terminal vertical gate remains in
force. Tests run sequentially. Checked-in limits are not raised.

A newly discovered defect interrupts the current element only when the next
happy-path step cannot be correct without it. Other findings are recorded in
this document's deferred ledger and left for the appropriate later element.

## Commit discipline

Each coherent element or independently useful sub-slice is committed after its
focused happy path is green. Commit messages record ownership moved, contracts
preserved, validation run, and known claim limits in multiple paragraphs.
Corrections receive new commits rather than amendments.

Mechanical extraction and semantic replacement remain separate commits when
combining them would obscure review. This history is the migration record; it
does not justify keeping the superseded implementation in the final tree.

## Performance evidence discipline

- Timed evidence is unprofiled; attribution comes from a separate deterministic
  replay.
- Comparisons use the same source, artifact mode, workload, affinity, and host
  session wherever possible. Paired results are preferred to historical
  medians on this noisy host.
- Short construction measurements may reject a design but do not establish a
  production speedup claim.
- Storage replay time, settlement timer time, and other inclusive components
  are not added together as a projected total saving.
- The first permitted canonical acceptance compares the final implementation
  against the preserved `72e1122` DBT baseline and an appropriate external
  pre-DBT worktree revision, with exact state and transcript equivalence.

## Initial decision ledger

| ID | Decision | Claim boundary |
|---|---|---|
| EK-D1 | Replace the production kernel in place and use revisions/worktrees as the oracle boundary. | No internal `v2`, old/new selector, compatibility facade, or duplicate production engine is retained. |
| EK-D2 | Separate guest architecture from host architecture: MP64 semantics live under `cpu/mp64`; x86-64 emission lives under `dbt/x86_64`. | No empty future backend, plugin registry, or runtime backend framework is introduced. |
| EK-D3 | Preserve every 1,000-instruction logical round while settling the ordinary no-event case natively. | This changes host custody, not virtual time, device order, interrupt eligibility, or public round accounting. |
| EK-D4 | The portable decoded executor and x86-64 lowering consume one decoded/block representation. | Specialized full/micro execution policies may differ, but instruction meaning and block identity do not fork. |
| EK-D5 | Make guest-cache identity a generic caller-bounded line set. | The observed two-line `LDN+BR` pair is an acceptance motif, not a hard-coded capacity or app-specific fusion. |
| EK-D6 | Use focused happy-path construction evidence until the rich-terminal gate permits final qualification. | A green construction selector is not a broad correctness or performance claim; deferred cases remain explicit. |
| EK-D7 | Judge performance with workload-specific paired evidence and exact equivalence. | No arbitrary universal threshold, historical unpaired median, or sum of inclusive profile timers establishes success. |

## Deferred findings ledger

| ID | Finding | Disposition |
|---|---|---|
| EK-F1 | Focused happy-path coverage does not exhaust every branch condition, prefix/fault boundary, CALL.L/RET.L register-alias shape, natural-width callback route, or strict-cycle replay form. | Keep construction on the seconds-scale happy-path spine. Add the compact table-driven semantic matrix and fault/alias timing oracles during Element 8 acceptance, or earlier only if one blocks the next production slice. |
