# MegaPad single-core execution-kernel replacement plan

**Started:** 2026-08-27

**Status:** Engine performance correction active after failed Desktop acceptance

**Branch:** `single-core-execution-kernel`

**Isolated worktree:** `.worktrees/megapad-single-core-execution-kernel`

**Old DBT comparison baseline:**
`72e1122adaac3de2bc23d235e58063cd179d43ce`

**External comparison snapshots:** `main` at
`b399bd066d9ecdc87bf445dee6fd8e615255e61d`; `rich-terminal-vertical` at
`3f339eb486cd9bcc52fd14a735ae218fd7abc219`

## Purpose

This branch replaces the exact-single full-core execution kernel in place. It
does not introduce a second engine, a versioned runtime selector, or a legacy
compatibility path. Git history and the independent `main` and
`rich-terminal-vertical` worktrees provide the comparison boundary; the
production source tree retains only the implementation moving toward release.

The DBT baseline was architecturally correct and modestly faster, but its
one-memory-operation blocks were too short to amortize admission, generated-code
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
| Byte-copy hot pair | 3,432,044 `LD.B R0,[R9]` rejection hits and 3,414,441 `ST.B [R7],R0` hits; about 6.83 million paired instructions are strongly implied | A generic ordered two-span block is the first multi-memory acceptance case |

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

## Execution-kernel ownership and future source shape

The execution-kernel ownership boundaries are normative. The complete tree
below also records a longer-term organization direction; entries not needed to
isolate the production execution kernel are not Element 8 completion
requirements. Final names may be refined as dependencies become concrete:

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
      lowering.h/.cpp          MP64 block IR to x86-64 machine code

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
internal headers when measured inlining requires it. Moving the remaining CPU
and machine state, scheduler, callback, snapshot, binding, profile, and device
integration out of `accel/mp64_accel.cpp` is a separate future organization
project, not an execution-kernel consolidation or acceptance requirement.
Cross-translation-unit optimization or LTO remains evidence-driven.

The extraction order must respect present coupling. `CPUState` currently mixes
architectural state, Python buffer ownership, borrowed devices, I-cache state,
and DBT handles, while x86-64 lowering encodes live `CPUState` member offsets.
The executable arena must also outlive every published code handle, and
strict-cycle replay calls the same authoritative instruction semantics. These
seams are moved deliberately; the plan does not authorize a bulk textual split
that forks state or semantics merely to produce the target directory tree.
The remaining integration ownership is not a temporary second engine or
compatibility bridge; any wholesale decomposition receives its own scoped plan
and qualification.

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

Each admitted scalar access has one ordered address recipe: an entry register,
a constant, or a prior direct-read result, plus a modulo-64-bit addend. The
recipe capacity derives from the caller-owned identity bound. Preflight resolves
every recipe through the existing bounded Bank 0 span contract before any
architectural effect; it reads a prior value only when a later address depends
on that value. A failed proof makes zero progress, while a completed terminal
store retains the existing exact invalidation boundary. This is generic dataflow
metadata, not opcode fusion or a shadow execution engine.

### Guest-cache identity

Block identity records the bounded set of resident guest I-cache lines it
observed. A matching tag and host-only slot-mutation generation provides the
hot O(1) proof. A refill or restored state performs the exact byte comparison
once and either refreshes the generation binding or discards the plan.
Invalidation, rollback, restore, and generation wrap conservatively prevent
stale execution.

### Block exit

Decoded and generated blocks return one shared exit description: completed
instruction and cycle counts plus the interrupt, timing, limit, or completion
reason. The next architectural PC/selector state is materialized directly in
the authoritative CPU state and is not duplicated into the descriptor on every
short block exit. Raw generated writes are invalidated inside the execution
kernel before exit publication, where their address and width are already
known, instead of inflating every common exit with rare mutation metadata. This
replaces backend-specific post-return settlement and lets the scheduler handle
one explicit contract without adding redundant hot-path copies.

### Host backend

The x86-64 backend lowers block IR without owning MP64 decoding, guest-cache
identity, scheduling, or Python callbacks. It keeps profitable guest state live
across the block and materializes it at exact exits. Element 7 declined direct
continuation because the measured successor edges do not amortize the required
invalidation, preflight, and broader entry/exit state; the reverted C++
successor-probe loop is not restored.

## Fixed implementation elements

| Element | Scope | Status |
|---|---|---|
| 1 | Source ownership and build decomposition | Complete |
| 2 | Bounded synchronous storage transfer | Complete |
| 3 | Native no-event round settlement | Complete |
| 4 | Algebraic memory foundation | Complete |
| 5 | Authoritative decoded execution kernel | Complete |
| 6 | Multi-line and multi-memory block construction | Complete |
| 7 | x86-64 lowering and continuation decision | Complete |
| 8 | Consolidation and acceptance | Consolidation complete; broad acceptance deferred |

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

Fourth-slice evidence (2026-08-27): `accel/cpu/mp64/block_ir.h` now owns the
shared block-exit reason and result used by decoded C++ and normalized generated
execution. The former backend-shaped `SingleCoreDecodedBlockRun` was deleted.
Complete blocks, caller-budget prefixes, between-instruction interrupts, timing
boundaries, and no-progress declines are now explicit outcomes with one set of
completed instruction/cycle counts. CPU/PERF mutation remains execution-kernel
owned, while the scheduler aggregates the reported delta exactly once.

The x86-64 function retains its private packed RAX token for completed-prefix
and conditional-branch reconstruction; C++ validates that token before
publishing the portable exit. The shared result is deliberately 16 bytes, so
the SysV boundary returns it in RAX/RDX and keeps all six execution arguments
in registers. This improves on the removed 24-byte hidden structure-return
boundary. Architectural PC/selector state remains materialized in `CPUState`,
and rare raw store/CALL.L I-cache invalidation remains at the known write
boundary before exit publication rather than adding payload and a branch to
every short-block return. Object inspection confirmed the register ABI and a
single range check for the common completion/limit reasons. The focused sliced
decoded, dynamic-branch, SEP, store-invalidation, CALL.L, interrupt, and timer
boundary spine passed serially (11 passed). Observational policy-parser
consolidation remained for the final slice.

Completion evidence (2026-08-27): semantic decoding and every observational
opcode scanner now share the same header-only parser for opcode families,
F9--FB engine recognition, modifiers, and double-prefix classification. The
parser reads no operands and accepts a caller-owned prefix-admission policy, so
SKIP sizing, non-accounting system classification, microcore oracle routing,
resident-only full-core admission, reduced-core private admission, strict-cycle
eligibility, and cluster resource arbitration retain their distinct fetch and
ownership contracts without retaining copied prefix loops.

Architectural decoding follows every non-engine modifier and publishes a
prefix before fetching byte two. Private scheduling deliberately admits only
F0--F6 and F8 and rejects other scheduler-private shapes after byte one. The
strict one-cycle classifier reuses the already-observed physical opcode instead
of peeking a second time, while cluster classification keeps operand tails
effective-opcode-relative and exact request identity original-PC-relative.
An initial aggregate handoff to strict classification was rejected after
object inspection showed it inflated the common full-private classifier by
about 38 percent. Narrowing that handoff to the one physical opcode byte strict
classification needs produced a 0x511-byte classifier, below the 0x5c2-byte
pre-migration body. Template and reader-adapter inspection found no emitted
header-parser call or function boundary. The extension rebuilt successfully;
23 focused semantic
decoder/executor instances and 17 focused private, micro, SKIP, system/TACC,
strict-cycle, and cluster-policy instances passed serially. Element 5 is now
complete; broad prefix/fault matrices remain deferred under EK-F1 rather than
blocking multi-line construction.

### Element 6 — Multi-line and multi-memory block construction

- Build caller-bounded blocks across the resident guest-cache lines needed by
  the decoded instruction stream.
- Prove the hot cross-line `LDN+BR` motif and ordinary Forth stack/load/store
  motifs through generic identity and memory rules.
- Replace repeated positive and negative identity byte comparisons with the
  generation/revalidation contract.
- Prove admission, exits, block length, memory shape, and decoded execution
  with short workload-derived profiles before broader lowering work.

First-slice evidence (2026-08-27): positive and negative block identities now
walk the resident guest I-cache line chunks touched by the existing
caller-owned 16-byte identity storage. Construction no longer treats the
starting line suffix as a block limit, and rejection entries retain the exact
fully inspected span rather than the rest of one line. A missing continuation
line is reported separately and never installs a partial rejection. This is a
generic bounded identity change: it adds no two-line block type, capacity
increase, opcode fusion, or x86-64 special case. Host-profile schema 11 names
the resulting rejection identity an exact I-cache span.

A focused motif placed `LDN R13,R13; BR` across a guest I-cache line boundary,
primed only the first line, and then executed four two-instruction passes. The
first nonresident admission remained unstored; ordinary architectural fetch
made the second line resident; one portable block was subsequently built and,
where available, lowered once and entered twice. Its final CPU state, cache
state, retired instructions, and cycles matched the generic two-core reference.
That selector passed alongside four focused one-line admission/rejection and
leading-load instances. Object inspection found no emitted identity-matcher
call boundary. This established the multi-line construction seam without yet
changing its repeated byte-validation contract.

Second-slice evidence (2026-08-27): the exact-single-owned plan cache now
carries one monotonic identity epoch and one epoch per direct-mapped guest
I-cache slot. Each positive and negative entry records only the global epoch
at which all of its bytes were last proved. Ordinary hits still check every
touched line's validity and physical tag, then accept when those line epochs
are no newer than the entry binding; they no longer load or compare identity
bytes. A newer touched line enters one out-of-line exact comparator. Equal
bytes refresh the binding, while changed bytes follow the existing rebuild or
rejection path.

Cache fill/replacement, matching invalidation, instruction rollback, and
changed replay-checkpoint restoration advance the affected slot. Epochs remain
host-only and are never checkpointed. Reset and public I-cache restore keep
their hard plan-discard boundary, and epoch wrap discards all surviving host
instruction plans before beginning a new monotonic interval. The storage lives
behind the existing exact-single plan-cache pointer, so multi-core and
microcore CPU state does not grow. Positive-entry alignment and the shared
epoch header add 9,224 bytes to that one cache, about 2.9 percent.

Ten focused native-plan, rejection, invalidation, and cross-line instances
passed serially, followed by the resident-byte cross-line private-fetch oracle.
The new two-line cases mutate only the continuation line: an identical refill
retains the decoded/native plan with zero builds or compilations, while a
changed `LDN` operand produces exactly one replacement and still matches the
generic-core architectural signature. Object inspection retains only the
0x148-byte out-of-line exact revalidator; the generation matcher itself is
inlined.

Completion evidence (2026-08-27): ordinary scalar blocks now carry an ordered,
caller-bounded address-recipe table derived once during construction. Symbolic
provenance advances the selected PC before each instruction and follows entry
registers, constants, moves, affine immediate changes, and prior direct-read
results. Unsupported multi-source address calculations conservatively end the
block. Any number of proved reads permitted by the block bound may precede the
one controlled terminal store; `CALL.L` and `RET.L` retain their separately
reviewed stack/control rules. The former leading-read-only rule, global
one-memory exclusion, stable-store-address restriction, two specialized scalar
preflights, and singular host-pointer selection were deleted.

Preflight walks the access recipes in instruction order and resolves every
nonwrapping Bank 0 span before native entry. It dereferences an earlier proved
read only when a later recipe needs that result, which admits pointer chasing
without interpreting the whole block twice. No write occurs before the
terminal instruction, external mutation remains excluded by the execution
lease, and the existing per-instruction interrupt polls still publish exact
completed prefixes. x86-64 now consumes a table of proved pointers rather than
one overloaded pointer; the raw terminal write is still invalidated before the
common exit is published. Recipe storage is a 72-byte parallel table per block,
adding 73,728 exact-single-only bytes without enlarging the hot block-entry
stride.

Three workload-derived oracles proved the measured two-instruction byte copy,
the five-instruction Forth `+` stack sequence, and the three-instruction Forth
`@` sequence whose second address comes from its first load. Each built once,
compiled once on x86-64, entered twice with different live addresses and data,
and exactly matched the generic coordinator's registers, flags, memory, guest
I-cache state, instruction count, and cycles. The retained self-modifying-store
oracle now obtains both its destination and byte from preceding loads and still
invalidates and replaces the victim plan exactly once. The extension rebuilt,
and those cases plus the existing direct-read, scalar-store, long-call/return,
cross-line, and dense-slot happy paths passed serially (13 focused instances).
This completes Element 6 without a BIOS timing or broad-qualification claim.

### Element 7 — x86-64 lowering and continuation decision

- Lower the proved block IR into the retained bounded W^X arena.
- Keep guest registers, flags, fetch accounting, and PC live across useful
  spans and materialize exact completed prefixes at exits.
- Add direct native continuation only where the block/exit profile proves that
  it removes more work than it introduces.
- Extract lowering from the mixed execution translation unit behind the stable
  block/access ABI, deleting displaced inline backend ownership and any unused
  chaining residue rather than retaining parallel implementations.

First-slice evidence (2026-08-27): all MP64-to-x86 byte generation now has one
owner in `dbt/x86_64/lowering.cpp`. The CPU integration supplies a non-owning
decoded-block view and one cached, checked displacement table; the backend sees
neither `CPUState` nor `SystemState`. Its opaque entry ABI, packed-return
constants, host availability, layout completeness checks, instruction operand
bounds, prologue, per-instruction lowering, interrupt branches, and common exit
are owned together. Arena allocation/publication, profile accounting, cache
slot selection, code-handle lifetime, architectural return validation, cycle
settlement, and write invalidation remain with the execution kernel. The old
emitter include, inline lowering functions, host-config branch, duplicated
unavailable stub, and CPU-named native function type were deleted from the
mixed translation unit.

The compiler/publication wrapper is deliberately out of line. Before this
split GCC folded it into the hot decoded-block executor, whose main and cold
bodies were `0x279f` and `0x887` bytes. They are now `0x151f` and `0x510`; the
separate compiler wrapper is `0x6d3` plus a `0x7f` cold clone, and the lowering
object has the sole `lower_block` implementation. `mp64_accel.o` has no emitter
reference. The dense block still publishes exactly 1,009 bytes, and 14 focused
serial cases cover register/flag execution, both branch widths, multi-memory
byte and natural-width paths, high-register `SEP`, `CALL.L`, and `RET.L`. This
is source-ownership and object-layout evidence, not a wall-time claim. Direct
continuation was deferred to the production comparison rather than presumed to
be part of the lowering extraction.

Second-slice evidence (2026-08-27): profitable generated blocks now
materialize their identity-proved entry PC in caller-saved x86-64 `R9`, advance
it as a register, and publish it once at the shared common exit. Selection uses
the emitted-byte delta for the block address, length, and terminal control
form, permitting at most one byte of growth for the measured short shapes
where live-PC execution removes hotter state traffic. A semantic PSEL operand
alias, unsafe CALL/stack selector alias, or more expensive shape retains the
established core-memory lowering; there is no alternate engine or cache ABI.
CALL stores the advanced `R9` return address before loading its proved
nonaliasing target, RET replaces `R9` from its proved stack pointer, branch
deltas update it directly, and every existing prefix-IPI stub reaches the
common materialization point. The dense eight-instruction oracle shrank from
1,009 to 983 generated bytes.

The short workload-derived Forth `+` loop alternates a five-instruction
multi-memory block with a two-instruction `SUBI+BR` tail. Its 70,000-instruction
profile remained 19,980 native entries and 69,870 native steps. In consecutive
nine-sample runs of 7,000,000 instructions on the same host, the median fell
from 93.978276 ms to 81.616872 ms: 13.15% less wall time and 15.15% more
throughput for this motif. Eighteen serial focused cases cover generic/Python
equivalence, the admitted live-PSEL source fallback, both branch widths and
outcomes, the cross-line `LDN+BR`, multi-memory paths, `SEP`, `CALL.L`, and
`RET.L`. This is a focused lowering result, not an end-to-end BIOS claim.

The same motif is evidence against adding continuation now. Its store edge
must publish raw-write I-cache invalidation before a successor, while its
return edge needs a fresh three-pointer transactional preflight table that the
non-memory predecessor does not carry. Direct continuation would therefore
require a broader entry/exit and aggregation contract rather than a safe jump
between the current blocks.

Third-slice evidence (2026-08-27): generated x86-64 blocks now retain their
existing CPU-state and optional IPI ABI arguments directly in caller-saved
`RDI` and `RSI`. The backend no longer copies them into `R12` and `R13`, so its
frame saves only the `RBX` step counter and `R15` fetch accumulator. Every
CPU-state load, store, arithmetic update, flag materialization, and common-exit
publication now uses the shorter `RDI+disp32` form; prefix interrupt polls read
the unchanged `RSI` pointer directly. No block-entry signature, cache state,
or architectural exit changed.

The dense eight-compare oracle fell from 983 to 834 generated bytes. The same
18 focused serial cases remained green. On the retained Forth `+` motif, the
70,000-instruction profile remained exactly 19,980 native entries and 69,870
native steps. A nine-sample 7,000,000-instruction median was 81.330257 ms and
86.068829 MIPS, versus the preceding checkpoint's 81.616872 ms and 85.767
MIPS. That roughly 0.35% movement is fine-turn evidence on a noisy host, not a
new material throughput or BIOS claim. A strictly-smaller-only selector trial
dropped the two-instruction branch tail and regressed to an 84.846404-ms
median, which is why the byte-cost gate retains the previously measured shape
for one byte of tolerated growth.

Element 7 completion evidence (2026-08-27): a narrowly authorized, sequential
BIOS+KDOS source-load checkpoint compared clean detached builds using the same
Python, compiler, `-march=native`, `-O3`, canonical schema-4 harness, and host
CPU 3. One excluded warm-up and validation run per revision preceded nine
position-balanced unprofiled rounds.

| Revision | Median wall | Median throughput |
|---|---:|---:|
| Pre-DBT `3f339eb` | 2.914373 s | 32.858 Msteps/s |
| Old DBT baseline `72e1122` | 2.658675 s | 36.018 Msteps/s |
| Current reworked DBT `02c675a` | 1.540888 s | 62.147 Msteps/s |

The current reworked DBT used 41.821% less median paired wall time and
delivered 71.883% more paired throughput than the old DBT baseline; it was
faster in all nine pairs. Against pre-DBT execution it used 43.216% less wall
time and delivered 76.105% more throughput, again faster in all nine pairs.
All 27 recorded reports retired exactly 95,760,826 instructions in 111,708,049
cycles and matched the complete error-free transcript, HERE/LATEST values,
idle and guest-JIT-tail state, boot-image and dictionary hashes, and every
harness validation.

Separate diagnostic replays, which are not timing evidence, show the current
JIT step fraction rising from 73.857% to 96.830%, guest steps per native
execution from 2.462 to 2.946, block misses falling from 24,932,919 to
2,856,084, and rejection-cache hits falling from 23,909,300 to 1,987,915.
Compilation and publication remained about 54 ms total.

Element 7 is complete without direct native continuation. The measured hot
successor edges still owe either terminal-write I-cache invalidation or fresh
transactional memory preflight, so continuation would broaden the entry/exit
ABI and aggregation state without evidence that it pays for itself. It is
explicitly declined, not pending. Any future proposal requires new
edge-specific retained evidence and paired exact comparison; the reverted
successor-probe design is not restored.

This checkpoint establishes an exact-equivalent end-to-end gain for the current
execution kernel. It does not attribute that gain to any one slice, exhaust
deferred semantic or fault cases, qualify strict-cycle or persistence paths, or
constitute broad final acceptance.

The retained comparison command shape used the current checkpoint's schema-4
harness for each clean detached runtime root after a forced local extension
build:

```text
python setup_accel.py build_ext --force --inplace
taskset -c 3 python /path/to/02c675a/bench_bios_kdos_load.py \
  --runtime-root /path/to/revision --json --output /path/to/report.json
```

One warm-up/validation invocation preceded the nine position-balanced recorded
invocations per revision. Timing invocations omitted `--host-profile`; the two
profile reports were separate diagnostic replays.

### Element 8 — Consolidation complete; broad acceptance deferred

Entry note (2026-08-27): Element 8 begins from `02c675a`. Consolidation, not
new execution machinery, became the active critical path. The authorized
comparison did not lift the project-wide qualification gate; broad acceptance
remains deferred, and deferred findings are addressed only where the final
ownership contract or permitted focused acceptance requires them.

The first consolidation slice retires the 313-line structural replay that ran
after the sole block builder. The authoritative decoder and builder now own
static construction validity once; dynamic cache identity, transactional
direct-memory preflight, and backend lowering validation remain at their real
use boundaries. Eight one-hop decoded-instruction wrappers and the impossible
structural-rejection profile path are gone, while a compile-time capacity proof
guards the memory-recipe sentinel encoding. The associated host-profile,
BIOS-benchmark, and report schemas advance with the dead counter removed.

Construction evidence for this slice is deliberately narrow: the extension
builds sequentially, 20 focused block/rejection/profile selectors pass, and
the x86-64 object reports the block builder shrinking from `0x13ef` to `0x0fc0`
bytes. That object-size observation is consolidation evidence, not a runtime
speed claim. No BIOS, cold-source, broad, worker-spawning, or rich-terminal
acceptance run was added.

The second consolidation slice resolves EK-F2 at the sole admission boundary.
Positive-hit, rejection-hit, and rejection-store helpers no longer repeat the
FULL-profile, enabled-I-cache, selector-range, or plan-cache predicates already
proved by their only caller. Exact address and selector equality, resident
I-cache tags, per-slot epochs, bounded identity spans, and cold byte
revalidation remain unchanged. A new positive-entry selector oracle complements
the existing rejection-entry coverage. The sequential extension build and 12
focused selector, collision, refill, and cross-line cases pass; this is a
caller-contract cleanup, not an identity-policy change.

The third consolidation slice resolves EK-F5 with two exact multi-memory
boundaries. A warmed byte-copy block whose later store uses an architectural
Bank-0 alias now proves that bounded direct-span preflight makes zero block or
JIT progress before ordinary execution performs the exact aliased load/store.
A one-shot, out-of-line diagnostic asserts the real IPI router line after warm
block admission; the unchanged generated poll retires exactly the first load,
publishes its one-instruction/one-cycle prefix, and leaves the terminal store
untouched before normal interrupt settlement. The diagnostic adds no generated
entry branch and changes no DBT ABI. The sequential build and 13 focused
preflight, multi-memory, callback-prefix, interrupt, store, CALL, and RET cases
pass.

The fourth consolidation slice resolves EK-F1 at a compact, evidence-driven
boundary rather than expanding it into an opcode cross-product. All sixteen
short-branch conditions now compare the authoritative native executor with the
Python model. `CALL.L` through a distinct stack selector proves that its target
is sampled before the push; this also corrected the Python model's former
post-push read. Ordinary return and failing CALL/RET stack accesses pin the
architectural mutation order. Natural-width callback tests prove eight ordered
little-endian reads and writes plus a later-byte failure through a high-register
prefix. Four representative one-cycle decoded forms compare exact strict-cycle
state and timing with unbounded execution under an explicit one-lane scheduler.
The 29 focused cases pass sequentially. Program-selector CALL aliases are not
silently assigned emulator-oracle status; their existing RTL disagreement is
recorded separately as EK-F7.

Engine-consolidation cutoff (2026-08-27): the four Element 8 slices remove
duplicate static validation, one-hop semantic wrappers, dead profile state,
caller-proved admission predicates, and the final unused execution placeholder.
They also add exact zero-progress and completed-prefix coverage at the
multi-memory boundaries and close the compact semantic matrix. The production
path has one semantic decoder, one decoded interpreter, one shared block exit,
one block-construction and admission route, and one x86-64 lowering owner. No
old/new engine selector, superseded chaining path, duplicate instruction
semantics, or temporary engine-extraction bridge remains. A forced sequential
extension build completed without compiler warnings.

`accel/mp64_accel.cpp` remains the integration owner for mixed `CPUState` and
`SystemState`, scheduling, Python callbacks, snapshots, and pybind registration.
Its size and mixed responsibilities are real organization debt, but decomposing
that integration unit is a separate future organization project rather than a
condition of this engine replacement.

Broad acceptance is explicitly not complete. Persistence qualification, broad
integration, Desktop and rich-terminal journeys, and final gate-level
acceptance remain deferred until the project-wide rich-terminal vertical and
normal resource gates permit them. This cutoff records engine consolidation,
the compact permitted semantic/fault matrix, and the already-authorized exact
BIOS+KDOS comparison only.

Post-cutoff rejection-admission correction (2026-08-27): a focused
shared-memory replay exposed a scalar region with 100,000 block lookups, zero
block builds, and 99,994 exact rejection-cache hits. Every instruction still
probed the positive table before reaching that known negative entry. The
uncontended segment now retains a non-authoritative bitmap, derived from the
existing bounded rejection-cache geometry, for entries already proved exact
in that segment. A hinted start tries the existing exact rejection matcher
first; address, PSEL/SPSEL, resident tags, generations, and bytes remain the
authority. A mismatch clears the hint and follows ordinary positive-first
admission. Interrupt checks, scalar `step_one` execution, timing boundaries,
and all persistent plan state are unchanged. There is no global
rejection-first policy, cached scalar executor, identity relaxation, or new
engine mode. Host-profile schema 13 records positive probes and rejection-hint
probes/hits with exact reconciliation.

An adjacent same-host, CPU-3 characterization used 1,000,000 instructions,
two 100,000-instruction warm-ups, and seven unprofiled repetitions per motif.
The shared-memory median moved from 28.142171 to 31.912840 MIPS, a 13.399%
increase; private compute moved from 186.965970 to 196.627274 MIPS, so this
sample found no positive-block regression. These short, non-position-balanced
construction measurements reject a local regression and justify the bounded
change; they are not a new canonical BIOS, Desktop, or release speed claim.
The separate accounting replay recorded 994,999 exact hint hits from 994,999
hint probes and only 5,001 positive probes across 1,000,000 shared-memory
lookups. Private compute used no hint and retained positive-first admission.
Focused rejection identity, refill, selector, collision, changed-byte, and
self-modification cases remained green; the self-modifying case proves that a
stale hint declines before the replacement positive block executes.

Desktop acceptance reversal (2026-08-27): the ordinary Akashic-main Desktop
smoke invalidated that correction and its supporting synthetic evidence. At
`4937724`, the journey reached the 420-second ceiling after 12,460,500,000
instructions, or 29.648797 Msteps/s. The parent `8eca80f` had completed in
402.21 seconds at 31.628999 Msteps/s, while the retained pre-engine control
completed in 346.77 seconds at 37.944459 Msteps/s. The hint therefore reduced
Desktop throughput another 6.261% and left the engine 21.863% below the
pre-engine control. RSS was unchanged. The failed journey stopped while Agent
was still processing `daybook.source`; it never reached the later composer and
File Explorer legs.

The former synthetic justification compared mutually exclusive extrema:
`private_compute` was one five-instruction positive block, while
`shared_memory` was effectively all exact rejection. Its timed samples were
only about 5--36 milliseconds, all baseline samples preceded all candidate
samples, and substantial within-run frequency drift was visible. Existing
Phase 0 evidence had already shown the rejected scalar path falling from
41.176 MIPS at pre-engine `3f339eb` to 31.887 MIPS at `8eca80f`; the earlier
decision failed to treat that adverse scenario independently.

Phase 0 schema 23 adds `bios_admission_mix`, a primed 100-instruction circuit
whose unreachable padding fixes every start's I-cache alignment. Per 1,000
measured instructions it produces exactly 360 lookups, 340 positive block
hits, 20 rejection-cache hits, 340 executions, 980 block steps, and 20 scalar
steps with no measured construction or publication. This closely matches the
canonical BIOS+KDOS profile's 359.506 lookups per 1,000 instructions, 5.774%
cached-rejection share, and 2.943 block steps per execution without pretending
to synthesize source-compilation churn. BIOS benchmark schema 8 now reports
lookups per 1,000 steps plus cached-rejection and build-attempt shares directly.

Future engine timing decisions use a matrix, never a weighted aggregate:

- an in-process warm-up and at least two seconds per synthetic timing sample
  on one pinned CPU; the calibrated mix currently needs about 200 million
  instructions on this host;
- separate, unprofiled, position-balanced A/B pairs with equal AB and BA
  ordering, followed by a diagnostic profile replay;
- independent results for the calibrated mix, stable positive blocks, and the
  rejected scalar path; a gain in one cannot cancel a regression in another;
- a position-balanced canonical BIOS+KDOS source-load comparison for real
  compilation/churn behavior; and
- a fresh-image Desktop load only after the seconds-scale matrix predicts a
  net win and all architectural/profile oracles agree.

The corrected matrix established the `4937724` before-state in ten
position-balanced pairs per scenario. The hint lost nine of ten stable-positive
pairs with a 1.09987 median paired wall ratio, lost nine of ten calibrated-mix
pairs at 1.05927, and won only six of ten rejection-only pairs at 0.98422. The
intended extreme therefore gained about 1.58% while the common and mixed paths
lost about 9.99% and 5.93%. All architectural end states were identical.

The hint and its profile fields are now removed rather than retained dormant.
The normal execution path again matches `8eca80f` exactly; only host-profile
schema 14 differs so removal is explicit to report consumers. A longer
200-million-instruction calibrated comparison, with 20 million in-process
warm-up instructions per sample, measured a 0.99639 median paired wall ratio
against a clean `8eca80f` build and exact state in every pair. That is rollback
parity, not a new speed claim. Phase 2 report schema 16 carries the host-profile
schema update. No replacement predictor is inferred.

The next production-prefix measurement exposed a second calibration gap. The
Phase 0 circuits are raw MP64 assembly and never boot the BIOS, so
`bios_admission_mix` reproduces host-admission ratios but does not exercise the
BIOS compile-time Forth JIT. Production `kdos.f` executes `JIT-ON` before its
own compilation, evaluates the complete Desktop autoexec under that policy,
and reaches `JIT-OFF` only after autoexec returns. Because `DESK-RUN` remains
live, an ordinary Desktop smoke is expected to retain guest JIT state one.
The pre-engine and refined revisions have byte-identical BIOS, KDOS,
networking, and standard-autoexec sources. A fresh-image 700-million-step
pre-roll followed by an exact 500-million-step Akashic source window confirmed
identical PC, cycles, HERE, LATEST, image hash, and guest-JIT counters across
the revisions, including guest JIT state one. Guest-JIT configuration is
therefore not the A/B difference.

That real source window is substantially harsher than BIOS+KDOS: 553.467 host
block lookups per 1,000 guest steps, a 19.136% cached-rejection share, a 3.073%
build-attempt share, and 1,222,363 block builds with 1,222,361 evictions. Of
216,495,983 successful positive admissions, 92,388,072 made zero block
progress, while native execution still covered 69.321% of guest steps. The
refined engine won the first 700 million steps but lost the following source
window to the pre-engine control; a globally enabled or disabled host JIT is
therefore not a sufficient answer.

`bench_guest_jit_source_load.py` adds a seconds-scale synthetic gate for this
missing shape. It boots production BIOS+KDOS, enters the external userland
dictionary, explicitly fixes guest JIT on and resets its counters, then pauses
at a `KEY` boundary. Setup is excluded from timing. The measured phase compiles
2,048 deterministic Forth bodies plus external data definitions, string
literals, literal folds, and sparse bigrams into external RAM, then runs two
million iterations combining an external-memory load/update/store word with a
guest-JIT-emitted scalar fallback word before returning through KDOS's final
`JIT-OFF`. Its report binds source, image, accelerator, guest-JIT, dictionary,
and host-profile identities. This is a source-churn and
external-memory diagnostic, not a replacement for the canonical BIOS+KDOS or
Desktop gates. The older `bench_bios_kdos_profile.py` labels are corrected:
its former “JIT OFF” case also executes KDOS's internal `JIT-ON` and was never
a valid off/on comparison.

The default generated fixture completes 859,562,672 measured steps and its
guest compiler records 27,278 inlines, 1,281 folds, and 137 peepholes. A
separate profiled replay measured 521.699 host lookups per 1,000 steps, 21.486%
cached rejections, 2.873% build attempts, 2.806 steps per block execution, and
74.199% native-JIT step coverage. Those values are materially closer to the
real source window than either former Phase 0 extreme, while the remaining
zero-progress and compilation-rate differences are explicit. One adjacent
unprofiled characterization measured 41.758 Msteps/s at the refined engine
and 39.328 Msteps/s at the pre-engine control, even though the real
700M--1.2B Desktop window favored the control. This is useful disagreement:
the generated gate, rejected-scalar gate, canonical BIOS+KDOS gate, and
fresh-image production prefix remain independent requirements. No synthetic
score is allowed to average away an adverse result.

External-RAM admission correction and production acceptance (2026-08-27):
the representative fixture exposed a mismatch between memory resolution and
native entry preflight. `resolve_direct_memory_span` already proves a complete,
non-wrapping span and distinguishes pinned external RAM from Bank 0, HBW, VRAM,
and MMIO. The final block preflight nevertheless accepted only Bank 0. Akashic
places its userland dictionary, compiled bodies, and much of its working data
in external RAM, so a valid positive block could be found or built and then
make zero progress on every attempted entry. In the exact 700M--1.2B Desktop
source window, 92,388,072 of 216,495,983 positive admissions made zero progress
(42.674%), leaving native-JIT coverage at 69.321%.

The production correction admits a resolved `EXTERNAL` scalar span at the same
preflight boundary as Bank 0. It does not admit HBW or VRAM, relax full-span or
MMIO checks, add a memory side exit, change timing, or broaden the instruction
set. Exact native-versus-generic tests cover byte traffic, unaligned natural
loads/stores, and a natural access crossing the external aperture; the crossing
case must decline native entry and retain authoritative bytewise routing. In a
diagnostic replay of the same Desktop window, zero-progress positives fell to
1,219,086 of 166,794,121 (0.731%), native-JIT coverage rose to 92.072%, and
lookups fell from 553.467 to 409.026 per 1,000 guest steps. PC, cycles,
HERE/LATEST, pristine image hash, and all guest-JIT counters remained exact.

The independent guest-JIT source fixture then improved from 41.758 to 56.167
Msteps/s with the same 859,562,672 measured steps, external dictionary hash,
and guest-JIT result. Its timed wall interval fell from 20.584 to 15.304
seconds. Three ordinary BIOS+KDOS source loads completed in 1.439--1.562
seconds with exact state and transcript; their 1.528-second median is below
both the retained refined-engine median of 2.041 seconds and pre-engine median
of 2.546 seconds. This benchmark compiles under KDOS's internal `JIT-ON` and
records state zero only after KDOS returns through its final `JIT-OFF`.

Finally, a fresh canonical Akashic-main Desktop source smoke passed all normal
applet interactions after 12,851,500,000 steps in 261.57 seconds. The prior
refined-engine smoke took 402.21 seconds and the retained pre-engine control
took 346.77 seconds. The exact bounded prefix, rather than those historical
full-run step totals, is the causal A/B proof; the completed smoke is the
production-path acceptance. The correction stops here. Remaining unsupported
guest-JIT-emitted opcodes and rejection costs are not expanded speculatively
after the smallest demonstrated fix cleared every independent gate.

Separate rich-host observation (2026-08-27): the retained rich-terminal host
currently rebuilds and recopies an owner's complete object mapping for each
`OBJECT_DEFINE`, making the 23,520-glyph Desktop hidden build quadratic. This
host-model performance defect is recorded only; correction is deferred until
after the execution-kernel refinement is merged and the rich-terminal vertical
again owns the critical path. It does not reopen engine consolidation or
justify a renderer-specific guest workaround.

Post-cutoff ISA clarification (2026-08-27): the architectural documentation now
locks the 1802-derived fetch-then-execute rule. A complete encoding, including
any prefix, advances `R[PSEL]` before execution-phase operands are read;
selector aliases remain legal and follow the instruction's ordered effects.
This selects the existing Python/shared-C++ behavior for PSEL aliases. The RTL
conformance work is intentionally deferred and no emulator, DBT, or RTL source
changed in this documentation-only decision.

### Post-dictionary profile: bounded host-plan associativity

The 2026-08-29 Desktop dictionary profile exposed a separate host-engine
problem after the architectural `EXT.DICT` repair. The exact-single path built
19,711,250 decoded plans and evicted 19,710,226 of them, compiled 8,537,037
native plans and evicted 8,536,586, and stored 21,753,554 exact rejections while
replacing 21,753,042. It republished 2.67 GB of generated code. Those tables
were direct-mapped host memoization structures; they are unrelated to the
guest-visible 1,024-entry `EXT.DICT` hardware cache.

The first bounded correction retains the existing hashes and makes decoded
plans 1,024 sets by four ways (4,096 entries) and exact rejections 512 sets by
four ways (2,048 entries). A stale entry with the same address and selector
identity is replaced first, then the lowest invalid way, then an insertion-only
round-robin victim. Hits do not mutate recency state. Way-major physical slots
give every plan and memory recipe one stable index, and every such index owns
one fixed executable-arena slot. Failed construction still leaves a prior
positive plan untouched.

This is entirely host-only storage. It is neither checkpointed nor included in
architectural hashes, and reset, discontinuous restore, and identity-epoch wrap
discard it. No opcode, cycle, guest I-cache, `EXT.DICT`, BIOS, RTL, or snapshot
contract changes. Host-profile schema 15 publishes both geometries explicitly
so later profiles cannot confuse the two cache families.

Seconds-scale position-balanced motifs established the intended boundary. Four
same-set positive plans ran at 79.4--87.6 Msteps/s versus 17.3--17.7 for the
direct-mapped parent, and two same-set exact rejections ran at 28.2--28.6 versus
10.1--10.3. A stable primary-way plan remained inside the parent run-to-run
range. Deliberate five-entry cyclic thrash was slower than the direct-mapped
parent because it pays the bounded way search without retaining an entry; that
case remains an explicit warning against claiming a production win from the
microbenchmarks. The focused 76-case exact-single suite and ten profile-schema
units pass. Cold source-load and Desktop A/B remain deferred by the active
rich-terminal vertical gate.

### Profile-guided opcode coverage

The first coverage slice admits the existing register-register `AND` semantic
into exact-single blocks and lowers it with x86-64 `AND` plus the established
MP64 logic-flag publication. It also classifies a selected-PC source exactly as
the already lowered register ALU peers do, preserving complete-fetch-before-
execute behavior. This adds no encoding or instruction meaning: shared decode,
the portable interpreter, the Python reference, BIOS, and hardware already
owned `AND`; only host block admission and lowering had omitted it.

A position-balanced three-instruction `AND; INC; LBR` motif improved from
52.8--55.0 Msteps/s at the cache-only parent to 126.9--131.4 Msteps/s. Native
coverage rose from two of every three retired instructions to effectively the
whole loop and halved block admissions. Exact-single, generic-core, and Python
state agree across uneven instruction budgets, zero and negative results,
destination/source aliasing, selected-PC input, and preserved flag bits. The
78-case exact-single selector passes; production source-load attribution
remains deferred with the vertical gate.

### Profile-guided indexed-address coverage

The dictionary index then supplied the evidence previously required by EK-F4:
its byte loops form addresses as `entry base + entry index + constant` before
ordinary `LD.B`. Address recipes now conservatively carry at most two distinct
entry-register sources in canonical order plus a wrapping 64-bit addend. The
existing constant and single entry/prior-read forms remain unchanged. Duplicate
sources, loaded-value composition, a third source, and nonconstant subtraction
become unknown and end construction as before.

Preflight resolves both entry values before native entry and sends only the
final modulo-64-bit address through the unchanged width, MMIO, aperture, and
ordinary-RAM span proof. Generated code still receives only the proven host
pointer by access index; its ABI and hot block-entry stride do not grow. The
parallel eight-access recipe table grows from 72 to 80 bytes, or 32 KiB across
the 4,096-entry host cache.

A position-balanced `MOV; ADD; LD.B; INC; LBR` motif improved from
64.4--75.3 Msteps/s at the `AND` parent to 104.2--107.3 Msteps/s, halving
native admissions while keeping all five guest instructions native. Focused
oracles prove ordinary and wrapping sums, exact-single/generic equivalence,
the prior-read and affine Forth forms, zero-progress fallback, and external
aperture rejection. The full 79-case exact-single selector passes. This is a
host proof optimization over existing MP64 `ADD` and `LD.B` semantics, not a
new indexed-load opcode or hardware addressing mode.

### Profile-guided multiply coverage

The next dictionary-hash boundary was the existing `C2` `UMUL Rd, Rs`
instruction. Shared decode and portable execution now own its low-64-bit
product, four-cycle timing, and Z/N-only flag update, and the exact-single
x86-64 backend lowers that same decoded operation with two-operand `IMUL`.
Signed and unsigned multiplication have the same low half; an explicit host
`TEST` publishes only Z/N because x86 does not define the multiply result's
ZF/SF and MP64 preserves the remaining flags. A destination that aliases PSEL
continues through the scalar path, while a PSEL source sees the architectural
post-fetch value.

Costs one through four use two instruction-indexed bitplanes for an exact
interrupt-truncated prefix. The complete-block path, which is the ordinary
case, reads a precomputed static cycle total from existing header padding.
This arrangement was selected by a negative control: consulting the new
bitplane on every native return slowed the unchanged indexed-load motif by
8--12%. After moving that work off the complete path, a position-balanced
control measured 123.6--129.7 Msteps/s for the candidate and 126.1--127.6 for
its parent, within the host's run-order drift.

On the positive motifs, `UMUL; INC; LBR` improved from 65.7--66.2 to
141.2--150.3 Msteps/s. Native execution grew from two instructions per loop to
effectively the full loop and block lookups halved. A nine-instruction
dictionary-hash loop combining the indexed byte load, XOR, UMUL, AND, and
counter maintenance improved from 68.2--68.3 to 104.0--107.4 Msteps/s.
Exact-single, generic shared-C++, and Python execution agree on wrapped and
zero products, REX timing, flags, PSEL-source slices, PSEL-destination decline,
and an injected interrupt after exactly the four-cycle native prefix. The full
84-case exact-single selector passes.

This is not a hardware-design change. `C2`, its register encoding, product,
flags, and timing already exist in the MP64 ISA, assembler, Python model, and
both RTL cores. The slice moves that existing semantic into the shared host
decoder and adds an x86-64 implementation; it adds no opcode, addressing mode,
architectural cache, register, cycle rule, BIOS ABI, snapshot field, or RTL
requirement.

### Resolve dictionary names as proved spans

The authoritative `EXT.DICT` helper formerly routed the length byte and every
name byte independently through generic system-memory resolution. It now keeps
the counted-length read on that exact route, returns the already-optimal empty
name immediately, and in supervisor non-journaled execution proves the whole
nonempty counted string once under the same supervisor-byte aperture order.
Only then does it copy the name bytes from the resolved host span.

User mode, resumable or strict-cycle bus access, MMIO overlap, guest-address
wrap, Bank 0 aliasing, aperture crossings, and any incomplete proof retain the
original bytewise path. The FNV result, cache lookup or mutation, flags, and
the helper's `length + 2` cycle charge are unchanged. Bank 0, external RAM,
wrap, external-end, MMIO-order, MPU first-fault, empty-name, and checkpointed
strict-replay oracles pass in the complete 23-case focused EXT.DICT file.

Position-balanced `DFIND; LBR` hit loops show the intended name-length scaling.
A six-byte Bank 0 name improved from 15.54--15.75 to 19.20--20.04 Msteps/s.
For a 31-byte name, Bank 0 improved from 5.45--5.49 to 15.78--16.26 Msteps/s
and external RAM from 6.19--6.22 to 16.12--16.28 Msteps/s. These are focused
helper measurements rather than a production source-load claim.

The change is a host memory-resolution optimization, not a hardware feature.
The architectural 256-by-four `EXT.DICT` table, counted-string format, FNV
hash, replacement policy, opcode behavior, cycles, BIOS ABI, snapshots, and
RTL requirements remain exactly as before.

### Reject a local `EXT.DICT` terminal continuation

A follow-on experiment tested whether a decoded native prefix should carry a
host-only hint that the next resident byte was an unprefixed `EXT.DICT`
header. After the prefix completed with budget remaining, the candidate
rechecked the live I-cache byte, reproduced the ordinary interrupt, halt, and
idle boundary, and called the existing authoritative `step_one` helper. The
hint shared the former compile-checked byte and never entered generated code;
the opcode tail, faults, dynamic cycles, flags, and dictionary cache remained
owned by the ordinary extension engine.

The implementation passed exact budget-two, budget-three, and following-
instruction sentinels, reserved-subop and user-MPU trap settlement, and the
complete 87-case exact-single selector. Its deterministic replay did remove
the intended work. Per 20,000 guest steps in a four-instruction BIOS-shaped
`MOV; ADDI; DFIND; BRNE` miss loop, lookups fell from 15,000 to 10,000 and
exact rejection hits from 10,000 to 5,000; native block and JIT work remained
the same 5,000 executions and 10,000 prefix steps.

That lookup reduction was not profitable. Two position-balanced eight-round
runs pinned to one host CPU timed 20,000,000 steps per arm and interleaved an
unmarked four-instruction native control in each process. Across the combined
paired samples, the candidate's median dictionary-to-control CPU-time ratio
was 4.886 versus 4.801 for its parent, a 1.78% normalized regression. Absolute
wall rates moved with host frequency and crossed directions, so they do not
support a contrary speedup claim. The production change and its candidate-
only tests were discarded.

This negative result concerns only host dispatch organization. The experiment
never proposed a guest opcode, cache rule, timing change, BIOS ABI, or RTL
change. Any future successor-edge profiler must also classify an authoritative
extension helper as a barrier rather than infer a native edge across it.

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

A new finding reopens engine consolidation only if it invalidates the
production kernel contract or makes later broad acceptance impossible. Other
findings stay in the deferred ledger until their owning gate permits work.

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
- The authorized pre-Element-8 checkpoint compared the current reworked DBT
  against the old `72e1122` DBT baseline and pre-DBT `3f339eb`, with exact
  state and transcript equivalence. It is an execution-kernel checkpoint, not
  final broad qualification; final acceptance remains subject to the
  rich-terminal and normal resource gates.

## Decision ledger

| ID | Decision | Claim boundary |
|---|---|---|
| EK-D1 | Replace the production kernel in place and use revisions/worktrees as the oracle boundary. | No internal `v2`, old/new selector, compatibility facade, or duplicate production engine is retained. |
| EK-D2 | Separate guest architecture from host architecture: MP64 semantics live under `cpu/mp64`; x86-64 emission lives under `dbt/x86_64`. | No empty future backend, plugin registry, or runtime backend framework is introduced. |
| EK-D3 | Preserve every 1,000-instruction logical round while settling the ordinary no-event case natively. | This changes host custody, not virtual time, device order, interrupt eligibility, or public round accounting. |
| EK-D4 | The portable decoded executor and x86-64 lowering consume one decoded/block representation. | Specialized full/micro execution policies may differ, but instruction meaning and block identity do not fork. |
| EK-D5 | Make guest-cache identity a generic caller-bounded line set. | The observed two-line `LDN+BR` pair is an acceptance motif, not a hard-coded capacity or app-specific fusion. |
| EK-D6 | Use focused happy-path construction evidence until the rich-terminal gate permits final qualification. | A green construction selector is not a broad correctness or performance claim; deferred cases remain explicit. |
| EK-D7 | Judge performance with workload-specific paired evidence and exact equivalence. | No arbitrary universal threshold, historical unpaired median, or sum of inclusive profile timers establishes success. |
| EK-D8 | Normalize decoded and generated execution into one 16-byte MP64 block exit while retaining a backend-private packed transport. | Common exits remain register-returned; authoritative CPU state and already-known raw-write invalidation are not duplicated into every descriptor. |
| EK-D9 | Represent scalar block addresses as ordered entry/constant/prior-read recipes and prove every span transactionally before entry. | The capacity derives from block identity, the hot motifs are not fused, no generated memory side exit is added, and a failed proof retires nothing. |
| EK-D10 | Keep the identity-proved PC in x86-64 R9 when its emitted-byte cost is favorable, tolerating at most one byte of growth for the measured short shapes, and no semantic selector alias exists. | Entry identity supplies the exact initial value; every normal or prefix-interrupt exit reaches one materialization point, while costlier or aliased blocks keep the established lowering. |
| EK-D11 | Close Element 7 without direct native continuation. | The current reworked DBT delivered a material exact-equivalent BIOS+KDOS gain, while measured hot successor edges still require write invalidation or fresh transactional preflight. No chaining state or broader entry/exit ABI is added; reconsideration requires new edge-specific retained evidence. |
| EK-D12 | Let the authoritative decoder and sole block builder own static candidate validity without replaying their construction policy immediately before publication. | Dynamic cache identity, transactional memory proof, and backend validation remain independent; consolidation removes a duplicate static checker rather than weakening a runtime boundary. |
| EK-D13 | Close engine consolidation without decomposing the remaining integration monolith. | CPU/machine state, scheduler, callback, snapshot, profile, and pybind extraction is a separate future organization project; broad acceptance remains deferred under the rich-terminal and resource gates. |
| EK-D14 | Preserve 1802-style complete-fetch-before-execute semantics for every PSEL register alias. | Prefixes are part of one encoding, execution reads the post-fetch PSEL value, selector collisions remain legal and ordered, and RTL conformance is a separate correctness slice rather than an engine change. |
| EK-D15 | Let a segment-local, rejection-indexed hint change probe order only after that segment has observed an exact resident rejection. | The persistent exact rejection entry remains authoritative; a mismatch forgets the hint and restores positive-first admission, while interrupts, scalar execution, identity proof, and persistent cache ownership remain unchanged. |
| EK-D16 | Reject EK-D15 after the ordinary Desktop smoke and replace its two-extrema evidence with a production-calibrated matrix. | The hint's exactness was not sufficient evidence of profitability: it taxed every positive admission, regressed Desktop throughput by 6.261%, and timed out the journey. A future admission change must win the calibrated mix, stable-positive, rejected-scalar, and real BIOS+KDOS comparisons independently before another Desktop run. |
| EK-D17 | Admit resolver-proved external-RAM scalar spans at native block entry. | External RAM shares Bank 0's pinned, directly contiguous execution contract, while full-span, MMIO, aperture-priority, timing, and I-cache invalidation checks remain authoritative; HBW and VRAM are not admitted. |
| EK-D18 | Replace the exact-single host plan and rejection direct maps with bounded four-way tables and stable physical JIT slots. | These caches remain host-only and disposable; the guest-visible 1,024-entry `EXT.DICT` cache, architectural state, timing, snapshots, and RTL contract do not change. Full source-load retention still requires the deferred production A/B. |
| EK-D19 | Admit and lower an opcode only through the existing shared decoded semantic before crediting it as DBT coverage. | Register `AND` is the first slice: its ISA, flags, timing, Python behavior, and hardware contract predate this host-only coverage change. |
| EK-D20 | Represent a profiled base-plus-index scalar address with two distinct block-entry sources and a wrapping addend. | Preflight still proves the complete ordinary-RAM span before native entry; generated memory operations and the MP64 ISA gain no addressing mode or unchecked host access. |
| EK-D21 | Lower the existing four-cycle `UMUL` semantic and settle complete blocks from a precomputed static cycle total. | Two cycle bitplanes retain exact interrupt-prefix timing, while the ordinary complete path gains no extra per-block bitplane check; the ISA, RTL, architectural state, and timing contract are unchanged. |
| EK-D22 | Resolve one complete nonempty `EXT.DICT` counted-name span before copying its bytes. | Only supervisor non-journaled ordinary memory takes the direct path; byte routing, faults, callbacks, dynamic cycle charge, the architectural cache, BIOS, and RTL remain unchanged. |
| EK-D23 | Reject the segment-local `EXT.DICT` terminal continuation after exact counters improved but position-balanced control-normalized timing did not. | Avoid retaining host dispatch complexity from a synthetic lookup-count win; the existing authoritative helper boundary and every guest hardware contract remain unchanged. |

## Deferred findings ledger

| ID | Finding | Disposition |
|---|---|---|
| EK-F1 | Focused happy-path coverage did not exhaust every branch condition, prefix/fault boundary, CALL.L/RET.L register-alias class, natural-width callback route, or strict-cycle replay form. | Resolved in the fourth Element 8 slice with all condition codes, the unambiguous stack-selector alias, CALL/RET fault ordering, ordered natural-width callbacks including a prefixed later-byte failure, and representative one-cycle strict/unbounded equivalence. Deliberate cross-products remain outside the compact consolidation matrix. |
| EK-F2 | Inlined positive and rejection identity checks retained predicates already proved by their sole admission caller. | Resolved in the second Element 8 slice: caller-proved eligibility and selector-range predicates were removed, while exact entry identity and dynamic I-cache validation remain. |
| EK-F3 | The rejection matcher remains behind a positive-cache probe in scalar regions whose starts repeatedly match exact resident rejections. | EK-D15's segment hint is rejected by EK-D16: optimizing the rejection-only extreme imposed a larger production cost. Restore the simpler positive-first path and leave this local inefficiency open unless representative mixed evidence supports a correction that does not tax the common path. |
| EK-F4 | Address provenance deliberately represented one entry, constant, or prior-read source plus an addend, so the dictionary index's independent base-plus-index calculation ended construction before `LD.B`. | Resolved by EK-D20 after the new dictionary profile supplied a concrete hot shape and the paired indexed-load motif showed a material exact-equivalent gain. Broader source algebra remains excluded. |
| EK-F5 | Happy-path construction coverage did not inject either a later-span preflight failure or an IPI between instructions of a multi-memory block. | Resolved in the third Element 8 slice: aliased later-span fallback proves zero block progress, and a one-shot real-router diagnostic proves the exact generated completed-prefix exit before a terminal store. |
| EK-F6 | Multi-memory entry still scans the bounded decoded plan and resolves each scalar span; a prior-read-derived address also rereads its dependency during preflight. | The canonical comparison proves a net engine gain but does not isolate this preflight's contribution. Keep it out of line and change it only if a future focused profile identifies it as material and paired exact evidence supports the change. |
| EK-F7 | RTL old-value/nonblocking-write behavior differs from the architectural post-fetch register view when an execution operand or destination aliases PSEL; `CALL.L` first exposed the mismatch. | ISA decision resolved by EK-D14 and documented in `isa-reference.md`. Python/shared C++ already implement the selected rule and exact block lowering declines semantic selector aliases; focused RTL conformance and correction remain deliberately unimplemented here. |
| EK-F8 | Four-way host caches collapse two-to-four-way conflict motifs but add work to a set that cycles through five or more live identities. | Retain the bounded implementation as the first profiled candidate, expose exact geometry/churn counters, and decide any associativity or capacity adjustment from a position-balanced cold source-load profile once the rich-terminal gate permits it. |
| EK-F9 | A decoded native prefix followed by `EXT.DICT` still performs another ordinary admission whose start normally matches an exact rejection. | The local terminal continuation removed that admission but regressed the control-normalized paired timing by 1.78%. Leave the boundary explicit unless a future design eliminates more than the lookup while preserving helper, interrupt, fault, and successor-edge barriers. |
