# Initial hybrid runtime ABI

Status: implemented and qualified for the bounded integer-routine profile,
2026-09-30. See the implementation ledger in `docs/unified-runtime-plan.md`
for the checks run and capabilities that remain outside this profile.

ABI identity: `megapad.hybrid.integer-routine`, version `1`.

This contract refines Phase 4 of `docs/unified-runtime-plan.md`. The first
profile calls declared, bounded MP64 integer routines from a semantic
MegaForth runtime. One session owns the memory image, the semantic runtime,
and one architectural full core. The semantic dictionary remains authoritative
for source execution. This profile does not boot a native BIOS, execute an
arbitrary native dictionary, or translate semantic XTs into machine addresses.

## Scope and dependency direction

The composition layer may import both engines. Backend-neutral backing and
ABI value types may live in `shared/`; they must not select or import an
execution engine. The existing dependency remains
`emulator -> shared <- simulator`.

The admitted machine profile initially contains only the integer operations
already implemented by the shared decoded MP64 interpreter, subject to the
restrictions below. Machine code cannot call semantic words, access MMIO,
perform port I/O, invoke host acceleration hooks, change selectors or service
CSRs, execute tile/crypto/FP operations, suspend, reset, or enable interrupts.
Those are explicit profile errors, never a request to enter an uncontracted
Python fallback. Semantic execution retains its existing services.

The public launcher must continue to reject hybrid mode until the backing,
runner, integration, and acceptance gates in this document pass. A kernel
test or host registration API alone does not qualify the complete application.

## Source constraints behind the design

The current implementation imposes several concrete constraints:

- `simulator/memory.py` owns ordinary-memory geometry and sparse pages.
  Missing pages read as zero. Complete ordinary spans cannot cross regions
  or wrap through the uint64 boundary.
- `simulator/accel/semantic_executor.cpp::MemoryRun::resolve_page` accepts
  page-sized Python bytearrays. Substituting memoryview pages without changing
  this contract would cause native semantic operations to decline.
- `emulator/accel/machine/memory.h::GuestMemoryMap` uses contiguous region
  pointers. Ordinary architectural accesses deliberately permit Bank-0 modulo
  aliases. `sys_read*` and `sys_write*` can reach native MMIO devices before
  the legacy MPU window is checked. MPU setup alone cannot enforce this ABI.
- `emulator/accel/cpu/mp64/decode.h` and `interpreter.h` already provide
  `decode_instruction` and `execute_decoded_instruction`. The latter accepts
  an operations adapter for its memory accesses. There is no need for another
  decoder or another implementation of integer instruction semantics.
- `simulator/stacks.py::ReturnStack` associates a slot with a continuation
  only while its raw cookie matches. Popped slots and their metadata remain
  available to later `RP!` restores. Copying only active cells or recreating
  continuations would lose observable state.
- `Dictionary.execution_generation` changes on word publication and rollback,
  but not on all `ALLOT` or `move_here` actions. `RegionAllocator` currently
  has no allocation generation. Existing counters alone cannot prove a
  machine-code allocation is still the same allocation.

## One backing image

Hybrid construction creates fixed-capacity ordinary-region buffers before
either runtime is created. Bank 0 and each configured external, VRAM, or HBW
region have their own contiguous buffer; address-space gaps allocate nothing.
Only configured regions exist. Each buffer starts at zero and remains pinned
for the entire hybrid session. Guest addresses retain their existing values.

The shared backing owner allocates these buffers itself and rejects aliases
between physical regions. It retains its own private buffer pin and gives
clients independent views, so releasing one client's view cannot detach the
owner's pin. A `dense_backing` construction argument must match the exact
configured region bases and sizes. The native semantic constructor retains
the existing sparse descriptors and adds an optional `dense_regions=()` input
whose elements are `(base, size, buffer)` with explicit retained leases.

`SparseAddressSpace` gains an explicit dense-backing construction option and
a region implementation with the same qualified read, write, integer, fill,
and cell-span methods. Keep its exact outer type so existing canonical-memory
qualification remains meaningful. Ordinary simulator construction continues
to use sparse pages. Hybrid construction must not change backing after the
dictionary, stacks, services, or native semantic program have obtained views.

The native semantic program gains an explicit pinned contiguous-region input
alongside its existing sparse-page input. A dense region resolves to its
buffer and offset directly. It must retain a writable, contiguous byte-buffer
lease and reject wrong capacities or malformed exporters before publication.
This is preferable to treating generic memoryviews as bytearray pages. Internal
page views are permissible, but they must refer to the same buffer and cannot
be independently replaced, detached, or resized.

The architectural core attaches those exact exporters through its existing
mapping leases. There are no per-transition memory copies, page reconciliation
passes, or authoritative shadow images. Construction of a new hybrid session
is explicit; converting an already-running sparse simulator is outside v1.

Zero reads remain zero. Hybrid memory is densely backed, so its diagnostic
resident-page count represents configured backed pages rather than pages
materialized by guest writes. Pure simulator allocation and absent-page
diagnostics retain their current behavior. No claim of equal host RSS follows
from equal guest memory geometry.

The composition owner excludes semantic execution, machine execution,
registration, backing replacement, and direct host mutation from each other.
The initial machine runner holds the GIL and the existing architectural memory
admission for its bounded interval. It performs no Python callbacks. Frame
publication and host events occur after the interval has settled. Releasing
the GIL is a later optimization requiring a complete ownership proof.

## Declarations and allocation lifetime

A host registration creates an ordinary semantic primitive word and binds its
exact `Word` identity to an immutable `RoutineDeclarationV1`. The declaration
contains:

- ABI identity/version and a session-owned registration nonce;
- an allocation lease, its generation, sealed code span and entry offset;
- input and output cell counts, each in the range zero through eight;
- buffer rules derived from entry arguments, with checked length expressions,
  maximum lengths, and read/write permissions;
- a bounded private machine return-stack span and its separate control lease;
- positive per-call and cumulative-dispatch instruction limits.

Version 1 applies finite host-validation limits before allocation or image
publication:

| Item | Maximum |
|---|---:|
| Code image per routine, including padding | 1 MiB |
| Private machine return stack | 8,192 cells / 64 KiB |
| Machine instructions per call | 1,000,000 |
| Machine instructions per outer dispatch | 10,000,000 |
| Buffer rules per routine | 16 |
| Routines per session, including manifest registrations | 64 |
| Total code image bytes per session | 16 MiB |
| Private control arena per session | 4 MiB |
| Manifest file size | 1 MiB |

Code must be nonempty, stack capacity and instruction limits positive, and
all sizes exact host integers rather than booleans. Actual memory geometry,
dictionary capacity, and stack protection can impose smaller limits. Runtime
budgets may only reduce configured limits. Larger values require a reviewed
ABI/profile change; they are not an unchecked launcher override.

An entry address lies inside the declared code span. Code is placed in an
aligned, padded body allocation of the registered semantic word using the
dictionary's existing `initial_body` publication. The declaration records the
complete owned body extent, not just its eight-byte semantic code slot.
The machine return stack is not stored in that word body. Padding makes
architectural instruction-cache line fills stay inside mapped ordinary
memory; padding is part of the sealed code image when executable.

The composition/runner owner allocates a separate fixed private control
buffer at session construction, with capacity no greater than 4 MiB. It maps
this buffer to an aligned logical control span disjoint from every shared
ordinary region, semantic stack allocation, and the MMIO aperture. Prefer a
gap immediately after the configured external region, then another suitable
gap; reject construction if no nonwrapping gap fits. The span is absent from
semantic address geometry and SysInfo, so source allocators cannot discover
or allocate it. It is native control state, not a second copy of guest bytes.

Each registration leases one bounded stack slice from this arena. The owner
pins the buffer for the session; a control lease binds the slice, registration
identity, and allocation generation. Revocation or slice reuse cannot revive
an older lease. Arena exhaustion fails registration before publication; it
never resizes or remaps the buffer. Only the checked `CALL.L`/`RET.L` control
roles access it, routed directly by the runner's operations adapter before
ordinary `GuestMemoryMap` routing. Ordinary loads, stores, instruction fetches,
and semantic accesses never resolve to this private control buffer.

The architectural differential fixture may use one merged external buffer
covering shared external data followed by the private control span. That
reference placement tests the same stack addresses and machine effects;
production semantic geometry must still expose only the shared data portion.

The dictionary must provide a body-allocation lease with an identity distinct
from a numerical address. A backward `ALLOT`, zone/frontier move, rollback, or
replacement that reclaims or overlaps that allocation revokes the lease
before its storage is reused. Reusing the same address and identical bytes
does not revive an old lease. Adding an unrelated definition need not revoke
a live routine: dictionary generation changes trigger exact identity and
allocation revalidation. Implement this allocation notification before
enabling machine dispatch; a hash alone is not an allocation identity.

The narrow dictionary interface is `acquire_body_lease(word)` plus
`is_body_lease_live(lease)`. A lease records the owning dictionary, exact Word,
initial body extent, and a unique allocation serial. Revocation covers
allocator-owned writes as well as rewinds: definition publication, comma,
`C,`, and transient dictionary writes must revoke any older body allocation
they overlap, even when the new bytes happen to be identical. Runtime
`ALLOT` can use `move_here`, so hooking `Dictionary.allot` alone is insufficient.
Ordinary data stores do not redefine allocation identity; code sealing handles
their executable-byte consequences separately.

Reclamation is zone-aware. A backward frontier in one zone reclaims only its
intersecting old allocation range. Leaving an arena does not reclaim it merely
because another arena has a lower numerical address. Reopening a zone at an
earlier frontier, or expanding its writable frontier over an old allocation,
revokes affected leases before reuse. Failed mutation preflight leaves leases
unchanged. Removing a word through `LATEST!` revokes its lease even if `HERE`
does not move.

The code bytes are sealed at registration. Entry revalidates the live word,
session token, code and control allocation generations, bounds, and sealed
bytes. Raw semantic writes that alter code cause `stale_code` at the next
attempted entry. Machine
stores to executable bytes are rejected before the store. Version 1 provides
no guest registration, arbitrary body-to-entry conversion, or self-modifying
code support.

Unknown XTs still fail in `_resolve_dispatch_word`/`Dictionary.resolve`.
Registration does not add a fallback for an unknown numerical XT. A new word
at a reclaimed XT must receive a new registration. Closing the session
revokes all registrations before releasing backing leases.

## Versioned host API and source invocation

The composition package `hybrid/` owns `HybridRuntime`, `HybridSession`, and
the declaration registry. It imports the backend adapters; neither backend
imports it. Shared immutable value types include `BufferRuleV1`,
`RoutineDeclarationV1`, and `MachineRoutineResultV1`. The backing owner and
its lifetime interface are also backend-neutral.

The initial host entry points are explicit and versioned:

```python
hybrid = HybridRuntime.create(
    geometry=memory_geometry,
    semantic_executor="native",       # also "python" or "auto"
    dispatch_instruction_limit=1_000_000,
)
word = hybrid.register_routine_v1(
    name="HYB-CHECKSUM",
    code=assembled_mp64_bytes,
    entry_offset=0,
    input_cells=2,
    output_cells=1,
    buffers=(BufferRuleV1(
        address_argument=0,
        length_argument=1,
        element_bytes=8,
        max_bytes=65536,
        access="read",
    ),),
    max_instructions=65536,
    return_stack_cells=128,
)
report = hybrid.evaluate(
    "SAMPLES 3 HYB-CHECKSUM",
    semantic_step_budget=1000,
    machine_instruction_budget=100000,
)
```

The example assumes the image implements a sum of unsigned cells and
`SAMPLES` already names three caller-owned cells outside the semantic stack
allocations, for example in a configured external-memory region. Argument
indices are zero-based in signature order. Length is the original unsigned
length cell times `element_bytes`; multiplication, guest-address addition,
and configured maximum are checked without wrapping. `access` is `read`, `write`, or
`read_write`. The registration API receives assembled bytes, not host code
or a Python callback supplied by the manifest. It returns the actual semantic
word, including its ordinary XT. No separate machine `EXECUTE` vocabulary is
introduced in v1.

Images are position-independent within their declared code span: relative
branches and PC-relative target construction can implement local control
flow, and shared data addresses arrive as arguments. The initial loader does
not relocate absolute code addresses or resolve external symbols. Code padding
uses MP64 `NOP` bytes and belongs to the sealed image.

Source can interpret the registered name, compile a normal call to it, or
obtain its XT and use the existing `EXECUTE`. The native semantic planner
leaves the new primitive as an explicit stop and resumes the same dispatcher
boundary after the bridge completes. `>BODY` does not make the returned code
address a callable semantic XT. `JIT-ON`/`JIT-OFF` retain their existing meaning.

The bridge host methods establish the cumulative machine allowance for the
outer dispatch. A smaller per-invocation budget may reduce the configured
limit. Nested evaluation and resumed host quanta inherit that same allowance
and cannot reset it. The report contains the existing semantic result plus
separate machine instruction/cycle counts and transition counts.

The first application invocation is opt-in:

```console
python megapad.py --mode hybrid --storage hybrid.img --executor native --hybrid-routines routines-v1.json
```

Other admitted source/image/session arguments retain the simulator frontend's
meaning. `--executor` selects the semantic implementation; the declared machine
routine always uses the architectural native interpreter in this profile.
Both native extensions must match the Python adapters when native semantics
are selected. Missing or incompatible ABI support fails at creation.

`--hybrid-routines` accepts a local JSON manifest with this schema:

```json
{
  "abi": "megapad.hybrid.integer-routine",
  "version": 1,
  "dispatch_instruction_limit": 1000000,
  "routines": [{
    "name": "HYB-CHECKSUM",
    "image": "checksum.bin",
    "entry_offset": 0,
    "input_cells": 2,
    "output_cells": 1,
    "buffers": [{
      "address_argument": 0,
      "length_argument": 1,
      "element_bytes": 8,
      "max_bytes": 65536,
      "access": "read"
    }],
    "max_instructions": 65536,
    "return_stack_cells": 128
  }]
}
```

Image paths resolve relative to the manifest. Unknown schema fields, duplicate
routine names, unsupported ABI versions, malformed spans, and invalid bounds
are errors. Read and validate all images/declarations before publishing any
routine or starting guest source. Registration occurs after the semantic BIOS
vocabulary is installed and before user source/autoexec is executed. Loading
the manifest must not execute a machine instruction. Boot-source name
shadowing follows normal dictionary rules; an existing XT stays bound to its
exact word until that word is removed or its allocation is revoked.

Hybrid status reports `mode=hybrid`, the selected semantic executor, the
machine interpreter and ABI version, declared-routine availability, and
separate work counters. Its capabilities explicitly deny arbitrary machine
XTs, callbacks, machine MMIO/services, native BIOS boot, multicore execution,
and native snapshot compatibility. Dense backing is a hybrid construction
choice, not a new default for simulator sessions.

## Entry ABI and semantic stack effects

The semantic signature is `( x0 ... xN-1 -- y0 ... yM-1 )`. Inputs are mapped
in that order to `R4` through `R11`; outputs are read from the same register
range. Arguments that describe buffers remain ordinary guest addresses.
Buffer access permissions are resolved once from the original argument cells,
not from registers that the routine can later change.

Before the first machine instruction, the bridge validates registration,
argument depth, final output capacity, all borrowed spans, code and private
stack bounds, and the available execution allowance. It peeks at the argument
cells without popping them. No machine instruction runs after failed entry
preflight.

Entry initializes all general registers to zero except argument registers,
`R3` for the entry PC, and `R15` for the private machine stack. Selectors are
`PSEL=3`, `XSEL=2`, and `SPSEL=15`; ordinary width is 64 bits. Flags, prefix
state, halt/idle state, and interrupt enable start clear. There are no pending
architectural interrupts or trap handlers in this profile. `R14` is scratch;
it is not the semantic data-stack pointer. No caller-visible GPR preservation
is promised beyond the declared outputs.

The bridge never installs the semantic data or return-stack pointers in the
machine core. Machine buffer admission excludes the entire semantic data-stack
and return-stack allocations, including inactive slots retained for `SP!` and
`RP!`, live
dictionary headers/code slots, registered executable spans, and private
machine-control storage belonging to any registration. Declared ordinary
data buffers may overlap each other; permitted reads/writes occur in exact
instruction order. A borrowed writable span must be caller-owned data, not a
way to expose dictionary allocation or continuation authority.

With the current default main-context stack geometry, these protected stack
allocations cover Bank 0. The first shared-buffer examples therefore use a
configured external, VRAM, or HBW region. Code fetches may target the sealed
word body in Bank 0. Private machine call-stack accesses target only the
separate private control arena. These roles never grant ordinary buffer
permission to protected bytes. Do not relax stack protection merely to admit
an address.

Host-created contexts must use this same canonical memory. Wrapper calls and
routine entries introduce their complete stack allocations automatically;
`hybrid.register_context(context)` introduces an otherwise inactive context
before host code permits any routine to borrow from its arena. Once introduced,
each stack allocation remains protected while its stack object is alive, even
when another context is current. This registry holds weak stack references and
does not keep abandoned contexts alive. Raw host allocations that have never
been introduced are the host caller's responsibility; ordinary source execution
uses the already registered main context.

After successful return, the bridge replaces the N inputs with the M outputs
using the already-qualified semantic stack capacity. No service state is
copied. Both semantic return-stack bytes and its continuation map, cookie
counter, pointer, and pointer-capture generation remain untouched by the
bridge. This preserves caller loops, user `>R` cells, and previously captured
`RP@`/`RP!` frontiers across a machine call. Only the declared input/output
replacement changes semantic data-stack cells; unrelated inactive `SP!`
frontiers retain their bytes.

## Machine execution and root return

Implement a bounded runner inside the existing architectural extension. Use
the existing `CPUState`, instruction-cache fetch path, shared decoder,
`execute_decoded_instruction`, and decoded-instruction accounting. A checked
operations adapter supplies ordinary reads and writes and always declines
architectural host call acceleration. Do not enter the general wrapper's
Python fallback, trap-handler continuation, or system scheduler.

The initial opcode set is the existing `DecodeStatus::DECODED` integer set,
excluding `SELECT_PROGRAM_COUNTER`. Deferred extension/device operations are
unsupported. Instructions may branch or write the fixed PC register, but
every subsequent instruction fetch must remain in the declared executable
span. Selector changes are unsupported. Direct register writes to `R15` are
unsupported; only the admitted `CALL.L` and `RET.L` semantics advance the
private machine stack. Other scratch registers remain available.

Instruction reads and data accesses are checked before architectural modulo
aliasing or device dispatch. Every scalar data access must fit completely in
one permitted ordinary span without uint64 wrap. `CALL.L` and `RET.L` use a
separate stack role that checks the private machine-stack bounds and alignment.
Ordinary buffer loads/stores cannot edit machine return cells. These checks
are profile admission rules, not a change to ordinary emulator addressing.

Nested machine calls are allowed within the declared code span. Semantic
callbacks and calls into other registrations are not. The bridge initially
pushes a host-owned root-return sentinel onto the private machine stack. A
successful root return requires `RET.L` to pop that sentinel from the original
root slot, restore the exact empty private stack pointer, and retain the fixed
selectors. A branch to the sentinel or an unbalanced return is an error. The
sentinel is recognized before fetching at its address; it is not a semantic
XT or an executable guest callback trampoline.

The core's instruction cache persists across calls. Per-entry register
initialization must not call a reset routine that clears cache state. DBT is
not part of v1; using the common interpreter is enough to establish the ABI.

## Outcomes, partial effects, and bounds

The native runner returns a structured `MachineRoutineResultV1` with an exit
kind, completed instruction and cycle counts, entry/failure PC, and applicable
trap or access details. Exit kinds distinguish normal return, instruction
limit, unsupported instruction, rejected access, architectural decode fault,
invalid return, and cancellation. The composition layer additionally reports
invalid or stale registration/code before native entry.

For any failure after entry, completed ordinary memory stores remain visible.
There is no implicit memory transaction or retry. The semantic input cells
are not replaced with outputs. Private machine register/stack effects and
fetch/cache observations are retained for diagnostics; they are not copied
onto semantic stacks. A rejected complete data span performs no bytes of that
access. An instruction may already have performed earlier architectural
effects, such as the `CALL.L` stack-pointer decrement before a failed store.
Counts include only completed instructions, following existing machine batch
accounting.

The bridge raises a typed `HybridExecutionError` derived from semantic
`ExecutionError` after settling the result and counters. Existing semantic
dispatch guards then perform their normal primitive-failure cleanup. Version 1
does not silently translate a profile error into a guest `FAULT-XT!` callback
or synthesize architectural trap entry. Machine-to-semantic fault callbacks
need the later callback contract.

The runner checks a positive instruction allowance before every instruction.
The allowance is the minimum of the declaration's per-call bound and the
remaining machine-instruction allowance for the outer semantic dispatch.
Nested host dispatch cannot renew that cumulative allowance. The return
instruction consumes one instruction from the allowance; a completed root
return on the last permitted instruction succeeds. A routine that fails to
return within the allowance terminates with an instruction-limit result.

The admitted integer instructions have bounded work per instruction. There
are no variable-length bulk, device wait, or host callback operations in v1.
Cancellation and close are admitted at settled boundaries; they cannot detach
buffers during a native interval. A pending close waits for at most the
current bounded interval and then revokes further entries. Configured bounds
must also preserve the application's existing host watchdog and quantum
latency expectations; instruction counts are not host-time guarantees.

## Timing, services, events, and cache publication

The semantic dispatcher charges its normal source operation/call steps for
invoking the registered primitive. Machine instructions and MP64 model cycles
are separate counters; neither is added to the semantic step count or treated
as hosted timer ticks. A machine call does not reset or replace the enclosing
semantic budget. Status must expose both domains and the actual profile.

The hosted timer retains its semantic-step policy. RTC retains its existing
manual or host-monotonic policy: host time may elapse during a machine call,
whereas a manual RTC changes only through its existing explicit mechanism.
Machine code cannot read or modify either service in v1. Host input and device
events are released through the existing owner at settled boundaries, never
halfway through a machine instruction interval.

The semantic runtime remains the sole authority for scalar FP state, tile
state, and every hosted device service. Their machine counterparts are not
admitted or synchronized. Integer-only machine execution cannot mutate them.
There is no second active service state that must later be reconciled.

Publishing a registration is an explicit code-publication operation. Once its
new allocation and sealed bytes have been installed, it explicitly invalidates
the architectural cache lines covering that code span before making the
registration callable. Reusing storage requires a new allocation generation
and a new publication operation. Merely writing backing bytes does not flush
the I-cache. Unpublished code edits fail v1 entry validation; they do not
silently make new bytes executable. Cache invalidation and allocation/code
identity are separate facts.

This restriction preserves the architectural cache's intentional
noncoherence while keeping v1 code immutable. Supporting callable modified
code while old cache lines remain resident is a later profile extension,
requiring its own publication and invalidation contract.

## Implementation sequence and acceptance gate

Commit this contract before execution changes. Implement in independently
reviewable slices:

1. Add fixed backing ownership and dense ordinary-region support, then native
   semantic contiguous-region support. Qualify byte sharing, lifetime,
   geometry, and unchanged sparse behavior before attaching a machine core.
2. Add dictionary body-allocation leases/revocation and immutable routine
   declarations, plus the private control arena and registration-bound stack
   leases. Qualify rollback, backward frontier movement, XT/address reuse,
   arena isolation/exhaustion, failed publication, and close without executing
   machine code.
3. Add the bounded architectural runner with checked operations, cache
   publication, private return stack, and structured outcomes. Compare its
   admitted operations with ordinary MP64 execution.
4. Add the composition bridge and semantic primitive registration. Qualify
   stack settlement, existing continuation ownership, counters, and budgets
   with both Python and native semantic execution selected.
5. Integrate the qualified profile with session creation, status, and the
   unified launcher. Preserve current emulator/simulator modes and their
   launch behavior. Hybrid help/status must state its actual capability set.

The acceptance gate must include:

- one assembler-produced buffer routine and one nested-call routine, compared
  with pure MP64 for results, permitted memory effects, instruction counts,
  and model cycles; repeated transitions use the same backing buffers;
- bidirectional shared-byte visibility, all admitted region edges, overlapping
  declared buffers, uint64 wrapping rejection, absent optional regions, and
  prohibited Bank-0 aliases/MMIO/port/service accesses with no device effects;
- private control-span gap selection, pinning, lease reuse/exhaustion, absence
  from semantic geometry/SysInfo, rejection of ordinary accesses to control
  bytes, and unchanged inactive semantic data/return-stack slots;
- zero/eight arguments and results, stack underflow/overflow preflight,
  interior instruction/decode faults, exact-limit return, infinite-loop
  bounds, invalid return, and completed-prefix memory effects;
- entry from colon calls and loops with user return cells, continuation
  cookies and inactive slots, plus saved `RP@` frontiers subsequently restored
  by `RP!` and inactive data cells subsequently restored by `SP!`; no
  bridge-created continuation is left on failure;
- unknown XT, stale word identity, allocation rollback/reuse, backward
  `ALLOT`, code edits, republishing reused addresses, explicit I-cache
  invalidation, and persistence of unrelated cache lines across calls;
- source-step/timer independence from machine instruction/cycle counts,
  manual and host RTC behavior, cumulative limits, queued cancellation,
  session close, ownership rejection, and no Python fallback or callback;
- default sparse simulator behavior and both existing application launch
  modes, followed by focused hybrid session startup/run/close qualification.

Run checks through the existing Make targets, sequentially with other native
builds/tests and with an isolated runtime namespace. The gate proves this
bounded functional profile. It does not prove full-system cycle equivalence,
multicore behavior, arbitrary binary compatibility, full Desktop acceptance,
or a performance improvement.

## Remaining compatibility work

No product decision needs to block this initial contract. The narrow choices
above follow the locked Phase 4 scope. The remaining work is implementation
and qualification, including the new body-allocation lifetime mechanism.

Later profiles must explicitly resolve machine access to the live Forth
stacks, nested semantic callbacks, `THROW` and machine trap callbacks, IDL and
wake, service state, arbitrary code mutation, native dictionary/JIT authority,
and snapshot compatibility. Version 1 must reject those capabilities rather
than accidentally acquiring them through a general emulator fallback.
