# Expanded hybrid interoperability plan

Date: 2026-09-30

Status: locked implementation plan for Phase 5; no capabilities in this
document are implemented or qualified by writing this plan. The implemented
`megapad.hybrid.integer-routine` version 1 contract in
[`hybrid-runtime-abi.md`](hybrid-runtime-abi.md) remains unchanged.

## Recommended next slice

Add an opt-in version 2 profile for synchronous, declared machine-to-semantic
callbacks. Start with exact canonical, total integer primitives such as
`MIN`, `MAX`, `ABS`, `AND`, `OR`, and `XOR`. A machine buffer loop can use one
of these semantic operations as a declared policy callback. Then admit a
small, statically closed colon policy such as a signed clamp. This provides a
real two-way boundary without requiring task exceptions, source evaluation,
devices, or a second scheduler in its first gate.

The architectural runner must **return a callback request to its owner**. It
must not invoke Python, a semantic word, or a device while holding its machine
execution admission. The composition layer runs the callback and explicitly
resumes the parked machine invocation. The callback receives only its declared
argument cells in a private semantic context. The outer semantic caller's
data and return stacks retain the v1 contract until the machine routine
finishes normally.

Do not begin with arbitrary callback XTs, guest JIT, machine MMIO, or a catch-all
Python fallback. Those do not have the lifetime, effect, or continuation proof
needed by the existing runtime.

| Slice | New capability | Required boundary |
|---|---|---|
| 5A | Declared synchronous leaf callbacks | Exact exports, sealed call sites, one parked machine frame, finite callback count |
| 5B | Closed colon policies and nested transitions | Transitive admission, shared root allowances, bounded native frame saves and disjoint private stacks |
| 5C | Defined callback failures; later task exceptions | Typed failure settlement first; a separately locked shared-task profile before cross-boundary `CATCH`/`THROW` |
| 5D | Suspension and wake through a callback | Dispatcher-owned foreign continuation, one composite suspension, existing wake authority |
| 5E | Selected hosted services | Explicit service effects, bounded operands, one existing semantic service owner |

The order is intentional. A synchronous callback API can be useful before a
general Forth callback is safe. Each row is a separately qualified capability;
later rows are not implied by admitting earlier ones.

## Source constraints that control the design

- `hybrid/runtime.py::_invoke` currently runs one synchronous primitive,
  qualifies its buffers, preserves semantic inputs until success, and settles
  completed machine work before raising. Its weak meter-keyed allowance
  survives semantic quanta, nested evaluation, and raw semantic entry.
- `RoutineRunnerV1` in `emulator/accel/mp64_accel.cpp` owns one `CPUState`, one
  pinned private control arena, and a nonreentrant `ActiveBoundary`. Its run
  interval uses the shared decoder/interpreter and keeps the GIL; only lock
  acquisition releases it. Every call currently initializes the integer
  register state and one registration's private return stack.
- `simulator/runtime.py::PrimitiveCallback` currently returns `None` or
  `Invoke`. A normal host primitive cannot leave a resumable operation behind
  merely by returning an object or unwinding its Python frame.
- `MegaForthRuntime._meter_for_public_call` shares the existing semantic meter
  with nested dispatch and rejects replacement budgets. `run_until_blocked`
  rejects nested resumable dispatch. The runtime owns exactly one
  `_SuspendedExecution`, with exact stack evidence, a dispatch root, a meter,
  a cursor, capture state, and one-shot wake receipts.
- `simulator/stacks.py::ReturnStack` retains cookies and continuation metadata
  below its active frontier for later `RP!`. Root and fault-abort
  `Continuation` objects have distinct meanings. Saving a list of active
  cells is not a complete continuation snapshot.
- The checked-in KDOS exception source in
  `tests/simulator/fixtures/kdos-exceptions-618-675.f` selects `HANDLER` by
  `TASK-ID`/`COREID`, not Python `ExecutionContext` identity. A private callback
  context with different stacks cannot safely inherit the caller's handler
  cell. This rules out invoking unrestricted KDOS `CATCH`/`THROW` in an
  isolated callback context.
- `InstructionFault` enters the existing `FAULT-XT!` path; `ForthAbort` carries
  an origin context; `_GuestControlTransfer` may consume an older dispatch
  root. None of these is interchangeable with a machine profile rejection.
- Scalar FP's authoritative FPCSR belongs to
  `simulator/scalar_float.py::HostedScalarFloatService`. Hosted device
  transaction state belongs to the existing platform services. A callback
  must use those instances, not create architectural counterparts to merge
  afterward.

Existing focused evidence to extend includes `tests/test_native_hybrid_routine.py`,
`tests/test_hybrid_runtime.py`, `tests/test_hybrid_registration_failure.py`,
`tests/test_hybrid_session.py`, `tests/simulator/test_kdos_exceptions.py`, and
the simulator suspension, return-stack, native execution, and session tests.

## Versioned exports and machine call sites

Use the existing ABI family with explicit version 2 values and a separately
versioned manifest schema. A v1 loader/runner continues to reject v2 inputs;
absence of a v2 capability must never silently select v1 or an unrestricted
fallback. Keep value types in `shared/`, semantic admission in `simulator/`,
native execution in `emulator/`, and their composition in `hybrid/`.

A semantic export records an exact live `Word`, an opaque owner/registration
identity, its input/output arities, effect class, and finite local limits.
The first export table is captured from canonical installed core words before
user shadowing. A same-named host primitive is not canonical. Later colon
exports retain the exact word identities in their admitted static call
closure, including any registered machine targets.

Bind names once at host publication. Redefinition changes subsequent lookup
but not an already-bound export. Removal, rollback, XT reuse, a replaced
implementation, or loss of a required machine allocation lease invalidates
the affected export. Recheck exact identities before every callback and every
resumed machine segment. Dictionary generation is a useful reason to recheck,
not sufficient proof of either liveness or revocation.

Machine code does not pass an XT, Python callable, or service address to request
a callback. Each sealed routine declares a bounded table of callback sites:

| Field | Meaning |
|---|---|
| `call_offset` | Instruction boundary of an admitted `CALL.L` within the sealed image |
| `stub_offset` | Local, sealed callback stub beginning with the exact `RET.L` instruction |
| `export_id` | Owner-bound semantic export, resolved before routine publication |
| Arity and limits | Must agree with that export and be no greater than session limits |

The actual `CALL.L` target must equal that site's declared stub address. The
stub remains in the routine's existing executable span; no semantic XT is
made executable and no extra private executable memory mapping is needed.
Programs can compute the local target from the fixed PC register using
ordinary admitted instructions. This plan does not add automatic relocation
or external symbol resolution to the loader.

Publication verifies instruction boundaries, the `CALL.L` site and `RET.L`
stub encodings, disjoint metadata entries, all export bindings and all limits
before making the routine callable. Image-dependent validation necessarily
follows bounded image reads; all independent JSON/path/name/limit validation
still precedes those reads. Publication and rollback retain the current
dictionary-plus-side-index transaction and fail-closed repair behavior.

## Native request and resume protocol

Keep v1 `run` unchanged. Add a versioned runner interface with the conceptual
operations `begin`, `resume_callback`, and `cancel_invocation`; exact method
names can follow repository conventions. Result variants are ordinary
completion/failure, a callback request, and eventually a host-quantum yield.
A request is not a completed machine call.

1. Execute the declared `CALL.L` using the existing decoder/interpreter. Its
   private-stack decrement/store, PC change, cycles, and completed-instruction
   count are real architectural effects. A failed call-stack access yields
   the ordinary partial-effect failure and creates no callback request.
2. Recognize arrival at the declared local stub only from that exact completed
   call site. Stop before fetching or executing its `RET.L`. A branch,
   fallthrough, altered target, forged return, or a different `CALL.L` landing
   at a callback stub is an explicit profile failure, not callback authority.
3. Settle the segment's instruction/cycle deltas and return a request holding
   the exact invocation identity, monotonically increasing request sequence,
   site/export identity, bounded `R4..R11` arguments, and an opaque native
   continuation token. Native state and its private return cell stay owned by
   the invocation. Release its execution admission before semantic dispatch.
4. The composition owner revalidates the export and budgets, executes the
   admitted semantic callback, and checks exact output arity and balanced
   callback return state. No argument/output values go onto the outer task's
   stacks in the isolated-context profile.
5. Resume accepts only the matching unconsumed request token and exact uint64
   output tuple. It updates only the declared output registers, preserves
   other integer registers and flags, then executes the real sealed `RET.L`
   through the existing interpreter. That return consumes its normal
   instruction allowance and cycles. The callback itself invents neither a
   machine instruction nor a hardware cycle charge.

The token is bound to owner, invocation, publication, callback sequence, and
the expected control-stack slot. Copies, foreign tokens, replayed replies,
wrong output arity, and replies after cancel/close fail before mutation.
Resume is not a new invocation: it must not call `initialize_entry`, reset
the I-cache, rewrite the root sentinel, renew limits, or rerun the prefix.

In 5A, a parked invocation excludes any other machine entry or registration.
The bridge drives callback/resume synchronously under the existing semantic
owner boundary. It does not admit new host input between these segments.

## Semantic admission and bounded callback state

For the first gate, expose only a small explicit table of total canonical
integer primitives, with their exact arities. Do not initially export divide,
FP, memory access, stack-pointer operations, dynamic `EXECUTE`, source
evaluation, dictionary mutation, scheduler words, or arbitrary host callbacks.
This avoids an implicit guest fault callback or service side effect inside a
profile that has not admitted it.

Use a fresh canonical private `ExecutionContext` for each callback. The first
primitive gate has a statically bounded stack requirement of at most eight
argument/result cells. It needs no shared guest stack arena or copied caller
stack. A later closed colon profile must prove a finite stack-growth bound
from its admitted operations and finite semantic allowance before execution;
an unbounded host list is not itself a stack-capacity policy.

Semantic dispatch must still own primitive invocation and step charging. Add
a small engine-owned export/admission interface rather than calling private
primitive closures from `hybrid/`. It must retain the current meter's identity
and charge every admitted semantic operation exactly once. A native semantic
executor may decline a private context through its existing reference path;
the report must describe actual execution without claiming all callbacks ran
in C++.

The closed colon gate admits explicit literals, selected integer operations,
static calls and qualified control flow with finite work/stack bounds. Reject
unknown operations and arbitrary dynamic targets before entry. New source
definitions require a new admission record; a static callback closure never
silently expands because another same-named word appeared. Machine callees in
that closure require the nested-transition gate below.

## Budgets and accounting

Introduce one owner-bound interop dispatch ledger tied to the existing outer
semantic meter. It survives raw semantic entry, wrapper entry, source token
boundaries, nested dispatch, quanta, IDL, and wake. Suspension/frame objects
retain its identity; a host resume cannot replace it. Weak lookup may locate
the ledger, but must not be its only owner while work is parked.

Recommended initial limits, to be locked before the corresponding code slice:

| Resource | Hard bound or rule |
|---|---|
| Machine instructions | Existing v1 per-routine 1,000,000 and per-root 10,000,000 ceilings |
| Semantic exports | 64 per session; no unbounded callback registry |
| Callback sites | 16 per routine, independent of the existing 16 buffer-rule limit |
| Callback requests | 1,024 per outer dispatch, including every nested invocation |
| Callback semantic work | At most 4,096 steps per callback and 65,536 cumulatively per root; lower outer semantic budget still wins |
| Parked machine frames | 1 in 5A; at most 8 after the nested-transition gate |
| Semantic callback arity | 0..8 input and output cells |
| Private machine stacks | Existing sealed slices and total 4 MiB arena; no resizing or shared active slice |

All limits are exact positive integers where applicable. Configuration may
lower, never raise, a live ledger's allowance. Root machine work charges every
completed instruction once. A routine's local instruction allowance covers
its own segments across callbacks; nested routine instructions charge their
own local allowance and the shared root allowance. Callback-local semantic
limits include nested semantic callback work so nesting cannot evade them.
The root semantic meter still accounts for normal surrounding source work.

The current semantic API cannot simply receive a new `step_budget` for each
nested callback. Add composable local allowance checks beneath dispatch while
keeping one original meter/on-tick path. For 5A's fixed leaf primitives the
required semantic work is known before invocation; prove this path first.

Keep the domains separate: semantic steps advance hosted semantic diagnostics
and timer; real machine instructions produce machine cycles; neither changes
RTC policy. Host-monotonic RTC may progress while suspended, and manual RTC
still advances only through its existing authority. Crossing the bridge adds
no modeled time. Reports distinguish segment deltas, invocation totals, and
root totals to prevent nested double counting.

## Nested transitions

After 5A, support semantic caller -> machine A -> admitted semantic callback
-> machine B -> callback return -> machine A. This requires a bounded native
frame stack, not recursively calling the current `RoutineRunnerV1.run`.

Each parked machine frame retains its exact registration/control leases,
borrowed spans, register/selector/flag/prefix state, PC, return-stack pointer,
request token, local instruction count, and entry-failure provenance. Save
only this bounded execution state. Shared memory, the I-cache, services and
root accounting remain owned once. Child entry can initialize its own CPU
view only after saving the parent's complete relevant integer state; child
completion must restore that parent before returning callback outputs.

The first nested profile rejects reentry of any registration already on the
active machine-frame chain. Distinct registrations already have disjoint
private stack slices. This makes nested calls useful without pretending that
one registration's fixed private stack can hold simultaneous invocations.
Recursive same-registration calls require a later finite frame-stack allocator
and independent leases; they are not enabled by increasing the depth limit.

Borrow authority can only narrow at a callback boundary. A child machine
borrow must fit the parent invocation's already-qualified data grants, with
no permission upgrade. A callback/service buffer is qualified afresh from
its original argument tuple and must fit those grants too. The root task may
make unrelated machine calls after an invocation completes. All live semantic
stack allocations, dictionary headers, sealed executable spans and private
control storage remain protected throughout the active chain.

No callback may publish, remove, rewind or replace dictionary/code allocations
while machine frames are parked. Reject those operations before effects in
the admitted callback profile. Revalidate identities and seals on resume
anyway; preflight is not a substitute for exact lifetime checks.

## Errors and task exceptions are separate gates

In the isolated callback profile, callback failure terminates the pending
machine invocation. Discard unpublished callback outputs, revoke its pending
token, settle all completed work, and raise a typed `HybridExecutionError`
with the original exception as its cause and the machine/callback location.
Completed ordinary stores and admitted service effects remain visible. The
outer semantic input cells retain the existing v1 failure rule, and ordinary
semantic dispatch guards perform their existing return-stack cleanup.

Budget exhaustion, a stale export, invalid callback reply, rejected machine
access, and illegal instruction remain distinct outcomes. Do not translate
them all into a guest `THROW`, invoke `FAULT-XT!` for a machine profile error,
or use `ForthAbort` to clear an unrelated caller context. Catch `BaseException`
only to cancel/settle owned frames and then propagate it; host interruption is
not a successful callback result. Cleanup failure preserves the original
error and disables further interop entry until close.

General Forth callbacks require a separately locked **shared-task callback
profile**, with a new ABI version if observable stack/failure rules change.
The recommended semantics for that later profile are:

- Use the original semantic task context and its existing HANDLER identity;
  do not swap a global handler cell around an unrelated private stack.
- Specify machine argument consumption at entry and callback argument/result
  placement explicitly. Ordinary task-stack effects and a nonlocal `THROW`
  cannot also promise unconditional restoration of v1's untouched inputs.
  Do not silently change v1/v2 settlement to accommodate them.
- Represent return to a parked machine frame by an engine-owned typed foreign
  continuation with exact root/frame identity. If represented in the guest
  return stack, use the existing cookie-validation machinery and retain the
  corresponding metadata below the active pointer. Never encode a machine
  PC as an ordinary semantic XT or fabricate a root `Continuation`.
- A normal return resumes only the matching live frame. `RP!`/`THROW` that
  discards a foreign continuation must revoke and unwind its machine frames
  before the older semantic root resumes. It must not return later to a stale
  Python `finally` block and restart discarded machine work.
- Preserve the distinction between a callback-local catch, a throw to an
  outer caller catch, returning fault callbacks, and task-origin `ABORT`.
  Reuse `_GuestFaultRequest`, `_GuestControlTransfer`, and origin-context
  semantics through a reviewed runtime interface; do not catch these signals
  as generic host failures midway through an authorized guest unwind.

Implement the typed failure gate before this shared-task gate. Its design and
tests must cover real KDOS CATCH/THROW and retained `RP@`/`RP!` evidence before
the capability is advertised. A new exception/task ABI is a technical design
decision for this branch, not a reason to ask the user to choose internals.

## Suspension, wake, and lifecycle

Callbacks cannot suspend in 5A/5B. A synchronous bridge primitive remains one
bounded semantic operation even if it contains several machine segments;
current host quanta do not make that primitive preemptible. Report this
limitation and retain an outer watchdog in qualification.

For 5D, introduce a backend-neutral foreign-operation request into the
semantic dispatcher. The dispatcher, not an active Python callback stack,
must retain the suspended parent cursor and the machine/callback frame chain.
The native planner exits at that operation and returns to the same owner.
An engine-owned cursor variant may refer to an opaque composition token;
`simulator/` must not import the hybrid implementation.

One composite suspension then owns the original task root, semantic meter,
interop ledger, all parked machine state, callback context/return evidence,
and all memory/control/code leases. A callback `IDL` suspends that whole chain.
It must not create a second independent `_SuspendedExecution` by recursively
calling `run_until_blocked`. Native stack/register state remains native-owned;
there is no whole-memory checkpoint or serialization claim.

Keep the current distinction between `YieldedExecution` and
`BlockedExecution`. Runnable quantum continuations use `resume_yielded` and
need no wake. An IDL continuation resumes only after the owner admits one
existing `IdleWakeReceipt` for that exact composite suspension. Receipt
copies, wrong generations, stale frames, and repeated wake/resume attempts
must fail before any segment runs. Preserve finite allowances across every
yield and wake, including suspension before the first callback or machine
instruction.

The existing session backend remains the sole input, terminal, timer-event and
display owner. Events arrive at its existing admitted boundary, not from a
new machine callback thread. Service completion and display acknowledgement
retain their current meanings. A yielded native segment is host scheduling,
not a fabricated architectural IDL or interrupt.

Cancellation unwinds the composite frame chain once, invalidates all native
resume tokens, applies the semantic capture/return cleanup policy, and leaves
completed shared effects visible. Close first cancels any owned composite
suspension, then revokes exports/registrations, then releases native pins.
Foreign/raw resume after close must fail. A session cannot free a private
stack still referenced by a parked child or resurrect it through an old token.

## Selected services

Services enter through declared exports, not by exposing a machine MMIO bus.
There is still one semantic instance of each admitted service. Capability
records state the allowed operation, ownership/transaction effects, input
geometry, work limit, and whether suspension is possible.

Recommended order after the relevant error/scheduler gates:

1. Scalar FP value operations and explicit FPCSR access. Preserve the existing
   rounding mode, sticky flags, bit patterns, fault/pop order, and exact native
   versus Python oracle. Resolve invalid rounding mode and `FAULT-XT!` routing
   under the admitted exception profile before enabling this export; exporting
   a name is not authority for an arbitrary installed fault handler.
2. Bounded ordinary-memory transforms using callback-qualified subsets of
   the machine borrow. Preserve complete-span rejection and completed-prefix
   writes. No callback may use its unrestricted semantic memory object to
   escape the export's buffer/effect admission.
3. Explicit, bounded crypto or audio operations using their existing checked
   transactions. Preserve acquire/update/final/abort ownership, generation,
   buffer alias rules, publication and error ordering. Byte counts need their
   own cap: one semantic primitive can contain substantial host work.

Terminal input, storage mutation, task scheduling, and arbitrary MMIO are
later service contracts. Their current owner/session authority must be
carried explicitly, not inferred from a pointer, XT, or machine core ID.
Returning a service status is not proof that a display was physically shown
or that an asynchronous operation completed.

## Implementation slices and acceptance

Commit this plan before implementation. For each slice, lock the narrower
versioned API/contract, implement it, run the focused repository Make gates
sequentially, then commit code and the evidence actually obtained.

1. **5A native boundary.** Add v2 request/resume/cancel and exact sealed sites,
   retaining the v1 path. Test register/flags/private-stack preservation,
   real CALL/RET accounting, failed call pushes, non-call gate entry, stale or
   replayed tokens, cancellation, last-instruction limits and repeated warm
   I-cache calls. Compare the machine segments against the existing ordinary
   MP64 runner at the same PC/stack geometry, with independently supplied
   callback outputs between segments.
2. **5A semantic composition.** Bind the small canonical export table and one
   synchronous callback context. Compare each callback with direct semantic
   execution for both selected semantic backends. Prove original caller
   stack bytes/cookies/captures survive; callbacks charge the original meter;
   prefix stores persist on failure; shadowing/removal/XT reuse and failed
   publication fail coherently. Prove no native execution lock spans Python
   dispatch and no machine MMIO or fallback callback is reached.
3. **5B closed policies and nesting.** Add only admitted static colon closure
   and distinct-registration nesting. Gate finite work/stack/depth limits,
   parent register restoration, non-escalating borrows, shared-root budgets,
   forbidden recursion, rollback/redefinition, and failure unwinding through
   multiple parked frames.
4. **5C errors and shared-task contract.** First qualify typed isolated-context
   failures. Then lock the separate task ABI and extend the semantic/native
   continuation boundary. Use real KDOS exception fixtures for catch inside
   the callback, throw across one/two machine frames, `RP!` to an older root,
   fault callbacks that return or throw, `ABORT`, host errors, and exhausted
   budgets. Check active and retained stack bytes plus cookie/capture state.
5. **5D resumable dispatcher.** Gate IDL and quantum suspension at each side
   of every transition, including before any machine work; duplicate/foreign
   wake and resume; event admission; memory/lease revalidation; cancellation;
   close; and one session/server smoke journey with exact work counters.
6. **5E one service at a time.** Reuse that service's existing oracle and
   transaction tests. Add only its crossing/alias/ownership/error cases.
   Record a bounded representative workload before claiming useful speed or
   changing defaults. Crossing into Python may cost more than equivalent
   direct semantic work; functionality alone establishes no acceleration.

Each result records actual mode/profile/executors, semantic steps, completed
machine instructions/cycles, callback count, maximum parked depth, yield/wake
counts and exit reason. Unprofiled latency remains separate from attribution.
No live Desktop, physical-device, multicore, or external-project evidence is
implied by these bounded tests.

## Decisions the branch can make now

Proceed with explicit v2 opt-in, sealed local CALL/RET stubs, exact canonical
exports, isolated callback contexts, one parked frame, no suspension and no
new service access. Keep v1 behavior and capability reporting unchanged.
The names of internal request/ledger types and the initial finite limits are
implementation choices that the root review can settle without user input.

Before expanding beyond that slice, lock the stack/failure semantics of the
shared-task profile and the dispatcher-owned foreign continuation. That is an
actual ABI decision, not an invitation to quietly broaden a callback table.

Guest compiler/dictionary integration, CREATE/DOES> machine mappings, guest
JIT, arbitrary binaries or self-modification, native snapshots and multicore
hybrid execution remain separate designs and gates. This plan grants none of
those capabilities.
