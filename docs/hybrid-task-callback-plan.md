# Shared-task callbacks and composite suspension: Phase 5C/5D

Date: 2026-09-30

Status: locked implementation contract, committed before code. This document
enables no capability. It refines the shared-task and
suspension sections of [the interoperability plan](hybrid-interop-plan.md).
The existing private callback profiles, including the
[closed](hybrid-closed-callback-plan.md) and
[bounded nested](hybrid-nested-callback-plan.md) profiles, retain their current
stack, failure, cancellation, and no-suspension contracts.

5C and 5D share one dispatcher-owned foreign-operation foundation. Qualify it
with a scripted reference adapter first, then a bounded native adapter, then
the production session owner. Synchronous shared-task exceptions can be
enabled before suspension, but must not acquire a second control mechanism
which suspension later replaces.

## Scope and version boundary

Use the distinct ABI family `megapad.hybrid.task-routine`, metadata and manifest
version `1`. Private `megapad.hybrid.integer-routine` versions 1 through 4 do
not accept these descriptors. Task native transport also has a separate
versioned entry point and capability identity; a private runner cannot gain
task behavior from a boolean argument or callback effect string.

The first task profile admits only the runtime's canonical backed
`main_context`, core 0/task 0. It uses that context's existing data stack,
return stack, guest fault configuration, and KDOS HANDLER cell. It never swaps
HANDLER around a private callback stack. Arbitrary host scratch contexts,
coroutine switching, worker tasks, multicore execution, and task replacement
are outside this profile. The existing runtime reports `(0, 0)` for every
`guest_identity` and installs a zero-valued TASK-ID word; this is not a general
task-to-context mapping.

Task callbacks may use only exact captured KDOS/core dependencies and the
explicitly admitted operations below. No arbitrary source evaluation,
dictionary/code publication, host primitive, scheduler operation, machine
MMIO, or service import is introduced. A machine routine retains explicit
buffer declarations and the current integer instruction restrictions.

The private and task profiles may share implementation helpers, one CPU and
backing owner, and accounting infrastructure. They must not share entry
authority or silently nest into one another. The first task profile rejects
cross-profile child calls. Native task nesting remains at most eight active,
distinct registrations with disjoint control slices; same-registration
recursion remains unsupported.

## Source constraints

| Source | Consequence |
| --- | --- |
| [`tests/simulator/fixtures/kdos-exceptions-618-675.f`](../tests/simulator/fixtures/kdos-exceptions-618-675.f) | CATCH saves SP, previous HANDLER and RP in guest storage. Nonzero THROW performs RP!/SP! and then ordinary Return; it does not raise a Python exception. |
| [`simulator/runtime.py`](../simulator/runtime.py), `PrimitiveCallback`, `_call_from_colon`, `_execute_top` | A synchronous primitive currently returns None or Invoke. An unwound Python bridge frame cannot serve as a resumable machine continuation. |
| The same file, `_DispatchCursor`, `_SuspendedExecution`, `run_until_blocked` | There is one detachable root, one suspended context, one original meter and one wake authority. Nested resumable public dispatch is rejected. |
| `_execute_guarded`, `_resume_guarded`, `_GuestControlTransfer` | Guest return through an older dispatch root is different from a host failure. Generic exception cleanup must not undo authorized guest unwinding. |
| `_invoke_primitive`, `_begin_guest_fault`, `_abort_guest_fault` | Semantic InstructionFault enters FAULT-XT!, with a THROW-discardable fault-abort continuation. A returning or recursively faulting handler aborts. |
| [`simulator/stacks.py`](../simulator/stacks.py), `ReturnStack` | Active and inactive cells retain raw cookies and continuation metadata. Pointer restoration changes the frontier without erasing slots; metadata remains typed only while its expected cookie matches. |
| `ReturnStack.capture_pointer`, `restore`, `snapshot` | The capture generation counts RP@ observations; it does not record captured addresses. Failure restore reissues ordinary continuation cookies, while snapshot may remove stale metadata after a raw mismatch. Neither is foreign-frame authority. |
| [`simulator/errors.py`](../simulator/errors.py), `ForthAbort` | ABORT has an origin context. A foreign-context abort cannot clear an unrelated caller's data stack. |
| [`simulator/rich_terminal_host.py`](../simulator/rich_terminal_host.py) | The existing backend owns input admission, yielded/IDL resumption and cancellation before owner release. A foreign operation must fit this lifecycle. |

The existing C++ semantic executor returns to Python for root/fault returns
and unknown continuation types. That decline is useful, but is not sufficient
qualification for new foreign metadata or raw-cookie mutation detection.

## Backend-neutral operation and adapter boundary

Introduce an engine-owned `ForeignDefinition` which refers to an exact
host-issued operation registration. Do not extend ordinary PrimitiveCallback
with an unchecked duck-typed result. Existing private machine words remain
their current primitive definitions. The semantic compiler still emits a
normal Call to the task Word, and dynamic EXECUTE still resolves through the
semantic dictionary.

Put immutable protocol values and the adapter interface in a backend-neutral
module under `shared/`. Conceptual values are:

- `ForeignOperation`: exact registration handle and declared signature;
- `ForeignCallbackRequest`: issued frame/request identity, captured export,
  arguments, and native continuation evidence;
- `ForeignCompleted`: outputs and an authoritative accounting receipt;
- `ForeignRunnableYield`: retained operation token and receipt;
- `ForeignFailed`: typed machine/profile outcome and receipt;
- `ForeignContinuation`: semantic engine-owned task return identity.

These values contain no import of `hybrid`, no Python callback supplied by
guest data, and no executable host pointer. Native state stays behind opaque
adapter-issued tokens. The engine owns the registration-to-adapter binding;
constructing or copying a value does not create authority.

The adapter supplies begin/advance, callback reply, contiguous-suffix
cancellation, and terminal cancellation. Calls occur only at dispatcher-owned
transition boundaries under the existing execution owner. Each returns
bounded state and settled receipts; no CPU
execution lock spans semantic dispatch. `simulator/` imports only the neutral
contract. `hybrid/` supplies the implementation and retains the native owner.

The dispatcher retains a typed foreign frame with the original semantic
resume target, root identity, context, registration, task data frontier,
parent frame, pending request and strong ledger reference. The resume target
is either an original `(Word, IR index)` or completion of the exact public
root. Do not invent an XT or root Continuation to represent a machine PC.

Entering a semantic callback switches targets inside the existing dispatch
loop. Returning through ForeignContinuation switches back to the owned
operation. It never recursively calls public execute/evaluate/run_until_blocked.
No correctness depends on returning later to a Python bridge `finally` block.

## Argument consumption and visible prefixes

These rules are intentionally different from private v1–v4's untouched outer
inputs on failure. They are part of the new task ABI.

1. Charge the ordinary semantic Call/definition-entry ticks at their existing
   boundaries. Validate task identity, registration/code/control leases,
   argument depth, final output capacity, complete borrowed spans and available
   limits. Rejection runs no machine instruction and consumes no machine input
   cells; already-charged semantic ticks remain charged.
2. Allocate the required bounded host/frame records before mutation. After
   successful admission, consume the N inputs exactly once, preserving their
   inactive backing bytes. Record the resulting task data frontier as the
   machine invocation's baseline, then begin native execution.
3. A callback request follows a real completed machine CALL and its existing
   private-stack effects. Preflight space for the callback argument cells and
   one foreign return cell before publishing either bridge transfer. Push
   arguments in signature order, install the foreign continuation, and enter
   the exact captured semantic target on the original task context.
4. Normal callback return must consume that exact live foreign continuation
   and restore the pre-callback return frontier. Its data frontier must equal
   the recorded baseline plus exactly the declared output arity. Extract/pop
   those outputs, preserving inactive bytes, then reply to the exact machine
   request. Do not restore a saved copy of underlying task cells.
5. Normal machine root return pushes its declared outputs onto the recorded
   task baseline and resumes the original semantic target. A callback's normal
   result must already have restored that baseline by extraction. Capacity or
   shape corruption discovered after effects terminates the operation; it does
   not recreate the original input stack.

An arity/frontier failure leaves the callback's actual data effects visible.
The task guard applies its normal return cleanup on the escaping host failure.
A nonlocal guest THROW instead keeps the SP/RP/HANDLER result produced by the
guest source. Completed machine stores, semantic memory writes, popped cells,
and instruction/tick counters are never rolled back by bridge cleanup.

Do not require unchanged bytes beneath a callback's data frontier: that would
silently undo or prohibit admitted ordinary task effects. The permitted
operations/effect admission define which writes are legal; normal result
validation checks the frontier and output shape, not a transactional stack
snapshot.

## Foreign cookies, captures, and permanent revocation

Add a distinct typed foreign return entry to the engine's return-entry model.
It occupies one ordinary guest cell with a runtime-generated opaque cookie,
like existing semantic continuations. Bind its metadata to the exact runtime,
root, context, frame/request, stack object, slot address and expected raw
cookie. R>/R@/loop operations must treat a matching foreign entry as control
state rather than a user cell. Return handles semantic, root, fault-abort and
foreign entries explicitly.

Every pending foreign return has its own evidence record. RP@'s capture
counter is retained for host-failure policy, but cannot identify which foreign
slots are still active. For the downward-growing backed return stack, a
pending slot `s` remains active only while `RP <= s < empty_pointer`, with its
exact issued metadata and cookie intact.

Reconcile this evidence after every admitted semantic operation, including
the partial effects of an operation that raises, before the next operation,
foreign reply or host suspension is admitted. Check it again before each
native segment and resume. Initial reference execution must not skip these
checks through a colon accelerator or native semantic interval.

The reconciliation rules are:

- A completed RP! which moves the frontier past a pending foreign slot
  permanently abandons that frame and its descendants. Apply RP!'s existing
  pointer update and input-cell consumption before cancellation; cancellation
  failure must not make the completed semantic operation disappear.
- A callback-local CATCH normally restores a frontier which still contains
  its enclosing foreign return; that frame remains live. THROW to an outer
  CATCH excludes one or more foreign slots and cancels precisely that suffix.
- A raw store that leaves a pending slot's cookie changed at the operation
  boundary, or replacement/removal of its exact typed metadata, permanently
  revokes the corresponding authority. Cancel before allowing another target
  to execute, even if ordinary snapshot/decode has already removed the stale
  metadata. The independent evidence record must survive that removal.
- Invalid RP! with no pointer effect does not revoke a valid frame. Identical
  raw bytes with the same live issued metadata do not alone revoke it. The
  observation boundary is the admitted semantic operation, not an invented
  atomicity guarantee for every intermediate byte of a memory primitive.
  Partial-fault writes are inspected at their failure boundary.
- Once canceled or normally consumed, restoring the former pointer, raw
  bytes, copied snapshot or cookie cannot revive a foreign continuation.
  Restoring bytes after a mismatch in a later operation is already too late.
  A mismatching raw cell follows ordinary stale-type/user-cell decoding;
  a retained typed foreign tombstone is non-resumable. Either route must fail
  before a machine segment if later used as a return.

Record retirement in an engine-owned token state independent of mutable guest
memory. Tombstones retain only identity and retirement status, not native
register snapshots, code owners or stack leases. Retained return metadata may
refer to them after a root completes; those references cannot keep the machine
chain alive. Slot reuse replaces its metadata normally. Token issuance and
active frame counts remain bounded by the limits below. Retired tokens need
no global history once no retained return metadata refers to them.

Normal callback Return is a distinct transition: validate its live token and
pop it through the designated foreign-return path, then retire it exactly
once. The post-operation checker must not misclassify this authorized pop as
an RP! discard. If reply validation/marshalling fails after that pop, cancel
the still-owned invocation and retain the semantic pop/output effects.

Do not implement foreign suspension or cleanup by `ReturnStack.restore`.
That method reconstructs ordinary continuations and resets capture bookkeeping;
it must not mint a fresh foreign token. Root failure/cancel first retires all
owned foreign authority, then performs the existing root return-guard cleanup.
Blocked snapshots compare exact foreign identities without reconstructing them.
Malformed raw-cookie state never becomes a trusted new baseline on resume.

## Subtree cancellation and guest control transfer

Task transport requires suffix cancellation, independently of private 5B's
whole-chain failure policy. Consider:

`machine A -> task callback with CATCH -> machine B -> task callback THROW`.

If the throw reaches the catch in A's callback, B is abandoned while A remains
parked. Before the next guest operation, the adapter must:

1. Settle all completed child receipts exactly once and retire every discarded
   child request/invocation token, deepest first.
2. Revalidate the surviving parent's code/control leases, saved integer state
   and live private return evidence, then restore that parent execution view.
3. Keep the parent's original pending callback token, site, arguments and
   remaining allowances unchanged. It can be replied to only when its semantic
   callback later returns normally through its own still-live foreign cookie.

Retire the discarded callback-local guards too. Subsequent catch-body work
continues under the surviving parent's original guard and the same root meter;
it cannot spend the canceled child's allowance or renew the parent's allowance.

RP! can retire a child while the current THROW/HANDLER implementation still has
semantic operations to finish. Retain that implementation's captured semantic
code, effect evidence and original bounded task-state/data grants until control
actually reaches the surviving pre-unwind continuation or transfers out of the
root. An intermediate Return inside HANDLER does not end this tail. Its grants
are not promoted into the parent's grant table. The tail retains no discarded
native frame, adapter token or child-local fuel and cannot enter another foreign
operation or mint export/request authority. Charge only the original root and
any surviving ancestor callback guards. A second RP! or raw slot mismatch must
revalidate the continuation frontier, code, effects and grants before another
operation. Tail completion removes this temporary semantic authority.

Do not execute a child's RET, fabricate child outputs, reset the parent, or
roll back shared bytes, cycles, caches or counters. Clear the host fetch window
when restoring the parent view; do not flush architectural I-cache. A throw
beyond A cancels the whole remaining chain instead. Cancellation itself has no
invented instruction or semantic-tick charge.

A cancellation/restoration failure is a host failure: disable further foreign
entry, retain accounting/effect evidence, and let the original task guard fail
closed. Never continue a guest catch after native restoration failed. Preserve
the initial failure or guest-unwind provenance in diagnostics and attach cleanup
failures without replacing it with an apparent normal callback result.

The existing `_GuestControlTransfer` remains the signal when a semantic return
consumes an older public root. Retire foreign frames belonging to discarded
roots before propagating or honoring that exact transfer. Do not restore the
discarded bridge's return snapshot over the guest's surviving catch frame.

## Semantic target and effect admission

Capture exact live Words, implementations, IR and required CREATE/DOES> actions
from the real KDOS/core dependencies after source loading and before execution.
Shadowing does not retarget them. Removal, changed actions/IR, numerical XT
reuse or lost registration authority rejects future entry. Names and integer
XTs supplied by machine registers are not export authority.

The first dependency vocabulary contains the canonical stack/integer/control
operations needed by the checked-in CATCH/THROW/HANDLER fixture, bounded static
colon calls and loops, canonical EXECUTE, admitted existing deferred actions,
ABORT and the captured fault hook. Admit only the exact selected targets of
dynamic EXECUTE, DEFER or a fault request. A genuinely unknown target is
rejected before target execution. Preserve the preceding canonical effects:
EXECUTE already charges its primitive tick and pops the XT before resolution;
a new peek-based precheck would change the reference failure prefix.

Do not run an arbitrary replacement FAULT-XT!, primitive closure or dictionary
emitter because its name resembles a captured dependency. Runtime target
admission must cover every actual invocation, including branches through
created-word actions and dynamic dispatch. Validate unused static operations
when capturing the profile; keep dynamic checks for data-selected targets.

Memory operations necessary for the task fixture use explicit task-state
grants: the canonical task stack allocations, declared HANDLER/control cells,
and any separately declared ordinary data spans. Task-state grants are distinct
from machine buffer grants; they do not let machine instructions touch semantic
stacks. A task-state address declaration is not a BodyAllocationLease or a
license to execute its bytes. Qualify its exact nonwrapping ordinary geometry
and captured task binding at entry. Machine children still inherit only
non-escalating ordinary grants from their machine parent. No unrestricted
MMIO/service fallback is available to the semantic callback.

Keep dictionary mutation, executable-code and machine-private-control writes,
public runtime reentry, source parsing, arbitrary host hooks and task switching
outside admission. Admitted semantic return-stack stores remain subject to
the foreign-cookie retirement rules above.
After one canonical operation begins, preserve its ordinary stack/memory fault
order; a preflight or effect guard must not re-run it to repair a partial result.
Operations with unbounded host work need a separate bounded service contract.

## Fault, ABORT, and host-failure behavior

Semantic InstructionFault follows the existing `_GuestFaultRequest` route on
the original context. The captured authorized fault hook receives its normal
throw code. Its fault-abort continuation sits above any pending foreign return;
a source THROW may discard both. A returning hook, absent hook or recursive
fault follows the existing report-and-ABORT behavior. A changed/unknown hook
is an admission failure, not permission to execute it.

Machine illegal instructions, invalid callback replies, access rejection,
expired code, and interop limits remain typed machine/profile outcomes. They
are not converted to a guest THROW or FAULT-XT! invocation by accident.
Distinguish faults from an admitted semantic operation from raw host failures:
an InstructionFault raised by an accounting hook remains that exact host
exception and must not enter the guest fault handler. The private bridge's
qualified host-escape behavior remains unchanged; the task profile adds guest
fault routing only for its explicitly admitted semantic operations.

Host errors and interruptions propagate as their original exception objects
after owned receipts are settled and frames canceled. StepBudgetExceeded is
a host escape, not a guest catch result. ForthAbort retains its origin context;
normalization binds an untagged abort to the innermost originating task, and
an unrelated origin never clears the caller's data stack. ABORT remains
uncatchable as an ordinary KDOS THROW.

Preserve existing capture policy. A host escape or cancellation after this
dispatch observed RP@ marks the context non-reusable; clearing/reconstructing
the active return stack cannot erase that evidence. Ordinary successful guest
THROW does not mark the context failed. HANDLER is changed by the actual guest
exception source, not by bridge cleanup pretending to finish CATCH. No failed
context with a stale handler may be silently reused.

## One root ledger and one composite suspension

Every foreign frame and composite suspension strongly retains the original
interop ledger and `_StepMeter`. A weak-key lookup may find them but is not
their only owner while detached. Preserve outer semantic fuel, callback-local
inclusive fuel, root callback fuel, native invocation limits and root machine
limits across every callback, child, yield and wake. Nested work charges each
root counter once; parent-local callback guards may include descendant work
without charging it again globally.

The dispatch cursor retains a strong task-root record containing the original
meter, spent ledger, adapter chain and foreign issuer even after the last
foreign frame has returned or retired. Ordinary outer quanta cannot drop this
record and obtain fresh foreign allowances on a later entry. Release it only
when that exact outer dispatch finishes or is canceled.

Retain current ceilings: 64 machine registrations and 64 semantic exports,
16 callback sites per routine, eight active distinct machine frames,
1,000,000 own instructions per invocation, 10,000,000 machine instructions per
outer dispatch, 1,024 callback requests per dispatch, 4,096 inclusive semantic
steps per callback and 65,536 callback steps per dispatch. Inputs/outputs stay
within eight cells. The first task profile also caps accepted foreign machine
entries at 1,024 per outer dispatch and captured callback IR at 4,096 operations
total, at most 64 captured Words per callback closure, and at most 16 declared
task-state/data spans per export. Configuration can only lower limits.
Rejected/prepared descriptors
cannot grow unbounded registries or restore consumed allowance.

Extend the dispatch cursor with a typed foreign state, including a pending
begin before any machine instruction. One `_SuspendedExecution` retains that
cursor, original task/root guard, full active foreign chain, original meter,
ledger and all execution leases. The current stack snapshots and capture
evidence remain part of resume validation; also validate every foreign slot,
token, code seal, task binding and native control lease. Snapshot comparison
alone is not proof of foreign liveness.

Semantic IDL inside an admitted callback suspends the whole root. Yielded
execution remains runnable and uses `resume_yielded`; IDL requires the existing
runtime-issued, one-shot IdleWakeReceipt. Wrong, copied, stale or repeated
wake/resume values fail before another segment. Delivering a wake does not
renew any budget. Source evaluation and nested arbitrary host dispatch remain
unresumable; this slice does not serialize their parser/Python frames.

Native scheduling requires a new `ForeignRunnableYield` outcome. An
instruction-limit failure is terminal and must never be renamed as a yield.
A native quantum stops between completed instructions, keeps exact PC,
registers, private return state and allowances, and resumes without entry
initialization or prefix replay. Yield before the first instruction is valid.
Host scheduling adds no MP64 IDL instruction, wake, interrupt, cycle or semantic
tick. Semantic ticks still advance the hosted Timer; machine cycles and RTC
remain separate domains.

Cancel/close retires the composite chain once, settles receipts, invalidates
resume tokens, applies the existing semantic root/capture cleanup, and only
then releases registrations and native pins. The production backend must
cancel its one suspension before releasing semantic ownership. No separate
machine thread, callback scheduler, input owner or display lease is created.

## Implementation and qualification gates

### A. Scripted reference adapter and semantic dispatcher

Implement the neutral values, engine-owned foreign definition/cursor, typed
return entries, operation-boundary reconciliation and strong ledger lifecycle
without importing a native extension. A deterministic scripted adapter produces
callback/complete/yield/failure events and receipts, including injected failures
after effects and during marshalling/cancellation. It must validate identities
and one-shot transitions rather than accept arbitrary test replies.

Run this gate through the Python dispatcher. While a foreign chain is active,
explicitly bypass native semantic plans and colon accelerators; selected
`executor=native` may resume its usual path after the chain ends, but status
must report actual task callback execution as reference dispatch. Later native
semantic admission is a separate optimization gate.

Use unchanged MegaPad fixtures and existing regression anchors:

- `tests/simulator/test_kdos_exceptions.py`: normal CATCH, THROW 0, local catch,
  rethrow, outer catch through DOES>/DEFER/loops, ABORT, every budget prefix;
- `tests/simulator/test_kdos_dictionary_task_hooks.py`: returning/throwing fault
  hooks, recursive faults, origin-aware ABORT, older-root transfer evidence;
- `tests/simulator/test_idle.py` and `test_native_stack_pointers.py`: RP@
  capture, IDL/quantum/wake, cancellation and failed second suspension;
- `tests/simulator/test_native_suspension_state.py` and `test_stacks.py`:
  snapshot ordering, cookies, pointer validation and retained inactive slots.

Add exact raw-cell, pointer, cookie and capture comparisons. Test RP! crossing
zero/one/several foreign boundaries; callback-local catch retaining the parent;
child THROW caught in the parent's callback; raw overwrite and later repair;
identical-cookie writes; snapshot metadata removal; typed tombstone restore;
normal foreign pop; and all cancellation failure windows. No synthetic Python
THROW exception may replace the real source fixture.

### B. Bounded native task adapter

Require the separate bounded nested machine transport to qualify first. Reuse
its decoder, sealed CALL/RET sites, control ownership and receipt mechanisms,
then add the independently versioned task transport with suffix cancellation
and real resumable quantum segments. Keep private v1–v4 cancellation unchanged.

Gate one-frame entry first, then at most eight distinct frames. Differentially
compare machine segments with ordinary MP64 execution, supplying independently
known callback effects between segments. Verify surviving-parent restoration
and unchanged callback tokens after suffix cancellation; exact prefix stores,
cycles and receipts; no native lock over semantic execution; unchanged I-cache
publication behavior; revoked leases; and failures after receipt publication.
The native adapter must not trust a semantic tombstone as resume authority.

### C. Production session and eventual application exposure

Run a small MegaPad-owned compiled entry through HybridSession and the existing
production backend owner: task callback IDL, admitted input/wake, nested child,
outer catch, final output and clean close. Sweep quanta around each transition
and test timeout/cancel while a parent and child are parked. Record the actual
profile, callback executor, semantic work, machine segments/instructions/cycles,
maximum depth, yields, wakes, and cleanup result.

The first session gate may use a host-prepared runtime through the existing
preconstructed-runtime seam: load the exact KDOS/task callback definitions,
capture dependencies/register task words, compile the final entry, then arm
execution. Do not weaken private manifest dependency-first publication or
invent unresolved executable placeholders. A public task manifest/launcher
needs a separately validated source-loading/publication order before exposure;
the session gate alone does not qualify that loader.

Only completed gates may advertise `shared_task_exceptions` or
`composite_suspension`. Until then both remain false, even if the reference
adapter tests pass. This design does not qualify general tasks, arbitrary
callbacks, source evaluation during suspension, selected devices/services,
compiler/JIT integration, native snapshots, multicore execution, or a Desktop
journey. Those remain separate contracts and evidence.

## Neutral value API appendix

[`shared/foreign_abi.py`](../shared/foreign_abi.py) defines the first value and
typing-interface slice. It implements no dispatcher, registration, cookie,
adapter, or native task transport, and enables neither capability above.
Every value has task ABI identity/version 1, frozen slots, exact validated
integers and tuples, and finite bounds. Constructors revalidate nested values.
Opaque references are retained without inspection, coercion, hashing or
equality; containing values use identity equality and omit those references
from representations. These conventions prevent validation from invoking
opaque user behavior, but do not establish that any reference was issued.

| Value | Fields and meaning |
| --- | --- |
| `ForeignSignatureV1` | `input_cells`, `output_cells`, each 0..8 |
| `ForeignSpanV1` | Exact uint64 `base`, `size`, nonwrapping span and read/write/read_write `access`; an empty span grants nothing |
| `ForeignOperationV1` | Opaque `registration`, signature, at most 16 resolved `machine_grants`, own instruction ceiling and own callback ceiling |
| `ForeignExportV1` | Opaque `export`, signature, at most 16 distinct-role `task_grants`, callback semantic ceiling; no captured Word/IR implementation |
| `ForeignBudgetV1` | Separate invocation/root remaining instruction and callback counts, plus `quantum_instructions`; values cannot renew original allowances |
| `ForeignReceiptV1` | Root/invocation/parent IDs, depth, root segment sequence, invocation-start flag, accepted root-entry count, state, segment work and own-invocation/root totals |
| `ForeignCallbackRequestV1` | Operation/request tokens, receipt, per-invocation request sequence, callback site, export descriptor and argument tuple |
| `ForeignCompletedV1` | Operation token, returned receipt and output tuple; the issued operation owner still checks declared output arity |
| `ForeignRunnableYieldV1` | Operation token and yielded receipt; zero instructions/cycles, including accepted entry before its first instruction, are valid |
| `ForeignFailedV1` | Operation token, failed receipt, bounded machine/profile failure kind, detail and optional instruction PC; not a wrapper for host exceptions |
| `ForeignCancellationV1` | Up to eight deepest-first retired invocation IDs, optional unchanged surviving-parent ID/request token and latest retained receipt |

Receipt segment deltas count completed instructions/cycles and zero or one
completed callback CALL. Invocation totals exclude child work; root totals
include every invocation once. All three levels remain distinct. Each accepted
segment, including a zero-work yield or terminal failure, obtains a new root
sequence. `invocation_started` marks an accepted entry exactly once, rather
than inferring entry from a changed invocation ID when returning to a parent.
Callback/returned/yielded/failed are distinct receipt states; only returned
and failed are terminal. A returned receipt requires a completed RET, and a
callback receipt requires a completed CALL. Local consistency checks do not
prove cross-receipt continuity, active ancestry, or correspondence with code.

`ForeignAdapterV1` is a typing protocol with these boundaries:

```python
begin(operation, arguments, *, root_token, root_id, budget, parent=None)
advance(operation_token, *, budget)
reply(request_token, outputs, *, budget)
cancel_suffix(operation_token)
cancel_all()
last_receipt()
```

The first three return one of the four event values. A child `begin` supplies
the parent's pending callback request; the adapter must validate its exact
issued authority, static child edge, root, ancestry and grants. `advance`
resumes only an issued runnable token; `reply` consumes only its exact live
callback token. Structural protocol conformance, copied values, numerical IDs
and matching descriptors are never admission. The engine-owned registration,
foreign return entry and token state remain outside this shared module.

The first begin is an admission-only, zero-quantum transition. Validate and
reserve the invocation, then publish a zero-work receipt and issued runnable
token without initializing CPU registers, writing a return sentinel or executing
an instruction. Only after delivery of that accepted event may the semantic
dispatcher consume the declared data-stack inputs. The following advance
initializes the CPU once and starts execution. If accepted-event allocation or
delivery fails, settle the retained zero-work entry receipt and cancel the
admission while leaving semantic inputs untouched. Preflight rejection still
creates no receipt. This ordering applies to child entry as well as root entry.

Adapters enforce the lesser of their retained original allowances and supplied
remaining ceilings. A zero scheduling quantum may produce a zero-work yield;
exhausted instruction fuel remains a terminal instruction-limit outcome.
The semantic engine retains separate callback-local/root semantic fuel and
capture/return authority; receipt machine counters do not replace it.

The adapter must publish `last_receipt()` before event allocation and preserve
it across cancellation and marshalling failure. The dispatcher settles each
issued sequence once, including on a raw host exception. Rejected begin
preflight creates no receipt. Cancellation creates no segment or invented
work: it returns the latest already-published receipt, which may belong to a
discarded child. Empty inactive cancellation still exposes that receipt.
Retired IDs alone cannot prove a contiguous suffix or parent restoration;
those checks and original parent-token preservation belong to the adapter.

## Native task transport contract lock — 2026-09-30

Implement the production adapter behind a distinct `TaskRoutineRunnerV1`
facade on the existing native `RoutineOwner`. Reuse its bounded integer
instruction loop and backing ownership; do not introduce a second CPU or
interpreter. A standalone constructor may create the owner. The cached
`RoutineRunnerV3.task_v1()` facade shares the existing owner. Facade garbage
collection only releases its reference. This contract enables no capability
until the native, dispatcher and application gates pass.

Task transport has distinct sealed `TaskRoutineSpecV1`, `TaskBudgetV1`, root,
operation, request and child-edge tokens, publication authority and receipts.
It does not inherit private V1/V2/V3 token authority. The spec uses the V3
integer parser and bounded callback fields. Exact immutable budget fields
mirror `ForeignBudgetV1`: `invocation_instructions_remaining`,
`root_instructions_remaining`, `invocation_callbacks_remaining`,
`root_callbacks_remaining`, and `quantum_instructions`.

### Root ownership and boundaries

`bind_root(root_id, instruction_limit, callback_limit, entry_limit=1024)`
issues a native `TaskRootTokenV1`. Keep one bounded retained root ledger,
separate from live frames and the CPU reservation. A new root may replace it
only with no active frame and a greater root ID. The semantic adapter maps
the exact engine root token and original outer dispatch ID to this token;
same-meter nested host frames reuse the mapping. One exact adapter owner is
admitted per semantic root. Different owners cannot rebase receipt counters.

Root token generations prevent old authority from reviving when a new root's
receipt sequence starts at zero. Accepted begin issues sequence one; every
accepted segment advances it. Invocation IDs increase within the task owner
and are independent of private V2/V3 IDs. Cancellation, empty-chain reentry
and ordinary semantic scheduling quanta retain original ceilings, spent work,
entry counts and the latest receipt. Never retain or destroy Python root
objects while holding native CPU admission.

```python
begin(spec, arguments, spans, *, root_token, budget,
      parent_token=None, child_edge=None, protected_spans=())
advance(operation_token, *, budget)
reply(request_token, outputs, *, budget)
cancel_suffix(operation_token)
cancel_all()
last_receipt()
```

Begin requires zero execution quantum and positive retained own/root
instruction fuel. Validate, reserve, stage arguments and publish an accepted
zero-work yielded event without changing guest CPU/control bytes or writing
the sentinel. First positive-quantum advance initializes exactly once, after
the semantic dispatcher has consumed inputs following accepted delivery.
Preflight rejection leaves both machines and all ledgers unchanged.

Every accepted event rotates operation authority; previous advance/cancel
tokens become stale. A parent operation/request token stays unchanged while
a child executes. Advance admits only the top runnable token. Reply admits
only the top exact pending request, validates and stages outputs, and consumes
that request once. At zero scheduling quantum it issues a runnable event,
deferring register publication and the real sealed stub RET until advance.

Retained absolute ceilings are lowered by spent work plus the supplied
remainders, never renewed by a later budget. Scheduling quantum is independent
of terminal fuel: available fuel with zero quantum may yield; exhausted fuel
fails even at zero quantum and does not publish pending outputs. A completed
CALL or RET on the final quantum instruction produces its actual callback or
returned event. Never invent IDL, instructions or cycles to represent yields.

Publish native POD receipts before Python allocation. The result carries
issued operation/request tokens, site, request sequence, export identity,
arguments, outputs or bounded failure metadata. The adapter caches the exact
converted neutral receipt by root generation and sequence. Native parent ID
zero maps to neutral `None`; callback/returned/yielded/failed map directly to
the four neutral states and their original accounting rules.

### Task publication and child authority

Task captures may contain EXECUTE, DEFER, loops and cyclic potential target
graphs. Do not impose the private V4 publication DAG on them. Only active
recursion is forbidden. Use two-stage publication:

* `prepare_code(spec)` registers an immutable code/control candidate, returns
  `None`, and grants no execution authority. The exact spec is the registry
  key and remains usable for query/rollback after result delivery failure.
  Count prepared entries immediately against the shared 64-publication,
  16 MiB owner limits. Preparation does not invalidate instruction caches.
* `seal_publications(batch)` takes an exact tuple of at most 64 pairs of
  prepared parent spec and child rows. Each parent has at most 1024 distinct
  `(site_index, child_spec)` rows (16 sites by 64 registrations). Children must
  already be sealed or prepared in this same atomic batch. Allocate and
  validate the entire batch and opaque `TaskChildEdgeV1` handles before
  publication/cache invalidation. Failed preflight leaves prepared state
  unchanged. Return per-parent handles in declared row order.
* `is_code_registered(spec)` observes prepared or sealed exact identity;
  `is_code_published(spec)` observes sealed executable identity. Both permit
  owned parked task read-only access, but reject active execution/delivery.
  `revoke_code(spec)` is idle-only, removes either state, and reclaims shared
  count/bytes/edge capacity. Incoming edges remain permanently stale.

Self edges and potential A-to-B-to-A graphs are legal at publication. Bind
each edge to exact owner, parent generation, callback site/export metadata
and child generation. Entry still enforces at most eight distinct active
frames, disjoint control storage, and child grants contained within one
immediate parent grant with equal or narrower permissions. Every control
arena remains excluded from grants. No path expansion is needed; any closure
walk has at most 64 visited nodes and 65536 edges. The common owner shares
aggregate publication/edge capacity with private transport.

Idempotent resealing requires identical child generations and order, with no
in-place rebinding. A failed seal result conversion leaves exact spec query
and revoke authority for rollback; the high-level registration transaction
stays undispatchable until every seal and semantic export capture succeeds.
Prepared entries cannot begin or satisfy executable-capability checks. A
stale edge never becomes valid after revoke/reprepare.

### Failure, cancellation and restoration

Task machine failure retains its failed frame until explicit suffix/all
cancellation. `cancel_suffix` requires the latest exact operation token for
any live frame and retires that frame and descendants deepest first. Validate
surviving ancestor code/control and allocate diagnostics before mutation.
Restore the parent's saved integer execution state, including initialization-
owned control fields that a partially failed child may have changed. Do not
execute RET, publish outputs, or rewind stores, cache state or cycle counters.
The surviving parent's pending request, site, arguments and allowances remain
unchanged. Cancellation creates no receipt and retains the last issued one.

Before issuing a returned child receipt or popping its frame, validate the
parent restoration. A completed RET followed by restoration failure issues
one failed receipt containing the actual work and retains a cancellable failed
child. Successful restoration/pop followed by result delivery failure retains
the returned receipt and only the surviving ancestors: never resurrect a
retired child or disagree with the neutral ledger's completed return.

Raw host/allocation errors propagate unchanged, with actual completed prefix
settled from the retained receipt. Result delivery failure blocks further
execution until `cancel_all` or close. Cancellation/restoration failure marks
task admission unusable while retaining safe all-cancel/close paths. Hold the
reservation through event delivery, including terminal empty-chain return and
cancellation. No Python callback or destructor runs under CPU admission;
tokens retain weak common-owner identities.

Task close cancels its own frames and closes the common owner, but rejects
active private V2/V3 transport. Private close/cancel rejects active task
frames and preserves existing private behavior. `last_receipt` remains
available after cancellation and close.

### Ordered implementation and qualification

1. Add the distinct facade, spec, budgets and tokens; empty-child atomic
   publication; retained root ledger; one-frame admission-only begin,
   advance/reply with real instruction quanta; all-cancel and receipts.
   Keep capability absent. Compare ordinary architectural execution at every
   instruction/CALL/RET boundary, untouched zero-admission CPU/sentinel state,
   zero/positive quantum, lowered fuel, replay/owner/stale seals, empty-frame
   ledger reuse, actual delivery failures, and all private native regressions.
2. Add child batches with potential cycles, the bounded frame chain, narrowed
   grants, validated real-return parent restoration, exact suffix cancellation
   and lifecycle exclusion. Cover quanta at every transition, unchanged parent
   requests after THROW-style suffix discard, cancellation before initialization,
   partial failure effects, ancestor corruption and delivery failure after pop.
3. Map native events to canonical neutral values with exact receipt identity
   caching. Run the reference dispatcher scenarios against that adapter, then
   composite suspension and application/session admission. A prepared host
   session seam may precede generic manifests; partial foundations never
   advertise the completed task or composite-suspension capability.

Use a new `cpu/mp64/routine_tasks.h` with narrow setup dependency changes and
the existing `mp64_accel.cpp` common-owner helpers. Build and test serially
through the repository Make gates, preserving private transport regressions.

### Nested native task refinement — 2026-09-30

The first one-frame implementation passed 541 native/private-composition and
application checks after an isolated GCC build. The next slice keeps the
locked public signatures and replaces the single frame with eight bounded
slots under the same reservation and retained root ledger.

At root and child admission, validate reachable publication generations with
one bounded walk: at most 64 visited publications and 65536 rows. Accept cycles
without expanding paths. Separately require the exact used edge, active parent
request and root token. Publication still does not impose active-frame overlap
or recursion rules on the potential target graph.

An admission-only child has not overwritten the parent's CPU state. Separate
saved ancestor code/token/control checks from validation of the currently live
CPU view. On successful child return, validate all survivors before restoration,
restore the initialization-owned integer controls and saved parent registers
and flags, advance only the parent's cycle frontier, then issue the child's
returned receipt and pop. A restoration failure records the completed child
prefix as failed, retains the child and preserves the original host exception.

Suffix cancellation preallocates diagnostics, validates survivors and restores
the parent before retiring frames. A cancellation-result delivery failure
retains the actual retired suffix and restored parent request under the owner
reservation, blocks execution, and allows only all-cancel or close. It never
resurrects frames or invents a receipt. An idle-only one-shot cancellation
delivery failpoint may qualify this exact window independently of the existing
accepted-segment failpoint.

### First synchronous semantic dispatcher gate

The reference dispatcher is a separate qualification slice from native task
transport and production session exposure. It uses the unchanged KDOS exception
fixture on the original main-context stacks. No task or composite-suspension
capability is advertised by this source foundation.

One semantic root admits exactly one adapter owner. Several registrations on
that owner share its receipt sequence and root totals, including later entries
after the active machine chain becomes empty. Entry through a different owner
fails before adapter admission or input consumption; counters are never rebased.

The initial retained-tail implementation pins the nearest exact surviving
pre-unwind continuation above the discarded foreign suffix, including a
surviving parent foreign continuation. The search examines active stack slots,
bounded by the original remaining semantic allowance. Intermediate helper
returns cannot assume this role. The tail keeps only its original captured
semantic grants and dependencies. A second pointer or cookie change which
destroys that boundary first retires the affected native suffix, then fails
closed. This slice does not claim general multi-frontier rethrow support;
expanding that support requires independently captured preexisting boundary
evidence, not promotion of newly created helper continuations.

Ordinary outer execution quanta retain the same strong task root and spent
ledger. Callback IDL and detachable machine execution remain a later gate.
The focused reference selectors are `tests/simulator/test_foreign_dispatch.py`
and `tests/simulator/test_foreign_dispatch_guards.py`, alongside the existing
foreign protocol, stack, registration, receipt and scripted-adapter gates.

### Production native adapter refinement — 2026-09-30

The native child transport passed 567 checks after an isolated GCC build. The
next slice connects that transport to the existing neutral task dispatcher;
it does not enable callback IDL, composite suspension or a generic task
manifest. Keep these changes separate from the synchronous reference engine
foundation so its existing behavior remains independently reviewable.

#### Adapter ownership and admission

Add an ordinary Python `NativeTaskAdapter` in `hybrid/task_adapter.py`. It
defines all six `ForeignAdapterV1` transition methods directly on its class:
`begin`, `advance`, `reply`, `cancel_suffix`, `cancel_all`, and `last_receipt`.
The engine's canonical adapter seal admits those exact Python functions; a
pybind facade or inherited transition methods do not satisfy that contract.
The adapter converts protocol values and validates issued authority. It never
executes semantic callback closures itself.

Use the existing common owner's cached `RoutineRunnerV3.task_v1()` facade.
There is one architectural CPU, one ordinary-memory/control pin owner and the
existing integer interpreter. No second native runner construction or memory
copy is permitted. Keep exact registered operation, export, code/body lease,
native spec and child-edge identities in bounded owner tables. Numeric export
IDs, equal descriptor copies and caller-supplied native specs grant no entry.

Expose a private exact-integer `_TASK_ROUTINE_TRANSPORT_REVISION = 2` only for
the qualified native child transport. The adapter factory checks that value
and the complete task spec/budget/token/publication/transition/receipt surface
before publication. This distinguishes the earlier one-frame extension,
which exposes many of the same method names. A stale extension fails with a
clear rebuild error; do not probe behavior or silently choose another adapter.
This private revision is not the public task capability marker.

#### Atomic semantic and native publication

Add a bounded engine-owned batch registration transaction under the original
session owner lock. Its boundary includes dictionary headers and bodies,
ForeignDefinition bindings, callback captures, native prepared/sealed
publications, and composition code/control allocation. Reject dispatch,
reentry and nested publication until the entire batch commits. Preserve the
existing single-operation registration API and its behavior.

Prevalidate names, signatures, grants and capacities. Checkpoint dictionary
and side-index state, exact binding/capture tables and capture IR allowance,
and composition allocation counters. Publish the batch's actual Foreign
Words using a narrow engine-owned initial-body/code-lease seam. Their aligned
machine images occupy those leased bodies, with exact sealed bytes and live
dictionary ownership checked on every admission. Include body bytes and
alignment in growth admission and rollback; do not allocate untracked code
beside a marker-only Word.

The transaction may define the required exact host-selected semantic IR and
Foreign Words before capturing callback dependencies. It does not evaluate
arbitrary source or execute guest/service side effects inside the rollback
boundary. After all candidate Words exist, prepare native specs, capture
callbacks, derive child rows, then atomically seal the complete batch. Only
after every step succeeds may the adapter expose executable registrations.
Prepared native specs and provisional semantic Words are undispatchable.

Rollback revokes only newly issued exact authority and restores the original
dictionary/index, binding/capture tables and allocation counters. Query native
`is_code_registered`/`is_code_published` before `revoke_code`, including when a
forwarding publication wrapper succeeds natively and then raises. Reclaim
prepared as well as sealed count, byte and edge capacity. Preserve preexisting
registrations and the original exception. If any cleanup cannot be proved,
disable further interop, retain safe close, and add diagnostics without
replacing the original error.

#### Captured child dependencies

Add an engine query `task_export_dependencies(export)` that verifies the exact
issued capture and returns its exact issued `ForeignOperationV1` dependencies.
It exposes no Word, callable or mutable capture table. Include transitive
ForeignDefinition targets reached through the captured static IR and explicitly
admitted dynamic EXECUTE/DEFER targets. A numeric registration ID or an
uncaptured current dictionary lookup cannot add a child.

For each native callback site, bind the exact engine export and derive
`(site_index, child_spec)` rows from those dependencies and the adapter's exact
operation registrations. Potential cycles remain legal. Do not reuse private
V4 static Call-edge IDs or impose its combined DAG proof. The native owner
independently enforces current generations, the exact used parent/site/child
edge, active-frame distinctness, depth and narrowed grants.

On delivery, check native site, export ID, signature and arguments against
the sealed site and captured export before issuing a neutral callback request.
Retain the exact parent request identity through child return and suffix
cancellation. A successful lookup by numeric site or export alone is not
callback authority.

#### Original root policy and profile exclusion

Add the engine-owned query
`adapter_root_policy(adapter, root_token, root_id)`. It proves the live original
task root, exact admitted adapter, engine ownership and exact semantic root
token/ID, then returns immutable original instruction, callback and entry
ceilings. Do not return the mutable ledger or infer its original entry ceiling
from `ForeignBudgetV1`, which contains only instruction/callback remainders
and the scheduling quantum.

Bind one native root token from that policy and retain the exact semantic root
token strongly in the adapter's bounded root slot. Frame-empty reentry,
ordinary outer quanta and same-meter nested host frames reuse that token,
receipt sequence and spent ledger. They cannot rebind or replenish ceilings.
Replace it only when the old chain is empty and the engine proves a later
original outer dispatch. Zero entry/instruction allowance rejects before
native binding or argument consumption.

For this first production profile, one original semantic root may enter
either private machine routines or task machine routines, never both. Pin
that choice through frame-empty intervals and scheduling quanta; reject the
opposite kind before native admission or input consumption. Separate original
roots may use either profile on the same owner. This explicit exclusion
prevents the existing private and task ledgers from each spending a fresh copy
of the same configured machine allowance. Shared mixed-profile accounting is
a separate future change, not an implicit exception to the root policy.

#### Exact receipts, delivery failure and accounting

Cache one exact neutral `ForeignReceiptV1` by native root generation and
per-root sequence. Repeated native receipt queries may return fresh native
value objects; they must yield the same cached neutral object and unchanged
scalar evidence. Every delivered event and cancellation refers to that exact
cached receipt. Keep this sequence and accounting separate from private
V2/V3 segment sequences. Map native parent ID zero to neutral `None` and use
the four existing callback/returned/yielded/failed states.

Keep at most eight adapter frame records and one pending transition record.
Reconcile an accepted native receipt, including entry or completed return,
before allocating the neutral event. Admission delivery failure may create a
zero-work frame without delivering its token; retain that fact for all-cancel
instead of retrying begin. A returned child is already retired when its event
is converted; retain the surviving parent and never resurrect the child.
Adapter-side conversion failure likewise blocks further execution until safe
cleanup and preserves the original exception.

Settle each native receipt once in composition totals: instruction/cycle
deltas, one segment per accepted receipt including zero-work admission,
machine transitions from `invocation_started`, and callback-request deltas
from the receipt. The semantic engine alone charges callback semantic work
through its original meter and root ledger. Do not subtract mutable meter
fields or charge semantic work again in the adapter.

Cancellation creates no work receipt. A cancellation delivery error may have
already retired a suffix and restored its surviving parent; preserve that
actual state, the latest settled receipt and the owner reservation, then allow
only all-cancel/close. Do not fabricate a successful neutral cancellation or
restore discarded authority to make the ledgers appear consistent. Failure
to recover exact retirement/accounting proof fails closed while preserving
the original host exception. Bounded native machine failures map to the
existing neutral failure kinds; host exceptions keep their exact identity
and never become guest THROW or FAULT events.

#### Prepared session and qualification boundary

Start with an explicit host-prepared `HybridSession` using the preconstructed
runtime seam. Install the unchanged KDOS exception definitions, exact callback
IR, code/Foreign Words and captures, then compile the final entry before
arming the session. Generic task manifest/startup support remains deferred.

The first session gate is synchronous. The dispatcher may consume successive
native runnable quanta internally, but that does not constitute a detachable
host suspension. Callback IDL/IdleUntil and composite cursor, wake and cancel
ownership require a later qualification slice. Report task callback execution
as the Python reference dispatcher even when the general semantic backend is
native; task execution still bypasses native semantic planning/accelerators.
Report task registration/profile and counters separately from private ABI
profiles. Do not advertise `shared_task_exceptions`, callback suspension or
`composite_suspension` solely because the private native revision is present.

Close must retire the semantic task root and native task frames before closing
the legacy facade/shared owner. A semantic cleanup error must still reach
safe native all-cancel/close so no task reservation or memory pin is stranded;
preserve the first exception and retain honest fail-closed state. Idle close
remains idempotent, and active host-dispatch exclusion remains in force.

Qualify the adapter first against the existing neutral/reference scenarios,
then the host-prepared session. Include atomic late-failure rollback and retry,
body-lease revocation, captured dynamic child dependencies, cyclic batches,
exact callback/receipt/token identity, zero-work admission before stack pops,
entry and instruction ceilings across empty-chain reentry, mixed-profile
rejection in both orders, suffix THROW-style cancellation, native and Python
delivery failures, exact host-error propagation, and close after failed
cleanup. Preserve all private transport/application gates. This first adapter
and synchronous session slice leaves public task and composite-suspension
capabilities disabled. Activation requires a separately reviewed application
capability gate; callback suspension remains separately deferred.

#### Lost suffix-cancellation delivery recovery

Before native suffix cancellation, retain one exact checkpoint of the owned
frame chain and the requested suffix boundary. If delivery raises, preserve
that exception and disable further task execution. A later successful native
all-cancel may return either the entire original chain (the suffix call failed
before retirement) or the exact surviving ancestor prefix (the suffix was
already retired). Validate those identities and the empty native survivor
before reconciling host control.

That all-cancel proves every original owned frame is now retired. Its neutral
result may therefore report their combined deepest-first IDs, including the
suffix already retired by the failed delivery. It does not claim the earlier
suffix call succeeded, create a work receipt, or permit guest reuse. Any other
retirement list fails closed. Qualification covers both native outcomes,
malformed IDs, exact original exception propagation and released ownership.

### Task completion refinement — 2026-09-30

The following gates complete retained-tail control and composite suspension.
The single-frontier and synchronous-session limitations above remain the
qualification boundaries of those earlier checkpoints. This refinement does
not activate a public capability or change the private callback profiles.

#### Original retained-tail frontiers

Before guest callback effects, retain a bounded immutable vector of original
active continuation evidence: exact entry identity, slot address and raw
cookie. Bound both scanning and retained records by the canonical return-stack
allocation and the original finite root ceiling. Inactive retained history
cannot add authority. On a foreign discard, select the relevant surviving
suffix of this vector and retain a monotonic frontier index with the original
semantic capture and effect scope.

A subsequent RP!, raw mismatch or metadata removal must first retire the
affected machine suffix, then revalidate the original vector, code and grants.
It may advance to a still-valid original frontier; a newly pushed helper, a
copied entry or a repaired tombstone can never become that frontier. Retain no
discarded machine frame, token, child-local fuel or machine-entry authority.
Charge the original root and surviving ancestor callback guards only. Release
the tail at the selected exact continuation or when the original root leaves;
fail closed if no trustworthy continuation remains. Preserve the existing
older-root `_GuestControlTransfer` behavior and guest SP/RP/HANDLER results.

#### Callback IDL under the existing suspension owner

Admit only the exact `Idle` and `IdleUntil` IR forms in this gate, including
their metadata and dispatcher route seals. Preserve canonical ordering: charge
the semantic tick, perform the operation's operand/deadline effects, then retain
the next semantic IP. This does not admit `UartReadAttempt`, `KEY`, `MS@` or
`IDLE-MS` as new callback effects.

For `IdleUntil`, capture the original canonical RTC owner and implementation
routes plus the selected monotonic-clock identity. Keep the deadline pop
before the uptime read and any clock failure. Admit only this deadline
observation, including the configured realtime policy; it grants no general
`MS@` or RTC callback authority. A narrowly installed deadline-read issuer
preserves exact host exceptions from the clock, and owner/code/grant evidence
is checked again after that host call before suspension publication.

Use the existing `_SuspendedExecution`, context lease and one-shot
`IdleWakeReceipt`. Strongly retain the original task root, meter, ledger,
foreign chain and retained tail through every block and resume, including an
empty machine chain. Add engine-issued suspension evidence for exact cursor,
frame/request identities, foreign slots, code/grants and owner routes; stack
snapshot equality alone is insufficient. Revalidate before another guest
effect. Copied, stale or repeated suspension/wake values confer no authority,
and waking renews no execution allowance. Source evaluation and nested public
host dispatch remain unresumable.

Cancellation settles and retires native/foreign authority before the existing
semantic root/capture cleanup and lease release. Never restore a return-stack
snapshot to suspend or recreate foreign authority. Preserve the first error
if cancellation or publication of a later suspension also fails.

#### Typed machine cursor and scheduling quantum

Extend the single dispatch cursor with engine-issued states for pending or
accepted zero-work entry and a runnable machine invocation. Retain the exact
binding, semantic resume target, operation token, root and completed entry
effects. Resume must not replay Call ticks, admission, CPU initialization,
input pops or callback reply. Preserve synchronous behavior when detachable
scheduling has not been selected.

Use a separate native instruction quantum per host turn. Spend it across all
machine segments and entries in that turn; do not reset it at each adapter
transition or confuse it with semantic steps. A later host turn receives a
fresh scheduling quantum while retaining every spent invocation/root limit.
Instruction-fuel exhaustion remains terminal. A host yield adds no semantic
tick, machine instruction/cycle, IDL or wake. Return runnable cursors through
the existing `YieldedExecution`/`resume_yielded` path, without a second scheduler,
input owner or suspension handle.

Before detaching and before resuming an active parked chain, use a no-work
native `validate_parked(root_token, operation_token, request_token=None)` query.
Validate the exact root and leaf operation, the exact pending request when in
callback state, every ancestor, publication/code seal, private control bytes
and live CPU evidence. Reuse the common owner's existing validators and
exclusion guard. The query must not initialize the CPU, execute, rotate tokens,
lower budgets, issue receipts or change cycles. Cached `last_receipt()` is not
this proof; validation at eventual reply is too late for resumed callback
effects. Composite admission requires this complete native surface, while the
earlier synchronous capability remains separately qualified.

#### Optional adapter validation and semantic settlement

The six mandatory neutral adapter methods remain the synchronous contract.
Two optional, directly class-defined Python methods are captured with their
original function, code and closure evidence. `validate_parked(root_token,
operation_token, request_token=None)` must return exact `True` without changing
issued frame/token/receipt evidence or doing work. Missing validation support
rejects a live callback detach after the canonical IDL tick and operand
effects, before suspension publication. It does not remove synchronous use.

`settle_semantic_receipt(receipt)` receives an engine-issued immutable
`TaskSemanticReceiptV1(root_token, root_id, sequence, semantic_steps)`. The count
is cumulative task callback work from the original ledger, independently of
native receipts and public meter projections. The engine retains one bounded
latest record and independent scalar evidence. Its
`task_semantic_receipt(adapter, root_token, root_id)` query only returns that
existing exact receipt; numeric equality or a copied value cannot issue it.
The first receipt has sequence one. More callback work advances the sequence;
retrying an unchanged total returns the same object and sequence. The initial
synchronous gate settles before root teardown; composite detach later uses
the same mechanism before publishing status. A final receipt already settled
at detach contributes zero additional work. Accounting values infer no
success or cancellation outcome. Settlement failure preserves its receipt
and first error while native/control cleanup is still attempted. Optional
method changes cannot replace or prevent use of otherwise intact mandatory
cancellation routes.

#### Completion qualification

Use the unchanged KDOS exception and IDLE fixtures with these focused journeys:

- Normal/local CATCH, THROW 0, nested rethrow and DEFER/DOES>/loop unwinds;
  child THROW caught by the parent and rethrow to the outer guest CATCH.
- A wrapper using `HANDLER @ RP! R> HANDLER ! -42 THROW` across two preexisting
  catch frontiers, while keeping the fixture's THROW/HANDLER unchanged. Check
  exact suffix retirement, intermediate helper returns, no discarded reply or
  RET, exact final code/HANDLER state and preserved completed stores/cycles.
- Raw overwrite, identical-cookie writes, metadata removal and later repair;
  budget and cancellation failures at both frontiers without restored authority.
- Callback and retained-tail IDL, repeated IDL, deadline/input wake, wrong or
  stale wake values, RP@ capture, failed second suspension publication and
  cancellation while parent and child are parked.
- Quantum boundaries around zero-work admission, CALL, reply, child return,
  RET and empty-chain reentry. Compare exact cumulative work, cookies, pointers
  and memory with uninterrupted execution, including terminal fuel exhaustion.

Then use one host-prepared `HybridSession`: load the fixtures and compile all
entries before arming; machine A's callback catches child B, whose callback
executes IDLE then `-17 THROW`. Existing backend input admission wakes the root,
B is canceled, and A receives/replies with the code. Ordinary outer `KEY EMIT`
consumes and emits the queued byte after the callback finishes. Repeat with a
parent rethrow to outer CATCH and with timeout/cancel at each parked boundary.

Record the actual Python-reference task callback dispatcher, semantic ticks,
machine segments/instructions/cycles, observed depth, yields, wakes and cleanup.
Preserve private-profile and ordinary suspension gates. Advertise task and
composite capabilities only after their reviewed application gates pass;
generic task manifests/startup remain a separate qualification.

#### Machine scheduling API and cursor lock — 2026-09-30

The opt-in host parameter is `machine_quantum_instructions: int | None = None`.
Accept only an exact integer in `1..10_000_000`, excluding booleans, or `None`.
`None` retains synchronous machine driving. Keep this parameter separate from
`quantum_steps`, instruction fuel, callback limits and machine-entry limits.
Session/backend configuration forwards the selected value only to the fresh
`run_until_blocked` call. Resume and wake retain that original selection and
offer no replacement limit. This adds no CLI/default/capability activation.

For each detachable outer host dispatch, create one exact immutable
`MachineTurn(limit)` and retain it on the original dispatch frame. The task
root's `begin_machine_turn(turn)` binds the original outer frame and turn
identities, original limit and starting cumulative instruction receipt. A
repeat admission with that same turn cannot reset its start. All machine
entries and segments in that turn, including reentry after an empty machine
chain, share the remaining allowance derived from the retained root ledger.
Do not use a hook-mutable consumed-work projection as a second accounting
authority. A resumed outer host frame creates a fresh turn with the original
limit; it retains the same root, meter, spent ledger and execution ceilings.
Finite scheduling cannot detach source evaluation or nested arbitrary public
host dispatch; reject such task admission before native begin.

Keep the existing adapter advance cap of 65,536 instructions and additionally
clamp it to the turn's remaining scheduling allowance. Begin remains an
admission-only zero-quantum transition, even when the preceding machine entry
spent the turn allowance. Validate and publish that accepted frame and consume
its input operands once, then retain its real runnable event. This avoids a
second pending-entry protocol: a later turn does not repeat the Call tick,
begin admission or operand pops. Native initialization still occurs only on
the first positive advance. Callback reply likewise consumes its request and
outputs exactly once at zero quantum before any resulting runnable yield.

Use a distinct exact frozen `ForeignMachineCursor`, with root token/root ID,
invocation ID, operation token and receipt observations, and fixed
`host_yield=True`. It carries no invented semantic XT/IP. The root retains one
exact issued cursor record containing the original runnable event, frame,
resume target, token/receipt and independent scalar evidence. A constructed or
copied cursor has no authority. `machine_cursor_evidence(cursor)` exposes an
immutable original snapshot for the existing engine-owned suspension witness;
`resume_machine(cursor)` validates and consumes the exact live issuance once,
then continues that retained event. Cancellation/close invalidates it. Neither
API reconstructs native state from public cursor fields.

Only an actual `ForeignRunnableYieldV1` may publish a machine cursor. A CALL or
RET completed on the last scheduling instruction retains its real callback or
return outcome; callback semantic execution may continue in the same host turn.
If a zero-quantum reply reports terminal fuel exhaustion, preserve that failure
instead of fabricating a runnable yield. A positive advance returning zero
work remains a no-progress error. A scheduling yield itself adds no semantic
tick, native instruction/cycle, IDL, wake or work receipt.

The ordinary dispatcher explicitly propagates this cursor from direct foreign
entry, Call/CallSelf, callback Return and `_continue_foreign`, and recognizes
it before resolving a resumed semantic XT/IP. The original outer frame supplies
the turn to `root_for`; nested semantic leaf frames cannot issue fresh turns.
The existing `_SuspendedExecution` holds either the semantic cursor or this
machine cursor, the original selected quantum and task root. The one existing
lease, suspension handle and engine witness validate both forms, using the
no-work parked-chain query before detach and resume. Machine yields use
`YieldedExecution`/`resume_yielded` and do not acquire an IDL wake receipt.

Qualification must compare synchronous execution with limits 1, 2 and larger
than the workload, covering accepted zero-work entry, a final-quantum CALL,
zero-quantum reply, final-quantum RET, multiple entries in one turn and empty
chain reentry. Assert exact consumed operands, PC initialization, outputs,
stores, cumulative work and one-shot token/receipt use. Include instruction,
callback, entry and semantic fuel exhaustion across repeated host yields;
copied/stale cursors, turn replacement, cancellation and failed later suspension
publication; and the prepared KDOS child-IDL/THROW/input journeys above. Keep
the synchronous and private-profile gates intact before any public capability
claim.
