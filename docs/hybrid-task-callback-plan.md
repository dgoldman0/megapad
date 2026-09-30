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
