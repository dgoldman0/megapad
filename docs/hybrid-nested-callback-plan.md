# Nested machine callbacks: Phase 5B2

Date: 2026-09-30

Status: locked implementation contract, committed before code. This document
enables no capability. Phase 5A is qualified; the
[5B1 closed policy profile](hybrid-closed-callback-plan.md) is in progress and
must qualify before this slice. Their one-frame and no-child-entry rules
remain unchanged.
This narrows the nested-transition section of
[`hybrid-interop-plan.md`](hybrid-interop-plan.md); it does not enable its later
shared-task, suspension or service profiles.

## Purpose and version boundary

Admit this synchronous path through the existing owners:

`semantic caller -> machine A -> closed policy -> machine B -> policy return -> machine A`.

A child is an exact registered machine Word captured as a static dependency
of the admitted policy. An arbitrary XT, callable, new source evaluation or
public runtime reentry is not a child-call interface. At most eight machine
invocations may be active at once, counting the root. All eight must use
distinct issued registrations and disjoint private return-stack leases.
Sequential calls to the same child after its previous invocation has returned
are permitted. Simultaneous recursion is not.

Use explicit **metadata and manifest version 4**, with the new policy effect
`closed_integer_nested`. Version 4 can also describe the already admitted
`integer_leaf` and `closed_integer_colon` effects under their existing rules.
The version 3 grammar and closed profile continue to reject machine targets.
An old loader, shared descriptor or runtime must reject version 4 explicitly.

Use a distinct **native transport version 3**, exposed as `RoutineRunnerV3`
and `HYBRID_NESTED_ROUTINE_ABI_VERSION = 3`. Its exact capability is
`distinct_registration_children`, with maximum active depth eight. The
existing `RoutineRunnerV2` remains the transport for metadata versions 2 and
3; no optional argument silently grants it child entry. Native transport
version, metadata version and admitted effect are separate status fields.

Share the current decoder/interpreter, admission checks, published-image
validation, receipts and token machinery through internal helpers. Do not
fork an interpreter or create another CPU, memory image, semantic runtime,
service owner or persistent export registry. The V3 Python binding must not
expose an inherited V1/V2 entry method that can replace a parked frame.
Explicit use of an older runner on the same CPU remains excluded by native
CPU reservation, independently of composition checks.

### One owner across transport versions

Mixed metadata versions remain supported in one `HybridRuntime`. Use one
internal native owner for the CPU, pinned mapping/control buffer, boundary
admission, reservation, publication identities and aggregate sealed-publication
count/bytes. Keep one composition registration table and control-stack
allocator across all versions; selecting another facade cannot replenish a
resource limit or create another persistent export registry.

The existing `RoutineRunnerV1(state, control_base, control_buffer)` and
`RoutineRunnerV2(state, control_base, control_buffer)` constructors retain
their standalone behavior. `RoutineRunnerV3` accepts the same constructor
arguments, pins that owner once, and exposes `legacy_v2()`: an owner-bound
`RoutineRunnerV2` facade with its existing V1 methods. This factory accepts
no CPU, buffer or publication arguments and acquires no second mapping pin.
The V3 Python type does not inherit the V1/V2 entry interface. A separately
constructed runner still cannot claim an already pinned CPU.

Composition routes metadata version 1 through the facade's V1 methods,
metadata versions 2/3 through its V2 methods, and metadata version 4 through
the explicit V3 methods. The older routes keep their existing transport
behavior and status; sharing an internal owner does not promote an old
registration to a nested child. Every older entry or mutation rejects before
effects while a V3 chain owns the CPU, including legacy cancellation and
close. V3 close cancels its complete chain before releasing the shared owner.
When close is admitted through either view, it closes that owner and every
facade; retaining another view cannot retain entry authority or buffer pins.
The existing V2 resume/cancel/close protocol remains available for its own
parked invocation, and an unrelated V3 entry remains excluded during it.

Read-only receipt queries remain available. Preserve independent V2 and V3
receipt sequences and result shapes, so a V2 call followed by a V4 call and
another V2 call does not introduce a gap into the existing V2 settlement
sequence. Aggregate work still charges the same outer dispatch allowance.

Construction selects the V3 owner and legacy facade when the full transport-3
capability is available. If it is unavailable, the existing V2/V1 construction
fallback remains available only when no V4 capability is required. A required
V4 manifest fails capability preflight before exposing its session owner;
it must not silently fall back or begin publication through an older runner.

## Source constraints

The current source establishes several changes that a nested runner needs:

- `RoutineRunnerV2` in `emulator/accel/mp64_accel.cpp` owns one `Frame` and
  puts `RoutineCPUReservation` inside it. A child cannot create a second
  reservation on the same CPU. V3 needs one chain-owned reservation and a
  bounded stack of frames under that owner.
- `capture_parked_frame` saves 32 registers, selectors, flags, modifier,
  global cycle count and the live private return bytes. Its present validator
  requires the live CPU to equal that snapshot. Child work legitimately
  changes the CPU view and advances global cycles; parent state must instead
  be restored from a native-owned snapshot at the defined return boundary.
- `initialize_entry` resets more than PC and arguments. It also resets
  selectors, flags and integer/control fields, enables I-cache and clears the
  fetch window. A V3 frame audit must account for every field that child entry
  writes, not merely the fields ordinary arithmetic changes.
- The broad `MP64_EXECUTION_CHECKPOINT_SCALARS` scheduler checkpoint is not
  suitable: restoring its cycle, cache, performance or unrelated device state
  would undo completed child work.
- `hybrid/runtime.py` currently rejects entry with `_active_machine`, and
  receipt settlement expects invocation IDs to increase whenever they
  change. Resuming an older parent violates that expectation. Segment IDs
  remain monotonic; the current frame's invocation ID need not be.
- The current bridge adds a callback's inclusive outer-meter difference to
  its semantic-work counter. Doing that at every nested callback would count
  descendant ticks again. The engine needs one root charge per actual tick
  and separate inclusive local guards.
- `simulator/interop_exports.py` retains one active export/private context.
  The closed dispatcher needs a bounded active-export stack and an exact
  static machine-call transition. Ordinary primitive recursion through its
  public invocation API would bypass that proof.

## Captured graph and startup publication

Extend the version 4 declarative policy grammar with exactly
`{"op":"call_machine","routine_id":N}`. Routine IDs are exact integers
0..63, unique within the manifest's machine table. All version 4 routine
entries carry that ID. It is a publication reference, not an XT or executable
address. All other policy operations retain version 3 meanings and bounds.
No dynamic child selector, source prelude, placeholder Word or deferred name
resolution is introduced.

Every version 4 routine also declares `max_callback_requests`, an exact integer
0..1024. This is a per-invocation ceiling, independently enforced across that
frame's own segments as well as by the shared dispatch ceiling. It appears in
the immutable image/declaration, manifest and native spec. Zero permits a
callback-free execution path; reaching a declared callback CALL then retains
the completed CALL effects and fails with `callback_limit`, without issuing
a request. A routine with one intended callback can declare one even when its
machine instruction limit is much larger.

Before image reads, validate the combined dependency graph of policies and
machine declarations:

- A policy depends on each statically called policy or machine registration.
- A machine depends on each policy exported by its declared callback sites.
- Canonical integer leaves are terminal dependencies.

Reject every cycle, including one passing through a machine, and reject a
longest machine chain greater than eight. Validate unreachable operations
and unused declared sites too. Keep the existing session limits of 64 machine
publications, 64 policies/exports as applicable, 4096 declared policy
operations and 16 callback sites per machine. Analysis uses bounded graph
summaries rather than expansion of all paths. An instruction loop within a
sealed machine remains subject to its finite instruction limit; it does not
create a static graph cycle by itself.

Install this acyclic graph dependency first in the fresh, unexposed server
runtime. A child machine is published before a policy that calls it; that
policy is installed and captured before a parent machine exports it. Use
ordinary `define_colon` and the existing registration transactions. Normalize
`call_machine` to a static `Call` to the exact just-published machine Word.
No routine executes during installation. Boot source is evaluated only after
the whole graph is installed, so ordinary source compilation can resolve all
declared words through the existing bootstrap path.

For host-programmatic use, capture only already-published dependencies.
Capture exact Word, XT, implementation, registration identity, code/body
lease, control lease, signature, buffer rules and publication generation.
Keep independent numerical snapshots alongside strong references, following
5B1's mutation checks. Removal, XT reuse, altered IR, replaced registration,
lease revocation or a changed seal invalidates the affected dependency.
Shadowing alone does not retarget it. Shared descriptors and diagnostic graph
copies carry no Word, callable, native token or entry authority.

All machine nodes in a V4 nested graph use the V3 sealed transport, including
nodes without callbacks. There is no implicit promotion of an existing V1
or V2 registration to a nested child. The host can explicitly register an
equivalent image in V4 after the full new admission checks.

## Native API and authority

The following names fix the proposed native surface; argument containers
retain current exact-type and uint64 validation. A separate shared V4 result
adapter presents the corresponding metadata values to application code.

| Interface | Meaning |
|---|---|
| `RoutineSpecV3` | Immutable V2-equivalent sealed integer image/site proof, `max_callback_requests`, issued registration and private-stack identity |
| `publish_code_v3(spec, child_edges)` | Idle publication; bind exact already-published child specs to a declared callback/export and captured static call edge |
| `begin_root_v3(spec, arguments, spans, instruction_limit, callback_limit, protected_spans)` | Begin one chain using the outer dispatch's remaining allowances |
| `begin_child_v3(parent_token, child_edge, arguments, spans, protected_spans)` | Enter the exact admitted child while its parent callback remains pending; no new root allowances |
| `resume_callback_v3(token, outputs)` | Resume only the current top frame's pending callback through its real sealed RET.L |
| `cancel_chain_v3(token=None)` | Cancel the complete owned chain; no-token owner cleanup is idempotent, an explicit token remains strict |
| `last_segment_v3()` | Immutable copy of the latest native accounting receipt, without continuation authority |
| `revoke_code_v3(spec)` / `is_code_published_v3(spec)` | Exact idle identity operations needed by transactional publication rollback |

Publication issues opaque `ChildEdgeV3` handles retained by the composition
registration. Each is bound to the runner, parent publication, callback/export,
captured call edge and exact child publication/stack lease. A matching name,
ID or copied descriptor cannot fabricate one. The semantic engine permits use
only at that captured Call after its normal charged pre-effect checks. The
native runner independently checks the issued handle against the exact
pending parent token and current top frame. It does not trust a supplied
child spec or a caller-claimed active depth.

Each invocation receives its own opaque owner-bound callback tokens, including
children. A parent's token stays pending while a child runs; starting a child
does not consume it or fabricate a second machine CALL. Only its eventual
matching callback reply consumes it. A parent cannot resume while a descendant
is active. A child's token can never resume its parent or a sibling. Tokens
remain nonconstructible, noncopyable, nonserializable, one-shot, publication-
and sequence-bound; they do not keep the owner alive after close.

Permit repeated sequential uses of a captured child edge only when the
admitted semantic dispatcher reaches that Call again. They are new child
invocations with fresh identities and finite shared allowances. Native
validation still rejects a registration already anywhere on the active chain,
even if a copied spec or another issued edge names it. The composition's
semantic proof and native child-edge admission are complementary checks.

Validate shape, exact identities, live leases, depth, distinctness, signatures,
parent evidence, child code/cache seal, borrow narrowing and available limits
before changing registers, writing a child sentinel or publishing a frame.
Allocate the bounded frame/result storage before mutation where possible.
A preflight rejection leaves the valid parent token usable for native retry;
the production bridge treats such an escaping child failure as a chain failure
and cancels it. No public source evaluation, registration, dictionary mutation,
unrelated root entry, CPU mutation or raw machine execution is permitted while
any frame remains owned.

## Frame state, control leases and normal return

The chain owns one `RoutineCPUReservation`; an execution guard is acquired only
for each native segment. No CPU execution lock spans semantic dispatch, and
native frames retain no Python callable. Synchronous semantic Python frames
do remain active while an admitted child runs; this is not a resumable host
continuation. The existing public-mutation handshake
continues to protect the complete interval, including parked parents and
allocation/marshalling windows.

Use a fixed-capacity container of eight frame slots. Each retains its exact
registration/spec and generation, control lease, borrowed grants, local work
limits/counters, parent invocation identity, pending token/site and bounded
native integer snapshot. Each registration retains its own fixed stack slice
in the existing private control owner. The maximum remains 8192 cells per
slice and 4 MiB for that owner; no child resizes or reuses an active slice.

The saved parent execution view includes all 32 registers (therefore PC and
R15), PSEL/XSEL/SPSEL, packed flags and prefix/modifier state. Audit and either
restore or prove invariant every additional integer/control field touched by
child initialization: `sw`, D/Q/T/EF, halted/idle state, IVT/vector/trap/wake,
privilege/core identity and the private IRQ latch. Raw instruction-bus access
must remain absent. Selectors and invariant fields must still satisfy the
admitted profile at each boundary. Do not capture unrelated tile, FP, crypto
or device state under the guise of a whole CPU checkpoint.

The current initialization audit identifies these invariant values: full-core
profile, selectors 3/2/15, `sw=1`, interrupt/supervisor flags clear, D/Q/T/EF
zero, halted/idle false, modifier -1, IVT/vector/trap/wake zero, privilege/core
ID zero, one core, private IRQ latch false, no instruction-bus pointer and
I-cache enabled. Ordinary integer registers and arithmetic flags may change.
Validate invariants before accepting/restoring a snapshot; a host-mutated
parent must not become an admitted baseline. Q/EF affect branch evaluation
even though admitted instructions cannot modify them. Prefix decoding uses
the existing modifier; there is no separate persistent REX register to copy.

Maintain one chain cycle frontier updated after each segment. An ancestor's
saved cycle count cannot equal the live CPU after a child has executed and
must never be restored. I-cache undo scratch, decode plans, performance
counters and cache identities likewise remain outside parent restoration.
On successful child return, preserve the child's result and receipt before
restoring its parent. If result marshalling then raises, cleanup is bound to
the root-chain identity, not to the now-current parent invocation ID; cancel
the complete chain and retain the already-issued child receipt.

Save exact live parent return bytes from its current R15 to the original
stack-empty pointer, at most 64 KiB per frame and 512 KiB for eight frames.
These bytes are **evidence**, never a rollback image. Parent control storage
remains pinned in place. The child can access only its own control slice
through checked CALL/RET roles; ordinary buffer access cannot reach any
control slice. Corrupting a parked parent's evidence fails the chain instead
of repairing its bytes. Inactive cells remain outside the live evidence but
inside the protected allocation, and no admitted operation can write them
through an ordinary buffer grant.

On a successful child's real root RET.L:

1. Settle its final native segment, local totals and exact result registers.
   Validate its root sentinel, final stack pointer and return instruction
   through the existing interpreter path.
2. Revalidate the parked parent's publication, code/cache seal, control
   lease and live return evidence. The child must be the exact top frame.
3. Restore the parent's saved integer execution view and remove only the
   completed child frame. Invalidate the host fetch window; do not invalidate
   architectural I-cache lines or restore their tags/data/counters.
4. Return the child's outputs to the suspended private semantic policy Call.
   Replace that policy's machine-call inputs only after successful completion
   and the already-proved output-capacity check. Continue ordinary admitted
   semantic dispatch. It eventually replies to the parent's still-pending
   callback token, which executes its own real RET.L.

The parent is restored before the successful child result crosses Python
marshalling. Keep the terminal child's PC, outputs and counters in its result;
they cannot be read back from the now-restored CPU. If result marshalling
fails, cancel the still-owned chain and retain its accounting receipt.

Never restore shared ordinary memory, parent or child control bytes, I-cache
contents/replacement effects, machine cycle count, performance counters,
semantic meter, hosted timer, RTC or service state. A parent may observe its
child's permitted stores after returning. Global cycles advance through every
real child instruction. Parked evidence therefore records frame-local state
and a chain-owned accounting frontier, not an equality requirement against
the parent's old global cycle count. Refresh current top-frame evidence only
from native-owned saved data and validated current state; do not accept a
host-mutated view as a new baseline.

## Borrow narrowing and semantic contexts

Resolve every child's declared buffer rules from that child's original
argument tuple, using the existing checked length multiplication, address
addition and complete-span admission before mapping/device dispatch. Every
nonempty child span must fit wholly inside one immediate-parent ordinary
grant, with a subset of its read/write permissions. Do not synthesize a grant
by joining adjacent spans or upgrade read-only access through an alias.
Transitivity then keeps every descendant inside the root grants. Scalars that
happen to contain addresses grant no authority. A buffer-free parent can call
only a buffer-free child invocation.

Apply the full protected-span policy at every depth: complete semantic data
and return allocations, including observed inactive contexts; all dictionary
headers and sealed executable spans; the entire private control arena; and
every other existing protected allocation. Keep current physical-buffer alias,
bounded Bank0 and MMIO exclusions. Semantic policies remain integer-only and
cannot read memory themselves. They may calculate child addresses/lengths,
but the resulting values must pass fresh narrowed admission.

Each active semantic callback owns the same canonical private context shape
as 5B1: eight data cells and eight typed return cells in a separate 128-byte
owner. At most eight such contexts can be active, for at most 1024 bytes of
private stack storage. A child's callback gets a distinct context; its
ancestor policy's active and retained continuation metadata stay untouched.
No machine PC is inserted into a semantic return stack. A static machine
Call returns synchronously through the current semantic dispatcher; no typed
foreign continuation or suspension is introduced in this slice.

## Work proof and one shared dispatch allowance

Keep the original outer `_StepMeter` and the existing owner-bound allowance
that survives raw entry, source tokens and outer quanta. At root machine entry,
the V3 chain receives only the dispatch's remaining machine/callback allowance;
its native ledger is a restrictive mirror for that tree, not a new budget.
Every child inherits that same remaining native ledger. Child entry and resume
have no argument that can replenish it. Independent later root calls are
seeded from the already-settled outer remainder.

The limits remain: 1,000,000 own instructions per machine invocation,
10,000,000 completed machine instructions per outer dispatch, 1024 callback
requests per dispatch, 4096 inclusive semantic steps per callback and 65536
callback semantic steps per dispatch. Configuration and wrapper limits may
only lower these values. Each frame also enforces its declared
`max_callback_requests` ceiling, which cannot be renewed by child entry or
resume. Eight is the hard active-depth ceiling.

Stack admission uses each static machine target's declared input/output
effect exactly like a captured primitive Call; both the Call tick and
primitive-dispatch tick occur through the existing engine. Machine segments
add no semantic ticks. Child semantic callbacks contribute their actual normal
ticks. Private data/return-stack depth is proven separately for each active
context; finite fuel does not replace that structural proof.

Extend closed-policy work summaries transitively. A machine's conservative
callback-work bound is its maximum possible callback count, bounded by its
declared `max_callback_requests`, instruction maximum and the root callback
ceiling, multiplied by the
largest admitted callback work bound among its sites. A callback-free machine
contributes zero callback semantic work. Summaries are computed dependency
first, with saturating arithmetic that rejects once a required callback bound
exceeds 4096. A policy adds these child contributions to its ordinary IR and
primitive tick summary. This deliberately conservative first profile may
reject a large declared limit even when a particular machine path is short;
hosts can declare a smaller truthful callback-count or instruction bound.
Declaring one callback per frame keeps the full eight-frame path admissible
without multiplying the policy bound by every routine's instruction count.
It does not require
an unqualified machine loop analyzer or expand paths exponentially.

At execution, one engine-owned pre-effect tick path checks every active local
callback allowance, the callback-work root allowance and the ordinary outer
semantic meter. Charge each actual semantic tick to the root once, while
charging it inclusively to every active ancestor callback's local allowance.
Do not sum ancestor meter differences into the root counter. Admit actual
prefixes under remaining budgets rather than precharging the worst-case
proof. The existing post-accounting-hook identity/stack checks still apply
before each effect, including the machine Call's second tick.

Every real native instruction charges its own frame once and the shared root
once. A parent's invocation totals exclude child instructions/cycles; root
totals include every frame. Descendant work never refunds a local or root
allowance. A declared CALL that consumes the final machine allowance retains
its real push/PC/count effects; the bridge dispatches no semantic callback
when no return instruction allowance remains. A child that consumes the last
root instruction cannot make the parent's pending return free. The next
required parent step fails with the completed child prefix retained.

Receipts use a runner-monotonic segment ID and include root-chain identity,
invocation ID, parent invocation ID, depth, segment deltas, frame totals,
chain totals and an explicit invocation-start indication. Returning to an
older invocation ID is valid. Count transitions from accepted invocation
starts, not from changes in the latest invocation ID. Settle each receipt
exactly once before any subsequent native boundary, on normal return and on
exception. The synchronous engine must not hide multiple native boundaries
behind a wrapper that exposes only the last receipt. A single retained POD
receipt therefore remains sufficient; no unbounded receipt history is needed.

Record completed-prefix receipts before result allocation/marshalling and
preserve them through cancellation/close. Zero-work accepted terminal segments
still obtain a new segment ID; rejected preflight does not. Allocation failure
must leave the entire chain cancellable and must never permit another entry
to recover spent budget. Semantic steps retain the hosted timer policy;
machine cycles and RTC retain their separate existing authorities.

## Failure, cancellation and close

Any execution/profile/budget failure in a child or its callback terminates the
complete chain. Preserve completed shared stores, real stack writes, cache
effects, machine cycles and semantic ticks. Do not publish child or ancestor
callback outputs, resume a parent's pending RET, or continue an enclosing
policy by catching the failure. Outer semantic input cells retain the current
success-only replacement rule. Existing semantic dispatch guards clean up
each private context's continuations while unwinding.

Capture the failing frame's bounded diagnostics and ancestor invocation IDs
before clearing frames. Leave the CPU's integer view at that failing native
prefix for diagnosis; cancellation is not a successful return and performs
no parent restoration or guest instruction. Failures during semantic work
refer to the current parked frame and export/Call location. Do not translate
profile failures into guest THROW, `FAULT-XT!`, task-origin ABORT or an older
handler context.

Cancel bottom-up, revoke every pending native token/child-edge use in that
chain, release context/frame references and finally release the one CPU
reservation. Revoking an invocation does not revoke an otherwise live idle
publication. Explicit-token cancellation accepts only the current top pending
token and cancels the whole chain; no-token owner cancellation works even if
marshalling failed before any token reached Python. An inactive no-token
cancel is idempotent. Copies, parent-token misuse while a child is active,
replay and foreign-owner tokens reject before mutation.

Catch `BaseException` only to settle/cancel owned work and then propagate it.
If cleanup itself fails, preserve the original error, mark the composition
owner unusable for further entry and retain a safe close path. Close cancels
the complete chain before revoking registrations and releasing pinned memory;
retained tokens cannot keep a released control arena callable. No Python
callback or destructor runs under native execution admission. As in V2,
public close during an actively executing native boundary rejects; owner
cleanup after that boundary can close every parked level.

## Implementation order and qualification

Commit this contract before code. Root runs the repository's builds, focused
gates and bounded measurements sequentially; the following is required
evidence, not a claim that any gate has run.

1. **Shared values and admission.** Add V4 immutable values/strict manifest
   discrimination, combined graph summaries, exact child-edge capture and
   dependency-first startup. Prove cycles, depth nine, duplicate registration
   identities, forged fields, unknown targets and version confusion fail
   before publication or image reads as applicable. Retain V1/V2/V3 behavior.
2. **Native chain.** Add V3 transport with fixed frame storage, one reservation,
   root ledger, separate tokens/control leases and bounded receipts. Share the
   existing fetch/decode/execute/accounting path. Keep existing V1/V2 tests
   unchanged and green before composition enables children.
3. **Semantic composition.** Add only the captured static machine-Call seam,
   active private-context stack and composable local tick guards. Explicitly
   bypass native interval planning and colon accelerators for these closed
   callbacks, as in 5B1. Preserve engine-owned post-tick validation and raw
   semantic-entry budget continuity.
4. **Application capability.** Wire strict V4 startup and truthful status only
   after the lower gates. Report maximum observed active depth, distinct
   invocation/segment/request counts, exclusive machine totals and root
   semantic totals; do not claim native callback execution or acceleration
   merely because the outer semantic executor is native.

Native differential fixtures must run the same sealed instructions through
the ordinary MP64 interpreter. Independently pause reference execution after
each real declared CALL, save only the specified parent integer state,
initialize and run the child's ordinary segments, supply independently
computed semantic callback outputs, and restore that parent state at the
specified successful child return. Use the same shared backing and continuous
reference I-cache/cycle state. Compare every segment's PC, registers, flags,
selectors, live control bytes, ordinary writes, completed counts and cycles.
Do not call the V3 runner to generate the reference frame restore or expected
semantic outputs.

Required boundary cases include:

- Two levels and the full eight; ninth-level rejection before child effects;
  repeated sequential siblings; direct/mutual active registration reuse;
  separate runner/CPU owner mismatches and all public CPU entry exclusions.
- Parent values in every integer register, nontrivial flags, internal nested
  machine CALLs, changed child scratch registers, disjoint sentinels/stacks,
  preserved inactive control bytes, and callback arities zero through eight.
- Parent and child code mapped to colliding I-cache lines, warm repeat calls,
  child stores that alter later parent data, explicit code publication,
  stale resident seals, and no fabricated crossing cycle/cache flush.
- Child subsets at permission/span boundaries, attempted widening/read-to-write
  promotion, integer wrap, union-of-spans escape, Bank0 alias, MMIO, every
  ancestor semantic stack, dictionary header/code and control-arena rejection.
- Root exhaustion in a child and before a parent RET, separate child/parent
  local instruction exhaustion, final-budget CALL, callback count exhaustion,
  exact per-frame callback ceilings (including zero and one),
  ancestor semantic allowance exhaustion inside a descendant, and exact-once
  root ticks/receipts as invocation IDs return to older parents.
- Wrong, copied, replayed, parent/child-swapped and cancelled tokens; corrupted
  parent return evidence; stale Word/IR/lease/publication at each return;
  failed child preflight followed by valid native retry; production whole-
  chain cancellation on the same error.
- Completed stores before failures at every depth; no output publication on
  failure; marshalling/allocation failure before token delivery; cleanup
  failure preserving the original error; close with all eight frames parked;
  buffer pins released only after chain cleanup, even with retained tokens.
- Both semantic executor selections, exact reference-dispatch callback work,
  outer task data/return bytes and retained cookies, rejected service/source/
  dynamic-XT/suspension paths, and no machine execution lock during Python.
- A real bounded `SessionServer.dispatch` journey from a strict V4 manifest:
  root machine calls a closed policy that statically invokes a second machine,
  whose leaf callback returns normally. Assert exact source semantic work,
  machine instructions/cycles, requests, invocations, segments and depth.
  Missing/stale native V3 capability must fail before exposing the session.

No shared-task CATCH/THROW/RP! behavior, foreign continuation, IDL/quantum
suspension inside the chain, service access, machine MMIO, recursion allocator,
multicore dispatch or external-project change is part of this slice. Those
remain separately locked gates in the interoperability roadmap.
