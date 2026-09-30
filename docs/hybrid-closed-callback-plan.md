# Closed integer callbacks: Phase 5B1

Date: 2026-09-30

Status: locked narrow implementation contract, committed before code. Phase
5A has passed its native, composition, manifest and application gates. This
document enables no additional capability. The version 1 routine ABI and
version 2 canonical leaf descriptors remain unchanged.

## Purpose and boundary

Admit a small, statically closed integer colon definition as a synchronous
machine callback. A signed clamp is sufficient as the first representative
policy:

```forth
: CLAMP ( x lo hi -- bounded-x ) ROT MIN MAX ;
```

The existing machine CALL/request/resume/RET protocol, one parked machine
frame, original semantic meter, private callback context and untouched outer
caller stacks remain the boundary. No callback may enter another machine
routine, suspend, access a service, use ordinary memory, evaluate source or
mutate the dictionary. No arbitrary host primitive or dynamic XT is admitted.

The callback uses the existing eight-cell data stack and eight-cell return
stack in its separate 128-byte memory owner. It does not grow either stack or
copy the outer task's stack. This is a functional interoperability slice;
callback execution through this initial profile makes no new acceleration
claim.

## Source facts used by this contract

- `simulator/runtime.py::ColonDefinition` holds a tuple of operations from
  `simulator/ir.py`. Static `Call` records an XT; `CallSelf` is a separate
  operation. Compiler metadata is not a machine-code image.
- `_execute_top` charges one tick before each IR operation. `_call_from_colon`
  charges another tick when the called target is a primitive. A static call
  to a colon definition therefore costs its Call tick plus the callee's IR
  work; a primitive call costs two ticks. Colon entry does not add a separate
  tick, and every executed Return costs one.
- The dispatcher pushes a root continuation for an ordinary colon entry and
  one continuation for each active colon call. `ReturnStack.push_continuation`
  represents each with one guest cell plus retained cookie metadata. The root
  consumes one of the eight return cells.
- A colon accelerator can replace a body with a single charged host callback.
  Native planning can execute multiple IR operations before Python regains
  control. Neither path currently establishes the admission checks required
  here. Accidental native decline for a private context is not a contract.
- The existing export engine captures canonical leaf Words and validates
  private stack ownership immediately after the admitted tick. Its one-shot
  top-level primitive guard is insufficient for a transitive colon closure.
- `hybrid/server.py::prepare_server` registers manifest routines before
  `prepare_image_bootstrap` reads and evaluates KDOS or autoexec. This is what
  lets ordinary boot-source compilation resolve declared machine words. A
  colon definition supplied only by later boot source cannot be bound at that
  earlier publication boundary.

## Versioning and interfaces

Use an explicit metadata **version 3**, with profile
`closed_integer_colon`, for this capability. Version 2's descriptor is an
exact six-name catalog with `max_semantic_steps == 1`; changing its accepted
values to include arbitrary colon names would silently broaden a locked ABI.
Version 1 and version 2 constructors, loaders, exact-type checks and capability
reports continue to reject version 3 values.

Reuse the existing ABI family, metadata field layout, common validation and
single export registry. The version 3 export schema distinguishes two explicit
effect values: `integer_leaf`, with the existing six-name/one-step rules, and
`closed_integer_colon`, with the rules below. This permits a version 3 routine
to name both kinds without granting new authority to a version 2 descriptor.
Do not create a second runtime, registry or runner for version 3.

Existing metadata that contains an export descriptor needs the corresponding
version 3 value shape: export/site, image/declaration and request/result.
The manifest and new declarative policy values also carry explicit version 3
metadata; they contain only immutable declaration data.
Factor validation rather than copying implementations. Shared values remain
exact immutable data, with no Word, callable, captured IR or native token.
Extend the bounded manifest layout with an explicit version 3 discriminator
and the finite declarative policy table specified below. All independent
metadata/path/limit checks still precede image reads. No source-prelude or
deferred unknown-word resolution interface is introduced.

Native transport remains `RoutineRunnerV2`: its sealed code, callback offsets,
integer export IDs, arities, tokens and instruction accounting do not change.
The composition owner translates admitted version 3 metadata to that existing
native transport. Report metadata/profile version separately from transport
version; a version 3 metadata value is not evidence of a new machine ABI.

The existing engine-owned bind/verify/invoke and registration transaction are
the public seams. Extend their version dispatch, retaining opaque issued
handles and exact runtime ownership. Add no API that accepts a callable or
executes a caller-supplied captured closure. A verified descriptor is copyable
diagnostic metadata, not authority to bind or invoke an export.

For a closed export, the host provides these values:

| Field | Required value |
|---|---|
| Name | Existing printable, nonwhitespace ASCII name bound once at publication |
| Export ID | Existing exact integer range 0..63, shared across all profiles |
| Input/output cells | Exact integers 0..8 |
| Effect/profile | Exact `closed_integer_colon` discriminator |
| Local semantic allowance | Exact integer 1..4096 |
| ABI/version | Existing ABI family and exact metadata version 3 |

The engine derives the work and stack proof; caller-supplied claims never
substitute for that proof. Its verified diagnostics include derived minimum
and maximum work, required input depth, net data-stack change, maximum data
depth and maximum return depth. These immutable diagnostics grant no entry
authority. Binding fails if the derived maximum work exceeds the declared
allowance or either private stack exceeds eight cells.

## Application admission before boot source

Keep version 3 available through the unified application, using a bounded
declarative IR table in the manifest. A host-programmatic API alone would
qualify only preconstructed HybridSession use, not the CLI/server path; it
would not satisfy this slice's application gate. Conversely, running arbitrary
Forth before manifest registration would introduce an unqualified execution
phase. Neither workaround is part of this contract.

The version 3 manifest adds a required `policies` array, possibly empty, with
at most 64 entries and 4096 total operations across the table. Its existing
manifest-byte cap remains unchanged. Each policy has exactly `policy_id`,
`name`, `input_cells`, `output_cells` and `operations`. Policy IDs are exact
integers 0..63, unique in their own namespace; names use the existing bounded
ASCII word-name rules and arities are 0..8. The operation array admits only
these exact JSON object shapes:

| Shape | Normalized semantic IR |
|---|---|
| `{"op":"literal","value":N}` | Literal of exact uint64 N |
| `{"op":"call_core","name":"ROT"}` | Call to that originally captured canonical catalog Word |
| `{"op":"call_policy","policy_id":N}` | Call to another declared policy's exact installed Word |
| `{"op":"branch","target":N}` | Forward Branch to a local operation index |
| `{"op":"branch_zero","target":N}` | Forward BranchZero to a local operation index |
| `{"op":"return"}` | Return |

Reject unknown/duplicate fields, Boolean integers, unsupported core names,
missing policy IDs, cyclic references and every out-of-range branch before
reading machine image files. No numeric XT, source text, file include, callable,
compiler word or machine-registration target appears in this grammar.
Normalization preserves one manifest operation per semantic IR operation,
so branch indexes do not change during installation.

The version 3 `exports` table has two exact variants. An `integer_leaf` entry
has `export_id`, `effect`, `name`, `input_cells`, `output_cells` and
`max_semantic_steps`, retaining the six-name and one-step rules. A
`closed_integer_colon` entry has `export_id`, `effect`, `policy_id` and
`max_semantic_steps`; its normalized descriptor derives the name and arities
from that declared policy. These derived values cannot disagree with a second
copy of the same metadata. Every closed manifest export must refer to this
table, never a name expected to appear in boot source. Callback sites retain
their existing export-ID references.

Perform structural, call-graph, stack and work admission on these immutable
declarations before image reads, using the fixed canonical primitive effect
table. Policy name collisions with another policy or a declared machine word
are errors under the dictionary's naming equivalence. After constructing the
fresh runtime, reject collisions with any already installed dictionary Word
before defining any policy. There is no implicit shadowing or policy reuse
by name during manifest installation.

Application preparation then follows this order:

1. Load and validate all manifest metadata, policy proofs and bounded images.
2. Construct the existing HybridRuntime, memory and storage owners; verify
   all required canonical core Word identities captured by its export engine.
3. Install policy definitions in dependency order through `define_colon`,
   constructing exact Literal/Call/Branch/BranchZero/Return objects directly.
   Resolve `call_core` only through captured canonical identities and
   `call_policy` only through the exact Words just installed. This publishes
   ordinary colon metadata and executes no Forth source or policy operation.
4. Revalidate/capture those exact installed definitions through the common
   closure admission engine, then bind exports and publish machine routines
   using the existing registration transactions.
5. Call unchanged `prepare_image_bootstrap` with that preconstructed semantic
   runtime. Its ordinary checked KDOS/autoexec compilation can now resolve
   every declared machine word. Create the session/server only on success.

The server owns a fresh, unexposed runtime during these stages. Any staging
failure closes that complete owner and releases its native pins; no partial
application or socket becomes visible. Existing per-registration rollback
still applies. If a policy-publication helper later accepts a caller-owned
runtime, that separate API must first provide exact dictionary/index rollback
for newly introduced policies; do not infer such authority from this startup
path. There is no second persistent policy registry: installed dictionary
Words and the single export engine retain the authoritative identities.

Host-programmatic registration may independently capture an already-live
admitted colon Word, as described below. It does not require a declarative
manifest. Both installation routes converge on the same proof and dispatch
guard. Later source shadowing keeps the original captured Word binding;
removal, rollback, XT reuse or body mutation revoke it normally. Undeclared
machine names remain ordinary unknown-word compilation errors. No deferred
lookup, placeholder primitive, extra bootstrap pass or Akashic dependency is
introduced.

## Bounded admission and immutable capture

Resolve the named entry once to an exact live Word with an exact
`ColonDefinition`. Capture its original Word, XT, implementation object and
operation tuple under the runtime's existing owner boundary. Resolve every
static Call transitively at publication and retain the exact target Word and
implementation; never resolve a name again during invocation.

Admission retains a bounded, engine-owned immutable proof with strong
references to every captured Word, definition, operation tuple and operation
object, plus independent exact integer field snapshots. A frozen dataclass is
not sufficient evidence: `object.__setattr__`, implementation replacement or
XT reuse must not alter a previously admitted closure undetected.

The initial hard metadata bounds are 64 captured Words and 4096 total IR
operations across distinct captured colon definitions, per export. The
existing 64-export session bound applies across leaf and closed profiles.
These limits bound analysis and retained proof memory; sharing proof storage
is an optional optimization, not an excuse to omit a bound.

The static call graph must be acyclic. Every operation in every captured
definition is validated, including unreachable suffixes. Every branch target
must be a valid operation index strictly greater than the branch's own index.
Every reachable path must terminate in Return; falling past the operation
tuple fails admission. Reject CallSelf, direct or mutual recursion, backward
branches and all loop operations in this slice. Finite runtime fuel alone
does not establish a finite private-stack bound.

Admit exact IR classes only:

| Operation | Admitted meaning |
|---|---|
| Literal | Exact uint64 value; push one cell |
| Call | Exact nonzero uint64 XT bound to a captured admitted target |
| Branch | Forward jump to a validated local index |
| BranchZero | Pop one cell; follow either forward edge |
| Return | Consume the current ordinary colon continuation |

An IR subclass, Boolean index, forged out-of-range field, unknown operation,
dynamic invocation or other definition kind fails admission. Constants must
appear as literal IR values; admitting mutable VALUE/CREATE bodies or another
definition kind is not part of this gate.

Primitive targets must be exact originally installed canonical Words and
callbacks from this fixed internal catalog:

| Words | Required data depth | Data depth change |
|---|---:|---:|
| MIN, MAX, AND, OR, XOR | 2 | -1 |
| ABS | 1 | 0 |
| DUP | 1 | +1 |
| DROP | 1 | -1 |
| SWAP | 2 | 0 |
| OVER | 2 | +1 |
| ROT | 3 | 0 |

These are total uint64-cell operations when the stack proof holds; MIN, MAX
and ABS retain their existing signed-cell interpretation. Capture them at
the canonical core-install boundary, before shadowing, including the five
stack operations needed by closed policies. A same-named replacement is not
canonical. The extra stack words are internal admitted dependencies, not new
top-level version 2 leaf exports.

No admitted primitive may return Invoke, dispatch another Word, access a
service or fault handler, or obtain guest addresses through the context.
Arithmetic extensions, comparison catalogs and loops can be separate later
admission additions if a measured or concrete policy requires them.

## Work and stack derivation

Analyze the acyclic call graph in dependency order and each local forward
control-flow graph in reverse operation order. Derive path summaries rather
than expanding an exponentially repeated call tree. Stop and reject as soon
as a hard bound is exceeded.

Propagate required entry depth, net data-depth change and maximum temporary
growth. Both alternatives of BranchZero must be safe for arbitrary cell
values. At each control-flow join, require the same data depth; every Return
must produce the same declared output depth. Combining a Call uses the
callee's proof rather than its name or a guessed primitive stack effect.
Starting from the descriptor's input count, no path may underflow or exceed
eight data cells. Check intermediate primitive effects as well as their net
effect; the fixed catalog's effect table is justified by the current source.

The longest active colon-call chain, including the entry root, must fit eight
return cells. No guest return-stack operation is admitted, so the only cells
there are the dispatcher's correctly typed continuations. The normal return
must leave depth zero; retained continuation cookies are validated through
the existing ReturnStack machinery, not replaced by an untyped host list.

Charge derivation follows ordinary reference dispatch exactly:

| Action | Semantic ticks |
|---|---:|
| Literal, Branch, BranchZero or Return | 1 |
| Call to an admitted primitive | 2 total: Call plus primitive |
| Call to a captured colon | 1 plus that callee's executed path |
| Root colon entry / continuation push | 0 additional |

For example, `ROT MIN MAX ;` costs seven ticks: three primitive Calls at two
ticks each and the final Return. Its data depth remains at most three and it
uses one return cell. A direct call to that policy from another closed colon
adds that caller's Call tick and a second active continuation.

Retain conservative minimum and maximum work bounds over the admitted
control-flow paths. Branch feasibility need not be inferred from cell values;
both edges may contribute to the proof even when a literal makes one edge
unreachable in practice. Each path's tick cost is exact, but the reported
range need not be tight. Binding requires the maximum bound at most 4096
and at most the declared local allowance. At invocation,
the original outer semantic meter and cumulative callback-work allowance can
still be smaller: actual execution stops at that existing budget boundary.
Do not precharge a worst-case path or reject an invocation merely because an
unselected path would exceed its remaining outer allowance. Charge only
actual ticks and retain their prefix on failure.

Use an engine-owned composable local allowance check beneath dispatcher
charging, alongside the original meter and on-tick route. No callback gets a
replacement outer budget, and no failure/return refunds work. The machine
request count remains one per real declared CALL, independent of how many
semantic Calls the policy executes. Existing root ceilings remain 1024
callbacks, 65536 callback semantic steps and 10000000 machine instructions;
per-routine machine limits and lower configured/root limits still apply.

## Dispatch and late mutation checks

Execute through the ordinary semantic dispatcher, with a fresh canonical
private context. While this closed export is active, bypass colon accelerators
and native interval planning explicitly, without invoking their predicates.
The outer runtime can still select native execution; diagnostics must report
the closed callback's actual reference dispatch. Native acceleration of this
profile requires its own equivalent admission and metering gate later.

Extend the engine-owned dispatcher seam to authorize each captured operation,
static target and continuation transition. The guard must be pinned before
an accounting hook can run and rechecked after the tick, immediately before
the operation's effect. The primitive part of a Call has its own second tick
and requires the corresponding post-tick recheck. Never call unguarded
`_invoke_primitive` or permit an Invoke result as an escape from the closure.

Check exact runtime/dictionary ownership, Word liveness, XT identity,
implementation and operation identities/fields, canonical primitive callback
identities, and private stack/memory authority. Recheck relevant stack depth,
data and continuation evidence across accounting hooks so a hook cannot
replace an admitted input or return cell after preflight. The context and
method routes must remain canonical; custom stack/dispatch behavior declines
the profile. A hook exception follows the ordinary failure cleanup policy.

Before every callback and machine resume, revalidate the whole bounded
closure. During semantic dispatch, validate the currently used captured
operation/target immediately before its effect. A generation counter may
trigger work but never replace exact checks. Shadowing preserves a captured
live Word; removal, rollback, XT reuse, definition/operation mutation or a
replaced primitive revokes eligibility. No new binding is created implicitly.

Keep the current one-active-export and one-parked-machine-frame rules. New
public source evaluation, callback invocation, machine entry or publication
from inside this callback is rejected before its effects. Ordinary static
colon calls inside the proven closure remain dispatcher operations, not
recursive calls to the public execute API.

Registration extends the existing export/dictionary/native-publication
transaction: failure removes only newly issued exact bindings, preserves
preexisting exports, repairs the index and revokes any newly published native
specification. Failed repair preserves the original error and disables further
interop entry until close. No parallel registration registry is introduced.

## Failure, clocks and lifecycle

Retain the isolated callback profile's failure settlement: discard unpublished
callback outputs, cancel the pending machine token, retain completed machine
stores and work, and preserve the outer caller's input cells. Preserve the
qualified 5A distinction between an owned interop failure and an exception
escaping from host execution:

- Bridge-generated admission, native-result, stale-registration and reply
  failures retain their defined `HybridExecutionError` classifications and
  callback location. Where the existing boundary deliberately translates an
  error, retain its original cause. Do not collapse distinct outcomes into
  one callback-failed category.
- An original exception raised by a host hook or executing callback remains
  that exact exception object/type, including `InstructionFault`,
  `StepBudgetExceeded`, allocation errors and host interruption. Settle and
  cancel owned work, then re-raise it with its traceback. Do not wrap every
  host exception in HybridExecutionError or reinterpret a raw host
  InstructionFault as an authorized guest FAULT-XT! event.
- Retain 5A's exact registered primitive host-escape authority across the
  bridge. It is scoped to that original implementation/callback identity;
  never disable ordinary guest fault routing globally or grant the escape
  to a same-named replacement. Policy-admission errors use the engine's typed
  admission error, not a fabricated InstructionFault.

Catch BaseException only for owned cleanup and then propagate. Cleanup failure
preserves the original error, adds its cleanup evidence and disables interop
until close. These isolated callbacks do not enter CATCH/THROW or an unrelated
task's ABORT handler. No callback suspension, host-quantum yield or replacement
meter is introduced. Close retains the existing cancel/revoke/release order.

Semantic ticks advance existing hosted semantic accounting and timer;
machine instructions retain their real cycles. The bridge adds no time and
does not alter host-monotonic/manual RTC ownership. Record callback actual
steps and the derived bound separately from machine deltas and root totals.

## Implementation order and completion criteria

1. Lock version 3 value/manifest admission and a pure bounded proof model.
   Gate exact types, all branch/closure bounds, same-depth joins, all paths
   returning, exact work, input/output arity and both eight-cell stack limits.
   Test direct/mutual recursion, hidden unsupported suffixes and forged frozen
   fields. Prove malformed policy metadata fails before any machine image
   read, and installation never evaluates source. Version 1/2 acceptance and
   rejection behavior stays unchanged.
2. Extend the existing export engine and dispatcher guards. Compare clamp,
   forward conditional policies and a nested static colon helper with direct
   ordinary reference execution, under both selected outer executors. Prove
   exact ticks, no accelerator predicate/native interval bypass, and no
   arbitrary primitive or fault callback. Exercise revocation before entry
   and in accounting hooks between Call and primitive ticks.
3. Extend versioned composition/manifest admission through the existing native
   v2 transport. Compare a machine buffer loop using the closed clamp callback
   with ordinary MP64 segments supplied independently computed results. Check
   output cells, full shared buffers, flags/registers, actual CALL/RET work,
   lower root budgets, failure prefixes and publication rollback.
4. Gate original outer stack bytes, retained return cookies/captures, exact
   identity after redefinition/removal/XT reuse, cancellation and close. Reject
   machine reentry, IDL, dynamic EXECUTE and every service effect. Prove raw
   host InstructionFault identity survives with zero guest fault dispatch,
   while bridge-generated failures retain their typed classifications.
5. Add a bounded CLI/server preparation smoke using the existing MegaPad
   `_boot_image` fixture: the manifest declares CLAMP IR and a machine caller,
   bootstrap source compiles and invokes that machine word, and HybridSession
   exposes correct output/status/counters. Gate both selected semantic
   executors, policy name/ID collisions, failed publication/owner cleanup,
   rejection of a closed export naming only a later boot definition, and an
   undeclared machine word's unchanged checked-source error. No external
   application checkout or physical-display claim is needed for this gate.

Completion requires these compatibility gates and a recorded bounded host
timing/control run. A useful callback capability can be complete even when it
is slower than the equivalent direct semantic computation; no native speedup
or default change follows automatically. Run repository gates sequentially
and record only observed evidence in the local implementation commit.

## Work remaining after this slice

The value/proof and strict manifest layer passed 483 Make-selected checks on
2026-09-30: 68 closed-ABI cases, 133 v3 manifest cases, and 282 existing v1/v2
callback/manifest cases. This qualifies metadata admission only. Live export
authority, composition, application startup and bounded timing remain separate
gates below this checkpoint.

- **5B2:** at most eight parked machine frames, distinct registrations only,
  parent integer-state restoration, disjoint control leases, narrowing child
  borrows, transitive local/root budgets and chain-wide failure cleanup. Shared
  memory, I-cache and accumulated machine cycles must not be restored backward
  when a parent resumes. Same-registration recursion remains unadmitted.
- **5C:** finish typed isolated-context failure qualification first, then lock
  a separate shared-task stack/failure ABI for CATCH/THROW, RP! and ABORT. Use
  original HANDLER/task identity and engine-owned foreign continuations; do
  not broaden this private-context profile to inherit task handlers.
- **5D:** dispatcher-owned foreign operations and one composite suspension,
  retaining the original meter/ledger, leases, cursor and all parked frames.
  Qualify distinct quantum-yield versus IDL wake authority, exactly-once
  receipts, unchanged finite budgets, cancellation and session close.
- **5E:** admit scalar FP/FPCSR, bounded borrowed-memory transforms and then
  selected transactional crypto/audio individually, after their error or
  scheduler prerequisites. Each needs its existing authoritative service,
  explicit effect/byte limits, oracle parity and a measured crossing workload.

Compiler/native-dictionary integration, guest JIT, CREATE/DOES> machine
mappings, arbitrary images/self-modification, native snapshots and multicore
hybrid execution remain separate designs and gates. None is enabled by a
closed semantic call graph or by reusing the native v2 callback transport.
