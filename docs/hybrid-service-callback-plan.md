# Private scalar service callbacks: Phase 5E

Date: 2026-09-30

Status: locked implementation contract, committed before code.
This document enables no capability. It narrows the selected-services section
of [hybrid-interop-plan.md](hybrid-interop-plan.md). The
[task callback contract](hybrid-task-callback-plan.md) remains a separate
profile and implementation gate.

The first slice exports fixed scalar floating-point words and explicit FPCSR
access through the runtime's existing scalar service. Ordinary-memory
transforms and checked crypto transactions are later, separate gates. No
machine FP instruction, machine MMIO route, replacement service owner,
arbitrary semantic callback, task handler, suspension or nested service call
is introduced.

## ABI choice and one native owner

Use the existing private ABI family `megapad.hybrid.integer-routine` with
explicit **metadata and manifest version 5**. The family continues to describe
an integer machine profile; its declared semantic service effects are new in
version 5. The initial effect is exactly `scalar_fp_state`, with capability
identity `private_scalar_fp_v1`. This is a metadata/semantic capability, not a
new machine instruction capability.

Preserve private metadata versions 1 through 4, their exact loaders and their
integer export catalogs. Do not add FP names to `integer_leaf`,
`closed_integer_colon` or `closed_integer_nested`. Preserve the distinct
`megapad.hybrid.task-routine` family/version 1. Its task contexts, handler
authority and nonlocal returns are not accepted by a version 5 registration.
An older loader/runtime must reject version 5 explicitly.

Reuse the existing native callback transport version 2: sealed CALL/request/
resume/RET, one parked machine frame, bounded argument/result registers and
one-shot continuation tokens. No new interpreter or native service state is
needed. Proposed shared values are `ServiceExportV5`, `CallbackSiteV5`,
`RoutineImageV5`, `RoutineDeclarationV5`, `RoutineManifestV5` and a version 5
result adapter. Native specs retain their transport-2 shape. Metadata values
carry no Word, callable, service pointer or resume authority.

Use one native owner for CPU state, pinned mappings, control storage,
publication identities and aggregate registration limits. When the nested
transport owner is available, route version 5 through its owner-bound legacy
V2 facade, just as versions 2/3 use that facade. Otherwise an already qualified
V2 owner suffices when no newer native capability is requested. Do not create
another runner/CPU/pin to add service exports.

Mixed registered metadata versions remain valid in one session for sequential
idle-boundary entry. One registration table, control allocator, export owner
and dispatch ledger cover all versions. Version 5 cannot enter an existing
version 4 child, become a version 4 child, call another version 5 routine, or
cross into the task family. Reject cross-profile and service child entry
before effects. Nested service calls require a later explicit capability and
gate; the availability of a nested native facade does not imply permission.

While a V5 invocation is parked, other facade entry, publication, revocation,
dictionary mutation and unrelated close/cancellation remain excluded by the
shared owner. Owned cancellation uses its existing V2 token protocol. Owner
close invalidates all facades and releases pins only after owned work has
settled. Transport receipt sequences and result shapes remain independent;
selecting a facade cannot reset an outer budget or double-count a receipt.

## Fixed first catalog and service identity

Capture these 14 canonical installed Words before user customization:

| Names | Input cells | Output cells | Permitted service effect |
|---|---:|---:|---|
| `F32+`, `F32-`, `F32*`, `F32/`, `F64+`, `F64-`, `F64*`, `F64/` | 2 | 1 | Read rounding mode; accumulate sticky flags |
| `F32SQRT`, `F64SQRT` | 1 | 1 | Read rounding mode; accumulate sticky flags |
| `F32FMA`, `F64FMA` | 3 | 1 | Read rounding mode; accumulate sticky flags |
| `FPCSR@` | 0 | 1 | Read FPCSR |
| `FPCSR!` | 1 | 0 | Write the existing masked FPCSR |

Every cell is an exact uint64 value; float operands/results are raw bit
patterns. Each export costs one semantic primitive tick, transfers no guest
buffer bytes, uses no callback return-stack cells, and cannot suspend.
There is no arbitrary operation-byte parameter. Conversions, comparisons,
classification, tile words and architectural FLAGS are outside this catalog.

The existing owner is `runtime.scalar_float`, an exact
`HostedScalarFloatService`. Retain its FPCSR across machine entry, callback
return, later callbacks and ordinary semantic calls. Never create a temporary
service or merge architectural FP state afterward. A later machine failure
does not undo a completed FPCSR write or sticky flag.

At publication, bind the exact Word, implementation, callback closure,
canonical shape/opcode, service identity, service methods and selected value
executor. The closures in `simulator/core_words.py::_scalar_float_word`
already capture the service; require that captured owner to remain the
runtime's selected owner. Capture an independent numerical descriptor
alongside strong identities. Names, copied handles, XT reuse or shadowing do
not retarget the binding.

Allow only the canonical Python value path or the exact shared native value
kernel admitted for that semantic runtime. A custom service, replacement
validator, Python callback in `_native_execute`, or changed helper is not an
equivalent implementation merely because it returns matching values. Do not
import the emulator's value executor to override a Python-selected semantic
runtime. Native and Python value executors must have the same raw-bit oracle.

Revalidate owner, implementation, relevant routes and executor before each
callback and again after its charged tick, immediately before invocation.
Accounting hooks can change identities during the tick. Revalidate before
native resume too, without rolling back completed service effects. No
qualification cache may let a later replacement escape these checks.

## Exact operation and invalid-rounding-mode order

The semantic engine owns a finite private callback context; the caller's
semantic stack, return cookies and HANDLER cell are not its storage. Stage the
request's exact argument tuple there after validating signature, private
capacity, current bindings and remaining budgets. No callback output becomes
a machine result until successful completion and output validation.

Preserve the canonical operation sequence:

1. The actual machine CALL and its completed instruction/cycle receipt occur
   before semantic dispatch. An exhausted allowance can prevent the callback
   while retaining those completed CALL effects.
2. Charge one semantic primitive tick on the original dispatch meter. If the
   tick fails, no callback operand has been popped and no FP effect occurred.
   Recheck admission after a successful tick.
3. Invoke the canonical closure. A binary operation pops right then left; a
   unary operation pops its input; FMA pops addend, right, left and passes
   `(Rd=addend, Rs=left, Rt=right)` to the service. Do not replace these pops
   with an earlier peek-based fault check.
4. `HostedScalarFloatService.operate` validates the fixed opcode and current
   FPCSR before invoking a value kernel or changing FPCSR. For the admitted
   arithmetic, reserved dynamic rounding modes 5, 6 and 7 fail here, after all
   operation inputs were consumed from the private stack.
5. On success, compute the exact value/flags, OR the already shifted sticky
   flags into FPCSR, then push the value. Preserve the Python/native oracle's
   NaNs, signed zeros, subnormals and format bit patterns. IEEE exceptional
   values such as division by zero or a negative square root produce their
   specified result/flags; they are not integer DivideByZeroFault events.

`FPCSR!` pops first and writes `u64(value) & 0x1F7`. This intentionally permits
reserved rounding-mode bit patterns; the write itself succeeds. `FPCSR@`
reads the current value and then pushes it. Neither operation implicitly
clears flags or restores an entry snapshot.

For an invalid-RM arithmetic request, the charged tick remains visible, the
private inputs are consumed, no private result is pushed, no value kernel
runs and FPCSR is unchanged from immediately before that operation. Earlier
successful service effects and machine stores remain visible. The outer
semantic input cells retain the private profile's transactional failure rule.

## Private fault settlement and host provenance

This slice requires the isolated-failure portion of Phase 5C. It does not
require or enable shared-task CATCH/THROW, FAULT-XT dispatch or suspension.
Ordinary semantic execution outside this profile keeps its current
instruction-fault behavior, including the existing handler/report/ABORT path.

The private engine must distinguish an admitted operation's validation fault
from a host hook throwing a similarly named exception. Define an engine-issued
`ServiceCallbackFailureV5` receipt for the exact active binding, invocation,
request and validation boundary. It records the canonical operation, fault
kind `illegal_scalar_float`, existing throw code `-21`, consumed private input
count, FPCSR at the validation boundary and completed semantic work. It retains
the original `IllegalScalarFloatError` as the cause. An exception class,
matching message or copied receipt is not issuance evidence.

Establish this evidence only around the admitted service validation, after
the tick and canonical operand pops. Do not wrap `meter.tick`, export
admission, accounting hooks, marshalling, output validation or the value
kernel in a blanket `except InstructionFault`. A raw InstructionFault from
an accounting hook is that exact host exception, not a guest service fault.
Unexpected kernel errors, MemoryError, interruption, ForthAbort and
StepBudgetExceeded also retain their existing host-escape provenance.

For an issued service failure, composition consumes its exact pending receipt,
cancels the pending native invocation once, settles all completed work and
raises `HybridExecutionError(reason="service_fault")` with the original
exception as its cause and immutable machine/site/export diagnostics. No
callback outputs are published and no machine RET is fabricated. A stale
binding, invalid output or exhausted allowance keeps its distinct typed
reason; it is not relabeled as a scalar arithmetic fault.

For a raw host exception, cancel/settle owned state and re-raise the original
object/type. Cleanup failure annotates the original error and disables further
interop entry until close; it must not replace the original cause or fabricate
a successful result. Catch BaseException only for this owned cleanup.

The private fault path never reads or invokes the caller's FAULT-XT, installs
or swaps HANDLER, emits a guest fault report, executes THROW, or clears the
caller as ABORT. A later task service profile must explicitly authorize the
captured handler and original task continuation under the task ABI. Enabling
these operations under version 5 cannot grant that authority accidentally.

## Bounds, manifest and startup publication

Expose `load_manifest_v5(path)` plus generic bounded version dispatch. Use one
bounded read, reject duplicate/unknown JSON fields at every level, exact-int
validation excluding booleans, and the existing regular-local-image/path
rules. Loading remains backend-neutral and performs no publication/execution.

The exact top-level fields are `abi`, `version`, `dispatch_instruction_limit`,
`dispatch_callback_limit`, `dispatch_callback_semantic_limit`, `exports` and
`routines`. They use the existing bounds: manifest at most 1 MiB; at most 64
exports/routines; dispatch limits 1..10,000,000 instructions, 1..1024 callback
requests and 1..65,536 callback semantic ticks. No policies, source prelude or
host callable field is accepted in this first V5 grammar.

Each export has exactly `export_id`, `name`, `input_cells`, `output_cells`,
`effect` and `max_semantic_steps`. IDs are unique exact integers 0..63. Names
must be exact uppercase entries in the 14-word table; arities must agree;
`effect` must be `scalar_fp_state`; `max_semantic_steps` must be 1. Distinct
IDs may bind the same canonical operation. Zero buffer bytes and no-suspension
are fixed descriptor properties, not configurable JSON allowances.

Each routine retains the V2 fields `name`, `image`, `entry_offset`,
`input_cells`, `output_cells`, `buffers`, `max_instructions`,
`return_stack_cells`, `callbacks`, plus required `max_callback_requests` in
0..1024. Routine buffers still grant only the machine's ordinary accesses;
they grant no FP callback memory access. Preserve limits of eight argument/
result cells, 16 buffer rules, 16 callback sites, 1,000,000 instructions per
invocation, 8192 private return cells, 1 MiB padded code per routine, 16 MiB
aggregate padded code and the shared 4 MiB control arena.

Callbacks retain exact `{call_offset, stub_offset, export_id}` metadata and
the existing real CALL/RET proof. Resolve each export once; validate all
independent names, signatures, limits, paths, references and site-overlap
metadata before image reads. Then validate actual image geometry/encodings.
The callback invocation limit passed to transport 2 is the minimum of that
routine's declared limit and the outer ledger's remaining allowance. It is
not replenished at a callback or resume. Zero admits only paths that request
no callback; reaching a callback retains its completed CALL and fails under
the existing exhausted-limit semantics.

At startup, preflight the exact V5 semantic capability, selected value
executor and native transport before exposing an owner. In one fresh,
unexposed session, bind exports transactionally, publish sealed machine Words
and only then evaluate existing boot source. No routine executes during
publication. Failure rolls back only newly issued exports/publications and
closes the unexposed owner; pre-existing host-programmatic registrations keep
their exact identities. Use ordinary dictionary/index/lease transactions.

Host-programmatic registration uses `register_routine_v5` with immutable
inputs and the same admission. A manifest, copied descriptor or status record
does not create authority. Missing capabilities fail explicitly; there is no
silent fallback to an integer export or unrestricted semantic execution.

Status/reporting separates metadata version 5, native transport version 2,
effect `scalar_fp_state`, private callback execution, actual Python/native
value executor, active/max parked depth one and suspension unsupported. The
native semantic selection alone must not imply native callback dispatch.

## Qualification and measurement gates

1. Lock shared values and strict loader bounds first. Cover version confusion,
   malformed exact types, forged descriptors/receipts, unknown operations,
   invalid arities, buffer/effect escalation and validation-before-image-read.
2. Qualify all 14 entries against direct service/semantic raw-bit results for
   both selected semantic backends. Extend
   `tests/simulator/test_scalar_float_words.py`,
   `tests/test_native_scalar_fp.py`,
   `tests/simulator/test_native_scalar_execution.py` and
   `tests/test_fp_scalar.py` evidence: every valid rounding mode, reserved
   modes, sticky flags, NaNs, infinities, signed zero, subnormals and FMA.
3. Lock invalid-RM tick/pop/FPCSR ordering, earlier successful effects,
   original caller arguments, private stack/cookie cleanup and cancellation.
   Install a fault handler with an observable side effect and prove V5 never
   invokes it. Inject same-class failures from tick/accounting hooks and
   output/cleanup paths to prove exact raw-host versus issued-fault provenance.
4. Test pre-use and late replacement of Word, service, helpers, native value
   executor and semantic owner, including mutation during the tick. Cover
   mixed-version sequential use, same-owner legacy facades, exhausted limits,
   wrong/replayed tokens, cross-profile child rejection and close.
5. Run a bounded `SessionServer.dispatch` smoke journey from a strict V5
   manifest: publish the declared FP/FPCSR sites, execute a machine routine,
   verify exact result bits and FPCSR, inspect profile/work counters, and
   close. Repeat under Python/native semantic selection. This establishes an
   application protocol journey, not a Desktop GUI or external-project test.
6. Record a bounded paired report before claiming useful speed. Compare
   direct semantic FP against machine-to-service-to-machine work on identical
   inputs/owners. Include a callback-free machine control and existing
   integer callback transport controls; report their differing work rather
   than subtracting incomparable timings. Reuse the four-operation FP64
   workload/oracles in `bench_runtime_hotspots.py` where appropriate, with
   explicit callback accounting. Start with 64 and 256 iterations and finite
   instruction/callback/semantic ceilings.

Record metadata/profile/executors, exact output bits/FPCSR, semantic ticks,
machine segments/instructions/cycles, callback count, maximum parked depth,
exit reason, source/native artifact hashes and current-host baseline rechecks.
Separate unprofiled latency from attribution and setup. Existing FP kernel
reports are direct-service controls, not expected hybrid gains. A functional
service capability may qualify even when crossing costs make it slower than
direct semantic execution. Record that result without claiming acceleration
or changing defaults; retain optimization machinery only when its independent
current-host comparison demonstrates a benefit.

## Later service gates: not admitted by version 5

**Bounded MOVE** is a separate proposed memory-effect capability: source read
and destination write grants derived from original arguments, capped initially
at 4096 bytes and intersected with the parent's ordinary grants. Preserve
zero-length/identity no-access behavior, overlap semantics, full-span
admission, protected-stack/code/control exclusions and completed writes after
an operation begins. No unrestricted semantic-memory or MMIO fallback is
permitted. Use the existing memory owner and extend
`test_bios_memory_words.py`/`test_dense_memory.py` with grant narrowing and
canaries. Do not add a generic buffer rule to V5 before that contract qualifies.

**Checked KECCAK-F1600** is a separate proposed crypto capability: exactly one
200-byte read/write grant and the existing `runtime.sha3` owner. Preserve
`keccak_f1600_checked` ordering: capability, complete span, busy-owner check,
acquire, device-state check, 25 little-endian input lanes, one permutation,
staged output, successful hardware clear, publication, release. Enforce the
grant at its existing span-check point so unsupported/status priority is not
changed. Operation failure leaves the destination untouched; clear failure
retains ownership. Admit only required internal service register accesses,
never a guest MMIO bus. Use the published zero-state/Python permutation
oracles and existing checked/native differential tests. Longer SHA3/SHAKE
transactions need separately bounded byte counts and lease lifetimes.

Audio requires its own sink/session and generation/playing-state contract:
submission can publish a capture before a fallible host callback, and failure
is not proof that playback stopped. AES transfer optimization was separately
rejected after current-host measurement; this plan does not reopen that work.
Task scheduling, storage mutation, terminal input, arbitrary services and
cross-profile/nested service calls remain later capabilities. All work and
evidence here are MegaPad-local.
