# Hybrid synchronous integer callback ABI values

Date: 2026-09-30

Status: Phase 5A value contract, semantic export engine, v2 manifest loader and
native request/resume runner are qualified. Composition and launcher admission
remain separate implementation gates.
The locked implementation direction is
[`hybrid-interop-plan.md`](hybrid-interop-plan.md).

ABI identity remains `megapad.hybrid.integer-routine`. These values require
version **2**, exposed as `HYBRID_CALLBACK_ABI_VERSION`. The existing
`HYBRID_ABI_VERSION` remains **1**. The v1 manifest, declaration, runner, and
`MachineExitKindV1` contract remain unchanged.

## Ownership and validation boundary

`shared/hybrid_abi.py` imports no execution backend. Its new values are frozen,
slotted, keyword-only dataclasses. Every numerical field requires an exact
Python `int`; booleans, floats and integer subclasses are rejected. Cell and
address values are uint64. Collections require exact immutable tuples, and
nested values require their exact declared classes.

These are copyable observations and declarations, not callable authority:

- An export name or ID does not bind a live word. The semantic owner must
  independently retain the exact original canonical `Word` identity and
  reject same-named replacements, forged handles, rollback and XT reuse.
- A site records numerical offsets and an export descriptor. Its constructor
  does not decode MP64 instructions or establish a valid control-flow path.
- A request's invocation number and sequence are diagnostic identity fields.
  Copying or reconstructing them must never grant permission to resume.
- Existing sealed declaration lease/nonces remain opaque identities. The
  composition owner must prove their liveness; construction proves only
  numerical and type constraints.

The native runner owns a separate single-use continuation token, bound
to the exact owner, invocation, publication, request and private return slot.
It is deliberately absent from these shared values. No value accepts a
semantic `Word`, execution XT or Python callable in place of an export.

The native `RoutineRunnerV2` executes the real declared `CALL.L`, parks with
its effects and accounting intact, then executes the real `RET.L` after an
admitted reply. It reserves the CPU across the parked interval without holding
an execution lock or the GIL while the host dispatches the callback. Public CPU
mutation and other execution entries reject that reservation, including
conversion and buffer-export reentry. Begin and resume prove the exact sealed
publication, instruction boundaries, resident instruction-cache bytes and
live private control bytes. Cancellation releases the reservation; it does not
undo completed machine effects.

Native remaining callback allowance may be zero: callback-free execution can
finish, while the first actual declared call returns `callback_limit` with its
completed call effects and no continuation token. A request on the final
instruction can likewise have no machine allowance left. A valid reply then
consumes the token and returns `instruction_limit` before publishing outputs
or executing the return. The composition owner must avoid dispatching semantic
work for such an exhausted request.

The September 30 native gate passed 246 checks: 61 callback-runner cases, 79 v1
routine cases and 106 private/worker/coordinator execution, scalar FP and TACC
regressions. Callback cases compare ordinary MP64 state and cover repeated
requests, bounds, token ownership/replay, stale code/control/cache evidence,
CPU mutation exclusion, cancellation and publication rollback. The v1 run body
is unchanged. This gate does not qualify the still-separate semantic bridge.

## Bounds

The existing code, buffer, arity, private-stack and machine-instruction bounds
are reused without widening them.

| Constant | Value | Meaning |
|---|---:|---|
| `MAX_CALLBACK_EXPORTS` | 64 | Session-local export ID space, `0..63` |
| `MAX_CALLBACK_SITES` | 16 | Sites in one routine |
| `MAX_DISPATCH_CALLBACKS` | 1,024 | Requests charged to one outer dispatch |
| `MAX_CALLBACK_SEMANTIC_STEPS` | 4,096 | Locked plan ceiling for a later admitted callback profile; **not** the cost allowed by this leaf catalog |
| `MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS` | 65,536 | Cumulative callback semantic-work ceiling |
| Existing signature limit | 8 | Maximum input/output cell count |
| Existing code limit | 1 MiB | Raw or sealed routine code size |
| Existing call instruction limit | 1,000,000 | Invocation total across all native segments |

The current six exports each require exactly **one** semantic step. Higher
per-callback work is not admitted merely because the later-profile ceiling
constant exists. Root limits may be reduced; nested or resumed calls may not
replace or renew the original root allowance. This value layer records limits
but does not enforce a running ledger.

## `CallbackExportV2`

| Field | Type and constraint |
|---|---|
| `export_id` | Exact integer `0..63`; meaningful only within its owning registry |
| `name` | Exact uppercase string from the six-name catalog below |
| `input_cells` | Exact integer matching the catalog |
| `output_cells` | Exact integer matching the catalog |
| `max_semantic_steps` | Exact integer **1**, default 1 |
| `effect` | Exact string **`integer_leaf`**, default `integer_leaf` |
| `abi`, `version` | Exact ABI string and exact version 2 |

| Name | Stack signature |
|---|---|
| `MIN`, `MAX` | `( x y -- z )`, using the existing signed-cell semantics |
| `ABS` | `( x -- y )`, retaining the existing uint64 wrap for the signed minimum |
| `AND`, `OR`, `XOR` | `( x y -- z )`, using existing cell bitwise semantics |

The catalog is deliberately case-sensitive metadata, not a new dictionary
lookup policy. A host resolves and validates export bindings before invoking
them. All names remain subject to the 1..127 printable nonwhitespace ASCII
name rule. Unknown names, incorrect arities, another effect class or a larger
semantic-step allowance fail construction.

The one-step bound follows the existing source: `simulator/core_words.py`
installs these six total `PrimitiveDefinition` callbacks, and
`simulator/runtime.py::_execute_top` charges one `meter.tick()`, invokes the
primitive, and returns when it returns `None`. These callbacks do not perform
nested dispatch, return `Invoke`, touch a hosted service, or raise an
instruction fault for a prequalified argument tuple. The engine's later
admission gate must prove it bound those original words, not merely matching
descriptor strings.

## `CallbackSiteV2`

| Field | Type and constraint |
|---|---|
| `call_offset` | Exact integer `0..MAX_CODE_BYTES-2` |
| `stub_offset` | Exact integer `0..MAX_CODE_BYTES-1` |
| `export` | Exact `CallbackExportV2` value |
| `abi`, `version` | Exact ABI string and exact version 2 |

The first native profile admits the canonical **unprefixed two-byte `CALL.L`**
site and a **one-byte `RET.L`** stub. Their own byte spans must be disjoint.
The lengths match `emulator/accel/cpu/mp64/decode_impl.h` cases `0xD` and `0xE`.
Prefix-bearing callback sites are outside this profile.

Routine-level validation additionally requires both complete spans to fit
the actual image, not merely the maximum image capacity. No two sites may
share or overlap a call byte, operand byte, or stub byte. In particular, two
calls cannot share one callback stub in this first profile. Multiple sites
may reference equal export descriptors. Reusing an export ID with different
descriptor contents is rejected.

The native publisher still proves the exact encodings and instruction
boundaries, seals the image, and qualifies dynamic arrival from the declared
call. A branch, fallthrough, altered target or undeclared call to a stub is
not callback authority. A numerically valid site whose bytes are NOPs may
pass shared value construction and must fail native publication.

For this v2 profile, the complete sealed image must linearly decode through
the existing common decoder into admitted integer instructions, including
its NOP padding. Embedded undecodable data and deferred extension operations
are rejected. Entry, call and stub offsets must be marked instruction
boundaries; every runtime branch, return and fetch target must also be a
marked boundary. A jump into an immediate operand is rejected even if its
bytes could decode in isolation. The native owner retains the exact published
spec identity and code seal and checks both before begin and resume. These
are v2 admission rules; v1 behavior is unchanged.

## Routine image and sealed declaration

`RoutineImageV2` has the existing `RoutineImageV1` fields with default version
2, plus required `callbacks: tuple[CallbackSiteV2, ...]`. It retains the exact
name, immutable code, entry-offset, signature, `BufferRuleV1` tuple, per-call
instruction and return-stack bounds. The callback tuple has `0..16` entries;
an empty table grants no callback capability.

`RoutineDeclarationV2` has the existing `RoutineDeclarationV1` fields with
default version 2 and the following additions:

| Field | Constraint |
|---|---|
| `callbacks` | Required exact tuple; same image-relative validation as above |
| `dispatch_callback_limit` | Exact integer `1..1024`, default 1024 |
| `dispatch_callback_semantic_limit` | Exact integer `1..65536`, default 65536 |

Sealed code remains 16-byte aligned and padded, lies entirely inside its
complete body allocation, and is disjoint from the cell-aligned private
machine stack. Existing opaque owner/registration/allocation/control identities
and positive generation values are retained. The two dispatch limits describe
the shared root ledger; they do not authorize each callback to obtain a fresh
allowance.

Export-ID consistency is checked within each routine value. The owner must
also reject conflicts across different routine publications in the same
session and enforce its 64-export bound. This module has no global registry.

The v2 Python image/declaration classes reuse v1 field definitions and common
validation, but a v1 API that requires exact v1 values must continue to reject
them. In particular, `RoutineManifestV1` does not accept `RoutineImageV2`.
The later loader slice adds `RoutineManifestV2` and single-read version
selection; see [hybrid-callback-manifest.md](hybrid-callback-manifest.md).

## Qualified semantic export engine

`MegaForthRuntime.bind_callback_export(descriptor)` returns an opaque issued
handle. `verify_callback_export(handle)` returns a fresh metadata copy, and
`invoke_callback_export(handle, arguments)` returns an exact output tuple and
the charged semantic-step count. The engine captures the six original core
Words at installation, before user shadowing. Removal, XT reuse or changed
implementations revoke their eligibility; a copied handle grants no authority.

Invocation uses the ordinary semantic dispatcher and outer meter. Its private
128-byte memory bounds both data and return stacks to eight cells. The approved
dispatch guard is pinned before accounting hooks run, and rechecks the Word,
implementation, owner and private backing immediately before invocation. It
does not enter an installed guest fault callback. The caller's stack bytes and
retained return metadata are not copied or restored. Native-selected semantic
execution can use its reference path for this separate private context.

The final engine gate passed 74 checks across both selected semantic backends.
The existing hybrid bridge and registration-failure gates also passed 99
checks. This engine does not yet connect an actual machine callback or enable
v2 manifests in the application. The separate loader gate passed 98 new checks
alongside 94 existing v1 manifest and 90 callback-value checks.

## `CallbackRequestV2`

The native `begin_v2` callback limit represents remaining allowance and may be
zero. A callback-free path can still return normally. If the routine executes
a declared call with no remaining callback allowance, its real CALL effects
and work are retained, then it exits `callback_limit` without issuing a request
or token. Configured session and shared-value callback limits remain positive.

| Field | Type and constraint |
|---|---|
| `invocation_id` | Exact integer `1..2^64-1`, diagnostic native-owner invocation identity |
| `sequence` | Exact integer `1..1024`, increasing request ordinal within that invocation |
| `site` | Exact `CallbackSiteV2` value |
| `arguments` | Exact tuple of uint64 cells, length exactly `site.export.input_cells` |
| `abi`, `version` | Exact ABI string and exact version 2 |

The tuple observes the declared argument registers in the same order as the
export signature. The owner must match the site/export against the sealed
registration and prove its liveness before dispatch. Matching integers and
matching descriptor equality alone are insufficient.

Construction neither consumes a native token nor spends a callback allowance.
It cannot establish sequence monotonicity relative to earlier requests.
Those are stateful owner obligations.

## `MachineSegmentResultV2`

This result retains the diagnostic fields of `MachineRoutineResultV1`, with
version 2 and the following explicit interpretation and additions:

| Field | Meaning |
|---|---|
| `instructions`, `cycles` | Completed machine work in this segment only |
| `invocation_id` | Exact positive uint64 identity of the native invocation |
| `invocation_instructions` | Completed instructions across this invocation's segments, `0..1,000,000` |
| `invocation_cycles` | Completed cycles across those segments, uint64 |
| `callback` | Exact `CallbackRequestV2` only for a callback-request exit; otherwise `None` |
| `entry_pc`, `pc`, `instruction_pc` | Existing entry/current/instruction-address diagnostics |
| `outputs` | Exact tuple of at most eight uint64 cells, permitted only on normal root return |
| Access/trap/detail fields | Existing typed access diagnostics, trap ID and text detail |

`MachineExitKindV2` contains the existing seven v1 outcomes and three additions:

- `callback_request`: nonterminal; the invocation is parked at its declared
  stub before the actual `RET.L` executes.
- `callback_limit`: a callback allowance prevented further callback work.
- `invalid_callback`: callback control provenance, binding or state is invalid.

`returned` requires at least one completed instruction in this segment: the
actual root `RET.L`. A callback-request result also requires positive segment
instruction work: the real `CALL.L` completed. Its payload invocation ID must
match the result, and its request ordinal cannot exceed total completed
invocation instructions. All callback and failure outcomes have empty final
`outputs`; all outcomes except `callback_request` have no callback payload.

Segment counters may not exceed invocation totals. A segment with zero
completed instructions has zero completed cycles. Each positive completed
instruction contributes at least one cycle. The prior prefix, computed as
invocation totals minus segment deltas, obeys the same constraints. These
values count completed instructions, not a partially executed failed access
or a diagnostic fetch effect. Consecutive-result monotonicity, exact deltas,
and the actual configured allowance still require the native owner to check
its retained state; one immutable result cannot prove history.

An instruction-limit or cancellation result after a parked callback may have
zero segment deltas and nonzero invocation totals. That retains the executed
prefix without charging it again. A valid final-budget `CALL.L` may produce
`callback_request` with no machine allowance left. The composition layer must
then cancel without executing semantic work. If a native caller supplies a
valid reply directly, resume consumes its token and returns
`instruction_limit` with zero deltas **before** applying outputs or executing
the stub's `RET.L`. Invalid/replayed replies remain rejected before mutation.

A declared `CALL.L` with the wrong target fails as `invalid_callback` after
the ordinary completed call effects: its push, register/PC effects and work
remain observable. A failed private-stack store has the existing failed-call
partial effects and creates no successful callback request.

## Required qualification before execution is advertised

The pure value gate checks exact types and bounds, immutable nesting, v1/v2
separation, site collisions, export-ID conflicts, arities, variant/counter
coherence, and a fresh-process import with every backend blocked.

That gate proves no native opcode, callable identity, continuation authority,
effect ordering, performance, or application integration. The next native
and semantic slices must separately prove canonical word identity, sealed
call provenance, one-shot token ownership, memory and stack protection,
real CALL/RET accounting, cumulative work limits, failure/close behavior and
unchanged v1 execution before exposing a callback capability.
