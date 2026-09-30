# Private scalar-service manifests, version 5

Version 5 admits the original scalar FP and FPCSR words through private
callbacks. Use `--hybrid-routines MANIFEST` with the unified hybrid application.
The generic loader and `hybrid.service_manifest.load_service_manifest_v5`
validate one bounded document and its local images without granting execution
authority. Startup requires qualified scalar-service support and native
transport 2, even for an empty version 5 manifest.

## Exact schema

All listed fields are required. Unknown fields, duplicate keys, booleans in
integer fields, nonfinite numbers, and nonlocal image paths are rejected.

| Top-level field | Constraint |
|---|---|
| `abi` | `"megapad.hybrid.integer-routine"` |
| `version` | Exact integer `5` |
| `dispatch_instruction_limit` | `1..10000000` |
| `dispatch_callback_limit` | `1..1024` |
| `dispatch_callback_semantic_limit` | `1..65536` |
| `exports` | At most 64 service descriptors |
| `routines` | At most 64 bounded integer routines |

A descriptor has exactly `export_id`, `name`, `input_cells`, `output_cells`,
`effect`, and `max_semantic_steps`. Its effect is `scalar_fp_state`, its
semantic allowance is exactly one tick, and its signature must match this
catalog:

| Names | Input cells | Output cells |
|---|---:|---:|
| `F32+`, `F32-`, `F32*`, `F32/` | 2 | 1 |
| `F64+`, `F64-`, `F64*`, `F64/` | 2 | 1 |
| `F32SQRT`, `F64SQRT` | 1 | 1 |
| `F32FMA`, `F64FMA` | 3 | 1 |
| `FPCSR@` | 0 | 1 |
| `FPCSR!` | 1 | 0 |

Floating-point cells contain raw IEEE bits. Binary arguments are left then
right; FMA arguments are left, right, addend. FPCSR belongs to the existing
semantic runtime and retains completed effects across later machine failure.

A routine has `name`, `image`, `entry_offset`, `input_cells`, `output_cells`,
`buffers`, `max_instructions`, `max_callback_requests`, `return_stack_cells`,
and `callbacks`. These follow the bounded integer-routine rules;
`max_callback_requests` is in `0..1024`. Each callback has `call_offset`,
`stub_offset`, and `export_id`. Native publication proves the actual sealed
CALL/RET instruction boundaries. Version 5 declares neither policies nor
machine child calls.

Independent signatures, limits, references and byte-overlap metadata validate
before images are read. Images are bounded regular local files, relative to
the manifest. Startup publishes exports and routines before the existing boot
source executes; a startup failure closes the unexposed owner.

## Execution and status

Each callback uses private eight-cell data and return storage. It charges the
original semantic meter, performs the original operand pops, validates the
current rounding mode, calls the already selected value kernel, updates sticky
flags and returns the result. The callback dispatcher is the Python reference
path under both outer semantic executors. Scalar computation uses the actual
selected Python or shared native kernel.

Reserved rounding modes fail arithmetic after its operand pops and charged
tick. Only exact engine-issued validation evidence becomes a
`HybridExecutionError` with reason `service_fault` and the original cause.
Unexpected host exceptions remain the original objects. Private failures do
not invoke the caller's FAULT handler, execute guest `THROW`, emit a guest fault
report, or clear caller arguments as guest `ABORT`.

Status distinguishes metadata version 5 from native transport 2, the
`private_scalar_fp_v1` capability, `scalar_fp_state` effect, private dispatcher,
selected scalar value executor, and the profile's single parked frame.
Mixed legacy/version-4/version-5 registrations report their actual metadata
and native transport sets separately. Work counters record completed machine
instructions, cycles and segments, callback requests and semantic ticks.

This profile admits no task-stack callbacks, suspension, machine FP opcodes,
MMIO, arbitrary services, child calls or new scalar owner. Ordinary-memory,
crypto and audio service exports require separate effect and grant gates.
See [the service contract](hybrid-service-callback-plan.md) for validation,
ownership and measurement requirements.
