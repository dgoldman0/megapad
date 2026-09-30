# Nested callback manifests, version 4

Version 4 lets a declared closed integer policy call another registered machine
routine. The unified hybrid application loads this profile with
`--hybrid-routines MANIFEST`. Both `hybrid.manifest.load_manifest(path)` and
`hybrid.nested_manifest.load_nested_manifest_v4(path)` produce immutable
`RoutineManifestV4` metadata without loading an execution backend or granting
execution authority.

Startup requires the complete semantic profile 4 and native transport 3,
including an empty version 4 manifest. Missing support fails before dictionary
publication, boot source execution or session creation. Versions 1 through 3
retain their schemas and explicit loaders.

## Document shape

Version 4 uses the top-level fields from
[version 3](hybrid-closed-callback-manifest.md): `abi`, `version`,
`dispatch_instruction_limit`, `dispatch_callback_limit`,
`dispatch_callback_semantic_limit`, `policies`, `exports` and `routines`.
The ABI is `megapad.hybrid.integer-routine` and the version is exactly `4`.
Unknown fields, duplicate keys, booleans where integers are required, and
nonfinite numbers are rejected. Images remain bounded, regular local files
relative to the manifest.

The version 4 additions are:

| Object | Field or operation | Meaning |
|---|---|---|
| Routine | `routine_id` | Unique exact integer in `0..63`, distinct from its name |
| Routine | `max_callback_requests` | Per-invocation callback ceiling in `0..1024` |
| Policy operation | `{"op":"call_machine","routine_id":N}` | Static call to the declared routine with that ID |
| Policy export | `"effect":"closed_integer_nested"` | Closed policy whose captured graph includes a machine call |

All other routine fields remain required: `name`, `image`, `entry_offset`,
`input_cells`, `output_cells`, `buffers`, `max_instructions`,
`return_stack_cells`, and `callbacks`. A callback row still contains
`call_offset`, `stub_offset`, and `export_id`. A policy still declares its
`policy_id`, name, input/output cell counts and operations. Every invocation
and callback has at most eight input/output cells.

An `integer_leaf` export has `export_id`, `name`, `input_cells`, `output_cells`,
`effect`, and `max_semantic_steps`. `MIN`, `MAX`, `ABS`, `AND`, `OR`, and `XOR`
remain the admitted direct leaf names, with one semantic step each.

A policy export has `export_id`, `effect`, `policy_id`, and
`max_semantic_steps`. Its name and signature come from the referenced policy.
Its effect is `closed_integer_colon` or `closed_integer_nested`, according to
the proved graph. The local semantic allowance is in `1..4096`.

## Graph and execution limits

The loader proves the combined machine/policy graph before opening any image.
It checks signatures, stack bounds, declared callback work, forward branches,
and an acyclic graph with at most eight active machine registrations. Table
order need not be dependency order. Startup installs children and policies in
the proved order before their parents, then performs the existing image boot.
A failed installation closes that fresh startup owner.

Each machine child uses the same native CPU owner and memory backing. Its
buffer permissions must be contained in one immediate-parent grant, with no
broader access. Code, native control storage and protected semantic regions
remain inaccessible through those grants. The parent resumes only after its
saved integer state and protected code/control have been validated.

Machine instructions, cycles, entries, segments and callback requests count
actual completed work once at the root. Semantic callback work also counts
against every active ancestor callback allowance. A child never creates a new
dispatch budget. Callback policies run on private bounded stacks through the
Python reference dispatcher even when the outer semantic executor is native.

Status reports the selected manifest version, actual registered versions,
native transport version, callback profiles and observed maximum machine
depth. `nested_callback_abi_available` describes complete support;
`nested_machine_callbacks` describes the selected execution profile.

This private profile has no arbitrary source callbacks, dynamic execution,
loops, recursive registrations, task-stack `CATCH`/`THROW`, suspension, MMIO or
multicore machine execution. Shared-task exceptions and suspension have a
separate [task contract](hybrid-task-callback-plan.md). The complete private
nesting contract is in [hybrid-nested-callback-plan.md](hybrid-nested-callback-plan.md).
