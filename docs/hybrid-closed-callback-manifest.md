# Closed callback manifests, version 3

Version 3 adds finite declarative integer policies to the hybrid manifest.
`load_manifest_v3(path)` and the generic `load_manifest(path)` return an
immutable `RoutineManifestV3`. Loading validates declarations and reads bounded
machine images; it creates no runtime, publishes no words, evaluates no source,
and imports no execution backend.

The admission rules are locked in
[hybrid-closed-callback-plan.md](hybrid-closed-callback-plan.md). Versions 1 and
2 retain their exact schemas and explicit loaders. Version 3 is a metadata
version: machine callbacks continue to use the version 2 CALL/request/resume/RET
transport. Metadata never grants an export handle or native continuation token.

## Exact document shape

Every listed field is required. Unknown fields and duplicate JSON keys are
errors at every level. Integers must be JSON integers; booleans, floating-point
values, NaN, and Infinity are rejected.

| Top-level field | Constraint |
|---|---|
| `abi` | `"megapad.hybrid.integer-routine"` |
| `version` | `3` |
| `dispatch_instruction_limit` | `1..10000000` |
| `dispatch_callback_limit` | `1..1024` |
| `dispatch_callback_semantic_limit` | `1..65536` |
| `policies` | Array of `0..64` policy declarations |
| `exports` | Array of `0..64` export descriptors |
| `routines` | Array of `0..64` machine routine declarations |

The three dispatch limits remain cumulative outer-dispatch allowances. A
callback or native resume does not reset them.

Each policy has exactly these fields:

| Field | Constraint |
|---|---|
| `policy_id` | Exact integer `0..63`, unique within the policy table |
| `name` | `1..127` printable nonwhitespace ASCII bytes |
| `input_cells`, `output_cells` | Exact integers `0..8` |
| `operations` | Array of operations from the grammar below |

Policy and routine names must be unique under ASCII case folding. Policy IDs
and export IDs have separate namespaces. The entire policy table contains at
most 4096 operations, including unreachable suffixes. Policies may refer to
later table entries; array order is preserved in the loaded value, while the
shared proof supplies dependency order for installation.

Only these exact operation objects are admitted:

| JSON object | Meaning |
|---|---|
| `{"op":"literal","value":N}` | Push an exact uint64 cell, `0..18446744073709551615` |
| `{"op":"call_core","name":"ROT"}` | Static call to an originally captured canonical core Word |
| `{"op":"call_policy","policy_id":N}` | Static call to another policy in this table |
| `{"op":"branch","target":N}` | Unconditional forward branch to a local operation index |
| `{"op":"branch_zero","target":N}` | Pop a cell and branch forward if it is zero |
| `{"op":"return"}` | Return from the current policy |

A branch target must be a valid index strictly greater than the branch's own
index. Each manifest operation becomes exactly one semantic IR operation;
normalization does not renumber branch destinations.

`call_core` admits exactly `MIN`, `MAX`, `ABS`, `AND`, `OR`, `XOR`, `DUP`,
`DROP`, `SWAP`, `OVER`, and `ROT`, in uppercase. The five stack operations are
internal policy dependencies; they do not become direct leaf exports.
`MIN`, `MAX`, and `ABS` retain signed-cell semantics. Dynamic XTs, loops,
recursion, source text, includes, compiler words, services, memory operations,
and machine routine calls have no representation in this grammar.

## Export variants

Version 3 requires an explicit `effect` and local semantic allowance. An
`integer_leaf` export has exactly:

```json
{
  "export_id": 1,
  "effect": "integer_leaf",
  "name": "MIN",
  "input_cells": 2,
  "output_cells": 1,
  "max_semantic_steps": 1
}
```

Only the six version 2 leaf signatures are admitted: `ABS` takes one cell;
`MIN`, `MAX`, `AND`, `OR`, and `XOR` take two. Each returns one cell and has
`max_semantic_steps` exactly `1`. Version 2 retains its previous four-field
export schema and does not accept these extra fields.

A `closed_integer_colon` export has exactly:

```json
{
  "export_id": 0,
  "effect": "closed_integer_colon",
  "policy_id": 7,
  "max_semantic_steps": 7
}
```

`export_id` is unique across both export variants and lies in `0..63`.
`policy_id` must identify a policy in this document; a name expected to appear
in later boot source is insufficient. The allowance is an exact integer in
`1..4096`. The loader derives the normalized `CallbackExportV3` name and
signature from that policy. The closed JSON variant cannot contain another
copy of the name or signature. Distinct export IDs may reference the same
policy with independently valid local allowances.

## Proof before image reads

The loader uses the shared, backend-independent policy analysis before opening
any machine image. It validates every operation, including unreachable
suffixes, and rejects missing policy references and cycles anywhere in the
table. Every reachable path must finish with Return. Branch alternatives must
join at equal data depths and produce the declared output depth.

The proof bounds all intermediate data depths and active colon continuations
within the separate eight-cell data and return stacks. The root continuation
counts as one return cell. It also bounds captured dependencies to 64 Words
and derives minimum and maximum semantic work without expanding repeated call
trees. No path may need more than 4096 ticks or more than its export's declared
allowance.

Work follows ordinary reference dispatch: Literal, Branch, BranchZero, and
Return each cost one tick; a core primitive Call costs two; a policy Call costs
one plus the callee's actual work. `ROT MIN MAX ;` therefore costs seven ticks,
requires three input cells, returns one, and uses one return cell. Both branch
alternatives are proved even when a literal would select only one at execution.
The running engine still charges only actual work against the original outer
meter and callback ledger.

Every independent routine field, relative image path, buffer rule, export
reference, and callback-site overlap also passes before image reads. A bad
later declaration prevents reads of earlier images. After loading, application
startup independently verifies captured core identities, rejects collisions
with existing dictionary words, installs policies in dependency order, and
registers routines before ordinary boot-source compilation. No policy
operation executes during installation. Live Word authority and late mutation
checks belong to that runtime boundary, not to the manifest values.

## Machine images and loading bounds

Routine objects, buffer rules, and callback sites use the same exact JSON
fields as [version 2](hybrid-callback-manifest.md#exact-json-schema). Each site
references an export ID; the loader gives all sites for that ID the same
immutable descriptor object. Call and stub byte spans must be disjoint and
fit the actual image. Native publication still proves instruction boundaries,
canonical CALL.L/RET.L encodings, and sealed code provenance.

The manifest remains a regular UTF-8 file of at most 1 MiB. Generic version
selection reads and decodes it once. Images remain regular local files of at
most 1 MiB each and 16 MiB total after rounding each reservation to 16 bytes.
Image paths are relative to the manifest directory; absolute, drive-qualified,
URI, NUL-containing, and unencodable paths fail. File descriptors are checked
for regular-file type and reads retain hard allocation limits. No failure
returns a partial manifest.

## Complete example

The offsets below illustrate the metadata shape; replace them with labels
from the actual assembled `clamp.bin`. The loader does not assemble or relocate
machine code.

```json
{
  "abi": "megapad.hybrid.integer-routine",
  "version": 3,
  "dispatch_instruction_limit": 1000,
  "dispatch_callback_limit": 32,
  "dispatch_callback_semantic_limit": 256,
  "policies": [
    {
      "policy_id": 7,
      "name": "CLAMP",
      "input_cells": 3,
      "output_cells": 1,
      "operations": [
        {"op": "call_core", "name": "ROT"},
        {"op": "call_core", "name": "MIN"},
        {"op": "call_core", "name": "MAX"},
        {"op": "return"}
      ]
    }
  ],
  "exports": [
    {"export_id": 0, "effect": "closed_integer_colon", "policy_id": 7, "max_semantic_steps": 7}
  ],
  "routines": [
    {
      "name": "H-CLAMP",
      "image": "clamp.bin",
      "entry_offset": 0,
      "input_cells": 3,
      "output_cells": 1,
      "buffers": [],
      "max_instructions": 100,
      "return_stack_cells": 16,
      "callbacks": [
        {"call_offset": 10, "stub_offset": 13, "export_id": 0}
      ]
    }
  ]
}
```
