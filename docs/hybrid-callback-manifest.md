# Hybrid callback manifests, version 2

Version 2 describes bounded machine routines that may request one of six
canonical integer leaves. Loading a manifest produces immutable, unpublished
values. It does not create a runtime, bind a dictionary word, publish code,
register a callback, or execute an instruction.

The value contracts are defined in [hybrid-callback-abi.md](hybrid-callback-abi.md).
Version 1 remains the integer-routine format described in
[hybrid-runtime-abi.md](hybrid-runtime-abi.md). This document adds a separate
format; it does not expand version 1 admission.

## Loading and version selection

`hybrid.manifest` provides three entry points:

| Function | Accepted format | Result |
|---|---|---|
| `load_manifest_v1(path)` | Exact version 1 schema | `RoutineManifestV1` |
| `load_manifest_v2(path)` | Exact version 2 schema | `RoutineManifestV2` |
| `load_manifest(path)` | Explicit version 1 or 2 | The corresponding exact value type |

The dispatcher opens and decodes the manifest once, then passes that same
decoded document to the selected validator. It does not reopen the file after
examining `version`. Unknown versions, booleans, floating-point versions, or a
missing version fail. The explicit v1 loader continues to reject version 2 and
callback fields. These APIs import no emulator, simulator, or native extension.

This slice provides the format and loader. Server selection and runtime
registration are separate composition work; loading a v2 file does not opt a
v1 session into callbacks.

## Exact JSON schema

Every field below is required. Unknown fields and duplicate JSON keys are
rejected at every nesting level. Integers must be exact JSON integers;
booleans and floating-point values are rejected. NaN and Infinity are rejected.

The top-level object has exactly these fields:

| Field | Constraint |
|---|---|
| `abi` | `"megapad.hybrid.integer-routine"` |
| `version` | `2` |
| `dispatch_instruction_limit` | Integer `1..10000000` |
| `dispatch_callback_limit` | Integer `1..1024` |
| `dispatch_callback_semantic_limit` | Integer `1..65536` |
| `exports` | Array of `0..64` export descriptors |
| `routines` | Array of `0..64` routine declarations |

The three limits describe cumulative outer-dispatch allowances. They do not
grant a fresh allowance at each callback or native resume. The loader records
these bounds; the execution owner enforces the running ledger and any lower
caller budget.

Each export has exactly `export_id`, `name`, `input_cells`, and `output_cells`:

| Name | Input cells | Output cells |
|---|---:|---:|
| `MIN`, `MAX` | 2 | 1 |
| `ABS` | 1 | 1 |
| `AND`, `OR`, `XOR` | 2 | 1 |

`export_id` is an integer in `0..63`, unique within the manifest even if a
repeated descriptor would be equal. Names are exact uppercase catalog names.
Distinct IDs may describe the same leaf. Each resolved `CallbackExportV2` has
the fixed values `max_semantic_steps=1` and `effect="integer_leaf"`; those are
not configurable JSON fields. A name or ID is metadata, never permission to
call an arbitrary word, XT, service, or Python function.

Each routine has the exact v1 fields plus required `callbacks`:

| Field | Constraint |
|---|---|
| `name` | `1..127` printable nonwhitespace ASCII bytes; unique ignoring ASCII case |
| `image` | Nonempty local path relative to the manifest directory |
| `entry_offset` | Integer `0..1048575`, subsequently checked against actual image length |
| `input_cells`, `output_cells` | Integers `0..8` |
| `buffers` | `0..16` unchanged v1 buffer-rule objects |
| `max_instructions` | Integer `1..1000000` |
| `return_stack_cells` | Integer `1..8192` |
| `callbacks` | Array of `0..16` callback sites |

A buffer rule still has exactly `address_argument`, `length_argument`,
`element_bytes`, `max_bytes`, and `access`. Argument indices must lie within
the routine's input signature; access is `read`, `write`, or `read_write`.
The existing uint64 bounds and nonwrapping buffer-resolution rules apply.

Each callback site has exactly these fields:

| Field | Constraint |
|---|---|
| `call_offset` | Integer `0..1048574`; reserves a two-byte unprefixed `CALL.L` |
| `stub_offset` | Integer `0..1048575`; reserves a one-byte `RET.L` |
| `export_id` | ID of a descriptor in this manifest's `exports` array |

No call byte, operand byte, or stub byte may overlap another declared site in
the same routine. A call cannot overlap its own stub. Shared stubs are not
admitted in this profile. Both complete spans must fit within the actual
unpublished image; future alignment padding cannot make an out-of-image site
valid. An empty callback list grants no callback capability.

The loader constructs each export descriptor once. All sites referring to its
ID hold that exact immutable descriptor object, including sites in different
routines. Direct construction of `RoutineManifestV2` permits equal descriptor
copies but rejects undeclared IDs and conflicting descriptors. Descriptor
equality does not establish the identity of an issued callback handle.

## Validation order and file bounds

The manifest must be a regular local file of at most 1 MiB, decoded as UTF-8.
Before opening any image, the loader validates every independent JSON shape,
name, path, signature, rule, limit, export ID, and callback overlap in the
entire document. Invalid metadata in a later routine therefore prevents reads
of earlier images.

Images are regular local files, each read with a hard 1 MiB bound. Paths are
resolved relative to the manifest directory regardless of the process working
directory. Absolute paths, drive-qualified Windows paths, URI paths, NULs,
and unencodable paths are rejected. Relative parent components retain their
ordinary local-path meaning. The loader checks the opened descriptor's file
type and uses a bounded read, so a FIFO does not wait for a writer and a file
growing after its size check cannot cause an unlimited allocation.

After reads, immutable `RoutineImageV2` values check entry and callback spans
against actual image length. Empty images fail. The aggregate code limit is
16 MiB, including each image's reservation rounded up to 16 bytes. A failure
in any image returns no partial manifest. Returned byte strings do not change
if the backing files are subsequently replaced.

Native publication remains responsible for decoding the complete sealed image,
admitting its instructions, verifying exact `CALL.L`/`RET.L` encodings and
instruction boundaries, and sealing the code. The runtime must independently
bind the original canonical leaf identities and enforce ownership, liveness,
budgets, and callback provenance. Numerically valid metadata alone proves none
of those execution properties.

## Metadata example

This illustrates the complete JSON shape. The offsets must be replaced with
labels from the actual assembled `policy.bin`; the manifest does not assemble
or relocate that file.

```json
{
  "abi": "megapad.hybrid.integer-routine",
  "version": 2,
  "dispatch_instruction_limit": 1000,
  "dispatch_callback_limit": 32,
  "dispatch_callback_semantic_limit": 64,
  "exports": [
    {"export_id": 0, "name": "MIN", "input_cells": 2, "output_cells": 1}
  ],
  "routines": [
    {
      "name": "H-POLICY",
      "image": "policy.bin",
      "entry_offset": 0,
      "input_cells": 2,
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
