# Qualified NTT ordinary-memory transfers

The hosted NTT service now transfers a complete 1,024-byte polynomial through
one ordinary-memory read or write when service, memory, helper and backing
identities prove a callback-free route. Loads retain uint32 decoding, modular
reduction and the final four staging bytes. Stores retain uint32 masking.
Status, unselected buffers and final index remain unchanged.

Custom routes, MMIO, wrapping addresses, cross-region spans, holes, malformed
state and unsupported modulus retain the original byte loops. Failed
qualification introduces no whole-span error. In particular, a failed fourth
store byte still exposes the scalar path's already-advanced coefficient index.
Namespace admission rejects hostile keys before lookup and compares helper
identities without user equality.

The focused gate passed 109 checks: 85 new transfer cases and 24 existing NTT
and KEM cases. Coverage includes sparse/dense backing, page edges, exact fault
prefixes, staging/index behavior, MMIO callbacks, late customization and both
selected semantic executors.

Each measured trial performs 64 forward transforms of 256 coefficients.
The transfer case also loads and stores 1,024 bytes per iteration. Three fresh
trials, one discarded warmup and separate attribution use the same bounded
harness and direct modular DFT oracle as the pre-change residual profile.

| Case / executor | Before | After |
| --- | ---: | ---: |
| Load/transform/store, Python | 135.774 ms | 29.195 ms |
| Load/transform/store, native semantic | 154.146 ms | 28.029 ms |
| Compute control, Python | 17.735 ms | 17.663 ms |
| Compute control, native semantic | 19.031 ms | 17.898 ms |

The transfer case retains 644 semantic steps, 131,072 transferred bytes, empty
stack, DONE status, index zero and output SHA-256
`1aa704129504620655681821da33640f93a1a602045bae1300bb0ed32db948c8`.
The transform itself remains the existing Python value implementation.
These small-sample host results support the transfer change, not a general
NTT or Desktop speed claim. Reports include checkout/working-tree provenance;
the after run includes the concurrently qualified callback bridge, whose
machine-callback path this probe does not enter.

- [Before and separate attribution](runtime-residual-crypto-2026-09-30.json)
- [After and compute control](runtime-ntt-transfer-2026-09-30.json)
