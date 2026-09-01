# Shared host ownership

This package owns host-side code that is genuinely independent of execution
backend: protocol codecs and models, immutable value types, conformance helpers,
and host sink/source interfaces.

Shared code imports neither `emulator` nor `simulator` and cannot inspect a
backend's private CPU, bus, scheduler, memory, or device objects.  An adapter
that does so belongs with that backend even if the model behind it is shared.

The crypto capability-bit registry, six-mode CRC parameter table and pure
byte/cell recurrences, AES/GHASH operations, the 24-round Keccak-f[1600]
permutation, 256-bit Field arithmetic/raw-product values, and RFC 7748 X25519
scalar multiplication, the generic 256-point radix-2 NTT value transforms,
deterministic ML-KEM-512 key-generation/encapsulation/decapsulation bytes, and
FP16/BF16/binary32 bit-value conversions are such shared value models. The
frozen storage sector size plus command, status, result, and capability values
are likewise backend-neutral ABI constants. Controller registers, image
ownership, checked-request execution, DMA, completion publication, and
durability remain backend state and policy. Field, NTT, ML-KEM, and
floating-point helpers similarly own pure result values only: mode and prime
selectors, ACC/TSRC/TDST, retained buffers/registers, previous results,
guest-memory ordering, status, and fault publication remain simulator/emulator
state. The FP16 encoder intentionally preserves the current executable
emulator's subnormal carry behavior while that discrepancy is unresolved; it
is compatibility machinery, not an independent IEEE conformance oracle. The
ML-KEM implementation is ordinary non-constant-time Python for target-value
compatibility; it is not a host-secret cryptography API.
Checked owner records, capability publication, MMIO state machines, entropy
sources, architectural register state, ISA execution, and semantic BIOS stack
adapters likewise remain in their respective backends.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative dependency direction and admission rules.
