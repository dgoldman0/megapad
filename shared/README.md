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
scalar multiplication are such shared value models. Field helpers own integer
results only: prime selection registers, ACC/TSRC/TDST, previous results,
guest-memory ordering, and fault publication remain simulator/emulator state.
Checked owner records, capability publication, MMIO state machines, entropy
sources, architectural register state, ISA execution, and semantic BIOS stack
adapters likewise remain in their respective backends.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative dependency direction and admission rules.
