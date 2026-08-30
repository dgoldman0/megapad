# Emulator ownership

This package owns the exact MP64 architectural execution backend: instruction
semantics, architectural devices, machine scheduling, DBT/native acceleration,
snapshots, and applications that inspect the concrete machine.

The existing flat emulator modules will move here in coherent dependency
clusters.  New emulator-only code belongs here now.  Target Forth source, BIOS
and ROM artifacts, RTL, and protocol specifications are project inputs rather
than emulator implementation and remain outside this package.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative backend boundary.

