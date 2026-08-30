# Hosted source simulator

This package owns the fast semantic execution backend for ordinary MegaForth,
KDOS, rich-terminal, and Akashic source.  It implements Forth compiler/runtime
semantics, virtual cells/stacks/memory/time/tasks, and simulator-facing platform
services without executing MP64 instructions.

It does not qualify MP64 binaries, machine timing, interrupts, snapshots,
physical concurrency, RTL, or hardware.  Those claims remain with the
architectural emulator and physical implementation.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.

