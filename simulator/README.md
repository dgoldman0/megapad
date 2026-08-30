# Hosted source simulator

This package owns the fast semantic execution backend for ordinary MegaForth,
KDOS, rich-terminal, and Akashic source. It implements source-visible Forth
semantics directly instead of executing MP64 instructions.

## Current slice

The implemented slices provide:

- byte-oriented source parsing, comments, `PROVIDED`, colon definitions, and
  `IF`/`ELSE`/`THEN`, `EXIT`, `DO`/`?DO`/`LOOP`, and `UNLOOP` compilation;
- wrapping 64-bit cells, full-width Forth flags, newest-definition lookup,
  stable numeric execution tokens, compile-time binding, and ordinary
  source-parsing `CONSTANT` definitions;
- an explicit dispatcher with colon continuations, loop state, and user
  `>R`/`R@`/`R>` values on one ordered return stack;
- a focused core vocabulary sufficient to execute the first unchanged Akashic
  utility source, with an optional caller-owned semantic step budget;
- a sparse 64-bit address space with distinct Bank 0, external, VRAM, HBW, and
  reserved MMIO classes, plus a caller-bounded allocator for hosted runtime
  storage;
- BIOS-compatible unaligned `@`, `!`, and `+!` access and byte `FILL` over that
  shared address space, plus the arithmetic and comparison words needed by the
  next unchanged Akashic source slice;
- memory-backed linked dictionary headers and CREATE-family bodies, including
  signed `ALLOT`, `,`, `C,`, `'`, `[']`, `>BODY`, and semantic `DOES>` actions;
- hosted UART output for the BIOS numeric printer, complete-task `ABORT`, and
  the stable execution-token behavior needed by source-defined `DEFER`/`IS`;
  and
- an exact-record bootstrap loader that supplies a shadowable `REQUIRE` before
  KDOS exists, with nested budgets, cycle detection, and registry-only failure
  cleanup.

This is deliberately not yet a complete MegaForth environment. Task stacks
are not yet backed by the sparse memory substrate. Persistent compiler state,
`CATCH`/`THROW`, the BIOS evaluator surfaces, tasks, clocks, complete UART/MMIO
service, media, and an ordinary complete KDOS load also remain. The simulator
does not execute ROMs, MP64 binaries, or MF64 native dictionaries, and it makes
no machine-timing, interrupt, snapshot, RTL, or hardware claim. Those remain
the architectural emulator's and physical implementation's responsibility.

## Run it

The focused simulator suite is seconds-scale and does not build the native
emulator accelerator:

```sh
make test-simulator
```

A minimal hosted-source invocation is:

```python
from simulator.runtime import MegaForthRuntime

runtime = MegaForthRuntime()
runtime.evaluate(b": TWICE DUP + ;")

context = runtime.new_context()
context.data.push(21)
runtime.execute("TWICE", context=context)
assert context.data.snapshot() == (42,)
```

## Real-source proofs

The conformance test loads a byte-for-byte snapshot of unchanged
`akashic/utils/uint-range.f` from Akashic revision
`8e65ccf5e62d00b47e4cb846a379d12ae9297f3b`, then executes its real
`URANGE-VALID?` and `URANGE-OVERLAP?` definitions over boundary vectors. The
fixture is revision- and SHA-256-bound; it is test input, not a simulator-side
rewrite. This proves only the source and runtime semantics exercised by that
module.

The second proof loads a byte-identical snapshot of unchanged
`akashic/utils/memory-span.f` at the same revision. Its real
`REQUIRE uint-range.f` resolves through the narrow bootstrap loader, and all
26 definitions are compiled from source. Acceptance exercises the scalar span
predicates and complete inline, caller-owned set API, including raw layout,
capacity bounds, adjacency without coalescing, overlapping `PUSH`, disjoint
`ADD`, malformed geometry, failure atomicity, borrowed bytes, and `CLEAR`.
There is no simulator-side memory-span substitute.

The first KDOS proof evaluates byte-for-byte `kdos.f` logical lines 39 through
69 from MegaPad revision `ed451faccfddb5f3fbb4e2200eb0dd0fdc314f4c`.
The unchanged source defines `.R`, `DEFER`, `IS`, and `SAMESTR?`. Acceptance
executes deferred children before and after ordinary `IS` rebinding, including
a precompiled caller, and checks guest body bytes, stable execution tokens,
`ABORT`, numeric UART output, and unsigned byte-string comparison. This is a
staged source-load proof, not yet a claim that complete `kdos.f` loads.

The bootstrap loader is not KDOS module-loader evidence. It has no filesystem
or dictionary transaction and must be shadowed by KDOS's ordinary `REQUIRE`.
The next slice advances to exact memory-backed stack pointers, `CATCH`/`THROW`,
and evaluator rollback before extending the unchanged KDOS source frontier.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
