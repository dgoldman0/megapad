# Hosted source simulator

This package owns the fast semantic execution backend for ordinary MegaForth,
KDOS, rich-terminal, and Akashic source. It implements source-visible Forth
semantics directly instead of executing MP64 instructions.

## Current slice

The first runnable slice provides:

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
  storage; and
- BIOS-compatible unaligned `@`, `!`, and `+!` access and byte `FILL` over that
  shared address space, plus the arithmetic and comparison words needed by the
  next unchanged Akashic source slice.

This is deliberately not yet a complete MegaForth environment. Dictionary
bodies and task stacks are not yet backed by the sparse memory substrate.
Persistent compiler state, `CATCH`/`THROW`, the BIOS evaluator surfaces, tasks,
clocks, UART, media, and an ordinary KDOS load also remain. The simulator does
not execute ROMs, MP64 binaries, or MF64 native dictionaries, and it makes no
machine-timing, interrupt, snapshot, RTL, or hardware claim. Those remain the
architectural emulator's and physical implementation's responsibility.

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

## First real-source proof

The conformance test loads a byte-for-byte snapshot of unchanged
`akashic/utils/uint-range.f` from Akashic revision
`8e65ccf5e62d00b47e4cb846a379d12ae9297f3b`, then executes its real
`URANGE-VALID?` and `URANGE-OVERLAP?` definitions over boundary vectors. The
fixture is revision- and SHA-256-bound; it is test input, not a simulator-side
rewrite. This proves only the source and runtime semantics exercised by that
module.

The immediate next source proof loads unchanged
`akashic/utils/memory-span.f` and exercises its complete caller-owned set API,
rather than adding a host-native substitute. Its temporary pre-KDOS dependency
loader is a narrow bootstrap surface that KDOS's real `REQUIRE` must later
shadow; it is not evidence for the KDOS module loader.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
