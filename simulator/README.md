# Hosted source simulator

This package owns the fast semantic execution backend for ordinary MegaForth,
KDOS, rich-terminal, and Akashic source. It implements source-visible Forth
semantics directly instead of executing MP64 instructions.

## Current slice

The first runnable slice provides:

- byte-oriented source parsing, comments, `PROVIDED`, colon definitions, and
  `IF`/`ELSE`/`THEN`, `EXIT`, `DO`/`LOOP`, and `UNLOOP` compilation;
- wrapping 64-bit cells, full-width Forth flags, newest-definition lookup,
  stable numeric execution tokens, and compile-time binding;
- an explicit dispatcher with colon continuations, loop state, and user
  `>R`/`R@`/`R>` values on one ordered return stack; and
- a focused core vocabulary sufficient to execute the first unchanged Akashic
  utility source, with an optional caller-owned semantic step budget.

This is deliberately not yet a complete MegaForth environment. Byte-addressed
memory, allocation, `REQUIRE`, transactional module loading, Forth
`CATCH`/`THROW`, tasks, clocks, UART, media, and the rich-terminal service path
are still to be implemented. The simulator does not execute ROMs, MP64
binaries, or MF64 native dictionaries, and it makes no machine-timing,
interrupt, snapshot, RTL, or hardware claim. Those remain the architectural
emulator's and physical implementation's responsibility.

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

The immediate next slice is byte-addressed memory and allocation together with
exceptions and transactional `REQUIRE`/source loading. That opens the next
unchanged source target, `akashic/utils/memory-span.f`, rather than adding a
host-native substitute for it.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
