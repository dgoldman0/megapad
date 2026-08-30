# Hosted source simulator

This package owns the fast semantic execution backend for ordinary MegaForth,
KDOS, rich-terminal, and Akashic source. It implements source-visible Forth
semantics directly instead of executing MP64 instructions.

## Current slice

The implemented slices provide:

- byte-oriented source parsing, comments, `PROVIDED`, colon definitions, and
  `IF`/`ELSE`/`THEN`, `BEGIN`/`UNTIL`/`AGAIN`,
  `BEGIN`/`WHILE`/`REPEAT`, `EXIT`, `DO`/`?DO`/`LOOP`, and `UNLOOP`
  compilation, including `LEAVE` through intervening conditionals;
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
- a read-only one-full-core SysInfo profile whose direct MMIO registers and
  BIOS topology words share the same service and report the actual sparse
  memory geometry;
- BIOS-compatible unaligned `@`, `!`, and `+!` access and byte `FILL` over that
  shared address space, plus the arithmetic and comparison words needed by the
  next unchanged Akashic source slice;
- memory-backed linked dictionary headers and CREATE-family bodies, including
  signed `ALLOT`, `,`, `C,`, `'`, `[']`, `>BODY`, and semantic `DOES>` actions;
- numeric `HERE`/`LATEST` checkpoint rollback with live-ancestry and contiguous
  reclaimed-zone validation, binding restoration, and stale-byte retention;
- the installable BIOS dictionary-fault callback, including the dynamic
  Bank-0 stack margin, exact hosted-span fit acceptance, same-dispatch guest
  `THROW`, and fail-closed handling when the callback is zero or returns;
- hosted UART output for the BIOS numeric printer, complete-task `ABORT`, and
  the stable execution-token behavior needed by source-defined `DEFER`/`IS`;
- active-line `WORD` with its transient counted string at `HERE`, forward
  `CMOVE`, byte fetch, stack depth, and compiled/interpret-state `."` plus the
  supported compile-state `ABORT"` path;
- a memory-backed canonical foreground data/return stack with exact downward
  cell geometry, retained continuation slots, `SP@`/`SP!` and `RP@`/`RP!`;
- the unchanged source-defined KDOS Bank-0 heap, including lazy setup,
  first-fit allocation, sorted free/coalescing, resize, statistics, structural
  verification, and its dictionary/stack/heap proximity guard; and
- an exact-record bootstrap loader that supplies a shadowable `REQUIRE` before
  KDOS exists, with nested budgets, cycle detection, and registry-only failure
  cleanup.

This is deliberately not yet a complete MegaForth environment. Additional
task stack arenas and scheduling remain pending. Persistent compiler state,
the BIOS evaluator surfaces, clocks, complete UART/MMIO service, media, and an
ordinary complete KDOS load also remain. The simulator does not execute ROMs,
MP64 binaries, or MF64 native dictionaries, and it makes no machine-timing,
interrupt, snapshot, RTL, or hardware claim. Those remain the architectural
emulator's and physical implementation's responsibility.

The current stack bounds enforce the canonical mapped Bank 0 halves, and the
ordinary KDOS `?DICT-ROOM` guard observes the live stack and heap. Every
current guest semantic HERE mutation and transient `WORD` span preflights
against the live data-stack margin before bytes or dictionary metadata change.
An installed `DICT-FAULT-XT!` callback receives rejection; zero or a returning
callback takes the BIOS diagnostic-and-ABORT fallback. Unbacked contexts from
`new_context()` are host scratch views rather than guest tasks, so their
dictionary operations use the canonical foreground stack margin. Direct
`runtime.dictionary` mutation remains a low-level host/test seam outside the
guest ABI. External user-dictionary bounds, their switching words, and the
later transactional evaluator remain pending.

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

The contiguous-prefix proof continues with byte-exact logical lines 71 through
115; the only omitted source byte between fixtures is blank line 70. The
unchanged definitions allocate `NAMEBUF`, `PATHBUF`, and `PN-LEN`, then compile
and execute `PARSE-NAME`, `NEEDS`, `ASSERT`, `.DEPTH`, and `0>=`. Acceptance
checks transient `WORD` geometry without moving `HERE`, path clamping and tail
clearing, low-to-high `CMOVE` overlap, exact quote payloads and abort output,
pre-push `DEPTH`, wrapped scalar operations, signed `>`, and the current
executable BIOS's unsigned `MIN` behavior. That `MIN` behavior mirrors an
[open documentation/implementation discrepancy](../docs/bios-forth.md), not a
decision that unsigned comparison is the desired final API. Interpret-state
`ABORT"` remains outside this supported slice because unchanged KDOS uses its
compile path; native BIOS currently emits orphan code for that malformed use
rather than providing useful interpreter semantics.

The contiguous proof now advances in one capability-sized block through
logical line 545. Byte-exact lines 116 through 545 compile all 32 definitions
in KDOS's Bank-0 allocator section. Acceptance reaches `MEM-SIZE` through its
ordinary direct SysInfo `@`, checks the one-core classification words, exact
heap header geometry and idempotent setup, pre-setup invalid-request rejection,
minimum/aligned splitting, sorted free and bidirectional coalescing, shrink,
adjacent in-place growth, fallback allocate-copy-free, failure preservation,
live statistics, exact `.HEAP` output, and corruption detection. The hosted
`RegionAllocator` is not substituted for this path; allocator links, sizes,
canaries, payloads, and mutations remain guest-visible KDOS memory.
Qualification also pins, but does not endorse, the current invalid-size
`RESIZE` result of `0 -1`; its difference from the OOM path's original-address
failure is recorded as an
[open KDOS contract discrepancy](../docs/kdos-reference.md#11-memory-allocator).

The next exact block, logical lines 546 through 617, loads KDOS's ordinary
`MARKER` and `FORGET` definitions. Acceptance checks their live dictionary
header access, numeric `HERE`/`LATEST` checkpoint bodies, self-removal,
case-insensitive lookup, shadow restoration, stale guest bytes, execution-token
invalidation, and address reuse. The same path exercises source-compiled
`LEAVE`; neither operation is replaced with a hosted whole-word substitute.

The now-contiguous loader then evaluates byte-exact logical lines 618 through
675 from the same revision. It installs the ordinary KDOS per-context `HANDLER`
tables and source-defined `CATCH`/`THROW`; the simulator does not substitute
host exception words. Acceptance covers normal completion, zero and nonzero
throws, nested rethrow, exact data/return-stack restoration, and unwinding
through an active loop and deferred `DOES>` action. `ABORT` remains the
distinct noncatchable BIOS reset path. Loading the intervening snapshot block
absorbs the former exception island into the monotonic frontier.

Byte-exact logical lines 676 through 719 install KDOS's ordinary
`_KDOS-DICT-FAULT` and exception-safe task-boundary wrappers. Acceptance drives
all current dictionary emitters through a real nested guest `CATCH`, proves
the standard `-8` result and failure atomicity, checks the zero/returning-hook
fallback abort, and verifies exact hosted semantic spans that fit do not call
the hook. The four task wrappers capture distinct, live pre-shadow BIOS XTs.
The shared source helper changes exactly one selected background handler and
never slot zero. Each start wrapper reaches that reset before its captured
BIOS entry reports scheduling unavailable. `TASK-STOP` orders its reset after
the captured entry, so that reset is deliberately unreachable until
cancellation exists.
Resumable cooperative task contexts, `PAUSE`, and scheduling have not been
implemented, so this slice makes no task-execution or cadence claim.

A host-side budget or implementation error that escapes a dispatch which has
observed `RP@` marks that execution context non-reusable. The registration is
kept for the complete dispatch because unchanged KDOS pops a saved handler
cell immediately before restoring the `HANDLER` variable. This conservative,
fail-closed boundary covers that one-operation cleanup window and prevents a
stale guest handler from reviving abandoned continuations. Transactional
context recovery belongs to the pending evaluator/rollback slice. Ordinary
source `THROW` never escapes the outer public host boundary, including when it
crosses a nested host primitive's `execute`/`evaluate` call. Guest `RP!`
remains a raw aligned restore within its caller-owned stack span.

### KDOS source frontier

| Logical lines | Status | Purpose |
|---|---|---|
| 39–719 | Contiguous qualified frontier | Ordinary bootstrap, parsing utilities, Bank-0 allocator, dictionary snapshots, exceptions, dictionary-fault routing, and task-boundary shadowing; line 70 is blank |
| 720 onward | Next uncovered frontier | CRC source first becomes executable at line 739; `CRC-FEED` is the first missing BIOS primitive reached at line 748 |

The primary progress measure is the monotonically advancing contiguous
frontier, not the number of isolated fixtures. A later island is admitted only
when it validates a cross-cutting capability needed by the frontier. As the
semantic BIOS vocabulary becomes complete, first-failure source loading should
cross more definitions per slice, the frontier increments should grow, and
qualified islands should be absorbed until ordinary complete `kdos.f` is one
continuous load.

The bootstrap loader is not KDOS module-loader evidence. It has no filesystem
or dictionary transaction and must be shadowed by KDOS's ordinary `REQUIRE`.
The next source boundary begins KDOS's CRC convenience family. Later slices
continue the same contiguous unchanged prefix toward the persistent evaluator,
ordinary checked module-loader surface, and deterministic cooperative task
scheduler.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
