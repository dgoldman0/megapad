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
  memory geometry, now advertising the admitted `0x7` crypto profile: CRC,
  checked SHA3/SHAKE streaming, and raw Keccak-f[1600];
- fail-closed construction for injected address spaces: their SysInfo
  capability qword must be readable and may advertise only admitted services;
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
- a shared bit-exact six-mode CRC value model with simulator-owned checked
  transaction state, coherent SysInfo capability discovery, exact byte/cell
  feeds, raw/final release, and source-visible status behavior;
- a per-runtime pseudo-BIOS diagnostic profile with persistent semantic-work
  accounting, retained non-destructive BIST observations, a real four-operation
  tile value self-test, and logical no-cache controls/zero cache counters;
- a routed per-runtime AES-128/256-GCM service shared by BIOS words and direct
  virtual MMIO, backed by a portable AES/GHASH value model and exact native
  command, status, fault, and incremental guest-transfer semantics;
- a routed per-runtime SHA3/SHAKE/raw-Keccak service shared by checked BIOS
  words and direct virtual MMIO, with complete caller-span preflight,
  `(COREID,TASK-ID)` transaction ownership, staged publication, and a portable
  24-round Keccak-f[1600] value model;
- runtime-local, per-core checked SHA-256 and SHA-512 streams with their
  distinct physical-span policy, exact 64/128-bit length accounting, staged
  digest publication, and no invented MMIO aperture or capability bit;
- a per-runtime deterministic TRNG-window model whose reproducible stream is
  derived from an explicit host-injected seed, with the native supplemental
  seed and latched-unusable lifecycle but no hardware-entropy or
  cryptographic-randomness claim;
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

Byte-exact logical lines 720 through 855 then load the complete KDOS CRC
convenience and CRC-diagnostic family. The nine BIOS words retain their
checked status and owner behavior while `_CRC-BUF-CHECKED` remains ordinary
source, including its qword loop, every exact 1–7-byte tail, and its balanced
early exits. Acceptance covers all six standard `123456789` vectors, mode-5
raw state, seeds and release, incremental memory faults, source `CATCH` and
`THROW`, the real `CRC-DIAG?`/`.CRC-DIAG`, and `.CRC32` through the live
memory-backed `BASE` cell and unsigned printer. CRC ISA instructions, CSRs,
hardware locks, and timing remain emulator-only claims.

Byte-exact logical lines 856 through 902 complete KDOS's hardware-diagnostic
source family: `.PERF`, `.BIST-STATUS`, `.TILE-DIAG`, `.ICACHE`, and aggregate
`DIAG`. All 18 BIOS words named by the section are bound. `PERF-CYCLES` counts
hosted semantic work and is not comparable to an MP64 cycle counter; the other
unmodeled hardware counters remain zero. Destructive BIST starts fail before
guest-memory or retained-BIST mutation, while their admitted faulting dispatch
still counts as work and retained idle/running/pass/fail snapshots remain
renderable.
The synchronous tile PASS is produced by the same small ADD/MUL/DOT/SUM value
kernel intended for later semantic tile adapters, with an architectural-emulator
differential, independent failure-bit coverage, and no guest scratchpad mutation.
Exact composed UART output is pinned with both signs of the backend-local
work-counter field normalized.

Byte-exact logical lines 903 through 1071 load the complete unchanged KDOS AES
section and its 14 ordinary source definitions. All 11 adjacent BIOS AES words
are bound, including the later-facing AES-128 mode selector. Acceptance pins
AES-128 and AES-256 external known answers, a direct native-MMIO differential,
mixed BIOS/MMIO state, one/two-block and exact in-place encryption, good/bad
decryption mutation order, every partial data tail, AAD lengths 1/15/16,
AAD-only data, scratch/tag lifetimes, status text, guest unwinding, recovery,
and byte-incremental caller faults. This is value/state compatibility, not a
hardware timing, RTL, constant-time, or host-secret-protection claim.

The unchanged high-level source is qualified in its defensible current domain:
plain positive uint32 multiples of 16 and AEAD AAD lengths 1..16 with
nonnegative uint32 data lengths. Plain zero/nonmultiple/high-cell lengths,
zero AAD, and AAD above 16 expose source defects; the latter can overwrite live
dictionary state. Bad-tag multi-block decrypt also leaves previously streamed
plaintext published. These cases remain explicit findings rather than hidden
host substitutions or hard-coded simulator capacities.

Byte-exact logical lines 1072 through 1216 load the complete unchanged KDOS
SHA3/SHAKE and random-helper section and its 26 ordinary source definitions.
The one-core SysInfo profile now reports `CRYPTO_CAPS = 0x7`: bit 0 is the
admitted reflected/raw CRC service, bit 1 is checked SHA3/SHAKE streaming, and
bit 2 is raw Keccak-f[1600]. Bit 3 remains clear because the hosted WOTS chain
has not been admitted. Acceptance covers SHA3-256, SHA3-512, SHAKE128,
SHAKE256, segmented and rate-boundary input, multi-window squeeze, cleanup and
ownership failures, in-place raw permutations, direct-MMIO access shapes, and
terminal state/error compatibility. The service completes semantic operations
synchronously; it does not claim an observable BUSY interval, round or bus
latency, backpressure, timeout cadence, interrupt delivery, physical spinlock
arbitration, RTL timing, or constant-time host execution.

All checked SHA input, output, and raw-state spans pass through the same
`CALLER-SPAN-STATUS` boundary before their first transfer. A nonempty Bank-0
span must begin at or above the hosted static/dictionary rollback floor and
end no higher than the active caller's future result-cell boundary
(`DSP-8`). External, HBW, and VRAM spans must fit wholly in one mapped region.
The transaction owner remains the BIOS `(COREID,TASK-ID)` pair; every current
pre-scheduler guest dispatch therefore uses `(0,0)`. An unbacked host scratch
context borrows the canonical foreground stack boundary rather than creating
an unbounded guest span.

`RANDOM`, `RANDOM8`, and `SEED-RNG` reach one per-runtime decoded TRNG window.
Its bytes are a repeatable SHA-256-derived stream from an explicit injected
seed, not host, physical, or cryptographically secure entropy. Equal seeds and
guest read/seed schedules reproduce exactly; separate runtimes do not share
pool position. Guest seed writes supplement only future bytes while usable and
cannot recover a latched failure.

Two unchanged-source limitations are now pinned rather than repaired in host
code. `.SHA3` uses `0 DO`, so only a positive, nonwrapping readable length is
qualified; zero or negative lengths can enter a wrapping/nonterminating loop.
`RAND-RANGE` is meaningful only for a positive signed maximum, faults for a
zero divisor, gives no useful range contract for a negative maximum, and uses
modulo reduction rather than rejection sampling, so its result is biased in
general. SHAKE's safe positive chunk sizes do not resolve the separate
[open `MIN` signedness discrepancy](../docs/bios-forth.md); the simulator
continues to record that mismatch without deciding whether the public word
should be signed or unsigned.

Byte-exact logical lines 1217 through 1269 add the unchanged `HASH`, `SHA256`,
and `SHA512` wrappers and their ten public status constants. `HASH` remains an
alias for the already admitted SHA3-256 wrapper. SHA-256 and SHA-512 instead
reach one runtime-local service whose contexts are keyed by architectural core,
not task, and whose successful finalization publishes exactly 32 or 64
big-endian digest bytes only after complete destination preflight.

`SHA2-SPAN-STATUS` intentionally does not reuse `CALLER-SPAN-STATUS`. A
nonempty SHA-2 span may include address zero or static Bank-0 bytes, but must
fit wholly in one Bank 0, external, HBW, or VRAM region; wrap, MMIO, unmapped,
and cross-region spans return RANGE. Native context arenas return
CONTEXT-ALIAS. Hosted SHA-2 contexts are private host objects and therefore do
not alias ordinary guest memory; a composition may provide mapped private
arena ranges when that distinction needs to be exposed. Empty spans ignore
their address. Every nonzero continuation result aborts its selected context,
and failed finalization publishes nothing to an ordinary destination.

The hosted value operation uses incremental `hashlib` objects. Clearing their
references plus explicit metadata/stage wiping proves logical simulator state,
not physical erasure inside CPython or its crypto library. The service makes no
claim about EXT.CRYPTO instructions or CSRs, engine latency, cluster
arbitration, stalls, interrupt masking, raw padding-buffer effects, or
constant-time execution. The working BIOS/native SHA-2 behavior also differs
materially from current RTL instruction glue; that discrepancy is recorded in
the [simulator contract](../docs/simulator-contract.md#6-platform-services).

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
| 39–1269 | Contiguous qualified frontier | Ordinary bootstrap through diagnostics, AES, SHA3/SHAKE/TRNG helpers, and the adjacent unchanged SHA-256/SHA-512 one-shot wrappers; line 70 is blank |
| 1270 onward | Next uncovered frontier | HMAC-SHA3 begins at line 1270; its shared-lock preamble reaches the next missing BIOS primitive, `SPIN@`, at line 1280 |

The primary progress measure is the monotonically advancing contiguous
frontier, not the number of isolated fixtures. A later island is admitted only
when it validates a cross-cutting capability needed by the frontier. As the
semantic BIOS vocabulary becomes complete, first-failure source loading should
cross more definitions per slice, the frontier increments should grow, and
qualified islands should be absorbed until ordinary complete `kdos.f` is one
continuous load.

The bootstrap loader is not KDOS module-loader evidence. It has no filesystem
or dictionary transaction and must be shadowed by KDOS's ordinary `REQUIRE`.
The next source boundary begins HMAC-SHA3 at line 1270. Its first actual
semantic-BIOS gap is `SPIN@` at line 1280; later slices continue the same
contiguous unchanged prefix toward the persistent evaluator, ordinary checked
module-loader surface, and deterministic cooperative task scheduler.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
