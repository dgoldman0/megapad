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
- BIOS-compatible unaligned `@`, `!`, and `+!` access, low-byte `C!`,
  bytewise little-endian `W@`/`W!`/`L@`/`L!`, byte `FILL`, and full-cell
  `XOR` over that shared address space, plus the arithmetic and comparison
  words needed by unchanged source;
- memory-backed linked dictionary headers and CREATE-family bodies, including
  signed `ALLOT`, `,`, `C,`, `'`, `[']`, `>BODY`, and semantic `DOES>` actions;
- separate open-definition and compile/interpret state for `[` and `]`, plus
  the exact `[ 0 C, ]` admission that compiles MP64 `IDL` as semantic `Idle`
  IR without leaking a native byte into hosted dictionary storage;
- a runtime-owned one-core IDL suspension boundary with opaque continuation
  handles, one-shot interrupt/DMA wake receipts, cumulative step budgets,
  cancellation, and original return-stack guard restoration;
- numeric `HERE`/`LATEST` checkpoint rollback with live-ancestry and contiguous
  reclaimed-zone validation, binding restoration, and stale-byte retention;
- the installable BIOS dictionary-fault callback, including the dynamic
  Bank-0 stack margin, exact hosted-span fit acceptance, same-dispatch guest
  `THROW`, and fail-closed handling when the callback is zero or returns;
- hosted UART byte I/O: immediate output for the BIOS numeric printer and
  bytewise `.ZSTR`, plus a runtime-owned deterministic input FIFO behind
  non-consuming `KEY?` and blocking one-byte `KEY`; output retains `.ZSTR`'s
  unbounded NUL scan and partial publication on a later read fault, while the
  same core slice provides complete-task `ABORT` and the stable execution-token
  behavior needed by source-defined `DEFER`/`IS`;
- a shared bit-exact six-mode CRC value model with simulator-owned checked
  transaction state, coherent SysInfo capability discovery, exact byte/cell
  feeds, raw/final release, and source-visible status behavior;
- a per-runtime pseudo-BIOS diagnostic profile with persistent semantic-work
  accounting, a distinct low-32-bit semantic `CYCLES` clock, retained
  non-destructive BIST observations, a real four-operation tile value
  self-test, and logical no-cache controls/zero cache counters;
- a runtime-local deterministic RTC epoch subwindow at `+0xB08..+0xB0F`, with
  explicit host set/advance controls, low-byte read latching, direct MMIO
  access, and BIOS `EPOCH@`, but no automatic or wall-clock advancement;
- a retained one-core semantic tile service for four integer lane widths plus
  FP16/BF16, integer and half-format ADD/SUB/MUL/SUM/MIN/MAX/SUMSQ/DOT,
  low-byte control registers, completed-operation accounting, and the
  ACC/TSRC0/TDST state shared with the hosted Field ALU;
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
- a runtime-local 16-entry semantic spinlock bank with physical-core ownership,
  depthless same-core reacquisition, nonblocking contention, and owner-only
  release for the ordinary pseudo-BIOS words;
- a runtime-local, exclusively claimed block-media service behind the ten
  production query/checked pseudo-BIOS words, with full physical-window DMA
  validation, generation-bound stale rejection, 255-sector chunking, precise
  whole-sector progress, and explicit flush semantics, but no raw storage MMIO
  or controller-timing claim;
- the native `MP64FS-VALID?` prerequisite, preserving the executable BIOS's
  literal `1`/`0` result, fixed scratch layout, three separately locked checked
  reads, dynamic marker-1 geometry, narrow occupied-entry predicate, and final
  attachment-generation check without strengthening its filesystem policy;
- a runtime-local per-core Field-ALU service for the 15 general arithmetic/raw
  BIOS words and six X25519 staging words, backed by portable Field and RFC
  7748 value models and preserving native four-qword effects without emulating
  EXT.CRYPTO timing;
- a runtime-local shared NTT service for all 10 raw BIOS words, backed by the
  executable emulator's generic 256-point transform values and preserving its
  coefficient, status, index, and byte-transfer effects without admitting the
  physical NTT MMIO window;
- a runtime-local shared KEM service for all seven raw BIOS words, backed by a
  portable deterministic ML-KEM-512 value model and preserving the executable
  Python device's five retained buffers, selector/index, synchronous status,
  and byte-fault order without admitting either physical KEM MMIO contract;
- unchanged source-defined HMAC-SHA256, SHA3/SHA-256 HKDF, and hybrid
  X25519+ML-KEM composition, including lock-9 serialization, checked status
  propagation, caller-bounded expansion, and the source's retained global PQ
  scratch and nontransactional failure order;
- dynamic `HBW-BASE`/`HBW-SIZE` BIOS reads routed to the bound SysInfo service
  and the unchanged source-defined HBW bump allocator, including its shared
  pointer, exact-fit/zero allocation, bulk reset, and unchecked edge behavior;
- checked external dictionary-bound publication plus unchanged KDOS userland
  partitioning, with Bank-0/XMEM `HERE` transitions, one linked dictionary,
  index-coherent external definitions and rollback, capacity-derived reserve,
  reset-floor protection, and the deferred free-span overlap guard;
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
  verification, and its dictionary/stack/heap proximity guard;
- an exact-record bootstrap loader that supplies a shadowable `REQUIRE` before
  KDOS exists, with nested budgets, cycle detection, and registry-only failure
  cleanup;
- the one-core semantic BIOS evaluator: raw guest `EVALUATE` through 255 bytes,
  checked statuses and diagnostics, persistent cross-call compiler/control
  state, explicit finish/reset/unwind, and one inherited public step budget;
- the retired user-mode compatibility ABI: stack-neutral `ENTER-USER` and
  `SYS-EXIT`, constant-supervisor `PRIV@`, and runtime-local, guest-visible
  `MPU-BASE`/`MPU-LIMIT` registers which retain values but do not enforce
  access restrictions;
- the unchanged KDOS checked whole-source compiler and MP64FS `LOAD`, including
  nested relative paths, concatenated primary/secondary extents, guest
  `THROW` cleanup, and the literal pre-registry transaction hooks;
- unchanged `APP-EVAL` and MP64FS `APP-LOAD` through their normal evaluator,
  loader-frame, allocation, extent-read, and transaction paths, plus the
  adjacent canonical ANSI byte helpers;
- unchanged MP64FS whole-file encryption through the ordinary open descriptor,
  Bank-0 DMA allocator, AES-GCM words, guarded storage transfers, directory
  cache, sync, and flush ordering;
- unchanged parent-byte `PWD`, `CD`, `MKDIR`, and `RMDIR` through the ordinary
  parser, directory cache, RTC timestamp, sync, and flush paths;
- the unchanged KDOS Documentation Browser through ordinary `FREAD`, file
  descriptors, directory scans, ANSI pagination, deterministic `KEY`, and
  final `FCLOSE`/sync rather than a hosted documentation shortcut;
- unchanged KDOS Dictionary Search over the guest-visible linked headers,
  transient `WORD` pattern, nested-loop substring matcher, native-compatible
  `TYPE`/`SPACE`, and raw `LATEST` traversal;
- the unchanged KDOS task registry and synchronous run-to-completion executor,
  including its descriptor/state bookkeeping and provisional deferred
  checkpoint binding.

This is deliberately not yet a complete MegaForth environment. Additional
private task contexts and genuine cooperative scheduling remain pending. The
loaded KDOS words execute task XTs inline on the caller's stacks; the IDL seam
blocks and resumes one compiled-word dispatch and cannot turn that registry
into `PAUSE`, task round-robin, interrupt-vector delivery, DMA timing, or a
device scheduler.
Public `SOURCE`, `>IN`, and `STATE`, interpret-time `[IF]`, `MS@` and the
remaining RTC/calendar service, raw UART MMIO, TX-ring capacity and timing,
terminal geometry, raw storage-controller access, and an ordinary complete
KDOS load still remain.
The simulator does not
execute ROMs, MP64
binaries, or MF64 native dictionaries, and it makes no machine-timing,
snapshot, RTL, or hardware claim. Those remain the architectural emulator's
and physical implementation's responsibility.

The current stack bounds enforce the canonical mapped Bank 0 halves, and the
ordinary KDOS `?DICT-ROOM` guard observes the live stack and heap. Every
current guest semantic HERE mutation and transient `WORD` span preflights
against the live data-stack margin before bytes or dictionary metadata change.
An installed `DICT-FAULT-XT!` callback receives rejection; zero or a returning
callback takes the BIOS diagnostic-and-ABORT fallback. Unbacked contexts from
`new_context()` are host scratch views rather than guest tasks, so their
dictionary operations use the canonical foreground stack margin. Direct
`runtime.dictionary` mutation remains a low-level host/test seam outside the
guest ABI. The external interval and its source-defined switching words are
now admitted, as are the persistent semantic BIOS evaluator and KDOS-owned
checked source/loader layer. The later module registry remains responsible for
installing real loader transaction actions. Hosted
Bank-0 relocation still refuses to move below the semantic dictionary's
initial start even though native raw `ALLOT` has no equivalent lower-bound
check. That pre-existing divergence is outside the userland transition and is
not presented as native equivalence.

The application compatibility words follow the checked-in BIOS after removal
of hardware user mode. `ENTER-USER` and `SYS-EXIT` are true no-ops and `PRIV@`
always reports supervisor level 0. `MPU-BASE!`, `MPU-LIMIT!`, and their fetchers
retain wrapped cells independently in each runtime so unchanged software can
observe its setup and teardown, but those registers do not gate semantic
memory access. They are compatibility state, not a sandbox or protection
boundary.

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

`IDLE` uses the explicit block/wake API. A host first runs a compiled word to
its next boundary, publishes one admitted wake, then resumes the exact opaque
suspension. Receipts cannot be reused across boundaries:

```python
from simulator.runtime import BlockedExecution, IdleWake

runtime.evaluate(b": IDLE [ 0 C, ] ;")
blocked = runtime.run_until_blocked("IDLE", context=context)
assert isinstance(blocked, BlockedExecution)
receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
runtime.resume(blocked.suspension, receipt)
```

Hosted UART ingress is explicit and replayable. `inject_uart_input` accepts
only `bytes`, appends them FIFO, and `uart_input` returns an immutable snapshot
without consuming it. `KEY?` returns a full-width Forth flag without changing
the queue; `KEY` consumes exactly one byte when available. An empty `KEY`
reaches the same `Idle` boundary shown above, and every inputless wake retries
and blocks again instead of inventing a character or spinning. Because source
evaluation and nested host dispatch cannot detach their Python continuation,
callers invoking an input-reading word through `evaluate` must prequeue every
byte it will consume. UART output is already published immediately, so the
native `KEY` operation's pre-block TX flush has no additional hosted effect.

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
executable BIOS's unsigned `MIN`/`MAX` behavior. That behavior mirrors an
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
task cancellation exists.
At this early prefix boundary the scheduler source has not loaded, so the
slice makes no task-execution or cadence claim. The later §8 frontier adds its
synchronous registry/executor, but resumable cooperative task contexts and
`PAUSE` remain unimplemented.

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

Byte-exact logical lines 1270 through 1431 complete unchanged KDOS §1.7. HMAC
runs as ordinary Forth over checked SHA3, not as a host-side shortcut: it
normalizes keys longer than the 136-byte rate, constructs ipad/opad, propagates
the first checked status, holds shared lock 9 across both hash transactions,
and wipes all 392 bytes of HMAC scratch before an ordinary release. Capability
absence wins before contention, and contention wins before argument
validation. The adjacent `ENCRYPT`/`DECRYPT` aliases reuse the admitted AES
path; `VERIFY` traverses its requested bytes but carries no hosted
constant-time claim. Only positive, nonwrapping `VERIFY` lengths are
qualified: its unchanged `0 DO` enters the body for zero length and can wrap
or fault instead of performing an empty comparison.

The hardware-compatible spinlock model deliberately keeps same-core
reacquisition depthless and ignores task identity. Consequently, the source's
cleanup-failure description is only peer-core fail-closed: a retained lock 9
blocks another physical core, but a later wrapper on the retaining core can
reacquire and one release can free it. The discrepancy is documented without
choosing a future KDOS-owner or hardware-recursion fix. Direct spinlock MMIO,
out-of-range lock-number aliasing, bus arbitration, fairness, timing, memory
fences, and host-thread synchronization are outside this pseudo-BIOS slice.
The bank is also not yet the backing object for SHA3's logical checked lock-8
owner; arbitrary `SPIN@ 8` interference and multicore guard interoperation
must be unified before either is claimed. Unchanged HMAC uses only lock 9 and
then the checked SHA3 ABI, so it does not cross that deferred seam.

Byte-exact logical lines 1433 through 1481 add unchanged KDOS X25519. The
source initializes the 32-byte base point, runs `X25519` through the six raw
BIOS words, fills the persistent private buffer with 32 `RANDOM8` calls for
`X25519-KEYGEN`, and computes `X25519-DH` into its persistent shared buffer.
Scalar clamping and point top-bit masking happen inside the Field operation;
the source buffers remain unchanged except for their documented outputs.
There is no checked status, capability bit, lock, automatic wipe, or all-zero
shared-secret rejection. The four global 32-byte buffers are cooperative KDOS
state rather than task/core-safe storage, and hosted key generation consumes
the deterministic development TRNG rather than secure entropy.

The hosted Field service is physical-core state shared by tasks on that core:
ACC0-ACC3, deferred TSRC0, raw-result TDST, prime configuration, and persistent
previous-low/high. Scalar loads, point reads, and result stores each use four
ascending qwords and retain the raw BIOS's partial-fault behavior; the
high-level word consumes both inputs before output and therefore permits
destination aliasing. Hosted execution claims RFC 7748 bytes and these
memory/state effects, not ISA encodings, CSRs, 4335-cycle latency, stalls,
interrupts, RTL integration, constant-time host execution, or physical
erasure.

This slice follows RFC 7748 and the native C++/standalone-RTL constant
`A24=121665`. The architectural Python emulator currently uses `121666` with
the same `AA + A24*E` formula and fails the published RFC vector. Separately,
the standalone Field-ALU RTL is not connected into the current full-core SoC
path, and the microcore declares Field ports that its cluster does not wire.
Simulator success is therefore not Python-interpreter or integrated-RTL
X25519 evidence; those discrepancies remain explicit rather than normalized.

Byte-exact logical lines 1483 through 1515 now add unchanged KDOS §1.10 over
all 15 general Field BIOS words. The source defines four prime selectors and
four zeroed 32-byte scratch buffers. The ABI uses address operands and
separate low/high raw destinations; `FCMOV` consumes an operand address and a
condition-byte address. `LOAD-PRIME` latches but does not select a custom
prime. Only custom-mode `FMUL`, `FSQR`, and the product in `FMAC` use a
nonzero Montgomery inverse; inverse and power remain ordinary exponentiation.

The qualified arithmetic domain is canonical field inputs with a valid prime
or custom Montgomery tuple. Hosted execution keeps architecturally intended
256-bit previous state and correct wrapped 512-bit raw-MAC carry. Native C++
currently leaks hidden upper limbs after noncanonical ADD/SUB and drops that
raw carry; Python uses full modulo and byte-granular Field traffic where
BIOS/native use qwords; standalone RTL differs on malformed custom-zero state
and is not integrated into a complete core. Exact publication/fault order and
these unresolved discrepancies are specified in the
[simulator contract](../docs/simulator-contract.md#6-platform-services).

Byte-exact logical lines 1517 through 1584 add unchanged KDOS §1.11. They
define the Kyber and Dilithium modulus constants, A/B selectors, two global
1024-byte scratch buffers, `NTT-POLYMUL`, and `.NTT-STATUS`. The hosted raw
surface contains all 10 checked-in words, including the previously omitted
`NTT-IDX!`; `NTT-LOAD` takes both an address and selector, while `NTT-PMUL`
and `NTT-PADD` take no stack arguments.

The service follows the BIOS plus the Python device used by both interpreted
and native-accelerated emulator execution: q is uint64, coefficients are
uint32 little endian, selector zero means A and every nonzero selector means
B, and ordinary commands complete synchronously from initial status 0 to
retained status 2. `NTT-WAIT` still waits specifically for DONE, so calling it
while idle remains an indefinite guest loop bounded only by an optional hosted
step budget. Loads and stores retain their exact byte-level partial-fault and
index-increment order. State is shared by all contexts in a runtime and has no
lock, owner, checked status, capability bit, or implicit unwind cleanup.

This generic transform computes cyclic convolution modulo `x^256-1`; it is
not the specialized negacyclic polynomial operation used by ML-KEM or ML-DSA.
The emulator's ML-KEM device uses separate ML-KEM-specific routines. Current NTT
RTL is also not executable-BIOS compatible: it has a different 64-bit-slot
map, consumes unit-width transfers while BIOS emits bytes, retains Kyber-only
twiddles and inverse scale when q changes, and exposes real BUSY latency.
Hosted NTT is therefore a pseudo-BIOS semantic slice, not direct MMIO, RTL,
cycle, arbitration, or standardized-PQ evidence.

Byte-exact logical lines 1586 through 1633 now add unchanged KDOS §1.12. They
define five buffer IDs, five size constants, `KYBER-KEYGEN`, `KYBER-ENCAPS`,
`KYBER-DECAPS`, and `.KEM-STATUS` over the exact seven-word raw BIOS surface:
`KEM-SEL!`, `KEM-LOAD`, `KEM-STORE`, the three commands, and `KEM-STATUS@`.
The source-visible `KEM-SEED-SIZE=32` remains recorded alongside the literal
64-byte `d || z` transfer performed by `KYBER-KEYGEN`; this slice does not
silently choose which interface should change.

The service follows the working Python device used by both interpreted and
native-accelerated emulator CPU execution. It owns one per-runtime set of
SEED/COIN, PK, SK, CT, and SS buffers, plus one selector, byte index, and
status shared by every guest context. Selection uses the low byte, clamps
values above four to SS, and resets the index. Short loads preserve suffixes;
excess DIN is dropped, excess DOUT is zero, and the index pins at capacity.
`KEM-LOAD` reads the caller byte before DIN, while `KEM-STORE` consumes DOUT
before the caller write, preserving the executable partial-fault order.
Commands synchronously replace only their documented output buffers and leave
retained DONE=2, selector, and index unchanged. There is no owner, lock,
capability bit, transaction rollback, automatic wipe, or warm-boot claim.

The shared value model exactly reproduces the former emulator-local code. A
pinned zero-`d || z`/zero-coin fixture was independently checked against local
OpenSSL 3.5.2 ML-KEM-512 through keygen, encapsulation, valid decapsulation,
and implicit rejection. That evidence is limited to generated or independently
validated fixed-size keys. The implementation accepts some malformed keys,
uses a fixed 840-byte SHAKE sampling prefix, compares in ordinary Python, and
retains secrets. It is neither FIPS certification nor a hostile-key validator,
constant-time host primitive, or protected secret boundary.

This is another pseudo-BIOS-only slice. Native CPU acceleration has no C++ KEM
implementation and falls through to the Python device. Current RTL instead
uses incompatible 64-bit slots, exposes BUSY latency, advances/clamps streams
differently, fills only partial buffers, and computes deterministic XOR test
data. In particular, BIOS DOUT at byte `+0x18` reads RTL BUF_SIZE. Hosted KEM
therefore makes no direct-MMIO, RTL, cycle, arbitration, or physical-erasure
claim.

Byte-exact logical lines 1635 through 2043 now load all 59 definitions in the
adjacent hybrid/HKDF block unchanged. The source first allocates and initializes
the hybrid scratch, defines the complete SHA3-HMAC HKDF family, adds
HMAC-SHA256 and HKDF-SHA256, then publishes `PQ-DERIVE`,
`PQ-EXCHANGE-INIT`, and `PQ-EXCHANGE-RESP`. Independent HMAC/HKDF vectors plus
a distinct-key, two-party initiator/responder exchange pin the guest
composition across one-, two-, and three-block derivations; the raw X25519 and
ML-KEM stage values use their already qualified shared value models.

Each public HMAC/HKDF call makes one nonblocking lock-9 attempt. SHA3
capability absence precedes that lock check; contention otherwise returns the
selected hash family's state status. Expand accepts only lengths 0 through
8,160 and rejects complete output aliases with the 32-byte PRK or nonempty info
before publication, then publishes successful blocks incrementally. Acquired
paths wipe their private HMAC/HKDF state before release; capability and busy
exits do not enter that guard. The null-salt branch tests only `slen=0`, despite
the source comment also mentioning pointer zero: nonempty address zero is
rejected by SHA3 caller-span policy but is physically readable by SHA-256. The
simulator records and reproduces that difference without selecting a future
interface.

The outer hybrid exchange is deliberately not upgraded into a transaction.
It shares `X25519-PRIV`, the raw KEM service, and retained secret-bearing PQ
scratch without an owner; extract and expand are two separately locked calls.
If extract contends or discovers absent SHA3 during INIT, X25519 and KEM have
already run, 32 deterministic entropy bytes have been consumed, the ciphertext
has been published, and SS/CAT/coin scratch has changed, while `_PQ-PRK` and
the final-key destination remain untouched. If extract succeeds but expand
then contends or fails, `_PQ-PRK` has also been published. Raw memory, entropy,
or service exceptions are not converted into an HKDF status. This qualifies
the ordinary KDOS application composition, not a standardized hybrid KEM,
protected secret boundary, constant-time implementation, or security proof.

Byte-exact logical lines 2044 through 2108 add the complete nine-definition
HBW bump allocator. `HBW-BASE` and `HBW-SIZE` are dynamic reads of the same
SysInfo qwords that describe the sparse address space; no host allocator or
copied geometry is substituted. Load-time `HBW-INIT` sets the two guest
variables, and ordinary source handles sequential, zero-byte, exact-fit,
checked-failure, 64-byte alignment, status rendering, and global pointer reset.
The pointer is shared across contexts in one runtime but independent across
runtimes. Reset reclaims addresses without wiping bytes or revoking stale
pointers, and there is no owner, lock, allocation ledger, or individual free.

The qualified allocation domain is a nonwrapping request within the remaining
mapped span. The source adds before a signed `>` check despite naming the size
`u`, so high-cell requests can wrap and succeed. `HBW-TALIGN` can also cross a
configured limit that is not 64-byte aligned. The canonical 3 MiB geometry is
aligned, while an absent hosted region reports `(base,size)=(0,0)`; a
configured-zero emulator retains fixed `HBW_BASE` instead. These cases are
pinned discrepancies, not simulator-side normalization.

Byte-exact logical lines 2110 through 2388 add the complete external-memory
allocator and allocation-dispatch slice through `XBUF`. `EXT-MEM-BASE` and
`EXT-MEM-SIZE` dynamically read the bound SysInfo geometry. The unchanged
source owns bump and free-list state, 16-byte normalization, LIFO first-fit
reuse and splitting, the eight-byte public-allocation prefix, resize copying,
Bank-0 DMA routing, reset floors, XBUF publication, and present/absent status
output. No host allocation object replaces guest-visible links or metadata.

Qualification covers normal owned pointers. The source's bounds checks are
failure-atomic but do not prove allocation ownership, alignment, disjointness,
or single free; interior and repeated frees can corrupt or cycle the list.
Free-list bytes are also excluded from `XMEM-FREE` accounting. `FREE` treats
every address at or above `MEM-SIZE` as XMEM, reset leaves stale bytes and
pointers, `XMEM-TALIGN` may cross a nonaligned limit, and XBUF can leak an
allocation if constant publication faults before the floor advances. The
shared XMEM state is unsynchronized, and several raw/free/reset paths lack the
documented core-0 guard. These are explicit source-contract gaps rather than
hosted fixes.

The hosted and executable-emulator constructors use external size zero for an
absent region; RTL's `EXT_MEM_SIZE_PARAM=0` instead selects the full window up
to VRAM, while ordinary emulator sessions default separately to 128 MiB.
Hosted words report the profile's actual SysInfo geometry and do not erase
that configuration discrepancy.

Exact logical lines 2390 through 2423 now run KDOS's one-shot caller-backed
dictionary-index initializer. The semantic BIOS validates the complete
external span, emits exact 16-byte FNV/length/entry slots, rebuilds newest
first, upserts later shadows, rebuilds after numeric rollback, and exposes
status 0/1/2 plus the four public flags. Canonical 128 MiB XMEM reserves a
1 MiB/65,536-slot authoritative table; absent or sub-2,048-byte capacity leaves
it disabled, while exactly 2,048 bytes deliberately produces a protected
one-slot saturated fallback. Executable `2/` is logical despite its stale
assembly comment describing arithmetic shift.

The caller must reserve the table exclusively: BIOS geometry checks do not
prove allocator ownership or disjointness, and rebuild clears the supplied
span. Disable leaves old bytes, `DICT-INDEX@` is not a coherent multicore
snapshot in hardware, and KDOS's early DONE publication makes an otherwise
unreachable post-allocation status-1 failure nontransactional. Hosted
one-core execution preserves the visible table/state contract without claiming
hardware-cache timing or multicore seqlock behavior.

Exact logical lines 2425 through 2574 now run the complete KDOS userland
section. `DICT-BOUNDS!` consumes and validates one nonempty external interval
without imposing alignment or allocator-ownership policy; invalid geometry
enters the dictionary-fault callback before changing the old pair, and `0 0`
or `DICT-BOUNDS-OFF` disables without moving `HERE`. Unchanged
`USERLAND-INIT`, `ENTER-USERLAND`, and `LEAVE-USERLAND` then seal a
capacity-derived XMEM partition and move the same semantic dictionary between
Bank 0 and external RAM while retaining the global `LATEST` chain and side
index.

The source remains runtime-global and ordered rather than task-owned or
transactional. Corrupted public partition cells or concurrent transitions can
leave bounds and `HERE` disagreeing. A failed capacity calculation may retain
the `_U-AVAILABLE` scratch value; exotic positive reserve rounding can cross
the signed-cell boundary and is then rejected; a non-16-byte external end can
publish a misaligned XMEM HERE/floor; and pre-init `.USERLAND` labels the
absolute external end as its reserve because its limit cell is still zero.
These behaviors are pinned as source discrepancies, not repaired by hosted
policy.

A host-side budget or implementation error that escapes a dispatch which has
observed `RP@` marks that execution context non-reusable. The registration is
kept for the complete dispatch because unchanged KDOS pops a saved handler
cell immediately before restoring the `HANDLER` variable. This conservative,
fail-closed boundary covers that one-operation cleanup window and prevents a
stale guest handler from reviving abandoned continuations. A host escape now
clears hidden evaluator frames and unfinished compiler state. KDOS
module-registry transactions remain pending; the raw loader's exact cleanup
and pre-guard leak are qualified below. Ordinary source
`THROW` never escapes the outer public host boundary, including when it crosses
a nested host primitive's `execute`/`evaluate` call. Guest `RP!` remains a raw
aligned restore within its caller-owned stack span.

### Semantic evaluator and checked source

The hosted BIOS now exposes `EVALUATE`, the early `EVALUATE-CHECKED`,
`EVALUATE-FINISH`, `EVALUATOR-RESET`, `EVALUATOR-UNWIND`, `EVAL-STATUS`,
`EVAL-LINE`, `EVAL-COLUMN`, `EVAL-DEPTH`, `EVAL-THROW`, and `EVAL-TOKEN`.
Guest `EVALUATE` consumes one physical input's supplied bytes rather than
normalizing carriage returns and accepts at most 255 bytes. Direct LF-containing
input remains outside this primitive; KDOS `SOURCE-EVALUATE-CHECKED` owns
physical-line splitting. A 256-byte request is rejected before reading its
address. The ordinary one-core compiler and its control-flow stack persist
across successful calls, so a colon definition and its conditionals may span
several evaluator inputs.

The early checked entry returns status 0 for success, 1 for an undefined
token, 2 for an overlength input, and 3 for evaluator-depth exhaustion.
`EVALUATE-FINISH` returns status 4 when the persistent compiler or cross-line
control state is unfinished. `EVALUATOR-RESET` clears that compiler
bookkeeping without erasing the retained status/diagnostics. The five `EVAL-*`
cells are distinct, zero-initialized guest-memory cells, and `EVAL-TOKEN`
returns a stable `( addr len )` view backed by protected 256-byte guest storage.
Nested failure is sticky: an inner failure stops the remaining inner input and
the enclosing source tail, while preserving the first diagnostic.

A guest `THROW` caught outside `EVALUATE` deliberately leaves its logical input
frame recorded until `EVALUATOR-UNWIND` receives a valid prior-depth
checkpoint. Negative or above-current checkpoints do nothing; a valid target
drops every abandoned logical frame and republishes the resulting depth. A
host-side abort, budget exhaustion, or implementation escape instead performs
fail-closed cleanup of hidden evaluator depth and unfinished compiler state.
Nested evaluation consumes the active outer public step budget and cannot
acquire a fresh allowance.

Exact KDOS source now shadows the early checked entry with its `CATCH` wrapper.
A caught guest exception records its exact code in `EVAL-THROW`, unwinds to the
saved evaluator-depth checkpoint, and returns status 5 normally.
`SOURCE-EVALUATE-CHECKED` walks LF-delimited input, strips one terminal CR from
each physical line, skips blank physical lines, accepts a final line without LF,
stops at the first nonzero status, and calls `EVALUATE-FINISH` at ordinary end
of input. Caller-owned dictionary rollback followed by `EVALUATOR-RESET`
removes completed and unfinished work while retaining the failure diagnostic.

The evaluator remains runtime-global and nonconcurrent and makes no claim for
public `SOURCE`, `>IN`, or `STATE`, direct LF-containing `EVALUATE` input, or
interpret-time `[IF]`. The filesystem loader's narrower raw-source domain and
literal failure behavior are recorded below. The contiguous KDOS frontier now
ends at line 6724.

### KDOS source frontier

| Logical lines | Status | Purpose |
|---|---|---|
| 39–6724 | Contiguous qualified frontier | Ordinary bootstrap through diagnostics, crypto, allocation, dictionary/userland/Arena, semantic `IDLE`, Buffer and compute layers, checked storage and partitioning, legacy files, MP64FS cache/lifecycle/mutation/transfers/FDs, the KDOS checked whole-source compiler, nested two-extent filesystem `LOAD`, Application Loading, ANSI byte helpers, whole-file encryption, parent-byte navigation/mutation, the Documentation Browser, raw linked-header Dictionary Search, and the task registry/synchronous executor |
| 6725 onward | Next uncovered frontier | Timer Preemption begins at line 6725, followed by multicore dispatch and the remaining ordinary KDOS source |

The primary progress measure is the monotonically advancing contiguous
frontier, not the number of isolated fixtures. A later island is admitted only
when it validates a cross-cutting capability needed by the frontier. As the
semantic BIOS vocabulary becomes complete, first-failure source loading should
cross more definitions per slice, the frontier increments should grow, and
qualified islands should be absorbed until ordinary complete `kdos.f` is one
continuous load.

The bootstrap loader is not KDOS module-loader evidence. It has no filesystem
or dictionary transaction and must be shadowed by KDOS's ordinary `REQUIRE`.
The Arena checkpoint executes all 31 unchanged definitions across general
`ALLOCATE`/`FREE`, raw XMEM, and HBW backing. It covers caller-placed and
dictionary descriptors, exact-fit and failed bump allocation, reclaim/reset,
bare snapshot tokens, the four-entry scoped stack, and source-visible
diagnostics. Exact lines 2782 through 2796 then keep the definition open while
`[` enters interpretation state, translate only raw opcode byte zero into an
`Idle` operation, and restore compile state with non-immediate `]`. Executing
that operation detaches a runtime-owned continuation after the IDL boundary;
only a matching runtime-issued interrupt or DMA receipt resumes it. Source
evaluation and nested Python-host dispatch cannot yet suspend and fail
explicitly. Compiling `]` while already in compile state also fails closed
until public persistent `STATE` exists. Canceling a path that observed `RP@`
restores its return stack but marks the context non-reusable so a leaked guest
pointer cannot resurrect detached control.

The adjacent 189-line, 7,191-byte fixture is exact `kdos.f` lines 2797–2985
(SHA-256
`eb4d6d1bf072f854c667e86f428f49370bde4cd06e4770bd095d5f549906b2f1`).
It executes the source's newest-first linked registry with no fixed 16-buffer
cap, dictionary/HBW/XMEM/Arena constructors, field and byte-size queries,
fill/zero, current-base fixed-64-byte preview, enumeration, and Arena
unregistration. The hosted runtime does not replace these with host buffer
objects or make construction transactional.

Source discrepancies remain visible. `BUF-NTH` is unchecked;
`ARENA-BUFFER` gives its data only eight-byte alignment; `XBUFFER` loses a
free-list allocation address by recording `XMEM-HERE` first; and Arena
destruction unlinks a descriptor without reclaiming its link node or
undefining its now-dangling constant. `ARENA-RESET` does no unregistration,
and dictionary rollback after registration does not repair `BUF-HEAD` or
`BUF-COUNT`.

The next 124-line, 4,170-byte fixture is exact `kdos.f` lines 2986–3109
(SHA-256
`91d0fc5a15da85c31f9e4c4fcf17691c2bd32ba306b6b5bc338a7cf8b1ab96c4`).
It publishes six byte Buffer operations plus `BTMP-NTILES` scratch unchanged.
The hosted BIOS seam
retains TMODE/TCTRL and tile addresses, shares ACC/TSRC0/TDST with Field-ALU
words, processes exact complete mapped 64-byte spans, and counts completed
operations. Its integer ADD/SUB/MUL and reductions cover 8/16/32/64-bit lane
modes; these admitted KDOS words themselves always force unsigned-byte mode.

The source's physical-tail and loop behavior remains visible. Rounded-up
SUM/MIN/MAX reductions include bytes after a partial logical buffer; ADD/SUB
write complete tiles, take their count only from the leftmost stack argument
named `src1` (loaded into hardware TSRC0), and use global scratch. Multi-tile
B.MIN/B.MAX mistake the running byte extreme for the next TSRC0 address.
B.SCALE is scalar and wraps products modulo 256 rather than clamping. Empty
B.MIN/B.MAX return zero, while empty B.SUM/B.ADD/B.SUB/B.SCALE enter `0 DO`
and cannot complete normally before 64-bit loop-index wrap; an invalid memory
access may fault first. The semantic service accepts exact unaligned spans
because existing backends disagree about alignment and Arena supplies only
eight-byte alignment; crossing, wrapping, and MMIO spans fail closed.

The following 107-line, 2,869-byte fixture is exact `kdos.f` lines 3110–3216
(SHA-256
`cea60476207e132760c32cf2fb82773d6325d6d1895f0e7d73c40bf667b75065`).
It publishes seven FP16/BF16 Buffer words unchanged. FP ADD and MUL write
half-format lanes; SUM and SUMSQ reductions plus DOT publish raw binary32 bits
in ACC0 and clear ACC1--ACC3. Successful source words reset TMODE to zero and
those reductions leave TCTRL at one. They process full rounded-up physical
tiles, take the two-input operation count only from the leftmost stack argument
named `src1` (loaded into hardware TSRC0), and do not validate descriptor type,
width, an even byte count, or compatible sizes. Zero counts enter `0 DO`; a
tile-loop memory fault or budget fault before the final `0 TMODE!` leaves
FP16/BF16 mode active.

The hosted result path provisionally follows the decoded Python emulator,
including host-language per-tile SUM/SUMSQ before one binary32 pack and the
current FP16 subnormal-carry encoding defect. The native accelerator currently
falls back to Python for those reductions; its bypassed direct C++ arithmetic
and RTL use different orders. Python and active native TDOT use a binary64 loop
before packing, unlike RTL. With ACC_ACC the existing binary32 ACC0 is widened,
added to the tile subtotal in binary64, and repacked at the inter-tile rounding
point. Reserved-mode aliases and high-accumulator-word behavior also differ.
These remain recorded discrepancies rather than claims of hardware parity.
The source example `0 1 64 BUFFER` has the right physical byte count but does
not describe 32 two-byte elements; `0 2 32 BUFFER` does.

The next 538-line, 16,586-byte fixture is exact `kdos.f` lines 3217–3754
(SHA-256
`ec724b8ca6f6887a2c4ce724edf9612726cf04a48416c29c2eb3ed9448949e40`).
It publishes 109 definitions through ordinary source: 23 kernel descriptors,
six registered Buffer constants, and three populated pipelines plus their
registry/accessor/kernel/step words. Load-time state is `KERN-COUNT=23`,
`PIPE-COUNT=3`, and `BUF-COUNT=6`, with no UART output. All three demos run
their normal bound XTs and ordinary Buffer paths; `BENCH` reports deterministic
hosted work, not MP64 timing.

The hosted `OFF` word performs an exact zero-cell store. Hosted `CYCLES` is the
wrapping low 32 bits of a separate per-runtime semantic-work clock, shared by
that runtime's contexts and unaffected by `PERF-RESET`; raw Timer MMIO remains
unimplemented. Emulator/native perform the intended full 32-bit Timer accesses,
while current RTL SoC wiring exposes only `COUNT_LO` to `CYCLES` and accepts
only `COMPARE_LO` from `TIMER!`, an explicit backend discrepancy.

The source's limits and defects remain observable. Full kernel/pipeline
registries silently omit later entries after still allocating their descriptors
and constants. `P.ADD` silently ignores a full pipeline, while `P.CLEAR` leaves
stale XT cells behind. `kavg` is currently an identity copy, `kdelta` emits the
first input byte rather than zero, short `kpeak` zeroes its destination then
underflows, and `krms-buf` divides by zero for mean square one. The moving
average and convolution scratch buffers are fixed at 256 bytes; oversized
cases are documented but not executed because they overwrite later dictionary
state. Representative zero-sized kernel loops retain their unsafe `0 DO`
behavior.

The adjacent storage fixture is exact unchanged `kdos.f` lines 3755–4099:
345 lines, 11,424 bytes, and SHA-256
`e4d09d0801838fc9721ba68e39f2c5a5dbc139101c9c4a3489fb66cab9b248b1`.
It publishes all 97 definitions through `VOL-FLUSH`, including structured
storage iors, caller-owned 128-byte block-device descriptors, caller-owned
144-byte raw/bounded volume descriptors, generation-stale validation,
reference lifetimes, and relative I/O. Load time performs no disk operation;
it initializes only `STORAGE-COOKIE` explicitly, while the other construction
scratch variables receive their normal zero-filled dictionary bodies.

The hosted storage service executes only the public checked surface. One
runtime claims one service instance, checked calls serialize through the same
depthless lock 2 contract, and a request validates presence, required
capabilities, caller generation, count, LBA range, and one complete physical
DMA window before transfer. Read/write requests split at 255 sectors and
report confirmed whole sectors. In-memory media is intentionally ephemeral:
its successful flush is an ordering barrier, not durability evidence. A
path-backed flush writes the complete live image and calls host flush/fsync,
but real close/reopen persistence qualification remains deferred by the
pre-rich-terminal resource gate. Raw setup/command words, the storage MMIO
window, BUSY/rejection visibility, DMA cadence, injected controller faults,
controller completion timing/timeouts, RESET, and host-thread/media-management
races are not admitted by this slice. Foreign-owner contention for checked
lock 2 still returns the public `TIMEOUT` cause immediately. The composition
must serialize attach, detach, write-protection changes, and I/O as host
management operations rather than concurrent controller events. Attach,
replacement, and detach never flush the outgoing image implicitly.

The unchanged object layer retains its literal caller-ownership hazards.
Descriptor destinations must name complete writable, nonoverlapping extents
and begin zeroed or as the caller's original live object. Copying or forging a
live descriptor can unbalance block-device references; cookies and constructor
scratch are runtime-global, non-atomic KDOS state; and validators check
structure, identity, and slice bounds rather than proving the descriptor span
itself safe. Early software rejection preserves old per-device diagnostics,
while a submitted operation updates ior/completed and read/write LBA/count
fields; flush updates only ior/completed. `BD-WRITE` and `VOL-WRITE` also
return their saved read-only error before stale, range, or DMA checking. These
are source behaviors, not simulator repairs.

The adjacent partition fixture is exact unchanged `kdos.f` lines 4100–4669:
570 lines, 18,979 bytes, and SHA-256
`bf46ad3acc9deaf380ac4229fe9196219fc0111df8d8f5a6650ffa95fb766112`.
It publishes all 110 raw/MBR/GPT discovery definitions through `PART-SCAN`.
Load time performs no disk, CRC, or lock operation. Acceptance uses ordinary
checked storage and source-defined descriptors against raw, MBR, and dual-copy
GPT images, including a cross-sector entry and partial CRC tail, structured
failure cleanup, CRC capability/ownership errors, and a guarded media swap
during later metadata reads.

The newly admitted `W@`, `W!`, `L@`, and `L!` preserve native low-to-high
byte routing. Fetches retain their input address on a late fault; stores have
already consumed both inputs and retain any written low-byte prefix. Partition
callers must supply mutually disjoint live block, output-volume, and writable
workspace extents; unchanged source does not preflight or prove that geometry.
The adjacent storage-compatibility fixture is exact unchanged `kdos.f` lines
4670–4803: 134 lines, 4,127 bytes, and SHA-256
`7ba6cb19989623363d2e78ac45ae81b1b7e4bb2ad51864005bfbb35b1f768199`.
It publishes 24 definitions through `DISK-INFO` without opening or touching
media at load time. Focused acceptance covers singleton replacement and
explicit stale rebind, borrowed custom-volume selection, global transfer
diagnostics, selected and raw checked wrappers, abort preservation,
exact-sector Buffer save/load, and attachment reporting.

The unchanged compatibility source retains important caller obligations.
`STORAGE-OPEN` attempts and discards both close results before opening, so a
busy block can leave the raw singleton cleared without rollback; it also does
not clear `FS-OK`. `FS-VOLUME!` borrows rather than owns its selected
descriptor. Management plus diagnostics are unlocked global state, and the
three diagnostic cells are not a coherent snapshot under concurrent calls.
Both flush wrappers leave the previous completed count, while raw flush alone
does not clear `FS-OK` for a stale result. `B.SAVE` does not flush, and
`B.SAVE`/`B.LOAD` submit a sector-rounded span even though ordinary Buffer
allocation reserves only logical bytes. Hosted acceptance therefore uses an
exact-sector payload instead of masking the possible 511-byte overrun with
simulator padding. `DISK-INFO` reports only ambient attachment presence, not a
usable or current selected binding.

The adjacent file-abstraction fixture is exact unchanged `kdos.f` lines
4804–5003: 200 lines, 6,781 bytes, and SHA-256
`b022f3514605371f527a1e823b78ea26b5b09dad44198b4936272eaef1bb091b`.
It publishes 38 definitions through `FILES` without creating a descriptor,
touching media, or printing at load time. Focused acceptance executes the real
`FILE` defining word and silent eight-pointer registry cap, metadata and
current unsigned clamp behavior, ordinary capacity and zero-length paths,
complete head/full/tail file I/O, a late range abort after earlier sector
writes, and exact `F.INFO`/`FILES` output.

Legacy descriptors are permanent four-cell dictionary objects, not later
MP64FS pool entries. They reserve no sectors and capture no volume identity;
selection changes redirect them. Ordinary successful I/O requires
nonnegative, nonwrapping geometry contained in the selected volume and
complete caller spans. Unchanged source does not enforce that domain: seek is
unchecked, truncate can expose old bytes, bounds mix signed comparisons with
unsigned `MIN`/`MAX`, and file ranges can overlap or escape the volume.
Descriptor fields are per-object, but construction, truncate, I/O, and sector
scratch use unlocked globals and are non-reentrant. Multi-stage failures may
leave earlier bytes committed without advancing descriptor metadata, and no
file operation flushes or persists that metadata.

The adjacent MP64FS foundation fixture is exact unchanged `kdos.f` lines
5004–5134: 131 lines, 4,579 bytes, and SHA-256
`caf26787745bdf711a89130db7f8b30d45b0f9a63534b4ccb58a601bb2cea062`.
It publishes 32 definitions through `FIND-FREE-SLOT`. Load installs
provisional geometry and root `CWD`, reserves the three global cache windows,
and leaves `FS-OK` false without validating or touching storage. Focused
acceptance covers geometry derivation, the complete 65,536-bit bitmap window,
first-fit runs including the upper boundary, all packed field offsets, and the
128-entry free-slot scan.

The adjacent lifecycle fixture is exact unchanged `kdos.f` lines 5135–5217:
83 lines, 2,999 bytes, and SHA-256
`829268e2d06f11c19bda4a5fa0606e883fdf3ab4a3690a741f0cd2616ada4137`.
It publishes only `FS-LOAD`, `FS-SYNC`, `FS-ENSURE`, and `FORMAT`; loading the
slice performs no binding, I/O, flush, UART output, or filesystem-state
mutation. Focused execution now exercises the separately qualified native
`MP64FS-VALID?` through ordinary `FS-LOAD`, including dynamic bitmap geometry,
the six-read successful path, and progressive publication on late failure.

`FS-LOAD` clears `FS-OK`, destructively selects raw storage, validates, then
publishes superblock geometry, bitmap, and directory in order; only complete
success restores `FS-OK`, and it never resets `CWD`. `FS-SYNC` writes bitmap
then directory and flushes without writing the superblock. `FS-ENSURE` trusts
an already-true marker without checking attachment identity. `FORMAT` writes
new superblock, active bitmap, and directory metadata before flushing; only
flush success publishes `FS-OK` and root `CWD`. None of these multi-stage paths
rolls back earlier cache or media effects.

The adjacent listing fixture is exact unchanged `kdos.f` lines 5218–5285: 68
lines, 2,167 bytes, and SHA-256
`c3c831bc183ee999c8b5a0d1fb4edd169890be1e5fa44ad726d3025923fdb3b7`.
It publishes only `.FTYPE`, `DIR`, and `CATALOG`; loading the slice only
installs their definitions and inline strings, with no binding, storage I/O,
filesystem-state mutation, or UART output. Focused pathless execution lists
occupied direct children of `CWD` from the cached directory and counts free
bitmap bits over `[FS-DSTART, FS-TOTAL)`. `DIR` renders compact type names and
marks type 8 with `/`; `CATALOG` reports only the primary `DE.COUNT` extent.
All numeric fields use signed `.` in the caller's current `BASE`.

Hosted `.ZSTR` consumes its address before reading and immediately publishes
each nonzero byte until the first NUL. It has no hidden length bound, does not
emit the NUL, and retains already-published bytes if a later read faults. The
BIOS validator accepts an occupied 24-byte name with no terminator, so listing
qualification requires the canonical producer invariant of a NUL within that
field; otherwise unchanged `DIR` and `CATALOG` can read and print adjacent
entry bytes. `FS-ENSURE` also trusts an already-true `FS-OK`, so an absent or
replaced attachment can still produce a stale cached listing.

The adjacent lookup/mutation fixture is exact unchanged `kdos.f` lines
5286–5408: 123 lines, 4,020 bytes, and SHA-256
`a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028`.
It publishes five colon words and six zero-initialized scratch variables
through `RENAME`. Loading performs no clock read, parsing, cache or media
mutation, sync, or UART output. Focused execution uses pathless in-memory media
and the explicit runtime epoch; it is not file-backed durability evidence.

The hosted epoch register defaults to zero and changes only through explicit
host set/advance or admitted direct MMIO writes. Reading its low byte latches
all eight little-endian bytes, and `EPOCH@` reconstructs that latched `u64`.
There is no automatic scheduling, monotonic-host, or wall-time advance.
`TICKS@` applies signed `/ 1000`; `MKFILE` then stores only the low 32 bits in
`mtime`. `MS@`, uptime, calendar, alarm, control, and realtime RTC behavior
remain unqualified.

`FIND-BY-NAME` compares all 24 name bytes, not only the visible NUL-terminated
prefix, filters by `CWD`, and returns the first exact slot. Validator-accepted
post-NUL tails and duplicate names can therefore make visible names unfindable
or shadow a later entry. Admitted mutation requires a nonempty canonical name,
positive in-range primary run, valid current parent, non-directory file type,
and exclusive disjoint extents. `FS-LOAD` retains `CWD`; a stale parent from a
previous image can make a newly created entry fail the next validation.

`MKFILE` marks cached bitmap bits and constructs an empty entry before
`FS-SYNC`, without clearing the claimed data sectors. `RMFILE` clears both
extent bitmaps and the entry but never wipes payload; it is unsafe for a
directory's zero primary count and can free sectors still referenced by an
overlapping validator-accepted entry. `RENAME` changes only the name, does not
update `mtime`, rejects a same-name rename, and an empty replacement makes the
entry invisible without freeing its extents. Every path mutates cache before
the bitmap/directory/flush sequence, so a later failure retains cache and may
retain earlier media effects; retry can short-circuit against that cache.

The parser is likewise unchanged: an unavailable-filesystem return occurs
before `MKFILE`, `RMFILE`, or `RENAME` consumes its name tokens, and an
old-name miss in `RENAME` leaves the proposed new token to the outer evaluator.
These defects are recorded, not treated as a safe command domain.

The exact unchanged `CAT` fixture is `kdos.f` lines 5409–5436: 28 LF lines,
838 bytes, SHA-256
`e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23`,
and Git blob `2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5`. It defines zero-initialized
`CAT-SLOT` and `CAT`. Loading only installs those dictionary entries and inline
strings; it performs no parse, filesystem/cache/media access, diagnostic
update, or UART publication.

Execution checks filesystem availability before parsing, then miss, then
`DE.USED = 0`; those exits respectively leave the filename token unconsumed,
print `Not found`, or print `(empty file)`, and none reads file data. A
nonempty match reads the complete primary `DE.COUNT` extent at `DE.SEC` into
the unreserved address at `HERE` without advancing `HERE`, even when only a
short prefix is used. It then publishes exactly `DE.USED` bytes: LF becomes
CRLF, every other byte including CR, NUL, and ESC is emitted raw, and no final
newline is added.

The admitted path requires a stable generation, a canonical matched
non-directory entry, a small positive primary extent, no secondary extent,
`DE.USED <= DE.COUNT * 512`, and a complete unused mapped span beginning at
`HERE`. The source neither reserves nor bounds that scratch and does not
inspect the file type. It ignores
the secondary extent even though BIOS validation permits one, so a valid file
whose used bytes extend beyond the primary extent publishes stale unread bytes
from after the DMA span. A generation-bound read failure aborts before content
publication, while any earlier partial DMA prefix remains. `CAT-SLOT`, parser
buffers, storage diagnostics, and the `HERE` scratch are global and unlocked.

The adjacent free-space fixture is exact unchanged `kdos.f` lines 5437–5471:
35 LF lines, 984 bytes, SHA-256
`6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c`,
and Git blob `1884c81ba2b8aa48082d472250f13a2265fd1def`. It adds zero-initialized
`LF-BEST` and `LF-RUN`, followed by `FS-LARGEST-FREE` and `FS-FREE`. Loading
only installs those four dictionary entries and inline strings; it performs no
filesystem ensure, bitmap/directory scan, cache or media access, diagnostic
update, or UART publication.

`FS-LARGEST-FREE` has no `FS-OK` gate. It resets its global scratch and scans
the cached bitmap over `[FS-DSTART, FS-TOTAL)`, including a trailing free run.
`FS-FREE` first ensures and checks the filesystem; an unavailable filesystem
prints `No filesystem` without scanning or changing the largest-run scratch.
Otherwise it separately counts cached free sectors, invokes the largest-run
scan, and counts every directory slot whose `name[0]` is nonzero. That last
number is global across all parents and includes directories despite the
printed `files` label. The report publishes sectors, `sectors * 512` bytes,
largest run, occupied count, and the 128-slot maximum with signed `.` in the
current `BASE`.

The reporting domain requires validator-approved positive geometry and
complete cache spans. A direct largest-run call does not establish that state,
and invalid ordinary-`DO` bounds remain excluded. `FS-ENSURE` trusts an
already-true `FS-OK`, so detached or replaced media can leave stale cached
numbers eligible. The total, largest, and occupied scans are separate,
global, and unlocked rather than one coherent allocation snapshot. This is
observability only, not allocator, ownership-validation, repair, compaction,
or persistence qualification.

The Buffer-I/O fixture is exact unchanged `kdos.f` lines 5472–5514: 43 LF
lines, 1,317 bytes, SHA-256
`7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104`,
and Git blob `8b4645f16c7ac2f21036282a896b7ede6bad16b0`. Its exact source-order ledger
is variable `SB-SLOT`, variable `SB-DESC`, colon `SAVE-BUFFER`, variable
`LB-SLOT`, variable `LB-DESC`, and colon `LOAD-BUFFER`: six definitions total,
with all four variables initialized to zero. Loading otherwise only installs
the bodies and inline strings. It does not ensure or parse, dereference a
Buffer, touch cache or media, change diagnostics, flush, or publish output.

Both words ensure and reject an unavailable filesystem before storing the
descriptor or parsing the filename; that exit drops the descriptor, leaves the
name token for the outer evaluator, and prints `No filesystem`. A lookup miss
occurs after the descriptor and `-1` slot have been saved in global scratch,
but before descriptor dereference or I/O. `SAVE-BUFFER` adds its `create with
MKFILE first` hint; `LOAD-BUFFER` does not.

On a match, both words transfer the complete primary allocation
(`DE.COUNT * 512` bytes at `DE.SEC`) and ignore `DE.USED` for transfer length
and `DE.EXT1-SEC`/`DE.EXT1-CNT` entirely. `SAVE-BUFFER` orders a
generation-bound payload write first, then stores the low 32 bits of `B.LEN`
as cached `used_bytes`, then calls the ordered, nontransactional `FS-SYNC`.
It retains the entry's `mtime`, CRC, flags, name, type, parent, and extent
fields; in particular it does not recompute the CRC or timestamp. A sync or
flush failure can therefore leave payload and some metadata written with the
cache already changed, and a payload failure can leave a partial media prefix.
`LOAD-BUFFER` reads the complete allocation, including padding after
`DE.USED`, into `B.DATA`; it leaves `B.LEN`, the rest of the Buffer descriptor,
and all directory metadata unchanged. A failed read can leave a partial Buffer
prefix.

The source stores and prints `B.LEN`, not `B.BYTES`; for width greater than
one that is an element count mislabeled as bytes, even though full sectors are
transferred. Safe use requires a stable mounted generation, a canonical
matched non-directory file with one valid positive primary extent and no
secondary extent, and a valid Buffer descriptor whose `B.DATA` backs at least
the full allocation (readable for save and writable for load). For ordinary
constructed Buffers, the unambiguous domain is byte width with
`B.LEN = B.BYTES = DE.COUNT * 512`; `B.LEN` must also fit the intended unsigned
32-bit `used_bytes` field, and save requires a writable selected volume. The
source does not enforce per-entry read-only or system flags. Success messages
print the saved `B.LEN` or cached
`DE.USED` with signed `.` in ambient `BASE`. The `SB-*`/`LB-*` cells, parser
state, filesystem cache, and storage diagnostics are global and unlocked; the
words add no transaction or filesystem-level lock.

The adjacent FD fixture is exact unchanged `kdos.f` lines 5515–5610: 96 LF
lines, 3,397 bytes, SHA-256
`16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78`,
and Git blob `e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9`. Its 14-definition
source-order ledger is `FD-MAX`, `FD-SLOT-SZ`, `FD-POOL`, `FD-SLOT`,
`FD-ALLOC`, `(FCLOSE-NOFS)`, `FCLOSE`, `FD-FILL`, `OP-SLOT`, `(OPEN)`,
`OPEN`, `F.SLOT`, `FFLUSH`, and `(FCLOSE)`. Loading allocates and zero-fills
the fixed 16 × 72-byte, 1,152-byte pool and zero-initializes `OP-SLOT`.
It creates `FCLOSE`, first binds it to `(FCLOSE-NOFS)`, creates and binds
`OPEN` to `(OPEN)`, then rebinds the same `FCLOSE` deferred word to final
`(FCLOSE)`. The final targets are therefore `(OPEN)` and `(FCLOSE)`. Loading
does no ensure, parse, cache or media I/O, synchronization, diagnostic update,
or UART output.

Each 72-byte slot begins with an eight-byte in-use header. The returned
descriptor is `slot + 8` and contains start sector at `+0`, maximum primary
sectors at `+8`, used bytes at `+16`, cursor at `+24`, directory slot at `+32`,
secondary start/count at `+40`/`+48`, and a reserved cell at `+56`. `FD-ALLOC`
scans slots lowest first, marks the first zero header `-1`, and returns its
descriptor; exhaustion returns zero. Allocation and either close body clear
no descriptor payload. `FD-FILL` snapshots the seven fields through secondary
count from the cached directory, resets cursor to zero, and deliberately leaves
the reserved cell unchanged. It begins as zero but remains retained across
close and reuse. The named `(FCLOSE-NOFS)` body also remains directly callable:
zero is a no-op, while nonzero clears only the preceding header and always
bypasses persistence.

`OPEN` ensures and gates availability before parsing. Failure returns zero and
prints `No filesystem`, leaving the name token and `OP-SLOT` unchanged. A miss
stores `-1` in `OP-SLOT`, prints the parsed name, and returns zero before pool
allocation. Exhaustion retains the matched slot in `OP-SLOT`, prints `No free
FD slots`, and returns zero. Success allocates the lowest slot and snapshots
the cached entry with cursor zero, without payload I/O or output. With
`FS-OK` already true the whole lookup/open path is cache-only; `FS-ENSURE` may
still perform `FS-LOAD` when it begins false.

The source does not reject directories, types, or flags, bind a descriptor to
a storage generation, revalidate cached geometry, prevent multiple opens, or
coordinate independent used/cursor snapshots. An already-true stale `FS-OK`
remains trusted, the first exact duplicate still shadows later entries, and
out-of-order flushes of multiple descriptors can overwrite newer `used_bytes`.
Although `FD-FILL` copies secondary-extent coordinates, this slice qualifies no
multi-extent read or write behavior.

`FFLUSH` checks `FS-OK` before descriptor access. A false marker drops the
argument and prints `FS not loaded` without cache or media effects. Otherwise
it stores only the low 32 bits of `F.USED` into the cached entry selected by
`F.SLOT`, then calls the existing nontransactional `FS-SYNC`; it does not write
file payload or update `mtime`, CRC, flags, or any extent. It validates neither
the descriptor/directory slot nor used against allocated capacity; `L!` simply
truncates the cell to low u32. A sync failure occurs after the cache mutation
and can leave partial bitmap/directory/flush effects.

Final `FCLOSE` ignores zero before reading `FS-OK`. With a true marker it calls
`FFLUSH` and releases the slot only after that call returns; a flush abort keeps
the header allocated while retaining the changed cache and any partial media
effects. With a false marker it silently skips persistence and releases the
slot, discarding the dirty used count. Successful release clears only the
header, retaining every descriptor and reserved cell and leaving file payload
untouched. No operation validates that an address is a currently allocated
pool descriptor. Reused addresses create an ABA hazard in which a stale handle
can flush or close a new occupant; pool headers, descriptor cells, `OP-SLOT`,
parser state, cache, and deferred bindings are global and unlocked.

The adjacent loader fixture is exact unchanged `kdos.f` lines 5611–5944: 334
LF records, 11,337 bytes, SHA-256
`efad4e40860bc7cdc484b58ac652d9b7286541a7adfdb156d4ae66a3f73ba9fe`,
and Git blob `8fd4577b4ac2128934672eb123ca78bf88468d52`. Its exact 50-definition ledger
installs four loader globals, a 16 × 56-byte nesting stack, evaluator-depth and
transaction accessors, three initially-no-op deferred transaction actions,
two-extent read helpers, relative-path scratch and traversal, evaluator status
constants, the KDOS `EVALUATE-CHECKED` shadow, the whole-source checked walker,
and final `LOAD`. Loading the fixture performs no filesystem or storage work.

`LOAD` must be reached through an active source cursor because `PARSE-NAME`
consumes its filename from that cursor. It calls `FS-ENSURE` before parsing; a
false marker prints `No filesystem` and leaves the would-be filename for the
enclosing interpreter. Once mounted, a missing file, empty file, or allocation
failure restores the saved loader globals and CWD without reads or transaction
hooks. Each active frame saves `LD-BUF`, `LD-SZ`, `LD-CUR`, `LD-LEN`, CWD, an
evaluator-depth checkpoint, and a future transaction head. Nesting is bounded
at 16 frames by the source-defined `ABORT"`.

Within the admitted domain, every path component and final name is at most 23
bytes, total path storage is at most 127 bytes, cached metadata is valid and
stable, and source is LF-delimited with no retained CR bytes. Every physical
line is at most 255 bytes and compiler state is complete at file end. `LOAD`
allocates the combined sector-rounded primary and secondary extents, reads and
concatenates both complete runs, but evaluates only cached `DE.USED`. The
allocation padding is therefore represented and transferred without becoming
source. Nested relative loading observes the containing directory and restores
each caller's CWD and walker globals. A final source line need not end in LF.

The transaction actions remain bound to `_LD-TXN-NOOP` until the later module
registry replaces them. Successful nested loads call commit and then release/
after-release from inner to outer. A guest `THROW` during walking is caught,
unwinds the evaluator checkpoint, calls rollback, releases/restores, calls
after-release, and rethrows. That path is reusable and leak-free, but an early
definition remains published because the current rollback action is literally
a no-op. This is pre-registry behavior, not evidence of dictionary
transactionality.

Several source defects deliberately remain visible. `_RESOLVE-PATH` prints an
intermediate-component error but returns no failure status; `LOAD` then looks
up the rejected component left in `NAMEBUF` and can load it instead of the
requested final path. Component and final-name copies are not length-bounded,
which is why the admitted path domain above is narrow. Hosted semantic lookup
also cannot reproduce native linked-header corruption caused by an overflowing
copy, so oversized paths are not a differential claim.

`_LD-READ-SLOT` executes before `_LD-WALK-GUARDED`. A read abort can therefore
leave the sector-rounded allocation live, `_LD-SP` advanced, CWD resolved, and
`LD-BUF`/`LD-SZ` replaced; no transaction action runs. Focused acceptance pins
that exact second-extent stale-media outcome only in a disposable runtime.
The filesystem marker and block diagnostics still fail closed, and the
storage lock is released.

Finally, `_LD-WALK` uses raw `EVALUATE`, retains CR, never reads `EVAL-STATUS`,
and never calls `EVALUATE-FINISH`. An undefined or overlong line can be skipped
while later lines execute and clear the diagnostic, and unfinished compiler
state can escape a nominally successful load. `LOAD` also does not reject
directories or inspect entry flags. These are unchanged KDOS behaviors; the
simulator does not insert a checked host loader behind them. Loader, resolver,
parser, evaluator, filesystem-cache, and transaction state are global and
unlocked.

The Application Loading fixture is unchanged `kdos.f` lines 5945–6059: 115 LF records,
2,892 bytes, SHA-256
`1c671d6f3677d9fb65e7c5b20a6af1b3d10323b28b5abb10d827cd80a58e5bb2`,
and Git blob `c95aa1a3385d10587ed42292328b0c7c323e702f`. It publishes the five
Application Loading words and six ANSI helpers without load-time I/O or MPU
mutation. `_APP-MPU-ON` overwrites the inert window with Bank 0 plus external
memory, `_APP-MPU-OFF` zeros it, and ordinary `APP-EVAL` observes that active
state before teardown. An externally caught guest `THROW` bypasses teardown
and evaluator unwind, leaving the active limit and abandoned depth visible.

`APP-LOAD` uses direct current-directory lookup rather than `LOAD`'s slash
resolver, but reuses the real loader frame, heap allocation, primary/secondary
extent reads, evaluator, and transaction hooks. The ordinary qualified domain
requires LF termination, no retained CR, at most 255 bytes per physical line,
complete compiler state, and zero net data-stack effect per line. The last two
bounds are not cosmetic: the raw walker never finishes or checks evaluator
status and exposes four internal cells beneath each evaluation. Its scanner
also compares the line offset against the buffer address rather than remaining
length, so an unterminated final line can execute sector padding beyond
`DE.USED`. Focused coverage preserves that defect with deterministic padding
rather than adding simulator-only bounds. Clean early failures leave prior MPU
state alone; read aborts still occur before the guard and inherit `LOAD`'s
allocation/frame leak. The ANSI helpers are ordinary UART byte publishers, not
a rich-terminal path.

The following exact fixture is unchanged lines 6060–6200: 141 LF records,
5,298 bytes, SHA-256
`35a8f33b51da4e3a319f193e0c709a876207f940923637d0f56b0f8160c7f574`,
and Git blob `ed442875e780976b10553721137e515e3742ddcb`. Its two CREATE bodies,
constant, six variables, and seven colon words publish the complete
filesystem-encryption family without running AES, allocating heap memory, or
touching storage at load time.

In the qualified synchronous one-core domain, a live matching descriptor names
one positive primary contiguous extent with room for the 16-byte-rounded data
plus one tag, `FS-OK` and media remain stable, and the shared AES engine begins
idle in AES-256 mode. `FENCRYPT` allocates two Bank-0 DMA buffers, reads whole
plaintext sectors, encrypts one whole-file padded span, writes ciphertext and
tag, sets only flag bit 2, syncs bitmap and directory, flushes, and frees both
buffers. `FDECRYPT` authenticates before any media/cache write; wrong-key
failure performs only its ciphertext read and frees both buffers, while success
writes plaintext, clears only bit 2, syncs, and frees. Focused acceptance uses
an external AES-GCM byte oracle and proves logical roundtrip, exact
storage-command counts, descriptor preservation, heap recovery, and the
repeated ciphertext
and tag produced when identical plaintext is encrypted again in the same slot
after decrypt/flag-clear. A direct call while flagged is only a no-op; changed
plaintext after flag-clear still dangerously reuses the nonce/keystream.

This is compatibility evidence, not a safe new storage design. `_FE-MKIV`
encodes only the directory slot plus four zeros, so slot reuse repeats a GCM
nonce; metadata is not AAD, and the source neither checks that `FS-KEY!` ran nor
forces AES-256 mode or checks AES status after encryption. Its apparent padding
can contain preexisting disk slack, secondary extents are ignored, and decrypt
omits encrypt's capacity check.
Payload and metadata writes are nontransactional, post-allocation aborts leak
unwiped buffers, and scratch cells retain freed addresses. The second-allocation
failure branch returns `0 -1`; nonencrypted decrypt returns 0 despite its prose,
and encrypted empty files remain flagged. A late flush-failure oracle preserves
the already-written ciphertext and directory flag while proving both buffers
remain allocated. No durability, crash-consistency, stale-descriptor,
concurrency, or secure nonce-management claim follows. The wrapper also ignores
MP64FS policy flags such as readonly; the lower storage path nevertheless
enforces media generation, volume bounds, and device write protection.

The next exact fixture is unchanged lines 6201–6296: 96 LF records, 3,082
bytes, SHA-256
`dc7f065cfac1fc3eb6efd1de7f4b0f472ff40e66fa14666e1087c18047e1d6c8`,
and Git blob `b964ca87a1af44e54b22abd25116edd2a7e2a853`. It publishes the raw
64-byte `_PWD-STK` body and the four parent-byte navigation words without
initializing that body or touching CWD, parser, filesystem/cache/media, RTC,
diagnostics, locks, or UART state at load time.

Within a stable validator-approved mounted cache and an acyclic parent tree,
`PWD` prints root or at most the eight components nearest CWD, in root-to-leaf
order within that retained suffix and with leading/trailing slashes. `CD`
recognizes exact `..` and `/`; otherwise it resolves only one direct type-8
child component and does not invoke the path resolver used by `LOAD`. `MKDIR`
clears the lowest free
48-byte slot, writes its zero-padded component, type, parent, and epoch-seconds
mtime, then syncs the unchanged bitmap and changed directory. Empty `RMDIR`
clears one direct type-8 child and performs the same sync. Neither operation
allocates or frees payload sectors or updates the parent mtime.

Focused acceptance pins exact path/diagnostic bytes, the eight-component PWD
display limit, volatile-only CD transitions, lowest-slot metadata, unchanged
bitmap/payload state, three-command successful syncs, rejection immutability,
and cache/media retention before an unsupported late flush. It also preserves
the source's nonempty-RMDIR stack discrepancy: that rejection returns with its
target slot still present.

The safe domain excludes parent cycles and non-NUL 24-byte names: metadata
validation accepts both, while `PWD` respectively loops forever or lets
`.ZSTR` read beyond the name field. `MKDIR` accepts an empty token as an
invisible logically free entry and permits reserved `..` and `/` names that
ordinary `CD` cannot reach. A failed filesystem gate occurs before operand
parsing, and validator-accepted duplicate siblings are first-slot-wins.
Mutation ignores MP64FS policy flags and is nontransactional; CWD,
NAMEBUF/PATHBUF/PN-LEN parser state, `_PWD-STK`, and cache state are global and
unlocked. Removing a directory also does not invalidate loader/REQUIRE CWD
snapshots that still name it.

The earlier low-level helper domain is validator-approved geometry, positive
run counts, in-range sectors and slots, complete cache spans, and structurally
valid directory entries. Those helper words do not gate on `FS-OK` or validate
their inputs. `FIND-FREE` only reports a run and shares `FF-*` scratch;
`FIND-FREE-SLOT` inspects only `name[0]`. Canonical producers zero all 48
bytes of a free slot, but executable BIOS validation also uses only
`name[0]`; stale tail bytes are accepted. Invalid ordinary-`DO` bounds can
traverse the 64-bit cell space, so acceptance does not execute them.

The next exact fixture is unchanged lines 6297–6427: 131 LF records, 3,945
bytes, SHA-256
`442e5e39598d71a589bf19d6345c5bb042d678ba9f51607a878ae5030fbdcee6`,
and Git blob `242fc879957ba14f3a00b3284e8af921a4fa365c`. Its 13-definition
ledger contains the two file-type constants, raw 512-byte `DOC-BUF`, zeroed
`DOC-LINES`, `PAGE-LINES`, and the eight browser colon words. Loading publishes
that state without parsing, filesystem/media access, FD allocation, input,
UART output, or synchronization.

In the qualified domain, `.DOC-CHUNK` emits every byte except LF unchanged,
maps LF to CRLF, and consumes one queued key after each twentieth LF—even when
that LF is the final byte. The count carries across chunks until a prompt and
`SHOW-FILE` resets it, reads in 512-byte calls from the descriptor's current
cursor through logical EOF, and consumes but does not close the descriptor.
`TOPICS` and `LESSONS` globally list cached type-4/type-6 names. `DOC` and
`TUTORIAL` are identical current-directory `OPEN` wrappers, while `DESCRIBE`
globally selects the lowest-slot type-4 entry whose complete zero-padded name
matches the parsed token case-sensitively. Successful wrappers close through
ordinary `FCLOSE`, so even read-only browsing rewrites bitmap/directory state
and flushes media.

This source is deliberately not strengthened. `DOC` and `TUTORIAL` do not
check type, encryption, or CRC and therefore publish any selected logical
payload, including ciphertext and raw NUL/ESC/control bytes. Listings ignore
CWD while named opening does not, and `SHOW-FILE` begins at an incoming cursor
rather than necessarily at byte zero. Legacy `FREAD` ignores secondary extents
and can expose neighboring sectors. `OPEN-BY-SLOT` trusts its slot and mounted
state; interruption or I/O/sync failure can leak an FD after partial output or
media effects. Open failure in `DOC`, `TUTORIAL`, and the final `DESCRIBE` path
returns a stray zero despite the declared clean stack; a no-filesystem
`DOC`/`TUTORIAL` failure also leaves the operand unparsed. Zero or malformed
`.DOC-CHUNK` spans and non-NUL directory names remain outside the safe domain.
The browser's buffers, counter, FD pool, parser, cache, and UART streams are
global and non-reentrant.

The following exact fixture is unchanged lines 6428–6510: 83 LF records,
2,682 bytes, SHA-256
`c1c7be64fd2d1c86465edec8f0fd6922c2742c6b77be9267dc7638f7eeb3ce5a`,
and Git blob `8335b7ef5566340e7fa1115de27fec9c75f6ae97`. It publishes six
colon definitions and eight zero-initialized scratch variables, advancing the
hosted dictionary by 398 bytes without executing a search, parsing a pattern,
or touching UART, filesystem, platform-service, task, or transient `WORD`
state.

`ENTRY>LINK` and `ENTRY>NAME` directly consume the guest-visible header layout:
link at `+0`, flags/length at `+8`, and spelling at `+9`, with the immediate bit
masked from the low-seven-bit length. `ICONTAINS?` performs the unchanged
nested `I`/`J` loops and folds only ASCII `a`–`z`. `WORDS-LIKE` stores its
count, current header, and transient counted pattern globally, walks every raw
header newest-first, and reports shadowed duplicates as separate matches;
`APROPOS` is an exact alias. `.RECENT` walks at most its signed-positive count.

The safe domain is one stable, mapped, acyclic linked dictionary with canonical
ASCII headers, ordinary nonnegative lengths and mapped spans, enough live
dictionary-tail room for `WORD`, a reasonable bounded traversal budget, and
synchronous non-reentrant use. The raw helpers validate nothing. A malformed
length can print beyond a header, an invalid link can fault after partial UART
output, and a cycle makes unbounded `WORDS-LIKE` nonterminating; `.RECENT` is
still bounded by its count. Global `IC-*`/`WL-*` scratch and the transient
pattern bytes remain changed after return or failure, with `WL-PA` left pointing
at `HERE`. Raw header mutation is observed by these walkers even though hosted
ordinary lookup retains its semantic metadata authority.

The following exact fixture is unchanged lines 6511–6724: 214 LF records,
6,935 bytes, SHA-256
`cc28cfab7033390f4efc885cc043feafecc136e913aa34cc6338f7ad1b6a1f4c`,
and Git blob `ccdee7bbf513495f25eb77ad4c0f13f63b07532c`. It publishes five task-state
constants, nine variables, 24 colon definitions, and deferred
`CORE-CHECKPOINT`: 39 definitions. Load initializes the scalar state, sets
`TIME-SLICE` to 50,000, and binds the checkpoint to its boot action. It runs no
task and performs no timer, UART, IDL, filesystem, or stack-switch operation.
Raw variable storage is 2,175 bytes plus the deferred action cell. In
particular, `TASK-TABLE` is 64 bytes, while `VARIABLE TASK-STACKS 2047 ALLOT`
reserves 2,055 bytes rather than the source comment's 2,048.

This prefix is qualified as a task registry and synchronous
run-to-completion executor, not as cooperative multitasking. `TASK` and
`SPAWN` append 48-byte descriptors and record a nominal data-stack midpoint,
but neither `RUN-TASK` nor any other word installs `T.DSP`; `T.RSP` and
`T.NAME` remain zero. Every XT executes inline on the caller's data stack,
return stack, loop frames, task identity, and exception state. `FIND-READY`
always selects the lowest READY table slot and never consults priority.
`YIELD` only marks the current descriptor DONE and returns to the task body,
so code after it continues. `SCHEDULE` therefore executes each selected XT to
return, not until a resumable yield, and leaves `CURRENT-TASK` stale.

The unchanged bounds and failure behavior remain visible. DONE slots are not
reclaimed and `TASK-COUNT` is monotonic. A ninth `TASK` still appends an orphan
descriptor and publishes its constant; a ninth `SPAWN` appends an orphan and
increments `SPAWN-COUNT`. Their nominal stack pointer is beyond the declared
arena. Even admitted descriptors point only 128 bytes above the lower edge of
their nominal downward-growing slot, and no return-stack arena exists. Late
name publication or task execution failure retains prior registry, status,
`CURRENT-TASK`, and `SCHED-RUNNING` mutations. Descriptor arguments and public
count/table state are unchecked. `T.FREE`, `T.BLOCKED`, saved stack fields,
names, and priority ordering are otherwise inert in this block.

The provisional checkpoint consumes only a manually set `PREEMPT-FLAG` and
then calls the same non-suspending `YIELD`; it neither samples a timer nor
switches tasks. The next slice begins with Timer Preemption at line 6725 and
requires real retained timer-control state before the unchanged words can be
loaded honestly.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
