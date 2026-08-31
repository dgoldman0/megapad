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
- BIOS-compatible unaligned `@`, `!`, and `+!` access, low-byte `C!`, byte
  `FILL`, and full-cell `XOR` over that shared address space, plus the
  arithmetic and comparison words needed by unchanged source;
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
- hosted UART output for the BIOS numeric printer, complete-task `ABORT`, and
  the stable execution-token behavior needed by source-defined `DEFER`/`IS`;
- a shared bit-exact six-mode CRC value model with simulator-owned checked
  transaction state, coherent SysInfo capability discovery, exact byte/cell
  feeds, raw/final release, and source-visible status behavior;
- a per-runtime pseudo-BIOS diagnostic profile with persistent semantic-work
  accounting, retained non-destructive BIST observations, a real four-operation
  tile value self-test, and logical no-cache controls/zero cache counters;
- a retained one-core semantic tile service for four integer lane widths,
  wrapping/saturating ADD/SUB, signed-aware SUM/MIN/MAX, low-byte control
  registers, completed-operation accounting, and the ACC/TSRC0/TDST state
  shared with the hosted Field ALU;
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
  verification, and its dictionary/stack/heap proximity guard; and
- an exact-record bootstrap loader that supplies a shadowable `REQUIRE` before
  KDOS exists, with nested budgets, cycle detection, and registry-only failure
  cleanup.

This is deliberately not yet a complete MegaForth environment. Additional
task stack arenas and cooperative scheduling remain pending. The IDL seam
blocks and resumes one compiled-word dispatch; it is not `PAUSE`, task
round-robin, interrupt-vector delivery, DMA timing, or a device scheduler.
Persistent compiler state across evaluator calls, public `STATE`, the BIOS
evaluator surfaces, clocks, complete UART/MMIO service, media, and an ordinary
complete KDOS load also remain. The simulator does not execute ROMs, MP64
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
now admitted; the later transactional evaluator remains pending. Hosted
Bank-0 relocation still refuses to move below the semantic dictionary's
initial start even though native raw `ALLOT` has no equivalent lower-bound
check. That pre-existing divergence is outside the userland transition and is
not presented as native equivalence.

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
task cancellation exists.
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
stale guest handler from reviving abandoned continuations. Transactional
context recovery belongs to the pending evaluator/rollback slice. Ordinary
source `THROW` never escapes the outer public host boundary, including when it
crosses a nested host primitive's `execute`/`evaluate` call. Guest `RP!`
remains a raw aligned restore within its caller-owned stack span.

### KDOS source frontier

| Logical lines | Status | Purpose |
|---|---|---|
| 39–3109 | Contiguous qualified frontier | Ordinary bootstrap through diagnostics, crypto, hybrid exchange, HBW/XMEM allocation, dictionary indexing, userland partitioning, the complete Arena allocator, semantic `IDLE`, Buffer construction, and the byte tile/scalar Buffer operations; blank separators have no definitions |
| 3110 onward | Next uncovered frontier | FP16/BF16 Buffer operations begin next; `FP16-MODE` in `F.SUM` at line 3127 is the first unadmitted word |

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
operations. Its integer ADD/SUB and reductions cover 8/16/32/64-bit lane
modes; the admitted KDOS words themselves always force unsigned-byte mode.
FP16/BF16 remains the next explicit capability boundary.

The source's physical-tail and loop behavior remains visible. Rounded-up
reductions include bytes after a partial logical buffer; ADD/SUB write complete
tiles, trust only `src1`'s count, and use global scratch. Multi-tile B.MIN/B.MAX
mistake the running byte extreme for the next TSRC0 address. B.SCALE is scalar
and wraps products modulo 256 rather than clamping. Empty B.MIN/B.MAX return
zero, while empty B.SUM/B.ADD/B.SUB/B.SCALE enter `0 DO` and cannot complete
normally before 64-bit loop-index wrap; an invalid memory access may fault
first. The semantic service accepts exact unaligned
spans because existing backends disagree about alignment and Arena supplies
only eight-byte alignment; crossing, wrapping, and MMIO spans fail closed.
Later slices continue the same contiguous prefix through FP Buffer operations
toward the persistent evaluator, ordinary checked module-loader surface, and
deterministic cooperative task scheduler.

This branch stops after the semantic BIOS and ordinary KDOS source load are
credible. It does not load or implement `rich-terminal.f`; that later work
must resynchronize with the then-current rich-terminal vertical.

See [`docs/simulator-contract.md`](../docs/simulator-contract.md) for the
normative compatibility surface and first implementation sequence.
