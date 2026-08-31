# Hosted Source Simulator Contract

Status: normative for the hosted source simulator

Contract ID: `MEGAPAD-HOSTED-SOURCE-1-2026-08-30`

This contract fixes the boundary between MegaPad's architectural emulator and
the hosted source simulator.  The simulator exists to run ordinary
MegaForth, KDOS, and Akashic source quickly.  It is a second execution backend,
not a relaxed claim that source execution proves the MP64 machine.

The architectural emulator remains the reference for every machine-level
claim.  When the two backends disagree about an observable claimed here, the
simulator is wrong unless the difference is explicitly listed as a nonclaim.
Performance is a goal and never evidence of compatibility.

## 1. Backend and directory ownership

The repository has three host-code ownership domains:

| Directory | Owns | Must not own |
|---|---|---|
| `emulator/` | MP64 instruction execution, architectural devices, machine scheduling, DBT/native acceleration, machine snapshots, and emulator-specific applications | Hosted Forth semantics or backend-neutral protocol policy |
| `simulator/` | Hosted Forth compiler/runtime, semantic dictionary and execution, virtual time/tasks/memory, and simulator service adapters | MP64 instruction execution, DBT, or emulator state |
| `shared/` | Backend-neutral host codecs, value types, protocol models, conformance helpers, and host sink/source interfaces | Selection of an execution backend or access to either backend's private state |

The dependency direction is strict:

```text
emulator  ───► shared ◄───  simulator
```

`shared` imports neither backend.  `emulator` and `simulator` do not import one
another.  A composition entry point may choose a backend and connect it to
shared ports, but shared code cannot perform that selection.

Target software is not emulator code.  `kdos.f`, `networking.f`,
`rich-terminal.f`, `graphics.f`, `tools.f`, `autoexec.f`, BIOS source and ROM,
RTL, FPGA sources, protocol specifications, and conformance vectors remain
backend-independent project artifacts.  The emulator executes their MP64
form; the simulator installs the public BIOS vocabulary and source-loads the
ordinary higher-level Forth files.

Existing flat host modules are staged migration inventory.  New
emulator-specific modules go under `emulator/`; new semantic-runtime modules go
under `simulator/`.  A temporary root import bridge may preserve an entry point
while consumers move, but it must alias the canonical module object when
callers can monkeypatch module state.  Bridges are deleted after migration and
are not a permanent compatibility surface for this unreleased project.

Code moves into `shared/` only after its API is expressed without either
backend's concrete CPU, bus, scheduler, memory, or device objects.  In
particular, a filename containing “shared” is not evidence of backend-neutral
ownership. Pure CRC mode parameters and recurrence/value transforms qualify;
CRC instruction execution and checked transaction ownership do not.

## 2. Compatibility claims

The simulator claims compatibility for:

- accepted MegaForth, KDOS, rich-terminal, and Akashic source;
- public word names, stack effects, status values, throw values, and module
  identities;
- 64-bit cell arithmetic, full-width Forth flags, little-endian byte-addressed
  data, public record layouts, and checked span behavior;
- source-visible dictionary behavior, stable execution tokens, callbacks,
  deferred words, exceptions, allocators, and transactional source loading;
- UART and ANSI output, portable filesystem/media bytes, network packets,
  APT-1 frames, UIDL data, and application data;
- ordinary Akashic UIDL/TUI, CELL fallback, rich publication, acknowledgement,
  and input lifecycles; and
- deterministic simulator replay when the initial state, capability profile,
  clock, entropy, and ingress schedule are the same.

The simulator does not claim compatibility for:

- MP64 opcodes, assembly, ROM execution, arbitrary MP64 binaries, or generated
  native code bytes;
- equal absolute `HERE`, `LATEST`, body, or execution-token addresses between
  backends;
- PC, registers, CSRs, instruction counts, cycle counts, cache behavior,
  interrupt observation points, or traps caused specifically by MP64
  instruction execution;
- physical multicore races, bus arbitration, device latency, UART baud timing,
  RTL behavior, FPGA behavior, or physical-display evidence;
- emulator snapshot compatibility; or
- execution of native MF64 dictionary images.

Portable binary data remains portable even when executable machine artifacts
do not.  A disk image, filesystem record, network packet, APT frame, UIDL
document, or application file has the same format in both backends.

## 3. Cell, stack, memory, and dictionary semantics

Cells are unsigned 64-bit bit patterns.  Arithmetic wraps modulo 2^64 unless a
word specifies a wider result.  Signed operations interpret the same bits as
two's-complement values.  False is zero and true is
`0xffffffffffffffff`.  Memory is byte addressed and little endian, including
unaligned loads and stores.

The admitted scalar vocabulary includes full-cell `XOR` and the ordinary
`C!` byte store. `C!` preflights exactly one addressed byte and stores the low
eight bits of its value, including for unaligned ordinary or admitted MMIO
addresses; it does not widen into a cell transfer.

The current executable BIOS implements scalar `MIN` as an unsigned comparison,
while the public Forth descriptions call it signed. That remains an
[open documentation/implementation discrepancy](bios-forth.md), not a hosted
simulator decision about the eventual public contract. Source qualification
records which input domain is insensitive to the mismatch and does not silently
replace the executable behavior.

Signed `MOD` is likewise not qualified for the single operand pair
`(INT64_MIN, -1)`: the current native C++ path omits signed division's overflow
guard, while the hosted primitive deterministically produces remainder zero.
That observation does not choose a permanent result-or-trap contract. The
qualified positive-divisor domain of KDOS `RAND-RANGE` excludes the edge.

The simulator exposes a sparse 64-bit logical address space with the same
source-visible address classes as the machine: Bank 0, external memory, HBW,
VRAM, and MMIO.  Host pointers never enter a guest cell.  Dynamic addresses
need not equal emulator addresses, but they are stable for their documented
lifetime.  Checked spans reject overflow and cannot become valid by wrapping
through address zero.

The data stack and the logical return stack retain ordinary MegaForth
semantics.  Colon continuations, user values moved by `>R`, and `DO` loop state
share one ordered return stack.  `DO` places its limit and index so `R@` sees
the loop counter unless source has deliberately placed a balanced value above
it.  `I`, `J`, `R@`, `R>`, `UNLOOP`, exceptions, and task switching must not be
implemented using independent stacks that merely appear equivalent in simple
programs.

The dictionary provides source-visible linked headers, flags and names,
newest-definition lookup, shadowing, compilation state, `HERE`, `LATEST`, and
rollback.  An execution token is a stable, nonzero 64-bit virtual code-field
address for the lifetime of its definition.  It works through `'`, `[']`,
`EXECUTE`, descriptors, deferred words, quotations, callbacks, and task
records.  No Python object identity or host function pointer may be exposed as
an execution token.

The simulator preserves the semantic behavior of `CREATE`, `DOES>`, `>BODY`,
`POSTPONE`, `:NONAME`, quotations, `DEFER`/`IS`, nested evaluation,
`CATCH`/`THROW`, `ALLOT`, dictionary zones, and `DICT-ROLLBACK`.  It may express
these operations as metadata or semantic IR rather than patching MP64 code.
Inspection or mutation of raw MP64 code fields is outside the contract.

## 4. Bootstrap and source loading

The simulator installs a semantic implementation of the public BIOS
vocabulary; it does not execute `bios.rom`.  KDOS, networking, the rich
terminal, and Akashic are then loaded from their normal source through the
ordinary evaluator and module path.

Host substitution stops at hardware primitives and explicitly admitted
accelerated primitive words.  Whole KDOS, filesystem, rich-terminal, UIDL,
Desk, or applet modules are not replaced with host implementations during a
compatibility journey.

`REQUIRE`, `PROVIDED`, evaluator nesting and diagnostics, compilation
transactions, rollback, dictionary-zone transitions, and allocation lifetimes
are required semantics.  JIT controls may be semantic no-ops only when their
documented effect is purely optimization.  Capability and status words must
report the simulator's actual support.

The current profile advertises one full core and `CRYPTO_CAPS = 0x7`: bit 0 is
the admitted semantic reflected/raw CRC service, bit 1 is checked SHA3/SHAKE
streaming, and bit 2 is raw Keccak-f[1600]. Bit 3 remains clear because the
hosted WOTS chain is not admitted. It advertises no additional cores,
accelerator timing, other crypto bits, devices, or sinks until their public
contracts have an implementation and differential evidence.

## 5. Scheduling and time

The simulator uses a deterministic cooperative scheduler, not uncontrolled
host threads.  Given the same runnable set, yield sequence, clock, entropy, and
ingress schedule, public task ordering and state repeat.

Task descriptors, per-task stacks, `PAUSE`, `TASK-YIELD`, `SCHEDULE`, exception
ownership, and checkpoint behavior remain visible source semantics.  A future
multicore profile must preserve publication order, generations, locks, and
barriers, but it still does not qualify physical races or arbitration.

Two clock modes are permitted:

- deterministic virtual uptime and epoch for tests and differential runs; and
- host-monotonic pacing for interactive use.

`MS@` and `EPOCH@` retain their public monotonic, wrapping, and timeout-ordering
semantics.  Deterministic mode advances at documented scheduling/service
boundaries so a polling loop can observe scheduled time without requiring
billions of simulated machine instructions.  Simulator ticks are diagnostics,
not MP64 cycles.

## 6. Platform services

The hosted BIOS/service layer may provide, as separately admitted
capabilities:

- UART and terminal geometry;
- monotonic and epoch time;
- block media and DMA-visible logical spans;
- SysInfo and memory topology;
- NIC frame ingress and egress;
- audio staging and sinks;
- entropy;
- CRC, SHA-2, SHA-3/SHAKE/Keccak, AES, and other crypto primitives;
- tile, SIMD, and TACC math; and
- core identity, dispatch, mailbox, barriers, and locks.

When a facility's direct virtual-MMIO surface is admitted, its BIOS words and
direct accesses reach one service implementation. A pseudo-BIOS-only frontier
slice must say so explicitly and does not qualify the corresponding raw
window. Ultimately, word-only substitution is insufficient because ordinary
source contains narrow direct-MMIO paths, including UART flush and audio
control.

An unsupported service advertises an absent capability and returns the
existing unsupported or absent status.  It never silently reports success.
Bit-exact results, checked spans, caller ownership, mutation order, status, and
error behavior are compatibility claims.  Latency and modeled machine cycles
are not.

The admitted CRC service implements the six public modes, mode-width seeds,
least-significant-byte-first cell feeds, byte feeds, raw/final reads, checked
status values, and the BIOS `(COREID,TASK-ID)` owner record. `CRYPTO-CAPS@` and
the SysInfo `CRYPTO_CAPS` qword expose the same profile. It does not claim CRC
instructions, CSRs, hardware arbitration, DMA, or accelerator latency.
Runtime construction requires that qword to be readable and rejects capability
bits without an admitted service; missing or malformed SysInfo never enables a
host fallback implicitly. Runtime construction likewise requires the admitted
one-core topology qwords to report exactly one full core.

The hosted diagnostic profile is intentionally backend-local. `PERF-CYCLES`
is a persistent, wrapping count of dispatched semantic work, not
wall time, instructions, or MP64 cycles; stall, tile-operation, and external-
beat counters remain zero until those specific services exist. BIST getters
retain the boot/profile snapshot, while destructive `BIST-FULL` and
`BIST-QUICK` fail before changing guest memory or retained BIST state. Their
admitted, faulting dispatch still counts as semantic work. The tile
self-test completes synchronously through the production 64-lane unsigned
ADD/MUL/DOT/SUM value kernel using host-only buffers. I-cache controls are
logical optimization state, dispatch remains immediately coherent, and its
hit/miss observations are zero. None of these diagnostic substitutions is
evidence for pipeline timing, physical RAM coverage, tile hardware, or a
physical instruction cache.

The admitted AES service is one per-runtime transaction engine behind the
virtual-MMIO router at `+0x700..+0x76F`; hosted BIOS words perform their normal
byte/word accesses against that same object. It implements AES-128/256 block
values, GHASH, partial final data, commands 0/1, statuses 0/1/2/3, uint32
length registers, native configuration/fault transitions, and incremental
BIOS buffer-transfer mutation. Guest contexts share it and guest control
unwinding does not reset it. A separate focused vector compares its public
windows directly with the native architectural device. The portable value
model is not constant-time host cryptography, and synchronous completion makes
no latency, interrupt, RTL, or side-channel claim.

Unchanged KDOS §1.5 is qualified only in its current safe source domain: plain
lengths are positive uint32 multiples of 16; AEAD AAD length is 1 through 16
and data length is a nonnegative uint32. Zero/nonmultiple plain lengths, zero
AAD, and AAD above 16 have source-level loop, classification, or overwrite
defects and are explicitly not repaired by a simulator-only cap. Bad-tag
decryption retains the executable mutation order: already streamed plaintext
remains published while the final output window is zero. These are recorded
compatibility findings, not endorsed security properties.

The admitted semantic `SPIN@`/`SPIN!` surface uses one runtime-local bank of
16 independent locks keyed by architectural physical-core ID, not task ID. A
free or same-owner acquire returns 0, a foreign-owner attempt returns 1, and
only the owner can release. Reacquisition is depthless, so one release frees a
lock after any number of same-core successes. Construction/reset starts every
lock free; guest `THROW` does not release one implicitly. The current slice
needs only valid lock 9 through the pseudo-BIOS words. It does not admit direct
spinlock MMIO, and rejects IDs outside 0 through 15 instead of reproducing the
BIOS's unchecked address arithmetic, which can fault or alias another device.
The simulator claims ordered terminal ownership/results, not host-thread
synchronization, memory fences, fairness, bus arbitration, contention timing,
interrupt behavior, or physical multicore evidence.

This first bank slice is not yet the backing object for the checked SHA3
service's internal logical lock-8 owner. The admitted HMAC path touches lock 9
and then enters SHA3 through its checked ABI, so that separation is not
observable there. Arbitrary guest manipulation of reserved lock 8, direct
lock-MMIO/SHA interference, and multicore guard interoperation remain outside
the current claim and must be unified before those paths are admitted.

The admitted SHA-2 service is runtime-local and keyed by architectural core,
not task. It implements the checked `SHA256-*` and `SHA512-*` BIOS words plus
pure `SHA2-SPAN-STATUS`; it is not an MMIO service and does not consume a
`CRYPTO_CAPS` bit. `INIT` always replaces that core's selected context,
`CLEAR` is idempotent, and continuations use statuses 0 OK, 1 STATE, 2 RANGE,
3 CONTEXT-ALIAS, and 4 LENGTH-OVERFLOW. SHA-256 tracks a checked 64-bit bit
length and SHA-512 a checked 128-bit bit length. Active marker, high-word where
applicable, partial-offset bound, byte alignment, and length/offset agreement
are validated before an empty update may succeed. Every nonzero continuation
result aborts and logically wipes the selected context.

SHA-2 physical-span qualification is deliberately broader than
`CALLER-SPAN-STATUS`. Empty spans ignore their address. A nonempty span may
start at address zero or name static Bank-0 bytes, but it must fit wholly in
one advertised Bank 0, external, HBW, or VRAM region; wrap, MMIO, unmapped, and
cross-region spans return RANGE. Native layouts then reject intersection with
either complete SHA-2 private-context arena as CONTEXT-ALIAS. Hosted contexts
are out-of-band host objects, so ordinary hosted guest spans cannot alias
them; a composition may configure mapped private arena ranges when it exposes
such storage. Geometry is still checked before alias classification.

Updates preflight their complete source before reading it. Finalization
preflights all 32 or 64 destination bytes, stages the standard big-endian
digest, publishes once, and then becomes inactive. A failed final does not
alter an ordinary destination. The exact unchanged KDOS slice through
`kdos.f` line 1269 retains `HASH` as the SHA3-256 alias and adds the adjacent
`SHA256` and `SHA512` one-shot wrappers, which return the first checked status
unchanged.

Hosted SHA-2 uses incremental `hashlib` objects. Dropping those objects and
overwriting explicit metadata and publication stages is a logical simulator
cleanup claim only; it does not prove physical erasure in CPython or its host
crypto library. The simulator does not claim EXT.CRYPTO instruction or CSR
behavior, raw engine state, compression latency, cluster arbitration or
stalls, interrupt masking, raw padding-buffer effects, or constant-time host
execution.

The hosted service follows the working BIOS/native executable SHA-2 behavior.
Current RTL instruction glue is not equivalent:

| Surface | Working BIOS/native behavior | Current RTL behavior |
|---|---|---|
| `SHA.PAD` / `SHA.FINAL` | Performs FIPS padding/final compression used directly by BIOS | Both are data-path no-ops, despite BIOS not constructing padding manually |
| `SHA.DOUT Rd,Rs` | Selects 32-bit `H[R[Rs] & 7]`, allowing the BIOS value-indexed loop | Selects a 64-bit accumulator qword from the encoded register field |
| `SHA.DIN` | Feeds one byte through the documented block-buffer path | Writes a whole qword into an accumulator slot |
| ROUND memory load | Reconstructs addressed bytes and parses big-endian SHA words | Splits little-endian memory qwords into W words without the required byte swap |
| SHA-512 | Native mode 2 backs the checked BIOS stream | The RTL SHA leaf identifies SHA-384/512 as future work |

Native checked tests cover standard digests and padding/split boundaries; the
present RTL tests exercise an already padded/endian-correct compression leaf
or ownership only. Simulator success therefore supplies no RTL or physical
SHA-2 evidence, and this discrepancy record does not choose which hardware or
public-ISA correction should land.

The admitted SHA service is one per-runtime shared transaction engine behind
the virtual-MMIO router at `+0x780..+0x7DF`. Checked BIOS words and direct
virtual-MMIO accesses reach that same object. The service implements all four
SHA3/SHAKE modes, their exact rates and domain separators, sequential
64-byte output windows, staged checked publication, and raw in-place
Keccak-f[1600] over 25 little-endian lanes. Fixed and extendable-output
transactions retain their BIOS `(COREID,TASK-ID)` owner until the required
terminal clear. In the current one-core pre-scheduler profile that identity is
always `(0,0)`; guest control unwinding does not implicitly release it.

Every nonempty checked input, output, or 200-byte raw-state transfer is
qualified as one complete caller-managed span before its first guest-memory
access. Bank 0 is bounded below by the hosted static/dictionary rollback floor
and above by the active caller's future result-cell boundary (`DSP-8`);
external, HBW, and VRAM spans must fit wholly within one advertised region.
Zero length ignores its unused address. Host scratch contexts without a
memory-backed stack borrow the canonical foreground stack boundary. These
checks establish geometry and protection, not allocation ownership,
mutability, lifetime, initialization, or nonaliasing.

SHA operations complete synchronously to their semantic terminal state. The
simulator claims terminal values, output bytes, ownership, mutation order, and
error/cleanup behavior; it does not claim an observable BUSY interval,
Keccak-round or bus latency, DIN backpressure, polling timeout cadence,
interrupt delivery, physical spinlock arbitration, RTL timing, or constant-
time host execution.

Direct SHA MMIO intentionally follows the current native executable model on
three error-priority cases where integrated RTL differs:

| Case | Native executable and simulator | Current integrated RTL |
|---|---|---|
| `INIT` while the stream feature is disabled and the engine is not owner-free/idle | The general owner/phase conflict has priority | A raw owner conflicts, but otherwise feature unavailability is tested before the general owner/phase conflict |
| `NEXT` in a fixed-output mode from a non-raw state that is not sponge/DONE | After feature availability, invalid mode has priority | Invalid mode is emitted only for sponge/DONE; the other wrong owner/phase states report conflict |
| Disabled DOUT or raw state/index read while the opposing owner is active | Opposing-owner conflict is recorded before the disabled feature returns zero | The feature gate returns zero first and does not record that conflict |

This discrepancy record chooses the executable behavior for hosted
differential compatibility; it does not decide which native/RTL contract must
ultimately change.

Byte-exact `kdos.f` lines 1270 through 1431 execute the complete unchanged
HMAC-SHA3 portion of unified crypto plus its `ENCRYPT`, `DECRYPT`, and
`VERIFY` words. Capability absence precedes the one nonblocking lock-9
attempt, contention precedes caller-span validation, and later checked SHA3
statuses propagate unchanged. The source builds 136-byte ipad/opad values,
hashes keys longer than the SHA3-256 rate, holds lock 9 across inner/outer
transactions, and wipes 392 bytes of HMAC scratch before ordinary release.
Final HMAC publication inherits `SHA3-FINAL`'s complete 32-byte preflight and
all-or-nothing write. The named scratch remains a cooperative caller
nonaliasing rule rather than a protection domain. `VERIFY` evaluates every
requested byte for its result when given a positive, nonwrapping length, but
that source property is not a host timing or constant-time claim. Its
unchanged `0 DO` loop does not admit zero as an empty comparison: equal zero
bounds enter the body and can wrap or fault. The hosted backend does not patch
that source-level discrepancy.

The exception guard also preserves the executable limitation recorded in the
project crypto contract. A successful lower clear after a Forth `THROW`
allows wipe, release, and rethrow of the original code. A nonzero lower clear
wins and leaves lock 9 owned, excluding peer cores. Because the hardware bank
accepts a depthless reacquire by that same physical core, a later same-core
wrapper can still enter and release it. Hosted execution does not invent task
ownership to conceal this discrepancy, and this record does not decide
whether KDOS software bookkeeping or the hardware contract should change.

The admitted X25519 path consists of a backend-neutral RFC 7748 value model,
one runtime-local Field-ALU state per architectural core, and the six raw BIOS
words `X25519-SCALAR!`, `X25519-POINT!`, `X25519-GO`, `X25519-WAIT`,
`X25519-STATUS@`, and `X25519-RESULT@`. There is no X25519 capability bit,
checked status namespace, lock, or task owner. ACC0-ACC3, TSRC0, and the
persistent previous result are physical-core state shared by tasks on that
core; construction/reset clears them, while guest `ABORT` and `THROW` do not.
`STATUS@` always returns 2 and `WAIT` is a no-op, including before any
operation.

`SCALAR!` loads four ascending little-endian qwords and can leave a partially
replaced ACC after a later access fault. `POINT!` records TSRC0 without
preflight. `GO` reads all four point qwords before replacing ACC and the
previous result, so a point-read fault leaves ACC unchanged. `RESULT@` stores
four ascending qwords and can leave a destination prefix after a later write
fault. Address stepping wraps as a guest uint64 cell; unaligned ordinary
memory is accepted, while MMIO validity is determined independently for each
qword. The raw path intentionally has no complete-span or all-or-nothing
publication claim.

The value operation clamps the scalar, clears the encoded point's top bit,
uses the field `2^255-19`, and emits 32 little-endian bytes. Low-order/all-zero
points are not rejected and may produce an all-zero result. The high-level
unchanged `X25519` consumes both inputs before publishing, so its destination
may alias either one. Exact `kdos.f` lines 1433 through 1481 also add the
global 32-byte `X25519-PRIV`, `X25519-PUB`, `X25519-SHARED`, and base-point
buffers plus ordinary `X25519-KEYGEN` and `X25519-DH`. Those buffers are not
task/core-isolated or automatically wiped. Key generation uses the admitted
deterministic development TRNG and is reproducible, not cryptographically
secure.

The hosted result follows RFC 7748, native C++, and the standalone Field-ALU
RTL by using `A24=121665` with `E*(AA+A24*E)`. The architectural Python
emulator currently uses `121666` with that same formula and returns the wrong
published vector. Current integrated RTL is a second discrepancy: the
full-core crypto dispatch treats units beyond CRC/SHA as no-ops, while the
microcore's declared Field ports are not connected by the cluster. Hosted
qualification therefore does not claim Python-interpreter or integrated-RTL
agreement and does not choose the eventual correction there. It also makes
no claim about EXT.CRYPTO encodings, CSRs, the nominal 4335-cycle latency,
stalls, interrupts, constant-time host execution, or host-memory erasure.

The general Field slice admits the same per-core state through all 15 raw BIOS
words: `GF-A!`, `GF-R@`, `GF-PRIME`, `LOAD-PRIME`, `FADD`, `FSUB`, `FMUL`,
`FSQR`, `FINV`, `FPOW`, `FMUL-RAW`, `FCMOV`, `FCEQ`, `FMAC`, and
`FMUL-ADD-RAW`. Exact unchanged `kdos.f` lines 1483 through 1515 then define
the four named prime selectors and four 32-byte scratch buffers. Every
ordinary operand/result argument is an address to a 32-byte little-endian
integer; raw operations take separate low and high addresses. `FCMOV` takes
an operand address and a condition-byte address, not an immediate condition.

ACC0–ACC3, TSRC0, TDST, the two-bit selector, custom prime and inverse, and
previous low/high values are physical-core state. Successful ordinary result
operations replace previous-low; successful raw operations replace both
halves. False `FCMOV`, transfer/configuration words, and `LOAD-PRIME` leave the
previous values unchanged. Previous-high deliberately remains stale across a
later low-only operation, so a subsequent raw MAC may combine it with the
newer low half. X25519 shares ACC, TSRC0, and previous-low with these words
while ignoring the selected prime. Hosted construction/reset clears the whole
service; guest `ABORT`, `THROW`, and memory failure do not roll it back.

The transfer contract is four ascending qword accesses at offsets 0, 8, 16,
and 24, each with uint64 address stepping. There is no complete-span preflight
or alignment restriction. An A-load fault retains its completed ACC prefix;
a B/exponent fault leaves fully loaded A but publishes no arithmetic result.
Normal arithmetic updates ACC/previous-low before its sequential result store.
Raw multiply updates ACC low, publishes high qwords, then commits the previous
pair, after which BIOS publishes low qwords. Thus a high fault leaves low in
ACC, a high destination prefix, the old previous pair, and an untouched low
destination; a low fault leaves high and the new previous pair committed.
Input/output aliasing is valid after complete input consumption. High and low
destinations may also alias, with the later low stores winning. `LOAD-PRIME`
fully loads `p`, sets TSRC0, then latches custom `p` before reading the inverse;
an inverse fault can retain new `p` with the old inverse. `FCMOV` sets TSRC0,
reads the condition byte, and reads all four operand qwords even when false.

Selectors 0, 1, and 2 name Curve25519, secp256k1, and NIST P-256. Selector 3
uses the latched custom value, with the executable native/Python zero fallback
to Curve25519. `LOAD-PRIME` neither selects custom mode nor validates its
tuple. Montgomery REDC is used only by `FMUL`, `FSQR`, and the product portion
of `FMAC`, and only when selector 3 has a nonzero inverse. `FINV` is literal
Fermat exponentiation, `FPOW` uses ordinary residues, and `FCEQ` compares exact
representations. The portable mathematical claim is limited to canonical
inputs and a valid prime/custom Montgomery tuple. Invalid primes, incorrect
inverses, and noncanonical Montgomery representations are accepted raw state,
not a cross-backend field contract.

Outside that domain the checked-in backends disagree. C++ and standalone RTL
use one conditional reduction for ADD, while Python uses full `% p`; SUB has
additional C++/RTL/Python differences for noncanonical inputs. Native C++ can
retain hidden upper `BigNum` limbs after such ADD/SUB operations and expose
them through a later FMAC, whereas hosted and RTL previous-low is exactly 256
bits. Native C++ also loses the carry from previous-low into previous-high in
raw MAC; hosted follows the wrapped 512-bit result agreed by Python and the
standalone RTL. Python transfers Field B/high data bytewise rather than in
BIOS/native qwords. Standalone RTL lacks the custom-zero fallback, and custom
`p=1,e=0` differs between native C++ and the other value models; both are
outside the valid-custom-prime claim. These discrepancies are pinned rather
than silently selected as alternate ABIs.

Raw `GF.CEQ` computes a Z flag, but the BIOS result-store helper immediately
executes flag-writing address increments before returning. The public result
is the stored 256-bit 1/0, not retained flags. There is no Field capability
bit, checked status, wait protocol, lock, task owner, span qualifier, automatic
wipe, allocation, or error return. Hosted Field execution makes no claim about
instruction bytes, CSRs, cycles, stalls, arbitration, interrupt interleaving,
integrated RTL, constant-time host behavior, or physical secret erasure. The
current emulator reset paths also omit some prime/previous state that hosted
`reset()` clears; simulator lifecycle tests do not resolve that reset defect.

The admitted NTT slice is one runtime-global semantic service behind all 10
raw BIOS words: `NTT-SETQ`, `NTT-IDX!`, `NTT-LOAD`, `NTT-STORE`, `NTT-FWD`,
`NTT-INV`, `NTT-PMUL`, `NTT-PADD`, `NTT-STATUS@`, and `NTT-WAIT`. It is
shared by guest contexts and has no core/task owner, lock, capability bit,
checked error result, or automatic wipe. Guest `ABORT`, `THROW`, and memory
faults do not roll it back. Hosted construction and explicit service reset
clear q/index/buffers/status to their construction values; this is not a claim
that the current emulator's warm `boot()` resets its NTT device.

The service follows the working BIOS plus architectural Python device. Native
CPU acceleration has no C++ NTT implementation and delegates this range to
that same Python device. Q is a retained uint64 initially 3329; IDX retains 16
bits, while buffer access and auto-increment use modulo 256. A and B each hold
256 reduced uint32 coefficients, result is separate, and changing q does not
renormalize retained buffers. Exact selector zero loads A and every nonzero
selector loads B. The portable q domain is set-before-load use of 3329 or
8380417; invalid/composite-q behavior is retained where deterministic but is
not advertised as a mathematical accelerator contract.

`NTT-LOAD` consumes both stack cells and resets IDX before reading memory. For
each coefficient, each source byte is read before its staging byte changes;
only byte 3 commits `uint32le % q` and increments the index. `NTT-STORE`
resets IDX and reads each result byte before its corresponding guest write; a
byte-3 result read increments before the guest byte-3 write. Address stepping
wraps as uint64, with no span preflight or alignment check. A later load fault
therefore retains prior coefficients and the current staging prefix. A later
store fault retains its destination prefix, and a byte-3 destination fault
also retains the already advanced index. Complete 1024-byte transfers end at
index zero. Input/output aliasing is safe after complete input consumption.

Commands use raw bytes 1, 3, 5, and 7. They synchronously replace only result,
leaving A and B intact, and transition from initial status 0 through an
unobservable busy interval to retained status 2. A modulus for which the
Python search finds no 256th root still reaches DONE without replacing result.
`NTT-WAIT` tests the DONE bit, not “not busy,” so an idle call never completes;
the hosted semantic dispatcher repeats it until a caller-provided step budget
expires. Hosted execution makes no BUSY-latency, fairness, arbitration, or
interrupt claim.

The shared value model reproduces the Python device's bounded root search and
ordinary radix-2 forward/inverse transforms. Pointwise addition and
multiplication are fully reduced. This means
`INTT(NTT(a)*NTT(b))` computes cyclic convolution modulo `x^256-1`, not the
negacyclic ring operation required by ML-KEM or ML-DSA. Exact unchanged
`kdos.f` lines 1517 through 1584 define both named moduli, selectors, two
global 1024-byte scratch buffers, `NTT-POLYMUL`, and `.NTT-STATUS`; the PQ
labels do not strengthen that mathematical claim. The KEM emulator uses
separate ML-KEM-specific routines. KDOS scratch aliases and concurrent
`NTT-POLYMUL` calls are unsafe because there is no ownership protocol.

This frontier is deliberately pseudo-BIOS-only: it does not admit direct
virtual NTT MMIO. Current RTL uses an incompatible 64-bit-slot map, while the
working BIOS/Python path uses byte windows STATUS `+00`, Q `+08..0F`, IDX
`+10..11`, A `+18..1B`, B `+1C..1F`, RESULT `+20..23`, and CMD `+28`.
Current RTL also fixes its twiddle tables and inverse scale to q=3329 even when
Q changes, exposes multi-cycle BUSY/partial work, and produces a different
forward ordering for its fixed root. BIOS byte accesses cannot drive that RTL
unit correctly. Simulator success therefore makes no direct-MMIO, RTL,
standardized-PQ, cycle, bus-width, constant-time, or physical-erasure claim,
and this discrepancy record does not choose the eventual hardware correction.

The admitted ML-KEM slice is one runtime-global semantic service behind the
seven authoritative raw BIOS words: `KEM-SEL!`, `KEM-LOAD`, `KEM-STORE`,
`KEM-KEYGEN`, `KEM-ENCAPS`, `KEM-DECAPS`, and `KEM-STATUS@`. Exact unchanged
`kdos.f` lines 1586 through 1633 define five buffer IDs, five size constants,
the three `KYBER-*` wrappers, and `.KEM-STATUS`. The source declares
`KEM-SEED-SIZE=32`, while `KYBER-KEYGEN` explicitly loads all 64 bytes consumed
as `d || z`; this is a pinned discrepancy, not an implicit simulator repair.

Construction creates zero-filled SEED/COIN=64, PK=800, SK=1632, CT=768, and
SS=32 buffers, selector zero, index zero, and status IDLE=0. All contexts share
that state with no core/task owner, lock, capability bit, command transaction,
unwind cleanup, or secret wipe. Selecting takes the low byte, clamps 5..255 to
SS/4, and resets only the index. Selection and transfer do not clear status.
Hosted explicit `reset()` restores construction state, but emulator warm
`System.boot()` does not reset its Python KEM device; hosted reset tests do not
qualify warm-boot behavior.

DIN and DOUT advance only while the selected buffer is in bounds. At capacity
DIN drops bytes, DOUT returns zero, and the index remains pinned. Short loads
retain the old suffix. `KEM-LOAD` consumes count then address from the data
stack and, for each byte, reads guest memory before writing DIN. `KEM-STORE`
also consumes count then address and reads DOUT before the corresponding guest
write. There is no whole-span preflight or alignment restriction; address
stepping wraps as uint64. Thus a load fault leaves only its successfully read
prefix committed, while an in-range store fault has already consumed the
faulting output byte. Excess loads still read the caller span after device
capacity, and excess stores publish zeros. Count zero touches neither memory
nor index. Normal wrapper inputs are fully loaded before any output store, so
input/output aliasing is safe; PK then SK and CT then SS store order determines
the final bytes when outputs overlap.

Commands synchronously snapshot fixed buffers and replace only their outputs:
keygen reads all 64 seed bytes and replaces PK/SK; encapsulation reads PK plus
the first 32 seed bytes and replaces CT/SS; decapsulation reads CT/SK and
replaces SS. Each returns with retained DONE=2 without resetting selector or
index. BUSY=1 is never observable, commands report no error state, and unknown
raw command bytes do nothing. `.KEM-STATUS` therefore renders only 0 as idle,
2 as done, and every other retained value as unknown.

The backend-neutral value implementation produces exact deterministic
ML-KEM-512 bytes for generated or independently validated fixed-size keys. An
independent local OpenSSL 3.5.2 zero-`d || z`/zero-coin comparison produced
SHA-256 hashes `52b46f0597ac5cb10c6281ad5731f18d599feaa92ce24d897d4084195b27e448`
(PK), `3a19948fd8e0d7af1e2f3bb32bf2299b91f40c66b3faeb773b8fc3dc2f140092`
(SK), `b9f7694fa5a2be9fb849d0c0ea8f55fce6d91eaecb9c34dffe47b5b5d6034de3`
(CT), and `e9a21d9e6c451ac6b7b78b57c7fef1aeb43af246bc782efbacdca0e19bac2c62`
(SS). Flipping every bit of CT byte zero yields implicit-rejection SS hash
`55dc98baa9f1632bb478e3348e3cb7b258df5309a9a7815c967f2dcada38c557`.

That oracle is not FIPS certification or a checked external API. Callers
supply `d`, `z`, and encapsulation randomness. Length-correct noncanonical
public keys and decapsulation keys with inconsistent embedded hashes are
accepted where OpenSSL rejects them; comparison and arithmetic are not
constant time; the rejection sampler assumes a fixed 840-byte SHAKE prefix is
sufficient; and secrets are retained rather than zeroized. The shared code is
target-value compatibility logic, not a host-secret cryptography boundary.

This frontier is pseudo-BIOS-only and does not admit direct virtual KEM MMIO.
Native C++ has no KEM implementation and routes the working Python device. Its
40-byte map is STATUS `+00`, CMD `+01`, selector `+08`, DIN `+10`, DOUT `+18`,
and little-endian size `+20..21`. Current RTL instead places CMD/STATUS in the
`+00` 64-bit slot, DIN/DOUT at `+10`, IDX_SET/size at `+18`, and index at
`+20`; it exposes multi-cycle BUSY, advances and clamps streams differently,
fills only prefixes of large outputs, and computes non-cryptographic XOR stub
values. BIOS DOUT therefore reads RTL size, and immediate KDOS stores race the
RTL lifecycle. Hosted success makes no direct-MMIO, RTL, timing, arbitration,
FIPS-validation, constant-time, or physical-erasure claim.

The admitted HMAC/HKDF/hybrid slice is exact unchanged `kdos.f` lines 1635
through 2043: 59 ordinary definitions spanning the §1.13 PQ scratch preamble,
the complete SHA3-HMAC HKDF family, HMAC-SHA256 and HKDF-SHA256, and the three
final PQ exchange words. No high-level HMAC, HKDF, or exchange word is a host
substitute. Hosted services beneath them remain the already admitted SHA3,
SHA-256, X25519, ML-KEM, spinlock, memory, and deterministic-entropy
primitives.

Public `HMAC`, `HMAC-SHA256`, and both HKDF families share one nonblocking
physical-core-owned spinlock 9. SHA3 capability absence is checked before lock
contention and returns `CRYPTO-UNSUPPORTED`; a busy lock returns `CRYPTO-STATE`
for SHA3 or `SHA256-STATE` for SHA-256. Capability and busy exits consume their
public arguments before the guard and do not wipe preexisting private scratch.
Acquired paths retain the existing `_HMAC-HKDF-GUARD` contract: ordinary and
checked-status returns wipe family-specific pad/key/digest/metadata state and
release; a caught Forth exception first invokes the selected checked-hash
clear, then wipes, releases, and rethrows. A lower clear failure takes
precedence and retains lock 9 as already specified by that guard. This slice
does not add an outer PQ owner.

Both expand words accept 0 through 8,160 output bytes, preflight the complete
32-byte PRK, info, and output spans, and reject any nonempty destination overlap
with the PRK or nonempty info before writing. Zero output ignores its output
pointer after the other required input checks. Successful output is published
one at-most-32-byte HMAC block at a time; a failure in a later block retains
the earlier prefix. Callers may not alias named private HMAC/HKDF scratch.

For extract, the empty-salt convention is selected solely by `slen=0`; the
pointer is ignored only in that case and the key becomes 32 zero bytes. The
source comment's "salt is 0 / slen=0" wording does not match that branch.
With nonzero length, pointer zero is treated as a real address: SHA3's
caller-managed span policy returns `CRYPTO-RANGE`, whereas SHA-256's physical
span policy admits Bank 0 address zero when otherwise valid and hashes it.
This contract records current execution and does not decide whether the source
comment or either implementation should change.

INIT and RESP populate `_PQ-CAT` as `_PQ-SS-X || _PQ-SS-K`; `PQ-DERIVE`
assumes that 64-byte value is already present, extracts `_PQ-PRK` with
SHA3-HMAC HKDF and the empty-salt convention, then expands 32 bytes with the
literal info `pq-hybrid`. INIT performs X25519, consumes 32 `RANDOM8` bytes,
encapsulates and publishes the 768-byte ciphertext, fills SS-X/SS-K/CAT/coin
scratch, and only then calls derivation. RESP performs X25519 and decapsulation
before the same derivation. SHA3 capability absence is therefore discovered
only after those raw side effects. The returned status is only HKDF's checked
result; X25519 and the KEM commands expose no checked result at this layer, and
memory, entropy, or raw-service exceptions are not converted into one.

`X25519-PRIV`, `_PQ-SS-X`, `_PQ-SS-K`, `_PQ-CAT`, `_PQ-PRK`, and `_PQ-COIN`
are runtime-global guest storage. They have no task/core owner, transaction,
rollback, or wipe. Extract and expand are separately locked, so exchanges may
interleave before, between, and after those calls. For INIT, initial extract
contention or SHA3 absence does not roll back entropy consumption, ciphertext
publication, KEM state, X25519 work, or the changed SS/CAT/coin scratch, but it
preserves `_PQ-PRK` and the final-key destination. RESP likewise retains its
completed X25519/KEM and SS/CAT effects. If extract succeeds but expand then
contends or fails, `_PQ-PRK` is also retained while the final-key destination
remains unchanged. Ciphertext and final-key outputs must be disjoint if both
must remain intact. External inputs are consumed before final publication, but
callers must not alias the
named PQ or HMAC/HKDF scratch.

The hosted entropy source is deterministic and non-cryptographic; the source
does not reject an all-zero X25519 result; and the raw KEM contract accepts
some malformed fixed-size keys. Admission proves exact source composition,
values, state, and failure ordering. It is not a standardized hybrid-KEM
claim, security proof, FIPS validation, constant-time claim, hostile-key
validation, concurrency safety, or protected host-secret boundary.

The admitted HBW slice installs `HBW-BASE` and `HBW-SIZE` as semantic BIOS
reads of SysInfo `+0x20` and `+0x28`, respectively. They always read the
currently bound sparse-memory geometry rather than copying host constants into
the dictionary. Exact unchanged `kdos.f` lines 2044 through 2108 then define
`HBW-HERE`, `HBW-LIMIT`, `HBW-INIT`, `HBW-ALLOT`, `HBW-ALLOT?`,
`HBW-TALIGN`, `HBW-RESET`, `HBW-FREE`, and `.HBW`, and execute `HBW-INIT` at
load time.

Those two variables are runtime-global guest state shared by all contexts.
There is no task/core owner, lock, transaction, allocation ledger, floor,
individual free, or automatic wipe. Allocation returns the old pointer and
advances by exactly the supplied cell without touching storage; zero and exact
fit succeed. Ordinary checked overflow returns `(0,-1)`, while the aborting
form emits `HBW overflow` and performs task `ABORT`; both leave the pointer
unchanged because their store follows the comparison. `HBW-TALIGN` rounds the
pointer up to 64 bytes. `HBW-RESET` rereads the base but does not clear memory,
revoke old addresses, or synchronize callers. `.HBW` reads live state and uses
the current numeric base plus signed `.` formatting.

The allocator reserves none of the advertised span for other subsystems.
`graphics.f` independently chooses `HBW-BASE + 0x200000` for its framebuffer
without moving `HBW-HERE`, so an allocation entering the third MiB may overlap
that framebuffer. Hosted execution preserves this composition requirement and
does not invent a hidden reservation.

The admitted allocation domain requires the current pointer and a nonwrapping
request to stay within the mapped HBW span. The source names the request `u`
but adds before applying signed `>` and performs no wrap check, so high-cell
requests can wrap and succeed. Alignment is also unchecked and can cross a
configured limit that is not 64-byte aligned. Canonical base
`0xFFD0_0000`/size 3 MiB is aligned. The hosted factory can explicitly model
no HBW and then reports `(0,0)`; the emulator's configured-zero edge instead
retains fixed `HBW_BASE` with size zero. These unqualified edge discrepancies
are reproduced and recorded without choosing a future contract.

The admitted external-memory slice installs `EXT-MEM-BASE` and
`EXT-MEM-SIZE` as dynamic semantic BIOS reads of SysInfo `+0x38` and `+0x40`.
They report the actual external region bound to the sparse address space; the
runtime does not copy a second geometry or substitute a host allocator.
Unchanged `kdos.f` lines 2110 through 2388 add all 31 definitions through
`XBUF`, including the raw XMEM allocator, first-fit free list, public
`ALLOCATE`/`FREE`/`RESIZE` dispatch, explicit Bank-0 DMA allocation, reset
floor, and status output. `NIP` and `TUCK` are ordinary stack primitives needed
by that source, not fused allocator operations.

Load-time `XMEM-INIT` is one-shot. A present region starts the bump pointer at
`EXT-MEM-BASE` and the limit at base plus size; an absent hosted region reports
`(base,size)=(0,0)`. Positive raw requests are rounded to 16 bytes, search the
LIFO free list by first fit, split only when a 16-byte tail remains, and then
fall back to the bump tail. Rejected checked allocation returns `(0,-1)` and
the aborting form publishes its source message; both preserve the bump pointer
and free-list topology, although a completed failed search leaves the shared
`FL-NEED`, `FL-PREV`, and `FL-CURR` scratch changed. Returned spans are
normalized and preflighted against the configured limit and current high-water
mark before free-list metadata is written. `XMEM-RESET`
restores the floor (or base), clears the list, and neither wipes storage nor
revokes old addresses. `XMEM-FREE` and `.XMEM` count only the unused bump tail,
not recyclable list nodes.

When XMEM is present, public `ALLOCATE` stores an eight-byte total-size prefix
and returns the following payload; `FREE` uses that prefix, and `RESIZE`
allocate-copies-frees while preserving the original address on allocation
failure. With no XMEM, these words retain the source-defined Bank-0 path.
`DMA-ALLOCATE`, `DMA-FREE`, and `DMA-RESIZE` always retain that Bank-0 path.
`XBUF` uses XMEM when present, publishes an ordinary constant, and advances
the reset floor; without XMEM it uses ordinary `CREATE ALLOT` in the active
dictionary.

The free checks establish interval bounds, not ownership. There is no live
allocation ledger, alignment proof, overlap check, or double-free check, so a
manufactured interior subspan can be admitted and a repeated free can create
a self-linked node. `FREE` classifies every nonzero address at or above
`MEM-SIZE` as XMEM and reads its prefix before validation; qualification is
therefore limited to zero or pointers returned by the corresponding allocator.
`XMEM-FLOOR` constrains reset only; free insertion does not consult it, so a
forged span can reclaim persistent XBUF or dictionary-index storage below the
floor.
`XBUF` allocation precedes constant publication and floor advancement, so a
later dictionary fault can leak an unprotected block. These are open KDOS
contract gaps, not simulator-side repairs.

Allocator variables, free-list links, search scratch, the resize scratch cell,
and floor are runtime-global and unsynchronized. The public XMEM `ALLOCATE`
branch checks `?CORE0`, but raw XMEM allocation/free/alignment/reset do not;
XMEM `FREE` is consequently unguarded, and XMEM `RESIZE` writes `_RS-OLD`
before its nested allocation reaches the guard. Hosted one-core evidence does
not claim multicore safety. The general memory documentation now records this
source/enforcement discrepancy rather than claiming every shared operation is
self-guarded.

The qualified geometry has a positive signed size below the next physical
window and a nonwrapping base-plus-size. `XMEM?` uses signed `0>`, and
`XMEM-TALIGN` can advance beyond a non-64-byte-aligned configured limit. The
public size guard also admits `0x7fff_ffff_ffff_fff0` before adding its
eight-byte prefix even though subsequent 16-byte normalization cannot remain
positive; ordinary sub-VRAM geometry rejects that request before the edge can
mutate state. The hosted and executable-emulator constructors interpret
configured size zero as
no external region, while the current RTL parameter interprets zero as the
maximum window up to VRAM; the normal emulator session profile separately
defaults to 128 MiB. Every profile must expose its actual SysInfo geometry.
This discrepancy is recorded without selecting one universal meaning for a
configuration value that is not itself a guest ABI input.

The admitted dictionary-index BIOS service preserves the executable
`DICT-INDEX!` validation and statuses. `0 0` disables with status 0; otherwise
the base must be 16-byte aligned, the slot count a nonzero power of two,
`slots*16` and base plus span must not wrap, and the complete span must fit in
advertised external memory. Invalid status 1 changes neither diagnostics nor
table bytes. Valid installation clears and rebuilds the new table
newest-to-oldest: complete state returns 0 with `BOUND|AUTHORITATIVE`, while an
incomplete or saturated rebuild returns 2 with the partial positive table
retained as `BOUND|SATURATED`.

Each exact 16-byte guest slot contains the entry pointer at `+0`, uppercase
FNV-1a32 at `+8`, the name length at `+12`, and three zero reserved bytes.
Metadata is written before the pointer. Rebuild uses insert-if-absent so the
newest shadow wins; later definitions upsert that pointer without increasing
the unique-name count. Numeric `DICT-ROLLBACK` clears and rebuilds the bound
table after restoring the semantic dictionary. The linked semantic dictionary
remains the correctness representation, so an unbound or saturated index has
the same lookup results without claiming hardware `EXT.DICT`, probe timing,
seqlock concurrency, or cycle counts.

Exact unchanged `kdos.f` lines 2390 through 2423 add
`_DICT-POW2-FLOOR`, `_DICT-INDEX-DONE`, and `_DICT-INDEX-INIT`, then run the
initializer. `2*` wraps one-cell left shifts. Executable BIOS `2/` is a logical
right shift even though its assembly comment calls it arithmetic; the sizing
source supplies positive values, and the discrepancy is pinned rather than
normalized. Canonical 128 MiB XMEM reserves 65,536 slots (1 MiB) and advances
both `XMEM-HERE` and `XMEM-FLOOR`. Absent XMEM and a present bump tail below
2,048 bytes leave the index disabled. Exactly 2,048 bytes selects one slot,
installs a saturated fallback, and protects the 16-byte table.

The BIOS validates physical geometry, not allocator ownership or disjointness;
a caller can bind over live XMEM data or dictionary headers and rebuild will
clear that span. Disable clears diagnostics but intentionally leaves old table
bytes. `DICT-INDEX@` is four sequential BIOS loads rather than an epoch-retried
multicore snapshot; the hosted one-core profile returns one stable state and
does not claim that stronger hardware behavior. KDOS sizes from `XMEM-FREE`,
which counts only virgin bump-tail capacity, not reclaimed nodes.

KDOS sets `_DICT-INDEX-DONE` before allocation and installation. A status-1
BIOS rejection after allocation therefore leaves retry disabled, the
allocation consumed, and the floor unadvanced before aborting. The admitted
fresh-boot geometry cannot reach that path, but it remains a documented
nontransactional edge rather than an invented rollback.

The admitted bounds service implements the executable `DICT-BOUNDS!` contract.
`0 0` disables; every other pair requires nonzero base, unsigned
`limit > base`, and complete containment in the nonwrapping external-memory
geometry advertised by SysInfo. There is deliberately no alignment,
current-HERE, allocator-ownership, index-overlap, or minimum-capacity rule.
Invalid input consumes both arguments and enters `DICT-FAULT-XT!` before the
old pair changes. `DICT-BOUNDS-OFF` clears the pair without moving `HERE`.
The hardware's limit-first publication ordering is not independently
observable in the one-core hosted profile; hosted callers see only the stable
old, disabled, or complete new state.

Exact unchanged `kdos.f` lines 2425 through 2574 define and execute the
complete userland lifecycle. Loading the slice only publishes its 18 words and
rebinds the XMEM free-span hook. Lazy initialization aligns above the live
XMEM high-water mark, assigns either an explicit rounded reserve or half the
remaining capacity to general XMEM, temporarily validates the complementary
dictionary interval, publishes its cells, and advances both XMEM HERE and
floor to the dictionary limit. The canonical post-index 128 MiB profile uses
base `0x0020_0000`, limit `0x0418_0000`, and equal `0x03f8_0000` dictionary
and general-XMEM spans.

`ENTER-USERLAND` and `LEAVE-USERLAND` use unchanged signed-delta `ALLOT`, not
an invented `HERE!` ABI. The semantic dictionary selects the corresponding
mapped physical zone, but keeps one global newest-first definition chain.
External definitions remain discoverable and executable in system mode;
Bank-0 definitions made after leaving can link back to them; re-entry resumes
the saved external frontier. Exact-limit `ALLOT` is valid, while the next
positive store faults before bytes or HERE change. Numeric rollback within
the active external zone removes bindings and rebuilds the caller-backed
index exactly as it does in Bank 0.

This lifecycle is runtime-global and unsynchronized, matching the source's
shared cells rather than inventing task ownership. Ordered transitions are
not transactional against corrupted public cells or concurrency. Capacity
failures can leave `_U-AVAILABLE` changed while the published partition stays
untouched. High positive reserve rounding can cross the signed-cell boundary
and is then rejected by the signed minimum check. External sizes not divisible
by 16 can publish a misaligned dictionary limit and XMEM floor; a
17-byte external region consequently yields a legal one-byte dictionary plus
a 16-byte reserve. Before initialization, `.USERLAND` subtracts zero from
`XMEM-LIMIT` and therefore labels the absolute external end as its reserve.

Native disabled-bound `ALLOT` checks only guarded Bank-0 geometry and can
rewind below the initial dictionary start. The hosted semantic dictionary
retains its initial-start lower bound and faults that otherwise-admitted raw
rewind. This existing safer host divergence does not affect the Bank-0/XMEM
transition and is documented without treating it as the desired native API.

The contiguous source frontier now ends at line 2574. The complete Arena
section through line 2780 already compiles under the admitted vocabulary; the
next first-failure probe reaches compile-state `[` in Buffer `IDLE` at line
2796. A hosted `IDLE` needs semantic scheduler-yield behavior, not merely an
accepted raw MP64 opcode byte.

The admitted TRNG window at `+0x800..+0x81F` is per runtime and deterministic.
Each 64-byte pool is derived reproducibly from an explicit host-injected seed
and refill counter using SHA-256. No operating-system or physical randomness
is consulted, and its output is not cryptographically secure entropy. Equal
seeds plus equal guest read and supplemental-seed schedules reproduce the same
stream, while separate runtimes have independent pools. `RANDOM`, `RANDOM8`,
and `SEED-RNG` retain the decoded aperture, byte-consumption, supplemental
future-byte mixing, zeroization, and latched-unusable behavior. Guest
`SEED-RNG` cannot recover an unusable source; only explicit host
reinitialization can do so.

The exact unchanged KDOS SHA3/random slice ending at `kdos.f` line 1216 is
qualified only in its current safe source domain. `.SHA3` uses `0 DO`, so its
length must be positive and nonwrapping; zero or negative lengths can wrap or
fail to terminate. `RAND-RANGE` requires a positive signed maximum, faults on
a zero divisor, has no useful negative-maximum contract, and is generally
modulo-biased because it does not use rejection sampling. These limitations
are not repaired by simulator-only substitutes.

## 7. Rich-terminal path

The simulator's conforming rich-terminal journey is:

```text
real rich-terminal.f source
    → hosted Forth execution
    → normal TYPE/TX-FLUSH virtual UART
    → APT-1 bytes
    → shared host transport
    → existing model/compositor/sink
```

The simulator does not implement `PT-*` as host-native replacements and does
not introduce a simulator scene API.  Desk, UIDL renderers, mounted widgets,
and applets still paint through their ordinary TUI draw boundary.  CELL remains
the complete fallback.

The transport remains byte transport.  It cannot call guest terminal words,
parse protocol on behalf of the guest, mutate the retained model during
settlement, acknowledge incomplete composition, or inject input not bound to
the acknowledged revision.

Deterministic byte-for-byte comparison pins the initial clock, virtual session
address, previous nonce, entropy, geometry, and ingress schedule.  This matters
because `PT-START` mixes freshness state with `MS@` and the session address.
When those inputs intentionally differ, a differential tool may canonicalize
only declared freshness fields after independently validating each frame's
length and CRC.  It may not normalize payload, sequence, revision, credit,
transaction, result, acknowledgement, or lifecycle differences.

Simulator presentation is host-path evidence.  It is not physical UART,
physical viewer, or exact machine-timing evidence; those remain emulator and
hardware acceptance concerns.

## 8. Storage and persistence

Compatibility runs reuse the same sector images and ordinary KDOS/VFS and
filesystem code.  Sector size, media generation, checked submission,
whole-sector progress, status values, stale-handle behavior, and operation
ordering remain observable.

A successful write means acceptance, not durability.  A successful flush is
the durability boundary and performs the corresponding host flush/fsync work.
Closing a simulator session is not a substitute for flush.  A faster host VFS
binding may exist for nonconforming development use, but it cannot stand in for
the ordinary storage journey in differential qualification.

Emulator and simulator runtime snapshots are separate formats.  Portable
persistence evidence consists of media bytes and application-level data, not
backend runtime state.

## 9. Native and warm images

MF64 v1/v2 images contain native dictionary bytes, relocations, imported
execution tokens, and executable entries.  The simulator may read, copy, hash,
and persist MF64 files as opaque data, but it does not execute them or claim to
validate their native semantics.  Source loading is the initial simulator
path.

A future semantic warm artifact uses a distinct magic, version, extension,
and manifest kind.  It is never emitted under the MF64 identity.  Its cache
key includes the simulator/runtime version, service ABI, complete source
dependency hashes, and relevant configuration.  It cannot become a production
path before cold, warm, and source/warm equivalence all pass and useful wall
time is demonstrated.

## 10. Differential authority

The architectural emulator is the default differential oracle. A documented
backend defect is not promoted into the semantic contract merely because it
is executable: the X25519 slice, for example, uses published RFC vectors and
the agreeing native C++/standalone-RTL value while recording the Python
emulator mismatch. A comparison starts from the same source revisions, copied
initial media, declared one-core capability profile, deterministic clock and
entropy, and timestamped ingress script.

Claimed comparisons include:

- lookup, binding, public stack effects, status, throws, diagnostics, and
  rollback outcomes;
- public structures and caller-owned memory bytes;
- UART and ANSI output;
- APT decoded frames and, with freshness pinned, exact bytes;
- canonical CELL and retained-terminal state, revisions, acknowledgement, and
  input ordering;
- VFS-visible files, metadata, and post-flush media bytes; and
- Desk, Pad, and Daybook state and ordinary interactions.

Comparisons exclude absolute dictionary addresses, compiled native bytes,
PC/register state, instruction/cycle counters, backend snapshots, and physical
timing.

Every admitted feature receives a focused semantic test and, where an emulator
equivalent exists, a differential vector.  The independent APT byte and state
oracles are the model for this separation: production encoders and decoders do
not define their own expected results. The hosted tile self-test is admitted
against the architectural Python emulator's corresponding public status,
failure mask, and scratch-preservation vector.

## 11. Initial implementation sequence

The simulator slices are intentionally vertical.  The current implementation
branch has an explicit pre-rich-terminal stop line:

1. package ownership, dependency guards, cells, source cursor, stacks,
   dictionary, and explicit semantic dispatch;
2. enough compiler and control-flow semantics to source-load unchanged real
   Akashic utility code, including shared return/loop-stack behavior;
3. sparse byte memory, dictionary/runtime backing, persistent compiler and
   evaluator state, exceptions, and numeric dictionary rollback;
4. the complete supported one-core semantic BIOS public vocabulary and
   platform profile, followed by ordinary `kdos.f` from source;
5. an unchanged, focused KDOS load followed by qualification of KDOS-owned
   evaluator and module-loading surfaces; and
6. stop before loading or implementing `rich-terminal.f`.

KDOS qualification maintains one monotonically advancing source frontier.
Later isolated slices may validate a cross-cutting prerequisite such as real
exception unwinding, but they do not move that frontier and are not a
substitute for filling the intervening source. Slice width is determined by
the next genuine unsupported capability rather than a fixed line count. As
the BIOS closure grows, each successful increment should span more ordinary
definitions and the remaining islands should be absorbed into one complete
`kdos.f` load.

Rich-terminal source loading, host transport integration, and the ordinary
Desk/Pad/Daybook journey remain part of the compatibility contract, but are a
later implementation phase.  They resume only after synchronizing this backend
with the then-current rich-terminal vertical, so evolving panels and terminal
core semantics are not copied prematurely into the simulator branch.

Only seconds-scale structural and focused unit tests run before the real rich
vertical exists.  Cold source load, Desktop smoke, sustained cadence,
persistence, full renderer, and physical-viewer qualification remain deferred
to vertical acceptance under the project's resource rules.
