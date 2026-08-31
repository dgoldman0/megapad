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

BIOS words and direct virtual-MMIO accesses for the same facility reach one
service implementation.  Word-only substitution is insufficient because
ordinary source contains narrow direct-MMIO paths, including UART flush and
audio control.

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

The architectural emulator is the differential oracle.  A comparison starts
from the same source revisions, copied initial media, declared one-core
capability profile, deterministic clock and entropy, and timestamped ingress
script.

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
