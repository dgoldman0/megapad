# Hosted Source Simulator Contract

Status: normative for the hosted source simulator

Contract ID: `MEGAPAD-HOSTED-SOURCE-1-2026-08-30`

This contract fixes the boundary between MegaPad's architectural emulator and
the hosted source simulator.  The simulator exists to run ordinary
MegaForth, KDOS, and Akashic source quickly.  It is a second execution backend,
not a relaxed claim that source execution proves the MP64 machine.

The architectural emulator remains the reference for machine-level claims
only. Source-visible compatibility is governed first by the locked decision
set below; an emulator defect cannot override it, and the emulator must be
corrected when it disagrees. For a claimed source-visible observation not
covered by a locked decision or documented backend defect, the architectural
emulator remains the default differential oracle. Performance is a goal and
never evidence of compatibility.

## Locked Akashic compatibility decisions

Decision set: `AKASHIC-FIRST-1-2026-09-04`

The compatibility boundary is the source-visible surface consumed by Akashic:
public word names and stack effects, status and throw conventions, persistent
record layouts, portable byte formats, module identities, and lifecycle
ordering. Emulator and simulator implementations may differ internally, but
they must not require Akashic source changes to obtain the behavior fixed
below. An accidental lower-layer result is not preserved merely because one
backend once produced it. Mathematical or cryptographic errors, host-language
undefined behavior, memory corruption, and falsely advertised capability are
corrected below the Akashic boundary.

These decisions are normative for the emulator and hosted simulator. RTL
differences remain documented implementation defects; changing or qualifying
RTL is explicitly deferred from this decision set.

Integration artifact boundary (2026-09-04): `bios.rom` is regenerated from the
merged `bios.asm` for the architectural emulator. `fpga/bios.hex` intentionally
remains the rich-terminal branch's pre-integration FPGA image and is not a
mirror of that ROM. Converging the newly locked semantic decisions into RTL,
regenerating the FPGA image, and qualifying that target are a separate landing;
emulator or simulator success is not FPGA/RTL evidence. This does not undo the
rich branch's already-integrated UART capability and physical-flush RTL work.

| Area | Locked decision |
|---|---|
| Scalar extrema | `MIN` and `MAX` compare their operands as signed two's-complement cells. Unsigned extrema, if needed, receive distinct `UMIN`/`UMAX` words rather than overloading these names. |
| Halving | `2/` is an arithmetic right shift by one, retaining the sign bit. `RSHIFT` remains the explicitly logical right-shift word. |
| X25519 | The Montgomery ladder uses `E * (AA + 121665 * E)`. The former Python-emulator use of 121666 with that same formula is an error, not a compatibility result. Published RFC 7748 vectors are authoritative. |
| Checked SHA-2 | The checked BIOS/KDOS SHA-256 and SHA-512 word ABI is authoritative: its names, stack effects, status namespace, complete-span checks, endian-visible digest bytes, ownership, finalization, and cleanup remain stable. Emulator and simulator implement that ABI; current RTL instruction glue does not qualify it. |
| Timer | `CYCLES` and Timer COUNT/COMPARE are zero-extended wrapping 32-bit values with little-endian 32-bit accesses. `TIMER!` retains the input's low 32 bits and Timer control remains a low-byte register. Simulator advancement is deterministic semantic time, not a machine-cycle claim. The current RTL low-byte-only data path is deferred. |
| NTT | The existing ten-word Forth surface and executable byte-window ABI remain stable. It denotes the current generic 256-point cyclic transform, not ML-KEM/ML-DSA negacyclic multiplication. Any future standardized-PQ transform is a distinct, versioned contract. The incompatible RTL slot map is deferred. |
| ML-KEM | The existing seven-word raw Forth surface and executable byte-window ABI remain stable. Key generation consumes a 64-byte `d || z` input and `KEM-SEED-SIZE` denotes 64 bytes; encapsulation consumes the first 32 bytes as its coin input. The RTL XOR-value block is a non-cryptographic interface stub and must not advertise or qualify ML-KEM. |
| Source loading | `REQUIRE`, `PROVIDED`, module identity, and existing public loader stack effects remain stable. A nominally successful load must check every evaluator result and call `EVALUATE-FINISH`; only complete source may commit. Each guarded frame checkpoints `HERE`/`LATEST`; an admitted failure delivered as guest `THROW` unwinds evaluator depth, rolls back provisional module IDs and the active dictionary zone, releases its allocation/frame, runs after-release, and restores ambient loader state before rethrowing. Evaluator statuses 1 through 4 throw their same positive values, status 5 rethrows the exact code retained in `EVAL-THROW`, and checked extent-read failure rethrows the exact nonzero code retained in `DISK-IO-IOR`. Successful nested module-ID chains remain provisional by merging into the parent, so a caught parent failure removes their IDs and definitions too. Task-resetting `ABORT`/`ABORT"` and backend faults that do not become guest `THROW` are outside this cleanup guarantee. Non-dictionary source effects are not thereby transactional. These are internal lifecycle repairs, not a new Akashic loader API. |
| Optional memory | A configured XMEM or HBW capacity of zero means that region is absent: SysInfo reports zero, no guest span is mapped there, and every direct allocation request targeting that region, including a zero-byte request, fails through the ordinary absent-region path. A generic allocator may still take its documented Bank-0 fallback. Zero never means “maximum available capacity.” |
| Graphics allocation | No hidden fixed-address framebuffer reservation is carved out behind the HBW or XMEM allocator. Graphics using either arena must receive caller-owned storage or obtain it through the ordinary visible allocator, then program the framebuffer base from that allocation; dedicated VRAM remains a separate explicit region. This is shared target-source composition work, not an emulator/simulator semantic difference; implementation remains beyond the present rich-terminal stop line. |
| Ring layout | The KDOS ring descriptor is the executable six-cell, 48-byte header: element size, capacity, head, tail, count, and lock, followed by data at `+48`. No seventh descriptor cell exists. |
| Hash-table layout | The KDOS hash-table descriptor remains the existing five-cell, 40-byte header followed by packed data at `+40`. Its field offsets and slot representation are Akashic-visible and are not changed as part of safety hardening. |

The following remain deliberately open: the result-versus-trap contract for
signed `MOD (INT64_MIN, -1)`; the barrier completion bit and pulse/sticky
lifecycle; any future standardized-PQ NTT identity beyond the retained generic
service; boot-time file-type, CRC, encryption, and root-policy enforcement;
and broad per-task/per-core reentrancy and lock redesign for KDOS global
scratch. Simulator-side rich-terminal source loading, live-session integration,
and current-head acceptance also remain beyond the present stop line. None of
these open matters licenses an emulator/simulator difference inside the locked
compatibility surface above.

Later byte-exact qualification ledgers retain hashes and observations for the
source revision named in each ledger. Where such a historical observation
conflicts with this decision set—most notably unsigned scalar helpers, the
32-byte KEM constant, or unchecked nominal loader completion—the decision set
is controlling. Historical evidence remains useful provenance but is not an
alternate current contract.

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
CRC instruction execution and checked transaction ownership do not. Frozen
storage sector/command/status/result/capability numbers likewise qualify as a
shared ABI registry; controller state, media ownership, DMA, checked execution,
and completion/durability publication remain backend-local. Pure marker-1
MP64FS geometry and metadata acceptance qualify for `shared/`; the three
checked reads and their scratch/controller effects remain backend-local.

The optional rich-terminal host port follows the same division. Attachment
epochs, lease validity, bounded admission, publication retention, event order,
backpressure, and retirement are one backend-neutral state machine in
`shared/rich_terminal_host.py`. Scheduler exclusion, UART drain and FIFO
mutation, geometry application, and machine-sink ownership remain explicit
backend hooks. The emulator adapter supplies those hooks without changing its
machine boundary. `SimulatorSessionBackend` supplies the same effects at outer
semantic-call boundaries, including exact UART-tail retirement and resumable
IDL ownership. It lends one backend-owned geometry state to the runtime for
the lifetime of that session: the guest-visible BIOS `COLS` and `ROWS` words
read it, while `RESIZED?` atomically consumes its sticky notification. Closing
the backend transfers an immutable value snapshot back to the unowned runtime,
so no callback into a dead session survives. `HostedTerminalGeometry` remains
the read-only host diagnostic view of that same state. `TERMSIZE` reads one
coherent pair. `RESIZE-REQUEST` publishes the two low-16-bit operands as one
asynchronous, generation-qualified request, and `RESIZE-DENIED?` consumes only
its independent sticky denial. Host acceptance and denial require the exact
still-current generation; the simulator does not invent a display policy or
automatic response. The shared policy and adapter are still not evidence that
the complete source module or a live APT session has run.

## 2. Compatibility claims

The simulator claims compatibility for:

- accepted MegaForth, KDOS, rich-terminal, and Akashic source;
- public word names, stack effects, status values, throw values, and module
  identities;
- 64-bit cell arithmetic, full-width Forth flags, little-endian byte-addressed
  data, public record layouts, and checked span behavior;
- source-visible dictionary behavior, stable execution tokens, callbacks,
  deferred words, exceptions, allocators, and transactional source loading;
- deterministic UART byte ingress, UART and ANSI output, portable
  filesystem/media bytes, network packets,
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

The admitted scalar vocabulary includes full-cell `XOR`, wrapping `CELL+`
(one eight-byte address step), the ordinary `C!` byte store, and `OFF`. `C!`
preflights exactly one addressed byte and stores
the low eight bits of its value, including for unaligned ordinary or admitted
MMIO addresses; it does not widen into a cell transfer. `OFF` pops its address
and performs the same exact unaligned eight-byte write as `!`, with a zero
value. A crossing or unmapped failure occurs after that address is consumed
and before any partial cell is published, matching executable BIOS ordering.

`W@`, `W!`, `L@`, and `L!` retain the native BIOS's explicitly bytewise
little-endian sequence rather than using an atomic 16- or 32-bit host access.
Addresses advance modulo 2^64 and each low-to-high byte is routed
independently, so unaligned MMIO reads need no wide-access alignment. A fetch
replaces its address only after every byte succeeds; a late fault preserves
the address even though earlier MMIO read effects may have occurred. A store
consumes value and address before its first byte and leaves an already-written
low-byte prefix committed if a later byte faults. `NEGATE` is ordinary
two's-complement cell negation, including the self-negating sign-bit value.

Scalar `MIN` and `MAX` compare signed two's-complement cells. This follows the
public Forth descriptions and Akashic's use of the words for signed geometry,
clipping, and clamps. The former unsigned BIOS/emulator behavior was an
implementation defect. `2/` likewise performs an arithmetic right shift by
one, while `RSHIFT` remains logical. These meanings are source-visible and
must agree between emulator and simulator.

Signed `MOD` is likewise not publicly settled for the single operand pair
`(INT64_MIN, -1)`. The current native C++ and hosted Python backends both
handle it without host-language overflow and provisionally return remainder
zero. That safe backend convention does not choose a permanent
result-or-trap contract. The qualified positive-divisor domain of KDOS
`RAND-RANGE` excludes the edge.

The simulator exposes a sparse 64-bit logical address space with the same
source-visible address classes as the machine: Bank 0, external memory, HBW,
VRAM, and MMIO.  Host pointers never enter a guest cell.  Dynamic addresses
need not equal emulator addresses, but they are stable for their documented
lifetime.  Checked spans reject overflow and cannot become valid by wrapping
through address zero.

The data stack and the logical return stack retain ordinary MegaForth
semantics. Colon continuations, user values moved by `>R` or `2>R`, and `DO`
loop state share one ordered return stack. A saved `( x1 x2 )` pair has `x1`
deeper and `x2` topmost; `2R@` copies and `2R>` restores that exact order.
`DO` places its `( limit index )` cells in the same order, so the pair words can
observe or consume a raw loop frame just as the inline native code can. They do
not search through a colon continuation. Hosted pair transfers preflight the
complete source shape and bounded destination capacity, then either transfer
both cells or fail without partial mutation.

`R@` sees the loop counter unless source has deliberately placed a balanced
value above it. A value or pair parked above a loop frame must be removed before
`I`, `J`, `LOOP`, `+LOOP`, `UNLOOP`, or `LEAVE`, because those words use fixed
return-stack positions. A helper definition likewise cannot retrieve values
below its own continuation. `I`, `J`, `R@`, `R>`, `2R@`, `2R>`, `UNLOOP`,
exceptions, and task switching must not be implemented using independent
stacks that merely appear equivalent in simple programs.

Hosted `ROLL` preserves the native mutation order: it consumes `u` before
accessing the selected depth. If the bounded hosted stack then detects
underflow, the offset remains consumed; native execution instead performs its
ordinary unchecked stack-address access.

The dictionary provides source-visible linked headers, flags and names,
newest-definition lookup, shadowing, compilation state, `HERE`, `LATEST`, and
rollback.  An execution token is a stable, nonzero 64-bit virtual code-field
address for the lifetime of its definition.  It works through `'`, `[']`,
`EXECUTE`, descriptors, deferred words, quotations, callbacks, and task
records.  No Python object identity or host function pointer may be exposed as
an execution token.

Public `FIND` searches the live, published semantic definitions newest-first
for an exact-length counted name, folding only ASCII `a` through `z`. A hit
replaces the counted-string address with the stable execution token and
returns `1` for an immediate word or the all-ones cell for an ordinary word;
a miss preserves the address and returns zero. Empty names, count bytes above
the 127-byte dictionary-name limit, and non-ASCII names are ordinary misses.
The count is read before lookup, and query bytes are read low-to-high only
while comparing a candidate of the same length. Thus an impossible count does
not require a mapped payload, while a later comparison fault leaves both
stacks unchanged. After lookup, the hosted bounded stack proves room for the
result flag before changing the input cell, so a payload fault takes precedence
over a capacity fault and either failure is stack-atomic. This is a semantic
newest-first search over live metadata: it does not consult or mutate the
optional dictionary side index, follow guest link bytes, or reinterpret
semantic code slots as MP64 instructions.

Compiled `S"` occurrences own distinct `payload + NUL` spans in their defining
colon's guest-visible body. Their semantic operation pushes that body's stable
address and source length, including when a created child enters a `DOES>`
suffix. The bytes contribute to ordinary definition growth and disappear from
the live dictionary metadata with that definition's rollback; they are not
host strings or allocations performed on each call. They remain ordinary
writable dictionary bytes, however, so raw stores or an unsafe `HERE` rewind
can corrupt them. Qualified execution assumes the payload and terminator have
not been modified: hosted IR retains the source length, whereas native
`(S")` rescans to the first NUL on each call. Compiled source containing an
embedded NUL is rejected rather than assigned one of those divergent lengths.

Interpret-state `S"` instead reuses one zero-initialized 256-byte buffer in the
protected Bank-0 prefix, publishes at most 255 payload bytes plus NUL, retains
any older tail beyond the new terminator, and does not move `HERE`.

Both hosted quote scans stop at the current physical-line boundary and accept
that boundary as an implicit terminator. This is deliberately fail-closed for
malformed input: current native interpret `S"` advances an end-of-line `>IN`
before an equality-only bound check, while compiled `S"` scans for a quote or
NUL without consulting the active TIB length, so stale bytes can be read after
a bare or unterminated form. At the 255-byte interpret limit, hosted leaves the
remaining source visible to the outer evaluator just as the native clamp does.

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

The retired user-mode ABI is preserved exactly as compatibility state rather
than revived as a simulator security boundary. `ENTER-USER` and `SYS-EXIT` are
stack-neutral no-ops, `PRIV@` always returns supervisor level 0, and
`MPU-BASE!`/`MPU-LIMIT!` retain runtime-local cells readable through the
corresponding fetchers. The MPU values do not restrict hosted memory because
the native checks are gated by the user privilege level that MegaPad removed.

`REQUIRE`, `PROVIDED`, evaluator nesting and diagnostics, compilation
transactions, rollback, dictionary-zone transitions, and allocation lifetimes
are required semantics.  JIT controls may be semantic no-ops only when their
documented effect is purely optimization.  Capability and status words must
report the simulator's actual support.

### Semantic evaluator and KDOS checked source

The admitted hosted BIOS vocabulary includes `EVALUATE`, the early
`EVALUATE-CHECKED`, `EVALUATE-FINISH`, `EVALUATOR-RESET`,
`EVALUATOR-UNWIND`, `EVAL-STATUS`, `EVAL-LINE`, `EVAL-COLUMN`, `EVAL-DEPTH`,
`EVAL-THROW`, and `EVAL-TOKEN`. `EVALUATE ( addr len -- )` interprets one
physical input's raw bytes, including carriage returns, with a maximum length
of 255. LF-delimited multi-line input belongs to KDOS
`SOURCE-EVALUATE-CHECKED`. An overlength request rejects before reading the
guest address. The one-core compiler, open-definition state, and compile-time
control stack persist across normal evaluator calls, permitting definitions
and conditionals to span inputs.

The early BIOS `EVALUATE-CHECKED ( addr len -- status )` returns 0 for success,
1 for the first undefined token, 2 for overlength input, and 3 for evaluator
nesting exhaustion. It preserves ordinary source data-stack effects.
`EVALUATE-FINISH ( -- status )` returns 4 if an open definition or cross-input
conditional remains, otherwise 0. `EVALUATOR-RESET` clears persistent compiler
bookkeeping after recovery but deliberately retains the last status and
diagnostics.

`EVAL-STATUS`, `EVAL-LINE`, `EVAL-COLUMN`, `EVAL-DEPTH`, and `EVAL-THROW`
return five distinct, zero-initialized guest-memory cell addresses.
`EVAL-TOKEN` returns a stable `( addr len )` view whose protected 256-byte
guest buffer begins zeroed and is not reclaimed by numeric dictionary rollback.
Undefined-token diagnostics retain the caller's line context, zero-based byte
column, and exact raw token. A nested evaluator failure is sticky through its
enclosing inputs: neither the failed inner tail nor the outer tail executes,
and the first diagnostic remains authoritative.

If a guest `THROW` crosses `EVALUATE` into a guest `CATCH`, its logical
evaluator frame remains present until KDOS explicitly accounts for the
abandoned depth. `EVALUATOR-UNWIND ( depth -- )` ignores negative and
above-current checkpoints and drops every abandoned logical frame only for a
valid prior depth. By contrast, an exception that escapes the public host
boundary, including a host abort, active-step-budget exhaustion, or
implementation error, clears hidden evaluator frames and unfinished compiler
state before the context can be reused. Nested guest evaluation remains
charged to the active outer public step budget; it does not reset or enlarge
that budget.

Exact KDOS source shadows the early checked primitive with a guest `CATCH`
wrapper. Normal evaluator statuses 0 through 3 pass through. A caught nonzero
guest `THROW` consumes the restored evaluator arguments, records the exact code
in `EVAL-THROW`, reconstructs abandoned input depth through
`EVALUATOR-UNWIND`, records status 5, and returns normally.

`SOURCE-EVALUATE-CHECKED ( addr len -- status )` walks LF-delimited physical
input, strips one terminal CR from each line, skips blank lines, and supports a
final line without LF. It publishes one-based `EVAL-LINE`, stops at the first
nonzero line status, and calls `EVALUATE-FINISH` only after ordinary end of
input. Status 4 therefore denotes unfinished compiler/control state. It does
not roll the dictionary back: a transaction caller saves `HERE`/`LATEST`,
invokes `DICT-ROLLBACK` after failure, and then calls `EVALUATOR-RESET`.
Rollback removes already completed definitions in the transaction; reset
discards the hidden unfinished compiler while retaining diagnostics.

This state is runtime-global for the one admitted core and is not a concurrent
evaluator contract. Ordinary interpret-mode `IF`/`ELSE`/`THEN` uses the native
anonymous temporary-compilation lifetime: it persists across physical
`EVALUATE` inputs, executes only at the outer `THEN`, clears its temporary
bytes, restores `HERE`, and publishes no dictionary word. This is distinct
from conditional-compilation `[IF]`, which remains unqualified. The admitted
surface also does not publish or qualify public `SOURCE`, `>IN`, or `STATE`, or
direct LF-containing guest `EVALUATE` input. Filesystem `LOAD` deliberately has
a narrower raw source domain and different failure behavior, specified below.
The contiguous KDOS source frontier now reaches EOF at line 9894.

The current profile advertises one full core and `CRYPTO_CAPS = 0x7`: bit 0 is
the admitted semantic reflected/raw CRC service, bit 1 is checked SHA3/SHAKE
streaming, and bit 2 is raw Keccak-f[1600]. Bit 3 remains clear because the
hosted WOTS chain is not admitted. It advertises no additional cores,
accelerator timing, other crypto bits, devices, or sinks until their public
contracts have an implementation and differential evidence.

## 5. Scheduling and time

The target design calls for a deterministic cooperative scheduler rather than
uncontrolled host threads: given the same runnable set, yield sequence, clock,
entropy, and ingress schedule, public task ordering and state must repeat. The
current one-core profile does not implement that target scheduler. It admits
the unchanged KDOS registry and table-ordered synchronous executor: selected
task XTs run inline to return on the caller's context. That behavior is
deterministic, but it is neither host threading nor cooperative task
scheduling.

It does implement the narrower architectural `IDL` boundary needed by
unchanged source. `run_until_blocked` detaches a compiled-word dispatch after
the semantic IDL operation while retaining its exact next instruction,
ordered return stack, root identity, pointer-capture guard, and original step
meter. The context and dictionary remain leased until cancellation or resume.
A runtime-issued, one-shot receipt bound to that exact suspension and an
explicit host-delivered interrupt or DMA wake is required before execution
continues. The wake is a semantic host event attestation; this profile does
not claim interrupt vectoring, an ISR, a DMA engine, latency, fairness, or
physical timing.

Only one compiled-word suspension is admitted at a time. Interpreted source
evaluation and a Python primitive's nested public dispatch have Python-owned
continuations that are not captured by this seam, so reaching IDL there fails
explicitly and restores the ordinary dispatch guard. A suspended context
rejects stack changes at resume, and dictionary mutation is rejected while a
suspension is detached. Resumption retains the original cumulative step
budget rather than silently granting a fresh quantum. This is an IDL
block/wake contract, not BIOS `PAUSE` or KDOS task scheduling.

Cancellation restores the pre-dispatch return stack. If the canceled path
observed `RP@`, the context is marked non-reusable because a data-stack copy
may still name detached continuation storage; cancellation never licenses a
later `RP!` to revive that control state. Failure while atomically publishing
a later IDL suspension after resumption receives the same fail-closed
treatment: no partial lease survives, the original return stack is restored,
and an `RP@`-observing context becomes non-reusable.

The hosted BIOS `KEY` service uses that same boundary rather than treating an
empty receive queue as zero or as end of input. Each runtime owns a FIFO byte
queue populated only by explicit bytes-only host injection. `KEY?` reports a
full-width Forth flag without consuming; `KEY` removes one byte immediately
when present, otherwise executes one semantic `Idle`, retries after an admitted
wake, and reblocks after an inputless wake. Input injection itself may occur
while the exact dispatch is suspended, but continuation resumption still
requires the matching one-shot admitted wake receipt; a UART host ordinarily
delivers an interrupt wake. The service does not echo, synthesize input, or
advance a clock. Interpreted source and nested host dispatch cannot yet detach
their Python continuations, so a caller using those paths must prequeue enough
input to avoid blocking.

Task descriptors, registry state, synchronous `SCHEDULE`, and the provisional
checkpoint are now qualified source semantics. `TASK-STACKS` and the saved
DSP/RSP fields are inert guest storage: execution never installs them. Active
per-task data/return stacks, suspension and resumption, priority scheduling,
task-local exception ownership, genuine `PAUSE`/`TASK-YIELD`, and preemption
remain unimplemented. A future multicore profile must preserve publication
order, generations, locks, and barriers, but it still does not qualify
physical races or arbitration.

Two eventual full-clock modes are permitted:

- deterministic virtual uptime and epoch for tests and differential runs; and
- host-monotonic pacing for interactive use.

The currently admitted surface is narrower and implements neither automatic
mode. One runtime-local deterministic epoch-millisecond register is routed at
MMIO `+0xB08..+0xB0F`; it defaults to zero and changes only through explicit
host set/advance operations or admitted direct MMIO writes. Host advance is
nonnegative and wraps modulo 64 bits. Reading the low byte latches the current
value, later byte reads use that latch, and `EPOCH@` reads the eight ascending
little-endian bytes into one `u64`. Supported direct access widths are 1, 2, 4,
or 8 bytes wholly contained in that subwindow. Writes change current register
bytes without changing the prior read latch.

This qualification does not admit `MS@`, uptime registers, calendar, alarm,
control/status, automatic scheduler-driven advancement, realtime pacing, or
host wall time. Those remain part of the future full-clock modes above.
Simulator ticks are diagnostics, not MP64 cycles.

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
existing unsupported or absent status when its ABI has one; otherwise it
fails explicitly. It never silently reports success.
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

That topology has no synthetic secondary worker. `CORE-STATUS` accepts only
core ID zero and replaces it with zero, meaning the core-0 secondary-worker
dispatch slot is idle; it is not a claim that the CPU itself is stopped.
Out-of-topology IDs fail without consuming their operand. `WAKE-CORE` always
fails without consuming either XT or core ID, because no valid secondary
target exists. It never resolves or executes the XT and creates no host thread,
worker slot, mailbox, IPI, asynchronous completion, or hidden no-op success.

It also has no micro-core cluster. `CLUSTER-EN@` returns zero, and
`CLUSTER-EN!` accepts only zero as an idempotent disable; a nonzero mask fails
before it is consumed. This semantic BIOS substitution is coherent with the
direct hosted SysInfo value at `+0x18`, while the direct SysInfo window remains
read-only. `BARRIER-ARRIVE` and `BARRIER-STATUS` fail without stack mutation,
so software cannot mistake an absent barrier for completion or enter a hidden
infinite poll. `CL-PRIV*` and `CL-MPU-*` similarly fail without consuming a
store operand or pushing a fetch value because there is no caller-relative
cluster register domain.

`SPAD` remains an address-producing BIOS word and returns the architectural
sentinel `0xFFFF_FE00_0000_0000`; the simulator maps no storage there, so an
access fails through the normal unmapped-memory contract. `MICRO?` preserves
the executable BIOS's unsigned `id >= N-FULL` classification rather than
performing topology validation. With `N-FULL = 1`, ID zero is false and every
other uint64 cell is true, including cells that cannot name a hosted core.
These choices claim source-visible failure and classification behavior only,
not cluster execution, scratchpad storage, barriers, privilege, or MPU
enforcement.

The NIC is absent from the admitted hosted profile. Pseudo-BIOS `NET-STATUS`
returns zero, so TX-busy, RX-available, link, error, DMA-busy, and present are
all clear. This is sufficient for ordinary feature/status branching but does
not admit the direct NIC MMIO window, `NET-SEND`, `NET-RECV`, `NET-MAC@`,
frame queues, DMA, host networking, or interrupt delivery. A direct access to
the architectural NIC aperture therefore remains an MMIO fault rather than
an alias of the pseudo-BIOS status.

The currently admitted UART surface is pseudo-BIOS byte I/O, not the physical
UART window. Output bytes become observable synchronously, making the native
TX flush before `KEY` an observational no-op. Input is the deterministic FIFO
specified above. No UART status register, RX/TX ring geometry, capacity,
overflow, baud rate, interrupt timing, or direct-MMIO behavior is claimed by
this slice.

The hosted diagnostic profile is intentionally backend-local. `PERF-CYCLES`
is a persistent, wrapping count of dispatched semantic work, not
wall time, instructions, or MP64 cycles. Stall and external-beat counters
remain zero; `PERF-TILEOPS` counts completed operations in the admitted
semantic tile service. BIST getters
retain the boot/profile snapshot, while destructive `BIST-FULL` and
`BIST-QUICK` fail before changing guest memory or retained BIST state. Their
admitted, faulting dispatch still counts as semantic work. The tile
self-test completes synchronously through the production 64-lane unsigned
ADD/MUL/DOT/SUM value kernel using host-only buffers. I-cache controls are
logical optimization state, dispatch remains immediately coherent, and its
hit/miss observations are zero. None of these diagnostic substitutions is
evidence for pipeline timing, physical RAM coverage, tile hardware, or a
physical instruction cache.

Pseudo-BIOS `CYCLES` reads a retained per-runtime 32-bit Timer counter. The
Timer is shared by that runtime's contexts, isolated between runtimes, starts
in the post-BIOS enabled state, and advances once before each admitted semantic
operation. It wraps at 32 bits and is unaffected by `PERF-RESET`; the internal
64-bit semantic-work diagnostic remains separate. The successful `CYCLES`
dispatch therefore contributes the tick it returns. Disabling timer control
freezes `CYCLES` even though semantic-work accounting continues.

`TIMER!` atomically retains the input cell's low 32 bits as compare and
`TIMER-CTRL!` retains its low byte. Control bit 0 enables advancement, bit 1
enables IRQ latching on a match, and bit 2 selects auto-reload. A newly reached
compare value sets sticky status bit 0, latches pending IRQ only when bit 1 is
set, and resets the counter to zero only when bit 2 is set. Compare equal to
the current counter is not immediate; compare zero next matches only after a
complete wrap. Compare/control writes do not clear counter, status, or an
already pending latch. `TIMER-ACK` clears sticky match and pending IRQ.

An `IDL` operation advances the Timer before suspension. No Timer step occurs
while the dispatch is detached or merely because the host delivers a wake;
resumed guest operations advance it normally. Pending IRQ state does not
vector, mutate `PREEMPT-FLAG`, or authorize/wake `IDL`. Raw Timer MMIO,
hardware cadence, and wall-clock pacing are not admitted. Emulator/native
implement the intended 32-bit timer accesses, while the current RTL SoC byte
peripheral wiring exposes only `COUNT_LO` to `CYCLES` and accepts only
`COMPARE_LO` from `TIMER!`. The full 32-bit behavior is the locked ABI; RTL
convergence and qualification are deferred rather than normalized into another
hosted mode.

The admitted one-core legacy tile service binds `TMODE!`, `TCTRL!`,
`TSRC0!`, `TSRC1!`, `TDST!`, `TADD`, `TSUB`, `TMUL`, `TDOT`, `TSUM`,
`TMIN`, `TMAX`, `TSUMSQ`, `FP16-MODE`, `BF16-MODE`, and `ACC@`. It retains
low-byte TMODE/TCTRL, full-cell addresses, and ACC0--ACC3;
ACC, TSRC0, and TDST are the same state observed by the hosted Field-ALU
surface. TCTRL zero-first is consumed only by a successful reduction or DOT.
For integer reductions, accumulate adds the new result to the existing
modulo-2^256 ACC value even for TMIN/TMAX. Binary operations read both complete
sources before writing the destination, so valid aliasing observes one
pre-operation pair of tiles.

Integer widths 8, 16, 32, and 64 implement wrapping or saturating ADD/SUB,
wrapping MUL, and signed-aware SUM/MIN/MAX/DOT/SUMSQ. FP16 and BF16 implement
ADD/SUB/MUL plus SUM/MIN/MAX/DOT/SUMSQ. Floating reductions and DOT place raw
binary32 bits in ACC0 and clear ACC1--ACC3; MIN/MAX skip NaNs and ignore
ACC_ACC, while SUM/DOT/SUMSQ add the binary32 value already in ACC0 when
requested. The signed and saturating mode flags are ignored for floating
formats. Reserved EW 6/7 and all unbound tile/TACC words fail rather than
silently aliasing another format.

Each used operand is the exact addressed 64-byte span and must fit one ordinary
mapped region; MMIO and crossing or wrapping spans are rejected before
destination or accumulator publication. Reductions read only TSRC0, matching
their logical and RTL data dependency; the Python emulator currently performs
an unused eager TSRC1 read.
This exact-address rule is a hosted contract choice while the prose, untimed
emulators, RTL, and strict-cycle transport still disagree about unaligned tile
addresses. Low-byte CSR writes and full 256-bit reductions follow the
documented Python architectural oracle. The native C++ and RTL TMODE/TCTRL
write paths retain higher bits, while RTL's legacy integer reduction
accumulator is narrower; both remain explicit discrepancies.
The service makes no MEX encoding, CSR, scratchpad, latency, flag, pipeline, or
hardware-throughput claim.

For FP SUM/SUMSQ the hosted service deliberately follows the executable Python
oracle: one host-language `sum` over a tile followed by one binary32 pack.
TDOT uses an explicit binary64 loop followed by the same pack. The native
accelerator currently falls back to Python for SUM/SUMSQ, although its bypassed
direct C++ implementation is sequential binary32; RTL uses a balanced binary32
tree.
With ACC_ACC, hosted execution decodes the existing binary32 value in ACC0,
widens it, adds it to that tile's subtotal in binary64, and packs once again;
that final pack is the inter-tile rounding point.
Results can therefore differ under cancellation, and Python's `sum` algorithm
is itself interpreter-version-sensitive. This is a recorded compatibility
choice, not a resolution of the hardware contract. Python/C++ conversion also
maps the FP16 product `0x0017 * 0x5190` to zero when an IEEE round-to-even carry
would produce minimum-normal `0x0400`; the shared hosted value model preserves
that executable behavior. Reserved EW 6/7 fail closed here even though current
Python/C++ and RTL implementations alias them differently. RTL scalar FP TRED
retains ACC1--ACC3 on overwrite or accumulate whenever ACC_ZERO is not taken,
and FP DOT retains them on ACC_ACC; Python and hosted operations always clear
them.

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
SHA-2 evidence. The checked BIOS/KDOS behavior above is the locked public ABI;
future RTL or instruction-glue work must implement that behavior without
changing Akashic-facing words. That RTL work and its qualification are deferred
from the present emulator/simulator change.

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

The emulator and hosted result follow RFC 7748, native C++, and the standalone
Field-ALU RTL by using `A24=121665` with `E*(AA+A24*E)`. The former
architectural-Python value 121666 with that same formula was an implementation
error and is not a retained compatibility mode. Current integrated RTL remains
a separate deferred discrepancy: full-core crypto dispatch treats units
beyond CRC/SHA as no-ops, while the microcore's declared Field ports are not
connected by the cluster. Hosted qualification therefore does not claim
integrated-RTL agreement. It also makes no claim about EXT.CRYPTO encodings,
CSRs, the nominal 4335-cycle latency, stalls, interrupts, constant-time host
execution, or host-memory erasure.

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
negacyclic ring operation required by ML-KEM or ML-DSA. Exact current `kdos.f`
lines 1517 through 1584 contain 68 LF records and 2,784 bytes, with SHA-256
`95769988473110183b3b2adcc90a2eb3bdd812100ab1702f8686d573af1f4194`
and Git blob `d4f2ce38b6818520b0227f5a2f8c69aef3c408b6`. They define both named
moduli, selectors, two global 1024-byte scratch buffers, `NTT-POLYMUL`, and
`.NTT-STATUS`; the PQ labels do not strengthen that mathematical claim. The KEM emulator uses
separate ML-KEM-specific routines. KDOS scratch aliases and concurrent
`NTT-POLYMUL` calls are unsafe because there is no ownership protocol.

This frontier is deliberately pseudo-BIOS-only: it does not admit direct
virtual NTT MMIO. The working BIOS/Python byte window is the retained
emulator/simulator ABI: STATUS `+00`, Q `+08..0F`, IDX
`+10..11`, A `+18..1B`, B `+1C..1F`, RESULT `+20..23`, and CMD `+28`.
Current RTL also fixes its twiddle tables and inverse scale to q=3329 even when
Q changes, exposes multi-cycle BUSY/partial work, and produces a different
forward ordering for its fixed root. BIOS byte accesses cannot drive that RTL
unit correctly. Simulator success therefore makes no direct-MMIO, RTL,
standardized-PQ, cycle, bus-width, constant-time, or physical-erasure claim,
and RTL convergence is deferred. A future standardized-PQ transform must use a
distinct, versioned identity rather than silently changing this generic cyclic
service.

The admitted ML-KEM slice is one runtime-global semantic service behind the
seven authoritative raw BIOS words: `KEM-SEL!`, `KEM-LOAD`, `KEM-STORE`,
`KEM-KEYGEN`, `KEM-ENCAPS`, `KEM-DECAPS`, and `KEM-STATUS@`. Exact current
`kdos.f` lines 1586 through 1633 contain 48 LF records and 1,510 bytes, with
SHA-256 `58fab7b6c7a7e722ca1d3bddf77046e700ed196084c0fa1a69608222b800f824`
and Git blob `5e74d7b947598492bc8ddc82a646687eb0eeaddb`. They define five buffer IDs, five size constants,
the three `KYBER-*` wrappers, and `.KEM-STATUS`. The pre-decision source
declared `KEM-SEED-SIZE=32` even though `KYBER-KEYGEN` loads all 64 bytes
consumed as `d || z`. `AKASHIC-FIRST-1-2026-09-04` resolves that discrepancy:
the public constant is 64 and both emulator and simulator consume the complete
64-byte input. Encapsulation continues to consume its first 32 bytes as coins.

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
the dictionary. Exact current `kdos.f` lines 2044 through 2108 contain 65 LF
records and 2,448 bytes, with SHA-256
`5fc825c8588b85a499ee34e7fc142b8bba7e74d7efb481bde4183c93476444c9`
and Git blob `2d9704f542181bbf91eaead01d5b6ea7a1f9cff0`. They define
`HBW-HERE`, `HBW-LIMIT`, `HBW-INIT`, `HBW-ALLOT`, `HBW-ALLOT?`,
`HBW-TALIGN`, `HBW-RESET`, `HBW-FREE`, and `.HBW`, and execute `HBW-INIT` at
load time.

Those two variables are runtime-global guest state shared by all contexts.
There is no task/core owner, lock, transaction, allocation ledger, floor,
individual free, or automatic wipe. With HBW present, allocation returns the
old pointer and advances by exactly the supplied cell without touching
storage; zero and exact fit succeed. Ordinary checked overflow returns
`(0,-1)`, while the aborting
form emits `HBW overflow` and performs task `ABORT`; both leave the pointer
unchanged because their store follows the comparison. `HBW-TALIGN` rounds the
pointer up to 64 bytes. `HBW-RESET` rereads the base but does not clear memory,
revoke old addresses, or synchronize callers. `.HBW` reads live state and uses
the current numeric base plus signed `.` formatting.

The allocator reserves none of the advertised span for other subsystems. The
current `graphics.f` choice of `HBW-BASE + 0x200000` without moving `HBW-HERE`
can overlap an allocation entering the third MiB and is not the retained
design. Graphics using HBW or XMEM must instead receive caller-owned storage
or allocate through the ordinary visible allocator and program the framebuffer
base from that result; dedicated VRAM remains separate. Both backends will
load the same target-source composition; no
simulator-only reservation or emulator device special case is permitted.
Implementing that source change remains beyond the present rich-terminal stop
line.

The admitted allocation domain requires the current pointer and a nonwrapping
request to stay within the mapped HBW span. The source names the request `u`
but adds before applying signed `>` and performs no wrap check, so high-cell
requests can wrap and succeed. Alignment is also unchecked and can cross a
configured limit that is not 64-byte aligned. Canonical base
`0xFFD0_0000`/size 3 MiB is aligned. The hosted factory can explicitly model
no HBW and then reports `(0,0)`. Configured zero is now the same absent-region
case in the emulator: it retains no guest-visible base or mapped span, and all
HBW allocation requests, including zero bytes, fail. `HBW-ALLOT` reports
`HBW unavailable`; `HBW-ALLOT?` returns `(0,-1)`. Any RTL zero-parameter
difference remains deferred rather than becoming another public meaning.

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
configured size zero as no external region. That meaning is now normative for
every optional XMEM or HBW region: SysInfo reports zero and no guest span is
mapped there. The normal emulator session profile may still select a nonzero
default such as 128 MiB explicitly. The current RTL parameter's use of zero
for the maximum window up to VRAM is an implementation discrepancy whose
correction is deferred.

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
initializer. `2*` wraps one-cell left shifts. `2/` is an arithmetic right
shift; this sizing source supplies positive values and is therefore unchanged
by correction of the former logical-shift implementation. Canonical 128 MiB
XMEM reserves 65,536 slots (1 MiB) and advances
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

The Arena portion of the contiguous source frontier ends at line 2780. Exact
unchanged lines 2576 through 2780 add all 31 Arena definitions without a host
allocator shortcut. `A-HEAP` follows public `ALLOCATE`/`FREE` and therefore uses
prefixed XMEM when present and Bank 0 otherwise; `A-XMEM` uses raw recyclable
XMEM blocks; `A-HBW` advances and later abandons HBW backing until
`HBW-RESET`. Dictionary descriptors consume 32 bytes permanently, while
`ARENA-NEW-AT` leaves HERE unchanged and publishes into caller storage.

The admitted ordinary domain uses positive representable sizes, valid
source IDs, live descriptors, and genuine snapshots. Current source does not
enforce that domain completely. Bump allocation applies wrapping
`7 + -8 AND` before a signed `<` capacity comparison: the highest seven cell
patterns round to zero, while other sign-bit-set aligned requests can pass,
wrap the pointer below its base, and make used/free accounting nonsensical.
HBW-backed construction also inherits raw `HBW-ALLOT?` request wrap.
Rollback accepts every address in the inclusive descriptor interval,
including future or unaligned values never returned by `ARENA-SNAP`; it does
not authenticate a token or require backward movement.

Backing allocation precedes the four independent descriptor stores. A later
dictionary fault in `ARENA-NEW`, or an invalid/crossing caller destination in
`ARENA-NEW-AT`, can therefore consume backing and partially publish or leave
no descriptor from which to reclaim it. The four-cell `ARENA-STK` and
`ARENA-SP` are one runtime-global unsynchronized selection stack, not
per-task/per-core state; direct `ARENA-ALLOT` on exclusively owned descriptors
does not have that selection race. These are pinned source-contract gaps, not
host-side repairs.

Exact unchanged lines 2782 through 2796 add Buffer's general `IDLE` helper.
The hosted compiler keeps
an open definition distinct from its compile/interpret mode, preserves the
native immediate `[` and non-immediate `]` flags, and translates the exact low
byte emitted by `[ 0 C, ]` into one semantic `Idle` IR operation. No raw byte
is stranded at hosted HERE, and any other bracketed raw opcode fails closed.
Execution blocks after that operation and resumes only through the exact
one-shot wake contract above; treating IDL as a no-op or ordinary cooperative
task yield is outside the contract. Public memory-backed `STATE`, general raw
native-code emission, and executing a `]` compiled while already in compile
state remain pending; that unsupported `]` form fails during source
compilation. Persistent one-core compiler state across evaluator calls is now
part of the semantic BIOS prerequisite above.

Exact current lines 2797 through 2985 contain 189 LF records and 7,084 bytes,
with SHA-256
`68826ac284decca406051412e4478710dd9ebd81319109f5dd326a04ca205a93`
and Git blob `cbf10f550499d59aa5c6721024ee4ba46b8b0acb`. They execute the Buffer registry,
descriptor accessors, ordinary,
HBW, XMEM, and Arena constructors, byte fill/inspection, and Arena destruction
integration against the same guest dictionary and sparse memory used by prior
slices. There is no host-side buffer object table. `(BUF-REG)` appends a
16-byte dictionary link and publishes it at `BUF-HEAD`; traversal and
`BUF-NTH` therefore see newest-first order. `BUF-COUNT` has no fixed 16-entry
capacity, and `BUF-NTH` deliberately retains the source's lack of bounds
checking.

Constructor effects retain source order rather than receiving a hosted
transaction. Descriptor fields, allocator movement, registry links/count, and
the final named constant can consequently be only partly published when a
later step fails. Width and length multiplication wraps as cell arithmetic and
the source does not validate its documented type/width conventions.
`BUFFER` and `HBW-BUFFER` align the selected data frontier to 64 bytes, while
`ARENA-BUFFER` rounds its data request only to eight bytes. `B.PREVIEW` always
reads 64 bytes in four rows and uses current `BASE`; it does not force hex or
clip a short buffer. `B.TILES` adds 63 to the wrapped byte count and then uses
signed `/`, so its ceiling calculation is qualified only for ordinary
nonnegative sizes that do not overflow either arithmetic step.

`XBUFFER` and `HBW-BUFFER` publish the exact address returned by their
allocator, including an address reused from the XMEM free list. Construction
is still not transactional: an allocation failure can leave a partial
descriptor whose data-address field remains zero. Arena destruction unlinks
matching descriptors and decrements `BUF-COUNT`,
but it does not reclaim the dictionary link nodes or undefine constants that
still contain the destroyed descriptor address. These are recorded source
behaviors, not reasons for simulator-only repair. `ARENA-RESET` makes Arena
storage reusable without unregistering its Buffer descriptors, and dictionary
rollback after a publication does not repair `BUF-HEAD` or `BUF-COUNT`; both
paths can therefore leave stale registry state.

Exact unchanged lines
2986 through 3109 publish `B.SUM`, `B.MIN`, `B.MAX`, `BTMP-NTILES`, `B.ADD`,
`B.SUB`, and `B.SCALE`. The five tile-backed words force unsigned-byte TMODE;
descriptor width changes byte and tile counts but never selects wider lanes.
`B.SCALE` never reaches the tile service: it scales exactly `B.BYTES` through
scalar `C@`, wrapping multiplication, `255 AND`, and `C!`.

`B.TILES` rounds up, but the tile instructions always touch 64 physical bytes.
SUM/MIN/MAX therefore include a partial tile's trailing bytes, while ADD/SUB
can read or overwrite beyond the logical source/destination. ADD/SUB take their
count only from the leftmost stack argument named `src1` (loaded into TSRC0),
do not validate the other descriptors, and share global
`BTMP-NTILES`. B.MIN/B.MAX are correct for one tile, but after the first
iteration their stack order makes `DUP TSRC0!` install the running extreme as
the next address. Empty MIN/MAX explicitly return zero; empty SUM, ADD, SUB,
and SCALE use `0 DO`, enter the body, and cannot complete normally before
64-bit index wrap, although an invalid memory access can fault first. These
defects are pinned rather than repaired by host objects.

Exact unchanged lines
3110 through 3216 publish `F.SUM`, `F.DOT`, `F.SUMSQ`, `F.ADD`, `F.MUL`,
`BF.SUM`, and `BF.DOT`. These words treat every complete physical tile as 32
little-endian half lanes without validating descriptor type, width, whether
`B.BYTES` is even, or cross-descriptor sizes. Reductions return raw binary32
bits through `ACC@`; binary words and DOT take their tile count only from the
leftmost stack argument named `src1`, which is loaded into hardware TSRC0. A
partial logical tail therefore participates or is overwritten, and with an
ordinary `BUFFER` that access can reach registry or dictionary bytes
immediately following the allocation.

Every zero-sized FP word inherits the unsafe `0 DO` entry behavior. On normal
return each word restores TMODE to hard-coded zero rather than the caller's
prior mode, and reductions leave TCTRL at one. A tile-loop memory fault or
budget fault before the final `0 TMODE!` leaves FP16/BF16 mode installed. The
source's example `0 1 64 BUFFER` occupies one correct physical tile but
describes 64 one-byte elements; `0 2 32 BUFFER` matches its stated 32-element
descriptor.

The kernel/pipeline portion of the contiguous source frontier ends at line
3754. Exact unchanged lines 3217 through 3754 contain 538 lines and 16,586
bytes (SHA-256
`ec724b8ca6f6887a2c4ce724edf9612726cf04a48416c29c2eb3ed9448949e40`).
They publish 109 definitions: the 32-slot kernel registry and 23 populated
descriptors, 18 registered byte/general descriptors plus five FP16 descriptors,
the eight-slot pipeline registry, three fully populated demo pipelines, and six ordinary
registered Buffer objects. Loading executes the normal defining words and
allocates 2,752 Buffer payload bytes; it does not construct host-only kernel or
pipeline objects. Normal demo execution reaches the existing Buffer/tile paths
and ordinary UART publisher.

Registry limits and source lifecycle remain literal. A kernel or pipeline
created after its table fills still allocates a descriptor and defines its
constant but is silently omitted from the registry. `P.ADD` silently discards
an XT at capacity; `P.CLEAR` zeros only the count and leaves old step cells;
`P.GET`/`P.SET`, counts, capacities, and shared construction scratch are
unchecked and non-reentrant. Negative or corrupted values can index before a
table, rewind `HERE`, or drive an unsafe loop. There is no unregister/destroy
path.

Several sample names describe intent more strongly than their current source.
`kavg` only copies through its fixed 256-byte scratch and ignores the recorded
window; `kdelta` emits `src[0]`, not zero, for its first result. `kpeak` handles
ordinary inputs but zeroes the destination and then underflows during cleanup
when the byte count is below three. The registered `krms-buf` divides by zero
for mean square one and its fixed Newton iterations are not exact over the
whole byte domain; the unused `krms` loses its descriptor. `kconvolve3` also
uses fixed 256-byte scratch. Oversized copies can overwrite following
dictionary state and are documented rather than exercised. Eleven byte/tile
loops in this slice use unguarded `0 DO`; representative bounded acceptance
pins the resulting nontermination instead of treating zero as an empty loop.
Earlier Buffer tail, count-owner, and multi-tile defects flow through their
kernel wrappers unchanged.

Exact unchanged lines 3755 through 4099 contain 345 lines and 11,424 bytes
(SHA-256
`e4d09d0801838fc9721ba68e39f2c5a5dbc139101c9c4a3489fb66cab9b248b1`).
They publish 97 definitions through `VOL-FLUSH`: the storage constants and
structured ior vocabulary, block-device and volume field readers, unsigned
range predicate, cookie allocator, descriptor validators/lifecycle, guarded
block I/O, raw and bounded volume constructors, reference accounting, and
relative volume I/O. Source load performs no media operation and explicitly
initializes only `STORAGE-COOKIE`.

Exact unchanged lines 4100 through 4669 contain 570 lines and 18,979 bytes
(SHA-256
`bf46ad3acc9deaf380ac4229fe9196219fc0111df8d8f5a6650ffa95fb766112`).
They publish 110 definitions through the locked public `MBR-SCAN`, `GPT-SCAN`,
and `PART-SCAN` entry points: raw fallback, transactional MBR discovery, dual-
copy GPT structure and CRC validation, staged volume publication, structured
partition iors, caller-owned output/workspace, and KDOS-global parser scratch
serialized by lock 0. Load time only constructs the dictionary and zero-filled
scratch cells; it performs no disk, CRC, or lock operation.

Hosted acceptance runs ordinary source against external raw, MBR, and GPT
images. It covers holes and preserved source indices, a cross-sector GPT entry
with an exact partial CRC tail, structured corruption/capacity/workspace
failure, mode-4 absence and contention, old-reference release, and a media
generation swap during later array reads. The caller must provide pairwise-
disjoint writable block descriptor, output, and workspace extents; the source
does not prove those spans or make a scan a same-medium content snapshot.

Exact unchanged lines 4670 through 4803 contain 134 lines and 4,127 bytes
(SHA-256
`7ba6cb19989623363d2e78ac45ae81b1b7e4bb2ad51864005bfbb35b1f768199`).
They publish 24 definitions through `DISK-INFO`: singleton raw storage
binding, a borrowed selected-volume pointer, cache-validity gating, retained
compatibility diagnostics, checked and aborting I/O wrappers, and the legacy
Buffer sector helpers. Loading allocates singleton bodies without explicitly
clearing their extents; virgin hosted memory supplies the descriptor-contract
zeros in the qualified cold runtime. It creates zero-initialized diagnostic
variables, points `FS-VOLUME` at still-invalid `SYSTEM-RAW-VOLUME`, and
explicitly clears `FS-OK`, without opening media, transferring sectors,
flushing, or printing.

`STORAGE-OPEN` is a destructive management operation: it attempts
`VOL-CLOSE`, attempts `BD-CLOSE`, discards both results, and only then calls
`BD-OPEN`. An extra live volume can therefore leave the block object valid,
clear the singleton raw volume, and make the new open fail `BD-E-BUSY`; there
is no rollback. The word also does not clear `FS-OK`. Direct callers must
invalidate caches first.
`FS-VOLUME!` validates and borrows its argument without acquiring a reference;
rejected selections preserve the old pointer and cache marker. An invalid
selection with a nonzero `FS-OK` makes `STORAGE-ENSURE` clear the marker and
return `VOL-E-STALE` without reopening; structurally valid stale selections
fail closed on every later call until explicit replacement or reselection.

Read/write compatibility wrappers retain raw status, actual completed count,
and structured ior. A zero-ior short completion becomes raw status 14 plus
`BD-E-INTERNAL`. Stale read, write, and selected-volume flush results clear
`FS-OK`, whereas `_RAW-DISK-FLUSH?` does not. Both flush paths update status and
ior but deliberately leave the last transfer's completed count. The state is
runtime-global and unlocked, so concurrent calls need not expose a coherent
three-cell snapshot. An abort preserves it without rolling back partial DMA or
media effects.

`B.SAVE` and `B.LOAD` are qualified only when the Buffer has a complete
sector-rounded payload. Unchanged source submits `ceil(B.BYTES/512)*512` bytes
from `B.DATA`, while ordinary constructors reserve only the logical payload;
otherwise the operation exposes or overwrites up to 511 adjacent bytes. A
zero-byte Buffer submits an invalid zero-sector request and aborts through the
checked wrapper. `B.SAVE` performs no flush and proves no durability. Hosted
acceptance uses an exact-sector Buffer and does not add hidden padding.
`DISK-INFO` samples only ambient attachment presence; it neither opens nor
validates the selected binding and does not report capabilities, staleness,
`FS-OK`, or durability.

The native hosted `MP64FS-VALID?` preserves the literal `1`/`0` result, fixed
core-0 scratch layout, dynamic geometry, up to three checked reads (all three
on the successful path), narrow occupied-entry predicate, and final
attachment-generation check. The admitted `FS-LOAD` path now consumes that
ordinary pseudo-BIOS word rather than a host filesystem shortcut.

The contiguous source frontier now reaches EOF at line 9894. Exact current
lines 4804 through 5003 contain 200 lines and 6,799 bytes (SHA-256
`d76d714ed903db5bcd5a6ba5271288ea31c08e2f5fdec2eabd86dbb0bd0cbc32`).
They publish all 38 legacy file definitions through `FILES`: the eight-pointer
display registry, one-sector scratch window, permanent four-cell dictionary
descriptors, metadata words, head/full/tail file I/O, and UART publishers.
Load initializes the count and variables and allocates the table and scratch;
it creates no `FILE`, performs no storage operation, and prints nothing.

`FILE` allocates or reserves no media and captures no volume identity. Its
start LBA is relative to the volume selected at each operation, so rebinding
redirects an existing descriptor. Only the first eight descriptor pointers
enter `FILE-TABLE`; later constants remain usable but are omitted from
`FILES`. There is no open/close lifecycle, and these 32-byte descriptors are
not compatible with the later MP64FS descriptor pool or `FCLOSE`.

Qualification covers ordinary nonnegative, nonwrapping geometry whose declared
extent fits the selected volume and whose complete caller spans are mapped.
Within that domain, `FWRITE` preserves partial-sector surroundings, DMAs whole
middle sectors, and publishes cursor/used only after every stage succeeds;
`FREAD` clamps to logical availability and returns zero at EOF. Acceptance
executes a real head/full/tail round trip through `_DISK-*`, exact capacity and
zero-length paths, metadata growth/clamping, registry publication, UART
listing, and a late range abort that retains earlier sector writes without
committing descriptor metadata. No file operation flushes.

The unchanged source does not enforce that domain. `FSEEK` is unchecked;
`FTRUNCATE` can grow and expose old bytes; file extents can overlap or escape
the volume; and arithmetic can wrap. `FWRITE` mixes a wrapping end calculation
with signed `>`, while `FREAD` uses signed `<`; both use signed
two's-complement `MIN`/`MAX`. High-bit values outside valid nonnegative file
geometry remain unqualified.
Shared `FDESC`, `FT-N`, `FW-*`, `FR-*`, and `FSCRATCH` make construction and
I/O non-reentrant, and per-sector locking does not make read-modify-write or
multi-sector work atomic. Failures can leave earlier destination or media
effects, no logical hole is automatically zero-filled, all raw field access
trusts its pointers, and descriptor metadata is never persisted. After blank
line 5004, exact unchanged lines 5005 through 5134 admit the initial MP64FS
foundation. Including that leading seam, its fixture contains 131 lines and
4,579 bytes (SHA-256
`caf26787745bdf711a89130db7f8b30d45b0f9a63534b4ccb58a601bb2cea062`)
and publishes 32 definitions through `FIND-FREE-SLOT`.

Load allocates three runtime-global cache windows, initializes provisional
geometry (`FS-TOTAL = 2048`, `FS-BMAP-N = 1`) and root `CWD = 255`, and leaves
`FS-OK = 0`. It performs no validation, binding, I/O, flush, locking, or UART
publication. Virgin hosted memory supplies the observed cold zeros; the
unchanged source does not clear the `ALLOT` tails. Because each declaration
starts with an eight-byte `VARIABLE` body and then uses `size 1- ALLOT`, the
raw reservations are 519, 8,199, and 6,151 bytes for operational windows of
512, 8,192, and 6,144 bytes.

The cache and bitmap helpers can represent all 65,536 sector bits;
`FIND-FREE` reports the first complete free run without allocating it; and the
directory readers decode every packed little-endian field. `FIND-FREE-SLOT`
checks only entry byte zero. The admitted contract therefore assumes
`1 <= FS-BMAP-N <= 16`, `13 + FS-BMAP-N < FS-TOTAL <= 65536`, in-range
sectors and slots, a positive free-run count, complete cache spans, and a
validator-conforming directory. Canonical producers zero all 48 bytes of a
free slot, but executable BIOS validation and `FIND-FREE-SLOT` use only
`name[0]`; a zero first byte makes the other 47 bytes irrelevant.

These helpers are deliberately not hardened by simulator policy. They do not
gate on `FS-OK` or validate pointers, indices, geometry, or counts.
`BIT-MASK` is a scalar cell shift only for `0..63`, with bitmap callers using
`0..7`; invalid ordinary-`DO` bounds can traverse the modulo-64-bit range.
`FIND-FREE` uses shared `FF-*` scratch and is non-reentrant.

Exact unchanged lines 5135 through 5217 add four definitions through `FORMAT`
in 83 lines and 2,999 bytes (SHA-256
`829268e2d06f11c19bda4a5fa0606e883fdf3ab4a3690a741f0cd2616ada4137`).
Loading the slice only installs definitions. Focused pathless in-memory
execution qualifies raw-binding load, ordered cache synchronization,
conditional autoload, and metadata-only marker-1 formatting.

`FS-LOAD` clears `FS-OK` before presence and destructive raw rebinding. After
BIOS validation it reads and publishes the superblock and geometry, then the
bitmap, then the directory; only the last success sets `FS-OK`, and `CWD` is
retained. A later abort can therefore leave the new binding, earlier caches,
and geometry live while the filesystem remains unmounted. Validation and
cache reads are separately locked operations, and the reread superblock is not
revalidated, so this is not a coherent same-medium content snapshot.

`FS-SYNC` writes bitmap, then directory, then flushes, never the superblock.
Later failure does not undo earlier writes; non-stale failure can leave
`FS-OK` true, while stale compatibility results clear it. `FS-ENSURE` is silent
for false-plus-absent, invokes `FS-LOAD` only for false-plus-present, and never
revalidates a true marker.

`FORMAT` clears `FS-OK`, destructively binds raw storage, accepts capacities
from 15 through 65,536 sectors, publishes geometry, then writes superblock,
active bitmap, and directory before flushing. Only flush success sets
`FS-OK = -1` and `CWD = 255`. Failure retains constructed caches, geometry,
binding, and earlier media writes; data sectors and the inactive bitmap-cache
tail are not erased.

Exact unchanged lines 5218 through 5285 add `.FTYPE`, `DIR`, and `CATALOG` in
68 lines and 2,167 bytes (SHA-256
`c3c831bc183ee999c8b5a0d1fb4edd169890be1e5fa44ad726d3025923fdb3b7`).
Loading them installs only three definitions and inline strings; it performs
no binding, storage operation, cache mutation, or UART publication. The
qualified execution path is pathless hosted listing from an admitted cached
filesystem, not file-backed persistence evidence.

The hosted BIOS `.ZSTR` consumes its address before its first read, reads and
publishes each nonzero byte in order, stops without publishing the first NUL,
and has no hidden length limit. It performs no decoding, character-policy
check, or escaping, so nonzero control bytes reach UART unchanged. A later
memory fault retains the already published prefix. `MP64FS-VALID?` does not
require a NUL in an occupied entry's 24-byte name, so such a
validator-accepted entry can make unchanged `DIR` or `CATALOG` publish adjacent
metadata and entries until a later zero or fault. Listing admission therefore
requires the canonical producer invariant of a terminator within the name
field rather than strengthening either word. The focused finite-spill oracle
preserves native discrepancy parity; it does not widen that admitted domain.

Both listings select occupied direct children of `CWD` from the global cache
and count bitmap free bits over `[FS-DSTART, FS-TOTAL)`, rather than deriving
free space from extents. `DIR` prints `DE.USED`, compact `.FTYPE` labels, and a
slash for type 8. `CATALOG` prints `DE.USED`, the primary `DE.COUNT` only,
numeric type, and flags. Their numeric fields use signed `.` in the current
`BASE`. The cache and output are not a coherent concurrent snapshot, and
`FS-ENSURE` does not revalidate an already-true `FS-OK`; detached or replaced
media can therefore leave a stale listing eligible. Blank line 5286 is the
leading seam of the adjacent lookup/mutation fixture.

Exact unchanged lines 5286 through 5408 add `FIND-BY-NAME`, `TICKS@`,
`MKFILE`, `RMFILE`, and `RENAME`, plus six scratch variables, in 123 lines and
4,020 bytes (SHA-256
`a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028`).
Load zero-initializes `MK-NSEC`, `MK-TYPE`, `MK-SLOT`, `MK-START`, `RM-SLOT`,
and `RN-SLOT` and installs the five colon definitions and their inline strings.
It performs no epoch read, name parse, cache or media mutation, sync, or UART
publication. Qualified execution uses pathless in-memory media and explicitly
controlled deterministic epoch state.

`TICKS@` applies the source's signed `/` to `EPOCH@ 1000`; admitted positive
values therefore truncate milliseconds toward zero. It returns a complete
cell despite its `u32` source comment, while `MKFILE`'s `L!` stores only the low
32 bits as `mtime`. This qualifies neither high-bit signed epoch inputs nor an
automatic clock.

`FIND-BY-NAME` has no `FS-OK` gate. It scans occupied entries in ascending slot
order, filters by `CWD`, and compares all 24 bytes against zero-padded
`NAMEBUF`. The validator does not reject duplicate names or nonzero bytes after
a NUL, so a visible match with a stale tail can fail and the first exact
duplicate shadows later slots. The lookup and mutations share global
`NAMEBUF`, `CWD`, caches, and scratch and are not reentrant or a coherent
concurrent transaction.

The admitted mutation domain requires a nonempty canonical single-component
name, positive in-range primary allocation, a non-directory validator-approved
type, valid current `CWD`, validator-approved geometry, and exclusive disjoint
non-directory extents. `FS-LOAD` deliberately retains `CWD`; after rebinding,
a stale parent can make `MKFILE` publish an entry rejected by the next mount.
`MKFILE` accepts arbitrary type/count/name cells in source, and an empty name
marks sectors before constructing an entry whose zero first byte remains free.
Type 8 with a positive run is likewise invalid as a directory. The safe domain
does not include these cases.

`MKFILE` selects a slot and one complete run, mutates cached bitmap and entry,
sets `used_bytes` and the secondary extent to zero, timestamps through
`TICKS@`, and only then calls `FS-SYNC`. It neither initializes nor erases the
claimed data sectors. `RMFILE` clears cached bits for both extents and zeros
the entry before syncing, without wiping payload. Its ordinary primary-count
`DO` is unsafe for a directory's zero extent. Because validation does not
prove extent disjointness or ownership, deleting an overlapping accepted file
can clear bits still referenced elsewhere.

`RENAME` zeroes and replaces only the 24-byte name before syncing; it retains
`mtime` and every other field. A same-name request is reported as taken, while
an empty replacement makes the slot invisible without releasing extents.
All three commands inherit `FS-SYNC`'s bitmap, directory, flush order. A later
failure retains cache mutation and may retain earlier media writes; a
non-stale failure can leave `FS-OK` true, and repeating the command can
short-circuit against the changed cache rather than repair media.

Parser consumption is also nontransactional. When the filesystem is
unavailable, `MKFILE`, `RMFILE`, and `RENAME` return before `PARSE-NAME`, so
their filename tokens remain for the outer evaluator. When `RENAME` cannot
find its old name, it returns before parsing the proposed new name. These are
preserved source defects, not admitted safe command forms. Blank line 5409 is
the leading seam of the adjacent `CAT` fixture.

Exact unchanged lines 5409 through 5436 contain 28 LF lines and 838 bytes,
with SHA-256
`e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23`
and Git blob `2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5`. They define zero-initialized
`CAT-SLOT` and the `CAT` colon word. Loading only mutates the dictionary and
installs inline strings: it performs no parse, filesystem ensure, cache or
media access, storage-diagnostic update, or UART publication.

`CAT` orders its exits as filesystem availability, parsed exact-name lookup,
then `DE.USED = 0`. The unavailable path returns before consuming the filename
token and prints ` No filesystem` plus CRLF. A miss prints ` Not found: `, the
canonical parsed name, and CRLF. A zero-used match prints ` (empty file)` plus
CRLF. Miss and empty exits issue no file-data read.

For a nonempty match, `CAT` makes one generation-bound `_DISK-READ` of the
complete primary `DE.COUNT` sectors at `DE.SEC` into the current unreserved
`HERE`; it does not reserve the span or advance `HERE`, and it reads allocation
tail bytes beyond `DE.USED`. Only after the read succeeds does it publish
exactly `DE.USED` bytes. Each LF byte becomes UART CRLF; CR, NUL, ESC, and every
other byte are emitted unchanged, and the word adds no implicit final newline.
A failed read aborts before content UART publication, but a lower-level partial
transfer can retain its already-written prefix at `HERE` and in diagnostics.

The admitted safe domain is a stable mounted generation, a canonical matched
non-directory file, one small positive primary extent, no secondary extent,
`DE.USED <= DE.COUNT * 512`, and a complete unused mapped DMA span from `HERE`.
`CAT` does not enforce that span, capacity relation, or type. It ignores
`DE.EXT1-SEC` and `DE.EXT1-CNT`; consequently a BIOS-validator-approved
two-extent file whose used content crosses the primary boundary causes `CAT`
to emit stale unread bytes already following the primary DMA span. `CAT-SLOT`,
`NAMEBUF`, `PATHBUF`, `PN-LEN`, storage diagnostics, and the unreserved `HERE`
scratch are global and unlocked. Blank line 5437 leads into the adjacent
free-space reporting fixture.

Exact unchanged lines 5437 through 5471 contain 35 LF lines and 984 bytes,
with SHA-256
`6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c`
and Git blob `1884c81ba2b8aa48082d472250f13a2265fd1def`. They define zero-initialized
`LF-BEST` and `LF-RUN`, then `FS-LARGEST-FREE` and `FS-FREE`. Loading only
mutates the dictionary and installs inline strings: it performs no filesystem
ensure, bitmap or directory scan, cache or media access, diagnostic update, or
UART publication.

`FS-LARGEST-FREE` is an unguarded cached-bitmap helper. It resets both global
scratch cells, scans `[FS-DSTART, FS-TOTAL)` in ascending order, resets the
current run on allocated bits, and updates the best length on every free bit,
including a trailing run. `FS-FREE` first calls `FS-ENSURE`; if `FS-OK` remains
false, it prints ` No filesystem` plus CRLF and returns without scanning or
changing `LF-BEST`/`LF-RUN`.

On availability, `FS-FREE` makes three separate cache observations: total
clear bitmap bits, the largest clear run, and every directory entry with
nonzero `name[0]`. The occupied count is global across all parents, includes
directories, and does not reconstruct ownership. It publishes total sectors,
their product with 512 bytes, largest contiguous sectors, occupied entries,
and the literal 128-entry maximum. Every number uses signed `.` in the current
`BASE`.

The admitted domain requires validator-approved positive geometry and complete
cache spans. Direct `FS-LARGEST-FREE` does not establish that precondition, and
invalid ordinary-`DO` bounds are excluded. `FS-ENSURE` does not revalidate an
already-true `FS-OK`, so detached or replaced media can leave stale reporting
eligible without storage I/O. The bitmap scans, directory scan, and `LF-*`
scratch are global and unlocked, not one coherent allocation snapshot. This
qualification admits reporting only, not allocation improvement, extent
ownership validation, repair, compaction, or persistence.

Exact unchanged lines 5472 through 5514 contain 43 LF lines and 1,317 bytes,
with SHA-256
`7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104`
and Git blob `8b4645f16c7ac2f21036282a896b7ede6bad16b0`. The exact source-order ledger is
variable `SB-SLOT`, variable `SB-DESC`, colon `SAVE-BUFFER`, variable
`LB-SLOT`, variable `LB-DESC`, and colon `LOAD-BUFFER`: four variables and two
colon bodies, six definitions total. The variables initialize to zero. Loading
only changes dictionary/allocation state and installs inline strings.
It does not call `FS-ENSURE`, parse a name, dereference the supplied descriptor,
touch filesystem cache or media, update diagnostics, flush, or emit UART data.

Each word calls `FS-ENSURE` and tests `FS-OK` before saving its descriptor or
parsing. If unavailable, it drops the descriptor, leaves the following name
token unconsumed, prints ` No filesystem` and CRLF, and leaves all four scratch
variables unchanged. A miss happens only after the appropriate descriptor and
parsed-name result have been stored: the slot becomes `-1`, no Buffer field is
read, no I/O occurs, and `SAVE-BUFFER` alone prints the `create with MKFILE
first` hint.

A match always transfers `DE.COUNT * 512` bytes at `DE.SEC`, regardless of
`DE.USED`, and neither word inspects or follows `DE.EXT1-SEC`/`DE.EXT1-CNT`.
`SAVE-BUFFER` first performs the generation-bound payload write from `B.DATA`,
then writes the low 32 bits of the cell-sized `B.LEN` to cached `used_bytes`,
then calls `FS-SYNC`, whose bitmap-write, directory-write, and flush order
remains nontransactional. No other entry field changes: name, primary extent,
type, flags, parent, `mtime`, CRC, and secondary extent are retained. Thus the
word neither timestamps nor recomputes integrity metadata. A payload failure
precedes the cache update and sync, but may retain a partial media prefix. A
later sync/flush failure can leave the payload and some metadata on media and
the new cached `used_bytes` visible without the success line.

`LOAD-BUFFER` performs one generation-bound read of the complete primary
allocation into `B.DATA`, including all allocation padding after `DE.USED`.
It does not change `B.LEN`, any other Buffer field, or filesystem metadata.
A failed read emits no success line but may leave the completed prefix in the
Buffer. On success the unchanged strings report the saved `B.LEN` or cached
`DE.USED` with signed `.` in the caller's ambient `BASE`; neither word changes
`BASE`.

The source's save metadata and message use `B.LEN`, while transfer capacity for
a Buffer is `B.BYTES = B.WIDTH * B.LEN`. A multi-byte element count is therefore
mislabeled and stored as bytes. The admitted ordinary-constructor domain uses
a valid byte-width descriptor with
`B.LEN = B.BYTES = DE.COUNT * 512`, a `B.DATA` span mapped and readable for
save or mapped and writable for load, and a `B.LEN` representable as the
intended unsigned 32-bit field. Save also requires a writable selected volume.
Both words require a stable mounted generation,
a canonical matched non-directory entry, one positive in-range primary extent,
and no secondary extent. The source enforces none of those descriptor, type,
capacity, length, or secondary-extent constraints and does not enforce
per-entry read-only or system flags. `SB-*`, `LB-*`, name/parser
state, cache, and storage diagnostics are global and unlocked; this slice adds
no file lock or transactional recovery.

Exact unchanged lines 5515 through 5610 contain 96 LF lines and 3,397 bytes,
with SHA-256
`16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78`
and Git blob `e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9`. Their exact 14-definition
ledger is, in source order, constants `FD-MAX` and `FD-SLOT-SZ`, created word
`FD-POOL`, colons `FD-SLOT`, `FD-ALLOC`, and `(FCLOSE-NOFS)`, deferred word
`FCLOSE`, colon `FD-FILL`, variable `OP-SLOT`, colon `(OPEN)`, deferred word
`OPEN`, then colons `F.SLOT`, `FFLUSH`, and `(FCLOSE)`.

Load allocates `16 * 72 = 1,152` bytes after `FD-POOL`, explicitly zero-fills
that entire span, and initializes `OP-SLOT` to zero. It creates `FCLOSE`, binds
it to `(FCLOSE-NOFS)`, later creates `OPEN` and binds it to `(OPEN)`, then after
compiling `FFLUSH` and `(FCLOSE)` rebinds the existing `FCLOSE` to `(FCLOSE)`.
The final deferred targets are exactly `(OPEN)` and `(FCLOSE)`; there is one
dictionary word for each public defer. These dictionary, allocation, fill, and
deferred-vector mutations are the only load-time effects: loading performs no
filesystem ensure or parse, cache/media I/O, sync or flush, diagnostic update,
clock access, or UART output.

Each 72-byte slot has an in-use cell at slot `+0`; its returned fdesc points to
slot `+8`. Relative to fdesc, the cells are primary start `+0`, primary maximum
sector count `+8`, used bytes `+16`, cursor `+24`, directory slot `+32`,
secondary start `+40`, secondary count `+48`, and reserved `+56`. `FD-SLOT`
is unchecked address arithmetic. `FD-ALLOC` scans indices 0 through 15,
selects the first zero in-use cell, stores `-1`, and returns slot `+8`; it
returns zero when all headers are nonzero. It never clears payload cells.
`FD-FILL` snapshots the selected cached directory entry into offsets `+0` through
`+48` and resets the cursor to zero, but does not overwrite reserved `+56`.
The reserved cell is zero after cold load and retained across fill, close, and
reuse. `(FCLOSE-NOFS)` remains a directly callable word after the deferred
rebind: zero is a no-op, while nonzero unconditionally clears the cell at
`fdesc - 8` without flushing or clearing payload.

Final `OPEN` calls `FS-ENSURE` and rejects false `FS-OK` before parsing. That
path returns zero, leaves the filename token and `OP-SLOT` unchanged, and
prints ` No filesystem` plus CRLF. After parsing, a lookup miss stores `-1` in
`OP-SLOT`, prints ` Not found: ` plus the parsed name and CRLF, and returns zero
without allocation. Pool exhaustion occurs only after a successful match;
`OP-SLOT` retains the match, `FD-ALLOC` returns zero, `OPEN` prints ` No free FD
slots` plus CRLF, and no slot changes. Success returns the lowest available
fdesc, snapshots the cache with cursor zero, and emits nothing. When `FS-OK`
was already true, every miss, exhaust, and success path is storage-I/O-free and
success reads no file payload; an initial false marker may allow `FS-ENSURE` to
invoke the already-qualified `FS-LOAD` before the gate.

`OPEN` does not reject a directory or inspect type/flags, capture a storage
binding or generation, revalidate a true `FS-OK`, prevent duplicate opens, or
link independent cursor/used snapshots. Directory mutation, cache reload, or
media rebinding can stale a live descriptor. Multiple descriptors for one
entry are allowed, and later flush order alone decides the cached/on-media used
count. `FD-FILL` copies secondary coordinates, but neither this fixture nor
that structural snapshot qualifies multi-extent `FREAD`, `FWRITE`, or any
other data I/O.

`FFLUSH` gates directly on `FS-OK` before accessing the supplied address. A
false marker drops it, prints ` FS not loaded` plus CRLF, and performs no cache
or media mutation. With a true marker, it reads `F.USED` and `F.SLOT`, stores
only the low 32 bits of used into cached directory offset 28, then calls
`FS-SYNC`. It does not write payload or change name, extents, type, flags,
parent, `mtime`, or CRC. It validates neither fdesc membership/directory-slot
range nor used against the allocated extent capacity; `L!` truncates any cell
to low u32. The cache mutation precedes bitmap write, directory write, and
flush, so a sync abort retains the changed cache and any completed media
prefix. Direct `FFLUSH` does not release the descriptor.

Final `FCLOSE` first treats zero as a no-op. For nonzero input it samples
`FS-OK`: true calls `FFLUSH`, and only a normal return clears the in-use header;
an abort keeps the descriptor allocated even though cache or media may already
have changed. False skips persistence silently and clears the header, thereby
discarding any dirty used count. Every release clears only the header: all
fdesc cells, the reserved cell, and the file payload remain intact. Neither
close path validates membership, alignment, allocation, or directory identity.
A stale pointer to a lowest-first-reused address can therefore flush or release
the new occupant (an ABA hazard), while a double close can race logical reuse.
Pool headers, fdesc payloads, `OP-SLOT`, parser state, directory cache, and the
deferred vectors are global and unlocked.

Exact current lines 5611 through 5944 contain 334 LF records and 11,980 bytes,
with SHA-256
`6a30453c933ac8666c1b798a98a4fb3e6a331afeb4c2d3048299a83a0ea79a7c`
and Git blob `f2bea50138ca04e235358debd734a4fc234e002a`. Their exact 55-definition
ledger installs five loader globals, constants for a 16 × 88-byte nesting stack,
frame/evaluator/transaction accessors, three deferred transaction actions,
two-extent read helpers, path scratch and traversal, six evaluator-status
constants, the KDOS checked-evaluator shadow, five whole-source walker cells,
`SOURCE-EVALUATE-CHECKED`, `_LD-STATUS-THROW`, the checked loader walker,
dictionary/error guards, and final raw filesystem `LOAD`. Its optional module
transaction
actions are all bound to `_LD-TXN-NOOP`; the later module registry owns their
real commit/rollback meanings. Fixture evaluation allocates dictionary storage
and explicitly clears `_LD-SP`, but performs no filesystem, storage, clock,
lock, or UART operation.

`LOAD` is a parsing word and is admitted only through an active ordinary source
cursor. It calls `FS-ENSURE` before `PARSE-NAME`: an unavailable filesystem
prints ` No filesystem` plus CRLF, returns before consuming the filename, and
lets the enclosing interpreter process that token. After parsing, a lookup
miss, zero `DE.USED`, or allocation failure emits its literal diagnostic and
restores the saved loader frame without payload I/O or transaction actions.
The current eleven-cell nesting frame saves `LD-BUF`, `LD-SZ`, `LD-CUR`,
`LD-LEN`, `LD-LINE`, `EVAL-LINE`, CWD, the evaluator-depth checkpoint, a
transaction head, and the frame's `HERE`/`LATEST` dictionary checkpoint; a seventeenth frame
aborts with the source's `REQUIRE nested too deep` message.

The admitted successful domain has valid stable mounted metadata; total path
storage at most 127 bytes; each intermediate and final component at most 23
bytes; and an in-range positive combined extent allocation. Source is
LF-delimited, contains no retained CR, has physical lines at most 255 bytes,
and ends with complete compiler/control state. A final line need not carry LF.
The loader allocates the combined primary/secondary sector span, reads each
complete run contiguously, and evaluates only `DE.USED`, so transferred
allocation padding is not source. Relative nested loads inherit the containing
directory during execution and restore each caller's CWD and walker globals.
Qualification crosses the 512-byte extent seam inside a definition, then
loads a sibling module normally.

Normal completion calls the optional transaction commit hook, frees the
allocation, restores the frame, and then calls after-release. Nested action
order is inner commit, inner after-release, outer commit, outer after-release.
On any admitted guarded failure delivered as guest `THROW`, KDOS unwinds
evaluator depth, calls the optional rollback hook, rolls the dictionary back to
the frame's saved `HERE`/`LATEST`, resets evaluator state, releases/restores
the allocation and frame, calls after-release, and rethrows the exact
exception. Dictionary
rollback is intrinsic to every loader frame even while the three module hooks
remain no-ops, so a definition completed earlier in the failing load does not
remain published.

Path resolution has two admitted source defects. Intermediate and final copies
have no component-length bound; oversized paths are excluded, and hosted
semantic dictionary metadata cannot reproduce native linked-header corruption
from such an overwrite. More visibly, `_RESOLVE-PATH` prints failure for a
missing or nondirectory intermediate but returns no status. `LOAD` continues
with that rejected component in `NAMEBUF`; it may print a second miss or load
the component itself instead of the requested final file. Focused acceptance
pins the latter result rather than adding simulator-only control flow.

At the pinned pre-decision revision the data read occurred before
`_LD-WALK-GUARDED`, so a read `ABORT"` could strand the transfer allocation,
loader frame, and resolved CWD. Current `LOAD` places the complete extent read
and checked walk under the same guard and uses `_DISK-READ?` so an admitted
media error becomes a catchable `DISK-IO-IOR`. That checked error, translated
evaluator statuses, and an ordinary source `THROW` after allocation take the
full dictionary rollback and cleanup lifecycle above; the storage service's
own already-completed media or diagnostic effects remain governed by its
separate contract. A task-resetting `ABORT`/`ABORT"`, arbitrary host exception,
or memory fault that never becomes guest `THROW` bypasses `CATCH`; this repair
does not claim cleanup or transactionality for those exits.

At the pinned pre-decision revision, `_LD-WALK` called raw `EVALUATE` without
checking `EVAL-STATUS` or calling `EVALUATE-FINISH`; it also preserved CR
instead of applying the checked walker's CRLF rule. The historical acceptance
fixture demonstrates that an undefined middle line could be skipped. That
nominal-success behavior is no longer conforming: ordinary loader source must
check each evaluator result, finish the complete source, commit only complete
input, and take its existing unwind/rollback/release/restore path on each
admitted checked failure.
The loader translates checked statuses 1 through 4 into the same positive
`THROW` values and translates status 5 back to the exact code retained in
`EVAL-THROW`. An extent read uses the checked `_DISK-READ?` path; failure
rethrows the exact nonzero code retained in `DISK-IO-IOR`. In every case the
common dictionary rollback and cleanup complete before the outward throw.
This is a KDOS lifecycle repair shared by emulator and simulator, not a direct
host-filesystem substitute. File-type/flag policy and the runtime-global,
unlocked loader scratch remain separate open matters.

Exact current lines 5945 through 6059 contain 115 LF records and 4,231 bytes,
with SHA-256
`b42f5c10635f43ff41e4dd719987f21ab5bcbb229d3985ad0cc854d2bba7ffc1`
and Git blob `bf344d51bdea5287d4af87c920d563a33adc1a85`. The 13-definition ledger
installs `_APP-MPU-ON`, `_APP-MPU-OFF`, `APP-EVAL`, `_APP-LOAD-WALK`,
`_APP-LOAD-USER`, `_APP-LOAD-RUN`, and `APP-LOAD`, followed by the six
canonical ANSI byte helpers. Loading the fixture only compiles dictionary
entries; it does not change MPU state or touch the filesystem, storage service,
locks, or UART.

`_APP-MPU-ON` overwrites the inert base with zero and selects either the
exclusive external-memory end or `MEM-SIZE` as its limit. `_APP-MPU-OFF`
zeros both registers rather than restoring their prior values. On ordinary
return, `APP-EVAL` exposes those active values and permanent supervisor
privilege to the supplied guest bytes, preserves their data-stack and
dictionary effects, then disables the MPU state. It calls raw `EVALUATE`, so
undefined and unfinished input retain that evaluator's literal status behavior.
A guest `THROW` caught outside `APP-EVAL` bypasses `SYS-EXIT`, MPU teardown,
and evaluator unwind: the configured limit and one abandoned evaluator frame
remain visible. Focused acceptance pins that defect in a disposable runtime;
the simulator does not insert an implicit guard.

`APP-LOAD` performs a direct current-directory `FIND-BY-NAME`; unlike `LOAD`,
it does not call `_RESOLVE-PATH`, so slash syntax has no navigation meaning.
It does not validate file type or flags. Once a nonempty entry is found it
uses the existing nested loader frame, sector-rounded allocation, complete
primary/secondary-extent reads, and shared checked physical-line walker. It
enables the MPU compatibility state only around evaluation. Success disables
the MPU, commits optional transaction state, releases the allocation/frame,
and calls after-release. A guest `THROW` first tears down the application MPU
window and then takes the common evaluator unwind, optional registry rollback,
dictionary rollback, release, after-release, and exact-rethrow path.

The pinned pre-decision application walker required a terminal LF and zero net
data-stack effect per line, could scan into sector padding, ignored evaluator
status, and never called `EVALUATE-FINISH`. Those are historical observations,
not current requirements. `APP-LOAD` now inherits the common bounded walker:
it accepts a final line without LF, trims a trailing CR, checks every physical
line of at most 255 bytes, preserves ordinary source data-stack effects, and
requires complete final compiler/control state before commit.

The clean no-filesystem, miss, empty-file, slash-literal miss, and
allocation-fail paths do not alter preexisting MPU state or invoke transaction
actions. After allocation, checked extent-read errors and checked-walker guest
`THROW` exits pass through the common loader guard, release the transfer,
restore the loader frame, and roll back dictionary state. Task-resetting aborts
and non-guest backend faults retain the same outside-guarantee boundary as
ordinary `LOAD`. Parser,
loader, evaluator, MPU, filesystem-cache, diagnostics, and transaction scratch
remain runtime-global and unlocked.

`ESC`, `CSI`, `SGR`, `RESET-COLOR`, and `DIM` publish literal UART byte
sequences through ordinary `EMIT`. `.N` emits a leading minus and explicit
decimal digits for ordinary magnitudes below 1,000; magnitudes at least 1,000
delegate to the existing base-sensitive `.` and inherit its trailing space.
No separate terminal renderer or rich-terminal path is involved.

Exact unchanged lines 6060 through 6200 contain 141 LF records and 5,298
bytes, with SHA-256
`35a8f33b51da4e3a319f193e0c709a876207f940923637d0f56b0f8160c7f574`
and Git blob `ed442875e780976b10553721137e515e3742ddcb`. The exact ledger is two
CREATE bodies (`FS-KEY`, 32 bytes, and `FS-IV`, 12 bytes), flag constant 4,
six zero-initialized VARIABLE cells, and seven colon definitions through
`FDECRYPT`. Source loading reserves 92 explicit mutable bytes and publishes
definitions only; it performs no heap allocation, AES operation, filesystem
access, storage command, lock acquisition, or UART output. The source does not
itself initialize the two raw CREATE/ALLOT bodies.

The ordinary qualified domain is a live matching OPEN descriptor on stable
mounted media with `FS-OK` true; one positive primary contiguous extent; a
nonwrapping logical size whose 16-byte-rounded span plus tag fits that primary
allocation; an explicitly installed 32-byte key; an idle shared AES engine in
AES-256 mode; and synchronous non-reentrant execution. `FS-KEY!` copies exactly
32 bytes. `_FE-MKIV` clears all twelve IV bytes and then writes the descriptor's
directory slot as one little-endian cell, leaving four zero bytes. Flag helpers
read and update the live directory cache through `F.SLOT`, preserve every bit
other than bit 2, and do not snapshot flags in the descriptor.

For nonempty unflagged input, `FENCRYPT` allocates and zeroes two complete
sector-rounded Bank-0 buffers, reads `ceil(used/512)` primary sectors, runs one
GCM transaction over `ceil16(used)` bytes, copies one tag immediately after the
ciphertext, and writes `ceil((ceil16(used)+16)/512)` sectors. It then sets cache
flag bit 2, rewrites the cached low-u32 used count, invokes the already-admitted
bitmap/directory/flush `FS-SYNC`, frees both buffers, and returns zero. A second
call while flagged returns zero before changing scratch or issuing I/O.

`FDECRYPT` reads that ciphertext/tag span and stages output. Authentication
failure returns true/-1 after freeing both buffers and performs no payload,
cache, sync, or flush write. Authentication success writes only
`ceil(used/512)` plaintext sectors, clears cache flag bit 2, rewrites used, syncs,
frees, and returns zero. Neither successful direction changes the descriptor's
cursor or used snapshot, name, type, parent, extents, mtime, or CRC. Focused
acceptance compares ciphertext/tag bytes with an external AES-GCM oracle,
checks exact storage-command counts and cache/media flags, proves heap recovery
on normal and auth-failure paths, and proves logical plaintext roundtrip.

Several discrepancies materially limit this interface. Zeroing BUF1 before a
whole-sector read does not create zero padding: physical bytes between
`used_bytes` and the 16-byte boundary overwrite those zeros and are encrypted
and authenticated. The IV contains only the stable slot, so decrypt/change/
re-encrypt or slot reuse repeats a nonce under one key; metadata and exact
logical length are not AAD. No key-set marker exists, and the wrappers do not
force AES-256, so an ambient AES-128 mode changes the algorithm despite the
section title. The operations ignore secondary coordinates. Encrypt rejects
insufficient primary tag capacity, while decrypt has no equivalent check and
can read into a neighboring allocation if handed externally flagged metadata.

The not-encrypted `FDECRYPT` branch returns zero even though its detailed source
comment says that case returns -1. Empty encrypt returns zero without setting
the flag; empty flagged decrypt returns zero without clearing it. A failed
second DMA allocation frees the first buffer but leaves the failed zero address
beneath -1, returning two cells. First-allocation failure returns only -1.
FD-pool membership/generation binding, file type, and MP64FS policy flags are
otherwise trusted rather than validated by this wrapper. The already-admitted
lower storage path still enforces current media generation, volume range, and
device write protection on each transfer.

Payload, cache flag, directory writes, and flush are ordered but not one
transaction. Any disk, AES, or sync exception after allocation bypasses both
frees. A data-write failure may leave a ciphertext prefix with a plaintext
flag; a later sync/flush failure may leave complete ciphertext plus changed
cache and partial or complete directory media. Focused late-flush acceptance
uses only the tiny in-memory storage service and pins the already-published
payload/flag plus both live leaked allocations; it is not durability or crash
recovery evidence. Key, IV, AES tag, plaintext/ciphertext buffers, and dangling
buffer addresses are never wiped. `FENCRYPT` does not inspect AES status after
`ENCRYPT`; it trusts the returned output and tag. All encryption, AES, FD,
cache, allocator, diagnostic, and storage state is runtime-global and
unlocked.

Exact unchanged lines 6201 through 6296 contain 96 LF records and 3,082
bytes, with SHA-256
`dc7f065cfac1fc3eb6efd1de7f4b0f472ff40e66fa14666e1087c18047e1d6c8`
and Git blob `b964ca87a1af44e54b22abd25116edd2a7e2a853`. The exact five-word ledger
is the raw 64-byte `_PWD-STK` CREATE/ALLOT body followed by `PWD`, `CD`,
`MKDIR`, and `RMDIR`. Loading reserves that body and publishes the four colon
definitions without initializing the raw body or changing CWD, parser,
filesystem/cache/media, RTC, diagnostics, locks, or UART state.

For non-root `PWD` and the three filesystem-gated words, the qualified domain
is a stable mounted validator-approved cache with
`FS-OK` true, CWD equal to root 255 or a live directory slot, sibling-unique
nonempty 1–23-byte NUL-terminated simple component names excluding reserved
navigation tokens, and an acyclic parent chain that reaches root.
Calls are synchronous and non-reentrant because CWD, the NAMEBUF/PATHBUF/PN-LEN
parser state, `_PWD-STK`, and the filesystem cache are shared and unlocked.
`PWD` itself does not ensure or
check the filesystem. At root it emits ` /` and CRLF without touching cached
entries. Otherwise it follows the complete parent chain, retains at most the
eight slots nearest CWD, then emits those retained names in root-to-leaf order
with one leading slash and a slash after every component.

`CD`, `MKDIR`, and `RMDIR` ensure and check the filesystem before parsing; if
the gate fails, the would-be operand remains for the outer evaluator. Exact
`..` in `CD` moves to the cached parent except at root, exact `/` stores root
255, and any other token is
one direct current-directory component lookup whose entry type must be 8.
Beyond shared parser scratch, its only state change is volatile CWD; it performs
no sync or media command. In particular,
`CD` does not use the separately admitted `_RESOLVE-PATH`: embedded slash
syntax, `.`, and multi-component paths have no navigation meaning here.

`MKDIR` rejects the first exact occupied sibling match, selects the lowest slot
whose `name[0]` is zero, clears all 48 bytes, copies the zero-padded 24-byte
NAMEBUF, writes type 8, the low CWD parent byte, and low-u32 epoch seconds, then
runs the ordinary bitmap/directory/flush `FS-SYNC`. It allocates no sectors and
does not update the parent mtime. `RMDIR` resolves one direct child, requires
type 8, and rejects it if any occupied entry names that slot as parent. Empty
success clears exactly 48 bytes and performs the same sync without freeing
payload sectors or updating a parent.

The source exposes several important edge behaviors. A parent self-loop or
cycle accepted by metadata validation makes `PWD` nonterminating, and a
validator-accepted 24-byte non-NUL name lets `.ZSTR` read beyond its field.
Depths above eight display only the suffix nearest CWD and silently omit the
ancestors farthest from CWD even though the walk continues to root. `MKDIR`
accepts an empty token, producing a
metadata-bearing entry that remains logically free because `name[0]` is zero.
Tokens longer than 23 bytes silently operate on the truncated NAMEBUF prefix.
It also permits the operationally reserved names `..` and `/`, which ordinary
`CD` cannot reach as named children. Validator-accepted duplicate sibling names
are first-slot-wins for lookup and mutation. Both mutation words ignore MP64FS
policy flags, mutate cache before nontransactional sync, and can leave
cache/media prefixes on an exception. `RMDIR` does not account for saved CWD
snapshots in the loader/REQUIRE machinery, and its nonempty rejection drops
only one of two target slot copies, returning with the slot leaked on the data
stack.

Exact unchanged lines 6297 through 6427 contain 131 LF records and 3,945
bytes, with SHA-256
`442e5e39598d71a589bf19d6345c5bb042d678ba9f51607a878ae5030fbdcee6`
and Git blob `242fc879957ba14f3a00b3284e8af921a4fa365c`. Their exact
source-order ledger is `FTYPE-DOC`, `FTYPE-TUT`, `DOC-BUF`, `DOC-LINES`,
`PAGE-LINES`, `.DOC-CHUNK`, `SHOW-FILE`, `TOPICS`, `LESSONS`, `DOC`,
`TUTORIAL`, `OPEN-BY-SLOT`, and `DESCRIBE`. Loading reserves the raw 512-byte
`DOC-BUF`, zero-initializes `DOC-LINES`, and publishes the other definitions.
It performs no parse, filesystem/media operation, FD allocation, input read,
UART write, synchronization, or lock action. The source does not explicitly
initialize the raw CREATE/ALLOT buffer.

The ordinary qualified browser domain is a stable validator-approved mounted
cache and matching media generation; canonical occupied entries with
NUL-terminated names; unencrypted one-primary-extent payloads satisfying
`cursor <= used <= primary capacity`; writable media for the final close;
positive mapped nonwrapping chunk spans; enough explicitly injected input for
each pagination prompt reached through source evaluation; and synchronous,
non-reentrant execution. The descriptor pool, directory/cache state,
`DOC-BUF`, `DOC-LINES`, parser scratch, UART streams, and storage service are
shared and unlocked.

`.DOC-CHUNK` emits bytes in address order. LF is replaced by UART CRLF and
increments the retained global line count; CR, NUL, ESC, and every other byte
are emitted unchanged, so a CRLF input becomes CR-CR-LF. At each twentieth LF
it emits the ordinary DIM/reset prompt, consumes and discards exactly one
`KEY`, emits CRLF, and resets the counter. The pause also occurs when the
twentieth LF is the chunk's final byte. Counts carry across calls. A blocking
prompt can be resumed through the hosted `KEY`/IDL boundary when the browser
word was entered by resumable compiled dispatch; the ordinary evaluator path
must prequeue its input as specified above.

`SHOW-FILE` resets the line count, issues consecutive `FREAD` calls of at most
512 bytes from the descriptor's incoming cursor, displays each positive
result, and stops on zero. It consumes the descriptor argument without closing
it and leaves the descriptor cursor at logical EOF. Consequently its source
comment's “entire file” means only the suffix from the current cursor. The
already-admitted legacy `FREAD` ignores secondary extents and has no primary
capacity clamp, so a valid two-extent logical file can publish neighboring
primary-sector bytes rather than its second extent.

`TOPICS` and `LESSONS` ensure the filesystem and scan all 128 cached slots,
ignoring CWD and parent, listing every occupied type-4 or type-6 name and an
ambient-base count. `DOC` and `TUTORIAL` are behaviorally identical wrappers
around ordinary current-directory `OPEN`: they do not resolve a path or check
type, encryption, CRC, or directory status. `DESCRIBE` does not inspect the
Forth dictionary or documentation content. It parses into the 23-byte name
domain, scans type-4 entries globally and case-sensitively, compares the full
24-byte zero-padded field, and passes the lowest-slot match to
`OPEN-BY-SLOT`. Thus a global listing or DESCRIBE match can name content which
the current-directory DOC/TUTORIAL path cannot open.

`OPEN-BY-SLOT` checks only whether the addressed dirent's first name byte is
nonzero, then allocates the lowest free descriptor and snapshots the supplied
slot. It does not ensure the filesystem or validate slot range, generation,
type, flags, descriptor identity, or parent. Successful DOC, TUTORIAL, and
DESCRIBE display then call ordinary `FCLOSE`; that invokes `FFLUSH` and
`FS-SYNC`, so nominally read-only browsing rewrites the bitmap and complete
directory and flushes the media. A stale descriptor can overwrite a newer
cached used value.

These routines intentionally retain the source's failure behavior. DOC,
TUTORIAL, and DESCRIBE's final open path each execute `DUP 0= IF EXIT THEN` and
therefore return one zero on open failure despite their declared clean stack.
When DOC/TUTORIAL fail their OPEN filesystem gate, parsing never occurred and
the operand also remains for the outer evaluator. Read, input, or close/sync
failure can retain partial output and global counter/cursor/cache effects and
leak the allocated FD. Encrypted input is emitted as ciphertext for logical
`used` bytes without displaying the appended tag; neither encryption nor CRC
is verified. Content and names are trusted terminal data. Zero or malformed
ordinary-`DO` chunk bounds, non-NUL names, invalid slots, concurrency,
durability, and multi-extent browsing remain outside the qualified domain.

Exact unchanged lines 6428 through 6510 contain 83 LF records and 2,682
bytes, with SHA-256
`c1c7be64fd2d1c86465edec8f0fd6922c2742c6b77be9267dc7638f7eeb3ce5a`
and Git blob `8335b7ef5566340e7fa1115de27fec9c75f6ae97`. The exact ledger is
`ENTRY>LINK`, `ENTRY>NAME`, four `IC-*` variables, `ICONTAINS?`, four `WL-*`
variables, `WORDS-LIKE`, `APROPOS`, and `.RECENT`: six colon definitions and
eight zero-initialized cells. Under hosted header geometry they advance HERE
by 398 bytes. Loading executes none of them and performs no UART, filesystem,
storage, MMIO, task, transient `WORD`, or parser action beyond ordinary source
consumption and dictionary publication.

This slice consumes the real pseudo-BIOS `TYPE` and `SPACE` words. `TYPE`
removes length then address before reading, performs no read for zero length,
routes bytes low-to-high with uint64 address wrapping, and retains an emitted
prefix on a later read fault. `SPACE` emits one byte `0x20`. Neither word adds
a retained terminal, renderer, raw UART MMIO, capacity, or timing claim.

`ENTRY>LINK` reads the little-endian link at header offset zero.
`ENTRY>NAME` masks the immediate bit from the flags/length byte at offset eight
and returns the original spelling at offset nine. This agrees with the
executable BIOS's no-padding header layout; hosted semantic code slots can have
different sizes because the slice never derives a code address or next header
from a name. Raw guest header mutation affects these readers and walkers,
while hosted `FIND` retains live semantic name, immediacy, and execution-token
metadata. Corrupted-state agreement between those raw searches and `FIND` is
therefore not claimed.

`ICONTAINS?` stores all four arguments in global scratch, treats an empty
pattern as matching, rejects a pattern longer than its subject, and otherwise
uses correctly nested `I`/`J` loops. Inner mismatch `LEAVE` removes only the
inner loop; success executes `UNLOOP` for the still-live outer frame before
`EXIT`. Case folding applies only to ASCII `a`–`z`; high bytes remain exact.
Its length guard uses signed `<`, so arbitrary full-cell lengths can bypass the
intended bounds and enter wrapping ordinary-`DO` behavior.

`WORDS-LIKE` parses a transient counted pattern at current HERE, records its
address/length and traversal state globally, follows raw links from LATEST to
zero, and prints every matching header newest-first. Shadowed duplicates,
internal words, and the search word itself are not deduplicated or hidden.
`APROPOS` invokes the same path. The counted bytes do not advance HERE and
`WL-PA` remains pointed at that transient region after return. `.RECENT` also
uses raw headers but stops after its signed-positive count or link zero.

The qualified domain requires a stable mapped acyclic dictionary chain,
canonical ASCII headers, ordinary nonnegative lengths and complete
nonwrapping pattern/name spans, enough live dictionary-tail space for the
transient counted word, an active physical input line for the parsing words,
a reasonable finite step budget, and synchronous non-reentrant execution.
The raw readers validate nothing. A link cycle makes WORDS-LIKE unbounded;
malformed name lengths can read or print beyond a header; an invalid link can
fault after partial output; and failure retains global `IC-*`/`WL-*` state,
transient pattern bytes, and any UART prefix. `.RECENT` remains count-bounded
even if a link cycles.

Exact unchanged lines 6511 through 6724 contain 214 LF records and 6,935
bytes, with SHA-256
`cc28cfab7033390f4efc885cc043feafecc136e913aa34cc6338f7ad1b6a1f4c`
and Git blob `ccdee7bbf513495f25eb77ad4c0f13f63b07532c`. Their source-order ledger
contains five constants, nine variables, 24 colon definitions, and one
deferred `CORE-CHECKPOINT`, for 39 publications. Loading zeroes
`TASK-COUNT`, `CURRENT-TASK`, `SCHED-RUNNING`, `PREEMPT-FLAG`, and
`SPAWN-COUNT`, initializes `TIME-SLICE` to 50,000, and installs
`_CORE-CHECKPOINT-BOOT`. It executes no task and touches no timer, UART, IDL,
filesystem, or task stack. The variables reserve 2,175 bytes and the deferred
word reserves one additional cell. `TASK-TABLE` occupies the intended 64
bytes; `VARIABLE TASK-STACKS 2047 ALLOT` occupies 2,055 bytes, despite the
source comment claiming 2,048.

The admitted behavior is a fixed table and synchronous run-to-completion
executor. `TASK` and `SPAWN` append ordinary 48-byte descriptors. DSP is a
computed placeholder that is never installed, RSP is zero with no return-stack
arena, and the stored name address remains zero. A selected XT executes on the
caller's active data stack, return stack, loop frames, task identity, and
exception context. Priority is stored and printed but never consulted;
`FIND-READY` scans from table slot zero on every call. `SCHEDULE` runs that
first READY task to return, then repeats. `SCHED-YIELD`/`YIELD` only mark
`CURRENT-TASK` DONE and return to the next instruction in the same XT. The boot
checkpoint merely clears a manually set global `PREEMPT-FLAG` and calls that
non-suspending `YIELD`. No behavior in this prefix creates a private context,
suspends a task, switches stacks, polls a timer, or preempts execution.

DONE slots are never reclaimed and `TASK-COUNT` only increases. Once eight
slots are registered, another `TASK` still appends an orphan descriptor and
publishes its constant; another `SPAWN` appends an orphan and increments
`SPAWN-COUNT`. Both compute a nominal DSP beyond the declared arena. `TASK`
mutates the registry before parsing and publishing its constant, so a late
source or dictionary fault retains that prefix. A task failure can retain a
RUNNING descriptor, `SCHED-RUNNING = 1`, and stale `CURRENT-TASK`; success also
does not clear `CURRENT-TASK`. Public counts, table cells, and descriptor
addresses are not validated.

Exact unchanged lines 6725 through 6758 contain 34 LF records and 1,143 bytes,
with SHA-256
`e55c6bf6e2df1fd6f543105822ac24217083dbeebe94bae0f631ac34d6dcd653`
and Git blob `a1955ae8ee10c8bee1de5455a55c725d752462ff`. They publish
`PREEMPT-ENABLED`, `PREEMPT-ON`, `PREEMPT-OFF`, and
`_CORE-CHECKPOINT-TIMER`, advancing the hosted dictionary by 134 bytes. Load
zeroes the new variable and rebinds deferred `CORE-CHECKPOINT`; it executes no
Timer word and changes no task, flag, UART, storage, RTC, or IDL state.
Ordinary evaluation steps can nevertheless advance an enabled Timer counter.

`PREEMPT-ON` writes low-32 `TIME-SLICE` to compare, writes control value 5,
and sets the software gate. Value 5 enables the counter and auto-reload but
leaves IRQ generation disabled. `PREEMPT-OFF` writes value 1, leaving the
counter running while clearing only the software gate. These words neither
reset nor acknowledge counter, sticky match, or pending IRQ state. The final
checkpoint never reads that Timer state: only an independently set
`PREEMPT-FLAG` reaches it. With the software gate off the flag is retained;
with the gate on it is cleared before the unchanged non-suspending `YIELD`.
Execution after the checkpoint continues, and no task XT is dispatched.
Consequently this slice qualifies Timer configuration and manual checkpoint
gating, not timer-driven preemption, scheduling fairness, or a time-slice
contract.

Exact unchanged lines 6759 through 6922 contain 164 LF records and 5,713
bytes, with SHA-256
`03dc68d356a186f11b63fedd818863e75da51886d6290b38ba2c769325ffa90f`
and Git blob `c919439c3c81cf5e35a270f47b7b122867df6a89`. Their 15 source-order
publications are eight initial colon definitions through `CORES`, five
zero-initialized `PAR-*` variables, then `P.RUN-PAR` and `P.BENCH-PAR`.
Together the ten colon definitions and five eight-byte variable bodies advance
the hosted dictionary by 415 bytes. Loading invokes no core, lock, UART,
storage, RTC, or IDL operation and leaves the public data and return stacks
empty. The enabled retained Timer counter may advance only through ordinary
semantic evaluation metering.

The topology contract remains one full core. `COREID` and both core counts are
one-core values. `CORE-STATUS` accepts only ID 0 and returns zero for its idle
worker slot; that result does not say that the executing primary is idle.
`WAKE-CORE` always fails without consuming the XT and core operands, creates
no worker, and never resolves or executes the XT. Thus unchanged `CORE-RUN`
has no valid target: it rejects core 0 as self and every negative or
above-range value as invalid before `WAKE-CORE`. `CORE-WAIT 0` returns,
`CORES` lists only core 0 as self/running, and `P.RUN-PAR` takes ordinary
ordered `P.RUN`; it dispatches nothing, provides no parallel speedup, and
leaves all five `PAR-*` variables zero.

This does not silently repair source defects. `ALL-CORES-WAIT` and
`ALL-FULL-WAIT` use plain `DO`; with equal start and limit at one they enter a
phantom core-1 iteration instead of zero-tripping. Strict `CORE-STATUS` makes
both and derived `BARRIER` fail promptly. `LOCK` polls the underlying owner
lock with non-suspending `YIELD?`; same-core acquisition is depthless and
contention cannot progress on this profile. No fairness, timing, queueing, or
explicit memory-fence property is inferred from those wrappers.

The source-only multicore pipeline branch dispatches at most one initial step
to each secondary full core, runs all remaining steps on core 0, and waits; it
is neither round-robin nor worker reuse. Shared `PAR-P`/`PAR-N` make it
non-reentrant, while `PAR-PIPE`/`PAR-STEP`/`PAR-CORE` are dead. It does not
check worker availability or validate pipeline descriptors and XTs and can
break ordered pipeline dependencies. The accompanying `AALLOT` safety claim
is also invalid because `CURRENT-ARENA`, `ARENA-STK`, and `ARENA-SP` are
global and unlocked. `P.BENCH-PAR` reports total cores, uses semantic Timer
work rather than physical speedup, performs non-normalized wrapping
subtraction, and leaks its original pipeline argument despite the declared
stack effect.

Exact unchanged lines 6923 through 7461 contain 539 LF records and 17,203
bytes, with SHA-256
`4e36452b9d65c41843f8b015065303375efae8667824c5bf606c30da6af32625`
and Git blob `022981afa233362debb10678b250ac044d8454d9`. They publish 91
definitions: 17 constants, 17 variables, and 57 colon definitions. The exact
hosted dictionary advance is 7,365 bytes. Load runs `RQ-INIT`, `AFF-INIT`,
`PREEMPT-FLAGS-INIT`, `MSG-INIT`, and `MSG-HINIT`, then rebinds deferred
`CORE-CHECKPOINT` to `_CORE-CHECKPOINT-PER-CORE`. It invokes no dispatch,
UART, lock, explicit Timer, storage, RTC, or IDL service and leaves both public
stacks empty. Semantic evaluation may advance an already-enabled Timer counter.

The declared queue/table layout has a systematic seven-byte discrepancy.
Each of nine `VARIABLE ... desired-size - 1 ALLOT` declarations includes the
variable's existing eight-byte body and therefore reserves `desired-size + 7`
bytes. Their initialized operational spans remain 4,896 bytes; the raw bodies
and allotments reserve 4,959 bytes. Those 63 slack bytes are retained rather
than normalized by the host.

Run queues are eight-index sentinel rings with capacity seven. Empty state is
head equal to tail; pop and clear do not erase slots, and XT zero can be
enqueued although consumers cannot distinguish it from an empty result.
Address/count operations validate neither core IDs nor mutable indices. The
qualified execution domain is canonical indices and core 0. `SCHED-CORE 0`
dequeues FIFO and executes each nonzero XT synchronously on the caller's
stacks. The core ID remains below the XT, so bodies must be stack-neutral.
Dequeue commits before XT execution; failure loses that item and leaves later
queue entries retained.

`SCHED-ALL` is not a one-core no-op. Each secondary pass uses plain
`NCORES 1 DO`; equal bounds enter at index 1 rather than zero-tripping. The
loop walks dormant tables 1–15 and then unchecked addresses beyond the
initialized arrays, where arbitrary dictionary bytes may provoke a fault or
dispatch attempt. Only an uninterrupted full-cell index cycle would reach the
loop exit. Bounded hosted evidence stops before the core-0 drain and proves
that queue 0 remains untouched. A populated phantom queue can be popped before
`CORE-RUN` rejects its ID. `SCHED-BALANCED` and `SCHED-AFFINE` inherit this
defect and are admitted only for bounded failure/state evidence, not successful
scheduling.

Work stealing mutates the same unlocked ring tables synchronously. `BALANCE`
is a no-op at one full core. Direct `STEAL-FROM`/`WORK-STEAL` can address
dormant tables; victim equal to thief rotates an item, zero XT is popped and
reported as failure, and a full-target abort occurs after the victim pop.
There is no worker-driven, automatic, or concurrent stealing contract.

Affinity is registry metadata with partial publication. `AFFINITY!` and
`AFFINITY@` reject only task indices at or above eight, not negative indices,
and never validate stored core IDs. `SPAWN-ON 0` enqueues first, then, while
the registry has space, appends a 48-byte READY descriptor with priority 128,
zero saved stacks/name, and a table/affinity entry. At saturation it still
queues but publishes no descriptor. `SCHED-AFFINE` queues that already-queued
READY XT again, marks the descriptor RUNNING before its broken `SCHED-ALL`
tail, and does not mark it DONE.

Per-core preemption is retained software state, not task preemption.
`PREEMPT-ON-ALL` writes low-32 `TIME-SLICE`, Timer control 7, and the global
gate, but no admitted ISR or unchanged KDOS word maps Timer pending state to a
per-core flag. The final checkpoint ignores the older global
`PREEMPT-FLAG`. On core 0 a manually populated table flag is cleared before
non-suspending `SCHED-YIELD`; a worker checkpoint only clears and continues.
`PREEMPT-OFF-ALL` writes control 1, leaving the counter enabled, clears the
gate and all 16 flags, and neither resets nor acknowledges sticky match or
pending IRQ state.

Messages are shared-memory sentinel inboxes, also capacity seven, with no IPI
or wake notification. The one-core qualified path is synchronous self-send
and receive; broadcast excludes self and returns zero. IDs and table indices
are largely unchecked. Global send scratch is written before lock 7 and
receive scratch is reread after unlocking, so the source is not reentrant or
made race-free by the lock. Successful `MSG-RECV` accidentally retains
`COREID`, producing `( core type sender payload -1 )`, while its empty path
returns the documented four zeroes. `MSG-DISPATCH` retains that core beneath
handler inputs/results. `MSG-FLUSH` consequently returns an initial zero plus
one `COREID + 1` cell per consumed message rather than a count. Handler store
has no type bound, and handler lookup accepts negative signed types as less
than four and can address before the table.

Named resource locks are opt-in wrappers around physical-core ownership, not
proof that the named subsystems acquire them. All hosted semantic tasks share
core ID 0. Same-core acquisition is depthless, one release ends a nested
critical section, and a foreign-owner wait cannot progress through the
non-suspending checkpoint. `WITH-LOCK` balances its `>R`/`R>` sequence and
releases only on normal return; a throw or abort strands ownership. Static
`LOCK-INFO` labels are not live lock state and omit the later networking lock
12.

Exact unchanged lines 7462 through 7568 contain 107 LF records and 3,693
bytes, with SHA-256
`7f349876f58c132cf72f116c0fa764a97ff0963679abb78d961e4f9a08770932`
and Git blob `3c13145b43c2eadc14841326f2fef22d34d01b6a`. They publish
`NUM-CLUSTERS` followed by thirteen colon definitions through `.CL-MPU`,
advancing the hosted dictionary by 398 bytes. Loading publishes definitions
only: it performs no cluster, barrier, scratchpad, MPU, UART, storage, lock,
or explicit Timer operation and leaves both public stacks empty.

The source hard-codes three clusters even though the hosted topology has none.
Its signed validation accepts only IDs 0 through 2. `CLUSTERS-OFF` and valid
`CLUSTER-DISABLE` calls succeed idempotently because they store zero;
`CLUSTERS-ON` and valid `CLUSTER-ENABLE` calls fail at the zero-only BIOS
boundary with their computed nonzero mask retained. Invalid IDs abort before
reading the mask. `CLUSTER-STATE` always prints three disabled rows, which is
a report of the source-declared request bits rather than hardware inventory.

`HW-BARRIER-WAIT` fails immediately at `BARRIER-ARRIVE` instead of entering an
unbounded poll. For an offset whose computed scratchpad address remains
unmapped, `SPAD-C@` faults without replacing that address and `SPAD-C!`
consumes its byte and address before the unmapped-store fault, preserving the
BIOS store mutation order. The source does not bound the offset: wrapping cell
addition can leave the sentinel aperture and access mapped Bank 0 or another
address class, so this API is not fail-closed for arbitrary cells. Cluster
privilege and MPU wrappers fail on their first primitive. In particular,
setup retains `( base limit )`, enter/exit retain their newly pushed privilege
value, off stops before either MPU write, and `.CL-MPU` retains its heading and
privilege-label output before failing.

Executable BIOS `MICRO?` compares unsigned against `N-FULL` and does not check
`NCORES`. Earlier KDOS `MICRO-CORE?` and `FULL-CORE?` use signed comparisons;
therefore the first sign-bit-set cell, `0x8000_0000_0000_0000`, is BIOS-micro
but KDOS-full. This is a documented source/BIOS discrepancy, not silently
normalized by the simulator.

Exact unchanged lines 7569 through 7838 contain 270 LF records and 8,868
bytes, with SHA-256
`c982515e55f9e94af0122ae1cd9e02af902774105bf59f65eae5a491973dfb82`
and Git blob `467892ab2c4d04851a9c8db7dc95eafe860f3ec8`. They publish 58
definitions: two constants, eight `CREATE` tables, 22 variables, and 26 colon
words. Hosted dictionary growth is exactly 4,519 bytes: 1,527 bytes of
headers/semantic slots, 176 bytes of initialized variable bodies, and 2,816
bytes of raw table allocation. Load invokes no UART, key input, filesystem,
storage, direct NIC, or rendering operation. In particular, `ALLOT` does not
clear the eight registry tables; only `VARIABLE` bodies and cells explicitly
written by source or registration have defined initial content.

Pseudo-BIOS `NET-STATUS = 0` makes unchanged `NET-RX?` return canonical false
without claiming a NIC. `.HEXDIG`, `AT-XY`, `PAGE`/`CLS`, color controls, and
`HBAR` retain their exact UART byte sequences; `HBAR` emits 60 raw `0xC4`
bytes rather than a UTF-8 glyph. With `FS-OK = 0`, `SHOW-NTH-DOC` records its
selector and returns without touching input or storage. Its mounted successful
path remains deliberately interactive and blocks at `KEY` after publishing a
document.

`REGISTER-SCREEN` returns zero-based IDs, initializes each admitted row's
key/action/subscreen-count cells, and returns `-1` without mutation once 16
rows exist. `ADD-SUBSCREEN` admits eight entries per parent and silently
consumes a ninth request. Neither it nor the handler setters validates an ID.
`SCREEN-SUBS` and `SCREEN-SELECTABLE?` likewise assume a valid 1-based global
`SCREEN-ID`. Unregistration shifts live rows and eight-cell subscreen blocks,
but source-literally leaves every vacated tail cell stale. Removing the
current screen resets `SCREEN-ID = 1`, `SCR-SEL = -1`, and `SCR-MAX = 0`, even
when no rows remain; it does not reset `SUBSCREEN-ID`.

The following unchanged-source discrepancies are part of this acceptance:

- on a `FIND-NTH-ACTIVE` match, the in-loop `DROP` removes the running count
  and the post-loop `DROP` then underflows an empty caller or consumes one
  pre-existing caller cell; `FNA-FOUND` is nevertheless set before the fault;
- `SCREEN-HEADER` uses plain `NSCREENS @ 0 DO`, so zero rows enter a wrapping
  loop instead of producing an empty header, while the zero-row footer formats
  `NSCREENS - 1` and reads stale row-zero subscreen state; and
- failed label execution is converted to visible `?`, but the exact
  `label-xt ['] EXECUTE CATCH` path leaves the saved data-stack-pointer cell
  exposed after the throw instead of satisfying the declared empty result.

Exact unchanged lines 7839 through 8339 contain 501 LF records and 18,051
bytes, with SHA-256
`a47d29e51c6754e24852bea08261b3119389e8a1849b9e39322bf1e9013cce7d`
and Git blob `01a3e0eff93567b66441e071003b3e7a25809d3d`. They publish 86
definitions: 16 constants, one 120-byte `WVEC` created body, 65 colon words,
and four variables. The 17 string-bearing colon definitions contain 102
compiled `S"` literals in 1,939 body bytes. Total hosted dictionary growth is
4,297 bytes: 2,206 bytes of headers, names, and semantic slots, 1,939 bytes of
literal pools, and 152 bytes of vector/variable bodies.

Load executes `INSTALL-TUI`: slots 0 through 12 and 14 receive the ordinary
ANSI TUI XTs, while the declared `WV-NONE` slot 13 retains its prior raw
`ALLOT` byte pattern. The four statistics variables are zero-initialized.
There is no key read, filesystem or storage I/O, direct NIC access, or UART
publication during load. Focused byte oracles cover selected public widgets,
scalar rows, document enumeration, absent-storage `SCR-STORAGE`, one-core
`SCR-CORES`, and `SCR-HOME-NET`; the zero-buffer statistics helper is
qualified separately. Their current selected renderer still publishes ANSI
bytes to UART. This is renderer-neutral source dispatch evidence, not a
rich-frame, compositor, or physical-viewer checkpoint.

The following additional source-literal discrepancies are accepted and left
visible:

- `WV@` and `WV!` neither check the 0-through-14 index nor validate the XT.
  Slot 13 is not installed, so dispatching it without an explicit binding can
  execute retained raw allocation bytes as an XT.
- `TUI-LIST` special-cases exact zero but accepts negative or high-cell
  counts; its `SWAP 0 DO` can then traverse essentially the whole cell domain.
  Callers must supply a nonnegative, bounded count.
- `TUI-DETAIL` tests `count >= selection` in its exit branch. It therefore
  suppresses every valid selection (and the `selection = count` boundary),
  while a larger selection prints the separator and executes the numeric
  selection as an XT, leaving the supplied detail XT on the stack.
- `TUI-INPUT` blocks indefinitely at `KEY`, including after a truncated CSI.
  A simple sequence such as `ESC [ A` balances, but every non-final byte in a
  parameterized CSI remains on the data stack. For example, `ESC [ 1 ; 5 A`
  leaves `49 59 53` above `( buf maxlen pos )`, corrupting subsequent input
  handling despite the source comment that CSI is consumed harmlessly.
- `.STOR-ROW ( slot i -- )` prints and drops only `i`, returning `slot`.
  `.DOC-FILE-LIST` resets numbering separately for each type, leaves
  `DOC-TUT-COUNT` stale when no filesystem is mounted, and `.DOCS-BODY`
  publishes only the final tutorial count as `SCR-MAX`; the later combined
  document activation path consequently does not share the displayed index
  model.
- Selected `.STOR-BODY` reaches the already documented matched-path extra
  `DROP` in `FIND-NTH-ACTIVE`. `.HOME-MEM-BUFS` uses `BUF-COUNT @ 0 DO`, so a
  true zero count wraps instead of making a zero trip. `SCR-HOME-MEMORY`
  computes free dictionary space from a hard-coded 65,536-byte ceiling rather
  than the hosted dictionary region.
- `.BSTATS-BODY` returns before clearing its four counters when `BUF-COUNT` is
  zero, so prior values remain stale. The Home network views translate absent
  `NET-RX?` into the user label `idle`; that label is not evidence that a NIC
  exists.

Exact unchanged lines 8340 through 8568 complete §9 in 229 LF records and
7,772 bytes, with SHA-256
`6294e7f8f2170e73bf7188481a8ae0575564e11b75e8fb61ae808ed305f155c1`
and Git blob `9de3741357f813221f0f44216340cc55c2f51cd0`. They publish 23
zero-body colon definitions using exactly 604 bytes of hosted headers, names,
and semantic slots. Load registers nine screens in source order, installs
`TASK-KEYS` on zero-based row 4, and registers three Home plus two Buffer
subscreens. It leaves `NSCREENS = 9`, `SUB-COUNTS = (3,2,0,0,0,0,0,0,0)`,
`SCREEN-ID = 1`, `SCR-SEL = -1`, `SCR-MAX = 0`, and `SUBSCREEN-ID = 0`.
Unused physical table tails remain untouched. No UART, key, filesystem,
storage, NIC, or renderer operation occurs during load.

Focused acceptance pins all 14 label byte sequences, both `RENDER-SCREEN`
branches, normal task kill/restart and document fallback dispatch, caught
renderer failure, bracket and CSI key handling, empty-list navigation, a
pre-terminated `SCREEN-LOOP`, and bounded public `SCREENS`/`SCREEN` entries.
This finishes the ordinary ANSI screen source seam; it does not load
`rich-terminal.f` or qualify a rich projection, compositor, viewer, or
revision-bound rich input.

The tail adds these source-literal discrepancies:

- The positive-subscreen branch of `RENDER-SCREEN` retains its normalized
  parent index, then recomputes the raw dispatch index. Every normal Home
  render therefore returns `0`, every Buffers render returns `1`, and repeated
  refreshes grow the data stack despite the declared `( -- )` effect.
- The initial invalid-`SCREEN-ID` normalization affects only the retained
  local index. `SUB-TABS`, subscreen dispatch, and the footer reread the raw
  global ID; `SWITCH-SCREEN`, `CALL-SCREEN-KEY`, `DO-SELECT`, and public
  `SCREEN` likewise do no complete bounds validation. `SUBSCREEN-ID` is not
  clamped before table lookup. Invalid state can therefore read or execute
  inactive physical table cells.
- Renderer, screen-action, and key-handler `EXECUTE CATCH` paths inherit the
  already documented saved-data-stack-pointer leak when their target throws.
  `TASK-KEYS` rejects selection `-1` specifically, but another signed-negative
  selection passes its upper-bound check and reads before `TASK-TABLE`.
- `HANDLE-KEY` checks availability before the CSI prefix only. A truncated
  `ESC [` blocks at the direction `KEY`; a parameterized sequence such as
  `ESC [ 1 ; 5 D` consumes only `1` and leaves `;5D` queued as later commands.
  Navigation with `SCR-MAX = 0` stores selection zero rather than preserving
  the empty `-1` sentinel.
- `SCREEN-LOOP` busy-polls `KEY?` and `CYCLES` without `PAUSE` or `IDLE`; it is
  intentionally unbounded until `SCREEN-RUN` clears. Re-evaluating the slice
  is also not idempotent: it appends registrations until the 16-row table is
  full and appends duplicate Home and Buffer subscreens.

Exact unchanged lines 8569 through 8943 add §10's transport-independent
Data Port structures and bindings, the empty §11 benchmark placeholder,
§12's text Dashboard/status words, and §13's Help system. The 375 LF
records occupy 15,702 bytes, have SHA-256
`0fff19ac85b6b0ff1261e587a1a0d7462035ac2f453229f58236af37e465a713`,
and have Git blob `7f5cd3054b3936f5e0561cbd53395da0af50d309`. They publish
27 definitions in source order: one constant, five variables, and 21 colon
definitions. The hosted dictionary grows by exactly 4,264 bytes: 459 fixed
header/semantic-slot bytes, 211 name bytes, and 3,594 body bytes.
`FRAME-BUF`, `PORT-TABLE`, `ROUTE-BUF`, `HW-FOUND`, and `HW-CSTR` have body
spans of 1,507, 2,048, 8, 8, and 23 bytes respectively; every other new word
has a zero-byte hosted body. The many compiled `."` publishers are semantic
operations rather than guest body-literal pools.

Load explicitly clears the complete 2,048-byte `PORT-TABLE`. `VARIABLE`
clears the leading cell of the other variables, while the 1,499-byte
`FRAME-BUF` and 15-byte `HW-CSTR` `ALLOT` tails retain their prior bytes.
The earlier `PORT-COUNT`, `PORT-RX`, and `PORT-DROP` cells are not reset.
There is no frame receive, descriptor binding, heap setup, UART or key
publication, filesystem or storage operation, NIC access, RTC change, or
lock change during load. Evaluation advances only the ordinary timer counter;
its compare, control, status, and interrupt state remain unchanged.

Focused acceptance covers ordinary bind/rebind/unbind transitions at IDs zero
and 255, the source's zero-binding counter drift, and address-only boundary
arithmetic without dereferencing invalid slots. It pins little-endian frame
header access, `FRAME-DATA`, exact `.FRAME`, `PORTS`, and `PORT-STATS` UART
bytes, both rule publishers, and the ordinary `STATUS` line. Specific Help
lookup covers found and missing words and the broken related-word result. The
complete 7,431-byte `HELP` publication has SHA-256
`c1d44c8970fa800f943db3e9b081cdaaf642af429c6cf4f9df27bcc63a2f1d07`.
This evidence does not execute the heap-reporting `.MEM`, `MEM-REPORT`, or
full `DASHBOARD` paths and does not qualify the later networking transport;
the following Pipeline Bundle implementation is qualified separately below.

The following source-literal discrepancies remain visible:

- `FRAME-BUF` is described as 1,500 bytes, but the eight-byte `VARIABLE` cell
  plus `1499 ALLOT` gives it a 1,507-byte body. Only its leading cell is
  initialized at load. `FRAME-SRC`, `FRAME-TYPE`, `FRAME-SEQ`, `FRAME-LEN`,
  `FRAME-DATA`, and `.FRAME` trust the current bytes completely: they carry no
  received-frame validity or freshness marker and perform no local payload
  length or type validation.
- `PORT-SLOT` accepts every cell. ID `-1` computes eight bytes before the
  table, and ID 256 computes the following `ROUTE-BUF` header address;
  `PORT@`, `PORT!`, and `UNPORT` then dereference those unchecked addresses.
  `PORT!` also accepts any nonzero cell as a descriptor, with no ownership,
  liveness, or Buffer-layout proof. A later transport call can therefore
  dereference an arbitrary or destroyed descriptor.
- `PORT!` increments `PORT-COUNT` when a zero value is stored into an empty
  slot. Repeating that operation increases the count while the slot remains
  unbound; replacing a nonzero value with zero silently unbinds without a
  decrement, and `UNPORT` cannot repair it. The shared table/count update has
  no core restriction or synchronization. Re-evaluating this slice clears a
  replacement table but preserves the earlier count and statistics, so the
  section is not hot-reload-idempotent.
- The deferred networking layer is not qualified here. Its outbound DTYPE
  mapping conflates Buffer-layout and wire-type enums, receive routing ignores
  `FRAME-TYPE`, and outbound header construction truncates an ID to one byte
  even though the core binding path first uses the unchecked full cell as a
  table index.
- `.MEM` labels `SP@ HERE -` as `Free`, but that is a raw address gap which
  includes heap and reserved space and can wrap into signed-looking output.
  `.MEM` and `MEM-REPORT` call `.HEAP`; before normal startup that supposedly
  observational path can run lazy `HEAP-SETUP`, align `HERE`, initialize heap
  headers, and fix `HEAP-BASE`. Their execution must remain isolated from a
  runtime used to extend the contiguous source load.
- `HW-CSTR` has a 23-byte body, while the maximum `PN-LEN = 23` query needs
  24 bytes for its count plus payload. `HELP-WORD` therefore writes the final
  query byte into the low byte of its own following header link. Longer input
  is truncated to 23 bytes, so longer dictionary names cannot be queried
  exactly.
- In the related-word loop, the live stack is `( count entry name-addr
  name-len )`; `2 PICK` selects `entry`, not `count`. A real header address is
  never below ten, so every lookup reports zero related words. If that branch
  were reached, `TYPE` would leave only `( count entry )` and the following
  `ROT` would underflow.
- `.HELP-ALL` advertises `POLL`, `INGEST`, and Bundle words before those
  definitions exist at the line-8943 boundary. The Bundle words arrive in the
  following qualified §15 slice, but the surrounding §10 source also promises
  `RECV-FRAME`, `ROUTE-FRAME`, `PORT-SEND`, and the deferred networking layer.
  None of those transport names, including `PORT-SEND-SLICE`, is executable
  through the current EOF line-9894 frontier; qualified Help text does not
  qualify a transport operation.

Exact unchanged lines 8944 through 9121 add §15 Pipeline Bundles in 178 LF
records and 5,801 bytes, with SHA-256
`370c6c6d17470ae7ea0c8a94ca5ede4ddcae04a8c9e0badcb007cc5358ef919f`
and Git blob `a7f49a7d29bbfa61d043dae73854924e74f4b2f8`. The checked fixture includes
the following §18 separator at line 9122: 179 LF records and 5,873 bytes, with
SHA-256
`8791e5eecef059d052ecd8b69976317857c41c29ae475e18cc53d79761d8b922`
and Git blob `3690e82c7a15e69fa69c84186fdda0caa5937d42`.

The slice publishes 27 definitions: `FTYPE-BUNDLE`, fourteen state variables,
and twelve colon words from `BDL-RESET` through `.BUNDLE`. Its 261 name bytes,
112 bytes of variable bodies, and 459 fixed hosted header/semantic-slot bytes
advance the dictionary by exactly 832 bytes. The compiled `."` publishers are
semantic output operations and add no guest literal pool.

All fourteen variables are explicitly initialized at load. `BDL-ACTIVE`,
`BDL-DRY`, `BDL-VER`, the three `BDL-N*` counts, `BDL-SCHED-I`,
`BDL-SCHED-F`, `BDL-POL-PERM`, and `BDL-POL-RET` become zero;
`BDL-SCHED-P` becomes all-ones (`-1`), `BDL-POL-EXP` becomes 3,
`BDL-SCR-DEF` becomes 1, and `BDL-SCR-MASK` becomes 255. `FTYPE-BUNDLE` is
7. Those dictionary stores are the only load-time execution: no bundle word
parses an active-input operand, emits UART output, constructs an object,
accesses filesystem/storage/NIC state, operates a lock, mutates the RTC,
schedules work, or renders.

Focused direct acceptance covers reset/begin state, all three configuration
setters, and the dry and live declaration branches. `BDL-RESET` preserves
`BDL-DRY` while restoring the other defaults; `BDL-BEGIN` then records the
supplied version and sets `BDL-ACTIVE`. In dry mode, `BDL-BUF`, `BDL-KERN`,
and `BDL-PIPE` consume their complete numeric inputs and line-local names and
increment only their bundle counters. In live mode they call the unchanged
`BUFFER`, `KERNEL`, and `PIPELINE` constructors before incrementing those
counters. Focused live construction uses small ordinary objects in a
disposable runtime.

Both `BDL-END` report branches and both `.BUNDLE` display branches have
byte-exact output evidence. Dry `BDL-END` reports all tracked fields without
changing `TIME-SLICE` or `SCREEN-ID`; live `BDL-END` conditionally copies the
tracked interval to `TIME-SLICE`, always copies the tracked default screen to
`SCREEN-ID`, prints the loaded counts, and clears `BDL-ACTIVE`. Direct dry
declarations overwrite only transient `WORD` bytes at `HERE`: they do not
advance `HERE`/`LATEST`, publish their requested names, or change the Buffer,
Kernel, or Pipeline registries.

Wrapper qualification remains disk-free. Each wrapper's hosted IR is checked
to call the exact `LOAD` execution token captured when §15 compiles; this pins
`BUNDLE-LOAD`'s leading `BDL-DRY = 0` store and `BUNDLE-INFO`'s leading-one,
call, trailing-zero sequence without touching a disk. For behavioral INFO
evidence, a dedicated runtime publishes a shadow `LOAD` before compiling the
slice. That shadow consumes the outer line's filename and evaluates a bounded
synthetic bundle in the same context. A normal `BUNDLE-INFO` presents
`BDL-DRY = 1`, exercises all three dry name paths without constructing
objects, and then clears the flag to zero.

The caught failure oracle executes a real guest `THROW` from the nested
synthetic source. It proves that `BUNDLE-INFO`'s trailing clear is skipped:
`BDL-DRY = 1`, `BDL-ACTIVE = 1`, version 9, and the partial count tuple
`(1,0,0)` persist. The three real registries, `HERE`, and `LATEST` remain
unchanged because dry mode created no resources and `BDL-END` was not reached.
The shadow must precede slice compilation because compiled `Call` operations
retain an XT; defining a newer `LOAD` later cannot redirect them, and removing
the captured shadow would leave stale calls.

The following source-literal limits remain part of the contract:

- `BDL-ACTIVE` is bookkeeping only. No declarative word or `BDL-END` checks
  it, and neither wrapper requires a file to contain one balanced
  `BDL-BEGIN ... BDL-END` sequence. Empty or truncated input can retain old
  fields or leave the bundle active.
- Version zero is accepted by `BDL-BEGIN`, but `.BUNDLE` tests `BDL-VER`
  rather than `BDL-ACTIVE`; it reports `(no bundle loaded)` for an active
  version-zero declaration and reports nonzero retained state after END.
- Live END applies only two fragments of configuration. It uses
  `BDL-SCHED-P` only as a `-1` sentinel, writes the interval but neither
  schedules that pipeline nor acts on the auto/repeat flags, and writes the
  default screen without validation. It never applies `BDL-SCR-MASK`. The
  initialized and example mask 255 has only eight bits although §9 registers
  nine screens; a nine-screen mask would require 511.
- The bundle counts are independent wrapping cells, not verified resource
  counts. Dry declarations increment them without resources. Live Kernel and
  Pipeline declarations continue to allocate descriptors and named constants,
  and increment bundle counts, after the ordinary registries saturate at 32
  and 8; those objects are omitted from registry listings. The Buffer registry
  is linked and unbounded except by dictionary capacity.
- A bundle is arbitrary Forth source evaluated by general `LOAD`.
  `FTYPE-BUNDLE` is never checked, versions and declaration fields are not
  validated, and dry mode affects only the three `BDL-*` object declarations.
  Any other word in a supposedly inspected bundle executes normally.
- `BUNDLE-INFO` is therefore not side-effect-free. Even a conventional dry
  bundle resets and rewrites shared tracking state and emits its report;
  arbitrary source can do more. Its `BDL-DRY` cleanup occurs only after a
  normal `LOAD` return, so a guest throw strands dry mode and partial tracking
  as the caught oracle demonstrates.
- Both wrappers inherit `LOAD` behavior. The pinned pre-decision revision used
  raw `EVALUATE`, ignored `EVAL-STATUS`, and omitted `EVALUATE-FINISH`; that
  historical path could commit after malformed source. Current conformance
  requires the shared checked-loader lifecycle, so a bundle cannot report
  nominal success after an undefined, overlong, or unfinished input. The
  false-filesystem filename-consumption behavior and bundle policy gates are
  separate matters.
- No bundle-level transaction, unload, ownership record, or idempotence
  mechanism exists. Generic `LOAD` nevertheless owns a dictionary transaction:
  a bundle failure caught as guest `THROW` rolls all definitions and dictionary
  bodies back to its saved `HERE`/`LATEST`, even before module hooks are
  installed. That does not make
  bundle execution atomic. Allocator reservations, registry links/counts,
  tracking/configuration stores, output, media effects, and other
  non-dictionary state may survive because no bundle transaction owns them.
  `BDL-RESET` clears tracking but neither frees those resources nor restores an
  already-applied `TIME-SLICE`/`SCREEN-ID`; repeated successful loading can
  shadow names, duplicate resources, and drive bounded registries into
  saturation.
- Policy permissions, retention, and export are reporting-only cells. Nothing
  in this slice enforces them. The scheduling flags and screen mask are also
  reporting-only. All bundle state, parser/evaluator state, registries, and
  constructor scratch are global and unlocked, with no nesting, core, or
  concurrent-owner isolation.

Exact current lines 9122 through 9214 add §18 Ring Buffer Primitives in 93
LF records and 3,031 bytes, with SHA-256
`3fa7f307956111f555ac07365f6b8fd1b9ad4b42a0f7240c88581118d01f3ec4`
and Git blob `783d29204b369b0fd05c352b82fac8bdbc46e755`. The checked fixture includes
the following separator at line 9215: 94 LF records and 3,103 bytes, with
SHA-256
`87599dcacd3fbc9a979028d47b9456e63a4be00931ae0994d1348772b0513e89`
and Git blob `4db5792de3de17318a66eb46696c0382c919ede2`. §19 Hash Table Primitives
begins at line 9216 and is not part of this slice.

The slice publishes fifteen definitions in source order: the `RING` defining
word; seven `RING.*` accessors; `RING-FULL?`, `RING-EMPTY?`, and
`RING-COUNT`; the `_RP-RING` variable; and `RING-PUSH`, `RING-POP`, and
`RING-PEEK`. Its 133 name bytes, eight-byte variable body, and 255 fixed
hosted header/semantic-slot bytes advance the dictionary by exactly 396
bytes. All fourteen colon words have zero-byte hosted bodies. Load merely
publishes those definitions and zero-initializes `_RP-RING`; it constructs no
ring, acquires no lock, emits no output, and changes no registry, storage,
RTC, UART, screen, scheduler, or other device state. Only the ordinary timer
counter advances while the source is evaluated.

Focused acceptance stays within positive, physically small geometry whose
`elem-size * capacity` product fits the available dictionary interval. It
pins the actual constructor layout, every accessor and initial predicate,
byte-exact multi-byte FIFO copies, full and empty rejection, head/tail/count
updates, wraparound, bounded peek order, and lock release after each admitted
push or pop. Constructor evidence poisons the future dictionary interval and
proves that `RING` writes its six header cells but leaves the allotted payload
unchanged. A zero-capacity ring is also covered only on its guarded ordinary
paths: it is both full and empty, push and pop return zero, a nonnegative peek
returns zero, and lock 4 is released. Two rings prove that source stores the
same machine-wide lock number 4 in every descriptor and that sequential
operations leave no owner behind.

That evidence admits only intact `RING`-created descriptors, mapped caller
spans of at least `elem-size` bytes, non-destructive `CMOVE` overlap, and
indices in `0 <= idx < count`. A caller using the lock-free peek result must
also provide its own lifetime synchronization against a pop or wraparound
overwrite. It does not admit malformed descriptors, faulting copy spans,
concurrent producer/consumer execution, or a fault while the ring lock is
held.

The unchanged source retains these discrepancies and unsafe domains:

- The descriptor is six cells and 48 bytes: `RING` stores six fixed cells and
  `RING.DATA` returns `ring + 48`. Payload therefore begins immediately after
  that header. With capacity
  zero, no payload is allotted and that address aliases the following named
  constant's header rather than ring-owned data.
- `RING` does not validate element size, capacity, their wrapping product, or
  remaining dictionary space. `ALLOT` interprets its cell as signed, so a
  negative or high-bit product can rewind `HERE` after some header cells have
  already been written. There is no constructor rollback, registry,
  destructor, or ownership record.
- The constructor neither aligns nor clears its descriptor or payload. It
  starts at raw `HERE`; element storage has no alignment promise, and bytes in
  a newly empty ring retain whatever previously occupied that interval.
- All descriptor cells are writable and trusted. Push and pop validate no
  head, tail, count, capacity, lock number, element pointer, mapped span, or
  arithmetic result. Offset multiplication and addition wrap, and `CMOVE`
  preserves its forward-copy overlap behavior rather than proving disjoint
  element storage.
- Full and peek bounds use signed `>=`, and index arithmetic uses signed
  `MOD`. A negative index can pass the peek bound. With head zero, element
  size eight, and a positive capacity, index `-1` returns `ring + 40`, the
  lock cell, rather than zero. On a zero-capacity ring the same negative index
  reaches `MOD 0` and traps; only the nonnegative empty-ring path avoids it.
- Every constructed ring stores global `RING-LOCK = 4`, serializing otherwise
  independent rings. `_RP-RING` is one shared, retained scratch cell written
  before lock acquisition. Current constructors all use lock 4, but a
  manufactured or concurrently changed lock field can make the final unlock
  consult another descriptor and release the wrong lock or leave the acquired
  one held.
- Push and pop have no unwind guard around their critical section. A guest
  throw, invalid descriptor, copy fault, or modulo fault after `LOCK` skips
  `UNLOCK` and strands ownership. Qualification therefore never deliberately
  faults while locked.
- `RING-PEEK` is deliberately lock-free and returns a mutable internal
  pointer, not a copied or versioned value. Concurrent head/count observation
  need not be coherent, and a successful returned address can be popped or
  overwritten immediately after return.

Exact unchanged lines 9215 through 9383 add §19 Hash Table Primitives in 169
LF records and 5,352 bytes, with SHA-256
`ce5fc5c20a4905a0092ec28cd647c0d1679317334968db81084aba7bf6410e24`
and Git blob `3c465404ec02b189269d5c982ee360c9d070e638`. The checked fixture includes
the following separator at line 9384: 170 LF records and 5,424 bytes, with
SHA-256
`9379a85c46423efe2d14242f61bb974f6d1fa746cd9449b046cfbc3dbebdb467`
and Git blob `b75a16f60f80d7885323443843919b8946af38ea`. §20 Module System begins at
line 9385 and is not part of this slice.

The slice publishes 28 definitions in source order: seventeen colon words
from `HASHTABLE` through `HT-EACH` and eleven scratch variables across the
constructor, put, get, delete, and iteration paths. Its 211 name bytes, 88
bytes of variable bodies, and 476 fixed hosted header/semantic-slot bytes
advance the dictionary by exactly 775 bytes. Every variable is zero at load
and every colon has a zero-byte hosted body. Load constructs no table, hashes
no key, acquires no lock, emits no output, and changes no CRC transaction,
registry, storage, RTC, UART, screen, scheduler, or other device state. Only
the ordinary timer counter advances while the source is evaluated.

Seven focused tests qualify the positive-small, single-core domain. The
constructor evidence pins its actual 40-byte header, field accessors, stride,
slot/key/value address arithmetic, named constant placement, and complete
zero-fill of the caller-requested slot interval. A one-byte-key,
one-byte-value, four-slot table pins non-reflected CRC mode 0 results and a
four-key collision chain. Those hashes are CRC-32/BZIP2-family results, not
the reflected CRC-32 used by zlib: keys `01`, `05`, `09`, and `0D` produce
`B5365DFC`, `A6322B20`, `933EB044`, and `803AC698`, respectively, and all
reduce to initial slot zero modulo four.

CRUD evidence proves linear probing, exact key/value copies, updates without
count growth, lookups, deletes, and owner release. Filling all four physical
slots leaves count four. Inserting a new key then returns normally with no
status while leaving bytes and count unchanged; unchanged source silently
drops the entry. Iteration evidence uses a callback that consumes exactly
`( key-addr val-addr -- )`, visits occupied slots in physical slot order,
skips a tombstone, preserves table state, and leaves the shared iteration
scratch containing the callback XT and table address. A separate bounded
equal-size nested-iteration oracle pins the reentrancy defect: the inner call
replaces both scratch cells, so the outer scan invokes the inner callback for
its remaining physical slot rather than restoring its own table and XT.

The tombstone oracle pins the source's duplicate/resurrection defect. After
two colliding keys occupy slots zero and one, deleting the first marks slot
zero as tombstone 2 while retaining its key and value bytes. Putting the
second key with a new value immediately reuses that first tombstone instead
of continuing to find its existing slot-one copy. Count therefore becomes
two because it counts occupied physical slots, not unique keys. Deleting the
new slot-zero copy makes lookup find and effectively resurrect the old
slot-one value.

Zero-width and zero-slot evidence remains explicitly degenerate rather than a
production contract. A zero-length key makes every caller address compare as
the same key, so a later put updates the first physical entry. A zero-length
value copies no caller bytes and GET returns the computed one-past-key
address; in the tested geometry that address aliases the following slot's
flag. A zero-slot table has no data interval, so `HT.DATA` aliases its
following constant header. Direct `HT-HASH` completes and releases the CRC
transaction, then traps at signed `MOD 0`; this test does not enter a locked
mutator or the iterator.

The admitted ordinary domain requires positive key/value sizes and slot
count, canonical flags, intact `HASHTABLE`-created geometry whose complete
nonwrapping data interval fits the dictionary, mapped key and value spans of
the declared lengths, an uncontended CRC service, and one nonnested caller.
An `HT-EACH` callback must consume exactly the supplied two cells, avoid
reentry or table mutation, return normally, and treat both addresses as
borrowed mutable views. The zero-width tests only preserve source-literal
alias behavior; they do not broaden that useful domain.

The unchanged source retains these discrepancies and unsafe domains:

- `HT-HASH` uses `CRC32-BUF`, which selects non-reflected mode 0 with all-ones
  initialization and final XOR. Calling it standard CRC-32 without the mode
  qualifier invites a zlib/reflected-hash mismatch and different probe chain.
- `HT-PUT` treats the first tombstone exactly like an empty slot and inserts
  before searching the remainder of the chain for an equal key. Duplicate
  physical keys, inflated count, and resurrection of an older value are
  therefore source behavior.
- Insertion stores occupied flag 1 before copying the key or value and before
  incrementing count. `HT-GET` and `HT-EACH` are lock-free, so they can observe
  a published flag with stale or partially copied bytes. Delete changes only
  the flag to 2; key and value bytes remain resident.
- `HT.COUNT` is a wrapping count of slots whose transitions ran through these
  words, not a proof of unique keys or canonical flags. Full-table insertion
  has no failure result and silently drops a new key. Flags other than 0, 1,
  and 2 are treated as neither empty, occupied, nor reusable: probing skips
  them while count is unchanged.
- Every table stores global `HT-LOCK = 5`, serializing all writers. Shared
  constructor and operation scratch has no caller identity. `HT-PUT` and
  `HT-DEL` publish table scratch before lock acquisition; GET and EACH scratch
  is entirely unlocked. Cross-table concurrency, nesting, or reentry can
  redirect the table, key, value, callback, or final unlock.
- Hashing uses the runtime-global CRC transaction. Lock-free GET can contend
  with another GET, a writer, or an unrelated CRC caller. PUT and DEL hold
  lock 5 while hashing; a CRC error, invalid span, copy fault, modulo fault,
  guest throw, or other escape after acquisition skips `UNLOCK` and strands
  that global lock.
- GET returns a direct mutable value pointer and EACH supplies direct key and
  value pointers. Neither has ownership, generation, lifetime, or coherent
  read protection. A concurrent update/delete can mutate or tombstone the
  referenced slot immediately after return.
- EACH invokes its XT with exactly two stack cells and has no `CATCH`, stack
  cleanup, or reentrancy guard. A callback that retains cells, consumes the
  wrong shape, throws, mutates flags, or recursively calls EACH can corrupt
  the caller's stack/iteration; recursive use also overwrites `_HTE-XT` and
  `_HTE-HT` used by the outer scan.
- Key size, value size, slot count, stride, total byte product, flags, probe
  index, descriptor cells, caller spans, and arithmetic results are trusted.
  Multiplication/addition wraps; `HT-SLOT` can address before or beyond data;
  `CMOVE` proves neither bounds nor overlap. The constructor starts at raw
  `HERE`, adds no alignment, and has no failure rollback, registry,
  destructor, or ownership record. Negative/high-bit geometry can make signed
  `ALLOT` rewind after partial header writes before `FILL` acts on the derived
  span.
- A zero-slot descriptor makes data alias the following header. HASH and GET
  reach `MOD 0`; PUT and DEL acquire lock 5 before the same trap and therefore
  leak it. EACH uses plain `0 DO`, so equal zero bounds enter the full
  `2^64`-iteration domain rather than performing zero visits. Qualification
  executes only direct HASH for this geometry and deliberately avoids the
  locked and unbounded paths.
- Zero key size makes all keys equal because both CRC and `SAMESTR?` consume
  zero bytes. Zero value size returns an address without owning a value byte,
  which can alias the next slot or following header. These cases are pinned as
  literal degeneracies, not safe general-purpose maps.

The §19 source ends at line 9383; the §20 qualification below continues the
contiguous frontier. Real bundle-file integration, scheduler or cadence
behavior, concurrent ring/hash qualification, rendering, physical viewing,
and every rich-terminal module/projection/compositor/input seam remain
deferred.

### KDOS §20 module-system contract

Exact current lines 9384 through 9853 contain 470 LF records and 14,414
bytes, with SHA-256
`73adf1e903e12f891908750aeeced70d4888dfb6087af6372a99eca1495ecd74`
and Git blob `231b452a63ad3d70fc635f3e4b40a7033627fc68`. The checked fixture adds the
line-9854 §14 Startup separator: 471 LF records, 14,486 bytes, SHA-256
`6213a62e8bbc1ada04565d775a436cebc2ace9b5c9b32f27302b13568d9d92b6`,
and Git blob `be9ab02eced24379053654034ff4199bef57dbf3`. Line 9855 begins Startup and is
not admitted by that sentinel.

The slice publishes 69 words: 40 colons, 17 zero-initialized variables, six
ordinary constants, three `CREATE` objects, two `DEFER` objects, and one
`XBUF`-produced constant. Its 776 name bytes, 329 dictionary-body bytes, and
1,173 fixed hosted header/semantic-slot bytes produce exactly 2,278 bytes of
dictionary growth in the canonical XMEM-present composition. Load initializes
128 zero inline-bucket bytes and the five-cell registry
`( inline, 16, 0, 0, 5 )`, writes `PROVIDED` plus NUL, binds the private
allocator/free seam to `DMA-ALLOCATE`/`DMA-FREE`, and rebinds the three loader
transaction hooks. `XBUF` reserves the 128-byte `_REQ-CWD-STK`, advances both
XMEM frontier/floor cells, preserves the old bytes, and, for the canonical
empty free list, leaves `FL-NEED = 128`, `FL-PREV = FL-CURR = 0`. No registry
node or grown bucket is allocated. The no-XMEM fallback would instead add the
128 bytes to dictionary growth for a 2,406-byte total and is not this profile.

Eight focused tests pin those effects and the ordinary one-core behavior. They
cover exact case-sensitive FNV-1a identity, duplicate neutrality, useful ID
bounds, node OOM, stable-node rehash from 16 to 32 buckets, retryable bucket
OOM, full-frame commit/rollback, prescan boundaries, pre-registration OOM
cleanup and retry before any source prefix executes, mounted in-memory MP64FS
  self-cycle and duplicate skipping, exact list output, and nested child
  success joining the parent's registry and dictionary rollback closure.
  Successful nested evaluation merges its provisional IDs into the parent
  frame, so a later parent failure rolls back the child IDs and definitions
  together.
  The source definitions, MP64FS lookup/transfer, loader frames, allocator, and
  evaluator run unchanged; there is no hosted module registry or direct file
  shortcut.

The admitted public contract is exact byte identity on core 0. Parsed
`PROVIDED` and `MODULE?` use case-sensitive BL-delimited tokens;
`PROVIDED-SPAN` accepts an immutable mapped caller span. Useful IDs are 1–246
bytes. A duplicate insertion is persistent-allocation-neutral. New nodes own
their copied ID bytes in the Bank-0 heap and remain stable across bucket growth
and XMEM resets. Growth is best-effort when entry count exceeds twice bucket
count; a failed bucket allocation retains a usable pending registry, whereas a
failed node allocation throws `-4100`. Successful outermost source completion
commits every frame-owned ID. Successful nested frames merge their provisional
chains into the parent; a later parent failure therefore removes the nested
IDs along with every definition added since the parent's dictionary
checkpoint.

Literal source discrepancies bound that contract:

- Prescan is lexical rather than compiler-aware. It sees only exact uppercase
  `PROVIDED` in the first byte-32-delimited position of an LF record, strips
  one terminal CR consistently with evaluation, ignores tabs as whitespace,
  and stops at the first match. Lowercase or a
  different layout can execute later through case-insensitive dictionary
  lookup but loses duplicate suppression and pre-evaluation cycle breaking.
- A first matching line longer than 255 bytes or without an ID synthesizes an
  empty match and then throws `-4101`; no later declaration is considered.
  `PROVIDED-SPAN` also accepts raw blanks, NULs, newlines, and control bytes
  that parsed `MODULE?` cannot reproduce and `MODULES` emits unescaped.
- The 246-byte upper check uses signed `>`. A high-bit cell bypasses it and
  enters unchecked hashing, wrapped allocation, and copying. Caller mapping,
  immutability, allocator reentry, arithmetic, node fields, bucket geometry,
  count, and lock descriptors are not independently validated.
- Duplicate `REQUIRE` still ensures the filesystem, resolves/looks up the
  path, allocates and reads the transfer, saves a loader frame, and prescans
  before it skips evaluation. It is persistent-state-neutral, not I/O-neutral;
  module identity is also independent of file path and content.
- Module loading retains the public `REQUIRE`/`PROVIDED` surface and module
  identity. At the pinned pre-decision revision it inherited raw `LOAD`, did
  not validate final `EVAL-STATUS` or call `EVALUATE-FINISH`, and could strand
  the loader allocation/frame after an early read/prescan fault. Those are now
  nonconforming lifecycle defects: a module commits only after checked final
  completion, while every admitted failure delivered through guest `THROW`
  unwinds, rolls back, releases, restores, and runs after-release.
  Task-resetting aborts and non-guest backend faults remain outside that
  guarantee. File-type/flags policy remains open.
- Catchable failure rollback removes the active frame's provisional registry
  nodes and restores its saved `HERE`/`LATEST`; nested successful IDs have
  already merged into the parent and roll back with it. Output, allocator/registry side
  effects outside the module-ID transaction, and object/media effects are not
  made atomic. There is no public unload/reset for IDs committed by a
  successful outermost load; those nodes consume Bank-0 heap for the runtime's
  life, and old inline bucket bytes remain stale after rehash.
- Registry, loader, path, prescan, growth, and list scratch is global. Lock 5
  is shared with hash-table writers; `MODULES` takes it before UART lock 1 and
  prints bucket/chain order. Reentry, concurrency, pre-held locks, reverse
  lock ordering, malformed state, and non-guest machine faults are outside the
  focused contract.

The safe domain is one core with canonical uncorrupted registry/loader state,
immutable mapped 1–246-byte IDs, a genuine exact-uppercase first-token
declaration on bounded LF records, direct root filenames, available
Bank-0 node storage, production-compatible nonthrowing allocator bindings,
and no reentry or pre-held shared lock. §20 ends at line 9853; the following
§14 qualification completes the contiguous frontier through EOF line 9894.

### Current KDOS §14 startup and EOF contract

Exact current lines 9854 through 9894, including the section separator,
contain 41 LF records and 1,432 bytes, with SHA-256
`d14948c62ff524ed67fe0743f1f3976d3430c1754809bf339c45ac8bd3569f82`
and Git blob `64644994439ac09da0bd19db31866c404d380582`. The executable section from
line 9855 through EOF contains 40 LF records and 1,360 bytes, with SHA-256
`480ab7b30f349044fdfd2c10257aee4525348819e15938396865ce332efa71fb`
and Git blob `5f5d1922439468bbd5884505b3c5801e8d295269`. At the historical qualification
revision, the complete `kdos.f` had 9,894 LF records and 341,355 bytes, with
SHA-256
`99e71114ed141c14522d687a3bef3110ead94de7b0a055ae693c135a94772fb8`
and Git blob `fd017b16dbd3ef4746d0e3467e980c015cf5a664` at revision
`ed451faccfddb5f3fbb4e2200eb0dd0fdc314f4c`.

The section prints the exact one-core banner, conditionally calls `FS-LOAD`,
forces lazy Bank-0 heap initialization with a 16-byte `DMA-ALLOCATE`/`DMA-FREE`
round trip, publishes `_AUTOEXEC-NAME` and `_AUTOEXEC-RUN`, invokes the latter,
executes hosted `JIT-OFF`, and prints the final newline. For pre-section
`HERE = H`, let `A = align64(H)`. The canonical fresh-heap path fixes
`HEAP-BASE = A + 32768`, coalesces the temporary allocation back into the one
free block, and reaches `HERE = A + 71` before any data-dependent autoexec
dictionary effects: 27 name bytes, 10 created-body bytes, and 34 fixed hosted
header/code-slot bytes. All four accepted fixtures end there. Neither anonymous
interpret-`IF` body is published or charged to that ledger.

Five focused startup tests cover no disk, attached invalid media, a valid
15-sector MP64FS without `autoexec.f`, a valid mounted `autoexec.f` loaded
through the ordinary module machinery, and failure of the checked DMA heap
probe. They pin exact UART bytes, heap geometry, the two-definition header
chain, filesystem status and completion, media immutability, zero-padded
`NAMEBUF`, loader-frame/CWD restoration, module registration, duplicate
suppression, released locks, and exact probe-error propagation without a fake
free. Six separate
semantic-BIOS tests pin ordinary anonymous interpret `IF`: true/false and
nested branches, calls into ordinary colon definitions, compiled string
literals, continuation across checked `EVALUATE` calls, survival of a
pre-checkpoint definition across that boundary, finish/reset state,
compile-both-branches lookup, and cleanup after an unfinished source. The
admitted exactly-one-full-core profile executes only the false
multicore-banner branch; this is not multicore startup evidence.

The already-run pre-decision bounded regular-load selector applied the CLI
blank/pure-comment filter to its pinned 9,894-line, 341,355-byte file and
submitted 6,693 lines (215,356 payload bytes, or 222,049 CLI UART bytes with
terminators; maximum line length 99) through the captured core checked
evaluator. It used canonical 128 MiB XMEM, 3 MiB HBW, 4 MiB VRAM, and valid
15-sector MP64FS media. Its post-load history consisted of 319 pseudo-BIOS
words plus 1,452 KDOS publications; the authoritative 65,536-slot index held
1,764 unique bindings and seven shadows. That historical checkpoint pinned six
buffers, 23 kernels, three pipelines, nine screens, intact heap/HBW state,
mounted media, zero modules, balanced stacks, and released locks, followed by
a checked post-boot definition, allocation/free, CRC32, ring FIFO, and module
listing. Current source-ledger accounting expects 1,460 KDOS publications and
1,772 unique bindings, but no new full regular-load measurement is claimed;
rerunning it remains deferred by the rich-terminal gate.

The startup filesystem path is literal. `DISK?` false skips `FS-LOAD` and does
not clear a stale true `FS-OK`. A successful load copies the exact lowercase,
unterminated ten bytes `autoexec.f` into `NAMEBUF`, zeroes the remaining 14
bytes, and searches the ambient `CWD`; `_MOD-LOAD-BODY` performs its own second
lookup. Neither lookup adds a file-type, flags, CRC, encryption, or root-CWD
gate, and the `Running` line is printed before module-body validation. The tiny
accepted autoexec proves this startup-to-module seam only. It does not qualify
the repository's standard `autoexec.f`, `networking.f`, or `tools.f` journey.

Source comments at lines 9877–9878 claim that line-by-line evaluation means a
multiline `IF` cannot gate execution. That contradicts both the immediately
preceding multiline startup branch and BIOS's persisted temporary-`IF`
implementation; the unchanged source is recorded rather than silently fixed.
The heap probe checks its allocation status. Failure rethrows the exact code
without passing the returned non-address to `DMA-FREE`; success alone frees
the temporary block. A `HEAP-SETUP` throw also escapes. Startup as a whole is
not transactional. A
module-load failure caught by the guard rewinds that frame's definitions and
provisional IDs, but
filesystem diagnostics and registry/output/object/media effects outside those
transactions can remain, and a throw can skip `JIT-OFF` plus the final newline.
Hosted `JIT-ON` at line 39 and `JIT-OFF` at line 9893 are semantic no-ops, not
evidence of native-code state or performance.

This qualification completes the monotonically contiguous unchanged-source
frontier from executable line 39 through EOF line 9894. It is focused semantic
evidence. A separate moderate selector now also submits the complete pinned
file through the persistent checked pseudo-BIOS evaluator on one fresh
canonical runtime; it is not native/exact-full-core cold-load or timing
evidence. Nothing in this advance loads or implements `rich-terminal.f` or
moves the rich-terminal vertical.

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

The admitted hosted service is pseudo-BIOS-only. It implements `DISK@`, the
three media queries, the six ordinary/generation-bound checked operations, and
`MP64FS-VALID?` against one exact sector image. It does not implement the raw
setup/command words or storage MMIO, BUSY/rejection visibility, RESET, DMA
cadence, controller timeouts, fault injection, or device interrupts. One mutable
service instance may be claimed by exactly one runtime. Host attachment,
detachment, write-protection changes, and checked calls are management
operations that the composition must serialize; this slice makes no
host-thread atomicity claim. Attach, replace, and detach never implicitly
flush the old image.

Checked read/write validates, in order, lock acquisition, presence, required
capabilities, caller generation where supplied, nonzero count, unsigned LBA
range, address overflow, and one complete ordinary physical DMA window. Write
protection is the accepted controller result after that software preflight.
An admitted transfer is synchronous and splits at 255-sector controller
boundaries, publishing exact whole-sector progress and the terminal result of
each accepted chunk. Software rejection before submission preserves the prior
terminal controller tuple. A generation change at the guarded acceptance edge
publishes `MEDIA_REMOVED`, zero transferred sectors, and one completion before
the checked layer returns stale (and marks any earlier confirmed chunks
partial). The public adapter owns depthless filesystem lock
2 exactly as the executable checked BIOS does; callers cannot safely wrap it
in an outer acquisition of the same lock.

`MP64FS-VALID?` reads raw LBA 0, the active bitmap, and the 12-sector
directory into the executable BIOS scratch layout. Each ordinary checked read
owns lock 2 separately; there is no lock spanning validation and no selected
KDOS volume binding. It requires canonical marker-1 geometry, reserved
metadata bits, occupied-entry type/parent rules, allocated extent bounds,
zero extents for directories, and used bytes within combined capacity. It
does not validate names or termination, uniqueness, flags, reserved fields,
timestamps, CRCs, parent acyclicity or root reachability, extent disjointness,
allocation ownership, orphan sectors, bitmap tail bits, or file data.

The final attachment-generation comparison detects replacement but not writes
to the same attached image, so the successful path's three reads are not a
coherent content snapshot. Qualification is one-full-core/core-0 behavior. Executable BIOS
derives scratch from `R2/2`, which is unsafe under secondary-core stack
geometry, and mutates shared `var_mp64fs_*` state, which is non-reentrant. The
hosted stateless predicate is not evidence that those native concurrency
defects are repaired.

A successful write means acceptance, not durability. For path-backed media, a
successful flush writes the complete live image and performs the corresponding
host flush/fsync work. Closing a simulator session is not a substitute for
flush. Pathless media is deliberately ephemeral: flush is an ordering barrier
and successful semantic completion, but is not persistence evidence. Real
file-backed fsync/close/reopen qualification remains deferred until the
rich-terminal vertical acceptance gate permits persistence testing. A faster
host VFS binding may exist for nonconforming development use, but it cannot
stand in for the ordinary storage journey in differential qualification.

The source-defined descriptors remain caller-owned memory. Construction
requires a complete writable, nonoverlapping zero-or-original-live extent;
copying or forging a live descriptor can corrupt reference accounting. Object
cookies and constructor scratch are runtime-global and non-reentrant, and the
validators do not prove that an arbitrary descriptor pointer names a safe
span. Early object-layer failures preserve old block diagnostics, submitted
read/write operations replace ior/completed/LBA/count, and submitted flush
replaces only ior/completed. Read-only rejection intentionally precedes stale,
range, and DMA checks for `BD-WRITE` and `VOL-WRITE`. These literal source
behaviors are compatibility findings, not hosted repairs; the full ABI and
lifetime rules are in
[`block-volume-contract.md`](block-volume-contract.md).

The current hosted service has no clone, sharing, or snapshot format, and a
claimed instance cannot be installed into another runtime. Emulator and
simulator runtime snapshots are separate formats. Portable persistence
evidence consists of copied media bytes and application-level data, not
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

Post-flush media comparison is required future vertical-acceptance evidence,
not a result claimed by the present focused rich-terminal integration. Current
filesystem coverage is limited to semantic ordering, injected flush failure,
and generation-stale no-effect behavior.

Comparisons exclude absolute dictionary addresses, compiled native bytes,
PC/register state, instruction/cycle counters, backend snapshots, and physical
timing.

Every admitted feature receives a focused semantic test and, where an emulator
equivalent exists, a differential vector.  The independent APT byte and state
oracles are the model for this separation: production encoders and decoders do
not define their own expected results. The hosted tile self-test is admitted
against the architectural Python emulator's corresponding public status,
failure mask, and scratch-preservation vector. The integer legacy tile service
also runs decoded `TADD`, `TSUB`, `TSUM`, `TMIN`, and `TMAX` instructions in
the Python architectural emulator for every admitted lane width, comparing the
complete destination, ACC0--ACC3, and TCTRL after each aligned in-bounds step.

## 11. Initial implementation sequence

The simulator slices are intentionally vertical. The original simulator
implementation branch had an explicit pre-rich-terminal stop line:

1. package ownership, dependency guards, cells, source cursor, stacks,
   dictionary, and explicit semantic dispatch;
2. enough compiler and control-flow semantics to source-load unchanged real
   Akashic utility code, including shared return/loop-stack behavior;
3. sparse byte memory, dictionary/runtime backing, staged compiler/evaluator
   semantics, exceptions, and numeric dictionary rollback;
4. advance one unchanged `kdos.f` frontier alongside the one-core semantic
   BIOS surfaces it actually reaches, while filling the remaining KDOS-owned
   evaluator and loader seams instead of replacing source definitions;
5. qualify KDOS-owned checked-evaluator and module-loading surfaces as the
   frontier reaches them, then complete the ordinary `kdos.f` load; and
6. stop before loading or implementing `rich-terminal.f`.

Steps 1 through 5 are now satisfied by contiguous unchanged-source
qualification from executable `kdos.f` line 39 through EOF line 9894. The
completed simulator branch stopped at step 6: no rich-terminal source,
projection, compositor, viewer, or input lifecycle was part of that qualified
KDOS slice. The ordinary semantic file also loaded through the CLI-like checked
line path. Native or exact-full-core cold loading, Akashic integration, and
Desktop execution remain deferred under the rich-terminal resource gate.

The rich-terminal integration now synchronizes the authoritative current
`rich-terminal.f` and appends exactly five source prerequisites to the hosted
pseudo-BIOS: `UM*`, `WITHIN`, `MOVE`, `MS@`, and `TX-FLUSH`. Three public
terminal-geometry words follow them: `COLS`, `ROWS`, `RESIZED?`, `TERMSIZE`,
`RESIZE-DENIED?`, and `RESIZE-REQUEST`. Existing hosted execution tokens retain
their addresses; absolute tokens remain nonportable between backends. Fresh
runtimes therefore publish 330 pre-KDOS words. Focused units qualify widening
multiplication, wrapping interval comparison, overlap/fault copy order,
deterministic latched uptime, immediate hosted flush semantics, session-bound
dimensions, independent clear-on-read resize status, and stale-safe
asynchronous resize requests. Those primitive units alone do not claim a
`rich-terminal.f` source load or APT/session lifecycle; the complete source
and live-snapshot evidence below establishes the larger boundary now reached.

The first shared-source oracle extracts the current contiguous module prefix
from `PT-S-OK` through `_PT-SEND-CREDIT` and executes it on both backends. The
fixture supplies the same one-core SPIN-based definitions for the two
KDOS-owned UART lock words, initializes caller-owned session storage, and
requires the complete 48-byte CREDIT output to equal the independent APT-1
wire encoder byte for byte. It also requires `PT-INIT`, `_PT-SEND-CREDIT`, and
final data-stack depth to report `0 0 0`. This proves that narrow production
encoder/CRC boundary only; it does not move the full-module frontier or claim
KDOS lock, input, session, compositor, or viewer acceptance.

A second oracle over that same exact prefix broadens the shared contract
without skipping intervening source. It rejects undersized RX, TX, and event
storage, misaligned session storage, and overlapping spans; admits valid
caller-owned storage; and checks the read-only disjointness query. With fixed
session fields it requires the source-defined PROBE and OPEN output to match
the independent negotiation codec byte for byte. Finally it invokes public
`PT-START` and requires a well-formed nonzero dynamic nonce, `PROBING` state,
retained stream ownership, and balanced stacks on each backend. The dynamic
nonce is deliberately not compared across backends because it incorporates
the session address and `MS@`. This remains pre-OFFER evidence: it does not
exercise UART input, accept a negotiation offer, or establish a live APT
session.

The next contiguous oracle extends the extracted source only through
`_PT-READ-BYTE`. After public `PT-START`, the host parses the emitted dynamic
nonce, independently encodes a valid OFFER, and injects those exact bytes
through each backend's real UART FIFO. The source-defined `KEY?`/`KEY` reader,
OFFER syntax and validity checks, scanner, buffer compaction, and acceptance
path must consume it completely. Both backends then emit the independent
codec's exact OPEN record and agree on `OPENING`, ownership, session ID,
payload and transaction limits, credits, geometry, text capacity, snapshot
need, probe count, sequence state, and empty binary/legacy buffers. This is a
real host-to-guest negotiation boundary, but it still stops before framed
SERVER_READY handling, CLIENT_READY, ACTIVE state, or a live driver/session.

Host-port integration begins by extracting the already-qualified emulator
attachment policy into the shared state machine described in section 1. The
emulator keeps thin scheduler, UART, geometry, and ingress hooks, so this move
does not alter its execution-batch settlement. The simulator now supplies
parallel hooks plus one stateful semantic-batch owner. It applies admitted
geometry before UART ingress, wakes an owned IDL suspension only when RX is
available, lets each outer guest call contribute at most one settlement
publication, and blocks later semantic execution behind the shared
retained-publication rule. Direct execution and host-side UART mutation through
the owned runtime are rejected. Output completed before backend acquisition
remains a distinct legacy boundary and is drained before attachment, matching
the emulator facade safeguard. The backend holds exclusive runtime ownership
until its enhanced lease is closed and `SimulatorSessionBackend.close()`
releases the backend; callers must perform both lifecycle steps. `TX-FLUSH`
remains the synchronous hosted primitive it was before and does not acquire
protocol framing meaning.

A simulator-only source oracle first isolates the KDOS exception dependency.
It loads the already-qualified exact closure through `CATCH` and `THROW`,
rather than adding those words to the pseudo-BIOS, then evaluates the
authoritative terminal prefix through public `PT-RESOURCE-ABORT`.
Invalid-session calls execute each public resource entry point and return
`PT-S-INVALID`; the two protected entry points also clear their temporary
argument and range state. This proves genuine KDOS exception-word linkage,
normal-return wrapper execution, balanced stacks, and silent failure. The
oracle does not exercise `THROW` recovery, admit a resource, publish a resource
frame, or qualify a successful retained writer.

The live simulator integration now evaluates the complete authoritative
`rich-terminal.f` against that exact exception closure and attaches the
production `RichTerminalDriver`. Ordinary guest calls alternate with driver
service: the source emits PROBE, accepts OFFER and emits OPEN, then accepts
framed SERVER_READY and emits framed CLIENT_READY. Both endpoints reach
`ACTIVE` with negotiated limits, geometry, credit, and empty queues. The
unchanged source then emits a complete five-frame, 312-byte 2x2 snapshot via
`PT-SNAPSHOT-BEGIN`, two `PT-SPAN-BEGIN`/CELL payloads, `PT-CURSOR`, and
`PT-TX-COMMIT`. The host publishes one immutable revision-1 CELL view with the
exact A/B/C/space cells and cursor. Its 108-byte TX_RESULT plus CREDIT response
clears the guest snapshot requirement, moves the client/server directional
sequences to six and three, and leaves the transport empty. A subsequent
56-byte guest CLOSE and 48-byte CLOSE_ACK return both endpoints to ANSI with
source ownership released. The complete journey accounts for 551 machine
publication bytes and 440 framed bytes, with balanced stacks and no failure.

This is complete terminal-module source evaluation and a real renderer-neutral
view, but not yet complete rich-terminal vertical acceptance. The test compiles
the intervening PRESENT and retained writer definitions without invoking them;
it does not project Akashic content, compose the resulting view, bind input to
a physically acknowledged revision, or exercise a display sink.

KDOS qualification maintains one monotonically advancing source frontier.
Later isolated slices may validate a cross-cutting prerequisite such as real
exception unwinding, but they do not move that frontier and are not a
substitute for filling the intervening source. Slice width is determined by
the next genuine unsupported capability rather than a fixed line count. As
the BIOS closure grows, each successful increment should span more ordinary
definitions and the remaining islands should be absorbed into one complete
`kdos.f` load.

The ordinary Desk/Pad/Daybook journey remains part of the compatibility
contract. The source frontier is no longer the blocker: the complete terminal
module and its genuine KDOS exception dependency have crossed the simulator
without terminal-specific semantic substitutes. The next boundary is
composition and lifecycle integration—sharing the existing presentation,
physical acknowledgement, and input authority while keeping the simulator's
run-to-IDL semantic scheduler distinct from emulator instruction batching.

`SimulatorMachineSession` in `simulator/session.py` now establishes that
scheduler/session composition.
It reuses the existing `MachineSession` terminal frontend through explicit
attachment, host-state, legacy-input, and legacy-geometry hooks rather than
impersonating `MegapadSystem`. One owner boundary services the shared driver,
runs or resumes the root semantic dispatch, then services the driver and
display cadence again. Completion and `IDL`, semantic steps, external-event
admission, host backpressure, and terminal failure retain their own names and
cannot be reported as instruction or cycle statistics. Focused evidence drives
the complete module to the same revision-1 CELL snapshot through this session
and leaves its root continuation quiescent at `IDL`.

`SimulatorSharedMachine` carries that session through the existing
backend-neutral `SharedMachine` presentation and input methods and unchanged
`SessionServer` dispatch. Its owner thread schedules semantic boundaries rather
than emulator batches; status names semantic steps, semantic boundaries, and
external-event admissions explicitly and contains no fabricated cycles, CPU,
clock, or NIC state. Focused dispatch evidence reaches the same revision-1 CELL
snapshot, applies generation rejection, admits terminal input, and delivers it
to the suspended guest. Emulator-only host profiling and hardware diagnostic
routes still fail explicitly. This does not yet establish a retained physical
display offer, Akashic image/root preparation, or the socket/viewer journey.

Only seconds-scale structural, focused unit, and the bounded moderate semantic
KDOS load run before the real rich vertical exists. Native/exact-full-core cold
load, Desktop smoke, sustained cadence, persistence, full renderer, and
physical-viewer qualification remain deferred to vertical acceptance under
the project's resource rules.

## 12. Performance evaluation boundary

Performance evidence keeps backend-normal readiness separate from matched
software work. `bench_bios_kdos_load.py` measures the emulator after
architectural reset, before its first BIOS instruction, through normal MP64FS
autoboot and the marker-only autoexec. `bench_simulator_kdos_load.py` measures a
prepared semantic BIOS immediately before its first checked packed-source line
through the same unchanged KDOS and marker-only autoexec outcome. The latter
intentionally omits ROM instruction execution and transfer of `kdos.f` into a
BIOS load buffer: semantic substitution is the product behavior being timed.
Consequently, their ratio is a backend-normal time-to-KDOS-ready comparison,
not equal-work compiler timing or an execution-engine speedup.

`bench_compare_kdos.py` admits that ratio only from clean, fresh processes with
the same source, deterministic full MP64FS image, canonical one-core geometry,
host, Python, commit, and position-balanced serial order. It also pins the
emulator harness and ignored native accelerator by content hash. Emulator
instructions/cycles and simulator semantic dispatch steps remain backend-local
diagnostics and are never divided. The coordinator's outer process envelope
includes unequal import, construction, validation, and JSON work and is
diagnostic only.

Qualified wall timing additionally requires an otherwise idle host and an
unprofiled run on one pinned CPU. The explicitly authorized 2026-09-01 harness
shakedown occurred while unrelated work kept the 16 logical CPUs about 87%
busy and left the selected CPU about 3% idle. Its child state and provenance
all passed, but its wall ratios are rejected as performance evidence and are
not a simulator speed claim. No qualified emulator/simulator speed result is
recorded yet. The one authorized shakedown does not lift the preceding general
rich-vertical resource gate for further cold-load qualification.
