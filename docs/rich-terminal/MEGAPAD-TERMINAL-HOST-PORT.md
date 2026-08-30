# MegaPad terminal host-port contract

Status: normative for the APT-1 CELL-1 and selected RETAINED-1
emulator/reference-host implementation. Physical UART delivery and a hardware
panel sink remain separate open qualification boundaries.

This document defines the boundary between `MegapadSystem` and a terminal
session. It does not define the APT-1 wire encoding. The wire contract is
`APT-1-WIRE.md` in this directory.

## 1. Required properties

The host port is the only production path while an enhanced frontend is
explicitly attached. It is additive to the existing UART/ANSI frontends; an
ordinary MegaPad session does not acquire this port and retains its current
behavior. While attached, the port provides:

* one exclusive primary consumer;
* ordered, lossless delivery within caller-provided capacity;
* bounded storage in both directions;
* non-throwing machine-side publication;
* deterministic host-to-machine scheduling; and
* epoch-qualified attach, reset, detach, and geometry changes.

The machine execution path may copy or transfer bytes into the port. It may
not parse terminal bytes, mutate a terminal model, render, synthesize replies,
or invoke terminal-owned code.

The guest-side protocol implementation is likewise not part of KDOS. It is a
separately source-loadable userland module, `rich-terminal.f`, in the
same architectural role as `networking.f`. BIOS and KDOS continue to expose
ordinary UART and geometry primitives whether or not that module is present.

### 1.1 Intended physical endpoint

The canonical intended physical rich-terminal endpoint is an e-paper terminal,
with touch and full-color panels as eventual product capabilities. This is a
product target, not an APT wire restriction: the protocol and immutable view
boundary remain hardware-neutral, and the ANSI/CELL fallback remains complete
for other terminals.

The retained model, latest-view coalescing, and exact offer/acknowledgement
split let a slow-refresh endpoint continue protocol service while a panel is
busy and then display the newest eligible complete composite. The selected
sink owns damage derivation, full-versus-partial refresh choice, panel waveform
and ghosting/full-refresh policy, color conversion and dithering, rasterization,
and all panel-controller buffers. None of those choices belongs in UIDL,
application state, or the APT wire contract.

This is intentionally stronger than a conventional terminal driver's
"accepted bytes" boundary. Attachment epochs prove transport ownership;
bounded queue admission proves complete storage; APT transactions prove a
logical revision; and the selected sink's offer acknowledgement proves display
completion at that sink's documented boundary before revision-bound input is
authorized. For pygame that boundary is only host display-API submission; only
a physical sink's post-controller, post-settle acknowledgement proves panel
completion. Those records should be deduplicated when they repeat the same
assertion, but they cannot be collapsed into one flag because each closes a
different failure boundary. The extra chain is what makes reconnect/replay,
slow-panel latest-view coalescing, and exact-revision touch safe rather than
merely best-effort.

## 2. Attachment and epochs

There is at most one enhanced primary attachment. Attaching returns an opaque token
containing a monotonically increasing 64-bit attachment epoch. A token is
valid only for the exact attachment that created it.

With no enhanced attachment, legacy callbacks and frontends retain ownership.
Acquisition fails if a legacy parser/consumer is active and cannot be paused
coherently. A successful acquisition suspends legacy consumption only for the
life of that lease; release restores it without discarding the ANSI terminal
implementation.

Attach and detach are applied while the system scheduler lock is held and no
guest batch is active. Detach is idempotent for the current token. Operations
using stale tokens return `STALE`; they do not mutate queues or the UART.

Machine reset, detach, or replacement of the primary attachment:

1. advances the attachment epoch;
2. invalidates outstanding handles and view revisions;
3. drops parser/model state owned by the old terminal session;
4. cancels old epoch-tagged, not-yet-applied terminal ingress and geometry;
5. clears old UART RX bytes originating from that attachment; and
6. returns stream ownership to ANSI.

Bytes already consumed by the guest cannot be recalled. Reset ordering must
therefore invalidate the epoch before guest execution resumes.

## 3. Capacity configuration

The attaching caller supplies positive limits for:

* accepted egress bytes and batches;
* one retained machine publication;
* pending ingress bytes and events; and
* pending geometry events.

These are configuration values, not protocol constants. Attachment fails
before acquiring ownership if the values cannot admit the negotiated CELL-1
limits. Implementations must use checked arithmetic when combining them.

No primary-path archive is enabled by default. Diagnostic observers may
receive bounded copies after primary acceptance; they may not delay, reject,
or parse the primary stream.

## 4. Machine egress

Each machine publication is an immutable record:

```
EgressBatch(attachment_epoch, publication_sequence, payload)
```

`publication_sequence` starts at zero for an attachment and increments once
per non-empty publication. UART ring-flush boundaries and execution-batch
boundaries have no APT framing meaning.

Publication uses accept-or-retain semantics:

* `ACCEPTED`: the port owns an immutable copy and the machine adapter may
  release its source bytes.
* `BACKPRESSURED`: ownership does not transfer. The adapter retains the exact
  publication and must not execute another guest batch.
* `STALE`: the attachment changed. The old publication is discarded as part
  of epoch retirement and must not be delivered to the new attachment.

The native UART drain may destructively move one completed execution batch
into a single adapter-owned retained slot. This is the sole permitted
one-publication overshoot. If the primary queue cannot accept that record, the
slot retains it byte-for-byte and the runner is backpressured before another
guest batch starts. The slot is cleared only after acceptance or epoch
retirement.

A valid in-contract publication must not raise into scheduler settlement.
Consumer failures are recorded as terminal-session failures and processed
after the machine boundary.

### 4.1 Physical UART status and evidence boundary

Software-to-RTL UART TX path is **IMPLEMENTED; BOARD EVIDENCE OPEN**. BIOS
reads `UART_CAPS` at `+07` once during boot. Every BIOS byte writer -- including
`EMIT`, `TYPE`, `CR`/LF, boot strings, and diagnostics -- uses one
capability-aware routine. Python and the native accelerator advertise bit 0,
`TX_RING_BATCH`, so that routine retains the existing RAM-ring path and
`TX-FLUSH` at `+06` still publishes one completed host batch. This keeps the
enhanced host port's batching, admission, and ownership behavior unchanged.

Integrated `mp64_uart` RTL reports zero capabilities. The same BIOS therefore
polls STATUS bit 0 (`TX_READY`) and writes each ordinary ANSI or APT byte to
`TX_DATA` at `+00`; direct-mode `TX-FLUSH` waits for STATUS bit 5. RTL bit 5 now
means FIFO empty and shifter inactive, including completion of the final stop
bit. The SoC instance remains fixed to real 8N1 serialization at 115,200 baud.
The FIFO also handles a same-cycle bus push and shifter pop without losing or
duplicating its count.

This closes the source/BIOS/RTL design seam, but does not prove attached-board
transmission. The current board flow has no accepted deployable BIOS image,
bitstream, or captured pin waveform, and the Genesys-2 measurement target does
not fit the default internal-memory contract. Emulator/native viewer success
still proves neither real baud timing, board-level backpressure, USB-UART
wiring, nor a panel controller.

The later reference journey at Akashic `d24540e` with MegaPad `c7045d6`
closes the selected Desk/Pad/Daybook software/viewer checkpoint, including
Daybook navigation and the ordinary shared-source handoff into Pad. It finished
at 783,434 decoded bytes after twelve acknowledged offers, with every scripted
input authorized only after the exact complete post-`pygame.display.flip()`
offer acknowledgement. Those bytes still crossed the in-process enhanced host
port, not an attached RTL TX pin, so the run does not close board evidence or
qualify an e-paper panel.

The recorded `eedcfb9`/`4f074ae` reference journey makes the consequence
quantifiable, although it is not physical-link evidence. At 8N1, 115,200 baud
has a nominal payload ceiling of 11,520 bytes/s. Its 744,222-byte first offer
would therefore require at least 64.60 seconds of uninterrupted serialization,
and the 24,541 post-first bytes would require another 2.13 seconds. Individual
post-first updates span 0--6,924 decoded bytes, or 0--601 ms of line time. At
the optional 1,000,000-baud profile those same no-gap lower bounds are 7.442
seconds, 245 ms, and 0--69 ms respectively.

Those figures are arithmetic lower bounds from decoded guest-to-terminal wire
bytes, not measurements. They omit terminal replies, scheduling gaps,
backpressure, composition, and panel refresh, and the physical link remains
unmeasured. They do show the likely division of labor: after first load, a
100 MHz implementation at even a four-clock floor projects the 69.0--123.0M
post-first instruction intervals to at least 2.76--4.92 seconds before longer
operations or stalls, so guest execution would still dominate their current
0--601 ms serialization floors. A GHz-class 1--2-average-CPI CPU instead
reduces that guest work to tens or low hundreds of milliseconds, at which
point a large update can become UART-bound and an e-paper endpoint can
independently remain refresh-bound. Retained deltas, coalescing, faster
negotiated transport, and the exact post-refresh acknowledgement rule address
different terms of that latency budget.

The 115,200 software/RTL path is implemented before any faster-rate work. The
architectural BIOS path polls real `TX_READY` and writes `TX_DATA`; the optional
capability keeps guest ring batching confined to backends that implement it.
No RTL DMA reader or ring fault protocol is implied. Attached hardware must
still prove ordinary ANSI and APT bytes on the physical pin at 115,200 baud.

Only after that baseline is measured may MegaPad add the optional
1,000,000-baud profile. At the current 100 MHz SoC clock its integer divisor is
exactly 100;
115,200 uses divisor 868 (approximately 115,207 baud). The existing two stored
baud bytes cannot encode either 115,200 or 1,000,000 as a numeric rate and are
not a suitable control. A two-value rate-profile selector is the smaller
interface; an arbitrary divisor design instead needs shadow registers plus one
atomic apply operation. The implemented status boundary already means physical
line idle (FIFO empty and shifter idle).

Reset, ANSI, probe/offer/accept, and the future rate-switch exchange start at
115,200. The faster profile is explicitly advertised and accepted, then
applied by both endpoints only at an acknowledged physical-line-idle boundary.
Close finishes at the active rate and returns to 115,200 at the corresponding
idle boundary; hard attachment/link reset also restores 115,200. The exact
current APT-1 negotiation grammar has no rate field, so a silent divisor write
or an emulator-only setting is not a conforming dual-rate implementation.

### 4.2 Bounded guest-phase observation

The shared-session host exposes an opt-in diagnostic observer for profiling
already-running guest code. `start_phase_profile` accepts the current machine
generation, one complete eight-byte cell wholly contained in mapped RAM or
external memory, and a positive caller-selected record bound. The address need
not be naturally aligned: MegaPad Forth `VARIABLE` data fields follow their
variable-length dictionary headers, and the architecture's 64-bit memory
operations support such addresses. MegaPad does not know an Akashic word name
or phase vocabulary; the profiling client resolves the guest variable and
interprets its packed value. Start is rejected before machine startup, during
or after shutdown, across a generation change, or when any byte of the cell
falls outside one admitted memory region.

The observed 64-bit cell uses a client-defined low-byte phase and high-56-bit
sequence. While enabled, the shared owner reads it immediately after each
retired-instruction batch under the same scheduler lock. A recorded change is
therefore bounded by the exact cumulative instruction counts before and after
that batch. Tear-free host sampling follows from the synchronous guest batch
having quiesced while that lock remains held; it is not a claim that concurrent
guest cores or a physical debug master observe an unaligned 64-bit store
atomically. A sequence jump reports how many intervening transitions were
coalesced; it does not invent their phases or divide the interval among them.
The observer retains the first caller-bounded records, counts later dropped
records and transitions, and has a host safety ceiling of 65,536 records.

This is not a cycle counter, an `INSTRET` CSR, or a guest ABI. It does not alter
deterministic virtual timing. Disabled sessions perform no guest-memory reads;
an enabled observer adds one read per nonempty execution batch. An invalid
event or read failure freezes only the diagnostic record and never pauses or
fails the guest. Reset and shutdown discard it. `phase_profile` returns a
bounded copy without another guest read, and `stop_phase_profile` atomically
freezes, returns, and removes the observer. Performance conclusions must retain
the raw intervals, generation, coalescing, overflow, and error fields rather
than presenting batch-bounded attribution as exact per-word instruction
accounting.

The first complete consumer of this diagnostic passed the ordinary twelve-offer
Desk/Pad/Daybook journey at Akashic `c5f2271` with MegaPad `6a9f10e`. Its
authoritative half-open window covered 933.0M retired guest instructions after
a separately recorded 3.0M-instruction attachment lag. It completed with no
observer error, dropped record, end straddle, or open phase, but 246 of 374
sequence transitions were coalesced at the 500,000-instruction batch boundary.
The result therefore supports bounded optimization decisions, not exact
per-phase accounting. Akashic's durable raw-evidence ledger is
`local_testing/evidence/rich-desktop-phase-profile-20260830.md`.

That profile found one newer logical revision whose complete pixels,
retained-only pixels, retained text, and draw count exactly matched its
acknowledged predecessor. It nevertheless spent 38.5--40.5M observed
instructions in hybrid preflight plus delta comparison. This justifies a
renderer-neutral certified-unchanged optimization in Akashic while preserving
the real revision fence, transaction, offer, physical composition, and exact
acknowledgement. It does not justify a MegaPad opcode, cached executable, cycle
claim, or emulator timing shortcut.

The successful observer run still used the in-process enhanced host port and
`pygame.display.flip()`. It did not traverse an attached RTL UART or e-paper
panel, so board evidence remains open and no transport-rate or panel-cadence
decision follows from it.

## 5. Terminal consumption

Terminal code polls accepted batches outside scheduler settlement. Polling
preserves publication order and transfers ownership to the caller. Releasing
a polled batch restores exactly its payload byte count and one batch slot.

The terminal parser may retain an incomplete APT frame across publications.
It must not retain references into mutable UART or adapter storage.

Terminal parsing, queued reply generation, output commits, snapshot
publication, and rendering all occur outside the machine boundary.

## 6. Terminal ingress

Terminal replies and normalized user input are immutable, epoch-tagged ingress
records. Admission is all-or-nothing:

* `ACCEPTED` reserves the complete payload;
* `BACKPRESSURED` leaves ownership with the terminal; and
* `STALE` rejects an old attachment.

Accepted bytes enter the existing external-event journal through
`schedule_uart_input`. They are applied only at a legal scheduler boundary.
No parser or renderer calls `UART.inject_input` directly.

Ingress capacity includes a reserved control allowance sufficient for one
APT credit, reset, close, or fatal-error response. Ordinary key, text, and
pointer events cannot consume this reserve.

An APT `RESIZE` is admitted through one composite operation:

```
ResizeRecord(attachment_epoch, schedule_sequence, payload, cols, rows)
```

It atomically reserves one ordinary ingress byte/event charge and one
geometry-event charge. If either allowance is unavailable, admission changes
no counter, queue, or sequence. A retained composite is retried byte-for-byte;
the wire frame is never re-encoded merely because the host port is full.
Before encoding that frame, the driver observes an empty egress poll and
confirms through the lease that neither accepted nor adapter-retained machine
egress remains. This prevents an unseen retained `TX_BEGIN` from being crossed
by asynchronous UI geometry. It also preflights the current ordinary-ingress
and geometry allowances before encoding. If either is full, the latest-wins
geometry intent remains unencoded while the runner drains older input; the
core does not enter resynchronization early.

## 7. Geometry

Initial geometry is attached before guest boot. While APT-1 is active, the
terminal session is authoritative. An accepted protocol resize is scheduled
through the existing generation-qualified geometry journal and mirrored to
legacy UART geometry as one coherent event.

At the scheduler boundary, the composite geometry is scheduled before its
framed `RESIZE` payload and no guest instruction may execute between them.
The fake host likewise transfers and releases both reservations as one
`ResizeRecord`; it cannot expose a half-resize boundary.

While ANSI owns the stream, the existing host/MMIO geometry path remains
authoritative. An enhanced-session geometry event and a legacy resize flag
must never be dispatched as two application events.

## 8. Runner admission

Before starting any guest execution batch, the runner services, in order:

1. epoch transition work;
2. an adapter-retained egress publication;
3. admitted terminal ingress and geometry; and
4. primary-queue low-water admission.

If item 2 remains backpressured, the runner returns a host-backpressure stop
reason without executing guest instructions. Queue space becoming available
wakes or permits the next call; it does not execute the machine from a
consumer callback.

## 9. View publication

The headless terminal core publishes immutable renderer-neutral views only
after an ANSI-visible change or an accepted enhanced transaction commit. ANSI
and CELL-only views carry attachment epoch, terminal session ID, model revision,
geometry, persistent rows, dirty spans, and cursor state. A retained-capable
session publishes one immutable `CompositeTerminalView` containing the global
revision and geometry plus independently shareable CELL and retained planes.

The production display boundary consumes that composite as one atomic display
offer; it must not reduce the offer to a CELL-only `TerminalSnapshot` or expose
the authoritative retained scene model to the renderer. The session may freeze
the CELL plane into the same immutable snapshot-shaped value used by the
compatibility path, but only beside the projected retained draw plane and the
exact composite scope. The source composite remains privately bound to that
offer until settlement.

Cadence may select a newer immutable composite without making it displayed.
Selection creates a monotone offer identifier and does not change the session
revision, visible output, cadence timestamp, geometry exposed through the
selected view, or revision-bound input eligibility. Only an exact offer-ID and
scope acknowledgement after complete composition and the selected sink's
documented completion boundary promotes the privately bound composite and
advances those boundaries. Sink loss revokes the exact offer; cadence then
re-offers it or a newer coalesced candidate without reusing an offer identifier.

That acknowledgement is a local view-sink attestation, not an APT wire frame.
The current Pygame reference sink draws the complete offer, calls
`pygame.display.flip()`, and only then invokes `present`. This is an exact host
display-API submission boundary; it proves neither scanout completion nor
panel refresh. Evidence ending there must say post-flip or host-API
acknowledgement rather than hardware-panel acknowledgement.

A physical e-paper sink must retain the exact offer, its immutable backing,
scope, and rendered hit map throughout refresh. It may invoke `present` only
after the panel controller reports that the exact refresh has completed through
its BUSY/READY transition or equivalent completion signal and any
panel-required settling interval has elapsed. Composition, a panel-buffer
write, SPI/DMA completion, or refresh-command acceptance alone is not this
boundary. Touch sampled before completion may be retained only as bounded raw
intent; normalized semantic input remains bound to the exact acknowledged
display revision.

The physical-UART gate in Section 4.1 and this hardware-panel gate are
independent. Bytes on the real UART pin do not establish panel completion, and
a reference or hardware sink acknowledgement does not establish that the guest
bytes reached it through the real UART.

The compositor uses the CELL canvas as its complete fallback base, draws every
selected rich region and glyph run in deterministic back-to-front order
with straight-alpha source-over blending, and draws the cursor overlay last.
The current implementation carries foreground, background, CELL attributes,
UTF-8 scalars, and exact bounds in that generic run. This proves the physical
raster and acknowledgement seam and keeps a complete styled-terminal fallback.
It does not complete the rich vertical, even when the ordinary TUI screen
transaction supplies the full Desk/Pad/Daybook GLYPH_RUN plane.

The implemented view slice also carries ordinary semantic menu controls from
the same UIDL/TUI lifecycle without letting applets author protocol scenes. The
selected renderer renders and hit-tests those controls, and normalized input
may activate one only against the exact selected revision after the complete
composite has crossed the sink-specific acknowledgement boundary. A glyph
imitation of a menu or button remains fallback output rather than semantic
control evidence.

The additive `RET_CONTROL_COLLECTIONS` model now also crosses the generic view
boundary as immutable `TextAreaDraw`, `TextGridDraw`, and `TabSetDraw` values.
That projection preserves exact owner/region authority, independent sibling
roots, stable item identities, logical viewport and selection state, and
deterministic draw order without choosing pixels or hit geometry. The shared
physical viewer wire now carries those exact draw values, using canonical STX1
bytes instead of defining another semantic-item schema. The Pygame sink now
paints text-area, grid, and tabset roots over the mandatory complete CELL base,
leaves the cursor overlay last, and derives immutable enabled-TAB hit geometry
from that exact paint pass. An accepted physical offer therefore authorizes TAB
ACTIVATE through the existing exact-revision path. Text/grid item input remains
deferred because the current event cannot name content revision, item key, and
scalar offset. Bit 9 remains unadvertised until the synchronized Akashic
producer and ordinary Desk/Pad/Daybook journey exercise this physical path.

Collection rasterization consumes the immutable values already validated at
wire/model admission; it does not repeat UTF-8, ordering, family, or rectangle
proofs. It maps logical geometry with exact integer edges, allocates no logical
rows-by-columns matrix, intersects extreme logical grid edges before creating
SDL-backed rectangles, and gives each emitted grid/tab glyph its own
single-scalar surface rather than creating an unbounded whole-string surface.
This bounds glyph allocations and render calls, not traversal of a long clipped
proportional-font prefix. No Pad or Daybook geometry enters the renderer. A
future e-paper sink derives damage from the final CELL-plus-rich-plus-cursor
raster and retains full/partial refresh, waveform, ghosting, controller
completion, and settling as sink-local policy.

A region's pixel rectangle is exactly its cell rectangle multiplied by the
selected cell width and height. For a parentless object's normalized edge, the
low edge rounds down and the high edge rounds up against that region's pixel
extent, then clips to the region when clipping is enabled. The terminal font is
authoritative. Each GLYPH_RUN fills its background and assigns one equal slot
to each scalar, clips glyph overhang and decorations to that slot, and applies
their alpha by source-over composition. Bold, dim, italic, underline, reverse,
and strike are exact; blink is rejected because this draw value has no
presentation-phase cadence. The ordinary TUI projection resolves existing
clips, lines, boxes, selection, and caret writes into these runs so substantive
UI pixels are rasterized here rather than supplied only by CELL.

Cursor blink and other renderer-only overlays do not create cell revisions.
Unchanged rows may be shared by identity across revisions. A renderer cannot
obtain mutable access to parser or model state.

## 10. Prohibited shortcuts

The following do not conform:

* a second raw-console or ANSI parser consuming bytes during an enhanced
  lease;
* unbounded `tx_buffer`, `_tx_log`, or `raw_output` retention on the primary
  path;
* invoking a parser or terminal callback from UART drain settlement;
* injecting DSR, geometry, key, or protocol replies synchronously;
* dropping, truncating, or splitting a declined machine publication; or
* a fake host that omits epochs, capacity, retained publication, or reset
  behavior.

Removing ANSI support, requiring `rich-terminal.f` during KDOS boot,
or automatically acquiring an enhanced lease for ordinary sessions also does
not conform.

## 11. Initial conformance cases

The lightweight host-port suite must prove:

1. two publications remain ordered;
2. a full queue retains the exact next publication and prevents another
   runner batch;
3. consumption admits that retained publication without duplication;
4. terminal code is not invoked during settlement;
5. ingress is applied only at a later scheduler boundary;
6. stale handles cannot publish after reset or detach; and
7. geometry has one epoch-qualified application;
8. protocol resize either reserves both ingress and geometry or neither;
9. a nonempty retained plane survives cadence and view publication without
   being reduced to the CELL compatibility snapshot; and
10. deterministic off-screen fixtures prove that the current rich draw plane
    changes pixels after CELL base rendering and before the cursor overlay; the
    generic renderer fixture additionally covers text, background/fill,
    clipping, styling, and z-order once that bounded vocabulary is present.

Cases 9 and 10 are focused seconds-scale units for the current functional
slice, not full renderer qualification. A CELL-only snapshot round trip, one
isolated glyph-run overlay, or even a complete GLYPH_RUN-only screen cannot be
cited as Desk/Pad/Daybook semantic-rich-rendering acceptance. That acceptance
also requires a real semantic control, its physical revision acknowledgement,
and revision-bound normalized activation through the ordinary application
lifecycle.
