# `rich-terminal.f` module boundary

Status: normative for the optional APT-1 guest implementation.

## 1. Placement

`rich-terminal.f` is a source-loadable MegaPad userland module. Its
role matches `networking.f`: KDOS supplies bounded hardware primitives and the
module supplies the higher-level protocol only when a caller loads it.

The provider identifier is `rich-terminal.f` (15 bytes). A normal
boot, ANSI application, or Akashic session does not require or automatically
load it. Autoexec policy is outside the module and must remain opt-in for the
first milestone.

No APT parser, cell transaction model, negotiation state, retained discovery
state, or enhanced input decoder is compiled into KDOS. The module requires no
new BIOS or MMIO mapping; it uses the existing UART and terminal-geometry
primitives. If future physical flow control requires hardware support, BIOS
exposes only that bounded primitive and the protocol policy remains here.

## 2. Supported absence

The following are ordinary supported configurations:

* the module is not present on disk;
* the module is present but not loaded;
* it is loaded but no caller requests an enhanced session;
* a caller requests a session and the terminal ignores or refuses APT-1; and
* an active session is synchronously closed, or an externally reset and
  drained lost attachment is replaced, and operation continues through ANSI.

None may prevent KDOS, the ANSI terminal, or Akashic's cell UI from working.

## 3. Ownership

Before negotiation, UART input belongs to the existing ANSI/key path. A caller
explicitly acquires a rich-terminal session and supplies bounded storage and
timeouts. The module interposes on raw terminal input from `PT-START` until a
proven ANSI-safe close/reset boundary, including while resynchronizing,
closing, or quarantined in `LOST`.

From successful `PT-START` through `OPEN`, `ACTIVE`, resynchronization, close,
or loss, the module exclusively owns framed rich-terminal bytes and returns
normalized input events through its API. It releases ownership on a
pre-`OPEN` refusal/timeout, a valid `CLOSE_ACK`, or an external attachment
reset that advances the link epoch and drains both directions. A post-`OPEN`
structural failure enters `PT-ST-LOST` and retains ownership; it is not an
ANSI fallback boundary. Buffered enhanced bytes are never passed into the
ANSI key decoder.

Probe failure restores the prior ANSI owner and forces a cell redraw if any
probe bytes could have affected physical display output. Ordinary key bytes
received before the enhanced switch boundary retain their original order.

## 4. Public responsibility

The module owns:

* the APT-1 ANSI-safe negotiation state machine;
* framed encoding and incremental decoding;
* session ID, sequence, wire `presentation_epoch`, and credit accounting;
* one non-nested outgoing transaction;
* replace-all snapshot transmission;
* normalized key, text, pointer, focus, and resize event decoding;
* explicitly requested RETAINED-1 discovery, exact CAPS/FORMATS validation,
  and lifecycle-bounded access to the accepted records;
* the shared CELL/PRESENT transaction-ID, revision, sequence, byte, credit, and
  completion gate;
* core OWNER_OPEN/OWNER_DROP lifecycle publication and exact RET_RESULT
  reconciliation; and
* PRESENT construction for CELL_NONE/DELTA/REPLACE plus fixed retained region
  DEFINE/REPLACE/DROP operations;
* close, hard failure, soft cache reset, and fallback.

It does not own consumer focus, host regions, widgets, retained semantic
objects, owner/item allocation policy, quota derivation, replay planning, or
the Akashic front/back cell buffers. The core retained writers accept bounded
wire-neutral intent from a single internal rich-terminal engine; they are not
an independently discoverable scene or mutation API. Object, resource, and
series families remain outside the currently implemented writer subset.

## 5. Caller-owned capacity

Initialization receives caller-owned frame scratch storage, incoming parser
storage, transaction limits, and event storage. The module validates these
against the terminal's negotiated limits before opening. It does not silently
substitute a smaller fixed screen, truncate transactions, or allocate an
unbounded transcript.

The module defines these status values, shared with the Akashic adapter:

| Value | Name | Meaning |
| ---: | --- | --- |
| 0 | `PT-S-OK` | Operation accepted. |
| 1 | `PT-S-WOULD-BLOCK` | Capacity/credit unavailable; no progress. |
| 2 | `PT-S-SESSION-LOST` | The enhanced session is no longer usable. |
| 3 | `PT-S-INVALID` | Invalid caller arguments or call order. |
| 4 | `PT-S-UNSUPPORTED` | Negotiation was refused, ignored, or timed out. |

The first implementation preserves these public stack contracts:

```forth
PT-SESSION-SIZE     ( -- bytes )
PT-EVENT-SIZE       ( -- bytes )
PT-COMPLETION-SIZE  ( -- bytes )
PT-INIT             ( rx-a rx-u tx-a tx-u event-a event-u session -- status )
PT-STORAGE-DISJOINT? ( a u session -- flag )
PT-START            ( session -- status )
PT-SERVICE          ( session -- status )
PT-STATE@           ( session -- state )
PT-ACTIVE?          ( session -- flag )
PT-SNAPSHOT-NEEDED? ( session -- flag )
PT-STREAM-OWNED?    ( -- flag )
PT-OWNS?            ( session -- flag )
PT-LEGACY-PENDING?  ( session -- flag )

PT-RETAINED-DISCOVER   ( session -- status )
PT-RETAINED-STATE@     ( session -- state )
PT-RETAINED-AVAILABLE? ( session -- flag )
PT-RETAINED-CAPS@      ( session -- a u )
PT-RETAINED-FORMATS@   ( session -- a u )

PT-COMPLETION-POLL  ( completion session -- status has-completion )

PT-OWNER-OPEN       ( owner generation region-q resource-q object-q series-q
                      resource-byte-q utf8-byte-q sample-slot-q session
                      -- status )
PT-OWNER-DROP       ( owner generation session -- status )

PT-PRESENT-BEGIN    ( cols rows cell-spans cells retained-ops
                      retained-frame-bytes cell-mode retained-mode session
                      -- status )
PT-PRESENT-OP       ( type payload-a payload-u session -- status )
PT-REGION-DEFINE    ( owner generation region x y cols rows z flags session
                      -- status )
PT-REGION-REPLACE   ( owner generation region x y cols rows z flags session
                      -- status )
PT-REGION-DROP      ( owner generation region session -- status )
PT-PRESENT-COMMIT   ( disposition session -- status )

PT-TX-BEGIN         ( cols rows span-count cell-count session -- status )
PT-SNAPSHOT-BEGIN   ( cols rows span-count cell-count session -- status )
PT-SPAN-BEGIN       ( row col count session -- status )
PT-CELL             ( cp fg bg attrs session -- status )
PT-CURSOR           ( row col visible session -- status )
PT-TX-COMMIT        ( session -- status )
PT-TX-ABORT         ( reason session -- status )

PT-EVENT-POLL       ( event session -- status has-event )
PT-LEGACY-POLL      ( session -- byte has-byte )
PT-CLOSE            ( reason session -- status )
```

`PT-STORAGE-DISJOINT?` accepts only a nonempty, nonwrapping candidate span and
a valid initialized session. It returns true only when the candidate is
disjoint from the session record and the session's complete borrowed RX, TX,
and event spans. Composed adapters must use this predicate before clearing or
retaining any additional caller-owned storage; they do not learn the private
borrowed addresses.

`PT-START` is nonblocking and is the only call that initiates negotiation.
`PT-SERVICE` incrementally advances negotiation, framed input, timeouts,
credit, reset, and close without waiting for another byte. `PT-LEGACY-POLL`
returns ordinary bytes held while a probe was being distinguished from ANSI;
it never returns enhanced binary.

RETAINED-1 discovery is not an automatic consequence of opening a CELL-1
session. `PT-RETAINED-DISCOVER` is the caller's explicit opt-in; the call
records intent but emits no bytes. It may be made before `PT-START` or on a
live session and is idempotent within the caller-owned session. The opt-in
survives soft reset and synchronized close/reopen, while `PT-INIT` clears it.
A caller that never invokes it remains a CELL-only client and never sends
`RET_QUERY`.

After opt-in, `PT-SERVICE` waits for the successful initial CELL snapshot
result, an empty transaction slot, no outstanding result, and the exact
directional credit preconditions before sending the epoch's one query. It
classifies the covering-CREDIT-only response as `PT-RET-ST-CELL-ONLY`, and
publishes `PT-RET-ST-AVAILABLE` only after validating one adjacent exact
CAPS/FORMATS pair and then receiving the covering CREDIT. The raw record
accessors return `0 0` unless that public state is currently AVAILABLE; close,
loss, ANSI state, and reset therefore cannot expose stale records.

The public retained states are `PT-RET-ST-PENDING`,
`PT-RET-ST-QUERYING`, `PT-RET-ST-AVAILABLE`,
`PT-RET-ST-CELL-ONLY`, and `PT-RET-ST-INACTIVE`. Pending covers an opted-in
live session whose mandatory snapshot has not settled. Querying covers query
publication through its covering CREDIT, including a malformed or incomplete
positive reply that will resolve to CELL-only at that watermark.

After `OPEN`, `PT-CLOSE` is asynchronous: `PT-S-OK` means the close frame was
published and the state is `PT-ST-CLOSING`. The caller continues
`PT-SERVICE` until `PT-ST-ANSI`; only the valid acknowledgement releases the
stream. A close timeout or structural fault enters `PT-ST-LOST`.
`PT-CLOSE` then returns `PT-S-SESSION-LOST` and keeps ownership until the
caller performs an external attachment reset/drain and reinitializes the
session at that proven boundary.

Transaction begin uses the exact span and cell counts to preflight all frame
bytes: `176 + 52 * span-count + 8 * cell-count`. Negotiation guarantees that a
maximum-width row span fits one payload. After a successful begin, valid calls
matching those counts cannot return `WOULD-BLOCK`. `PT-SPAN-BEGIN` opens one
declared span and exactly `count` calls to `PT-CELL` complete it. The module
encodes every field; it does not accept a pointer to Akashic's native packed
cell.

`PT-SNAPSHOT-NEEDED?` is true after opening and after an accepted soft reset.
Only a successful `TX_RESULT` for a snapshot commit clears it. Normal delta
begin while it is true returns `PT-S-INVALID` without output.

After positive retained discovery, ordinary CELL delta transactions remain
available through the existing public words and share PT's one transaction-ID,
result, and revision domain with PRESENT. Legacy replace-all snapshots are
forbidden by RETAINED-1.
`PT-SNAPSHOT-BEGIN` therefore returns `PT-S-UNSUPPORTED` while the discovery
state is AVAILABLE. A rich-terminal consumer must not opt in unless it owns the
bounded semantic/replay state needed to drive PT's PRESENT writer through later
resize and replacement. The generic CELL adapter does not opt in. A soft reset
returns discovery to pending and allows the mandatory revision-zero-to-one CELL
recovery snapshot before the module rediscovers retained support.

If resize arrives after positive discovery, the module records the new
geometry and layout-required state but preserves the global model revision.
It does not fabricate the legacy revision-zero snapshot sequence;
the owning rich-terminal engine first publishes canonical PRESENT CELL_REPLACE
and then completes the retained layout/reveal or closes.
An exact positive discovery reply followed by RESIZE before its covering CREDIT
uses the same PRESENT capacity preflight and preserves the revision; once the
covering CREDIT arrives, the still-empty retained plane starts its required
initial replacement at the accepted geometry. A later RESIZE supersedes an
unstarted older replacement and is admitted only after the same exact checks.

Local commit acceptance leaves exactly one transaction awaiting `TX_RESULT`.
All transaction-begin and lifecycle words return `PT-S-WOULD-BLOCK` until the
outstanding result is processed by `PT-SERVICE`; retained completions must also
be polled before another writer is admitted. Except for the narrow retained
exceptions below, a failed result changes the session to lost before another
event can be returned; this module requires a hard attachment reset and drain
before ANSI can be restored.

The retained APIs make the narrow RETAINED-1 result exceptions explicit rather
than weakening that CELL rule. `PT-COMPLETION-POLL` returns one fixed 80-byte
native descriptor containing completion kind, completed request type, status,
detail, transaction ID, revision, owner tuple, item, and accepted bytes.
OWNER_OPEN always completes through RET_RESULT. OWNER_DROP and PRESENT complete
through TX_RESULT. Once retained discovery is positive, successful legacy CELL
deltas also produce a completion so the upper engine observes their place in
the shared transaction/revision domain; CELL-only callers retain the original
automatic settlement behavior. PT accepts a nonzero PRESENT result without
loss only when the transaction was retained-only, and accepts ordinary
OWNER_DROP status 2 or 3 without loss; the consumer must retain authoritative
desired state and poll the completion before PT admits another writer. Mixed
or CELL-bearing PRESENT, legacy CELL, and SNAPSHOT rejections retain the base
fail-closed behavior.

Owner lifecycle publication is temporarily backpressured while an accepted
resize still requires its first PRESENT CELL_REPLACE. It returns
`PT-S-WOULD-BLOCK`, not `PT-S-UNSUPPORTED`, because retained support remains
negotiated. A crossed soft reset similarly holds result completion and its
new-epoch acknowledgement in order; cumulative CREDIT and caller-requested
close wait until that bounded settlement is complete.

`PT-PRESENT-BEGIN` derives transaction ID, base revision, geometry generation,
and exact declared bytes. `retained-frame-bytes` is the exact sum of complete
40-byte headers plus payloads for the declared retained operations. PT
preflights the complete BEGIN-through-COMMIT byte and sequence budget before it
emits BEGIN, then checks every operation against the declared count and byte
sum. The current generic `PT-PRESENT-OP` admission recognizes only the fixed
REGION_DEFINE/REPLACE/DROP payloads implemented by this module; the typed region
words keep their raw payload assembly private.

The mode constants are `PT-CELL-NONE`, `PT-CELL-DELTA`,
`PT-CELL-REPLACE`, `PT-RET-NONE`, `PT-RET-DELTA`,
`PT-RET-REPLACE-START`, `PT-RET-REPLACE-CONTINUE`,
`PT-RET-LAYOUT-START`, and `PT-RET-LAYOUT-CONTINUE`. Commit disposition is
`PT-COMMIT` or `PT-COMMIT-AND-REVEAL`. PT tracks only the coarse wire rebuild
state needed to reject an impossible START/CONTINUE/DELTA sequence; semantic
model and replay authority remain in the upper rich-terminal engine.
CELL_REPLACE is canonical full-row replacement both for the required resize
boundary and for a caller-selected replace-all update while ACTIVE. RET_NONE,
RET_DELTA, and either START mode require COMMIT. A final matching CONTINUE may
use COMMIT_AND_REVEAL and may include CELL_REPLACE so both planes become visible
at one logical boundary.

## 6. Akashic adapter

Akashic retains its ANSI backend as the default. Its optional neutral
rich-terminal engine binds only to a live module session, serializes semantic
work above PT, and translates native cells field-by-field into CELL-1/PRESENT
spans. The engine does not duplicate the module's wire parser, UART ownership,
session state machine, transaction allocator, revision, credit, or result
slot.

System composition loads this MegaPad module at boot when the configured
hardware includes the optional rich terminal, following the established
`networking.f` pattern. Akashic consumes the resulting public `PT-*` runtime
ABI. No Akashic source file may `REQUIRE` a filesystem path into MegaPad, copy
this module, or pull it into Akashic's linked source closure. The relevant
composition order is therefore:

```forth
REQUIRE rich-terminal.f
\ continue booting the independently packaged Akashic system
```

The generic Akashic engine and its separate UIDL-TUI adapter may depend on the
already-loaded public `PT-*` words and bind only to a caller-supplied live
session. They do not create or auto-open a hidden global session, and Desk or
applets do not receive a separate service, broker, or scene API.

If the module is absent or inactive, the adapter is not constructed. An
acknowledged close atomically restores the ANSI backend, leaves
application/domain state in Akashic, and requests a full ANSI cell redraw.
Session loss instead leaves Akashic quiet with the APT backend and input owner
still bound until an external reset/drain boundary proves ANSI safe.

## 7. Initial conformance

The lightweight module tests prove:

1. KDOS and ANSI behavior without loading the module are unchanged;
2. loading the module alone emits no bytes and changes no input ownership;
3. an ignored probe times out and returns the original owner;
4. successful negotiation establishes exclusive framed ownership;
5. acknowledged close and externally drained hard reset restore ANSI
   ownership, while structural loss alone does not; and
6. an Akashic adapter can send one real cell snapshot through the public API;
7. a caller that explicitly opts in sends deterministic discovery only after
   its successful snapshot, and the covering-CREDIT-only answer leaves it on
   CELL-1 without exposing partial capability records;
8. positive discovery can open one bounded owner, poll its exact RET_RESULT,
   commit a hidden PRESENT containing a real fixed region, and poll the shared
   TX_RESULT completion; and
9. an ordinary legacy CELL delta after retained enablement interleaves in the
   same transaction-ID and global revision domain.

The current guest module conformance claims only the core owner/region writer
slice. Object, resource, series, full semantic replay, and end-to-end retained
resize journeys remain upper-engine follow-on work and must not be advertised
through this API until implemented and qualified.
