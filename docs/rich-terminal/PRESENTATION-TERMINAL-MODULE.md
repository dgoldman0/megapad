# `presentation-terminal.f` module boundary

Status: normative for the optional APT-1 guest implementation.

## 1. Placement

`presentation-terminal.f` is a source-loadable MegaPad userland module. Its
role matches `networking.f`: KDOS supplies bounded hardware primitives and the
module supplies the higher-level protocol only when a caller loads it.

The provider identifier is `presentation-terminal.f` (23 bytes). A normal
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
explicitly acquires a presentation session and supplies bounded storage and
timeouts. The module interposes on raw terminal input from `PT-START` until a
proven ANSI-safe close/reset boundary, including while resynchronizing,
closing, or quarantined in `LOST`.

From successful `PT-START` through `OPEN`, `ACTIVE`, resynchronization, close,
or loss, the module exclusively owns UART presentation bytes and returns
normalized input events through its API. It releases ownership on a
pre-`OPEN` refusal/timeout, a valid `CLOSE_ACK`, or an external attachment
reset that advances the link epoch and drains both directions. A post-`OPEN`
structural failure enters `PT-ST-LOST` and retains ownership; it is not an
ANSI fallback boundary. Buffered enhanced bytes are never passed into the
ANSI key decoder.

Probe failure restores the prior ANSI owner and forces a cell redraw if any
probe bytes could have affected physical presentation. Ordinary key bytes
received before the enhanced switch boundary retain their original order.

## 4. Public responsibility

The module owns:

* the APT-1 ANSI-safe negotiation state machine;
* framed encoding and incremental decoding;
* session ID, sequence, presentation epoch, and credit accounting;
* one non-nested outgoing transaction;
* replace-all snapshot transmission;
* normalized key, text, pointer, focus, and resize event decoding;
* explicitly requested RETAINED-1 discovery, exact CAPS/FORMATS validation,
  and lifecycle-bounded access to the accepted records; and
* close, hard failure, soft cache reset, and fallback.

It does not own application focus, Desk regions, widgets, retained semantic
objects, retained PRESENT transaction construction, or the Akashic front/back
cell buffers. In particular, a successful discovery does not imply that this
module can yet publish a retained scene.

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
PT-INIT             ( rx-a rx-u tx-a tx-u event-a event-u session -- status )
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
available, but legacy replace-all snapshots are forbidden by RETAINED-1.
`PT-SNAPSHOT-BEGIN` therefore returns `PT-S-UNSUPPORTED` while the discovery
state is AVAILABLE. A presentation consumer must not opt in unless it owns the
separate PRESENT builder needed for later resize/replacement work. The generic
CELL adapter does not opt in. A soft reset returns discovery to pending and
allows the mandatory revision-zero-to-one CELL recovery snapshot before the
module rediscovers retained support.

If resize arrives after positive discovery, the module records the new
geometry and replacement-needed state but preserves the global presentation
revision. It does not fabricate the legacy revision-zero snapshot sequence;
the owning PRESENT consumer must complete the retained replacement or close.

Local commit acceptance leaves exactly one transaction awaiting `TX_RESULT`.
Both begin words return `PT-S-WOULD-BLOCK` until a successful result is
processed by `PT-SERVICE`. A failed result changes the session to lost before
another event can be returned; this module requires a hard attachment reset
and drain before ANSI can be restored.

## 6. Akashic adapter

Akashic retains its ANSI backend as the default. Its optional APT adapter binds
only to a live module session and translates native cells field-by-field into
CELL-1 spans. Akashic may load and call the module, but does not duplicate its
wire parser or session state machine.

The generic Akashic screen and ANSI backend never `REQUIRE` this module. The
optional integration loader uses this explicit order:

```forth
REQUIRE presentation-terminal.f
REQUIRE akashic/tui/screen-backend-apt1.f
```

`screen-backend-apt1.f` may depend on the public `PT-` words and constructs a
backend only from a caller-supplied live `session`; it does not create or
auto-open a hidden global session. Packaging that adapter must therefore make
the MegaPad root module available to KDOS `REQUIRE` resolution.

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
   and
7. a caller that explicitly opts in sends deterministic discovery only after
   its successful snapshot, and the covering-CREDIT-only answer leaves it on
   CELL-1 without exposing partial capability records.

The current guest module conformance does not claim a positive retained scene:
PRESENT construction, owner/resource brokerage, replay, and retained resize
are separate consumers and remain outside this module boundary.
