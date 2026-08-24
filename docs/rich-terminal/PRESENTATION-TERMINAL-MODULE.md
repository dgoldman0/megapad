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

No APT parser, cell transaction model, negotiation state, or enhanced input
decoder is compiled into KDOS. Phase 1 requires no new BIOS or MMIO mapping;
the module uses the existing UART and terminal-geometry primitives. If future
physical flow control requires hardware support, BIOS exposes only that
bounded primitive and the protocol policy remains in this module.

## 2. Supported absence

The following are ordinary supported configurations:

* the module is not present on disk;
* the module is present but not loaded;
* it is loaded but no caller requests an enhanced session;
* a caller requests a session and the terminal ignores or refuses APT-1; and
* an active session is closed or lost and operation continues through ANSI.

None may prevent KDOS, the ANSI terminal, or Akashic's cell UI from working.

## 3. Ownership

Before negotiation, UART input belongs to the existing ANSI/key path. A caller
explicitly acquires a presentation session and supplies bounded storage and
timeouts. The module temporarily interposes on raw terminal input only while
probing/opening or active.

During `ACTIVE`, the module exclusively owns UART presentation bytes and
returns normalized input events through its API. It releases ownership at the
exact close, failure, timeout, or hard-reset boundary. Buffered enhanced bytes
are never passed into the ANSI key decoder.

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
* normalized key, text, pointer, focus, and resize event decoding; and
* close, hard failure, soft cache reset, and fallback.

It does not own application focus, Desk regions, widgets, retained semantic
objects, or the Akashic front/back cell buffers.

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

Local commit acceptance leaves exactly one transaction awaiting `TX_RESULT`.
Both begin words return `PT-S-WOULD-BLOCK` until a successful result is
processed by `PT-SERVICE`. A failed result changes the session to lost before
another event can be returned; the caller must perform synchronized close or
hard attachment reset and restore ANSI.

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

If the module is absent or inactive, the adapter is not constructed. Session
loss atomically restores the ANSI backend, leaves application/domain state in
Akashic, and requests a full ANSI cell redraw.

## 7. Initial conformance

The lightweight module tests prove:

1. KDOS and ANSI behavior without loading the module are unchanged;
2. loading the module alone emits no bytes and changes no input ownership;
3. an ignored probe times out and returns the original owner;
4. successful negotiation establishes exclusive framed ownership;
5. close and hard reset restore ANSI ownership; and
6. an Akashic adapter can send one real cell snapshot through the public API.
