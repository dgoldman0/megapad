\ =====================================================================
\  rich-terminal.f -- optional APT-1 rich-terminal guest client
\ =====================================================================
\
\  This module is deliberately inert when loaded.  PT-START is the only
\  word that emits an APT probe or takes raw UART input ownership.  ANSI
\  remains the baseline before negotiation and after a synchronized close.
\
\  Contracts: APT-1-CELL-1-2026-08-24 plus the core owner/region subset of
\             APT-1-RETAINED-1-2026-08-24.
\  Normative wire text: docs/rich-terminal/APT-1-WIRE.md

PROVIDED rich-terminal.f

\ =====================================================================
\  Public constants and caller-owned records
\ =====================================================================

0 CONSTANT PT-S-OK
1 CONSTANT PT-S-WOULD-BLOCK
2 CONSTANT PT-S-SESSION-LOST
3 CONSTANT PT-S-INVALID
4 CONSTANT PT-S-UNSUPPORTED

0 CONSTANT PT-ST-ANSI
1 CONSTANT PT-ST-PROBING
2 CONSTANT PT-ST-OPENING
3 CONSTANT PT-ST-ACTIVE
4 CONSTANT PT-ST-RESYNCING
5 CONSTANT PT-ST-CLOSING
6 CONSTANT PT-ST-LOST

\ PT-EVENT-POLL writes this fixed, byte-copyable descriptor:
\   +0  wire message type       +8  model revision
\   +16 value 0                 +24 value 1
\   +32 value 2                 +40 value 3
\   +48 data address            +56 data byte count
\ KEY:     revision=model, v0=symbol, v1=action, v2=location, v3=modifiers.
\ TEXT:    revision=model, v0=flags, data=UTF-8 bytes.
\ POINTER: revision=model, v0=x, v1=y, v2=buttons|changed<<16,
\          v3=modifiers|kind<<16|raw-wheel-x<<32|raw-wheel-y<<48.
\ RESIZE:  v0=cols, v1=rows, v2=geometry generation (revision is zero).
\ FOCUS:   revision=model, v0=focused.
\ TEXT data remains valid until the next PT-SERVICE for the session.
64  CONSTANT /PT-EVENT
80  CONSTANT /PT-COMPLETION
872 CONSTANT /PT-SESSION

: PT-SESSION-SIZE  ( -- bytes )  /PT-SESSION ;
: PT-EVENT-SIZE    ( -- bytes )  /PT-EVENT ;
: PT-COMPLETION-SIZE  ( -- bytes )  /PT-COMPLETION ;

\ Event kinds are the corresponding APT-1 input message IDs.
0x0200 CONSTANT PT-EVENT-KEY
0x0201 CONSTANT PT-EVENT-TEXT
0x0202 CONSTANT PT-EVENT-POINTER
0x0203 CONSTANT PT-EVENT-RESIZE
0x0204 CONSTANT PT-EVENT-FOCUS

: PT-EVENT-TYPE@      ( event -- u )       @ ;
: PT-EVENT-REVISION@  ( event -- u )   8 + @ ;
: PT-EVENT-VALUE0@    ( event -- u )  16 + @ ;
: PT-EVENT-VALUE1@    ( event -- u )  24 + @ ;
: PT-EVENT-VALUE2@    ( event -- u )  32 + @ ;
: PT-EVENT-VALUE3@    ( event -- u )  40 + @ ;
: PT-EVENT-DATA@      ( event -- a u ) DUP 48 + @ SWAP 56 + @ ;

\ PT-COMPLETION-POLL writes this fixed, byte-copyable descriptor:
\   +0  completion kind          +8  completed wire request type
\   +16 wire status              +24 bounded detail
\   +32 transaction ID           +40 resulting/current revision
\   +48 owner ID                 +56 owner generation
\   +64 item ID                  +72 accepted resource bytes
\ OWNER_OPEN has no transaction ID.  PRESENT and OWNER_DROP have no item or
\ accepted-byte result.  All unused fields are zero.
1 CONSTANT PT-COMPLETE-TX
2 CONSTANT PT-COMPLETE-RET

0 CONSTANT PT-TX-RESULT-OK
1 CONSTANT PT-TX-RESULT-ABORTED
2 CONSTANT PT-TX-RESULT-INVALID
3 CONSTANT PT-TX-RESULT-STALE

0 CONSTANT PT-RET-OK
1 CONSTANT PT-RET-INVALID
2 CONSTANT PT-RET-STALE-OWNER
3 CONSTANT PT-RET-NO-CAPACITY
4 CONSTANT PT-RET-DUPLICATE-ID
5 CONSTANT PT-RET-IN-USE
6 CONSTANT PT-RET-BAD-CONTENT
7 CONSTANT PT-RET-ABORTED

: PT-COMPLETION-KIND@           ( completion -- u )       @ ;
: PT-COMPLETION-REQUEST@        ( completion -- u )   8 + @ ;
: PT-COMPLETION-STATUS@         ( completion -- u )  16 + @ ;
: PT-COMPLETION-DETAIL@         ( completion -- u )  24 + @ ;
: PT-COMPLETION-TXID@           ( completion -- u )  32 + @ ;
: PT-COMPLETION-REVISION@       ( completion -- u )  40 + @ ;
: PT-COMPLETION-OWNER@          ( completion -- u )  48 + @ ;
: PT-COMPLETION-GENERATION@     ( completion -- u )  56 + @ ;
: PT-COMPLETION-ITEM@           ( completion -- u )  64 + @ ;
: PT-COMPLETION-ACCEPTED-BYTES@ ( completion -- u )  72 + @ ;

\ Public PRESENT modes are the normative wire values.  The PT module remains
\ the authority for session, epoch, transaction ID, base revision, geometry
\ generation, exact declared bytes, sequence, and credit.
0 CONSTANT PT-CELL-NONE
1 CONSTANT PT-CELL-DELTA
2 CONSTANT PT-CELL-REPLACE

0 CONSTANT PT-RET-NONE
1 CONSTANT PT-RET-DELTA
2 CONSTANT PT-RET-REPLACE-START
3 CONSTANT PT-RET-REPLACE-CONTINUE
4 CONSTANT PT-RET-LAYOUT-START
5 CONSTANT PT-RET-LAYOUT-CONTINUE

0 CONSTANT PT-COMMIT
1 CONSTANT PT-COMMIT-AND-REVEAL

\ RETAINED-1 discovery is an explicit caller opt-in.  Calling
\ PT-RETAINED-DISCOVER records that opt-in on the caller-owned session; it
\ never emits a frame itself.  PT-SERVICE sends the one query only after a
\ successful initial CELL snapshot has settled and no transaction result is
\ outstanding.  The opt-in survives soft reset and synchronized close/reopen,
\ and PT-INIT clears it.
\
\ PENDING means the opted-in session still needs its initial snapshot,
\ QUERYING covers RET_QUERY through its covering CREDIT, AVAILABLE requires
\ one valid adjacent CAPS/FORMATS pair, and CELL-ONLY is the deterministic
\ negative answer.  A rejected pair remains QUERYING until its covering
\ CREDIT makes that negative answer final.  INACTIVE covers a caller that has
\ not opted in, an invalid handle, ANSI/probing/opening, closing, or loss.
0 CONSTANT PT-RET-ST-PENDING
1 CONSTANT PT-RET-ST-QUERYING
2 CONSTANT PT-RET-ST-AVAILABLE
3 CONSTANT PT-RET-ST-CELL-ONLY
4 CONSTANT PT-RET-ST-INACTIVE

\ Fixed frame and profile constants.
40       CONSTANT _PT-HDR
1048576  CONSTANT _PT-MAX-PAYLOAD
4096     CONSTANT _PT-CONTROL-RESERVE
0x3F     CONSTANT _PT-CAPS
250      CONSTANT _PT-TIMEOUT-MS
3        CONSTANT _PT-PROBE-LIMIT
256      CONSTANT _PT-SERVICE-BYTES
92       CONSTANT _PT-OFFER-BYTES
73       CONSTANT _PT-OPEN-BYTES
38       CONSTANT _PT-PROBE-BYTES
0x4150543153455353 CONSTANT _PT-SIGNATURE

\ The UART has exactly one enhanced owner.  KDOS has no generic raw-ingress
\ lease, so this registry is a cooperative guest-side contract: the foreground
\ UI/input-owner core serializes PT calls and quiesces all other KEY?/KEY
\ readers from successful PT-START through close or external reset.  LOST keeps
\ this owner; it is never permission to resume ANSI input.
CREATE _PT-OWNER  0 ,

\ Message IDs.
0x0001 CONSTANT _PT-M-SERVER-READY
0x0002 CONSTANT _PT-M-CLIENT-READY
0x0003 CONSTANT _PT-M-CREDIT
0x0004 CONSTANT _PT-M-ERROR
0x0005 CONSTANT _PT-M-CLOSE
0x0006 CONSTANT _PT-M-CLOSE-ACK
0x0007 CONSTANT _PT-M-SOFT-RESET-REQUEST
0x0008 CONSTANT _PT-M-SOFT-RESET-ACK
0x0009 CONSTANT _PT-M-TX-RESULT
0x000A CONSTANT _PT-M-RET-RESULT
0x000B CONSTANT _PT-M-OWNER-DROP
0x0100 CONSTANT _PT-M-TX-BEGIN
0x0101 CONSTANT _PT-M-CELL-SPAN
0x0102 CONSTANT _PT-M-CURSOR
0x0103 CONSTANT _PT-M-TX-COMMIT
0x0104 CONSTANT _PT-M-TX-ABORT
0x0110 CONSTANT _PT-M-SNAPSHOT-BEGIN
0x0111 CONSTANT _PT-M-SNAPSHOT-COMMIT
0x0200 CONSTANT _PT-M-KEY
0x0201 CONSTANT _PT-M-TEXT
0x0202 CONSTANT _PT-M-POINTER
0x0203 CONSTANT _PT-M-RESIZE
0x0204 CONSTANT _PT-M-FOCUS
0x8000 CONSTANT _PT-M-RET-QUERY
0x8001 CONSTANT _PT-M-RET-CAPS
0x8002 CONSTANT _PT-M-RET-FORMATS
0x2000 CONSTANT _PT-M-PRESENT-BEGIN
0x2001 CONSTANT _PT-M-PRESENT-COMMIT
0x2002 CONSTANT _PT-M-OWNER-OPEN
0x2010 CONSTANT _PT-M-REGION-DEFINE
0x2011 CONSTANT _PT-M-REGION-REPLACE
0x2012 CONSTANT _PT-M-REGION-DROP

\ Completion request values intentionally expose only the implemented
\ retained lifecycle and transaction writers, not the private message table.
_PT-M-PRESENT-COMMIT CONSTANT PT-REQUEST-PRESENT-COMMIT
_PT-M-OWNER-OPEN     CONSTANT PT-REQUEST-OWNER-OPEN
_PT-M-OWNER-DROP     CONSTANT PT-REQUEST-OWNER-DROP
_PT-M-TX-COMMIT      CONSTANT PT-REQUEST-TX-COMMIT

0x31544552 CONSTANT _PT-RET1-TAG
208        CONSTANT _PT-RET-REPLY-BYTES

0 CONSTANT _PT-RD-SNAPSHOT
1 CONSTANT _PT-RD-WAIT-CAPS
2 CONSTANT _PT-RD-WAIT-FORMATS
3 CONSTANT _PT-RD-WAIT-CREDIT
4 CONSTANT _PT-RD-INVALID
5 CONSTANT _PT-RD-AVAILABLE
6 CONSTANT _PT-RD-CELL-ONLY

\ Session fields are all native 64-bit cells.  Wire fields are always read
\ and written with explicit little-endian helpers below.
: _PT.S.SIGNATURE       ( s -- a )       ;
: _PT.S.RX-A            ( s -- a )   8 + ;
: _PT.S.RX-U            ( s -- a )  16 + ;
: _PT.S.BIN-U           ( s -- a )  24 + ;
: _PT.S.LEGACY-U        ( s -- a )  32 + ;
: _PT.S.LEGACY-POS      ( s -- a )  40 + ;
: _PT.S.NEG-SCAN        ( s -- a )  48 + ;
: _PT.S.TX-A            ( s -- a )  56 + ;
: _PT.S.TX-U            ( s -- a )  64 + ;
: _PT.S.EVENT-A         ( s -- a )  72 + ;
: _PT.S.EVENT-U         ( s -- a )  80 + ;
: _PT.S.EVENT-PENDING   ( s -- a )  88 + ;
: _PT.S.EVENT-TYPE      ( s -- a )  96 + ;
: _PT.S.EVENT-LEN       ( s -- a ) 104 + ;
: _PT.S.EVENT-CHARGE    ( s -- a ) 112 + ;
: _PT.S.STATE           ( s -- a ) 120 + ;
: _PT.S.DEADLINE        ( s -- a ) 128 + ;
: _PT.S.PROBES          ( s -- a ) 136 + ;
: _PT.S.NONCE           ( s -- a ) 144 + ;
: _PT.S.SESSION-ID      ( s -- a ) 152 + ;
: _PT.S.PEER-MAX-PAY    ( s -- a ) 160 + ;
: _PT.S.PEER-MAX-TX     ( s -- a ) 168 + ;
: _PT.S.PEER-GRANT      ( s -- a ) 176 + ;
: _PT.S.PEER-SENT       ( s -- a ) 184 + ;
: _PT.S.COLS            ( s -- a ) 192 + ;
: _PT.S.ROWS            ( s -- a ) 200 + ;
: _PT.S.CAPS            ( s -- a ) 208 + ;
: _PT.S.CLIENT-MAX-PAY  ( s -- a ) 216 + ;
: _PT.S.LOCAL-GRANT     ( s -- a ) 224 + ;
: _PT.S.LOCAL-RECEIVED  ( s -- a ) 232 + ;
: _PT.S.MAX-TEXT        ( s -- a ) 240 + ;
: _PT.S.TX-SEQ          ( s -- a ) 248 + ;
: _PT.S.RX-SEQ          ( s -- a ) 256 + ;
: _PT.S.EPOCH           ( s -- a ) 264 + ;
: _PT.S.NEXT-TXID       ( s -- a ) 272 + ;
: _PT.S.REVISION        ( s -- a ) 280 + ;
: _PT.S.SNAPSHOT?       ( s -- a ) 288 + ;
: _PT.S.TX-OPEN?        ( s -- a ) 296 + ;
: _PT.S.TX-SNAPSHOT?    ( s -- a ) 304 + ;
: _PT.S.TXID            ( s -- a ) 312 + ;
: _PT.S.TX-SPANS        ( s -- a ) 320 + ;
: _PT.S.TX-CELLS        ( s -- a ) 328 + ;
: _PT.S.TX-SPANS-DONE   ( s -- a ) 336 + ;
: _PT.S.TX-CELLS-DONE   ( s -- a ) 344 + ;
: _PT.S.SPAN-REMAIN     ( s -- a ) 352 + ;
: _PT.S.CURSOR-DONE?    ( s -- a ) 360 + ;
: _PT.S.LAST-END        ( s -- a ) 368 + ;
: _PT.S.TX-BYTES        ( s -- a ) 376 + ;
: _PT.S.AWAIT?          ( s -- a ) 384 + ;
: _PT.S.AWAIT-TXID      ( s -- a ) 392 + ;
: _PT.S.AWAIT-SNAPSHOT? ( s -- a ) 400 + ;
: _PT.S.CLOSE-REASON    ( s -- a ) 408 + ;
: _PT.S.PEER-INITIAL    ( s -- a ) 416 + ;
: _PT.S.CREDIT-DIRTY?   ( s -- a ) 424 + ;
: _PT.S.GEOMETRY-GEN    ( s -- a ) 432 + ;
: _PT.S.GEOMETRY-SEEN?  ( s -- a ) 440 + ;
: _PT.S.CLOSE-OPENING?  ( s -- a ) 448 + ;
: _PT.S.RET-STATE       ( s -- a ) 456 + ;
: _PT.S.RET-WATERMARK   ( s -- a ) 464 + ;
: _PT.S.RET-CAPS        ( s -- a ) 472 + ;  \ exact 64-byte RET_CAPS payload
: _PT.S.RET-FORMATS     ( s -- a ) 536 + ;  \ exact 64-byte RET_FORMATS payload
: _PT.S.RET-SQUERY      ( s -- a ) 600 + ;
: _PT.S.RET-ENABLED?    ( s -- a ) 608 + ;
: _PT.S.TX-KIND         ( s -- a ) 616 + ;
: _PT.S.TX-CELL-MODE    ( s -- a ) 624 + ;
: _PT.S.TX-RET-MODE     ( s -- a ) 632 + ;
: _PT.S.TX-RET-OPS      ( s -- a ) 640 + ;
: _PT.S.TX-RET-OPS-DONE ( s -- a ) 648 + ;
: _PT.S.TX-RET-BYTES    ( s -- a ) 656 + ;
: _PT.S.TX-RET-BYTES-DONE ( s -- a ) 664 + ;
: _PT.S.TX-DISPOSITION  ( s -- a ) 672 + ;
: _PT.S.AWAIT-KIND      ( s -- a ) 680 + ;
: _PT.S.AWAIT-CELL-MODE ( s -- a ) 688 + ;
: _PT.S.AWAIT-RET-MODE  ( s -- a ) 696 + ;
: _PT.S.AWAIT-DISPOSITION ( s -- a ) 704 + ;
: _PT.S.AWAIT-OWNER     ( s -- a ) 712 + ;
: _PT.S.AWAIT-GENERATION ( s -- a ) 720 + ;
: _PT.S.LIFE-AWAIT?     ( s -- a ) 728 + ;
: _PT.S.LIFE-TYPE       ( s -- a ) 736 + ;
: _PT.S.LIFE-OWNER      ( s -- a ) 744 + ;
: _PT.S.LIFE-GENERATION ( s -- a ) 752 + ;
: _PT.S.COMPLETE?       ( s -- a ) 760 + ;
: _PT.S.COMP-KIND       ( s -- a ) 768 + ;
: _PT.S.COMP-REQUEST    ( s -- a ) 776 + ;
: _PT.S.COMP-STATUS     ( s -- a ) 784 + ;
: _PT.S.COMP-DETAIL     ( s -- a ) 792 + ;
: _PT.S.COMP-TXID       ( s -- a ) 800 + ;
: _PT.S.COMP-REVISION   ( s -- a ) 808 + ;
: _PT.S.COMP-OWNER      ( s -- a ) 816 + ;
: _PT.S.COMP-GENERATION ( s -- a ) 824 + ;
: _PT.S.COMP-ITEM       ( s -- a ) 832 + ;
: _PT.S.COMP-ACCEPTED   ( s -- a ) 840 + ;
: _PT.S.RET-REBUILD     ( s -- a ) 848 + ;
: _PT.S.RESET-PENDING?  ( s -- a ) 856 + ;
: _PT.S.RESET-EPOCH     ( s -- a ) 864 + ;

0 CONSTANT _PT-TX-NONE
1 CONSTANT _PT-TX-CELL
2 CONSTANT _PT-TX-SNAPSHOT
3 CONSTANT _PT-TX-PRESENT

0 CONSTANT _PT-AWAIT-NONE
1 CONSTANT _PT-AWAIT-CELL
2 CONSTANT _PT-AWAIT-SNAPSHOT
3 CONSTANT _PT-AWAIT-PRESENT
4 CONSTANT _PT-AWAIT-OWNER-DROP

0 CONSTANT _PT-RB-NONE
1 CONSTANT _PT-RB-REPLACE-REQUIRED
2 CONSTANT _PT-RB-REPLACE-PENDING
3 CONSTANT _PT-RB-LAYOUT-REQUIRED
4 CONSTANT _PT-RB-LAYOUT-PENDING

: _PT-VALID-S?  ( s -- flag )
    DUP 0= IF DROP FALSE EXIT THEN
    @ _PT-SIGNATURE = ;

: PT-STREAM-OWNED?  ( -- flag )
    _PT-OWNER @ 0<> ;

: PT-OWNS?  ( session -- flag )
    DUP _PT-VALID-S? 0= IF DROP FALSE EXIT THEN
    _PT-OWNER @ = ;

: PT-LEGACY-PENDING?  ( session -- flag )
    DUP _PT-VALID-S? 0= IF DROP FALSE EXIT THEN
    DUP _PT.S.LEGACY-POS @ SWAP _PT.S.LEGACY-U @ U< ;

: PT-STATE@  ( session -- state )
    DUP _PT-VALID-S? 0= IF DROP PT-ST-LOST EXIT THEN
    _PT.S.STATE @ ;

: PT-ACTIVE?  ( session -- flag )
    PT-STATE@ DUP PT-ST-ACTIVE = SWAP PT-ST-RESYNCING = OR ;

: PT-SNAPSHOT-NEEDED?  ( session -- flag )
    DUP _PT-VALID-S? 0= IF DROP FALSE EXIT THEN
    _PT.S.SNAPSHOT? @ 0<> ;

: PT-RETAINED-STATE@  ( session -- state )
    DUP _PT-VALID-S? 0= IF DROP PT-RET-ST-INACTIVE EXIT THEN
    DUP _PT.S.RET-ENABLED? @ 0= IF DROP PT-RET-ST-INACTIVE EXIT THEN
    DUP _PT.S.STATE @ DUP PT-ST-ACTIVE = SWAP PT-ST-RESYNCING = OR 0= IF
        DROP PT-RET-ST-INACTIVE EXIT
    THEN
    _PT.S.RET-STATE @
    DUP _PT-RD-AVAILABLE = IF DROP PT-RET-ST-AVAILABLE EXIT THEN
    DUP _PT-RD-CELL-ONLY = IF DROP PT-RET-ST-CELL-ONLY EXIT THEN
    DUP _PT-RD-SNAPSHOT = IF DROP PT-RET-ST-PENDING EXIT THEN
    DROP PT-RET-ST-QUERYING ;

: PT-RETAINED-AVAILABLE?  ( session -- flag )
    PT-RETAINED-STATE@ PT-RET-ST-AVAILABLE = ;

\ The returned records are exact little-endian wire payloads with the layouts
\ documented in APT-1-RETAINED-1 sections 4.1 and 4.2.  Their addresses remain
\ valid until soft reset, close, loss, or PT-INIT.  An unavailable record is
\ reported as 0 0; callers never observe a partial pair.
: PT-RETAINED-CAPS@  ( session -- a u )
    DUP PT-RETAINED-AVAILABLE? 0= IF DROP 0 0 EXIT THEN
    _PT.S.RET-CAPS 64 ;

: PT-RETAINED-FORMATS@  ( session -- a u )
    DUP PT-RETAINED-AVAILABLE? 0= IF DROP 0 0 EXIT THEN
    _PT.S.RET-FORMATS 64 ;

: _PT-RET-RECORDS-CLEAR  ( s -- )
    DUP _PT.S.RET-CAPS 64 0 FILL
    _PT.S.RET-FORMATS 64 0 FILL ;

: _PT-RET-RESET  ( s -- )
    DUP _PT-RET-RECORDS-CLEAR
    DUP _PT.S.RET-WATERMARK OFF
    DUP _PT.S.RET-SQUERY OFF
    DUP _PT.S.RET-REBUILD OFF
    _PT-RD-SNAPSHOT SWAP _PT.S.RET-STATE ! ;

: _PT-RET-INVALIDATE  ( s -- )
    DUP _PT-RET-RECORDS-CLEAR
    DUP _PT.S.RET-REBUILD OFF
    _PT-RD-INVALID SWAP _PT.S.RET-STATE ! ;

: _PT-RET-CELL-ONLY  ( s -- )
    DUP _PT-RET-RECORDS-CLEAR
    DUP _PT.S.RET-REBUILD OFF
    _PT-RD-CELL-ONLY SWAP _PT.S.RET-STATE ! ;

\ Opt in without publishing bytes.  It is safe before PT-START or after the
\ initial snapshot; PT-SERVICE performs the quiescence and credit preflight.
\ Repeating the call is idempotent and never retries an epoch-local answer.
: PT-RETAINED-DISCOVER  ( session -- status )
    DUP _PT-VALID-S? 0= IF DROP PT-S-INVALID EXIT THEN
    DUP _PT.S.STATE @ DUP PT-ST-CLOSING = SWAP PT-ST-LOST = OR IF
        DROP PT-S-SESSION-LOST EXIT
    THEN
    DUP _PT.S.RET-ENABLED? @ IF DROP PT-S-OK EXIT THEN
    TRUE OVER _PT.S.RET-ENABLED? !
    _PT-RET-RESET PT-S-OK ;

: _PT-OP-LOST?  ( s -- flag )
    _PT.S.STATE @ DUP PT-ST-LOST = SWAP PT-ST-CLOSING = OR ;

: _PT-OWNER-RELEASE  ( s -- )
    _PT-OWNER @ OVER = IF DROP 0 _PT-OWNER ! ELSE DROP THEN ;

: _PT-TX-CLEAR  ( s -- )
    DUP _PT.S.TX-OPEN? OFF
    DUP _PT.S.TX-KIND OFF
    DUP _PT.S.TX-SNAPSHOT? OFF
    DUP _PT.S.TX-CELL-MODE OFF
    DUP _PT.S.TX-RET-MODE OFF
    DUP _PT.S.TX-RET-OPS OFF
    DUP _PT.S.TX-RET-OPS-DONE OFF
    DUP _PT.S.TX-RET-BYTES OFF
    DUP _PT.S.TX-RET-BYTES-DONE OFF
    DUP _PT.S.TX-DISPOSITION OFF
    DUP _PT.S.SPAN-REMAIN OFF
    DROP ;

: _PT-AWAIT-CLEAR  ( s -- )
    DUP _PT.S.AWAIT? OFF
    DUP _PT.S.AWAIT-KIND OFF
    DUP _PT.S.AWAIT-SNAPSHOT? OFF
    DUP _PT.S.AWAIT-CELL-MODE OFF
    DUP _PT.S.AWAIT-RET-MODE OFF
    DUP _PT.S.AWAIT-DISPOSITION OFF
    DUP _PT.S.AWAIT-OWNER OFF
    _PT.S.AWAIT-GENERATION OFF ;

: _PT-LIFE-CLEAR  ( s -- )
    DUP _PT.S.LIFE-AWAIT? OFF
    DUP _PT.S.LIFE-TYPE OFF
    DUP _PT.S.LIFE-OWNER OFF
    _PT.S.LIFE-GENERATION OFF ;

: _PT-COMPLETION-CLEAR  ( s -- )
    DUP _PT.S.COMPLETE? OFF
    _PT.S.COMP-KIND /PT-COMPLETION 0 FILL ;

: _PT-RESULT-BUSY?  ( s -- flag )
    DUP _PT.S.AWAIT? @
    OVER _PT.S.LIFE-AWAIT? @ OR
    OVER _PT.S.COMPLETE? @ OR
    SWAP _PT.S.RESET-PENDING? @ OR ;

\ =====================================================================
\  Checked scalar, range, and little-endian helpers
\ =====================================================================

: _PT-U<=  ( a b -- flag )  U> 0= ;
: _PT-U>=  ( a b -- flag )  U< 0= ;

: _PT-U64@  ( a -- u )
    DUP L@ SWAP 4 + L@ 32 LSHIFT OR ;

VARIABLE _PT-U64-V
VARIABLE _PT-U64-A
: _PT-U64!  ( u a -- )
    _PT-U64-A ! _PT-U64-V !
    _PT-U64-V @ _PT-U64-A @ L!
    _PT-U64-V @ 32 RSHIFT _PT-U64-A @ 4 + L! ;

: _PT-I32@  ( a -- n )
    L@ DUP 0x80000000 AND IF 0xFFFFFFFF00000000 OR THEN ;

: _PT-I16@  ( a -- n )
    W@ DUP 0x8000 AND IF 0xFFFFFFFFFFFF0000 OR THEN ;

: _PT-RANGE-VALID?  ( a u -- flag )
    OVER 0<> OVER 0<> AND 0= IF 2DROP FALSE EXIT THEN
    OVER + SWAP U> ;

VARIABLE _PT-RA
VARIABLE _PT-RU
VARIABLE _PT-RB
VARIABLE _PT-RV
: _PT-RANGES-OVERLAP?  ( a u b v -- flag )
    _PT-RV ! _PT-RB ! _PT-RU ! _PT-RA !
    _PT-RA @ _PT-RU @ + _PT-RB @ U>
    _PT-RB @ _PT-RV @ + _PT-RA @ U> AND ;

: _PT-U32?  ( u -- flag )  0xFFFFFFFF _PT-U<= ;
: _PT-U16?  ( u -- flag )  0xFFFF _PT-U<= ;
: _PT-U8?   ( u -- flag )  0xFF _PT-U<= ;

: _PT-I32?  ( n -- flag )
    DUP -2147483648 < IF DROP FALSE EXIT THEN
    2147483647 > 0= ;

: _PT-UADD?  ( a b -- sum flag )
    OVER + DUP ROT _PT-U>= ;

: _PT-UMUL?  ( a b -- product flag )
    UM* DUP IF 2DROP 0 FALSE EXIT THEN
    DROP TRUE ;

: _PT-SCALAR?  ( cp -- flag )
    DUP 0x10FFFF U> IF DROP FALSE EXIT THEN
    DUP 0xD800 _PT-U>= SWAP 0xDFFF _PT-U<= AND 0= ;

VARIABLE _PT-CA
VARIABLE _PT-CU
VARIABLE _PT-CC
VARIABLE _PT-CI
: _PT-CRC-RANGE  ( crc a u -- crc' )
    _PT-CU ! _PT-CA ! _PT-CC ! 0 _PT-CI !
    BEGIN _PT-CI @ _PT-CU @ U< WHILE
        _PT-CC @ _PT-CA @ _PT-CI @ + C@ XOR _PT-CC !
        0
        BEGIN DUP 8 < WHILE
            _PT-CC @ DUP 1 AND IF
                1 RSHIFT 0x82F63B78 XOR
            ELSE
                1 RSHIFT
            THEN
            0xFFFFFFFF AND _PT-CC !
            1+
        REPEAT DROP
        _PT-CI @ 1+ _PT-CI !
    REPEAT
    _PT-CC @ ;

VARIABLE _PT-CRC-A
VARIABLE _PT-CRC-U
: _PT-FRAME-CRC  ( frame payload-u -- crc )
    _PT-CRC-U ! _PT-CRC-A !
    0xFFFFFFFF _PT-CRC-A @ 36 _PT-CRC-RANGE
    _PT-CRC-A @ _PT-HDR + _PT-CRC-U @ _PT-CRC-RANGE
    0xFFFFFFFF XOR ;

\ =====================================================================
\  Initialization: validates all borrowed storage without side effects
\ =====================================================================

VARIABLE _PT-I-RXA
VARIABLE _PT-I-RXU
VARIABLE _PT-I-TXA
VARIABLE _PT-I-TXU
VARIABLE _PT-I-EVA
VARIABLE _PT-I-EVU
VARIABLE _PT-I-S

: _PT-INIT-RANGES?  ( -- flag )
    _PT-I-RXA @ _PT-I-RXU @ _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-I-TXA @ _PT-I-TXU @ _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-I-EVA @ _PT-I-EVU @ _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-I-S @ /PT-SESSION _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-I-S @ 7 AND IF FALSE EXIT THEN
    _PT-I-RXA @ _PT-I-RXU @ _PT-I-TXA @ _PT-I-TXU @
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-I-RXA @ _PT-I-RXU @ _PT-I-EVA @ _PT-I-EVU @
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-I-RXA @ _PT-I-RXU @ _PT-I-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-I-TXA @ _PT-I-TXU @ _PT-I-EVA @ _PT-I-EVU @
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-I-TXA @ _PT-I-TXU @ _PT-I-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-I-EVA @ _PT-I-EVU @ _PT-I-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? 0= ;

: PT-INIT  ( rx-a rx-u tx-a tx-u event-a event-u session -- status )
    _PT-I-S ! _PT-I-EVU ! _PT-I-EVA ! _PT-I-TXU !
    _PT-I-TXA ! _PT-I-RXU ! _PT-I-RXA !
    _PT-I-RXU @ _PT-CONTROL-RESERVE _PT-HDR + 32 + U< IF
        PT-S-INVALID EXIT
    THEN
    _PT-I-TXU @ _PT-OPEN-BYTES U< IF PT-S-INVALID EXIT THEN
    _PT-I-EVU @ /PT-EVENT U< IF PT-S-INVALID EXIT THEN
    _PT-INIT-RANGES? 0= IF PT-S-INVALID EXIT THEN
    _PT-I-S @ _PT-VALID-S? IF
        _PT-I-S @ _PT.S.LEGACY-POS @
        _PT-I-S @ _PT.S.LEGACY-U @ <> IF PT-S-WOULD-BLOCK EXIT THEN
    THEN
    _PT-OWNER @ _PT-I-S @ = IF
        \ Reinitializing the live owner is forbidden.  LOST may be
        \ reinitialized only at the caller's external reset-and-drain
        \ boundary; PT-INIT itself does not reset the attachment.
        _PT-I-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
        _PT-I-S @ _PT.S.STATE @ PT-ST-LOST <> IF PT-S-INVALID EXIT THEN
        0 _PT-OWNER !
    THEN
    _PT-I-S @ /PT-SESSION 0 FILL
    _PT-SIGNATURE _PT-I-S @ _PT.S.SIGNATURE !
    _PT-I-RXA @ _PT-I-S @ _PT.S.RX-A !
    _PT-I-RXU @ _PT-I-S @ _PT.S.RX-U !
    _PT-I-TXA @ _PT-I-S @ _PT.S.TX-A !
    _PT-I-TXU @ _PT-I-S @ _PT.S.TX-U !
    _PT-I-EVA @ _PT-I-S @ _PT.S.EVENT-A !
    _PT-I-EVU @ _PT-I-S @ _PT.S.EVENT-U !
    PT-ST-ANSI _PT-I-S @ _PT.S.STATE !
    1 _PT-I-S @ _PT.S.NEXT-TXID !
    PT-S-OK ;

\ A composed adapter may own additional caller-provided storage beside a live
\ PT session.  PT retains the exact geometry of all four of its borrowed
\ spans, so it is the only module that can prove such storage does not alias
\ the session record or any RX, TX, or event backing.  This predicate performs
\ no caller-visible storage writes and exposes no borrowed address.
VARIABLE _PT-SD-A
VARIABLE _PT-SD-U
VARIABLE _PT-SD-S
: PT-STORAGE-DISJOINT?  ( a u session -- flag )
    _PT-SD-S ! _PT-SD-U ! _PT-SD-A !
    _PT-SD-S @ _PT-VALID-S? 0= IF FALSE EXIT THEN
    _PT-SD-A @ _PT-SD-U @ _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-SD-A @ _PT-SD-U @ _PT-SD-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-SD-A @ _PT-SD-U @ _PT-SD-S @ _PT.S.RX-A @
        _PT-SD-S @ _PT.S.RX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-SD-A @ _PT-SD-U @ _PT-SD-S @ _PT.S.TX-A @
        _PT-SD-S @ _PT.S.TX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-SD-A @ _PT-SD-U @ _PT-SD-S @ _PT.S.EVENT-A @
        _PT-SD-S @ _PT.S.EVENT-U @ _PT-RANGES-OVERLAP? 0= ;

\ =====================================================================
\  Atomic UART publication and fixed-width negotiation encoding
\ =====================================================================

VARIABLE _PT-W-S
VARIABLE _PT-W-POS

: _PT-W-BEGIN  ( s -- )  _PT-W-S ! 0 _PT-W-POS ! ;

: _PT-W-C,  ( c -- )
    _PT-W-S @ _PT.S.TX-A @ _PT-W-POS @ + C!
    _PT-W-POS @ 1+ _PT-W-POS ! ;

VARIABLE _PT-W-A
VARIABLE _PT-W-U
: _PT-W-APPEND  ( a u -- )
    _PT-W-U ! _PT-W-A !
    _PT-W-A @ _PT-W-S @ _PT.S.TX-A @ _PT-W-POS @ + _PT-W-U @ CMOVE
    _PT-W-POS @ _PT-W-U @ + _PT-W-POS ! ;

: _PT-HEX-C  ( nibble -- c )
    DUP 10 U< IF 48 + ELSE 10 - 65 + THEN ;

VARIABLE _PT-W-HV
VARIABLE _PT-W-HN
: _PT-W-HEX  ( u digits -- )
    _PT-W-HN ! _PT-W-HV !
    BEGIN _PT-W-HN @ WHILE
        _PT-W-HN @ 1- 4 * _PT-W-HV @ SWAP RSHIFT 15 AND
        _PT-HEX-C _PT-W-C,
        _PT-W-HN @ 1- _PT-W-HN !
    REPEAT ;

: _PT-W-PREFIX  ( selector -- )
    27 _PT-W-C, 93 _PT-W-C,
    S" 9999;APT1;" _PT-W-APPEND
    _PT-W-C, 59 _PT-W-C, ;

: _PT-W-ST  ( -- )  27 _PT-W-C, 92 _PT-W-C, ;

: _PT-W-PUBLISH  ( -- )
    UART-ACQUIRE
    _PT-W-S @ _PT.S.TX-A @ _PT-W-POS @ TYPE TX-FLUSH
    UART-RELEASE ;

: _PT-SEND-PROBE  ( s -- )
    DUP _PT-W-BEGIN
    80 _PT-W-PREFIX                    \ P
    DUP _PT.S.NONCE @ 16 _PT-W-HEX
    S" ;CELL1" _PT-W-APPEND _PT-W-ST
    DROP _PT-W-PUBLISH ;

: _PT-SEND-OPEN  ( s -- )
    DUP _PT-W-BEGIN
    65 _PT-W-PREFIX                    \ A
    DUP _PT.S.NONCE @ 16 _PT-W-HEX 59 _PT-W-C,
    DUP _PT.S.SESSION-ID @ 16 _PT-W-HEX 59 _PT-W-C,
    DUP _PT.S.CLIENT-MAX-PAY @ 8 _PT-W-HEX 59 _PT-W-C,
    DUP _PT.S.LOCAL-GRANT @ 8 _PT-W-HEX
    S" ;CELL1" _PT-W-APPEND _PT-W-ST
    DROP _PT-W-PUBLISH ;

\ =====================================================================
\  OFFER scanner and checked negotiation admission
\ =====================================================================

VARIABLE _PT-H-A
VARIABLE _PT-H-U
VARIABLE _PT-H-V
VARIABLE _PT-H-I
: _PT-HEX@  ( a u -- value flag )
    _PT-H-U ! _PT-H-A ! 0 _PT-H-V ! 0 _PT-H-I !
    BEGIN _PT-H-I @ _PT-H-U @ U< WHILE
        _PT-H-A @ _PT-H-I @ + C@
        DUP 48 58 WITHIN IF 48 - ELSE
            DUP 65 71 WITHIN IF 65 - 10 + ELSE
                DROP 0 FALSE EXIT
            THEN
        THEN
        _PT-H-V @ 16 * + _PT-H-V !
        _PT-H-I @ 1+ _PT-H-I !
    REPEAT
    _PT-H-V @ TRUE ;

VARIABLE _PT-O-A
VARIABLE _PT-O-NONCE
VARIABLE _PT-O-SESSION
VARIABLE _PT-O-MAXPAY
VARIABLE _PT-O-MAXTX
VARIABLE _PT-O-CREDIT
VARIABLE _PT-O-COLS
VARIABLE _PT-O-ROWS

CREATE _PT-OFFER-PREFIX
    27 C, 93 C, 57 C, 57 C, 57 C, 57 C, 59 C,
    65 C, 80 C, 84 C, 49 C, 59 C, 79 C, 59 C,

: _PT-OFFER-PUNCT?  ( a -- flag )
    DUP 30 + C@ 59 =
    OVER 47 + C@ 59 = AND
    OVER 56 + C@ 59 = AND
    OVER 65 + C@ 59 = AND
    OVER 74 + C@ 59 = AND
    OVER 79 + C@ 59 = AND
    OVER 84 + C@ 59 = AND
    OVER 90 + C@ 27 = AND
    SWAP 91 + C@ 92 = AND ;

: _PT-OFFER-SYNTAX?  ( a -- flag )
    DUP _PT-O-A !
    DUP 14 _PT-OFFER-PREFIX 14 COMPARE 0= 0= IF DROP FALSE EXIT THEN
    DUP _PT-OFFER-PUNCT? 0= IF DROP FALSE EXIT THEN
    DUP 85 + 5 S" CELL1" COMPARE 0= 0= IF DROP FALSE EXIT THEN
    DUP 14 + 16 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-NONCE !
    DUP 31 + 16 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-SESSION !
    DUP 48 + 8 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-MAXPAY !
    DUP 57 + 8 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-MAXTX !
    DUP 66 + 8 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-CREDIT !
    DUP 75 + 4 _PT-HEX@ 0= IF 2DROP FALSE EXIT THEN _PT-O-COLS !
    80 + 4 _PT-HEX@ 0= IF DROP FALSE EXIT THEN _PT-O-ROWS !
    TRUE ;

VARIABLE _PT-V-S
VARIABLE _PT-V-START
VARIABLE _PT-V-AVAILABLE
VARIABLE _PT-V-ROWPAY
VARIABLE _PT-V-SNAPSHOT
: _PT-OFFER-VALID?  ( s offer-start -- flag )
    _PT-V-START ! _PT-V-S !
    _PT-O-NONCE @ _PT-V-S @ _PT.S.NONCE @ <> IF FALSE EXIT THEN
    _PT-O-SESSION @ 0= IF FALSE EXIT THEN
    _PT-O-MAXPAY @ 32 U< _PT-O-MAXPAY @ _PT-MAX-PAYLOAD U> OR IF FALSE EXIT THEN
    _PT-O-MAXTX @ 0= _PT-O-CREDIT @ 0= OR IF FALSE EXIT THEN
    _PT-O-COLS @ 0= _PT-O-ROWS @ 0= OR IF FALSE EXIT THEN
    12 _PT-O-COLS @ 8 * + DUP _PT-V-ROWPAY !
    _PT-O-MAXPAY @ U> IF FALSE EXIT THEN
    52 _PT-O-COLS @ 8 * + _PT-O-ROWS @ * 176 + DUP _PT-V-SNAPSHOT !
    _PT-O-MAXTX @ U> IF FALSE EXIT THEN
    _PT-O-CREDIT @ _PT-O-MAXTX @ U< IF FALSE EXIT THEN
    _PT-V-S @ _PT.S.RX-U @
    _PT-V-S @ _PT.S.BIN-U @ _PT-OFFER-BYTES - - DUP
    _PT-CONTROL-RESERVE _PT-HDR + 13 + U< IF DROP FALSE EXIT THEN
    _PT-CONTROL-RESERVE - DUP _PT-V-AVAILABLE ! DROP
    _PT-V-AVAILABLE @ _PT-HDR - _PT-MAX-PAYLOAD MIN
    _PT-V-S @ _PT.S.EVENT-U @ MIN DUP 32 U< IF DROP FALSE EXIT THEN
    DUP _PT-V-S @ _PT.S.CLIENT-MAX-PAY !
    12 - _PT-V-S @ _PT.S.MAX-TEXT !
    _PT-V-AVAILABLE @ 0xFFFFFFFF MIN DUP 0= IF DROP FALSE EXIT THEN
    _PT-V-S @ _PT.S.LOCAL-GRANT !
    _PT-V-ROWPAY @ _PT-HDR + _PT-V-S @ _PT.S.TX-U @ U> IF FALSE EXIT THEN
    TRUE ;

VARIABLE _PT-N-S
VARIABLE _PT-N-I
VARIABLE _PT-N-LEN
: _PT-SCAN-OFFER  ( s -- start found? )
    DUP _PT-N-S !
    DUP _PT.S.NEG-SCAN @ _PT-N-I !
    _PT.S.BIN-U @ _PT-N-LEN !
    BEGIN _PT-N-I @ 14 + _PT-N-LEN @ _PT-U<= WHILE
        _PT-N-S @ _PT.S.RX-A @ _PT-N-I @ + 14
        _PT-OFFER-PREFIX 14 COMPARE 0= IF
            _PT-N-I @ _PT-OFFER-BYTES + _PT-N-LEN @ U> IF
                _PT-N-I @ _PT-N-S @ _PT.S.NEG-SCAN !
                0 FALSE EXIT
            THEN
            _PT-N-S @ _PT.S.RX-A @ _PT-N-I @ + _PT-OFFER-SYNTAX? IF
                _PT-N-S @ _PT-N-I @ _PT-OFFER-VALID? IF
                    _PT-N-I @ TRUE EXIT
                THEN
            THEN
        THEN
        _PT-N-I @ 1+ DUP _PT-N-I ! _PT-N-S @ _PT.S.NEG-SCAN !
    REPEAT
    0 FALSE ;

: _PT-PROMOTE-LEGACY  ( s -- )
    DUP _PT.S.BIN-U @ OVER _PT.S.LEGACY-U !
    0 OVER _PT.S.LEGACY-POS !
    0 OVER _PT.S.BIN-U !
    0 SWAP _PT.S.NEG-SCAN ! ;

VARIABLE _PT-A-S
VARIABLE _PT-A-START
VARIABLE _PT-A-TOTAL
VARIABLE _PT-A-SUFFIX
: _PT-ACCEPT-OFFER  ( start s -- )
    _PT-A-S ! _PT-A-START !
    _PT-O-SESSION @ _PT-A-S @ _PT.S.SESSION-ID !
    _PT-O-MAXPAY @ _PT-A-S @ _PT.S.PEER-MAX-PAY !
    _PT-O-MAXTX @ _PT-A-S @ _PT.S.PEER-MAX-TX !
    _PT-O-CREDIT @ _PT-A-S @ _PT.S.PEER-GRANT !
    _PT-O-CREDIT @ _PT-A-S @ _PT.S.PEER-INITIAL !
    _PT-O-COLS @ _PT-A-S @ _PT.S.COLS !
    _PT-O-ROWS @ _PT-A-S @ _PT.S.ROWS !
    _PT-CAPS _PT-A-S @ _PT.S.CAPS !
    _PT-A-S @ _PT.S.BIN-U @ DUP _PT-A-TOTAL !
    _PT-A-START @ _PT-OFFER-BYTES + - DUP _PT-A-SUFFIX ! IF
        _PT-A-S @ _PT.S.RX-A @ _PT-A-START @ _PT-OFFER-BYTES + +
        _PT-A-S @ _PT.S.RX-A @ _PT-A-START @ +
        _PT-A-SUFFIX @ MOVE
    THEN
    _PT-A-TOTAL @ _PT-OFFER-BYTES - _PT-A-S @ _PT.S.LEGACY-U !
    0 _PT-A-S @ _PT.S.LEGACY-POS !
    0 _PT-A-S @ _PT.S.BIN-U !
    0 _PT-A-S @ _PT.S.NEG-SCAN !
    0 _PT-A-S @ _PT.S.TX-SEQ !
    0 _PT-A-S @ _PT.S.RX-SEQ !
    0 _PT-A-S @ _PT.S.EPOCH !
    0 _PT-A-S @ _PT.S.PEER-SENT !
    0 _PT-A-S @ _PT.S.LOCAL-RECEIVED !
    1 _PT-A-S @ _PT.S.NEXT-TXID !
    0 _PT-A-S @ _PT.S.REVISION !
    TRUE _PT-A-S @ _PT.S.SNAPSHOT? !
    _PT-A-S @ _PT-RET-RESET
    _PT-A-S @ _PT-SEND-OPEN
    PT-ST-OPENING _PT-A-S @ _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + _PT-A-S @ _PT.S.DEADLINE ! ;

VARIABLE _PT-START-S
: PT-START  ( session -- status )
    DUP _PT-VALID-S? 0= IF DROP PT-S-INVALID EXIT THEN
    DUP _PT-START-S !
    DUP _PT.S.STATE @ PT-ST-ANSI <> IF DROP PT-S-INVALID EXIT THEN
    DUP _PT.S.LEGACY-POS @ OVER _PT.S.LEGACY-U @ <> IF
        DROP PT-S-WOULD-BLOCK EXIT
    THEN
    DUP _PT.S.EVENT-PENDING @ IF DROP PT-S-WOULD-BLOCK EXIT THEN
    DUP _PT.S.BIN-U @ IF DROP PT-S-WOULD-BLOCK EXIT THEN
    0 OVER _PT.S.LEGACY-U !
    0 OVER _PT.S.LEGACY-POS !
    _PT-OWNER @ IF DROP PT-S-WOULD-BLOCK EXIT THEN
    DUP _PT-OWNER !
    MS@ OVER XOR OVER _PT.S.NONCE @ XOR 0x9E3779B97F4A7C15 XOR
    DUP 0= IF DROP 1 THEN OVER _PT.S.NONCE !
    1 OVER _PT.S.PROBES !
    0 OVER _PT.S.NEG-SCAN !
    PT-ST-PROBING OVER _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + OVER _PT.S.DEADLINE !
    _PT-SEND-PROBE
    PT-S-OK ;

\ =====================================================================
\  APT-1 frame encoder and control messages
\ =====================================================================

VARIABLE _PT-F-S
VARIABLE _PT-F-A
VARIABLE _PT-F-TYPE
VARIABLE _PT-F-PAY
VARIABLE _PT-F-TOTAL

: _PT-FRAME-BEGIN  ( type payload-u s -- status )
    _PT-F-S ! _PT-F-PAY ! _PT-F-TYPE !
    _PT-F-PAY @ _PT-MAX-PAYLOAD U> IF PT-S-INVALID EXIT THEN
    _PT-F-PAY @ _PT-F-S @ _PT.S.PEER-MAX-PAY @ U> IF PT-S-INVALID EXIT THEN
    _PT-F-PAY @ _PT-HDR + DUP _PT-F-TOTAL !
    _PT-F-S @ _PT.S.TX-U @ U> IF PT-S-INVALID EXIT THEN
    _PT-F-S @ _PT.S.TX-SEQ @ 0xFFFFFFFFFFFFFFFF =
    _PT-F-TYPE @ _PT-M-CLOSE <> AND
    _PT-F-TYPE @ _PT-M-CLOSE-ACK <> AND
    _PT-F-TYPE @ _PT-M-ERROR <> AND IF
        PT-ST-LOST _PT-F-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-F-S @ _PT.S.TX-A @ DUP _PT-F-A !
    _PT-F-TOTAL @ 0 FILL
    0x315450A5 _PT-F-A @ L!
    1 _PT-F-A @ 4 + C!
    _PT-HDR _PT-F-A @ 5 + C!
    _PT-F-TYPE @ _PT-F-A @ 6 + W!
    _PT-F-PAY @ _PT-F-A @ 12 + L!
    _PT-F-S @ _PT.S.SESSION-ID @ _PT-F-A @ 16 + _PT-U64!
    _PT-F-S @ _PT.S.TX-SEQ @ _PT-F-A @ 24 + _PT-U64!
    _PT-F-S @ _PT.S.EPOCH @ _PT-F-A @ 32 + L!
    PT-S-OK ;

VARIABLE _PT-F-DATA
: _PT-FRAME-SEND  ( data? s -- status )
    _PT-F-S ! _PT-F-DATA !
    _PT-F-DATA @ IF
        _PT-F-S @ _PT.S.PEER-SENT @ _PT-F-TOTAL @ +
        _PT-F-S @ _PT.S.PEER-SENT @ U< IF
            PT-ST-LOST _PT-F-S @ _PT.S.STATE !
            PT-S-SESSION-LOST EXIT
        THEN
    THEN
    _PT-F-A @ _PT-F-PAY @ _PT-FRAME-CRC _PT-F-A @ 36 + L!
    UART-ACQUIRE
    _PT-F-A @ _PT-F-TOTAL @ TYPE TX-FLUSH
    UART-RELEASE
    _PT-F-DATA @ IF
        _PT-F-S @ _PT.S.PEER-SENT @ _PT-F-TOTAL @ +
        _PT-F-S @ _PT.S.PEER-SENT !
    THEN
    _PT-F-S @ _PT.S.TX-SEQ @ DUP 0xFFFFFFFFFFFFFFFF <> IF
        1+ _PT-F-S @ _PT.S.TX-SEQ !
    ELSE DROP THEN
    PT-S-OK ;

: _PT-FRAME-PAYLOAD  ( -- a )  _PT-F-A @ _PT-HDR + ;

: _PT-SEND-CLIENT-READY  ( s -- status )
    DUP _PT-F-S !
    _PT-M-CLIENT-READY 32 ROT _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    1 _PT-FRAME-PAYLOAD L!
    _PT-F-S @ _PT.S.CLIENT-MAX-PAY @ _PT-FRAME-PAYLOAD 4 + L!
    _PT-F-S @ _PT.S.LOCAL-GRANT @ _PT-FRAME-PAYLOAD 12 + L!
    _PT-F-S @ _PT.S.MAX-TEXT @ _PT-FRAME-PAYLOAD 16 + L!
    _PT-CAPS _PT-FRAME-PAYLOAD 24 + _PT-U64!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

: _PT-SEND-CREDIT  ( s -- status )
    DUP _PT-F-S !
    _PT-M-CREDIT 8 ROT _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-F-S @ _PT.S.LOCAL-GRANT @ _PT-FRAME-PAYLOAD _PT-U64!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

VARIABLE _PT-CTL-REASON
: _PT-SEND-CLOSE  ( reason s -- status )
    _PT-F-S ! _PT-CTL-REASON !
    _PT-M-CLOSE 16 _PT-F-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-CTL-REASON @ _PT-FRAME-PAYLOAD W!
    _PT-F-S @ _PT.S.REVISION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

: _PT-SEND-CLOSE-ACK  ( reason s -- status )
    _PT-F-S ! _PT-CTL-REASON !
    _PT-M-CLOSE-ACK 8 _PT-F-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-CTL-REASON @ _PT-FRAME-PAYLOAD W!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

VARIABLE _PT-ABORT-ID
VARIABLE _PT-ABORT-REASON
: _PT-SEND-ABORT  ( txid reason s -- status )
    _PT-F-S ! _PT-ABORT-REASON ! _PT-ABORT-ID !
    _PT-M-TX-ABORT 16 _PT-F-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-ABORT-ID @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-ABORT-REASON @ _PT-FRAME-PAYLOAD 8 + W!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

VARIABLE _PT-RESET-EPOCH
: _PT-SEND-RESET-ACK  ( requested-epoch s -- status )
    _PT-F-S ! _PT-RESET-EPOCH !
    _PT-M-SOFT-RESET-ACK 8 _PT-F-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-RESET-EPOCH @ _PT-FRAME-PAYLOAD L!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

VARIABLE _PT-RQ-S
VARIABLE _PT-RQ-SENT
VARIABLE _PT-RQ-WATERMARK
: _PT-SEND-RET-QUERY  ( s -- status )
    _PT-RQ-S !
    _PT-RQ-S @ _PT.S.PEER-SENT @ 48 + DUP _PT-RQ-SENT !
    _PT-RQ-S @ _PT.S.PEER-SENT @ U< IF
        _PT-RQ-S @ _PT-RET-CELL-ONLY
        PT-S-OK EXIT
    THEN
    _PT-RQ-SENT @ _PT-RQ-S @ _PT.S.PEER-INITIAL @ +
    DUP _PT-RQ-WATERMARK !
    _PT-RQ-S @ _PT.S.PEER-INITIAL @ U< IF
        _PT-RQ-S @ _PT-RET-CELL-ONLY
        PT-S-OK EXIT
    THEN
    _PT-M-RET-QUERY 8 _PT-RQ-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-RET1-TAG _PT-FRAME-PAYLOAD L!
    TRUE _PT-RQ-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    _PT-RQ-SENT @ _PT-RQ-S @ _PT.S.RET-SQUERY !
    _PT-RQ-WATERMARK @ _PT-RQ-S @ _PT.S.RET-WATERMARK !
    _PT-RD-WAIT-CAPS _PT-RQ-S @ _PT.S.RET-STATE !
    PT-S-OK ;

VARIABLE _PT-ERR-CODE
VARIABLE _PT-ERR-TYPE
VARIABLE _PT-ERR-SEQ
: _PT-SEND-FATAL-ERROR  ( code type sequence s -- status )
    _PT-F-S ! _PT-ERR-SEQ ! _PT-ERR-TYPE ! _PT-ERR-CODE !
    _PT-M-ERROR 16 _PT-F-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-ERR-CODE @ _PT-FRAME-PAYLOAD W!
    2 _PT-FRAME-PAYLOAD 2 + C!
    _PT-ERR-TYPE @ _PT-FRAME-PAYLOAD 4 + W!
    _PT-ERR-SEQ @ _PT-FRAME-PAYLOAD 6 + _PT-U64!
    FALSE _PT-F-S @ _PT-FRAME-SEND ;

\ =====================================================================
\  Legacy-prefix retention and receive-buffer ownership
\ =====================================================================

: _PT-BIN-A  ( s -- a )
    DUP _PT.S.RX-A @ SWAP _PT.S.LEGACY-U @ + ;

: _PT-BIN-CAP  ( s -- u )
    DUP _PT.S.RX-U @ SWAP _PT.S.LEGACY-U @ - ;

VARIABLE _PT-L-S
VARIABLE _PT-L-OLD
VARIABLE _PT-L-GRANT
: _PT-COMPACT-LEGACY  ( s -- )
    _PT-L-S !
    _PT-L-S @ _PT.S.LEGACY-U @ DUP _PT-L-OLD ! 0= IF EXIT THEN
    _PT-L-S @ _PT.S.LEGACY-POS @ _PT-L-OLD @ <> IF EXIT THEN
    _PT-L-S @ _PT-BIN-A _PT-L-S @ _PT.S.RX-A @
    _PT-L-S @ _PT.S.BIN-U @ MOVE
    0 _PT-L-S @ _PT.S.LEGACY-U !
    0 _PT-L-S @ _PT.S.LEGACY-POS !
    _PT-L-S @ _PT.S.STATE @ DUP PT-ST-OPENING = SWAP PT-ST-ACTIVE = OR
    _PT-L-S @ _PT.S.STATE @ PT-ST-RESYNCING = OR IF
        _PT-L-S @ _PT.S.LOCAL-GRANT @ _PT-L-OLD @ + DUP _PT-L-GRANT !
        _PT-L-S @ _PT.S.LOCAL-GRANT @ U< IF
            PT-ST-LOST _PT-L-S @ _PT.S.STATE ! EXIT
        THEN
        _PT-L-GRANT @ _PT-L-S @ _PT.S.LOCAL-GRANT !
        TRUE _PT-L-S @ _PT.S.CREDIT-DIRTY? !
    THEN ;

: PT-LEGACY-POLL  ( session -- byte has-byte )
    DUP _PT-VALID-S? 0= IF DROP 0 FALSE EXIT THEN
    _PT-L-S !
    _PT-L-S @ _PT.S.STATE @ PT-ST-PROBING = IF 0 FALSE EXIT THEN
    _PT-L-S @ _PT.S.LEGACY-POS @ _PT-L-S @ _PT.S.LEGACY-U @ U< IF
        _PT-L-S @ _PT.S.RX-A @ _PT-L-S @ _PT.S.LEGACY-POS @ + C@
        _PT-L-S @ _PT.S.LEGACY-POS @ 1+
        _PT-L-S @ _PT.S.LEGACY-POS !
        _PT-L-S @ _PT-COMPACT-LEGACY TRUE EXIT
    THEN
    _PT-L-S @ _PT-COMPACT-LEGACY 0 FALSE ;

VARIABLE _PT-RD-S
: _PT-READ-BYTE  ( s -- got? )
    _PT-RD-S !
    KEY? 0= IF FALSE EXIT THEN
    _PT-RD-S @ _PT.S.BIN-U @ _PT-RD-S @ _PT-BIN-CAP _PT-U>= IF
        FALSE EXIT
    THEN
    KEY _PT-RD-S @ _PT-BIN-A _PT-RD-S @ _PT.S.BIN-U @ + C!
    _PT-RD-S @ _PT.S.BIN-U @ 1+ _PT-RD-S @ _PT.S.BIN-U !
    TRUE ;

\ =====================================================================
\  Input payload validation
\ =====================================================================

: _PT-CONTINUATION?  ( c -- flag )  0x80 0xC0 WITHIN ;

VARIABLE _PT-U8-A
VARIABLE _PT-U8-END
VARIABLE _PT-U8-B
: _PT-UTF8?  ( a u -- flag )
    DUP 0= IF 2DROP FALSE EXIT THEN
    OVER + _PT-U8-END ! _PT-U8-A !
    BEGIN _PT-U8-A @ _PT-U8-END @ U< WHILE
        _PT-U8-A @ C@ DUP _PT-U8-B !
        0x80 U< IF
            _PT-U8-A @ 1+ _PT-U8-A !
        ELSE
            _PT-U8-B @ 0xC2 0xE0 WITHIN IF
                _PT-U8-A @ 2 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xE0 = IF
                _PT-U8-A @ 3 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ 0xA0 0xC0 WITHIN 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xE1 0xED WITHIN
                 _PT-U8-B @ 0xEE 0xF0 WITHIN OR IF
                _PT-U8-A @ 3 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xED = IF
                _PT-U8-A @ 3 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ 0x80 0xA0 WITHIN 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xF0 = IF
                _PT-U8-A @ 4 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ 0x90 0xC0 WITHIN 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 4 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xF1 0xF4 WITHIN IF
                _PT-U8-A @ 4 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 4 + _PT-U8-A !
            ELSE _PT-U8-B @ 0xF4 = IF
                _PT-U8-A @ 4 + _PT-U8-END @ U> IF FALSE EXIT THEN
                _PT-U8-A @ 1+ C@ 0x80 0x90 WITHIN 0= IF FALSE EXIT THEN
                _PT-U8-A @ 2 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 3 + C@ _PT-CONTINUATION? 0= IF FALSE EXIT THEN
                _PT-U8-A @ 4 + _PT-U8-A !
            ELSE FALSE EXIT THEN THEN THEN THEN THEN THEN THEN
        THEN
    REPEAT TRUE ;

: _PT-KEY-SYMBOL?  ( u -- flag )
    DUP _PT-SCALAR? IF DROP TRUE EXIT THEN
    DUP 0x00110001 0x0011000F WITHIN IF DROP TRUE EXIT THEN
    0x00110020 0x0011002C WITHIN ;

\ =====================================================================
\  Receive-frame validation and message dispatch
\ =====================================================================

VARIABLE _PT-RX-S
VARIABLE _PT-RX-A
VARIABLE _PT-RX-P
VARIABLE _PT-RX-TYPE
VARIABLE _PT-RX-LEN
VARIABLE _PT-RX-TOTAL
VARIABLE _PT-RX-SEQNO
VARIABLE _PT-RX-DATA?

: _PT-CONTROL-TYPE?  ( type -- flag )
    DUP _PT-M-SERVER-READY = IF DROP TRUE EXIT THEN
    DUP _PT-M-CLIENT-READY = IF DROP TRUE EXIT THEN
    DUP _PT-M-CREDIT = IF DROP TRUE EXIT THEN
    DUP _PT-M-ERROR = IF DROP TRUE EXIT THEN
    DUP _PT-M-CLOSE = IF DROP TRUE EXIT THEN
    DUP _PT-M-CLOSE-ACK = IF DROP TRUE EXIT THEN
    DUP _PT-M-SOFT-RESET-REQUEST = IF DROP TRUE EXIT THEN
    DUP _PT-M-SOFT-RESET-ACK = IF DROP TRUE EXIT THEN
    DUP _PT-M-TX-ABORT = IF DROP TRUE EXIT THEN
    DUP _PT-M-RET-RESULT = IF DROP TRUE EXIT THEN
    _PT-M-TX-RESULT = ;

: _PT-INPUT-TYPE?  ( type -- flag )
    DUP _PT-M-KEY = IF DROP TRUE EXIT THEN
    DUP _PT-M-TEXT = IF DROP TRUE EXIT THEN
    DUP _PT-M-POINTER = IF DROP TRUE EXIT THEN
    DUP _PT-M-RESIZE = IF DROP TRUE EXIT THEN
    _PT-M-FOCUS = ;

: _PT-TO-ANSI  ( s -- )
    DUP _PT-RET-RESET
    DUP _PT.S.BIN-U OFF
    DUP _PT.S.EVENT-PENDING OFF
    DUP _PT-TX-CLEAR
    DUP _PT-AWAIT-CLEAR
    DUP _PT-LIFE-CLEAR
    DUP _PT-COMPLETION-CLEAR
    DUP _PT.S.RESET-PENDING? OFF
    DUP _PT.S.CREDIT-DIRTY? OFF
    DUP _PT.S.CLOSE-OPENING? OFF
    DUP PT-ST-ANSI SWAP _PT.S.STATE !
    _PT-OWNER-RELEASE ;

VARIABLE _PT-FAIL-S
VARIABLE _PT-FAIL-CODE
VARIABLE _PT-FAIL-TYPE
VARIABLE _PT-FAIL-SEQ
: _PT-FAIL-COMMON  ( -- status )
    _PT-FAIL-S @ _PT.S.STATE @ PT-ST-ANSI <> IF
        _PT-FAIL-CODE @ _PT-FAIL-TYPE @ _PT-FAIL-SEQ @ _PT-FAIL-S @
        _PT-SEND-FATAL-ERROR DROP
    THEN
    0 _PT-FAIL-S @ _PT.S.BIN-U !
    0 _PT-FAIL-S @ _PT.S.EVENT-PENDING !
    _PT-FAIL-S @ _PT-TX-CLEAR
    _PT-FAIL-S @ _PT-AWAIT-CLEAR
    _PT-FAIL-S @ _PT-LIFE-CLEAR
    _PT-FAIL-S @ _PT.S.RESET-PENDING? OFF
    _PT-FAIL-S @ _PT-RET-RESET
    PT-ST-LOST _PT-FAIL-S @ _PT.S.STATE !
    PT-S-SESSION-LOST ;

: _PT-STRUCTURAL-FAIL  ( s type sequence code -- status )
    _PT-FAIL-CODE ! _PT-FAIL-SEQ ! _PT-FAIL-TYPE ! _PT-FAIL-S !
    _PT-FAIL-COMMON ;

: _PT-SEMANTIC-FAIL  ( s type sequence code -- status )
    _PT-FAIL-CODE ! _PT-FAIL-SEQ ! _PT-FAIL-TYPE ! _PT-FAIL-S !
    _PT-FAIL-COMMON ;

VARIABLE _PT-Z-A
VARIABLE _PT-Z-U
: _PT-ZERO-BYTES?  ( a u -- flag )
    _PT-Z-U ! _PT-Z-A !
    BEGIN _PT-Z-U @ WHILE
        _PT-Z-A @ C@ IF FALSE EXIT THEN
        _PT-Z-A @ 1+ _PT-Z-A !
        _PT-Z-U @ 1- _PT-Z-U !
    REPEAT TRUE ;

: _PT-READY-PAYLOAD?  ( s -- flag )
    _PT-RX-LEN @ 32 <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ L@ 1 <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 4 + L@ OVER _PT.S.PEER-MAX-PAY @ <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 8 + L@ OVER _PT.S.PEER-MAX-TX @ <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 12 + L@ OVER _PT.S.PEER-GRANT @ <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 16 + L@ OVER _PT.S.COLS @ <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 20 + L@ OVER _PT.S.ROWS @ <> IF DROP FALSE EXIT THEN
    _PT-RX-P @ 24 + _PT-U64@ _PT-CAPS = SWAP DROP ;

: _PT-DISPATCH-READY  ( s -- status )
    DUP _PT.S.STATE @ PT-ST-OPENING = IF
        _PT-RX-SEQNO @ 0<> IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DUP _PT-READY-PAYLOAD? 0= IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DUP _PT-SEND-CLIENT-READY ?DUP IF NIP EXIT THEN
        PT-ST-ACTIVE SWAP _PT.S.STATE ! PT-S-OK EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF
        DUP _PT.S.CLOSE-OPENING? @ 0=
        _PT-RX-SEQNO @ 0<> OR IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DUP _PT-READY-PAYLOAD? 0= IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DROP PT-S-OK EXIT
    THEN
    6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL ;

VARIABLE _PT-CR-GRANT

\ RETAINED-1 cannot become caller-visible while a legacy CELL transaction is
\ open or awaiting its result.  Otherwise that transaction can finish after
\ discovery, materialize a completion with no retained-engine owner, and
\ strand the first PRESENT.  Covering credit is remembered in PEER-GRANT and
\ activation is retried after service settles the legacy authority.
: _PT-RET-ACTIVATE-READY  ( s -- )
    DUP _PT.S.RET-STATE @ _PT-RD-WAIT-CREDIT <> IF DROP EXIT THEN
    DUP _PT.S.PEER-GRANT @ OVER _PT.S.RET-WATERMARK @ U< IF DROP EXIT THEN
    DUP _PT.S.TX-OPEN? @ OVER _PT-RESULT-BUSY? OR IF DROP EXIT THEN
    DUP _PT-RB-REPLACE-REQUIRED SWAP _PT.S.RET-REBUILD !
    _PT-RD-AVAILABLE SWAP _PT.S.RET-STATE ! ;

: _PT-DISPATCH-CREDIT  ( s -- status )
    _PT-RX-LEN @ 8 <> IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ _PT-U64@ DUP _PT-CR-GRANT !
    OVER _PT.S.PEER-GRANT @ U< IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-CR-GRANT @ OVER _PT.S.PEER-GRANT !
    DUP _PT.S.RET-STATE @ DUP _PT-RD-WAIT-CAPS _PT-U>=
    SWAP _PT-RD-INVALID _PT-U<= AND IF
        _PT-CR-GRANT @ OVER _PT.S.RET-WATERMARK @ _PT-U>= IF
            DUP _PT.S.RET-STATE @ _PT-RD-WAIT-CREDIT = IF
                \ A resize may have been ordered after the exact reply pair
                \ but before this covering credit.  In that case the profile
                \ is still in its initial replacement phase; the accepted
                \ geometry is already held in the session.
                _PT-RET-ACTIVATE-READY
            ELSE
                _PT-RET-CELL-ONLY
            THEN
            PT-S-OK EXIT
        THEN
    THEN
    DROP PT-S-OK ;

VARIABLE _PT-CMP-S
VARIABLE _PT-CMP-REQUEST
VARIABLE _PT-CMP-STATUS
VARIABLE _PT-CMP-DETAIL
VARIABLE _PT-CMP-TXID
VARIABLE _PT-CMP-REVISION
VARIABLE _PT-CMP-OWNER
VARIABLE _PT-CMP-GENERATION
VARIABLE _PT-CMP-ITEM
VARIABLE _PT-CMP-ACCEPTED

: _PT-COMPLETE-TX!  ( request status txid revision owner generation s -- )
    _PT-CMP-S ! _PT-CMP-GENERATION ! _PT-CMP-OWNER !
    _PT-CMP-REVISION ! _PT-CMP-TXID ! _PT-CMP-STATUS ! _PT-CMP-REQUEST !
    _PT-CMP-S @ _PT-COMPLETION-CLEAR
    PT-COMPLETE-TX _PT-CMP-S @ _PT.S.COMP-KIND !
    _PT-CMP-REQUEST @ _PT-CMP-S @ _PT.S.COMP-REQUEST !
    _PT-CMP-STATUS @ _PT-CMP-S @ _PT.S.COMP-STATUS !
    _PT-CMP-TXID @ _PT-CMP-S @ _PT.S.COMP-TXID !
    _PT-CMP-REVISION @ _PT-CMP-S @ _PT.S.COMP-REVISION !
    _PT-CMP-OWNER @ _PT-CMP-S @ _PT.S.COMP-OWNER !
    _PT-CMP-GENERATION @ _PT-CMP-S @ _PT.S.COMP-GENERATION !
    TRUE _PT-CMP-S @ _PT.S.COMPLETE? ! ;

: _PT-COMPLETE-RET!  ( request status detail revision owner generation
                        item accepted-bytes s -- )
    _PT-CMP-S ! _PT-CMP-ACCEPTED ! _PT-CMP-ITEM !
    _PT-CMP-GENERATION ! _PT-CMP-OWNER ! _PT-CMP-REVISION !
    _PT-CMP-DETAIL ! _PT-CMP-STATUS ! _PT-CMP-REQUEST !
    _PT-CMP-S @ _PT-COMPLETION-CLEAR
    PT-COMPLETE-RET _PT-CMP-S @ _PT.S.COMP-KIND !
    _PT-CMP-REQUEST @ _PT-CMP-S @ _PT.S.COMP-REQUEST !
    _PT-CMP-STATUS @ _PT-CMP-S @ _PT.S.COMP-STATUS !
    _PT-CMP-DETAIL @ _PT-CMP-S @ _PT.S.COMP-DETAIL !
    _PT-CMP-REVISION @ _PT-CMP-S @ _PT.S.COMP-REVISION !
    _PT-CMP-OWNER @ _PT-CMP-S @ _PT.S.COMP-OWNER !
    _PT-CMP-GENERATION @ _PT-CMP-S @ _PT.S.COMP-GENERATION !
    _PT-CMP-ITEM @ _PT-CMP-S @ _PT.S.COMP-ITEM !
    _PT-CMP-ACCEPTED @ _PT-CMP-S @ _PT.S.COMP-ACCEPTED !
    TRUE _PT-CMP-S @ _PT.S.COMPLETE? ! ;

: _PT-PRESENT-REBUILD-COMMITTED  ( s -- )
    DUP _PT.S.AWAIT-RET-MODE @ DUP PT-RET-REPLACE-START =
    SWAP PT-RET-REPLACE-CONTINUE = OR IF
        DUP _PT.S.AWAIT-DISPOSITION @ PT-COMMIT-AND-REVEAL = IF
            _PT-RB-NONE SWAP _PT.S.RET-REBUILD !
        ELSE
            _PT-RB-REPLACE-PENDING SWAP _PT.S.RET-REBUILD !
        THEN EXIT
    THEN
    DUP _PT.S.AWAIT-RET-MODE @ DUP PT-RET-LAYOUT-START =
    SWAP PT-RET-LAYOUT-CONTINUE = OR IF
        DUP _PT.S.AWAIT-DISPOSITION @ PT-COMMIT-AND-REVEAL = IF
            _PT-RB-NONE SWAP _PT.S.RET-REBUILD !
        ELSE
            _PT-RB-LAYOUT-PENDING SWAP _PT.S.RET-REBUILD !
        THEN EXIT
    THEN
    DROP ;

VARIABLE _PT-RES-EXPECTED
VARIABLE _PT-RES-STATUS
VARIABLE _PT-RES-REVISION
VARIABLE _PT-RES-KIND

: _PT-RESULT-FAIL  ( s -- status )
    7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL ;

: _PT-RECORD-TX-COMPLETION  ( s -- )
    DUP _PT.S.AWAIT-KIND @ _PT-AWAIT-CELL =
    OVER _PT.S.RET-STATE @ _PT-RD-AVAILABLE = AND IF
        >R
        PT-REQUEST-TX-COMMIT _PT-RES-STATUS @
        R@ _PT.S.AWAIT-TXID @ _PT-RES-REVISION @ 0 0 R>
        _PT-COMPLETE-TX! EXIT
    THEN
    DUP _PT.S.AWAIT-KIND @ _PT-AWAIT-PRESENT = IF
        >R
        PT-REQUEST-PRESENT-COMMIT _PT-RES-STATUS @
        R@ _PT.S.AWAIT-TXID @ _PT-RES-REVISION @ 0 0 R>
        _PT-COMPLETE-TX! EXIT
    THEN
    DUP _PT.S.AWAIT-KIND @ _PT-AWAIT-OWNER-DROP = IF
        >R
        PT-REQUEST-OWNER-DROP _PT-RES-STATUS @
        R@ _PT.S.AWAIT-TXID @ _PT-RES-REVISION @
        R@ _PT.S.AWAIT-OWNER @ R@ _PT.S.AWAIT-GENERATION @ R>
        _PT-COMPLETE-TX! EXIT
    THEN
    DROP ;

: _PT-DISPATCH-TX-RESULT  ( s -- status )
    DUP _PT-CMP-S !
    _PT-RX-LEN @ 20 <> IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ 10 + W@ 0<> IF _PT-RESULT-FAIL EXIT THEN
    DUP _PT.S.AWAIT? @ 0= IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ _PT-U64@ OVER _PT.S.AWAIT-TXID @ <> IF
        _PT-RESULT-FAIL EXIT
    THEN
    _PT-RX-P @ 8 + W@ DUP 3 U> IF DROP _PT-RESULT-FAIL EXIT THEN
    DUP _PT-RES-STATUS ! DROP
    _PT-RX-P @ 12 + _PT-U64@ _PT-RES-REVISION !
    DUP _PT.S.AWAIT-KIND @ _PT-RES-KIND !

    DUP _PT.S.RESET-PENDING? @
    _PT-RES-STATUS @ PT-TX-RESULT-ABORTED <> AND IF
        _PT-RESULT-FAIL EXIT
    THEN

    _PT-RES-STATUS @ 0= IF
        _PT-RES-KIND @ _PT-AWAIT-SNAPSHOT = IF
            1 _PT-RES-EXPECTED !
        ELSE
            DUP _PT.S.REVISION @ DUP 0xFFFFFFFFFFFFFFFF = IF
                DROP _PT-RESULT-FAIL EXIT
            THEN
            1+ _PT-RES-EXPECTED !
        THEN
        _PT-RES-REVISION @ _PT-RES-EXPECTED @ <> IF
            _PT-RESULT-FAIL EXIT
        THEN
        _PT-RES-EXPECTED @ OVER _PT.S.REVISION !
        _PT-RES-KIND @ _PT-AWAIT-SNAPSHOT = IF
            FALSE OVER _PT.S.SNAPSHOT? !
            DUP _PT.S.STATE @ PT-ST-CLOSING <> IF
                PT-ST-ACTIVE OVER _PT.S.STATE !
            THEN
        THEN
        _PT-RES-KIND @ _PT-AWAIT-PRESENT = IF
            DUP _PT-PRESENT-REBUILD-COMMITTED
            DUP _PT.S.AWAIT-CELL-MODE @ PT-CELL-REPLACE = IF
                FALSE OVER _PT.S.SNAPSHOT? !
                DUP _PT.S.STATE @ PT-ST-CLOSING <> IF
                    PT-ST-ACTIVE OVER _PT.S.STATE !
                THEN
            THEN
        THEN
        DUP _PT-RECORD-TX-COMPLETION
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN

    _PT-RES-REVISION @ OVER _PT.S.REVISION @ <> IF
        _PT-RESULT-FAIL EXIT
    THEN
    _PT-RES-STATUS @ 1 = IF
        DUP _PT.S.RESET-PENDING? @ 0= IF _PT-RESULT-FAIL EXIT THEN
        DUP _PT-RECORD-TX-COMPLETION
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN
    _PT-RES-KIND @ _PT-AWAIT-PRESENT =
    OVER _PT.S.AWAIT-CELL-MODE @ PT-CELL-NONE = AND IF
        DUP _PT-RECORD-TX-COMPLETION
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN
    _PT-RES-KIND @ _PT-AWAIT-OWNER-DROP = IF
        DUP _PT-RECORD-TX-COMPLETION
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN
    _PT-RESULT-FAIL ;

: _PT-DISPATCH-RET-RESULT  ( s -- status )
    DUP _PT-CMP-S !
    _PT-RX-LEN @ 48 <> IF _PT-RESULT-FAIL EXIT THEN
    DUP _PT.S.LIFE-AWAIT? @ 0= IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ W@ OVER _PT.S.LIFE-TYPE @ <> IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ 2 + W@ DUP 3 U> IF DROP _PT-RESULT-FAIL EXIT THEN
    _PT-RES-STATUS !
    _PT-RX-P @ 4 + L@ _PT-CMP-DETAIL !
    _PT-CMP-DETAIL @ 0<> IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ 8 + _PT-U64@ OVER _PT.S.LIFE-OWNER @ <> IF
        _PT-RESULT-FAIL EXIT
    THEN
    _PT-RX-P @ 16 + _PT-U64@ OVER _PT.S.LIFE-GENERATION @ <> IF
        _PT-RESULT-FAIL EXIT
    THEN
    _PT-RX-P @ 24 + _PT-U64@ 0<> IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ 32 + _PT-U64@ DUP _PT-RES-REVISION !
    OVER _PT.S.REVISION @ <> IF _PT-RESULT-FAIL EXIT THEN
    _PT-RX-P @ 40 + _PT-U64@ 0<> IF _PT-RESULT-FAIL EXIT THEN
    >R
    R@ _PT.S.LIFE-TYPE @ _PT-RES-STATUS @ _PT-CMP-DETAIL @
    _PT-RES-REVISION @ R@ _PT.S.LIFE-OWNER @
    R@ _PT.S.LIFE-GENERATION @ 0 0 R@ _PT-COMPLETE-RET!
    R> _PT-LIFE-CLEAR PT-S-OK ;

VARIABLE _PT-SR-S
VARIABLE _PT-SR-EPOCH
VARIABLE _PT-SR-GRANT
: _PT-DISCARD-PENDING-EVENT  ( s -- status )
    DUP _PT-SR-S !
    _PT.S.EVENT-PENDING @ 0= IF PT-S-OK EXIT THEN
    _PT-SR-S @ _PT.S.LOCAL-GRANT @
    _PT-SR-S @ _PT.S.EVENT-CHARGE @ + DUP _PT-SR-GRANT !
    _PT-SR-S @ _PT.S.LOCAL-GRANT @ U< IF
        PT-ST-LOST _PT-SR-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-SR-GRANT @ _PT-SR-S @ _PT.S.LOCAL-GRANT !
    0 _PT-SR-S @ _PT.S.EVENT-PENDING !
    TRUE _PT-SR-S @ _PT.S.CREDIT-DIRTY? !
    PT-S-OK ;

: _PT-ABORT-OPEN-RAW  ( reason s -- status )
    DUP _PT.S.TX-OPEN? @ 0= IF 2DROP PT-S-OK EXIT THEN
    DUP _PT-SR-S ! SWAP _PT-ABORT-REASON !
    DUP _PT.S.TXID @ _PT-ABORT-REASON @ ROT _PT-SEND-ABORT
    DUP IF EXIT THEN DROP
    _PT-SR-S @ _PT-TX-CLEAR
    PT-S-OK ;

: _PT-RESET-CLOSE  ( s -- status )
    DUP _PT-SR-S !
    4 OVER _PT-SEND-CLOSE ?DUP IF NIP EXIT THEN
    4 _PT-SR-S @ _PT.S.CLOSE-REASON !
    FALSE _PT-SR-S @ _PT.S.CLOSE-OPENING? !
    _PT-SR-S @ _PT-TX-CLEAR
    0 _PT-SR-S @ _PT.S.EVENT-PENDING !
    _PT-SR-S @ _PT-RET-RESET
    PT-ST-CLOSING _PT-SR-S @ _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + _PT-SR-S @ _PT.S.DEADLINE !
    DROP PT-S-OK ;

: _PT-APPLY-PENDING-RESET  ( s -- status )
    DUP _PT-SR-S !
    DUP _PT.S.RESET-PENDING? @ 0= IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.TX-OPEN? @ OVER _PT.S.AWAIT? @ OR
    OVER _PT.S.LIFE-AWAIT? @ OR OVER _PT.S.COMPLETE? @ OR IF
        DROP PT-S-OK EXIT
    THEN
    DUP _PT.S.RESET-EPOCH @ OVER _PT.S.EPOCH !
    DUP _PT.S.RESET-PENDING? OFF
    DUP _PT.S.RESET-EPOCH OFF
    0 OVER _PT.S.REVISION !
    1 OVER _PT.S.NEXT-TXID !
    TRUE OVER _PT.S.SNAPSHOT? !
    DUP _PT-AWAIT-CLEAR
    DUP _PT-LIFE-CLEAR
    DUP _PT-RET-RESET
    PT-ST-RESYNCING OVER _PT.S.STATE !
    DUP _PT.S.EPOCH @ SWAP _PT-SEND-RESET-ACK ;

: _PT-DISPATCH-SOFT-RESET  ( s -- status )
    DUP _PT-SR-S !
    _PT-RX-LEN @ 16 <> IF
        4 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 4 + L@ 0<> IF
        4 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.STATE @ DUP PT-ST-ACTIVE <>
    SWAP PT-ST-CLOSING <> AND IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.EPOCH @ DUP 0xFFFFFFFF = IF
        DROP 4 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    1+ DUP _PT-SR-EPOCH ! _PT-RX-P @ L@ <> IF
        4 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 8 + _PT-U64@ OVER _PT.S.REVISION @ <> IF
        4 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.RESET-PENDING? @ IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-SR-S @ _PT.S.TX-SEQ @
    _PT-SR-S @ _PT.S.TX-OPEN? @ IF
        0xFFFFFFFFFFFFFFFC
    ELSE
        0xFFFFFFFFFFFFFFFD
    THEN U> IF _PT-RESET-CLOSE EXIT THEN
    DUP _PT-DISCARD-PENDING-EVENT ?DUP IF NIP EXIT THEN
    0 OVER _PT-ABORT-OPEN-RAW ?DUP IF NIP EXIT THEN
    _PT-SR-EPOCH @ OVER _PT.S.RESET-EPOCH !
    TRUE OVER _PT.S.RESET-PENDING? !
    _PT-APPLY-PENDING-RESET ;

: _PT-DISPATCH-CLOSE  ( s -- status )
    _PT-RX-LEN @ 16 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 2 + 6 _PT-ZERO-BYTES? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ W@ OVER _PT-SEND-CLOSE-ACK ?DUP IF NIP EXIT THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF DROP PT-S-OK EXIT THEN
    _PT-TO-ANSI PT-S-OK ;

: _PT-DISPATCH-CLOSE-ACK  ( s -- status )
    _PT-RX-LEN @ 8 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 2 + 6 _PT-ZERO-BYTES? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING <>
    _PT-RX-P @ W@ _PT-RX-S @ _PT.S.CLOSE-REASON @ <> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-TO-ANSI PT-S-OK ;

VARIABLE _PT-ER-EFFECT
VARIABLE _PT-ER-CODE
: _PT-DISPATCH-ERROR  ( s -- status )
    _PT-RX-LEN @ 16 U< IF
        1 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ W@ DUP _PT-ER-CODE ! DUP 1 U< SWAP 11 U> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 3 + C@ _PT-RX-P @ 14 + W@ 240 U> OR
    _PT-RX-P @ 14 + W@ 16 + _PT-RX-LEN @ <> OR IF
        1 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 14 + W@ ?DUP IF
        _PT-RX-P @ 16 + SWAP _PT-UTF8? 0= IF
            1 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    _PT-RX-P @ 2 + C@ DUP _PT-ER-EFFECT !
    2 U> IF 6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT THEN
    DUP _PT.S.STATE @ PT-ST-OPENING = _PT-ER-EFFECT @ 2 <> AND IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-ER-CODE @ 5 _PT-U<=
    _PT-ER-CODE @ 10 = OR _PT-ER-CODE @ 11 = OR IF
        _PT-ER-EFFECT @ 2 <> IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = _PT-ER-EFFECT @ 2 <> AND IF
        DROP PT-S-OK EXIT
    THEN
    _PT-ER-EFFECT @ 0= IF
        _PT-ER-CODE @ 6 =
        OVER _PT.S.TX-OPEN? @ OR
        OVER _PT.S.AWAIT? @ OR IF
            7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DROP PT-S-OK EXIT
    THEN
    _PT-ER-EFFECT @ 1 = IF
        DUP _PT.S.AWAIT? @ IF
            7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DUP _PT.S.TX-OPEN? @ 0= IF
            7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        DUP _PT-TX-CLEAR
        _PT-AWAIT-CLEAR PT-S-OK EXIT
    THEN
    7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL ;

VARIABLE _PT-REL-S
VARIABLE _PT-REL-U
: _PT-RELEASE-DATA  ( frame-bytes s -- status )
    _PT-REL-S ! _PT-REL-U !
    _PT-REL-S @ _PT.S.LOCAL-GRANT @ _PT-REL-U @ + DUP
    _PT-REL-S @ _PT.S.LOCAL-GRANT @ U< IF
        DROP
        PT-ST-LOST _PT-REL-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-REL-S @ _PT.S.LOCAL-GRANT !
    TRUE _PT-REL-S @ _PT.S.CREDIT-DIRTY? ! PT-S-OK ;

\ =====================================================================
\  RETAINED-1 fixed discovery records and cross-field admission
\ =====================================================================

: _PT-POSITIVE-EXACT?  ( value feature-present? -- flag )
    IF 0<> ELSE 0= THEN ;

VARIABLE _PT-RV-S
VARIABLE _PT-RV-P
VARIABLE _PT-RV-FEATURES
VARIABLE _PT-RV-RETMAX
VARIABLE _PT-RV-ROWBYTES
VARIABLE _PT-RV-TOTAL

: _PT-RET-CAPS-VALID?  ( s -- flag )
    _PT-RV-S ! _PT-RX-P @ _PT-RV-P !
    _PT-RX-LEN @ 64 <> IF FALSE EXIT THEN
    _PT-RV-P @ L@ _PT-RET1-TAG <> IF FALSE EXIT THEN
    _PT-RV-P @ 4 + W@ 1 <> _PT-RV-P @ 6 + W@ 0<> OR IF
        FALSE EXIT
    THEN
    _PT-RV-P @ 8 + _PT-U64@ DUP _PT-RV-FEATURES !
    DUP 0x3F INVERT AND IF DROP FALSE EXIT THEN
    DUP 1 AND 0= IF DROP FALSE EXIT THEN
    DUP 0x10 AND SWAP 0x08 AND 0= AND IF FALSE EXIT THEN

    _PT-RV-P @ 16 + L@ 0= _PT-RV-P @ 20 + L@ 0= OR IF FALSE EXIT THEN
    _PT-RV-P @ 20 + L@ _PT-RV-P @ 16 + L@ U> IF FALSE EXIT THEN
    _PT-RV-P @ 24 + L@ 0= _PT-RV-P @ 40 + L@ 0= OR IF FALSE EXIT THEN
    _PT-RV-P @ 48 + _PT-U64@ DUP _PT-RV-RETMAX ! 0= IF FALSE EXIT THEN
    _PT-RV-RETMAX @ 248 U< IF FALSE EXIT THEN
    _PT-RV-RETMAX @ _PT-RV-S @ _PT.S.PEER-MAX-TX @ U> IF FALSE EXIT THEN
    _PT-RV-S @ _PT.S.PEER-MAX-PAY @ 64 U<
    _PT-RV-S @ _PT.S.CLIENT-MAX-PAY @ 64 U< OR IF FALSE EXIT THEN
    _PT-RV-S @ _PT.S.TX-U @ 104 U< IF FALSE EXIT THEN

    _PT-RV-P @ 32 + L@
    _PT-RV-FEATURES @ 0x1E AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RV-P @ 36 + L@
    _PT-RV-FEATURES @ 0x10 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RV-P @ 28 + L@
    _PT-RV-FEATURES @ 0x04 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RV-P @ 44 + L@
    _PT-RV-FEATURES @ 0x04 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RV-P @ 56 + _PT-U64@
    _PT-RV-FEATURES @ 0x04 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN

    _PT-RV-FEATURES @ 0x04 AND IF
        _PT-RV-S @ _PT.S.PEER-MAX-PAY @ 80 U< IF FALSE EXIT THEN
        _PT-RV-P @ 44 + L@ 32 +
        _PT-RV-S @ _PT.S.PEER-MAX-PAY @ U> IF FALSE EXIT THEN
    THEN

    12 _PT-RV-S @ _PT.S.COLS @ 8 * +
    _PT-RV-S @ _PT.S.PEER-MAX-PAY @ U> IF FALSE EXIT THEN
    52 _PT-RV-S @ _PT.S.COLS @ 8 * + DUP _PT-RV-ROWBYTES !
    _PT-RV-S @ _PT.S.ROWS @ UM* DUP IF
        2DROP FALSE EXIT
    THEN DROP
    DUP 0xFFFFFFFFFFFFFFFF 216 - U> IF DROP FALSE EXIT THEN
    216 + DUP _PT-RV-TOTAL !
    _PT-RV-RETMAX @ U> IF FALSE EXIT THEN
    _PT-RV-TOTAL @ _PT-RV-S @ _PT.S.PEER-MAX-TX @ U> IF FALSE EXIT THEN
    TRUE ;

VARIABLE _PT-RF-CAPS
VARIABLE _PT-RF-FORMATS
VARIABLE _PT-RF-PIXELS

: _PT-RET-FORMATS-VALID?  ( s -- flag )
    DUP _PT-RV-S !
    DUP _PT.S.RET-CAPS _PT-RF-CAPS !
    DROP _PT-RX-P @ _PT-RF-FORMATS !
    _PT-RX-LEN @ 64 <> IF FALSE EXIT THEN
    _PT-RF-FORMATS @ L@ 1 <>
    _PT-RF-FORMATS @ 4 + L@ 1 <> OR IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 8 + L@ 1 U> IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 56 + _PT-U64@ IF FALSE EXIT THEN
    _PT-RF-CAPS @ 8 + _PT-U64@ _PT-RV-FEATURES !
    _PT-RF-CAPS @ 48 + _PT-U64@ _PT-RV-RETMAX !

    _PT-RF-FORMATS @ 8 + L@
    _PT-RV-FEATURES @ 0x04 AND 0<> IF 1 = ELSE 0= THEN
    0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 12 + L@
    _PT-RV-FEATURES @ 0x04 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 16 + L@
    _PT-RV-FEATURES @ 0x04 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 20 + L@
    _PT-RV-FEATURES @ 0x02 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 24 + L@
    _PT-RV-FEATURES @ 0x08 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 48 + _PT-U64@
    _PT-RV-FEATURES @ 0x08 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 28 + L@
    _PT-RV-FEATURES @ 0x10 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 32 + L@
    _PT-RV-FEATURES @ 0x10 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 40 + _PT-U64@
    _PT-RV-FEATURES @ 0x10 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN
    _PT-RF-FORMATS @ 36 + L@
    _PT-RV-FEATURES @ 0x20 AND 0<> _PT-POSITIVE-EXACT? 0= IF FALSE EXIT THEN

    _PT-RV-FEATURES @ 0x08 AND IF
        _PT-RF-FORMATS @ 48 + _PT-U64@
        _PT-RF-FORMATS @ 24 + L@ U< IF FALSE EXIT THEN
        _PT-RV-S @ _PT.S.PEER-MAX-PAY @ 112 U< IF FALSE EXIT THEN
        _PT-RF-FORMATS @ 24 + L@ 104 +
        _PT-RV-S @ _PT.S.PEER-MAX-PAY @ U> IF FALSE EXIT THEN
        _PT-RF-FORMATS @ 24 + L@ 304 + 312 MAX
        _PT-RV-RETMAX @ U> IF FALSE EXIT THEN
    THEN
    _PT-RV-FEATURES @ 0x02 AND IF
        _PT-RF-FORMATS @ 20 + L@ 8 * 80 +
        DUP _PT-RV-S @ _PT.S.PEER-MAX-PAY @ U> IF DROP FALSE EXIT THEN
        200 + _PT-RV-RETMAX @ U> IF FALSE EXIT THEN
    THEN
    _PT-RV-FEATURES @ 0x04 AND IF
        _PT-RF-FORMATS @ 12 + L@ _PT-RF-FORMATS @ 16 + L@ UM*
        DUP IF 2DROP FALSE EXIT THEN DROP DUP _PT-RF-PIXELS !
        0x3FFFFFFFFFFFFFFF U> IF FALSE EXIT THEN
        _PT-RF-PIXELS @ 4 * _PT-RF-CAPS @ 56 + _PT-U64@ U> IF
            FALSE EXIT
        THEN
        _PT-RV-RETMAX @ 280 U< IF FALSE EXIT THEN
    THEN
    _PT-RV-FEATURES @ 0x10 AND IF
        _PT-RF-FORMATS @ 28 + L@ DUP
        _PT-RF-FORMATS @ 32 + L@ U> IF DROP FALSE EXIT THEN
        DROP
        _PT-RF-FORMATS @ 32 + L@
        _PT-RF-FORMATS @ 40 + _PT-U64@ U> IF FALSE EXIT THEN
        _PT-RV-S @ _PT.S.PEER-MAX-PAY @ 112 U< IF FALSE EXIT THEN
        _PT-RF-FORMATS @ 28 + L@ 16 * 40 +
        DUP _PT-RV-S @ _PT.S.PEER-MAX-PAY @ U> IF DROP FALSE EXIT THEN
        200 + 312 MAX _PT-RV-RETMAX @ U> IF FALSE EXIT THEN
    THEN
    TRUE ;

: _PT-DISPATCH-RET-CAPS  ( s -- status )
    DUP _PT.S.RET-STATE @ _PT-RD-WAIT-CAPS = IF
        DUP _PT-RET-CAPS-VALID? IF
            _PT-RX-P @ OVER _PT.S.RET-CAPS 64 MOVE
            _PT-RD-WAIT-FORMATS OVER _PT.S.RET-STATE !
        ELSE
            DUP _PT-RET-INVALIDATE
        THEN
    ELSE
        DUP _PT.S.RET-STATE @ DUP _PT-RD-WAIT-FORMATS =
        SWAP _PT-RD-WAIT-CREDIT = OR IF DUP _PT-RET-INVALIDATE THEN
    THEN
    _PT-RX-TOTAL @ SWAP _PT-RELEASE-DATA ;

: _PT-DISPATCH-RET-FORMATS  ( s -- status )
    DUP _PT.S.RET-STATE @ _PT-RD-WAIT-FORMATS = IF
        DUP _PT-RET-FORMATS-VALID? IF
            _PT-RX-P @ OVER _PT.S.RET-FORMATS 64 MOVE
            _PT-RD-WAIT-CREDIT OVER _PT.S.RET-STATE !
        ELSE
            DUP _PT-RET-INVALIDATE
        THEN
    ELSE
        DUP _PT.S.RET-STATE @ DUP _PT-RD-WAIT-CAPS =
        SWAP _PT-RD-WAIT-CREDIT = OR IF DUP _PT-RET-INVALIDATE THEN
    THEN
    _PT-RX-TOTAL @ SWAP _PT-RELEASE-DATA ;

: _PT-RET-CHECK-ADJACENCY  ( s -- )
    DUP _PT.S.RET-STATE @ _PT-RD-WAIT-FORMATS =
    _PT-RX-TYPE @ _PT-M-RET-FORMATS <> AND IF
        _PT-RET-INVALIDATE
    ELSE
        DROP
    THEN ;

VARIABLE _PT-EV-S
: _PT-ACCEPT-EVENT  ( s -- status )
    DUP _PT-EV-S !
    _PT-RX-LEN @ OVER _PT.S.EVENT-U @ U> IF
        11 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF
        _PT-RX-TOTAL @ SWAP _PT-RELEASE-DATA EXIT
    THEN
    _PT-RX-P @ OVER _PT.S.EVENT-A @ _PT-RX-LEN @ MOVE
    _PT-RX-TYPE @ OVER _PT.S.EVENT-TYPE !
    _PT-RX-LEN @ OVER _PT.S.EVENT-LEN !
    _PT-RX-TOTAL @ OVER _PT.S.EVENT-CHARGE !
    TRUE SWAP _PT.S.EVENT-PENDING ! PT-S-OK ;

: _PT-INPUT-STATE?  ( s -- flag )
    _PT.S.STATE @ DUP PT-ST-ACTIVE = SWAP PT-ST-CLOSING = OR ;

: _PT-DISPATCH-KEY  ( s -- status )
    DUP _PT-INPUT-STATE? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-LEN @ 16 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ L@ _PT-KEY-SYMBOL? 0=
    _PT-RX-P @ 4 + C@ DUP 1 U< SWAP 3 U> OR OR
    _PT-RX-P @ 5 + C@ 3 U> OR
    _PT-RX-P @ 6 + W@ 0x3F INVERT AND 0<> OR
    _PT-RX-P @ 8 + _PT-U64@ _PT-RX-S @ _PT.S.REVISION @ <> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-ACCEPT-EVENT ;

: _PT-DISPATCH-TEXT  ( s -- status )
    DUP _PT-INPUT-STATE? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-LEN @ 12 _PT-U<= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ W@ 1 INVERT AND 0<> _PT-RX-P @ 2 + W@ 0<> OR
    _PT-RX-P @ 4 + _PT-U64@ _PT-RX-S @ _PT.S.REVISION @ <> OR
    _PT-RX-LEN @ 12 - _PT-RX-S @ _PT.S.MAX-TEXT @ U> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 12 + _PT-RX-LEN @ 12 - _PT-UTF8? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-ACCEPT-EVENT ;

: _PT-DISPATCH-POINTER  ( s -- status )
    DUP _PT-INPUT-STATE? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-LEN @ 28 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 8 + W@ 0x1F INVERT AND 0<>
    _PT-RX-P @ 10 + W@ 0x1F INVERT AND 0<> OR
    _PT-RX-P @ 12 + W@ 0x3F INVERT AND 0<> OR
    _PT-RX-P @ 14 + W@ DUP 1 U< SWAP 4 U> OR OR
    _PT-RX-P @ 20 + _PT-U64@ _PT-RX-S @ _PT.S.REVISION @ <> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 14 + W@ 4 <> IF
        _PT-RX-P @ 16 + W@ _PT-RX-P @ 18 + W@ OR IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    _PT-ACCEPT-EVENT ;

VARIABLE _PT-RSZ-COLS
VARIABLE _PT-RSZ-ROWS
VARIABLE _PT-RSZ-GEN
VARIABLE _PT-RSZ-ROWBYTES
VARIABLE _PT-RSZ-SNAPSHOT
VARIABLE _PT-RSZ-BASE
: _PT-RESIZE-STATE?  ( s -- flag )
    _PT.S.STATE @ DUP PT-ST-ACTIVE =
    OVER PT-ST-RESYNCING = OR
    SWAP PT-ST-CLOSING = OR ;

: _PT-DISPATCH-RESIZE  ( s -- status )
    DUP _PT-RESIZE-STATE? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-LEN @ 16 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ L@ DUP _PT-RSZ-COLS ! DUP 0= IF
        DROP 8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN DROP
    _PT-RX-P @ 4 + L@ DUP _PT-RSZ-ROWS ! DUP 0= IF
        DROP 8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN DROP
    _PT-RX-P @ 8 + _PT-U64@ _PT-RSZ-GEN !
    _PT-RSZ-GEN @ 0= IF
        8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.GEOMETRY-SEEN? @ IF
        _PT-RSZ-GEN @ OVER _PT.S.GEOMETRY-GEN @ _PT-U<= IF
            8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF _PT-ACCEPT-EVENT EXIT THEN
    DUP _PT.S.TX-OPEN? @ OVER _PT.S.AWAIT? @ OR
    OVER _PT.S.LIFE-AWAIT? @ OR OVER _PT.S.RESET-PENDING? @ OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    52 _PT-RSZ-COLS @ 8 * + _PT-RSZ-ROWBYTES !
    _PT-RSZ-ROWBYTES @ 40 - OVER _PT.S.PEER-MAX-PAY @ U> IF
        8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RSZ-ROWBYTES @ OVER _PT.S.TX-U @ U> IF
        11 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RSZ-ROWBYTES @ _PT-RSZ-ROWS @ UM*
    DUP IF
        2DROP 8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN DROP
    OVER _PT.S.RET-STATE @ DUP _PT-RD-AVAILABLE =
    SWAP _PT-RD-WAIT-CREDIT = OR IF 216 ELSE 176 THEN
    DUP _PT-RSZ-BASE !
    0xFFFFFFFFFFFFFFFF SWAP - OVER U< IF
        DROP 8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RSZ-BASE @ + _PT-RSZ-SNAPSHOT !
    _PT-RSZ-SNAPSHOT @ OVER _PT.S.PEER-MAX-TX @ U> IF
        8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.RET-STATE @ DUP _PT-RD-AVAILABLE =
    SWAP _PT-RD-WAIT-CREDIT = OR IF
        _PT-RSZ-SNAPSHOT @ OVER _PT.S.RET-CAPS 48 + _PT-U64@ U> IF
            8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    DUP _PT.S.PEER-SENT @ OVER _PT.S.PEER-GRANT @ U> IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RSZ-SNAPSHOT @ OVER _PT.S.PEER-GRANT @
    2 PICK _PT.S.PEER-SENT @ - U> IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RSZ-COLS @ OVER _PT.S.COLS !
    _PT-RSZ-ROWS @ OVER _PT.S.ROWS !
    _PT-RSZ-GEN @ OVER _PT.S.GEOMETRY-GEN !
    TRUE OVER _PT.S.GEOMETRY-SEEN? !
    \ Base CELL resize recovery restarts at revision zero.  Retained resize
    \ is a later PRESENT commit in the existing global revision sequence, so
    \ discovery-only clients preserve that revision and report replacement
    \ needed without fabricating a legacy snapshot.
    DUP _PT.S.RET-STATE @ DUP _PT-RD-AVAILABLE <>
    SWAP _PT-RD-WAIT-CREDIT <> AND IF
        0 OVER _PT.S.REVISION !
    ELSE DUP _PT.S.RET-STATE @ _PT-RD-AVAILABLE = IF
        _PT-RB-LAYOUT-REQUIRED OVER _PT.S.RET-REBUILD !
    THEN
    THEN
    TRUE OVER _PT.S.SNAPSHOT? !
    PT-ST-RESYNCING OVER _PT.S.STATE !
    _PT-ACCEPT-EVENT ;

: _PT-DISPATCH-FOCUS  ( s -- status )
    DUP _PT-INPUT-STATE? 0= IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-LEN @ 16 <> IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ C@ 1 U>
    _PT-RX-P @ 1+ C@ _PT-RX-P @ 2 + W@ OR
    _PT-RX-P @ 4 + L@ OR 0<> OR
    _PT-RX-P @ 8 + _PT-U64@ _PT-RX-S @ _PT.S.REVISION @ <> OR IF
        6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-ACCEPT-EVENT ;

: _PT-DISPATCH  ( s -- status )
    DUP _PT-RET-CHECK-ADJACENCY
    DUP _PT.S.STATE @ PT-ST-OPENING = IF
        _PT-RX-TYPE @ _PT-M-SERVER-READY =
        _PT-RX-TYPE @ _PT-M-CLOSE = OR
        _PT-RX-TYPE @ _PT-M-ERROR = OR 0= IF
            6 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    _PT-RX-TYPE @ _PT-M-SERVER-READY = IF _PT-DISPATCH-READY EXIT THEN
    _PT-RX-TYPE @ _PT-M-CREDIT = IF _PT-DISPATCH-CREDIT EXIT THEN
    _PT-RX-TYPE @ _PT-M-ERROR = IF _PT-DISPATCH-ERROR EXIT THEN
    _PT-RX-TYPE @ _PT-M-CLOSE = IF _PT-DISPATCH-CLOSE EXIT THEN
    _PT-RX-TYPE @ _PT-M-CLOSE-ACK = IF _PT-DISPATCH-CLOSE-ACK EXIT THEN
    _PT-RX-TYPE @ _PT-M-SOFT-RESET-REQUEST = IF
        _PT-DISPATCH-SOFT-RESET EXIT
    THEN
    _PT-RX-TYPE @ _PT-M-TX-RESULT = IF _PT-DISPATCH-TX-RESULT EXIT THEN
    _PT-RX-TYPE @ _PT-M-RET-RESULT = IF _PT-DISPATCH-RET-RESULT EXIT THEN
    _PT-RX-TYPE @ _PT-M-RET-CAPS = IF _PT-DISPATCH-RET-CAPS EXIT THEN
    _PT-RX-TYPE @ _PT-M-RET-FORMATS = IF
        _PT-DISPATCH-RET-FORMATS EXIT
    THEN
    _PT-RX-TYPE @ _PT-M-KEY = IF _PT-DISPATCH-KEY EXIT THEN
    _PT-RX-TYPE @ _PT-M-TEXT = IF _PT-DISPATCH-TEXT EXIT THEN
    _PT-RX-TYPE @ _PT-M-POINTER = IF _PT-DISPATCH-POINTER EXIT THEN
    _PT-RX-TYPE @ _PT-M-RESIZE = IF _PT-DISPATCH-RESIZE EXIT THEN
    _PT-RX-TYPE @ _PT-M-FOCUS = IF _PT-DISPATCH-FOCUS EXIT THEN
    _PT-RX-TYPE @ 0x8000 AND IF
        _PT-RX-TOTAL @ SWAP _PT-RELEASE-DATA EXIT
    THEN
    10 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL ;

VARIABLE _PT-RX-REMAIN
: _PT-RX-STRUCTURAL  ( code -- status )
    _PT-FAIL-CODE !
    _PT-RX-S @ _PT-RX-TYPE @ _PT-RX-SEQNO @ _PT-FAIL-CODE @
    _PT-STRUCTURAL-FAIL ;

: _PT-RX-HEADER?  ( s -- status complete? )
    DUP _PT-RX-S !
    DUP _PT-BIN-A DUP _PT-RX-A ! _PT-HDR + _PT-RX-P !
    DUP _PT.S.BIN-U @ _PT-HDR U< IF DROP PT-S-OK FALSE EXIT THEN
    _PT-RX-A @ 6 + W@ _PT-RX-TYPE !
    _PT-RX-A @ 24 + _PT-U64@ _PT-RX-SEQNO !
    _PT-RX-A @ L@ 0x315450A5 <>
    _PT-RX-A @ 4 + C@ 1 <> OR
    _PT-RX-A @ 5 + C@ _PT-HDR <> OR
    _PT-RX-A @ 8 + W@ 0<> OR
    _PT-RX-A @ 10 + W@ 0<> OR IF
        DROP 1 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    _PT-RX-A @ 12 + L@ DUP _PT-RX-LEN !
    DUP _PT-MAX-PAYLOAD U> IF
        2DROP 1 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    DUP _PT-RX-S @ _PT.S.CLIENT-MAX-PAY @ U> IF
        2DROP 1 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    DROP
    _PT-RX-LEN @ _PT-HDR + DUP _PT-RX-TOTAL !
    OVER _PT-BIN-CAP U> IF DROP 11 _PT-RX-STRUCTURAL FALSE EXIT THEN
    _PT-RX-A @ 16 + _PT-U64@ OVER _PT.S.SESSION-ID @ <> IF
        DROP 3 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    _PT-RX-SEQNO @ OVER _PT.S.RX-SEQ @ <> IF
        DROP 2 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    _PT-RX-A @ 32 + L@ OVER _PT.S.EPOCH @ <> IF
        DROP 4 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    DUP _PT.S.BIN-U @ _PT-RX-TOTAL @ U< IF DROP PT-S-OK FALSE EXIT THEN
    _PT-RX-A @ _PT-RX-LEN @ _PT-FRAME-CRC
    _PT-RX-A @ 36 + L@ <> IF DROP 1 _PT-RX-STRUCTURAL FALSE EXIT THEN
    DROP PT-S-OK TRUE ;

: _PT-RX-CONSUME  ( s -- )
    DUP _PT.S.BIN-U @ _PT-RX-TOTAL @ - DUP _PT-RX-REMAIN !
    ?DUP IF
        _PT-RX-A @ _PT-RX-TOTAL @ + _PT-RX-A @ ROT MOVE
    THEN
    _PT-RX-REMAIN @ SWAP _PT.S.BIN-U ! ;

: _PT-TRY-FRAME  ( s -- status consumed? )
    _PT-RX-S !
    _PT-RX-S @ _PT-RX-HEADER? IF
        DROP
    ELSE
        FALSE EXIT
    THEN
    _PT-RX-TYPE @ _PT-CONTROL-TYPE? 0= _PT-RX-DATA? !
    _PT-RX-DATA? @ IF
        _PT-RX-S @ _PT.S.LOCAL-RECEIVED @
        _PT-RX-S @ _PT.S.LOCAL-GRANT @ U> IF
            5 _PT-RX-STRUCTURAL FALSE EXIT
        THEN
        _PT-RX-TOTAL @
        _PT-RX-S @ _PT.S.LOCAL-GRANT @
        _PT-RX-S @ _PT.S.LOCAL-RECEIVED @ - U> IF
            5 _PT-RX-STRUCTURAL FALSE EXIT
        THEN
    THEN
    _PT-RX-DATA? @ _PT-RX-S @ _PT.S.EVENT-PENDING @ AND IF
        PT-S-WOULD-BLOCK FALSE EXIT
    THEN
    _PT-RX-S @ _PT.S.RX-SEQ @ 0xFFFFFFFFFFFFFFFF =
    _PT-RX-TYPE @ _PT-M-CLOSE <> AND
    _PT-RX-TYPE @ _PT-M-CLOSE-ACK <> AND IF
        2 _PT-RX-STRUCTURAL FALSE EXIT
    THEN
    _PT-RX-DATA? @ IF
        _PT-RX-S @ _PT.S.LOCAL-RECEIVED @ _PT-RX-TOTAL @ +
        _PT-RX-S @ _PT.S.LOCAL-RECEIVED !
    THEN
    _PT-RX-S @ _PT.S.RX-SEQ @ DUP 0xFFFFFFFFFFFFFFFF <> IF
        1+ _PT-RX-S @ _PT.S.RX-SEQ !
    ELSE DROP THEN
    _PT-RX-S @ _PT-RX-CONSUME
    _PT-RX-S @ _PT-DISPATCH TRUE ;

\ =====================================================================
\  Incremental service, close, and normalized event polling
\ =====================================================================

VARIABLE _PT-SVC-S
VARIABLE _PT-SVC-N
: _PT-SERVICE-PROBE  ( s -- status )
    _PT-SVC-S ! 0 _PT-SVC-N !
    BEGIN _PT-SVC-N @ _PT-SERVICE-BYTES U< WHILE
        _PT-SVC-S @ _PT-SCAN-OFFER IF
            _PT-SVC-S @ _PT-ACCEPT-OFFER PT-S-OK EXIT
        THEN DROP
        _PT-SVC-S @ _PT-READ-BYTE IF
            _PT-SVC-N @ 1+ _PT-SVC-N !
        ELSE
            _PT-SERVICE-BYTES _PT-SVC-N !
        THEN
    REPEAT
    _PT-SVC-S @ _PT-SCAN-OFFER IF
        _PT-SVC-S @ _PT-ACCEPT-OFFER PT-S-OK EXIT
    THEN DROP
    _PT-SVC-S @ _PT.S.BIN-U @ _PT-SVC-S @ _PT-BIN-CAP _PT-U>= IF
        _PT-SVC-S @ _PT-PROMOTE-LEGACY
        PT-ST-ANSI _PT-SVC-S @ _PT.S.STATE !
        _PT-SVC-S @ _PT-OWNER-RELEASE
        PT-S-UNSUPPORTED EXIT
    THEN
    MS@ _PT-SVC-S @ _PT.S.DEADLINE @ U< IF PT-S-OK EXIT THEN
    _PT-SVC-S @ _PT.S.PROBES @ _PT-PROBE-LIMIT U< IF
        _PT-SVC-S @ _PT-SEND-PROBE
        _PT-SVC-S @ _PT.S.PROBES @ 1+ _PT-SVC-S @ _PT.S.PROBES !
        MS@ _PT-TIMEOUT-MS + _PT-SVC-S @ _PT.S.DEADLINE !
        PT-S-OK EXIT
    THEN
    _PT-SVC-S @ _PT-PROMOTE-LEGACY
    PT-ST-ANSI _PT-SVC-S @ _PT.S.STATE !
    _PT-SVC-S @ _PT-OWNER-RELEASE
    PT-S-UNSUPPORTED ;

: _PT-BEGIN-CLOSE  ( reason s -- status )
    DUP _PT-SVC-S !
    OVER OVER _PT-SEND-CLOSE ?DUP IF NIP NIP EXIT THEN
    OVER _PT-SVC-S @ _PT.S.CLOSE-REASON !
    _PT-SVC-S @ _PT-RET-RESET
    PT-ST-CLOSING _PT-SVC-S @ _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + _PT-SVC-S @ _PT.S.DEADLINE !
    2DROP PT-S-OK ;

: _PT-SERVICE-BINARY  ( s -- status )
    _PT-SVC-S ! 0 _PT-SVC-N !
    \ A materialized completion is an ownership boundary.  Its exact caller
    \ must reconcile it before service may consume a following CLOSE/CLOSE_ACK
    \ frame whose ANSI transition would erase the completion record.
    _PT-SVC-S @ _PT.S.COMPLETE? @ IF PT-S-OK EXIT THEN
    BEGIN _PT-SVC-N @ _PT-SERVICE-BYTES U< WHILE
        _PT-SVC-S @ _PT-TRY-FRAME IF
            DUP PT-S-OK <> IF EXIT THEN DROP
            _PT-SVC-N @ _PT-RX-TOTAL @ + _PT-SVC-N !
            _PT-SVC-S @ _PT.S.COMPLETE? @ IF PT-S-OK EXIT THEN
            _PT-SVC-S @ _PT.S.STATE @ PT-ST-ANSI = IF PT-S-OK EXIT THEN
            _PT-SVC-S @ _PT.S.EVENT-PENDING @ IF PT-S-OK EXIT THEN
        ELSE
            DUP PT-S-OK <> IF EXIT THEN DROP
            _PT-SVC-S @ _PT-READ-BYTE 0= IF PT-S-OK EXIT THEN
            _PT-SVC-N @ 1+ _PT-SVC-N !
        THEN
    REPEAT
    PT-S-OK ;

: _PT-SERVICE-CREDIT  ( s -- status )
    DUP _PT.S.CREDIT-DIRTY? @ 0= IF DROP PT-S-OK EXIT THEN
    \ A held soft reset has reserved the remaining old-epoch sequence
    \ headroom for its transaction settlement and RESET_ACK.  Credit is
    \ cumulative and may safely wait until the new epoch is established.
    DUP _PT.S.RESET-PENDING? @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.TX-OPEN? @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.SPAN-REMAIN @ IF DROP PT-S-OK EXIT THEN
    DUP _PT-SEND-CREDIT ?DUP IF NIP EXIT THEN
    0 SWAP _PT.S.CREDIT-DIRTY? ! PT-S-OK ;

: _PT-SERVICE-RET-QUERY  ( s -- status )
    DUP _PT.S.RET-ENABLED? @ 0= IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.RESET-PENDING? @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.RET-STATE @ _PT-RD-SNAPSHOT <> IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-ACTIVE <> IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.SNAPSHOT? @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.EVENT-PENDING @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.TX-OPEN? @ OVER _PT.S.AWAIT? @ OR IF
        DROP PT-S-OK EXIT
    THEN
    DUP _PT.S.LOCAL-RECEIVED @ OVER _PT.S.LOCAL-GRANT @ U> IF
        PT-ST-LOST SWAP _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    DUP _PT.S.LOCAL-GRANT @ OVER _PT.S.LOCAL-RECEIVED @ -
    _PT-RET-REPLY-BYTES U< IF
        _PT-RET-CELL-ONLY PT-S-OK EXIT
    THEN
    DUP _PT.S.PEER-SENT @ OVER _PT.S.PEER-GRANT @ U> IF
        PT-ST-LOST SWAP _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    DUP _PT.S.PEER-GRANT @ OVER _PT.S.PEER-SENT @ - 48 U< IF
        DROP PT-S-OK EXIT
    THEN
    _PT-SEND-RET-QUERY ;

: PT-SERVICE  ( session -- status )
    DUP _PT-VALID-S? 0= IF DROP PT-S-INVALID EXIT THEN
    DUP _PT-SVC-S !
    DUP _PT.S.STATE @ PT-ST-ANSI = IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-LOST = IF DROP PT-S-SESSION-LOST EXIT THEN
    DUP _PT.S.STATE @ PT-ST-PROBING = IF _PT-SERVICE-PROBE EXIT THEN
    DUP _PT.S.STATE @ PT-ST-ACTIVE =
    OVER _PT.S.STATE @ PT-ST-RESYNCING = OR IF
        _PT-SVC-S @ _PT.S.TX-SEQ @ 0xFFFFFFFFFFFFFFFE _PT-U>=
        IF
            _PT-SVC-S @ _PT.S.TX-OPEN? @ 0=
            _PT-SVC-S @ _PT-RESULT-BUSY? 0= AND IF
                FALSE _PT-SVC-S @ _PT.S.CLOSE-OPENING? !
                0 _PT-SVC-S @ _PT.S.EVENT-PENDING !
                DROP 2 _PT-SVC-S @ _PT-BEGIN-CLOSE EXIT
            THEN
            \ Reserve the final sequence for CLOSE.  While an admitted result
            \ is outstanding, service may only receive enough input to expose
            \ its completion; it must not emit CREDIT, reset, or RET query.
            DROP _PT-SVC-S @ _PT-SERVICE-BINARY EXIT
        THEN
        DUP _PT-SERVICE-CREDIT ?DUP IF NIP EXIT THEN
    THEN
    DUP _PT-RET-ACTIVATE-READY
    \ Give an already-eligible discovery query priority over newly buffered
    \ ordinary input.  The second opportunity below remains necessary when
    \ this service call itself consumes the initial snapshot TX_RESULT.
    DUP _PT-APPLY-PENDING-RESET ?DUP IF NIP EXIT THEN
    DUP _PT-SERVICE-RET-QUERY ?DUP IF NIP EXIT THEN
    DUP _PT-SERVICE-BINARY ?DUP IF NIP EXIT THEN
    DUP _PT-RET-ACTIVATE-READY
    DUP _PT-APPLY-PENDING-RESET ?DUP IF NIP EXIT THEN
    DUP _PT-SERVICE-RET-QUERY ?DUP IF NIP EXIT THEN
    DUP _PT.S.STATE @ PT-ST-OPENING = IF
        MS@ OVER _PT.S.DEADLINE @ _PT-U>= IF
            TRUE OVER _PT.S.CLOSE-OPENING? !
            1 SWAP _PT-BEGIN-CLOSE EXIT
        THEN
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF
        MS@ OVER _PT.S.DEADLINE @ _PT-U>= IF
            PT-ST-LOST SWAP _PT.S.STATE !
            PT-S-SESSION-LOST EXIT
        THEN
    THEN
    DROP PT-S-OK ;

\ After OPEN, OK means CLOSE was published and state is CLOSING; the caller
\ continues PT-SERVICE until ANSI.  Repeated close is WOULD-BLOCK.  LOST is
\ never a fallback boundary: PT-CLOSE returns SESSION-LOST and keeps ownership.
: PT-CLOSE  ( reason session -- status )
    DUP _PT-VALID-S? 0= IF 2DROP PT-S-INVALID EXIT THEN
    OVER _PT-U16? 0= IF 2DROP PT-S-INVALID EXIT THEN
    DUP _PT.S.STATE @ PT-ST-ANSI = IF 2DROP PT-S-OK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-PROBING = IF
        DUP _PT-SVC-S !
        DUP _PT-PROMOTE-LEGACY
        PT-ST-ANSI OVER _PT.S.STATE !
        DUP _PT-OWNER-RELEASE
        2DROP PT-S-OK EXIT
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF 2DROP PT-S-WOULD-BLOCK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-LOST = IF 2DROP PT-S-SESSION-LOST EXIT THEN
    DUP _PT.S.RESET-PENDING? @ IF 2DROP PT-S-WOULD-BLOCK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-OPENING = OVER _PT.S.CLOSE-OPENING? !
    DUP _PT-TX-CLEAR
    0 OVER _PT.S.EVENT-PENDING !
    _PT-BEGIN-CLOSE ;

VARIABLE _PT-EP-DST
VARIABLE _PT-EP-S
VARIABLE _PT-EP-P
VARIABLE _PT-EP-TYPE
: _PT-EVENT-DEST?  ( event s -- flag )
    _PT-EP-S ! _PT-EP-DST !
    _PT-EP-DST @ /PT-EVENT _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-EP-DST @ /PT-EVENT _PT-EP-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-EP-DST @ /PT-EVENT _PT-EP-S @ _PT.S.RX-A @
        _PT-EP-S @ _PT.S.RX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-EP-DST @ /PT-EVENT _PT-EP-S @ _PT.S.TX-A @
        _PT-EP-S @ _PT.S.TX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-EP-DST @ /PT-EVENT _PT-EP-S @ _PT.S.EVENT-A @
        _PT-EP-S @ _PT.S.EVENT-U @ _PT-RANGES-OVERLAP? 0= ;

: _PT-EVENT-DESCRIBE  ( event s -- )
    _PT-EP-S ! _PT-EP-DST !
    _PT-EP-DST @ /PT-EVENT 0 FILL
    _PT-EP-S @ _PT.S.EVENT-A @ _PT-EP-P !
    _PT-EP-S @ _PT.S.EVENT-TYPE @ DUP _PT-EP-TYPE ! _PT-EP-DST @ !
    _PT-EP-TYPE @ _PT-M-KEY = IF
        _PT-EP-P @ 8 + _PT-U64@ _PT-EP-DST @ 8 + !
        _PT-EP-P @ L@ _PT-EP-DST @ 16 + !
        _PT-EP-P @ 4 + C@ _PT-EP-DST @ 24 + !
        _PT-EP-P @ 5 + C@ _PT-EP-DST @ 32 + !
        _PT-EP-P @ 6 + W@ _PT-EP-DST @ 40 + ! EXIT
    THEN
    _PT-EP-TYPE @ _PT-M-TEXT = IF
        _PT-EP-P @ 4 + _PT-U64@ _PT-EP-DST @ 8 + !
        _PT-EP-P @ W@ _PT-EP-DST @ 16 + !
        _PT-EP-P @ 12 + _PT-EP-DST @ 48 + !
        _PT-EP-S @ _PT.S.EVENT-LEN @ 12 - _PT-EP-DST @ 56 + ! EXIT
    THEN
    _PT-EP-TYPE @ _PT-M-POINTER = IF
        _PT-EP-P @ 20 + _PT-U64@ _PT-EP-DST @ 8 + !
        _PT-EP-P @ _PT-I32@ _PT-EP-DST @ 16 + !
        _PT-EP-P @ 4 + _PT-I32@ _PT-EP-DST @ 24 + !
        _PT-EP-P @ 8 + W@ _PT-EP-P @ 10 + W@ 16 LSHIFT OR
            _PT-EP-DST @ 32 + !
        _PT-EP-P @ 12 + W@
        _PT-EP-P @ 14 + W@ 16 LSHIFT OR
        _PT-EP-P @ 16 + W@ 32 LSHIFT OR
        _PT-EP-P @ 18 + W@ 48 LSHIFT OR _PT-EP-DST @ 40 + ! EXIT
    THEN
    _PT-EP-TYPE @ _PT-M-RESIZE = IF
        _PT-EP-P @ L@ _PT-EP-DST @ 16 + !
        _PT-EP-P @ 4 + L@ _PT-EP-DST @ 24 + !
        _PT-EP-P @ 8 + _PT-U64@ _PT-EP-DST @ 32 + ! EXIT
    THEN
    _PT-EP-TYPE @ _PT-M-FOCUS = IF
        _PT-EP-P @ 8 + _PT-U64@ _PT-EP-DST @ 8 + !
        _PT-EP-P @ C@ _PT-EP-DST @ 16 + !
    THEN ;

: PT-EVENT-POLL  ( event session -- status has-event )
    DUP _PT-VALID-S? 0= IF 2DROP PT-S-INVALID FALSE EXIT THEN
    DUP _PT.S.STATE @ DUP PT-ST-LOST = SWAP PT-ST-CLOSING = OR IF
        2DROP PT-S-SESSION-LOST FALSE EXIT
    THEN
    2DUP _PT-EVENT-DEST? 0= IF 2DROP PT-S-INVALID FALSE EXIT THEN
    DUP _PT.S.EVENT-PENDING @ 0= IF 2DROP PT-S-OK FALSE EXIT THEN
    2DUP _PT-EVENT-DESCRIBE
    DUP _PT.S.EVENT-CHARGE @ OVER _PT-RELEASE-DATA
    DUP IF >R 2DROP R> FALSE EXIT THEN DROP
    0 SWAP _PT.S.EVENT-PENDING ! DROP
    PT-S-OK TRUE ;

VARIABLE _PT-CP-DST
VARIABLE _PT-CP-S
: _PT-COMPLETION-DEST?  ( completion s -- flag )
    _PT-CP-S ! _PT-CP-DST !
    _PT-CP-DST @ /PT-COMPLETION _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-CP-DST @ /PT-COMPLETION _PT-CP-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-CP-DST @ /PT-COMPLETION _PT-CP-S @ _PT.S.RX-A @
        _PT-CP-S @ _PT.S.RX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-CP-DST @ /PT-COMPLETION _PT-CP-S @ _PT.S.TX-A @
        _PT-CP-S @ _PT.S.TX-U @ _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-CP-DST @ /PT-COMPLETION _PT-CP-S @ _PT.S.EVENT-A @
        _PT-CP-S @ _PT.S.EVENT-U @ _PT-RANGES-OVERLAP? 0= ;

: PT-COMPLETION-POLL  ( completion session -- status has-completion )
    DUP _PT-VALID-S? 0= IF 2DROP PT-S-INVALID FALSE EXIT THEN
    2DUP _PT-COMPLETION-DEST? 0= IF 2DROP PT-S-INVALID FALSE EXIT THEN
    DUP _PT.S.COMPLETE? @ 0= IF
        DUP _PT.S.STATE @ DUP PT-ST-LOST = SWAP PT-ST-CLOSING = OR IF
            2DROP PT-S-SESSION-LOST FALSE EXIT
        THEN
        2DROP PT-S-OK FALSE EXIT
    THEN
    DUP _PT.S.COMP-KIND ROT /PT-COMPLETION MOVE
    _PT-COMPLETION-CLEAR
    PT-S-OK TRUE ;

\ =====================================================================
\  RETAINED-1 owner lifecycle writers
\ =====================================================================

VARIABLE _PT-OO-S
VARIABLE _PT-OO-OWNER
VARIABLE _PT-OO-GENERATION
VARIABLE _PT-OO-REGIONS
VARIABLE _PT-OO-RESOURCES
VARIABLE _PT-OO-OBJECTS
VARIABLE _PT-OO-SERIES
VARIABLE _PT-OO-RESOURCE-BYTES
VARIABLE _PT-OO-UTF8-BYTES
VARIABLE _PT-OO-SAMPLE-SLOTS

: _PT-RETAINED-WRITE-STATE?  ( s -- flag )
    DUP PT-RETAINED-AVAILABLE? 0= IF DROP FALSE EXIT THEN
    _PT.S.STATE @ DUP PT-ST-ACTIVE = SWAP PT-ST-RESYNCING = OR ;

: _PT-RETAINED-LIFECYCLE-STATE?  ( s -- flag )
    DUP PT-RETAINED-AVAILABLE? 0= IF DROP FALSE EXIT THEN
    DUP _PT.S.STATE @ PT-ST-ACTIVE =
    SWAP _PT.S.SNAPSHOT? @ 0= AND ;

: _PT-OWNER-QUOTAS?  ( -- flag )
    _PT-OO-REGIONS @ _PT-U32? 0=
    _PT-OO-RESOURCES @ _PT-U32? 0= OR
    _PT-OO-OBJECTS @ _PT-U32? 0= OR
    _PT-OO-SERIES @ _PT-U32? 0= OR IF FALSE EXIT THEN
    _PT-OO-REGIONS @ _PT-OO-S @ _PT.S.RET-CAPS 24 + L@ U> IF FALSE EXIT THEN
    _PT-OO-RESOURCES @ _PT-OO-S @ _PT.S.RET-CAPS 28 + L@ U> IF FALSE EXIT THEN
    _PT-OO-OBJECTS @ _PT-OO-S @ _PT.S.RET-CAPS 32 + L@ U> IF FALSE EXIT THEN
    _PT-OO-SERIES @ _PT-OO-S @ _PT.S.RET-CAPS 36 + L@ U> IF FALSE EXIT THEN
    _PT-OO-RESOURCE-BYTES @ _PT-OO-S @ _PT.S.RET-CAPS 56 + _PT-U64@ U> IF
        FALSE EXIT
    THEN
    _PT-OO-UTF8-BYTES @ _PT-OO-S @ _PT.S.RET-FORMATS 48 + _PT-U64@ U> IF
        FALSE EXIT
    THEN
    _PT-OO-SAMPLE-SLOTS @ _PT-OO-S @ _PT.S.RET-FORMATS 40 + _PT-U64@ U> IF
        FALSE EXIT
    THEN
    TRUE ;

: PT-OWNER-OPEN  ( owner generation region-q resource-q object-q series-q
                    resource-byte-q utf8-byte-q sample-slot-q session -- status )
    _PT-OO-S ! _PT-OO-SAMPLE-SLOTS ! _PT-OO-UTF8-BYTES !
    _PT-OO-RESOURCE-BYTES ! _PT-OO-SERIES ! _PT-OO-OBJECTS !
    _PT-OO-RESOURCES ! _PT-OO-REGIONS ! _PT-OO-GENERATION ! _PT-OO-OWNER !
    _PT-OO-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-OO-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-OO-S @ _PT-RETAINED-WRITE-STATE? 0= IF
        PT-S-UNSUPPORTED EXIT
    THEN
    _PT-OO-S @ _PT-RETAINED-LIFECYCLE-STATE? 0= IF
        PT-S-WOULD-BLOCK EXIT
    THEN
    _PT-OO-S @ _PT.S.TX-OPEN? @ IF PT-S-INVALID EXIT THEN
    _PT-OO-S @ _PT-RESULT-BUSY? IF PT-S-WOULD-BLOCK EXIT THEN
    _PT-OO-OWNER @ 0= _PT-OO-GENERATION @ 0= OR IF PT-S-INVALID EXIT THEN
    _PT-OWNER-QUOTAS? 0= IF PT-S-INVALID EXIT THEN
    _PT-OO-S @ _PT.S.PEER-SENT @ _PT-OO-S @ _PT.S.PEER-GRANT @ U> IF
        PT-ST-LOST _PT-OO-S @ _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    _PT-OO-S @ _PT.S.PEER-GRANT @ _PT-OO-S @ _PT.S.PEER-SENT @ -
    104 U< IF PT-S-WOULD-BLOCK EXIT THEN
    _PT-M-OWNER-OPEN 64 _PT-OO-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-OO-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-OO-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!
    _PT-OO-REGIONS @ _PT-FRAME-PAYLOAD 16 + L!
    _PT-OO-RESOURCES @ _PT-FRAME-PAYLOAD 20 + L!
    _PT-OO-OBJECTS @ _PT-FRAME-PAYLOAD 24 + L!
    _PT-OO-SERIES @ _PT-FRAME-PAYLOAD 28 + L!
    _PT-OO-RESOURCE-BYTES @ _PT-FRAME-PAYLOAD 32 + _PT-U64!
    _PT-OO-UTF8-BYTES @ _PT-FRAME-PAYLOAD 40 + _PT-U64!
    _PT-OO-SAMPLE-SLOTS @ _PT-FRAME-PAYLOAD 48 + _PT-U64!
    TRUE _PT-OO-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    TRUE _PT-OO-S @ _PT.S.LIFE-AWAIT? !
    _PT-M-OWNER-OPEN _PT-OO-S @ _PT.S.LIFE-TYPE !
    _PT-OO-OWNER @ _PT-OO-S @ _PT.S.LIFE-OWNER !
    _PT-OO-GENERATION @ _PT-OO-S @ _PT.S.LIFE-GENERATION !
    PT-S-OK ;

VARIABLE _PT-OD-S
VARIABLE _PT-OD-OWNER
VARIABLE _PT-OD-GENERATION
VARIABLE _PT-OD-TXID

: PT-OWNER-DROP  ( owner generation session -- status )
    _PT-OD-S ! _PT-OD-GENERATION ! _PT-OD-OWNER !
    _PT-OD-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-OD-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-OD-S @ _PT-RETAINED-WRITE-STATE? 0= IF
        PT-S-UNSUPPORTED EXIT
    THEN
    _PT-OD-S @ _PT-RETAINED-LIFECYCLE-STATE? 0= IF
        PT-S-WOULD-BLOCK EXIT
    THEN
    _PT-OD-S @ _PT.S.TX-OPEN? @ IF PT-S-INVALID EXIT THEN
    _PT-OD-S @ _PT-RESULT-BUSY? IF PT-S-WOULD-BLOCK EXIT THEN
    _PT-OD-OWNER @ 0= _PT-OD-GENERATION @ 0= OR IF PT-S-INVALID EXIT THEN
    _PT-OD-S @ _PT.S.REVISION @ 0xFFFFFFFFFFFFFFFF = IF
        PT-ST-LOST _PT-OD-S @ _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    _PT-OD-S @ _PT.S.NEXT-TXID @ DUP 0=
    OVER 0xFFFFFFFFFFFFFFFF = OR IF
        DROP PT-ST-LOST _PT-OD-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-OD-TXID !
    _PT-M-OWNER-DROP 32 _PT-OD-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-OD-TXID @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-OD-S @ _PT.S.REVISION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!
    _PT-OD-OWNER @ _PT-FRAME-PAYLOAD 16 + _PT-U64!
    _PT-OD-GENERATION @ _PT-FRAME-PAYLOAD 24 + _PT-U64!
    FALSE _PT-OD-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    _PT-OD-TXID @ 1+ _PT-OD-S @ _PT.S.NEXT-TXID !
    TRUE _PT-OD-S @ _PT.S.AWAIT? !
    _PT-AWAIT-OWNER-DROP _PT-OD-S @ _PT.S.AWAIT-KIND !
    _PT-OD-TXID @ _PT-OD-S @ _PT.S.AWAIT-TXID !
    _PT-OD-OWNER @ _PT-OD-S @ _PT.S.AWAIT-OWNER !
    _PT-OD-GENERATION @ _PT-OD-S @ _PT.S.AWAIT-GENERATION !
    PT-S-OK ;

\ =====================================================================
\  Shared CELL-1/PRESENT transaction builder
\ =====================================================================

VARIABLE _PT-B-S
VARIABLE _PT-B-COLS
VARIABLE _PT-B-ROWS
VARIABLE _PT-B-SPANS
VARIABLE _PT-B-CELLS
VARIABLE _PT-B-SNAPSHOT
VARIABLE _PT-B-BYTES
VARIABLE _PT-B-TXID
VARIABLE _PT-B-FRAMES
VARIABLE _PT-B-SEQ-ROOM?
VARIABLE _PT-B-SPAN-BYTES
VARIABLE _PT-B-CELL-BYTES

: _PT-BEGIN-ARGS?  ( -- flag )
    _PT-B-COLS @ 0= _PT-B-ROWS @ 0= OR IF FALSE EXIT THEN
    _PT-B-COLS @ _PT-U32? 0= _PT-B-ROWS @ _PT-U32? 0= OR IF FALSE EXIT THEN
    _PT-B-SPANS @ _PT-U32? 0= _PT-B-CELLS @ _PT-U32? 0= OR IF FALSE EXIT THEN
    _PT-B-COLS @ _PT-B-S @ _PT.S.COLS @ <>
    _PT-B-ROWS @ _PT-B-S @ _PT.S.ROWS @ <> OR IF FALSE EXIT THEN
    _PT-B-CELLS @ _PT-B-COLS @ _PT-B-ROWS @ * U> IF FALSE EXIT THEN
    _PT-B-CELLS @ _PT-B-SPANS @ _PT-B-COLS @ * U> IF FALSE EXIT THEN
    _PT-B-SPANS @ 0= IF _PT-B-CELLS @ 0= EXIT THEN
    _PT-B-CELLS @ _PT-B-SPANS @ _PT-U>= ;

: _PT-SNAPSHOT-COUNTS?  ( -- flag )
    _PT-B-SPANS @ 0= IF FALSE EXIT THEN
    _PT-B-SPANS @ _PT-B-ROWS @ U< IF FALSE EXIT THEN
    _PT-B-COLS @ _PT-B-ROWS @ * _PT-B-CELLS @ =
    _PT-B-SPANS @ _PT-B-CELLS @ _PT-U<= AND ;

: _PT-TX-PREFLIGHT?  ( -- flag )
    TRUE _PT-B-SEQ-ROOM? !
    _PT-B-SPANS @ 3 + DUP _PT-B-FRAMES !
    0xFFFFFFFFFFFFFFFF SWAP -
    _PT-B-S @ _PT.S.TX-SEQ @ U< IF
        FALSE _PT-B-SEQ-ROOM? ! FALSE EXIT
    THEN
    _PT-B-SPANS @ 52 _PT-UMUL? 0= IF DROP FALSE EXIT THEN
    _PT-B-SPAN-BYTES !
    _PT-B-CELLS @ 8 _PT-UMUL? 0= IF DROP FALSE EXIT THEN
    _PT-B-CELL-BYTES !
    _PT-B-SPAN-BYTES @ _PT-B-CELL-BYTES @ _PT-UADD? 0= IF
        DROP FALSE EXIT
    THEN
    176 _PT-UADD? 0= IF DROP FALSE EXIT THEN DUP _PT-B-BYTES !
    _PT-B-S @ _PT.S.PEER-MAX-TX @ U> IF FALSE EXIT THEN
    _PT-B-S @ _PT.S.PEER-SENT @ _PT-B-S @ _PT.S.PEER-GRANT @ U> IF
        FALSE EXIT
    THEN
    _PT-B-BYTES @
    _PT-B-S @ _PT.S.PEER-GRANT @ _PT-B-S @ _PT.S.PEER-SENT @ - _PT-U<= ;

: _PT-EMIT-BEGIN  ( -- status )
    _PT-B-SNAPSHOT @ IF _PT-M-SNAPSHOT-BEGIN ELSE _PT-M-TX-BEGIN THEN
    32 _PT-B-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-B-TXID @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-B-SNAPSHOT @ IF 0 ELSE _PT-B-S @ _PT.S.REVISION @ THEN
        _PT-FRAME-PAYLOAD 8 + _PT-U64!
    _PT-B-COLS @ _PT-FRAME-PAYLOAD 16 + L!
    _PT-B-ROWS @ _PT-FRAME-PAYLOAD 20 + L!
    _PT-B-SPANS @ _PT-FRAME-PAYLOAD 24 + L!
    _PT-B-CELLS @ _PT-FRAME-PAYLOAD 28 + L!
    TRUE _PT-B-S @ _PT-FRAME-SEND ;

: _PT-BEGIN-TX  ( cols rows span-count cell-count snapshot? session -- status )
    _PT-B-S ! _PT-B-SNAPSHOT ! _PT-B-CELLS ! _PT-B-SPANS !
    _PT-B-ROWS ! _PT-B-COLS !
    _PT-B-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-B-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-B-SNAPSHOT @ IF
        _PT-B-S @ _PT.S.STATE @ DUP PT-ST-ACTIVE <>
        SWAP PT-ST-RESYNCING <> AND IF PT-S-INVALID EXIT THEN
        _PT-B-S @ _PT.S.SNAPSHOT? @ 0= IF PT-S-INVALID EXIT THEN
        \ A positive discovery makes later replace-all work a PRESENT
        \ transaction.  This word must not emit the now-forbidden legacy
        \ SNAPSHOT_BEGIN in its place.  Soft reset first
        \ returns the discovery state to SNAPSHOT, so its mandatory CELL
        \ recovery remains available.
        _PT-B-S @ _PT.S.RET-STATE @ _PT-RD-AVAILABLE = IF
            PT-S-UNSUPPORTED EXIT
        THEN
    ELSE
        _PT-B-S @ _PT.S.STATE @ PT-ST-ACTIVE <> IF PT-S-INVALID EXIT THEN
        _PT-B-S @ _PT.S.SNAPSHOT? @ IF PT-S-INVALID EXIT THEN
        _PT-B-S @ _PT.S.REVISION @ 0xFFFFFFFFFFFFFFFF = IF
            PT-ST-LOST _PT-B-S @ _PT.S.STATE !
            PT-S-SESSION-LOST EXIT
        THEN
    THEN
    _PT-B-S @ _PT-RESULT-BUSY? IF PT-S-WOULD-BLOCK EXIT THEN
    _PT-B-S @ _PT.S.TX-OPEN? @ IF PT-S-INVALID EXIT THEN
    _PT-BEGIN-ARGS? 0= IF PT-S-INVALID EXIT THEN
    _PT-B-SNAPSHOT @ IF _PT-SNAPSHOT-COUNTS? 0= IF PT-S-INVALID EXIT THEN THEN
    _PT-TX-PREFLIGHT? 0= IF
        _PT-B-SEQ-ROOM? @ 0= IF PT-S-INVALID EXIT THEN
        _PT-B-BYTES @ _PT-B-S @ _PT.S.PEER-MAX-TX @ U> IF
            PT-S-INVALID
        ELSE
            PT-S-WOULD-BLOCK
        THEN EXIT
    THEN
    _PT-B-S @ _PT.S.NEXT-TXID @ DUP 0=
    OVER 0xFFFFFFFFFFFFFFFF = OR IF
        DROP
        PT-ST-LOST _PT-B-S @ _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    DUP _PT-B-TXID ! DROP
    _PT-EMIT-BEGIN ?DUP IF EXIT THEN
    _PT-B-TXID @ 1+ _PT-B-S @ _PT.S.NEXT-TXID !
    TRUE _PT-B-S @ _PT.S.TX-OPEN? !
    _PT-B-SNAPSHOT @ IF _PT-TX-SNAPSHOT ELSE _PT-TX-CELL THEN
        _PT-B-S @ _PT.S.TX-KIND !
    _PT-B-SNAPSHOT @ IF PT-CELL-REPLACE ELSE PT-CELL-DELTA THEN
        _PT-B-S @ _PT.S.TX-CELL-MODE !
    _PT-B-SNAPSHOT @ _PT-B-S @ _PT.S.TX-SNAPSHOT? !
    _PT-B-TXID @ _PT-B-S @ _PT.S.TXID !
    _PT-B-SPANS @ _PT-B-S @ _PT.S.TX-SPANS !
    _PT-B-CELLS @ _PT-B-S @ _PT.S.TX-CELLS !
    0 _PT-B-S @ _PT.S.TX-SPANS-DONE !
    0 _PT-B-S @ _PT.S.TX-CELLS-DONE !
    0 _PT-B-S @ _PT.S.SPAN-REMAIN !
    0 _PT-B-S @ _PT.S.CURSOR-DONE? !
    0 _PT-B-S @ _PT.S.LAST-END !
    _PT-B-BYTES @ _PT-B-S @ _PT.S.TX-BYTES !
    PT-S-OK ;

: PT-TX-BEGIN  ( cols rows span-count cell-count session -- status )
    FALSE SWAP _PT-BEGIN-TX ;

: PT-SNAPSHOT-BEGIN  ( cols rows span-count cell-count session -- status )
    TRUE SWAP _PT-BEGIN-TX ;

VARIABLE _PT-PB-S
VARIABLE _PT-PB-COLS
VARIABLE _PT-PB-ROWS
VARIABLE _PT-PB-SPANS
VARIABLE _PT-PB-CELLS
VARIABLE _PT-PB-RET-OPS
VARIABLE _PT-PB-RET-BYTES
VARIABLE _PT-PB-CELL-MODE
VARIABLE _PT-PB-RET-MODE
VARIABLE _PT-PB-SPAN-BYTES
VARIABLE _PT-PB-CELL-BYTES
VARIABLE _PT-PB-BYTES
VARIABLE _PT-PB-FRAMES
VARIABLE _PT-PB-TXID
VARIABLE _PT-PB-HARD-FAIL?
VARIABLE _PT-PB-RET-MIN

: _PT-PB-CELL-ARGS?  ( -- flag )
    _PT-PB-COLS @ 0= _PT-PB-ROWS @ 0= OR IF FALSE EXIT THEN
    _PT-PB-COLS @ _PT-U32? 0= _PT-PB-ROWS @ _PT-U32? 0= OR IF FALSE EXIT THEN
    _PT-PB-SPANS @ _PT-U32? 0= _PT-PB-CELLS @ _PT-U32? 0= OR IF FALSE EXIT THEN
    _PT-PB-COLS @ _PT-PB-S @ _PT.S.COLS @ <>
    _PT-PB-ROWS @ _PT-PB-S @ _PT.S.ROWS @ <> OR IF FALSE EXIT THEN
    _PT-PB-CELL-MODE @ PT-CELL-NONE = IF
        _PT-PB-SPANS @ 0= _PT-PB-CELLS @ 0= AND EXIT
    THEN
    _PT-PB-CELL-MODE @ PT-CELL-DELTA = IF
        _PT-PB-S @ _PT.S.STATE @ PT-ST-ACTIVE <> IF FALSE EXIT THEN
        _PT-PB-S @ _PT.S.SNAPSHOT? @ IF FALSE EXIT THEN
        _PT-PB-CELLS @ _PT-PB-COLS @ _PT-PB-ROWS @ * U> IF FALSE EXIT THEN
        _PT-PB-CELLS @ _PT-PB-SPANS @ _PT-PB-COLS @ * U> IF FALSE EXIT THEN
        _PT-PB-SPANS @ 0= IF _PT-PB-CELLS @ 0= EXIT THEN
        _PT-PB-CELLS @ _PT-PB-SPANS @ _PT-U>= EXIT
    THEN
    _PT-PB-CELL-MODE @ PT-CELL-REPLACE <> IF FALSE EXIT THEN
    _PT-PB-S @ _PT.S.STATE @ PT-ST-ACTIVE =
    _PT-PB-S @ _PT.S.SNAPSHOT? @ 0= AND
    _PT-PB-S @ _PT.S.STATE @ PT-ST-RESYNCING =
    _PT-PB-S @ _PT.S.SNAPSHOT? @ AND OR 0= IF FALSE EXIT THEN
    _PT-PB-SPANS @ _PT-PB-ROWS @ <>
    _PT-PB-CELLS @ _PT-PB-COLS @ _PT-PB-ROWS @ * <> OR 0= ;

: _PT-PB-RET-ARGS?  ( -- flag )
    _PT-PB-RET-OPS @ _PT-U32? 0= IF FALSE EXIT THEN
    _PT-PB-RET-OPS @ _PT-PB-S @ _PT.S.RET-CAPS 40 + L@ U> IF FALSE EXIT THEN
    _PT-PB-RET-MODE @ PT-RET-NONE = IF
        _PT-PB-RET-OPS @ 0= _PT-PB-RET-BYTES @ 0= AND EXIT
    THEN
    _PT-PB-RET-MODE @ PT-RET-LAYOUT-CONTINUE U> IF FALSE EXIT THEN
    _PT-PB-RET-OPS @ 0= IF
        _PT-PB-RET-BYTES @ 0<> IF FALSE EXIT THEN
    ELSE
        _PT-PB-RET-OPS @ 64 _PT-UMUL? 0= IF DROP FALSE EXIT THEN
        DUP _PT-PB-RET-MIN !
        _PT-PB-RET-BYTES @ U> IF FALSE EXIT THEN
        _PT-PB-RET-BYTES @ _PT-PB-RET-MIN @ - DUP 24 MOD IF
            DROP FALSE EXIT
        THEN
        _PT-PB-RET-OPS @ 24 * U> IF FALSE EXIT THEN
    THEN
    _PT-PB-RET-MODE @ PT-RET-DELTA = IF
        _PT-PB-RET-OPS @ 0<>
        _PT-PB-S @ _PT.S.RET-REBUILD @ _PT-RB-NONE = AND EXIT
    THEN
    _PT-PB-RET-MODE @ PT-RET-REPLACE-START = IF
        _PT-PB-S @ _PT.S.RET-REBUILD @ _PT-RB-REPLACE-REQUIRED = EXIT
    THEN
    _PT-PB-RET-MODE @ PT-RET-REPLACE-CONTINUE = IF
        _PT-PB-S @ _PT.S.RET-REBUILD @ _PT-RB-REPLACE-PENDING = EXIT
    THEN
    _PT-PB-RET-MODE @ PT-RET-LAYOUT-START = IF
        _PT-PB-S @ _PT.S.RET-REBUILD @ _PT-RB-LAYOUT-REQUIRED = EXIT
    THEN
    _PT-PB-S @ _PT.S.RET-REBUILD @ _PT-RB-LAYOUT-PENDING = ;

: _PT-PB-PREFLIGHT?  ( -- flag )
    FALSE _PT-PB-HARD-FAIL? !
    _PT-PB-SPANS @ 52 _PT-UMUL? 0= IF
        DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-SPAN-BYTES !
    _PT-PB-CELLS @ 8 _PT-UMUL? 0= IF
        DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-CELL-BYTES !
    160 _PT-PB-SPAN-BYTES @ _PT-UADD? 0= IF
        DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-CELL-BYTES @ _PT-UADD? 0= IF
        DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-CELL-MODE @ PT-CELL-NONE <> IF
        56 _PT-UADD? 0= IF
            DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
        THEN
    THEN
    _PT-PB-RET-BYTES @ _PT-UADD? 0= IF
        DROP TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    DUP _PT-PB-BYTES !
    _PT-PB-S @ _PT.S.RET-CAPS 48 + _PT-U64@ U> IF
        TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-BYTES @ _PT-PB-S @ _PT.S.PEER-MAX-TX @ U> IF
        TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-SPANS @ _PT-PB-RET-OPS @ + 2 +
    _PT-PB-CELL-MODE @ PT-CELL-NONE <> IF 1+ THEN
    DUP _PT-PB-FRAMES !
    0xFFFFFFFFFFFFFFFF SWAP - _PT-PB-S @ _PT.S.TX-SEQ @ U< IF
        TRUE _PT-PB-HARD-FAIL? ! FALSE EXIT
    THEN
    _PT-PB-S @ _PT.S.PEER-SENT @ _PT-PB-S @ _PT.S.PEER-GRANT @ U> IF
        FALSE EXIT
    THEN
    _PT-PB-BYTES @ _PT-PB-S @ _PT.S.PEER-GRANT @
    _PT-PB-S @ _PT.S.PEER-SENT @ - _PT-U<= ;

: _PT-PB-EMIT  ( -- status )
    _PT-M-PRESENT-BEGIN 64 _PT-PB-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-PB-TXID @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-PB-S @ _PT.S.REVISION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!
    _PT-PB-S @ _PT.S.GEOMETRY-GEN @ _PT-FRAME-PAYLOAD 16 + _PT-U64!
    _PT-PB-BYTES @ _PT-FRAME-PAYLOAD 24 + _PT-U64!
    _PT-PB-COLS @ _PT-FRAME-PAYLOAD 32 + L!
    _PT-PB-ROWS @ _PT-FRAME-PAYLOAD 36 + L!
    _PT-PB-SPANS @ _PT-FRAME-PAYLOAD 40 + L!
    _PT-PB-CELLS @ _PT-FRAME-PAYLOAD 44 + L!
    _PT-PB-RET-OPS @ _PT-FRAME-PAYLOAD 48 + L!
    _PT-PB-CELL-MODE @ _PT-FRAME-PAYLOAD 52 + L!
    _PT-PB-RET-MODE @ _PT-FRAME-PAYLOAD 56 + L!
    TRUE _PT-PB-S @ _PT-FRAME-SEND ;

: PT-PRESENT-BEGIN  ( cols rows cell-spans cells retained-ops
                       retained-frame-bytes cell-mode retained-mode
                       session -- status )
    _PT-PB-S ! _PT-PB-RET-MODE ! _PT-PB-CELL-MODE !
    _PT-PB-RET-BYTES ! _PT-PB-RET-OPS ! _PT-PB-CELLS !
    _PT-PB-SPANS ! _PT-PB-ROWS ! _PT-PB-COLS !
    _PT-PB-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-PB-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-PB-S @ _PT-RETAINED-WRITE-STATE? 0= IF PT-S-UNSUPPORTED EXIT THEN
    _PT-PB-S @ _PT-RESULT-BUSY? IF PT-S-WOULD-BLOCK EXIT THEN
    _PT-PB-S @ _PT.S.TX-OPEN? @ IF PT-S-INVALID EXIT THEN
    _PT-PB-S @ _PT.S.REVISION @ 0xFFFFFFFFFFFFFFFF = IF
        PT-ST-LOST _PT-PB-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-PB-CELL-MODE @ PT-CELL-NONE =
    _PT-PB-RET-MODE @ PT-RET-NONE = AND IF PT-S-INVALID EXIT THEN
    _PT-PB-CELL-ARGS? 0= IF PT-S-INVALID EXIT THEN
    _PT-PB-RET-ARGS? 0= IF PT-S-INVALID EXIT THEN
    _PT-PB-S @ _PT.S.SNAPSHOT? @
    _PT-PB-CELL-MODE @ PT-CELL-REPLACE <> AND IF PT-S-INVALID EXIT THEN
    _PT-PB-S @ _PT.S.PEER-SENT @ _PT-PB-S @ _PT.S.PEER-GRANT @ U> IF
        PT-ST-LOST _PT-PB-S @ _PT.S.STATE ! PT-S-SESSION-LOST EXIT
    THEN
    _PT-PB-PREFLIGHT? 0= IF
        _PT-PB-HARD-FAIL? @ IF PT-S-INVALID ELSE PT-S-WOULD-BLOCK THEN
        EXIT
    THEN
    _PT-PB-S @ _PT.S.NEXT-TXID @ DUP 0=
    OVER 0xFFFFFFFFFFFFFFFF = OR IF
        DROP PT-ST-LOST _PT-PB-S @ _PT.S.STATE !
        PT-S-SESSION-LOST EXIT
    THEN
    _PT-PB-TXID !
    _PT-PB-EMIT ?DUP IF EXIT THEN
    _PT-PB-TXID @ 1+ _PT-PB-S @ _PT.S.NEXT-TXID !
    TRUE _PT-PB-S @ _PT.S.TX-OPEN? !
    _PT-TX-PRESENT _PT-PB-S @ _PT.S.TX-KIND !
    _PT-PB-CELL-MODE @ _PT-PB-S @ _PT.S.TX-CELL-MODE !
    _PT-PB-RET-MODE @ _PT-PB-S @ _PT.S.TX-RET-MODE !
    _PT-PB-TXID @ _PT-PB-S @ _PT.S.TXID !
    _PT-PB-SPANS @ _PT-PB-S @ _PT.S.TX-SPANS !
    _PT-PB-CELLS @ _PT-PB-S @ _PT.S.TX-CELLS !
    _PT-PB-RET-OPS @ _PT-PB-S @ _PT.S.TX-RET-OPS !
    _PT-PB-RET-BYTES @ _PT-PB-S @ _PT.S.TX-RET-BYTES !
    _PT-PB-BYTES @ _PT-PB-S @ _PT.S.TX-BYTES !
    0 _PT-PB-S @ _PT.S.TX-SPANS-DONE !
    0 _PT-PB-S @ _PT.S.TX-CELLS-DONE !
    0 _PT-PB-S @ _PT.S.TX-RET-OPS-DONE !
    0 _PT-PB-S @ _PT.S.TX-RET-BYTES-DONE !
    0 _PT-PB-S @ _PT.S.SPAN-REMAIN !
    0 _PT-PB-S @ _PT.S.CURSOR-DONE? !
    0 _PT-PB-S @ _PT.S.LAST-END !
    PT-S-OK ;

VARIABLE _PT-SP-S
VARIABLE _PT-SP-ROW
VARIABLE _PT-SP-COL
VARIABLE _PT-SP-COUNT
VARIABLE _PT-SP-START
VARIABLE _PT-SP-END
: PT-SPAN-BEGIN  ( row col count session -- status )
    _PT-SP-S ! _PT-SP-COUNT ! _PT-SP-COL ! _PT-SP-ROW !
    _PT-SP-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-SP-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT = IF
        _PT-SP-S @ _PT.S.TX-CELL-MODE @ PT-CELL-NONE =
        _PT-SP-S @ _PT.S.TX-RET-OPS-DONE @ 0<> OR IF
            PT-S-INVALID EXIT
        THEN
    THEN
    _PT-SP-S @ _PT.S.SPAN-REMAIN @ IF PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT.S.CURSOR-DONE? @ IF PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT.S.TX-SPANS-DONE @ _PT-SP-S @ _PT.S.TX-SPANS @
        _PT-U>= IF PT-S-INVALID EXIT THEN
    _PT-SP-ROW @ _PT-U32? 0= _PT-SP-COL @ _PT-U32? 0= OR
    _PT-SP-COUNT @ _PT-U32? 0= OR _PT-SP-COUNT @ 0= OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-SP-ROW @ _PT-SP-S @ _PT.S.ROWS @ _PT-U>= IF PT-S-INVALID EXIT THEN
    _PT-SP-COL @ _PT-SP-S @ _PT.S.COLS @ _PT-U>= IF PT-S-INVALID EXIT THEN
    _PT-SP-COUNT @ _PT-SP-S @ _PT.S.COLS @ _PT-SP-COL @ - U> IF
        PT-S-INVALID EXIT
    THEN
    _PT-SP-S @ _PT.S.TX-CELLS-DONE @ _PT-SP-COUNT @ + DUP
    _PT-SP-S @ _PT.S.TX-CELLS-DONE @ U< IF DROP PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT.S.TX-CELLS @ U> IF PT-S-INVALID EXIT THEN
    _PT-SP-ROW @ _PT-SP-S @ _PT.S.COLS @ * _PT-SP-COL @ +
    DUP _PT-SP-START ! _PT-SP-COUNT @ + DUP _PT-SP-END !
    _PT-SP-S @ _PT.S.LAST-END @ U< IF PT-S-INVALID EXIT THEN
    _PT-SP-S @ _PT.S.TX-SNAPSHOT? @ IF
        _PT-SP-START @ _PT-SP-S @ _PT.S.LAST-END @ <> IF
            PT-S-INVALID EXIT
        THEN
    THEN
    _PT-SP-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT =
    _PT-SP-S @ _PT.S.TX-CELL-MODE @ PT-CELL-REPLACE = AND IF
        _PT-SP-COL @ 0<>
        _PT-SP-COUNT @ _PT-SP-S @ _PT.S.COLS @ <> OR
        _PT-SP-ROW @ _PT-SP-S @ _PT.S.TX-SPANS-DONE @ <> OR IF
            PT-S-INVALID EXIT
        THEN
    THEN
    12 _PT-SP-COUNT @ 8 * + DUP
    _PT-SP-S @ _PT.S.PEER-MAX-PAY @ U> IF DROP PT-S-INVALID EXIT THEN
    _PT-M-CELL-SPAN SWAP _PT-SP-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-SP-ROW @ _PT-FRAME-PAYLOAD L!
    _PT-SP-COL @ _PT-FRAME-PAYLOAD 4 + L!
    _PT-SP-COUNT @ _PT-FRAME-PAYLOAD 8 + L!
    _PT-SP-COUNT @ _PT-SP-S @ _PT.S.SPAN-REMAIN !
    _PT-SP-END @ _PT-SP-S @ _PT.S.LAST-END !
    PT-S-OK ;

VARIABLE _PT-C-S
VARIABLE _PT-C-CP
VARIABLE _PT-C-FG
VARIABLE _PT-C-BG
VARIABLE _PT-C-ATTRS
VARIABLE _PT-C-INDEX
: PT-CELL  ( cp fg bg attrs session -- status )
    _PT-C-S ! _PT-C-ATTRS ! _PT-C-BG ! _PT-C-FG ! _PT-C-CP !
    _PT-C-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-C-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-C-S @ _PT.S.TX-OPEN? @ 0=
    _PT-C-S @ _PT.S.SPAN-REMAIN @ 0= OR IF PT-S-INVALID EXIT THEN
    _PT-C-CP @ 0= IF 32 _PT-C-CP ! THEN
    _PT-C-CP @ _PT-SCALAR? 0= IF PT-S-INVALID EXIT THEN
    _PT-C-FG @ _PT-U8? 0= _PT-C-BG @ _PT-U8? 0= OR
    _PT-C-ATTRS @ _PT-U16? 0= OR
    _PT-C-ATTRS @ 0xFF80 AND 0<> OR IF PT-S-INVALID EXIT THEN
    _PT-FRAME-PAYLOAD 8 + L@
    _PT-C-S @ _PT.S.SPAN-REMAIN @ - DUP _PT-C-INDEX !
    8 * _PT-FRAME-PAYLOAD 12 + +
    DUP _PT-C-CP @ SWAP L!
    DUP 4 + _PT-C-FG @ SWAP C!
    DUP 5 + _PT-C-BG @ SWAP C!
    6 + _PT-C-ATTRS @ SWAP W!
    _PT-C-S @ _PT.S.SPAN-REMAIN @ 1- DUP
    _PT-C-S @ _PT.S.SPAN-REMAIN ! IF PT-S-OK EXIT THEN
    TRUE _PT-C-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    _PT-C-S @ _PT.S.TX-SPANS-DONE @ 1+
        _PT-C-S @ _PT.S.TX-SPANS-DONE !
    _PT-C-S @ _PT.S.TX-CELLS-DONE @ _PT-FRAME-PAYLOAD 8 + L@ +
        _PT-C-S @ _PT.S.TX-CELLS-DONE !
    PT-S-OK ;

VARIABLE _PT-CUR-S
VARIABLE _PT-CUR-ROW
VARIABLE _PT-CUR-COL
VARIABLE _PT-CUR-VISIBLE
: PT-CURSOR  ( row col visible session -- status )
    _PT-CUR-S ! _PT-CUR-VISIBLE ! _PT-CUR-COL ! _PT-CUR-ROW !
    _PT-CUR-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-CUR-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-CUR-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-CUR-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT = IF
        _PT-CUR-S @ _PT.S.TX-CELL-MODE @ PT-CELL-NONE =
        _PT-CUR-S @ _PT.S.TX-RET-OPS-DONE @ 0<> OR IF
            PT-S-INVALID EXIT
        THEN
    THEN
    _PT-CUR-S @ _PT.S.SPAN-REMAIN @ IF PT-S-INVALID EXIT THEN
    _PT-CUR-S @ _PT.S.CURSOR-DONE? @ IF PT-S-INVALID EXIT THEN
    _PT-CUR-S @ _PT.S.TX-SPANS-DONE @ _PT-CUR-S @ _PT.S.TX-SPANS @ <>
    _PT-CUR-S @ _PT.S.TX-CELLS-DONE @ _PT-CUR-S @ _PT.S.TX-CELLS @ <> OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-CUR-ROW @ _PT-U32? 0= _PT-CUR-COL @ _PT-U32? 0= OR
    _PT-CUR-VISIBLE @ DUP 0<> SWAP 1 <> AND OR IF PT-S-INVALID EXIT THEN
    _PT-CUR-VISIBLE @ IF
        _PT-CUR-ROW @ _PT-CUR-S @ _PT.S.ROWS @ _PT-U>=
        _PT-CUR-COL @ _PT-CUR-S @ _PT.S.COLS @ _PT-U>= OR IF
            PT-S-INVALID EXIT
        THEN
    THEN
    _PT-M-CURSOR 16 _PT-CUR-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-CUR-ROW @ _PT-FRAME-PAYLOAD L!
    _PT-CUR-COL @ _PT-FRAME-PAYLOAD 4 + L!
    _PT-CUR-VISIBLE @ _PT-FRAME-PAYLOAD 8 + C!
    TRUE _PT-CUR-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    TRUE _PT-CUR-S @ _PT.S.CURSOR-DONE? ! PT-S-OK ;

VARIABLE _PT-PO-TYPE
VARIABLE _PT-PO-A
VARIABLE _PT-PO-U
VARIABLE _PT-PO-S
VARIABLE _PT-PO-NEXT-OPS
VARIABLE _PT-PO-NEXT-BYTES

: _PT-PO-SOURCE?  ( -- flag )
    _PT-PO-A @ _PT-PO-U @ _PT-RANGE-VALID? 0= IF FALSE EXIT THEN
    _PT-PO-A @ _PT-PO-U @ _PT-PO-S @ /PT-SESSION
        _PT-RANGES-OVERLAP? IF FALSE EXIT THEN
    _PT-PO-A @ _PT-PO-U @ _PT-PO-S @ _PT.S.TX-A @
        _PT-PO-S @ _PT.S.TX-U @ _PT-RANGES-OVERLAP? 0= ;

: _PT-PO-REGION?  ( -- flag )
    _PT-PO-TYPE @ DUP _PT-M-REGION-DEFINE =
    SWAP _PT-M-REGION-REPLACE = OR IF
        _PT-PO-U @ 48 <> IF FALSE EXIT THEN
        _PT-PO-A @ _PT-U64@ 0=
        _PT-PO-A @ 8 + _PT-U64@ 0= OR
        _PT-PO-A @ 16 + _PT-U64@ 0= OR IF FALSE EXIT THEN
        _PT-PO-A @ 24 + L@ _PT-PO-A @ 32 + L@ _PT-UADD?
        0= IF DROP FALSE EXIT THEN
        _PT-PO-S @ _PT.S.COLS @ U> IF FALSE EXIT THEN
        _PT-PO-A @ 28 + L@ _PT-PO-A @ 36 + L@ _PT-UADD?
        0= IF DROP FALSE EXIT THEN
        _PT-PO-S @ _PT.S.ROWS @ U> IF FALSE EXIT THEN
        _PT-PO-A @ 32 + L@ 0= _PT-PO-A @ 36 + L@ 0= OR IF FALSE EXIT THEN
        _PT-PO-A @ 44 + L@ 3 INVERT AND 0= EXIT
    THEN
    _PT-PO-TYPE @ _PT-M-REGION-DROP <> IF FALSE EXIT THEN
    _PT-PO-U @ 24 <> IF FALSE EXIT THEN
    _PT-PO-A @ _PT-U64@ 0<>
    _PT-PO-A @ 8 + _PT-U64@ 0<> AND
    _PT-PO-A @ 16 + _PT-U64@ 0<> AND ;

: PT-PRESENT-OP  ( type payload-a payload-u session -- status )
    _PT-PO-S ! _PT-PO-U ! _PT-PO-A ! _PT-PO-TYPE !
    _PT-PO-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-PO-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT <> IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.TX-RET-MODE @ PT-RET-NONE = IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.SPAN-REMAIN @ IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.TX-SPANS-DONE @ _PT-PO-S @ _PT.S.TX-SPANS @ <>
    _PT-PO-S @ _PT.S.TX-CELLS-DONE @ _PT-PO-S @ _PT.S.TX-CELLS @ <> OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-PO-S @ _PT.S.TX-CELL-MODE @ PT-CELL-NONE = IF
        _PT-PO-S @ _PT.S.CURSOR-DONE? @ IF PT-S-INVALID EXIT THEN
    ELSE
        _PT-PO-S @ _PT.S.CURSOR-DONE? @ 0= IF PT-S-INVALID EXIT THEN
    THEN
    _PT-PO-SOURCE? 0= IF PT-S-INVALID EXIT THEN
    _PT-PO-REGION? 0= IF PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.TX-RET-OPS-DONE @ 1 _PT-UADD?
    0= IF DROP PT-S-INVALID EXIT THEN DUP _PT-PO-NEXT-OPS !
    _PT-PO-S @ _PT.S.TX-RET-OPS @ U> IF PT-S-INVALID EXIT THEN
    _PT-PO-U @ 40 _PT-UADD? 0= IF DROP PT-S-INVALID EXIT THEN
    _PT-PO-S @ _PT.S.TX-RET-BYTES-DONE @ SWAP _PT-UADD?
    0= IF DROP PT-S-INVALID EXIT THEN DUP _PT-PO-NEXT-BYTES !
    _PT-PO-S @ _PT.S.TX-RET-BYTES @ U> IF PT-S-INVALID EXIT THEN
    _PT-PO-TYPE @ _PT-PO-U @ _PT-PO-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-PO-A @ _PT-PO-U @ _PT-FRAME-PAYLOAD SWAP MOVE
    TRUE _PT-PO-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    _PT-PO-NEXT-OPS @ _PT-PO-S @ _PT.S.TX-RET-OPS-DONE !
    _PT-PO-NEXT-BYTES @ _PT-PO-S @ _PT.S.TX-RET-BYTES-DONE !
    PT-S-OK ;

CREATE _PT-REGION-PAYLOAD 48 ALLOT
VARIABLE _PT-RG-TYPE
VARIABLE _PT-RG-S
VARIABLE _PT-RG-OWNER
VARIABLE _PT-RG-GENERATION
VARIABLE _PT-RG-ID
VARIABLE _PT-RG-X
VARIABLE _PT-RG-Y
VARIABLE _PT-RG-COLS
VARIABLE _PT-RG-ROWS
VARIABLE _PT-RG-Z
VARIABLE _PT-RG-FLAGS

: _PT-REGION-WRITE  ( owner generation region x y cols rows z flags
                       session type -- status )
    _PT-RG-TYPE ! _PT-RG-S ! _PT-RG-FLAGS ! _PT-RG-Z ! _PT-RG-ROWS !
    _PT-RG-COLS ! _PT-RG-Y ! _PT-RG-X ! _PT-RG-ID !
    _PT-RG-GENERATION ! _PT-RG-OWNER !
    _PT-RG-X @ _PT-U32? 0= _PT-RG-Y @ _PT-U32? 0= OR
    _PT-RG-COLS @ _PT-U32? 0= OR _PT-RG-ROWS @ _PT-U32? 0= OR
    _PT-RG-Z @ _PT-I32? 0= OR _PT-RG-FLAGS @ _PT-U32? 0= OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-REGION-PAYLOAD 48 0 FILL
    _PT-RG-OWNER @ _PT-REGION-PAYLOAD _PT-U64!
    _PT-RG-GENERATION @ _PT-REGION-PAYLOAD 8 + _PT-U64!
    _PT-RG-ID @ _PT-REGION-PAYLOAD 16 + _PT-U64!
    _PT-RG-X @ _PT-REGION-PAYLOAD 24 + L!
    _PT-RG-Y @ _PT-REGION-PAYLOAD 28 + L!
    _PT-RG-COLS @ _PT-REGION-PAYLOAD 32 + L!
    _PT-RG-ROWS @ _PT-REGION-PAYLOAD 36 + L!
    _PT-RG-Z @ _PT-REGION-PAYLOAD 40 + L!
    _PT-RG-FLAGS @ _PT-REGION-PAYLOAD 44 + L!
    _PT-RG-TYPE @ _PT-REGION-PAYLOAD 48 _PT-RG-S @ PT-PRESENT-OP ;

: PT-REGION-DEFINE  ( owner generation region x y cols rows z flags
                       session -- status )
    _PT-M-REGION-DEFINE _PT-REGION-WRITE ;

: PT-REGION-REPLACE  ( owner generation region x y cols rows z flags
                        session -- status )
    _PT-M-REGION-REPLACE _PT-REGION-WRITE ;

CREATE _PT-REGION-DROP-PAYLOAD 24 ALLOT
VARIABLE _PT-RDROP-S
VARIABLE _PT-RDROP-OWNER
VARIABLE _PT-RDROP-GENERATION
VARIABLE _PT-RDROP-ID
: PT-REGION-DROP  ( owner generation region session -- status )
    _PT-RDROP-S ! _PT-RDROP-ID ! _PT-RDROP-GENERATION ! _PT-RDROP-OWNER !
    _PT-RDROP-OWNER @ 0= _PT-RDROP-GENERATION @ 0= OR
    _PT-RDROP-ID @ 0= OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-RDROP-OWNER @ _PT-REGION-DROP-PAYLOAD _PT-U64!
    _PT-RDROP-GENERATION @ _PT-REGION-DROP-PAYLOAD 8 + _PT-U64!
    _PT-RDROP-ID @ _PT-REGION-DROP-PAYLOAD 16 + _PT-U64!
    _PT-M-REGION-DROP _PT-REGION-DROP-PAYLOAD 24
        _PT-RDROP-S @ PT-PRESENT-OP ;

VARIABLE _PT-PC-S
VARIABLE _PT-PC-DISPOSITION
: PT-PRESENT-COMMIT  ( disposition session -- status )
    _PT-PC-S ! _PT-PC-DISPOSITION !
    _PT-PC-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-PC-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT <> IF PT-S-INVALID EXIT THEN
    _PT-PC-DISPOSITION @ DUP PT-COMMIT =
    SWAP PT-COMMIT-AND-REVEAL = OR 0= IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT.S.SPAN-REMAIN @ IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT.S.TX-SPANS-DONE @ _PT-PC-S @ _PT.S.TX-SPANS @ <>
    _PT-PC-S @ _PT.S.TX-CELLS-DONE @ _PT-PC-S @ _PT.S.TX-CELLS @ <> OR IF
        PT-S-INVALID EXIT
    THEN
    _PT-PC-S @ _PT.S.TX-CELL-MODE @ PT-CELL-NONE = IF
        _PT-PC-S @ _PT.S.CURSOR-DONE? @ IF PT-S-INVALID EXIT THEN
    ELSE
        _PT-PC-S @ _PT.S.CURSOR-DONE? @ 0= IF PT-S-INVALID EXIT THEN
    THEN
    _PT-PC-S @ _PT.S.TX-RET-OPS-DONE @
        _PT-PC-S @ _PT.S.TX-RET-OPS @ <> IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT.S.TX-RET-BYTES-DONE @
        _PT-PC-S @ _PT.S.TX-RET-BYTES @ <> IF PT-S-INVALID EXIT THEN
    _PT-PC-S @ _PT.S.TX-RET-MODE @ DUP PT-RET-NONE =
    SWAP PT-RET-DELTA = OR IF
        _PT-PC-DISPOSITION @ PT-COMMIT <> IF PT-S-INVALID EXIT THEN
    THEN
    _PT-PC-S @ _PT.S.TX-RET-MODE @ DUP PT-RET-REPLACE-START =
    SWAP PT-RET-LAYOUT-START = OR IF
        _PT-PC-DISPOSITION @ PT-COMMIT <> IF PT-S-INVALID EXIT THEN
    THEN
    _PT-M-PRESENT-COMMIT 16 _PT-PC-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-PC-S @ _PT.S.TXID @ _PT-FRAME-PAYLOAD _PT-U64!
    _PT-PC-DISPOSITION @ _PT-FRAME-PAYLOAD 8 + L!
    TRUE _PT-PC-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    TRUE _PT-PC-S @ _PT.S.AWAIT? !
    _PT-AWAIT-PRESENT _PT-PC-S @ _PT.S.AWAIT-KIND !
    _PT-PC-S @ _PT.S.TXID @ _PT-PC-S @ _PT.S.AWAIT-TXID !
    _PT-PC-S @ _PT.S.TX-CELL-MODE @ _PT-PC-S @ _PT.S.AWAIT-CELL-MODE !
    _PT-PC-S @ _PT.S.TX-RET-MODE @ _PT-PC-S @ _PT.S.AWAIT-RET-MODE !
    _PT-PC-DISPOSITION @ _PT-PC-S @ _PT.S.AWAIT-DISPOSITION !
    _PT-PC-S @ _PT-TX-CLEAR
    PT-S-OK ;

VARIABLE _PT-COMMIT-S
: _PT-COMMIT  ( session -- status )
    _PT-COMMIT-S !
    _PT-COMMIT-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-KIND @ _PT-TX-PRESENT = IF
        PT-S-INVALID EXIT
    THEN
    _PT-COMMIT-S @ _PT.S.SPAN-REMAIN @ IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-SPANS-DONE @
        _PT-COMMIT-S @ _PT.S.TX-SPANS @ <> IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-CELLS-DONE @
        _PT-COMMIT-S @ _PT.S.TX-CELLS @ <> IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT.S.CURSOR-DONE? @ 0= IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-SNAPSHOT? @ IF
        _PT-COMMIT-S @ _PT.S.COLS @ _PT-COMMIT-S @ _PT.S.ROWS @ *
        _PT-COMMIT-S @ _PT.S.LAST-END @ <> IF PT-S-INVALID EXIT THEN
        _PT-M-SNAPSHOT-COMMIT
    ELSE _PT-M-TX-COMMIT THEN
    8 _PT-COMMIT-S @ _PT-FRAME-BEGIN ?DUP IF EXIT THEN
    _PT-COMMIT-S @ _PT.S.TXID @ _PT-FRAME-PAYLOAD _PT-U64!
    TRUE _PT-COMMIT-S @ _PT-FRAME-SEND ?DUP IF EXIT THEN
    TRUE _PT-COMMIT-S @ _PT.S.AWAIT? !
    _PT-COMMIT-S @ _PT.S.TXID @ _PT-COMMIT-S @ _PT.S.AWAIT-TXID !
    _PT-COMMIT-S @ _PT.S.TX-SNAPSHOT? @ DUP
        _PT-COMMIT-S @ _PT.S.AWAIT-SNAPSHOT? !
    IF _PT-AWAIT-SNAPSHOT ELSE _PT-AWAIT-CELL THEN
        _PT-COMMIT-S @ _PT.S.AWAIT-KIND !
    _PT-COMMIT-S @ _PT-TX-CLEAR
    PT-S-OK ;

: PT-TX-COMMIT  ( session -- status )
    _PT-COMMIT ;

: PT-TX-ABORT  ( reason session -- status )
    DUP _PT-VALID-S? 0= IF 2DROP PT-S-INVALID EXIT THEN
    DUP _PT-OP-LOST? IF 2DROP PT-S-SESSION-LOST EXIT THEN
    OVER _PT-U16? 0= IF 2DROP PT-S-INVALID EXIT THEN
    DUP _PT.S.TX-OPEN? @ 0= IF 2DROP PT-S-INVALID EXIT THEN
    _PT-ABORT-OPEN-RAW ;
