\ =====================================================================
\  presentation-terminal.f -- optional APT-1 CELL-1 guest client
\ =====================================================================
\
\  This module is deliberately inert when loaded.  PT-START is the only
\  word that emits an APT probe or takes raw UART input ownership.  ANSI
\  remains the baseline before negotiation and after a synchronized close.
\
\  Contract: APT-1-CELL-1-2026-08-24
\  Normative wire text: docs/rich-terminal/APT-1-WIRE.md

PROVIDED presentation-terminal.f

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
512 CONSTANT /PT-SESSION

: PT-SESSION-SIZE  ( -- bytes )  /PT-SESSION ;
: PT-EVENT-SIZE    ( -- bytes )  /PT-EVENT ;

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
: _PT.S.CREDIT-DIRTY?   ( s -- a ) 424 + ;
: _PT.S.GEOMETRY-GEN    ( s -- a ) 432 + ;
: _PT.S.GEOMETRY-SEEN?  ( s -- a ) 440 + ;
: _PT.S.CLOSE-OPENING?  ( s -- a ) 448 + ;

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

: _PT-OP-LOST?  ( s -- flag )
    _PT.S.STATE @ DUP PT-ST-LOST = SWAP PT-ST-CLOSING = OR ;

: _PT-OWNER-RELEASE  ( s -- )
    _PT-OWNER @ OVER = IF DROP 0 _PT-OWNER ! ELSE DROP THEN ;

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
    _PT-M-TX-RESULT = ;

: _PT-INPUT-TYPE?  ( type -- flag )
    DUP _PT-M-KEY = IF DROP TRUE EXIT THEN
    DUP _PT-M-TEXT = IF DROP TRUE EXIT THEN
    DUP _PT-M-POINTER = IF DROP TRUE EXIT THEN
    DUP _PT-M-RESIZE = IF DROP TRUE EXIT THEN
    _PT-M-FOCUS = ;

: _PT-TO-ANSI  ( s -- )
    DUP _PT.S.BIN-U OFF
    DUP _PT.S.EVENT-PENDING OFF
    DUP _PT.S.TX-OPEN? OFF
    DUP _PT.S.AWAIT? OFF
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
    0 _PT-FAIL-S @ _PT.S.TX-OPEN? !
    0 _PT-FAIL-S @ _PT.S.AWAIT? !
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

: _PT-DISPATCH-CREDIT  ( s -- status )
    _PT-RX-LEN @ 8 <> IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ _PT-U64@ OVER _PT.S.PEER-GRANT @ U< IF
        5 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ _PT-U64@ SWAP _PT.S.PEER-GRANT ! PT-S-OK ;

VARIABLE _PT-RES-EXPECTED
: _PT-DISPATCH-TX-RESULT  ( s -- status )
    _PT-RX-LEN @ 20 <> IF
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 10 + W@ 0<> IF
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.AWAIT? @ 0= IF
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ _PT-U64@ OVER _PT.S.AWAIT-TXID @ <> IF
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RX-P @ 8 + W@ DUP 3 U> IF
        DROP 7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    IF
        DUP _PT.S.STATE @ PT-ST-CLOSING = IF
            0 OVER _PT.S.AWAIT? ! DROP PT-S-OK EXIT
        THEN
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    DUP _PT.S.AWAIT-SNAPSHOT? @ IF
        1 _PT-RES-EXPECTED !
    ELSE
        DUP _PT.S.REVISION @ DUP 0xFFFFFFFFFFFFFFFF = IF
            DROP 7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
        1+ _PT-RES-EXPECTED !
    THEN
    _PT-RX-P @ 12 + _PT-U64@ _PT-RES-EXPECTED @ <> IF
        7 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    _PT-RES-EXPECTED @ OVER _PT.S.REVISION !
    0 OVER _PT.S.AWAIT? !
    DUP _PT.S.AWAIT-SNAPSHOT? @ IF
        0 OVER _PT.S.SNAPSHOT? !
        DUP _PT.S.STATE @ PT-ST-CLOSING <> IF
            PT-ST-ACTIVE OVER _PT.S.STATE !
        THEN
    THEN
    0 SWAP _PT.S.AWAIT-SNAPSHOT? ! PT-S-OK ;

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
    0 _PT-SR-S @ _PT.S.TX-OPEN? !
    0 _PT-SR-S @ _PT.S.SPAN-REMAIN !
    PT-S-OK ;

: _PT-RESET-CLOSE  ( s -- status )
    DUP _PT-SR-S !
    4 OVER _PT-SEND-CLOSE ?DUP IF NIP EXIT THEN
    4 _PT-SR-S @ _PT.S.CLOSE-REASON !
    FALSE _PT-SR-S @ _PT.S.CLOSE-OPENING? !
    0 _PT-SR-S @ _PT.S.TX-OPEN? !
    0 _PT-SR-S @ _PT.S.SPAN-REMAIN !
    0 _PT-SR-S @ _PT.S.EVENT-PENDING !
    PT-ST-CLOSING _PT-SR-S @ _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + _PT-SR-S @ _PT.S.DEADLINE !
    DROP PT-S-OK ;

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
    _PT-SR-S @ _PT.S.TX-SEQ @
    _PT-SR-S @ _PT.S.TX-OPEN? @ IF
        0xFFFFFFFFFFFFFFFC
    ELSE
        0xFFFFFFFFFFFFFFFD
    THEN U> IF _PT-RESET-CLOSE EXIT THEN
    DUP _PT-DISCARD-PENDING-EVENT ?DUP IF NIP EXIT THEN
    0 OVER _PT-ABORT-OPEN-RAW ?DUP IF NIP EXIT THEN
    0 OVER _PT.S.AWAIT? !
    0 OVER _PT.S.AWAIT-SNAPSHOT? !
    _PT-SR-EPOCH @ OVER _PT.S.EPOCH !
    0 OVER _PT.S.REVISION !
    1 OVER _PT.S.NEXT-TXID !
    TRUE OVER _PT.S.SNAPSHOT? !
    PT-ST-RESYNCING OVER _PT.S.STATE !
    _PT-SR-EPOCH @ SWAP _PT-SEND-RESET-ACK ;

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
        0 OVER _PT.S.TX-OPEN? !
        0 OVER _PT.S.SPAN-REMAIN !
        0 SWAP _PT.S.AWAIT? ! PT-S-OK EXIT
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
: _PT-DISPATCH-RESIZE  ( s -- status )
    DUP _PT-INPUT-STATE? 0= IF
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
    DUP _PT.S.GEOMETRY-SEEN? @ IF
        _PT-RSZ-GEN @ OVER _PT.S.GEOMETRY-GEN @ _PT-U<= IF
            8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
        THEN
    THEN
    DUP _PT.S.STATE @ PT-ST-CLOSING = IF _PT-ACCEPT-EVENT EXIT THEN
    DUP _PT.S.TX-OPEN? @ OVER _PT.S.AWAIT? @ OR IF
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
    DUP 0xFFFFFFFFFFFFFFFF 176 - U> IF
        DROP 8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
    THEN
    176 + _PT-RSZ-SNAPSHOT !
    _PT-RSZ-SNAPSHOT @ OVER _PT.S.PEER-MAX-TX @ U> IF
        8 _PT-RX-TYPE @ _PT-RX-SEQNO @ ROT _PT-SEMANTIC-FAIL EXIT
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
    0 OVER _PT.S.REVISION !
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
    PT-ST-CLOSING _PT-SVC-S @ _PT.S.STATE !
    MS@ _PT-TIMEOUT-MS + _PT-SVC-S @ _PT.S.DEADLINE !
    2DROP PT-S-OK ;

: _PT-SERVICE-BINARY  ( s -- status )
    _PT-SVC-S ! 0 _PT-SVC-N !
    BEGIN _PT-SVC-N @ _PT-SERVICE-BYTES U< WHILE
        _PT-SVC-S @ _PT-TRY-FRAME IF
            DUP PT-S-OK <> IF EXIT THEN DROP
            _PT-SVC-N @ _PT-RX-TOTAL @ + _PT-SVC-N !
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
    DUP _PT.S.TX-OPEN? @ IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.SPAN-REMAIN @ IF DROP PT-S-OK EXIT THEN
    DUP _PT-SEND-CREDIT ?DUP IF NIP EXIT THEN
    0 SWAP _PT.S.CREDIT-DIRTY? ! PT-S-OK ;

: PT-SERVICE  ( session -- status )
    DUP _PT-VALID-S? 0= IF DROP PT-S-INVALID EXIT THEN
    DUP _PT-SVC-S !
    DUP _PT.S.STATE @ PT-ST-ANSI = IF DROP PT-S-OK EXIT THEN
    DUP _PT.S.STATE @ PT-ST-LOST = IF DROP PT-S-SESSION-LOST EXIT THEN
    DUP _PT.S.STATE @ PT-ST-PROBING = IF _PT-SERVICE-PROBE EXIT THEN
    DUP _PT.S.STATE @ PT-ST-ACTIVE =
    OVER _PT.S.STATE @ PT-ST-RESYNCING = OR IF
        _PT-SVC-S @ _PT.S.TX-SEQ @ 0xFFFFFFFFFFFFFFFE _PT-U>=
        _PT-SVC-S @ _PT.S.TX-OPEN? @ 0= AND IF
            FALSE _PT-SVC-S @ _PT.S.CLOSE-OPENING? !
            0 _PT-SVC-S @ _PT.S.EVENT-PENDING !
            DROP 2 _PT-SVC-S @ _PT-BEGIN-CLOSE EXIT
        THEN
        DUP _PT-SERVICE-CREDIT ?DUP IF NIP EXIT THEN
    THEN
    DUP _PT-SERVICE-BINARY ?DUP IF NIP EXIT THEN
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
    DUP _PT.S.STATE @ PT-ST-OPENING = OVER _PT.S.CLOSE-OPENING? !
    0 OVER _PT.S.TX-OPEN? !
    0 OVER _PT.S.SPAN-REMAIN !
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

\ =====================================================================
\  CELL-1 transaction builder
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
    1+ 0xFFFFFFFFFFFFFFFF SWAP -
    _PT-B-S @ _PT.S.TX-SEQ @ U< IF
        FALSE _PT-B-SEQ-ROOM? ! FALSE EXIT
    THEN
    _PT-B-SPANS @ 52 *
    _PT-B-CELLS @ 8 * + 176 + DUP _PT-B-BYTES !
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
    ELSE
        _PT-B-S @ _PT.S.STATE @ PT-ST-ACTIVE <> IF PT-S-INVALID EXIT THEN
        _PT-B-S @ _PT.S.SNAPSHOT? @ IF PT-S-INVALID EXIT THEN
        _PT-B-S @ _PT.S.REVISION @ 0xFFFFFFFFFFFFFFFF = IF
            PT-ST-LOST _PT-B-S @ _PT.S.STATE !
            PT-S-SESSION-LOST EXIT
        THEN
    THEN
    _PT-B-S @ _PT.S.AWAIT? @ IF PT-S-WOULD-BLOCK EXIT THEN
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

VARIABLE _PT-COMMIT-S
: _PT-COMMIT  ( session -- status )
    _PT-COMMIT-S !
    _PT-COMMIT-S @ _PT-VALID-S? 0= IF PT-S-INVALID EXIT THEN
    _PT-COMMIT-S @ _PT-OP-LOST? IF PT-S-SESSION-LOST EXIT THEN
    _PT-COMMIT-S @ _PT.S.TX-OPEN? @ 0= IF PT-S-INVALID EXIT THEN
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
    0 _PT-COMMIT-S @ _PT.S.TX-OPEN? !
    TRUE _PT-COMMIT-S @ _PT.S.AWAIT? !
    _PT-COMMIT-S @ _PT.S.TXID @ _PT-COMMIT-S @ _PT.S.AWAIT-TXID !
    _PT-COMMIT-S @ _PT.S.TX-SNAPSHOT?
        @ _PT-COMMIT-S @ _PT.S.AWAIT-SNAPSHOT? !
    PT-S-OK ;

: PT-TX-COMMIT  ( session -- status )
    _PT-COMMIT ;

: PT-TX-ABORT  ( reason session -- status )
    DUP _PT-VALID-S? 0= IF 2DROP PT-S-INVALID EXIT THEN
    DUP _PT-OP-LOST? IF 2DROP PT-S-SESSION-LOST EXIT THEN
    OVER _PT-U16? 0= IF 2DROP PT-S-INVALID EXIT THEN
    DUP _PT.S.TX-OPEN? @ 0= IF 2DROP PT-S-INVALID EXIT THEN
    _PT-ABORT-OPEN-RAW ;
