\ =====================================================================
\  chiplet.f -- KDOS system module for chiplets
\ =====================================================================
\
\  Two layers in one module (release plan Part 9a, decided 2026-11-11):
\
\  - The general chiplet words serve any chiplet and name none: the
\    ports, identity, lending, boot, stop, reset and doorbells of
\    docs/chiplet-link.md, lendable memory for each port, and the queue
\    pairs and offers of its section 8.
\  - Above them, managed data's words (docs/managed-data.md section 14):
\    MegaPad's side of the function of class 1, which LM1's collector
\    serves.  Its records, windows, handles, store logs, regions and
\    failure handling follow that document.
\
\  Load after KDOS:  REQUIRE chiplet.f
\
\  Locks: CHIP-LOCK (13) guards the port window's shared PORT_SEL and the
\  lend pool; OBJ-LOCK (14) guards managed data's handle table, owners and
\  shape registry.  Neither is held across a wait.
\
\  Waiting: a core waits for a chiplet with the loop of _CHIP-SLEEP.  It
\  reads MBOX_STATUS, at which the hosted simulator steps its chiplets,
\  acknowledges the port's doorbell, looks at its queues, and then sleeps
\  with IDLE-UNTIL until a signal or the next millisecond.  A doorbell that
\  arrives after the acknowledgement keeps the core's pending bit set, so
\  the sleep that follows ends at once and no signal is lost.

PROVIDED chiplet.f

\ =====================================================================
\  Status codes and settings
\ =====================================================================

-1001 CONSTANT CHIP-E-ABSENT     \ no chiplet on the port, or no such port
-1002 CONSTANT CHIP-E-TIMEOUT    \ the chiplet missed a deadline
-1003 CONSTANT CHIP-E-BROKEN     \ a queue the chiplet writes broke
-1004 CONSTANT CHIP-E-NOMEM      \ no lendable memory, or no free lent range
-1005 CONSTANT CHIP-E-REFUSED    \ the chiplet refused a request

13 CONSTANT CHIP-LOCK

\ The deadline MegaPad gives each request, in milliseconds.  The bounds
\ of managed-data.md section 16 are still to be measured.
VARIABLE CHIP-DEADLINE   100 CHIP-DEADLINE !

\ =====================================================================
\  The port window (docs/chiplet-link.md sections 1 and 2)
\ =====================================================================

0xFFFFFF0000000E00 CONSTANT _CHIP-WIN
0xFFFFFF0000000370 CONSTANT _CHIP-INFO       \ System Info CHIPLET_PORTS
0xFFFFFF0000000508 CONSTANT _MBOX-SEND
0xFFFFFF0000000509 CONSTANT _MBOX-STATUS
0xFFFFFF000000050A CONSTANT _MBOX-ACK

0x80 CONSTANT _P-CTRL        0x88 CONSTANT _P-STATUS
0x90 CONSTANT _P-FAULT-ADDR  0x98 CONSTANT _P-FAULT-INFO
0xA0 CONSTANT _P-RANGE-SEL   0xA8 CONSTANT _P-RANGE-BASE
0xB0 CONSTANT _P-RANGE-LIMIT 0xB8 CONSTANT _P-RANGE-RIGHTS
0xC8 CONSTANT _P-ID-INDEX    0xD0 CONSTANT _P-ID-DATA
0xD8 CONSTANT _P-BOOT-ADDR   0xE0 CONSTANT _P-BOOT-LEN
0xE8 CONSTANT _P-BOOT-ARG

1 CONSTANT _CTRL-ENABLE      2 CONSTANT _CTRL-RESET-HOLD
4 CONSTANT _CTRL-STOP-REQ    8 CONSTANT _CTRL-DOORBELL-EN
32 CONSTANT _CTRL-BORROW-EN
1 CONSTANT _ST-PRESENT       2 CONSTANT _ST-STOPPED
4 CONSTANT _ST-READY         8 CONSTANT _ST-BUSY
16 CONSTANT _ST-FAULT

1 CONSTANT CHIP-READ
3 CONSTANT CHIP-READ-WRITE

\ PORT_COUNT, which System Info repeats and which reads zero on a die
\ built without ports.
: CHIP-PORTS   ( -- n )   _CHIP-INFO @ 0xFF AND ;
: CHIP-RANGES  ( -- n )   _CHIP-INFO @ 16 RSHIFT 0xFF AND ;
\ The mailbox ID of a port: the first port's ID plus its number.
: CHIP-ID      ( port -- id )   _CHIP-INFO @ 8 RSHIFT 0xFF AND + ;

\ The selected port's registers.  PORT_SEL is shared by every core, so
\ the callers of these hold CHIP-LOCK.
: _CHIP-SEL  ( port -- )   _CHIP-WIN 8 + ! ;
: _CHIP@     ( offset port -- x )   _CHIP-SEL _CHIP-WIN + @ ;
: _CHIP!     ( x offset port -- )   _CHIP-SEL _CHIP-WIN + ! ;

: CHIP@  ( offset port -- x )   CHIP-LOCK LOCK _CHIP@ CHIP-LOCK UNLOCK ;
: CHIP!  ( x offset port -- )   CHIP-LOCK LOCK _CHIP! CHIP-LOCK UNLOCK ;

: CHIP-PRESENT?  ( port -- flag )
    DUP 0< OVER CHIP-PORTS < 0= OR IF DROP FALSE EXIT THEN
    _P-STATUS SWAP CHIP@ _ST-PRESENT AND 0<> ;

\ One word of the chiplet's identity record, read over the sideband.
: CHIP-ID@  ( index port -- x )
    CHIP-LOCK LOCK
    TUCK _P-ID-INDEX SWAP _CHIP!
    _P-ID-DATA SWAP _CHIP@
    CHIP-LOCK UNLOCK ;

\ Install a lent range in one step from its staged base and limit; rights
\ of zero withdraw it.  The caller holds CHIP-LOCK.
: _CHIP-RANGE!  ( base limit rights slot port -- )
    >R  _P-RANGE-SEL R@ _CHIP!
    ROT _P-RANGE-BASE R@ _CHIP!
    SWAP _P-RANGE-LIMIT R@ _CHIP!
    _P-RANGE-RIGHTS R> _CHIP! ;

: CHIP-LEND  ( base limit rights slot port -- ior )
    DUP CHIP-PRESENT? 0= IF 2DROP 2DROP DROP CHIP-E-ABSENT EXIT THEN
    OVER CHIP-RANGES U< 0= IF 2DROP 2DROP DROP CHIP-E-NOMEM EXIT THEN
    CHIP-LOCK LOCK _CHIP-RANGE! CHIP-LOCK UNLOCK 0 ;

: CHIP-UNLEND  ( slot port -- )
    >R >R 0 0 0 R> R> CHIP-LOCK LOCK _CHIP-RANGE! CHIP-LOCK UNLOCK ;

\ Read and clear the refusal record.
: CHIP-FAULT@  ( port -- addr info )
    CHIP-LOCK LOCK
    _P-FAULT-ADDR OVER _CHIP@  _P-FAULT-INFO 2 PICK _CHIP@
    _ST-FAULT _P-STATUS 3 PICK _CHIP!
    ROT DROP
    CHIP-LOCK UNLOCK ;

\ Signal the port as this core, and take the port's signal to this core.
\ Reading MBOX_STATUS first lets the hosted simulator step its chiplets.
: CHIP-RING  ( port -- )   CHIP-ID _MBOX-SEND C! ;
: _CHIP-ACK  ( port -- )   _MBOX-STATUS C@ DROP  CHIP-ID _MBOX-ACK C! ;
\ Ring a core, as the mailbox lets one core signal another.
: _CHIP-RING-CORE  ( core -- )   _MBOX-SEND C! ;

\ =====================================================================
\  Deadlines and sleeping
\ =====================================================================

: CHIP-DEADLINE@  ( -- ms )   MS@ CHIP-DEADLINE @ + ;
: _CHIP-PAST?     ( deadline -- flag )   MS@ SWAP U> ;
\ Sleep until a signal or the next millisecond, never past DEADLINE.
: _CHIP-NAP  ( deadline -- )   MS@ 1+ MIN IDLE-UNTIL ;

\ =====================================================================
\  Private memory and the lend pool
\ =====================================================================
\
\  A port's lendable memory comes from external memory in chunks, each
\  lent to the port as one range: what MegaPad writes in chunks lent for
\  reading, what the chiplet writes in chunks lent for reading and
\  writing.  Each chunk belongs to an owner, a function or the port
\  itself, so the owner's memory goes back to KDOS whole.  An owner's
\  records are carved from its chunks; a chunk that fills is followed by
\  one twice the size of all before it.  A block, such as one of managed
\  data's heap blocks, is a chunk of its own, which can go back alone.
\  The port's lent ranges bound how many chunks a port holds at once.

\ Zeroed external memory of MegaPad's own, never lent.
: _CHIP-PRIVATE  ( u -- addr ior )
    DUP >R XMEM-ALLOT? ?DUP IF R> DROP EXIT THEN
    DUP R> 0 FILL 0 ;

10 CELLS CONSTANT _CK
\ Chunk fields.
0 CONSTANT CK.USED   1 CONSTANT CK.PORT   2 CONSTANT CK.OWNER
3 CONSTANT CK.RIGHTS 4 CONSTANT CK.SLOT   5 CONSTANT CK.BASE
6 CONSTANT CK.LIMIT  7 CONSTANT CK.HERE   8 CONSTANT CK.RAW
9 CONSTANT CK.KIND                        \ 0 an arena, 1 a block

VARIABLE _CHUNKS     0 _CHUNKS !
: _CHUNK-N  ( -- n )   CHIP-PORTS CHIP-RANGES * ;
: _CHUNK    ( i -- addr )   _CK * _CHUNKS @ + ;
: CK@  ( chunk field -- x )   CELLS + @ ;
: CK!  ( x chunk field -- )   CELLS + ! ;

: _CHUNKS-READY  ( -- ior )
    _CHUNKS @ IF 0 EXIT THEN
    _CHUNK-N 1 MAX _CK * _CHIP-PRIVATE ?DUP IF NIP EXIT THEN
    _CHUNKS ! 0 ;

\ Each port's image slot while it boots, or -1.
CREATE _CHIP-IMAGE-SLOT  CHIP-PORTS 1 MAX CELLS ALLOT
_CHIP-IMAGE-SLOT CHIP-PORTS 1 MAX CELLS -1 FILL

: _SLOT-USED?  ( slot port -- flag )
    2DUP CELLS _CHIP-IMAGE-SLOT + @ = IF 2DROP TRUE EXIT THEN
    _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ IF
            DUP CK.PORT CK@ 2 PICK = SWAP CK.SLOT CK@ 3 PICK = AND IF
                2DROP TRUE UNLOOP EXIT
            THEN
        ELSE DROP THEN
    LOOP 2DROP FALSE ;

\ A lent range slot the port does not use, or -1.
: _FREE-SLOT  ( port -- slot )
    CHIP-RANGES 0 ?DO
        I OVER _SLOT-USED? 0= IF DROP I UNLOOP EXIT THEN
    LOOP DROP -1 ;

: _FREE-CHUNK  ( -- chunk | 0 )
    _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ 0= IF UNLOOP EXIT THEN DROP
    LOOP 0 ;

\ The bytes the owner's arena chunks of these rights on the port hold.
: _OWNER-BYTES  ( rights owner port -- u )
    0 _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ IF
            DUP CK.PORT CK@ 3 PICK = OVER CK.OWNER CK@ 5 PICK = AND
            OVER CK.RIGHTS CK@ 6 PICK = AND OVER CK.KIND CK@ 0= AND IF
                DUP CK.LIMIT CK@ SWAP CK.BASE CK@ - +
            ELSE DROP THEN
        ELSE DROP THEN
    LOOP NIP NIP NIP ;

VARIABLE _NEW-SIZE   VARIABLE _NEW-RIGHTS   VARIABLE _NEW-OWNER
VARIABLE _NEW-PORT   VARIABLE _NEW-KIND

\ A new chunk of _NEW-SIZE bytes, 64-byte aligned, zeroed and lent.  The
\ caller holds CHIP-LOCK.
: _CHUNK-MAKE  ( -- chunk ior )
    _CHUNKS-READY ?DUP IF 0 SWAP EXIT THEN
    _FREE-CHUNK ?DUP 0= IF 0 CHIP-E-NOMEM EXIT THEN
    _NEW-PORT @ _FREE-SLOT DUP 0< IF 2DROP 0 CHIP-E-NOMEM EXIT THEN
    ( chunk slot )
    _NEW-SIZE @ 64 + XMEM-ALLOT? IF 2DROP DROP 0 CHIP-E-NOMEM EXIT THEN
    ( chunk slot raw )
    2 PICK CK.RAW CK!
    OVER CK.SLOT CK!
    DUP CK.RAW CK@ 63 + -64 AND OVER CK.BASE CK!
    DUP CK.BASE CK@ _NEW-SIZE @ + OVER CK.LIMIT CK!
    DUP CK.BASE CK@ OVER CK.HERE CK!
    _NEW-PORT @ OVER CK.PORT CK!
    _NEW-OWNER @ OVER CK.OWNER CK!
    _NEW-RIGHTS @ OVER CK.RIGHTS CK!
    _NEW-KIND @ OVER CK.KIND CK!
    DUP CK.BASE CK@ _NEW-SIZE @ 0 FILL
    TRUE OVER CK.USED CK!
    DUP CK.BASE CK@ OVER CK.LIMIT CK@ 2 PICK CK.RIGHTS CK@
    3 PICK CK.SLOT CK@ 4 PICK CK.PORT CK@ _CHIP-RANGE!
    0 ;

: _CHUNK-DROP  ( chunk -- )   \ the caller holds CHIP-LOCK
    DUP CK.SLOT CK@ OVER CK.PORT CK@ >R >R 0 0 0 R> R> _CHIP-RANGE!
    DUP CK.RAW CK@ OVER CK.LIMIT CK@ 2 PICK CK.BASE CK@ - 64 + XMEM-FREE-BLOCK
    _CK 0 FILL ;

: _ROUND64  ( u -- u' )   63 + -64 AND ;

\ U bytes of zeroed lendable memory for OWNER on PORT, lent with RIGHTS.
: CHIP-LEND-ALLOT  ( u rights owner port -- addr ior )
    _NEW-PORT ! _NEW-OWNER ! _NEW-RIGHTS ! _ROUND64 _NEW-SIZE !
    CHIP-LOCK LOCK
    _CHUNKS-READY ?DUP IF CHIP-LOCK UNLOCK 0 SWAP EXIT THEN
    _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ OVER CK.KIND CK@ 0= AND
        OVER CK.PORT CK@ _NEW-PORT @ = AND
        OVER CK.OWNER CK@ _NEW-OWNER @ = AND
        OVER CK.RIGHTS CK@ _NEW-RIGHTS @ = AND IF
            DUP CK.LIMIT CK@ OVER CK.HERE CK@ - _NEW-SIZE @ U< 0= IF
                DUP CK.HERE CK@ TUCK _NEW-SIZE @ + SWAP CK.HERE CK!
                CHIP-LOCK UNLOCK 0 UNLOOP EXIT
            THEN
        THEN DROP
    LOOP
    \ A new chunk, at least the request and twice the owner's chunks so far.
    _NEW-SIZE @  _NEW-RIGHTS @ _NEW-OWNER @ _NEW-PORT @ _OWNER-BYTES 2*  4096 MAX MAX
    _NEW-SIZE @ SWAP _NEW-SIZE !  ( asked )
    0 _NEW-KIND !
    _CHUNK-MAKE ?DUP IF NIP NIP CHIP-LOCK UNLOCK 0 SWAP EXIT THEN
    ( asked chunk )
    DUP CK.HERE CK@ ROT OVER + ROT CK.HERE CK!
    CHIP-LOCK UNLOCK 0 ;

\ A block of U bytes, a chunk of its own lent for reading and writing.
: CHIP-LEND-BLOCK  ( u owner port -- addr ior )
    _NEW-PORT ! _NEW-OWNER ! _ROUND64 _NEW-SIZE !
    CHIP-READ-WRITE _NEW-RIGHTS !  1 _NEW-KIND !
    CHIP-LOCK LOCK
    _CHUNK-MAKE ?DUP IF NIP CHIP-LOCK UNLOCK 0 SWAP EXIT THEN
    DUP CK.LIMIT CK@ OVER CK.HERE CK!
    CK.BASE CK@
    CHIP-LOCK UNLOCK 0 ;

\ Withdraw and free the block at ADDR.
: CHIP-UNLEND-BLOCK  ( addr port -- )
    CHIP-LOCK LOCK
    _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ OVER CK.KIND CK@ 1 = AND
        OVER CK.PORT CK@ 3 PICK = AND OVER CK.BASE CK@ 4 PICK = AND IF
            _CHUNK-DROP
        ELSE DROP THEN
    LOOP 2DROP
    CHIP-LOCK UNLOCK ;

\ Withdraw and free every chunk OWNER holds on PORT.
: CHIP-UNLEND-OWNER  ( owner port -- )
    CHIP-LOCK LOCK
    _CHUNKS @ IF
        _CHUNK-N 0 ?DO
            I _CHUNK DUP CK.USED CK@ OVER CK.PORT CK@ 3 PICK = AND
            OVER CK.OWNER CK@ 4 PICK = AND IF _CHUNK-DROP ELSE DROP THEN
        LOOP
    THEN 2DROP
    CHIP-LOCK UNLOCK ;

\ The bytes OWNER holds lent on PORT, its blocks included.
: CHIP-LENT-BYTES  ( owner port -- u )
    0 _CHUNKS @ 0= IF NIP NIP EXIT THEN
    _CHUNK-N 0 ?DO
        I _CHUNK DUP CK.USED CK@ OVER CK.PORT CK@ 4 PICK = AND
        OVER CK.OWNER CK@ 5 PICK = AND IF
            DUP CK.LIMIT CK@ SWAP CK.BASE CK@ - +
        ELSE DROP THEN
    LOOP NIP NIP ;

\ =====================================================================
\  Queues and queue pairs (docs/chiplet-link.md section 8)
\ =====================================================================
\
\  A pair as MegaPad keeps it, in its own memory:
\    the four parts' addresses, the two capacities, the core the chiplet
\    rings and the port; MegaPad's counts (TAIL of the queue to the
\    chiplet, HEAD of the queue from it) and the chiplet's as last read;
\    the tag of MegaPad's request in flight and its reply; the word that
\    serves the chiplet's requests ( entry pair -- ), and one that takes
\    a reply MegaPad does not wait for ( pair -- ).

0 CONSTANT P.TO-RING     1 CONSTANT P.TO-HEAD    2 CONSTANT P.FROM-RING
3 CONSTANT P.FROM-HEAD   4 CONSTANT P.TO-CAP     5 CONSTANT P.FROM-CAP
6 CONSTANT P.NOTIFY      7 CONSTANT P.PORT       8 CONSTANT P.TAIL
9 CONSTANT P.HEAD-SEEN   10 CONSTANT P.HEAD      11 CONSTANT P.TAIL-SEEN
12 CONSTANT P.TAG        13 CONSTANT P.READY     14 CONSTANT P.HANDLER
15 CONSTANT P.DONE       16 CONSTANT P.REPLY     \ eight cells
24 CELLS CONSTANT _PAIR-SIZE

: P@  ( pair field -- x )   CELLS + @ ;
: P!  ( x pair field -- )   CELLS + ! ;
: P'  ( pair field -- addr )   CELLS + ;

\ An entry's words.
0 CONSTANT E.HEADER   1 CONSTANT E.TAG     2 CONSTANT E.IN-ADDR
3 CONSTANT E.IN-LEN   4 CONSTANT E.OUT-ADDR 5 CONSTANT E.OUT-LEN
6 CONSTANT E.ARG0     7 CONSTANT E.ARG1
64 CONSTANT _ENTRY
0x10000 CONSTANT _REPLY-BIT

: E@  ( entry field -- x )   CELLS + @ ;
: E!  ( x entry field -- )   CELLS + ! ;
: E-OP      ( entry -- op )   @ 0xFFFF AND ;
: E-STATUS  ( entry -- st )   @ 32 RSHIFT ;
: E-REPLY?  ( entry -- flag )   @ _REPLY-BIT AND 0<> ;

0 CONSTANT CHIP-OK       1 CONSTANT CHIP-UNSUPPORTED
2 CONSTANT CHIP-CANCELLED   3 CONSTANT CHIP-FAILED   4 CONSTANT CHIP-REFUSED
1 CONSTANT _OP-CANCEL    2 CONSTANT _OP-OFFER

\ Per-core scratch entries: one each for taking, replying and asking.
CREATE _CHIP-SCRATCH  NCORES 3 * _ENTRY * ALLOT
: _TAKE-BUF   ( -- entry )   COREID 3 * _ENTRY * _CHIP-SCRATCH + ;
: _SEND-BUF   ( -- entry )   _TAKE-BUF _ENTRY + ;
: _ASK-BUF    ( -- entry )   _TAKE-BUF _ENTRY 2* + ;
: _ENTRY-CLEAR  ( entry -- )   _ENTRY 0 FILL ;

VARIABLE _MK-TO     VARIABLE _MK-FROM   VARIABLE _MK-NOTIFY
VARIABLE _MK-OWNER  VARIABLE _MK-PORT   VARIABLE _MK-PAIR

\ A pair for OWNER on PORT whose queues hold TO and FROM entries, and
\ whose entries the chiplet signals to core NOTIFY.
: CHIP-PAIR-MAKE  ( to from notify owner port -- pair ior )
    _MK-PORT ! _MK-OWNER ! _MK-NOTIFY ! _MK-FROM ! _MK-TO !
    _PAIR-SIZE _CHIP-PRIVATE ?DUP IF EXIT THEN _MK-PAIR !
    _MK-TO @ _MK-PAIR @ P.TO-CAP P!   _MK-FROM @ _MK-PAIR @ P.FROM-CAP P!
    _MK-NOTIFY @ _MK-PAIR @ P.NOTIFY P!   _MK-PORT @ _MK-PAIR @ P.PORT P!
    _MK-TO @ 1+ _ENTRY * CHIP-READ _MK-OWNER @ _MK-PORT @ CHIP-LEND-ALLOT
    ?DUP IF NIP 0 SWAP EXIT THEN _MK-PAIR @ P.TO-RING P!
    _ENTRY CHIP-READ-WRITE _MK-OWNER @ _MK-PORT @ CHIP-LEND-ALLOT
    ?DUP IF NIP 0 SWAP EXIT THEN _MK-PAIR @ P.TO-HEAD P!
    _MK-FROM @ 1+ _ENTRY * CHIP-READ-WRITE _MK-OWNER @ _MK-PORT @ CHIP-LEND-ALLOT
    ?DUP IF NIP 0 SWAP EXIT THEN _MK-PAIR @ P.FROM-RING P!
    _ENTRY CHIP-READ _MK-OWNER @ _MK-PORT @ CHIP-LEND-ALLOT
    ?DUP IF NIP 0 SWAP EXIT THEN _MK-PAIR @ P.FROM-HEAD P!
    _MK-PAIR @ 0 ;

\ The six-word record that names the pair to the chiplet.
: CHIP-PAIR-RECORD!  ( pair addr -- )
    >R
    DUP P.TO-RING P@ R@ !
    DUP P.TO-HEAD P@ R@ 8 + !
    DUP P.FROM-RING P@ R@ 16 + !
    DUP P.FROM-HEAD P@ R@ 24 + !
    DUP P.TO-CAP P@ OVER P.FROM-CAP P@ 32 LSHIFT OR R@ 32 + !
    P.NOTIFY P@ R> 40 + ! ;

\ Put ENTRY on the queue to the chiplet: 0 put, 1 full, or CHIP-E-BROKEN
\ when the chiplet's HEAD moved backwards or past MegaPad's TAIL.
: _PAIR-PUT  ( entry pair -- n )
    DUP P.TAIL P@ OVER P.HEAD-SEEN P@ - OVER P.TO-CAP P@ U< 0= IF
        DUP P.TO-HEAD P@ @
        DUP 2 PICK P.HEAD-SEEN P@ U< OVER 3 PICK P.TAIL P@ U> OR IF
            DROP 2DROP CHIP-E-BROKEN EXIT
        THEN
        OVER P.HEAD-SEEN P!
        DUP P.TAIL P@ OVER P.HEAD-SEEN P@ - OVER P.TO-CAP P@ U< 0= IF
            2DROP 1 EXIT
        THEN
    THEN
    \ The entry, then TAIL, then the doorbell.
    DUP P.TAIL P@ OVER P.TO-CAP P@ 1- AND 1+ _ENTRY * OVER P.TO-RING P@ +
    ROT SWAP _ENTRY MOVE
    DUP P.TAIL P@ 1+ OVER P.TAIL P!
    DUP P.TAIL P@ OVER P.TO-RING P@ !
    P.PORT P@ CHIP-RING 0 ;

\ Take the next entry from the chiplet into BUF: true when there was one.
: _PAIR-TAKE  ( buf pair -- flag ior )
    DUP P.FROM-RING P@ @
    DUP 2 PICK P.TAIL-SEEN P@ U<
    OVER 3 PICK P.HEAD P@ - 3 PICK P.FROM-CAP P@ U> OR IF
        DROP 2DROP FALSE CHIP-E-BROKEN EXIT
    THEN
    OVER P.TAIL-SEEN P!
    DUP P.HEAD P@ OVER P.TAIL-SEEN P@ = IF 2DROP FALSE 0 EXIT THEN
    DUP P.HEAD P@ OVER P.FROM-CAP P@ 1- AND 1+ _ENTRY * OVER P.FROM-RING P@ +
    ROT _ENTRY MOVE
    DUP P.HEAD P@ 1+ OVER P.HEAD P!
    DUP P.HEAD P@ SWAP P.FROM-HEAD P@ !
    TRUE 0 ;

\ Begin a reply to REQUEST, with STATUS, in this core's send entry.
: CHIP-REPLY-BEGIN  ( status request -- entry )
    _SEND-BUF _ENTRY-CLEAR
    DUP E-OP _REPLY-BIT OR ROT 32 LSHIFT OR _SEND-BUF !
    E.TAG E@ _SEND-BUF E.TAG E!  _SEND-BUF ;

\ Put ENTRY on PAIR, waiting for room until the deadline.
: CHIP-PUT  ( entry pair -- ior )
    CHIP-DEADLINE@ >R
    BEGIN
        2DUP _PAIR-PUT DUP 1 =
    WHILE
        DROP R@ _CHIP-PAST? IF 2DROP R> DROP CHIP-E-TIMEOUT EXIT THEN
        DUP P.PORT P@ _CHIP-ACK  R@ _CHIP-NAP
    REPEAT
    NIP NIP R> DROP ;

: CHIP-REPLY  ( status request pair -- ior )   >R CHIP-REPLY-BEGIN R> CHIP-PUT ;

\ A broken queue, or a chiplet that missed a deadline, ends the port's
\ service: the functions on the port hear of it, and the port is reset.
DEFER _CHIP-FAILED   ( cause port -- )
: _CHIP-FAILED-NONE  ( cause port -- )   2DROP ;
' _CHIP-FAILED-NONE IS _CHIP-FAILED

\ Take every entry waiting on PAIR: the reply MegaPad waits for, replies to
\ requests it gave up on, and the chiplet's own requests.
: CHIP-PAIR-SERVE  ( pair -- ior )
    BEGIN
        _TAKE-BUF OVER _PAIR-TAKE ?DUP IF NIP NIP EXIT THEN
    WHILE
        _TAKE-BUF E-REPLY? IF
            _TAKE-BUF E.TAG E@ OVER P.TAG P@ = OVER P.READY P@ 0= AND IF
                _TAKE-BUF OVER P.REPLY P' _ENTRY MOVE
                TRUE OVER P.READY P!
                DUP P.DONE P@ ?DUP IF OVER SWAP EXECUTE THEN
            THEN
        ELSE
            _TAKE-BUF E-OP _OP-CANCEL <> IF
                DUP P.HANDLER P@ ?DUP IF
                    _TAKE-BUF 2 PICK ROT EXECUTE
                ELSE
                    CHIP-UNSUPPORTED _TAKE-BUF 2 PICK CHIP-REPLY DROP
                THEN
            THEN
        THEN
    REPEAT DROP 0 ;

\ =====================================================================
\  Ports in use: their control queues, boot and reset
\ =====================================================================
\
\  Each port MegaPad boots has a control block, CHIPCTRL, lent for
\  reading, naming the control queues' pair, whose doorbells reach the
\  core that booted the port.  That core serves the control queues and
\  the pairs the port's functions are granted.

0 CONSTANT PS.BOOTED   1 CONSTANT PS.CONTROL   2 CONSTANT PS.PAIR
3 CONSTANT PS.SERVED   4 CONSTANT PS.DOWN
5 CELLS CONSTANT _PS
CREATE _CHIP-PORT-STATE  CHIP-PORTS 1 MAX _PS * ALLOT
_CHIP-PORT-STATE CHIP-PORTS 1 MAX _PS * 0 FILL
: PS'  ( port field -- addr )   CELLS SWAP _PS * + _CHIP-PORT-STATE + ;

0x4C52544350494843 CONSTANT _CHIPCTRL     \ ASCII CHIPCTRL

\ Words that serve a function's OFFER, by class: ( entry pair -- ).
VARIABLE _CHIP-CLASSES   0 _CHIP-CLASSES !
: CHIP-CLASS!  ( xt class -- )
    HERE _CHIP-CLASSES @ , SWAP , SWAP , _CHIP-CLASSES ! ;
: _CHIP-CLASS-XT  ( class -- xt | 0 )
    _CHIP-CLASSES @
    BEGIN DUP WHILE
        DUP 8 + @ 2 PICK = IF NIP 16 + @ EXIT THEN @
    REPEAT NIP ;

\ The control queues' requests: an OFFER goes to its class's word, and a
\ class nobody serves is refused.
: _CHIP-CONTROL-REQUEST  ( entry pair -- )
    OVER E-OP _OP-OFFER = IF
        OVER E.ARG0 E@ 0xFFFFFFFF AND _CHIP-CLASS-XT ?DUP IF EXECUTE EXIT THEN
        CHIP-REFUSED -ROT CHIP-REPLY DROP EXIT
    THEN
    CHIP-UNSUPPORTED -ROT CHIP-REPLY DROP ;

\ A pair the core it names serves along with the port's control queues.
: CHIP-SERVE-PAIR  ( pair -- ior )
    16 _CHIP-PRIVATE ?DUP IF NIP EXIT THEN
    TUCK 8 + !
    DUP 8 + @ P.PORT P@ PS.SERVED PS' DUP @ 2 PICK ! !
    0 ;
: CHIP-UNSERVE-PAIR  ( pair -- )
    DUP P.PORT P@ PS.SERVED PS'
    BEGIN DUP @ WHILE
        DUP @ 8 + @ 2 PICK = IF
            DUP @ DUP @ ROT ! 16 XMEM-FREE-BLOCK DROP EXIT
        THEN @
    REPEAT 2DROP ;

\ Serve what waits for this core on every port it serves.
: _CHIP-SERVICE  ( -- )
    CHIP-PORTS 0 ?DO
        I PS.BOOTED PS' @ I PS.DOWN PS' @ 0= AND IF
            I _CHIP-ACK
            I PS.PAIR PS' @ DUP P.NOTIFY P@ COREID = IF
                CHIP-PAIR-SERVE IF 3 I _CHIP-FAILED THEN
            ELSE DROP THEN
            \ A failure may end the port's functions and their pairs.
            I PS.DOWN PS' @ IF 0 ELSE I PS.SERVED PS' @ THEN
            BEGIN DUP WHILE
                DUP 8 + @ DUP P.NOTIFY P@ COREID = IF
                    CHIP-PAIR-SERVE IF 3 I _CHIP-FAILED DROP 0 ELSE @ THEN
                ELSE DROP @ THEN
            REPEAT DROP
        THEN
    LOOP ;

\ Serve the port until DEADLINE, for a caller that waits for something
\ the port's functions do.
: CHIP-SERVE  ( deadline port -- )
    BEGIN OVER _CHIP-PAST? 0= WHILE
        _CHIP-SERVICE OVER _CHIP-NAP
    REPEAT 2DROP ;

: CHIP-DOWN?  ( port -- flag )   PS.DOWN PS' @ ;

\ Hold the chiplet in reset.  The port's functions hear of it first, so
\ that what they lent goes back once nothing uses it.
DEFER _CHIP-RESET-HOOK   ( port -- )
: _CHIP-RESET-NONE  ( port -- )   DROP ;
' _CHIP-RESET-NONE IS _CHIP-RESET-HOOK
: _CHIP-HOLD  ( port -- )
    _CTRL-RESET-HOLD _P-CTRL ROT CHIP!  ;
: CHIP-RESET  ( port -- )
    DUP _CHIP-HOLD  TRUE OVER PS.DOWN PS' !  _CHIP-RESET-HOOK ;

\ Ask the chiplet to stop, and wait until it has or the deadline passes.
: CHIP-STOP  ( port -- ior )
    _P-CTRL OVER CHIP@ _CTRL-STOP-REQ OR _P-CTRL 2 PICK CHIP!
    CHIP-DEADLINE@
    BEGIN
        _P-STATUS 2 PICK CHIP@ _ST-STOPPED AND IF 2DROP 0 EXIT THEN
        DUP _CHIP-PAST? IF 2DROP CHIP-E-TIMEOUT EXIT THEN
        DUP _CHIP-NAP
    AGAIN ;

\ Wait for the port's signal, then acknowledge it.
: CHIP-WAIT  ( port -- )
    BEGIN
        _MBOX-STATUS C@ OVER CHIP-ID 1 SWAP LSHIFT AND 0=
    WHILE 0 IDLE-UNTIL REPEAT
    _CHIP-ACK ;

\ Bring the chiplet up as docs/chiplet-link.md section 6 orders it: the
\ control block written and lent, the image lent read-only, BOOT_ADDR,
\ BOOT_LEN and BOOT_ARG named, and the chiplet released, then READY.
\ The image's range is withdrawn once the chiplet is up.
VARIABLE _BOOT-PORT
: _BOOT-FAIL  ( ior -- ior )   _BOOT-PORT @ _CHIP-HOLD ;
: CHIP-BOOT  ( addr len port -- ior )
    DUP CHIP-PRESENT? 0= IF DROP 2DROP CHIP-E-ABSENT EXIT THEN
    DUP _BOOT-PORT !
    _CHIP-HOLD
    _BOOT-PORT @ PS.BOOTED PS' @ 0= IF
        8 8 COREID 0 _BOOT-PORT @ CHIP-PAIR-MAKE ?DUP IF NIP NIP NIP EXIT THEN
        ['] _CHIP-CONTROL-REQUEST OVER P.HANDLER P!
        _BOOT-PORT @ PS.PAIR PS' !
        64 CHIP-READ 0 _BOOT-PORT @ CHIP-LEND-ALLOT ?DUP IF NIP NIP NIP EXIT THEN
        _CHIPCTRL OVER !  1 OVER 8 + !
        _BOOT-PORT @ PS.PAIR PS' @ OVER 16 + CHIP-PAIR-RECORD!
        _BOOT-PORT @ PS.CONTROL PS' !
        TRUE _BOOT-PORT @ PS.BOOTED PS' !
    THEN
    \ The control queues start empty at each boot.
    _BOOT-PORT @ PS.PAIR PS' @ >R
    0 R@ P.TAIL P!  0 R@ P.HEAD-SEEN P!  0 R@ P.HEAD P!  0 R@ P.TAIL-SEEN P!
    R@ P.TO-RING P@ R@ P.TO-CAP P@ 1+ _ENTRY * 0 FILL
    0 R@ P.TO-HEAD P@ !  0 R@ P.FROM-RING P@ !  0 R@ P.FROM-HEAD P@ !
    R> DROP
    ( addr len )
    DUP IF
        CHIP-LOCK LOCK _BOOT-PORT @ _FREE-SLOT CHIP-LOCK UNLOCK
        DUP 0< IF DROP 2DROP CHIP-E-NOMEM _BOOT-FAIL EXIT THEN
        DUP _BOOT-PORT @ CELLS _CHIP-IMAGE-SLOT + !
        >R 2DUP OVER + CHIP-READ R> _BOOT-PORT @ CHIP-LEND ?DUP IF NIP NIP _BOOT-FAIL EXIT THEN
    THEN
    _P-BOOT-LEN _BOOT-PORT @ CHIP!  _P-BOOT-ADDR _BOOT-PORT @ CHIP!
    _BOOT-PORT @ PS.CONTROL PS' @ _P-BOOT-ARG _BOOT-PORT @ CHIP!
    FALSE _BOOT-PORT @ PS.DOWN PS' !
    _CTRL-ENABLE _CTRL-DOORBELL-EN OR _CTRL-BORROW-EN OR _P-CTRL _BOOT-PORT @ CHIP!
    CHIP-DEADLINE@
    BEGIN
        _P-STATUS _BOOT-PORT @ CHIP@ _ST-READY AND 0=
    WHILE
        DUP _CHIP-PAST? IF DROP CHIP-E-TIMEOUT _BOOT-FAIL EXIT THEN
        DUP _CHIP-NAP
    REPEAT DROP
    _BOOT-PORT @ CELLS _CHIP-IMAGE-SLOT + @ DUP 0< 0= IF
        _BOOT-PORT @ CHIP-UNLEND  -1 _BOOT-PORT @ CELLS _CHIP-IMAGE-SLOT + !
    ELSE DROP THEN
    0 ;

\ =====================================================================
\  Requests
\ =====================================================================

\ Wait until PAIR holds the reply MegaPad waits for, serving the port
\ meanwhile: true when it came before DEADLINE.
: _CHIP-AWAIT  ( deadline pair -- flag ior )
    BEGIN
        DUP P.PORT P@ CHIP-DOWN? IF 2DROP FALSE CHIP-E-ABSENT EXIT THEN
        _CHIP-SERVICE
        DUP P.PORT P@ _CHIP-ACK
        DUP CHIP-PAIR-SERVE ?DUP IF NIP NIP FALSE SWAP EXIT THEN
        DUP P.READY P@ IF 2DROP TRUE 0 EXIT THEN
        OVER _CHIP-PAST? IF 2DROP FALSE 0 EXIT THEN
        OVER _CHIP-NAP
    AGAIN ;

\ Send ENTRY as a request on PAIR and wait for its reply, which lands in
\ the pair's reply entry.  A request that misses its deadline is
\ cancelled, and when the cancel goes unanswered too the port's service
\ ends: its functions hear of it and the port is reset.
: CHIP-REQUEST  ( entry pair -- ior )
    DUP P.TAG P@ 1+ OVER P.TAG P!  FALSE OVER P.READY P!
    DUP P.TAG P@ 2 PICK E.TAG E!
    TUCK CHIP-PUT ?DUP IF NIP EXIT THEN
    CHIP-DEADLINE@ OVER _CHIP-AWAIT ?DUP IF NIP NIP EXIT THEN
    IF DROP 0 EXIT THEN
    \ The cancel's one answer is the request's reply.
    _SEND-BUF _ENTRY-CLEAR  _OP-CANCEL _SEND-BUF !
    DUP P.TAG P@ _SEND-BUF E.TAG E!
    _SEND-BUF OVER CHIP-PUT ?DUP IF NIP EXIT THEN
    CHIP-DEADLINE@ OVER _CHIP-AWAIT ?DUP IF NIP NIP EXIT THEN
    IF DROP 0 EXIT THEN
    5 SWAP P.PORT P@ _CHIP-FAILED CHIP-E-TIMEOUT ;

\ =====================================================================
\  Managed data (docs/managed-data.md): constants and state
\ =====================================================================

1 CONSTANT OBJ-CLASS
14 CONSTANT OBJ-LOCK
1 CONSTANT _OBJ-OWNER            \ the function's owner in the lend pool

-1101 CONSTANT OBJ-E-ABSENT      \ no engine serves managed data
-1102 CONSTANT OBJ-E-FAILED      \ the shared heap is lost
-1103 CONSTANT OBJ-E-STALE       \ the handle names no live slot
-1104 CONSTANT OBJ-E-OWNER       \ the owner token is not two positive cells
-1105 CONSTANT OBJ-E-WINDOW      \ in a window where none may be, or outside one
-1106 CONSTANT OBJ-E-KIND        \ not a reference, a fixnum, or of the shape
-1107 CONSTANT OBJ-E-RANGE       \ a field or byte outside its object, or no fixnum
-1108 CONSTANT OBJ-E-CORRUPT     \ a word that breaks LM1's object model
-1109 CONSTANT OBJ-E-NOMEM       \ no memory for a region, a segment or a log

0x100 CONSTANT _OP-ATTACH   0x101 CONSTANT _OP-REGION
0x102 CONSTANT _OP-SHAPE    0x103 CONSTANT _OP-COLLECT
0x180 CONSTANT _OP-GROW     0x181 CONSTANT _OP-RETURN
0x182 CONSTANT _OP-NOTICE   0x183 CONSTANT _OP-KIND
0x100 CONSTANT OBJ-NO-MEMORY   0x101 CONSTANT OBJ-BAD-RECORD
0x102 CONSTANT OBJ-NO-RESUME

\ The records of section 9, by offset.
0x00444547414E414D CONSTANT _MANAGED      \ ASCII MANAGED and a zero
0x00 CONSTANT MC.MAGIC      0x08 CONSTANT MC.VERSION    0x10 CONSTANT MC.CORES
0x18 CONSTANT MC.CORE-TABLE 0x20 CONSTANT MC.LM1-BLOCK  0x28 CONSTANT MC.HANDLE-DIR
0x30 CONSTANT MC.STATE      0x38 CONSTANT MC.CAUSE      0x40 CONSTANT MC.BLOCKS
0x48 CONSTANT MC.SHAPES
0x00 CONSTANT CR.WINDOW     0x08 CONSTANT CR.REGION     0x10 CONSTANT CR.FILL
0x18 CONSTANT CR.LOG-RING   0x20 CONSTANT CR.LOG-CAP    0x28 CONSTANT CR.TAIL
0x40 CONSTANT CR.PAIR       128 CONSTANT _CR
0x00 CONSTANT LB.HOLD       0x08 CONSTANT LB.WINDOW-LIMIT 0x10 CONSTANT LB.HOLD-LIMIT
0x18 CONSTANT LB.HOLD-GAP   0x20 CONSTANT LB.SHAPE-CONS 0x28 CONSTANT LB.SHAPE-VECTOR
0x30 CONSTANT LB.SHAPE-BYTES 0x38 CONSTANT LB.SHAPE-STRING 0x40 CONSTANT LB.MOVE
0x48 CONSTANT LB.MOVE-SET   0x50 CONSTANT LB.SHAPE-RANGE 0x80 CONSTANT LB.CORES
0x00 CONSTANT CE.LOG-HEAD   0x10 CONSTANT CE.NEXT-BASE  0x18 CONSTANT CE.NEXT-LIMIT
0x20 CONSTANT CE.NEXT-SERIAL 64 CONSTANT _CE

0 CONSTANT W-CLOSED   1 CONSTANT W-OPEN   2 CONSTANT W-WAITING
1 CONSTANT H-HELD     2 CONSTANT H-ACTIVE 4 CONSTANT H-FAILED

0x05 CONSTANT _NIL    0x0D CONSTANT _T
0xFFF8000000000000 CONSTANT _METADATA     \ bits 63:51 of a reference
0x0007FFFFFFFFFFF8 CONSTANT _ADDR-BITS

\ Settings, read when the function starts: the handle table's first
\ segment, a power of two, and the entries of each core's first log.
VARIABLE OBJ-HANDLE-FIRST   64 OBJ-HANDLE-FIRST !
VARIABLE OBJ-LOG-ENTRIES    256 OBJ-LOG-ENTRIES !

\ The function: 0 absent, 1 attaching, 2 served by LM1, 3 lost with its
\ memory held, 4 lost with its memory returned to KDOS.
VARIABLE _OBJ-STATE   0 _OBJ-STATE !
VARIABLE _OBJ-PORT    VARIABLE _OBJ-GRANT   VARIABLE _OBJ-GRANT-REC
VARIABLE _OBJ-CB      VARIABLE _OBJ-CT      VARIABLE _OBJ-LB
VARIABLE _OBJ-HD      VARIABLE _OBJ-BL      VARIABLE _OBJ-SR
VARIABLE _OBJ-CAUSE   0 _OBJ-CAUSE !
VARIABLE _OBJ-RESET   \ LM1 has been reset since the loss
VARIABLE _OBJ-ATTACH-STATUS
CREATE _OBJ-ATTACH-BUF  _ENTRY ALLOT
\ The built-in shapes LM1 stated at ATTACH.
VARIABLE _SH-CONS  VARIABLE _SH-VECTOR  VARIABLE _SH-BYTES  VARIABLE _SH-STRING
\ The NOTICEs LM1 sent: how many, and the last one's kind, ARG1 and address.
VARIABLE OBJ-NOTICES  VARIABLE OBJ-NOTICE-KIND  VARIABLE OBJ-NOTICE-ARG
VARIABLE OBJ-NOTICE-ADDR

\ Each core's own state, in MegaPad's memory.
0 CONSTANT OC.DEPTH   1 CONSTANT OC.SEEN    2 CONSTANT OC.BASE
3 CONSTANT OC.LIMIT   4 CONSTANT OC.BUMP    5 CONSTANT OC.MADE
6 CONSTANT OC.LOGGED  7 CONSTANT OC.RING    8 CONSTANT OC.CAP
9 CONSTANT OC.TAIL    10 CONSTANT OC.PAIR   11 CONSTANT OC.SERIAL
12 CONSTANT OC.OD-EPOCH 13 CONSTANT OC.OD-SINCE 14 CONSTANT OC.OD-KIND
15 CONSTANT OC.ARG0   16 CONSTANT OC.ARG1   17 CONSTANT OC.STATUS
18 CONSTANT OC.FULL   19 CONSTANT OC.TMP
24 CELLS CONSTANT _OC
CREATE _OBJ-CORE  NCORES _OC * ALLOT
: _OC'  ( field -- addr )   CELLS COREID _OC * + _OBJ-CORE + ;
: OC@   ( field -- x )   _OC' @ ;
: OC!   ( x field -- )   _OC' ! ;
: _OBJ-CORES-CLEAR  ( -- )
    _OBJ-CORE NCORES _OC * 0 FILL
    NCORES 0 ?DO -1 I _OC * _OBJ-CORE + OC.SEEN CELLS + ! LOOP ;
_OBJ-CORES-CLEAR

\ This core's record and its entry in LM1's block.
: _REC      ( -- addr )   COREID _CR * _OBJ-CT @ + ;
: _REC-OF   ( core -- addr )   _CR * _OBJ-CT @ + ;
: _ENT      ( -- addr )   COREID _CE * LB.CORES + _OBJ-LB @ + ;
: _HOLD@    ( -- hold )   _OBJ-LB @ @ ;
: _WINDOW!  ( state -- )   _REC CR.WINDOW + ! ;

\ =====================================================================
\  Engines and failure (sections 11 and 13)
\ =====================================================================

: OBJ-READY?  ( -- flag )   _OBJ-STATE @ 2 = ;
\ Which engine serves: 0 none, 1 LM1; and the cause of the failure that
\ lost the heap, else 0.  MegaPad's own engine is recovery's stage 1.
: OBJ-ENGINE  ( -- engine cause )
    _OBJ-STATE @ 2 = IF 1 ELSE 0 THEN _OBJ-CAUSE @ ;

\ 0 while an engine serves; otherwise the status every word returns.
: _OBJ-SERVES  ( -- ior )
    _OBJ-STATE @ 2 = IF 0 EXIT THEN
    _OBJ-STATE @ 3 < IF OBJ-E-ABSENT ELSE OBJ-E-FAILED THEN ;

\ Whether a window is open on any core, by the window words.
: _OBJ-ANY-OPEN?  ( -- flag )
    NCORES 0 ?DO I _REC-OF CR.WINDOW + @ W-OPEN = IF TRUE UNLOOP EXIT THEN LOOP
    FALSE ;

\ The handle table's private side: for each slot its generation, owner,
\ place in the owner's list and serial, in segments shaped as the table's.
0 CONSTANT HS.GEN   1 CONSTANT HS.OWNER   2 CONSTANT HS.NEXT
3 CONSTANT HS.PREV  4 CONSTANT HS.SERIAL
5 CELLS CONSTANT _HS
CREATE _OBJ-SIDE  64 CELLS ALLOT          \ the side segments' bases
VARIABLE _OBJ-FREE-SLOT    \ the free list's first slot, index + 1, or 0
VARIABLE _OBJ-SLOTS        \ slots in the table
\ Owners: id, generation, newest handle (index + 1), next serial.
4 CELLS CONSTANT _OW
VARIABLE _OBJ-OWNERS   VARIABLE _OBJ-OWNER-N   VARIABLE _OBJ-OWNER-CAP

: _HD-FIRST  ( -- n )   _OBJ-HD @ @ ;
: _HD-SEGS   ( -- n )   _OBJ-HD @ 8 + @ ;
\ The segment holding slot INDEX and the slot's place in it.
: _SEG  ( index -- k pos )
    0 BEGIN
        2DUP 1+ 1 SWAP LSHIFT 1- _HD-FIRST * U< 0=
    WHILE 1+ REPEAT
    TUCK 1 SWAP LSHIFT 1- _HD-FIRST * - ;
: _SLOT  ( index -- addr )   _SEG CELLS SWAP CELLS _OBJ-HD @ 16 + + @ + ;
: _SIDE  ( index -- addr )   _SEG _HS * SWAP CELLS _OBJ-SIDE + @ + ;
: HS@  ( index field -- x )   CELLS SWAP _SIDE + @ ;
: HS!  ( x index field -- )   CELLS SWAP _SIDE + ! ;

\ Return every bit of the function's memory to KDOS once no core has a
\ window open and LM1 has been reset, since until then LM1 may still hold
\ references into it (section 11, fix).
: _OBJ-FREE-PRIVATE  ( -- )
    _OBJ-HD @ IF
        _HD-SEGS 0 ?DO
            I CELLS _OBJ-SIDE + @ ?DUP IF
                _HD-FIRST I LSHIFT _HS * XMEM-FREE-BLOCK
            THEN
        LOOP
    THEN
    _OBJ-SIDE 64 CELLS 0 FILL
    _OBJ-OWNERS @ ?DUP IF _OBJ-OWNER-CAP @ _OW * XMEM-FREE-BLOCK THEN
    0 _OBJ-OWNERS !  0 _OBJ-OWNER-N !  0 _OBJ-OWNER-CAP !
    _OBJ-GRANT @ ?DUP IF DUP CHIP-UNSERVE-PAIR _PAIR-SIZE XMEM-FREE-BLOCK THEN
    NCORES 0 ?DO
        I _OC * _OBJ-CORE + OC.PAIR CELLS + @ ?DUP IF _PAIR-SIZE XMEM-FREE-BLOCK THEN
    LOOP ;
: _OBJ-RETURN?  ( -- )
    _OBJ-STATE @ 3 = _OBJ-RESET @ AND 0= IF EXIT THEN
    _OBJ-ANY-OPEN? IF EXIT THEN
    _OBJ-FREE-PRIVATE
    _OBJ-OWNER _OBJ-PORT @ CHIP-UNLEND-OWNER
    0 _OBJ-GRANT !  0 _OBJ-CB !  0 _OBJ-CT !  0 _OBJ-LB !  0 _OBJ-HD !
    0 _OBJ-BL !  0 _OBJ-SR !  0 _OBJ-FREE-SLOT !  0 _OBJ-SLOTS !
    _OBJ-CORES-CLEAR
    4 _OBJ-STATE ! ;

\ The heap is lost (section 11): STATE reads LOST with the cause, every
\ core that waits is rung, and LM1 is held in reset unless it set FAILED,
\ in which case its private work goes on and the memory waits for its
\ reset.  Until recovery's stage 1, every failure loses the heap.
: _OBJ-LOSE  ( cause -- )
    _OBJ-STATE @ 1 3 WITHIN 0= IF DROP EXIT THEN
    DUP _OBJ-CAUSE !
    DUP 1 <> IF
        _OBJ-PORT @ DUP _CHIP-HOLD TRUE SWAP PS.DOWN PS' !  TRUE _OBJ-RESET !
    THEN DROP
    3 _OBJ-STATE !
    2 _OBJ-CB @ MC.STATE + !  _OBJ-CAUSE @ _OBJ-CB @ MC.CAUSE + !
    NCORES 0 ?DO
        I COREID <> IF I _REC-OF CR.WINDOW + @ W-WAITING = IF I _CHIP-RING-CORE THEN THEN
    LOOP
    _OBJ-RETURN? ;

\ The port's service ended: a broken queue (3) or a missed deadline (5).
: _OBJ-PORT-FAILED  ( cause port -- )
    _OBJ-PORT @ = _OBJ-STATE @ 1 3 WITHIN AND IF _OBJ-LOSE ELSE DROP THEN ;
' _OBJ-PORT-FAILED IS _CHIP-FAILED

\ The chiplet was reset for any other reason (6).
: _OBJ-PORT-RESET  ( port -- )
    _OBJ-PORT @ = _OBJ-STATE @ 1 4 WITHIN AND IF
        TRUE _OBJ-RESET !
        _OBJ-STATE @ 3 < IF 6 _OBJ-LOSE ELSE _OBJ-RETURN? THEN
    THEN ;
' _OBJ-PORT-RESET IS _CHIP-RESET-HOOK

\ A check of LM1's words failed: the heap is lost (cause 2).
: _OBJ-CORRUPT  ( -- ior )   2 _OBJ-LOSE OBJ-E-FAILED ;

\ =====================================================================
\  References (section 6)
\ =====================================================================

\ Whether ADDR lies in a block of the shared heap, as the block list
\ names the blocks MegaPad granted.
: _OBJ-IN-HEAP?  ( addr -- flag )
    _OBJ-BL @ DUP @ 0 ?DO
        2DUP I 16 * 16 + + @ -
        OVER I 16 * 24 + + @ U< IF 2DROP TRUE UNLOOP EXIT THEN
    LOOP 2DROP FALSE ;

\ Whether a tagged word has one of the two reference tags, 001 for an
\ object and 011 for a cons cell, in bits 2:0.
: _REF?  ( t -- flag )   7 AND DUP 1 = SWAP 3 = OR ;

\ A tagged word as a reference: tag 001 or 011, zero in bits 63:51, and
\ an address in the shared heap.  A word with a reference tag that fails
\ is corrupt, and is never followed.
: OBJ>REF  ( t -- r ior )
    _OBJ-SERVES ?DUP IF NIP 0 SWAP EXIT THEN
    DUP _REF? 0= IF DROP 0 OBJ-E-KIND EXIT THEN
    DUP _METADATA AND IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    DUP _ADDR-BITS AND _OBJ-IN-HEAP? 0= IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    0 ;

\ A value a store may write into a tagged field: any tagged word but a
\ header, a reference only into the shared heap.
: _OBJ-VALUE?  ( t -- ior )
    DUP 7 AND 7 = IF DROP OBJ-E-KIND EXIT THEN
    DUP _REF? IF OBJ>REF NIP EXIT THEN DROP 0 ;

: _OBJ-ADDR  ( r -- addr )   _ADDR-BITS AND ;

\ A sound header of one of the four kinds MegaPad reads and makes, not a
\ forwarding pointer.  Its GC bits are ignored.
1 CONSTANT K-CONS   2 CONSTANT K-VECTOR   3 CONSTANT K-BYTES   16 CONSTANT K-RECORD
: H-SUB    ( h -- sub )   3 RSHIFT 0x1F AND ;
: H-SIZE   ( h -- n )   8 RSHIFT 0xFFFF AND ;
: H-SHAPE  ( h -- id )   24 RSHIFT 0xFFFFFFFF AND ;
: _OBJ-HEADER  ( r -- h ior )
    DUP _OBJ-ADDR _OBJ-IN-HEAP? 0= IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    _OBJ-ADDR @
    DUP 7 AND 7 <> IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    DUP 56 RSHIFT 0xFF = IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    DUP H-SUB DUP K-CONS = OVER K-VECTOR = OR OVER K-BYTES = OR SWAP K-RECORD = OR
    0= IF DROP 0 OBJ-E-CORRUPT EXIT THEN
    0 ;

\ The address of field I.
: _FIELD  ( r i -- addr )   1+ CELLS SWAP _OBJ-ADDR + ;

\ =====================================================================
\  Values (section 14)
\ =====================================================================

: OBJ-NIL  ( -- t )   _NIL ;
: OBJ-T    ( -- t )   _T ;
: INT>OBJ  ( n -- t ior )
    DUP 2* DUP 2/ ROT = IF 0 ELSE DROP 0 OBJ-E-RANGE THEN ;
: OBJ>INT  ( t -- n ior )
    DUP 1 AND IF DROP 0 OBJ-E-KIND EXIT THEN 2/ 0 ;

\ Whether a window word must be open, or closed, for a word to run.
: _OBJ-IN   ( -- ior )
    _OBJ-SERVES ?DUP IF EXIT THEN OC.DEPTH OC@ 0= IF OBJ-E-WINDOW EXIT THEN 0 ;
: _OBJ-OUT  ( -- ior )
    _OBJ-SERVES ?DUP IF EXIT THEN OC.DEPTH OC@ IF OBJ-E-WINDOW EXIT THEN 0 ;

: _OBJ-ALLOT-REC  ( u rights -- addr ior )   _OBJ-OWNER _OBJ-PORT @ CHIP-LEND-ALLOT ;
: _NEXT-POW2  ( n -- p )   1 BEGIN 2DUP U> WHILE 2* REPEAT NIP ;

\ =====================================================================
\  Requests on a core's own pair (section 10)
\ =====================================================================

\ This core's pair, made before its first request and named in its
\ record, where LM1 reads it at the core's first ring.  A core keeps one
\ request unanswered at a time, so the pair holds two entries each way.
: _OBJ-PAIR  ( -- pair ior )
    OC.PAIR OC@ ?DUP IF 0 EXIT THEN
    2 2 COREID _OBJ-OWNER _OBJ-PORT @ CHIP-PAIR-MAKE ?DUP IF EXIT THEN
    DUP _REC CR.PAIR + CHIP-PAIR-RECORD!
    DUP OC.PAIR OC! 0 ;

\ A request on this core's pair: the reply's status, with its arguments
\ kept in OC.ARG0 and OC.ARG1.  A port that fails ends the function.
: _OBJ-ASK  ( op in-addr in-len arg0 -- status ior )
    _ASK-BUF _ENTRY-CLEAR
    _ASK-BUF E.ARG0 E!  _ASK-BUF E.IN-LEN E!  _ASK-BUF E.IN-ADDR E!  _ASK-BUF !
    _OBJ-PAIR ?DUP IF NIP 0 SWAP EXIT THEN
    _ASK-BUF OVER CHIP-REQUEST IF DROP 5 _OBJ-LOSE 0 OBJ-E-FAILED EXIT THEN
    P.REPLY P'
    DUP E.ARG0 E@ OC.ARG0 OC!  DUP E.ARG1 E@ OC.ARG1 OC!
    E-STATUS DUP OC.STATUS OC! 0 ;

\ =====================================================================
\  The records, the grant and ATTACH (sections 9 and 10)
\ =====================================================================

\ The block list and the shape registry grow by being written anew,
\ larger, and then named in the control block.  The caller holds OBJ-LOCK.
VARIABLE _LA-LIST   VARIABLE _LA-OFF
: _OBJ-LIST-ADD  ( a b list-var control-offset -- ior )
    _LA-OFF ! _LA-LIST !
    _LA-LIST @ @ DUP @ SWAP 8 + @ = IF
        _LA-LIST @ @ 8 + @ 2* DUP 16 * 16 + CHIP-READ _OBJ-ALLOT-REC IF
            2DROP 2DROP OBJ-E-NOMEM EXIT
        THEN
        _LA-LIST @ @ OVER _LA-LIST @ @ @ 16 * 16 + MOVE
        TUCK 8 + !  DUP _LA-LIST @ !  _OBJ-CB @ _LA-OFF @ + !
    THEN
    _LA-LIST @ @ DUP @ 16 * 16 + + TUCK 8 + ! !
    _LA-LIST @ @ @ 1+ _LA-LIST @ @ !  0 ;

\ The entry of list LIST whose first word is A, or 0.
: _OBJ-LIST-FIND  ( a list -- entry | 0 )
    DUP @ 0 ?DO
        DUP I 16 * 16 + + @ 2 PICK = IF I 16 * 16 + + NIP UNLOOP EXIT THEN
    LOOP 2DROP 0 ;

\ The registry's copy of the description of shape ID, or 0.
: _OBJ-KIND-DESC  ( id -- desc | 0 )
    _OBJ-SR @ _OBJ-LIST-FIND DUP IF 8 + @ THEN ;

\ ---- LM1's requests on the grant's pair: GROW, RETURN and NOTICE ----

VARIABLE _GR-LEN
\ A block of at least the bytes asked, lent for reading and writing, added
\ to the block list before the reply names it.
: _OBJ-GROW  ( entry pair -- )
    _OBJ-STATE @ 2 <> IF OBJ-NO-MEMORY -ROT CHIP-REPLY DROP EXIT THEN
    OVER E.ARG0 E@ 4096 MAX _ROUND64 _GR-LEN !
    _GR-LEN @ _OBJ-OWNER _OBJ-PORT @ CHIP-LEND-BLOCK IF
        DROP OBJ-NO-MEMORY -ROT CHIP-REPLY DROP EXIT
    THEN
    OBJ-LOCK LOCK DUP _GR-LEN @ _OBJ-BL MC.BLOCKS _OBJ-LIST-ADD OBJ-LOCK UNLOCK IF
        _OBJ-PORT @ CHIP-UNLEND-BLOCK OBJ-NO-MEMORY -ROT CHIP-REPLY DROP EXIT
    THEN
    >R CHIP-OK ROT CHIP-REPLY-BEGIN R> OVER E.ARG0 E!  _GR-LEN @ OVER E.ARG1 E!
    SWAP CHIP-PUT DROP ;

\ A block LM1 no longer uses: taken back, then struck from the list.
: _OBJ-RETURN-BLOCK  ( entry pair -- )
    OVER E.IN-ADDR E@ _OBJ-BL @ _OBJ-LIST-FIND ?DUP IF
        DUP @ _OBJ-PORT @ CHIP-UNLEND-BLOCK
        OBJ-LOCK LOCK
        _OBJ-BL @ DUP @ 1- 16 * 16 + + SWAP 16 MOVE
        _OBJ-BL @ @ 1- _OBJ-BL @ !
        OBJ-LOCK UNLOCK
        CHIP-OK
    ELSE CHIP-REFUSED THEN
    -ROT CHIP-REPLY DROP ;

\ A NOTICE: an overdue window (1), corrupt data (2) or a broken log (3).
: _OBJ-NOTICE  ( entry pair -- )
    OVER E.ARG0 E@ OBJ-NOTICE-KIND !  OVER E.ARG1 E@ OBJ-NOTICE-ARG !
    OVER E.IN-ADDR E@ OBJ-NOTICE-ADDR !  1 OBJ-NOTICES +!
    CHIP-OK -ROT CHIP-REPLY DROP ;

: _OBJ-LM1-REQUEST  ( entry pair -- )
    OVER E-OP _OP-GROW = IF _OBJ-GROW EXIT THEN
    OVER E-OP _OP-RETURN = IF _OBJ-RETURN-BLOCK EXIT THEN
    OVER E-OP _OP-NOTICE = IF _OBJ-NOTICE EXIT THEN
    CHIP-UNSUPPORTED -ROT CHIP-REPLY DROP ;

\ ATTACH's reply.  On OK LM1's block holds the hold word clear, its bounds
\ and the four built-in shapes, nonzero and distinct; anything else ends
\ the function (cause 2).
: _OBJ-ATTACHED  ( pair -- )
    _OBJ-STATE @ 1 <> IF DROP EXIT THEN
    P.REPLY P' E-STATUS DUP _OBJ-ATTACH-STATUS ! IF 2 _OBJ-LOSE EXIT THEN
    _OBJ-LB @ >R
    R@ LB.SHAPE-CONS + @ _SH-CONS !     R@ LB.SHAPE-VECTOR + @ _SH-VECTOR !
    R@ LB.SHAPE-BYTES + @ _SH-BYTES !   R@ LB.SHAPE-STRING + @ _SH-STRING !
    R@ LB.HOLD + @ 0<>
    R@ LB.WINDOW-LIMIT + @ 0= OR  R@ LB.HOLD-LIMIT + @ 0= OR  R> LB.HOLD-GAP + @ 0= OR
    _SH-CONS @ 0= OR _SH-VECTOR @ 0= OR _SH-BYTES @ 0= OR _SH-STRING @ 0= OR
    _SH-CONS @ _SH-VECTOR @ = OR _SH-CONS @ _SH-BYTES @ = OR _SH-CONS @ _SH-STRING @ = OR
    _SH-VECTOR @ _SH-BYTES @ = OR _SH-VECTOR @ _SH-STRING @ = OR _SH-BYTES @ _SH-STRING @ = OR
    IF 2 _OBJ-LOSE EXIT THEN
    2 _OBJ-STATE ! ;

VARIABLE _OF-ENTRY   VARIABLE _OF-PAIR   VARIABLE _OF-MEM   VARIABLE _OF-LEN

\ The grant's pair, the shared heap's first block, and the records of
\ section 9, all in the function's own lendable memory.
: _OBJ-BUILD  ( -- ior )
    _OF-ENTRY @ E.ARG1 E@ 0xFFFFFFFF AND 2 MAX _NEXT-POW2 DUP
    COREID _OBJ-OWNER _OBJ-PORT @ CHIP-PAIR-MAKE ?DUP IF NIP EXIT THEN
    DUP _OBJ-GRANT !
    ['] _OBJ-LM1-REQUEST OVER P.HANDLER P!  ['] _OBJ-ATTACHED SWAP P.DONE P!
    _OF-ENTRY @ E.OUT-LEN E@ 4096 MAX _ROUND64 DUP _OF-LEN !
    CHIP-READ-WRITE _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OF-MEM !
    64 CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN
    _OBJ-GRANT @ OVER CHIP-PAIR-RECORD!
    _OF-MEM @ OVER 0x30 + !  _OF-LEN @ OVER 0x38 + !  _OBJ-GRANT-REC !
    128 CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-CB !
    NCORES _CR * CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-CT !
    NCORES _CE * LB.CORES + CHIP-READ-WRITE _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-LB !
    0x210 CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-HD !
    8 16 * 16 + CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-BL !
    8 16 * 16 + CHIP-READ _OBJ-ALLOT-REC ?DUP IF NIP EXIT THEN _OBJ-SR !
    OBJ-HANDLE-FIRST @ 1 MAX _NEXT-POW2 _OBJ-HD @ !  0 _OBJ-HD @ 8 + !
    1 _OBJ-BL @ !  8 _OBJ-BL @ 8 + !  _OF-MEM @ _OBJ-BL @ 16 + !  _OF-LEN @ _OBJ-BL @ 24 + !
    0 _OBJ-SR @ !  8 _OBJ-SR @ 8 + !
    _OBJ-CB @ >R
    _MANAGED R@ MC.MAGIC + !  1 R@ MC.VERSION + !  NCORES R@ MC.CORES + !
    _OBJ-CT @ R@ MC.CORE-TABLE + !  _OBJ-LB @ R@ MC.LM1-BLOCK + !
    _OBJ-HD @ R@ MC.HANDLE-DIR + !  0 R@ MC.STATE + !  0 R@ MC.CAUSE + !
    _OBJ-BL @ R@ MC.BLOCKS + !  _OBJ-SR @ R> MC.SHAPES + !
    0 ;

\ LM1 offers managed data: MegaPad grants it a pair and the heap's first
\ block, builds its records, and sends ATTACH to start it afresh.  While
\ a function serves, or a lost heap's memory is held, the offer is
\ refused.
: _OBJ-OFFER  ( entry pair -- )
    _OBJ-STATE @ DUP 0= SWAP 4 = OR 0= IF CHIP-REFUSED -ROT CHIP-REPLY DROP EXIT THEN
    _OF-PAIR ! _OF-ENTRY !
    _OF-PAIR @ P.PORT P@ _OBJ-PORT !
    _OBJ-CORES-CLEAR  0 _OBJ-CAUSE !  FALSE _OBJ-RESET !  0 OBJ-NOTICES !
    0 _OBJ-FREE-SLOT !  0 _OBJ-SLOTS !  0 _OBJ-GRANT !
    _OBJ-BUILD IF
        CHIP-REFUSED _OF-ENTRY @ _OF-PAIR @ CHIP-REPLY DROP
        _OBJ-GRANT @ ?DUP IF _PAIR-SIZE XMEM-FREE-BLOCK THEN  0 _OBJ-GRANT !
        _OBJ-OWNER _OBJ-PORT @ CHIP-UNLEND-OWNER  0 _OBJ-STATE ! EXIT
    THEN
    CHIP-OK _OF-ENTRY @ CHIP-REPLY-BEGIN
    _OBJ-GRANT-REC @ OVER E.IN-ADDR E!  64 OVER E.IN-LEN E!
    _OF-PAIR @ CHIP-PUT DROP
    _OBJ-GRANT @ CHIP-SERVE-PAIR DROP
    1 _OBJ-STATE !
    _OBJ-ATTACH-BUF _ENTRY-CLEAR  _OP-ATTACH _OBJ-ATTACH-BUF !
    _OBJ-CB @ _OBJ-ATTACH-BUF E.IN-ADDR E!  128 _OBJ-ATTACH-BUF E.IN-LEN E!
    _OBJ-GRANT @ DUP P.TAG P@ 1+ OVER P.TAG P!  FALSE OVER P.READY P!
    P.TAG P@ _OBJ-ATTACH-BUF E.TAG E!
    _OBJ-ATTACH-BUF _OBJ-GRANT @ CHIP-PUT DROP ;
' _OBJ-OFFER OBJ-CLASS CHIP-CLASS!

\ Serve PORT until managed data is up, for a caller that has booted it.
: OBJ-START  ( port -- ior )
    DROP CHIP-DEADLINE@
    BEGIN
        _OBJ-STATE @ 2 = IF DROP 0 EXIT THEN
        _OBJ-STATE @ 2 > IF DROP OBJ-E-FAILED EXIT THEN
        DUP _CHIP-PAST? IF DROP OBJ-E-ABSENT EXIT THEN
        _CHIP-SERVICE DUP _CHIP-NAP
    AGAIN ;

\ =====================================================================
\  Windows and holds (section 3)
\ =====================================================================

\ Whether a core other than this one has its window open.
: _OBJ-OTHER-OPEN?  ( -- flag )
    NCORES 0 ?DO
        I COREID <> IF I _REC-OF CR.WINDOW + @ W-OPEN = IF TRUE UNLOOP EXIT THEN THEN
    LOOP FALSE ;

\ A held hold that has lasted, at one epoch, more than twice HOLD_LIMIT:
\ ACTIVE so long, or HELD without ACTIVE so long with no window open,
\ which makes the delay LM1's (section 3, the fix of an unended hold).
: _OBJ-OVERDUE?  ( hold -- flag )
    DUP H-ACTIVE AND IF 1 ELSE _OBJ-OTHER-OPEN? IF 0 ELSE 2 THEN THEN
    DUP 0= IF 2DROP 0 OC.OD-KIND OC! FALSE EXIT THEN
    SWAP 3 RSHIFT
    2DUP OC.OD-EPOCH OC@ = SWAP OC.OD-KIND OC@ = AND 0= IF
        OC.OD-EPOCH OC! OC.OD-KIND OC! MS@ OC.OD-SINCE OC! FALSE EXIT
    THEN 2DROP
    MS@ OC.OD-SINCE OC@ - 1000000 *  _OBJ-LB @ LB.HOLD-LIMIT + @ 2* > ;

\ Whether [BASE, LIMIT) can be this core's region: inside the shared heap
\ and overlapping no other core's.  A core that has not opened a window
\ since the last hold still names the region that hold retired, which LM1
\ may already have granted again, so only regions of this epoch count.
: _OBJ-REGION-OK?  ( base limit -- flag )
    2DUP U< 0= IF 2DROP FALSE EXIT THEN
    OVER 7 AND IF 2DROP FALSE EXIT THEN
    OVER _OBJ-IN-HEAP? OVER 1- _OBJ-IN-HEAP? AND 0= IF 2DROP FALSE EXIT THEN
    NCORES 0 ?DO
        I COREID <> I _OC * _OBJ-CORE + OC.SEEN CELLS + @ OC.SEEN OC@ = AND IF
            I _OC * _OBJ-CORE + DUP OC.BASE CELLS + @ SWAP OC.LIMIT CELLS + @
            OVER IF
                3 PICK U> SWAP 2 PICK U< AND IF 2DROP FALSE UNLOOP EXIT THEN
            ELSE 2DROP THEN
        THEN
    LOOP 2DROP TRUE ;

\ Take the region LM1 left this core in its NEXT fields, if it raised
\ NEXT_SERIAL since the last one the core took; the serial is read
\ before the base and limit.  With none, the core holds no region.
: _OBJ-TAKE-NEXT  ( -- took? ior )
    _ENT CE.NEXT-SERIAL + @ DUP OC.SERIAL OC@ = IF
        DROP 0 OC.BASE OC! 0 OC.LIMIT OC! 0 OC.BUMP OC! 0 _REC CR.REGION + !
        FALSE 0 EXIT
    THEN
    OC.SERIAL OC!
    _ENT CE.NEXT-BASE + @ _ENT CE.NEXT-LIMIT + @
    2DUP _OBJ-REGION-OK? 0= IF 2DROP FALSE _OBJ-CORRUPT EXIT THEN
    \ FILL at the base: the core has made nothing in its new region yet.
    OC.LIMIT OC!  DUP OC.BASE OC! DUP OC.BUMP OC!
    DUP _REC CR.FILL + !  _REC CR.REGION + !
    TRUE 0 ;

\ ---- The store log (section 7) ----

: _OBJ-LOG-HEAD@  ( -- n )   _ENT CE.LOG-HEAD + @ ;

\ The core's first ring, lent for reading and named in its record before
\ any TAIL counts entries in it.
: _OBJ-LOG-MAKE  ( -- ior )
    OBJ-LOG-ENTRIES @ 2 MAX _NEXT-POW2 DUP CELLS CHIP-READ _OBJ-ALLOT-REC
    ?DUP IF NIP NIP OBJ-E-NOMEM SWAP DROP EXIT THEN
    DUP OC.RING OC! _REC CR.LOG-RING + !
    DUP OC.CAP OC! _REC CR.LOG-CAP + ! 0 ;

\ Outside a window, once LM1 has drained a ring this core found full, the
\ core moves to one twice as large: the new ring's address and capacity
\ first, and only then a TAIL that counts entries in it.
: _OBJ-LOG-GROW  ( -- )
    OC.FULL OC@ 0= IF EXIT THEN
    _OBJ-LOG-HEAD@ OC.TAIL OC@ <> IF EXIT THEN
    OC.CAP OC@ 2* DUP CELLS CHIP-READ _OBJ-ALLOT-REC IF 2DROP EXIT THEN
    DUP OC.RING OC! _REC CR.LOG-RING + !
    DUP OC.CAP OC! _REC CR.LOG-CAP + !
    FALSE OC.FULL OC! ;

\ A full ring: TAIL stored, the port rung, and LM1 given the deadline of a
\ request to move LOG_HEAD, inside a window or outside one.
: _OBJ-LOG-WAIT  ( -- ior )
    OC.TAIL OC@ _REC CR.TAIL + !  TRUE OC.FULL OC!
    _OBJ-PORT @ CHIP-RING
    _OBJ-LOG-HEAD@ CHIP-DEADLINE@
    BEGIN
        _OBJ-SERVES ?DUP IF NIP NIP EXIT THEN
        _CHIP-SERVICE
        OVER _OBJ-LOG-HEAD@ <> IF 2DROP 0 EXIT THEN
        DUP _CHIP-PAST? IF 2DROP 5 _OBJ-LOSE OBJ-E-FAILED EXIT THEN
        DUP _CHIP-NAP
    AGAIN ;

\ An entry for a reference stored at ADDR: the word first, then the entry,
\ and TAIL after both, whenever the ring is half full and at the close.
: _OBJ-LOG  ( addr -- ior )
    OC.RING OC@ 0= IF _OBJ-LOG-MAKE ?DUP IF NIP EXIT THEN THEN
    BEGIN
        _OBJ-LOG-HEAD@ _REC CR.TAIL + @ U> IF DROP _OBJ-CORRUPT EXIT THEN
        OC.TAIL OC@ _OBJ-LOG-HEAD@ - OC.CAP OC@ U< 0=
    WHILE
        _OBJ-LOG-WAIT ?DUP IF NIP EXIT THEN
    REPEAT
    OC.TAIL OC@ OC.CAP OC@ 1- AND CELLS OC.RING OC@ + !
    OC.TAIL OC@ 1+ OC.TAIL OC!  TRUE OC.LOGGED OC!
    OC.TAIL OC@ _OBJ-LOG-HEAD@ - OC.CAP OC@ 2/ U< 0= IF
        OC.TAIL OC@ _REC CR.TAIL + !  _OBJ-PORT @ CHIP-RING
    THEN 0 ;

\ ---- Opening and closing ----

\ Sleep while the hold lasts, serving the port, until a signal or the
\ next millisecond.  The service acknowledges the port's doorbell before
\ the hold word is read again, so a ring at the hold's end is never lost.
: _OBJ-SLEEP  ( -- )
    _CHIP-SERVICE
    _OBJ-STATE @ 2 = IF _HOLD@ H-HELD AND IF MS@ 1+ IDLE-UNTIL THEN THEN ;

\ An outermost open (section 3, steps 1 to 4).
: _OBJ-OPEN-OUTER  ( -- ior )
    _OBJ-LOG-GROW
    BEGIN
        _OBJ-SERVES ?DUP IF
            _OBJ-STATE @ 3 = IF W-CLOSED _WINDOW! _OBJ-RETURN? THEN EXIT
        THEN
        W-OPEN _WINDOW!
        _HOLD@
        DUP H-FAILED AND IF DROP W-CLOSED _WINDOW! 1 _OBJ-LOSE OBJ-E-FAILED EXIT THEN
        DUP H-HELD AND 0= IF
            1 OC.DEPTH OC!
            3 RSHIFT DUP OC.SEEN OC@ = IF DROP 0 EXIT THEN
            \ EPOCH never moves back.
            DUP OC.SEEN OC@ U< OC.SEEN OC@ -1 <> AND IF
                DROP 0 OC.DEPTH OC! W-CLOSED _WINDOW! _OBJ-CORRUPT EXIT
            THEN
            \ A hold has ended since the last open: take the region LM1 left.
            OC.SEEN OC!  _OBJ-TAKE-NEXT NIP ?DUP IF
                0 OC.DEPTH OC! W-CLOSED _WINDOW! _OBJ-RETURN? EXIT
            THEN
            0 EXIT
        THEN
        W-WAITING _WINDOW!  _OBJ-PORT @ CHIP-RING
        _HOLD@ DUP H-HELD AND IF
            _OBJ-OVERDUE? IF DROP W-CLOSED _WINDOW! 4 _OBJ-LOSE OBJ-E-FAILED EXIT THEN
            _OBJ-SLEEP
        ELSE DROP THEN
        DROP
    AGAIN ;

\ An outermost close (section 3): FILL and TAIL first, then CLOSED, then
\ a ring if a hold waits.
: _OBJ-CLOSE-OUTER  ( -- )
    _OBJ-STATE @ 2 = IF
        OC.MADE OC@ IF OC.BUMP OC@ _REC CR.FILL + !  FALSE OC.MADE OC! THEN
        OC.LOGGED OC@ IF OC.TAIL OC@ _REC CR.TAIL + !  FALSE OC.LOGGED OC! THEN
        W-CLOSED _WINDOW!
        _HOLD@ H-HELD AND IF _OBJ-PORT @ CHIP-RING THEN
        EXIT
    THEN
    _OBJ-STATE @ 3 = IF W-CLOSED _WINDOW! _OBJ-RETURN? THEN ;

: OBJ-OPEN  ( -- ior )
    _OBJ-SERVES ?DUP IF EXIT THEN
    OC.DEPTH OC@ IF OC.DEPTH OC@ 1+ OC.DEPTH OC! 0 EXIT THEN
    _OBJ-OPEN-OUTER ;

: OBJ-CLOSE  ( -- )
    OC.DEPTH OC@ 0= IF EXIT THEN
    OC.DEPTH OC@ 1- DUP OC.DEPTH OC! IF EXIT THEN
    _OBJ-CLOSE-OUTER ;

\ Run XT in a window, closing it on return or exception; a failed open
\ throws its status.
: OBJ-WITHIN  ( i*x xt -- j*x )
    OBJ-OPEN ?DUP IF NIP THROW THEN
    CATCH OBJ-CLOSE THROW ;

\ Let a waiting hold pass: true when one may have, and every reference
\ read before is then invalid.
: OBJ-POLL  ( -- flag )
    OC.DEPTH OC@ 0= IF FALSE EXIT THEN
    _OBJ-SERVES IF FALSE EXIT THEN
    _HOLD@ H-HELD AND 0= IF FALSE EXIT THEN
    OC.DEPTH OC@ >R
    _OBJ-CLOSE-OUTER  0 OC.DEPTH OC!
    _OBJ-OPEN-OUTER ?DUP IF R> DROP THROW THEN
    R> OC.DEPTH OC!  TRUE ;

\ KDOS's QUIT callback, then the aborting core's window closed: ABORT and
\ QUIT reset the return stack, where no close survives.
: _OBJ-QUIT  ( -- )
    _KDOS-QUIT
    OC.DEPTH OC@ IF 1 OC.DEPTH OC! OBJ-CLOSE THEN ;
' _OBJ-QUIT QUIT-XT!

\ ---- Regions (section 8) ----

\ A region holding at least BYTES: FILL stored, then a REGION request.
: _OBJ-REGION  ( bytes -- ior )
    OC.BASE OC@ IF OC.BUMP OC@ _REC CR.FILL + ! THEN
    >R _OP-REGION 0 0 R@ _OBJ-ASK ?DUP IF NIP R> DROP EXIT THEN
    DUP CHIP-OK = IF
        DROP _OBJ-TAKE-NEXT ?DUP IF NIP R> DROP EXIT THEN
        0= IF R> DROP _OBJ-CORRUPT EXIT THEN
        OC.LIMIT OC@ OC.BASE OC@ - R> U< IF _OBJ-CORRUPT EXIT THEN
        0 EXIT
    THEN
    R> DROP
    DUP OBJ-NO-MEMORY = IF DROP OBJ-E-NOMEM EXIT THEN
    DROP _HOLD@ H-FAILED AND IF 1 _OBJ-LOSE THEN OBJ-E-FAILED ;

\ BYTES at the region's next free byte, asking for a region when they do
\ not fit.
: _OBJ-ALLOT  ( bytes -- addr ior )
    OC.BASE OC@ IF
        OC.BUMP OC@ OVER + OC.LIMIT OC@ U> 0= IF
            OC.BUMP OC@ TUCK + OC.BUMP OC!  TRUE OC.MADE OC!  0 EXIT
        THEN
    THEN
    DUP _OBJ-REGION ?DUP IF NIP 0 SWAP EXIT THEN
    OC.BUMP OC@ TUCK + OC.BUMP OC!  TRUE OC.MADE OC!  0 ;

\ Whether ADDR lies in this core's current region, whose objects the hold
\ that retires it traces whole.
: _OBJ-IN-REGION?  ( addr -- flag )
    OC.BASE OC@ DUP 0= IF 2DROP FALSE EXIT THEN
    - OC.LIMIT OC@ OC.BASE OC@ - U< ;

\ =====================================================================
\  Reading, inside a window (sections 5 and 6)
\ =====================================================================

\ Field I, unchecked: rule 1's direct read.
: OBJ@  ( r i -- t )   _FIELD @ ;

: OBJ-SHAPE@  ( r -- shape ior )
    _OBJ-IN ?DUP IF NIP 0 SWAP EXIT THEN
    _OBJ-HEADER ?DUP IF NIP 0 SWAP EXIT THEN H-SHAPE 0 ;

\ A vector's elements, a bytevector's bytes, or a record's tagged fields.
: OBJ-LENGTH  ( r -- n ior )
    _OBJ-IN ?DUP IF NIP 0 SWAP EXIT THEN
    DUP _OBJ-HEADER ?DUP IF NIP NIP 0 SWAP EXIT THEN
    H-SUB K-CONS = IF DROP 0 OBJ-E-KIND EXIT THEN
    0 _FIELD @ 2/ 0 ;

: _OBJ-CONS-FIELD  ( r i -- t ior )
    _OBJ-IN ?DUP IF NIP NIP 0 SWAP EXIT THEN
    OVER _OBJ-HEADER ?DUP IF NIP NIP NIP 0 SWAP EXIT THEN
    DUP H-SUB K-CONS <> SWAP H-SIZE 2 <> OR IF 2DROP 0 OBJ-E-KIND EXIT THEN
    _FIELD @ 0 ;
: OBJ-CAR  ( r -- t ior )   0 _OBJ-CONS-FIELD ;
: OBJ-CDR  ( r -- t ior )   1 _OBJ-CONS-FIELD ;

\ The address of a record's raw word J, after its tagged fields.
: _OBJ-RAW-ADDR  ( r j -- addr ior )
    OVER _OBJ-HEADER ?DUP IF NIP NIP NIP 0 SWAP EXIT THEN
    DUP H-SUB K-RECORD <> IF DROP 2DROP 0 OBJ-E-KIND EXIT THEN
    H-SIZE 1- 2 PICK 0 _FIELD @ 2/ -
    OVER U> 0= IF 2DROP 0 OBJ-E-RANGE EXIT THEN
    OVER 0 _FIELD @ 2/ 1+ + _FIELD 0 ;

: OBJ-RAW@  ( r j -- x ior )
    _OBJ-IN ?DUP IF NIP NIP 0 SWAP EXIT THEN
    _OBJ-RAW-ADDR ?DUP IF EXIT THEN @ 0 ;

\ Copy a bytevector's bytes into the buffer, as many as fit.
: OBJ>SPAN  ( r buf len -- n ior )
    _OBJ-IN ?DUP IF >R 2DROP DROP 0 R> EXIT THEN
    2 PICK _OBJ-HEADER ?DUP IF >R DROP 2DROP DROP 0 R> EXIT THEN
    H-SUB K-BYTES <> IF 2DROP DROP 0 OBJ-E-KIND EXIT THEN
    2 PICK 0 _FIELD @ 2/ MIN
    >R SWAP 1 _FIELD SWAP R@ MOVE R> 0 ;

\ =====================================================================
\  Writing, inside a window (section 7)
\ =====================================================================

\ Whether field I of R is a tagged field a store may write: a cons's car
\ or cdr, a vector's element, a record's tagged field, never a header, a
\ record's count or a length.
: _OBJ-TAGGED-FIELD?  ( r i -- ior )
    OVER _OBJ-HEADER ?DUP IF NIP NIP NIP EXIT THEN
    DUP H-SUB K-CONS = IF DROP NIP 2 U< IF 0 ELSE OBJ-E-RANGE THEN EXIT THEN
    DUP H-SUB K-VECTOR = IF H-SIZE 1 SWAP WITHIN NIP IF 0 ELSE OBJ-E-RANGE THEN EXIT THEN
    DUP H-SUB K-RECORD = IF
        DROP OVER 0 _FIELD @ 2/ 1+ 1 SWAP WITHIN NIP IF 0 ELSE OBJ-E-RANGE THEN EXIT
    THEN
    DROP 2DROP OBJ-E-KIND ;

\ Store a tagged word into field I, logging a reference unless the object
\ lies in this core's current region.
: OBJ!  ( t r i -- ior )
    _OBJ-IN ?DUP IF NIP NIP NIP EXIT THEN
    2DUP _OBJ-TAGGED-FIELD? ?DUP IF NIP NIP NIP EXIT THEN
    2 PICK _OBJ-VALUE? ?DUP IF NIP NIP NIP EXIT THEN
    _FIELD 2DUP !
    SWAP _REF? IF
        DUP _OBJ-IN-REGION? IF DROP 0 ELSE _OBJ-LOG THEN
    ELSE DROP 0 THEN ;

: OBJ-RAW!  ( x r j -- ior )
    _OBJ-IN ?DUP IF NIP NIP NIP EXIT THEN
    _OBJ-RAW-ADDR ?DUP IF NIP NIP EXIT THEN ! 0 ;

\ Copy a span's bytes into a bytevector from byte AT on.
: OBJ-BYTES!  ( src len r at -- ior )
    _OBJ-IN ?DUP IF >R 2DROP 2DROP R> EXIT THEN
    OVER _OBJ-HEADER ?DUP IF >R DROP 2DROP 2DROP R> EXIT THEN
    H-SUB K-BYTES <> IF 2DROP 2DROP OBJ-E-KIND EXIT THEN
    2 PICK OVER + 2 PICK 0 _FIELD @ 2/ U> IF 2DROP 2DROP OBJ-E-RANGE EXIT THEN
    SWAP 1 _FIELD + SWAP MOVE 0 ;

\ =====================================================================
\  Making objects, inside a window (sections 5 and 8)
\ =====================================================================

: _HDR  ( sub size shape -- h )   24 LSHIFT SWAP 8 LSHIFT OR SWAP 3 LSHIFT OR 7 OR ;

: OBJ-CONS  ( car cdr -- r ior )
    _OBJ-IN ?DUP IF NIP NIP 0 SWAP EXIT THEN
    OVER _OBJ-VALUE? ?DUP IF NIP NIP 0 SWAP EXIT THEN
    DUP _OBJ-VALUE? ?DUP IF NIP NIP 0 SWAP EXIT THEN
    24 _OBJ-ALLOT ?DUP IF NIP NIP NIP 0 SWAP EXIT THEN
    K-CONS 2 _SH-CONS @ _HDR OVER !
    TUCK 16 + !  TUCK 8 + !  3 OR 0 ;

\ A header's size counts up to 65,535 fields, so a vector holds at most
\ 65,534 elements until LM1's extended header is defined (section 16).
: OBJ-VECTOR  ( fill n -- r ior )
    _OBJ-IN ?DUP IF NIP NIP 0 SWAP EXIT THEN
    DUP 0xFFFE U> IF 2DROP 0 OBJ-E-RANGE EXIT THEN
    OVER _OBJ-VALUE? ?DUP IF NIP NIP 0 SWAP EXIT THEN
    DUP 2 + CELLS _OBJ-ALLOT ?DUP IF NIP NIP NIP 0 SWAP EXIT THEN
    K-VECTOR 2 PICK 1+ _SH-VECTOR @ _HDR OVER !
    OVER 2* OVER 8 + !
    SWAP 0 ?DO 2DUP I 2 + CELLS + ! LOOP
    NIP 1 OR 0 ;

: _OBJ-BYTEVECTOR  ( src len shape -- r ior )
    >R
    _OBJ-IN ?DUP IF NIP NIP 0 SWAP R> DROP EXIT THEN
    DUP 7 + 3 RSHIFT 1+
    DUP 0xFFFF U> IF DROP 2DROP 0 OBJ-E-RANGE R> DROP EXIT THEN
    DUP 1+ CELLS _OBJ-ALLOT ?DUP IF NIP NIP NIP NIP 0 SWAP R> DROP EXIT THEN
    OC.TMP OC!
    K-BYTES SWAP R> _HDR OC.TMP OC@ !
    DUP 2* OC.TMP OC@ 8 + !
    DUP IF 0 OC.TMP OC@ 16 + 2 PICK 1- 3 RSHIFT CELLS + ! THEN
    OC.TMP OC@ 16 + SWAP MOVE
    OC.TMP OC@ 1 OR 0 ;
: OBJ-BYTES   ( src len -- r ior )   _SH-BYTES @ _OBJ-BYTEVECTOR ;
: OBJ-STRING  ( src len -- r ior )   _SH-STRING @ _OBJ-BYTEVECTOR ;

\ A record of a registered shape: nil in its tagged fields, zero in its
\ raw words.
: OBJ-RECORD  ( shape -- r ior )
    _OBJ-IN ?DUP IF NIP 0 SWAP EXIT THEN
    DUP _OBJ-KIND-DESC ?DUP 0= IF DROP 0 OBJ-E-KIND EXIT THEN
    DUP @ SWAP 8 + @
    2DUP + 1+ DUP 1+ CELLS _OBJ-ALLOT ?DUP IF NIP NIP NIP NIP NIP 0 SWAP EXIT THEN
    OC.TMP OC!
    K-RECORD SWAP 4 PICK _HDR OC.TMP OC@ !
    OVER 2* OC.TMP OC@ 8 + !
    OVER 0 ?DO _NIL OC.TMP OC@ I 2 + CELLS + ! LOOP
    SWAP 2 + SWAP 0 ?DO 0 OC.TMP OC@ 2 PICK I + CELLS + ! LOOP
    2DROP OC.TMP OC@ 1 OR 0 ;

\ =====================================================================
\  Handles and owners (section 4)
\ =====================================================================

: _OW'  ( o -- addr )   1- _OW * _OBJ-OWNERS @ + ;

: _OBJ-OWNER-OK?  ( id gen -- ior )   0> SWAP 0> AND IF 0 ELSE OBJ-E-OWNER THEN ;

\ The owner (index + 1) the token names, or 0.
: _OBJ-OWNER-FIND  ( id gen -- o | 0 )
    _OBJ-OWNER-N @ 0 ?DO
        I _OW * _OBJ-OWNERS @ + DUP @ 3 PICK = SWAP 8 + @ 2 PICK = AND IF
            2DROP I 1+ UNLOOP EXIT
        THEN
    LOOP 2DROP 0 ;

\ A free entry of the owner table, which doubles when full.  The caller
\ holds OBJ-LOCK.
: _OBJ-OWNER-MAKE  ( id gen -- o | 0 )
    0 0 _OBJ-OWNER-FIND ?DUP 0= IF
        _OBJ-OWNER-N @ _OBJ-OWNER-CAP @ = IF
            _OBJ-OWNER-CAP @ 2* 16 MAX DUP _OW * _CHIP-PRIVATE IF 2DROP 2DROP 0 EXIT THEN
            _OBJ-OWNERS @ ?DUP IF OVER _OBJ-OWNER-N @ _OW * MOVE
                _OBJ-OWNERS @ _OBJ-OWNER-CAP @ _OW * XMEM-FREE-BLOCK THEN
            _OBJ-OWNERS !  _OBJ-OWNER-CAP !
        THEN
        _OBJ-OWNER-N @ 1+ DUP _OBJ-OWNER-N !
    THEN
    >R  R@ _OW' 8 + !  R@ _OW' !  0 R@ _OW' 16 + !  0 R@ _OW' 24 + !  R> ;

\ The slots: a new segment when every slot is in use, zeroed and listed
\ in the directory before its first slot is used.  The 32-bit index is the
\ only bound.  The caller holds OBJ-LOCK.
: _OBJ-GROW-TABLE  ( -- ior )
    _HD-SEGS DUP 64 = IF DROP OBJ-E-NOMEM EXIT THEN
    _HD-FIRST OVER LSHIFT
    _OBJ-SLOTS @ OVER + 0xFFFFFFFF U> IF 2DROP OBJ-E-NOMEM EXIT THEN
    DUP CELLS CHIP-READ-WRITE _OBJ-ALLOT-REC IF DROP 2DROP OBJ-E-NOMEM EXIT THEN
    OVER _HS * _CHIP-PRIVATE IF 2DROP 2DROP OBJ-E-NOMEM EXIT THEN
    3 PICK CELLS _OBJ-SIDE + !
    2 PICK CELLS _OBJ-HD @ 16 + + !
    OVER 1+ _OBJ-HD @ 8 + !
    _OBJ-SLOTS @ TUCK + DUP _OBJ-SLOTS !
    BEGIN 2DUP < WHILE
        1- DUP _OBJ-FREE-SLOT @ SWAP HS.NEXT HS!  DUP 1+ _OBJ-FREE-SLOT !
    REPEAT
    2DROP DROP 0 ;

: _OBJ-TAKE-SLOT  ( -- index ior )
    _OBJ-FREE-SLOT @ 0= IF _OBJ-GROW-TABLE ?DUP IF 0 SWAP EXIT THEN THEN
    _OBJ-FREE-SLOT @ 1-  DUP HS.NEXT HS@ _OBJ-FREE-SLOT !
    DUP HS.GEN HS@ 0= IF 1 OVER HS.GEN HS! THEN 0 ;

\ The slot joins its owner's list, newest first, with the owner's next
\ serial.  The caller holds OBJ-LOCK.
: _OBJ-LINK  ( index o -- )
    2DUP SWAP HS.OWNER HS!
    DUP _OW' 24 + DUP @ DUP 1+ ROT ! 2 PICK HS.SERIAL HS!
    DUP _OW' 16 + @
    DUP 3 PICK HS.NEXT HS!  0 3 PICK HS.PREV HS!
    ?DUP IF 2 PICK 1+ SWAP 1- HS.PREV HS! THEN
    SWAP 1+ SWAP _OW' 16 + ! ;

\ Release a slot: out of its owner's list, freed inside the window, its
\ generation raised.  A slot whose generation reaches 2^32 - 1 is retired.
\ The caller holds OBJ-LOCK.
: _OBJ-RELEASE  ( index -- )
    >R
    R@ HS.NEXT HS@  R@ HS.PREV HS@
    DUP IF OVER SWAP 1- HS.NEXT HS! ELSE DROP DUP R@ HS.OWNER HS@ _OW' 16 + ! THEN
    ?DUP IF R@ HS.PREV HS@ SWAP 1- HS.PREV HS! THEN
    0 R@ _SLOT !
    0 R@ HS.OWNER HS!
    R@ HS.GEN HS@ 1+ DUP R@ HS.GEN HS!
    0xFFFFFFFF <> IF _OBJ-FREE-SLOT @ R@ HS.NEXT HS!  R@ 1+ _OBJ-FREE-SLOT ! THEN
    R> DROP ;

\ A handle's slot, or OBJ-E-STALE: an index beyond the table, a free slot,
\ or another generation.
: _OBJ-CHECK  ( h -- index ior )
    DUP 0xFFFFFFFF AND SWAP 32 RSHIFT
    OVER _OBJ-SLOTS @ U< 0= IF 2DROP 0 OBJ-E-STALE EXIT THEN
    OVER HS.OWNER HS@ 0= IF 2DROP 0 OBJ-E-STALE EXIT THEN
    OVER HS.GEN HS@ <> IF DROP 0 OBJ-E-STALE EXIT THEN 0 ;

\ A handle owned by the token, for a reference or for nil.
: OBJ-HANDLE  ( t id gen -- h ior )
    _OBJ-IN ?DUP IF >R DROP 2DROP 0 R> EXIT THEN
    2DUP _OBJ-OWNER-OK? ?DUP IF >R DROP 2DROP 0 R> EXIT THEN
    2 PICK _NIL <> IF 2 PICK OBJ>REF NIP ?DUP IF >R DROP 2DROP 0 R> EXIT THEN THEN
    OBJ-LOCK LOCK
    2DUP _OBJ-OWNER-FIND ?DUP 0= IF 2DUP _OBJ-OWNER-MAKE THEN
    ?DUP 0= IF OBJ-LOCK UNLOCK DROP 2DROP 0 OBJ-E-NOMEM EXIT THEN
    NIP NIP
    _OBJ-TAKE-SLOT ?DUP IF OBJ-LOCK UNLOCK >R 2DROP DROP 0 R> EXIT THEN
    TUCK SWAP _OBJ-LINK
    OBJ-LOCK UNLOCK
    2DUP _SLOT !
    OVER _NIL <> IF DUP _SLOT _OBJ-LOG ?DUP IF NIP NIP 0 SWAP EXIT THEN THEN
    NIP DUP HS.GEN HS@ 32 LSHIFT OR 0 ;

\ The object the handle names; OBJ-E-KIND while it names none.
: OBJ-GET  ( h -- r ior )
    _OBJ-IN ?DUP IF NIP 0 SWAP EXIT THEN
    _OBJ-CHECK ?DUP IF NIP 0 SWAP EXIT THEN
    _SLOT @ DUP _NIL = IF DROP 0 OBJ-E-KIND EXIT THEN
    OBJ>REF ;

: OBJ-SET  ( r h -- ior )
    _OBJ-IN ?DUP IF NIP NIP EXIT THEN
    _OBJ-CHECK ?DUP IF NIP NIP EXIT THEN
    OVER _NIL <> IF OVER OBJ>REF NIP ?DUP IF NIP NIP EXIT THEN THEN
    _SLOT 2DUP !  SWAP _NIL <> IF _OBJ-LOG ELSE DROP 0 THEN ;

\ Release one handle, in a window of its own when called outside one.
: OBJ-DROP  ( h -- ior )
    _OBJ-SERVES ?DUP IF NIP EXIT THEN
    _OBJ-CHECK ?DUP IF NIP EXIT THEN
    OC.DEPTH OC@ 0= DUP IF OBJ-OPEN ?DUP IF NIP NIP EXIT THEN THEN
    OBJ-LOCK LOCK
    OVER HS.OWNER HS@ IF SWAP _OBJ-RELEASE ELSE NIP THEN
    OBJ-LOCK UNLOCK
    IF OBJ-CLOSE THEN 0 ;

\ The serial the owner's next handle takes.
: OBJ-OWNER-MARK  ( id gen -- mark )
    _OBJ-STATE @ 2 <> IF 2DROP 0 EXIT THEN
    _OBJ-OWNER-FIND ?DUP IF _OW' 24 + @ ELSE 0 THEN ;

: _OBJ-RELEASE-SINCE  ( mark o -- )
    BEGIN
        DUP _OW' 16 + @ DUP IF 1- HS.SERIAL HS@ 2 PICK U< 0= ELSE DROP FALSE THEN
    WHILE
        DUP _OW' 16 + @ 1- _OBJ-RELEASE
    REPEAT 2DROP ;

\ Release the owner's handles made since the mark.  Once the heap is lost
\ releasing succeeds and does nothing.
: OBJ-OWNER-RELEASE-TO  ( mark id gen -- ior )
    _OBJ-STATE @ 2 > IF DROP 2DROP 0 EXIT THEN
    _OBJ-SERVES ?DUP IF NIP NIP NIP EXIT THEN
    2DUP _OBJ-OWNER-OK? ?DUP IF NIP NIP NIP EXIT THEN
    _OBJ-OWNER-FIND ?DUP 0= IF DROP 0 EXIT THEN
    OC.DEPTH OC@ 0= DUP IF OBJ-OPEN IF 2DROP DROP 0 EXIT THEN THEN
    >R OBJ-LOCK LOCK _OBJ-RELEASE-SINCE OBJ-LOCK UNLOCK
    R> IF OBJ-CLOSE THEN 0 ;

\ Release every handle the owner holds, in as many short windows as it
\ needs, outside a window; the owner's entry is then free.
: OBJ-OWNER-RELEASE  ( id gen -- ior )
    _OBJ-STATE @ 2 > IF 2DROP 0 EXIT THEN
    _OBJ-SERVES ?DUP IF NIP NIP EXIT THEN
    OC.DEPTH OC@ IF 2DROP OBJ-E-WINDOW EXIT THEN
    2DUP _OBJ-OWNER-OK? ?DUP IF NIP NIP EXIT THEN
    _OBJ-OWNER-FIND ?DUP 0= IF 0 EXIT THEN
    BEGIN DUP _OW' 16 + @ WHILE
        OBJ-OPEN IF DROP 0 EXIT THEN
        OBJ-LOCK LOCK
        32 0 DO DUP _OW' 16 + @ ?DUP IF 1- _OBJ-RELEASE ELSE LEAVE THEN LOOP
        OBJ-LOCK UNLOCK
        OBJ-CLOSE
    REPEAT
    OBJ-LOCK LOCK  0 OVER _OW' !  0 OVER _OW' 8 + !  OBJ-LOCK UNLOCK
    DROP 0 ;

\ =====================================================================
\  Kinds and collections, outside a window (sections 8 and 10)
\ =====================================================================

\ Register a record kind.  The description is copied into memory lent for
\ reading, which the registry keeps; LM1 checks it and gives the ID, and
\ the registry holds the kind before any core uses the ID.  A description
\ LM1 refuses gives OBJ-E-KIND, with LM1's status in OBJ-STATUS.
: OBJ-STATUS  ( -- status )   OC.STATUS OC@ ;
: OBJ-SHAPE  ( desc len -- shape ior )
    _OBJ-OUT ?DUP IF NIP NIP 0 SWAP EXIT THEN
    DUP 24 < IF 2DROP 0 OBJ-E-RANGE EXIT THEN
    DUP CHIP-READ _OBJ-ALLOT-REC IF DROP 2DROP 0 OBJ-E-NOMEM EXIT THEN
    >R TUCK R@ SWAP MOVE
    _OP-SHAPE R@ ROT 0 _OBJ-ASK ?DUP IF R> DROP NIP 0 SWAP EXIT THEN
    DUP CHIP-OK <> IF
        R> DROP
        DUP OBJ-NO-MEMORY = IF DROP 0 OBJ-E-NOMEM EXIT THEN
        CHIP-FAILED = IF 0 OBJ-E-FAILED ELSE 0 OBJ-E-KIND THEN EXIT
    THEN
    DROP OC.ARG0 OC@ R>
    OVER _OBJ-KIND-DESC IF DROP 0 EXIT THEN
    OBJ-LOCK LOCK OVER SWAP _OBJ-SR MC.SHAPES _OBJ-LIST-ADD OBJ-LOCK UNLOCK ;

\ Ask for a collection: 0 the young objects, 1 the whole shared heap, 2
\ the whole heap moving every object it can.  OBJ-COLLECTED then gives
\ the reply's live bytes and epoch.
: OBJ-COLLECT  ( how -- ior )
    _OBJ-OUT ?DUP IF NIP EXIT THEN
    >R _OP-COLLECT 0 0 R> _OBJ-ASK ?DUP IF NIP EXIT THEN
    DUP CHIP-OK = IF DROP 0 EXIT THEN
    CHIP-FAILED = IF _HOLD@ H-FAILED AND IF 1 _OBJ-LOSE THEN OBJ-E-FAILED ELSE OBJ-E-KIND THEN ;
: OBJ-COLLECTED  ( -- live epoch )   OC.ARG0 OC@ OC.ARG1 OC@ ;

\ The bytes of lendable memory the function holds, for its owner checks.
: OBJ-MEMORY  ( -- u )   _OBJ-PORT @ 0< IF 0 EXIT THEN _OBJ-OWNER _OBJ-PORT @ CHIP-LENT-BYTES ;
