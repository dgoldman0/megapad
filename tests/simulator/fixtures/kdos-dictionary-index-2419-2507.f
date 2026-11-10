\ _DICT-POW2-FLOOR ( u -- p )  greatest power of two not above u
: _DICT-POW2-FLOOR  ( u -- p )
    DUP 0= IF EXIT THEN
    1 SWAP
    BEGIN DUP 1 > WHILE
        2/ SWAP 2* SWAP
    REPEAT DROP ;

VARIABLE _DICT-INDEX-DONE  0 _DICT-INDEX-DONE !

\ The index grows instead of filling.  Through DICT-INDEX-NOTIFY! the BIOS
\ calls _DICT-INDEX-GROW once the index holds three quarters of its slots.
\ It binds a table twice the size from the XMEM allocator, the BIOS rebuilds
\ that table from the dictionary, and the old table returns to the free list.
\ The index only speeds up lookup, so a new table may take at most half of
\ the free XMEM tail and never its last bytes.  If XMEM cannot supply the
\ table, the notification is re-armed for the moment the table is full;
\ after a second refusal the BIOS's saturated fallback through the linked
\ dictionary remains in charge.
VARIABLE _DICT-INDEX-GROW-XT  0 _DICT-INDEX-GROW-XT !

\ _DICT-INDEX-ARM ( count -- )  call _DICT-INDEX-GROW at this index count
: _DICT-INDEX-ARM  ( count -- )
    _DICT-INDEX-GROW-XT @ DICT-INDEX-NOTIFY! ;

\ _DICT-INDEX-WATERMARK ( slots -- count )  three quarters of slots
: _DICT-INDEX-WATERMARK  ( slots -- count )
    DUP 2/ SWAP 2/ 2/ + ;

\ _DICT-INDEX-TAKE ( u -- addr ior )  XMEM-ALLOT? within half the free tail
: _DICT-INDEX-TAKE  ( u -- addr ior )
    DUP XMEM-FREE 2/ U> IF DROP 0 -1 EXIT THEN
    XMEM-ALLOT? ;

: _DICT-INDEX-GROW  ( -- )
    DICT-INDEX@ 1 AND 0= IF 2DROP DROP EXIT THEN    ( base slots count )
    \ The XMEM allocator is core-0 only; core 0's next definition retries.
    COREID IF 1+ _DICT-INDEX-ARM 2DROP EXIT THEN
    OVER 2* DUP 16 * _DICT-INDEX-TAKE     ( base slots count slots2 base2 ior )
    IF  2DROP OVER < IF _DICT-INDEX-ARM ELSE DROP THEN DROP EXIT  THEN
    ROT DROP                              ( base slots slots2 base2 )
    2DUP SWAP DICT-INDEX!                 ( base slots slots2 base2 status )
    1 = IF SWAP 16 * XMEM-FREE-BLOCK 2DROP EXIT THEN
    DROP _DICT-INDEX-WATERMARK _DICT-INDEX-ARM
    16 * XMEM-FREE-BLOCK ;

' _DICT-INDEX-GROW _DICT-INDEX-GROW-XT !

\ _DICT-INDEX-INIT ( -- )
\   Reserve at most 1/128 of currently free XMEM for the first table of the
\   BIOS dictionary index, below the XMEM reset floor.  A power-of-two slot
\   count keeps probing masked; the canonical 128 MiB arrangement selects
\   65,536 16-byte slots (1 MiB), which holds the Desktop dictionary without
\   growing.  No-XMEM systems explicitly leave the optional index disabled.
\   A rebuild may report saturation (status 2) without compromising
\   linked-list lookup.  This boot-only initializer is one-shot.
: _DICT-INDEX-INIT  ( -- )
    ?CORE0
    _DICT-INDEX-DONE @ IF EXIT THEN
    1 _DICT-INDEX-DONE !
    XMEM? 0= IF 0 0 DICT-INDEX! DROP EXIT THEN
    XMEM-FREE 2048 / _DICT-POW2-FLOOR
    DUP 0= IF DROP 0 0 DICT-INDEX! DROP EXIT THEN
    DUP 16 * XMEM-ALLOT?              ( slots base ior )
    IF 2DROP 0 0 DICT-INDEX! DROP EXIT THEN
    OVER DICT-INDEX!                  ( slots status )
    DUP 1 = ABORT" BIOS rejected dictionary index"
    DROP                              ( slots ; status 0 or safe status 2 )
    XMEM-HERE @ XMEM-FLOOR !
    _DICT-INDEX-WATERMARK _DICT-INDEX-ARM ;

_DICT-INDEX-INIT

\ _DICT-XMEM-RESET ( -- )  XMEM-RESET that keeps the dictionary index
\   (XMEM-RESET) returns everything above XMEM-FLOOR to the bump allocator.
\   A table that grew into that span is unbound, then bound again at the
\   floor, which rises past it as XBUF raises it for other persistent kernel
\   buffers.  A table below the floor is untouched.  A later subsystem that
\   replaces XMEM-RESET should call this action rather than (XMEM-RESET).
: _DICT-XMEM-RESET  ( -- )
    DICT-INDEX@ 2DROP                     ( base slots )
    OVER XMEM-FLOOR @ U< OVER 0= OR IF 2DROP (XMEM-RESET) EXIT THEN
    NIP 0 0 DICT-INDEX! DROP              ( slots )
    (XMEM-RESET)
    DUP 16 * XMEM-ALLOT? IF 2DROP EXIT THEN    ( slots base )
    SWAP DICT-INDEX! DROP
    XMEM-HERE @ XMEM-FLOOR ! ;

' _DICT-XMEM-RESET IS XMEM-RESET
