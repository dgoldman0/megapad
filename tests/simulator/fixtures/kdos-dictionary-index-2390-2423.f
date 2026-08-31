\ _DICT-POW2-FLOOR ( u -- p )  greatest power of two not above u
: _DICT-POW2-FLOOR  ( u -- p )
    DUP 0= IF EXIT THEN
    1 SWAP
    BEGIN DUP 1 > WHILE
        2/ SWAP 2* SWAP
    REPEAT DROP ;

VARIABLE _DICT-INDEX-DONE  0 _DICT-INDEX-DONE !

\ _DICT-INDEX-INIT ( -- )
\   Permanently reserve at most 1/128 of currently free XMEM for the BIOS
\   dictionary index.  A power-of-two slot count keeps probing masked; the
\   canonical 128 MiB arrangement selects 65,536 16-byte slots (1 MiB).
\   No-XMEM systems explicitly leave the optional index disabled.  A rebuild
\   may report saturation (status 2) without compromising linked-list lookup.
\   This boot-only initializer is one-shot so neither its table nor the XMEM
\   reset floor can be silently replaced later.
: _DICT-INDEX-INIT  ( -- )
    ?CORE0
    _DICT-INDEX-DONE @ IF EXIT THEN
    1 _DICT-INDEX-DONE !
    XMEM? 0= IF 0 0 DICT-INDEX! DROP EXIT THEN
    XMEM-FREE 2048 / _DICT-POW2-FLOOR
    DUP 0= IF DROP 0 0 DICT-INDEX! DROP EXIT THEN
    DUP 16 * XMEM-ALLOT?              ( slots base ior )
    IF 2DROP 0 0 DICT-INDEX! DROP EXIT THEN
    SWAP                              ( base slots )
    DICT-INDEX!                       ( status )
    DUP 1 = ABORT" BIOS rejected dictionary index"
    DROP                              ( status 0 or safe status 2 fallback )
    XMEM-HERE @ XMEM-FLOOR ! ;

_DICT-INDEX-INIT
