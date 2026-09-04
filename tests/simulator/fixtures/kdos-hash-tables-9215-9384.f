\ =====================================================================
\  §19  Hash Table Primitives
\ =====================================================================
\
\  Open-addressing hash table with linear probing.
\  Uses CRC-32 for hashing.  Write operations are lock-protected;
\  reads (HT-GET, HT-EACH) are lock-free.
\
\  Hash table descriptor layout (5 cells + data = 40 + data bytes):
\    +0   keysize     bytes per key
\    +8   valsize     bytes per value
\    +16  slots       number of slots
\    +24  count       occupied slot count
\    +32  lock#       spinlock number
\    +40  data...     slots × (1 + keysize + valsize) bytes
\
\  Each slot:
\    [0]  flag        0 = empty, 1 = occupied, 2 = tombstone
\    [1..keysize]     key bytes
\    [1+keysize..]    value bytes

VARIABLE _HT-KSIZE
VARIABLE _HT-VSIZE
VARIABLE _HT-NSLOTS

: HASHTABLE  ( keysize valsize slots "name" -- )
    _HT-NSLOTS !  _HT-VSIZE !  _HT-KSIZE !
    HERE >R
    _HT-KSIZE @  ,              \ +0  keysize
    _HT-VSIZE @  ,              \ +8  valsize
    _HT-NSLOTS @ ,              \ +16 slots
    0 ,                          \ +24 count = 0
    HT-LOCK ,                    \ +32 lock#
    \ total data = slots × (1 + keysize + valsize)
    _HT-KSIZE @ _HT-VSIZE @ + 1+
    _HT-NSLOTS @ *
    DUP ALLOT                    \ allot data area
    R@ 40 + SWAP 0 FILL         \ zero-fill (all slots empty)
    R> CONSTANT ;

\ --- Hash table accessors ---
: HT.KSIZE  ( ht -- n )     @ ;
: HT.VSIZE  ( ht -- n )     8 + @ ;
: HT.SLOTS  ( ht -- n )     16 + @ ;
: HT.COUNT  ( ht -- n )     24 + @ ;
: HT.LOCK   ( ht -- n )     32 + @ ;
: HT.DATA   ( ht -- addr )  40 + ;
: HT.STRIDE ( ht -- n )     DUP @ SWAP 8 + @ + 1+ ;

\ --- HT-SLOT ( slot# ht -- slot-addr ) ---
: HT-SLOT   ( slot# ht -- addr )  TUCK HT.STRIDE * SWAP HT.DATA + ;

\ --- HT-HASH ( key-addr ht -- slot# ) ---
: HT-HASH   ( key-addr ht -- slot# )
    DUP >R HT.KSIZE CRC32-BUF R> HT.SLOTS MOD ;

\ --- Slot field helpers ---
: HT-KEY    ( slot-addr -- key-addr )   1+ ;
: HT-VAL    ( slot-addr ht -- val-addr )  HT.KSIZE 1+ + ;

\ --- HT-COUNT ( ht -- n ) ---
: HT-COUNT  ( ht -- n )  24 + @ ;

\ --- HT-PUT ( key-addr val-addr ht -- ) ---
\ Insert or update.  Lock-protected.
VARIABLE _HTP-KEY
VARIABLE _HTP-VAL
VARIABLE _HTP-HT

: HT-PUT  ( key-addr val-addr ht -- )
    DUP _HTP-HT !  DUP HT.LOCK LOCK
    DROP _HTP-VAL !  _HTP-KEY !
    _HTP-KEY @ _HTP-HT @ HT-HASH
    _HTP-HT @ HT.SLOTS 0 DO
        DUP _HTP-HT @ HT-SLOT
        DUP C@ 0= OVER C@ 2 = OR IF       \ empty or tombstone → insert
            1 OVER C!                       \ mark occupied
            DUP HT-KEY _HTP-KEY @ SWAP _HTP-HT @ HT.KSIZE CMOVE
            DUP _HTP-HT @ HT-VAL _HTP-VAL @ SWAP _HTP-HT @ HT.VSIZE CMOVE
            1 _HTP-HT @ 24 + +!            \ count++
            DROP DROP
            _HTP-HT @ HT.LOCK UNLOCK
            UNLOOP EXIT
        THEN
        DUP C@ 1 = IF                      \ occupied → check key match
            DUP HT-KEY _HTP-KEY @ _HTP-HT @ HT.KSIZE SAMESTR? IF
                DUP _HTP-HT @ HT-VAL _HTP-VAL @ SWAP _HTP-HT @ HT.VSIZE CMOVE
                DROP DROP
                _HTP-HT @ HT.LOCK UNLOCK
                UNLOOP EXIT
            THEN
        THEN
        DROP
        1+ _HTP-HT @ HT.SLOTS MOD
    LOOP
    DROP
    _HTP-HT @ HT.LOCK UNLOCK ;

\ --- HT-GET ( key-addr ht -- val-addr | 0 ) ---
\ Lookup key.  Lock-free.  Returns pointer to value or 0.
VARIABLE _HTG-KEY
VARIABLE _HTG-HT

: HT-GET  ( key-addr ht -- val-addr | 0 )
    _HTG-HT !  _HTG-KEY !
    _HTG-KEY @ _HTG-HT @ HT-HASH
    _HTG-HT @ HT.SLOTS 0 DO
        DUP _HTG-HT @ HT-SLOT
        DUP C@ 0= IF                       \ empty → not found
            2DROP 0 UNLOOP EXIT
        THEN
        DUP C@ 1 = IF                      \ occupied → check key
            DUP HT-KEY _HTG-KEY @ _HTG-HT @ HT.KSIZE SAMESTR? IF
                _HTG-HT @ HT-VAL
                NIP UNLOOP EXIT
            THEN
        THEN
        DROP                                \ skip tombstones
        1+ _HTG-HT @ HT.SLOTS MOD
    LOOP
    DROP 0 ;

\ --- HT-DEL ( key-addr ht -- flag ) ---
\ Remove entry.  Returns -1 if found and deleted, 0 if absent.
VARIABLE _HTD-KEY

: HT-DEL  ( key-addr ht -- flag )
    DUP _HTP-HT !  DUP HT.LOCK LOCK
    SWAP _HTD-KEY !  DROP
    _HTD-KEY @ _HTP-HT @ HT-HASH
    _HTP-HT @ HT.SLOTS 0 DO
        DUP _HTP-HT @ HT-SLOT
        DUP C@ 0= IF
            2DROP 0
            _HTP-HT @ HT.LOCK UNLOCK
            UNLOOP EXIT
        THEN
        DUP C@ 1 = IF
            DUP HT-KEY _HTD-KEY @ _HTP-HT @ HT.KSIZE SAMESTR? IF
                2 OVER C!                   \ tombstone
                -1 _HTP-HT @ 24 + +!       \ count--
                2DROP -1
                _HTP-HT @ HT.LOCK UNLOCK
                UNLOOP EXIT
            THEN
        THEN
        DROP
        1+ _HTP-HT @ HT.SLOTS MOD
    LOOP
    DROP 0
    _HTP-HT @ HT.LOCK UNLOCK ;

\ --- HT-EACH ( xt ht -- ) ---
\ Iterate occupied slots.  Calls xt with ( key-addr val-addr -- ).
VARIABLE _HTE-XT
VARIABLE _HTE-HT

: HT-EACH  ( xt ht -- )
    _HTE-HT !  _HTE-XT !
    _HTE-HT @ HT.SLOTS 0 DO
        I _HTE-HT @ HT-SLOT
        DUP C@ 1 = IF
            DUP HT-KEY
            OVER _HTE-HT @ HT-VAL
            _HTE-XT @ EXECUTE
        THEN
        DROP
    LOOP ;

\ =====================================================================
