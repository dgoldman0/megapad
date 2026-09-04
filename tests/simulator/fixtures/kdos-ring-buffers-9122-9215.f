\ =====================================================================
\  §18  Ring Buffer Primitives
\ =====================================================================
\
\  Lock-aware circular buffer for multi-core producer/consumer patterns.
\
\  Ring descriptor layout (6 cells = 48 bytes, then payload):
\    +0   elem-size   bytes per element
\    +8   capacity    max number of elements
\    +16  head        index of oldest element (read position)
\    +24  tail        index of next write position
\    +32  count       current number of elements
\    +40  lock#       spinlock number for atomicity
\    +48  data...     capacity × elem-size bytes

: RING  ( elem-size capacity "name" -- )
    HERE >R
    SWAP ,                      \ +0  elem-size
    DUP ,                       \ +8  capacity
    0 ,                         \ +16 head = 0
    0 ,                         \ +24 tail = 0
    0 ,                         \ +32 count = 0
    RING-LOCK ,                 \ +40 lock#
    R@ @ *                      \ capacity × elem-size
    ALLOT                       \ allot data area
    R> CONSTANT ;

\ --- Ring accessors ---
: RING.ESIZE  ( ring -- n )     @ ;
: RING.CAP    ( ring -- n )     8 + @ ;
: RING.HEAD   ( ring -- addr )  16 + ;
: RING.TAIL   ( ring -- addr )  24 + ;
: RING.COUNT  ( ring -- n )     32 + @ ;
: RING.LOCK   ( ring -- n )     40 + @ ;
: RING.DATA   ( ring -- addr )  48 + ;

: RING-FULL?  ( ring -- flag )  DUP RING.COUNT SWAP RING.CAP >= ;
: RING-EMPTY? ( ring -- flag )  RING.COUNT 0= ;
: RING-COUNT  ( ring -- n )     RING.COUNT ;

\ --- RING-PUSH ( elem-addr ring -- flag ) ---
\ Append element to tail.  Returns 0 if full, -1 on success.
VARIABLE _RP-RING

: RING-PUSH  ( elem-addr ring -- flag )
    DUP _RP-RING !
    DUP RING.LOCK LOCK
    DUP RING-FULL? IF
        2DROP 0
    ELSE
        >R
        \ dst = data + tail × esize
        R@ RING.TAIL @ R@ RING.ESIZE * R@ RING.DATA +
        R@ RING.ESIZE CMOVE
        \ tail = (tail + 1) % cap
        R@ RING.TAIL @ 1+ R@ RING.CAP MOD R@ RING.TAIL !
        \ count++
        1 R> 32 + +!
        -1
    THEN
    _RP-RING @ RING.LOCK UNLOCK ;

\ --- RING-POP ( elem-addr ring -- flag ) ---
\ Dequeue oldest element from head.  Returns 0 if empty, -1 on success.
: RING-POP  ( elem-addr ring -- flag )
    DUP _RP-RING !
    DUP RING.LOCK LOCK
    DUP RING-EMPTY? IF
        2DROP 0
    ELSE
        >R
        \ src = data + head × esize
        R@ RING.HEAD @ R@ RING.ESIZE * R@ RING.DATA +
        SWAP R@ RING.ESIZE CMOVE
        \ head = (head + 1) % cap
        R@ RING.HEAD @ 1+ R@ RING.CAP MOD R@ RING.HEAD !
        \ count--
        -1 R> 32 + +!
        -1
    THEN
    _RP-RING @ RING.LOCK UNLOCK ;

\ --- RING-PEEK ( idx ring -- elem-addr | 0 ) ---
\ Read element at index without consuming.  Lock-free.
: RING-PEEK  ( idx ring -- elem-addr | 0 )
    >R
    DUP R@ RING.COUNT >= IF
        DROP R> DROP 0
    ELSE
        R@ RING.HEAD @ + R@ RING.CAP MOD
        R@ RING.ESIZE * R> RING.DATA +
    THEN ;

\ =====================================================================
