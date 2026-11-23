\ =====================================================================
\  §1.1b  Arena Allocator
\ =====================================================================
\
\  Region-aware scoped allocation for scratch memory.
\  An arena is a pre-allocated region with O(1) bump allocation
\  and O(1) bulk deallocation.  No per-object headers or free list.
\
\  Arena descriptor (4 cells = 32 bytes, in dictionary):
\    +0   base     start of data region
\    +8   size     total capacity in bytes
\    +16  ptr      current bump pointer
\    +24  source   0 = heap, 1 = XMEM, 2 = HBW
\
\  Full design: docs/arenas.md

\ -- Source constants --
0 CONSTANT A-HEAP    \ arena backed by the general ALLOCATE/FREE route
1 CONSTANT A-XMEM    \ arena backed by external RAM
2 CONSTANT A-HBW     \ arena backed by HBW math RAM

\ -- Field accessors --
: A.BASE    ( arena -- addr )  ;           \ +0
: A.SIZE    ( arena -- addr )  8 + ;       \ +8
: A.PTR     ( arena -- addr )  16 + ;      \ +16
: A.SOURCE  ( arena -- addr )  24 + ;      \ +24

\ -- Scratch variables --
VARIABLE AR-SZ     \ requested size
VARIABLE AR-SRC    \ source id
VARIABLE AR-BLK    \ backing block address

\ (AR-ALLOC-BACKING) ( size source -- addr ior )
\   Dispatch to the correct region allocator.
\   Uses ?-variants so all paths return ior uniformly.
: (AR-ALLOC-BACKING)  ( size source -- addr ior )
    DUP 0 = IF  DROP ALLOCATE EXIT  THEN
    DUP 1 = IF  DROP XMEM-ALLOT? EXIT  THEN
    2 = IF  HBW-ALLOT? EXIT  THEN
    DROP 0 -1 ;    \ unknown source

\ (AR-FREE-BACKING) ( addr size source -- )
\   Free the backing block.  Heap blocks are individually freed;
\   XMEM blocks are returned to the XMEM free-list for reuse.
\   HBW blocks are still abandoned until HBW-RESET.
: (AR-FREE-BACKING)  ( addr size source -- )
    DUP 0 = IF  DROP DROP FREE EXIT  THEN
    1 = IF  XMEM-FREE-BLOCK EXIT  THEN
    2DROP ;    \ HBW — abandoned (short-lived, 3 MiB region)

\ ARENA-NEW ( size source -- arena ior )
\   Allocate a backing region, build descriptor in dictionary.
\   Sources: 0=A-HEAP, 1=A-XMEM, 2=A-HBW.
\   NOTE: the 32-byte descriptor is permanently committed to the
\   dictionary.  For temporary arenas created/destroyed in a loop,
\   use ARENA-NEW-AT with a pre-allocated descriptor address.
\ Core-0 only — uses shared scratch variables (AR-SZ, AR-SRC, AR-BLK).
: ARENA-NEW  ( size source -- arena ior )
    ?CORE0
    OVER 0= IF  2DROP 0 -1 EXIT  THEN      \ zero size → fail
    AR-SRC !  AR-SZ !
    AR-SZ @ AR-SRC @ (AR-ALLOC-BACKING) IF
        DROP 0 -1 EXIT                      \ alloc failed
    THEN
    AR-BLK !
    HERE                                     ( arena )
    AR-BLK @ ,                               \ +0  base
    AR-SZ @ ,                                \ +8  size
    AR-BLK @ ,                               \ +16 ptr = base (empty)
    AR-SRC @ ,                               \ +24 source
    0 ;                                      ( arena 0 )

\ ARENA-NEW-AT ( desc size source -- ior )
\   Like ARENA-NEW but writes the 32-byte descriptor at a user-provided
\   address instead of consuming dictionary space.  Useful for temporary
\   arenas created/destroyed in a loop — avoids the slow dictionary leak.
\   'desc' must point to >= 32 bytes of writable, cell-aligned storage
\   (e.g. a CREATE/ALLOT block, a VARIABLE cluster, or an arena-allotted
\   region in another arena).
\ Core-0 only — uses shared scratch variables (AR-SZ, AR-SRC, AR-BLK).
: ARENA-NEW-AT  ( desc size source -- ior )
    ?CORE0
    OVER 0= IF  DROP 2DROP -1 EXIT  THEN     \ zero size → fail
    AR-SRC !  AR-SZ !                         ( desc )
    AR-SZ @ AR-SRC @ (AR-ALLOC-BACKING) IF   ( desc 0 )
        2DROP -1 EXIT                         \ alloc failed
    THEN
    AR-BLK !                                  ( desc )
    AR-BLK @ OVER !                           \ +0  base
    AR-SZ @  OVER 8 + !                       \ +8  size
    AR-BLK @ OVER 16 + !                      \ +16 ptr = base
    AR-SRC @ SWAP 24 + !                      \ +24 source
    0 ;                                       ( 0 )

\ ARENA-USED ( arena -- u )  bytes consumed
: ARENA-USED  ( arena -- u )
    DUP A.PTR @  SWAP A.BASE @ - ;

\ ARENA-FREE ( arena -- u )  bytes remaining
: ARENA-FREE  ( arena -- u )
    DUP A.SIZE @  SWAP ARENA-USED - ;

\ ARENA-ALLOT ( arena u -- addr )
\   Bump-allocate u bytes (8-byte aligned).  Aborts on overflow
\   or if the arena has been destroyed.
: ARENA-ALLOT  ( arena u -- addr )
    OVER A.BASE @ 0= ABORT" arena destroyed"
    7 + -8 AND                               ( arena u-aligned )
    OVER ARENA-FREE OVER < ABORT" arena full"
    OVER A.PTR @                             ( arena u addr )
    -ROT                                     ( addr arena u )
    OVER A.PTR @ +  SWAP A.PTR ! ;           ( addr )

\ ARENA-ALLOT? ( arena u -- addr ior )
\   Like ARENA-ALLOT but returns ior instead of aborting.
: ARENA-ALLOT?  ( arena u -- addr ior )
    OVER A.BASE @ 0= IF  2DROP 0 -1 EXIT  THEN  \ destroyed
    7 + -8 AND                               ( arena u-aligned )
    OVER ARENA-FREE OVER < IF
        2DROP 0 -1 EXIT                      \ overflow
    THEN
    OVER A.PTR @                             ( arena u addr )
    -ROT                                     ( addr arena u )
    OVER A.PTR @ +  SWAP A.PTR !             ( addr )
    0 ;

\ ARENA-RESET ( arena -- )
\   Rewind ptr to base.  All allocations logically freed.  O(1).
: ARENA-RESET  ( arena -- )
    DUP A.BASE @  SWAP A.PTR ! ;

\ ARENA-DESTROY ( arena -- )
\   Free the backing region and zero the descriptor.
\   Heap blocks are individually freed via FREE.
\   XMEM blocks are returned to the XMEM free-list for reuse.
\   HBW blocks are abandoned until HBW-RESET.
\   Core-0 only — calls (AR-FREE-BACKING) which uses shared state.
: ARENA-DESTROY  ( arena -- )
    ?CORE0
    DUP A.BASE @  OVER A.SIZE @  ROT DUP >R A.SOURCE @
    (AR-FREE-BACKING)
    R>
    0 OVER !  0 OVER 8 + !                   \ zero base, size
    0 OVER 16 + !  0 SWAP 24 + ! ;           \ zero ptr, source

\ -- Snapshots: save/restore bump pointer for transactional scratch --

\ ARENA-SNAP ( arena -- snap )
\   Save the current bump pointer as a snapshot token.
: ARENA-SNAP  ( arena -- snap )
    A.PTR @ ;

\ ARENA-ROLLBACK ( arena snap -- )
\   Restore the bump pointer to a previous snapshot.
\   Everything allocated after the snapshot is logically freed.
\   Validates that snap falls within [base, base+size].
: ARENA-ROLLBACK  ( arena snap -- )
    OVER A.BASE @                        ( arena snap base )
    OVER SWAP                            ( arena snap snap base )
    < ABORT" rollback: snap below base"  ( arena snap )
    OVER DUP A.BASE @ SWAP A.SIZE @ +   ( arena snap limit )
    OVER SWAP                            ( arena snap snap limit )
    > ABORT" rollback: snap above limit" ( arena snap )
    SWAP A.PTR ! ;

\ ARENA-SNAP-DROP ( snap -- )
\   Discard a snapshot token.  No-op (for API symmetry).
: ARENA-SNAP-DROP  ( snap -- )
    DROP ;

\ -- Scoped arena stack: implicit "current arena" for polymorphic code --

4 CONSTANT ARENA-STK-DEPTH       \ max nesting depth
CREATE ARENA-STK  ARENA-STK-DEPTH 8 * ALLOT
VARIABLE ARENA-SP   0 ARENA-SP !  \ stack pointer (0 = empty)

\ CURRENT-ARENA ( -- arena )  return the arena on top of stack
: CURRENT-ARENA  ( -- arena )
    ARENA-SP @ 0= ABORT" no current arena"
    ARENA-STK  ARENA-SP @ 1- 8 * + @ ;

\ ARENA-PUSH ( arena -- )  push arena onto the scoped stack
: ARENA-PUSH  ( arena -- )
    ARENA-SP @ ARENA-STK-DEPTH >= ABORT" arena stack full"
    ARENA-STK  ARENA-SP @ 8 * + !
    1 ARENA-SP +! ;

\ ARENA-POP ( -- )  pop the current arena from the scoped stack
: ARENA-POP  ( -- )
    ARENA-SP @ 0= ABORT" arena stack underflow"
    -1 ARENA-SP +! ;

\ AALLOT ( u -- addr )  allocate from current arena
: AALLOT  ( u -- addr )
    CURRENT-ARENA SWAP ARENA-ALLOT ;

\ .ARENA ( arena -- )  print arena status
: .ARENA  ( arena -- )
    ." Arena: base=" DUP A.BASE @ .
    ."  size=" DUP A.SIZE @ .
    ."  used=" DUP ARENA-USED .
    ."  free=" DUP ARENA-FREE .
    ."  src=" A.SOURCE @ DUP 0 = IF DROP ." heap" ELSE
    DUP 1 = IF DROP ." xmem" ELSE
    2 = IF ." hbw" ELSE ." ?" THEN THEN THEN CR ;
