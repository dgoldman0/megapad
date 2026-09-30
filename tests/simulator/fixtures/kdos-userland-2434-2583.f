\ =====================================================================
\  §1.15  Userland Memory Isolation
\ =====================================================================
\
\  Provides separate dictionary space for user-loaded modules
\  (tools.f, user scripts) in external RAM, protecting the kernel
\  dictionary in system RAM from overflow.
\
\  When ENTER-USERLAND is called, the Forth dictionary pointer (HERE)
\  is redirected to external memory.  All subsequent CREATE, ALLOT,
\  : definitions, VARIABLEs, etc. compile into the userland zone.
\  System words remain in Bank 0 and are still accessible.
\
\  Memory layout when userland is active (ext mem present):
\
\    System RAM (Bank 0, 1 MiB):
\      0x00000 .. dict_free   BIOS code + dictionary
\      dict_free .. kernel-data-end  topology-sized CRC owner records
\      kernel-data-end .. ~0x7F000   KDOS dictionary + system heap
\      0x80000 .. 0xFFFFF     Stacks (data + return)
\
\    External RAM (capacity reported by hardware, at EXT-MEM-BASE):
\      prior XMEM high-water .. U-DICT-BASE  existing kernel/loader objects
\      U-DICT-BASE .. U-DICT-LIMIT           userland dictionary
\      U-DICT-LIMIT .. XMEM-LIMIT            XMEM bump allocator
\
\  Words:
\    ULAND           ( -- addr )  flag variable: 0=system, 1=userland
\    ENTER-USERLAND  ( -- )       switch HERE to userland zone
\    LEAVE-USERLAND  ( -- )       switch HERE back to system dict
\    .USERLAND       ( -- )       display userland status

VARIABLE ULAND          0 ULAND !
VARIABLE SYS-HERE-SAVE  0 SYS-HERE-SAVE !
VARIABLE U-DICT-HERE    0 U-DICT-HERE !
VARIABLE U-DICT-BASE    0 U-DICT-BASE !
VARIABLE U-DICT-LIMIT   0 U-DICT-LIMIT !
VARIABLE U-INIT-DONE    0 U-INIT-DONE !
VARIABLE U-XMEM-RESERVE 0 U-XMEM-RESERVE !
VARIABLE _U-AVAILABLE   0 _U-AVAILABLE !

\ U-ZONE-SIZE ( -- u )  Capacity of the initialized dictionary interval.
: U-ZONE-SIZE  ( -- u )
    U-DICT-LIMIT @ U-DICT-BASE @ - ;

\ U-XMEM-RESERVE! ( u -- )  Select post-partition general-XMEM capacity.
\   Zero restores the capacity-derived default (half the available span).
\   A positive request is rounded up to the allocator's 16-byte boundary at
\   initialization.  Partition policy is immutable once userland is live.
: U-XMEM-RESERVE!  ( u -- )
    U-INIT-DONE @ ABORT" Userland partition already initialized"
    DUP 0< ABORT" Invalid XMEM reserve"
    U-XMEM-RESERVE ! ;

\ Reject any manufactured free-list node that intersects the live dictionary.
\ XMEM-FREE-BLOCK has already proved addr+size is non-wrapping and within the
\ physical external-memory limit before this hook executes.  A reclaimed
\ pre-init loader object may remain wholly below U-DICT-BASE and is valid.
: _U-XMEM-FREE-SPAN-CHECK  ( addr size -- )
    U-INIT-DONE @ 0= IF 2DROP EXIT THEN
    OVER U-DICT-LIMIT @ < IF
        2DUP + U-DICT-BASE @ >
        ABORT" XMEM-FREE: user dictionary overlap"
    THEN
    2DROP ;

' _U-XMEM-FREE-SPAN-CHECK IS _XMEM-FREE-SPAN-CHECK

\ USERLAND-INIT ( -- )  Seal a dictionary/general-XMEM partition above every
\   live pre-init allocation.  The default assigns half the remaining capacity
\   to each side; U-XMEM-RESERVE! may instead provide an explicit runtime
\   requirement.  Called lazily on first ENTER-USERLAND.  No-op if ext mem is
\   absent or already initialized.  No partition cell is published until the
\   complete capacity calculation has passed.
: USERLAND-INIT  ( -- )
    U-INIT-DONE @ IF EXIT THEN
    XMEM? 0= IF EXIT THEN
    \ Start userland dict above any live prior XMEM allocations.  Loader
    \ source buffers use reclaimable ALLOCATE/FREE storage, while persistent
    \ XBUF kernel buffers remain below this boundary.  Align to the XMEM
    \ allocator's 16-byte recyclable-block geometry (and therefore cells).
    XMEM-HERE @ 15 + -16 AND
    DUP XMEM-LIMIT @ >= ABORT" Insufficient ext mem for userland partition"
    XMEM-LIMIT @ OVER - DUP _U-AVAILABLE ! DROP
    U-XMEM-RESERVE @ ?DUP IF
        15 + -16 AND
    ELSE
        _U-AVAILABLE @ 2/ 15 + -16 AND
    THEN
    DUP 16 < ABORT" Insufficient XMEM reserve"
    DUP _U-AVAILABLE @ >= ABORT" Insufficient ext mem for userland dictionary"
    XMEM-LIMIT @ SWAP -              ( base dict-limit )
    2DUP >= ABORT" Insufficient ext mem for userland dictionary"
    \ Validate the physical interval before publishing any KDOS partition
    \ cell.  USERLAND-INIT is independently callable, so disarm the checked
    \ interval again until ENTER-USERLAND redirects HERE into it.
    2DUP DICT-BOUNDS! DICT-BOUNDS-OFF
    DUP U-DICT-LIMIT !
    OVER U-DICT-BASE !
    OVER U-DICT-HERE !
    \ Push the general allocator to the sealed dictionary limit.  This is also
    \ the reset floor, so neither reset nor bump allocation can enter the zone.
    DUP XMEM-HERE ! DUP XMEM-FLOOR !
    2DROP
    1 U-INIT-DONE ! ;

\ ENTER-USERLAND ( -- )  Save system HERE, redirect to userland dict.
: ENTER-USERLAND  ( -- )
    XMEM? 0= IF ." No ext mem -- userland disabled" CR EXIT THEN
    ULAND @ IF EXIT THEN             \ already in userland
    U-INIT-DONE @ 0= IF USERLAND-INIT THEN
    HERE SYS-HERE-SAVE !             \ save system dict pointer
    U-DICT-BASE @ U-DICT-LIMIT @ DICT-BOUNDS!
    U-DICT-HERE @ HERE - ALLOT       \ HERE <- userland dict pointer
    1 ULAND ! ;

\ LEAVE-USERLAND ( -- )  Save userland HERE, restore system dict.
: LEAVE-USERLAND  ( -- )
    ULAND @ 0= IF EXIT THEN          \ not in userland
    HERE U-DICT-HERE !               \ save userland dict pointer
    DICT-BOUNDS-OFF
    SYS-HERE-SAVE @ HERE - ALLOT     \ HERE <- system dict pointer
    0 ULAND ! ;

\ U-HERE ( -- addr )  Current userland dictionary pointer.
: U-HERE  ( -- addr )
    ULAND @ IF HERE ELSE U-DICT-HERE @ THEN ;

\ U-USED ( -- u )  Bytes used in userland dictionary.
: U-USED  ( -- u )
    ULAND @ IF HERE ELSE U-DICT-HERE @ THEN
    U-DICT-BASE @ - ;

\ U-FREE ( -- u )  Bytes remaining in userland dictionary zone.
: U-FREE  ( -- u )
    U-DICT-LIMIT @ U-HERE - ;

\ .USERLAND ( -- )  Display userland status.
: .USERLAND  ( -- )
    ." Userland:" CR
    XMEM? IF
        ."   Mode  = " ULAND @ IF ." ACTIVE" ELSE ." system" THEN CR
        ."   Base  = " U-DICT-BASE @ . CR
        ."   Limit = " U-DICT-LIMIT @ . CR
        ."   Used  = " U-USED . ." bytes" CR
        ."   Free  = " U-FREE . ." bytes" CR
        ."   XMEM reserve = " XMEM-LIMIT @ U-DICT-LIMIT @ - . ." bytes" CR
    ELSE
        ."   (no ext mem -- userland disabled)" CR
    THEN ;
