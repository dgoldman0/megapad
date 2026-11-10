
\ ── FD Pool — fixed pool of reusable file descriptors ────────────────
\
\  16 slots × 72 bytes = 1,152 bytes, allocated once at boot.
\  Slot layout (9 cells):
\    +0  in_use   (cell)  0=free, -1=in-use
\    +8  start_sec (cell)  — fdesc offset +0
\    +16 max_sec   (cell)  — fdesc offset +8
\    +24 used_bytes (cell) — fdesc offset +16
\    +32 cursor    (cell)  — fdesc offset +24
\    +40 dir_slot  (cell)  — fdesc offset +32
\    +48 ext1_start(cell)  — fdesc offset +40
\    +56 ext1_count(cell)  — fdesc offset +48
\    +64 reserved  (cell)  — padding
\  The returned fdesc points to +8, so existing field accessors
\  (F.START +0, F.MAX +8, etc.) remain unchanged.

16 CONSTANT FD-MAX
72 CONSTANT FD-SLOT-SZ
CREATE FD-POOL  FD-MAX FD-SLOT-SZ * ALLOT
FD-POOL FD-MAX FD-SLOT-SZ * 0 FILL          \ zero the pool

\ FD-SLOT ( n -- addr )  address of pool slot n  (0..15)
: FD-SLOT  ( n -- addr )  FD-SLOT-SZ * FD-POOL + ;

\ FD-ALLOC ( -- fdesc | 0 )  allocate a pool slot, return fdesc or 0
: FD-ALLOC  ( -- fdesc | 0 )
    FD-MAX 0 DO
        I FD-SLOT @ 0= IF          \ in_use == 0?
            -1 I FD-SLOT !          \ mark in-use
            I FD-SLOT 8 +           \ fdesc = slot + 8
            UNLOOP EXIT
        THEN
    LOOP
    0 ;                              \ pool exhausted

\ FCLOSE ( fdesc -- )  release FD back to pool
\   Initially a simple free; redefined after FFLUSH to auto-flush.
: (FCLOSE-NOFS)  ( fdesc -- )
    DUP 0= IF DROP EXIT THEN
    8 -                              \ back to slot header
    0 SWAP ! ;                       \ clear in_use flag

DEFER FCLOSE
' (FCLOSE-NOFS) IS FCLOSE

\ FD-FILL ( fdesc slot -- )  populate fdesc fields from dir slot
: FD-FILL  ( fdesc slot -- )
    >R
    R@ DIRENT DE.SEC       OVER !          \ +0 start_sector
    R@ DIRENT DE.COUNT     OVER 8 + !     \ +8 max_sectors
    R@ DIRENT DE.USED      OVER 16 + !    \ +16 used_bytes
    0                      OVER 24 + !    \ +24 cursor = 0
    R@                     OVER 32 + !    \ +32 dir_slot
    R@ DIRENT DE.EXT1-SEC  OVER 40 + !    \ +40 ext1_start
    R> DIRENT DE.EXT1-CNT  SWAP 48 + ! ;  \ +48 ext1_count

\ ── OPEN — open a file by name ───────────────────────────────────────

VARIABLE OP-SLOT

: (OPEN)  ( "name" -- fdesc | 0 )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR 0 EXIT THEN
    PARSE-NAME
    FIND-BY-NAME OP-SLOT !
    OP-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR 0 EXIT
    THEN
    FD-ALLOC DUP 0= IF
        ."  No free FD slots" CR EXIT
    THEN
    DUP OP-SLOT @ FD-FILL ;

DEFER OPEN
' (OPEN) IS OPEN

\ F.SLOT ( fdesc -- n ) directory slot index (for OPEN'd files)
: F.SLOT  ( fdesc -- n )  32 + @ ;

\ FFLUSH ( fdesc -- ) write metadata back to directory on disk
: FFLUSH  ( fdesc -- )
    FS-OK @ 0= IF DROP ."  FS not loaded" CR EXIT THEN
    DUP F.USED
    OVER F.SLOT DIRENT 28 + L!      \ update used_bytes in dir cache
    DROP
    FS-SYNC ;

\ Now that FFLUSH exists, upgrade FCLOSE to auto-flush.
: (FCLOSE)  ( fdesc -- )
    DUP 0= IF DROP EXIT THEN
    FS-OK @ IF DUP FFLUSH THEN      \ persist used_bytes before release
    8 -                              \ back to slot header
    0 SWAP ! ;                       \ clear in_use flag
' (FCLOSE) IS FCLOSE

