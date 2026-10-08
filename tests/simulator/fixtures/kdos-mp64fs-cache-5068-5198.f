
\ =====================================================================
\  §7.6  MP64FS — On-Disk Named File System
\ =====================================================================
\
\  Disk layout (15..65536 x 512-byte sectors, format marker 1):
\    Sector 0         Superblock (magic "MP64", marker, geometry)
\    Sectors 1..B     Allocation bitmap (one bit per sector)
\    Next 12 sectors  Directory (128 entries x 48 bytes)
\    Remaining        Data area
\  B = ceil(total_sectors / 4096); directory and data starts follow B.
\
\  Directory entry (48 bytes):
\    +0   name[24]       null-terminated (max 23 chars)
\    +24  start_sector[2] u16 LE
\    +26  sec_count[2]   u16 LE
\    +28  used_bytes[4]  u32 LE
\    +32  type[1]        0=free 1=raw 2=text 3=forth 4=doc 5=data
\                        6=tut 7=bundle 8=dir 9=stream 10=link
\    +33  flags[1]       bit0=readonly bit1=system bit2=encrypted bit3=append
\    +34  parent[1]      parent dir slot (0xFF=root)
\    +35  reserved[1]
\    +36  mtime[4]       u32 seconds since boot
\    +40  data_crc32[4]  u32 CRC-32
\    +44  ext1_start[2]  u16 LE second extent start
\    +46  ext1_count[2]  u16 LE second extent sector count
\
\  File descriptor layout (allocated from FD pool, freed by FCLOSE):
\    +0   start_sector   (cell)
\    +8   max_sectors     (cell)
\    +16  used_bytes      (cell)
\    +24  cursor          (cell)
\    +32  dir_slot        (cell) — index into directory cache
\    +40  ext1_start      (cell) — second extent start
\    +48  ext1_count      (cell) — second extent count
\  Pool slot has an in_use flag at fdesc - 8.

\ -- Constants --
128 CONSTANT FS-MAX-FILES
48  CONSTANT FS-ENTRY-SIZE
16  CONSTANT FS-MAX-BMAP-SECTORS
65536 CONSTANT FS-MAX-SECTORS

\ -- RAM caches (loaded from disk by FS-LOAD) --
VARIABLE FS-SUPER  SECTOR 1- ALLOT            \ 512 bytes — superblock
VARIABLE FS-BMAP   SECTOR FS-MAX-BMAP-SECTORS * 1- ALLOT
                                                \ up to 65536 sector bits
VARIABLE FS-DIR    SECTOR 12 * 1- ALLOT       \ 6144 bytes — directory

VARIABLE FS-TOTAL  2048 FS-TOTAL !
VARIABLE FS-BMAP-N 1 FS-BMAP-N !
: FS-DIR-START  ( -- n ) FS-BMAP-N @ 1+ ;
: FS-DSTART     ( -- n ) FS-DIR-START 12 + ;

\ -- Current working directory (parent slot, 0xFF = root) --
VARIABLE CWD   255 CWD !

\ ── Bitmap operations ────────────────────────────────────────────────

\ BIT-MASK ( bitpos -- mask ) compute 1 << bitpos
: BIT-MASK  ( bitpos -- mask )
    DUP 0= IF DROP 1 EXIT THEN
    1 SWAP 0 DO 2* LOOP ;

\ BIT-FREE? ( sector -- flag ) true if sector is unallocated
: BIT-FREE?  ( sector -- flag )
    DUP 8 / FS-BMAP + C@
    SWAP 8 MOD BIT-MASK
    AND 0= ;

\ BIT-SET ( sector -- ) mark sector as allocated
: BIT-SET  ( sector -- )
    DUP 8 / FS-BMAP +
    DUP C@
    ROT 8 MOD BIT-MASK
    OR SWAP C! ;

\ BIT-CLR ( sector -- ) mark sector as free
: BIT-CLR  ( sector -- )
    DUP 8 / FS-BMAP +
    DUP C@
    ROT 8 MOD BIT-MASK
    INVERT AND SWAP C! ;

\ FIND-FREE ( count -- sector | -1 ) find contiguous free run
VARIABLE FF-NEED
VARIABLE FF-START
VARIABLE FF-LEN

: FIND-FREE  ( count -- sector | -1 )
    FF-NEED !
    FS-DSTART FF-START !
    0 FF-LEN !
    -1                                 \ result on stack
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF
            FF-LEN @ 0= IF I FF-START ! THEN
            1 FF-LEN +!
            FF-LEN @ FF-NEED @ >= IF
                DROP FF-START @        \ replace -1 with start
                LEAVE
            THEN
        ELSE
            0 FF-LEN !
        THEN
    LOOP ;

\ ── Directory helpers ────────────────────────────────────────────────

\ DIRENT ( n -- addr ) address of directory entry N in RAM cache
: DIRENT  ( n -- addr )  FS-ENTRY-SIZE * FS-DIR + ;

\ Directory entry field readers
: DE.SEC    ( de -- u16 )   24 + W@ ;
: DE.COUNT  ( de -- u16 )   26 + W@ ;
: DE.USED   ( de -- u32 )   28 + L@ ;
: DE.TYPE   ( de -- u8 )    32 + C@ ;
: DE.FLAGS  ( de -- u8 )    33 + C@ ;
: DE.PARENT ( de -- u8 )    34 + C@ ;
: DE.MTIME  ( de -- u32 )   36 + L@ ;
: DE.CRC    ( de -- u32 )   40 + L@ ;
: DE.EXT1-SEC   ( de -- u16 ) 44 + W@ ;
: DE.EXT1-CNT   ( de -- u16 ) 46 + W@ ;

\ FIND-FREE-SLOT ( -- slot | -1 ) first empty directory slot

: FIND-FREE-SLOT  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0= IF DROP I LEAVE THEN
    LOOP ;
