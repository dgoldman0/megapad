
\ =====================================================================
\  §7.5  File Abstraction
\ =====================================================================
\
\  A FILE is a simple contiguous region on disk.
\
\  FILE descriptor (4 cells = 32 bytes):
\    +0   start_sector   first sector on disk
\    +8   max_sectors    allocated size in sectors
\    +16  used_bytes     bytes actually written
\    +24  cursor         current read/write position (byte offset)
\
\  Uses a scratch buffer for partial-sector I/O.
\  Files are NOT tile-aligned; they use the DMA to a RAM scratch area.

\ -- Registry (up to 8 files) --
VARIABLE FILE-COUNT
0 FILE-COUNT !
VARIABLE FILE-TABLE  7 CELLS ALLOT

\ -- Scratch buffer for sector I/O (one sector) --
VARIABLE FSCRATCH  SECTOR 1- ALLOT

\ -- Field accessors --
: F.START  ( fdesc -- sec )    @ ;
: F.MAX    ( fdesc -- n )      8 + @ ;
: F.USED   ( fdesc -- n )     16 + @ ;
: F.CURSOR ( fdesc -- n )     24 + @ ;

\ -- Internal temp --
VARIABLE FDESC

\ FILE ( start_sector max_sectors "name" -- )
\   Create a file descriptor backed by disk sectors.
: FILE
    HERE FDESC !
    SWAP ,                    \ +0  start_sector
    DUP ,                     \ +8  max_sectors  (keep copy)
    0 ,                       \ +16 used_bytes = 0
    0 ,                       \ +24 cursor = 0
    DROP                      \ drop extra max_sectors copy
    \ register
    FILE-COUNT @ 8 < IF
        FDESC @ FILE-COUNT @ CELLS FILE-TABLE + !
        FILE-COUNT @ 1+ FILE-COUNT !
    THEN
    FDESC @ CONSTANT ;

\ FSEEK ( pos fdesc -- ) set cursor position
: FSEEK  ( pos fdesc -- )  24 + ! ;

\ FREWIND ( fdesc -- ) reset cursor to 0
: FREWIND  ( fdesc -- )  0 SWAP FSEEK ;

\ FSIZE ( fdesc -- n ) return used bytes
: FSIZE  ( fdesc -- n )  F.USED ;

\ FTRUNCATE ( n fdesc -- ) set used_bytes to n, clamp to capacity
\   Resets cursor to min(cursor, n).  Does NOT zero freed bytes.
VARIABLE FT-N
: FTRUNCATE  ( n fdesc -- )
    SWAP  OVER F.MAX SECTOR *  MIN  FT-N !  ( fdesc )
    FT-N @  OVER 16 + !               \ used_bytes = n'
    DUP F.CURSOR  FT-N @  MIN         ( fdesc cursor' )
    SWAP 24 + ! ;                      \ cursor = min(old_cursor, n')

\ FWRITE ( addr len fdesc -- ) write len bytes from addr at cursor
\   Handles byte-unaligned cursors via read-modify-write with FSCRATCH.
\   Head/tail partial sectors are read into FSCRATCH, patched, and
\   written back.  Full middle sectors DMA directly from user buffer.
\   Advances cursor by len.  Updates used_bytes.
VARIABLE FW-FD
VARIABLE FW-ADDR
VARIABLE FW-LEN
VARIABLE FW-REM     \ remaining bytes to write
VARIABLE FW-POS     \ current byte position in file

\ FW-DISK-SEC ( -- sec ) absolute disk sector for current FW-POS
: FW-DISK-SEC  FW-POS @ SECTOR / FW-FD @ F.START + ;

VARIABLE FW-CHUNK   \ temp: byte count for head copy
\ FW-HEAD ( -- ) read-modify-write leading partial sector
: FW-HEAD  ( -- )
    FW-POS @ SECTOR MOD  DUP 0= IF DROP EXIT THEN  ( off )
    FSCRATCH FW-DISK-SEC 1 _DISK-READ
    SECTOR OVER -  FW-REM @ MIN  FW-CHUNK !  ( off )
    FW-ADDR @  SWAP FSCRATCH +  FW-CHUNK @  CMOVE
    FSCRATCH FW-DISK-SEC 1 _DISK-WRITE
    FW-CHUNK @ DUP FW-ADDR +!  DUP FW-POS +!  NEGATE FW-REM +! ;

\ FW-FULL ( -- ) DMA full sectors directly from user buffer
: FW-FULL  ( -- )
    FW-REM @ SECTOR / DUP 0= IF DROP EXIT THEN
    DUP >R
    FW-ADDR @ FW-DISK-SEC ROT _DISK-WRITE
    R> SECTOR *
    DUP FW-ADDR +!
    DUP FW-POS +!
    NEGATE FW-REM +! ;

\ FW-TAIL ( -- ) read-modify-write trailing partial sector
: FW-TAIL  ( -- )
    FW-REM @ 0= IF EXIT THEN
    FSCRATCH FW-DISK-SEC 1 _DISK-READ
    FW-ADDR @  FSCRATCH  FW-REM @  CMOVE
    FSCRATCH FW-DISK-SEC 1 _DISK-WRITE ;

: FWRITE  ( addr len fdesc -- )
    FW-FD !  FW-LEN !  FW-ADDR !
    \ Bounds check: cursor + len <= max_sectors * 512
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ F.MAX SECTOR * > IF
        ."  FWRITE: out of space" CR EXIT
    THEN
    FW-LEN @ 0= IF EXIT THEN
    FW-FD @ F.CURSOR FW-POS !
    FW-LEN @ FW-REM !
    FW-HEAD  FW-FULL  FW-TAIL
    \ Advance cursor by len
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ 24 + !
    \ Update used_bytes = max(used, cursor)
    FW-FD @ F.CURSOR FW-FD @ F.USED MAX
    FW-FD @ 16 + ! ;

\ FREAD ( addr len fdesc -- actual ) read up to len bytes at cursor
\   Handles byte-unaligned cursors via FSCRATCH for partial sectors.
\   Head/tail partial sectors are read into FSCRATCH and the relevant
\   bytes copied to the user buffer.  Full middle sectors DMA directly.
\   Advances cursor by actual bytes read.
VARIABLE FR-FD
VARIABLE FR-ADDR
VARIABLE FR-LEN
VARIABLE FR-REM     \ remaining bytes to read
VARIABLE FR-POS     \ current byte position in file

\ FR-DISK-SEC ( -- sec ) absolute disk sector for current FR-POS
: FR-DISK-SEC  FR-POS @ SECTOR / FR-FD @ F.START + ;

\ FR-HEAD ( -- ) copy leading partial sector via scratch buffer
VARIABLE FR-CHUNK   \ temp: byte count for head copy
: FR-HEAD  ( -- )
    FR-POS @ SECTOR MOD  DUP 0= IF DROP EXIT THEN  ( off )
    FSCRATCH FR-DISK-SEC 1 _DISK-READ
    SECTOR OVER -  FR-REM @ MIN  FR-CHUNK !  ( off )
    FSCRATCH +  FR-ADDR @  FR-CHUNK @  CMOVE
    FR-CHUNK @ DUP FR-ADDR +!  DUP FR-POS +!  NEGATE FR-REM +! ;

\ FR-FULL ( -- ) DMA full sectors directly into user buffer
: FR-FULL  ( -- )
    FR-REM @ SECTOR / DUP 0= IF DROP EXIT THEN
    DUP >R
    FR-ADDR @ FR-DISK-SEC ROT _DISK-READ
    R> SECTOR *
    DUP FR-ADDR +!
    DUP FR-POS +!
    NEGATE FR-REM +! ;

\ FR-TAIL ( -- ) copy trailing partial sector via scratch buffer
: FR-TAIL  ( -- )
    FR-REM @ 0= IF EXIT THEN
    FSCRATCH FR-DISK-SEC 1 _DISK-READ
    FSCRATCH  FR-ADDR @  FR-REM @  CMOVE ;

: FREAD  ( addr len fdesc -- actual )
    FR-FD !  FR-LEN !  FR-ADDR !
    \ Guard: cursor already at or past file end → return 0
    \ (MIN is unsigned; without this guard the subtraction below
    \  wraps to a huge value and FREAD loops forever.)
    FR-FD @ F.CURSOR  FR-FD @ F.USED  < 0= IF 0 EXIT THEN
    \ Clamp len to available bytes (safe: cursor < used)
    FR-FD @ F.USED FR-FD @ F.CURSOR -
    FR-LEN @ MIN FR-LEN !
    FR-LEN @ 0= IF 0 EXIT THEN
    FR-FD @ F.CURSOR FR-POS !
    FR-LEN @ FR-REM !
    FR-HEAD  FR-FULL  FR-TAIL
    \ Advance cursor by actual bytes read
    FR-FD @ F.CURSOR FR-LEN @ +
    FR-FD @ 24 + !
    FR-LEN @ ;

\ F.INFO ( fdesc -- ) print file descriptor
: F.INFO  ( fdesc -- )
    ."  [file"
    DUP ."   sec=" F.START .
    DUP ."   max=" F.MAX .
    DUP ."   used=" F.USED .
    ."   cur=" F.CURSOR . ."  ]" CR ;

\ -- List all registered (legacy) files --
: FILES  ( -- )
    ."  --- Files (" FILE-COUNT @ . ."  ) ---" CR
    FILE-COUNT @ DUP IF
        0 DO
            I . ."  : "
            I CELLS FILE-TABLE + @ F.INFO
        LOOP
    ELSE DROP THEN ;
