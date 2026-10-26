
\ ── FIND-BY-NAME — shared directory lookup ───────────────────────────
\ Searches directory for an entry whose first 24 bytes match NAMEBUF
\ and whose parent matches CWD.
\ Returns slot index or -1 if not found.  Caller must call PARSE-NAME first.

: FIND-BY-NAME  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                I DIRENT NAMEBUF 24 SAMESTR? IF
                    DROP I LEAVE
                THEN
            THEN
        THEN
    LOOP ;

\ ── TICKS@ — epoch seconds for mtime ─────────────────────────────────
: TICKS@  ( -- u32 )  EPOCH@ 1000 / ;

\ ── MKFILE — create a new file ───────────────────────────────────────

VARIABLE MK-NSEC
VARIABLE MK-TYPE
VARIABLE MK-SLOT
VARIABLE MK-START

: MKFILE  ( nsectors type "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR 2DROP EXIT THEN
    MK-TYPE !  MK-NSEC !
    PARSE-NAME
    \ Check for duplicate name
    FIND-BY-NAME -1 <> IF
        ."  File exists: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Find empty directory slot
    FIND-FREE-SLOT MK-SLOT !
    MK-SLOT @ -1 = IF ."  Directory full" CR EXIT THEN
    \ Find contiguous free sectors
    MK-NSEC @ FIND-FREE MK-START !
    MK-START @ -1 = IF
        ."  No space: need " MK-NSEC @ .
        ."  sectors, "
        0 FS-TOTAL @ FS-DSTART DO
            I BIT-FREE? IF 1+ THEN
        LOOP
        . ."  free" CR EXIT
    THEN
    \ Allocate sectors in bitmap
    MK-NSEC @ 0 DO
        MK-START @ I + BIT-SET
    LOOP
    \ Build directory entry
    MK-SLOT @ DIRENT                 ( de )
    DUP FS-ENTRY-SIZE 0 FILL        \ zero slot
    DUP NAMEBUF SWAP 24 CMOVE       \ copy name (24 bytes)
    DUP MK-START @ SWAP 24 + W!     \ start sector
    DUP MK-NSEC  @ SWAP 26 + W!     \ sector count
    DUP 0          SWAP 28 + L!     \ used_bytes = 0
    DUP MK-TYPE  @ SWAP 32 + C!     \ type
    DUP CWD @     SWAP 34 + C!     \ parent = CWD
    DUP TICKS@    SWAP 36 + L!     \ mtime = current time
    DROP
    FS-SYNC
    ."  Created: " NAMEBUF .ZSTR
    ."  (" MK-NSEC @ . ."  sectors at " MK-START @ . ."  )" CR ;

\ ── RMFILE — delete a file ───────────────────────────────────────────

VARIABLE RM-SLOT

: RMFILE  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME RM-SLOT !
    RM-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Free bitmap sectors (primary extent)
    RM-SLOT @ DIRENT DE.COUNT
    RM-SLOT @ DIRENT DE.SEC
    SWAP 0 DO                        ( start )
        DUP I + BIT-CLR
    LOOP DROP
    \ Free second extent if present
    RM-SLOT @ DIRENT DE.EXT1-CNT DUP 0<> IF
        RM-SLOT @ DIRENT DE.EXT1-SEC
        SWAP 0 DO
            DUP I + BIT-CLR
        LOOP DROP
    ELSE DROP THEN
    \ Clear directory entry
    RM-SLOT @ DIRENT FS-ENTRY-SIZE 0 FILL
    FS-SYNC
    ."  Deleted: " NAMEBUF .ZSTR CR ;

\ ── RENAME — rename a file ───────────────────────────────────────────

VARIABLE RN-SLOT

: RENAME  ( "oldname" "newname" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    \ Look up old name
    PARSE-NAME
    FIND-BY-NAME RN-SLOT !
    RN-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Parse new name
    PARSE-NAME
    \ Check new name doesn't already exist
    FIND-BY-NAME -1 <> IF
        ."  Name taken: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Overwrite name in directory entry
    RN-SLOT @ DIRENT 24 0 FILL       \ zero old name
    NAMEBUF RN-SLOT @ DIRENT 24 CMOVE
    FS-SYNC
    ."  Renamed to: " NAMEBUF .ZSTR CR ;
