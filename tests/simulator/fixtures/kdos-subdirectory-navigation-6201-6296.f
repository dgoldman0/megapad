\ =====================================================================
\  §7.6.2  Subdirectory Navigation
\ =====================================================================
\  Parent-byte subdirectory model — each directory entry has a 1-byte
\  parent field (slot index 0-127, or 255 for root).

\ PWD ( -- ) print current working directory path
CREATE _PWD-STK 64 ALLOT
: PWD  ( -- )
    CWD @ 255 = IF ."  /" CR EXIT THEN
    \ Walk parent chain, collect up to 8 levels
    0  CWD @                             ( depth slot )
    BEGIN DUP 255 <> WHILE
        SWAP DUP 8 < IF
            2DUP CELLS _PWD-STK + !      \ save slot
            1+
        THEN SWAP
        DIRENT DE.PARENT
    REPEAT DROP                          ( depth )
    ."  /"
    DUP 0 DO
        DUP 1- I - CELLS _PWD-STK + @   ( slot )
        DIRENT .ZSTR
        47 EMIT                          \ '/'
    LOOP DROP CR ;

\ CD ( "name" -- ) change current directory
: CD  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    \ Handle ".." — go to parent of CWD
    NAMEBUF C@ 46 = NAMEBUF 1+ C@ 46 = AND NAMEBUF 2 + C@ 0= AND IF
        CWD @ 255 = IF EXIT THEN        \ already at root
        CWD @ DIRENT DE.PARENT CWD !
        EXIT
    THEN
    \ Handle "/" — go to root
    NAMEBUF C@ 47 = NAMEBUF 1+ C@ 0= AND IF
        255 CWD ! EXIT
    THEN
    \ Look up directory entry in CWD
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DUP DIRENT DE.TYPE 8 <> IF
        DROP ."  Not a directory: " NAMEBUF .ZSTR CR EXIT
    THEN
    CWD ! ;

\ MKDIR ( "name" -- ) create a subdirectory entry
: MKDIR  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME -1 <> IF
        ."  Already exists: " NAMEBUF .ZSTR CR EXIT
    THEN
    FIND-FREE-SLOT DUP -1 = IF
        DROP ."  Directory full" CR EXIT
    THEN                                 ( slot )
    DIRENT                               ( de )
    DUP FS-ENTRY-SIZE 0 FILL
    DUP NAMEBUF SWAP 24 CMOVE           \ name
    DUP 8 SWAP 32 + C!                  \ type = FTYPE_DIR
    DUP CWD @ SWAP 34 + C!              \ parent = CWD
    TICKS@ SWAP 36 + L!                 \ mtime
    FS-SYNC
    ."  Created dir: " NAMEBUF .ZSTR CR ;

\ RMDIR ( "name" -- ) remove an empty subdirectory
: RMDIR  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN                                 ( slot )
    DUP DIRENT DE.TYPE 8 <> IF
        DROP ."  Not a directory" CR EXIT
    THEN
    \ Check directory is empty (no children with this parent)
    DUP
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT OVER = IF
                DROP ."  Directory not empty" CR
                UNLOOP EXIT
            THEN
        THEN
    LOOP DROP
    \ Clear entry
    DIRENT FS-ENTRY-SIZE 0 FILL
    FS-SYNC
    ."  Removed dir: " NAMEBUF .ZSTR CR ;

