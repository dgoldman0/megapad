
\ ── SAVE-BUFFER — save buffer data to a named file ──────────────────

VARIABLE SB-SLOT
VARIABLE SB-DESC

: SAVE-BUFFER  ( buf "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF DROP ."  No filesystem" CR EXIT THEN
    SB-DESC !
    PARSE-NAME
    FIND-BY-NAME SB-SLOT !
    SB-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR
        ."   (create with MKFILE first)" CR EXIT
    THEN
    \ Write buffer data into file's sectors
    SB-DESC @ B.DATA SB-SLOT @ DIRENT DE.SEC
    SB-SLOT @ DIRENT DE.COUNT _DISK-WRITE
    \ Update used_bytes in directory
    SB-DESC @ B.LEN
    SB-SLOT @ DIRENT 28 + L!
    FS-SYNC
    ."  Saved " SB-DESC @ B.LEN . ."  bytes to " NAMEBUF .ZSTR CR ;

\ ── LOAD-BUFFER — load file data into a buffer ──────────────────────

VARIABLE LB-SLOT
VARIABLE LB-DESC

: LOAD-BUFFER  ( buf "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF DROP ."  No filesystem" CR EXIT THEN
    LB-DESC !
    PARSE-NAME
    FIND-BY-NAME LB-SLOT !
    LB-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Read file data into buffer
    LB-DESC @ B.DATA LB-SLOT @ DIRENT DE.SEC
    LB-SLOT @ DIRENT DE.COUNT _DISK-READ
    ."  Loaded " LB-SLOT @ DIRENT 28 + L@ . ."  bytes from " NAMEBUF .ZSTR CR ;
