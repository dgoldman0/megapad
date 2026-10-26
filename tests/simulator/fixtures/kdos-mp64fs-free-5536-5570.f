
\ ── FS-LARGEST-FREE — largest contiguous free run in bitmap ──────────

VARIABLE LF-BEST
VARIABLE LF-RUN

: FS-LARGEST-FREE  ( -- n )
    0 LF-BEST !  0 LF-RUN !
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF
            1 LF-RUN +!
            LF-RUN @ LF-BEST @ > IF LF-RUN @ LF-BEST ! THEN
        ELSE
            0 LF-RUN !
        THEN
    LOOP
    LF-BEST @ ;

\ ── FS-FREE — report free disk space ────────────────────────────────

: FS-FREE  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    0   \ free sector count
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes)" CR
    ."  Largest contiguous: " FS-LARGEST-FREE . ."  sectors" CR
    \ Count used files
    0  FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF 1+ THEN
    LOOP
    . ."  files, " FS-MAX-FILES . ."  max" CR ;
