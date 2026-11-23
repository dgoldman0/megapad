
\ ── .FTYPE — print file type name ───────────────────────────────────

: .FTYPE  ( type -- )
    DUP 0 = IF DROP ."  free"   EXIT THEN
    DUP 1 = IF DROP ."  raw"    EXIT THEN
    DUP 2 = IF DROP ."  text"   EXIT THEN
    DUP 3 = IF DROP ."  forth"  EXIT THEN
    DUP 4 = IF DROP ."  doc"    EXIT THEN
    DUP 5 = IF DROP ."  data"   EXIT THEN
    DUP 6 = IF DROP ."  tut"    EXIT THEN
    DUP 7 = IF DROP ."  bdl"    EXIT THEN
    DUP 8 = IF DROP ."  dir"    EXIT THEN
    DUP 9 = IF DROP ."  stream" EXIT THEN
    DUP 10 = IF DROP ."  link"  EXIT THEN
    ."  ?" . ;

\ ── DIR — list files ─────────────────────────────────────────────────

: DIR  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  --- Directory ---" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                1+
                ."   " I DIRENT .ZSTR
                I DIRENT DE.TYPE 8 = IF ."  /"  THEN
                ."    " I DIRENT DE.USED . ."  B"
                ."    " I DIRENT DE.TYPE .FTYPE
                CR
            THEN
        THEN
    LOOP
    DUP . ."  file(s), "
    0  FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes free)" CR
    DROP ;

\ CATALOG ( -- ) detailed directory listing
: CATALOG  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Name                     Bytes     Secs  Type  Flg" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                1+
                ."   " I DIRENT .ZSTR
                ."   " I DIRENT DE.USED .
                ."   " I DIRENT DE.COUNT .
                ."   " I DIRENT DE.TYPE .
                ."   " I DIRENT DE.FLAGS .
                CR
            THEN
        THEN
    LOOP
    ."  (" . ."  files, "
    0 FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    . ."  free sectors)" CR ;
