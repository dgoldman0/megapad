
\ ── CAT — print file contents to terminal ────────────────────────────

VARIABLE CAT-SLOT

: CAT  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME CAT-SLOT !
    CAT-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    CAT-SLOT @ DIRENT DE.USED DUP 0= IF
        DROP ."  (empty file)" CR EXIT
    THEN                                 ( used_bytes )
    \ Read file sectors into HERE (temporary)
    HERE CAT-SLOT @ DIRENT DE.SEC
    CAT-SLOT @ DIRENT DE.COUNT _DISK-READ
    \ Print used_bytes characters from HERE
    HERE SWAP                            ( addr count )
    0 DO
        DUP I + C@ DUP 10 = IF
            DROP CR
        ELSE
            EMIT
        THEN
    LOOP DROP ;
