\ =====================================================================
\  §7.7  Documentation Browser
\ =====================================================================
\
\  Built-in documentation and interactive tutorials stored as MP64FS
\  files.  DOC pages through a documentation file; TUTORIAL walks
\  through a lesson; DESCRIBE shows a topic by name.
\  TOPICS / LESSONS list available content.
\
\  File types:
\    type 4 (doc)      — documentation files
\    type 6 (tutorial) — interactive tutorials

\ -- Constants --
4 CONSTANT FTYPE-DOC
6 CONSTANT FTYPE-TUT

\ -- Read buffer for paging (one sector at a time) --
CREATE DOC-BUF SECTOR ALLOT
VARIABLE DOC-LINES              \ newline counter for pagination
20 CONSTANT PAGE-LINES          \ lines per page before pause

\ .DOC-CHUNK ( addr len -- )  print bytes, pausing every PAGE-LINES
: .DOC-CHUNK  ( addr len -- )
    OVER + SWAP DO
        I C@ DUP 10 = IF
            DROP CR
            1 DOC-LINES +!
            DOC-LINES @ PAGE-LINES >= IF
                DIM ."  --- more ---" RESET-COLOR
                KEY DROP CR
                DOC-LINES OFF
            THEN
        ELSE
            EMIT
        THEN
    LOOP ;

\ SHOW-FILE ( fdesc -- )  read and display entire file page by page
: SHOW-FILE  ( fdesc -- )
    DOC-LINES OFF
    BEGIN
        DOC-BUF SECTOR ROT        ( buf len fd )
        DUP >R FREAD              ( actual  R: fd )
        DUP 0> WHILE
        DOC-BUF SWAP .DOC-CHUNK
        R>
    REPEAT
    DROP R> DROP ;

\ TOPICS ( -- )  list available documentation files (type=4)
: TOPICS  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Available topics:" CR
    0                                        \ count
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                1+
                ."    " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."    (none)" CR THEN
    ."  (" . ."  topics)" CR ;

\ LESSONS ( -- )  list available tutorials (type=6)
: LESSONS  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Available lessons:" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-TUT = IF
                1+
                ."    " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."    (none)" CR THEN
    ."  (" . ."  lessons)" CR ;

\ DOC ( "name" -- )  page through a documentation file
: DOC  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

\ TUTORIAL ( "name" -- )  walk through a tutorial file
: TUTORIAL  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

\ OPEN-BY-SLOT ( slot -- fdesc | 0 )  open a file by directory slot
\   Like OPEN but takes a slot index instead of parsing a name.
\   Uses the FD pool; caller should FCLOSE when done.
: OPEN-BY-SLOT  ( slot -- fdesc | 0 )
    DUP DIRENT C@ 0= IF DROP 0 EXIT THEN
    FD-ALLOC DUP 0= IF
        ."  No free FD slots" CR NIP EXIT
    THEN
    DUP ROT FD-FILL ;

\ DESCRIBE ( "word" -- )  look up a word in the documentation
\   Tries to open a doc file matching the name.  If no exact match,
\   suggests using TOPICS.
: DESCRIBE  ( "word" -- )
    PARSE-NAME PN-LEN @ 0= IF ."  Usage: DESCRIBE <word>" CR EXIT THEN
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    \ Search for a doc file whose name matches NAMEBUF
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                I DIRENT NAMEBUF 24 SAMESTR? IF
                    DROP I LEAVE
                THEN
            THEN
        THEN
    LOOP
    DUP -1 = IF
        DROP
        ."  No doc for: " NAMEBUF .ZSTR CR
        ."  Use TOPICS to list available documentation."  CR
        EXIT
    THEN
    OPEN-BY-SLOT DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

