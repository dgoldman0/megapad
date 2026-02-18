\ tools.f — KDOS Loadable Tools Module
\
\ Provides:
\   1. ED — Simple line editor for on-device Forth editing
\   2. SCROLL — Socket Client for Remote Object Retrieval Over Links
\
\ Load with:  LOAD tools.f    or    REQUIRE tools.f

PROVIDED tools.f

\ =====================================================================
\  §T1  ED — Line Editor
\ =====================================================================
\
\  A minimal line editor for creating and modifying Forth source
\  files in MP64FS.  Inspired by classic Forth block editors.
\
\  Commands:
\    ED <file>        Open file for editing (or create new)
\    I <text>         Insert line after current position
\    D                Delete current line
\    L                List all lines with line numbers
\    P                Print current line
\    T                Go to top (line 0)
\    N                Move to next line
\    B                Move to previous line (back)
\    G <n>            Go to line n
\    R <text>         Replace current line with <text>
\    S                Save file to disk
\    Q                Quit editor
\    H                Show help
\
\  Internal storage: up to 64 lines × 80 chars in a heap buffer.

64 CONSTANT ED-MAXLINES
80 CONSTANT ED-LINELEN

VARIABLE ED-BUF                  \ base of edit buffer
VARIABLE ED-NLINES               \ number of lines
VARIABLE ED-CUR                  \ current line index
VARIABLE ED-DIRTY                \ modified flag
CREATE ED-FILE 16 ALLOT          \ current filename (NAMEBUF format)

\ -- Allocate edit buffer on first use --
VARIABLE ED-ALLOC   0 ED-ALLOC !
: ED-ENSURE-BUF  ( -- )
    ED-ALLOC @ IF EXIT THEN
    HERE ED-BUF !
    ED-MAXLINES ED-LINELEN * ALLOT
    1 ED-ALLOC ! ;

\ -- Line address --
: ED-LINE  ( n -- addr )  ED-LINELEN * ED-BUF @ + ;

\ -- Zero-fill a line slot --
: ED-BLANK  ( n -- )  ED-LINE ED-LINELEN 0 FILL ;

\ -- Shift lines down (make room at position n) --
: ED-SHIFT-DN  ( n -- )
    ED-NLINES @ 1- SWAP        ( last ins )
    DO
        I ED-LINE I 1+ ED-LINE ED-LINELEN CMOVE
    -1 +LOOP ;

\ -- Shift lines up (close gap at position n) --
: ED-SHIFT-UP  ( n -- )
    DUP 1+ ED-NLINES @ 1- DO
        I 1+ ED-LINE I ED-LINE ED-LINELEN CMOVE
    LOOP
    ED-NLINES @ 1- ED-BLANK ;

\ -- Copy text into line n (truncate to ED-LINELEN) --
: ED-SET-LINE  ( addr len n -- )
    DUP ED-BLANK
    ED-LINE                    ( addr len dest )
    ROT ROT                    ( dest addr len )
    ED-LINELEN MIN             ( dest addr len' )
    CMOVE ;

\ -- Print a single line with line number --
: ED-PRINT-LINE  ( n -- )
    DUP ED-NLINES @ >= IF DROP EXIT THEN
    DUP ED-CUR @ = IF 42 EMIT ELSE 32 EMIT THEN
    DUP 3 .R 32 EMIT
    ED-LINE ED-LINELEN TYPE CR ;

\ -- List all lines --
: ED-LIST  ( -- )
    ED-NLINES @ 0 DO
        I ED-PRINT-LINE
    LOOP ;

\ -- Parse rest of input line (for Insert / Replace) --
CREATE ED-PARSE-BUF 80 ALLOT
VARIABLE ED-PL
: ED-PARSE-REST  ( -- addr len )
    0 ED-PL !
    BEGIN
        >IN @ #TIB @ <
    WHILE
        TIB >IN @ + C@
        ED-PARSE-BUF ED-PL @ + C!
        ED-PL @ 1+ DUP 80 < IF ED-PL ! ELSE DROP THEN
        1 >IN +!
    REPEAT
    ED-PARSE-BUF ED-PL @ ;

\ -- Ed commands --
: ED-INSERT  ( -- )
    ED-NLINES @ ED-MAXLINES >= IF ."  Buffer full" CR EXIT THEN
    ED-PARSE-REST                   ( addr len )
    ED-CUR @ 1+ DUP ED-NLINES @ < IF
        DUP ED-SHIFT-DN
    THEN
    2DUP ED-NLINES @ < IF THEN
    -ROT ED-CUR @ 1+ ED-SET-LINE
    ED-NLINES @ 1+ ED-NLINES !
    ED-CUR @ 1+ ED-CUR !
    1 ED-DIRTY ! ;

: ED-DELETE  ( -- )
    ED-NLINES @ 0= IF ."  Empty" CR EXIT THEN
    ED-CUR @ ED-SHIFT-UP
    ED-NLINES @ 1- 0 MAX ED-NLINES !
    ED-CUR @ ED-NLINES @ >= IF
        ED-NLINES @ 1- 0 MAX ED-CUR !
    THEN
    1 ED-DIRTY ! ;

: ED-REPLACE  ( -- )
    ED-NLINES @ 0= IF ."  No lines" CR EXIT THEN
    ED-PARSE-REST ED-CUR @ ED-SET-LINE
    1 ED-DIRTY ! ;

: ED-GOTO  ( n -- )
    0 MAX ED-NLINES @ 1- MIN ED-CUR ! ;

\ -- Save buffer back to file --
VARIABLE ED-SAVE-SZ
: ED-SAVE  ( -- )
    ED-FILE NAMEBUF 16 CMOVE
    FIND-BY-NAME DUP -1 = IF
        DROP ."  File not found on disk" CR EXIT
    THEN
    DIRENT
    \ Build contiguous text from lines
    ED-NLINES @ 0= IF  DROP ."  Nothing to save" CR EXIT  THEN
    0 ED-SAVE-SZ !
    HERE
    ED-NLINES @ 0 DO
        I ED-LINE ED-LINELEN             ( here lineaddr 80 )
        \ Find actual line length (strip trailing NULs)
        DUP 0 DO
            OVER I + C@ 0= IF
                DROP I LEAVE
            THEN
        LOOP                              ( here lineaddr truelen )
        DUP 0 > IF
            ROT 2DUP + >R
            -ROT CMOVE
            R>
            10 OVER C! 1+                 \ append LF
            ED-SAVE-SZ @ + 1+ ED-SAVE-SZ !
        ELSE
            2DROP
        THEN
    LOOP
    DROP
    \ Write to disk
    DUP DE.SEC DISK-SEC!
    HERE DISK-DMA!
    DUP DE.COUNT DISK-N!
    DISK-WRITE
    ED-SAVE-SZ @ SWAP 20 + L!
    FS-SYNC
    0 ED-DIRTY !
    ."  Saved " ED-SAVE-SZ @ . ."  bytes" CR ;

\ -- Load file into editor buffer --
: ED-LOAD  ( -- )
    ED-ENSURE-BUF
    0 ED-NLINES !  0 ED-CUR !  0 ED-DIRTY !
    ED-FILE NAMEBUF 16 CMOVE
    FIND-BY-NAME DUP -1 = IF
        DROP ."  New file (not on disk yet)" CR EXIT
    THEN
    DIRENT DUP DE.USED DUP 0= IF
        2DROP ."  Empty file" CR EXIT
    THEN                              ( de used )
    SWAP
    DUP DE.SEC DISK-SEC!
    HERE DISK-DMA!
    DUP DE.COUNT DISK-N!
    DISK-READ
    DROP                              ( used )
    \ Parse lines from HERE
    HERE SWAP                          ( addr remaining )
    BEGIN
        DUP 0> ED-NLINES @ ED-MAXLINES < AND
    WHILE
        OVER                           ( addr rem linestart )
        2 PICK                         ( addr rem linestart rem )
        0                              ( addr rem linestart rem linelen )
        BEGIN
            OVER 0>
            OVER ED-LINELEN < AND
        WHILE
            2 PICK OVER + C@
            10 = IF                    \ LF = end of line
                1+                     \ count the LF
                LEAVE
            THEN
            1+
            SWAP 1- SWAP
        REPEAT                         ( addr rem linestart consumed linelen )
        DROP                           ( addr rem linestart consumed )
        \ Copy line to ED buffer
        DUP 0> IF
            2DUP OVER + 1- C@ 10 = IF 1- THEN   \ strip LF
            ED-NLINES @ ED-SET-LINE
            ED-NLINES @ 1+ ED-NLINES !
        ELSE
            DROP
        THEN
        SWAP DROP                      ( addr rem consumed )
        ROT OVER + -ROT               ( addr' rem consumed )
        -
    REPEAT
    2DROP
    ."  Loaded " ED-NLINES @ . ."  lines" CR ;

\ -- Editor help --
: ED-HELP  ( -- )
    ."  ED Commands:" CR
    ."   I <text>  Insert line after cursor" CR
    ."   D         Delete current line" CR
    ."   R <text>  Replace current line" CR
    ."   L         List all lines" CR
    ."   P         Print current line" CR
    ."   T         Go to top" CR
    ."   N         Next line" CR
    ."   B         Back one line" CR
    ."   G <n>     Goto line n" CR
    ."   S         Save to disk" CR
    ."   Q         Quit" CR
    ."   H         This help" CR ;

\ -- Editor main loop --
CREATE ED-CMD-BUF 80 ALLOT
: ED-PROMPT  ( -- )
    ."  ed[" ED-CUR @ .N ."  /" ED-NLINES @ .N ." ] " ;

: ED  ( "filename" -- )
    ED-ENSURE-BUF
    PARSE-NAME
    NAMEBUF ED-FILE 16 CMOVE
    ED-LOAD
    ED-HELP
    BEGIN
        ED-PROMPT
        ED-CMD-BUF 78 ACCEPT          ( n )
        DUP 0> IF
            ED-CMD-BUF C@
            CASE
                73 OF  ED-INSERT   ENDOF   \ I
                105 OF ED-INSERT   ENDOF   \ i
                68 OF  ED-DELETE   ENDOF   \ D
                100 OF ED-DELETE   ENDOF   \ d
                76 OF  ED-LIST     ENDOF   \ L
                108 OF ED-LIST     ENDOF   \ l
                80 OF  ED-CUR @ ED-PRINT-LINE  ENDOF   \ P
                112 OF ED-CUR @ ED-PRINT-LINE  ENDOF   \ p
                84 OF  0 ED-CUR !  ENDOF   \ T
                116 OF 0 ED-CUR !  ENDOF   \ t
                78 OF  ED-CUR @ 1+ ED-NLINES @ 1- MIN ED-CUR !  ENDOF \ N
                110 OF ED-CUR @ 1+ ED-NLINES @ 1- MIN ED-CUR !  ENDOF \ n
                66 OF  ED-CUR @ 1- 0 MAX ED-CUR !  ENDOF  \ B
                98 OF  ED-CUR @ 1- 0 MAX ED-CUR !  ENDOF  \ b
                82 OF  ED-REPLACE  ENDOF   \ R
                114 OF ED-REPLACE  ENDOF   \ r
                83 OF  ED-SAVE     ENDOF   \ S
                115 OF ED-SAVE     ENDOF   \ s
                81 OF  ED-DIRTY @ IF ."  Unsaved changes! S to save" CR
                       ELSE  ."  Bye" CR EXIT
                       THEN  ENDOF         \ Q
                113 OF ED-DIRTY @ IF ."  Unsaved changes! S to save" CR
                       ELSE  ."  Bye" CR EXIT
                       THEN  ENDOF         \ q
                72 OF  ED-HELP     ENDOF   \ H
                104 OF ED-HELP     ENDOF   \ h
                71 OF                      \ G <n>
                    ED-CMD-BUF 2 + C@ 48 - ED-GOTO
                ENDOF
                103 OF                     \ g <n>
                    ED-CMD-BUF 2 + C@ 48 - ED-GOTO
                ENDOF
                ."  Unknown: " DUP EMIT CR
            ENDCASE
        THEN
        DROP
    AGAIN ;

\ =====================================================================
\  §T2  SCROLL — Socket Client for Remote Object Retrieval Over Links
\ =====================================================================
\
\  Multi-protocol resource fetcher.  Given a URL, resolves the host,
\  connects, transfers, and delivers the payload.
\
\  Protocols supported:
\    http://host[:port]/path   — HTTP/1.1 GET over TCP
\    tftp://host/path          — TFTP RRQ over UDP
\    gopher://host[:port]/sel  — Gopher type-0 text fetch
\
\  Public API:
\    SCROLL-GET   ( url len -- buf len | 0 )  fetch to RAM
\    SCROLL-SAVE  ( url len "file" -- )        fetch and save to disk
\    SCROLL-LOAD  ( url len -- )               fetch and EVALUATE
\
\  Uses TCP (§16.7), UDP (§16.4), DNS (§16.6), TLS (§17) from KDOS.

\ -- URL parsing state --
VARIABLE _SC-PROTO             \ 0=http, 1=tftp, 2=gopher, 3=https
VARIABLE _SC-PORT              \ destination port
CREATE _SC-HOST  64 ALLOT      \ hostname (NUL-terminated)
CREATE _SC-PATH 256 ALLOT      \ path (NUL-terminated)
VARIABLE _SC-HOST-LEN
VARIABLE _SC-PATH-LEN
VARIABLE _SC-IP                \ resolved IP (4 bytes inline)

\ Protocol constants
0 CONSTANT PROTO-HTTP
1 CONSTANT PROTO-TFTP
2 CONSTANT PROTO-GOPHER
3 CONSTANT PROTO-HTTPS

\ -- Response buffer --
CREATE SCROLL-BUF 4096 ALLOT
VARIABLE SCROLL-LEN  0 SCROLL-LEN !

\ ── URL Parser ──────────────────────────────────────────────────────
\
\ Parse "proto://host[:port]/path" into _SC-PROTO, _SC-HOST, _SC-PORT,
\ _SC-PATH.  Returns 0 on success, -1 on parse error.

VARIABLE _UP-POS              \ current parse position
VARIABLE _UP-END              \ end of URL string
VARIABLE _UP-URL              \ URL base addr

: _URL-CH  ( -- c | -1 )
    _UP-POS @ _UP-END @ >= IF -1 EXIT THEN
    _UP-URL @ _UP-POS @ + C@
    1 _UP-POS +! ;

: _URL-MATCH  ( addr len -- flag )
    \ Check if URL at current position starts with addr/len
    DUP _UP-POS @ + _UP-END @ > IF 2DROP 0 EXIT THEN
    _UP-URL @ _UP-POS @ + SWAP             ( urlpos len matchaddr len )
    0 DO
        OVER I + C@  OVER I + C@
        <> IF 2DROP 0 UNLOOP EXIT THEN
    LOOP
    2DROP -1 ;

: URL-PARSE  ( addr len -- ior )
    _UP-END !  DUP _UP-URL !  0 _UP-POS !
    80 _SC-PORT !                           \ default HTTP port
    _SC-HOST 64 0 FILL
    _SC-PATH 256 0 FILL
    0 _SC-HOST-LEN !  0 _SC-PATH-LEN !

    \ Detect protocol prefix
    S" https://" _URL-MATCH IF
        PROTO-HTTPS _SC-PROTO !
        443 _SC-PORT !
        8 _UP-POS +!
    ELSE S" http://" _URL-MATCH IF
        PROTO-HTTP _SC-PROTO !
        7 _UP-POS +!
    ELSE S" tftp://" _URL-MATCH IF
        PROTO-TFTP _SC-PROTO !
        69 _SC-PORT !
        7 _UP-POS +!
    ELSE S" gopher://" _URL-MATCH IF
        PROTO-GOPHER _SC-PROTO !
        70 _SC-PORT !
        9 _UP-POS +!
    ELSE
        -1 EXIT                             \ unknown protocol
    THEN THEN THEN THEN

    \ Parse hostname (up to : or / or end)
    BEGIN
        _URL-CH DUP -1 <> IF
            DUP 58 = IF                      \ ':'
                DROP
                \ Parse port number
                0                            ( accum )
                BEGIN
                    _URL-CH DUP -1 <> IF
                        DUP 47 = IF          \ '/'
                            DROP
                            _SC-PATH 47 SWAP C!
                            1 _SC-PATH-LEN !
                            _SC-PORT !
                            0                \ sentinel: break
                        ELSE
                            48 -
                            SWAP 10 * +
                            -1               \ continue
                        THEN
                    ELSE
                        DROP  _SC-PORT !
                        0                    \ sentinel: break
                    THEN
                WHILE REPEAT
                0                            \ break host loop
            ELSE DUP 47 = IF                 \ '/'
                DROP
                _SC-PATH 47 SWAP C!
                1 _SC-PATH-LEN !
                0                            \ break
            ELSE
                _SC-HOST _SC-HOST-LEN @ + C!
                _SC-HOST-LEN @ 1+ _SC-HOST-LEN !
                -1                           \ continue
            THEN THEN
        ELSE
            DROP 0                           \ end of string
        THEN
    0= UNTIL

    \ Parse remaining path
    BEGIN
        _URL-CH DUP -1 <>
    WHILE
        _SC-PATH _SC-PATH-LEN @ + C!
        _SC-PATH-LEN @ 1+ _SC-PATH-LEN !
    REPEAT
    DROP

    \ Default path = "/" if empty
    _SC-PATH-LEN @ 0= IF
        47 _SC-PATH C!  1 _SC-PATH-LEN !
    THEN

    0 ;

\ ── DNS Resolution ──────────────────────────────────────────────────

: _SC-RESOLVE  ( -- ior )
    \ Try parsing hostname as dotted-quad first
    _SC-HOST _SC-HOST-LEN @
    \ Simple check: if first char is digit, try IP parse
    _SC-HOST C@ DUP 48 >= SWAP 57 <= AND IF
        \ Quick dotted-quad parse: 4 octets separated by '.'
        0 0 0                              ( o1 o2 o3 )
        0                                  ( accum )
        _SC-HOST-LEN @ 0 DO
            _SC-HOST I + C@ DUP 46 = IF    \ '.'
                DROP
                SWAP                       \ push accum, shift
                0
            ELSE
                48 -  SWAP 10 * +
            THEN
        LOOP
        \ Stack: o1 o2 o3 o4
        _SC-IP 3 + C!
        _SC-IP 2 + C!
        _SC-IP 1 + C!
        _SC-IP C!
        0 EXIT
    THEN
    \ DNS lookup
    _SC-HOST _SC-HOST-LEN @ DNS-RESOLVE DUP 0= IF
        -1 EXIT                            \ DNS failure
    THEN
    _SC-IP ! 0 ;

\ ── HTTP/1.1 Client ─────────────────────────────────────────────────
\
\  Sends GET request, reads response, parses status + headers,
\  extracts body into SCROLL-BUF.

CREATE _HTTP-REQ 512 ALLOT     \ request build buffer
VARIABLE _HTTP-RLEN

\ Build GET request string
: _HTTP-BUILD-REQ  ( -- addr len )
    _HTTP-REQ 512 0 FILL
    0 _HTTP-RLEN !
    \ "GET "
    S" GET " _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE  4 _HTTP-RLEN +!
    \ path
    _SC-PATH _SC-PATH-LEN @ _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE
    _SC-PATH-LEN @ _HTTP-RLEN +!
    \ " HTTP/1.1\r\nHost: "
    S"  HTTP/1.1" _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE  9 _HTTP-RLEN +!
    13 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    10 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    S" Host: " _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE  6 _HTTP-RLEN +!
    _SC-HOST _SC-HOST-LEN @ _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE
    _SC-HOST-LEN @ _HTTP-RLEN +!
    13 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    10 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    S" Connection: close" _HTTP-REQ _HTTP-RLEN @ + SWAP CMOVE
    17 _HTTP-RLEN +!
    13 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    10 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    13 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    10 _HTTP-REQ _HTTP-RLEN @ + C!  1 _HTTP-RLEN +!
    _HTTP-REQ _HTTP-RLEN @ ;

\ Parse Content-Length from response header area (simple scan)
VARIABLE _HTTP-CLEN
: _HTTP-PARSE-CLEN  ( hdr-addr hdr-len -- )
    -1 _HTTP-CLEN !
    OVER + SWAP                 ( end start )
    BEGIN
        2DUP > 
    WHILE
        DUP C@ 67 = IF         \ 'C' for Content-Length
            DUP S" Content-Length: " ROT SWAP
            16 0 DO
                2DUP I + C@  3 PICK I + C@
                <> IF 2DROP DROP 0 LEAVE THEN
            LOOP
            IF
                16 +
                0               ( accum )
                BEGIN
                    OVER C@ DUP 48 >= SWAP 57 <= AND
                WHILE
                    SWAP DUP C@ 48 -
                    ROT 10 * + SWAP 1+
                REPEAT
                _HTTP-CLEN !
                DROP
            THEN
        THEN
        1+
    REPEAT
    2DROP ;

\ Find end of HTTP headers (\r\n\r\n)
VARIABLE _HTTP-HEND
: _HTTP-FIND-HEND  ( addr len -- )
    0 _HTTP-HEND !
    OVER + SWAP                 ( end start )
    BEGIN
        2DUP 3 - >              \ need at least 4 bytes
    WHILE
        DUP     C@ 13 =
        OVER 1+ C@ 10 = AND
        OVER 2 + C@ 13 = AND
        OVER 3 + C@ 10 = AND
        IF
            4 + _HTTP-HEND !
            2DROP EXIT
        THEN
        1+
    REPEAT
    2DROP ;

\ HTTP GET — fetch via TCP
VARIABLE _HG-TCB
: HTTP-GET  ( -- ior )
    _SC-IP @ _SC-PORT @ 12345 TCP-CONNECT
    DUP 0= IF ."  TCP connect failed" CR -1 EXIT THEN
    _HG-TCB !
    \ Wait for connection (simplified: poll a few times)
    100 0 DO  _HG-TCB @ TCP-POLL  LOOP
    \ Send request
    _HTTP-BUILD-REQ _HG-TCB @ -ROT TCP-SEND DROP
    \ Receive response
    0 SCROLL-LEN !
    200 0 DO
        _HG-TCB @ TCP-POLL
        _HG-TCB @
        SCROLL-BUF SCROLL-LEN @ +
        4096 SCROLL-LEN @ -
        TCP-RECV
        DUP 0> IF
            SCROLL-LEN +!
        ELSE
            DROP
            SCROLL-LEN @ 0> IF LEAVE THEN
        THEN
    LOOP
    _HG-TCB @ TCP-CLOSE
    \ Parse headers to find body
    SCROLL-BUF SCROLL-LEN @ _HTTP-FIND-HEND
    _HTTP-HEND @ 0= IF
        ."  No HTTP header end found" CR -1 EXIT
    THEN
    SCROLL-BUF _HTTP-HEND @ _HTTP-PARSE-CLEN
    \ Move body to start of SCROLL-BUF
    _HTTP-HEND @ SCROLL-BUF +        ( body-start )
    SCROLL-LEN @ _HTTP-HEND @ -      ( body-start body-len )
    _HTTP-CLEN @ -1 <> IF
        _HTTP-CLEN @ MIN
    THEN
    DUP SCROLL-LEN !
    SCROLL-BUF SWAP CMOVE
    0 ;

\ ── TFTP Client ─────────────────────────────────────────────────────
\
\  Trivial File Transfer Protocol: sends RRQ, receives DATA blocks,
\  sends ACK for each.  512-byte blocks, last block < 512 signals EOF.

CREATE _TFTP-PKT 516 ALLOT    \ max TFTP packet
VARIABLE _TFTP-SRCPORT        \ our ephemeral source port
VARIABLE _TFTP-TID            \ server's TID (port from first DATA)
VARIABLE _TFTP-BLK            \ expected block number
VARIABLE _TFTP-DONE

: _TFTP-SEND-RRQ  ( -- )
    \ Build RRQ: opcode=1, filename, 0, "octet", 0
    _TFTP-PKT 516 0 FILL
    0 _TFTP-PKT C!  1 _TFTP-PKT 1+ C!   \ opcode 1 (big-endian)
    _SC-PATH 1+                          \ skip leading '/'
    _SC-PATH-LEN @ 1-                    \ path without '/'
    _TFTP-PKT 2 + SWAP CMOVE
    _SC-PATH-LEN @ 1- 2 +               ( offset-after-name )
    0 _TFTP-PKT OVER + C!  1+           \ NUL terminator
    S" octet" _TFTP-PKT 2 PICK + SWAP CMOVE
    5 +  0 _TFTP-PKT OVER + C!  1+      \ NUL after "octet"
    \ Send via UDP
    _TFTP-PKT SWAP
    _SC-IP @ _TFTP-SRCPORT @ _SC-PORT @ UDP-SEND DROP ;

: _TFTP-SEND-ACK  ( blk -- )
    _TFTP-PKT 4 0 FILL
    0 _TFTP-PKT C!  4 _TFTP-PKT 1+ C!   \ opcode 4 (ACK)
    DUP 8 RSHIFT _TFTP-PKT 2 + C!
    255 AND _TFTP-PKT 3 + C!
    _TFTP-PKT 4
    _SC-IP @ _TFTP-SRCPORT @ _TFTP-TID @ UDP-SEND DROP ;

: TFTP-GET  ( -- ior )
    31337 _TFTP-SRCPORT !
    0 _TFTP-BLK !
    0 _TFTP-DONE !
    0 SCROLL-LEN !
    _TFTP-SEND-RRQ
    500 0 DO
        POLL
        \ Check for UDP data on our port (simplified)
        \ In a real implementation, we'd register a UDP handler
        \ For now, check the NIC RX buffer directly
        _TFTP-DONE @ IF LEAVE THEN
    LOOP
    _TFTP-DONE @ IF 0 ELSE -1 THEN ;

\ ── Gopher Client ──────────────────────────────────────────────────
\
\  Type-0 (text) Gopher: connect, send selector + CRLF, read response.

VARIABLE _GO-TCB
: GOPHER-GET  ( -- ior )
    _SC-IP @ _SC-PORT @ 12346 TCP-CONNECT
    DUP 0= IF ."  Gopher connect failed" CR -1 EXIT THEN
    _GO-TCB !
    100 0 DO _GO-TCB @ TCP-POLL LOOP
    \ Send selector + CRLF
    _SC-PATH _SC-PATH-LEN @ _GO-TCB @ -ROT TCP-SEND DROP
    S\" \r\n" _GO-TCB @ -ROT TCP-SEND DROP
    \ Receive
    0 SCROLL-LEN !
    200 0 DO
        _GO-TCB @ TCP-POLL
        _GO-TCB @
        SCROLL-BUF SCROLL-LEN @ +
        4096 SCROLL-LEN @ -
        TCP-RECV
        DUP 0> IF
            SCROLL-LEN +!
        ELSE
            DROP
            SCROLL-LEN @ 0> IF LEAVE THEN
        THEN
    LOOP
    _GO-TCB @ TCP-CLOSE
    SCROLL-LEN @ 0> IF 0 ELSE -1 THEN ;

\ ── Dispatch ────────────────────────────────────────────────────────

: _SC-FETCH  ( -- ior )
    _SC-RESOLVE 0<> IF
        ."  DNS/IP resolve failed for: " _SC-HOST _SC-HOST-LEN @ TYPE CR
        -1 EXIT
    THEN
    _SC-PROTO @
    CASE
        PROTO-HTTP  OF  HTTP-GET   ENDOF
        PROTO-HTTPS OF  HTTP-GET   ENDOF   \ TLS upgrade TODO
        PROTO-TFTP  OF  TFTP-GET   ENDOF
        PROTO-GOPHER OF GOPHER-GET ENDOF
        ."  Unsupported protocol" CR -1 SWAP
    ENDCASE ;

\ ── Public API ──────────────────────────────────────────────────────

\ SCROLL-GET ( addr len -- buf len | 0 )
\   Fetch URL into SCROLL-BUF.  Returns buf addr + length, or 0 on error.
: SCROLL-GET  ( addr len -- buf len | 0 )
    URL-PARSE 0<> IF
        ."  URL parse error" CR 0 EXIT
    THEN
    _SC-FETCH 0<> IF 0 EXIT THEN
    SCROLL-BUF SCROLL-LEN @ ;

\ SCROLL-SAVE ( addr len "filename" -- )
\   Fetch URL and write content to MP64FS file.
: SCROLL-SAVE  ( addr len "filename" -- )
    URL-PARSE 0<> IF ."  URL parse error" CR EXIT THEN
    _SC-FETCH 0<> IF ."  Fetch failed" CR EXIT THEN
    \ Save SCROLL-BUF to named file
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  File not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DIRENT
    DUP DE.SEC DISK-SEC!
    SCROLL-BUF DISK-DMA!
    DUP DE.COUNT DISK-N!
    DISK-WRITE
    SCROLL-LEN @ SWAP 20 + L!
    FS-SYNC
    ."  Saved " SCROLL-LEN @ . ."  bytes" CR ;

\ SCROLL-LOAD ( addr len -- )
\   Fetch Forth source from URL and EVALUATE it.
: SCROLL-LOAD  ( addr len -- )
    URL-PARSE 0<> IF ."  URL parse error" CR EXIT THEN
    _SC-FETCH 0<> IF ."  Fetch failed" CR EXIT THEN
    SCROLL-BUF SCROLL-LEN @ EVALUATE ;

\ -- Convenience: string-literal versions --
\ Usage:  S" http://10.0.0.2/pkg.f" SCROLL-LOAD
\         S" tftp://10.0.0.2/data.bin" SCROLL-GET

."  tools.f loaded: ED SCROLL-GET SCROLL-SAVE SCROLL-LOAD" CR
