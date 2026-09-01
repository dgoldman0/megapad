\ =====================================================================
\ §9.5  Screen Definition Language (SDL) — Widget Vocabulary
\ =====================================================================
\
\  Standard building blocks for screen definitions.
\  Each W.xxx word encapsulates a common TUI pattern, making screens
\  declarative.  A future renderer (e.g. web/HTML) can redefine these
\  through the vector table (WVEC) without touching screen definitions.
\
\  Widget vocabulary:
\    W.TITLE     ( addr len -- )           Bold section title
\    W.SECTION   ( addr len -- )           Bold sub-heading
\    W.LINE      ( addr len -- )           Indented text line
\    W.KV        ( n addr len -- )         Key : number
\    W.KV-XT     ( xt addr len -- )        Key : <execute xt>
\    W.FLAG      ( flag addr len -- )      Key : green-ON / dim-OFF
\    W.FLAG-2    ( flag t-a t-n f-a f-n addr len -- )  Key : colored yes/no text
\    W.HBAR      ( -- )                    Horizontal rule
\    W.GAP       ( -- )                    Blank line
\    W.LIST      ( count item-xt -- )      Iterable list (sets SCR-MAX)
\    W.DETAIL    ( count xt -- )           Detail pane for selected item
\    W.HINT      ( addr len -- )           Dim action-hint line
\    W.CUSTOM    ( xt -- )                 Escape hatch: call xt directly

\ ── Renderer vector table ─────────────────────────────────────────
\ 15 entries — each holds an xt dispatched by the corresponding W.xxx.
\ Default = TUI renderer.  Swap for web/HTML by replacing all entries.

15 CONSTANT WVEC-SIZE
CREATE WVEC  WVEC-SIZE CELLS ALLOT
 0 CONSTANT WV-TITLE      1 CONSTANT WV-SECTION
 2 CONSTANT WV-LINE       3 CONSTANT WV-KV
 4 CONSTANT WV-KV-XT      5 CONSTANT WV-FLAG
 6 CONSTANT WV-FLAG-2     7 CONSTANT WV-HBAR
 8 CONSTANT WV-GAP        9 CONSTANT WV-LIST
10 CONSTANT WV-DETAIL    11 CONSTANT WV-HINT
12 CONSTANT WV-CUSTOM    13 CONSTANT WV-NONE
14 CONSTANT WV-INPUT

: WV@  ( idx -- xt )  CELLS WVEC + @ ;
: WV!  ( xt idx -- )  CELLS WVEC + ! ;

\ ── TUI renderer implementation ───────────────────────────────────

: TUI-TITLE  ( addr len -- )
    .LABEL ."   " TYPE ./LABEL CR CR ;

: TUI-SECTION  ( addr len -- )
    CR BOLD ."   " TYPE ." :" RESET-COLOR CR ;

: TUI-LINE  ( addr len -- )
    ."    " TYPE CR ;

: TUI-KV  ( n addr len -- )
    ."    " TYPE ."  : " .N CR ;

: TUI-KV-XT  ( xt addr len -- )
    ."    " TYPE ."  : " EXECUTE CR ;

: TUI-FLAG  ( flag addr len -- )
    ."    " TYPE ."  : "
    IF 2 FG ." ON" ELSE DIM ." OFF" THEN RESET-COLOR CR ;

: TUI-FLAG-2  ( flag true-a true-n false-a false-n addr len -- )
    ."    " TYPE ."  : "
    2>R ROT IF 2R> 2DROP 2 FG ELSE 2R> ROT DROP ROT DROP DIM THEN
    TYPE RESET-COLOR CR ;

: TUI-HBAR  ( -- )  HBAR ;

: TUI-GAP   ( -- )  CR ;

: TUI-LIST  ( count item-xt -- )
    OVER 0= IF 2DROP ."    (none)" CR  0 SCR-MAX ! EXIT THEN
    OVER SCR-MAX !
    SWAP 0 DO
        SCR-SEL @ I = IF 2 FG ."  > " RESET-COLOR ELSE ."    " THEN
        I OVER EXECUTE CR
    LOOP DROP ;

: TUI-DETAIL  ( count xt -- )
    SWAP SCR-SEL @ DUP -1 = IF 2DROP DROP EXIT THEN
    SWAP OVER >= IF 2DROP EXIT THEN
    CR HBAR EXECUTE ;

: TUI-HINT  ( addr len -- )
    DIM ."   " TYPE RESET-COLOR CR ;

: TUI-CUSTOM  ( xt -- )  EXECUTE ;

\ W.INPUT ( buf maxlen prompt-addr prompt-len -- actual-len )
\   Display prompt, read a line of text into buf (max maxlen chars).
\   Handles printable ASCII (32-126), Backspace (8/127), Enter (13)
\   to confirm, Escape (27) to cancel (returns 0).  Arrow keys and
\   other CSI escape sequences are consumed harmlessly.
\   Buffer is always null-terminated on exit.

: TUI-INPUT  ( buf maxlen prompt-addr prompt-len -- actual-len )
    TYPE                                \ print prompt
    0                                   ( buf maxlen pos )
    BEGIN
        KEY                             ( buf maxlen pos c )
        DUP 13 = IF DROP               \ Enter -> confirm
            2 PICK OVER + 0 SWAP C!    \ null-terminate buf[pos]
            NIP NIP EXIT               ( pos )
        THEN
        DUP 27 = IF DROP               \ ESC byte received
            KEY? IF                     \ sequence follows -> consume it
                KEY DUP 91 = IF         \ CSI '[' prefix
                    DROP
                    BEGIN KEY DUP 64 >= OVER 126 <= AND UNTIL
                    DROP                \ consume until final byte 64-126
                ELSE DROP THEN          \ non-CSI: consumed one extra byte
            ELSE                        \ bare Esc -> cancel
                2 PICK 0 SWAP C!        \ null-terminate buf[0]
                DROP 2DROP 0 EXIT       ( 0 )
            THEN
        ELSE
        DUP 8 = OVER 127 = OR IF       \ Backspace (BS=8, DEL=127)
            DROP
            DUP 0> IF
                1-  8 EMIT 32 EMIT 8 EMIT  \ erase previous char
            THEN
        ELSE
        DUP 32 >= OVER 126 <= AND IF    \ printable ASCII only (32..126)
            2 PICK 2 PICK > IF          \ pos < maxlen?
                DUP EMIT                ( buf maxlen pos c )
                3 PICK 2 PICK + C!      \ buf[pos] = c
                1+
            ELSE DROP THEN
        ELSE
            DROP                        \ ignore control chars / non-ASCII
        THEN THEN THEN
    AGAIN ;

\ ── Install TUI renderer ──────────────────────────────────────────
: INSTALL-TUI  ( -- )
    ['] TUI-TITLE    WV-TITLE   WV!
    ['] TUI-SECTION  WV-SECTION WV!
    ['] TUI-LINE     WV-LINE    WV!
    ['] TUI-KV       WV-KV      WV!
    ['] TUI-KV-XT    WV-KV-XT   WV!
    ['] TUI-FLAG     WV-FLAG    WV!
    ['] TUI-FLAG-2   WV-FLAG-2  WV!
    ['] TUI-HBAR     WV-HBAR    WV!
    ['] TUI-GAP      WV-GAP     WV!
    ['] TUI-LIST     WV-LIST    WV!
    ['] TUI-DETAIL   WV-DETAIL  WV!
    ['] TUI-HINT     WV-HINT    WV!
    ['] TUI-CUSTOM   WV-CUSTOM  WV!
    ['] TUI-INPUT    WV-INPUT   WV! ;
INSTALL-TUI

\ ── Public widget API (dispatch through WVEC) ─────────────────────
: W.TITLE    ( addr len -- )                           WV-TITLE   WV@ EXECUTE ;
: W.SECTION  ( addr len -- )                           WV-SECTION WV@ EXECUTE ;
: W.LINE     ( addr len -- )                           WV-LINE    WV@ EXECUTE ;
: W.KV       ( n addr len -- )                         WV-KV      WV@ EXECUTE ;
: W.KV-XT    ( xt addr len -- )                        WV-KV-XT   WV@ EXECUTE ;
: W.FLAG     ( flag addr len -- )                      WV-FLAG    WV@ EXECUTE ;
: W.FLAG-2   ( flag t-a t-n f-a f-n addr len -- )      WV-FLAG-2  WV@ EXECUTE ;
: W.HBAR     ( -- )                                    WV-HBAR    WV@ EXECUTE ;
: W.GAP      ( -- )                                    WV-GAP     WV@ EXECUTE ;
: W.LIST     ( count item-xt -- )                      WV-LIST    WV@ EXECUTE ;
: W.DETAIL   ( count xt -- )                           WV-DETAIL  WV@ EXECUTE ;
: W.HINT     ( addr len -- )                           WV-HINT    WV@ EXECUTE ;
: W.CUSTOM   ( xt -- )                                 WV-CUSTOM  WV@ EXECUTE ;
: W.INPUT    ( buf maxlen prompt-addr prompt-len -- len ) WV-INPUT   WV@ EXECUTE ;

\ ── Title with dynamic count suffix ──────────────────────────────
\ Convenience: "Label (N)" — used by many list screens.
: W.TITLE-N  ( n addr len -- )
    .LABEL ."   " TYPE ."  (" .N ." )" ./LABEL CR CR ;


\ =====================================================================
\ §9.6  Screen Definitions (SDL)
\ =====================================================================
\
\  Each screen is a word that calls W.xxx widgets.  Registration,
\  event loop, and SCREENS are unchanged.  Item renderers (.XXX-ROW)
\  are small helper words called by W.LIST.

\ ── List-item renderers ──────────────────────────────────────────

: .BTYPE  ( n -- )    \ print buffer type tag
    DUP 0 = IF DROP ." raw" EXIT THEN
    DUP 1 = IF DROP ." rec" EXIT THEN
    DUP 2 = IF DROP ." til" EXIT THEN
    3 = IF ." bit" EXIT THEN
    ." ?" ;

: .BUF-ROW  ( i -- )
    DUP .N ."   "
    BUF-NTH
    DUP B.TYPE .BTYPE
    ."  w=" DUP B.WIDTH .N
    ."  n=" DUP B.LEN .N
    ."  tiles=" DUP B.TILES .N
    ."  @" B.DATA .N ;

: .BUF-DETAIL  ( -- )
    SCR-SEL @ BUF-NTH
    DUP B.INFO B.PREVIEW ;

: .KERN-ROW  ( i -- )
    DUP .N ."   "
    CELLS KERN-TABLE + @
    DUP K.IN .N ."  in "
    DUP K.OUT .N ."  out "
    DUP K.FOOT .N ."  foot "
    K.FLAGS IF 3 FG ." [tile]" RESET-COLOR ELSE DIM ." [cpu]" RESET-COLOR THEN ;

: .PIPE-ROW  ( i -- )
    DUP .N ."   "
    CELLS PIPE-TABLE + @
    ." cap=" DUP P.CAP .N
    ."  steps=" P.COUNT .N ;

: .TASK-STATUS  ( st -- )    \ print colored status tag
    DUP 0 = IF DROP DIM ." FREE " RESET-COLOR EXIT THEN
    DUP 1 = IF DROP 2 FG ." READY" RESET-COLOR EXIT THEN
    DUP 2 = IF DROP 3 FG ." RUN  " RESET-COLOR EXIT THEN
    DUP 3 = IF DROP 1 FG ." BLOCK" RESET-COLOR EXIT THEN
    4 = IF DIM ." DONE " RESET-COLOR EXIT THEN
    ." ?    " ;

: .TASK-ROW  ( i -- )
    DUP .N ."   "
    CELLS TASK-TABLE + @
    DUP T.STATUS .TASK-STATUS
    ."  pri=" DUP T.PRIORITY .N
    ."  xt=" T.XT .N ;

: .TASK-DETAIL  ( -- )
    SCR-SEL @ CELLS TASK-TABLE + @
    ."   Status: " DUP T.STATUS .TASK-STATUS CR
    ."   XT: " DUP T.XT .N ."    Priority: " T.PRIORITY .N CR
    S" [k] Kill  [s] Restart" W.HINT ;

: .CORE-ROW  ( i -- )
    DUP .N ."   "
    DUP MICRO-CORE? IF DIM ." [mu] " RESET-COLOR ELSE ." [full] " THEN
    DUP COREID = IF
        DROP 3 FG ." RUNNING" RESET-COLOR ."  (self)"
    ELSE
        CORE-STATUS IF 2 FG ." BUSY" RESET-COLOR
        ELSE DIM ." IDLE" RESET-COLOR THEN
    THEN ;

: .PORT-ROW  ( i -- )
    ." port " .N ;

\ ── Helper: count active dir entries by file-type ────────────────
\ .DOC-FILE-LIST ( ftype -- n )  list docs/tuts with selection
: .DOC-FILE-LIST  ( ftype -- n )
    0 DOC-N !
    FS-OK @ IF
        0 DOC-TUT-COUNT !
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE OVER = IF
                    SCR-SEL @ DOC-N @ = IF 2 FG ."  > " RESET-COLOR ELSE ."     " THEN
                    DOC-N @ .N ."   " I DIRENT .ZSTR CR
                    1 DOC-N +!  1 DOC-TUT-COUNT +!
                THEN
            THEN
        LOOP
        DROP
        DOC-TUT-COUNT @ 0= IF ."     (none)" CR THEN
    ELSE
        DROP ."     (no filesystem loaded)" CR
    THEN
    DOC-N @ ;

: .STOR-ROW  ( slot i -- )     \ storage row from STOR-N iteration
    DUP .N ."   "
    DROP    \ slot unused here — row printed by caller
    ;

\ ── Screen 1: Home ──
: .HOME-CORES-VAL  ( -- )
    NCORES .N
    NCORES 1 > IF 2 FG ."  multicore" ELSE DIM ."  single" THEN RESET-COLOR ;
: .HOME-PORTS-VAL  ( -- )
    PORT-COUNT @ .N ."  bound  rx=" PORT-RX @ .N ."  drop=" PORT-DROP @ .N ;

: SCR-HOME  ( -- )
    S" System Overview" W.TITLE
    HERE          S" Memory"    W.KV
    ['] .HOME-CORES-VAL S" Cores" W.KV-XT
    BUF-COUNT @   S" Buffers"   W.KV
    KERN-COUNT @  S" Kernels"   W.KV
    PIPE-COUNT @  S" Pipes"     W.KV
    TASK-COUNT @  S" Tasks"     W.KV
    FILE-COUNT @  S" Files"     W.KV
    DISK? S" present" S" not attached" S" Storage" W.FLAG-2
    ['] .HOME-PORTS-VAL S" Ports" W.KV-XT
    NET-RX? S" frame waiting" S" idle" S" Network" W.FLAG-2
    W.GAP
    PREEMPT-ENABLED @ S" preempt ON" S" cooperative" S" Scheduler" W.FLAG-2
    TASK-COUNT-READY  S" Tasks rdy" W.KV ;

\ ── Screen 2: Buffers ──
: SCR-BUFFERS  ( -- )
    BUF-COUNT @ S" Buffers" W.TITLE-N
    BUF-COUNT @ ['] .BUF-ROW W.LIST
    BUF-COUNT @ ['] .BUF-DETAIL W.DETAIL ;

\ ── Screen 3: Kernels ──
: SCR-KERNELS  ( -- )
    KERN-COUNT @ S" Kernels" W.TITLE-N
    KERN-COUNT @ ['] .KERN-ROW W.LIST ;

\ ── Screen 4: Pipelines ──
: SCR-PIPES  ( -- )
    PIPE-COUNT @ S" Pipelines" W.TITLE-N
    PIPE-COUNT @ ['] .PIPE-ROW W.LIST ;

\ ── Screen 5: Tasks ──
: SCR-TASKS  ( -- )
    TASK-COUNT @ S" Tasks" W.TITLE-N
    TASK-COUNT @ ['] .TASK-ROW W.LIST
    TASK-COUNT @ ['] .TASK-DETAIL W.DETAIL ;

\ ── Screen 6: Help ──
: SCR-HELP  ( -- )
    S" Quick Reference" W.TITLE
    S" Buffers" W.SECTION
    S" 0 1 N BUFFER name    Create buffer" W.LINE
    S" buf B.SUM/MIN/MAX    Tile reductions" W.LINE
    S" a b c B.ADD/SUB      Element-wise ops" W.LINE
    S" n buf B.SCALE/FILL   Modify buffer" W.LINE
    S" Kernels" W.SECTION
    S" 1 1 2 0 KERNEL name  Register kernel" W.LINE
    S" buf kzero/kfill/kadd Sample kernels" W.LINE
    S" buf knorm/khistogram  Advanced kernels" W.LINE
    S" th src dst kpeak      Peak detection" W.LINE
    S" Pipelines" W.SECTION
    S" 3 PIPELINE name      Create pipeline" W.LINE
    S" ' w pipe P.ADD/RUN   Build & execute" W.LINE
    S" pipe P.RUN-PAR       Parallel execute" W.LINE
    S" Tasks" W.SECTION
    S" ' w 0 TASK name      Create task" W.LINE
    S" SCHEDULE / BG         Run tasks" W.LINE
    S" Multicore" W.SECTION
    S" xt core CORE-RUN      Dispatch to core" W.LINE
    S" core CORE-WAIT        Wait for core" W.LINE
    S" BARRIER               Sync all cores" W.LINE
    S" n LOCK / n UNLOCK     Spinlock ops" W.LINE
    S" CORES                 Show core status" W.LINE
    S" Storage" W.SECTION
    S" buf sec B.SAVE/LOAD  Persist buffers" W.LINE
    S" DIR / CATALOG        List disk files" W.LINE
    S" CAT name             Print file" W.LINE
    S" buf SAVE-BUFFER name Save buf to file" W.LINE
    S" Data Ports" W.SECTION
    S" buf id PORT!          Bind NIC source" W.LINE
    S" networking.f: POLL / n INGEST" W.LINE
    S" PORTS                 List bindings" W.LINE
    S" Tools" W.SECTION
    S" DASHBOARD / STATUS    System views" W.LINE
    S" ' w BENCH / .BENCH   Benchmark" W.LINE ;

\ ── Screen 7: Documentation ──
: .DOCS-BODY  ( -- )
    S" Topics" W.SECTION
    FTYPE-DOC .DOC-FILE-LIST DROP
    W.GAP
    S" Tutorials" W.SECTION
    FTYPE-TUT .DOC-FILE-LIST DROP
    DOC-N @ SCR-MAX !
    W.GAP
    S" [Enter] Read selected document" W.HINT ;

: SCR-DOCS  ( -- )
    S" Documentation" W.TITLE
    ['] .DOCS-BODY W.CUSTOM ;

\ ── Screen 8: Storage ──
: .STOR-BODY  ( -- )
    DISK? 0= IF
        S" (no storage attached)" W.LINE  0 SCR-MAX ! EXIT
    THEN
    FS-OK @ 0= IF
        S" (filesystem not loaded)" W.LINE  0 SCR-MAX ! EXIT
    THEN
    0 STOR-N !
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            SCR-SEL @ STOR-N @ = IF 2 FG ."  > " RESET-COLOR ELSE ."    " THEN
            STOR-N @ .N ."   "
            I DIRENT .ZSTR
            ."   " I DIRENT DE.USED .N ."  B"
            ."   " I DIRENT DE.TYPE .FTYPE
            CR
            1 STOR-N +!
        THEN
    LOOP
    STOR-N @ SCR-MAX !
    STOR-N @ 0= IF S" (empty)" W.LINE THEN
    W.GAP
    0  FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DIM ."   " .N ."  free sectors" RESET-COLOR CR
    \ detail pane
    SCR-SEL @ -1 <> SCR-SEL @ STOR-N @ < AND IF
        W.HBAR
        SCR-SEL @ FIND-NTH-ACTIVE DUP -1 <> IF
            ."   Name  : " DUP DIRENT .ZSTR CR
            ."   Type  : " DUP DIRENT DE.TYPE .FTYPE CR
            ."   Size  : " DUP DIRENT DE.USED .N ."  bytes" CR
            ."   Start : sector " DUP DIRENT DE.SEC .N CR
            ."   Count : " DIRENT DE.COUNT .N ."  sectors" CR
        ELSE DROP THEN
    THEN ;

: SCR-STORAGE  ( -- )
    S" Storage" W.TITLE
    ['] .STOR-BODY W.CUSTOM ;

\ ── Screen 9: Cores ──
: .CORES-BODY  ( -- )
    NCORES 1 <= IF
        S" Single-core mode -- no secondary cores available." W.LINE
    ELSE
        NCORES ['] .CORE-ROW W.LIST
        S" Multicore Words" W.SECTION
        S" xt core CORE-RUN    Dispatch work to core" W.LINE
        S" core CORE-WAIT      Wait for core to finish" W.LINE
        S" BARRIER             Sync all secondary cores" W.LINE
        S" pipe P.RUN-PAR      Parallel pipeline execute" W.LINE
        S" n LOCK / n UNLOCK   Spinlock operations" W.LINE
    THEN ;

: SCR-CORES  ( -- )
    NCORES S" Cores" W.TITLE-N
    ['] .CORES-BODY W.CUSTOM ;

\ ── Home subscreens ──

: SCR-HOME-OVERVIEW  ( -- )  SCR-HOME ;

: .HOME-MEM-BUFS  ( -- )
    BUF-COUNT @ 0 DO
        ."      " I .N ."  "
        I BUF-NTH DUP B.WIDTH .N ." x" B.LEN .N CR
    LOOP ;

: SCR-HOME-MEMORY  ( -- )
    S" Memory Detail" W.TITLE
    HERE             S" HERE"       W.KV
    65536 HERE -     S" Free dict"  W.KV
    HEAP-INIT @ S" initialized" S" not initialized" S" Heap" W.FLAG-2
    HEAP-INIT @ IF HEAP-BASE @ S" Heap base" W.KV THEN
    S" Stack" W.SECTION
    DEPTH            S" SP depth"   W.KV
    S" Buffers memory" W.SECTION
    BUF-COUNT @      S" Count"      W.KV
    ['] .HOME-MEM-BUFS W.CUSTOM ;

: SCR-HOME-NET  ( -- )
    S" Network Status" W.TITLE
    NET-RX? S" frame waiting" S" idle" S" NIC state" W.FLAG-2
    PORT-COUNT @     S" Ports"      W.KV
    PORT-RX @        S" RX count"   W.KV
    PORT-DROP @      S" Drops"      W.KV
    S" Port Bindings" W.SECTION
    PORT-COUNT @ ['] .PORT-ROW W.LIST ;

\ ── Buffer subscreens ──

: SCR-BUF-LIST  ( -- )  SCR-BUFFERS ;

VARIABLE _SRAW
VARIABLE _SREC
VARIABLE _STIL
VARIABLE _SBIT

: .BSTATS-BODY  ( -- )
    BUF-COUNT @ 0= IF EXIT THEN
    0 _SRAW !  0 _SREC !  0 _STIL !  0 _SBIT !
    BUF-COUNT @ 0 DO
        I BUF-NTH B.TYPE
        DUP 0 = IF 1 _SRAW +! THEN
        DUP 1 = IF 1 _SREC +! THEN
        DUP 2 = IF 1 _STIL +! THEN
        3 = IF 1 _SBIT +! THEN
    LOOP
    S" By Type" W.SECTION
    _SRAW @ S" Raw"    W.KV
    _SREC @ S" Record" W.KV
    _STIL @ S" Tile"   W.KV
    _SBIT @ S" Bitmap" W.KV ;

: SCR-BUF-STATS  ( -- )
    S" Buffer Statistics" W.TITLE
    BUF-COUNT @  S" Total buffers" W.KV
    ['] .BSTATS-BODY W.CUSTOM ;

\ ---- Screen label words (for registry) ----
