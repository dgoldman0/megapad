\ ---- Screen label words (for registry) ----

: LBL-HOME  ." Home" ;
: LBL-BUFS  ." Bufs" ;
: LBL-KERN  ." Kern" ;
: LBL-PIPE  ." Pipe" ;
: LBL-TASK  ." Task" ;
: LBL-HELP  ." Help" ;
: LBL-DOCS  ." Docs" ;
: LBL-STOR  ." Stor" ;
: LBL-CORE  ." Core" ;

\ ---- Subscreen label words ----

: LBL-OVERVIEW  ." Overview" ;
: LBL-MEMORY    ." Memory" ;
: LBL-NET       ." Network" ;
: LBL-BLIST     ." List" ;
: LBL-BSTATS    ." Stats" ;

\ -- Screen dispatch (registry-based) --
: RENDER-SCREEN  ( -- )
    PAGE SCREEN-HEADER
    SCREEN-ID @ DUP 1 < OVER NSCREENS @ > OR IF DROP 1 THEN
    1-                                          \ 0-based index
    DUP CELLS SUB-COUNTS + @ DUP 0> IF
        DROP SUB-TABS                           \ show sub-tab bar
        SCREEN-ID @ 1- MAX-SUBS * SUBSCREEN-ID @ +
        CELLS SUB-XT + @ DUP 0<> IF
            ['] EXECUTE CATCH IF
                RESET-COLOR CR ." [screen error]" CR
            THEN
        ELSE DROP ." [no renderer]" CR THEN
    ELSE
        DROP
        CELLS SCR-XT + @ DUP 0<> IF
            ['] EXECUTE CATCH IF
                RESET-COLOR CR ." [screen error]" CR
            THEN
        ELSE DROP ." [no renderer]" CR THEN
    THEN
    CR SCREEN-FOOTER ;

\ -- Screen switch helper (registry-based) --
: SWITCH-SCREEN  ( n -- )
    DUP SCREEN-ID !
    1- CELLS SCR-FLAGS + @ 1 AND
    IF 0 ELSE -1 THEN SCR-SEL !
    0 SCR-MAX !
    0 SUBSCREEN-ID !
    RENDER-SCREEN ;

\ ---- Screen-specific key handlers ----

: TASK-KEYS  ( c -- consumed )
    DUP 107 = IF DROP                         \ 'k' = kill task
        SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
            CELLS TASK-TABLE + @ KILL
            RENDER-SCREEN
        ELSE DROP THEN -1 EXIT
    THEN
    DUP 115 = IF DROP                         \ 's' = restart task
        SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
            CELLS TASK-TABLE + @ RESTART
            RENDER-SCREEN
        ELSE DROP THEN -1 EXIT
    THEN
    DROP 0 ;

\ -- Per-screen key dispatch (returns consumed flag) --
: CALL-SCREEN-KEY  ( c -- c consumed )
    SCREEN-ID @ 1- CELLS SCR-KEY-XT + @ DUP 0<> IF
        OVER SWAP ['] EXECUTE CATCH IF DROP 0 THEN
    ELSE
        DROP 0              \ no handler -> not consumed
    THEN ;

\ -- Activate selected item --
: DO-SELECT  ( -- )
    SCREEN-ID @ 1- CELLS SCR-ACT-XT + @ DUP 0<> IF
        ['] EXECUTE CATCH IF
            RESET-COLOR CR ." [action error]" CR
        THEN
    ELSE
        DROP
        SCREEN-ID @ 7 = IF SCR-SEL @ SHOW-NTH-DOC THEN   \ legacy fallback
    THEN ;

\ -- Event loop: poll KEY?, dispatch on keypress (registry-based) --
: HANDLE-KEY  ( c -- )
    \ ESC sequence: consume CSI arrow keys for subscreen navigation
    DUP 27 = IF DROP                               \ ESC (0x1B)
        KEY? IF
            KEY DUP 91 = IF                        \ '[' = CSI prefix
                DROP KEY                           \ read direction byte
                DUP 68 = IF DROP                   \ Left arrow → prev sub
                    SCREEN-SUBS 0> IF
                        SUBSCREEN-ID @ 1- DUP 0< IF
                            DROP SCREEN-SUBS 1-
                        THEN SUBSCREEN-ID !
                        RENDER-SCREEN
                    THEN EXIT
                THEN
                DUP 67 = IF DROP                   \ Right arrow → next sub
                    SCREEN-SUBS 0> IF
                        SUBSCREEN-ID @ 1+ DUP SCREEN-SUBS >= IF
                            DROP 0
                        THEN SUBSCREEN-ID !
                        RENDER-SCREEN
                    THEN EXIT
                THEN
                DROP EXIT                          \ Up/Down/other: ignore
            ELSE DROP THEN                         \ non-CSI: consume & ignore
        THEN EXIT                                  \ bare ESC: ignore
    THEN
    \ Per-screen custom key handler (priority: checked first)
    CALL-SCREEN-KEY IF DROP EXIT THEN
    \ Digit keys 0-9: switch to screen 1-10 (key '0'=48...'9'=57)
    DUP 48 >= OVER 57 <= AND IF
        DUP 48 - DUP NSCREENS @ < IF
            1+ SWITCH-SCREEN DROP EXIT
        ELSE DROP THEN
    THEN
    \ Hex keys a-f: switch to screen 11-16 (key 'a'=97...'f'=102)
    DUP 97 >= OVER 102 <= AND IF
        DUP 97 - 10 + DUP NSCREENS @ < IF
            1+ SWITCH-SCREEN DROP EXIT
        ELSE DROP THEN
    THEN
    DUP 113 = IF DROP 0 SCREEN-RUN ! EXIT THEN   \ 'q'
    DUP 114 = IF DROP RENDER-SCREEN EXIT THEN     \ 'r'
    DUP 65 = IF DROP                               \ 'A' = toggle auto-refresh
        AUTO-REFRESH @ IF 0 ELSE -1 THEN AUTO-REFRESH !
        RENDER-SCREEN EXIT
    THEN
    \ Subscreen navigation: '[' = prev sub, ']' = next sub
    DUP 91 = IF DROP                               \ '['
        SCREEN-SUBS 0> IF
            SUBSCREEN-ID @ 1- DUP 0< IF
                DROP SCREEN-SUBS 1-
            THEN SUBSCREEN-ID !
            RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 93 = IF DROP                               \ ']'
        SCREEN-SUBS 0> IF
            SUBSCREEN-ID @ 1+ DUP SCREEN-SUBS >= IF
                DROP 0
            THEN SUBSCREEN-ID !
            RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 110 = IF DROP                              \ 'n' = next item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1+ DUP SCR-MAX @ >= IF DROP 0 THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 112 = IF DROP                              \ 'p' = prev item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1- DUP 0< IF
                DROP SCR-MAX @ 1- DUP 0< IF DROP 0 THEN
            THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 13 = OVER 32 = OR IF DROP                  \ ENTER / SPACE = activate
        SCREEN-SELECTABLE? IF
            SCR-SEL @ -1 <> IF
                DO-SELECT RENDER-SCREEN
            THEN
        THEN EXIT
    THEN
    DROP ;

\ -- §9.10  Screen registration --

\ Register screens (order = display order; index 0..N-1, key = hex 0..F)
' SCR-HOME     ' LBL-HOME 0 REGISTER-SCREEN DROP  \ [0]Home
' SCR-BUFFERS  ' LBL-BUFS 1 REGISTER-SCREEN DROP  \ [1]Bufs  (selectable)
' SCR-KERNELS  ' LBL-KERN 0 REGISTER-SCREEN DROP  \ [2]Kern
' SCR-PIPES    ' LBL-PIPE 0 REGISTER-SCREEN DROP  \ [3]Pipe
' SCR-TASKS    ' LBL-TASK 1 REGISTER-SCREEN DROP  \ [4]Task  (selectable)
' SCR-HELP     ' LBL-HELP 0 REGISTER-SCREEN DROP  \ [5]Help
' SCR-DOCS     ' LBL-DOCS 1 REGISTER-SCREEN DROP  \ [6]Docs  (selectable)
' SCR-STORAGE  ' LBL-STOR 1 REGISTER-SCREEN DROP  \ [7]Stor  (selectable)
' SCR-CORES    ' LBL-CORE 0 REGISTER-SCREEN DROP  \ [8]Core

\ Per-screen custom key handlers
' TASK-KEYS  4 SET-SCREEN-KEYS    \ screen [5]Task (index 4) gets k/s keys

\ Home subscreens:  Overview | Memory | Network
' SCR-HOME-OVERVIEW  ' LBL-OVERVIEW 0 ADD-SUBSCREEN
' SCR-HOME-MEMORY    ' LBL-MEMORY   0 ADD-SUBSCREEN
' SCR-HOME-NET       ' LBL-NET      0 ADD-SUBSCREEN

\ Buffers subscreens: List | Stats
' SCR-BUF-LIST       ' LBL-BLIST   1 ADD-SUBSCREEN
' SCR-BUF-STATS      ' LBL-BSTATS  1 ADD-SUBSCREEN

\ -- TUI event loop (factored for reuse) --
: SCREEN-LOOP  ( -- )
    1 SCREEN-RUN !
    CYCLES REFRESH-LAST !
    BEGIN
        KEY? IF KEY HANDLE-KEY THEN
        AUTO-REFRESH @ IF
            CYCLES REFRESH-LAST @ - 5000000 > IF
                CYCLES REFRESH-LAST !
                RENDER-SCREEN
            THEN
        THEN
        SCREEN-RUN @
    0= UNTIL
    PAGE
    ."  Returned to REPL."  CR ;

\ -- Main TUI entry point --
: SCREENS  ( -- )
    1 SCREEN-ID !
    -1 SCR-SEL !  0 SCR-MAX !
    0 SUBSCREEN-ID !
    RENDER-SCREEN
    SCREEN-LOOP ;

\ -- Enter TUI at screen n  (e.g. 9 SCREEN → [8]Core) --
: SCREEN  ( n -- )
    SWITCH-SCREEN  SCREEN-LOOP ;

\ =====================================================================
