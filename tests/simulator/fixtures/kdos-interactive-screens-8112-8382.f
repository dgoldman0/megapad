\ -- Forward declarations for §10 words needed by §9 TUI --
VARIABLE PORT-COUNT     0 PORT-COUNT !
VARIABLE PORT-RX        0 PORT-RX !
VARIABLE PORT-DROP      0 PORT-DROP !
: NET-RX?  ( -- flag )   NET-STATUS 2 AND 0<> ;

\ =====================================================================
\  §9  Interactive Screens
\ =====================================================================
\
\  Full-screen TUI built on ANSI escape sequences.
\  Screens are registered dynamically via REGISTER-SCREEN.
\  Each screen can own subscreens, navigated with [ and ].
\  Keys: 0-9/a-f switch, n/p select, [/] sub-switch, Enter activate, A auto, r/q.
\
\  THREADING RULE: All screen state (NSCREENS, SCREEN-ID, SCR-SEL,
\  SCR-* arrays) lives in shared dictionary memory and is NOT
\  thread-safe.  REGISTER-SCREEN, SWITCH-SCREEN, RENDER-SCREEN,
\  and HANDLE-KEY must only be called from the main core (core 0).
\  Background tasks on secondary cores that need to register or
\  modify screens should send a request via the mailbox (IPI) and
\  let the main-core event loop service it between iterations.
\

\ -- §9.1  Screen & subscreen registry tables --
16 CONSTANT MAX-SCREENS
 8 CONSTANT MAX-SUBS

CREATE SCR-XT      MAX-SCREENS CELLS ALLOT    \ render xt per screen
CREATE SCR-LBL-XT  MAX-SCREENS CELLS ALLOT    \ label-print xt
CREATE SCR-FLAGS   MAX-SCREENS CELLS ALLOT    \ bit 0 = selectable
CREATE SCR-KEY-XT  MAX-SCREENS CELLS ALLOT    \ per-screen key handler (0=none)
CREATE SCR-ACT-XT  MAX-SCREENS CELLS ALLOT    \ per-screen activate xt (0=none)

CREATE SUB-XT      MAX-SCREENS MAX-SUBS * CELLS ALLOT
CREATE SUB-LBL-XT  MAX-SCREENS MAX-SUBS * CELLS ALLOT
CREATE SUB-COUNTS  MAX-SCREENS CELLS ALLOT

VARIABLE NSCREENS      0 NSCREENS !

\ -- Hex digit printer for screen labels --
: .HEXDIG  ( n -- )   \ print single hex digit 0-15
    DUP 10 < IF 48 + EMIT ELSE 10 - 65 + EMIT THEN ;

\ -- §9.2  Cursor & screen control (ESC/CSI/.N/SGR/RESET-COLOR/DIM above §7.6.1) --
: AT-XY   ( col row -- )  CSI .N 59 EMIT .N 72 EMIT ;   \ ESC[row;colH
: PAGE     ( -- )  CSI 50 EMIT 74 EMIT CSI 72 EMIT ;     \ ESC[2J ESC[H
: CLS      ( -- )  PAGE ;                                  \ alias

\ -- Extra colors --
: BOLD     ( -- )  1 SGR ;
: REVERSE  ( -- )  7 SGR ;
: FG       ( n -- )  30 + SGR ;    \ 0=black 1=red 2=green 3=yellow 4=blue 5=magenta 6=cyan 7=white
: BG-COLOR ( n -- )  40 + SGR ;

\ -- Horizontal line with color --
: HBAR   ( -- )
    DIM
    60 0 DO 196 EMIT LOOP
    RESET-COLOR CR ;

\ -- Padded label field --
: .LABEL  ( -- )  BOLD ;    \ turn bold on before label
: ./LABEL ( -- )  RESET-COLOR ;  \ turn off after

\ -- Screen state --
VARIABLE SCREEN-ID      1 SCREEN-ID !   \ current screen: 1-based (index+1)
VARIABLE SCREEN-RUN     \ flag: 0 = exit loop

\ -- Extended screen state --
VARIABLE SCR-SEL      -1 SCR-SEL !     \ selected item on current screen
VARIABLE SCR-MAX       0 SCR-MAX !     \ max selectable items on screen
VARIABLE AUTO-REFRESH  0 AUTO-REFRESH !
VARIABLE REFRESH-LAST
VARIABLE SUBSCREEN-ID  0 SUBSCREEN-ID !  \ active subscreen index

\ -- Find Nth active directory entry (for Storage screen) --
VARIABLE FNA-WANT
VARIABLE FNA-FOUND

: FIND-NTH-ACTIVE  ( n -- slot | -1 )
    FNA-WANT !  -1 FNA-FOUND !
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            DUP FNA-WANT @ = IF
                DROP I FNA-FOUND !  LEAVE
            THEN
            1+
        THEN
    LOOP
    DROP FNA-FOUND @ ;

\ -- Show Nth doc/tutorial file (full-screen pager) --
VARIABLE DOC-SEL-N
VARIABLE DOC-SEL-FOUND

: SHOW-NTH-DOC  ( n -- )
    DOC-SEL-N !  0 DOC-SEL-FOUND !
    FS-OK @ 0= IF EXIT THEN
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE DUP FTYPE-DOC = SWAP FTYPE-TUT = OR IF
                DOC-SEL-FOUND @ DOC-SEL-N @ = IF
                    I OPEN-BY-SLOT DUP 0<> IF
                        DUP >R PAGE SHOW-FILE R> FCLOSE
                        CR DIM ."   Press any key to return..."  RESET-COLOR
                        KEY DROP
                    ELSE DROP THEN
                    LEAVE
                THEN
                1 DOC-SEL-FOUND +!
            THEN
        THEN
    LOOP ;

\ -- Screen-local counters --
VARIABLE STOR-N
VARIABLE DOC-N
VARIABLE DOC-TUT-COUNT

\ -- §9.4  Registration API --

VARIABLE _ASUB-P
VARIABLE _ASUB-I

: REGISTER-SCREEN  ( xt-render xt-label flags -- id | -1 )
    NSCREENS @ DUP MAX-SCREENS >= IF
        DROP 2DROP DROP -1 EXIT       \ table full → return -1
    THEN
    >R
    R@ CELLS SCR-FLAGS + !
    R@ CELLS SCR-LBL-XT + !
    R@ CELLS SCR-XT + !
    0 R@ CELLS SCR-KEY-XT + !
    0 R@ CELLS SCR-ACT-XT + !
    0 R@ CELLS SUB-COUNTS + !
    NSCREENS @ 1+ NSCREENS !
    \ Reset selection state if we happen to be viewing this slot
    SCREEN-ID @ 1- R@ = IF
        R@ CELLS SCR-FLAGS + @ 1 AND
        IF 0 ELSE -1 THEN SCR-SEL !
        0 SCR-MAX !
    THEN
    R> ;

: SET-SCREEN-KEYS  ( xt screen-id -- )
    CELLS SCR-KEY-XT + ! ;

: SET-SCREEN-ACT  ( xt screen-id -- )
    CELLS SCR-ACT-XT + ! ;

\ UNREGISTER-SCREEN ( id -- )
\   Remove screen at 0-based index 'id'.  Shifts all entries above
\   id down by one and decrements NSCREENS.  Adjusts SCREEN-ID and
\   SCR-SEL if the current screen was removed or its index shifted.
\   No-op if id is out of range.
VARIABLE _UNR-I
VARIABLE _UNR-N

: (SHIFT-ARRAY)  ( base id n -- )
    \ Shift cells base[id+1..n-1] down into base[id..n-2].
    SWAP                              ( base n id )
    BEGIN DUP 1+ 2 PICK < WHILE      ( base n id -- while id+1 < n )
        2 PICK OVER 1+ CELLS + @     ( base n id val[id+1] )
        3 PICK 2 PICK CELLS + !      ( base[id] = val[id+1] )
        1+
    REPEAT
    2DROP DROP ;

: (SHIFT-SUB-ARRAYS)  ( id n -- )
    \ Shift sub-screen tables: each screen owns MAX-SUBS slots.
    SWAP                              ( n id )
    BEGIN DUP 1+ 2 PICK < WHILE
        \ SUB-XT: copy MAX-SUBS cells from (id+1)*MAX-SUBS → id*MAX-SUBS
        DUP 1+ MAX-SUBS * CELLS SUB-XT +
        OVER   MAX-SUBS * CELLS SUB-XT +
        MAX-SUBS CELLS CMOVE
        \ SUB-LBL-XT: same
        DUP 1+ MAX-SUBS * CELLS SUB-LBL-XT +
        OVER   MAX-SUBS * CELLS SUB-LBL-XT +
        MAX-SUBS CELLS CMOVE
        1+
    REPEAT
    2DROP ;

: UNREGISTER-SCREEN  ( id -- )
    DUP 0< OVER NSCREENS @ >= OR IF DROP EXIT THEN
    _UNR-I !  NSCREENS @ _UNR-N !
    \ Shift each per-screen array down
    SCR-XT     _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    SCR-LBL-XT _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    SCR-FLAGS  _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    SCR-KEY-XT _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    SCR-ACT-XT _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    SUB-COUNTS _UNR-I @ _UNR-N @ (SHIFT-ARRAY)
    _UNR-I @ _UNR-N @ (SHIFT-SUB-ARRAYS)
    \ Decrement count
    _UNR-N @ 1- NSCREENS !
    \ Fix SCREEN-ID if needed
    SCREEN-ID @ 1- _UNR-I @ = IF
        \ Current screen was removed — fall back to 1
        1 SCREEN-ID !  -1 SCR-SEL !  0 SCR-MAX !
    ELSE
        SCREEN-ID @ 1- _UNR-I @ > IF
            \ Current screen's index shifted down
            SCREEN-ID @ 1- SCREEN-ID !
        THEN
    THEN ;

: ADD-SUBSCREEN  ( xt-render xt-label parent-id -- )
    _ASUB-P !
    _ASUB-P @ CELLS SUB-COUNTS + @ _ASUB-I !
    _ASUB-I @ MAX-SUBS >= IF 2DROP EXIT THEN   \ sub table full → silently ignore
    _ASUB-P @ MAX-SUBS * _ASUB-I @ + CELLS SUB-LBL-XT +
    !
    _ASUB-P @ MAX-SUBS * _ASUB-I @ + CELLS SUB-XT +
    !
    _ASUB-P @ CELLS SUB-COUNTS + DUP @ 1+ SWAP ! ;

: SCREEN-SUBS  ( -- n )
    SCREEN-ID @ 1- CELLS SUB-COUNTS + @ ;

: SCREEN-SELECTABLE?  ( -- flag )
    SCREEN-ID @ 1- CELLS SCR-FLAGS + @ 1 AND 0<> ;

\ -- Screen header (loops over registry) --
: SCREEN-HEADER  ( -- )
    1 1 AT-XY
    REVERSE
    ."   KDOS v1.1 "
    RESET-COLOR
    SPACE
    NSCREENS @ 0 DO
        SCREEN-ID @ I 1+ = IF REVERSE THEN
        ."  [" I .HEXDIG ." ]"
        NSCREENS @ 10 <= IF                       \ show labels only when ≤10
            I CELLS SCR-LBL-XT + @ DUP 0<> IF
                ['] EXECUTE CATCH IF ." ?" THEN
            ELSE DROP ." ?" THEN
        THEN
        ."  " RESET-COLOR
    LOOP
    CR HBAR ;

\ -- Subscreen tabs (shown when screen has subs) --
: SUB-TABS  ( -- )
    SCREEN-SUBS DUP 0= IF DROP EXIT THEN
    DIM ."    "
    0 DO
        SUBSCREEN-ID @ I = IF BOLD THEN
        ." ["
        SCREEN-ID @ 1- MAX-SUBS * I + CELLS SUB-LBL-XT + @ DUP 0<> IF
            ['] EXECUTE CATCH IF ." ?" THEN
        ELSE DROP ." ?" THEN
        ." ] "
        RESET-COLOR DIM
    LOOP
    RESET-COLOR CR ;

\ -- Screen footer --
: SCREEN-FOOTER  ( -- )
    DIM
    ."   [0-" NSCREENS @ 1- .HEXDIG ." ] Switch  [n/p] Select"
    SCREEN-SUBS 0> IF ."   [[/]] Sub" THEN
    ."   [r] Refresh"
    AUTO-REFRESH @ IF 2 FG ."   Auto:ON" RESET-COLOR DIM ELSE ."   [A]Auto" THEN
    ."    [q] Quit"
    RESET-COLOR CR ;

\ =====================================================================
