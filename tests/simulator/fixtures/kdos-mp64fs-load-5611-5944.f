\ ── LOAD — load and execute a Forth source file ─────────────────────
\ LOAD ( "filename" -- ) open a file by name, read it, EVALUATE it
\   Reads the entire file into a reclaimable loader allocation, then walks
\   through it line by line, EVALUATEing each line.
VARIABLE LD-BUF
VARIABLE LD-SZ
VARIABLE LD-CUR
VARIABLE LD-LEN
VARIABLE LD-LINE
\ Nesting support: save/restore walker state for nested LOAD/REQUIRE.
\ Includes CWD so relative-path loads restore the working directory.  Each
\ frame owns evaluator-depth and HERE/LATEST checkpoints plus a private module
\ transaction pointer.  Only the module-ID hooks are no-ops until §20 binds
\ them; dictionary rollback is intrinsic to every guarded loader frame.
\
\ Frame layout (11 cells, 88 bytes):
\   +0  saved LD-BUF       +8  saved LD-SZ
\   +16 saved LD-CUR       +24 saved LD-LEN
\   +32 saved LD-LINE      +40 saved EVAL-LINE
\   +48 saved CWD          +56 evaluator depth
\   +64 transaction head   +72 saved HERE       +80 saved LATEST
88 CONSTANT _LD-FRAME
16 CONSTANT _LD-MAXLVL
CREATE _LD-STK _LD-FRAME _LD-MAXLVL * ALLOT
VARIABLE _LD-SP
0 _LD-SP !
: _LD-ACTIVE-FRAME  ( -- addr )
    _LD-SP @ _LD-FRAME - _LD-STK + ;
: _LD-EVAL-CHECKPOINT  ( -- n )
    _LD-ACTIVE-FRAME 56 + @ ;
: _LD-TXN-HEAD  ( -- addr )
    _LD-ACTIVE-FRAME 64 + ;
: _LD-TXN-NOOP  ( -- ) ;
DEFER _LD-TXN-COMMIT
DEFER _LD-TXN-ROLLBACK
DEFER _LD-TXN-AFTER-RELEASE
' _LD-TXN-NOOP IS _LD-TXN-COMMIT
' _LD-TXN-NOOP IS _LD-TXN-ROLLBACK
' _LD-TXN-NOOP IS _LD-TXN-AFTER-RELEASE
: _LD-SAVE  ( -- )
    _LD-SP @ _LD-FRAME _LD-MAXLVL * >= ABORT" REQUIRE nested too deep"
    _LD-SP @ _LD-STK +
    LD-BUF @ OVER      !
    LD-SZ  @ OVER  8 + !
    LD-CUR @ OVER 16 + !
    LD-LEN @ OVER 24 + !
    LD-LINE @ OVER 32 + !
    EVAL-LINE @ OVER 40 + !
    CWD @ OVER 48 + !
    EVAL-DEPTH @ OVER 56 + !
    0 OVER 64 + !
    HERE OVER 72 + !
    LATEST SWAP 80 + !
    _LD-FRAME _LD-SP +! ;
: _LD-RESTORE  ( -- )
    _LD-SP @ 0= ABORT" REQUIRE nesting underflow"
    _LD-FRAME NEGATE _LD-SP +!
    _LD-SP @ _LD-STK +
    DUP      @ LD-BUF !
    DUP  8 + @ LD-SZ  !
    DUP 16 + @ LD-CUR !
    DUP 24 + @ LD-LEN !
    DUP 32 + @ LD-LINE !  DUP 40 + @ EVAL-LINE !
        48 + @ CWD    ! ;
VARIABLE _LD-RUN-SEC
VARIABLE _LD-RUN-CNT
VARIABLE _LD-RUN-ADDR
\ _LD-READ-RUN ( sector count addr -- next-addr )
\ The BIOS checked layer owns hardware-sized splitting and completion.
: _LD-READ-RUN  ( sector count addr -- next-addr )
    _LD-RUN-ADDR ! _LD-RUN-CNT ! _LD-RUN-SEC !
    _LD-RUN-ADDR @ _LD-RUN-SEC @ _LD-RUN-CNT @ _DISK-READ? 0= IF DISK-IO-IOR @ THROW THEN
    _LD-RUN-ADDR @ _LD-RUN-CNT @ SECTOR * + ;
: _LD-SLOT-BYTES  ( slot -- bytes )
    DIRENT DUP DE.COUNT SWAP DE.EXT1-CNT + SECTOR * ;
\ _LD-READ-SLOT ( slot -- )  Concatenate both validated extents in LD-BUF.
: _LD-READ-SLOT  ( slot -- )
    DIRENT
    DUP DE.SEC OVER DE.COUNT LD-BUF @ _LD-READ-RUN
    OVER DE.EXT1-CNT DUP IF
        2 PICK DE.EXT1-SEC SWAP ROT _LD-READ-RUN 2DROP
    ELSE
        DROP 2DROP
    THEN ;
\ ── Relative-path resolution for LOAD / REQUIRE ─────────────────────
\  Paths like "../markup/html.f" or "lib/util.f" are split on '/'.
\  Each intermediate component adjusts CWD (".." goes to parent,
\  anything else CDs into a subdirectory).  The final component
\  (the filename) is left in NAMEBUF for FIND-BY-NAME.  CWD is
\  saved by _LD-SAVE and restored by _LD-RESTORE so that nested
\  loads always return to the caller's working directory.
CREATE _RP-PATH 128 ALLOT    \ copy of full path from PATHBUF (up to 128 B)
CREATE _RP-COMP 24 ALLOT     \ current component being processed (≤ 23 chars)
VARIABLE _RP-I                \ scan position within _RP-PATH
\ _HAS-SLASH? ( -- flag )  True if PATHBUF contains a '/' character.
: _HAS-SLASH?  ( -- flag )
    FALSE
    128 0 DO
        PATHBUF I + C@ DUP 0= IF DROP LEAVE THEN
        47 = IF DROP TRUE LEAVE THEN
    LOOP ;
\ _RP-NEXT-SEP ( -- pos )  Index of next '/' or NUL from _RP-I.
: _RP-NEXT-SEP  ( -- pos )
    _RP-I @
    BEGIN
        DUP 128 < IF
            _RP-PATH OVER + C@ DUP 0= SWAP 47 = OR
            IF TRUE ELSE 1+ FALSE THEN
        ELSE TRUE THEN
    UNTIL ;
\ _RP-IS-DOTDOT? ( -- flag )  True if _RP-COMP is "..\0".
: _RP-IS-DOTDOT?  ( -- flag )
    _RP-COMP     C@ 46 =
    _RP-COMP 1+  C@ 46 = AND
    _RP-COMP 2 + C@ 0=  AND ;
\ _RP-CD-COMP ( -- ok? )  CD into directory named in _RP-COMP.
: _RP-CD-COMP  ( -- ok? )
    NAMEBUF 24 0 FILL
    _RP-COMP NAMEBUF 24 CMOVE
    FIND-BY-NAME DUP -1 = IF DROP FALSE EXIT THEN
    DUP DIRENT DE.TYPE 8 <> IF DROP FALSE EXIT THEN
    CWD ! TRUE ;
\ _RESOLVE-PATH ( -- )
\   If PATHBUF contains '/', walk directory components adjusting CWD
\   and leave the final filename in NAMEBUF.  No-op for plain names.
: _RESOLVE-PATH  ( -- )
    _HAS-SLASH? 0= IF EXIT THEN
    \ Handle leading '/' — absolute path, start from root
    PATHBUF C@ 47 = IF 255 CWD ! THEN
    PATHBUF _RP-PATH 128 CMOVE
    \ Skip leading '/' if present
    _RP-PATH C@ 47 = IF 1 ELSE 0 THEN  _RP-I !
    BEGIN
        _RP-NEXT-SEP                     ( end )
        \ What character terminated the scan?
        DUP 128 < IF _RP-PATH OVER + C@ ELSE 0 THEN
        47 = IF
            \ '/' found — extract directory component [_RP-I, end)
            _RP-COMP 24 0 FILL
            DUP _RP-I @ -                ( end len )
            _RP-PATH _RP-I @ + _RP-COMP ROT CMOVE  ( end )
            1+ _RP-I !                   \ advance past '/'
            \ Process component
            _RP-IS-DOTDOT? IF
                CWD @ 255 <> IF CWD @ DIRENT DE.PARENT CWD ! THEN
            ELSE
                _RP-CD-COMP 0= IF
                    ."  Path component not found: "
                    _RP-COMP .ZSTR CR EXIT
                THEN
            THEN
            FALSE                        \ continue loop
        ELSE
            \ NUL or end of buffer — remainder is the filename
            NAMEBUF 24 0 FILL
            DUP _RP-I @ - DUP 0> IF
                _RP-PATH _RP-I @ + NAMEBUF ROT CMOVE
            ELSE DROP THEN
            DROP TRUE                    \ done
        THEN
    UNTIL ;

\ ── Checked source compiler ─────────────────────────────────────────
\
\ SOURCE-EVALUATE-CHECKED is the transaction-friendly compiler surface
\ used by hosted tools such as Akashic Pad.  It walks a complete buffer,
\ evaluates one physical line at a time, stops at the first error, and
\ then checks that no colon definition or cross-line conditional remains
\ unfinished.  Callers pass their saved HERE/LATEST pair to DICT-ROLLBACK;
\ after a successful rollback they call EVALUATOR-RESET to clear compiler
\ bookkeeping.

0 CONSTANT EVAL-S-OK
1 CONSTANT EVAL-S-UNDEFINED
2 CONSTANT EVAL-S-LINE-TOO-LONG
3 CONSTANT EVAL-S-DEPTH
4 CONSTANT EVAL-S-UNFINISHED
5 CONSTANT EVAL-S-THROW

\ BIOS supplies the primitive EVALUATE-CHECKED before KDOS has an exception
\ handler.  From this point onward KDOS deliberately shadows that dictionary
\ entry with the same public name.  The wrapper owns CATCH/HANDLER semantics;
\ BIOS owns complete input-frame restoration through EVALUATOR-UNWIND.
\
\ CATCH restores the input addr/len beneath its throw code.  On a caught
\ source exception, consume those restored arguments, retain the exact code in
\ EVAL-THROW, reconstruct every abandoned nested input frame, and return
\ status 5 normally.  Normal source data-stack effects remain untouched.
: EVALUATE-CHECKED  ( addr len -- status )
    EVAL-DEPTH @ >R
    ['] EVALUATE CATCH
    DUP IF
        EVAL-THROW ! 2DROP
        R@ EVALUATOR-UNWIND
        EVAL-S-THROW DUP EVAL-STATUS !
        R> DROP EXIT
    THEN
    DROP R> DROP EVAL-STATUS @ ;

VARIABLE _SEC-CUR
VARIABLE _SEC-REM
VARIABLE _SEC-RAW-LEN
VARIABLE _SEC-EVAL-LEN
VARIABLE _SEC-LINE

\ _SEC-MEASURE ( -- )  Measure the next LF-delimited physical line.
\ _SEC-RAW-LEN includes a trailing CR; _SEC-EVAL-LEN does not.
: _SEC-MEASURE  ( -- )
    _SEC-REM @ 0
    BEGIN
        DUP 2 PICK < IF
            _SEC-CUR @ OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
        ELSE TRUE THEN
    UNTIL
    NIP DUP _SEC-RAW-LEN ! _SEC-EVAL-LEN !
    _SEC-EVAL-LEN @ 0> IF
        _SEC-CUR @ _SEC-EVAL-LEN @ 1- + C@ 13 = IF
            -1 _SEC-EVAL-LEN +!
        THEN
    THEN ;

\ _SEC-ADVANCE ( -- )  Consume the measured line and an LF, if present.
: _SEC-ADVANCE  ( -- )
    _SEC-RAW-LEN @ DUP _SEC-CUR +! NEGATE _SEC-REM +!
    _SEC-REM @ 0> IF
        1 _SEC-CUR +!  -1 _SEC-REM +!
    THEN ;

\ SOURCE-EVALUATE-CHECKED ( addr len -- status )
\
\ Lines are numbered from 1 and columns from 0.  EVAL-LINE,
\ EVAL-COLUMN, and EVAL-TOKEN retain the first failing location/token.
\ As with EVALUATE, source-level data-stack effects are preserved.
: SOURCE-EVALUATE-CHECKED  ( addr len -- status )
    _SEC-REM ! _SEC-CUR !
    0 _SEC-LINE !
    BEGIN _SEC-REM @ 0> WHILE
        1 _SEC-LINE +!
        _SEC-LINE @ EVAL-LINE !
        _SEC-MEASURE
        _SEC-EVAL-LEN @ 0> IF
            _SEC-CUR @ _SEC-EVAL-LEN @ EVALUATE-CHECKED
            DUP EVAL-S-OK <> IF EXIT THEN DROP
        THEN
        _SEC-ADVANCE
    REPEAT
    _SEC-LINE @ EVAL-LINE !
    EVALUATE-FINISH ;

\ Translate checked status back to the loader's THROW surface.  Status 5
\ represents a caught source THROW, whose exact code is retained separately.
: _LD-STATUS-THROW  ( status -- )
    DUP EVAL-S-THROW = IF DROP EVAL-THROW @ THEN THROW ;

\ _LD-WALK checks each bounded physical line independently.  Its private
\ LD-* cursor is frame-saved because a line may execute nested LOAD/REQUIRE.
: _LD-WALK  ( -- )
    LD-BUF @ LD-CUR !
    0 LD-LINE !
    BEGIN LD-SZ @ 0> WHILE
        1 LD-LINE +!
        LD-LINE @ EVAL-LINE !
        LD-SZ @ 0
        BEGIN
            DUP 2 PICK < IF
                LD-CUR @ OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
            ELSE TRUE THEN
        UNTIL
        NIP LD-LEN !
        LD-LEN @ DUP 0> IF
            LD-CUR @ OVER 1- + C@ 13 = IF 1- THEN
        THEN
        DUP 0> IF
            LD-CUR @ SWAP EVALUATE-CHECKED _LD-STATUS-THROW
        ELSE DROP THEN
        LD-LEN @ DUP LD-CUR +! NEGATE LD-SZ +!
        LD-SZ @ 0> IF 1 LD-CUR +! -1 LD-SZ +! THEN
    REPEAT
    EVALUATE-FINISH _LD-STATUS-THROW ;

: _LD-RELEASE  ( -- )
    LD-BUF @ FREE
    _LD-RESTORE ;
\ Every operation after allocation runs inside this guard.  Roll back module
\ identities before their defining dictionary entries, reset compiler state,
\ then release and restore the loader frame before rethrowing the exact code.
: _LD-FAIL  ( exception -- )
    >R
    EVAL-LINE @ >R
    _LD-EVAL-CHECKPOINT EVALUATOR-UNWIND
    _LD-TXN-ROLLBACK
    _LD-ACTIVE-FRAME DUP 72 + @ SWAP 80 + @ DICT-ROLLBACK
    EVALUATOR-RESET
    _LD-RELEASE
    _LD-TXN-AFTER-RELEASE
    R> EVAL-LINE ! R> THROW ;

: _LD-GUARDED  ( xt -- )
    CATCH DUP IF _LD-FAIL THEN DROP
    _LD-TXN-COMMIT
    _LD-RELEASE
    _LD-TXN-AFTER-RELEASE ;

: _LD-WALK-GUARDED  ( -- )
    ['] _LD-WALK _LD-GUARDED ;

: _LD-READ-WALK  ( -- )
    LD-CUR @ _LD-READ-SLOT
    _LD-WALK ;

: LOAD  ( "filename" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    \ Save walker state (including CWD) before resolving path.
    _LD-SAVE
    _RESOLVE-PATH
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR
        _LD-RESTORE EXIT
    THEN
    DUP DIRENT DE.USED DUP 0= IF
        2DROP ."  Empty file" CR
        _LD-RESTORE EXIT
    THEN
    LD-SZ !                              ( slot )
    \ Source text is temporary.  ALLOCATE routes it to XMEM when present;
    \ the full sector-rounded span bounds DMA and is reclaimed after walk.
    DUP _LD-SLOT-BYTES ALLOCATE IF
        2DROP ."  File buffer allocation failed" CR
        _LD-RESTORE EXIT
    THEN
    LD-BUF ! LD-CUR !
    ['] _LD-READ-WALK _LD-GUARDED ;
