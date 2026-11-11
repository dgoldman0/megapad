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
\ Includes CWD, so a directory the loaded text changes to does not outlive
\ the load; _LD-FROM-END gives back the one a path resolution left.  Each
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
\ _LD-READ-SLOT ( addr slot -- )  Concatenate both validated extents at ADDR.
: _LD-READ-SLOT  ( addr slot -- )
    DIRENT SWAP >R
    DUP DE.SEC OVER DE.COUNT R> _LD-READ-RUN
    OVER DE.EXT1-CNT DUP IF
        2 PICK DE.EXT1-SEC SWAP ROT _LD-READ-RUN 2DROP
    ELSE
        DROP 2DROP
    THEN ;
\ ── Relative-path resolution for LOAD / REQUIRE ─────────────────────
\  Paths like "../markup/html.f" or "lib/util.f" are split on '/'.
\  Each intermediate component adjusts CWD (".." goes to parent,
\  anything else CDs into a subdirectory).  The final component
\  (the filename) is left in NAMEBUF for FIND-BY-NAME.  The loader
\  keeps the directory a load started from and gives it back when the
\  load ends (_LD-FROM-SAVE, _LD-FROM-END), so that nested loads always
\  return to the caller's working directory.
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

\ ── Module sources ──────────────────────────────────────────────────
\
\  LOAD and REQUIRE find and read a file through the current module
\  source.  KDOS's own MP64FS is the built-in source.  A filesystem layer
\  that mounts another volume builds a source of its own and selects it
\  with MODULE-SOURCE!, and modules then come from that volume.  The
\  contract is docs/module-source-contract.md.
\
\  A source is five cells, which its owner keeps alive while it is
\  selected or a load that started from it is under way:
\    +0   resolve  ( c-addr u file source -- ior )
\    +8   read     ( addr file source -- ior )
\    +16  dir@     ( source -- token )
\    +24  dir!     ( token source -- )
\    +32  context, for the owner
\  RESOLVE finds the file a path names from the source's current
\  directory, makes that file's directory current, and fills FILE:
\    +0   identity, two cells; both zero when the source has none
\    +16  bytes of source text
\    +24  bytes READ needs at ADDR, at least the text
\    +32  the source's own handle
\  READ follows its RESOLVE with no other call to the source between
\  them, and at most once.  The loader takes DIR@ before it resolves and
\  gives that token back to DIR! exactly once, when the load ends, so a
\  nested load returns to its caller's directory.
40 CONSTANT /MODULE-SOURCE
: MS.RESOLVE  ( source -- addr ) ;
: MS.READ     ( source -- addr )   8 + ;
: MS.DIR@     ( source -- addr )  16 + ;
: MS.DIR!     ( source -- addr )  24 + ;
: MS.CONTEXT  ( source -- addr )  32 + ;
40 CONSTANT /MODULE-FILE
: MF.ID       ( file -- addr ) ;
: MF.USED     ( file -- addr )  16 + ;
: MF.RESERVE  ( file -- addr )  24 + ;
: MF.HANDLE   ( file -- addr )  32 + ;
-4110 CONSTANT MS-E-NOT-FOUND
-4111 CONSTANT MS-E-NO-FILESYSTEM

\ KDOS's MP64FS as a source.  The current directory is CWD, and a file's
\ handle is its directory slot.  Its identity is that slot with the
\ file's first extent, length, time and checksum.  _RESOLVE-PATH walks
\ the path, so RESOLVE leaves it in PATHBUF and NAMEBUF.
VARIABLE _FMS-FILE
: _FS-MS-RESOLVE  ( c-addr u file source -- ior )
    DROP _FMS-FILE ! _PATH!
    FS-ENSURE
    FS-OK @ 0= IF MS-E-NO-FILESYSTEM EXIT THEN
    _RESOLVE-PATH
    FIND-BY-NAME DUP -1 = IF DROP MS-E-NOT-FOUND EXIT THEN
    DUP _FMS-FILE @ MF.HANDLE !
    DUP _LD-SLOT-BYTES _FMS-FILE @ MF.RESERVE !
    DUP DIRENT                          ( slot de )
    DUP DE.USED _FMS-FILE @ MF.USED !
    DUP DE.CRC 32 LSHIFT OVER DE.USED OR _FMS-FILE @ MF.ID !
    DUP DE.MTIME 32 LSHIFT SWAP DE.SEC 16 LSHIFT OR OR
    _FMS-FILE @ MF.ID 8 + !
    0 ;
: _FS-MS-READ  ( addr file source -- ior )
    DROP MF.HANDLE @ _LD-READ-SLOT 0 ;
: _FS-MS-DIR@  ( source -- token )  DROP CWD @ ;
: _FS-MS-DIR!  ( token source -- )  DROP CWD ! ;
CREATE FS-MODULE-SOURCE
    ' _FS-MS-RESOLVE ,  ' _FS-MS-READ ,
    ' _FS-MS-DIR@ ,     ' _FS-MS-DIR! ,
    0 ,
VARIABLE MODULE-SOURCE
FS-MODULE-SOURCE MODULE-SOURCE !

\ MODULE-SOURCE! ( source -- )  Select the source that later loads use.
\   A load under way when the selection changes leaves it in place when it
\   ends; a load that fails selects the source it started from again.
: MODULE-SOURCE!  ( source -- )
    ?CORE0 MODULE-SOURCE ! ;

\ The file the current source resolved last.  The loader reads it before
\ it evaluates anything, so nested loads may fill the record again.
CREATE _LD-FILE /MODULE-FILE ALLOT
: _LD-PATH  ( -- c-addr u )
    PATHBUF 0
    BEGIN
        DUP 127 < IF 2DUP + C@ 0<> ELSE FALSE THEN
    WHILE 1+ REPEAT ;
: _LD-RESOLVE  ( -- ior )
    _LD-FILE /MODULE-FILE 0 FILL
    _LD-PATH _LD-FILE MODULE-SOURCE @ DUP MS.RESOLVE @ EXECUTE ;
: _LD-READ  ( -- )
    LD-BUF @ _LD-FILE MODULE-SOURCE @ DUP MS.READ @ EXECUTE THROW ;
\ _LD-FOUND? ( ior module? -- flag )  True when RESOLVE found the file.
\   A missing filesystem and a path that names no file are reported
\   here, for LOAD or for REQUIRE; any other failure is thrown.
: _LD-FOUND?  ( ior module? -- flag )
    OVER 0= IF 2DROP TRUE EXIT THEN
    OVER MS-E-NO-FILESYSTEM = IF
        2DROP ."  No filesystem" CR FALSE EXIT
    THEN
    OVER MS-E-NOT-FOUND <> IF DROP THROW THEN
    NIP IF ."  Module not found: " ELSE ."  Not found: " THEN
    PATHBUF .ZSTR CR FALSE ;

\ Each LOAD and REQUIRE keeps the source and directory it started from,
\ and gives the directory back to that source when it ends.  A source the
\ load selected stays selected when the load succeeds; a failed load
\ selects the source it started from again.
CREATE _LD-FROM-STK _LD-MAXLVL 16 * ALLOT
VARIABLE _LD-FROM-SP
0 _LD-FROM-SP !
: _LD-FROM-SAVE  ( -- )
    _LD-FROM-SP @ _LD-MAXLVL 16 * >= ABORT" REQUIRE nested too deep"
    MODULE-SOURCE @ DUP _LD-FROM-SP @ _LD-FROM-STK + !
    DUP MS.DIR@ @ EXECUTE _LD-FROM-SP @ _LD-FROM-STK + 8 + !
    16 _LD-FROM-SP +! ;
\ _LD-FROM-END ( exception -- )  End one load and rethrow what it threw.
: _LD-FROM-END  ( exception -- )
    _LD-FROM-SP @ 0= ABORT" REQUIRE nesting underflow"
    -16 _LD-FROM-SP +!
    _LD-FROM-SP @ _LD-FROM-STK +        ( exception entry )
    OVER IF DUP @ MODULE-SOURCE ! THEN
    DUP 8 + @ SWAP @ DUP MS.DIR! @ EXECUTE
    THROW ;

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

\ BIOS EVALUATE keeps each caller's input frame outside the return stack, so
\ a THROW that crosses it leaves the frame, and the evaluator's depth, in
\ place.  BIOS owns complete input-frame restoration through
\ EVALUATOR-UNWIND; KDOS owns CATCH/HANDLER semantics, and so it owns giving
\ the evaluator its depth back when an exception passes.  This word does that
\ once for both evaluator words below: it notes the depth, runs BIOS EVALUATE
\ under CATCH, and on an exception reconstructs every abandoned input frame
\ down to the depth it noted.  CATCH has restored addr/len beneath the code.
: _EVALUATE-GUARDED  ( i*x addr len -- j*x 0 | i*x addr len exception )
    EVAL-DEPTH @ >R
    ['] EVALUATE CATCH
    DUP IF R@ EVALUATOR-UNWIND THEN
    R> DROP ;

\ From this point onward KDOS deliberately shadows the BIOS entries EVALUATE
\ and EVALUATE-CHECKED with words of the same public names.
\
\ EVALUATE rethrows what it guarded.  Whoever catches that exception, at any
\ distance, finds the evaluator at the depth it had when this word was
\ entered, as WITH-GUARD's and the loader's callers find their state.
: EVALUATE  ( i*x addr len -- j*x )
    _EVALUATE-GUARDED THROW ;

\ EVALUATE-CHECKED reports the exception as a status.  On a caught source
\ exception, consume the restored arguments, retain the exact code in
\ EVAL-THROW, and return status 5 normally.  Normal source data-stack effects
\ remain untouched.
: EVALUATE-CHECKED  ( addr len -- status )
    _EVALUATE-GUARDED
    DUP IF
        EVAL-THROW ! 2DROP
        EVAL-S-THROW DUP EVAL-STATUS !
        EXIT
    THEN
    DROP EVAL-STATUS @ ;

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
    _LD-READ
    _LD-WALK ;

: _LOAD-BODY  ( -- )
    _LD-RESOLVE FALSE _LD-FOUND? 0= IF EXIT THEN
    _LD-FILE MF.USED @ 0= IF ."  Empty file" CR EXIT THEN
    _LD-SAVE
    _LD-FILE MF.USED @ LD-SZ !
    \ Source text is temporary.  ALLOCATE routes it to XMEM when present;
    \ the span the source asks for bounds its read and is reclaimed after
    \ the walk.
    _LD-FILE MF.RESERVE @ ALLOCATE IF
        DROP ."  File buffer allocation failed" CR
        _LD-RESTORE EXIT
    THEN
    LD-BUF !
    ['] _LD-READ-WALK _LD-GUARDED ;

: LOAD  ( "filename" -- )
    PARSE-NAME
    _LD-FROM-SAVE
    ['] _LOAD-BODY CATCH _LD-FROM-END ;
