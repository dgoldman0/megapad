\ ── Application Loading ──────────────────────────────────────────────
\  APP-EVAL evaluates a string.  ENTER-USER / SYS-EXIT are retained
\  as no-ops for API compatibility (hardware user mode was removed
\  because it conflicted with 1802-heritage SEP/SEX dispatch).
\
\  MPU setup (_APP-MPU-ON / _APP-MPU-OFF) is retained but currently
\  inert since MPU is gated on priv_level which is always 0.
\
\  LOAD / FSLOAD remain for OS modules and drivers.
\ Application loader setup and checked execution follow.
\ _APP-MPU-ON ( -- )  set MPU window to cover Bank 0 + ext mem
: _APP-MPU-ON  ( -- )
    0 MPU-BASE!
    XMEM? IF
        EXT-MEM-BASE EXT-MEM-SIZE + MPU-LIMIT!
    ELSE
        MEM-SIZE MPU-LIMIT!
    THEN ;

\ _APP-MPU-OFF ( -- )  disable MPU (supervisor mode)
: _APP-MPU-OFF  ( -- )
    0 MPU-BASE!  0 MPU-LIMIT! ;

: APP-EVAL  ( addr u -- )
    _APP-MPU-ON
    ENTER-USER EVALUATE SYS-EXIT
    _APP-MPU-OFF ;

: _APP-LOAD-WALK  ( -- )
    _LD-WALK ;

: _APP-LOAD-USER  ( -- )
    _APP-MPU-ON ENTER-USER _APP-LOAD-WALK ;

: _APP-LOAD-RUN  ( -- )
    LD-CUR @ _LD-READ-SLOT
    ['] _APP-LOAD-USER CATCH
    SYS-EXIT
    _APP-MPU-OFF
    THROW ;

: APP-LOAD  ( "filename" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DUP DIRENT DE.USED DUP 0= IF
        2DROP ."  Empty file" CR EXIT
    THEN
    _LD-SAVE
    LD-SZ !
    DUP _LD-SLOT-BYTES ALLOCATE IF
        2DROP ."  File buffer allocation failed" CR
        _LD-RESTORE EXIT
    THEN
    LD-BUF ! LD-CUR !
    ['] _APP-LOAD-RUN _LD-GUARDED ;

\ APP-LOAD intentionally uses the same checked physical-line walker and the
\ same transaction guard as LOAD and REQUIRE.  It does not copy the complete
\ source into SOURCE-EVALUATE-CHECKED because that helper's _SEC-* cursor is
\ global and would be overwritten by a nested REQUIRE.
\
\ The loader frame instead owns every mutable cursor value needed to resume an
\ outer application after nested loading.  A nonempty line has one terminal CR
\ removed, receives EVALUATE-CHECKED, and prevents all later lines from running
\ when its status is nonzero.  A clean EOF receives EVALUATE-FINISH before the
\ transaction can commit.
\
\ _APP-LOAD-RUN places extent transfer under _LD-GUARDED but delays MPU setup
\ until that transfer succeeds.  Once user execution starts, normal return or
\ catchable THROW reaches the inner CATCH, which performs SYS-EXIT and disables
\ the compatibility MPU window before passing the result to the common guard.
\
\ The common failure path restores evaluator depth, provisional module state,
\ dictionary HERE/LATEST, compiler bookkeeping, the transfer allocation, the
\ loader frame, and ambient CWD before rethrow.  Source status 5 maps back to
\ EVAL-THROW; statuses 1 through 4 retain their checked status values.
\
\ These are lifecycle guarantees, not application isolation.  Completed UART,
\ storage, device, or writes to objects that predate the loader checkpoint are
\ not undone.  APP-LOAD also intentionally retains direct current-directory
\ lookup and the existing public parsing/stack contract.
\
\ A successful nested module remains provisional to every enclosing loader
\ transaction until the outermost source completes.  This keeps its registry
\ identity and dictionary definitions in the same rollback closure.
\ Public LOAD, APP-LOAD, REQUIRE, and PROVIDED stack effects remain unchanged;
\ the transaction machinery is deliberately private loader infrastructure.

\ -- ANSI helpers (canonical definitions; used by .DOC-CHUNK and §9) --
: ESC   ( -- )  27 EMIT ;
: CSI   ( -- )  ESC 91 EMIT ;
: .N  ( n -- )
    DUP 0< IF 45 EMIT NEGATE THEN
    DUP 10 < IF
        48 + EMIT
    ELSE DUP 100 < IF
        DUP 10 / 48 + EMIT
        10 MOD 48 + EMIT
    ELSE
        DUP 1000 < IF
            DUP 100 / 48 + EMIT
            DUP 10 / 10 MOD 48 + EMIT
            10 MOD 48 + EMIT
        ELSE
            .
        THEN
    THEN THEN ;
: SGR      ( n -- )  CSI .N 109 EMIT ;
: RESET-COLOR  ( -- )  0 SGR ;
: DIM      ( -- )  2 SGR ;

