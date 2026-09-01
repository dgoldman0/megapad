\ ── Application Loading ──────────────────────────────────────────────
\  APP-EVAL evaluates a string.  ENTER-USER / SYS-EXIT are retained
\  as no-ops for API compatibility (hardware user mode was removed
\  because it conflicted with 1802-heritage SEP/SEX dispatch).
\
\  MPU setup (_APP-MPU-ON / _APP-MPU-OFF) is retained but currently
\  inert since MPU is gated on priv_level which is always 0.
\
\  LOAD / FSLOAD remain for OS modules and drivers.

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
    LD-BUF @
    LD-SZ @
    BEGIN DUP 0> WHILE
        OVER
        2 PICK
        0
        BEGIN
            DUP 2 PICK < IF
                OVER OVER + C@ 10 = IF
                    TRUE
                ELSE
                    1+ FALSE
                THEN
            ELSE TRUE THEN
        UNTIL
        NIP
        DUP 0> IF
            2DUP EVALUATE
        THEN
        1+
        ROT OVER - >R
        + SWAP DROP
        R>
    REPEAT
    2DROP ;

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
    LD-BUF !
    _LD-READ-SLOT
    \ Configure MPU (Bank 0 + ext mem visible) and enter user mode
    _APP-MPU-ON
    ENTER-USER
    ['] _APP-LOAD-WALK CATCH
    DUP IF
        SYS-EXIT _APP-MPU-OFF
        _LD-EVAL-CHECKPOINT EVALUATOR-UNWIND
        _LD-TXN-ROLLBACK
        _LD-RELEASE
        _LD-TXN-AFTER-RELEASE
        THROW
    THEN
    DROP
    SYS-EXIT
    _APP-MPU-OFF
    _LD-TXN-COMMIT
    _LD-RELEASE
    _LD-TXN-AFTER-RELEASE ;

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

