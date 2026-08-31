\ =====================================================================
\  §1.6  SHA-3 / SHAKE Hashing
\ =====================================================================
\  Checked BIOS primitives (hardware accelerator at MMIO 0x0780):
\    SHA3-BEGIN  SHA3-UPDATE  SHA3-FINAL  SHAKE-FINAL
\    SHAKE-READ  SHA3-CLEAR  SHA3-STATUS@  SHA3-MODE@
\  BIOS TRNG (hardware at MMIO 0x0800):
\    RANDOM  RANDOM8  SEED-RNG
\
\  Modes: 0=SHA3-256  1=SHA3-512  2=SHAKE128  3=SHAKE256

0 CONSTANT SHA3-256-MODE
1 CONSTANT SHA3-512-MODE
2 CONSTANT SHAKE128-MODE
3 CONSTANT SHAKE256-MODE

\ Common checked-crypto status namespace.
0 CONSTANT CRYPTO-OK
1 CONSTANT CRYPTO-UNSUPPORTED
2 CONSTANT CRYPTO-STATE
3 CONSTANT CRYPTO-RANGE
4 CONSTANT CRYPTO-PROTECTED
5 CONSTANT CRYPTO-TIMEOUT
6 CONSTANT CRYPTO-HARDWARE

2 CONSTANT CRYPTO-CAP-SHA3-STREAM
4 CONSTANT CRYPTO-CAP-KECCAK-F1600

\ Convert CALLER-SPAN-STATUS values to the common crypto namespace.
: _CRYPTO-SPAN-STATUS ( addr len -- status )
    CALLER-SPAN-STATUS
    DUP 2 = IF DROP CRYPTO-RANGE EXIT THEN
    DUP 3 = IF DROP CRYPTO-PROTECTED THEN ;

\ SHA3 ( addr len out -- status )  SHA3-256 hash (32 bytes output).
: SHA3  ( addr len out -- status )
    >R
    SHA3-256-MODE SHA3-BEGIN DUP IF
        >R 2DROP R> R> DROP EXIT
    THEN DROP
    SHA3-UPDATE DUP IF
        R> DROP EXIT
    THEN DROP
    R> SHA3-FINAL ;

\ SHA3-512 ( addr len out -- status )  SHA3-512 hash (64 bytes output).
: SHA3-512  ( addr len out -- status )
    >R
    SHA3-512-MODE SHA3-BEGIN DUP IF
        >R 2DROP R> R> DROP EXIT
    THEN DROP
    SHA3-UPDATE DUP IF
        R> DROP EXIT
    THEN DROP
    R> SHA3-FINAL ;

\ Preserve the first error when cleanup succeeds.  A cleanup failure takes
\ precedence because the guard remains deliberately held fail-closed.
: _SHAKE-CLEAN-ERROR ( status -- status )
    >R
    SHA3-CLEAR
    DUP IF R> DROP EXIT THEN
    DROP R> ;

: (SHAKE) ( addr len out outlen mode -- status )
    DUP SHA3-BEGIN
    DUP IF >R 2DROP 2DROP DROP R> EXIT THEN DROP DROP
    \ Preflight the complete output so multi-window calls cannot publish a
    \ prefix before discovering a bad later address.
    2DUP _CRYPTO-SPAN-STATUS
    DUP IF >R 2DROP 2DROP R> _SHAKE-CLEAN-ERROR EXIT THEN DROP
    2SWAP SHA3-UPDATE
    DUP IF >R 2DROP R> EXIT THEN DROP
    SHAKE-FINAL
    DUP IF >R 2DROP R> EXIT THEN DROP
    BEGIN DUP 0> WHILE
        OVER OVER 32 MIN SHAKE-READ
        DUP IF >R 2DROP R> EXIT THEN DROP
        DUP 32 MIN >R
        R@ -
        SWAP R> + SWAP
    REPEAT
    2DROP
    SHA3-CLEAR ;

: SHAKE128  ( addr len out outlen -- status )
    SHAKE128-MODE (SHAKE) ;

: SHAKE256  ( addr len out outlen -- status )
    SHAKE256-MODE (SHAKE) ;

\ SHAKE-STREAM ( addr blocks -- status )
\ Read 32-byte chunks from an already-finalized checked SHAKE transaction,
\ then clear it on every success or failure path.
: SHAKE-STREAM ( addr blocks -- status )
    \ Probe with a zero-length read first: continuation ownership, capability,
    \ and phase have priority over scalar or destination inspection.
    OVER 0 SHAKE-READ
    DUP IF >R 2DROP R> EXIT THEN DROP
    DUP 0< IF
        2DROP CRYPTO-RANGE _SHAKE-CLEAN-ERROR EXIT
    THEN
    \ Qualify the whole destination before publishing the first chunk.  Reject
    \ a left shift that would lose high bits; a sign-bit result is rejected by
    \ the shared span checker.  This derives the bound from caller address
    \ arithmetic rather than imposing a private block-count capacity.
    DUP 59 RSHIFT IF
        2DROP CRYPTO-RANGE _SHAKE-CLEAN-ERROR EXIT
    THEN
    2DUP 5 LSHIFT _CRYPTO-SPAN-STATUS
    DUP IF >R 2DROP R> _SHAKE-CLEAN-ERROR EXIT THEN DROP
    BEGIN DUP 0> WHILE
        OVER 32 SHAKE-READ
        DUP IF >R 2DROP R> EXIT THEN DROP
        SWAP 32 + SWAP 1-
    REPEAT
    2DROP SHA3-CLEAR ;

: .SHA3-STATUS
    SHA3-STATUS@
    3 AND
    DUP 0 = IF DROP ."  SHA3: idle" CR ELSE
    DUP 1 = IF DROP ."  SHA3: busy" CR ELSE
    DUP 2 = IF DROP ."  SHA3: done" CR ELSE
    DROP ."  SHA3: error" CR
    THEN THEN THEN ;

: .SHA3  ( addr len -- )
    0 DO
        DUP I + C@
        DUP 4 RSHIFT
        DUP 10 < IF 48 + ELSE 55 + THEN EMIT
        15 AND
        DUP 10 < IF 48 + ELSE 55 + THEN EMIT
    LOOP DROP ;

: RANDOM32  ( -- u )
    RANDOM 0xFFFFFFFF AND ;

: RANDOM16  ( -- u )
    RANDOM 0xFFFF AND ;

: RAND-RANGE  ( max -- n )
    RANDOM SWAP MOD ABS ;

