\ =====================================================================
\  §1.3  CRC Convenience Words
\ =====================================================================
\
\  The BIOS provides one raw capability query and eight checked CRC words:
\    CRYPTO-CAPS@   ( -- caps )
\    CRC-MODE!      ( mode -- status )
\    CRC-RESET      ( -- status )
\    CRC-INIT!      ( seed -- status )
\    CRC-FEED       ( cell -- status )
\    CRC-FEED-BYTE  ( byte -- status )
\    CRC@           ( -- raw status )
\    CRC-RAW-FINAL@ ( -- raw status )
\    CRC-FINAL@     ( -- finalized )
\
\  Convenience operations throw an unchanged nonzero checked status.  They
\  keep their result-only public shape while the primitives remain available
\  to code that wants explicit retry or error policy.

: _CRC-REQUIRE-OK  ( status -- )  ?DUP IF THROW THEN ;

\ _CRC-BUF-CHECKED ( addr u -- status )  Non-throwing exact buffer feed.
\   The status form lets storage and diagnostic callers release only a CRC
\   transaction that they actually acquired.  It processes full 8-byte
\   chunks via CRC-FEED, then feeds each remaining byte exactly once.
: _CRC-BUF-CHECKED  ( addr u -- status )
    \ Process full 8-byte chunks using BEGIN/WHILE/REPEAT
    BEGIN  DUP 8 >=  WHILE
        OVER @ CRC-FEED ?DUP IF
            >R 2DROP R> EXIT
        THEN
        SWAP 8 + SWAP
        8 -
    REPEAT
    \ Remaining bytes: 0..7
    BEGIN  DUP 0 >  WHILE
        OVER C@ CRC-FEED-BYTE ?DUP IF
            >R 2DROP R> EXIT
        THEN
        SWAP 1+ SWAP
        1-
    REPEAT
    2DROP 0
;

\ CRC-BUF ( addr u -- )  Throw an unchanged checked feed failure.
: CRC-BUF  ( addr u -- )  _CRC-BUF-CHECKED _CRC-REQUIRE-OK ;

\ CRC32-BUF ( addr u -- crc )  Compute CRC-32 of a buffer.
: CRC32-BUF
    0 CRC-MODE! _CRC-REQUIRE-OK
    0xFFFFFFFF CRC-INIT! _CRC-REQUIRE-OK
    CRC-BUF
    CRC-FINAL@ ;

\ CRC32C-BUF ( addr u -- crc )  Compute standard reflected CRC-32C.
: CRC32C-BUF
    5 CRC-MODE! _CRC-REQUIRE-OK
    0xFFFFFFFF CRC-INIT! _CRC-REQUIRE-OK
    CRC-BUF
    CRC-FINAL@ ;

\ CRC64-BUF ( addr u -- crc )  Compute CRC-64/WE of a buffer.
: CRC64-BUF
    2 CRC-MODE! _CRC-REQUIRE-OK
    0xFFFFFFFFFFFFFFFF CRC-INIT! _CRC-REQUIRE-OK
    CRC-BUF
    CRC-FINAL@ ;

\ CRC32-STR ( c-addr u -- crc )  CRC-32 of a counted/addr+len string.
\   Same as CRC32-BUF, just an alias for readability.
: CRC32-STR  CRC32-BUF ;

\ .CRC32 ( addr u -- )  Print CRC-32 of buffer in hex.
: .CRC32  CRC32-BUF BASE @ SWAP HEX U. BASE ! ;

\ =====================================================================
\  §1.4  Hardware Diagnostics
\ =====================================================================
\
\  Wrapper words for the 18 BIOS diagnostic primitives:
\    PERF-CYCLES, PERF-STALLS, PERF-TILEOPS, PERF-EXTMEM, PERF-RESET
\    BIST-FULL, BIST-QUICK, BIST-STATUS, BIST-FAIL-ADDR, BIST-FAIL-DATA
\    TILE-TEST, TILE-TEST@, TILE-DETAIL@
\    ICACHE-ON, ICACHE-OFF, ICACHE-INV, ICACHE-HITS, ICACHE-MISSES

\ The CRC diagnostic is intentionally implemented through the checked BIOS
\ surface, not through a software oracle.  One quad plus one byte exercises
\ the exact mixed-fragment path for the canonical "123456789" vector.
CREATE _CRC-DIAG-DATA
  49 C, 50 C, 51 C, 52 C, 53 C, 54 C, 55 C, 56 C, 57 C,
VARIABLE _CRC-DIAG-RAW
VARIABLE _CRC-DIAG-EXPECT

: _CRC-DIAG-RUN?  ( mode raw-final? -- value status )
    _CRC-DIAG-RAW !
    CRC-MODE! ?DUP IF 0 SWAP EXIT THEN
    CRC-RESET ?DUP IF
        >R CRC-FINAL@ DROP 0 R> EXIT
    THEN
    _CRC-DIAG-DATA 9 _CRC-BUF-CHECKED ?DUP IF
        >R CRC-FINAL@ DROP 0 R> EXIT
    THEN
    _CRC-DIAG-RAW @ IF
        CRC-RAW-FINAL@
        DUP IF
            >R DROP CRC-FINAL@ DROP 0 R>
        THEN
    ELSE
        CRC-FINAL@ 0
    THEN ;

: _CRC-DIAG-ONE  ( mode expected raw-final? -- flag )
    _CRC-DIAG-RAW !
    _CRC-DIAG-EXPECT !
    _CRC-DIAG-RAW @ _CRC-DIAG-RUN?
    DUP IF 2DROP FALSE EXIT THEN
    DROP _CRC-DIAG-EXPECT @ = ;

: CRC-DIAG?  ( -- flag )
    0 0xFC891918 FALSE _CRC-DIAG-ONE
    1 0x05440F15 FALSE _CRC-DIAG-ONE AND
    2 0x62EC59E3F1A4F00A FALSE _CRC-DIAG-ONE AND
    4 0xCBF43926 FALSE _CRC-DIAG-ONE AND
    5 0xE3069283 FALSE _CRC-DIAG-ONE AND
    6 0x995DC9BBDF1939FA FALSE _CRC-DIAG-ONE AND
    5 0x1CF96D7C TRUE _CRC-DIAG-ONE AND ;

: .CRC-DIAG  ( -- )
    CR ."   CRC Standard Vectors" CR
    CRC-DIAG? IF
        ."     PASS (modes 0,1,2,4,5,6 and mode-5 raw)" CR
    ELSE
        ."     FAIL (busy, unsupported, or vector mismatch)" CR
    THEN ;

