\ NAMEBUF -- 24-byte scratch for single filename component (dirent name).
\   Used by FIND-BY-NAME, MKFILE, RENAME, etc. — always ≤ 23 chars.
VARIABLE NAMEBUF  23 ALLOT

\ PATHBUF -- 128-byte scratch for full paths including '/' separators.
\   Populated by PARSE-NAME alongside NAMEBUF.  Used by _RESOLVE-PATH
\   so that paths like "lib/crypto/aes.f" (>23 chars total) are preserved.
VARIABLE PATHBUF  127 ALLOT

\ PARSE-NAME ( "name" -- )
\   Parse next whitespace-delimited word.  Stores full path (up to 127
\   chars) in PATHBUF, and the first 23 chars in NAMEBUF (for direct
\   dirent lookups).  Sets PN-LEN to the clamped NAMEBUF length.
VARIABLE PN-LEN

: PARSE-NAME  ( "name" -- )
    NAMEBUF 24 0 FILL
    PATHBUF 128 0 FILL
    BL WORD DUP C@                     ( waddr rawlen )
    DUP 127 MIN >R                     \ R: pathlen (up to 127)
    DROP 1+                             ( src )  \ drop rawlen, skip count byte
    DUP PATHBUF R@ CMOVE               \ copy full path into PATHBUF
    R> 23 MIN PN-LEN !                  \ clamp for NAMEBUF
    NAMEBUF PN-LEN @                    ( src dst len )
    CMOVE ;

\ -- Stack safety utilities --

\ NEEDS ( n -- )  abort if stack has fewer than n items
: NEEDS  ( n -- )
    DEPTH 1 - >  ABORT" Stack underflow" ;

\ ASSERT ( flag -- )  abort if flag is false
: ASSERT  ( flag -- )
    0= ABORT" Assertion failed" ;

\ ['] — use BIOS primitive (includes reloc_record for binimg support)


\ .DEPTH ( -- )  show current stack depth
: .DEPTH  ( -- )  ."  [" DEPTH . ."  deep]" ;

\ 0>= ( x -- flag )  true if x ≥ 0
: 0>=  ( x -- flag )  0< INVERT ;

