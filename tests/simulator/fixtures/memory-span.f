\ =====================================================================
\  memory-span.f - Policy-neutral bounded memory-span predicates
\ =====================================================================
\  These words reason only about half-open address intervals.  They do
\  not decide whether address zero is a valid caller address, whether an
\  empty span is admitted by a particular API, or who owns either span.
\  Callers keep those policies at their public boundary.
\ =====================================================================

PROVIDED akashic-memory-span

REQUIRE uint-range.f

: MSPAN-NONWRAPPING?  ( address length -- flag )
    URANGE-VALID? ;

: MSPAN-OVERLAP?  ( a1 u1 a2 u2 -- flag )
    URANGE-OVERLAP? 0= IF DROP 0 EXIT THEN ;

\ =====================================================================
\  Caller-owned bounded span sets
\ =====================================================================
\  A set stores only borrowed address/length geometry.  It never reads,
\  copies, frees, or otherwise owns the bytes named by an entry.  PUSH is
\  useful for collecting a possibly-overlapping borrowed object graph;
\  ADD additionally requires the new span to be disjoint from every entry.
\
\  Zero-length spans are valid entries and consume one slot, but—as with
\  MSPAN-OVERLAP?—they never overlap another span.  Exact adjacency is
\  disjoint.  All failed mutations leave the set unchanged.

0 CONSTANT MSPAN-SET-S-OK
1 CONSTANT MSPAN-SET-S-INVALID
2 CONSTANT MSPAN-SET-S-OVERLAP
3 CONSTANT MSPAN-SET-S-CAPACITY

16 CONSTANT MSPAN-SET-ENTRY-SIZE
16 CONSTANT MSPAN-SET-HEADER-SIZE

0 CONSTANT _MSS-COUNT
8 CONSTANT _MSS-CAPACITY
16 CONSTANT _MSS-ENTRIES

: MSPAN-SET.COUNT     ( set -- a ) _MSS-COUNT + ;
: _MSPAN-SET.CAPACITY  ( set -- a ) _MSS-CAPACITY + ;
: _MSPAN-SET.ENTRIES   ( set -- a ) _MSS-ENTRIES + ;

\ The inline layout uses one 16-byte header plus 16 bytes per entry.  A
\ result of zero means the requested capacity is negative, would wrap, or
\ would produce a byte count that cannot be a nonnegative Forth length.
-1 5 RSHIFT 1- CONSTANT _MSPAN-SET-CAPACITY-MAX

: MSPAN-SET-BYTES  ( capacity -- bytes|0 )
    DUP 0< IF DROP 0 EXIT THEN
    DUP _MSPAN-SET-CAPACITY-MAX U> IF DROP 0 EXIT THEN
    MSPAN-SET-ENTRY-SIZE * MSPAN-SET-HEADER-SIZE + ;

: MSPAN-SET-COUNT@     ( set -- count ) MSPAN-SET.COUNT @ ;
: MSPAN-SET-CAPACITY@  ( set -- capacity ) _MSPAN-SET.CAPACITY @ ;

: _MSPAN-SET-NTH  ( index set -- entry )
    _MSPAN-SET.ENTRIES SWAP MSPAN-SET-ENTRY-SIZE * + ;

: _MSPAN-SET-ENTRIES-VALID?  ( set -- flag )
    DUP MSPAN-SET-COUNT@ 0 ?DO
        I OVER _MSPAN-SET-NTH DUP @ SWAP 8 + @
        MSPAN-NONWRAPPING? 0= IF DROP 0 UNLOOP EXIT THEN
    LOOP
    DROP -1 ;

: MSPAN-SET-VALID?  ( set -- flag )
    DUP 0= IF DROP 0 EXIT THEN
    DUP MSPAN-SET-HEADER-SIZE MSPAN-NONWRAPPING? 0= IF DROP 0 EXIT THEN
    DUP _MSPAN-SET.CAPACITY @ DUP 0< IF 2DROP 0 EXIT THEN
    DUP MSPAN-SET-BYTES DUP 0= IF 2DROP DROP 0 EXIT THEN
    2 PICK OVER MSPAN-NONWRAPPING? 0= IF 2DROP DROP 0 EXIT THEN
    DROP
    OVER @ DUP 0< IF 2DROP DROP 0 EXIT THEN
    OVER <= 0= IF 2DROP 0 EXIT THEN
    DROP _MSPAN-SET-ENTRIES-VALID? ;

: MSPAN-SET-INIT  ( capacity set -- status )
    >R
    DUP MSPAN-SET-BYTES DUP 0= IF
        2DROP R> DROP MSPAN-SET-S-INVALID EXIT
    THEN
    R@ 0= IF 2DROP R> DROP MSPAN-SET-S-INVALID EXIT THEN
    R@ OVER MSPAN-NONWRAPPING? 0= IF
        2DROP R> DROP MSPAN-SET-S-INVALID EXIT
    THEN
    R@ OVER 0 FILL
    OVER R@ _MSPAN-SET.CAPACITY !
    2DROP R> DROP MSPAN-SET-S-OK ;

: MSPAN-SET-CLEAR  ( set -- status )
    DUP MSPAN-SET-VALID? 0= IF DROP MSPAN-SET-S-INVALID EXIT THEN
    0 SWAP MSPAN-SET.COUNT ! MSPAN-SET-S-OK ;

: MSPAN-SET-OVERLAP?  ( address length set -- flag )
    2 PICK 2 PICK MSPAN-NONWRAPPING? 0= IF 2DROP DROP 0 EXIT THEN
    DUP MSPAN-SET-VALID? 0= IF 2DROP DROP 0 EXIT THEN
    DUP MSPAN-SET-COUNT@ 0 ?DO
        2 PICK 2 PICK I 3 PICK _MSPAN-SET-NTH
        DUP @ SWAP 8 + @ MSPAN-OVERLAP? IF
            DROP 2DROP -1 UNLOOP EXIT
        THEN
    LOOP
    DROP 2DROP 0 ;

: MSPAN-SET-PUSH  ( address length set -- status )
    >R
    2DUP MSPAN-NONWRAPPING? 0= IF
        2DROP R> DROP MSPAN-SET-S-INVALID EXIT
    THEN
    R@ MSPAN-SET-VALID? 0= IF
        2DROP R> DROP MSPAN-SET-S-INVALID EXIT
    THEN
    R@ MSPAN-SET-COUNT@ R@ MSPAN-SET-CAPACITY@ >= IF
        2DROP R> DROP MSPAN-SET-S-CAPACITY EXIT
    THEN
    R@ MSPAN-SET-COUNT@ R@ _MSPAN-SET-NTH >R
    OVER R@ !
    DUP R@ 8 + !
    2DROP R> DROP
    1 R@ MSPAN-SET.COUNT +!
    R> DROP MSPAN-SET-S-OK ;

: MSPAN-SET-ADD  ( address length set -- status )
    >R
    2DUP R@ MSPAN-SET-OVERLAP? IF
        2DROP R> DROP MSPAN-SET-S-OVERLAP EXIT
    THEN
    R> MSPAN-SET-PUSH ;
