\ =====================================================================
\  §20  Module System
\ =====================================================================
\
\ Module identities are exact, case-sensitive evaluator tokens.  They are
\ independent of NAMEBUF and its MP64FS component limit.  Stable registry
\ entries own their complete ID bytes in the Bank-0 heap, so XMEM-RESET and
\ userland transitions cannot invalidate them.  A small inline bucket vector
\ is only a performance seed; chained entries are limited by available heap
\ memory, not by bucket count.
\
\ A loader pre-registers the first PROVIDED identity before evaluating any
\ source.  This remains the cycle-breaking rule.  Every new identity declared
\ while that source is active joins the loader frame's provisional list.
\ Successful evaluation commits the whole list; a throw unlinks and frees the
\ whole list before the source allocation is released and the error rethrown.
\
\ All public module operations are core-0-only.  Registry operations use
\ HT-LOCK, but never hold it while allocating, freeing, or evaluating source.

16 CONSTANT _MOD-INLINE-BUCKETS
CREATE _MOD-INLINE  _MOD-INLINE-BUCKETS CELLS ALLOT
_MOD-INLINE _MOD-INLINE-BUCKETS CELLS 0 FILL

\ Registry descriptor: bucket pointer, bucket count, entry count,
\ heap-owned-bucket flag, lock number.
CREATE _MOD-REG
    _MOD-INLINE ,
    _MOD-INLINE-BUCKETS ,
    0 ,
    0 ,
    HT-LOCK ,

: _MOD-BUCKETS       ( -- addr )  _MOD-REG      @ ;
: _MOD-BUCKET-COUNT  ( -- n )     _MOD-REG  8 + @ ;
: _MOD-COUNT         ( -- n )     _MOD-REG 16 + @ ;
: _MOD-BUCKETS-HEAP? ( -- flag )  _MOD-REG 24 + @ ;
: _MOD-LOCK          ( -- n )     _MOD-REG 32 + @ ;

\ Stable entry: bucket-next, provisional-next, hash, exact length, ID bytes.
32 CONSTANT /MOD-NODE
: _MN-NEXT  ( node -- addr ) ;
: _MN-PROV  ( node -- addr )  8 + ;
: _MN-HASH  ( node -- addr ) 16 + ;
: _MN-LEN   ( node -- addr ) 24 + ;
: _MN-ID    ( node -- addr ) 32 + ;

\ Private allocation seam.  Production bindings deliberately use the Bank-0
\ heap; DEFER keeps deterministic entry/rehash failure qualification possible.
DEFER _MOD-ALLOCATE
DEFER _MOD-FREE
' DMA-ALLOCATE IS _MOD-ALLOCATE
' DMA-FREE     IS _MOD-FREE

-4100 CONSTANT _MOD-E-NOMEM
-4101 CONSTANT _MOD-E-BAD-ID
255 CONSTANT _MOD-EVAL-LINE-MAX
_MOD-EVAL-LINE-MAX 9 - CONSTANT _MOD-ID-MAX  \ minus "PROVIDED "

\ Private nonthrowing FNV-1a hash.  The 32-bit mask keeps MOD input positive.
: _MOD-HASH  ( addr len -- hash )
    0x811C9DC5 -ROT
    OVER + SWAP ?DO
        I C@ XOR 0x01000193 * 0xFFFFFFFF AND
    LOOP ;

: _MOD-BUCKET  ( hash -- bucket-addr )
    _MOD-BUCKET-COUNT MOD CELLS _MOD-BUCKETS + ;

\ Lookup scratch is written only after _MOD-LOCK is held.  No word called by
\ the locked walk allocates, frees, yields, or throws.
VARIABLE _MF-A
VARIABLE _MF-U
VARIABLE _MF-H

: _MOD-FIND-LOCKED  ( id-addr id-len hash -- node | 0 )
    _MF-H ! _MF-U ! _MF-A !
    _MF-H @ _MOD-BUCKET @
    BEGIN DUP WHILE
        DUP _MN-HASH @ _MF-H @ = IF
            DUP _MN-LEN @ _MF-U @ = IF
                DUP _MN-ID _MF-U @ _MF-A @ _MF-U @ COMPARE 0= IF
                    EXIT
                THEN
            THEN
        THEN
        @
    REPEAT ;

: _MOD-FIND  ( id-addr id-len -- node | 0 )
    2DUP _MOD-HASH >R
    _MOD-LOCK LOCK
    R> _MOD-FIND-LOCKED
    _MOD-LOCK UNLOCK ;

\ _MOD-INSERT ( id-addr id-len -- node inserted? ior )
\ Publish only a complete node.  Duplicate lookup occurs before allocation and
\ is repeated under the publication lock, so duplicates remain allocation-free.
: _MOD-INSERT  ( id-addr id-len -- node inserted? ior )
    ?CORE0
    DUP 0= OVER _MOD-ID-MAX > OR IF
        2DROP 0 FALSE _MOD-E-BAD-ID EXIT
    THEN
    2DUP _MOD-HASH                         ( id-addr id-len hash )
    _MOD-LOCK LOCK
    2 PICK 2 PICK 2 PICK _MOD-FIND-LOCKED ( id-addr id-len hash node )
    _MOD-LOCK UNLOCK
    DUP IF
        >R 2DROP DROP R> FALSE 0 EXIT
    THEN
    DROP                                    ( id-addr id-len hash )

    OVER /MOD-NODE + _MOD-ALLOCATE IF
        DROP 2DROP DROP 0 FALSE _MOD-E-NOMEM EXIT
    THEN                                    ( id-addr id-len hash candidate )
    0 OVER !
    0 OVER _MN-PROV !
    OVER OVER _MN-HASH !
    2 PICK OVER _MN-LEN !
    3 PICK OVER _MN-ID 4 PICK CMOVE

    _MOD-LOCK LOCK
    3 PICK 3 PICK 3 PICK _MOD-FIND-LOCKED  ( ... candidate node )
    DUP IF
        _MOD-LOCK UNLOCK
        >R _MOD-FREE 2DROP DROP R> FALSE 0 EXIT
    THEN
    DROP                                    ( id-addr id-len hash candidate )
    OVER _MOD-BUCKET                        ( ... candidate bucket-addr )
    DUP @ 2 PICK _MN-NEXT !
    OVER SWAP !
    1 _MOD-REG 16 + +!
    _MOD-LOCK UNLOCK
    >R 2DROP DROP R> TRUE 0 ;

\ Best-effort retained growth.  Load factor may affect lookup time but never
\ entry capacity.  Nodes stay at stable addresses while only bucket links move.
VARIABLE _MG-NEW
VARIABLE _MG-N
VARIABLE _MG-NODE
VARIABLE _MG-NEXT

: _MOD-GROW-TARGET  ( -- buckets | 0 )
    _MOD-LOCK LOCK
    _MOD-BUCKET-COUNT DUP
    BEGIN _MOD-COUNT OVER 2* > WHILE 2* REPEAT
    2DUP = IF 2DROP 0 ELSE NIP THEN
    _MOD-LOCK UNLOCK ;

: _MOD-MAYBE-GROW  ( -- )
    _MOD-GROW-TARGET DUP 0= IF DROP EXIT THEN
    DUP CELLS _MOD-ALLOCATE IF 2DROP EXIT THEN  ( target candidate )
    DUP 2 PICK CELLS 0 FILL

    _MOD-LOCK LOCK
    _MOD-BUCKET-COUNT 2 PICK >=
    _MOD-COUNT _MOD-BUCKET-COUNT 2* <= OR IF
        _MOD-LOCK UNLOCK
        NIP _MOD-FREE EXIT
    THEN
    _MG-NEW ! _MG-N !

    _MOD-BUCKET-COUNT 0 DO
        _MOD-BUCKETS I CELLS + @ _MG-NODE !
        BEGIN _MG-NODE @ WHILE
            _MG-NODE @ _MN-NEXT @ _MG-NEXT !
            _MG-NODE @ _MN-HASH @ _MG-N @ MOD CELLS _MG-NEW @ +
            DUP @ _MG-NODE @ _MN-NEXT !
            _MG-NODE @ SWAP !
            _MG-NEXT @ _MG-NODE !
        REPEAT
    LOOP

    _MOD-BUCKETS _MOD-BUCKETS-HEAP?       ( old-buckets old-heap? )
    _MG-NEW @ _MOD-REG !
    _MG-N @ _MOD-REG 8 + !
    1 _MOD-REG 24 + !
    _MOD-LOCK UNLOCK
    IF _MOD-FREE ELSE DROP THEN ;

VARIABLE _MOD-GROW-PENDING
VARIABLE _MOD-GROW-READY
0 _MOD-GROW-PENDING !
0 _MOD-GROW-READY !

: _MOD-TRY-PENDING-GROWTH  ( -- )
    _MOD-GROW-PENDING @ IF
        _MOD-MAYBE-GROW
        _MOD-GROW-TARGET 0= IF 0 _MOD-GROW-PENDING ! THEN
    THEN ;

: _MOD-ADOPT  ( node inserted? -- )
    IF
        _LD-SP @ IF
            _LD-TXN-HEAD @ OVER _MN-PROV !
            _LD-TXN-HEAD !
        ELSE
            0 OVER _MN-PROV ! DROP
            1 _MOD-GROW-PENDING !
            _MOD-TRY-PENDING-GROWTH
        THEN
    ELSE
        DROP
    THEN ;

VARIABLE _MU-TARGET
VARIABLE _MU-LINK

: _MOD-UNLINK-LOCKED  ( node -- )
    _MU-TARGET !
    _MU-TARGET @ _MN-HASH @ _MOD-BUCKET _MU-LINK !
    BEGIN _MU-LINK @ @ DUP WHILE
        DUP _MU-TARGET @ = IF
            _MN-NEXT @ _MU-LINK @ !
            -1 _MOD-REG 16 + +!
            EXIT
        THEN
        _MN-NEXT _MU-LINK !
    REPEAT
    DROP ;

VARIABLE _MRB-NODE

: _MOD-ROLLBACK-FRAME  ( -- )
    _LD-TXN-HEAD @ DUP 0= IF DROP EXIT THEN
    0 _LD-TXN-HEAD !
    DUP _MRB-NODE !
    _MOD-LOCK LOCK
    BEGIN _MRB-NODE @ WHILE
        _MRB-NODE @ _MOD-UNLINK-LOCKED
        _MRB-NODE @ _MN-PROV @ _MRB-NODE !
    REPEAT
    _MOD-LOCK UNLOCK
    BEGIN DUP WHILE
        DUP _MN-PROV @ SWAP _MOD-FREE
    REPEAT
    DROP ;

: _MOD-COMMIT-FRAME  ( -- )
    _LD-TXN-HEAD @
    0 _LD-TXN-HEAD !
    DUP IF
        1 _MOD-GROW-PENDING !
        1 _MOD-GROW-READY !
    THEN
    BEGIN DUP WHILE
        DUP _MN-PROV @ SWAP _MN-PROV 0 SWAP !
    REPEAT
    DROP ;

: _MOD-AFTER-RELEASE  ( -- )
    _LD-SP @ 0= _MOD-GROW-READY @ AND IF
        0 _MOD-GROW-READY !
        _MOD-TRY-PENDING-GROWTH
    THEN ;

' _MOD-COMMIT-FRAME   IS _LD-TXN-COMMIT
' _MOD-ROLLBACK-FRAME IS _LD-TXN-ROLLBACK
' _MOD-AFTER-RELEASE  IS _LD-TXN-AFTER-RELEASE

\ ── Pre-scan for PROVIDED ────────────────────────────────────────────
\ _MOD-PRESCAN scans LD-BUF/LD-SZ for a line whose first evaluator token
\ is PROVIDED.  It returns the following token as an exact slice of the live
\ source buffer; it never copies through filesystem scratch.  The evaluator
\ and BL WORD delimit on byte 32, so this scanner deliberately does the same.

CREATE _PS-TAG  9 ALLOT   \ "PROVIDED" + NUL
80 _PS-TAG     C!         \ P
82 _PS-TAG 1+  C!         \ R
79 _PS-TAG 2 + C!         \ O
86 _PS-TAG 3 + C!         \ V
73 _PS-TAG 4 + C!         \ I
68 _PS-TAG 5 + C!         \ D
69 _PS-TAG 6 + C!         \ E
68 _PS-TAG 7 + C!         \ D
 0 _PS-TAG 8 + C!         \ NUL

\ _PS-MATCH8? ( addr -- flag )  True if addr points to "PROVIDED"
\   (exactly 8 chars, case-sensitive).
: _PS-MATCH8?  ( addr -- flag )
    TRUE 8 0 DO
        OVER I + C@ _PS-TAG I + C@ <> IF
            DROP FALSE LEAVE
        THEN
    LOOP NIP ;

\ _PS-SKIP-WS ( addr rem -- addr' rem' )  Skip evaluator delimiters.
: _PS-SKIP-WS  ( addr rem -- addr' rem' )
    BEGIN
        DUP 0> IF
            OVER C@ 32 =
        ELSE FALSE THEN
    WHILE
        1- SWAP 1+ SWAP
    REPEAT ;

\ _PS-TOKEN-LEN ( addr rem -- len )  Exact BL-delimited token length.
: _PS-TOKEN-LEN  ( addr rem -- len )
    0                                ( addr rem len )
    BEGIN
        OVER 0> IF
            2 PICK OVER + C@ 32 <>
        ELSE FALSE THEN
    WHILE
        1+ SWAP 1- SWAP
    REPEAT
    NIP NIP ;

VARIABLE _PS-PTR
VARIABLE _PS-REM
VARIABLE _PS-LINE-U

: _PS-LINE-LEN  ( addr rem -- len )
    0
    BEGIN
        DUP 2 PICK < IF
            2 PICK OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
        ELSE TRUE THEN
    UNTIL
    NIP NIP ;

: _MOD-PRESCAN  ( -- id-addr id-len found? )
    LD-BUF @ _PS-PTR !
    LD-SZ @ _PS-REM !
    BEGIN _PS-REM @ 0> WHILE
        _PS-PTR @ _PS-REM @ _PS-LINE-LEN DUP _PS-LINE-U !
        _PS-PTR @ SWAP _PS-SKIP-WS
        2DUP _PS-TOKEN-LEN 8 = IF
            OVER _PS-MATCH8? IF
                _PS-LINE-U @ _MOD-EVAL-LINE-MAX > IF
                    2DROP 0 0 TRUE EXIT
                THEN
                8 - SWAP 8 + SWAP _PS-SKIP-WS
                2DUP _PS-TOKEN-LEN NIP TRUE EXIT
            THEN
        THEN
        2DROP
        _PS-LINE-U @
        DUP _PS-REM @ < IF 1+ THEN
        DUP _PS-PTR +!
        NEGATE _PS-REM +!
    REPEAT
    0 0 FALSE ;

: _MOD-PARSE-ID  ( "id" -- id-addr id-len )
    BL WORD COUNT ;

\ PROVIDED-SPAN ( id-addr id-len -- ) registers a caller-owned exact ID.
\ PROVIDED parses the same public operation from the current input source.
\ Neither word leaves insertion status on the public data stack.
: PROVIDED-SPAN  ( id-addr id-len -- )
    ?CORE0
    _MOD-INSERT
    DUP IF >R 2DROP R> THROW THEN
    DROP _MOD-ADOPT ;

: PROVIDED  ( "name" -- )
    _MOD-PARSE-ID PROVIDED-SPAN ;

\ MODULE? ( "name" -- flag )  Test if a module is already loaded.
: MODULE?  ( "name" -- flag )
    ?CORE0
    _MOD-PARSE-ID
    DUP 0= OVER _MOD-ID-MAX > OR IF 2DROP _MOD-E-BAD-ID THROW THEN
    _MOD-FIND 0<> ;

\ _MOD-LOAD-BODY ( -- )  Load file whose name is already in NAMEBUF.
\   This is the core of LOAD without the PARSE-NAME call.
\   CWD must already point to the target directory (set by
\   _RESOLVE-PATH when the REQUIRE argument contains '/').
\   After reading the file, pre-scans for PROVIDED.  If the module
\   is already loaded, skips execution entirely (zero side effects).
: _MOD-LOAD-BODY  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Module not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DUP DIRENT DE.USED DUP 0= IF
        2DROP ."  Empty module" CR EXIT
    THEN
    _LD-SAVE
    LD-SZ !                              ( slot )
    \ Module source is a reclaimable loader allocation, not permanent XMEM.
    DUP _LD-SLOT-BYTES ALLOCATE IF
        2DROP ."  Module buffer allocation failed" CR
        _LD-RESTORE EXIT
    THEN
    LD-BUF !
    _LD-READ-SLOT
    \ Pre-register the exact source-buffer slice before executing anything.
    _MOD-PRESCAN IF
        _MOD-INSERT                     ( node inserted? ior )
        DUP IF
            >R 2DROP
            _LD-RELEASE
            _LD-TXN-AFTER-RELEASE
            R> THROW
        THEN
        DROP                            ( node inserted? )
        DUP 0= IF
            2DROP
            _LD-RELEASE
            _LD-TXN-AFTER-RELEASE
            EXIT
        THEN
        _MOD-ADOPT
    ELSE
        2DROP
    THEN
    _LD-WALK-GUARDED ;

\ REQUIRE ( "name" -- )  Load a module file.
\   The file's own PROVIDED line is the sole guard against duplicate
\   loading.  REQUIRE just resolves the path and loads the file.
\   Accepts relative paths: REQUIRE ../lib/util.f
\   Path components adjust CWD; the final name is looked up in
\   the resolved directory.  CWD is restored after loading.
_LD-MAXLVL 8 * XBUF _REQ-CWD-STK
VARIABLE _REQ-SP  0 _REQ-SP !

: _REQ-SAVE-CWD  ( -- )
    _REQ-SP @ _LD-MAXLVL 8 * >= ABORT" REQUIRE nested too deep"
    CWD @ _REQ-SP @ _REQ-CWD-STK + !
    8 _REQ-SP +! ;

: _REQ-RESTORE-CWD  ( -- )
    _REQ-SP @ 0= ABORT" REQUIRE nesting underflow"
    -8 _REQ-SP +!
    _REQ-SP @ _REQ-CWD-STK + @ CWD ! ;

: _REQUIRE-BODY  ( -- )
    _RESOLVE-PATH
    _MOD-LOAD-BODY ;

: REQUIRE  ( "name" -- )
    ?CORE0
    PARSE-NAME
    FS-ENSURE                  \ load FS before path resolution
    _REQ-SAVE-CWD
    ['] _REQUIRE-BODY CATCH
    _REQ-RESTORE-CWD           \ restore before returning or rethrowing
    THROW ;

VARIABLE _ML-NODE

: _MOD-LIST-BODY  ( -- )
    ."  Loaded modules:" CR
    _MOD-BUCKET-COUNT 0 DO
        _MOD-BUCKETS I CELLS + @ _ML-NODE !
        BEGIN _ML-NODE @ WHILE
            ."   "
            _ML-NODE @ DUP _MN-ID SWAP _MN-LEN @ TYPE CR
            _ML-NODE @ _MN-NEXT @ _ML-NODE !
        REPEAT
    LOOP
    _MOD-COUNT . ."  module(s)" CR ;

\ MODULES ( -- )  List all loaded modules.
\ Lock order is registry then UART; rollback cannot free an ID while TYPE uses
\ it, and CATCH guarantees both locks are released before any rethrow.
: MODULES  ( -- )
    ?CORE0
    _MOD-LOCK LOCK
    UART-ACQUIRE
    ['] _MOD-LIST-BODY CATCH
    UART-RELEASE
    _MOD-LOCK UNLOCK
    THROW ;

\ =====================================================================
