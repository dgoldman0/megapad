\ =====================================================================
\  KDOS v1.1 — Kernel Dashboard OS for Megapad-64
\ =====================================================================
\
\  Loaded via UART into the Megapad-64 BIOS v1.0 Forth system.
\
\  Subsystems:
\    1. Utilities       — common words, CMOVE, +!, tile-alignment
\    1.12 HBW Math RAM  — HBW-ALLOT, HBW-BUFFER, HBW-RESET
\    2. Buffers         — typed, tile-aligned data regions (Bank 0 + HBW)
\    3. Tile Ops        — SIMD buffer operations (SUM, ADD, etc.)
\    4. Kernels         — metadata registry for compute kernels
\    5. Sample Kernels  — kzero, kfill, kadd, kscale, kstats, kthresh
\    6. Pipelines       — kernel pipeline engine
\    7. Storage         — disk persistence, file abstraction, MP64FS
\       7.6 MP64FS     — on-disk named file system
\       7.7 Doc Browser — DOC, DESCRIBE, TUTORIAL, TOPICS, LESSONS
\       7.8 Dict Search — WORDS-LIKE, APROPOS, .RECENT
\    8. Scheduler       — cooperative & preemptive multitasking
\    8.1 Multicore      — CORE-RUN, CORE-WAIT, BARRIER, P.RUN-PAR
\    8.8 Micro-Cluster  — CLUSTER-ENABLE, HW-BARRIER-WAIT, SPAD
\    8.9 Cluster MPU    — CL-MPU-SETUP, CL-ENTER-USER, CL-MPU-OFF
\    9. Screens         — interactive TUI (ANSI terminal, 9 screens)
\   10. Data Ports      — transport-neutral structures and binding
\   11. Benchmarking    — BENCH for timing via CYCLES
\   12. Dashboard       — text-mode system overview
\   13. Help            — online reference for all KDOS words
\   14. Startup
\   15. Bundles         — versioned, declarative pipeline bundle format
\   18. Ring Buffers    — RING, RING-PUSH, RING-POP, RING-PEEK
\   19. Hash Tables     — HASHTABLE, HT-PUT, HT-GET, HT-DEL, HT-EACH
\   20. Module System   — PROVIDED, REQUIRE, MODULE?, MODULES
\
\  Loadable networking: networking.f provides Ethernet through TLS,
\  sockets, and the UDP-backed data-port transport in userland.

\ Enable JIT compilation for bulk KDOS load (saves ~49 KiB dictionary).
\ Turned off at the end of this file so interactive use is non-JIT.
JIT-ON

\ =====================================================================
\  §1  Utility Words
\ =====================================================================
\  Note: BIOS v1.0 provides all ANS core words including OFF, >=, <=,
\  W@, W!, L@, L!, .ZSTR, UCHAR, 2OVER, 2SWAP, 2ROT, TALIGN, MOVE,
\  COMPARE, ABORT, ABORT", LEAVE, EVALUATE, EVALUATE-CHECKED,
\  EVALUATE-FINISH, EVALUATOR-RESET, EVALUATOR-UNWIND, EVAL-STATUS,
\  EVAL-LINE, EVAL-COLUMN, EVAL-DEPTH, EVAL-THROW, EVAL-TOKEN,
\  FIND, SOURCE, >IN, >NUMBER,
\  VALUE, TO, DOES>, POSTPONE, RECURSE, COUNT, WITHIN, U<, U>, 2/,
\  CHAR, [CHAR], 2>R, 2R>, 2R@, QUIT, plus the v0.5 set.

\ .R ( n width -- ) print number right-justified in width
: .R  ( just use . for now )
    DROP . ;

\ DEFER ( "name" -- ) create a deferred word (default = ABORT)
\ IS   ( xt "name" -- ) set the action of a deferred word
: DEFER  ( "name" -- )  CREATE ['] ABORT ,  DOES> @ EXECUTE ;
: IS     ( xt "name" -- )  ' >BODY ! ;

\ -- String utilities (needed by MP64FS file system) --

\ SAMESTR? ( addr1 addr2 maxlen -- flag )
\   Compare two byte strings up to maxlen bytes (zero-padded).
\   Returns -1 if equal, 0 if different.
: SAMESTR?  ( a1 a2 maxlen -- flag )
    DUP >R SWAP R> COMPARE 0= IF -1 ELSE 0 THEN ;

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

\ =====================================================================
\  §1.1  Memory Allocator
\ =====================================================================
\
\  First-fit free-list allocator.  Each block has a 16-byte header:
\    +0   next    pointer to next free block (0 = end of list)
\    +8   size    usable bytes in this block (excludes header)
\
\  ALLOCATE returns an address past the header.  FREE takes that
\  address, backs up 16 bytes to find the header, and inserts
\  the block into the free list (sorted by address, coalescing
\  adjacent blocks).
\
\  The heap lives above HERE (which is reserved for the Forth
\  dictionary).  HEAP-BASE marks the start; it's set at load time
\  to a safe offset above HERE.
\
\  Memory layout:
\    0x00000  ...  BIOS+KDOS dictionary  ...  HERE
\    HEAP-BASE  ...  heap blocks  ...
\    DSP ↓  (data stack grows down)
\

24 CONSTANT /ALLOC-HDR
0xA110CA7EDEADBEEF CONSTANT ALLOC-MAGIC

\ -- Heap state (declared early so ?DICT-ROOM can reference them) --
VARIABLE HEAP-BASE    0 HEAP-BASE !
VARIABLE HEAP-FREE    0 HEAP-FREE !    \ head of free list
VARIABLE HEAP-INIT    0 HEAP-INIT !    \ flag: has heap been initialised?

\ ?DICT-ROOM ( u -- )
\   Abort if HERE + u would reach within 256 bytes of SP,
\   or would collide with the heap (if initialised).
\   Use before large ALLOT or CREATE sequences in Forth code
\   to catch dictionary overflow before it corrupts the stack.
: ?DICT-ROOM  ( u -- )
    HERE + 256 +
    DUP SP@ >= ABORT" dictionary overflow"
    HEAP-INIT @ IF  HEAP-BASE @ >= ABORT" dictionary into heap"  ELSE DROP  THEN ;

\ MEM-SIZE ( -- u )  total RAM in bytes
\   Reads bank0_size (64-bit, in bytes) from SysInfo register at offset 0x08.
: MEM-SIZE  ( -- u )
    0xFFFFFF0000000308 @ ;        \ SysInfo + 0x08 = bank0_size (bytes)

\ -- Core-type identification (dynamic, reads from SysInfo) --
\ N-FULL is a BIOS word that reads SysInfo + 0x48 = NUM_FULL_CORES.
\ MICRO-CORE? and FULL-CORE? use it so the threshold adapts to any
\ configuration (e.g. 16 full + 3 clusters).

\ MICRO-CORE? ( id -- flag )  true if core id is a micro-core
: MICRO-CORE?  ( id -- flag )  N-FULL >= ;

\ FULL-CORE? ( id -- flag )  true if core id is a full core
: FULL-CORE?   ( id -- flag )  N-FULL < ;

\ Legacy alias (matches BIOS N-FULL)
: N-FULL-CORES  ( -- n )  N-FULL ;

\ -- Allocator scratch variables (avoid deep stack gymnastics) --
VARIABLE A-PREV       \ previous free-list node (0 = update HEAP-FREE)
VARIABLE A-CURR       \ current free-list node being examined
VARIABLE A-SIZE       \ requested allocation size (rounded)

\ -- Stack-proximity guard constant --
4096 CONSTANT HEAP-GUARD   \ minimum gap between heap top and stack bottom

\ HEAP-SETUP ( -- )  initialise the heap above HERE
\   Leaves a 16 KiB gap above HERE for late Bank-0 dictionary growth,
\   then creates one large free block spanning to the stack guard.
: HEAP-SETUP  ( -- )
    HEAP-INIT @ IF EXIT THEN
    HERE  16384  + TALIGN  HEAP-BASE !
    \ Heap end = data-stack bottom - 4096 guard
    MEM-SIZE 2 / 4096 -   ( heap-end )
    HEAP-BASE @ -          ( available-bytes )
    /ALLOC-HDR -           ( usable size for first block )
    DUP 64 < ABORT" Heap too small"
    \ Write header for the single free block
    0 HEAP-BASE @ !              \ next = 0 (end of list)
    HEAP-BASE @ 8 + !            \ size = available
    0 HEAP-BASE @ 16 + !         \ magic = 0 (free)
    HEAP-BASE @ HEAP-FREE !      \ free list head
    1 HEAP-INIT ! ;

\ (LINK-PREV!) ( addr -- )
\   Set previous node's next field (or HEAP-FREE) to addr.
: (LINK-PREV!)  ( addr -- )
    A-PREV @ 0= IF  HEAP-FREE !  ELSE  A-PREV @ !  THEN ;

\ -- Multicore safety guard --
\   Words that use shared scratch VARIABLEs (A-PREV, A-CURR, AR-SZ,
\   FL-PREV, etc.) are unsafe under concurrent execution.  ?CORE0
\   aborts if called from a secondary core, catching accidental
\   concurrent access at the point of call rather than allowing
\   silent corruption.  See §8.1 for the concurrency contract.
: ?CORE0  ( -- )
    COREID 0<> ABORT" core-0 only: use ARENA-ALLOT on secondary cores" ;

\ (BANK0-ALLOCATE) ( u -- addr ior )
\   Allocate u bytes from Bank 0 heap.  Returns address and 0 on success,
\   or 0 and -1 on failure.  First-fit search.
\   Core-0 only — uses shared scratch variables.
: (BANK0-ALLOCATE)  ( u -- addr ior )
    ?CORE0
    \ Reject non-positive sizes and values whose alignment addition would
    \ cross the signed-cell ceiling.  Validate before lazy heap setup so a
    \ rejected request cannot mutate allocator state.
    DUP 0< OVER 0= OR
    OVER 0x7FFFFFFFFFFFFFF8 > OR IF DROP 0 -1 EXIT THEN
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    \ Round up to 8-byte alignment, minimum 16
    7 + -8 AND  DUP 16 < IF DROP 16 THEN
    A-SIZE !
    0 A-PREV !   HEAP-FREE @ A-CURR !
    BEGIN
        A-CURR @ 0= IF  0 -1 EXIT  THEN      \ OOM
        A-CURR @ 8 + @                         ( block-size )
        A-SIZE @ >= IF
            \ Stack-proximity guard: block-end must stay below SP
            A-CURR @ /ALLOC-HDR + A-SIZE @ +  ( blk-end )
            SP@ >= IF
                \ This block would collide with the stack — reject
                0 -1 EXIT
            THEN
            \ Found a big enough block
            A-CURR @ 8 + @  A-SIZE @ -         ( leftover )
            DUP /ALLOC-HDR 16 + >= IF
                \ Split: new free block after the allocated region
                A-CURR @ /ALLOC-HDR + A-SIZE @ +  ( leftover new-blk )
                A-CURR @ @ OVER !                  \ new-blk.next = curr.next
                SWAP /ALLOC-HDR - OVER 8 + !       \ new-blk.size = leftover-hdr
                A-SIZE @ A-CURR @ 8 + !            \ curr.size = requested
                0 OVER 16 + !                       \ new-blk.magic = 0 (free)
                (LINK-PREV!)                        \ prev → new-blk
            ELSE
                \ Use whole block — unlink from free list
                DROP
                A-CURR @ @  (LINK-PREV!)            \ prev → curr.next
            THEN
            ALLOC-MAGIC A-CURR @ 16 + !              \ stamp allocated canary
            A-CURR @ /ALLOC-HDR +  0  EXIT          \ return user addr + success
        THEN
        \ Block too small — advance
        A-CURR @ A-PREV !
        A-CURR @ @ A-CURR !
    AGAIN ;

\ (COALESCE) ( -- )
\   After FREE inserts a block into the free list (address in A-CURR),
\   merge with adjacent neighbours.
\   Forward: if block_end == block.next, absorb successor.
\   Backward: if prev_end == block, absorb block into predecessor.
: (COALESCE)  ( -- )
    \ -- Forward merge: block with its successor --
    A-CURR @ @ ?DUP IF                           ( next )
        A-CURR @ DUP 8 + @ + /ALLOC-HDR +       ( next block-end )
        OVER = IF                                ( next )
            \ block.size += /ALLOC-HDR + next.size
            A-CURR @ 8 + @  /ALLOC-HDR +         ( next old+hdr )
            OVER 8 + @ +                         ( next new-sz )
            A-CURR @ 8 + !                       ( next )
            \ block.next = next.next
            @ A-CURR @ !                         ( )
        ELSE  DROP
        THEN
    THEN
    \ -- Backward merge: predecessor with block --
    A-PREV @ ?DUP IF                             ( prev )
        DUP DUP 8 + @ + /ALLOC-HDR +            ( prev prev-end )
        A-CURR @ = IF                            ( prev )
            \ prev.size += /ALLOC-HDR + block.size
            A-CURR @ 8 + @  /ALLOC-HDR +         ( prev blk+hdr )
            OVER 8 + @ +  OVER 8 + !             ( prev )
            \ prev.next = block.next
            A-CURR @ @  SWAP !                    ( )
        ELSE  DROP
        THEN
    THEN ;

\ (BANK0-FREE) ( addr -- )
\   Return a previously allocated block to the Bank 0 free list.
\   Inserts in address-sorted order and coalesces adjacent blocks.
\   Core-0 only — uses shared scratch variables.
: (BANK0-FREE)  ( addr -- )
    ?CORE0
    DUP 0= IF DROP EXIT THEN
    /ALLOC-HDR -   ( block )
    DUP 16 + @ ALLOC-MAGIC <> ABORT" FREE: invalid or double-free"
    0 OVER 16 + !                                   \ clear canary
    0 A-PREV !   HEAP-FREE @ A-CURR !
    BEGIN
        A-CURR @ 0= IF
            \ End of list — append here
            A-CURR !                                \ A-CURR = block
            0 A-CURR @ !                            \ block.next = 0
            A-PREV @ 0= IF  A-CURR @ HEAP-FREE !
            ELSE  A-CURR @ A-PREV @ !  THEN
            (COALESCE) EXIT
        THEN
        A-CURR @ OVER > IF
            \ Insert before curr
            A-CURR @ OVER !                         \ block.next = old-curr
            A-CURR !                                \ A-CURR = block
            A-PREV @ 0= IF  A-CURR @ HEAP-FREE !
            ELSE  A-CURR @ A-PREV @ !  THEN
            (COALESCE) EXIT
        THEN
        \ Advance
        A-CURR @ A-PREV !
        A-CURR @ @ A-CURR !
    AGAIN ;

\ (BANK0-RESIZE) ( a1 u -- a2 ior )
\   Resize a Bank 0 allocated block.
\   1) If shrinking or same size: update size in place, split if worthwhile.
\   2) If growing and the next free block is adjacent + big enough: merge.
\   3) Otherwise: alloc new, copy, free old.
\   On failure returns original address and non-zero ior.

VARIABLE R-BLK     \ block header address
VARIABLE R-OLD     \ old usable size
VARIABLE R-NEW     \ new requested size (rounded)

\ (TRY-GROW) ( -- flag )
\   Attempt in-place growth of R-BLK from R-OLD to R-NEW.
\   If the free block immediately after our block is big enough,
\   absorb it and return TRUE.  Otherwise return FALSE.
: (TRY-GROW)  ( -- flag )
    R-BLK @ /ALLOC-HDR + R-OLD @ +    ( block-end )
    0 A-PREV !   HEAP-FREE @ A-CURR !
    BEGIN
        A-CURR @ 0= IF  DROP FALSE EXIT  THEN
        A-CURR @ OVER = IF
            \ Found adjacent free block — check size
            DROP
            A-CURR @ 8 + @  /ALLOC-HDR +   ( avail )
            R-NEW @ R-OLD @ -              ( avail need )
            2DUP >= IF
                \ Enough — absorb the free block
                2DROP
                A-CURR @ @  (LINK-PREV!)
                \ block.size = old + header + free.size
                R-OLD @ /ALLOC-HDR + A-CURR @ 8 + @ +
                R-BLK @ 8 + !
                \ Split off leftover if worthwhile
                R-BLK @ 8 + @  R-NEW @ -   ( leftover )
                DUP /ALLOC-HDR 16 + >= IF
                    R-NEW @ R-BLK @ 8 + !  \ block.size = new
                    R-BLK @ /ALLOC-HDR + R-NEW @ +  ( leftover remnant )
                    0 OVER !                \ remnant.next = 0
                    SWAP /ALLOC-HDR - OVER 8 + !  \ remnant.size
                    ALLOC-MAGIC OVER 16 + ! \ stamp so FREE accepts it
                    /ALLOC-HDR + (BANK0-FREE)  \ free the remnant
                ELSE DROP
                THEN
                TRUE EXIT
            ELSE  2DROP FALSE EXIT
            THEN
        THEN
        A-CURR @ A-PREV !
        A-CURR @ @ A-CURR !
    AGAIN ;

\ Core-0 only — uses shared scratch variables.
: (BANK0-RESIZE)  ( a1 u -- a2 ior )
    ?CORE0
    \ Reject non-positive or unroundable sizes before adding the alignment
    \ bias.  This keeps a wrapped request from looking like a small resize.
    DUP 0< OVER 0= OR
    OVER 0x7FFFFFFFFFFFFFF8 > OR IF 2DROP 0 -1 EXIT THEN
    \ Round new size
    7 + -8 AND  DUP 16 < IF DROP 16 THEN
    R-NEW !
    DUP /ALLOC-HDR -  R-BLK !            \ block = a1 - header
    R-BLK @ 8 + @  R-OLD !               \ old size
    \ --- Case 1: shrinking or same size ---
    R-NEW @ R-OLD @ <= IF
        R-OLD @ R-NEW @ - DUP /ALLOC-HDR 16 + >= IF
            \ Worth splitting: create a free remnant
            R-NEW @ R-BLK @ 8 + !        \ block.size = new
            R-BLK @ /ALLOC-HDR + R-NEW @ +  ( leftover remnant )
            0 OVER !                       \ remnant.next = 0
            SWAP /ALLOC-HDR - OVER 8 + !   \ remnant.size = leftover-hdr
            ALLOC-MAGIC OVER 16 + !        \ stamp so FREE accepts it
            /ALLOC-HDR + (BANK0-FREE)      \ free the remnant
        ELSE DROP
        THEN
        0  EXIT                            \ return ( a1 0 )
    THEN
    \ --- Case 2: try in-place growth ---
    (TRY-GROW) IF
        0  EXIT                            \ return ( a1 0 )
    THEN
    \ --- Case 3: fallback alloc+copy+free ---
    R-NEW @ (BANK0-ALLOCATE)               ( a1 a2 ior )
    IF  DROP -1 EXIT  THEN                 ( a1 a2 )
    R-BLK !                                \ repurpose R-BLK to save a2
    DUP R-BLK @ R-OLD @ CMOVE             ( a1 ; CMOVE src=a1 dst=a2 cnt=old )
    (BANK0-FREE)                           ( ; free old — clobbers A-CURR )
    R-BLK @ 0 ;                            \ ( a2 0 )

\ HEAP-FREE-BYTES ( -- u )
\   Walk the free list summing available bytes.
: HEAP-FREE-BYTES  ( -- u )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    0 HEAP-FREE @
    BEGIN
        DUP 0<> WHILE
        DUP 8 + @ ROT + SWAP   ( sum' curr )
        @                       ( sum' next )
    REPEAT
    DROP ;

\ HEAP-FRAG ( -- n )
\   Count the number of free blocks.  Fragmentation = n - 1 when n > 0.
\   A perfectly defragmented heap has 1 free block (frag = 0).
: HEAP-FRAG  ( -- n )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    0 HEAP-FREE @
    BEGIN
        DUP 0<> WHILE
        SWAP 1+ SWAP   ( count+1 curr )
        @               ( count+1 next )
    REPEAT
    DROP ;

\ HEAP-LARGEST ( -- u )
\   Return the size of the largest contiguous free block.
: HEAP-LARGEST  ( -- u )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    0 HEAP-FREE @
    BEGIN
        DUP 0<> WHILE
        DUP 8 + @  ROT MAX SWAP   ( max' curr )
        @                          ( max' next )
    REPEAT
    DROP ;

\ (HEAP-TOP) ( -- addr )  highest occupied byte + 1 across all alloc'd blocks
\   Walk free list to find the block whose end is closest to the stack.
\   The real top = address of the last allocated region's end.
\   Approximation: HEAP-BASE + total-heap-size (MEM-SIZE/2 - 4096 gap).
\   For the guard check we use a simpler metric: the candidate block's
\   end address must not intrude into SP@ - HEAP-GUARD.

\ HEAP-CHECK ( -- flag )  true if heap is safely below data stack
: HEAP-CHECK  ( -- flag )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    \ Walk all free blocks, find the highest block-end address
    \ Heap top = max(each-free-block + header + size) or HEAP-BASE if empty
    HEAP-BASE @   HEAP-FREE @
    BEGIN
        DUP 0<> WHILE
        DUP DUP 8 + @ + /ALLOC-HDR +   ( best curr blk-end )
        ROT MAX SWAP                    ( best' curr )
        @                               ( best' next )
    REPEAT
    DROP                                ( heap-top-estimate )
    SP@ < ;                              ( flag: heap below stack )

\ .HEAP ( -- ) show heap summary
: .HEAP  ( -- )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    ."  Heap: base=" HEAP-BASE @ .
    ."   free=" HEAP-FREE-BYTES . ."  bytes"
    ."   blocks=" HEAP-FRAG .
    ."   largest=" HEAP-LARGEST .
    ."   safe=" HEAP-CHECK IF ." yes" ELSE ." NO" THEN CR ;

\ HEAP-VERIFY ( -- flag )
\   Walk the free list and verify structural integrity:
\   1. Each block address >= HEAP-BASE
\   2. Blocks are in ascending address order
\   3. Free blocks have magic = 0 (not allocated)
\   Returns TRUE if heap is consistent, FALSE if corruption detected.
: HEAP-VERIFY  ( -- flag )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    TRUE                              ( ok )
    0                                 ( ok prev )
    HEAP-FREE @                       ( ok prev curr )
    BEGIN DUP WHILE
        \ bounds: block must be >= HEAP-BASE
        DUP HEAP-BASE @ < IF
            ." heap: block below base" CR
            ROT DROP FALSE -ROT
        THEN
        \ address ordering (skip for first block where prev=0)
        OVER 0<> IF
            OVER OVER >= IF
                ." heap: blocks out of order" CR
                ROT DROP FALSE -ROT
            THEN
        THEN
        \ magic must be 0 for free blocks
        DUP 16 + @ 0<> IF
            ." heap: free block has non-zero magic" CR
            ROT DROP FALSE -ROT
        THEN
        \ advance: prev=curr, curr=curr.next
        SWAP DROP DUP             ( ok curr curr )
        @                         ( ok prev' curr' )
    REPEAT
    2DROP ;

\ =====================================================================
\  §1.1a  Dictionary Snapshots — MARKER / FORGET
\ =====================================================================
\
\  MARKER creates a named word that, when executed, forgets
\  everything defined after it (restores HERE and LATEST).
\
\  FORGET parses a word name and forgets everything from that
\  word onward (including the named word itself).
\
\  Implementation: var_latest is at STATE + 24 (adjacent BIOS
\  variables: state(+0), base(+8), here(+16), latest(+24)).
\  Verified at load time.
\

\ VAR-LATEST — verified address of the BIOS var_latest variable
\ BIOS layout: var_state(+0) var_base(+8) var_here(+16) var_latest(+24)
STATE 24 +  DUP @  LATEST <>
    IF ." VAR-LATEST offset mismatch" CR ABORT THEN
CONSTANT VAR-LATEST

\ LATEST! ( entry -- )  set the dictionary head pointer
: LATEST!  ( entry -- )  VAR-LATEST ! ;

\ MARKER ( "name" -- )
\   Create a checkpoint word.  Executing it later forgets everything
\   defined after (and including) the marker itself.
: MARKER  ( "name" -- )
    HERE LATEST            ( save-here save-latest )
    CREATE , ,             ( ; data+0=save-latest  data+8=save-here )
    DOES>
        DUP @ SWAP 8 + @  ( save-latest save-here )
        HERE - ALLOT       ( save-latest ; HERE restored )
        LATEST!            ( ; LATEST restored )
    ;

\ (ENTRY>NAME) ( entry -- addr len )  inline name accessor
\   Dictionary header: [link:8][flags+len:1][name:N]
: (ENTRY>NAME)  ( entry -- addr len )
    DUP 8 + C@ 127 AND  SWAP 9 + SWAP ;

VARIABLE FG-A   VARIABLE FG-L     \ FORGET scratch

\ FORGET ( "name" -- )
\   Forget a word and everything defined after it.
\   Case-insensitive match (same as the outer interpreter).
: FORGET  ( "name" -- )
    BL WORD COUNT                    ( c-addr u )
    DUP 0= ABORT" Usage: FORGET <name>"
    FG-L !  FG-A !
    LATEST                           ( entry )
    BEGIN
        DUP 0= ABORT" FORGET: not found"
        DUP (ENTRY>NAME)             ( entry ea el )
        FG-L @ OVER <> IF
            \ Lengths differ — skip
            2DROP
        ELSE
            \ Compare chars case-insensitively
            TRUE SWAP 0 DO           ( entry ea flag )
                OVER I + C@ UCHAR
                FG-A @ I + C@ UCHAR
                <> IF  DROP FALSE LEAVE  THEN
            LOOP                     ( entry ea flag )
            SWAP DROP                ( entry flag )
            IF
                DUP @ LATEST!        ( entry ; LATEST = entry.link )
                HERE - ALLOT         ( ; HERE = entry addr )
                EXIT
            THEN
        THEN
        @                            ( next-entry )
    AGAIN ;

\ =====================================================================
\  §1.2  Exception Handling — CATCH / THROW
\ =====================================================================
\
\  ANS Forth CATCH/THROW (EXCEPTION word set).
\
\  CATCH saves the current stack pointers and installs an exception
\  frame.  If the executed XT calls THROW with a non-zero code,
\  control returns to the matching CATCH with stacks restored.
\
\  Exception frames are chained through execution-context-local HANDLER cells:
\  one for each BIOS coroutine on core 0, and one for each physical worker
\  core.  CATCH frames may therefore remain live across PAUSE/TASK-YIELD.
\  A stopped or replaced coroutine never resumes to unwind its live frames, so
\  KDOS clears that slot's chain head at the BIOS scheduling boundary below.
\
\  Requires BIOS words: SP@ SP! RP@ RP!
\

\ Each execution context with independent data/return stacks must also have an
\ independent exception-chain head.  Core 0 selects by cooperative TASK-ID;
\ physical worker cores select by COREID and do not consult core 0's task state.
\ Keep HANDLER's traditional `( -- addr )` interface.
CREATE _HANDLERS  NCORES CELLS ALLOT
_HANDLERS NCORES CELLS 0 FILL
CREATE _TASK-HANDLERS  4 CELLS ALLOT
_TASK-HANDLERS 4 CELLS 0 FILL

: HANDLER  ( -- addr )
    COREID ?DUP IF
        CELLS _HANDLERS +
    ELSE
        TASK-ID CELLS _TASK-HANDLERS +
    THEN ;

\ CATCH ( xt -- exception# | 0 )
\   Execute xt.  If it completes normally, return 0.
\   If xt (or anything it calls) does THROW n, return n
\   with data stack restored to depth at CATCH entry + 1.
: CATCH  ( xt -- 0 | exception# )
    SP@ >R              ( save data-stack pointer )
    HANDLER @ >R        ( save previous handler frame )
    RP@ HANDLER !       ( install new handler = current RSP )
    EXECUTE             ( run the XT )
    R> HANDLER !        ( restore previous handler )
    R> DROP             ( discard saved SP )
    0 ;                 ( no exception → 0 )

\ THROW ( n -- )
\   If n = 0, do nothing (identity).
\   If n ≠ 0, unwind to most recent CATCH, restoring stacks.
: THROW  ( n -- )
    ?DUP IF
        HANDLER @ RP!   ( unwind RSP to handler frame )
        R> HANDLER !    ( pop & restore previous handler )
        R> SWAP >R      ( recover saved SP, stash throw-code )
        SP!             ( restore data stack )
        DROP R>         ( drop stale TOS, retrieve throw-code )
    THEN ;

\ Preserve the BIOS task ABI while adding KDOS-owned exception cleanup.  BIOS
\ cannot clear _TASK-HANDLERS because that table is allocated when KDOS loads.
\ Scheduling a slot is also replacement, so reset on both start and stop.  Slot
\ zero is the foreground handler and is deliberately never touched here.
' BACKGROUND  CONSTANT _BIOS-BACKGROUND-XT
' BACKGROUND2 CONSTANT _BIOS-BACKGROUND2-XT
' BACKGROUND3 CONSTANT _BIOS-BACKGROUND3-XT
' TASK-STOP   CONSTANT _BIOS-TASK-STOP-XT

: _TASK-HANDLER-RESET  ( slot -- )
    CELLS _TASK-HANDLERS + 0 SWAP ! ;

: BACKGROUND  ( xt -- )
    1 _TASK-HANDLER-RESET  _BIOS-BACKGROUND-XT EXECUTE ;

: BACKGROUND2  ( xt -- )
    2 _TASK-HANDLER-RESET  _BIOS-BACKGROUND2-XT EXECUTE ;

: BACKGROUND3  ( xt -- )
    3 _TASK-HANDLER-RESET  _BIOS-BACKGROUND3-XT EXECUTE ;

: TASK-STOP  ( slot -- )
    DUP _BIOS-TASK-STOP-XT EXECUTE  _TASK-HANDLER-RESET ;

\ =====================================================================
\  §1.3  CRC Convenience Words
\ =====================================================================
\
\  The BIOS provides eight primitives backed by the CRC ISA accelerator:
\    CRC-POLY!  ( n -- )       0=BZIP2 tuple, 1=non-reflected Castagnoli,
\                              2=CRC-64/WE parameters
\    CRC-INIT!  ( n -- )       initial CRC value
\    CRC-FEED   ( n -- )       feed 8 bytes (LE 64-bit cell)
\    CRC-FEED-BYTE ( b -- )    feed exactly one byte
\    CRC@       ( -- n )       current CRC result
\    CRC-RESET  ( -- )         reset to init value
\    CRC-FINAL  ( -- )         XOR-out (finalize)
\    CRC-FINAL@ ( -- n )       atomically finalize and return result
\
\  Below we build high-level words on top of those primitives.

\ CRC-BUF ( addr u -- )  Feed u bytes from addr into the CRC engine.
\   Processes full 8-byte chunks via CRC-FEED, then feeds each remaining
\   byte exactly once.  No padding bytes become part of the checksum.
: CRC-BUF  ( addr u -- )
    \ Process full 8-byte chunks using BEGIN/WHILE/REPEAT
    BEGIN  DUP 8 >=  WHILE
        OVER @ CRC-FEED
        SWAP 8 + SWAP
        8 -
    REPEAT
    \ Remaining bytes: 0..7
    BEGIN  DUP 0 >  WHILE
        OVER C@ CRC-FEED-BYTE
        SWAP 1+ SWAP
        1-
    REPEAT
    2DROP
;

\ CRC32-BUF ( addr u -- crc )  Compute CRC-32 of a buffer.
: CRC32-BUF
    0 CRC-POLY!
    0xFFFFFFFF CRC-INIT!
    CRC-BUF
    CRC-FINAL@ ;

\ CRC32C-BUF ( addr u -- crc )  Compute mode-1 non-reflected Castagnoli CRC.
: CRC32C-BUF
    1 CRC-POLY!
    0xFFFFFFFF CRC-INIT!
    CRC-BUF
    CRC-FINAL@ ;

\ CRC64-BUF ( addr u -- crc )  Compute CRC-64/WE of a buffer.
: CRC64-BUF
    2 CRC-POLY!
    0xFFFFFFFFFFFFFFFF CRC-INIT!
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

\ .PERF ( -- )  Display performance counters.
: .PERF
    CR ."   Performance Counters" CR
    ."     Cycles:   " PERF-CYCLES . CR
    ."     Stalls:   " PERF-STALLS . CR
    ."     Tile ops: " PERF-TILEOPS . CR
    ."     Ext mem:  " PERF-EXTMEM . CR ;

\ .BIST-STATUS ( -- )  Display last BIST result (from boot, NOT re-run).
\   BIST destroys all RAM so must NOT be run after KDOS loads.
: .BIST-STATUS
    CR ."   Memory BIST Status" CR
    BIST-STATUS
    DUP 0 = IF DROP ."     idle (no BIST run)" CR ELSE
    DUP 2 = IF DROP ."     PASS" CR ELSE
    DUP 3 = IF DROP ."     FAIL at addr " BIST-FAIL-ADDR . CR
                    ."     Expected/Actual: " BIST-FAIL-DATA . CR ELSE
    DROP ."     running..."  CR
    THEN THEN THEN ;

\ .TILE-DIAG ( -- )  Run tile self-test and display result.
: .TILE-DIAG
    CR ."   Tile Datapath Self-Test..."  CR
    TILE-TEST
    BEGIN TILE-TEST@ DUP 0 = WHILE DROP REPEAT
    DUP 2 = IF
        DROP ."     PASS (ADD, MUL, DOT, SUM)" CR
    ELSE
        DROP ."     FAIL — failed sub-tests: " TILE-DETAIL@ . CR
    THEN ;

\ .ICACHE ( -- )  Display I-cache statistics.
: .ICACHE
    CR ."   I-Cache Statistics" CR
    ."     Hits:     " ICACHE-HITS . CR
    ."     Misses:   " ICACHE-MISSES . CR ;

\ DIAG ( -- )  Run full hardware diagnostics suite.
: DIAG
    CR ."  ======== Hardware Diagnostics ========" CR
    .PERF
    .BIST-STATUS
    .TILE-DIAG
    .ICACHE
    ."  ======================================" CR ;

\ =====================================================================
\  §1.5  AES-256-GCM Encryption
\ =====================================================================
\
\  BIOS primitives (hardware accelerator at MMIO 0x0700):
\    AES-KEY!      ( addr -- )      write 32-byte key
\    AES-IV!       ( addr -- )      write 12-byte IV/nonce
\    AES-AAD-LEN!  ( n -- )         set AAD length in bytes
\    AES-DATA-LEN! ( n -- )         set data length in bytes
\    AES-CMD!      ( n -- )         0=encrypt, 1=decrypt
\    AES-STATUS@   ( -- n )         0=idle, 2=done, 3=auth-fail
\    AES-DIN!      ( addr -- )      write 16-byte input block
\    AES-DOUT@     ( addr -- )      read 16-byte output block
\    AES-TAG@      ( addr -- )      read 16-byte GCM tag
\    AES-TAG!      ( addr -- )      write 16-byte expected tag
\
\  Flow: set key → set IV → set lengths → CMD → feed blocks → read tag

\ Scratch buffers for AES block I/O (16-byte aligned)
CREATE AES-BLK-IN  16 ALLOT
CREATE AES-BLK-OUT 16 ALLOT
CREATE AES-TAG-BUF 16 ALLOT

\ AES-ENCRYPT-BLK ( src dst -- )  Encrypt one 16-byte block in place.
\   Assumes key/IV/lengths/CMD already set.
: AES-ENCRYPT-BLK ( src dst -- )
    SWAP AES-DIN!                \ feed src block to hardware
    AES-DOUT@                    \ read result into dst
;

\ AES-ENCRYPT ( key iv src dst len -- tag-addr )
\   Encrypt 'len' bytes (must be multiple of 16) from src to dst.
\   key = 32-byte addr, iv = 12-byte addr.
\   Returns address of 16-byte tag buffer.
: AES-ENCRYPT ( key iv src dst len -- tag-addr )
    >R >R >R          \ R: len dst src
    AES-IV!            \ set IV
    AES-KEY!           \ set key
    0 AES-AAD-LEN!
    R> R> R>           \ src dst len
    DUP AES-DATA-LEN!
    0 AES-CMD!         \ 0 = encrypt
    \ loop over 16-byte blocks
    DUP 4 RSHIFT       \ len nblocks (len/16)
    >R                 \ R: nblocks  stack: src dst len
    DROP                \ src dst
    R> 0 DO             \ src dst  (I = block index)
        OVER AES-DIN!       \ feed source block
        DUP AES-DOUT@       \ read output to dest
        SWAP 16 + SWAP 16 + \ advance both pointers
    LOOP
    2DROP
    AES-TAG-BUF AES-TAG@    \ read computed tag
    AES-TAG-BUF              \ return tag buffer address
;

\ AES-DECRYPT ( key iv src dst len tag -- flag )
\   Decrypt 'len' bytes from src to dst.  tag = 16-byte expected tag addr.
\   Returns 0 if authentication passed, -1 if failed.
: AES-DECRYPT ( key iv src dst len tag -- flag )
    AES-TAG!           \ write expected tag for verification
    >R >R >R           \ R: len dst src
    AES-IV!
    AES-KEY!
    0 AES-AAD-LEN!
    R> R> R>           \ src dst len
    DUP AES-DATA-LEN!
    1 AES-CMD!         \ 1 = decrypt
    DUP 4 RSHIFT       \ len nblocks
    >R DROP             \ src dst   R: nblocks
    R> 0 DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    2DROP
    AES-STATUS@ 3 = IF -1 ELSE 0 THEN
;

\ .AES-STATUS ( -- )  Print human-readable AES status.
: .AES-STATUS
    AES-STATUS@
    DUP 0 = IF DROP ."  AES: idle" CR ELSE
    DUP 2 = IF DROP ."  AES: done (OK)" CR ELSE
    DUP 3 = IF DROP ."  AES: AUTH FAIL" CR ELSE
    DROP ."  AES: busy" CR
    THEN THEN THEN ;

\ -- AES-256-GCM with AAD (Authenticated Associated Data) --
\   TLS 1.3 requires AAD (the 5-byte record header) be authenticated
\   but not encrypted.  These words extend AES-ENCRYPT/DECRYPT with AAD.
CREATE AES-AAD-PAD 16 ALLOT           \ zero-padded AAD block
CREATE AES-PARTIAL-PAD 16 ALLOT       \ zero-padded partial final block
VARIABLE _AEAD-AAD
VARIABLE _AEAD-AADLEN
VARIABLE _AEAD-REM

: AES-ENCRYPT-AEAD ( key iv aad aadlen src dst dlen -- tag-addr )
    >R >R >R            \ R: dlen dst src.  Stack: key iv aad aadlen
    _AEAD-AADLEN !
    _AEAD-AAD !
    AES-IV!
    AES-KEY!
    _AEAD-AADLEN @ AES-AAD-LEN!
    R> R> R>             \ src dst dlen
    DUP AES-DATA-LEN!
    0 AES-CMD!
    \ Feed AAD block (zero-padded to 16 bytes)
    AES-AAD-PAD 16 0 FILL
    _AEAD-AAD @ AES-AAD-PAD _AEAD-AADLEN @ CMOVE
    AES-AAD-PAD AES-DIN!
    \ Feed complete 16-byte data blocks
    DUP 15 AND _AEAD-REM !
    DUP 4 RSHIFT         \ src dst dlen nblocks
    >R DROP               \ src dst   R: nblocks
    R> 0 ?DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    \ Handle partial final block (non-16-aligned data)
    _AEAD-REM @ 0> IF
        AES-PARTIAL-PAD 16 0 FILL
        OVER AES-PARTIAL-PAD _AEAD-REM @ CMOVE
        AES-PARTIAL-PAD AES-DIN!
        AES-PARTIAL-PAD AES-DOUT@
        AES-PARTIAL-PAD OVER _AEAD-REM @ CMOVE
    THEN
    2DROP
    AES-TAG-BUF AES-TAG@
    AES-TAG-BUF
;

: AES-DECRYPT-AEAD ( key iv aad aadlen src dst dlen tag -- flag )
    AES-TAG!
    >R >R >R             \ R: dlen dst src.  Stack: key iv aad aadlen
    _AEAD-AADLEN !
    _AEAD-AAD !
    AES-IV!
    AES-KEY!
    _AEAD-AADLEN @ AES-AAD-LEN!
    R> R> R>             \ src dst dlen
    DUP AES-DATA-LEN!
    1 AES-CMD!
    \ Feed AAD block (zero-padded to 16 bytes)
    AES-AAD-PAD 16 0 FILL
    _AEAD-AAD @ AES-AAD-PAD _AEAD-AADLEN @ CMOVE
    AES-AAD-PAD AES-DIN!
    \ Feed complete 16-byte data blocks
    DUP 15 AND _AEAD-REM !
    DUP 4 RSHIFT
    >R DROP
    R> 0 ?DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    \ Handle partial final block (non-16-aligned data)
    _AEAD-REM @ 0> IF
        AES-PARTIAL-PAD 16 0 FILL
        OVER AES-PARTIAL-PAD _AEAD-REM @ CMOVE
        AES-PARTIAL-PAD AES-DIN!
        AES-PARTIAL-PAD AES-DOUT@
        AES-PARTIAL-PAD OVER _AEAD-REM @ CMOVE
    THEN
    2DROP
    AES-STATUS@ 3 = IF -1 ELSE 0 THEN
;

\ =====================================================================
\  §1.6  SHA-3 / SHAKE Hashing
\ =====================================================================
\  BIOS primitives (hardware accelerator at MMIO 0x0780):
\    SHA3-INIT  SHA3-UPDATE  SHA3-FINAL  SHA3-STATUS@
\    SHA3-MODE! SHA3-MODE@  SHA3-SQUEEZE
\  BIOS TRNG (hardware at MMIO 0x0800):
\    RANDOM  RANDOM8  SEED-RNG
\
\  Modes: 0=SHA3-256  1=SHA3-512  2=SHAKE128  3=SHAKE256

0 CONSTANT SHA3-256-MODE
1 CONSTANT SHA3-512-MODE
2 CONSTANT SHAKE128-MODE
3 CONSTANT SHAKE256-MODE

CREATE SHA3-BUF 64 ALLOT

\ SHA3 ( addr len out -- )  SHA3-256 hash (32 bytes output).
: SHA3
    >R
    SHA3-INIT
    SHA3-UPDATE
    R> SHA3-FINAL ;

\ SHA3-512 ( addr len out -- )  SHA3-512 hash (64 bytes output).
\   Sets mode to SHA3-512, hashes, copies 64 bytes, restores SHA3-256 mode.
: SHA3-512  ( addr len out -- )
    >R
    SHA3-512-MODE SHA3-MODE!
    SHA3-INIT
    SHA3-UPDATE
    R> SHA3-FINAL
    SHA3-256-MODE SHA3-MODE! ;

: SHAKE128  ( addr len out outlen -- )
    >R >R
    SHAKE128-MODE SHA3-MODE!
    SHA3-INIT
    SHA3-UPDATE
    R> R>
    OVER SHA3-FINAL
    DROP ;

: SHAKE256  ( addr len out outlen -- )
    >R >R
    SHAKE256-MODE SHA3-MODE!
    SHA3-INIT
    SHA3-UPDATE
    R> R>
    OVER SHA3-FINAL
    DROP
    SHA3-256-MODE SHA3-MODE! ;

\ SHAKE-STREAM ( addr blocks -- )
\   Read `blocks` 32-byte chunks of continuous XOF output into addr.
\   Must be called AFTER SHAKE-INIT + SHA3-UPDATE + SHA3-FINAL has been
\   used to set up the SHAKE state.  First 32 bytes are already in DOUT
\   after FINAL.  For each subsequent block, SHA3-SQUEEZE-NEXT permutes
\   the Keccak state and refills DOUT.
\
\   BIOS primitives used:
\     SHA3-DOUT@ ( addr -- )         — read 32 bytes from DOUT
\     SHA3-SQUEEZE-NEXT ( -- )       — permute, refill DOUT
: SHAKE-STREAM ( addr blocks -- )
    0 DO
        DUP SHA3-DOUT@          \ read current DOUT block
        32 +                    \ advance addr
        SHA3-SQUEEZE-NEXT       \ permute for next block
    LOOP
    DROP ;

: .SHA3-STATUS
    SHA3-STATUS@
    DUP 0 = IF DROP ."  SHA3: idle" CR ELSE
    DUP 2 = IF DROP ."  SHA3: done" CR ELSE
    DROP ."  SHA3: unknown" CR
    THEN THEN ;

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

\ =====================================================================
\  §1.7  Unified Crypto Words
\ =====================================================================
\  High-level wrappers over AES-256-GCM (§1.5) and SHA-3 (§1.6).
\
\  HASH      ( addr len out -- )           SHA3-256 hash
\  HMAC      ( key klen msg mlen out -- )   HMAC-SHA3-256
\  ENCRYPT   ( key iv src dst len -- tag )  AES-256-GCM encrypt
\  DECRYPT   ( key iv src dst len tag -- f) AES-256-GCM decrypt
\  VERIFY    ( a1 a2 len -- flag )          constant-time compare

\ HASH ( addr len hash-addr -- )  Alias for SHA3.
: HASH  SHA3 ;

\ SHA256 ( addr len out -- )  SHA-256 hash wrapper.
: SHA256  ( addr len out -- )
    >R
    SHA256-INIT
    SHA256-UPDATE
    R> SHA256-FINAL ;

\ --- HMAC-SHA3-256 ---
\ HMAC(K,m) = SHA3((K ^ opad) || SHA3((K ^ ipad) || m))
\ SHA3-256 rate (block size) = 136 bytes

136 CONSTANT HMAC-BLKSZ

CREATE HMAC-IPAD 136 ALLOT
CREATE HMAC-OPAD 136 ALLOT
CREATE HMAC-INNER 32 ALLOT
VARIABLE _HMAC-PAD-PTR
VARIABLE _HMAC-XBYTE
VARIABLE _HMAC-OUT
VARIABLE _VERIFY-ACC

\ HMAC-PAD ( key-addr key-len pad-addr xor-byte -- )
\   Zero pad, copy key into pad, XOR entire pad with xor-byte.
: HMAC-PAD
    _HMAC-XBYTE !                     \ save xor-byte
    _HMAC-PAD-PTR !                   \ save pad-addr
    \ Zero the pad
    _HMAC-PAD-PTR @ HMAC-BLKSZ 0 FILL
    \ Copy key bytes into pad[0..klen-1]
    0 DO                               \ key-addr  (limit=klen start=0)
        DUP I + C@                     \ key-addr byte
        _HMAC-PAD-PTR @ I + C!         \ key-addr
    LOOP DROP
    \ XOR every byte of pad with xor-byte
    HMAC-BLKSZ 0 DO
        _HMAC-PAD-PTR @ I + C@
        _HMAC-XBYTE @ XOR
        _HMAC-PAD-PTR @ I + C!
    LOOP
;

\ HMAC ( key-addr key-len msg-addr msg-len out-addr -- )
\   Compute HMAC-SHA3-256.
: HMAC
    _HMAC-OUT !                        \ save out-addr
    >R >R                              \ R: mlen msg  S: key klen
    \ Build ipad and opad from same key
    2DUP HMAC-IPAD 54 HMAC-PAD        \ ipad = key XOR 0x36
    HMAC-OPAD 92 HMAC-PAD             \ opad = key XOR 0x5C  (consumes key klen)
    \ Inner hash: SHA3(ipad || message)
    SHA3-INIT
    HMAC-IPAD HMAC-BLKSZ SHA3-UPDATE
    R> R> SHA3-UPDATE                  \ msg mlen
    HMAC-INNER SHA3-FINAL
    \ Outer hash: SHA3(opad || inner)
    SHA3-INIT
    HMAC-OPAD HMAC-BLKSZ SHA3-UPDATE
    HMAC-INNER 32 SHA3-UPDATE
    _HMAC-OUT @ SHA3-FINAL
;

\ ENCRYPT ( key iv src dst len -- tag-addr )  AES-256-GCM encrypt.
: ENCRYPT  AES-ENCRYPT ;

\ DECRYPT ( key iv src dst len tag -- flag )  AES-256-GCM decrypt+verify.
: DECRYPT  AES-DECRYPT ;

\ VERIFY ( addr1 addr2 len -- flag )
\   Constant-time comparison.  Returns 0 if equal, -1 if different.
: VERIFY
    0 _VERIFY-ACC !                     \ acc = 0
    0 DO                                \ a1 a2
        OVER I + C@                     \ a1 a2 b1
        OVER I + C@                     \ a1 a2 b1 b2
        XOR _VERIFY-ACC @ OR _VERIFY-ACC !  \ acc |= (b1 ^ b2)
    LOOP 2DROP
    _VERIFY-ACC @ IF -1 ELSE 0 THEN     \ -1=different, 0=equal
;

\ =====================================================================
\  §1.8  X25519 — Elliptic Curve Diffie-Hellman (RFC 7748)
\ =====================================================================
\  BIOS primitives (per-core ISA via EXT.CRYPTO gf.x25519):
\    X25519-SCALAR! ( addr -- )    Load 32-byte scalar → ACC
\    X25519-POINT!  ( addr -- )    Set operand B address (TSRC0)
\    X25519-GO      ( -- )         Execute gf.x25519 (synchronous)
\    X25519-WAIT    ( -- )         No-op (ISA is synchronous)
\    X25519-STATUS@ ( -- n )       Always 2 (done)
\    X25519-RESULT@ ( addr -- )    Store ACC → 32 bytes

CREATE X25519-PRIV  32 ALLOT
CREATE X25519-PUB   32 ALLOT
CREATE X25519-SHARED 32 ALLOT

\ Curve25519 basepoint (u = 9, little-endian 32 bytes)
CREATE X25519-BASE  32 ALLOT
9 X25519-BASE C!
0 X25519-BASE 1 + C!  0 X25519-BASE 2 + C!  0 X25519-BASE 3 + C!
0 X25519-BASE 4 + C!  0 X25519-BASE 5 + C!  0 X25519-BASE 6 + C!
0 X25519-BASE 7 + C!  0 X25519-BASE 8 + C!  0 X25519-BASE 9 + C!
0 X25519-BASE 10 + C!  0 X25519-BASE 11 + C!  0 X25519-BASE 12 + C!
0 X25519-BASE 13 + C!  0 X25519-BASE 14 + C!  0 X25519-BASE 15 + C!
0 X25519-BASE 16 + C!  0 X25519-BASE 17 + C!  0 X25519-BASE 18 + C!
0 X25519-BASE 19 + C!  0 X25519-BASE 20 + C!  0 X25519-BASE 21 + C!
0 X25519-BASE 22 + C!  0 X25519-BASE 23 + C!  0 X25519-BASE 24 + C!
0 X25519-BASE 25 + C!  0 X25519-BASE 26 + C!  0 X25519-BASE 27 + C!
0 X25519-BASE 28 + C!  0 X25519-BASE 29 + C!  0 X25519-BASE 30 + C!
0 X25519-BASE 31 + C!

\ X25519 ( scalar-addr point-addr result-addr -- )
\   Full scalar multiply: result = clamp(scalar) * point.
: X25519 ( s p r -- )
    >R SWAP
    X25519-SCALAR!
    X25519-POINT!
    X25519-GO
    R> X25519-RESULT@ ;

\ X25519-KEYGEN ( -- )
\   Generate a random private key and compute the public key.
: X25519-KEYGEN ( -- )
    32 0 DO RANDOM8 X25519-PRIV I + C! LOOP
    X25519-PRIV X25519-BASE X25519-PUB X25519 ;

\ X25519-DH ( their-pub-addr -- )
\   Compute shared secret = our_priv * their_pub.
: X25519-DH ( addr -- )
    X25519-PRIV SWAP X25519-SHARED X25519 ;

\ =====================================================================
\  §1.10  Field ALU — Multi-Prime Coprocessor + Raw 256x256 Multiply
\ =====================================================================
\  Per-core ISA instructions (EXT.CRYPTO FB 20-2D).
\
\  BIOS primitives:
\    GF-A!       ( addr -- )    Load 32 bytes → ACC0-ACC3
\    GF-R@       ( addr -- )    Store ACC0-ACC3 → 32 bytes
\    GF-PRIME    ( n -- )       Set prime: 0=25519, 1=secp, 2=P256, 3=custom
\    LOAD-PRIME  ( p pinv -- )  Latch custom prime + Montgomery p_inv
\    FADD        ( a b r -- )   (a + b) mod p
\    FSUB        ( a b r -- )   (a - b) mod p
\    FMUL        ( a b r -- )   (a * b) mod p
\    FSQR        ( a r -- )     a^2 mod p
\    FINV        ( a r -- )     a^(p-2) mod p
\    FPOW        ( a e r -- )   a^e mod p
\    FMUL-RAW    ( a b rlo rhi -- )  256×256 → 512 raw
\    FCMOV       ( a cond -- )  Constant-time conditional move
\    FCEQ        ( a b r -- )   Constant-time equality test
\    FMAC        ( a b r -- )   (a*b + prev) mod p
\    FMUL-ADD-RAW ( a b rlo rhi -- ) Raw 512-bit MAC

\ Prime selection
: PRIME-25519  ( -- ) 0 GF-PRIME ;
: PRIME-SECP   ( -- ) 1 GF-PRIME ;
: PRIME-P256   ( -- ) 2 GF-PRIME ;
: PRIME-CUSTOM ( -- ) 3 GF-PRIME ;

\ Scratch buffers for field ops (32 bytes each)
CREATE _FA  32 ALLOT
CREATE _FB  32 ALLOT
CREATE _FR  32 ALLOT
CREATE _FRH 32 ALLOT

\ =====================================================================
\  §1.11  NTT Engine — 256-point Number Theoretic Transform
\ =====================================================================
\  Hardware-accelerated NTT for ML-KEM (Kyber) and ML-DSA (Dilithium)
\  post-quantum lattice-based cryptography.
\
\  BIOS primitives:
\    NTT-SETQ ( q -- )          Set modulus
\    NTT-IDX! ( idx -- )        Set coefficient index
\    NTT-LOAD ( addr buf -- )   Load 256 coefficients (buf: 0=A, 1=B)
\    NTT-STORE ( addr -- )      Store 256 result coefficients
\    NTT-FWD ( -- )             Forward NTT
\    NTT-INV ( -- )             Inverse NTT
\    NTT-PMUL ( -- )            Pointwise multiply A*B mod q
\    NTT-PADD ( -- )            Pointwise add (A+B) mod q
\    NTT-STATUS@ ( -- n )       Read status
\    NTT-WAIT ( -- )            Poll until done
\
\  Convenience:
\    NTT-POLYMUL ( a b r -- )   Full polynomial multiply via NTT
\      Load a→A, b→B, forward NTT both, pointwise multiply, inverse NTT,
\      store to r. Requires q already set.

\ Standard modulus constants
3329     CONSTANT Q-KYBER      \ ML-KEM (Kyber)
8380417  CONSTANT Q-DILITHIUM  \ ML-DSA (Dilithium)

\ NTT buffer selector constants
0 CONSTANT NTT-BUF-A
1 CONSTANT NTT-BUF-B

\ Internal temp buffers for NTT-POLYMUL (256 coefficients × 4 bytes)
CREATE _NTT-TMP-A 1024 ALLOT
CREATE _NTT-TMP-B 1024 ALLOT

\ NTT-POLYMUL ( a-addr b-addr r-addr -- )
\   Full polynomial multiply: r = INTT( NTT(a) · NTT(b) )
\   Modulus q must be set beforehand via NTT-SETQ.
: NTT-POLYMUL ( a-addr b-addr r-addr -- )
    >R >R                         \ ( a-addr ) R:( r-addr b-addr )
    \ Step 1: NTT(a) → _NTT-TMP-A
    NTT-BUF-A NTT-LOAD            \ load a → A
    NTT-FWD NTT-WAIT              \ forward NTT
    _NTT-TMP-A NTT-STORE          \ store NTT(a)
    \ Step 2: NTT(b) → _NTT-TMP-B
    R> NTT-BUF-A NTT-LOAD         \ load b → A
    NTT-FWD NTT-WAIT              \ forward NTT
    _NTT-TMP-B NTT-STORE          \ store NTT(b)
    \ Step 3: Pointwise multiply NTT(a) * NTT(b)
    _NTT-TMP-A NTT-BUF-A NTT-LOAD
    _NTT-TMP-B NTT-BUF-B NTT-LOAD
    NTT-PMUL NTT-WAIT             \ result = NTT(a) · NTT(b)
    \ Step 4: INTT → r
    \ To do INTT, we need result in A. Store result, reload to A.
    _NTT-TMP-A NTT-STORE          \ reuse TMP-A for product
    _NTT-TMP-A NTT-BUF-A NTT-LOAD
    NTT-INV NTT-WAIT              \ result = INTT(product)
    R> NTT-STORE                  \ store to r
;

\ .NTT-STATUS ( -- )  Print human-readable NTT status.
: .NTT-STATUS
    NTT-STATUS@
    DUP 0 = IF DROP ."  NTT: idle" CR ELSE
    DUP 2 = IF DROP ."  NTT: done" CR ELSE
    DUP 1 = IF DROP ."  NTT: busy" CR ELSE
    DROP ."  NTT: unknown" CR
    THEN THEN THEN ;

\ =====================================================================
\  §1.12  ML-KEM-512 (Kyber) — Lattice-Based Key Encapsulation
\ =====================================================================
\ Uses KEM accelerator device for FIPS 203 ML-KEM-512.
\ BIOS primitives: KEM-SEL! KEM-LOAD KEM-STORE KEM-KEYGEN KEM-ENCAPS
\                  KEM-DECAPS KEM-STATUS@
\ Buffer IDs: 0=SEED/COIN(64B) 1=PK(800B) 2=SK(1632B) 3=CT(768B) 4=SS(32B)

0 CONSTANT KBUF-SEED
1 CONSTANT KBUF-PK
2 CONSTANT KBUF-SK
3 CONSTANT KBUF-CT
4 CONSTANT KBUF-SS

32  CONSTANT KEM-SEED-SIZE
800 CONSTANT KEM-PK-SIZE
1632 CONSTANT KEM-SK-SIZE
768 CONSTANT KEM-CT-SIZE
32  CONSTANT KEM-SS-SIZE

: KYBER-KEYGEN ( seed64-addr pk-addr sk-addr -- )
    >R >R
    KBUF-SEED KEM-SEL!  64 KEM-LOAD
    KEM-KEYGEN
    KBUF-PK KEM-SEL!  R> KEM-PK-SIZE KEM-STORE
    KBUF-SK KEM-SEL!  R> KEM-SK-SIZE KEM-STORE ;

: KYBER-ENCAPS ( pk-addr coin-addr ct-addr ss-addr -- )
    >R >R
    KBUF-SEED KEM-SEL!  32 KEM-LOAD
    KBUF-PK KEM-SEL!  KEM-PK-SIZE KEM-LOAD
    KEM-ENCAPS
    KBUF-CT KEM-SEL!  R> KEM-CT-SIZE KEM-STORE
    KBUF-SS KEM-SEL!  R> KEM-SS-SIZE KEM-STORE ;

: KYBER-DECAPS ( ct-addr sk-addr ss-addr -- )
    >R
    KBUF-SK KEM-SEL!  KEM-SK-SIZE KEM-LOAD
    KBUF-CT KEM-SEL!  KEM-CT-SIZE KEM-LOAD
    KEM-DECAPS
    KBUF-SS KEM-SEL!  R> KEM-SS-SIZE KEM-STORE ;

: .KEM-STATUS
    KEM-STATUS@
    DUP 0 = IF DROP ."  KEM: idle" CR ELSE
    DUP 2 = IF DROP ."  KEM: done" CR ELSE
    DROP ."  KEM: unknown" CR
    THEN THEN ;

\ =====================================================================
\  §1.13  Hybrid PQ Key Exchange — X25519 + ML-KEM-512
\ =====================================================================
\ Combines classical X25519 ECDH with post-quantum ML-KEM-512 Kyber.
\ Both shared secrets are concatenated and fed through HKDF-Extract +
\ HKDF-Expand to derive the final hybrid shared secret.
\
\ Usage:
\   1. Both parties generate X25519 keypairs (X25519-KEYGEN)
\   2. Both parties generate Kyber keypairs (KYBER-KEYGEN with seed)
\   3. Initiator calls PQ-EXCHANGE-INIT with peer's X25519 pub + Kyber pk
\   4. Responder calls PQ-EXCHANGE-RESP with peer's X25519 pub + the ct
\
\ Scratch buffers for hybrid exchange:
CREATE _PQ-SS-X 32 ALLOT        \ X25519 shared secret
CREATE _PQ-SS-K 32 ALLOT        \ Kyber shared secret
CREATE _PQ-CAT  64 ALLOT        \ concatenated ss: X25519 || Kyber
CREATE _PQ-PRK  32 ALLOT        \ HKDF-Extract output
CREATE _PQ-COIN 32 ALLOT        \ Kyber encaps coin
CREATE _PQ-INFO 9 ALLOT         \ HKDF info string "pq-hybrid"
: _PQ-INFO-INIT
    112 _PQ-INFO C!
    113 _PQ-INFO 1 + C!
    45  _PQ-INFO 2 + C!
    104 _PQ-INFO 3 + C!
    121 _PQ-INFO 4 + C!
    98  _PQ-INFO 5 + C!
    114 _PQ-INFO 6 + C!
    105 _PQ-INFO 7 + C!
    100 _PQ-INFO 8 + C! ;
_PQ-INFO-INIT

\ =====================================================================
\  §1.9  HKDF — HMAC-based Key Derivation Function (RFC 5869)
\ =====================================================================
\  Uses HMAC-SHA3-256 as the underlying PRF.
\  Hash output length (L_H) = 32 bytes.
\
\  HKDF-EXTRACT ( salt slen ikm ilen out -- )
\    PRK = HMAC(salt, IKM)
\    If salt is 0 / slen=0, uses 32 zero bytes as salt.
\
\  HKDF-EXPAND ( prk info ilen len out -- )
\    OKM = T(1) || T(2) || ...  truncated to len bytes.
\    T(i) = HMAC(PRK, T(i-1) || info || i)

32 CONSTANT HKDF-HASHLEN

CREATE _HKDF-ZERO-SALT  32 ALLOT       \ 32 zero bytes for null-salt case
_HKDF-ZERO-SALT 32 0 FILL

\ Scratch buffers for Expand
CREATE _HKDF-T       32 ALLOT          \ T(i-1) / T(i) — running HMAC output
CREATE _HKDF-BLOCK  200 ALLOT          \ T(i-1) || info || counter (max ~200B)
VARIABLE _HKDF-PRK-PTR
VARIABLE _HKDF-INFO-PTR
VARIABLE _HKDF-INFO-LEN
VARIABLE _HKDF-OUT-PTR
VARIABLE _HKDF-REMAIN
VARIABLE _HKDF-TPREV-LEN
VARIABLE _HKDF-COUNTER

: HKDF-EXTRACT ( salt slen ikm ilen out -- )
    >R                                  \ R: out
    2SWAP                               \ ikm ilen salt slen
    DUP 0= IF                           \ null salt → use zero-salt
        2DROP _HKDF-ZERO-SALT 32
    THEN
    \ Stack: ikm ilen salt slen   R: out
    \ HMAC( salt, IKM ) → out
    2SWAP                               \ salt slen ikm ilen
    R>                                  \ salt slen ikm ilen out
    HMAC
;

: HKDF-EXPAND ( prk info ilen len out -- )
    _HKDF-OUT-PTR !
    _HKDF-REMAIN !
    _HKDF-INFO-LEN !
    _HKDF-INFO-PTR !
    _HKDF-PRK-PTR !
    0 _HKDF-TPREV-LEN !                \ T(0) = empty
    1 _HKDF-COUNTER !                  \ counter starts at 1
    BEGIN _HKDF-REMAIN @ 0> WHILE
        \ --- Build block: T(i-1) || info || counter_byte ---
        \ Step 1: copy T(i-1) into _HKDF-BLOCK[0..]
        _HKDF-TPREV-LEN @ 0> IF
            _HKDF-T _HKDF-BLOCK _HKDF-TPREV-LEN @ CMOVE
        THEN
        \ Step 2: append info at _HKDF-BLOCK[tprev_len..]
        _HKDF-INFO-PTR @
        _HKDF-BLOCK _HKDF-TPREV-LEN @ +
        _HKDF-INFO-LEN @ CMOVE
        \ Step 3: append counter byte
        _HKDF-COUNTER @
        _HKDF-BLOCK _HKDF-TPREV-LEN @ + _HKDF-INFO-LEN @ + C!
        \ block_len = tprev_len + info_len + 1
        _HKDF-TPREV-LEN @ _HKDF-INFO-LEN @ + 1+
        \ --- HMAC(PRK, block) → _HKDF-T ---
        >R                              \ R: block_len
        _HKDF-PRK-PTR @ HKDF-HASHLEN
        _HKDF-BLOCK R>
        _HKDF-T HMAC                   \ ( )
        \ --- Copy min(HASHLEN, remain) → output ---
        _HKDF-REMAIN @ HKDF-HASHLEN MIN
        _HKDF-T _HKDF-OUT-PTR @ ROT CMOVE
        \ Update output pointer and remaining count
        _HKDF-REMAIN @ HKDF-HASHLEN MIN
        DUP _HKDF-OUT-PTR @ + _HKDF-OUT-PTR !
        _HKDF-REMAIN @ SWAP - _HKDF-REMAIN !
        \ Next iteration
        HKDF-HASHLEN _HKDF-TPREV-LEN !
        _HKDF-COUNTER @ 1+ _HKDF-COUNTER !
    REPEAT
;

\ =====================================================================
\  §1.9b  HMAC-SHA256 / HKDF-SHA256 (for standard TLS 1.3)
\ =====================================================================
\  Same HMAC/HKDF constructions as §1.7/§1.9, but using SHA-256 instead
\  of SHA3-256.  SHA-256 block size = 64 bytes, output = 32 bytes.
\
\  HMAC-SHA256 ( key klen msg mlen out -- )
\  HKDF-SHA256-EXTRACT ( salt slen ikm ilen out -- )
\  HKDF-SHA256-EXPAND  ( prk info ilen len out -- )

64 CONSTANT HMAC256-BLKSZ

CREATE HMAC256-IPAD 64 ALLOT
CREATE HMAC256-OPAD 64 ALLOT
CREATE HMAC256-INNER 32 ALLOT
VARIABLE _HMAC256-PAD-PTR
VARIABLE _HMAC256-XBYTE
VARIABLE _HMAC256-OUT

: HMAC256-PAD ( key-addr key-len pad-addr xor-byte -- )
    _HMAC256-XBYTE !
    _HMAC256-PAD-PTR !
    _HMAC256-PAD-PTR @ HMAC256-BLKSZ 0 FILL
    0 DO
        DUP I + C@
        _HMAC256-PAD-PTR @ I + C!
    LOOP DROP
    HMAC256-BLKSZ 0 DO
        _HMAC256-PAD-PTR @ I + C@
        _HMAC256-XBYTE @ XOR
        _HMAC256-PAD-PTR @ I + C!
    LOOP
;

: HMAC-SHA256 ( key-addr key-len msg-addr msg-len out-addr -- )
    _HMAC256-OUT !
    >R >R
    2DUP HMAC256-IPAD 54 HMAC256-PAD
    HMAC256-OPAD 92 HMAC256-PAD
    \ Inner hash: SHA256(ipad || message)
    SHA256-INIT
    HMAC256-IPAD HMAC256-BLKSZ SHA256-UPDATE
    R> R> SHA256-UPDATE
    HMAC256-INNER SHA256-FINAL
    \ Outer hash: SHA256(opad || inner)
    SHA256-INIT
    HMAC256-OPAD HMAC256-BLKSZ SHA256-UPDATE
    HMAC256-INNER 32 SHA256-UPDATE
    _HMAC256-OUT @ SHA256-FINAL
;

\ Scratch buffers for HKDF-SHA256
CREATE _HKDF256-ZERO-SALT  32 ALLOT
_HKDF256-ZERO-SALT 32 0 FILL
CREATE _HKDF256-T       32 ALLOT
CREATE _HKDF256-BLOCK  200 ALLOT
VARIABLE _HKDF256-PRK-PTR
VARIABLE _HKDF256-INFO-PTR
VARIABLE _HKDF256-INFO-LEN
VARIABLE _HKDF256-OUT-PTR
VARIABLE _HKDF256-REMAIN
VARIABLE _HKDF256-TPREV-LEN
VARIABLE _HKDF256-COUNTER

: HKDF-SHA256-EXTRACT ( salt slen ikm ilen out -- )
    >R
    2SWAP
    DUP 0= IF
        2DROP _HKDF256-ZERO-SALT 32
    THEN
    2SWAP
    R>
    HMAC-SHA256
;

: HKDF-SHA256-EXPAND ( prk info ilen len out -- )
    _HKDF256-OUT-PTR !
    _HKDF256-REMAIN !
    _HKDF256-INFO-LEN !
    _HKDF256-INFO-PTR !
    _HKDF256-PRK-PTR !
    0 _HKDF256-TPREV-LEN !
    1 _HKDF256-COUNTER !
    BEGIN _HKDF256-REMAIN @ 0> WHILE
        _HKDF256-TPREV-LEN @ 0> IF
            _HKDF256-T _HKDF256-BLOCK _HKDF256-TPREV-LEN @ CMOVE
        THEN
        _HKDF256-INFO-PTR @
        _HKDF256-BLOCK _HKDF256-TPREV-LEN @ +
        _HKDF256-INFO-LEN @ CMOVE
        _HKDF256-COUNTER @
        _HKDF256-BLOCK _HKDF256-TPREV-LEN @ + _HKDF256-INFO-LEN @ + C!
        _HKDF256-TPREV-LEN @ _HKDF256-INFO-LEN @ + 1+
        >R
        _HKDF256-PRK-PTR @ 32
        _HKDF256-BLOCK R>
        _HKDF256-T HMAC-SHA256
        _HKDF256-REMAIN @ 32 MIN
        _HKDF256-T _HKDF256-OUT-PTR @ ROT CMOVE
        _HKDF256-REMAIN @ 32 MIN
        DUP _HKDF256-OUT-PTR @ + _HKDF256-OUT-PTR !
        _HKDF256-REMAIN @ SWAP - _HKDF256-REMAIN !
        32 _HKDF256-TPREV-LEN !
        _HKDF256-COUNTER @ 1+ _HKDF256-COUNTER !
    REPEAT
;

\ PQ-DERIVE ( out -- )
\   Internal: HKDF-derive final 32-byte key from concatenated secrets.
\   Assumes _PQ-CAT already has 64 bytes of combined keying material.
: PQ-DERIVE ( out-addr -- )
    >R
    \ HKDF-Extract: salt=empty(0), ikm=_PQ-CAT(64B) → _PQ-PRK
    0 0 _PQ-CAT 64 _PQ-PRK HKDF-EXTRACT
    \ HKDF-Expand: prk=_PQ-PRK, info="pq-hybrid"(9B), len=32, out
    _PQ-PRK _PQ-INFO 9 32 R> HKDF-EXPAND ;

\ PQ-EXCHANGE-INIT ( their-x-pub kyber-pk ct-out ss-out -- )
\   Initiator side:
\   1. X25519-DH with their X25519 public key → _PQ-SS-X
\   2. Generate random coin, KYBER-ENCAPS with their Kyber pk → ct + _PQ-SS-K
\   3. Concatenate, HKDF-derive → ss-out
: PQ-EXCHANGE-INIT ( their-x-pub kyber-pk ct-out ss-out -- )
    >R >R                          \ R: ss-out ct-out
    \ X25519 DH
    SWAP                            \ Stack: kyber-pk their-x-pub
    X25519-PRIV OVER _PQ-SS-X X25519
    DROP                            \ Stack: kyber-pk
    \ Generate random coin for Kyber
    32 0 DO RANDOM8 _PQ-COIN I + C! LOOP
    \ KYBER-ENCAPS ( pk coin ct ss -- )
    _PQ-COIN R> _PQ-SS-K KYBER-ENCAPS
    \ Concatenate: _PQ-CAT = _PQ-SS-X || _PQ-SS-K
    _PQ-SS-X _PQ-CAT 32 CMOVE
    _PQ-SS-K _PQ-CAT 32 + 32 CMOVE
    \ Derive final key
    R> PQ-DERIVE ;

\ PQ-EXCHANGE-RESP ( their-x-pub ct kyber-sk ss-out -- )
\   Responder side:
\   1. X25519-DH with their X25519 public key → _PQ-SS-X
\   2. KYBER-DECAPS with ct and our Kyber sk → _PQ-SS-K
\   3. Concatenate, HKDF-derive → ss-out
: PQ-EXCHANGE-RESP ( their-x-pub ct kyber-sk ss-out -- )
    >R                              \ R: ss-out
    \ Stack: their-x-pub ct kyber-sk
    ROT                             \ Stack: ct kyber-sk their-x-pub
    X25519-PRIV OVER _PQ-SS-X X25519
    DROP                            \ Stack: ct kyber-sk
    \ KYBER-DECAPS ( ct sk ss -- )
    SWAP _PQ-SS-K KYBER-DECAPS
    \ Concatenate
    _PQ-SS-X _PQ-CAT 32 CMOVE
    _PQ-SS-K _PQ-CAT 32 + 32 CMOVE
    \ Derive final key
    R> PQ-DERIVE ;

\ =====================================================================
\  §1.12  HBW Math RAM Allocator
\ =====================================================================
\
\  Simple bump allocator for the High-Bandwidth (HBW) math RAM.
\  HBW is 3 MiB of dedicated internal BRAM (banks 1-3) at addresses
\  starting from HBW-BASE (typically 0xFFD0_0000).  Ideal for large
\  tile/SIMD working buffers to avoid contention with Bank 0 (system
\  RAM, dictionary, stacks, heap).
\
\  Unlike the heap (§1.1), HBW uses a bump allocator — no individual
\  FREE.  Use HBW-RESET to reclaim all HBW memory at once.
\
\  HBW-INIT     ( -- )          initialise HBW allocator
\  HBW-ALLOT    ( u -- addr )   allocate u bytes from HBW, return addr
\  HBW-TALIGN   ( -- )          align HBW-HERE to 64-byte tile boundary
\  HBW-RESET    ( -- )          reclaim all HBW memory
\  HBW-FREE     ( -- u )        bytes remaining in HBW
\  .HBW         ( -- )          display HBW status

VARIABLE HBW-HERE    0 HBW-HERE !
VARIABLE HBW-LIMIT   0 HBW-LIMIT !

\ HBW-INIT ( -- )  set up HBW pointers from SysInfo registers
: HBW-INIT  ( -- )
    HBW-BASE HBW-HERE !
    HBW-BASE HBW-SIZE + HBW-LIMIT ! ;

\ HBW-ALLOT ( u -- addr )  bump-allocate u bytes from HBW
: HBW-ALLOT  ( u -- addr )
    HBW-HERE @ SWAP                  \ addr u
    OVER + DUP HBW-LIMIT @ > ABORT" HBW overflow"
    HBW-HERE !                        \ update pointer
    ;                                 \ leave addr on stack

\ HBW-ALLOT? ( u -- addr ior )  like HBW-ALLOT but returns ior
: HBW-ALLOT?  ( u -- addr ior )
    HBW-HERE @ SWAP
    OVER + DUP HBW-LIMIT @ > IF
        2DROP 0 -1 EXIT              \ overflow → (0, -1)
    THEN
    HBW-HERE ! 0 ;                   \ success  → (addr, 0)

\ HBW-TALIGN ( -- )  align HBW-HERE up to 64-byte boundary
: HBW-TALIGN  ( -- )
    HBW-HERE @  63 + -64 AND  HBW-HERE ! ;

\ HBW-RESET ( -- )  reclaim all HBW memory (bulk free)
: HBW-RESET  ( -- )
    HBW-BASE HBW-HERE ! ;

\ HBW-FREE ( -- u )  bytes remaining in HBW
: HBW-FREE  ( -- u )
    HBW-LIMIT @ HBW-HERE @ - ;

\ .HBW ( -- )  display HBW status
: .HBW  ( -- )
    ."  HBW Math RAM:" CR
    ."    Base = " HBW-BASE . CR
    ."    Size = " HBW-SIZE . ."  bytes" CR
    ."    Used = " HBW-HERE @ HBW-BASE - . ."  bytes" CR
    ."    Free = " HBW-FREE . ."  bytes" CR ;

HBW-INIT      \ initialise at load time

\ =====================================================================
\  §1.12a  External Memory Allocator
\ =====================================================================
\
\  Bump allocator for external RAM (HyperRAM / SDRAM) starting at
\  EXT-MEM-BASE (typically 0x0010_0000, right after 1 MiB Bank 0).
\  Modelled after the HBW allocator (§1.12).
\
\  On systems without external memory (EXT-MEM-SIZE = 0) all words
\  degrade gracefully: XMEM? returns false, XMEM-ALLOT aborts,
\  XMEM-FREE returns 0.
\
\  XMEM?        ( -- flag )     true if external memory is present
\  XMEM-INIT    ( -- )          initialise ext mem allocator
\  XMEM-ALLOT   ( u -- addr )   allocate u bytes, return start addr
\  XMEM-TALIGN  ( -- )          align XMEM-HERE to 64-byte tile boundary
\  XMEM-RESET   ( -- )          reclaim all ext mem
\  XMEM-FREE    ( -- u )        bytes remaining in ext mem
\  .XMEM        ( -- )          display ext mem status

VARIABLE XMEM-HERE   0 XMEM-HERE !
VARIABLE XMEM-LIMIT  0 XMEM-LIMIT !

\ -- XMEM free-list for individual block reclaim --
\   Each freed block stores at its address:
\     +0  size   (bytes)
\     +8  next   (ptr to next free block, or 0)
\   Minimum recyclable block: 16 bytes (2 cells).
VARIABLE XMEM-FL     0 XMEM-FL !    \ free-list head (0 = empty)
VARIABLE FL-PREV                     \ search scratch
VARIABLE FL-CURR                     \ search scratch
VARIABLE FL-NEED                     \ requested bytes during first-fit
VARIABLE FL-NEXT                     \ successor preserved while splitting

\ XMEM-FREE-BLOCK ( addr size -- )  return a block to the XMEM free-list
\   Validates that addr falls within [EXT-MEM-BASE, XMEM-LIMIT),
\   addr+size does not exceed XMEM-LIMIT, and size >= 16.
: XMEM-FREE-BLOCK  ( addr size -- )
    DUP 16 < ABORT" XMEM-FREE: block too small"
    OVER EXT-MEM-BASE < ABORT" XMEM-FREE: addr below base"
    OVER XMEM-LIMIT @ >= ABORT" XMEM-FREE: exceeds limit"
    \ Check size <= limit-addr before any address addition.  The old
    \ addr+size comparison could wrap and admit a span crossing the limit.
    2DUP SWAP XMEM-LIMIT @ SWAP - >
    ABORT" XMEM-FREE: exceeds limit"
    OVER !                            \ addr+0 = size
    XMEM-FL @ OVER 8 + !             \ addr+8 = old head
    XMEM-FL ! ;                       \ head = addr

\ _XMEM-FL-REPLACE ( replacement -- )
\   Replace FL-CURR in the free-list with replacement (or unlink it when 0).
: _XMEM-FL-REPLACE  ( replacement -- )
    FL-PREV @ 0= IF
        XMEM-FL !
    ELSE
        FL-PREV @ 8 + !
    THEN ;

\ (XMEM-FL-FIND) ( u -- addr true | false )
\   First-fit search of the XMEM free-list.  A larger reclaimed block is
\   split so a small allocation cannot strand the unused tail.
: (XMEM-FL-FIND)  ( u -- addr true | false )
    FL-NEED !
    0 FL-PREV !   XMEM-FL @ FL-CURR !
    BEGIN FL-CURR @ WHILE
        FL-CURR @ @ FL-NEED @ >= IF       \ curr.size >= need ?
            FL-CURR @ @ FL-NEED @ -
            DUP 16 >= IF
                \ Keep the tail as a recyclable free block.  Preserve the
                \ successor before writing the tail header: for an 8-byte
                \ request the tail starts on the current node's +8 next cell.
                FL-CURR @ 8 + @ FL-NEXT !
                FL-CURR @ FL-NEED @ +
                SWAP OVER !
                FL-NEXT @ OVER 8 + !
                _XMEM-FL-REPLACE
            ELSE
                DROP FL-CURR @ 8 + @ _XMEM-FL-REPLACE
            THEN
            FL-CURR @ TRUE EXIT
        THEN
        FL-CURR @ FL-PREV !
        FL-CURR @ 8 + @ FL-CURR !
    REPEAT
    FALSE ;

\ XMEM? ( -- flag )  true if external memory hardware reports non-zero size
: XMEM?  ( -- flag )
    EXT-MEM-SIZE 0> ;

\ XMEM-INIT ( -- )  read base/size from SysInfo, set up pointers
: XMEM-INIT  ( -- )
    XMEM? IF
        EXT-MEM-BASE XMEM-HERE !
        EXT-MEM-BASE EXT-MEM-SIZE + XMEM-LIMIT !
    ELSE
        0 XMEM-HERE !  0 XMEM-LIMIT !
    THEN ;

\ XMEM-ALLOT ( u -- addr )  allocate u bytes from ext mem
\   Tries the free-list first (first-fit), then falls back to bump.
: XMEM-ALLOT  ( u -- addr )
    XMEM? 0= ABORT" No external memory"
    DUP 0< OVER 0= OR ABORT" Invalid ext mem size"
    DUP (XMEM-FL-FIND) IF              \ found a recycled block
        NIP EXIT
    THEN
    \ Prove the request fits in the remaining span before adding it to the
    \ bump pointer; base+size must never be used as the bounds check.
    DUP XMEM-LIMIT @ XMEM-HERE @ - > ABORT" Ext mem overflow"
    XMEM-HERE @ SWAP
    OVER +
    XMEM-HERE ! ;

\ XMEM-ALLOT? ( u -- addr ior )  like XMEM-ALLOT but returns ior
: XMEM-ALLOT?  ( u -- addr ior )
    XMEM? 0= IF DROP 0 -1 EXIT THEN
    DUP 0< OVER 0= OR IF DROP 0 -1 EXIT THEN
    DUP (XMEM-FL-FIND) IF NIP 0 EXIT THEN
    DUP XMEM-LIMIT @ XMEM-HERE @ - > IF
        DROP 0 -1 EXIT
    THEN
    XMEM-HERE @ SWAP
    OVER +
    XMEM-HERE ! 0 ;

\ =====================================================================
\  §1.0b  Xmem-aware allocation dispatch
\ =====================================================================
\
\  When extended memory is available, ALLOCATE routes to XMEM-ALLOT?
\  with an 8-byte prefix storing the total block size (usable + 8).
\  FREE reads this total to return the full block to XMEM-FREE-BLOCK.
\  DMA-ALLOCATE / DMA-FREE always use the Bank 0 heap (required by
\  DMA engines that dereference s.mem[] directly).

\ ALLOCATE ( u -- addr ior )
\   Xmem-aware: routes to xmem when available, Bank 0 otherwise.
: ALLOCATE  ( u -- addr ior )
    XMEM? IF
        ?CORE0
        \ The aligned payload plus its 8-byte prefix must remain a positive
        \ signed cell.  Reject before either addition or free-list search.
        DUP 0< OVER 0= OR
        OVER 0x7FFFFFFFFFFFFFF0 > OR IF DROP 0 -1 EXIT THEN
        \ Round to 8-byte alignment, minimum 16, add 8-byte prefix
        7 + -8 AND DUP 16 < IF DROP 16 THEN 8 +  ( total )
        DUP XMEM-ALLOT?                  ( total addr ior )
        IF  2DROP 0 -1 EXIT  THEN       ( total addr )
        TUCK !                           \ store total at addr  ( addr )
        8 +  0  EXIT                     \ return addr+8, ior=0
    THEN
    (BANK0-ALLOCATE) ;

\ FREE ( addr -- )
\   Auto-routes: xmem pointers → XMEM-FREE-BLOCK, Bank 0 → (BANK0-FREE).
: FREE  ( addr -- )
    DUP 0= IF DROP EXIT THEN
    DUP MEM-SIZE >= IF
        \ Xmem block: total-size stored 8 bytes before user pointer
        8 -  DUP @                       ( block-addr total-size )
        XMEM-FREE-BLOCK  EXIT
    THEN
    (BANK0-FREE) ;

\ RESIZE ( a1 u -- a2 ior )
\   Xmem blocks: alloc new, copy, free old (no in-place growth).
\   Bank 0 blocks: full in-place resize support.
VARIABLE _RS-OLD   \ saved old-addr for xmem resize
: RESIZE  ( a1 u -- a2 ior )
    OVER MEM-SIZE >= IF
        \ Xmem path: alloc new, copy min(old,new), free old
        OVER _RS-OLD !                   \ save a1
        DUP ALLOCATE IF DROP DROP -1 EXIT THEN  ( a1 u a2 )
        SWAP                             ( a1 a2 u )
        _RS-OLD @ 8 - @  8 -            ( a1 a2 u old-usable )
        MIN                              ( a1 a2 copy-len )
        >R SWAP R>                       ( a2 a1 copy-len )
        2 PICK SWAP CMOVE               ( a2 ; copied a1→a2 )
        _RS-OLD @ FREE  0  EXIT
    THEN
    (BANK0-RESIZE) ;

\ DMA-ALLOCATE ( u -- addr ior )
\   Always allocates from Bank 0 heap (DMA-safe).
: DMA-ALLOCATE  ( u -- addr ior )
    (BANK0-ALLOCATE) ;

\ DMA-FREE ( addr -- )
\   Free a Bank 0 heap block.
: DMA-FREE  ( addr -- )
    (BANK0-FREE) ;

\ DMA-RESIZE ( a1 u -- a2 ior )
\   Resize a Bank 0 heap block.
: DMA-RESIZE  ( a1 u -- a2 ior )
    (BANK0-RESIZE) ;

\ XMEM-TALIGN ( -- )  align XMEM-HERE up to 64-byte boundary
: XMEM-TALIGN  ( -- )
    XMEM-HERE @  63 + -64 AND  XMEM-HERE ! ;

VARIABLE XMEM-FLOOR  0 XMEM-FLOOR !

\ XMEM-RESET ( -- )  reclaim all external memory (bulk free)
\   Respects XMEM-FLOOR — will not reset below the userland zone.
\   Also clears the free-list (all blocks return to the bump region).
: XMEM-RESET  ( -- )
    XMEM? IF
        XMEM-FLOOR @ ?DUP IF XMEM-HERE ! ELSE EXT-MEM-BASE XMEM-HERE ! THEN
        0 XMEM-FL !
    THEN ;

\ XMEM-FREE ( -- u )  bytes remaining in ext mem
: XMEM-FREE  ( -- u )
    XMEM? IF XMEM-LIMIT @ XMEM-HERE @ - ELSE 0 THEN ;

\ .XMEM ( -- )  display external memory status
: .XMEM  ( -- )
    ."  External RAM:" CR
    XMEM? IF
        ."    Base  = " EXT-MEM-BASE . CR
        ."    Size  = " EXT-MEM-SIZE . ."  bytes" CR
        ."    Used  = " XMEM-HERE @ EXT-MEM-BASE - . ."  bytes" CR
        ."    Free  = " XMEM-FREE . ."  bytes" CR
    ELSE
        ."    (not present)" CR
    THEN ;

XMEM-INIT      \ initialise at load time

\ XBUF ( size "name" -- )  Allocate a data buffer, preferring ext mem.
\   When ext mem is present, the buffer lives in XMEM (saving system
\   dictionary space); otherwise falls back to a normal CREATE/ALLOT.
\   Either way, executing "name" pushes the buffer's start address.
\   Also advances XMEM-FLOOR to protect kernel allocations from XMEM-RESET.
: XBUF  ( size "name" -- )
    XMEM? IF
        XMEM-ALLOT CONSTANT
        XMEM-HERE @ XMEM-FLOOR !
    ELSE
        CREATE ALLOT
    THEN ;

\ =====================================================================
\  §1.15  Userland Memory Isolation
\ =====================================================================
\
\  Provides separate dictionary space for user-loaded modules
\  (tools.f, user scripts) in external RAM, protecting the kernel
\  dictionary in system RAM from overflow.
\
\  When ENTER-USERLAND is called, the Forth dictionary pointer (HERE)
\  is redirected to external memory.  All subsequent CREATE, ALLOT,
\  : definitions, VARIABLEs, etc. compile into the userland zone.
\  System words remain in Bank 0 and are still accessible.
\
\  Memory layout when userland is active (ext mem present):
\
\    System RAM (Bank 0, 1 MiB):
\      0x00000 .. dict_free   BIOS code + dictionary
\      dict_free .. ~0x7F000  KDOS dictionary + system heap
\      0x80000 .. 0xFFFFF     Stacks (data + return)
\
\    External RAM (128 MiB by default, at 0x100000):
\      0x100000 .. +U-ZONE    Userland dictionary (HERE when ULAND=1)
\      +U-ZONE  .. end        XMEM general allocator
\
\  Words:
\    ULAND           ( -- addr )  flag variable: 0=system, 1=userland
\    ENTER-USERLAND  ( -- )       switch HERE to userland zone
\    LEAVE-USERLAND  ( -- )       switch HERE back to system dict
\    .USERLAND       ( -- )       display userland status

33554432 CONSTANT U-ZONE-SIZE  \ 32 MiB reserved for userland dictionary

VARIABLE ULAND          0 ULAND !
VARIABLE SYS-HERE-SAVE  0 SYS-HERE-SAVE !
VARIABLE U-DICT-HERE    0 U-DICT-HERE !
VARIABLE U-DICT-BASE    0 U-DICT-BASE !
VARIABLE U-INIT-DONE    0 U-INIT-DONE !

\ USERLAND-INIT ( -- )  Partition ext mem: [0..U-ZONE) = userland,
\   [U-ZONE..end) = XMEM.  Called lazily on first ENTER-USERLAND.
\   No-op if ext mem is absent or already initialised.
: USERLAND-INIT  ( -- )
    U-INIT-DONE @ IF EXIT THEN
    XMEM? 0= IF EXIT THEN
    \ Start userland dict above any live prior XMEM allocations.  Loader
    \ source buffers use reclaimable ALLOCATE/FREE storage, while persistent
    \ XBUF kernel buffers remain below this boundary.  Cell-align for safe
    \ @ / ! access.
    XMEM-HERE @ 7 + -8 AND
    DUP U-ZONE-SIZE + EXT-MEM-BASE EXT-MEM-SIZE + >
        ABORT" Insufficient ext mem for userland dictionary"
    DUP U-DICT-BASE ! U-DICT-HERE !
    \ Push XMEM allocator past the userland zone
    U-DICT-BASE @ U-ZONE-SIZE +  DUP XMEM-HERE ! XMEM-FLOOR !
    1 U-INIT-DONE ! ;

\ ENTER-USERLAND ( -- )  Save system HERE, redirect to userland dict.
: ENTER-USERLAND  ( -- )
    XMEM? 0= IF ." No ext mem -- userland disabled" CR EXIT THEN
    ULAND @ IF EXIT THEN             \ already in userland
    U-INIT-DONE @ 0= IF USERLAND-INIT THEN
    HERE SYS-HERE-SAVE !             \ save system dict pointer
    U-DICT-HERE @ HERE - ALLOT       \ HERE <- userland dict pointer
    1 ULAND ! ;

\ LEAVE-USERLAND ( -- )  Save userland HERE, restore system dict.
: LEAVE-USERLAND  ( -- )
    ULAND @ 0= IF EXIT THEN          \ not in userland
    HERE U-DICT-HERE !               \ save userland dict pointer
    SYS-HERE-SAVE @ HERE - ALLOT     \ HERE <- system dict pointer
    0 ULAND ! ;

\ U-HERE ( -- addr )  Current userland dictionary pointer.
: U-HERE  ( -- addr )
    ULAND @ IF HERE ELSE U-DICT-HERE @ THEN ;

\ U-USED ( -- u )  Bytes used in userland dictionary.
: U-USED  ( -- u )
    ULAND @ IF HERE ELSE U-DICT-HERE @ THEN
    U-DICT-BASE @ - ;

\ U-FREE ( -- u )  Bytes remaining in userland dictionary zone.
: U-FREE  ( -- u )
    U-ZONE-SIZE U-USED - ;

\ .USERLAND ( -- )  Display userland status.
: .USERLAND  ( -- )
    ." Userland:" CR
    XMEM? IF
        ."   Mode  = " ULAND @ IF ." ACTIVE" ELSE ." system" THEN CR
        ."   Base  = " U-DICT-BASE @ . CR
        ."   Used  = " U-USED . ." bytes" CR
        ."   Free  = " U-FREE . ." bytes" CR
    ELSE
        ."   (no ext mem -- userland disabled)" CR
    THEN ;

\ =====================================================================
\  §1.1b  Arena Allocator
\ =====================================================================
\
\  Region-aware scoped allocation for scratch memory.
\  An arena is a pre-allocated region with O(1) bump allocation
\  and O(1) bulk deallocation.  No per-object headers or free list.
\
\  Arena descriptor (4 cells = 32 bytes, in dictionary):
\    +0   base     start of data region
\    +8   size     total capacity in bytes
\    +16  ptr      current bump pointer
\    +24  source   0 = heap, 1 = XMEM, 2 = HBW
\
\  Full design: docs/arenas.md

\ -- Source constants --
0 CONSTANT A-HEAP    \ arena backed by Bank 0 heap
1 CONSTANT A-XMEM    \ arena backed by external RAM
2 CONSTANT A-HBW     \ arena backed by HBW math RAM

\ -- Field accessors --
: A.BASE    ( arena -- addr )  ;           \ +0
: A.SIZE    ( arena -- addr )  8 + ;       \ +8
: A.PTR     ( arena -- addr )  16 + ;      \ +16
: A.SOURCE  ( arena -- addr )  24 + ;      \ +24

\ -- Scratch variables --
VARIABLE AR-SZ     \ requested size
VARIABLE AR-SRC    \ source id
VARIABLE AR-BLK    \ backing block address

\ (AR-ALLOC-BACKING) ( size source -- addr ior )
\   Dispatch to the correct region allocator.
\   Uses ?-variants so all paths return ior uniformly.
: (AR-ALLOC-BACKING)  ( size source -- addr ior )
    DUP 0 = IF  DROP ALLOCATE EXIT  THEN
    DUP 1 = IF  DROP XMEM-ALLOT? EXIT  THEN
    2 = IF  HBW-ALLOT? EXIT  THEN
    DROP 0 -1 ;    \ unknown source

\ (AR-FREE-BACKING) ( addr size source -- )
\   Free the backing block.  Heap blocks are individually freed;
\   XMEM blocks are returned to the XMEM free-list for reuse.
\   HBW blocks are still abandoned until HBW-RESET.
: (AR-FREE-BACKING)  ( addr size source -- )
    DUP 0 = IF  DROP DROP FREE EXIT  THEN
    1 = IF  XMEM-FREE-BLOCK EXIT  THEN
    2DROP ;    \ HBW — abandoned (short-lived, 3 MiB region)

\ ARENA-NEW ( size source -- arena ior )
\   Allocate a backing region, build descriptor in dictionary.
\   Sources: 0=A-HEAP, 1=A-XMEM, 2=A-HBW.
\   NOTE: the 32-byte descriptor is permanently committed to the
\   dictionary.  For temporary arenas created/destroyed in a loop,
\   use ARENA-NEW-AT with a pre-allocated descriptor address.
\ Core-0 only — uses shared scratch variables (AR-SZ, AR-SRC, AR-BLK).
: ARENA-NEW  ( size source -- arena ior )
    ?CORE0
    OVER 0= IF  2DROP 0 -1 EXIT  THEN      \ zero size → fail
    AR-SRC !  AR-SZ !
    AR-SZ @ AR-SRC @ (AR-ALLOC-BACKING) IF
        DROP 0 -1 EXIT                      \ alloc failed
    THEN
    AR-BLK !
    HERE                                     ( arena )
    AR-BLK @ ,                               \ +0  base
    AR-SZ @ ,                                \ +8  size
    AR-BLK @ ,                               \ +16 ptr = base (empty)
    AR-SRC @ ,                               \ +24 source
    0 ;                                      ( arena 0 )

\ ARENA-NEW-AT ( desc size source -- ior )
\   Like ARENA-NEW but writes the 32-byte descriptor at a user-provided
\   address instead of consuming dictionary space.  Useful for temporary
\   arenas created/destroyed in a loop — avoids the slow dictionary leak.
\   'desc' must point to >= 32 bytes of writable, cell-aligned storage
\   (e.g. a CREATE/ALLOT block, a VARIABLE cluster, or an arena-allotted
\   region in another arena).
\ Core-0 only — uses shared scratch variables (AR-SZ, AR-SRC, AR-BLK).
: ARENA-NEW-AT  ( desc size source -- ior )
    ?CORE0
    OVER 0= IF  DROP 2DROP -1 EXIT  THEN     \ zero size → fail
    AR-SRC !  AR-SZ !                         ( desc )
    AR-SZ @ AR-SRC @ (AR-ALLOC-BACKING) IF   ( desc 0 )
        2DROP -1 EXIT                         \ alloc failed
    THEN
    AR-BLK !                                  ( desc )
    AR-BLK @ OVER !                           \ +0  base
    AR-SZ @  OVER 8 + !                       \ +8  size
    AR-BLK @ OVER 16 + !                      \ +16 ptr = base
    AR-SRC @ SWAP 24 + !                      \ +24 source
    0 ;                                       ( 0 )

\ ARENA-USED ( arena -- u )  bytes consumed
: ARENA-USED  ( arena -- u )
    DUP A.PTR @  SWAP A.BASE @ - ;

\ ARENA-FREE ( arena -- u )  bytes remaining
: ARENA-FREE  ( arena -- u )
    DUP A.SIZE @  SWAP ARENA-USED - ;

\ ARENA-ALLOT ( arena u -- addr )
\   Bump-allocate u bytes (8-byte aligned).  Aborts on overflow
\   or if the arena has been destroyed.
: ARENA-ALLOT  ( arena u -- addr )
    OVER A.BASE @ 0= ABORT" arena destroyed"
    7 + -8 AND                               ( arena u-aligned )
    OVER ARENA-FREE OVER < ABORT" arena full"
    OVER A.PTR @                             ( arena u addr )
    -ROT                                     ( addr arena u )
    OVER A.PTR @ +  SWAP A.PTR ! ;           ( addr )

\ ARENA-ALLOT? ( arena u -- addr ior )
\   Like ARENA-ALLOT but returns ior instead of aborting.
: ARENA-ALLOT?  ( arena u -- addr ior )
    OVER A.BASE @ 0= IF  2DROP 0 -1 EXIT  THEN  \ destroyed
    7 + -8 AND                               ( arena u-aligned )
    OVER ARENA-FREE OVER < IF
        2DROP 0 -1 EXIT                      \ overflow
    THEN
    OVER A.PTR @                             ( arena u addr )
    -ROT                                     ( addr arena u )
    OVER A.PTR @ +  SWAP A.PTR !             ( addr )
    0 ;

\ ARENA-RESET ( arena -- )
\   Rewind ptr to base.  All allocations logically freed.  O(1).
: ARENA-RESET  ( arena -- )
    DUP A.BASE @  SWAP A.PTR ! ;

\ ARENA-DESTROY ( arena -- )
\   Free the backing region and zero the descriptor.
\   Heap blocks are individually freed via FREE.
\   XMEM blocks are returned to the XMEM free-list for reuse.
\   HBW blocks are abandoned until HBW-RESET.
\   Core-0 only — calls (AR-FREE-BACKING) which uses shared state.
: ARENA-DESTROY  ( arena -- )
    ?CORE0
    DUP A.BASE @  OVER A.SIZE @  ROT DUP >R A.SOURCE @
    (AR-FREE-BACKING)
    R>
    0 OVER !  0 OVER 8 + !                   \ zero base, size
    0 OVER 16 + !  0 SWAP 24 + ! ;           \ zero ptr, source

\ -- Snapshots: save/restore bump pointer for transactional scratch --

\ ARENA-SNAP ( arena -- snap )
\   Save the current bump pointer as a snapshot token.
: ARENA-SNAP  ( arena -- snap )
    A.PTR @ ;

\ ARENA-ROLLBACK ( arena snap -- )
\   Restore the bump pointer to a previous snapshot.
\   Everything allocated after the snapshot is logically freed.
\   Validates that snap falls within [base, base+size].
: ARENA-ROLLBACK  ( arena snap -- )
    OVER A.BASE @                        ( arena snap base )
    OVER SWAP                            ( arena snap snap base )
    < ABORT" rollback: snap below base"  ( arena snap )
    OVER DUP A.BASE @ SWAP A.SIZE @ +   ( arena snap limit )
    OVER SWAP                            ( arena snap snap limit )
    > ABORT" rollback: snap above limit" ( arena snap )
    SWAP A.PTR ! ;

\ ARENA-SNAP-DROP ( snap -- )
\   Discard a snapshot token.  No-op (for API symmetry).
: ARENA-SNAP-DROP  ( snap -- )
    DROP ;

\ -- Scoped arena stack: implicit "current arena" for polymorphic code --

4 CONSTANT ARENA-STK-DEPTH       \ max nesting depth
CREATE ARENA-STK  ARENA-STK-DEPTH 8 * ALLOT
VARIABLE ARENA-SP   0 ARENA-SP !  \ stack pointer (0 = empty)

\ CURRENT-ARENA ( -- arena )  return the arena on top of stack
: CURRENT-ARENA  ( -- arena )
    ARENA-SP @ 0= ABORT" no current arena"
    ARENA-STK  ARENA-SP @ 1- 8 * + @ ;

\ ARENA-PUSH ( arena -- )  push arena onto the scoped stack
: ARENA-PUSH  ( arena -- )
    ARENA-SP @ ARENA-STK-DEPTH >= ABORT" arena stack full"
    ARENA-STK  ARENA-SP @ 8 * + !
    1 ARENA-SP +! ;

\ ARENA-POP ( -- )  pop the current arena from the scoped stack
: ARENA-POP  ( -- )
    ARENA-SP @ 0= ABORT" arena stack underflow"
    -1 ARENA-SP +! ;

\ AALLOT ( u -- addr )  allocate from current arena
: AALLOT  ( u -- addr )
    CURRENT-ARENA SWAP ARENA-ALLOT ;

\ .ARENA ( arena -- )  print arena status
: .ARENA  ( arena -- )
    ." Arena: base=" DUP A.BASE @ .
    ."  size=" DUP A.SIZE @ .
    ."  used=" DUP ARENA-USED .
    ."  free=" DUP ARENA-FREE .
    ."  src=" A.SOURCE @ DUP 0 = IF DROP ." heap" ELSE
    DUP 1 = IF DROP ." xmem" ELSE
    2 = IF ." hbw" ELSE ." ?" THEN THEN THEN CR ;

\ =====================================================================
\  §2  Buffer Subsystem
\ =====================================================================
\
\  BUFFER descriptor (4 cells = 32 bytes):
\    +0   type        0=raw  1=records  2=tiles  3=bitset
\    +8   elem_width  bytes per element (1, 2, 4, or 8)
\    +16  length      number of elements
\    +24  data_addr   pointer to tile-aligned data
\
\  Usage:  0 1 1024 BUFFER my-raw-buf
\          ( type=raw  width=1  length=1024 )

\ -- Useful general helper --  \
: IDLE  ( -- )  [ 0 C, ] ;  \ IDL opcode — yield CPU until next interrupt

\ -- Registry (linked list — no slot limit) --
VARIABLE BUF-COUNT
0 BUF-COUNT !
VARIABLE BUF-HEAD     0 BUF-HEAD !   \ head of buffer linked list

\ (BUF-REG) ( desc -- )  register a descriptor in the linked list.
\   Allocates a 16-byte link node in the dictionary:
\     link+0 = desc addr, link+8 = old head.
: (BUF-REG)  ( desc -- )
    HERE SWAP         ( link-addr desc )
    ,                 ( link-addr ; link+0 = desc )
    BUF-HEAD @ ,      ( link-addr ; link+8 = old head )
    BUF-HEAD !        ( ; head = link-addr )
    1 BUF-COUNT +! ;

\ BUF-NTH ( n -- desc )  Return descriptor of nth registered buffer
\   (0-based).  Walks the linked list from BUF-HEAD.
: BUF-NTH  ( n -- desc )
    BUF-HEAD @          ( n node )
    SWAP 0 ?DO          ( node )
        8 + @           ( next-node )
    LOOP
    @                   ( desc ) ;

\ -- Field accessors --
: B.TYPE   ( desc -- type )     @ ;
: B.WIDTH  ( desc -- width )    8 + @ ;
: B.LEN    ( desc -- len )      16 + @ ;
: B.DATA   ( desc -- addr )     24 + @ ;

\ -- Internal: temp for descriptor address --
VARIABLE BDESC

\ BUFFER ( type width length "name" -- )
\   Allocates a descriptor + tile-aligned data region.
\   Defines a CONSTANT whose value is the descriptor address.
: BUFFER
    HERE BDESC !              \ remember descriptor start
    ROT ,                     \ +0  store type  (3rd item)
    SWAP ,                    \ +8  store width (2nd item)
    DUP ,                     \ +16 store length (keep copy)
    BDESC @ B.WIDTH *         \ total data bytes = length * width
    0 ,                       \ +24 reserve cell for data_addr (prevent overlap)
    TALIGN                    \ align HERE for data start
    HERE BDESC @ 24 + !       \ +24 store data_addr = HERE
    ALLOT                     \ advance HERE past data region
    \ register
    BDESC @ (BUF-REG)
    BDESC @ CONSTANT ;

\ HBW-BUFFER ( type width length "name" -- )
\   Like BUFFER, but allocates the data region in HBW math RAM.
\   Descriptor stays in Bank 0 (dictionary); data in HBW for fast tile ops.
: HBW-BUFFER
    HERE BDESC !              \ descriptor in dictionary
    ROT ,                     \ +0  store type
    SWAP ,                    \ +8  store width
    DUP ,                     \ +16 store length
    BDESC @ B.WIDTH *         \ total data bytes
    0 ,                       \ +24 reserve cell for data_addr
    HBW-TALIGN                \ align HBW pointer
    HBW-HERE @ BDESC @ 24 + ! \ +24 data_addr = HBW-HERE
    HBW-ALLOT DROP            \ advance HBW-HERE past data region
    \ register
    BDESC @ (BUF-REG)
    BDESC @ CONSTANT ;

\ XBUFFER ( type width length "name" -- )
\   Like BUFFER, but allocates the data region in external memory.
\   Descriptor stays in Bank 0 (dictionary); data in ext mem.
\   Requires external memory (XMEM? must be true).
: XBUFFER
    HERE BDESC !              \ descriptor in dictionary
    ROT ,                     \ +0  store type
    SWAP ,                    \ +8  store width
    DUP ,                     \ +16 store length
    BDESC @ B.WIDTH *         \ total data bytes
    0 ,                       \ +24 reserve cell for data_addr
    XMEM-TALIGN               \ align ext mem pointer
    XMEM-HERE @ BDESC @ 24 + ! \ +24 data_addr = XMEM-HERE
    XMEM-ALLOT DROP           \ advance XMEM-HERE past data region
    \ register
    BDESC @ (BUF-REG)
    BDESC @ CONSTANT ;

\ -- Derived queries --
: B.BYTES  ( desc -- n )  DUP B.LEN SWAP B.WIDTH * ;
: B.TILES  ( desc -- n )  B.BYTES 63 + 64 / ;

\ -- Operations --
: B.FILL   ( byte desc -- )  DUP B.DATA SWAP B.BYTES ROT FILL ;
: B.ZERO   ( desc -- )       0 SWAP B.FILL ;

\ -- Info --
: B.INFO   ( desc -- )
    ."  [buf"
    DUP ."   t=" B.TYPE .
    DUP ."   w=" B.WIDTH .
    DUP ."   n=" B.LEN .
    DUP ."   tiles=" B.TILES .
    ."   @" B.DATA . ."  ]" CR ;

\ -- Preview first tile (64 bytes) as hex --
: B.PREVIEW ( desc -- )
    B.DATA
    4 0 DO
        16 0 DO
            DUP C@ .
            1+
        LOOP CR
    LOOP DROP ;

\ -- List all registered buffers --
: BUFFERS  ( -- )
    ."  --- Buffers (" BUF-COUNT @ . ."  ) ---" CR
    0 BUF-HEAD @
    BEGIN DUP WHILE
        SWAP DUP . ."  : " SWAP        ( idx link )
        DUP @ B.INFO                     \ link+0 = desc addr
        SWAP 1+ SWAP
        8 + @                            \ link+8 = next link
    REPEAT
    2DROP ;

\ =====================================================================
\  §2.1  Arena–Buffer Integration
\ =====================================================================
\
\  Extends the arena allocator with buffer support.
\  Must come after §2 so BUF-HEAD / BUF-COUNT are available.

VARIABLE AB-AR      \ scratch: arena address
VARIABLE AB-DESC    \ scratch: descriptor address

\ (AR-UNREG-BUFS) ( base limit -- )
\   Walk BUF-HEAD linked list.  Unlink every node whose descriptor
\   address falls in [base, limit).  O(n) in registered buffers.
: (AR-UNREG-BUFS)  ( base limit -- )
    BUF-HEAD      ( base limit pp )      \ pp = pointer-to-pointer
    BEGIN  DUP @ 0<>  WHILE              ( base limit pp )
        DUP @                            ( base limit pp node )
        DUP @                            ( base limit pp node desc )
        4 PICK OVER > 0=                 ( base limit pp node desc base<=desc )
        SWAP 4 PICK <                    ( base limit pp node base<=desc desc<lim )
        AND IF
            \ unlink: *pp = node.next
            8 + @  OVER !               ( base limit pp )
            -1 BUF-COUNT +!
        ELSE
            \ keep: advance pp = &(node.next)
            NIP 8 +                     ( base limit pp' )
        THEN
    REPEAT
    DROP 2DROP ;

\ Redefine ARENA-DESTROY to also unregister arena-scoped buffers.
\   Core-0 only — calls (AR-FREE-BACKING) which uses shared state.
: ARENA-DESTROY  ( arena -- )
    ?CORE0
    DUP A.BASE @  OVER A.SIZE @ OVER + ( arena base limit )
    (AR-UNREG-BUFS)                     ( arena )
    DUP A.BASE @  OVER A.SIZE @  ROT DUP >R A.SOURCE @
    (AR-FREE-BACKING)
    R>
    0 OVER !  0 OVER 8 + !             \ zero base, size
    0 OVER 16 + !  0 SWAP 24 + ! ;     \ zero ptr, source

\ ARENA-BUFFER ( type width length arena "name" -- )
\   Like BUFFER, but both descriptor and data are arena-allocated.
\   Registered in BUF-HEAD; auto-unregistered by ARENA-DESTROY.
: ARENA-BUFFER  ( type width length arena "name" -- )
    AB-AR !                              ( type width length )
    AB-AR @ 32 ARENA-ALLOT AB-DESC !     ( type width length )
    \ Store length at +16
    DUP AB-DESC @ 16 + !                ( type width length )
    \ data-bytes = length * width
    OVER *                               ( type width data-bytes )
    \ Store width at +8
    SWAP AB-DESC @ 8 + !                 ( type data-bytes )
    \ Store type at +0
    SWAP AB-DESC @ !                     ( data-bytes )
    \ Allocate data from arena (8-byte aligned)
    7 + -8 AND
    AB-AR @ SWAP ARENA-ALLOT             ( data-addr )
    AB-DESC @ 24 + !                     ( )
    \ Register in buffer list and define constant
    AB-DESC @ (BUF-REG)
    AB-DESC @ CONSTANT ;

\ =====================================================================
\  §3  Tile-Aware Buffer Operations
\ =====================================================================
\
\  These words use the MEX tile engine to perform SIMD operations
\  on buffer data.  They set up TSRC0/TSRC1/TDST and iterate over
\  tiles, reading results via ACC@.
\
\  TMODE is set to 0 (8-bit unsigned, 64 lanes) for byte buffers.
\  For wider elements, callers should set TMODE before calling.

\ B.SUM ( desc -- n ) sum all bytes in buffer via tile engine
: B.SUM  ( desc -- n )
    0 TMODE!                  \ 8-bit unsigned
    2 TCTRL!                  \ ACC_ZERO: clear ACC before first op
    DUP B.DATA                ( desc addr )
    SWAP B.TILES              ( addr ntiles )
    0 DO                      ( addr )
        DUP TSRC0!            \ point tile engine at this tile
        TSUM                  \ reduce → ACC (accumulates after first)
        1 TCTRL!              \ ACC_ACC: accumulate subsequent tiles
        64 +                  \ advance to next tile
    LOOP
    DROP
    ACC@ ;                    \ read accumulated result

\ B.MIN ( desc -- n ) minimum byte in buffer via tile engine
: B.MIN  ( desc -- n )
    0 TMODE!
    DUP B.DATA                ( desc addr )
    SWAP B.TILES              ( addr ntiles )
    DUP 0= IF 2DROP 0 ELSE
        0 DO                  ( addr )
            DUP TSRC0!
            2 TCTRL!          \ zero ACC each time (we want per-tile min)
            TMIN
            I 0= IF
                ACC@          \ first tile: start with this min
            ELSE
                ACC@          \ subsequent tiles: take minimum
                MIN
            THEN
            SWAP 64 + SWAP    \ advance address
        LOOP
        NIP                   \ drop address, keep min
    THEN ;

\ B.MAX ( desc -- n ) maximum byte in buffer via tile engine
: B.MAX  ( desc -- n )
    0 TMODE!
    DUP B.DATA                ( desc addr )
    SWAP B.TILES              ( addr ntiles )
    DUP 0= IF 2DROP 0 ELSE
        0 DO                  ( addr )
            DUP TSRC0!
            2 TCTRL!
            TMAX
            I 0= IF
                ACC@
            ELSE
                ACC@
                MAX
            THEN
            SWAP 64 + SWAP
        LOOP
        NIP
    THEN ;

\ B.ADD ( src1 src2 dst -- ) element-wise add two buffers → dst
\   All three buffers must have the same tile count.
VARIABLE BTMP-NTILES
: B.ADD  ( src1 src2 dst -- )
    0 TMODE!
    ROT                       ( src2 dst src1 )
    DUP B.TILES BTMP-NTILES ! ( src2 dst src1 )
    B.DATA                    ( src2 dst addr1 )
    ROT B.DATA                ( dst addr1 addr2 )
    ROT B.DATA                ( addr1 addr2 addrd )
    BTMP-NTILES @             ( addr1 addr2 addrd ntiles )
    0 DO
        2 PICK TSRC0!         \ src1 tile
        OVER   TSRC1!         \ src2 tile
        DUP    TDST!          \ dst tile
        TADD
        ROT 64 + -ROT         \ advance src1
        SWAP 64 + SWAP        \ advance src2
        64 +                  \ advance dst
    LOOP
    DROP 2DROP ;

\ B.SUB ( src1 src2 dst -- ) element-wise subtract: dst = src1 - src2
: B.SUB  ( src1 src2 dst -- )
    0 TMODE!
    ROT
    DUP B.TILES BTMP-NTILES !
    B.DATA ROT B.DATA ROT B.DATA
    BTMP-NTILES @
    0 DO
        2 PICK TSRC0!
        OVER   TSRC1!
        DUP    TDST!
        TSUB
        ROT 64 + -ROT
        SWAP 64 + SWAP
        64 +
    LOOP
    DROP 2DROP ;

\ B.SCALE ( n desc -- ) multiply each byte by n (in-place via broadcast)
\   This uses element-wise MUL with broadcast from register.
\   We store n at a known scratch tile, use TSRC1 as broadcast source.
\   Actually, we'll use the simpler approach: iterate and C! manually.
: B.SCALE  ( n desc -- )
    DUP B.DATA SWAP B.BYTES   ( n addr nbytes )
    ROT                       ( addr nbytes n )
    -ROT                      ( n addr nbytes )
    0 DO                      ( n addr )
        OVER OVER C@ *        ( n addr product )
        255 AND               ( n addr product&0xFF )
        OVER C!               ( n addr )
        1+
    LOOP
    2DROP ;

\ =====================================================================
\  §3.1  FP16 / BF16 Buffer Operations
\ =====================================================================
\
\  Half-precision (FP16) and bfloat16 (BF16) operations for ML and
\  signal processing.  Data is packed 32 elements per 64-byte tile.
\  Reductions (SUM, DOT, SUMSQ) use FP32 accumulation to avoid
\  catastrophic precision loss.
\
\  Users store raw half-float bit patterns in buffers.  The tile
\  engine interprets them according to TMODE.  To create an FP16
\  buffer with 32 elements: `0 1 64 BUFFER myfp16`
\  (64 bytes = 32 × 2-byte FP16 values = 1 tile)

\ -- F.SUM ( desc -- n )  FP16 sum reduction with FP32 accumulation --
: F.SUM  ( desc -- n )
    FP16-MODE
    2 TCTRL!                  \ ACC_ZERO
    DUP B.DATA
    SWAP B.TILES
    0 DO
        DUP TSRC0!  TSUM
        1 TCTRL!              \ ACC_ACC
        64 +
    LOOP
    DROP ACC@
    0 TMODE! ;                \ restore default

\ -- F.DOT ( src1 src2 -- n )  FP16 dot product with FP32 accum --
: F.DOT  ( src1 src2 -- n )
    FP16-MODE
    2 TCTRL!                  \ ACC_ZERO
    SWAP DUP B.DATA SWAP B.TILES
    ROT B.DATA                ( a1 ntiles a2 )
    SWAP                      ( a1 a2 ntiles )
    0 DO
        OVER TSRC0!
        DUP  TSRC1!
        TDOT
        1 TCTRL!              \ ACC_ACC
        SWAP 64 + SWAP 64 +
    LOOP
    2DROP ACC@
    0 TMODE! ;

\ -- F.SUMSQ ( desc -- n )  FP16 sum of squares, FP32 accum --
: F.SUMSQ  ( desc -- n )
    FP16-MODE
    2 TCTRL!
    DUP B.DATA
    SWAP B.TILES
    0 DO
        DUP TSRC0!  TSUMSQ
        1 TCTRL!
        64 +
    LOOP
    DROP ACC@
    0 TMODE! ;

\ -- F.ADD ( src1 src2 dst -- )  FP16 element-wise add --
: F.ADD  ( src1 src2 dst -- )
    FP16-MODE
    ROT DUP B.TILES BTMP-NTILES !
    B.DATA ROT B.DATA ROT B.DATA
    BTMP-NTILES @
    0 DO
        2 PICK TSRC0!  OVER TSRC1!  DUP TDST!  TADD
        ROT 64 + -ROT  SWAP 64 + SWAP  64 +
    LOOP
    DROP 2DROP
    0 TMODE! ;

\ -- F.MUL ( src1 src2 dst -- )  FP16 element-wise multiply --
: F.MUL  ( src1 src2 dst -- )
    FP16-MODE
    ROT DUP B.TILES BTMP-NTILES !
    B.DATA ROT B.DATA ROT B.DATA
    BTMP-NTILES @
    0 DO
        2 PICK TSRC0!  OVER TSRC1!  DUP TDST!  TMUL
        ROT 64 + -ROT  SWAP 64 + SWAP  64 +
    LOOP
    DROP 2DROP
    0 TMODE! ;

\ -- BF.SUM ( desc -- n )  BF16 sum with FP32 accum --
: BF.SUM  ( desc -- n )
    BF16-MODE
    2 TCTRL!
    DUP B.DATA SWAP B.TILES
    0 DO  DUP TSRC0!  TSUM  1 TCTRL!  64 +  LOOP
    DROP ACC@
    0 TMODE! ;

\ -- BF.DOT ( src1 src2 -- n )  BF16 dot product, FP32 accum --
: BF.DOT  ( src1 src2 -- n )
    BF16-MODE
    2 TCTRL!
    SWAP DUP B.DATA SWAP B.TILES
    ROT B.DATA SWAP
    0 DO
        OVER TSRC0!  DUP TSRC1!  TDOT
        1 TCTRL!  SWAP 64 + SWAP 64 +
    LOOP
    2DROP ACC@
    0 TMODE! ;

\ =====================================================================
\  §4  Kernel Registry
\ =====================================================================
\
\  KERNEL descriptor (4 cells = 32 bytes):
\    +0   n_inputs    number of input buffers
\    +8   n_outputs   number of output buffers
\    +16  footprint   estimated tile working set
\    +24  flags       0 = normal, 1 = tile-accelerated
\
\  Kernels are separately-defined Forth colon words.
\  The descriptor holds metadata for the dashboard.
\  To run a kernel, call the Forth word directly.
\
\  Usage:
\    : my-op ( buf -- )  B.ZERO ;
\    1 1 2 0 KERNEL my-op-desc
\    ( 1 input, 1 output, 2-tile footprint, flags=0 )

\ -- Registry (up to 32 kernels) --
VARIABLE KERN-COUNT
0 KERN-COUNT !
VARIABLE KERN-TABLE  31 CELLS ALLOT

\ -- Field accessors --
: K.IN     ( desc -- n_in )     @ ;
: K.OUT    ( desc -- n_out )    8 + @ ;
: K.FOOT   ( desc -- foot )    16 + @ ;
: K.FLAGS  ( desc -- flags )   24 + @ ;

\ -- Internal temp --
VARIABLE KDESC

\ KERNEL ( n_in n_out footprint flags "name" -- )
\   Allocates a kernel descriptor and defines a CONSTANT for it.
: KERNEL
    HERE KDESC !
    32 ALLOT                  \ reserve 4 cells
    KDESC @ 24 + !            \ +24  flags     (TOS)
    KDESC @ 16 + !            \ +16  footprint
    KDESC @ 8  + !            \ +8   n_outputs
    KDESC @      !            \ +0   n_inputs
    \ register
    KERN-COUNT @ 32 < IF
        KDESC @  KERN-COUNT @ CELLS KERN-TABLE + !
        KERN-COUNT @ 1+ KERN-COUNT !
    THEN
    KDESC @ CONSTANT ;

\ -- Info --
: K.INFO   ( desc -- )
    ."  [kern"
    DUP ."   in=" K.IN .
    DUP ."   out=" K.OUT .
    DUP ."   foot=" K.FOOT .
    ."   fl=" K.FLAGS . ."  ]" CR ;

\ -- List all registered kernels --
: KERNELS  ( -- )
    ."  --- Kernels (" KERN-COUNT @ . ."  ) ---" CR
    KERN-COUNT @ DUP IF
        0 DO
            I . ."  : "
            I CELLS KERN-TABLE + @ K.INFO
        LOOP
    ELSE DROP THEN ;

\ =====================================================================
\  §5  Sample Kernels
\ =====================================================================

\ --- kzero: zero an entire buffer ---
: kzero  ( buf-desc -- )  B.ZERO ;
1 1 0 0 KERNEL kzero-desc

\ --- kfill: fill a buffer with a byte value ---
: kfill  ( byte buf-desc -- )  B.FILL ;
1 1 0 0 KERNEL kfill-desc

\ --- kadd: element-wise add two buffers into a third ---
: kadd  ( src1 src2 dst -- )  B.ADD ;
2 1 3 1 KERNEL kadd-desc

\ --- ksum: sum all bytes in a buffer, leave on stack ---
: ksum  ( buf-desc -- n )  B.SUM ;
1 0 1 1 KERNEL ksum-desc

\ --- kstats: compute sum, min, max of a buffer ---
: kstats  ( buf-desc -- sum min max )
    DUP DUP                   ( desc desc desc )
    B.SUM                     ( desc desc sum )
    ROT B.MIN                 ( desc sum min )
    ROT B.MAX                 ( sum min max )
;
1 0 3 1 KERNEL kstats-desc

\ --- kscale: multiply each byte by n (in-place) ---
: kscale  ( n buf-desc -- )  B.SCALE ;
1 1 1 0 KERNEL kscale-desc

\ --- kthresh: threshold — set bytes < n to 0, >= n to 255 ---
: kthresh  ( n buf-desc -- )
    DUP B.DATA SWAP B.BYTES  ( n addr nbytes )
    ROT                      ( addr nbytes thresh )
    -ROT                     ( thresh addr nbytes )
    0 DO                     ( thresh addr )
        DUP C@               ( thresh addr val )
        2 PICK < IF          ( thresh addr )  \ val < thresh
            0 OVER C!
        ELSE
            255 OVER C!
        THEN
        1+
    LOOP
    2DROP ;
1 1 1 0 KERNEL kthresh-desc

\ --- kclamp: clamp bytes to [lo, hi] range (in-place) ---
: kclamp  ( lo hi buf-desc -- )
    DUP B.DATA SWAP B.BYTES  ( lo hi addr nbytes )
    0 DO                     ( lo hi addr )
        DUP C@               ( lo hi addr val )
        3 PICK MAX           ( lo hi addr clamped-lo )
        2 PICK MIN           ( lo hi addr clamped )
        OVER C! 1+
    LOOP
    DROP 2DROP ;
1 1 1 0 KERNEL kclamp-desc

\ --- kavg: moving average with window size w (in-place) ---
\   Simplified single-pass: averages each byte with its w-1 successors.
0 1 256 BUFFER mavg-scratch
VARIABLE MAVG-SUM
VARIABLE MAVG-WIN
VARIABLE MAVG-NBYTES
: kavg  ( window buf-desc -- )
    SWAP MAVG-WIN !
    DUP B.BYTES MAVG-NBYTES !
    DUP B.DATA mavg-scratch B.DATA
    MAVG-NBYTES @ CMOVE
    DUP B.DATA SWAP B.BYTES
    0 DO
        mavg-scratch B.DATA I + C@
        OVER C! 1+
    LOOP
    DROP ;
1 1 5 0 KERNEL kavg-desc

\ --- khistogram: build 256-bin histogram of byte values ---
\   Result goes into a histogram buffer (256 elements, 8 bytes wide).
\   Each bin counts occurrences of that byte value.
0 8 256 BUFFER hist-bins
: khistogram  ( src-desc -- )
    \ Zero the histogram
    hist-bins B.DATA 2048 0 FILL    ( src )
    DUP B.DATA SWAP B.BYTES         ( addr nbytes )
    0 DO                            ( addr )
        DUP C@                      ( addr val )
        CELLS hist-bins B.DATA +    ( addr bin-addr )
        1 SWAP +!                   ( addr )
        1+
    LOOP DROP ;
1 1 4 0 KERNEL khistogram-desc

\ -- histogram query: fetch count for byte value v --
: HIST@  ( v -- count )
    CELLS hist-bins B.DATA + @ ;

\ -- histogram display: show non-zero bins --
: .HIST  ( -- )
    ."  --- Histogram ---" CR
    256 0 DO
        I HIST@ DUP IF
            ."   [" I . ."  ]=" . CR
        ELSE DROP THEN
    LOOP ;

\ --- kdelta: delta encode (out[i] = in[i] - in[i-1], first = 0) ---
\   Writes result to dst buffer (same size as src).
VARIABLE DELTA-PREV
: kdelta  ( src dst -- )
    OVER B.BYTES              ( src dst nbytes )
    ROT B.DATA                ( dst nbytes src_data )
    ROT B.DATA                ( nbytes src_data dst_data )
    ROT                       ( src_data dst_data nbytes )
    0 DELTA-PREV !
    0 DO                      ( src dst )
        OVER C@               ( src dst val )
        DUP DELTA-PREV @      ( src dst val val prev )
        - 255 AND             ( src dst val delta )
        2 PICK C!             ( src dst val )
        DELTA-PREV !          ( src dst )
        1+ SWAP 1+ SWAP      ( src+1 dst+1 )
    LOOP
    2DROP ;
1 1 2 0 KERNEL kdelta-desc

\ --- knorm: normalize buffer to use full 0-255 range ---
\   Uses tile engine for fast min/max, then byte-loop to rescale.
VARIABLE NORM-MIN
VARIABLE NORM-MAX
VARIABLE NORM-RANGE
: knorm  ( buf-desc -- )
    DUP B.MIN NORM-MIN !
    DUP B.MAX NORM-MAX !
    NORM-MAX @ NORM-MIN @ - DUP NORM-RANGE !
    0= IF DROP EXIT THEN     \ flat signal, nothing to do
    DUP B.DATA SWAP B.BYTES  ( addr nbytes )
    0 DO                      ( addr )
        DUP C@                ( addr val )
        NORM-MIN @ -          ( addr val-min )
        255 * NORM-RANGE @ /  ( addr scaled )
        255 AND OVER C! 1+
    LOOP DROP ;
1 1 2 1 KERNEL knorm-desc

\ --- kpeak: peak detector — marks local maxima above threshold ---
\   Output: 255 at peak positions, 0 elsewhere.
\   A peak: val > left neighbor AND val > right neighbor AND val >= thresh
: kpeak  ( thresh src dst -- )
    ROT -ROT                  ( thresh src dst )
    OVER B.BYTES              ( thresh src dst nbytes )
    2 PICK B.DATA             ( thresh src dst nbytes src_data )
    2 PICK B.DATA             ( thresh src dst nbytes src_data dst_data )
    \ Zero dst first
    3 PICK B.ZERO
    ROT                       ( thresh src dst src_data dst_data nbytes )
    DUP 3 < IF 2DROP 2DROP 2DROP DROP EXIT THEN
    1- 1 DO                   ( thresh src dst src_data dst_data )
        \ Get src_data[i-1], src_data[i], src_data[i+1]
        OVER I 1- + C@       ( ... left )
        2 PICK I + C@        ( ... left center )
        3 PICK I 1+ + C@     ( ... left center right )
        ROT                  ( ... center right left )
        2 PICK SWAP > IF     ( ... center right )  \ center > left
            OVER SWAP > IF   ( ... center )   \ center > right
                5 PICK >= IF ( ... )          \ center >= thresh
                    255 OVER I + C!           \ mark peak
                THEN
            ELSE DROP THEN
        ELSE 2DROP THEN
    LOOP
    2DROP 2DROP DROP ;
1 1 2 0 KERNEL kpeak-desc

\ --- krms: root-mean-square (integer approximation) ---
\   Uses tile engine TMUL for squaring, TSUM for accumulation.
\   Returns integer sqrt of (sum of squares / count).
VARIABLE RMS-SSQ
: krms  ( buf-desc -- rms )
    0 RMS-SSQ !
    DUP B.DATA SWAP B.BYTES  ( addr nbytes )
    0 DO                      ( addr )
        DUP C@                ( addr val )
        DUP *                 ( addr val^2 )
        RMS-SSQ +!
        1+
    LOOP DROP
    RMS-SSQ @                 ( sum-of-squares )
    DUP B.BYTES /             ( mean-sq )  \ Note: B.BYTES already consumed
    \ Integer square root (Newton's method, 8 iterations)
    DUP IF
        DUP 2 / SWAP         ( guess n )
        8 0 DO
            OVER /            ( guess n/guess )
            OVER +            ( guess guess+n/guess )
            2 /               ( guess new_guess )
            NIP DUP           ( new_guess new_guess )
        LOOP
        DROP                  ( result )
    THEN ;

\ Wrapped version: takes buf-desc, returns rms on stack
: krms-buf  ( buf-desc -- rms )
    DUP >R
    0 RMS-SSQ !
    DUP B.DATA SWAP B.BYTES  ( addr nbytes )
    0 DO
        DUP C@ DUP * RMS-SSQ +!
        1+
    LOOP DROP
    RMS-SSQ @ R> B.BYTES /   ( mean-of-sq )
    \ isqrt via Newton's method
    DUP 0= IF EXIT THEN
    DUP 2 /                  ( mean guess )
    8 0 DO
        OVER OVER / + 2 /
    LOOP
    NIP ;
1 0 1 0 KERNEL krms-desc

\ --- kcorrelate: dot product of two buffers (integer) ---
\   Uses tile engine: TSRC0!, TSRC1!, TDOT on each tile pair,
\   accumulating into ACC.
: kcorrelate  ( a b -- dot )
    0 TMODE!
    SWAP DUP B.DATA SWAP B.TILES ( b a_data a_tiles )
    ROT B.DATA SWAP              ( a_data b_data tiles )
    2 TCTRL!                     ( a_data b_data tiles ) \ ACC=0
    0 DO
        OVER TSRC0!              ( a_data b_data )
        DUP TSRC1!
        TDOT                     ( a_data b_data )
        1 TCTRL!                 \ accumulate
        SWAP 64 + SWAP 64 +
    LOOP
    2DROP
    ACC@ ;
2 0 2 1 KERNEL kcorrelate-desc

\ --- kconvolve3: 3-tap FIR convolution [c0, c1, c2] (in-place) ---
\   out[i] = (c0*in[i-1] + c1*in[i] + c2*in[i+1]) / 256
\   Edges: replicate boundary values.
0 1 256 BUFFER conv-scratch
VARIABLE CONV-C0
VARIABLE CONV-C1
VARIABLE CONV-C2
VARIABLE CONV-DESC
: kconvolve3  ( c0 c1 c2 buf-desc -- )
    CONV-DESC ! CONV-C2 ! CONV-C1 ! CONV-C0 !
    \ Copy buffer -> scratch
    CONV-DESC @ B.DATA conv-scratch B.DATA CONV-DESC @ B.BYTES CMOVE
    \ Convolve from scratch -> buffer
    CONV-DESC @ B.DATA CONV-DESC @ B.BYTES  ( dst nbytes )
    0 DO                      ( dst )
        \ Get left, center, right from scratch
        I 0= IF
            conv-scratch B.DATA C@    \ replicate left edge
        ELSE
            conv-scratch B.DATA I 1- + C@
        THEN
        conv-scratch B.DATA I + C@
        I 1+ CONV-DESC @ B.BYTES < IF
            conv-scratch B.DATA I 1+ + C@
        ELSE
            conv-scratch B.DATA I + C@  \ replicate right edge
        THEN
        \ Compute weighted sum
        CONV-C2 @ *
        SWAP CONV-C1 @ * +
        SWAP CONV-C0 @ * +
        256 /                 ( dst result )
        255 AND OVER C! 1+
    LOOP
    DROP ;
1 1 5 0 KERNEL kconvolve3-desc

\ --- kinvert: bitwise invert all bytes (255 - val) ---
: kinvert  ( buf-desc -- )
    DUP B.DATA SWAP B.BYTES
    0 DO
        DUP C@ 255 SWAP - OVER C! 1+
    LOOP DROP ;
1 1 1 0 KERNEL kinvert-desc

\ --- kcount: count bytes matching a value ---
VARIABLE KCOUNT-N
: kcount  ( val buf-desc -- count )
    0 KCOUNT-N !
    DUP B.DATA SWAP B.BYTES  ( val addr nbytes )
    0 DO                      ( val addr )
        DUP C@ 2 PICK = IF
            1 KCOUNT-N +!
        THEN
        1+
    LOOP 2DROP
    KCOUNT-N @ ;
1 0 1 0 KERNEL kcount-desc

\ --- FP16 kernels ---

\ kfsum: FP16 sum reduction → stack
: kfsum  ( buf -- n )  F.SUM ;
1 0 1 1 KERNEL kfsum-desc

\ kfdot: FP16 dot product of two buffers → stack
: kfdot  ( a b -- n )  F.DOT ;
2 0 1 1 KERNEL kfdot-desc

\ kfsumsq: FP16 sum of squares → stack
: kfsumsq  ( buf -- n )  F.SUMSQ ;
1 0 1 1 KERNEL kfsumsq-desc

\ kfadd: FP16 element-wise add → dst
: kfadd  ( src1 src2 dst -- )  F.ADD ;
2 1 3 1 KERNEL kfadd-desc

\ kfmul: FP16 element-wise multiply → dst
: kfmul  ( src1 src2 dst -- )  F.MUL ;
2 1 3 1 KERNEL kfmul-desc

\ =====================================================================
\  §6  Pipeline Engine
\ =====================================================================
\
\  A PIPELINE is an ordered sequence of no-argument execution tokens.
\  Each step is a Forth word that operates on pre-bound buffers.
\
\  PIPELINE descriptor (2 cells header + step array):
\    +0   capacity    max steps
\    +8   count       current steps in use
\    +16  steps[]     array of XTs (capacity cells)
\
\  Usage:
\    3 PIPELINE my-pipe
\    : step1 ( -- ) 42 mybuf B.FILL ;
\    ' step1 my-pipe P.ADD
\    my-pipe P.RUN

\ -- Registry (up to 8 pipelines) --
VARIABLE PIPE-COUNT
0 PIPE-COUNT !
VARIABLE PIPE-TABLE  7 CELLS ALLOT

\ -- Field accessors --
: P.CAP   ( pipe -- n )     @ ;
: P.COUNT ( pipe -- n )     8 + @ ;
: P.DATA  ( pipe -- addr )  16 + ;

\ -- Internal temp --
VARIABLE PDESC
VARIABLE P-XT
VARIABLE P-PIPE

\ PIPELINE ( capacity "name" -- )
\   Allocates a pipeline descriptor and defines a CONSTANT.
: PIPELINE
    HERE PDESC !
    DUP ,                     \ +0  capacity
    0 ,                       \ +8  count = 0
    CELLS ALLOT               \ +16 step array (capacity cells)
    \ register
    PIPE-COUNT @ 8 < IF
        PDESC @ PIPE-COUNT @ CELLS PIPE-TABLE + !
        PIPE-COUNT @ 1+ PIPE-COUNT !
    THEN
    PDESC @ CONSTANT ;

\ P.GET ( pipe n -- xt ) get step n XT
: P.GET  ( pipe n -- xt )
    CELLS SWAP P.DATA + @ ;

\ P.SET ( xt pipe n -- ) set step n XT
: P.SET  ( xt pipe n -- )
    CELLS SWAP P.DATA + ! ;

\ P.ADD ( xt pipe -- ) append step, auto-increment count
: P.ADD  ( xt pipe -- )
    P-PIPE ! P-XT !
    P-PIPE @ P.COUNT  P-PIPE @ P.CAP < IF
        P-XT @  P-PIPE @  P-PIPE @ P.COUNT  P.SET
        P-PIPE @ P.COUNT 1+  P-PIPE @ 8 + !
    THEN ;

\ P.CLEAR ( pipe -- ) reset to 0 steps
: P.CLEAR  ( pipe -- )  8 + OFF ;

\ P.RUN ( pipe -- ) execute all steps in order
: P.RUN  ( pipe -- )
    DUP P.COUNT DUP IF
        0 DO
            DUP I P.GET EXECUTE
        LOOP
    ELSE DROP THEN
    DROP ;

\ -- Benchmarking primitives (moved before P.BENCH) --
VARIABLE BENCH-T0

: BENCH  ( xt -- cycles )
    CYCLES BENCH-T0 !
    EXECUTE
    CYCLES BENCH-T0 @ - ;

: .BENCH  ( xt -- )
    BENCH
    ."  cycles=" . CR ;

\ P.BENCH ( pipe -- ) execute and time each step
: P.BENCH  ( pipe -- )
    ."  Pipeline (" DUP P.COUNT . ."  steps):" CR
    DUP P.COUNT DUP IF
        0 DO
            DUP I P.GET BENCH
            ."    step " I . ."  = " . ."  cycles" CR
        LOOP
    ELSE DROP THEN
    DROP ;

\ P.INFO ( pipe -- ) show pipeline descriptor
: P.INFO  ( pipe -- )
    ."  [pipe cap=" DUP P.CAP .
    ."  steps=" P.COUNT . ."  ]" CR ;

\ -- List all registered pipelines --
: PIPES  ( -- )
    ."  --- Pipelines (" PIPE-COUNT @ . ."  ) ---" CR
    PIPE-COUNT @ DUP IF
        0 DO
            I . ."  : "
            I CELLS PIPE-TABLE + @ P.INFO
        LOOP
    ELSE DROP THEN ;

\ -- Demo buffers for sample pipelines --
0 1 64 BUFFER demo-a
0 1 64 BUFFER demo-b
0 1 64 BUFFER demo-c

\ --- Pipeline 1: fill-sum ---
\   Fill demo-a with 42, then sum and print.
: p1-fill ( -- ) 42 demo-a B.FILL ;
: p1-sum  ( -- ) demo-a B.SUM ."  sum=" . CR ;
2 PIPELINE pipe-fill-sum
' p1-fill pipe-fill-sum P.ADD
' p1-sum  pipe-fill-sum P.ADD

\ --- Pipeline 2: add-stats ---
\   Fill a with 10, b with 20, add a+b->c, print stats.
: p2-init  ( -- ) 10 demo-a B.FILL  20 demo-b B.FILL ;
: p2-add   ( -- ) demo-a demo-b demo-c kadd ;
: p2-stats ( -- ) demo-c kstats ."  max=" . ."   min=" . ."   sum=" . CR ;
3 PIPELINE pipe-add-stats
' p2-init  pipe-add-stats P.ADD
' p2-add   pipe-add-stats P.ADD
' p2-stats pipe-add-stats P.ADD

\ --- Pipeline 3: threshold ---
\   Fill demo-a with ramp 0..63, threshold at 32, print stats.
: p3-fill ( -- )
    demo-a B.DATA 64 0 DO I OVER I + C! LOOP DROP ;
: p3-thresh ( -- ) 32 demo-a kthresh ;
: p3-stats  ( -- ) demo-a kstats ."  max=" . ."   min=" . ."   sum=" . CR ;
3 PIPELINE pipe-thresh
' p3-fill   pipe-thresh P.ADD
' p3-thresh pipe-thresh P.ADD
' p3-stats  pipe-thresh P.ADD

\ =====================================================================
\  §7  Storage & Persistence
\ =====================================================================
\
\  Production I/O uses the BIOS checked block words.  The raw setup/command
\  words remain BIOS diagnostics and are not used by KDOS filesystem paths.
\  Storage device: 512-byte sectors, DMA to/from RAM.
\
\  B.SAVE / B.LOAD persist buffers by writing their data region to disk.
\  Buffer data is tile-aligned (64 bytes), sectors are 512 bytes.
\  A buffer that is N tiles writes ceil(N*64/512) sectors.

512 CONSTANT SECTOR

\ DISK? ( -- flag ) true if storage device present
: DISK?  ( -- flag )  DISK@ 128 AND 0<> ;

\ ---------------------------------------------------------------------
\  Explicit block-device and bounded-volume objects
\ ---------------------------------------------------------------------
\
\  These descriptors are the storage ABI consumed by filesystems.  The ABI
\  marker is permanent identity/capability validation, not a second legacy
\  implementation.  Controller generation-guarded commands make the saved
\  MEDIA_GEN an atomic acceptance condition: a stale descriptor cannot touch
\  replacement media in the race between a software check and submission.

0x31305645444B4C42 CONSTANT BLOCK-DEVICE-MAGIC   \ "BLKDEV01"
0x3130454D554C4F56 CONSTANT VOLUME-MAGIC         \ "VOLUME01"
1 CONSTANT STORAGE-ABI
128 CONSTANT /BLOCK-DEVICE
144 CONSTANT /VOLUME

\ Structured storage ior (low 32 bits):
\   7..0 raw controller cause, 15..8 stable code, 23..16 domain,
\   31..24 flags.  Zero alone is success.
1 CONSTANT IOR-D-BLOCK
2 CONSTANT IOR-D-DEVICE
3 CONSTANT IOR-D-VOLUME
4 CONSTANT IOR-D-PARTITION

1  CONSTANT IOR-F-PARTIAL
2  CONSTANT IOR-F-RETRYABLE
4  CONSTANT IOR-F-STALE
8  CONSTANT IOR-F-CORRUPT
16 CONSTANT IOR-F-UNSUPPORTED
32 CONSTANT IOR-F-READONLY

16 CONSTANT IOR-C-BAD-DESCRIPTOR
17 CONSTANT IOR-C-STALE
18 CONSTANT IOR-C-RANGE
19 CONSTANT IOR-C-READONLY
20 CONSTANT IOR-C-CORRUPT
21 CONSTANT IOR-C-CAPACITY
22 CONSTANT IOR-C-WORKSPACE
23 CONSTANT IOR-C-UNSUPPORTED
24 CONSTANT IOR-C-BUSY

: IOR-MAKE  ( raw code domain flags -- ior )
    24 LSHIFT  SWAP 16 LSHIFT OR  SWAP 8 LSHIFT OR  OR ;
: IOR>RAW     ( ior -- u )  0xFF AND ;
: IOR>CODE    ( ior -- u )  8 RSHIFT 0xFF AND ;
: IOR>DOMAIN  ( ior -- u )  16 RSHIFT 0xFF AND ;
: IOR>FLAGS   ( ior -- u )  24 RSHIFT 0xFF AND ;
: IOR-PARTIAL?  ( ior -- flag )  IOR>FLAGS IOR-F-PARTIAL AND 0<> ;
: IOR-STALE?    ( ior -- flag )  IOR>FLAGS IOR-F-STALE AND 0<> ;

14 IOR-C-BAD-DESCRIPTOR IOR-D-DEVICE 0
    IOR-MAKE CONSTANT BD-E-BAD-DESCRIPTOR
1  1 IOR-D-DEVICE 0 IOR-MAKE CONSTANT BD-E-NO-MEDIA
2  IOR-C-UNSUPPORTED IOR-D-DEVICE IOR-F-UNSUPPORTED
    IOR-MAKE CONSTANT BD-E-UNSUPPORTED
11 IOR-C-STALE IOR-D-DEVICE IOR-F-STALE
    IOR-MAKE CONSTANT BD-E-STALE
14 14 IOR-D-BLOCK 0 IOR-MAKE CONSTANT BD-E-INTERNAL
4  IOR-C-RANGE IOR-D-BLOCK 0 IOR-MAKE CONSTANT BD-E-RANGE
8  IOR-C-READONLY IOR-D-BLOCK IOR-F-READONLY
    IOR-MAKE CONSTANT BD-E-READONLY
14 IOR-C-BUSY IOR-D-DEVICE 0 IOR-MAKE CONSTANT BD-E-BUSY

14 IOR-C-BAD-DESCRIPTOR IOR-D-VOLUME 0
    IOR-MAKE CONSTANT VOL-E-BAD-DESCRIPTOR
11 IOR-C-STALE IOR-D-VOLUME IOR-F-STALE
    IOR-MAKE CONSTANT VOL-E-STALE
4  IOR-C-RANGE IOR-D-VOLUME 0 IOR-MAKE CONSTANT VOL-E-RANGE
8  IOR-C-READONLY IOR-D-VOLUME IOR-F-READONLY
    IOR-MAKE CONSTANT VOL-E-READONLY

\ Translate a checked BIOS result.  The controller partial bit becomes an
\ explicit flag while the low byte retains the unmodified cause.
: IOR-FROM-BLOCK-RESULT  ( status -- ior )
    DUP 0= IF EXIT THEN
    DUP 128 AND IF IOR-F-PARTIAL ELSE 0 THEN
    SWAP 127 AND                         ( flags raw )
    DUP 7 = OVER 10 = OR IF
        SWAP IOR-F-RETRYABLE OR SWAP
    THEN
    DUP 11 = IF SWAP IOR-F-STALE OR SWAP THEN
    DUP IOR-D-BLOCK 3 ROLL IOR-MAKE ;

\ Descriptor field readers.  Fields not exposed here remain diagnostic.
: BD.COOKIE      ( bd -- u )  16 + @ ;
: BD.MEDIA-GEN   ( bd -- u )  40 + @ ;
: BD.SECTOR-SIZE ( bd -- u )  48 + @ ;
: BD.SECTORS     ( bd -- u )  56 + @ ;
: BD.CAPS        ( bd -- u )  64 + @ ;
: BD.FLAGS       ( bd -- u )  72 + @ ;
: BD.REFS        ( bd -- a )  88 + ;

: VOL.COOKIE      ( vol -- u )  16 + @ ;
: VOL.BD          ( vol -- bd ) 24 + @ ;
: VOL.BD-COOKIE   ( vol -- u )  32 + @ ;
: VOL.MEDIA-GEN   ( vol -- u )  40 + @ ;
: VOL.BASE        ( vol -- u )  48 + @ ;
: VOL.SECTORS     ( vol -- u )  56 + @ ;
: VOL.SECTOR-SIZE ( vol -- u )  64 + @ ;
: VOL.FLAGS       ( vol -- u )  72 + @ ;
: VOL.SCHEME      ( vol -- u )  80 + @ ;
: VOL.INDEX       ( vol -- u )  88 + @ ;

0 CONSTANT VOL-SCHEME-RAW
1 CONSTANT VOL-SCHEME-MBR
2 CONSTANT VOL-SCHEME-GPT
1 CONSTANT VOL-F-READONLY

\ Overflow-safe interval validation: count is nonzero and
\ lba <= length-count.  All inputs are treated as unsigned cells.
: BLOCK-RANGE?  ( lba count length -- flag )
    >R
    DUP 0= IF 2DROP R> DROP FALSE EXIT THEN
    DUP R@ U> IF 2DROP R> DROP FALSE EXIT THEN
    R> SWAP - U> 0= ;

VARIABLE STORAGE-COOKIE
0 STORAGE-COOKIE !
: STORAGE-COOKIE-NEXT  ( -- u )
    1 STORAGE-COOKIE +!
    STORAGE-COOKIE @ DUP 0= IF DROP 1 STORAGE-COOKIE +! STORAGE-COOKIE @ THEN ;

\ Required controller facilities: READ, precise result, completion counter,
\ media generation, and atomic generation guard.  WRITE/FLUSH remain
\ discoverable capabilities so a future read-only backend can still open.
0x79 CONSTANT BD-REQUIRED-CAPS

: BD-VALID?  ( bd -- flag )
    DUP 0= IF DROP FALSE EXIT THEN
    DUP @ BLOCK-DEVICE-MAGIC =
    OVER 8 + @ STORAGE-ABI = AND
    OVER BD.COOKIE 0<> AND
    OVER BD.SECTOR-SIZE SECTOR = AND
    OVER BD.SECTORS 0<> AND
    SWAP BD.CAPS BD-REQUIRED-CAPS AND BD-REQUIRED-CAPS = AND ;

: BD-STALE?  ( bd -- flag )
    DUP BD-VALID? 0= IF DROP TRUE EXIT THEN
    DISK? 0= IF DROP TRUE EXIT THEN
    BD.MEDIA-GEN DISK-MEDIA-GEN <> ;

: BD-OPEN  ( bd -- ior )
    DUP BD-VALID? IF
        DUP BD.REFS @ IF DROP BD-E-BUSY EXIT THEN
    THEN
    DUP /BLOCK-DEVICE 0 FILL
    DISK? 0= IF DROP BD-E-NO-MEDIA EXIT THEN
    DISK-CAPS BD-REQUIRED-CAPS AND BD-REQUIRED-CAPS <> IF
        DROP BD-E-UNSUPPORTED EXIT
    THEN
    STORAGE-ABI OVER 8 + !
    STORAGE-COOKIE-NEXT OVER 16 + !
    1 OVER 24 + !                    \ backend kind: Megapad controller
    1 OVER 32 + !                    \ controller/backend identity
    DISK-MEDIA-GEN OVER 40 + !
    SECTOR OVER 48 + !
    DISK-SECTORS DUP 0= IF
        DROP /BLOCK-DEVICE 0 FILL BD-E-NO-MEDIA EXIT
    THEN
    OVER 56 + !
    DISK-CAPS OVER 64 + !
    0
    DISK-CAPS 2 AND 0= IF VOL-F-READONLY OR THEN
    DISK@ 32 AND IF VOL-F-READONLY OR THEN
    OVER 72 + !
    1 OVER 80 + !                    \ synchronization kind: FS lock/controller
    DISK-MEDIA-GEN OVER BD.MEDIA-GEN <> IF
        /BLOCK-DEVICE 0 FILL BD-E-STALE EXIT
    THEN
    BLOCK-DEVICE-MAGIC SWAP !
    0 ;

: BD-CLOSE  ( bd -- ior )
    DUP BD-VALID? 0= IF DROP 0 EXIT THEN
    DUP BD.REFS @ IF DROP BD-E-BUSY EXIT THEN
    /BLOCK-DEVICE 0 FILL 0 ;

: _BD-CHECK  ( lba count bd -- ior )
    >R
    R@ BD-VALID? 0= IF 2DROP R> DROP BD-E-BAD-DESCRIPTOR EXIT THEN
    R@ BD-STALE? IF 2DROP R> DROP BD-E-STALE EXIT THEN
    2DUP R@ BD.SECTORS BLOCK-RANGE? 0= IF
        2DROP R> DROP BD-E-RANGE EXIT
    THEN
    2DROP R> DROP 0 ;

: BD-READ  ( dma lba count bd -- completed ior )
    2 PICK 2 PICK 2 PICK _BD-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R
    2 PICK R@ 112 + !
    OVER R@ 120 + !
    BD.MEDIA-GEN DISK-READ-GEN-CHECKED
    DUP IOR-FROM-BLOCK-RESULT SWAP DROP
    DUP R@ 96 + !
    OVER R@ 104 + !
    R> DROP ;

: BD-WRITE  ( dma lba count bd -- completed ior )
    DUP BD-VALID? IF
        DUP BD.FLAGS VOL-F-READONLY AND IF
            2DROP 2DROP 0 BD-E-READONLY EXIT
        THEN
    THEN
    2 PICK 2 PICK 2 PICK _BD-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R
    2 PICK R@ 112 + !
    OVER R@ 120 + !
    BD.MEDIA-GEN DISK-WRITE-GEN-CHECKED
    DUP IOR-FROM-BLOCK-RESULT SWAP DROP
    DUP R@ 96 + !
    OVER R@ 104 + !
    R> DROP ;

: BD-FLUSH  ( bd -- ior )
    DUP BD-VALID? 0= IF DROP BD-E-BAD-DESCRIPTOR EXIT THEN
    DUP BD-STALE? IF DROP BD-E-STALE EXIT THEN
    DUP BD.CAPS 4 AND 0= IF DROP BD-E-UNSUPPORTED EXIT THEN
    DUP >R BD.MEDIA-GEN DISK-FLUSH-GEN-CHECKED
    IOR-FROM-BLOCK-RESULT
    DUP R@ 96 + !
    0 R@ 104 + !
    R> DROP ;

: VOL-VALID?  ( vol -- flag )
    DUP 0= IF DROP FALSE EXIT THEN
    >R
    R@ @ VOLUME-MAGIC =
    R@ 8 + @ STORAGE-ABI = AND
    R@ VOL.COOKIE 0<> AND
    R@ VOL.SECTOR-SIZE SECTOR = AND
    R@ VOL.SECTORS 0<> AND
    R@ VOL.BD BD-VALID? AND
    R@ VOL.BD-COOKIE R@ VOL.BD BD.COOKIE = AND
    R@ VOL.BASE R@ VOL.SECTORS R@ VOL.BD BD.SECTORS BLOCK-RANGE? AND
    R> DROP ;

: VOL-STALE?  ( vol -- flag )
    DUP VOL-VALID? 0= IF DROP TRUE EXIT THEN
    DUP VOL.MEDIA-GEN OVER VOL.BD BD.MEDIA-GEN <> IF DROP TRUE EXIT THEN
    VOL.BD BD-STALE? ;

: _VOL-CLEAR  ( vol -- )
    DUP VOL-VALID? IF
        DUP VOL.BD DUP BD-VALID? IF -1 SWAP BD.REFS +! ELSE DROP THEN
    THEN
    /VOLUME 0 FILL ;

VARIABLE _VOL-BD
VARIABLE _VOL-PTR
VARIABLE _VOL-BASE
VARIABLE _VOL-LEN
VARIABLE _VOL-SCHEME
VARIABLE _VOL-INDEX

\ VOL-SLICE validates and constructs one bounded slice.  Partition-specific
\ type/GUID fields may be filled only after an entire table validates.
: VOL-SLICE  ( base length scheme index bd vol -- ior )
    _VOL-PTR ! _VOL-BD ! _VOL-INDEX ! _VOL-SCHEME ! _VOL-LEN ! _VOL-BASE !
    _VOL-BD @ BD-VALID? 0= IF VOL-E-BAD-DESCRIPTOR EXIT THEN
    _VOL-BD @ BD-STALE? IF VOL-E-STALE EXIT THEN
    _VOL-BASE @ _VOL-LEN @ _VOL-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        VOL-E-RANGE EXIT
    THEN
    _VOL-PTR @ _VOL-CLEAR
    _VOL-PTR @ /VOLUME 0 FILL
    STORAGE-ABI _VOL-PTR @ 8 + !
    STORAGE-COOKIE-NEXT _VOL-PTR @ 16 + !
    _VOL-BD @ _VOL-PTR @ 24 + !
    _VOL-BD @ BD.COOKIE _VOL-PTR @ 32 + !
    _VOL-BD @ BD.MEDIA-GEN _VOL-PTR @ 40 + !
    _VOL-BASE @ _VOL-PTR @ 48 + !
    _VOL-LEN @ _VOL-PTR @ 56 + !
    _VOL-BD @ BD.SECTOR-SIZE _VOL-PTR @ 64 + !
    _VOL-BD @ BD.FLAGS _VOL-PTR @ 72 + !
    _VOL-SCHEME @ _VOL-PTR @ 80 + !
    _VOL-INDEX @ _VOL-PTR @ 88 + !
    VOLUME-MAGIC _VOL-PTR @ !
    1 _VOL-BD @ BD.REFS +!
    0 ;

VARIABLE _VR-BD
VARIABLE _VR-VOL
: VOL-RAW  ( bd vol -- ior )
    _VR-VOL ! _VR-BD !
    0 _VR-BD @ BD.SECTORS VOL-SCHEME-RAW 0 _VR-BD @ _VR-VOL @ VOL-SLICE ;

: VOL-CLOSE  ( vol -- ior )
    _VOL-CLEAR 0 ;

: _VOL-CHECK  ( lba count vol -- ior )
    >R
    R@ VOL-VALID? 0= IF 2DROP R> DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    R@ VOL-STALE? IF 2DROP R> DROP VOL-E-STALE EXIT THEN
    2DUP R@ VOL.SECTORS BLOCK-RANGE? 0= IF
        2DROP R> DROP VOL-E-RANGE EXIT
    THEN
    2DROP R> DROP 0 ;

: VOL-READ  ( dma lba count vol -- completed ior )
    2 PICK 2 PICK 2 PICK _VOL-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R DROP
    SWAP R@ VOL.BASE + SWAP R@ VOL.BD BD-READ
    R> DROP ;

: VOL-WRITE  ( dma lba count vol -- completed ior )
    DUP VOL-VALID? IF
        DUP VOL.FLAGS VOL-F-READONLY AND IF
            2DROP 2DROP 0 VOL-E-READONLY EXIT
        THEN
    THEN
    2 PICK 2 PICK 2 PICK _VOL-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R DROP
    SWAP R@ VOL.BASE + SWAP R@ VOL.BD BD-WRITE
    R> DROP ;

: VOL-FLUSH  ( vol -- ior )
    DUP VOL-VALID? 0= IF DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    DUP VOL-STALE? IF DROP VOL-E-STALE EXIT THEN
    VOL.BD BD-FLUSH ;

\ ---------------------------------------------------------------------
\  Read-only partition discovery (raw, MBR, and GPT)
\ ---------------------------------------------------------------------

5120 CONSTANT PART-WORKSPACE-MIN
4096 CONSTANT GPT-MAX-ENTRIES
4096 CONSTANT GPT-MAX-ENTRY-SIZE

9 IOR-C-CORRUPT IOR-D-PARTITION IOR-F-CORRUPT
    IOR-MAKE CONSTANT PART-E-CORRUPT
4 IOR-C-CAPACITY IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-CAPACITY
14 IOR-C-WORKSPACE IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-WORKSPACE
2 IOR-C-UNSUPPORTED IOR-D-PARTITION IOR-F-UNSUPPORTED
    IOR-MAKE CONSTANT PART-E-UNSUPPORTED
14 IOR-C-BAD-DESCRIPTOR IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-BAD-DESCRIPTOR

VARIABLE _PART-BD
VARIABLE _PART-OUT
VARIABLE _PART-MAX
VARIABLE _PART-WS
VARIABLE _PART-BYTES
VARIABLE _PART-COUNT

: PART-VOLUME  ( index -- vol )  /VOLUME * _PART-OUT @ + ;

: _PART-CLEAR  ( -- )
    _PART-MAX @ 0 DO I PART-VOLUME VOL-CLOSE DROP LOOP
    0 _PART-COUNT ! ;

: _PART-SETUP  ( bd volumes max workspace bytes -- ior )
    _PART-BYTES ! _PART-WS ! _PART-MAX ! _PART-OUT ! _PART-BD !
    _PART-OUT @ 0= _PART-MAX @ 0= OR IF PART-E-CAPACITY EXIT THEN
    \ Once the output extent itself is usable, every later failure is
    \ transactional even if it occurs before the first metadata read.
    _PART-CLEAR
    _PART-BD @ BD-VALID? 0= IF PART-E-BAD-DESCRIPTOR EXIT THEN
    _PART-BD @ BD-STALE? IF VOL-E-STALE EXIT THEN
    _PART-WS @ 0= _PART-BYTES @ PART-WORKSPACE-MIN < OR IF
        PART-E-WORKSPACE EXIT
    THEN
    0 ;

: _PART-FAIL  ( ior -- count ior )
    _PART-CLEAR 0 SWAP ;

: _PART-READ  ( buffer lba -- ior )
    1 _PART-BD @ BD-READ             ( completed ior )
    DUP IF NIP EXIT THEN
    DROP 1 = IF 0 ELSE BD-E-INTERNAL THEN ;

: _PART-FINALIZE  ( vol -- ior )
    DUP 48 + @ OVER 56 + @ _PART-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        DROP PART-E-CORRUPT EXIT
    THEN
    STORAGE-ABI OVER 8 + !
    STORAGE-COOKIE-NEXT OVER 16 + !
    _PART-BD @ OVER 24 + !
    _PART-BD @ BD.COOKIE OVER 32 + !
    _PART-BD @ BD.MEDIA-GEN OVER 40 + !
    _PART-BD @ BD.SECTOR-SIZE OVER 64 + !
    _PART-BD @ BD.FLAGS OVER 72 + !
    VOLUME-MAGIC SWAP !
    1 _PART-BD @ BD.REFS +!
    0 ;

VARIABLE _OV-A
VARIABLE _OV-ALEN
VARIABLE _OV-B
VARIABLE _OV-BLEN
: _RANGES-OVERLAP?  ( a alen b blen -- flag )
    _OV-BLEN ! _OV-B ! _OV-ALEN ! _OV-A !
    _OV-A @ _OV-B @ _OV-BLEN @ + U<
    _OV-B @ _OV-A @ _OV-ALEN @ + U< AND ;

: _MBR-ENTRY  ( table-index -- addr )  16 * _PART-WS @ 446 + + ;
: _MBR-TYPE   ( entry -- type )  4 + C@ ;
: _MBR-BASE   ( entry -- lba )   8 + L@ ;
: _MBR-LEN    ( entry -- count ) 12 + L@ ;
: _MBR-EXTENDED?  ( type -- flag )
    DUP 0x05 = OVER 0x0F = OR SWAP 0x85 = OR ;

VARIABLE _MBR-E
VARIABLE _MBR-TYPE-V
VARIABLE _MBR-BASE-V
VARIABLE _MBR-LEN-V
VARIABLE _MBR-BOOT-V
VARIABLE _MBR-INDEX-V

: _MBR-CANDIDATE-OVERLAP?  ( base len -- flag )
    _MBR-LEN-V ! _MBR-BASE-V !
    FALSE
    _PART-COUNT @ DUP IF
        0 DO
            _MBR-BASE-V @ _MBR-LEN-V @
            I PART-VOLUME VOL.BASE I PART-VOLUME VOL.SECTORS
            _RANGES-OVERLAP? OR
        LOOP
    ELSE DROP THEN ;

: _MBR-STAGE  ( table-index -- ior )
    DUP _MBR-INDEX-V ! _MBR-ENTRY _MBR-E !
    _MBR-E @ C@ _MBR-BOOT-V !
    _MBR-E @ _MBR-TYPE _MBR-TYPE-V !
    _MBR-E @ _MBR-BASE _MBR-BASE-V !
    _MBR-E @ _MBR-LEN _MBR-LEN-V !
    _MBR-TYPE-V @ 0= IF
        _MBR-BOOT-V @ _MBR-BASE-V @ OR _MBR-LEN-V @ OR IF
            PART-E-CORRUPT EXIT
        THEN
        0 EXIT
    THEN
    _MBR-BOOT-V @ DUP 0<> SWAP 0x80 <> AND IF
        PART-E-CORRUPT EXIT
    THEN
    _MBR-TYPE-V @ _MBR-EXTENDED? IF PART-E-UNSUPPORTED EXIT THEN
    _MBR-BASE-V @ 0= _MBR-LEN-V @ 0= OR IF PART-E-CORRUPT EXIT THEN
    _MBR-BASE-V @ _MBR-LEN-V @ _PART-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        PART-E-CORRUPT EXIT
    THEN
    _MBR-BASE-V @ _MBR-LEN-V @ _MBR-CANDIDATE-OVERLAP? IF
        PART-E-CORRUPT EXIT
    THEN
    _PART-COUNT @ _PART-MAX @ >= IF PART-E-CAPACITY EXIT THEN
    _PART-COUNT @ PART-VOLUME DUP /VOLUME 0 FILL
    DUP _MBR-BASE-V @ SWAP 48 + !
    DUP _MBR-LEN-V @ SWAP 56 + !
    DUP VOL-SCHEME-MBR SWAP 80 + !
    DUP _MBR-INDEX-V @ SWAP 88 + !
    DUP _MBR-TYPE-V @ SWAP 96 + !
    _MBR-BOOT-V @ SWAP 136 + !
    1 _PART-COUNT +!
    0 ;

: _MBR-PROTECTIVE-VALID?  ( -- flag )
    _PART-COUNT @ 1 <> IF FALSE EXIT THEN
    0 PART-VOLUME
    DUP 96 + @ 0xEE =
    OVER VOL.BASE 1 = AND
    OVER VOL.SECTORS _PART-BD @ BD.SECTORS 1- = AND
    SWAP 136 + @ 0= AND ;

: _MBR-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 510 + W@ 0xAA55 <> IF PART-E-CORRUPT _PART-FAIL EXIT THEN
    4 0 DO
        I _MBR-STAGE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
    LOOP
    \ A protective table is GPT metadata, never an ordinary MBR slice.
    FALSE
    _PART-COUNT @ DUP IF
        0 DO I PART-VOLUME 96 + @ 0xEE = OR LOOP
    ELSE DROP THEN
    IF
        _MBR-PROTECTIVE-VALID? IF PART-E-UNSUPPORTED ELSE PART-E-CORRUPT THEN
        _PART-FAIL EXIT
    THEN
    _PART-BD @ BD-STALE? IF VOL-E-STALE _PART-FAIL EXIT THEN
    _PART-COUNT @ DUP IF
        0 DO
            I PART-VOLUME _PART-FINALIZE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
        LOOP
    ELSE DROP THEN
    _PART-COUNT @ 0 ;

\ GPT uses the reflected IEEE CRC-32 tuple.  KDOS's general CRC32-BUF is the
\ non-reflected BZIP2 tuple and is deliberately not reused here.
VARIABLE _IEEE-CRC
: CRC32-IEEE-START  ( -- )  0xFFFFFFFF _IEEE-CRC ! ;
: CRC32-IEEE-FEED  ( addr bytes -- )
    BEGIN DUP 0> WHILE
        OVER C@ _IEEE-CRC @ XOR
        8 0 DO
            DUP 1 AND IF 1 RSHIFT 0xEDB88320 XOR ELSE 1 RSHIFT THEN
        LOOP
        0xFFFFFFFF AND _IEEE-CRC !
        SWAP 1+ SWAP 1-
    REPEAT
    2DROP ;
: CRC32-IEEE-FINISH  ( -- crc )  _IEEE-CRC @ INVERT 0xFFFFFFFF AND ;
: CRC32-IEEE-BUF  ( addr bytes -- crc )
    CRC32-IEEE-START CRC32-IEEE-FEED CRC32-IEEE-FINISH ;

: _GUID-ZERO?  ( guid -- flag )  DUP @ SWAP 8 + @ OR 0= ;
: _GUID-SAME?  ( guid-a guid-b -- flag )
    2DUP @ SWAP @ = >R
    8 + @ SWAP 8 + @ = R> AND ;

: _BYTES-ZERO?  ( addr bytes -- flag )
    BEGIN DUP 0> WHILE
        OVER C@ IF 2DROP FALSE EXIT THEN
        SWAP 1+ SWAP 1-
    REPEAT
    2DROP TRUE ;

VARIABLE _GH-BUF
VARIABLE _GH-CUR
VARIABLE _GH-BACK
VARIABLE _GH-SAVED-CRC

: _GPT-HEADER-CRC?  ( header -- flag )
    DUP _GH-BUF !
    16 + L@ _GH-SAVED-CRC !
    0 _GH-BUF @ 16 + L!
    _GH-BUF @ DUP 12 + L@ CRC32-IEEE-BUF
    _GH-SAVED-CRC @ _GH-BUF @ 16 + L!
    _GH-SAVED-CRC @ = ;

: _GPT-HEADER-VALID?  ( header current-lba backup-lba -- flag )
    _GH-BACK ! _GH-CUR ! _GH-BUF !
    _GH-BUF @ @ 0x5452415020494645 <> IF FALSE EXIT THEN  \ "EFI PART"
    _GH-BUF @ 8 + L@ 0x00010000 <> IF FALSE EXIT THEN
    _GH-BUF @ 12 + L@ DUP 92 < SWAP 512 > OR IF FALSE EXIT THEN
    _GH-BUF @ 20 + L@ IF FALSE EXIT THEN
    _GH-BUF @ 24 + @ _GH-CUR @ <> IF FALSE EXIT THEN
    _GH-BUF @ 32 + @ _GH-BACK @ <> IF FALSE EXIT THEN
    _GH-BUF @ 40 + @ 1 U> 0= IF FALSE EXIT THEN
    _GH-BUF @ 40 + @ _GH-BUF @ 48 + @ U> IF FALSE EXIT THEN
    _GH-BUF @ 48 + @ _PART-BD @ BD.SECTORS 1- U< 0= IF FALSE EXIT THEN
    _GH-BUF @ 56 + _GUID-ZERO? IF FALSE EXIT THEN
    _GH-BUF @ 72 + @ DUP 0= SWAP _PART-BD @ BD.SECTORS U< 0= OR IF
        FALSE EXIT
    THEN
    _GH-BUF @ 80 + L@ DUP 0= SWAP GPT-MAX-ENTRIES > OR IF FALSE EXIT THEN
    _GH-BUF @ 84 + L@ DUP 128 < SWAP GPT-MAX-ENTRY-SIZE > OR IF
        FALSE EXIT
    THEN
    _GH-BUF @ 84 + L@ 7 AND IF FALSE EXIT THEN
    \ UEFI reserves the complete remainder of the logical block, not only
    \ bytes counted by HeaderSize.  Revision 1.0 extensions are therefore
    \ admitted only when their currently reserved bytes remain zero.
    _GH-BUF @ 92 + SECTOR 92 - _BYTES-ZERO? 0= IF
        FALSE EXIT
    THEN
    _GH-BUF @ _GPT-HEADER-CRC? ;

VARIABLE _GPT-FIRST
VARIABLE _GPT-LAST
VARIABLE _GPT-PARRAY
VARIABLE _GPT-BARRAY
VARIABLE _GPT-NENT
VARIABLE _GPT-ESIZE
VARIABLE _GPT-ACRC
VARIABLE _GPT-GUID0
VARIABLE _GPT-GUID1
VARIABLE _GPT-HSIZE
VARIABLE _GPT-ARRAY-BYTES
VARIABLE _GPT-ARRAY-SECTORS

: _GPT-SAVE-PRIMARY  ( header -- )
    DUP 40 + @ _GPT-FIRST !
    DUP 48 + @ _GPT-LAST !
    DUP 56 + @ _GPT-GUID0 !
    DUP 64 + @ _GPT-GUID1 !
    DUP 72 + @ _GPT-PARRAY !
    DUP 80 + L@ _GPT-NENT !
    DUP 84 + L@ _GPT-ESIZE !
    DUP 88 + L@ _GPT-ACRC !
    12 + L@ _GPT-HSIZE !
    _GPT-NENT @ _GPT-ESIZE @ * DUP _GPT-ARRAY-BYTES !
    511 + SECTOR / _GPT-ARRAY-SECTORS ! ;

: _GPT-HEADERS-AGREE?  ( backup-header -- flag )
    DUP 40 + @ _GPT-FIRST @ =
    OVER 48 + @ _GPT-LAST @ = AND
    OVER 56 + @ _GPT-GUID0 @ = AND
    OVER 64 + @ _GPT-GUID1 @ = AND
    OVER 80 + L@ _GPT-NENT @ = AND
    OVER 84 + L@ _GPT-ESIZE @ = AND
    OVER 88 + L@ _GPT-ACRC @ = AND
    OVER 12 + L@ _GPT-HSIZE @ = AND
    SWAP 72 + @ DUP _GPT-BARRAY ! 0<> AND ;

: _GPT-METADATA-VALID?  ( -- flag )
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS BLOCK-RANGE?
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS BLOCK-RANGE? AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _GPT-BARRAY @ _GPT-ARRAY-SECTORS @
        _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ 0 2 _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ 0 2 _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS 1- 1
        _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS 1- 1
        _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @
        _GPT-FIRST @ _GPT-LAST @ _GPT-FIRST @ - 1+
        _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @
        _GPT-FIRST @ _GPT-LAST @ _GPT-FIRST @ - 1+
        _RANGES-OVERLAP? 0= AND ;

VARIABLE _GP-USED
VARIABLE _GP-OK
: _GPT-PROTECTIVE-MBR?  ( -- flag )
    _PART-WS @ 510 + W@ 0xAA55 <> IF FALSE EXIT THEN
    0 _GP-USED ! TRUE _GP-OK !
    4 0 DO
        I _MBR-ENTRY _MBR-E !
        _MBR-E @ C@ _MBR-BOOT-V !
        _MBR-E @ _MBR-TYPE _MBR-TYPE-V !
        _MBR-E @ _MBR-BASE _MBR-BASE-V !
        _MBR-E @ _MBR-LEN _MBR-LEN-V !
        _MBR-TYPE-V @ 0= IF
            _MBR-BOOT-V @ _MBR-BASE-V @ OR _MBR-LEN-V @ OR IF
                FALSE _GP-OK !
            THEN
        ELSE
            1 _GP-USED +!
            _MBR-BOOT-V @ 0<> IF FALSE _GP-OK ! THEN
            _MBR-TYPE-V @ 0xEE <> IF FALSE _GP-OK ! THEN
            _MBR-BASE-V @ 1 <> IF FALSE _GP-OK ! THEN
            _MBR-LEN-V @ _PART-BD @ BD.SECTORS 1- <> IF FALSE _GP-OK ! THEN
        THEN
    LOOP
    _GP-OK @ _GP-USED @ 1 = AND ;

VARIABLE _GA-LBA
VARIABLE _GA-REM
VARIABLE _GA-CHUNK
VARIABLE _GA-EXPECTED

: _GPT-ARRAY-CRC?  ( array-lba expected-crc -- ior )
    _GA-EXPECTED ! _GA-LBA !
    _GPT-ARRAY-BYTES @ _GA-REM !
    CRC32-IEEE-START
    BEGIN _GA-REM @ 0> WHILE
        _PART-WS @ _GA-LBA @ _PART-READ ?DUP IF EXIT THEN
        _GA-REM @ SECTOR MIN DUP _GA-CHUNK !
        _PART-WS @ SWAP CRC32-IEEE-FEED
        1 _GA-LBA +!
        _GA-CHUNK @ NEGATE _GA-REM +!
    REPEAT
    CRC32-IEEE-FINISH _GA-EXPECTED @ = IF 0 ELSE PART-E-CORRUPT THEN ;

: _GPT-ARRAYS-AGREE?  ( -- ior )
    _GPT-ARRAY-BYTES @ _GA-REM !
    _GPT-PARRAY @ _GA-LBA !
    _GPT-BARRAY @ _GA-EXPECTED !
    BEGIN _GA-REM @ 0> WHILE
        _PART-WS @ _GA-LBA @ _PART-READ ?DUP IF EXIT THEN
        _PART-WS @ 512 + _GA-EXPECTED @ _PART-READ ?DUP IF EXIT THEN
        _GA-REM @ SECTOR MIN DUP _GA-CHUNK !
        DUP >R _PART-WS @ SWAP _PART-WS @ 512 + R> COMPARE 0<> IF
            PART-E-CORRUPT EXIT
        THEN
        1 _GA-LBA +!
        1 _GA-EXPECTED +!
        _GA-CHUNK @ NEGATE _GA-REM +!
    REPEAT
    0 ;

VARIABLE _GE-INDEX
VARIABLE _GE-ARRAY
VARIABLE _GE-SECTOR
VARIABLE _GE-INTRA

: _GPT-READ-ENTRY  ( index array-lba -- entry ior )
    _GE-ARRAY ! _GE-INDEX !
    _GE-INDEX @ _GPT-ESIZE @ *
    DUP SECTOR / _GE-ARRAY @ + _GE-SECTOR !
    SECTOR MOD _GE-INTRA !
    _PART-WS @ _GE-SECTOR @ _PART-READ ?DUP IF 0 SWAP EXIT THEN
    _GE-INTRA @ 56 + SECTOR > IF
        _PART-WS @ 512 + _GE-SECTOR @ 1+ _PART-READ ?DUP IF
            0 SWAP EXIT
        THEN
    THEN
    _PART-WS @ _GE-INTRA @ + 0 ;

VARIABLE _GPE-TYPE0
VARIABLE _GPE-TYPE1
VARIABLE _GPE-UNIQ0
VARIABLE _GPE-UNIQ1
VARIABLE _GPE-FIRST
VARIABLE _GPE-LAST
VARIABLE _GPE-LEN
VARIABLE _GPE-ATTR
VARIABLE _GPE-INDEX
VARIABLE _GPC-CONFLICT
VARIABLE _GPC-IOR
VARIABLE _GPC-P

: _GPT-PRIOR-CONFLICT?  ( current-index -- conflict ior )
    0 _GPC-CONFLICT ! 0 _GPC-IOR !
    DUP IF
        0 DO
            I _GPT-PARRAY @ _GPT-READ-ENTRY
            DUP IF
                _GPC-IOR ! DROP LEAVE
            THEN
            DROP                              ( prior-entry )
            DUP _GPC-P !
            DUP _GUID-ZERO? 0= IF
                DUP 16 + @ _GPE-UNIQ0 @ =
                OVER 24 + @ _GPE-UNIQ1 @ = AND IF
                    TRUE _GPC-CONFLICT !
                THEN
                _GPC-P @ 32 + @
                _GPC-P @ 40 + @ _GPC-P @ 32 + @ - 1+
                _GPE-FIRST @ _GPE-LEN @ _RANGES-OVERLAP? IF
                    TRUE _GPC-CONFLICT !
                THEN
            THEN
            DROP
            _GPC-CONFLICT @ IF LEAVE THEN
        LOOP
    ELSE DROP THEN
    _GPC-CONFLICT @ _GPC-IOR @ ;

: _GPT-STAGE-ENTRY  ( table-index -- ior )
    DUP _GPE-INDEX !
    _GPT-PARRAY @ _GPT-READ-ENTRY
    DUP IF NIP EXIT THEN DROP               ( entry )
    DUP _GUID-ZERO? IF DROP 0 EXIT THEN
    DUP @ _GPE-TYPE0 !
    DUP 8 + @ _GPE-TYPE1 !
    DUP 16 + @ _GPE-UNIQ0 !
    DUP 24 + @ _GPE-UNIQ1 !
    _GPE-UNIQ0 @ _GPE-UNIQ1 @ OR 0= IF DROP PART-E-CORRUPT EXIT THEN
    DUP 32 + @ _GPE-FIRST !
    DUP 40 + @ _GPE-LAST !
    48 + @ _GPE-ATTR !
    _GPE-FIRST @ _GPE-LAST @ U> IF PART-E-CORRUPT EXIT THEN
    _GPE-FIRST @ _GPT-FIRST @ U<
    _GPE-LAST @ _GPT-LAST @ U> OR IF PART-E-CORRUPT EXIT THEN
    _GPE-LAST @ _GPE-FIRST @ - 1+ DUP 0= IF DROP PART-E-CORRUPT EXIT THEN
    _GPE-LEN !
    _GPE-INDEX @ _GPT-PRIOR-CONFLICT?
    DUP IF NIP EXIT THEN
    DROP IF PART-E-CORRUPT EXIT THEN
    _PART-COUNT @ _PART-MAX @ >= IF PART-E-CAPACITY EXIT THEN
    _PART-COUNT @ PART-VOLUME DUP /VOLUME 0 FILL
    DUP _GPE-FIRST @ SWAP 48 + !
    DUP _GPE-LEN @ SWAP 56 + !
    DUP VOL-SCHEME-GPT SWAP 80 + !
    DUP _GPE-INDEX @ SWAP 88 + !
    DUP _GPE-TYPE0 @ SWAP 104 + !
    DUP _GPE-TYPE1 @ SWAP 112 + !
    DUP _GPE-UNIQ0 @ SWAP 120 + !
    DUP _GPE-UNIQ1 @ SWAP 128 + !
    _GPE-ATTR @ SWAP 136 + !
    1 _PART-COUNT +!
    0 ;

: _GPT-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    _PART-BD @ BD.SECTORS 6 < IF PART-E-CORRUPT _PART-FAIL EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _GPT-PROTECTIVE-MBR? 0= IF PART-E-CORRUPT _PART-FAIL EXIT THEN

    _PART-WS @ 1 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 1 _PART-BD @ BD.SECTORS 1- _GPT-HEADER-VALID? 0= IF
        PART-E-CORRUPT _PART-FAIL EXIT
    THEN
    _PART-WS @ _GPT-SAVE-PRIMARY

    _PART-WS @ 512 + _PART-BD @ BD.SECTORS 1- _PART-READ ?DUP IF
        _PART-FAIL EXIT
    THEN
    _PART-WS @ 512 + _PART-BD @ BD.SECTORS 1- 1
        _GPT-HEADER-VALID? 0= IF PART-E-CORRUPT _PART-FAIL EXIT THEN
    _PART-WS @ 512 + _GPT-HEADERS-AGREE? 0= IF
        PART-E-CORRUPT _PART-FAIL EXIT
    THEN
    _GPT-METADATA-VALID? 0= IF PART-E-CORRUPT _PART-FAIL EXIT THEN

    _GPT-PARRAY @ _GPT-ACRC @ _GPT-ARRAY-CRC? ?DUP IF _PART-FAIL EXIT THEN
    _GPT-BARRAY @ _GPT-ACRC @ _GPT-ARRAY-CRC? ?DUP IF _PART-FAIL EXIT THEN
    _GPT-ARRAYS-AGREE? ?DUP IF _PART-FAIL EXIT THEN

    _GPT-NENT @ 0 DO
        I _GPT-STAGE-ENTRY ?DUP IF UNLOOP _PART-FAIL EXIT THEN
    LOOP
    _PART-BD @ BD-STALE? IF VOL-E-STALE _PART-FAIL EXIT THEN
    _PART-COUNT @ DUP IF
        0 DO
            I PART-VOLUME _PART-FINALIZE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
        LOOP
    ELSE DROP THEN
    _PART-COUNT @ 0 ;

VARIABLE _PS-USED
VARIABLE _PS-PROTECTIVE

: _PART-RAW-RESULT  ( -- count ior )
    _PART-BD @ 0 PART-VOLUME VOL-RAW ?DUP IF _PART-FAIL EXIT THEN
    1 _PART-COUNT !
    1 0 ;

: _PART-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 510 + W@ 0xAA55 <> IF _PART-RAW-RESULT EXIT THEN
    0 _PS-USED ! 0 _PS-PROTECTIVE !
    4 0 DO
        I _MBR-ENTRY _MBR-TYPE DUP IF
            1 _PS-USED +!
            0xEE = IF TRUE _PS-PROTECTIVE ! THEN
        ELSE DROP THEN
    LOOP
    _PS-PROTECTIVE @ IF
        _PART-BD @ _PART-OUT @ _PART-MAX @ _PART-WS @ _PART-BYTES @ _GPT-SCAN
        EXIT
    THEN
    _PS-USED @ 0= IF
        4 0 DO
            I _MBR-ENTRY DUP C@ SWAP DUP _MBR-BASE SWAP _MBR-LEN OR OR IF
                PART-E-CORRUPT UNLOOP _PART-FAIL EXIT
            THEN
        LOOP
        _PART-RAW-RESULT EXIT
    THEN
    _PART-BD @ _PART-OUT @ _PART-MAX @ _PART-WS @ _PART-BYTES @ _MBR-SCAN ;

\ Partition parsing uses fixed scratch variables in addition to the caller's
\ data workspace.  Serialize the complete administrative scan on machine
\ lock 0 (the dictionary lock) while individual I/O continues to use the
\ checked block layer's lock 2.  CATCH guarantees release if a parser defect
\ or caller exception escapes despite the normal structured-error paths.
: _PART-LOCK  ( -- )  BEGIN 0 SPIN@ 0= UNTIL ;
: _PART-UNLOCK  ( -- )  0 SPIN! ;
: MBR-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _MBR-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;
: GPT-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _GPT-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;
: PART-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _PART-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;

\ The KDOS singleton is a raw compatibility/recovery binding.  Akashic VFS
\ instances may bind other validated volume descriptors independently.
CREATE SYSTEM-BD  /BLOCK-DEVICE ALLOT
CREATE SYSTEM-RAW-VOLUME  /VOLUME ALLOT
VARIABLE FS-VOLUME
SYSTEM-RAW-VOLUME FS-VOLUME !

\ Declared here so any stale storage completion can fail a loaded MP64FS
\ closed.  The caches and the rest of MP64FS are declared below.
VARIABLE FS-OK
0 FS-OK !

: STORAGE-OPEN  ( -- ior )
    SYSTEM-RAW-VOLUME VOL-CLOSE DROP
    SYSTEM-BD BD-CLOSE DROP
    SYSTEM-BD BD-OPEN ?DUP IF EXIT THEN
    SYSTEM-BD SYSTEM-RAW-VOLUME VOL-RAW ?DUP IF
        SYSTEM-BD BD-CLOSE DROP
        EXIT
    THEN
    SYSTEM-RAW-VOLUME FS-VOLUME !
    0 ;

: FS-VOLUME!  ( vol -- ior )
    DUP VOL-VALID? 0= IF DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    DUP VOL-STALE? IF DROP VOL-E-STALE EXIT THEN
    FS-VOLUME !
    0 FS-OK !
    0 ;

: STORAGE-ENSURE  ( -- ior )
    FS-VOLUME @ VOL-VALID? 0= IF
        FS-OK @ IF 0 FS-OK ! VOL-E-STALE EXIT THEN
        STORAGE-OPEN EXIT
    THEN
    FS-VOLUME @ VOL-STALE? IF 0 FS-OK ! VOL-E-STALE EXIT THEN
    0 ;

\ Last checked result is retained for diagnostics even when the compatibility
\ wrapper raises ABORT.  A zero ior with a short transfer is an internal
\ contract failure, never a successful KDOS operation.
VARIABLE DISK-IO-STATUS
VARIABLE DISK-IO-COMPLETED
VARIABLE DISK-IO-IOR

: _DISK-XFER-OK?  ( expected completed ior -- flag )
    DISK-IO-IOR !
    DISK-IO-COMPLETED !
    DISK-IO-IOR @ IOR>RAW DISK-IO-STATUS !
    DISK-IO-IOR @ IOR-STALE? IF 0 FS-OK ! THEN
    DISK-IO-IOR @ 0<> IF DROP FALSE EXIT THEN
    DISK-IO-COMPLETED @ =
    DUP 0= IF
        14 DISK-IO-STATUS !
        BD-E-INTERNAL DISK-IO-IOR !
    THEN ;

\ Raw checked wrappers remain available for boot diagnostics and source
\ compatibility.  Production KDOS paths below use FS-VOLUME instead.
: _RAW-DISK-READ?  ( dma lba count -- flag )
    DUP >R DISK-READ-CHECKED
    IOR-FROM-BLOCK-RESULT
    R> -ROT _DISK-XFER-OK? ;

: _RAW-DISK-WRITE?  ( dma lba count -- flag )
    DUP >R DISK-WRITE-CHECKED
    IOR-FROM-BLOCK-RESULT
    R> -ROT _DISK-XFER-OK? ;

: _RAW-DISK-FLUSH?  ( -- flag )
    DISK-FLUSH-CHECKED IOR-FROM-BLOCK-RESULT
    DUP DISK-IO-IOR ! DUP IOR>RAW DISK-IO-STATUS ! 0= ;

: _DISK-READ?  ( dma lba count -- flag )
    DUP >R
    STORAGE-ENSURE ?DUP IF
        >R 2DROP DROP R> R> SWAP 0 SWAP _DISK-XFER-OK? EXIT
    THEN
    FS-VOLUME @ VOL-READ R> -ROT _DISK-XFER-OK? ;

: _DISK-WRITE?  ( dma lba count -- flag )
    DUP >R
    STORAGE-ENSURE ?DUP IF
        >R 2DROP DROP R> R> SWAP 0 SWAP _DISK-XFER-OK? EXIT
    THEN
    FS-VOLUME @ VOL-WRITE R> -ROT _DISK-XFER-OK? ;

: _DISK-FLUSH?  ( -- flag )
    STORAGE-ENSURE ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS ! FALSE EXIT
    THEN
    FS-VOLUME @ VOL-FLUSH
    DUP DISK-IO-IOR !
    DUP IOR>RAW DISK-IO-STATUS !
    DUP IOR-STALE? IF 0 FS-OK ! THEN
    0= ;

: _DISK-READ  ( dma lba count -- )
    _DISK-READ? 0= ABORT" Disk read failed" ;

: _DISK-WRITE  ( dma lba count -- )
    _DISK-WRITE? 0= ABORT" Disk write failed" ;

: _DISK-FLUSH  ( -- )
    _DISK-FLUSH? 0= ABORT" Disk flush failed" ;

\ B.SECTORS ( desc -- n ) number of disk sectors needed for buffer data
: B.SECTORS  ( desc -- n )  B.BYTES SECTOR 1- + SECTOR / ;

\ B.SAVE ( desc sector -- ) save buffer data to disk starting at sector
: B.SAVE  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT SWAP                  ( addr sector nsectors )
    _DISK-WRITE ;

\ B.LOAD ( desc sector -- ) load buffer data from disk starting at sector
: B.LOAD  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT SWAP                  ( addr sector nsectors )
    _DISK-READ ;

\ DISK-INFO ( -- ) print storage device status
: DISK-INFO  ( -- )
    ."  Storage: "
    DISK? IF
        ."  present" CR
    ELSE
        ."  not attached" CR
    THEN ;

\ =====================================================================
\  §7.5  File Abstraction
\ =====================================================================
\
\  A FILE is a simple contiguous region on disk.
\
\  FILE descriptor (4 cells = 32 bytes):
\    +0   start_sector   first sector on disk
\    +8   max_sectors    allocated size in sectors
\    +16  used_bytes     bytes actually written
\    +24  cursor         current read/write position (byte offset)
\
\  Uses a scratch buffer for partial-sector I/O.
\  Files are NOT tile-aligned; they use the DMA to a RAM scratch area.

\ -- Registry (up to 8 files) --
VARIABLE FILE-COUNT
0 FILE-COUNT !
VARIABLE FILE-TABLE  7 CELLS ALLOT

\ -- Scratch buffer for sector I/O (one sector) --
VARIABLE FSCRATCH  SECTOR 1- ALLOT

\ -- Field accessors --
: F.START  ( fdesc -- sec )    @ ;
: F.MAX    ( fdesc -- n )      8 + @ ;
: F.USED   ( fdesc -- n )     16 + @ ;
: F.CURSOR ( fdesc -- n )     24 + @ ;

\ -- Internal temp --
VARIABLE FDESC

\ FILE ( start_sector max_sectors "name" -- )
\   Create a file descriptor backed by disk sectors.
: FILE
    HERE FDESC !
    SWAP ,                    \ +0  start_sector
    DUP ,                     \ +8  max_sectors  (keep copy)
    0 ,                       \ +16 used_bytes = 0
    0 ,                       \ +24 cursor = 0
    DROP                      \ drop extra max_sectors copy
    \ register
    FILE-COUNT @ 8 < IF
        FDESC @ FILE-COUNT @ CELLS FILE-TABLE + !
        FILE-COUNT @ 1+ FILE-COUNT !
    THEN
    FDESC @ CONSTANT ;

\ FSEEK ( pos fdesc -- ) set cursor position
: FSEEK  ( pos fdesc -- )  24 + ! ;

\ FREWIND ( fdesc -- ) reset cursor to 0
: FREWIND  ( fdesc -- )  0 SWAP FSEEK ;

\ FSIZE ( fdesc -- n ) return used bytes
: FSIZE  ( fdesc -- n )  F.USED ;

\ FTRUNCATE ( n fdesc -- ) set used_bytes to n, clamp to capacity
\   Resets cursor to min(cursor, n).  Does NOT zero freed bytes.
VARIABLE FT-N
: FTRUNCATE  ( n fdesc -- )
    SWAP  OVER F.MAX SECTOR *  MIN  FT-N !  ( fdesc )
    FT-N @  OVER 16 + !               \ used_bytes = n'
    DUP F.CURSOR  FT-N @  MIN         ( fdesc cursor' )
    SWAP 24 + ! ;                      \ cursor = min(old_cursor, n')

\ FWRITE ( addr len fdesc -- ) write len bytes from addr at cursor
\   Handles byte-unaligned cursors via read-modify-write with FSCRATCH.
\   Head/tail partial sectors are read into FSCRATCH, patched, and
\   written back.  Full middle sectors DMA directly from user buffer.
\   Advances cursor by len.  Updates used_bytes.
VARIABLE FW-FD
VARIABLE FW-ADDR
VARIABLE FW-LEN
VARIABLE FW-REM     \ remaining bytes to write
VARIABLE FW-POS     \ current byte position in file

\ FW-DISK-SEC ( -- sec ) absolute disk sector for current FW-POS
: FW-DISK-SEC  FW-POS @ SECTOR / FW-FD @ F.START + ;

VARIABLE FW-CHUNK   \ temp: byte count for head copy
\ FW-HEAD ( -- ) read-modify-write leading partial sector
: FW-HEAD  ( -- )
    FW-POS @ SECTOR MOD  DUP 0= IF DROP EXIT THEN  ( off )
    FSCRATCH FW-DISK-SEC 1 _DISK-READ
    SECTOR OVER -  FW-REM @ MIN  FW-CHUNK !  ( off )
    FW-ADDR @  SWAP FSCRATCH +  FW-CHUNK @  CMOVE
    FSCRATCH FW-DISK-SEC 1 _DISK-WRITE
    FW-CHUNK @ DUP FW-ADDR +!  DUP FW-POS +!  NEGATE FW-REM +! ;

\ FW-FULL ( -- ) DMA full sectors directly from user buffer
: FW-FULL  ( -- )
    FW-REM @ SECTOR / DUP 0= IF DROP EXIT THEN
    DUP >R
    FW-ADDR @ FW-DISK-SEC ROT _DISK-WRITE
    R> SECTOR *
    DUP FW-ADDR +!
    DUP FW-POS +!
    NEGATE FW-REM +! ;

\ FW-TAIL ( -- ) read-modify-write trailing partial sector
: FW-TAIL  ( -- )
    FW-REM @ 0= IF EXIT THEN
    FSCRATCH FW-DISK-SEC 1 _DISK-READ
    FW-ADDR @  FSCRATCH  FW-REM @  CMOVE
    FSCRATCH FW-DISK-SEC 1 _DISK-WRITE ;

: FWRITE  ( addr len fdesc -- )
    FW-FD !  FW-LEN !  FW-ADDR !
    \ Bounds check: cursor + len <= max_sectors * 512
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ F.MAX SECTOR * > IF
        ."  FWRITE: out of space" CR EXIT
    THEN
    FW-LEN @ 0= IF EXIT THEN
    FW-FD @ F.CURSOR FW-POS !
    FW-LEN @ FW-REM !
    FW-HEAD  FW-FULL  FW-TAIL
    \ Advance cursor by len
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ 24 + !
    \ Update used_bytes = max(used, cursor)
    FW-FD @ F.CURSOR FW-FD @ F.USED MAX
    FW-FD @ 16 + ! ;

\ FREAD ( addr len fdesc -- actual ) read up to len bytes at cursor
\   Handles byte-unaligned cursors via FSCRATCH for partial sectors.
\   Head/tail partial sectors are read into FSCRATCH and the relevant
\   bytes copied to the user buffer.  Full middle sectors DMA directly.
\   Advances cursor by actual bytes read.
VARIABLE FR-FD
VARIABLE FR-ADDR
VARIABLE FR-LEN
VARIABLE FR-REM     \ remaining bytes to read
VARIABLE FR-POS     \ current byte position in file

\ FR-DISK-SEC ( -- sec ) absolute disk sector for current FR-POS
: FR-DISK-SEC  FR-POS @ SECTOR / FR-FD @ F.START + ;

\ FR-HEAD ( -- ) copy leading partial sector via scratch buffer
VARIABLE FR-CHUNK   \ temp: byte count for head copy
: FR-HEAD  ( -- )
    FR-POS @ SECTOR MOD  DUP 0= IF DROP EXIT THEN  ( off )
    FSCRATCH FR-DISK-SEC 1 _DISK-READ
    SECTOR OVER -  FR-REM @ MIN  FR-CHUNK !  ( off )
    FSCRATCH +  FR-ADDR @  FR-CHUNK @  CMOVE
    FR-CHUNK @ DUP FR-ADDR +!  DUP FR-POS +!  NEGATE FR-REM +! ;

\ FR-FULL ( -- ) DMA full sectors directly into user buffer
: FR-FULL  ( -- )
    FR-REM @ SECTOR / DUP 0= IF DROP EXIT THEN
    DUP >R
    FR-ADDR @ FR-DISK-SEC ROT _DISK-READ
    R> SECTOR *
    DUP FR-ADDR +!
    DUP FR-POS +!
    NEGATE FR-REM +! ;

\ FR-TAIL ( -- ) copy trailing partial sector via scratch buffer
: FR-TAIL  ( -- )
    FR-REM @ 0= IF EXIT THEN
    FSCRATCH FR-DISK-SEC 1 _DISK-READ
    FSCRATCH  FR-ADDR @  FR-REM @  CMOVE ;

: FREAD  ( addr len fdesc -- actual )
    FR-FD !  FR-LEN !  FR-ADDR !
    \ Guard: cursor already at or past file end → return 0
    \ (MIN is unsigned; without this guard the subtraction below
    \  wraps to a huge value and FREAD loops forever.)
    FR-FD @ F.CURSOR  FR-FD @ F.USED  < 0= IF 0 EXIT THEN
    \ Clamp len to available bytes (safe: cursor < used)
    FR-FD @ F.USED FR-FD @ F.CURSOR -
    FR-LEN @ MIN FR-LEN !
    FR-LEN @ 0= IF 0 EXIT THEN
    FR-FD @ F.CURSOR FR-POS !
    FR-LEN @ FR-REM !
    FR-HEAD  FR-FULL  FR-TAIL
    \ Advance cursor by actual bytes read
    FR-FD @ F.CURSOR FR-LEN @ +
    FR-FD @ 24 + !
    FR-LEN @ ;

\ F.INFO ( fdesc -- ) print file descriptor
: F.INFO  ( fdesc -- )
    ."  [file"
    DUP ."   sec=" F.START .
    DUP ."   max=" F.MAX .
    DUP ."   used=" F.USED .
    ."   cur=" F.CURSOR . ."  ]" CR ;

\ -- List all registered (legacy) files --
: FILES  ( -- )
    ."  --- Files (" FILE-COUNT @ . ."  ) ---" CR
    FILE-COUNT @ DUP IF
        0 DO
            I . ."  : "
            I CELLS FILE-TABLE + @ F.INFO
        LOOP
    ELSE DROP THEN ;

\ =====================================================================
\  §7.6  MP64FS — On-Disk Named File System
\ =====================================================================
\
\  Disk layout (15..8192 x 512-byte sectors, format marker 1):
\    Sector 0         Superblock (magic "MP64", marker, geometry)
\    Sectors 1..B     Allocation bitmap (one bit per sector)
\    Next 12 sectors  Directory (128 entries x 48 bytes)
\    Remaining        Data area
\  B = ceil(total_sectors / 4096); directory and data starts follow B.
\
\  Directory entry (48 bytes):
\    +0   name[24]       null-terminated (max 23 chars)
\    +24  start_sector[2] u16 LE
\    +26  sec_count[2]   u16 LE
\    +28  used_bytes[4]  u32 LE
\    +32  type[1]        0=free 1=raw 2=text 3=forth 4=doc 5=data
\                        6=tut 7=bundle 8=dir 9=stream 10=link
\    +33  flags[1]       bit0=readonly bit1=system bit2=encrypted bit3=append
\    +34  parent[1]      parent dir slot (0xFF=root)
\    +35  reserved[1]
\    +36  mtime[4]       u32 seconds since boot
\    +40  data_crc32[4]  u32 CRC-32
\    +44  ext1_start[2]  u16 LE second extent start
\    +46  ext1_count[2]  u16 LE second extent sector count
\
\  File descriptor layout (allocated from FD pool, freed by FCLOSE):
\    +0   start_sector   (cell)
\    +8   max_sectors     (cell)
\    +16  used_bytes      (cell)
\    +24  cursor          (cell)
\    +32  dir_slot        (cell) — index into directory cache
\    +40  ext1_start      (cell) — second extent start
\    +48  ext1_count      (cell) — second extent count
\  Pool slot has an in_use flag at fdesc - 8.

\ -- Constants --
128 CONSTANT FS-MAX-FILES
48  CONSTANT FS-ENTRY-SIZE

\ -- RAM caches (loaded from disk by FS-LOAD) --
VARIABLE FS-SUPER  SECTOR 1- ALLOT            \ 512 bytes — superblock
VARIABLE FS-BMAP   SECTOR 2 * 1- ALLOT         \ up to 8192 sector bits
VARIABLE FS-DIR    SECTOR 12 * 1- ALLOT       \ 6144 bytes — directory

VARIABLE FS-TOTAL  2048 FS-TOTAL !
VARIABLE FS-BMAP-N 1 FS-BMAP-N !
: FS-DIR-START  ( -- n ) FS-BMAP-N @ 1+ ;
: FS-DSTART     ( -- n ) FS-DIR-START 12 + ;

\ -- Current working directory (parent slot, 0xFF = root) --
VARIABLE CWD   255 CWD !

\ ── Bitmap operations ────────────────────────────────────────────────

\ BIT-MASK ( bitpos -- mask ) compute 1 << bitpos
: BIT-MASK  ( bitpos -- mask )
    DUP 0= IF DROP 1 EXIT THEN
    1 SWAP 0 DO 2* LOOP ;

\ BIT-FREE? ( sector -- flag ) true if sector is unallocated
: BIT-FREE?  ( sector -- flag )
    DUP 8 / FS-BMAP + C@
    SWAP 8 MOD BIT-MASK
    AND 0= ;

\ BIT-SET ( sector -- ) mark sector as allocated
: BIT-SET  ( sector -- )
    DUP 8 / FS-BMAP +
    DUP C@
    ROT 8 MOD BIT-MASK
    OR SWAP C! ;

\ BIT-CLR ( sector -- ) mark sector as free
: BIT-CLR  ( sector -- )
    DUP 8 / FS-BMAP +
    DUP C@
    ROT 8 MOD BIT-MASK
    INVERT AND SWAP C! ;

\ FIND-FREE ( count -- sector | -1 ) find contiguous free run
VARIABLE FF-NEED
VARIABLE FF-START
VARIABLE FF-LEN

: FIND-FREE  ( count -- sector | -1 )
    FF-NEED !
    FS-DSTART FF-START !
    0 FF-LEN !
    -1                                 \ result on stack
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF
            FF-LEN @ 0= IF I FF-START ! THEN
            1 FF-LEN +!
            FF-LEN @ FF-NEED @ >= IF
                DROP FF-START @        \ replace -1 with start
                LEAVE
            THEN
        ELSE
            0 FF-LEN !
        THEN
    LOOP ;

\ ── Directory helpers ────────────────────────────────────────────────

\ DIRENT ( n -- addr ) address of directory entry N in RAM cache
: DIRENT  ( n -- addr )  FS-ENTRY-SIZE * FS-DIR + ;

\ Directory entry field readers
: DE.SEC    ( de -- u16 )   24 + W@ ;
: DE.COUNT  ( de -- u16 )   26 + W@ ;
: DE.USED   ( de -- u32 )   28 + L@ ;
: DE.TYPE   ( de -- u8 )    32 + C@ ;
: DE.FLAGS  ( de -- u8 )    33 + C@ ;
: DE.PARENT ( de -- u8 )    34 + C@ ;
: DE.MTIME  ( de -- u32 )   36 + L@ ;
: DE.CRC    ( de -- u32 )   40 + L@ ;
: DE.EXT1-SEC   ( de -- u16 ) 44 + W@ ;
: DE.EXT1-CNT   ( de -- u16 ) 46 + W@ ;

\ FIND-FREE-SLOT ( -- slot | -1 ) first empty directory slot

: FIND-FREE-SLOT  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0= IF DROP I LEAVE THEN
    LOOP ;

\ ── Loading and syncing ──────────────────────────────────────────────

\ FS-LOAD ( -- ) read superblock + bitmap + directory from disk
: FS-LOAD  ( -- )
    0 FS-OK !
    DISK? 0= IF
        ."  No disk attached" CR EXIT
    THEN
    STORAGE-OPEN ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS !
        ."  Unable to bind storage" CR EXIT
    THEN
    MP64FS-VALID? 0= IF
        ."  Invalid MP64FS" CR EXIT
    THEN
    \ The shared BIOS validator has already checked the marker, geometry,
    \ reserved bitmap, directory entries, parents, extents, and byte bounds.
    \ Read the accepted geometry into KDOS's cache coordinates.
    FS-SUPER 0 1 _DISK-READ
    FS-SUPER 6 + L@ FS-TOTAL !
    FS-SUPER 12 + W@ FS-BMAP-N !
    \ Read the complete geometry-selected bitmap and directory.
    FS-BMAP 1 FS-BMAP-N @ _DISK-READ
    FS-DIR FS-DIR-START 12 _DISK-READ
    -1 FS-OK !
    ."  MP64FS loaded" CR ;

\ FS-SYNC ( -- ) write bitmap + directory back to disk
: FS-SYNC  ( -- )
    FS-OK @ 0= IF ."  FS not loaded" CR EXIT THEN
    FS-BMAP 1 FS-BMAP-N @ _DISK-WRITE
    FS-DIR FS-DIR-START 12 _DISK-WRITE
    _DISK-FLUSH ;

\ FS-ENSURE ( -- ) auto-load if not yet loaded
: FS-ENSURE  ( -- )
    FS-OK @ 0= IF
        DISK? IF FS-LOAD THEN
    THEN ;

\ ── FORMAT ───────────────────────────────────────────────────────────

\ FORMAT ( -- ) initialise a fresh MP64FS on the attached disk
: FORMAT  ( -- )
    0 FS-OK !
    DISK? 0= IF ."  No disk" CR EXIT THEN
    STORAGE-OPEN ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS !
        ."  Unable to bind storage" CR EXIT
    THEN
    FS-VOLUME @ VOL.SECTORS DUP 15 < OVER 8192 > OR IF
        DROP ."  Unsupported disk size" CR EXIT
    THEN
    FS-TOTAL !
    FS-TOTAL @ 4095 + 4096 / FS-BMAP-N !
    \ Build superblock in RAM
    FS-SUPER SECTOR 0 FILL
    77 FS-SUPER     C!              \ 'M'
    80 FS-SUPER 1+  C!              \ 'P'
    54 FS-SUPER 2 + C!              \ '6'
    52 FS-SUPER 3 + C!              \ '4'
    1              FS-SUPER 4  + W! \ format marker
    FS-TOTAL @     FS-SUPER 6  + L! \ total sectors (u32)
    1               FS-SUPER 10 + W!
    FS-BMAP-N @     FS-SUPER 12 + W!
    FS-DIR-START    FS-SUPER 14 + W!
    12              FS-SUPER 16 + W!
    FS-DSTART       FS-SUPER 18 + W!
    128  FS-SUPER 20 + C!           \ max files
    48   FS-SUPER 21 + C!           \ entry size
    FS-SUPER 0 1 _DISK-WRITE
    \ Initialise bitmap — mark every sector below FS-DSTART as metadata
    FS-BMAP FS-BMAP-N @ SECTOR * 0 FILL
    FS-DSTART 0 DO I BIT-SET LOOP
    FS-BMAP 1 FS-BMAP-N @ _DISK-WRITE
    \ Zero directory
    FS-DIR SECTOR 12 * 0 FILL
    FS-DIR FS-DIR-START 12 _DISK-WRITE
    _DISK-FLUSH
    -1 FS-OK !
    255 CWD !
    ."  MP64FS formatted" CR ;

\ ── .FTYPE — print file type name ───────────────────────────────────

: .FTYPE  ( type -- )
    DUP 0 = IF DROP ."  free"   EXIT THEN
    DUP 1 = IF DROP ."  raw"    EXIT THEN
    DUP 2 = IF DROP ."  text"   EXIT THEN
    DUP 3 = IF DROP ."  forth"  EXIT THEN
    DUP 4 = IF DROP ."  doc"    EXIT THEN
    DUP 5 = IF DROP ."  data"   EXIT THEN
    DUP 6 = IF DROP ."  tut"    EXIT THEN
    DUP 7 = IF DROP ."  bdl"    EXIT THEN
    DUP 8 = IF DROP ."  dir"    EXIT THEN
    DUP 9 = IF DROP ."  stream" EXIT THEN
    DUP 10 = IF DROP ."  link"  EXIT THEN
    ."  ?" . ;

\ ── DIR — list files ─────────────────────────────────────────────────

: DIR  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  --- Directory ---" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                1+
                ."   " I DIRENT .ZSTR
                I DIRENT DE.TYPE 8 = IF ."  /"  THEN
                ."    " I DIRENT DE.USED . ."  B"
                ."    " I DIRENT DE.TYPE .FTYPE
                CR
            THEN
        THEN
    LOOP
    DUP . ."  file(s), "
    0  FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes free)" CR
    DROP ;

\ CATALOG ( -- ) detailed directory listing
: CATALOG  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Name                     Bytes     Secs  Type  Flg" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                1+
                ."   " I DIRENT .ZSTR
                ."   " I DIRENT DE.USED .
                ."   " I DIRENT DE.COUNT .
                ."   " I DIRENT DE.TYPE .
                ."   " I DIRENT DE.FLAGS .
                CR
            THEN
        THEN
    LOOP
    ."  (" . ."  files, "
    0 FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    . ."  free sectors)" CR ;

\ ── FIND-BY-NAME — shared directory lookup ───────────────────────────
\ Searches directory for an entry whose first 24 bytes match NAMEBUF
\ and whose parent matches CWD.
\ Returns slot index or -1 if not found.  Caller must call PARSE-NAME first.

: FIND-BY-NAME  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT CWD @ = IF
                I DIRENT NAMEBUF 24 SAMESTR? IF
                    DROP I LEAVE
                THEN
            THEN
        THEN
    LOOP ;

\ ── TICKS@ — epoch seconds for mtime ─────────────────────────────────
: TICKS@  ( -- u32 )  EPOCH@ 1000 / ;

\ ── MKFILE — create a new file ───────────────────────────────────────

VARIABLE MK-NSEC
VARIABLE MK-TYPE
VARIABLE MK-SLOT
VARIABLE MK-START

: MKFILE  ( nsectors type "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR 2DROP EXIT THEN
    MK-TYPE !  MK-NSEC !
    PARSE-NAME
    \ Check for duplicate name
    FIND-BY-NAME -1 <> IF
        ."  File exists: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Find empty directory slot
    FIND-FREE-SLOT MK-SLOT !
    MK-SLOT @ -1 = IF ."  Directory full" CR EXIT THEN
    \ Find contiguous free sectors
    MK-NSEC @ FIND-FREE MK-START !
    MK-START @ -1 = IF
        ."  No space: need " MK-NSEC @ .
        ."  sectors, "
        0 FS-TOTAL @ FS-DSTART DO
            I BIT-FREE? IF 1+ THEN
        LOOP
        . ."  free" CR EXIT
    THEN
    \ Allocate sectors in bitmap
    MK-NSEC @ 0 DO
        MK-START @ I + BIT-SET
    LOOP
    \ Build directory entry
    MK-SLOT @ DIRENT                 ( de )
    DUP FS-ENTRY-SIZE 0 FILL        \ zero slot
    DUP NAMEBUF SWAP 24 CMOVE       \ copy name (24 bytes)
    DUP MK-START @ SWAP 24 + W!     \ start sector
    DUP MK-NSEC  @ SWAP 26 + W!     \ sector count
    DUP 0          SWAP 28 + L!     \ used_bytes = 0
    DUP MK-TYPE  @ SWAP 32 + C!     \ type
    DUP CWD @     SWAP 34 + C!     \ parent = CWD
    DUP TICKS@    SWAP 36 + L!     \ mtime = current time
    DROP
    FS-SYNC
    ."  Created: " NAMEBUF .ZSTR
    ."  (" MK-NSEC @ . ."  sectors at " MK-START @ . ."  )" CR ;

\ ── RMFILE — delete a file ───────────────────────────────────────────

VARIABLE RM-SLOT

: RMFILE  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME RM-SLOT !
    RM-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Free bitmap sectors (primary extent)
    RM-SLOT @ DIRENT DE.COUNT
    RM-SLOT @ DIRENT DE.SEC
    SWAP 0 DO                        ( start )
        DUP I + BIT-CLR
    LOOP DROP
    \ Free second extent if present
    RM-SLOT @ DIRENT DE.EXT1-CNT DUP 0<> IF
        RM-SLOT @ DIRENT DE.EXT1-SEC
        SWAP 0 DO
            DUP I + BIT-CLR
        LOOP DROP
    ELSE DROP THEN
    \ Clear directory entry
    RM-SLOT @ DIRENT FS-ENTRY-SIZE 0 FILL
    FS-SYNC
    ."  Deleted: " NAMEBUF .ZSTR CR ;

\ ── RENAME — rename a file ───────────────────────────────────────────

VARIABLE RN-SLOT

: RENAME  ( "oldname" "newname" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    \ Look up old name
    PARSE-NAME
    FIND-BY-NAME RN-SLOT !
    RN-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Parse new name
    PARSE-NAME
    \ Check new name doesn't already exist
    FIND-BY-NAME -1 <> IF
        ."  Name taken: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Overwrite name in directory entry
    RN-SLOT @ DIRENT 24 0 FILL       \ zero old name
    NAMEBUF RN-SLOT @ DIRENT 24 CMOVE
    FS-SYNC
    ."  Renamed to: " NAMEBUF .ZSTR CR ;

\ ── CAT — print file contents to terminal ────────────────────────────

VARIABLE CAT-SLOT

: CAT  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME CAT-SLOT !
    CAT-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    CAT-SLOT @ DIRENT DE.USED DUP 0= IF
        DROP ."  (empty file)" CR EXIT
    THEN                                 ( used_bytes )
    \ Read file sectors into HERE (temporary)
    HERE CAT-SLOT @ DIRENT DE.SEC
    CAT-SLOT @ DIRENT DE.COUNT _DISK-READ
    \ Print used_bytes characters from HERE
    HERE SWAP                            ( addr count )
    0 DO
        DUP I + C@ DUP 10 = IF
            DROP CR
        ELSE
            EMIT
        THEN
    LOOP DROP ;

\ ── FS-LARGEST-FREE — largest contiguous free run in bitmap ──────────

VARIABLE LF-BEST
VARIABLE LF-RUN

: FS-LARGEST-FREE  ( -- n )
    0 LF-BEST !  0 LF-RUN !
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF
            1 LF-RUN +!
            LF-RUN @ LF-BEST @ > IF LF-RUN @ LF-BEST ! THEN
        ELSE
            0 LF-RUN !
        THEN
    LOOP
    LF-BEST @ ;

\ ── FS-FREE — report free disk space ────────────────────────────────

: FS-FREE  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    0   \ free sector count
    FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes)" CR
    ."  Largest contiguous: " FS-LARGEST-FREE . ."  sectors" CR
    \ Count used files
    0  FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF 1+ THEN
    LOOP
    . ."  files, " FS-MAX-FILES . ."  max" CR ;

\ ── SAVE-BUFFER — save buffer data to a named file ──────────────────

VARIABLE SB-SLOT
VARIABLE SB-DESC

: SAVE-BUFFER  ( buf "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF DROP ."  No filesystem" CR EXIT THEN
    SB-DESC !
    PARSE-NAME
    FIND-BY-NAME SB-SLOT !
    SB-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR
        ."   (create with MKFILE first)" CR EXIT
    THEN
    \ Write buffer data into file's sectors
    SB-DESC @ B.DATA SB-SLOT @ DIRENT DE.SEC
    SB-SLOT @ DIRENT DE.COUNT _DISK-WRITE
    \ Update used_bytes in directory
    SB-DESC @ B.LEN
    SB-SLOT @ DIRENT 28 + L!
    FS-SYNC
    ."  Saved " SB-DESC @ B.LEN . ."  bytes to " NAMEBUF .ZSTR CR ;

\ ── LOAD-BUFFER — load file data into a buffer ──────────────────────

VARIABLE LB-SLOT
VARIABLE LB-DESC

: LOAD-BUFFER  ( buf "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF DROP ."  No filesystem" CR EXIT THEN
    LB-DESC !
    PARSE-NAME
    FIND-BY-NAME LB-SLOT !
    LB-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Read file data into buffer
    LB-DESC @ B.DATA LB-SLOT @ DIRENT DE.SEC
    LB-SLOT @ DIRENT DE.COUNT _DISK-READ
    ."  Loaded " LB-SLOT @ DIRENT 28 + L@ . ."  bytes from " NAMEBUF .ZSTR CR ;

\ ── FD Pool — fixed pool of reusable file descriptors ────────────────
\
\  16 slots × 72 bytes = 1,152 bytes, allocated once at boot.
\  Slot layout (9 cells):
\    +0  in_use   (cell)  0=free, -1=in-use
\    +8  start_sec (cell)  — fdesc offset +0
\    +16 max_sec   (cell)  — fdesc offset +8
\    +24 used_bytes (cell) — fdesc offset +16
\    +32 cursor    (cell)  — fdesc offset +24
\    +40 dir_slot  (cell)  — fdesc offset +32
\    +48 ext1_start(cell)  — fdesc offset +40
\    +56 ext1_count(cell)  — fdesc offset +48
\    +64 reserved  (cell)  — padding
\  The returned fdesc points to +8, so existing field accessors
\  (F.START +0, F.MAX +8, etc.) remain unchanged.

16 CONSTANT FD-MAX
72 CONSTANT FD-SLOT-SZ
CREATE FD-POOL  FD-MAX FD-SLOT-SZ * ALLOT
FD-POOL FD-MAX FD-SLOT-SZ * 0 FILL          \ zero the pool

\ FD-SLOT ( n -- addr )  address of pool slot n  (0..15)
: FD-SLOT  ( n -- addr )  FD-SLOT-SZ * FD-POOL + ;

\ FD-ALLOC ( -- fdesc | 0 )  allocate a pool slot, return fdesc or 0
: FD-ALLOC  ( -- fdesc | 0 )
    FD-MAX 0 DO
        I FD-SLOT @ 0= IF          \ in_use == 0?
            -1 I FD-SLOT !          \ mark in-use
            I FD-SLOT 8 +           \ fdesc = slot + 8
            UNLOOP EXIT
        THEN
    LOOP
    0 ;                              \ pool exhausted

\ FCLOSE ( fdesc -- )  release FD back to pool
\   Initially a simple free; redefined after FFLUSH to auto-flush.
: (FCLOSE-NOFS)  ( fdesc -- )
    DUP 0= IF DROP EXIT THEN
    8 -                              \ back to slot header
    0 SWAP ! ;                       \ clear in_use flag

DEFER FCLOSE
' (FCLOSE-NOFS) IS FCLOSE

\ FD-FILL ( fdesc slot -- )  populate fdesc fields from dir slot
: FD-FILL  ( fdesc slot -- )
    >R
    R@ DIRENT DE.SEC       OVER !          \ +0 start_sector
    R@ DIRENT DE.COUNT     OVER 8 + !     \ +8 max_sectors
    R@ DIRENT DE.USED      OVER 16 + !    \ +16 used_bytes
    0                      OVER 24 + !    \ +24 cursor = 0
    R@                     OVER 32 + !    \ +32 dir_slot
    R@ DIRENT DE.EXT1-SEC  OVER 40 + !    \ +40 ext1_start
    R> DIRENT DE.EXT1-CNT  SWAP 48 + ! ;  \ +48 ext1_count

\ ── OPEN — open a file by name ───────────────────────────────────────

VARIABLE OP-SLOT

: (OPEN)  ( "name" -- fdesc | 0 )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR 0 EXIT THEN
    PARSE-NAME
    FIND-BY-NAME OP-SLOT !
    OP-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR 0 EXIT
    THEN
    FD-ALLOC DUP 0= IF
        ."  No free FD slots" CR EXIT
    THEN
    DUP OP-SLOT @ FD-FILL ;

DEFER OPEN
' (OPEN) IS OPEN

\ F.SLOT ( fdesc -- n ) directory slot index (for OPEN'd files)
: F.SLOT  ( fdesc -- n )  32 + @ ;

\ FFLUSH ( fdesc -- ) write metadata back to directory on disk
: FFLUSH  ( fdesc -- )
    FS-OK @ 0= IF DROP ."  FS not loaded" CR EXIT THEN
    DUP F.USED
    OVER F.SLOT DIRENT 28 + L!      \ update used_bytes in dir cache
    DROP
    FS-SYNC ;

\ Now that FFLUSH exists, upgrade FCLOSE to auto-flush.
: (FCLOSE)  ( fdesc -- )
    DUP 0= IF DROP EXIT THEN
    FS-OK @ IF DUP FFLUSH THEN      \ persist used_bytes before release
    8 -                              \ back to slot header
    0 SWAP ! ;                       \ clear in_use flag
' (FCLOSE) IS FCLOSE

\ ── LOAD — load and execute a Forth source file ─────────────────────
\ LOAD ( "filename" -- ) open a file by name, read it, EVALUATE it
\   Reads the entire file into a reclaimable loader allocation, then walks
\   through it line by line, EVALUATEing each line.

VARIABLE LD-BUF
VARIABLE LD-SZ
VARIABLE LD-CUR
VARIABLE LD-LEN

\ Nesting support: save/restore walker state for nested LOAD/REQUIRE.
\ Includes CWD so relative-path loads restore the working directory.
\ Frame = 5 vars × 8 bytes = 40 bytes.  16 levels → 640 bytes.
40 CONSTANT _LD-FRAME
16 CONSTANT _LD-MAXLVL
CREATE _LD-STK _LD-FRAME _LD-MAXLVL * ALLOT
VARIABLE _LD-SP
0 _LD-SP !

: _LD-SAVE  ( -- )
    _LD-SP @ _LD-FRAME _LD-MAXLVL * >= ABORT" REQUIRE nested too deep"
    LD-BUF @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-SZ  @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-CUR @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-LEN @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    CWD  @ _LD-SP @ _LD-STK + !  8 _LD-SP +! ;

: _LD-RESTORE  ( -- )
    _LD-SP @ 0= ABORT" REQUIRE nesting underflow"
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ CWD  !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-LEN !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-CUR !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-SZ  !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-BUF ! ;

VARIABLE _LD-RUN-SEC
VARIABLE _LD-RUN-CNT
VARIABLE _LD-RUN-ADDR

\ _LD-READ-RUN ( sector count addr -- next-addr )
\ The BIOS checked layer owns hardware-sized splitting and completion.
: _LD-READ-RUN  ( sector count addr -- next-addr )
    _LD-RUN-ADDR ! _LD-RUN-CNT ! _LD-RUN-SEC !
    _LD-RUN-ADDR @ _LD-RUN-SEC @ _LD-RUN-CNT @ _DISK-READ
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
\ unfinished.  Callers own dictionary rollback (HERE/LATEST); after that
\ rollback they must call EVALUATOR-RESET to clear compiler bookkeeping.

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

\ _LD-WALK ( -- ) Walk file buffer line-by-line, EVALUATEing each.
\   Uses LD-BUF / LD-SZ / LD-CUR / LD-LEN.  The data stack is kept
\   clean across EVALUATE calls so compile-time control-flow items
\   (DO..LOOP, IF..THEN, BEGIN..REPEAT etc.) are undisturbed.
: _LD-WALK  ( -- )
    LD-BUF @ LD-CUR !
    BEGIN LD-SZ @ 0> WHILE
        \ Find length of current line (up to newline or end)
        LD-SZ @                          ( rem )
        0                                ( rem i )
        BEGIN
            DUP 2 PICK < IF
                LD-CUR @ OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
            ELSE TRUE THEN
        UNTIL                            ( rem linelen )
        NIP LD-LEN !
        \ EVALUATE if non-empty
        LD-LEN @ 0> IF
            LD-CUR @ LD-LEN @ EVALUATE
        THEN
        \ Advance past line + newline
        LD-LEN @ 1+
        DUP NEGATE LD-SZ +!
        LD-CUR +!
    REPEAT ;

: _LD-RELEASE  ( -- )
    LD-BUF @ FREE
    _LD-RESTORE ;

\ Run a source walk with the loader allocation and nesting frame owned by
\ this call.  Cleanup precedes rethrow so a bad module cannot poison the next
\ LOAD/REQUIRE or strand its sector-rounded source allocation.
: _LD-WALK-GUARDED  ( -- )
    ['] _LD-WALK CATCH
    DUP IF
        >R _LD-RELEASE R> THROW
    THEN
    DROP _LD-RELEASE ;

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
    LD-BUF !
    _LD-READ-SLOT
    _LD-WALK-GUARDED ;

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
        >R _LD-RELEASE R> THROW
    THEN
    DROP
    SYS-EXIT
    _APP-MPU-OFF
    _LD-RELEASE ;

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

\ =====================================================================
\  §7.6.1  Filesystem Encryption
\ =====================================================================
\  Optional at-rest encryption for MP64FS files using AES-256-GCM.
\  Requires FS-KEY to be set before use.  Operates on OPEN'd files.
\
\  On-disk layout of an encrypted file:
\    Sectors contain: ciphertext (padded to 16B) || 16-byte GCM tag
\    Directory used_bytes = original plaintext length (unchanged)
\    Directory flags bit 2 = encrypted
\    IV = 12 bytes derived from directory slot number
\
\  FS-KEY!     ( addr -- )          set 32-byte filesystem encryption key
\  ENCRYPTED?  ( fdesc -- flag )    true if file has encrypted flag
\  FENCRYPT    ( fdesc -- ior )     encrypt open file in-place (0=ok)
\  FDECRYPT    ( fdesc -- flag )    decrypt open file (0=ok, -1=auth-fail)

CREATE FS-KEY 32 ALLOT            \ system-level encryption key
CREATE FS-IV  12 ALLOT            \ derived IV for current operation

4 CONSTANT F-ENC-FLAG             \ flags bit 2 = encrypted

VARIABLE _FE-DESC                 \ current file descriptor
VARIABLE _FE-USED                 \ original plaintext byte count
VARIABLE _FE-PAD                  \ padded length (multiple of 16)
VARIABLE _FE-SECS                 \ number of sectors for I/O
VARIABLE _FE-BUF1                 \ buffer 1
VARIABLE _FE-BUF2                 \ buffer 2

\ FS-KEY! ( addr -- )  Copy 32-byte key into FS-KEY.
: FS-KEY!  FS-KEY 32 CMOVE ;

\ _FE-MKIV ( fdesc -- )  Derive 12-byte IV from file's directory slot.
: _FE-MKIV
    FS-IV 12 0 FILL
    F.SLOT FS-IV !
;

\ ENCRYPTED? ( fdesc -- flag )  True if file has encrypted flag set.
: ENCRYPTED?
    F.SLOT DIRENT 33 + C@
    F-ENC-FLAG AND 0<>
;

\ _FE-SET-ENC ( fdesc -- )  Set encrypted flag in FS-DIR cache.
: _FE-SET-ENC
    F.SLOT DIRENT 33 +
    DUP C@ F-ENC-FLAG OR SWAP C!
;

\ _FE-CLR-ENC ( fdesc -- )  Clear encrypted flag in FS-DIR cache.
: _FE-CLR-ENC
    F.SLOT DIRENT 33 +
    DUP C@ F-ENC-FLAG INVERT AND SWAP C!
;

\ FENCRYPT ( fdesc -- ior )
\   Encrypt an open file's data in-place on disk.
\   Uses FS-KEY as the encryption key.
\   Returns 0 on success, -1 on error.
: FENCRYPT
    DUP ENCRYPTED? IF DROP 0 EXIT THEN    \ already encrypted — no-op
    _FE-DESC !                                      \ consume fdesc
    _FE-DESC @ FSIZE DUP _FE-USED !
    0= IF 0 EXIT THEN                              \ empty — skip
    \ Padded length = round up to 16
    _FE-USED @ 15 + -16 AND _FE-PAD !
    \ Check: padded + 16 (tag) must fit in allocated sectors
    _FE-PAD @ 16 +
    _FE-DESC @ F.MAX 512 * > IF
        ."  FENCRYPT: insufficient space" CR -1 EXIT
    THEN
    \ Sectors for ciphertext + tag
    _FE-PAD @ 16 + 511 + 512 / _FE-SECS !
    \ Allocate sector-aligned buffers (DMA operates on full sectors)
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF _FE-BUF1 @ DMA-FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero both buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read file data from disk into buf1
    _FE-BUF1 @ _FE-DESC @ F.START
    _FE-USED @ 511 + 512 / _DISK-READ
    \ Derive IV from directory slot
    _FE-DESC @ _FE-MKIV
    \ Encrypt: ( key iv src dst len -- tag-addr )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @ ENCRYPT
    \ Copy 16-byte tag after ciphertext in buf2
    _FE-BUF2 @ _FE-PAD @ + 16 CMOVE
    \ DMA-write ciphertext + tag back to disk
    _FE-BUF2 @ _FE-DESC @ F.START _FE-SECS @ _DISK-WRITE
    \ Set encrypted flag and sync directory
    _FE-DESC @ _FE-SET-ENC
    _FE-USED @ _FE-DESC @ F.SLOT DIRENT 28 + L!
    FS-SYNC
    \ Free buffers
    _FE-BUF1 @ DMA-FREE
    _FE-BUF2 @ DMA-FREE
    0
;

\ FDECRYPT ( fdesc -- flag )
\   Decrypt an encrypted file in-place on disk.
\   Returns 0 if authentication passed, -1 if failed or not encrypted.
: FDECRYPT
    DUP ENCRYPTED? 0= IF DROP 0 EXIT THEN  \ not encrypted — no-op
    _FE-DESC !                                      \ consume fdesc
    _FE-DESC @ FSIZE DUP _FE-USED !
    0= IF 0 EXIT THEN
    \ Padded length
    _FE-USED @ 15 + -16 AND _FE-PAD !
    _FE-PAD @ 16 + 511 + 512 / _FE-SECS !
    \ Allocate sector-aligned buffers (DMA operates on full sectors)
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF _FE-BUF1 @ DMA-FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read ciphertext + tag from disk
    _FE-BUF1 @ _FE-DESC @ F.START _FE-SECS @ _DISK-READ
    \ Derive IV
    _FE-DESC @ _FE-MKIV
    \ Decrypt: ( key iv src dst len tag -- flag )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @
    _FE-BUF1 @ _FE-PAD @ +       \ tag is right after ciphertext
    DECRYPT
    \ flag on stack: 0 = auth OK, -1 = auth fail
    DUP 0= IF
        \ Write plaintext back to disk
        _FE-BUF2 @ _FE-DESC @ F.START
        _FE-USED @ 511 + 512 / _DISK-WRITE
        \ Clear encrypted flag, sync
        _FE-DESC @ _FE-CLR-ENC
        _FE-USED @ _FE-DESC @ F.SLOT DIRENT 28 + L!
        FS-SYNC
    THEN
    \ Free buffers
    _FE-BUF1 @ DMA-FREE
    _FE-BUF2 @ DMA-FREE
;

\ =====================================================================
\  §7.6.2  Subdirectory Navigation
\ =====================================================================
\  Parent-byte subdirectory model — each directory entry has a 1-byte
\  parent field (slot index 0-127, or 255 for root).

\ PWD ( -- ) print current working directory path
CREATE _PWD-STK 64 ALLOT
: PWD  ( -- )
    CWD @ 255 = IF ."  /" CR EXIT THEN
    \ Walk parent chain, collect up to 8 levels
    0  CWD @                             ( depth slot )
    BEGIN DUP 255 <> WHILE
        SWAP DUP 8 < IF
            2DUP CELLS _PWD-STK + !      \ save slot
            1+
        THEN SWAP
        DIRENT DE.PARENT
    REPEAT DROP                          ( depth )
    ."  /"
    DUP 0 DO
        DUP 1- I - CELLS _PWD-STK + @   ( slot )
        DIRENT .ZSTR
        47 EMIT                          \ '/'
    LOOP DROP CR ;

\ CD ( "name" -- ) change current directory
: CD  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    \ Handle ".." — go to parent of CWD
    NAMEBUF C@ 46 = NAMEBUF 1+ C@ 46 = AND NAMEBUF 2 + C@ 0= AND IF
        CWD @ 255 = IF EXIT THEN        \ already at root
        CWD @ DIRENT DE.PARENT CWD !
        EXIT
    THEN
    \ Handle "/" — go to root
    NAMEBUF C@ 47 = NAMEBUF 1+ C@ 0= AND IF
        255 CWD ! EXIT
    THEN
    \ Look up directory entry in CWD
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DUP DIRENT DE.TYPE 8 <> IF
        DROP ."  Not a directory: " NAMEBUF .ZSTR CR EXIT
    THEN
    CWD ! ;

\ MKDIR ( "name" -- ) create a subdirectory entry
: MKDIR  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME -1 <> IF
        ."  Already exists: " NAMEBUF .ZSTR CR EXIT
    THEN
    FIND-FREE-SLOT DUP -1 = IF
        DROP ."  Directory full" CR EXIT
    THEN                                 ( slot )
    DIRENT                               ( de )
    DUP FS-ENTRY-SIZE 0 FILL
    DUP NAMEBUF SWAP 24 CMOVE           \ name
    DUP 8 SWAP 32 + C!                  \ type = FTYPE_DIR
    DUP CWD @ SWAP 34 + C!              \ parent = CWD
    TICKS@ SWAP 36 + L!                 \ mtime
    FS-SYNC
    ."  Created dir: " NAMEBUF .ZSTR CR ;

\ RMDIR ( "name" -- ) remove an empty subdirectory
: RMDIR  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN                                 ( slot )
    DUP DIRENT DE.TYPE 8 <> IF
        DROP ."  Not a directory" CR EXIT
    THEN
    \ Check directory is empty (no children with this parent)
    DUP
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.PARENT OVER = IF
                DROP ."  Directory not empty" CR
                UNLOOP EXIT
            THEN
        THEN
    LOOP DROP
    \ Clear entry
    DIRENT FS-ENTRY-SIZE 0 FILL
    FS-SYNC
    ."  Removed dir: " NAMEBUF .ZSTR CR ;

\ =====================================================================
\  §7.7  Documentation Browser
\ =====================================================================
\
\  Built-in documentation and interactive tutorials stored as MP64FS
\  files.  DOC pages through a documentation file; TUTORIAL walks
\  through a lesson; DESCRIBE shows a topic by name.
\  TOPICS / LESSONS list available content.
\
\  File types:
\    type 4 (doc)      — documentation files
\    type 6 (tutorial) — interactive tutorials

\ -- Constants --
4 CONSTANT FTYPE-DOC
6 CONSTANT FTYPE-TUT

\ -- Read buffer for paging (one sector at a time) --
CREATE DOC-BUF SECTOR ALLOT
VARIABLE DOC-LINES              \ newline counter for pagination
20 CONSTANT PAGE-LINES          \ lines per page before pause

\ .DOC-CHUNK ( addr len -- )  print bytes, pausing every PAGE-LINES
: .DOC-CHUNK  ( addr len -- )
    OVER + SWAP DO
        I C@ DUP 10 = IF
            DROP CR
            1 DOC-LINES +!
            DOC-LINES @ PAGE-LINES >= IF
                DIM ."  --- more ---" RESET-COLOR
                KEY DROP CR
                DOC-LINES OFF
            THEN
        ELSE
            EMIT
        THEN
    LOOP ;

\ SHOW-FILE ( fdesc -- )  read and display entire file page by page
: SHOW-FILE  ( fdesc -- )
    DOC-LINES OFF
    BEGIN
        DOC-BUF SECTOR ROT        ( buf len fd )
        DUP >R FREAD              ( actual  R: fd )
        DUP 0> WHILE
        DOC-BUF SWAP .DOC-CHUNK
        R>
    REPEAT
    DROP R> DROP ;

\ TOPICS ( -- )  list available documentation files (type=4)
: TOPICS  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Available topics:" CR
    0                                        \ count
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                1+
                ."    " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."    (none)" CR THEN
    ."  (" . ."  topics)" CR ;

\ LESSONS ( -- )  list available tutorials (type=6)
: LESSONS  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Available lessons:" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-TUT = IF
                1+
                ."    " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."    (none)" CR THEN
    ."  (" . ."  lessons)" CR ;

\ DOC ( "name" -- )  page through a documentation file
: DOC  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

\ TUTORIAL ( "name" -- )  walk through a tutorial file
: TUTORIAL  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

\ OPEN-BY-SLOT ( slot -- fdesc | 0 )  open a file by directory slot
\   Like OPEN but takes a slot index instead of parsing a name.
\   Uses the FD pool; caller should FCLOSE when done.
: OPEN-BY-SLOT  ( slot -- fdesc | 0 )
    DUP DIRENT C@ 0= IF DROP 0 EXIT THEN
    FD-ALLOC DUP 0= IF
        ."  No free FD slots" CR NIP EXIT
    THEN
    DUP ROT FD-FILL ;

\ DESCRIBE ( "word" -- )  look up a word in the documentation
\   Tries to open a doc file matching the name.  If no exact match,
\   suggests using TOPICS.
: DESCRIBE  ( "word" -- )
    PARSE-NAME PN-LEN @ 0= IF ."  Usage: DESCRIBE <word>" CR EXIT THEN
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    \ Search for a doc file whose name matches NAMEBUF
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                I DIRENT NAMEBUF 24 SAMESTR? IF
                    DROP I LEAVE
                THEN
            THEN
        THEN
    LOOP
    DUP -1 = IF
        DROP
        ."  No doc for: " NAMEBUF .ZSTR CR
        ."  Use TOPICS to list available documentation."  CR
        EXIT
    THEN
    OPEN-BY-SLOT DUP 0= IF EXIT THEN
    DUP >R CR SHOW-FILE CR R> FCLOSE ;

\ =====================================================================
\  §7.8  Dictionary Search — WORDS-LIKE, APROPOS
\ =====================================================================
\
\  Walk the dictionary linked list using LATEST to find words whose
\  names contain a given substring (case-insensitive).
\
\  Dictionary entry layout (set by BIOS):
\    [link:8][flags+len:1][name:N]...code
\  LATEST pushes address of most-recent entry; @ follows links.

\ ENTRY>LINK ( entry -- next )  follow dictionary link field
: ENTRY>LINK  ( entry -- next )  @ ;

\ ENTRY>NAME ( entry -- addr len )  extract name from dictionary entry
: ENTRY>NAME  ( entry -- addr len )
    DUP 8 + C@  127 AND     ( entry namelen )
    SWAP 9 +  SWAP ;        ( nameaddr namelen )

\ ICONTAINS? ( pa pl sa sl -- flag )
\   Case-insensitive substring search.
\   Returns true if the pattern (pa,pl) appears anywhere in string (sa,sl).
VARIABLE IC-PA
VARIABLE IC-PL
VARIABLE IC-SA
VARIABLE IC-SL

: ICONTAINS?  ( pa pl sa sl -- flag )
    IC-SL !  IC-SA !  IC-PL !  IC-PA !
    IC-PL @ 0= IF  -1 EXIT  THEN            \ empty pattern matches all
    IC-SL @ IC-PL @ < IF  0 EXIT  THEN      \ pattern longer than string
    IC-SL @ IC-PL @ - 1+  0 DO              \ I = start position in string
        TRUE                                  \ assume match
        IC-PL @ 0 DO                          \ I = pat offset, J = start pos
            IC-SA @ J + I + C@ UCHAR
            IC-PA @ I + C@ UCHAR
            <> IF  DROP FALSE LEAVE  THEN
        LOOP
        IF  UNLOOP -1 EXIT  THEN             \ found match
    LOOP
    0 ;

\ -- WORDS-LIKE --
VARIABLE WL-CNT
VARIABLE WL-ENT
VARIABLE WL-PA
VARIABLE WL-PL

: WORDS-LIKE  ( "pattern" -- )
    BL WORD DUP C@ DUP 0= IF
        2DROP ."  Usage: WORDS-LIKE <pattern>" CR EXIT
    THEN                              ( waddr len )
    SWAP 1+ SWAP                      ( pataddr patlen )
    WL-PL !  WL-PA !
    0 WL-CNT !
    LATEST WL-ENT !
    BEGIN  WL-ENT @ WHILE
        WL-ENT @ ENTRY>NAME          ( na nl )
        WL-PA @ WL-PL @ 2OVER        ( na nl pa pl na nl )
        ICONTAINS? IF
            TYPE SPACE
            1 WL-CNT +!
        ELSE
            2DROP
        THEN
        WL-ENT @ ENTRY>LINK WL-ENT !
    REPEAT
    CR ."  (" WL-CNT @ . ."  found)" CR ;

\ APROPOS ( "pattern" -- )  alias for WORDS-LIKE
: APROPOS  ( "pattern" -- )  WORDS-LIKE ;

\ .RECENT ( n -- )  show the last n words defined in the dictionary
: .RECENT  ( n -- )
    CR ."  Recent words:" CR
    LATEST
    BEGIN  OVER 0> OVER AND WHILE
        DUP ENTRY>NAME TYPE SPACE
        ENTRY>LINK
        SWAP 1- SWAP
    REPEAT
    DROP DROP CR ;

\ =====================================================================
\  §8  Scheduler & Tasks
\ =====================================================================
\
\  Cooperative multitasking with timer-assisted preemption.
\
\  Each TASK has its own data stack area (256 bytes) and a descriptor.
\  The scheduler round-robins among READY tasks, respecting priority.
\
\  Timer preemption: a timer ISR sets a flag (PREEMPT-FLAG) that
\  cooperative yield points check.  This avoids the complexity of
\  saving/restoring Forth stacks inside an ISR.
\
\  BIOS words used: TIMER! TIMER-CTRL! TIMER-ACK EI! DI! ISR! CYCLES
\
\  Task states:
\    0 = FREE     (slot available)
\    1 = READY    (runnable, waiting for CPU)
\    2 = RUNNING  (currently executing)
\    3 = BLOCKED  (waiting for event)
\    4 = DONE     (finished, awaiting cleanup)

\ -- Constants --
0 CONSTANT T.FREE
1 CONSTANT T.READY
2 CONSTANT T.RUNNING
3 CONSTANT T.BLOCKED
4 CONSTANT T.DONE

\ -- Task descriptor: 6 cells = 48 bytes --
\    +0  status
\    +8  priority      (0=highest, 255=lowest)
\    +16 xt            (execution token — the task body)
\    +24 dsp_save      (saved data stack pointer)
\    +32 rsp_save      (saved return stack pointer)
\    +40 name_addr     (0 or pointer to name string)

\ -- Registry (up to 8 tasks) --
VARIABLE TASK-COUNT
0 TASK-COUNT !
VARIABLE TASK-TABLE  7 CELLS ALLOT

\ -- Scheduler state --
VARIABLE CURRENT-TASK    \ pointer to currently running task descriptor
0 CURRENT-TASK !
VARIABLE SCHED-RUNNING   \ 1 if scheduler is active
0 SCHED-RUNNING !
VARIABLE PREEMPT-FLAG    \ set by timer ISR, checked by YIELD?
0 PREEMPT-FLAG !
VARIABLE TIME-SLICE      \ compare-match value for preemption timer
50000 TIME-SLICE !        \ ~50k cycles per slice

\ -- Per-task stack area: 256 bytes each, up to 8 tasks = 2048 bytes --
VARIABLE TASK-STACKS  2047 ALLOT

\ -- Field accessors --
: T.STATUS  ( tdesc -- n )     @ ;
: T.PRIORITY ( tdesc -- n )    8 + @ ;
: T.XT      ( tdesc -- xt )   16 + @ ;
: T.DSP     ( tdesc -- n )    24 + @ ;
: T.RSP     ( tdesc -- n )    32 + @ ;
: T.NAME    ( tdesc -- addr ) 40 + @ ;

\ T.STATUS! ( n tdesc -- )
: T.STATUS! ( n tdesc -- ) ! ;
\ T.DSP! ( n tdesc -- )
: T.DSP! ( n tdesc -- ) 24 + ! ;
\ T.RSP! ( n tdesc -- )
: T.RSP! ( n tdesc -- ) 32 + ! ;

\ -- TASK ( xt priority "name" -- ) create and register a task --
VARIABLE TDESC-TEMP
: TASK  ( xt priority "name" -- )
    HERE TDESC-TEMP !
    T.READY ,                 \ +0  status = READY
    ,                         \ +8  priority
    ,                         \ +16 xt
    \ Allocate private data stack: 256 bytes, use middle as initial DSP
    TASK-COUNT @ 256 * TASK-STACKS + 128 +
    ,                         \ +24 dsp_save (point to middle of stack area)
    0 ,                       \ +32 rsp_save (unused initially)
    0 ,                       \ +40 name_addr (set below)
    \ Register
    TASK-COUNT @ 8 < IF
        TDESC-TEMP @ TASK-COUNT @ CELLS TASK-TABLE + !
        TASK-COUNT @ 1+ TASK-COUNT !
    THEN
    TDESC-TEMP @ CONSTANT ;

\ -- Task queries --
: T.INFO  ( tdesc -- )
    ."  [task"
    DUP ."   st=" T.STATUS .
    DUP ."   pri=" T.PRIORITY .
    ."   xt=" T.XT . ."  ]" CR ;

: TASKS  ( -- )
    ."  --- Tasks (" TASK-COUNT @ . ."  ) ---" CR
    TASK-COUNT @ DUP IF
        0 DO
            I . ."  : "
            I CELLS TASK-TABLE + @ T.INFO
        LOOP
    ELSE DROP THEN ;

\ -- Find next ready task --
\   Scans task table for the first READY task.
\   Returns task descriptor address or 0 if none ready.
: FIND-READY  ( -- tdesc | 0 )
    0                                  \ default: not found
    TASK-COUNT @ DUP 0<> IF
        0 DO
            I CELLS TASK-TABLE + @     ( tdesc )
            DUP T.STATUS T.READY = IF
                NIP LEAVE              \ replace 0 with tdesc
            ELSE DROP THEN
        LOOP
    ELSE DROP THEN ;

\ -- RUN-TASK ( tdesc -- ) execute a task's XT, mark DONE when it returns --
: RUN-TASK  ( tdesc -- )
    DUP T.RUNNING SWAP T.STATUS!    ( -- make it running )
    DUP CURRENT-TASK !
    T.XT EXECUTE                    ( run the task body )
    \ When XT returns, mark task as DONE
    CURRENT-TASK @ DUP IF
        T.DONE SWAP T.STATUS!
    ELSE DROP THEN ;

\ -- SCHEDULE ( -- ) run scheduler: execute ready tasks round-robin --
\   Each call picks the next ready task, runs it to completion
\   (or until it yields), then returns.
: SCHEDULE  ( -- )
    1 SCHED-RUNNING !
    BEGIN
        FIND-READY DUP
    WHILE
        RUN-TASK
    REPEAT
    DROP
    0 SCHED-RUNNING ! ;

\ -- YIELD ( -- ) cooperative yield: mark current task READY, return --
\   The task's XT should call YIELD to give up the CPU.
\   Since our tasks run to completion or yield by returning,
\   YIELD marks the task as DONE (it has "yielded" its time slice).
: SCHED-YIELD  ( -- )
    COREID 0<> IF EXIT THEN
    CURRENT-TASK @ DUP IF
        T.DONE SWAP T.STATUS!
    ELSE DROP THEN ;

: YIELD  ( -- )
    \ KDOS tasks are scheduled on core 0.  A dispatched full-core worker has
    \ no CURRENT-TASK entry and must never mutate core 0's scheduler state.
    SCHED-YIELD ;

\ CORE-CHECKPOINT is deferred because the per-core preemption table is built
\ later in the multicore section.  Words compiled before that point (notably
\ CORE-WAIT and LOCK) still reach the final installed implementation.
: _CORE-CHECKPOINT-BOOT  ( -- )
    PREEMPT-FLAG @ IF
        0 PREEMPT-FLAG !
        YIELD
    THEN ;

DEFER CORE-CHECKPOINT
' _CORE-CHECKPOINT-BOOT IS CORE-CHECKPOINT

: YIELD?  ( -- )  CORE-CHECKPOINT ;

\ -- SPAWN ( xt -- ) create an anonymous ready task with default priority --
VARIABLE SPAWN-COUNT
0 SPAWN-COUNT !

: SPAWN  ( xt -- )
    128                               ( xt priority=128 )
    HERE TDESC-TEMP !
    T.READY ,                         \ +0  status
    ,                                 \ +8  priority
    ,                                 \ +16 xt
    TASK-COUNT @ 256 * TASK-STACKS + 128 +
    ,                                 \ +24 dsp_save
    0 ,                               \ +32 rsp_save
    0 ,                               \ +40 name_addr
    TASK-COUNT @ 8 < IF
        TDESC-TEMP @ TASK-COUNT @ CELLS TASK-TABLE + !
        TASK-COUNT @ 1+ TASK-COUNT !
    THEN
    SPAWN-COUNT @ 1+ SPAWN-COUNT ! ;

\ -- KILL ( tdesc -- ) cancel a task by marking it DONE --
: KILL  ( tdesc -- )
    T.DONE SWAP T.STATUS! ;

\ -- RESTART ( tdesc -- ) reset a DONE task back to READY --
: RESTART  ( tdesc -- )
    T.READY SWAP T.STATUS! ;

\ -- BG ( xt -- ) spawn task and run scheduler --
: BG  ( xt -- )
    SPAWN SCHEDULE ;

\ -- TASK-COUNT-READY ( -- n ) count tasks in READY state --
: TASK-COUNT-READY  ( -- n )
    0                                 ( count )
    TASK-COUNT @ DUP IF
        0 DO
            I CELLS TASK-TABLE + @ T.STATUS T.READY = IF
                1+
            THEN
        LOOP
    ELSE DROP THEN ;

\ -- Timer preemption setup --
\   Uses polling approach: YIELD? checks if the timer counter has
\   exceeded the preemption threshold.  This is simpler than a
\   hardware ISR (which would need raw machine code for RTI).
\
\   PREEMPT-ON starts the auto-reload timer and enables the flag check.
\   YIELD? checks the timer STATUS register for compare-match.

VARIABLE PREEMPT-ENABLED
0 PREEMPT-ENABLED !

\ -- PREEMPT-ON ( -- ) enable timer-based preemption polling --
: PREEMPT-ON  ( -- )
    TIME-SLICE @ TIMER!           \ set compare value
    5 TIMER-CTRL!                 \ enable + auto-reload (no IRQ)
    1 PREEMPT-ENABLED ! ;

\ -- PREEMPT-OFF ( -- ) disable timer preemption --
: PREEMPT-OFF  ( -- )
    1 TIMER-CTRL!                 \ enable counter only, no auto-reload
    0 PREEMPT-ENABLED ! ;

\ Install the timer-aware single-core checkpoint.  Existing callers of the
\ deferred CORE-CHECKPOINT immediately see this action.
: _CORE-CHECKPOINT-TIMER  ( -- )
    PREEMPT-ENABLED @ IF
        PREEMPT-FLAG @ IF
            0 PREEMPT-FLAG !
            YIELD
        THEN
    THEN ;

' _CORE-CHECKPOINT-TIMER IS CORE-CHECKPOINT

\ =====================================================================
\  §8.1  Multicore Dispatch
\ =====================================================================
\
\  High-level words that build on the BIOS multicore primitives
\  (COREID, NCORES, WAKE-CORE, CORE-STATUS, SPIN@, SPIN!).
\
\  CORE-RUN   ( xt core -- )  dispatch XT to a secondary core
\  CORE-WAIT  ( core -- )     busy-wait until a core finishes
\  ALL-CORES-WAIT ( -- )      wait for all secondary cores to idle
\  BARRIER    ( -- )          synchronize: wait for all cores
\  LOCK       ( n -- )        acquire spinlock n (busy-wait)
\  UNLOCK     ( n -- )        release spinlock n
\  CORES      ( -- )          display per-core status
\  P.RUN-PAR  ( pipe -- )     run pipeline steps in parallel across cores
\
\  Uses BIOS words: COREID NCORES WAKE-CORE CORE-STATUS SPIN@ SPIN!
\                   IPI-SEND IPI-STATUS MBOX! MBOX@
\
\  --- Multicore Concurrency Contract ---
\
\  All dictionary, heap, and arena-management words use shared
\  scratch VARIABLEs (A-PREV, A-CURR, AR-SZ, FL-PREV, etc.) that
\  are NOT safe under concurrent execution.  The following words
\  enforce core-0 only access via ?CORE0:
\
\    ALLOCATE  FREE  RESIZE   (heap — shared free-list + scratch)
\    ARENA-NEW  ARENA-NEW-AT  (arena setup — AR-SZ, AR-SRC, AR-BLK)
\    ARENA-DESTROY            (calls FREE or XMEM-FREE-BLOCK)
\
\  Secondary cores dispatched via CORE-RUN should ONLY use:
\
\    ARENA-ALLOT / ARENA-ALLOT?  (pure stack + one arena-local ptr)
\    ARENA-FREE / ARENA-USED     (read-only)
\    ARENA-SNAP / ARENA-ROLLBACK (single pointer write)
\    AALLOT                      (via CURRENT-ARENA — push before dispatch)
\    Direct memory access (@ ! C@ C! MOVE FILL etc.)
\
\  Pattern: core 0 creates arenas and allocates at setup time,
\  dispatches self-contained XTs that only bump-allocate from
\  pre-created per-core arenas, then collects results after BARRIER.
\
\  Per-core arenas eliminate contention entirely — each core only
\  touches its own bump pointer.  Inter-core results pass through
\  the mailbox (MBOX! / MBOX@); scratch stays local.

\ -- CORE-RUN ( xt core -- )  dispatch XT to secondary core --
\   Validates the core number, then sends via WAKE-CORE.
\   Note: caller is responsible for ensuring the XT is safe for
\   the target core type (micro-cores cannot run tile/MEX ops).
: CORE-RUN  ( xt core -- )
    DUP COREID = ABORT" Cannot dispatch to self"
    DUP 0<  OVER NCORES >= OR ABORT" Invalid core ID"
    WAKE-CORE ;

\ -- CORE-WAIT ( core -- )  busy-wait until core is idle --
\   Polls CORE-STATUS (worker XT slot) until it reads 0.
\   Each iteration also checks YIELD? so preemption still works.
: CORE-WAIT  ( core -- )
    BEGIN
        DUP CORE-STATUS 0<>
    WHILE
        YIELD?
    REPEAT
    DROP ;

\ -- ALL-CORES-WAIT ( -- )  wait for all secondary cores to idle --
: ALL-CORES-WAIT  ( -- )
    NCORES 1 DO
        I CORE-WAIT
    LOOP ;

\ -- ALL-FULL-WAIT ( -- )  wait for all secondary full cores to idle --
: ALL-FULL-WAIT  ( -- )
    N-FULL-CORES 1 DO
        I CORE-WAIT
    LOOP ;

\ -- BARRIER ( -- )  synchronize: wait for all secondary cores --
\   Core 0 calls this to wait until all dispatched work is finished.
: BARRIER  ( -- )
    ALL-CORES-WAIT ;

\ -- LOCK ( n -- )  acquire spinlock n (busy-wait) --
\   Retries SPIN@ until it returns 0 (acquired).
: LOCK  ( n -- )
    BEGIN
        DUP SPIN@ 0<>
    WHILE
        YIELD?
    REPEAT
    DROP ;

\ -- UNLOCK ( n -- )  release spinlock n --
: UNLOCK  ( n -- )
    SPIN! ;

\ -- CORES ( -- )  display per-core status --
: CORES  ( -- )
    ."  --- Cores (" NCORES . ."  ) ---" CR
    NCORES 0 DO
        ."    Core " I .
        I COREID = IF
            ."   [self] RUNNING" CR
        ELSE
            I CORE-STATUS IF
                ."   BUSY" CR
            ELSE
                ."   IDLE" CR
            THEN
        THEN
    LOOP ;

\ -- Parallel pipeline variables --
VARIABLE PAR-PIPE       \ pipeline being dispatched
VARIABLE PAR-STEP       \ current step index (used by wrappers)
VARIABLE PAR-CORE       \ next core to assign

\ -- Step wrapper XTs: executed on secondary cores --
\   We pre-define wrappers for steps 0-7 (max pipeline capacity).
\   Each wrapper reads the pipeline and step index from shared
\   variables, looks up the step XT, and calls it.

\ Since secondary cores call the XT directly and the pipeline
\ step XTs are no-argument words (they operate on pre-bound
\ buffers), we dispatch them directly via CORE-RUN.

\ -- P.RUN-PAR ( pipe -- )  run pipeline steps in parallel --
\   Distributes steps across available secondary FULL cores only.
\   Pipeline steps use tile/MEX ops which micro-cores cannot execute.
\   If there are more steps than full cores, remaining steps run on core 0.
\   Always waits for all dispatched work before returning.
VARIABLE PAR-P          \ pipeline being dispatched
VARIABLE PAR-N          \ next core to use

: P.RUN-PAR  ( pipe -- )
    N-FULL-CORES 1 <= IF
        \ Single core: fall back to sequential
        P.RUN EXIT
    THEN
    DUP P.COUNT 0= IF DROP EXIT THEN
    PAR-P !
    1 PAR-N !               \ start dispatching to core 1
    PAR-P @ P.COUNT 0 DO
        PAR-P @ I P.GET      ( step-xt )
        PAR-N @ N-FULL-CORES < IF
            PAR-N @ CORE-RUN
            PAR-N @ 1+ PAR-N !
        ELSE
            EXECUTE
        THEN
    LOOP
    ALL-FULL-WAIT ;

\ -- P.BENCH-PAR ( pipe -- )  benchmark parallel pipeline --
: P.BENCH-PAR  ( pipe -- )
    ."  Parallel pipeline (" DUP P.COUNT . ."  steps, "
    NCORES . ."  cores):" CR
    DUP
    CYCLES >R
    P.RUN-PAR
    CYCLES R> -
    ."    total = " . ."  cycles" CR ;

\ =====================================================================
\  §8.2  Per-Core Run Queues
\ =====================================================================
\
\  Each core has its own circular queue of task XTs.  Tasks can be
\  enqueued on any core and dequeued/dispatched independently.
\  Core 0 runs dequeued tasks locally; secondary cores receive
\  tasks via CORE-RUN (IPI dispatch).
\
\  Queue layout: circular buffer with head/tail indices per core.
\  RQ-DEPTH entries per core, NCORES_MAX (16) cores.

\ -- Constants --
8 CONSTANT RQ-DEPTH       \ max tasks per core queue
16 CONSTANT NCORES_MAX    \ maximum cores (4 full + 12 micro)

\ -- Per-core queue storage --
\    RQ-SLOTS: NCORES_MAX × RQ-DEPTH × CELL = 16×8×8 = 1024 bytes
\    Each slot holds an XT (0 = empty).
VARIABLE RQ-SLOTS  1023 ALLOT

\ -- Per-core head/tail indices (one CELL each per core) --
\    HEAD = next slot to dequeue from
\    TAIL = next slot to enqueue into
VARIABLE RQ-HEADS  127 ALLOT      \ 16 × 8 = 128 bytes
VARIABLE RQ-TAILS  127 ALLOT      \ 16 × 8 = 128 bytes

\ -- Queue initialisation --
: RQ-INIT  ( -- )
    NCORES_MAX 0 DO
        0 I CELLS RQ-HEADS + !
        0 I CELLS RQ-TAILS + !
        RQ-DEPTH 0 DO
            0  J RQ-DEPTH * I + CELLS RQ-SLOTS + !
        LOOP
    LOOP ;

RQ-INIT       \ initialise at load time

\ -- Slot address: ( slot core -- addr ) --
: RQ-SLOT  ( slot core -- addr )
    RQ-DEPTH * + CELLS RQ-SLOTS + ;

\ -- RQ-COUNT ( core -- n ) number of enqueued tasks --
: RQ-COUNT  ( core -- n )
    DUP CELLS RQ-TAILS + @          ( core tail )
    SWAP CELLS RQ-HEADS + @         ( tail head )
    - DUP 0< IF RQ-DEPTH + THEN ;

\ -- RQ-EMPTY? ( core -- flag ) true if core's queue is empty --
: RQ-EMPTY?  ( core -- flag )
    RQ-COUNT 0= ;

\ -- RQ-FULL? ( core -- flag ) true if core's queue is full --
: RQ-FULL?  ( core -- flag )
    RQ-COUNT RQ-DEPTH 1- >= ;

\ -- RQ-PUSH ( xt core -- ) enqueue a task XT onto a core's queue --
: RQ-PUSH  ( xt core -- )
    DUP RQ-FULL? ABORT" Run queue full"
    DUP CELLS RQ-TAILS + @          ( xt core tail )
    2 PICK                           ( xt core tail xt )
    DROP                             ( xt core tail )
    OVER                             ( xt core tail core )
    RQ-SLOT                          ( xt core slot-addr )
    ROT SWAP !                       ( core )  \ store xt at slot
    DUP CELLS RQ-TAILS + @          ( core tail )
    1+ RQ-DEPTH MOD                  ( core new-tail )
    SWAP CELLS RQ-TAILS + ! ;

\ -- RQ-POP ( core -- xt | 0 ) dequeue next XT from a core's queue --
: RQ-POP  ( core -- xt | 0 )
    DUP RQ-EMPTY? IF DROP 0 EXIT THEN
    DUP CELLS RQ-HEADS + @          ( core head )
    OVER                             ( core head core )
    RQ-SLOT @                        ( core xt )
    SWAP                             ( xt core )
    DUP CELLS RQ-HEADS + @          ( xt core head )
    1+ RQ-DEPTH MOD                  ( xt core new-head )
    SWAP CELLS RQ-HEADS + ! ;        ( xt )

\ -- RQ-CLEAR ( core -- ) clear a core's queue --
: RQ-CLEAR  ( core -- )
    DUP CELLS RQ-HEADS + 0 SWAP !
    CELLS RQ-TAILS + 0 SWAP ! ;

\ -- SCHED-CORE ( core -- ) dispatch all queued tasks on a core --
\   Core 0: runs locally via EXECUTE.
\   Cores 1-N: dispatches via CORE-RUN, waits for each to finish.
: SCHED-CORE  ( core -- )
    BEGIN
        DUP RQ-EMPTY? 0=
    WHILE
        DUP RQ-POP                   ( core xt )
        OVER 0= IF
            EXECUTE                  \ core 0: run locally
        ELSE
            OVER CORE-RUN            \ secondary: dispatch via IPI
            DUP CORE-WAIT            \ wait for completion
        THEN
    REPEAT
    DROP ;

\ -- SCHED-ALL ( -- ) dispatch tasks from all core queues --
\   Dispatches secondary cores first (so they run in parallel),
\   then drains core 0's queue locally.
: SCHED-ALL  ( -- )
    \ First pass: kick one task to each secondary core
    NCORES 1 DO
        I RQ-EMPTY? 0= IF
            I RQ-POP I CORE-RUN      \ dispatch first task
        THEN
    LOOP
    \ Drain core 0's queue locally while secondaries work
    0 SCHED-CORE
    \ Wait for all secondary cores and drain their remaining tasks
    NCORES 1 DO
        I CORE-WAIT                  \ wait for current task
        I SCHED-CORE                 \ drain remaining tasks
    LOOP ;

\ -- RQ-INFO ( -- ) display per-core queue status --
: RQ-INFO  ( -- )
    ."  --- Run Queues ---" CR
    NCORES 0 DO
        ."    Core " I . ."  : "
        I RQ-COUNT . ."  task(s)"
        I RQ-EMPTY? IF ."   [empty]" THEN
        CR
    LOOP ;

\ =====================================================================
\  §8.3  Work Stealing
\ =====================================================================
\
\  When a core's run queue is empty, it can "steal" a task from the
\  busiest core's queue.  This balances load automatically without
\  manual task placement.
\
\  STEAL-FROM   ( victim thief -- flag )  steal one task
\  RQ-BUSIEST   ( exclude -- core | -1 )  find core with most tasks
\  WORK-STEAL   ( core -- flag )          try to steal for a core
\  BALANCE      ( -- )                    rebalance all queues

\ -- STEAL-FROM ( victim thief -- flag ) steal one task from victim to thief --
\   Returns true (-1) if a task was stolen, false (0) otherwise.
: STEAL-FROM  ( victim thief -- flag )
    OVER RQ-EMPTY? IF 2DROP 0 EXIT THEN
    SWAP RQ-POP                       ( thief xt )
    DUP 0= IF 2DROP 0 EXIT THEN
    SWAP RQ-PUSH  -1 ;               ( flag )

\ -- RQ-BUSIEST ( exclude -- core | -1 ) find full core with most queued tasks --
\   Skips the core with ID 'exclude' and micro-cores.
\   Returns -1 if all full-core queues empty.
: RQ-BUSIEST  ( exclude -- core | -1 )
    -1                                ( exclude best-core )
    0                                 ( exclude best-core best-count )
    N-FULL-CORES 0 DO
        I 3 PICK = IF                \ skip excluded core
        ELSE
            I RQ-COUNT DUP           ( excl bc bcnt cnt cnt )
            2 PICK > IF              ( excl bc bcnt cnt )
                NIP NIP              ( excl cnt )
                I SWAP               ( excl new-core cnt )
            ELSE DROP THEN
        THEN
    LOOP
    DROP                              ( exclude best-core )
    NIP ;                             ( best-core )

\ -- WORK-STEAL ( core -- flag ) try to steal one task for core --
\   Finds the busiest other core and steals one task from it.
\   Returns true if a task was stolen.
: WORK-STEAL  ( core -- flag )
    DUP RQ-BUSIEST                    ( core victim )
    DUP -1 = IF 2DROP 0 EXIT THEN    \ no tasks anywhere
    DUP RQ-EMPTY? IF 2DROP 0 EXIT THEN
    SWAP STEAL-FROM ;                 ( flag )

\ -- BALANCE ( -- ) rebalance work across all full cores --
\   Idle full cores steal from the busiest full core, one task at a time,
\   until no more imbalance exists (max difference ≤ 1).
: BALANCE  ( -- )
    \ Repeat until stable
    BEGIN
        0                             ( stole-any? )
        N-FULL-CORES 0 DO
            I RQ-EMPTY? IF
                I WORK-STEAL IF
                    DROP -1           \ mark that we stole something
                THEN
            THEN
        LOOP
        0=                            ( stop if nothing was stolen )
    UNTIL ;

\ -- SCHED-BALANCED ( -- ) balance then dispatch all --
: SCHED-BALANCED  ( -- )
    BALANCE SCHED-ALL ;

\ =====================================================================
\  §8.4  Core Affinity
\ =====================================================================
\
\  Pin tasks to specific cores.  An affinity table maps task slots
\  (from the §8 TASK-TABLE) to a preferred core.  -1 means "any core"
\  (no affinity).  SPAWN-ON creates a task directly on a core's queue.

VARIABLE AFF-TABLE  63 ALLOT

: AFF-INIT  ( -- )
    8 0 DO
        -1  I CELLS AFF-TABLE + !
    LOOP ;

AFF-INIT

: AFFINITY!  ( core task# -- )
    DUP 8 >= ABORT" Invalid task slot"
    CELLS AFF-TABLE + ! ;

: AFFINITY@  ( task# -- core )
    DUP 8 >= ABORT" Invalid task slot"
    CELLS AFF-TABLE + @ ;

: SPAWN-ON  ( xt core -- )
    DUP 0<  OVER NCORES >= OR ABORT" Invalid core ID"
    DUP MICRO-CORE? IF
        ." WARNING: dispatching to micro-core (no tile/MEX)" CR
    THEN
    OVER OVER
    RQ-PUSH
    TASK-COUNT @ DUP 8 < IF
        DUP >R AFFINITY!
        HERE TDESC-TEMP !
        T.READY , 128 , , 0 , 0 , 0 ,
        TDESC-TEMP @ R> CELLS TASK-TABLE + !
        TASK-COUNT @ 1+ TASK-COUNT !
    ELSE
        DROP 2DROP
    THEN ;

: SCHED-AFFINE  ( -- )
    TASK-COUNT @ 0 ?DO
        I CELLS TASK-TABLE + @
        DUP T.STATUS T.READY = IF
            DUP T.XT
            I AFFINITY@
            DUP -1 = IF DROP 0 THEN
            RQ-PUSH
            T.RUNNING SWAP T.STATUS!
        ELSE DROP THEN
    LOOP
    SCHED-ALL ;

: AFF-INFO  ( -- )
    ."  --- Core Affinity ---" CR
    TASK-COUNT @ DUP 0= IF DROP ."    (no tasks)" CR EXIT THEN
    0 DO
        ."    Task " I . ."  -> "
        I AFFINITY@ DUP -1 = IF
            DROP ."  any"
        ELSE
            ."  core " .
        THEN
        CR
    LOOP ;

\ =====================================================================
\  §8.5  Per-Core Preemption
\ =====================================================================
\
\  Timer-assisted preemption for all cores.  The timer IRQ is
\  broadcast to all cores, and each core's ISR sets its own preempt
\  flag.  Cooperative yield points (YIELD?) check the per-core flag.

VARIABLE PREEMPT-FLAGS  127 ALLOT     \ 16 × 8 = 128 bytes

: PREEMPT-FLAGS-INIT  ( -- )
    NCORES_MAX 0 DO
        0  I CELLS PREEMPT-FLAGS + !
    LOOP ;

PREEMPT-FLAGS-INIT

: PREEMPT-FLAG!  ( val core -- )
    CELLS PREEMPT-FLAGS + ! ;

: PREEMPT-FLAG@  ( core -- val )
    CELLS PREEMPT-FLAGS + @ ;

: PREEMPT-SET  ( core -- )
    1 SWAP PREEMPT-FLAG! ;

: PREEMPT-CLR  ( core -- )
    0 SWAP PREEMPT-FLAG! ;

: PREEMPT-ON-ALL  ( -- )
    TIME-SLICE @ TIMER!
    7 TIMER-CTRL!
    1 PREEMPT-ENABLED ! ;

: PREEMPT-OFF-ALL  ( -- )
    1 TIMER-CTRL!
    0 PREEMPT-ENABLED !
    PREEMPT-FLAGS-INIT ;

: WORKER-CHECKPOINT  ( -- )
    PREEMPT-ENABLED @ IF
        COREID PREEMPT-FLAG@ IF
            COREID PREEMPT-CLR
        THEN
    THEN ;

: _CORE-CHECKPOINT-PER-CORE  ( -- )
    COREID 0= IF
        PREEMPT-ENABLED @ IF
            0 PREEMPT-FLAG@ IF
                0 PREEMPT-CLR SCHED-YIELD
            THEN
        THEN
    ELSE
        \ A secondary full core acknowledges its own flag and continues its
        \ one-shot dispatch; there is no suspended task scheduler to enter.
        WORKER-CHECKPOINT
    THEN ;

' _CORE-CHECKPOINT-PER-CORE IS CORE-CHECKPOINT

: PREEMPT-INFO  ( -- )
    ."  --- Preemption ---" CR
    ."    Enabled: " PREEMPT-ENABLED @ IF ."  yes" ELSE ."  no" THEN CR
    ."    Slice:   " TIME-SLICE @ . ."  cycles" CR
    NCORES 0 DO
        ."    Core " I . ."  : flag="
        I PREEMPT-FLAG@ . CR
    LOOP ;

\ =====================================================================
\  §8.6  IPI Messaging
\ =====================================================================
\
\  Structured inter-core message passing via shared-memory queues.
\  Each core has a MSG-DEPTH-deep circular inbox.  Messages are
\  3 cells wide: type, sender, payload.  Protected by hardware
\  spinlock MSG-SLOCK.
\
\  MSG-SEND      ( type payload target -- flag )
\  MSG-RECV      ( -- type sender payload flag )
\  MSG-PEEK      ( -- flag )
\  MSG-BROADCAST ( type payload -- n )
\  MSG-FLUSH     ( -- n )
\  MSG-HANDLER!  ( xt type -- )
\  MSG-DISPATCH  ( -- flag )
\  MSG-INFO      ( -- )

8 CONSTANT MSG-DEPTH
3 CONSTANT MSG-CELLS
7 CONSTANT MSG-SLOCK

VARIABLE MSG-INBOX  3071 ALLOT        \ 16 × 8 × 3 × 8 = 3072 bytes
VARIABLE MSG-IHEAD  127 ALLOT         \ 16 × 8 = 128 bytes
VARIABLE MSG-ITAIL  127 ALLOT         \ 16 × 8 = 128 bytes

0 CONSTANT MSG-CALL
1 CONSTANT MSG-DATA
2 CONSTANT MSG-SIGNAL
3 CONSTANT MSG-USER

: MSG-ISLOT  ( idx core -- addr )
    MSG-DEPTH MSG-CELLS * CELLS *  MSG-INBOX +
    SWAP MSG-CELLS CELLS * + ;

: MSG-ICOUNT  ( core -- n )
    DUP CELLS MSG-ITAIL + @  SWAP CELLS MSG-IHEAD + @  -
    MSG-DEPTH + MSG-DEPTH MOD ;

: MSG-IFULL?  ( core -- flag )
    MSG-ICOUNT MSG-DEPTH 1- >= ;

: MSG-IEMPTY?  ( core -- flag )
    DUP CELLS MSG-IHEAD + @  SWAP CELLS MSG-ITAIL + @  = ;

: MSG-INIT  ( -- )
    NCORES_MAX 0 DO
        0 I CELLS MSG-IHEAD + !
        0 I CELLS MSG-ITAIL + !
    LOOP
    MSG-INBOX  MSG-DEPTH MSG-CELLS * NCORES_MAX * CELLS  0 FILL ;

MSG-INIT

VARIABLE MS-T   VARIABLE MS-P   VARIABLE MS-G

: MSG-SEND  ( type payload target -- flag )
    MS-G !  MS-P !  MS-T !
    MSG-SLOCK LOCK
    MS-G @ MSG-IFULL? IF  MSG-SLOCK UNLOCK  0 EXIT  THEN
    MS-G @ DUP CELLS MSG-ITAIL + @  SWAP MSG-ISLOT
    MS-T @  OVER !  CELL+
    COREID  OVER !  CELL+
    MS-P @  SWAP !
    MS-G @ CELLS MSG-ITAIL + @  1+  MSG-DEPTH MOD
    MS-G @ CELLS MSG-ITAIL + !
    MSG-SLOCK UNLOCK  -1 ;

VARIABLE MR-T   VARIABLE MR-S   VARIABLE MR-P

: MSG-RECV  ( -- type sender payload flag )
    COREID MSG-IEMPTY? IF  0 0 0 0 EXIT  THEN
    MSG-SLOCK LOCK
    COREID DUP CELLS MSG-IHEAD + @  SWAP MSG-ISLOT
    DUP @ MR-T !  CELL+
    DUP @ MR-S !  CELL+
    @     MR-P !
    COREID DUP CELLS MSG-IHEAD + @  1+  MSG-DEPTH MOD
    COREID CELLS MSG-IHEAD + !
    MSG-SLOCK UNLOCK
    MR-T @ MR-S @ MR-P @ -1 ;

: MSG-PEEK  ( -- flag )
    COREID MSG-IEMPTY? INVERT ;

4 CONSTANT MSG-HTYPES
VARIABLE MSG-HTABLE  31 ALLOT

: MSG-HINIT  ( -- )
    MSG-HTYPES 0 DO  0 I CELLS MSG-HTABLE + !  LOOP ;

MSG-HINIT

: MSG-HANDLER!  ( xt type -- )
    CELLS MSG-HTABLE + ! ;

: MSG-HANDLER@  ( type -- xt|0 )
    DUP MSG-HTYPES < IF  CELLS MSG-HTABLE + @  ELSE  DROP 0  THEN ;

: MSG-DISPATCH  ( -- flag )
    MSG-PEEK 0= IF  0 EXIT  THEN
    MSG-RECV DROP
    ROT DUP MSG-HANDLER@
    DUP IF  EXECUTE -1  ELSE  DROP DROP DROP DROP 0  THEN ;

VARIABLE MB-T   VARIABLE MB-P

: MSG-BROADCAST  ( type payload -- n )
    MB-P !  MB-T !
    0
    NCORES 0 DO
        I COREID <> IF
            MB-T @ MB-P @ I MSG-SEND IF  1+  THEN
        THEN
    LOOP ;

: MSG-FLUSH  ( -- n )
    0
    BEGIN  MSG-PEEK  WHILE
        MSG-RECV IF  DROP DROP DROP  THEN  1+
    REPEAT ;

: MSG-INFO  ( -- )
    ."  --- IPI Messages ---" CR
    NCORES 0 DO
        ."    Core " I . ."  : " I MSG-ICOUNT . ."  msg(s)" CR
    LOOP
    ."  Handlers:" CR
    MSG-HTYPES 0 DO
        I MSG-HANDLER@ IF
            ."    type " I . CR
        THEN
    LOOP ;

\ =====================================================================
\  §8.7  Shared Resource Locks
\ =====================================================================
\
\  Named machine-wide spinlock assignments.  Application/runtime concurrency
\  owns lock 6 (Akashic uses it as EVT-LOCK); crypto must not reuse it.
\  Spinlock 7 is reserved for IPI messaging (MSG-SLOCK).
\
\  DICT-ACQUIRE / DICT-RELEASE   — dictionary (HERE, ALLOT, CREATE)
\  UART-ACQUIRE / UART-RELEASE   — UART output (EMIT, TYPE, .)
\  FS-ACQUIRE   / FS-RELEASE     — filesystem (F-OPEN, F-READ, etc.)
\  HEAP-ACQUIRE / HEAP-RELEASE   — heap allocator (ALLOC, FREE)
\  WITH-LOCK    ( xt lock# -- )  — execute xt while holding lock
\
\  Checked disk/filesystem operations acquire FS-LOCK internally.  Do not
\  wrap them in FS-ACQUIRE or WITH-LOCK: the hardware recognizes a same-core
\  reacquire but has no recursion depth, so the inner release would end the
\  outer critical section early.

0 CONSTANT DICT-LOCK
1 CONSTANT UART-LOCK
2 CONSTANT FS-LOCK
3 CONSTANT HEAP-LOCK
4 CONSTANT RING-LOCK
5 CONSTANT HT-LOCK
6 CONSTANT APP-LOCK

: DICT-ACQUIRE  ( -- )  DICT-LOCK LOCK ;
: DICT-RELEASE  ( -- )  DICT-LOCK UNLOCK ;
: UART-ACQUIRE  ( -- )  UART-LOCK LOCK ;
: UART-RELEASE  ( -- )  UART-LOCK UNLOCK ;
: FS-ACQUIRE    ( -- )  FS-LOCK LOCK ;
: FS-RELEASE    ( -- )  FS-LOCK UNLOCK ;
: HEAP-ACQUIRE  ( -- )  HEAP-LOCK LOCK ;
: HEAP-RELEASE  ( -- )  HEAP-LOCK UNLOCK ;

: WITH-LOCK  ( xt lock# -- )
    DUP >R LOCK
    EXECUTE
    R> UNLOCK ;

: LOCK-INFO  ( -- )
    ."  --- Resource Locks ---" CR
    ."  Assignments:" CR
    ."    0 = Dictionary" CR
    ."    1 = UART" CR
    ."    2 = Filesystem" CR
    ."    3 = Heap" CR
    ."    4 = Ring Buffers" CR
    ."    5 = Hash Tables" CR
    ."    6 = Application Runtime" CR
    ."    7 = IPI Messaging" CR ;

\ =====================================================================
\  §8.8  Micro-Cluster Support
\ =====================================================================
\
\  High-level words for managing micro-core clusters.  Builds on the
\  BIOS primitives: CLUSTER-EN! CLUSTER-EN@ BARRIER-ARRIVE
\  BARRIER-STATUS SPAD N-FULL MICRO? HBW-BASE HBW-SIZE
\  and KDOS §1 words: MICRO-CORE? FULL-CORE? N-FULL-CORES
\
\  CLUSTER-ENABLE   ( n -- )    enable cluster n (0-based)
\  CLUSTER-DISABLE  ( n -- )    disable cluster n
\  CLUSTERS-ON      ( -- )      enable all 3 clusters
\  CLUSTERS-OFF     ( -- )      disable all clusters
\  CLUSTER-STATE    ( -- )      display cluster enable state
\  HW-BARRIER-WAIT  ( -- )      arrive at hardware barrier, spin until done
\  SPAD-C@          ( off -- c ) read byte from cluster scratchpad
\  SPAD-C!          ( c off -- ) write byte to cluster scratchpad

3 CONSTANT NUM-CLUSTERS

\ CLUSTER-ENABLE ( n -- )  enable cluster n by setting bit n in mask
: CLUSTER-ENABLE  ( n -- )
    DUP 0< OVER NUM-CLUSTERS >= OR ABORT" Invalid cluster ID"
    1 SWAP LSHIFT
    CLUSTER-EN@ OR
    CLUSTER-EN! ;

\ CLUSTER-DISABLE ( n -- )  disable cluster n by clearing bit n
: CLUSTER-DISABLE  ( n -- )
    DUP 0< OVER NUM-CLUSTERS >= OR ABORT" Invalid cluster ID"
    1 SWAP LSHIFT INVERT
    CLUSTER-EN@ AND
    CLUSTER-EN! ;

\ CLUSTERS-ON ( -- )  enable all clusters (mask = 0x07)
: CLUSTERS-ON  ( -- )
    7 CLUSTER-EN! ;

\ CLUSTERS-OFF ( -- )  disable all clusters
: CLUSTERS-OFF  ( -- )
    0 CLUSTER-EN! ;

\ CLUSTER-STATE ( -- )  display cluster enable status
: CLUSTER-STATE  ( -- )
    ."  Clusters: " CLUSTER-EN@ DUP . ."  (mask)" CR
    NUM-CLUSTERS 0 DO
        ."    Cluster " I .
        DUP 1 I LSHIFT AND IF
            ."   ENABLED" CR
        ELSE
            ."   disabled" CR
        THEN
    LOOP DROP ;

\ HW-BARRIER-WAIT ( -- )  arrive and spin until hardware barrier fires
\   Uses the CSR-based barrier (micro-core clusters only).
: HW-BARRIER-WAIT  ( -- )
    BARRIER-ARRIVE
    BEGIN
        BARRIER-STATUS 256 AND 0<>      \ bit 8 = done flag
    UNTIL ;

\ SPAD-C@ ( off -- c )  read byte from cluster scratchpad
: SPAD-C@  ( off -- c )
    SPAD + C@ ;

\ SPAD-C! ( c off -- )  write byte to cluster scratchpad
: SPAD-C!  ( c off -- )
    SPAD + C! ;

\ =====================================================================
\  §8.9  Cluster MPU — Memory Protection for Micro-Cores
\ =====================================================================
\
\  One shared MPU per cluster (not per micro-core) — enforced in the
\  cluster bus arbiter.  MMIO and scratchpad always allowed.
\
\  CL-MPU-SETUP   ( base limit -- )  configure cluster MPU window
\  CL-ENTER-USER  ( -- )             switch cluster to user mode
\  CL-EXIT-USER   ( -- )             return cluster to supervisor mode
\  CL-MPU-OFF     ( -- )             disable cluster MPU (base=limit=0)
\  .CL-MPU        ( -- )             display cluster MPU state

\ CL-MPU-SETUP ( base limit -- )  set cluster MPU window [base, limit)
: CL-MPU-SETUP  ( base limit -- )
    CL-MPU-LIMIT! CL-MPU-BASE! ;

\ CL-ENTER-USER ( -- )  switch cluster privilege to user mode
: CL-ENTER-USER  ( -- )
    1 CL-PRIV! ;

\ CL-EXIT-USER ( -- )  switch cluster back to supervisor mode
: CL-EXIT-USER  ( -- )
    0 CL-PRIV! ;

\ CL-MPU-OFF ( -- )  disable cluster MPU (clear window)
: CL-MPU-OFF  ( -- )
    0 CL-PRIV!
    0 0 CL-MPU-SETUP ;

\ .CL-MPU ( -- )  display cluster MPU configuration
: .CL-MPU  ( -- )
    ."  Cluster MPU:" CR
    ."    priv = " CL-PRIV@ . CR
    ."    base = " CL-MPU-BASE@ HEX U. DECIMAL CR
    ."    limit= " CL-MPU-LIMIT@ HEX U. DECIMAL CR ;

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
\ §9.5  Screen Definition Language (SDL) — Widget Vocabulary
\ =====================================================================
\
\  Standard building blocks for screen definitions.
\  Each W.xxx word encapsulates a common TUI pattern, making screens
\  declarative.  A future renderer (e.g. web/HTML) can redefine these
\  through the vector table (WVEC) without touching screen definitions.
\
\  Widget vocabulary:
\    W.TITLE     ( addr len -- )           Bold section title
\    W.SECTION   ( addr len -- )           Bold sub-heading
\    W.LINE      ( addr len -- )           Indented text line
\    W.KV        ( n addr len -- )         Key : number
\    W.KV-XT     ( xt addr len -- )        Key : <execute xt>
\    W.FLAG      ( flag addr len -- )      Key : green-ON / dim-OFF
\    W.FLAG-2    ( flag t-a t-n f-a f-n addr len -- )  Key : colored yes/no text
\    W.HBAR      ( -- )                    Horizontal rule
\    W.GAP       ( -- )                    Blank line
\    W.LIST      ( count item-xt -- )      Iterable list (sets SCR-MAX)
\    W.DETAIL    ( count xt -- )           Detail pane for selected item
\    W.HINT      ( addr len -- )           Dim action-hint line
\    W.CUSTOM    ( xt -- )                 Escape hatch: call xt directly

\ ── Renderer vector table ─────────────────────────────────────────
\ 15 entries — each holds an xt dispatched by the corresponding W.xxx.
\ Default = TUI renderer.  Swap for web/HTML by replacing all entries.

15 CONSTANT WVEC-SIZE
CREATE WVEC  WVEC-SIZE CELLS ALLOT
 0 CONSTANT WV-TITLE      1 CONSTANT WV-SECTION
 2 CONSTANT WV-LINE       3 CONSTANT WV-KV
 4 CONSTANT WV-KV-XT      5 CONSTANT WV-FLAG
 6 CONSTANT WV-FLAG-2     7 CONSTANT WV-HBAR
 8 CONSTANT WV-GAP        9 CONSTANT WV-LIST
10 CONSTANT WV-DETAIL    11 CONSTANT WV-HINT
12 CONSTANT WV-CUSTOM    13 CONSTANT WV-NONE
14 CONSTANT WV-INPUT

: WV@  ( idx -- xt )  CELLS WVEC + @ ;
: WV!  ( xt idx -- )  CELLS WVEC + ! ;

\ ── TUI renderer implementation ───────────────────────────────────

: TUI-TITLE  ( addr len -- )
    .LABEL ."   " TYPE ./LABEL CR CR ;

: TUI-SECTION  ( addr len -- )
    CR BOLD ."   " TYPE ." :" RESET-COLOR CR ;

: TUI-LINE  ( addr len -- )
    ."    " TYPE CR ;

: TUI-KV  ( n addr len -- )
    ."    " TYPE ."  : " .N CR ;

: TUI-KV-XT  ( xt addr len -- )
    ."    " TYPE ."  : " EXECUTE CR ;

: TUI-FLAG  ( flag addr len -- )
    ."    " TYPE ."  : "
    IF 2 FG ." ON" ELSE DIM ." OFF" THEN RESET-COLOR CR ;

: TUI-FLAG-2  ( flag true-a true-n false-a false-n addr len -- )
    ."    " TYPE ."  : "
    2>R ROT IF 2R> 2DROP 2 FG ELSE 2R> ROT DROP ROT DROP DIM THEN
    TYPE RESET-COLOR CR ;

: TUI-HBAR  ( -- )  HBAR ;

: TUI-GAP   ( -- )  CR ;

: TUI-LIST  ( count item-xt -- )
    OVER 0= IF 2DROP ."    (none)" CR  0 SCR-MAX ! EXIT THEN
    OVER SCR-MAX !
    SWAP 0 DO
        SCR-SEL @ I = IF 2 FG ."  > " RESET-COLOR ELSE ."    " THEN
        I OVER EXECUTE CR
    LOOP DROP ;

: TUI-DETAIL  ( count xt -- )
    SWAP SCR-SEL @ DUP -1 = IF 2DROP DROP EXIT THEN
    SWAP OVER >= IF 2DROP EXIT THEN
    CR HBAR EXECUTE ;

: TUI-HINT  ( addr len -- )
    DIM ."   " TYPE RESET-COLOR CR ;

: TUI-CUSTOM  ( xt -- )  EXECUTE ;

\ W.INPUT ( buf maxlen prompt-addr prompt-len -- actual-len )
\   Display prompt, read a line of text into buf (max maxlen chars).
\   Handles printable ASCII (32-126), Backspace (8/127), Enter (13)
\   to confirm, Escape (27) to cancel (returns 0).  Arrow keys and
\   other CSI escape sequences are consumed harmlessly.
\   Buffer is always null-terminated on exit.

: TUI-INPUT  ( buf maxlen prompt-addr prompt-len -- actual-len )
    TYPE                                \ print prompt
    0                                   ( buf maxlen pos )
    BEGIN
        KEY                             ( buf maxlen pos c )
        DUP 13 = IF DROP               \ Enter -> confirm
            2 PICK OVER + 0 SWAP C!    \ null-terminate buf[pos]
            NIP NIP EXIT               ( pos )
        THEN
        DUP 27 = IF DROP               \ ESC byte received
            KEY? IF                     \ sequence follows -> consume it
                KEY DUP 91 = IF         \ CSI '[' prefix
                    DROP
                    BEGIN KEY DUP 64 >= OVER 126 <= AND UNTIL
                    DROP                \ consume until final byte 64-126
                ELSE DROP THEN          \ non-CSI: consumed one extra byte
            ELSE                        \ bare Esc -> cancel
                2 PICK 0 SWAP C!        \ null-terminate buf[0]
                DROP 2DROP 0 EXIT       ( 0 )
            THEN
        ELSE
        DUP 8 = OVER 127 = OR IF       \ Backspace (BS=8, DEL=127)
            DROP
            DUP 0> IF
                1-  8 EMIT 32 EMIT 8 EMIT  \ erase previous char
            THEN
        ELSE
        DUP 32 >= OVER 126 <= AND IF    \ printable ASCII only (32..126)
            2 PICK 2 PICK > IF          \ pos < maxlen?
                DUP EMIT                ( buf maxlen pos c )
                3 PICK 2 PICK + C!      \ buf[pos] = c
                1+
            ELSE DROP THEN
        ELSE
            DROP                        \ ignore control chars / non-ASCII
        THEN THEN THEN
    AGAIN ;

\ ── Install TUI renderer ──────────────────────────────────────────
: INSTALL-TUI  ( -- )
    ['] TUI-TITLE    WV-TITLE   WV!
    ['] TUI-SECTION  WV-SECTION WV!
    ['] TUI-LINE     WV-LINE    WV!
    ['] TUI-KV       WV-KV      WV!
    ['] TUI-KV-XT    WV-KV-XT   WV!
    ['] TUI-FLAG     WV-FLAG    WV!
    ['] TUI-FLAG-2   WV-FLAG-2  WV!
    ['] TUI-HBAR     WV-HBAR    WV!
    ['] TUI-GAP      WV-GAP     WV!
    ['] TUI-LIST     WV-LIST    WV!
    ['] TUI-DETAIL   WV-DETAIL  WV!
    ['] TUI-HINT     WV-HINT    WV!
    ['] TUI-CUSTOM   WV-CUSTOM  WV!
    ['] TUI-INPUT    WV-INPUT   WV! ;
INSTALL-TUI

\ ── Public widget API (dispatch through WVEC) ─────────────────────
: W.TITLE    ( addr len -- )                           WV-TITLE   WV@ EXECUTE ;
: W.SECTION  ( addr len -- )                           WV-SECTION WV@ EXECUTE ;
: W.LINE     ( addr len -- )                           WV-LINE    WV@ EXECUTE ;
: W.KV       ( n addr len -- )                         WV-KV      WV@ EXECUTE ;
: W.KV-XT    ( xt addr len -- )                        WV-KV-XT   WV@ EXECUTE ;
: W.FLAG     ( flag addr len -- )                      WV-FLAG    WV@ EXECUTE ;
: W.FLAG-2   ( flag t-a t-n f-a f-n addr len -- )      WV-FLAG-2  WV@ EXECUTE ;
: W.HBAR     ( -- )                                    WV-HBAR    WV@ EXECUTE ;
: W.GAP      ( -- )                                    WV-GAP     WV@ EXECUTE ;
: W.LIST     ( count item-xt -- )                      WV-LIST    WV@ EXECUTE ;
: W.DETAIL   ( count xt -- )                           WV-DETAIL  WV@ EXECUTE ;
: W.HINT     ( addr len -- )                           WV-HINT    WV@ EXECUTE ;
: W.CUSTOM   ( xt -- )                                 WV-CUSTOM  WV@ EXECUTE ;
: W.INPUT    ( buf maxlen prompt-addr prompt-len -- len ) WV-INPUT   WV@ EXECUTE ;

\ ── Title with dynamic count suffix ──────────────────────────────
\ Convenience: "Label (N)" — used by many list screens.
: W.TITLE-N  ( n addr len -- )
    .LABEL ."   " TYPE ."  (" .N ." )" ./LABEL CR CR ;


\ =====================================================================
\ §9.6  Screen Definitions (SDL)
\ =====================================================================
\
\  Each screen is a word that calls W.xxx widgets.  Registration,
\  event loop, and SCREENS are unchanged.  Item renderers (.XXX-ROW)
\  are small helper words called by W.LIST.

\ ── List-item renderers ──────────────────────────────────────────

: .BTYPE  ( n -- )    \ print buffer type tag
    DUP 0 = IF DROP ." raw" EXIT THEN
    DUP 1 = IF DROP ." rec" EXIT THEN
    DUP 2 = IF DROP ." til" EXIT THEN
    3 = IF ." bit" EXIT THEN
    ." ?" ;

: .BUF-ROW  ( i -- )
    DUP .N ."   "
    BUF-NTH
    DUP B.TYPE .BTYPE
    ."  w=" DUP B.WIDTH .N
    ."  n=" DUP B.LEN .N
    ."  tiles=" DUP B.TILES .N
    ."  @" B.DATA .N ;

: .BUF-DETAIL  ( -- )
    SCR-SEL @ BUF-NTH
    DUP B.INFO B.PREVIEW ;

: .KERN-ROW  ( i -- )
    DUP .N ."   "
    CELLS KERN-TABLE + @
    DUP K.IN .N ."  in "
    DUP K.OUT .N ."  out "
    DUP K.FOOT .N ."  foot "
    K.FLAGS IF 3 FG ." [tile]" RESET-COLOR ELSE DIM ." [cpu]" RESET-COLOR THEN ;

: .PIPE-ROW  ( i -- )
    DUP .N ."   "
    CELLS PIPE-TABLE + @
    ." cap=" DUP P.CAP .N
    ."  steps=" P.COUNT .N ;

: .TASK-STATUS  ( st -- )    \ print colored status tag
    DUP 0 = IF DROP DIM ." FREE " RESET-COLOR EXIT THEN
    DUP 1 = IF DROP 2 FG ." READY" RESET-COLOR EXIT THEN
    DUP 2 = IF DROP 3 FG ." RUN  " RESET-COLOR EXIT THEN
    DUP 3 = IF DROP 1 FG ." BLOCK" RESET-COLOR EXIT THEN
    4 = IF DIM ." DONE " RESET-COLOR EXIT THEN
    ." ?    " ;

: .TASK-ROW  ( i -- )
    DUP .N ."   "
    CELLS TASK-TABLE + @
    DUP T.STATUS .TASK-STATUS
    ."  pri=" DUP T.PRIORITY .N
    ."  xt=" T.XT .N ;

: .TASK-DETAIL  ( -- )
    SCR-SEL @ CELLS TASK-TABLE + @
    ."   Status: " DUP T.STATUS .TASK-STATUS CR
    ."   XT: " DUP T.XT .N ."    Priority: " T.PRIORITY .N CR
    S" [k] Kill  [s] Restart" W.HINT ;

: .CORE-ROW  ( i -- )
    DUP .N ."   "
    DUP MICRO-CORE? IF DIM ." [mu] " RESET-COLOR ELSE ." [full] " THEN
    DUP COREID = IF
        DROP 3 FG ." RUNNING" RESET-COLOR ."  (self)"
    ELSE
        CORE-STATUS IF 2 FG ." BUSY" RESET-COLOR
        ELSE DIM ." IDLE" RESET-COLOR THEN
    THEN ;

: .PORT-ROW  ( i -- )
    ." port " .N ;

\ ── Helper: count active dir entries by file-type ────────────────
\ .DOC-FILE-LIST ( ftype -- n )  list docs/tuts with selection
: .DOC-FILE-LIST  ( ftype -- n )
    0 DOC-N !
    FS-OK @ IF
        0 DOC-TUT-COUNT !
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE OVER = IF
                    SCR-SEL @ DOC-N @ = IF 2 FG ."  > " RESET-COLOR ELSE ."     " THEN
                    DOC-N @ .N ."   " I DIRENT .ZSTR CR
                    1 DOC-N +!  1 DOC-TUT-COUNT +!
                THEN
            THEN
        LOOP
        DROP
        DOC-TUT-COUNT @ 0= IF ."     (none)" CR THEN
    ELSE
        DROP ."     (no filesystem loaded)" CR
    THEN
    DOC-N @ ;

: .STOR-ROW  ( slot i -- )     \ storage row from STOR-N iteration
    DUP .N ."   "
    DROP    \ slot unused here — row printed by caller
    ;

\ ── Screen 1: Home ──
: .HOME-CORES-VAL  ( -- )
    NCORES .N
    NCORES 1 > IF 2 FG ."  multicore" ELSE DIM ."  single" THEN RESET-COLOR ;
: .HOME-PORTS-VAL  ( -- )
    PORT-COUNT @ .N ."  bound  rx=" PORT-RX @ .N ."  drop=" PORT-DROP @ .N ;

: SCR-HOME  ( -- )
    S" System Overview" W.TITLE
    HERE          S" Memory"    W.KV
    ['] .HOME-CORES-VAL S" Cores" W.KV-XT
    BUF-COUNT @   S" Buffers"   W.KV
    KERN-COUNT @  S" Kernels"   W.KV
    PIPE-COUNT @  S" Pipes"     W.KV
    TASK-COUNT @  S" Tasks"     W.KV
    FILE-COUNT @  S" Files"     W.KV
    DISK? S" present" S" not attached" S" Storage" W.FLAG-2
    ['] .HOME-PORTS-VAL S" Ports" W.KV-XT
    NET-RX? S" frame waiting" S" idle" S" Network" W.FLAG-2
    W.GAP
    PREEMPT-ENABLED @ S" preempt ON" S" cooperative" S" Scheduler" W.FLAG-2
    TASK-COUNT-READY  S" Tasks rdy" W.KV ;

\ ── Screen 2: Buffers ──
: SCR-BUFFERS  ( -- )
    BUF-COUNT @ S" Buffers" W.TITLE-N
    BUF-COUNT @ ['] .BUF-ROW W.LIST
    BUF-COUNT @ ['] .BUF-DETAIL W.DETAIL ;

\ ── Screen 3: Kernels ──
: SCR-KERNELS  ( -- )
    KERN-COUNT @ S" Kernels" W.TITLE-N
    KERN-COUNT @ ['] .KERN-ROW W.LIST ;

\ ── Screen 4: Pipelines ──
: SCR-PIPES  ( -- )
    PIPE-COUNT @ S" Pipelines" W.TITLE-N
    PIPE-COUNT @ ['] .PIPE-ROW W.LIST ;

\ ── Screen 5: Tasks ──
: SCR-TASKS  ( -- )
    TASK-COUNT @ S" Tasks" W.TITLE-N
    TASK-COUNT @ ['] .TASK-ROW W.LIST
    TASK-COUNT @ ['] .TASK-DETAIL W.DETAIL ;

\ ── Screen 6: Help ──
: SCR-HELP  ( -- )
    S" Quick Reference" W.TITLE
    S" Buffers" W.SECTION
    S" 0 1 N BUFFER name    Create buffer" W.LINE
    S" buf B.SUM/MIN/MAX    Tile reductions" W.LINE
    S" a b c B.ADD/SUB      Element-wise ops" W.LINE
    S" n buf B.SCALE/FILL   Modify buffer" W.LINE
    S" Kernels" W.SECTION
    S" 1 1 2 0 KERNEL name  Register kernel" W.LINE
    S" buf kzero/kfill/kadd Sample kernels" W.LINE
    S" buf knorm/khistogram  Advanced kernels" W.LINE
    S" th src dst kpeak      Peak detection" W.LINE
    S" Pipelines" W.SECTION
    S" 3 PIPELINE name      Create pipeline" W.LINE
    S" ' w pipe P.ADD/RUN   Build & execute" W.LINE
    S" pipe P.RUN-PAR       Parallel execute" W.LINE
    S" Tasks" W.SECTION
    S" ' w 0 TASK name      Create task" W.LINE
    S" SCHEDULE / BG         Run tasks" W.LINE
    S" Multicore" W.SECTION
    S" xt core CORE-RUN      Dispatch to core" W.LINE
    S" core CORE-WAIT        Wait for core" W.LINE
    S" BARRIER               Sync all cores" W.LINE
    S" n LOCK / n UNLOCK     Spinlock ops" W.LINE
    S" CORES                 Show core status" W.LINE
    S" Storage" W.SECTION
    S" buf sec B.SAVE/LOAD  Persist buffers" W.LINE
    S" DIR / CATALOG        List disk files" W.LINE
    S" CAT name             Print file" W.LINE
    S" buf SAVE-BUFFER name Save buf to file" W.LINE
    S" Data Ports" W.SECTION
    S" buf id PORT!          Bind NIC source" W.LINE
    S" networking.f: POLL / n INGEST" W.LINE
    S" PORTS                 List bindings" W.LINE
    S" Tools" W.SECTION
    S" DASHBOARD / STATUS    System views" W.LINE
    S" ' w BENCH / .BENCH   Benchmark" W.LINE ;

\ ── Screen 7: Documentation ──
: .DOCS-BODY  ( -- )
    S" Topics" W.SECTION
    FTYPE-DOC .DOC-FILE-LIST DROP
    W.GAP
    S" Tutorials" W.SECTION
    FTYPE-TUT .DOC-FILE-LIST DROP
    DOC-N @ SCR-MAX !
    W.GAP
    S" [Enter] Read selected document" W.HINT ;

: SCR-DOCS  ( -- )
    S" Documentation" W.TITLE
    ['] .DOCS-BODY W.CUSTOM ;

\ ── Screen 8: Storage ──
: .STOR-BODY  ( -- )
    DISK? 0= IF
        S" (no storage attached)" W.LINE  0 SCR-MAX ! EXIT
    THEN
    FS-OK @ 0= IF
        S" (filesystem not loaded)" W.LINE  0 SCR-MAX ! EXIT
    THEN
    0 STOR-N !
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            SCR-SEL @ STOR-N @ = IF 2 FG ."  > " RESET-COLOR ELSE ."    " THEN
            STOR-N @ .N ."   "
            I DIRENT .ZSTR
            ."   " I DIRENT DE.USED .N ."  B"
            ."   " I DIRENT DE.TYPE .FTYPE
            CR
            1 STOR-N +!
        THEN
    LOOP
    STOR-N @ SCR-MAX !
    STOR-N @ 0= IF S" (empty)" W.LINE THEN
    W.GAP
    0  FS-TOTAL @ FS-DSTART DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DIM ."   " .N ."  free sectors" RESET-COLOR CR
    \ detail pane
    SCR-SEL @ -1 <> SCR-SEL @ STOR-N @ < AND IF
        W.HBAR
        SCR-SEL @ FIND-NTH-ACTIVE DUP -1 <> IF
            ."   Name  : " DUP DIRENT .ZSTR CR
            ."   Type  : " DUP DIRENT DE.TYPE .FTYPE CR
            ."   Size  : " DUP DIRENT DE.USED .N ."  bytes" CR
            ."   Start : sector " DUP DIRENT DE.SEC .N CR
            ."   Count : " DIRENT DE.COUNT .N ."  sectors" CR
        ELSE DROP THEN
    THEN ;

: SCR-STORAGE  ( -- )
    S" Storage" W.TITLE
    ['] .STOR-BODY W.CUSTOM ;

\ ── Screen 9: Cores ──
: .CORES-BODY  ( -- )
    NCORES 1 <= IF
        S" Single-core mode -- no secondary cores available." W.LINE
    ELSE
        NCORES ['] .CORE-ROW W.LIST
        S" Multicore Words" W.SECTION
        S" xt core CORE-RUN    Dispatch work to core" W.LINE
        S" core CORE-WAIT      Wait for core to finish" W.LINE
        S" BARRIER             Sync all secondary cores" W.LINE
        S" pipe P.RUN-PAR      Parallel pipeline execute" W.LINE
        S" n LOCK / n UNLOCK   Spinlock operations" W.LINE
    THEN ;

: SCR-CORES  ( -- )
    NCORES S" Cores" W.TITLE-N
    ['] .CORES-BODY W.CUSTOM ;

\ ── Home subscreens ──

: SCR-HOME-OVERVIEW  ( -- )  SCR-HOME ;

: .HOME-MEM-BUFS  ( -- )
    BUF-COUNT @ 0 DO
        ."      " I .N ."  "
        I BUF-NTH DUP B.WIDTH .N ." x" B.LEN .N CR
    LOOP ;

: SCR-HOME-MEMORY  ( -- )
    S" Memory Detail" W.TITLE
    HERE             S" HERE"       W.KV
    65536 HERE -     S" Free dict"  W.KV
    HEAP-INIT @ S" initialized" S" not initialized" S" Heap" W.FLAG-2
    HEAP-INIT @ IF HEAP-BASE @ S" Heap base" W.KV THEN
    S" Stack" W.SECTION
    DEPTH            S" SP depth"   W.KV
    S" Buffers memory" W.SECTION
    BUF-COUNT @      S" Count"      W.KV
    ['] .HOME-MEM-BUFS W.CUSTOM ;

: SCR-HOME-NET  ( -- )
    S" Network Status" W.TITLE
    NET-RX? S" frame waiting" S" idle" S" NIC state" W.FLAG-2
    PORT-COUNT @     S" Ports"      W.KV
    PORT-RX @        S" RX count"   W.KV
    PORT-DROP @      S" Drops"      W.KV
    S" Port Bindings" W.SECTION
    PORT-COUNT @ ['] .PORT-ROW W.LIST ;

\ ── Buffer subscreens ──

: SCR-BUF-LIST  ( -- )  SCR-BUFFERS ;

VARIABLE _SRAW
VARIABLE _SREC
VARIABLE _STIL
VARIABLE _SBIT

: .BSTATS-BODY  ( -- )
    BUF-COUNT @ 0= IF EXIT THEN
    0 _SRAW !  0 _SREC !  0 _STIL !  0 _SBIT !
    BUF-COUNT @ 0 DO
        I BUF-NTH B.TYPE
        DUP 0 = IF 1 _SRAW +! THEN
        DUP 1 = IF 1 _SREC +! THEN
        DUP 2 = IF 1 _STIL +! THEN
        3 = IF 1 _SBIT +! THEN
    LOOP
    S" By Type" W.SECTION
    _SRAW @ S" Raw"    W.KV
    _SREC @ S" Record" W.KV
    _STIL @ S" Tile"   W.KV
    _SBIT @ S" Bitmap" W.KV ;

: SCR-BUF-STATS  ( -- )
    S" Buffer Statistics" W.TITLE
    BUF-COUNT @  S" Total buffers" W.KV
    ['] .BSTATS-BODY W.CUSTOM ;

\ ---- Screen label words (for registry) ----

: LBL-HOME  ." Home" ;
: LBL-BUFS  ." Bufs" ;
: LBL-KERN  ." Kern" ;
: LBL-PIPE  ." Pipe" ;
: LBL-TASK  ." Task" ;
: LBL-HELP  ." Help" ;
: LBL-DOCS  ." Docs" ;
: LBL-STOR  ." Stor" ;
: LBL-CORE  ." Core" ;

\ ---- Subscreen label words ----

: LBL-OVERVIEW  ." Overview" ;
: LBL-MEMORY    ." Memory" ;
: LBL-NET       ." Network" ;
: LBL-BLIST     ." List" ;
: LBL-BSTATS    ." Stats" ;

\ -- Screen dispatch (registry-based) --
: RENDER-SCREEN  ( -- )
    PAGE SCREEN-HEADER
    SCREEN-ID @ DUP 1 < OVER NSCREENS @ > OR IF DROP 1 THEN
    1-                                          \ 0-based index
    DUP CELLS SUB-COUNTS + @ DUP 0> IF
        DROP SUB-TABS                           \ show sub-tab bar
        SCREEN-ID @ 1- MAX-SUBS * SUBSCREEN-ID @ +
        CELLS SUB-XT + @ DUP 0<> IF
            ['] EXECUTE CATCH IF
                RESET-COLOR CR ." [screen error]" CR
            THEN
        ELSE DROP ." [no renderer]" CR THEN
    ELSE
        DROP
        CELLS SCR-XT + @ DUP 0<> IF
            ['] EXECUTE CATCH IF
                RESET-COLOR CR ." [screen error]" CR
            THEN
        ELSE DROP ." [no renderer]" CR THEN
    THEN
    CR SCREEN-FOOTER ;

\ -- Screen switch helper (registry-based) --
: SWITCH-SCREEN  ( n -- )
    DUP SCREEN-ID !
    1- CELLS SCR-FLAGS + @ 1 AND
    IF 0 ELSE -1 THEN SCR-SEL !
    0 SCR-MAX !
    0 SUBSCREEN-ID !
    RENDER-SCREEN ;

\ ---- Screen-specific key handlers ----

: TASK-KEYS  ( c -- consumed )
    DUP 107 = IF DROP                         \ 'k' = kill task
        SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
            CELLS TASK-TABLE + @ KILL
            RENDER-SCREEN
        ELSE DROP THEN -1 EXIT
    THEN
    DUP 115 = IF DROP                         \ 's' = restart task
        SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
            CELLS TASK-TABLE + @ RESTART
            RENDER-SCREEN
        ELSE DROP THEN -1 EXIT
    THEN
    DROP 0 ;

\ -- Per-screen key dispatch (returns consumed flag) --
: CALL-SCREEN-KEY  ( c -- c consumed )
    SCREEN-ID @ 1- CELLS SCR-KEY-XT + @ DUP 0<> IF
        OVER SWAP ['] EXECUTE CATCH IF DROP 0 THEN
    ELSE
        DROP 0              \ no handler -> not consumed
    THEN ;

\ -- Activate selected item --
: DO-SELECT  ( -- )
    SCREEN-ID @ 1- CELLS SCR-ACT-XT + @ DUP 0<> IF
        ['] EXECUTE CATCH IF
            RESET-COLOR CR ." [action error]" CR
        THEN
    ELSE
        DROP
        SCREEN-ID @ 7 = IF SCR-SEL @ SHOW-NTH-DOC THEN   \ legacy fallback
    THEN ;

\ -- Event loop: poll KEY?, dispatch on keypress (registry-based) --
: HANDLE-KEY  ( c -- )
    \ ESC sequence: consume CSI arrow keys for subscreen navigation
    DUP 27 = IF DROP                               \ ESC (0x1B)
        KEY? IF
            KEY DUP 91 = IF                        \ '[' = CSI prefix
                DROP KEY                           \ read direction byte
                DUP 68 = IF DROP                   \ Left arrow → prev sub
                    SCREEN-SUBS 0> IF
                        SUBSCREEN-ID @ 1- DUP 0< IF
                            DROP SCREEN-SUBS 1-
                        THEN SUBSCREEN-ID !
                        RENDER-SCREEN
                    THEN EXIT
                THEN
                DUP 67 = IF DROP                   \ Right arrow → next sub
                    SCREEN-SUBS 0> IF
                        SUBSCREEN-ID @ 1+ DUP SCREEN-SUBS >= IF
                            DROP 0
                        THEN SUBSCREEN-ID !
                        RENDER-SCREEN
                    THEN EXIT
                THEN
                DROP EXIT                          \ Up/Down/other: ignore
            ELSE DROP THEN                         \ non-CSI: consume & ignore
        THEN EXIT                                  \ bare ESC: ignore
    THEN
    \ Per-screen custom key handler (priority: checked first)
    CALL-SCREEN-KEY IF DROP EXIT THEN
    \ Digit keys 0-9: switch to screen 1-10 (key '0'=48...'9'=57)
    DUP 48 >= OVER 57 <= AND IF
        DUP 48 - DUP NSCREENS @ < IF
            1+ SWITCH-SCREEN DROP EXIT
        ELSE DROP THEN
    THEN
    \ Hex keys a-f: switch to screen 11-16 (key 'a'=97...'f'=102)
    DUP 97 >= OVER 102 <= AND IF
        DUP 97 - 10 + DUP NSCREENS @ < IF
            1+ SWITCH-SCREEN DROP EXIT
        ELSE DROP THEN
    THEN
    DUP 113 = IF DROP 0 SCREEN-RUN ! EXIT THEN   \ 'q'
    DUP 114 = IF DROP RENDER-SCREEN EXIT THEN     \ 'r'
    DUP 65 = IF DROP                               \ 'A' = toggle auto-refresh
        AUTO-REFRESH @ IF 0 ELSE -1 THEN AUTO-REFRESH !
        RENDER-SCREEN EXIT
    THEN
    \ Subscreen navigation: '[' = prev sub, ']' = next sub
    DUP 91 = IF DROP                               \ '['
        SCREEN-SUBS 0> IF
            SUBSCREEN-ID @ 1- DUP 0< IF
                DROP SCREEN-SUBS 1-
            THEN SUBSCREEN-ID !
            RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 93 = IF DROP                               \ ']'
        SCREEN-SUBS 0> IF
            SUBSCREEN-ID @ 1+ DUP SCREEN-SUBS >= IF
                DROP 0
            THEN SUBSCREEN-ID !
            RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 110 = IF DROP                              \ 'n' = next item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1+ DUP SCR-MAX @ >= IF DROP 0 THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 112 = IF DROP                              \ 'p' = prev item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1- DUP 0< IF
                DROP SCR-MAX @ 1- DUP 0< IF DROP 0 THEN
            THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 13 = OVER 32 = OR IF DROP                  \ ENTER / SPACE = activate
        SCREEN-SELECTABLE? IF
            SCR-SEL @ -1 <> IF
                DO-SELECT RENDER-SCREEN
            THEN
        THEN EXIT
    THEN
    DROP ;

\ -- §9.10  Screen registration --

\ Register screens (order = display order; index 0..N-1, key = hex 0..F)
' SCR-HOME     ' LBL-HOME 0 REGISTER-SCREEN DROP  \ [0]Home
' SCR-BUFFERS  ' LBL-BUFS 1 REGISTER-SCREEN DROP  \ [1]Bufs  (selectable)
' SCR-KERNELS  ' LBL-KERN 0 REGISTER-SCREEN DROP  \ [2]Kern
' SCR-PIPES    ' LBL-PIPE 0 REGISTER-SCREEN DROP  \ [3]Pipe
' SCR-TASKS    ' LBL-TASK 1 REGISTER-SCREEN DROP  \ [4]Task  (selectable)
' SCR-HELP     ' LBL-HELP 0 REGISTER-SCREEN DROP  \ [5]Help
' SCR-DOCS     ' LBL-DOCS 1 REGISTER-SCREEN DROP  \ [6]Docs  (selectable)
' SCR-STORAGE  ' LBL-STOR 1 REGISTER-SCREEN DROP  \ [7]Stor  (selectable)
' SCR-CORES    ' LBL-CORE 0 REGISTER-SCREEN DROP  \ [8]Core

\ Per-screen custom key handlers
' TASK-KEYS  4 SET-SCREEN-KEYS    \ screen [5]Task (index 4) gets k/s keys

\ Home subscreens:  Overview | Memory | Network
' SCR-HOME-OVERVIEW  ' LBL-OVERVIEW 0 ADD-SUBSCREEN
' SCR-HOME-MEMORY    ' LBL-MEMORY   0 ADD-SUBSCREEN
' SCR-HOME-NET       ' LBL-NET      0 ADD-SUBSCREEN

\ Buffers subscreens: List | Stats
' SCR-BUF-LIST       ' LBL-BLIST   1 ADD-SUBSCREEN
' SCR-BUF-STATS      ' LBL-BSTATS  1 ADD-SUBSCREEN

\ -- TUI event loop (factored for reuse) --
: SCREEN-LOOP  ( -- )
    1 SCREEN-RUN !
    CYCLES REFRESH-LAST !
    BEGIN
        KEY? IF KEY HANDLE-KEY THEN
        AUTO-REFRESH @ IF
            CYCLES REFRESH-LAST @ - 5000000 > IF
                CYCLES REFRESH-LAST !
                RENDER-SCREEN
            THEN
        THEN
        SCREEN-RUN @
    0= UNTIL
    PAGE
    ."  Returned to REPL."  CR ;

\ -- Main TUI entry point --
: SCREENS  ( -- )
    1 SCREEN-ID !
    -1 SCR-SEL !  0 SCR-MAX !
    0 SUBSCREEN-ID !
    RENDER-SCREEN
    SCREEN-LOOP ;

\ -- Enter TUI at screen n  (e.g. 9 SCREEN → [8]Core) --
: SCREEN  ( n -- )
    SWITCH-SCREEN  SCREEN-LOOP ;

\ =====================================================================
\  §10  Data Ports — Structures and Binding
\ =====================================================================
\
\  Frame protocol (6-byte header + payload, rides inside UDP on port 9000):
\    +0  u8   SRC_ID       source identifier (0-255)
\    +1  u8   DTYPE        data type (0=raw 1=u8 2=u16 3=u64 4=text 5=cmd)
\    +2  u16  SEQ          sequence number (LE)
\    +4  u16  PAYLOAD_LEN  payload byte count (LE)
\    +6  ...  PAYLOAD      data bytes
\
\  This section defines data structures and port binding only.
\  Transport words (POLL, INGEST, PORT-SEND) are provided by
\  networking.f so the Bank 0 core stays transport-independent.
\
\  Python side: data_sources.py provides SineSource, CounterSource, etc.
\  that inject frames wrapped in ETH+IP+UDP via system.nic.inject_frame().

\ -- Constants --
6 CONSTANT /FRAME-HDR

\ -- Data-port protocol receive buffer (1500 bytes, not a raw L2 frame) --
VARIABLE FRAME-BUF  1499 ALLOT

\ -- Port table: 256 slots, each holds a buffer descriptor addr (0=unbound) --
VARIABLE PORT-TABLE  255 CELLS ALLOT
PORT-TABLE 256 CELLS 0 FILL

\ -- Port registry count (defined early, before §9 TUI) --
\ -- PORT-COUNT, PORT-RX, PORT-DROP already defined before §9 --

\ -- Stats (already defined before §9) --

\ -- Temp for routing --
VARIABLE ROUTE-BUF

\ -- Port binding --
: PORT-SLOT  ( id -- addr )     CELLS PORT-TABLE + ;
: PORT!      ( buf id -- )      DUP PORT-SLOT @ 0= IF 1 PORT-COUNT +! THEN
                                PORT-SLOT ! ;
: PORT@      ( id -- buf|0 )    PORT-SLOT @ ;
: UNPORT     ( id -- )          DUP PORT@ 0<> IF -1 PORT-COUNT +! THEN
                                0 SWAP PORT-SLOT ! ;

\ -- NIC convenience (defined early, before §9 TUI) --

\ -- Frame header accessors (valid after POLL/RECV-FRAME fills FRAME-BUF) --
: FRAME-SRC   ( -- id )    FRAME-BUF C@ ;
: FRAME-TYPE  ( -- type )  FRAME-BUF 1 + C@ ;
: FRAME-SEQ   ( -- seq )   FRAME-BUF 2 + C@  FRAME-BUF 3 + C@ 256 * + ;
: FRAME-LEN   ( -- len )   FRAME-BUF 4 + C@  FRAME-BUF 5 + C@ 256 * + ;
: FRAME-DATA  ( -- addr )  FRAME-BUF /FRAME-HDR + ;

\ -- (RECV-FRAME, ROUTE-FRAME, POLL, INGEST defined in §10.1 after §16) --

\ -- Debug: print last received frame header --
: .FRAME  ( -- )
    ."  src=" FRAME-SRC .
    ."  type=" FRAME-TYPE .
    ."  seq=" FRAME-SEQ .
    ."  len=" FRAME-LEN . CR ;

\ -- List bound ports --
: PORTS  ( -- )
    ."  --- Ports (" PORT-COUNT @ . ."  ) ---" CR
    256 0 DO
        I PORT@ DUP 0<> IF
            ."    src=" I . ."   -> buf @" . CR
        ELSE DROP THEN
    LOOP
    ."    rx=" PORT-RX @ . ."  drop=" PORT-DROP @ . CR ;

\ -- Port stats one-liner --
: PORT-STATS  ( -- )
    ."  ports=" PORT-COUNT @ .
    ."  rx=" PORT-RX @ .
    ."  drop=" PORT-DROP @ . ;

\ =====================================================================
\  §11  Benchmarking
\ =====================================================================
\
\  BENCH and .BENCH are defined in §6 (before P.BENCH needs them).
\  This section is kept as a placeholder for additional benchmark words.

\ =====================================================================
\  §12  Dashboard
\ =====================================================================

: HRULE  ( -- )  60 0 DO 45 EMIT LOOP CR ;
: THIN-RULE  ( -- )  40 0 DO 46 EMIT LOOP CR ;

\ -- Unified memory report --
: .MEM  ( -- )
    ."   Bank 0 (System RAM):" CR
    ."     HERE  = " HERE . CR
    ."     Free  = " SP@ HERE - . ."  bytes (to data stack)" CR
    .HEAP
    .HBW
    .XMEM
    ."   Buffers: " BUF-COUNT @ . CR
    ."   Stack depth: " DEPTH . CR ;

\ MEM-REPORT ( -- )
\   Unified memory status with heap integrity check.
: MEM-REPORT  ( -- )
    CR ." === Memory Report ===" CR
    .HEAP
    .HBW
    .XMEM
    ."  Dict: HERE=" HERE .
    ."  SP=" SP@ .
    ."  gap=" SP@ HERE - . ."  bytes" CR
    ."  Heap integrity: "
    HEAP-VERIFY IF ." OK" ELSE ." CORRUPT" THEN CR ;

\ -- Dashboard --
: DASHBOARD ( -- )
    CR HRULE
    ."   KDOS v1.1 — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR ."   Cores: " NCORES .
    NCORES 1 > IF ."  (multicore)" ELSE ."  (single-core)" THEN CR
    CR DISK-INFO
    CR BUFFERS
    CR KERNELS
    CR PIPES
    CR TASKS
    CR FILES
    CR PORTS
    CR .PERF
    CR HRULE ;

\ -- Status: quick one-liner --
: STATUS ( -- )
    ."  KDOS v1.1 | cores=" NCORES .
    ."  bufs=" BUF-COUNT @ .
    ."  kerns=" KERN-COUNT @ .
    ."  pipes=" PIPE-COUNT @ .
    ."  tasks=" TASK-COUNT @ .
    ."  files=" FILE-COUNT @ .
    ."  ports=" PORT-COUNT @ .
    ."  disk=" DISK? IF ."  yes" ELSE ."  no" THEN
    ."   HERE=" HERE . CR ;

\ =====================================================================
\  §13  Help System
\ =====================================================================

\ -- Word-specific help lookup --
\ HELP-WORD ( -- )  look up the word already in NAMEBUF
\   1. Check if it exists in the dictionary
\   2. Check for matching doc file on disk
\   3. Suggest WORDS-LIKE for related words

VARIABLE HW-FOUND
VARIABLE HW-CSTR    15 ALLOT    \ counted string for FIND

: HELP-WORD  ( -- )
    CR
    \ Build counted string for FIND from NAMEBUF
    PN-LEN @ HW-CSTR C!
    NAMEBUF HW-CSTR 1+ PN-LEN @ CMOVE
    \ Try to find the word in the dictionary
    HW-CSTR FIND SWAP DROP    ( flag: 0=miss, 1/-1=found )
    DUP HW-FOUND !
    0<> IF
        2 FG ."  Found: " RESET-COLOR
        NAMEBUF .ZSTR ."   — defined in dictionary" CR
    ELSE
        1 FG ."  Not found: " RESET-COLOR
        NAMEBUF .ZSTR ."   — not in dictionary" CR
    THEN
    \ Check for matching doc on disk
    FS-OK @ IF
        FIND-BY-NAME DUP -1 <> IF
            CR ."   Documentation available:" CR
            DUP DIRENT DE.TYPE .FTYPE ."   file: "
            DIRENT .ZSTR CR
            ."   Use: DOC " NAMEBUF .ZSTR ."   or  DESCRIBE " NAMEBUF .ZSTR CR
        ELSE
            DROP
        THEN
    THEN
    \ Show related words
    CR ."   Related words:" CR ."   "
    0                                    \ match count
    LATEST
    BEGIN DUP WHILE
        DUP ENTRY>NAME                   ( count entry na nl )
        NAMEBUF PN-LEN @                 ( count entry na nl pa pl )
        2OVER ICONTAINS? IF
            2 PICK 10 < IF               \ limit output to 10 matches
                TYPE SPACE
                ROT 1+ -ROT
            ELSE
                2DROP
            THEN
        ELSE
            2DROP
        THEN
        ENTRY>LINK
    REPEAT
    DROP
    DUP 0= IF ."  (none)" THEN
    CR ."  (" . ."  related)" CR ;

\ -- Full reference --
: .HELP-ALL  ( -- )
    CR HRULE
    ."   KDOS v1.1 — Quick Reference" CR
    HRULE
    CR ."   BUFFER WORDS:" CR
    ."     0 1 256 BUFFER name   Create 256-byte raw buffer" CR
    ."     buf B.INFO             Show buffer descriptor" CR
    ."     buf B.PREVIEW          Hex dump first tile" CR
    ."     byte buf B.FILL        Fill buffer with byte" CR
    ."     buf B.ZERO             Zero buffer" CR
    ."     buf B.SUM              Sum all bytes (via tile engine)" CR
    ."     buf B.MIN              Minimum byte (via tile engine)" CR
    ."     buf B.MAX              Maximum byte (via tile engine)" CR
    ."     a b c B.ADD            Element-wise add a+b -> c" CR
    ."     a b c B.SUB            Element-wise sub a-b -> c" CR
    ."     n buf B.SCALE          Multiply each byte by n" CR
    ."     BUFFERS                List all buffers" CR
    CR ."   KERNEL WORDS:" CR
    ."     1 1 2 0 KERNEL name   Register kernel metadata" CR
    ."     desc K.INFO            Show kernel descriptor" CR
    ."     KERNELS                List all kernels" CR
    CR ."   SAMPLE KERNELS:" CR
    ."     buf kzero              Zero a buffer" CR
    ."     byte buf kfill         Fill a buffer" CR
    ."     a b c kadd             Add two buffers" CR
    ."     buf ksum               Sum buffer -> stack" CR
    ."     buf kstats             Sum, min, max -> stack" CR
    ."     n buf kscale           Scale buffer by n" CR
    ."     n buf kthresh          Threshold: <n->0, >=n->255" CR
    CR ."   ADVANCED KERNELS:" CR
    ."     lo hi buf kclamp       Clamp bytes to [lo,hi]" CR
    ."     w buf kavg             Moving average (window w)" CR
    ."     buf khistogram         256-bin histogram -> hist-bins" CR
    ."     v HIST@                Query histogram bin v" CR
    ."     .HIST                  Print non-zero histogram bins" CR
    ."     src dst kdelta         Delta encode src -> dst" CR
    ."     buf knorm              Normalize to full 0-255 range" CR
    ."     th src dst kpeak       Peak detect (thresh th)" CR
    ."     buf krms-buf           RMS of buffer -> stack" CR
    ."     a b kcorrelate         Dot product (tile engine)" CR
    ."     c0 c1 c2 buf kconvolve3  3-tap FIR convolution" CR
    ."     buf kinvert            Bitwise invert (255-val)" CR
    ."     val buf kcount         Count matching bytes" CR
    CR ."   PIPELINE WORDS:" CR
    ."     3 PIPELINE name        Create 3-step pipeline" CR
    ."     ' word pipe P.ADD      Append step to pipeline" CR
    ."     pipe P.RUN             Execute all steps" CR
    ."     pipe P.BENCH           Time each step" CR
    ."     pipe P.INFO            Show pipeline descriptor" CR
    ."     pipe P.CLEAR           Reset pipeline" CR
    ."     PIPES                  List all pipelines" CR
    CR ."   STORAGE WORDS:" CR
    ."     DISK?                  Is storage present?" CR
    ."     DISK-INFO              Print storage status" CR
    ."     buf sec B.SAVE         Save buffer to disk" CR
    ."     buf sec B.LOAD         Load buffer from disk" CR
    CR ."   MP64FS FILE SYSTEM:" CR
    ."     FORMAT                 Format disk with MP64FS" CR
    ."     FS-LOAD                Load FS from disk into RAM" CR
    ."     FS-SYNC                Write FS changes to disk" CR
    ."     FS-FREE                Show free disk space" CR
    ."     DIR                    List files on disk" CR
    ."     CATALOG                Detailed file listing" CR
    ."     8 2 MKFILE name        Create file (8 secs, type 2)" CR
    ."     RMFILE name            Delete file from disk" CR
    ."     RENAME old new         Rename a file" CR
    ."     CAT name               Print file to terminal" CR
    ."     OPEN name              Open file -> fdesc" CR
    ."     f FFLUSH               Write metadata to disk" CR
    ."     buf SAVE-BUFFER name   Save buffer to file" CR
    CR ."   FILE I/O:" CR
    ."     10 8 FILE name         Create manual file (legacy)" CR
    ."     addr len f FWRITE      Write bytes (advances cursor)" CR
    ."     addr len f FREAD       Read bytes (advances cursor)" CR
    ."     pos f FSEEK            Set file cursor" CR
    ."     f FREWIND              Reset cursor to 0" CR
    ."     n f FTRUNCATE          Set file size (clamps cursor)" CR
    ."     f FSIZE / f F.INFO     File size / info" CR
    ."     FILES                  List legacy files" CR
    CR ."   SCHEDULER WORDS:" CR
    ."     ' word 0 TASK name     Create named task (xt pri)" CR
    ."     xt SPAWN               Spawn anonymous task" CR
    ."     xt BG                  Spawn + run scheduler" CR
    ."     SCHEDULE               Run all ready tasks" CR
    ."     YIELD                  Cooperative yield" CR
    ."     tdesc KILL             Cancel task" CR
    ."     tdesc RESTART          Reset done task to ready" CR
    ."     PREEMPT-ON             Enable timer preemption" CR
    ."     PREEMPT-OFF            Disable timer preemption" CR
    ."     TASKS                  List all tasks" CR
    CR ."   MULTICORE WORDS:" CR
    ."     COREID                 Push current core ID" CR
    ."     NCORES                 Push number of hardware cores" CR
    ."     xt core CORE-RUN       Dispatch XT to secondary core" CR
    ."     core CORE-WAIT         Wait for core to finish" CR
    ."     ALL-CORES-WAIT         Wait for all secondary cores" CR
    ."     BARRIER                Synchronize all cores" CR
    ."     n LOCK                 Acquire spinlock n (busy-wait)" CR
    ."     n UNLOCK               Release spinlock n" CR
    ."     CORES                  Show per-core status" CR
    ."     pipe P.RUN-PAR         Run pipeline in parallel" CR
    ."     pipe P.BENCH-PAR       Benchmark parallel pipeline" CR
    CR ."   DATA PORT WORDS:" CR
    ."     buf id PORT!           Bind source id to buffer" CR
    ."     id UNPORT              Unbind source" CR
    ."     POLL                   Receive & route one frame" CR
    ."     n INGEST               Receive & route n frames" CR
    ."     NET-RX?                Is a NIC frame waiting?" CR
    ."     PORTS                  List port bindings" CR
    ."     .FRAME                 Show last frame header" CR
    CR ."   SCREENS & TOOLS:" CR
    ."     SCREENS                Interactive TUI (1-9, n/p, a, q, r)" CR
    ."     DASHBOARD              Full system overview" CR
    ."     STATUS                 Quick status line" CR
    ."     ' word BENCH           Time word, leave cycles on stack" CR
    ."     ' word .BENCH          Time word and print cycles" CR
    ."     HELP                   Full quick reference" CR
    ."     HELP <word>            Look up a specific word" CR
    CR ."   DOCUMENTATION:" CR
    ."     TOPICS                 List available doc topics" CR
    ."     LESSONS                List available tutorials" CR
    ."     DOC <topic>            Page through documentation" CR
    ."     DESCRIBE <topic>       Show topic by name" CR
    ."     TUTORIAL <name>        Interactive lesson" CR
    CR ."   DICTIONARY SEARCH:" CR
    ."     WORDS-LIKE <pat>       Find words containing pattern" CR
    ."     APROPOS <pat>          Alias for WORDS-LIKE" CR
    ."     n .RECENT              Show last n defined words" CR
    ."     LATEST                 Push most-recent dict entry addr" CR
    ."     entry ENTRY>NAME       Get name (addr len) from entry" CR
    ."     entry ENTRY>LINK       Follow dict link to next entry" CR
    CR ."   PIPELINE BUNDLES:" CR
    ."     1 BDL-BEGIN             Start bundle (version 1)" CR
    ."     0 1 256 BDL-BUF name   Declare buffer in bundle" CR
    ."     1 1 2 1 BDL-KERN name  Declare kernel in bundle" CR
    ."     3 BDL-PIPE name         Declare pipeline in bundle" CR
    ."     0 50000 3 BDL-SCHED    Set schedule (pipe int flags)" CR
    ."     0 0 3 BDL-POLICY       Set policy (perms ret exp)" CR
    ."     1 255 BDL-SCREEN        Set screen (default mask)" CR
    ."     BDL-END                 Finalize bundle" CR
    ."     BUNDLE-LOAD name       Load bundle from disk" CR
    ."     BUNDLE-INFO name       Inspect bundle (dry run)" CR
    ."     .BUNDLE                Show current bundle state" CR
    CR ."   STACK & DIAGNOSTICS:" CR
    ."     n NEEDS                Abort if stack has < n items" CR
    ."     flag ASSERT            Abort if flag is false" CR
    ."     .DEPTH                 Show current stack depth" CR
    CR HRULE ;

\ -- Dispatching HELP --
\ HELP ( "name" | -- )
\   With no argument: show full reference
\   With a word name: look up word-specific info
: HELP  ( -- )
    PARSE-NAME PN-LEN @ 0= IF
        .HELP-ALL
    ELSE
        HELP-WORD
    THEN ;

\ =====================================================================
\  §15  Pipeline Bundles
\ =====================================================================
\
\  A Pipeline Bundle is a versioned, declarative file (type 7) that
\  packages a complete pipeline configuration in a single artifact:
\
\    - Buffer schemas       (type, width, length)
\    - Kernel registrations (n_in, n_out, footprint, flags)
\    - Pipeline structures  (capacity)
\    - Scheduling config    (pipe index, interval, auto/repeat flags)
\    - Policies             (permissions, retention, export)
\    - Dashboard config     (default screen, screen enable mask)
\
\  Bundles enable fleet deployment (commercial), change control
\  (industrial/civil), and easy sharing/install (personal).
\
\  A bundle file is valid Forth source using BDL-* declarative words.
\  Load with BUNDLE-LOAD, inspect with BUNDLE-INFO.
\
\  Example bundle file (stored as type 7 on disk):
\    1 BDL-BEGIN
\    0 1 256 BDL-BUF my-data
\    1 1 2 1 BDL-KERN my-kern
\    3 BDL-PIPE my-pipe
\    0 50000 3 BDL-SCHED
\    0 0 3 BDL-POLICY
\    1 255 BDL-SCREEN
\    BDL-END

7 CONSTANT FTYPE-BUNDLE

\ -- Bundle tracking state --
VARIABLE BDL-ACTIVE   0 BDL-ACTIVE !
VARIABLE BDL-DRY      0 BDL-DRY !
VARIABLE BDL-VER      0 BDL-VER !
VARIABLE BDL-NBUFS    0 BDL-NBUFS !
VARIABLE BDL-NKERNS   0 BDL-NKERNS !
VARIABLE BDL-NPIPES   0 BDL-NPIPES !
\ Schedule
VARIABLE BDL-SCHED-P  -1 BDL-SCHED-P !
VARIABLE BDL-SCHED-I  0 BDL-SCHED-I !
VARIABLE BDL-SCHED-F  0 BDL-SCHED-F !
\ Policy
VARIABLE BDL-POL-PERM 0 BDL-POL-PERM !
VARIABLE BDL-POL-RET  0 BDL-POL-RET !
VARIABLE BDL-POL-EXP  3 BDL-POL-EXP !
\ Dashboard
VARIABLE BDL-SCR-DEF  1 BDL-SCR-DEF !
VARIABLE BDL-SCR-MASK 255 BDL-SCR-MASK !

\ BDL-RESET ( -- ) clear all bundle tracking state
\   Does NOT touch BDL-DRY — that is managed by BUNDLE-LOAD / BUNDLE-INFO.
: BDL-RESET  ( -- )
    0 BDL-ACTIVE !  0 BDL-VER !
    0 BDL-NBUFS !  0 BDL-NKERNS !  0 BDL-NPIPES !
    -1 BDL-SCHED-P !  0 BDL-SCHED-I !  0 BDL-SCHED-F !
    0 BDL-POL-PERM !  0 BDL-POL-RET !  3 BDL-POL-EXP !
    1 BDL-SCR-DEF !  255 BDL-SCR-MASK ! ;

\ BDL-BEGIN ( version -- ) start a bundle declaration
: BDL-BEGIN  ( version -- )
    BDL-RESET  BDL-VER !  1 BDL-ACTIVE ! ;

\ BDL-BUF ( type width length "name" -- ) declare a buffer schema
: BDL-BUF  ( type width length "name" -- )
    BDL-DRY @ IF
        DROP DROP DROP
        BL WORD DROP
    ELSE
        BUFFER
    THEN
    1 BDL-NBUFS +! ;

\ BDL-KERN ( n_in n_out foot flags "name" -- ) declare a kernel
: BDL-KERN  ( n_in n_out foot flags "name" -- )
    BDL-DRY @ IF
        DROP DROP DROP DROP
        BL WORD DROP
    ELSE
        KERNEL
    THEN
    1 BDL-NKERNS +! ;

\ BDL-PIPE ( capacity "name" -- ) declare a pipeline
: BDL-PIPE  ( capacity "name" -- )
    BDL-DRY @ IF
        DROP
        BL WORD DROP
    ELSE
        PIPELINE
    THEN
    1 BDL-NPIPES +! ;

\ BDL-SCHED ( pipe-idx interval flags -- ) declare scheduling
\   flags: bit0=auto-start  bit1=repeat
: BDL-SCHED  ( pipe-idx interval flags -- )
    BDL-SCHED-F !  BDL-SCHED-I !  BDL-SCHED-P ! ;

\ BDL-POLICY ( permissions retention export -- ) declare policy
\   permissions: bit0=readonly  bit1=system
\   export:      bit0=nic-ok    bit1=disk-ok
: BDL-POLICY  ( permissions retention export -- )
    BDL-POL-EXP !  BDL-POL-RET !  BDL-POL-PERM ! ;

\ BDL-SCREEN ( default-screen screen-mask -- ) declare dashboard config
: BDL-SCREEN  ( default-screen screen-mask -- )
    BDL-SCR-MASK !  BDL-SCR-DEF ! ;

\ BDL-END ( -- ) finalize bundle, report summary
: BDL-END  ( -- )
    BDL-DRY @ IF
        ."  --- Bundle v" BDL-VER @ . ."  ---" CR
        ."    Buffers  : " BDL-NBUFS @ . CR
        ."    Kernels  : " BDL-NKERNS @ . CR
        ."    Pipelines: " BDL-NPIPES @ . CR
        BDL-SCHED-P @ -1 <> IF
            ."    Schedule : pipe " BDL-SCHED-P @ .
            ."   interval=" BDL-SCHED-I @ .
            BDL-SCHED-F @ 1 AND IF ."   auto" THEN
            BDL-SCHED-F @ 2 AND IF ."   repeat" THEN
            CR
        ELSE
            ."    Schedule : (none)" CR
        THEN
        ."    Policy   : perms=" BDL-POL-PERM @ .
        ."  ret=" BDL-POL-RET @ .
        ."  exp=" BDL-POL-EXP @ . CR
        ."    Screen   : default=" BDL-SCR-DEF @ .
        ."  mask=" BDL-SCR-MASK @ . CR
    ELSE
        \ Apply schedule interval to TIME-SLICE
        BDL-SCHED-P @ -1 <> IF
            BDL-SCHED-I @ TIME-SLICE !
        THEN
        \ Apply screen default
        BDL-SCR-DEF @ SCREEN-ID !
        \ Report
        CR ."  Bundle v" BDL-VER @ . ."  loaded: "
        BDL-NBUFS @ . ."  bufs "
        BDL-NKERNS @ . ."  kerns "
        BDL-NPIPES @ . ."  pipes" CR
    THEN
    0 BDL-ACTIVE ! ;

\ BUNDLE-LOAD ( "name" -- ) load and apply a bundle from disk
: BUNDLE-LOAD  ( "name" -- )
    0 BDL-DRY !  LOAD ;

\ BUNDLE-INFO ( "name" -- ) inspect a bundle without applying
: BUNDLE-INFO  ( "name" -- )
    1 BDL-DRY !  LOAD  0 BDL-DRY ! ;

\ .BUNDLE ( -- ) show current bundle state
: .BUNDLE  ( -- )
    ."  --- Current Bundle ---" CR
    BDL-VER @ 0= IF
        ."    (no bundle loaded)" CR EXIT
    THEN
    ."    Version  : " BDL-VER @ . CR
    ."    Buffers  : " BDL-NBUFS @ . CR
    ."    Kernels  : " BDL-NKERNS @ . CR
    ."    Pipelines: " BDL-NPIPES @ . CR
    ."    Schedule : "
    BDL-SCHED-P @ -1 = IF ."  none" ELSE
        ."  pipe " BDL-SCHED-P @ .
        ."  interval=" BDL-SCHED-I @ .
        BDL-SCHED-F @ 1 AND IF ."  [auto]" THEN
        BDL-SCHED-F @ 2 AND IF ."  [repeat]" THEN
    THEN CR
    ."    Policy   : "
    BDL-POL-PERM @ 1 AND IF ."  RO" THEN
    BDL-POL-PERM @ 2 AND IF ."  SYS" THEN
    ."  ret=" BDL-POL-RET @ .
    ."  exp=" BDL-POL-EXP @ . CR
    ."    Screen   : default=" BDL-SCR-DEF @ .
    ."  mask=" BDL-SCR-MASK @ . CR ;

\ =====================================================================
\  §18  Ring Buffer Primitives
\ =====================================================================
\
\  Lock-aware circular buffer for multi-core producer/consumer patterns.
\
\  Ring descriptor layout (7 cells = 56 bytes):
\    +0   elem-size   bytes per element
\    +8   capacity    max number of elements
\    +16  head        index of oldest element (read position)
\    +24  tail        index of next write position
\    +32  count       current number of elements
\    +40  lock#       spinlock number for atomicity
\    +48  data...     capacity × elem-size bytes

: RING  ( elem-size capacity "name" -- )
    HERE >R
    SWAP ,                      \ +0  elem-size
    DUP ,                       \ +8  capacity
    0 ,                         \ +16 head = 0
    0 ,                         \ +24 tail = 0
    0 ,                         \ +32 count = 0
    RING-LOCK ,                 \ +40 lock#
    R@ @ *                      \ capacity × elem-size
    ALLOT                       \ allot data area
    R> CONSTANT ;

\ --- Ring accessors ---
: RING.ESIZE  ( ring -- n )     @ ;
: RING.CAP    ( ring -- n )     8 + @ ;
: RING.HEAD   ( ring -- addr )  16 + ;
: RING.TAIL   ( ring -- addr )  24 + ;
: RING.COUNT  ( ring -- n )     32 + @ ;
: RING.LOCK   ( ring -- n )     40 + @ ;
: RING.DATA   ( ring -- addr )  48 + ;

: RING-FULL?  ( ring -- flag )  DUP RING.COUNT SWAP RING.CAP >= ;
: RING-EMPTY? ( ring -- flag )  RING.COUNT 0= ;
: RING-COUNT  ( ring -- n )     RING.COUNT ;

\ --- RING-PUSH ( elem-addr ring -- flag ) ---
\ Append element to tail.  Returns 0 if full, -1 on success.
VARIABLE _RP-RING

: RING-PUSH  ( elem-addr ring -- flag )
    DUP _RP-RING !
    DUP RING.LOCK LOCK
    DUP RING-FULL? IF
        2DROP 0
    ELSE
        >R
        \ dst = data + tail × esize
        R@ RING.TAIL @ R@ RING.ESIZE * R@ RING.DATA +
        R@ RING.ESIZE CMOVE
        \ tail = (tail + 1) % cap
        R@ RING.TAIL @ 1+ R@ RING.CAP MOD R@ RING.TAIL !
        \ count++
        1 R> 32 + +!
        -1
    THEN
    _RP-RING @ RING.LOCK UNLOCK ;

\ --- RING-POP ( elem-addr ring -- flag ) ---
\ Dequeue oldest element from head.  Returns 0 if empty, -1 on success.
: RING-POP  ( elem-addr ring -- flag )
    DUP _RP-RING !
    DUP RING.LOCK LOCK
    DUP RING-EMPTY? IF
        2DROP 0
    ELSE
        >R
        \ src = data + head × esize
        R@ RING.HEAD @ R@ RING.ESIZE * R@ RING.DATA +
        SWAP R@ RING.ESIZE CMOVE
        \ head = (head + 1) % cap
        R@ RING.HEAD @ 1+ R@ RING.CAP MOD R@ RING.HEAD !
        \ count--
        -1 R> 32 + +!
        -1
    THEN
    _RP-RING @ RING.LOCK UNLOCK ;

\ --- RING-PEEK ( idx ring -- elem-addr | 0 ) ---
\ Read element at index without consuming.  Lock-free.
: RING-PEEK  ( idx ring -- elem-addr | 0 )
    >R
    DUP R@ RING.COUNT >= IF
        DROP R> DROP 0
    ELSE
        R@ RING.HEAD @ + R@ RING.CAP MOD
        R@ RING.ESIZE * R> RING.DATA +
    THEN ;

\ =====================================================================
\  §19  Hash Table Primitives
\ =====================================================================
\
\  Open-addressing hash table with linear probing.
\  Uses CRC-32 for hashing.  Write operations are lock-protected;
\  reads (HT-GET, HT-EACH) are lock-free.
\
\  Hash table descriptor layout (5 cells + data = 40 + data bytes):
\    +0   keysize     bytes per key
\    +8   valsize     bytes per value
\    +16  slots       number of slots
\    +24  count       occupied slot count
\    +32  lock#       spinlock number
\    +40  data...     slots × (1 + keysize + valsize) bytes
\
\  Each slot:
\    [0]  flag        0 = empty, 1 = occupied, 2 = tombstone
\    [1..keysize]     key bytes
\    [1+keysize..]    value bytes

VARIABLE _HT-KSIZE
VARIABLE _HT-VSIZE
VARIABLE _HT-NSLOTS

: HASHTABLE  ( keysize valsize slots "name" -- )
    _HT-NSLOTS !  _HT-VSIZE !  _HT-KSIZE !
    HERE >R
    _HT-KSIZE @  ,              \ +0  keysize
    _HT-VSIZE @  ,              \ +8  valsize
    _HT-NSLOTS @ ,              \ +16 slots
    0 ,                          \ +24 count = 0
    HT-LOCK ,                    \ +32 lock#
    \ total data = slots × (1 + keysize + valsize)
    _HT-KSIZE @ _HT-VSIZE @ + 1+
    _HT-NSLOTS @ *
    DUP ALLOT                    \ allot data area
    R@ 40 + SWAP 0 FILL         \ zero-fill (all slots empty)
    R> CONSTANT ;

\ --- Hash table accessors ---
: HT.KSIZE  ( ht -- n )     @ ;
: HT.VSIZE  ( ht -- n )     8 + @ ;
: HT.SLOTS  ( ht -- n )     16 + @ ;
: HT.COUNT  ( ht -- n )     24 + @ ;
: HT.LOCK   ( ht -- n )     32 + @ ;
: HT.DATA   ( ht -- addr )  40 + ;
: HT.STRIDE ( ht -- n )     DUP @ SWAP 8 + @ + 1+ ;

\ --- HT-SLOT ( slot# ht -- slot-addr ) ---
: HT-SLOT   ( slot# ht -- addr )  TUCK HT.STRIDE * SWAP HT.DATA + ;

\ --- HT-HASH ( key-addr ht -- slot# ) ---
: HT-HASH   ( key-addr ht -- slot# )
    DUP >R HT.KSIZE CRC32-BUF R> HT.SLOTS MOD ;

\ --- Slot field helpers ---
: HT-KEY    ( slot-addr -- key-addr )   1+ ;
: HT-VAL    ( slot-addr ht -- val-addr )  HT.KSIZE 1+ + ;

\ --- HT-COUNT ( ht -- n ) ---
: HT-COUNT  ( ht -- n )  24 + @ ;

\ --- HT-PUT ( key-addr val-addr ht -- ) ---
\ Insert or update.  Lock-protected.
VARIABLE _HTP-KEY
VARIABLE _HTP-VAL
VARIABLE _HTP-HT

: HT-PUT  ( key-addr val-addr ht -- )
    DUP _HTP-HT !  DUP HT.LOCK LOCK
    DROP _HTP-VAL !  _HTP-KEY !
    _HTP-KEY @ _HTP-HT @ HT-HASH
    _HTP-HT @ HT.SLOTS 0 DO
        DUP _HTP-HT @ HT-SLOT
        DUP C@ 0= OVER C@ 2 = OR IF       \ empty or tombstone → insert
            1 OVER C!                       \ mark occupied
            DUP HT-KEY _HTP-KEY @ SWAP _HTP-HT @ HT.KSIZE CMOVE
            DUP _HTP-HT @ HT-VAL _HTP-VAL @ SWAP _HTP-HT @ HT.VSIZE CMOVE
            1 _HTP-HT @ 24 + +!            \ count++
            DROP DROP
            _HTP-HT @ HT.LOCK UNLOCK
            UNLOOP EXIT
        THEN
        DUP C@ 1 = IF                      \ occupied → check key match
            DUP HT-KEY _HTP-KEY @ _HTP-HT @ HT.KSIZE SAMESTR? IF
                DUP _HTP-HT @ HT-VAL _HTP-VAL @ SWAP _HTP-HT @ HT.VSIZE CMOVE
                DROP DROP
                _HTP-HT @ HT.LOCK UNLOCK
                UNLOOP EXIT
            THEN
        THEN
        DROP
        1+ _HTP-HT @ HT.SLOTS MOD
    LOOP
    DROP
    _HTP-HT @ HT.LOCK UNLOCK ;

\ --- HT-GET ( key-addr ht -- val-addr | 0 ) ---
\ Lookup key.  Lock-free.  Returns pointer to value or 0.
VARIABLE _HTG-KEY
VARIABLE _HTG-HT

: HT-GET  ( key-addr ht -- val-addr | 0 )
    _HTG-HT !  _HTG-KEY !
    _HTG-KEY @ _HTG-HT @ HT-HASH
    _HTG-HT @ HT.SLOTS 0 DO
        DUP _HTG-HT @ HT-SLOT
        DUP C@ 0= IF                       \ empty → not found
            2DROP 0 UNLOOP EXIT
        THEN
        DUP C@ 1 = IF                      \ occupied → check key
            DUP HT-KEY _HTG-KEY @ _HTG-HT @ HT.KSIZE SAMESTR? IF
                _HTG-HT @ HT-VAL
                NIP UNLOOP EXIT
            THEN
        THEN
        DROP                                \ skip tombstones
        1+ _HTG-HT @ HT.SLOTS MOD
    LOOP
    DROP 0 ;

\ --- HT-DEL ( key-addr ht -- flag ) ---
\ Remove entry.  Returns -1 if found and deleted, 0 if absent.
VARIABLE _HTD-KEY

: HT-DEL  ( key-addr ht -- flag )
    DUP _HTP-HT !  DUP HT.LOCK LOCK
    SWAP _HTD-KEY !  DROP
    _HTD-KEY @ _HTP-HT @ HT-HASH
    _HTP-HT @ HT.SLOTS 0 DO
        DUP _HTP-HT @ HT-SLOT
        DUP C@ 0= IF
            2DROP 0
            _HTP-HT @ HT.LOCK UNLOCK
            UNLOOP EXIT
        THEN
        DUP C@ 1 = IF
            DUP HT-KEY _HTD-KEY @ _HTP-HT @ HT.KSIZE SAMESTR? IF
                2 OVER C!                   \ tombstone
                -1 _HTP-HT @ 24 + +!       \ count--
                2DROP -1
                _HTP-HT @ HT.LOCK UNLOCK
                UNLOOP EXIT
            THEN
        THEN
        DROP
        1+ _HTP-HT @ HT.SLOTS MOD
    LOOP
    DROP 0
    _HTP-HT @ HT.LOCK UNLOCK ;

\ --- HT-EACH ( xt ht -- ) ---
\ Iterate occupied slots.  Calls xt with ( key-addr val-addr -- ).
VARIABLE _HTE-XT
VARIABLE _HTE-HT

: HT-EACH  ( xt ht -- )
    _HTE-HT !  _HTE-XT !
    _HTE-HT @ HT.SLOTS 0 DO
        I _HTE-HT @ HT-SLOT
        DUP C@ 1 = IF
            DUP HT-KEY
            OVER _HTE-HT @ HT-VAL
            _HTE-XT @ EXECUTE
        THEN
        DROP
    LOOP ;

\ =====================================================================
\  §20  Module System
\ =====================================================================
\
\  Pre-scan guard: before executing any line of a loaded file,
\  _MOD-PRESCAN scans the raw file buffer for a line beginning with
\  "PROVIDED".  If found, the module name is extracted and checked
\  against the hash table.  If already loaded, _MOD-LOAD-BODY skips
\  _LD-WALK entirely — zero lines are executed, zero side effects.
\
\  PROVIDED itself (when executed during _LD-WALK) simply registers
\  the module name in the hash table.  It is a plain marker now; all
\  guard logic lives in the pre-scan.
\
\  Because the hash key is always the canonical name declared by the
\  file's own PROVIDED line (not the caller's REQUIRE path), different
\  paths to the same file share the same key.
\
\  Uses a hash table (§19) for O(1) lookup.  24-byte key = module
\  name (zero-padded, matching NAMEBUF layout), 1-byte value.

24 1 128 HASHTABLE _MOD-HT

\ Scratch for the 1-byte "loaded" marker value.
CREATE _MOD-VAL  1 ALLOT
1 _MOD-VAL C!

\ One provisional PROVIDED key per active loader frame.  A module is marked
\ before execution to break mutual-REQUIRE cycles; if its walk throws, that
\ frame owns deletion of the incomplete mark before propagating the error.
_LD-MAXLVL 24 * XBUF _MOD-PENDING-NAMES
_LD-MAXLVL 8 * XBUF _MOD-PENDING-FLAGS
_MOD-PENDING-FLAGS _LD-MAXLVL 8 * 0 FILL

: _MOD-DEPTH-INDEX  ( -- n )
    _LD-SP @ _LD-FRAME / 1- ;

: _MOD-PENDING-NAME  ( -- addr )
    _MOD-DEPTH-INDEX 24 * _MOD-PENDING-NAMES + ;

: _MOD-PENDING-FLAG  ( -- addr )
    _MOD-DEPTH-INDEX 8 * _MOD-PENDING-FLAGS + ;

\ _MOD-MARK ( -- )  Mark NAMEBUF as a loaded module in _MOD-HT.
: _MOD-MARK  ( -- )
    NAMEBUF _MOD-VAL _MOD-HT HT-PUT ;

\ _MOD-LOADED? ( -- flag )  True if current NAMEBUF is in _MOD-HT.
: _MOD-LOADED?  ( -- flag )
    NAMEBUF _MOD-HT HT-GET 0<> ;

\ ── Pre-scan for PROVIDED ────────────────────────────────────────────
\  _MOD-PRESCAN scans the file buffer (LD-BUF / LD-SZ) line by line
\  looking for a line whose first non-whitespace token is "PROVIDED".
\  Comment lines (starting with '\') are skipped.
\  If found, the module name (the next whitespace-delimited word) is
\  copied into NAMEBUF and the word returns TRUE.
\  If no PROVIDED line exists, returns FALSE.  NAMEBUF is unchanged.

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

CREATE _PS-NBSAVE 24 ALLOT  \ save area for NAMEBUF across prescan

\ _PS-MATCH8? ( addr -- flag )  True if addr points to "PROVIDED"
\   (exactly 8 chars, case-sensitive).
: _PS-MATCH8?  ( addr -- flag )
    TRUE 8 0 DO
        OVER I + C@ _PS-TAG I + C@ <> IF
            DROP FALSE LEAVE
        THEN
    LOOP NIP ;

\ _PS-SKIP-WS ( addr rem -- addr' rem' )  Skip spaces/tabs.
: _PS-SKIP-WS  ( addr rem -- addr' rem' )
    BEGIN
        DUP 0> IF
            OVER C@ DUP 32 = SWAP 9 = OR
        ELSE FALSE THEN
    WHILE
        1- SWAP 1+ SWAP
    REPEAT ;

\ _PS-TOKEN-LEN ( addr rem -- len )  Length of next non-WS token.
: _PS-TOKEN-LEN  ( addr rem -- len )
    0                                ( addr rem len )
    BEGIN
        OVER 0> IF
            2 PICK OVER + C@ DUP 32 > SWAP 127 < AND   ( ... printable? )
        ELSE FALSE THEN
    WHILE
        1+ SWAP 1- SWAP
    REPEAT
    NIP NIP ;

\ _MOD-PRESCAN ( -- flag )  Scan LD-BUF/LD-SZ for PROVIDED line.
\   On TRUE: module name is in NAMEBUF (ready for _MOD-LOADED?).
\   On FALSE: no PROVIDED found; NAMEBUF is unchanged.
: _MOD-PRESCAN  ( -- flag )
    LD-BUF @ LD-SZ @                    ( ptr rem )
    BEGIN DUP 0> WHILE
        \ Find end of current line (newline or end of buffer)
        2DUP                             ( ptr rem ptr rem )
        0                                ( ptr rem ptr rem i )
        BEGIN
            DUP 2 PICK < IF
                2 PICK OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
            ELSE TRUE THEN
        UNTIL                            ( ptr rem ptr rem linelen )
        NIP NIP                          ( ptr rem linelen )
        \ Process this line: skip leading whitespace
        >R 2DUP R>                       ( ptr rem ptr rem linelen )
        DROP                             ( ptr rem lineptr linerem )
        _PS-SKIP-WS                      ( ptr rem lp' lr' )
        \ Skip comment lines (first non-ws char is '\')
        DUP 0> IF
            OVER C@ 92 = IF             \ backslash
                2DROP                    ( ptr rem )
            ELSE
                \ Check if first token is "PROVIDED" (8 chars)
                2DUP _PS-TOKEN-LEN       ( ptr rem lp lr toklen )
                8 = IF
                    OVER _PS-MATCH8? IF  ( ptr rem lp lr )
                        \ Found! Extract module name after "PROVIDED "
                        8 - SWAP 8 + SWAP  ( ptr rem name-area-ptr nar )
                        _PS-SKIP-WS        ( ptr rem np nr )
                        2DUP _PS-TOKEN-LEN ( ptr rem np nr namelen )
                        DUP 0= IF
                            \ PROVIDED with no argument — ignore
                            DROP 2DROP     ( ptr rem )
                        ELSE
                            \ Copy name into NAMEBUF
                            NAMEBUF 24 0 FILL
                            23 MIN         ( ptr rem np nr namelen' )
                            NIP            ( ptr rem np namelen' )
                            NAMEBUF SWAP CMOVE  ( ptr rem )
                            2DROP TRUE EXIT
                        THEN
                    ELSE
                        2DROP              ( ptr rem )
                    THEN
                ELSE
                    2DROP                  ( ptr rem )
                THEN
            THEN
        ELSE
            2DROP                          ( ptr rem )
        THEN
        \ Advance past line + newline
        2DUP                               ( ptr rem ptr rem )
        0                                  ( ptr rem ptr rem i )
        BEGIN
            DUP 2 PICK < IF
                2 PICK OVER + C@ 10 = IF TRUE ELSE 1+ FALSE THEN
            ELSE TRUE THEN
        UNTIL                              ( ptr rem ptr rem linelen )
        NIP NIP                            ( ptr rem linelen )
        1+ DUP >R
        NEGATE +                           ( ptr rem' )
        SWAP R> + SWAP                     ( ptr' rem' )
    REPEAT
    2DROP FALSE ;

\ PROVIDED ( "name" -- )  Register a module as loaded.
\   Simply marks the name in the hash table.  The pre-scan in
\   _MOD-LOAD-BODY handles the actual duplicate-load guard;
\   this word just does the registration when executed normally.
: PROVIDED  ( "name" -- )
    PARSE-NAME  _MOD-MARK ;

\ MODULE? ( "name" -- flag )  Test if a module is already loaded.
: MODULE?  ( "name" -- flag )
    PARSE-NAME  _MOD-LOADED? ;

: _MOD-ROLLBACK-PENDING  ( -- )
    _MOD-PENDING-FLAG @ IF
        _MOD-PENDING-NAME _MOD-HT HT-DEL DROP
        0 _MOD-PENDING-FLAG !
    THEN ;

: _MOD-WALK-GUARDED  ( -- )
    ['] _LD-WALK CATCH
    DUP IF
        >R
        _MOD-ROLLBACK-PENDING
        _LD-RELEASE
        R> THROW
    THEN
    DROP
    0 _MOD-PENDING-FLAG !
    _LD-RELEASE ;

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
    0 _MOD-PENDING-FLAG !
    LD-SZ !                              ( slot )
    \ Module source is a reclaimable loader allocation, not permanent XMEM.
    DUP _LD-SLOT-BYTES ALLOCATE IF
        2DROP ."  Module buffer allocation failed" CR
        _LD-RESTORE EXIT
    THEN
    LD-BUF !
    _LD-READ-SLOT
    \ Pre-scan: look for PROVIDED before executing anything.
    \ Save/restore NAMEBUF across prescan (NAMEBUF holds the
    \ filename we just loaded; prescan overwrites it with the
    \ PROVIDED argument if found).
    NAMEBUF _PS-NBSAVE 24 CMOVE         \ save NAMEBUF
    _MOD-PRESCAN IF
        \ PROVIDED line found — check if already loaded
        _MOD-LOADED? IF
            \ Already loaded — skip execution entirely
            _PS-NBSAVE NAMEBUF 24 CMOVE  \ restore NAMEBUF
            _LD-RELEASE EXIT
        THEN
        \ First time — pre-register NOW so that any mutual
        \ REQUIRE during _LD-WALK sees us as already loaded.
        \ NAMEBUF still holds the PROVIDED name from prescan.
        NAMEBUF _MOD-PENDING-NAME 24 CMOVE
        1 _MOD-PENDING-FLAG !
        _MOD-MARK
        _PS-NBSAVE NAMEBUF 24 CMOVE
    ELSE
        \ No PROVIDED found — restore NAMEBUF; load unconditionally
        _PS-NBSAVE NAMEBUF 24 CMOVE
    THEN
    _MOD-WALK-GUARDED ;

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
    PARSE-NAME
    FS-ENSURE                  \ load FS before path resolution
    _REQ-SAVE-CWD
    ['] _REQUIRE-BODY CATCH
    _REQ-RESTORE-CWD           \ restore before returning or rethrowing
    THROW ;

\ _MOD-SHOW ( key-addr val-addr -- )  Print one module name.
: _MOD-SHOW  ( key-addr val-addr -- )
    DROP ."   " .ZSTR CR ;

\ MODULES ( -- )  List all loaded modules.
: MODULES  ( -- )
    ."  Loaded modules:" CR
    ['] _MOD-SHOW _MOD-HT HT-EACH
    _MOD-HT HT-COUNT . ."  module(s)" CR ;

\ =====================================================================
\  §14  Startup
\ =====================================================================

CR HRULE
."   KDOS v1.1 — Kernel Dashboard OS" CR
HRULE
."  Type HELP for commands, HELP <word> for details."  CR
."  Type SCREENS for interactive TUI (or N SCREEN for screen N)."  CR
."  Type TOPICS or LESSONS for documentation."  CR
NCORES 1 > IF
    ."   Multicore: " NCORES . ."  cores available" CR
    ."   Use CORE-RUN, BARRIER, P.RUN-PAR for parallel work."  CR
THEN
DISK? IF FS-LOAD THEN

\ Force system heap initialisation before userland can confuse HEAP-SETUP.
\ DMA-ALLOCATE 16 bytes to trigger lazy HEAP-SETUP, then DMA-FREE.
\ Must use DMA- variants to target Bank 0 directly (ALLOCATE routes
\ to xmem when extended memory is present).
16 DMA-ALLOCATE DROP DMA-FREE

\ -- AUTOEXEC: run autoexec.f if present on disk --
\ Must use a colon definition because FSLOAD evaluates each line
\ independently — multi-line IF/THEN does not gate execution.
CREATE _AUTOEXEC-NAME
  97 C, 117 C, 116 C, 111 C, 101 C, 120 C, 101 C, 99 C, 46 C, 102 C,

: _AUTOEXEC-RUN  ( -- )
    FS-OK @ 0= IF EXIT THEN
    _AUTOEXEC-NAME NAMEBUF 10 CMOVE
    NAMEBUF 10 + 14 0 FILL
    FIND-BY-NAME -1 = IF EXIT THEN
    ."  Running autoexec.f..." CR
    _MOD-LOAD-BODY ;

_AUTOEXEC-RUN

\ JIT served its purpose — disable for interactive use.
JIT-OFF
CR
