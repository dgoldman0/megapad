\ =====================================================================
\  §1.1  Memory Allocator
\ =====================================================================
\
\  Bank-0 first-fit free-list allocator.  Each block has a 24-byte header:
\    +0   next    pointer to next free block (0 = end of list)
\    +8   size    usable bytes in this block (excludes header)
\    +16  magic   allocation canary (zero while free)
\
\  The private Bank-0 allocator returns an address past the header.  Its
\  paired free takes that address, backs up 24 bytes to find the header, and
\  inserts the block into the free list (sorted by address and coalescing
\  adjacent blocks).  §1.0b later routes public ALLOCATE/FREE through XMEM
\  when available and exposes DMA-ALLOCATE/DMA-FREE for explicit Bank 0.
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
\   In Bank 0, abort if HERE + u would reach within 256 bytes of SP,
\   or would collide with the heap (if initialised).  While the BIOS
\   user-dictionary interval is active, prove the exact request fits by
\   subtraction so a wrapping HERE + u cannot evade the bound.
\   Use before large ALLOT or CREATE sequences in Forth code
\   to catch dictionary overflow before it corrupts the stack.
: ?DICT-ROOM  ( u -- )
    DUP 0< ABORT" Invalid dictionary size"
    DICT-LIMIT@ ?DUP IF
        >R
        HERE DICT-BASE@ < ABORT" userland dictionary below base"
        HERE R@ > ABORT" userland dictionary beyond limit"
        R@ HERE - > ABORT" userland dictionary full"
        R> DROP EXIT
    THEN
    \ Keep every comparison in subtraction geometry.  A positive request near
    \ the signed-cell ceiling must not turn HERE+u into a negative value that
    \ slips past the signed address comparisons.
    HERE SP@ 256 - >= ABORT" dictionary overflow"
    DUP SP@ 256 - HERE - >= ABORT" dictionary overflow"
    HEAP-INIT @ IF
        HERE HEAP-BASE @ 256 - >= ABORT" dictionary into heap"
        DUP HEAP-BASE @ 256 - HERE - >= ABORT" dictionary into heap"
    THEN
    DROP ;

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

\ Late Bank-0 source compilation grows the dictionary after the system heap
\ is initialised.  Keep that dictionary reserve ahead of every persistent heap
\ allocation; graphics.f currently uses a little over 18 KiB of it.
32768 CONSTANT LATE-DICT-RESERVE

\ HEAP-SETUP ( -- )  initialise the heap above HERE
\   Leaves LATE-DICT-RESERVE bytes above HERE for late Bank-0 dictionary growth,
\   then creates one large free block spanning to the stack guard.
: HEAP-SETUP  ( -- )
    HEAP-INIT @ IF EXIT THEN
    TALIGN
    HERE LATE-DICT-RESERVE + HEAP-BASE !
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
