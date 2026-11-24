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

VARIABLE XMEM-HERE       0 XMEM-HERE !
VARIABLE XMEM-LIMIT      0 XMEM-LIMIT !
VARIABLE XMEM-INIT-DONE  0 XMEM-INIT-DONE !

\ -- XMEM free-list for individual block reclaim --
\   Each freed block stores at its address:
\     +0  size   (bytes)
\     +8  next   (ptr to next free block, or 0)
\   Requests are rounded up to 16 bytes, the minimum recyclable block.
\   XMEM-FREE-BLOCK accepts the original positive request size and applies
\   the same normalization, so sub-node tails never become unrecoverable.
VARIABLE XMEM-FL     0 XMEM-FL !    \ free-list head (0 = empty)
VARIABLE FL-PREV                     \ search scratch
VARIABLE FL-CURR                     \ search scratch
VARIABLE FL-NEED                     \ requested bytes during first-fit

\ The allocator's state, XMEM-HERE with the free list and its search
\ scratch, is shared by every core that allocates: KDOS's own words on core
\ 0, and system modules whose words run on any core, such as chiplet.f.
\ Each word that changes it holds HEAP-LOCK, hardware spinlock 3 (section
\ 8.7), for the few accesses the change makes and never across a wait.  It
\ takes the lock with SPIN@ and SPIN!, since LOCK and UNLOCK come later in
\ this file, and tries a busy lock again at once: the holder runs only a
\ short walk of the free list, and the simulator ends a core's turn when it
\ finds a lock busy.  SPIN@ grants a lock its own core holds already, with
\ no depth count, so no caller holds HEAP-LOCK around an allocator word.
3 CONSTANT HEAP-LOCK
: _XMEM-ACQUIRE  ( -- )   BEGIN HEAP-LOCK SPIN@ 0= UNTIL ;
: _XMEM-RELEASE  ( -- )   HEAP-LOCK SPIN! ;

\ USERLAND-INIT installs an interval-aware validator later in this file.  The
\ indirection is required because XMEM-FREE-BLOCK is defined before the
\ userland partition state.  Prior to initialization every returned block is
\ necessarily below the future dictionary base.
: (_XMEM-FREE-SPAN-CHECK)  ( addr size -- )  2DROP ;
DEFER _XMEM-FREE-SPAN-CHECK
' (_XMEM-FREE-SPAN-CHECK) IS _XMEM-FREE-SPAN-CHECK

: _XMEM-NORMALIZE-SIZE  ( u -- u' )
    15 + -16 AND ;

\ XMEM-FREE-BLOCK ( addr size -- )  return a block to the XMEM free-list
\   Validates that addr falls within [EXT-MEM-BASE, XMEM-LIMIT),
\   the positive original request fits, and its 16-byte-normalized span
\   does not exceed XMEM-LIMIT.  The checks run before the lock, so that an
\   ABORT" leaves it free; XMEM-HERE only rises between resets, so a check
\   against a value another core has just raised stays sound.
: XMEM-FREE-BLOCK  ( addr size -- )
    DUP 1 < ABORT" XMEM-FREE: block too small"
    OVER EXT-MEM-BASE < ABORT" XMEM-FREE: addr below base"
    OVER XMEM-LIMIT @ >= ABORT" XMEM-FREE: exceeds limit"
    \ Check size <= limit-addr before any address addition.  The old
    \ addr+size comparison could wrap and admit a span crossing the limit.
    2DUP SWAP XMEM-LIMIT @ SWAP - >
    ABORT" XMEM-FREE: exceeds limit"
    _XMEM-NORMALIZE-SIZE
    \ Rounding is part of the owned span, so validate it independently.
    2DUP SWAP XMEM-LIMIT @ SWAP - >
    ABORT" XMEM-FREE: exceeds limit"
    \ A returned block must already belong to the allocated high-water span.
    \ This rejects manufactured future free-list nodes before USERLAND-INIT
    \ as well as after the dictionary/general-XMEM partition is sealed.
    OVER XMEM-HERE @ >= ABORT" XMEM-FREE: above high water"
    2DUP SWAP XMEM-HERE @ SWAP - >
    ABORT" XMEM-FREE: above high water"
    2DUP _XMEM-FREE-SPAN-CHECK
    _XMEM-ACQUIRE
    OVER !                            \ addr+0 = size
    XMEM-FL @ OVER 8 + !             \ addr+8 = old head
    XMEM-FL !                         \ head = addr
    _XMEM-RELEASE ;

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
\   split so a small allocation cannot strand the unused tail.  The caller
\   holds HEAP-LOCK.
: (XMEM-FL-FIND)  ( u -- addr true | false )
    FL-NEED !
    0 FL-PREV !   XMEM-FL @ FL-CURR !
    BEGIN FL-CURR @ WHILE
        FL-CURR @ @ FL-NEED @ >= IF       \ curr.size >= need ?
            FL-CURR @ @ FL-NEED @ -
            DUP 16 >= IF
                \ Keep the tail as a recyclable free block with the current
                \ successor.  Normalized requests put its header at least
                \ 16 bytes beyond the current node.
                FL-CURR @ FL-NEED @ +
                SWAP OVER !
                FL-CURR @ 8 + @ OVER 8 + !
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
    XMEM-INIT-DONE @ IF EXIT THEN
    XMEM? IF
        EXT-MEM-BASE XMEM-HERE !
        EXT-MEM-BASE EXT-MEM-SIZE + XMEM-LIMIT !
    ELSE
        0 XMEM-HERE !  0 XMEM-LIMIT !
    THEN
    1 XMEM-INIT-DONE ! ;

\ XMEM-ALLOT? ( u -- addr ior )  allocate u bytes from ext mem, or fail
\   Rounds positive requests to 16 bytes, tries the free-list first
\   (first-fit), then falls back to bump allocation, holding HEAP-LOCK for
\   both.
: XMEM-ALLOT?  ( u -- addr ior )
    XMEM? 0= IF DROP 0 -1 EXIT THEN
    DUP 0< OVER 0= OR IF DROP 0 -1 EXIT THEN
    DUP XMEM-LIMIT @ EXT-MEM-BASE - > IF DROP 0 -1 EXIT THEN
    _XMEM-NORMALIZE-SIZE
    _XMEM-ACQUIRE
    DUP (XMEM-FL-FIND) IF NIP 0 _XMEM-RELEASE EXIT THEN
    \ Prove the request fits in the remaining span before adding it to the
    \ bump pointer; base+size must never be used as the bounds check.
    DUP XMEM-LIMIT @ XMEM-HERE @ - > IF
        DROP 0 -1 _XMEM-RELEASE EXIT
    THEN
    XMEM-HERE @ SWAP
    OVER +
    XMEM-HERE ! 0
    _XMEM-RELEASE ;

\ XMEM-ALLOT ( u -- addr )  allocate u bytes from ext mem, or abort
: XMEM-ALLOT  ( u -- addr )
    XMEM? 0= ABORT" No external memory"
    DUP 0< OVER 0= OR ABORT" Invalid ext mem size"
    XMEM-ALLOT? ABORT" Ext mem overflow" ;

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
    _XMEM-ACQUIRE  XMEM-HERE @  63 + -64 AND  XMEM-HERE !  _XMEM-RELEASE ;

VARIABLE XMEM-FLOOR  0 XMEM-FLOOR !

\ (XMEM-RESET) ( -- )  primitive external-memory bulk reset
\   Respects XMEM-FLOOR — will not reset below the userland zone.
\   Also clears the free-list (all blocks return to the bump region).
: (XMEM-RESET)  ( -- )
    XMEM? IF
        _XMEM-ACQUIRE
        XMEM-FLOOR @ ?DUP IF XMEM-HERE ! ELSE EXT-MEM-BASE XMEM-HERE ! THEN
        0 XMEM-FL !
        _XMEM-RELEASE
    THEN ;

\ The public action is deferred so later lower subsystems with persistent
\ allocations can refuse a destructive reset while an owned object is live.
DEFER XMEM-RESET
' (XMEM-RESET) IS XMEM-RESET

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
