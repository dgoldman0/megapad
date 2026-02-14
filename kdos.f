\ =====================================================================
\  KDOS v1.1 — Kernel Dashboard OS for Megapad-64
\ =====================================================================
\
\  Loaded via UART into the Megapad-64 BIOS v1.0 Forth system.
\
\  Subsystems:
\    1. Utilities       — common words, CMOVE, +!, tile-alignment
\    2. Buffers         — typed, tile-aligned data regions
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
\    9. Screens         — interactive TUI (ANSI terminal, 9 screens)
\   10. Data Ports      — NIC-based external data ingestion
\   11. Benchmarking    — BENCH for timing via CYCLES
\   12. Dashboard       — text-mode system overview
\   13. Help            — online reference for all KDOS words
\   14. Startup
\   15. Bundles         — versioned, declarative pipeline bundle format

\ =====================================================================
\  §1  Utility Words
\ =====================================================================
\  Note: BIOS v1.0 provides all ANS core words including OFF, >=, <=,
\  W@, W!, L@, L!, .ZSTR, UCHAR, 2OVER, 2SWAP, 2ROT, TALIGN, MOVE,
\  COMPARE, ABORT, ABORT", LEAVE, EVALUATE, FIND, SOURCE, >IN, >NUMBER,
\  VALUE, TO, DOES>, POSTPONE, RECURSE, COUNT, WITHIN, U<, U>, 2/,
\  CHAR, [CHAR], 2>R, 2R>, 2R@, QUIT, plus the v0.5 set.

\ .R ( n width -- ) print number right-justified in width
: .R  ( just use . for now )
    DROP . ;

\ -- String utilities (needed by MP64FS file system) --

\ SAMESTR? ( addr1 addr2 maxlen -- flag )
\   Compare two byte strings up to maxlen bytes (zero-padded).
\   Returns -1 if equal, 0 if different.
: SAMESTR?  ( a1 a2 maxlen -- flag )
    DUP >R SWAP R> COMPARE 0= IF -1 ELSE 0 THEN ;

\ NAMEBUF -- 16-byte scratch for file name parsing
VARIABLE NAMEBUF  15 ALLOT

\ PARSE-NAME ( "name" -- )
\   Parse next whitespace-delimited word, copy into NAMEBUF, null-terminate.
VARIABLE PN-LEN

: PARSE-NAME  ( "name" -- )
    NAMEBUF 16 0 FILL
    BL WORD DUP C@ 15 MIN PN-LEN !   ( waddr )
    1+                                 ( src )
    NAMEBUF PN-LEN @                   ( src dst len )
    CMOVE ;

\ -- Stack safety utilities --

\ NEEDS ( n -- )  abort if stack has fewer than n items
: NEEDS  ( n -- )
    DEPTH 1 - >  ABORT" Stack underflow" ;

\ ASSERT ( flag -- )  abort if flag is false
: ASSERT  ( flag -- )
    0= ABORT" Assertion failed" ;

\ ['] ( "name" -- )  compile-time: parse next word, compile its XT as literal
\   Equivalent to: ' name LITERAL
: ['] ' POSTPONE LITERAL ; IMMEDIATE

\ .DEPTH ( -- )  show current stack depth
: .DEPTH  ( -- )  ." [" DEPTH . ." deep]" ;

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

16 CONSTANT /ALLOC-HDR

\ MEM-SIZE ( -- u )  total RAM in bytes
\   Reads from SysInfo MMIO device: MEM_SIZE at offsets +0x06, +0x07
\   Value is in KiB; multiply by 1024 for bytes.
: MEM-SIZE  ( -- u )
    0xFFFFFF0000000306 C@       \ MEM_SIZE_LO (KiB low byte)
    0xFFFFFF0000000307 C@       \ MEM_SIZE_HI (KiB high byte)
    8 LSHIFT OR  1024 * ;

\ -- Heap state --
VARIABLE HEAP-BASE    0 HEAP-BASE !
VARIABLE HEAP-FREE    0 HEAP-FREE !    \ head of free list
VARIABLE HEAP-INIT    0 HEAP-INIT !    \ flag: has heap been initialised?

\ -- Allocator scratch variables (avoid deep stack gymnastics) --
VARIABLE A-PREV       \ previous free-list node (0 = update HEAP-FREE)
VARIABLE A-CURR       \ current free-list node being examined
VARIABLE A-SIZE       \ requested allocation size (rounded)

\ HEAP-SETUP ( -- )  initialise the heap above HERE
\   Leaves a 4 KiB gap above HERE for dictionary growth,
\   then creates one large free block spanning to the stack guard.
: HEAP-SETUP  ( -- )
    HEAP-INIT @ IF EXIT THEN
    HERE 4096 + TALIGN  HEAP-BASE !
    \ Heap end = data-stack bottom - 4096 guard
    MEM-SIZE 2 / 4096 -   ( heap-end )
    HEAP-BASE @ -          ( available-bytes )
    /ALLOC-HDR -           ( usable size for first block )
    DUP 64 < ABORT" Heap too small"
    \ Write header for the single free block
    0 HEAP-BASE @ !              \ next = 0 (end of list)
    HEAP-BASE @ 8 + !            \ size = available
    HEAP-BASE @ HEAP-FREE !      \ free list head
    1 HEAP-INIT ! ;

\ (LINK-PREV!) ( addr -- )
\   Set previous node's next field (or HEAP-FREE) to addr.
: (LINK-PREV!)  ( addr -- )
    A-PREV @ 0= IF  HEAP-FREE !  ELSE  A-PREV @ !  THEN ;

\ ALLOCATE ( u -- addr ior )
\   Allocate u bytes.  Returns address and 0 on success,
\   or 0 and -1 on failure.  First-fit search.
: ALLOCATE  ( u -- addr ior )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    DUP 0= IF DROP 0 -1 EXIT THEN
    \ Round up to 8-byte alignment, minimum 16
    7 + -8 AND  DUP 16 < IF DROP 16 THEN
    A-SIZE !
    0 A-PREV !   HEAP-FREE @ A-CURR !
    BEGIN
        A-CURR @ 0= IF  0 -1 EXIT  THEN      \ OOM
        A-CURR @ 8 + @                         ( block-size )
        A-SIZE @ >= IF
            \ Found a big enough block
            A-CURR @ 8 + @  A-SIZE @ -         ( leftover )
            DUP /ALLOC-HDR 16 + >= IF
                \ Split: new free block after the allocated region
                A-CURR @ /ALLOC-HDR + A-SIZE @ +  ( leftover new-blk )
                A-CURR @ @ OVER !                  \ new-blk.next = curr.next
                SWAP /ALLOC-HDR - OVER 8 + !       \ new-blk.size = leftover-hdr
                A-SIZE @ A-CURR @ 8 + !            \ curr.size = requested
                (LINK-PREV!)                        \ prev → new-blk
            ELSE
                \ Use whole block — unlink from free list
                DROP
                A-CURR @ @  (LINK-PREV!)            \ prev → curr.next
            THEN
            A-CURR @ /ALLOC-HDR +  0  EXIT          \ return user addr + success
        THEN
        \ Block too small — advance
        A-CURR @ A-PREV !
        A-CURR @ @ A-CURR !
    AGAIN ;

\ FREE ( addr -- )
\   Return a previously allocated block to the free list.
\   Inserts in address-sorted order (for future coalescing).
: FREE  ( addr -- )
    DUP 0= IF DROP EXIT THEN
    /ALLOC-HDR -   ( block )
    0 A-PREV !   HEAP-FREE @ A-CURR !
    BEGIN
        A-CURR @ 0= IF
            \ End of list — append here
            0 OVER !                                \ block.next = 0
            A-PREV @ 0= IF  HEAP-FREE !
            ELSE  A-PREV @ !  THEN
            EXIT
        THEN
        A-CURR @ OVER > IF
            \ Insert before curr
            A-CURR @ OVER !                         \ block.next = curr
            A-PREV @ 0= IF  HEAP-FREE !
            ELSE  A-PREV @ !  THEN
            EXIT
        THEN
        \ Advance
        A-CURR @ A-PREV !
        A-CURR @ @ A-CURR !
    AGAIN ;

\ RESIZE ( a1 u -- a2 ior )
\   Resize an allocated block.  Simple: alloc new, copy, free old.
\   On failure returns original address and non-zero ior.
: RESIZE  ( a1 u -- a2 ior )
    DUP ALLOCATE               ( a1 u a2 ior )
    IF  2DROP DROP -1 EXIT  THEN
    \ Success: ( a1 u a2 ) — copy u bytes from a1 to a2, free a1
    A-CURR !                   \ stash a2
    A-SIZE !                   \ stash u
    DUP A-CURR @ A-SIZE @ CMOVE   \ copy u bytes from a1 to a2
    FREE                           \ free a1
    A-CURR @ 0 ;               \ return ( a2 0 )

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

\ .HEAP ( -- ) show heap summary
: .HEAP  ( -- )
    HEAP-INIT @ 0= IF HEAP-SETUP THEN
    ." Heap: base=" HEAP-BASE @ .
    ."  free=" HEAP-FREE-BYTES . ." bytes" CR ;

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
\  Exception frames are chained via HANDLER variable.
\
\  Requires BIOS words: SP@ SP! RP@ RP!
\

VARIABLE HANDLER   0 HANDLER !

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

\ =====================================================================
\  §1.3  CRC Convenience Words
\ =====================================================================
\
\  The BIOS provides six CRC primitives that talk directly to the
\  hardware CRC accelerator (MMIO at +0x07C0):
\    CRC-POLY!  ( n -- )       0=CRC32, 1=CRC32C, 2=CRC64
\    CRC-INIT!  ( n -- )       initial CRC value
\    CRC-FEED   ( n -- )       feed 8 bytes (LE 64-bit cell)
\    CRC@       ( -- n )       current CRC result
\    CRC-RESET  ( -- )         reset to init value
\    CRC-FINAL  ( -- )         XOR-out (finalize)
\
\  Below we build high-level words on top of those primitives.

\ CRC-BUF ( addr u -- )  Feed u bytes from addr into the CRC engine.
\   Processes full 8-byte chunks via CRC-FEED, then zero-pads and
\   feeds any remaining 1..7 bytes.
: CRC-BUF  ( addr u -- )
    \ Process full 8-byte chunks using BEGIN/WHILE/REPEAT
    BEGIN  DUP 8 >=  WHILE
        OVER @ CRC-FEED
        SWAP 8 + SWAP
        8 -
    REPEAT
    \ Remaining bytes: 0..7
    DUP 0 > IF
        \ Build a zero-padded 64-bit LE cell from remaining bytes.
        \ Byte 0 → bits 0..7, byte 1 → bits 8..15, etc.
        0 SWAP         ( addr cell rem )
        0 DO
            OVER I + C@   ( addr cell byte )
            I 3 LSHIFT LSHIFT  ( addr cell byte<<shift )
            OR            ( addr cell' )
        LOOP
        CRC-FEED
        DROP
    ELSE
        2DROP
    THEN
;

\ CRC32-BUF ( addr u -- crc )  Compute CRC-32 of a buffer.
: CRC32-BUF
    0 CRC-POLY!
    0xFFFFFFFF CRC-INIT!
    CRC-BUF
    CRC-FINAL
    CRC@ ;

\ CRC32C-BUF ( addr u -- crc )  Compute CRC-32C of a buffer.
: CRC32C-BUF
    1 CRC-POLY!
    0xFFFFFFFF CRC-INIT!
    CRC-BUF
    CRC-FINAL
    CRC@ ;

\ CRC64-BUF ( addr u -- crc )  Compute CRC-64-ECMA of a buffer.
: CRC64-BUF
    2 CRC-POLY!
    0xFFFFFFFFFFFFFFFF CRC-INIT!
    CRC-BUF
    CRC-FINAL
    CRC@ ;

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
    CR ."  Performance Counters" CR
    ."    Cycles:   " PERF-CYCLES . CR
    ."    Stalls:   " PERF-STALLS . CR
    ."    Tile ops: " PERF-TILEOPS . CR
    ."    Ext mem:  " PERF-EXTMEM . CR ;

\ .BIST-STATUS ( -- )  Display last BIST result (from boot, NOT re-run).
\   BIST destroys all RAM so must NOT be run after KDOS loads.
: .BIST-STATUS
    CR ."  Memory BIST Status" CR
    BIST-STATUS
    DUP 0 = IF DROP ."    idle (no BIST run)" CR ELSE
    DUP 2 = IF DROP ."    PASS" CR ELSE
    DUP 3 = IF DROP ."    FAIL at addr " BIST-FAIL-ADDR . CR
                    ."    Expected/Actual: " BIST-FAIL-DATA . CR ELSE
    DROP ."    running..." CR
    THEN THEN THEN ;

\ .TILE-DIAG ( -- )  Run tile self-test and display result.
: .TILE-DIAG
    CR ."  Tile Datapath Self-Test..." CR
    TILE-TEST
    BEGIN TILE-TEST@ DUP 0 = WHILE DROP REPEAT
    DUP 2 = IF
        DROP ."    PASS (ADD, MUL, DOT, SUM)" CR
    ELSE
        DROP ."    FAIL — failed sub-tests: " TILE-DETAIL@ . CR
    THEN ;

\ .ICACHE ( -- )  Display I-cache statistics.
: .ICACHE
    CR ."  I-Cache Statistics" CR
    ."    Hits:     " ICACHE-HITS . CR
    ."    Misses:   " ICACHE-MISSES . CR ;

\ DIAG ( -- )  Run full hardware diagnostics suite.
: DIAG
    CR ." ======== Hardware Diagnostics ========" CR
    .PERF
    .BIST-STATUS
    .TILE-DIAG
    .ICACHE
    ." ======================================" CR ;

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
    DUP 0 = IF DROP ." AES: idle" CR ELSE
    DUP 2 = IF DROP ." AES: done (OK)" CR ELSE
    DUP 3 = IF DROP ." AES: AUTH FAIL" CR ELSE
    DROP ." AES: busy" CR
    THEN THEN THEN ;

\ =====================================================================
\  §1.6  SHA-3 (Keccak-256) Hashing
\ =====================================================================
\  BIOS primitives (hardware accelerator at MMIO 0x0780):
\    SHA3-INIT  SHA3-UPDATE  SHA3-FINAL  SHA3-STATUS@

CREATE SHA3-BUF 32 ALLOT

\ SHA3 ( addr len hash-addr -- )  Hash buffer, store 32-byte digest.
: SHA3
    >R                        \ save dest
    SHA3-INIT
    SHA3-UPDATE
    R> SHA3-FINAL ;

\ .SHA3-STATUS ( -- )  Print human-readable SHA3 status.
: .SHA3-STATUS
    SHA3-STATUS@
    DUP 0 = IF DROP ." SHA3: idle" CR ELSE
    DUP 2 = IF DROP ." SHA3: done" CR ELSE
    DROP ." SHA3: unknown" CR
    THEN THEN ;

\ .SHA3 ( addr -- )  Print 32-byte digest as hex string.
: .SHA3
    32 0 DO
        DUP I + C@
        DUP 4 RSHIFT
        DUP 10 < IF 48 + ELSE 55 + THEN EMIT
        15 AND
        DUP 10 < IF 48 + ELSE 55 + THEN EMIT
    LOOP DROP ;

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

\ -- Registry (up to 16 buffers) --
VARIABLE BUF-COUNT
0 BUF-COUNT !
VARIABLE BUF-TABLE  15 CELLS ALLOT

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
    BUF-COUNT @ 16 < IF
        BDESC @  BUF-COUNT @ CELLS BUF-TABLE + !
        BUF-COUNT @ 1+ BUF-COUNT !
    THEN
    BDESC @ CONSTANT ;

\ -- Derived queries --
: B.BYTES  ( desc -- n )  DUP B.LEN SWAP B.WIDTH * ;
: B.TILES  ( desc -- n )  B.BYTES 63 + 64 / ;

\ -- Operations --
: B.FILL   ( byte desc -- )  DUP B.DATA SWAP B.BYTES ROT FILL ;
: B.ZERO   ( desc -- )       0 SWAP B.FILL ;

\ -- Info --
: B.INFO   ( desc -- )
    ." [buf"
    DUP ."  t=" B.TYPE .
    DUP ."  w=" B.WIDTH .
    DUP ."  n=" B.LEN .
    DUP ."  tiles=" B.TILES .
    ."  @" B.DATA . ." ]" CR ;

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
    ." --- Buffers (" BUF-COUNT @ . ." ) ---" CR
    BUF-COUNT @ DUP IF
        0 DO
            I . ." : "
            I CELLS BUF-TABLE + @ B.INFO
        LOOP
    ELSE DROP THEN ;

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
    ." [kern"
    DUP ."  in=" K.IN .
    DUP ."  out=" K.OUT .
    DUP ."  foot=" K.FOOT .
    ."  fl=" K.FLAGS . ." ]" CR ;

\ -- List all registered kernels --
: KERNELS  ( -- )
    ." --- Kernels (" KERN-COUNT @ . ." ) ---" CR
    KERN-COUNT @ DUP IF
        0 DO
            I . ." : "
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
    ." --- Histogram ---" CR
    256 0 DO
        I HIST@ DUP IF
            ."  [" I . ." ]=" . CR
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
    ." cycles=" . CR ;

\ P.BENCH ( pipe -- ) execute and time each step
: P.BENCH  ( pipe -- )
    ." Pipeline (" DUP P.COUNT . ." steps):" CR
    DUP P.COUNT DUP IF
        0 DO
            DUP I P.GET BENCH
            ."   step " I . ." = " . ." cycles" CR
        LOOP
    ELSE DROP THEN
    DROP ;

\ P.INFO ( pipe -- ) show pipeline descriptor
: P.INFO  ( pipe -- )
    ." [pipe cap=" DUP P.CAP .
    ." steps=" P.COUNT . ." ]" CR ;

\ -- List all registered pipelines --
: PIPES  ( -- )
    ." --- Pipelines (" PIPE-COUNT @ . ." ) ---" CR
    PIPE-COUNT @ DUP IF
        0 DO
            I . ." : "
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
: p1-sum  ( -- ) demo-a B.SUM ." sum=" . CR ;
2 PIPELINE pipe-fill-sum
' p1-fill pipe-fill-sum P.ADD
' p1-sum  pipe-fill-sum P.ADD

\ --- Pipeline 2: add-stats ---
\   Fill a with 10, b with 20, add a+b->c, print stats.
: p2-init  ( -- ) 10 demo-a B.FILL  20 demo-b B.FILL ;
: p2-add   ( -- ) demo-a demo-b demo-c kadd ;
: p2-stats ( -- ) demo-c kstats ." max=" . ."  min=" . ."  sum=" . CR ;
3 PIPELINE pipe-add-stats
' p2-init  pipe-add-stats P.ADD
' p2-add   pipe-add-stats P.ADD
' p2-stats pipe-add-stats P.ADD

\ --- Pipeline 3: threshold ---
\   Fill demo-a with ramp 0..63, threshold at 32, print stats.
: p3-fill ( -- )
    demo-a B.DATA 64 0 DO I OVER I + C! LOOP DROP ;
: p3-thresh ( -- ) 32 demo-a kthresh ;
: p3-stats  ( -- ) demo-a kstats ." max=" . ."  min=" . ."  sum=" . CR ;
3 PIPELINE pipe-thresh
' p3-fill   pipe-thresh P.ADD
' p3-thresh pipe-thresh P.ADD
' p3-stats  pipe-thresh P.ADD

\ =====================================================================
\  §7  Storage & Persistence
\ =====================================================================
\
\  Uses BIOS words: DISK@ DISK-SEC! DISK-DMA! DISK-N! DISK-READ DISK-WRITE
\  Storage device: 512-byte sectors, DMA to/from RAM.
\
\  B.SAVE / B.LOAD persist buffers by writing their data region to disk.
\  Buffer data is tile-aligned (64 bytes), sectors are 512 bytes.
\  A buffer that is N tiles writes ceil(N*64/512) sectors.

512 CONSTANT SECTOR

\ DISK? ( -- flag ) true if storage device present
: DISK?  ( -- flag )  DISK@ 128 AND 0<> ;

\ B.SECTORS ( desc -- n ) number of disk sectors needed for buffer data
: B.SECTORS  ( desc -- n )  B.BYTES SECTOR 1- + SECTOR / ;

\ B.SAVE ( desc sector -- ) save buffer data to disk starting at sector
: B.SAVE  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT                       ( addr nsectors sector )
    DISK-SEC!                 ( addr nsectors )
    DUP DISK-N!               ( addr nsectors )
    DROP                      ( addr )
    DISK-DMA!                 ( )
    DISK-WRITE ;

\ B.LOAD ( desc sector -- ) load buffer data from disk starting at sector
: B.LOAD  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT                       ( addr nsectors sector )
    DISK-SEC!                 ( addr nsectors )
    DUP DISK-N!               ( addr nsectors )
    DROP                      ( addr )
    DISK-DMA!                 ( )
    DISK-READ ;

\ DISK-INFO ( -- ) print storage device status
: DISK-INFO  ( -- )
    ." Storage: "
    DISK? IF
        ." present" CR
    ELSE
        ." not attached" CR
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

\ FWRITE ( addr len fdesc -- ) write len bytes from addr at cursor
\   Uses variables for clean stack management.  Advances cursor.
VARIABLE FW-FD
VARIABLE FW-ADDR
VARIABLE FW-LEN

: FWRITE  ( addr len fdesc -- )
    FW-FD !  FW-LEN !  FW-ADDR !
    \ Bounds check: cursor + len <= max_sectors * 512
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ F.MAX SECTOR * > IF
        ." FWRITE: out of space" CR EXIT
    THEN
    \ Set up DMA: sector = cursor/512 + start_sector
    FW-FD @ F.CURSOR SECTOR /
    FW-FD @ F.START + DISK-SEC!
    FW-ADDR @ DISK-DMA!
    FW-LEN @ SECTOR 1- + SECTOR / DISK-N!
    DISK-WRITE
    \ Advance cursor
    FW-FD @ F.CURSOR FW-LEN @ +
    FW-FD @ 24 + !
    \ Update used_bytes = max(used, cursor)
    FW-FD @ F.CURSOR FW-FD @ F.USED MAX
    FW-FD @ 16 + ! ;

\ FREAD ( addr len fdesc -- actual ) read up to len bytes at cursor
\   Returns actual bytes read.  Advances cursor.
VARIABLE FR-FD
VARIABLE FR-ADDR
VARIABLE FR-LEN

: FREAD  ( addr len fdesc -- actual )
    FR-FD !  FR-LEN !  FR-ADDR !
    \ Clamp len to available bytes
    FR-FD @ F.USED FR-FD @ F.CURSOR -
    FR-LEN @ MIN FR-LEN !
    FR-LEN @ 0= IF 0 EXIT THEN
    \ Set up DMA read
    FR-FD @ F.CURSOR SECTOR /
    FR-FD @ F.START + DISK-SEC!
    FR-ADDR @ DISK-DMA!
    FR-LEN @ SECTOR 1- + SECTOR / DISK-N!
    DISK-READ
    \ Advance cursor
    FR-FD @ F.CURSOR FR-LEN @ +
    FR-FD @ 24 + !
    FR-LEN @ ;

\ F.INFO ( fdesc -- ) print file descriptor
: F.INFO  ( fdesc -- )
    ." [file"
    DUP ."  sec=" F.START .
    DUP ."  max=" F.MAX .
    DUP ."  used=" F.USED .
    ."  cur=" F.CURSOR . ." ]" CR ;

\ -- List all registered (legacy) files --
: FILES  ( -- )
    ." --- Files (" FILE-COUNT @ . ." ) ---" CR
    FILE-COUNT @ DUP IF
        0 DO
            I . ." : "
            I CELLS FILE-TABLE + @ F.INFO
        LOOP
    ELSE DROP THEN ;

\ =====================================================================
\  §7.6  MP64FS — On-Disk Named File System
\ =====================================================================
\
\  Disk layout (1 MiB = 2048 x 512-byte sectors):
\    Sector 0       Superblock (magic "MP64", version, geometry)
\    Sector 1       Allocation bitmap (2048 bits = 256 bytes)
\    Sectors 2-5    Directory (64 entries x 32 bytes)
\    Sectors 6+     Data area (2042 sectors ~ 1 MB usable)
\
\  Directory entry (32 bytes):
\    +0   name[16]       null-terminated (max 15 chars)
\    +16  start_sec[2]   u16 LE
\    +18  sec_count[2]   u16 LE
\    +20  used_bytes[4]  u32 LE
\    +24  type[1]        0=free 1=raw 2=text 3=forth 4=doc 5=data
\    +25  flags[1]       bit0=readonly bit1=system
\    +26  reserved[6]
\
\  File descriptor layout (created by OPEN):
\    +0   start_sector   (cell)
\    +8   max_sectors     (cell)
\    +16  used_bytes      (cell)
\    +24  cursor          (cell)
\    +32  dir_slot        (cell) — index into directory cache

\ -- Constants --
6   CONSTANT FS-DATA-START
64  CONSTANT FS-MAX-FILES
32  CONSTANT FS-ENTRY-SIZE

\ -- RAM caches (loaded from disk by FS-LOAD) --
VARIABLE FS-SUPER  SECTOR 1- ALLOT            \ 512 bytes — superblock
VARIABLE FS-BMAP   SECTOR 1- ALLOT            \ 512 bytes — bitmap
VARIABLE FS-DIR    SECTOR 4 * 1- ALLOT        \ 2048 bytes — directory

VARIABLE FS-OK     0 FS-OK !

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
    FS-DATA-START FF-START !
    0 FF-LEN !
    -1                                 \ result on stack
    2048 FS-DATA-START DO
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
: DE.SEC    ( de -- u16 )   16 + W@ ;
: DE.COUNT  ( de -- u16 )   18 + W@ ;
: DE.USED   ( de -- u32 )   20 + L@ ;
: DE.TYPE   ( de -- u8 )    24 + C@ ;
: DE.FLAGS  ( de -- u8 )    25 + C@ ;

\ FIND-FREE-SLOT ( -- slot | -1 ) first empty directory slot

: FIND-FREE-SLOT  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0= IF DROP I LEAVE THEN
    LOOP ;

\ ── Loading and syncing ──────────────────────────────────────────────

\ FS-LOAD ( -- ) read superblock + bitmap + directory from disk
: FS-LOAD  ( -- )
    DISK? 0= IF
        ." No disk attached" CR EXIT
    THEN
    \ Read superblock (sector 0)
    0 DISK-SEC!  FS-SUPER DISK-DMA!  1 DISK-N!  DISK-READ
    \ Check magic "MP64" (M=77 P=80 6=54 4=52)
    FS-SUPER     C@ 77 <>
    FS-SUPER 1+  C@ 80 <> OR
    FS-SUPER 2 + C@ 54 <> OR
    FS-SUPER 3 + C@ 52 <> OR
    IF
        ." Not an MP64FS disk" CR EXIT
    THEN
    \ Read bitmap (sector 1)
    1 DISK-SEC!  FS-BMAP DISK-DMA!  1 DISK-N!  DISK-READ
    \ Read directory (sectors 2-5)
    2 DISK-SEC!  FS-DIR DISK-DMA!  4 DISK-N!  DISK-READ
    -1 FS-OK !
    ." MP64FS loaded" CR ;

\ FS-SYNC ( -- ) write bitmap + directory back to disk
: FS-SYNC  ( -- )
    FS-OK @ 0= IF ." FS not loaded" CR EXIT THEN
    1 DISK-SEC!  FS-BMAP DISK-DMA!  1 DISK-N!  DISK-WRITE
    2 DISK-SEC!  FS-DIR  DISK-DMA!  4 DISK-N!  DISK-WRITE ;

\ FS-ENSURE ( -- ) auto-load if not yet loaded
: FS-ENSURE  ( -- )
    FS-OK @ 0= IF
        DISK? IF FS-LOAD THEN
    THEN ;

\ ── FORMAT ───────────────────────────────────────────────────────────

\ FORMAT ( -- ) initialise a fresh MP64FS on the attached disk
: FORMAT  ( -- )
    DISK? 0= IF ." No disk" CR EXIT THEN
    \ Build superblock in RAM
    FS-SUPER SECTOR 0 FILL
    77 FS-SUPER     C!              \ 'M'
    80 FS-SUPER 1+  C!              \ 'P'
    54 FS-SUPER 2 + C!              \ '6'
    52 FS-SUPER 3 + C!              \ '4'
    1    FS-SUPER 4  + W!           \ version
    2048 FS-SUPER 6  + W!           \ total sectors
    1    FS-SUPER 8  + W!           \ bitmap start
    1    FS-SUPER 10 + W!           \ bitmap sectors
    2    FS-SUPER 12 + W!           \ dir start
    4    FS-SUPER 14 + W!           \ dir sectors
    6    FS-SUPER 16 + W!           \ data start
    0 DISK-SEC!  FS-SUPER DISK-DMA!  1 DISK-N!  DISK-WRITE
    \ Initialise bitmap — mark sectors 0-5 (metadata) as allocated
    FS-BMAP SECTOR 0 FILL
    FS-DATA-START 0 DO I BIT-SET LOOP
    1 DISK-SEC!  FS-BMAP DISK-DMA!  1 DISK-N!  DISK-WRITE
    \ Zero directory
    FS-DIR SECTOR 4 * 0 FILL
    2 DISK-SEC!  FS-DIR DISK-DMA!  4 DISK-N!  DISK-WRITE
    -1 FS-OK !
    ." MP64FS formatted" CR ;

\ ── .FTYPE — print file type name ───────────────────────────────────

: .FTYPE  ( type -- )
    DUP 0 = IF DROP ." free"  EXIT THEN
    DUP 1 = IF DROP ." raw"   EXIT THEN
    DUP 2 = IF DROP ." text"  EXIT THEN
    DUP 3 = IF DROP ." forth" EXIT THEN
    DUP 4 = IF DROP ." doc"   EXIT THEN
    DUP 5 = IF DROP ." data"  EXIT THEN
    DUP 6 = IF DROP ." tut"   EXIT THEN
    DUP 7 = IF DROP ." bdl"   EXIT THEN
    ." ?" . ;

\ ── DIR — list files ─────────────────────────────────────────────────

: DIR  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    ." --- Directory ---" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            1+
            ."  " I DIRENT .ZSTR
            ."   " I DIRENT DE.USED . ." B"
            ."   " I DIRENT DE.TYPE .FTYPE
            CR
        THEN
    LOOP
    DUP . ." file(s), "
    0  2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ." free sectors ("
    SECTOR * . ." bytes free)" CR
    DROP ;

\ CATALOG ( -- ) detailed directory listing
: CATALOG  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    ." Name             Bytes     Secs  Type" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            1+
            ."  " I DIRENT .ZSTR
            ."  " I DIRENT DE.USED .
            ."  " I DIRENT DE.COUNT .
            ."  " I DIRENT DE.TYPE .
            CR
        THEN
    LOOP
    ." (" . ." files, "
    0 2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    . ." free sectors)" CR ;

\ ── FIND-BY-NAME — shared directory lookup ───────────────────────────
\ Searches directory for an entry whose first 16 bytes match NAMEBUF.
\ Returns slot index or -1 if not found.  Caller must call PARSE-NAME first.

: FIND-BY-NAME  ( -- slot | -1 )
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT NAMEBUF 16 SAMESTR? IF
                DROP I LEAVE
            THEN
        THEN
    LOOP ;

\ ── MKFILE — create a new file ───────────────────────────────────────

VARIABLE MK-NSEC
VARIABLE MK-TYPE
VARIABLE MK-SLOT
VARIABLE MK-START

: MKFILE  ( nsectors type "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR 2DROP EXIT THEN
    MK-TYPE !  MK-NSEC !
    PARSE-NAME
    \ Check for duplicate name
    FIND-BY-NAME -1 <> IF
        ." File exists: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Find empty directory slot
    FIND-FREE-SLOT MK-SLOT !
    MK-SLOT @ -1 = IF ." Directory full" CR EXIT THEN
    \ Find contiguous free sectors
    MK-NSEC @ FIND-FREE MK-START !
    MK-START @ -1 = IF ." No space on disk" CR EXIT THEN
    \ Allocate sectors in bitmap
    MK-NSEC @ 0 DO
        MK-START @ I + BIT-SET
    LOOP
    \ Build directory entry
    MK-SLOT @ DIRENT                 ( de )
    DUP FS-ENTRY-SIZE 0 FILL        \ zero slot
    DUP NAMEBUF SWAP 16 CMOVE       \ copy name
    DUP MK-START @ SWAP 16 + W!     \ start sector
    DUP MK-NSEC  @ SWAP 18 + W!     \ sector count
    DUP 0          SWAP 20 + L!     \ used_bytes = 0
    MK-TYPE  @ SWAP 24 + C!         \ type
    FS-SYNC
    ." Created: " NAMEBUF .ZSTR
    ." (" MK-NSEC @ . ." sectors at " MK-START @ . ." )" CR ;

\ ── RMFILE — delete a file ───────────────────────────────────────────

VARIABLE RM-SLOT

: RMFILE  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME RM-SLOT !
    RM-SLOT @ -1 = IF
        ." Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Free bitmap sectors
    RM-SLOT @ DIRENT DE.COUNT
    RM-SLOT @ DIRENT DE.SEC
    SWAP 0 DO                        ( start )
        DUP I + BIT-CLR
    LOOP DROP
    \ Clear directory entry
    RM-SLOT @ DIRENT FS-ENTRY-SIZE 0 FILL
    FS-SYNC
    ." Deleted: " NAMEBUF .ZSTR CR ;

\ ── RENAME — rename a file ───────────────────────────────────────────

VARIABLE RN-SLOT

: RENAME  ( "oldname" "newname" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    \ Look up old name
    PARSE-NAME
    FIND-BY-NAME RN-SLOT !
    RN-SLOT @ -1 = IF
        ." Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Parse new name
    PARSE-NAME
    \ Check new name doesn't already exist
    FIND-BY-NAME -1 <> IF
        ." Name taken: " NAMEBUF .ZSTR CR EXIT
    THEN
    \ Overwrite name in directory entry
    RN-SLOT @ DIRENT 16 0 FILL       \ zero old name
    NAMEBUF RN-SLOT @ DIRENT 16 CMOVE
    FS-SYNC
    ." Renamed to: " NAMEBUF .ZSTR CR ;

\ ── CAT — print file contents to terminal ────────────────────────────

VARIABLE CAT-SLOT

: CAT  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME CAT-SLOT !
    CAT-SLOT @ -1 = IF
        ." Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    CAT-SLOT @ DIRENT DE.USED DUP 0= IF
        DROP ." (empty file)" CR EXIT
    THEN                                 ( used_bytes )
    \ Read file sectors into HERE (temporary)
    CAT-SLOT @ DIRENT DE.SEC DISK-SEC!
    HERE DISK-DMA!
    CAT-SLOT @ DIRENT DE.COUNT DISK-N!
    DISK-READ
    \ Print used_bytes characters from HERE
    HERE SWAP                            ( addr count )
    0 DO
        DUP I + C@ DUP 10 = IF
            DROP CR
        ELSE
            EMIT
        THEN
    LOOP DROP ;

\ ── FS-FREE — report free disk space ────────────────────────────────

: FS-FREE  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    0   \ free sector count
    2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ." free sectors ("
    SECTOR * . ." bytes)" CR
    \ Count used files
    0  FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF 1+ THEN
    LOOP
    . ." files, " FS-MAX-FILES . ." max" CR ;

\ ── SAVE-BUFFER — save buffer data to a named file ──────────────────

VARIABLE SB-SLOT
VARIABLE SB-DESC

: SAVE-BUFFER  ( buf "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF DROP ." No filesystem" CR EXIT THEN
    SB-DESC !
    PARSE-NAME
    FIND-BY-NAME SB-SLOT !
    SB-SLOT @ -1 = IF
        ." Not found: " NAMEBUF .ZSTR
        ."  (create with MKFILE first)" CR EXIT
    THEN
    \ Write buffer data into file's sectors
    SB-SLOT @ DIRENT DE.SEC DISK-SEC!
    SB-DESC @ B.DATA DISK-DMA!
    SB-SLOT @ DIRENT DE.COUNT DISK-N!
    DISK-WRITE
    \ Update used_bytes in directory
    SB-DESC @ B.LEN
    SB-SLOT @ DIRENT 20 + L!
    FS-SYNC
    ." Saved " SB-DESC @ B.LEN . ." bytes to " NAMEBUF .ZSTR CR ;

\ ── OPEN — open a file by name ───────────────────────────────────────

VARIABLE OP-SLOT

: OPEN  ( "name" -- fdesc | 0 )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR 0 EXIT THEN
    PARSE-NAME
    FIND-BY-NAME OP-SLOT !
    OP-SLOT @ -1 = IF
        ." Not found: " NAMEBUF .ZSTR CR 0 EXIT
    THEN
    \ Create file descriptor in dictionary (5 cells = 40 bytes)
    HERE
    OP-SLOT @ DIRENT DE.SEC   ,      \ +0  start_sector
    OP-SLOT @ DIRENT DE.COUNT ,      \ +8  max_sectors
    OP-SLOT @ DIRENT DE.USED  ,      \ +16 used_bytes
    0 ,                               \ +24 cursor = 0
    OP-SLOT @ ,                       \ +32 dir_slot
    ;

\ F.SLOT ( fdesc -- n ) directory slot index (for OPEN'd files)
: F.SLOT  ( fdesc -- n )  32 + @ ;

\ FFLUSH ( fdesc -- ) write metadata back to directory on disk
: FFLUSH  ( fdesc -- )
    FS-OK @ 0= IF DROP ." FS not loaded" CR EXIT THEN
    DUP F.USED
    OVER F.SLOT DIRENT 20 + L!      \ update used_bytes in dir cache
    DROP
    FS-SYNC ;

\ ── LOAD — load and execute a Forth source file ─────────────────────
\ LOAD ( "filename" -- ) open a file by name, read it, EVALUATE it
\   Reads the entire file into a temporary buffer at HERE, then
\   walks through it line by line, EVALUATEing each line.

VARIABLE LD-FD
VARIABLE LD-BUF
VARIABLE LD-SZ

: LOAD  ( "filename" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ." Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DIRENT                               ( de )
    \ Open by slot
    DUP DE.USED DUP 0= IF
        2DROP ." Empty file" CR EXIT
    THEN LD-SZ !                         ( de )
    DUP 16 + W@ SWAP DE.COUNT           ( start count )
    \ Read file data into HERE, then advance HERE past it
    \ so that EVALUATE'd code (BUFFER, KERNEL, BL WORD, etc.)
    \ cannot overwrite the file data.
    HERE LD-BUF !
    LD-SZ @ ALLOT                        ( start count )
    \ Read sectors directly into buffer
    OVER DISK-SEC!
    LD-BUF @ DISK-DMA!
    DUP DISK-N!
    DISK-READ
    2DROP                                ( -- clean stack )
    \ Now walk buffer line by line and EVALUATE each line
    LD-BUF @                             ( addr )
    LD-SZ @                              ( addr remaining )
    BEGIN DUP 0> WHILE
        \ Find next newline or end
        OVER                             ( addr rem linestart )
        2 PICK                           ( addr rem linestart rem )
        0                                ( addr rem linestart rem i )
        BEGIN
            DUP 2 PICK < IF
                OVER OVER + C@ 10 = IF
                    TRUE                 \ found newline
                ELSE
                    1+ FALSE
                THEN
            ELSE TRUE THEN              \ end of buffer
        UNTIL                            ( addr rem linestart rem linelen )
        NIP                              ( addr rem linestart linelen )
        DUP 0> IF
            2DUP EVALUATE
        THEN
        \ Advance past line + newline
        1+                               ( addr rem linestart skip )
        ROT OVER - >R                    ( addr linestart skip  R: rem' )
        + SWAP DROP                      ( addr' )
        R>                               ( addr' rem' )
    REPEAT
    2DROP ;

\ -- ANSI helpers (needed by .DOC-CHUNK; full set defined in §9) --
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
    F.SLOT DIRENT 25 + C@
    F-ENC-FLAG AND 0<>
;

\ _FE-SET-ENC ( fdesc -- )  Set encrypted flag in FS-DIR cache.
: _FE-SET-ENC
    F.SLOT DIRENT 25 +
    DUP C@ F-ENC-FLAG OR SWAP C!
;

\ _FE-CLR-ENC ( fdesc -- )  Clear encrypted flag in FS-DIR cache.
: _FE-CLR-ENC
    F.SLOT DIRENT 25 +
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
        ." FENCRYPT: insufficient space" CR -1 EXIT
    THEN
    \ Sectors for ciphertext + tag
    _FE-PAD @ 16 + 511 + 512 / _FE-SECS !
    \ Allocate sector-aligned buffers (DMA operates on full sectors)
    _FE-SECS @ 512 * ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * ALLOCATE DROP DUP 0= IF _FE-BUF1 @ FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero both buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read file data from disk into buf1
    _FE-DESC @ F.START DISK-SEC!
    _FE-BUF1 @ DISK-DMA!
    _FE-USED @ 511 + 512 / DISK-N!
    DISK-READ
    \ Derive IV from directory slot
    _FE-DESC @ _FE-MKIV
    \ Encrypt: ( key iv src dst len -- tag-addr )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @ ENCRYPT
    \ Copy 16-byte tag after ciphertext in buf2
    _FE-BUF2 @ _FE-PAD @ + 16 CMOVE
    \ DMA-write ciphertext + tag back to disk
    _FE-DESC @ F.START DISK-SEC!
    _FE-BUF2 @ DISK-DMA!
    _FE-SECS @ DISK-N!
    DISK-WRITE
    \ Set encrypted flag and sync directory
    _FE-DESC @ _FE-SET-ENC
    _FE-USED @ _FE-DESC @ F.SLOT DIRENT 20 + L!
    FS-SYNC
    \ Free buffers
    _FE-BUF1 @ FREE
    _FE-BUF2 @ FREE
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
    _FE-SECS @ 512 * ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * ALLOCATE DROP DUP 0= IF _FE-BUF1 @ FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read ciphertext + tag from disk
    _FE-DESC @ F.START DISK-SEC!
    _FE-BUF1 @ DISK-DMA!
    _FE-SECS @ DISK-N!
    DISK-READ
    \ Derive IV
    _FE-DESC @ _FE-MKIV
    \ Decrypt: ( key iv src dst len tag -- flag )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @
    _FE-BUF1 @ _FE-PAD @ +       \ tag is right after ciphertext
    DECRYPT
    \ flag on stack: 0 = auth OK, -1 = auth fail
    DUP 0= IF
        \ Write plaintext back to disk
        _FE-DESC @ F.START DISK-SEC!
        _FE-BUF2 @ DISK-DMA!
        _FE-USED @ 511 + 512 / DISK-N!
        DISK-WRITE
        \ Clear encrypted flag, sync
        _FE-DESC @ _FE-CLR-ENC
        _FE-USED @ _FE-DESC @ F.SLOT DIRENT 20 + L!
        FS-SYNC
    THEN
    \ Free buffers
    _FE-BUF1 @ FREE
    _FE-BUF2 @ FREE
;

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
                DIM ." --- more ---" RESET-COLOR
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
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    ." Available topics:" CR
    0                                        \ count
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                1+
                ."   " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."   (none)" CR THEN
    ." (" . ." topics)" CR ;

\ LESSONS ( -- )  list available tutorials (type=6)
: LESSONS  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    ." Available lessons:" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-TUT = IF
                1+
                ."   " I DIRENT .ZSTR CR
            THEN
        THEN
    LOOP
    DUP 0= IF ."   (none)" CR THEN
    ." (" . ." lessons)" CR ;

\ DOC ( "name" -- )  page through a documentation file
: DOC  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    CR SHOW-FILE CR ;

\ TUTORIAL ( "name" -- )  walk through a tutorial file
: TUTORIAL  ( "name" -- )
    OPEN DUP 0= IF EXIT THEN
    CR SHOW-FILE CR ;

\ OPEN-BY-SLOT ( slot -- fdesc | 0 )  open a file by directory slot
\   Like OPEN but takes a slot index instead of parsing a name.
: OPEN-BY-SLOT  ( slot -- fdesc | 0 )
    DUP DIRENT C@ 0= IF DROP 0 EXIT THEN
    HERE SWAP                             ( here slot )
    DUP DIRENT DE.SEC   ,                 \ +0  start_sector
    DUP DIRENT DE.COUNT ,                 \ +8  max_sectors
    DUP DIRENT DE.USED  ,                 \ +16 used_bytes
    0 ,                                    \ +24 cursor = 0
    ,                                      \ +32 dir_slot (consumes slot)
    ;

\ DESCRIBE ( "word" -- )  look up a word in the documentation
\   Tries to open a doc file matching the name.  If no exact match,
\   suggests using TOPICS.
: DESCRIBE  ( "word" -- )
    PARSE-NAME PN-LEN @ 0= IF ." Usage: DESCRIBE <word>" CR EXIT THEN
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    \ Search for a doc file whose name matches NAMEBUF
    -1
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            I DIRENT DE.TYPE FTYPE-DOC = IF
                I DIRENT NAMEBUF 16 SAMESTR? IF
                    DROP I LEAVE
                THEN
            THEN
        THEN
    LOOP
    DUP -1 = IF
        DROP
        ." No doc for: " NAMEBUF .ZSTR CR
        ." Use TOPICS to list available documentation." CR
        EXIT
    THEN
    OPEN-BY-SLOT DUP 0= IF EXIT THEN
    CR SHOW-FILE CR ;

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
        2DROP ." Usage: WORDS-LIKE <pattern>" CR EXIT
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
    CR ." (" WL-CNT @ . ." found)" CR ;

\ APROPOS ( "pattern" -- )  alias for WORDS-LIKE
: APROPOS  ( "pattern" -- )  WORDS-LIKE ;

\ .RECENT ( n -- )  show the last n words defined in the dictionary
: .RECENT  ( n -- )
    CR ." Recent words:" CR
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
    ." [task"
    DUP ."  st=" T.STATUS .
    DUP ."  pri=" T.PRIORITY .
    ."  xt=" T.XT . ." ]" CR ;

: TASKS  ( -- )
    ." --- Tasks (" TASK-COUNT @ . ." ) ---" CR
    TASK-COUNT @ DUP IF
        0 DO
            I . ." : "
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
: YIELD  ( -- )
    CURRENT-TASK @ DUP IF
        T.DONE SWAP T.STATUS!
    ELSE DROP THEN ;

\ -- YIELD? ( -- ) check preempt flag, yield if set --
: YIELD?  ( -- )
    PREEMPT-FLAG @ IF
        0 PREEMPT-FLAG !
        YIELD
    THEN ;

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

\ -- YIELD? ( -- ) check timer, yield if time slice expired --
: YIELD?  ( -- )
    PREEMPT-ENABLED @ IF
        PREEMPT-FLAG @ IF
            0 PREEMPT-FLAG !
            YIELD
        THEN
    THEN ;

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

\ -- CORE-RUN ( xt core -- )  dispatch XT to secondary core --
\   Validates the core number, then sends via WAKE-CORE.
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
    ." --- Cores (" NCORES . ." ) ---" CR
    NCORES 0 DO
        ."   Core " I .
        I COREID = IF
            ."  [self] RUNNING" CR
        ELSE
            I CORE-STATUS IF
                ."  BUSY" CR
            ELSE
                ."  IDLE" CR
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
\   Distributes steps across available secondary cores.
\   If there are more steps than cores, remaining steps run on core 0.
\   Always waits for all dispatched work before returning.
VARIABLE PAR-P          \ pipeline being dispatched
VARIABLE PAR-N          \ next core to use

: P.RUN-PAR  ( pipe -- )
    NCORES 1 <= IF
        \ Single core: fall back to sequential
        P.RUN EXIT
    THEN
    DUP P.COUNT 0= IF DROP EXIT THEN
    PAR-P !
    1 PAR-N !               \ start dispatching to core 1
    PAR-P @ P.COUNT 0 DO
        PAR-P @ I P.GET      ( step-xt )
        PAR-N @ NCORES < IF
            PAR-N @ CORE-RUN
            PAR-N @ 1+ PAR-N !
        ELSE
            EXECUTE
        THEN
    LOOP
    ALL-CORES-WAIT ;

\ -- P.BENCH-PAR ( pipe -- )  benchmark parallel pipeline --
: P.BENCH-PAR  ( pipe -- )
    ." Parallel pipeline (" DUP P.COUNT . ." steps, "
    NCORES . ." cores):" CR
    DUP
    CYCLES >R
    P.RUN-PAR
    CYCLES R> -
    ."   total = " . ." cycles" CR ;

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
\  Screens: [1]Home [2]Bufs [3]Kern [4]Pipe [5]Task [6]Help [7]Docs [8]Stor [9]Core
\  Keys: 1-9 switch, n/p navigate, Enter select, a auto-refresh, r/q.
\

\ -- ANSI escape primitives --
: ESC   ( -- )  27 EMIT ;
: CSI   ( -- )  ESC 91 EMIT ;     \ ESC [

\ .N ( n -- )  print number without trailing space
: .N  ( n -- )
    DUP 0< IF 45 EMIT NEGATE THEN
    DUP 10 < IF
        48 + EMIT
    ELSE DUP 100 < IF
        DUP 10 / 48 + EMIT
        10 MOD 48 + EMIT
    ELSE
        \ General case: use .  and trim space
        DUP 1000 < IF
            DUP 100 / 48 + EMIT
            DUP 10 / 10 MOD 48 + EMIT
            10 MOD 48 + EMIT
        ELSE
            . \ fallback with trailing space for large numbers
        THEN
    THEN THEN ;

\ -- Cursor & screen control --
: AT-XY   ( col row -- )  CSI .N 59 EMIT .N 72 EMIT ;   \ ESC[row;colH
: PAGE     ( -- )  CSI 50 EMIT 74 EMIT CSI 72 EMIT ;     \ ESC[2J ESC[H
: CLS      ( -- )  PAGE ;                                  \ alias

\ -- Colors (SGR) --
: SGR      ( n -- )  CSI .N 109 EMIT ;   \ ESC[Nm
: RESET-COLOR  ( -- )  0 SGR ;
: BOLD     ( -- )  1 SGR ;
: DIM      ( -- )  2 SGR ;
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
VARIABLE SCREEN-ID      \ current screen: 1-8
VARIABLE SCREEN-RUN     \ flag: 0 = exit loop

\ -- Extended screen state --
VARIABLE SCR-SEL      -1 SCR-SEL !     \ selected item on current screen
VARIABLE SCR-MAX       0 SCR-MAX !     \ max selectable items on screen
VARIABLE AUTO-REFRESH  0 AUTO-REFRESH !
VARIABLE REFRESH-LAST

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
                        PAGE SHOW-FILE
                        CR DIM ."  Press any key to return..." RESET-COLOR
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

\ -- Screen header --
: SCREEN-HEADER  ( -- )
    1 1 AT-XY
    REVERSE
    ."  KDOS v1.1 "
    RESET-COLOR
    SPACE
    SCREEN-ID @ DUP 1 = IF REVERSE THEN ." [1]Home " RESET-COLOR
    DUP 2 = IF REVERSE THEN ." [2]Bufs " RESET-COLOR
    DUP 3 = IF REVERSE THEN ." [3]Kern " RESET-COLOR
    DUP 4 = IF REVERSE THEN ." [4]Pipe " RESET-COLOR
    DUP 5 = IF REVERSE THEN ." [5]Task " RESET-COLOR
    DUP 6 = IF REVERSE THEN ." [6]Help " RESET-COLOR
    DUP 7 = IF REVERSE THEN ." [7]Docs " RESET-COLOR
    DUP 8 = IF REVERSE THEN ." [8]Stor " RESET-COLOR
    9 = IF REVERSE THEN ." [9]Core " RESET-COLOR
    CR HBAR ;

\ -- Screen footer --
: SCREEN-FOOTER  ( -- )
    DIM
    ."  [1-9] Switch  [n/p] Select  [r] Refresh"
    AUTO-REFRESH @ IF 2 FG ."  Auto:ON" RESET-COLOR DIM ELSE ."  [a]Auto" THEN
    ."   [q] Quit"
    RESET-COLOR CR ;

\ ---- Screen 1: Home ----
: SCR-HOME  ( -- )
    .LABEL ."  System Overview" ./LABEL CR CR
    ."   Memory  : HERE = " HERE . CR
    ."   Cores   : " NCORES .N
    NCORES 1 > IF 2 FG ."  multicore" ELSE DIM ."  single" THEN RESET-COLOR CR
    ."   Buffers : " BUF-COUNT @ .N CR
    ."   Kernels : " KERN-COUNT @ .N CR
    ."   Pipes   : " PIPE-COUNT @ .N CR
    ."   Tasks   : " TASK-COUNT @ .N CR
    ."   Files   : " FILE-COUNT @ .N CR
    ."   Storage : " DISK? IF 2 FG ." present" ELSE 1 FG ." not attached" THEN RESET-COLOR CR
    ."   Ports   : " PORT-COUNT @ .N ."  bound  rx=" PORT-RX @ .N ."  drop=" PORT-DROP @ .N CR
    ."   Network : " NET-RX? IF 2 FG ." frame waiting" ELSE DIM ." idle" THEN RESET-COLOR CR
    CR
    ."   Scheduler: " PREEMPT-ENABLED @ IF 2 FG ." preempt ON" ELSE DIM ." cooperative" THEN RESET-COLOR CR
    ."   Tasks rdy: " TASK-COUNT-READY .N CR ;

\ ---- Screen 2: Buffers ----
: SCR-BUFFERS  ( -- )
    .LABEL ."  Buffers (" BUF-COUNT @ .N ." )" ./LABEL CR CR
    BUF-COUNT @ DUP 0= IF
        DROP ."   (none registered)" CR
        0 SCR-MAX !
    ELSE
        DUP SCR-MAX !
        0 DO
            SCR-SEL @ I = IF 2 FG ." > " RESET-COLOR ELSE ."   " THEN
            I .N ."  "
            I CELLS BUF-TABLE + @
            DUP B.TYPE
            DUP 0 = IF DROP ." raw " THEN
            DUP 1 = IF DROP ." rec " THEN
            DUP 2 = IF DROP ." til " THEN
            DUP 3 = IF DROP ." bit " THEN
            ." w=" DUP B.WIDTH .N
            ."  n=" DUP B.LEN .N
            ."  tiles=" DUP B.TILES .N
            ."  @" B.DATA .N
            CR
        LOOP
    THEN
    \ -- Inline detail for selected buffer --
    SCR-SEL @ -1 <> SCR-SEL @ BUF-COUNT @ < AND IF
        CR HBAR
        SCR-SEL @ CELLS BUF-TABLE + @
        DUP B.INFO
        B.PREVIEW
    THEN ;

\ ---- Screen 3: Kernels ----
: SCR-KERNELS  ( -- )
    .LABEL ."  Kernels (" KERN-COUNT @ .N ." )" ./LABEL CR CR
    KERN-COUNT @ DUP 0= IF
        DROP ."   (none registered)" CR
    ELSE
        0 DO
            ."   " I .N ."  "
            I CELLS KERN-TABLE + @
            DUP K.IN .N ." in "
            DUP K.OUT .N ." out "
            DUP K.FOOT .N ." foot "
            K.FLAGS IF 3 FG ." [tile]" RESET-COLOR ELSE DIM ." [cpu]" RESET-COLOR THEN
            CR
        LOOP
    THEN ;

\ ---- Screen 4: Pipelines ----
: SCR-PIPES  ( -- )
    .LABEL ."  Pipelines (" PIPE-COUNT @ .N ." )" ./LABEL CR CR
    PIPE-COUNT @ DUP 0= IF
        DROP ."   (none registered)" CR
    ELSE
        0 DO
            ."   " I .N ."  "
            I CELLS PIPE-TABLE + @
            ." cap=" DUP P.CAP .N
            ."  steps=" P.COUNT .N
            CR
        LOOP
    THEN ;

\ ---- Screen 5: Tasks ----
: SCR-TASKS  ( -- )
    .LABEL ."  Tasks (" TASK-COUNT @ .N ." )" ./LABEL CR CR
    TASK-COUNT @ DUP 0= IF
        DROP ."   (none registered)" CR
        0 SCR-MAX !
    ELSE
        DUP SCR-MAX !
        0 DO
            SCR-SEL @ I = IF 2 FG ." > " RESET-COLOR ELSE ."   " THEN
            I .N ."  "
            I CELLS TASK-TABLE + @
            DUP T.STATUS
            DUP 0 = IF DIM ." FREE " RESET-COLOR THEN
            DUP 1 = IF 2 FG ." READY" RESET-COLOR THEN
            DUP 2 = IF 3 FG ." RUN  " RESET-COLOR THEN
            DUP 3 = IF 1 FG ." BLOCK" RESET-COLOR THEN
            DUP 4 = IF DIM ." DONE " RESET-COLOR THEN
            DROP
            ."  pri=" DUP T.PRIORITY .N
            ."  xt=" T.XT .N
            CR
        LOOP
    THEN
    \ -- Inline detail for selected task --
    SCR-SEL @ -1 <> SCR-SEL @ TASK-COUNT @ < AND IF
        CR HBAR
        SCR-SEL @ CELLS TASK-TABLE + @
        ."  Status: " DUP T.STATUS
        DUP 0 = IF ." FREE" THEN
        DUP 1 = IF 2 FG ." READY" RESET-COLOR THEN
        DUP 2 = IF 3 FG ." RUNNING" RESET-COLOR THEN
        DUP 3 = IF 1 FG ." BLOCKED" RESET-COLOR THEN
        DUP 4 = IF DIM ." DONE" RESET-COLOR THEN
        DROP CR
        ."  XT: " DUP T.XT .N ."   Priority: " T.PRIORITY .N CR
        CR DIM ."  [k] Kill  [s] Restart" RESET-COLOR CR
    THEN ;

\ ---- Screen 6: Help ----
: SCR-HELP  ( -- )
    .LABEL ."  Quick Reference" ./LABEL CR CR
    BOLD ."  Buffers:" RESET-COLOR CR
    ."   0 1 N BUFFER name    Create buffer" CR
    ."   buf B.SUM/MIN/MAX    Tile reductions" CR
    ."   a b c B.ADD/SUB      Element-wise ops" CR
    ."   n buf B.SCALE/FILL   Modify buffer" CR
    BOLD ."  Kernels:" RESET-COLOR CR
    ."   1 1 2 0 KERNEL name  Register kernel" CR
    ."   buf kzero/kfill/kadd Sample kernels" CR
    ."   buf knorm/khistogram  Advanced kernels" CR
    ."   th src dst kpeak      Peak detection" CR
    BOLD ."  Pipelines:" RESET-COLOR CR
    ."   3 PIPELINE name      Create pipeline" CR
    ."   ' w pipe P.ADD/RUN   Build & execute" CR
    ."   pipe P.RUN-PAR       Parallel execute" CR
    BOLD ."  Tasks:" RESET-COLOR CR
    ."   ' w 0 TASK name      Create task" CR
    ."   SCHEDULE / BG         Run tasks" CR
    BOLD ."  Multicore:" RESET-COLOR CR
    ."   xt core CORE-RUN      Dispatch to core" CR
    ."   core CORE-WAIT        Wait for core" CR
    ."   BARRIER               Sync all cores" CR
    ."   n LOCK / n UNLOCK     Spinlock ops" CR
    ."   CORES                 Show core status" CR
    BOLD ."  Storage:" RESET-COLOR CR
    ."   buf sec B.SAVE/LOAD  Persist buffers" CR
    ."   DIR / CATALOG        List disk files" CR
    ."   CAT name             Print file" CR
    ."   buf SAVE-BUFFER name Save buf to file" CR
    BOLD ."  Data Ports:" RESET-COLOR CR
    ."   buf id PORT!          Bind NIC source" CR
    ."   POLL / n INGEST       Receive frames" CR
    ."   PORTS                 List bindings" CR
    BOLD ."  Tools:" RESET-COLOR CR
    ."   DASHBOARD / STATUS    System views" CR
    ."   ' w BENCH / .BENCH   Benchmark" CR ;

\ ---- Screen 7: Documentation ----
: SCR-DOCS  ( -- )
    .LABEL ."  Documentation" ./LABEL CR CR
    0 DOC-N !
    BOLD ."  Topics:" RESET-COLOR CR
    FS-OK @ IF
        0 DOC-TUT-COUNT !
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE FTYPE-DOC = IF
                    SCR-SEL @ DOC-N @ = IF 2 FG ." > " RESET-COLOR ELSE ."    " THEN
                    DOC-N @ .N ."  " I DIRENT .ZSTR CR
                    1 DOC-N +!  1 DOC-TUT-COUNT +!
                THEN
            THEN
        LOOP
        DOC-TUT-COUNT @ 0= IF ."    (none)" CR THEN
    ELSE
        ."    (no filesystem loaded)" CR
    THEN
    CR
    BOLD ."  Tutorials:" RESET-COLOR CR
    FS-OK @ IF
        0 DOC-TUT-COUNT !
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE FTYPE-TUT = IF
                    SCR-SEL @ DOC-N @ = IF 2 FG ." > " RESET-COLOR ELSE ."    " THEN
                    DOC-N @ .N ."  " I DIRENT .ZSTR CR
                    1 DOC-N +!  1 DOC-TUT-COUNT +!
                THEN
            THEN
        LOOP
        DOC-TUT-COUNT @ 0= IF ."    (none)" CR THEN
    ELSE
        ."    (no filesystem loaded)" CR
    THEN
    DOC-N @ SCR-MAX !
    CR
    DIM ."  [Enter] Read selected document" RESET-COLOR CR ;

\ ---- Screen 8: Storage ----
: SCR-STORAGE  ( -- )
    .LABEL ."  Storage" ./LABEL CR CR
    DISK? 0= IF
        ."   (no storage attached)" CR
        0 SCR-MAX ! EXIT
    THEN
    FS-OK @ 0= IF
        ."   (filesystem not loaded)" CR
        0 SCR-MAX ! EXIT
    THEN
    0 STOR-N !
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            SCR-SEL @ STOR-N @ = IF 2 FG ." > " RESET-COLOR ELSE ."   " THEN
            STOR-N @ .N ."  "
            I DIRENT .ZSTR
            ."  " I DIRENT DE.USED .N ." B"
            ."  " I DIRENT DE.TYPE .FTYPE
            CR
            1 STOR-N +!
        THEN
    LOOP
    STOR-N @ SCR-MAX !
    STOR-N @ 0= IF ."   (empty)" CR THEN
    CR
    0  2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DIM ."  " .N ."  free sectors" RESET-COLOR CR
    \ -- Inline detail for selected file --
    SCR-SEL @ -1 <> SCR-SEL @ STOR-N @ < AND IF
        CR HBAR
        SCR-SEL @ FIND-NTH-ACTIVE DUP -1 <> IF
            ."  Name  : " DUP DIRENT .ZSTR CR
            ."  Type  : " DUP DIRENT DE.TYPE .FTYPE CR
            ."  Size  : " DUP DIRENT DE.USED .N ."  bytes" CR
            ."  Start : sector " DUP DIRENT DE.SEC .N CR
            ."  Count : " DIRENT DE.COUNT .N ."  sectors" CR
        ELSE DROP THEN
    THEN ;

\ ---- Screen 9: Cores ----
: SCR-CORES  ( -- )
    .LABEL ."  Cores (" NCORES .N ." )" ./LABEL CR CR
    NCORES 1 <= IF
        ."   Single-core mode — no secondary cores available." CR
    ELSE
        NCORES 0 DO
            ."   Core " I .N ."  "
            I COREID = IF
                3 FG ." RUNNING" RESET-COLOR ."  (self — scheduler)" CR
            ELSE
                I CORE-STATUS IF
                    2 FG ." BUSY" RESET-COLOR CR
                ELSE
                    DIM ." IDLE" RESET-COLOR CR
                THEN
            THEN
        LOOP
        CR BOLD ."  Multicore Words:" RESET-COLOR CR
        ."   xt core CORE-RUN    Dispatch work to core" CR
        ."   core CORE-WAIT      Wait for core to finish" CR
        ."   BARRIER             Sync all secondary cores" CR
        ."   pipe P.RUN-PAR      Parallel pipeline execute" CR
        ."   n LOCK / n UNLOCK   Spinlock operations" CR
    THEN ;

\ -- Screen dispatch --
: RENDER-SCREEN  ( -- )
    PAGE SCREEN-HEADER
    SCREEN-ID @
    DUP 1 = IF DROP SCR-HOME    ELSE
    DUP 2 = IF DROP SCR-BUFFERS ELSE
    DUP 3 = IF DROP SCR-KERNELS ELSE
    DUP 4 = IF DROP SCR-PIPES   ELSE
    DUP 5 = IF DROP SCR-TASKS   ELSE
    DUP 6 = IF DROP SCR-HELP    ELSE
    DUP 7 = IF DROP SCR-DOCS    ELSE
    DUP 8 = IF DROP SCR-STORAGE ELSE
    DUP 9 = IF DROP SCR-CORES   ELSE
        DROP SCR-HOME
    THEN THEN THEN THEN THEN THEN THEN THEN THEN
    CR SCREEN-FOOTER ;

\ -- Screen switch helper --
: SWITCH-SCREEN  ( n -- )
    DUP SCREEN-ID !
    DUP 2 = OVER 5 = OR OVER 7 = OR SWAP 8 = OR
    IF 0 ELSE -1 THEN SCR-SEL !
    0 SCR-MAX !
    RENDER-SCREEN ;

\ -- Navigation predicates --
: SCREEN-SELECTABLE?  ( -- flag )
    SCREEN-ID @ 2 = SCREEN-ID @ 5 = OR
    SCREEN-ID @ 7 = OR SCREEN-ID @ 8 = OR ;

\ -- Activate selected item --
: DO-SELECT  ( -- )
    SCREEN-ID @ 7 = IF SCR-SEL @ SHOW-NTH-DOC THEN ;

\ -- Event loop: poll KEY?, dispatch on keypress --
: HANDLE-KEY  ( c -- )
    DUP 49 = IF DROP 1 SWITCH-SCREEN EXIT THEN  \ '1'
    DUP 50 = IF DROP 2 SWITCH-SCREEN EXIT THEN  \ '2'
    DUP 51 = IF DROP 3 SWITCH-SCREEN EXIT THEN  \ '3'
    DUP 52 = IF DROP 4 SWITCH-SCREEN EXIT THEN  \ '4'
    DUP 53 = IF DROP 5 SWITCH-SCREEN EXIT THEN  \ '5'
    DUP 54 = IF DROP 6 SWITCH-SCREEN EXIT THEN  \ '6'
    DUP 55 = IF DROP 7 SWITCH-SCREEN EXIT THEN  \ '7'
    DUP 56 = IF DROP 8 SWITCH-SCREEN EXIT THEN  \ '8'
    DUP 57 = IF DROP 9 SWITCH-SCREEN EXIT THEN  \ '9'
    DUP 113 = IF DROP 0 SCREEN-RUN ! EXIT THEN  \ 'q'
    DUP 114 = IF DROP RENDER-SCREEN EXIT THEN    \ 'r'
    DUP 97 = IF DROP                              \ 'a' = toggle auto-refresh
        AUTO-REFRESH @ IF 0 ELSE -1 THEN AUTO-REFRESH !
        RENDER-SCREEN EXIT
    THEN
    DUP 110 = IF DROP                             \ 'n' = next item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1+ DUP SCR-MAX @ >= IF DROP 0 THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 112 = IF DROP                             \ 'p' = prev item
        SCREEN-SELECTABLE? IF
            SCR-SEL @ 1- DUP 0< IF
                DROP SCR-MAX @ 1- DUP 0< IF DROP 0 THEN
            THEN
            SCR-SEL !  RENDER-SCREEN
        THEN EXIT
    THEN
    DUP 13 = OVER 32 = OR IF DROP                 \ ENTER / SPACE = activate
        SCREEN-SELECTABLE? IF
            SCR-SEL @ -1 <> IF
                DO-SELECT RENDER-SCREEN
            THEN
        THEN EXIT
    THEN
    \ -- Task-specific actions --
    SCREEN-ID @ 5 = IF
        DUP 107 = IF DROP                         \ 'k' = kill task
            SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
                CELLS TASK-TABLE + @ KILL
                RENDER-SCREEN
            ELSE DROP THEN EXIT
        THEN
        DUP 115 = IF DROP                         \ 's' = restart task
            SCR-SEL @ DUP -1 <> OVER TASK-COUNT @ < AND IF
                CELLS TASK-TABLE + @ RESTART
                RENDER-SCREEN
            ELSE DROP THEN EXIT
        THEN
    THEN
    DROP ;

\ -- Main TUI entry point --
: SCREENS  ( -- )
    1 SCREEN-ID !
    1 SCREEN-RUN !
    -1 SCR-SEL !  0 SCR-MAX !
    CYCLES REFRESH-LAST !
    RENDER-SCREEN
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
    ." Returned to REPL." CR ;

\ =====================================================================
\  §10  Data Ports — NIC-Based External Data Ingestion
\ =====================================================================
\
\  Frame protocol (6-byte header + payload, fits NIC MTU of 1500):
\    +0  u8   SRC_ID       source identifier (0-255)
\    +1  u8   DTYPE        data type (0=raw 1=u8 2=u16 3=u64 4=text 5=cmd)
\    +2  u16  SEQ          sequence number (LE)
\    +4  u16  PAYLOAD_LEN  payload byte count (LE)
\    +6  ...  PAYLOAD      data bytes
\
\  Data flows in via NIC (emulated or real UDP), gets routed to buffers.
\  Same Forth code works on real hardware — just swap the NIC.
\
\  Python side: data_sources.py provides SineSource, CounterSource, etc.
\  that inject frames via system.nic.inject_frame() or UDP.

\ -- Constants --
6 CONSTANT /FRAME-HDR

\ -- Frame receive buffer (1500 bytes = NIC MTU) --
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

\ -- Frame header accessors (valid after NET-RECV into FRAME-BUF) --
: FRAME-SRC   ( -- id )    FRAME-BUF C@ ;
: FRAME-TYPE  ( -- type )  FRAME-BUF 1 + C@ ;
: FRAME-SEQ   ( -- seq )   FRAME-BUF 2 + C@  FRAME-BUF 3 + C@ 256 * + ;
: FRAME-LEN   ( -- len )   FRAME-BUF 4 + C@  FRAME-BUF 5 + C@ 256 * + ;
: FRAME-DATA  ( -- addr )  FRAME-BUF /FRAME-HDR + ;

\ -- Receive one frame into FRAME-BUF, return raw byte count --
: RECV-FRAME  ( -- len )   FRAME-BUF NET-RECV ;

\ -- Route: receive frame, copy payload to bound buffer, return src_id --
: ROUTE-FRAME  ( -- id|-1 )
    NET-RX? 0= IF -1 EXIT THEN
    RECV-FRAME DUP 0= IF DROP -1 EXIT THEN
    DROP  \ discard raw length — we parse the header
    FRAME-SRC PORT@ DUP 0= IF
        DROP  1 PORT-DROP +!  -1
    ELSE
        ROUTE-BUF !
        FRAME-DATA  ROUTE-BUF @ B.DATA
        FRAME-LEN  ROUTE-BUF @ B.BYTES  MIN
        CMOVE
        1 PORT-RX +!
        FRAME-SRC
    THEN ;

\ -- High-level words --
: POLL    ( -- id|-1 )       ROUTE-FRAME ;
: INGEST  ( n -- received )
    0 SWAP   \ ( count n )
    0 DO POLL -1 <> IF 1 + THEN LOOP ;

\ -- Debug: print last received frame header --
: .FRAME  ( -- )
    ." src=" FRAME-SRC .
    ." type=" FRAME-TYPE .
    ." seq=" FRAME-SEQ .
    ." len=" FRAME-LEN . CR ;

\ -- List bound ports --
: PORTS  ( -- )
    ." --- Ports (" PORT-COUNT @ . ." ) ---" CR
    256 0 DO
        I PORT@ DUP 0<> IF
            ."   src=" I . ."  -> buf @" . CR
        ELSE DROP THEN
    LOOP
    ."   rx=" PORT-RX @ . ." drop=" PORT-DROP @ . CR ;

\ -- Port stats one-liner --
: PORT-STATS  ( -- )
    ." ports=" PORT-COUNT @ .
    ." rx=" PORT-RX @ .
    ." drop=" PORT-DROP @ . ;

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

\ -- Memory usage bar --
: .MEM  ( -- )
    ."  Memory:" CR
    ."    HERE  = " HERE . CR ;

\ -- Dashboard --
: DASHBOARD ( -- )
    CR HRULE
    ."  KDOS v1.1 — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR ."  Cores: " NCORES .
    NCORES 1 > IF ." (multicore)" ELSE ." (single-core)" THEN CR
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
    ." KDOS v1.1 | cores=" NCORES .
    ." bufs=" BUF-COUNT @ .
    ." kerns=" KERN-COUNT @ .
    ." pipes=" PIPE-COUNT @ .
    ." tasks=" TASK-COUNT @ .
    ." files=" FILE-COUNT @ .
    ." ports=" PORT-COUNT @ .
    ." disk=" DISK? IF ." yes" ELSE ." no" THEN
    ."  HERE=" HERE . CR ;

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
        2 FG ." Found: " RESET-COLOR
        NAMEBUF .ZSTR ."  — defined in dictionary" CR
    ELSE
        1 FG ." Not found: " RESET-COLOR
        NAMEBUF .ZSTR ."  — not in dictionary" CR
    THEN
    \ Check for matching doc on disk
    FS-OK @ IF
        FIND-BY-NAME DUP -1 <> IF
            CR ."  Documentation available:" CR
            DUP DIRENT DE.TYPE .FTYPE ."  file: "
            DIRENT .ZSTR CR
            ."  Use: DOC " NAMEBUF .ZSTR ."  or  DESCRIBE " NAMEBUF .ZSTR CR
        ELSE
            DROP
        THEN
    THEN
    \ Show related words
    CR ."  Related words:" CR ."  "
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
    DUP 0= IF ." (none)" THEN
    CR ." (" . ." related)" CR ;

\ -- Full reference --
: .HELP-ALL  ( -- )
    CR HRULE
    ."  KDOS v1.1 — Quick Reference" CR
    HRULE
    CR ."  BUFFER WORDS:" CR
    ."    0 1 256 BUFFER name   Create 256-byte raw buffer" CR
    ."    buf B.INFO             Show buffer descriptor" CR
    ."    buf B.PREVIEW          Hex dump first tile" CR
    ."    byte buf B.FILL        Fill buffer with byte" CR
    ."    buf B.ZERO             Zero buffer" CR
    ."    buf B.SUM              Sum all bytes (via tile engine)" CR
    ."    buf B.MIN              Minimum byte (via tile engine)" CR
    ."    buf B.MAX              Maximum byte (via tile engine)" CR
    ."    a b c B.ADD            Element-wise add a+b -> c" CR
    ."    a b c B.SUB            Element-wise sub a-b -> c" CR
    ."    n buf B.SCALE          Multiply each byte by n" CR
    ."    BUFFERS                List all buffers" CR
    CR ."  KERNEL WORDS:" CR
    ."    1 1 2 0 KERNEL name   Register kernel metadata" CR
    ."    desc K.INFO            Show kernel descriptor" CR
    ."    KERNELS                List all kernels" CR
    CR ."  SAMPLE KERNELS:" CR
    ."    buf kzero              Zero a buffer" CR
    ."    byte buf kfill         Fill a buffer" CR
    ."    a b c kadd             Add two buffers" CR
    ."    buf ksum               Sum buffer -> stack" CR
    ."    buf kstats             Sum, min, max -> stack" CR
    ."    n buf kscale           Scale buffer by n" CR
    ."    n buf kthresh          Threshold: <n->0, >=n->255" CR
    CR ."  ADVANCED KERNELS:" CR
    ."    lo hi buf kclamp       Clamp bytes to [lo,hi]" CR
    ."    w buf kavg             Moving average (window w)" CR
    ."    buf khistogram         256-bin histogram -> hist-bins" CR
    ."    v HIST@                Query histogram bin v" CR
    ."    .HIST                  Print non-zero histogram bins" CR
    ."    src dst kdelta         Delta encode src -> dst" CR
    ."    buf knorm              Normalize to full 0-255 range" CR
    ."    th src dst kpeak       Peak detect (thresh th)" CR
    ."    buf krms-buf           RMS of buffer -> stack" CR
    ."    a b kcorrelate         Dot product (tile engine)" CR
    ."    c0 c1 c2 buf kconvolve3  3-tap FIR convolution" CR
    ."    buf kinvert            Bitwise invert (255-val)" CR
    ."    val buf kcount         Count matching bytes" CR
    CR ."  PIPELINE WORDS:" CR
    ."    3 PIPELINE name        Create 3-step pipeline" CR
    ."    ' word pipe P.ADD      Append step to pipeline" CR
    ."    pipe P.RUN             Execute all steps" CR
    ."    pipe P.BENCH           Time each step" CR
    ."    pipe P.INFO            Show pipeline descriptor" CR
    ."    pipe P.CLEAR           Reset pipeline" CR
    ."    PIPES                  List all pipelines" CR
    CR ."  STORAGE WORDS:" CR
    ."    DISK?                  Is storage present?" CR
    ."    DISK-INFO              Print storage status" CR
    ."    buf sec B.SAVE         Save buffer to disk" CR
    ."    buf sec B.LOAD         Load buffer from disk" CR
    CR ."  MP64FS FILE SYSTEM:" CR
    ."    FORMAT                 Format disk with MP64FS" CR
    ."    FS-LOAD                Load FS from disk into RAM" CR
    ."    FS-SYNC                Write FS changes to disk" CR
    ."    FS-FREE                Show free disk space" CR
    ."    DIR                    List files on disk" CR
    ."    CATALOG                Detailed file listing" CR
    ."    8 2 MKFILE name        Create file (8 secs, type 2)" CR
    ."    RMFILE name            Delete file from disk" CR
    ."    RENAME old new         Rename a file" CR
    ."    CAT name               Print file to terminal" CR
    ."    OPEN name              Open file -> fdesc" CR
    ."    f FFLUSH               Write metadata to disk" CR
    ."    buf SAVE-BUFFER name   Save buffer to file" CR
    CR ."  FILE I/O:" CR
    ."    10 8 FILE name         Create manual file (legacy)" CR
    ."    addr len f FWRITE      Write bytes (advances cursor)" CR
    ."    addr len f FREAD       Read bytes (advances cursor)" CR
    ."    pos f FSEEK            Set file cursor" CR
    ."    f FREWIND              Reset cursor to 0" CR
    ."    f FSIZE / f F.INFO     File size / info" CR
    ."    FILES                  List legacy files" CR
    CR ."  SCHEDULER WORDS:" CR
    ."    ' word 0 TASK name     Create named task (xt pri)" CR
    ."    xt SPAWN               Spawn anonymous task" CR
    ."    xt BG                  Spawn + run scheduler" CR
    ."    SCHEDULE               Run all ready tasks" CR
    ."    YIELD                  Cooperative yield" CR
    ."    tdesc KILL             Cancel task" CR
    ."    tdesc RESTART          Reset done task to ready" CR
    ."    PREEMPT-ON             Enable timer preemption" CR
    ."    PREEMPT-OFF            Disable timer preemption" CR
    ."    TASKS                  List all tasks" CR
    CR ."  MULTICORE WORDS:" CR
    ."    COREID                 Push current core ID" CR
    ."    NCORES                 Push number of hardware cores" CR
    ."    xt core CORE-RUN       Dispatch XT to secondary core" CR
    ."    core CORE-WAIT         Wait for core to finish" CR
    ."    ALL-CORES-WAIT         Wait for all secondary cores" CR
    ."    BARRIER                Synchronize all cores" CR
    ."    n LOCK                 Acquire spinlock n (busy-wait)" CR
    ."    n UNLOCK               Release spinlock n" CR
    ."    CORES                  Show per-core status" CR
    ."    pipe P.RUN-PAR         Run pipeline in parallel" CR
    ."    pipe P.BENCH-PAR       Benchmark parallel pipeline" CR
    CR ."  DATA PORT WORDS:" CR
    ."    buf id PORT!           Bind source id to buffer" CR
    ."    id UNPORT              Unbind source" CR
    ."    POLL                   Receive & route one frame" CR
    ."    n INGEST               Receive & route n frames" CR
    ."    NET-RX?                Is a NIC frame waiting?" CR
    ."    PORTS                  List port bindings" CR
    ."    .FRAME                 Show last frame header" CR
    CR ."  SCREENS & TOOLS:" CR
    ."    SCREENS                Interactive TUI (1-9, n/p, a, q, r)" CR
    ."    DASHBOARD              Full system overview" CR
    ."    STATUS                 Quick status line" CR
    ."    ' word BENCH           Time word, leave cycles on stack" CR
    ."    ' word .BENCH          Time word and print cycles" CR
    ."    HELP                   Full quick reference" CR
    ."    HELP <word>            Look up a specific word" CR
    CR ."  DOCUMENTATION:" CR
    ."    TOPICS                 List available doc topics" CR
    ."    LESSONS                List available tutorials" CR
    ."    DOC <topic>            Page through documentation" CR
    ."    DESCRIBE <topic>       Show topic by name" CR
    ."    TUTORIAL <name>        Interactive lesson" CR
    CR ."  DICTIONARY SEARCH:" CR
    ."    WORDS-LIKE <pat>       Find words containing pattern" CR
    ."    APROPOS <pat>          Alias for WORDS-LIKE" CR
    ."    n .RECENT              Show last n defined words" CR
    ."    LATEST                 Push most-recent dict entry addr" CR
    ."    entry ENTRY>NAME       Get name (addr len) from entry" CR
    ."    entry ENTRY>LINK       Follow dict link to next entry" CR
    CR ."  PIPELINE BUNDLES:" CR
    ."    1 BDL-BEGIN             Start bundle (version 1)" CR
    ."    0 1 256 BDL-BUF name   Declare buffer in bundle" CR
    ."    1 1 2 1 BDL-KERN name  Declare kernel in bundle" CR
    ."    3 BDL-PIPE name         Declare pipeline in bundle" CR
    ."    0 50000 3 BDL-SCHED    Set schedule (pipe int flags)" CR
    ."    0 0 3 BDL-POLICY       Set policy (perms ret exp)" CR
    ."    1 255 BDL-SCREEN        Set screen (default mask)" CR
    ."    BDL-END                 Finalize bundle" CR
    ."    BUNDLE-LOAD name       Load bundle from disk" CR
    ."    BUNDLE-INFO name       Inspect bundle (dry run)" CR
    ."    .BUNDLE                Show current bundle state" CR
    CR ."  STACK & DIAGNOSTICS:" CR
    ."    n NEEDS                Abort if stack has < n items" CR
    ."    flag ASSERT            Abort if flag is false" CR
    ."    .DEPTH                 Show current stack depth" CR
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
        ." --- Bundle v" BDL-VER @ . ." ---" CR
        ."   Buffers  : " BDL-NBUFS @ . CR
        ."   Kernels  : " BDL-NKERNS @ . CR
        ."   Pipelines: " BDL-NPIPES @ . CR
        BDL-SCHED-P @ -1 <> IF
            ."   Schedule : pipe " BDL-SCHED-P @ .
            ."  interval=" BDL-SCHED-I @ .
            BDL-SCHED-F @ 1 AND IF ."  auto" THEN
            BDL-SCHED-F @ 2 AND IF ."  repeat" THEN
            CR
        ELSE
            ."   Schedule : (none)" CR
        THEN
        ."   Policy   : perms=" BDL-POL-PERM @ .
        ." ret=" BDL-POL-RET @ .
        ." exp=" BDL-POL-EXP @ . CR
        ."   Screen   : default=" BDL-SCR-DEF @ .
        ." mask=" BDL-SCR-MASK @ . CR
    ELSE
        \ Apply schedule interval to TIME-SLICE
        BDL-SCHED-P @ -1 <> IF
            BDL-SCHED-I @ TIME-SLICE !
        THEN
        \ Apply screen default
        BDL-SCR-DEF @ SCREEN-ID !
        \ Report
        CR ." Bundle v" BDL-VER @ . ." loaded: "
        BDL-NBUFS @ . ." bufs "
        BDL-NKERNS @ . ." kerns "
        BDL-NPIPES @ . ." pipes" CR
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
    ." --- Current Bundle ---" CR
    BDL-VER @ 0= IF
        ."   (no bundle loaded)" CR EXIT
    THEN
    ."   Version  : " BDL-VER @ . CR
    ."   Buffers  : " BDL-NBUFS @ . CR
    ."   Kernels  : " BDL-NKERNS @ . CR
    ."   Pipelines: " BDL-NPIPES @ . CR
    ."   Schedule : "
    BDL-SCHED-P @ -1 = IF ." none" ELSE
        ." pipe " BDL-SCHED-P @ .
        ." interval=" BDL-SCHED-I @ .
        BDL-SCHED-F @ 1 AND IF ." [auto]" THEN
        BDL-SCHED-F @ 2 AND IF ." [repeat]" THEN
    THEN CR
    ."   Policy   : "
    BDL-POL-PERM @ 1 AND IF ." RO" THEN
    BDL-POL-PERM @ 2 AND IF ." SYS" THEN
    ." ret=" BDL-POL-RET @ .
    ." exp=" BDL-POL-EXP @ . CR
    ."   Screen   : default=" BDL-SCR-DEF @ .
    ." mask=" BDL-SCR-MASK @ . CR ;

\ =====================================================================
\  §16  Network Stack — Ethernet Framing
\ =====================================================================
\
\  Bottom-up network stack built on the NIC hardware (§10 data ports).
\  §16 covers Ethernet II frame layout: MAC addresses, EtherType,
\  constants, and frame build/parse words.
\
\  Ethernet II frame layout (no VLAN, no FCS — NIC handles FCS):
\    +0   6 bytes   Destination MAC
\    +6   6 bytes   Source MAC
\    +12  2 bytes   EtherType (big-endian)
\    +14  ...       Payload (46–1486 bytes; MTU 1500 - 14 = 1486)
\
\  All multi-byte network fields are big-endian (network byte order).

\ -- Network byte order helpers (big-endian 16-bit) --
: N>H  ( be16 -- he16 )   DUP 8 RSHIFT SWAP 255 AND 8 LSHIFT OR ;
: H>N  ( he16 -- be16 )   N>H ;   \ same swap operation

\ -- Big-endian 16-bit memory access --
: NW@  ( addr -- u16 )   DUP C@ 8 LSHIFT  SWAP 1 + C@ OR ;
: NW!  ( u16 addr -- )   OVER 8 RSHIFT OVER C!  1 + SWAP 255 AND SWAP C! ;

\ -- Ethernet constants --
6  CONSTANT /MAC            \ MAC address length
14 CONSTANT /ETH-HDR        \ Ethernet header length (no VLAN)
1500 CONSTANT ETH-MTU       \ Maximum frame size
1486 CONSTANT ETH-MAX-PLD   \ Maximum payload (MTU - header)

\ -- EtherType constants (big-endian values as seen on wire) --
2048  CONSTANT ETYPE-IP4    \ 0x0800  IPv4
2054  CONSTANT ETYPE-ARP    \ 0x0806  ARP
34525 CONSTANT ETYPE-IP6    \ 0x86DD  IPv6 (reserved, not implemented)

\ -- Broadcast MAC --
CREATE MAC-BCAST  255 C, 255 C, 255 C, 255 C, 255 C, 255 C,

\ -- Our MAC address (6 bytes, copied from NIC at init) --
CREATE MY-MAC  6 ALLOT
: MAC-INIT  ( -- )   NET-MAC@ MY-MAC 6 CMOVE ;

\ -- Ethernet TX frame buffer (MTU bytes) --
CREATE ETH-TX-BUF  ETH-MTU ALLOT

\ -- Ethernet RX frame buffer (MTU bytes) --
CREATE ETH-RX-BUF  ETH-MTU ALLOT

\ -- Frame header field accessors (given frame base address) --
: ETH-DST   ( frame -- addr )  ;               \ +0
: ETH-SRC   ( frame -- addr )  6 + ;           \ +6
: ETH-TYPE  ( frame -- etype ) 12 + NW@ ;      \ +12, big-endian
: ETH-TYPE! ( etype frame -- ) 12 + NW! ;      \ +12, big-endian
: ETH-PLD   ( frame -- addr )  /ETH-HDR + ;    \ +14

\ -- MAC comparison --
: MAC=  ( addr1 addr2 -- flag )   6 SAMESTR? ;

\ -- Print MAC address --
: .MAC  ( addr -- )
    BASE @ >R HEX
    6 0 DO
        DUP I + C@
        DUP 16 < IF 48 EMIT THEN   \ leading zero for single hex digit
        .
        I 5 < IF 58 EMIT THEN      \ colon separator
    LOOP DROP
    R> BASE ! ;

\ -- Debug: print Ethernet frame header --
: .ETH  ( frame -- )
    ." dst=" DUP ETH-DST .MAC
    ."  src=" DUP ETH-SRC .MAC
    ."  type=" ETH-TYPE . CR ;

\ -- ETH-BUILD: construct an Ethernet frame in a buffer --
\   ( dst-mac src-mac etype payload pay-len frame -- total-len )
\   Copies dst MAC, src MAC, writes EtherType, copies payload.
\   Returns total frame length (14 + pay-len).
VARIABLE _EB-LEN
: ETH-BUILD  ( dst src etype payload paylen frame -- total )
    >R                              \ save frame addr
    DUP _EB-LEN !                   \ save paylen
    R@ /ETH-HDR + SWAP CMOVE       \ copy payload to frame+14
    R@ ETH-TYPE!                    \ write EtherType (big-endian)
    R@ ETH-SRC /MAC CMOVE          \ copy src MAC to frame+6
    R@ ETH-DST /MAC CMOVE          \ copy dst MAC to frame+0
    R> DROP                         \ discard frame addr
    _EB-LEN @ /ETH-HDR + ;         \ paylen + 14 = total length

\ -- ETH-BUILD-TX: build frame in ETH-TX-BUF with MY-MAC as source --
\   ( dst-mac etype payload paylen -- total-len )
VARIABLE _ETB-ETYPE
VARIABLE _ETB-PAY
VARIABLE _ETB-PAYLEN
: ETH-BUILD-TX  ( dst etype payload paylen -- total )
    _ETB-PAYLEN !  _ETB-PAY !  _ETB-ETYPE !
    \ stack: dst
    MY-MAC _ETB-ETYPE @ _ETB-PAY @ _ETB-PAYLEN @
    ETH-TX-BUF ETH-BUILD ;

\ -- ETH-PARSE: extract fields from a received frame in ETH-RX-BUF --
\   ( frame -- dst-mac src-mac etype payload paylen )
\   Pushes field addresses/values for inspection.
\   paylen must be computed from total frame length minus /ETH-HDR.
\   This word pushes addresses — caller uses the accessors.

VARIABLE ETH-RX-LEN   0 ETH-RX-LEN !   \ last received frame length

: ETH-FRAME-PAYLEN  ( -- n )   ETH-RX-LEN @ /ETH-HDR - 0 MAX ;
: ETH-IS-IP4?   ( frame -- flag )   ETH-TYPE ETYPE-IP4 = ;
: ETH-IS-ARP?   ( frame -- flag )   ETH-TYPE ETYPE-ARP = ;
: ETH-FOR-US?   ( frame -- flag )
    DUP ETH-DST MY-MAC MAC=         \ unicast to us?
    SWAP ETH-DST MAC-BCAST MAC= OR ; \ or broadcast?

\ -- ETH-SEND: transmit an Ethernet frame via NIC DMA --
\   ( frame len -- )
\   Uses NET-SEND BIOS primitive (sets DMA addr + len, issues SEND cmd).
: ETH-SEND  ( frame len -- )   NET-SEND ;

\ -- ETH-SEND-TX: build and send in one step --
\   ( dst etype payload paylen -- )
: ETH-SEND-TX  ( dst etype payload paylen -- )
    ETH-BUILD-TX                    \ ( total-len )
    ETH-TX-BUF SWAP NET-SEND ;     \ send from ETH-TX-BUF

\ -- Transmit statistics --
VARIABLE ETH-TX-COUNT   0 ETH-TX-COUNT !

: ETH-SEND-COUNTED  ( frame len -- )
    NET-SEND  1 ETH-TX-COUNT +! ;

\ -- ETH-RECV: receive an Ethernet frame from NIC into ETH-RX-BUF --
\   ( -- len | 0 )
\   Returns frame length or 0 if no frame available.
\   Stores the length in ETH-RX-LEN for ETH-FRAME-PAYLEN.
VARIABLE ETH-RX-COUNT   0 ETH-RX-COUNT !

: ETH-RECV  ( -- len )
    NET-RX? 0= IF 0 EXIT THEN       \ no frame waiting
    ETH-RX-BUF NET-RECV             \ receive into ETH-RX-BUF
    DUP ETH-RX-LEN !               \ save length
    DUP 0<> IF 1 ETH-RX-COUNT +! THEN ;

\ -- ETH-RECV-WAIT: blocking receive with timeout (in attempts) --
\   ( max-attempts -- len | 0 )
: ETH-RECV-WAIT  ( n -- len )
    0 DO
        ETH-RECV DUP 0<> IF UNLOOP EXIT THEN
        DROP
    LOOP
    0 ;

\ -- ETH-RECV-FILTER: receive, keep only frames for us --
\   ( -- len | 0 )
: ETH-RECV-FILTER  ( -- len )
    ETH-RECV DUP 0= IF EXIT THEN     \ no frame → 0
    ETH-RX-BUF ETH-FOR-US? 0= IF
        DROP 0                        \ not for us → discard
    THEN ;

\ -- Network statistics --
: .NET-STATS  ( -- )
    ." tx=" ETH-TX-COUNT @ .
    ." rx=" ETH-RX-COUNT @ . CR ;

\ =====================================================================
\  §16.1  ARP — Address Resolution Protocol
\ =====================================================================
\
\  ARP maps IPv4 addresses to MAC addresses.  We maintain a small
\  static table (8 entries) and support request/reply for resolution.
\
\  ARP table entry layout (16 bytes):
\    +0   4 bytes   IPv4 address (network byte order)
\    +4   6 bytes   MAC address
\    +10  2 bytes   flags: bit0 = valid
\    +12  4 bytes   (reserved/padding)
\
\  ARP packet layout (28 bytes, inside Ethernet payload):
\    +0   2 bytes   HTYPE (0x0001 = Ethernet)
\    +2   2 bytes   PTYPE (0x0800 = IPv4)
\    +4   1 byte    HLEN  (6)
\    +5   1 byte    PLEN  (4)
\    +6   2 bytes   OPER  (1=request, 2=reply)
\    +8   6 bytes   SHA   (sender hardware address)
\    +14  4 bytes   SPA   (sender protocol address)
\    +18  6 bytes   THA   (target hardware address)
\    +24  4 bytes   TPA   (target protocol address)

\ -- ARP constants --
16 CONSTANT /ARP-ENTRY
8  CONSTANT ARP-MAX-ENTRIES
28 CONSTANT /ARP-PKT

\ -- ARP table (8 entries × 16 bytes = 128 bytes) --
CREATE ARP-TABLE  ARP-MAX-ENTRIES /ARP-ENTRY * ALLOT
ARP-TABLE ARP-MAX-ENTRIES /ARP-ENTRY * 0 FILL

\ -- Our IPv4 address (4 bytes, network byte order) --
CREATE MY-IP  4 ALLOT
MY-IP 4 0 FILL                 \ 0.0.0.0 until configured

\ -- Gateway and subnet --
CREATE GW-IP  4 ALLOT
GW-IP 4 0 FILL                 \ 0.0.0.0 until configured
CREATE NET-MASK  4 ALLOT
255 NET-MASK C!  255 NET-MASK 1+ C!  255 NET-MASK 2 + C!  0 NET-MASK 3 + C!
\ default 255.255.255.0

\ -- ARP entry accessors --
: ARP-ENTRY  ( idx -- addr )    /ARP-ENTRY * ARP-TABLE + ;
: ARP-E.IP   ( entry -- addr )  ;          \ +0
: ARP-E.MAC  ( entry -- addr )  4 + ;      \ +4
: ARP-E.FLAG ( entry -- addr )  10 + ;     \ +10
: ARP-E.VALID? ( entry -- flag ) ARP-E.FLAG W@ 1 AND 0<> ;

\ -- IPv4 address comparison (4 bytes) --
: IP=  ( addr1 addr2 -- flag )   4 SAMESTR? ;

\ -- IP address store/fetch helpers --
: IP!  ( a b c d addr -- )   \ store dotted quad a.b.c.d
    >R
    R@ 3 + C!    \ d at addr+3
    R@ 2 + C!    \ c at addr+2
    R@ 1+ C!     \ b at addr+1
    R> C! ;      \ a at addr+0

: IP@  ( addr -- a b c d )   \ fetch dotted quad a.b.c.d
    DUP C@ SWAP
    DUP 1+ C@ SWAP
    DUP 2 + C@ SWAP
    3 + C@ ;

\ -- Set our IP address (dotted quad) --
: IP-SET  ( b0 b1 b2 b3 -- )   MY-IP IP! ;

\ -- Print IPv4 address --
: .IP  ( addr -- )
    DUP C@ .  46 EMIT
    DUP 1+ C@ .  46 EMIT
    DUP 2 + C@ .  46 EMIT
    3 + C@ . ;

\ -- ARP-LOOKUP: find MAC for a given IPv4 address --
\   ( ip-addr -- mac-addr | 0 )
: ARP-LOOKUP  ( ip -- mac|0 )
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            DUP ARP-E.IP 2 PICK IP= IF
                ARP-E.MAC NIP UNLOOP EXIT
            THEN
        THEN
        DROP
    LOOP
    DROP 0 ;

\ -- ARP-INSERT: add or update an entry in the ARP table --
\   ( ip-addr mac-addr -- )
\   If ip already exists, update MAC.  Otherwise use first free slot.
VARIABLE _ARP-SLOT
: ARP-INSERT  ( ip mac -- )
    -1 _ARP-SLOT !
    \ First pass: look for existing entry with same IP
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            ARP-E.IP 2 PICK IP= IF
                I _ARP-SLOT !  LEAVE
            THEN
        ELSE
            DROP
            _ARP-SLOT @ -1 = IF I _ARP-SLOT ! THEN  \ remember first free
        THEN
    LOOP
    _ARP-SLOT @ -1 = IF 2DROP EXIT THEN  \ table full
    _ARP-SLOT @ ARP-ENTRY >R
    OVER R@ ARP-E.IP 4 CMOVE            \ copy IP
    R@ ARP-E.MAC 6 CMOVE                \ copy MAC
    DROP                                 \ drop ip-addr
    1 R> ARP-E.FLAG W! ;                 \ mark valid

\ -- ARP-CLEAR: clear the ARP table --
: ARP-CLEAR  ( -- )
    ARP-TABLE ARP-MAX-ENTRIES /ARP-ENTRY * 0 FILL ;

\ -- .ARP: print the ARP table --
: .ARP  ( -- )
    ." --- ARP table ---" CR
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            ."   " DUP ARP-E.IP .IP ."  -> " ARP-E.MAC .MAC CR
        ELSE
            DROP
        THEN
    LOOP ;

\ -- 10b: ARP request / reply build + parse + resolve --
\ ARP payload is 28 bytes (Ethernet/IPv4):
\   +0  HTYPE  2B  0x0001 (Ethernet)
\   +2  PTYPE  2B  0x0800 (IPv4)
\   +4  HLEN   1B  6
\   +5  PLEN   1B  4
\   +6  OPER   2B  1=request  2=reply
\   +8  SHA    6B  sender MAC
\  +14  SPA    4B  sender IP
\  +18  THA    6B  target MAC
\  +24  TPA    4B  target IP

1 CONSTANT ARP-OP-REQUEST
2 CONSTANT ARP-OP-REPLY

CREATE ARP-PKT-BUF  /ARP-PKT ALLOT   \ 28-byte scratch for building ARP

\ -- field offsets within ARP payload --
: ARP-F.HTYPE  ( buf -- addr )        ;             \ +0
: ARP-F.PTYPE  ( buf -- addr )  2 + ;               \ +2
: ARP-F.HLEN   ( buf -- addr )  4 + ;               \ +4
: ARP-F.PLEN   ( buf -- addr )  5 + ;               \ +5
: ARP-F.OPER   ( buf -- addr )  6 + ;               \ +6
: ARP-F.SHA    ( buf -- addr )  8 + ;               \ +8
: ARP-F.SPA    ( buf -- addr )  14 + ;              \ +14
: ARP-F.THA    ( buf -- addr )  18 + ;              \ +18
: ARP-F.TPA    ( buf -- addr )  24 + ;              \ +24

\ -- ARP-FILL-HDR: set the invariant Ethernet/IPv4 header fields --
: ARP-FILL-HDR  ( buf -- )
    DUP /ARP-PKT 0 FILL        \ zero entire buffer
    DUP ARP-F.HTYPE  0 SWAP C!  DUP ARP-F.HTYPE 1+ 1 SWAP C!   \ HTYPE=1 (BE)
    DUP ARP-F.PTYPE  8 SWAP C!  DUP ARP-F.PTYPE 1+ 0 SWAP C!   \ PTYPE=0x0800 (BE)
    DUP ARP-F.HLEN   6 SWAP C!                                   \ HLEN=6
    ARP-F.PLEN   4 SWAP C! ;                                     \ PLEN=4

\ -- ARP-SET-OPER: write operation code (big-endian 16-bit) --
: ARP-SET-OPER  ( op buf -- )
    ARP-F.OPER  SWAP DUP 8 RSHIFT  ( addr lo hi )
    2 PICK C!  SWAP 1+ C! ;

\ -- ARP-BUILD-REQUEST: build ARP request for target IP --
\ ( target-ip -- buf len )   buf = ARP-PKT-BUF, len = /ARP-PKT
: ARP-BUILD-REQUEST
    ARP-PKT-BUF ARP-FILL-HDR
    ARP-OP-REQUEST ARP-PKT-BUF ARP-SET-OPER
    \ SHA = MY-MAC
    MY-MAC ARP-PKT-BUF ARP-F.SHA 6 CMOVE
    \ SPA = MY-IP
    MY-IP ARP-PKT-BUF ARP-F.SPA 4 CMOVE
    \ THA = 00:00:00:00:00:00  (already zeroed)
    \ TPA = target-ip
    ARP-PKT-BUF ARP-F.TPA 4 CMOVE
    ARP-PKT-BUF /ARP-PKT ;

\ -- ARP-BUILD-REPLY: build ARP reply to a received ARP request --
\ ( rx-arp-buf -- buf len )  builds reply in ARP-PKT-BUF
: ARP-BUILD-REPLY  ( rx-arp -- buf len )
    ARP-PKT-BUF ARP-FILL-HDR
    ARP-OP-REPLY ARP-PKT-BUF ARP-SET-OPER
    \ SHA = MY-MAC
    MY-MAC ARP-PKT-BUF ARP-F.SHA 6 CMOVE
    \ SPA = MY-IP
    MY-IP ARP-PKT-BUF ARP-F.SPA 4 CMOVE
    \ THA = requester's SHA
    DUP ARP-F.SHA ARP-PKT-BUF ARP-F.THA 6 CMOVE
    \ TPA = requester's SPA
    ARP-F.SPA ARP-PKT-BUF ARP-F.TPA 4 CMOVE
    ARP-PKT-BUF /ARP-PKT ;

\ -- ARP-SEND-REQUEST: broadcast ARP request for target IP --
\ ( target-ip -- )
: ARP-SEND-REQUEST
    ARP-BUILD-REQUEST               \ ( buf len )
    >R >R                           \ save buf,len on R
    MAC-BCAST ETYPE-ARP R> R>       \ ( dst etype buf len )
    ETH-SEND-TX ;

\ -- ARP-PARSE-REPLY: extract sender MAC+IP from ARP reply into table --
\ ( arp-buf -- )  feeds ARP-INSERT with sender info
: ARP-PARSE-REPLY  ( arp-buf -- )
    DUP ARP-F.SPA SWAP ARP-F.SHA   \ ( spa sha )
    ARP-INSERT ;

\ -- ARP-IS-REQUEST?: check if ARP payload is a request (OPER=1) --
: ARP-IS-REQUEST?  ( arp-buf -- flag )
    ARP-F.OPER DUP C@ 8 LSHIFT SWAP 1+ C@ +  \ big-endian 16-bit
    ARP-OP-REQUEST = ;

\ -- ARP-IS-REPLY?: check if ARP payload is a reply (OPER=2) --
: ARP-IS-REPLY?  ( arp-buf -- flag )
    ARP-F.OPER DUP C@ 8 LSHIFT SWAP 1+ C@ +
    ARP-OP-REPLY = ;

\ -- ARP-FOR-US?: check if ARP TPA matches MY-IP --
: ARP-FOR-US?  ( arp-buf -- flag )
    ARP-F.TPA MY-IP IP= ;

VARIABLE _ARP-RES-IP    \ saved target IP for ARP-RESOLVE
\ -- ARP-RESOLVE: resolve IP to MAC, sending request if needed --
\ ( ip-addr -- mac-addr | 0 )
\ First checks table; if miss, sends request & waits for reply.
: ARP-RESOLVE  ( ip -- mac|0 )
    DUP ARP-LOOKUP DUP 0<> IF
        NIP EXIT   \ found in table
    THEN
    DROP                      \ drop the 0
    DUP _ARP-RES-IP !         \ save IP
    ARP-SEND-REQUEST           \ broadcast request (consumes ip)
    \ Wait for ARP reply (up to 10 receive attempts)
    10 0 DO
        3 ETH-RECV-WAIT           \ ( len ) — ETH-RECV uses ETH-RX-BUF internally
        DUP 0 > IF
            ETH-RX-BUF ETH-IS-ARP? IF
                ETH-RX-BUF ETH-PLD ARP-IS-REPLY? IF
                    ETH-RX-BUF ETH-PLD ARP-PARSE-REPLY
                    _ARP-RES-IP @ ARP-LOOKUP
                    DUP 0<> IF
                        SWAP DROP   \ drop recv len under mac
                        UNLOOP EXIT
                    THEN
                    DROP   \ drop 0 from failed lookup
                THEN
            THEN
        THEN
        DROP   \ drop recv len
    LOOP
    0 ;        \ timeout — no reply

\ -- 10c: ARP auto-responder --
\ Handles an incoming Ethernet frame if it is an ARP request for us.
\ Sends an ARP reply and records the requester in the ARP table.
\ Returns: -1 if handled, 0 if not an ARP request for us.

: ARP-HANDLE  ( -- flag )
    \ Assumes a frame is already in ETH-RX-BUF
    ETH-RX-BUF ETH-IS-ARP? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD ARP-IS-REQUEST? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD ARP-FOR-US? 0= IF 0 EXIT THEN
    \ It's an ARP request for us — learn the sender
    ETH-RX-BUF ETH-PLD ARP-PARSE-REPLY   \ record sender MAC+IP
    \ Build and send reply
    ETH-RX-BUF ETH-PLD ARP-BUILD-REPLY   \ ( buf len )
    >R >R
    ETH-RX-BUF ETH-SRC ETYPE-ARP R> R>   \ ( dst etype buf len )
    ETH-SEND-TX
    -1 ;    \ handled

\ -- ARP-POLL: receive one frame; auto-reply if ARP request for us --
\   Returns: received frame length (0 if nothing), with flag on top
\   ( -- len handled? )
: ARP-POLL  ( -- len handled? )
    ETH-RECV DUP 0= IF 0 EXIT THEN   \ no frame → 0 0
    ARP-HANDLE ;

\ =====================================================================
\  §14  Startup
\ =====================================================================

CR HRULE
."  KDOS v1.1 — Kernel Dashboard OS" CR
HRULE
." Type HELP for commands, HELP <word> for details." CR
." Type SCREENS for interactive TUI." CR
." Type TOPICS or LESSONS for documentation." CR
NCORES 1 > IF
    ."  Multicore: " NCORES . ." cores available" CR
    ."  Use CORE-RUN, BARRIER, P.RUN-PAR for parallel work." CR
THEN
MAC-INIT
DISK? IF FS-LOAD THEN
CR
