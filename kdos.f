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
\   10. Data Ports      — NIC-based external data ingestion
\   11. Benchmarking    — BENCH for timing via CYCLES
\   12. Dashboard       — text-mode system overview
\   13. Help            — online reference for all KDOS words
\   14. Startup
\   15. Bundles         — versioned, declarative pipeline bundle format
\   16. Network Stack   — Ethernet, ARP, IP, TCP, TLS
\   17. Socket API      — SOCKET, BIND, LISTEN, CONNECT, SEND, RECV
\   18. Ring Buffers    — RING, RING-PUSH, RING-POP, RING-PEEK
\   19. Hash Tables     — HASHTABLE, HT-PUT, HT-GET, HT-DEL, HT-EACH

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

16 CONSTANT /ALLOC-HDR

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
    ."  Heap: base=" HEAP-BASE @ .
    ."   free=" HEAP-FREE-BYTES . ."  bytes" CR ;

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
VARIABLE _AEAD-AAD
VARIABLE _AEAD-AADLEN

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
    \ Feed data blocks
    DUP 4 RSHIFT         \ src dst dlen nblocks
    >R DROP               \ src dst   R: nblocks
    R> 0 DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
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
    \ Feed data blocks
    DUP 4 RSHIFT
    >R DROP
    R> 0 DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
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

: SHA3
    >R
    SHA3-INIT
    SHA3-UPDATE
    R> SHA3-FINAL ;

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
\  BIOS primitives (hardware accelerator at MMIO 0x0840):
\    X25519-SCALAR! ( addr -- )    Write 32-byte scalar
\    X25519-POINT!  ( addr -- )    Write 32-byte u-coordinate
\    X25519-GO      ( -- )         Start computation
\    X25519-WAIT    ( -- )         Poll until done
\    X25519-STATUS@ ( -- n )       Read status (bit0=busy, bit1=done)
\    X25519-RESULT@ ( addr -- )    Read 32-byte result

CREATE X25519-PRIV  32 ALLOT      \ private key scratch buffer
CREATE X25519-PUB   32 ALLOT      \ public key scratch buffer
CREATE X25519-SHARED 32 ALLOT     \ shared secret scratch buffer

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
    X25519-WAIT
    R> X25519-RESULT@ ;

\ X25519-KEYGEN ( -- )
\   Generate a random private key and compute the public key.
\   Private key stored in X25519-PRIV, public key in X25519-PUB.
: X25519-KEYGEN ( -- )
    \ Fill private key with 32 random bytes
    32 0 DO RANDOM8 X25519-PRIV I + C! LOOP
    \ public = clamp(priv) * basepoint(9)
    X25519-PRIV X25519-BASE X25519-PUB X25519 ;

\ X25519-DH ( their-pub-addr -- )
\   Compute shared secret = our_priv * their_pub.
\   Result stored in X25519-SHARED.
: X25519-DH ( addr -- )
    X25519-PRIV SWAP X25519-SHARED X25519 ;

\ .X25519-STATUS ( -- )  Print human-readable X25519 status.
: .X25519-STATUS
    X25519-STATUS@
    DUP 0 = IF DROP ."  X25519: idle" CR ELSE
    DUP 2 = IF DROP ."  X25519: done" CR ELSE
    DUP 1 = IF DROP ."  X25519: busy" CR ELSE
    DROP ."  X25519: unknown" CR
    THEN THEN THEN ;

\ =====================================================================
\  §1.10  Field ALU — Multi-Prime Coprocessor + Raw 256x256 Multiply
\ =====================================================================
\  Hardware-accelerated field arithmetic.  Supports multiple primes via
\  PRIME-SECP / PRIME-25519 words (CMD bits [7:6] select the modulus).
\  Default prime is Curve25519 (2^255-19).  X25519 (mode 0) always uses
\  Curve25519 regardless of prime_sel.
\  Shares the same MMIO base as X25519 (0x0840).
\
\  BIOS primitives used:
\    X25519-SCALAR! ( addr -- )   = FIELD-A! (write operand A, 32 bytes)
\    X25519-POINT!  ( addr -- )   = FIELD-B! (write operand B, 32 bytes)
\    FIELD-CMD!     ( mode -- )   Start computation with given mode
\    X25519-WAIT    ( -- )        = FIELD-WAIT
\    X25519-STATUS@ ( -- n )      = FIELD-STATUS@
\    X25519-RESULT@ ( addr -- )   = FIELD-RESULT@ (read result_lo)
\    FIELD-RESULT-HI@ ( addr -- ) Read result_hi (MUL_RAW only)
\
\  Modes:
\    0 = X25519 (legacy scalar multiply)
\    1 = FADD  (a+b) mod p
\    2 = FSUB  (a-b) mod p
\    3 = FMUL  (a*b) mod p
\    4 = FSQR  (a^2) mod p
\    5 = FINV  a^(p-2) mod p
\    6 = FPOW  a^b mod p
\    7 = MUL_RAW  256x256 -> 512-bit product (no reduction)

\ Mode constants
0 CONSTANT FMODE-X25519
1 CONSTANT FMODE-ADD
2 CONSTANT FMODE-SUB
3 CONSTANT FMODE-MUL
4 CONSTANT FMODE-SQR
5 CONSTANT FMODE-INV
6 CONSTANT FMODE-POW
7 CONSTANT FMODE-RAW
8 CONSTANT FMODE-CMOV
9 CONSTANT FMODE-CEQ
10 CONSTANT FMODE-LOAD-PRIME
11 CONSTANT FMODE-MAC
12 CONSTANT FMODE-MAC-RAW

\ Prime selection — write CMD byte with go=0, prime_sel in bits [7:6].
\ CMD address = MMIO_BASE + 0x0880
0xFFFFFF0000000880 CONSTANT _FIELD-CMD-ADDR

\ Aliases for readability (must precede LOAD-PRIME which uses them)
: FIELD-A!      X25519-SCALAR! ;
: FIELD-B!      X25519-POINT! ;
: FIELD-WAIT    X25519-WAIT ;
: FIELD-STATUS@ X25519-STATUS@ ;
: FIELD-RESULT@ X25519-RESULT@ ;

: PRIME-25519  ( -- )   0 _FIELD-CMD-ADDR C! ;    \ prime_sel=0
: PRIME-SECP   ( -- )  64 _FIELD-CMD-ADDR C! ;    \ prime_sel=1 (1<<6)
: PRIME-P256   ( -- ) 128 _FIELD-CMD-ADDR C! ;    \ prime_sel=2 (2<<6)
: PRIME-CUSTOM ( -- ) 192 _FIELD-CMD-ADDR C! ;    \ prime_sel=3 (3<<6)

\ LOAD-PRIME ( p-addr pinv-addr -- )  Latch custom prime + p_inv.
: LOAD-PRIME ( p pinv -- )
    SWAP FIELD-A! FIELD-B!
    FMODE-LOAD-PRIME FIELD-CMD!
    FIELD-WAIT ;

\ Scratch buffers for field ops (32 bytes each)
CREATE _FA  32 ALLOT
CREATE _FB  32 ALLOT
CREATE _FR  32 ALLOT
CREATE _FRH 32 ALLOT

\ ------------------------------------------------------------------
\ Core field operations — all take/return 32-byte buffer addresses
\ ------------------------------------------------------------------

\ FADD ( a-addr b-addr result-addr -- )  (a+b) mod p
: FADD ( a b r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-ADD FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FSUB ( a-addr b-addr result-addr -- )  (a-b) mod p
: FSUB ( a b r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-SUB FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FMUL ( a-addr b-addr result-addr -- )  (a*b) mod p
: FMUL ( a b r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-MUL FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FSQR ( a-addr result-addr -- )  a^2 mod p
: FSQR ( a r -- )
    >R
    FIELD-A!
    FMODE-SQR FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FINV ( a-addr result-addr -- )  a^(p-2) mod p  (Fermat inversion)
: FINV ( a r -- )
    >R
    FIELD-A!
    FMODE-INV FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FPOW ( base-addr exp-addr result-addr -- )  base^exp mod p
: FPOW ( a e r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-POW FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FMUL-RAW ( a-addr b-addr rlo-addr rhi-addr -- )
\   Raw 256x256 -> 512-bit product.  No modular reduction.
\   Result split: low 256 bits in rlo, high 256 bits in rhi.
: FMUL-RAW ( a b rlo rhi -- )
    >R >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-RAW FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@
    R> FIELD-RESULT-HI@ ;

\ FCMOV ( a-addr cond-addr -- )  Constant-time conditional move.
\   If cond[0]=1, result_lo <- a; else result_lo unchanged.
\   Result stays in device register (no copy-out).
: FCMOV ( a cond -- )
    SWAP
    FIELD-A!
    FIELD-B!
    FMODE-CMOV FIELD-CMD!
    FIELD-WAIT ;

\ FCEQ ( a-addr b-addr result-addr -- )  Constant-time equality test.
\   result = (a == b) ? 1 : 0.
: FCEQ ( a b r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-CEQ FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FMAC ( a-addr b-addr result-addr -- )
\   Field multiply-accumulate: result += a*b mod p.
\   Accumulates into device's result_lo register, then copies out.
: FMAC ( a b r -- )
    >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-MAC FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@ ;

\ FMUL-ADD-RAW ( a-addr b-addr rlo-addr rhi-addr -- )
\   Raw 512-bit multiply-accumulate (no field reduction).
\   Accumulates into device's {result_hi, result_lo}, then copies out.
: FMUL-ADD-RAW ( a b rlo rhi -- )
    >R >R SWAP
    FIELD-A!
    FIELD-B!
    FMODE-MAC-RAW FIELD-CMD!
    FIELD-WAIT
    R> FIELD-RESULT@
    R> FIELD-RESULT-HI@ ;

\ .FIELD-STATUS ( -- )  Print human-readable field ALU status.
: .FIELD-STATUS
    FIELD-STATUS@
    DUP 0 = IF DROP ."  Field ALU: idle" CR ELSE
    DUP 2 = IF DROP ."  Field ALU: done" CR ELSE
    DUP 1 = IF DROP ."  Field ALU: busy" CR ELSE
    DROP ."  Field ALU: unknown" CR
    THEN THEN THEN ;

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

\ XMEM-ALLOT ( u -- addr )  bump-allocate u bytes from ext mem
: XMEM-ALLOT  ( u -- addr )
    XMEM? 0= ABORT" No external memory"
    XMEM-HERE @ SWAP
    OVER + DUP XMEM-LIMIT @ > ABORT" Ext mem overflow"
    XMEM-HERE ! ;

\ XMEM-TALIGN ( -- )  align XMEM-HERE up to 64-byte boundary
: XMEM-TALIGN  ( -- )
    XMEM-HERE @  63 + -64 AND  XMEM-HERE ! ;

\ XMEM-RESET ( -- )  reclaim all external memory (bulk free)
: XMEM-RESET  ( -- )
    XMEM? IF EXT-MEM-BASE XMEM-HERE ! THEN ;

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
    BUF-COUNT @ 16 < IF
        BDESC @  BUF-COUNT @ CELLS BUF-TABLE + !
        BUF-COUNT @ 1+ BUF-COUNT !
    THEN
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
    BUF-COUNT @ DUP IF
        0 DO
            I . ."  : "
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
        ."  FWRITE: out of space" CR EXIT
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
        ."  No disk attached" CR EXIT
    THEN
    \ Read superblock (sector 0)
    0 DISK-SEC!  FS-SUPER DISK-DMA!  1 DISK-N!  DISK-READ
    \ Check magic "MP64" (M=77 P=80 6=54 4=52)
    FS-SUPER     C@ 77 <>
    FS-SUPER 1+  C@ 80 <> OR
    FS-SUPER 2 + C@ 54 <> OR
    FS-SUPER 3 + C@ 52 <> OR
    IF
        ."  Not an MP64FS disk" CR EXIT
    THEN
    \ Read bitmap (sector 1)
    1 DISK-SEC!  FS-BMAP DISK-DMA!  1 DISK-N!  DISK-READ
    \ Read directory (sectors 2-5)
    2 DISK-SEC!  FS-DIR DISK-DMA!  4 DISK-N!  DISK-READ
    -1 FS-OK !
    ."  MP64FS loaded" CR ;

\ FS-SYNC ( -- ) write bitmap + directory back to disk
: FS-SYNC  ( -- )
    FS-OK @ 0= IF ."  FS not loaded" CR EXIT THEN
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
    DISK? 0= IF ."  No disk" CR EXIT THEN
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
    ."  MP64FS formatted" CR ;

\ ── .FTYPE — print file type name ───────────────────────────────────

: .FTYPE  ( type -- )
    DUP 0 = IF DROP ."  free"  EXIT THEN
    DUP 1 = IF DROP ."  raw"   EXIT THEN
    DUP 2 = IF DROP ."  text"  EXIT THEN
    DUP 3 = IF DROP ."  forth" EXIT THEN
    DUP 4 = IF DROP ."  doc"   EXIT THEN
    DUP 5 = IF DROP ."  data"  EXIT THEN
    DUP 6 = IF DROP ."  tut"   EXIT THEN
    DUP 7 = IF DROP ."  bdl"   EXIT THEN
    ."  ?" . ;

\ ── DIR — list files ─────────────────────────────────────────────────

: DIR  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  --- Directory ---" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            1+
            ."   " I DIRENT .ZSTR
            ."    " I DIRENT DE.USED . ."  B"
            ."    " I DIRENT DE.TYPE .FTYPE
            CR
        THEN
    LOOP
    DUP . ."  file(s), "
    0  2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes free)" CR
    DROP ;

\ CATALOG ( -- ) detailed directory listing
: CATALOG  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    ."  Name             Bytes     Secs  Type" CR
    0
    FS-MAX-FILES 0 DO
        I DIRENT C@ 0<> IF
            1+
            ."   " I DIRENT .ZSTR
            ."   " I DIRENT DE.USED .
            ."   " I DIRENT DE.COUNT .
            ."   " I DIRENT DE.TYPE .
            CR
        THEN
    LOOP
    ."  (" . ."  files, "
    0 2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    . ."  free sectors)" CR ;

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
    MK-START @ -1 = IF ."  No space on disk" CR EXIT THEN
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
    \ Free bitmap sectors
    RM-SLOT @ DIRENT DE.COUNT
    RM-SLOT @ DIRENT DE.SEC
    SWAP 0 DO                        ( start )
        DUP I + BIT-CLR
    LOOP DROP
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
    RN-SLOT @ DIRENT 16 0 FILL       \ zero old name
    NAMEBUF RN-SLOT @ DIRENT 16 CMOVE
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
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    0   \ free sector count
    2048 FS-DATA-START DO
        I BIT-FREE? IF 1+ THEN
    LOOP
    DUP . ."  free sectors ("
    SECTOR * . ."  bytes)" CR
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
    SB-SLOT @ DIRENT DE.SEC DISK-SEC!
    SB-DESC @ B.DATA DISK-DMA!
    SB-SLOT @ DIRENT DE.COUNT DISK-N!
    DISK-WRITE
    \ Update used_bytes in directory
    SB-DESC @ B.LEN
    SB-SLOT @ DIRENT 20 + L!
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
    LB-SLOT @ DIRENT DE.SEC DISK-SEC!
    LB-DESC @ B.DATA DISK-DMA!
    LB-SLOT @ DIRENT DE.COUNT DISK-N!
    DISK-READ
    ."  Loaded " LB-SLOT @ DIRENT 20 + L@ . ."  bytes from " NAMEBUF .ZSTR CR ;

\ ── OPEN — open a file by name ───────────────────────────────────────

VARIABLE OP-SLOT

: OPEN  ( "name" -- fdesc | 0 )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR 0 EXIT THEN
    PARSE-NAME
    FIND-BY-NAME OP-SLOT !
    OP-SLOT @ -1 = IF
        ."  Not found: " NAMEBUF .ZSTR CR 0 EXIT
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
    FS-OK @ 0= IF DROP ."  FS not loaded" CR EXIT THEN
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
VARIABLE LD-CUR
VARIABLE LD-LEN

\ Nesting support: save/restore walker state for nested LOAD/REQUIRE.
CREATE _LD-STK 128 ALLOT    \ 4 vars * 8 bytes * 4 nesting levels
VARIABLE _LD-SP
0 _LD-SP !

: _LD-SAVE  ( -- )
    LD-BUF @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-SZ  @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-CUR @ _LD-SP @ _LD-STK + !  8 _LD-SP +!
    LD-LEN @ _LD-SP @ _LD-STK + !  8 _LD-SP +! ;

: _LD-RESTORE  ( -- )
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-LEN !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-CUR !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-SZ  !
    -8 _LD-SP +!  _LD-SP @ _LD-STK + @ LD-BUF ! ;

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

: LOAD  ( "filename" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DIRENT                               ( de )
    \ Open by slot
    DUP DE.USED DUP 0= IF
        2DROP ."  Empty file" CR EXIT
    THEN
    \ Save outer walker state before modifying variables (nesting).
    _LD-SAVE
    LD-SZ !                              ( de )
    DUP 16 + W@ SWAP DE.COUNT           ( start count )
    \ Read file data into a buffer.  If external memory is present
    \ use it — file text is only needed during EVALUATE and keeping
    \ it out of Bank 0 relieves dictionary / stack pressure.
    \ When no ext mem, fall back to HERE + ALLOT (original path).
    XMEM? IF
        LD-SZ @ XMEM-ALLOT LD-BUF !     ( start count )
    ELSE
        HERE LD-BUF !
        LD-SZ @ ALLOT                    ( start count )
    THEN
    \ Read sectors directly into buffer
    OVER DISK-SEC!
    LD-BUF @ DISK-DMA!
    DUP DISK-N!
    DISK-READ
    2DROP                                ( -- clean stack )
    _LD-WALK
    _LD-RESTORE ;

\ ── User-Mode Application Loading ───────────────────────────────────
\  APP-EVAL evaluates a string in user mode.  ENTER-USER drops the
\  privilege level to 1 (user); EVALUATE runs the code; SYS-EXIT
\  fires a TRAP syscall that returns to supervisor mode.
\
\  User code can call all standard Forth words (EMIT, KEY, +, IF, :,
\  VARIABLE, etc.) because those use unrestricted instructions.  Only
\  1802-heritage families (MEMALU, IO, SEP, SEX) and protected CSR
\  writes are blocked — those trigger IVEC_PRIV_FAULT.
\
\  MPU is configured to [0, EXT-MEM-END) so user code can access
\  Bank 0 (needed for dictionary lookup by EVALUATE) AND external
\  RAM (where user programs / data live).  HBW access is blocked
\  unconditionally for user mode (RTL hard-wired).  For tighter
\  sandboxing (e.g. running pre-compiled code with its own stack),
\  set MPU-BASE!/MPU-LIMIT! to a narrower window before ENTER-USER.
\
\  LOAD / FSLOAD remain supervisor-mode for OS modules and drivers.

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

: APP-LOAD  ( "filename" -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    PARSE-NAME
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DIRENT
    DUP DE.USED DUP 0= IF
        2DROP ."  Empty file" CR EXIT
    THEN LD-SZ !
    DUP 16 + W@ SWAP DE.COUNT
    \ File buffer in ext mem when available — user data belongs there.
    XMEM? IF
        LD-SZ @ XMEM-ALLOT LD-BUF !
    ELSE
        HERE LD-BUF !
        LD-SZ @ ALLOT
    THEN
    OVER DISK-SEC!
    LD-BUF @ DISK-DMA!
    DUP DISK-N!
    DISK-READ
    2DROP
    \ Configure MPU (Bank 0 + ext mem visible) and enter user mode
    _APP-MPU-ON
    ENTER-USER
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
    2DROP
    SYS-EXIT
    _APP-MPU-OFF ;

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
        ."  FENCRYPT: insufficient space" CR -1 EXIT
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
    PARSE-NAME PN-LEN @ 0= IF ."  Usage: DESCRIBE <word>" CR EXIT THEN
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
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
        ."  No doc for: " NAMEBUF .ZSTR CR
        ."  Use TOPICS to list available documentation."  CR
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

: YIELD?  ( -- )
    PREEMPT-ENABLED @ IF
        COREID PREEMPT-FLAG@ IF
            COREID PREEMPT-CLR
            YIELD
        THEN
    THEN ;

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
\  Named spinlock assignments for shared resources.
\  Uses hardware spinlocks 0-3 for specific subsystems.
\  Spinlock 7 reserved for IPI messaging (MSG-SLOCK).
\
\  DICT-ACQUIRE / DICT-RELEASE   — dictionary (HERE, ALLOT, CREATE)
\  UART-ACQUIRE / UART-RELEASE   — UART output (EMIT, TYPE, .)
\  FS-ACQUIRE   / FS-RELEASE     — filesystem (F-OPEN, F-READ, etc.)
\  HEAP-ACQUIRE / HEAP-RELEASE   — heap allocator (ALLOC, FREE)
\  WITH-LOCK    ( xt lock# -- )  — execute xt while holding lock

0 CONSTANT DICT-LOCK
1 CONSTANT UART-LOCK
2 CONSTANT FS-LOCK
3 CONSTANT HEAP-LOCK

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

\ -- §9.1  Screen & subscreen registry tables --
16 CONSTANT MAX-SCREENS
 8 CONSTANT MAX-SUBS

CREATE SCR-XT      MAX-SCREENS CELLS ALLOT    \ render xt per screen
CREATE SCR-LBL-XT  MAX-SCREENS CELLS ALLOT    \ label-print xt
CREATE SCR-FLAGS   MAX-SCREENS CELLS ALLOT    \ bit 0 = selectable
CREATE SCR-KEY-XT  MAX-SCREENS CELLS ALLOT    \ per-screen key handler (0=none)

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
                        PAGE SHOW-FILE
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

: REGISTER-SCREEN  ( xt-render xt-label flags -- id )
    NSCREENS @ DUP MAX-SCREENS >= ABORT" screen table full"
    >R
    R@ CELLS SCR-FLAGS + !
    R@ CELLS SCR-LBL-XT + !
    R@ CELLS SCR-XT + !
    0 R@ CELLS SCR-KEY-XT + !
    0 R@ CELLS SUB-COUNTS + !
    NSCREENS @ 1+ NSCREENS !
    R> ;

: SET-SCREEN-KEYS  ( xt screen-id -- )
    CELLS SCR-KEY-XT + ! ;

: ADD-SUBSCREEN  ( xt-render xt-label parent-id -- )
    _ASUB-P !
    _ASUB-P @ CELLS SUB-COUNTS + @ _ASUB-I !
    _ASUB-I @ MAX-SUBS >= ABORT" sub table full"
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
        I CELLS SCR-LBL-XT + @ EXECUTE
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
        SCREEN-ID @ 1- MAX-SUBS * I + CELLS SUB-LBL-XT + @ EXECUTE
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
\ 14 entries — each holds an xt dispatched by the corresponding W.xxx.
\ Default = TUI renderer.  Swap for web/HTML by replacing all entries.

14 CONSTANT WVEC-SIZE
CREATE WVEC  WVEC-SIZE CELLS ALLOT
 0 CONSTANT WV-TITLE      1 CONSTANT WV-SECTION
 2 CONSTANT WV-LINE       3 CONSTANT WV-KV
 4 CONSTANT WV-KV-XT      5 CONSTANT WV-FLAG
 6 CONSTANT WV-FLAG-2     7 CONSTANT WV-HBAR
 8 CONSTANT WV-GAP        9 CONSTANT WV-LIST
10 CONSTANT WV-DETAIL    11 CONSTANT WV-HINT
12 CONSTANT WV-CUSTOM    13 CONSTANT WV-NONE

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
    ['] TUI-CUSTOM   WV-CUSTOM  WV! ;
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
    CELLS BUF-TABLE + @
    DUP B.TYPE .BTYPE
    ."  w=" DUP B.WIDTH .N
    ."  n=" DUP B.LEN .N
    ."  tiles=" DUP B.TILES .N
    ."  @" B.DATA .N ;

: .BUF-DETAIL  ( -- )
    SCR-SEL @ CELLS BUF-TABLE + @
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
    S" POLL / n INGEST       Receive frames" W.LINE
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
    0  2048 FS-DATA-START DO
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
        I CELLS BUF-TABLE + @ DUP B.WIDTH .N ." x" B.LEN .N CR
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
        I CELLS BUF-TABLE + @ B.TYPE
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
        CELLS SUB-XT + @ EXECUTE                \ render active sub
    ELSE
        DROP
        CELLS SCR-XT + @ EXECUTE                \ render main screen
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

: TASK-KEYS  ( c -- )
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
    DROP ;

\ -- Activate selected item --
: DO-SELECT  ( -- )
    SCREEN-ID @ 7 = IF SCR-SEL @ SHOW-NTH-DOC THEN ;

\ -- Event loop: poll KEY?, dispatch on keypress (registry-based) --
: HANDLE-KEY  ( c -- )
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
    \ Per-screen custom key handler (from registry)
    SCREEN-ID @ 1- CELLS SCR-KEY-XT + @ DUP 0<> IF
        EXECUTE EXIT
    ELSE DROP THEN
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
\  Transport words (POLL, INGEST, PORT-SEND) are in §10.1 after §16
\  so they can use the proper UDP network stack.
\
\  Python side: data_sources.py provides SineSource, CounterSource, etc.
\  that inject frames wrapped in ETH+IP+UDP via system.nic.inject_frame().

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

\ -- Memory usage bar --
: .MEM  ( -- )
    ."   Bank 0 (System RAM):" CR
    ."     HERE  = " HERE . CR
    ."     Free  = " SP@ HERE - . ."  bytes (to data stack)" CR
    .HBW
    .XMEM ;

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
    ."  dst=" DUP ETH-DST .MAC
    ."   src=" DUP ETH-SRC .MAC
    ."   type=" ETH-TYPE . CR ;

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
    ."  tx=" ETH-TX-COUNT @ .
    ."  rx=" ETH-RX-COUNT @ . CR ;

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

\ -- Print IPv4 address (compact, no trailing space) --
: .IP  ( addr -- )
    DUP C@ .N  46 EMIT
    DUP 1+ C@ .N  46 EMIT
    DUP 2 + C@ .N  46 EMIT
    3 + C@ .N ;

\ -- NEXT-HOP: determine ARP target for a destination IP --
\   If dst is on our subnet (same masked network), ARP-resolve dst
\   directly. Otherwise, ARP-resolve the gateway.
\   If GW-IP is 0.0.0.0 (unconfigured), treat all destinations as on-link.
\   ( dst-ip -- arp-target )
: NEXT-HOP  ( dst -- target )
    \ If gateway is unconfigured (0.0.0.0), treat everything as on-link
    GW-IP C@ GW-IP 1+ C@ OR GW-IP 2 + C@ OR GW-IP 3 + C@ OR
    0= IF EXIT THEN    \ no gateway → return dst as-is
    \ Compute dst AND mask
    DUP C@            NET-MASK C@            AND  >R
    DUP 1+ C@         NET-MASK 1+ C@         AND  >R
    DUP 2 + C@        NET-MASK 2 + C@        AND  >R
    DUP 3 + C@        NET-MASK 3 + C@        AND  >R
    \ Compute my-ip AND mask
    MY-IP C@           NET-MASK C@            AND
    MY-IP 1+ C@        NET-MASK 1+ C@         AND
    MY-IP 2 + C@       NET-MASK 2 + C@        AND
    MY-IP 3 + C@       NET-MASK 3 + C@        AND
    \ Compare: my-masked[3] == dst-masked[3], etc
    R> = SWAP R> = AND SWAP R> = AND SWAP R> = AND
    IF EXIT THEN    \ on-subnet: return dst as-is
    DROP GW-IP ;    \ off-subnet: return gateway

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
    ."  --- ARP table ---" CR
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            ."    " DUP ARP-E.IP .IP ."   -> " ARP-E.MAC .MAC CR
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

\ ---------------------------------------------------------------------
\  §16.2  IPv4 — minimal, no fragmentation
\ ---------------------------------------------------------------------
\ IPv4 header (20 bytes, no options):
\   +0   ver/ihl  1B   0x45 (v4, 5×4=20 byte header)
\   +1   DSCP/ECN 1B   0x00
\   +2   total-len 2B  header+payload (big-endian)
\   +4   ident    2B   packet ID
\   +6   flags/fo 2B   0x4000 = Don't Fragment
\   +8   TTL      1B   default 64
\   +9   protocol 1B   1=ICMP  6=TCP  17=UDP
\  +10   checksum 2B   header checksum (big-endian)
\  +12   src-ip   4B
\  +16   dst-ip   4B

20 CONSTANT /IP-HDR
1  CONSTANT IP-PROTO-ICMP
6  CONSTANT IP-PROTO-TCP
17 CONSTANT IP-PROTO-UDP

CREATE IP-TX-BUF  /IP-HDR 1480 + ALLOT    \ max IP packet buffer (1500)

VARIABLE IP-IDENT    \ rolling packet ID
0 IP-IDENT !

\ -- IPv4 header field accessors (from start of IP header) --
: IP-H.VER     ( hdr -- addr )           ;    \ +0
: IP-H.DSCP    ( hdr -- addr )  1 +  ;        \ +1
: IP-H.TLEN    ( hdr -- addr )  2 +  ;        \ +2  total length (BE)
: IP-H.IDENT   ( hdr -- addr )  4 +  ;        \ +4
: IP-H.FLAGS   ( hdr -- addr )  6 +  ;        \ +6  flags+frag offset (BE)
: IP-H.TTL     ( hdr -- addr )  8 +  ;        \ +8
: IP-H.PROTO   ( hdr -- addr )  9 +  ;        \ +9
: IP-H.CKSUM   ( hdr -- addr )  10 + ;        \ +10 checksum (BE)
: IP-H.SRC     ( hdr -- addr )  12 + ;        \ +12 source IP
: IP-H.DST     ( hdr -- addr )  16 + ;        \ +16 dest IP
: IP-H.DATA    ( hdr -- addr )  /IP-HDR + ;   \ +20 payload

\ -- NW16!: store 16-bit big-endian --
: NW16!  ( val addr -- )
    OVER 8 RSHIFT OVER C!  1+ SWAP 255 AND SWAP C! ;

\ -- NW16@: fetch 16-bit big-endian --
: NW16@  ( addr -- val )
    DUP C@ 8 LSHIFT SWAP 1+ C@ + ;

\ -- IP-CHECKSUM: compute ones-complement checksum over n bytes --
\   ( addr n -- cksum )
VARIABLE _IPCS-SUM
: IP-CHECKSUM  ( addr n -- cksum )
    0 _IPCS-SUM !
    2 / 0 DO                        \ iterate 16-bit words
        DUP NW16@ _IPCS-SUM @ +
        _IPCS-SUM !
        2 +
    LOOP DROP
    \ fold carries
    _IPCS-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;   \ ones complement

\ -- IP-FILL-HDR: fill invariant IPv4 header fields --
\   ( proto payload-len dst-ip buf -- )
VARIABLE _IPF-BUF
VARIABLE _IPF-DST
VARIABLE _IPF-PLEN
: IP-FILL-HDR  ( proto paylen dst-ip buf -- )
    _IPF-BUF !  _IPF-DST !  _IPF-PLEN !
    \ zero header
    _IPF-BUF @ /IP-HDR 0 FILL
    \ ver/ihl = 0x45
    69 _IPF-BUF @ IP-H.VER C!
    \ total length = header + payload
    _IPF-PLEN @ /IP-HDR + _IPF-BUF @ IP-H.TLEN NW16!
    \ ident
    IP-IDENT @ _IPF-BUF @ IP-H.IDENT NW16!
    IP-IDENT @ 1+ 65535 AND IP-IDENT !
    \ flags = Don't Fragment (0x4000)
    16384 _IPF-BUF @ IP-H.FLAGS NW16!
    \ TTL = 64
    64 _IPF-BUF @ IP-H.TTL C!
    \ protocol
    _IPF-BUF @ IP-H.PROTO C!   \ proto still on stack
    \ source = MY-IP
    MY-IP _IPF-BUF @ IP-H.SRC 4 CMOVE
    \ destination
    _IPF-DST @ _IPF-BUF @ IP-H.DST 4 CMOVE
    \ checksum (computed over header with cksum field = 0)
    _IPF-BUF @ /IP-HDR IP-CHECKSUM
    _IPF-BUF @ IP-H.CKSUM NW16! ;

\ -- IP-BUILD: build IPv4 packet in IP-TX-BUF --
\   ( proto dst-ip payload paylen -- buf total-len )
VARIABLE _IPB-PAY
VARIABLE _IPB-PLEN
: IP-BUILD  ( proto dst-ip payload paylen -- buf total-len )
    _IPB-PLEN !  _IPB-PAY !
    \ IP-FILL-HDR ( proto paylen dst-ip buf -- )
    _IPB-PLEN @  SWAP  IP-TX-BUF  IP-FILL-HDR
    \ copy payload after header
    _IPB-PAY @ IP-TX-BUF IP-H.DATA _IPB-PLEN @ CMOVE
    IP-TX-BUF  _IPB-PLEN @ /IP-HDR + ;

\ -- IP-PARSE: extract fields from a received IPv4 header --
\   ( ip-hdr -- proto src-ip dst-ip payload paylen )
: IP-PARSE  ( hdr -- proto src dst data datalen )
    DUP IP-H.PROTO C@            \ proto
    OVER IP-H.SRC                \ src-ip addr
    2 PICK IP-H.DST              \ dst-ip addr
    3 PICK IP-H.DATA             \ payload addr
    4 PICK IP-H.TLEN NW16@
    /IP-HDR -                    \ payload length
    >R >R >R >R >R
    DROP                         \ drop original hdr
    R> R> R> R> R> ;

\ -- IP-VERIFY-CKSUM: check if IP header checksum is valid --
\   ( ip-hdr -- flag )  returns -1 if valid
: IP-VERIFY-CKSUM  ( hdr -- flag )
    /IP-HDR IP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- 11b: IP-SEND — ARP-resolve → Ethernet → NIC TX --

VARIABLE _IPS-PROTO
VARIABLE _IPS-DST
VARIABLE _IPS-PAY
VARIABLE _IPS-PLEN

\ -- IP-SEND: build + send an IPv4 packet over Ethernet --
\   ( proto dst-ip payload paylen -- ior )
\   ior = 0 on success, -1 if ARP resolution failed
\   Uses NEXT-HOP to route via gateway when dst is off-subnet.
: IP-SEND  ( proto dst-ip payload paylen -- ior )
    _IPS-PLEN !  _IPS-PAY !  _IPS-DST !  _IPS-PROTO !
    \ Determine L2 next-hop (gateway if off-subnet)
    _IPS-DST @ NEXT-HOP ARP-RESOLVE DUP 0= IF
        DROP -1 EXIT                   \ ARP failure
    THEN
    \ mac-addr on stack; build the IP packet
    _IPS-PROTO @ _IPS-DST @ _IPS-PAY @ _IPS-PLEN @ IP-BUILD
    \ stack: mac buf total-len
    >R >R                              \ R: total-len buf
    ETYPE-IP4 R> R>                    \ stack: mac etype buf total-len
    ETH-SEND-TX
    0 ;                                \ success

\ -- 11c: IP-RECV — demux incoming Ethernet frames by EtherType --

\ -- IP-RECV: receive an IP packet from the network --
\   Polls ETH-RECV; if EtherType=IPv4 and checksum valid,
\   returns pointer to IP header in ETH-RX-BUF and its length.
\   Also auto-handles ARP requests via ARP-HANDLE.
\   ( -- ip-hdr ip-len | 0 0 )
: IP-RECV  ( -- hdr len | 0 0 )
    ETH-RECV DUP 0= IF 0 EXIT THEN    \ no frame → 0 0
    DROP                               \ drop raw frame len
    \ Is it ARP? Handle transparently
    ETH-RX-BUF ETH-IS-ARP? IF
        ARP-HANDLE DROP
        0 0 EXIT
    THEN
    \ Is it IPv4?
    ETH-RX-BUF ETH-IS-IP4? 0= IF 0 0 EXIT THEN
    \ Verify checksum
    ETH-RX-BUF ETH-PLD IP-VERIFY-CKSUM 0= IF 0 0 EXIT THEN
    \ Return pointer to IP header and its total length
    ETH-RX-BUF ETH-PLD
    DUP IP-H.TLEN NW16@ ;

\ -- IP-RECV-WAIT: blocking IP receive with timeout --
\   ( max-attempts -- hdr len | 0 0 )
: IP-RECV-WAIT  ( n -- hdr len | 0 0 )
    0 DO
        IP-RECV DUP 0<> IF UNLOOP EXIT THEN
        DROP   \ drop the extra 0
    LOOP
    0 0 ;

\ ---------------------------------------------------------------------
\  §16.3  ICMP — echo request / echo reply
\ ---------------------------------------------------------------------
\ ICMP header (8 bytes for echo req/rep):
\   +0  type      1B   8=echo-request  0=echo-reply
\   +1  code      1B   0
\   +2  checksum  2B   (big-endian, over entire ICMP message)
\   +4  ident     2B   (big-endian)
\   +6  seq       2B   (big-endian)
\   +8  data      nB   echo payload

8   CONSTANT /ICMP-HDR
8   CONSTANT ICMP-TYPE-ECHO-REQ
0   CONSTANT ICMP-TYPE-ECHO-REP
CREATE ICMP-BUF  /ICMP-HDR 1480 + ALLOT   \ scratch for building ICMP

\ -- ICMP field accessors --
: ICMP-H.TYPE   ( buf -- addr )          ;       \ +0
: ICMP-H.CODE   ( buf -- addr )  1 +  ;          \ +1
: ICMP-H.CKSUM  ( buf -- addr )  2 +  ;          \ +2
: ICMP-H.IDENT  ( buf -- addr )  4 +  ;          \ +4
: ICMP-H.SEQ    ( buf -- addr )  6 +  ;          \ +6
: ICMP-H.DATA   ( buf -- addr )  /ICMP-HDR + ;   \ +8

\ -- ICMP-IS-ECHO-REQ?: check type=8 --
: ICMP-IS-ECHO-REQ?  ( icmp-buf -- flag )
    ICMP-H.TYPE C@ ICMP-TYPE-ECHO-REQ = ;

\ -- ICMP-IS-ECHO-REP?: check type=0 --
: ICMP-IS-ECHO-REP?  ( icmp-buf -- flag )
    ICMP-H.TYPE C@ ICMP-TYPE-ECHO-REP = ;

VARIABLE ICMP-SEQ    0 ICMP-SEQ !

\ -- ICMP-BUILD-ECHO-REQ: build echo request --
\   ( payload paylen -- buf total-len )
VARIABLE _ICR-PLEN
: ICMP-BUILD-ECHO-REQ  ( payload paylen -- buf total-len )
    DUP _ICR-PLEN !
    \ Clear header area
    ICMP-BUF /ICMP-HDR 0 FILL
    \ Type=8 (echo request)
    ICMP-TYPE-ECHO-REQ ICMP-BUF ICMP-H.TYPE C!
    \ Ident = 0x4D50 ("MP")
    19792 ICMP-BUF ICMP-H.IDENT NW16!
    \ Seq number
    ICMP-SEQ @ ICMP-BUF ICMP-H.SEQ NW16!
    ICMP-SEQ @ 1+ 65535 AND ICMP-SEQ !
    \ Copy payload
    ICMP-BUF ICMP-H.DATA _ICR-PLEN @ CMOVE   \ ( payload → consumed )
    \ Compute checksum over entire ICMP message
    ICMP-BUF _ICR-PLEN @ /ICMP-HDR + IP-CHECKSUM
    ICMP-BUF ICMP-H.CKSUM NW16!
    ICMP-BUF _ICR-PLEN @ /ICMP-HDR + ;

\ -- ICMP-BUILD-ECHO-REP: build echo reply from received echo request --
\   Copies ident, seq, and data from request; sets type=0.
\   ( icmp-req req-total-len -- buf rep-total-len )
: ICMP-BUILD-ECHO-REP  ( req rlen -- buf rlen )
    DUP >R                          \ save rlen on R
    OVER ICMP-BUF ROT CMOVE        \ copy entire request to ICMP-BUF
    DROP                             \ drop req addr
    ICMP-TYPE-ECHO-REP ICMP-BUF ICMP-H.TYPE C!   \ type=0
    \ Recompute checksum
    0 ICMP-BUF ICMP-H.CKSUM NW16!   \ zero checksum field
    ICMP-BUF R@ IP-CHECKSUM          \ compute over full ICMP message
    ICMP-BUF ICMP-H.CKSUM NW16!
    ICMP-BUF R> ;

\ -- 12b: ICMP auto-responder --

\ -- ICMP-HANDLE: handle an incoming ICMP echo request --
\   Assumes IP-RECV has been called and ip-hdr is on the stack.
\   If it's an echo request for us, sends a reply. Returns flag.
\   ( ip-hdr ip-len -- flag )   -1 if handled
VARIABLE _ICH-SRC
: ICMP-HANDLE  ( ip-hdr ip-len -- flag )
    DROP                                      \ drop ip-len
    DUP IP-H.PROTO C@ IP-PROTO-ICMP <> IF     \ not ICMP?
        DROP 0 EXIT
    THEN
    DUP IP-H.SRC _ICH-SRC !                  \ save sender IP addr
    DUP IP-H.TLEN NW16@ /IP-HDR -            \ ICMP message length
    SWAP IP-H.DATA SWAP                      \ ( icmp-data icmp-len )
    OVER ICMP-IS-ECHO-REQ? 0= IF 2DROP 0 EXIT THEN
    \ Build echo reply
    ICMP-BUILD-ECHO-REP                       \ ( buf rlen )
    \ Send via IP to the sender
    >R >R
    IP-PROTO-ICMP _ICH-SRC @ R> R>           \ ( proto dst buf len )
    IP-SEND DROP                              \ ignore ior
    -1 ;

\ -- PING-POLL: receive one IP frame, auto-reply to pings --
\   ( -- flag )  -1 if ping was handled, 0 otherwise
: PING-POLL  ( -- flag )
    IP-RECV DUP 0= IF DROP EXIT THEN    \ no frame → 0
    ICMP-HANDLE ;

\ -- 32a: PING command — user-facing ICMP echo request --
\ Sends N echo requests to the target IP, waits for replies, prints RTT.
\ Uses PERF-CYCLES for timing.  Target can be on- or off-subnet (NEXT-HOP).

CREATE PING-TARGET  4 ALLOT           \ target IP for current ping
VARIABLE PING-RTT                     \ cycle count at send time
VARIABLE PING-SENT                    \ number of echo requests sent
VARIABLE PING-RCVD                    \ number of echo replies received
CREATE PING-PAY  8 ALLOT             \ static payload buffer for echo
PING-PAY 8 65 FILL                   \ fill with 'A'

\ -- PING-SEND1: send one ICMP echo request to PING-TARGET --
\   ( seq -- ior )
: PING-SEND1  ( seq -- ior )
    ICMP-SEQ !                        \ set sequence number
    PING-PAY 8 ICMP-BUILD-ECHO-REQ   \ ( buf total-len )
    >R >R
    IP-PROTO-ICMP PING-TARGET R> R>   \ ( proto dst buf len )
    IP-SEND ;

\ -- PING-WAIT-REPLY: poll for ICMP echo reply, return flag --
\   Polls up to max-attempts, handles ARP/ICMP passively.
\   ( max-attempts -- reply-flag )
: PING-WAIT-REPLY  ( n -- flag )
    0 DO
        IP-RECV DUP 0<> IF            \ got a frame
            OVER IP-H.PROTO C@ IP-PROTO-ICMP = IF
                OVER IP-H.DATA ICMP-IS-ECHO-REP? IF
                    2DROP -1 UNLOOP EXIT    \ got our reply!
                THEN
            THEN
            \ Not our reply — might be ARP, handle transparently
            2DROP
        ELSE
            DROP                       \ drop extra 0
        THEN
    LOOP
    0 ;                                \ timeout

\ -- PING: send N echo requests to an IP address --
\   ( ip-addr count -- )
\   Prints summary: bytes, seq, RTT in cycles.
VARIABLE _PING-T0
VARIABLE _PING-FLAG
: PING  ( ip count -- )
    SWAP PING-TARGET 4 CMOVE          \ save target IP
    0 PING-SENT !  0 PING-RCVD !
    ."  PING " PING-TARGET .IP CR
    ( count ) 0 DO
        I PING-SEND1 0= IF
            1 PING-SENT +!
            PERF-CYCLES _PING-T0 !    \ record start time
            50 PING-WAIT-REPLY        \ ( flag )
            _PING-FLAG !
            PERF-CYCLES _PING-T0 @ -  \ elapsed = end - start
            _PING-FLAG @ IF
                1 PING-RCVD +!
                ."    Reply seq=" I .N
                ."   time=" .N ."  cy" CR
            ELSE
                DROP                   \ discard elapsed
                ."    Request timeout seq=" I .N CR
            THEN
        ELSE
            1 PING-SENT +!
            ."    ARP failure seq=" I .N CR
        THEN
    LOOP
    ."  --- " PING-TARGET .IP ."   ping statistics ---" CR
    PING-SENT @ .N ."  sent, "
    PING-RCVD @ .N ."  received" CR ;

\ -- PING-IP: convenience wrapper with dotted-quad --
\   ( a b c d count -- )
CREATE PING-IP-BUF  4 ALLOT
: PING-IP  ( a b c d n -- )
    >R  PING-IP-BUF IP!
    PING-IP-BUF R> PING ;

\ ---------------------------------------------------------------------
\  §16.4  UDP — connectionless datagrams
\ ---------------------------------------------------------------------
\ UDP header (8 bytes):
\   +0  src-port  2B   big-endian
\   +2  dst-port  2B   big-endian
\   +4  length    2B   header + data (big-endian)
\   +6  checksum  2B   pseudo-header checksum (big-endian)

8 CONSTANT /UDP-HDR

CREATE UDP-TX-BUF  /UDP-HDR 1472 + ALLOT   \ max UDP datagram (1480 max IP payload)

\ -- UDP header field accessors --
: UDP-H.SPORT  ( buf -- addr )           ;   \ +0
: UDP-H.DPORT  ( buf -- addr )  2 +  ;      \ +2
: UDP-H.LEN    ( buf -- addr )  4 +  ;      \ +4
: UDP-H.CKSUM  ( buf -- addr )  6 +  ;      \ +6
: UDP-H.DATA   ( buf -- addr )  /UDP-HDR + ; \ +8

\ -- UDP-CHECKSUM: compute UDP checksum with IPv4 pseudo-header --
\   Pseudo-header: src-ip(4) + dst-ip(4) + 0 + proto(1) + udp-len(2)
\   Then the UDP header + data.
\   ( src-ip dst-ip udp-buf udp-len -- cksum )
VARIABLE _UCK-SUM
: UDP-CHECKSUM  ( src-ip dst-ip udp-buf udp-len -- cksum )
    0 _UCK-SUM !
    \ Accumulate pseudo-header: src-ip (4 bytes → 2 words)
    >R >R                              \ R: udp-buf udp-len→ wait, order: R: udp-len udp-buf
    SWAP                               \ ( dst-ip src-ip )
    DUP NW16@ _UCK-SUM @ + _UCK-SUM !  \ src-ip[0:1]
    2 + NW16@ _UCK-SUM @ + _UCK-SUM !  \ src-ip[2:3]
    DUP NW16@ _UCK-SUM @ + _UCK-SUM !  \ dst-ip[0:1]
    2 + NW16@ _UCK-SUM @ + _UCK-SUM !  \ dst-ip[2:3]
    \ proto=17 as 16-bit: 0x0011
    17 _UCK-SUM @ + _UCK-SUM !
    \ udp-len
    R> R>                              \ ( udp-buf udp-len )
    DUP _UCK-SUM @ + _UCK-SUM !       \ add udp-len to sum
    \ Accumulate UDP header + data (16-bit words)
    DUP 1 AND >R                       \ R: odd-flag
    2 / 0 DO
        DUP NW16@ _UCK-SUM @ + _UCK-SUM !
        2 +
    LOOP
    R> IF DUP C@ 8 LSHIFT _UCK-SUM @ + _UCK-SUM ! THEN
    DROP
    \ Fold carries
    _UCK-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;

\ -- UDP-BUILD: build a UDP datagram in UDP-TX-BUF --
\   ( src-port dst-port payload paylen -- buf udp-total-len )
\   Note: caller must supply src-ip/dst-ip for checksum via UDP-FILL-CKSUM
VARIABLE _UDB-PLEN
VARIABLE _UDB-PAY
: UDP-BUILD  ( sport dport payload paylen -- buf total )
    _UDB-PLEN !  _UDB-PAY !
    \ Zero header
    UDP-TX-BUF /UDP-HDR 0 FILL
    \ dst-port, src-port
    UDP-TX-BUF UDP-H.DPORT NW16!
    UDP-TX-BUF UDP-H.SPORT NW16!
    \ length = header + payload
    _UDB-PLEN @ /UDP-HDR +
    UDP-TX-BUF UDP-H.LEN NW16!
    \ Copy payload
    _UDB-PAY @ UDP-TX-BUF UDP-H.DATA _UDB-PLEN @ CMOVE
    \ Checksum left as 0 (caller fills via UDP-FILL-CKSUM)
    UDP-TX-BUF  _UDB-PLEN @ /UDP-HDR + ;

\ -- UDP-FILL-CKSUM: compute and store the UDP checksum --
\   ( src-ip dst-ip udp-buf udp-len -- )
VARIABLE _UFC-BUF
VARIABLE _UFC-LEN
: UDP-FILL-CKSUM  ( src-ip dst-ip buf len -- )
    _UFC-LEN !  _UFC-BUF !
    \ Zero the checksum field
    0 _UFC-BUF @ UDP-H.CKSUM NW16!
    \ Compute checksum  ( src-ip dst-ip buf len -- cksum )
    _UFC-BUF @ _UFC-LEN @  UDP-CHECKSUM
    \ Per RFC 768: if checksum is 0, transmit 0xFFFF
    DUP 0= IF DROP 65535 THEN
    _UFC-BUF @ UDP-H.CKSUM NW16! ;

\ -- UDP-PARSE: extract fields from a received UDP datagram --
\   ( udp-buf -- sport dport data datalen )
: UDP-PARSE  ( buf -- sport dport data datalen )
    DUP UDP-H.SPORT NW16@
    OVER UDP-H.DPORT NW16@
    2 PICK UDP-H.DATA
    3 PICK UDP-H.LEN NW16@ /UDP-HDR -
    >R >R >R >R
    DROP
    R> R> R> R> ;

\ -- UDP-VERIFY-CKSUM: verify UDP checksum --
\   ( src-ip dst-ip udp-buf udp-len -- flag )   -1 if valid, 0 if bad
: UDP-VERIFY-CKSUM  ( src-ip dst-ip buf len -- flag )
    UDP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- 13b: UDP-SEND / UDP-RECV, port demux table --

\ -- Port demux table: maps listening ports to handler XTs --
\   Each entry: 2-cell (port, xt). Up to 8 listeners.
8 CONSTANT /UDP-PORT-MAX
CREATE UDP-PORT-TABLE  /UDP-PORT-MAX 2 * CELLS ALLOT
: UDP-PORT-CLEAR  ( -- )
    UDP-PORT-TABLE /UDP-PORT-MAX 2 * CELLS 0 FILL ;
UDP-PORT-CLEAR

\ -- UDP-PORT-BIND: register a handler for a port --
\   ( port xt -- flag )  -1 if bound, 0 if table full
: UDP-PORT-BIND  ( port xt -- flag )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 0= IF          \ empty slot
            >R               \ save slot addr
            OVER R@ !        \ store port
            R> CELL+ !       \ store xt
            DROP -1           \ success
            UNLOOP EXIT
        THEN
        DROP
    LOOP
    2DROP 0 ;                 \ table full

\ -- UDP-PORT-UNBIND: remove a port binding --
\   ( port -- )
: UDP-PORT-UNBIND  ( port -- )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 2 PICK = IF
            DUP 0 SWAP !               \ clear port
            CELL+ 0 SWAP !             \ clear xt
            DROP UNLOOP EXIT
        THEN
        DROP
    LOOP
    DROP ;

\ -- UDP-PORT-LOOKUP: find handler for a port --
\   ( port -- xt | 0 )
: UDP-PORT-LOOKUP  ( port -- xt | 0 )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 2 PICK = IF
            CELL+ @ NIP
            UNLOOP EXIT
        THEN
        DROP
    LOOP
    DROP 0 ;

\ -- UDP-SEND: send a UDP datagram over IPv4 --
\   ( dst-ip dst-port src-port payload paylen -- ior )
\   ior = 0 on success, -1 on ARP failure
VARIABLE _UDS-DST
VARIABLE _UDS-DPORT
VARIABLE _UDS-SPORT
: UDP-SEND  ( dst-ip dport sport payload paylen -- ior )
    >R >R                              \ save paylen, payload on R
    _UDS-SPORT !  _UDS-DPORT !  _UDS-DST !
    \ Build UDP datagram
    _UDS-SPORT @ _UDS-DPORT @ R> R>    \ ( sport dport payload paylen )
    UDP-BUILD                           \ ( buf udp-len )
    \ Fill checksum
    2DUP                                \ ( buf len buf len )
    MY-IP _UDS-DST @ ROT ROT           \ ( buf len src dst buf len )
    UDP-FILL-CKSUM                      \ ( buf len )
    \ Send via IP
    >R >R
    IP-PROTO-UDP _UDS-DST @ R> R>      \ ( proto dst buf len )
    IP-SEND ;

\ -- UDP-RECV: receive a UDP datagram from the network --
\   Receives an IP frame, checks for UDP, verifies checksum.
\   Returns source IP address pointer, UDP header pointer, and UDP length.
\   Also auto-handles ARP (via IP-RECV) and ICMP ping.
\   ( -- src-ip udp-buf udp-len | 0 0 0 )
VARIABLE _UDR-HDR
VARIABLE _UDR-IPLEN
: UDP-RECV  ( -- src-ip udp-buf udp-len | 0 0 0 )
    IP-RECV DUP 0= IF DROP 0 0 EXIT THEN    \ no frame → 0 0 0
    _UDR-IPLEN !  _UDR-HDR !
    \ Auto-handle ICMP pings
    _UDR-HDR @ IP-H.PROTO C@ IP-PROTO-ICMP = IF
        _UDR-HDR @ _UDR-IPLEN @ ICMP-HANDLE DROP
        0 0 0 EXIT
    THEN
    \ Check for UDP
    _UDR-HDR @ IP-H.PROTO C@ IP-PROTO-UDP <> IF
        0 0 0 EXIT
    THEN
    \ Verify UDP checksum
    _UDR-HDR @ IP-H.SRC
    _UDR-HDR @ IP-H.DST
    _UDR-HDR @ IP-H.DATA
    _UDR-IPLEN @ /IP-HDR -
    UDP-VERIFY-CKSUM 0= IF
        0 0 0 EXIT
    THEN
    \ Return ( src-ip udp-buf udp-len )
    _UDR-HDR @ IP-H.SRC
    _UDR-HDR @ IP-H.DATA
    _UDR-IPLEN @ /IP-HDR - ;

\ -- UDP-DISPATCH: receive and dispatch to bound port handler --
\   Calls the registered handler xt with ( src-ip sport data dlen -- )
\   ( -- flag )  -1 if dispatched, 0 if not
: UDP-DISPATCH  ( -- flag )
    UDP-RECV DUP 0= IF DROP DROP EXIT THEN    \ no frame → 0
    \ Stack: ( src-ip udp-buf udp-len )
    DROP                                       \ drop udp-len
    DUP UDP-H.DPORT NW16@                     \ get dest port
    UDP-PORT-LOOKUP DUP 0= IF                 \ no handler?
        DROP DROP DROP 0 EXIT
    THEN
    \ Stack: ( src-ip udp-buf xt )
    >R                                         \ save xt
    DUP UDP-H.SPORT NW16@                     \ ( src-ip udp-buf sport )
    OVER UDP-H.DATA                            \ ( src-ip udp-buf sport data )
    2 PICK UDP-H.LEN NW16@ /UDP-HDR -         \ ( src-ip udp-buf sport data dlen )
    >R >R >R
    DROP                                       \ drop udp-buf
    R> R> R>                                   \ ( src-ip sport data dlen )
    R> EXECUTE                                 \ call handler
    -1 ;

\ ---------------------------------------------------------------------
\  §16.5  DHCP — Dynamic Host Configuration Protocol
\ ---------------------------------------------------------------------
\ DHCP uses BOOTP format over UDP 67(server)/68(client).
\ Simplified packet layout (first 240 bytes):
\   +0   op       1B   1=BOOTREQUEST 2=BOOTREPLY
\   +1   htype    1B   1=Ethernet
\   +2   hlen     1B   6
\   +3   hops     1B   0
\   +4   xid      4B   transaction ID
\   +8   secs     2B   0
\  +10   flags    2B   0x8000=broadcast
\  +12   ciaddr   4B   client IP
\  +16   yiaddr   4B   "your" IP (offered)
\  +20   siaddr   4B   server IP
\  +24   giaddr   4B   gateway IP
\  +28   chaddr  16B   client hardware address (MAC + padding)
\  +44   sname   64B   server hostname (unused)
\ +108   file   128B   boot filename (unused)
\ +236   magic    4B   99.130.83.99 = DHCP magic cookie
\ +240   options  var  DHCP options (type, len, data...)

240 CONSTANT /DHCP-HDR
CREATE DHCP-BUF  /DHCP-HDR 312 + ALLOT   \ 548 bytes max (options up to 312)

\ DNS-SERVER-IP: declared here so DHCP can populate it from option 6.
\ Default value (8.8.8.8) is set later in §16.6 DNS.
CREATE DNS-SERVER-IP  4 ALLOT
8 8 8 8 DNS-SERVER-IP IP!

\ DHCP message types (option 53)
1 CONSTANT DHCP-DISCOVER
2 CONSTANT DHCP-OFFER
3 CONSTANT DHCP-REQUEST
5 CONSTANT DHCP-ACK
6 CONSTANT DHCP-NAK

\ DHCP field accessors
: DHCP-F.OP     ( buf -- addr )             ;       \ +0
: DHCP-F.HTYPE  ( buf -- addr )   1 +  ;            \ +1
: DHCP-F.HLEN   ( buf -- addr )   2 +  ;            \ +2
: DHCP-F.XID    ( buf -- addr )   4 +  ;            \ +4
: DHCP-F.FLAGS  ( buf -- addr )  10 + ;              \ +10
: DHCP-F.CIADDR ( buf -- addr )  12 + ;              \ +12
: DHCP-F.YIADDR ( buf -- addr )  16 + ;              \ +16
: DHCP-F.SIADDR ( buf -- addr )  20 + ;              \ +20
: DHCP-F.CHADDR ( buf -- addr )  28 + ;              \ +28
: DHCP-F.MAGIC  ( buf -- addr ) 236 + ;              \ +236
: DHCP-F.OPTS   ( buf -- addr ) 240 + ;              \ +240

VARIABLE DHCP-XID    305419896 DHCP-XID !   \ 0x12345678

: DHCP-NEW-XID  ( -- )
    RANDOM32 DHCP-XID ! ;

\ -- DHCP-VALIDATE-REPLY: strict validation of incoming DHCP reply --
\   Checks: op=BOOTREPLY(2), magic cookie, xid match, chaddr match
\   ( dhcp-buf -- flag )  -1 if valid, 0 if invalid
: DHCP-VALIDATE-REPLY  ( buf -- flag )
    \ Check op = BOOTREPLY (2)
    DUP DHCP-F.OP C@ 2 <> IF DROP 0 EXIT THEN
    \ Check magic cookie = 99.130.83.99
    DUP DHCP-F.MAGIC DUP C@ 99 <> IF 2DROP 0 EXIT THEN
    DUP 1+ C@ 130 <> IF 2DROP 0 EXIT THEN
    DUP 2 + C@ 83 <> IF 2DROP 0 EXIT THEN
    3 + C@ 99 <> IF DROP 0 EXIT THEN
    \ Check XID matches
    DUP DHCP-F.XID NW16@ 16 LSHIFT
    OVER DHCP-F.XID 2 + NW16@ OR
    DHCP-XID @ <> IF DROP 0 EXIT THEN
    \ Check chaddr matches MY-MAC (first 6 bytes)
    DUP DHCP-F.CHADDR MY-MAC 6 TUCK COMPARE 0<> IF DROP 0 EXIT THEN
    DROP -1 ;

\ -- DHCP-FILL-COMMON: fill common BOOTP fields --
\   ( buf -- )
: DHCP-FILL-COMMON  ( buf -- )
    DUP /DHCP-HDR 312 + 0 FILL      \ zero entire buffer
    1 OVER DHCP-F.OP C!              \ op = BOOTREQUEST
    1 OVER DHCP-F.HTYPE C!           \ htype = Ethernet
    6 OVER DHCP-F.HLEN C!            \ hlen = 6
    \ xid (4 bytes, big-endian) using two 16-bit stores
    DHCP-XID @ DUP 16 RSHIFT 2 PICK DHCP-F.XID NW16!
    65535 AND OVER DHCP-F.XID 2 + NW16!
    \ flags = broadcast (0x8000)
    32768 OVER DHCP-F.FLAGS NW16!
    \ chaddr = our MAC (6 bytes)
    MY-MAC OVER DHCP-F.CHADDR 6 CMOVE
    \ magic cookie: 99.130.83.99
    DUP DHCP-F.MAGIC
    99 OVER C!  130 OVER 1+ C!  83 OVER 2 + C!  99 SWAP 3 + C!
    DROP ;

\ -- DHCP-ADD-MSGTYPE: append message type option --
\   ( opt-ptr type -- opt-ptr' )
: DHCP-ADD-MSGTYPE  ( ptr type -- ptr' )
    OVER     53 SWAP C!        \ option 53
    OVER 1+   1 SWAP C!        \ length 1
    OVER 2 +     C!            \ value = type
    3 + ;

\ -- DHCP-ADD-END: append end option --
\   ( opt-ptr -- opt-ptr' )
: DHCP-ADD-END  ( ptr -- ptr' )
    255 OVER C!  1+ ;

\ -- DHCP-ADD-REQIP: append requested IP option (50) --
\   ( opt-ptr ip-addr -- opt-ptr' )
: DHCP-ADD-REQIP  ( ptr ip -- ptr' )
    OVER      50 SWAP C!      \ option 50
    OVER 1+    4 SWAP C!      \ length 4
    OVER 2 + SWAP 4 CMOVE     \ copy 4 IP bytes
    6 + ;

\ -- DHCP-ADD-SERVERID: append server identifier option (54) --
\   ( opt-ptr ip-addr -- opt-ptr' )
: DHCP-ADD-SERVERID  ( ptr ip -- ptr' )
    OVER      54 SWAP C!
    OVER 1+    4 SWAP C!
    OVER 2 + SWAP 4 CMOVE
    6 + ;

\ -- DHCP-ADD-PARAMLIST: append Parameter Request List option (55) --
\   Requests: subnet(1), router(3), DNS(6), lease-time(51)
\   ( opt-ptr -- opt-ptr' )
: DHCP-ADD-PARAMLIST  ( ptr -- ptr' )
    55 OVER C!              \ option 55
     4 OVER 1+ C!           \ length 4
     1 OVER 2 + C!          \ subnet mask
     3 OVER 3 + C!          \ router
     6 OVER 4 + C!          \ DNS server
    51 OVER 5 + C!          \ lease time
    6 + ;

\ -- DHCP-BUILD-DISCOVER: build a DHCP DISCOVER packet --
\   ( -- buf len )
: DHCP-BUILD-DISCOVER  ( -- buf len )
    DHCP-NEW-XID
    DHCP-BUF DHCP-FILL-COMMON
    DHCP-BUF DHCP-F.OPTS
    DHCP-DISCOVER DHCP-ADD-MSGTYPE
    DHCP-ADD-PARAMLIST
    DHCP-ADD-END
    DHCP-BUF - DHCP-BUF SWAP ;

\ -- DHCP-BUILD-REQUEST: build a DHCP REQUEST packet --
\   Requests the offered IP, includes server ID.
\   ( offered-ip server-ip -- buf len )
VARIABLE _DHR-OIP
VARIABLE _DHR-SIP
: DHCP-BUILD-REQUEST  ( offered-ip server-ip -- buf len )
    _DHR-SIP !  _DHR-OIP !
    DHCP-BUF DHCP-FILL-COMMON
    DHCP-BUF DHCP-F.OPTS
    DHCP-REQUEST DHCP-ADD-MSGTYPE
    _DHR-OIP @ DHCP-ADD-REQIP
    _DHR-SIP @ DHCP-ADD-SERVERID
    DHCP-ADD-END
    DHCP-BUF - DHCP-BUF SWAP ;

\ -- DHCP-SEND: send a DHCP packet via broadcast UDP --
\   ( buf len -- )  uses src-ip 0.0.0.0, dst-ip 255.255.255.255
VARIABLE _DHS-BUF
VARIABLE _DHS-LEN
CREATE DHCP-BCAST-IP  255 C, 255 C, 255 C, 255 C,
CREATE DHCP-ZERO-IP      0 C,   0 C,   0 C,   0 C,
: DHCP-SEND  ( buf len -- )
    _DHS-LEN !  _DHS-BUF !
    \ Build UDP datagram: sport=68, dport=67
    68 67 _DHS-BUF @ _DHS-LEN @ UDP-BUILD   \ ( udp-buf udp-len )
    \ Fill UDP checksum with src=0.0.0.0, dst=255.255.255.255
    DHCP-ZERO-IP DHCP-BCAST-IP 2 PICK 2 PICK UDP-FILL-CKSUM
    \ Build IP header around UDP payload
    \ IP-FILL-HDR ( proto paylen dst-ip buf -- )
    IP-PROTO-UDP OVER DHCP-BCAST-IP IP-TX-BUF IP-FILL-HDR
    \ Override src=0.0.0.0 (we don't have an IP yet)
    DHCP-ZERO-IP IP-TX-BUF IP-H.SRC 4 CMOVE
    \ Recompute IP header checksum
    0 IP-TX-BUF IP-H.CKSUM NW16!
    IP-TX-BUF /IP-HDR IP-CHECKSUM IP-TX-BUF IP-H.CKSUM NW16!
    \ Copy UDP data after IP header
    \ stack: ( udp-buf udp-len )
    >R
    IP-TX-BUF IP-H.DATA R@ CMOVE     \ copy udp-buf → IP payload area
    \ Send raw Ethernet broadcast
    MAC-BCAST ETYPE-IP4
    IP-TX-BUF R> /IP-HDR +           \ ( mac etype ip-buf total-len )
    ETH-SEND-TX ;

\ -- DHCP-GET-MSGTYPE: extract message type from DHCP options --
\   ( buf -- type | 0 )
: DHCP-GET-MSGTYPE  ( buf -- type )
    DHCP-F.OPTS
    BEGIN
        DUP C@ 255 <> WHILE           \ not END option
        DUP C@ 53 = IF                \ option 53 = message type
            2 + C@ EXIT
        THEN
        DUP C@ 0= IF 1+ ELSE         \ pad option (type 0, no len)
            DUP 1+ C@ 2 + +           \ skip: type + len + data
        THEN
    REPEAT
    DROP 0 ;

\ -- DHCP-GET-OPTION: extract a specific option from DHCP packet --
\   ( buf option-code -- addr len | 0 0 )
: DHCP-GET-OPTION  ( buf code -- addr len | 0 0 )
    SWAP DHCP-F.OPTS SWAP
    BEGIN
        OVER C@ 255 <> WHILE
        OVER C@ OVER = IF            \ found!
            DROP 2 + DUP 1- C@       \ ( data-addr data-len )
            EXIT
        THEN
        OVER C@ 0= IF SWAP 1+ SWAP ELSE
            SWAP DUP 1+ C@ 2 + + SWAP
        THEN
    REPEAT
    2DROP 0 0 ;

\ -- DHCP-PARSE-OFFER: parse DHCP OFFER, extract offered IP + server IP --
\   ( dhcp-buf -- offered-ip server-ip | 0 0 )
CREATE DHCP-OFFERED-IP  4 ALLOT
CREATE DHCP-SERVER-IP   4 ALLOT
CREATE DHCP-GW-OFFER    4 ALLOT
CREATE DHCP-MASK-OFFER  4 ALLOT
: DHCP-PARSE-OFFER  ( buf -- offered-ip server-ip | 0 0 )
    DUP DHCP-GET-MSGTYPE DHCP-OFFER <> IF DROP 0 0 EXIT THEN
    \ Offered IP = yiaddr
    DUP DHCP-F.YIADDR DHCP-OFFERED-IP 4 CMOVE
    \ Server IP = siaddr (or option 54)
    DUP DHCP-F.SIADDR DHCP-SERVER-IP 4 CMOVE
    \ Try option 54 (server identifier) as override
    DUP 54 DHCP-GET-OPTION DUP 0<> IF
        DROP DHCP-SERVER-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Extract subnet mask (option 1) if present
    DUP 1 DHCP-GET-OPTION DUP 0<> IF
        DROP DHCP-MASK-OFFER 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Extract router/gateway (option 3) if present
    DUP 3 DHCP-GET-OPTION DUP 0<> IF
        DROP DHCP-GW-OFFER 4 CMOVE
    ELSE
        2DROP
    THEN
    DROP
    DHCP-OFFERED-IP  DHCP-SERVER-IP ;

\ -- DHCP-PARSE-ACK: parse DHCP ACK, configure network --
\   ( dhcp-buf -- flag )  -1 if ACK applied, 0 if not ACK
: DHCP-PARSE-ACK  ( buf -- flag )
    DUP DHCP-GET-MSGTYPE DHCP-ACK <> IF DROP 0 EXIT THEN
    \ Set MY-IP from yiaddr
    DUP DHCP-F.YIADDR MY-IP 4 CMOVE
    \ Set subnet mask from option 1
    DUP 1 DHCP-GET-OPTION DUP 0<> IF
        DROP NET-MASK 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Set gateway from option 3
    DUP 3 DHCP-GET-OPTION DUP 0<> IF
        DROP GW-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Set DNS server from option 6 (first 4 bytes = primary DNS)
    DUP 6 DHCP-GET-OPTION DUP 4 >= IF
        DROP DNS-SERVER-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    DROP -1 ;

\ -- DHCP-WAIT-REPLY: wait for a validated DHCP reply on port 68 --
\   ( max-attempts expected-type -- dhcp-buf | 0 )
\   Returns pointer to DHCP data or 0. Validates reply and type.
: DHCP-WAIT-REPLY  ( n expected-type -- dhcp-buf | 0 )
    SWAP 0 DO
        UDP-RECV DUP 0<> IF
            \ Stack: ( expected-type src-ip udp-buf udp-len )
            DROP              \ drop udp-len
            DUP UDP-H.DPORT NW16@ 68 = IF
                UDP-H.DATA    \ pointer to DHCP data
                \ Stack: ( expected-type src-ip dhcp-data )
                DUP DHCP-VALIDATE-REPLY IF
                    \ Check message type matches expected
                    DUP DHCP-GET-MSGTYPE 3 PICK = IF
                        NIP NIP   \ drop src-ip & expected-type
                        UNLOOP EXIT
                    THEN
                THEN
                DROP          \ invalid reply, discard dhcp-data
            ELSE
                DROP          \ drop udp-buf (wrong port)
            THEN
        ELSE
            DROP DROP         \ drop 2 of 3 zeros from failure
        THEN
        DROP                  \ drop src-ip (or last 0)
    LOOP
    DROP                      \ drop expected-type
    0 ;

\ -- DHCP-START: run DHCP client with retry/backoff --
\   Retries DISCOVER up to 4 times with increasing wait attempts.
\   ( -- flag )  -1 if success, 0 if failed
VARIABLE _DHST-OIP
VARIABLE _DHST-SIP
: DHCP-START  ( -- flag )
    4 0 DO                           \ retry loop (up to 4 attempts)
        \ Step 1: Send DISCOVER (generates new XID each time)
        DHCP-BUILD-DISCOVER DHCP-SEND
        \ Step 2: Wait for OFFER (increasing patience: 50, 100, 200, 400)
        50 I 0 ?DO 2 * LOOP
        DHCP-OFFER DHCP-WAIT-REPLY
        DUP 0<> IF
            \ Parse OFFER
            DUP DHCP-PARSE-OFFER
            OVER 0= IF 2DROP DROP ELSE
                _DHST-SIP !  _DHST-OIP !
                DROP                     \ drop dhcp-buf
                \ Step 3: Send REQUEST
                _DHST-OIP @ _DHST-SIP @ DHCP-BUILD-REQUEST DHCP-SEND
                \ Step 4: Wait for ACK
                100 DHCP-ACK DHCP-WAIT-REPLY
                DUP 0<> IF
                    DHCP-PARSE-ACK
                    UNLOOP EXIT
                THEN
                DROP
            THEN
        ELSE
            DROP                         \ drop 0
        THEN
    LOOP
    0 ;

\ ---------------------------------------------------------------------
\  §16.6  DNS — Domain Name System (A-record client)
\ ---------------------------------------------------------------------
\ DNS query/response format:
\   +0   ID        2B   transaction ID
\   +2   flags     2B   0x0100 = standard query (RD=1)
\   +4   QDCOUNT   2B   number of questions (1)
\   +6   ANCOUNT   2B   number of answers
\   +8   NSCOUNT   2B   0
\  +10   ARCOUNT   2B   0
\  +12   question  var  encoded domain name + type + class
\        answer    var  name + type + class + TTL + rdlen + rdata

12 CONSTANT /DNS-HDR
CREATE DNS-BUF  512 ALLOT  \ max DNS message
\ DNS-SERVER-IP is declared in §16.5 (DHCP) so DHCP-PARSE-ACK can set it.

VARIABLE DNS-ID   RANDOM16 DNS-ID !

\ -- DNS-ENCODE-NAME: encode domain name in DNS wire format --
\   Converts "example.com" → [7]example[3]com[0]
\   ( c-addr len buf -- buf end-ptr )
\   c-addr/len is the domain string; buf is where to write.
VARIABLE _DNE-BUF
VARIABLE _DNE-LEN
VARIABLE _DNE-SRC
VARIABLE _DNE-LPTR
: DNS-ENCODE-NAME  ( src slen buf -- buf end )
    _DNE-BUF !              \ save output buffer
    _DNE-LEN !  _DNE-SRC !  \ save source string
    _DNE-BUF @              \ output pointer
    DUP _DNE-LPTR !         \ save label length position
    1+                      \ advance past length byte
    0                        \ label-count = 0
    _DNE-LEN @ 0 DO
        _DNE-SRC @ I + C@
        DUP 46 = IF          \ '.' found
            DROP
            \ store label length at _DNE-LPTR
            OVER _DNE-LPTR @ -  1-   _DNE-LPTR @ C!
            \ set new label length position
            OVER _DNE-LPTR !
            SWAP 1+ SWAP     \ advance output ptr past length byte
        ELSE
            2 PICK C!        \ store character
            SWAP 1+ SWAP     \ advance output ptr
        THEN
    LOOP
    DROP                     \ drop label-count
    \ Store final label length
    DUP _DNE-LPTR @ -  1-  _DNE-LPTR @ C!
    \ Terminate with 0-length label
    0 OVER C!  1+
    _DNE-BUF @ SWAP ;

\ -- DNS-BUILD-QUERY: build a DNS A-record query --
\   ( domain-addr domain-len -- buf total-len )
VARIABLE _DNQ-DADDR
VARIABLE _DNQ-DLEN
: DNS-BUILD-QUERY  ( daddr dlen -- buf total )
    _DNQ-DLEN !  _DNQ-DADDR !
    DNS-BUF 512 0 FILL
    \ Header
    DNS-ID @ DNS-BUF NW16!           \ ID
    256 DNS-BUF 2 + NW16!            \ flags = 0x0100 (RD=1)
    1 DNS-BUF 4 + NW16!              \ QDCOUNT = 1
    \ Question section: encoded name + type(A=1) + class(IN=1)
    _DNQ-DADDR @ _DNQ-DLEN @ DNS-BUF /DNS-HDR + DNS-ENCODE-NAME
    NIP                               \ drop buf, keep end ptr
    DUP 1 SWAP NW16!  2 +            \ QTYPE = A (1)
    DUP 1 SWAP NW16!  2 +            \ QCLASS = IN (1)
    DNS-BUF -                         \ total length
    RANDOM16 DNS-ID !                  \ random ID per query
    DNS-BUF SWAP ;

\ -- DNS-PARSE-RESPONSE: parse DNS response, extract first A record IP --
\   ( buf len -- ip-addr | 0 )   ip-addr points to 4-byte resolved IP
\   Returns 0 if no A record found or response indicates error.
CREATE DNS-RESULT-IP  4 ALLOT

\ Helper: skip a DNS name (labels or compression pointer)
: DNS-SKIP-NAME  ( ptr -- ptr' )
    BEGIN
        DUP C@ DUP 0= IF DROP 1+ EXIT THEN
        DUP 192 AND 192 = IF DROP 2 + EXIT THEN
        1+ +
    0 UNTIL ;

VARIABLE _DNP-BUF
VARIABLE _DNP-LEN
: DNS-PARSE-RESPONSE  ( buf len -- ip | 0 )
    _DNP-LEN !  _DNP-BUF !
    \ Check response flag (bit 15 of flags = QR must be 1)
    _DNP-BUF @ 2 + NW16@ 32768 AND 0= IF 0 EXIT THEN
    \ Check RCODE (lower 4 bits of flags) = 0
    _DNP-BUF @ 2 + NW16@ 15 AND 0<> IF 0 EXIT THEN
    \ Get ANCOUNT
    _DNP-BUF @ 6 + NW16@ DUP 0= IF DROP 0 EXIT THEN
    >R                               \ save ANCOUNT
    \ Skip question section: jump over header
    _DNP-BUF @ /DNS-HDR + DNS-SKIP-NAME
    4 +                               \ skip QTYPE + QCLASS
    \ Now sitting at the answer section
    R> 0 DO                          \ iterate ANCOUNT answers
        \ Skip NAME (may be compressed pointer)
        DUP C@ 192 AND 192 = IF
            2 +
        ELSE
            BEGIN DUP C@ 0<> WHILE DUP C@ 1+ + REPEAT 1+
        THEN
        \ Read TYPE (2B) and CLASS (2B)
        DUP NW16@ 1 = IF             \ TYPE = A?
            DUP 2 + NW16@ 1 = IF     \ CLASS = IN?
                \ TTL at +4 (4B), RDLENGTH at +8 (2B), RDATA at +10
                DUP 8 + NW16@ 4 = IF  \ RDLENGTH = 4?
                    10 + DNS-RESULT-IP 4 CMOVE
                    DNS-RESULT-IP
                    UNLOOP EXIT
                THEN
            THEN
        THEN
        \ Skip this RR: +2(type) +2(class) +4(TTL) +2(rdlength) + rdlength
        DUP 8 + NW16@                \ RDLENGTH
        10 + +                       \ skip to next RR
    LOOP
    DROP 0 ;

\ -- DNS-RESOLVE: resolve a domain name to an IP address --
\   ( c-addr len -- ip-addr | 0 )
\   Sends DNS query to DNS-SERVER-IP, waits for response.
VARIABLE _DNR-DADDR
VARIABLE _DNR-DLEN
: DNS-RESOLVE  ( daddr dlen -- ip | 0 )
    _DNR-DLEN !  _DNR-DADDR !
    \ Build query
    _DNR-DADDR @ _DNR-DLEN @ DNS-BUILD-QUERY
    \ Send via UDP to DNS server, port 53
    \ DNS-BUILD-QUERY returns ( buf total )
    \ UDP-SEND expects ( dst-ip dport sport payload paylen )
    >R >R
    DNS-SERVER-IP 53 12345 R> R>     \ ( dst-ip dport sport payload paylen )
    UDP-SEND DROP                    \ ignore ior
    \ Wait for response on our port 12345
    50 0 DO
        UDP-RECV DUP 0<> IF
            \ ( src-ip udp-buf udp-len )
            >R                        \ save udp-len
            DUP UDP-H.SPORT NW16@ 53 = IF
                UDP-H.DATA R>
                /UDP-HDR -            \ ( src-ip dns-buf dns-len )
                DNS-PARSE-RESPONSE
                NIP                   \ drop src-ip
                UNLOOP EXIT
            ELSE
                DROP R> DROP          \ wrong port, discard
            THEN
        ELSE
            DROP DROP                \ drop udp-buf(0) and udp-len(0)
        THEN
        DROP                         \ drop src-ip / 0
    LOOP
    0 ;

\ =====================================================================
\  §16.7  TCP — Transmission Control Protocol (RFC 793/9293)
\ =====================================================================
\ Full TCP implementation: 3-way handshake, data transfer with
\ segmentation, sliding window, fast retransmit, congestion control
\ (slow start + congestion avoidance), and graceful teardown.
\
\ Design:
\   - 4 TCB (Transmission Control Block) slots
\   - Each TCB owns a 1460-byte TX ring and 4096-byte RX ring
\   - Retransmit timer per TCB (RTO = 1s initial, doubles on timeout)
\   - ISN via RANDOM32 for security
\   - MSS = 1460 (Ethernet MTU − IP − TCP headers)

\ -- TCP constants --
20 CONSTANT /TCP-HDR              \ minimum TCP header (no options)
4  CONSTANT /TCP-MAX-CONN         \ max simultaneous connections
1460 CONSTANT TCP-MSS             \ Max Segment Size (1500-20-20)
4096 CONSTANT /TCP-RXBUF          \ per-connection RX ring buffer
1460 CONSTANT /TCP-TXBUF          \ per-connection TX buffer (1 MSS)

\ -- TCP flags (bit positions in flags byte) --
1   CONSTANT TCP-FIN
2   CONSTANT TCP-SYN
4   CONSTANT TCP-RST
8   CONSTANT TCP-PSH
16  CONSTANT TCP-ACK
32  CONSTANT TCP-URG

\ -- TCP states (RFC 793 state machine) --
0  CONSTANT TCPS-CLOSED
1  CONSTANT TCPS-LISTEN
2  CONSTANT TCPS-SYN-SENT
3  CONSTANT TCPS-SYN-RCVD
4  CONSTANT TCPS-ESTABLISHED
5  CONSTANT TCPS-FIN-WAIT-1
6  CONSTANT TCPS-FIN-WAIT-2
7  CONSTANT TCPS-CLOSE-WAIT
8  CONSTANT TCPS-CLOSING
9  CONSTANT TCPS-LAST-ACK
10 CONSTANT TCPS-TIME-WAIT

\ =====================================================================
\  TCB — Transmission Control Block
\ =====================================================================
\ Each TCB is a contiguous region.  Field offsets:
\   +0   STATE       1 cell    TCP state enum
\   +8   LOCAL-PORT  1 cell    local port number
\  +16   REMOTE-PORT 1 cell    remote port number
\  +24   REMOTE-IP   4 bytes   remote IPv4 address
\  +32   SND-UNA     1 cell    oldest unACKed seq
\  +40   SND-NXT     1 cell    next seq to send
\  +48   SND-WND     1 cell    send window (peer's RWND)
\  +56   RCV-NXT     1 cell    next expected seq from peer
\  +64   RCV-WND     1 cell    our receive window
\  +72   ISS         1 cell    initial send seq
\  +80   IRS         1 cell    initial recv seq (peer's ISN)
\  +88   RTO-TIMER   1 cell    retransmit timeout counter (ticks)
\  +96   RTO-VALUE   1 cell    current RTO in ticks (starts ~100)
\ +104   RETRIES     1 cell    retransmit attempt count
\ +112   TX-BUF      1460B     outgoing data (1 MSS)
\ +1572  TX-LEN      1 cell    bytes in TX-BUF
\ +1580  RX-BUF      4096B     incoming data ring
\ +5676  RX-HEAD     1 cell    ring read position
\ +5684  RX-TAIL     1 cell    ring write position
\ +5692  RX-COUNT    1 cell    bytes available in RX ring
\ +5700  CWND        1 cell    congestion window (bytes)
\ +5708  SSTHRESH    1 cell    slow-start threshold
\ +5716  DUP-ACKS    1 cell    duplicate ACK counter
\ +5724  (pad to 5728)
5728 CONSTANT /TCB               \ size of one TCB

\ -- TCB field accessors (add to TCB base) --
: TCB.STATE       ( tcb -- addr )          ;
: TCB.LOCAL-PORT  ( tcb -- addr )  8  + ;
: TCB.REMOTE-PORT ( tcb -- addr )  16 + ;
: TCB.REMOTE-IP   ( tcb -- addr )  24 + ;
: TCB.SND-UNA     ( tcb -- addr )  32 + ;
: TCB.SND-NXT     ( tcb -- addr )  40 + ;
: TCB.SND-WND     ( tcb -- addr )  48 + ;
: TCB.RCV-NXT     ( tcb -- addr )  56 + ;
: TCB.RCV-WND     ( tcb -- addr )  64 + ;
: TCB.ISS         ( tcb -- addr )  72 + ;
: TCB.IRS         ( tcb -- addr )  80 + ;
: TCB.RTO-TIMER   ( tcb -- addr )  88 + ;
: TCB.RTO-VALUE   ( tcb -- addr )  96 + ;
: TCB.RETRIES     ( tcb -- addr ) 104 + ;
: TCB.TX-BUF      ( tcb -- addr ) 112 + ;
: TCB.TX-LEN      ( tcb -- addr ) 1572 + ;
: TCB.RX-BUF      ( tcb -- addr ) 1580 + ;
: TCB.RX-HEAD     ( tcb -- addr ) 5676 + ;
: TCB.RX-TAIL     ( tcb -- addr ) 5684 + ;
: TCB.RX-COUNT    ( tcb -- addr ) 5692 + ;
: TCB.CWND        ( tcb -- addr ) 5700 + ;
: TCB.SSTHRESH    ( tcb -- addr ) 5708 + ;
: TCB.DUP-ACKS    ( tcb -- addr ) 5716 + ;

\ -- TCB table (4 connections) --
CREATE TCP-TCBS  /TCB /TCP-MAX-CONN * ALLOT
TCP-TCBS /TCB /TCP-MAX-CONN * 0 FILL

\ -- TCB-N: get TCB pointer for connection index 0..3 --
: TCB-N  ( n -- tcb )  /TCB * TCP-TCBS + ;

\ -- TCB-INIT: initialise a TCB to CLOSED --
: TCB-INIT  ( tcb -- )
    DUP /TCB 0 FILL
    TCPS-CLOSED SWAP TCB.STATE !  ;

\ -- TCP-INIT-ALL: zero all TCBs --
: TCP-INIT-ALL  ( -- )
    /TCP-MAX-CONN 0 DO I TCB-N TCB-INIT LOOP ;

TCP-INIT-ALL    \ initialize at load time

\ -- TCB-ALLOC: find a free (CLOSED) TCB, return index or -1 --
: TCB-ALLOC  ( -- idx | -1 )
    /TCP-MAX-CONN 0 DO
        I TCB-N TCB.STATE @ TCPS-CLOSED = IF
            I UNLOOP EXIT
        THEN
    LOOP -1 ;

\ -- TCB-FIND: find TCB matching local-port + remote-port + remote-ip --
\   ( lport rport rip -- tcb | 0 )
VARIABLE _TCF-LP
VARIABLE _TCF-RP
VARIABLE _TCF-RIP
: TCB-FIND  ( lport rport rip -- tcb | 0 )
    _TCF-RIP !  _TCF-RP !  _TCF-LP !
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-CLOSED <> IF
            DUP TCB.LOCAL-PORT @ _TCF-LP @ = IF
            DUP TCB.REMOTE-PORT @ _TCF-RP @ = IF
            DUP TCB.REMOTE-IP 4 _TCF-RIP @ 4 COMPARE 0= IF
                UNLOOP EXIT           \ found — leave tcb on stack
            THEN THEN THEN
        THEN
        DROP
    LOOP 0 ;

\ -- TCB-FIND-LPORT: find TCB in LISTEN state on local port --
: TCB-FIND-LPORT  ( lport -- tcb | 0 )
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-LISTEN = IF
            DUP TCB.LOCAL-PORT @ 2 PICK = IF
                NIP UNLOOP EXIT
            THEN
        THEN
        DROP
    LOOP DROP 0 ;

\ =====================================================================
\  TCP header build / parse
\ =====================================================================
\ TCP header (20 bytes minimum):
\   +0   src-port   2B (BE)
\   +2   dst-port   2B (BE)
\   +4   seq        4B (BE)
\   +8   ack        4B (BE)
\  +12   data-off   1B  (upper 4 bits = header length in 32-bit words)
\  +13   flags      1B  (FIN=0x01 SYN=0x02 RST=0x04 PSH=0x08 ACK=0x10)
\  +14   window     2B (BE)
\  +16   checksum   2B (BE)
\  +18   urgent     2B (BE)

\ -- TCP header field accessors --
: TCP-H.SPORT   ( hdr -- addr )          ;    \ +0
: TCP-H.DPORT   ( hdr -- addr )  2 +  ;       \ +2
: TCP-H.SEQ     ( hdr -- addr )  4 +  ;       \ +4
: TCP-H.ACK     ( hdr -- addr )  8 +  ;       \ +8
: TCP-H.DOFF    ( hdr -- addr )  12 + ;        \ +12 data offset
: TCP-H.FLAGS   ( hdr -- addr )  13 + ;        \ +13
: TCP-H.WIN     ( hdr -- addr )  14 + ;        \ +14
: TCP-H.CKSUM   ( hdr -- addr )  16 + ;        \ +16
: TCP-H.URG     ( hdr -- addr )  18 + ;        \ +18
: TCP-H.DATA    ( hdr -- addr )  /TCP-HDR + ;  \ +20 (no options)

\ -- NW32!: store 32-bit big-endian --
: NW32!  ( val addr -- )
    OVER 24 RSHIFT OVER C!  1+
    OVER 16 RSHIFT 255 AND OVER C!  1+
    OVER 8  RSHIFT 255 AND OVER C!  1+
    SWAP 255 AND SWAP C! ;

\ -- NW32@: fetch 32-bit big-endian --
: NW32@  ( addr -- val )
    DUP C@ 24 LSHIFT
    OVER 1+ C@ 16 LSHIFT +
    OVER 2 + C@ 8 LSHIFT +
    SWAP 3 + C@ + ;

\ -- TCP TX buffer --
CREATE TCP-TX-PKT  /IP-HDR /TCP-HDR + TCP-MSS + ALLOT

\ -- TCP-BUILD: build a TCP segment --
\   ( tcb flags payload paylen -- buf total-len )
\   Builds in TCP-TX-PKT (IP header filled by IP-SEND later).
VARIABLE _TB-TCB
VARIABLE _TB-FLAGS
VARIABLE _TB-PLD
VARIABLE _TB-PLEN
: TCP-BUILD  ( tcb flags payload paylen -- buf total-len )
    _TB-PLEN !  _TB-PLD !  _TB-FLAGS !  _TB-TCB !
    \ Zero header area
    TCP-TX-PKT /TCP-HDR 0 FILL
    \ Source port
    _TB-TCB @ TCB.LOCAL-PORT @ TCP-TX-PKT TCP-H.SPORT NW16!
    \ Dest port
    _TB-TCB @ TCB.REMOTE-PORT @ TCP-TX-PKT TCP-H.DPORT NW16!
    \ Sequence number
    _TB-TCB @ TCB.SND-NXT @ TCP-TX-PKT TCP-H.SEQ NW32!
    \ ACK number (only if ACK flag set)
    _TB-FLAGS @ TCP-ACK AND IF
        _TB-TCB @ TCB.RCV-NXT @ TCP-TX-PKT TCP-H.ACK NW32!
    THEN
    \ Data offset: 5 (20 bytes header, no options) → upper nibble
    80 TCP-TX-PKT TCP-H.DOFF C!   \ 5 << 4 = 0x50
    \ Flags
    _TB-FLAGS @ TCP-TX-PKT TCP-H.FLAGS C!
    \ Window
    _TB-TCB @ TCB.RCV-WND @ TCP-TX-PKT TCP-H.WIN NW16!
    \ Copy payload after header
    _TB-PLEN @ 0 > IF
        _TB-PLD @ TCP-TX-PKT TCP-H.DATA _TB-PLEN @ CMOVE
    THEN
    \ Checksum = 0 for now; filled by TCP-FILL-CKSUM
    0 TCP-TX-PKT TCP-H.CKSUM NW16!
    TCP-TX-PKT  /TCP-HDR _TB-PLEN @ + ;

\ -- TCP-CHECKSUM: compute TCP checksum with pseudo-header --
\   Exactly like UDP-CHECKSUM but proto=6 instead of 17.
\   ( src-ip dst-ip tcp-buf tcp-len -- cksum )
VARIABLE _TCK-SUM
: TCP-CHECKSUM  ( src-ip dst-ip tcp-buf tcp-len -- cksum )
    0 _TCK-SUM !
    >R >R                              \ R: tcp-len tcp-buf
    SWAP                               \ ( dst-ip src-ip )
    DUP NW16@ _TCK-SUM @ + _TCK-SUM !  \ src-ip[0:1]
    2 + NW16@ _TCK-SUM @ + _TCK-SUM !  \ src-ip[2:3]
    DUP NW16@ _TCK-SUM @ + _TCK-SUM !  \ dst-ip[0:1]
    2 + NW16@ _TCK-SUM @ + _TCK-SUM !  \ dst-ip[2:3]
    \ proto = 6 (TCP)
    6 _TCK-SUM @ + _TCK-SUM !
    R> R>                              \ ( tcp-buf tcp-len )
    DUP _TCK-SUM @ + _TCK-SUM !       \ add tcp-len to sum
    \ Accumulate TCP header + data (16-bit words)
    DUP 1 AND >R                       \ R: odd-flag
    2 / 0 DO
        DUP NW16@ _TCK-SUM @ + _TCK-SUM !
        2 +
    LOOP
    R> IF DUP C@ 8 LSHIFT _TCK-SUM @ + _TCK-SUM ! THEN
    DROP
    \ Fold carries
    _TCK-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;

\ -- TCP-FILL-CKSUM: compute and store the TCP checksum --
\   ( src-ip dst-ip tcp-buf tcp-len -- )
VARIABLE _TFC-BUF
VARIABLE _TFC-LEN
: TCP-FILL-CKSUM  ( src-ip dst-ip buf len -- )
    _TFC-LEN !  _TFC-BUF !
    \ Zero the checksum field
    0 _TFC-BUF @ TCP-H.CKSUM NW16!
    \ Compute checksum
    _TFC-BUF @ _TFC-LEN @  TCP-CHECKSUM
    \ Per TCP spec: if checksum is 0, keep 0 (unlike UDP)
    _TFC-BUF @ TCP-H.CKSUM NW16! ;

\ -- TCP-VERIFY-CKSUM: verify TCP checksum --
\   ( src-ip dst-ip tcp-buf tcp-len -- flag )  -1 if valid, 0 if bad
: TCP-VERIFY-CKSUM  ( src-ip dst-ip buf len -- flag )
    TCP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- TCP-PARSE: extract fields from received TCP header --
\   ( tcp-buf -- sport dport seq ack flags win datalen )
: TCP-PARSE  ( buf -- sport dport seq ack flags win datalen )
    DUP TCP-H.SPORT NW16@
    OVER TCP-H.DPORT NW16@
    2 PICK TCP-H.SEQ NW32@
    3 PICK TCP-H.ACK NW32@
    4 PICK TCP-H.FLAGS C@
    5 PICK TCP-H.WIN NW16@
    \ datalen = total - header (data offset field >> 4 gives 32-bit words)
    7 PICK IP-H.TLEN NW16@ /IP-HDR -       \ TCP segment length
    7 PICK TCP-H.DOFF C@ 4 RSHIFT 4 *      \ header length from doff
    -                                        \ payload length
    >R >R >R >R >R >R >R
    DROP
    R> R> R> R> R> R> R> ;

\ =====================================================================
\  TCP segment send / receive
\ =====================================================================

\ -- TCP-SEND-SEG: build, checksum, send a TCP segment via IP --
\   ( tcb flags payload paylen -- ior )
VARIABLE _TSS-TCB
: TCP-SEND-SEG  ( tcb flags payload paylen -- ior )
    >R >R >R                           \ save flags payload paylen
    _TSS-TCB !                          \ store tcb
    _TSS-TCB @ R> R> R>                 \ ( tcb flags payload paylen )
    TCP-BUILD                           \ ( buf seg-len )
    \ Fill checksum: need MY-IP and remote-ip
    2DUP >R >R
    MY-IP _TSS-TCB @ TCB.REMOTE-IP R> R>  \ ( buf len src dst buf len )
    TCP-FILL-CKSUM                      \ ( buf len )
    \ Send via IP-SEND
    >R >R
    IP-PROTO-TCP _TSS-TCB @ TCB.REMOTE-IP R> R>  \ ( proto dst buf len )
    IP-SEND ;

\ -- TCP-SEND-CTL: send a control segment (no payload) --
\   ( tcb flags -- ior )
: TCP-SEND-CTL  ( tcb flags -- ior )
    0 0 TCP-SEND-SEG ;

\ -- TCP-SEND-RST: send a RST in response to unexpected segment --
\   ( remote-ip rport lport seq -- )
\   Builds a raw RST without a TCB.
VARIABLE _TR-RIPVAR
CREATE _TR-PSEUDO-TCB  /TCB ALLOT
: TCP-SEND-RST  ( remote-ip rport lport seq -- )
    >R                                 \ save seq
    _TR-PSEUDO-TCB /TCB 0 FILL
    _TR-PSEUDO-TCB TCB.LOCAL-PORT !
    _TR-PSEUDO-TCB TCB.REMOTE-PORT !
    _TR-RIPVAR !
    _TR-RIPVAR @ _TR-PSEUDO-TCB TCB.REMOTE-IP 4 CMOVE
    R> _TR-PSEUDO-TCB TCB.SND-NXT !
    0 _TR-PSEUDO-TCB TCB.RCV-NXT !
    0 _TR-PSEUDO-TCB TCB.RCV-WND !
    _TR-PSEUDO-TCB TCP-RST TCP-ACK OR TCP-SEND-CTL DROP ;

\ =====================================================================
\  TCP RX ring buffer operations
\ =====================================================================

\ -- TCP-RX-PUSH: add data to a TCB's RX ring buffer --
\   ( tcb addr len -- actual )
\   Returns number of bytes actually pushed (may be less if ring full).
VARIABLE _TRP-TCB
VARIABLE _TRP-SRC
VARIABLE _TRP-LEN
VARIABLE _TRP-ACTUAL
: TCP-RX-PUSH  ( tcb addr len -- actual )
    _TRP-LEN !  _TRP-SRC !  _TRP-TCB !
    \ Available space = RXBUF size - current count
    /TCP-RXBUF _TRP-TCB @ TCB.RX-COUNT @ -
    _TRP-LEN @ MIN  _TRP-ACTUAL !
    _TRP-ACTUAL @ 0= IF 0 EXIT THEN
    \ Copy byte by byte into ring (simple; could optimize later)
    _TRP-ACTUAL @ 0 DO
        _TRP-SRC @ I + C@
        _TRP-TCB @ TCB.RX-BUF
        _TRP-TCB @ TCB.RX-TAIL @ + C!
        _TRP-TCB @ TCB.RX-TAIL @  1+ /TCP-RXBUF MOD
        _TRP-TCB @ TCB.RX-TAIL !
    LOOP
    _TRP-ACTUAL @ _TRP-TCB @ TCB.RX-COUNT +!
    _TRP-ACTUAL @ ;

\ -- TCP-RX-POP: read data from a TCB's RX ring buffer --
\   ( tcb addr maxlen -- actual )
VARIABLE _TRPOP-TCB
VARIABLE _TRPOP-DST
VARIABLE _TRPOP-MAX
VARIABLE _TRPOP-ACTUAL
: TCP-RX-POP  ( tcb addr maxlen -- actual )
    _TRPOP-MAX !  _TRPOP-DST !  _TRPOP-TCB !
    _TRPOP-TCB @ TCB.RX-COUNT @
    _TRPOP-MAX @ MIN  _TRPOP-ACTUAL !
    _TRPOP-ACTUAL @ 0= IF 0 EXIT THEN
    _TRPOP-ACTUAL @ 0 DO
        _TRPOP-TCB @ TCB.RX-BUF
        _TRPOP-TCB @ TCB.RX-HEAD @ + C@
        _TRPOP-DST @ I + C!
        _TRPOP-TCB @ TCB.RX-HEAD @  1+ /TCP-RXBUF MOD
        _TRPOP-TCB @ TCB.RX-HEAD !
    LOOP
    _TRPOP-ACTUAL @ NEGATE _TRPOP-TCB @ TCB.RX-COUNT +!
    _TRPOP-ACTUAL @ ;

\ =====================================================================
\  TCP state machine — input processing (RFC 793 §3.9)
\ =====================================================================

\ Helper: generate initial sequence number
: TCP-GEN-ISN  ( -- isn )  RANDOM32 ;

\ Helper: TCP sequence-number comparison (handles 32-bit wrap)
\   SEQ< returns true if a < b (mod 2^32)
: SEQ<  ( a b -- flag )
    - DUP 0< IF DROP -1 ELSE
      DUP 0= IF DROP 0 ELSE
      DROP 0 THEN THEN ;

\ Helper: SEQ>=
: SEQ>=  ( a b -- flag )  SEQ< 0= ;

\ -- TCP-INPUT: process a received TCP segment --
\   Called with the IP header pointer and total IP length.
\   Demuxes to the correct TCB and drives the state machine.
\   ( ip-hdr ip-len -- )
VARIABLE _TI-HDR
VARIABLE _TI-IPLEN
VARIABLE _TI-TCPHDR
VARIABLE _TI-TCPLEN
VARIABLE _TI-TCB
VARIABLE _TI-SEQ
VARIABLE _TI-ACK
VARIABLE _TI-FLAGS
VARIABLE _TI-WIN
VARIABLE _TI-DATALEN
VARIABLE _TI-SPORT
VARIABLE _TI-DPORT
VARIABLE _TI-DATA

\ Helper: SEQ> (strictly greater)
: SEQ>  ( a b -- flag )  SWAP SEQ< ;

\ -- TCP-INPUT-LISTEN: handle segment in LISTEN state --
\   Only SYN is valid; allocate a new TCB for the connection.
: TCP-INPUT-LISTEN  ( tcb -- )
    \ If not SYN, ignore
    _TI-FLAGS @ TCP-SYN AND 0= IF DROP EXIT THEN
    \ Set up connection in this TCB
    DUP >R
    _TI-HDR @ IP-H.SRC R@ TCB.REMOTE-IP 4 CMOVE
    _TI-SPORT @ R@ TCB.REMOTE-PORT !
    _TI-SEQ @ 1+ R@ TCB.RCV-NXT !         \ SYN consumes 1 seq
    _TI-SEQ @ R@ TCB.IRS !
    TCP-GEN-ISN DUP R@ TCB.ISS !
    DUP R@ TCB.SND-NXT !
    R@ TCB.SND-UNA !
    _TI-WIN @ R@ TCB.SND-WND !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    100 R@ TCB.RTO-VALUE !                 \ ~1s at 100 ticks
    TCPS-SYN-RCVD R@ TCB.STATE !
    \ Send SYN+ACK
    R> TCP-SYN TCP-ACK OR TCP-SEND-CTL DROP ;

\ -- TCP-INPUT-SYN-SENT: handle segment in SYN-SENT state --
\   Expecting SYN+ACK from peer (active open).
: TCP-INPUT-SYN-SENT  ( tcb -- )
    >R
    \ Must have ACK
    _TI-FLAGS @ TCP-ACK AND IF
        \ Verify ACK covers our SYN
        _TI-ACK @ R@ TCB.ISS @ 1+ <> IF
            R> DROP EXIT               \ bad ACK — ignore
        THEN
    THEN
    \ Must have SYN
    _TI-FLAGS @ TCP-SYN AND 0= IF R> DROP EXIT THEN
    \ Accept connection
    _TI-SEQ @ R@ TCB.IRS !
    _TI-SEQ @ 1+ R@ TCB.RCV-NXT !
    _TI-ACK @ R@ TCB.SND-UNA !
    _TI-WIN @ R@ TCB.SND-WND !
    \ Advance SND-NXT past our SYN
    R@ TCB.ISS @ 1+ R@ TCB.SND-NXT !
    TCPS-ESTABLISHED R@ TCB.STATE !
    \ Send ACK
    R> TCP-ACK TCP-SEND-CTL DROP ;

\ -- TCP-INPUT-ESTABLISHED-ETC: common handler for states 4-10 --
\   Handles data delivery, ACK processing, FIN processing.
: TCP-INPUT-ESTABLISHED-ETC  ( tcb -- )
    >R
    \ --- Check RST ---
    _TI-FLAGS @ TCP-RST AND IF
        TCPS-CLOSED R@ TCB.STATE !
        R> TCB-INIT EXIT
    THEN
    \ --- Check SYN (unexpected → RST) ---
    _TI-FLAGS @ TCP-SYN AND IF
        TCPS-CLOSED R@ TCB.STATE !
        R> TCB-INIT EXIT
    THEN
    \ --- Process ACK ---
    _TI-FLAGS @ TCP-ACK AND IF
        R@ TCB.STATE @ TCPS-SYN-RCVD = IF
            \ Transition to ESTABLISHED
            TCPS-ESTABLISHED R@ TCB.STATE !
        THEN
        \ Update SND-UNA if ACK advances it
        _TI-ACK @ R@ TCB.SND-UNA @ SEQ>= IF
            _TI-ACK @ R@ TCB.SND-UNA !
            \ Reset retransmit state
            0 R@ TCB.RETRIES !
            100 R@ TCB.RTO-VALUE !
            0 R@ TCB.DUP-ACKS !
            \ Congestion control: grow CWND
            R@ TCB.CWND @ R@ TCB.SSTHRESH @ < IF
                \ Slow start: CWND += MSS
                TCP-MSS R@ TCB.CWND +!
            ELSE
                \ Congestion avoidance: CWND += MSS*MSS/CWND
                TCP-MSS TCP-MSS * R@ TCB.CWND @ /
                1 MAX R@ TCB.CWND +!
            THEN
        ELSE
            \ Duplicate ACK handling (fast retransmit)
            1 R@ TCB.DUP-ACKS +!
            R@ TCB.DUP-ACKS @ 3 = IF
                \ Fast retransmit: halve CWND, retransmit
                R@ TCB.CWND @ 2 / TCP-MSS MAX R@ TCB.SSTHRESH !
                R@ TCB.SSTHRESH @ R@ TCB.CWND !
                \ Retransmit unACKed data from TX-BUF
                R@ TCB.TX-LEN @ 0 > IF
                    R@ TCP-ACK TCP-PSH OR
                    R@ TCB.TX-BUF R@ TCB.TX-LEN @
                    TCP-SEND-SEG DROP
                THEN
            THEN
        THEN
        \ Update send window
        _TI-WIN @ R@ TCB.SND-WND !
        \ Handle FIN-WAIT-1 → FIN-WAIT-2 if all data ACKed
        R@ TCB.STATE @ TCPS-FIN-WAIT-1 = IF
            R@ TCB.SND-NXT @ R@ TCB.SND-UNA @ = IF
                TCPS-FIN-WAIT-2 R@ TCB.STATE !
            THEN
        THEN
        \ Handle CLOSING → TIME-WAIT
        R@ TCB.STATE @ TCPS-CLOSING = IF
            TCPS-TIME-WAIT R@ TCB.STATE !
        THEN
        \ Handle LAST-ACK → CLOSED
        R@ TCB.STATE @ TCPS-LAST-ACK = IF
            R@ TCB-INIT
            R> DROP EXIT
        THEN
    THEN
    \ --- Process data (ESTABLISHED, FIN-WAIT-1, FIN-WAIT-2) ---
    _TI-DATALEN @ 0 > IF
        R@ TCB.STATE @ TCPS-ESTABLISHED =
        R@ TCB.STATE @ TCPS-FIN-WAIT-1 = OR
        R@ TCB.STATE @ TCPS-FIN-WAIT-2 = OR IF
            \ Check sequence number matches expected
            _TI-SEQ @ R@ TCB.RCV-NXT @ = IF
                \ Push data into RX ring
                R@ _TI-DATA @ _TI-DATALEN @ TCP-RX-PUSH
                R@ TCB.RCV-NXT +!     \ advance RCV-NXT by actual bytes consumed
                \ Update receive window
                /TCP-RXBUF R@ TCB.RX-COUNT @ - R@ TCB.RCV-WND !
                \ Send ACK
                R@ TCP-ACK TCP-SEND-CTL DROP
            ELSE
                \ Out-of-order: send duplicate ACK
                R@ TCP-ACK TCP-SEND-CTL DROP
            THEN
        THEN
    THEN
    \ --- Process FIN ---
    _TI-FLAGS @ TCP-FIN AND IF
        _TI-SEQ @ _TI-DATALEN @ + R@ TCB.RCV-NXT @ = IF
            \ Advance RCV-NXT past FIN
            R@ TCB.RCV-NXT @ 1+ R@ TCB.RCV-NXT !
            R@ TCB.STATE @
            CASE
                TCPS-ESTABLISHED OF
                    TCPS-CLOSE-WAIT R@ TCB.STATE !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
                TCPS-FIN-WAIT-1 OF
                    TCPS-CLOSING R@ TCB.STATE !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
                TCPS-FIN-WAIT-2 OF
                    TCPS-TIME-WAIT R@ TCB.STATE !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
            ENDCASE
        THEN
    THEN
    R> DROP ;

\ -- TCP-INPUT: process a received TCP segment --
\   Called with the IP header pointer and total IP length.
\   Demuxes to the correct TCB and drives the state machine.
\   ( ip-hdr ip-len -- )
: TCP-INPUT  ( ip-hdr ip-len -- )
    _TI-IPLEN !  _TI-HDR !
    \ Extract TCP header from IP payload
    _TI-HDR @ IP-H.DATA _TI-TCPHDR !
    _TI-IPLEN @ /IP-HDR - _TI-TCPLEN !
    \ Verify TCP checksum
    _TI-HDR @ IP-H.SRC
    _TI-HDR @ IP-H.DST
    _TI-TCPHDR @ _TI-TCPLEN @
    TCP-VERIFY-CKSUM 0= IF EXIT THEN
    \ Extract header fields
    _TI-TCPHDR @ TCP-H.SPORT NW16@  _TI-SPORT !
    _TI-TCPHDR @ TCP-H.DPORT NW16@  _TI-DPORT !
    _TI-TCPHDR @ TCP-H.SEQ NW32@    _TI-SEQ !
    _TI-TCPHDR @ TCP-H.ACK NW32@    _TI-ACK !
    _TI-TCPHDR @ TCP-H.FLAGS C@     _TI-FLAGS !
    _TI-TCPHDR @ TCP-H.WIN NW16@    _TI-WIN !
    \ Data starts after TCP header (use data offset field)
    _TI-TCPHDR @ TCP-H.DOFF C@ 4 RSHIFT 4 *
    _TI-TCPHDR @ + _TI-DATA !
    \ Data length = TCP segment length - TCP header length
    _TI-TCPLEN @ _TI-TCPHDR @ TCP-H.DOFF C@ 4 RSHIFT 4 * - _TI-DATALEN !
    \ Look up TCB: try exact match first, then LISTEN match
    _TI-DPORT @ _TI-SPORT @ _TI-HDR @ IP-H.SRC TCB-FIND
    DUP 0= IF
        DROP _TI-DPORT @ TCB-FIND-LPORT
    THEN
    DUP 0= IF
        \ No matching TCB — send RST (unless incoming is RST)
        _TI-FLAGS @ TCP-RST AND 0= IF
            _TI-HDR @ IP-H.SRC _TI-SPORT @ _TI-DPORT @
            _TI-SEQ @ _TI-DATALEN @ + TCP-SEND-RST
        THEN
        DROP EXIT
    THEN
    _TI-TCB !
    \ Dispatch on TCB state
    _TI-TCB @ TCB.STATE @
    CASE
        TCPS-LISTEN   OF  _TI-TCB @ TCP-INPUT-LISTEN   ENDOF
        TCPS-SYN-SENT OF  _TI-TCB @ TCP-INPUT-SYN-SENT ENDOF
        \ All other states use a common handler
        _TI-TCB @ TCP-INPUT-ESTABLISHED-ETC
    ENDCASE ;

\ =====================================================================
\  TCP user API — connect, listen, send, recv, close
\ =====================================================================

\ -- TCP-CONNECT: active open (client) --
\   ( remote-ip remote-port local-port -- tcb | 0 )
VARIABLE _TC-RIP
VARIABLE _TC-RPORT
VARIABLE _TC-LPORT
: TCP-CONNECT  ( rip rport lport -- tcb | 0 )
    _TC-LPORT !  _TC-RPORT !  _TC-RIP !
    TCB-ALLOC DUP -1 = IF DROP 0 EXIT THEN
    TCB-N >R
    R@ /TCB 0 FILL
    _TC-LPORT @ R@ TCB.LOCAL-PORT !
    _TC-RPORT @ R@ TCB.REMOTE-PORT !
    _TC-RIP @ R@ TCB.REMOTE-IP 4 CMOVE
    TCP-GEN-ISN DUP R@ TCB.ISS !
    R@ TCB.SND-NXT !
    0 R@ TCB.SND-UNA !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    100 R@ TCB.RTO-VALUE !
    TCPS-SYN-SENT R@ TCB.STATE !
    \ Send SYN
    R@ TCP-SYN TCP-SEND-CTL DROP
    R> ;

\ -- TCP-LISTEN: passive open (server) --
\   ( local-port -- tcb | 0 )
: TCP-LISTEN  ( lport -- tcb | 0 )
    TCB-ALLOC DUP -1 = IF 2DROP 0 EXIT THEN
    TCB-N >R
    R@ /TCB 0 FILL
    R@ TCB.LOCAL-PORT !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    TCPS-LISTEN R@ TCB.STATE !
    R> ;

\ -- TCP-SEND: queue data for transmission --
\   ( tcb addr len -- actual )
\   Copies data into the TCB's TX-BUF and sends a segment.
\   Returns number of bytes actually accepted (up to 1 MSS).
VARIABLE _TSND-TCB
VARIABLE _TSND-SRC
VARIABLE _TSND-LEN
: TCP-SEND  ( tcb addr len -- actual )
    _TSND-LEN !  _TSND-SRC !  _TSND-TCB !
    _TSND-TCB @ TCB.STATE @ TCPS-ESTABLISHED <> IF 0 EXIT THEN
    _TSND-LEN @ TCP-MSS MIN  _TSND-LEN !
    \ Copy into TCB's TX-BUF
    _TSND-SRC @ _TSND-TCB @ TCB.TX-BUF _TSND-LEN @ CMOVE
    _TSND-LEN @ _TSND-TCB @ TCB.TX-LEN !
    \ Send segment
    _TSND-TCB @ TCP-ACK TCP-PSH OR
    _TSND-TCB @ TCB.TX-BUF _TSND-LEN @
    TCP-SEND-SEG DROP
    \ Advance SND-NXT
    _TSND-LEN @ _TSND-TCB @ TCB.SND-NXT +!
    \ Start retransmit timer
    _TSND-TCB @ TCB.RTO-VALUE @ _TSND-TCB @ TCB.RTO-TIMER !
    _TSND-LEN @ ;

\ -- TCP-RECV: read received data from RX ring --
\   ( tcb addr maxlen -- actual )
: TCP-RECV  ( tcb addr maxlen -- actual )
    TCP-RX-POP ;

\ -- TCP-CLOSE: initiate graceful close --
\   ( tcb -- )
: TCP-CLOSE  ( tcb -- )
    DUP TCB.STATE @
    CASE
        TCPS-ESTABLISHED OF
            TCPS-FIN-WAIT-1 OVER TCB.STATE !
            DUP TCP-FIN TCP-ACK OR TCP-SEND-CTL DROP
            1 SWAP TCB.SND-NXT +!    \ FIN consumes 1 seq number
        ENDOF
        TCPS-CLOSE-WAIT OF
            TCPS-LAST-ACK OVER TCB.STATE !
            DUP TCP-FIN TCP-ACK OR TCP-SEND-CTL DROP
            1 SWAP TCB.SND-NXT +!    \ FIN consumes 1 seq number
        ENDOF
        TCPS-LISTEN OF  TCB-INIT  ENDOF
        TCPS-SYN-SENT OF  TCB-INIT  ENDOF
        \ other states: just reset
        SWAP TCB-INIT
    ENDCASE ;

\ -- TCP-POLL: poll network for incoming TCP segments --
\   Receives one IP frame; if TCP, processes it.
\   ( -- )
VARIABLE _TPL-HDR
VARIABLE _TPL-LEN
: TCP-POLL  ( -- )
    IP-RECV DUP 0= IF 2DROP EXIT THEN
    _TPL-LEN !  _TPL-HDR !
    \ Handle ICMP pings transparently
    _TPL-HDR @ IP-H.PROTO C@ IP-PROTO-ICMP = IF
        _TPL-HDR @ _TPL-LEN @ ICMP-HANDLE DROP EXIT
    THEN
    \ If TCP, process
    _TPL-HDR @ IP-H.PROTO C@ IP-PROTO-TCP = IF
        _TPL-HDR @ _TPL-LEN @ TCP-INPUT EXIT
    THEN ;

\ -- TCP-POLL-WAIT: blocking TCP poll with timeout --
\   ( max-attempts -- )
: TCP-POLL-WAIT  ( n -- )
    0 DO TCP-POLL LOOP ;

\ =====================================================================
\  §16.8  TLS 1.3 Record Layer
\ =====================================================================
\
\  TLS 1.3 (RFC 8446) record-level encrypt/decrypt.
\  Cipher suite: X25519 + AES-256-GCM + SHA3-256 (non-standard hash).
\
\  Record format (§5.1):
\    ContentType      = 23 (app_data) for all encrypted records
\    ProtocolVersion  = 0x0303
\    Length           = len(ciphertext) + 16 (tag)
\    EncryptedRecord  = AES-256-GCM( inner_plaintext || content_type )
\
\  Nonce (§5.3):  nonce = write_iv XOR padded_seqnum
\    padded_seqnum = 4 zero bytes || 8-byte sequence number (big-endian)
\
\  Uses AES-ENCRYPT-AEAD / AES-DECRYPT-AEAD from §1.5.

\ --- Content Type Constants ---
21 CONSTANT TLS-CT-ALERT
22 CONSTANT TLS-CT-HANDSHAKE
23 CONSTANT TLS-CT-APP-DATA
20 CONSTANT TLS-CT-CCS

\ --- TLS Context Structure ---
\  Per-connection TLS state.  4 contexts, one per TCB slot.
\
\  Offset  Field          Size  Description
\  +0      STATE          8     0=NONE 1=HS 2=ESTABLISHED 3=CLOSING
\  +8      TCB-IDX        8     Associated TCB slot index
\  +16     WR-KEY         32    Our write key (e.g., client_traffic_key)
\  +48     WR-IV          12    Our write IV
\  +60     PAD            4     (alignment)
\  +64     WR-SEQ         8     Write sequence number
\  +72     RD-KEY         32    Peer write key (e.g., server_traffic_key)
\  +104    RD-IV          12    Peer write IV
\  +116    PAD            4     (alignment)
\  +120    RD-SEQ         8     Read sequence number
\  +128    HS-STATE       8     Handshake sub-state
\  +136    TRANSCRIPT     32    Running SHA3-256 transcript hash
\  +168    HS-SECRET      32    Handshake secret
\  +200    MS-SECRET      32    Master secret
\  +232    MY-PRIVKEY     32    X25519 ephemeral private key
\  +264    MY-PUBKEY      32    X25519 ephemeral public key
\  +296    PEER-PUBKEY    32    Peer X25519 public key
\  +328    SHARED-SECRET  32    X25519 shared secret
\  +360    EARLY-SECRET   32    Early secret (from HKDF-Extract)
\  +392    C-HS-TRAFFIC   32    Client handshake traffic secret
\  +424    S-HS-TRAFFIC   32    Server handshake traffic secret
\  +456    C-AP-TRAFFIC   32    Client application traffic secret
\  +488    S-AP-TRAFFIC   32    Server application traffic secret
\  +520    PSK            32    Pre-shared key (reserved)
\  Total: 552 bytes

552 CONSTANT /TLS-CTX
4 CONSTANT TLS-MAX-CTX

: TLS-CTX.STATE       ( ctx -- addr )       ;  \ +0
: TLS-CTX.TCB         ( ctx -- addr )  8 +  ;
: TLS-CTX.WR-KEY      ( ctx -- addr )  16 + ;
: TLS-CTX.WR-IV       ( ctx -- addr )  48 + ;
: TLS-CTX.WR-SEQ      ( ctx -- addr )  64 + ;
: TLS-CTX.RD-KEY      ( ctx -- addr )  72 + ;
: TLS-CTX.RD-IV       ( ctx -- addr )  104 + ;
: TLS-CTX.RD-SEQ      ( ctx -- addr )  120 + ;
: TLS-CTX.HS-STATE    ( ctx -- addr )  128 + ;
: TLS-CTX.TRANSCRIPT  ( ctx -- addr )  136 + ;
: TLS-CTX.HS-SECRET   ( ctx -- addr )  168 + ;
: TLS-CTX.MS-SECRET   ( ctx -- addr )  200 + ;
: TLS-CTX.MY-PRIVKEY  ( ctx -- addr )  232 + ;
: TLS-CTX.MY-PUBKEY   ( ctx -- addr )  264 + ;
: TLS-CTX.PEER-PUBKEY ( ctx -- addr )  296 + ;
: TLS-CTX.SHARED      ( ctx -- addr )  328 + ;
: TLS-CTX.EARLY       ( ctx -- addr )  360 + ;
: TLS-CTX.C-HS-TRAFFIC ( ctx -- addr ) 392 + ;
: TLS-CTX.S-HS-TRAFFIC ( ctx -- addr ) 424 + ;
: TLS-CTX.C-AP-TRAFFIC ( ctx -- addr ) 456 + ;
: TLS-CTX.S-AP-TRAFFIC ( ctx -- addr ) 488 + ;
: TLS-CTX.PSK          ( ctx -- addr ) 520 + ;

CREATE TLS-CTXS  /TLS-CTX TLS-MAX-CTX * ALLOT
TLS-CTXS /TLS-CTX TLS-MAX-CTX * 0 FILL

\ TLS-CTX@ ( idx -- ctx-addr )  Get context by index (0..3).
: TLS-CTX@ ( idx -- ctx )
    /TLS-CTX * TLS-CTXS + ;

\ --- TLS State Constants ---
0 CONSTANT TLSS-NONE
1 CONSTANT TLSS-HANDSHAKE
2 CONSTANT TLSS-ESTABLISHED
3 CONSTANT TLSS-CLOSING

\ --- Handshake Sub-States ---
0 CONSTANT TLSH-IDLE
1 CONSTANT TLSH-CLIENT-HELLO-SENT
2 CONSTANT TLSH-SERVER-HELLO-RCVD
3 CONSTANT TLSH-WAIT-EE
4 CONSTANT TLSH-WAIT-CERT
5 CONSTANT TLSH-WAIT-CV
6 CONSTANT TLSH-WAIT-FINISHED
7 CONSTANT TLSH-CONNECTED

\ --- Scratch Buffers for Record Layer ---
CREATE TLS-NONCE-BUF  12 ALLOT        \ constructed per-record nonce
CREATE TLS-REC-HDR     5 ALLOT        \ 5-byte TLS record header (AAD)
CREATE TLS-PAD-BUF    16 ALLOT        \ plaintext padding to 16B boundary
CREATE TLS-INNER-BUF 1500 ALLOT       \ inner plaintext + content type byte
CREATE TLS-CIPHER-BUF 1520 ALLOT      \ ciphertext output
CREATE TLS-PLAIN-BUF  1500 ALLOT      \ decrypted plaintext scratch

\ --- TLS Nonce Construction ---
\ TLS-BUILD-NONCE ( iv seq out -- )
\   nonce = iv XOR (0x00000000 || seq_be64)
\   iv is 12 bytes, seq is a 64-bit cell (native endian).
\   out receives 12 bytes.
: TLS-BUILD-NONCE ( iv seq out -- )
    >R                         \ R: out
    \ Convert seq to 8-byte big-endian in out[4..11]
    \ First, copy iv to out
    SWAP R@ 12 CMOVE           \ copy iv → out.  Stack: seq  R: out
    \ XOR seq bytes into out[4..11] (big-endian)
    DUP 56 RSHIFT 255 AND R@  4 + C@ XOR R@  4 + C!
    DUP 48 RSHIFT 255 AND R@  5 + C@ XOR R@  5 + C!
    DUP 40 RSHIFT 255 AND R@  6 + C@ XOR R@  6 + C!
    DUP 32 RSHIFT 255 AND R@  7 + C@ XOR R@  7 + C!
    DUP 24 RSHIFT 255 AND R@  8 + C@ XOR R@  8 + C!
    DUP 16 RSHIFT 255 AND R@  9 + C@ XOR R@  9 + C!
    DUP  8 RSHIFT 255 AND R@ 10 + C@ XOR R@ 10 + C!
                  255 AND R@ 11 + C@ XOR R@ 11 + C!
    R> DROP
;

\ --- TLS Record Encryption ---
\ TLS-ENCRYPT-RECORD ( ctx content-type plaintext plen rec-buf -- rec-len )
\   Builds a TLS 1.3 encrypted record:
\     rec-buf = [type=23 | ver=0x0303 | length(BE16)] ++ ciphertext ++ tag
\   The inner plaintext = plaintext || content_type_byte, padded to 16B.
\   Returns total record length (5 + padded_inner + 16).
VARIABLE _TER-CTX
VARIABLE _TER-CTYPE
VARIABLE _TER-PT
VARIABLE _TER-PLEN
VARIABLE _TER-REC
VARIABLE _TER-PADLEN

: TLS-ENCRYPT-RECORD ( ctx ctype pt plen rec -- reclen )
    _TER-REC !  _TER-PLEN !  _TER-PT !
    _TER-CTYPE !  _TER-CTX !
    \ 1. Build inner plaintext: data || content_type, pad to 16B
    _TER-PT @ TLS-INNER-BUF _TER-PLEN @ CMOVE
    _TER-CTYPE @ TLS-INNER-BUF _TER-PLEN @ + C!
    _TER-PLEN @ 1+ 15 + -16 AND _TER-PADLEN !
    \ Zero-pad remainder
    _TER-PLEN @ 1+ _TER-PADLEN @ < IF
        TLS-INNER-BUF _TER-PLEN @ 1+ +
        _TER-PADLEN @ _TER-PLEN @ 1+ - 0 FILL
    THEN
    \ 2. Build nonce from write IV + write seq
    _TER-CTX @ TLS-CTX.WR-IV
    _TER-CTX @ TLS-CTX.WR-SEQ @
    TLS-NONCE-BUF TLS-BUILD-NONCE
    \ 3. Build AAD (5-byte record header)
    TLS-CT-APP-DATA TLS-REC-HDR C!
    3 TLS-REC-HDR 1 + C!   3 TLS-REC-HDR 2 + C!
    _TER-PADLEN @ 16 +
    DUP 8 RSHIFT TLS-REC-HDR 3 + C!
    255 AND      TLS-REC-HDR 4 + C!
    \ 4. Copy header to rec-buf
    TLS-REC-HDR _TER-REC @ 5 CMOVE
    \ 5. Encrypt with AAD
    _TER-CTX @ TLS-CTX.WR-KEY  TLS-NONCE-BUF
    TLS-REC-HDR 5
    TLS-INNER-BUF  _TER-REC @ 5 +  _TER-PADLEN @
    AES-ENCRYPT-AEAD DROP
    \ 6. Copy tag after ciphertext
    AES-TAG-BUF  _TER-REC @ 5 + _TER-PADLEN @ +  16 CMOVE
    \ 7. Increment write sequence number
    _TER-CTX @ TLS-CTX.WR-SEQ DUP @ 1+ SWAP !
    \ 8. Return total record length
    _TER-PADLEN @ 5 + 16 +
;

\ --- TLS Record Decryption ---
\ TLS-DECRYPT-RECORD ( ctx rec-buf rec-len plain-buf -- ctype plen | -1 0 )
\   Decrypts a TLS 1.3 record.
\   rec-buf starts with 5-byte header, then ciphertext, then 16-byte tag.
\   Returns content type and plaintext length, or -1 0 on auth failure.
VARIABLE _TDR-CTX
VARIABLE _TDR-REC
VARIABLE _TDR-RLEN
VARIABLE _TDR-PLAIN
VARIABLE _TDR-CLEN

: TLS-DECRYPT-RECORD ( ctx rec rlen plain -- ctype plen | -1 0 )
    _TDR-PLAIN !  _TDR-RLEN !  _TDR-REC !  _TDR-CTX !
    \ Encrypted payload length = rec-len - 5 (header) - 16 (tag)
    _TDR-RLEN @ 5 - 16 -  DUP _TDR-CLEN !
    DUP 1 < IF DROP -1 0 EXIT THEN
    DROP
    \ 1. Build nonce from read IV + read seq
    _TDR-CTX @ TLS-CTX.RD-IV
    _TDR-CTX @ TLS-CTX.RD-SEQ @
    TLS-NONCE-BUF TLS-BUILD-NONCE
    \ 2. Decrypt: AES-DECRYPT-AEAD(rd-key, nonce, hdr, 5, ct, plain, clen, tag)
    _TDR-CTX @ TLS-CTX.RD-KEY  TLS-NONCE-BUF
    _TDR-REC @ 5                                     \ AAD = rec[0..4], len=5
    _TDR-REC @ 5 +                                   \ ciphertext start
    _TDR-PLAIN @                                     \ plaintext output
    _TDR-CLEN @                                      \ cipher_len
    _TDR-REC @ 5 + _TDR-CLEN @ +                     \ tag address
    AES-DECRYPT-AEAD                                  \ → flag (0=ok, -1=fail)
    0<> IF -1 0 EXIT THEN                             \ auth failure
    \ 3. Increment read sequence number
    _TDR-CTX @ TLS-CTX.RD-SEQ DUP @ 1+ SWAP !
    \ 4. Extract inner content type = last non-zero byte of plaintext
    _TDR-CLEN @
    BEGIN
        1-
        DUP 0< IF DROP -1 0 EXIT THEN
        DUP _TDR-PLAIN @ + C@ 0<>
    UNTIL
    DUP _TDR-PLAIN @ + C@  SWAP                      \ ctype plen
;

\ .TLS-STATUS ( ctx -- )  Print TLS context status.
: .TLS-STATUS ( ctx -- )
    DUP TLS-CTX.STATE @
    DUP TLSS-NONE        = IF DROP ."  TLS: none" CR ELSE
    DUP TLSS-HANDSHAKE   = IF DROP ."  TLS: handshake" CR ELSE
    DUP TLSS-ESTABLISHED = IF DROP ."  TLS: established" CR ELSE
    DUP TLSS-CLOSING     = IF DROP ."  TLS: closing" CR ELSE
    DROP ."  TLS: unknown" CR
    THEN THEN THEN THEN
    DROP                              \ drop ctx
;

\ =====================================================================
\  §16.9  TLS 1.3 Handshake (RFC 8446)
\ =====================================================================
\
\  Full TLS 1.3 handshake state machine (client side).
\  Cipher suite 0xFF01: TLS_X25519_AES_256_GCM_SHA3_256 (private use).
\
\  Flow: CH → SH → [EncExt] → [Cert] → [CertVerify] → Finished → Finished
\
\  Certificates are hashed into transcript but not validated (no X.509).
\  HelloRetryRequest → abort.  CCS records → silently ignored by caller.

\ --- Cipher Suite ---
65281 CONSTANT TLS-SUITE-X25519-SHA3    \ 0xFF01

\ --- Handshake Message Types ---
1  CONSTANT TLSHT-CLIENT-HELLO
2  CONSTANT TLSHT-SERVER-HELLO
8  CONSTANT TLSHT-ENCRYPTED-EXT
11 CONSTANT TLSHT-CERTIFICATE
15 CONSTANT TLSHT-CERT-VERIFY
20 CONSTANT TLSHT-FINISHED

\ --- Scratch Buffers for Handshake ---
CREATE TLS-HKDF-LABEL  64 ALLOT
CREATE TLS-HS-TRANSCRIPT 1024 ALLOT
VARIABLE TLS-HS-TR-LEN
CREATE TLS-CH-BUF 256 ALLOT
CREATE TLS-HS-HASH 32 ALLOT
CREATE TLS-TEMP-SECRET 32 ALLOT
CREATE TLS-TEMP-SECRET2 32 ALLOT
CREATE TLS-FINISHED-KEY 32 ALLOT
CREATE TLS-VERIFY-DATA 32 ALLOT
CREATE TLS-FINISHED-MSG 40 ALLOT

\ Pre-compute SHA3-256 of empty string (for "derived" key schedule steps).
CREATE TLS-EMPTY-HASH 32 ALLOT
SHA3-INIT  TLS-EMPTY-HASH SHA3-FINAL

\ --- TLS 1.3 Label Strings ---
\ Labels WITHOUT "tls13 " prefix (added by TLS-EXPAND-LABEL).
CREATE TLS-L-DERIVED    100 C, 101 C, 114 C, 105 C, 118 C, 101 C, 100 C,
7 CONSTANT /TLS-L-DERIVED

CREATE TLS-L-C-HS-TR   99 C, 32 C, 104 C, 115 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-C-HS-TR

CREATE TLS-L-S-HS-TR   115 C, 32 C, 104 C, 115 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-S-HS-TR

CREATE TLS-L-KEY   107 C, 101 C, 121 C,
3 CONSTANT /TLS-L-KEY

CREATE TLS-L-IV   105 C, 118 C,
2 CONSTANT /TLS-L-IV

CREATE TLS-L-C-AP-TR   99 C, 32 C, 97 C, 112 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-C-AP-TR

CREATE TLS-L-S-AP-TR   115 C, 32 C, 97 C, 112 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-S-AP-TR

CREATE TLS-L-FINISHED   102 C, 105 C, 110 C, 105 C, 115 C, 104 C, 101 C, 100 C,
8 CONSTANT /TLS-L-FINISHED

\ --- TLS-EXPAND-LABEL ---
\ TLS-EXPAND-LABEL ( secret label llen context clen olen out -- )
\   Construct HkdfLabel and call HKDF-EXPAND.
\   HkdfLabel = length(2BE) || (6+llen)(1) || "tls13 " || label || clen(1) || ctx
VARIABLE _TEL-SECRET
VARIABLE _TEL-LABEL
VARIABLE _TEL-LLEN
VARIABLE _TEL-CTXP
VARIABLE _TEL-CLEN
VARIABLE _TEL-OLEN
VARIABLE _TEL-OUT

: TLS-EXPAND-LABEL ( secret label llen context clen olen out -- )
    _TEL-OUT !  _TEL-OLEN !  _TEL-CLEN !  _TEL-CTXP !
    _TEL-LLEN !  _TEL-LABEL !  _TEL-SECRET !
    \ [0-1] output length (big-endian)
    _TEL-OLEN @ 8 RSHIFT TLS-HKDF-LABEL C!
    _TEL-OLEN @ 255 AND TLS-HKDF-LABEL 1+ C!
    \ [2] total label length = 6 + llen
    _TEL-LLEN @ 6 + TLS-HKDF-LABEL 2 + C!
    \ [3..8] "tls13 "
    116 TLS-HKDF-LABEL 3 + C!
    108 TLS-HKDF-LABEL 4 + C!
    115 TLS-HKDF-LABEL 5 + C!
    49  TLS-HKDF-LABEL 6 + C!
    51  TLS-HKDF-LABEL 7 + C!
    32  TLS-HKDF-LABEL 8 + C!
    \ [9..9+llen-1] label bytes
    _TEL-LABEL @ TLS-HKDF-LABEL 9 + _TEL-LLEN @ CMOVE
    \ [9+llen] context length
    _TEL-CLEN @ TLS-HKDF-LABEL 9 + _TEL-LLEN @ + C!
    \ [10+llen..] context bytes (if any)
    _TEL-CLEN @ 0> IF
        _TEL-CTXP @ TLS-HKDF-LABEL 10 + _TEL-LLEN @ + _TEL-CLEN @ CMOVE
    THEN
    \ HKDF-EXPAND( prk, info, info_len, output_len, output )
    \ info_len = 2 + 1 + 6 + llen + 1 + clen = 10 + llen + clen
    _TEL-SECRET @ TLS-HKDF-LABEL
    _TEL-LLEN @ 10 + _TEL-CLEN @ +
    _TEL-OLEN @ _TEL-OUT @ HKDF-EXPAND
;

\ --- Transcript Management ---
: TLS-TR-RESET ( -- )  0 TLS-HS-TR-LEN ! ;

VARIABLE _TTA-SRC
VARIABLE _TTA-LEN

: TLS-TR-APPEND ( addr len -- )
    _TTA-LEN !  _TTA-SRC !
    _TTA-LEN @ TLS-HS-TR-LEN @ + 1024 > IF EXIT THEN
    _TTA-SRC @ TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ + _TTA-LEN @ CMOVE
    _TTA-LEN @ TLS-HS-TR-LEN @ + TLS-HS-TR-LEN !
;

\ --- TLS-DERIVE-DERIVED ---
\ TLS-DERIVE-DERIVED ( secret out -- )
\   = HKDF-Expand-Label(Secret, "derived", SHA3-256(""), 32)
: TLS-DERIVE-DERIVED ( secret out -- )
    >R
    TLS-L-DERIVED /TLS-L-DERIVED
    TLS-EMPTY-HASH 32 32 R> TLS-EXPAND-LABEL
;

\ --- TLS-DERIVE-SECRET ---
\ TLS-DERIVE-SECRET ( secret label llen out -- )
\   = HKDF-Expand-Label(Secret, Label, Hash(Messages), 32)
\   Messages = current transcript buffer.
VARIABLE _TDS-SECRET
VARIABLE _TDS-LABEL
VARIABLE _TDS-LLEN
VARIABLE _TDS-OUT

: TLS-DERIVE-SECRET ( secret label llen out -- )
    _TDS-OUT !  _TDS-LLEN !  _TDS-LABEL !  _TDS-SECRET !
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH SHA3
    \ HKDF-Expand-Label(secret, label, llen, hash, 32, 32, out)
    _TDS-SECRET @ _TDS-LABEL @ _TDS-LLEN @
    TLS-HS-HASH 32 32 _TDS-OUT @ TLS-EXPAND-LABEL
;

\ --- Key Schedule Phase 1 ---
\ TLS-KS-HANDSHAKE ( ctx -- )
\   Derive and install handshake traffic keys.
\   Prerequisite: ctx.SHARED filled, transcript = CH||SH.
VARIABLE _TKSH-CTX

: TLS-KS-HANDSHAKE ( ctx -- )
    _TKSH-CTX !
    \ 1. Early Secret = HKDF-Extract(0*32, 0*32)
    0 0  _HKDF-ZERO-SALT 32  _TKSH-CTX @ TLS-CTX.EARLY  HKDF-EXTRACT
    \ 2. derived_es = Expand-Label(ES, "derived", empty_hash, 32)
    _TKSH-CTX @ TLS-CTX.EARLY  TLS-TEMP-SECRET  TLS-DERIVE-DERIVED
    \ 3. HS = HKDF-Extract(derived_es, shared_secret)
    TLS-TEMP-SECRET 32  _TKSH-CTX @ TLS-CTX.SHARED 32
    _TKSH-CTX @ TLS-CTX.HS-SECRET  HKDF-EXTRACT
    \ 4. c_hs_traffic = Derive-Secret(HS, "c hs traffic", Hash(CH||SH))
    _TKSH-CTX @ TLS-CTX.HS-SECRET  TLS-L-C-HS-TR /TLS-L-C-HS-TR
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC  TLS-DERIVE-SECRET
    \ 5. s_hs_traffic = Derive-Secret(HS, "s hs traffic", Hash(CH||SH))
    _TKSH-CTX @ TLS-CTX.HS-SECRET  TLS-L-S-HS-TR /TLS-L-S-HS-TR
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC  TLS-DERIVE-SECRET
    \ 6-7. Client HS key+IV → WR-KEY/WR-IV
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  32  _TKSH-CTX @ TLS-CTX.WR-KEY
    TLS-EXPAND-LABEL
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSH-CTX @ TLS-CTX.WR-IV
    TLS-EXPAND-LABEL
    \ 8-9. Server HS key+IV → RD-KEY/RD-IV
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  32  _TKSH-CTX @ TLS-CTX.RD-KEY
    TLS-EXPAND-LABEL
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSH-CTX @ TLS-CTX.RD-IV
    TLS-EXPAND-LABEL
    \ 10. Zero sequence numbers
    0 _TKSH-CTX @ TLS-CTX.WR-SEQ !
    0 _TKSH-CTX @ TLS-CTX.RD-SEQ !
;

\ --- Key Schedule Phase 2 ---
\ TLS-KS-APPLICATION ( ctx -- )
\   Derive and install application traffic keys from master secret.
\   Prerequisite: HS-SECRET set, transcript through server Finished.
VARIABLE _TKSA-CTX

: TLS-KS-APPLICATION ( ctx -- )
    _TKSA-CTX !
    \ 1. derived_hs = Expand-Label(HS, "derived", empty_hash, 32)
    _TKSA-CTX @ TLS-CTX.HS-SECRET  TLS-TEMP-SECRET  TLS-DERIVE-DERIVED
    \ 2. MS = HKDF-Extract(derived_hs, 0*32)
    TLS-TEMP-SECRET 32  _HKDF-ZERO-SALT 32
    _TKSA-CTX @ TLS-CTX.MS-SECRET  HKDF-EXTRACT
    \ 3. c_ap_traffic = Derive-Secret(MS, "c ap traffic", Hash(full))
    _TKSA-CTX @ TLS-CTX.MS-SECRET  TLS-L-C-AP-TR /TLS-L-C-AP-TR
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC  TLS-DERIVE-SECRET
    \ 4. s_ap_traffic = Derive-Secret(MS, "s ap traffic", Hash(full))
    _TKSA-CTX @ TLS-CTX.MS-SECRET  TLS-L-S-AP-TR /TLS-L-S-AP-TR
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC  TLS-DERIVE-SECRET
    \ 5-6. Client app key+IV → WR-KEY/WR-IV
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  32  _TKSA-CTX @ TLS-CTX.WR-KEY
    TLS-EXPAND-LABEL
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSA-CTX @ TLS-CTX.WR-IV
    TLS-EXPAND-LABEL
    \ 7-8. Server app key+IV → RD-KEY/RD-IV
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  32  _TKSA-CTX @ TLS-CTX.RD-KEY
    TLS-EXPAND-LABEL
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSA-CTX @ TLS-CTX.RD-IV
    TLS-EXPAND-LABEL
    \ 9. Zero sequence numbers
    0 _TKSA-CTX @ TLS-CTX.WR-SEQ !
    0 _TKSA-CTX @ TLS-CTX.RD-SEQ !
    \ 10. Mark connection established
    TLSS-ESTABLISHED _TKSA-CTX @ TLS-CTX.STATE !
    TLSH-CONNECTED _TKSA-CTX @ TLS-CTX.HS-STATE !
;

\ --- ClientHello Builder ---
\ TLS-BUILD-CLIENT-HELLO ( ctx -- addr len )
\   Build complete ClientHello record (149 bytes).
\   Generates X25519 ephemeral keypair, stores in context.
\   Appends handshake message to transcript.
\   Returns buffer address and total length.
VARIABLE _TBCH-CTX

: TLS-BUILD-CLIENT-HELLO ( ctx -- addr len )
    _TBCH-CTX !
    TLS-TR-RESET
    \ Generate ephemeral keypair
    X25519-KEYGEN
    X25519-PRIV _TBCH-CTX @ TLS-CTX.MY-PRIVKEY 32 CMOVE
    X25519-PUB  _TBCH-CTX @ TLS-CTX.MY-PUBKEY  32 CMOVE
    \ --- TLS Record Header (5 bytes) ---
    TLS-CT-HANDSHAKE TLS-CH-BUF C!                    \ [0] = 22
    3 TLS-CH-BUF 1 + C!   1 TLS-CH-BUF 2 + C!        \ [1-2] = 0x0301
    0 TLS-CH-BUF 3 + C!   144 TLS-CH-BUF 4 + C!      \ [3-4] len=144
    \ --- Handshake Header (4 bytes) ---
    TLSHT-CLIENT-HELLO TLS-CH-BUF 5 + C!              \ [5] = 1
    0 TLS-CH-BUF 6 + C!  0 TLS-CH-BUF 7 + C!         \ [6-8] = 140
    140 TLS-CH-BUF 8 + C!
    \ --- ClientHello Body ---
    3 TLS-CH-BUF 9 + C!   3 TLS-CH-BUF 10 + C!       \ [9-10] 0x0303
    32 0 DO RANDOM8 TLS-CH-BUF 11 I + + C! LOOP       \ [11-42] random
    32 TLS-CH-BUF 43 + C!                              \ [43] sid_len=32
    32 0 DO RANDOM8 TLS-CH-BUF 44 I + + C! LOOP       \ [44-75] sid
    0 TLS-CH-BUF 76 + C!  2 TLS-CH-BUF 77 + C!       \ [76-77] suites_len=2
    255 TLS-CH-BUF 78 + C!  1 TLS-CH-BUF 79 + C!     \ [78-79] 0xFF01
    1 TLS-CH-BUF 80 + C!                               \ [80] comp_len=1
    0 TLS-CH-BUF 81 + C!                               \ [81] null comp
    0 TLS-CH-BUF 82 + C!  65 TLS-CH-BUF 83 + C!      \ [82-83] ext_len=65
    \ --- Extensions ---
    \ supported_versions (0x002B): 7 bytes
    0  TLS-CH-BUF 84 + C!   43 TLS-CH-BUF 85 + C!
    0  TLS-CH-BUF 86 + C!   3  TLS-CH-BUF 87 + C!
    2  TLS-CH-BUF 88 + C!
    3  TLS-CH-BUF 89 + C!   4  TLS-CH-BUF 90 + C!    \ 0x0304
    \ key_share (0x0033): 42 bytes
    0  TLS-CH-BUF 91 + C!   51 TLS-CH-BUF 92 + C!
    0  TLS-CH-BUF 93 + C!   38 TLS-CH-BUF 94 + C!
    0  TLS-CH-BUF 95 + C!   36 TLS-CH-BUF 96 + C!
    0  TLS-CH-BUF 97 + C!   29 TLS-CH-BUF 98 + C!    \ x25519
    0  TLS-CH-BUF 99 + C!   32 TLS-CH-BUF 100 + C!
    _TBCH-CTX @ TLS-CTX.MY-PUBKEY TLS-CH-BUF 101 + 32 CMOVE
    \ signature_algorithms (0x000D): 8 bytes
    0  TLS-CH-BUF 133 + C!  13 TLS-CH-BUF 134 + C!
    0  TLS-CH-BUF 135 + C!  4  TLS-CH-BUF 136 + C!
    0  TLS-CH-BUF 137 + C!  2  TLS-CH-BUF 138 + C!
    8  TLS-CH-BUF 139 + C!  7  TLS-CH-BUF 140 + C!   \ ed25519
    \ supported_groups (0x000A): 8 bytes
    0  TLS-CH-BUF 141 + C!  10 TLS-CH-BUF 142 + C!
    0  TLS-CH-BUF 143 + C!  4  TLS-CH-BUF 144 + C!
    0  TLS-CH-BUF 145 + C!  2  TLS-CH-BUF 146 + C!
    0  TLS-CH-BUF 147 + C!  29 TLS-CH-BUF 148 + C!   \ x25519
    \ Set handshake state
    TLSS-HANDSHAKE _TBCH-CTX @ TLS-CTX.STATE !
    TLSH-CLIENT-HELLO-SENT _TBCH-CTX @ TLS-CTX.HS-STATE !
    \ Append handshake message to transcript (bytes 5..148 = 144 bytes)
    TLS-CH-BUF 5 + 144 TLS-TR-APPEND
    \ Return buffer + total length
    TLS-CH-BUF 149
;

\ --- ServerHello Parser ---
\ TLS-PARSE-SERVER-HELLO ( ctx msg mlen -- flag )
\   Parse ServerHello handshake message (without TLS record header).
\   msg starts at handshake type byte.  Extracts peer X25519 public key.
\   Returns 0 on success, -1 on error.
VARIABLE _TPSH-CTX
VARIABLE _TPSH-MSG
VARIABLE _TPSH-MLEN
VARIABLE _TPSH-POS
VARIABLE _TPSH-SIDLEN
VARIABLE _TPSH-EXTLEN
VARIABLE _TPSH-ETYPE
VARIABLE _TPSH-ELEN
VARIABLE _TPSH-OK

: TLS-PARSE-SERVER-HELLO ( ctx msg mlen -- flag )
    _TPSH-MLEN !  _TPSH-MSG !  _TPSH-CTX !
    0 _TPSH-OK !
    \ Verify handshake type = 2
    _TPSH-MSG @ C@ TLSHT-SERVER-HELLO <> IF -1 EXIT THEN
    \ +4: server_version(2), +6: random(32), +38: session_id_len
    _TPSH-MSG @ 38 + C@ _TPSH-SIDLEN !
    39 _TPSH-SIDLEN @ + _TPSH-POS !
    \ cipher_suite (2 bytes)
    _TPSH-MSG @ _TPSH-POS @ + C@ 8 LSHIFT
    _TPSH-MSG @ _TPSH-POS @ 1+ + C@ OR
    TLS-SUITE-X25519-SHA3 <> IF -1 _TPSH-OK ! THEN
    _TPSH-POS @ 2 + _TPSH-POS !
    \ compression_method (1 byte, must be 0)
    _TPSH-MSG @ _TPSH-POS @ + C@ 0<> IF -1 _TPSH-OK ! THEN
    _TPSH-POS @ 1+ _TPSH-POS !
    \ extensions_len (2 bytes)
    _TPSH-MSG @ _TPSH-POS @ + C@ 8 LSHIFT
    _TPSH-MSG @ _TPSH-POS @ 1+ + C@ OR
    _TPSH-EXTLEN !
    _TPSH-POS @ 2 + _TPSH-POS !
    \ Walk extensions
    _TPSH-EXTLEN @
    BEGIN DUP 0> WHILE
        _TPSH-MSG @ _TPSH-POS @ + C@ 8 LSHIFT
        _TPSH-MSG @ _TPSH-POS @ 1+ + C@ OR  _TPSH-ETYPE !
        _TPSH-MSG @ _TPSH-POS @ 2 + + C@ 8 LSHIFT
        _TPSH-MSG @ _TPSH-POS @ 3 + + C@ OR  _TPSH-ELEN !
        _TPSH-POS @ 4 + _TPSH-POS !
        \ key_share (0x0033 = 51)
        _TPSH-ETYPE @ 51 = IF
            _TPSH-MSG @ _TPSH-POS @ + C@ 8 LSHIFT
            _TPSH-MSG @ _TPSH-POS @ 1+ + C@ OR
            29 <> IF -1 _TPSH-OK ! THEN
            _TPSH-MSG @ _TPSH-POS @ 4 + +
            _TPSH-CTX @ TLS-CTX.PEER-PUBKEY 32 CMOVE
        THEN
        \ supported_versions (0x002B = 43)
        _TPSH-ETYPE @ 43 = IF
            _TPSH-MSG @ _TPSH-POS @ + C@ 8 LSHIFT
            _TPSH-MSG @ _TPSH-POS @ 1+ + C@ OR
            772 <> IF -1 _TPSH-OK ! THEN
        THEN
        _TPSH-POS @ _TPSH-ELEN @ + _TPSH-POS !
        _TPSH-ELEN @ 4 + -
    REPEAT DROP
    _TPSH-OK @
;

\ --- Finished MAC Verification ---
\ TLS-VERIFY-FINISHED ( traffic-secret verify-data -- flag )
\   Verify a Finished message's verify_data (32 bytes).
\   Returns 0 if valid, -1 if not.
VARIABLE _TVF-SECRET
VARIABLE _TVF-VERIFY

: TLS-VERIFY-FINISHED ( traffic-secret verify-data -- flag )
    _TVF-VERIFY !  _TVF-SECRET !
    \ finished_key = HKDF-Expand-Label(secret, "finished", "", 32)
    _TVF-SECRET @
    TLS-L-FINISHED /TLS-L-FINISHED  0 0  32  TLS-FINISHED-KEY
    TLS-EXPAND-LABEL
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH SHA3
    \ expected = HMAC(finished_key, transcript_hash)
    TLS-FINISHED-KEY 32  TLS-HS-HASH 32  TLS-VERIFY-DATA  HMAC
    \ Compare with received
    TLS-VERIFY-DATA _TVF-VERIFY @ 32 VERIFY
;

\ --- Client Finished Builder ---
\ TLS-BUILD-FINISHED ( ctx rec -- reclen )
\   Build and encrypt client Finished message.
\   Does NOT append to transcript (caller manages for key derivation order).
\   Returns encrypted record length.
VARIABLE _TBF-CTX
VARIABLE _TBF-REC

: TLS-BUILD-FINISHED ( ctx rec -- reclen )
    _TBF-REC !  _TBF-CTX !
    \ finished_key = Expand-Label(c_hs_traffic, "finished", "", 32)
    _TBF-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-FINISHED /TLS-L-FINISHED  0 0  32  TLS-FINISHED-KEY
    TLS-EXPAND-LABEL
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH SHA3
    \ verify_data = HMAC(finished_key, hash)
    TLS-FINISHED-KEY 32  TLS-HS-HASH 32  TLS-VERIFY-DATA  HMAC
    \ Build Finished handshake message (36 bytes)
    TLSHT-FINISHED  TLS-FINISHED-MSG C!
    0  TLS-FINISHED-MSG 1 + C!
    0  TLS-FINISHED-MSG 2 + C!
    32 TLS-FINISHED-MSG 3 + C!
    TLS-VERIFY-DATA TLS-FINISHED-MSG 4 + 32 CMOVE
    \ Encrypt: TLS-ENCRYPT-RECORD(ctx, HANDSHAKE, msg, 36, rec)
    _TBF-CTX @ TLS-CT-HANDSHAKE TLS-FINISHED-MSG 36 _TBF-REC @
    TLS-ENCRYPT-RECORD
;

\ --- Handshake Message Processor ---
\ TLS-PROCESS-HS-MSG ( ctx msg mlen -- flag )
\   Process a received handshake message (decrypted, no record header).
\   Dispatches on handshake type, updates transcript and state.
\   Returns 0 for success, -1 for error.
VARIABLE _TPHM-CTX
VARIABLE _TPHM-MSG
VARIABLE _TPHM-MLEN
VARIABLE _TPHM-TYPE

: TLS-PROCESS-HS-MSG ( ctx msg mlen -- flag )
    _TPHM-MLEN !  _TPHM-MSG !  _TPHM-CTX !
    _TPHM-MSG @ C@ _TPHM-TYPE !
    _TPHM-TYPE @ TLSHT-SERVER-HELLO = IF
        _TPHM-CTX @ _TPHM-MSG @ _TPHM-MLEN @ TLS-PARSE-SERVER-HELLO
        DUP 0<> IF EXIT THEN DROP
        \ Append SH to transcript
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        \ Compute shared secret via X25519
        _TPHM-CTX @ TLS-CTX.MY-PRIVKEY X25519-PRIV 32 CMOVE
        _TPHM-CTX @ TLS-CTX.PEER-PUBKEY X25519-DH
        X25519-SHARED _TPHM-CTX @ TLS-CTX.SHARED 32 CMOVE
        \ Key schedule phase 1
        _TPHM-CTX @ TLS-KS-HANDSHAKE
        TLSH-SERVER-HELLO-RCVD _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-ENCRYPTED-EXT = IF
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLSH-WAIT-EE _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-CERTIFICATE = IF
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLSH-WAIT-CERT _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-CERT-VERIFY = IF
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLSH-WAIT-CV _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-FINISHED = IF
        \ Verify server Finished MAC (transcript without this Finished)
        _TPHM-CTX @ TLS-CTX.S-HS-TRAFFIC
        _TPHM-MSG @ 4 +
        TLS-VERIFY-FINISHED
        DUP 0<> IF EXIT THEN DROP
        \ Append server Finished to transcript
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLSH-WAIT-FINISHED _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    \ Unknown type — hash into transcript, continue
    _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
    0
;

\ --- Handshake Completion ---
\ TLS-HANDSHAKE-COMPLETE ( ctx rec -- reclen )
\   After server Finished verified and appended to transcript:
\   1. Build+encrypt client Finished (using handshake keys)
\   2. Derive and install application traffic keys
\   Returns encrypted client Finished record length.
VARIABLE _THC-CTX
VARIABLE _THC-REC

: TLS-HANDSHAKE-COMPLETE ( ctx rec -- reclen )
    _THC-REC !  _THC-CTX !
    _THC-CTX @  _THC-REC @  TLS-BUILD-FINISHED
    _THC-CTX @ TLS-KS-APPLICATION
;

\ =====================================================================
\  §16.10  TLS 1.3 Application Data (RFC 8446 §5.1)
\ =====================================================================
\
\  High-level words for sending/receiving application data over an
\  established TLS connection.  Uses the record layer from §16.8.
\
\  TLS-SEND-DATA  ( ctx addr len -- actual )
\  TLS-RECV-DATA  ( ctx addr maxlen -- actual | -1 )
\  TLS-SEND-ALERT ( ctx level desc -- )
\  TLS-CLOSE      ( ctx -- )

CREATE TLS-SEND-REC 1600 ALLOT
CREATE TLS-RECV-REC 1600 ALLOT

\ --- TLS-SEND-DATA ---
\ Encrypt plaintext as app_data record and send via TCP.
VARIABLE _TSD-CTX
VARIABLE _TSD-SRC
VARIABLE _TSD-LEN

: TLS-SEND-DATA ( ctx addr len -- actual )
    _TSD-LEN !  _TSD-SRC !  _TSD-CTX !
    _TSD-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED <> IF 0 EXIT THEN
    _TSD-LEN @ 1400 MIN _TSD-LEN !
    \ Encrypt
    _TSD-CTX @  TLS-CT-APP-DATA  _TSD-SRC @  _TSD-LEN @
    TLS-SEND-REC  TLS-ENCRYPT-RECORD
    \ Send via TCP
    _TSD-CTX @ TLS-CTX.TCB @   TLS-SEND-REC  ROT  TCP-SEND
    DUP 0> IF DROP _TSD-LEN @ ELSE DROP 0 THEN
;

\ --- TLS-RECV-DATA ---
\ Receive TCP data, decrypt, and return plaintext.
\ Returns actual bytes received, or -1 on decryption error, or 0 if nothing.
VARIABLE _TRD-CTX
VARIABLE _TRD-DST
VARIABLE _TRD-MAXLEN
VARIABLE _TRD-RLEN

: TLS-RECV-DATA ( ctx addr maxlen -- actual | -1 )
    _TRD-MAXLEN !  _TRD-DST !  _TRD-CTX !
    _TRD-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED <> IF 0 EXIT THEN
    \ Try to receive raw TCP data
    _TRD-CTX @ TLS-CTX.TCB @  TLS-RECV-REC 1600  TCP-RECV
    DUP 0= IF EXIT THEN
    _TRD-RLEN !
    \ Decrypt
    _TRD-CTX @  TLS-RECV-REC  _TRD-RLEN @  TLS-PLAIN-BUF
    TLS-DECRYPT-RECORD
    \ Stack: ctype plen  (or -1 0)
    DUP 0= IF 2DROP -1 EXIT THEN \ decrypt failed
    SWAP TLS-CT-APP-DATA <> IF DROP -1 EXIT THEN \ not app data
    \ Copy min(plen, maxlen) to destination
    _TRD-MAXLEN @ MIN
    TLS-PLAIN-BUF _TRD-DST @ ROT DUP >R CMOVE
    R>
;

\ --- TLS-SEND-ALERT ---
\ Send a TLS alert (e.g., close_notify = level=1, desc=0).
CREATE TLS-ALERT-BUF 2 ALLOT

: TLS-SEND-ALERT ( ctx level desc -- )
    TLS-ALERT-BUF 1+ C!   TLS-ALERT-BUF C!
    TLS-CT-ALERT  TLS-ALERT-BUF  2
    TLS-SEND-REC  TLS-ENCRYPT-RECORD
    \ Send via TCP
    SWAP TLS-CTX.TCB @  TLS-SEND-REC  ROT  TCP-SEND DROP
;

\ --- TLS-CLOSE ---
\ Send close_notify alert and close TCP connection.
: TLS-CLOSE ( ctx -- )
    DUP TLS-CTX.STATE @ TLSS-ESTABLISHED <> IF DROP EXIT THEN
    DUP 1 0 TLS-SEND-ALERT                   \ close_notify
    DUP TLSS-CLOSING SWAP TLS-CTX.STATE !
    TLS-CTX.TCB @ TCP-CLOSE
;

\ =====================================================================
\  §16.11  TLS 1.3 Connection API
\ =====================================================================
\
\  User-facing words for TLS connections.
\
\  TLS-CONNECT ( rip rport lport -- ctx | 0 )
\    Allocate TLS context, do TCP connect, run full TLS handshake.
\    Blocks until handshake completes or times out.
\
\  TLS-SEND ( ctx addr len -- actual )
\    Encrypt and send application data.  (Alias for TLS-SEND-DATA.)
\
\  TLS-RECV ( ctx addr maxlen -- actual | -1 )
\    Receive and decrypt application data.  (Alias for TLS-RECV-DATA.)

\ -- TLS context allocator --
: TLS-CTX-ALLOC ( -- ctx | 0 )
    TLS-MAX-CTX 0 DO
        I TLS-CTX@ TLS-CTX.STATE @ TLSS-NONE = IF
            I TLS-CTX@ UNLOOP EXIT
        THEN
    LOOP 0 ;

\ -- TLS-CONNECT --
VARIABLE _TLSC-CTX
VARIABLE _TLSC-TCB
VARIABLE _TLSC-RLEN
VARIABLE _TLSC-CTYPE

: TLS-CONNECT ( rip rport lport -- ctx | 0 )
    >R >R >R
    \ 1. Allocate TLS context
    TLS-CTX-ALLOC DUP 0= IF R> R> R> 2DROP DROP EXIT THEN
    _TLSC-CTX !
    _TLSC-CTX @ /TLS-CTX 0 FILL
    \ 2. TCP connect
    R> R> R> ROT SWAP ROT TCP-CONNECT
    DUP 0= IF _TLSC-CTX @ DROP EXIT THEN
    _TLSC-TCB !
    _TLSC-TCB @ _TLSC-CTX @ TLS-CTX.TCB !
    \ 3. Wait for TCP established
    50 TCP-POLL-WAIT
    _TLSC-TCB @ TCB.STATE @ TCPS-ESTABLISHED <> IF 0 EXIT THEN
    \ 4. Build+send ClientHello
    _TLSC-CTX @ TLS-BUILD-CLIENT-HELLO
    _TLSC-TCB @ ROT ROT TCP-SEND DROP
    20 TCP-POLL-WAIT
    \ 5. Receive ServerHello (plaintext)
    _TLSC-TCB @ TLS-RECV-REC 1600 TCP-RECV
    DUP 0= IF DROP 0 EXIT THEN
    _TLSC-RLEN !
    \ Parse SH: skip 5-byte TLS record header
    _TLSC-CTX @  TLS-RECV-REC 5 +  _TLSC-RLEN @ 5 -
    TLS-PROCESS-HS-MSG 0<> IF 0 EXIT THEN
    \ 6. Receive encrypted handshake messages
    \ (EncryptedExtensions, optionally Cert/CertVerify, server Finished)
    20 TCP-POLL-WAIT
    BEGIN
        _TLSC-CTX @ TLS-CTX.HS-STATE @ TLSH-WAIT-FINISHED <
    WHILE
        _TLSC-TCB @ TLS-RECV-REC 1600 TCP-RECV
        DUP 0= IF DROP 0 EXIT THEN
        _TLSC-RLEN !
        \ Decrypt record
        _TLSC-CTX @ TLS-RECV-REC _TLSC-RLEN @ TLS-PLAIN-BUF
        TLS-DECRYPT-RECORD
        DUP 0= IF 2DROP 0 EXIT THEN
        _TLSC-CTYPE !
        _TLSC-CTYPE @ TLS-CT-CCS = IF DROP
        ELSE
            _TLSC-CTX @ TLS-PLAIN-BUF ROT TLS-PROCESS-HS-MSG
            0<> IF 0 EXIT THEN
        THEN
        10 TCP-POLL-WAIT
    REPEAT
    \ 7. Send client Finished + install app keys
    _TLSC-CTX @  TLS-SEND-REC  TLS-HANDSHAKE-COMPLETE
    _TLSC-TCB @  TLS-SEND-REC  ROT  TCP-SEND DROP
    \ 8. Return context
    _TLSC-CTX @
;

\ -- TLS-SEND / TLS-RECV convenience aliases --
: TLS-SEND ( ctx addr len -- actual )  TLS-SEND-DATA ;
: TLS-RECV ( ctx addr maxlen -- actual | -1 )  TLS-RECV-DATA ;

\ =====================================================================
\  §17  Socket API
\ =====================================================================
\
\  BSD-style socket abstraction over TCP and TLS.
\
\  Socket descriptor table: 8 slots.
\  Each socket is 32 bytes:
\    +0   STATE      8    0=FREE 1=TCP 2=TLS 3=LISTENING 4=ACCEPTED
\    +8   TCB/CTX    8    TCB pointer (TCP) or TLS-CTX pointer (TLS)
\    +16  LOCAL-PORT 8    Local port number
\    +24  FLAGS      8    Bit0: TLS mode
\
\  API:
\    SOCKET     ( type -- sd | -1 )   type: 0=TCP, 1=TLS
\    BIND       ( sd port -- ior )
\    LISTEN     ( sd -- ior )
\    ACCEPT     ( sd -- new-sd | -1 )
\    CONNECT    ( sd rip rport -- ior )
\    SEND       ( sd addr len -- actual )
\    RECV       ( sd addr maxlen -- actual )
\    CLOSE      ( sd -- )

\ --- Socket Constants ---
32 CONSTANT /SOCK
8  CONSTANT SOCK-MAX

0 CONSTANT SOCKST-FREE
1 CONSTANT SOCKST-TCP
2 CONSTANT SOCKST-TLS
3 CONSTANT SOCKST-LISTENING
4 CONSTANT SOCKST-ACCEPTED

0 CONSTANT SOCK-TYPE-TCP
1 CONSTANT SOCK-TYPE-TLS

\ --- Socket Descriptor Table ---
CREATE SOCK-TABLE  /SOCK SOCK-MAX * ALLOT
SOCK-TABLE /SOCK SOCK-MAX * 0 FILL

: SOCK-N ( n -- addr )  /SOCK * SOCK-TABLE + ;
: SOCK.STATE      ( sd -- addr )           ;
: SOCK.HANDLE     ( sd -- addr )   8  + ;
: SOCK.LOCAL-PORT ( sd -- addr )   16 + ;
: SOCK.FLAGS      ( sd -- addr )   24 + ;

\ --- SOCKET ( type -- sd | -1 ) ---
\ Allocate a new socket descriptor.
VARIABLE _SOK-TYPE

: SOCKET ( type -- sd | -1 )
    _SOK-TYPE !
    SOCK-MAX 0 DO
        I SOCK-N SOCK.STATE @ SOCKST-FREE = IF
            I SOCK-N /SOCK 0 FILL
            _SOK-TYPE @ 1 AND I SOCK-N SOCK.FLAGS !
            _SOK-TYPE @ 1 AND IF SOCKST-TLS ELSE SOCKST-TCP THEN
            I SOCK-N SOCK.STATE !
            I SOCK-N UNLOOP EXIT
        THEN
    LOOP -1 ;

\ --- BIND ( sd port -- ior ) ---
: BIND ( sd port -- ior )
    SWAP SOCK.LOCAL-PORT ! 0 ;

\ --- LISTEN ( sd -- ior ) ---
\ Move to LISTENING state, open TCP passive listener.
VARIABLE _SLSN-SD

: LISTEN ( sd -- ior )
    _SLSN-SD !
    _SLSN-SD @ SOCK.LOCAL-PORT @ TCP-LISTEN
    DUP 0= IF DROP -1 EXIT THEN
    _SLSN-SD @ SOCK.HANDLE !
    SOCKST-LISTENING _SLSN-SD @ SOCK.STATE !
    0
;

\ --- ACCEPT ( sd -- new-sd | -1 ) ---
\ Poll for incoming connection on a listening socket.
\ Returns a new socket descriptor for the accepted connection.
VARIABLE _SACC-SD
VARIABLE _SACC-TCB

: ACCEPT ( sd -- new-sd | -1 )
    _SACC-SD !
    _SACC-SD @ SOCK.STATE @ SOCKST-LISTENING <> IF -1 EXIT THEN
    \ Check if the listening TCB has transitioned to ESTABLISHED
    _SACC-SD @ SOCK.HANDLE @ TCB.STATE @ TCPS-ESTABLISHED <> IF -1 EXIT THEN
    _SACC-SD @ SOCK.HANDLE @ _SACC-TCB !
    \ Allocate new socket for the accepted connection
    _SACC-SD @ SOCK.FLAGS @ 1 AND SOCKET   \ same type (TCP/TLS)
    DUP -1 = IF EXIT THEN
    DUP >R
    _SACC-TCB @ R@ SOCK.HANDLE !
    SOCKST-ACCEPTED R@ SOCK.STATE !
    _SACC-SD @ SOCK.LOCAL-PORT @ R@ SOCK.LOCAL-PORT !
    \ Re-open listener for next connection
    _SACC-SD @ SOCK.LOCAL-PORT @ TCP-LISTEN
    DUP 0<> IF _SACC-SD @ SOCK.HANDLE ! THEN
    DROP
    R>
;

\ --- CONNECT ( sd rip rport -- ior ) ---
\ Active open: TCP connect (+ TLS handshake if TLS socket).
VARIABLE _SCON-SD
VARIABLE _SCON-RIP
VARIABLE _SCON-RPORT

: CONNECT ( sd rip rport -- ior )
    _SCON-RPORT !  _SCON-RIP !  _SCON-SD !
    _SCON-SD @ SOCK.FLAGS @ 1 AND IF
        \ TLS socket
        _SCON-RIP @  _SCON-RPORT @  _SCON-SD @ SOCK.LOCAL-PORT @
        TLS-CONNECT
        DUP 0= IF DROP -1 EXIT THEN
        _SCON-SD @ SOCK.HANDLE !
        SOCKST-TLS _SCON-SD @ SOCK.STATE !
        0
    ELSE
        \ Plain TCP
        _SCON-RIP @  _SCON-RPORT @  _SCON-SD @ SOCK.LOCAL-PORT @
        TCP-CONNECT
        DUP 0= IF DROP -1 EXIT THEN
        _SCON-SD @ SOCK.HANDLE !
        50 TCP-POLL-WAIT
        _SCON-SD @ SOCK.HANDLE @ TCB.STATE @ TCPS-ESTABLISHED = IF
            SOCKST-TCP _SCON-SD @ SOCK.STATE !
            0
        ELSE -1 THEN
    THEN
;

\ --- SEND ( sd addr len -- actual ) ---
: SEND ( sd addr len -- actual )
    ROT DUP SOCK.STATE @ CASE
        SOCKST-TCP OF SOCK.HANDLE @  -ROT  TCP-SEND  ENDOF
        SOCKST-TLS OF SOCK.HANDLE @  -ROT  TLS-SEND  ENDOF
        SOCKST-ACCEPTED OF SOCK.HANDLE @  -ROT  TCP-SEND  ENDOF
        >R 2DROP DROP 0 R>
    ENDCASE
;

\ --- RECV ( sd addr maxlen -- actual ) ---
: RECV ( sd addr maxlen -- actual )
    ROT DUP SOCK.STATE @ CASE
        SOCKST-TCP OF SOCK.HANDLE @  -ROT  TCP-RECV  ENDOF
        SOCKST-TLS OF SOCK.HANDLE @  -ROT  TLS-RECV  ENDOF
        SOCKST-ACCEPTED OF SOCK.HANDLE @  -ROT  TCP-RECV  ENDOF
        >R 2DROP DROP 0 R>
    ENDCASE
;

\ --- CLOSE ( sd -- ) ---
: CLOSE ( sd -- )
    DUP SOCK.HANDLE @ 0<> IF
        DUP SOCK.STATE @ CASE
            SOCKST-TCP     OF DUP SOCK.HANDLE @ TCP-CLOSE  ENDOF
            SOCKST-TLS     OF DUP SOCK.HANDLE @ TLS-CLOSE  ENDOF
            SOCKST-ACCEPTED OF DUP SOCK.HANDLE @ TCP-CLOSE ENDOF
            SOCKST-LISTENING OF DUP SOCK.HANDLE @ TCB-INIT ENDOF
        ENDCASE
    THEN
    /SOCK 0 FILL   \ reset slot to FREE
;

\ SOCKET-READY? ( sd -- flag )  Non-blocking readiness check.
\ Returns -1 if data is available to read, 0 otherwise.
: SOCKET-READY?  ( sd -- flag )
    SOCK-N
    DUP SOCK.STATE @
    DUP SOCKST-TCP = OVER SOCKST-ACCEPTED = OR IF
        DROP  SOCK.HANDLE @
        DUP 0<> IF TCB.RX-COUNT @ 0>
        ELSE DROP 0 THEN
    ELSE
        2DROP 0
    THEN ;

\ .SOCKET ( sd -- )  Print socket status.
: .SOCKET ( sd -- )
    DUP SOCK.STATE @
    DUP SOCKST-FREE = IF DROP ."  socket: free" CR ELSE
    DUP SOCKST-TCP  = IF DROP ."  socket: TCP connected" CR ELSE
    DUP SOCKST-TLS  = IF DROP ."  socket: TLS connected" CR ELSE
    DUP SOCKST-LISTENING = IF DROP ."  socket: listening" CR ELSE
    DUP SOCKST-ACCEPTED  = IF DROP ."  socket: accepted" CR ELSE
    DROP ."  socket: unknown" CR
    THEN THEN THEN THEN THEN
    DROP
;

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

4 CONSTANT RING-LOCK

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

5 CONSTANT HT-LOCK

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
\  REQUIRE / PROVIDED prevent duplicate loading of Forth source files.
\  Each module should call  PROVIDED <name>  at the top to register
\  itself.  REQUIRE <name>  checks the registry; if already loaded it
\  is a no-op, otherwise it LOADs the file and marks it loaded.
\
\  Uses a hash table (§19) for O(1) lookup.  16-byte key = filename
\  (zero-padded, matching NAMEBUF layout), 1-byte value.

16 1 32 HASHTABLE _MOD-HT

\ Scratch for the 1-byte "loaded" marker value.
CREATE _MOD-VAL  1 ALLOT
1 _MOD-VAL C!

\ _MOD-MARK ( -- )  Mark NAMEBUF as a loaded module in _MOD-HT.
: _MOD-MARK  ( -- )
    NAMEBUF _MOD-VAL _MOD-HT HT-PUT ;

\ _MOD-LOADED? ( -- flag )  True if current NAMEBUF is in _MOD-HT.
: _MOD-LOADED?  ( -- flag )
    NAMEBUF _MOD-HT HT-GET 0<> ;

\ PROVIDED ( "name" -- )  Register a module as loaded.
\   Typically called at the top of a module file.
: PROVIDED  ( "name" -- )
    PARSE-NAME  _MOD-MARK ;

\ MODULE? ( "name" -- flag )  Test if a module is already loaded.
: MODULE?  ( "name" -- flag )
    PARSE-NAME  _MOD-LOADED? ;

\ _MOD-LOAD-BODY ( -- )  Load file whose name is already in NAMEBUF.
\   This is the core of LOAD without the PARSE-NAME call.
: _MOD-LOAD-BODY  ( -- )
    FS-ENSURE
    FS-OK @ 0= IF ."  No filesystem" CR EXIT THEN
    FIND-BY-NAME DUP -1 = IF
        DROP ."  Module not found: " NAMEBUF .ZSTR CR EXIT
    THEN
    DIRENT
    DUP DE.USED DUP 0= IF
        2DROP ."  Empty module" CR EXIT
    THEN
    _LD-SAVE
    LD-SZ !
    DUP 16 + W@ SWAP DE.COUNT
    \ File buffer in ext mem when available (see §1.12a)
    XMEM? IF
        LD-SZ @ XMEM-ALLOT LD-BUF !
    ELSE
        HERE LD-BUF !
        LD-SZ @ ALLOT
    THEN
    OVER DISK-SEC!
    LD-BUF @ DISK-DMA!
    DUP DISK-N!
    DISK-READ
    2DROP
    _LD-WALK
    _LD-RESTORE ;

\ REQUIRE ( "name" -- )  Load a module if not already loaded.
: REQUIRE  ( "name" -- )
    PARSE-NAME
    _MOD-LOADED? IF EXIT THEN
    _MOD-MARK
    _MOD-LOAD-BODY ;

\ _MOD-SHOW ( key-addr val-addr -- )  Print one module name.
: _MOD-SHOW  ( key-addr val-addr -- )
    DROP ."   " .ZSTR CR ;

\ MODULES ( -- )  List all loaded modules.
: MODULES  ( -- )
    ."  Loaded modules:" CR
    ['] _MOD-SHOW _MOD-HT HT-EACH
    _MOD-HT HT-COUNT . ."  module(s)" CR ;

\ =====================================================================
\  §10.1  Data Port Transport — UDP-Based Send/Receive
\ =====================================================================
\
\  Data ports ride on the §16 network stack via UDP port 9000.
\  Inbound: UDP frames on port 9000 are dispatched to _PORT-RX-HANDLER,
\           which copies the §10 payload into FRAME-BUF and routes to
\           the bound buffer via the port table.
\  Outbound: PORT-SEND wraps buffer data in a §10 frame header and
\            sends via UDP-SEND to PORT-DST-IP on port 9000.
\
\  PORT-INIT must be called at boot (done in §14 Startup).

\ -- Transport constants --
9000 CONSTANT PORT-UDP       \ well-known UDP port for data ports

\ -- Outbound destination (configurable, default 10.0.0.2) --
CREATE PORT-DST-IP  4 ALLOT
CREATE _PORT-BCAST-MAC  6 ALLOT
_PORT-BCAST-MAC 6 255 FILL   \ default: broadcast MAC

\ -- TX stats and buffer --
VARIABLE PORT-TX        0 PORT-TX !
VARIABLE TX-SEQ         0 TX-SEQ !
VARIABLE TX-FRAME-BUF   1499 ALLOT

\ -- Build 6-byte frame header in TX-FRAME-BUF --
VARIABLE _PBH-ID
VARIABLE _PBH-DT
VARIABLE _PBH-LEN
: _PORT-BUILD-HDR  ( id dtype paylen -- )
    _PBH-LEN !  _PBH-DT !  _PBH-ID !
    _PBH-ID @ TX-FRAME-BUF C!
    _PBH-DT @ TX-FRAME-BUF 1+ C!
    TX-SEQ @ DUP 255 AND TX-FRAME-BUF 2 + C!
    8 RSHIFT TX-FRAME-BUF 3 + C!
    _PBH-LEN @ DUP 255 AND TX-FRAME-BUF 4 + C!
    8 RSHIFT TX-FRAME-BUF 5 + C! ;

\ -- Map buffer type -> frame DTYPE --
: _PORT-BUF-DTYPE  ( buf -- dtype )
    B.TYPE
    CASE
        0 OF 0 ENDOF
        1 OF 1 ENDOF
        2 OF 2 ENDOF
        3 OF 0 ENDOF
        SWAP DROP 0 SWAP
    ENDCASE ;

\ -- PORT-SEND ( buf id -- )  send buffer data via UDP --
VARIABLE _PS-BUF
VARIABLE _PS-ID
: PORT-SEND  ( buf id -- )
    _PS-ID !  _PS-BUF !
    _PS-BUF @ B.BYTES 1400 MIN   \ leave room for UDP+IP+ETH headers
    DUP 0= IF DROP EXIT THEN
    >R
    _PS-ID @  _PS-BUF @ _PORT-BUF-DTYPE  R@  _PORT-BUILD-HDR
    _PS-BUF @ B.DATA  TX-FRAME-BUF /FRAME-HDR +  R@ CMOVE
    PORT-DST-IP PORT-UDP PORT-UDP
    TX-FRAME-BUF  R> /FRAME-HDR +
    UDP-SEND DROP
    1 PORT-TX +!
    1 TX-SEQ +! ;

\ -- PORT-SEND-SLICE ( buf off len id -- )  send sub-range via UDP --
VARIABLE _PSS-BUF
VARIABLE _PSS-OFF
VARIABLE _PSS-LEN
VARIABLE _PSS-ID
: PORT-SEND-SLICE  ( buf off len id -- )
    _PSS-ID !  _PSS-LEN !  _PSS-OFF !  _PSS-BUF !
    _PSS-LEN @ 1400 MIN  _PSS-LEN !
    _PSS-LEN @ 0= IF EXIT THEN
    \ Clamp offset + len to buffer bounds
    _PSS-OFF @ _PSS-LEN @ +  _PSS-BUF @ B.BYTES > IF
        _PSS-BUF @ B.BYTES _PSS-OFF @ -  0 MAX  _PSS-LEN !
    THEN
    _PSS-LEN @ 0= IF EXIT THEN
    _PSS-ID @  _PSS-BUF @ _PORT-BUF-DTYPE  _PSS-LEN @  _PORT-BUILD-HDR
    _PSS-BUF @ B.DATA _PSS-OFF @ +
    TX-FRAME-BUF /FRAME-HDR +  _PSS-LEN @ CMOVE
    PORT-DST-IP PORT-UDP PORT-UDP
    TX-FRAME-BUF  _PSS-LEN @ /FRAME-HDR +
    UDP-SEND DROP
    1 PORT-TX +!
    1 TX-SEQ +! ;

\ -- PORT-DST-SET ( b0 b1 b2 b3 -- )  change outbound destination IP --
: PORT-DST-SET  ( b0 b1 b2 b3 -- )  PORT-DST-IP IP! ;

\ -- Inbound reception via UDP-DISPATCH handler --
VARIABLE _POLL-RESULT   -1 _POLL-RESULT !

: _PORT-RX-HANDLER  ( src-ip sport data dlen -- )
    >R >R DROP DROP        \ R: dlen data ; drop src-ip sport
    R> FRAME-BUF R> CMOVE  \ copy §10 frame into FRAME-BUF
    FRAME-SRC PORT@ DUP 0= IF
        DROP 1 PORT-DROP +!  -1 _POLL-RESULT !
    ELSE
        ROUTE-BUF !
        FRAME-DATA  ROUTE-BUF @ B.DATA
        FRAME-LEN  ROUTE-BUF @ B.BYTES  MIN
        CMOVE
        1 PORT-RX +!
        FRAME-SRC _POLL-RESULT !
    THEN ;

\ -- RECV-FRAME ( -- flag )  receive one §10 frame from UDP --
: RECV-FRAME  ( -- flag )
    -1 _POLL-RESULT !
    UDP-DISPATCH DROP
    _POLL-RESULT @ -1 <> ;

\ -- ROUTE-FRAME ( -- id|-1 )  receive and route one frame --
: ROUTE-FRAME  ( -- id|-1 )
    -1 _POLL-RESULT !
    UDP-DISPATCH DROP
    _POLL-RESULT @ ;

\ -- POLL ( -- id|-1 )  receive and route one data port frame --
: POLL  ( -- id|-1 )  ROUTE-FRAME ;

\ -- INGEST ( n -- received )  poll for up to n frames --
: INGEST  ( n -- received )
    0 SWAP
    0 DO POLL -1 <> IF 1 + THEN LOOP ;

\ -- .PORT-STATS ( -- )  port stats including TX counter --
: .PORT-STATS  ( -- )
    ."  ports=" PORT-COUNT @ .
    ."  rx=" PORT-RX @ .
    ."  tx=" PORT-TX @ .
    ."  drop=" PORT-DROP @ . ;

\ -- PORT-INIT ( -- )  bind UDP port and seed ARP for default dest --
: PORT-INIT  ( -- )
    10 0 0 2 PORT-DST-IP IP!
    PORT-DST-IP _PORT-BCAST-MAC ARP-INSERT
    PORT-UDP ['] _PORT-RX-HANDLER UDP-PORT-BIND DROP ;

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
MAC-INIT
10 0 0 1 IP-SET
PORT-INIT
DISK? IF FS-LOAD THEN

\ -- AUTOEXEC: run autoexec.f if present on disk --
FS-OK @ IF
    S" autoexec.f" DROP NAMEBUF 10 CMOVE
    NAMEBUF 10 + 6 0 FILL
    FIND-BY-NAME -1 <> IF
        ."  Running autoexec.f..." CR
        LOAD autoexec.f
    THEN
THEN
CR
