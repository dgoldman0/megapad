\ =====================================================================
\  KDOS v0.6 — Kernel Dashboard OS for Megapad-64
\ =====================================================================
\
\  Loaded via UART into the Megapad-64 BIOS v0.4+ Forth system.
\
\  Subsystems:
\    1. Utilities  — common words, tile-alignment helpers
\    2. Buffers    — typed, tile-aligned data regions with descriptors
\    3. Tile Ops   — buffer-level tile engine operations (SUM, ADD, etc.)
\    4. Kernels    — metadata registry for named compute kernels
\    5. Sample Kernels — kzero, kfill, kadd, kscale, kstats, kthresh
\    6. Pipelines      — kernel pipeline engine with demo pipelines
\    7. Storage        — disk persistence for buffers, file abstraction
\    8. Benchmarking   — BENCH for timing via CYCLES
\    9. Dashboard  — text-mode system overview via UART
\   10. Help       — online reference for all KDOS words

\ =====================================================================
\  §1  Utility Words
\ =====================================================================

: CELLS   ( n -- n*8 )       8 * ;
: CELL+   ( a -- a+8 )       8 + ;
: 2*      ( n -- n*2 )       DUP + ;
: <>      ( a b -- flag )    = 0= ;
: 0<>     ( n -- flag )      0= 0= ;
: MIN     ( a b -- min )     2DUP > IF SWAP THEN DROP ;
: MAX     ( a b -- max )     2DUP < IF SWAP THEN DROP ;
: OFF     ( addr -- )        0 SWAP ! ;
: ABS     ( n -- |n| )       DUP 0< IF 0 SWAP - THEN ;
: NEGATE  ( n -- -n )        0 SWAP - ;
: /       ( a b -- a/b )     /MOD SWAP DROP ;
: MOD     ( a b -- rem )     /MOD DROP ;
: SPACES  ( n -- )           DUP IF 0 DO SPACE LOOP ELSE DROP THEN ;
: 2DROP   ( a b -- )         DROP DROP ;
: NIP     ( a b -- b )       SWAP DROP ;
: TUCK    ( a b -- b a b )   SWAP OVER ;
: -ROT    ( a b c -- c a b ) ROT ROT ;

\ Tile-align HERE: pad with zero bytes to next 64-byte boundary
: TALIGN  ( -- )  BEGIN HERE 63 AND WHILE 0 C, REPEAT ;

\ .R ( n width -- ) print number right-justified in width
: .R  SWAP DUP ABS 0 ( width n abs carry )
    BEGIN SWAP 10 /MOD SWAP ROT 1+ DUP 0= UNTIL ( width n digits count )
    DROP DROP DROP
    ( just use . for now )
    DROP . ;

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

\ -- Registry (up to 16 kernels) --
VARIABLE KERN-COUNT
0 KERN-COUNT !
VARIABLE KERN-TABLE  15 CELLS ALLOT

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
    KERN-COUNT @ 16 < IF
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
\   Writes sector-by-sector using scratch buffer for partial sectors.
: FWRITE  ( addr len fdesc -- )
    -ROT                      ( fdesc addr len )
    2DUP + 2 PICK F.CURSOR + ( fdesc addr len new-end )
    3 PICK F.MAX SECTOR *     ( fdesc addr len new-end capacity )
    OVER < IF                 ( fdesc addr len new-end )
        ." FWRITE: out of space" CR
        2DROP 2DROP
    ELSE DROP                 ( fdesc addr len )
        \ Compute sector + offset within sector
        2 PICK F.CURSOR SECTOR /
        2 PICK F.START + DISK-SEC!  ( fdesc addr len -- set sector )
        2 PICK F.CURSOR SECTOR MOD  ( fdesc addr len offset )
        \ Simple approach: DMA whole sectors from addr if offset=0
        \ For now, byte-copy to scratch then DMA
        DUP 0= IF
            DROP              ( fdesc addr len )
            \ Aligned write: DMA directly from addr
            DUP SECTOR 1- + SECTOR / DUP DISK-N!
            DROP              ( fdesc addr len )
            OVER DISK-DMA!    ( fdesc addr len )
            DISK-WRITE
        ELSE
            \ Unaligned write: copy to scratch
            DROP              ( fdesc addr len )
            FSCRATCH SECTOR 0 FILL  ( zero scratch )
            FSCRATCH SWAP     ( fdesc addr scratch len )
            0 DO              ( fdesc addr scratch )
                OVER I + C@   ( fdesc addr scratch byte )
                OVER I + C!   ( fdesc addr scratch )
            LOOP
            NIP               ( fdesc scratch )
            DISK-DMA!         ( fdesc )
            1 DISK-N!
            DISK-WRITE
            DROP              ( )
        THEN
        \ Update cursor and used_bytes
        \ (re-fetch fdesc — it's 3rd from top before the THENs)
    THEN ;

\ Simpler FWRITE: always byte-copy to scratch per sector
\ (We'll use a cleaner implementation)

\ FREAD ( addr len fdesc -- actual ) read up to len bytes at cursor
: FREAD  ( addr len fdesc -- actual )
    DUP F.USED OVER F.CURSOR -  ( addr len fdesc avail )
    ROT MIN                   ( addr fdesc actual-len )
    DUP 0= IF
        NIP NIP               ( 0 )
    ELSE
        ROT ROT               ( actual addr fdesc )
        \ Set up DMA read at cursor's sector
        DUP F.CURSOR SECTOR / OVER F.START + DISK-SEC!
        OVER DISK-DMA!        ( actual addr fdesc )
        \ Read enough sectors
        2 PICK SECTOR 1- + SECTOR / DISK-N!
        DISK-READ             ( actual addr fdesc )
        DROP DROP             ( actual )
    THEN ;

\ F.INFO ( fdesc -- ) print file descriptor
: F.INFO  ( fdesc -- )
    ." [file"
    DUP ."  sec=" F.START .
    DUP ."  max=" F.MAX .
    DUP ."  used=" F.USED .
    ."  cur=" F.CURSOR . ." ]" CR ;

\ -- List all registered files --
: FILES  ( -- )
    ." --- Files (" FILE-COUNT @ . ." ) ---" CR
    FILE-COUNT @ DUP IF
        0 DO
            I . ." : "
            I CELLS FILE-TABLE + @ F.INFO
        LOOP
    ELSE DROP THEN ;

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
VARIABLE FOUND-TASK
: FIND-READY  ( -- tdesc | 0 )
    0 FOUND-TASK !
    TASK-COUNT @ DUP 0<> IF
        0 DO
            FOUND-TASK @ 0= IF
                I CELLS TASK-TABLE + @       ( tdesc )
                DUP T.STATUS T.READY = IF
                    FOUND-TASK !             ( -- store it )
                ELSE DROP THEN
            THEN
        LOOP
    ELSE DROP THEN
    FOUND-TASK @ ;

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
\  §9  Interactive Screens
\ =====================================================================
\
\  Full-screen TUI built on ANSI escape sequences.
\  Screens: [1]Home [2]Buffers [3]Kernels [4]Pipes [5]Tasks [6]Help
\  Navigation: number keys switch screens, q quits, r refreshes.
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
VARIABLE SCREEN-ID      \ current screen: 1-6
VARIABLE SCREEN-RUN     \ flag: 0 = exit loop

\ -- Screen header --
: SCREEN-HEADER  ( -- )
    1 1 AT-XY
    REVERSE
    ."  KDOS v0.6 "
    RESET-COLOR
    SPACE
    SCREEN-ID @ DUP 1 = IF REVERSE THEN ." [1]Home " RESET-COLOR
    DUP 2 = IF REVERSE THEN ." [2]Bufs " RESET-COLOR
    DUP 3 = IF REVERSE THEN ." [3]Kern " RESET-COLOR
    DUP 4 = IF REVERSE THEN ." [4]Pipe " RESET-COLOR
    DUP 5 = IF REVERSE THEN ." [5]Task " RESET-COLOR
    6 = IF REVERSE THEN ." [6]Help " RESET-COLOR
    CR HBAR ;

\ -- Screen footer --
: SCREEN-FOOTER  ( -- )
    DIM
    ."  [1-6] Switch screen  [r] Refresh  [q] Quit"
    RESET-COLOR CR ;

\ ---- Screen 1: Home ----
: SCR-HOME  ( -- )
    .LABEL ."  System Overview" ./LABEL CR CR
    ."   Memory  : HERE = " HERE . CR
    ."   Buffers : " BUF-COUNT @ .N CR
    ."   Kernels : " KERN-COUNT @ .N CR
    ."   Pipes   : " PIPE-COUNT @ .N CR
    ."   Tasks   : " TASK-COUNT @ .N CR
    ."   Files   : " FILE-COUNT @ .N CR
    ."   Storage : " DISK? IF 2 FG ." present" ELSE 1 FG ." not attached" THEN RESET-COLOR CR
    CR
    ."   Scheduler: " PREEMPT-ENABLED @ IF 2 FG ." preempt ON" ELSE DIM ." cooperative" THEN RESET-COLOR CR
    ."   Tasks rdy: " TASK-COUNT-READY .N CR ;

\ ---- Screen 2: Buffers ----
: SCR-BUFFERS  ( -- )
    .LABEL ."  Buffers (" BUF-COUNT @ .N ." )" ./LABEL CR CR
    BUF-COUNT @ DUP 0= IF
        DROP ."   (none registered)" CR
    ELSE
        0 DO
            ."   " I .N ."  "
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
    ELSE
        0 DO
            ."   " I .N ."  "
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
    BOLD ."  Pipelines:" RESET-COLOR CR
    ."   3 PIPELINE name      Create pipeline" CR
    ."   ' w pipe P.ADD/RUN   Build & execute" CR
    BOLD ."  Tasks:" RESET-COLOR CR
    ."   ' w 0 TASK name      Create task" CR
    ."   SCHEDULE / BG         Run tasks" CR
    BOLD ."  Storage:" RESET-COLOR CR
    ."   buf sec B.SAVE/LOAD  Persist buffers" CR
    ."   0 16 FILE name       Create file" CR
    BOLD ."  Tools:" RESET-COLOR CR
    ."   DASHBOARD / STATUS    System views" CR
    ."   ' w BENCH / .BENCH   Benchmark" CR ;

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
        DROP SCR-HOME
    THEN THEN THEN THEN THEN THEN
    CR SCREEN-FOOTER ;

\ -- Event loop: poll KEY?, dispatch on keypress --
: HANDLE-KEY  ( c -- )
    DUP 49 = IF DROP 1 SCREEN-ID ! RENDER-SCREEN ELSE  \ '1'
    DUP 50 = IF DROP 2 SCREEN-ID ! RENDER-SCREEN ELSE  \ '2'
    DUP 51 = IF DROP 3 SCREEN-ID ! RENDER-SCREEN ELSE  \ '3'
    DUP 52 = IF DROP 4 SCREEN-ID ! RENDER-SCREEN ELSE  \ '4'
    DUP 53 = IF DROP 5 SCREEN-ID ! RENDER-SCREEN ELSE  \ '5'
    DUP 54 = IF DROP 6 SCREEN-ID ! RENDER-SCREEN ELSE  \ '6'
    DUP 113 = IF DROP 0 SCREEN-RUN !               ELSE  \ 'q'
    DUP 114 = IF DROP RENDER-SCREEN                ELSE  \ 'r'
        DROP
    THEN THEN THEN THEN THEN THEN THEN THEN ;

\ -- Main TUI entry point --
: SCREENS  ( -- )
    1 SCREEN-ID !
    1 SCREEN-RUN !
    RENDER-SCREEN
    BEGIN
        KEY? IF KEY HANDLE-KEY THEN
        SCREEN-RUN @
    0= UNTIL
    PAGE
    ." Returned to REPL." CR ;

\ =====================================================================
\  §10  Benchmarking
\ =====================================================================
\
\  BENCH ( xt -- cycles )
\    Time the execution of a word (given as XT from ' or ['] ).
\    Returns the cycle count difference.

VARIABLE BENCH-T0

: BENCH  ( xt -- cycles )
    CYCLES BENCH-T0 !
    EXECUTE
    CYCLES BENCH-T0 @ - ;

\ .BENCH ( xt -- ) time and print result
: .BENCH  ( xt -- )
    BENCH
    ." cycles=" . CR ;

\ =====================================================================
\  §11  Dashboard
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
    ."  KDOS v0.6 — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR DISK-INFO
    CR BUFFERS
    CR KERNELS
    CR PIPES
    CR TASKS
    CR FILES
    CR HRULE ;

\ -- Status: quick one-liner --
: STATUS ( -- )
    ." KDOS | bufs=" BUF-COUNT @ .
    ." kerns=" KERN-COUNT @ .
    ." pipes=" PIPE-COUNT @ .
    ." tasks=" TASK-COUNT @ .
    ." files=" FILE-COUNT @ .
    ." disk=" DISK? IF ." yes" ELSE ." no" THEN
    ."  HERE=" HERE . CR ;

\ =====================================================================
\  §12  Help System
\ =====================================================================

: HELP  ( -- )
    CR HRULE
    ."  KDOS v0.6 — Quick Reference" CR
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
    CR ."  FILE WORDS:" CR
    ."    10 8 FILE name         Create file at sec 10, 8 secs" CR
    ."    f F.INFO               Show file descriptor" CR
    ."    addr len f FWRITE      Write bytes to file" CR
    ."    addr len f FREAD       Read bytes from file" CR
    ."    pos f FSEEK            Set file cursor" CR
    ."    f FREWIND              Reset cursor to 0" CR
    ."    f FSIZE                Get used bytes" CR
    ."    FILES                  List all files" CR
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
    CR ."  SCREENS & TOOLS:" CR
    ."    SCREENS                Interactive TUI (1-6, q, r)" CR
    ."    DASHBOARD              Full system overview" CR
    ."    STATUS                 Quick status line" CR
    ."    ' word BENCH           Time word, leave cycles on stack" CR
    ."    ' word .BENCH          Time word and print cycles" CR
    ."    HELP                   This help" CR
    CR HRULE ;

\ =====================================================================
\  §13  Startup
\ =====================================================================

CR HRULE
."  KDOS v0.6 — Kernel Dashboard OS" CR
HRULE
." Type HELP for command reference." CR
." Type SCREENS for interactive TUI." CR
CR
