\ =====================================================================
\  KDOS v0.2 — Kernel Dashboard OS for Megapad-64
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
\    6. Benchmarking   — BENCH, K.BENCH for timing via CYCLES
\    7. Dashboard  — text-mode system overview via UART
\    8. Help       — online reference for all KDOS words

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
: SPACES  ( n -- )           0 DO SPACE LOOP ;
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
    BUF-COUNT @ 0 DO
        I . ." : "
        I CELLS BUF-TABLE + @ B.INFO
    LOOP ;

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
    KERN-COUNT @ 0 DO
        I . ." : "
        I CELLS KERN-TABLE + @ K.INFO
    LOOP ;

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
\  §6  Benchmarking
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
\  §7  Dashboard
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
    ."  KDOS v0.2 — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR BUFFERS
    CR KERNELS
    CR HRULE ;

\ -- Status: quick one-liner --
: STATUS ( -- )
    ." KDOS | bufs=" BUF-COUNT @ .
    ." kerns=" KERN-COUNT @ .
    ." HERE=" HERE . CR ;

\ =====================================================================
\  §8  Help System
\ =====================================================================

: HELP  ( -- )
    CR HRULE
    ."  KDOS v0.2 — Quick Reference" CR
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
    CR ."  BENCH & TOOLS:" CR
    ."    ' word BENCH           Time word, leave cycles on stack" CR
    ."    ' word .BENCH          Time word and print cycles" CR
    ."    DASHBOARD              Full system overview" CR
    ."    STATUS                 Quick status line" CR
    ."    HELP                   This help" CR
    CR HRULE ;

\ =====================================================================
\  §9  Startup
\ =====================================================================

CR HRULE
."  KDOS v0.2 — Kernel Dashboard OS" CR
HRULE
." Type HELP for command reference." CR
." Type DASHBOARD for system overview." CR
CR
