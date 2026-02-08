\ =====================================================================
\  KDOS v0.9c — Kernel Dashboard OS for Megapad-64
\ =====================================================================
\
\  Loaded via UART into the Megapad-64 BIOS v0.5+ Forth system.
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
\    8. Scheduler       — cooperative & preemptive multitasking
\    9. Screens         — interactive TUI (ANSI terminal, 7 screens)
\   10. Data Ports      — NIC-based external data ingestion
\   11. Benchmarking    — BENCH for timing via CYCLES
\   12. Dashboard       — text-mode system overview
\   13. Help            — online reference for all KDOS words
\   14. Startup

\ =====================================================================
\  §1  Utility Words
\ =====================================================================
\  Note: BIOS v0.5 now provides CELLS, CELL+, 2*, <>, 0<>, MIN, MAX,
\  -ROT, +!, CMOVE, ?DUP, ABS, NEGATE, 2DROP, NIP, TUCK, BL, TRUE,
\  FALSE, EXIT, >R, R>, R@, J, UNLOOP, +LOOP, AGAIN, STATE, [, ],
\  LITERAL, IMMEDIATE, CREATE, S", 0>.

: OFF     ( addr -- )        0 SWAP ! ;

\ Comparison words not in BIOS
: >=  ( a b -- flag )  < 0= ;
: <=  ( a b -- flag )  > 0= ;

\ Tile-align HERE: pad with zero bytes to next 64-byte boundary
: TALIGN  ( -- )  BEGIN HERE 63 AND WHILE 0 C, REPEAT ;

\ .R ( n width -- ) print number right-justified in width
: .R  ( just use . for now )
    DROP . ;

\ 16-bit LE fetch/store (needed for frame header parsing)
: W@  ( addr -- u16 )  DUP C@ SWAP 1+ C@ 8 LSHIFT OR ;
: W!  ( u16 addr -- )  2DUP C! SWAP 8 RSHIFT SWAP 1+ C! ;

\ 32-bit LE fetch/store
: L@  ( addr -- u32 )  DUP W@ SWAP 2 + W@ 16 LSHIFT OR ;
: L!  ( u32 addr -- )  2DUP W! SWAP 16 RSHIFT SWAP 2 + W! ;

\ -- String utilities (needed by MP64FS file system) --

\ Print a null-terminated string
: .ZSTR  ( addr -- )
    BEGIN DUP C@ ?DUP WHILE EMIT 1+ REPEAT DROP ;

\ SAMESTR? ( addr1 addr2 maxlen -- flag )
\   Compare two null-terminated byte strings up to maxlen characters.
\   Returns -1 if equal, 0 if different.
VARIABLE STR-A
VARIABLE STR-B
VARIABLE STR-N

: SAMESTR?  ( a1 a2 maxlen -- flag )
    STR-N !  STR-B !  STR-A !
    BEGIN
        STR-N @ 0>
    WHILE
        STR-A @ C@  STR-B @ C@    ( c1 c2 )
        2DUP <> IF
            2DROP 0 EXIT           \ different
        THEN
        OR 0= IF
            -1 EXIT                \ both null -> match
        THEN
        1 STR-A +!  1 STR-B +!
        -1 STR-N +!
    REPEAT
    -1 ;

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
    DUP B.DATA mavg-scratch B.DATA
    OVER B.BYTES CMOVE
    DUP B.BYTES MAVG-NBYTES !
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
VARIABLE FF-RESULT

: FIND-FREE  ( count -- sector | -1 )
    FF-NEED !
    FS-DATA-START FF-START !
    0 FF-LEN !
    -1 FF-RESULT !
    2048 FS-DATA-START DO
        FF-RESULT @ -1 = IF
            I BIT-FREE? IF
                FF-LEN @ 0= IF I FF-START ! THEN
                1 FF-LEN +!
                FF-LEN @ FF-NEED @ >= IF
                    FF-START @ FF-RESULT !
                THEN
            ELSE
                0 FF-LEN !
            THEN
        THEN
    LOOP
    FF-RESULT @ ;

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
VARIABLE FFS-RESULT

: FIND-FREE-SLOT  ( -- slot | -1 )
    -1 FFS-RESULT !
    FS-MAX-FILES 0 DO
        FFS-RESULT @ -1 = IF
            I DIRENT C@ 0= IF I FFS-RESULT ! THEN
        THEN
    LOOP
    FFS-RESULT @ ;

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
            ."   type=" I DIRENT DE.TYPE . CR
        THEN
    LOOP
    ." (" . ." files)" CR ;

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

\ ── MKFILE — create a new file ───────────────────────────────────────

VARIABLE MK-NSEC
VARIABLE MK-TYPE
VARIABLE MK-SLOT
VARIABLE MK-START
VARIABLE MK-DUP

: MKFILE  ( nsectors type "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR 2DROP EXIT THEN
    MK-TYPE !  MK-NSEC !
    PARSE-NAME
    \ Check for duplicate name
    0 MK-DUP !
    FS-MAX-FILES 0 DO
        MK-DUP @ 0= IF
            I DIRENT C@ 0<> IF
                I DIRENT NAMEBUF 16 SAMESTR? IF
                    -1 MK-DUP !
                THEN
            THEN
        THEN
    LOOP
    MK-DUP @ IF
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
    DUP FS-ENTRY-SIZE 0 FILL        ( de )  zero slot
    DUP NAMEBUF SWAP 16 CMOVE       ( de )  copy name
    DUP MK-START @ SWAP 16 + W!     ( de )  start sector
    DUP MK-NSEC  @ SWAP 18 + W!     ( de )  sector count
    DUP 0          SWAP 20 + L!     ( de )  used_bytes = 0
    MK-TYPE  @ SWAP 24 + C!         ( )    type
    FS-SYNC
    ." Created: " NAMEBUF .ZSTR
    ." (" MK-NSEC @ . ." sectors at " MK-START @ . ." )" CR ;

\ ── RMFILE — delete a file ───────────────────────────────────────────

VARIABLE RM-SLOT

: RMFILE  ( "name" -- )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    PARSE-NAME
    -1 RM-SLOT !
    FS-MAX-FILES 0 DO
        RM-SLOT @ -1 = IF
            I DIRENT C@ 0<> IF
                I DIRENT NAMEBUF 16 SAMESTR? IF
                    I RM-SLOT !
                THEN
            THEN
        THEN
    LOOP
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

\ ── OPEN — open a file by name ───────────────────────────────────────

VARIABLE OP-SLOT

: OPEN  ( "name" -- fdesc | 0 )
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR 0 EXIT THEN
    PARSE-NAME
    -1 OP-SLOT !
    FS-MAX-FILES 0 DO
        OP-SLOT @ -1 = IF
            I DIRENT C@ 0<> IF
                I DIRENT NAMEBUF 16 SAMESTR? IF
                    I OP-SLOT !
                THEN
            THEN
        THEN
    LOOP
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
VARIABLE DS-SLOT
: DESCRIBE  ( "word" -- )
    PARSE-NAME PN-LEN @ 0= IF ." Usage: DESCRIBE <word>" CR EXIT THEN
    FS-ENSURE
    FS-OK @ 0= IF ." No filesystem" CR EXIT THEN
    \ Search for a doc file whose name matches NAMEBUF
    -1 DS-SLOT !
    FS-MAX-FILES 0 DO
        DS-SLOT @ -1 = IF
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE FTYPE-DOC = IF
                    I DIRENT NAMEBUF 16 SAMESTR? IF
                        I DS-SLOT !
                    THEN
                THEN
            THEN
        THEN
    LOOP
    DS-SLOT @ -1 = IF
        ." No doc for: " NAMEBUF .ZSTR CR
        ." Use TOPICS to list available documentation." CR
        EXIT
    THEN
    DS-SLOT @ OPEN-BY-SLOT DUP 0= IF EXIT THEN
    CR SHOW-FILE CR ;

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
    ."  KDOS v0.9c "
    RESET-COLOR
    SPACE
    SCREEN-ID @ DUP 1 = IF REVERSE THEN ." [1]Home " RESET-COLOR
    DUP 2 = IF REVERSE THEN ." [2]Bufs " RESET-COLOR
    DUP 3 = IF REVERSE THEN ." [3]Kern " RESET-COLOR
    DUP 4 = IF REVERSE THEN ." [4]Pipe " RESET-COLOR
    DUP 5 = IF REVERSE THEN ." [5]Task " RESET-COLOR
    DUP 6 = IF REVERSE THEN ." [6]Help " RESET-COLOR
    7 = IF REVERSE THEN ." [7]Docs " RESET-COLOR
    CR HBAR ;

\ -- Screen footer --
: SCREEN-FOOTER  ( -- )
    DIM
    ."  [1-7] Switch screen  [r] Refresh  [q] Quit"
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
    ."   buf knorm/khistogram  Advanced kernels" CR
    ."   th src dst kpeak      Peak detection" CR
    BOLD ."  Pipelines:" RESET-COLOR CR
    ."   3 PIPELINE name      Create pipeline" CR
    ."   ' w pipe P.ADD/RUN   Build & execute" CR
    BOLD ."  Tasks:" RESET-COLOR CR
    ."   ' w 0 TASK name      Create task" CR
    ."   SCHEDULE / BG         Run tasks" CR
    BOLD ."  Storage:" RESET-COLOR CR
    ."   buf sec B.SAVE/LOAD  Persist buffers" CR
    ."   0 16 FILE name       Create file" CR
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
    BOLD ."  Topics:" RESET-COLOR CR
    FS-OK @ IF
        0
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE FTYPE-DOC = IF
                    1+
                    ."    " I DIRENT .ZSTR CR
                THEN
            THEN
        LOOP
        DUP 0= IF ."    (none)" CR THEN DROP
    ELSE
        ."    (no filesystem loaded)" CR
    THEN
    CR
    BOLD ."  Tutorials:" RESET-COLOR CR
    FS-OK @ IF
        0
        FS-MAX-FILES 0 DO
            I DIRENT C@ 0<> IF
                I DIRENT DE.TYPE FTYPE-TUT = IF
                    1+
                    ."    " I DIRENT .ZSTR CR
                THEN
            THEN
        LOOP
        DUP 0= IF ."    (none)" CR THEN DROP
    ELSE
        ."    (no filesystem loaded)" CR
    THEN
    CR
    BOLD ."  Commands:" RESET-COLOR CR
    ."    DOC <topic>        Read documentation" CR
    ."    DESCRIBE <topic>   Show topic by name" CR
    ."    TUTORIAL <name>    Interactive lesson" CR
    ."    TOPICS             List all topics" CR
    ."    LESSONS            List all lessons" CR ;

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
        DROP SCR-HOME
    THEN THEN THEN THEN THEN THEN THEN
    CR SCREEN-FOOTER ;

\ -- Event loop: poll KEY?, dispatch on keypress --
: HANDLE-KEY  ( c -- )
    DUP 49 = IF DROP 1 SCREEN-ID ! RENDER-SCREEN ELSE  \ '1'
    DUP 50 = IF DROP 2 SCREEN-ID ! RENDER-SCREEN ELSE  \ '2'
    DUP 51 = IF DROP 3 SCREEN-ID ! RENDER-SCREEN ELSE  \ '3'
    DUP 52 = IF DROP 4 SCREEN-ID ! RENDER-SCREEN ELSE  \ '4'
    DUP 53 = IF DROP 5 SCREEN-ID ! RENDER-SCREEN ELSE  \ '5'
    DUP 54 = IF DROP 6 SCREEN-ID ! RENDER-SCREEN ELSE  \ '6'
    DUP 55 = IF DROP 7 SCREEN-ID ! RENDER-SCREEN ELSE  \ '7'
    DUP 113 = IF DROP 0 SCREEN-RUN !               ELSE  \ 'q'
    DUP 114 = IF DROP RENDER-SCREEN                ELSE  \ 'r'
        DROP
    THEN THEN THEN THEN THEN THEN THEN THEN THEN ;

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

\ -- Port registry count --
VARIABLE PORT-COUNT
0 PORT-COUNT !

\ -- Stats --
VARIABLE PORT-RX       \ total frames routed
0 PORT-RX !
VARIABLE PORT-DROP     \ frames dropped (unbound source)
0 PORT-DROP !

\ -- Temp for routing --
VARIABLE ROUTE-BUF

\ -- Port binding --
: PORT-SLOT  ( id -- addr )     CELLS PORT-TABLE + ;
: PORT!      ( buf id -- )      DUP PORT-SLOT @ 0= IF 1 PORT-COUNT +! THEN
                                PORT-SLOT ! ;
: PORT@      ( id -- buf|0 )    PORT-SLOT @ ;
: UNPORT     ( id -- )          DUP PORT@ 0<> IF -1 PORT-COUNT +! THEN
                                0 SWAP PORT-SLOT ! ;

\ -- NIC convenience --
: NET-RX?  ( -- flag )   NET-STATUS 2 AND 0<> ;

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
    ."  KDOS v0.9c — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR DISK-INFO
    CR BUFFERS
    CR KERNELS
    CR PIPES
    CR TASKS
    CR FILES
    CR PORTS
    CR HRULE ;

\ -- Status: quick one-liner --
: STATUS ( -- )
    ." KDOS v0.9c | bufs=" BUF-COUNT @ .
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

: HELP  ( -- )
    CR HRULE
    ."  KDOS v0.9c — Quick Reference" CR
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
    ."    DIR                    List files on disk" CR
    ."    CATALOG                Detailed file listing" CR
    ."    8 2 MKFILE name        Create file (8 secs, type 2)" CR
    ."    RMFILE name            Delete file from disk" CR
    ."    OPEN name              Open file -> fdesc" CR
    ."    f FFLUSH               Write metadata to disk" CR
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
    CR ."  DATA PORT WORDS:" CR
    ."    buf id PORT!           Bind source id to buffer" CR
    ."    id UNPORT              Unbind source" CR
    ."    POLL                   Receive & route one frame" CR
    ."    n INGEST               Receive & route n frames" CR
    ."    NET-RX?                Is a NIC frame waiting?" CR
    ."    PORTS                  List port bindings" CR
    ."    .FRAME                 Show last frame header" CR
    CR ."  SCREENS & TOOLS:" CR
    ."    SCREENS                Interactive TUI (1-7, q, r)" CR
    ."    DASHBOARD              Full system overview" CR
    ."    STATUS                 Quick status line" CR
    ."    ' word BENCH           Time word, leave cycles on stack" CR
    ."    ' word .BENCH          Time word and print cycles" CR
    ."    HELP                   This help" CR
    CR ."  DOCUMENTATION:" CR
    ."    TOPICS                 List available doc topics" CR
    ."    LESSONS                List available tutorials" CR
    ."    DOC <topic>            Page through documentation" CR
    ."    DESCRIBE <topic>       Show topic by name" CR
    ."    TUTORIAL <name>        Interactive lesson" CR
    CR HRULE ;

\ =====================================================================
\  §14  Startup
\ =====================================================================

CR HRULE
."  KDOS v0.9c — Kernel Dashboard OS" CR
HRULE
." Type HELP for command reference." CR
." Type SCREENS for interactive TUI." CR
." Type TOPICS or LESSONS for documentation." CR
DISK? IF FS-LOAD THEN
CR
