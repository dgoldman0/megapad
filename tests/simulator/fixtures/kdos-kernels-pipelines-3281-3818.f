
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
