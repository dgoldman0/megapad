
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
