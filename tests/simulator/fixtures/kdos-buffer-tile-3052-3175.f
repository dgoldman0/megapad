
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
