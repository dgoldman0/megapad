\ =====================================================================
\  KDOS — Kernel Dashboard OS for Megapad-64
\ =====================================================================
\
\  Loaded via UART into the Megapad-64 BIOS v0.4 Forth system.
\
\  Subsystems:
\    1. Buffers  — typed, tile-aligned data regions with descriptors
\    2. Kernels  — metadata registry for named compute kernels
\    3. Dashboard — text-mode system overview via UART
\
\  Conventions:
\    - Tile = 64 bytes; all buffer data is tile-aligned
\    - Descriptors are 4-cell (32-byte) structs in dictionary space
\    - Registry tables hold up to 16 descriptor addresses each
\    - Kernels are plain Forth words; descriptors hold metadata only

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

\ Tile-align HERE: pad with zero bytes to next 64-byte boundary
: TALIGN  ( -- )  BEGIN HERE 63 AND WHILE 0 C, REPEAT ;

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
\  §3  Kernel Registry
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
\  §4  Dashboard
\ =====================================================================

: HRULE  ( -- )  60 0 DO 45 EMIT LOOP CR ;

: DASHBOARD ( -- )
    CR HRULE
    ."  KERNEL DASHBOARD OS  (KDOS)" CR
    HRULE
    ."  HERE = " HERE . CR
    CR BUFFERS
    CR KERNELS
    CR HRULE ;

\ =====================================================================
\  §5  Built-in Example: tile-zero kernel
\ =====================================================================

\ A trivial kernel that zeros a buffer
: kzero  ( buf-desc -- )  B.ZERO ;

\ Register its metadata (1 input, 1 output, 0 footprint, flags=0)
1 1 0 0 KERNEL kzero-desc

\ =====================================================================
\  §6  Startup
\ =====================================================================

CR HRULE
."  KERNEL DASHBOARD OS  (KDOS)" CR
HRULE
." Type DASHBOARD for system overview." CR
." Type BUFFERS or KERNELS for subsystem info." CR
CR
