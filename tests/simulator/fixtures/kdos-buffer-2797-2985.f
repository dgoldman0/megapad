
\ -- Registry (linked list — no slot limit) --
VARIABLE BUF-COUNT
0 BUF-COUNT !
VARIABLE BUF-HEAD     0 BUF-HEAD !   \ head of buffer linked list

\ (BUF-REG) ( desc -- )  register a descriptor in the linked list.
\   Allocates a 16-byte link node in the dictionary:
\     link+0 = desc addr, link+8 = old head.
: (BUF-REG)  ( desc -- )
    HERE SWAP         ( link-addr desc )
    ,                 ( link-addr ; link+0 = desc )
    BUF-HEAD @ ,      ( link-addr ; link+8 = old head )
    BUF-HEAD !        ( ; head = link-addr )
    1 BUF-COUNT +! ;

\ BUF-NTH ( n -- desc )  Return descriptor of nth registered buffer
\   (0-based).  Walks the linked list from BUF-HEAD.
: BUF-NTH  ( n -- desc )
    BUF-HEAD @          ( n node )
    SWAP 0 ?DO          ( node )
        8 + @           ( next-node )
    LOOP
    @                   ( desc ) ;

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
    BDESC @ (BUF-REG)
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
    BDESC @ (BUF-REG)
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
    BDESC @ (BUF-REG)
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
    0 BUF-HEAD @
    BEGIN DUP WHILE
        SWAP DUP . ."  : " SWAP        ( idx link )
        DUP @ B.INFO                     \ link+0 = desc addr
        SWAP 1+ SWAP
        8 + @                            \ link+8 = next link
    REPEAT
    2DROP ;

\ =====================================================================
\  §2.1  Arena–Buffer Integration
\ =====================================================================
\
\  Extends the arena allocator with buffer support.
\  Must come after §2 so BUF-HEAD / BUF-COUNT are available.

VARIABLE AB-AR      \ scratch: arena address
VARIABLE AB-DESC    \ scratch: descriptor address

\ (AR-UNREG-BUFS) ( base limit -- )
\   Walk BUF-HEAD linked list.  Unlink every node whose descriptor
\   address falls in [base, limit).  O(n) in registered buffers.
: (AR-UNREG-BUFS)  ( base limit -- )
    BUF-HEAD      ( base limit pp )      \ pp = pointer-to-pointer
    BEGIN  DUP @ 0<>  WHILE              ( base limit pp )
        DUP @                            ( base limit pp node )
        DUP @                            ( base limit pp node desc )
        4 PICK OVER > 0=                 ( base limit pp node desc base<=desc )
        SWAP 4 PICK <                    ( base limit pp node base<=desc desc<lim )
        AND IF
            \ unlink: *pp = node.next
            8 + @  OVER !               ( base limit pp )
            -1 BUF-COUNT +!
        ELSE
            \ keep: advance pp = &(node.next)
            NIP 8 +                     ( base limit pp' )
        THEN
    REPEAT
    DROP 2DROP ;

\ Redefine ARENA-DESTROY to also unregister arena-scoped buffers.
\   Core-0 only — calls (AR-FREE-BACKING) which uses shared state.
: ARENA-DESTROY  ( arena -- )
    ?CORE0
    DUP A.BASE @  OVER A.SIZE @ OVER + ( arena base limit )
    (AR-UNREG-BUFS)                     ( arena )
    DUP A.BASE @  OVER A.SIZE @  ROT DUP >R A.SOURCE @
    (AR-FREE-BACKING)
    R>
    0 OVER !  0 OVER 8 + !             \ zero base, size
    0 OVER 16 + !  0 SWAP 24 + ! ;     \ zero ptr, source

\ ARENA-BUFFER ( type width length arena "name" -- )
\   Like BUFFER, but both descriptor and data are arena-allocated.
\   Registered in BUF-HEAD; auto-unregistered by ARENA-DESTROY.
: ARENA-BUFFER  ( type width length arena "name" -- )
    AB-AR !                              ( type width length )
    AB-AR @ 32 ARENA-ALLOT AB-DESC !     ( type width length )
    \ Store length at +16
    DUP AB-DESC @ 16 + !                ( type width length )
    \ data-bytes = length * width
    OVER *                               ( type width data-bytes )
    \ Store width at +8
    SWAP AB-DESC @ 8 + !                 ( type data-bytes )
    \ Store type at +0
    SWAP AB-DESC @ !                     ( data-bytes )
    \ Allocate data from arena (8-byte aligned)
    7 + -8 AND
    AB-AR @ SWAP ARENA-ALLOT             ( data-addr )
    AB-DESC @ 24 + !                     ( )
    \ Register in buffer list and define constant
    AB-DESC @ (BUF-REG)
    AB-DESC @ CONSTANT ;
