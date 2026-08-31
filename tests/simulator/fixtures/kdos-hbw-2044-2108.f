
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

\ HBW-ALLOT? ( u -- addr ior )  like HBW-ALLOT but returns ior
: HBW-ALLOT?  ( u -- addr ior )
    HBW-HERE @ SWAP
    OVER + DUP HBW-LIMIT @ > IF
        2DROP 0 -1 EXIT              \ overflow → (0, -1)
    THEN
    HBW-HERE ! 0 ;                   \ success  → (addr, 0)

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
