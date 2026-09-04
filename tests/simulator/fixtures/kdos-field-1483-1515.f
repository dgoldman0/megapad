\ =====================================================================
\  §1.10  Field ALU — Multi-Prime Coprocessor + Raw 256x256 Multiply
\ =====================================================================
\  Per-core ISA instructions (EXT.CRYPTO FB 20-2D).
\
\  BIOS primitives:
\    GF-A!       ( addr -- )    Load 32 bytes → ACC0-ACC3
\    GF-R@       ( addr -- )    Store ACC0-ACC3 → 32 bytes
\    GF-PRIME    ( n -- )       Set prime: 0=25519, 1=secp, 2=P256, 3=custom
\    LOAD-PRIME  ( p pinv -- )  Latch custom prime + Montgomery p_inv
\    FADD        ( a b r -- )   (a + b) mod p
\    FSUB        ( a b r -- )   (a - b) mod p
\    FMUL        ( a b r -- )   (a * b) mod p
\    FSQR        ( a r -- )     a^2 mod p
\    FINV        ( a r -- )     a^(p-2) mod p
\    FPOW        ( a e r -- )   a^e mod p
\    FMUL-RAW    ( a b rlo rhi -- )  256×256 → 512 raw
\    FCMOV       ( a cond -- )  Constant-time conditional move
\    FCEQ        ( a b r -- )   Constant-time equality test
\    FMAC        ( a b r -- )   (a*b + prev) mod p
\    FMUL-ADD-RAW ( a b rlo rhi -- ) Raw 512-bit MAC

\ Prime selection
: PRIME-25519  ( -- ) 0 GF-PRIME ;
: PRIME-SECP   ( -- ) 1 GF-PRIME ;
: PRIME-P256   ( -- ) 2 GF-PRIME ;
: PRIME-CUSTOM ( -- ) 3 GF-PRIME ;

\ Scratch buffers for field ops (32 bytes each)
CREATE _FA  32 ALLOT
CREATE _FB  32 ALLOT
CREATE _FR  32 ALLOT
CREATE _FRH 32 ALLOT
