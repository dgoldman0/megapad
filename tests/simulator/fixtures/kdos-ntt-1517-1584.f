\ =====================================================================
\  §1.11  NTT Engine — 256-point Number Theoretic Transform
\ =====================================================================
\  Generic cyclic NTT service over x^256 - 1; this is not the standardized
\  negacyclic transform used by ML-KEM or ML-DSA.
\
\  BIOS primitives:
\    NTT-SETQ ( q -- )          Set modulus
\    NTT-IDX! ( idx -- )        Set coefficient index
\    NTT-LOAD ( addr buf -- )   Load 256 coefficients (buf: 0=A, 1=B)
\    NTT-STORE ( addr -- )      Store 256 result coefficients
\    NTT-FWD ( -- )             Forward NTT
\    NTT-INV ( -- )             Inverse NTT
\    NTT-PMUL ( -- )            Pointwise multiply A*B mod q
\    NTT-PADD ( -- )            Pointwise add (A+B) mod q
\    NTT-STATUS@ ( -- n )       Read status
\    NTT-WAIT ( -- )            Poll until done
\
\  Convenience:
\    NTT-POLYMUL ( a b r -- )   Full polynomial multiply via NTT
\      Load a→A, b→B, forward NTT both, pointwise multiply, inverse NTT,
\      store to r. Requires q already set.

\ Standard modulus constants
3329     CONSTANT Q-KYBER      \ ML-KEM (Kyber)
8380417  CONSTANT Q-DILITHIUM  \ ML-DSA (Dilithium)

\ NTT buffer selector constants
0 CONSTANT NTT-BUF-A
1 CONSTANT NTT-BUF-B

\ Internal temp buffers for NTT-POLYMUL (256 coefficients × 4 bytes)
CREATE _NTT-TMP-A 1024 ALLOT
CREATE _NTT-TMP-B 1024 ALLOT

\ NTT-POLYMUL ( a-addr b-addr r-addr -- )
\   Full polynomial multiply: r = INTT( NTT(a) · NTT(b) )
\   Modulus q must be set beforehand via NTT-SETQ.
: NTT-POLYMUL ( a-addr b-addr r-addr -- )
    >R >R                         \ ( a-addr ) R:( r-addr b-addr )
    \ Step 1: NTT(a) → _NTT-TMP-A
    NTT-BUF-A NTT-LOAD            \ load a → A
    NTT-FWD NTT-WAIT              \ forward NTT
    _NTT-TMP-A NTT-STORE          \ store NTT(a)
    \ Step 2: NTT(b) → _NTT-TMP-B
    R> NTT-BUF-A NTT-LOAD         \ load b → A
    NTT-FWD NTT-WAIT              \ forward NTT
    _NTT-TMP-B NTT-STORE          \ store NTT(b)
    \ Step 3: Pointwise multiply NTT(a) * NTT(b)
    _NTT-TMP-A NTT-BUF-A NTT-LOAD
    _NTT-TMP-B NTT-BUF-B NTT-LOAD
    NTT-PMUL NTT-WAIT             \ result = NTT(a) · NTT(b)
    \ Step 4: INTT → r
    \ To do INTT, we need result in A. Store result, reload to A.
    _NTT-TMP-A NTT-STORE          \ reuse TMP-A for product
    _NTT-TMP-A NTT-BUF-A NTT-LOAD
    NTT-INV NTT-WAIT              \ result = INTT(product)
    R> NTT-STORE                  \ store to r
;

\ .NTT-STATUS ( -- )  Print human-readable NTT status.
: .NTT-STATUS
    NTT-STATUS@
    DUP 0 = IF DROP ."  NTT: idle" CR ELSE
    DUP 2 = IF DROP ."  NTT: done" CR ELSE
    DUP 1 = IF DROP ."  NTT: busy" CR ELSE
    DROP ."  NTT: unknown" CR
    THEN THEN THEN ;
