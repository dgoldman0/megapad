\ =====================================================================
\  §1.12  ML-KEM-512 (Kyber) — Lattice-Based Key Encapsulation
\ =====================================================================
\ Uses KEM accelerator device for FIPS 203 ML-KEM-512.
\ BIOS primitives: KEM-SEL! KEM-LOAD KEM-STORE KEM-KEYGEN KEM-ENCAPS
\                  KEM-DECAPS KEM-STATUS@
\ Buffer IDs: 0=SEED/COIN(64B) 1=PK(800B) 2=SK(1632B) 3=CT(768B) 4=SS(32B)

0 CONSTANT KBUF-SEED
1 CONSTANT KBUF-PK
2 CONSTANT KBUF-SK
3 CONSTANT KBUF-CT
4 CONSTANT KBUF-SS

64  CONSTANT KEM-SEED-SIZE
800 CONSTANT KEM-PK-SIZE
1632 CONSTANT KEM-SK-SIZE
768 CONSTANT KEM-CT-SIZE
32  CONSTANT KEM-SS-SIZE

: KYBER-KEYGEN ( seed64-addr pk-addr sk-addr -- )
    >R >R
    KBUF-SEED KEM-SEL!  64 KEM-LOAD
    KEM-KEYGEN
    KBUF-PK KEM-SEL!  R> KEM-PK-SIZE KEM-STORE
    KBUF-SK KEM-SEL!  R> KEM-SK-SIZE KEM-STORE ;

: KYBER-ENCAPS ( pk-addr coin-addr ct-addr ss-addr -- )
    >R >R
    KBUF-SEED KEM-SEL!  32 KEM-LOAD
    KBUF-PK KEM-SEL!  KEM-PK-SIZE KEM-LOAD
    KEM-ENCAPS
    KBUF-CT KEM-SEL!  R> KEM-CT-SIZE KEM-STORE
    KBUF-SS KEM-SEL!  R> KEM-SS-SIZE KEM-STORE ;

: KYBER-DECAPS ( ct-addr sk-addr ss-addr -- )
    >R
    KBUF-SK KEM-SEL!  KEM-SK-SIZE KEM-LOAD
    KBUF-CT KEM-SEL!  KEM-CT-SIZE KEM-LOAD
    KEM-DECAPS
    KBUF-SS KEM-SEL!  R> KEM-SS-SIZE KEM-STORE ;

: .KEM-STATUS
    KEM-STATUS@
    DUP 0 = IF DROP ."  KEM: idle" CR ELSE
    DUP 2 = IF DROP ."  KEM: done" CR ELSE
    DROP ."  KEM: unknown" CR
    THEN THEN ;
