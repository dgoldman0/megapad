\ =====================================================================
\  §1.8  X25519 — Elliptic Curve Diffie-Hellman (RFC 7748)
\ =====================================================================
\  BIOS primitives (per-core ISA via EXT.CRYPTO gf.x25519):
\    X25519-SCALAR! ( addr -- )    Load 32-byte scalar → ACC
\    X25519-POINT!  ( addr -- )    Set operand B address (TSRC0)
\    X25519-GO      ( -- )         Execute gf.x25519 (synchronous)
\    X25519-WAIT    ( -- )         No-op (ISA is synchronous)
\    X25519-STATUS@ ( -- n )       Always 2 (done)
\    X25519-RESULT@ ( addr -- )    Store ACC → 32 bytes

CREATE X25519-PRIV  32 ALLOT
CREATE X25519-PUB   32 ALLOT
CREATE X25519-SHARED 32 ALLOT

\ Curve25519 basepoint (u = 9, little-endian 32 bytes)
CREATE X25519-BASE  32 ALLOT
9 X25519-BASE C!
0 X25519-BASE 1 + C!  0 X25519-BASE 2 + C!  0 X25519-BASE 3 + C!
0 X25519-BASE 4 + C!  0 X25519-BASE 5 + C!  0 X25519-BASE 6 + C!
0 X25519-BASE 7 + C!  0 X25519-BASE 8 + C!  0 X25519-BASE 9 + C!
0 X25519-BASE 10 + C!  0 X25519-BASE 11 + C!  0 X25519-BASE 12 + C!
0 X25519-BASE 13 + C!  0 X25519-BASE 14 + C!  0 X25519-BASE 15 + C!
0 X25519-BASE 16 + C!  0 X25519-BASE 17 + C!  0 X25519-BASE 18 + C!
0 X25519-BASE 19 + C!  0 X25519-BASE 20 + C!  0 X25519-BASE 21 + C!
0 X25519-BASE 22 + C!  0 X25519-BASE 23 + C!  0 X25519-BASE 24 + C!
0 X25519-BASE 25 + C!  0 X25519-BASE 26 + C!  0 X25519-BASE 27 + C!
0 X25519-BASE 28 + C!  0 X25519-BASE 29 + C!  0 X25519-BASE 30 + C!
0 X25519-BASE 31 + C!

\ X25519 ( scalar-addr point-addr result-addr -- )
\   Full scalar multiply: result = clamp(scalar) * point.
: X25519 ( s p r -- )
    >R SWAP
    X25519-SCALAR!
    X25519-POINT!
    X25519-GO
    R> X25519-RESULT@ ;

\ X25519-KEYGEN ( -- )
\   Generate a random private key and compute the public key.
: X25519-KEYGEN ( -- )
    32 0 DO RANDOM8 X25519-PRIV I + C! LOOP
    X25519-PRIV X25519-BASE X25519-PUB X25519 ;

\ X25519-DH ( their-pub-addr -- )
\   Compute shared secret = our_priv * their_pub.
: X25519-DH ( addr -- )
    X25519-PRIV SWAP X25519-SHARED X25519 ;
