\ =====================================================================
\  §1.13  Hybrid PQ Key Exchange — X25519 + ML-KEM-512
\ =====================================================================
\ Combines classical X25519 ECDH with post-quantum ML-KEM-512 Kyber.
\ Both shared secrets are concatenated and fed through HKDF-Extract +
\ HKDF-Expand to derive the final hybrid shared secret.
\
\ Usage:
\   1. Both parties generate X25519 keypairs (X25519-KEYGEN)
\   2. Both parties generate Kyber keypairs (KYBER-KEYGEN with seed)
\   3. Initiator calls PQ-EXCHANGE-INIT with peer's X25519 pub + Kyber pk
\   4. Responder calls PQ-EXCHANGE-RESP with peer's X25519 pub + the ct
\
\ Scratch buffers for hybrid exchange:
CREATE _PQ-SS-X 32 ALLOT        \ X25519 shared secret
CREATE _PQ-SS-K 32 ALLOT        \ Kyber shared secret
CREATE _PQ-CAT  64 ALLOT        \ concatenated ss: X25519 || Kyber
CREATE _PQ-PRK  32 ALLOT        \ HKDF-Extract output
CREATE _PQ-COIN 32 ALLOT        \ Kyber encaps coin
CREATE _PQ-INFO 9 ALLOT         \ HKDF info string "pq-hybrid"
: _PQ-INFO-INIT
    112 _PQ-INFO C!
    113 _PQ-INFO 1 + C!
    45  _PQ-INFO 2 + C!
    104 _PQ-INFO 3 + C!
    121 _PQ-INFO 4 + C!
    98  _PQ-INFO 5 + C!
    114 _PQ-INFO 6 + C!
    105 _PQ-INFO 7 + C!
    100 _PQ-INFO 8 + C! ;
_PQ-INFO-INIT

\ =====================================================================
\  §1.9  HKDF — HMAC-based Key Derivation Function (RFC 5869)
\ =====================================================================
\  Uses HMAC-SHA3-256 as the underlying PRF.
\  Hash output length (L_H) = 32 bytes.
\
\  HKDF-EXTRACT ( salt slen ikm ilen out -- status )
\    PRK = HMAC(salt, IKM)
\    If salt is 0 / slen=0, uses 32 zero bytes as salt.
\
\  HKDF-EXPAND ( prk info ilen len out -- status )
\    OKM = T(1) || T(2) || ...  truncated to len bytes.
\    T(i) = HMAC(PRK, T(i-1) || info || i)

32 CONSTANT HKDF-HASHLEN

CREATE _HKDF-ZERO-SALT  32 ALLOT       \ 32 zero bytes for null-salt case
_HKDF-ZERO-SALT 32 0 FILL

\ Scratch buffers for Expand
CREATE _HKDF-T       32 ALLOT          \ T(i-1) / T(i) — running HMAC output
VARIABLE _HKDF-PRK-PTR
VARIABLE _HKDF-INFO-PTR
VARIABLE _HKDF-INFO-LEN
VARIABLE _HKDF-OUT-PTR
VARIABLE _HKDF-REMAIN
VARIABLE _HKDF-TPREV-LEN
VARIABLE _HKDF-COUNTER

: _HKDF-WIPE ( -- )
    _HKDF-ZERO-SALT 32 0 FILL
    _HKDF-T 32 0 FILL
    0 _HKDF-PRK-PTR !
    0 _HKDF-INFO-PTR !
    0 _HKDF-INFO-LEN !
    0 _HKDF-OUT-PTR !
    0 _HKDF-REMAIN !
    0 _HKDF-TPREV-LEN !
    0 _HKDF-COUNTER !
    _HMAC-WIPE ;

: _HKDF-OUTPUT-ALIASES-INPUT? ( -- flag )
    _HKDF-REMAIN @ 0= IF FALSE EXIT THEN
    _HKDF-OUT-PTR @ _HKDF-PRK-PTR @ HKDF-HASHLEN + <
    _HKDF-PRK-PTR @ _HKDF-OUT-PTR @ _HKDF-REMAIN @ + < AND
    _HKDF-INFO-LEN @ 0> IF
        _HKDF-OUT-PTR @ _HKDF-INFO-PTR @ _HKDF-INFO-LEN @ + <
        _HKDF-INFO-PTR @ _HKDF-OUT-PTR @ _HKDF-REMAIN @ + < AND OR
    THEN ;

: _HKDF-EXTRACT-NOLOCK ( salt slen ikm ilen out -- status )
    >R                                  \ R: out
    2SWAP                               \ ikm ilen salt slen
    DUP 0= IF                           \ null salt → use zero-salt
        2DROP _HKDF-ZERO-SALT 32
    THEN
    \ Stack: ikm ilen salt slen   R: out
    \ HMAC( salt, IKM ) → out
    2SWAP                               \ salt slen ikm ilen
    R>                                  \ salt slen ikm ilen out
    _HMAC-NOLOCK
;

: HKDF-EXTRACT ( salt slen ikm ilen out -- status )
    CRYPTO-CAPS@ CRYPTO-CAP-SHA3-STREAM AND 0= IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-UNSUPPORTED EXIT
    THEN
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-STATE EXIT
    THEN
    ['] _HKDF-EXTRACT-NOLOCK ['] _HKDF-WIPE ['] SHA3-CLEAR
    _HMAC-HKDF-GUARD ;

: _HKDF-EXPAND-NOLOCK ( prk info ilen len out -- status )
    _HKDF-OUT-PTR !
    _HKDF-REMAIN !
    _HKDF-INFO-LEN !
    _HKDF-INFO-PTR !
    _HKDF-PRK-PTR !
    _HKDF-REMAIN @ 0< IF CRYPTO-RANGE EXIT THEN
    _HKDF-REMAIN @ 8160 > IF CRYPTO-RANGE EXIT THEN
    _HKDF-PRK-PTR @ HKDF-HASHLEN _CRYPTO-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF-INFO-PTR @ _HKDF-INFO-LEN @ _CRYPTO-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF-OUT-PTR @ _HKDF-REMAIN @ _CRYPTO-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF-OUTPUT-ALIASES-INPUT? IF CRYPTO-RANGE EXIT THEN
    0 _HKDF-TPREV-LEN !                \ T(0) = empty
    1 _HKDF-COUNTER !                  \ counter starts at 1
    BEGIN _HKDF-REMAIN @ 0> WHILE
        \ Stream T(i-1), info, and the counter as separate HMAC segments.
        \ This keeps info length caller-bounded instead of imposing a private
        \ concatenation-buffer capacity.
        _HKDF-PRK-PTR @ HKDF-HASHLEN _HMAC-BEGIN-NOLOCK
        DUP IF EXIT THEN DROP
        _HKDF-TPREV-LEN @ 0> IF
            _HKDF-T _HKDF-TPREV-LEN @ SHA3-UPDATE
            DUP IF EXIT THEN DROP
        THEN
        _HKDF-INFO-PTR @ _HKDF-INFO-LEN @ SHA3-UPDATE
        DUP IF EXIT THEN DROP
        _HKDF-COUNTER 1 SHA3-UPDATE
        DUP IF EXIT THEN DROP
        _HKDF-T _HMAC-FINISH-NOLOCK
        DUP IF EXIT THEN DROP
        \ --- Copy min(HASHLEN, remain) → output ---
        _HKDF-REMAIN @ HKDF-HASHLEN MIN
        _HKDF-T _HKDF-OUT-PTR @ ROT CMOVE
        \ Update output pointer and remaining count
        _HKDF-REMAIN @ HKDF-HASHLEN MIN
        DUP _HKDF-OUT-PTR @ + _HKDF-OUT-PTR !
        _HKDF-REMAIN @ SWAP - _HKDF-REMAIN !
        \ Next iteration
        HKDF-HASHLEN _HKDF-TPREV-LEN !
        _HKDF-COUNTER @ 1+ _HKDF-COUNTER !
    REPEAT
    CRYPTO-OK
;

: HKDF-EXPAND ( prk info ilen len out -- status )
    CRYPTO-CAPS@ CRYPTO-CAP-SHA3-STREAM AND 0= IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-UNSUPPORTED EXIT
    THEN
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-STATE EXIT
    THEN
    ['] _HKDF-EXPAND-NOLOCK ['] _HKDF-WIPE ['] SHA3-CLEAR
    _HMAC-HKDF-GUARD ;

\ =====================================================================
\  §1.9b  HMAC-SHA256 / HKDF-SHA256 (for standard TLS 1.3)
\ =====================================================================
\  Same HMAC/HKDF constructions as §1.7/§1.9, but using SHA-256 instead
\  of SHA3-256.  SHA-256 block size = 64 bytes, output = 32 bytes.
\
\  HMAC-SHA256 ( key klen msg mlen out -- status )
\  HKDF-SHA256-EXTRACT ( salt slen ikm ilen out -- status )
\  HKDF-SHA256-EXPAND  ( prk info ilen len out -- status )

64 CONSTANT HMAC256-BLKSZ

CREATE HMAC256-IPAD 64 ALLOT
CREATE HMAC256-OPAD 64 ALLOT
CREATE HMAC256-INNER 32 ALLOT
CREATE HMAC256-KEY 32 ALLOT
VARIABLE _HMAC256-PAD-PTR
VARIABLE _HMAC256-XBYTE
VARIABLE _HMAC256-OUT
VARIABLE _HMAC256-KEY-PTR
VARIABLE _HMAC256-KEY-LEN
VARIABLE _HMAC256-MSG-PTR
VARIABLE _HMAC256-MSG-LEN

: _HMAC256-WIPE ( -- )
    HMAC256-IPAD HMAC256-BLKSZ 0 FILL
    HMAC256-OPAD HMAC256-BLKSZ 0 FILL
    HMAC256-INNER 32 0 FILL
    HMAC256-KEY 32 0 FILL
    0 _HMAC256-PAD-PTR !
    0 _HMAC256-XBYTE !
    0 _HMAC256-OUT !
    0 _HMAC256-KEY-PTR !
    0 _HMAC256-KEY-LEN !
    0 _HMAC256-MSG-PTR !
    0 _HMAC256-MSG-LEN ! ;

: HMAC256-PAD ( key-addr key-len pad-addr xor-byte -- )
    _HMAC256-XBYTE !
    _HMAC256-PAD-PTR !
    _HMAC256-PAD-PTR @ HMAC256-BLKSZ 0 FILL
    0 ?DO
        DUP I + C@
        _HMAC256-PAD-PTR @ I + C!
    LOOP DROP
    HMAC256-BLKSZ 0 DO
        _HMAC256-PAD-PTR @ I + C@
        _HMAC256-XBYTE @ XOR
        _HMAC256-PAD-PTR @ I + C!
    LOOP
;

: _HMAC256-BEGIN-NOLOCK ( key-addr key-len -- status )
    _HMAC256-KEY-LEN !
    _HMAC256-KEY-PTR !
    _HMAC256-KEY-PTR @ _HMAC256-KEY-LEN @ SHA2-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HMAC256-KEY-LEN @ HMAC256-BLKSZ > IF
        _HMAC256-KEY-PTR @ _HMAC256-KEY-LEN @ HMAC256-KEY SHA256
        DUP IF EXIT THEN DROP
        HMAC256-KEY _HMAC256-KEY-PTR !
        32 _HMAC256-KEY-LEN !
    THEN
    _HMAC256-KEY-PTR @ _HMAC256-KEY-LEN @
    2DUP HMAC256-IPAD 54 HMAC256-PAD
    HMAC256-OPAD 92 HMAC256-PAD
    SHA256-INIT DUP IF EXIT THEN DROP
    HMAC256-IPAD HMAC256-BLKSZ SHA256-UPDATE ;

: _HMAC256-FINISH-NOLOCK ( out-addr -- status )
    _HMAC256-OUT !
    HMAC256-INNER SHA256-FINAL
    DUP IF EXIT THEN DROP
    SHA256-INIT DUP IF EXIT THEN DROP
    HMAC256-OPAD HMAC256-BLKSZ SHA256-UPDATE
    DUP IF EXIT THEN DROP
    HMAC256-INNER 32 SHA256-UPDATE
    DUP IF EXIT THEN DROP
    _HMAC256-OUT @ SHA256-FINAL ;

: _HMAC256-NOLOCK ( key-addr key-len msg-addr msg-len out-addr -- status )
    _HMAC256-OUT !
    _HMAC256-MSG-LEN !
    _HMAC256-MSG-PTR !
    _HMAC256-KEY-LEN !
    _HMAC256-KEY-PTR !
    _HMAC256-KEY-PTR @ _HMAC256-KEY-LEN @ _HMAC256-BEGIN-NOLOCK
    DUP IF EXIT THEN DROP
    _HMAC256-MSG-PTR @ _HMAC256-MSG-LEN @ SHA256-UPDATE
    DUP IF EXIT THEN DROP
    _HMAC256-OUT @ _HMAC256-FINISH-NOLOCK ;

: HMAC-SHA256 ( key-addr key-len msg-addr msg-len out-addr -- status )
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS SHA256-STATE EXIT
    THEN
    ['] _HMAC256-NOLOCK ['] _HMAC256-WIPE ['] SHA256-CLEAR
    _HMAC-HKDF-GUARD ;

\ Scratch buffers for HKDF-SHA256
CREATE _HKDF256-ZERO-SALT  32 ALLOT
_HKDF256-ZERO-SALT 32 0 FILL
CREATE _HKDF256-T       32 ALLOT
VARIABLE _HKDF256-PRK-PTR
VARIABLE _HKDF256-INFO-PTR
VARIABLE _HKDF256-INFO-LEN
VARIABLE _HKDF256-OUT-PTR
VARIABLE _HKDF256-REMAIN
VARIABLE _HKDF256-TPREV-LEN
VARIABLE _HKDF256-COUNTER

: _HKDF256-WIPE ( -- )
    _HKDF256-ZERO-SALT 32 0 FILL
    _HKDF256-T 32 0 FILL
    0 _HKDF256-PRK-PTR !
    0 _HKDF256-INFO-PTR !
    0 _HKDF256-INFO-LEN !
    0 _HKDF256-OUT-PTR !
    0 _HKDF256-REMAIN !
    0 _HKDF256-TPREV-LEN !
    0 _HKDF256-COUNTER !
    _HMAC256-WIPE ;

: _HKDF256-OUTPUT-ALIASES-INPUT? ( -- flag )
    _HKDF256-REMAIN @ 0= IF FALSE EXIT THEN
    _HKDF256-OUT-PTR @ _HKDF256-PRK-PTR @ 32 + <
    _HKDF256-PRK-PTR @ _HKDF256-OUT-PTR @ _HKDF256-REMAIN @ + < AND
    _HKDF256-INFO-LEN @ 0> IF
        _HKDF256-OUT-PTR @ _HKDF256-INFO-PTR @ _HKDF256-INFO-LEN @ + <
        _HKDF256-INFO-PTR @ _HKDF256-OUT-PTR @ _HKDF256-REMAIN @ + < AND OR
    THEN ;

: _HKDF256-EXTRACT-NOLOCK ( salt slen ikm ilen out -- status )
    >R
    2SWAP
    DUP 0= IF
        2DROP _HKDF256-ZERO-SALT 32
    THEN
    2SWAP
    R>
    _HMAC256-NOLOCK
;

: HKDF-SHA256-EXTRACT ( salt slen ikm ilen out -- status )
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS SHA256-STATE EXIT
    THEN
    ['] _HKDF256-EXTRACT-NOLOCK ['] _HKDF256-WIPE ['] SHA256-CLEAR
    _HMAC-HKDF-GUARD ;

: _HKDF256-EXPAND-NOLOCK ( prk info ilen len out -- status )
    _HKDF256-OUT-PTR !
    _HKDF256-REMAIN !
    _HKDF256-INFO-LEN !
    _HKDF256-INFO-PTR !
    _HKDF256-PRK-PTR !
    _HKDF256-REMAIN @ 0< IF SHA256-RANGE EXIT THEN
    _HKDF256-REMAIN @ 8160 > IF SHA256-RANGE EXIT THEN
    _HKDF256-PRK-PTR @ 32 SHA2-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF256-INFO-PTR @ _HKDF256-INFO-LEN @ SHA2-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF256-OUT-PTR @ _HKDF256-REMAIN @ SHA2-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HKDF256-OUTPUT-ALIASES-INPUT? IF SHA256-RANGE EXIT THEN
    0 _HKDF256-TPREV-LEN !
    1 _HKDF256-COUNTER !
    BEGIN _HKDF256-REMAIN @ 0> WHILE
        _HKDF256-PRK-PTR @ 32 _HMAC256-BEGIN-NOLOCK
        DUP IF EXIT THEN DROP
        _HKDF256-TPREV-LEN @ 0> IF
            _HKDF256-T _HKDF256-TPREV-LEN @ SHA256-UPDATE
            DUP IF EXIT THEN DROP
        THEN
        _HKDF256-INFO-PTR @ _HKDF256-INFO-LEN @ SHA256-UPDATE
        DUP IF EXIT THEN DROP
        _HKDF256-COUNTER 1 SHA256-UPDATE
        DUP IF EXIT THEN DROP
        _HKDF256-T _HMAC256-FINISH-NOLOCK
        DUP IF EXIT THEN DROP
        _HKDF256-REMAIN @ 32 MIN
        _HKDF256-T _HKDF256-OUT-PTR @ ROT CMOVE
        _HKDF256-REMAIN @ 32 MIN
        DUP _HKDF256-OUT-PTR @ + _HKDF256-OUT-PTR !
        _HKDF256-REMAIN @ SWAP - _HKDF256-REMAIN !
        32 _HKDF256-TPREV-LEN !
        _HKDF256-COUNTER @ 1+ _HKDF256-COUNTER !
    REPEAT
    SHA256-OK ;

: HKDF-SHA256-EXPAND ( prk info ilen len out -- status )
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS SHA256-STATE EXIT
    THEN
    ['] _HKDF256-EXPAND-NOLOCK ['] _HKDF256-WIPE ['] SHA256-CLEAR
    _HMAC-HKDF-GUARD ;

\ PQ-DERIVE ( out -- status )
\   Internal: HKDF-derive final 32-byte key from concatenated secrets.
\   Assumes _PQ-CAT already has 64 bytes of combined keying material.
: PQ-DERIVE ( out-addr -- status )
    >R
    \ HKDF-Extract: salt=empty(0), ikm=_PQ-CAT(64B) → _PQ-PRK
    0 0 _PQ-CAT 64 _PQ-PRK HKDF-EXTRACT
    DUP IF R> DROP EXIT THEN DROP
    \ HKDF-Expand: prk=_PQ-PRK, info="pq-hybrid"(9B), len=32, out
    _PQ-PRK _PQ-INFO 9 32 R> HKDF-EXPAND ;

\ PQ-EXCHANGE-INIT ( their-x-pub kyber-pk ct-out ss-out -- status )
\   Initiator side:
\   1. X25519-DH with their X25519 public key → _PQ-SS-X
\   2. Generate random coin, KYBER-ENCAPS with their Kyber pk → ct + _PQ-SS-K
\   3. Concatenate, HKDF-derive → ss-out
: PQ-EXCHANGE-INIT ( their-x-pub kyber-pk ct-out ss-out -- status )
    >R >R                          \ R: ss-out ct-out
    \ X25519 DH
    SWAP                            \ Stack: kyber-pk their-x-pub
    X25519-PRIV OVER _PQ-SS-X X25519
    DROP                            \ Stack: kyber-pk
    \ Generate random coin for Kyber
    32 0 DO RANDOM8 _PQ-COIN I + C! LOOP
    \ KYBER-ENCAPS ( pk coin ct ss -- )
    _PQ-COIN R> _PQ-SS-K KYBER-ENCAPS
    \ Concatenate: _PQ-CAT = _PQ-SS-X || _PQ-SS-K
    _PQ-SS-X _PQ-CAT 32 CMOVE
    _PQ-SS-K _PQ-CAT 32 + 32 CMOVE
    \ Derive final key
    R> PQ-DERIVE ;

\ PQ-EXCHANGE-RESP ( their-x-pub ct kyber-sk ss-out -- status )
\   Responder side:
\   1. X25519-DH with their X25519 public key → _PQ-SS-X
\   2. KYBER-DECAPS with ct and our Kyber sk → _PQ-SS-K
\   3. Concatenate, HKDF-derive → ss-out
: PQ-EXCHANGE-RESP ( their-x-pub ct kyber-sk ss-out -- status )
    >R                              \ R: ss-out
    \ Stack: their-x-pub ct kyber-sk
    ROT                             \ Stack: ct kyber-sk their-x-pub
    X25519-PRIV OVER _PQ-SS-X X25519
    DROP                            \ Stack: ct kyber-sk
    \ KYBER-DECAPS ( ct sk ss -- )
    _PQ-SS-K KYBER-DECAPS
    \ Concatenate
    _PQ-SS-X _PQ-CAT 32 CMOVE
    _PQ-SS-K _PQ-CAT 32 + 32 CMOVE
    \ Derive final key
    R> PQ-DERIVE ;
