\ --- HMAC-SHA3-256 ---
\ HMAC(K,m) = SHA3((K ^ opad) || SHA3((K ^ ipad) || m))
\ SHA3-256 rate (block size) = 136 bytes

\ HMAC and HKDF use shared KDOS scratch, so one nonblocking hardware lock
\ serializes both hash families across every core.  SHA3 callers take this
\ lock before the checked BIOS path takes its private lock 8.
9 CONSTANT HMAC-HKDF-LOCK

: _HMAC-HKDF-TRY ( -- busy )
    HMAC-HKDF-LOCK SPIN@ ;

: _HMAC-HKDF-RELEASE ( -- )
    HMAC-HKDF-LOCK SPIN! ;

: _HMAC-HKDF-DROP-ARGS ( a b c d e -- )
    2DROP 2DROP DROP ;

\ Execute one five-argument HMAC/HKDF core while lock 9 is owned.  CATCH
\ restores all five original arguments on an exception, so discard that frame,
\ abort the selected checked-hash transaction, and then erase KDOS scratch.  A
\ failed lower cleanup retains lock 9 fail-closed and takes precedence over the
\ original exception; otherwise release the lock and rethrow the exact code.
: _HMAC-HKDF-GUARD ( a b c d e work-xt wipe-xt clear-xt -- status )
    SWAP >R >R CATCH
    ?DUP IF
        >R _HMAC-HKDF-DROP-ARGS
        R> R> R>
        >R SWAP >R EXECUTE
        R> R> EXECUTE
        SWAP ?DUP IF SWAP DROP THROW THEN
        _HMAC-HKDF-RELEASE
        THROW
    THEN
    R> DROP
    R> EXECUTE
    _HMAC-HKDF-RELEASE ;

136 CONSTANT HMAC-BLKSZ

CREATE HMAC-IPAD 136 ALLOT
CREATE HMAC-OPAD 136 ALLOT
CREATE HMAC-INNER 32 ALLOT
CREATE HMAC-KEY 32 ALLOT
VARIABLE _HMAC-PAD-PTR
VARIABLE _HMAC-XBYTE
VARIABLE _HMAC-OUT
VARIABLE _HMAC-KEY-PTR
VARIABLE _HMAC-KEY-LEN
VARIABLE _HMAC-MSG-PTR
VARIABLE _HMAC-MSG-LEN
VARIABLE _VERIFY-ACC

: _HMAC-WIPE ( -- )
    HMAC-IPAD HMAC-BLKSZ 0 FILL
    HMAC-OPAD HMAC-BLKSZ 0 FILL
    HMAC-INNER 32 0 FILL
    HMAC-KEY 32 0 FILL
    0 _HMAC-PAD-PTR !
    0 _HMAC-XBYTE !
    0 _HMAC-OUT !
    0 _HMAC-KEY-PTR !
    0 _HMAC-KEY-LEN !
    0 _HMAC-MSG-PTR !
    0 _HMAC-MSG-LEN ! ;

\ HMAC-PAD ( key-addr key-len pad-addr xor-byte -- )
\   Zero pad, copy key into pad, XOR entire pad with xor-byte.
: HMAC-PAD
    _HMAC-XBYTE !                     \ save xor-byte
    _HMAC-PAD-PTR !                   \ save pad-addr
    \ Zero the pad
    _HMAC-PAD-PTR @ HMAC-BLKSZ 0 FILL
    \ Copy key bytes into pad[0..klen-1]
    0 ?DO                              \ key-addr  (limit=klen start=0)
        DUP I + C@                     \ key-addr byte
        _HMAC-PAD-PTR @ I + C!         \ key-addr
    LOOP DROP
    \ XOR every byte of pad with xor-byte
    HMAC-BLKSZ 0 DO
        _HMAC-PAD-PTR @ I + C@
        _HMAC-XBYTE @ XOR
        _HMAC-PAD-PTR @ I + C!
    LOOP
;

\ Normalize a key, build both pads, and start the inner hash.
\ Long HMAC keys are hashed per FIPS 198 rather than imposing a private
\ capacity limit on callers.
: _HMAC-BEGIN-NOLOCK ( key-addr key-len -- status )
    _HMAC-KEY-LEN !
    _HMAC-KEY-PTR !
    _HMAC-KEY-PTR @ _HMAC-KEY-LEN @ _CRYPTO-SPAN-STATUS
    DUP IF EXIT THEN DROP
    _HMAC-KEY-LEN @ HMAC-BLKSZ > IF
        _HMAC-KEY-PTR @ _HMAC-KEY-LEN @ HMAC-KEY SHA3
        DUP IF EXIT THEN DROP
        HMAC-KEY _HMAC-KEY-PTR !
        32 _HMAC-KEY-LEN !
    THEN
    _HMAC-KEY-PTR @ _HMAC-KEY-LEN @
    2DUP HMAC-IPAD 54 HMAC-PAD
    HMAC-OPAD 92 HMAC-PAD
    SHA3-256-MODE SHA3-BEGIN
    DUP IF EXIT THEN DROP
    HMAC-IPAD HMAC-BLKSZ SHA3-UPDATE ;

\ Finish the active inner hash and compute the outer hash.
: _HMAC-FINISH-NOLOCK ( out-addr -- status )
    _HMAC-OUT !
    HMAC-INNER SHA3-FINAL
    DUP IF EXIT THEN DROP
    SHA3-256-MODE SHA3-BEGIN
    DUP IF EXIT THEN DROP
    HMAC-OPAD HMAC-BLKSZ SHA3-UPDATE
    DUP IF EXIT THEN DROP
    HMAC-INNER 32 SHA3-UPDATE
    DUP IF EXIT THEN DROP
    _HMAC-OUT @ SHA3-FINAL ;

\ Private HMAC core.  Its caller owns HMAC-HKDF-LOCK.
: _HMAC-NOLOCK ( key-addr key-len msg-addr msg-len out-addr -- status )
    _HMAC-OUT !
    _HMAC-MSG-LEN !
    _HMAC-MSG-PTR !
    _HMAC-KEY-LEN !
    _HMAC-KEY-PTR !
    _HMAC-KEY-PTR @ _HMAC-KEY-LEN @ _HMAC-BEGIN-NOLOCK
    DUP IF EXIT THEN DROP
    _HMAC-MSG-PTR @ _HMAC-MSG-LEN @ SHA3-UPDATE
    DUP IF EXIT THEN DROP
    _HMAC-OUT @ _HMAC-FINISH-NOLOCK ;

\ HMAC ( key-addr key-len msg-addr msg-len out-addr -- status )
\ Capability absence wins over lock contention and argument validation.
: HMAC  ( key-addr key-len msg-addr msg-len out-addr -- status )
    CRYPTO-CAPS@ CRYPTO-CAP-SHA3-STREAM AND 0= IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-UNSUPPORTED EXIT
    THEN
    _HMAC-HKDF-TRY IF
        _HMAC-HKDF-DROP-ARGS CRYPTO-STATE EXIT
    THEN
    ['] _HMAC-NOLOCK ['] _HMAC-WIPE ['] SHA3-CLEAR
    _HMAC-HKDF-GUARD ;

\ ENCRYPT ( key iv src dst len -- tag-addr )  AES-256-GCM encrypt.
: ENCRYPT  AES-ENCRYPT ;

\ DECRYPT ( key iv src dst len tag -- flag )  AES-256-GCM decrypt+verify.
: DECRYPT  AES-DECRYPT ;

\ VERIFY ( addr1 addr2 len -- flag )
\   Constant-time comparison.  Returns 0 if equal, -1 if different.
: VERIFY
    0 _VERIFY-ACC !                     \ acc = 0
    0 DO                                \ a1 a2
        OVER I + C@                     \ a1 a2 b1
        OVER I + C@                     \ a1 a2 b1 b2
        XOR _VERIFY-ACC @ OR _VERIFY-ACC !  \ acc |= (b1 ^ b2)
    LOOP 2DROP
    _VERIFY-ACC @ IF -1 ELSE 0 THEN     \ -1=different, 0=equal
;
