\ =====================================================================
\  §1.5  AES-256-GCM Encryption
\ =====================================================================
\
\  BIOS primitives (hardware accelerator at MMIO 0x0700):
\    AES-KEY!      ( addr -- )      write 32-byte key
\    AES-IV!       ( addr -- )      write 12-byte IV/nonce
\    AES-AAD-LEN!  ( n -- )         set AAD length in bytes
\    AES-DATA-LEN! ( n -- )         set data length in bytes
\    AES-CMD!      ( n -- )         0=encrypt, 1=decrypt
\    AES-STATUS@   ( -- n )         0=idle, 2=done, 3=auth-fail
\    AES-DIN!      ( addr -- )      write 16-byte input block
\    AES-DOUT@     ( addr -- )      read 16-byte output block
\    AES-TAG@      ( addr -- )      read 16-byte GCM tag
\    AES-TAG!      ( addr -- )      write 16-byte expected tag
\
\  Flow: set key → set IV → set lengths → CMD → feed blocks → read tag

\ Scratch buffers for AES block I/O (16-byte aligned)
CREATE AES-BLK-IN  16 ALLOT
CREATE AES-BLK-OUT 16 ALLOT
CREATE AES-TAG-BUF 16 ALLOT

\ AES-ENCRYPT-BLK ( src dst -- )  Encrypt one 16-byte block in place.
\   Assumes key/IV/lengths/CMD already set.
: AES-ENCRYPT-BLK ( src dst -- )
    SWAP AES-DIN!                \ feed src block to hardware
    AES-DOUT@                    \ read result into dst
;

\ AES-ENCRYPT ( key iv src dst len -- tag-addr )
\   Encrypt 'len' bytes (must be multiple of 16) from src to dst.
\   key = 32-byte addr, iv = 12-byte addr.
\   Returns address of 16-byte tag buffer.
: AES-ENCRYPT ( key iv src dst len -- tag-addr )
    >R >R >R          \ R: len dst src
    AES-IV!            \ set IV
    AES-KEY!           \ set key
    0 AES-AAD-LEN!
    R> R> R>           \ src dst len
    DUP AES-DATA-LEN!
    0 AES-CMD!         \ 0 = encrypt
    \ loop over 16-byte blocks
    DUP 4 RSHIFT       \ len nblocks (len/16)
    >R                 \ R: nblocks  stack: src dst len
    DROP                \ src dst
    R> 0 DO             \ src dst  (I = block index)
        OVER AES-DIN!       \ feed source block
        DUP AES-DOUT@       \ read output to dest
        SWAP 16 + SWAP 16 + \ advance both pointers
    LOOP
    2DROP
    AES-TAG-BUF AES-TAG@    \ read computed tag
    AES-TAG-BUF              \ return tag buffer address
;

\ AES-DECRYPT ( key iv src dst len tag -- flag )
\   Decrypt 'len' bytes from src to dst.  tag = 16-byte expected tag addr.
\   Returns 0 if authentication passed, -1 if failed.
: AES-DECRYPT ( key iv src dst len tag -- flag )
    AES-TAG!           \ write expected tag for verification
    >R >R >R           \ R: len dst src
    AES-IV!
    AES-KEY!
    0 AES-AAD-LEN!
    R> R> R>           \ src dst len
    DUP AES-DATA-LEN!
    1 AES-CMD!         \ 1 = decrypt
    DUP 4 RSHIFT       \ len nblocks
    >R DROP             \ src dst   R: nblocks
    R> 0 DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    2DROP
    AES-STATUS@ 3 = IF -1 ELSE 0 THEN
;

\ .AES-STATUS ( -- )  Print human-readable AES status.
: .AES-STATUS
    AES-STATUS@
    DUP 0 = IF DROP ."  AES: idle" CR ELSE
    DUP 2 = IF DROP ."  AES: done (OK)" CR ELSE
    DUP 3 = IF DROP ."  AES: AUTH FAIL" CR ELSE
    DROP ."  AES: busy" CR
    THEN THEN THEN ;

\ -- AES-256-GCM with AAD (Authenticated Associated Data) --
\   TLS 1.3 requires AAD (the 5-byte record header) be authenticated
\   but not encrypted.  These words extend AES-ENCRYPT/DECRYPT with AAD.
CREATE AES-AAD-PAD 16 ALLOT           \ zero-padded AAD block
CREATE AES-PARTIAL-PAD 16 ALLOT       \ zero-padded partial final block
VARIABLE _AEAD-AAD
VARIABLE _AEAD-AADLEN
VARIABLE _AEAD-REM

: AES-ENCRYPT-AEAD ( key iv aad aadlen src dst dlen -- tag-addr )
    >R >R >R            \ R: dlen dst src.  Stack: key iv aad aadlen
    _AEAD-AADLEN !
    _AEAD-AAD !
    AES-IV!
    AES-KEY!
    _AEAD-AADLEN @ AES-AAD-LEN!
    R> R> R>             \ src dst dlen
    DUP AES-DATA-LEN!
    0 AES-CMD!
    \ Feed AAD block (zero-padded to 16 bytes)
    AES-AAD-PAD 16 0 FILL
    _AEAD-AAD @ AES-AAD-PAD _AEAD-AADLEN @ CMOVE
    AES-AAD-PAD AES-DIN!
    \ Feed complete 16-byte data blocks
    DUP 15 AND _AEAD-REM !
    DUP 4 RSHIFT         \ src dst dlen nblocks
    >R DROP               \ src dst   R: nblocks
    R> 0 ?DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    \ Handle partial final block (non-16-aligned data)
    _AEAD-REM @ 0> IF
        AES-PARTIAL-PAD 16 0 FILL
        OVER AES-PARTIAL-PAD _AEAD-REM @ CMOVE
        AES-PARTIAL-PAD AES-DIN!
        AES-PARTIAL-PAD AES-DOUT@
        AES-PARTIAL-PAD OVER _AEAD-REM @ CMOVE
    THEN
    2DROP
    AES-TAG-BUF AES-TAG@
    AES-TAG-BUF
;

: AES-DECRYPT-AEAD ( key iv aad aadlen src dst dlen tag -- flag )
    AES-TAG!
    >R >R >R             \ R: dlen dst src.  Stack: key iv aad aadlen
    _AEAD-AADLEN !
    _AEAD-AAD !
    AES-IV!
    AES-KEY!
    _AEAD-AADLEN @ AES-AAD-LEN!
    R> R> R>             \ src dst dlen
    DUP AES-DATA-LEN!
    1 AES-CMD!
    \ Feed AAD block (zero-padded to 16 bytes)
    AES-AAD-PAD 16 0 FILL
    _AEAD-AAD @ AES-AAD-PAD _AEAD-AADLEN @ CMOVE
    AES-AAD-PAD AES-DIN!
    \ Feed complete 16-byte data blocks
    DUP 15 AND _AEAD-REM !
    DUP 4 RSHIFT
    >R DROP
    R> 0 ?DO
        OVER AES-DIN!
        DUP AES-DOUT@
        SWAP 16 + SWAP 16 +
    LOOP
    \ Handle partial final block (non-16-aligned data)
    _AEAD-REM @ 0> IF
        AES-PARTIAL-PAD 16 0 FILL
        OVER AES-PARTIAL-PAD _AEAD-REM @ CMOVE
        AES-PARTIAL-PAD AES-DIN!
        AES-PARTIAL-PAD AES-DOUT@
        AES-PARTIAL-PAD OVER _AEAD-REM @ CMOVE
    THEN
    2DROP
    AES-STATUS@ 3 = IF -1 ELSE 0 THEN
;

