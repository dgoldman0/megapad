\ =====================================================================
\  §7.6.1  Filesystem Encryption
\ =====================================================================
\  Optional at-rest encryption for MP64FS files using AES-256-GCM.
\  Requires FS-KEY to be set before use.  Operates on OPEN'd files.
\
\  On-disk layout of an encrypted file:
\    Sectors contain: ciphertext (padded to 16B) || 16-byte GCM tag
\    Directory used_bytes = original plaintext length (unchanged)
\    Directory flags bit 2 = encrypted
\    IV = 12 bytes derived from directory slot number
\
\  FS-KEY!     ( addr -- )          set 32-byte filesystem encryption key
\  ENCRYPTED?  ( fdesc -- flag )    true if file has encrypted flag
\  FENCRYPT    ( fdesc -- ior )     encrypt open file in-place (0=ok)
\  FDECRYPT    ( fdesc -- flag )    decrypt open file (0=ok, -1=auth-fail)

CREATE FS-KEY 32 ALLOT            \ system-level encryption key
CREATE FS-IV  12 ALLOT            \ derived IV for current operation

4 CONSTANT F-ENC-FLAG             \ flags bit 2 = encrypted

VARIABLE _FE-DESC                 \ current file descriptor
VARIABLE _FE-USED                 \ original plaintext byte count
VARIABLE _FE-PAD                  \ padded length (multiple of 16)
VARIABLE _FE-SECS                 \ number of sectors for I/O
VARIABLE _FE-BUF1                 \ buffer 1
VARIABLE _FE-BUF2                 \ buffer 2

\ FS-KEY! ( addr -- )  Copy 32-byte key into FS-KEY.
: FS-KEY!  FS-KEY 32 CMOVE ;

\ _FE-MKIV ( fdesc -- )  Derive 12-byte IV from file's directory slot.
: _FE-MKIV
    FS-IV 12 0 FILL
    F.SLOT FS-IV !
;

\ ENCRYPTED? ( fdesc -- flag )  True if file has encrypted flag set.
: ENCRYPTED?
    F.SLOT DIRENT 33 + C@
    F-ENC-FLAG AND 0<>
;

\ _FE-SET-ENC ( fdesc -- )  Set encrypted flag in FS-DIR cache.
: _FE-SET-ENC
    F.SLOT DIRENT 33 +
    DUP C@ F-ENC-FLAG OR SWAP C!
;

\ _FE-CLR-ENC ( fdesc -- )  Clear encrypted flag in FS-DIR cache.
: _FE-CLR-ENC
    F.SLOT DIRENT 33 +
    DUP C@ F-ENC-FLAG INVERT AND SWAP C!
;

\ FENCRYPT ( fdesc -- ior )
\   Encrypt an open file's data in-place on disk.
\   Uses FS-KEY as the encryption key.
\   Returns 0 on success, -1 on error.
: FENCRYPT
    DUP ENCRYPTED? IF DROP 0 EXIT THEN    \ already encrypted — no-op
    _FE-DESC !                                      \ consume fdesc
    _FE-DESC @ FSIZE DUP _FE-USED !
    0= IF 0 EXIT THEN                              \ empty — skip
    \ Padded length = round up to 16
    _FE-USED @ 15 + -16 AND _FE-PAD !
    \ Check: padded + 16 (tag) must fit in allocated sectors
    _FE-PAD @ 16 +
    _FE-DESC @ F.MAX 512 * > IF
        ."  FENCRYPT: insufficient space" CR -1 EXIT
    THEN
    \ Sectors for ciphertext + tag
    _FE-PAD @ 16 + 511 + 512 / _FE-SECS !
    \ Allocate sector-aligned buffers (DMA operates on full sectors)
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF _FE-BUF1 @ DMA-FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero both buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read file data from disk into buf1
    _FE-BUF1 @ _FE-DESC @ F.START
    _FE-USED @ 511 + 512 / _DISK-READ
    \ Derive IV from directory slot
    _FE-DESC @ _FE-MKIV
    \ Encrypt: ( key iv src dst len -- tag-addr )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @ ENCRYPT
    \ Copy 16-byte tag after ciphertext in buf2
    _FE-BUF2 @ _FE-PAD @ + 16 CMOVE
    \ DMA-write ciphertext + tag back to disk
    _FE-BUF2 @ _FE-DESC @ F.START _FE-SECS @ _DISK-WRITE
    \ Set encrypted flag and sync directory
    _FE-DESC @ _FE-SET-ENC
    _FE-USED @ _FE-DESC @ F.SLOT DIRENT 28 + L!
    FS-SYNC
    \ Free buffers
    _FE-BUF1 @ DMA-FREE
    _FE-BUF2 @ DMA-FREE
    0
;

\ FDECRYPT ( fdesc -- flag )
\   Decrypt an encrypted file in-place on disk.
\   Returns 0 if authentication passed, -1 if failed or not encrypted.
: FDECRYPT
    DUP ENCRYPTED? 0= IF DROP 0 EXIT THEN  \ not encrypted — no-op
    _FE-DESC !                                      \ consume fdesc
    _FE-DESC @ FSIZE DUP _FE-USED !
    0= IF 0 EXIT THEN
    \ Padded length
    _FE-USED @ 15 + -16 AND _FE-PAD !
    _FE-PAD @ 16 + 511 + 512 / _FE-SECS !
    \ Allocate sector-aligned buffers (DMA operates on full sectors)
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF DROP -1 EXIT THEN _FE-BUF1 !
    _FE-SECS @ 512 * DMA-ALLOCATE DROP DUP 0= IF _FE-BUF1 @ DMA-FREE -1 EXIT THEN _FE-BUF2 !
    \ Zero buffers
    _FE-BUF1 @ _FE-SECS @ 512 * 0 FILL
    _FE-BUF2 @ _FE-SECS @ 512 * 0 FILL
    \ DMA-read ciphertext + tag from disk
    _FE-BUF1 @ _FE-DESC @ F.START _FE-SECS @ _DISK-READ
    \ Derive IV
    _FE-DESC @ _FE-MKIV
    \ Decrypt: ( key iv src dst len tag -- flag )
    FS-KEY FS-IV _FE-BUF1 @ _FE-BUF2 @ _FE-PAD @
    _FE-BUF1 @ _FE-PAD @ +       \ tag is right after ciphertext
    DECRYPT
    \ flag on stack: 0 = auth OK, -1 = auth fail
    DUP 0= IF
        \ Write plaintext back to disk
        _FE-BUF2 @ _FE-DESC @ F.START
        _FE-USED @ 511 + 512 / _DISK-WRITE
        \ Clear encrypted flag, sync
        _FE-DESC @ _FE-CLR-ENC
        _FE-USED @ _FE-DESC @ F.SLOT DIRENT 28 + L!
        FS-SYNC
    THEN
    \ Free buffers
    _FE-BUF1 @ DMA-FREE
    _FE-BUF2 @ DMA-FREE
;

