
\ ---------------------------------------------------------------------
\  Read-only partition discovery (raw, MBR, and GPT)
\ ---------------------------------------------------------------------

5120 CONSTANT PART-WORKSPACE-MIN
4096 CONSTANT GPT-MAX-ENTRIES
4096 CONSTANT GPT-MAX-ENTRY-SIZE

9 IOR-C-CORRUPT IOR-D-PARTITION IOR-F-CORRUPT
    IOR-MAKE CONSTANT PART-E-CORRUPT
4 IOR-C-CAPACITY IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-CAPACITY
14 IOR-C-WORKSPACE IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-WORKSPACE
2 IOR-C-UNSUPPORTED IOR-D-PARTITION IOR-F-UNSUPPORTED
    IOR-MAKE CONSTANT PART-E-UNSUPPORTED
1 IOR-C-UNSUPPORTED IOR-D-PARTITION IOR-F-UNSUPPORTED
    IOR-MAKE CONSTANT PART-E-CRC-UNSUPPORTED
2 IOR-C-BUSY IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-CRC-BUSY
14 IOR-C-BAD-DESCRIPTOR IOR-D-PARTITION 0
    IOR-MAKE CONSTANT PART-E-BAD-DESCRIPTOR

\ Translate the checked BIOS CRC status without pretending engine contention
\ is corrupt media.  RANGE is impossible for the fixed mode-4 path and is
\ therefore an internal parser/backend mismatch rather than a disk result.
: _GPT-CRC-STATUS>IOR  ( status -- ior )
    DUP 1 = IF DROP PART-E-CRC-UNSUPPORTED EXIT THEN
    DUP 2 = IF DROP PART-E-CRC-BUSY EXIT THEN
    14 IOR-D-PARTITION 0 IOR-MAKE ;

VARIABLE _PART-BD
VARIABLE _PART-OUT
VARIABLE _PART-MAX
VARIABLE _PART-WS
VARIABLE _PART-BYTES
VARIABLE _PART-COUNT

: PART-VOLUME  ( index -- vol )  /VOLUME * _PART-OUT @ + ;

: _PART-CLEAR  ( -- )
    _PART-MAX @ 0 DO I PART-VOLUME VOL-CLOSE DROP LOOP
    0 _PART-COUNT ! ;

: _PART-SETUP  ( bd volumes max workspace bytes -- ior )
    _PART-BYTES ! _PART-WS ! _PART-MAX ! _PART-OUT ! _PART-BD !
    _PART-OUT @ 0= _PART-MAX @ 0= OR IF PART-E-CAPACITY EXIT THEN
    \ Once the output extent itself is usable, every later failure is
    \ transactional even if it occurs before the first metadata read.
    _PART-CLEAR
    _PART-BD @ BD-VALID? 0= IF PART-E-BAD-DESCRIPTOR EXIT THEN
    _PART-BD @ BD-STALE? IF VOL-E-STALE EXIT THEN
    _PART-WS @ 0= _PART-BYTES @ PART-WORKSPACE-MIN < OR IF
        PART-E-WORKSPACE EXIT
    THEN
    0 ;

: _PART-FAIL  ( ior -- count ior )
    _PART-CLEAR 0 SWAP ;

: _PART-READ  ( buffer lba -- ior )
    1 _PART-BD @ BD-READ             ( completed ior )
    DUP IF NIP EXIT THEN
    DROP 1 = IF 0 ELSE BD-E-INTERNAL THEN ;

: _PART-FINALIZE  ( vol -- ior )
    DUP 48 + @ OVER 56 + @ _PART-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        DROP PART-E-CORRUPT EXIT
    THEN
    STORAGE-ABI OVER 8 + !
    STORAGE-COOKIE-NEXT OVER 16 + !
    _PART-BD @ OVER 24 + !
    _PART-BD @ BD.COOKIE OVER 32 + !
    _PART-BD @ BD.MEDIA-GEN OVER 40 + !
    _PART-BD @ BD.SECTOR-SIZE OVER 64 + !
    _PART-BD @ BD.FLAGS OVER 72 + !
    VOLUME-MAGIC SWAP !
    1 _PART-BD @ BD.REFS +!
    0 ;

VARIABLE _OV-A
VARIABLE _OV-ALEN
VARIABLE _OV-B
VARIABLE _OV-BLEN
: _RANGES-OVERLAP?  ( a alen b blen -- flag )
    _OV-BLEN ! _OV-B ! _OV-ALEN ! _OV-A !
    _OV-A @ _OV-B @ _OV-BLEN @ + U<
    _OV-B @ _OV-A @ _OV-ALEN @ + U< AND ;

: _MBR-ENTRY  ( table-index -- addr )  16 * _PART-WS @ 446 + + ;
: _MBR-TYPE   ( entry -- type )  4 + C@ ;
: _MBR-BASE   ( entry -- lba )   8 + L@ ;
: _MBR-LEN    ( entry -- count ) 12 + L@ ;
: _MBR-EXTENDED?  ( type -- flag )
    DUP 0x05 = OVER 0x0F = OR SWAP 0x85 = OR ;

VARIABLE _MBR-E
VARIABLE _MBR-TYPE-V
VARIABLE _MBR-BASE-V
VARIABLE _MBR-LEN-V
VARIABLE _MBR-BOOT-V
VARIABLE _MBR-INDEX-V

: _MBR-CANDIDATE-OVERLAP?  ( base len -- flag )
    _MBR-LEN-V ! _MBR-BASE-V !
    FALSE
    _PART-COUNT @ DUP IF
        0 DO
            _MBR-BASE-V @ _MBR-LEN-V @
            I PART-VOLUME VOL.BASE I PART-VOLUME VOL.SECTORS
            _RANGES-OVERLAP? OR
        LOOP
    ELSE DROP THEN ;

: _MBR-STAGE  ( table-index -- ior )
    DUP _MBR-INDEX-V ! _MBR-ENTRY _MBR-E !
    _MBR-E @ C@ _MBR-BOOT-V !
    _MBR-E @ _MBR-TYPE _MBR-TYPE-V !
    _MBR-E @ _MBR-BASE _MBR-BASE-V !
    _MBR-E @ _MBR-LEN _MBR-LEN-V !
    _MBR-TYPE-V @ 0= IF
        _MBR-BOOT-V @ _MBR-BASE-V @ OR _MBR-LEN-V @ OR IF
            PART-E-CORRUPT EXIT
        THEN
        0 EXIT
    THEN
    _MBR-BOOT-V @ DUP 0<> SWAP 0x80 <> AND IF
        PART-E-CORRUPT EXIT
    THEN
    _MBR-TYPE-V @ _MBR-EXTENDED? IF PART-E-UNSUPPORTED EXIT THEN
    _MBR-BASE-V @ 0= _MBR-LEN-V @ 0= OR IF PART-E-CORRUPT EXIT THEN
    _MBR-BASE-V @ _MBR-LEN-V @ _PART-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        PART-E-CORRUPT EXIT
    THEN
    _MBR-BASE-V @ _MBR-LEN-V @ _MBR-CANDIDATE-OVERLAP? IF
        PART-E-CORRUPT EXIT
    THEN
    _PART-COUNT @ _PART-MAX @ >= IF PART-E-CAPACITY EXIT THEN
    _PART-COUNT @ PART-VOLUME DUP /VOLUME 0 FILL
    DUP _MBR-BASE-V @ SWAP 48 + !
    DUP _MBR-LEN-V @ SWAP 56 + !
    DUP VOL-SCHEME-MBR SWAP 80 + !
    DUP _MBR-INDEX-V @ SWAP 88 + !
    DUP _MBR-TYPE-V @ SWAP 96 + !
    _MBR-BOOT-V @ SWAP 136 + !
    1 _PART-COUNT +!
    0 ;

: _MBR-PROTECTIVE-VALID?  ( -- flag )
    _PART-COUNT @ 1 <> IF FALSE EXIT THEN
    0 PART-VOLUME
    DUP 96 + @ 0xEE =
    OVER VOL.BASE 1 = AND
    OVER VOL.SECTORS _PART-BD @ BD.SECTORS 1- = AND
    SWAP 136 + @ 0= AND ;

: _MBR-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 510 + W@ 0xAA55 <> IF PART-E-CORRUPT _PART-FAIL EXIT THEN
    4 0 DO
        I _MBR-STAGE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
    LOOP
    \ A protective table is GPT metadata, never an ordinary MBR slice.
    FALSE
    _PART-COUNT @ DUP IF
        0 DO I PART-VOLUME 96 + @ 0xEE = OR LOOP
    ELSE DROP THEN
    IF
        _MBR-PROTECTIVE-VALID? IF PART-E-UNSUPPORTED ELSE PART-E-CORRUPT THEN
        _PART-FAIL EXIT
    THEN
    _PART-BD @ BD-STALE? IF VOL-E-STALE _PART-FAIL EXIT THEN
    _PART-COUNT @ DUP IF
        0 DO
            I PART-VOLUME _PART-FINALIZE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
        LOOP
    ELSE DROP THEN
    _PART-COUNT @ 0 ;

\ GPT uses the reflected IEEE CRC-32 tuple.  KDOS's general CRC32-BUF is the
\ non-reflected BZIP2 tuple and is deliberately not reused here.  Each call
\ below owns mode 4 only while bytes already resident in Bank 0 are fed, then
\ raw-final releases the engine before any later storage I/O.
: _CRC32-IEEE-RAW?  ( addr bytes seed -- raw status )
    4 CRC-MODE! ?DUP IF
        >R 2DROP DROP 0 R> EXIT
    THEN
    CRC-INIT! ?DUP IF
        >R 2DROP CRC-FINAL@ DROP 0 R> EXIT
    THEN
    _CRC-BUF-CHECKED ?DUP IF
        >R CRC-FINAL@ DROP 0 R> EXIT
    THEN
    CRC-RAW-FINAL@
    DUP IF
        >R DROP CRC-FINAL@ DROP 0 R>
    THEN ;

: _CRC32-IEEE-CHECKED  ( addr bytes -- crc status )
    0xFFFFFFFF _CRC32-IEEE-RAW?
    DUP IF EXIT THEN
    DROP INVERT 0xFFFFFFFF AND 0 ;

: CRC32-IEEE-BUF  ( addr bytes -- crc )
    _CRC32-IEEE-CHECKED _CRC-REQUIRE-OK ;

: _GUID-ZERO?  ( guid -- flag )  DUP @ SWAP 8 + @ OR 0= ;
: _GUID-SAME?  ( guid-a guid-b -- flag )
    2DUP @ SWAP @ = >R
    8 + @ SWAP 8 + @ = R> AND ;

: _BYTES-ZERO?  ( addr bytes -- flag )
    BEGIN DUP 0> WHILE
        OVER C@ IF 2DROP FALSE EXIT THEN
        SWAP 1+ SWAP 1-
    REPEAT
    2DROP TRUE ;

VARIABLE _GH-BUF
VARIABLE _GH-CUR
VARIABLE _GH-BACK
VARIABLE _GH-SAVED-CRC
VARIABLE _GPT-CRC-IOR

: _GPT-HEADER-CRC?  ( header -- flag )
    DUP _GH-BUF !
    16 + L@ _GH-SAVED-CRC !
    0 _GH-BUF @ 16 + L!
    _GH-BUF @ DUP 12 + L@ _CRC32-IEEE-CHECKED
    _GH-SAVED-CRC @ _GH-BUF @ 16 + L!
    DUP IF
        _GPT-CRC-STATUS>IOR _GPT-CRC-IOR !
        DROP FALSE EXIT
    THEN
    DROP
    _GH-SAVED-CRC @ = ;

: _GPT-HEADER-VALID?  ( header current-lba backup-lba -- flag )
    _GH-BACK ! _GH-CUR ! _GH-BUF !
    _GH-BUF @ @ 0x5452415020494645 <> IF FALSE EXIT THEN  \ "EFI PART"
    _GH-BUF @ 8 + L@ 0x00010000 <> IF FALSE EXIT THEN
    _GH-BUF @ 12 + L@ DUP 92 < SWAP 512 > OR IF FALSE EXIT THEN
    _GH-BUF @ 20 + L@ IF FALSE EXIT THEN
    _GH-BUF @ 24 + @ _GH-CUR @ <> IF FALSE EXIT THEN
    _GH-BUF @ 32 + @ _GH-BACK @ <> IF FALSE EXIT THEN
    _GH-BUF @ 40 + @ 1 U> 0= IF FALSE EXIT THEN
    _GH-BUF @ 40 + @ _GH-BUF @ 48 + @ U> IF FALSE EXIT THEN
    _GH-BUF @ 48 + @ _PART-BD @ BD.SECTORS 1- U< 0= IF FALSE EXIT THEN
    _GH-BUF @ 56 + _GUID-ZERO? IF FALSE EXIT THEN
    _GH-BUF @ 72 + @ DUP 0= SWAP _PART-BD @ BD.SECTORS U< 0= OR IF
        FALSE EXIT
    THEN
    _GH-BUF @ 80 + L@ DUP 0= SWAP GPT-MAX-ENTRIES > OR IF FALSE EXIT THEN
    _GH-BUF @ 84 + L@ DUP 128 < SWAP GPT-MAX-ENTRY-SIZE > OR IF
        FALSE EXIT
    THEN
    _GH-BUF @ 84 + L@ 7 AND IF FALSE EXIT THEN
    \ UEFI reserves the complete remainder of the logical block, not only
    \ bytes counted by HeaderSize.  Revision 1.0 extensions are therefore
    \ admitted only when their currently reserved bytes remain zero.
    _GH-BUF @ 92 + SECTOR 92 - _BYTES-ZERO? 0= IF
        FALSE EXIT
    THEN
    _GH-BUF @ _GPT-HEADER-CRC? ;

VARIABLE _GPT-FIRST
VARIABLE _GPT-LAST
VARIABLE _GPT-PARRAY
VARIABLE _GPT-BARRAY
VARIABLE _GPT-NENT
VARIABLE _GPT-ESIZE
VARIABLE _GPT-ACRC
VARIABLE _GPT-GUID0
VARIABLE _GPT-GUID1
VARIABLE _GPT-HSIZE
VARIABLE _GPT-ARRAY-BYTES
VARIABLE _GPT-ARRAY-SECTORS

: _GPT-SAVE-PRIMARY  ( header -- )
    DUP 40 + @ _GPT-FIRST !
    DUP 48 + @ _GPT-LAST !
    DUP 56 + @ _GPT-GUID0 !
    DUP 64 + @ _GPT-GUID1 !
    DUP 72 + @ _GPT-PARRAY !
    DUP 80 + L@ _GPT-NENT !
    DUP 84 + L@ _GPT-ESIZE !
    DUP 88 + L@ _GPT-ACRC !
    12 + L@ _GPT-HSIZE !
    _GPT-NENT @ _GPT-ESIZE @ * DUP _GPT-ARRAY-BYTES !
    511 + SECTOR / _GPT-ARRAY-SECTORS ! ;

: _GPT-HEADERS-AGREE?  ( backup-header -- flag )
    DUP 40 + @ _GPT-FIRST @ =
    OVER 48 + @ _GPT-LAST @ = AND
    OVER 56 + @ _GPT-GUID0 @ = AND
    OVER 64 + @ _GPT-GUID1 @ = AND
    OVER 80 + L@ _GPT-NENT @ = AND
    OVER 84 + L@ _GPT-ESIZE @ = AND
    OVER 88 + L@ _GPT-ACRC @ = AND
    OVER 12 + L@ _GPT-HSIZE @ = AND
    SWAP 72 + @ DUP _GPT-BARRAY ! 0<> AND ;

: _GPT-METADATA-VALID?  ( -- flag )
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS BLOCK-RANGE?
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS BLOCK-RANGE? AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _GPT-BARRAY @ _GPT-ARRAY-SECTORS @
        _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ 0 2 _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ 0 2 _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS 1- 1
        _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @ _PART-BD @ BD.SECTORS 1- 1
        _RANGES-OVERLAP? 0= AND
    _GPT-PARRAY @ _GPT-ARRAY-SECTORS @
        _GPT-FIRST @ _GPT-LAST @ _GPT-FIRST @ - 1+
        _RANGES-OVERLAP? 0= AND
    _GPT-BARRAY @ _GPT-ARRAY-SECTORS @
        _GPT-FIRST @ _GPT-LAST @ _GPT-FIRST @ - 1+
        _RANGES-OVERLAP? 0= AND ;

VARIABLE _GP-USED
VARIABLE _GP-OK
: _GPT-PROTECTIVE-MBR?  ( -- flag )
    _PART-WS @ 510 + W@ 0xAA55 <> IF FALSE EXIT THEN
    0 _GP-USED ! TRUE _GP-OK !
    4 0 DO
        I _MBR-ENTRY _MBR-E !
        _MBR-E @ C@ _MBR-BOOT-V !
        _MBR-E @ _MBR-TYPE _MBR-TYPE-V !
        _MBR-E @ _MBR-BASE _MBR-BASE-V !
        _MBR-E @ _MBR-LEN _MBR-LEN-V !
        _MBR-TYPE-V @ 0= IF
            _MBR-BOOT-V @ _MBR-BASE-V @ OR _MBR-LEN-V @ OR IF
                FALSE _GP-OK !
            THEN
        ELSE
            1 _GP-USED +!
            _MBR-BOOT-V @ 0<> IF FALSE _GP-OK ! THEN
            _MBR-TYPE-V @ 0xEE <> IF FALSE _GP-OK ! THEN
            _MBR-BASE-V @ 1 <> IF FALSE _GP-OK ! THEN
            _MBR-LEN-V @ _PART-BD @ BD.SECTORS 1- <> IF FALSE _GP-OK ! THEN
        THEN
    LOOP
    _GP-OK @ _GP-USED @ 1 = AND ;

VARIABLE _GA-LBA
VARIABLE _GA-REM
VARIABLE _GA-CHUNK
VARIABLE _GA-EXPECTED
VARIABLE _GA-RAW

: _GPT-ARRAY-CRC?  ( array-lba expected-crc -- ior )
    _GA-EXPECTED ! _GA-LBA !
    _GPT-ARRAY-BYTES @ _GA-REM !
    0xFFFFFFFF _GA-RAW !
    BEGIN _GA-REM @ 0> WHILE
        _PART-WS @ _GA-LBA @ _PART-READ ?DUP IF EXIT THEN
        _GA-REM @ SECTOR MIN DUP _GA-CHUNK !
        _PART-WS @ SWAP _GA-RAW @ _CRC32-IEEE-RAW?
        DUP IF
            _GPT-CRC-STATUS>IOR >R DROP R> EXIT
        THEN
        DROP _GA-RAW !
        1 _GA-LBA +!
        _GA-CHUNK @ NEGATE _GA-REM +!
    REPEAT
    _GA-RAW @ INVERT 0xFFFFFFFF AND _GA-EXPECTED @ =
    IF 0 ELSE PART-E-CORRUPT THEN ;

: _GPT-ARRAYS-AGREE?  ( -- ior )
    _GPT-ARRAY-BYTES @ _GA-REM !
    _GPT-PARRAY @ _GA-LBA !
    _GPT-BARRAY @ _GA-EXPECTED !
    BEGIN _GA-REM @ 0> WHILE
        _PART-WS @ _GA-LBA @ _PART-READ ?DUP IF EXIT THEN
        _PART-WS @ 512 + _GA-EXPECTED @ _PART-READ ?DUP IF EXIT THEN
        _GA-REM @ SECTOR MIN DUP _GA-CHUNK !
        DUP >R _PART-WS @ SWAP _PART-WS @ 512 + R> COMPARE 0<> IF
            PART-E-CORRUPT EXIT
        THEN
        1 _GA-LBA +!
        1 _GA-EXPECTED +!
        _GA-CHUNK @ NEGATE _GA-REM +!
    REPEAT
    0 ;

VARIABLE _GE-INDEX
VARIABLE _GE-ARRAY
VARIABLE _GE-SECTOR
VARIABLE _GE-INTRA

: _GPT-READ-ENTRY  ( index array-lba -- entry ior )
    _GE-ARRAY ! _GE-INDEX !
    _GE-INDEX @ _GPT-ESIZE @ *
    DUP SECTOR / _GE-ARRAY @ + _GE-SECTOR !
    SECTOR MOD _GE-INTRA !
    _PART-WS @ _GE-SECTOR @ _PART-READ ?DUP IF 0 SWAP EXIT THEN
    _GE-INTRA @ 56 + SECTOR > IF
        _PART-WS @ 512 + _GE-SECTOR @ 1+ _PART-READ ?DUP IF
            0 SWAP EXIT
        THEN
    THEN
    _PART-WS @ _GE-INTRA @ + 0 ;

VARIABLE _GPE-TYPE0
VARIABLE _GPE-TYPE1
VARIABLE _GPE-UNIQ0
VARIABLE _GPE-UNIQ1
VARIABLE _GPE-FIRST
VARIABLE _GPE-LAST
VARIABLE _GPE-LEN
VARIABLE _GPE-ATTR
VARIABLE _GPE-INDEX
VARIABLE _GPC-CONFLICT
VARIABLE _GPC-IOR
VARIABLE _GPC-P

: _GPT-PRIOR-CONFLICT?  ( current-index -- conflict ior )
    0 _GPC-CONFLICT ! 0 _GPC-IOR !
    DUP IF
        0 DO
            I _GPT-PARRAY @ _GPT-READ-ENTRY
            DUP IF
                _GPC-IOR ! DROP LEAVE
            THEN
            DROP                              ( prior-entry )
            DUP _GPC-P !
            DUP _GUID-ZERO? 0= IF
                DUP 16 + @ _GPE-UNIQ0 @ =
                OVER 24 + @ _GPE-UNIQ1 @ = AND IF
                    TRUE _GPC-CONFLICT !
                THEN
                _GPC-P @ 32 + @
                _GPC-P @ 40 + @ _GPC-P @ 32 + @ - 1+
                _GPE-FIRST @ _GPE-LEN @ _RANGES-OVERLAP? IF
                    TRUE _GPC-CONFLICT !
                THEN
            THEN
            DROP
            _GPC-CONFLICT @ IF LEAVE THEN
        LOOP
    ELSE DROP THEN
    _GPC-CONFLICT @ _GPC-IOR @ ;

: _GPT-STAGE-ENTRY  ( table-index -- ior )
    DUP _GPE-INDEX !
    _GPT-PARRAY @ _GPT-READ-ENTRY
    DUP IF NIP EXIT THEN DROP               ( entry )
    DUP _GUID-ZERO? IF DROP 0 EXIT THEN
    DUP @ _GPE-TYPE0 !
    DUP 8 + @ _GPE-TYPE1 !
    DUP 16 + @ _GPE-UNIQ0 !
    DUP 24 + @ _GPE-UNIQ1 !
    _GPE-UNIQ0 @ _GPE-UNIQ1 @ OR 0= IF DROP PART-E-CORRUPT EXIT THEN
    DUP 32 + @ _GPE-FIRST !
    DUP 40 + @ _GPE-LAST !
    48 + @ _GPE-ATTR !
    _GPE-FIRST @ _GPE-LAST @ U> IF PART-E-CORRUPT EXIT THEN
    _GPE-FIRST @ _GPT-FIRST @ U<
    _GPE-LAST @ _GPT-LAST @ U> OR IF PART-E-CORRUPT EXIT THEN
    _GPE-LAST @ _GPE-FIRST @ - 1+ DUP 0= IF DROP PART-E-CORRUPT EXIT THEN
    _GPE-LEN !
    _GPE-INDEX @ _GPT-PRIOR-CONFLICT?
    DUP IF NIP EXIT THEN
    DROP IF PART-E-CORRUPT EXIT THEN
    _PART-COUNT @ _PART-MAX @ >= IF PART-E-CAPACITY EXIT THEN
    _PART-COUNT @ PART-VOLUME DUP /VOLUME 0 FILL
    DUP _GPE-FIRST @ SWAP 48 + !
    DUP _GPE-LEN @ SWAP 56 + !
    DUP VOL-SCHEME-GPT SWAP 80 + !
    DUP _GPE-INDEX @ SWAP 88 + !
    DUP _GPE-TYPE0 @ SWAP 104 + !
    DUP _GPE-TYPE1 @ SWAP 112 + !
    DUP _GPE-UNIQ0 @ SWAP 120 + !
    DUP _GPE-UNIQ1 @ SWAP 128 + !
    _GPE-ATTR @ SWAP 136 + !
    1 _PART-COUNT +!
    0 ;

: _GPT-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    0 _GPT-CRC-IOR !
    _PART-BD @ BD.SECTORS 6 < IF PART-E-CORRUPT _PART-FAIL EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _GPT-PROTECTIVE-MBR? 0= IF PART-E-CORRUPT _PART-FAIL EXIT THEN

    _PART-WS @ 1 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 1 _PART-BD @ BD.SECTORS 1- _GPT-HEADER-VALID? 0= IF
        _GPT-CRC-IOR @ ?DUP IF _PART-FAIL EXIT THEN
        PART-E-CORRUPT _PART-FAIL EXIT
    THEN
    _PART-WS @ _GPT-SAVE-PRIMARY

    _PART-WS @ 512 + _PART-BD @ BD.SECTORS 1- _PART-READ ?DUP IF
        _PART-FAIL EXIT
    THEN
    0 _GPT-CRC-IOR !
    _PART-WS @ 512 + _PART-BD @ BD.SECTORS 1- 1
        _GPT-HEADER-VALID? 0= IF
            _GPT-CRC-IOR @ ?DUP IF _PART-FAIL EXIT THEN
            PART-E-CORRUPT _PART-FAIL EXIT
        THEN
    _PART-WS @ 512 + _GPT-HEADERS-AGREE? 0= IF
        PART-E-CORRUPT _PART-FAIL EXIT
    THEN
    _GPT-METADATA-VALID? 0= IF PART-E-CORRUPT _PART-FAIL EXIT THEN

    _GPT-PARRAY @ _GPT-ACRC @ _GPT-ARRAY-CRC? ?DUP IF _PART-FAIL EXIT THEN
    _GPT-BARRAY @ _GPT-ACRC @ _GPT-ARRAY-CRC? ?DUP IF _PART-FAIL EXIT THEN
    _GPT-ARRAYS-AGREE? ?DUP IF _PART-FAIL EXIT THEN

    _GPT-NENT @ 0 DO
        I _GPT-STAGE-ENTRY ?DUP IF UNLOOP _PART-FAIL EXIT THEN
    LOOP
    _PART-BD @ BD-STALE? IF VOL-E-STALE _PART-FAIL EXIT THEN
    _PART-COUNT @ DUP IF
        0 DO
            I PART-VOLUME _PART-FINALIZE ?DUP IF UNLOOP _PART-FAIL EXIT THEN
        LOOP
    ELSE DROP THEN
    _PART-COUNT @ 0 ;

VARIABLE _PS-USED
VARIABLE _PS-PROTECTIVE

: _PART-RAW-RESULT  ( -- count ior )
    _PART-BD @ 0 PART-VOLUME VOL-RAW ?DUP IF _PART-FAIL EXIT THEN
    1 _PART-COUNT !
    1 0 ;

: _PART-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-SETUP ?DUP IF 0 SWAP EXIT THEN
    _PART-WS @ 0 _PART-READ ?DUP IF _PART-FAIL EXIT THEN
    _PART-WS @ 510 + W@ 0xAA55 <> IF _PART-RAW-RESULT EXIT THEN
    0 _PS-USED ! 0 _PS-PROTECTIVE !
    4 0 DO
        I _MBR-ENTRY _MBR-TYPE DUP IF
            1 _PS-USED +!
            0xEE = IF TRUE _PS-PROTECTIVE ! THEN
        ELSE DROP THEN
    LOOP
    _PS-PROTECTIVE @ IF
        _PART-BD @ _PART-OUT @ _PART-MAX @ _PART-WS @ _PART-BYTES @ _GPT-SCAN
        EXIT
    THEN
    _PS-USED @ 0= IF
        4 0 DO
            I _MBR-ENTRY DUP C@ SWAP DUP _MBR-BASE SWAP _MBR-LEN OR OR IF
                PART-E-CORRUPT UNLOOP _PART-FAIL EXIT
            THEN
        LOOP
        _PART-RAW-RESULT EXIT
    THEN
    _PART-BD @ _PART-OUT @ _PART-MAX @ _PART-WS @ _PART-BYTES @ _MBR-SCAN ;

\ Partition parsing uses fixed scratch variables in addition to the caller's
\ data workspace.  Serialize the complete administrative scan on machine
\ lock 0 (the dictionary lock) while individual I/O continues to use the
\ checked block layer's lock 2.  CATCH guarantees release if a parser defect
\ or caller exception escapes despite the normal structured-error paths.
: _PART-LOCK  ( -- )  BEGIN 0 SPIN@ 0= UNTIL ;
: _PART-UNLOCK  ( -- )  0 SPIN! ;
: MBR-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _MBR-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;
: GPT-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _GPT-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;
: PART-SCAN  ( bd volumes max workspace bytes -- count ior )
    _PART-LOCK ['] _PART-SCAN CATCH _PART-UNLOCK ?DUP IF THROW THEN ;
