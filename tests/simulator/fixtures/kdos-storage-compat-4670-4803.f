
\ The KDOS singleton is a raw compatibility/recovery binding.  Akashic VFS
\ instances may bind other validated volume descriptors independently.
CREATE SYSTEM-BD  /BLOCK-DEVICE ALLOT
CREATE SYSTEM-RAW-VOLUME  /VOLUME ALLOT
VARIABLE FS-VOLUME
SYSTEM-RAW-VOLUME FS-VOLUME !

\ Declared here so any stale storage completion can fail a loaded MP64FS
\ closed.  The caches and the rest of MP64FS are declared below.
VARIABLE FS-OK
0 FS-OK !

: STORAGE-OPEN  ( -- ior )
    SYSTEM-RAW-VOLUME VOL-CLOSE DROP
    SYSTEM-BD BD-CLOSE DROP
    SYSTEM-BD BD-OPEN ?DUP IF EXIT THEN
    SYSTEM-BD SYSTEM-RAW-VOLUME VOL-RAW ?DUP IF
        SYSTEM-BD BD-CLOSE DROP
        EXIT
    THEN
    SYSTEM-RAW-VOLUME FS-VOLUME !
    0 ;

: FS-VOLUME!  ( vol -- ior )
    DUP VOL-VALID? 0= IF DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    DUP VOL-STALE? IF DROP VOL-E-STALE EXIT THEN
    FS-VOLUME !
    0 FS-OK !
    0 ;

: STORAGE-ENSURE  ( -- ior )
    FS-VOLUME @ VOL-VALID? 0= IF
        FS-OK @ IF 0 FS-OK ! VOL-E-STALE EXIT THEN
        STORAGE-OPEN EXIT
    THEN
    FS-VOLUME @ VOL-STALE? IF 0 FS-OK ! VOL-E-STALE EXIT THEN
    0 ;

\ Last checked result is retained for diagnostics even when the compatibility
\ wrapper raises ABORT.  A zero ior with a short transfer is an internal
\ contract failure, never a successful KDOS operation.
VARIABLE DISK-IO-STATUS
VARIABLE DISK-IO-COMPLETED
VARIABLE DISK-IO-IOR

: _DISK-XFER-OK?  ( expected completed ior -- flag )
    DISK-IO-IOR !
    DISK-IO-COMPLETED !
    DISK-IO-IOR @ IOR>RAW DISK-IO-STATUS !
    DISK-IO-IOR @ IOR-STALE? IF 0 FS-OK ! THEN
    DISK-IO-IOR @ 0<> IF DROP FALSE EXIT THEN
    DISK-IO-COMPLETED @ =
    DUP 0= IF
        14 DISK-IO-STATUS !
        BD-E-INTERNAL DISK-IO-IOR !
    THEN ;

\ Raw checked wrappers remain available for boot diagnostics and source
\ compatibility.  Production KDOS paths below use FS-VOLUME instead.
: _RAW-DISK-READ?  ( dma lba count -- flag )
    DUP >R DISK-READ-CHECKED
    IOR-FROM-BLOCK-RESULT
    R> -ROT _DISK-XFER-OK? ;

: _RAW-DISK-WRITE?  ( dma lba count -- flag )
    DUP >R DISK-WRITE-CHECKED
    IOR-FROM-BLOCK-RESULT
    R> -ROT _DISK-XFER-OK? ;

: _RAW-DISK-FLUSH?  ( -- flag )
    DISK-FLUSH-CHECKED IOR-FROM-BLOCK-RESULT
    DUP DISK-IO-IOR ! DUP IOR>RAW DISK-IO-STATUS ! 0= ;

: _DISK-READ?  ( dma lba count -- flag )
    DUP >R
    STORAGE-ENSURE ?DUP IF
        >R 2DROP DROP R> R> SWAP 0 SWAP _DISK-XFER-OK? EXIT
    THEN
    FS-VOLUME @ VOL-READ R> -ROT _DISK-XFER-OK? ;

: _DISK-WRITE?  ( dma lba count -- flag )
    DUP >R
    STORAGE-ENSURE ?DUP IF
        >R 2DROP DROP R> R> SWAP 0 SWAP _DISK-XFER-OK? EXIT
    THEN
    FS-VOLUME @ VOL-WRITE R> -ROT _DISK-XFER-OK? ;

: _DISK-FLUSH?  ( -- flag )
    STORAGE-ENSURE ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS ! FALSE EXIT
    THEN
    FS-VOLUME @ VOL-FLUSH
    DUP DISK-IO-IOR !
    DUP IOR>RAW DISK-IO-STATUS !
    DUP IOR-STALE? IF 0 FS-OK ! THEN
    0= ;

: _DISK-READ  ( dma lba count -- )
    _DISK-READ? 0= ABORT" Disk read failed" ;

: _DISK-WRITE  ( dma lba count -- )
    _DISK-WRITE? 0= ABORT" Disk write failed" ;

: _DISK-FLUSH  ( -- )
    _DISK-FLUSH? 0= ABORT" Disk flush failed" ;

\ B.SECTORS ( desc -- n ) number of disk sectors needed for buffer data
: B.SECTORS  ( desc -- n )  B.BYTES SECTOR 1- + SECTOR / ;

\ B.SAVE ( desc sector -- ) save buffer data to disk starting at sector
: B.SAVE  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT SWAP                  ( addr sector nsectors )
    _DISK-WRITE ;

\ B.LOAD ( desc sector -- ) load buffer data from disk starting at sector
: B.LOAD  ( desc sector -- )
    SWAP                      ( sector desc )
    DUP B.DATA                ( sector desc addr )
    SWAP B.SECTORS            ( sector addr nsectors )
    ROT SWAP                  ( addr sector nsectors )
    _DISK-READ ;

\ DISK-INFO ( -- ) print storage device status
: DISK-INFO  ( -- )
    ."  Storage: "
    DISK? IF
        ."  present" CR
    ELSE
        ."  not attached" CR
    THEN ;
