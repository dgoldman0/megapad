
\ =====================================================================
\  §7  Storage & Persistence
\ =====================================================================
\
\  Production I/O uses the BIOS checked block words.  The raw setup/command
\  words remain BIOS diagnostics and are not used by KDOS filesystem paths.
\  Storage device: 512-byte sectors, DMA to/from RAM.
\
\  B.SAVE / B.LOAD persist buffers by writing their data region to disk.
\  Buffer data is tile-aligned (64 bytes), sectors are 512 bytes.
\  A buffer that is N tiles writes ceil(N*64/512) sectors.

512 CONSTANT SECTOR

\ DISK? ( -- flag ) true if storage device present
: DISK?  ( -- flag )  DISK@ 128 AND 0<> ;

\ ---------------------------------------------------------------------
\  Explicit block-device and bounded-volume objects
\ ---------------------------------------------------------------------
\
\  These descriptors are the storage ABI consumed by filesystems.  The ABI
\  marker is permanent identity/capability validation, not a second legacy
\  implementation.  Controller generation-guarded commands make the saved
\  MEDIA_GEN an atomic acceptance condition: a stale descriptor cannot touch
\  replacement media in the race between a software check and submission.

0x31305645444B4C42 CONSTANT BLOCK-DEVICE-MAGIC   \ "BLKDEV01"
0x3130454D554C4F56 CONSTANT VOLUME-MAGIC         \ "VOLUME01"
1 CONSTANT STORAGE-ABI
128 CONSTANT /BLOCK-DEVICE
144 CONSTANT /VOLUME

\ Structured storage ior (low 32 bits):
\   7..0 raw controller cause, 15..8 stable code, 23..16 domain,
\   31..24 flags.  Zero alone is success.
1 CONSTANT IOR-D-BLOCK
2 CONSTANT IOR-D-DEVICE
3 CONSTANT IOR-D-VOLUME
4 CONSTANT IOR-D-PARTITION

1  CONSTANT IOR-F-PARTIAL
2  CONSTANT IOR-F-RETRYABLE
4  CONSTANT IOR-F-STALE
8  CONSTANT IOR-F-CORRUPT
16 CONSTANT IOR-F-UNSUPPORTED
32 CONSTANT IOR-F-READONLY

16 CONSTANT IOR-C-BAD-DESCRIPTOR
17 CONSTANT IOR-C-STALE
18 CONSTANT IOR-C-RANGE
19 CONSTANT IOR-C-READONLY
20 CONSTANT IOR-C-CORRUPT
21 CONSTANT IOR-C-CAPACITY
22 CONSTANT IOR-C-WORKSPACE
23 CONSTANT IOR-C-UNSUPPORTED
24 CONSTANT IOR-C-BUSY

: IOR-MAKE  ( raw code domain flags -- ior )
    24 LSHIFT  SWAP 16 LSHIFT OR  SWAP 8 LSHIFT OR  OR ;
: IOR>RAW     ( ior -- u )  0xFF AND ;
: IOR>CODE    ( ior -- u )  8 RSHIFT 0xFF AND ;
: IOR>DOMAIN  ( ior -- u )  16 RSHIFT 0xFF AND ;
: IOR>FLAGS   ( ior -- u )  24 RSHIFT 0xFF AND ;
: IOR-PARTIAL?  ( ior -- flag )  IOR>FLAGS IOR-F-PARTIAL AND 0<> ;
: IOR-STALE?    ( ior -- flag )  IOR>FLAGS IOR-F-STALE AND 0<> ;

14 IOR-C-BAD-DESCRIPTOR IOR-D-DEVICE 0
    IOR-MAKE CONSTANT BD-E-BAD-DESCRIPTOR
1  1 IOR-D-DEVICE 0 IOR-MAKE CONSTANT BD-E-NO-MEDIA
2  IOR-C-UNSUPPORTED IOR-D-DEVICE IOR-F-UNSUPPORTED
    IOR-MAKE CONSTANT BD-E-UNSUPPORTED
11 IOR-C-STALE IOR-D-DEVICE IOR-F-STALE
    IOR-MAKE CONSTANT BD-E-STALE
14 14 IOR-D-BLOCK 0 IOR-MAKE CONSTANT BD-E-INTERNAL
4  IOR-C-RANGE IOR-D-BLOCK 0 IOR-MAKE CONSTANT BD-E-RANGE
8  IOR-C-READONLY IOR-D-BLOCK IOR-F-READONLY
    IOR-MAKE CONSTANT BD-E-READONLY
14 IOR-C-BUSY IOR-D-DEVICE 0 IOR-MAKE CONSTANT BD-E-BUSY

14 IOR-C-BAD-DESCRIPTOR IOR-D-VOLUME 0
    IOR-MAKE CONSTANT VOL-E-BAD-DESCRIPTOR
11 IOR-C-STALE IOR-D-VOLUME IOR-F-STALE
    IOR-MAKE CONSTANT VOL-E-STALE
4  IOR-C-RANGE IOR-D-VOLUME 0 IOR-MAKE CONSTANT VOL-E-RANGE
8  IOR-C-READONLY IOR-D-VOLUME IOR-F-READONLY
    IOR-MAKE CONSTANT VOL-E-READONLY

\ Translate a checked BIOS result.  The controller partial bit becomes an
\ explicit flag while the low byte retains the unmodified cause.
: IOR-FROM-BLOCK-RESULT  ( status -- ior )
    DUP 0= IF EXIT THEN
    DUP 128 AND IF IOR-F-PARTIAL ELSE 0 THEN
    SWAP 127 AND                         ( flags raw )
    DUP 7 = OVER 10 = OR IF
        SWAP IOR-F-RETRYABLE OR SWAP
    THEN
    DUP 11 = IF SWAP IOR-F-STALE OR SWAP THEN
    DUP IOR-D-BLOCK 3 ROLL IOR-MAKE ;

\ Descriptor field readers.  Fields not exposed here remain diagnostic.
: BD.COOKIE      ( bd -- u )  16 + @ ;
: BD.MEDIA-GEN   ( bd -- u )  40 + @ ;
: BD.SECTOR-SIZE ( bd -- u )  48 + @ ;
: BD.SECTORS     ( bd -- u )  56 + @ ;
: BD.CAPS        ( bd -- u )  64 + @ ;
: BD.FLAGS       ( bd -- u )  72 + @ ;
: BD.REFS        ( bd -- a )  88 + ;

: VOL.COOKIE      ( vol -- u )  16 + @ ;
: VOL.BD          ( vol -- bd ) 24 + @ ;
: VOL.BD-COOKIE   ( vol -- u )  32 + @ ;
: VOL.MEDIA-GEN   ( vol -- u )  40 + @ ;
: VOL.BASE        ( vol -- u )  48 + @ ;
: VOL.SECTORS     ( vol -- u )  56 + @ ;
: VOL.SECTOR-SIZE ( vol -- u )  64 + @ ;
: VOL.FLAGS       ( vol -- u )  72 + @ ;
: VOL.SCHEME      ( vol -- u )  80 + @ ;
: VOL.INDEX       ( vol -- u )  88 + @ ;

0 CONSTANT VOL-SCHEME-RAW
1 CONSTANT VOL-SCHEME-MBR
2 CONSTANT VOL-SCHEME-GPT
1 CONSTANT VOL-F-READONLY

\ Overflow-safe interval validation: count is nonzero and
\ lba <= length-count.  All inputs are treated as unsigned cells.
: BLOCK-RANGE?  ( lba count length -- flag )
    >R
    DUP 0= IF 2DROP R> DROP FALSE EXIT THEN
    DUP R@ U> IF 2DROP R> DROP FALSE EXIT THEN
    R> SWAP - U> 0= ;

VARIABLE STORAGE-COOKIE
0 STORAGE-COOKIE !
: STORAGE-COOKIE-NEXT  ( -- u )
    1 STORAGE-COOKIE +!
    STORAGE-COOKIE @ DUP 0= IF DROP 1 STORAGE-COOKIE +! STORAGE-COOKIE @ THEN ;

\ Required controller facilities: READ, precise result, completion counter,
\ media generation, and atomic generation guard.  WRITE/FLUSH remain
\ discoverable capabilities so a future read-only backend can still open.
0x79 CONSTANT BD-REQUIRED-CAPS

: BD-VALID?  ( bd -- flag )
    DUP 0= IF DROP FALSE EXIT THEN
    DUP @ BLOCK-DEVICE-MAGIC =
    OVER 8 + @ STORAGE-ABI = AND
    OVER BD.COOKIE 0<> AND
    OVER BD.SECTOR-SIZE SECTOR = AND
    OVER BD.SECTORS 0<> AND
    SWAP BD.CAPS BD-REQUIRED-CAPS AND BD-REQUIRED-CAPS = AND ;

: BD-STALE?  ( bd -- flag )
    DUP BD-VALID? 0= IF DROP TRUE EXIT THEN
    DISK? 0= IF DROP TRUE EXIT THEN
    BD.MEDIA-GEN DISK-MEDIA-GEN <> ;

: BD-OPEN  ( bd -- ior )
    DUP BD-VALID? IF
        DUP BD.REFS @ IF DROP BD-E-BUSY EXIT THEN
    THEN
    DUP /BLOCK-DEVICE 0 FILL
    DISK? 0= IF DROP BD-E-NO-MEDIA EXIT THEN
    DISK-CAPS BD-REQUIRED-CAPS AND BD-REQUIRED-CAPS <> IF
        DROP BD-E-UNSUPPORTED EXIT
    THEN
    STORAGE-ABI OVER 8 + !
    STORAGE-COOKIE-NEXT OVER 16 + !
    1 OVER 24 + !                    \ backend kind: Megapad controller
    1 OVER 32 + !                    \ controller/backend identity
    DISK-MEDIA-GEN OVER 40 + !
    SECTOR OVER 48 + !
    DISK-SECTORS DUP 0= IF
        DROP /BLOCK-DEVICE 0 FILL BD-E-NO-MEDIA EXIT
    THEN
    OVER 56 + !
    DISK-CAPS OVER 64 + !
    0
    DISK-CAPS 2 AND 0= IF VOL-F-READONLY OR THEN
    DISK@ 32 AND IF VOL-F-READONLY OR THEN
    OVER 72 + !
    1 OVER 80 + !                    \ synchronization kind: FS lock/controller
    DISK-MEDIA-GEN OVER BD.MEDIA-GEN <> IF
        /BLOCK-DEVICE 0 FILL BD-E-STALE EXIT
    THEN
    BLOCK-DEVICE-MAGIC SWAP !
    0 ;

: BD-CLOSE  ( bd -- ior )
    DUP BD-VALID? 0= IF DROP 0 EXIT THEN
    DUP BD.REFS @ IF DROP BD-E-BUSY EXIT THEN
    /BLOCK-DEVICE 0 FILL 0 ;

: _BD-CHECK  ( lba count bd -- ior )
    >R
    R@ BD-VALID? 0= IF 2DROP R> DROP BD-E-BAD-DESCRIPTOR EXIT THEN
    R@ BD-STALE? IF 2DROP R> DROP BD-E-STALE EXIT THEN
    2DUP R@ BD.SECTORS BLOCK-RANGE? 0= IF
        2DROP R> DROP BD-E-RANGE EXIT
    THEN
    2DROP R> DROP 0 ;

: BD-READ  ( dma lba count bd -- completed ior )
    2 PICK 2 PICK 2 PICK _BD-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R
    2 PICK R@ 112 + !
    OVER R@ 120 + !
    BD.MEDIA-GEN DISK-READ-GEN-CHECKED
    DUP IOR-FROM-BLOCK-RESULT SWAP DROP
    DUP R@ 96 + !
    OVER R@ 104 + !
    R> DROP ;

: BD-WRITE  ( dma lba count bd -- completed ior )
    DUP BD-VALID? IF
        DUP BD.FLAGS VOL-F-READONLY AND IF
            2DROP 2DROP 0 BD-E-READONLY EXIT
        THEN
    THEN
    2 PICK 2 PICK 2 PICK _BD-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R
    2 PICK R@ 112 + !
    OVER R@ 120 + !
    BD.MEDIA-GEN DISK-WRITE-GEN-CHECKED
    DUP IOR-FROM-BLOCK-RESULT SWAP DROP
    DUP R@ 96 + !
    OVER R@ 104 + !
    R> DROP ;

: BD-FLUSH  ( bd -- ior )
    DUP BD-VALID? 0= IF DROP BD-E-BAD-DESCRIPTOR EXIT THEN
    DUP BD-STALE? IF DROP BD-E-STALE EXIT THEN
    DUP BD.CAPS 4 AND 0= IF DROP BD-E-UNSUPPORTED EXIT THEN
    DUP >R BD.MEDIA-GEN DISK-FLUSH-GEN-CHECKED
    IOR-FROM-BLOCK-RESULT
    DUP R@ 96 + !
    0 R@ 104 + !
    R> DROP ;

: VOL-VALID?  ( vol -- flag )
    DUP 0= IF DROP FALSE EXIT THEN
    >R
    R@ @ VOLUME-MAGIC =
    R@ 8 + @ STORAGE-ABI = AND
    R@ VOL.COOKIE 0<> AND
    R@ VOL.SECTOR-SIZE SECTOR = AND
    R@ VOL.SECTORS 0<> AND
    R@ VOL.BD BD-VALID? AND
    R@ VOL.BD-COOKIE R@ VOL.BD BD.COOKIE = AND
    R@ VOL.BASE R@ VOL.SECTORS R@ VOL.BD BD.SECTORS BLOCK-RANGE? AND
    R> DROP ;

: VOL-STALE?  ( vol -- flag )
    DUP VOL-VALID? 0= IF DROP TRUE EXIT THEN
    DUP VOL.MEDIA-GEN OVER VOL.BD BD.MEDIA-GEN <> IF DROP TRUE EXIT THEN
    VOL.BD BD-STALE? ;

: _VOL-CLEAR  ( vol -- )
    DUP VOL-VALID? IF
        DUP VOL.BD DUP BD-VALID? IF -1 SWAP BD.REFS +! ELSE DROP THEN
    THEN
    /VOLUME 0 FILL ;

VARIABLE _VOL-BD
VARIABLE _VOL-PTR
VARIABLE _VOL-BASE
VARIABLE _VOL-LEN
VARIABLE _VOL-SCHEME
VARIABLE _VOL-INDEX

\ VOL-SLICE validates and constructs one bounded slice.  Partition-specific
\ type/GUID fields may be filled only after an entire table validates.
: VOL-SLICE  ( base length scheme index bd vol -- ior )
    _VOL-PTR ! _VOL-BD ! _VOL-INDEX ! _VOL-SCHEME ! _VOL-LEN ! _VOL-BASE !
    _VOL-BD @ BD-VALID? 0= IF VOL-E-BAD-DESCRIPTOR EXIT THEN
    _VOL-BD @ BD-STALE? IF VOL-E-STALE EXIT THEN
    _VOL-BASE @ _VOL-LEN @ _VOL-BD @ BD.SECTORS BLOCK-RANGE? 0= IF
        VOL-E-RANGE EXIT
    THEN
    _VOL-PTR @ _VOL-CLEAR
    _VOL-PTR @ /VOLUME 0 FILL
    STORAGE-ABI _VOL-PTR @ 8 + !
    STORAGE-COOKIE-NEXT _VOL-PTR @ 16 + !
    _VOL-BD @ _VOL-PTR @ 24 + !
    _VOL-BD @ BD.COOKIE _VOL-PTR @ 32 + !
    _VOL-BD @ BD.MEDIA-GEN _VOL-PTR @ 40 + !
    _VOL-BASE @ _VOL-PTR @ 48 + !
    _VOL-LEN @ _VOL-PTR @ 56 + !
    _VOL-BD @ BD.SECTOR-SIZE _VOL-PTR @ 64 + !
    _VOL-BD @ BD.FLAGS _VOL-PTR @ 72 + !
    _VOL-SCHEME @ _VOL-PTR @ 80 + !
    _VOL-INDEX @ _VOL-PTR @ 88 + !
    VOLUME-MAGIC _VOL-PTR @ !
    1 _VOL-BD @ BD.REFS +!
    0 ;

VARIABLE _VR-BD
VARIABLE _VR-VOL
: VOL-RAW  ( bd vol -- ior )
    _VR-VOL ! _VR-BD !
    0 _VR-BD @ BD.SECTORS VOL-SCHEME-RAW 0 _VR-BD @ _VR-VOL @ VOL-SLICE ;

: VOL-CLOSE  ( vol -- ior )
    _VOL-CLEAR 0 ;

: _VOL-CHECK  ( lba count vol -- ior )
    >R
    R@ VOL-VALID? 0= IF 2DROP R> DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    R@ VOL-STALE? IF 2DROP R> DROP VOL-E-STALE EXIT THEN
    2DUP R@ VOL.SECTORS BLOCK-RANGE? 0= IF
        2DROP R> DROP VOL-E-RANGE EXIT
    THEN
    2DROP R> DROP 0 ;

: VOL-READ  ( dma lba count vol -- completed ior )
    2 PICK 2 PICK 2 PICK _VOL-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R DROP
    SWAP R@ VOL.BASE + SWAP R@ VOL.BD BD-READ
    R> DROP ;

: VOL-WRITE  ( dma lba count vol -- completed ior )
    DUP VOL-VALID? IF
        DUP VOL.FLAGS VOL-F-READONLY AND IF
            2DROP 2DROP 0 VOL-E-READONLY EXIT
        THEN
    THEN
    2 PICK 2 PICK 2 PICK _VOL-CHECK ?DUP IF
        >R 2DROP 2DROP 0 R> EXIT
    THEN
    DUP >R DROP
    SWAP R@ VOL.BASE + SWAP R@ VOL.BD BD-WRITE
    R> DROP ;

: VOL-FLUSH  ( vol -- ior )
    DUP VOL-VALID? 0= IF DROP VOL-E-BAD-DESCRIPTOR EXIT THEN
    DUP VOL-STALE? IF DROP VOL-E-STALE EXIT THEN
    VOL.BD BD-FLUSH ;
