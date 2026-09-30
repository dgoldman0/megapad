
\ ── Loading and syncing ──────────────────────────────────────────────

\ FS-LOAD ( -- ) read superblock + bitmap + directory from disk
: FS-LOAD  ( -- )
    0 FS-OK !
    DISK? 0= IF
        ."  No disk attached" CR EXIT
    THEN
    STORAGE-OPEN ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS !
        ."  Unable to bind storage" CR EXIT
    THEN
    MP64FS-VALID? 0= IF
        ."  Invalid MP64FS" CR EXIT
    THEN
    \ The shared BIOS validator has already checked the marker, geometry,
    \ reserved bitmap, directory entries, parents, extents, and byte bounds.
    \ Read the accepted geometry into KDOS's cache coordinates.
    FS-SUPER 0 1 _DISK-READ
    FS-SUPER 6 + L@ FS-TOTAL !
    FS-SUPER 12 + W@ FS-BMAP-N !
    \ Read the complete geometry-selected bitmap and directory.
    FS-BMAP 1 FS-BMAP-N @ _DISK-READ
    FS-DIR FS-DIR-START 12 _DISK-READ
    -1 FS-OK !
    ."  MP64FS loaded" CR ;

\ FS-SYNC ( -- ) write bitmap + directory back to disk
: FS-SYNC  ( -- )
    FS-OK @ 0= IF ."  FS not loaded" CR EXIT THEN
    FS-BMAP 1 FS-BMAP-N @ _DISK-WRITE
    FS-DIR FS-DIR-START 12 _DISK-WRITE
    _DISK-FLUSH ;

\ FS-ENSURE ( -- ) auto-load if not yet loaded
: FS-ENSURE  ( -- )
    FS-OK @ 0= IF
        DISK? IF FS-LOAD THEN
    THEN ;

\ ── FORMAT ───────────────────────────────────────────────────────────

\ FORMAT ( -- ) initialise a fresh MP64FS on the attached disk
: FORMAT  ( -- )
    0 FS-OK !
    DISK? 0= IF ."  No disk" CR EXIT THEN
    STORAGE-OPEN ?DUP IF
        DUP DISK-IO-IOR ! IOR>RAW DISK-IO-STATUS !
        ."  Unable to bind storage" CR EXIT
    THEN
    FS-VOLUME @ VOL.SECTORS DUP 15 < OVER FS-MAX-SECTORS > OR IF
        DROP ."  Unsupported disk size" CR EXIT
    THEN
    FS-TOTAL !
    FS-TOTAL @ 4095 + 4096 / FS-BMAP-N !
    \ Build superblock in RAM
    FS-SUPER SECTOR 0 FILL
    77 FS-SUPER     C!              \ 'M'
    80 FS-SUPER 1+  C!              \ 'P'
    54 FS-SUPER 2 + C!              \ '6'
    52 FS-SUPER 3 + C!              \ '4'
    1              FS-SUPER 4  + W! \ format marker
    FS-TOTAL @     FS-SUPER 6  + L! \ total sectors (u32)
    1               FS-SUPER 10 + W!
    FS-BMAP-N @     FS-SUPER 12 + W!
    FS-DIR-START    FS-SUPER 14 + W!
    12              FS-SUPER 16 + W!
    FS-DSTART       FS-SUPER 18 + W!
    128  FS-SUPER 20 + C!           \ max files
    48   FS-SUPER 21 + C!           \ entry size
    FS-SUPER 0 1 _DISK-WRITE
    \ Initialise bitmap — mark every sector below FS-DSTART as metadata
    FS-BMAP FS-BMAP-N @ SECTOR * 0 FILL
    FS-DSTART 0 DO I BIT-SET LOOP
    FS-BMAP 1 FS-BMAP-N @ _DISK-WRITE
    \ Zero directory
    FS-DIR SECTOR 12 * 0 FILL
    FS-DIR FS-DIR-START 12 _DISK-WRITE
    _DISK-FLUSH
    -1 FS-OK !
    255 CWD !
    ."  MP64FS formatted" CR ;
