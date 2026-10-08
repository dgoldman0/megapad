\ =====================================================================
\  §8.8  Micro-Cluster Support
\ =====================================================================
\
\  High-level words for managing micro-core clusters.  Builds on the
\  BIOS primitives: CLUSTER-EN! CLUSTER-EN@ BARRIER-ARRIVE
\  BARRIER-STATUS SPAD N-FULL MICRO? HBW-BASE HBW-SIZE
\  and KDOS §1 words: MICRO-CORE? FULL-CORE? N-FULL-CORES
\
\  CLUSTER-ENABLE   ( n -- )    enable cluster n (0-based)
\  CLUSTER-DISABLE  ( n -- )    disable cluster n
\  CLUSTERS-ON      ( -- )      enable all 3 clusters
\  CLUSTERS-OFF     ( -- )      disable all clusters
\  CLUSTER-STATE    ( -- )      display cluster enable state
\  HW-BARRIER-WAIT  ( -- )      arrive at hardware barrier, spin until done
\  SPAD-C@          ( off -- c ) read byte from cluster scratchpad
\  SPAD-C!          ( c off -- ) write byte to cluster scratchpad

3 CONSTANT NUM-CLUSTERS

\ CLUSTER-ENABLE ( n -- )  enable cluster n by setting bit n in mask
: CLUSTER-ENABLE  ( n -- )
    DUP 0< OVER NUM-CLUSTERS >= OR ABORT" Invalid cluster ID"
    1 SWAP LSHIFT
    CLUSTER-EN@ OR
    CLUSTER-EN! ;

\ CLUSTER-DISABLE ( n -- )  disable cluster n by clearing bit n
: CLUSTER-DISABLE  ( n -- )
    DUP 0< OVER NUM-CLUSTERS >= OR ABORT" Invalid cluster ID"
    1 SWAP LSHIFT INVERT
    CLUSTER-EN@ AND
    CLUSTER-EN! ;

\ CLUSTERS-ON ( -- )  enable all clusters (mask = 0x07)
: CLUSTERS-ON  ( -- )
    7 CLUSTER-EN! ;

\ CLUSTERS-OFF ( -- )  disable all clusters
: CLUSTERS-OFF  ( -- )
    0 CLUSTER-EN! ;

\ CLUSTER-STATE ( -- )  display cluster enable status
: CLUSTER-STATE  ( -- )
    ."  Clusters: " CLUSTER-EN@ DUP . ."  (mask)" CR
    NUM-CLUSTERS 0 DO
        ."    Cluster " I .
        DUP 1 I LSHIFT AND IF
            ."   ENABLED" CR
        ELSE
            ."   disabled" CR
        THEN
    LOOP DROP ;

\ HW-BARRIER-WAIT ( -- )  arrive and spin until hardware barrier fires
\   Uses the CSR-based barrier (micro-core clusters only).
: HW-BARRIER-WAIT  ( -- )
    BARRIER-ARRIVE
    BEGIN
        BARRIER-STATUS 256 AND 0<>      \ bit 8 = done flag
    UNTIL ;

\ SPAD-C@ ( off -- c )  read byte from cluster scratchpad
: SPAD-C@  ( off -- c )
    SPAD + C@ ;

\ SPAD-C! ( c off -- )  write byte to cluster scratchpad
: SPAD-C!  ( c off -- )
    SPAD + C! ;

\ =====================================================================
\  §8.9  Cluster MPU — Memory Protection for Micro-Cores
\ =====================================================================
\
\  One shared MPU per cluster (not per micro-core) — enforced in the
\  cluster bus arbiter.  MMIO and scratchpad always allowed.
\
\  CL-MPU-SETUP   ( base limit -- )  configure cluster MPU window
\  CL-ENTER-USER  ( -- )             switch cluster to user mode
\  CL-EXIT-USER   ( -- )             return cluster to supervisor mode
\  CL-MPU-OFF     ( -- )             disable cluster MPU (base=limit=0)
\  .CL-MPU        ( -- )             display cluster MPU state

\ CL-MPU-SETUP ( base limit -- )  set cluster MPU window [base, limit)
: CL-MPU-SETUP  ( base limit -- )
    CL-MPU-LIMIT! CL-MPU-BASE! ;

\ CL-ENTER-USER ( -- )  switch cluster privilege to user mode
: CL-ENTER-USER  ( -- )
    1 CL-PRIV! ;

\ CL-EXIT-USER ( -- )  switch cluster back to supervisor mode
: CL-EXIT-USER  ( -- )
    0 CL-PRIV! ;

\ CL-MPU-OFF ( -- )  disable cluster MPU (clear window)
: CL-MPU-OFF  ( -- )
    0 CL-PRIV!
    0 0 CL-MPU-SETUP ;

\ .CL-MPU ( -- )  display cluster MPU configuration
: .CL-MPU  ( -- )
    ."  Cluster MPU:" CR
    ."    priv = " CL-PRIV@ . CR
    ."    base = " CL-MPU-BASE@ HEX U. DECIMAL CR
    ."    limit= " CL-MPU-LIMIT@ HEX U. DECIMAL CR ;

\ -- Forward declarations for §10 words needed by §9 TUI --
