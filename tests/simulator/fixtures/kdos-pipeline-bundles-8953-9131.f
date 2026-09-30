\ =====================================================================
\  §15  Pipeline Bundles
\ =====================================================================
\
\  A Pipeline Bundle is a versioned, declarative file (type 7) that
\  packages a complete pipeline configuration in a single artifact:
\
\    - Buffer schemas       (type, width, length)
\    - Kernel registrations (n_in, n_out, footprint, flags)
\    - Pipeline structures  (capacity)
\    - Scheduling config    (pipe index, interval, auto/repeat flags)
\    - Policies             (permissions, retention, export)
\    - Dashboard config     (default screen, screen enable mask)
\
\  Bundles enable fleet deployment (commercial), change control
\  (industrial/civil), and easy sharing/install (personal).
\
\  A bundle file is valid Forth source using BDL-* declarative words.
\  Load with BUNDLE-LOAD, inspect with BUNDLE-INFO.
\
\  Example bundle file (stored as type 7 on disk):
\    1 BDL-BEGIN
\    0 1 256 BDL-BUF my-data
\    1 1 2 1 BDL-KERN my-kern
\    3 BDL-PIPE my-pipe
\    0 50000 3 BDL-SCHED
\    0 0 3 BDL-POLICY
\    1 255 BDL-SCREEN
\    BDL-END

7 CONSTANT FTYPE-BUNDLE

\ -- Bundle tracking state --
VARIABLE BDL-ACTIVE   0 BDL-ACTIVE !
VARIABLE BDL-DRY      0 BDL-DRY !
VARIABLE BDL-VER      0 BDL-VER !
VARIABLE BDL-NBUFS    0 BDL-NBUFS !
VARIABLE BDL-NKERNS   0 BDL-NKERNS !
VARIABLE BDL-NPIPES   0 BDL-NPIPES !
\ Schedule
VARIABLE BDL-SCHED-P  -1 BDL-SCHED-P !
VARIABLE BDL-SCHED-I  0 BDL-SCHED-I !
VARIABLE BDL-SCHED-F  0 BDL-SCHED-F !
\ Policy
VARIABLE BDL-POL-PERM 0 BDL-POL-PERM !
VARIABLE BDL-POL-RET  0 BDL-POL-RET !
VARIABLE BDL-POL-EXP  3 BDL-POL-EXP !
\ Dashboard
VARIABLE BDL-SCR-DEF  1 BDL-SCR-DEF !
VARIABLE BDL-SCR-MASK 255 BDL-SCR-MASK !

\ BDL-RESET ( -- ) clear all bundle tracking state
\   Does NOT touch BDL-DRY — that is managed by BUNDLE-LOAD / BUNDLE-INFO.
: BDL-RESET  ( -- )
    0 BDL-ACTIVE !  0 BDL-VER !
    0 BDL-NBUFS !  0 BDL-NKERNS !  0 BDL-NPIPES !
    -1 BDL-SCHED-P !  0 BDL-SCHED-I !  0 BDL-SCHED-F !
    0 BDL-POL-PERM !  0 BDL-POL-RET !  3 BDL-POL-EXP !
    1 BDL-SCR-DEF !  255 BDL-SCR-MASK ! ;

\ BDL-BEGIN ( version -- ) start a bundle declaration
: BDL-BEGIN  ( version -- )
    BDL-RESET  BDL-VER !  1 BDL-ACTIVE ! ;

\ BDL-BUF ( type width length "name" -- ) declare a buffer schema
: BDL-BUF  ( type width length "name" -- )
    BDL-DRY @ IF
        DROP DROP DROP
        BL WORD DROP
    ELSE
        BUFFER
    THEN
    1 BDL-NBUFS +! ;

\ BDL-KERN ( n_in n_out foot flags "name" -- ) declare a kernel
: BDL-KERN  ( n_in n_out foot flags "name" -- )
    BDL-DRY @ IF
        DROP DROP DROP DROP
        BL WORD DROP
    ELSE
        KERNEL
    THEN
    1 BDL-NKERNS +! ;

\ BDL-PIPE ( capacity "name" -- ) declare a pipeline
: BDL-PIPE  ( capacity "name" -- )
    BDL-DRY @ IF
        DROP
        BL WORD DROP
    ELSE
        PIPELINE
    THEN
    1 BDL-NPIPES +! ;

\ BDL-SCHED ( pipe-idx interval flags -- ) declare scheduling
\   flags: bit0=auto-start  bit1=repeat
: BDL-SCHED  ( pipe-idx interval flags -- )
    BDL-SCHED-F !  BDL-SCHED-I !  BDL-SCHED-P ! ;

\ BDL-POLICY ( permissions retention export -- ) declare policy
\   permissions: bit0=readonly  bit1=system
\   export:      bit0=nic-ok    bit1=disk-ok
: BDL-POLICY  ( permissions retention export -- )
    BDL-POL-EXP !  BDL-POL-RET !  BDL-POL-PERM ! ;

\ BDL-SCREEN ( default-screen screen-mask -- ) declare dashboard config
: BDL-SCREEN  ( default-screen screen-mask -- )
    BDL-SCR-MASK !  BDL-SCR-DEF ! ;

\ BDL-END ( -- ) finalize bundle, report summary
: BDL-END  ( -- )
    BDL-DRY @ IF
        ."  --- Bundle v" BDL-VER @ . ."  ---" CR
        ."    Buffers  : " BDL-NBUFS @ . CR
        ."    Kernels  : " BDL-NKERNS @ . CR
        ."    Pipelines: " BDL-NPIPES @ . CR
        BDL-SCHED-P @ -1 <> IF
            ."    Schedule : pipe " BDL-SCHED-P @ .
            ."   interval=" BDL-SCHED-I @ .
            BDL-SCHED-F @ 1 AND IF ."   auto" THEN
            BDL-SCHED-F @ 2 AND IF ."   repeat" THEN
            CR
        ELSE
            ."    Schedule : (none)" CR
        THEN
        ."    Policy   : perms=" BDL-POL-PERM @ .
        ."  ret=" BDL-POL-RET @ .
        ."  exp=" BDL-POL-EXP @ . CR
        ."    Screen   : default=" BDL-SCR-DEF @ .
        ."  mask=" BDL-SCR-MASK @ . CR
    ELSE
        \ Apply schedule interval to TIME-SLICE
        BDL-SCHED-P @ -1 <> IF
            BDL-SCHED-I @ TIME-SLICE !
        THEN
        \ Apply screen default
        BDL-SCR-DEF @ SCREEN-ID !
        \ Report
        CR ."  Bundle v" BDL-VER @ . ."  loaded: "
        BDL-NBUFS @ . ."  bufs "
        BDL-NKERNS @ . ."  kerns "
        BDL-NPIPES @ . ."  pipes" CR
    THEN
    0 BDL-ACTIVE ! ;

\ BUNDLE-LOAD ( "name" -- ) load and apply a bundle from disk
: BUNDLE-LOAD  ( "name" -- )
    0 BDL-DRY !  LOAD ;

\ BUNDLE-INFO ( "name" -- ) inspect a bundle without applying
: BUNDLE-INFO  ( "name" -- )
    1 BDL-DRY !  LOAD  0 BDL-DRY ! ;

\ .BUNDLE ( -- ) show current bundle state
: .BUNDLE  ( -- )
    ."  --- Current Bundle ---" CR
    BDL-VER @ 0= IF
        ."    (no bundle loaded)" CR EXIT
    THEN
    ."    Version  : " BDL-VER @ . CR
    ."    Buffers  : " BDL-NBUFS @ . CR
    ."    Kernels  : " BDL-NKERNS @ . CR
    ."    Pipelines: " BDL-NPIPES @ . CR
    ."    Schedule : "
    BDL-SCHED-P @ -1 = IF ."  none" ELSE
        ."  pipe " BDL-SCHED-P @ .
        ."  interval=" BDL-SCHED-I @ .
        BDL-SCHED-F @ 1 AND IF ."  [auto]" THEN
        BDL-SCHED-F @ 2 AND IF ."  [repeat]" THEN
    THEN CR
    ."    Policy   : "
    BDL-POL-PERM @ 1 AND IF ."  RO" THEN
    BDL-POL-PERM @ 2 AND IF ."  SYS" THEN
    ."  ret=" BDL-POL-RET @ .
    ."  exp=" BDL-POL-EXP @ . CR
    ."    Screen   : default=" BDL-SCR-DEF @ .
    ."  mask=" BDL-SCR-MASK @ . CR ;

\ =====================================================================
