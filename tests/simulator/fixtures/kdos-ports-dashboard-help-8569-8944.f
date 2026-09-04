\ =====================================================================
\  §10  Data Ports — Structures and Binding
\ =====================================================================
\
\  Frame protocol (6-byte header + payload, rides inside UDP on port 9000):
\    +0  u8   SRC_ID       source identifier (0-255)
\    +1  u8   DTYPE        data type (0=raw 1=u8 2=u16 3=u64 4=text 5=cmd)
\    +2  u16  SEQ          sequence number (LE)
\    +4  u16  PAYLOAD_LEN  payload byte count (LE)
\    +6  ...  PAYLOAD      data bytes
\
\  This section defines data structures and port binding only.
\  Transport words (POLL, INGEST, PORT-SEND) are provided by
\  networking.f so the Bank 0 core stays transport-independent.
\
\  Python side: data_sources.py provides SineSource, CounterSource, etc.
\  that inject frames wrapped in ETH+IP+UDP via system.nic.inject_frame().

\ -- Constants --
6 CONSTANT /FRAME-HDR

\ -- Data-port protocol receive buffer (1500 bytes, not a raw L2 frame) --
VARIABLE FRAME-BUF  1499 ALLOT

\ -- Port table: 256 slots, each holds a buffer descriptor addr (0=unbound) --
VARIABLE PORT-TABLE  255 CELLS ALLOT
PORT-TABLE 256 CELLS 0 FILL

\ -- Port registry count (defined early, before §9 TUI) --
\ -- PORT-COUNT, PORT-RX, PORT-DROP already defined before §9 --

\ -- Stats (already defined before §9) --

\ -- Temp for routing --
VARIABLE ROUTE-BUF

\ -- Port binding --
: PORT-SLOT  ( id -- addr )     CELLS PORT-TABLE + ;
: PORT!      ( buf id -- )      DUP PORT-SLOT @ 0= IF 1 PORT-COUNT +! THEN
                                PORT-SLOT ! ;
: PORT@      ( id -- buf|0 )    PORT-SLOT @ ;
: UNPORT     ( id -- )          DUP PORT@ 0<> IF -1 PORT-COUNT +! THEN
                                0 SWAP PORT-SLOT ! ;

\ -- NIC convenience (defined early, before §9 TUI) --

\ -- Frame header accessors (valid after POLL/RECV-FRAME fills FRAME-BUF) --
: FRAME-SRC   ( -- id )    FRAME-BUF C@ ;
: FRAME-TYPE  ( -- type )  FRAME-BUF 1 + C@ ;
: FRAME-SEQ   ( -- seq )   FRAME-BUF 2 + C@  FRAME-BUF 3 + C@ 256 * + ;
: FRAME-LEN   ( -- len )   FRAME-BUF 4 + C@  FRAME-BUF 5 + C@ 256 * + ;
: FRAME-DATA  ( -- addr )  FRAME-BUF /FRAME-HDR + ;

\ -- (RECV-FRAME, ROUTE-FRAME, POLL, INGEST defined in §10.1 after §16) --

\ -- Debug: print last received frame header --
: .FRAME  ( -- )
    ."  src=" FRAME-SRC .
    ."  type=" FRAME-TYPE .
    ."  seq=" FRAME-SEQ .
    ."  len=" FRAME-LEN . CR ;

\ -- List bound ports --
: PORTS  ( -- )
    ."  --- Ports (" PORT-COUNT @ . ."  ) ---" CR
    256 0 DO
        I PORT@ DUP 0<> IF
            ."    src=" I . ."   -> buf @" . CR
        ELSE DROP THEN
    LOOP
    ."    rx=" PORT-RX @ . ."  drop=" PORT-DROP @ . CR ;

\ -- Port stats one-liner --
: PORT-STATS  ( -- )
    ."  ports=" PORT-COUNT @ .
    ."  rx=" PORT-RX @ .
    ."  drop=" PORT-DROP @ . ;

\ =====================================================================
\  §11  Benchmarking
\ =====================================================================
\
\  BENCH and .BENCH are defined in §6 (before P.BENCH needs them).
\  This section is kept as a placeholder for additional benchmark words.

\ =====================================================================
\  §12  Dashboard
\ =====================================================================

: HRULE  ( -- )  60 0 DO 45 EMIT LOOP CR ;
: THIN-RULE  ( -- )  40 0 DO 46 EMIT LOOP CR ;

\ -- Unified memory report --
: .MEM  ( -- )
    ."   Bank 0 (System RAM):" CR
    ."     HERE  = " HERE . CR
    ."     Free  = " SP@ HERE - . ."  bytes (to data stack)" CR
    .HEAP
    .HBW
    .XMEM
    ."   Buffers: " BUF-COUNT @ . CR
    ."   Stack depth: " DEPTH . CR ;

\ MEM-REPORT ( -- )
\   Unified memory status with heap integrity check.
: MEM-REPORT  ( -- )
    CR ." === Memory Report ===" CR
    .HEAP
    .HBW
    .XMEM
    ."  Dict: HERE=" HERE .
    ."  SP=" SP@ .
    ."  gap=" SP@ HERE - . ."  bytes" CR
    ."  Heap integrity: "
    HEAP-VERIFY IF ." OK" ELSE ." CORRUPT" THEN CR ;

\ -- Dashboard --
: DASHBOARD ( -- )
    CR HRULE
    ."   KDOS v1.1 — Kernel Dashboard OS" CR
    HRULE
    .MEM
    CR ."   Cores: " NCORES .
    NCORES 1 > IF ."  (multicore)" ELSE ."  (single-core)" THEN CR
    CR DISK-INFO
    CR BUFFERS
    CR KERNELS
    CR PIPES
    CR TASKS
    CR FILES
    CR PORTS
    CR .PERF
    CR HRULE ;

\ -- Status: quick one-liner --
: STATUS ( -- )
    ."  KDOS v1.1 | cores=" NCORES .
    ."  bufs=" BUF-COUNT @ .
    ."  kerns=" KERN-COUNT @ .
    ."  pipes=" PIPE-COUNT @ .
    ."  tasks=" TASK-COUNT @ .
    ."  files=" FILE-COUNT @ .
    ."  ports=" PORT-COUNT @ .
    ."  disk=" DISK? IF ."  yes" ELSE ."  no" THEN
    ."   HERE=" HERE . CR ;

\ =====================================================================
\  §13  Help System
\ =====================================================================

\ -- Word-specific help lookup --
\ HELP-WORD ( -- )  look up the word already in NAMEBUF
\   1. Check if it exists in the dictionary
\   2. Check for matching doc file on disk
\   3. Suggest WORDS-LIKE for related words

VARIABLE HW-FOUND
VARIABLE HW-CSTR    15 ALLOT    \ counted string for FIND

: HELP-WORD  ( -- )
    CR
    \ Build counted string for FIND from NAMEBUF
    PN-LEN @ HW-CSTR C!
    NAMEBUF HW-CSTR 1+ PN-LEN @ CMOVE
    \ Try to find the word in the dictionary
    HW-CSTR FIND SWAP DROP    ( flag: 0=miss, 1/-1=found )
    DUP HW-FOUND !
    0<> IF
        2 FG ."  Found: " RESET-COLOR
        NAMEBUF .ZSTR ."   — defined in dictionary" CR
    ELSE
        1 FG ."  Not found: " RESET-COLOR
        NAMEBUF .ZSTR ."   — not in dictionary" CR
    THEN
    \ Check for matching doc on disk
    FS-OK @ IF
        FIND-BY-NAME DUP -1 <> IF
            CR ."   Documentation available:" CR
            DUP DIRENT DE.TYPE .FTYPE ."   file: "
            DIRENT .ZSTR CR
            ."   Use: DOC " NAMEBUF .ZSTR ."   or  DESCRIBE " NAMEBUF .ZSTR CR
        ELSE
            DROP
        THEN
    THEN
    \ Show related words
    CR ."   Related words:" CR ."   "
    0                                    \ match count
    LATEST
    BEGIN DUP WHILE
        DUP ENTRY>NAME                   ( count entry na nl )
        NAMEBUF PN-LEN @                 ( count entry na nl pa pl )
        2OVER ICONTAINS? IF
            2 PICK 10 < IF               \ limit output to 10 matches
                TYPE SPACE
                ROT 1+ -ROT
            ELSE
                2DROP
            THEN
        ELSE
            2DROP
        THEN
        ENTRY>LINK
    REPEAT
    DROP
    DUP 0= IF ."  (none)" THEN
    CR ."  (" . ."  related)" CR ;

\ -- Full reference --
: .HELP-ALL  ( -- )
    CR HRULE
    ."   KDOS v1.1 — Quick Reference" CR
    HRULE
    CR ."   BUFFER WORDS:" CR
    ."     0 1 256 BUFFER name   Create 256-byte raw buffer" CR
    ."     buf B.INFO             Show buffer descriptor" CR
    ."     buf B.PREVIEW          Hex dump first tile" CR
    ."     byte buf B.FILL        Fill buffer with byte" CR
    ."     buf B.ZERO             Zero buffer" CR
    ."     buf B.SUM              Sum all bytes (via tile engine)" CR
    ."     buf B.MIN              Minimum byte (via tile engine)" CR
    ."     buf B.MAX              Maximum byte (via tile engine)" CR
    ."     a b c B.ADD            Element-wise add a+b -> c" CR
    ."     a b c B.SUB            Element-wise sub a-b -> c" CR
    ."     n buf B.SCALE          Multiply each byte by n" CR
    ."     BUFFERS                List all buffers" CR
    CR ."   KERNEL WORDS:" CR
    ."     1 1 2 0 KERNEL name   Register kernel metadata" CR
    ."     desc K.INFO            Show kernel descriptor" CR
    ."     KERNELS                List all kernels" CR
    CR ."   SAMPLE KERNELS:" CR
    ."     buf kzero              Zero a buffer" CR
    ."     byte buf kfill         Fill a buffer" CR
    ."     a b c kadd             Add two buffers" CR
    ."     buf ksum               Sum buffer -> stack" CR
    ."     buf kstats             Sum, min, max -> stack" CR
    ."     n buf kscale           Scale buffer by n" CR
    ."     n buf kthresh          Threshold: <n->0, >=n->255" CR
    CR ."   ADVANCED KERNELS:" CR
    ."     lo hi buf kclamp       Clamp bytes to [lo,hi]" CR
    ."     w buf kavg             Moving average (window w)" CR
    ."     buf khistogram         256-bin histogram -> hist-bins" CR
    ."     v HIST@                Query histogram bin v" CR
    ."     .HIST                  Print non-zero histogram bins" CR
    ."     src dst kdelta         Delta encode src -> dst" CR
    ."     buf knorm              Normalize to full 0-255 range" CR
    ."     th src dst kpeak       Peak detect (thresh th)" CR
    ."     buf krms-buf           RMS of buffer -> stack" CR
    ."     a b kcorrelate         Dot product (tile engine)" CR
    ."     c0 c1 c2 buf kconvolve3  3-tap FIR convolution" CR
    ."     buf kinvert            Bitwise invert (255-val)" CR
    ."     val buf kcount         Count matching bytes" CR
    CR ."   PIPELINE WORDS:" CR
    ."     3 PIPELINE name        Create 3-step pipeline" CR
    ."     ' word pipe P.ADD      Append step to pipeline" CR
    ."     pipe P.RUN             Execute all steps" CR
    ."     pipe P.BENCH           Time each step" CR
    ."     pipe P.INFO            Show pipeline descriptor" CR
    ."     pipe P.CLEAR           Reset pipeline" CR
    ."     PIPES                  List all pipelines" CR
    CR ."   STORAGE WORDS:" CR
    ."     DISK?                  Is storage present?" CR
    ."     DISK-INFO              Print storage status" CR
    ."     buf sec B.SAVE         Save buffer to disk" CR
    ."     buf sec B.LOAD         Load buffer from disk" CR
    CR ."   MP64FS FILE SYSTEM:" CR
    ."     FORMAT                 Format disk with MP64FS" CR
    ."     FS-LOAD                Load FS from disk into RAM" CR
    ."     FS-SYNC                Write FS changes to disk" CR
    ."     FS-FREE                Show free disk space" CR
    ."     DIR                    List files on disk" CR
    ."     CATALOG                Detailed file listing" CR
    ."     8 2 MKFILE name        Create file (8 secs, type 2)" CR
    ."     RMFILE name            Delete file from disk" CR
    ."     RENAME old new         Rename a file" CR
    ."     CAT name               Print file to terminal" CR
    ."     OPEN name              Open file -> fdesc" CR
    ."     f FFLUSH               Write metadata to disk" CR
    ."     buf SAVE-BUFFER name   Save buffer to file" CR
    CR ."   FILE I/O:" CR
    ."     10 8 FILE name         Create manual file (legacy)" CR
    ."     addr len f FWRITE      Write bytes (advances cursor)" CR
    ."     addr len f FREAD       Read bytes (advances cursor)" CR
    ."     pos f FSEEK            Set file cursor" CR
    ."     f FREWIND              Reset cursor to 0" CR
    ."     n f FTRUNCATE          Set file size (clamps cursor)" CR
    ."     f FSIZE / f F.INFO     File size / info" CR
    ."     FILES                  List legacy files" CR
    CR ."   MODULE WORDS (CORE 0 ONLY):" CR
    ."     PROVIDED id            Register parsed exact ID (case-sensitive)" CR
    ."     addr len PROVIDED-SPAN Register caller-owned exact ID span" CR
    ."     MODULE? id             Query exact ID -> flag" CR
    ."     REQUIRE path           Load source once via PROVIDED" CR
    ."     MODULES                List exact IDs and count" CR
    CR ."   SCHEDULER WORDS:" CR
    ."     ' word 0 TASK name     Create named task (xt pri)" CR
    ."     xt SPAWN               Spawn anonymous task" CR
    ."     xt BG                  Spawn + run scheduler" CR
    ."     SCHEDULE               Run all ready tasks" CR
    ."     YIELD                  Cooperative yield" CR
    ."     tdesc KILL             Cancel task" CR
    ."     tdesc RESTART          Reset done task to ready" CR
    ."     PREEMPT-ON             Enable timer preemption" CR
    ."     PREEMPT-OFF            Disable timer preemption" CR
    ."     TASKS                  List all tasks" CR
    CR ."   MULTICORE WORDS:" CR
    ."     COREID                 Push current core ID" CR
    ."     NCORES                 Push number of hardware cores" CR
    ."     xt core CORE-RUN       Dispatch XT to secondary core" CR
    ."     core CORE-WAIT         Wait for core to finish" CR
    ."     ALL-CORES-WAIT         Wait for all secondary cores" CR
    ."     BARRIER                Synchronize all cores" CR
    ."     n LOCK                 Acquire spinlock n (busy-wait)" CR
    ."     n UNLOCK               Release spinlock n" CR
    ."     CORES                  Show per-core status" CR
    ."     pipe P.RUN-PAR         Run pipeline in parallel" CR
    ."     pipe P.BENCH-PAR       Benchmark parallel pipeline" CR
    CR ."   DATA PORT WORDS:" CR
    ."     buf id PORT!           Bind source id to buffer" CR
    ."     id UNPORT              Unbind source" CR
    ."     POLL                   Receive & route one frame" CR
    ."     n INGEST               Receive & route n frames" CR
    ."     NET-RX?                Is a NIC frame waiting?" CR
    ."     PORTS                  List port bindings" CR
    ."     .FRAME                 Show last frame header" CR
    CR ."   SCREENS & TOOLS:" CR
    ."     SCREENS                Interactive TUI (1-9, n/p, a, q, r)" CR
    ."     DASHBOARD              Full system overview" CR
    ."     STATUS                 Quick status line" CR
    ."     ' word BENCH           Time word, leave cycles on stack" CR
    ."     ' word .BENCH          Time word and print cycles" CR
    ."     HELP                   Full quick reference" CR
    ."     HELP <word>            Look up a specific word" CR
    CR ."   DOCUMENTATION:" CR
    ."     TOPICS                 List available doc topics" CR
    ."     LESSONS                List available tutorials" CR
    ."     DOC <topic>            Page through documentation" CR
    ."     DESCRIBE <topic>       Show topic by name" CR
    ."     TUTORIAL <name>        Interactive lesson" CR
    CR ."   DICTIONARY SEARCH:" CR
    ."     WORDS-LIKE <pat>       Find words containing pattern" CR
    ."     APROPOS <pat>          Alias for WORDS-LIKE" CR
    ."     n .RECENT              Show last n defined words" CR
    ."     LATEST                 Push most-recent dict entry addr" CR
    ."     entry ENTRY>NAME       Get name (addr len) from entry" CR
    ."     entry ENTRY>LINK       Follow dict link to next entry" CR
    CR ."   PIPELINE BUNDLES:" CR
    ."     1 BDL-BEGIN             Start bundle (version 1)" CR
    ."     0 1 256 BDL-BUF name   Declare buffer in bundle" CR
    ."     1 1 2 1 BDL-KERN name  Declare kernel in bundle" CR
    ."     3 BDL-PIPE name         Declare pipeline in bundle" CR
    ."     0 50000 3 BDL-SCHED    Set schedule (pipe int flags)" CR
    ."     0 0 3 BDL-POLICY       Set policy (perms ret exp)" CR
    ."     1 255 BDL-SCREEN        Set screen (default mask)" CR
    ."     BDL-END                 Finalize bundle" CR
    ."     BUNDLE-LOAD name       Load bundle from disk" CR
    ."     BUNDLE-INFO name       Inspect bundle (dry run)" CR
    ."     .BUNDLE                Show current bundle state" CR
    CR ."   STACK & DIAGNOSTICS:" CR
    ."     n NEEDS                Abort if stack has < n items" CR
    ."     flag ASSERT            Abort if flag is false" CR
    ."     .DEPTH                 Show current stack depth" CR
    CR HRULE ;

\ -- Dispatching HELP --
\ HELP ( "name" | -- )
\   With no argument: show full reference
\   With a word name: look up word-specific info
: HELP  ( -- )
    PARSE-NAME PN-LEN @ 0= IF
        .HELP-ALL
    ELSE
        HELP-WORD
    THEN ;

\ =====================================================================
