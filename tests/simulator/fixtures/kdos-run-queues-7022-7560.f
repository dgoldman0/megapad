\ =====================================================================
\  §8.2  Per-Core Run Queues
\ =====================================================================
\
\  Each core has its own circular queue of task XTs.  Tasks can be
\  enqueued on any core and dequeued/dispatched independently.
\  Core 0 runs dequeued tasks locally; secondary cores receive
\  tasks via CORE-RUN (IPI dispatch).
\
\  Queue layout: circular buffer with head/tail indices per core.
\  RQ-DEPTH entries per core, NCORES_MAX (16) cores.

\ -- Constants --
8 CONSTANT RQ-DEPTH       \ max tasks per core queue
16 CONSTANT NCORES_MAX    \ maximum cores (4 full + 12 micro)

\ -- Per-core queue storage --
\    RQ-SLOTS: NCORES_MAX × RQ-DEPTH × CELL = 16×8×8 = 1024 bytes
\    Each slot holds an XT (0 = empty).
VARIABLE RQ-SLOTS  1023 ALLOT

\ -- Per-core head/tail indices (one CELL each per core) --
\    HEAD = next slot to dequeue from
\    TAIL = next slot to enqueue into
VARIABLE RQ-HEADS  127 ALLOT      \ 16 × 8 = 128 bytes
VARIABLE RQ-TAILS  127 ALLOT      \ 16 × 8 = 128 bytes

\ -- Queue initialisation --
: RQ-INIT  ( -- )
    NCORES_MAX 0 DO
        0 I CELLS RQ-HEADS + !
        0 I CELLS RQ-TAILS + !
        RQ-DEPTH 0 DO
            0  J RQ-DEPTH * I + CELLS RQ-SLOTS + !
        LOOP
    LOOP ;

RQ-INIT       \ initialise at load time

\ -- Slot address: ( slot core -- addr ) --
: RQ-SLOT  ( slot core -- addr )
    RQ-DEPTH * + CELLS RQ-SLOTS + ;

\ -- RQ-COUNT ( core -- n ) number of enqueued tasks --
: RQ-COUNT  ( core -- n )
    DUP CELLS RQ-TAILS + @          ( core tail )
    SWAP CELLS RQ-HEADS + @         ( tail head )
    - DUP 0< IF RQ-DEPTH + THEN ;

\ -- RQ-EMPTY? ( core -- flag ) true if core's queue is empty --
: RQ-EMPTY?  ( core -- flag )
    RQ-COUNT 0= ;

\ -- RQ-FULL? ( core -- flag ) true if core's queue is full --
: RQ-FULL?  ( core -- flag )
    RQ-COUNT RQ-DEPTH 1- >= ;

\ -- RQ-PUSH ( xt core -- ) enqueue a task XT onto a core's queue --
: RQ-PUSH  ( xt core -- )
    DUP RQ-FULL? ABORT" Run queue full"
    DUP CELLS RQ-TAILS + @          ( xt core tail )
    2 PICK                           ( xt core tail xt )
    DROP                             ( xt core tail )
    OVER                             ( xt core tail core )
    RQ-SLOT                          ( xt core slot-addr )
    ROT SWAP !                       ( core )  \ store xt at slot
    DUP CELLS RQ-TAILS + @          ( core tail )
    1+ RQ-DEPTH MOD                  ( core new-tail )
    SWAP CELLS RQ-TAILS + ! ;

\ -- RQ-POP ( core -- xt | 0 ) dequeue next XT from a core's queue --
: RQ-POP  ( core -- xt | 0 )
    DUP RQ-EMPTY? IF DROP 0 EXIT THEN
    DUP CELLS RQ-HEADS + @          ( core head )
    OVER                             ( core head core )
    RQ-SLOT @                        ( core xt )
    SWAP                             ( xt core )
    DUP CELLS RQ-HEADS + @          ( xt core head )
    1+ RQ-DEPTH MOD                  ( xt core new-head )
    SWAP CELLS RQ-HEADS + ! ;        ( xt )

\ -- RQ-CLEAR ( core -- ) clear a core's queue --
: RQ-CLEAR  ( core -- )
    DUP CELLS RQ-HEADS + 0 SWAP !
    CELLS RQ-TAILS + 0 SWAP ! ;

\ -- SCHED-CORE ( core -- ) dispatch all queued tasks on a core --
\   Core 0: runs locally via EXECUTE.
\   Cores 1-N: dispatches via CORE-RUN, waits for each to finish.
: SCHED-CORE  ( core -- )
    BEGIN
        DUP RQ-EMPTY? 0=
    WHILE
        DUP RQ-POP                   ( core xt )
        OVER 0= IF
            EXECUTE                  \ core 0: run locally
        ELSE
            OVER CORE-RUN            \ secondary: dispatch via IPI
            DUP CORE-WAIT            \ wait for completion
        THEN
    REPEAT
    DROP ;

\ -- SCHED-ALL ( -- ) dispatch tasks from all core queues --
\   Dispatches secondary cores first (so they run in parallel),
\   then drains core 0's queue locally.
: SCHED-ALL  ( -- )
    \ First pass: kick one task to each secondary core
    NCORES 1 DO
        I RQ-EMPTY? 0= IF
            I RQ-POP I CORE-RUN      \ dispatch first task
        THEN
    LOOP
    \ Drain core 0's queue locally while secondaries work
    0 SCHED-CORE
    \ Wait for all secondary cores and drain their remaining tasks
    NCORES 1 DO
        I CORE-WAIT                  \ wait for current task
        I SCHED-CORE                 \ drain remaining tasks
    LOOP ;

\ -- RQ-INFO ( -- ) display per-core queue status --
: RQ-INFO  ( -- )
    ."  --- Run Queues ---" CR
    NCORES 0 DO
        ."    Core " I . ."  : "
        I RQ-COUNT . ."  task(s)"
        I RQ-EMPTY? IF ."   [empty]" THEN
        CR
    LOOP ;

\ =====================================================================
\  §8.3  Work Stealing
\ =====================================================================
\
\  When a core's run queue is empty, it can "steal" a task from the
\  busiest core's queue.  This balances load automatically without
\  manual task placement.
\
\  STEAL-FROM   ( victim thief -- flag )  steal one task
\  RQ-BUSIEST   ( exclude -- core | -1 )  find core with most tasks
\  WORK-STEAL   ( core -- flag )          try to steal for a core
\  BALANCE      ( -- )                    rebalance all queues

\ -- STEAL-FROM ( victim thief -- flag ) steal one task from victim to thief --
\   Returns true (-1) if a task was stolen, false (0) otherwise.
: STEAL-FROM  ( victim thief -- flag )
    OVER RQ-EMPTY? IF 2DROP 0 EXIT THEN
    SWAP RQ-POP                       ( thief xt )
    DUP 0= IF 2DROP 0 EXIT THEN
    SWAP RQ-PUSH  -1 ;               ( flag )

\ -- RQ-BUSIEST ( exclude -- core | -1 ) find full core with most queued tasks --
\   Skips the core with ID 'exclude' and micro-cores.
\   Returns -1 if all full-core queues empty.
: RQ-BUSIEST  ( exclude -- core | -1 )
    -1                                ( exclude best-core )
    0                                 ( exclude best-core best-count )
    N-FULL-CORES 0 DO
        I 3 PICK = IF                \ skip excluded core
        ELSE
            I RQ-COUNT DUP           ( excl bc bcnt cnt cnt )
            2 PICK > IF              ( excl bc bcnt cnt )
                NIP NIP              ( excl cnt )
                I SWAP               ( excl new-core cnt )
            ELSE DROP THEN
        THEN
    LOOP
    DROP                              ( exclude best-core )
    NIP ;                             ( best-core )

\ -- WORK-STEAL ( core -- flag ) try to steal one task for core --
\   Finds the busiest other core and steals one task from it.
\   Returns true if a task was stolen.
: WORK-STEAL  ( core -- flag )
    DUP RQ-BUSIEST                    ( core victim )
    DUP -1 = IF 2DROP 0 EXIT THEN    \ no tasks anywhere
    DUP RQ-EMPTY? IF 2DROP 0 EXIT THEN
    SWAP STEAL-FROM ;                 ( flag )

\ -- BALANCE ( -- ) rebalance work across all full cores --
\   Underloaded full cores steal from the busiest full core, one task at a time,
\   until no more imbalance exists (max difference ≤ 1).
: BALANCE  ( -- )
    \ Repeat until stable
    BEGIN
        0                             ( stole-any? )
        N-FULL-CORES 0 DO
            I RQ-BUSIEST DUP -1 <> IF
                \ Move one task only when the busiest victim exceeds this
                \ queue by more than one.  Each move strictly reduces the
                \ imbalance, including when work is sparser than the core
                \ set, so singleton tasks cannot circulate forever.
                DUP RQ-COUNT I RQ-COUNT - 1 > IF
                    I STEAL-FROM IF
                        DROP -1       \ mark that we stole something
                    THEN
                ELSE DROP THEN
            ELSE DROP THEN
        LOOP
        0=                            ( stop if nothing was stolen )
    UNTIL ;

\ -- SCHED-BALANCED ( -- ) balance then dispatch all --
: SCHED-BALANCED  ( -- )
    BALANCE SCHED-ALL ;

\ =====================================================================
\  §8.4  Core Affinity
\ =====================================================================
\
\  Pin tasks to specific cores.  An affinity table maps task slots
\  (from the §8 TASK-TABLE) to a preferred core.  -1 means "any core"
\  (no affinity).  SPAWN-ON creates a task directly on a core's queue.

VARIABLE AFF-TABLE  63 ALLOT

: AFF-INIT  ( -- )
    8 0 DO
        -1  I CELLS AFF-TABLE + !
    LOOP ;

AFF-INIT

: AFFINITY!  ( core task# -- )
    DUP 8 >= ABORT" Invalid task slot"
    CELLS AFF-TABLE + ! ;

: AFFINITY@  ( task# -- core )
    DUP 8 >= ABORT" Invalid task slot"
    CELLS AFF-TABLE + @ ;

: SPAWN-ON  ( xt core -- )
    DUP 0<  OVER NCORES >= OR ABORT" Invalid core ID"
    DUP MICRO-CORE? IF
        ." WARNING: dispatching to micro-core (no tile/MEX)" CR
    THEN
    OVER OVER
    RQ-PUSH
    TASK-COUNT @ DUP 8 < IF
        DUP >R AFFINITY!
        HERE TDESC-TEMP !
        T.READY , 128 , , 0 , 0 , 0 ,
        TDESC-TEMP @ R> CELLS TASK-TABLE + !
        TASK-COUNT @ 1+ TASK-COUNT !
    ELSE
        DROP 2DROP
    THEN ;

: SCHED-AFFINE  ( -- )
    TASK-COUNT @ 0 ?DO
        I CELLS TASK-TABLE + @
        DUP T.STATUS T.READY = IF
            DUP T.XT
            I AFFINITY@
            DUP -1 = IF DROP 0 THEN
            RQ-PUSH
            T.RUNNING SWAP T.STATUS!
        ELSE DROP THEN
    LOOP
    SCHED-ALL ;

: AFF-INFO  ( -- )
    ."  --- Core Affinity ---" CR
    TASK-COUNT @ DUP 0= IF DROP ."    (no tasks)" CR EXIT THEN
    0 DO
        ."    Task " I . ."  -> "
        I AFFINITY@ DUP -1 = IF
            DROP ."  any"
        ELSE
            ."  core " .
        THEN
        CR
    LOOP ;

\ =====================================================================
\  §8.5  Per-Core Preemption
\ =====================================================================
\
\  Timer-assisted preemption for all cores.  The timer IRQ is
\  broadcast to all cores, and each core's ISR sets its own preempt
\  flag.  Cooperative yield points (YIELD?) check the per-core flag.

VARIABLE PREEMPT-FLAGS  127 ALLOT     \ 16 × 8 = 128 bytes

: PREEMPT-FLAGS-INIT  ( -- )
    NCORES_MAX 0 DO
        0  I CELLS PREEMPT-FLAGS + !
    LOOP ;

PREEMPT-FLAGS-INIT

: PREEMPT-FLAG!  ( val core -- )
    CELLS PREEMPT-FLAGS + ! ;

: PREEMPT-FLAG@  ( core -- val )
    CELLS PREEMPT-FLAGS + @ ;

: PREEMPT-SET  ( core -- )
    1 SWAP PREEMPT-FLAG! ;

: PREEMPT-CLR  ( core -- )
    0 SWAP PREEMPT-FLAG! ;

: PREEMPT-ON-ALL  ( -- )
    TIME-SLICE @ TIMER!
    7 TIMER-CTRL!
    1 PREEMPT-ENABLED ! ;

: PREEMPT-OFF-ALL  ( -- )
    1 TIMER-CTRL!
    0 PREEMPT-ENABLED !
    PREEMPT-FLAGS-INIT ;

: WORKER-CHECKPOINT  ( -- )
    PREEMPT-ENABLED @ IF
        COREID PREEMPT-FLAG@ IF
            COREID PREEMPT-CLR
        THEN
    THEN ;

: _CORE-CHECKPOINT-PER-CORE  ( -- )
    COREID 0= IF
        PREEMPT-ENABLED @ IF
            0 PREEMPT-FLAG@ IF
                0 PREEMPT-CLR SCHED-YIELD
            THEN
        THEN
    ELSE
        \ A secondary full core acknowledges its own flag and continues its
        \ one-shot dispatch; there is no suspended task scheduler to enter.
        WORKER-CHECKPOINT
    THEN ;

' _CORE-CHECKPOINT-PER-CORE IS CORE-CHECKPOINT

: PREEMPT-INFO  ( -- )
    ."  --- Preemption ---" CR
    ."    Enabled: " PREEMPT-ENABLED @ IF ."  yes" ELSE ."  no" THEN CR
    ."    Slice:   " TIME-SLICE @ . ."  cycles" CR
    NCORES 0 DO
        ."    Core " I . ."  : flag="
        I PREEMPT-FLAG@ . CR
    LOOP ;

\ =====================================================================
\  §8.6  IPI Messaging
\ =====================================================================
\
\  Structured inter-core message passing via shared-memory queues.
\  Each core has a MSG-DEPTH-deep circular inbox.  Messages are
\  3 cells wide: type, sender, payload.  Protected by hardware
\  spinlock MSG-SLOCK.
\
\  MSG-SEND      ( type payload target -- flag )
\  MSG-RECV      ( -- type sender payload flag )
\  MSG-PEEK      ( -- flag )
\  MSG-BROADCAST ( type payload -- n )
\  MSG-FLUSH     ( -- n )
\  MSG-HANDLER!  ( xt type -- )
\  MSG-DISPATCH  ( -- flag )
\  MSG-INFO      ( -- )

8 CONSTANT MSG-DEPTH
3 CONSTANT MSG-CELLS
7 CONSTANT MSG-SLOCK

VARIABLE MSG-INBOX  3071 ALLOT        \ 16 × 8 × 3 × 8 = 3072 bytes
VARIABLE MSG-IHEAD  127 ALLOT         \ 16 × 8 = 128 bytes
VARIABLE MSG-ITAIL  127 ALLOT         \ 16 × 8 = 128 bytes

0 CONSTANT MSG-CALL
1 CONSTANT MSG-DATA
2 CONSTANT MSG-SIGNAL
3 CONSTANT MSG-USER

: MSG-ISLOT  ( idx core -- addr )
    MSG-DEPTH MSG-CELLS * CELLS *  MSG-INBOX +
    SWAP MSG-CELLS CELLS * + ;

: MSG-ICOUNT  ( core -- n )
    DUP CELLS MSG-ITAIL + @  SWAP CELLS MSG-IHEAD + @  -
    MSG-DEPTH + MSG-DEPTH MOD ;

: MSG-IFULL?  ( core -- flag )
    MSG-ICOUNT MSG-DEPTH 1- >= ;

: MSG-IEMPTY?  ( core -- flag )
    DUP CELLS MSG-IHEAD + @  SWAP CELLS MSG-ITAIL + @  = ;

: MSG-INIT  ( -- )
    NCORES_MAX 0 DO
        0 I CELLS MSG-IHEAD + !
        0 I CELLS MSG-ITAIL + !
    LOOP
    MSG-INBOX  MSG-DEPTH MSG-CELLS * NCORES_MAX * CELLS  0 FILL ;

MSG-INIT

VARIABLE MS-T   VARIABLE MS-P   VARIABLE MS-G

: MSG-SEND  ( type payload target -- flag )
    MS-G !  MS-P !  MS-T !
    MSG-SLOCK LOCK
    MS-G @ MSG-IFULL? IF  MSG-SLOCK UNLOCK  0 EXIT  THEN
    MS-G @ DUP CELLS MSG-ITAIL + @  SWAP MSG-ISLOT
    MS-T @  OVER !  CELL+
    COREID  OVER !  CELL+
    MS-P @  SWAP !
    MS-G @ CELLS MSG-ITAIL + @  1+  MSG-DEPTH MOD
    MS-G @ CELLS MSG-ITAIL + !
    MSG-SLOCK UNLOCK  -1 ;

VARIABLE MR-T   VARIABLE MR-S   VARIABLE MR-P

: MSG-RECV  ( -- type sender payload flag )
    COREID MSG-IEMPTY? IF  0 0 0 0 EXIT  THEN
    MSG-SLOCK LOCK
    COREID DUP CELLS MSG-IHEAD + @  SWAP MSG-ISLOT
    DUP @ MR-T !  CELL+
    DUP @ MR-S !  CELL+
    @     MR-P !
    COREID DUP CELLS MSG-IHEAD + @  1+  MSG-DEPTH MOD
    COREID CELLS MSG-IHEAD + !
    MSG-SLOCK UNLOCK
    MR-T @ MR-S @ MR-P @ -1 ;

: MSG-PEEK  ( -- flag )
    COREID MSG-IEMPTY? INVERT ;

4 CONSTANT MSG-HTYPES
VARIABLE MSG-HTABLE  31 ALLOT

: MSG-HINIT  ( -- )
    MSG-HTYPES 0 DO  0 I CELLS MSG-HTABLE + !  LOOP ;

MSG-HINIT

: MSG-HANDLER!  ( xt type -- )
    CELLS MSG-HTABLE + ! ;

: MSG-HANDLER@  ( type -- xt|0 )
    DUP MSG-HTYPES < IF  CELLS MSG-HTABLE + @  ELSE  DROP 0  THEN ;

: MSG-DISPATCH  ( -- flag )
    MSG-PEEK 0= IF  0 EXIT  THEN
    MSG-RECV DROP
    ROT DUP MSG-HANDLER@
    DUP IF  EXECUTE -1  ELSE  DROP DROP DROP DROP 0  THEN ;

VARIABLE MB-T   VARIABLE MB-P

: MSG-BROADCAST  ( type payload -- n )
    MB-P !  MB-T !
    0
    NCORES 0 DO
        I COREID <> IF
            MB-T @ MB-P @ I MSG-SEND IF  1+  THEN
        THEN
    LOOP ;

: MSG-FLUSH  ( -- n )
    0
    BEGIN  MSG-PEEK  WHILE
        MSG-RECV IF  DROP DROP DROP  THEN  1+
    REPEAT ;

: MSG-INFO  ( -- )
    ."  --- IPI Messages ---" CR
    NCORES 0 DO
        ."    Core " I . ."  : " I MSG-ICOUNT . ."  msg(s)" CR
    LOOP
    ."  Handlers:" CR
    MSG-HTYPES 0 DO
        I MSG-HANDLER@ IF
            ."    type " I . CR
        THEN
    LOOP ;

\ =====================================================================
\  §8.7  Shared Resource Locks
\ =====================================================================
\
\  Named machine-wide spinlock assignments.  Application/runtime concurrency
\  owns lock 6 (Akashic uses it as EVT-LOCK); crypto must not reuse it.
\  Spinlock 7 is reserved for IPI messaging (MSG-SLOCK).
\
\  DICT-ACQUIRE / DICT-RELEASE   — dictionary (HERE, ALLOT, CREATE)
\  UART-ACQUIRE / UART-RELEASE   — UART output (EMIT, TYPE, .)
\  FS-ACQUIRE   / FS-RELEASE     — filesystem (F-OPEN, F-READ, etc.)
\  HEAP-ACQUIRE / HEAP-RELEASE   — heap allocator (ALLOC, FREE)
\  WITH-LOCK    ( xt lock# -- )  — execute xt while holding lock
\
\  Checked disk/filesystem operations acquire FS-LOCK internally.  Do not
\  wrap them in FS-ACQUIRE or WITH-LOCK: the hardware recognizes a same-core
\  reacquire but has no recursion depth, so the inner release would end the
\  outer critical section early.

0 CONSTANT DICT-LOCK
1 CONSTANT UART-LOCK
2 CONSTANT FS-LOCK
3 CONSTANT HEAP-LOCK
4 CONSTANT RING-LOCK
5 CONSTANT HT-LOCK
6 CONSTANT APP-LOCK
\ Lock 7 is MSG-SLOCK; BIOS reserves lock 8; HMAC-HKDF-LOCK is lock 9;
\ networking.f reserves lock 10 for its machine-wide TLS workspace owner and
\ lock 11 for short credential-registry publication/cancellation sections.

: DICT-ACQUIRE  ( -- )  DICT-LOCK LOCK ;
: DICT-RELEASE  ( -- )  DICT-LOCK UNLOCK ;
: UART-ACQUIRE  ( -- )  UART-LOCK LOCK ;
: UART-RELEASE  ( -- )  UART-LOCK UNLOCK ;
: FS-ACQUIRE    ( -- )  FS-LOCK LOCK ;
: FS-RELEASE    ( -- )  FS-LOCK UNLOCK ;
: HEAP-ACQUIRE  ( -- )  HEAP-LOCK LOCK ;
: HEAP-RELEASE  ( -- )  HEAP-LOCK UNLOCK ;

: WITH-LOCK  ( xt lock# -- )
    DUP >R LOCK
    EXECUTE
    R> UNLOCK ;

: LOCK-INFO  ( -- )
    ."  --- Resource Locks ---" CR
    ."  Assignments:" CR
    ."    0 = Dictionary" CR
    ."    1 = UART" CR
    ."    2 = Filesystem" CR
    ."    3 = Heap" CR
    ."    4 = Ring Buffers" CR
    ."    5 = Hash Tables" CR
    ."    6 = Application Runtime" CR
    ."    7 = IPI Messaging" CR
    ."    8 = Checked BIOS Crypto" CR
    ."    9 = KDOS HMAC/HKDF" CR
    ."    10 = KDOS TLS Workspace" CR
    ."    11 = KDOS TLS Credentials" CR ;

