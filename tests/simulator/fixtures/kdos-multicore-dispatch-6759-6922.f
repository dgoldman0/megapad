\ =====================================================================
\  §8.1  Multicore Dispatch
\ =====================================================================
\
\  High-level words that build on the BIOS multicore primitives
\  (COREID, NCORES, WAKE-CORE, CORE-STATUS, SPIN@, SPIN!).
\
\  CORE-RUN   ( xt core -- )  dispatch XT to a secondary core
\  CORE-WAIT  ( core -- )     busy-wait until a core finishes
\  ALL-CORES-WAIT ( -- )      wait for all secondary cores to idle
\  BARRIER    ( -- )          synchronize: wait for all cores
\  LOCK       ( n -- )        acquire spinlock n (busy-wait)
\  UNLOCK     ( n -- )        release spinlock n
\  CORES      ( -- )          display per-core status
\  P.RUN-PAR  ( pipe -- )     run pipeline steps in parallel across cores
\
\  Uses BIOS words: COREID NCORES WAKE-CORE CORE-STATUS SPIN@ SPIN!
\                   IPI-SEND IPI-STATUS MBOX! MBOX@
\
\  --- Multicore Concurrency Contract ---
\
\  All dictionary, heap, and arena-management words use shared
\  scratch VARIABLEs (A-PREV, A-CURR, AR-SZ, FL-PREV, etc.) that
\  are NOT safe under concurrent execution.  The following words
\  enforce core-0 only access via ?CORE0:
\
\    ALLOCATE  FREE  RESIZE   (heap — shared free-list + scratch)
\    ARENA-NEW  ARENA-NEW-AT  (arena setup — AR-SZ, AR-SRC, AR-BLK)
\    ARENA-DESTROY            (calls FREE or XMEM-FREE-BLOCK)
\
\  Secondary cores dispatched via CORE-RUN should ONLY use:
\
\    ARENA-ALLOT / ARENA-ALLOT?  (pure stack + one arena-local ptr)
\    ARENA-FREE / ARENA-USED     (read-only)
\    ARENA-SNAP / ARENA-ROLLBACK (single pointer write)
\    AALLOT                      (via CURRENT-ARENA — push before dispatch)
\    Direct memory access (@ ! C@ C! MOVE FILL etc.)
\
\  Pattern: core 0 creates arenas and allocates at setup time,
\  dispatches self-contained XTs that only bump-allocate from
\  pre-created per-core arenas, then collects results after BARRIER.
\
\  Per-core arenas eliminate contention entirely — each core only
\  touches its own bump pointer.  Inter-core results pass through
\  the mailbox (MBOX! / MBOX@); scratch stays local.

\ -- CORE-RUN ( xt core -- )  dispatch XT to secondary core --
\   Validates the core number, then sends via WAKE-CORE.
\   Note: caller is responsible for ensuring the XT is safe for
\   the target core type (micro-cores cannot run tile/MEX ops).
: CORE-RUN  ( xt core -- )
    DUP COREID = ABORT" Cannot dispatch to self"
    DUP 0<  OVER NCORES >= OR ABORT" Invalid core ID"
    WAKE-CORE ;

\ -- CORE-WAIT ( core -- )  busy-wait until core is idle --
\   Polls CORE-STATUS (worker XT slot) until it reads 0.
\   Each iteration also checks YIELD? so preemption still works.
: CORE-WAIT  ( core -- )
    BEGIN
        DUP CORE-STATUS 0<>
    WHILE
        YIELD?
    REPEAT
    DROP ;

\ -- ALL-CORES-WAIT ( -- )  wait for all secondary cores to idle --
: ALL-CORES-WAIT  ( -- )
    NCORES 1 DO
        I CORE-WAIT
    LOOP ;

\ -- ALL-FULL-WAIT ( -- )  wait for all secondary full cores to idle --
: ALL-FULL-WAIT  ( -- )
    N-FULL-CORES 1 DO
        I CORE-WAIT
    LOOP ;

\ -- BARRIER ( -- )  synchronize: wait for all secondary cores --
\   Core 0 calls this to wait until all dispatched work is finished.
: BARRIER  ( -- )
    ALL-CORES-WAIT ;

\ -- LOCK ( n -- )  acquire spinlock n (busy-wait) --
\   Retries SPIN@ until it returns 0 (acquired).
: LOCK  ( n -- )
    BEGIN
        DUP SPIN@ 0<>
    WHILE
        YIELD?
    REPEAT
    DROP ;

\ -- UNLOCK ( n -- )  release spinlock n --
: UNLOCK  ( n -- )
    SPIN! ;

\ -- CORES ( -- )  display per-core status --
: CORES  ( -- )
    ."  --- Cores (" NCORES . ."  ) ---" CR
    NCORES 0 DO
        ."    Core " I .
        I COREID = IF
            ."   [self] RUNNING" CR
        ELSE
            I CORE-STATUS IF
                ."   BUSY" CR
            ELSE
                ."   IDLE" CR
            THEN
        THEN
    LOOP ;

\ -- Parallel pipeline variables --
VARIABLE PAR-PIPE       \ pipeline being dispatched
VARIABLE PAR-STEP       \ current step index (used by wrappers)
VARIABLE PAR-CORE       \ next core to assign

\ -- Step wrapper XTs: executed on secondary cores --
\   We pre-define wrappers for steps 0-7 (max pipeline capacity).
\   Each wrapper reads the pipeline and step index from shared
\   variables, looks up the step XT, and calls it.

\ Since secondary cores call the XT directly and the pipeline
\ step XTs are no-argument words (they operate on pre-bound
\ buffers), we dispatch them directly via CORE-RUN.

\ -- P.RUN-PAR ( pipe -- )  run pipeline steps in parallel --
\   Distributes steps across available secondary FULL cores only.
\   Pipeline steps use tile/MEX ops which micro-cores cannot execute.
\   If there are more steps than full cores, remaining steps run on core 0.
\   Always waits for all dispatched work before returning.
VARIABLE PAR-P          \ pipeline being dispatched
VARIABLE PAR-N          \ next core to use

: P.RUN-PAR  ( pipe -- )
    N-FULL-CORES 1 <= IF
        \ Single core: fall back to sequential
        P.RUN EXIT
    THEN
    DUP P.COUNT 0= IF DROP EXIT THEN
    PAR-P !
    1 PAR-N !               \ start dispatching to core 1
    PAR-P @ P.COUNT 0 DO
        PAR-P @ I P.GET      ( step-xt )
        PAR-N @ N-FULL-CORES < IF
            PAR-N @ CORE-RUN
            PAR-N @ 1+ PAR-N !
        ELSE
            EXECUTE
        THEN
    LOOP
    ALL-FULL-WAIT ;

\ -- P.BENCH-PAR ( pipe -- )  benchmark parallel pipeline --
: P.BENCH-PAR  ( pipe -- )
    ."  Parallel pipeline (" DUP P.COUNT . ."  steps, "
    NCORES . ."  cores):" CR
    DUP
    CYCLES >R
    P.RUN-PAR
    CYCLES R> -
    ."    total = " . ."  cycles" CR ;

