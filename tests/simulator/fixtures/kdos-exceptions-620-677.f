\  §1.2  Exception Handling — CATCH / THROW
\ =====================================================================
\
\  ANS Forth CATCH/THROW (EXCEPTION word set).
\
\  CATCH saves the current stack pointers and installs an exception
\  frame.  If the executed XT calls THROW with a non-zero code,
\  control returns to the matching CATCH with stacks restored.
\
\  Exception frames are chained through execution-context-local HANDLER cells:
\  one for each BIOS coroutine on core 0, and one for each physical worker
\  core.  CATCH frames may therefore remain live across PAUSE/TASK-YIELD.
\  A stopped or replaced coroutine never resumes to unwind its live frames, so
\  KDOS clears that slot's chain head at the BIOS scheduling boundary below.
\
\  Requires BIOS words: SP@ SP! RP@ RP!
\

\ Each execution context with independent data/return stacks must also have an
\ independent exception-chain head.  Core 0 selects by cooperative TASK-ID;
\ physical worker cores select by COREID and do not consult core 0's task state.
\ Keep HANDLER's traditional `( -- addr )` interface.
CREATE _HANDLERS  NCORES CELLS ALLOT
_HANDLERS NCORES CELLS 0 FILL
CREATE _TASK-HANDLERS  4 CELLS ALLOT
_TASK-HANDLERS 4 CELLS 0 FILL

: HANDLER  ( -- addr )
    COREID ?DUP IF
        CELLS _HANDLERS +
    ELSE
        TASK-ID CELLS _TASK-HANDLERS +
    THEN ;

\ CATCH ( xt -- exception# | 0 )
\   Execute xt.  If it completes normally, return 0.
\   If xt (or anything it calls) does THROW n, return n
\   with data stack restored to depth at CATCH entry + 1.
: CATCH  ( xt -- 0 | exception# )
    SP@ >R              ( save data-stack pointer )
    HANDLER @ >R        ( save previous handler frame )
    RP@ HANDLER !       ( install new handler = current RSP )
    EXECUTE             ( run the XT )
    R> HANDLER !        ( restore previous handler )
    R> DROP             ( discard saved SP )
    0 ;                 ( no exception → 0 )

\ THROW ( n -- )
\   If n = 0, do nothing (identity).
\   If n ≠ 0, unwind to most recent CATCH, restoring stacks.
: THROW  ( n -- )
    ?DUP IF
        HANDLER @ RP!   ( unwind RSP to handler frame )
        R> HANDLER !    ( pop & restore previous handler )
        R> SWAP >R      ( recover saved SP, stash throw-code )
        SP!             ( restore data stack )
        DROP R>         ( drop stale TOS, retrieve throw-code )
    THEN ;
