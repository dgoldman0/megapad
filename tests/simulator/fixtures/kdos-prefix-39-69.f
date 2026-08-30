JIT-ON

\ =====================================================================
\  §1  Utility Words
\ =====================================================================
\  Note: BIOS v1.0 provides all ANS core words including OFF, >=, <=,
\  W@, W!, L@, L!, .ZSTR, UCHAR, 2OVER, 2SWAP, 2ROT, TALIGN, MOVE,
\  COMPARE, ABORT, ABORT", LEAVE, EVALUATE, EVALUATE-CHECKED,
\  EVALUATE-FINISH, EVALUATOR-RESET, EVALUATOR-UNWIND, EVAL-STATUS,
\  EVAL-LINE, EVAL-COLUMN, EVAL-DEPTH, EVAL-THROW, EVAL-TOKEN,
\  DICT-INDEX!, DICT-INDEX@, DICT-ROLLBACK, LATEST!,
\  FIND, SOURCE, >IN, >NUMBER,
\  VALUE, TO, DOES>, POSTPONE, RECURSE, COUNT, WITHIN, U<, U>, 2/,
\  CHAR, [CHAR], 2>R, 2R>, 2R@, QUIT, plus the v0.5 set.

\ .R ( n width -- ) print number right-justified in width
: .R  ( just use . for now )
    DROP . ;

\ DEFER ( "name" -- ) create a deferred word (default = ABORT)
\ IS   ( xt "name" -- ) set the action of a deferred word
: DEFER  ( "name" -- )  CREATE ['] ABORT ,  DOES> @ EXECUTE ;
: IS     ( xt "name" -- )  ' >BODY ! ;

\ -- String utilities (needed by MP64FS file system) --

\ SAMESTR? ( addr1 addr2 maxlen -- flag )
\   Compare two byte strings up to maxlen bytes (zero-padded).
\   Returns -1 if equal, 0 if different.
: SAMESTR?  ( a1 a2 maxlen -- flag )
    DUP >R SWAP R> COMPARE 0= IF -1 ELSE 0 THEN ;
