
\ BIOS dictionary emitters call this hook only after proving that an exact
\ operation cannot fit, and before publishing HERE or writing any byte.  The
\ standard -8 exception lets EVALUATE-CHECKED report EVAL-S-THROW and lets its
\ caller roll back the containing source transaction.  Outside a CATCH, retain
\ a stable interactive diagnostic and ABORT rather than returning to BIOS.
-8 CONSTANT U-DICT-E-FULL

: _KDOS-DICT-FAULT  ( -- )
    \ Bank-0 and userland source transactions share the same checked failure.
    \ The Bank-0 preflight retains a 256-byte stack margin, so the handler can
    \ unwind before any write without abandoning loader-owned state.
    HANDLER @ IF U-DICT-E-FULL THROW THEN
    DICT-LIMIT@ IF
        TRUE ABORT" Userland dictionary full"
    THEN
    TRUE ABORT" dictionary overflow" ;

' _KDOS-DICT-FAULT DICT-FAULT-XT!

\ Preserve the BIOS task ABI while adding KDOS-owned exception cleanup.  BIOS
\ cannot clear _TASK-HANDLERS because that table is allocated when KDOS loads.
\ Scheduling a slot is also replacement, so reset on both start and stop.  Slot
\ zero is the foreground handler and is deliberately never touched here.
' BACKGROUND  CONSTANT _BIOS-BACKGROUND-XT
' BACKGROUND2 CONSTANT _BIOS-BACKGROUND2-XT
' BACKGROUND3 CONSTANT _BIOS-BACKGROUND3-XT
' TASK-STOP   CONSTANT _BIOS-TASK-STOP-XT

: _TASK-HANDLER-RESET  ( slot -- )
    CELLS _TASK-HANDLERS + 0 SWAP ! ;

: BACKGROUND  ( xt -- )
    1 _TASK-HANDLER-RESET  _BIOS-BACKGROUND-XT EXECUTE ;

: BACKGROUND2  ( xt -- )
    2 _TASK-HANDLER-RESET  _BIOS-BACKGROUND2-XT EXECUTE ;

: BACKGROUND3  ( xt -- )
    3 _TASK-HANDLER-RESET  _BIOS-BACKGROUND3-XT EXECUTE ;

: TASK-STOP  ( slot -- )
    DUP _BIOS-TASK-STOP-XT EXECUTE  _TASK-HANDLER-RESET ;

