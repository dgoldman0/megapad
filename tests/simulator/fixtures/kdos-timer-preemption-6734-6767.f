\ -- Timer preemption setup --
\   Uses polling approach: YIELD? checks if the timer counter has
\   exceeded the preemption threshold.  This is simpler than a
\   hardware ISR (which would need raw machine code for RTI).
\
\   PREEMPT-ON starts the auto-reload timer and enables the flag check.
\   YIELD? checks the timer STATUS register for compare-match.

VARIABLE PREEMPT-ENABLED
0 PREEMPT-ENABLED !

\ -- PREEMPT-ON ( -- ) enable timer-based preemption polling --
: PREEMPT-ON  ( -- )
    TIME-SLICE @ TIMER!           \ set compare value
    5 TIMER-CTRL!                 \ enable + auto-reload (no IRQ)
    1 PREEMPT-ENABLED ! ;

\ -- PREEMPT-OFF ( -- ) disable timer preemption --
: PREEMPT-OFF  ( -- )
    1 TIMER-CTRL!                 \ enable counter only, no auto-reload
    0 PREEMPT-ENABLED ! ;

\ Install the timer-aware single-core checkpoint.  Existing callers of the
\ deferred CORE-CHECKPOINT immediately see this action.
: _CORE-CHECKPOINT-TIMER  ( -- )
    PREEMPT-ENABLED @ IF
        PREEMPT-FLAG @ IF
            0 PREEMPT-FLAG !
            YIELD
        THEN
    THEN ;

' _CORE-CHECKPOINT-TIMER IS CORE-CHECKPOINT

