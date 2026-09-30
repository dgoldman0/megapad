\ .PERF ( -- )  Display performance counters.
: .PERF
    CR ."   Performance Counters" CR
    ."     Cycles:   " PERF-CYCLES . CR
    ."     Stalls:   " PERF-STALLS . CR
    ."     Tile ops: " PERF-TILEOPS . CR
    ."     Ext mem:  " PERF-EXTMEM . CR ;

\ .BIST-STATUS ( -- )  Display last BIST result (from boot, NOT re-run).
\   BIST destroys all RAM so must NOT be run after KDOS loads.
: .BIST-STATUS
    CR ."   Memory BIST Status" CR
    BIST-STATUS
    DUP 0 = IF DROP ."     idle (no BIST run)" CR ELSE
    DUP 2 = IF DROP ."     PASS" CR ELSE
    DUP 3 = IF DROP ."     FAIL at addr " BIST-FAIL-ADDR . CR
                    ."     Expected/Actual: " BIST-FAIL-DATA . CR ELSE
    DROP ."     running..."  CR
    THEN THEN THEN ;

\ .TILE-DIAG ( -- )  Run tile self-test and display result.
: .TILE-DIAG
    CR ."   Tile Datapath Self-Test..."  CR
    TILE-TEST
    BEGIN TILE-TEST@ DUP 0 = WHILE DROP REPEAT
    DUP 2 = IF
        DROP ."     PASS (ADD, MUL, DOT, SUM)" CR
    ELSE
        DROP ."     FAIL — failed sub-tests: " TILE-DETAIL@ . CR
    THEN ;

\ .ICACHE ( -- )  Display I-cache statistics.
: .ICACHE
    CR ."   I-Cache Statistics" CR
    ."     Hits:     " ICACHE-HITS . CR
    ."     Misses:   " ICACHE-MISSES . CR ;

\ DIAG ( -- )  Run full hardware diagnostics suite.
: DIAG
    CR ."  ======== Hardware Diagnostics ========" CR
    .PERF
    .CRC-DIAG
    .BIST-STATUS
    .TILE-DIAG
    .ICACHE
    ."  ======================================" CR ;

