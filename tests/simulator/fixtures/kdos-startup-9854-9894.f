\ =====================================================================
\  §14  Startup
\ =====================================================================

CR HRULE
."   KDOS v1.1 — Kernel Dashboard OS" CR
HRULE
."  Type HELP for commands, HELP <word> for details."  CR
."  Type SCREENS for interactive TUI (or N SCREEN for screen N)."  CR
."  Type TOPICS or LESSONS for documentation."  CR
NCORES 1 > IF
    ."   Multicore: " NCORES . ."  cores available" CR
    ."   Use CORE-RUN, BARRIER, P.RUN-PAR for parallel work."  CR
THEN
DISK? IF FS-LOAD THEN

\ Force system heap initialisation before userland can confuse HEAP-SETUP.
\ DMA-ALLOCATE 16 bytes to trigger lazy HEAP-SETUP, then DMA-FREE.
\ Must use DMA- variants to target Bank 0 directly (ALLOCATE routes
\ to xmem when extended memory is present).
16 DMA-ALLOCATE DROP DMA-FREE

\ -- AUTOEXEC: run autoexec.f if present on disk --
\ Must use a colon definition because FSLOAD evaluates each line
\ independently — multi-line IF/THEN does not gate execution.
CREATE _AUTOEXEC-NAME
  97 C, 117 C, 116 C, 111 C, 101 C, 120 C, 101 C, 99 C, 46 C, 102 C,

: _AUTOEXEC-RUN  ( -- )
    FS-OK @ 0= IF EXIT THEN
    _AUTOEXEC-NAME NAMEBUF 10 CMOVE
    NAMEBUF 10 + 14 0 FILL
    FIND-BY-NAME -1 = IF EXIT THEN
    ."  Running autoexec.f..." CR
    _MOD-LOAD-BODY ;

_AUTOEXEC-RUN

\ JIT served its purpose — disable for interactive use.
JIT-OFF
CR
