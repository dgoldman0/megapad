\ =====================================================================
\  §2  Buffer Subsystem
\ =====================================================================
\
\  BUFFER descriptor (4 cells = 32 bytes):
\    +0   type        0=raw  1=records  2=tiles  3=bitset
\    +8   elem_width  bytes per element (1, 2, 4, or 8)
\    +16  length      number of elements
\    +24  data_addr   pointer to tile-aligned data
\
\  Usage:  0 1 1024 BUFFER my-raw-buf
\          ( type=raw  width=1  length=1024 )

\ -- Useful general helper --  \
: IDLE  ( -- )  [ 0 C, ] ;  \ IDL opcode — yield CPU until next interrupt
