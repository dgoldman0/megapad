; =====================================================================
;  Megapad-64 BIOS  v0.2
; =====================================================================
;  Bootstraps the system from a cold reset (entry at address 0).
;
;  1. Set up stack, UART, timer, trap handlers
;  2. Print boot banner with system info
;  3. Enter interactive monitor shell
;
;  Register conventions (preserved across shell iterations):
;    R0  = scratch / CSR operand       (CSR-capable: R0-R7)
;    R1  = argument / return / scratch (CSR-capable)
;    R2  = XSEL data pointer           (CSR-capable)
;    R3  = PC  (PSEL=3)                (CSR-capable)
;    R4  = subroutine: print_str       (CSR-capable)
;    R5  = subroutine: read_char       (CSR-capable)
;    R6  = subroutine: print_hex_byte  (CSR-capable)
;    R7  = scratch                     (CSR-capable)
;    R8  = UART TX base address
;    R9  = user address register (for dump/go/tile addressing)
;    R10 = string pointer (for print_str)
;    R11-R14 = scratch (used by command handlers)
;    R15 = SP  (SPSEL=15)
;
;  MMIO layout:
;    UART      0xFFFF_FF00_0000_0000  TX=+0 RX=+1 STATUS=+2
;    Timer     0xFFFF_FF00_0000_0100  COUNT=+0..+3 CTRL=+8
;    Storage   0xFFFF_FF00_0000_0200
;    SysInfo   0xFFFF_FF00_0000_0300  BOARD=+0 RAM=+4 STORAGE=+8
;
;  Tile CSRs (via CSRR/CSRW, R0-R7 only):
;    0x10=SB  0x11=SR  0x12=SC  0x13=SW
;    0x14=TMODE  0x15=TCTRL
;    0x16=TSRC0  0x17=TSRC1  0x18=TDST
;    0x19=ACC0  0x1A=ACC1  0x1B=ACC2  0x1C=ACC3
;    0x20=IVT_BASE  0x22=TRAP_ADDR  0x30=MEGAPAD_SZ  0x31=CPUID
;
;  Shell commands:
;    h  help       r  regs       d  dump 16 bytes    s  set addr
;    g  go (call)  q  quit       m  megapad info     i  tile info
;    c  cycles     f  fill tile  v  view tile        x  exec tile op
;    t  tile config
; =====================================================================

; === Entry point (address 0) ===

    ; SP = ram_size (boot() loads R2 with ram_size)
    mov r15, r2

    ; UART base in R8
    ldi64 r8, 0xFFFF_FF00_0000_0000

    ; Zero user address register
    ldi r9, 0

    ; Subroutine pointers (most-used in dedicated registers)
    ldi64 r4, print_str
    ldi64 r5, read_char
    ldi64 r6, print_hex_byte

    ; Enable timer (write 0x01 to TIMER CONTROL at +0x08)
    ldi64 r11, 0xFFFF_FF00_0000_0108
    ldi r1, 0x01
    st.b r11, r1

    ; Install IVT for bus-fault trap handler
    ldi64 r0, ivt_table
    csrw 0x20, r0

    ; --- Banner ---
    ldi64 r10, banner
    call.l r4

    ; Probe storage: SysInfo offset 0x08
    ldi64 r11, 0xFFFF_FF00_0000_0308
    ld.b r12, r11
    ldi64 r10, msg_storage
    call.l r4
    ldi r1, 0x4E
    cmpi r12, 0
    breq boot_no_storage
    ldi r1, 0x59
boot_no_storage:
    st.b r8, r1

    ; Probe Megapad size
    ldi64 r10, msg_megapad_sz
    call.l r4
    csrr r0, 0x30
    mov r1, r0
    ldi64 r11, print_hex32
    call.l r11

    ; "Ready."
    ldi64 r10, msg_ready
    call.l r4

; =====================================================================
;  Shell loop
; =====================================================================

shell_loop:
    ; Prompt "> "
    ldi r1, 0x3E
    st.b r8, r1
    ldi r1, 0x20
    st.b r8, r1

shell_read:
    call.l r5

    ; Skip CR/LF silently
    cmpi r1, 0x0A
    breq shell_read
    cmpi r1, 0x0D
    breq shell_read

    ; Echo char + newline
    st.b r8, r1
    ldi r0, 0x0D
    st.b r8, r0
    ldi r0, 0x0A
    st.b r8, r0

    ; --- Dispatch ---
    cmpi r1, 0x68
    lbreq cmd_help
    cmpi r1, 0x72
    lbreq cmd_regs
    cmpi r1, 0x64
    lbreq cmd_dump
    cmpi r1, 0x73
    lbreq cmd_setaddr
    cmpi r1, 0x67
    lbreq cmd_go
    cmpi r1, 0x71
    lbreq cmd_quit
    cmpi r1, 0x6D
    lbreq cmd_megapad
    cmpi r1, 0x69
    lbreq cmd_tileinfo
    cmpi r1, 0x63
    lbreq cmd_cycles
    cmpi r1, 0x66
    lbreq cmd_fill
    cmpi r1, 0x76
    lbreq cmd_viewtile
    cmpi r1, 0x78
    lbreq cmd_exec
    cmpi r1, 0x74
    lbreq cmd_tileconf

    ; Unknown -> '?'
    ldi r1, 0x3F
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; =====================================================================
;  Commands -- basic
; =====================================================================

; --- h: help ---
cmd_help:
    ldi64 r10, msg_help
    call.l r4
    lbr shell_loop

; --- r: registers ---
cmd_regs:
    ldi64 r10, msg_pc
    call.l r4
    mov r1, r3
    call.l r6
    ldi64 r10, msg_sp
    call.l r4
    mov r1, r15
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, msg_r9
    call.l r4
    mov r1, r9
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; --- d: dump 16 bytes at R9 ---
cmd_dump:
    ldi r12, 16
    mov r11, r9
dump_loop:
    ld.b r1, r11
    call.l r6
    ldi r1, 0x20
    st.b r8, r1
    inc r11
    dec r12
    cmpi r12, 0
    brne dump_loop
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; --- s: set address (read 4 hex chars HHLL -> R9) ---
cmd_setaddr:
    ldi64 r11, read_hex_byte
    call.l r11              ; high byte
    lsli r1, 8
    mov r9, r1
    call.l r11              ; low byte  (R11 preserved across call)
    or r9, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

; --- g: go (call address in R9) ---
cmd_go:
    call.l r9
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

; --- q: quit ---
cmd_quit:
    ldi64 r10, msg_bye
    call.l r4
    halt

; =====================================================================
;  Commands -- megapad / tile
; =====================================================================

; --- m: megapad info ---
cmd_megapad:
    ldi64 r10, msg_cpuid
    call.l r4
    csrr r1, 0x31
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    ldi64 r10, msg_mpad_sz
    call.l r4
    csrr r1, 0x30
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; --- i: tile CSR info ---
cmd_tileinfo:
    ; TMODE TCTRL
    ldi64 r10, msg_ti_mode
    call.l r4
    csrr r1, 0x14
    call.l r6
    ldi64 r10, msg_ti_ctrl
    call.l r4
    csrr r1, 0x15
    call.l r6
    ldi64 r11, print_crlf
    call.l r11

    ; TSRC0 TSRC1 TDST
    ldi64 r10, msg_ti_src0
    call.l r4
    csrr r1, 0x16
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, msg_ti_src1
    call.l r4
    csrr r1, 0x17
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, msg_ti_dst
    call.l r4
    csrr r1, 0x18
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11

    ; Cursor: SB/SR/SC/SW
    ldi64 r10, msg_ti_cursor
    call.l r4
    csrr r1, 0x10
    call.l r6
    ldi r1, 0x2F
    st.b r8, r1
    csrr r1, 0x11
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x2F
    st.b r8, r1
    csrr r1, 0x12
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x2F
    st.b r8, r1
    csrr r1, 0x13
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11

    ; ACC0-ACC3
    ldi64 r10, msg_ti_acc
    call.l r4
    csrr r1, 0x19
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1A
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1B
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1C
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; --- c: cycle count (read timer) ---
cmd_cycles:
    ldi64 r10, msg_cycles
    call.l r4
    ldi64 r11, 0xFFFF_FF00_0000_0100
    ld.w r1, r11
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; --- f: fill 64 bytes at R9 with pattern byte ---
cmd_fill:
    ldi64 r10, msg_fill_prompt
    call.l r4
    ldi64 r11, read_hex_byte
    call.l r11
    ; R1 = pattern byte, fill 64 bytes at [R9]
    mov r11, r9
    ldi r12, 64
fill_loop:
    st.b r11, r1
    inc r11
    dec r12
    cmpi r12, 0
    brne fill_loop
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

; --- v: view tile (64 bytes at R9, 4 lines of 16) ---
cmd_viewtile:
    mov r11, r9
    ldi r13, 4
view_outer:
    ldi r12, 16
view_inner:
    ld.b r1, r11
    call.l r6
    ldi r1, 0x20
    st.b r8, r1
    inc r11
    dec r12
    cmpi r12, 0
    brne view_inner
    ; CRLF
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    dec r13
    cmpi r13, 0
    brne view_outer
    lbr shell_loop

; --- t: tile config submenu ---
;   m=TMODE  c=TCTRL  0=TSRC0=R9  1=TSRC1=R9  d=TDST=R9
;   b=SB  r=SR  l=SC  w=SW
cmd_tileconf:
    ldi64 r10, msg_tconf
    call.l r4
    call.l r5

    ; Skip CR/LF
    cmpi r1, 0x0A
    lbreq shell_loop
    cmpi r1, 0x0D
    lbreq shell_loop

    cmpi r1, 0x6D
    lbreq tconf_mode
    cmpi r1, 0x63
    lbreq tconf_ctrl
    cmpi r1, 0x30
    lbreq tconf_src0
    cmpi r1, 0x31
    lbreq tconf_src1
    cmpi r1, 0x64
    lbreq tconf_dst
    cmpi r1, 0x62
    lbreq tconf_sb
    cmpi r1, 0x72
    lbreq tconf_sr
    cmpi r1, 0x6C
    lbreq tconf_sc
    cmpi r1, 0x77
    lbreq tconf_sw

    ldi r1, 0x3F
    st.b r8, r1
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

tconf_mode:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x14, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_ctrl:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x15, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_src0:
    mov r0, r9
    csrw 0x16, r0
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_src1:
    mov r0, r9
    csrw 0x17, r0
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_dst:
    mov r0, r9
    csrw 0x18, r0
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_sb:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x10, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_sr:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x11, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_sc:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x12, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

tconf_sw:
    ldi64 r11, read_hex_byte
    call.l r11
    csrw 0x13, r1
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

; --- x: execute tile op submenu ---
;   a=ADD s=SUB n=AND o=OR e=XOR  (TALU)
;   m=MUL d=DOT                   (TMUL)
;   S=SUM M=MIN X=MAX             (TRED)
;   t=TRANS z=ZERO                 (TSYS)
cmd_exec:
    ldi64 r10, msg_exec
    call.l r4
    call.l r5

    ; Skip CR/LF
    cmpi r1, 0x0A
    lbreq shell_loop
    cmpi r1, 0x0D
    lbreq shell_loop

    cmpi r1, 0x61
    lbreq exec_add
    cmpi r1, 0x73
    lbreq exec_sub
    cmpi r1, 0x6E
    lbreq exec_and
    cmpi r1, 0x6F
    lbreq exec_or
    cmpi r1, 0x65
    lbreq exec_xor
    cmpi r1, 0x6D
    lbreq exec_mul
    cmpi r1, 0x64
    lbreq exec_dot
    cmpi r1, 0x53
    lbreq exec_sum
    cmpi r1, 0x4D
    lbreq exec_rmin
    cmpi r1, 0x58
    lbreq exec_rmax
    cmpi r1, 0x74
    lbreq exec_trans
    cmpi r1, 0x7A
    lbreq exec_zero

    ldi r1, 0x3F
    st.b r8, r1
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; -- TALU ops --
exec_add:
    t.add
    lbr exec_done_ok
exec_sub:
    t.sub
    lbr exec_done_ok
exec_and:
    t.and
    lbr exec_done_ok
exec_or:
    t.or
    lbr exec_done_ok
exec_xor:
    t.xor
    lbr exec_done_ok

; -- TMUL ops --
exec_mul:
    t.mul
    lbr exec_done_ok
exec_dot:
    t.dot
    lbr exec_done_acc

; -- TRED ops --
exec_sum:
    t.sum
    lbr exec_done_acc
exec_rmin:
    t.rmin
    lbr exec_done_acc
exec_rmax:
    t.rmax
    lbr exec_done_acc

; -- TSYS ops --
exec_trans:
    t.trans
    lbr exec_done_ok
exec_zero:
    t.zero
    lbr exec_done_ok

; -- Shared exec result handlers --
exec_done_ok:
    ldi64 r10, msg_ok
    call.l r4
    lbr shell_loop

exec_done_acc:
    ldi64 r10, msg_acc_res
    call.l r4
    csrr r1, 0x19
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1A
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1B
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    st.b r8, r1
    csrr r1, 0x1C
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    lbr shell_loop

; =====================================================================
;  Subroutines
; =====================================================================

; --- print_str: print null-terminated string at R10 ---
;     Auto-translates \n (0x0A) -> \r\n for raw terminals.
;     Clobbers: R1, R10
print_str:
    ld.b r1, r10
    cmpi r1, 0
    breq print_str_done
    cmpi r1, 0x0A
    brne print_str_emit
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
print_str_emit:
    st.b r8, r1
    inc r10
    br print_str
print_str_done:
    ret.l

; --- read_char: blocking read one byte from UART -> R1 ---
;     Uses IDL to sleep while waiting. Clobbers: R1, R13
read_char:
    ldi64 r13, 0xFFFF_FF00_0000_0002
read_char_poll:
    ld.b r1, r13
    andi r1, 0x02
    cmpi r1, 0
    brne read_char_ready
    idl
    br read_char_poll
read_char_ready:
    ldi64 r13, 0xFFFF_FF00_0000_0001
    ld.b r1, r13
    ret.l

; --- print_hex_byte: print R1 low byte as 2 hex chars ---
;     Clobbers: R0, R1
print_hex_byte:
    mov r0, r1
    lsri r1, 4
    andi r1, 0x0F
    addi r1, 0x30
    cmpi r1, 0x3A
    brcc phb_hi_ok
    addi r1, 7
phb_hi_ok:
    st.b r8, r1
    mov r1, r0
    andi r1, 0x0F
    addi r1, 0x30
    cmpi r1, 0x3A
    brcc phb_lo_ok
    addi r1, 7
phb_lo_ok:
    st.b r8, r1
    ret.l

; --- print_hex16: print R1 low 16 bits as 4 hex chars ---
;     Clobbers: R0, R1, R14
print_hex16:
    mov r14, r1
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    mov r1, r14
    andi r1, 0xFF
    call.l r6
    ret.l

; --- print_hex32: print R1 low 32 bits as 8 hex chars ---
;     Clobbers: R0, R1, R14
print_hex32:
    mov r14, r1
    ; Byte 3 (bits 31-24)
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    ; Byte 2 (bits 23-16)
    mov r1, r14
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    ; Byte 1 (bits 15-8)
    mov r1, r14
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    ; Byte 0 (bits 7-0)
    mov r1, r14
    andi r1, 0xFF
    call.l r6
    ret.l

; --- print_crlf: emit CR + LF ---
;     Clobbers: R1
print_crlf:
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    ret.l

; --- read_hex_byte: read 2 hex chars from UART -> R1 (byte value) ---
;     Handles 0-9, A-F, a-f.  Clobbers: R0, R1, R13, R14
read_hex_byte:
    call.l r5
    ; Convert first hex char to high nibble
    subi r1, 0x30
    cmpi r1, 10
    brcc rhb_d1_ok
    andi r1, 0x1F
    subi r1, 7
rhb_d1_ok:
    andi r1, 0x0F
    lsli r1, 4
    mov r14, r1
    call.l r5
    ; Convert second hex char to low nibble
    subi r1, 0x30
    cmpi r1, 10
    brcc rhb_d2_ok
    andi r1, 0x1F
    subi r1, 7
rhb_d2_ok:
    andi r1, 0x0F
    or r1, r14
    ret.l

; =====================================================================
;  Bus fault trap handler
; =====================================================================
bus_fault_handler:
    ; Restore critical registers (user program may have clobbered them)
    ldi64 r8, 0xFFFF_FF00_0000_0000
    ldi64 r4, print_str
    ldi64 r5, read_char
    ldi64 r6, print_hex_byte
    ; Read faulting address
    csrr r0, 0x22
    ; Print error message
    ldi64 r10, msg_busfault
    call.l r4
    mov r1, r0
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    ; Clean up trap frame (2 x push64 by _trap) and return to shell
    addi r15, 16
    lbr shell_loop

; =====================================================================
;  String data
; =====================================================================

banner:
    .asciiz "\nMegapad-64 BIOS v0.2\n"

msg_storage:
    .asciiz "  Storage: "

msg_megapad_sz:
    .asciiz "  Megapad: "

msg_ready:
    .asciiz "\nReady.\n"

msg_help:
    .asciiz "h=help r=regs d=dump s=set g=go q=quit\nm=mpad i=tile c=cyc f=fill v=view x=exec t=conf\n"

msg_pc:
    .asciiz "PC="

msg_sp:
    .asciiz " SP="

msg_r9:
    .asciiz " R9="

msg_ok:
    .asciiz "OK\n"

msg_bye:
    .asciiz "Bye!\n"

msg_cpuid:
    .asciiz "CPUID: "

msg_mpad_sz:
    .asciiz "MPAD_SZ: "

msg_cycles:
    .asciiz "Timer: "

msg_fill_prompt:
    .asciiz "Byte: "

msg_tconf:
    .asciiz "m=mode c=ctrl 0=src0 1=src1 d=dst b=sb r=sr l=sc w=sw\n"

msg_exec:
    .asciiz "a=add s=sub n=and o=or e=xor m=mul d=dot\nS=SUM M=MIN X=MAX t=trans z=zero\n"

msg_acc_res:
    .asciiz "ACC: "

msg_ti_mode:
    .asciiz "TMODE="

msg_ti_ctrl:
    .asciiz " TCTRL="

msg_ti_src0:
    .asciiz "SRC0="

msg_ti_src1:
    .asciiz " SRC1="

msg_ti_dst:
    .asciiz " DST="

msg_ti_cursor:
    .asciiz "Cursor SB/SR/SC/SW: "

msg_ti_acc:
    .asciiz "ACC: "

msg_busfault:
    .asciiz "\n*** BUS FAULT @ "

; =====================================================================
;  IVT (Interrupt Vector Table)
; =====================================================================
;  7 entries x 8 bytes = 56 bytes
;  Index: 0=RESET 1=NMI 2=ILLEGAL_OP 3=ALIGN_FAULT
;         4=DIV_ZERO 5=BUS_FAULT 6=SW_TRAP
ivt_table:
    .dq 0
    .dq 0
    .dq 0
    .dq 0
    .dq 0
    .dq bus_fault_handler
    .dq 0
