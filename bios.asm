; =====================================================================
;  Megapad-64 BIOS  v1.0 — ANS Forth Core + Tile Engine
; =====================================================================
;
;  A subroutine-threaded Forth interpreter on the Megapad-64.
;  v0.4 added colon definitions, control flow, VARIABLE, CONSTANT,
;  string words, NIC words, and compilation infrastructure.
;  v0.5 adds EXIT, >R/R>/R@, J, UNLOOP, +LOOP, AGAIN, S",
;  CREATE, IMMEDIATE, STATE, [, ], LITERAL, MIN, MAX, CELLS,
;  CELL+, +!, 2*, CMOVE, -ROT, <>, 0<>, 0>, ?DUP, BL, TRUE, FALSE,
;  WORD, LATEST.
;
;  Register conventions
;  --------------------
;    R0  = scratch / CSR operand      (CSR-capable: R0-R7)
;    R1  = scratch / argument
;    R2  = ram_size (set by boot, preserved)
;    R3  = PC  (PSEL=3)
;    R4  = subroutine: emit_char
;    R5  = subroutine: key_char (blocking)
;    R6  = subroutine: print_hex_byte
;    R7  = scratch
;    R8  = UART TX base address
;    R9  = scratch / word pointer
;    R10 = string pointer for print_str
;    R11 = scratch / temp
;    R12 = scratch / counter
;    R13 = scratch / temp
;    R14 = DSP (data stack pointer, grows downward)
;    R15 = RSP (return/call stack, grows downward)
;
;  Memory map (256 KiB default)
;  ----------------------------
;    0x00000 – ~BIOS end   BIOS code + dictionary + strings
;    dict_free  onward     User dictionary (HERE grows up)
;    ram/2 – 8  downward   Data stack   (R14 shrinks)
;    ram – 8    downward   Return stack (R15 shrinks)
;
;  MMIO
;  ----
;    UART   0xFFFF_FF00_0000_0000   TX=+0 RX=+1 STATUS=+2
;    Timer  0xFFFF_FF00_0000_0100   COUNT=+0..+3 CTRL=+8
;    NIC    0xFFFF_FF00_0000_0400   CMD=+0 STATUS=+1 DMA=+2..+9
;    Mbox   0xFFFF_FF00_0000_0500   DATA=+0..7 SEND=+8 STATUS=+9 ACK=+A
;    Spin   0xFFFF_FF00_0000_0600   ACQUIRE=+N*4 RELEASE=+N*4+1
;    CRC    0xFFFF_FF00_0000_07C0   POLY=+0 INIT=+8 DIN=+10 RESULT=+18 CTRL=+20
;
;  Multicore CSRs
;  ----
;    0x20  COREID    read-only, hardware core ID (0-3)
;    0x21  NCORES    read-only, number of cores
;    0x22  MBOX      read = pending IPI mask; write = send IPI to target
;    0x23  IPIACK    write = ACK IPI from source core
;
;  Per-core stacks (1 MiB RAM)
;  ----
;    Core 0:  RSP=0x100000  DSP=0x80000
;    Core 1:  RSP=0xF0000   DSP=0xE8000  (64 KiB zone, split 32K/32K)
;    Core 2:  RSP=0xE0000   DSP=0xD8000
;    Core 3:  RSP=0xD0000   DSP=0xC8000
; =====================================================================

; === Entry point (address 0x0000) ===
boot:
    ; ---- Multicore gate: route secondary cores away ----
    csrr r0, 0x20                       ; R0 = COREID (CSR 0x20)
    cmpi r0, 0
    lbrne secondary_core_entry          ; cores 1-3 jump away

    ; ---- Core 0 continues normal BIOS boot ----
    ; R15 = RSP = ram top
    mov r15, r2

    ; DSP = ram_size / 2  (data stack grows down from here)
    mov r14, r2
    lsri r14, 1

    ; UART base in R8
    ldi64 r8, 0xFFFF_FF00_0000_0000

    ; Subroutine pointers
    ldi64 r4, emit_char
    ldi64 r5, key_char
    ldi64 r6, print_hex_byte

    ; Enable timer
    ldi64 r11, 0xFFFF_FF00_0000_0108
    ldi r1, 0x01
    st.b r11, r1

    ; Install IVT for bus-fault
    ldi64 r0, ivt_table
    csrw 0x04, r0

    ; --- Initialise Forth variables ---
    ldi r1, 0
    ldi64 r11, var_state
    str r11, r1               ; STATE = 0 (interpreting)

    ldi r1, 10
    ldi64 r11, var_base
    str r11, r1               ; BASE = 10

    ldi64 r1, dict_free
    ldi64 r11, var_here
    str r11, r1               ; HERE = first free byte

    ldi64 r1, latest_entry
    ldi64 r11, var_latest
    str r11, r1               ; LATEST = last built-in word

    ; --- Banner ---
    ldi64 r10, str_banner
    ldi64 r11, print_str
    call.l r11

    ; Print RAM size
    mov r1, r2
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, str_bytes_ram
    ldi64 r11, print_str
    call.l r11

    ; " ok" then fall into QUIT
    ldi64 r11, do_print_ok
    call.l r11

    ; ---- Auto-boot from disk if present ----
    ; Check disk status register bit 7 (disk present)
    ldi64 r11, 0xFFFF_FF00_0000_0201
    ld.b r1, r11
    andi r1, 0x80
    lbreq no_autoboot

    ; Read directory sectors 2-5 into buffer at R2/2
    mov r9, r2
    lsri r9, 1
    ldi r1, 2                 ; start sector
    ldi r12, 4                ; 4 sectors = 2048 bytes = 64 entries
    subi r15, 8
    str r15, r9               ; save buf_addr on RSP
    ldi64 r11, disk_read_sectors
    call.l r11

    ; Scan directory for first Forth-type file (type == 3 at offset +24)
    ldn r9, r15               ; buf_addr
    ldi r0, 0                 ; entry index
autoboot_scan:
    cmpi r0, 64
    lbreq autoboot_not_found

    mov r13, r0
    lsli r13, 5               ; entry_addr = buf + index * 32
    add r13, r9

    ; Check type byte at offset +24
    mov r11, r13
    addi r11, 24
    ld.b r7, r11
    cmpi r7, 3                ; FTYPE_FORTH
    breq autoboot_found

    inc r0
    lbr autoboot_scan

autoboot_found:
    ; R13 = directory entry with type=3
    ; Build "FSLOAD <name>" in a scratch area (RSP-64 is safe)
    addi r15, 8               ; pop buf_addr
    mov r9, r15
    subi r9, 64               ; scratch area below RSP

    ; Write "FSLOAD " prefix (7 bytes)
    ldi r7, 0x46              ; 'F'
    st.b r9, r7
    mov r11, r9
    inc r11
    ldi r7, 0x53              ; 'S'
    st.b r11, r7
    inc r11
    ldi r7, 0x4C              ; 'L'
    st.b r11, r7
    inc r11
    ldi r7, 0x4F              ; 'O'
    st.b r11, r7
    inc r11
    ldi r7, 0x41              ; 'A'
    st.b r11, r7
    inc r11
    ldi r7, 0x44              ; 'D'
    st.b r11, r7
    inc r11
    ldi r7, 0x20              ; ' '
    st.b r11, r7
    inc r11

    ; Copy entry name (up to 15 chars, stop at null)
    ldi r1, 0                 ; name index
autoboot_copy_name:
    cmpi r1, 15
    breq autoboot_name_done
    mov r10, r13
    add r10, r1
    ld.b r7, r10
    cmpi r7, 0
    breq autoboot_name_done
    st.b r11, r7
    inc r11
    inc r1
    br autoboot_copy_name

autoboot_name_done:
    ; R12 = total string length = 7 (prefix) + R1 (name length)
    mov r12, r1
    addi r12, 7

    ; Push ( addr len ) and EVALUATE
    subi r14, 8
    str r14, r9                ; push string addr
    subi r14, 8
    str r14, r12               ; push string len
    ldi64 r11, w_evaluate
    call.l r11
    br no_autoboot

autoboot_not_found:
    addi r15, 8               ; pop buf_addr
no_autoboot:

    ; Fall into the outer interpreter
    ldi64 r11, forth_quit
    call.l r11
    halt

; =====================================================================
;  QUIT — the outer interpreter
; =====================================================================
forth_quit:
    ; Reset RSP to top of RAM each time
    mov r15, r2

quit_loop:
    ; Prompt  "> " if interpreting (STATE=0)
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    brne quit_no_prompt
    ldi r1, 0x3E
    call.l r4
    ldi r1, 0x20
    call.l r4
quit_no_prompt:

    ; Read a line into TIB
    ldi64 r11, read_line
    call.l r11

    ; >IN = 0
    ldi r1, 0
    ldi64 r11, var_to_in
    str r11, r1

    ; --- interpret tokens ---
interp_loop:
    ldi64 r11, parse_word
    call.l r11
    ; R9 = word addr, R12 = length (0 = EOL)
    cmpi r12, 0
    lbreq interp_line_done

    ; Look up in dictionary
    ldi64 r11, find_word
    call.l r11
    ; R9 = entry (0 = not found), R1 = flags byte
    cmpi r9, 0
    breq interp_try_number

    ; Found — check STATE
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    breq interp_execute       ; STATE=0 → interpret

    ; STATE=1 → compiling.  Check IMMEDIATE flag (bit 7)
    mov r0, r1
    andi r0, 0x80
    brne interp_execute       ; IMMEDIATE → execute even in compile mode

    ; Not IMMEDIATE — compile a call to this word
    ldi64 r11, entry_to_code
    call.l r11
    ; R9 = code pointer — compile ldi64 r11,<addr>; call.l r11
    mov r1, r9
    ldi64 r11, compile_call
    call.l r11
    lbr interp_loop

interp_execute:
    ; Compute code field address and execute
    ldi64 r11, entry_to_code
    call.l r11
    ; R9 = code pointer
    call.l r9
    lbr interp_loop

interp_try_number:
    ; Reload saved word addr/len
    ldi64 r11, var_word_addr
    ldn r9, r11
    ldi64 r11, var_word_len
    ldn r12, r11

    ldi64 r11, parse_number
    call.l r11
    ; R1 = value, R0 = 1 success / 0 fail
    cmpi r0, 0
    breq interp_undefined

    ; Check STATE
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    breq interp_push_number

    ; Compiling — emit literal push: ldi64 r1,<val>; subi r14,8; str r14,r1
    ; compile_literal handles this
    ldi64 r11, compile_literal
    call.l r11
    lbr interp_loop

interp_push_number:
    ; Push number (interpret mode)
    subi r14, 8
    str r14, r1
    lbr interp_loop

interp_undefined:
    ; Print word then " ?"
    ldi64 r11, var_word_addr
    ldn r9, r11
    ldi64 r11, var_word_len
    ldn r12, r11
    ldi64 r11, print_counted
    call.l r11
    ldi64 r10, str_undefined
    ldi64 r11, print_str
    call.l r11
    ; Reset STATE to interpret mode (recover from compile-mode errors)
    ldi r1, 0
    ldi64 r11, var_state
    str r11, r1
    ; Abort rest of line
    lbr quit_loop

interp_line_done:
    ; Check for stack underflow: R14 should be <= R2/2 (initial DSP)
    mov r11, r2
    lsri r11, 1               ; dsp_init = R2/2
    cmp r14, r11
    brle interp_no_underflow
    ; Stack underflow detected — reset DSP and warn
    mov r14, r11
    ldi64 r10, str_stack_underflow
    ldi64 r11, print_str
    call.l r11
    lbr quit_loop
interp_no_underflow:
    ldi64 r11, do_print_ok
    call.l r11
    lbr quit_loop

; =====================================================================
;  I/O Primitives
; =====================================================================

; emit_char: write byte R1 to UART
emit_char:
    st.b r8, r1
    ret.l

; key_char: blocking read -> R1
key_char:
    ldi64 r13, 0xFFFF_FF00_0000_0002
kc_poll:
    ld.b r1, r13
    andi r1, 0x02
    cmpi r1, 0
    brne kc_ready
    idl
    br kc_poll
kc_ready:
    ldi64 r13, 0xFFFF_FF00_0000_0001
    ld.b r1, r13
    ret.l

; print_str: null-terminated string at R10, \n → \r\n
print_str:
    ld.b r1, r10
    cmpi r1, 0
    breq ps_done
    cmpi r1, 0x0A
    brne ps_not_lf
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
ps_not_lf:
    st.b r8, r1
    inc r10
    br print_str
ps_done:
    ret.l

; print_counted: R12 bytes starting at R9
print_counted:
    cmpi r12, 0
    breq pcnt_done
    mov r11, r9
    mov r13, r12
pcnt_loop:
    ld.b r1, r11
    call.l r4
    inc r11
    dec r13
    cmpi r13, 0
    brne pcnt_loop
pcnt_done:
    ret.l

; print_hex_byte: R1 low byte -> two hex chars
print_hex_byte:
    mov r0, r1
    lsri r1, 4
    andi r1, 0x0F
    addi r1, 0x30
    cmpi r1, 0x3A
    brcc phb_h
    addi r1, 7
phb_h:
    st.b r8, r1
    mov r1, r0
    andi r1, 0x0F
    addi r1, 0x30
    cmpi r1, 0x3A
    brcc phb_l
    addi r1, 7
phb_l:
    st.b r8, r1
    ret.l

; print_hex32: R1 low 32 bits -> 8 hex chars
print_hex32:
    mov r7, r1
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    mov r1, r7
    lsri r1, 8
    andi r1, 0xFF
    call.l r6
    mov r1, r7
    andi r1, 0xFF
    call.l r6
    ret.l

; print_crlf
print_crlf:
    ldi r1, 0x0D
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    ret.l

; do_print_ok:  print " ok\n"
do_print_ok:
    ldi64 r10, str_ok
    ldi64 r11, print_str
    call.l r11
    ret.l

; print_space
print_space:
    ldi r1, 0x20
    call.l r4
    ret.l

; =====================================================================
;  Line Input
; =====================================================================

; read_line: read into tib_buffer, handle BS, echo.
;   Sets var_tib_len.
read_line:
    ldi64 r9, tib_buffer
    ldi r12, 0
rl_loop:
    call.l r5                 ; key_char -> R1
    ; CR / LF → done
    cmpi r1, 0x0D
    breq rl_done
    cmpi r1, 0x0A
    breq rl_done
    ; Backspace
    cmpi r1, 0x08
    breq rl_bs
    cmpi r1, 0x7F
    breq rl_bs
    ; Printable? >= 0x20
    cmpi r1, 0x20
    brcc rl_loop              ; carry clear = R1 < 0x20 → skip control chars
    ; Store if room (max 255)
    cmpi r12, 0xFF
    breq rl_loop
    mov r13, r9
    add r13, r12
    st.b r13, r1
    inc r12
    call.l r4                 ; echo
    br rl_loop
rl_bs:
    cmpi r12, 0
    breq rl_loop
    dec r12
    ldi r1, 0x08
    call.l r4
    ldi r1, 0x20
    call.l r4
    ldi r1, 0x08
    call.l r4
    br rl_loop
rl_done:
    ldi64 r11, print_crlf
    call.l r11
    ldi64 r11, var_tib_len
    str r11, r12
    ret.l

; =====================================================================
;  Word Parsing
; =====================================================================

; parse_word: skip spaces, delimit next word.
;   Uses var_to_in (offset into TIB).
;   Returns R9=word addr, R12=length (0 if EOL).
;   Saves to var_word_addr / var_word_len.
parse_word:
    ldi64 r11, var_to_in
    ldn r13, r11              ; >IN
    ldi64 r9, tib_buffer
    ldi64 r11, var_tib_len
    ldn r7, r11               ; TIB length

    ; skip leading spaces
pw_skip:
    cmp r13, r7
    breq pw_empty             ; == end
    brgt pw_empty             ; > end
    mov r11, r9
    add r11, r13
    ld.b r0, r11
    cmpi r0, 0x20
    brne pw_start
    inc r13
    br pw_skip
pw_empty:
    ldi r12, 0
    ldi64 r11, var_to_in
    str r11, r13
    ret.l

pw_start:
    mov r12, r13              ; remember start offset

    ; scan to space or end
pw_scan:
    cmp r13, r7
    breq pw_end
    brgt pw_end
    mov r11, r9
    add r11, r13
    ld.b r0, r11
    cmpi r0, 0x20
    breq pw_end
    inc r13
    br pw_scan

pw_end:
    ; Update >IN
    ldi64 r11, var_to_in
    str r11, r13
    ; R9 = TIB + start
    ldi64 r9, tib_buffer
    add r9, r12
    ; Length = current - start
    mov r0, r13
    sub r0, r12
    mov r12, r0
    ; Save for error messages
    ldi64 r11, var_word_addr
    str r11, r9
    ldi64 r11, var_word_len
    str r11, r12
    ret.l

; =====================================================================
;  Dictionary Lookup
; =====================================================================

; find_word: search dictionary for word at R9 (len R12).
;   Returns R9=entry (0=not found), R1=flags byte.
find_word:
    ldi64 r11, var_latest
    ldn r13, r11              ; current entry

fw_loop:
    cmpi r13, 0
    breq fw_miss

    ; flags+len at entry+8
    mov r11, r13
    addi r11, 8
    ld.b r7, r11              ; flags|len
    mov r0, r7
    andi r0, 0x7F             ; name length

    cmp r0, r12
    brne fw_next

    ; Compare names (case-insensitive)
    mov r11, r13
    addi r11, 9               ; entry name start
    ldi r1, 0                 ; index
fw_cmp:
    cmp r1, r12
    breq fw_hit

    ; Entry char → R0
    mov r0, r11
    add r0, r1
    ld.b r0, r0
    ; to upper
    cmpi r0, 0x61
    brcc fw_eu_skip
    cmpi r0, 0x7B
    brcs fw_eu_skip
    subi r0, 0x20
fw_eu_skip:

    ; Search char → R7
    mov r7, r9
    add r7, r1
    ld.b r7, r7
    ; to upper
    cmpi r7, 0x61
    brcc fw_su_skip
    cmpi r7, 0x7B
    brcs fw_su_skip
    subi r7, 0x20
fw_su_skip:

    cmp r0, r7
    brne fw_next
    inc r1
    br fw_cmp

fw_hit:
    mov r9, r13
    mov r11, r13
    addi r11, 8
    ld.b r1, r11              ; flags byte
    ret.l

fw_next:
    ldn r13, r13              ; follow link
    br fw_loop

fw_miss:
    ldi r9, 0
    ret.l

; =====================================================================
;  Entry → Code address
; =====================================================================
; Entry layout: [link:8][flags+len:1][name:N][padding to 2-align][code…]
; We align to 2 bytes because the smallest instruction is 1 byte and
; ldi64 trampoline is 13 bytes; no strict alignment is needed.
; Actually let's keep it simple: skip link(8)+flags(1)+name(N), then
; round up to even address so ldi64 encoding is clean.

entry_to_code:
    mov r11, r9
    addi r11, 8               ; skip link
    ld.b r0, r11              ; flags+len
    andi r0, 0x7F
    addi r11, 1               ; skip flags byte
    add r11, r0               ; skip name
    ; No alignment needed — Megapad-64 is byte-addressable
    mov r9, r11
    ret.l

; =====================================================================
;  Number Parsing
; =====================================================================
; parse_number: word at R9 (len R12) → R1=value, R0=1/0 success.
; Handles '-' prefix and '0x' prefix.  Uses var_base otherwise.

parse_number:
    ldi r1, 0                 ; accumulator
    ldi r0, 0                 ; sign
    mov r11, r9
    mov r13, r12

    cmpi r13, 0
    lbreq pn_fail

    ; Check '-'
    ld.b r7, r11
    cmpi r7, 0x2D
    brne pn_no_neg
    ldi r0, 1
    inc r11
    dec r13
    cmpi r13, 0
    lbreq pn_fail
pn_no_neg:

    ; Save sign on data stack
    subi r14, 8
    str r14, r0

    ; Check '0x' / '0X'
    cmpi r13, 2
    lbrcc pn_base             ; <2 chars → use BASE
    ld.b r7, r11
    cmpi r7, 0x30
    lbrne pn_base
    mov r7, r11
    inc r7
    ld.b r7, r7
    cmpi r7, 0x78
    breq pn_hex_prefix
    cmpi r7, 0x58
    breq pn_hex_prefix
    lbr pn_base

pn_hex_prefix:
    addi r11, 2
    subi r13, 2
    cmpi r13, 0
    lbreq pn_pop_fail
    ldi r1, 0

pn_hex_loop:
    cmpi r13, 0
    lbreq pn_pop_done
    ld.b r7, r11
    ; '0'-'9'
    subi r7, 0x30
    cmpi r7, 10
    brcc pn_hex_ok
    ; 'A'-'F'
    addi r7, 0x30
    subi r7, 0x41
    cmpi r7, 6
    brcs pn_hex_lc
    addi r7, 10
    br pn_hex_ok
pn_hex_lc:
    addi r7, 0x41
    subi r7, 0x61
    cmpi r7, 6
    lbrcs pn_pop_fail
    addi r7, 10
pn_hex_ok:
    lsli r1, 4
    add r1, r7
    inc r11
    dec r13
    br pn_hex_loop

pn_base:
    ; Parse using BASE
    ldi r1, 0
    ldi64 r0, var_base
    ldn r0, r0                ; R0 = BASE

pn_b_loop:
    cmpi r13, 0
    lbreq pn_pop_done
    ld.b r7, r11
    ; '0'-'9'
    subi r7, 0x30
    cmpi r7, 10
    brcc pn_b_digit
    ; 'A'-'Z'
    addi r7, 0x30
    subi r7, 0x41
    cmpi r7, 26
    brcs pn_b_lc
    addi r7, 10
    lbr pn_b_range
pn_b_lc:
    addi r7, 0x41
    subi r7, 0x61
    cmpi r7, 26
    lbrcs pn_pop_fail
    addi r7, 10
pn_b_range:
    ; digit < base?
    cmp r7, r0
    lbreq pn_pop_fail
    lbrgt pn_pop_fail
pn_b_digit:
    ; also check digit < base for 0-9 path
    cmp r7, r0
    lbreq pn_pop_fail
    lbrgt pn_pop_fail
    mul r1, r0
    add r1, r7
    inc r11
    dec r13
    br pn_b_loop

pn_pop_done:
    ; Pop sign and apply
    ldn r0, r14
    addi r14, 8
    cmpi r0, 0
    breq pn_ok
    neg r1, r1
pn_ok:
    ldi r0, 1
    ret.l

pn_pop_fail:
    ldn r0, r14
    addi r14, 8
pn_fail:
    ldi r0, 0
    ret.l

; =====================================================================
;  Number Printing
; =====================================================================

; print_number: signed 64-bit in R1 using BASE.
print_number:
    ; Check sign (bit 63)
    mov r13, r1
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    andi r7, 0x80
    cmpi r7, 0
    breq pnum_pos
    ; Negative
    ldi r1, 0x2D
    call.l r4
    neg r13, r13
pnum_pos:
    mov r1, r13
    ; fall through to print_unsigned

; print_unsigned: unsigned 64-bit in R1 using BASE.
print_unsigned:
    ldi64 r11, var_base
    ldn r11, r11              ; base
    ldi r12, 0                ; digit count

pu_div:
    udiv r1, r11              ; R1 = quot, R0 = rem
    ; Convert rem to ASCII
    cmpi r0, 10
    brcc pu_num               ; carry clear = R0 < 10
    addi r0, 0x37             ; R0 >= 10: 'A'-10 = 0x37 + digit
    br pu_push
pu_num:
    addi r0, 0x30             ; R0 < 10: '0' = 0x30 + digit
pu_push:
    subi r14, 8
    str r14, r0
    inc r12
    cmpi r1, 0
    brne pu_div

    ; Print digits (reverse order on stack)
pu_emit:
    cmpi r12, 0
    breq pu_done
    ldn r1, r14
    addi r14, 8
    call.l r4
    dec r12
    br pu_emit
pu_done:
    ret.l

; =====================================================================
;  Forth Words — Stack
; =====================================================================

; DUP ( a -- a a )
w_dup:
    ldn r1, r14
    subi r14, 8
    str r14, r1
    ret.l

; DROP ( a -- )
w_drop:
    addi r14, 8
    ret.l

; SWAP ( a b -- b a )
w_swap:
    ldn r1, r14
    mov r11, r14
    addi r11, 8
    ldn r0, r11
    str r14, r0
    str r11, r1
    ret.l

; OVER ( a b -- a b a )
w_over:
    mov r11, r14
    addi r11, 8
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; ROT ( a b c -- b c a )
w_rot:
    ldn r1, r14               ; c
    mov r11, r14
    addi r11, 8
    ldn r0, r11               ; b
    mov r7, r14
    addi r7, 16
    ldn r13, r7               ; a
    str r14, r13              ; TOS=a
    str r11, r1               ; NOS=c
    str r7, r0                ; 3rd=b
    ret.l

; NIP ( a b -- b )
w_nip:
    ldn r1, r14
    addi r14, 8
    str r14, r1
    ret.l

; TUCK ( a b -- b a b )
w_tuck:
    ldn r1, r14               ; b
    mov r11, r14
    addi r11, 8
    ldn r0, r11               ; a
    str r11, r1
    str r14, r0
    subi r14, 8
    str r14, r1
    ret.l

; 2DUP ( a b -- a b a b )
w_2dup:
    ldn r1, r14               ; b
    mov r11, r14
    addi r11, 8
    ldn r0, r11               ; a
    subi r14, 8
    str r14, r0
    subi r14, 8
    str r14, r1
    ret.l

; 2DROP ( a b -- )
w_2drop:
    addi r14, 16
    ret.l

; DEPTH ( -- n )
w_depth:
    mov r1, r2
    lsri r1, 1
    sub r1, r14
    lsri r1, 3
    subi r14, 8
    str r14, r1
    ret.l

; PICK ( n -- x )
w_pick:
    ldn r1, r14
    addi r14, 8
    lsli r1, 3
    mov r11, r14
    add r11, r1
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; =====================================================================
;  Forth Words — Arithmetic
; =====================================================================

; + ( a b -- a+b )
w_plus:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    add r0, r1
    str r14, r0
    ret.l

; - ( a b -- a-b )
w_minus:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    sub r0, r1
    str r14, r0
    ret.l

; * ( a b -- a*b )
w_star:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    mul r0, r1
    str r14, r0
    ret.l

; / ( a b -- quot )  signed
;   Note: div Rd,Rs puts quotient in Rd and remainder in R0.
;   When Rd=R0, the remainder overwrites the quotient.  So we use R7
;   as the dividend register to avoid the conflict.
w_slash:
    ldn r1, r14
    addi r14, 8
    ldn r7, r14
    div r7, r1
    str r14, r7
    ret.l

; MOD ( a b -- rem )  signed
w_mod:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    mod r0, r1
    str r14, r0
    ret.l

; /MOD ( a b -- rem quot )
w_slashmod:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r7, r14               ; a
    mov r0, r7
    div r7, r1                ; r7=quot, r0=rem (per ISA: R0 gets rem)
    str r14, r0               ; NOS = rem
    subi r14, 8
    str r14, r7               ; TOS = quot
    ret.l

; NEGATE ( n -- -n )
w_negate:
    ldn r1, r14
    neg r1, r1
    str r14, r1
    ret.l

; ABS ( n -- |n| )
w_abs:
    ldn r1, r14
    mov r0, r1
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    andi r0, 0x80
    cmpi r0, 0
    breq w_abs_ok
    neg r1, r1
    str r14, r1
w_abs_ok:
    ret.l

; 1+ ( n -- n+1 )
w_1plus:
    ldn r1, r14
    inc r1
    str r14, r1
    ret.l

; 1- ( n -- n-1 )
w_1minus:
    ldn r1, r14
    dec r1
    str r14, r1
    ret.l

; =====================================================================
;  Forth Words — Logic & Comparison
; =====================================================================

; AND ( a b -- a&b )
w_and:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    and r0, r1
    str r14, r0
    ret.l

; OR ( a b -- a|b )
w_or:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    or r0, r1
    str r14, r0
    ret.l

; XOR ( a b -- a^b )
w_xor:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    xor r0, r1
    str r14, r0
    ret.l

; INVERT ( a -- ~a )
w_invert:
    ldn r1, r14
    not r1, r1
    str r14, r1
    ret.l

; LSHIFT ( a n -- a<<n )
w_lshift:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    shl r0, r1
    str r14, r0
    ret.l

; RSHIFT ( a n -- a>>n )
w_rshift:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    shr r0, r1
    str r14, r0
    ret.l

; = ( a b -- flag )  TRUE = -1
w_equal:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    cmp r0, r1
    ldi r0, 0
    brne w_eq_z
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_eq_z:
    str r14, r0
    ret.l

; < ( a b -- flag ) signed less-than
w_less:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ; signed less: N xor V after CMP
    ldi r0, 0
    csrr r0, 0x00             ; FLAGS
    mov r7, r0
    lsri r7, 2
    andi r7, 0x01             ; N
    mov r11, r0
    lsri r11, 3
    andi r11, 0x01            ; V
    xor r7, r11               ; N^V=1 ⇒ a < b
    ldi r0, 0
    cmpi r7, 0
    breq w_lt_z
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_lt_z:
    str r14, r0
    ret.l

; > ( a b -- flag ) signed greater-than
w_greater:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ldi r0, 0
    csrr r0, 0x00             ; FLAGS: Z=0, N=2, V=3, G=5
    mov r7, r0
    andi r7, 0x01             ; Z
    cmpi r7, 0
    brne w_gt_z               ; equal → false

    mov r7, r0
    lsri r7, 2
    andi r7, 0x01             ; N
    mov r11, r0
    lsri r11, 3
    andi r11, 0x01            ; V
    xor r7, r11
    ; N^V = 0 and Z=0 → greater
    cmpi r7, 0
    brne w_gt_z
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
    str r14, r0
    ret.l
w_gt_z:
    ldi r0, 0
    str r14, r0
    ret.l

; 0= ( n -- flag )
w_0eq:
    ldn r1, r14
    ldi r0, 0
    cmpi r1, 0
    brne w_0eq_z
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_0eq_z:
    str r14, r0
    ret.l

; 0< ( n -- flag )
w_0lt:
    ldn r1, r14
    mov r0, r1
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    andi r0, 0x80
    ldi r1, 0
    cmpi r0, 0
    breq w_0lt_z
    ldi64 r1, 0xFFFFFFFFFFFFFFFF
w_0lt_z:
    str r14, r1
    ret.l

; =====================================================================
;  Forth Words — Memory
; =====================================================================

; @ ( addr -- val )
w_fetch:
    ldn r1, r14
    ldn r0, r1
    str r14, r0
    ret.l

; ! ( val addr -- )
w_store:
    ldn r1, r14               ; addr
    addi r14, 8
    ldn r0, r14               ; val
    addi r14, 8
    str r1, r0
    ret.l

; C@ ( addr -- byte )
w_cfetch:
    ldn r1, r14
    ld.b r0, r1
    str r14, r0
    ret.l

; C! ( byte addr -- )
w_cstore:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    addi r14, 8
    st.b r1, r0
    ret.l

; HERE ( -- addr )
w_here:
    ldi64 r11, var_here
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; ALLOT ( n -- )
w_allot:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    add r0, r1
    str r11, r0
    ret.l

; , ( x -- ) store cell at HERE, advance HERE by 8
w_comma:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    str r0, r1
    addi r0, 8
    str r11, r0
    ret.l

; C, ( c -- ) store byte at HERE, advance 1
w_ccomma:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    st.b r0, r1
    inc r0
    str r11, r0
    ret.l

; =====================================================================
;  Forth Words — I/O
; =====================================================================

; EMIT ( c -- )
w_emit:
    ldn r1, r14
    addi r14, 8
    call.l r4
    ret.l

; KEY ( -- c )
w_key:
    call.l r5
    subi r14, 8
    str r14, r1
    ret.l

; KEY? ( -- flag )  non-blocking: true if a key is available
w_key_query:
    ldi64 r13, 0xFFFF_FF00_0000_0002   ; UART STATUS register
    ld.b r1, r13
    andi r1, 0x02                       ; bit 1 = RX_AVAIL
    cmpi r1, 0
    ldi r0, 0                           ; assume false
    breq kq_done
    ldi64 r0, 0xFFFFFFFFFFFFFFFF        ; TRUE = -1
kq_done:
    subi r14, 8
    str r14, r0
    ret.l

; CR ( -- )
w_cr:
    ldi64 r11, print_crlf
    call.l r11
    ret.l

; . ( n -- ) print signed + space
w_dot:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, print_number
    call.l r11
    ldi64 r11, print_space
    call.l r11
    ret.l

; U. ( u -- ) print unsigned + space
w_udot:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, print_unsigned
    call.l r11
    ldi64 r11, print_space
    call.l r11
    ret.l

; .S ( -- ) non-destructive stack print
w_dotS:
    ; <depth> items from bottom to top
    ldi r1, 0x3C
    call.l r4
    ; depth
    mov r1, r2
    lsri r1, 1
    sub r1, r14
    lsri r1, 3
    mov r13, r1
    ldi64 r11, print_number
    call.l r11
    ldi64 r10, str_stk_hdr
    ldi64 r11, print_str
    call.l r11
    ; Print bottom → top
    cmpi r13, 0
    breq ds_done
    mov r11, r2
    lsri r11, 1               ; dsp_init
ds_loop:
    subi r11, 8
    cmp r11, r14
    brcc ds_done
    ldn r1, r11
    ; save R11 on return stack
    subi r15, 8
    str r15, r11
    ldi64 r12, print_number
    call.l r12
    ldi64 r12, print_space
    call.l r12
    ldn r11, r15
    addi r15, 8
    br ds_loop
ds_done:
    ret.l

; HEX ( -- )
w_hex:
    ldi r1, 16
    ldi64 r11, var_base
    str r11, r1
    ret.l

; DECIMAL ( -- )
w_decimal:
    ldi r1, 10
    ldi64 r11, var_base
    str r11, r1
    ret.l

; BASE ( -- addr )
w_base_addr:
    ldi64 r1, var_base
    subi r14, 8
    str r14, r1
    ret.l

; WORDS ( -- )
w_words:
    ldi64 r11, var_latest
    ldn r13, r11
wds_loop:
    cmpi r13, 0
    breq wds_done
    mov r11, r13
    addi r11, 8
    ld.b r12, r11
    andi r12, 0x7F
    inc r11
    ; save R13
    subi r15, 8
    str r15, r13
    mov r9, r11
    ldi64 r11, print_counted
    call.l r11
    ldi64 r11, print_space
    call.l r11
    ldn r13, r15
    addi r15, 8
    ldn r13, r13              ; follow link
    br wds_loop
wds_done:
    ldi64 r11, print_crlf
    call.l r11
    ret.l

; BYE ( -- )
w_bye:
    ldi64 r10, str_bye
    ldi64 r11, print_str
    call.l r11
    halt

; =====================================================================
;  Forth Words — Memory Dump / Fill
; =====================================================================

; DUMP ( addr n -- )
w_dump:
    ldn r12, r14              ; n
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
dm_outer:
    cmpi r12, 0
    breq dm_done
    mov r1, r9
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x3A
    call.l r4
    ldi r1, 0x20
    call.l r4
    ldi r13, 16
dm_inner:
    cmpi r12, 0
    breq dm_nl
    cmpi r13, 0
    breq dm_nl
    ld.b r1, r9
    call.l r6
    ldi r1, 0x20
    call.l r4
    inc r9
    dec r12
    dec r13
    br dm_inner
dm_nl:
    ldi64 r11, print_crlf
    call.l r11
    br dm_outer
dm_done:
    ret.l

; FILL ( addr n byte -- )
w_fill:
    ldn r7, r14               ; byte
    addi r14, 8
    ldn r12, r14              ; n
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
fl_loop:
    cmpi r12, 0
    breq fl_done
    st.b r9, r7
    inc r9
    dec r12
    br fl_loop
fl_done:
    ret.l

; =====================================================================
;  Forth Words — Tile Engine
; =====================================================================

; TI ( -- ) print tile CSR info
w_ti:
    ldi64 r10, str_ti_mode
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x14
    call.l r6
    ldi64 r10, str_ti_ctrl
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x15
    call.l r6
    ldi64 r11, print_crlf
    call.l r11
    ldi64 r10, str_ti_src0
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x16
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, str_ti_src1
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x17
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r10, str_ti_dst
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x18
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    ldi64 r10, str_ti_acc
    ldi64 r11, print_str
    call.l r11
    csrr r1, 0x19
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    call.l r4
    csrr r1, 0x1A
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    call.l r4
    csrr r1, 0x1B
    ldi64 r11, print_hex32
    call.l r11
    ldi r1, 0x20
    call.l r4
    csrr r1, 0x1C
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    ret.l

; TVIEW ( addr -- ) display 64 bytes as 4×16 hex grid
w_tview:
    ldn r9, r14
    addi r14, 8
    ldi r13, 4
tv_o:
    ldi r12, 16
tv_i:
    ld.b r1, r9
    call.l r6
    ldi r1, 0x20
    call.l r4
    inc r9
    dec r12
    cmpi r12, 0
    brne tv_i
    ldi64 r11, print_crlf
    call.l r11
    dec r13
    cmpi r13, 0
    brne tv_o
    ret.l

; TFILL ( addr byte -- ) fill 64 bytes
w_tfill:
    ldn r7, r14
    addi r14, 8
    ldn r9, r14
    addi r14, 8
    ldi r12, 64
tf_lp:
    st.b r9, r7
    inc r9
    dec r12
    cmpi r12, 0
    brne tf_lp
    ret.l

; TSRC0! ( addr -- )
w_tsrc0:
    ldn r0, r14
    addi r14, 8
    csrw 0x16, r0
    ret.l

; TSRC1! ( addr -- )
w_tsrc1:
    ldn r0, r14
    addi r14, 8
    csrw 0x17, r0
    ret.l

; TDST! ( addr -- )
w_tdst:
    ldn r0, r14
    addi r14, 8
    csrw 0x18, r0
    ret.l

; TMODE! ( n -- )
w_tmode:
    ldn r0, r14
    addi r14, 8
    csrw 0x14, r0
    ret.l

; TCTRL! ( n -- )
w_tctrl:
    ldn r0, r14
    addi r14, 8
    csrw 0x15, r0
    ret.l

; Tile operations (no stack args)
w_tadd:
    t.add
    ret.l
w_tsub:
    t.sub
    ret.l
w_tand:
    t.and
    ret.l
w_tor:
    t.or
    ret.l
w_txor:
    t.xor
    ret.l
w_tmul:
    t.mul
    ret.l
w_tdot:
    t.dot
    ret.l
w_tsum:
    t.sum
    ret.l
w_tmin:
    t.rmin
    ret.l
w_tmax:
    t.rmax
    ret.l
w_ttrans:
    t.trans
    ret.l
w_tzero:
    t.zero
    ret.l

; CYCLES ( -- n )
w_cycles:
    ldi64 r11, 0xFFFF_FF00_0000_0100
    ld.w r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; ACC@ ( -- n ) read ACC0 to stack
w_acc_fetch:
    csrr r0, 0x19
    subi r14, 8
    str r14, r0
    ret.l

; ACC1@ ( -- n ) read ACC1 to stack
w_acc1_fetch:
    csrr r0, 0x1A
    subi r14, 8
    str r14, r0
    ret.l

; ACC2@ ( -- n ) read ACC2 to stack
w_acc2_fetch:
    csrr r0, 0x1B
    subi r14, 8
    str r14, r0
    ret.l

; ACC3@ ( -- n ) read ACC3 to stack
w_acc3_fetch:
    csrr r0, 0x1C
    subi r14, 8
    str r14, r0
    ret.l

; TPOPCNT ( -- ) popcount reduction, result in ACC
w_tpopcnt:
    t.popcnt
    ret.l

; TL1 ( -- ) L1 norm reduction, result in ACC
w_tl1:
    t.l1
    ret.l

; TEMIN ( -- ) element-wise min, writes to DST tile
w_temin:
    t.min
    ret.l

; TEMAX ( -- ) element-wise max, writes to DST tile
w_temax:
    t.max
    ret.l

; TABS ( -- ) element-wise absolute value, writes to DST tile
w_tabs:
    t.abs
    ret.l

; TSUMSQ ( -- ) sum-of-squares reduction, result in ACC
w_tsumsq:
    t.sumsq
    ret.l

; TMINIDX ( -- ) argmin reduction, index in ACC0, value in ACC1
w_tminidx:
    t.minidx
    ret.l

; TMAXIDX ( -- ) argmax reduction, index in ACC0, value in ACC1
w_tmaxidx:
    t.maxidx
    ret.l

; TWMUL ( -- ) widening multiply, output to TDST and TDST+64
w_twmul:
    t.wmul
    ret.l

; TMAC ( -- ) multiply-accumulate in-place: dst[i] += a[i]*b[i]
w_tmac:
    t.mac
    ret.l

; TFMA ( -- ) fused multiply-add: dst[i] = a[i]*b[i] + dst[i]
w_tfma:
    t.fma
    ret.l

; TDOTACC ( -- ) 4-way chunked dot product into ACC0-ACC3
w_tdotacc:
    t.dotacc
    ret.l

; TMODE@ ( -- n ) read current tile mode
w_tmode_fetch:
    csrr r0, 0x14
    subi r14, 8
    str r14, r0
    ret.l

; TCTRL@ ( -- n ) read current tile ctrl
w_tctrl_fetch:
    csrr r0, 0x15
    subi r14, 8
    str r14, r0
    ret.l

; EXECUTE ( xt -- ) call execution token
w_execute:
    ldn r9, r14
    addi r14, 8
    call.l r9
    ret.l

; ' ( "name" -- xt ) find word, push code field address
w_tick:
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    breq tick_fail
    ldi64 r11, find_word
    call.l r11
    cmpi r9, 0
    breq tick_fail
    ldi64 r11, entry_to_code
    call.l r11
    subi r14, 8
    str r14, r9
    ret.l
tick_fail:
    ldi r0, 0
    subi r14, 8
    str r14, r0
    ret.l

; =====================================================================
;  Compilation Helpers
; =====================================================================

; compile_call: emit "ldi64 r11, <addr>; call.l r11" at HERE.
;   R1 = target address.  Advances HERE by 13 bytes.
;   ldi64 r11,val = 0xF0 0x60 0xB0 <8 bytes LE>  = 11 bytes
;   call.l r11    = 0x0D 0x0B                      = 2 bytes
compile_call:
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE
    ; EXT prefix: 0xF0
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ; LDI opcode: 0x60
    ldi r7, 0x60
    st.b r0, r7
    inc r0
    ; Register byte: 0xB0 (r11 << 4)
    ldi r7, 0xB0
    st.b r0, r7
    inc r0
    ; 8 bytes of address (little-endian)
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ; CALL.L r11: 0x0D 0x0B
    ldi r7, 0x0D
    st.b r0, r7
    inc r0
    ldi r7, 0x0B
    st.b r0, r7
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ret.l

; compile_literal: emit code to push R1 value onto data stack at HERE.
;   Emits: ldi64 r1, <val>; subi r14, 8; str r14, r1
;   ldi64 r1,val = 0xF0 0x60 0x10 <8 bytes LE> = 11 bytes
;   subi r14, 8  = 0x67 0xE0 0x08               = 3 bytes
;   str r14, r1  = 0x54 0xE1                     = 2 bytes
;   Total = 16 bytes
compile_literal:
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE
    ; ldi64 r1: 0xF0 0x60 0x10
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x60
    st.b r0, r7
    inc r0
    ldi r7, 0x10
    st.b r0, r7
    inc r0
    ; 8 bytes of value (LE)
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ; subi r14, 8: 0x67 0xE0 0x08
    ldi r7, 0x67
    st.b r0, r7
    inc r0
    ldi r7, 0xE0
    st.b r0, r7
    inc r0
    ldi r7, 0x08
    st.b r0, r7
    inc r0
    ; str r14, r1: 0x54 0xE1
    ldi r7, 0x54
    st.b r0, r7
    inc r0
    ldi r7, 0xE1
    st.b r0, r7
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ret.l

; compile_byte: emit one byte from R1 at HERE, advance HERE.
compile_byte:
    ldi64 r11, var_here
    ldn r0, r11
    st.b r0, r1
    inc r0
    str r11, r0
    ret.l

; compile_ret: emit ret.l (0x0E) at HERE.
compile_ret:
    ldi r1, 0x0E
    ldi64 r11, var_here
    ldn r0, r11
    st.b r0, r1
    inc r0
    str r11, r0
    ret.l

; =====================================================================
;  : (colon) — begin a new word definition
; =====================================================================
;  Creates dict entry: [link:8][flags+len:1][name:N]
;  Then sets STATE=1 (compiling).  Code starts at HERE after the header.

w_colon:
    ; Check dictionary space: HERE + 1024 < R14 (data stack pointer)
    ldi64 r11, var_here
    ldn r11, r11
    addi r11, 1024             ; HERE + safety margin
    cmp r11, r14
    brle w_colon_space_ok
    ; Dictionary full!
    ldi64 r10, str_dict_full
    ldi64 r11, print_str
    call.l r11
    ret.l
w_colon_space_ok:
    ; Parse the name
    ldi64 r11, parse_word
    call.l r11
    ; R9=word addr, R12=word length
    cmpi r12, 0
    lbreq w_colon_err          ; no name given

    ; Save name addr/len on data stack temporarily
    subi r14, 8
    str r14, r9               ; name addr
    subi r14, 8
    str r14, r12              ; name len

    ; Write link field: current LATEST at HERE
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE (start of new entry)

    ; Save entry start for later (to set LATEST)
    subi r14, 8
    str r14, r0               ; entry addr on stack

    ldi64 r11, var_latest
    ldn r1, r11               ; R1 = current LATEST
    str r0, r1                ; store link
    addi r0, 8                ; skip link

    ; Write flags+len byte
    ldn r12, r14              ; peek entry addr (ignore — we need name len)
    ; Actually name len is at DSP+8
    mov r11, r14
    addi r11, 8
    ldn r12, r11              ; name len
    st.b r0, r12
    inc r0

    ; Copy name bytes
    mov r11, r14
    addi r11, 16
    ldn r9, r11               ; name addr
    ldi r1, 0
w_colon_copy:
    cmp r1, r12
    breq w_colon_name_done
    mov r7, r9
    add r7, r1
    ld.b r7, r7
    st.b r0, r7
    inc r0
    inc r1
    br w_colon_copy

w_colon_name_done:
    ; Update HERE to after the header (code starts here)
    ldi64 r11, var_here
    str r11, r0

    ; Set LATEST to entry start
    ldn r0, r14               ; entry addr
    ldi64 r11, var_latest
    str r11, r0

    ; Drop temps from data stack (entry addr, name len, name addr)
    addi r14, 24

    ; Save code-start address for ; to use if needed
    ; (not strictly needed for subroutine threading — code just follows)

    ; STATE = 1 (compiling)
    ldi r1, 1
    ldi64 r11, var_state
    str r11, r1
    ret.l

w_colon_err:
    ldi64 r10, str_no_name
    ldi64 r11, print_str
    call.l r11
    ret.l

; =====================================================================
;  ; (semicolon, IMMEDIATE) — end definition
; =====================================================================
w_semicolon:
    ; Compile ret.l
    ldi64 r11, compile_ret
    call.l r11
    ; STATE = 0 (interpreting)
    ldi r1, 0
    ldi64 r11, var_state
    str r11, r1
    ret.l

; =====================================================================
;  Control Flow (all IMMEDIATE)
; =====================================================================

; IF ( flag -- )  compile: pop, test, forward branch if zero
;   Emits: ldn r1, r14; addi r14, 8; cmpi r1, 0; lbreq <placeholder>
;   Pushes fixup address onto data stack
w_if:
    ; Compile: ldn r1, r14   → 0x50 0xE1  (LDN Rx,Ry = 0x50, xE|y1 → 0xE1? no...)
    ; LDN is MEM F=5,N=0, regbyte = dst<<4|src → r1,r14 = 0x1E
    ; Wait — let me re-check the encoding.
    ; MEM family: byte0 = 0x50 | subop.  LDN = subop 0 → 0x50
    ; byte1 = (dst << 4) | src → (1 << 4) | 14 = 0x1E
    ; So LDN r1, r14 = 0x50, 0x1E
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8 = 0x62 0xE0 0x08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; cmpi r1, 0 = 0x66 0x10 0x00
    ldi r1, 0x66
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; lbreq <placeholder> → 0x41 0xXX 0xXX (LBREQ = LBR family, cond=EQ=1)
    ; LBR family: F=4, N=cond. EQ=1 → byte0 = 0x41
    ; Then 16-bit signed offset (LE)
    ; Push current HERE (= address of the offset bytes) for fixup
    ldi64 r11, var_here
    ldn r1, r11
    ; Compile the branch opcode
    mov r0, r1
    ldi r7, 0x41
    st.b r0, r7
    inc r0
    ; Push fixup address (where offset bytes go)
    subi r14, 8
    str r14, r0
    ; Write placeholder offset (0x0000)
    ldi r7, 0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ret.l

; ELSE (IMMEDIATE) — resolve IF forward branch, start new forward branch
w_else:
    ; First compile an unconditional forward branch (lbr <placeholder>)
    ; LBR unconditional: F=4, N=0 → 0x40
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x40
    st.b r0, r7
    inc r0
    ; Save new fixup address
    mov r13, r0               ; save else-fixup addr
    ; Placeholder
    ldi r7, 0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0

    ; Now resolve the IF fixup: pop IF's fixup addr, patch it
    ; fixup_addr = opcode+1 (where offset bytes start)
    ; offset = target - (fixup_addr + 2) = target - (opcode + 3)
    ldn r9, r14               ; IF's fixup address
    addi r14, 8               ; pop
    ldi64 r11, var_here
    ldn r0, r11               ; current HERE = target
    mov r1, r0
    sub r1, r9                ; r1 = target - fixup_addr
    subi r1, 2                ; r1 = target - (fixup_addr + 2)
    ; Patch the 16-bit offset (big-endian: high byte first)
    mov r7, r1
    lsri r7, 8
    st.b r9, r7               ; high byte at fixup_addr
    inc r9
    st.b r9, r1               ; low byte at fixup_addr+1

    ; Push the ELSE fixup address
    subi r14, 8
    str r14, r13
    ret.l

; THEN (IMMEDIATE) — resolve forward branch from IF or ELSE
w_then:
    ; Pop fixup address (= opcode+1, where offset bytes start)
    ldn r9, r14
    addi r14, 8
    ; offset = target - (fixup_addr + 2)
    ldi64 r11, var_here
    ldn r0, r11               ; target = HERE
    mov r1, r0
    sub r1, r9                ; r1 = target - fixup_addr
    subi r1, 2                ; r1 = target - (fixup_addr + 2)
    ; Patch 16-bit big-endian offset
    mov r7, r1
    lsri r7, 8
    st.b r9, r7               ; high byte at fixup_addr
    inc r9
    st.b r9, r1               ; low byte at fixup_addr+1
    ret.l

; BEGIN (IMMEDIATE) — push current HERE as loop target
w_begin:
    ldi64 r11, var_here
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; UNTIL (IMMEDIATE) — compile: pop, test, branch back if zero
;   Emits: ldn r1, r14; addi r14, 8; cmpi r1, 0; lbreq <back>
w_until:
    ; Compile: ldn r1, r14 (0x50, 0x1E)
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8 = 0x62 0xE0 0x08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; cmpi r1, 0 = 0x66 0x10 0x00
    ldi r1, 0x66
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; lbreq <back> — offset = target - (opcode_addr + 3)
    ldn r9, r14               ; BEGIN target addr
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11               ; current HERE = position of lbreq opcode
    ; offset = target - (HERE + 3)   (PC is at HERE+3 after consuming 3-byte LBR)
    mov r1, r9
    sub r1, r0
    subi r1, 3                ; offset = target - (HERE + 3)
    ; Compile lbreq (0x41) + 16-bit big-endian offset
    ldi r7, 0x41
    st.b r0, r7
    inc r0
    ; Write big-endian: high byte first, then low byte
    mov r7, r1
    lsri r7, 8
    st.b r0, r7               ; high byte
    inc r0
    st.b r0, r1               ; low byte
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ret.l

; WHILE (IMMEDIATE) — like IF but inside BEGIN..WHILE..REPEAT
;   Compiles conditional forward branch, pushes fixup addr
;   Stack: ( begin-addr -- begin-addr fixup-addr )
w_while:
    ; Same as IF — compile test and forward branch
    ; Save begin-addr temporarily
    ldn r9, r14               ; begin-addr (keep on stack)
    ; Compile: ldn r1, r14
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8 = 0x62 0xE0 0x08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; cmpi r1, 0 = 0x66 0x10 0x00
    ldi r1, 0x66
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; lbreq <placeholder>
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x41
    st.b r0, r7
    inc r0
    ; Push fixup addr (ON TOP of begin-addr which is already on stack)
    subi r14, 8
    str r14, r0
    ; Placeholder
    ldi r7, 0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    ldi64 r11, var_here
    str r11, r0
    ret.l

; REPEAT (IMMEDIATE) — compile backward branch to BEGIN, resolve WHILE
;   Stack: ( begin-addr fixup-addr -- )
w_repeat:
    ; Pop WHILE fixup addr
    ldn r13, r14              ; while-fixup
    addi r14, 8
    ; Pop BEGIN addr
    ldn r9, r14               ; begin-addr
    addi r14, 8
    ; Compile unconditional backward branch: lbr <begin>
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x40              ; LBR unconditional
    st.b r0, r7
    inc r0
    ; offset = begin-addr - (opcode_addr+3); R0 is at opcode+1
    mov r1, r9
    sub r1, r0
    subi r1, 2                ; offset = begin-addr - (R0 + 2) = begin-addr - (opcode+3)
    ; store 16-bit big-endian offset
    mov r7, r1
    lsri r7, 8
    st.b r0, r7               ; high byte
    inc r0
    st.b r0, r1               ; low byte
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ; Now resolve WHILE forward branch
    ; fixup at r13, target = HERE; offset = HERE - (fixup + 2)
    ldi64 r11, var_here
    ldn r0, r11
    mov r1, r0
    sub r1, r13
    subi r1, 2                ; offset = target - (fixup_addr + 2)
    ; Patch big-endian: high byte first
    mov r7, r1
    lsri r7, 8
    st.b r13, r7              ; high byte at fixup_addr
    inc r13
    st.b r13, r1              ; low byte at fixup_addr+1
    ret.l

; DO (IMMEDIATE) — start a counted loop
;   ( limit index -- ) at runtime
;   Compiles: move limit and index to return stack
;   Pushes HERE as loop target
w_do:
    ; Compile at runtime:
    ;   ldn r1, r14       ; index (TOS)          = 50 1E
    ;   addi r14, 8                               = 62 E0 08
    ;   ldn r7, r14       ; limit (NOS)           = 50 7E
    ;   addi r14, 8                               = 62 E0 08
    ;   subi r15, 8       ; push limit to RSP     = 67 F0 08
    ;   str r15, r7                               = 54 F7
    ;   subi r15, 8       ; push index to RSP     = 67 F0 08
    ;   str r15, r1                               = 54 F1
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x7E
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF7
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF1
    ldi64 r11, compile_byte
    call.l r11
    ; Push loop-target = HERE
    ldi64 r11, var_here
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ; Save current leave_count on data stack (for nesting) then reset to 0
    ldi64 r11, var_leave_count
    ldn r1, r11
    subi r14, 8
    str r14, r1               ; push saved leave_count
    ldi r1, 0
    str r11, r1               ; leave_count = 0
    ret.l

; LOOP (IMMEDIATE) — end counted loop
;   Compiles: increment index on RSP, compare to limit, branch back if not done
;   Then: clean up RSP (drop limit & index)
w_loop:
    ; Compile at runtime:
    ;   ldn r1, r15       ; index from RSP
    ;   addi r1, 1        ; increment (INC would also work but let's use ADDI for clarity)
    ;   str r15, r1       ; store back
    ;   mov r7, r15       ; copy RSP
    ;   addi r7, 8        ; point to limit
    ;   ldn r7, r7        ; load limit
    ;   cmp r1, r7        ; index vs limit
    ;   lbrne <loop-top>  ; branch if not equal
    ;   addi r15, 16      ; drop both from RSP
    ; Encoded:
    ;   ldn r1, r15  = 50 1F
    ;   inc r1       = 11
    ;   str r15, r1  = 54 F1
    ;   mov r7, r15  = 78 7F    (ALU MOV = sub 8)
    ;   addi r7, 8   = 62 70 08
    ;   ldn r7, r7   = 50 77
    ;   cmp r1, r7   = 77 17    (ALU CMP = sub 7)
    ;   lbrne <off>  = 42 XX XX
    ;   addi r15, 16 = 62 F0 10

    ; ldn r1, r15
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ; inc r1 = 0x11
    ldi r1, 0x11
    ldi64 r11, compile_byte
    call.l r11
    ; str r15, r1 = 54 F1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF1
    ldi64 r11, compile_byte
    call.l r11
    ; mov r7, r15 = 78 7F
    ldi r1, 0x78
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x7F
    ldi64 r11, compile_byte
    call.l r11
    ; addi r7, 8 = 62 70 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x70
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r7, r7 = 50 77
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x77
    ldi64 r11, compile_byte
    call.l r11
    ; cmp r1, r7 = 77 17
    ldi r1, 0x77
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x17
    ldi64 r11, compile_byte
    call.l r11
    ; lbrne <loop-top>
    ; Pop saved_leave_count first (pushed by DO after loop-target)
    ldn r12, r14              ; saved leave_count
    addi r14, 8
    ; Pop loop-target
    ldn r9, r14               ; loop target addr
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x42              ; LBRNE
    st.b r0, r7
    inc r0
    ; offset = target - (opcode_addr+3); R0 is at opcode+1
    mov r1, r9
    sub r1, r0
    subi r1, 2                ; offset = target - (R0 + 2) = target - (opcode+3)
    ; Write big-endian: high byte first, then low byte
    mov r7, r1
    lsri r7, 8
    st.b r0, r7               ; high byte
    inc r0
    st.b r0, r1               ; low byte
    inc r0
    ; addi r15, 16 = 62 F0 10
    ldi r7, 0x62
    st.b r0, r7
    inc r0
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x10
    st.b r0, r7
    inc r0
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ; --- Resolve LEAVE fixups ---
    ; R0 = HERE (target for LEAVE branches = just past LOOP cleanup)
    ; R12 = saved leave_count (from DO)
    ; Walk var_leave_fixups[0 .. var_leave_count-1], patch each to branch to HERE
    ldi64 r11, var_leave_count
    ldn r1, r11               ; current leave_count
    cmpi r1, 0
    breq w_loop_leave_done
    ldi64 r9, var_leave_fixups
    ldi r7, 0                 ; index
w_loop_leave_resolve:
    cmp r7, r1
    breq w_loop_leave_done
    ; fixup_addr = var_leave_fixups[r7]
    mov r11, r7
    lsli r11, 3               ; index * 8
    add r11, r9               ; &var_leave_fixups[index]
    ldn r11, r11              ; fixup_addr
    ; offset = HERE - (fixup_addr + 2)
    mov r13, r0
    sub r13, r11
    subi r13, 2
    ; Patch big-endian: high byte then low byte
    mov r1, r13               ; preserve full offset for low byte
    lsri r1, 8
    st.b r11, r1              ; high byte at fixup_addr
    inc r11
    st.b r11, r13             ; low byte at fixup_addr+1
    inc r7
    ldi64 r11, var_leave_count
    ldn r1, r11               ; reload count (R1 was clobbered)
    br w_loop_leave_resolve
w_loop_leave_done:
    ; Restore leave_count from saved value (R12)
    ldi64 r11, var_leave_count
    str r11, r12
    ret.l

; I (IMMEDIATE at compile time but not actually — it's a runtime word)
; I ( -- index ) push loop index from RSP
;   When called via compiled trampoline, RSP has:
;   [ret_from_trampoline, ret_from_compiled_call, index, limit, ...]
;   So index is at RSP+16
w_i:
    mov r11, r15
    addi r11, 16              ; skip two return addresses
    ldn r1, r11               ; index
    subi r14, 8
    str r14, r1
    ret.l

; =====================================================================
;  BIOS v0.5 / v1.0 — New Words
; =====================================================================

; EXIT (IMMEDIATE) — compile ret.l inline (same as what ; does)
w_exit:
    ldi64 r11, compile_ret
    call.l r11
    ret.l

; >R (IMMEDIATE) — compile inline: pop data stack, push return stack
;   Emits:  ldn r1, r14  (50 1E)
;           addi r14, 8  (62 E0 08)
;           subi r15, 8  (67 F0 08)
;           str r15, r1  (54 F1)
;   Total = 10 bytes
w_to_r:
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; R> (IMMEDIATE) — compile inline: pop return stack, push data stack
;   Emits:  ldn r1, r15  (50 1F)
;           addi r15, 8  (62 F0 08)
;           subi r14, 8  (67 E0 08)
;           str r14, r1  (54 E1)
;   Total = 10 bytes
w_r_from:
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; R@ (IMMEDIATE) — compile inline: peek return stack, push data stack
;   Emits:  ldn r1, r15  (50 1F)
;           subi r14, 8  (67 E0 08)
;           str r14, r1  (54 E1)
;   Total = 7 bytes
w_r_fetch:
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; J ( -- outer-index ) outer loop index via trampoline
;   RSP layout when called from nested DO..LOOP via trampoline:
;   RSP+0   return to trampoline ret.l
;   RSP+8   return to compiled code
;   RSP+16  inner loop index
;   RSP+24  inner loop limit
;   RSP+32  outer loop index ← this
w_j:
    mov r11, r15
    addi r11, 16              ; skip two return addresses
    addi r11, 16              ; skip inner loop index + limit
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; UNLOOP (IMMEDIATE) — compile: drop loop index+limit from RSP
;   Emits: addi r15, 16  (62 F0 10)
;   Total = 3 bytes
w_unloop:
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; +LOOP (IMMEDIATE) — end counted loop with custom increment
;   Like LOOP but adds TOS to index instead of 1
;   Compiles:
;     ldn r7, r14       ; increment from data stack     50 7E
;     addi r14, 8       ;                               62 E0 08
;     ldn r1, r15       ; index from RSP                50 1F
;     add r1, r7        ; add increment                 70 17
;     str r15, r1       ; store back                    54 F1
;     mov r7, r15       ; copy RSP                      78 7F
;     addi r7, 8        ; point to limit                62 70 08
;     ldn r7, r7        ; load limit                    50 77
;     cmp r1, r7        ; index vs limit                77 17
;     lbrne <loop-top>  ; branch if not equal           42 XX XX
;     addi r15, 16      ; drop both from RSP            62 F0 10
w_plus_loop:
    ; ldn r7, r14 = 50 7E
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x7E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8 = 62 E0 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r1, r15 = 50 1F
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ; add r1, r7 = 70 17
    ldi r1, 0x70
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x17
    ldi64 r11, compile_byte
    call.l r11
    ; str r15, r1 = 54 F1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF1
    ldi64 r11, compile_byte
    call.l r11
    ; mov r7, r15 = 78 7F
    ldi r1, 0x78
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x7F
    ldi64 r11, compile_byte
    call.l r11
    ; addi r7, 8 = 62 70 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x70
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r7, r7 = 50 77
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x77
    ldi64 r11, compile_byte
    call.l r11
    ; cmp r1, r7 = 77 17
    ldi r1, 0x77
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x17
    ldi64 r11, compile_byte
    call.l r11
    ; lbrne <loop-top>: 42 XX XX
    ; Pop saved_leave_count first (pushed by DO after loop-target)
    ldn r12, r14              ; saved leave_count
    addi r14, 8
    ; Pop loop-target
    ldn r9, r14               ; loop target addr from data stack
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x42              ; LBRNE
    st.b r0, r7
    inc r0
    mov r1, r9
    sub r1, r0
    subi r1, 2                ; offset = target - (opcode_addr+3)
    mov r7, r1
    lsri r7, 8
    st.b r0, r7               ; high byte
    inc r0
    st.b r0, r1               ; low byte
    inc r0
    ; addi r15, 16 = 62 F0 10
    ldi r7, 0x62
    st.b r0, r7
    inc r0
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x10
    st.b r0, r7
    inc r0
    ldi64 r11, var_here
    str r11, r0
    ; --- Resolve LEAVE fixups (same as w_loop) ---
    ldi64 r11, var_leave_count
    ldn r1, r11
    cmpi r1, 0
    breq w_ploop_leave_done
    ldi64 r9, var_leave_fixups
    ldi r7, 0
w_ploop_leave_resolve:
    cmp r7, r1
    breq w_ploop_leave_done
    mov r11, r7
    lsli r11, 3
    add r11, r9
    ldn r11, r11              ; fixup_addr
    mov r13, r0
    sub r13, r11
    subi r13, 2
    mov r1, r13
    lsri r1, 8
    st.b r11, r1
    inc r11
    st.b r11, r13
    inc r7
    ldi64 r11, var_leave_count
    ldn r1, r11
    br w_ploop_leave_resolve
w_ploop_leave_done:
    ldi64 r11, var_leave_count
    str r11, r12
    ret.l

; AGAIN (IMMEDIATE) — unconditional backward branch
;   Pops loop target from stack, compiles lbr <offset>
w_again:
    ldn r9, r14               ; target from stack
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11
    ldi r7, 0x40              ; LBR (unconditional)
    st.b r0, r7
    inc r0
    mov r1, r9
    sub r1, r0
    subi r1, 2                ; offset = target - (lbr_addr + 3)
    mov r7, r1
    lsri r7, 8
    st.b r0, r7               ; high byte
    inc r0
    st.b r0, r1               ; low byte
    inc r0
    ldi64 r11, var_here
    str r11, r0
    ret.l

; STATE ( -- addr ) push address of var_state
w_state:
    ldi64 r1, var_state
    subi r14, 8
    str r14, r1
    ret.l

; [ (IMMEDIATE) — switch to interpret mode
w_left_bracket:
    ldi r1, 0
    ldi64 r11, var_state
    str r11, r1
    ret.l

; ] — switch to compile mode
w_right_bracket:
    ldi r1, 1
    ldi64 r11, var_state
    str r11, r1
    ret.l

; LITERAL (IMMEDIATE) — compile literal from TOS
;   Takes value from data stack, emits compile_literal code
w_literal:
    ldn r1, r14               ; pop value
    addi r14, 8
    ldi64 r11, compile_literal
    call.l r11
    ret.l

; IMMEDIATE — set IMMEDIATE flag on most recent word
w_immediate:
    ldi64 r11, var_latest
    ldn r9, r11               ; R9 = latest entry address
    mov r11, r9
    addi r11, 8               ; point to flags+len byte
    ld.b r0, r11              ; read it
    ori r0, 0x80              ; set IMMEDIATE bit
    st.b r11, r0              ; write back
    ret.l

; CREATE ( "name" -- ) — create dictionary entry, runtime pushes data-field addr
;   Like VARIABLE but doesn't allot a cell; leaves HERE at the data field.
w_create:
    ; Parse name
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_create_err

    ; Save name info
    mov r13, r9               ; R13 = name addr
    mov r7, r12               ; R7 = name len

    ; Build dictionary entry header at HERE
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE
    mov r1, r0                ; save entry start in R1

    ; Link field = current LATEST
    ldi64 r11, var_latest
    ldn r11, r11
    str r0, r11               ; write link
    addi r0, 8

    ; Flags+len byte (just length, no IMMEDIATE)
    st.b r0, r7
    inc r0

    ; Copy name
    mov r11, r13              ; R11 = source name
    mov r12, r7               ; R12 = count
w_create_copy:
    ld.b r9, r11
    st.b r0, r9
    inc r11
    inc r0
    dec r12
    cmpi r12, 0
    brne w_create_copy

    ; Emit code: ldi64 r1, <data_addr>; subi r14, 8; str r14, r1; ret.l + DOES> slot
    ; 30-byte trampoline: ldi64(11) + subi(3) + str(2) + ret.l(1) + 13 bytes DOES> slot
    ; data_addr = R0 + 30
    mov r9, r0
    addi r9, 30               ; data_addr (where data field will start)

    ; ldi64 r1, <data_addr>: F0 60 10 + 8 bytes LE
    ldi r12, 0xF0
    st.b r0, r12
    inc r0
    ldi r12, 0x60
    st.b r0, r12
    inc r0
    ldi r12, 0x10
    st.b r0, r12
    inc r0
    ; 8 bytes of data_addr (R9)
    st.b r0, r9
    inc r0
    mov r12, r9
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    mov r12, r9
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    lsri r12, 8
    st.b r0, r12
    inc r0
    ; subi r14, 8: 67 E0 08
    ldi r12, 0x67
    st.b r0, r12
    inc r0
    ldi r12, 0xE0
    st.b r0, r12
    inc r0
    ldi r12, 0x08
    st.b r0, r12
    inc r0
    ; str r14, r1: 54 E1
    ldi r12, 0x54
    st.b r0, r12
    inc r0
    ldi r12, 0xE1
    st.b r0, r12
    inc r0
    ; ret.l: 0E
    ldi r12, 0x0E
    st.b r0, r12
    inc r0

    ; DOES> slot: 13 bytes of zero padding (will be patched by DOES>)
    ldi r12, 0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0
    st.b r0, r12
    inc r0

    ; Update HERE (points to data field)
    ldi64 r11, var_here
    str r11, r0

    ; Update LATEST
    ldi64 r11, var_latest
    str r11, r1
    ret.l
w_create_err:
    ldi64 r10, str_no_name
    ldi64 r11, print_str
    call.l r11
    ret.l

; S" (IMMEDIATE) — compile string literal, at runtime pushes (addr len)
;   Compiles: ldi64 r11, squote_runtime; call.l r11; <string bytes> <null>
;   Runtime: push addr+len of string to data stack
w_squote:
    ; Compile call to runtime helper
    ldi64 r1, squote_runtime
    ldi64 r11, compile_call
    call.l r11
    ; Now scan input for closing " and compile each byte
    ldi64 r11, var_to_in
    ldn r13, r11              ; >IN
    ldi64 r9, tib_buffer
    add r9, r13               ; current input position
    ; Skip the leading space delimiter (ANS Forth: S" skips one space)
    ld.b r1, r9
    cmpi r1, 0x20
    brne sq_scan
    inc r9
sq_scan:
    ld.b r1, r9
    cmpi r1, 0                ; end of input?
    breq sq_done
    cmpi r1, 0x22             ; '"' ?
    breq sq_done_skip
    ; compile this byte
    ldi64 r11, compile_byte
    call.l r11
    inc r9
    br sq_scan
sq_done_skip:
    inc r9                    ; skip past closing "
sq_done:
    ; compile null terminator
    ldi r1, 0
    ldi64 r11, compile_byte
    call.l r11
    ; update >IN
    ldi64 r11, tib_buffer
    sub r9, r11
    ldi64 r11, var_to_in
    str r11, r9
    ret.l

; squote_runtime: called at runtime.  Return address on RSP = string address.
;   Scan to null, compute length, push (addr len), patch return past string.
squote_runtime:
    ldn r10, r15              ; R10 = string start
    mov r9, r10               ; R9 = scan pointer
sqr_scan:
    ld.b r1, r9
    cmpi r1, 0
    breq sqr_found
    inc r9
    br sqr_scan
sqr_found:
    ; R10 = string start, R9 = null position
    mov r1, r9
    sub r1, r10               ; R1 = length
    ; Push addr
    subi r14, 8
    str r14, r10
    ; Push len
    subi r14, 8
    str r14, r1
    ; Patch return address past null
    inc r9                    ; skip null
    str r15, r9
    ret.l

; 0> ( n -- flag ) true if n > 0
w_zero_gt:
    ldn r1, r14               ; TOS
    ; zero? → false
    cmpi r1, 0
    breq w_zero_gt_false
    ; negative? check sign bit
    mov r0, r1
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    andi r0, 0x80
    cmpi r0, 0
    brne w_zero_gt_false      ; sign bit set → negative → false
    ; positive nonzero → true
    ldi r1, 0
    dec r1                    ; -1 (true)
    str r14, r1
    ret.l
w_zero_gt_false:
    ldi r1, 0
    str r14, r1
    ret.l

; <> ( a b -- flag ) not equal
w_not_equal:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r7, r14               ; a
    cmp r7, r1
    breq w_ne_false
    ldi r1, 0
    dec r1                    ; -1 (true)
    str r14, r1
    ret.l
w_ne_false:
    ldi r1, 0
    str r14, r1
    ret.l

; 0<> ( n -- flag ) true if n ≠ 0
w_zero_ne:
    ldn r1, r14
    cmpi r1, 0
    brne w_zne_true
    ldi r1, 0
    str r14, r1
    ret.l
w_zne_true:
    ldi r1, 0
    dec r1
    str r14, r1
    ret.l

; ?DUP ( x -- x x | 0 ) duplicate if nonzero
w_qdup:
    ldn r1, r14
    cmpi r1, 0
    breq w_qdup_done
    subi r14, 8
    str r14, r1
w_qdup_done:
    ret.l

; MIN ( a b -- min )
w_min:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r7, r14               ; a
    cmp r7, r1
    brle w_min_done            ; a <= b, keep a
    mov r7, r1                ; a > b, use b
w_min_done:
    str r14, r7
    ret.l

; MAX ( a b -- max )
w_max:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r7, r14               ; a
    cmp r7, r1
    brgt w_max_a              ; a > b, keep a
    brle w_max_b              ; a <= b, use b
w_max_a:
    str r14, r7
    ret.l
w_max_b:
    str r14, r1
    ret.l

; CELLS ( n -- n*8 )
w_cells:
    ldn r1, r14
    lsli r1, 3                ; * 8
    str r14, r1
    ret.l

; CELL+ ( a -- a+8 )
w_cell_plus:
    ldn r1, r14
    addi r1, 8
    str r14, r1
    ret.l

; +! ( n addr -- ) add n to contents of addr
w_plus_store:
    ldn r1, r14               ; addr
    addi r14, 8
    ldn r7, r14               ; n
    addi r14, 8
    ldn r9, r1                ; fetch current value
    add r9, r7                ; add
    str r1, r9                ; store back
    ret.l

; 2* ( n -- n*2 )
w_two_star:
    ldn r1, r14
    lsli r1, 1
    str r14, r1
    ret.l

; CMOVE ( src dst u -- ) copy u bytes from src to dst
w_cmove:
    ldn r12, r14              ; u (count)
    addi r14, 8
    ldn r7, r14               ; dst
    addi r14, 8
    ldn r9, r14               ; src
    addi r14, 8
    cmpi r12, 0
    breq w_cmove_done
w_cmove_loop:
    ld.b r1, r9
    st.b r7, r1
    inc r9
    inc r7
    dec r12
    cmpi r12, 0
    brne w_cmove_loop
w_cmove_done:
    ret.l

; -ROT ( a b c -- c a b )
w_neg_rot:
    ldn r1, r14               ; c
    mov r11, r14
    addi r11, 8
    ldn r7, r11               ; b
    mov r9, r14
    addi r9, 16
    ldn r13, r9               ; a
    str r14, r7               ; TOS = b
    str r11, r13              ; NOS = a
    str r9, r1                ; 3OS = c
    ret.l

; BL ( -- 32 ) blank constant
w_bl:
    ldi r1, 32
    subi r14, 8
    str r14, r1
    ret.l

; TRUE ( -- -1 )
w_true:
    ldi r1, 0
    dec r1
    subi r14, 8
    str r14, r1
    ret.l

; FALSE ( -- 0 )
w_false:
    ldi r1, 0
    subi r14, 8
    str r14, r1
    ret.l

; WORD ( char "ccc<char>" -- c-addr )
;   Parse input delimited by char, store counted string at HERE.
;   Does NOT advance HERE — the string is transient.
;   Reserved GPRs: R2(XSEL), R3(PC/PSEL), R4(emit), R5(key),
;                  R6(hexprint), R8(UART), R14(DSP), R15(RSP).
w_word_forth:
    ; Pop delimiter into R13
    ldn r13, r14
    addi r14, 8
    ; Get >IN → R0
    ldi64 r11, var_to_in
    ldn r0, r11
    ; Get TIB base → R9
    ldi64 r9, tib_buffer
    ; Get #TIB → R7
    ldi64 r11, var_tib_len
    ldn r7, r11
    ; R10 = HERE (destination for counted string)
    ldi64 r11, var_here
    ldn r10, r11
    ; Save HERE start in R1 (will be c-addr to return)
    mov r1, r10
    inc r10                   ; skip past count byte
    ldi r12, 0                ; R12 = char count
    ; Skip leading delimiters
wd_skip:
    cmp r0, r7
    breq wd_done
    brgt wd_done
    mov r11, r9
    add r11, r0
    ld.b r11, r11
    cmp r11, r13
    brne wd_copy
    inc r0
    br wd_skip
    ; Copy non-delimiter chars
wd_copy:
    cmp r0, r7
    breq wd_done
    brgt wd_done
    mov r11, r9
    add r11, r0
    ld.b r11, r11
    cmp r11, r13
    breq wd_trail
    st.b r10, r11
    inc r10
    inc r12
    inc r0
    br wd_copy
wd_trail:
    inc r0                    ; skip past trailing delimiter
wd_done:
    ; Store count byte at c-addr (saved in R1)
    st.b r1, r12
    ; Null-terminate after the string
    ldi r11, 0
    st.b r10, r11
    ; Update >IN
    ldi64 r11, var_to_in
    str r11, r0
    ; Push c-addr (R1 = HERE at entry)
    subi r14, 8
    str r14, r1
    ret.l

; =====================================================================
;  VARIABLE, CONSTANT
; =====================================================================

; VARIABLE ( "name" -- )
;   Creates a word that pushes address of an 8-byte cell.
;   : VARIABLE CREATE 8 ALLOT ;
;   But implemented natively for simplicity.
w_variable:
    ; Parse name
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_colon_err

    ; Build dictionary entry header (same as : does)
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE = entry start
    mov r13, r0               ; save entry start

    ; Link
    ldi64 r11, var_latest
    ldn r1, r11
    str r0, r1
    addi r0, 8

    ; Flags+len
    st.b r0, r12
    inc r0

    ; Copy name
    ldi r1, 0
w_var_copy:
    cmp r1, r12
    breq w_var_name_done
    mov r7, r9
    add r7, r1
    ld.b r7, r7
    st.b r0, r7
    inc r0
    inc r1
    br w_var_copy
w_var_name_done:
    ; Code: push address of the data cell that follows the ret.l
    ; We'll emit: ldi64 r1, <data_addr>; subi r14, 8; str r14, r1; ret.l
    ; But we don't know data_addr yet. So we emit the code, then the data.
    ; data_addr = current pos + 16 + 1  (16 for ldi64+subi+str, 1 for ret.l)
    ; Actually: ldi64(11) + subi(3) + str(2) + ret.l(1) = 17 bytes
    ; data_addr = R0 + 17
    mov r1, r0
    addi r1, 17

    ; Save R0 (code start), emit ldi64 r1 for data addr
    ; ldi64 r1: 0xF0 0x60 0x10
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x60
    st.b r0, r7
    inc r0
    ldi r7, 0x10
    st.b r0, r7
    inc r0
    ; 8 bytes of data_addr (R1) LE
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ; subi r14, 8: 0x67 0xE0 0x08
    ldi r7, 0x67
    st.b r0, r7
    inc r0
    ldi r7, 0xE0
    st.b r0, r7
    inc r0
    ldi r7, 0x08
    st.b r0, r7
    inc r0
    ; str r14, r1: 0x54 0xE1
    ldi r7, 0x54
    st.b r0, r7
    inc r0
    ldi r7, 0xE1
    st.b r0, r7
    inc r0
    ; ret.l: 0x0E
    ldi r7, 0x0E
    st.b r0, r7
    inc r0
    ; Now R0 = data cell address. Initialize to 0.
    ldi r1, 0
    str r0, r1
    addi r0, 8
    ; Update HERE and LATEST
    ldi64 r11, var_here
    str r11, r0
    ldi64 r11, var_latest
    str r11, r13
    ret.l

; CONSTANT ( n "name" -- )
;   Creates a word that pushes n.
;   Note: value is saved in R10 because parse_word clobbers R13,
;   and R8 is the global UART TX address.
w_constant:
    ; Get value from stack
    ldn r10, r14              ; n → R10 (safe across parse_word)
    addi r14, 8
    ; Parse name
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_colon_err

    ; Build dictionary entry header
    ldi64 r11, var_here
    ldn r0, r11
    mov r1, r0                ; save entry start

    ; push entry start
    subi r14, 8
    str r14, r1

    ; Link
    ldi64 r11, var_latest
    ldn r1, r11
    str r0, r1
    addi r0, 8

    ; Flags+len
    st.b r0, r12
    inc r0

    ; Copy name
    ldi r1, 0
w_const_copy:
    cmp r1, r12
    breq w_const_name_done
    mov r7, r9
    add r7, r1
    ld.b r7, r7
    st.b r0, r7
    inc r0
    inc r1
    br w_const_copy
w_const_name_done:
    ; Update HERE
    ldi64 r11, var_here
    str r11, r0
    ; Code: compile literal push of R10 (the saved value) then ret.l
    mov r1, r10
    ldi64 r11, compile_literal
    call.l r11
    ldi64 r11, compile_ret
    call.l r11
    ; Set LATEST
    ldn r1, r14               ; entry start
    addi r14, 8
    ldi64 r11, var_latest
    str r11, r1
    ret.l

; =====================================================================
;  String Words
; =====================================================================

; TYPE ( addr len -- ) print counted string
w_type:
    ldn r12, r14              ; len
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
w_type_loop:
    cmpi r12, 0
    breq w_type_done
    ld.b r1, r9
    call.l r4
    inc r9
    dec r12
    br w_type_loop
w_type_done:
    ret.l

; SPACE ( -- ) emit a space
w_space:
    ldi r1, 0x20
    call.l r4
    ret.l

; SPACES ( n -- ) emit n spaces
w_spaces:
    ldn r12, r14
    addi r14, 8
w_spaces_loop:
    cmpi r12, 0
    breq w_spaces_done
    ldi r1, 0x20
    call.l r4
    dec r12
    br w_spaces_loop
w_spaces_done:
    ret.l

; \ (backslash comment, IMMEDIATE) — skip rest of input line
;   Works in both interpret and compile modes.
w_backslash:
    ldi64 r11, var_tib_len
    ldn r0, r11               ; R0 = TIB-LEN
    ldi64 r11, var_to_in
    str r11, r0               ; >IN = TIB-LEN (skip rest of line)
    ret.l

; ( (paren comment, IMMEDIATE) — skip until matching )
;   Works in both interpret and compile modes.
w_paren:
    ldi64 r9, tib_buffer
    ldi64 r11, var_to_in
    ldn r13, r11              ; R13 = >IN
    ldi64 r11, var_tib_len
    ldn r12, r11              ; R12 = TIB-LEN
w_paren_scan:
    cmp r13, r12
    breq w_paren_done         ; hit end of line without )
    mov r11, r9
    add r11, r13
    ld.b r1, r11
    inc r13
    cmpi r1, 0x29             ; ')'
    brne w_paren_scan
w_paren_done:
    ldi64 r11, var_to_in
    str r11, r13              ; update >IN past the )
    ret.l

; ." (IMMEDIATE) — print or compile inline string
;   Interpret mode: reads chars until " and prints them immediately.
;   Compile mode:   embeds the string and compiles a call to print it.
;   Compiles: ldi64 r11, dotquote_runtime; call.l r11; <bytes> <null>
w_dotquote:
    ; Check STATE — interpret vs compile
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    brne w_dotquote_compile

    ; ---- Interpret mode: print chars immediately ----
    ldi64 r11, var_to_in
    ldn r13, r11              ; >IN
    ldi64 r9, tib_buffer
dq_interp_loop:
    ldi64 r11, var_tib_len
    ldn r7, r11
    cmp r13, r7
    breq dq_interp_done       ; hit EOL without closing quote
    mov r11, r9
    add r11, r13
    ld.b r1, r11
    inc r13
    cmpi r1, 0x22             ; '"'
    breq dq_interp_done
    call.l r4                 ; emit_char
    br dq_interp_loop
dq_interp_done:
    ldi64 r11, var_to_in
    str r11, r13
    ret.l

w_dotquote_compile:
    ; ---- Compile mode (original behaviour) ----
    ; Emits: ldi64 r11, dotquote_runtime; call.l r11; <string bytes> <null>
    ldi64 r1, dotquote_runtime
    ldi64 r11, compile_call
    call.l r11
    ; Now read chars from input and compile them as bytes until "
    ldi64 r11, var_to_in
    ldn r13, r11              ; >IN
    ldi64 r9, tib_buffer
dq_scan:
    ldi64 r11, var_tib_len
    ldn r7, r11
    cmp r13, r7
    breq dq_end               ; hit EOL without closing quote
    mov r11, r9
    add r11, r13
    ld.b r1, r11
    inc r13
    cmpi r1, 0x22             ; '"'
    breq dq_end
    ; Compile this byte
    ldi64 r11, compile_byte
    call.l r11
    br dq_scan
dq_end:
    ; Compile null terminator
    ldi r1, 0
    ldi64 r11, compile_byte
    call.l r11
    ; Update >IN
    ldi64 r11, var_to_in
    str r11, r13
    ret.l

; dotquote_runtime: called by compiled ." code.
;   The return address on RSP points to the start of the string.
;   We print it, then adjust the return address past the null.
dotquote_runtime:
    ; The call.l pushed the return address onto RSP (R15).
    ; Return addr = address right after the call.l instruction = string start
    ldn r10, r15              ; R10 = string address
    ; Print the string
    ldi64 r11, print_str
    call.l r11
    ; R10 was advanced by print_str? No, print_str doesn't modify R10 in a useful way.
    ; We need to scan past the null to find the new return address.
    ldn r10, r15              ; reload string start
dqrt_scan:
    ld.b r1, r10
    inc r10
    cmpi r1, 0
    brne dqrt_scan
    ; R10 now points past the null — this is the real return address
    str r15, r10
    ret.l

; ACCEPT ( addr max -- n ) read up to max chars into addr, return count
w_accept:
    ldn r12, r14              ; max
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
    ldi r13, 0                ; count
w_accept_loop:
    cmp r13, r12
    breq w_accept_done
    call.l r5                 ; key -> R1
    cmpi r1, 0x0D
    breq w_accept_done
    cmpi r1, 0x0A
    breq w_accept_done
    mov r7, r9
    add r7, r13
    st.b r7, r1
    inc r13
    call.l r4                 ; echo
    br w_accept_loop
w_accept_done:
    ldi64 r11, print_crlf
    call.l r11
    subi r14, 8
    str r14, r13              ; push count
    ret.l

; =====================================================================
;  NIC Words
; =====================================================================

; NIC base = MMIO_BASE + 0x0400 = 0xFFFF_FF00_0000_0400

; NET-STATUS ( -- status )  read NIC STATUS register
w_net_status:
    ldi64 r11, 0xFFFF_FF00_0000_0401
    ld.b r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; NET-SEND ( addr len -- )  send a frame via NIC DMA
w_net_send:
    ldn r12, r14              ; len
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0400
    ; Write DMA addr (8 bytes at offset 0x02)
    mov r0, r11
    addi r0, 2
    mov r1, r9
    ; Write 8 bytes of addr LE
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ldi r7, 0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
    ; Write frame length (16-bit at offset 0x0A)
    mov r0, r11
    addi r0, 0x0A
    mov r1, r12
    st.b r0, r1
    inc r0
    mov r7, r12
    lsri r7, 8
    st.b r0, r7
    ; Send command (0x01 at offset 0x00)
    ldi r1, 0x01
    st.b r11, r1
    ret.l

; NET-RECV ( addr -- len )  receive a frame via NIC DMA
;   Returns 0 if no frame available.
w_net_recv:
    ldn r9, r14               ; addr
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0400
    ; Check STATUS for RX available (bit 1)
    mov r0, r11
    inc r0
    ld.b r1, r0
    mov r7, r1
    andi r7, 0x02
    cmpi r7, 0
    breq w_net_recv_none
    ; Write DMA addr
    mov r0, r11
    addi r0, 2
    mov r1, r9
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ldi r7, 0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
    inc r0
    st.b r0, r7
    ; RECV command
    ldi r1, 0x02
    st.b r11, r1
    ; Read frame length
    mov r0, r11
    addi r0, 0x0A
    ld.b r1, r0
    inc r0
    ld.b r7, r0
    lsli r7, 8
    or r1, r7
    subi r14, 8
    str r14, r1
    ret.l
w_net_recv_none:
    ldi r1, 0
    subi r14, 8
    str r14, r1
    ret.l

; NET-MAC@ ( -- addr )  push address of MAC (in MMIO space, 6 bytes at NIC+0x0E)
w_net_mac:
    ldi64 r1, 0xFFFF_FF00_0000_040E
    subi r14, 8
    str r14, r1
    ret.l

; =====================================================================
;  Storage device words
; =====================================================================
; Storage base = MMIO_BASE + 0x0200 = 0xFFFF_FF00_0000_0200
;
; Register map:
;   +0x00  CMD       (W)   0x01=READ, 0x02=WRITE
;   +0x01  STATUS    (R)   bit7=present, bit0=busy, bit1=error
;   +0x02  SECTOR_0  (RW)  sector number (4 bytes LE)
;   +0x06  DMA_ADDR  (RW)  DMA address (8 bytes LE)
;   +0x0E  SEC_COUNT (RW)  number of sectors
;   +0x0F  DATA      (RW)  byte-at-a-time port

; DISK@ ( -- status )  read storage STATUS register
w_disk_status:
    ldi64 r11, 0xFFFF_FF00_0000_0201
    ld.b r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; DISK-SEC! ( sector -- )  set sector number (32-bit)
w_disk_sec_store:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0202
    ; Write 4 bytes LE
    mov r7, r1
    st.b r11, r7
    inc r11
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    ret.l

; DISK-DMA! ( addr -- )  set DMA address (64-bit)
w_disk_dma_store:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0206
    ; Write 8 bytes LE
    mov r7, r1
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    ; Upper 4 bytes = 0 (we're in normal RAM, not MMIO)
    ldi r7, 0
    st.b r11, r7
    inc r11
    st.b r11, r7
    inc r11
    st.b r11, r7
    inc r11
    st.b r11, r7
    ret.l

; DISK-N! ( count -- )  set sector count
w_disk_n_store:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_020E
    st.b r11, r1
    ret.l

; DISK-READ ( -- )  execute READ command (DMA from disk to RAM)
w_disk_read:
    ldi64 r11, 0xFFFF_FF00_0000_0200
    ldi r1, 0x01
    st.b r11, r1
    ret.l

; DISK-WRITE ( -- )  execute WRITE command (DMA from RAM to disk)
w_disk_write:
    ldi64 r11, 0xFFFF_FF00_0000_0200
    ldi r1, 0x02
    st.b r11, r1
    ret.l

; =====================================================================
;  FSLOAD — load and evaluate a file from MP64FS disk
; =====================================================================

; disk_read_sectors: internal helper — DMA read from disk.
;   R1 = start sector (32-bit), R9 = DMA dest addr, R12 = sector count
;   Clobbers: R7, R11
disk_read_sectors:
    ; --- Set sector number (4 bytes LE) ---
    ldi64 r11, 0xFFFF_FF00_0000_0202
    mov r7, r1
    st.b r11, r7
    inc r11
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    ; --- Set DMA address (8 bytes LE, upper 4 = 0) ---
    ldi64 r11, 0xFFFF_FF00_0000_0206
    mov r7, r9
    st.b r11, r7
    inc r11
    mov r7, r9
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r9
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    mov r7, r9
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r11, r7
    inc r11
    ldi r7, 0
    st.b r11, r7
    inc r11
    st.b r11, r7
    inc r11
    st.b r11, r7
    inc r11
    st.b r11, r7
    ; --- Set sector count (1 byte) ---
    ldi64 r11, 0xFFFF_FF00_0000_020E
    st.b r11, r12
    ; --- Issue READ command ---
    ldi64 r11, 0xFFFF_FF00_0000_0200
    ldi r7, 0x01
    st.b r11, r7
    ret.l

; FSLOAD ( "name" -- )  Load a file from MP64FS and EVALUATE its contents.
;
; Parses the next word as a filename, reads the MP64FS directory from
; the attached storage device, locates the file, reads its data sectors
; into a RAM buffer at R2/2, then walks through the content line-by-line
; calling EVALUATE on each line.
;
; Buffer location: R2/2 (initial DSP position).  The data stack grows
; downward from R2/2 and the buffer extends upward, so they do not
; conflict during normal operation.
;
; RSP frame during evaluate loop:
;   [RSP+0]  cur_ptr     — pointer to next unprocessed byte
;   [RSP+8]  remaining   — bytes left to process
;   [RSP+16] caller return address (from trampoline call.l)

w_fsload:
    ; ---- Parse filename from input stream ----
    ldi64 r11, parse_word
    call.l r11
    ; R9 = word address (in TIB), R12 = length
    cmpi r12, 0
    lbreq w_fsload_no_name

    ; Save parsed name on RSP  [RSP+0]=name_len, [RSP+8]=name_addr
    subi r15, 8
    str r15, r9               ; name_addr
    subi r15, 8
    str r15, r12              ; name_len

    ; ---- Check disk present ----
    ldi64 r11, 0xFFFF_FF00_0000_0201
    ld.b r1, r11
    andi r1, 0x80
    lbreq w_fsload_no_disk

    ; ---- Read directory sectors 2-5 (2048 bytes) into buffer ----
    mov r9, r2
    lsri r9, 1                ; buffer = R2 / 2
    subi r15, 8
    str r15, r9               ; [RSP+0]=buf_addr
    ldi r1, 2                 ; start sector = 2
    ldi r12, 4                ; 4 sectors
    ldi64 r11, disk_read_sectors
    call.l r11

    ; ---- Scan 64 directory entries for name match ----
    ldn r9, r15               ; buf_addr
    ldi r0, 0                 ; entry index

w_fsload_scan:
    cmpi r0, 64
    lbreq w_fsload_not_found

    ; entry_addr = buf + index * 32
    mov r13, r0
    lsli r13, 5               ; * 32
    add r13, r9               ; R13 = entry address

    ; Skip empty entries (first byte = 0)
    ld.b r7, r13
    cmpi r7, 0
    breq w_fsload_next

    ; Compare parsed name with directory entry name (up to 16 bytes)
    ; R13 = entry name, name_addr at [RSP+16], name_len at [RSP+8]
    mov r11, r15
    addi r11, 8
    ldn r12, r11              ; name_len
    addi r11, 8
    ldn r7, r11               ; name_addr
    ldi r1, 0                 ; byte index
w_fsload_cmp:
    cmp r1, r12
    breq w_fsload_cmp_tail
    ; Compare entry_name[i] with parsed_name[i]
    mov r11, r13
    add r11, r1
    ld.b r11, r11             ; entry char
    mov r10, r7
    add r10, r1
    ld.b r10, r10             ; name char
    cmp r11, r10
    brne w_fsload_next
    inc r1
    br w_fsload_cmp

w_fsload_cmp_tail:
    ; Matched all name_len bytes — verify remaining entry name bytes are 0
    cmpi r1, 15
    brgt w_fsload_found
    mov r11, r13
    add r11, r1
    ld.b r11, r11
    cmpi r11, 0
    brne w_fsload_next
    inc r1
    br w_fsload_cmp_tail

w_fsload_next:
    ldn r9, r15               ; reload buf_addr
    inc r0
    lbr w_fsload_scan

w_fsload_found:
    ; R13 = matching directory entry address
    ; Extract start_sector (u16 LE at +16)
    mov r11, r13
    addi r11, 16
    ld.b r1, r11              ; lo
    inc r11
    ld.b r7, r11              ; hi
    lsli r7, 8
    or r1, r7                 ; R1 = start_sector

    ; Extract sector_count (u16 LE at +18)
    inc r11
    ld.b r12, r11             ; lo
    inc r11
    ld.b r7, r11              ; hi
    lsli r7, 8
    or r12, r7                ; R12 = sector_count

    ; Extract used_bytes (u32 LE at +20)
    inc r11
    ld.b r13, r11             ; byte 0
    inc r11
    ld.b r7, r11              ; byte 1
    lsli r7, 8
    or r13, r7
    inc r11
    ld.b r7, r11              ; byte 2
    lsli r7, 8
    lsli r7, 8
    or r13, r7
    inc r11
    ld.b r7, r11              ; byte 3
    lsli r7, 8
    lsli r7, 8
    lsli r7, 8
    or r13, r7                ; R13 = used_bytes

    ; Clean up name/buf RSP frame (3 slots)
    addi r15, 24

    ; ---- Read file data into buffer at R2/2 ----
    ; R1 = start_sector, R12 = sector_count, R13 = used_bytes
    mov r9, r2
    lsri r9, 1                ; buffer = R2 / 2
    ; Save loop state on RSP
    subi r15, 8
    str r15, r13              ; [RSP+0] = remaining (used_bytes)
    subi r15, 8
    str r15, r9               ; [RSP+0] = cur_ptr (buffer start)
    ; Do the disk read
    ldi64 r11, disk_read_sectors
    call.l r11

    ; Initialize FSLOAD line counter
    ldi r1, 0
    ldi64 r11, var_fsload_line
    str r11, r1

    ; ---- Evaluate line by line ----
w_fsload_line_loop:
    ; Increment line counter
    ldi64 r11, var_fsload_line
    ldn r1, r11
    inc r1
    str r11, r1

    ; Load state from RSP
    ldn r9, r15               ; cur_ptr
    mov r11, r15
    addi r11, 8
    ldn r13, r11              ; remaining

    ; Done?
    cmpi r13, 0
    lbreq w_fsload_done

    ; Scan for LF (0x0A)
    ldi r12, 0                ; line length
w_fsload_find_lf:
    cmp r12, r13
    breq w_fsload_last_chunk
    mov r11, r9
    add r11, r12
    ld.b r7, r11
    cmpi r7, 0x0A
    breq w_fsload_got_line
    inc r12
    br w_fsload_find_lf

w_fsload_last_chunk:
    ; End of data without trailing LF — treat remainder as last line
    br w_fsload_eval_line

w_fsload_got_line:
    ; R12 = line length (not including LF)
    ; Strip trailing CR if present
    cmpi r12, 0
    breq w_fsload_empty_line
    mov r11, r9
    add r11, r12
    dec r11
    ld.b r7, r11
    cmpi r7, 0x0D
    brne w_fsload_eval_line
    dec r12
    br w_fsload_eval_line

w_fsload_empty_line:
    ; Advance past LF, continue
    inc r9
    dec r13
    str r15, r9
    mov r11, r15
    addi r11, 8
    str r11, r13
    lbr w_fsload_line_loop

w_fsload_eval_line:
    ; R9 = line start, R12 = line length, R13 = total remaining
    ; Skip blank lines (length 0)
    cmpi r12, 0
    breq w_fsload_advance

    ; Compute updated state BEFORE calling evaluate
    ; new_cur_ptr = r9 + r12
    mov r0, r9
    add r0, r12               ; points to LF (or end)
    ; new_remaining = r13 - r12
    mov r7, r13
    sub r7, r12

    ; Skip past LF if present
    cmpi r7, 0
    breq w_fsload_update_state
    ; There are more bytes — skip the LF
    inc r0
    dec r7

w_fsload_update_state:
    ; Write updated state to RSP
    str r15, r0               ; new cur_ptr
    mov r11, r15
    addi r11, 8
    str r11, r7               ; new remaining

    ; Push ( addr len ) for EVALUATE
    subi r14, 8
    str r14, r9               ; line address
    subi r14, 8
    str r14, r12              ; line length

    ; Call EVALUATE
    ldi64 r11, w_evaluate
    call.l r11

    lbr w_fsload_line_loop

w_fsload_advance:
    ; Blank line after CR/LF stripping — advance past LF
    mov r0, r9
    add r0, r12
    mov r7, r13
    sub r7, r12
    cmpi r7, 0
    breq w_fsload_advance_done
    inc r0
    dec r7
w_fsload_advance_done:
    str r15, r0
    mov r11, r15
    addi r11, 8
    str r11, r7
    lbr w_fsload_line_loop

w_fsload_done:
    ; Clear FSLOAD line counter
    ldi r1, 0
    ldi64 r11, var_fsload_line
    str r11, r1
    ; Clean up RSP (cur_ptr + remaining)
    addi r15, 16
    ret.l

w_fsload_no_name:
    ldi64 r10, str_no_name
    ldi64 r11, print_str
    call.l r11
    ret.l

w_fsload_no_disk:
    ; Clean up name frame
    addi r15, 16
    ldi64 r10, str_fsload_no_disk
    ldi64 r11, print_str
    call.l r11
    ret.l

w_fsload_not_found:
    ; Clean up name + buf frame
    addi r15, 24
    ; Print the filename
    ldi64 r10, str_fsload_err
    ldi64 r11, print_str
    call.l r11
    ret.l

; =====================================================================
;  Timer Control Words
; =====================================================================

; TIMER! ( compare -- )  set the 32-bit compare-match register
w_timer_store:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0104   ; COMPARE_LO
    st.w r11, r1                        ; write 32 bits LE
    ret.l

; TIMER-CTRL! ( ctrl -- )  write timer CONTROL register
;   bit 0: enable, bit 1: compare-match IRQ enable, bit 2: auto-reload
w_timer_ctrl_store:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0108   ; CONTROL
    st.b r11, r1
    ret.l

; TIMER-ACK ( -- )  acknowledge timer interrupt (write-1-to-clear STATUS)
w_timer_ack:
    ldi64 r11, 0xFFFF_FF00_0000_0109   ; STATUS
    ldi r1, 0x01
    st.b r11, r1
    ret.l

; EI! ( -- )  enable interrupts globally
w_ei:
    ei
    ret.l

; DI! ( -- )  disable interrupts globally
w_di:
    di
    ret.l

; ISR! ( xt slot -- )  install execution token at IVT slot
;   Writes xt to ivt_table + slot*8
w_isr_store:
    ldn r1, r14                         ; slot
    addi r14, 8
    ldn r2, r14                         ; xt
    addi r14, 8
    ldi r3, 3
    shl r1, r3                          ; slot * 8
    ldi64 r11, ivt_table
    add r11, r1
    str r11, r2                         ; write xt to IVT entry
    ret.l

; =====================================================================
;  BIOS v1.0 — New Word Implementations
; =====================================================================

; 2OVER ( a b c d -- a b c d a b )
w_2over:
    mov r11, r14
    addi r11, 24              ; point to a (4th item)
    ldn r0, r11               ; a
    mov r11, r14
    addi r11, 16              ; point to b (3rd item)
    ldn r7, r11               ; b
    subi r14, 8
    str r14, r0               ; push a
    subi r14, 8
    str r14, r7               ; push b
    ret.l

; 2SWAP ( a b c d -- c d a b )
w_2swap:
    ldn r1, r14               ; d (TOS)
    mov r11, r14
    addi r11, 8
    ldn r7, r11               ; c
    mov r9, r14
    addi r9, 16
    ldn r0, r9                ; b
    mov r13, r14
    addi r13, 24
    ldn r12, r13              ; a
    str r14, r0               ; TOS = b
    str r11, r12              ; NOS = a
    str r9, r1                ; 3OS = d
    str r13, r7               ; 4OS = c
    ret.l

; 2ROT ( a b c d e f -- c d e f a b )
w_2rot:
    ; Stack layout: r14->f, +8->e, +16->d, +24->c, +32->b, +40->a
    ldn r1, r14               ; f
    mov r11, r14
    addi r11, 8
    ldn r7, r11               ; e
    mov r11, r14
    addi r11, 16
    ldn r0, r11               ; d
    mov r11, r14
    addi r11, 24
    ldn r9, r11               ; c
    mov r11, r14
    addi r11, 32
    ldn r12, r11              ; b
    mov r11, r14
    addi r11, 40
    ldn r13, r11              ; a
    ; Write back: TOS=b, +8=a, +16=f, +24=e, +32=d, +40=c
    str r14, r12              ; TOS = b
    mov r11, r14
    addi r11, 8
    str r11, r13              ; NOS = a
    mov r11, r14
    addi r11, 16
    str r11, r1               ; 3OS = f
    mov r11, r14
    addi r11, 24
    str r11, r7               ; 4OS = e
    mov r11, r14
    addi r11, 32
    str r11, r0               ; 5OS = d
    mov r11, r14
    addi r11, 40
    str r11, r9               ; 6OS = c
    ret.l

; >= ( a b -- flag ) signed greater-or-equal
w_gte:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ; signed >=: true iff NOT(N xor V), i.e., N==V
    ldi r0, 0
    csrr r0, 0x00             ; FLAGS
    mov r7, r0
    lsri r7, 2
    andi r7, 0x01             ; N flag (bit 2)
    mov r11, r0
    lsri r11, 3
    andi r11, 0x01            ; V flag (bit 3)
    xor r7, r11               ; N^V: 1 = a < b, 0 = a >= b
    ldi r0, 0
    cmpi r7, 0
    brne w_gte_done            ; N^V=1 -> a < b -> false (r0=0)
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_gte_done:
    str r14, r0
    ret.l

; <= ( a b -- flag ) signed less-or-equal
w_lte:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ; signed <=: true iff Z=1 OR (N xor V = 1)
    ldi r0, 0
    csrr r0, 0x00             ; FLAGS
    mov r7, r0
    andi r7, 0x01             ; Z flag (bit 0)
    cmpi r7, 0
    brne w_lte_true            ; Z=1 -> equal -> true
    mov r7, r0
    lsri r7, 2
    andi r7, 0x01             ; N flag
    mov r11, r0
    lsri r11, 3
    andi r11, 0x01            ; V flag
    xor r7, r11               ; N^V: 1 = a < b
    cmpi r7, 0
    brne w_lte_true            ; a < b -> true
    ldi r0, 0
    str r14, r0
    ret.l
w_lte_true:
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
    str r14, r0
    ret.l

; U< ( a b -- flag ) unsigned less-than
w_u_lt:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ; After CMP a, b: C=1 iff u64(a) >= u64(b). C=0 iff a < b (unsigned)
    ldi r0, 0
    brcs w_u_lt_done           ; C=1 -> a >= b -> false
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_u_lt_done:
    str r14, r0
    ret.l

; U> ( a b -- flag ) unsigned greater-than
w_u_gt:
    ldn r1, r14               ; b
    addi r14, 8
    ldn r0, r14               ; a
    cmp r0, r1
    ; After CMP a, b: G=1 iff u64(a) > u64(b)
    ldi r0, 0
    brle w_u_gt_done           ; G=0 -> a <= b -> false
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_u_gt_done:
    str r14, r0
    ret.l

; OFF ( addr -- ) store 0 at addr
w_off:
    ldn r1, r14
    addi r14, 8
    ldi r7, 0
    str r1, r7
    ret.l

; W@ ( addr -- u16 ) fetch 16-bit LE value
w_wfetch:
    ldn r1, r14               ; addr
    ld.b r7, r1               ; low byte
    inc r1
    ld.b r0, r1               ; high byte
    lsli r0, 8
    or r7, r0
    str r14, r7
    ret.l

; W! ( u16 addr -- ) store 16-bit LE value
w_wstore:
    ldn r1, r14               ; addr
    addi r14, 8
    ldn r7, r14               ; value
    addi r14, 8
    st.b r1, r7               ; low byte
    mov r0, r7
    lsri r0, 8
    inc r1
    st.b r1, r0               ; high byte
    ret.l

; L@ ( addr -- u32 ) fetch 32-bit LE value
w_lfetch:
    ldn r1, r14               ; addr
    ldi r7, 0
    ld.b r0, r1
    or r7, r0                 ; byte 0
    inc r1
    ld.b r0, r1
    lsli r0, 8
    or r7, r0                 ; byte 1
    inc r1
    ld.b r0, r1
    lsli r0, 8
    lsli r0, 8
    or r7, r0                 ; byte 2
    inc r1
    ld.b r0, r1
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r7, r0                 ; byte 3
    str r14, r7
    ret.l

; L! ( u32 addr -- ) store 32-bit LE value
w_lstore:
    ldn r1, r14               ; addr
    addi r14, 8
    ldn r7, r14               ; value
    addi r14, 8
    st.b r1, r7               ; byte 0
    mov r0, r7
    lsri r0, 8
    inc r1
    st.b r1, r0               ; byte 1
    mov r0, r7
    lsri r0, 8
    lsri r0, 8
    inc r1
    st.b r1, r0               ; byte 2
    mov r0, r7
    lsri r0, 8
    lsri r0, 8
    lsri r0, 8
    inc r1
    st.b r1, r0               ; byte 3
    ret.l

; .ZSTR ( addr -- ) print null-terminated string
w_zstr:
    ldn r9, r14
    addi r14, 8
w_zstr_loop:
    ld.b r1, r9
    cmpi r1, 0
    breq w_zstr_done
    call.l r4                 ; emit char (R4 = emit_char)
    inc r9
    br w_zstr_loop
w_zstr_done:
    ret.l

; UCHAR ( c -- C ) convert lowercase to uppercase
w_uchar:
    ldn r1, r14
    cmpi r1, 0x61             ; 'a'
    brcc w_uchar_done          ; C=0 -> r1 < 'a' -> not lowercase
    cmpi r1, 0x7B             ; 'z'+1
    brcs w_uchar_done          ; C=1 -> r1 >= 'z'+1 -> not lowercase
    subi r1, 0x20
    str r14, r1
w_uchar_done:
    ret.l

; TALIGN ( -- ) align HERE to next 64-byte boundary
w_talign:
    ldi64 r11, var_here
    ldn r0, r11               ; HERE
    mov r1, r0
    andi r1, 63               ; HERE mod 64
    cmpi r1, 0
    breq w_talign_done         ; already aligned
    ldi r7, 64
    sub r7, r1                ; bytes needed
    add r0, r7                ; advance HERE
    str r11, r0
w_talign_done:
    ret.l

; ABORT ( -- ) clear stacks, return to REPL
w_abort:
    mov r14, r2               ; reset DSP to ram_size
    lsri r14, 1               ; DSP = ram_size / 2
    mov r15, r2               ; reset RSP to ram_size
    ldi64 r11, forth_quit
    call.l r11                ; jump to QUIT (never returns)
    halt

; ABORT" (IMMEDIATE) -- compile: test flag, if true print message and abort
;   At compile time, parses up to closing '"' and compiles:
;     [ pop flag, test, branch-if-zero past message+abort ]
;     [ inline string data ]
;     [ load string addr -> R10, call print_str, call w_abort ]
w_abort_quote:
    ; Compile: ldn r1, r14  (pop flag to r1)
    ; ldn rD, rE -> 0x50 0xDE  -> 0x50 0x1E
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; Compile: addi r14, 8  (drop from stack)
    ; addi rD, imm8 -> 0x62 0xDE 0xII -> 0x62 0xE0 0x08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; Compile: cmpi r1, 0 -> 0x66 0x10 0x00
    ldi r1, 0x66
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; Compile: lbreq <skip> -> 0x41 XX XX (branch if flag was 0 = false)
    ldi r1, 0x41              ; LBREQ
    ldi64 r11, compile_byte
    call.l r11
    ; Save fixup address (points to offset bytes)
    ldi64 r11, var_here
    ldn r0, r11
    subi r14, 8
    str r14, r0               ; push fixup addr on data stack
    ; Placeholder offset (2 bytes)
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; Compile LBR <over_string> to jump over inline string data
    ldi r1, 0x40              ; LBR opcode
    ldi64 r11, compile_byte
    call.l r11
    ; Save LBR fixup address on data stack
    ldi64 r11, var_here
    ldn r0, r11
    subi r14, 8
    str r14, r0               ; push LBR fixup addr
    ; Placeholder offset bytes
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; Parse the string from input (up to closing '"')
    ldi64 r11, var_to_in
    ldn r0, r11               ; >IN
    ldi64 r9, tib_buffer
    ldi64 r11, var_tib_len
    ldn r7, r11               ; TIB length
    ; Skip leading space
    cmp r0, r7
    lbreq w_abq_empty
    mov r11, r9
    add r11, r0
    ld.b r1, r11
    cmpi r1, 0x20
    brne w_abq_nospc
    inc r0
w_abq_nospc:
    mov r12, r0               ; R12 = string start offset in TIB
w_abq_scan:
    cmp r0, r7
    breq w_abq_endstr
    mov r11, r9
    add r11, r0
    ld.b r1, r11
    cmpi r1, 0x22             ; '"'
    breq w_abq_endstr
    inc r0
    br w_abq_scan
w_abq_endstr:
    mov r13, r0
    sub r13, r12              ; R13 = string length
    ; Advance >IN past closing quote
    cmp r0, r7
    breq w_abq_no_advance
    inc r0
w_abq_no_advance:
    ldi64 r11, var_to_in
    str r11, r0
    ; Compile inline string bytes at HERE
    ldi64 r11, var_here
    ldn r0, r11               ; HERE = string start in code space
    ; Save string addr for later ldi64
    mov r7, r0                ; R7 = string address in code
    ldi r1, 0
w_abq_copy:
    cmp r1, r13
    breq w_abq_copy_done
    mov r11, r9               ; TIB base
    add r11, r12              ; + start offset
    add r11, r1               ; + index
    ld.b r11, r11             ; char from TIB
    mov r0, r7
    add r0, r1                ; code addr + index
    st.b r0, r11              ; write char
    inc r1
    br w_abq_copy
w_abq_copy_done:
    ; Null-terminate
    mov r0, r7
    add r0, r13
    ldi r11, 0
    st.b r0, r11
    ; Update HERE past string + null
    mov r0, r7
    add r0, r13
    inc r0
    ldi64 r11, var_here
    str r11, r0
    ; Resolve LBR <over_string> fixup
    ldn r9, r14               ; pop LBR fixup addr from data stack
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11               ; current HERE (after string)
    mov r1, r0
    sub r1, r9
    subi r1, 2                ; offset = HERE - (fixup_addr + 2)
    mov r11, r1
    lsri r11, 8
    st.b r9, r11              ; high byte
    inc r9
    st.b r9, r1               ; low byte
    ; Now compile: ldi64 r10, <string_addr>; call print_str; call w_abort
    ; Use compile_byte for ldi64 r10 opcode bytes, then compile_call
    ; ldi64 r10: F0 60 A0 <8-byte LE addr>
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x60
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xA0              ; r10 nibble
    ldi64 r11, compile_byte
    call.l r11
    ; 8 bytes of string address (R7) in LE
    mov r1, r7
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r7
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    ; Compile: call.l print_str (13 bytes via compile_call)
    ldi64 r1, print_str
    ldi64 r11, compile_call
    call.l r11
    ; Compile: call.l w_abort (13 bytes via compile_call)
    ldi64 r1, w_abort
    ldi64 r11, compile_call
    call.l r11
    ; Normal path: LBR already resolved, skip to LBREQ resolution
    lbr w_abq_resolve_skip
w_abq_empty:
    ; Empty string case: resolve LBR <over_string> fixup first
    ldn r9, r14               ; pop LBR fixup addr
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11               ; current HERE
    mov r1, r0
    sub r1, r9
    subi r1, 2
    mov r11, r1
    lsri r11, 8
    st.b r9, r11              ; high byte
    inc r9
    st.b r9, r1               ; low byte
w_abq_resolve_skip:
    ; Resolve the forward branch (LBREQ <skip>)
    ldn r9, r14               ; pop fixup addr
    addi r14, 8
    ldi64 r11, var_here
    ldn r0, r11               ; current HERE
    mov r1, r0
    sub r1, r9
    subi r1, 2                ; offset = HERE - (fixup_addr + 2)
    mov r7, r1
    lsri r7, 8
    st.b r9, r7               ; high byte
    inc r9
    st.b r9, r1               ; low byte
    ret.l

; LEAVE (IMMEDIATE) -- compile inline UNLOOP + forward branch
;   Stores fixup address in var_leave_fixups[leave_count].
;   LOOP/+LOOP resolves all fixups after their own code.
w_leave:
    ; Compile: addi r15, 16  -- unloop (drop limit+index from RSP)
    ; Encoding: 0x62 0xF0 0x10
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ; Compile: lbr <placeholder>  -- 0x40 XX XX
    ldi r1, 0x40              ; LBR unconditional
    ldi64 r11, compile_byte
    call.l r11
    ; Record fixup address (HERE points to first offset byte)
    ldi64 r11, var_here
    ldn r13, r11              ; R13 = fixup addr (preserved across compile_byte)
    ; Placeholder offset bytes
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x00
    ldi64 r11, compile_byte
    call.l r11
    ; Store fixup in var_leave_fixups[leave_count]
    ldi64 r11, var_leave_count
    ldn r1, r11               ; count
    ; count < 8 check
    cmpi r1, 8
    brcs w_leave_done          ; C=1 means count >= 8, skip
    ; fixups[count] = fixup_addr (R13)
    mov r9, r1
    lsli r9, 3                ; count * 8
    ldi64 r7, var_leave_fixups
    add r7, r9
    str r7, r13               ; store fixup addr
    ; leave_count++
    inc r1
    str r11, r1               ; update var_leave_count
w_leave_done:
    ret.l

; 2/ ( n -- n/2 ) arithmetic shift right by 1
w_two_slash:
    ldn r1, r14
    lsri r1, 1
    str r14, r1
    ret.l

; COUNT ( c-addr -- addr len ) convert counted string
w_count:
    ldn r1, r14               ; c-addr
    ld.b r7, r1               ; length byte
    inc r1                    ; addr past count byte
    str r14, r1               ; replace TOS with addr
    subi r14, 8
    str r14, r7               ; push len
    ret.l

; MOVE ( src dst u -- ) copy u bytes, handles overlap
w_move:
    ldn r12, r14              ; u
    addi r14, 8
    ldn r7, r14               ; dst
    addi r14, 8
    ldn r9, r14               ; src
    addi r14, 8
    cmpi r12, 0
    breq w_move_done
    ; If dst <= src: forward copy is safe
    cmp r7, r9
    breq w_move_done           ; same addresses, nothing to do
    brle w_move_fwd            ; G=0 after unsigned cmp -> dst <= src
    ; dst > src -- check for overlap
    mov r11, r9
    add r11, r12              ; src + u
    cmp r7, r11
    brcs w_move_fwd            ; C=1 -> dst >= src+u -> no overlap
    ; Backward copy (overlap: src < dst < src+u)
    mov r0, r9
    add r0, r12
    dec r0                    ; src end
    mov r1, r7
    add r1, r12
    dec r1                    ; dst end
w_move_bwd:
    ld.b r11, r0
    st.b r1, r11
    dec r0
    dec r1
    dec r12
    cmpi r12, 0
    brne w_move_bwd
    ret.l
w_move_fwd:
    ld.b r11, r9
    st.b r7, r11
    inc r9
    inc r7
    dec r12
    cmpi r12, 0
    brne w_move_fwd
w_move_done:
    ret.l

; WITHIN ( n lo hi -- flag ) ANS: true if (n-lo) u< (hi-lo)
w_within:
    ldn r1, r14               ; hi
    addi r14, 8
    ldn r7, r14               ; lo
    addi r14, 8
    ldn r0, r14               ; n
    sub r0, r7                ; n - lo
    sub r1, r7                ; hi - lo
    ; unsigned compare: true if (n-lo) < (hi-lo)
    cmp r0, r1
    ; C=0 iff u64(n-lo) < u64(hi-lo)
    ldi r0, 0
    brcs w_within_done         ; C=1 -> (n-lo) >= (hi-lo) -> false
    ldi64 r0, 0xFFFFFFFFFFFFFFFF
w_within_done:
    str r14, r0
    ret.l

; FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
w_find_forth:
    ldn r9, r14               ; c-addr (counted string)
    mov r13, r9               ; save original c-addr
    ld.b r12, r9              ; length
    inc r9                    ; name start
    ; Call find_word: R9=name, R12=len -> R9=entry or 0, R1=flags
    ldi64 r11, find_word
    call.l r11
    cmpi r9, 0
    breq w_find_miss
    ; Found -- get code field address
    mov r0, r1                ; save flags
    ldi64 r11, entry_to_code
    call.l r11
    ; R9 = xt (code field)
    str r14, r9               ; replace c-addr with xt
    ; Determine flag: IMMEDIATE -> 1, else -> -1
    mov r7, r0
    andi r7, 0x80
    subi r14, 8
    cmpi r7, 0
    brne w_find_imm
    ldi r1, 0
    dec r1                    ; -1
    str r14, r1
    ret.l
w_find_imm:
    ldi r1, 1
    str r14, r1
    ret.l
w_find_miss:
    str r14, r13              ; c-addr unchanged
    ldi r1, 0
    subi r14, 8
    str r14, r1
    ret.l

; SOURCE ( -- addr len ) return current input source
w_source:
    ldi64 r1, tib_buffer
    subi r14, 8
    str r14, r1
    ldi64 r11, var_tib_len
    ldn r1, r11
    subi r14, 8
    str r14, r1
    ret.l

; >IN ( -- addr ) push address of >IN variable
w_to_in:
    ldi64 r1, var_to_in
    subi r14, 8
    str r14, r1
    ret.l

; EVALUATE ( addr len -- ) interpret a string as Forth source
w_evaluate:
    ; Check nesting depth limit (max 16)
    ldi64 r11, var_eval_depth
    ldn r1, r11
    cmpi r1, 15
    lbrgt w_eval_depth_err
    ; Increment depth
    inc r1
    str r11, r1
    ldn r12, r14              ; len
    addi r14, 8
    ldn r9, r14               ; addr
    addi r14, 8
    ; Save current input state on return stack
    ldi64 r11, var_to_in
    ldn r1, r11
    subi r15, 8
    str r15, r1               ; save >IN
    ldi64 r11, var_tib_len
    ldn r1, r11
    subi r15, 8
    str r15, r1               ; save TIB len
    ; Copy source string into TIB (max 255 chars)
    cmpi r12, 255
    brle w_eval_len_ok
    ldi r12, 255
w_eval_len_ok:
    ldi64 r7, tib_buffer
    ldi r1, 0
w_eval_copy:
    cmp r1, r12
    breq w_eval_copy_done
    mov r0, r9
    add r0, r1
    ld.b r0, r0
    mov r11, r7
    add r11, r1
    st.b r11, r0
    inc r1
    br w_eval_copy
w_eval_copy_done:
    ; Set up new input
    ldi64 r11, var_tib_len
    str r11, r12
    ldi r1, 0
    ldi64 r11, var_to_in
    str r11, r1
    ; Run the interpreter loop
w_eval_loop:
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_eval_done
    ; Look up word
    ldi64 r11, find_word
    call.l r11
    cmpi r9, 0
    breq w_eval_try_num
    ; Found -- check STATE
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    breq w_eval_exec
    ; Compiling -- check IMMEDIATE
    mov r0, r1
    andi r0, 0x80
    brne w_eval_exec           ; IMMEDIATE -> execute
    ; Compile call to word
    ldi64 r11, entry_to_code
    call.l r11
    mov r1, r9
    ldi64 r11, compile_call
    call.l r11
    lbr w_eval_loop
w_eval_exec:
    ldi64 r11, entry_to_code
    call.l r11
    call.l r9
    lbr w_eval_loop
w_eval_try_num:
    ldi64 r11, var_word_addr
    ldn r9, r11
    ldi64 r11, var_word_len
    ldn r12, r11
    ldi64 r11, parse_number
    call.l r11
    cmpi r0, 0
    breq w_eval_undef
    ; Check STATE
    ldi64 r11, var_state
    ldn r11, r11
    cmpi r11, 0
    breq w_eval_push_num
    ; Compiling -- compile literal
    ldi64 r11, compile_literal
    call.l r11
    lbr w_eval_loop
w_eval_push_num:
    subi r14, 8
    str r14, r1
    lbr w_eval_loop
w_eval_undef:
    ; Check if inside FSLOAD (var_fsload_line > 0)
    ldi64 r11, var_fsload_line
    ldn r11, r11
    cmpi r11, 0
    breq w_eval_undef_no_ctx
    ; Print "  line "
    subi r15, 8
    str r15, r11              ; save line number
    ldi64 r10, str_line_prefix
    ldi64 r11, print_str
    call.l r11
    ; Print line number
    ldn r1, r15
    addi r15, 8
    ldi64 r11, print_number
    call.l r11
    ; Print ": "
    ldi64 r10, str_colon_space
    ldi64 r11, print_str
    call.l r11
w_eval_undef_no_ctx:
    ldi64 r11, var_word_addr
    ldn r9, r11
    ldi64 r11, var_word_len
    ldn r12, r11
    ldi64 r11, print_counted
    call.l r11
    ldi64 r10, str_undefined
    ldi64 r11, print_str
    call.l r11
w_eval_done:
    ; Restore input state from return stack
    ldn r1, r15
    addi r15, 8
    ldi64 r11, var_tib_len
    str r11, r1
    ldn r1, r15
    addi r15, 8
    ldi64 r11, var_to_in
    str r11, r1
    ; Decrement EVALUATE depth
    ldi64 r11, var_eval_depth
    ldn r1, r11
    dec r1
    str r11, r1
    ret.l

w_eval_depth_err:
    ; Depth limit exceeded — drop args and print error
    addi r14, 16              ; drop addr len
    ldi64 r10, str_eval_depth
    ldi64 r11, print_str
    call.l r11
    ret.l

; COMPARE ( addr1 u1 addr2 u2 -- n ) compare two strings
w_compare:
    ldn r12, r14              ; u2
    addi r14, 8
    ldn r7, r14               ; addr2
    addi r14, 8
    ldn r13, r14              ; u1
    addi r14, 8
    ldn r9, r14               ; addr1
    ; R9=addr1, R13=u1, R7=addr2, R12=u2
    ldi r0, 0                 ; index
w_cmp_loop:
    cmp r0, r13
    breq w_cmp_s1end
    cmp r0, r12
    breq w_cmp_s2end
    mov r1, r9
    add r1, r0
    ld.b r1, r1               ; s1[i]
    mov r11, r7
    add r11, r0
    ld.b r11, r11             ; s2[i]
    cmp r1, r11
    brcc w_cmp_lt              ; C=0 -> s1[i] < s2[i] unsigned
    brgt w_cmp_gt              ; G=1 -> s1[i] > s2[i] unsigned
    inc r0
    br w_cmp_loop
w_cmp_s1end:
    cmp r0, r12
    breq w_cmp_eq
w_cmp_lt:
    ldi r1, 0
    dec r1                    ; -1
    str r14, r1
    ret.l
w_cmp_s2end:
w_cmp_gt:
    ldi r1, 1
    str r14, r1
    ret.l
w_cmp_eq:
    ldi r1, 0
    str r14, r1
    ret.l

; CHAR ( "name" -- c ) parse word, push first character
w_char:
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    breq w_char_empty
    ld.b r1, r9
    subi r14, 8
    str r14, r1
    ret.l
w_char_empty:
    ldi r1, 0
    subi r14, 8
    str r14, r1
    ret.l

; [CHAR] (IMMEDIATE) -- compile literal of next char
w_bracket_char:
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    breq w_bchar_empty
    ld.b r1, r9
    ldi64 r11, compile_literal
    call.l r11
w_bchar_empty:
    ret.l

; RECURSE (IMMEDIATE) -- compile call to current definition
w_recurse:
    ldi64 r11, var_latest
    ldn r9, r11
    ldi64 r11, entry_to_code
    call.l r11
    mov r1, r9
    ldi64 r11, compile_call
    call.l r11
    ret.l


; =====================================================================
;  Phase 1C — VALUE/TO, POSTPONE, 2>R/2R>/2R@, DOES>
; =====================================================================

; VALUE ( x "name" -- )
;   Like VARIABLE but at runtime pushes the *contents* of the data cell.
;   Trampoline (19 bytes):
;     ldi64 r1, <data_addr>   F0 60 10 + 8 bytes = 11
;     ldn r1, r1              50 11                = 2
;     subi r14, 8             67 E0 08             = 3
;     str r14, r1             54 E1                = 2
;     ret.l                   0E                   = 1
;   data_addr = code_start + 19
w_value:
    ; Get initial value from stack
    ldn r10, r14              ; x → R10 (safe across parse_word)
    addi r14, 8
    ; Parse name
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_colon_err

    ; Build dictionary entry header at HERE
    ldi64 r11, var_here
    ldn r0, r11               ; R0 = HERE
    mov r13, r0               ; save entry start

    ; Link
    ldi64 r11, var_latest
    ldn r1, r11
    str r0, r1
    addi r0, 8

    ; Flags+len
    st.b r0, r12
    inc r0

    ; Copy name
    ldi r1, 0
w_val_copy:
    cmp r1, r12
    breq w_val_name_done
    mov r7, r9
    add r7, r1
    ld.b r7, r7
    st.b r0, r7
    inc r0
    inc r1
    br w_val_copy
w_val_name_done:
    ; data_addr = R0 + 19
    mov r1, r0
    addi r1, 19

    ; ldi64 r1, <data_addr>: F0 60 10 + 8 bytes LE
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x60
    st.b r0, r7
    inc r0
    ldi r7, 0x10
    st.b r0, r7
    inc r0
    ; 8 bytes of data_addr (R1) LE
    st.b r0, r1
    inc r0
    mov r7, r1
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r1
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ; ldn r1, r1: 50 11
    ldi r7, 0x50
    st.b r0, r7
    inc r0
    ldi r7, 0x11
    st.b r0, r7
    inc r0
    ; subi r14, 8: 67 E0 08
    ldi r7, 0x67
    st.b r0, r7
    inc r0
    ldi r7, 0xE0
    st.b r0, r7
    inc r0
    ldi r7, 0x08
    st.b r0, r7
    inc r0
    ; str r14, r1: 54 E1
    ldi r7, 0x54
    st.b r0, r7
    inc r0
    ldi r7, 0xE1
    st.b r0, r7
    inc r0
    ; ret.l: 0E
    ldi r7, 0x0E
    st.b r0, r7
    inc r0
    ; Now R0 = data cell address. Initialize to x (R10).
    str r0, r10
    addi r0, 8
    ; Update HERE and LATEST
    ldi64 r11, var_here
    str r11, r0
    ldi64 r11, var_latest
    str r11, r13
    ret.l

; TO ( x "name" -- )  [IMMEDIATE]
;   Stores x into the data field of a VALUE word.
;   Extracts data_addr from the ldi64 immediate at code+3.
;   State-smart: interpret → store directly; compile → emit inline store.
w_to:
    ; Parse next word
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_colon_err

    ; Find in dictionary
    ldi64 r11, find_word
    call.l r11
    cmpi r9, 0
    lbreq interp_undefined

    ; entry_to_code → R9 = code addr
    ldi64 r11, entry_to_code
    call.l r11

    ; Extract data_addr from ldi64 immediate at code+3
    ; The trampoline starts with: F0 60 10 <8-byte LE addr>
    ; So data_addr = 8-byte LE at R9+3
    mov r11, r9
    addi r11, 3
    ; Read 8 bytes LE into R10
    ld.b r10, r11
    inc r11
    ld.b r0, r11
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    inc r11
    ld.b r0, r11
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    lsli r0, 8
    or r10, r0
    ; R10 = data_addr

    ; Check STATE
    ldi64 r11, var_state
    ldn r0, r11
    cmpi r0, 0
    brne w_to_compile

    ; --- Interpret mode: store TOS at data_addr ---
    ldn r1, r14
    addi r14, 8
    str r10, r1
    ret.l

w_to_compile:
    ; --- Compile mode: emit inline code ---
    ; Emit: ldn r1, r14      (50 1E)     = 2 bytes (pop TOS)
    ;       addi r14, 8      (62 E0 08)  = 3 bytes
    ;       ldi64 r11, <da>  (F0 60 B0 + 8 bytes) = 11 bytes
    ;       str r11, r1      (54 B1)     = 2 bytes
    ; Total = 18 bytes
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldi64 r11, <data_addr>  — R10 holds data_addr
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x60
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xB0
    ldi64 r11, compile_byte
    call.l r11
    ; 8 bytes of R10 LE
    mov r1, r10
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    mov r1, r10
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    lsri r1, 8
    andi r1, 0xFF
    ldi64 r11, compile_byte
    call.l r11
    ; str r11, r1: 54 B1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xB1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; POSTPONE ( "name" -- )  [IMMEDIATE]
;   If name is IMMEDIATE: compile a call to it (normal compile).
;   If name is NOT IMMEDIATE: compile code that at runtime compiles a call.
;   For non-immediate: compile_literal(code_addr) then compile_call(postpone_helper)
;   postpone_helper pops code_addr from stack into R1 and calls compile_call.
w_postpone:
    ; Parse next word
    ldi64 r11, parse_word
    call.l r11
    cmpi r12, 0
    lbreq w_colon_err

    ; Find in dictionary
    ldi64 r11, find_word
    call.l r11
    ; R9 = entry (0 = not found), R1 = flags byte
    cmpi r9, 0
    lbreq interp_undefined

    mov r10, r1               ; save flags in R10
    ; entry_to_code → R9 = code addr
    ldi64 r11, entry_to_code
    call.l r11
    ; R9 = code addr

    ; Check IMMEDIATE flag
    mov r0, r10
    andi r0, 0x80
    brne w_postpone_imm

    ; Non-IMMEDIATE: compile code to compile a call at runtime
    ; Step 1: compile_literal(code_addr) — pushes code_addr onto stack at runtime
    mov r1, r9
    ldi64 r11, compile_literal
    call.l r11
    ; Step 2: compile_call(postpone_helper) — calls helper that does the compile
    ldi64 r1, postpone_helper
    ldi64 r11, compile_call
    call.l r11
    ret.l

w_postpone_imm:
    ; IMMEDIATE word: just compile a call to it
    mov r1, r9
    ldi64 r11, compile_call
    call.l r11
    ret.l

; postpone_helper: runtime helper for POSTPONE of non-immediate words.
;   Pops code_addr from data stack, compiles a call to it.
postpone_helper:
    ldn r1, r14
    addi r14, 8
    ldi64 r11, compile_call
    call.l r11
    ret.l

; 2>R (IMMEDIATE) — compile inline: pop two from data stack, push to return stack
;   Emits:  ldn r1, r14      (50 1E)     — x2 (top)
;           addi r14, 8      (62 E0 08)
;           ldn r0, r14      (50 0E)     — x1
;           addi r14, 8      (62 E0 08)
;           subi r15, 8      (67 F0 08)  — push x1 first (deeper)
;           str r15, r0      (54 F0)
;           subi r15, 8      (67 F0 08)  — push x2 on top
;           str r15, r1      (54 F1)
;   Total = 20 bytes
w_2to_r:
    ; ldn r1, r14: 50 1E
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8: 62 E0 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r0, r14: 50 0E
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x0E
    ldi64 r11, compile_byte
    call.l r11
    ; addi r14, 8: 62 E0 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; subi r15, 8: 67 F0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r15, r0: 54 F0
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ; subi r15, 8: 67 F0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r15, r1: 54 F1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; 2R> (IMMEDIATE) — compile inline: pop two from return stack, push to data stack
;   Emits:  ldn r1, r15      (50 1F)     — x2 (top of RSP)
;           addi r15, 8      (62 F0 08)
;           ldn r0, r15      (50 0F)     — x1
;           addi r15, 8      (62 F0 08)
;           subi r14, 8      (67 E0 08)  — push x1 first (deeper)
;           str r14, r0      (54 E0)
;           subi r14, 8      (67 E0 08)  — push x2 on top
;           str r14, r1      (54 E1)
;   Total = 20 bytes
w_2r_from:
    ; ldn r1, r15: 50 1F
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ; addi r15, 8: 62 F0 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r0, r15: 50 0F
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x0F
    ldi64 r11, compile_byte
    call.l r11
    ; addi r15, 8: 62 F0 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xF0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; subi r14, 8: 67 E0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r14, r0: 54 E0
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ; subi r14, 8: 67 E0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r14, r1: 54 E1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE1
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; 2R@ (IMMEDIATE) — compile inline: copy two from return stack, push to data stack
;   Emits:  ldn r0, r15      (50 0F)     — x2 (top of RSP)
;           mov r1, r15      (42 1F)
;           addi r1, 8       (62 10 08)
;           ldn r1, r1       (50 11)     — x1
;           subi r14, 8      (67 E0 08)  — push x1 first (deeper)
;           str r14, r1      (54 E1)
;           subi r14, 8      (67 E0 08)  — push x2 on top
;           str r14, r0      (54 E0)
;   Total = 19 bytes
w_2r_fetch:
    ; ldn r0, r15: 50 0F
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x0F
    ldi64 r11, compile_byte
    call.l r11
    ; mov r1, r15: 42 1F
    ldi r1, 0x42
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x1F
    ldi64 r11, compile_byte
    call.l r11
    ; addi r1, 8: 62 10 08
    ldi r1, 0x62
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x10
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; ldn r1, r1: 50 11
    ldi r1, 0x50
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x11
    ldi64 r11, compile_byte
    call.l r11
    ; subi r14, 8: 67 E0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r14, r1: 54 E1
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE1
    ldi64 r11, compile_byte
    call.l r11
    ; subi r14, 8: 67 E0 08
    ldi r1, 0x67
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0x08
    ldi64 r11, compile_byte
    call.l r11
    ; str r14, r0: 54 E0
    ldi r1, 0x54
    ldi64 r11, compile_byte
    call.l r11
    ldi r1, 0xE0
    ldi64 r11, compile_byte
    call.l r11
    ret.l

; DOES> (IMMEDIATE) — CREATE…DOES> defining pattern
;   At compile time: emits call to does_runtime, then ret.l.
;   The code after the ret.l is the DOES> body.
;   At runtime (when the defining word executes): does_runtime patches
;   the most recently CREATEd word's trampoline.
;
;   CREATE emits a 30-byte trampoline (with 14-byte DOES> slot):
;     offset 0-10:  ldi64 r1, <data_addr>   (11 bytes)
;     offset 11-13: subi r14, 8             (3 bytes)
;     offset 14-15: str r14, r1             (2 bytes)
;     offset 16:    ret.l                   (1 byte) ← DOES> overwrites here
;     offset 17-29: padding                 (13 bytes)
;     data field starts at offset 30
;
;   DOES> patches offset 16-29 with:
;     ldi64 r11, <does_body>  (11 bytes)
;     call.l r11              (2 bytes)
;     ret.l                   (1 byte)
w_does:
    ; Compile call to does_runtime
    ldi64 r1, does_runtime
    ldi64 r11, compile_call
    call.l r11
    ; Compile ret.l — ends the defining word's runtime
    ldi64 r11, compile_ret
    call.l r11
    ; The code compiled AFTER this point (by ; and friends) becomes the DOES> body.
    ; Actually we need to NOT compile ret.l here for the DOES> body to follow...
    ; Wait — the DOES> body is compiled by the outer interpreter after we return.
    ; But we just compiled ret.l, so the defining word ends here.
    ; The DOES> body code address = HERE (right after the ret.l we just compiled).
    ; When the defining word runs, does_runtime reads the return address
    ; (which points past the call to does_runtime, i.e. to the ret.l),
    ; then does_body = ret.l_addr + 1 = the DOES> body code.
    ret.l

; does_runtime: runtime helper for DOES>
;   Called when the defining word executes. The return address on RSP points
;   to the byte after "call does_runtime", which is a ret.l. So:
;     does_body = return_addr + 1
;   Patches LATEST (the most recently CREATEd word) trampoline at offset 16.
does_runtime:
    ; Get return address from RSP (the call.l pushed it)
    ldn r10, r15              ; R10 = return address (points to ret.l)
    ; does_body = R10 + 1 (skip the ret.l)
    mov r13, r10
    addi r13, 1               ; R13 = does_body address

    ; Get LATEST entry → code addr
    ldi64 r11, var_latest
    ldn r9, r11
    ldi64 r11, entry_to_code
    call.l r11
    ; R9 = code_start of the CREATEd word

    ; Patch offset 16-29 of the trampoline
    ; offset 16: ldi64 r11, <does_body> = F0 60 B0 + 8 bytes LE
    mov r0, r9
    addi r0, 16
    ldi r7, 0xF0
    st.b r0, r7
    inc r0
    ldi r7, 0x60
    st.b r0, r7
    inc r0
    ldi r7, 0xB0
    st.b r0, r7
    inc r0
    ; 8 bytes of does_body addr (R13) LE
    st.b r0, r13
    inc r0
    mov r7, r13
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    mov r7, r13
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    lsri r7, 8
    st.b r0, r7
    inc r0
    ; call.l r11: 0D 0B
    ldi r7, 0x0D
    st.b r0, r7
    inc r0
    ldi r7, 0x0B
    st.b r0, r7
    inc r0
    ; ret.l: 0E
    ldi r7, 0x0E
    st.b r0, r7

    ; Return from the defining word (does_runtime was called, so just ret.l)
    ; The ret.l we return to is the one we're pointing at (R10), which exits
    ; the defining word.
    ret.l


; =====================================================================
;  Phase 1D — >NUMBER, QUIT
; =====================================================================

; >NUMBER ( ud addr len -- ud' addr' len' )
;   ANS Forth >NUMBER. Converts characters to digits using BASE.
;   ud is a double-cell accumulator (low cell deeper, high cell on top).
;   Processes from addr for len characters, stopping at first non-digit.
;   Returns updated accumulator, advanced addr, and remaining length.
;
;   Stack: ( ud_lo ud_hi addr len -- ud_lo' ud_hi' addr' len' )
;   We simplify: treat ud as a single 64-bit value (ignore the double-cell
;   distinction since our cells are 64-bit).
;
;   Algorithm per char c at addr:
;     digit = char_to_digit(c, BASE)
;     if digit >= BASE → stop
;     ud = ud * BASE + digit
;     addr++, len--
w_to_number:
    ; Stack: DSP+24=ud_lo, DSP+16=ud_hi, DSP+8=addr, DSP+0=len
    ldn r12, r14              ; len
    mov r11, r14
    addi r11, 8
    ldn r9, r11               ; addr
    mov r11, r14
    addi r11, 16
    ldn r1, r11               ; ud_hi (we'll use as accumulator)
    ; We treat (ud_lo, ud_hi) as single value, using ud_hi only.
    ; For ANS compat with 64-bit cells, this is fine.

    ; Load BASE
    ldi64 r11, var_base
    ldn r10, r11              ; R10 = BASE

w_tnum_loop:
    cmpi r12, 0
    lbreq w_tnum_done

    ; Read char at addr
    ld.b r0, r9               ; R0 = char

    ; char_to_digit: '0'-'9' → 0-9, 'A'-'Z' → 10-35, 'a'-'z' → 10-35
    subi r0, 0x30
    cmpi r0, 10
    brcc w_tnum_digit          ; < 10 → it's a digit 0-9
    ; Try uppercase: restore, subtract 'A', add 10
    addi r0, 0x30
    subi r0, 0x41
    cmpi r0, 26
    brcs w_tnum_try_lc         ; >= 26 → try lowercase
    addi r0, 10
    br w_tnum_check
w_tnum_try_lc:
    addi r0, 0x41
    subi r0, 0x61
    cmpi r0, 26
    lbrcs w_tnum_done          ; >= 26 → not a letter → stop
    addi r0, 10
w_tnum_check:
    ; R0 = digit value. Check digit < BASE
    cmp r0, r10
    lbreq w_tnum_done          ; digit == BASE → stop
    lbrgt w_tnum_done          ; digit > BASE → stop
w_tnum_digit:
    ; Also check digit < BASE for the 0-9 path
    cmp r0, r10
    lbreq w_tnum_done
    lbrgt w_tnum_done
    ; ud = ud * BASE + digit
    mul r1, r10
    add r1, r0
    inc r9
    dec r12
    lbr w_tnum_loop

w_tnum_done:
    ; Write back: len, addr, ud_hi, ud_lo
    str r14, r12              ; len
    mov r11, r14
    addi r11, 8
    str r11, r9               ; addr
    mov r11, r14
    addi r11, 16
    str r11, r1               ; ud_hi
    ; ud_lo = 0 (we folded everything into ud_hi)
    mov r11, r14
    addi r11, 24
    ldi r0, 0
    str r11, r0
    ret.l

; QUIT ( -- )
;   Reset return stack, enter outer interpreter loop. Does not return.
w_quit:
    ldi64 r11, forth_quit
    call.l r11
    halt


; =====================================================================
;  Bus Fault Handler
; =====================================================================
bus_fault_handler:
    ldi64 r8, 0xFFFF_FF00_0000_0000
    ldi64 r4, emit_char
    ldi64 r5, key_char
    ldi64 r6, print_hex_byte
    csrr r0, 0x25
    ldi64 r10, str_busfault
    ldi64 r11, print_str
    call.l r11
    mov r1, r0
    ldi64 r11, print_hex32
    call.l r11
    ldi64 r11, print_crlf
    call.l r11
    ; Resume REPL
    addi r15, 16
    ldi64 r11, quit_loop
    call.l r11
    halt

; =====================================================================
;  Multicore — Secondary Core Entry, IPI Handler, Worker Loop
; =====================================================================
;
;  All cores boot at 0x0000.  Core 0 runs the normal BIOS.  Cores 1-3
;  jump here, set up their per-core stacks, install the IVT, enable
;  interrupts, and enter IDL waiting for an IPI.
;
;  The IPI handler reads the 64-bit message from the mailbox (an XT to
;  execute).  If the XT is non-zero the worker loop calls it.  When it
;  returns the core re-enters IDL.
;
;  Per-core stack layout (within each core's 64 KiB zone):
;    RSP = zone top (R15, grows down)
;    DSP = zone top - 0x8000 (R14, grows down)
;
;  MMIO addresses:
;    MBOX_BASE   = 0xFFFF_FF00_0000_0500
;    MBOX_DATA   = MBOX_BASE + 0x00 .. 0x07  (8 bytes, 64-bit message)
;    MBOX_SEND   = MBOX_BASE + 0x08  (write target core ID)
;    MBOX_STATUS = MBOX_BASE + 0x09  (read pending bitmask)
;    MBOX_ACK    = MBOX_BASE + 0x0A  (write source core ID to clear)
;
;    SPIN_BASE   = 0xFFFF_FF00_0000_0600
;    SPIN_ACQ(n) = SPIN_BASE + n*4      (read: 0=acquired, 1=busy)
;    SPIN_REL(n) = SPIN_BASE + n*4 + 1  (write: release)
; =====================================================================

secondary_core_entry:
    ; ---- Set per-core stacks ----
    ; R2 is already set to per-core stack top by the system
    ; RSP = R2 (return stack at top of zone)
    mov r15, r2
    ; DSP = R2 - 0x8000 (data stack in lower half of zone)
    mov r14, r2
    ldi64 r11, 0x8000
    sub r14, r11

    ; ---- Set up UART base and subroutine pointers (for any I/O) ----
    ldi64 r8, 0xFFFF_FF00_0000_0000
    ldi64 r4, emit_char
    ldi64 r5, key_char
    ldi64 r6, print_hex_byte

    ; ---- Install IVT (same table as core 0 — shared) ----
    ldi64 r0, ivt_table
    csrw 0x04, r0

    ; ---- Clear the per-core worker XT slot ----
    csrr r0, 0x20                       ; core ID
    ldi r1, 3                           ; shift = log2(8)
    shl r0, r1                          ; core_id * 8
    ldi64 r11, worker_xt_table
    add r11, r0
    ldi r1, 0
    str r11, r1                         ; worker_xt[core_id] = 0

    ; ---- Enable interrupts and enter idle ----
    ei

secondary_idle_loop:
    idl                                 ; sleep until IPI

    ; ---- Woken by IPI — check if we have an XT to execute ----
    csrr r0, 0x20                       ; core ID
    ldi r1, 3
    shl r0, r1
    ldi64 r11, worker_xt_table
    add r11, r0
    ldn r0, r11                         ; R0 = worker_xt[core_id]
    cmpi r0, 0
    lbreq secondary_idle_loop           ; spurious wake, re-idle

    ; ---- Execute the XT ----
    ; R11 points to our worker_xt slot — save it on return stack
    ; (push BEFORE reset so we can place it at the very bottom of RSP)

    ; Reset stacks before executing worker XT
    mov r15, r2                         ; RSP = zone top
    mov r14, r2
    ldi64 r11, 0x8000
    sub r14, r11                        ; DSP = zone top - 0x8000

    ; Re-compute our worker_xt slot address (R11 was clobbered above)
    csrr r1, 0x20                       ; core ID
    ldi r7, 3
    shl r1, r7                          ; core_id * 8
    ldi64 r11, worker_xt_table
    add r11, r1                         ; R11 → worker_xt[core_id]

    ; Push slot pointer on return stack (so call.l's ret can't clobber it)
    subi r15, 8
    str r15, r11                        ; save on stack

    ; Call the XT (it's a Forth execution token = code address)
    call.l r0

    ; Worker returned — retrieve slot pointer and clear it
    ldn r11, r15
    addi r15, 8
    ldi r1, 0
    str r11, r1                         ; worker_xt[core_id] = 0

    ; Worker returned — go back to idle
    lbr secondary_idle_loop

; =====================================================================
;  IPI Interrupt Handler (IVT slot 8)
; =====================================================================
;  Called when this core receives an inter-processor interrupt.
;  Reads the mailbox to find who sent the IPI, extracts the 64-bit
;  message (an XT), stores it in worker_xt_table, ACKs the IPI,
;  and returns.  The secondary_idle_loop picks up the XT.
;
;  For core 0 (which runs the Forth REPL), the handler just stores
;  the XT and returns — the user can poll with IPI-STATUS.
; =====================================================================
ipi_handler:
    ; ---- Save registers we'll use ----
    subi r15, 8
    str r15, r0
    subi r15, 8
    str r15, r1
    subi r15, 8
    str r15, r7
    subi r15, 8
    str r15, r9
    subi r15, 8
    str r15, r11
    subi r15, 8
    str r15, r12

    ; ---- Read MBOX_STATUS to find which core(s) sent us an IPI ----
    ldi64 r11, 0xFFFF_FF00_0000_0509    ; MBOX_STATUS
    ld.b r7, r11                        ; R7 = pending bitmask

    ; ---- Find the lowest set bit (sender core ID) ----
    ; Simple scan: check bits 0-3
    ldi r9, 0                           ; sender = 0
ipi_find_sender:
    mov r0, r7
    ldi r1, 1
    and r0, r1                          ; test bit 0
    cmpi r0, 0
    lbrne ipi_found_sender
    lsri r7, 1
    addi r9, 1
    cmpi r9, 4
    lbrne ipi_find_sender
    ; No sender found (shouldn't happen) — bail
    lbr ipi_handler_done

ipi_found_sender:
    ; R9 = sender core ID
    ; ---- Read 64-bit message (XT) from MBOX_DATA ----
    ldi64 r11, 0xFFFF_FF00_0000_0500    ; MBOX_DATA base
    ; Read 8 bytes (little-endian 64-bit value)
    ldi r0, 0                           ; accumulator
    ldi r12, 0                          ; byte index
ipi_read_data:
    mov r1, r11
    add r1, r12
    ld.b r1, r1                         ; byte[i]
    ; shift byte into position: r1 << (r12 * 8)
    mov r7, r12
    ldi r9, 3                           ; shift by 3 to multiply by 8
    shl r7, r9                          ; r7 = byte_index * 8
    shl r1, r7                          ; r1 = byte << (i*8)
    or r0, r1                           ; accumulate
    addi r12, 1
    cmpi r12, 8
    lbrne ipi_read_data

    ; R0 = 64-bit XT from mailbox

    ; ---- Store XT in worker_xt_table[my_core_id] ----
    csrr r1, 0x20                       ; my core ID
    ldi r7, 3
    shl r1, r7                          ; core_id * 8
    ldi64 r11, worker_xt_table
    add r11, r1
    str r11, r0                         ; worker_xt[core_id] = XT

    ; ---- ACK the IPI from the sender ----
    ; Re-read STATUS to get sender (we clobbered r9)
    ldi64 r11, 0xFFFF_FF00_0000_0509    ; MBOX_STATUS
    ld.b r7, r11                        ; pending mask
    ldi r9, 0
ipi_ack_find:
    mov r0, r7
    ldi r1, 1
    and r0, r1
    cmpi r0, 0
    lbrne ipi_ack_found
    lsri r7, 1
    addi r9, 1
    cmpi r9, 4
    lbrne ipi_ack_find
    lbr ipi_handler_done
ipi_ack_found:
    ; R9 = sender core ID — ACK it
    ldi64 r11, 0xFFFF_FF00_0000_050A    ; MBOX_ACK
    st.b r11, r9                        ; ACK sender

ipi_handler_done:
    ; ---- Restore registers ----
    ldn r12, r15
    addi r15, 8
    ldn r11, r15
    addi r15, 8
    ldn r9, r15
    addi r15, 8
    ldn r7, r15
    addi r15, 8
    ldn r1, r15
    addi r15, 8
    ldn r0, r15
    addi r15, 8
    ; Return from interrupt
    rti

; Per-core worker XT slots (one 64-bit entry per core)
worker_xt_table:
    .dq 0                               ; core 0
    .dq 0                               ; core 1
    .dq 0                               ; core 2
    .dq 0                               ; core 3

; =====================================================================
;  Multicore Forth Words
; =====================================================================

; COREID ( -- n )  push this core's hardware ID (0-3)
w_coreid:
    csrr r0, 0x20
    subi r14, 8
    str r14, r0
    ret.l

; NCORES ( -- n )  push number of hardware cores
w_ncores:
    csrr r0, 0x21
    subi r14, 8
    str r14, r0
    ret.l

; IPI-SEND ( xt core -- )  send XT to target core via IPI
;   Writes the 64-bit XT to mailbox DATA, then writes target core
;   to MBOX_SEND to trigger the IPI.
w_ipi_send:
    ldn r1, r14                         ; R1 = target core ID
    addi r14, 8
    ldn r0, r14                         ; R0 = XT (64-bit value)
    addi r14, 8
    ; Write XT to MBOX_DATA (8 bytes, little-endian)
    ldi64 r11, 0xFFFF_FF00_0000_0500    ; MBOX_DATA
    ldi r12, 0                          ; byte index
ipi_send_loop:
    mov r7, r0
    ldi r9, 0xFF
    and r7, r9                          ; low byte
    mov r9, r11
    add r9, r12
    st.b r9, r7                         ; write byte
    lsri r0, 8                          ; shift right 8
    addi r12, 1
    cmpi r12, 8
    lbrne ipi_send_loop
    ; Send IPI to target
    ldi64 r11, 0xFFFF_FF00_0000_0508    ; MBOX_SEND
    st.b r11, r1
    ret.l

; IPI-STATUS ( -- mask )  read pending IPI bitmask for this core
w_ipi_status:
    ldi64 r11, 0xFFFF_FF00_0000_0509    ; MBOX_STATUS
    ld.b r0, r11
    subi r14, 8
    str r14, r0
    ret.l

; IPI-ACK ( core -- )  acknowledge IPI from given core
w_ipi_ack:
    ldn r0, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_050A    ; MBOX_ACK
    st.b r11, r0
    ret.l

; MBOX! ( d -- )  write 64-bit value to mailbox outgoing data
w_mbox_store:
    ldn r0, r14                         ; R0 = 64-bit value
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_0500    ; MBOX_DATA
    ldi r12, 0
mbox_store_loop:
    mov r7, r0
    ldi r9, 0xFF
    and r7, r9
    mov r9, r11
    add r9, r12
    st.b r9, r7
    lsri r0, 8
    addi r12, 1
    cmpi r12, 8
    lbrne mbox_store_loop
    ret.l

; MBOX@ ( -- d )  read 64-bit value from mailbox incoming data
w_mbox_fetch:
    ldi64 r11, 0xFFFF_FF00_0000_0500    ; MBOX_DATA
    ldi r0, 0                           ; accumulator
    ldi r12, 0
mbox_fetch_loop:
    mov r1, r11
    add r1, r12
    ld.b r1, r1
    mov r7, r12
    ldi r9, 3
    shl r7, r9                          ; byte_index * 8
    shl r1, r7
    or r0, r1
    addi r12, 1
    cmpi r12, 8
    lbrne mbox_fetch_loop
    subi r14, 8
    str r14, r0
    ret.l

; SPIN@ ( n -- flag )  try to acquire spinlock n; flag = 0 if acquired
w_spin_fetch:
    ldn r0, r14                         ; R0 = lock number
    ; Address = SPIN_BASE + n * 4
    ldi r1, 2
    shl r0, r1                          ; n * 4
    ldi64 r11, 0xFFFF_FF00_0000_0600    ; SPIN_BASE
    add r11, r0
    ld.b r0, r11                        ; read ACQUIRE: 0=got it, 1=busy
    str r14, r0                         ; replace TOS with result
    ret.l

; SPIN! ( n -- )  release spinlock n
w_spin_release:
    ldn r0, r14                         ; R0 = lock number
    addi r14, 8                         ; drop
    ; Address = SPIN_BASE + n * 4 + 1
    ldi r1, 2
    shl r0, r1                          ; n * 4
    ldi64 r11, 0xFFFF_FF00_0000_0600    ; SPIN_BASE
    add r11, r0
    addi r11, 1                         ; +1 for RELEASE register
    ldi r0, 1
    st.b r11, r0                        ; write anything to release
    ret.l

; WAKE-CORE ( xt core -- )  convenience: send XT via IPI to wake a core
;   Pre-writes the XT into worker_xt_table[core] so CORE-STATUS sees
;   non-zero immediately, then sends the IPI.  The handler will
;   overwrite the slot with the same value — harmless.
w_wake_core:
    ; Pre-write XT into worker_xt_table[core] (shared memory)
    ; Stack: ( xt core -- )  → TOS = core, NOS = xt
    ldn r0, r14                         ; R0 = core ID (TOS)
    ldi r1, 3
    shl r0, r1                          ; core_id * 8
    ldi64 r11, worker_xt_table
    add r11, r0                         ; R11 → worker_xt[core]
    addi r14, 8                         ; peek at NOS (xt)
    ldn r0, r14                         ; R0 = xt
    str r11, r0                         ; worker_xt[core] = xt
    subi r14, 8                         ; restore stack pointer
    ; Now delegate to IPI-SEND (stack still: xt core)
    ldi64 r11, w_ipi_send
    call.l r11
    ret.l

; =====================================================================
;  Performance Counter Words
; =====================================================================

; PERF-CYCLES ( -- n )  push cycle counter
w_perf_cycles:
    csrr r0, 0x68
    subi r14, 8
    str r14, r0
    ret.l

; PERF-STALLS ( -- n )  push stall counter
w_perf_stalls:
    csrr r0, 0x69
    subi r14, 8
    str r14, r0
    ret.l

; PERF-TILEOPS ( -- n )  push tile-op counter
w_perf_tileops:
    csrr r0, 0x6A
    subi r14, 8
    str r14, r0
    ret.l

; PERF-EXTMEM ( -- n )  push external-memory beat counter
w_perf_extmem:
    csrr r0, 0x6B
    subi r14, 8
    str r14, r0
    ret.l

; PERF-RESET ( -- )  reset all perf counters and re-enable
w_perf_reset:
    ldi r0, 3
    csrw 0x6C, r0
    ret.l

; =====================================================================
;  CRC Engine — MMIO at 0xFFFF_FF00_0000_07C0
; =====================================================================
; CRC base   = 0xFFFF_FF00_0000_07C0
;   POLY  +0x00 (W)  polynomial select: 0=CRC32, 1=CRC32C, 2=CRC64
;   INIT  +0x08 (W)  initial CRC value (64-bit LE)
;   DIN   +0x10 (W)  data input (8 bytes, processes on full write)
;   RESULT+0x18 (R)  current CRC value (64-bit LE)
;   CTRL  +0x20 (W)  0=reset to init, 1=finalize (XOR-out)

; CRC-POLY! ( n -- )  set polynomial: 0=CRC32, 1=CRC32C, 2=CRC64
w_crc_poly_store:
    ldn r0, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_07C0    ; CRC_POLY
    str r11, r0
    ret.l

; CRC-INIT! ( n -- )  set initial CRC value
w_crc_init_store:
    ldn r0, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_07C8    ; CRC_INIT
    str r11, r0
    ret.l

; CRC-FEED ( n -- )  feed 8 bytes of data
w_crc_feed:
    ldn r0, r14
    addi r14, 8
    ldi64 r11, 0xFFFF_FF00_0000_07D0    ; CRC_DIN
    str r11, r0
    ret.l

; CRC@ ( -- n )  read current CRC result
w_crc_fetch:
    ldi64 r11, 0xFFFF_FF00_0000_07D8    ; CRC_RESULT
    ldn r0, r11
    subi r14, 8
    str r14, r0
    ret.l

; CRC-RESET ( -- )  reset CRC to init value
w_crc_reset:
    ldi r0, 0
    ldi64 r11, 0xFFFF_FF00_0000_07E0    ; CRC_CTRL = 0 (reset)
    str r11, r0
    ret.l

; CRC-FINAL ( -- )  finalize CRC (XOR-out)
w_crc_final:
    ldi r0, 1
    ldi64 r11, 0xFFFF_FF00_0000_07E0    ; CRC_CTRL = 1 (finalize)
    str r11, r0
    ret.l

; =====================================================================
;  Memory BIST — CSR 0x60..0x63
; =====================================================================

; BIST-FULL ( -- )  start full memory BIST (March C- + checkerboard + addr-as-data)
w_bist_full:
    ldi r0, 1
    csrw 0x60, r0
    ret.l

; BIST-QUICK ( -- )  start quick memory BIST (March C- only)
w_bist_quick:
    ldi r0, 2
    csrw 0x60, r0
    ret.l

; BIST-STATUS ( -- n )  read BIST status: 0=idle 1=running 2=pass 3=fail
w_bist_status:
    csrr r0, 0x61
    subi r14, 8
    str r14, r0
    ret.l

; BIST-FAIL-ADDR ( -- n )  read first failing address
w_bist_fail_addr:
    csrr r0, 0x62
    subi r14, 8
    str r14, r0
    ret.l

; BIST-FAIL-DATA ( -- n )  read expected/actual data (packed)
w_bist_fail_data:
    csrr r0, 0x63
    subi r14, 8
    str r14, r0
    ret.l

; =====================================================================
;  Tile Datapath Self-Test — CSR 0x64..0x65
; =====================================================================

; TILE-TEST ( -- )  start tile datapath self-test
w_tile_test:
    ldi r0, 1
    csrw 0x64, r0
    ret.l

; TILE-TEST@ ( -- n )  read self-test status: 0=idle 2=pass 3=fail
w_tile_test_fetch:
    csrr r0, 0x64
    subi r14, 8
    str r14, r0
    ret.l

; TILE-DETAIL@ ( -- n )  read failed sub-test bitmask
w_tile_detail_fetch:
    csrr r0, 0x65
    subi r14, 8
    str r14, r0
    ret.l

; CORE-STATUS ( core -- n )  read worker XT slot for core (0 = idle)
w_core_status:
    ldn r0, r14                         ; core ID
    ldi r1, 3
    shl r0, r1                          ; core_id * 8
    ldi64 r11, worker_xt_table
    add r11, r0
    ldn r0, r11                         ; worker_xt[core_id]
    str r14, r0                         ; replace TOS
    ret.l

; =====================================================================
;  Dictionary
; =====================================================================
;  Each entry:
;    [link : 8 bytes]  pointer to previous entry (0 = end)
;    [flags+len : 1]   bit7=IMMEDIATE, bits 6:0 = name length
;    [name : N bytes]
;    [pad : 0-1 bytes] to 2-byte align
;    [trampoline code]  ldi64 r11,<impl>; call.l r11; ret.l  (13 bytes)
;
;  Built from first (link=0) to last.  "latest_entry" labels the final one.

; === DUP ===
d_dup:
    .dq 0
    .db 3
    .ascii "DUP"
    ldi64 r11, w_dup
    call.l r11
    ret.l

; === DROP ===
d_drop:
    .dq d_dup
    .db 4
    .ascii "DROP"
    ldi64 r11, w_drop
    call.l r11
    ret.l

; === SWAP ===
d_swap:
    .dq d_drop
    .db 4
    .ascii "SWAP"
    ldi64 r11, w_swap
    call.l r11
    ret.l

; === OVER ===
d_over:
    .dq d_swap
    .db 4
    .ascii "OVER"
    ldi64 r11, w_over
    call.l r11
    ret.l

; === ROT ===
d_rot:
    .dq d_over
    .db 3
    .ascii "ROT"
    ldi64 r11, w_rot
    call.l r11
    ret.l

; === NIP ===
d_nip:
    .dq d_rot
    .db 3
    .ascii "NIP"
    ldi64 r11, w_nip
    call.l r11
    ret.l

; === TUCK ===
d_tuck:
    .dq d_nip
    .db 4
    .ascii "TUCK"
    ldi64 r11, w_tuck
    call.l r11
    ret.l

; === 2DUP ===
d_2dup:
    .dq d_tuck
    .db 4
    .ascii "2DUP"
    ldi64 r11, w_2dup
    call.l r11
    ret.l

; === 2DROP ===
d_2drop:
    .dq d_2dup
    .db 5
    .ascii "2DROP"
    ldi64 r11, w_2drop
    call.l r11
    ret.l

; === DEPTH ===
d_depth:
    .dq d_2drop
    .db 5
    .ascii "DEPTH"
    ldi64 r11, w_depth
    call.l r11
    ret.l

; === PICK ===
d_pick:
    .dq d_depth
    .db 4
    .ascii "PICK"
    ldi64 r11, w_pick
    call.l r11
    ret.l

; === + ===
d_plus:
    .dq d_pick
    .db 1
    .ascii "+"
    ldi64 r11, w_plus
    call.l r11
    ret.l

; === - ===
d_minus:
    .dq d_plus
    .db 1
    .ascii "-"
    ldi64 r11, w_minus
    call.l r11
    ret.l

; === * ===
d_star:
    .dq d_minus
    .db 1
    .ascii "*"
    ldi64 r11, w_star
    call.l r11
    ret.l

; === / ===
d_slash:
    .dq d_star
    .db 1
    .ascii "/"
    ldi64 r11, w_slash
    call.l r11
    ret.l

; === MOD ===
d_mod:
    .dq d_slash
    .db 3
    .ascii "MOD"
    ldi64 r11, w_mod
    call.l r11
    ret.l

; === /MOD ===
d_slashmod:
    .dq d_mod
    .db 4
    .ascii "/MOD"
    ldi64 r11, w_slashmod
    call.l r11
    ret.l

; === NEGATE ===
d_negate:
    .dq d_slashmod
    .db 6
    .ascii "NEGATE"
    ldi64 r11, w_negate
    call.l r11
    ret.l

; === ABS ===
d_abs:
    .dq d_negate
    .db 3
    .ascii "ABS"
    ldi64 r11, w_abs
    call.l r11
    ret.l

; === 1+ ===
d_1plus:
    .dq d_abs
    .db 2
    .ascii "1+"
    ldi64 r11, w_1plus
    call.l r11
    ret.l

; === 1- ===
d_1minus:
    .dq d_1plus
    .db 2
    .ascii "1-"
    ldi64 r11, w_1minus
    call.l r11
    ret.l

; === AND ===
d_and:
    .dq d_1minus
    .db 3
    .ascii "AND"
    ldi64 r11, w_and
    call.l r11
    ret.l

; === OR ===
d_or:
    .dq d_and
    .db 2
    .ascii "OR"
    ldi64 r11, w_or
    call.l r11
    ret.l

; === XOR ===
d_xor:
    .dq d_or
    .db 3
    .ascii "XOR"
    ldi64 r11, w_xor
    call.l r11
    ret.l

; === INVERT ===
d_invert:
    .dq d_xor
    .db 6
    .ascii "INVERT"
    ldi64 r11, w_invert
    call.l r11
    ret.l

; === LSHIFT ===
d_lshift:
    .dq d_invert
    .db 6
    .ascii "LSHIFT"
    ldi64 r11, w_lshift
    call.l r11
    ret.l

; === RSHIFT ===
d_rshift:
    .dq d_lshift
    .db 6
    .ascii "RSHIFT"
    ldi64 r11, w_rshift
    call.l r11
    ret.l

; === = ===
d_equal:
    .dq d_rshift
    .db 1
    .ascii "="
    ldi64 r11, w_equal
    call.l r11
    ret.l

; === < ===
d_less:
    .dq d_equal
    .db 1
    .ascii "<"
    ldi64 r11, w_less
    call.l r11
    ret.l

; === > ===
d_greater:
    .dq d_less
    .db 1
    .ascii ">"
    ldi64 r11, w_greater
    call.l r11
    ret.l

; === 0= ===
d_0eq:
    .dq d_greater
    .db 2
    .ascii "0="
    ldi64 r11, w_0eq
    call.l r11
    ret.l

; === 0< ===
d_0lt:
    .dq d_0eq
    .db 2
    .ascii "0<"
    ldi64 r11, w_0lt
    call.l r11
    ret.l

; === @ ===
d_fetch:
    .dq d_0lt
    .db 1
    .ascii "@"
    ldi64 r11, w_fetch
    call.l r11
    ret.l

; === ! ===
d_store:
    .dq d_fetch
    .db 1
    .ascii "!"
    ldi64 r11, w_store
    call.l r11
    ret.l

; === C@ ===
d_cfetch:
    .dq d_store
    .db 2
    .ascii "C@"
    ldi64 r11, w_cfetch
    call.l r11
    ret.l

; === C! ===
d_cstore:
    .dq d_cfetch
    .db 2
    .ascii "C!"
    ldi64 r11, w_cstore
    call.l r11
    ret.l

; === HERE ===
d_here:
    .dq d_cstore
    .db 4
    .ascii "HERE"
    ldi64 r11, w_here
    call.l r11
    ret.l

; === ALLOT ===
d_allot:
    .dq d_here
    .db 5
    .ascii "ALLOT"
    ldi64 r11, w_allot
    call.l r11
    ret.l

; === , ===
d_comma:
    .dq d_allot
    .db 1
    .ascii ","
    ldi64 r11, w_comma
    call.l r11
    ret.l

; === C, ===
d_ccomma:
    .dq d_comma
    .db 2
    .ascii "C,"
    ldi64 r11, w_ccomma
    call.l r11
    ret.l

; === EMIT ===
d_emit:
    .dq d_ccomma
    .db 4
    .ascii "EMIT"
    ldi64 r11, w_emit
    call.l r11
    ret.l

; === KEY ===
d_key:
    .dq d_emit
    .db 3
    .ascii "KEY"
    ldi64 r11, w_key
    call.l r11
    ret.l

; === KEY? ===
d_key_query:
    .dq d_key
    .db 4
    .ascii "KEY?"
    ldi64 r11, w_key_query
    call.l r11
    ret.l

; === CR ===
d_cr:
    .dq d_key_query
    .db 2
    .ascii "CR"
    ldi64 r11, w_cr
    call.l r11
    ret.l

; === . ===
d_dot:
    .dq d_cr
    .db 1
    .ascii "."
    ldi64 r11, w_dot
    call.l r11
    ret.l

; === U. ===
d_udot:
    .dq d_dot
    .db 2
    .ascii "U."
    ldi64 r11, w_udot
    call.l r11
    ret.l

; === .S ===
d_dotS:
    .dq d_udot
    .db 2
    .ascii ".S"
    ldi64 r11, w_dotS
    call.l r11
    ret.l

; === HEX ===
d_hex:
    .dq d_dotS
    .db 3
    .ascii "HEX"
    ldi64 r11, w_hex
    call.l r11
    ret.l

; === DECIMAL ===
d_decimal:
    .dq d_hex
    .db 7
    .ascii "DECIMAL"
    ldi64 r11, w_decimal
    call.l r11
    ret.l

; === BASE ===
d_base:
    .dq d_decimal
    .db 4
    .ascii "BASE"
    ldi64 r11, w_base_addr
    call.l r11
    ret.l

; === WORDS ===
d_words:
    .dq d_base
    .db 5
    .ascii "WORDS"
    ldi64 r11, w_words
    call.l r11
    ret.l

; === BYE ===
d_bye:
    .dq d_words
    .db 3
    .ascii "BYE"
    ldi64 r11, w_bye
    call.l r11
    ret.l

; === DUMP ===
d_dump:
    .dq d_bye
    .db 4
    .ascii "DUMP"
    ldi64 r11, w_dump
    call.l r11
    ret.l

; === FILL ===
d_fill:
    .dq d_dump
    .db 4
    .ascii "FILL"
    ldi64 r11, w_fill
    call.l r11
    ret.l

; === TI ===
d_ti:
    .dq d_fill
    .db 2
    .ascii "TI"
    ldi64 r11, w_ti
    call.l r11
    ret.l

; === TVIEW ===
d_tview:
    .dq d_ti
    .db 5
    .ascii "TVIEW"
    ldi64 r11, w_tview
    call.l r11
    ret.l

; === TFILL ===
d_tfill:
    .dq d_tview
    .db 5
    .ascii "TFILL"
    ldi64 r11, w_tfill
    call.l r11
    ret.l

; === TSRC0! ===
d_tsrc0:
    .dq d_tfill
    .db 6
    .ascii "TSRC0!"
    ldi64 r11, w_tsrc0
    call.l r11
    ret.l

; === TSRC1! ===
d_tsrc1:
    .dq d_tsrc0
    .db 6
    .ascii "TSRC1!"
    ldi64 r11, w_tsrc1
    call.l r11
    ret.l

; === TDST! ===
d_tdst:
    .dq d_tsrc1
    .db 5
    .ascii "TDST!"
    ldi64 r11, w_tdst
    call.l r11
    ret.l

; === TMODE! ===
d_tmode:
    .dq d_tdst
    .db 6
    .ascii "TMODE!"
    ldi64 r11, w_tmode
    call.l r11
    ret.l

; === TCTRL! ===
d_tctrl:
    .dq d_tmode
    .db 6
    .ascii "TCTRL!"
    ldi64 r11, w_tctrl
    call.l r11
    ret.l

; === TADD ===
d_tadd:
    .dq d_tctrl
    .db 4
    .ascii "TADD"
    ldi64 r11, w_tadd
    call.l r11
    ret.l

; === TSUB ===
d_tsub:
    .dq d_tadd
    .db 4
    .ascii "TSUB"
    ldi64 r11, w_tsub
    call.l r11
    ret.l

; === TAND ===
d_tand:
    .dq d_tsub
    .db 4
    .ascii "TAND"
    ldi64 r11, w_tand
    call.l r11
    ret.l

; === TOR ===
d_tor:
    .dq d_tand
    .db 3
    .ascii "TOR"
    ldi64 r11, w_tor
    call.l r11
    ret.l

; === TXOR ===
d_txor:
    .dq d_tor
    .db 4
    .ascii "TXOR"
    ldi64 r11, w_txor
    call.l r11
    ret.l

; === TMUL ===
d_tmul:
    .dq d_txor
    .db 4
    .ascii "TMUL"
    ldi64 r11, w_tmul
    call.l r11
    ret.l

; === TDOT ===
d_tdot:
    .dq d_tmul
    .db 4
    .ascii "TDOT"
    ldi64 r11, w_tdot
    call.l r11
    ret.l

; === TSUM ===
d_tsum:
    .dq d_tdot
    .db 4
    .ascii "TSUM"
    ldi64 r11, w_tsum
    call.l r11
    ret.l

; === TMIN ===
d_tmin:
    .dq d_tsum
    .db 4
    .ascii "TMIN"
    ldi64 r11, w_tmin
    call.l r11
    ret.l

; === TMAX ===
d_tmax:
    .dq d_tmin
    .db 4
    .ascii "TMAX"
    ldi64 r11, w_tmax
    call.l r11
    ret.l

; === TTRANS ===
d_ttrans:
    .dq d_tmax
    .db 6
    .ascii "TTRANS"
    ldi64 r11, w_ttrans
    call.l r11
    ret.l

; === TZERO ===
d_tzero:
    .dq d_ttrans
    .db 5
    .ascii "TZERO"
    ldi64 r11, w_tzero
    call.l r11
    ret.l

; === CYCLES ===
d_cycles:
    .dq d_tzero
    .db 6
    .ascii "CYCLES"
    ldi64 r11, w_cycles
    call.l r11
    ret.l

; === ACC@ ===
d_acc_fetch:
    .dq d_cycles
    .db 4
    .ascii "ACC@"
    ldi64 r11, w_acc_fetch
    call.l r11
    ret.l

; === ACC1@ ===
d_acc1_fetch:
    .dq d_acc_fetch
    .db 5
    .ascii "ACC1@"
    ldi64 r11, w_acc1_fetch
    call.l r11
    ret.l

; === ACC2@ ===
d_acc2_fetch:
    .dq d_acc1_fetch
    .db 5
    .ascii "ACC2@"
    ldi64 r11, w_acc2_fetch
    call.l r11
    ret.l

; === ACC3@ ===
d_acc3_fetch:
    .dq d_acc2_fetch
    .db 5
    .ascii "ACC3@"
    ldi64 r11, w_acc3_fetch
    call.l r11
    ret.l

; === TPOPCNT ===
d_tpopcnt:
    .dq d_acc3_fetch
    .db 7
    .ascii "TPOPCNT"
    ldi64 r11, w_tpopcnt
    call.l r11
    ret.l

; === TL1 ===
d_tl1:
    .dq d_tpopcnt
    .db 3
    .ascii "TL1"
    ldi64 r11, w_tl1
    call.l r11
    ret.l

; === TEMIN ===
d_temin:
    .dq d_tl1
    .db 5
    .ascii "TEMIN"
    ldi64 r11, w_temin
    call.l r11
    ret.l

; === TEMAX ===
d_temax:
    .dq d_temin
    .db 5
    .ascii "TEMAX"
    ldi64 r11, w_temax
    call.l r11
    ret.l

; === TSUMSQ ===
d_tsumsq:
    .dq d_temax
    .db 6
    .ascii "TSUMSQ"
    ldi64 r11, w_tsumsq
    call.l r11
    ret.l

; === TMINIDX ===
d_tminidx:
    .dq d_tsumsq
    .db 7
    .ascii "TMINIDX"
    ldi64 r11, w_tminidx
    call.l r11
    ret.l

; === TMAXIDX ===
d_tmaxidx:
    .dq d_tminidx
    .db 7
    .ascii "TMAXIDX"
    ldi64 r11, w_tmaxidx
    call.l r11
    ret.l

; === TWMUL ===
d_twmul:
    .dq d_tmaxidx
    .db 5
    .ascii "TWMUL"
    ldi64 r11, w_twmul
    call.l r11
    ret.l

; === TMAC ===
d_tmac:
    .dq d_twmul
    .db 4
    .ascii "TMAC"
    ldi64 r11, w_tmac
    call.l r11
    ret.l

; === TFMA ===
d_tfma:
    .dq d_tmac
    .db 4
    .ascii "TFMA"
    ldi64 r11, w_tfma
    call.l r11
    ret.l

; === TDOTACC ===
d_tdotacc:
    .dq d_tfma
    .db 7
    .ascii "TDOTACC"
    ldi64 r11, w_tdotacc
    call.l r11
    ret.l

; === TABS ===
d_tabs:
    .dq d_tdotacc
    .db 4
    .ascii "TABS"
    ldi64 r11, w_tabs
    call.l r11
    ret.l

; === TMODE@ ===
d_tmode_fetch:
    .dq d_tabs
    .db 6
    .ascii "TMODE@"
    ldi64 r11, w_tmode_fetch
    call.l r11
    ret.l

; === TCTRL@ ===
d_tctrl_fetch:
    .dq d_tmode_fetch
    .db 6
    .ascii "TCTRL@"
    ldi64 r11, w_tctrl_fetch
    call.l r11
    ret.l

; === EXECUTE ===
d_execute:
    .dq d_tctrl_fetch
    .db 7
    .ascii "EXECUTE"
    ldi64 r11, w_execute
    call.l r11
    ret.l

; === ' (tick) ===
d_tick:
    .dq d_execute
    .db 1
    .ascii "'"
    ldi64 r11, w_tick
    call.l r11
    ret.l

; === : (colon) ===
d_colon:
    .dq d_tick
    .db 1
    .ascii ":"
    ldi64 r11, w_colon
    call.l r11
    ret.l

; === ; (semicolon, IMMEDIATE) ===
d_semicolon:
    .dq d_colon
    .db 0x81
    .ascii ";"
    ldi64 r11, w_semicolon
    call.l r11
    ret.l

; === IF (IMMEDIATE) ===
d_if:
    .dq d_semicolon
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii "IF"
    ldi64 r11, w_if
    call.l r11
    ret.l

; === ELSE (IMMEDIATE) ===
d_else:
    .dq d_if
    .db 0x84                  ; IMMEDIATE | len 4
    .ascii "ELSE"
    ldi64 r11, w_else
    call.l r11
    ret.l

; === THEN (IMMEDIATE) ===
d_then:
    .dq d_else
    .db 0x84                  ; IMMEDIATE | len 4
    .ascii "THEN"
    ldi64 r11, w_then
    call.l r11
    ret.l

; === BEGIN (IMMEDIATE) ===
d_begin:
    .dq d_then
    .db 0x85                  ; IMMEDIATE | len 5
    .ascii "BEGIN"
    ldi64 r11, w_begin
    call.l r11
    ret.l

; === UNTIL (IMMEDIATE) ===
d_until:
    .dq d_begin
    .db 0x85                  ; IMMEDIATE | len 5
    .ascii "UNTIL"
    ldi64 r11, w_until
    call.l r11
    ret.l

; === WHILE (IMMEDIATE) ===
d_while:
    .dq d_until
    .db 0x85                  ; IMMEDIATE | len 5
    .ascii "WHILE"
    ldi64 r11, w_while
    call.l r11
    ret.l

; === REPEAT (IMMEDIATE) ===
d_repeat:
    .dq d_while
    .db 0x86                  ; IMMEDIATE | len 6
    .ascii "REPEAT"
    ldi64 r11, w_repeat
    call.l r11
    ret.l

; === DO (IMMEDIATE) ===
d_do:
    .dq d_repeat
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii "DO"
    ldi64 r11, w_do
    call.l r11
    ret.l

; === LOOP (IMMEDIATE) ===
d_loop:
    .dq d_do
    .db 0x84                  ; IMMEDIATE | len 4
    .ascii "LOOP"
    ldi64 r11, w_loop
    call.l r11
    ret.l

; === I ===
d_i:
    .dq d_loop
    .db 1
    .ascii "I"
    ldi64 r11, w_i
    call.l r11
    ret.l

; === VARIABLE ===
d_variable:
    .dq d_i
    .db 8
    .ascii "VARIABLE"
    ldi64 r11, w_variable
    call.l r11
    ret.l

; === CONSTANT ===
d_constant:
    .dq d_variable
    .db 8
    .ascii "CONSTANT"
    ldi64 r11, w_constant
    call.l r11
    ret.l

; === TYPE ===
d_type:
    .dq d_constant
    .db 4
    .ascii "TYPE"
    ldi64 r11, w_type
    call.l r11
    ret.l

; === SPACE ===
d_space:
    .dq d_type
    .db 5
    .ascii "SPACE"
    ldi64 r11, w_space
    call.l r11
    ret.l

; === SPACES ===
d_spaces:
    .dq d_space
    .db 6
    .ascii "SPACES"
    ldi64 r11, w_spaces
    call.l r11
    ret.l

; === ." (IMMEDIATE) ===
d_dotquote:
    .dq d_spaces
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii ".\""
    ldi64 r11, w_dotquote
    call.l r11
    ret.l

; === ACCEPT ===
d_accept:
    .dq d_dotquote
    .db 6
    .ascii "ACCEPT"
    ldi64 r11, w_accept
    call.l r11
    ret.l

; === NET-STATUS ===
d_net_status:
    .dq d_accept
    .db 10
    .ascii "NET-STATUS"
    ldi64 r11, w_net_status
    call.l r11
    ret.l

; === NET-SEND ===
d_net_send:
    .dq d_net_status
    .db 8
    .ascii "NET-SEND"
    ldi64 r11, w_net_send
    call.l r11
    ret.l

; === NET-RECV ===
d_net_recv:
    .dq d_net_send
    .db 8
    .ascii "NET-RECV"
    ldi64 r11, w_net_recv
    call.l r11
    ret.l

; === NET-MAC@ ===
d_net_mac:
    .dq d_net_recv
    .db 8
    .ascii "NET-MAC@"
    ldi64 r11, w_net_mac
    call.l r11
    ret.l

; === DISK@ ===
d_disk_status:
    .dq d_net_mac
    .db 5
    .ascii "DISK@"
    ldi64 r11, w_disk_status
    call.l r11
    ret.l

; === DISK-SEC! ===
d_disk_sec_store:
    .dq d_disk_status
    .db 9
    .ascii "DISK-SEC!"
    ldi64 r11, w_disk_sec_store
    call.l r11
    ret.l

; === DISK-DMA! ===
d_disk_dma_store:
    .dq d_disk_sec_store
    .db 9
    .ascii "DISK-DMA!"
    ldi64 r11, w_disk_dma_store
    call.l r11
    ret.l

; === DISK-N! ===
d_disk_n_store:
    .dq d_disk_dma_store
    .db 7
    .ascii "DISK-N!"
    ldi64 r11, w_disk_n_store
    call.l r11
    ret.l

; === DISK-READ ===
d_disk_read:
    .dq d_disk_n_store
    .db 9
    .ascii "DISK-READ"
    ldi64 r11, w_disk_read
    call.l r11
    ret.l

; === DISK-WRITE ===
d_disk_write:
    .dq d_disk_read
    .db 10
    .ascii "DISK-WRITE"
    ldi64 r11, w_disk_write
    call.l r11
    ret.l

; === TIMER! ===
d_timer_store:
    .dq d_disk_write
    .db 6
    .ascii "TIMER!"
    ldi64 r11, w_timer_store
    call.l r11
    ret.l

; === TIMER-CTRL! ===
d_timer_ctrl_store:
    .dq d_timer_store
    .db 11
    .ascii "TIMER-CTRL!"
    ldi64 r11, w_timer_ctrl_store
    call.l r11
    ret.l

; === TIMER-ACK ===
d_timer_ack:
    .dq d_timer_ctrl_store
    .db 9
    .ascii "TIMER-ACK"
    ldi64 r11, w_timer_ack
    call.l r11
    ret.l

; === EI! ===
d_ei:
    .dq d_timer_ack
    .db 3
    .ascii "EI!"
    ldi64 r11, w_ei
    call.l r11
    ret.l

; === DI! ===
d_di:
    .dq d_ei
    .db 3
    .ascii "DI!"
    ldi64 r11, w_di
    call.l r11
    ret.l

; === ISR! ===
d_isr_store:
    .dq d_di
    .db 4
    .ascii "ISR!"
    ldi64 r11, w_isr_store
    call.l r11
    ret.l

; === \ (backslash comment, IMMEDIATE) ===
d_backslash:
    .dq d_isr_store
    .db 0x81                  ; IMMEDIATE | len 1
    .ascii "\\"
    ldi64 r11, w_backslash
    call.l r11
    ret.l

; === ( (paren comment, IMMEDIATE) ===
d_paren:
    .dq d_backslash
    .db 0x81                  ; IMMEDIATE | len 1
    .ascii "("
    ldi64 r11, w_paren
    call.l r11
    ret.l

; =====================================================================
;  BIOS v0.5 / v1.0 Dictionary Entries
; =====================================================================

; === EXIT (IMMEDIATE) ===
d_exit:
    .dq d_paren
    .db 0x84                  ; IMMEDIATE | len 4
    .ascii "EXIT"
    ldi64 r11, w_exit
    call.l r11
    ret.l

; === >R (IMMEDIATE) ===
d_to_r:
    .dq d_exit
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii ">R"
    ldi64 r11, w_to_r
    call.l r11
    ret.l

; === R> (IMMEDIATE) ===
d_r_from:
    .dq d_to_r
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii "R>"
    ldi64 r11, w_r_from
    call.l r11
    ret.l

; === R@ (IMMEDIATE) ===
d_r_fetch:
    .dq d_r_from
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii "R@"
    ldi64 r11, w_r_fetch
    call.l r11
    ret.l

; === J ===
d_j:
    .dq d_r_fetch
    .db 1
    .ascii "J"
    ldi64 r11, w_j
    call.l r11
    ret.l

; === UNLOOP (IMMEDIATE) ===
d_unloop:
    .dq d_j
    .db 0x86                  ; IMMEDIATE | len 6
    .ascii "UNLOOP"
    ldi64 r11, w_unloop
    call.l r11
    ret.l

; === +LOOP (IMMEDIATE) ===
d_plus_loop:
    .dq d_unloop
    .db 0x85                  ; IMMEDIATE | len 5
    .ascii "+LOOP"
    ldi64 r11, w_plus_loop
    call.l r11
    ret.l

; === AGAIN (IMMEDIATE) ===
d_again:
    .dq d_plus_loop
    .db 0x85                  ; IMMEDIATE | len 5
    .ascii "AGAIN"
    ldi64 r11, w_again
    call.l r11
    ret.l

; === STATE ===
d_state:
    .dq d_again
    .db 5
    .ascii "STATE"
    ldi64 r11, w_state
    call.l r11
    ret.l

; === [ (IMMEDIATE) ===
d_left_bracket:
    .dq d_state
    .db 0x81                  ; IMMEDIATE | len 1
    .ascii "["
    ldi64 r11, w_left_bracket
    call.l r11
    ret.l

; === ] ===
d_right_bracket:
    .dq d_left_bracket
    .db 1
    .ascii "]"
    ldi64 r11, w_right_bracket
    call.l r11
    ret.l

; === LITERAL (IMMEDIATE) ===
d_literal:
    .dq d_right_bracket
    .db 0x87                  ; IMMEDIATE | len 7
    .ascii "LITERAL"
    ldi64 r11, w_literal
    call.l r11
    ret.l

; === IMMEDIATE ===
d_immediate:
    .dq d_literal
    .db 9
    .ascii "IMMEDIATE"
    ldi64 r11, w_immediate
    call.l r11
    ret.l

; === CREATE ===
d_create:
    .dq d_immediate
    .db 6
    .ascii "CREATE"
    ldi64 r11, w_create
    call.l r11
    ret.l

; === S" (IMMEDIATE) ===
d_squote:
    .dq d_create
    .db 0x82                  ; IMMEDIATE | len 2
    .ascii "S\""
    ldi64 r11, w_squote
    call.l r11
    ret.l

; === 0> ===
d_zero_gt:
    .dq d_squote
    .db 2
    .ascii "0>"
    ldi64 r11, w_zero_gt
    call.l r11
    ret.l

; === <> ===
d_not_equal:
    .dq d_zero_gt
    .db 2
    .ascii "<>"
    ldi64 r11, w_not_equal
    call.l r11
    ret.l

; === 0<> ===
d_zero_ne:
    .dq d_not_equal
    .db 3
    .ascii "0<>"
    ldi64 r11, w_zero_ne
    call.l r11
    ret.l

; === ?DUP ===
d_qdup:
    .dq d_zero_ne
    .db 4
    .ascii "?DUP"
    ldi64 r11, w_qdup
    call.l r11
    ret.l

; === MIN ===
d_min:
    .dq d_qdup
    .db 3
    .ascii "MIN"
    ldi64 r11, w_min
    call.l r11
    ret.l

; === MAX ===
d_max:
    .dq d_min
    .db 3
    .ascii "MAX"
    ldi64 r11, w_max
    call.l r11
    ret.l

; === CELLS ===
d_cells:
    .dq d_max
    .db 5
    .ascii "CELLS"
    ldi64 r11, w_cells
    call.l r11
    ret.l

; === CELL+ ===
d_cell_plus:
    .dq d_cells
    .db 5
    .ascii "CELL+"
    ldi64 r11, w_cell_plus
    call.l r11
    ret.l

; === +! ===
d_plus_store:
    .dq d_cell_plus
    .db 2
    .ascii "+!"
    ldi64 r11, w_plus_store
    call.l r11
    ret.l

; === 2* ===
d_two_star:
    .dq d_plus_store
    .db 2
    .ascii "2*"
    ldi64 r11, w_two_star
    call.l r11
    ret.l

; === CMOVE ===
d_cmove:
    .dq d_two_star
    .db 5
    .ascii "CMOVE"
    ldi64 r11, w_cmove
    call.l r11
    ret.l

; === -ROT ===
d_neg_rot:
    .dq d_cmove
    .db 4
    .ascii "-ROT"
    ldi64 r11, w_neg_rot
    call.l r11
    ret.l

; === BL ===
d_bl:
    .dq d_neg_rot
    .db 2
    .ascii "BL"
    ldi64 r11, w_bl
    call.l r11
    ret.l

; === TRUE ===
d_true:
    .dq d_bl
    .db 4
    .ascii "TRUE"
    ldi64 r11, w_true
    call.l r11
    ret.l

; === FALSE ===
d_false:
    .dq d_true
    .db 5
    .ascii "FALSE"
    ldi64 r11, w_false
    call.l r11
    ret.l

; === WORD ===
d_word:
    .dq d_false
    .db 4
    .ascii "WORD"
    ldi64 r11, w_word_forth
    call.l r11
    ret.l

; === LATEST ===
d_latest:
    .dq d_word
    .db 6
    .ascii "LATEST"
    ldi64 r1, var_latest
    ldn r1, r1
    subi r14, 8
    str r14, r1
    ret.l

; === 2OVER ===
d_2over:
    .dq d_latest
    .db 5
    .ascii "2OVER"
    ldi64 r11, w_2over
    call.l r11
    ret.l

; === 2SWAP ===
d_2swap:
    .dq d_2over
    .db 5
    .ascii "2SWAP"
    ldi64 r11, w_2swap
    call.l r11
    ret.l

; === 2ROT ===
d_2rot:
    .dq d_2swap
    .db 4
    .ascii "2ROT"
    ldi64 r11, w_2rot
    call.l r11
    ret.l

; === >= ===
d_gte:
    .dq d_2rot
    .db 2
    .ascii ">="
    ldi64 r11, w_gte
    call.l r11
    ret.l

; === <= ===
d_lte:
    .dq d_gte
    .db 2
    .ascii "<="
    ldi64 r11, w_lte
    call.l r11
    ret.l

; === U< ===
d_u_lt:
    .dq d_lte
    .db 2
    .ascii "U<"
    ldi64 r11, w_u_lt
    call.l r11
    ret.l

; === U> ===
d_u_gt:
    .dq d_u_lt
    .db 2
    .ascii "U>"
    ldi64 r11, w_u_gt
    call.l r11
    ret.l

; === OFF ===
d_off:
    .dq d_u_gt
    .db 3
    .ascii "OFF"
    ldi64 r11, w_off
    call.l r11
    ret.l

; === W@ ===
d_wfetch:
    .dq d_off
    .db 2
    .ascii "W@"
    ldi64 r11, w_wfetch
    call.l r11
    ret.l

; === W! ===
d_wstore:
    .dq d_wfetch
    .db 2
    .ascii "W!"
    ldi64 r11, w_wstore
    call.l r11
    ret.l

; === L@ ===
d_lfetch:
    .dq d_wstore
    .db 2
    .ascii "L@"
    ldi64 r11, w_lfetch
    call.l r11
    ret.l

; === L! ===
d_lstore:
    .dq d_lfetch
    .db 2
    .ascii "L!"
    ldi64 r11, w_lstore
    call.l r11
    ret.l

; === .ZSTR ===
d_zstr:
    .dq d_lstore
    .db 5
    .ascii ".ZSTR"
    ldi64 r11, w_zstr
    call.l r11
    ret.l

; === UCHAR ===
d_uchar:
    .dq d_zstr
    .db 5
    .ascii "UCHAR"
    ldi64 r11, w_uchar
    call.l r11
    ret.l

; === TALIGN ===
d_talign:
    .dq d_uchar
    .db 6
    .ascii "TALIGN"
    ldi64 r11, w_talign
    call.l r11
    ret.l

; === ABORT ===
d_abort:
    .dq d_talign
    .db 5
    .ascii "ABORT"
    ldi64 r11, w_abort
    call.l r11
    ret.l

; === ABORT" === (IMMEDIATE)
d_abort_quote:
    .dq d_abort
    .db 0x86
    .ascii "ABORT\""
    ldi64 r11, w_abort_quote
    call.l r11
    ret.l

; === LEAVE === (IMMEDIATE)
d_leave:
    .dq d_abort_quote
    .db 0x85
    .ascii "LEAVE"
    ldi64 r11, w_leave
    call.l r11
    ret.l

; === 2/ ===
d_two_slash:
    .dq d_leave
    .db 2
    .ascii "2/"
    ldi64 r11, w_two_slash
    call.l r11
    ret.l

; === COUNT ===
d_count:
    .dq d_two_slash
    .db 5
    .ascii "COUNT"
    ldi64 r11, w_count
    call.l r11
    ret.l

; === MOVE ===
d_move:
    .dq d_count
    .db 4
    .ascii "MOVE"
    ldi64 r11, w_move
    call.l r11
    ret.l

; === WITHIN ===
d_within:
    .dq d_move
    .db 6
    .ascii "WITHIN"
    ldi64 r11, w_within
    call.l r11
    ret.l

; === FIND ===
d_find:
    .dq d_within
    .db 4
    .ascii "FIND"
    ldi64 r11, w_find_forth
    call.l r11
    ret.l

; === SOURCE ===
d_source:
    .dq d_find
    .db 6
    .ascii "SOURCE"
    ldi64 r11, w_source
    call.l r11
    ret.l

; === >IN ===
d_to_in:
    .dq d_source
    .db 3
    .ascii ">IN"
    ldi64 r11, w_to_in
    call.l r11
    ret.l

; === EVALUATE ===
d_evaluate:
    .dq d_to_in
    .db 8
    .ascii "EVALUATE"
    ldi64 r11, w_evaluate
    call.l r11
    ret.l

; === COMPARE ===
d_compare:
    .dq d_evaluate
    .db 7
    .ascii "COMPARE"
    ldi64 r11, w_compare
    call.l r11
    ret.l

; === CHAR ===
d_char:
    .dq d_compare
    .db 4
    .ascii "CHAR"
    ldi64 r11, w_char
    call.l r11
    ret.l

; === [CHAR] === (IMMEDIATE)
d_bracket_char:
    .dq d_char
    .db 0x86
    .ascii "[CHAR]"
    ldi64 r11, w_bracket_char
    call.l r11
    ret.l

; === RECURSE === (IMMEDIATE)
d_recurse:
    .dq d_bracket_char
    .db 0x87
    .ascii "RECURSE"
    ldi64 r11, w_recurse
    call.l r11
    ret.l

; === VALUE ===
d_value:
    .dq d_recurse
    .db 5
    .ascii "VALUE"
    ldi64 r11, w_value
    call.l r11
    ret.l

; === TO === (IMMEDIATE)
d_to:
    .dq d_value
    .db 0x82
    .ascii "TO"
    ldi64 r11, w_to
    call.l r11
    ret.l

; === POSTPONE === (IMMEDIATE)
d_postpone:
    .dq d_to
    .db 0x88
    .ascii "POSTPONE"
    ldi64 r11, w_postpone
    call.l r11
    ret.l

; === 2>R === (IMMEDIATE)
d_2to_r:
    .dq d_postpone
    .db 0x83
    .ascii "2>R"
    ldi64 r11, w_2to_r
    call.l r11
    ret.l

; === 2R> === (IMMEDIATE)
d_2r_from:
    .dq d_2to_r
    .db 0x83
    .ascii "2R>"
    ldi64 r11, w_2r_from
    call.l r11
    ret.l

; === 2R@ === (IMMEDIATE)
d_2r_fetch:
    .dq d_2r_from
    .db 0x83
    .ascii "2R@"
    ldi64 r11, w_2r_fetch
    call.l r11
    ret.l

; === DOES> === (IMMEDIATE)
d_does:
    .dq d_2r_fetch
    .db 0x85
    .ascii "DOES>"
    ldi64 r11, w_does
    call.l r11
    ret.l

; === >NUMBER ===
d_to_number:
    .dq d_does
    .db 7
    .ascii ">NUMBER"
    ldi64 r11, w_to_number
    call.l r11
    ret.l

; === QUIT ===
d_quit:
    .dq d_to_number
    .db 4
    .ascii "QUIT"
    ldi64 r11, w_quit
    call.l r11
    ret.l

; === FSLOAD ===
d_fsload:
    .dq d_quit
    .db 6
    .ascii "FSLOAD"
    ldi64 r11, w_fsload
    call.l r11
    ret.l

; === COREID ===
d_coreid:
    .dq d_fsload
    .db 6
    .ascii "COREID"
    ldi64 r11, w_coreid
    call.l r11
    ret.l

; === NCORES ===
d_ncores:
    .dq d_coreid
    .db 6
    .ascii "NCORES"
    ldi64 r11, w_ncores
    call.l r11
    ret.l

; === IPI-SEND ===
d_ipi_send:
    .dq d_ncores
    .db 8
    .ascii "IPI-SEND"
    ldi64 r11, w_ipi_send
    call.l r11
    ret.l

; === IPI-STATUS ===
d_ipi_status:
    .dq d_ipi_send
    .db 10
    .ascii "IPI-STATUS"
    ldi64 r11, w_ipi_status
    call.l r11
    ret.l

; === IPI-ACK ===
d_ipi_ack:
    .dq d_ipi_status
    .db 7
    .ascii "IPI-ACK"
    ldi64 r11, w_ipi_ack
    call.l r11
    ret.l

; === MBOX! ===
d_mbox_store:
    .dq d_ipi_ack
    .db 5
    .ascii "MBOX!"
    ldi64 r11, w_mbox_store
    call.l r11
    ret.l

; === MBOX@ ===
d_mbox_fetch:
    .dq d_mbox_store
    .db 5
    .ascii "MBOX@"
    ldi64 r11, w_mbox_fetch
    call.l r11
    ret.l

; === SPIN@ ===
d_spin_fetch:
    .dq d_mbox_fetch
    .db 5
    .ascii "SPIN@"
    ldi64 r11, w_spin_fetch
    call.l r11
    ret.l

; === SPIN! ===
d_spin_release:
    .dq d_spin_fetch
    .db 5
    .ascii "SPIN!"
    ldi64 r11, w_spin_release
    call.l r11
    ret.l

; === WAKE-CORE ===
d_wake_core:
    .dq d_spin_release
    .db 9
    .ascii "WAKE-CORE"
    ldi64 r11, w_wake_core
    call.l r11
    ret.l

; === CORE-STATUS ===
d_core_status:
    .dq d_wake_core
    .db 11
    .ascii "CORE-STATUS"
    ldi64 r11, w_core_status
    call.l r11
    ret.l

; === PERF-CYCLES ===
d_perf_cycles:
    .dq d_core_status
    .db 11
    .ascii "PERF-CYCLES"
    ldi64 r11, w_perf_cycles
    call.l r11
    ret.l

; === PERF-STALLS ===
d_perf_stalls:
    .dq d_perf_cycles
    .db 11
    .ascii "PERF-STALLS"
    ldi64 r11, w_perf_stalls
    call.l r11
    ret.l

; === PERF-TILEOPS ===
d_perf_tileops:
    .dq d_perf_stalls
    .db 12
    .ascii "PERF-TILEOPS"
    ldi64 r11, w_perf_tileops
    call.l r11
    ret.l

; === PERF-EXTMEM ===
d_perf_extmem:
    .dq d_perf_tileops
    .db 11
    .ascii "PERF-EXTMEM"
    ldi64 r11, w_perf_extmem
    call.l r11
    ret.l

; === PERF-RESET ===
d_perf_reset:
    .dq d_perf_extmem
    .db 10
    .ascii "PERF-RESET"
    ldi64 r11, w_perf_reset
    call.l r11
    ret.l

; === CRC-POLY! ===
d_crc_poly_store:
    .dq d_perf_reset
    .db 9
    .ascii "CRC-POLY!"
    ldi64 r11, w_crc_poly_store
    call.l r11
    ret.l

; === CRC-INIT! ===
d_crc_init_store:
    .dq d_crc_poly_store
    .db 9
    .ascii "CRC-INIT!"
    ldi64 r11, w_crc_init_store
    call.l r11
    ret.l

; === CRC-FEED ===
d_crc_feed:
    .dq d_crc_init_store
    .db 8
    .ascii "CRC-FEED"
    ldi64 r11, w_crc_feed
    call.l r11
    ret.l

; === CRC@ ===
d_crc_fetch:
    .dq d_crc_feed
    .db 4
    .ascii "CRC@"
    ldi64 r11, w_crc_fetch
    call.l r11
    ret.l

; === CRC-RESET ===
d_crc_reset:
    .dq d_crc_fetch
    .db 9
    .ascii "CRC-RESET"
    ldi64 r11, w_crc_reset
    call.l r11
    ret.l

; === CRC-FINAL ===
d_crc_final:
    .dq d_crc_reset
    .db 9
    .ascii "CRC-FINAL"
    ldi64 r11, w_crc_final
    call.l r11
    ret.l

; === BIST-FULL ===
d_bist_full:
    .dq d_crc_final
    .db 9
    .ascii "BIST-FULL"
    ldi64 r11, w_bist_full
    call.l r11
    ret.l

; === BIST-QUICK ===
d_bist_quick:
    .dq d_bist_full
    .db 10
    .ascii "BIST-QUICK"
    ldi64 r11, w_bist_quick
    call.l r11
    ret.l

; === BIST-STATUS ===
d_bist_status:
    .dq d_bist_quick
    .db 11
    .ascii "BIST-STATUS"
    ldi64 r11, w_bist_status
    call.l r11
    ret.l

; === BIST-FAIL-ADDR ===
d_bist_fail_addr:
    .dq d_bist_status
    .db 14
    .ascii "BIST-FAIL-ADDR"
    ldi64 r11, w_bist_fail_addr
    call.l r11
    ret.l

; === BIST-FAIL-DATA ===
d_bist_fail_data:
    .dq d_bist_fail_addr
    .db 14
    .ascii "BIST-FAIL-DATA"
    ldi64 r11, w_bist_fail_data
    call.l r11
    ret.l

; === TILE-TEST ===
d_tile_test:
    .dq d_bist_fail_data
    .db 9
    .ascii "TILE-TEST"
    ldi64 r11, w_tile_test
    call.l r11
    ret.l

; === TILE-TEST@ ===
d_tile_test_fetch:
    .dq d_tile_test
    .db 10
    .ascii "TILE-TEST@"
    ldi64 r11, w_tile_test_fetch
    call.l r11
    ret.l

; === TILE-DETAIL@ ===
d_tile_detail_fetch:
    .dq d_tile_test_fetch
    .db 12
    .ascii "TILE-DETAIL@"
    ldi64 r11, w_tile_detail_fetch
    call.l r11
    ret.l

; === TSTRIDE-R! ( n -- ) ===
d_tstride_r:
    .dq d_tile_detail_fetch
    .db 10
    .ascii "TSTRIDE-R!"
    ldn r0, r14
    addi r14, 8
    csrw 0x40, r0
    ret.l

; === TSTRIDE-R@ ( -- n ) ===
d_tstride_r_fetch:
    .dq d_tstride_r
    .db 10
    .ascii "TSTRIDE-R@"
    csrr r0, 0x40
    subi r14, 8
    str r0, r14
    ret.l

; === TTILE-H! ( n -- ) ===
d_ttile_h:
    .dq d_tstride_r_fetch
    .db 8
    .ascii "TTILE-H!"
    ldn r0, r14
    addi r14, 8
    csrw 0x42, r0
    ret.l

; === TTILE-W! ( n -- ) ===
d_ttile_w:
    .dq d_ttile_h
    .db 8
    .ascii "TTILE-W!"
    ldn r0, r14
    addi r14, 8
    csrw 0x43, r0
    ret.l

; === TLOAD2D ( -- ) ===
d_tload2d:
    .dq d_ttile_w
    .db 7
    .ascii "TLOAD2D"
    t.load2d
    ret.l

; === TSTORE2D ( -- ) ===
latest_entry:
d_tstore2d:
    .dq d_tload2d
    .db 8
    .ascii "TSTORE2D"
    t.store2d
    ret.l

; =====================================================================
;  Forth Variables
; =====================================================================
var_state:
    .dq 0
var_base:
    .dq 10
var_here:
    .dq 0
var_latest:
    .dq 0
var_to_in:
    .dq 0
var_tib_len:
    .dq 0
var_word_addr:
    .dq 0
var_word_len:
    .dq 0

; FSLOAD line tracking — used for error context
var_fsload_line:
    .dq 0

; EVALUATE nesting depth — prevents unbounded RSP growth
var_eval_depth:
    .dq 0

; LEAVE tracking — used by DO/LEAVE/LOOP at compile time
var_leave_count:
    .dq 0
var_leave_fixups:
    .dq 0, 0, 0, 0, 0, 0, 0, 0   ; up to 8 LEAVEs per loop level

; =====================================================================
;  String Constants
; =====================================================================
str_banner:
    .asciiz "\nMegapad-64 Forth BIOS v1.0\nRAM: "
str_bytes_ram:
    .asciiz " bytes\n"
str_ok:
    .asciiz " ok\n"
str_undefined:
    .asciiz " ? (not found)\n"
str_bye:
    .asciiz "Bye!\n"
str_stk_hdr:
    .asciiz "> "

str_ti_mode:
    .asciiz "TMODE="
str_ti_ctrl:
    .asciiz " TCTRL="
str_ti_src0:
    .asciiz "SRC0="
str_ti_src1:
    .asciiz " SRC1="
str_ti_dst:
    .asciiz " DST="
str_ti_acc:
    .asciiz "ACC: "

str_no_name:
    .asciiz " name expected\n"

str_line_prefix:
    .asciiz "  line "
str_colon_space:
    .asciiz ": "
str_stack_underflow:
    .asciiz "Stack underflow\n"
str_eval_depth:
    .asciiz "EVALUATE depth limit exceeded\n"
str_dict_full:
    .asciiz "Dictionary full\n"

str_fsload_no_disk:
    .asciiz "FSLOAD: no disk\n"
str_fsload_err:
    .asciiz "FSLOAD: not found\n"

str_busfault:
    .asciiz "\n*** BUS FAULT @ "

; =====================================================================
;  IVT (Interrupt Vector Table)
; =====================================================================
ivt_table:
    .dq 0                            ; [0] RESET
    .dq 0                            ; [1] NMI
    .dq 0                            ; [2] ILLEGAL OP
    .dq 0                            ; [3] ALIGN FAULT
    .dq 0                            ; [4] DIV ZERO
    .dq bus_fault_handler            ; [5] BUS FAULT
    .dq 0                            ; [6] SW TRAP
    .dq 0                            ; [7] TIMER — installed by KDOS via ISR!
    .dq ipi_handler                  ; [8] IPI — inter-processor interrupt

; =====================================================================
;  TIB (Text Input Buffer) — 256 bytes
; =====================================================================
tib_buffer:
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0

; --- User dictionary free space starts here ---
dict_free: