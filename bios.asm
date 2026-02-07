; =====================================================================
;  Megapad-64 BIOS  v0.4 — Forth with Compilation
; =====================================================================
;
;  A subroutine-threaded Forth interpreter on the Megapad-64.
;  v0.4 adds colon definitions, control flow, VARIABLE, CONSTANT,
;  string words, NIC words, and compilation infrastructure.
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
; =====================================================================

; === Entry point (address 0x0000) ===
boot:
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
    csrw 0x20, r0

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
    ; Abort rest of line
    lbr quit_loop

interp_line_done:
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

; ." (IMMEDIATE) — compile inline string and print call
;   At compile time, reads chars until " and embeds them.
;   Compiles: ldi64 r10, <string>; ldi64 r11, print_str; call.l r11; lbr <past>
;   <string data> <null>
;   <past:> ...
w_dotquote:
    ; We compile a call to an inline string printer.
    ; Strategy: compile a forward branch over the string data,
    ; then compile a call to print_str that references the string.
    ; Actually simpler: compile a call to our helper dotquote_runtime
    ; which pops the return address (= string addr), prints until null,
    ; then jumps past the string.
    ;
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
;  Bus Fault Handler
; =====================================================================
bus_fault_handler:
    ldi64 r8, 0xFFFF_FF00_0000_0000
    ldi64 r4, emit_char
    ldi64 r5, key_char
    ldi64 r6, print_hex_byte
    csrr r0, 0x22
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

; === CR ===
d_cr:
    .dq d_key
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

; === TABS ===
d_tabs:
    .dq d_temax
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

; === \ (backslash comment, IMMEDIATE) ===
d_backslash:
    .dq d_disk_write
    .db 0x81                  ; IMMEDIATE | len 1
    .ascii "\\"
    ldi64 r11, w_backslash
    call.l r11
    ret.l

; === ( (paren comment, IMMEDIATE) ===
latest_entry:
d_paren:
    .dq d_backslash
    .db 0x81                  ; IMMEDIATE | len 1
    .ascii "("
    ldi64 r11, w_paren
    call.l r11
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

; =====================================================================
;  String Constants
; =====================================================================
str_banner:
    .asciiz "\nMegapad-64 Forth BIOS v0.4\nRAM: "
str_bytes_ram:
    .asciiz " bytes\n"
str_ok:
    .asciiz " ok\n"
str_undefined:
    .asciiz " ?\n"
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

str_busfault:
    .asciiz "\n*** BUS FAULT @ "

; =====================================================================
;  IVT (Interrupt Vector Table)
; =====================================================================
ivt_table:
    .dq 0
    .dq 0
    .dq 0
    .dq 0
    .dq 0
    .dq bus_fault_handler
    .dq 0

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