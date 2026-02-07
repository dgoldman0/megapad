; =====================================================================
;  Megapad-64 BIOS  v0.3 — Forth MVP REPL
; =====================================================================
;
;  A subroutine-threaded Forth interpreter on the Megapad-64.
;  Provides an interactive REPL with ~60 built-in words for testing
;  the CPU, memory, and tile engine.
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

    ; Found — compute code field address and execute
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

    ; Push number
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
w_slash:
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    div r0, r1
    str r14, r0
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

; === CYCLES (last built-in) ===
latest_entry:
d_cycles:
    .dq d_tzero
    .db 6
    .ascii "CYCLES"
    ldi64 r11, w_cycles
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
    .asciiz "\nMegapad-64 Forth BIOS v0.3\nRAM: "
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