; =====================================================================
;  Megapad-64 BIOS  v0.1
;  =====================================================================
;  Bootstraps the system from a cold reset (entry at address 0):
;    1. Set up stack pointer (R15) at top of RAM
;    2. Set up data pointer R2 (XSEL)
;    3. Initialize UART I/O base address in R8
;    4. Print boot banner via UART
;    5. Probe storage, print status
;    6. Enter a simple monitor shell loop:
;       - Read a line of input via UART
;       - Parse single-character commands:
;           'h' — help
;           'r' — dump registers (via TRAP → returns to monitor)
;           'd' — hex dump 16 bytes from address in R9
;           's' — set address (R9) from hex input
;           'g' — go: jump to address in R9
;           'q' — halt
;       - Unknown input → print '?'
;
;  Register conventions:
;    R3  = PC (PSEL=3)
;    R2  = data pointer (XSEL=2)
;    R8  = UART base address (MMIO)
;    R9  = user address register (for dump/go)
;    R10 = scratch / string pointer
;    R15 = SP (SPSEL=15)
;
;  UART register offsets (from devices.py):
;    +0x00 = TX_DATA (write)
;    +0x01 = RX_DATA (read)
;    +0x02 = STATUS  (bit 1 = RX_AVAIL)
;
;  MMIO base: 0xFFFF_FF00_0000_0000
;  UART base: MMIO_BASE + 0x0000 = 0xFFFF_FF00_0000_0000
;  SysInfo:   MMIO_BASE + 0x0300 = 0xFFFF_FF00_0000_0300
;  Storage:   MMIO_BASE + 0x0200 = 0xFFFF_FF00_0000_0200
; =====================================================================

; --- Entry point (address 0) ---

    ; Set up stack pointer: boot() sets R2 = ram_size
    mov r15, r2

    ; Set up UART base address in R8
    ldi64 r8, 0xFFFF_FF00_0000_0000

    ; Zero out user address register
    ldi r9, 0

    ; Set up subroutine pointers
    ldi64 r4, print_str
    ldi64 r5, read_char
    ldi64 r6, print_hex_byte

    ; R10 = pointer to banner string
    ldi64 r10, banner
    call.l r4               ; print banner string

    ; Probe storage: read SysInfo offset 0x08 (storage present)
    ldi64 r11, 0xFFFF_FF00_0000_0308  ; SysInfo + 0x08
    ld.b r12, r11           ; R12 = storage present flag
    ldi64 r10, msg_storage
    call.l r4

    ; Print 'Y' or 'N' based on R12
    ldi r1, 0x4E            ; 'N'
    cmpi r12, 0
    breq skip_yes
    ldi r1, 0x59            ; 'Y'
skip_yes:
    st.b r8, r1             ; UART TX ← R1

    ; Print newline
    ldi r1, 0x0A
    st.b r8, r1
    ldi r1, 0x0D
    st.b r8, r1

    ; Print prompt and enter shell loop
    ldi64 r10, msg_ready
    call.l r4

shell_loop:
    ; Print prompt "> "
    ldi r1, 0x3E            ; '>'
    st.b r8, r1
    ldi r1, 0x20            ; ' '
    st.b r8, r1

    ; Read one character
    call.l r5               ; R1 ← char from UART

    ; Echo it back
    st.b r8, r1

    ; Newline
    ldi r7, 0x0A
    st.b r8, r7
    ldi r7, 0x0D
    st.b r8, r7

    ; Dispatch on character
    cmpi r1, 0x68           ; 'h'
    lbreq cmd_help
    cmpi r1, 0x72           ; 'r'
    lbreq cmd_regs
    cmpi r1, 0x64           ; 'd'
    lbreq cmd_dump
    cmpi r1, 0x67           ; 'g'
    lbreq cmd_go
    cmpi r1, 0x71           ; 'q'
    lbreq cmd_quit
    cmpi r1, 0x73           ; 's'
    lbreq cmd_setaddr

    ; Unknown command: print '?'
    ldi r1, 0x3F            ; '?'
    st.b r8, r1
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; --- Command: help ---
cmd_help:
    ldi64 r10, msg_help
    call.l r4
    lbr shell_loop

; --- Command: regs (just print PC and a few regs as hex) ---
cmd_regs:
    ; Print "PC=" then R3 as hex
    ldi64 r10, msg_pc
    call.l r4
    ; Print low byte of PC (R3)
    mov r1, r3              ; R1 = PC value (full 64-bit)
    andi r1, 0xFF           ; mask to low byte
    call.l r6               ; print_hex_byte
    ldi r1, 0x0A
    st.b r8, r1
    ; Print "R9=" (user addr)
    ldi64 r10, msg_r9
    call.l r4
    mov r1, r9
    andi r1, 0xFF           ; mask to low byte
    call.l r6
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; --- Command: dump 16 bytes at R9 ---
cmd_dump:
    ; Print 16 bytes starting at R9
    ldi r12, 16             ; counter
    mov r11, r9             ; current address
dump_loop:
    ld.b r1, r11            ; R1 = byte at [R11]
    call.l r6               ; print as hex
    ldi r1, 0x20            ; space
    st.b r8, r1
    inc r11
    dec r12
    cmpi r12, 0
    brne dump_loop
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; --- Command: set address (read 2 hex digits → R9) ---
cmd_setaddr:
    ; Read 2 hex chars → byte → R9
    call.l r5               ; first hex digit → R1
    ldi r12, 0              ; accumulator
    ; Convert ASCII hex to nibble
    subi r1, 0x30           ; '0' = 0x30
    cmpi r1, 10
    brcc setaddr_d1_ok      ; C=0 means < 10, it's 0-9
    subi r1, 7              ; 'A'(0x41)-0x30=0x11, -7=0xA
setaddr_d1_ok:
    andi r1, 0x0F
    lsli r1, 4
    mov r12, r1

    call.l r5               ; second hex digit → R1
    subi r1, 0x30
    cmpi r1, 10
    brcc setaddr_d2_ok      ; C=0 means < 10
    subi r1, 7
setaddr_d2_ok:
    andi r1, 0x0F
    or r12, r1
    mov r9, r12
    ; Echo the address
    ldi r1, 0x0A
    st.b r8, r1
    lbr shell_loop

; --- Command: go (jump to R9) ---
cmd_go:
    call.l r9               ; CALL.L R9 — will return here via RET.L
    lbr shell_loop

; --- Command: quit ---
cmd_quit:
    ldi64 r10, msg_bye
    call.l r4
    halt

; =====================================================================
;  Subroutines
; =====================================================================

; --- print_str: print null-terminated string at R10 via UART ---
print_str:
    ld.b r1, r10            ; R1 = byte at [R10]
    cmpi r1, 0              ; null terminator?
    breq print_str_done
    st.b r8, r1             ; UART TX ← R1
    inc r10
    br print_str
print_str_done:
    ret.l

; --- read_char: blocking read one byte from UART → R1 ---
read_char:
    ; Poll UART STATUS bit 1 (RX_AVAIL)
    ldi64 r13, 0xFFFF_FF00_0000_0002  ; UART STATUS register
read_char_poll:
    ld.b r1, r13            ; R1 = status
    andi r1, 0x02           ; mask RX_AVAIL
    cmpi r1, 0
    breq read_char_poll     ; spin until data available
    ; Read the byte
    ldi64 r13, 0xFFFF_FF00_0000_0001  ; UART RX_DATA register
    ld.b r1, r13            ; R1 = received byte
    ret.l

; --- print_hex_byte: print R1 low byte as 2 hex chars ---
print_hex_byte:
    ; Save R1
    mov r14, r1
    ; High nibble
    lsri r1, 4
    andi r1, 0x0F
    addi r1, 0x30           ; '0'
    cmpi r1, 0x3A           ; >= ':' (i.e. > '9')?
    brcc phb_hi_ok          ; C=0 means < 0x3A → skip adjustment
    addi r1, 7              ; adjust to 'A'-'F'
phb_hi_ok:
    st.b r8, r1
    ; Low nibble
    mov r1, r14
    andi r1, 0x0F
    addi r1, 0x30
    cmpi r1, 0x3A
    brcc phb_lo_ok          ; C=0 means < 0x3A → skip adjustment
    addi r1, 7
phb_lo_ok:
    st.b r8, r1
    ret.l

; =====================================================================
;  String data
; =====================================================================

banner:
    .db 0x0A
    .db 0x4D, 0x65, 0x67, 0x61, 0x70, 0x61, 0x64  ; "Megapad"
    .db 0x2D, 0x36, 0x34, 0x20                       ; "-64 "
    .db 0x42, 0x49, 0x4F, 0x53, 0x20                 ; "BIOS "
    .db 0x76, 0x30, 0x2E, 0x31                       ; "v0.1"
    .db 0x0A, 0x00

msg_storage:
    .db 0x53, 0x74, 0x6F, 0x72, 0x61, 0x67, 0x65  ; "Storage"
    .db 0x3A, 0x20                                   ; ": "
    .db 0x00

msg_ready:
    .db 0x0A
    .db 0x52, 0x65, 0x61, 0x64, 0x79, 0x2E          ; "Ready."
    .db 0x0A, 0x00

msg_help:
    .db 0x68, 0x3D, 0x68, 0x65, 0x6C, 0x70          ; "h=help"
    .db 0x20
    .db 0x72, 0x3D, 0x72, 0x65, 0x67, 0x73          ; "r=regs"
    .db 0x20
    .db 0x64, 0x3D, 0x64, 0x75, 0x6D, 0x70          ; "d=dump"
    .db 0x20
    .db 0x73, 0x3D, 0x73, 0x65, 0x74, 0x61, 0x64, 0x64, 0x72 ; "s=setaddr"
    .db 0x20
    .db 0x67, 0x3D, 0x67, 0x6F                       ; "g=go"
    .db 0x20
    .db 0x71, 0x3D, 0x71, 0x75, 0x69, 0x74          ; "q=quit"
    .db 0x0A, 0x00

msg_pc:
    .db 0x50, 0x43, 0x3D                              ; "PC="
    .db 0x00

msg_r9:
    .db 0x52, 0x39, 0x3D                              ; "R9="
    .db 0x00

msg_bye:
    .db 0x42, 0x79, 0x65, 0x21, 0x0A                  ; "Bye!\n"
    .db 0x00
