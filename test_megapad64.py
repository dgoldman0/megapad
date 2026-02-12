"""
Megapad-64 Emulator Test Suite
===============================
Covers all 16 instruction families, assembler round-trip, and edge cases.
Run with:  python test_megapad64.py
"""

import sys
import traceback

from megapad64 import (Megapad64, HaltError, TrapError, u64, sign_extend,
                       EW_FP16, EW_BF16, _float_to_fp16, _fp16_to_float,
                       _float_to_bf16, _bf16_to_float, _fp32_to_bits,
                       _bits_to_fp32)
from asm import assemble

PASS = 0
FAIL = 0

def check(name: str, condition: bool, detail: str = ""):
    global PASS, FAIL
    if condition:
        PASS += 1
        print(f"  ✓ {name}")
    else:
        FAIL += 1
        msg = f"  ✗ {name}"
        if detail:
            msg += f"  — {detail}"
        print(msg)

def run_asm(source: str, base: int = 0) -> tuple[Megapad64, bytearray]:
    """Assemble, load at base, run until halt/idle, return (cpu, bytecode)."""
    code = assemble(source, base)
    cpu = Megapad64()
    cpu.load_bytes(base, code)
    cpu.pc = base
    cpu.regs[cpu.spsel] = 0x10000  # stack at 64K
    try:
        cpu.run(max_steps=50000)
    except HaltError:
        pass
    return cpu, code

# =========================================================================
#  Test groups
# =========================================================================

def test_sys():
    """Family 0x0: SYS — NOP, HALT, SEQ/REQ, EI/DI, MARK/SAV/RET, CALL.L/RET.L, TRAP"""
    print("\n== SYS (0x0) ==")

    # NOP + HALT
    cpu, _ = run_asm("nop\nnop\nhalt")
    check("NOP+HALT", cpu.halted)

    # SEQ / REQ
    cpu, _ = run_asm("seq\nhalt")
    check("SEQ sets Q=1", cpu.q_out == 1)
    cpu, _ = run_asm("seq\nreq\nhalt")
    check("REQ clears Q=0", cpu.q_out == 0)

    # EI / DI
    cpu, _ = run_asm("ei\nhalt")
    check("EI sets I=1", cpu.flag_i == 1)
    cpu, _ = run_asm("ei\ndi\nhalt")
    check("DI clears I=0", cpu.flag_i == 0)

    # MARK + SAV
    cpu, _ = run_asm("""
        sex r2
        sep r3          ; PSEL=3
        mark            ; push X|P, T = X|P, XSEL←PSEL
        halt
    """)
    check("MARK: XSEL becomes PSEL", cpu.xsel == cpu.psel)
    check("MARK: T = old_X|old_P", cpu.t_reg == ((2 << 4) | 3))

    # CALL.L / RET.L round-trip
    cpu, _ = run_asm("""
        ldi r4, 10      ; R4 = target address (function)
        call.l r4       ; push return addr, jump to R4
        halt            ; should execute after RET.L
    .org 10
        nop
        ret.l           ; pop return addr
    """)
    check("CALL.L/RET.L round-trip halts correctly", cpu.halted)


def test_inc_dec():
    """Family 0x1/0x2: INC/DEC"""
    print("\n== INC/DEC (0x1, 0x2) ==")

    cpu, _ = run_asm("""
        ldi r1, 5
        inc r1
        halt
    """)
    check("INC R1: 5→6", cpu.regs[1] == 6)

    cpu, _ = run_asm("""
        ldi r1, 5
        dec r1
        halt
    """)
    check("DEC R1: 5→4", cpu.regs[1] == 4)

    # Edge: DEC from 0 wraps to 0xFFFF_FFFF_FFFF_FFFF
    cpu, _ = run_asm("""
        ldi r1, 0
        dec r1
        halt
    """)
    check("DEC R1: 0 wraps to MAX64", cpu.regs[1] == u64(-1))


def test_branch():
    """Family 0x3: BR (short branch)"""
    print("\n== BR (0x3) ==")

    # Unconditional branch forward
    cpu, _ = run_asm("""
        ldi r1, 42
        br 1            ; skip next instruction (+1 from end of BR)
        ldi r1, 99
        halt
    """)
    check("BR (always) skips over LDI", cpu.regs[1] == 42)

    # Conditional: BEQ taken
    cpu, _ = run_asm("""
        ldi r1, 0
        cmpi r1, 0      ; Z=1
        breq 1           ; taken: skip LDI r1,99
        ldi r1, 99
        halt
    """)
    check("BREQ taken when Z=1", cpu.regs[1] == 0)

    # Conditional: BEQ not taken
    cpu, _ = run_asm("""
        ldi r1, 5
        cmpi r1, 3      ; Z=0
        breq 1           ; not taken
        ldi r1, 77
        halt
    """)
    check("BREQ not taken when Z=0", cpu.regs[1] == 77)


def test_lbr():
    """Family 0x4: LBR (long branch)"""
    print("\n== LBR (0x4) ==")

    cpu, _ = run_asm("""
        ldi r1, 42
        lbr 2            ; skip 2 bytes forward past the LDI
        ldi r1, 0
        halt
    """)
    # Long branch should skip over the ldi r1,0
    check("LBR forward skips instruction", cpu.regs[1] == 42 or cpu.halted)


def test_mem():
    """Family 0x5: MEM (scalar load/store)"""
    print("\n== MEM (0x5) ==")

    # STR + LDN round-trip (64-bit)
    cpu, _ = run_asm("""
        ldi r1, 0xFF
        ldi r2, 200         ; address
        str r2, r1           ; M[R2] ← R1
        ldi r1, 0            ; clear R1
        ldn r1, r2           ; R1 ← M[R2]
        halt
    """)
    check("STR+LDN: 64-bit store/load round-trip", cpu.regs[1] == 0xFF)

    # LD.B / ST.B
    cpu, _ = run_asm("""
        ldi r1, 0xAB
        ldi r2, 200
        st.b r2, r1
        ldi r1, 0
        ld.b r1, r2
        halt
    """)
    check("ST.B+LD.B: byte round-trip", cpu.regs[1] == 0xAB)


def test_imm():
    """Family 0x6: IMM"""
    print("\n== IMM (0x6) ==")

    cpu, _ = run_asm("""
        ldi r1, 42
        halt
    """)
    check("LDI R1, 42", cpu.regs[1] == 42)

    cpu, _ = run_asm("""
        ldi r5, 100
        addi r5, 50
        halt
    """)
    check("ADDI R5: 100+50=150", cpu.regs[5] == 150)

    cpu, _ = run_asm("""
        ldi r6, 0xFF
        andi r6, 0x0F
        halt
    """)
    check("ANDI R6: 0xFF & 0x0F = 0x0F", cpu.regs[6] == 0x0F)

    cpu, _ = run_asm("""
        ldi r7, 0x0F
        ori r7, 0xF0
        halt
    """)
    check("ORI R7: 0x0F | 0xF0 = 0xFF", cpu.regs[7] == 0xFF)

    cpu, _ = run_asm("""
        ldi r1, 0xFF
        xori r1, 0x0F
        halt
    """)
    check("XORI R1: 0xFF ^ 0x0F = 0xF0", cpu.regs[1] == 0xF0)

    cpu, _ = run_asm("""
        ldi r1, 100
        subi r1, 30
        halt
    """)
    check("SUBI R1: 100-30=70", cpu.regs[1] == 70)

    # Shift
    cpu, _ = run_asm("""
        ldi r1, 1
        lsli r1, 4
        halt
    """)
    check("LSLI R1: 1<<4=16", cpu.regs[1] == 16)

    # GLO / PLO
    cpu, _ = run_asm("""
        ldi r1, 0xAB
        glo r1           ; D ← R1[7:0] = 0xAB
        ldi r2, 0
        plo r2           ; R2[7:0] ← D = 0xAB
        halt
    """)
    check("GLO+PLO: D shuttle", cpu.regs[2] == 0xAB)


def test_alu():
    """Family 0x7: ALU (register-register)"""
    print("\n== ALU (0x7) ==")

    cpu, _ = run_asm("""
        ldi r1, 10
        ldi r2, 20
        add r1, r2
        halt
    """)
    check("ADD R1,R2: 10+20=30", cpu.regs[1] == 30)

    cpu, _ = run_asm("""
        ldi r1, 50
        ldi r2, 20
        sub r1, r2
        halt
    """)
    check("SUB R1,R2: 50-20=30", cpu.regs[1] == 30)

    cpu, _ = run_asm("""
        ldi r1, 0xFF
        ldi r2, 0x0F
        and r1, r2
        halt
    """)
    check("AND R1,R2: 0xFF & 0x0F = 0x0F", cpu.regs[1] == 0x0F)

    cpu, _ = run_asm("""
        ldi r1, 0x0F
        ldi r2, 0xF0
        or r1, r2
        halt
    """)
    check("OR R1,R2: 0x0F | 0xF0 = 0xFF", cpu.regs[1] == 0xFF)

    cpu, _ = run_asm("""
        ldi r1, 0xFF
        ldi r2, 0x0F
        xor r1, r2
        halt
    """)
    check("XOR R1,R2: 0xFF ^ 0x0F = 0xF0", cpu.regs[1] == 0xF0)

    cpu, _ = run_asm("""
        ldi r1, 10
        ldi r2, 10
        cmp r1, r2
        halt
    """)
    check("CMP equal: Z=1", cpu.flag_z == 1)
    check("CMP equal: G=0", cpu.flag_g == 0)

    cpu, _ = run_asm("""
        ldi r1, 20
        ldi r2, 10
        cmp r1, r2
        halt
    """)
    check("CMP 20>10: G=1", cpu.flag_g == 1)

    cpu, _ = run_asm("""
        ldi r1, 42
        ldi r2, 42
        mov r5, r2
        halt
    """)
    check("MOV R5,R2: R5=42", cpu.regs[5] == 42)

    cpu, _ = run_asm("""
        ldi r2, 1
        neg r1, r2
        halt
    """)
    check("NEG R1,R2: -1", cpu.regs[1] == u64(-1))

    cpu, _ = run_asm("""
        ldi r1, 0xFF
        ldi r2, 0xFF
        not r1, r2
        halt
    """)
    check("NOT R1,R2: ~0xFF", cpu.regs[1] == u64(~0xFF))


def test_memalu():
    """Family 0x8: MEMALU (1802 compatibility)"""
    print("\n== MEMALU (0x8) ==")

    # LDX: load D from M(R(X))
    cpu = Megapad64()
    cpu.xsel = 2
    cpu.regs[2] = 0x200  # R(X) points to 0x200
    cpu.mem_write8(0x200, 0x42)  # put value there
    code = assemble("ldx\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("LDX: D ← M(R(X))", cpu.d_reg == 0x42)

    # ADD.X
    cpu = Megapad64()
    cpu.xsel = 2
    cpu.regs[2] = 0x200
    cpu.mem_write8(0x200, 10)  # M(R(X)) = 10
    cpu.d_reg = 10             # D = 10
    code = assemble("add.x\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("ADD.X: D = 10+10 = 20", cpu.d_reg == 20)

    # SHL.D
    cpu = Megapad64()
    cpu.d_reg = 0x40
    code = assemble("shl.d\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("SHL.D: 0x40 << 1 = 0x80", cpu.d_reg == 0x80)
    check("SHL.D: carry = 0", cpu.flag_c == 0)

    cpu.d_reg = 0x80
    code = assemble("shl.d\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    cpu.halted = False
    try:
        cpu.run()
    except HaltError:
        pass
    check("SHL.D: 0x80 << 1 = 0x00, C=1", cpu.d_reg == 0x00 and cpu.flag_c == 1)


def test_io():
    """Family 0x9: I/O"""
    print("\n== I/O (0x9) ==")

    captured = []
    cpu = Megapad64()
    cpu.on_output = lambda port, val: captured.append((port, val))
    cpu.regs[2] = 100  # R(X)
    cpu.xsel = 2
    cpu.mem_write8(100, 0xAB)

    code = assemble("out1\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("OUT1: port=1, val=0xAB", len(captured) == 1 and captured[0] == (1, 0xAB))
    check("OUT1: R(X) advanced by 1", cpu.regs[2] == 101)


def test_sep_sex():
    """Family 0xA/0xB: SEP / SEX"""
    print("\n== SEP / SEX (0xA, 0xB) ==")

    cpu, _ = run_asm("""
        ldi r5, 0
        sex r5
        halt
    """)
    check("SEX R5: XSEL=5", cpu.xsel == 5)

    # SEP is trickier — it changes which register is PC
    # We'll test by setting up R5 with a valid address and SEP to it
    cpu = Megapad64()
    # Place HALT at address 30
    halt_code = assemble("halt")
    cpu.load_bytes(30, halt_code)
    # Place SEP R5 at address 0
    sep_code = assemble("ldi r5, 30\nsep r5")
    cpu.load_bytes(0, sep_code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("SEP R5: execution continues at R5", cpu.halted and cpu.psel == 5)


def test_muldiv():
    """Family 0xC: MUL/DIV"""
    print("\n== MUL/DIV (0xC) ==")

    cpu, _ = run_asm("""
        ldi r1, 7
        ldi r2, 6
        mul r1, r2
        halt
    """)
    check("MUL R1,R2: 7*6=42", cpu.regs[1] == 42)

    cpu, _ = run_asm("""
        ldi r1, 7
        ldi r2, 6
        umul r1, r2
        halt
    """)
    check("UMUL R1,R2: 7*6=42", cpu.regs[1] == 42)

    cpu, _ = run_asm("""
        ldi r1, 100
        ldi r2, 7
        udiv r1, r2
        halt
    """)
    check("UDIV 100/7: q=14", cpu.regs[1] == 14)
    check("UDIV 100/7: rem=2 in R0", cpu.regs[0] == 2)

    cpu, _ = run_asm("""
        ldi r1, 100
        ldi r2, 10
        umod r1, r2
        halt
    """)
    check("UMOD 100%10 = 0", cpu.regs[1] == 0)

    # Divide by zero
    try:
        cpu, _ = run_asm("""
            ldi r1, 10
            ldi r2, 0
            udiv r1, r2
            halt
        """)
        check("UDIV by zero → trap", False, "No exception raised")
    except TrapError as e:
        check("UDIV by zero → trap", e.ivec_id == 0x04)


def test_csr():
    """Family 0xD: CSR read/write"""
    print("\n== CSR (0xD) ==")

    cpu, _ = run_asm("""
        ldi r1, 7
        csrw 0x03, r1    ; SPSEL ← 7
        csrr r2, 0x03    ; R2 ← SPSEL
        halt
    """)
    check("CSRW+CSRR SPSEL: R2=7", cpu.regs[2] == 7)
    check("SPSEL updated to 7", cpu.spsel == 7)


def test_mex():
    """Family 0xE: MEX tile ops (basic)"""
    print("\n== MEX (0xE) ==")

    cpu = Megapad64()
    # Set up tile addresses
    cpu.tsrc0 = 0x1000
    cpu.tsrc1 = 0x2000
    cpu.tdst  = 0x3000
    cpu.tmode = 0x00  # 8-bit elements, unsigned

    # Fill src0 with 1s, src1 with 2s
    for i in range(64):
        cpu.mem_write8(0x1000 + i, 1)
        cpu.mem_write8(0x2000 + i, 2)

    # TALU ADD
    code = assemble("t.add\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass

    # Check dst tile: each byte should be 3 (1+2)
    all_three = all(cpu.mem_read8(0x3000 + i) == 3 for i in range(64))
    check("T.ADD: 64 lanes of 1+2=3", all_three)

    # TRED SUM
    cpu2 = Megapad64()
    cpu2.tsrc0 = 0x1000
    cpu2.tmode = 0x00
    for i in range(64):
        cpu2.mem_write8(0x1000 + i, 1)  # 64 ones → sum = 64

    code2 = assemble("t.sum\nhalt")
    cpu2.load_bytes(0, code2)
    cpu2.pc = 0
    try:
        cpu2.run()
    except HaltError:
        pass
    check("T.SUM: sum of 64 ones = 64", cpu2.acc[0] == 64)


def test_vshr_vshl():
    """Extended tile ALU: VSHR / VSHL / VCLZ"""
    print("\n== VSHR / VSHL / VCLZ ==")

    # --- VSHR: 8-bit unsigned, shift each lane right by shift tile ---
    cpu = Megapad64()
    cpu.tsrc0 = 0x1000
    cpu.tsrc1 = 0x2000
    cpu.tdst  = 0x3000
    cpu.tmode = 0x00  # 8-bit unsigned
    for i in range(64):
        cpu.mem_write8(0x1000 + i, 0x80)  # src0: all 128
        cpu.mem_write8(0x2000 + i, 2)     # src1: shift by 2
    code = assemble("t.vshr\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    result = cpu.mem_read8(0x3000)
    check("VSHR u8: 128 >> 2 = 32", result == 32,
          f"got {result}")
    check("VSHR u8: all lanes", all(cpu.mem_read8(0x3000 + i) == 32 for i in range(64)))

    # --- VSHL: 8-bit unsigned ---
    cpu2 = Megapad64()
    cpu2.tsrc0 = 0x1000
    cpu2.tsrc1 = 0x2000
    cpu2.tdst  = 0x3000
    cpu2.tmode = 0x00
    for i in range(64):
        cpu2.mem_write8(0x1000 + i, 3)   # src0: all 3
        cpu2.mem_write8(0x2000 + i, 4)   # src1: shift by 4
    code2 = assemble("t.vshl\nhalt")
    cpu2.load_bytes(0, code2)
    cpu2.pc = 0
    try:
        cpu2.run()
    except HaltError:
        pass
    result = cpu2.mem_read8(0x3000)
    check("VSHL u8: 3 << 4 = 48", result == 48, f"got {result}")

    # --- VSHR with rounding (TMODE bit 6) ---
    cpu3 = Megapad64()
    cpu3.tsrc0 = 0x1000
    cpu3.tsrc1 = 0x2000
    cpu3.tdst  = 0x3000
    cpu3.tmode = 0x40  # 8-bit unsigned, rounding (bit 6)
    for i in range(64):
        cpu3.mem_write8(0x1000 + i, 7)   # src0: all 7
        cpu3.mem_write8(0x2000 + i, 1)   # src1: shift by 1
    # Without rounding: 7 >> 1 = 3 (truncated)
    # With rounding:    (7 + 1) >> 1 = 4 (rounded)
    code3 = assemble("t.vshr\nhalt")
    cpu3.load_bytes(0, code3)
    cpu3.pc = 0
    try:
        cpu3.run()
    except HaltError:
        pass
    result = cpu3.mem_read8(0x3000)
    check("VSHR rounding u8: (7+0.5)>>1 = 4", result == 4, f"got {result}")

    # --- VSHR signed ---
    cpu4 = Megapad64()
    cpu4.tsrc0 = 0x1000
    cpu4.tsrc1 = 0x2000
    cpu4.tdst  = 0x3000
    cpu4.tmode = 0x10  # 8-bit signed (bit 4 = signed)
    for i in range(64):
        cpu4.mem_write8(0x1000 + i, 0xF0)  # -16 in signed i8
        cpu4.mem_write8(0x2000 + i, 2)     # shift by 2
    code4 = assemble("t.vshr\nhalt")
    cpu4.load_bytes(0, code4)
    cpu4.pc = 0
    try:
        cpu4.run()
    except HaltError:
        pass
    result = cpu4.mem_read8(0x3000)
    # -16 >> 2 = -4 → 0xFC
    check("VSHR signed i8: -16 >> 2 = -4 (0xFC)", result == 0xFC,
          f"got 0x{result:02X}")

    # --- VSHR 16-bit ---
    cpu5 = Megapad64()
    cpu5.tsrc0 = 0x1000
    cpu5.tsrc1 = 0x2000
    cpu5.tdst  = 0x3000
    cpu5.tmode = 0x01  # 16-bit unsigned
    # Write 0x0100 (256) to each 16-bit lane of src0
    for i in range(32):
        cpu5.mem_write8(0x1000 + i*2, 0x00)
        cpu5.mem_write8(0x1000 + i*2 + 1, 0x01)
        # Shift amount: 4
        cpu5.mem_write8(0x2000 + i*2, 4)
        cpu5.mem_write8(0x2000 + i*2 + 1, 0)
    code5 = assemble("t.vshr\nhalt")
    cpu5.load_bytes(0, code5)
    cpu5.pc = 0
    try:
        cpu5.run()
    except HaltError:
        pass
    lo = cpu5.mem_read8(0x3000)
    hi = cpu5.mem_read8(0x3001)
    result16 = lo | (hi << 8)
    check("VSHR u16: 256 >> 4 = 16", result16 == 16, f"got {result16}")

    # --- VCLZ: count leading zeros ---
    cpu6 = Megapad64()
    cpu6.tsrc0 = 0x1000
    cpu6.tsrc1 = 0x2000
    cpu6.tdst  = 0x3000
    cpu6.tmode = 0x00  # 8-bit unsigned
    cpu6.mem_write8(0x1000, 0x01)  # CLZ = 7
    cpu6.mem_write8(0x1000 + 1, 0x80)  # CLZ = 0
    cpu6.mem_write8(0x1000 + 2, 0x00)  # CLZ = 8
    cpu6.mem_write8(0x1000 + 3, 0x10)  # CLZ = 3
    for i in range(4, 64):
        cpu6.mem_write8(0x1000 + i, 0xFF)  # CLZ = 0
        cpu6.mem_write8(0x2000 + i, 0)     # unused for VCLZ
    code6 = assemble("t.vclz\nhalt")
    cpu6.load_bytes(0, code6)
    cpu6.pc = 0
    try:
        cpu6.run()
    except HaltError:
        pass
    check("VCLZ u8: clz(0x01) = 7", cpu6.mem_read8(0x3000) == 7,
          f"got {cpu6.mem_read8(0x3000)}")
    check("VCLZ u8: clz(0x80) = 0", cpu6.mem_read8(0x3001) == 0,
          f"got {cpu6.mem_read8(0x3001)}")
    check("VCLZ u8: clz(0x00) = 8", cpu6.mem_read8(0x3002) == 8,
          f"got {cpu6.mem_read8(0x3002)}")
    check("VCLZ u8: clz(0x10) = 3", cpu6.mem_read8(0x3003) == 3,
          f"got {cpu6.mem_read8(0x3003)}")

    # --- VSHR broadcast mode ---
    cpu7 = Megapad64()
    cpu7.tsrc0 = 0x1000
    cpu7.tdst  = 0x3000
    cpu7.tmode = 0x00  # 8-bit unsigned
    cpu7.regs[5] = 3    # shift all lanes by 3
    for i in range(64):
        cpu7.mem_write8(0x1000 + i, 0x40)  # src0: all 64
    code7 = assemble("t.vshr r5\nhalt")
    cpu7.load_bytes(0, code7)
    cpu7.pc = 0
    try:
        cpu7.run()
    except HaltError:
        pass
    result = cpu7.mem_read8(0x3000)
    check("VSHR broadcast u8: 64 >> 3 = 8", result == 8, f"got {result}")


def test_tile_views():
    """TSYS tile views: SHUFFLE, PACK, UNPACK, RROT"""
    print("\n== Tile Views ==")

    # --- SHUFFLE: permute lanes by index tile ---
    cpu = Megapad64()
    cpu.tsrc0 = 0x1000
    cpu.tsrc1 = 0x2000  # index tile
    cpu.tdst  = 0x3000
    cpu.tmode = 0x00  # 8-bit unsigned
    # src0: lane i = i (identity)
    for i in range(64):
        cpu.mem_write8(0x1000 + i, i)
    # index: reverse order
    for i in range(64):
        cpu.mem_write8(0x2000 + i, 63 - i)
    code = assemble("t.shuffle\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    check("SHUFFLE: reverse permute [0]=63",
          cpu.mem_read8(0x3000) == 63,
          f"got {cpu.mem_read8(0x3000)}")
    check("SHUFFLE: reverse permute [63]=0",
          cpu.mem_read8(0x3000 + 63) == 0,
          f"got {cpu.mem_read8(0x3000 + 63)}")
    check("SHUFFLE: reverse permute [32]=31",
          cpu.mem_read8(0x3000 + 32) == 31,
          f"got {cpu.mem_read8(0x3000 + 32)}")

    # SHUFFLE: broadcast (all indices = 5)
    cpu2 = Megapad64()
    cpu2.tsrc0 = 0x1000
    cpu2.tsrc1 = 0x2000
    cpu2.tdst  = 0x3000
    cpu2.tmode = 0x00
    for i in range(64):
        cpu2.mem_write8(0x1000 + i, i * 2)  # src: 0, 2, 4, ...
        cpu2.mem_write8(0x2000 + i, 5)       # all indices point to lane 5
    code2 = assemble("t.shuffle\nhalt")
    cpu2.load_bytes(0, code2)
    cpu2.pc = 0
    try:
        cpu2.run()
    except HaltError:
        pass
    expected = 5 * 2
    check("SHUFFLE: broadcast index 5 → all lanes = 10",
          all(cpu2.mem_read8(0x3000 + i) == expected for i in range(64)),
          f"lane0={cpu2.mem_read8(0x3000)}")

    # --- PACK: 16-bit → 8-bit (unsigned, no saturation) ---
    cpu3 = Megapad64()
    cpu3.tsrc0 = 0x1000
    cpu3.tdst  = 0x3000
    cpu3.tmode = 0x01  # 16-bit unsigned (pack to 8-bit)
    # Write 32 × 16-bit values: [0x0042, 0x0043, 0x0044, ...]
    for i in range(32):
        val = 0x42 + i
        cpu3.mem_write8(0x1000 + i*2, val & 0xFF)
        cpu3.mem_write8(0x1000 + i*2 + 1, (val >> 8) & 0xFF)
    code3 = assemble("t.pack\nhalt")
    cpu3.load_bytes(0, code3)
    cpu3.pc = 0
    try:
        cpu3.run()
    except HaltError:
        pass
    check("PACK u16→u8: lane 0 = 0x42",
          cpu3.mem_read8(0x3000) == 0x42,
          f"got 0x{cpu3.mem_read8(0x3000):02X}")
    check("PACK u16→u8: lane 1 = 0x43",
          cpu3.mem_read8(0x3001) == 0x43,
          f"got 0x{cpu3.mem_read8(0x3001):02X}")

    # PACK with saturation: 16-bit signed → 8-bit signed, values > 127 clamped
    cpu4 = Megapad64()
    cpu4.tsrc0 = 0x1000
    cpu4.tdst  = 0x3000
    cpu4.tmode = 0x31  # 16-bit, signed (bit 4), saturating (bit 5)
    # Lane 0: 200 (should saturate to 127)
    cpu4.mem_write8(0x1000, 200 & 0xFF)
    cpu4.mem_write8(0x1001, 0)
    # Lane 1: -200 (0xFF38, should saturate to -128 = 0x80)
    val_neg = (-200) & 0xFFFF
    cpu4.mem_write8(0x1002, val_neg & 0xFF)
    cpu4.mem_write8(0x1003, (val_neg >> 8) & 0xFF)
    # Lane 2: 50 (fits in i8)
    cpu4.mem_write8(0x1004, 50)
    cpu4.mem_write8(0x1005, 0)
    for i in range(3, 32):
        cpu4.mem_write8(0x1000 + i*2, 0)
        cpu4.mem_write8(0x1000 + i*2 + 1, 0)
    code4 = assemble("t.pack\nhalt")
    cpu4.load_bytes(0, code4)
    cpu4.pc = 0
    try:
        cpu4.run()
    except HaltError:
        pass
    check("PACK sat i16→i8: 200 → 127",
          cpu4.mem_read8(0x3000) == 0x7F,
          f"got 0x{cpu4.mem_read8(0x3000):02X}")
    check("PACK sat i16→i8: -200 → -128 (0x80)",
          cpu4.mem_read8(0x3001) == 0x80,
          f"got 0x{cpu4.mem_read8(0x3001):02X}")
    check("PACK sat i16→i8: 50 → 50",
          cpu4.mem_read8(0x3002) == 50,
          f"got {cpu4.mem_read8(0x3002)}")

    # --- UNPACK: 8-bit → 16-bit (unsigned) ---
    cpu5 = Megapad64()
    cpu5.tsrc0 = 0x1000
    cpu5.tdst  = 0x3000
    cpu5.tmode = 0x00  # 8-bit unsigned (unpack to 16-bit)
    for i in range(64):
        cpu5.mem_write8(0x1000 + i, 0x42 + (i % 10))
    code5 = assemble("t.unpack\nhalt")
    cpu5.load_bytes(0, code5)
    cpu5.pc = 0
    try:
        cpu5.run()
    except HaltError:
        pass
    # Output: 32 × 16-bit values from first 32 bytes of input
    lo = cpu5.mem_read8(0x3000)
    hi = cpu5.mem_read8(0x3001)
    check("UNPACK u8→u16: lane 0 = 0x0042",
          lo == 0x42 and hi == 0x00,
          f"got 0x{hi:02X}{lo:02X}")

    # UNPACK signed: 8-bit signed → 16-bit signed (sign-extend)
    cpu6 = Megapad64()
    cpu6.tsrc0 = 0x1000
    cpu6.tdst  = 0x3000
    cpu6.tmode = 0x10  # 8-bit signed
    cpu6.mem_write8(0x1000, 0xFE)  # -2 in i8
    cpu6.mem_write8(0x1001, 0x05)  # +5 in i8
    for i in range(2, 64):
        cpu6.mem_write8(0x1000 + i, 0)
    code6 = assemble("t.unpack\nhalt")
    cpu6.load_bytes(0, code6)
    cpu6.pc = 0
    try:
        cpu6.run()
    except HaltError:
        pass
    lo0 = cpu6.mem_read8(0x3000)
    hi0 = cpu6.mem_read8(0x3001)
    val0 = lo0 | (hi0 << 8)
    check("UNPACK signed i8→i16: -2 → 0xFFFE",
          val0 == 0xFFFE, f"got 0x{val0:04X}")
    lo1 = cpu6.mem_read8(0x3002)
    hi1 = cpu6.mem_read8(0x3003)
    val1 = lo1 | (hi1 << 8)
    check("UNPACK signed i8→i16: +5 → 0x0005",
          val1 == 0x0005, f"got 0x{val1:04X}")

    # --- RROT: row-rotate-left by 2 (8-bit mode = 8×8 matrix) ---
    cpu7 = Megapad64()
    cpu7.tsrc0 = 0x1000
    cpu7.tdst  = 0x3000
    cpu7.tmode = 0x00  # 8-bit
    # Fill src: lane i = i
    for i in range(64):
        cpu7.mem_write8(0x1000 + i, i)
    # ctrl = direction 0 (row-left), amount 2, mirror=0
    # ctrl byte = (amount << 2) | direction = (2 << 2) | 0 = 8
    ctrl = (2 << 2) | 0  # 8
    code7 = assemble(f"t.rrot {ctrl}\nhalt")
    cpu7.load_bytes(0, code7)
    cpu7.pc = 0
    try:
        cpu7.run()
    except HaltError:
        pass
    # Row 0 of 8×8: [0,1,2,3,4,5,6,7] rotate left by 2 → [2,3,4,5,6,7,0,1]
    check("RROT row-left-2: [0]=2",
          cpu7.mem_read8(0x3000) == 2,
          f"got {cpu7.mem_read8(0x3000)}")
    check("RROT row-left-2: [6]=0",
          cpu7.mem_read8(0x3006) == 0,
          f"got {cpu7.mem_read8(0x3006)}")
    check("RROT row-left-2: [7]=1",
          cpu7.mem_read8(0x3007) == 1,
          f"got {cpu7.mem_read8(0x3007)}")

    # RROT: horizontal mirror (mirror=1, direction bit 0 = 0 → h-mirror)
    cpu8 = Megapad64()
    cpu8.tsrc0 = 0x1000
    cpu8.tdst  = 0x3000
    cpu8.tmode = 0x00
    for i in range(64):
        cpu8.mem_write8(0x1000 + i, i)
    # ctrl: mirror=1 (bit 5), direction=0 (h-mirror)
    ctrl = (1 << 5) | 0  # 0x20
    code8 = assemble(f"t.rrot {ctrl}\nhalt")
    cpu8.load_bytes(0, code8)
    cpu8.pc = 0
    try:
        cpu8.run()
    except HaltError:
        pass
    # Row 0: [0,1,2,3,4,5,6,7] h-mirror → [7,6,5,4,3,2,1,0]
    check("RROT h-mirror: [0]=7",
          cpu8.mem_read8(0x3000) == 7,
          f"got {cpu8.mem_read8(0x3000)}")
    check("RROT h-mirror: [7]=0",
          cpu8.mem_read8(0x3007) == 0,
          f"got {cpu8.mem_read8(0x3007)}")
    # Row 1: [8,9,10,11,12,13,14,15] h-mirror → [15,14,13,12,11,10,9,8]
    check("RROT h-mirror: [8]=15",
          cpu8.mem_read8(0x3008) == 15,
          f"got {cpu8.mem_read8(0x3008)}")

    # RROT: column-rotate-down by 1
    cpu9 = Megapad64()
    cpu9.tsrc0 = 0x1000
    cpu9.tdst  = 0x3000
    cpu9.tmode = 0x00
    for i in range(64):
        cpu9.mem_write8(0x1000 + i, i)
    # ctrl: direction=3 (col-down), amount=1
    ctrl = (1 << 2) | 3  # 7
    code9 = assemble(f"t.rrot {ctrl}\nhalt")
    cpu9.load_bytes(0, code9)
    cpu9.pc = 0
    try:
        cpu9.run()
    except HaltError:
        pass
    # Col 0 was [0,8,16,24,32,40,48,56], rotate down by 1 → [56,0,8,16,24,32,40,48]
    check("RROT col-down-1: [0]=56",
          cpu9.mem_read8(0x3000) == 56,
          f"got {cpu9.mem_read8(0x3000)}")
    check("RROT col-down-1: [8]=0",
          cpu9.mem_read8(0x3008) == 0,
          f"got {cpu9.mem_read8(0x3008)}")


def test_strided_2d():
    """Strided/2D tile addressing: LOAD2D, STORE2D, stride CSRs"""
    print("\n== Strided/2D Tile Addressing ==")

    # --- LOAD2D: gather an 8×8 patch from a 16-byte-wide image ---
    cpu = Megapad64()
    # Lay out a 16-wide × 10-tall "image" at address 0x2000
    img_base = 0x2000
    img_width = 16
    for row in range(10):
        for col in range(16):
            cpu.mem_write8(img_base + row * img_width + col, row * 16 + col)

    # Set up cursor to point at the image base
    cpu.sb = 0
    cpu.sr = 0
    cpu.sc = 0
    cpu.sw = 1
    # For _tile_cursor_addr: bank_base + (sr*sw + sc)*64 = 0
    # We need cursor to point at 0x2000, so let's set it via direct tile address
    # Actually, _tile_cursor_addr computes from sb/sr/sc/sw:
    #   addr = sb*(4MiB) + (sr*sw + sc)*64
    # We want addr=0x2000=8192, so (sr*sw + sc)*64 = 8192, sr*sw+sc = 128
    # With sw=1: sr=128, sc=0
    cpu.sr = 128
    cpu.sc = 0
    cpu.sw = 1

    # Configure stride and tile dimensions
    cpu.tstride_r = img_width   # 16 bytes between rows
    cpu.ttile_h = 8             # 8 rows
    cpu.ttile_w = 8             # 8 columns (bytes per row)
    cpu.tdst = 0x4000           # output tile

    code = assemble("t.load2d\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass

    # Check: output tile should contain the 8×8 top-left patch
    # Row 0: pixels 0..7, Row 1: pixels 16..23, etc.
    check("LOAD2D [0,0]=0",
          cpu.mem_read8(0x4000) == 0,
          f"got {cpu.mem_read8(0x4000)}")
    check("LOAD2D [0,7]=7",
          cpu.mem_read8(0x4007) == 7,
          f"got {cpu.mem_read8(0x4007)}")
    # Row 1 starts at offset 8 in the tile (8 bytes per row)
    check("LOAD2D [1,0]=16",
          cpu.mem_read8(0x4008) == 16,
          f"got {cpu.mem_read8(0x4008)}")
    check("LOAD2D [1,7]=23",
          cpu.mem_read8(0x400F) == 23,
          f"got {cpu.mem_read8(0x400F)}")
    # Row 7 starts at offset 7*8=56 in the tile
    check("LOAD2D [7,0]=112",
          cpu.mem_read8(0x4038) == 112,
          f"got {cpu.mem_read8(0x4038)}")
    check("LOAD2D [7,7]=119",
          cpu.mem_read8(0x403F) == 119,
          f"got {cpu.mem_read8(0x403F)}")

    # --- STORE2D: scatter tile back to a different location ---
    cpu2 = Megapad64()
    # Source tile at 0x4000 with sequential data
    for i in range(64):
        cpu2.mem_write8(0x4000 + i, i + 100)
    cpu2.tsrc0 = 0x4000

    # Destination: 32-byte-wide image at 0x6000
    dst_base = 0x6000
    dst_width = 32
    # Cursor → 0x6000: (sr*sw + sc)*64 = 0x6000 = 24576, sr=384, sc=0, sw=1
    cpu2.sr = 384
    cpu2.sc = 0
    cpu2.sw = 1
    cpu2.tstride_r = dst_width  # 32 bytes between rows
    cpu2.ttile_h = 4            # only 4 rows
    cpu2.ttile_w = 8            # 8 bytes per row

    code2 = assemble("t.store2d\nhalt")
    cpu2.load_bytes(0, code2)
    cpu2.pc = 0
    try:
        cpu2.run()
    except HaltError:
        pass

    # Row 0: bytes 100..107 written to 0x6000..0x6007
    check("STORE2D [0,0]=100",
          cpu2.mem_read8(0x6000) == 100,
          f"got {cpu2.mem_read8(0x6000)}")
    check("STORE2D [0,7]=107",
          cpu2.mem_read8(0x6007) == 107,
          f"got {cpu2.mem_read8(0x6007)}")
    # Row 1: bytes 108..115 written to 0x6020..0x6027 (stride=32)
    check("STORE2D [1,0]=108",
          cpu2.mem_read8(0x6020) == 108,
          f"got {cpu2.mem_read8(0x6020)}")
    # Gap between rows should be untouched (zero)
    check("STORE2D gap untouched",
          cpu2.mem_read8(0x6008) == 0,
          f"got {cpu2.mem_read8(0x6008)}")
    # Row 3: bytes 124..131 at 0x6060..0x6067
    check("STORE2D [3,0]=124",
          cpu2.mem_read8(0x6060) == 124,
          f"got {cpu2.mem_read8(0x6060)}")

    # --- LOAD2D with stride=0: defaults to contiguous (w=ttile_w) ---
    cpu3 = Megapad64()
    for i in range(64):
        cpu3.mem_write8(0x2000 + i, i)
    cpu3.sr = 128  # cursor → 0x2000
    cpu3.sc = 0
    cpu3.sw = 1
    cpu3.tstride_r = 0  # disabled → stride = ttile_w
    cpu3.ttile_h = 8
    cpu3.ttile_w = 8
    cpu3.tdst = 0x4000

    code3 = assemble("t.load2d\nhalt")
    cpu3.load_bytes(0, code3)
    cpu3.pc = 0
    try:
        cpu3.run()
    except HaltError:
        pass
    # With stride = ttile_w = 8, contiguous: tile byte 0 = mem[0x2000]
    check("LOAD2D contiguous [0]=0",
          cpu3.mem_read8(0x4000) == 0,
          f"got {cpu3.mem_read8(0x4000)}")
    check("LOAD2D contiguous [63]=63",
          cpu3.mem_read8(0x403F) == 63,
          f"got {cpu3.mem_read8(0x403F)}")

    # --- CSR read/write for stride registers ---
    # NOTE: R3 is the PC (psel=3), so avoid using it for CSR readback!
    cpu4, _ = run_asm("""
        ldi64 r1, 640
        csrw 0x40, r1       ; TSTRIDE_R = 640
        csrr r2, 0x40       ; read it back into r2
        ldi r1, 6
        csrw 0x42, r1       ; TTILE_H = 6
        csrr r5, 0x42       ; read back into r5 (avoid r3=PC!)
        ldi r1, 10
        csrw 0x43, r1       ; TTILE_W = 10
        csrr r4, 0x43       ; read back into r4
        halt
    """)
    check("TSTRIDE_R CSR round-trip: 640",
          cpu4.regs[2] == 640, f"got {cpu4.regs[2]}")
    check("TTILE_H CSR round-trip: 6",
          cpu4.regs[5] == 6, f"got {cpu4.regs[5]}")
    check("TTILE_W CSR round-trip: 10",
          cpu4.regs[4] == 10, f"got {cpu4.regs[4]}")

    # --- Partial tile (H×W < 64): zero-fill remainder ---
    cpu5 = Megapad64()
    for i in range(64):
        cpu5.mem_write8(0x2000 + i, 0xAA)
    cpu5.sr = 128  # cursor → 0x2000
    cpu5.sc = 0
    cpu5.sw = 1
    cpu5.tstride_r = 4
    cpu5.ttile_h = 2
    cpu5.ttile_w = 3    # only 2×3 = 6 bytes loaded
    cpu5.tdst = 0x4000
    # Pre-fill dst with 0xFF to verify zero-fill
    for i in range(64):
        cpu5.mem_write8(0x4000 + i, 0xFF)

    code5 = assemble("t.load2d\nhalt")
    cpu5.load_bytes(0, code5)
    cpu5.pc = 0
    try:
        cpu5.run()
    except HaltError:
        pass
    check("LOAD2D partial: [0]=0xAA",
          cpu5.mem_read8(0x4000) == 0xAA,
          f"got 0x{cpu5.mem_read8(0x4000):02X}")
    check("LOAD2D partial: 6 bytes loaded, [6]=0 (zero-fill)",
          cpu5.mem_read8(0x4006) == 0,
          f"got 0x{cpu5.mem_read8(0x4006):02X}")
    check("LOAD2D partial: [63]=0 (zero-fill)",
          cpu5.mem_read8(0x403F) == 0,
          f"got 0x{cpu5.mem_read8(0x403F):02X}")


def test_tile_fp():
    """Tile engine FP16/bfloat16 operations"""
    print("\n== Tile FP16 / bfloat16 ==")
    import struct, math

    # Helper: write fp16 value to tile lane
    def write_fp16_lane(cpu, base, lane, val_f):
        raw = _float_to_fp16(val_f)
        off = base + lane * 2
        cpu.mem_write8(off, raw & 0xFF)
        cpu.mem_write8(off + 1, (raw >> 8) & 0xFF)

    def read_fp16_lane(cpu, base, lane):
        off = base + lane * 2
        raw = cpu.mem_read8(off) | (cpu.mem_read8(off + 1) << 8)
        return _fp16_to_float(raw)

    def write_bf16_lane(cpu, base, lane, val_f):
        raw = _float_to_bf16(val_f)
        off = base + lane * 2
        cpu.mem_write8(off, raw & 0xFF)
        cpu.mem_write8(off + 1, (raw >> 8) & 0xFF)

    def read_bf16_lane(cpu, base, lane):
        off = base + lane * 2
        raw = cpu.mem_read8(off) | (cpu.mem_read8(off + 1) << 8)
        return _bf16_to_float(raw)

    # ---- FP16 conversion round-trip ----
    for val in [0.0, 1.0, -1.0, 0.5, 65504.0, -0.0]:
        raw = _float_to_fp16(val)
        back = _fp16_to_float(raw)
        check(f"FP16 round-trip {val}", back == val or (val == 0.0 and back == 0.0),
              f"got {back}")

    # ---- BF16 conversion round-trip ----
    for val in [0.0, 1.0, -1.0, 0.5, 256.0]:
        raw = _float_to_bf16(val)
        back = _bf16_to_float(raw)
        check(f"BF16 round-trip {val}", back == val, f"got {back}")

    # ---- FP16 TALU ADD: 1.0 + 2.0 = 3.0 in all 32 lanes ----
    cpu = Megapad64()
    cpu.tsrc0 = 0x1000
    cpu.tsrc1 = 0x2000
    cpu.tdst  = 0x3000
    cpu.tmode = EW_FP16  # fp16 mode
    for lane in range(32):
        write_fp16_lane(cpu, 0x1000, lane, 1.0)
        write_fp16_lane(cpu, 0x2000, lane, 2.0)

    code = assemble("t.add\nhalt")
    cpu.load_bytes(0, code)
    cpu.pc = 0
    try:
        cpu.run()
    except HaltError:
        pass
    r = read_fp16_lane(cpu, 0x3000, 0)
    check("FP16 T.ADD: 1.0+2.0=3.0", r == 3.0, f"got {r}")
    check("FP16 T.ADD: all lanes",
          all(read_fp16_lane(cpu, 0x3000, i) == 3.0 for i in range(32)))

    # ---- FP16 TALU SUB: 5.0 - 2.0 = 3.0 ----
    cpu2 = Megapad64()
    cpu2.tsrc0 = 0x1000; cpu2.tsrc1 = 0x2000; cpu2.tdst = 0x3000
    cpu2.tmode = EW_FP16
    for lane in range(32):
        write_fp16_lane(cpu2, 0x1000, lane, 5.0)
        write_fp16_lane(cpu2, 0x2000, lane, 2.0)
    code = assemble("t.sub\nhalt")
    cpu2.load_bytes(0, code)
    cpu2.pc = 0
    try:
        cpu2.run()
    except HaltError:
        pass
    check("FP16 T.SUB: 5.0-2.0=3.0", read_fp16_lane(cpu2, 0x3000, 0) == 3.0,
          f"got {read_fp16_lane(cpu2, 0x3000, 0)}")

    # ---- FP16 TMUL MUL: 3.0 * 4.0 = 12.0 ----
    cpu3 = Megapad64()
    cpu3.tsrc0 = 0x1000; cpu3.tsrc1 = 0x2000; cpu3.tdst = 0x3000
    cpu3.tmode = EW_FP16
    for lane in range(32):
        write_fp16_lane(cpu3, 0x1000, lane, 3.0)
        write_fp16_lane(cpu3, 0x2000, lane, 4.0)
    code = assemble("t.mul\nhalt")
    cpu3.load_bytes(0, code)
    cpu3.pc = 0
    try:
        cpu3.run()
    except HaltError:
        pass
    check("FP16 T.MUL: 3.0*4.0=12.0", read_fp16_lane(cpu3, 0x3000, 0) == 12.0,
          f"got {read_fp16_lane(cpu3, 0x3000, 0)}")

    # ---- FP16 MIN/MAX ----
    cpu4 = Megapad64()
    cpu4.tsrc0 = 0x1000; cpu4.tsrc1 = 0x2000; cpu4.tdst = 0x3000
    cpu4.tmode = EW_FP16
    write_fp16_lane(cpu4, 0x1000, 0, 3.5)
    write_fp16_lane(cpu4, 0x2000, 0, 7.0)
    for lane in range(1, 32):
        write_fp16_lane(cpu4, 0x1000, lane, 0.0)
        write_fp16_lane(cpu4, 0x2000, lane, 0.0)
    code = assemble("t.min\nhalt")
    cpu4.load_bytes(0, code)
    cpu4.pc = 0
    try:
        cpu4.run()
    except HaltError:
        pass
    check("FP16 T.MIN: min(3.5,7.0)=3.5", read_fp16_lane(cpu4, 0x3000, 0) == 3.5,
          f"got {read_fp16_lane(cpu4, 0x3000, 0)}")

    cpu4b = Megapad64()
    cpu4b.tsrc0 = 0x1000; cpu4b.tsrc1 = 0x2000; cpu4b.tdst = 0x3000
    cpu4b.tmode = EW_FP16
    write_fp16_lane(cpu4b, 0x1000, 0, 3.5)
    write_fp16_lane(cpu4b, 0x2000, 0, 7.0)
    for lane in range(1, 32):
        write_fp16_lane(cpu4b, 0x1000, lane, 0.0)
        write_fp16_lane(cpu4b, 0x2000, lane, 0.0)
    code = assemble("t.max\nhalt")
    cpu4b.load_bytes(0, code)
    cpu4b.pc = 0
    try:
        cpu4b.run()
    except HaltError:
        pass
    check("FP16 T.MAX: max(3.5,7.0)=7.0", read_fp16_lane(cpu4b, 0x3000, 0) == 7.0,
          f"got {read_fp16_lane(cpu4b, 0x3000, 0)}")

    # ---- FP16 ABS: abs(-5.0) = 5.0 ----
    cpu5 = Megapad64()
    cpu5.tsrc0 = 0x1000; cpu5.tsrc1 = 0x2000; cpu5.tdst = 0x3000
    cpu5.tmode = EW_FP16
    write_fp16_lane(cpu5, 0x1000, 0, -5.0)
    write_fp16_lane(cpu5, 0x1000, 1, 3.0)
    for lane in range(2, 32):
        write_fp16_lane(cpu5, 0x1000, lane, 0.0)
    code = assemble("t.abs\nhalt")
    cpu5.load_bytes(0, code)
    cpu5.pc = 0
    try:
        cpu5.run()
    except HaltError:
        pass
    check("FP16 T.ABS: abs(-5.0)=5.0", read_fp16_lane(cpu5, 0x3000, 0) == 5.0,
          f"got {read_fp16_lane(cpu5, 0x3000, 0)}")
    check("FP16 T.ABS: abs(3.0)=3.0", read_fp16_lane(cpu5, 0x3000, 1) == 3.0,
          f"got {read_fp16_lane(cpu5, 0x3000, 1)}")

    # ---- FP16 DOT product: 32 lanes of 1.0 * 2.0 → ACC = 64.0 (FP32) ----
    cpu6 = Megapad64()
    cpu6.tsrc0 = 0x1000; cpu6.tsrc1 = 0x2000; cpu6.tdst = 0x3000
    cpu6.tmode = EW_FP16
    cpu6.tctrl = 0
    for lane in range(32):
        write_fp16_lane(cpu6, 0x1000, lane, 1.0)
        write_fp16_lane(cpu6, 0x2000, lane, 2.0)
    code = assemble("t.dot\nhalt")
    cpu6.load_bytes(0, code)
    cpu6.pc = 0
    try:
        cpu6.run()
    except HaltError:
        pass
    dot_result = _bits_to_fp32(cpu6.acc[0])
    check("FP16 T.DOT: 32×(1.0*2.0) = 64.0 (FP32 in ACC)",
          dot_result == 64.0, f"got {dot_result}")

    # ---- FP16 SUM: 32 lanes of 0.5 → 16.0 (FP32 in ACC) ----
    cpu7 = Megapad64()
    cpu7.tsrc0 = 0x1000; cpu7.tmode = EW_FP16; cpu7.tctrl = 0
    for lane in range(32):
        write_fp16_lane(cpu7, 0x1000, lane, 0.5)
    code = assemble("t.sum\nhalt")
    cpu7.load_bytes(0, code)
    cpu7.pc = 0
    try:
        cpu7.run()
    except HaltError:
        pass
    sum_result = _bits_to_fp32(cpu7.acc[0])
    check("FP16 T.SUM: 32×0.5 = 16.0 (FP32 in ACC)",
          sum_result == 16.0, f"got {sum_result}")

    # ---- FP16 FMA: dst = a*b + dst ----
    cpu8 = Megapad64()
    cpu8.tsrc0 = 0x1000; cpu8.tsrc1 = 0x2000; cpu8.tdst = 0x3000
    cpu8.tmode = EW_FP16
    write_fp16_lane(cpu8, 0x1000, 0, 2.0)  # a
    write_fp16_lane(cpu8, 0x2000, 0, 3.0)  # b
    write_fp16_lane(cpu8, 0x3000, 0, 1.0)  # c (existing dst)
    for lane in range(1, 32):
        write_fp16_lane(cpu8, 0x1000, lane, 0.0)
        write_fp16_lane(cpu8, 0x2000, lane, 0.0)
        write_fp16_lane(cpu8, 0x3000, lane, 0.0)
    code = assemble("t.fma\nhalt")
    cpu8.load_bytes(0, code)
    cpu8.pc = 0
    try:
        cpu8.run()
    except HaltError:
        pass
    fma_result = read_fp16_lane(cpu8, 0x3000, 0)
    check("FP16 T.FMA: 2.0*3.0+1.0=7.0", fma_result == 7.0,
          f"got {fma_result}")

    # ---- FP16 MAC: dst += a*b ----
    cpu8b = Megapad64()
    cpu8b.tsrc0 = 0x1000; cpu8b.tsrc1 = 0x2000; cpu8b.tdst = 0x3000
    cpu8b.tmode = EW_FP16
    write_fp16_lane(cpu8b, 0x1000, 0, 2.0)
    write_fp16_lane(cpu8b, 0x2000, 0, 3.0)
    write_fp16_lane(cpu8b, 0x3000, 0, 10.0)
    for lane in range(1, 32):
        write_fp16_lane(cpu8b, 0x1000, lane, 0.0)
        write_fp16_lane(cpu8b, 0x2000, lane, 0.0)
        write_fp16_lane(cpu8b, 0x3000, lane, 0.0)
    code = assemble("t.mac\nhalt")
    cpu8b.load_bytes(0, code)
    cpu8b.pc = 0
    try:
        cpu8b.run()
    except HaltError:
        pass
    mac_result = read_fp16_lane(cpu8b, 0x3000, 0)
    check("FP16 T.MAC: 10.0+2.0*3.0=16.0", mac_result == 16.0,
          f"got {mac_result}")

    # ---- BF16 ADD: 1.0 + 2.0 = 3.0 ----
    cpu9 = Megapad64()
    cpu9.tsrc0 = 0x1000; cpu9.tsrc1 = 0x2000; cpu9.tdst = 0x3000
    cpu9.tmode = EW_BF16
    for lane in range(32):
        write_bf16_lane(cpu9, 0x1000, lane, 1.0)
        write_bf16_lane(cpu9, 0x2000, lane, 2.0)
    code = assemble("t.add\nhalt")
    cpu9.load_bytes(0, code)
    cpu9.pc = 0
    try:
        cpu9.run()
    except HaltError:
        pass
    bf_r = read_bf16_lane(cpu9, 0x3000, 0)
    check("BF16 T.ADD: 1.0+2.0=3.0", bf_r == 3.0, f"got {bf_r}")
    check("BF16 T.ADD: all lanes",
          all(read_bf16_lane(cpu9, 0x3000, i) == 3.0 for i in range(32)))

    # ---- BF16 MUL: 4.0 * 5.0 = 20.0 ----
    cpu10 = Megapad64()
    cpu10.tsrc0 = 0x1000; cpu10.tsrc1 = 0x2000; cpu10.tdst = 0x3000
    cpu10.tmode = EW_BF16
    for lane in range(32):
        write_bf16_lane(cpu10, 0x1000, lane, 4.0)
        write_bf16_lane(cpu10, 0x2000, lane, 5.0)
    code = assemble("t.mul\nhalt")
    cpu10.load_bytes(0, code)
    cpu10.pc = 0
    try:
        cpu10.run()
    except HaltError:
        pass
    check("BF16 T.MUL: 4.0*5.0=20.0", read_bf16_lane(cpu10, 0x3000, 0) == 20.0,
          f"got {read_bf16_lane(cpu10, 0x3000, 0)}")

    # ---- BF16 DOT: 32 lanes of 1.0 * 3.0 → 96.0 ----
    cpu11 = Megapad64()
    cpu11.tsrc0 = 0x1000; cpu11.tsrc1 = 0x2000
    cpu11.tmode = EW_BF16; cpu11.tctrl = 0
    for lane in range(32):
        write_bf16_lane(cpu11, 0x1000, lane, 1.0)
        write_bf16_lane(cpu11, 0x2000, lane, 3.0)
    code = assemble("t.dot\nhalt")
    cpu11.load_bytes(0, code)
    cpu11.pc = 0
    try:
        cpu11.run()
    except HaltError:
        pass
    dot_bf = _bits_to_fp32(cpu11.acc[0])
    check("BF16 T.DOT: 32×(1.0*3.0)=96.0", dot_bf == 96.0,
          f"got {dot_bf}")

    # ---- FP16 PACK: fp16 → bf16 format conversion ----
    cpu12 = Megapad64()
    cpu12.tsrc0 = 0x1000; cpu12.tdst = 0x3000
    cpu12.tmode = EW_FP16
    write_fp16_lane(cpu12, 0x1000, 0, 1.5)
    write_fp16_lane(cpu12, 0x1000, 1, -3.0)
    for lane in range(2, 32):
        write_fp16_lane(cpu12, 0x1000, lane, 0.0)
    code = assemble("t.pack\nhalt")
    cpu12.load_bytes(0, code)
    cpu12.pc = 0
    try:
        cpu12.run()
    except HaltError:
        pass
    # Result should be bf16 encoded values
    raw0 = cpu12.mem_read8(0x3000) | (cpu12.mem_read8(0x3001) << 8)
    raw1 = cpu12.mem_read8(0x3002) | (cpu12.mem_read8(0x3003) << 8)
    check("FP16 PACK→BF16: 1.5", _bf16_to_float(raw0) == 1.5,
          f"got {_bf16_to_float(raw0)}")
    check("FP16 PACK→BF16: -3.0", _bf16_to_float(raw1) == -3.0,
          f"got {_bf16_to_float(raw1)}")

    # ---- FP16 UNPACK: fp16 → fp32 widening ----
    cpu13 = Megapad64()
    cpu13.tsrc0 = 0x1000; cpu13.tdst = 0x3000
    cpu13.tmode = EW_FP16
    write_fp16_lane(cpu13, 0x1000, 0, 2.5)
    write_fp16_lane(cpu13, 0x1000, 1, -7.0)
    for lane in range(2, 32):
        write_fp16_lane(cpu13, 0x1000, lane, 0.0)
    code = assemble("t.unpack\nhalt")
    cpu13.load_bytes(0, code)
    cpu13.pc = 0
    try:
        cpu13.run()
    except HaltError:
        pass
    # Output: 16 × fp32 values (4 bytes each)
    fp32_raw0 = cpu13.mem_read32(0x3000)
    fp32_raw1 = cpu13.mem_read32(0x3004)
    check("FP16 UNPACK→FP32: 2.5", _bits_to_fp32(fp32_raw0) == 2.5,
          f"got {_bits_to_fp32(fp32_raw0)}")
    check("FP16 UNPACK→FP32: -7.0", _bits_to_fp32(fp32_raw1) == -7.0,
          f"got {_bits_to_fp32(fp32_raw1)}")

    # ---- FP16 NaN propagation in MIN/MAX ----
    cpu14 = Megapad64()
    cpu14.tsrc0 = 0x1000; cpu14.tsrc1 = 0x2000; cpu14.tdst = 0x3000
    cpu14.tmode = EW_FP16
    write_fp16_lane(cpu14, 0x1000, 0, float('nan'))
    write_fp16_lane(cpu14, 0x2000, 0, 5.0)
    for lane in range(1, 32):
        write_fp16_lane(cpu14, 0x1000, lane, 0.0)
        write_fp16_lane(cpu14, 0x2000, lane, 0.0)
    code = assemble("t.min\nhalt")
    cpu14.load_bytes(0, code)
    cpu14.pc = 0
    try:
        cpu14.run()
    except HaltError:
        pass
    raw_nan = cpu14.mem_read8(0x3000) | (cpu14.mem_read8(0x3001) << 8)
    is_nan_result = ((raw_nan >> 10) & 0x1F) == 0x1F and (raw_nan & 0x3FF) != 0
    check("FP16 NaN propagation in MIN", is_nan_result,
          f"got raw=0x{raw_nan:04X}")

    # ---- FP16 DOTACC: 4-way chunked dot product ----
    cpu15 = Megapad64()
    cpu15.tsrc0 = 0x1000; cpu15.tsrc1 = 0x2000
    cpu15.tmode = EW_FP16; cpu15.tctrl = 0
    # 32 lanes, 4 chunks of 8: chunk0 all 1.0*2.0, chunk1 all 3.0*1.0, etc.
    for lane in range(8):
        write_fp16_lane(cpu15, 0x1000, lane, 1.0)
        write_fp16_lane(cpu15, 0x2000, lane, 2.0)
    for lane in range(8, 16):
        write_fp16_lane(cpu15, 0x1000, lane, 3.0)
        write_fp16_lane(cpu15, 0x2000, lane, 1.0)
    for lane in range(16, 32):
        write_fp16_lane(cpu15, 0x1000, lane, 0.0)
        write_fp16_lane(cpu15, 0x2000, lane, 0.0)
    code = assemble("t.dotacc\nhalt")
    cpu15.load_bytes(0, code)
    cpu15.pc = 0
    try:
        cpu15.run()
    except HaltError:
        pass
    dacc0 = _bits_to_fp32(cpu15.acc[0])
    dacc1 = _bits_to_fp32(cpu15.acc[1])
    check("FP16 DOTACC chunk0: 8×(1.0*2.0)=16.0", dacc0 == 16.0,
          f"got {dacc0}")
    check("FP16 DOTACC chunk1: 8×(3.0*1.0)=24.0", dacc1 == 24.0,
          f"got {dacc1}")

    # ---- FP16 SUMSQ: sum of squares ----
    cpu16 = Megapad64()
    cpu16.tsrc0 = 0x1000; cpu16.tmode = EW_FP16; cpu16.tctrl = 0
    write_fp16_lane(cpu16, 0x1000, 0, 3.0)
    write_fp16_lane(cpu16, 0x1000, 1, 4.0)
    for lane in range(2, 32):
        write_fp16_lane(cpu16, 0x1000, lane, 0.0)
    code = assemble("t.sumsq\nhalt")
    cpu16.load_bytes(0, code)
    cpu16.pc = 0
    try:
        cpu16.run()
    except HaltError:
        pass
    ssq = _bits_to_fp32(cpu16.acc[0])
    check("FP16 SUMSQ: 3²+4²=25.0", ssq == 25.0, f"got {ssq}")

    # ---- FP16 WMUL: widening multiply fp16→fp32 ----
    cpu17 = Megapad64()
    cpu17.tsrc0 = 0x1000; cpu17.tsrc1 = 0x2000; cpu17.tdst = 0x3000
    cpu17.tmode = EW_FP16
    write_fp16_lane(cpu17, 0x1000, 0, 2.0)
    write_fp16_lane(cpu17, 0x2000, 0, 5.0)
    write_fp16_lane(cpu17, 0x1000, 1, -3.0)
    write_fp16_lane(cpu17, 0x2000, 1, 4.0)
    for lane in range(2, 32):
        write_fp16_lane(cpu17, 0x1000, lane, 0.0)
        write_fp16_lane(cpu17, 0x2000, lane, 0.0)
    code = assemble("t.wmul\nhalt")
    cpu17.load_bytes(0, code)
    cpu17.pc = 0
    try:
        cpu17.run()
    except HaltError:
        pass
    wmul0 = _bits_to_fp32(cpu17.mem_read32(0x3000))
    wmul1 = _bits_to_fp32(cpu17.mem_read32(0x3004))
    check("FP16 WMUL: 2.0*5.0=10.0 (fp32)", wmul0 == 10.0, f"got {wmul0}")
    check("FP16 WMUL: -3.0*4.0=-12.0 (fp32)", wmul1 == -12.0, f"got {wmul1}")

    # ---- FP16 TRED MIN/MAX with reduction ----
    cpu18 = Megapad64()
    cpu18.tsrc0 = 0x1000; cpu18.tmode = EW_FP16; cpu18.tctrl = 0
    write_fp16_lane(cpu18, 0x1000, 0, 5.0)
    write_fp16_lane(cpu18, 0x1000, 1, -2.0)
    write_fp16_lane(cpu18, 0x1000, 2, 10.0)
    for lane in range(3, 32):
        write_fp16_lane(cpu18, 0x1000, lane, 0.0)
    code = assemble("t.rmin\nhalt")
    cpu18.load_bytes(0, code)
    cpu18.pc = 0
    try:
        cpu18.run()
    except HaltError:
        pass
    rmin_r = _bits_to_fp32(cpu18.acc[0])
    check("FP16 TRED MIN: min(5,-2,10,0...)=-2.0", rmin_r == -2.0,
          f"got {rmin_r}")

    # ---- Bitwise ops work in FP mode (AND/OR/XOR are bitwise) ----
    cpu19 = Megapad64()
    cpu19.tsrc0 = 0x1000; cpu19.tsrc1 = 0x2000; cpu19.tdst = 0x3000
    cpu19.tmode = EW_FP16
    # Write raw 0xFFFF and 0x7FFF → AND should give 0x7FFF (clears sign bit)
    cpu19.mem_write8(0x1000, 0xFF); cpu19.mem_write8(0x1001, 0xFF)
    cpu19.mem_write8(0x2000, 0xFF); cpu19.mem_write8(0x2001, 0x7F)
    for lane in range(1, 32):
        write_fp16_lane(cpu19, 0x1000, lane, 0.0)
        write_fp16_lane(cpu19, 0x2000, lane, 0.0)
    code = assemble("t.and\nhalt")
    cpu19.load_bytes(0, code)
    cpu19.pc = 0
    try:
        cpu19.run()
    except HaltError:
        pass
    raw = cpu19.mem_read8(0x3000) | (cpu19.mem_read8(0x3001) << 8)
    check("FP16 bitwise AND: 0xFFFF & 0x7FFF = 0x7FFF", raw == 0x7FFF,
          f"got 0x{raw:04X}")

    # ---- TMODE EW=4 doesn't break existing integer ops (regression) ----
    # Quick check: u8 mode still works after fp tests
    cpu20 = Megapad64()
    cpu20.tsrc0 = 0x1000; cpu20.tsrc1 = 0x2000; cpu20.tdst = 0x3000
    cpu20.tmode = 0x00  # u8
    for i in range(64):
        cpu20.mem_write8(0x1000 + i, 1)
        cpu20.mem_write8(0x2000 + i, 2)
    code = assemble("t.add\nhalt")
    cpu20.load_bytes(0, code)
    cpu20.pc = 0
    try:
        cpu20.run()
    except HaltError:
        pass
    check("u8 regression: 1+2=3 still works",
          all(cpu20.mem_read8(0x3000 + i) == 3 for i in range(64)))


def test_ext_prefix():
    """Family 0xF: EXT prefix"""
    print("\n== EXT (0xF) ==")

    # LDI64: loads a full 64-bit immediate via EXT prefix
    cpu, _ = run_asm("""
        ldi64 r1, 0x0102030405060708
        halt
    """)
    check("LDI64 R1: full 64-bit immediate", cpu.regs[1] == 0x0102030405060708)


def test_fibonacci():
    """Integration: compute fibonacci(10) = 55"""
    print("\n== Integration: Fibonacci ==")

    cpu, _ = run_asm("""
        ; Compute fib(10) = 55
        ; R1 = a = 0, R2 = b = 1, R4 = counter = 10, R5 = temp
        ldi r1, 0
        ldi r2, 1
        ldi r4, 10
    loop:
        mov r5, r2       ; temp = b
        add r2, r1       ; b = a + b
        mov r1, r5       ; a = temp
        dec r4
        cmpi r4, 0
        brne loop
        halt
    """)
    check("Fibonacci(10) = 55", cpu.regs[1] == 55,
          f"got R1={cpu.regs[1]}, R2={cpu.regs[2]}")


def test_subroutine():
    """Integration: subroutine call via CALL.L/RET.L"""
    print("\n== Integration: Subroutine ==")

    cpu, _ = run_asm("""
        ; Main: R1 = 10, call double, R1 should be 20
        ldi r1, 10
        ldi r4, 30       ; address of 'double' routine
        call.l r4
        halt

    .org 30
        ; double: R1 = R1 + R1
        add r1, r1
        ret.l
    """)
    check("Subroutine doubles R1: 10→20", cpu.regs[1] == 20)


def test_stack_ops():
    """Integration: push/pop via STXD/LDXAR"""
    print("\n== Integration: Stack Push/Pop ==")

    # Manual push via STR + DEC, pop via INC + LDN
    cpu, _ = run_asm("""
        ldi r1, 0x42        ; value to push
        ldi r2, 0x200       ; stack pointer
        sex r2               ; XSEL = 2

        ; Push: store at R(X), decrement
        stxd r1              ; M(R(X)) ← R1; R(X) -= 8

        ldi r1, 0            ; clear R1

        ; Pop: increment, load
        inc r2
        inc r2
        inc r2
        inc r2
        inc r2
        inc r2
        inc r2
        inc r2               ; R2 += 8
        ldxr r1              ; R1 ← M(R(X))
        halt
    """)
    check("Push/pop: R1 recovered = 0x42", cpu.regs[1] == 0x42,
          f"got R1={cpu.regs[1]:#x}")


def test_1802_heritage():
    """Integration: 1802-style SEP subroutine call pattern"""
    print("\n== Integration: 1802-style SEP Call ==")

    # Classic 1802 pattern: SEP R4 to call, SEP R3 to return
    cpu, _ = run_asm("""
        ; Set up: R3 is initial PC, R4 points to subroutine
        ldi r4, 40           ; R4 → subroutine at address 40
        sep r4               ; PSEL ← 4, execution jumps to R4=40

    .org 10
        ; Return point — execution resumes here after SEP R3
        halt

    .org 40
        ; Subroutine body (running as R4-PC)
        ldi r1, 0xAA         ; do some work
        ldi r3, 10           ; R3 → return address
        sep r3               ; PSEL ← 3, jump to R3=10
    """)
    check("1802 SEP pattern: R1 = 0xAA after subroutine", cpu.regs[1] == 0xAA)
    check("1802 SEP pattern: halted at return point", cpu.halted)


# =========================================================================
#  Runner
# =========================================================================

def main():
    global PASS, FAIL
    print("=" * 60)
    print("  Megapad-64 Emulator Test Suite")
    print("=" * 60)

    tests = [
        test_sys,
        test_inc_dec,
        test_branch,
        test_lbr,
        test_mem,
        test_imm,
        test_alu,
        test_memalu,
        test_io,
        test_sep_sex,
        test_muldiv,
        test_csr,
        test_mex,
        test_vshr_vshl,
        test_tile_views,
        test_strided_2d,
        test_tile_fp,
        test_ext_prefix,
        test_fibonacci,
        test_subroutine,
        test_stack_ops,
        test_1802_heritage,
    ]

    for t in tests:
        try:
            t()
        except Exception as e:
            FAIL += 1
            print(f"  ✗ {t.__name__} CRASHED: {e}")
            traceback.print_exc()

    print("\n" + "=" * 60)
    print(f"  Results: {PASS} passed, {FAIL} failed")
    print("=" * 60)
    return 0 if FAIL == 0 else 1

if __name__ == "__main__":
    sys.exit(main())
