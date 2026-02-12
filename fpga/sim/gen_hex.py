#!/usr/bin/env python3
"""Generate hex files for FPGA simulation from binary images."""
import sys, os

def bin_to_hex(inpath, outpath, width=64):
    """Convert binary file to $readmemh-compatible hex.
    
    width: bytes per line (64 = 512-bit rows for mem[0:16383])
    """
    with open(inpath, 'rb') as f:
        data = f.read()
    
    with open(outpath, 'w') as f:
        f.write(f"// Generated from {os.path.basename(inpath)} ({len(data)} bytes)\n")
        for i in range(0, len(data), width):
            chunk = data[i:i+width].ljust(width, b'\x00')
            # Little-endian: LSB first in memory, but hex file is MSB-first per word
            val = int.from_bytes(chunk, 'little')
            f.write(f"{val:0{width*2}x}\n")
        # Pad remaining to fill expected depth if needed
    
    lines = (len(data) + width - 1) // width
    print(f"{inpath} -> {outpath}: {len(data)} bytes, {lines} words")

def make_smoke_nop(outpath):
    """16 NOPs then HALT — verifies PC increments."""
    prog = bytes([0x01] * 15 + [0x02])  # 15x NOP + HALT
    with open(outpath, 'w') as f:
        f.write("// smoke_nop: 15x NOP + HALT\n")
        for i in range(0, len(prog), 8):
            chunk = prog[i:i+8].ljust(8, b'\x00')
            val = int.from_bytes(chunk, 'little')
            f.write(f"{val:016x}\n")
    print(f"smoke_nop -> {outpath}: {len(prog)} bytes")

def make_smoke_inc(outpath):
    """INC R0 x4, DEC R0, HALT — verifies register ops."""
    prog = bytes([
        0x10,  # INC R0
        0x10,  # INC R0
        0x10,  # INC R0
        0x10,  # INC R0 -> R0=4
        0x20,  # DEC R0 -> R0=3
        0x02,  # HALT
    ])
    with open(outpath, 'w') as f:
        f.write("// smoke_inc: INC R0 x4, DEC R0, HALT\n")
        for i in range(0, len(prog), 8):
            chunk = prog[i:i+8].ljust(8, b'\x00')
            val = int.from_bytes(chunk, 'little')
            f.write(f"{val:016x}\n")
    print(f"smoke_inc -> {outpath}: {len(prog)} bytes")

def make_smoke_alu(outpath):
    """LDI R0,10; LDI R1,20; ADD R0,R1; HALT — tests IMM+ALU."""
    prog = bytes([
        0x60, 0x00, 0x0A,  # LDI R0, 10
        0x60, 0x10, 0x14,  # LDI R1, 20
        0x70, 0x01,        # ADD R0, R1 -> R0=30
        0x02,              # HALT
    ])
    with open(outpath, 'w') as f:
        f.write("// smoke_alu: LDI R0,10; LDI R1,20; ADD R0,R1; HALT\n")
        for i in range(0, len(prog), 8):
            chunk = prog[i:i+8].ljust(8, b'\x00')
            val = int.from_bytes(chunk, 'little')
            f.write(f"{val:016x}\n")
    print(f"smoke_alu -> {outpath}: {len(prog)} bytes")

def make_smoke_branch(outpath):
    """LDI R0,5; LOOP: DEC R0; BR.NE LOOP; HALT."""
    prog = bytes([
        0x60, 0x00, 0x05,  # LDI R0, 5
        # LOOP (offset 3):
        0x20,              # DEC R0
        0x32, 0xFD,        # BR.NE -3 (back to DEC, relative to next PC)
        0x02,              # HALT
    ])
    with open(outpath, 'w') as f:
        f.write("// smoke_branch: count R0 from 5 to 0\n")
        for i in range(0, len(prog), 8):
            chunk = prog[i:i+8].ljust(8, b'\x00')
            val = int.from_bytes(chunk, 'little')
            f.write(f"{val:016x}\n")
    print(f"smoke_branch -> {outpath}: {len(prog)} bytes")

if __name__ == '__main__':
    simdir = os.path.dirname(os.path.abspath(__file__))
    projdir = os.path.abspath(os.path.join(simdir, '..', '..'))
    
    # BIOS hex
    bios = os.path.join(projdir, 'bios.rom')
    if not os.path.exists(bios):
        bios = os.path.join(projdir, 'bios.bin')  # fallback
    if os.path.exists(bios):
        bin_to_hex(bios, os.path.join(simdir, 'bios.hex'))
    
    # Smoke tests
    make_smoke_nop(os.path.join(simdir, 'smoke_nop.hex'))
    make_smoke_inc(os.path.join(simdir, 'smoke_inc.hex'))
    make_smoke_alu(os.path.join(simdir, 'smoke_alu.hex'))
    make_smoke_branch(os.path.join(simdir, 'smoke_branch.hex'))
    
    print("Done.")
