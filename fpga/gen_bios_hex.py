#!/usr/bin/env python3
"""Convert bios.rom (raw binary) → bios.hex ($readmemh format).

Usage:  python fpga/gen_bios_hex.py [bios.rom [fpga/bios.hex]]

Output is one 64-bit hex word per line, suitable for mp64_rom with
DATA_W=64.  Pads the last word with zeros if the binary length isn't
a multiple of 8.
"""
import sys, pathlib

def main():
    src  = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else "bios.rom")
    dst  = pathlib.Path(sys.argv[2] if len(sys.argv) > 2 else "fpga/bios.hex")
    data = src.read_bytes()

    # Pad to 8-byte boundary
    pad = (8 - len(data) % 8) % 8
    data += b'\x00' * pad

    words = len(data) // 8
    with open(dst, 'w') as f:
        f.write(f"// {src.name}: {len(data)-pad} bytes → {words} × 64-bit words\n")
        for i in range(words):
            word = int.from_bytes(data[i*8:(i+1)*8], 'big')
            f.write(f"{word:016X}\n")

    print(f"{src} → {dst}: {words} words ({len(data)} bytes)")

if __name__ == "__main__":
    main()
