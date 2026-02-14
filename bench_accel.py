#!/usr/bin/env python3
"""
bench_accel.py — Compare Python vs C++ accelerator performance
==============================================================

Runs a tight CPU loop (simple ALU + branch) under both backends and
reports instructions/second.

Usage:
    python bench_accel.py          # auto-detect available backends
    python bench_accel.py --steps 5000000
"""

import argparse
import time
import sys


def make_bench_program() -> bytes:
    """
    Build a small benchmark kernel:
        LDI R4, 0          ; R4 = counter
        LDI R5, 1          ; R5 = increment
    loop:
        ADD R4, R5          ; R4 += R5
        BR.AL loop          ; unconditional branch back
    """
    prog = bytearray()

    # LDI R4, 0   → family=0x6 n=0x0, byte1=(R4<<4)=0x40, imm8=0x00
    prog += bytes([0x60, 0x40, 0x00])

    # LDI R5, 1   → 0x60, 0x50, 0x01
    prog += bytes([0x60, 0x50, 0x01])

    # ADD R4, R5  → family=0x7 n=0x0, byte1=(R4<<4|R5)=0x45
    add_offset = len(prog)
    prog += bytes([0x70, 0x45])

    # BR.AL -2    → family=0x3 n=0x0 (AL), offset = -2 (back to ADD)
    prog += bytes([0x30, 0xFE])  # -2 as signed 8-bit = 0xFE

    return bytes(prog), add_offset


def run_python(steps: int) -> float:
    """Run pure-Python Megapad64 for N steps, return elapsed seconds."""
    from megapad64 import Megapad64
    prog, _ = make_bench_program()
    cpu = Megapad64()
    for i, b in enumerate(prog):
        cpu.mem[i] = b

    t0 = time.perf_counter()
    for _ in range(steps):
        cpu.step()
    t1 = time.perf_counter()
    return t1 - t0


def run_accel(steps: int) -> float:
    """Run C++ accelerated Megapad64 for N steps, return elapsed seconds."""
    from accel_wrapper import Megapad64, ACCEL_AVAILABLE
    if not ACCEL_AVAILABLE:
        return -1.0

    prog, _ = make_bench_program()
    cpu = Megapad64()
    for i, b in enumerate(prog):
        cpu.mem[i] = b

    t0 = time.perf_counter()
    cpu.run_steps(steps)
    t1 = time.perf_counter()
    return t1 - t0


def run_accel_perstep(steps: int) -> float:
    """Run C++ accelerated Megapad64 with per-step calls (overhead test)."""
    from accel_wrapper import Megapad64, ACCEL_AVAILABLE
    if not ACCEL_AVAILABLE:
        return -1.0

    prog, _ = make_bench_program()
    cpu = Megapad64()
    for i, b in enumerate(prog):
        cpu.mem[i] = b

    t0 = time.perf_counter()
    for _ in range(steps):
        cpu.step()
    t1 = time.perf_counter()
    return t1 - t0


def main():
    parser = argparse.ArgumentParser(description="Megapad-64 benchmark")
    parser.add_argument("--steps", type=int, default=1_000_000,
                        help="Number of steps to execute (default: 1M)")
    args = parser.parse_args()

    steps = args.steps
    print(f"Benchmark: {steps:,} steps")
    print(f"Program: LDI, LDI, then tight ADD+BR loop")
    print()

    # Python backend
    print("Running pure-Python backend...", end=" ", flush=True)
    t_py = run_python(steps)
    ips_py = steps / t_py
    print(f"{t_py:.3f}s  ({ips_py:,.0f} steps/s)")

    # C++ backend — batch mode (real acceleration)
    try:
        from accel_wrapper import ACCEL_AVAILABLE
        if ACCEL_AVAILABLE:
            print("Running C++ batch run_steps()...", end=" ", flush=True)
            t_cpp = run_accel(steps)
            ips_cpp = steps / t_cpp
            print(f"{t_cpp:.3f}s  ({ips_cpp:,.0f} steps/s)")

            print("Running C++ per-step (overhead test)...", end=" ", flush=True)
            t_cpp_ps = run_accel_perstep(steps)
            ips_cpp_ps = steps / t_cpp_ps
            print(f"{t_cpp_ps:.3f}s  ({ips_cpp_ps:,.0f} steps/s)")

            print()
            speedup_batch = t_py / t_cpp if t_cpp > 0 else float("inf")
            speedup_step = t_py / t_cpp_ps if t_cpp_ps > 0 else float("inf")
            print(f"Speedup (batch run_steps):  {speedup_batch:.1f}×")
            print(f"Speedup (per-step call):    {speedup_step:.1f}×")
        else:
            print("C++ accelerator not available — build with `make accel`")
    except ImportError:
        print("C++ accelerator not available — build with `make accel`")


if __name__ == "__main__":
    main()
