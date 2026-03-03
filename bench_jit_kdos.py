#!/usr/bin/env python3
"""Benchmark: KDOS load cycles with JIT OFF vs JIT ON.

Measures exact step counts for loading the full KDOS source into the
Megapad-64 BIOS interpreter, with and without the JIT compiler enabled.
"""

import sys, time
from asm import assemble
from accel_wrapper import Megapad64, HaltError
from system import MegapadSystem

BIOS_PATH = "bios.asm"
KDOS_PATH = "kdos.f"
MAX_STEPS = 2_000_000_000  # 2 billion — very generous ceiling


def make_system(ram_kib=1024, ext_mem_mib=0):
    return MegapadSystem(ram_size=ram_kib * 1024,
                         ext_mem_size=ext_mem_mib * (1 << 20))


def capture_uart(sys_obj):
    buf = []
    sys_obj.uart.on_tx = lambda b: buf.append(b)
    return buf


def uart_text(buf):
    return "".join(
        chr(b) if (0x20 <= b < 0x7F or b in (10, 13, 9)) else ""
        for b in buf
    )


def _next_line_chunk(data: bytes, pos: int) -> bytes:
    nl = data.find(b'\n', pos)
    if nl == -1:
        return data[pos:]
    return data[pos:nl + 1]


def load_kdos(bios_code, kdos_lines, jit_on: bool, label: str):
    """Boot BIOS, optionally enable JIT, load KDOS, return step count."""
    sys_obj = make_system(ram_kib=1024, ext_mem_mib=16)
    buf = capture_uart(sys_obj)
    sys_obj.load_binary(0, bios_code)
    sys_obj.boot()

    # Prepare payload: optionally JIT-ON before KDOS, then KDOS lines
    preamble = "JIT-ON\n" if jit_on else ""
    kdos_payload = "\n".join(kdos_lines) + "\n"
    # After KDOS loads, query JIT stats if JIT was on
    postamble = "JIT-STATS\nBYE\n" if jit_on else "BYE\n"
    
    payload = preamble + kdos_payload + postamble
    data = payload.encode()
    pos = 0
    total = 0

    t0 = time.monotonic()
    while total < MAX_STEPS:
        if sys_obj.cpu.halted:
            break
        if sys_obj.cpu.idle and not sys_obj.uart.has_rx_data:
            if pos < len(data):
                chunk = _next_line_chunk(data, pos)
                sys_obj.uart.inject_input(chunk)
                pos += len(chunk)
            else:
                break
            continue
        batch = sys_obj.run_batch(min(100_000, MAX_STEPS - total))
        total += max(batch, 1)
    elapsed = time.monotonic() - t0

    text = uart_text(buf)
    halted = sys_obj.cpu.halted
    idle = sys_obj.cpu.idle
    fed_all = pos >= len(data)

    print(f"\n{'='*60}")
    print(f"  {label}")
    print(f"{'='*60}")
    print(f"  Steps:      {total:>15,}")
    print(f"  Wall time:  {elapsed:>15.2f} s")
    print(f"  Halted:     {halted}")
    print(f"  Idle:       {idle}")
    print(f"  Input fed:  {fed_all}  ({pos}/{len(data)} bytes)")
    
    # Check for errors in output
    lines = text.splitlines()
    errors = [l for l in lines if '?' in l and 'ok' not in l.lower()]
    if errors:
        print(f"  Errors:     {len(errors)}")
        for e in errors[:5]:
            print(f"    {e[:80]}")
    
    # Show JIT stats if present
    jit_lines = [l for l in lines if 'JIT:' in l]
    for jl in jit_lines:
        print(f"  {jl.strip()}")
    
    print(f"{'='*60}")
    return total, elapsed, halted


def main():
    print("Loading BIOS...")
    with open(BIOS_PATH) as f:
        bios_code = assemble(f.read())
    print(f"  BIOS: {len(bios_code)} bytes")

    print("Loading KDOS source...")
    with open(KDOS_PATH) as f:
        kdos_lines = []
        for line in f.read().splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith('\\'):
                continue
            kdos_lines.append(line)
    print(f"  KDOS: {len(kdos_lines)} lines")

    # --- JIT OFF ---
    steps_off, time_off, halted_off = load_kdos(
        bios_code, kdos_lines, jit_on=False, label="KDOS Load — JIT OFF")

    # --- JIT ON ---
    steps_on, time_on, halted_on = load_kdos(
        bios_code, kdos_lines, jit_on=True, label="KDOS Load — JIT ON")

    # --- Summary ---
    print(f"\n{'='*60}")
    print(f"  COMPARISON")
    print(f"{'='*60}")
    print(f"  JIT OFF steps:  {steps_off:>15,}")
    print(f"  JIT ON  steps:  {steps_on:>15,}")
    if steps_off > 0:
        ratio = steps_on / steps_off
        overhead = steps_on - steps_off
        pct = (overhead / steps_off) * 100
        print(f"  Overhead:       {overhead:>+15,} steps ({pct:+.1f}%)")
        print(f"  Ratio:          {ratio:.3f}x")
    print(f"  JIT OFF time:   {time_off:>15.2f} s")
    print(f"  JIT ON  time:   {time_on:>15.2f} s")
    
    # Check if 400M budget would be enough
    budget = 400_000_000
    print(f"\n  400M budget sufficient?")
    print(f"    JIT OFF: {'YES' if steps_off <= budget else 'NO'} ({steps_off:,} / {budget:,})")
    print(f"    JIT ON:  {'YES' if steps_on <= budget else 'NO'} ({steps_on:,} / {budget:,})")
    
    if steps_on > budget:
        # Suggest new budget
        suggested = int(steps_on * 1.25)  # 25% headroom
        print(f"    Suggested new budget: {suggested:,}")
    
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
