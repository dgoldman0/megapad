#!/usr/bin/env python3
"""Benchmark: JIT speedup on primitive-heavy Forth workloads.

Compiles and runs tight Forth loops that heavily use JIT-inlined
primitives (DUP, DROP, SWAP, OVER, +, -, @, !, XOR, AND, OR, CELLS,
CELL+, NEGATE, INVERT, NIP, 2DROP).  Compares step counts JIT OFF vs ON.
"""

import time
from asm import assemble
from system import MegapadSystem

BIOS_PATH = "bios.asm"
MAX_STEPS = 500_000_000


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


# ---------------------------------------------------------------------------
# Forth benchmark programs — designed to exercise JIT-inlined primitives
# ---------------------------------------------------------------------------

BENCHMARKS = {
    "fib-loop": {
        "desc": "Fibonacci via OVER + SWAP in a 50k-iteration loop",
        "code": [
            ": BENCH-FIB  0 1  50000 0 DO  OVER + SWAP  LOOP  2DROP ;",
            "BENCH-FIB",
        ],
    },
    "stack-churn": {
        "desc": "DUP OVER SWAP NIP DROP in a 50k-iteration loop",
        "code": [
            ": BENCH-STACK  1 2  50000 0 DO  DUP OVER SWAP NIP DROP  LOOP  2DROP ;",
            "BENCH-STACK",
        ],
    },
    "alu-mix": {
        "desc": "XOR AND OR INVERT NEGATE on stack, 50k iterations",
        "code": [
            ": BENCH-ALU  $DEADBEEF $CAFEBABE  50000 0 DO",
            "    OVER OVER XOR INVERT NEGATE DROP",
            "  LOOP  2DROP ;",
            "BENCH-ALU",
        ],
    },
    "array-fill": {
        "desc": "Fill 1000-cell array with @, !, +, CELL+, CELLS",
        "code": [
            "CREATE BUF 1000 CELLS ALLOT",
            ": FILL-BUF  ( -- )  1000 0 DO  I  BUF I CELLS +  !  LOOP ;",
            ": SUM-BUF   ( -- n )  0  1000 0 DO  BUF I CELLS + @  +  LOOP ;",
            ": BENCH-ARR  50 0 DO  FILL-BUF SUM-BUF DROP  LOOP ;",
            "BENCH-ARR",
        ],
    },
    "double-add": {
        "desc": "Trivial DUP + (pure inline), 100k iterations",
        "code": [
            ": DOUBLE  DUP + ;",
            ": BENCH-DBL  1  100000 0 DO  DOUBLE  LOOP  DROP ;",
            "BENCH-DBL",
        ],
    },
    "nested-prims": {
        "desc": "Fibonacci step: OVER + SWAP, 50k iterations",
        "code": [
            ": CRUNCH  ( a b -- b a+b )  OVER + SWAP ;",
            ": BENCH-NEST  0 1  50000 0 DO  CRUNCH  LOOP  2DROP ;",
            "BENCH-NEST",
        ],
    },
}


def run_bench(bios_code, bench_lines, jit_on):
    """Boot BIOS, optionally JIT-ON, compile + run Forth, return steps."""
    sys_obj = make_system(ram_kib=256)
    buf = capture_uart(sys_obj)
    sys_obj.load_binary(0, bios_code)
    sys_obj.boot()

    preamble = "JIT-ON\n" if jit_on else ""
    payload = preamble + "\n".join(bench_lines) + "\nBYE\n"
    data = payload.encode()
    pos = 0
    total = 0

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

    text = uart_text(buf)
    ok = sys_obj.cpu.halted and pos >= len(data)
    return total, text, ok


def main():
    print("Loading BIOS...")
    with open(BIOS_PATH) as f:
        bios_code = assemble(f.read())
    print(f"  {len(bios_code)} bytes\n")

    hdr = f"{'Benchmark':<16} {'JIT OFF':>14} {'JIT ON':>14} {'Saved':>14} {'Speedup':>8}"
    print(hdr)
    print("-" * len(hdr))

    for name, bench in BENCHMARKS.items():
        steps_off, text_off, ok_off = run_bench(bios_code, bench["code"], jit_on=False)
        steps_on, text_on, ok_on = run_bench(bios_code, bench["code"], jit_on=True)

        saved = steps_off - steps_on
        speedup = steps_off / steps_on if steps_on > 0 else 0
        status = "" if (ok_off and ok_on) else " *ERR*"

        print(f"{name:<16} {steps_off:>14,} {steps_on:>14,} {saved:>+14,} {speedup:>7.3f}x{status}")

    print()
    print("Detailed results:")
    print("=" * 60)
    for name, bench in BENCHMARKS.items():
        print(f"\n  {name}: {bench['desc']}")


if __name__ == "__main__":
    main()
