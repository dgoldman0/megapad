#!/usr/bin/env python3
"""Measure native execution with and without UART status polling."""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

from asm import assemble
from system import MegapadSystem


ROOT = Path(__file__).resolve().parent


def boot_bios() -> MegapadSystem:
    labels: dict[str, int] = {}
    code = assemble((ROOT / "bios.asm").read_text(), labels_out=labels)
    system = MegapadSystem(ram_size=1024 * 1024)
    system.load_binary(0, code)
    for name, hook_id in (
        ("w_rect_fill", 1),
        ("w_blit_glyph", 2),
        ("w_vram_copy", 3),
        ("w_blit_string", 4),
    ):
        if name in labels:
            system.cpu.register_accel_hook(labels[name], hook_id)
    system.boot()
    for _ in range(100):
        if system.cpu.idle:
            break
        system.run_batch(100_000)
    if not system.cpu.idle:
        raise RuntimeError("BIOS did not reach the idle REPL")
    system.uart.drain_tx()
    return system


def benchmark_loop(name: str, source: str, steps: int) -> dict:
    system = boot_bios()
    system.uart.inject_input(source + "\n")

    # Enter the infinite loop, then measure a clean native batch.
    system.run_batch(1_000_000)
    if system.cpu.idle or system.cpu.halted:
        raise RuntimeError(f"{name} did not enter its benchmark loop")

    callback_reads = 0
    original_read = system.cpu._mmio_read8

    def counted_read(addr):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(addr)

    system.cpu._mmio_read8 = counted_read
    start = time.perf_counter()
    executed = system.run_batch(steps)
    elapsed = time.perf_counter() - start
    return {
        "name": name,
        "steps": executed,
        "elapsed_s": elapsed,
        "msteps_s": executed / elapsed / 1_000_000,
        "python_mmio_reads": callback_reads,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--steps", type=int, default=50_000_000)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    results = [
        benchmark_loop("spin", ": SPIN BEGIN AGAIN ; SPIN", args.steps),
        benchmark_loop("key_poll", ": POLL BEGIN KEY? DROP AGAIN ; POLL", args.steps),
    ]
    ratio = results[1]["msteps_s"] / results[0]["msteps_s"]
    report = {"results": results, "key_poll_to_spin_ratio": ratio}

    if args.json:
        print(json.dumps(report, indent=2))
    else:
        for result in results:
            print(
                f"{result['name']:>10}: {result['msteps_s']:7.2f} Msteps/s  "
                f"Python MMIO reads: {result['python_mmio_reads']}"
            )
        print(f"poll/spin: {ratio:.3f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
