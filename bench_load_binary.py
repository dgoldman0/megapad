#!/usr/bin/env python3
"""Compare slice-based binary loading with the former byte loop."""

from __future__ import annotations

import argparse
import json
import time

from system import MegapadSystem


def legacy_load(system: MegapadSystem, addr: int, data: bytes):
    """Previous Bank 0 behavior, retained here only as a benchmark oracle."""
    for index, value in enumerate(data):
        system._shared_mem[(addr + index) % system.ram_size] = value


def timed(callable_):
    start = time.perf_counter()
    callable_()
    return time.perf_counter() - start


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mib", type=int, default=8)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    if args.mib <= 0:
        parser.error("--mib must be positive")

    size = args.mib << 20
    payload = bytes(range(256)) * (size // 256)
    legacy = MegapadSystem(ram_size=1 << 20, ext_mem_size=0, vram_size=0)
    current = MegapadSystem(ram_size=1 << 20, ext_mem_size=0, vram_size=0)

    legacy_s = timed(lambda: legacy_load(legacy, 0, payload))
    current_s = timed(lambda: current.load_binary(0, payload))
    if legacy._shared_mem != current._shared_mem:
        raise RuntimeError("legacy and slice loaders produced different Bank 0 data")

    report = {
        "bytes": size,
        "legacy_s": legacy_s,
        "slice_s": current_s,
        "speedup": legacy_s / current_s,
        "slice_gib_s": size / current_s / (1 << 30),
    }
    if args.json:
        print(json.dumps(report, indent=2))
    else:
        print(f"payload: {args.mib} MiB")
        print(f"legacy byte loop: {legacy_s:.6f} s")
        print(f"slice loader:     {current_s:.6f} s")
        print(f"speedup:          {report['speedup']:.1f}x")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
