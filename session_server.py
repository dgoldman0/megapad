#!/usr/bin/env python3
"""Start a long-lived shared MegaPad machine session."""

from __future__ import annotations

import argparse
import signal
from pathlib import Path

from session import MachineSession
from shared_session import DEFAULT_SOCKET, SessionServer, SharedMachine


ROOT = Path(__file__).resolve().parent


def main() -> int:
    parser = argparse.ArgumentParser(description="Run a shared MegaPad session")
    parser.add_argument("--bios", type=Path, default=ROOT / "bios.asm")
    parser.add_argument("--storage", type=Path)
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--ram-kib", type=int, default=1024)
    parser.add_argument("--ext-mem-mib", type=int, default=16)
    parser.add_argument("--vram-mib", type=int, default=4)
    parser.add_argument("--cores", type=int, default=1)
    parser.add_argument("--clusters", type=int, default=0)
    parser.add_argument("--cols", type=int, default=80)
    parser.add_argument("--rows", type=int, default=30)
    parser.add_argument("--batch-steps", type=int, default=100_000)
    parser.add_argument(
        "--nic-tap",
        nargs="?",
        const="mp64tap0",
        help="attach the machine NIC to a preconfigured Linux TAP device",
    )
    parser.add_argument(
        "--virtual-clock",
        action="store_true",
        help="advance the RTC from emulated cycles instead of host wall time",
    )
    parser.add_argument("--paused", action="store_true")
    args = parser.parse_args()

    nic_backend = None
    if args.nic_tap:
        from nic_backends import TAPBackend, tap_available

        if not tap_available(args.nic_tap):
            parser.error(
                f"TAP device {args.nic_tap!r} does not exist or is not accessible"
            )
        nic_backend = TAPBackend(tap_name=args.nic_tap)

    session = MachineSession.from_bios(
        args.bios,
        storage_image=args.storage,
        ram_size=args.ram_kib << 10,
        ext_mem_size=args.ext_mem_mib << 20,
        vram_size=args.vram_mib << 20,
        num_cores=args.cores,
        num_clusters=args.clusters,
        cols=args.cols,
        rows=args.rows,
        batch_steps=args.batch_steps,
        nic_backend=nic_backend,
        realtime_clock=not args.virtual_clock,
    )
    machine = SharedMachine(session)
    machine.paused = args.paused
    server = SessionServer(machine, args.socket)

    def stop(_signum=None, _frame=None):
        server.stop()

    signal.signal(signal.SIGINT, stop)
    signal.signal(signal.SIGTERM, stop)
    server.start()
    print(f"[shared] socket: {server.socket_path}", flush=True)
    print(f"[shared] bios:   {args.bios.resolve()}", flush=True)
    if args.nic_tap:
        print(f"[shared] nic:    tap:{args.nic_tap}", flush=True)
    print(
        f"[shared] clock:  {'virtual' if args.virtual_clock else 'realtime'}",
        flush=True,
    )
    print("[shared] machine owner running; Ctrl+C stops it", flush=True)
    try:
        server.serve_forever()
    finally:
        server.stop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
