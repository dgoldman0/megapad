#!/usr/bin/env python3
"""Start a shared MegaPad semantic-simulator session from an MP64FS image."""

from __future__ import annotations

import argparse
import signal
from dataclasses import dataclass
from pathlib import Path

from session_server import _retained_policy, _rich_terminal_policy
from shared_session import DEFAULT_SOCKET, SessionServer
from simulator.image_bootstrap import (
    ImageBootstrapPreparation,
    prepare_image_bootstrap,
)
from simulator.platform import create_one_core_address_space
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine
from simulator.storage import HostedStorageService


_CANONICAL_HBW_MIB = 3


@dataclass(frozen=True, slots=True)
class PreparedSimulatorServer:
    """One prepared image, semantic machine facade, and server authority."""

    preparation: ImageBootstrapPreparation
    machine: SimulatorSharedMachine
    server: SessionServer


def _positive_int(value: str) -> int:
    try:
        parsed = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("must be an integer") from exc
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be positive")
    return parsed


def _nonnegative_int(value: str) -> int:
    try:
        parsed = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("must be an integer") from exc
    if parsed < 0:
        raise argparse.ArgumentTypeError("must not be negative")
    return parsed


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run a shared MegaPad semantic-simulator session"
    )
    parser.add_argument("--storage", type=Path, required=True)
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--ram-kib", type=_positive_int, default=1024)
    parser.add_argument("--ext-mem-mib", type=_nonnegative_int, default=128)
    parser.add_argument("--vram-mib", type=_nonnegative_int, default=4)
    parser.add_argument("--cols", type=_positive_int, default=80)
    parser.add_argument("--rows", type=_positive_int, default=30)
    parser.add_argument(
        "--semantic-step-budget",
        type=_positive_int,
        help="optional shared budget for each resumable semantic dispatch",
    )
    parser.add_argument(
        "--rich-terminal-policy",
        type=_rich_terminal_policy,
        metavar="JSON",
        help=(
            "attach the optional rich terminal with the complete "
            "caller-owned JSON policy"
        ),
    )
    parser.add_argument(
        "--retained-terminal-policy",
        type=_retained_policy,
        metavar="JSON",
        help=(
            "enable RETAINED-1 with the complete caller-owned JSON policy; "
            "requires --rich-terminal-policy"
        ),
    )
    parser.add_argument("--paused", action="store_true")
    return parser


def prepare_server(args: argparse.Namespace) -> PreparedSimulatorServer:
    """Build, but do not start, one server from validated CLI arguments."""

    if (
        args.retained_terminal_policy is not None
        and args.rich_terminal_policy is None
    ):
        raise ValueError(
            "--retained-terminal-policy requires --rich-terminal-policy"
        )
    storage_path = args.storage.resolve()
    if not storage_path.is_file():
        raise ValueError(f"storage image does not exist: {storage_path}")

    rich_terminal = None
    if args.rich_terminal_policy is not None:
        rich_terminal = args.rich_terminal_policy.configuration(
            args.cols,
            args.rows,
            retained_policy=args.retained_terminal_policy,
        )

    memory = create_one_core_address_space(
        bank0_size=args.ram_kib << 10,
        external_size=args.ext_mem_mib << 20,
        vram_size=args.vram_mib << 20,
        hbw_size=_CANONICAL_HBW_MIB << 20,
    )
    storage = HostedStorageService(image_path=storage_path)
    preparation = prepare_image_bootstrap(memory=memory, storage=storage)
    session = SimulatorMachineSession(
        preparation.runtime,
        preparation.root_xt,
        cols=args.cols,
        rows=args.rows,
        semantic_step_budget=args.semantic_step_budget,
        rich_terminal=rich_terminal,
    )
    machine = SimulatorSharedMachine(session)
    machine.paused = args.paused
    return PreparedSimulatorServer(
        preparation=preparation,
        machine=machine,
        server=SessionServer(machine, args.socket),
    )


def main(argv: list[str] | None = None) -> int:
    parser = build_argument_parser()
    args = parser.parse_args(argv)
    try:
        prepared = prepare_server(args)
    except (OSError, RuntimeError, TypeError, ValueError) as exc:
        parser.error(str(exc))

    server = prepared.server

    def stop(_signum=None, _frame=None) -> None:
        server.stop()

    signal.signal(signal.SIGINT, stop)
    signal.signal(signal.SIGTERM, stop)
    try:
        server.start()
        preparation = prepared.preparation
        print(f"[shared] socket:  {server.socket_path}", flush=True)
        print("[shared] backend: simulator", flush=True)
        print(f"[shared] image:   {args.storage.resolve()}", flush=True)
        print(
            f"[shared] boot:    "
            f"{preparation.boot_filename.decode('ascii', errors='replace')}",
            flush=True,
        )
        print(
            f"[shared] prepare: {preparation.preparation_semantic_steps} "
            "semantic steps",
            flush=True,
        )
        print(
            "[shared] source acceleration: "
            + (
                ", ".join(
                    name.decode("ascii", errors="replace")
                    for name in preparation.source_accelerators
                )
                or "none (ordinary colon dispatch)"
            ),
            flush=True,
        )
        print(
            "[shared] terminal: "
            + (
                (
                    "APT-1 + RETAINED-1 optional attachment"
                    if args.retained_terminal_policy is not None
                    else "APT-1 optional attachment"
                )
                if args.rich_terminal_policy is not None
                else "ANSI"
            ),
            flush=True,
        )
        print("[shared] machine owner running; Ctrl+C stops it", flush=True)
        server.serve_forever()
    finally:
        server.stop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = [
    "PreparedSimulatorServer",
    "build_argument_parser",
    "main",
    "prepare_server",
]
