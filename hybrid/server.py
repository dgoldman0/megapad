"""Start a shared semantic session that also runs declared machine routines."""

from __future__ import annotations

import argparse
import signal
import time
from dataclasses import dataclass
from pathlib import Path

from hybrid.manifest import load_manifest
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from shared.session_options import configured_production_executor
from shared_session import SessionServer
from simulator.image_bootstrap import ImageBootstrapPreparation, prepare_image_bootstrap
from simulator.platform import create_one_core_address_space
from simulator.server import build_argument_parser as semantic_argument_parser
from simulator.session import configured_semantic_quantum_steps
from simulator.storage import HostedStorageService


# A host turn runs at most this many machine instructions before the session
# can serve its terminal; an unbounded routine still finishes over many turns.
DEFAULT_MACHINE_QUANTUM_INSTRUCTIONS = 1_000_000


@dataclass(frozen=True, slots=True)
class PreparedHybridServer:
    """One registered routine set, prepared image, and server authority."""

    hybrid: HybridRuntime
    preparation: ImageBootstrapPreparation
    machine: HybridSharedMachine
    server: SessionServer


def _positive(value: str) -> int:
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be a positive integer")
    return number


def build_argument_parser() -> argparse.ArgumentParser:
    parser = semantic_argument_parser(
        description="Run a shared MegaPad hybrid session",
    )
    parser.add_argument(
        "--hybrid-routines",
        type=Path,
        metavar="MANIFEST",
        help="JSON manifest of machine routines to define before the image boots",
    )
    parser.add_argument(
        "--machine-quantum-instructions",
        type=_positive,
        default=DEFAULT_MACHINE_QUANTUM_INSTRUCTIONS,
        metavar="N",
        help="machine instructions per host turn (default: %(default)s)",
    )
    parser.add_argument(
        "--machine-instruction-budget",
        type=_positive,
        metavar="N",
        help="stop a dispatch after this many machine instructions (default: no limit)",
    )
    parser.epilog = (
        "Hybrid mode runs Forth semantically and declared MP64 routines on a native "
        "core that shares its memory and return stack. Routines reach memory through "
        "argument-named buffers and call Forth words at declared sites."
    )
    return parser


def prepare_server(args: argparse.Namespace) -> PreparedHybridServer:
    """Load the routines, define them, then prepare the boot image."""

    if (
        args.retained_terminal_policy is not None
        and args.rich_terminal_policy is None
    ):
        raise ValueError("--retained-terminal-policy requires --rich-terminal-policy")
    storage_path = args.storage.resolve()
    if not storage_path.is_file():
        raise ValueError(f"storage image does not exist: {storage_path}")
    quantum_steps = configured_semantic_quantum_steps(args.semantic_quantum_steps)
    executor = configured_production_executor(args.executor)
    # Every image is read and checked before any runtime or socket exists.
    manifest = None if args.hybrid_routines is None else load_manifest(args.hybrid_routines)
    rich_terminal = None
    if args.rich_terminal_policy is not None:
        rich_terminal = args.rich_terminal_policy.configuration(
            args.cols, args.rows, retained_policy=args.retained_terminal_policy,
        )
    memory = create_one_core_address_space(
        bank0_size=args.ram_kib << 10,
        external_size=args.ext_mem_mib << 20,
        vram_size=args.vram_mib << 20,
        hbw_size=3 << 20,
        initial_epoch_ms=time.time_ns() // 1_000_000,
        dense_backing=True,
    )
    memory.mmio.rtc.bind_monotonic_clock(time.monotonic_ns)
    storage = HostedStorageService(image_path=storage_path)
    hybrid = HybridRuntime.create(
        executor=executor,
        memory=memory,
        storage=storage,
        machine_instruction_budget=args.machine_instruction_budget,
    )
    session = None
    try:
        # Boot source and autoexec can call the routines by name.
        if manifest is not None:
            hybrid.register_manifest(manifest)
        preparation = prepare_image_bootstrap(
            memory=memory,
            storage=storage,
            runtime=hybrid.semantic,
            terminal_cols=args.cols,
            terminal_rows=args.rows,
            semantic_step_budget=args.semantic_step_budget,
        )
        session = HybridSession(
            hybrid,
            preparation.root_xt,
            cols=args.cols,
            rows=args.rows,
            semantic_step_budget=(
                None
                if args.semantic_step_budget is None
                else args.semantic_step_budget - preparation.autoexec_semantic_steps
            ),
            semantic_quantum_steps=quantum_steps,
            machine_quantum_instructions=args.machine_quantum_instructions,
            rich_terminal=rich_terminal,
        )
        machine = HybridSharedMachine(session)
        machine.paused = args.paused
        return PreparedHybridServer(
            hybrid=hybrid,
            preparation=preparation,
            machine=machine,
            server=SessionServer(machine, args.socket),
        )
    except BaseException:
        if session is not None:
            session.close()
        else:
            hybrid.close()
        raise


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
        routines = ", ".join(routine.name for routine in prepared.hybrid.registered_routines)
        print(f"[shared] socket:  {server.socket_path}", flush=True)
        print("[shared] backend: hybrid", flush=True)
        print("[shared] clock:   realtime", flush=True)
        print(f"[shared] image:   {args.storage.resolve()}", flush=True)
        print(f"[shared] routines: {routines or 'none'}", flush=True)
        print(
            f"[shared] execution: {prepared.preparation.runtime.execution_backend} "
            "semantic + MP64 native routine runner",
            flush=True,
        )
        print(
            f"[shared] prepare: {prepared.preparation.preparation_semantic_steps} "
            "semantic steps",
            flush=True,
        )
        print(
            "[shared] quantum: "
            f"{prepared.machine.semantic_session.semantic_quantum_steps} semantic steps, "
            f"{args.machine_quantum_instructions} machine instructions",
            flush=True,
        )
        print("[shared] machine owner running; Ctrl+C stops it", flush=True)
        server.serve_forever()
    finally:
        server.stop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = ["DEFAULT_MACHINE_QUANTUM_INSTRUCTIONS", "PreparedHybridServer",
           "build_argument_parser", "main", "prepare_server"]
