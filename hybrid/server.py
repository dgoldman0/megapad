"""Start a shared semantic session with declared bounded MP64 routines."""

from __future__ import annotations

import argparse
import signal
import time
from dataclasses import dataclass
from pathlib import Path

from hybrid.manifest import load_manifest_v1
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from shared_session import SessionServer
from simulator.image_bootstrap import ImageBootstrapPreparation, prepare_image_bootstrap
from simulator.platform import create_one_core_address_space
from simulator.session import configured_semantic_quantum_steps
from simulator.storage import HostedStorageService
from simulator_server import build_argument_parser as semantic_argument_parser


@dataclass(frozen=True, slots=True)
class PreparedHybridServer:
    """One validated routine registry, prepared image, and server authority."""

    hybrid: HybridRuntime
    preparation: ImageBootstrapPreparation
    machine: HybridSharedMachine
    server: SessionServer


def build_argument_parser() -> argparse.ArgumentParser:
    parser = semantic_argument_parser(
        description="Run a shared MegaPad hybrid session",
    )
    parser.add_argument(
        "--hybrid-routines",
        type=Path,
        required=True,
        metavar="MANIFEST",
        help="version 1 JSON manifest of bounded integer machine routines",
    )
    parser.epilog = (
        "Hybrid mode requires the native MP64 interpreter. Machine routines "
        "use declared buffers and fixed call bounds; machine MMIO, callbacks, "
        "native BIOS boot, and multicore execution are unavailable."
    )
    return parser


def prepare_server(args: argparse.Namespace) -> PreparedHybridServer:
    """Validate every routine before publication, then prepare the boot image."""

    if (
        args.retained_terminal_policy is not None
        and args.rich_terminal_policy is None
    ):
        raise ValueError("--retained-terminal-policy requires --rich-terminal-policy")
    storage_path = args.storage.resolve()
    if not storage_path.is_file():
        raise ValueError(f"storage image does not exist: {storage_path}")
    quantum_steps = configured_semantic_quantum_steps(args.semantic_quantum_steps)
    # The loader resolves and validates all images before any runtime, routine
    # binding, boot source, or socket is made visible.
    manifest = load_manifest_v1(args.hybrid_routines)
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
        executor=args.executor,
        memory=memory,
        storage=storage,
        dispatch_instruction_limit=manifest.dispatch_instruction_limit,
    )
    session = None
    try:
        # Core BIOS vocabulary already exists. Source compilation and autoexec
        # can now resolve the exact declared words through ordinary lookup.
        for image in manifest.routines:
            hybrid.register_routine_v1(image)
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
        print(f"[shared] socket:  {server.socket_path}", flush=True)
        print("[shared] backend: hybrid", flush=True)
        print("[shared] clock:   realtime", flush=True)
        print(f"[shared] image:   {args.storage.resolve()}", flush=True)
        print(f"[shared] routines: {args.hybrid_routines.resolve()}", flush=True)
        print(
            f"[shared] execution: {prepared.preparation.runtime.execution_backend} "
            "semantic + bounded MP64 native interpreter",
            flush=True,
        )
        print(
            f"[shared] prepare: {prepared.preparation.preparation_semantic_steps} "
            "semantic steps",
            flush=True,
        )
        print(
            "[shared] quantum: "
            f"{prepared.machine.semantic_session.semantic_quantum_steps} "
            "semantic steps",
            flush=True,
        )
        print("[shared] machine owner running; Ctrl+C stops it", flush=True)
        server.serve_forever()
    finally:
        server.stop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = ["PreparedHybridServer", "build_argument_parser", "main", "prepare_server"]
