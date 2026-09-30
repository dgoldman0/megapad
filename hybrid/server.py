"""Start a shared semantic session with declared bounded MP64 routines."""

from __future__ import annotations

import argparse
import signal
import time
from dataclasses import dataclass, replace
from pathlib import Path

from hybrid.manifest import load_manifest
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from shared.hybrid_abi import RoutineManifestV2, RoutineManifestV3
from shared.hybrid_closed import (
    PolicyBranchV3, PolicyBranchZeroV3, PolicyCallV3, PolicyCoreCallV3,
    PolicyLiteralV3, PolicyReturnV3, prove_policies,
)
from shared.hybrid_nested import PolicyMachineCallV4, RoutineManifestV4
from shared.session_options import configured_production_executor
from shared_session import SessionServer
from simulator.image_bootstrap import ImageBootstrapPreparation, prepare_image_bootstrap
from simulator.ir import Branch, BranchZero, Call, Literal, Return
from simulator.platform import create_one_core_address_space
from simulator.session import configured_semantic_quantum_steps
from simulator.storage import HostedStorageService
from simulator.server import build_argument_parser as semantic_argument_parser


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
        help="version 1, 2 or 3 JSON manifest of bounded integer machine routines",
    )
    parser.epilog = (
        "Hybrid mode requires the native MP64 interpreter. Machine routines "
        "use declared buffers and fixed call bounds. Version 2 admits declared "
        "MIN/MAX/ABS/AND/OR/XOR callbacks; version 3 adds declared closed integer "
        "policies. Machine MMIO, arbitrary or nested machine callbacks, "
        "native BIOS boot, and multicore execution are unavailable."
    )
    return parser


def _install_manifest_policies(hybrid: HybridRuntime, manifest: RoutineManifestV3) -> None:
    """Install a proved table only into this server's fresh, unexposed owner.

    Failure closes the whole preparation owner. This is deliberately not a
    caller-owned-runtime publication API with an implied batch transaction.
    """
    runtime = hybrid.semantic
    policies = tuple(replace(policy, operations=tuple(
        replace(operation) for operation in policy.operations
    )) for policy in manifest.policies)
    proofs = prove_policies(policies)
    by_id = {policy.policy_id: policy for policy in policies}
    with runtime._session_owner_lock:
        runtime._require_session_owner_access("install hybrid manifest policies")
        runtime._require_no_suspension("install hybrid manifest policies")
        if runtime._active_dispatches or runtime._active_input_states:
            raise RuntimeError("manifest policies require a fresh idle runtime")
        # Preflight the complete namespace and original primitive identities
        # before publishing any policy. Later source may shadow these Words;
        # callback capture retains the originals, never a fresh name lookup.
        for policy in policies:
            if runtime.find(policy.name) is not None:
                raise ValueError(f"manifest policy name already exists: {policy.name}")
        core = {name: runtime.callback_policy_core_xt(name)
                for name in sorted({name for proof in proofs for name in proof.core_names})}
        defined = {}
        for proof in proofs:
            policy = by_id[proof.policy_id]
            operations = []
            for operation in policy.operations:
                kind = type(operation)
                if kind is PolicyLiteralV3:
                    lowered = Literal(operation.value)
                elif kind is PolicyCoreCallV3:
                    lowered = Call(core[operation.name])
                elif kind is PolicyCallV3:
                    lowered = Call(defined[operation.policy_id].xt)
                elif kind is PolicyBranchV3:
                    lowered = Branch(operation.target)
                elif kind is PolicyBranchZeroV3:
                    lowered = BranchZero(operation.target)
                elif kind is PolicyReturnV3:
                    lowered = Return()
                else:
                    raise TypeError("manifest policy contains an unsupported operation")
                operations.append(lowered)
            defined[policy.policy_id] = runtime.define_colon(policy.name, tuple(operations))


def _install_nested_manifest(hybrid: HybridRuntime, manifest: RoutineManifestV4) -> None:
    """Publish a proved graph only into this fresh, unexposed preparation owner.

    The loader's dependency order is diagnostic. Runtime registration captures
    and proves the exact newly published static Words independently; no graph
    ID, copied proof or source-evaluated placeholder grants child authority.
    """
    if type(manifest) is not RoutineManifestV4:
        raise TypeError("nested startup requires a RoutineManifestV4")
    RoutineManifestV4.__post_init__(manifest)
    exports = {export.export_id: replace(export) for export in manifest.exports}
    manifest = replace(
        manifest,
        policies=tuple(replace(policy, operations=tuple(replace(operation)
                       for operation in policy.operations)) for policy in manifest.policies),
        exports=tuple(exports.values()),
        routines=tuple(replace(image, callbacks=tuple(replace(
            site, export=exports[site.export.export_id]) for site in image.callbacks))
            for image in manifest.routines),
    )
    graph = manifest.graph_proof()
    policies = {policy.policy_id: policy for policy in manifest.policies}
    routines = {image.routine_id: image for image in manifest.routines}
    runtime = hybrid.semantic
    with runtime._session_owner_lock:
        runtime._require_session_owner_access("install nested hybrid manifest")
        runtime._require_no_suspension("install nested hybrid manifest")
        if runtime._active_dispatches or runtime._active_input_states:
            raise RuntimeError("nested manifest publication requires a fresh idle runtime")
        if getattr(hybrid, "nested_callback_abi_available", False) is not True:
            raise RuntimeError("hybrid nested callbacks require full semantic profile v4 and native transport v3")
        # Check the complete fresh namespace before even the earliest child
        # is published. Existing V1/V2/V3 startup keeps its original rules.
        for value in (*manifest.policies, *manifest.routines):
            if runtime.find(value.name) is not None:
                raise ValueError(f"nested manifest name already exists: {value.name}")
        core_names = {name for proof in graph.policy_proofs for name in proof.core_names}
        core_names.update(export.name for export in manifest.exports
                          if export.effect == "integer_leaf")
        core = {name: runtime.callback_policy_core_xt(name) for name in sorted(core_names)}
        defined_policies, defined_routines = {}, {}
        for node in graph.publication_order:
            if node.kind == "routine":
                defined_routines[node.node_id] = hybrid.register_routine_v4(routines[node.node_id])
                continue
            policy = policies[node.node_id]
            operations = []
            for operation in policy.operations:
                kind = type(operation)
                if kind is PolicyLiteralV3:
                    lowered = Literal(operation.value)
                elif kind is PolicyCoreCallV3:
                    lowered = Call(core[operation.name])
                elif kind is PolicyCallV3:
                    lowered = Call(defined_policies[operation.policy_id].xt)
                elif kind is PolicyMachineCallV4:
                    lowered = Call(defined_routines[operation.routine_id].xt)
                elif kind is PolicyBranchV3:
                    lowered = Branch(operation.target)
                elif kind is PolicyBranchZeroV3:
                    lowered = BranchZero(operation.target)
                elif kind is PolicyReturnV3:
                    lowered = Return()
                else:
                    raise TypeError("nested manifest contains an unsupported operation")
                operations.append(lowered)
            defined_policies[node.node_id] = runtime.define_colon(policy.name, tuple(operations))


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
    executor = configured_production_executor(args.executor)
    # The loader resolves and validates all images before any runtime, routine
    # binding, boot source, or socket is made visible.
    manifest = load_manifest(args.hybrid_routines)
    closed_manifest = type(manifest) is RoutineManifestV3
    nested_manifest = type(manifest) is RoutineManifestV4
    callback_manifest = type(manifest) in (RoutineManifestV2, RoutineManifestV3, RoutineManifestV4)
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
        dispatch_instruction_limit=manifest.dispatch_instruction_limit,
        **({"require_nested_callbacks": True} if nested_manifest else {}),
        **({
            "dispatch_callback_limit": manifest.dispatch_callback_limit,
            "dispatch_callback_semantic_limit": manifest.dispatch_callback_semantic_limit,
        } if callback_manifest else {}),
    )
    session = None
    try:
        if nested_manifest and getattr(hybrid, "nested_callback_abi_available", False) is not True:
            raise RuntimeError(
                "hybrid nested callbacks require full semantic profile v4 and native transport v3"
            )
        if closed_manifest and not hybrid.closed_callback_abi_available:
            raise RuntimeError(
                "hybrid closed callbacks require semantic profile v3 and "
                "a matching _mp64_accel v2; run make build"
            )
        if callback_manifest and not hybrid.callback_abi_available:
            raise RuntimeError("hybrid callbacks require a matching _mp64_accel v2; run make build")
        # Core BIOS vocabulary already exists. Source compilation and autoexec
        # can now resolve the exact declared words through ordinary lookup.
        if nested_manifest:
            _install_nested_manifest(hybrid, manifest)
        elif closed_manifest:
            _install_manifest_policies(hybrid, manifest)
        for image in (() if nested_manifest else manifest.routines):
            if closed_manifest:
                hybrid.register_routine_v3(image)
            elif callback_manifest:
                hybrid.register_routine_v2(image)
            else:
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
            manifest_abi_version=manifest.version,
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
