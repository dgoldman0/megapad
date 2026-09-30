"""Declared machine routines behind the shared semantic session authority."""

from __future__ import annotations

from hybrid.runtime import HybridRuntime
from shared.hybrid_abi import HYBRID_ABI, HYBRID_ABI_VERSION
from shared.session import RichTerminalSessionConfig
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine


class HybridSession(SimulatorMachineSession):
    """Own a hybrid composition after attaching its semantic session backend.

    The ordinary semantic backend keeps terminal, input, suspension, and owner
    boundaries. Registered words enter the bounded machine runner through that
    same runtime; machine instructions remain separate from semantic steps.
    """

    def __init__(
        self,
        hybrid: HybridRuntime,
        entry: bytes | str | int,
        *,
        cols: int = 80,
        rows: int = 30,
        semantic_step_budget: int | None = None,
        semantic_quantum_steps: int | None = None,
        rich_terminal: RichTerminalSessionConfig | None = None,
    ) -> None:
        if not isinstance(hybrid, HybridRuntime):
            raise TypeError("hybrid must be a HybridRuntime")
        if hybrid.closed:
            raise RuntimeError("the hybrid runtime is closed")
        self.hybrid = hybrid
        super().__init__(
            hybrid.semantic,
            entry,
            cols=cols,
            rows=rows,
            semantic_step_budget=semantic_step_budget,
            semantic_quantum_steps=semantic_quantum_steps,
            rich_terminal=rich_terminal,
        )

    def close(self) -> None:
        # Release the terminal lease, cancel any owned continuation, and give
        # up semantic authority before revoking code or native mapping leases.
        super().close()
        self.hybrid.close()


class HybridSharedMachine(SimulatorSharedMachine):
    """The existing shared control protocol with explicit hybrid capabilities."""

    def __init__(self, session: HybridSession) -> None:
        if not isinstance(session, HybridSession):
            raise TypeError("session must be a HybridSession")
        super().__init__(session)

    def status(self, *, detailed: bool = True) -> dict:
        with self.lock:
            result = super().status(detailed=detailed)
            hybrid = self.semantic_session.hybrid
            registrations = hybrid.registered_routines
            registered_versions = sorted({image.version for image in registrations})
            exports = sorted({
                site.export.name
                for image in registrations
                for site in getattr(image, "callbacks", ())
            })
            result["backend"] = "hybrid"
            result["runtime"]["mode"] = "hybrid"
            result["runtime"]["capabilities"].update(
                machine_code=True,
                declared_machine_routines=True,
                arbitrary_machine_code=False,
                machine_mmio=False,
                semantic_callbacks=bool(exports),
                arbitrary_semantic_callbacks=False,
                nested_machine_callbacks=False,
                callback_suspension=False,
                native_bios_boot=False,
                multicore=False,
                native_snapshot=False,
            )
            result["machine_execution"] = {
                "backend": "mp64_native_interpreter",
                "abi": HYBRID_ABI,
                "abi_version": max(registered_versions, default=HYBRID_ABI_VERSION),
                "registered_abi_versions": registered_versions,
                "instructions": hybrid.machine_instructions,
                "cycles": hybrid.machine_cycles,
                "transitions": hybrid.transitions,
                "segments": hybrid.machine_segments,
                "dispatch_instruction_limit": hybrid.dispatch_instruction_limit,
                "callback_abi_available": hybrid.callback_abi_available,
                "callback_profile": "canonical_integer_leaf" if exports else None,
                "callback_exports": exports,
                "callback_requests": hybrid.callback_requests,
                "callback_semantic_steps": hybrid.callback_semantic_steps,
                "dispatch_callback_limit": hybrid.dispatch_callback_limit,
                "dispatch_callback_semantic_limit": hybrid.dispatch_callback_semantic_limit,
            }
            if detailed:
                result["hybrid"] = result.pop("simulator")
            return result


__all__ = ["HybridSession", "HybridSharedMachine"]
