"""Declared machine routines behind the shared semantic session authority."""

from __future__ import annotations

from hybrid.runtime import HybridRuntime
from shared.hybrid_abi import HYBRID_ABI, HYBRID_ABI_VERSION
from shared.session import RichTerminalSessionConfig
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine


_HYBRID_CLOSE = HybridRuntime.close


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
        machine_quantum_instructions: int | None = None,
        rich_terminal: RichTerminalSessionConfig | None = None,
        manifest_abi_version: int | None = None,
    ) -> None:
        if not isinstance(hybrid, HybridRuntime):
            raise TypeError("hybrid must be a HybridRuntime")
        if hybrid.closed:
            raise RuntimeError("the hybrid runtime is closed")
        if manifest_abi_version is not None:
            if type(manifest_abi_version) is not int:
                raise TypeError("manifest ABI version must be an exact integer or None")
            if not 1 <= manifest_abi_version <= 5:
                raise ValueError("manifest ABI version must be in 1..5")
        if (manifest_abi_version == 4
                and getattr(hybrid, "nested_callback_abi_available", False) is not True):
            raise RuntimeError("hybrid nested callbacks require full semantic profile v4 and native transport v3")
        if (manifest_abi_version == 5
                and getattr(hybrid, "service_callback_abi_available", False) is not True):
            raise RuntimeError("hybrid scalar callbacks require qualified private scalar services and native transport v2")
        self.hybrid = hybrid
        self._manifest_abi_version = manifest_abi_version
        super().__init__(
            hybrid.semantic,
            entry,
            cols=cols,
            rows=rows,
            semantic_step_budget=semantic_step_budget,
            semantic_quantum_steps=semantic_quantum_steps,
            machine_quantum_instructions=machine_quantum_instructions,
            rich_terminal=rich_terminal,
        )

    @property
    def manifest_abi_version(self) -> int | None:
        return self._manifest_abi_version

    def close(self) -> None:
        # Release the terminal lease, cancel any owned continuation, and give
        # up semantic authority before revoking code or native mapping leases.
        hybrid = self.hybrid
        # Capture before frontend/backend cleanup can raise or alter a route.
        cleanup = (lambda close=_HYBRID_CLOSE: close(hybrid)) if type(hybrid) is HybridRuntime else hybrid.close
        try:
            super().close()
        except BaseException as error:
            try:
                cleanup()
            except BaseException:
                try:
                    BaseException.add_note(error, "hybrid native owner cleanup also failed")
                except BaseException:
                    pass
            raise
        cleanup()


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
            manifest_version = self.semantic_session.manifest_abi_version
            abi_version = max(registered_versions, default=(manifest_version or HYBRID_ABI_VERSION))
            nested_available = getattr(hybrid, "nested_callback_abi_available", False) is True
            selected_versions = registered_versions or [manifest_version or HYBRID_ABI_VERSION]
            nested_profile = 4 in selected_versions
            service_profile = 5 in selected_versions
            service_available = getattr(hybrid, "service_callback_abi_available", False) is True
            transports = sorted({3 if version == 4 else 2 if version >= 2 else 1
                                 for version in selected_versions})
            exports = sorted({
                site.export.name
                for image in registrations
                for site in getattr(image, "callbacks", ())
            })
            effects = {site.export.effect
                       for image in registrations
                       for site in getattr(image, "callbacks", ())}
            closed_policies = bool(effects & {"closed_integer_colon", "closed_integer_nested"})
            profiles = (["canonical_integer_leaf"] if "integer_leaf" in effects else [])
            if "closed_integer_colon" in effects:
                profiles.append("closed_integer_colon")
            if "closed_integer_nested" in effects:
                profiles.append("closed_integer_nested")
            if "scalar_fp_state" in effects:
                profiles.append("scalar_fp_state")
            result["backend"] = "hybrid"
            result["runtime"]["mode"] = "hybrid"
            result["runtime"]["capabilities"].update(
                machine_code=True,
                declared_machine_routines=True,
                arbitrary_machine_code=False,
                machine_mmio=False,
                semantic_callbacks=bool(exports),
                closed_integer_callbacks=closed_policies,
                arbitrary_semantic_callbacks=False,
                nested_machine_callbacks=nested_profile and nested_available,
                private_scalar_fp_v1=service_profile and service_available,
                callback_suspension=False,
                shared_task_exceptions=False,
                composite_suspension=False,
                native_bios_boot=False,
                multicore=False,
                native_snapshot=False,
            )
            result["machine_execution"] = {
                "backend": "mp64_native_interpreter",
                "abi": HYBRID_ABI,
                "abi_version": abi_version,
                "manifest_abi_version": manifest_version,
                "registered_abi_versions": registered_versions,
                "native_transport_version": max(transports),
                "native_transport_versions": transports,
                "instructions": hybrid.machine_instructions,
                "cycles": hybrid.machine_cycles,
                "transitions": hybrid.transitions,
                "segments": hybrid.machine_segments,
                "dispatch_instruction_limit": hybrid.dispatch_instruction_limit,
                "callback_abi_available": hybrid.callback_abi_available,
                "closed_callback_abi_available": hybrid.closed_callback_abi_available,
                "nested_callback_abi_available": nested_available,
                "service_callback_abi_available": service_available,
                "service_callback_profile": "private_scalar_fp_v1" if service_profile else None,
                "service_value_executor": (getattr(hybrid, "service_callback_value_executor", None)
                                           if service_profile and service_available else None),
                "private_callback_executor": ("python_reference"
                                              if closed_policies or "scalar_fp_state" in effects else None),
                "service_parked_depth_limit": 1 if service_profile else 0,
                "max_machine_depth": hybrid.max_machine_depth,
                "callback_profile": ("closed_integer_nested" if "closed_integer_nested" in effects else
                                     "closed_integer_colon" if closed_policies else
                                     "scalar_fp_state" if "scalar_fp_state" in effects else
                                     "canonical_integer_leaf" if exports else None),
                "callback_profiles": profiles,
                "closed_callback_executor": "python_reference" if closed_policies else None,
                "callback_exports": exports,
                "callback_requests": hybrid.callback_requests,
                "callback_semantic_steps": hybrid.callback_semantic_steps,
                "dispatch_callback_limit": hybrid.dispatch_callback_limit,
                "dispatch_callback_semantic_limit": hybrid.dispatch_callback_semantic_limit,
            }
            task_execution = hybrid.task_execution_status
            if task_execution is not None:
                result["task_execution"] = {
                    **task_execution,
                    "registered_words": list(task_execution["registered_words"]),
                    "quantum_instructions": self.semantic_session.machine_quantum_instructions,
                }
            if detailed:
                result["hybrid"] = result.pop("simulator")
            return result


__all__ = ["HybridSession", "HybridSharedMachine"]
