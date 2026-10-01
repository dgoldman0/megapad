"""A shared session whose Forth runtime also runs declared machine routines."""

from __future__ import annotations

from hybrid.manifest import ABI, VERSION
from hybrid.runtime import HybridRuntime
from shared.session import RichTerminalSessionConfig
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine


class HybridSession(SimulatorMachineSession):
    """Run a hybrid runtime through the ordinary semantic session.

    The semantic session keeps the terminal, input, suspension and owner
    boundaries. Routine words run the machine through the same runtime, and
    machine instructions are counted apart from semantic steps.
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
            machine_quantum_instructions=machine_quantum_instructions,
            rich_terminal=rich_terminal,
        )

    def close(self) -> None:
        # Release the terminal and any suspended dispatch before the core.
        try:
            super().close()
        except BaseException as error:
            try:
                self.hybrid.close()
            except BaseException as cleanup:
                error.add_note(f"hybrid runtime close also failed: {cleanup!r}")
            raise
        self.hybrid.close()


class HybridSharedMachine(SimulatorSharedMachine):
    """The shared session protocol, reporting the machine side too."""

    def __init__(self, session: HybridSession) -> None:
        if not isinstance(session, HybridSession):
            raise TypeError("session must be a HybridSession")
        super().__init__(session)

    def status(self, *, detailed: bool = True) -> dict:
        with self.lock:
            result = super().status(detailed=detailed)
            session = self.semantic_session
            hybrid = session.hybrid
            result["backend"] = "hybrid"
            result["runtime"]["mode"] = "hybrid"
            result["runtime"]["capabilities"].update(
                machine_code=True,
                declared_machine_routines=True,
                arbitrary_machine_code=False,
                semantic_callbacks=True,
                machine_mmio=False,
                native_bios_boot=False,
                multicore=False,
                native_snapshot=False,
            )
            result["machine_execution"] = {
                "backend": "mp64_native_interpreter",
                "abi": ABI,
                "abi_version": VERSION,
                "routines": [routine.name for routine in hybrid.registered_routines],
                "instructions": hybrid.machine_instructions,
                "cycles": hybrid.machine_cycles,
                "segments": hybrid.machine_segments,
                "transitions": hybrid.transitions,
                "callbacks": hybrid.callback_requests,
                "quantum_instructions": session.machine_quantum_instructions,
                "instruction_budget": hybrid.machine_instruction_budget,
            }
            if detailed:
                result["hybrid"] = result.pop("simulator")
            return result


__all__ = ["HybridSession", "HybridSharedMachine"]
