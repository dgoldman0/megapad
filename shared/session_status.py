"""Backend-neutral execution descriptors and terminal status values.

Adapters supply their actual execution and timing policies. This module
neither selects a backend nor reaches into a CPU, runtime, scheduler, or
device. Callers retain their existing owner lock while collecting status.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True, kw_only=True)
class SessionRuntimeDescriptor:
    """The selected engine, accounting units, and supported session actions.

    ``executor`` names the selected engine; it does not assert that every
    operation runs natively. Capability flags describe supported actions,
    independently of whether an optional facility is currently enabled.
    ``step_request_unit`` states what the control protocol's step count means.
    """

    mode: str
    executor: str
    step_unit: str
    step_request_unit: str
    batch_unit: str
    timer_unit: str
    rtc_mode: str
    machine_code: bool
    cpu_diagnostics: bool
    network_diagnostics: bool
    reset: bool
    host_profiling: bool

    def to_dict(self) -> dict:
        return {
            "mode": self.mode,
            "executor": self.executor,
            "step_unit": self.step_unit,
            "step_request_unit": self.step_request_unit,
            "batch_unit": self.batch_unit,
            "timing": {
                "timer_unit": self.timer_unit,
                "rtc_mode": self.rtc_mode,
            },
            "capabilities": {
                "machine_code": self.machine_code,
                "cpu_diagnostics": self.cpu_diagnostics,
                "network_diagnostics": self.network_diagnostics,
                "reset": self.reset,
                "host_profiling": self.host_profiling,
            },
        }


def terminal_status(session, *, pending: bool, failure: str | None) -> dict:
    """Collect the common terminal payload through the session's public view.

    Pending work and failure are supplied from the same observations used by
    the owner's state decision. Reading status does not service the driver.
    """

    driver = session.rich_terminal_driver
    core = None if driver is None else driver.core
    state = session.rich_terminal_state
    return {
        "enabled": session.rich_terminal_enabled,
        "display_required": session.retained_display_required,
        "state": None if state is None else state.value,
        "pending": pending,
        "lost": session.rich_terminal_lost,
        "failure": failure,
        "machine_publications": (
            0 if core is None else core.machine_publications_received
        ),
        "machine_publication_bytes": (
            0 if core is None else core.machine_publication_bytes_received
        ),
        "frames": 0 if core is None else core.frames_received,
        "frame_bytes": 0 if core is None else core.frame_bytes_received,
        "frames_by_type": (
            {}
            if core is None
            else {
                f"0x{frame_type:04X}": count
                for frame_type, count in sorted(core.frames_received_by_type.items())
            }
        ),
        "frame_bytes_by_type": (
            {}
            if core is None
            else {
                f"0x{frame_type:04X}": byte_count
                for frame_type, byte_count in sorted(
                    core.frame_bytes_received_by_type.items()
                )
            }
        ),
        "decoder_buffered_bytes": (
            0 if core is None else core.decoder_buffered_bytes
        ),
    }


__all__ = ["SessionRuntimeDescriptor", "terminal_status"]
