"""The hosted profile's full-width tile accumulator.

The service is native code, simulator/accel/tacc_service.h: core 0's
private TACC, its lifecycle, TAMAC in its tile-by-tile and broadcast forms,
and the words of borrowed tile work, with the checks the machine's TACC
memory port makes and a STORE's route into a chiplet port's window. The
hosted tile service's words call it with the tile registers. This module
loads the class with the simulator's extension on first use and turns the
fault an operation reports into the one the runtime routes:

* an illegal operation becomes ``IllegalInstructionFault`` and an alignment
  fault ``AlignmentFault``, which reach the ``FAULT-XT!`` callback with
  throw codes -21 and -23, as the BIOS's instruction-fault handlers pass
  them;
* a bus fault becomes ``TACCBusFault``, a guest memory access error.  The
  machine's BIOS reports a bus fault and returns to its prompt without the
  callback; the simulator fails the dispatch, as it does for every guest
  access outside mapped memory, with the address the machine reports.
  ``TACC-SEND`` turns its store's bus fault into status 6 instead, as the
  BIOS's bus-fault handler does for that one store.
"""

from __future__ import annotations

from simulator.errors import AlignmentFault, IllegalInstructionFault
from simulator.memory import MemoryAccessError


FAULT_NONE = 0
FAULT_ILLEGAL = 1
FAULT_ALIGNMENT = 2
FAULT_BUS = 3

TACC_IMAGE_BYTES = 256
TACC_OWNER_NONE = 31

# TACC-SEND's statuses, from the BIOS's common checked namespace.
TACC_SEND_OK = 0
TACC_SEND_HARDWARE = 6

# What made a bus fault, by tacc_service.h's Cause.
_CAUSES = {
    1: "runs past the end of the address space",
    2: "meets the device window",
    3: "runs past the end of its memory region",
    4: "starts outside routed memory",
    5: "runs past its chiplet port's window",
    6: "was not taken by its chiplet port",
    7: "lies outside ordinary memory",
}


class TACCBusFault(MemoryAccessError):
    """A TACC operation that the machine ends with a bus fault.

    ``address`` is the one the machine reports in ``TRAP_ADDR``, and
    ``answer`` the chiplet's answer when a port did not take a store image.
    """

    def __init__(
        self,
        message: str,
        *,
        operation: str,
        address: int,
        length: int,
        answer: int | None = None,
    ) -> None:
        self.answer = answer
        super().__init__(
            message, operation=operation, address=address, length=length
        )


def raise_fault(outcome: tuple, word: str) -> None:
    """Raise the fault in OUTCOME, a service result ``(fault, cause,
    address, length, retired, answer)``, for WORD."""

    fault, cause, address, length, _retired, answer = outcome
    if fault == FAULT_ILLEGAL:
        raise IllegalInstructionFault(
            f"{word} is illegal for the accumulator's owner, state, or TMODE"
        )
    if fault == FAULT_ALIGNMENT:
        raise AlignmentFault(
            address, f"{word} needs a 64-byte-aligned address, not {address:#018x}"
        )
    if fault == FAULT_BUS:
        what = "scalar" if cause == 7 else "span"
        detail = _CAUSES.get(cause, "has no route")
        if cause == 6:
            detail += f" (answer {answer})"
        raise TACCBusFault(
            f"{word}'s {what} {detail}",
            operation=word,
            address=address,
            length=length,
            answer=answer if cause == 6 else None,
        )


def __getattr__(name: str):
    if name == "HostedTACCService":
        from simulator.native_execution import native_service

        service = native_service("TACCService", "full-width tile accumulator")
        globals()[name] = service
        return service
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    "FAULT_ALIGNMENT",
    "FAULT_BUS",
    "FAULT_ILLEGAL",
    "FAULT_NONE",
    "HostedTACCService",
    "TACCBusFault",
    "TACC_IMAGE_BYTES",
    "TACC_OWNER_NONE",
    "TACC_SEND_HARDWARE",
    "TACC_SEND_OK",
    "raise_fault",
]
