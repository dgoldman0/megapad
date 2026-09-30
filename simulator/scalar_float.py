"""The hosted scalar floating-point service behind the BIOS FP words.

docs/floating-point.md §11 names the BIOS words.  The hosted simulator
models no instruction encodings, so each word here applies the operation
byte its BIOS body executes to :func:`shared.scalar_fp.execute`, the same
definition the Python emulator runs. Native-selected runtimes bind the shared
exact C++ value kernel. The service keeps the runtime's ``FPCSR``: the dynamic
rounding mode and the sticky flags.
"""

from __future__ import annotations

from shared import scalar_fp
from shared.cells import u64
from simulator.errors import IllegalInstructionFault


class IllegalScalarFloatError(IllegalInstructionFault):
    """The operation would raise ``IVEC_ILLEGAL_OP`` on the machine."""


class HostedScalarFloatService:
    """One runtime's ``FPCSR`` and scalar ``FC`` operations."""

    __slots__ = ("_fpcsr", "_native_execute")

    def __init__(self) -> None:
        self._fpcsr = 0
        self._native_execute = None

    @property
    def fpcsr(self) -> int:
        return self._fpcsr

    def write_fpcsr(self, value: int) -> None:
        self._fpcsr = u64(value) & scalar_fp.FPCSR_WRITE_MASK

    def operate(self, op: int, rd: int, rs: int, rt: int = 0) -> int:
        """Apply operation byte ``op`` and return the new ``Rd`` value.

        Raises :class:`IllegalScalarFloatError` where the machine traps,
        before any state changes.
        """

        try:
            scalar_fp.validate(op, 0, self._fpcsr)
        except scalar_fp.IllegalOperation as exc:
            raise IllegalScalarFloatError(str(exc)) from None
        if self._native_execute is None:
            outcome = scalar_fp.execute(op, u64(rd), u64(rs), u64(rt),
                                        self._fpcsr)
            value, flags = outcome.value, outcome.flags
        else:
            value, flags, _relation = self._native_execute(
                op, u64(rd), u64(rs), u64(rt), self._fpcsr,
            )
        self._fpcsr |= flags
        return value


__all__ = ["HostedScalarFloatService", "IllegalScalarFloatError"]
