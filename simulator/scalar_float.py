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


_VALIDATE = scalar_fp.validate
_VALIDATE_CODE = _VALIDATE.__code__
_ILLEGAL_OPERATION = scalar_fp.IllegalOperation
_ILLEGAL_SCALAR_FLOAT = IllegalScalarFloatError
_PRIVATE_OPERATIONS = (0x00, 0x01, 0x02, 0x03, 0x04, 0x07,
                       0x40, 0x41, 0x42, 0x43, 0x44, 0x47)


class _ValidationBoundary:
    """One service-issued observation scope, never callback authority."""

    __slots__ = ("_owner", "operation", "failure")

    def __init__(self, owner, operation):
        self._owner = owner
        self.operation = operation
        self.failure = None

    def __copy__(self):
        # A detached value copy has no active-scope identity or pending error.
        # Avoid object.__reduce_ex__: its slot-name cache mutates this sealed
        # class merely because a caller attempts to copy an opaque boundary.
        return _ValidationBoundary(None, _OPERATION_SLOT.__get__(self))

    def __deepcopy__(self, memo):
        duplicate = _ValidationBoundary(None, _OPERATION_SLOT.__get__(self))
        memo[id(self)] = duplicate
        return duplicate

    def __reduce_ex__(self, protocol):
        raise TypeError("scalar validation boundaries cannot be serialized")


class HostedScalarFloatService:
    """One runtime's ``FPCSR`` and scalar ``FC`` operations."""

    __slots__ = ("_fpcsr", "_native_execute", "_validation_scope", "_validation_armed")

    def __init__(self) -> None:
        self._fpcsr = 0
        self._native_execute = None
        self._validation_scope = None
        self._validation_armed = None

    def _begin_validation_boundary(self, operation):
        """Arm one internal scope after the private dispatcher charges its tick.

        This issues no export, machine continuation, or guest fault receipt.
        ``None`` is the non-arithmetic FPCSR access case.
        """

        if operation is not None and (
                type(operation) is not int or operation not in _PRIVATE_OPERATIONS):
            raise ValueError("validation boundary requires a fixed scalar service operation")
        if _SCOPE_SLOT.__get__(self) is not None:
            raise RuntimeError("scalar validation boundary is already active")
        boundary = _ValidationBoundary(self, operation)
        _SCOPE_SLOT.__set__(self, boundary)
        _ARMED_SLOT.__set__(self, boundary)
        return boundary

    def _finish_validation_boundary(self, boundary, error):
        """Consume the exact active scope, clearing it even for a raw escape."""

        if (type(boundary) is not _ValidationBoundary
                or _BOUNDARY_OWNER_SLOT.__get__(boundary) is not self
                or _SCOPE_SLOT.__get__(self) is not boundary):
            if boundary is not None and _ARMED_SLOT.__get__(self) is boundary:
                _ARMED_SLOT.__set__(self, None)
            raise RuntimeError("scalar validation boundary was not issued here or was consumed")
        # Pinned slot descriptors keep cleanup independent of a changed class
        # route. A foreign/copy token never clears the actual owner's scope.
        _ARMED_SLOT.__set__(self, None)
        _SCOPE_SLOT.__set__(self, None)
        failure = _FAILURE_SLOT.__get__(boundary)
        _FAILURE_SLOT.__set__(boundary, None)
        return failure if failure is not None and failure[0] is error else None

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

        # Claim at the first canonical entry, before validation. In particular,
        # a value kernel that reenters operate cannot inherit this boundary.
        boundary = self._validation_armed
        self._validation_armed = None
        boundary_fpcsr = self._fpcsr
        validator = scalar_fp.validate
        try:
            validator(op, 0, boundary_fpcsr)
        except scalar_fp.IllegalOperation as exc:
            error = IllegalScalarFloatError(str(exc))
            if (boundary is not None and boundary is self._validation_scope
                    and _BOUNDARY_OWNER_SLOT.__get__(boundary) is self
                    and validator is _VALIDATE and validator.__code__ is _VALIDATE_CODE
                    and type(exc) is _ILLEGAL_OPERATION
                    and type(error) is _ILLEGAL_SCALAR_FLOAT
                    and type(op) is int and op == _OPERATION_SLOT.__get__(boundary)
                    and type(boundary_fpcsr) is int
                    and 0 <= boundary_fpcsr <= 0x1F7
                    and boundary_fpcsr & ~0x1F7 == 0
                    and boundary_fpcsr & 7 in (5, 6, 7)):
                _FAILURE_SLOT.__set__(boundary, (error, op, boundary_fpcsr))
            raise error from None
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


_SCOPE_SLOT = HostedScalarFloatService.__dict__["_validation_scope"]
_ARMED_SLOT = HostedScalarFloatService.__dict__["_validation_armed"]
_OPERATION_SLOT = _ValidationBoundary.__dict__["operation"]
_FAILURE_SLOT = _ValidationBoundary.__dict__["failure"]
_BOUNDARY_OWNER_SLOT = _ValidationBoundary.__dict__["_owner"]


__all__ = ["HostedScalarFloatService", "IllegalScalarFloatError"]
