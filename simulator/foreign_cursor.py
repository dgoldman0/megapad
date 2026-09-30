"""Finite observations for machine scheduling under the semantic owner.

Neither value issues a turn, native operation, or suspension. The existing
dispatcher retains the original frame and one exact cursor issuance; equality,
copies and freshly constructed values cannot replace that evidence.
"""

from dataclasses import dataclass, field
from sys import _getframe

from shared.cells import MASK64
from shared.foreign_abi import ForeignReceiptV1, MAX_ROOT_INSTRUCTIONS


@dataclass(frozen=True, slots=True, eq=False)
class MachineTurn:
    """The original instruction allowance selected for one outer host turn."""

    limit: int

    def __post_init__(self):
        if type(self.limit) is not int:
            raise TypeError("machine quantum instructions must be an exact integer")
        if not 1 <= self.limit <= MAX_ROOT_INSTRUCTIONS:
            raise ValueError(
                f"machine quantum instructions must be in 1..{MAX_ROOT_INSTRUCTIONS}")


@dataclass(frozen=True, slots=True, eq=False)
class ForeignMachineCursor:
    """Observation of one accepted runnable event, never a semantic XT/IP."""

    root_token: object = field(repr=False)
    root_id: int
    invocation_id: int
    operation_token: object = field(repr=False)
    receipt: ForeignReceiptV1 = field(repr=False)
    host_yield: bool = field(default=True, init=False)

    def __post_init__(self):
        if self.root_token is None or self.operation_token is None:
            raise TypeError("machine cursor requires opaque root and operation tokens")
        for value, label in ((self.root_id, "root ID"),
                             (self.invocation_id, "invocation ID")):
            if type(value) is not int:
                raise TypeError(f"machine cursor {label} must be an exact integer")
            if not 1 <= value <= MASK64:
                raise ValueError(f"machine cursor {label} must be in uint64 range")
        if type(self.receipt) is not ForeignReceiptV1:
            raise TypeError("machine cursor requires an exact task receipt")
        if self.host_yield is not True:
            raise ValueError("machine cursor must denote a host scheduling yield")


def _machine_state_cell(owner, publishers, _frame=_getframe, _error=RuntimeError,
                        _tuple=tuple, _zip=zip):
    """One private state cell, retained by the original engine-owned holder.

    The state is an immutable (turn, cursor) tuple. It is not a second work
    ledger; instruction counts always come from the original task ledger.
    Keeping it off replaceable root fields also makes those fields harmless
    projections instead of an alternate scheduling authority.
    """
    state = (None, None)

    def read():
        return state

    def replace(previous, current, _error=_error, _frame=_frame):
        nonlocal state
        caller = _frame(1)
        permitted = False
        for code in publishers:
            if caller.f_code is code:
                permitted = True
                break
        if not permitted or caller.f_locals.get("self") is not owner:
            raise _error("machine scheduling publication requires its original dispatcher")
        if state is not previous:
            raise _error("machine scheduling state changed during publication")
        state = current

    routes = _tuple((function, function.__code__, function.__globals__,
                    function.__defaults__, function.__kwdefaults__, function.__closure__,
                    _tuple((cell, cell.cell_contents)
                          for name, cell in _zip(function.__code__.co_freevars,
                                                 function.__closure__ or ())
                          if name != "state"))
                   for function in (read, replace))
    return read, replace, routes


__all__ = ["MachineTurn", "ForeignMachineCursor"]
