"""Typed, callback-free memory checks for an active task semantic scope.

The dispatcher installs already-qualified grant values on the original stacks.
This object never executes an adapter, retires a cookie, or calls guest code.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from shared.cells import MASK64
from shared.foreign_abi import ForeignAccessV1, ForeignSpanV1, MAX_DEPTH, MAX_GRANTS


_ISSUANCE = object()


def _reject(message):
    # Keep stacks independent of the runtime import cycle. A bound guard can
    # exist only after runtime construction and engine admission have finished.
    from simulator.foreign_runtime import ForeignTaskError

    raise ForeignTaskError(message)


@dataclass(frozen=True, slots=True, eq=False)
class TaskEffectScope:
    """Numerical evidence only; attaching a scope requires the issued owner."""

    grants: tuple[tuple[int, int, ForeignAccessV1], ...]
    _issuer: object = field(repr=False)
    _retired: bool = field(default=False, init=False, repr=False)


class TaskEffectGuard:
    """One task's exact stack owners and immutable active permission scope."""

    __slots__ = ("_issuer", "_data", "_returns", "_scope", "_scopes", "_closed")

    def __init__(self, issuer, data, returns, *, _issuance=None):
        if _issuance is not _ISSUANCE:
            raise TypeError("task effect guards must be issued by the dispatcher")
        self._issuer, self._data, self._returns = issuer, data, returns
        self._scope = None
        self._scopes = []
        self._closed = False

    @classmethod
    def issue(cls, issuer, data, returns):
        from simulator.stacks import DataStack, ReturnStack

        if issuer is None or type(data) is not DataStack or type(returns) is not ReturnStack:
            raise TypeError("task effect authority requires exact backed task stacks")
        if data._memory is None or data._memory is not returns._memory:
            raise TypeError("task effect stacks must share ordinary backing")
        if data._task_effect_guard is not None or returns._task_effect_guard is not None:
            _reject("task stack effects already have an owner")
        return cls(issuer, data, returns, _issuance=_ISSUANCE)

    def _require(self, issuer):
        if type(self) is not TaskEffectGuard or issuer is not self._issuer:
            _reject("task effect guard has a different issuer")
        if self._closed is not False:
            _reject("task effect guard is closed")
        if type(self._scopes) is not list or len(self._scopes) > MAX_DEPTH + 1:
            _reject("task effect scope ownership changed")
        for record in self._scopes:
            if (type(record) is not tuple or len(record) != 2
                    or type(record[0]) is not TaskEffectScope or type(record[1]) is not tuple
                    or len(record[1]) > MAX_GRANTS):
                _reject("task effect scope evidence changed")

    def scope(self, issuer, grants):
        TaskEffectGuard._require(self, issuer)
        # Eight parked callbacks plus one bounded bridge-transfer scope.
        if len(self._scopes) >= MAX_DEPTH + 1:
            _reject("task effect scope table is full")
        if type(grants) is not tuple or len(grants) > MAX_GRANTS:
            raise TypeError("task effect grants must be a bounded exact tuple")
        values = []
        for grant in grants:
            if type(grant) is not ForeignSpanV1:
                raise TypeError("task effect grant must be an exact ForeignSpanV1")
            ForeignSpanV1.__post_init__(grant)
            values.append((grant.base, grant.base + grant.size, grant.access))
        values = tuple(values)
        scope = TaskEffectScope(values, self._issuer)
        self._scopes.append((scope, values))
        return scope

    def release_scope(self, issuer, scope):
        TaskEffectGuard._require(self, issuer)
        index = next((index for index, record in enumerate(self._scopes) if record[0] is scope), None)
        if index is None:
            _reject("task effect scope is no longer issued")
        if self._scope is self._scopes[index]:
            _reject("active task effect scope must be detached before release")
        del self._scopes[index]
        object.__setattr__(scope, "_retired", True)

    def attach(self, issuer, scope):
        TaskEffectGuard._require(self, issuer)
        record = next((record for record in self._scopes if record[0] is scope), None)
        if (type(scope) is not TaskEffectScope or record is None
                or scope._issuer is not self._issuer or scope.grants is not record[1]
                or scope._retired is not False):
            _reject("task effect scope was not issued by this owner")
        for stack in (self._data, self._returns):
            if stack._task_effect_guard is not None and stack._task_effect_guard is not self:
                _reject("task stack effect binding changed")
        self._scope = record
        self._data._task_effect_guard = self
        self._returns._task_effect_guard = self

    def detach(self, issuer):
        TaskEffectGuard._require(self, issuer)
        for stack in (self._data, self._returns):
            if stack._task_effect_guard is not None and stack._task_effect_guard is not self:
                _reject("task stack effect binding changed")
        self._data._task_effect_guard = None
        self._returns._task_effect_guard = None
        self._scope = None

    def require_binding(self, issuer, scope):
        TaskEffectGuard._require(self, issuer)
        record = self._scope
        if (type(record) is not tuple or len(record) != 2 or record[0] is not scope
                or not any(candidate is record for candidate in self._scopes)
                or self._data._task_effect_guard is not self
                or self._returns._task_effect_guard is not self
                or type(scope) is not TaskEffectScope or scope.grants is not record[1]
                or scope._issuer is not issuer or scope._retired is not False):
            _reject("task original effect binding changed")

    def close(self, issuer):
        if type(self) is not TaskEffectGuard or issuer is not self._issuer:
            _reject("task effect guard has a different issuer")
        if self._closed is True:
            return
        if self._closed is not False:
            _reject("task effect guard lifecycle state changed")
        TaskEffectGuard.detach(self, issuer)
        for scope, _values in self._scopes:
            object.__setattr__(scope, "_retired", True)
        self._scopes.clear()
        self._closed = True
        self._data = self._returns = None

    @staticmethod
    def require_stack_access(guard, stack, address, width, access):
        if type(guard) is not TaskEffectGuard:
            _reject("task stack effect guard is not an issued exact authority")
        if stack is not guard._data and stack is not guard._returns:
            _reject("task effect guard does not own this stack")
        if stack._task_effect_guard is not guard:
            _reject("task stack effect binding changed")
        TaskEffectGuard.require_access(guard, address, width, access)

    def require_access(self, address, width, access):
        if type(self) is not TaskEffectGuard or self._closed is not False:
            _reject("task effect guard is not active")
        record = self._scope
        if type(record) is not tuple or len(record) != 2:
            _reject("task effect has no issued semantic scope")
        scope, values = record
        if (type(scope) is not TaskEffectScope or scope._issuer is not self._issuer
                or scope.grants is not values or scope._retired is not False
                or type(values) is not tuple or len(values) > MAX_GRANTS):
            _reject("task effect has no issued semantic scope")
        if (type(address) is not int or type(width) is not int
                or not 0 <= address <= MASK64 or not 1 <= width <= MASK64
                or width - 1 > MASK64 - address):
            _reject("task memory effect has invalid uint64 geometry")
        if type(access) is not str or access not in ("read", "write"):
            _reject("task memory effect has an invalid operation")
        for grant in values:
            if (type(grant) is not tuple or len(grant) != 3
                    or type(grant[0]) is not int or type(grant[1]) is not int
                    or not 0 <= grant[0] <= grant[1] <= MASK64 + 1
                    or type(grant[2]) is not ForeignAccessV1):
                _reject("task effect grant evidence changed")
            base, limit, permission = grant
            if (base <= address and address + width <= limit
                    and (permission is ForeignAccessV1.READ_WRITE
                         or (access == "read" and permission is ForeignAccessV1.READ)
                         or (access == "write" and permission is ForeignAccessV1.WRITE))):
                return
        _reject("task memory effect is outside its original captured grants")


_EFFECT_ROUTES = tuple((kind, tuple(vars(kind).items()))
                       for kind in (TaskEffectGuard, TaskEffectScope))

__all__ = ["TaskEffectGuard", "TaskEffectScope"]
