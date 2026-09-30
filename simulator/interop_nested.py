"""Bounded accounting and exact call authority for the nested profile.

This is an internal foundation, not an enabled callback profile. Export
capture and the native V3 bridge must both admit a transition before these
authorities can reach machine execution. Existing V2/V3 dispatch is unchanged.
"""

from __future__ import annotations

from dataclasses import dataclass
import weakref

from shared.cells import MASK64
from simulator.errors import StepBudgetExceeded
from simulator.interop_exports import CallbackExportError


MAX_NESTED_DEPTH = 8
MAX_NESTED_SEMANTIC_STEPS = 65536
MAX_NESTED_LOCAL_STEPS = 4096
_MINT = object()


def _integer(value, minimum, maximum, label):
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} must be in {minimum}..{maximum}")
    return value


class _Authority:
    __slots__ = ()

    def __new__(cls, key=None):
        if key is not _MINT:
            raise TypeError("nested authority is issued by its engine")
        return super().__new__(cls)

    def __copy__(self):
        raise TypeError("nested authority cannot be copied")

    def __deepcopy__(self, memo):
        raise TypeError("nested authority cannot be copied")

    def __reduce__(self):
        raise TypeError("nested authority cannot be serialized")


class NestedChainToken(_Authority):
    __slots__ = ()


class NestedCallbackCheckpoint(_Authority):
    __slots__ = ()


class CapturedMachineCall(_Authority):
    __slots__ = ()


class MachineCallUse(_Authority):
    __slots__ = ()


@dataclass(frozen=True, slots=True)
class NestedCallbackReceipt:
    entered: bool
    completed: bool
    inclusive_semantic_steps: int
    chain_semantic_steps: int


@dataclass(frozen=True, slots=True)
class NestedChainReceipt:
    chain_semantic_steps: int
    callbacks_entered: int
    maximum_callback_depth: int
    cancelled: bool


@dataclass(frozen=True, slots=True)
class _TickState:
    chain_steps: int
    callback_steps: tuple[tuple[NestedCallbackCheckpoint, int], ...]


@dataclass(slots=True)
class _CallbackRecord:
    token: NestedCallbackCheckpoint
    handle: object
    invocation_id: int
    local_limit: int
    entered: bool = False
    running: bool = False
    completed: bool = False
    machine_use: object = None


@dataclass(slots=True)
class _MachineUseRecord:
    token: MachineCallUse
    checkpoint: NestedCallbackCheckpoint
    captured_call: CapturedMachineCall
    native_edge: object
    consumed: bool = False


@dataclass(frozen=True, slots=True)
class _CapturedCallRecord:
    token: CapturedMachineCall
    handle: object
    word: object
    word_xt: int
    implementation: object
    operations: tuple
    operation: object
    operation_index: int
    operation_xt: int
    target: object

    def verify(self, engine):
        from simulator.interop_closed import _require_metadata_routes
        from simulator.ir import Call

        _require_metadata_routes()
        try:
            live = engine._dictionary.resolve(self.word_xt)
        except KeyError:
            raise CallbackExportError("captured machine Call owner is no longer live") from None
        if (live is not self.word or type(self.word.xt) is not int or self.word.xt != self.word_xt
                or self.word.implementation is not self.implementation
                or self.implementation.operations is not self.operations
                or self.operations[self.operation_index] is not self.operation
                or type(self.operation) is not Call or type(self.operation.xt) is not int
                or self.operation.xt != self.operation_xt):
            raise CallbackExportError("captured machine Call changed")
        engine._nested_owner.call("_verify_nested_target", self.target)


class NestedMachineOwner:
    """One exact HybridRuntime, with routes pinned before customization.

    The engine retains only this adapter and captured dependency references;
    the HybridRuntime registration table remains the sole machine registry.
    """

    def __init__(self, engine, owner):
        from hybrid.runtime import (
            HybridRuntime, _NESTED_OWNER_ROUTES, _NESTED_OWNER_SPECIAL_ROUTES,
            _NESTED_OWNER_DICT_DESCRIPTOR, _NESTED_OWNER_FIELD_ROUTES, _NESTED_OWNER_ABSENT,
        )

        if type(owner) is not HybridRuntime:
            raise CallbackExportError("nested machine owner must be this runtime's exact HybridRuntime")
        self._engine = engine
        self._owner = weakref.ref(owner)
        self._owner_type = HybridRuntime
        self._routes = _NESTED_OWNER_ROUTES
        self._special_routes = _NESTED_OWNER_SPECIAL_ROUTES
        self._dictionary_descriptor = _NESTED_OWNER_DICT_DESCRIPTOR
        self._field_routes = _NESTED_OWNER_FIELD_ROUTES
        self._absent = _NESTED_OWNER_ABSENT
        self.require(owner)

    def require(self, owner=None):
        current = self._owner()
        if current is None or (owner is not None and owner is not current):
            raise CallbackExportError("nested machine owner is no longer the issued owner")
        if (type(current) is not self._owner_type or hasattr(self._owner_type, "__getattr__")
                or any(getattr(self._owner_type, name) is not original
                       for name, original in self._special_routes)
                or vars(self._owner_type).get("__dict__") is not self._dictionary_descriptor
                or any(vars(self._owner_type).get(name, self._absent) is not original
                       for name, original in self._field_routes)):
            raise CallbackExportError("nested machine owner attribute routing changed")
        namespace = self._dictionary_descriptor.__get__(current, self._owner_type)
        if type(namespace) is not dict or dict.get(namespace, "semantic") is not self._engine._runtime:
            raise CallbackExportError("nested machine semantic owner changed")
        for name, original in self._routes:
            if name in namespace or vars(type(current)).get(name) is not original:
                raise CallbackExportError("nested machine owner route changed")
        return current

    def call(self, operation, *arguments):
        owner = self.require()
        route = next((original for name, original in self._routes if name == operation), None)
        if route is None:
            raise CallbackExportError("unknown nested machine owner operation")
        return route(owner, *arguments)


class NestedChainAccounting:
    """One root ledger and at most eight live callback checkpoints.

    Only a current, entered checkpoint can charge a tick. Descendant ticks
    count once at the root and inclusively in every active ancestor. Private
    stack/effect guards surround this ledger in the eventual dispatcher.
    """

    def __init__(self, engine, adapter, owner, meter, semantic_limit):
        from simulator.interop_closed import capture_meter

        adapter.require(owner)
        self.engine, self.adapter = engine, adapter
        self.meter = meter
        self.namespace, self.starting_steps = capture_meter(meter)
        self.meter_budget, self.on_tick = meter.budget, meter._on_tick
        if (self.meter_budget is not None
                and (type(self.meter_budget) is not int or not 0 <= self.meter_budget <= MASK64)):
            raise CallbackExportError("nested callback meter budget is not exact")
        if not callable(self.on_tick):
            raise CallbackExportError("nested callback meter hook is unavailable")
        self.limit = _integer(semantic_limit, 0, MAX_NESTED_SEMANTIC_STEPS,
                              "nested callback semantic allowance")
        self.token = NestedChainToken(_MINT)
        self._tick_state = _TickState(0, ())
        self.callbacks_entered = 0
        self.maximum_depth = 0
        self._callbacks = []
        self._closed = False

    @property
    def steps(self):
        return self._tick_state.chain_steps

    def _inclusive_steps(self, checkpoint):
        return next((steps for token, steps in self._tick_state.callback_steps
                     if token is checkpoint), 0)

    def _require(self, *, cleanup=False):
        if self._closed or self.engine._nested_chain is not self:
            raise CallbackExportError("nested callback chain is not the issued active chain")
        if self.engine._runtime._callback_exports is not self.engine:
            raise CallbackExportError("nested callback engine owner changed")
        if not cleanup:
            self.adapter.require()
            self.engine._require_owner("dispatch a nested callback")

    def _record(self, checkpoint, *, cleanup=False):
        self._require(cleanup=cleanup)
        if (type(checkpoint) is not NestedCallbackCheckpoint or not self._callbacks
                or self._callbacks[-1].token is not checkpoint):
            raise CallbackExportError("nested callback checkpoint is not the current issued identity")
        return self._callbacks[-1]

    def _repair_meter(self):
        from simulator.interop_closed import repair_meter

        changed = repair_meter(self.meter, self.namespace, self.starting_steps + self.steps)
        if changed:
            self.engine._registration_failure = "nested callback accounting meter changed"
        # A host hook may corrupt controls and raise without changing steps.
        # Preserve the issued receipt for cleanup, but never admit another
        # dispatch through an altered budget, hook, or canonical meter route.
        try:
            self._require_meter()
        except BaseException:
            changed = True
        return changed

    def _require_meter(self):
        from simulator.interop_closed import capture_meter

        try:
            namespace, steps = capture_meter(self.meter)
            if (namespace is not self.namespace or steps != self.starting_steps + self.steps
                    or type(self.meter.budget) is not type(self.meter_budget)
                    or self.meter.budget != self.meter_budget or self.meter._on_tick is not self.on_tick):
                raise CallbackExportError("nested callback accounting meter changed")
        except BaseException:
            self.engine._registration_failure = "nested callback accounting meter changed"
            raise

    def begin_callback(self, handle, invocation_id, local_limit, *, via_use=None):
        self._require()
        _integer(invocation_id, 1, MASK64, "nested invocation ID")
        _integer(local_limit, 1, MAX_NESTED_LOCAL_STEPS, "nested callback local allowance")
        if len(self._callbacks) >= MAX_NESTED_DEPTH:
            raise CallbackExportError("nested callback depth exceeds eight")
        if self._callbacks:
            parent = self._callbacks[-1]
            use = parent.machine_use
            if (not parent.running or type(via_use) is not MachineCallUse or use is None
                    or use.token is not via_use or not use.consumed):
                raise CallbackExportError("nested callback has no active captured machine Call")
        elif via_use is not None:
            raise CallbackExportError("root callback cannot name a child Call authority")
        self._require_meter()
        token = NestedCallbackCheckpoint(_MINT)
        record = _CallbackRecord(token, handle, invocation_id, local_limit)
        self._callbacks.append(record)
        self.maximum_depth = max(self.maximum_depth, len(self._callbacks))
        return token

    def enter_callback(self, checkpoint, handle):
        record = self._record(checkpoint)
        if record.handle is not handle or record.entered:
            raise CallbackExportError("nested checkpoint admits one exact callback invocation")
        self._require_meter()
        record.entered = True
        record.running = True
        self.callbacks_entered += 1

    def leave_callback(self, checkpoint, *, completed):
        record = self._record(checkpoint, cleanup=True)
        if type(completed) is not bool or not record.running or record.machine_use is not None:
            raise CallbackExportError("nested callback cannot retire its active transition")
        if completed and self._inclusive_steps(checkpoint) == 0:
            raise CallbackExportError("nested callback completed without a semantic tick")
        record.running = False
        record.completed = completed

    def consume_callback(self, checkpoint):
        record = self._record(checkpoint, cleanup=True)
        if record.running or record.machine_use is not None:
            raise CallbackExportError("nested callback is still executing")
        self._repair_meter()
        receipt = NestedCallbackReceipt(record.entered, record.completed,
                                        self._inclusive_steps(checkpoint), self.steps)
        self._callbacks.pop()
        return receipt

    def tick(self, checkpoint):
        record = self._record(checkpoint)
        if not record.running or record.machine_use is not None:
            raise CallbackExportError("nested semantic tick has no active callback authority")
        self._require_meter()
        if self.steps >= self.limit:
            raise self.engine._budget_error("callback_semantic_limit", self.limit, self.steps)
        for active in self._callbacks:
            if not active.running:
                raise CallbackExportError("nested callback ancestor is not active")
            inclusive = self._inclusive_steps(active.token)
            if inclusive >= active.local_limit:
                raise self.engine._budget_error(
                    "callback_local_limit", active.local_limit, inclusive,
                )
        before = self.starting_steps + self.steps
        if self.meter_budget is not None and before >= self.meter_budget:
            raise StepBudgetExceeded(self.meter_budget)
        after = before + 1
        # Allocate the whole future receipt before publishing any work. The
        # one reference update in finally cannot leave ancestors half charged.
        next_ticks = _TickState(self.steps + 1, tuple(
            (active.token, self._inclusive_steps(active.token) + 1)
            for active in self._callbacks
        ))
        try:
            dict.__setitem__(self.namespace, "steps", after)
        finally:
            current = dict.get(self.namespace, "steps")
            if type(current) is int and current == after:
                object.__setattr__(self, "_tick_state", next_ticks)
        try:
            self.on_tick()
        except BaseException as error:
            try:
                self._repair_meter()
            except BaseException:
                self.engine._registration_failure = "nested callback meter cleanup could not be proved"
                try:
                    BaseException.add_note(error, self.engine._registration_failure)
                except BaseException:
                    pass
            raise
        self._require_meter()

    def issue_machine_use(self, checkpoint, captured_call, native_edge):
        record = self._record(checkpoint)
        if (not record.running or record.machine_use is not None
                or type(captured_call) is not CapturedMachineCall or native_edge is None):
            raise CallbackExportError("machine Call is outside its admitted callback transition")
        captured = self.engine._nested_calls.get(captured_call)
        if captured is None or captured.handle is not record.handle:
            raise CallbackExportError("machine Call is not captured by this callback")
        captured.verify(self.engine)
        token = MachineCallUse(_MINT)
        record.machine_use = _MachineUseRecord(token, checkpoint, captured_call, native_edge)
        return token

    def consume_machine_use(self, token):
        self._require()
        if type(token) is not MachineCallUse or not self._callbacks:
            raise CallbackExportError("machine Call authority is not issued here")
        record = self._callbacks[-1]
        use = record.machine_use
        if use is None or use.token is not token or use.consumed or not record.running:
            raise CallbackExportError("machine Call authority is stale or already consumed")
        captured = self.engine._nested_calls.get(use.captured_call)
        if captured is None:
            raise CallbackExportError("machine Call capture is no longer live")
        captured.verify(self.engine)
        use.consumed = True
        return captured, use.native_edge

    def finish_machine_use(self, token):
        self._require()
        if not self._callbacks:
            raise CallbackExportError("machine Call authority has no active callback")
        use = self._callbacks[-1].machine_use
        if type(token) is not MachineCallUse or use is None or use.token is not token or not use.consumed:
            raise CallbackExportError("machine Call authority is not the current consumed transition")
        self._callbacks[-1].machine_use = None

    def finish(self, *, cancelled=False):
        self._require(cleanup=True)
        if type(cancelled) is not bool or (self._callbacks and not cancelled):
            raise CallbackExportError("nested chain still owns callback checkpoints")
        self._repair_meter()
        receipt = NestedChainReceipt(self.steps, self.callbacks_entered, self.maximum_depth, cancelled)
        self._callbacks.clear()
        self._closed = True
        return receipt


def capture_machine_call(engine, handle, word, operation, operation_index, target):
    """Issue identity for a verified static Call; execution checks it again.

    The capture dispatcher, not callers or copied diagnostic metadata, chooses
    these objects. Each export deduplicates helpers by exact Word/operation.
    """
    from simulator.dictionary import Word
    from simulator.ir import Call
    from simulator.runtime import ColonDefinition
    from simulator.interop_closed import _require_metadata_routes

    _require_metadata_routes()
    if engine._nested_chain is not None or engine._active is not None:
        raise CallbackExportError("machine Call capture requires an idle export boundary")
    if (type(word) is not Word or type(word.implementation) is not ColonDefinition
            or type(operation) is not Call or type(operation_index) is not int
            or not 0 <= operation_index < len(word.implementation.operations)
            or word.implementation.operations[operation_index] is not operation):
        raise CallbackExportError("machine Call capture requires an exact static operation")
    engine._nested_owner.call("_verify_nested_target", target)
    if (type(operation.xt) is not int or type(target.word.xt) is not int
            or operation.xt != target.word.xt):
        raise CallbackExportError("machine Call does not name its exact registered target")
    for existing in engine._nested_calls.values():
        if (existing.handle is handle and existing.word is word and existing.operation is operation
                and existing.operation_index == operation_index):
            if existing.target is not target:
                raise CallbackExportError("captured machine Call cannot change its registered target")
            existing.verify(engine)
            return existing.token
    if len(engine._nested_calls) >= 65536:
        raise CallbackExportError("captured machine Call table exceeds 65536 operations")
    token = CapturedMachineCall(_MINT)
    engine._nested_calls[token] = _CapturedCallRecord(
        token, handle, word, word.xt, word.implementation, word.implementation.operations,
        operation, operation_index, operation.xt, target,
    )
    return token
