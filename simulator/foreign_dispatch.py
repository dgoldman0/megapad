"""Synchronous task transport owned by the ordinary semantic dispatcher.

This module schedules typed adapter events; it never interprets semantic IR.
Every callback reenters the existing dispatcher on the task's original stacks.
"""

from dataclasses import dataclass
from types import FunctionType

from shared.cells import CELL_BYTES
from shared.foreign_abi import (
    ForeignBudgetV1, ForeignCallbackRequestV1, ForeignCancellationV1,
    ForeignCompletedV1, ForeignFailedV1, ForeignRunnableYieldV1, ForeignSpanV1,
)
from simulator.foreign_control import ForeignContinuation, ForeignReturnControl
from simulator.foreign_cursor import MachineTurn, ForeignMachineCursor, _machine_state_cell
from simulator.foreign_effects import TaskEffectGuard, TaskEffectScope
from simulator.foreign_runtime import (
    ForeignRootLedger, ForeignTaskError, ForeignTaskBudgetExceeded, _StepMeter,
    _STACK_ROUTES, _namespace, ExecutionContext,
    _METADATA_ROUTES, _FunctionSeal,
)
from simulator.foreign_types import ForeignCallbackTarget, ForeignResumeTarget
from simulator.errors import ForthAbort, StepBudgetExceeded
from simulator.stacks import Continuation
from simulator.runtime import _TASK_METER_ROUTES, _TASK_METER_NAMESPACE, _DispatchFrame


_METER_ROUTES = _TASK_METER_ROUTES
_METER_NAMESPACE = _TASK_METER_NAMESPACE
_CONTEXT_FAULT = vars(ExecutionContext)["_host_control_fault"]


def _meter_namespace(meter):
    fields = vars(_StepMeter)
    if (type(meter) is not _StepMeter or any(type(key) is not str for key in fields)
            or any(fields.get(name) is not value for name, value in _METER_ROUTES)
            or any(name in fields for name in ("steps", "budget", "_on_tick", "__getattr__"))
            or _StepMeter.__getattribute__ is not object.__getattribute__
            or _StepMeter.__setattr__ is not object.__setattr__):
        raise ForeignTaskError("task original semantic meter routes changed")
    namespace = _METER_NAMESPACE.__get__(meter, _StepMeter)
    if type(namespace) is not dict or any(type(key) is not str for key in namespace):
        raise ForeignTaskError("task original semantic meter namespace changed")
    budget, steps = dict.get(namespace, "budget"), dict.get(namespace, "steps")
    if ((budget is not None and (type(budget) is not int or budget < 1))
            or type(steps) is not int or steps < 0):
        raise ForeignTaskError("task original semantic meter controls changed")
    return namespace


@dataclass(slots=True)
class _Invocation:
    binding: object
    resume: object
    base_pointer: int
    invocation_id: int = 0
    token: object = None
    request: object = None
    capture: object = None
    scope: object = None
    cookie: object = None
    callback_pointer: int = 0
    frontiers: tuple = ()
    frontier_losses: int = 0


@dataclass(frozen=True, slots=True)
class _Tail:
    capture: object
    scope: object
    frontiers: tuple
    index: int = 0
    losses: int = 0


class TaskDispatchRoot:
    """One original semantic root, meter, adapter owner, and spent ledger."""

    def __init__(self, engine, meter, root_id, limits,
                 _machine_factory=(_machine_state_cell, _machine_state_cell.__code__,
                                   _machine_state_cell.__defaults__),
                 _machine_metadata=tuple((kind, tuple(vars(kind).items()))
                                         for kind in (MachineTurn, ForeignMachineCursor)),
                 *, machine_turn=None):
        meter_namespace = _meter_namespace(meter)
        self.engine = engine
        self.context = engine._context
        self.ledger = ForeignRootLedger(meter, root_id, instruction_limit=limits[0],
            callback_limit=limits[1], entry_limit=limits[2], semantic_limit=limits[3])
        self.issuer = object()
        self.effects = TaskEffectGuard.issue(self.issuer, self.context.data, self.context.returns)
        self.frames = []
        self.tail = None
        self.adapter = None
        self.pending_binding = None
        self.cancelled = False
        self.closed = False
        self.busy = False
        self._unwinding_error = None
        self._meter_policy = (dict.get(meter_namespace, "budget"), dict.get(meter_namespace, "_on_tick"))
        self._meter_namespace = meter_namespace
        self._effects_close = TaskEffectGuard.close
        self._control_close = ForeignReturnControl.close
        self._cleanup_functions = (_FunctionSeal.capture(self._effects_close),
                                   _FunctionSeal.capture(self._control_close))
        self._machine_scheduled = machine_turn is not None
        self._machine_routes = _machine_metadata
        publishers = tuple(callback.__code__ for name, callback in engine._dispatch_routes
                           if name in ("begin_machine_turn", "_issue_machine_cursor",
                                       "resume_machine", "close"))
        factory, code, defaults = _machine_factory
        if (type(factory) is not FunctionType or factory.__code__ is not code
                or factory.__defaults__ is not defaults or factory.__kwdefaults__ is not None
                or factory.__closure__ is not None):
            raise ForeignTaskError("task original machine scheduling factory changed")
        self._machine_schedule = factory(self, publishers)
        self.control = None
        # Allocate host state first; binding the control is the final publish.
        try:
            self.control = self.context.returns.bind_foreign_control(self.issuer)
        except BaseException as failure:
            pending = self.context.returns._foreign_control
            cleanups = [(self._effects_close, self.effects)]
            if type(pending) is ForeignReturnControl and pending._issuer is self.issuer:
                cleanups.append((self._control_close, pending))
            for close, owned in cleanups:
                try:
                    close(owned, self.issuer)
                except BaseException as cleanup:
                    engine._execution_failure = cleanup
                    try:
                        BaseException.add_note(failure, "task control admission cleanup also failed")
                    except BaseException:
                        pass
            raise

    @property
    def active(self):
        return self.tail is not None or any(frame.capture is not None for frame in self.frames)

    def _machine_state(self):
        """Read the original state cell only through its retained routes."""
        self.engine._require_task(self.context)
        for kind, routes in self._machine_routes:
            fields = vars(kind)
            if (any(type(name) is not str for name in fields)
                    or kind.__getattribute__ is not object.__getattribute__
                    or "__getattr__" in fields
                    or any(fields.get(name) is not value for name, value in routes)
                    or any(name in fields and not any(key == name for key, _ in routes)
                           for name in ("__new__", "__getattribute__", "__getattr__"))):
                raise ForeignTaskError("task machine scheduling metadata routes changed")
        read, replace, routes = self._machine_schedule
        for function, code, globals_, defaults, kwdefaults, closure, fixed in routes:
            if (type(function) is not FunctionType or function.__code__ is not code
                    or function.__globals__ is not globals_ or function.__defaults__ is not defaults
                    or function.__kwdefaults__ is not kwdefaults or function.__closure__ is not closure
                    or any(cell.cell_contents is not value for cell, value in fixed)):
                raise ForeignTaskError("task machine scheduling holder routes changed")
        state = read()
        if type(state) is not tuple or len(state) != 2:
            raise ForeignTaskError("task machine scheduling holder changed")
        if self._machine_scheduled and state[0] is not None:
            machine_call, call_seal = self.engine._machine_methods[0][1:]
            call_seal.verify()
            machine_call(self.engine, "require_machine_selection", self.ledger.meter, state[0][2])
        return state

    def _machine_frame(self):
        runtime = self.engine._runtime
        frames = runtime._active_dispatches
        if type(frames) is not list or not frames:
            raise ForeignTaskError("task machine scheduling has no outer host frame")
        if len(frames) != 1 or runtime._active_input_states:
            raise ForeignTaskError("task machine scheduling cannot detach source or nested host dispatch")
        frame = frames[0]
        if (type(frame) is not _DispatchFrame or frame.context is not self.context
                or frame.meter is not self.ledger.meter or frame.root_id != self.ledger.root_id
                or frame.closed_guard is not None or frame.task_root is not self):
            raise ForeignTaskError("task machine scheduling changed its original host frame")
        return frame

    def _machine_instructions(self):
        self.engine.require_native_chain(self)
        ledger = self.ledger
        receipt = ledger.last_receipt
        instructions = ledger.instructions
        if type(instructions) is not int or instructions < 0:
            raise ForeignTaskError("task machine scheduling instruction projection changed")
        if receipt is None:
            if instructions:
                raise ForeignTaskError("task machine scheduling lacks its original receipt")
        else:
            ledger.settle(receipt, issued_receipt=receipt)
            if instructions != receipt.root_instructions:
                raise ForeignTaskError("task machine scheduling receipt projection changed")
        return instructions

    def begin_machine_turn(self, turn):
        """Admit one outer scheduling turn without renewing execution fuel."""
        state = self._machine_state()
        previous, cursor = state
        if not self._machine_scheduled:
            if turn is not None or previous is not None or cursor is not None:
                raise ForeignTaskError("synchronous task root cannot acquire machine scheduling")
            return
        frame = self._machine_frame()
        turn_kind = self._machine_routes[0][0]
        if type(turn) is not turn_kind or frame.machine_turn is not turn:
            raise ForeignTaskError("task machine turn is not its exact outer frame selection")
        limit = turn.limit
        if type(limit) is not int or not 1 <= limit <= 10_000_000:
            raise ForeignTaskError("task machine quantum instructions must be an exact integer in 1..10000000")
        instructions = self._machine_instructions()
        if previous is not None:
            old_frame, old_turn, original_limit, start = previous
            if limit != original_limit:
                raise ForeignTaskError("task machine scheduling cannot replace its original limit")
            if frame is old_frame:
                if turn is not old_turn or instructions < start:
                    raise ForeignTaskError("task machine turn changed during its original host dispatch")
                return
            if turn is old_turn or any(item is old_frame for item in self.engine._runtime._active_dispatches):
                raise ForeignTaskError("task machine scheduling cannot renew an active or repeated turn")
        current = (frame, turn, limit, instructions)
        self._machine_schedule[1](state, (current, cursor))

    def machine_turn_evidence(self):
        """The immutable original turn record for the existing parked witness."""
        state = self._machine_state()
        current = state[0]
        if not self._machine_scheduled:
            if current is not None or state[1] is not None:
                raise ForeignTaskError("synchronous task root acquired scheduling state")
            return None
        if type(current) is not tuple or len(current) != 4:
            raise ForeignTaskError("task machine turn has not been admitted")
        frame, turn, limit, start = current
        if (type(frame) is not _DispatchFrame or frame.machine_turn is not turn
                or frame.context is not self.context or frame.meter is not self.ledger.meter
                or type(frame.root_id) is not int or frame.root_id != self.ledger.root_id
                or frame.closed_guard is not None or frame.task_root is not self
                or type(turn) is not self._machine_routes[0][0]
                or type(turn.limit) is not int or turn.limit != limit
                or type(limit) is not int or type(start) is not int
                or self._machine_instructions() < start):
            raise ForeignTaskError("task original machine turn evidence changed")
        return current

    def _machine_quantum(self):
        if not self._machine_scheduled:
            return 65536
        frame, _turn, limit, start = self.machine_turn_evidence()
        if self._machine_frame() is not frame:
            raise ForeignTaskError("task machine scheduling has not admitted this host turn")
        spent = self._machine_instructions() - start
        if spent > limit:
            raise ForeignTaskError("task adapter exceeded the original machine turn allowance")
        return min(65536, limit - spent)

    def _issue_machine_cursor(self, event, frame):
        state = self._machine_state()
        current, previous = state
        if previous is not None:
            raise ForeignTaskError("task machine cursor already has a live issuance")
        if (type(event) is not ForeignRunnableYieldV1 or event.receipt is not self.ledger.last_receipt
                or not self.frames or frame is not self.frames[-1] or frame.request is not None
                or event.operation_token is not frame.token):
            raise ForeignTaskError("task machine cursor lacks its exact runnable invocation")
        self.machine_turn_evidence()
        kind, routes = self._machine_routes[1]
        values = (self.ledger.root_token, self.ledger.root_id, frame.invocation_id,
                  frame.token, event.receipt, True)
        cursor = object.__new__(kind)
        for name, value in zip(("root_token", "root_id", "invocation_id", "operation_token",
                                "receipt", "host_yield"), values):
            slot = next(descriptor for field_name, descriptor in routes if field_name == name)
            slot.__set__(cursor, value)
        record = (cursor, event, frame, frame.binding, frame.resume, current,
                  *values[:-1], self.context.data.pointer, self.context.returns.pointer)
        self._machine_schedule[1](state, (current, record))
        return cursor

    def machine_cursor_evidence(self, cursor):
        """Validate exact one-shot issuance without using cursor fields as authority."""
        state = self._machine_state()
        record = state[1]
        if type(record) is not tuple or len(record) != 13 or record[0] is not cursor:
            raise ForeignTaskError("task machine cursor is not the exact live issuance")
        (issued, event, frame, binding, resume, _turn, root_token, root_id,
         invocation_id, token, receipt, data_pointer, return_pointer) = record
        kind = self._machine_routes[1][0]
        if (type(issued) is not kind or issued.root_token is not root_token
                or type(issued.root_id) is not int or issued.root_id != root_id
                or type(issued.invocation_id) is not int or issued.invocation_id != invocation_id
                or issued.operation_token is not token or issued.receipt is not receipt
                or issued.host_yield is not True or self.ledger.root_token is not root_token
                or self.ledger.root_id != root_id or self.ledger.last_receipt is not receipt
                or type(event) is not ForeignRunnableYieldV1 or event.receipt is not receipt
                or event.operation_token is not token or not self.frames or self.frames[-1] is not frame
                or frame.binding is not binding or frame.resume is not resume or frame.token is not token
                or type(frame.invocation_id) is not int or frame.invocation_id != invocation_id
                or frame.request is not None or frame.capture is not None or frame.cookie is not None
                or self.context.data.pointer != data_pointer or self.context.returns.pointer != return_pointer):
            raise ForeignTaskError("task machine cursor changed its accepted invocation evidence")
        if self.engine.require_definition(binding.word) is not binding:
            raise ForeignTaskError("task machine cursor binding changed")
        self.machine_turn_evidence()
        self.ledger.settle(receipt, issued_receipt=receipt)
        return record

    def resume_machine(self, cursor):
        record = self.machine_cursor_evidence(cursor)
        state = self._machine_state()
        if self._machine_frame() is not state[0][0] or state[0] is record[5]:
            raise ForeignTaskError("task machine cursor requires one fresh outer host turn")
        self._machine_schedule[1](state, (state[0], None))
        return self._drive(record[1])

    def _scope(self):
        if self.tail is not None:
            return self.tail.capture, self.tail.scope
        for frame in reversed(self.frames):
            if frame.capture is not None:
                return frame.capture, frame.scope
        return None, None

    def _attach(self):
        capture, scope = self._scope()
        if capture is None:
            self.effects.detach(self.issuer)
        else:
            self.effects.attach(self.issuer, scope)

    def require_target(self, target, ip=0):
        self.engine._require_task(self.context)
        capture, scope = self._scope()
        if capture is not None:
            self.effects.require_binding(self.issuer, scope)
            return capture.require_target(self.engine, target, entry_ip=ip,
                                          permit_machine=self.tail is None)

    def tick(self, target, ip=0, operation=None):
        require_task = self.engine._require_task
        require_meter, require_target, reconcile = self._require_meter, self.require_target, self.reconcile
        ledger, context = self.ledger, self.context
        meter, meter_namespace = ledger.meter, self._meter_namespace
        meter_descriptor = _METER_NAMESPACE
        root_namespace = self.engine._dispatch_namespace.__get__(self, type(self))
        issue_host_abort = self.engine._runtime._private_host_abort.issue_task
        self._require_meter()
        evidence = self.require_target(target, ip)
        active = self.active
        if active:
            self.ledger.require_semantic_step()
        before = dict.get(meter_namespace, "steps")
        budget, hook = self._meter_policy
        if budget is not None and before >= budget:
            raise StepBudgetExceeded(budget)
        # Publish the charged root and every surviving ancestor together,
        # before host accounting can throw or modify a meter projection.
        prior_state = ledger._state
        try:
            if active:
                ledger.charge_semantic_step()
            dict.__setitem__(meter_namespace, "steps", before + 1)
            try:
                hook()
            except ForthAbort as failure:
                issue_host_abort(self, failure)
                if dict.get(root_namespace, "_unwinding_error") is None:
                    dict.__setitem__(root_namespace, "_unwinding_error", failure)
                raise
        except BaseException:
            # Restore only the charged public projection, preserving the
            # original hook failure and the already-published receipt.
            if ledger._state is not prior_state:
                meter_descriptor.__set__(meter, meter_namespace)
                dict.__setitem__(meter_namespace, "steps", before + 1)
            raise
        try:
            require_task(context)
            if self._machine_scheduled:
                self.machine_turn_evidence()
            require_meter()
            projected = dict.get(meter_namespace, "steps")
            if type(projected) is not int or projected != before + 1:
                raise ForeignTaskError("task semantic tick projection changed during accounting")
        except BaseException:
            meter_descriptor.__set__(meter, meter_namespace)
            dict.__setitem__(meter_namespace, "steps", before + 1)
            raise
        reconcile()
        require_meter()
        after = require_target(target, ip)
        if after is not evidence:
            raise ForeignTaskError("task target evidence changed during accounting")
        if active and operation is not None and (after is None or after.operations[ip] is not operation):
            raise ForeignTaskError("task semantic operation changed during accounting")

    def _require_meter(self):
        meter = self.ledger.meter
        budget, hook = self._meter_policy
        namespace = _meter_namespace(meter)
        if (namespace is not self._meter_namespace or type(namespace) is not dict
                or any(type(key) is not str for key in namespace)):
            raise ForeignTaskError("task original semantic meter namespace changed")
        steps, actual_budget = dict.get(namespace, "steps"), dict.get(namespace, "budget")
        if (dict.get(namespace, "_on_tick") is not hook or type(steps) is not int or steps < 0
                or (budget is None and actual_budget is not None)
                or (budget is not None and (type(actual_budget) is not int or actual_budget != budget))):
            raise ForeignTaskError("task original semantic meter policy changed")

    def require_access(self, address, width, access):
        capture, _scope = self._scope()
        if capture is not None:
            capture.require_access(self.engine, address, width, access)
            self.effects.require_access(address, width, access)

    def _budget(self, binding, *, quantum):
        leaf = self.ledger._state.active[-1] if self.ledger._state.active else None
        continuing = leaf is not None and leaf.registration is binding.metadata.registration
        return ForeignBudgetV1(
            invocation_instructions_remaining=binding.metadata.max_instructions - (leaf.instructions if continuing else 0),
            root_instructions_remaining=self.ledger.instruction_limit - self.ledger.instructions,
            invocation_callbacks_remaining=binding.metadata.max_callbacks - (leaf.callbacks if continuing else 0),
            root_callbacks_remaining=self.ledger.callback_limit - self.ledger.callbacks,
            quantum_instructions=quantum,
        )

    def _latest(self, *, starting=None):
        receipt = self._owned_call("last_receipt")
        if receipt is not None:
            if receipt is self.ledger.last_receipt:
                self.ledger.settle(receipt, issued_receipt=receipt)
            else:
                self.ledger.settle(receipt, issued_receipt=receipt,
                                   starting_operation=starting)
            self.engine.record_native_receipt(self, receipt)
        return receipt

    def _owned_call(self, name, *args, **kwargs):
        engine = self.engine
        namespace = engine._dispatch_namespace.__get__(self, type(self))
        issue_host_abort = engine._runtime._private_host_abort.issue_task
        prior = self.busy
        dict.__setitem__(namespace, "busy", True)
        try:
            return self.adapter.call(name, *args, **kwargs)
        except ForthAbort as failure:
            issue_host_abort(self, failure)
            if dict.get(namespace, "_unwinding_error") is None:
                dict.__setitem__(namespace, "_unwinding_error", failure)
            if name in ("cancel_suffix", "cancel_all"):
                engine._execution_failure = failure
            raise
        except BaseException as failure:
            if name in ("cancel_suffix", "cancel_all"):
                engine._execution_failure = failure
            raise
        finally:
            dict.__setitem__(namespace, "busy", prior)

    def _transition(self, name, *args, starting=None, **kwargs):
        self.effects.detach(self.issuer)
        self.busy = True
        try:
            event = self._owned_call(name, *args, **kwargs)
        except BaseException as failure:
            # The adapter publishes its receipt before allocating an event.
            # Recovery consumes that receipt once even when delivery failed.
            try:
                self._latest(starting=starting)
            except BaseException:
                try:
                    BaseException.add_note(failure, "task receipt recovery also failed")
                except BaseException:
                    pass
            raise
        finally:
            self.busy = False
        if type(event) not in (ForeignCallbackRequestV1, ForeignCompletedV1,
                              ForeignRunnableYieldV1, ForeignFailedV1):
            raise ForeignTaskError("adapter returned an unknown task event")
        type(event).__post_init__(event)
        receipt = self._owned_call("last_receipt")
        self.ledger.settle(event.receipt, issued_receipt=receipt, starting_operation=starting)
        self.engine.record_native_receipt(self, event.receipt)
        return event

    def begin(self, word, resume):
        self.require_target(word)
        self.ledger.require_entry()
        binding = self.engine.require_definition(word)
        if self.adapter is not None and self.adapter.adapter is not binding.adapter.adapter:
            raise ForeignTaskError("task root requires one exact adapter owner")
        if self.tail is not None:
            raise ForeignTaskError("retired semantic tail has no machine-entry authority")
        data, returns = self.context.data, self.context.returns
        signature = binding.metadata.signature
        # Read inputs while still in the caller's scope, without consuming any.
        arguments = tuple(data.peek(index) for index in range(signature.input_cells - 1, -1, -1))
        base = data.pointer + signature.input_cells * CELL_BYTES
        if base - signature.output_cells * CELL_BYTES < data.floor:
            raise ForeignTaskError("task result exceeds original data stack capacity")
        parent = self.frames[-1].request if self.frames else None
        if self.frames and parent is None:
            raise ForeignTaskError("task child requires its parent's parked callback")
        frame = _Invocation(binding, resume, base)
        original_data_pointer, original_return_pointer = data.pointer, returns.pointer
        self.adapter = binding.adapter
        self.pending_binding = binding
        event = self._transition("begin", binding.operation, arguments,
            root_token=self.ledger.root_token, root_id=self.ledger.root_id,
            budget=self._budget(binding, quantum=0), parent=parent,
            starting=binding.metadata)
        if (type(event) is not ForeignRunnableYieldV1 or not event.receipt.invocation_started
                or event.receipt.instructions or event.receipt.cycles or event.receipt.callback_requests):
            raise ForeignTaskError("task begin must be an admission-only zero-work yield")
        self.engine.require_definition(word)
        if data.pointer != original_data_pointer or returns.pointer != original_return_pointer:
            raise ForeignTaskError("task admission changed the original stack frontiers")
        frame.invocation_id, frame.token = event.receipt.invocation_id, event.operation_token
        self.frames.append(frame)
        self.pending_binding = None
        # Accepted admission precedes these original, observable input pops.
        self._attach()
        for _ in range(signature.input_cells):
            data.pop()
        self.effects.detach(self.issuer)
        return self._drive(event)

    def _drive(self, event):
        while True:
            self.engine._require_task(self.context)
            if self._machine_scheduled:
                self.machine_turn_evidence()
            frame = self.frames[-1]
            frame.token = event.operation_token
            if type(event) is ForeignRunnableYieldV1:
                quantum = self._machine_quantum()
                budget = self._budget(frame.binding, quantum=quantum)
                # A runnable adapter value cannot turn exhausted execution
                # fuel into a host scheduling boundary.
                if not budget.invocation_instructions_remaining or not budget.root_instructions_remaining:
                    raise ForeignTaskBudgetExceeded("task machine instruction allowance exhausted")
                if not quantum:
                    return self._issue_machine_cursor(event, frame)
                event = self._transition("advance", frame.token,
                                         budget=budget)
                if event.receipt.instructions > quantum:
                    raise ForeignTaskError("task adapter exceeded the original machine advance quantum")
                if type(event) is ForeignRunnableYieldV1 and not event.receipt.instructions:
                    raise ForeignTaskError("task adapter made no progress with positive quantum")
                continue
            if type(event) is ForeignFailedV1:
                failure = ForeignTaskError(f"task machine failed: {event.kind}: {event.detail}")
                failure.event = event
                raise failure
            if type(event) is ForeignCompletedV1:
                if self.context.data.pointer != frame.base_pointer:
                    raise ForeignTaskError("task machine completion changed semantic data frontier")
                if len(event.outputs) != frame.binding.metadata.signature.output_cells:
                    raise ForeignTaskError("task machine result arity differs from registration")
                self.frames.pop()
                self._attach()
                for value in event.outputs:
                    self.context.data.push(value)
                return frame.resume
            if self.context.data.pointer != frame.base_pointer:
                raise ForeignTaskError("task callback request changed the original data frontier")
            capture = self.engine.require_export(event.export)
            if len(event.arguments) != capture.metadata.signature.input_cells:
                raise ForeignTaskError("task callback argument arity differs from capture")
            data, returns = self.context.data, self.context.returns
            data.require_push_capacity(len(event.arguments))
            returns.require_push_capacity(1)
            # Capture only already-active continuation authority, before any
            # callback argument, foreign cookie or guest helper is pushed.
            frontiers = self._capture_frontiers()
            scope = self.effects.scope(self.issuer, capture.metadata.task_grants)
            frame.capture, frame.scope = capture, scope
            frame.request = event
            frame.callback_pointer = data.pointer
            frame.frontiers = frontiers
            frame.frontier_losses = 0
            self.ledger.begin_callback(frame.invocation_id, capture.metadata.max_semantic_steps)
            self.effects.attach(self.issuer, scope)
            for value in event.arguments:
                data.push(value)
            # Engine-owned control publication still obeys the captured grant.
            self.effects.require_access(returns.pointer - CELL_BYTES, CELL_BYTES, "write")
            frame.cookie = self.control.push(self.issuer, root_id=self.ledger.root_id,
                frame_id=frame.invocation_id, request_id=event.request_sequence)
            return ForeignCallbackTarget(capture.entry)

    def callback_return(self, cookie=None):
        if not self.frames or self.tail is not None:
            raise ForeignTaskError("retired task callback has no return authority")
        frame = self.frames[-1]
        if frame.capture is None or (cookie is not None and cookie is not frame.cookie):
            raise ForeignTaskError("task return does not name its issued callback")
        data, returns = self.context.data, self.context.returns
        self.engine._verify_export(frame.capture)
        if returns.pointer != frame.cookie.slot_address:
            raise ForeignTaskError("task callback returned with a different return frontier")
        count = frame.capture.metadata.signature.output_cells
        if data.pointer != frame.callback_pointer - count * CELL_BYTES:
            raise ForeignTaskError("task callback returned with the wrong data arity")
        outputs = tuple(data.peek(index) for index in range(count - 1, -1, -1))
        self.effects.require_access(returns.pointer, CELL_BYTES, "read")
        self.control.consume(self.issuer, frame.cookie)
        retired = self.control.drain_retired(self.issuer)
        if len(retired) != 1 or retired[0].entry is not frame.cookie:
            raise ForeignTaskError("normal task return retired an unexpected suffix")
        for _ in range(count):
            data.pop()
        request = frame.request
        self.ledger.end_callback(frame.invocation_id)
        self.effects.detach(self.issuer)
        self.effects.release_scope(self.issuer, frame.scope)
        frame.capture = frame.scope = frame.cookie = frame.request = None
        frame.frontiers = ()
        frame.frontier_losses = 0
        event = self._transition("reply", request.request_token, outputs,
                                 budget=self._budget(frame.binding, quantum=0))
        return self._drive(event)

    def _cancellation(self, result, expected, parent=None):
        if type(result) is not ForeignCancellationV1:
            raise ForeignTaskError("adapter returned an unknown cancellation value")
        ForeignCancellationV1.__post_init__(result)
        if result.receipt is not self.ledger.last_receipt or result.retired_invocation_ids != expected:
            raise ForeignTaskError("adapter cancellation differs from its owned suffix")
        if parent is None:
            if result.surviving_parent_id is not None or result.surviving_parent_token is not None:
                raise ForeignTaskError("task cancellation invented a surviving parent")
        elif (result.surviving_parent_id != parent.invocation_id
              or result.surviving_parent_token is not parent.request.request_token):
            raise ForeignTaskError("task cancellation changed its parent's pending authority")
        self.ledger.retire_suffix(expected)
        self.engine.record_native_cancellation(self, result)
        # Cancellation invalidates a parked runnable issuance without calling
        # any replaceable function on the failure/cleanup path. This is the
        # original state cell retained in the engine-owned holder evidence.
        state_cell = self._machine_schedule[2][0][5][0]
        state = state_cell.cell_contents
        if type(state) is tuple and len(state) == 2:
            state_cell.cell_contents = (state[0], None)
        else:
            state_cell.cell_contents = (None, None)

    def reconcile(self):
        if self.closed:
            return
        capture, scope = self._scope()
        if capture is not None:
            parked = self._machine_state()[1] if self._machine_scheduled else None
            runnable = (type(parked) is tuple and len(parked) == 13 and self.frames
                        and parked[2] is self.frames[-1] and self.frames[-1].request is None)
            if runnable:
                # A child machine parks with semantic effects detached. Its
                # parent's captured scope remains issued, but confers no stack
                # access until normal completion reattaches it. Reconcile raw
                # foreign-cookie losses before the full cursor pointer proof.
                self.engine._verify_export(capture)
                TaskEffectGuard._require(self.effects, self.issuer)
                record = next((item for item in self.effects._scopes if item[0] is scope), None)
                if (type(scope) is not TaskEffectScope or record is None
                        or scope._issuer is not self.issuer or scope.grants is not record[1]
                        or scope._retired is not False or self.effects._scope is not None
                        or self.context.data._task_effect_guard is not None
                        or self.context.returns._task_effect_guard is not None):
                    raise ForeignTaskError("runnable task lost its detached ancestor scope")
            else:
                self.effects.require_binding(self.issuer, scope)
        self.control.reconcile(self.issuer)
        retired = self.control.drain_retired(self.issuer)
        if retired:
            prior_tail = self.tail
            matches = [index for index, frame in enumerate(self.frames)
                       if any(item.entry is frame.cookie for item in retired)]
            if not matches:
                raise ForeignTaskError("foreign retirement has no dispatch owner")
            first = min(matches)
            removed = self.frames[first:]
            active_capture, active_scope = self._scope()
            if prior_tail is not None:
                frontiers, losses = prior_tail.frontiers, prior_tail.losses
            else:
                source = next(frame for frame in reversed(self.frames)
                              if frame.capture is active_capture)
                frontiers, losses = source.frontiers, source.frontier_losses
            floor = max(item.slot_address + CELL_BYTES for item in retired)
            expected = tuple(frame.invocation_id for frame in reversed(removed))
            result = self._owned_call("cancel_suffix", removed[0].token)
            self._latest()
            self._cancellation(result, expected, self.frames[first - 1] if first else None)
            self.cancelled = True
            self.effects.detach(self.issuer)
            del self.frames[first:]
            for frame in removed:
                if frame.scope is not None and frame.scope is not active_scope:
                    self.effects.release_scope(self.issuer, frame.scope)
            if active_capture is not None:
                self.tail = _Tail(active_capture, active_scope, frontiers,
                                  prior_tail.index if prior_tail is not None else 0, losses)
            self._attach()
            # Cancellation and its real receipt are settled before a lost
            # frontier can reject the remainder of the original semantic tail.
            if self.tail is not None:
                self._advance_tail(floor)
        for frame in self.frames:
            if frame.capture is not None:
                frame.frontier_losses = self._frontier_losses(
                    frame.frontiers, frame.frontier_losses, self.context.returns.pointer)
        if self.tail is not None:
            self.engine._verify_export(self.tail.capture)
            # A later ordinary RP! or raw write may cross a frontier without
            # retiring another machine. It still advances the same vector.
            self._advance_tail(self.context.returns.pointer)

    def _capture_frontiers(self):
        returns = self.context.returns
        self.control.reconcile(self.issuer)
        floor = returns.pointer
        count = (returns.empty_pointer - floor) // CELL_BYTES
        # Use the original ceiling, not remaining fuel: inspecting evidence
        # charges no guest work and a later callback cannot renew that fuel.
        if count > self.ledger.semantic_limit:
            raise ForeignTaskError("retained tail frontier exceeds its original finite scan allowance")
        frontiers = []
        for index in range(count):
            address = floor + index * CELL_BYTES
            record = dict.get(returns._continuations, address)
            if (type(record) is tuple and len(record) == 2
                    and type(record[0]) in (Continuation, ForeignContinuation)
                    and type(record[1]) is int
                    and returns._memory_view.read64(address) == record[1]):
                entry = record[0]
                values = ((entry.xt, entry.ip, entry.root, entry.dispatch_id, entry.fault_abort)
                          if type(entry) is Continuation else None)
                frontier = (entry, address, record[1], values)
                if self._frontier_live(frontier):
                    frontiers.append(frontier)
        return tuple(frontiers)

    def _frontier_live(self, frontier):
        entry, address, raw, values = frontier
        returns = self.context.returns
        record = dict.get(returns._continuations, address)
        if (returns.pointer > address or type(record) is not tuple or len(record) != 2
                or record[0] is not entry or type(record[1]) is not int
                or record[1] != raw or returns._memory_view.read64(address) != raw):
            return False
        if type(entry) is ForeignContinuation:
            # Matching bytes and metadata cannot revive a retired foreign
            # cookie, even if its diagnostic retirement flag was repaired.
            return any(live.entry is entry for live in self.control._live)
        if type(entry) is not Continuation:
            return False
        return (type(entry.xt) is int and type(entry.ip) is int
                and type(entry.root) is bool and type(entry.dispatch_id) is int
                and entry.xt == values[0] and entry.ip == values[1]
                and entry.root is values[2] and entry.dispatch_id == values[3]
                and entry.fault_abort is values[4])

    def _frontier_losses(self, frontiers, losses, floor):
        # Check the entire original vector. A later row may be overwritten
        # and repaired while a nearer boundary is still live; observing that
        # loss permanently excludes it even before the callback is retired.
        for index, frontier in enumerate(frontiers):
            bit = 1 << index
            if not losses & bit and (frontier[1] < floor or not self._frontier_live(frontier)):
                losses |= bit
        return losses

    def _advance_tail(self, floor):
        tail = self.tail
        losses = self._frontier_losses(tail.frontiers, tail.losses, floor)
        index = tail.index
        while index < len(tail.frontiers) and losses & (1 << index):
            index += 1
        if index != tail.index or losses != tail.losses:
            # Publish the monotonic loss before raising. Neither a subsequent
            # raw repair nor a newly pushed helper can select an earlier row.
            self.tail = _Tail(tail.capture, tail.scope, tail.frontiers, index, losses)
        if index == len(tail.frontiers):
            raise ForeignTaskError("retained task tail lost its pre-unwind continuation")

    def ordinary_return(self, continuation):
        if (self.tail is not None and self.tail.index < len(self.tail.frontiers)
                and continuation is self.tail.frontiers[self.tail.index][0]):
            self.effects.detach(self.issuer)
            self.effects.release_scope(self.issuer, self.tail.scope)
            self.tail = None
            self._attach()

    def cleanup_safe(self):
        try:
            self.engine.require_cleanup_routes()
            if self.context is not self.engine._context:
                return False
            for actual, evidence, (kind, routes) in zip(
                    (self.context.data, self.context.returns), self.engine._stacks, _STACK_ROUTES):
                if actual is not evidence[0]:
                    return False
                _namespace(actual, kind, routes)
                if self.closed and actual._task_effect_guard is not None:
                    return False
            if self.closed and self.context.returns._foreign_control is not None:
                return False
            return True
        except BaseException:
            return False

    def mark_unsafe_cleanup(self):
        _CONTEXT_FAULT.__set__(self.context, "task host failure changed trusted stack cleanup routes")

    def _close_helper(self, kind, instance, close, function):
        fields = vars(kind)
        if any(type(key) is not str for key in fields):
            raise ForeignTaskError("task helper cleanup namespace changed")
        original = next(routes for owner, routes in _METADATA_ROUTES if owner is kind)
        if any(name != "close" and fields.get(name) is not value for name, value in original):
            raise ForeignTaskError("task helper cleanup route changed")
        function.verify()
        close(instance, self.issuer)

    def close(self, *, completed):
        if self.closed:
            return
        self._machine_schedule[2][0][5][0].cell_contents = (None, None)
        original = None

        def failed(error):
            nonlocal original
            if original is None:
                original = error
            else:
                try:
                    BaseException.add_note(original, "another task cleanup operation also failed")
                except BaseException:
                    pass

        if self.adapter is not None:
            try:
                starting = self.pending_binding.metadata if self.pending_binding is not None else None
                self._latest(starting=starting)
            except BaseException as error:
                failed(error)
            if self.ledger.depth or self.frames or self.pending_binding is not None:
                try:
                    result = self._owned_call("cancel_all")
                    expected = tuple(item.invocation_id for item in reversed(self.ledger._state.active))
                    self._cancellation(result, expected)
                    self.cancelled = True
                except BaseException as error:
                    failed(error)
        # Canonical helper bodies still write the original stack owners. If a
        # hook replaced those setters or backing routes, native cancellation
        # above remains valid, but even the original helper must not use them.
        if self.engine.task_cleanup_safe(self):
            try:
                self._close_helper(TaskEffectGuard, self.effects, self._effects_close, self._cleanup_functions[0])
            except BaseException as error:
                failed(error)
            try:
                self._close_helper(ForeignReturnControl, self.control, self._control_close, self._cleanup_functions[1])
            except BaseException as error:
                failed(error)
        else:
            self.engine.mark_task_cleanup_unsafe(self)
            failed(ForeignTaskError("task host failure changed trusted stack cleanup routes"))
        self.frames.clear()
        self.tail = None
        self.closed = True
        if not completed or original is not None:
            self.cancelled = True
        if not self.engine.task_cleanup_safe(self):
            self.engine.mark_task_cleanup_unsafe(self)
            failed(ForeignTaskError("task host failure changed trusted stack cleanup routes"))
        if original is not None:
            raise original


__all__ = ["TaskDispatchRoot"]
