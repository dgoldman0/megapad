"""Deterministic, identity-checked reference adapter for dispatcher tests.

Scripts contain data only. They cannot call Python, emulate guest THROW, or
import an execution backend. A Store touches only the supplied exact bytearray.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared.cells import MASK64
from shared.foreign_abi import (
    MAX_DEPTH, MAX_INVOCATION_INSTRUCTIONS, MAX_ROOT_ENTRIES, MAX_CALLBACK_REQUESTS,
    ForeignAccessV1, ForeignBudgetV1, ForeignCallbackRequestV1, ForeignCancellationV1,
    ForeignCompletedV1, ForeignExportV1, ForeignFailedV1, ForeignFailureKindV1,
    ForeignOperationV1, ForeignReceiptV1, ForeignRunnableYieldV1, ForeignSignatureV1,
    ForeignSpanV1, ForeignStateV1,
)


def _integer(value, label, minimum=0, maximum=MASK64):
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} is outside its finite bounds")


def _exact(value, kind):
    if type(value) is not kind:
        raise TypeError(f"expected exact {kind.__name__}")
    kind.__post_init__(value)


def _work(instructions, cycles, *, zero=False):
    _integer(instructions, "instructions", 0 if zero else 1, MAX_INVOCATION_INSTRUCTIONS)
    _integer(cycles, "cycles", instructions)
    if instructions == 0 and cycles != 0:
        raise ValueError("zero instructions require zero cycles")


@dataclass(frozen=True, slots=True)
class Input:
    index: int

    def __post_init__(self):
        _integer(self.index, "input index", 0, 7)


@dataclass(frozen=True, slots=True)
class Reply:
    index: int

    def __post_init__(self):
        _integer(self.index, "reply index", 0, 7)


def _cells(values, *, expressions=False):
    if type(values) is not tuple:
        raise TypeError("cells must be an exact tuple")
    if len(values) > 8:
        raise ValueError("cell tuple exceeds eight cells")
    for value in values:
        if expressions and type(value) in (Input, Reply):
            type(value).__post_init__(value)
        else:
            _integer(value, "cell")


@dataclass(frozen=True, slots=True, kw_only=True)
class Callback:
    export: ForeignExportV1
    arguments: tuple
    site: int = 0
    instructions: int = 1
    cycles: int = 1
    children: tuple[ForeignOperationV1, ...] = ()

    def __post_init__(self):
        _exact(self.export, ForeignExportV1)
        _cells(self.arguments, expressions=True)
        if len(self.arguments) != self.export.signature.input_cells:
            raise ValueError("callback script argument arity differs from its export")
        _integer(self.site, "callback site", 0, 15)
        _work(self.instructions, self.cycles)
        if type(self.children) is not tuple:
            raise TypeError("callback children must be an exact tuple")
        if len(self.children) > 64:
            raise ValueError("callback children exceed the finite bound")
        for child in self.children:
            _exact(child, ForeignOperationV1)


@dataclass(frozen=True, slots=True, kw_only=True)
class Return:
    outputs: tuple
    instructions: int = 1
    cycles: int = 1

    def __post_init__(self):
        _cells(self.outputs, expressions=True)
        _work(self.instructions, self.cycles)


@dataclass(frozen=True, slots=True, kw_only=True)
class Failure:
    kind: ForeignFailureKindV1 | str
    detail: str = ""
    instructions: int = 0
    cycles: int = 0
    instruction_pc: int | None = None

    def __post_init__(self):
        if type(self.kind) not in (str, ForeignFailureKindV1):
            raise TypeError("failure kind must be exact")
        object.__setattr__(self, "kind", ForeignFailureKindV1(self.kind))
        if type(self.detail) is not str:
            raise TypeError("failure detail must be an exact string")
        if len(self.detail) > 1024:
            raise ValueError("failure detail exceeds the finite bound")
        _work(self.instructions, self.cycles, zero=True)
        if self.instruction_pc is not None:
            _integer(self.instruction_pc, "instruction PC")


@dataclass(frozen=True, slots=True, kw_only=True)
class Store:
    address: int
    data: bytes
    cycles: int = 1

    def __post_init__(self):
        _integer(self.address, "store address")
        if type(self.data) is not bytes:
            raise TypeError("store data must be exact bytes")
        if not 1 <= len(self.data) <= 8 or len(self.data) - 1 > MASK64 - self.address:
            raise ValueError("store must fit one nonwrapping one-to-eight-byte access")
        _work(1, self.cycles)


def _grant_values(grants):
    if type(grants) is not tuple:
        raise TypeError("grants must be an exact tuple")
    if len(grants) > 16:
        raise ValueError("grants exceed the finite bound")
    values = []
    for grant in grants:
        _exact(grant, ForeignSpanV1)
        values.append((grant.base, grant.size, grant.access))
    return tuple(values)


def _export_seal(export):
    _exact(export, ForeignExportV1)
    return (export, export.export, export.signature,
            (export.signature.input_cells, export.signature.output_cells),
            export.task_grants, _grant_values(export.task_grants), export.max_semantic_steps)


@dataclass(frozen=True, slots=True)
class _Binding:
    operation: ForeignOperationV1
    registration: object
    signature: ForeignSignatureV1
    arity: tuple[int, int]
    grants: tuple[ForeignSpanV1, ...]
    grant_values: tuple
    limits: tuple[int, int]
    script: tuple
    exports: tuple


@dataclass(slots=True)
class _Frame:
    binding: _Binding
    arguments: tuple[int, ...]
    token: object
    invocation_id: int
    parent_id: int | None
    instruction_limit: int
    callback_limit: int
    instructions: int = 0
    cycles: int = 0
    callbacks: int = 0
    index: int = 0
    offset: int = 0
    reply_values: tuple[int, ...] = ()
    state: str = "runnable"
    callback: Callback | None = None
    request_token: object | None = None
    request: ForeignCallbackRequestV1 | None = None


class ScriptedForeignAdapter:
    """One issued operation registry, root ledger and at most eight live frames."""

    def __init__(self, memory: bytearray | None = None, *, memory_base=0):
        if memory is not None and type(memory) is not bytearray:
            raise TypeError("reference memory must be an exact bytearray")
        _integer(memory_base, "memory base")
        if memory is not None and len(memory) > MASK64 + 1 - memory_base:
            raise ValueError("reference memory wraps the address space")
        self._memory, self._memory_base = memory, memory_base
        self._bindings = {}
        self._frames = []
        self._root_token = None
        self._root_id = self._sequence = self._entries = self._next_invocation = 0
        self._root_instructions = self._root_cycles = self._root_callbacks = 0
        self._root_instruction_limit = self._root_callback_limit = 0
        self._last = None
        self._admitted_inputs = []
        self._replies = []
        self._delivery_failure = None
        self._cancel_failure = None

    @property
    def memory(self):
        return self._memory

    @property
    def memory_base(self):
        return self._memory_base

    @property
    def admitted_inputs(self):
        return tuple(self._admitted_inputs)

    @property
    def replies(self):
        return tuple(self._replies)

    @property
    def active_invocations(self):
        return tuple(frame.invocation_id for frame in self._frames)

    def register(self, signature, script, *, machine_grants=(),
                 max_instructions=MAX_INVOCATION_INSTRUCTIONS,
                 max_callbacks=MAX_CALLBACK_REQUESTS):
        if self._frames:
            raise RuntimeError("cannot register during a foreign operation")
        _exact(signature, ForeignSignatureV1)
        if type(script) is not tuple:
            raise TypeError("script must be an exact tuple")
        if not 1 <= len(script) <= 4096:
            raise ValueError("script must contain 1..4096 steps")
        if len(self._bindings) >= 64:
            raise ValueError("reference operation registry is full")
        operation = ForeignOperationV1(registration=object(), signature=signature,
            machine_grants=machine_grants, max_instructions=max_instructions,
            max_callbacks=max_callbacks)
        normalized, exports = [], []
        reply_arity = 0

        def expressions(values):
            copied = []
            for value in values:
                if type(value) is Input:
                    if value.index >= signature.input_cells:
                        raise ValueError("Input reference exceeds operation signature")
                    value = Input(value.index)
                elif type(value) is Reply:
                    if value.index >= reply_arity:
                        raise ValueError("Reply reference precedes or exceeds its callback reply")
                    value = Reply(value.index)
                copied.append(value)
            return tuple(copied)

        for step in script:
            if type(step) not in (Callback, Return, Failure, Store):
                raise TypeError("script contains an unrecognized step")
            type(step).__post_init__(step)
            if type(step) is Callback:
                for child in step.children:
                    self._binding(child)
                if len({id(child) for child in step.children}) != len(step.children):
                    raise ValueError("duplicate callback child authority")
                normalized.append(Callback(export=step.export, arguments=expressions(step.arguments),
                    site=step.site, instructions=step.instructions, cycles=step.cycles,
                    children=step.children))
                exports.append(_export_seal(step.export))
                reply_arity = step.export.signature.output_cells
            elif type(step) is Return:
                if len(step.outputs) != signature.output_cells:
                    raise ValueError("return arity differs from operation signature")
                normalized.append(Return(outputs=expressions(step.outputs),
                                         instructions=step.instructions, cycles=step.cycles))
            elif type(step) is Store:
                normalized.append(Store(address=step.address, data=step.data, cycles=step.cycles))
            else:
                normalized.append(Failure(kind=step.kind, detail=step.detail,
                    instructions=step.instructions, cycles=step.cycles, instruction_pc=step.instruction_pc))
        if type(normalized[-1]) not in (Return, Failure):
            raise ValueError("finite scripts end in Return or Failure")
        self._bindings[id(operation)] = _Binding(operation, operation.registration, signature,
            (signature.input_cells, signature.output_cells), machine_grants, _grant_values(machine_grants),
            (max_instructions, max_callbacks), tuple(normalized), tuple(exports))
        return operation

    def _binding(self, operation):
        binding = self._bindings.get(id(operation))
        if binding is None or binding.operation is not operation:
            raise ValueError("operation was not issued by this adapter")
        self._validate_binding(binding, set())
        return binding

    def _validate_binding(self, binding, seen):
        if id(binding.operation) in seen:
            return
        seen.add(id(binding.operation))
        operation = binding.operation
        _exact(operation, ForeignOperationV1)
        if (operation.registration is not binding.registration or operation.signature is not binding.signature
                or operation.machine_grants is not binding.grants
                or (operation.signature.input_cells, operation.signature.output_cells) != binding.arity
                or _grant_values(operation.machine_grants) != binding.grant_values
                or (operation.max_instructions, operation.max_callbacks) != binding.limits):
            raise ValueError("issued operation metadata changed")
        for export, identity, signature, arity, grants, grant_values, maximum in binding.exports:
            _exact(export, ForeignExportV1)
            if (export.export is not identity or export.signature is not signature
                    or (signature.input_cells, signature.output_cells) != arity
                    or export.task_grants is not grants or _grant_values(grants) != grant_values
                    or export.max_semantic_steps != maximum):
                raise ValueError("captured callback export changed")
        for step in binding.script:
            if type(step) is Callback:
                for child in step.children:
                    captured = self._bindings.get(id(child))
                    if captured is None or captured.operation is not child:
                        raise ValueError("captured child registration is stale")
                    self._validate_binding(captured, seen)

    def _geometry(self, binding):
        for grant in binding.grants:
            if grant.size and (self.memory is None or grant.base < self.memory_base
                    or grant.limit > self.memory_base + len(self.memory)):
                raise ValueError("machine grant escapes reference ordinary memory")

    @staticmethod
    def _narrow(child, parent):
        for grant in child.grants:
            if not grant.size:
                continue
            if not any(prior.base <= grant.base and grant.limit <= prior.limit
                       and (prior.access is ForeignAccessV1.READ_WRITE or prior.access is grant.access)
                       for prior in parent.grants):
                raise ValueError("child machine grants escalate parent authority")

    def _check_budget(self, budget, *, new_root=False):
        _exact(budget, ForeignBudgetV1)
        if not new_root and self._sequence == MASK64:
            raise ValueError("root receipt sequence exhausted")

    def _limits(self, frame, budget):
        return (min(frame.instruction_limit, frame.instructions + budget.invocation_instructions_remaining),
                min(frame.callback_limit, frame.callbacks + budget.invocation_callbacks_remaining),
                min(self._root_instruction_limit, self._root_instructions + budget.root_instructions_remaining),
                min(self._root_callback_limit, self._root_callbacks + budget.root_callbacks_remaining))

    def _apply_limits(self, frame, limits):
        (frame.instruction_limit, frame.callback_limit,
         self._root_instruction_limit, self._root_callback_limit) = limits

    def begin(self, operation, arguments, *, root_token, root_id, budget, parent=None):
        binding = self._binding(operation)
        _cells(arguments)
        if len(arguments) != binding.arity[0]:
            raise ValueError("operation input arity differs")
        new_root = root_token is not self._root_token
        self._check_budget(budget, new_root=new_root)
        _integer(root_id, "root ID", 1)
        if root_token is None:
            raise TypeError("root token must be non-None")
        self._geometry(binding)
        if new_root:
            if self._frames or parent is not None or root_id <= self._root_id:
                raise ValueError("foreign root identity is stale or another root is active")
            root_instructions, root_callbacks, entries = 0, 0, 0
            root_instruction_limit, root_callback_limit = (budget.root_instructions_remaining,
                                                           budget.root_callbacks_remaining)
        else:
            if root_id != self._root_id:
                raise ValueError("root token and ID disagree")
            root_instructions, root_callbacks, entries = (self._root_instructions,
                                                         self._root_callbacks, self._entries)
            root_instruction_limit = min(self._root_instruction_limit,
                                        root_instructions + budget.root_instructions_remaining)
            root_callback_limit = min(self._root_callback_limit,
                                     root_callbacks + budget.root_callbacks_remaining)
        parent_frame = None
        if parent is None:
            if self._frames:
                raise ValueError("an unrelated root cannot replace live foreign frames")
        else:
            _exact(parent, ForeignCallbackRequestV1)
            if not self._frames:
                raise ValueError("parent callback is no longer live")
            parent_frame = self._frames[-1]
            if parent_frame.state != "callback" or parent_frame.request is not parent:
                raise ValueError("parent request is not exact top pending authority")
            if not any(operation is child for child in parent_frame.callback.children):
                raise ValueError("child is outside the captured static callback edges")
            self._narrow(binding, parent_frame.binding)
        if len(self._frames) >= MAX_DEPTH or entries >= MAX_ROOT_ENTRIES:
            raise ValueError("foreign depth or root-entry limit exhausted")
        if any(frame.binding.registration is binding.registration for frame in self._frames):
            raise ValueError("same-registration recursion is not admitted")
        own_limit = min(operation.max_instructions, budget.invocation_instructions_remaining)
        if own_limit == 0 or root_instruction_limit <= root_instructions:
            raise ValueError("instruction allowance exhausted before admission")
        if self._next_invocation == MASK64:
            raise ValueError("invocation identity space exhausted")
        frame = _Frame(binding, arguments, object(), self._next_invocation + 1,
                       parent_frame.invocation_id if parent_frame is not None else None,
                       own_limit, min(operation.max_callbacks, budget.invocation_callbacks_remaining))
        if new_root:
            self._root_token, self._root_id = root_token, root_id
            self._sequence = self._entries = 0
            self._root_instructions = self._root_cycles = self._root_callbacks = 0
        self._root_instruction_limit, self._root_callback_limit = root_instruction_limit, root_callback_limit
        self._next_invocation += 1
        self._entries += 1
        self._frames.append(frame)
        self._admitted_inputs.append((frame.invocation_id, arguments))
        return self._drive(frame, budget.quantum_instructions, started=True)

    def _top(self, operation_token=None, request_token=None):
        if not self._frames:
            raise ValueError("no foreign invocation is active")
        frame = self._frames[-1]
        if operation_token is not None and frame.token is not operation_token:
            raise ValueError("operation token is foreign, stale or not topmost")
        if request_token is not None and (frame.state != "callback"
                or frame.request_token is not request_token or frame.request is None):
            raise ValueError("request token is foreign, consumed or not topmost")
        self._binding(frame.binding.operation)
        return frame

    def advance(self, operation_token, *, budget):
        if operation_token is None:
            raise TypeError("operation token must be non-None")
        frame = self._top(operation_token=operation_token)
        if frame.state != "runnable":
            raise ValueError("operation is not runnable")
        self._check_budget(budget)
        self._apply_limits(frame, self._limits(frame, budget))
        return self._drive(frame, budget.quantum_instructions, started=False)

    def reply(self, request_token, outputs, *, budget):
        if request_token is None:
            raise TypeError("request token must be non-None")
        frame = self._top(request_token=request_token)
        _cells(outputs)
        if len(outputs) != frame.callback.export.signature.output_cells:
            raise ValueError("callback reply arity differs")
        self._check_budget(budget)
        limits = self._limits(frame, budget)
        self._replies.append((frame.invocation_id, frame.callbacks, outputs))
        frame.reply_values = outputs
        frame.request_token = frame.request = frame.callback = None
        frame.state = "runnable"
        self._apply_limits(frame, limits)
        return self._drive(frame, budget.quantum_instructions, started=False)

    @staticmethod
    def _resolve(values, frame):
        return tuple(frame.arguments[value.index] if type(value) is Input else
                     frame.reply_values[value.index] if type(value) is Reply else value for value in values)

    def _emit(self, frame, state, before, started, *, outputs=(), failure=None):
        self._sequence += 1
        receipt = ForeignReceiptV1(root_id=self._root_id, invocation_id=frame.invocation_id,
            parent_invocation_id=frame.parent_id, depth=len(self._frames), sequence=self._sequence,
            invocation_started=started, root_entries=self._entries, state=state,
            instructions=frame.instructions - before[0], cycles=frame.cycles - before[1],
            callback_requests=frame.callbacks - before[2], invocation_instructions=frame.instructions,
            invocation_cycles=frame.cycles, invocation_callbacks=frame.callbacks,
            root_instructions=self._root_instructions, root_cycles=self._root_cycles,
            root_callbacks=self._root_callbacks)
        self._last = receipt
        # Every returned segment consumes its prior runnable authority. A
        # caller cannot replay an older yield token to run the next segment.
        # Delivery failure is recovered through last_receipt()/cancel_all().
        frame.token = object()
        if state is ForeignStateV1.RETURNED:
            self._frames.pop()
        if self._delivery_failure is not None:
            count, error = self._delivery_failure
            self._delivery_failure = (count - 1, error) if count > 1 else None
            if count == 1:
                raise error
        if state is ForeignStateV1.CALLBACK:
            step = frame.callback
            frame.request_token = object()
            event = ForeignCallbackRequestV1(operation_token=frame.token, request_token=frame.request_token,
                receipt=receipt, request_sequence=frame.callbacks, site=step.site, export=step.export,
                arguments=self._resolve(step.arguments, frame))
            frame.request = event
            return event
        if state is ForeignStateV1.RETURNED:
            return ForeignCompletedV1(operation_token=frame.token, receipt=receipt, outputs=outputs)
        if state is ForeignStateV1.YIELDED:
            return ForeignRunnableYieldV1(operation_token=frame.token, receipt=receipt)
        return ForeignFailedV1(operation_token=frame.token, receipt=receipt, kind=failure.kind,
                               detail=failure.detail, instruction_pc=failure.instruction_pc)

    def _fail(self, frame, before, started, kind, detail):
        frame.state = "failed"
        return self._emit(frame, ForeignStateV1.FAILED, before, started,
                          failure=Failure(kind=kind, detail=detail))

    def _drive(self, frame, quantum, *, started):
        before = frame.instructions, frame.cycles, frame.callbacks
        while True:
            remaining = min(frame.instruction_limit - frame.instructions,
                            self._root_instruction_limit - self._root_instructions)
            if remaining == 0:
                return self._fail(frame, before, started, "instruction_limit", "original instruction allowance exhausted")
            if quantum == 0:
                return self._emit(frame, ForeignStateV1.YIELDED, before, started)
            step = frame.binding.script[frame.index]
            if type(step) is Store:
                end = step.address + len(step.data)
                allowed = any(grant.base <= step.address and end <= grant.limit
                              and grant.access in (ForeignAccessV1.WRITE, ForeignAccessV1.READ_WRITE)
                              for grant in frame.binding.grants)
                if (not allowed or self.memory is None or step.address < self.memory_base
                        or end > self.memory_base + len(self.memory)):
                    return self._fail(frame, before, started, "rejected_access", "store escapes an ordinary write grant")
            count = 1 if type(step) is Store else step.instructions
            take = min(count - frame.offset, remaining, quantum)
            cycles = take
            if frame.offset + take == count:
                cycles += step.cycles - count
            if cycles > MASK64 - self._root_cycles:
                return self._fail(frame, before, started, "profile_rejected", "cycle counter would overflow")
            frame.offset += take
            frame.instructions += take
            frame.cycles += cycles
            self._root_instructions += take
            self._root_cycles += cycles
            quantum -= take
            if frame.offset < count:
                continue
            frame.offset = 0
            frame.index += 1
            if type(step) is Store:
                offset = step.address - self.memory_base
                self.memory[offset:offset + len(step.data)] = step.data
            elif type(step) is Callback:
                if frame.callbacks == frame.callback_limit or self._root_callbacks == self._root_callback_limit:
                    return self._fail(frame, before, started, "callback_limit", "original callback allowance exhausted")
                frame.callbacks += 1
                self._root_callbacks += 1
                frame.state, frame.callback = "callback", step
                return self._emit(frame, ForeignStateV1.CALLBACK, before, started)
            elif type(step) is Return:
                return self._emit(frame, ForeignStateV1.RETURNED, before, started,
                                  outputs=self._resolve(step.outputs, frame))
            else:
                frame.state = "failed"
                return self._emit(frame, ForeignStateV1.FAILED, before, started, failure=step)

    def validate_parked(self, root_token, operation_token, request_token=None):
        if root_token is not self._root_token or operation_token is None:
            raise ValueError("parked validation requires the exact current root and operation")
        frame = self._top(operation_token=operation_token)
        if frame.state == "callback":
            if request_token is None or request_token is not frame.request_token:
                raise ValueError("parked validation requires the exact pending request")
            self._top(request_token=request_token)
        elif frame.state != "runnable" or request_token is not None:
            raise ValueError("parked foreign state is not resumable")
        parent = None
        for current in self._frames:
            self._binding(current.binding.operation)
            if current.parent_id != (parent.invocation_id if parent is not None else None):
                raise ValueError("parked foreign ancestry changed")
            if parent is not None and (parent.state != "callback" or parent.request is None):
                raise ValueError("parked ancestor no longer owns its request")
            parent = current
        return True

    def last_receipt(self):
        return self._last

    def cancel_suffix(self, operation_token):
        index = next((index for index, frame in enumerate(self._frames)
                      if frame.token is operation_token), None)
        if index is None:
            raise ValueError("cancellation token is not a live invocation")
        return self._cancel(index)

    def cancel_all(self):
        return self._cancel(0)

    def _cancel(self, index):
        failure, self._cancel_failure = self._cancel_failure, None
        if failure is not None and not failure[1]:
            raise failure[0]
        retired = tuple(frame.invocation_id for frame in reversed(self._frames[index:]))
        parent = self._frames[index - 1] if index else None
        result = ForeignCancellationV1(retired_invocation_ids=retired,
            surviving_parent_id=parent.invocation_id if parent is not None else None,
            surviving_parent_token=parent.request_token if parent is not None else None,
            receipt=self._last)
        del self._frames[index:]
        if failure is not None:
            raise failure[0]
        return result

    def fail_delivery_after(self, count, error):
        if self._frames:
            raise RuntimeError("delivery failure must be armed while idle")
        _integer(count, "delivery failure count", 1, 64)
        if not isinstance(error, BaseException):
            raise TypeError("delivery failure must be an exception object")
        self._delivery_failure = count, error

    def fail_next_cancel(self, error, *, after_retirement=False):
        if self._frames:
            raise RuntimeError("cancellation failure must be armed while idle")
        if not isinstance(error, BaseException) or type(after_retirement) is not bool:
            raise TypeError("cancellation failure needs an exception and an exact boolean")
        self._cancel_failure = error, after_retirement


__all__ = ["ScriptedForeignAdapter", "Input", "Reply", "Callback", "Return", "Failure", "Store"]
