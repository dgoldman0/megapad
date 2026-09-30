"""Captured semantic dependencies and accounting for task foreign dispatch.

This is the host-owned foundation of the reference task profile. It does not
execute a foreign definition, install a native adapter, or advertise a task
capability. The ordinary dispatcher must explicitly own every transition.
"""

from __future__ import annotations

from dataclasses import dataclass, field, replace
from types import FunctionType, GetSetDescriptorType, MemberDescriptorType

from shared.cells import CELL_BYTES, MASK64
from shared.foreign_abi import (
    ForeignAccessV1, ForeignExportV1, ForeignOperationV1, ForeignReceiptV1,
    ForeignSignatureV1, ForeignSpanV1, ForeignStateV1, MAX_CALLBACK_REQUESTS, MAX_DEPTH,
    MAX_ROOT_CALLBACK_SEMANTIC_STEPS, MAX_ROOT_ENTRIES, MAX_ROOT_INSTRUCTIONS,
)
from simulator import core_words
from simulator.dictionary import Dictionary, Word
from simulator.errors import ExecutionError
from simulator.ir import (
    Branch, BranchZero, Call, CallSelf, Do, Literal, Loop, PlusLoop, QuestionDo,
    RestoreDataStackPointer, RestoreReturnStackPointer, Return, RPeek, RPeekPair,
    RPop, RPopPair, RPush, RPushPair, StoreValue, Unloop,
)
from simulator.memory import SparseAddressSpace
from simulator.runtime import (
    ColonDefinition, ConstantDefinition, CreatedDefinition, DoesBodyRef,
    ExecutionContext, MegaForthRuntime, PrimitiveDefinition, ValueDefinition,
    _StepMeter,
)
from simulator.stacks import DataStack, ReturnStack


_MAX_WORDS = 64
_MAX_OPERATIONS = 4096
_MAX_REGISTRATIONS = 64
_MAX_EXPORTS = 64
_CORE_NAMES = (
    "DUP DROP SWAP OVER NIP TUCK ROT -ROT 2DUP 2DROP 2OVER 2SWAP ?DUP PICK "
    "+ - * / MOD NEGATE ABS MIN MAX 1+ 1- 2* 2/ AND OR XOR LSHIFT RSHIFT INVERT "
    "= <> 0= 0<> 0< 0> U< U> < <= >= > @ C@ W@ L@ ! C! W! L! +! OFF "
    "CELLS CELL+ DEPTH SP@ RP@ COREID TASK-ID I J EXECUTE ABORT"
).split()
_OPERATION_FIELDS = {
    Literal: "value", Call: "xt", StoreValue: "address", Branch: "target",
    BranchZero: "target", QuestionDo: "target", Loop: "target", PlusLoop: "target",
    CallSelf: None, Return: None, Do: None, Unloop: None, RPush: None, RPop: None,
    RPeek: None, RPushPair: None, RPopPair: None, RPeekPair: None,
    RestoreDataStackPointer: None, RestoreReturnStackPointer: None,
}
_BRANCH_TYPES = (Branch, BranchZero, QuestionDo, Loop, PlusLoop)
_MEMORY_ROUTES = tuple((name, value) for name, value in vars(SparseAddressSpace).items()
                       if type(value) is FunctionType or type(value) is property)
_DICTIONARY_ROUTES = tuple((name, vars(Dictionary)[name]) for name in ("find", "resolve", "words"))
_CORE_HELPERS = tuple((name, value) for name, value in vars(core_words).items()
                     if type(value) is FunctionType)
_METADATA_ROUTES = tuple((kind, tuple(vars(kind).items())) for kind in (
    Word, PrimitiveDefinition, ColonDefinition, ConstantDefinition, ValueDefinition,
    CreatedDefinition, DoesBodyRef, *_OPERATION_FIELDS,
    ForeignSignatureV1, ForeignSpanV1, ForeignExportV1, ForeignOperationV1, ForeignReceiptV1,
))
_NAMESPACE_DESCRIPTORS = (
    (SparseAddressSpace, vars(SparseAddressSpace)["__dict__"]),
    (Dictionary, vars(Dictionary)["__dict__"]),
)


class ForeignTaskError(ExecutionError):
    """A task transition lacks its captured authority or violates its profile."""


class ForeignTaskBudgetExceeded(ForeignTaskError):
    """The original task-root or inclusive callback allowance is exhausted."""


def _uint(value, label, *, minimum=0, maximum=MASK64):
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} must be in {minimum}..{maximum}")


def _value(value, kind):
    _require_metadata()
    if type(value) is not kind:
        raise TypeError(f"expected an exact {kind.__name__}")
    kind.__post_init__(value)
    return value


def _namespace(instance, kind, routes, *, descriptor=None):
    if type(instance) is not kind or type(kind) is not type:
        raise ForeignTaskError("task dependency requires its canonical owner type")
    fields = vars(kind)
    if any(type(key) is not str for key in fields):
        raise ForeignTaskError("task dependency class namespace is not canonical")
    if kind.__getattribute__ is not object.__getattribute__ or "__getattr__" in fields:
        raise ForeignTaskError("task dependency attribute routing changed")
    if descriptor is None:
        descriptor = next((value for owner, value in _NAMESPACE_DESCRIPTORS if owner is kind), None)
    if type(descriptor) is not GetSetDescriptorType or fields.get("__dict__") is not descriptor:
        raise ForeignTaskError("task dependency namespace routing changed")
    namespace = descriptor.__get__(instance, kind)
    if type(namespace) is not dict or any(type(key) is not str for key in namespace):
        raise ForeignTaskError("task dependency namespace is not canonical")
    for name, original in routes:
        if name in namespace or vars(kind).get(name) is not original:
            raise ForeignTaskError(f"task dependency route changed: {name}")


def _require_metadata():
    # Prove the original descriptors before reading any consumed Word, IR or
    # implementation field. Exact instance type alone does not prove getters.
    for kind, original in _METADATA_ROUTES:
        fields = vars(kind)
        if any(type(key) is not str for key in fields):
            raise ForeignTaskError("task metadata class namespace changed")
        if kind.__getattribute__ is not object.__getattribute__ or "__getattr__" in fields:
            raise ForeignTaskError("task metadata attribute routing changed")
        for name, value in original:
            if (type(value) in (FunctionType, MemberDescriptorType, property)
                    or name in ("__setattr__", "__delattr__", "__dict__")):
                if fields.get(name) is not value:
                    raise ForeignTaskError("task metadata descriptor routing changed")


def _overlaps(base, size, other_base, other_limit):
    return bool(size and base < other_limit and other_base < base + size)


@dataclass(frozen=True, slots=True, eq=False)
class ForeignDefinition:
    """An implementation marker; only the issuing engine binds it to a Word."""

    _registration: object = field(repr=False)


@dataclass(frozen=True, slots=True, eq=False)
class ForeignResumeTarget:
    """An owned semantic location, or completion of the original public root.

    This value is not a machine PC and grants no return authority by itself.
    """

    word: Word | None = field(repr=False)
    ip: int

    def __post_init__(self):
        _uint(self.ip, "semantic resume IP")
        if self.word is None:
            if self.ip:
                raise ValueError("root completion must have zero semantic IP")
        elif type(self.word) is not Word:
            raise TypeError("semantic resume target must retain an exact Word")


@dataclass(frozen=True, slots=True)
class _FunctionSeal:
    callback: object = field(repr=False)
    code: object = field(repr=False)
    globals: object = field(repr=False)
    closure: tuple = field(repr=False)
    defaults: object = field(repr=False)
    kwdefaults: object = field(repr=False)

    @classmethod
    def capture(cls, callback):
        if type(callback) is not FunctionType:
            raise ForeignTaskError("task primitive must be an original Python function")
        return cls(callback, callback.__code__, callback.__globals__,
                   tuple(cell.cell_contents for cell in (callback.__closure__ or ())),
                   callback.__defaults__, callback.__kwdefaults__)

    def verify(self):
        callback = self.callback
        if (type(callback) is not FunctionType or callback.__code__ is not self.code
                or callback.__globals__ is not self.globals
                or callback.__defaults__ is not self.defaults
                or callback.__kwdefaults__ is not self.kwdefaults):
            raise ForeignTaskError("captured task primitive implementation changed")
        live = callback.__closure__ or ()
        if len(live) != len(self.closure) or any(
                cell.cell_contents is not expected for cell, expected in zip(live, self.closure)):
            raise ForeignTaskError("captured task primitive closure changed")


@dataclass(frozen=True, slots=True)
class _OperationSeal:
    operation: object = field(repr=False)
    kind: type
    field_name: str | None
    value: int | None

    def verify(self):
        if type(self.operation) is not self.kind:
            raise ForeignTaskError("captured task operation type changed")
        if self.field_name is not None:
            current = getattr(self.operation, self.field_name)
            if type(current) is not int or current != self.value:
                raise ForeignTaskError("captured task operation field changed")


@dataclass(frozen=True, slots=True)
class _CapturedWord:
    word: Word = field(repr=False)
    xt: int
    header: int
    implementation: object = field(repr=False)
    start_ip: int = 0
    operations: tuple | None = field(default=None, repr=False)
    evidence: tuple[_OperationSeal, ...] = ()
    function: _FunctionSeal | None = None
    constant: int | None = None
    action: DoesBodyRef | None = None
    action_fields: tuple[int, int] | None = None

    def verify(self, engine):
        _require_metadata()
        word = self.word
        if (type(word) is not Word or type(word.xt) is not int or word.xt != self.xt
                or type(word.header_address) is not int or word.header_address != self.header
                or engine._dictionary.resolve(self.xt) is not word
                or word.implementation is not self.implementation):
            raise ForeignTaskError("captured task Word was changed or removed")
        implementation = self.implementation
        if self.function is not None:
            if type(implementation) is not PrimitiveDefinition or implementation.callback is not self.function.callback:
                raise ForeignTaskError("captured task primitive changed")
            self.function.verify()
        elif type(implementation) is ColonDefinition:
            if implementation.operations is not self.operations:
                raise ForeignTaskError("captured task operation tuple changed")
            for operation in self.evidence:
                operation.verify()
        elif type(implementation) is ConstantDefinition:
            if type(implementation.value) is not int or implementation.value != self.constant:
                raise ForeignTaskError("captured task constant changed")
        elif type(implementation) is CreatedDefinition:
            if implementation.action is not self.action:
                raise ForeignTaskError("captured CREATE action changed")
            if self.action is not None:
                if (type(self.action) is not DoesBodyRef
                        or type(self.action.source_xt) is not int
                        or type(self.action.entry_ip) is not int
                        or (self.action.source_xt, self.action.entry_ip) != self.action_fields):
                    raise ForeignTaskError("captured DOES> entry changed")
        elif type(implementation) is ForeignDefinition:
            engine.require_definition(word)
        elif type(implementation) is not ValueDefinition:
            raise ForeignTaskError("captured task implementation is unsupported")


@dataclass(frozen=True, slots=True, eq=False)
class CapturedTaskExport:
    """Engine-owned capture; descriptor copies cannot select this evidence."""

    descriptor: ForeignExportV1
    metadata: ForeignExportV1 = field(repr=False)
    entry: Word = field(repr=False)
    words: tuple[_CapturedWord, ...] = field(repr=False)
    fault_target: Word | None = field(default=None, repr=False)

    def require_target(self, engine, word, *, entry_ip=0, permit_machine=True):
        engine._verify_export(self)
        if type(word) is not Word or type(entry_ip) is not int:
            raise ForeignTaskError("task target is not an exact captured semantic entry")
        evidence = next((item for item in self.words if item.word is word), None)
        if evidence is None:
            raise ForeignTaskError("task dynamic target was not captured")
        if type(word.implementation) is ForeignDefinition and not permit_machine:
            raise ForeignTaskError("retired semantic tail has no machine-entry authority")
        if evidence.operations is not None:
            if not evidence.start_ip <= entry_ip < len(evidence.operations):
                raise ForeignTaskError("task entry IP escapes the captured suffix")
        elif entry_ip:
            raise ForeignTaskError("non-colon task target has a nonzero entry IP")
        return evidence

    def require_access(self, engine, address, width, access):
        """Check at the actual canonical memory access, never before stack pops."""
        engine._verify_export(self)
        _uint(address, "task access address")
        _uint(width, "task access width", minimum=1)
        if type(access) is not str or access not in ("read", "write"):
            raise TypeError("task access must be read or write")
        if width - 1 > MASK64 - address:
            raise ForeignTaskError("task access wraps the address space")
        for grant in self.descriptor.task_grants:
            if (grant.base <= address and address + width <= grant.base + grant.size
                    and (grant.access is ForeignAccessV1.READ_WRITE or grant.access.value == access)):
                return
        raise ForeignTaskError("task memory access is outside its captured grants")


@dataclass(frozen=True, slots=True)
class _AdapterSeal:
    adapter: object = field(repr=False)
    kind: type
    methods: tuple = field(repr=False)
    namespace_descriptor: object = field(repr=False)

    @classmethod
    def capture(cls, adapter):
        kind = type(adapter)
        if type(kind) is not type:
            raise TypeError("task adapter must have ordinary Python class ownership")
        names = ("begin", "advance", "reply", "cancel_suffix", "cancel_all", "last_receipt")
        methods = tuple((name, vars(kind).get(name)) for name in names)
        if any(type(method) is not FunctionType for _, method in methods):
            raise TypeError("task adapter must implement exact Python transition methods")
        result = cls(adapter, kind, methods, vars(kind).get("__dict__"))
        result.verify()
        return result

    def verify(self):
        _namespace(self.adapter, self.kind, self.methods, descriptor=self.namespace_descriptor)

    def call(self, name, *args, **kwargs):
        self.verify()
        method = next((method for method_name, method in self.methods if method_name == name), None)
        if method is None:
            raise ForeignTaskError("unknown owned adapter transition")
        return method(self.adapter, *args, **kwargs)


@dataclass(frozen=True, slots=True)
class _ForeignBinding:
    word: Word
    definition: ForeignDefinition
    operation: ForeignOperationV1
    metadata: ForeignOperationV1
    adapter: _AdapterSeal
    protected_spans: tuple[ForeignSpanV1, ...]


class ForeignTaskEngine:
    """One runtime's opt-in task registrations and exact captured dependencies."""

    def __init__(self, runtime, *, core_installed):
        self._runtime = runtime
        self._dictionary = runtime.dictionary
        self._memory = runtime.memory
        self._context = runtime.main_context
        self._owner = object()
        self._bindings: dict[int, _ForeignBinding] = {}
        self._capture_state: tuple[dict[int, CapturedTaskExport], int] = ({}, 0)
        self._canonical: dict[int, _CapturedWord] = {}
        self._task_root = None
        self._publication_failure = None
        self._registration_active = False
        if core_installed:
            for name in _CORE_NAMES:
                word = self._dictionary.find(name)
                if word is not None and type(word.implementation) is PrimitiveDefinition:
                    self._canonical[id(word)] = _CapturedWord(
                        word, word.xt, word.header_address, word.implementation,
                        function=_FunctionSeal.capture(word.implementation.callback),
                    )

    @property
    def _exports(self):
        return self._capture_state[0]

    def _require_idle(self, operation):
        if self._registration_active:
            raise ForeignTaskError("task registration is already in progress")
        runtime = self._runtime
        runtime._require_session_owner_access(operation)
        runtime._require_no_suspension(operation)
        if (runtime._active_dispatches or runtime._active_input_states
                or self._task_root is not None):
            raise ForeignTaskError(f"cannot {operation} during task execution")
        self._require_task(self._context)

    def _require_task(self, context):
        if self._registration_active:
            raise ForeignTaskError("task registration is already in progress")
        if self._publication_failure is not None:
            raise ForeignTaskError("task registration rollback failed; further admission is disabled")
        _require_metadata()
        runtime = self._runtime
        if (type(runtime) is not MegaForthRuntime or type(context) is not ExecutionContext
                or runtime._foreign_tasks is not self
                or context is not self._context or runtime.main_context is not context
                or runtime.memory is not self._memory or runtime.dictionary is not self._dictionary
                or type(context.data) is not DataStack or type(context.returns) is not ReturnStack
                or context.data._memory is not self._memory or context.returns._memory is not self._memory):
            raise ForeignTaskError("task profile requires the original canonical main context")
        _namespace(self._memory, SparseAddressSpace, _MEMORY_ROUTES)
        _namespace(self._dictionary, Dictionary, _DICTIONARY_ROUTES)
        for name, seal in _CORE_FUNCTION_SEALS:
            if vars(core_words).get(name) is not seal.callback:
                raise ForeignTaskError("canonical task core helper changed")
            seal.verify()

    def _word(self, target):
        if type(target) is Word:
            word = target
        elif type(target) in (str, bytes):
            word = self._dictionary.find(target)
        elif type(target) is int:
            try:
                word = self._dictionary.resolve(target)
            except KeyError:
                word = None
        else:
            raise TypeError("task target must be an exact Word, name or XT")
        if word is None or type(word) is not Word:
            raise ForeignTaskError("task target is not a live Word")
        _uint(word.xt, "task XT", minimum=1)
        _uint(word.header_address, "task header")
        if self._dictionary.resolve(word.xt) is not word:
            raise ForeignTaskError("task target no longer belongs to this dictionary")
        return word

    def _check_grants(self, grants, *, machine=False):
        if type(grants) is not tuple or len(grants) > 16:
            raise TypeError("task grants must be an exact tuple of at most 16 spans")
        headers = []
        for word in self._dictionary.words:
            if type(word) is not Word:
                raise ForeignTaskError("dictionary contains noncanonical task metadata")
            _uint(word.header_address, "dictionary header address")
            _uint(word.xt, "dictionary XT", maximum=MASK64 - CELL_BYTES)
            headers.append((word.header_address, word.xt + CELL_BYTES))
        protected = tuple(span for binding in self._bindings.values() for span in binding.protected_spans)
        for span in protected:
            _value(span, ForeignSpanV1)
        for grant in grants:
            _value(grant, ForeignSpanV1)
            if not grant.size:
                continue
            self._memory._qualify_ordinary_span(grant.base, grant.size)
            if any(_overlaps(grant.base, grant.size, base, limit) for base, limit in headers):
                raise ForeignTaskError("task grant overlaps a dictionary header or semantic code slot")
            if any(_overlaps(grant.base, grant.size, span.base, span.base + span.size) for span in protected):
                raise ForeignTaskError("task grant overlaps protected machine storage")
            if machine and any(_overlaps(grant.base, grant.size, stack.floor, stack.empty_pointer)
                               for stack in (self._context.data, self._context.returns)):
                raise ForeignTaskError("machine grant overlaps a semantic stack allocation")

    def define_operation(self, name, adapter, operation, *, protected_spans=()):
        """Publish a host-selected implementation marker, without executing it."""
        with self._runtime._session_owner_lock:
            return self._define_operation(name, adapter, operation, protected_spans=protected_spans)

    def _define_operation(self, name, adapter, operation, *, protected_spans):
        self._require_idle("define a task foreign operation")
        _value(operation, ForeignOperationV1)
        if len(self._bindings) >= _MAX_REGISTRATIONS:
            raise ForeignTaskError("task machine registration table is full")
        seal = _AdapterSeal.capture(adapter)
        self._check_grants(operation.machine_grants, machine=True)
        if type(protected_spans) is not tuple or len(protected_spans) > 16:
            raise TypeError("protected spans must be a bounded exact tuple")
        for span in protected_spans:
            _value(span, ForeignSpanV1)
            if span.size:
                self._memory._qualify_ordinary_span(span.base, span.size)
                if any(_overlaps(span.base, span.size, stack.floor, stack.empty_pointer)
                       for stack in (self._context.data, self._context.returns)):
                    raise ForeignTaskError("protected machine storage overlaps a semantic stack")
                if any(_overlaps(span.base, span.size, grant.base, grant.base + grant.size)
                       for grant in operation.machine_grants):
                    raise ForeignTaskError("machine grants overlap its protected storage")
        definition = ForeignDefinition(object())
        metadata = replace(operation, signature=replace(operation.signature),
                           machine_grants=tuple(replace(span) for span in operation.machine_grants))
        protected = tuple(replace(span) for span in protected_spans)
        width = self._dictionary.definition_size(name)
        rejection = self._runtime._dictionary_growth_rejection(width, self._context)
        if rejection is not None:
            raise ForeignTaskError(f"task registration dictionary growth rejected: {rejection}")
        checkpoint = self._dictionary.checkpoint()
        previous = self._bindings
        replacement = dict(previous)
        self._registration_active = True
        try:
            word = self._runtime._define_public_dictionary_word(name, definition)
            replacement[id(definition)] = _ForeignBinding(
                word, definition, operation, metadata, seal, protected,
            )
            self._bindings = replacement
            return word
        except BaseException as failure:
            self._bindings = previous
            self._publication_failure = failure
            try:
                self._dictionary.rollback(checkpoint)
                self._runtime.dictionary_index.rebuild()
            except BaseException:
                try:
                    BaseException.add_note(failure, "task registration rollback failed; further admission is disabled")
                except BaseException:
                    pass
            else:
                self._publication_failure = None
            raise
        finally:
            self._registration_active = False

    def require_definition(self, word):
        self._require_task(self._context)
        word = self._word(word)
        definition = word.implementation
        binding = self._bindings.get(id(definition))
        if (type(definition) is not ForeignDefinition or binding is None
                or binding.word is not word or binding.definition is not definition):
            raise ForeignTaskError("foreign definition is not the engine's issued registration")
        _value(binding.operation, ForeignOperationV1)
        metadata = binding.metadata
        if (binding.operation.registration is not metadata.registration
                or binding.operation.signature != metadata.signature
                or binding.operation.machine_grants != metadata.machine_grants
                or binding.operation.max_instructions != metadata.max_instructions
                or binding.operation.max_callbacks != metadata.max_callbacks):
            raise ForeignTaskError("task operation descriptor changed after registration")
        binding.adapter.verify()
        self._check_grants(binding.operation.machine_grants, machine=True)
        return binding

    def capture_export(self, target, signature, *, task_grants=(), dynamic_targets=(),
                       fault_target=None, max_semantic_steps=4096):
        with self._runtime._session_owner_lock:
            return self._capture_export(
                target, signature, task_grants=task_grants, dynamic_targets=dynamic_targets,
                fault_target=fault_target, max_semantic_steps=max_semantic_steps,
            )

    def _capture_export(self, target, signature, *, task_grants, dynamic_targets,
                        fault_target, max_semantic_steps):
        self._require_idle("capture a task callback")
        _value(signature, ForeignSignatureV1)
        if type(dynamic_targets) is not tuple or len(dynamic_targets) > _MAX_WORDS:
            raise TypeError("dynamic targets must be a bounded exact tuple")
        if len(self._exports) >= _MAX_EXPORTS:
            raise ForeignTaskError("task export table is full")
        self._check_grants(task_grants)
        entry = self._word(target)
        fault = None if fault_target is None else self._word(fault_target)
        if fault is not None and self._runtime._fault_xt != fault.xt:
            raise ForeignTaskError("task fault target is not the selected guest fault hook")
        descriptor = ForeignExportV1(export=object(), signature=replace(signature),
                                     task_grants=tuple(replace(span) for span in task_grants),
                                     max_semantic_steps=max_semantic_steps)
        pending = [(entry, 0), *((self._word(target), 0) for target in dynamic_targets)]
        if fault is not None:
            pending.append((fault, 0))
        captured = {}
        total_operations = 0
        while pending:
            word, start_ip = pending.pop()
            word = self._word(word)
            previous = captured.get(id(word))
            if previous is not None and previous.start_ip <= start_ip:
                continue
            if previous is None and len(captured) >= _MAX_WORDS:
                raise ForeignTaskError("task closure captures more than 64 Words")
            implementation = word.implementation
            kind = type(implementation)
            evidence = _CapturedWord(word, word.xt, word.header_address, implementation, start_ip)
            if kind is PrimitiveDefinition:
                evidence = self._canonical.get(id(word))
                if evidence is None:
                    raise ForeignTaskError("task primitive is not an original admitted core Word")
                evidence.verify(self)
            elif kind is ColonDefinition:
                operations = implementation.operations
                if type(operations) is not tuple or not 0 <= start_ip < len(operations):
                    raise ForeignTaskError("task colon has invalid immutable operations or entry")
                total_operations += len(operations) - start_ip
                if previous is not None:
                    total_operations -= len(previous.evidence)
                if self._capture_state[1] + total_operations > _MAX_OPERATIONS:
                    raise ForeignTaskError("task closure exceeds 4096 operations")
                items = []
                for operation in operations[start_ip:]:
                    operation_kind = type(operation)
                    if operation_kind not in _OPERATION_FIELDS:
                        raise ForeignTaskError("task colon contains an unsupported operation")
                    field_name = _OPERATION_FIELDS[operation_kind]
                    value = None if field_name is None else getattr(operation, field_name)
                    if field_name is not None:
                        _uint(value, "task operation field")
                    if operation_kind in _BRANCH_TYPES and not start_ip <= value < len(operations):
                        raise ForeignTaskError("task branch escapes its captured suffix")
                    if operation_kind is Call:
                        pending.append((self._word(value), 0))
                    elif operation_kind is CallSelf:
                        pending.append((word, 0))
                    items.append(_OperationSeal(operation, operation_kind, field_name, value))
                evidence = replace(evidence, operations=operations, evidence=tuple(items))
            elif kind is ConstantDefinition:
                _uint(implementation.value, "task constant")
                evidence = replace(evidence, constant=implementation.value)
            elif kind is CreatedDefinition:
                action = implementation.action
                if action is not None:
                    if type(action) is not DoesBodyRef:
                        raise ForeignTaskError("task CREATE action is not an exact DOES> entry")
                    _uint(action.source_xt, "task DOES> XT", minimum=1)
                    _uint(action.entry_ip, "task DOES> IP")
                    pending.append((self._word(action.source_xt), action.entry_ip))
                    evidence = replace(evidence, action=action,
                                       action_fields=(action.source_xt, action.entry_ip))
            elif kind is ForeignDefinition:
                self.require_definition(word)
            elif kind is not ValueDefinition:
                raise ForeignTaskError("task target implementation is outside the profile")
            captured[id(word)] = evidence
        metadata = replace(descriptor, signature=replace(descriptor.signature),
                           task_grants=tuple(replace(span) for span in descriptor.task_grants))
        capture = CapturedTaskExport(descriptor, metadata, entry, tuple(captured.values()), fault)
        # Count each declared capture's IR, including repeated dependencies;
        # rejected captures spend neither registration nor operation capacity.
        replacement = dict(self._exports)
        replacement[id(descriptor)] = capture
        self._capture_state = replacement, self._capture_state[1] + total_operations
        return descriptor

    def require_export(self, descriptor):
        capture = self._exports.get(id(descriptor))
        if capture is None or capture.descriptor is not descriptor:
            raise ForeignTaskError("task export is not the engine's issued identity")
        self._verify_export(capture)
        return capture

    def _verify_export(self, capture):
        self._require_task(self._context)
        if self._exports.get(id(capture.descriptor)) is not capture:
            raise ForeignTaskError("task capture is not registered")
        _value(capture.descriptor, ForeignExportV1)
        descriptor, metadata = capture.descriptor, capture.metadata
        if (descriptor.export is not metadata.export or descriptor.signature != metadata.signature
                or descriptor.task_grants != metadata.task_grants
                or descriptor.max_semantic_steps != metadata.max_semantic_steps):
            raise ForeignTaskError("task export descriptor changed after capture")
        self._check_grants(capture.descriptor.task_grants)
        for word in capture.words:
            try:
                word.verify(self)
            except KeyError:
                raise ForeignTaskError("captured task Word was removed") from None
        if capture.fault_target is not None and self._runtime._fault_xt != capture.fault_target.xt:
            raise ForeignTaskError("captured task fault hook changed")


@dataclass(frozen=True, slots=True)
class _InvocationAccount:
    invocation_id: int
    parent_id: int | None
    depth: int
    registration: object = field(repr=False)
    instruction_limit: int
    callback_limit: int
    instructions: int = 0
    cycles: int = 0
    callbacks: int = 0
    terminal: bool = False
    state: ForeignStateV1 = ForeignStateV1.YIELDED


@dataclass(frozen=True, slots=True)
class _LedgerState:
    instructions: int = 0
    cycles: int = 0
    callbacks: int = 0
    entries: int = 0
    sequence: int = 0
    last_invocation_id: int = 0
    receipt: ForeignReceiptV1 | None = field(default=None, repr=False)
    receipt_values: ForeignReceiptV1 | None = field(default=None, repr=False)
    active: tuple[_InvocationAccount, ...] = ()
    semantic_steps: int = 0


@dataclass(frozen=True, slots=True)
class _RootPolicy:
    meter: _StepMeter = field(repr=False)
    root_id: int
    root_token: object = field(repr=False)
    instruction_limit: int
    callback_limit: int
    entry_limit: int
    semantic_limit: int


class ForeignRootLedger:
    """Bounded accounting retained by the original dispatcher root and meter.

    Values are checked for continuity only after an owned adapter identifies
    its exact latest receipt. This ledger itself cannot issue adapter tokens.
    An empty active chain deliberately preserves every spent root counter.
    """

    def __init__(self, meter, root_id, *, instruction_limit=MAX_ROOT_INSTRUCTIONS,
                 callback_limit=MAX_CALLBACK_REQUESTS, entry_limit=MAX_ROOT_ENTRIES,
                 semantic_limit=MAX_ROOT_CALLBACK_SEMANTIC_STEPS):
        if type(meter) is not _StepMeter:
            raise TypeError("task root requires the original exact semantic meter")
        _uint(root_id, "task root ID", minimum=1)
        for value, name, maximum in (
                (instruction_limit, "root instruction limit", MAX_ROOT_INSTRUCTIONS),
                (callback_limit, "root callback limit", MAX_CALLBACK_REQUESTS),
                (entry_limit, "root entry limit", MAX_ROOT_ENTRIES),
                (semantic_limit, "root semantic limit", MAX_ROOT_CALLBACK_SEMANTIC_STEPS)):
            _uint(value, name, maximum=maximum)
        self._policy = _RootPolicy(meter, root_id, object(), instruction_limit,
                                   callback_limit, entry_limit, semantic_limit)
        self._state = _LedgerState()

    @property
    def meter(self):
        return self._policy.meter

    @property
    def root_id(self):
        return self._policy.root_id

    @property
    def root_token(self):
        return self._policy.root_token

    @property
    def instruction_limit(self):
        return self._policy.instruction_limit

    @property
    def callback_limit(self):
        return self._policy.callback_limit

    @property
    def entry_limit(self):
        return self._policy.entry_limit

    @property
    def semantic_limit(self):
        return self._policy.semantic_limit

    @property
    def semantic_steps(self):
        return self._state.semantic_steps

    @property
    def instructions(self):
        return self._state.instructions

    @property
    def cycles(self):
        return self._state.cycles

    @property
    def callbacks(self):
        return self._state.callbacks

    @property
    def entries(self):
        return self._state.entries

    @property
    def last_receipt(self):
        return self._state.receipt

    @property
    def depth(self):
        return len(self._state.active)

    def require_entry(self):
        if self.entries >= self.entry_limit:
            raise ForeignTaskBudgetExceeded("task root entry allowance exhausted")
        if self.instructions >= self.instruction_limit:
            raise ForeignTaskBudgetExceeded("task root instruction allowance exhausted")
        if len(self._state.active) >= MAX_DEPTH:
            raise ForeignTaskError("task foreign depth exceeds eight")
        if self._state.active and self._state.active[-1].terminal:
            raise ForeignTaskError("failed task invocation must be canceled before entry")
        if self._state.active and self._state.active[-1].state is not ForeignStateV1.CALLBACK:
            raise ForeignTaskError("task child entry requires a parked callback")

    def settle(self, receipt, *, issued_receipt, starting_operation=None):
        if receipt is not issued_receipt:
            raise ForeignTaskError("task receipt is not the adapter's latest issued identity")
        _value(receipt, ForeignReceiptV1)
        state = self._state
        if receipt is state.receipt:
            if receipt != state.receipt_values:
                raise ForeignTaskError("already-settled task receipt values changed")
            return False
        if receipt.root_id != self.root_id or receipt.sequence != state.sequence + 1:
            raise ForeignTaskError("task receipt root or sequence is discontinuous")
        if (receipt.root_instructions != self.instructions + receipt.instructions
                or receipt.root_cycles != self.cycles + receipt.cycles
                or receipt.root_callbacks != self.callbacks + receipt.callback_requests
                or receipt.root_entries != self.entries + int(receipt.invocation_started)):
            raise ForeignTaskError("task root receipt counters are discontinuous")
        if (receipt.root_instructions > self.instruction_limit
                or receipt.root_callbacks > self.callback_limit or receipt.root_entries > self.entry_limit):
            raise ForeignTaskError("task adapter exceeded the original root allowance")
        if receipt.invocation_started:
            self.require_entry()
            _value(starting_operation, ForeignOperationV1)
            parent = None if not state.active else state.active[-1].invocation_id
            if (receipt.invocation_id <= state.last_invocation_id
                    or receipt.parent_invocation_id != parent or receipt.depth != len(state.active) + 1):
                raise ForeignTaskError("task started receipt does not extend the owned chain")
            if any(item.registration is starting_operation.registration for item in state.active):
                raise ForeignTaskError("task same-registration recursion is unsupported")
            account = _InvocationAccount(receipt.invocation_id, parent, receipt.depth,
                                         starting_operation.registration,
                                         starting_operation.max_instructions,
                                         starting_operation.max_callbacks)
        else:
            if starting_operation is not None or not state.active:
                raise ForeignTaskError("task continuing receipt has no active invocation")
            account = state.active[-1]
            if (account.terminal or receipt.invocation_id != account.invocation_id
                    or receipt.parent_invocation_id != account.parent_id or receipt.depth != account.depth):
                raise ForeignTaskError("task receipt does not belong to the active leaf")
        if (receipt.invocation_instructions != account.instructions + receipt.instructions
                or receipt.invocation_cycles != account.cycles + receipt.cycles
                or receipt.invocation_callbacks != account.callbacks + receipt.callback_requests
                or receipt.invocation_instructions > account.instruction_limit
                or receipt.invocation_callbacks > account.callback_limit):
            raise ForeignTaskError("task invocation receipt counters are discontinuous")
        account = replace(account, instructions=receipt.invocation_instructions,
                          cycles=receipt.invocation_cycles, callbacks=receipt.invocation_callbacks,
                          terminal=receipt.terminal, state=receipt.state)
        active = state.active + (account,) if receipt.invocation_started else state.active[:-1] + (account,)
        if receipt.state is ForeignStateV1.RETURNED:
            active = active[:-1]
        # All allocation/validation precedes this one publication. Recovery
        # therefore observes either the complete old state or complete receipt.
        self._state = _LedgerState(
            receipt.root_instructions, receipt.root_cycles, receipt.root_callbacks,
            receipt.root_entries, receipt.sequence,
            receipt.invocation_id if receipt.invocation_started else state.last_invocation_id,
            receipt, replace(receipt), active,
        )
        return True

    def retire_suffix(self, invocation_ids):
        if type(invocation_ids) is not tuple or len(invocation_ids) > MAX_DEPTH:
            raise TypeError("retired invocation IDs must be a bounded exact tuple")
        for value in invocation_ids:
            _uint(value, "retired invocation ID", minimum=1)
        state = self._state
        expected = tuple(item.invocation_id for item in reversed(state.active[-len(invocation_ids):])) if invocation_ids else ()
        if invocation_ids != expected:
            raise ForeignTaskError("task cancellation does not retire an exact active suffix")
        if invocation_ids:
            self._state = replace(state, active=state.active[:-len(invocation_ids)])


_CORE_FUNCTION_SEALS = tuple((name, _FunctionSeal.capture(function))
                             for name, function in _CORE_HELPERS)


__all__ = [
    "CapturedTaskExport", "ForeignDefinition", "ForeignResumeTarget",
    "ForeignRootLedger", "ForeignTaskBudgetExceeded", "ForeignTaskEngine", "ForeignTaskError",
]
