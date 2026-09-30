"""Captured semantic dependencies and accounting for task foreign dispatch.

This is the host-owned foundation of the reference task profile. It does not
execute a foreign definition, install a native adapter, or advertise a task
capability. The ordinary dispatcher must explicitly own every transition.
"""

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, field, replace
from types import FunctionType, GetSetDescriptorType, MemberDescriptorType, MethodType
from weakref import WeakKeyDictionary

from shared.cells import CELL_BYTES, MASK64
from shared.foreign_abi import (
    ForeignAccessV1, ForeignExportV1, ForeignOperationV1, ForeignReceiptV1,
    ForeignSignatureV1, ForeignSpanV1, ForeignStateV1, MAX_CALLBACK_REQUESTS, MAX_DEPTH,
    MAX_ROOT_CALLBACK_SEMANTIC_STEPS, MAX_ROOT_ENTRIES, MAX_ROOT_INSTRUCTIONS,
)
from simulator import core_words, stacks as _stack_module, runtime as _runtime_module
from simulator import foreign_effects as _effect_module
from simulator import memory as _memory_module
from simulator.foreign_effects import TaskEffectGuard, TaskEffectScope, _EFFECT_ROUTES
from simulator.foreign_control import ForeignContinuation, ForeignReturnControl, ForeignRetirement, _LiveReturn
from simulator.dictionary import BodyAllocationLease, Dictionary, Word
from simulator.errors import ExecutionError
from simulator.foreign_types import ForeignDefinition, ForeignResumeTarget, ForeignDispatchReport
from simulator.ir import (
    Branch, BranchZero, Call, CallSelf, Do, Literal, Loop, PlusLoop, QuestionDo,
    RestoreDataStackPointer, RestoreReturnStackPointer, Return, RPeek, RPeekPair,
    RPop, RPopPair, RPush, RPushPair, StoreValue, Unloop,
)
from simulator.memory import SparseAddressSpace, _QualifiedOrdinarySpan, _SparseRegion, _DenseRegion, RegionSpec, _ResolvedSpan
from simulator.runtime import (
    ColonDefinition, ConstantDefinition, CreatedDefinition, DoesBodyRef,
    ExecutionContext, MegaForthRuntime, PrimitiveDefinition, ValueDefinition,
    _StepMeter,
    _TASK_DISPATCH_ALIASES,
)
from simulator.stacks import DataStack, ReturnStack, Continuation, FaultAbort


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
_DICTIONARY_ROUTES = tuple((name, vars(Dictionary)[name]) for name in (
    "find", "resolve", "words", "acquire_body_lease", "is_body_lease_live"))
_STACK_ROUTES = tuple((kind, tuple((name, value) for name, value in vars(kind).items()
                                  if type(value) is FunctionType or type(value) is property))
                      for kind in (DataStack, ReturnStack))
_VIEW_ROUTES = tuple(vars(_QualifiedOrdinarySpan).items())
_MEMORY_GLOBALS = tuple((name, vars(_memory_module)[name]) for name in (
    "_checked_span", "_require_integer", "_ResolvedSpan", "_QualifiedOrdinarySpan",
    "_SparseRegion", "_DenseRegion", "RegionSpec", "AddressClass", "struct",
    "_INTEGER_WIDTHS", "MMIO_BASE", "MMIO_LIMIT", "ADDRESS_SPACE_SIZE",
))
_MEMORY_FORMATS = dict(_memory_module._INTEGER_FORMATS)
_MEMORY_STRUCT = (_memory_module.struct.unpack_from, _memory_module.struct.pack_into)
_CORE_HELPERS = tuple((name, value) for name, value in vars(core_words).items()
                     if type(value) is FunctionType)
_METADATA_ROUTES = tuple((kind, tuple(vars(kind).items())) for kind in (
    Word, BodyAllocationLease, PrimitiveDefinition, ColonDefinition, ConstantDefinition, ValueDefinition,
    CreatedDefinition, DoesBodyRef, *_OPERATION_FIELDS,
    ForeignSignatureV1, ForeignSpanV1, ForeignExportV1, ForeignOperationV1, ForeignReceiptV1,
    ExecutionContext, ForeignDefinition, ForeignResumeTarget,
    _QualifiedOrdinarySpan, _SparseRegion, _DenseRegion, RegionSpec, _ResolvedSpan,
    Continuation, FaultAbort, ForeignContinuation, ForeignReturnControl, ForeignRetirement, _LiveReturn,
)) + _EFFECT_ROUTES
_NAMESPACE_DESCRIPTORS = (
    (SparseAddressSpace, vars(SparseAddressSpace)["__dict__"]),
    (Dictionary, vars(Dictionary)["__dict__"]),
    (DataStack, vars(DataStack)["__dict__"]),
    (ReturnStack, vars(ReturnStack)["__dict__"]),
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
    if (kind.__getattribute__ is not object.__getattribute__ or "__getattr__" in fields
            or kind.__setattr__ is not object.__setattr__ or kind.__delattr__ is not object.__delattr__):
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
            if (type(value) in (FunctionType, MemberDescriptorType, property, classmethod, staticmethod)
                    or name in ("__setattr__", "__delattr__", "__dict__")):
                if fields.get(name) is not value:
                    raise ForeignTaskError("task metadata descriptor routing changed")


def _overlaps(base, size, other_base, other_limit):
    return bool(size and base < other_limit and other_base < base + size)


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
    body_lease: BodyAllocationLease | None = field(default=None, repr=False)
    body_bytes: bytes = field(default=b"", repr=False)
    lease_evidence: tuple = field(default=(), repr=False)


@dataclass(frozen=True, slots=True)
class TaskAdapterRootPolicy:
    """Original ceilings only; this value issues no transition authority."""

    instruction_limit: int
    callback_limit: int
    entry_limit: int

    def __post_init__(self):
        _uint(self.instruction_limit, "root instruction limit", maximum=MAX_ROOT_INSTRUCTIONS)
        _uint(self.callback_limit, "root callback limit", maximum=MAX_CALLBACK_REQUESTS)
        _uint(self.entry_limit, "root entry limit", maximum=MAX_ROOT_ENTRIES)


class TaskRegistrationBatch:
    """One engine-issued host publication transaction, never guest code."""

    def __init__(self, engine):
        self._engine = engine
        self._closed = False

    def _require(self):
        engine = self._engine
        if (type(self) is not TaskRegistrationBatch or self._closed is not False
                or engine._batch is not self or not engine._registration_active
                or engine._batch_calling or engine._batch_failure is not None):
            raise ForeignTaskError("task registration batch is stale, foreign or reentrant")
        return engine

    def _call(self, callback, *args, **kwargs):
        engine = self._require()
        engine._batch_calling = True
        try:
            result = callback(*args, **kwargs)
            if type(result) is Word:
                if len(engine._batch_words) >= _MAX_REGISTRATIONS + _MAX_WORDS:
                    raise ForeignTaskError("task batch publishes too many Words")
                engine._batch_words += (result,)
            return result
        except BaseException as failure:
            if engine._batch_failure is None:
                engine._batch_failure = failure
            raise
        finally:
            engine._batch_calling = False

    def define_operation(self, name, adapter, operation, *, initial_body=b"", protected_spans=()):
        engine = self._require()
        return self._call(engine._define_operation, name, adapter, operation,
                          initial_body=initial_body, protected_spans=protected_spans, batch=self)

    def define_colon(self, name, operations):
        engine = self._require()
        return self._call(engine._batch_define_colon, self, name, operations)

    def capture_export(self, target, signature, *, task_grants=(), dynamic_targets=(),
                       fault_target=None, max_semantic_steps=4096):
        engine = self._require()
        return self._call(engine._capture_export, target, signature,
                          task_grants=task_grants, dynamic_targets=dynamic_targets,
                          fault_target=fault_target, max_semantic_steps=max_semantic_steps, batch=self)

    def body_lease(self, word):
        engine = self._require()
        if type(word) is not Word or not any(item is word for item in engine._batch_words):
            raise ForeignTaskError("body lease requires this batch's exact published Word")
        return self._call(engine._batch_body_lease, self, word)

    def task_export_dependencies(self, export):
        engine = self._require()
        return self._call(engine.task_export_dependencies, export)

    def on_rollback(self, callback):
        engine = self._require()
        if engine._batch_rollback is not None:
            raise ForeignTaskError("task batch already has its one rollback participant")
        if type(callback) is FunctionType:
            engine._batch_rollback = (_FunctionSeal.capture(callback), None, None)
        elif type(callback) is MethodType and type(callback.__func__) is FunctionType:
            owner, function = callback.__self__, callback.__func__
            kind = type(owner)
            if type(kind) is not type:
                raise TypeError("rollback owner must be an ordinary Python class")
            name = next((name for name, value in vars(kind).items() if value is function), None)
            if type(name) is not str:
                raise TypeError("rollback method must be defined by its exact owner class")
            descriptor = vars(kind).get("__dict__")
            _namespace(owner, kind, ((name, function),), descriptor=descriptor)
            engine._batch_rollback = (_FunctionSeal.capture(function), owner, (kind, name, descriptor))
        else:
            raise TypeError("rollback participant must be an exact Python function or bound method")

    def _rollback_owned(self, evidence):
        if evidence is None:
            return
        seal, owner, route = evidence
        seal.verify()
        if route is None:
            seal.callback()
        else:
            kind, name, descriptor = route
            _namespace(owner, kind, ((name, seal.callback),), descriptor=descriptor)
            seal.callback(owner)


class ForeignTaskEngine:
    """One runtime's opt-in task registrations and exact captured dependencies."""

    def __init__(self, runtime, *, core_installed):
        self._runtime = runtime
        self._dictionary = runtime.dictionary
        self._memory = runtime.memory
        self._context = runtime.main_context
        self._stacks = tuple((stack, stack._memory_view, stack._floor, stack._empty_pointer)
                             for stack in (self._context.data, self._context.returns))
        self._owner = object()
        self._bindings: dict[int, _ForeignBinding] = {}
        self._capture_state: tuple[dict[int, CapturedTaskExport], int] = ({}, 0)
        self._canonical: dict[int, _CapturedWord] = {}
        self._task_root = None
        self._root_ownership = None
        self._cleanup_ownership = None
        self._limits = (MAX_ROOT_INSTRUCTIONS, MAX_CALLBACK_REQUESTS,
                        MAX_ROOT_ENTRIES, MAX_ROOT_CALLBACK_SEMANTIC_STEPS)
        self._last_dispatch = None
        self._publication_failure = None
        self._execution_failure = None
        self._memory_evidence = None
        self._registration_active = False
        self._batch = None
        self._batch_calling = False
        self._batch_rollback = None
        self._batch_adapter = None
        self._batch_failure = None
        self._batch_words = ()
        self._batch_operation_count = 0
        self._machine_profiles = WeakKeyDictionary()
        self._effect_functions = tuple(_FunctionSeal.capture(
            value.__func__ if type(value) in (classmethod, staticmethod) else value)
            for _kind, routes in _METADATA_ROUTES for _name, value in routes
            if type(value) in (FunctionType, classmethod, staticmethod))
        self._backing_functions = tuple(_FunctionSeal.capture(value)
            for _name, value in (*_MEMORY_ROUTES, *(_item for _kind, routes in _STACK_ROUTES for _item in routes),
                                 *_MEMORY_GLOBALS, *_DICTIONARY_ROUTES) if type(value) is FunctionType)
        if core_installed:
            for name in _CORE_NAMES:
                word = self._dictionary.find(name)
                if word is not None and type(word.implementation) is PrimitiveDefinition:
                    self._canonical[id(word)] = _CapturedWord(
                        word, word.xt, word.header_address, word.implementation,
                        function=_FunctionSeal.capture(word.implementation.callback),
                    )
        from simulator.foreign_dispatch import TaskDispatchRoot

        self._dispatch_kind = TaskDispatchRoot
        self._dispatch_routes = tuple(vars(TaskDispatchRoot).items())
        self._dispatch_namespace = vars(TaskDispatchRoot)["__dict__"]
        self._dispatch_functions = tuple(_FunctionSeal.capture(value)
            for _name, value in self._dispatch_routes if type(value) is FunctionType)
        runtime._private_host_abort.install_task_issuers(self, TaskDispatchRoot)

    @property
    def _exports(self):
        return self._capture_state[0]

    @property
    def last_dispatch(self):
        return self._last_dispatch

    def configure_limits(self, *, instruction_limit=None, callback_limit=None,
                         entry_limit=None, semantic_limit=None):
        with self._runtime._session_owner_lock:
            self._require_idle("configure task limits")
            requested = (instruction_limit, callback_limit, entry_limit, semantic_limit)
            values = tuple(old if value is None else value for old, value in zip(self._limits, requested))
            for value, maximum in zip(values, (MAX_ROOT_INSTRUCTIONS, MAX_CALLBACK_REQUESTS,
                                               MAX_ROOT_ENTRIES, MAX_ROOT_CALLBACK_SEMANTIC_STEPS)):
                _uint(value, "task root limit", maximum=maximum)
            self._limits = values

    def claim_machine_profile(self, meter, profile):
        """Exclude private/task mixing for the entire original meter lifetime.

        This choice may outlive one guarded task root. It neither shares nor
        renews that root's accounting ledger and grants no entry authority.
        """
        if self._publication_failure is not None or self._execution_failure is not None:
            raise ForeignTaskError("task interop cleanup failed; further machine admission is disabled")
        if (type(meter) is not _StepMeter or _StepMeter.__hash__ is not object.__hash__
                or _StepMeter.__eq__ is not object.__eq__):
            raise TypeError("machine profile requires the exact original meter")
        if type(profile) is not str or profile not in ("private", "task"):
            raise ValueError("machine profile must be private or task")
        if not any(frame.meter is meter for frame in self._runtime._active_dispatches):
            raise ForeignTaskError("machine profile has no active original dispatch")
        previous = self._machine_profiles.get(meter)
        if previous is not None and previous != profile:
            raise ForeignTaskError("one original semantic meter cannot mix private and task machine profiles")
        self._machine_profiles[meter] = profile

    def adapter_root_policy(self, adapter, root_token, root_id):
        self._require_task(self._context)
        _uint(root_id, "task root ID", minimum=1)
        root = self._task_root
        if (root is None or root.adapter is None or root.adapter.adapter is not adapter
                or root.ledger.root_token is not root_token or root.ledger.root_id != root_id
                or root.busy is not True):
            raise ForeignTaskError("adapter policy requires its exact active original task root")
        root.adapter.verify()
        return TaskAdapterRootPolicy(root.ledger.instruction_limit, root.ledger.callback_limit,
                                     root.ledger.entry_limit)

    def task_export_dependencies(self, export):
        capture = self.require_export(export)
        dependencies = []
        for evidence in capture.words:
            if type(evidence.implementation) is ForeignDefinition:
                operation = self.require_definition(evidence.word).operation
                if not any(item is operation for item in dependencies):
                    dependencies.append(operation)
        return tuple(dependencies)

    @contextmanager
    def registration_batch(self):
        """Publish exact host declarations under one rollback boundary."""
        with self._runtime._session_owner_lock:
            self._require_idle("begin task registration batch")
            checkpoint = self._dictionary.checkpoint()
            previous_bindings, previous_captures = self._bindings, self._capture_state
            batch = TaskRegistrationBatch(self)
            rollback_owned = TaskRegistrationBatch._rollback_owned
            rollback_seal = _FunctionSeal.capture(rollback_owned)
            try:
                self._batch = batch
                self._registration_active = True
                yield batch
                if self._batch_failure is not None:
                    raise self._batch_failure
                self._batch_calling = True
                for key, binding in self._bindings.items():
                    if previous_bindings.get(key) is not binding:
                        self.require_definition(binding.word)
                for key, capture in self._exports.items():
                    if previous_captures[0].get(key) is not capture:
                        self._verify_export(capture)
            except BaseException as failure:
                self._batch_calling = False
                self._publication_failure = failure
                clean = True
                try:
                    rollback_seal.verify()
                    rollback_owned(batch, self._batch_rollback)
                except BaseException:
                    clean = False
                    try:
                        BaseException.add_note(failure, "task native publication rollback failed; further admission is disabled")
                    except BaseException:
                        pass
                self._bindings, self._capture_state = previous_bindings, previous_captures
                try:
                    self._dictionary.rollback(checkpoint)
                    self._runtime.dictionary_index.rebuild()
                except BaseException:
                    clean = False
                    try:
                        BaseException.add_note(failure, "task dictionary rollback failed; further admission is disabled")
                    except BaseException:
                        pass
                if clean:
                    self._publication_failure = None
                raise
            finally:
                self._batch_calling = False
                self._registration_active = False
                self._batch = None
                self._batch_rollback = None
                self._batch_adapter = None
                self._batch_failure = None
                self._batch_words = ()
                self._batch_operation_count = 0
                batch._closed = True

    def _require_batch(self, batch):
        if (type(batch) is not TaskRegistrationBatch or self._batch is not batch
                or batch._closed is not False or self._batch_calling is not True
                or self._registration_active is not True):
            raise ForeignTaskError("task publication requires the exact active batch")
        self._require_task(self._context)

    def _batch_define_colon(self, batch, name, operations):
        self._require_batch(batch)
        if type(operations) is not tuple or not operations:
            raise TypeError("task batch colon requires a nonempty exact IR tuple")
        if len(operations) + self._batch_operation_count > _MAX_OPERATIONS:
            raise ForeignTaskError("task batch semantic IR exceeds 4096 operations")
        for operation in operations:
            kind = type(operation)
            if kind not in _OPERATION_FIELDS:
                raise ForeignTaskError("task batch colon contains an unsupported operation")
            name_of_field = _OPERATION_FIELDS[kind]
            if name_of_field is not None:
                value = getattr(operation, name_of_field)
                _uint(value, "task operation field")
                if kind in _BRANCH_TYPES and value >= len(operations):
                    raise ForeignTaskError("task batch branch escapes its exact IR tuple")
                if kind is Call:
                    self._word(value)
        if type(operations[-1]) is not Return:
            raise ForeignTaskError("task batch colon requires a final Return")
        if type(name) not in (str, bytes):
            raise TypeError("task name must be exact bytes or str")
        width = self._dictionary.definition_size(name)
        rejection = self._runtime._dictionary_growth_rejection(width, self._context)
        if rejection is not None:
            raise ForeignTaskError(f"task batch dictionary growth rejected: {rejection}")
        word = self._runtime._define_public_dictionary_word(name, ColonDefinition(operations))
        self._batch_operation_count += len(operations)
        return word

    def _batch_body_lease(self, batch, word):
        self._require_batch(batch)
        return self._dictionary.acquire_body_lease(word)

    def root_for(self, context, meter):
        self._require_task(context)
        self.claim_machine_profile(meter, "task")
        frames = [frame for frame in self._runtime._active_dispatches
                  if frame.context is context and frame.meter is meter and frame.closed_guard is None]
        if not frames:
            raise ForeignTaskError("task entry has no owning semantic dispatch")
        root = next((frame.task_root for frame in frames if frame.task_root is not None), None)
        if root is None:
            root = self._dispatch_kind(self, meter, frames[0].root_id, self._limits)
            try:
                namespace = self._dispatch_namespace.__get__(root, self._dispatch_kind)
                self._root_ownership = (root, namespace, tuple((name, getattr(root, name)) for name in (
                    "engine", "context", "ledger", "issuer", "effects", "control", "frames",
                    "_meter_namespace", "_meter_policy", "_effects_close", "_control_close", "_cleanup_functions")))
            except BaseException as failure:
                # No host callback or machine instruction has run; release
                # the exact constructor-owned control before publication.
                for close, owned in ((TaskEffectGuard.close, root.effects),
                                     (ForeignReturnControl.close, root.control)):
                    try:
                        close(owned, root.issuer)
                    except BaseException as cleanup:
                        self._execution_failure = cleanup
                        try:
                            BaseException.add_note(failure, "task root admission cleanup also failed")
                        except BaseException:
                            pass
                raise
        elif root.engine is not self:
            raise ForeignTaskError("task root belongs to a different engine")
        self._task_root = root
        for frame in frames:
            object.__setattr__(frame, "task_root", root)
        return root

    def finish_root(self, root, *, completed):
        ownership = self._restore_cleanup_owners(root)
        dict.__setitem__(ownership[1], "closed", False)
        try:
            self._root_cleanup_call(root, "close", completed=completed)
        except BaseException as failure:
            self._execution_failure = failure
            completed = False
            raise
        finally:
            ledger = dict(ownership[2])["ledger"]
            namespace = ownership[1]
            dict.__setitem__(namespace, "closed", True)
            cancelled = dict.get(namespace, "cancelled")
            if type(cancelled) is not bool or not completed:
                cancelled = True
                dict.__setitem__(namespace, "cancelled", True)
            self._last_dispatch = ForeignDispatchReport(
                ledger.root_id, ledger.instructions, ledger.cycles, ledger.callbacks,
                ledger.entries, ledger.semantic_steps, completed, cancelled)
            if self._task_root is root:
                self._task_root = None
                self._cleanup_ownership = self._root_ownership
                self._root_ownership = None

    def _restore_cleanup_owners(self, root):
        ownership = self._root_ownership
        if ownership is None or ownership[0] is not root:
            ownership = self._cleanup_ownership
        if ownership is None or ownership[0] is not root:
            raise ForeignTaskError("task cleanup lacks its original owner evidence")
        namespace = ownership[1]
        self._dispatch_namespace.__set__(root, namespace)
        for name, value in ownership[2]:
            dict.__setitem__(namespace, name, value)
        # Remove only shadowed host method routes. Guest stack/memory state is
        # never restored by this repair of host reference projections.
        for name, _value in self._dispatch_routes:
            dict.pop(namespace, name, None)
        return ownership

    def _root_cleanup_call(self, root, name, **kwargs):
        self._restore_cleanup_owners(root)
        fields = vars(self._dispatch_kind)
        if (any(type(key) is not str for key in fields)
                or self._dispatch_kind.__getattribute__ is not object.__getattribute__
                or self._dispatch_kind.__setattr__ is not object.__setattr__
                or self._dispatch_kind.__delattr__ is not object.__delattr__
                or any(name in fields for name, _value in self._restore_cleanup_owners(root)[2])
                or any(method not in ("close", "cleanup_safe", "mark_unsafe_cleanup")
                       and fields.get(method) is not value for method, value in self._dispatch_routes)):
            raise ForeignTaskError("task canonical cleanup implementation changed")
        callback = next(value for method, value in self._dispatch_routes if method == name)
        seal = next(seal for seal in self._dispatch_functions if seal.callback is callback)
        seal.verify()
        return callback(root, **kwargs)

    def task_cleanup_safe(self, root):
        try:
            return self._root_cleanup_call(root, "cleanup_safe")
        except BaseException:
            return False

    def mark_task_cleanup_unsafe(self, root):
        ownership = self._restore_cleanup_owners(root)
        context = dict(ownership[2])["context"]
        descriptor = next(value for kind, fields in _METADATA_ROUTES if kind is ExecutionContext
                          for name, value in fields if name == "_host_control_fault")
        descriptor.__set__(context, "task host failure changed trusted cleanup authority")

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
        if self._registration_active and not (self._batch is not None and self._batch_calling is True):
            raise ForeignTaskError("task registration is already in progress")
        if self._publication_failure is not None:
            raise ForeignTaskError("task registration rollback failed; further admission is disabled")
        if self._execution_failure is not None:
            raise ForeignTaskError("task transport cleanup failed; further foreign admission is disabled")
        _require_metadata()
        ownership = self._root_ownership
        if ownership is not None:
            root, original_namespace, original_fields = ownership
            _namespace(root, self._dispatch_kind, self._dispatch_routes,
                       descriptor=self._dispatch_namespace)
            for seal in self._dispatch_functions:
                seal.verify()
            namespace = self._dispatch_namespace.__get__(root, self._dispatch_kind)
            fields = vars(self._dispatch_kind)
            if (root is not self._task_root or namespace is not original_namespace
                    or dict.get(namespace, "closed") is not False
                    or type(dict.get(namespace, "busy")) is not bool
                    or any(name in fields or dict.get(namespace, name) is not value
                                                 for name, value in original_fields)):
                raise ForeignTaskError("task original dispatcher ownership changed")
        if any(type(key) is not str for module in (core_words, _stack_module, _effect_module,
                                                  _runtime_module, _memory_module) for key in vars(module)):
            raise ForeignTaskError("task consumed module namespace changed")
        if (vars(core_words).get("TaskEffectGuard") is not TaskEffectGuard
                or vars(_stack_module).get("TaskEffectGuard") is not TaskEffectGuard
                or vars(_effect_module).get("TaskEffectGuard") is not TaskEffectGuard
                or vars(_effect_module).get("TaskEffectScope") is not TaskEffectScope
                or any(vars(_runtime_module).get(name) is not original
                       for name, original in _TASK_DISPATCH_ALIASES)):
            raise ForeignTaskError("task dispatcher or effect helper alias changed")
        for seal in self._effect_functions:
            seal.verify()
        if (any(vars(_memory_module).get(name) is not original for name, original in _MEMORY_GLOBALS)
                or type(_memory_module._INTEGER_FORMATS) is not dict
                or any(type(key) is not int or type(value) is not str
                       for key, value in _memory_module._INTEGER_FORMATS.items())
                or _memory_module._INTEGER_FORMATS != _MEMORY_FORMATS
                or _memory_module.struct.unpack_from is not _MEMORY_STRUCT[0]
                or _memory_module.struct.pack_into is not _MEMORY_STRUCT[1]):
            raise ForeignTaskError("task ordinary memory helper route changed")
        for seal in self._backing_functions:
            seal.verify()
        runtime = self._runtime
        if (type(runtime) is not MegaForthRuntime or type(context) is not ExecutionContext
                or runtime._foreign_tasks is not self
                or context is not self._context or runtime.main_context is not context
                or runtime.memory is not self._memory or runtime.dictionary is not self._dictionary
                or type(context.data) is not DataStack or type(context.returns) is not ReturnStack
                or context.data._memory is not self._memory or context.returns._memory is not self._memory):
            raise ForeignTaskError("task profile requires the original canonical main context")
        for actual, evidence, (kind, routes) in zip((context.data, context.returns), self._stacks, _STACK_ROUTES):
            stack, view, floor, empty = evidence
            _namespace(actual, kind, routes)
            if (actual is not stack or actual._memory_view is not view
                    or type(actual._floor) is not int or actual._floor != floor
                    or type(actual._empty_pointer) is not int or actual._empty_pointer != empty
                    or type(actual._pointer) is not int or not floor <= actual._pointer <= empty
                    or actual._pointer % CELL_BYTES):
                raise ForeignTaskError("task profile original stack geometry changed")
        fields = vars(_QualifiedOrdinarySpan)
        if any(type(key) is not str for key in fields) or any(fields.get(name) is not value for name, value in _VIEW_ROUTES):
            raise ForeignTaskError("task stack backing routes changed")
        _namespace(self._memory, SparseAddressSpace, _MEMORY_ROUTES)
        self._require_memory_backing()
        _namespace(self._dictionary, Dictionary, _DICTIONARY_ROUTES)
        for name, seal in _CORE_FUNCTION_SEALS:
            if vars(core_words).get(name) is not seal.callback:
                raise ForeignTaskError("canonical task core helper changed")
            seal.verify()

    def _require_memory_backing(self):
        regions = self._memory._regions
        if type(regions) is not tuple or len(regions) > 4:
            raise ForeignTaskError("task memory regions are not canonical")
        evidence = []
        previous = 0
        for region in regions:
            if type(region) not in (_SparseRegion, _DenseRegion) or type(region.spec) is not RegionSpec:
                raise ForeignTaskError("task memory backing has a custom owner")
            spec = region.spec
            if (type(spec.base) is not int or type(spec.size) is not int
                    or spec.base < previous or spec.size <= 0 or spec.size > MASK64 + 1 - spec.base):
                raise ForeignTaskError("task ordinary memory geometry changed")
            previous = spec.base + spec.size
            if type(region) is _SparseRegion:
                pages = region.pages
                if (type(region.page_size) is not int or region.page_size <= 0
                        or type(pages) is not dict
                        or any(type(key) is not int or key < 0 for key in pages)
                        or any(type(page) is not bytearray or len(page) != region.page_size for page in pages.values())):
                    raise ForeignTaskError("task sparse backing may execute custom access code")
                backing = pages
                width = region.page_size
            else:
                backing = region._buffer
                if (type(backing) is not memoryview or backing.readonly or not backing.c_contiguous
                        or backing.ndim != 1 or backing.itemsize != 1 or len(backing) != spec.size):
                    raise ForeignTaskError("task dense backing is not its fixed writable view")
                width = spec.size
            evidence.append((region, spec, spec.base, spec.size, backing, width))
        if type(self._memory._specs) is not tuple or len(self._memory._specs) != len(evidence):
            raise ForeignTaskError("task memory specification table changed")
        if any(spec is not item[1] for spec, item in zip(self._memory._specs, evidence)):
            raise ForeignTaskError("task memory specifications differ from its regions")
        prior = self._memory_evidence
        if prior is None:
            self._memory_evidence = regions, tuple(evidence)
        elif (regions is not prior[0] or len(evidence) != len(prior[1])
              or any(a[0] is not b[0] or a[1] is not b[1] or a[2:4] != b[2:4]
                     or a[4] is not b[4] or a[5] != b[5] for a, b in zip(evidence, prior[1]))):
            raise ForeignTaskError("task original memory ownership changed")
        for stack, view, floor, empty in self._stacks:
            if (type(view) is not _QualifiedOrdinarySpan or type(view._base) is not int
                    or type(view._offset) is not int or view._base != floor
                    or not any(view._region is row[0] and row[2] <= floor < empty <= row[2] + row[3]
                               and view._offset == floor - row[2] for row in evidence)):
                raise ForeignTaskError("task original stack backing view changed")

    def require_cleanup_routes(self):
        """Prove only routes used by ordinary stack restoration after failure."""
        _require_metadata()
        for seal in self._effect_functions:
            seal.verify()
        if any(type(key) is not str for key in vars(_memory_module)):
            raise ForeignTaskError("task cleanup memory namespace changed")
        if (any(vars(_memory_module).get(name) is not original for name, original in _MEMORY_GLOBALS)
                or _memory_module.struct.unpack_from is not _MEMORY_STRUCT[0]
                or _memory_module.struct.pack_into is not _MEMORY_STRUCT[1]
                or type(_memory_module._INTEGER_FORMATS) is not dict
                or any(type(key) is not int or type(value) is not str
                       for key, value in _memory_module._INTEGER_FORMATS.items())
                or _memory_module._INTEGER_FORMATS != _MEMORY_FORMATS):
            raise ForeignTaskError("task cleanup memory routes changed")
        for seal in self._backing_functions:
            seal.verify()
        _namespace(self._memory, SparseAddressSpace, _MEMORY_ROUTES)
        self._require_memory_backing()

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

    def _define_operation(self, name, adapter, operation, *, protected_spans, initial_body=b"", batch=None):
        if batch is None:
            self._require_idle("define a task foreign operation")
        else:
            self._require_batch(batch)
        if type(name) not in (str, bytes) or type(initial_body) is not bytes:
            raise TypeError("task name and initial body must be exact values")
        if len(initial_body) > (1 << 20) + 15:
            raise ForeignTaskError("task initial body exceeds one padded machine image")
        _value(operation, ForeignOperationV1)
        if len(self._bindings) >= _MAX_REGISTRATIONS:
            raise ForeignTaskError("task machine registration table is full")
        seal = _AdapterSeal.capture(adapter)
        if batch is not None:
            if self._batch_adapter is not None and self._batch_adapter is not adapter:
                raise ForeignTaskError("task batch requires one exact adapter owner")
            self._batch_adapter = adapter
        self._check_grants(operation.machine_grants, machine=True)
        if type(protected_spans) is not tuple or len(protected_spans) > 16 - bool(initial_body):
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
        width = self._dictionary.definition_size(name, initial_body=initial_body)
        rejection = self._runtime._dictionary_growth_rejection(width, self._context)
        if rejection is not None:
            raise ForeignTaskError(f"task registration dictionary growth rejected: {rejection}")
        checkpoint = self._dictionary.checkpoint() if batch is None else None
        previous = self._bindings
        replacement = dict(previous)
        if batch is None:
            self._registration_active = True
        try:
            word = self._runtime._define_public_dictionary_word(name, definition, initial_body=initial_body)
            lease, lease_evidence = None, ()
            if initial_body:
                lease = self._dictionary.acquire_body_lease(word)
                lease_evidence = (lease.word, lease.body_address, lease.body_limit,
                                  lease.allocation_serial, lease._owner)
                protected += (ForeignSpanV1(base=word.body_address, size=len(initial_body), access="read"),)
            replacement[id(definition)] = _ForeignBinding(
                word, definition, operation, metadata, seal, protected, lease, initial_body, lease_evidence,
            )
            self._bindings = replacement
            return word
        except BaseException as failure:
            if batch is not None:
                raise
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
            if batch is None:
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
        if binding.body_lease is not None:
            lease = binding.body_lease
            if type(lease) is not BodyAllocationLease:
                raise ForeignTaskError("task body lease type changed")
            word_owner, base, limit, serial, owner = binding.lease_evidence
            if (lease.word is not word_owner or lease._owner is not owner
                    or type(lease.body_address) is not int or lease.body_address != base
                    or type(lease.body_limit) is not int or lease.body_limit != limit
                    or type(lease.allocation_serial) is not int or lease.allocation_serial != serial
                    or not self._dictionary.is_body_lease_live(lease)
                    or self._memory.read_bytes(base, len(binding.body_bytes)) != binding.body_bytes):
                raise ForeignTaskError("task machine body was reclaimed or changed")
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
                        fault_target, max_semantic_steps, batch=None):
        if batch is None:
            self._require_idle("capture a task callback")
        else:
            self._require_batch(batch)
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
class _SemanticAccount:
    invocation_id: int
    limit: int
    steps: int = 0


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
    semantic_scopes: tuple[_SemanticAccount, ...] = ()


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
            state.semantic_steps,
            state.semantic_scopes,
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
            self._state = replace(state, active=state.active[:-len(invocation_ids)],
                semantic_scopes=tuple(scope for scope in state.semantic_scopes
                                      if scope.invocation_id not in invocation_ids))

    def begin_callback(self, invocation_id, limit):
        _uint(invocation_id, "callback invocation", minimum=1)
        _uint(limit, "callback semantic allowance", minimum=1, maximum=4096)
        state = self._state
        if (not state.active or state.active[-1].invocation_id != invocation_id
                or len(state.semantic_scopes) >= MAX_DEPTH
                or any(scope.invocation_id == invocation_id for scope in state.semantic_scopes)):
            raise ForeignTaskError("callback semantic account lacks active invocation authority")
        self._state = replace(state, semantic_scopes=state.semantic_scopes + (_SemanticAccount(invocation_id, limit),))

    def end_callback(self, invocation_id):
        state = self._state
        if not state.semantic_scopes or state.semantic_scopes[-1].invocation_id != invocation_id:
            raise ForeignTaskError("callback completion lacks active semantic accounting")
        self._state = replace(state, semantic_scopes=state.semantic_scopes[:-1])

    def require_semantic_step(self):
        if self.semantic_steps >= self.semantic_limit:
            raise ForeignTaskBudgetExceeded("task root semantic allowance exhausted")
        if any(scope.steps >= scope.limit for scope in self._state.semantic_scopes):
            raise ForeignTaskBudgetExceeded("task callback semantic allowance exhausted")

    def charge_semantic_step(self):
        self.require_semantic_step()
        state = self._state
        updated = replace(state, semantic_steps=state.semantic_steps + 1,
                          semantic_scopes=tuple(replace(scope, steps=scope.steps + 1)
                                                for scope in state.semantic_scopes))
        self._state = updated


_CORE_FUNCTION_SEALS = tuple((name, _FunctionSeal.capture(function))
                             for name, function in _CORE_HELPERS)


__all__ = [
    "CapturedTaskExport", "ForeignDefinition", "ForeignResumeTarget",
    "ForeignRootLedger", "ForeignTaskBudgetExceeded", "ForeignTaskEngine", "ForeignTaskError",
]
