"""Live identity evidence and reference-dispatch guards for closed callbacks."""

from __future__ import annotations

from dataclasses import dataclass
from types import MemberDescriptorType

from shared.cells import MASK64
from shared.hybrid_closed import (
    PolicyBodyV3, PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3,
    PolicyBranchV3, PolicyBranchZeroV3, PolicyReturnV3, prove_policies,
)
from simulator import core_words, stacks
from simulator import runtime as runtime_module
from simulator.dictionary import Dictionary, Word
from simulator.errors import StepBudgetExceeded
from simulator.ir import Literal, Call, Branch, BranchZero, Return
from simulator.memory import SparseAddressSpace, _SparseRegion, _QualifiedOrdinarySpan
from simulator.runtime import (
    MegaForthRuntime, ExecutionContext, ColonDefinition, PrimitiveDefinition, _StepMeter, _DispatchFrame,
)
from simulator.stacks import DataStack, ReturnStack, Continuation
from simulator.interop_exports import (
    CallbackExportError, CallbackExportBudgetExceeded, _PrivateStackSeal,
)


def _routes(cls):
    return tuple((name, value) for name, value in vars(cls).items()
                 if callable(value) or isinstance(value, (property, staticmethod, classmethod, MemberDescriptorType)))


_CLASS_ROUTES = tuple((cls, _routes(cls)) for cls in (
    ExecutionContext, DataStack, ReturnStack, SparseAddressSpace,
    _SparseRegion, _QualifiedOrdinarySpan, Continuation, _StepMeter,
))
_METADATA_ROUTES = tuple((cls, _routes(cls)) for cls in (
    Literal, Call, Branch, BranchZero, Return, Word, ColonDefinition, PrimitiveDefinition, _DispatchFrame,
))
_RUNTIME_ROUTE_NAMES = (
    "_execute_guarded", "_execute_top", "_call_from_colon", "_resolve_dispatch_word",
    "_meter_for_public_call", "_allocate_dispatch_root_id", "_require_session_owner_access",
    "_require_no_suspension", "_require_no_closed_callback_entry",
    "begin_closed_callback_accounting", "consume_closed_callback_accounting",
)
_RUNTIME_ROUTES = tuple((name, getattr(MegaForthRuntime, name)) for name in _RUNTIME_ROUTE_NAMES)
_DICTIONARY_ROUTES = tuple((name, getattr(Dictionary, name)) for name in ("resolve", "find"))
_MAGIC_ROUTES = {cls: tuple((name, getattr(cls, name)) for name in ("__setattr__", "__delattr__"))
                 for cls in (*(item[0] for item in _CLASS_ROUTES + _METADATA_ROUTES), MegaForthRuntime, Dictionary)}
_DICT_DESCRIPTORS = {cls: vars(cls).get("__dict__") for cls in _MAGIC_ROUTES}
_HELPERS = ((core_words, "s64", core_words.s64), (stacks, "u64", stacks.u64),
            (stacks, "Continuation", Continuation))
_RUNTIME_METADATA = tuple((name, getattr(runtime_module, name)) for name in (
    "Literal", "Call", "Branch", "BranchZero", "Return", "Word", "ColonDefinition",
    "PrimitiveDefinition", "_DispatchFrame",
))


def _require_routes(instance, cls, routes):
    if type(instance) is not cls:
        raise CallbackExportError("closed callback requires canonical method owners")
    if cls.__getattribute__ is not object.__getattribute__ or hasattr(cls, "__getattr__"):
        raise CallbackExportError("closed callback attribute routing changed")
    if any(getattr(cls, name) is not original for name, original in _MAGIC_ROUTES[cls]):
        raise CallbackExportError("closed callback attribute mutation routing changed")
    if cls is _StepMeter and any(name in vars(cls) for name in ("steps", "budget", "_on_tick")):
        raise CallbackExportError("closed callback meter field routing changed")
    if vars(cls).get("__dict__") is not _DICT_DESCRIPTORS[cls]:
        raise CallbackExportError("closed callback namespace routing changed")
    attributes = (object.__getattribute__(instance, "__dict__")
                  if _DICT_DESCRIPTORS[cls] is not None else {})
    if type(attributes) is not dict or any(type(name) is not str for name in attributes):
        raise CallbackExportError("closed callback object namespace is not canonical")
    for name, original in routes:
        if name in attributes or vars(cls).get(name) is not original:
            raise CallbackExportError(f"closed callback method route changed: {cls.__name__}.{name}")


def _require_metadata_routes():
    if any(getattr(runtime_module, name) is not value for name, value in _RUNTIME_METADATA):
        raise CallbackExportError("closed callback runtime metadata alias changed")
    # Compare raw descriptors before reading even an exact instance's fields.
    for cls, routes in _METADATA_ROUTES:
        if (cls.__getattribute__ is not object.__getattribute__ or hasattr(cls, "__getattr__")
                or any(getattr(cls, name) is not value for name, value in _MAGIC_ROUTES[cls])
                or any(vars(cls).get(name) is not value for name, value in routes)
                or vars(cls).get("__dict__") is not _DICT_DESCRIPTORS[cls]):
            raise CallbackExportError("closed callback metadata routing changed")


def capture_meter(meter):
    _require_routes(meter, _StepMeter, dict(_CLASS_ROUTES)[_StepMeter])
    namespace = _DICT_DESCRIPTORS[_StepMeter].__get__(meter, _StepMeter)
    steps = dict.get(namespace, "steps")
    if type(steps) is not int or not 0 <= steps <= MASK64:
        raise CallbackExportError("closed callback accounting meter is not exact")
    return namespace, steps


def repair_meter(meter, namespace, expected):
    descriptor = _DICT_DESCRIPTORS[_StepMeter]
    current_namespace = descriptor.__get__(meter, _StepMeter)
    changed = current_namespace is not namespace
    if changed:
        descriptor.__set__(meter, namespace)
    current = dict.get(namespace, "steps")
    if type(current) is not int or current != expected:
        dict.__setitem__(namespace, "steps", expected)
        changed = True
    return changed


@dataclass(frozen=True, slots=True)
class CapturedOperation:
    operation: object
    kind: type
    field: str | None
    value: int | None

    def verify(self):
        _require_metadata_routes()
        if type(self.operation) is not self.kind:
            raise CallbackExportError("closed callback operation type changed")
        if self.field is not None:
            value = getattr(self.operation, self.field)
            if type(value) is not int or value != self.value:
                raise CallbackExportError("closed callback operation field changed")


@dataclass(frozen=True, slots=True)
class CapturedWord:
    word: Word
    xt: int
    implementation: object
    operations: tuple | None
    evidence: tuple[CapturedOperation, ...]
    core_name: str | None = None
    callback: object = None

    def verify(self, dictionary, *, all_operations=True):
        _require_metadata_routes()
        try:
            live = dictionary.resolve(self.xt)
        except KeyError:
            raise CallbackExportError("closed callback Word was removed") from None
        if (live is not self.word or type(live) is not Word
                or type(live.xt) is not int or live.xt != self.xt
                or live.implementation is not self.implementation):
            raise CallbackExportError("closed callback Word or implementation changed")
        if self.core_name is not None:
            if (type(self.implementation) is not PrimitiveDefinition
                    or self.implementation.callback is not self.callback):
                raise CallbackExportError("closed callback canonical primitive changed")
        else:
            if (type(self.implementation) is not ColonDefinition
                    or self.implementation.operations is not self.operations):
                raise CallbackExportError("closed callback operation tuple changed")
            if all_operations:
                for evidence in self.evidence:
                    evidence.verify()


@dataclass(frozen=True, slots=True)
class ClosedCapture:
    entry: CapturedWord
    words: tuple[CapturedWord, ...]
    proof: object

    @classmethod
    def create(cls, engine, descriptor):
        _require_metadata_routes()
        _require_routes(engine._dictionary, Dictionary, _DICTIONARY_ROUTES)
        dictionary = engine._dictionary
        entry = dictionary.find(descriptor.name)
        if entry is None or type(entry.implementation) is not ColonDefinition:
            raise CallbackExportError("closed callback entry must name a live colon definition")
        captured = {}
        pending = [entry]
        total_operations = 0
        while pending:
            word = pending.pop()
            if id(word) in captured:
                continue
            if len(captured) >= 64:
                raise CallbackExportError("closed callback captures more than 64 Words")
            if type(word) is not Word or type(word.xt) is not int or not 0 < word.xt <= MASK64:
                raise CallbackExportError("closed callback requires exact live Word identities")
            implementation = word.implementation
            if type(implementation) is PrimitiveDefinition:
                canonical = next(((name, leaf) for name, leaf in engine._canonical.items()
                                  if leaf.word is word), None)
                if canonical is None:
                    raise CallbackExportError("closed callback primitive is not an original canonical Word")
                name, leaf = canonical
                engine._require_leaf(leaf)
                captured[id(word)] = CapturedWord(word, word.xt, implementation, None, (), name, leaf.callback)
                continue
            if type(implementation) is not ColonDefinition or type(implementation.operations) is not tuple:
                raise CallbackExportError("closed callback target is not a canonical colon or primitive")
            operations = implementation.operations
            total_operations += len(operations)
            if total_operations > 4096 or not operations:
                raise CallbackExportError("closed callback operation count must be in 1..4096")
            evidence = []
            for operation in operations:
                kind = type(operation)
                field = {Literal: "value", Call: "xt", Branch: "target", BranchZero: "target", Return: None}.get(kind, "invalid")
                if field == "invalid":
                    raise CallbackExportError("closed callback contains an unsupported operation")
                value = None if field is None else getattr(operation, field)
                if field is not None and (type(value) is not int or not 0 <= value <= MASK64):
                    raise CallbackExportError("closed callback fields must be exact uint64 integers")
                if kind is Call:
                    if value == 0:
                        raise CallbackExportError("closed callback static XT must be nonzero")
                    try:
                        pending.append(dictionary.resolve(value))
                    except KeyError:
                        raise CallbackExportError("closed callback static target is not live") from None
                evidence.append(CapturedOperation(operation, kind, field, value))
            captured[id(word)] = CapturedWord(word, word.xt, implementation, operations, tuple(evidence))
        words = tuple(captured.values())
        by_xt = {word.xt: word for word in words}
        colons = tuple(word for word in words if word.core_name is None)
        ids = {word.xt: index for index, word in enumerate(colons)}
        bodies = []
        try:
            for word in colons:
                operations = []
                for item in word.evidence:
                    if item.kind is Literal:
                        value = PolicyLiteralV3(value=item.value)
                    elif item.kind is Call:
                        target = by_xt[item.value]
                        value = (PolicyCoreCallV3(name=target.core_name) if target.core_name is not None
                                 else PolicyCallV3(policy_id=ids[target.xt]))
                    elif item.kind is Branch:
                        value = PolicyBranchV3(target=item.value)
                    elif item.kind is BranchZero:
                        value = PolicyBranchZeroV3(target=item.value)
                    else:
                        value = PolicyReturnV3()
                    operations.append(value)
                bodies.append(PolicyBodyV3(policy_id=ids[word.xt], name=f"CAPTURE-{ids[word.xt]}",
                                          operations=tuple(operations)))
            proofs = prove_policies(tuple(bodies))
            proof = next(proof for proof in proofs if proof.policy_id == ids[entry.xt])
            proof.validate_signature(descriptor.input_cells, descriptor.output_cells,
                                     max_semantic_steps=descriptor.max_semantic_steps)
        except (TypeError, ValueError) as error:
            raise CallbackExportError(f"closed callback proof rejected: {error}") from error
        result = cls(captured[id(entry)], words, proof)
        result.verify(engine)
        return result

    def verify(self, engine):
        _require_metadata_routes()
        _require_routes(engine._runtime, MegaForthRuntime, _RUNTIME_ROUTES)
        _require_routes(engine._dictionary, Dictionary, _DICTIONARY_ROUTES)
        engine._require_owner("verify a closed callback")
        for word in self.words:
            word.verify(engine._dictionary)

    def target(self, xt):
        target = next((word for word in self.words if word.xt == xt), None)
        if target is None:
            raise CallbackExportError("closed callback escaped its captured targets")
        return target


class ClosedDispatch:
    """A pinned, one-invocation guard; all execution stays in runtime.py."""

    def __init__(self, engine, binding, context, meter, semantic_step_limit):
        self.engine, self.binding, self.context, self.meter = engine, binding, context, meter
        self.capture = binding.closed
        self.data, self.returns = context.data, context.returns
        self.data_seal, self.return_seal = _PrivateStackSeal.capture(self.data), _PrivateStackSeal.capture(self.returns)
        self.starting_steps = meter.steps
        self.local_limit = binding.descriptor.max_semantic_steps
        self.semantic_step_limit = semantic_step_limit
        self.meter_budget, self.on_tick = meter.budget, meter._on_tick
        self.dictionary_guard = engine._dictionary._mutation_guard
        self.private_regions = self.data._memory._regions
        self.region_spec = self.data_seal.region.spec
        self.root_id = None
        self.frames = None
        self.frame = None
        self.completed = False
        self.outer_contexts = ()
        self.charged_ticks = 0
        self.meter_namespace = object.__getattribute__(meter, "__dict__")
        self.accounting_record = engine._closed_accounting

    def failure(self, message):
        self.engine._registration_failure = "closed callback integrity validation failed"
        return CallbackExportError(message)

    def require_state(self):
        _require_metadata_routes()
        _require_routes(self.engine._runtime, MegaForthRuntime, _RUNTIME_ROUTES)
        _require_routes(self.engine._dictionary, Dictionary, _DICTIONARY_ROUTES)
        objects = (self.context, self.data, self.returns, self.data_seal.memory,
                   self.data_seal.region, self.data_seal.view, self.return_seal.view,
                   self.meter)
        for obj in objects:
            route = next((item for item in _CLASS_ROUTES if type(obj) is item[0]), None)
            if route is None:
                raise self.failure("closed callback private method owner changed")
            _require_routes(obj, *route)
        if _DICT_DESCRIPTORS[_StepMeter].__get__(self.meter, _StepMeter) is not self.meter_namespace:
            raise self.failure("closed callback meter namespace changed")
        self.engine._require_owner("dispatch a closed callback")
        if self.frames is not None:
            if (self.engine._runtime._active_dispatches is not self.frames
                    or type(self.frames) is not list or not self.frames or self.frames[-1] is not self.frame
                    or self.frame.context is not self.context or self.frame.meter is not self.meter
                    or type(self.frame.root_id) is not int or self.frame.root_id != self.root_id
                    or self.frame.closed_guard is not self):
                raise self.failure("closed callback dispatch frame authority changed")
        for seal in (self.data_seal, self.return_seal):
            for value in (seal.stack._floor, seal.stack._empty_pointer, seal.view._base, seal.view._offset):
                if type(value) is not int:
                    raise self.failure("closed callback private geometry is not exact")
        if (self.engine._active is not self or type(self.context) is not ExecutionContext
                or self.context.data is not self.data or self.context.returns is not self.returns
                or not self.data_seal.matches() or not self.return_seal.matches()
                or self.context._host_control_fault is not None
                or self.context._suspension_sequence is not None):
            raise self.failure("closed callback private owner or storage changed")
        if (self.engine._dictionary._mutation_guard is not self.dictionary_guard
                or self.data_seal.memory._regions is not self.private_regions
                or self.data_seal.region.spec is not self.region_spec
                or type(self.data_seal.region.page_size) is not int
                or self.data_seal.region.page_size != 128
                or type(self.data_seal.page) is not bytearray or len(self.data_seal.page) != 128):
            raise self.failure("closed callback backing geometry or mutation guard changed")
        for module, name, original in _HELPERS:
            if getattr(module, name) is not original:
                raise self.failure("closed callback canonical value helper changed")
        if (type(self.meter.steps) is not int
                or (type(self.meter.budget), self.meter.budget) != (type(self.meter_budget), self.meter_budget)
                or self.meter._on_tick is not self.on_tick
                or type(self.data._pointer) is not int or not 0 <= self.data._pointer <= 64
                or self.data._pointer % 8 or type(self.returns._pointer) is not int
                or not 64 <= self.returns._pointer <= 128 or self.returns._pointer % 8):
            raise self.failure("closed callback meter or stack control changed")

    def pin_frame(self, frames, frame, root_id, prefix):
        # Pin ownership before begin performs any verification that can throw.
        self.frames, self.frame, self.root_id = frames, frame, root_id
        self.outer_contexts = tuple(item.context for item in prefix)

    def begin(self, word, root_id):
        self.require_state()
        self.capture.verify(self.engine)
        if word is not self.capture.entry.word or self.root_id != root_id:
            raise self.failure("closed callback entry changed")

    def release_frame(self, frame):
        if self.frame is not None and frame is not self.frame:
            raise self.failure("closed callback released a foreign dispatch frame")
        self.frames = None
        self.frame = None

    def tick(self):
        # Same meter, root budget and hook as ordinary tick, with an independent
        # receipt recorded before entering host code. The finally also accounts
        # for an asynchronous escape immediately after the integer publication.
        before = self.starting_steps + self.charged_ticks
        if self.meter_budget is not None and before >= self.meter_budget:
            raise StepBudgetExceeded(self.meter_budget)
        after = before + 1
        try:
            dict.__setitem__(self.meter_namespace, "steps", after)
        finally:
            current = dict.get(self.meter_namespace, "steps")
            if type(current) is int and current == after:
                self.charged_ticks = after - self.starting_steps
                if self.accounting_record is not None:
                    self.accounting_record.semantic_steps = self.charged_ticks
        self.on_tick()

    def prepare_unwind(self, error):
        expected = self.starting_steps + self.charged_ticks
        if repair_meter(self.meter, self.meter_namespace, expected):
            self.cleanup_failed(error)
        if self.engine._registration_failure is None:
            try:
                self.require_state()
                self.capture.verify(self.engine)
            except BaseException:
                self.cleanup_failed(error)
        unsafe = False
        try:
            # These are all shared routes ordinary stack cleanup can traverse.
            for cls, routes in _CLASS_ROUTES:
                if (cls.__getattribute__ is not object.__getattribute__ or hasattr(cls, "__getattr__")
                        or any(getattr(cls, name) is not value for name, value in _MAGIC_ROUTES[cls])
                        or any(vars(cls).get(name) is not value for name, value in routes)
                        or vars(cls).get("__dict__") is not _DICT_DESCRIPTORS[cls]):
                    unsafe = True
                    break
        except BaseException:
            unsafe = True
        if unsafe:
            self.engine._unwind_error = error
            self.engine._unwind_contexts = self.outer_contexts
            descriptor = dict(dict(_CLASS_ROUTES)[ExecutionContext])["_host_control_fault"]
            for context in self.outer_contexts:
                if type(context) is ExecutionContext:
                    descriptor.__set__(context, "closed callback cleanup routes changed")
            self.cleanup_failed(error)

    def cursor(self, word, ip):
        self.require_state()
        captured = self.capture.target(word.xt)
        captured.verify(self.engine._dictionary, all_operations=False)
        if (captured.word is not word or captured.operations is None or type(ip) is not int
                or not 0 <= ip < len(captured.operations)):
            raise self.failure("closed callback cursor escaped its captured body")
        return captured

    def _evidence(self):
        continuations = self.returns._continuations
        if type(continuations) is not dict or len(continuations) > 8:
            raise self.failure("closed callback continuation table changed")
        for counter in (self.returns._continuation_cookie, self.returns._pointer_capture_generation):
            if type(counter) is not int or not 0 <= counter <= MASK64:
                raise self.failure("closed callback continuation counter changed")
        entries = []
        for address, stored in continuations.items():
            if (type(address) is not int or not 64 <= address < 128 or address % 8
                    or type(stored) is not tuple or len(stored) != 2):
                raise self.failure("closed callback continuation slot changed")
            entry, raw = stored
            if type(entry) is not Continuation:
                raise self.failure("closed callback continuation type changed")
            _require_routes(entry, Continuation, dict(_CLASS_ROUTES)[Continuation])
            for value in (raw, entry.xt, entry.ip, entry.dispatch_id):
                if type(value) is not int or not 0 <= value <= MASK64:
                    raise self.failure("closed callback continuation field changed")
            if (type(entry.root) is not bool or entry.fault_abort is not None
                    or entry.xt == 0 or (entry.root and entry.dispatch_id != self.root_id)
                    or (not entry.root and entry.dispatch_id != 0)):
                raise self.failure("closed callback continuation control changed")
            entries.append((address, id(entry), raw, entry.xt, entry.ip, entry.root, entry.dispatch_id))
        return (bytes(self.data_seal.page), self.data._pointer, self.returns._pointer,
                id(continuations), tuple(entries), self.returns._continuation_cookie,
                self.returns._pointer_capture_generation)

    def before_tick(self, word, ip=None, operation=None, *, caller=None, call_ip=None):
        self.require_state()
        captured = self.capture.target(word.xt)
        captured.verify(self.engine._dictionary, all_operations=False)
        item = None
        if ip is not None:
            captured = self.cursor(word, ip)
            item = captured.evidence[ip]
            if item.operation is not operation:
                raise self.failure("closed callback operation identity changed")
            item.verify()
        elif captured.core_name is None:
            raise self.failure("closed callback primitive target changed")
        parent = None
        if caller is not None:
            parent_word = self.cursor(caller, call_ip)
            parent = parent_word.evidence[call_ip]
            parent.verify()
            if parent.kind is not Call or parent.value != captured.xt:
                raise self.failure("closed callback primitive call edge changed")
        spent = self.meter.steps - self.starting_steps
        if self.semantic_step_limit is not None and spent >= self.semantic_step_limit:
            raise self.engine._budget_error("callback_semantic_limit", self.semantic_step_limit, spent)
        if spent >= self.local_limit:
            raise self.engine._budget_error("callback_local_limit", self.local_limit, spent)
        return captured, item, parent, self._evidence(), self.meter.steps

    def after_tick(self, evidence):
        captured, item, parent, state, before = evidence
        self.require_state()
        captured.verify(self.engine._dictionary, all_operations=False)
        if item is not None:
            item.verify()
            if item.kind is Call:
                self.capture.target(item.value).verify(self.engine._dictionary)
        if parent is not None:
            parent.verify()
        if self.meter.steps != before + 1 or self._evidence() != state:
            raise self.failure("closed callback private evidence changed during accounting")
        return captured.callback

    def call_target(self, operation):
        target = self.capture.target(operation.xt)
        target.verify(self.engine._dictionary)
        return target.word

    def returned_target(self, continuation):
        self.require_state()
        if (type(continuation) is not Continuation or continuation.fault_abort is not None
                or (continuation.root and continuation.dispatch_id != self.root_id)):
            raise self.failure("closed callback continuation escaped its dispatch")
        if continuation.root:
            if self.returns.depth() != 0:
                raise self.failure("closed callback root return is unbalanced")
            self.completed = True
            return None
        target = self.capture.target(continuation.xt)
        target.verify(self.engine._dictionary)
        if target.operations is None or not 0 <= continuation.ip < len(target.operations):
            raise self.failure("closed callback return target escaped its captured body")
        return target.word

    def cleanup_failed(self, original):
        self.engine._registration_failure = "closed callback dispatch ownership could not be restored"
        if original is not None:
            try:
                BaseException.add_note(original, self.engine._registration_failure)
            except BaseException:
                pass
        else:
            raise CallbackExportError(self.engine._registration_failure)
