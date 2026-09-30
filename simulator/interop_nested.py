"""Bounded accounting and exact call authority for the nested profile.

This is an internal foundation, not an enabled callback profile. Export
capture and the native V3 bridge must both admit a transition before these
authorities can reach machine execution. Existing V2/V3 dispatch is unchanged.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
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
            _nested_accounting_authority,
        )

        if type(owner) is not HybridRuntime:
            raise CallbackExportError("nested machine owner must be this runtime's exact HybridRuntime")
        self._engine = engine
        self._composition_binding = None
        self._accounting_factory = _nested_accounting_authority
        self._engine_type = type(engine)
        self._engine_dictionary = vars(type(engine))["__dict__"]
        self._engine_namespace = self._engine_dictionary.__get__(engine, type(engine))
        self._engine_routes = tuple((name, vars(type(engine))[name]) for name in (
            "_begin_nested_chain", "_require_nested_chain", "_finish_nested_chain",
            "_begin_nested_callback", "_invoke_nested_callback", "_consume_nested_callback",
            "_nested_private_context", "_binding", "_require_binding", "_require_owner", "_require_leaf",
        ))
        self._owner = weakref.ref(owner)
        self._owner_type = HybridRuntime
        self._routes = _NESTED_OWNER_ROUTES
        self._special_routes = _NESTED_OWNER_SPECIAL_ROUTES
        self._dictionary_descriptor = _NESTED_OWNER_DICT_DESCRIPTOR
        self._field_routes = _NESTED_OWNER_FIELD_ROUTES
        self._absent = _NESTED_OWNER_ABSENT
        self.require(owner)

    def require(self, owner=None):
        if (type(self) is not NestedMachineOwner
                or any(name in vars(self) or vars(NestedMachineOwner).get(name) is not route
                       for name, route in _NESTED_ADAPTER_ROUTES)):
            raise CallbackExportError("nested composition adapter routes changed")
        if (type(self._engine) is not self._engine_type
                or self._engine_type.__getattribute__ is not object.__getattribute__
                or hasattr(self._engine_type, "__getattr__")
                or vars(self._engine_type).get("__dict__") is not self._engine_dictionary
                or self._engine_dictionary.__get__(self._engine, self._engine_type) is not self._engine_namespace
                or any(name in self._engine_namespace or vars(self._engine_type).get(name) is not route
                       for name, route in self._engine_routes)):
            raise CallbackExportError("nested semantic engine routes changed")
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
        route = next(original for name, original in self._routes if name == "_require_nested_execution")
        route(current)
        return current

    def bind_composition(self, chain, state):
        if (self._composition_binding is not None or self._engine._nested_chain is not chain
                or chain.adapter is not self or chain._composition_authority is not None):
            raise CallbackExportError("nested composition accounting already has an owner")
        owner = self._owner()
        if owner is None or owner._nested_execution is not state:
            raise CallbackExportError("nested composition requires its exact creating owner")
        authority = self._accounting_factory(owner, state)
        self._composition_binding = (chain, authority)
        chain._composition_authority = authority
        return authority

    def composition_for(self, chain, *, cleanup=False):
        binding = self._composition_binding
        if binding is None or binding[0] is not chain:
            raise CallbackExportError("nested accounting authority is not bound to this chain")
        authority = binding[1]
        if chain._composition_authority is not authority:
            chain._composition_authority = authority
            owner = self._owner()
            if owner is not None:
                owner._registration_failure = "nested composition accounting authority changed"
            if not cleanup:
                raise CallbackExportError("nested composition accounting authority changed")
        return authority

    def release_composition(self, chain):
        if self._composition_binding is not None:
            if self._composition_binding[0] is not chain:
                raise CallbackExportError("nested accounting cleanup named another chain")
            self._composition_binding = None

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
        self._composition_authority = None

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

    def finish_machine_use(self, token, *, cleanup=False):
        self._require(cleanup=cleanup)
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


@dataclass(frozen=True, slots=True)
class CapturedMachineTarget:
    """Independent registration evidence, never an executable XT permit."""

    registration: object
    word: object
    xt: int
    implementation: object
    callback: object
    declaration: object
    spec: object
    exports: tuple
    child_edges: tuple
    nested_graph: object
    image: object
    allocation_lease: object
    control_lease: object
    geometry: tuple

    @staticmethod
    def _image(declaration):
        from shared.hybrid_nested import RoutineImageV4
        return RoutineImageV4(
            name=declaration.name, code=declaration.code, entry_offset=declaration.entry_offset,
            input_cells=declaration.input_cells, output_cells=declaration.output_cells,
            buffers=tuple(replace(rule) for rule in declaration.buffers),
            return_stack_cells=declaration.return_stack_cells, max_instructions=declaration.max_instructions,
            routine_id=declaration.routine_id, max_callback_requests=declaration.max_callback_requests,
            callbacks=tuple(replace(site, export=replace(site.export)) for site in declaration.callbacks),
        )

    @staticmethod
    def _geometry(declaration):
        return tuple(getattr(declaration, name) for name in (
            "allocation_generation", "control_generation", "body_base", "body_size",
            "code_base", "stack_base", "dispatch_instruction_limit",
            "dispatch_callback_limit", "dispatch_callback_semantic_limit",
        ))

    @classmethod
    def create(cls, owner, registration):
        from shared.hybrid_nested import RoutineDeclarationV4
        declaration = registration.declaration
        if type(declaration) is not RoutineDeclarationV4:
            raise CallbackExportError("nested machine targets require explicit V4 registration")
        RoutineDeclarationV4.__post_init__(declaration)
        result = cls(registration, registration.word, registration.word.xt,
                     registration.implementation, registration.callback, declaration, registration.spec,
                     registration.exports, registration.child_edges, registration.nested_graph,
                     cls._image(declaration), declaration.allocation_lease,
                     declaration.control_lease, cls._geometry(declaration))
        result.verify_owner(owner)
        return result

    def verify_owner(self, owner):
        from shared.hybrid_nested import RoutineDeclarationV4, RoutineImageV4
        registration = self.registration
        if (type(self.xt) is not int or not 0 < self.xt <= MASK64
                or type(self.geometry) is not tuple or len(self.geometry) != 9
                or any(type(value) is not int for value in self.geometry)
                or type(self.image) is not RoutineImageV4):
            raise CallbackExportError("captured machine evidence values changed")
        RoutineImageV4.__post_init__(self.image)
        if (registration.word is not self.word or type(self.word.xt) is not int or self.word.xt != self.xt
                or registration.implementation is not self.implementation
                or registration.callback is not self.callback or registration.spec is not self.spec
                or registration.exports is not self.exports or registration.child_edges is not self.child_edges
                or registration.nested_graph is not self.nested_graph
                or registration.declaration is not self.declaration
                or type(self.declaration) is not RoutineDeclarationV4):
            raise CallbackExportError("captured machine registration identity changed")
        RoutineDeclarationV4.__post_init__(self.declaration)
        if (self.declaration.allocation_lease is not self.allocation_lease
                or self.declaration.control_lease is not self.control_lease
                or self._geometry(self.declaration) != self.geometry
                or self._image(self.declaration) != self.image):
            raise CallbackExportError("captured machine registration values changed")
        owner._validate_registration(registration, verify_exports=False)
        if owner._nested_runner is None or not owner._nested_runner.is_code_published_v3(self.spec):
            raise CallbackExportError("captured machine publication is no longer live")


@dataclass(frozen=True, slots=True)
class _ExportEvidence:
    binding: object
    descriptor: object
    leaf: object
    closed: object

    def verify(self, engine):
        binding = self.binding
        if (engine._exports.get(self.descriptor.export_id) is not binding
                or binding.handle._owner is not engine._owner
                or binding.leaf is not self.leaf or binding.closed is not self.closed
                or type(binding.descriptor) is not type(self.descriptor)
                or replace(binding.descriptor) != self.descriptor):
            raise CallbackExportError("captured machine callback binding changed")


@dataclass(frozen=True, slots=True)
class NestedGraphCapture:
    words: tuple
    machines: tuple[CapturedMachineTarget, ...]
    exports: tuple[_ExportEvidence, ...]
    policies: tuple
    routine_nodes: tuple
    export_values: tuple
    proof: object
    policy_words: tuple[tuple[int, object], ...]

    def verify(self, engine):
        from simulator.interop_closed import (
            _require_metadata_routes, _require_routes, _RUNTIME_ROUTES, _DICTIONARY_ROUTES,
        )
        from simulator.dictionary import Dictionary
        from simulator.runtime import MegaForthRuntime
        _require_metadata_routes()
        _require_routes(engine._runtime, MegaForthRuntime, _RUNTIME_ROUTES)
        _require_routes(engine._dictionary, Dictionary, _DICTIONARY_ROUTES)
        engine._require_owner("verify a nested callback graph")
        for word in self.words:
            word.verify(engine._dictionary)
        for target in self.machines:
            engine._nested_owner.call("_verify_nested_target", target)
        for export in self.exports:
            export.verify(engine)


def capture_nested_graph(engine, roots, *, root_machine=None):
    """Rebuild a bounded graph from exact live Words and issued dependencies.

    ``roots`` pairs already validated descriptors with their captured entry
    Words; no manifest ID or name retargets an existing dependency. Policy IDs
    are local proof labels, consistently rebased for the complete snapshot.
    """
    from shared.hybrid_nested import (
        CallbackExportV4, PolicyBodyV4, PolicyMachineCallV4, prove_nested_graph,
    )
    from shared.hybrid_closed import (
        PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3, PolicyBranchV3,
        PolicyBranchZeroV3, PolicyReturnV3,
    )
    from simulator.dictionary import Word
    from simulator.ir import Literal, Call, Branch, BranchZero, Return
    from simulator.runtime import ColonDefinition, PrimitiveDefinition
    from simulator.interop_closed import CapturedWord, CapturedOperation, _require_metadata_routes

    _require_metadata_routes()
    if engine._nested_owner is None:
        raise CallbackExportError("nested capture requires an exact installed machine owner")
    engine._nested_owner.require()
    words, machines, exports, evidence = {}, {}, {}, {}
    pending = []
    if type(roots) is not tuple or len(roots) > 64:
        raise CallbackExportError("nested graph roots must be a bounded exact tuple")
    for descriptor, word in roots:
        if type(descriptor) is not CallbackExportV4:
            raise CallbackExportError("nested graph requires exact V4 exports")
        checked = replace(descriptor)
        previous = exports.setdefault(descriptor.export_id, (checked, word))
        if previous[0] != checked or previous[1] is not word:
            raise CallbackExportError("nested graph root export identity conflicts")
        pending.append(word)
    total_operations = 0
    while pending:
        word = pending.pop()
        if id(word) in words or id(word) in machines:
            continue
        if type(word) is not Word or type(word.xt) is not int or not 0 < word.xt <= MASK64:
            raise CallbackExportError("nested capture requires exact live Word identities")
        try:
            live = engine._dictionary.resolve(word.xt)
        except KeyError:
            raise CallbackExportError("nested callback Word is no longer live") from None
        if live is not word:
            raise CallbackExportError("nested callback Word is no longer live")
        implementation = word.implementation
        if type(implementation) is PrimitiveDefinition:
            canonical = next(((name, leaf) for name, leaf in engine._canonical.items()
                              if leaf.word is word), None)
            if canonical is not None:
                name, leaf = canonical
                engine._require_leaf(leaf)
                words[id(word)] = CapturedWord(word, word.xt, implementation, None, (), name, leaf.callback)
                continue
            target = engine._nested_owner.call("_capture_nested_target", word)
            if type(target) is not CapturedMachineTarget or target.word is not word:
                raise CallbackExportError("nested owner returned a foreign machine capture")
            machines[id(word)] = target
            if len(machines) > 64:
                raise CallbackExportError("nested graph has more than 64 machines")
            for site, handle in target.registration.exports:
                binding = engine._exports.get(handle.export_id)
                if (binding is None or binding.handle is not handle or handle._owner is not engine._owner
                        or type(binding.descriptor) is not CallbackExportV4
                        or binding.descriptor != site.export):
                    raise CallbackExportError("nested machine callback is not the issued V4 binding")
                existing = evidence.setdefault(handle.export_id, _ExportEvidence(
                    binding, replace(binding.descriptor), binding.leaf, binding.closed,
                ))
                existing.verify(engine)
                if binding.closed is not None:
                    binding.closed.verify(engine)
                entry = binding.leaf.word if binding.closed is None else binding.closed.entry.word
                previous = exports.setdefault(handle.export_id, (replace(binding.descriptor), entry))
                if previous[0] != binding.descriptor or previous[1] is not entry:
                    raise CallbackExportError("nested graph callback export identity conflicts")
                pending.append(entry)
            continue
        if type(implementation) is not ColonDefinition or type(implementation.operations) is not tuple:
            raise CallbackExportError("nested callback target is not admitted static IR")
        operations = implementation.operations
        total_operations += len(operations)
        if not operations or total_operations > 4096:
            raise CallbackExportError("nested policy graph exceeds 4096 operations")
        captured_operations = []
        for operation in operations:
            kind = type(operation)
            field = {Literal: "value", Call: "xt", Branch: "target", BranchZero: "target", Return: None}.get(kind, "invalid")
            if field == "invalid":
                raise CallbackExportError("nested callback contains an unsupported operation")
            value = None if field is None else getattr(operation, field)
            if field is not None and (type(value) is not int or not 0 <= value <= MASK64):
                raise CallbackExportError("nested callback fields must be exact uint64 integers")
            if kind is Call:
                try:
                    pending.append(engine._dictionary.resolve(value))
                except KeyError:
                    raise CallbackExportError("nested callback static target is not live") from None
            captured_operations.append(CapturedOperation(operation, kind, field, value))
        words[id(word)] = CapturedWord(word, word.xt, implementation, operations, tuple(captured_operations))
    colons = tuple(word for word in words.values() if word.operations is not None)
    if len(colons) > 64:
        raise CallbackExportError("nested graph has more than 64 policies")
    # Preserve the first root's label when possible; every other reference is
    # rebased together, including callback descriptors in machine nodes.
    ids = {}
    first = next(((descriptor, word) for descriptor, word in roots if descriptor.policy_id is not None), None)
    if first is not None:
        ids[id(first[1])] = first[0].policy_id
    available = iter(index for index in range(64) if index not in ids.values())
    for word in colons:
        if id(word.word) not in ids:
            ids[id(word.word)] = next(available)
    by_xt = {word.xt: word for word in words.values()}
    machine_by_xt = {target.xt: target for target in machines.values()}
    normalized_exports = {}
    for identifier, (descriptor, word) in exports.items():
        normalized_exports[identifier] = (replace(descriptor, policy_id=ids[id(word)],
                                                name=f"CAPTURE-{ids[id(word)]}")
                                           if descriptor.policy_id is not None else descriptor)
    policies = []
    for word in colons:
        lowered = []
        for operation in word.evidence:
            if operation.kind is Literal:
                value = PolicyLiteralV3(value=operation.value)
            elif operation.kind is Call:
                if operation.value in machine_by_xt:
                    value = PolicyMachineCallV4(routine_id=machine_by_xt[operation.value].image.routine_id)
                else:
                    target = by_xt[operation.value]
                    value = (PolicyCoreCallV3(name=target.core_name) if target.core_name is not None
                             else PolicyCallV3(policy_id=ids[id(target.word)]))
            elif operation.kind is Branch:
                value = PolicyBranchV3(target=operation.value)
            elif operation.kind is BranchZero:
                value = PolicyBranchZeroV3(target=operation.value)
            else:
                value = PolicyReturnV3()
            lowered.append(value)
        identifier = ids[id(word.word)]
        policies.append(PolicyBodyV4(policy_id=identifier, name=f"CAPTURE-{identifier}", operations=tuple(lowered)))
    routine_nodes = []
    for node in (*(target.image.graph_node() for target in machines.values()),
                 *((root_machine,) if root_machine is not None else ())):
        routine_nodes.append(replace(node, callbacks=tuple(
            replace(site, export=normalized_exports[site.export.export_id]) for site in node.callbacks
        )))
    try:
        proof = prove_nested_graph(policies=tuple(policies), routines=tuple(routine_nodes),
                                   exports=tuple(normalized_exports.values()),
                                   dispatch_callback_limit=engine._nested_owner.require().dispatch_callback_limit)
    except (TypeError, ValueError) as error:
        raise CallbackExportError(f"nested callback graph rejected: {error}") from error
    result = NestedGraphCapture(tuple(words.values()), tuple(machines.values()), tuple(evidence.values()),
                                tuple(policies), tuple(routine_nodes), tuple(normalized_exports.values()), proof,
                                tuple((ids[id(word.word)], word) for word in colons))
    result.verify(engine)
    return result


@dataclass(frozen=True, slots=True)
class NestedCapture:
    entry: object
    words: tuple
    machines: tuple
    proof: object
    graph: NestedGraphCapture
    calls: tuple[CapturedMachineCall, ...]

    @classmethod
    def create(cls, engine, descriptor, handle):
        from simulator.runtime import ColonDefinition
        entry = engine._dictionary.find(descriptor.name)
        if entry is None or type(entry.implementation) is not ColonDefinition:
            raise CallbackExportError("nested callback entry must name a live colon definition")
        graph = capture_nested_graph(engine, ((descriptor, entry),))
        proof = next(item for item in graph.proof.policy_proofs if item.policy_id == descriptor.policy_id)
        policy_words = dict(graph.policy_words)
        machines = {target.image.routine_id: target for target in graph.machines}
        calls = []
        before = frozenset(engine._nested_calls)
        try:
            for call in proof.child_calls:
                word = policy_words[call.policy_id]
                calls.append(capture_machine_call(engine, handle, word.word,
                                                  word.operations[call.operation_index], call.operation_index,
                                                  machines[call.routine_id]))
        except BaseException:
            for token in tuple(engine._nested_calls):
                if token not in before:
                    del engine._nested_calls[token]
            raise
        return cls(next(word for word in graph.words if word.word is entry),
                   tuple(word for word in graph.words if word.core_name in proof.core_names
                         or (word.operations is not None and any(word is policy_words[item] for item in proof.policy_ids))),
                   tuple(machines[item] for item in proof.routine_ids), proof, graph, tuple(calls))

    def verify(self, engine):
        self.graph.verify(engine)
        for token in self.calls:
            captured = engine._nested_calls.get(token)
            if captured is None:
                raise CallbackExportError("nested callback Call authority was revoked")
            captured.verify(engine)

    def target(self, xt):
        value = next((word for word in self.words if word.xt == xt), None)
        if value is None:
            value = next((target for target in self.machines if target.xt == xt), None)
        if value is None:
            raise CallbackExportError("nested callback escaped its captured targets")
        return value


@dataclass(frozen=True, slots=True)
class _NestedLeafCapture:
    entry: object
    leaf: object

    def verify(self, engine):
        engine._require_leaf(self.leaf)
        self.entry.verify(engine._dictionary)

    def target(self, xt):
        if type(xt) is not int or xt != self.entry.xt:
            raise CallbackExportError("nested leaf escaped its exact captured target")
        return self.entry


# Loaded after interop_exports has installed its types. This reuses the same
# private-stack/IR/continuation checks and the same runtime dispatcher as V3.
from simulator.interop_closed import ClosedDispatch, CapturedWord


class NestedDispatch(ClosedDispatch):
    def __init__(self, engine, binding, context, chain, checkpoint):
        super().__init__(engine, binding, context, chain.meter, None)
        self.chain, self.checkpoint = chain, checkpoint
        self.parked_evidence = None
        self.dispatch_stack = engine._nested_dispatches
        self.dispatch_prefix = tuple(self.dispatch_stack)
        if binding.closed is None:
            leaf = binding.leaf
            self.capture = _NestedLeafCapture(
                CapturedWord(leaf.word, leaf.word.xt, leaf.implementation, None, (),
                             binding.descriptor.name, leaf.callback), leaf,
            )

    def require_state(self, *, parked=False):
        # Check methods before reading guard-owned state or entering inherited
        # checks. A host accounting hook cannot replace the invoked adapter.
        if (type(self) is not NestedDispatch or type(self).__getattribute__ is not object.__getattribute__
                or any(name in vars(self) or vars(NestedDispatch).get(name) is not route
                       for name, route in _NESTED_DISPATCH_ROUTES)
                or any(vars(ClosedDispatch).get(name) is not route for name, route in _BASE_DISPATCH_ROUTES)
                or any(name in vars(NestedDispatch) for name in _NESTED_INHERITED_ROUTES)
                or any(name in vars(NestedDispatch) or name in vars(ClosedDispatch)
                       for name in _NESTED_GUARD_FIELDS)):
            raise CallbackExportError("nested private dispatch routes changed")
        self.chain.adapter.require()
        ClosedDispatch.require_state(self, parked=parked)
        dispatches = self.engine._nested_dispatches
        position = len(self.dispatch_prefix)
        if (dispatches is not self.dispatch_stack or type(dispatches) is not list
                or len(dispatches) <= position or dispatches[position] is not self
                or any(item is not dispatches[index] for index, item in enumerate(self.dispatch_prefix))
                or (not parked and len(dispatches) != position + 1)):
            raise self.failure("nested private dispatch is not the issued current context")
        self.chain._require_meter()
        if parked and (self.parked_evidence is None or self._evidence() != self.parked_evidence):
            raise self.failure("parked callback private state changed during child execution")
        if not parked:
            for parent in self.dispatch_prefix:
                if type(parent) is not NestedDispatch:
                    raise self.failure("nested ancestor dispatch identity changed")
                NestedDispatch.require_state(parent, parked=True)

    def _verify_target(self, captured):
        if type(captured) is CapturedMachineTarget:
            self.engine._nested_owner.call("_verify_nested_target", captured)
        else:
            captured.verify(self.engine._dictionary, all_operations=False)

    def before_tick(self, word, ip=None, operation=None, *, caller=None, call_ip=None):
        from simulator.ir import Call
        self.require_state()
        captured = self.capture.target(word.xt)
        self._verify_target(captured)
        item = None
        if ip is not None:
            captured = self.cursor(word, ip)
            item = captured.evidence[ip]
            if item.operation is not operation:
                raise self.failure("nested callback operation identity changed")
            item.verify()
        elif type(captured) is not CapturedMachineTarget and captured.core_name is None:
            raise self.failure("nested callback primitive target changed")
        parent = None
        if caller is not None:
            parent = self.cursor(caller, call_ip).evidence[call_ip]
            parent.verify()
            if parent.kind is not Call or parent.value != captured.xt:
                raise self.failure("nested callback primitive call edge changed")
        return captured, item, parent, self._evidence(), self.chain.steps

    def tick(self):
        self.chain.tick(self.checkpoint)
        self.charged_ticks = self.chain._inclusive_steps(self.checkpoint)
        _NESTED_REQUIRE_STATE(self)

    def after_tick(self, evidence):
        from simulator.ir import Call
        captured, item, parent, state, before = evidence
        self.require_state()
        self._verify_target(captured)
        if item is not None:
            item.verify()
            if item.kind is Call:
                self._verify_target(self.capture.target(item.value))
        if parent is not None:
            parent.verify()
        if self.chain.steps != before + 1 or self._evidence() != state:
            raise self.failure("nested private evidence changed during accounting")
        return captured.callback

    def call_target(self, operation):
        target = self.capture.target(operation.xt)
        self._verify_target(target)
        return target.word

    def invoke_primitive(self, target, callback, context, *, caller=None, call_ip=None):
        from simulator.interop_exports import _cells
        self.require_state()
        captured = self.capture.target(target.xt)
        if type(captured) is not CapturedMachineTarget:
            return callback(context)
        if caller is None or context is not self.context:
            raise self.failure("machine child requires a captured static Call cursor")
        matches = [token for token in self.capture.calls
                   if (self.engine._nested_calls[token].word is caller
                       and self.engine._nested_calls[token].operation_index == call_ip
                       and self.engine._nested_calls[token].target is captured)]
        if len(matches) != 1:
            raise self.failure("machine child Call has no exact captured site")
        declaration = captured.declaration
        arguments = tuple(self.data.peek(index) for index in reversed(range(declaration.input_cells)))
        self.data.require_push_capacity(max(0, declaration.output_cells - declaration.input_cells))
        edge = self.engine._nested_owner.call("_nested_child_edge", self.checkpoint, matches[0])
        use = self.chain.issue_machine_use(self.checkpoint, matches[0], edge)
        self.parked_evidence = self._evidence()
        try:
            outputs = self.engine._nested_owner.call("_invoke_nested_child", use, arguments)
            self.require_state()
            if self._evidence() != self.parked_evidence:
                raise self.failure("machine child changed its caller's private stack")
            _cells(outputs, count=declaration.output_cells, label="child outputs")
            self.chain.finish_machine_use(use)
        except BaseException as error:
            try:
                self.chain.finish_machine_use(use, cleanup=True)
            except BaseException:
                _BASE_CLEANUP_FAILED(self, error)
            raise
        finally:
            self.parked_evidence = None
        for _ in range(declaration.input_cells):
            self.data.pop()
        for cell in outputs:
            self.data.push(cell)
        return None

    def prepare_unwind(self, error):
        self.charged_ticks = self.chain._inclusive_steps(self.checkpoint)
        _BASE_PREPARE_UNWIND(self, error)


_NESTED_REQUIRE_STATE = NestedDispatch.require_state
_BASE_CLEANUP_FAILED = ClosedDispatch.cleanup_failed
_BASE_PREPARE_UNWIND = ClosedDispatch.prepare_unwind
_BASE_DISPATCH_ROUTES = tuple((name, value) for name, value in vars(ClosedDispatch).items()
                              if callable(value))
_NESTED_DISPATCH_ROUTES = tuple((name, value) for name, value in vars(NestedDispatch).items()
                                if callable(value))

_NESTED_INHERITED_ROUTES = tuple(name for name, _ in _BASE_DISPATCH_ROUTES
                                 if name not in vars(NestedDispatch))
_NESTED_GUARD_FIELDS = (
    "engine", "binding", "context", "meter", "capture", "data", "returns", "data_seal", "return_seal",
    "starting_steps", "local_limit", "semantic_step_limit", "meter_budget", "on_tick", "dictionary_guard",
    "private_regions", "region_spec", "root_id", "frames", "frame", "completed", "outer_contexts",
    "charged_ticks", "meter_namespace", "accounting_record", "chain", "checkpoint", "parked_evidence",
    "dispatch_stack", "dispatch_prefix",
)

_NESTED_ADAPTER_ROUTES = tuple((name, value) for name, value in vars(NestedMachineOwner).items()
                               if callable(value))
