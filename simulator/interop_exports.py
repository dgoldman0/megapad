"""Owner-bound semantic admission for versioned private callbacks.

Shared descriptors contain only values. This engine retains the original
installed Words and dispatches them through the runtime's ordinary meter;
neither a name, an XT nor a copied handle grants invocation authority.
"""

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, field, replace
from typing import Iterator

from shared.cells import CELL_BYTES, MASK64
from shared.hybrid_abi import (
    CallbackExportV2, CallbackExportV3, MAX_CALLBACK_EXPORTS, MAX_SIGNATURE_CELLS,
    MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
)
from shared.hybrid_nested import CallbackExportV4
from shared.hybrid_services import CallbackRequestV5, CallbackSiteV5, ServiceExportV5
from simulator import core_words
from simulator.dictionary import Word
from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace
from simulator.runtime import ExecutionContext, PrimitiveDefinition
from simulator.stacks import DataStack, ReturnStack


_CANONICAL_CALLBACKS = {
    "MIN": core_words._minimum,
    "MAX": core_words._maximum,
    "ABS": core_words._absolute,
    "AND": core_words._and,
    "OR": core_words._or,
    "XOR": core_words._xor,
    "DUP": core_words._dup,
    "DROP": core_words._drop,
    "SWAP": core_words._swap,
    "OVER": core_words._over,
    "ROT": core_words._rotate,
}


class CallbackExportError(ExecutionError):
    """An export is unadmitted, stale or violates its bounded leaf contract."""


class CallbackExportBudgetExceeded(CallbackExportError):
    """An engine-owned callback allowance was exhausted before another tick."""

    def __init__(self, reason, limit, semantic_steps):
        self.reason, self.limit, self.semantic_steps = reason, limit, semantic_steps
        super().__init__(f"{reason}: callback allowance {limit} exhausted after {semantic_steps} steps")


@dataclass(frozen=True, slots=True, eq=False)
class CallbackExportHandle:
    """Opaque issued identity; its numerical ID alone is not authority."""

    export_id: int
    _owner: object = field(repr=False)


@dataclass(frozen=True, slots=True)
class CallbackExportResult:
    outputs: tuple[int, ...]
    semantic_steps: int


@dataclass(frozen=True, slots=True)
class ClosedCallbackReceipt:
    semantic_steps: int
    entered: bool
    completed: bool


@dataclass(frozen=True, slots=True, eq=False)
class ServiceCallbackFailureV5:
    """Exact engine-issued failure; copies are diagnostics, not authority."""

    export_id: int
    name: str
    invocation_id: int
    sequence: int
    call_offset: int
    stub_offset: int
    operation: int
    consumed_input_cells: int
    fpcsr: int
    semantic_steps: int
    cause: BaseException = field(repr=False)
    fault_kind: str = "illegal_scalar_float"
    throw_code: int = -21


@dataclass(frozen=True, slots=True)
class ServiceCallbackReceiptV5:
    semantic_steps: int
    entered: bool
    completed: bool
    consumed_input_cells: int | None
    failure: ServiceCallbackFailureV5 | None


@dataclass(frozen=True, slots=True)
class ServiceCallbackProfileV5:
    """Qualified semantic service metadata, without native bridge authority."""

    value_executor: str
    version: int = 5
    capability: str = "private_scalar_fp_v1"
    effect: str = "scalar_fp_state"


def _service_value_layout(kind, names):
    namespace = type.__getattribute__(kind, "__dict__")
    return (kind, tuple(namespace[name] for name in names),
            tuple((name, value) for name, value in namespace.items() if name != "__slotnames__"),
            namespace["__slots__"], type.__getattribute__(kind, "__mro__"))


_SERVICE_VALUE_NEW = object.__new__
_SERVICE_RESULT_LAYOUT = _service_value_layout(CallbackExportResult, ("outputs", "semantic_steps"))
_SERVICE_RECEIPT_LAYOUT = _service_value_layout(ServiceCallbackReceiptV5, (
    "semantic_steps", "entered", "completed", "consumed_input_cells", "failure",
))
_SERVICE_FAILURE_LAYOUT = _service_value_layout(ServiceCallbackFailureV5, (
    "export_id", "name", "invocation_id", "sequence", "call_offset", "stub_offset",
    "operation", "consumed_input_cells", "fpcsr", "semantic_steps", "cause", "fault_kind", "throw_code",
))
_SERVICE_PROFILE_LAYOUT = _service_value_layout(ServiceCallbackProfileV5, (
    "value_executor", "version", "capability", "effect",
))


def _service_value_routes_match(layout):
    kind, _fields, entries, slots, mro = layout
    namespace = type.__getattribute__(kind, "__dict__")
    # Scan first: an unrelated non-exact key must not run equality while a
    # route is checked. copyreg's normal derived cache is harmless metadata.
    if len(namespace) > 4096 or any(type(key) is not str for key in namespace):
        return False
    cache = namespace.get("__slotnames__")
    cached = "__slotnames__" in namespace
    if cached and (type(cache) is not list or len(cache) != len(slots)
                   or any(type(name) is not str for name in cache) or tuple(cache) != slots):
        return False
    return (type.__getattribute__(kind, "__mro__") is mro
            and len(namespace) == len(entries) + cached
            and all(namespace.get(name) is value for name, value in entries))


def _issue_service_value(layout, values):
    # Do not dispatch a potentially replaced __new__, __init__, __setattr__,
    # or public field descriptor while publishing authoritative work. Retained
    # member descriptors write the original storage even after a route change.
    kind, fields, _entries, _slots, _mro = layout
    result = _SERVICE_VALUE_NEW(kind)
    for field, value in zip(fields, values):
        field.__set__(result, value)
    return result


@dataclass(frozen=True, slots=True, eq=False)
class _ServiceCheckpoint:
    _owner: object
    _record: object


@dataclass(slots=True)
class _ClosedAccounting:
    token: object
    handle: CallbackExportHandle
    meter: object
    namespace: object = None
    starting_steps: int = 0
    semantic_steps: int = 0
    entered: bool = False
    completed: bool = False
    admitting: bool = False


@dataclass(frozen=True, slots=True)
class _CanonicalLeaf:
    word: Word
    implementation: PrimitiveDefinition
    callback: object


@dataclass(frozen=True, slots=True)
class _ExportBinding:
    handle: CallbackExportHandle
    descriptor: CallbackExportV2 | CallbackExportV3 | CallbackExportV4 | ServiceExportV5
    leaf: _CanonicalLeaf | None
    closed: object = None
    service: object = None


@dataclass(slots=True)
class _ExportRegistration:
    table: dict[int, _ExportBinding]
    previous: dict[int, _ExportBinding]
    issued: list[tuple[int, _ExportBinding]] = field(default_factory=list)


@dataclass(slots=True)
class _ActiveExport:
    binding: _ExportBinding
    context: ExecutionContext
    data: DataStack
    returns: ReturnStack
    arguments: tuple[int, ...]
    data_seal: _PrivateStackSeal
    return_seal: _PrivateStackSeal
    dispatched: bool = False


@dataclass(frozen=True, slots=True)
class _PrivateStackSeal:
    stack: object
    stack_type: type
    memory: object
    view: object
    region: object
    pages: dict
    page: bytearray
    bounds: tuple[int, int, int, int]

    @classmethod
    def capture(cls, stack):
        view = stack._memory_view
        region = view._region
        return cls(
            stack, type(stack), stack._memory, view, region, region.pages,
            region.pages[0],
            (stack._floor, stack._empty_pointer, view._base, view._offset),
        )

    def matches(self) -> bool:
        stack = self.stack
        return (
            type(stack) is self.stack_type
            and stack._task_effect_guard is None
            and (self.stack_type is not ReturnStack or stack._foreign_control is None)
            and stack._memory is self.memory and stack._memory_view is self.view
            and self.view._region is self.region and self.region.pages is self.pages
            and self.pages.get(0) is self.page
            and (stack._floor, stack._empty_pointer,
                 self.view._base, self.view._offset) == self.bounds
        )


def _descriptor(value):
    if type(value) not in (CallbackExportV2, CallbackExportV3, CallbackExportV4, ServiceExportV5):
        raise TypeError("callback descriptor must be an exact admitted callback value")
    if type(value) is ServiceExportV5:
        from simulator.interop_services import _EXPORT_CLASS
        _EXPORT_CLASS.instance(value)
    # Revalidate even a forged frozen value and keep our own metadata copy.
    return replace(value)


def _cells(value: tuple[int, ...], *, count: int, label: str) -> None:
    if type(value) is not tuple:
        raise TypeError(f"callback {label} must be an exact tuple")
    if len(value) != count or len(value) > MAX_SIGNATURE_CELLS:
        raise CallbackExportError(f"callback {label} do not match the declared arity")
    if any(type(cell) is not int or not 0 <= cell <= MASK64 for cell in value):
        raise TypeError(f"callback {label} must contain exact uint64 cells")


class CallbackExportEngine:
    """One runtime's bounded table, captured at its core-install boundary."""

    def __init__(self, runtime, *, core_installed: bool) -> None:
        self._runtime = runtime
        self._dictionary = runtime.dictionary
        self._owner = object()
        self._canonical: dict[str, _CanonicalLeaf] = {}
        self._exports: dict[int, _ExportBinding] = {}
        self._active: _ActiveExport | None = None
        self._registration: _ExportRegistration | None = None
        self._registration_failure: str | None = None
        self._budget_failure: CallbackExportBudgetExceeded | None = None
        self._closed_accounting = None
        # The nested profile has its own bounded chain ledger. Installation
        # supplies no execution capability; the native/composition seam must
        # still explicitly admit V4 before any export can use it.
        self._nested_owner = None
        self._nested_chain = None
        self._nested_calls = {}
        self._nested_dispatches = []
        self._unwind_error = None
        self._unwind_contexts = ()
        runtime._closed_cleanup_guard = self._guard_outer_unwind
        runtime._closed_accounting_guard = self._guard_accounting_entry
        self._accounting_routes = {
            "begin": ("begin_closed_accounting", type(self).begin_closed_accounting),
            "consume": ("consume_closed_accounting", type(self).consume_closed_accounting),
        }
        runtime._closed_accounting_dispatch = self._accounting_call
        if core_installed:
            for name, callback in _CANONICAL_CALLBACKS.items():
                word = runtime.dictionary.find(name)
                if (word is not None
                        and type(word.implementation) is PrimitiveDefinition
                        and word.implementation.callback is callback):
                    self._canonical[name] = _CanonicalLeaf(
                        word, word.implementation, callback
                    )
        # Capture canonical dispatch/memory routes at the core-install boundary,
        # before a caller can customize this runtime or its private stack path.
        from simulator.interop_closed import ClosedCapture, ClosedDispatch

        self._closed_capture_type = ClosedCapture
        self._closed_dispatch_type = ClosedDispatch
        from simulator.interop_services import ScalarServiceCatalog, ServiceDispatch, _ServiceAccounting

        self._service_catalog = None
        self._service_dispatch_type = ServiceDispatch
        self._service_accounting_type = _ServiceAccounting
        self._service_finalized = False
        self._service_unavailable = "private scalar services were not captured"
        # Private-profile qualification must not reject a valid old embedding.
        # Only our own canonical-admission rejection disables this profile;
        # allocation failures and unexpected host exceptions remain visible.
        if type(core_installed) is bool and core_installed:
            try:
                self._service_catalog = ScalarServiceCatalog.capture(runtime, core_installed=True)
            except CallbackExportError:
                self._service_unavailable = "private scalar services require canonical installed owners"
        self._accounting_routes.update({
            "begin_service": ("begin_service_accounting", type(self).begin_service_accounting),
            "consume_service": ("consume_service_accounting", type(self).consume_service_accounting),
        })

    def finalize_service_executor(self, executor):
        """One constructor seam after the runtime selects its value backend."""
        if self._service_finalized:
            raise CallbackExportError("private scalar service executor was already finalized")
        self._service_finalized = True
        if self._service_catalog is None:
            return
        try:
            self._service_catalog.finalize_executor(executor)
        except CallbackExportError:
            self._service_catalog = None
            self._service_unavailable = "private scalar value executor is not canonical"

    def _guard_accounting_entry(self, operation):
        if self._nested_chain is not None:
            raise CallbackExportError(f"cannot {operation} during nested callback accounting")
        record = self._closed_accounting
        if record is not None and not record.admitting:
            raise CallbackExportError(f"cannot {operation} during closed callback accounting")

    def _accounting_call(self, operation, *arguments):
        name, callback = self._accounting_routes[operation]
        attributes = object.__getattribute__(self, "__dict__")
        if name in attributes or vars(type(self)).get(name) is not callback:
            raise CallbackExportError("closed callback accounting route changed")
        return callback(self, *arguments)

    def _install_nested_owner(self, owner):
        """Bind the exact creating HybridRuntime, never a generic callable."""
        with self._runtime._session_owner_lock:
            self._require_owner("install the nested machine owner")
            self._runtime._require_no_suspension("install the nested machine owner")
            if (self._nested_owner is not None or self._active is not None
                    or self._registration is not None or self._closed_accounting is not None
                    or self._runtime._active_dispatches or self._runtime._active_input_states):
                raise CallbackExportError("nested machine owner installation requires one fresh idle owner")
            from simulator.interop_nested import NestedMachineOwner
            self._nested_owner = NestedMachineOwner(self, owner)

    def _begin_nested_chain(self, owner, meter, semantic_limit):
        with self._runtime._session_owner_lock:
            self._require_owner("begin nested callback accounting")
            if (self._nested_owner is None or self._nested_chain is not None
                    or self._active is not None or self._registration is not None
                    or self._closed_accounting is not None):
                raise CallbackExportError("nested callback accounting requires its idle installed owner")
            states, frames = self._runtime._active_input_states, self._runtime._active_dispatches
            active_meter = states[0].meter if states else (frames[0].meter if frames else None)
            if active_meter is None or meter is not active_meter:
                raise CallbackExportError("nested callback accounting requires the original outer meter")
            from simulator.interop_nested import NestedChainAccounting
            chain = NestedChainAccounting(self, self._nested_owner, owner, meter, semantic_limit)
            self._nested_chain = chain
            return chain.token

    def _require_nested_chain(self, token):
        chain = self._nested_chain
        if chain is None or chain.token is not token:
            raise CallbackExportError("nested callback chain token is not issued here")
        return chain

    def _finish_nested_chain(self, token, *, cancelled=False):
        with self._runtime._session_owner_lock:
            # The exact issued cleanup path remains usable after fail-closing.
            chain = self._require_nested_chain(token)
            receipt = chain.finish(cancelled=cancelled)
            self._nested_chain = None
            return receipt

    def _begin_nested_callback(self, chain_token, handle, invocation_id, arguments):
        chain = self._require_nested_chain(chain_token)
        binding = self._binding(handle)
        if type(binding.descriptor) is not CallbackExportV4:
            raise CallbackExportError("nested dispatch requires an exact V4 export")
        _cells(arguments, count=binding.descriptor.input_cells, label="arguments")
        via_use = self._nested_owner.call("_admit_nested_callback", handle, invocation_id, arguments, True)
        self._budget_failure = None
        return chain.begin_callback(handle, invocation_id, binding.descriptor.max_semantic_steps,
                                    via_use=via_use)

    def _invoke_nested_callback(self, checkpoint, handle, arguments):
        from simulator.interop_nested import NestedDispatch
        chain = self._nested_chain
        if chain is None:
            raise CallbackExportError("nested callback requires its issued chain")
        record = chain._record(checkpoint)
        binding = self._binding(handle)
        self._nested_owner.call("_admit_nested_callback", handle, record.invocation_id, arguments)
        _cells(arguments, count=binding.descriptor.input_cells, label="arguments")
        chain.enter_callback(checkpoint, handle)
        previous = self._active
        dispatches = self._nested_dispatches
        prefix = tuple(dispatches)
        namespace = object.__getattribute__(self, "__dict__")
        active = None
        prepare_unwind = cleanup_failed = None
        completed = False
        original = None
        try:
            memory = SparseAddressSpace(bank0_size=128, page_size=128)
            memory.write8(0, 0)
            context = ExecutionContext(
                data=DataStack(arguments, memory=memory, floor=0, empty_pointer=64),
                returns=ReturnStack(memory=memory, floor=64, empty_pointer=128),
            )
            active = NestedDispatch(self, binding, context, chain, checkpoint)
            prepare_unwind, cleanup_failed = active.prepare_unwind, active.cleanup_failed
            self._nested_dispatches.append(active)
            self._active = active
            active.require_state()
            self._runtime._execute_guarded(active.capture.entry.word, context, chain.meter,
                                           closed_guard=active)
            active.require_state()
            active.capture.verify(self)
            if not active.completed or context.returns.depth() != 0:
                raise CallbackExportError("nested callback did not return balanced private state")
            outputs = context.data.snapshot()
            _cells(outputs, count=binding.descriptor.output_cells, label="outputs")
            result = CallbackExportResult(outputs, chain._inclusive_steps(checkpoint))
            completed = True
            return result
        except BaseException as error:
            original = error
            if active is not None:
                try:
                    prepare_unwind(error)
                except BaseException:
                    cleanup_failed(error)
            raise
        finally:
            try:
                clean = (dict.get(namespace, "_nested_dispatches") is dispatches
                         and len(dispatches) == len(prefix) + (active is not None)
                         and all(item is dispatches[index] for index, item in enumerate(prefix))
                         and (active is None or dispatches[-1] is active))
                list.__setitem__(dispatches, slice(None), prefix)
                dict.__setitem__(namespace, "_nested_dispatches", dispatches)
                dict.__setitem__(namespace, "_active", previous)
                chain.leave_callback(checkpoint, completed=completed)
                if not clean:
                    raise CallbackExportError("nested callback dispatch ownership changed")
            except BaseException as cleanup:
                self._registration_failure = "nested callback dispatch cleanup could not be proved"
                if original is None:
                    raise
                try:
                    BaseException.add_note(original, self._registration_failure)
                except BaseException:
                    pass

    def _consume_nested_callback(self, chain_token, checkpoint):
        return self._require_nested_chain(chain_token).consume_callback(checkpoint)

    def _nested_private_context(self, context):
        for active in self._nested_dispatches:
            if active.context is context:
                active.require_state(parked=active is not self._active)
                return True
        return False

    def begin_closed_accounting(self, handle):
        with self._runtime._session_owner_lock:
            self._require_owner("begin closed callback accounting")
            if (self._active is not None or self._registration is not None
                    or self._closed_accounting is not None or self._nested_chain is not None):
                raise CallbackExportError("closed callback accounting already has an active owner")
            binding = self._binding(handle)
            if type(binding.descriptor) is CallbackExportV4:
                raise CallbackExportError("V4 callbacks require their admitted nested chain")
            if binding.closed is None:
                raise CallbackExportError("closed callback accounting requires a closed export")
            frames = self._runtime._active_dispatches
            states = self._runtime._active_input_states
            meter = frames[-1].meter if frames else (states[-1].meter if states else None)
            token = object()
            record = _ClosedAccounting(token, handle, meter)
            if meter is not None:
                from simulator.interop_closed import capture_meter
                record.namespace, record.starting_steps = capture_meter(meter)
            self._closed_accounting = record
            return token

    def consume_closed_accounting(self, token, handle):
        with self._runtime._session_owner_lock:
            record = self._closed_accounting
            # Cleanup proof deliberately remains available after fail-closing.
            if (type(record) is not _ClosedAccounting or record.token is not token or record.handle is not handle
                    or self._runtime._callback_exports is not self or self._active is not None):
                raise CallbackExportError("closed callback accounting checkpoint is not issued here")
            if (type(record.semantic_steps) is not int or not 0 <= record.semantic_steps <= 4096
                    or type(record.entered) is not bool or type(record.completed) is not bool):
                raise CallbackExportError("closed callback accounting receipt changed")
            if record.meter is not None:
                from simulator.interop_closed import repair_meter
                if repair_meter(record.meter, record.namespace, record.starting_steps + record.semantic_steps):
                    self._registration_failure = "closed callback accounting meter changed"
            result = ClosedCallbackReceipt(record.semantic_steps, record.entered, record.completed)
            self._closed_accounting = None
            return result

    def begin_service_accounting(self, handle, request):
        from simulator.interop_services import ServiceDispatch, _ServiceAccounting

        with self._runtime._session_owner_lock:
            self._require_owner("begin private scalar service accounting")
            self._runtime._require_no_suspension("begin private scalar service accounting")
            if (self._service_accounting_type is not _ServiceAccounting
                    or self._service_dispatch_type is not ServiceDispatch):
                self._registration_failure = "private service type projections changed"
                raise CallbackExportError(self._registration_failure)
            if (self._active is not None or self._registration is not None
                    or self._closed_accounting is not None or self._nested_chain is not None):
                raise CallbackExportError("private service accounting requires an idle callback owner")
            binding = self._binding(handle)
            if binding.service is None or type(request) is not CallbackRequestV5:
                raise CallbackExportError("private service accounting requires its V5 binding and request")
            from simulator.interop_services import _SERVICE_REQUEST_CLASS, _SERVICE_SITE_CLASS, _EXPORT_CLASS
            _SERVICE_REQUEST_CLASS.instance(request)
            _SERVICE_SITE_CLASS.instance(request.site)
            _EXPORT_CLASS.instance(request.site.export)
            CallbackRequestV5.__post_init__(request)
            if request.site.export != binding.descriptor:
                raise CallbackExportError("private service request does not match its issued export")
            copied = CallbackRequestV5(
                invocation_id=request.invocation_id, sequence=request.sequence,
                site=CallbackSiteV5(call_offset=request.site.call_offset,
                                    stub_offset=request.site.stub_offset,
                                    export=replace(binding.descriptor)),
                arguments=request.arguments,
            )
            frames, states = self._runtime._active_dispatches, self._runtime._active_input_states
            meter = frames[-1].meter if frames else (states[-1].meter if states else None)
            record = _ServiceAccounting(token=None, handle=handle, binding=binding,
                                        request=copied, meter=meter)
            if meter is not None:
                from simulator.interop_closed import capture_meter
                record.namespace, record.starting_steps = capture_meter(meter)
            checkpoint = _ServiceCheckpoint(self._owner, record)
            record.token = checkpoint
            self._closed_accounting = record
            return checkpoint

    def consume_service_accounting(self, checkpoint, handle, error=None):
        from simulator.interop_services import ServiceDispatch, _ServiceAccounting

        with self._runtime._session_owner_lock:
            record = self._closed_accounting
            # Exact checkpoint cleanup remains available after fail-closing.
            if (type(checkpoint) is not _ServiceCheckpoint or checkpoint._owner is not self._owner
                    or type(record) is not _ServiceAccounting
                    or checkpoint._record is not record or record.token is not checkpoint
                    or record.handle is not handle or self._active is not None
                    or self._runtime._callback_exports is not self):
                raise CallbackExportError("private service checkpoint was not issued here or was consumed")
            steps, entered, completed = record.semantic_steps, record.entered, record.completed
            consumed = record.consumed_input_cells
            if (type(steps) is not int or not 0 <= steps <= 1
                    or type(entered) is not bool or type(completed) is not bool
                    or (not entered and (steps or completed)) or (completed and steps != 1)
                    or (consumed is not None and (type(consumed) is not int
                        or not 0 <= consumed <= record.binding.descriptor.input_cells))):
                self._registration_failure = "private service accounting receipt changed"
                raise CallbackExportError(self._registration_failure)
            if record.meter is not None:
                from simulator.interop_closed import repair_meter
                if repair_meter(record.meter, record.namespace, record.starting_steps + steps):
                    self._registration_failure = "private service accounting meter changed"
            if not all(_service_value_routes_match(layout) for layout in (
                    _SERVICE_RESULT_LAYOUT, _SERVICE_RECEIPT_LAYOUT, _SERVICE_FAILURE_LAYOUT)):
                self._registration_failure = "private service issued value routes changed"
            failure = None
            observation, scope, active = record.validation_failure, record.scope, record.dispatch
            if observation is not None and self._registration_failure is None:
                from simulator.interop_services import (
                    ScalarValidationFailure, _SERVICE_VALIDATION_CLASS, _SERVICE_SCOPE_CLASS,
                    _SERVICE_DISPATCH_CLASS, _CLOSED_DISPATCH_CLASS, _SERVICE_PRIVATE_CLASSES,
                    _SERVICE_REQUEST_CLASS, _SERVICE_SITE_CLASS, _EXPORT_CLASS,
                    _service_request_values,
                )
                _SERVICE_VALIDATION_CLASS.instance(observation)
                _SERVICE_SCOPE_CLASS.instance(scope)
                _CLOSED_DISPATCH_CLASS.verify()
                _SERVICE_DISPATCH_CLASS.instance(active)
                for seal in _SERVICE_PRIVATE_CLASSES:
                    seal.verify()
                binding = record.binding
                binding.service.verify()
                _SERVICE_REQUEST_CLASS.instance(record.request)
                _SERVICE_SITE_CLASS.instance(record.request.site)
                _EXPORT_CLASS.instance(record.request.site.export)
                CallbackRequestV5.__post_init__(record.request)
                request_values = _service_request_values(record.request)
                next(item for item in _SERVICE_PRIVATE_CLASSES if item.cls is ExecutionContext).instance(active.context)
                # The observation already proved the exact popped prefix.
                # Recheck final empty private pointers without invoking stack
                # methods that a caller could replace after the escape.
                private = {}
                for kind, value in ((DataStack, active.data), (ReturnStack, active.returns)):
                    seal = next(item for item in _SERVICE_PRIVATE_CLASSES if item.cls is kind)
                    private[kind] = seal.instance(value)
                if (type(observation) is not ScalarValidationFailure
                        or type(active) is not ServiceDispatch
                        or active.accounting_record is not record or active._scope is not scope
                        or scope is None or scope.failure is not observation
                        or scope._capture is not binding.service or scope._boundary is not None
                        or record.error is not error or observation.cause is not error or error is None
                        or steps != 1 or not entered or completed
                        or consumed != binding.descriptor.input_cells
                        or type(observation.operation) is not int or type(observation.fpcsr) is not int
                        or observation.operation != binding.service.original.operation
                        or record.request is not active._request
                        or len(request_values) != len(active._request_values)
                        or any(type(value) is not type(original) or value != original
                               for value, original in zip(request_values, active._request_values))
                        or active.context.data is not active.data or active.context.returns is not active.returns
                        or type(private[DataStack].get("_pointer")) is not int
                        or private[DataStack].get("_pointer") != 64
                        or type(private[ReturnStack].get("_pointer")) is not int
                        or private[ReturnStack].get("_pointer") != 128):
                    self._registration_failure = "private service validation issuance changed"
                    raise CallbackExportError(self._registration_failure)
                if binding.service.catalog._service.fpcsr != observation.fpcsr:
                    self._registration_failure = "private service validation FPCSR changed"
                    raise CallbackExportError(self._registration_failure)
                request = record.request
                failure = _issue_service_value(_SERVICE_FAILURE_LAYOUT, (
                    binding.descriptor.export_id, binding.descriptor.name,
                    request.invocation_id, request.sequence, request.site.call_offset,
                    request.site.stub_offset, observation.operation, consumed,
                    observation.fpcsr, steps, error, "illegal_scalar_float", -21,
                ))
            result = _issue_service_value(_SERVICE_RECEIPT_LAYOUT,
                                          (steps, entered, completed, consumed, failure))
            self._closed_accounting = None
            return result

    def _guard_outer_unwind(self, error, context):
        return self._unwind_error is error and any(item is context for item in self._unwind_contexts)

    @property
    def _active_context(self) -> ExecutionContext | None:
        return None if self._active is None else self._active.context

    def _require_owner(self, operation: str) -> None:
        self._runtime._require_session_owner_access(operation)
        if self._registration_failure is not None:
            raise CallbackExportError(self._registration_failure)
        if getattr(self._runtime, "_callback_exports", None) is not self:
            raise CallbackExportError("callback export engine is not its runtime's owner")
        if self._runtime.dictionary is not self._dictionary:
            raise CallbackExportError("callback export dictionary owner has changed")

    def _require_leaf(self, leaf: _CanonicalLeaf) -> None:
        try:
            live = self._dictionary.resolve(leaf.word.xt)
        except KeyError:
            raise CallbackExportError("callback export Word is no longer live") from None
        if live is not leaf.word:
            raise CallbackExportError("callback export XT no longer names its original Word")
        if (live.implementation is not leaf.implementation
                or leaf.implementation.callback is not leaf.callback):
            raise CallbackExportError("callback export implementation has changed")

    def _binding(self, handle: CallbackExportHandle) -> _ExportBinding:
        if type(handle) is not CallbackExportHandle:
            raise TypeError("callback invocation requires an issued export handle")
        if handle._owner is not self._owner:
            raise CallbackExportError("callback export belongs to a different owner")
        if type(handle.export_id) is not int:
            raise CallbackExportError("callback export handle has a malformed ID")
        binding = self._exports.get(handle.export_id)
        if binding is None or binding.handle is not handle:
            raise CallbackExportError("callback export handle is not the issued identity")
        self._require_binding(binding)
        return binding

    def _require_binding(self, binding):
        if binding.service is not None:
            binding.service.verify()
        elif binding.closed is None:
            self._require_leaf(binding.leaf)
        else:
            binding.closed.verify(self)

    def bind(self, descriptor: CallbackExportV2) -> CallbackExportHandle:
        with self._runtime._session_owner_lock:
            self._require_owner("bind a semantic callback export")
            self._runtime._require_no_suspension("bind a semantic callback export")
            if self._active is not None:
                raise CallbackExportError("cannot publish an export during a callback")
            if self._registration is not None:
                raise CallbackExportError("cannot publish an export during export registration")
            return self._bind_descriptor(_descriptor(descriptor))

    def _bind_descriptor(self, descriptor: CallbackExportV2) -> CallbackExportHandle:
        """Bind already revalidated metadata under the owner's held lock."""

        existing = self._exports.get(descriptor.export_id)
        if existing is not None:
            if existing.descriptor != descriptor:
                raise CallbackExportError("callback export ID has a conflicting descriptor")
            self._require_binding(existing)
            return existing.handle
        closed = None
        leaf = None
        service = None
        if len(self._exports) >= MAX_CALLBACK_EXPORTS:
            raise CallbackExportError("callback export table is full")
        handle = CallbackExportHandle(descriptor.export_id, self._owner)
        binding = None
        try:
            if type(descriptor) is ServiceExportV5:
                if self._service_catalog is None or not self._service_finalized:
                    raise CallbackExportError(self._service_unavailable)
                service = self._service_catalog.bind(descriptor)
            elif type(descriptor) is CallbackExportV4 and descriptor.effect != "integer_leaf":
                from simulator.interop_nested import NestedCapture
                closed = NestedCapture.create(self, descriptor, handle)
            elif descriptor.effect == "closed_integer_colon":
                closed = self._closed_capture_type.create(self, descriptor)
            else:
                leaf = self._canonical.get(descriptor.name)
                if leaf is None:
                    raise CallbackExportError("canonical installed callback Word is unavailable")
                self._require_leaf(leaf)
            binding = _ExportBinding(handle, descriptor, leaf, closed, service)
            if self._registration is not None:
                # Record the exact identity before insertion, so a failure after
                # publication but before return still has complete rollback data.
                self._registration.issued.append((descriptor.export_id, binding))
            self._exports[descriptor.export_id] = binding
            return handle
        except BaseException:
            if binding is not None and self._exports.get(descriptor.export_id) is binding:
                del self._exports[descriptor.export_id]
            for token, captured in tuple(self._nested_calls.items()):
                if captured.handle is handle:
                    del self._nested_calls[token]
            raise

    def _check_registration(self, registration: _ExportRegistration) -> None:
        self._require_owner("finish semantic callback export registration")
        if self._registration is not registration or self._exports is not registration.table:
            raise CallbackExportError("callback registration table ownership changed")
        expected = dict(registration.previous)
        expected.update(registration.issued)
        if (len(self._exports) != len(expected)
                or any(self._exports.get(key) is not binding for key, binding in expected.items())):
            raise CallbackExportError("callback registration binding identity changed")
        for binding in expected.values():
            self._binding(binding.handle)

    def _rollback_registration(self, registration: _ExportRegistration) -> None:
        table = registration.table
        for export_id, binding in reversed(registration.issued):
            # Never remove a replacement, a preexisting binding, or a binding
            # merely carrying the same numerical export ID.
            if table.get(export_id) is binding:
                del table[export_id]
            for token, captured in tuple(self._nested_calls.items()):
                if captured.handle is binding.handle:
                    del self._nested_calls[token]
        if (self._registration is not registration or self._exports is not table
                or len(table) != len(registration.previous)
                or any(table.get(key) is not binding
                       for key, binding in registration.previous.items())):
            raise CallbackExportError("callback registration rollback could not restore exact bindings")

    @contextmanager
    def registration(
        self, descriptors: tuple[CallbackExportV2, ...],
    ) -> Iterator[tuple[CallbackExportHandle, ...]]:
        """Issue a bounded batch; retain it only if the caller's body succeeds.

        Existing equal bindings are reused. Exceptional exit revokes only new
        exact bindings from this batch. The owner lock covers the whole body;
        verification is allowed, but nested publication and invocation are not.
        """

        with self._runtime._session_owner_lock:
            self._require_owner("register semantic callback exports")
            self._runtime._require_no_suspension("register semantic callback exports")
            if self._active is not None:
                raise CallbackExportError("cannot publish exports during a callback")
            if self._registration is not None:
                raise CallbackExportError("nested callback export registration is not admitted")
            if type(descriptors) is not tuple:
                raise TypeError("callback registration descriptors must be an exact tuple")
            if len(descriptors) > MAX_CALLBACK_EXPORTS:
                raise ValueError("callback registration admits at most 64 descriptors")
            checked = tuple(_descriptor(value) for value in descriptors)
            unique: dict[int, CallbackExportV2] = {}
            for descriptor in checked:
                previous = unique.setdefault(descriptor.export_id, descriptor)
                if previous != descriptor:
                    raise CallbackExportError("callback export ID has a conflicting descriptor")
            registration = _ExportRegistration(self._exports, dict(self._exports))
            self._registration = registration
            try:
                handles = {key: self._bind_descriptor(descriptor)
                           for key, descriptor in unique.items()}
                yield tuple(handles[descriptor.export_id] for descriptor in checked)
                self._check_registration(registration)
            except BaseException as error:
                try:
                    self._rollback_registration(registration)
                except BaseException as cleanup:
                    try:
                        cleanup_detail = str(cleanup)
                    except BaseException:
                        cleanup_detail = "cleanup error text unavailable"
                    self._registration_failure = (
                        f"callback export registration rollback failed: "
                        f"{type(cleanup).__name__}: {cleanup_detail}"
                    )
                    # Match routine publication: preserve the original error
                    # object/type and its cause even if repair also fails.
                    add_note = getattr(BaseException, "add_note", None)
                    if add_note is not None:
                        add_note(error, self._registration_failure)
                raise
            finally:
                self._registration = None

    def verify(self, handle: CallbackExportHandle) -> CallbackExportV2:
        with self._runtime._session_owner_lock:
            self._require_owner("verify a semantic callback export")
            return replace(self._binding(handle).descriptor)

    def inspect(self, handle):
        with self._runtime._session_owner_lock:
            self._require_owner("inspect a callback export")
            closed = self._binding(handle).closed
            return None if closed is None else replace(closed.proof)

    def policy_core_xt(self, name):
        with self._runtime._session_owner_lock:
            self._require_owner("resolve a canonical policy dependency")
            self._runtime._require_no_suspension("resolve a canonical policy dependency")
            if self._active is not None or self._registration is not None:
                raise CallbackExportError("canonical policy resolution requires an idle export boundary")
            if type(name) is not str or name not in _CANONICAL_CALLBACKS:
                raise CallbackExportError("policy core name is not in the canonical catalog")
            leaf = self._canonical.get(name)
            if leaf is None:
                raise CallbackExportError("canonical policy Word is unavailable")
            from simulator.interop_closed import _require_metadata_routes
            _require_metadata_routes()
            self._require_leaf(leaf)
            return leaf.word.xt

    def _budget_error(self, reason, limit, semantic_steps):
        error = CallbackExportBudgetExceeded(reason, limit, semantic_steps)
        self._budget_failure = error
        return error

    def consume_budget_failure(self, error):
        with self._runtime._session_owner_lock:
            self._require_owner("identify an owned callback budget failure")
            if self._budget_failure is error and error is not None:
                self._budget_failure = None
                return True
            return False

    def invoke(
        self, handle: CallbackExportHandle, arguments: tuple[int, ...], *,
        semantic_step_limit: int | None = None,
    ) -> CallbackExportResult:
        from simulator.interop_services import _ServiceAccounting

        with self._runtime._session_owner_lock:
            self._require_owner("invoke a semantic callback export")
            record = self._closed_accounting
            if record is not None:
                if record.handle is not handle or record.entered:
                    raise CallbackExportError("closed accounting admits one exact callback invocation")
                record.admitting = True
            try:
                self._runtime._require_no_suspension("invoke a semantic callback export")
            finally:
                if record is not None:
                    record.admitting = False
            if self._active is not None:
                raise CallbackExportError("nested semantic callback exports are not admitted")
            if self._registration is not None:
                raise CallbackExportError("cannot invoke an export during export registration")
            record = self._closed_accounting
            if record is not None:
                if record.handle is not handle or record.entered:
                    raise CallbackExportError("closed accounting admits one exact callback invocation")
                record.entered = True
            self._budget_failure = None
            binding = self._binding(handle)
            if type(binding.descriptor) is CallbackExportV4:
                raise CallbackExportError("V4 callbacks require their admitted nested chain")
            _cells(arguments, count=binding.descriptor.input_cells, label="arguments")
            if binding.service is not None:
                if (type(record) is not _ServiceAccounting or record.binding is not binding
                        or record.request.arguments != arguments):
                    raise CallbackExportError("private service invocation requires its exact prepared request")
            elif type(record) is _ServiceAccounting:
                raise CallbackExportError("private service checkpoint cannot invoke another profile")
            if semantic_step_limit is not None:
                if type(semantic_step_limit) is not int:
                    raise TypeError("callback semantic step limit must be an exact integer")
                if not 0 <= semantic_step_limit <= MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS:
                    raise ValueError("callback semantic step limit must be in 0..65536")
                if semantic_step_limit == 0:
                    raise self._budget_error("callback_semantic_limit", 0, 0)

            # Real finite stack bounds, using the canonical stack classes and
            # a separate tiny memory owner. No caller stack or guest byte is
            # copied, aliased or restored across this dispatch.
            half = MAX_SIGNATURE_CELLS * CELL_BYTES
            memory = SparseAddressSpace(bank0_size=2 * half, page_size=2 * half)
            memory.write8(0, 0)  # Materialize the one private page even for zero-input policies.
            data = DataStack(arguments, memory=memory, floor=0, empty_pointer=half)
            returns = ReturnStack(memory=memory, floor=half, empty_pointer=2 * half)
            context = ExecutionContext(data=data, returns=returns)
            if binding.service is not None:
                return self._invoke_service(binding, context, semantic_step_limit)
            if binding.closed is not None:
                return self._invoke_closed(binding, context, semantic_step_limit)
            active = _ActiveExport(
                binding, context, data, returns, arguments,
                _PrivateStackSeal.capture(data), _PrivateStackSeal.capture(returns),
            )
            self._active = active
            try:
                # Existing execute chooses the original meter when nested.
                # The fixed leaf costs one step; a fresh root gets that same
                # finite allowance instead of an unbounded private budget.
                result = self._runtime.execute(
                    binding.leaf.word.xt,
                    context=context,
                    step_budget=None if self._runtime._active_dispatches else 1,
                )
                if (not active.dispatched or type(result.semantic_steps) is not int
                        or result.semantic_steps != 1
                        or type(context) is not ExecutionContext
                        or context.data is not data or context.returns is not returns
                        or not active.data_seal.matches() or not active.return_seal.matches()
                        or not context.reusable or returns.depth() != 0):
                    raise CallbackExportError("callback dispatch did not return balanced leaf state")
                outputs = data.snapshot()
                _cells(outputs, count=binding.descriptor.output_cells, label="outputs")
                return CallbackExportResult(outputs, result.semantic_steps)
            finally:
                self._active = None

    def _invoke_service(self, binding, context, semantic_step_limit):
        from simulator.interop_closed import capture_meter
        from simulator.interop_services import ServiceDispatch, _ServiceAccounting

        record = self._closed_accounting
        meter, _starting = self._runtime._meter_for_public_call(
            None if self._runtime._active_dispatches else 1
        )
        if record.meter is not None and record.meter is not meter:
            raise CallbackExportError("private service meter owner changed")
        if record.meter is None:
            record.namespace, record.starting_steps = capture_meter(meter)
            record.meter = meter
        active = None
        original = None
        namespace = object.__getattribute__(self, "__dict__")
        try:
            if (self._service_accounting_type is not _ServiceAccounting
                    or self._service_dispatch_type is not ServiceDispatch):
                self._registration_failure = "private service type projections changed"
                raise CallbackExportError(self._registration_failure)
            active = ServiceDispatch(self, binding, context, meter, semantic_step_limit)
            prepare_unwind, cleanup_failed = active.prepare_unwind, active.cleanup_failed
            self._active = active
            active.require_state()
            self._runtime._execute_guarded(binding.service.word, context, meter, closed_guard=active)
            active.require_state()
            binding.service.verify()
            if not active.completed or context.returns.depth() != 0:
                raise CallbackExportError("private scalar service did not return balanced state")
            outputs = context.data.snapshot()
            _cells(outputs, count=binding.descriptor.output_cells, label="outputs")
            result = _issue_service_value(_SERVICE_RESULT_LAYOUT, (outputs, record.semantic_steps))
            if not _service_value_routes_match(_SERVICE_RESULT_LAYOUT):
                self._registration_failure = "private service issued result routes changed"
            record.completed = True
            return result
        except BaseException as error:
            original = error
            record.error = error
            if active is not None:
                try:
                    prepare_unwind(error)
                except BaseException:
                    cleanup_failed(error)
            raise
        finally:
            changed = dict.get(namespace, "_closed_accounting") is not record
            dict.__setitem__(namespace, "_closed_accounting", record)
            dict.__setitem__(namespace, "_active", None)
            if changed:
                dict.__setitem__(namespace, "_registration_failure", "private service accounting owner changed")
                if original is None:
                    raise CallbackExportError("private service accounting owner changed")
                try:
                    BaseException.add_note(original, "private service accounting owner changed")
                except BaseException:
                    pass

    def _invoke_closed(self, binding, context, semantic_step_limit):
        meter, starting_steps = self._runtime._meter_for_public_call(
            None if self._runtime._active_dispatches else binding.descriptor.max_semantic_steps
        )
        record = self._closed_accounting
        if record is not None:
            if record.meter is not None and record.meter is not meter:
                raise CallbackExportError("closed callback accounting meter owner changed")
            if record.meter is None:
                from simulator.interop_closed import capture_meter
                record.namespace, record.starting_steps = capture_meter(meter)
            record.meter = meter
        active = self._closed_dispatch_type(self, binding, context, meter, semantic_step_limit)
        self._active = active
        try:
            active.require_state()
            self._runtime._execute_guarded(
                binding.closed.entry.word, context, meter, closed_guard=active,
            )
            active.require_state()
            binding.closed.verify(self)
            if not active.completed or context.returns.depth() != 0:
                raise CallbackExportError("closed callback did not return balanced private state")
            outputs = context.data.snapshot()
            _cells(outputs, count=binding.descriptor.output_cells, label="outputs")
            if record is not None:
                record.completed = True
            return CallbackExportResult(outputs, active.charged_ticks)
        except BaseException as error:
            try:
                active.prepare_unwind(error)
            except BaseException:
                active.cleanup_failed(error)
            raise
        finally:
            self._active = None

    def _guard_primitive(self, implementation, context: ExecutionContext):
        """Recheck after the admitted tick, immediately before its invocation.

        Accounting hooks run during meter.tick(). They cannot use that window
        to replace the approved implementation or private stack authority.
        The runtime dispatcher, not this engine, invokes the returned callback.
        """

        active = self._active
        if active is None or active.context is not context or active.dispatched:
            raise CallbackExportError("callback primitive escaped its admitted private dispatch")
        self._require_owner("dispatch a semantic callback export")
        self._require_leaf(active.binding.leaf)
        if (implementation is not active.binding.leaf.implementation
                or type(context) is not ExecutionContext
                or context.data is not active.data or context.returns is not active.returns
                or not active.data_seal.matches() or not active.return_seal.matches()
                or active.data.snapshot() != active.arguments or active.returns.depth() != 0):
            raise CallbackExportError("callback private dispatch state changed before invocation")
        active.dispatched = True
        return active.binding.leaf.callback


def verify_callback_export(runtime, handle: CallbackExportHandle) -> CallbackExportV2:
    """Verify an issued handle and return copyable metadata, never its Word."""

    engine = getattr(runtime, "_callback_exports", None)
    if type(engine) is not CallbackExportEngine:
        raise CallbackExportError("runtime has no canonical callback export engine")
    return engine.verify(handle)


_BEGIN_SERVICE_ACCOUNTING = CallbackExportEngine.begin_service_accounting
_CONSUME_SERVICE_ACCOUNTING = CallbackExportEngine.consume_service_accounting


def _service_engine(runtime):
    from simulator.runtime import MegaForthRuntime

    if type(runtime) is not MegaForthRuntime or MegaForthRuntime.__getattribute__ is not object.__getattribute__:
        raise CallbackExportError("private service runtime lookup route changed")
    namespace = object.__getattribute__(runtime, "__dict__")
    if type(namespace) is not dict or len(namespace) > 4096 or any(type(key) is not str for key in namespace):
        raise CallbackExportError("private service runtime namespace changed")
    engine = dict.get(namespace, "_callback_exports")
    if type(engine) is not CallbackExportEngine:
        raise CallbackExportError("runtime has no canonical private service export owner")
    values = object.__getattribute__(engine, "__dict__")
    if (type(values) is not dict or len(values) > 4096 or any(type(key) is not str for key in values)
            or dict.get(values, "_runtime") is not runtime):
        raise CallbackExportError("private service export owner changed")
    return engine


def begin_service_callback_accounting(runtime, handle, request):
    """Prepare one exact V5 request in the existing callback accounting slot.

    This private integration seam does not advertise native V5 capability.
    The bridge retains the returned identity and the original consume helper.
    """
    return _BEGIN_SERVICE_ACCOUNTING(_service_engine(runtime), handle, request)


def consume_service_callback_accounting(runtime, checkpoint, handle, error=None):
    """Consume engine-issued work and optional exact validation provenance.

    The returned failure is trusted only as part of this exact consume call;
    constructing or copying the diagnostic dataclass grants no fault authority.
    Cleanup intentionally uses the captured route after an exceptional dispatch.
    """
    return _CONSUME_SERVICE_ACCOUNTING(_service_engine(runtime), checkpoint, handle, error)


def service_callback_profile(runtime):
    """Report the finalized scalar owner, independent of outer executor mode.

    This is a semantic qualification query. A caller still needs its separately
    admitted V5 bridge and native callback transport before advertising service
    execution. Unsupported old embeddings simply have no private profile.
    """
    from simulator.interop_services import (
        ScalarServiceCatalog, ServiceDispatch, _ServiceAccounting, _SERVICE_ENGINE_CLASS, _keys,
    )

    try:
        engine = _service_engine(runtime)
        namespace = _SERVICE_ENGINE_CLASS.instance(engine)
        with runtime._session_owner_lock:
            if (namespace.get("_registration_failure") is not None
                    or namespace.get("_service_finalized") is not True
                    or namespace.get("_service_accounting_type") is not _ServiceAccounting
                    or namespace.get("_service_dispatch_type") is not ServiceDispatch):
                return None
            if not _service_value_routes_match(_SERVICE_PROFILE_LAYOUT):
                return None
            catalog = namespace.get("_service_catalog")
            if type(catalog) is not ScalarServiceCatalog:
                return None
            values = _keys(object.__getattribute__(catalog, "__dict__"))
            if (values.get("_runtime") is not runtime
                    or values.get("_dictionary") is not namespace.get("_dictionary")
                    or any(name in values for name in ("verify", "_verify_owner"))):
                return None
            ScalarServiceCatalog.verify(catalog)
            executor = "python_reference" if catalog._executor is None else "shared_native_kernel"
            return _issue_service_value(_SERVICE_PROFILE_LAYOUT,
                                        (executor, 5, "private_scalar_fp_v1", "scalar_fp_state"))
    except CallbackExportError:
        return None


__all__ = [
    "CallbackExportError", "CallbackExportBudgetExceeded", "CallbackExportHandle", "CallbackExportResult",
    "verify_callback_export", "ServiceCallbackFailureV5", "ServiceCallbackReceiptV5",
    "begin_service_callback_accounting", "consume_service_callback_accounting",
    "ServiceCallbackProfileV5", "service_callback_profile",
]
