"""Owner-bound semantic admission for the v2 canonical integer callbacks.

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
    descriptor: CallbackExportV2 | CallbackExportV3
    leaf: _CanonicalLeaf | None
    closed: object = None


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
            and (self.stack_type is not ReturnStack or stack._foreign_control is None)
            and stack._memory is self.memory and stack._memory_view is self.view
            and self.view._region is self.region and self.region.pages is self.pages
            and self.pages.get(0) is self.page
            and (stack._floor, stack._empty_pointer,
                 self.view._base, self.view._offset) == self.bounds
        )


def _descriptor(value):
    if type(value) not in (CallbackExportV2, CallbackExportV3):
        raise TypeError("callback descriptor must be an exact CallbackExportV2 or CallbackExportV3")
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

    def begin_closed_accounting(self, handle):
        with self._runtime._session_owner_lock:
            self._require_owner("begin closed callback accounting")
            if (self._active is not None or self._registration is not None
                    or self._closed_accounting is not None or self._nested_chain is not None):
                raise CallbackExportError("closed callback accounting already has an active owner")
            if self._binding(handle).closed is None:
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
            if (record is None or record.token is not token or record.handle is not handle
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
        if binding.closed is None:
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
        if descriptor.effect == "closed_integer_colon":
            closed = self._closed_capture_type.create(self, descriptor)
        else:
            leaf = self._canonical.get(descriptor.name)
            if leaf is None:
                raise CallbackExportError("canonical installed callback Word is unavailable")
            self._require_leaf(leaf)
        if len(self._exports) >= MAX_CALLBACK_EXPORTS:
            raise CallbackExportError("callback export table is full")
        handle = CallbackExportHandle(descriptor.export_id, self._owner)
        binding = _ExportBinding(handle, descriptor, leaf, closed)
        if self._registration is not None:
            # Record the exact identity before insertion, so a failure after
            # publication but before return still has complete rollback data.
            self._registration.issued.append((descriptor.export_id, binding))
        self._exports[descriptor.export_id] = binding
        return handle

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
            _cells(arguments, count=binding.descriptor.input_cells, label="arguments")
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


__all__ = [
    "CallbackExportError", "CallbackExportBudgetExceeded", "CallbackExportHandle", "CallbackExportResult",
    "verify_callback_export",
]
