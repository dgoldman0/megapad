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
    CallbackExportV2, MAX_CALLBACK_EXPORTS, MAX_SIGNATURE_CELLS,
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
}


class CallbackExportError(ExecutionError):
    """An export is unadmitted, stale or violates its bounded leaf contract."""


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
class _CanonicalLeaf:
    word: Word
    implementation: PrimitiveDefinition
    callback: object


@dataclass(frozen=True, slots=True)
class _ExportBinding:
    handle: CallbackExportHandle
    descriptor: CallbackExportV2
    leaf: _CanonicalLeaf


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
            and stack._memory is self.memory and stack._memory_view is self.view
            and self.view._region is self.region and self.region.pages is self.pages
            and self.pages.get(0) is self.page
            and (stack._floor, stack._empty_pointer,
                 self.view._base, self.view._offset) == self.bounds
        )


def _descriptor(value: CallbackExportV2) -> CallbackExportV2:
    if type(value) is not CallbackExportV2:
        raise TypeError("callback descriptor must be a CallbackExportV2")
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
        if core_installed:
            for name, callback in _CANONICAL_CALLBACKS.items():
                word = runtime.dictionary.find(name)
                if (word is not None
                        and type(word.implementation) is PrimitiveDefinition
                        and word.implementation.callback is callback):
                    self._canonical[name] = _CanonicalLeaf(
                        word, word.implementation, callback
                    )

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
        self._require_leaf(binding.leaf)
        return binding

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
            self._require_leaf(existing.leaf)
            return existing.handle
        leaf = self._canonical.get(descriptor.name)
        if leaf is None:
            raise CallbackExportError("canonical installed callback Word is unavailable")
        self._require_leaf(leaf)
        if len(self._exports) >= MAX_CALLBACK_EXPORTS:
            raise CallbackExportError("callback export table is full")
        handle = CallbackExportHandle(descriptor.export_id, self._owner)
        binding = _ExportBinding(handle, descriptor, leaf)
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

    def invoke(
        self, handle: CallbackExportHandle, arguments: tuple[int, ...],
    ) -> CallbackExportResult:
        with self._runtime._session_owner_lock:
            self._require_owner("invoke a semantic callback export")
            self._runtime._require_no_suspension("invoke a semantic callback export")
            if self._active is not None:
                raise CallbackExportError("nested semantic callback exports are not admitted")
            if self._registration is not None:
                raise CallbackExportError("cannot invoke an export during export registration")
            binding = self._binding(handle)
            _cells(arguments, count=binding.descriptor.input_cells, label="arguments")

            # Real finite stack bounds, using the canonical stack classes and
            # a separate tiny memory owner. No caller stack or guest byte is
            # copied, aliased or restored across this dispatch.
            half = MAX_SIGNATURE_CELLS * CELL_BYTES
            memory = SparseAddressSpace(bank0_size=2 * half, page_size=2 * half)
            data = DataStack(arguments, memory=memory, floor=0, empty_pointer=half)
            returns = ReturnStack(memory=memory, floor=half, empty_pointer=2 * half)
            context = ExecutionContext(data=data, returns=returns)
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
    "CallbackExportError", "CallbackExportHandle", "CallbackExportResult",
    "verify_callback_export",
]
