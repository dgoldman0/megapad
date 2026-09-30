"""Owner-bound semantic admission for the v2 canonical integer callbacks.

Shared descriptors contain only values. This engine retains the original
installed Words and dispatches them through the runtime's ordinary meter;
neither a name, an XT nor a copied handle grants invocation authority.
"""

from __future__ import annotations

from dataclasses import dataclass, field, replace

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
            descriptor = _descriptor(descriptor)
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
            self._exports[descriptor.export_id] = _ExportBinding(handle, descriptor, leaf)
            return handle

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
