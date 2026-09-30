"""Owned conversion between task foreign events and the shared native CPU.

This is a synchronous internal adapter. Its transport revision does not enable
the public task or composite-suspension capabilities.
"""

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass, replace
from types import FunctionType

from shared.cells import MASK64
from shared.foreign_abi import (
    ForeignBudgetV1, ForeignCallbackRequestV1, ForeignCancellationV1,
    ForeignCompletedV1, ForeignExportV1, ForeignFailedV1, ForeignFailureKindV1,
    ForeignOperationV1, ForeignReceiptV1, ForeignRunnableYieldV1,
    ForeignSignatureV1, ForeignSpanV1, ForeignStateV1,
)
from shared.hybrid_abi import (
    CODE_ALIGNMENT, MAX_CODE_BYTES, MAX_CONTROL_BYTES, MAX_RETURN_STACK_CELLS,
    MAX_ROUTINES, MAX_TOTAL_CODE_BYTES,
)
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES, Word
from simulator.foreign_runtime import ForeignTaskEngine, ForeignTaskError
from simulator.foreign_types import TaskSemanticReceiptV1


_BUDGET_FIELDS = (
    "invocation_instructions_remaining", "root_instructions_remaining",
    "invocation_callbacks_remaining", "root_callbacks_remaining", "quantum_instructions",
)
_RECEIPT_FIELDS = (
    "root_id", "invocation_id", "parent_invocation_id", "depth", "sequence",
    "invocation_started", "root_entries", "state", "instructions", "cycles",
    "callback_requests", "invocation_instructions", "invocation_cycles",
    "invocation_callbacks", "root_instructions", "root_cycles", "root_callbacks",
)
_NATIVE_METHODS = (
    "prepare_code", "seal_publications", "is_code_registered", "is_code_published",
    "revoke_code", "bind_root", "begin", "advance", "reply", "cancel_suffix",
    "cancel_all", "last_receipt", "close",
)
_NATIVE_TYPES = (
    "TaskRoutineRunnerV1", "TaskRoutineSpecV1", "TaskBudgetV1", "TaskRootTokenV1",
    "TaskOperationTokenV1", "TaskRequestTokenV1", "TaskChildEdgeV1",
    "TaskSegmentReceiptV1", "TaskSegmentResultV1", "TaskCancellationV1",
)
_OWNER_HOOKS = ("_install_task_adapter", "_require_task_adapter", "_admit_task_root",
                "_settle_task_receipt", "_settle_task_semantic_receipt")
_SEMANTIC_RECEIPT_FIELDS = tuple(vars(TaskSemanticReceiptV1)[name] for name in (
    "root_token", "root_id", "sequence", "semantic_steps"))


def _uint(value, label, minimum=0, maximum=MASK64):
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not minimum <= value <= maximum:
        raise ValueError(f"{label} is outside its finite bounds")


def _value(value, kind):
    if type(value) is not kind:
        raise TypeError(f"expected exact {kind.__name__}")
    kind.__post_init__(value)


def _cells(values, count):
    if type(values) is not tuple or len(values) != count:
        raise TypeError("task cells must be an exact tuple with the declared arity")
    for cell in values:
        _uint(cell, "task cell")


def _note(error, text):
    try:
        BaseException.add_note(error, text)
    except BaseException:
        pass


def _spans(values):
    return tuple((span.base, span.size, span.access.value) for span in values)


@dataclass(frozen=True, slots=True)
class _Registration:
    word: Word
    operation: ForeignOperationV1
    lease: object
    body: bytes
    code_base: int
    code: bytes
    entry_offset: int
    stack_base: int
    stack_size: int
    callbacks: tuple = ()
    spec: object = None
    children: tuple = ()


@dataclass(frozen=True, slots=True)
class _Frame:
    registration: _Registration
    invocation_id: int
    operation_token: object = None
    request: ForeignCallbackRequestV1 | None = None
    instructions: int = 0
    cycles: int = 0
    callbacks: int = 0


@dataclass(frozen=True, slots=True)
class TaskAdapterTotals:
    machine_instructions: int = 0
    machine_cycles: int = 0
    machine_segments: int = 0
    transitions: int = 0
    callback_requests: int = 0


@dataclass(frozen=True, slots=True)
class _Settlement:
    generation: int = 0
    receipt: ForeignReceiptV1 | None = None
    values: tuple | None = None
    frames: tuple = ()
    totals: TaskAdapterTotals = TaskAdapterTotals()
    owner_pending: bool = False


class NativeTaskAdapter:
    """One ordinary Python adapter over an existing hybrid native owner."""

    def __init__(self, hybrid):
        from hybrid.runtime import HybridRuntime

        if type(hybrid) is not HybridRuntime:
            raise TypeError("native task adapter requires the exact HybridRuntime owner")
        with hybrid.semantic._session_owner_lock:
            hybrid._require_open()
            native = hybrid._native
            revision = getattr(native, "_TASK_ROUTINE_TRANSPORT_REVISION", None)
            if (type(revision) is not int or revision != 2
                    or any(not callable(getattr(native, name, None)) for name in _NATIVE_TYPES)
                    or any(not callable(getattr(native.TaskRoutineRunnerV1, name, None))
                           for name in _NATIVE_METHODS)
                    or hybrid._nested_runner is None):
                raise RuntimeError("native task transport revision 2 is required; run make build")
            engine = hybrid.semantic._foreign_tasks
            if (type(engine) is not ForeignTaskEngine
                    or any(not callable(getattr(engine, name, None)) for name in (
                        "registration_batch", "adapter_root_policy", "task_export_dependencies",
                        "task_semantic_receipt"))):
                raise RuntimeError("native task adapter requires the engine publication and root-policy API")
            if any(not callable(getattr(hybrid, name, None)) for name in _OWNER_HOOKS):
                raise RuntimeError("native task adapter requires the hybrid task ownership hooks")
            self._owner, self._semantic, self._engine = hybrid, hybrid.semantic, engine
            self._dictionary = self._semantic.dictionary
            self._native = native
            self._runner = hybrid._nested_runner.task_v1()
            if type(self._runner) is not native.TaskRoutineRunnerV1:
                raise TypeError("task facade is not the exact shared native owner")
            self._native_types = tuple((name, getattr(native, name)) for name in _NATIVE_TYPES)
            optional_native = ("validate_parked",) if callable(getattr(type(self._runner), "validate_parked", None)) else ()
            self._runner_routes = tuple((name, getattr(type(self._runner), name),
                                         getattr(self._runner, name)) for name in (*_NATIVE_METHODS, *optional_native))
            self._engine_routes = tuple((name, getattr(ForeignTaskEngine, name), getattr(engine, name))
                                       for name in ("registration_batch", "adapter_root_policy",
                                                    "task_export_dependencies", "require_definition",
                                                    "task_semantic_receipt"))
            self._owner_routes = tuple((name, getattr(HybridRuntime, name), getattr(hybrid, name))
                                      for name in (*_OWNER_HOOKS, "close"))
            self._registrations = {}
            self._batch = None
            self._settlement = _Settlement()
            self._pending = None
            self._cancel_checkpoint = None
            self._projecting = False
            self._root_token = self._native_root = None
            self._root_id = 0
            self._root_policy = None
            self._failed = None
            self._delivery_failed = False
            self._closed = False
            self._composition_authority = None
            # The composition owner retains this exact closure before any guest
            # callback. Emergency release must not traverse changed Python routes.
            cancel_native, close_native = self._runner.cancel_all, self._runner.close
            lock = self._semantic._session_owner_lock
            settlement_kind = _Settlement
            namespace = object.__getattribute__(self, "__dict__")
            raw_get, raw_set = dict.__getitem__, dict.__setitem__
            settlement_fields = tuple((name, vars(_Settlement)[name]) for name in (
                "generation", "receipt", "values", "frames", "totals", "owner_pending"))
            raw_new = object.__new__
            native_closed = False

            def native_cleanup():
                nonlocal native_closed
                with lock:
                    if native_closed:
                        raw_set(namespace, "_closed", True)
                        return
                    failure = None
                    try:
                        cancel_native()
                    except BaseException as error:
                        failure = error
                    try:
                        close_native()
                    except BaseException:
                        if failure is None:
                            raise
                        raise failure
                    native_closed = True
                    raw_set(namespace, "_closed", True)
                    try:
                        state = raw_get(namespace, "_settlement")
                        if type(state) is settlement_kind:
                            cleared = raw_new(settlement_kind)
                            for name, field in settlement_fields:
                                field.__set__(cleared, () if name == "frames" else
                                              field.__get__(state, settlement_kind))
                            raw_set(namespace, "_settlement", cleared)
                    except BaseException:
                        if failure is None:
                            raise
                    if failure is not None:
                        raise failure

            self._native_cleanup = native_cleanup
            self._native_closed = lambda: native_closed
            registration_word = vars(_Registration)["word"]
            word_name = vars(Word)["name"]
            settlement_frames = vars(_Settlement)["frames"]

            def status_metadata():
                # These names describe issued registrations, including ones
                # later revoked by the dictionary. They grant no live lease.
                registrations = raw_get(namespace, "_registrations")
                state = raw_get(namespace, "_settlement")
                if type(registrations) is not dict or len(registrations) > MAX_ROUTINES:
                    raise ForeignTaskError("task diagnostic registration table changed")
                if type(state) is not settlement_kind:
                    raise ForeignTaskError("task diagnostic settlement changed")
                frames = settlement_frames.__get__(state, settlement_kind)
                if type(frames) is not tuple or len(frames) > 8:
                    raise ForeignTaskError("task diagnostic frame list changed")
                names = []
                for registration in dict.values(registrations):
                    if type(registration) is not _Registration:
                        raise ForeignTaskError("task diagnostic registration changed")
                    word = registration_word.__get__(registration, _Registration)
                    if type(word) is not Word:
                        raise ForeignTaskError("task diagnostic word changed")
                    name = word_name.__get__(word, Word)
                    if type(name) is not bytes:
                        raise ForeignTaskError("task diagnostic name changed")
                    names.append(name.decode("utf-8", errors="replace"))
                return tuple(sorted(names)), len(frames)

            self._status_metadata = status_metadata
            self._owner_call("_install_task_adapter", self)

    def _owner_call(self, name, *args):
        for key, route, bound in self._owner_routes:
            if key == name:
                if (vars(type(self._owner)).get(key) is not route
                        or key in vars(self._owner)):
                    raise ForeignTaskError("task owner route changed")
                return bound(*args)
        raise ForeignTaskError("unknown task owner route")

    def _engine_call(self, name, *args, **kwargs):
        for key, route, bound in self._engine_routes:
            if key == name:
                if (vars(ForeignTaskEngine).get(key) is not route
                        or key in vars(self._engine)):
                    raise ForeignTaskError("task engine route changed")
                return bound(*args, **kwargs)
        raise ForeignTaskError("unknown task engine route")

    def _native_call(self, name, *args, **kwargs):
        for key, route, bound in self._runner_routes:
            if key == name:
                if vars(type(self._runner)).get(key) is not route:
                    raise ForeignTaskError("native task transport route changed")
                return bound(*args, **kwargs)
        raise ForeignTaskError("unknown native task route")

    def _require_owner(self, *, cleanup=False):
        if type(self) is not NativeTaskAdapter:
            raise ForeignTaskError("task adapter class changed")
        attributes = vars(self)
        for name, route in _ADAPTER_ROUTES:
            if cleanup and name in ("validate_parked", "settle_semantic_receipt"):
                continue
            if vars(NativeTaskAdapter).get(name) is not route or name in attributes:
                raise ForeignTaskError("task adapter transition route changed")
        if (self._owner.semantic is not self._semantic
                or self._semantic._foreign_tasks is not self._engine
                or self._semantic.dictionary is not self._dictionary
                or self._owner._native is not self._native
                or any(getattr(self._native, name, None) is not kind
                       for name, kind in self._native_types)):
            raise ForeignTaskError("task adapter owner changed")
        if not cleanup:
            self._owner_call("_require_task_adapter", self)
            if self._closed or self._failed is not None or self._delivery_failed:
                raise ForeignTaskError("task adapter is closed or requires failure cleanup")
            if self._batch is not None:
                raise ForeignTaskError("task publication is still in progress")

    @contextmanager
    def registration_batch(self):
        with self._semantic._session_owner_lock:
            self._require_owner()
            if self._frames or self._pending is not None:
                raise ForeignTaskError("task publication requires an idle owner")
            with self._engine_call("registration_batch") as transaction:
                batch = _RegistrationBatch(self, transaction)
                self._batch = batch
                transaction.on_rollback(batch._rollback)
                try:
                    yield batch
                    batch._commit()
                finally:
                    batch._active = False
                    self._batch = None

    def _registration(self, operation):
        _value(operation, ForeignOperationV1)
        binding = self._registrations.get(id(operation))
        if binding is None or binding.operation is not operation:
            raise ForeignTaskError("task operation was not issued by this adapter")
        evidence = self._engine_call("require_definition", binding.word)
        if (evidence.operation is not operation or evidence.adapter.adapter is not self
                or not self._dictionary.is_body_lease_live(binding.lease)
                or binding.lease.word is not binding.word
                or self._semantic.memory.read_bytes(binding.lease.body_address, len(binding.body)) != binding.body
                or not self._native_call("is_code_published", binding.spec)):
            raise ForeignTaskError("task registration or its sealed body is stale")
        for _call, _stub, export in binding.callbacks:
            self._engine_call("task_export_dependencies", export)
        return binding

    def operation(self, word):
        with self._semantic._session_owner_lock:
            self._require_owner()
            for binding in self._registrations.values():
                if binding.word is word:
                    return self._registration(binding.operation).operation
            raise ForeignTaskError("Word is not this adapter's issued task operation")

    def _budget(self, value):
        _value(value, ForeignBudgetV1)
        return self._native.TaskBudgetV1(*(getattr(value, name) for name in _BUDGET_FIELDS))

    def _bind_root(self, token, root_id):
        _uint(root_id, "task root ID", 1)
        if token is None:
            raise TypeError("task root token is required")
        policy = self._engine_call("adapter_root_policy", self, token, root_id)
        from simulator.foreign_runtime import TaskAdapterRootPolicy

        if type(policy) is not TaskAdapterRootPolicy:
            raise ForeignTaskError("task root policy was not issued by the engine")
        limits = (policy.instruction_limit, policy.callback_limit, policy.entry_limit)
        for value, maximum in zip(limits, (10_000_000, 1024, 1024)):
            _uint(value, "task root ceiling", maximum=maximum)
        if not limits[0] or not limits[2]:
            raise ForeignTaskError("task root instruction or entry allowance is exhausted")
        self._owner_call("_admit_task_root", self, token, root_id)
        if token is self._root_token:
            if root_id != self._root_id or limits != self._root_policy:
                raise ForeignTaskError("task root identity or original policy changed")
            return
        if self._frames or root_id <= self._root_id:
            raise ForeignTaskError("task root cannot replace an active or newer owner")
        native_root = self._native_call("bind_root", root_id, limits[0], limits[1], entry_limit=limits[2])
        self._root_token, self._native_root = token, native_root
        self._root_id, self._root_policy = root_id, limits
        self._settlement = _Settlement(totals=self._totals)

    def _protected_spans(self):
        spans = list(self._owner._protected_spans(self._semantic.main_context))
        spans.extend((item.lease.body_address, len(item.body))
                     for item in self._registrations.values()
                     if self._dictionary.is_body_lease_live(item.lease))
        from hybrid.runtime import _merge_spans
        return _merge_spans(spans)

    def begin(self, operation, arguments, *, root_token, root_id, budget, parent=None):
        with self._semantic._session_owner_lock:
            self._require_owner()
            binding = self._registration(operation)
            _cells(arguments, operation.signature.input_cells)
            allowance = self._budget(budget)
            if budget.quantum_instructions != 0:
                raise ValueError("task begin is admission-only")
            self._bind_root(root_token, root_id)
            parent_token = edge = None
            if parent is not None:
                if (type(parent) is not ForeignCallbackRequestV1 or not self._frames
                        or self._frames[-1].request is not parent):
                    raise ForeignTaskError("task child requires the exact issued parent request")
                parent_frame = self._frames[-1]
                self._registration(parent_frame.registration.operation)
                edge = next((handle for site, child, handle in parent_frame.registration.children
                             if site == parent.site and child is operation), None)
                if edge is None:
                    raise ForeignTaskError("task child was not captured by this callback site")
                parent_token = parent.request_token
            elif self._frames:
                raise ForeignTaskError("task child cannot discard its parent request")
            return self._transition("begin", binding, binding.spec, arguments,
                _spans(operation.machine_grants), root_token=self._native_root,
                budget=allowance, parent_token=parent_token, child_edge=edge,
                protected_spans=self._protected_spans())

    def _top(self, token, *, request=False):
        if not self._frames:
            raise ForeignTaskError("task token has no live invocation")
        frame = self._frames[-1]
        actual = frame.request.request_token if request and frame.request is not None else (
            None if request else frame.operation_token)
        if token is None or token is not actual:
            raise ForeignTaskError("task token is not the exact current authority")
        self._registration(frame.registration.operation)
        return frame

    def advance(self, operation_token, *, budget):
        with self._semantic._session_owner_lock:
            self._require_owner()
            frame = self._top(operation_token)
            if frame.request is not None:
                raise ForeignTaskError("task invocation is waiting for a callback reply")
            return self._transition("advance", frame.registration, operation_token, budget=self._budget(budget))

    def reply(self, request_token, outputs, *, budget):
        with self._semantic._session_owner_lock:
            self._require_owner()
            frame = self._top(request_token, request=True)
            _cells(outputs, frame.request.export.signature.output_cells)
            return self._transition("reply", frame.registration, request_token, outputs,
                                    budget=self._budget(budget))

    def validate_parked(self, root_token, operation_token, request_token=None):
        """Prove retained authority without work, token rotation or CPU writes."""
        with self._semantic._session_owner_lock:
            self._require_owner()
            if root_token is not self._root_token or self._native_root is None:
                raise ForeignTaskError("parked validation requires the original task root")
            if self._pending is not None:
                raise ForeignTaskError("cannot validate during an active native transition")
            frame = self._top(operation_token)
            issued = None if frame.request is None else frame.request.request_token
            if request_token is not issued:
                raise ForeignTaskError("parked validation requires the exact current request")
            if not any(name == "validate_parked" for name, _route, _bound in self._runner_routes):
                raise RuntimeError("native parked validation is unavailable; run make build")
            if self._native_call("validate_parked", self._native_root,
                                 operation_token, request_token) is not True:
                raise ForeignTaskError("native task parked validation failed")
            return True

    def settle_semantic_receipt(self, receipt):
        """Project only the engine's issued cumulative callback work."""
        with self._semantic._session_owner_lock:
            engine_call, owner_call = self._engine_call, self._owner_call

            def project():
                if type(receipt) is not TaskSemanticReceiptV1:
                    raise ForeignTaskError("task callback work requires an exact engine receipt")
                token, root_id, sequence, steps = (
                    field.__get__(receipt, TaskSemanticReceiptV1) for field in _SEMANTIC_RECEIPT_FIELDS)
                _uint(root_id, "semantic receipt root", 1)
                _uint(sequence, "semantic receipt sequence", 1)
                _uint(steps, "semantic callback work", maximum=65536)
                if engine_call("task_semantic_receipt", self, token, root_id) is not receipt:
                    raise ForeignTaskError("task callback receipt was not issued to this adapter")
                # Cleanup may already be fail-closed. The original query and
                # owner authority remain usable to retain the actual prefix.
                owner_call("_settle_task_semantic_receipt", self, receipt)

            try:
                project()
            except BaseException as error:
                # The owner publishes its immutable accounting snapshot before
                # projecting counters. A single retry repairs a trace/host
                # escape in that window without charging the receipt twice.
                try:
                    project()
                except BaseException:
                    _note(error, "task semantic receipt recovery also failed")
                raise

    def _transition(self, name, binding, *args, **kwargs):
        if self._pending is not None:
            raise ForeignTaskError("task native transition is already active")
        self._pending = (name, binding)
        before = self._last
        try:
            raw = self._native_call(name, *args, **kwargs)
            receipt = self._recover_receipt()
            if receipt is None or receipt is before:
                raise ForeignTaskError("native task accepted event has no new receipt")
            return self._event(raw, binding, receipt)
        except BaseException as failure:
            try:
                self._recover_receipt()
            except BaseException:
                self._failed = failure
                _note(failure, "native task receipt recovery failed; admission is disabled")
            if self._last is not before:
                self._delivery_failed = True
            raise
        finally:
            self._pending = None

    def _recover_receipt(self):
        raw = self._native_call("last_receipt")
        if raw is None:
            if self._last is not None:
                raise ForeignTaskError("native task lost its retained receipt")
            return None
        if type(raw) is not self._native.TaskSegmentReceiptV1:
            raise ForeignTaskError("native task returned an unrelated receipt")
        generation = raw.root_generation
        _uint(generation, "native root generation", 1)
        values = tuple(getattr(raw, name) for name in _RECEIPT_FIELDS)
        if raw.root_id != self._root_id:
            raise ForeignTaskError("native receipt belongs to another original root")
        if self._root_generation and generation != self._root_generation:
            raise ForeignTaskError("native task root generation changed")
        if self._last is not None and raw.sequence == self._last.sequence:
            if values != self._last_values:
                raise ForeignTaskError("native task rewrote its settled receipt")
            _value(self._last, ForeignReceiptV1)
            expected = dict(zip(_RECEIPT_FIELDS, values))
            expected["parent_invocation_id"] = raw.parent_invocation_id or None
            if self._last != ForeignReceiptV1(**expected):
                raise ForeignTaskError("cached neutral task receipt changed")
            self._project_owner()
            return self._last
        fields = dict(zip(_RECEIPT_FIELDS, values))
        fields["parent_invocation_id"] = raw.parent_invocation_id or None
        receipt = ForeignReceiptV1(**fields)
        previous = self._last
        expected_sequence = 1 if previous is None else previous.sequence + 1
        if (receipt.sequence != expected_sequence
                or receipt.root_instructions != (0 if previous is None else previous.root_instructions) + receipt.instructions
                or receipt.root_cycles != (0 if previous is None else previous.root_cycles) + receipt.cycles
                or receipt.root_callbacks != (0 if previous is None else previous.root_callbacks) + receipt.callback_requests
                or receipt.root_entries != (0 if previous is None else previous.root_entries) + int(receipt.invocation_started)):
            raise ForeignTaskError("native task receipt sequence or totals are discontinuous")
        frames = self._frames
        if receipt.invocation_started:
            if self._pending is None or self._pending[0] != "begin":
                raise ForeignTaskError("native task began outside the admitted transition")
            frame = _Frame(self._pending[1], receipt.invocation_id)
            parent = frames[-1].invocation_id if frames else None
            if receipt.parent_invocation_id != parent or receipt.depth != len(frames) + 1:
                raise ForeignTaskError("native task receipt does not extend this frame chain")
            frames += (frame,)
        elif not frames or frames[-1].invocation_id != receipt.invocation_id:
            raise ForeignTaskError("native task receipt has no current adapter frame")
        frame = frames[-1]
        if (receipt.invocation_instructions != frame.instructions + receipt.instructions
                or receipt.invocation_cycles != frame.cycles + receipt.cycles
                or receipt.invocation_callbacks != frame.callbacks + receipt.callback_requests):
            raise ForeignTaskError("native task invocation totals are discontinuous")
        updated = replace(frame, operation_token=None, request=None,
                          instructions=receipt.invocation_instructions,
                          cycles=receipt.invocation_cycles, callbacks=receipt.invocation_callbacks)
        frames = frames[:-1] if receipt.state is ForeignStateV1.RETURNED else frames[:-1] + (updated,)
        totals = self._totals
        updated_totals = TaskAdapterTotals(
            totals.machine_instructions + receipt.instructions,
            totals.machine_cycles + receipt.cycles,
            totals.machine_segments + 1,
            totals.transitions + int(receipt.invocation_started),
            totals.callback_requests + receipt.callback_requests,
        )
        # Publish the receipt and actual frame retirement before event allocation.
        self._settlement = _Settlement(generation, receipt, values, frames, updated_totals, True)
        self._project_owner()
        return receipt

    def _project_owner(self):
        settlement = self._settlement
        if not settlement.owner_pending or self._projecting:
            return
        try:
            self._projecting = True
            # The exact owner hook is idempotent for this receipt. A host escape
            # after projection but before acknowledgement must be replayable.
            self._owner_call("_settle_task_receipt", self, settlement.receipt)
        except BaseException as failure:
            self._failed = failure
            raise
        finally:
            self._projecting = False
        self._settlement = replace(settlement, owner_pending=False)

    def _event(self, raw, binding, receipt):
        if type(raw) is not self._native.TaskSegmentResultV1:
            raise ForeignTaskError("native task returned an unrelated event")
        if (type(raw.receipt) is not self._native.TaskSegmentReceiptV1
                or raw.receipt.root_generation != self._root_generation
                or tuple(getattr(raw.receipt, name) for name in _RECEIPT_FIELDS) != self._last_values
                or type(raw.operation_token) is not self._native.TaskOperationTokenV1):
            raise ForeignTaskError("native task event and retained receipt disagree")
        token = raw.operation_token
        state = receipt.state
        if state is ForeignStateV1.CALLBACK:
            site = raw.site
            _uint(site, "callback site", maximum=len(binding.callbacks) - 1)
            _call, _stub, export = binding.callbacks[site]
            if raw.export_id != site or type(raw.request_token) is not self._native.TaskRequestTokenV1:
                raise ForeignTaskError("native callback site does not match its issued export")
            self._engine_call("task_export_dependencies", export)
            event = ForeignCallbackRequestV1(operation_token=token, request_token=raw.request_token,
                receipt=receipt, request_sequence=raw.request_sequence, site=site,
                export=export, arguments=raw.arguments)
        elif state is ForeignStateV1.RETURNED:
            _cells(raw.outputs, binding.operation.signature.output_cells)
            event = ForeignCompletedV1(operation_token=token, receipt=receipt, outputs=raw.outputs)
        elif state is ForeignStateV1.YIELDED:
            event = ForeignRunnableYieldV1(operation_token=token, receipt=receipt)
        else:
            # Native sealed-site/target rejection is a bounded machine profile
            # failure. It is distinct from a Python adapter/host exception.
            kind = (ForeignFailureKindV1.PROFILE_REJECTED if raw.exit_kind == "invalid_callback"
                    else ForeignFailureKindV1(raw.exit_kind))
            event = ForeignFailedV1(operation_token=token, receipt=receipt, kind=kind,
                detail=raw.detail, instruction_pc=raw.instruction_pc)
        if state is not ForeignStateV1.RETURNED:
            frame = self._frames[-1]
            frames = self._frames[:-1] + (replace(frame, operation_token=token,
                request=event if state is ForeignStateV1.CALLBACK else None),)
            self._settlement = replace(self._settlement, frames=frames)
        return event

    def last_receipt(self):
        with self._semantic._session_owner_lock:
            self._require_owner(cleanup=True)
            return self._recover_receipt()

    def _cancellation(self, raw, *, all_cancel=False):
        if type(raw) is not self._native.TaskCancellationV1:
            raise ForeignTaskError("native task returned an unrelated cancellation")
        receipt = self._recover_receipt()
        if raw.receipt is None:
            if receipt is not None:
                raise ForeignTaskError("native cancellation lost its retained receipt")
        elif (type(raw.receipt) is not self._native.TaskSegmentReceiptV1
                or raw.receipt.root_generation != self._root_generation
                or tuple(getattr(raw.receipt, name) for name in _RECEIPT_FIELDS) != self._last_values):
            raise ForeignTaskError("native cancellation and retained receipt disagree")
        retired = raw.retired_invocation_ids
        checkpoint = self._cancel_checkpoint
        if all_cancel and checkpoint is not None:
            original, boundary = checkpoint
            full = tuple(frame.invocation_id for frame in reversed(original))
            prefix = tuple(frame.invocation_id for frame in reversed(original[:boundary]))
            if (type(retired) is not tuple or any(type(value) is not int for value in retired)
                    or retired not in (full, prefix) or raw.surviving_parent_id != 0
                    or raw.surviving_parent_token is not None):
                raise ForeignTaskError("native all-cancel cannot reconcile the exact lost suffix")
            # The successful native all-cancel proves both the earlier retired
            # suffix and any remaining ancestors are gone. Execution stays failed.
            self._settlement = replace(self._settlement, frames=())
            self._cancel_checkpoint = None
            return ForeignCancellationV1(retired_invocation_ids=full, receipt=receipt)
        if type(retired) is not tuple or len(retired) > len(self._frames):
            raise ForeignTaskError("native cancellation exceeds the owned frame chain")
        expected = tuple(frame.invocation_id for frame in reversed(self._frames[-len(retired):])) if retired else ()
        if retired != expected:
            raise ForeignTaskError("native cancellation did not retire the exact owned suffix")
        survivors = self._frames[:-len(retired)] if retired else self._frames
        parent = survivors[-1] if survivors else None
        expected_parent = None if parent is None else parent.invocation_id
        expected_token = None if parent is None or parent.request is None else parent.request.request_token
        if (raw.surviving_parent_id or None) != expected_parent or raw.surviving_parent_token is not expected_token:
            raise ForeignTaskError("native cancellation changed the surviving parent request")
        self._settlement = replace(self._settlement, frames=survivors)
        return ForeignCancellationV1(retired_invocation_ids=retired,
            surviving_parent_id=expected_parent, surviving_parent_token=expected_token, receipt=receipt)

    def cancel_suffix(self, operation_token):
        with self._semantic._session_owner_lock:
            self._require_owner()
            if operation_token is None or not any(frame.operation_token is operation_token for frame in self._frames):
                raise ForeignTaskError("suffix cancellation requires an exact live operation token")
            boundary = next(index for index, frame in enumerate(self._frames)
                            if frame.operation_token is operation_token)
            self._cancel_checkpoint = (self._frames, boundary)
            try:
                result = self._cancellation(self._native_call("cancel_suffix", operation_token))
            except BaseException as failure:
                self._failed = failure
                self._delivery_failed = True
                raise
            self._cancel_checkpoint = None
            return result

    def cancel_all(self):
        with self._semantic._session_owner_lock:
            self._require_owner(cleanup=True)
            try:
                result = self._cancellation(self._native_call("cancel_all"), all_cancel=True)
            except BaseException as failure:
                self._failed = failure
                raise
            self._delivery_failed = False
            return result

    def close(self):
        """Close the composition owner through its original lifecycle route."""
        return self._owner_call("close")

    def _close_native(self):
        """Owner-only cleanup after semantic task retirement at an idle boundary."""
        return self._native_cleanup()

    @property
    def active_invocations(self):
        return tuple(frame.invocation_id for frame in self._frames)

    @property
    def _frames(self):
        return self._settlement.frames

    @property
    def _last(self):
        return self._settlement.receipt

    @property
    def _last_values(self):
        return self._settlement.values

    @property
    def _root_generation(self):
        return self._settlement.generation

    @property
    def _totals(self):
        return self._settlement.totals

    @property
    def totals(self):
        return self._totals

    @property
    def machine_instructions(self):
        return self._totals.machine_instructions

    @property
    def machine_cycles(self):
        return self._totals.machine_cycles

    @property
    def machine_segments(self):
        return self._totals.machine_segments

    @property
    def transitions(self):
        return self._totals.transitions

    @property
    def callback_requests(self):
        return self._totals.callback_requests


class _RegistrationBatch:
    def __init__(self, adapter, transaction):
        self._adapter, self._transaction = adapter, transaction
        self._active = True
        self._original = adapter._registrations
        self._candidates = {}
        self._prepared = []
        owner = adapter._owner
        self._allocation = (owner._control_used, owner._issued_code_bytes, owner._issued_child_edges)
        self._control_used, self._code_bytes, self._edge_count = self._allocation

    def _require(self):
        if not self._active or self._adapter._batch is not self:
            raise ForeignTaskError("task registration batch is no longer active")

    def define_operation(self, name, code, signature, *, entry_offset=0,
                         machine_grants=(), max_instructions=1_000_000,
                         max_callbacks=1024, return_stack_cells=16):
        self._require()
        if type(name) is not str or not name or not name.isascii():
            raise TypeError("task operation name must be a nonempty ASCII string")
        if type(code) is not bytes or not 1 <= len(code) <= MAX_CODE_BYTES:
            raise ValueError("task code must be nonempty bounded exact bytes")
        _value(signature, ForeignSignatureV1)
        _uint(entry_offset, "entry offset", maximum=len(code) - 1)
        _uint(return_stack_cells, "return stack cells", 1, MAX_RETURN_STACK_CELLS)
        adapter = self._adapter
        owner = adapter._owner
        if len(owner._registrations) + len(self._original) + len(self._candidates) >= MAX_ROUTINES:
            raise ValueError("the shared native publication table is full")
        padded = code + b"\x01" * (-len(code) % CODE_ALIGNMENT)
        stack_size = return_stack_cells * 8
        if self._code_bytes + len(padded) > MAX_TOTAL_CODE_BYTES or self._control_used + stack_size > MAX_CONTROL_BYTES:
            raise ValueError("task code or control arena capacity is exhausted")
        dictionary = adapter._dictionary
        body_base = dictionary.here + HEADER_FIXED_BYTES + len(name) + SEMANTIC_CODE_SLOT_BYTES
        prepad = -body_base % CODE_ALIGNMENT
        body = b"\x01" * prepad + padded
        operation = ForeignOperationV1(registration=object(), signature=signature,
            machine_grants=machine_grants, max_instructions=max_instructions, max_callbacks=max_callbacks)
        word = self._transaction.define_operation(name, adapter, operation, initial_body=body,
            protected_spans=(ForeignSpanV1(base=body_base, size=len(body), access="read"),))
        lease = self._transaction.body_lease(word)
        if lease.body_address != body_base or lease.body_limit != body_base + len(body):
            raise ForeignTaskError("task code body publication changed its admitted geometry")
        binding = _Registration(word, operation, lease, body, body_base + prepad, padded,
            entry_offset, owner._control_base + self._control_used, stack_size)
        self._candidates[id(word)] = binding
        self._control_used += stack_size
        self._code_bytes += len(padded)
        return word

    def define_colon(self, name, operations):
        self._require()
        return self._transaction.define_colon(name, operations)

    def capture_export(self, target, signature, *, task_grants=(), dynamic_targets=(),
                       fault_target=None, max_semantic_steps=4096):
        self._require()
        return self._transaction.capture_export(target, signature, task_grants=task_grants,
            dynamic_targets=dynamic_targets, fault_target=fault_target, max_semantic_steps=max_semantic_steps)

    def set_callbacks(self, word, callbacks):
        self._require()
        binding = self._candidates.get(id(word))
        if binding is None or binding.word is not word:
            raise ForeignTaskError("callback sites require this batch's exact operation Word")
        if type(callbacks) is not tuple or len(callbacks) > 16:
            raise TypeError("callback sites must be an exact tuple of at most sixteen rows")
        for row in callbacks:
            if type(row) is not tuple or len(row) != 3:
                raise TypeError("callback row must contain call offset, stub offset and export")
            call, stub, export = row
            _uint(call, "callback call offset", maximum=len(binding.code) - 1)
            _uint(stub, "callback stub offset", maximum=len(binding.code) - 1)
            _value(export, ForeignExportV1)
        self._candidates[id(word)] = replace(binding, callbacks=callbacks)

    def _commit(self):
        self._require()
        adapter = self._adapter
        candidates = []
        for binding in self._candidates.values():
            operation = binding.operation
            spec = adapter._native.TaskRoutineSpecV1(binding.code_base, binding.code, binding.entry_offset,
                operation.signature.input_cells, operation.signature.output_cells, binding.stack_base,
                binding.stack_size, operation.max_instructions, operation.max_callbacks,
                tuple((call, stub, index, export.signature.input_cells, export.signature.output_cells)
                      for index, (call, stub, export) in enumerate(binding.callbacks)))
            # Enroll identity before preparation, including forward-then-raise failures.
            self._prepared.append(spec)
            adapter._native_call("prepare_code", spec)
            candidates.append(replace(binding, spec=spec))
        by_operation = dict(self._original)
        by_operation.update((id(binding.operation), binding) for binding in candidates)
        batches, row_authority = [], []
        for binding in candidates:
            rows, authority = [], []
            for site, (_call, _stub, export) in enumerate(binding.callbacks):
                dependencies = self._transaction.task_export_dependencies(export)
                for dependency in dependencies:
                    child = by_operation.get(id(dependency))
                    if child is None or child.operation is not dependency:
                        raise ForeignTaskError("captured task child belongs to another adapter")
                    rows.append((site, child.spec))
                    authority.append((site, dependency))
            self._edge_count += len(rows)
            if self._edge_count > 65536:
                raise ValueError("shared task child-edge capacity is exhausted")
            batches.append((binding.spec, tuple(rows)))
            row_authority.append(tuple(authority))
        handles = adapter._native_call("seal_publications", tuple(batches))
        if type(handles) is not tuple or len(handles) != len(candidates):
            raise ForeignTaskError("native task publication returned an invalid edge table")
        replacements = dict(self._original)
        for binding, authority, edges in zip(candidates, row_authority, handles):
            if type(edges) is not tuple or len(edges) != len(authority) or any(
                    type(edge) is not adapter._native.TaskChildEdgeV1 for edge in edges):
                raise ForeignTaskError("native task publication returned invalid child authority")
            binding = replace(binding, children=tuple((site, operation, edge)
                for (site, operation), edge in zip(authority, edges)))
            replacements[id(binding.operation)] = binding
        adapter._registrations = replacements
        owner = adapter._owner
        owner._control_used, owner._issued_code_bytes, owner._issued_child_edges = (
            self._control_used, self._code_bytes, self._edge_count)

    def _rollback(self):
        adapter = self._adapter
        failure = None
        for spec in reversed(self._prepared):
            try:
                if adapter._native_call("is_code_registered", spec):
                    adapter._native_call("revoke_code", spec)
            except BaseException as error:
                if failure is None:
                    failure = error
        adapter._registrations = self._original
        owner = adapter._owner
        owner._control_used, owner._issued_code_bytes, owner._issued_child_edges = self._allocation
        if failure is not None:
            adapter._failed = failure
            raise failure


_ADAPTER_ROUTES = tuple((name, value) for name, value in vars(NativeTaskAdapter).items()
                        if type(value) in (FunctionType, property))


# Keep original function bodies alongside the existing eager route identities.
# Read-only application capability checks must not create a new late baseline.
_ADAPTER_CAPABILITY_ROUTES = tuple(
    (NativeTaskAdapter, name, route, (callback, callback.__code__, callback.__globals__,
        callback.__defaults__, callback.__kwdefaults__,
        tuple(cell.cell_contents for cell in (callback.__closure__ or ()))))
    for name, route in _ADAPTER_ROUTES
    for callback in ((route.fget if type(route) is property else route),)
)
