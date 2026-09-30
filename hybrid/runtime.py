"""One semantic owner with explicit, bounded architectural routine calls.

The semantic runtime remains the source, service, and continuation authority.
Only registered primitive callbacks cross into the architectural interpreter;
both engines retain the same fixed ordinary-memory buffers.
"""

from __future__ import annotations

from contextlib import nullcontext
from dataclasses import dataclass, field, replace
import os
from typing import Any, Callable
import weakref
from types import BuiltinFunctionType, BuiltinMethodType, MethodType

from shared.cells import MASK64
from shared.hybrid_abi import (
    CELL_BYTES, CODE_ALIGNMENT, HYBRID_ABI, HYBRID_ABI_VERSION,
    MAX_CONTROL_BYTES, MAX_DISPATCH_INSTRUCTIONS, MAX_ROUTINES,
    MAX_TOTAL_CODE_BYTES, MachineRoutineResultV1,
    RoutineDeclarationV1, RoutineImageV1,
    HYBRID_CALLBACK_ABI_VERSION, MAX_DISPATCH_CALLBACKS,
    MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS, CallbackRequestV2, CallbackSiteV2,
    MachineExitKindV2, MachineSegmentResultV2, RoutineDeclarationV2, RoutineImageV2,
    HYBRID_CLOSED_ABI_VERSION, CallbackRequestV3, MachineSegmentResultV3,
    RoutineDeclarationV3, RoutineImageV3,
)
from shared.hybrid_nested import (RoutineImageV4, RoutineDeclarationV4, MAX_CHILD_EDGES,
                                  CallbackRequestV4, MachineSegmentResultV4)
from shared.hybrid_services import (
    RoutineImageV5, RoutineDeclarationV5, CallbackRequestV5, MachineSegmentResultV5,
    SERVICE_CAPABILITY_V5, SERVICE_EFFECT_V5,
)
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES, Word
from simulator.errors import ExecutionBlocked, ExecutionError
from simulator.interop_exports import (
    CallbackExportBudgetExceeded, CallbackExportResult, ClosedCallbackReceipt, CallbackExportEngine,
    ServiceCallbackReceiptV5, ServiceCallbackFailureV5, ServiceCallbackProfileV5,
    begin_service_callback_accounting, consume_service_callback_accounting,
    service_callback_profile,
)
from simulator.memory import AddressClass, MMIO_BASE, MMIO_LIMIT, SparseAddressSpace
from simulator.platform import create_one_core_address_space
from simulator.runtime import ExecutionContext, MegaForthRuntime
from simulator.stacks import DataStack, ReturnStack


# Capture the issuing routes once. A same-shaped/copyable receipt alone never
# establishes service-fault authority, and a hook cannot replace cleanup.
_SERVICE_ACCOUNTING_BEGIN = begin_service_callback_accounting
_SERVICE_ACCOUNTING_CONSUME = consume_service_callback_accounting
_SERVICE_RECEIPT_FIELDS = tuple(vars(ServiceCallbackReceiptV5)[name] for name in (
    "semantic_steps", "entered", "completed", "consumed_input_cells", "failure",
))
_SERVICE_FAILURE_FIELDS = tuple(vars(ServiceCallbackFailureV5)[name] for name in (
    "export_id", "name", "invocation_id", "sequence", "call_offset", "stub_offset",
    "operation", "consumed_input_cells", "fpcsr", "semantic_steps", "cause", "fault_kind", "throw_code",
))
_SERVICE_RESULT_FIELDS = tuple(vars(CallbackExportResult)[name] for name in ("outputs", "semantic_steps"))
_SERVICE_VALUE_ROUTES = tuple((cls, tuple((name, value) for name, value in vars(cls).items()
                                        if name != "__slotnames__")) for cls in (
    ServiceCallbackReceiptV5, ServiceCallbackFailureV5, CallbackExportResult,
))


def _service_value_routes_unchanged():
    for cls, items in _SERVICE_VALUE_ROUTES:
        values = vars(cls)
        cache = values.get("__slotnames__")
        slots = next(value for name, value in items if name == "__slots__")
        if ("__slotnames__" in values and (type(cache) is not list
                or len(cache) != len(slots)
                or any(type(name) is not str or name != expected
                       for name, expected in zip(cache, slots)))):
            return False
        if (len(values) != len(items) + int("__slotnames__" in values)
                or any(values.get(name) is not value for name, value in items)):
            return False
    return True


class HybridExecutionError(ExecutionError):
    """An entry preflight or bounded machine interval failed coherently."""

    def __init__(self, reason: str, detail: str = "", *,
                 result: MachineRoutineResultV1 | None = None,
                 service_failure: ServiceCallbackFailureV5 | None = None) -> None:
        self.reason = reason
        self.result = result
        self.service_failure = service_failure
        super().__init__(f"hybrid {reason}: {detail}" if detail else f"hybrid {reason}")


@dataclass(frozen=True, slots=True)
class HybridRunReport:
    """Semantic result and machine work performed during this host call."""

    semantic_result: object
    machine_instructions: int
    machine_cycles: int
    transitions: int
    callback_requests: int = 0
    callback_semantic_steps: int = 0
    machine_segments: int = 0


@dataclass(frozen=True, slots=True)
class _ControlLease:
    owner: object
    generation: int
    base: int
    size: int


@dataclass(frozen=True, slots=True)
class _Registration:
    word: Word
    declaration: RoutineDeclarationV1
    spec: object
    implementation: object
    callback: object
    exports: tuple[tuple[CallbackSiteV2, object], ...] = ()
    child_edges: tuple = ()
    nested_graph: object = None


@dataclass(slots=True)
class _MachineAllowance:
    limit: int
    callback_limit: int
    callback_semantic_limit: int
    instructions: int = 0
    callback_requests: int = 0
    callback_semantic_steps: int = 0


@dataclass(frozen=True, slots=True, weakref_slot=True)
class _TaskOwnerAuthority:
    owner: object
    adapter: object
    accounting: object
    cleanup: object
    closed: object
    engine: object


_TASK_OWNER_AUTHORITIES = {}


def _task_owner_authority(owner, adapter=None):
    registered = _TASK_OWNER_AUTHORITIES.get(id(owner))
    if registered is None or registered[0]() is not owner:
        raise HybridExecutionError("callback_accounting", "task owner has no issued installation")
    held = registered[1]()
    if type(held) is not _TaskOwnerAuthority or held.owner is not owner:
        raise HybridExecutionError("callback_accounting", "task owner lost its issued installation")
    published = object.__getattribute__(owner, "_task_authority")
    if adapter is None:
        adapter = held.adapter
    if held.adapter is not adapter:
        raise HybridExecutionError("callback_accounting", "task adapter lost its exact installation authority")
    changed = (published is not held or object.__getattribute__(adapter, "_composition_authority") is not held
               or owner._task_adapter is not adapter
               or owner._task_accounting is not held.accounting or owner._task_cleanup is not held.cleanup)
    return held, changed


def _task_accounting_authority(owner, adapter):
    """Retain one root's counter proof independently of public projections."""
    from hybrid.task_adapter import NativeTaskAdapter
    from shared.foreign_abi import ForeignReceiptV1
    from simulator.foreign_types import TaskSemanticReceiptV1

    namespace = object.__getattribute__(owner, "__dict__")
    names = ("_machine_instructions", "_machine_cycles", "_transitions",
             "_callback_requests", "_machine_segments", "_max_machine_depth")
    receipt_names = tuple(name for name in ForeignReceiptV1.__slots__
                          if name not in ("abi", "version"))
    receipt_fields = tuple(vars(ForeignReceiptV1)[name] for name in receipt_names)
    last_receipt = NativeTaskAdapter.last_receipt
    semantic_query = adapter._engine.task_semantic_receipt
    semantic_fields = tuple(vars(TaskSemanticReceiptV1)[name] for name in (
        "root_token", "root_id", "sequence", "semantic_steps"))
    # Root token, root ID, pre-root owner totals, latest exact receipt and its
    # copied scalars, absolute owner projection. Publish each change once.
    state = (None, 0, (), None, (), ())
    # Separate from native segment sequences and from total semantic meter
    # steps. This snapshot can be replayed after an interrupted projection.
    semantic_state = (None, 0, 0, None, (), 0, False)

    def project(values):
        for name, value in zip(names, values):
            dict.__setitem__(namespace, name, value)

    def dispatch(action, *values):
        nonlocal state, semantic_state
        if object.__getattribute__(owner, "__dict__") is not namespace:
            raise HybridExecutionError("callback_accounting", "task owner namespace changed")
        token, root_id, baseline, issued, evidence, projected = state
        if action == "semantic":
            if len(values) != 1 or type(values[0]) is not TaskSemanticReceiptV1:
                raise HybridExecutionError("callback_accounting", "task semantic receipt type changed")
            receipt, = values
            copied = tuple(field.__get__(receipt, TaskSemanticReceiptV1) for field in semantic_fields)
            sem_token, sem_root, sequence, steps = copied
            if semantic_query(adapter, sem_token, sem_root) is not receipt:
                raise HybridExecutionError("callback_accounting", "task semantic work has no engine proof")
            old_token, old_root, sem_base, old_receipt, old_values, sem_projection, pending = semantic_state
            if sem_token is not old_token or sem_root != old_root:
                # An entry rejected before native root admission cannot have
                # run a callback; its engine receipt may still report zero.
                if steps == 0:
                    return
                raise HybridExecutionError("callback_accounting", "task callback work has no original root baseline")
            if old_receipt is receipt:
                if (copied[0] is not old_values[0] or copied[1:] != old_values[1:]):
                    raise HybridExecutionError("callback_accounting", "settled callback work changed")
                if pending:
                    dict.__setitem__(namespace, "_callback_semantic_steps", sem_projection)
                    semantic_state = (sem_token, sem_root, sem_base, receipt, copied, sem_projection, False)
                return
            if (sequence != (1 if old_receipt is None else old_values[2] + 1)
                    or steps < (0 if old_receipt is None else old_values[3])):
                raise HybridExecutionError("callback_accounting", "task semantic receipt is discontinuous")
            sem_projection = sem_base + steps
            semantic_state = (sem_token, sem_root, sem_base, receipt, copied, sem_projection, True)
            dict.__setitem__(namespace, "_callback_semantic_steps", sem_projection)
            semantic_state = (sem_token, sem_root, sem_base, receipt, copied, sem_projection, False)
            return
        if action == "begin":
            requested_token, requested_id = values
            if requested_token is token:
                if requested_id != root_id:
                    raise HybridExecutionError("callback_accounting", "task root identity changed")
                return
            if requested_id <= root_id:
                raise HybridExecutionError("callback_accounting", "task root identity did not advance")
            baseline = tuple(dict.__getitem__(namespace, name) for name in names)
            if any(type(value) is not int or value < 0 for value in baseline):
                raise HybridExecutionError("callback_accounting", "task owner counters are not exact")
            sem_base = dict.__getitem__(namespace, "_callback_semantic_steps")
            if type(sem_base) is not int or sem_base < 0:
                raise HybridExecutionError("callback_accounting", "task semantic counter is not exact")
            semantic_state = (requested_token, requested_id, sem_base, None, (), sem_base, False)
            state = (requested_token, requested_id, baseline, None, (), baseline)
            return
        if action != "settle" or len(values) != 1 or token is None:
            raise HybridExecutionError("callback_accounting", "task accounting has no issued root")
        receipt, = values
        # Only the adapter's pending original projection may call this hook.
        # Its pinned native receipt query proves identity and counters afresh.
        if (adapter._projecting is not True or not adapter._settlement.owner_pending
                or type(receipt) is not ForeignReceiptV1
                or last_receipt(adapter) is not receipt):
            raise HybridExecutionError("callback_accounting", "task receipt has no pending native proof")
        copied = tuple(field.__get__(receipt, ForeignReceiptV1) for field in receipt_fields)
        fields = dict(zip(receipt_names, copied))
        if fields["root_id"] != root_id:
            raise HybridExecutionError("callback_accounting", "task receipt belongs to another root")
        if issued is receipt:
            if copied != evidence:
                raise HybridExecutionError("callback_accounting", "settled task receipt changed")
            # A trace/host escape may interrupt projection after any one field.
            project(projected)
            return
        previous = dict(zip(receipt_names, evidence)) if issued is not None else None
        if (fields["sequence"] != (1 if previous is None else previous["sequence"] + 1)
                or fields["root_instructions"] != (0 if previous is None else previous["root_instructions"]) + fields["instructions"]
                or fields["root_cycles"] != (0 if previous is None else previous["root_cycles"]) + fields["cycles"]
                or fields["root_callbacks"] != (0 if previous is None else previous["root_callbacks"]) + fields["callback_requests"]
                or fields["root_entries"] != (0 if previous is None else previous["root_entries"]) + int(fields["invocation_started"])):
            raise HybridExecutionError("callback_accounting", "task receipt work is discontinuous")
        next_projection = (
            baseline[0] + fields["root_instructions"],
            baseline[1] + fields["root_cycles"],
            baseline[2] + fields["root_entries"],
            baseline[3] + fields["root_callbacks"],
            baseline[4] + fields["sequence"],
            max(projected[5], fields["depth"]),
        )
        state = (token, root_id, baseline, receipt, copied, next_projection)
        project(next_projection)

    return dispatch



@dataclass(slots=True)
class _NestedMachineFrame:
    registration: _Registration
    spans: tuple
    protected: tuple
    via_use: object = None
    raw: object = None
    callback_started: bool = False


@dataclass(slots=True)
class _NestedExecution:
    chain_token: object
    meter: object
    allowance: _MachineAllowance
    semantic_limit: int
    frames: list = field(default_factory=list)
    semantic_steps: int = 0
    root_invocation_id: int = 0
    machine_instructions: int = 0
    machine_cycles: int = 0
    callback_requests: int = 0


def _native_bound_route(method, owner):
    if type(method) is BuiltinMethodType:
        return method.__self__ is owner
    return (type(method) is MethodType and method.__self__ is owner
            and type(method.__func__) is BuiltinFunctionType)


def _service_accounting_authority(owner, allowance, runner, meter):
    """One immutable V5 ledger; mutable public counters are only projections."""
    namespace = object.__getattribute__(owner, "__dict__")
    allowances = dict.__getitem__(namespace, "_allowances")
    weak_namespace = vars(weakref.WeakKeyDictionary)["__dict__"]
    allowance_namespace = weak_namespace.__get__(allowances, weakref.WeakKeyDictionary)
    weak_routes = tuple((cls, tuple(vars(cls).items())) for cls in weakref.WeakKeyDictionary.__mro__
                        if cls is not object)
    weak_methods = tuple((name, dict.get(allowance_namespace, name)) for name in (
        "get", "__getitem__", "__setitem__", "__getattribute__",
    ))
    allowance_table = dict.__getitem__(allowance_namespace, "data")
    # The stored weakref already owns its hash. Never rehash the meter after a
    # hook may have changed a route the private engine has rejected.
    allowance_key = next(key for key in allowance_table if key() is meter)
    fields = tuple(vars(_MachineAllowance)[name] for name in (
        "limit", "callback_limit", "callback_semantic_limit", "instructions",
        "callback_requests", "callback_semantic_steps",
    ))
    initial = tuple(field.__get__(allowance, _MachineAllowance) for field in fields)
    names = ("_machine_instructions", "_machine_cycles", "_callback_requests",
             "_callback_semantic_steps", "_transitions", "_machine_segments", "_max_machine_depth")
    baseline = tuple(dict.__getitem__(namespace, name) for name in names)
    # Last segment, invocation, instructions, cycles, requests, semantic ticks,
    # segments, last consumed request, exact consumed semantic receipt.
    snapshot = (owner._v2_segment_id, owner._v2_invocation_id, 0, 0, 0, 0, 0, 0, None)
    original_invocation = snapshot[1]
    receipt_type = owner._native.RoutineSegmentReceiptV2
    class_route, class_fields = _MachineAllowance.__getattribute__, tuple(vars(_MachineAllowance).items())

    def projected():
        started = int(snapshot[1] != original_invocation)
        return (baseline[0] + snapshot[2], baseline[1] + snapshot[3],
                baseline[2] + snapshot[4], baseline[3] + snapshot[5],
                baseline[4] + started, baseline[5] + snapshot[6], max(baseline[6], started))

    def project():
        for name, value in zip(names, projected()):
            dict.__setitem__(namespace, name, value)
        dict.__setitem__(namespace, "_v2_segment_id", snapshot[0])
        dict.__setitem__(namespace, "_v2_invocation_id", snapshot[1])
        dict.__setitem__(namespace, "_runner", runner)
        dict.__setitem__(namespace, "_active_machine", True)
        dict.__setitem__(namespace, "_allowances", allowances)
        weak_namespace.__set__(allowances, allowance_namespace)
        dict.__setitem__(allowance_namespace, "data", allowance_table)
        dict.__setitem__(allowance_table, allowance_key, allowance)
        for field, value in zip(fields, initial[:3] + (
                initial[3] + snapshot[2], initial[4] + snapshot[4], initial[5] + snapshot[5])):
            field.__set__(allowance, value)

    def equal(value, expected):
        return type(value) is int and value == expected

    def require():
        if (_MachineAllowance.__getattribute__ is not class_route
                or len(vars(_MachineAllowance)) != len(class_fields)
                or any(vars(_MachineAllowance).get(name) is not value for name, value in class_fields)
                or dict.get(namespace, "_runner") is not runner
                or dict.get(namespace, "_active_machine") is not True
                or dict.get(namespace, "_allowances") is not allowances
                or weak_namespace.__get__(allowances, weakref.WeakKeyDictionary) is not allowance_namespace
                or any(dict.get(allowance_namespace, name) is not value for name, value in weak_methods)
                or any(len(vars(cls)) != len(items)
                       or any(vars(cls).get(name) is not value for name, value in items)
                       for cls, items in weak_routes)
                or dict.get(allowance_namespace, "data") is not allowance_table
                or dict.get(allowance_table, allowance_key) is not allowance
                or not equal(dict.get(namespace, "_v2_segment_id"), snapshot[0])
                or not equal(dict.get(namespace, "_v2_invocation_id"), snapshot[1])
                or any(not equal(dict.get(namespace, name), value) for name, value in zip(names, projected()))
                or any(not equal(field.__get__(allowance, _MachineAllowance), value)
                       for field, value in zip(fields, initial[:3] + (
                           initial[3] + snapshot[2], initial[4] + snapshot[4], initial[5] + snapshot[5])))):
            project()
            dict.__setitem__(namespace, "_registration_failure", "service accounting authority changed")
            raise HybridExecutionError("callback_accounting", "service accounting authority changed")

    def publish(value):
        nonlocal snapshot
        snapshot = value
        try:
            project()
        except BaseException as error:
            dict.__setitem__(namespace, "_registration_failure", "service accounting publication interrupted")
            try:
                project()
            except BaseException:
                try:
                    BaseException.add_note(error, "service accounting projection could not be restored")
                except BaseException:
                    pass
            raise

    def dispatch(operation, value=None):
        mismatch = None
        try:
            require()
        except HybridExecutionError as error:
            mismatch = error
        if operation == "native":
            receipt = value
            if receipt is not None:
                if type(receipt) is not receipt_type:
                    raise RuntimeError("service native accounting returned a foreign receipt")
                if receipt.segment_id != snapshot[0]:
                    if (receipt.segment_id != snapshot[0] + 1
                            or receipt.invocation_id <= original_invocation
                            or (snapshot[6] and receipt.invocation_id != snapshot[1])
                            or receipt.instructions < 0 or receipt.cycles < receipt.instructions
                            or receipt.invocation_instructions != snapshot[2] + receipt.instructions
                            or receipt.invocation_cycles != snapshot[3] + receipt.cycles):
                        raise RuntimeError("service native accounting sequence changed")
                    publish((receipt.segment_id, receipt.invocation_id,
                             receipt.invocation_instructions, receipt.invocation_cycles,
                             snapshot[4] + int(receipt.callback_request), snapshot[5],
                             snapshot[6] + 1, snapshot[7], snapshot[8]))
        elif operation == "semantic":
            sequence, receipt, steps = value
            if sequence == snapshot[7] and receipt is snapshot[8]:
                project()
            elif sequence <= snapshot[7] or sequence != snapshot[4]:
                raise RuntimeError("service semantic receipt was already consumed or has no native request")
            else:
                publish(snapshot[:5] + (snapshot[5] + steps, snapshot[6], sequence, receipt))
        if mismatch is not None:
            raise mismatch
        if operation == "remaining":
            return (initial[0] - initial[3] - snapshot[2],
                    initial[1] - initial[4] - snapshot[4],
                    initial[2] - initial[5] - snapshot[5])
        if operation == "segment":
            return snapshot[0]

    return dispatch


def _nested_accounting_authority(owner, state):
    """Retain authoritative counters in one private, atomically replaced value.

    The exposed owner/state/allowance fields are projections, never sources of
    deltas. A hook may corrupt them, or an interruption may split projection;
    cleanup can always replay the last complete host-accounting publication.
    """
    namespace = object.__getattribute__(owner, "__dict__")
    allowance = state.allowance
    allowance_fields = tuple(vars(_MachineAllowance)[name] for name in (
        "limit", "callback_limit", "callback_semantic_limit", "instructions",
        "callback_requests", "callback_semantic_steps",
    ))
    allowance_values = tuple(field.__get__(allowance, _MachineAllowance) for field in allowance_fields)
    counters = ("_machine_instructions", "_machine_cycles", "_callback_requests",
                "_callback_semantic_steps", "_transitions", "_machine_segments", "_max_machine_depth")
    baseline = tuple(dict.__getitem__(namespace, name) for name in counters)
    state_fields = tuple(vars(_NestedExecution)[name] for name in (
        "allowance", "meter", "semantic_limit", "frames", "chain_token",
        "root_invocation_id", "machine_instructions", "machine_cycles", "callback_requests", "semantic_steps",
    ))
    fixed = (allowance, state.meter, state.semantic_limit, state.frames, state.chain_token)
    runner = dict.__getitem__(namespace, "_nested_runner")
    # segment, root, instructions, cycles, requests, semantic, entries, segments, depth
    snapshot = (dict.__getitem__(namespace, "_v3_segment_id"), 0, 0, 0, 0, 0, 0, 0, 0)
    frame_evidence = ()
    frame_fields = tuple(vars(_NestedMachineFrame)[name] for name in (
        "registration", "spans", "protected", "via_use", "raw", "callback_started",
    ))

    def values():
        _, _, instructions, cycles, requests, semantic, entries, segments, depth = snapshot
        return (baseline[0] + instructions, baseline[1] + cycles, baseline[2] + requests,
                baseline[3] + semantic, baseline[4] + entries, baseline[5] + segments,
                max(baseline[6], depth))

    def project():
        dict.__setitem__(namespace, "_nested_execution", state)
        dict.__setitem__(namespace, "_active_machine", True)
        dict.__setitem__(namespace, "_nested_runner", runner)
        for name, value in zip(counters, values()):
            dict.__setitem__(namespace, name, value)
        dict.__setitem__(namespace, "_v3_segment_id", snapshot[0])
        projected = allowance_values[:3] + (
            allowance_values[3] + snapshot[2], allowance_values[4] + snapshot[4],
            allowance_values[5] + snapshot[5],
        )
        for field, value in zip(allowance_fields, projected):
            field.__set__(allowance, value)
        projected_state = fixed + (snapshot[1], snapshot[2], snapshot[3], snapshot[4], snapshot[5])
        for field, value in zip(state_fields, projected_state):
            field.__set__(state, value)
        list.__setitem__(fixed[3], slice(None), tuple(row[0] for row in frame_evidence))
        for frame, recorded in frame_evidence:
            for field, value in zip(frame_fields, recorded):
                field.__set__(frame, value)

    def identical(value, expected):
        return value is expected if type(expected) not in (int, bool) else type(value) is type(expected) and value == expected

    original_classes = tuple((cls, cls.__getattribute__, tuple(vars(cls).items()))
                             for cls in (_NestedExecution, _NestedMachineFrame, _MachineAllowance))

    def require():
        routes_changed = any(
            cls.__getattribute__ is not route or hasattr(cls, "__getattr__")
            or len(vars(cls)) != len(fields)
            or any(vars(cls).get(name) is not original for name, original in fields)
            for cls, route, fields in original_classes
        )
        if (routes_changed or dict.get(namespace, "_nested_execution") is not state
                or dict.get(namespace, "_active_machine") is not True
                or dict.get(namespace, "_nested_runner") is not runner
                or any(not identical(dict.get(namespace, name), expected)
                       for name, expected in zip(counters, values()))
                or not identical(dict.get(namespace, "_v3_segment_id"), snapshot[0])
                or any(not identical(field.__get__(state, _NestedExecution), expected)
                       for field, expected in zip(state_fields, fixed + (
                           snapshot[1], snapshot[2], snapshot[3], snapshot[4], snapshot[5])))
                or any(not identical(field.__get__(allowance, _MachineAllowance), expected)
                       for field, expected in zip(allowance_fields, allowance_values[:3] + (
                           allowance_values[3] + snapshot[2], allowance_values[4] + snapshot[4],
                           allowance_values[5] + snapshot[5])))
                or len(fixed[3]) != len(frame_evidence)
                or any(fixed[3][index] is not frame
                       or any(not identical(field.__get__(frame, _NestedMachineFrame), expected)
                              for field, expected in zip(frame_fields, recorded))
                       for index, (frame, recorded) in enumerate(frame_evidence))):
            project()
            dict.__setitem__(namespace, "_registration_failure", "nested execution accounting authority changed")
            raise HybridExecutionError("callback_accounting", "nested execution accounting authority changed")

    def publish(next_snapshot):
        nonlocal snapshot
        snapshot = next_snapshot
        try:
            project()
        except BaseException as error:
            dict.__setitem__(namespace, "_registration_failure", "nested accounting projection was interrupted")
            try:
                project()
            except BaseException:
                try:
                    BaseException.add_note(error, "nested accounting projection could not be restored")
                except BaseException:
                    pass
            raise

    def dispatch(operation, value=None):
        nonlocal frame_evidence
        mismatch = None
        try:
            require()
        except HybridExecutionError as error:
            mismatch = error
        if operation == "native":
            receipt = value
            if receipt is not None and receipt.segment_id != snapshot[0]:
                if (receipt.segment_id != snapshot[0] + 1
                        or not 1 <= receipt.depth <= 8
                        or receipt.instructions < 0 or receipt.cycles < receipt.instructions
                        or receipt.chain_instructions != snapshot[2] + receipt.instructions
                        or receipt.chain_cycles != snapshot[3] + receipt.cycles
                        or receipt.chain_callbacks != snapshot[4] + int(receipt.callback_request)
                        or (snapshot[1] and receipt.root_invocation_id != snapshot[1])):
                    raise RuntimeError("nested native accounting receipt is inconsistent")
                publish((receipt.segment_id, receipt.root_invocation_id,
                         receipt.chain_instructions, receipt.chain_cycles, receipt.chain_callbacks,
                         snapshot[5], snapshot[6] + int(receipt.invocation_started), snapshot[7] + 1,
                         max(snapshot[8], receipt.depth if receipt.invocation_started else 0)))
        elif operation == "semantic":
            steps = value.chain_semantic_steps
            if type(steps) is not int or not snapshot[5] <= steps <= fixed[2]:
                raise RuntimeError("nested semantic receipt changed its cumulative allowance")
            publish(snapshot[:5] + (steps,) + snapshot[6:])
        elif mismatch is None:
            if operation == "push":
                frame_evidence += ((value, tuple(field.__get__(value, _NestedMachineFrame) for field in frame_fields)),)
                project()
            elif operation == "raw":
                frame, raw = value
                if not frame_evidence or frame_evidence[-1][0] is not frame:
                    raise RuntimeError("native frame is not the current issued identity")
                frame_evidence = frame_evidence[:-1] + ((frame, frame_evidence[-1][1][:4] + (raw, False)),)
                project()
            elif operation == "begin":
                if not frame_evidence or frame_evidence[-1][0] is not value or frame_evidence[-1][1][5]:
                    raise HybridExecutionError("invalid_callback", "native request already issued its callback checkpoint")
                frame_evidence = frame_evidence[:-1] + ((value, frame_evidence[-1][1][:5] + (True,)),)
                project()
            elif operation == "pop":
                if not frame_evidence or frame_evidence[-1][0] is not value:
                    raise RuntimeError("native frame cleanup is not the current issued identity")
                frame_evidence = frame_evidence[:-1]
                project()
            elif operation != "check":
                raise RuntimeError("unknown nested accounting operation")
        if mismatch is not None:
            raise mismatch
    return dispatch


def _positive_limit(value: int, maximum: int, label: str) -> int:
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact integer")
    if not 1 <= value <= maximum:
        raise ValueError(f"{label} must be in 1..{maximum}")
    return value


def _alias(first: Any, second: Any, label: str) -> Any:
    if first is not None and second is not None:
        raise TypeError(f"specify only one {label} argument")
    return first if first is not None else second


def _error_detail(error: BaseException) -> str:
    try:
        detail = str(error)
    except BaseException:
        detail = "error text unavailable"
    return f"{type(error).__name__}: {detail}"


def _overlap(base: int, size: int, other_base: int, other_size: int) -> bool:
    return bool(size and other_size and
                base < other_base + other_size and other_base < base + size)


def _merge_spans(spans: list[tuple[int, int]]) -> tuple[tuple[int, int], ...]:
    merged: list[tuple[int, int]] = []
    for base, size in sorted(spans):
        if not size:
            continue
        if merged and base <= merged[-1][0] + merged[-1][1]:
            start, previous = merged[-1]
            merged[-1] = (start, max(start + previous, base + size) - start)
        else:
            merged.append((base, size))
    return tuple(merged)


class HybridRuntime:
    """Own one shared image and its sealed integer-routine declarations.

    ``semantic`` is the actual MegaForthRuntime, including its original stack,
    dictionary, native planner, timers, and services. Raw semantic entry remains
    safe: every registered callback enforces its outer meter's machine budget.
    """

    @classmethod
    def create(
        cls, *, executor: str | None = None, semantic_executor: str | None = None,
        memory: SparseAddressSpace | None = None, geometry: dict | None = None,
        dispatch_instruction_limit: int = MAX_DISPATCH_INSTRUCTIONS,
        dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS,
        dispatch_callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
        require_nested_callbacks: bool = False,
        require_service_callbacks: bool = False,
        **runtime_kwargs: Any,
    ) -> HybridRuntime:
        if type(require_service_callbacks) is not bool:
            raise TypeError("require_service_callbacks must be an exact boolean")
        if require_service_callbacks and cls is not HybridRuntime:
            raise RuntimeError("hybrid scalar callbacks require a canonical qualified owner")
        if type(require_nested_callbacks) is not bool:
            raise TypeError("require_nested_callbacks must be an exact boolean")
        if require_nested_callbacks and (cls is not HybridRuntime or not cls._supports_nested_semantics()):
            raise RuntimeError("hybrid nested callbacks require fully qualified semantic V4 and native V3")
        limit = _positive_limit(dispatch_instruction_limit,
                                MAX_DISPATCH_INSTRUCTIONS, "dispatch instruction limit")
        callback_limit = _positive_limit(
            dispatch_callback_limit, MAX_DISPATCH_CALLBACKS, "dispatch callback limit"
        )
        callback_semantic_limit = _positive_limit(
            dispatch_callback_semantic_limit, MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
            "dispatch callback semantic limit",
        )
        if executor is not None and semantic_executor is not None and executor != semantic_executor:
            raise ValueError("executor and semantic_executor disagree")
        selected = executor if executor is not None else semantic_executor
        if selected is None:
            selected = os.environ.get("MEGAFORTH_EXECUTOR", "python")
        if selected not in ("python", "native", "auto"):
            raise ValueError("semantic executor must be python, native, or auto")
        if "execution_backend" in runtime_kwargs:
            raise TypeError("use executor or semantic_executor, not execution_backend")
        if memory is not None and geometry is not None:
            raise TypeError("specify memory or geometry, not both")
        if geometry is not None and type(geometry) is not dict:
            raise TypeError("geometry must be a dictionary")
        if geometry is not None and "dense_backing" in geometry:
            raise TypeError("hybrid geometry always uses fixed dense backing")
        if memory is not None and (
            type(memory) is not SparseAddressSpace or memory.dense_backing is None
        ):
            raise ValueError("hybrid memory must be an exact all-dense SparseAddressSpace")
        try:
            import _mp64_accel as native
        except ImportError as exc:
            raise RuntimeError("hybrid execution requires _mp64_accel; run make build") from exc
        if (getattr(native, "HYBRID_ROUTINE_ABI_ID", None) != HYBRID_ABI
                or getattr(native, "HYBRID_ROUTINE_ABI_VERSION", None) != HYBRID_ABI_VERSION):
            raise RuntimeError("hybrid execution requires a matching _mp64_accel; run make build")
        if require_nested_callbacks and not cls._supports_nested_execution(native):
            raise RuntimeError("hybrid nested callbacks require fully qualified semantic V4 and native V3")
        if require_service_callbacks and not cls._supports_callbacks(native):
            raise RuntimeError("hybrid scalar callbacks require qualified native transport V2")
        if memory is None:
            memory = create_one_core_address_space(dense_backing=True, **(geometry or {}))
        # Validate and pin architectural geometry before the semantic runtime
        # claims a caller-owned storage service or publishes its BIOS words.
        machine = cls._prepare_machine(memory, native)
        try:
            semantic = MegaForthRuntime(memory=memory, execution_backend=selected, **runtime_kwargs)
        except BaseException:
            machine[1].close()
            machine = None
            raise
        try:
            owner = cls(semantic, native, limit, machine, callback_limit, callback_semantic_limit)
            if require_nested_callbacks and not owner.nested_callback_abi_available:
                owner.close()
                raise RuntimeError("hybrid nested callbacks require fully qualified semantic V4 and native V3")
            if require_service_callbacks and not owner.service_callback_abi_available:
                owner.close()
                raise RuntimeError("hybrid scalar callbacks require a canonical qualified service owner")
            return owner
        except BaseException:
            machine[1].close()
            raise

    def __init__(self, semantic: MegaForthRuntime, native: Any, limit: int,
                 machine: tuple, callback_limit: int = MAX_DISPATCH_CALLBACKS,
                 callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS) -> None:
        self.semantic = semantic
        self._native = native
        self._dispatch_instruction_limit = limit
        self._dispatch_callback_limit = callback_limit
        self._dispatch_callback_semantic_limit = callback_semantic_limit
        self._executor = semantic.execution_backend
        self._session_nonce = object()
        self._closed = False
        self._registration_failure: str | None = None
        self._active_machine = False
        self._registrations: dict[int, _Registration] = {}
        self._by_nonce: dict[object, _Registration] = {}
        self._issued_code_bytes = 0
        self._control_used = 0
        self._issued_child_edges = 0
        self._machine_instructions = 0
        self._machine_cycles = 0
        self._transitions = 0
        self._callback_requests = 0
        self._callback_semantic_steps = 0
        self._machine_segments = 0
        self._v2_segment_id = 0
        self._v2_invocation_id = 0
        self._v3_segment_id = 0
        self._max_machine_depth = 0
        self._nested_execution = None
        self._task_adapter = None
        self._task_accounting = None
        self._task_cleanup = None
        self._task_authority = None
        self._allowances: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._stack_allocations: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._wrapper_limits: list[tuple[int, int, int]] = []
        self._cpu, self._runner, self._control_base, self._control_buffer, self._nested_runner = machine
        self._callbacks_available = self._supports_callbacks(native)
        self._service_native_authority = None
        if self._callbacks_available and type(self._runner) is native.RoutineRunnerV2:
            methods = tuple(getattr(self._runner, name) for name in (
                "begin_v2", "resume_callback", "last_segment_v2", "cancel_invocation",
            ))
            if all(_native_bound_route(method, self._runner) for method in methods):
                native_classes = tuple(dict.fromkeys(
                    base for cls in (native.RoutineRunnerV2, native.RoutineSegmentResultV2,
                                     native.RoutineSegmentReceiptV2, native.RoutineCallbackRequestV2)
                    for base in cls.__mro__ if base is not object
                ))
                classes = tuple((cls, tuple(vars(cls).items())) for cls in native_classes)
                self._service_native_authority = (self._runner, methods, classes)
        self._dictionary = semantic.dictionary
        self._memory = semantic.memory
        previous_guard = self._dictionary._mutation_guard
        owner = weakref.ref(self)

        def mutation_guard(operation: str) -> None:
            if previous_guard is not None:
                previous_guard(operation)
            current = owner()
            if current is not None and current._active_machine:
                raise HybridExecutionError(
                    "active_dispatch", "dictionary mutation is forbidden during a machine invocation"
                )

        self._dictionary_guard = mutation_guard
        self._dictionary._mutation_guard = mutation_guard
        self._remember_context(semantic.main_context)
        # Legacy embedders may subclass this owner. Only the new nested
        # profile requires the exact canonical owner and its pinned methods.
        if type(self) is HybridRuntime:
            semantic._callback_exports._install_nested_owner(self)

    def _capture_nested_target(self, word):
        """Resolve only this owner's exact explicitly V4 registration."""
        if type(self._registrations) is not dict:
            raise HybridExecutionError("stale_registration", "machine registration table changed")
        registration = self._registrations.get(id(word))
        if type(registration) is not _Registration or registration.word is not word:
            raise HybridExecutionError("stale_registration", "machine target is not registered here")
        from simulator.interop_nested import CapturedMachineTarget
        return CapturedMachineTarget.create(self, registration)

    def _verify_nested_target(self, target):
        from simulator.interop_nested import CapturedMachineTarget
        if (type(target) is not CapturedMachineTarget
                or type(target.registration) is not _Registration
                or type(target.registration.declaration) is not RoutineDeclarationV4):
            raise HybridExecutionError("invalid_child", "nested targets require an explicit V4 registration")
        target.verify_owner(self)

    def _require_nested_execution(self):
        chain = self.semantic._callback_exports._nested_chain
        if chain is not None and chain.adapter._composition_binding is not None:
            chain.adapter.composition_for(chain)("check")
        elif self._nested_execution is not None:
            raise HybridExecutionError("callback_accounting", "nested execution lost its issued ledger")

    def _admit_nested_callback(self, handle, invocation_id, arguments, begin=False):
        self.semantic._callback_exports._nested_owner.require(self)
        state = self._nested_execution
        if state is None or not state.frames or not self._active_machine:
            raise HybridExecutionError("invalid_callback", "no owned native callback is pending")
        frame = state.frames[-1]
        raw = frame.raw
        if (type(raw) is not self._native.RoutineSegmentResultV3
                or raw.exit_kind != "callback_request" or raw.token is None
                or type(invocation_id) is not int or invocation_id != raw.invocation_id
                or type(arguments) is not tuple or any(type(value) is not int for value in arguments)):
            raise HybridExecutionError("invalid_callback", "callback has no exact pending native request")
        callback = raw.callback
        if (type(callback) is not self._native.RoutineCallbackRequestV3
                or callback.site_index >= len(frame.registration.exports)):
            raise HybridExecutionError("invalid_callback", "native callback site is unavailable")
        site, issued = frame.registration.exports[callback.site_index]
        if (issued is not handle or arguments != tuple(callback.arguments)
                or callback.invocation_id != raw.invocation_id
                or callback.call_offset != site.call_offset or callback.stub_offset != site.stub_offset
                or callback.export_id != site.export.export_id):
            raise HybridExecutionError("invalid_callback", "callback differs from its declared native site")
        self._validate_registration(frame.registration)
        if type(begin) is not bool:
            raise TypeError("callback checkpoint admission flag must be exact")
        if begin:
            chain = self.semantic._callback_exports._nested_chain
            chain.adapter.composition_for(chain)("begin", frame)
        return frame.via_use

    def _nested_child_edge(self, checkpoint, captured_call):
        state = self._nested_execution
        if state is None or not state.frames:
            raise HybridExecutionError("invalid_child", "child Call has no active native parent")
        engine = self.semantic._callback_exports
        record = engine._require_nested_chain(state.chain_token)._record(checkpoint)
        frame = state.frames[-1]
        raw = frame.raw
        self._admit_nested_callback(record.handle, record.invocation_id, tuple(raw.callback.arguments))
        matches = [edge for site, call, target, edge in frame.registration.child_edges
                   if site == raw.callback.site_index and call is captured_call]
        if len(matches) != 1:
            raise HybridExecutionError("invalid_child", "Call is not issued at the current callback site")
        return matches[0]

    def _invoke_nested_child(self, use, arguments):
        state = self._nested_execution
        if state is None or not state.frames:
            raise HybridExecutionError("invalid_child", "child entry has no owned root invocation")
        chain = self.semantic._callback_exports._require_nested_chain(state.chain_token)
        captured, edge = chain.consume_machine_use(use)
        registration = captured.target.registration
        parent = state.frames[-1]
        if len(state.frames) >= 8 or any(frame.registration is registration for frame in state.frames):
            raise HybridExecutionError("invalid_child", "children require at most eight distinct registrations")
        self._verify_nested_target(captured.target)
        active = self.semantic._callback_exports._nested_dispatches[-1]
        protected = self._protected_spans(active.context)
        spans = self._nested_borrows(registration, arguments, protected, parent.spans)
        frame = _NestedMachineFrame(registration, spans, protected, use)
        result = self._drive_nested_frame(
            frame, self._nested_runner.begin_child_v3, parent.raw.token, edge, arguments, spans,
            protected_spans=protected,
        )
        if result.exit_kind != "returned":
            raise HybridExecutionError(result.exit_kind.value, result.detail, result=result)
        if len(result.outputs) != registration.declaration.output_cells:
            raise HybridExecutionError("invalid_return", "child output arity changed", result=result)
        return result.outputs

    def _nested_borrows(self, registration, arguments, protected, parent_spans=None):
        declaration = registration.declaration
        if (type(arguments) is not tuple or len(arguments) != declaration.input_cells
                or any(type(value) is not int or not 0 <= value <= MASK64 for value in arguments)):
            raise HybridExecutionError("invalid_entry", "machine arguments differ from their declared signature")
        spans = []
        try:
            for rule in declaration.buffers:
                span = rule.resolve(arguments)
                if span.size:
                    if not any(region.base <= span.base and span.limit <= region.limit
                               for region in self._memory.regions):
                        raise ValueError("borrowed buffer must fit one ordinary shared region")
                    if any(_overlap(span.base, span.size, base, size) for base, size in protected):
                        raise ValueError("borrowed buffer overlaps protected stack, code, or header bytes")
                    if parent_spans is not None and not any(
                        base <= span.base and span.limit <= base + size
                        and (access == "read_write" or access == span.access.value)
                        for base, size, access in parent_spans
                    ):
                        raise ValueError("child borrow must fit one immediate parent grant without escalation")
                spans.append((span.base, span.size, span.access.value))
        except (TypeError, ValueError) as error:
            raise HybridExecutionError("rejected_access", str(error)) from error
        return tuple(spans)

    def _settle_nested_receipt(self):
        receipt = self._nested_runner.last_segment_v3()
        if receipt is not None and type(receipt) is not self._native.RoutineSegmentReceiptV3:
            raise RuntimeError("nested native accounting returned a foreign receipt")
        chain = self.semantic._callback_exports._nested_chain
        chain.adapter.composition_for(chain, cleanup=True)("native", receipt)

    def _nested_native_boundary(self, operation, *args, **kwargs):
        before_segment = self._v3_segment_id
        try:
            raw = operation(*args, **kwargs)
        except BaseException as error:
            try:
                self._settle_nested_receipt()
            except BaseException as cleanup:
                self._registration_cleanup_error(error, cleanup)
            raise
        try:
            self._settle_nested_receipt()
            self._require_open()
            if (type(raw) is not self._native.RoutineSegmentResultV3
                    or raw.segment_id != self._v3_segment_id or raw.segment_id != before_segment + 1):
                raise RuntimeError("nested native result differs from its accounting receipt")
        except BaseException as error:
            self._registration_failure = f"nested native accounting failed: {_error_detail(error)}"
            raise
        return raw

    def _settle_nested_semantics(self, receipt):
        chain = self.semantic._callback_exports._nested_chain
        chain.adapter.composition_for(chain, cleanup=True)("semantic", receipt)

    def _nested_callback_boundary(self, handle, request):
        from simulator.interop_nested import NestedCallbackReceipt
        engine = self.semantic._callback_exports
        state = self._nested_execution
        consume = engine._consume_nested_callback
        checkpoint = engine._begin_nested_callback(
            state.chain_token, handle, request.invocation_id, request.arguments,
        )
        def settle():
            receipt = consume(state.chain_token, checkpoint)
            if type(receipt) is not NestedCallbackReceipt:
                raise RuntimeError("nested callback returned a foreign accounting receipt")
            self._settle_nested_semantics(receipt)
            if (type(receipt.entered) is not bool or type(receipt.completed) is not bool
                    or type(receipt.inclusive_semantic_steps) is not int
                    or not 0 <= receipt.inclusive_semantic_steps <= request.site.export.max_semantic_steps
                    or (not receipt.entered and (receipt.completed or receipt.inclusive_semantic_steps))):
                raise RuntimeError("nested callback receipt is inconsistent")
            return receipt
        try:
            result = engine._invoke_nested_callback(checkpoint, handle, request.arguments)
        except BaseException as error:
            try:
                settle()
            except BaseException as cleanup:
                self._registration_cleanup_error(error, cleanup)
            raise
        try:
            receipt = settle()
            if (not receipt.entered or not receipt.completed or type(result) is not CallbackExportResult
                    or result.semantic_steps != receipt.inclusive_semantic_steps):
                raise RuntimeError("nested callback has no completed issued invocation")
        except BaseException as error:
            self._registration_failure = f"nested callback accounting failed: {_error_detail(error)}"
            raise
        return result

    def _drive_nested_frame(self, frame, operation, *arguments, **kwargs):
        state = self._nested_execution
        chain = self.semantic._callback_exports._nested_chain
        accounting = chain.adapter.composition_for(chain)
        accounting("push", frame)
        original = None
        try:
            raw = self._nested_native_boundary(operation, *arguments, **kwargs)
            while True:
                accounting("raw", (frame, raw))
                request = None
                handle = None
                if raw.exit_kind == "callback_request":
                    callback = raw.callback
                    if not 0 <= callback.site_index < len(frame.registration.exports):
                        raise HybridExecutionError("invalid_callback", "native callback site is undeclared")
                    site, handle = frame.registration.exports[callback.site_index]
                    request = CallbackRequestV4(invocation_id=callback.invocation_id,
                                                sequence=callback.sequence, site=site,
                                                arguments=tuple(callback.arguments))
                    self._admit_nested_callback(handle, raw.invocation_id, request.arguments)
                result = MachineSegmentResultV4(
                    **self._result_fields(raw), invocation_id=raw.invocation_id,
                    invocation_instructions=raw.invocation_instructions,
                    invocation_cycles=raw.invocation_cycles, callback=request,
                    segment_id=raw.segment_id, root_invocation_id=raw.root_invocation_id,
                    parent_invocation_id=raw.parent_invocation_id, depth=raw.depth,
                    invocation_started=raw.invocation_started,
                    chain_instructions=raw.chain_instructions, chain_cycles=raw.chain_cycles,
                )
                if result.exit_kind != "callback_request":
                    return result
                if (state.allowance.instructions >= state.allowance.limit
                        or raw.invocation_instructions >= frame.registration.declaration.max_instructions):
                    raise HybridExecutionError("instruction_limit", "machine allowance exhausted before callback",
                                               result=result)
                if state.allowance.callback_semantic_steps >= state.allowance.callback_semantic_limit:
                    raise HybridExecutionError("callback_semantic_limit", "outer semantic callback allowance exhausted",
                                               result=result)
                try:
                    callback_result = self._nested_callback_boundary(handle, request)
                except CallbackExportBudgetExceeded as error:
                    if (self._registration_failure is None
                            and self.semantic.consume_callback_budget_failure(error)):
                        raise HybridExecutionError(error.reason, str(error), result=result) from error
                    raise
                self._validate_registration(frame.registration)
                raw = self._nested_native_boundary(self._nested_runner.resume_callback_v3,
                                                   raw.token, callback_result.outputs)
        except BaseException as error:
            original = error
            raise
        finally:
            try:
                accounting("pop", frame)
            except BaseException as cleanup:
                if original is None:
                    raise
                self._registration_cleanup_error(original, cleanup)

    def _invoke_published_nested(self, word, context):
        """Internal core; public source admission remains capability-gated."""
        with self.semantic._session_owner_lock:
            self._require_open()
            if self._active_machine or self._nested_execution is not None:
                raise HybridExecutionError("active_dispatch", "root machine entry is not reentrant")
            registration = self._registrations.get(id(word))
            if (registration is None or registration.word is not word
                    or type(registration.declaration) is not RoutineDeclarationV4):
                raise HybridExecutionError("stale_registration", "root requires an issued V4 registration")
            self._validate_registration(registration)
            meter = self._current_meter()
            if meter is None:
                raise HybridExecutionError("invalid_entry", "root requires the original semantic dispatch")
            if self._task_adapter is not None:
                self.semantic._foreign_tasks.claim_machine_profile(meter, "private")
            allowance = self._allowance(meter)
            remaining = allowance.limit - allowance.instructions
            if remaining <= 0:
                raise HybridExecutionError("instruction_limit", "outer machine allowance exhausted")
            declaration = registration.declaration
            arguments = tuple(context.data.peek(index) for index in reversed(range(declaration.input_cells)))
            context.data.require_push_capacity(max(0, declaration.output_cells - declaration.input_cells))
            protected = self._protected_spans(context)
            spans = self._nested_borrows(registration, arguments, protected)
            engine = self.semantic._callback_exports
            semantic_limit = allowance.callback_semantic_limit - allowance.callback_semantic_steps
            finish_chain = engine._finish_nested_chain
            adapter = engine._nested_owner
            release_composition = adapter.release_composition
            bind_composition = adapter.bind_composition
            runner = self._nested_runner
            last_native_receipt = runner.last_segment_v3
            cancel_native = runner.cancel_chain_v3
            state = _NestedExecution(None, meter, allowance, semantic_limit)
            accounting = None
            chain = None
            original = None
            chain_token = engine._begin_nested_chain(self, meter, semantic_limit)
            try:
                chain = engine._nested_chain
                state.chain_token = chain_token
                self._nested_execution, self._active_machine = state, True
                accounting = bind_composition(chain, state)
                frame = _NestedMachineFrame(registration, spans, protected)
                result = self._drive_nested_frame(
                    frame, self._nested_runner.begin_root_v3, registration.spec, arguments, spans, remaining,
                    callback_limit=max(0, allowance.callback_limit - allowance.callback_requests),
                    protected_spans=protected,
                )
                if result.exit_kind != "returned":
                    raise HybridExecutionError(result.exit_kind.value, result.detail, result=result)
                if len(result.outputs) != declaration.output_cells:
                    raise HybridExecutionError("invalid_return", "root output arity changed", result=result)
                receipt = finish_chain(chain_token)
                accounting("semantic", receipt)
            except BaseException as error:
                original = error
                # Recovery uses the original owner/query and local authority,
                # including failures before the next snapshot was allocated.
                if accounting is not None:
                    try:
                        receipt = last_native_receipt()
                        if receipt is not None and type(receipt) is not self._native.RoutineSegmentReceiptV3:
                            raise RuntimeError("nested native recovery returned a foreign receipt")
                        accounting("native", receipt)
                    except BaseException as cleanup:
                        self._registration_cleanup_error(error, cleanup)
                try:
                    cancel_native()
                except BaseException as cleanup:
                    self._registration_cleanup_error(error, cleanup)
                try:
                    if engine._nested_chain is not None:
                        receipt = finish_chain(chain_token, cancelled=True)
                        if accounting is not None:
                            accounting("semantic", receipt)
                except BaseException as cleanup:
                    self._registration_cleanup_error(error, cleanup)
                raise
            finally:
                try:
                    if chain is not None:
                        release_composition(chain)
                except BaseException as cleanup:
                    if original is None:
                        raise
                    self._registration_cleanup_error(original, cleanup)
                finally:
                    self._nested_execution, self._active_machine = None, False
            for _ in range(declaration.input_cells):
                context.data.pop()
            for cell in result.outputs:
                context.data.push(cell)

    @staticmethod
    def _supports_callbacks(native: Any) -> bool:
        runner = getattr(native, "RoutineRunnerV2", None)
        return (
            getattr(native, "HYBRID_CALLBACK_ABI_VERSION", None) == HYBRID_CALLBACK_ABI_VERSION
            and callable(runner) and callable(getattr(native, "RoutineSpecV2", None))
            and all(callable(getattr(runner, name, None)) for name in (
                "publish_code_v2", "is_code_published_v2", "revoke_code_v2",
                "begin_v2", "resume_callback", "cancel_invocation", "last_segment_v2",
            ))
        )

    @staticmethod
    def _supports_nested_publication(native: Any) -> bool:
        runner = getattr(native, "RoutineRunnerV3", None)
        return (
            HybridRuntime._supports_callbacks(native)
            and callable(runner) and callable(getattr(native, "RoutineSpecV3", None))
            and all(callable(getattr(runner, name, None)) for name in (
                "legacy_v2", "publish_code_v3", "revoke_code_v3", "is_code_published_v3", "close",
            ))
        )

    @staticmethod
    def _supports_nested_semantics() -> bool:
        version = getattr(MegaForthRuntime, "NESTED_CALLBACK_ABI_VERSION", None)
        return (
            type(version) is int and version == 4
            and all(callable(getattr(CallbackExportEngine, name, None)) for name in (
                "_install_nested_owner", "_begin_nested_chain", "_finish_nested_chain",
                "_begin_nested_callback", "_invoke_nested_callback", "_consume_nested_callback",
                "_nested_private_context", "consume_budget_failure",
            ))
        )

    @staticmethod
    def _supports_nested_execution(native: Any) -> bool:
        version = getattr(native, "HYBRID_NESTED_ROUTINE_ABI_VERSION", None)
        profile = getattr(native, "HYBRID_NESTED_ROUTINE_CAPABILITY", None)
        depth = getattr(native, "HYBRID_NESTED_ROUTINE_MAX_DEPTH", None)
        runner = getattr(native, "RoutineRunnerV3", None)
        return (
            type(version) is int and version == 3
            and type(profile) is str and profile == "distinct_registration_children"
            and type(depth) is int and depth == 8
            and HybridRuntime._supports_nested_publication(native)
            and all(callable(getattr(runner, name, None)) for name in (
                "begin_root_v3", "begin_child_v3", "resume_callback_v3", "cancel_chain_v3", "last_segment_v3",
            ))
            and all(callable(getattr(native, name, None)) for name in (
                "RoutineSegmentResultV3", "RoutineSegmentReceiptV3", "RoutineCallbackRequestV3",
            ))
        )

    @staticmethod
    def _prepare_machine(memory: SparseAddressSpace, native: Any) -> tuple:
        # This logical span is deliberately absent from semantic memory and
        # SysInfo. It owns native CALL/RET control cells, never shared data.
        regions = memory.regions
        occupied = [(region.base, region.size) for region in regions]
        occupied.append((MMIO_BASE, MMIO_LIMIT - MMIO_BASE))
        external = next((region for region in regions
                         if region.kind is AddressClass.EXTERNAL), None)
        candidates = ([external.limit] if external is not None else [])
        candidates += [base + size for base, size in occupied] + [0]
        control_base = None
        for candidate in candidates:
            base = (candidate + CELL_BYTES - 1) & -CELL_BYTES
            if base + MAX_CONTROL_BYTES > MASK64:
                continue
            if not any(_overlap(base, MAX_CONTROL_BYTES, start, size)
                       for start, size in occupied):
                control_base = base
                break
        if control_base is None:
            raise ValueError("ordinary memory leaves no aligned private control arena")
        control_buffer = bytearray(MAX_CONTROL_BYTES)
        cpu = native.CPUState()
        backing = memory.dense_backing
        assert backing is not None
        for region in regions:
            buffer = backing.buffer_at(region.base)
            if region.kind is AddressClass.BANK0:
                cpu.attach_mem(buffer, region.size)
            elif region.kind is AddressClass.EXTERNAL:
                cpu.attach_ext_mem(buffer, region.base, region.size)
            elif region.kind is AddressClass.HBW:
                cpu.attach_hbw_mem(buffer, region.base, region.size)
            elif region.kind is AddressClass.VRAM:
                cpu.attach_vram(buffer, region.base, region.size)
            else:
                raise ValueError("unsupported hybrid ordinary region")
        nested = None
        if HybridRuntime._supports_nested_publication(native):
            nested = native.RoutineRunnerV3(cpu, control_base, control_buffer)
            try:
                runner = nested.legacy_v2()
            except BaseException as error:
                try:
                    nested.close()
                except BaseException as cleanup:
                    BaseException.add_note(error, f"native owner cleanup failed: {_error_detail(cleanup)}")
                raise
        else:
            runner_type = (native.RoutineRunnerV2 if HybridRuntime._supports_callbacks(native)
                           else native.RoutineRunnerV1)
            runner = runner_type(cpu, control_base, control_buffer)
        return cpu, runner, control_base, control_buffer, nested

    @property
    def executor(self) -> str:
        """The selected semantic executor; architectural execution is native."""
        return self._executor

    @property
    def dispatch_instruction_limit(self) -> int:
        return self._dispatch_instruction_limit

    @property
    def dispatch_callback_limit(self) -> int:
        return self._dispatch_callback_limit

    @property
    def dispatch_callback_semantic_limit(self) -> int:
        return self._dispatch_callback_semantic_limit

    @property
    def callback_abi_available(self) -> bool:
        """Whether this owner's native runner supports callback ABI v2."""
        return self._callbacks_available

    @property
    def closed_callback_abi_available(self) -> bool:
        """Whether this owner admits V3 closed policies over the V2 transport."""
        version = getattr(self.semantic, "callback_export_abi_version", None)
        return (
            self._callbacks_available
            and type(version) is int and version in (HYBRID_CLOSED_ABI_VERSION, 4)
            and callable(getattr(self.semantic, "inspect_callback_export", None))
            and callable(getattr(self.semantic, "callback_policy_core_xt", None))
            and callable(getattr(self.semantic, "consume_callback_budget_failure", None))
            and callable(getattr(self.semantic, "begin_closed_callback_accounting", None))
            and callable(getattr(self.semantic, "consume_closed_callback_accounting", None))
        )

    @property
    def nested_callback_abi_available(self) -> bool:
        """Exact qualified semantic profile and distinct-child native transport."""
        version = getattr(self.semantic, "callback_export_abi_version", None)
        return (
            type(self) is HybridRuntime and type(self.semantic) is MegaForthRuntime
            and type(version) is int and version == 4
            and self._supports_nested_semantics() and self._supports_nested_execution(self._native)
            and type(self._nested_runner) is self._native.RoutineRunnerV3
            and type(self.semantic._callback_exports) is CallbackExportEngine
            and self.semantic._callback_exports._nested_owner is not None
        )

    @property
    def service_callback_abi_available(self) -> bool:
        """Qualified private scalar services on this exact native owner."""
        if type(self) is not HybridRuntime or not self._callbacks_available:
            return False
        try:
            self._require_service_profile()
        except (RuntimeError, CallbackExportError):
            return False
        return True

    @property
    def service_callback_value_executor(self) -> str | None:
        if not self.service_callback_abi_available:
            return None
        return self._require_service_profile().value_executor

    def _service_native_routes(self):
        authority = self._service_native_authority
        if (type(authority) is not tuple or len(authority) != 3
                or authority[0] is not self._runner
                or type(self._runner) is not self._native.RoutineRunnerV2):
            raise RuntimeError("scalar service execution requires its original exact native facade")
        runner, methods, classes = authority
        if (any(not _native_bound_route(method, runner) for method in methods)
                or any(len(vars(cls)) != len(fields)
                       or any(vars(cls).get(name) is not value for name, value in fields)
                       for cls, fields in classes)):
            raise RuntimeError("scalar service native facade or result routes changed")
        return authority

    def _require_service_profile(self) -> ServiceCallbackProfileV5:
        self._service_native_routes()
        profile = service_callback_profile(self.semantic)
        if (type(self) is not HybridRuntime or not self._callbacks_available
                or type(profile) is not ServiceCallbackProfileV5
                or type(profile.version) is not int or profile.version != 5
                or type(profile.capability) is not str or profile.capability != SERVICE_CAPABILITY_V5
                or type(profile.effect) is not str or profile.effect != SERVICE_EFFECT_V5
                or type(profile.value_executor) is not str
                or profile.value_executor not in ("python_reference", "shared_native_kernel")):
            raise RuntimeError("hybrid scalar service callbacks require a canonical finalized owner and native V2")
        return profile

    @property
    def max_machine_depth(self) -> int:
        return self._max_machine_depth

    @property
    def callback_requests(self) -> int:
        return self._callback_requests

    @property
    def callback_semantic_steps(self) -> int:
        return self._callback_semantic_steps

    @property
    def machine_segments(self) -> int:
        return self._machine_segments

    @property
    def machine_instructions(self) -> int:
        return self._machine_instructions

    @property
    def machine_cycles(self) -> int:
        return self._machine_cycles

    @property
    def transitions(self) -> int:
        return self._transitions

    @property
    def closed(self) -> bool:
        return self._closed

    def _require_open(self) -> None:
        if self._closed:
            raise HybridExecutionError("closed", "the hybrid owner has been closed")
        if self._registration_failure is not None:
            raise HybridExecutionError("registration_cleanup", self._registration_failure)

    def _install_task_adapter(self, adapter) -> None:
        from hybrid.task_adapter import NativeTaskAdapter

        self._require_open()
        self._require_authority()
        self.semantic._foreign_tasks._require_idle("install a native task adapter")
        if (type(self) is not HybridRuntime or type(adapter) is not NativeTaskAdapter
                or adapter._owner is not self or adapter._semantic is not self.semantic
                or self._nested_runner is None
                or adapter._runner is not self._nested_runner.task_v1()):
            raise HybridExecutionError("invalid_entry", "task adapter does not own this native facade")
        if self._task_adapter is not None:
            raise HybridExecutionError("invalid_entry", "this owner already has its task adapter")
        accounting = _task_accounting_authority(self, adapter)
        authority = _TaskOwnerAuthority(self, adapter, accounting, adapter._native_cleanup,
                                        adapter._native_closed, adapter._engine)
        owner_id = id(self)
        table = _TASK_OWNER_AUTHORITIES

        def retire(owned):
            current = table.get(owner_id)
            if current is not None and current[0] is owned:
                table.pop(owner_id, None)

        _TASK_OWNER_AUTHORITIES[owner_id] = (weakref.ref(self, retire), weakref.ref(authority))
        adapter._composition_authority = authority
        self._task_cleanup = adapter._native_cleanup
        self._task_accounting = accounting
        self._task_authority = authority
        self._task_adapter = adapter

    def _require_task_adapter(self, adapter) -> None:
        self._require_open()
        self._require_authority()
        _authority, changed = _task_owner_authority(self, adapter)
        if changed:
            self._registration_failure = "task installation authority changed"
            raise HybridExecutionError("callback_accounting", self._registration_failure)
        if (self._task_adapter is not adapter or adapter._owner is not self
                or adapter._semantic is not self.semantic
                or adapter._engine is not self.semantic._foreign_tasks
                or self._nested_runner is None
                or adapter._runner is not self._nested_runner.task_v1()):
            raise HybridExecutionError("stale_registration", "native task adapter ownership changed")
        if self._active_machine:
            raise HybridExecutionError("active_dispatch", "private machine invocation owns the CPU")
        self.semantic._require_session_owner_access("use the native task adapter")

    def _admit_task_root(self, adapter, root_token, root_id) -> None:
        self._require_task_adapter(adapter)
        engine = self.semantic._foreign_tasks
        engine.adapter_root_policy(adapter, root_token, root_id)
        meter = self._current_meter()
        if meter is None:
            raise HybridExecutionError("invalid_entry", "task entry has no original semantic meter")
        engine.claim_machine_profile(meter, "task")
        authority, _changed = _task_owner_authority(self, adapter)
        authority.accounting("begin", root_token, root_id)

    def _settle_task_receipt(self, adapter, receipt) -> None:
        # This path remains available for exact receipt recovery after a host
        # failure. Replaying the same receipt is projection, never another charge.
        try:
            authority, changed = _task_owner_authority(self, adapter)
            authority.accounting("settle", receipt)
            if changed:
                raise HybridExecutionError("callback_accounting", "task installation authority changed")
        except BaseException as error:
            self._registration_failure = "task native receipt accounting failed"
            raise

    def _settle_task_semantic_receipt(self, adapter, receipt) -> None:
        try:
            authority, changed = _task_owner_authority(self, adapter)
            authority.accounting("semantic", receipt)
            if changed:
                raise HybridExecutionError("callback_accounting", "task installation authority changed")
        except BaseException:
            self._registration_failure = "task semantic receipt accounting failed"
            raise

    def register_routine_v1(self, image: RoutineImageV1 | None = None,
                            **values: Any) -> Word:
        """Publish a sealed image as one ordinary semantic primitive word."""
        return self._register_routine(image, RoutineImageV1, values)

    def register_routine_v2(self, image: RoutineImageV2 | None = None,
                            **values: Any) -> Word:
        """Publish an integer image with explicit canonical semantic callbacks."""
        return self._register_routine(image, RoutineImageV2, values)

    def register_routine_v3(self, image: RoutineImageV3 | None = None,
                            **values: Any) -> Word:
        """Publish an integer image with admitted closed semantic policies."""
        return self._register_routine(image, RoutineImageV3, values)

    def register_routine_v4(self, image: RoutineImageV4 | None = None, **values: Any) -> Word:
        """Publish nested metadata only with the full executable capability."""
        if not self.nested_callback_abi_available:
            raise RuntimeError("hybrid nested callbacks require fully qualified semantic V4 and native V3")
        return self._publish_nested_routine(image, **values)

    def _publish_nested_routine(self, image: RoutineImageV4 | None = None, **values: Any) -> Word:
        """Transactional publication core; it grants no execution by itself."""
        return self._register_routine(image, RoutineImageV4, values)

    def register_routine_v5(self, image: RoutineImageV5 | None = None, **values: Any) -> Word:
        """Publish service metadata only with its full executable capability."""
        if not self.service_callback_abi_available:
            raise RuntimeError("hybrid scalar callbacks require a canonical qualified service owner")
        return self._publish_service_routine(image, **values)

    def _publish_service_routine(self, image: RoutineImageV5 | None = None, **values: Any) -> Word:
        """Permanent V5 publication core; the public Word remains gated."""
        self._require_service_profile()
        return self._register_routine(image, RoutineImageV5, values)

    def _invoke_published_service(self, word: Word, context: ExecutionContext) -> None:
        """Exercise the qualified lower boundary without advertising V5."""
        registration = self._registrations.get(id(word))
        if (registration is None or registration.word is not word
                or type(registration.declaration) is not RoutineDeclarationV5):
            raise HybridExecutionError("stale_registration", "word has no issued V5 registration")
        return self._invoke(registration.declaration.registration_nonce, context, _private_service=True)

    def _require_authority(self) -> None:
        if (self.semantic.dictionary is not self._dictionary
                or self._dictionary._mutation_guard is not self._dictionary_guard
                or self.semantic.memory is not self._memory):
            raise HybridExecutionError("stale_registration", "shared dictionary or memory owner changed")

    def _registration_cleanup_error(self, error: BaseException, cleanup: BaseException) -> None:
        self._registration_failure = (
            f"registration rollback failed: {_error_detail(cleanup)}"
        )
        try:
            BaseException.add_note(error, self._registration_failure)
        except BaseException:
            pass

    def _register_routine(self, image, image_type, values: dict[str, Any]) -> Word:
        with self.semantic._session_owner_lock:
            self._require_open()
            self._require_authority()
            self.semantic._require_session_owner_access("register a hybrid routine")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "register only at an idle host boundary")
            self.semantic._require_no_suspension("register a hybrid routine")
            if self.semantic._active_dispatches or self.semantic._active_input_states:
                raise HybridExecutionError("active_dispatch", "register only at an idle host boundary")
            if image is not None and values:
                raise TypeError(f"pass a {image_type.__name__} or its keyword fields, not both")
            if image is None:
                image = image_type(**values)
            if type(image) is not image_type:
                raise TypeError(f"registration requires a {image_type.__name__}")
            nested = image_type is RoutineImageV4
            callbacks = image_type in (RoutineImageV2, RoutineImageV3, RoutineImageV4, RoutineImageV5)
            if image_type is RoutineImageV5:
                RoutineImageV5.__post_init__(image)
                self._require_service_profile()
            if nested:
                RoutineImageV4.__post_init__(image)
                if (type(self) is not HybridRuntime or self._nested_runner is None
                        or self.semantic._callback_exports._nested_owner is None):
                    raise RuntimeError("nested publication requires the exact owner and native V3 publication surface")
                if any(type(item.declaration) is RoutineDeclarationV4
                       and item.declaration.routine_id == image.routine_id
                       for item in self._registrations.values()):
                    raise ValueError("a V4 routine ID is already registered in this owner")
            if image_type is RoutineImageV3 and not self.closed_callback_abi_available:
                raise RuntimeError(
                    "hybrid closed callbacks require semantic profile v3 and "
                    "a matching _mp64_accel v2; run make build"
                )
            if callbacks:
                if not self._callbacks_available:
                    raise RuntimeError("hybrid callbacks require a matching _mp64_accel v2; run make build")
                image = replace(image, callbacks=tuple(
                    replace(site, export=replace(site.export)) for site in image.callbacks
                ))
            if len(self._registrations) >= MAX_ROUTINES:
                raise ValueError("a hybrid session may issue at most 64 registrations")
            code = image.code + b"\x01" * (-len(image.code) % CODE_ALIGNMENT)
            if self._issued_code_bytes + len(code) > MAX_TOTAL_CODE_BYTES:
                raise ValueError("registered padded code exceeds 16 MiB")
            stack_size = image.return_stack_cells * CELL_BYTES
            if self._control_used + stack_size > MAX_CONTROL_BYTES:
                raise ValueError("private control arena is exhausted")
            dictionary = self.semantic.dictionary
            body_base = (dictionary.here + HEADER_FIXED_BYTES + len(image.name)
                         + SEMANTIC_CODE_SLOT_BYTES)
            prepad = -body_base % CODE_ALIGNMENT
            body = b"\x01" * prepad + code
            code_base = body_base + prepad
            stack_base = self._control_base + self._control_used
            rejection = self.semantic._dictionary_growth_rejection(
                dictionary.definition_size(image.name, initial_body=body),
                self.semantic.main_context,
            )
            if rejection is not None:
                raise ValueError(rejection)
            if nested:
                spec = self._native.RoutineSpecV3(
                    code_base, code, image.entry_offset, image.input_cells, image.output_cells,
                    stack_base, stack_size, image.max_instructions, image.max_callback_requests,
                    tuple((site.call_offset, site.stub_offset, site.export.export_id,
                           site.export.input_cells, site.export.output_cells) for site in image.callbacks),
                )
            elif callbacks:
                spec = self._native.RoutineSpecV2(
                    code_base, code, image.entry_offset, image.input_cells,
                    image.output_cells, stack_base, stack_size, image.max_instructions,
                    tuple((site.call_offset, site.stub_offset, site.export.export_id,
                           site.export.input_cells, site.export.output_cells)
                          for site in image.callbacks),
                )
            else:
                spec = self._native.RoutineSpecV1(
                    code_base, len(code), image.entry_offset, image.input_cells,
                    image.output_cells, stack_base, stack_size, image.max_instructions,
                )
            if not any(region.base <= code_base and code_base + len(code) <= region.limit
                       for region in self.semantic.memory.regions):
                raise ValueError("complete padded routine image must fit one ordinary region")
            registration_nonce = object()
            owner = weakref.ref(self)

            def invoke(context: ExecutionContext) -> None:
                current = owner()
                if current is None:
                    raise HybridExecutionError("closed", "hybrid registration owner no longer exists")
                current._invoke(registration_nonce, context)

            checkpoint = dictionary.checkpoint()
            host_escape = None
            child_rows = ()
            child_captures = ()
            nested_graph = None
            transaction = (self.semantic.callback_export_registration(
                tuple(site.export for site in image.callbacks)
            ) if callbacks else nullcontext(()))
            try:
                with transaction as handles:
                    if nested:
                        from simulator.interop_nested import capture_nested_graph
                        engine = self.semantic._callback_exports
                        roots = []
                        rows, captures = [], []
                        for site_index, (site, handle) in enumerate(zip(image.callbacks, handles)):
                            binding = engine._binding(handle)
                            entry = binding.leaf.word if binding.closed is None else binding.closed.entry.word
                            roots.append((site.export, entry))
                            for call_id, token in enumerate(() if binding.closed is None else binding.closed.calls):
                                captured = engine._nested_calls[token]
                                rows.append((site_index, call_id, captured.target.spec))
                                captures.append((site_index, token, captured.target))
                        child_rows, child_captures = tuple(rows), tuple(captures)
                        if self._issued_child_edges + len(child_rows) > MAX_CHILD_EDGES:
                            raise ValueError("registered child edges exceed 65536")
                        nested_graph = capture_nested_graph(engine, tuple(roots), root_machine=image.graph_node())
                    word = self.semantic.define_primitive(image.name, invoke, initial_body=body)
                    if callbacks:
                        host_escape = self.semantic._register_primitive_host_escape(
                            word.implementation, invoke
                        )
                    lease = dictionary.acquire_body_lease(word)
                    if lease.body_address != body_base or lease.body_limit != body_base + len(body):
                        raise HybridExecutionError("stale_registration", "body publication geometry changed")
                    generation = len(self._registrations) + 1
                    control = _ControlLease(self._session_nonce, generation, stack_base, stack_size)
                    declaration_type = {
                        RoutineImageV1: RoutineDeclarationV1,
                        RoutineImageV2: RoutineDeclarationV2,
                        RoutineImageV3: RoutineDeclarationV3,
                        RoutineImageV4: RoutineDeclarationV4,
                        RoutineImageV5: RoutineDeclarationV5,
                    }[image_type]
                    extra = dict(
                        callbacks=image.callbacks,
                        dispatch_callback_limit=self._dispatch_callback_limit,
                        dispatch_callback_semantic_limit=self._dispatch_callback_semantic_limit,
                    ) if callbacks else {}
                    if nested:
                        extra.update(routine_id=image.routine_id, max_callback_requests=image.max_callback_requests)
                    elif image_type is RoutineImageV5:
                        extra.update(max_callback_requests=image.max_callback_requests)
                    declaration = declaration_type(
                        name=image.name, session_nonce=self._session_nonce,
                        registration_nonce=registration_nonce, allocation_lease=lease,
                        allocation_generation=lease.allocation_serial,
                        control_lease=control, control_generation=generation,
                        body_base=body_base, body_size=len(body), code_base=code_base,
                        code=code, entry_offset=image.entry_offset,
                        input_cells=image.input_cells, output_cells=image.output_cells,
                        buffers=image.buffers, stack_base=stack_base,
                        return_stack_cells=image.return_stack_cells,
                        max_instructions=image.max_instructions,
                        dispatch_instruction_limit=self._dispatch_instruction_limit,
                        **extra,
                    )
                    registration = _Registration(
                        word, declaration, spec, word.implementation, invoke,
                        tuple(zip(image.callbacks, handles)) if callbacks else (),
                        (), nested_graph,
                    )
                    # Allocate the host tables before native publication. They
                    # become visible only after the export transaction commits.
                    registrations = {**self._registrations, id(word): registration}
                    by_nonce = {**self._by_nonce, registration_nonce: registration}
                    if nested:
                        edges = self._nested_runner.publish_code_v3(spec, child_rows)
                        if len(edges) != len(child_captures):
                            raise RuntimeError("native child edge publication shape changed")
                        registration = replace(registration, child_edges=tuple(
                            (*captured, edge) for captured, edge in zip(child_captures, edges)
                        ))
                        registrations[id(word)] = registration
                        by_nonce[registration_nonce] = registration
                    elif callbacks:
                        self._runner.publish_code_v2(spec)
                    else:
                        self._runner.publish_code(spec)
            except BaseException as error:
                if callbacks:
                    try:
                        # A host wrapper may forward publication then raise;
                        # query exact native identity before revoking it.
                        if nested and self._nested_runner.is_code_published_v3(spec):
                            self._nested_runner.revoke_code_v3(spec)
                        elif not nested and self._runner.is_code_published_v2(spec):
                            self._runner.revoke_code_v2(spec)
                    except BaseException as cleanup:
                        self._registration_cleanup_error(error, cleanup)
                if host_escape is not None:
                    try:
                        self.semantic._revoke_primitive_host_escape(host_escape)
                    except BaseException as cleanup:
                        self._registration_cleanup_error(error, cleanup)
                try:
                    dictionary.rollback(checkpoint)
                    self.semantic.dictionary_index.rebuild()
                except BaseException as cleanup:
                    self._registration_cleanup_error(error, cleanup)
                raise
            self._registrations = registrations
            self._by_nonce = by_nonce
            self._issued_code_bytes += len(code)
            self._control_used += stack_size
            self._issued_child_edges += len(child_rows)
            return word

    def declaration_for(self, word: Word) -> RoutineDeclarationV1:
        """Return immutable metadata; possession does not renew a revoked lease."""
        with self.semantic._session_owner_lock:
            self._require_open()
            registration = self._registrations.get(id(word))
            if registration is None or registration.word is not word:
                raise HybridExecutionError("stale_registration", "word has no registration in this owner")
            return registration.declaration

    @property
    def registered_routines(self) -> tuple[RoutineImageV1 | RoutineImageV2 | RoutineImageV3, ...]:
        """Fresh value-only snapshots of issued declarations, without leases.

        These describe registrations, including ones whose dictionary leases
        were later revoked; they do not grant or promise live entry authority.
        """
        with self.semantic._session_owner_lock:
            self._require_open()
            values = []
            for registration in self._registrations.values():
                declaration = registration.declaration
                callbacks = type(declaration) in (RoutineDeclarationV2, RoutineDeclarationV3, RoutineDeclarationV4, RoutineDeclarationV5)
                image_type = {
                    RoutineDeclarationV1: RoutineImageV1,
                    RoutineDeclarationV2: RoutineImageV2,
                    RoutineDeclarationV3: RoutineImageV3,
                    RoutineDeclarationV4: RoutineImageV4,
                    RoutineDeclarationV5: RoutineImageV5,
                }[type(declaration)]
                extra = dict(callbacks=tuple(
                    replace(site, export=replace(site.export)) for site in declaration.callbacks
                )) if callbacks else {}
                if type(declaration) is RoutineDeclarationV4:
                    extra.update(routine_id=declaration.routine_id,
                                 max_callback_requests=declaration.max_callback_requests)
                elif type(declaration) is RoutineDeclarationV5:
                    extra.update(max_callback_requests=declaration.max_callback_requests)
                values.append(image_type(
                    name=declaration.name, code=declaration.code,
                    entry_offset=declaration.entry_offset,
                    input_cells=declaration.input_cells, output_cells=declaration.output_cells,
                    buffers=tuple(replace(rule) for rule in declaration.buffers),
                    return_stack_cells=declaration.return_stack_cells,
                    max_instructions=declaration.max_instructions, **extra,
                ))
            return tuple(values)

    def _current_meter(self) -> object | None:
        # A source input is one outer budget even though it starts a fresh
        # semantic dispatch for each token. Nested evaluation shares that root.
        if self.semantic._active_input_states:
            return self.semantic._active_input_states[0].meter
        if self.semantic._active_dispatches:
            return self.semantic._active_dispatches[0].meter
        return None

    def _allowance(self, meter: object) -> _MachineAllowance:
        requested = self._inherited_limits()
        allowance = self._allowances.get(meter)
        if allowance is None:
            allowance = _MachineAllowance(*requested)
            self._allowances[meter] = allowance
        else:
            allowance.limit = min(allowance.limit, requested[0])
            allowance.callback_limit = min(allowance.callback_limit, requested[1])
            allowance.callback_semantic_limit = min(allowance.callback_semantic_limit, requested[2])
        return allowance

    def _inherited_limits(self) -> tuple[int, int, int]:
        limits = (self._dispatch_instruction_limit, self._dispatch_callback_limit,
                  self._dispatch_callback_semantic_limit)
        for current in self._wrapper_limits:
            limits = tuple(min(left, right) for left, right in zip(limits, current))
        return limits

    def _remember_context(self, context: ExecutionContext) -> None:
        if type(context) is not ExecutionContext:
            raise HybridExecutionError("invalid_context", "hybrid requires canonical semantic contexts")
        for stack, expected in ((context.data, DataStack), (context.returns, ReturnStack)):
            if type(stack) is not expected or (
                stack._memory is not None and stack._memory is not self.semantic.memory
            ):
                raise HybridExecutionError("invalid_context", "stacks must use this shared image")
            if stack.backed:
                self._stack_allocations[stack] = (stack.floor, stack.empty_pointer - stack.floor)

    def register_context(self, context: ExecutionContext) -> None:
        """Protect a host context's complete stack allocations while alive.

        Wrappers and routine entries introduce their contexts automatically.
        A host using only raw semantic calls must introduce other context
        arenas before any routine could borrow them. Inactive allocations stay
        protected until their stack objects die; no context is retained here.
        """
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("register a hybrid context")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "register contexts only at an idle boundary")
            self._remember_context(context)

    def _protected_spans(self, context: ExecutionContext) -> tuple[tuple[int, int], ...]:
        contexts = [self.semantic.main_context, context]
        contexts += [frame.context for frame in self.semantic._active_dispatches]
        contexts += [state.context for state in self.semantic._active_input_states]
        seen = set()
        for current in contexts:
            if id(current) in seen:
                continue
            seen.add(id(current))
            if self.semantic._callback_exports._nested_private_context(current):
                continue
            self._remember_context(current)
        spans = list(self._stack_allocations.values())
        for word in self.semantic.dictionary.words:
            spans.append((word.header_address, word.body_address - word.header_address))
        for registration in self._registrations.values():
            declaration = registration.declaration
            if self.semantic.dictionary.is_body_lease_live(declaration.allocation_lease):
                spans.append((declaration.code_base, declaration.code_size))
        spans.append((self._control_base, MAX_CONTROL_BYTES))
        return _merge_spans(spans)

    def _validate_registration(self, registration: _Registration, *, verify_exports: bool = True) -> None:
        self._require_open()
        self._require_authority()
        declaration = registration.declaration
        lease = declaration.allocation_lease
        control = declaration.control_lease
        if (declaration.session_nonce is not self._session_nonce
                or self._by_nonce.get(declaration.registration_nonce) is not registration
                or self._registrations.get(id(registration.word)) is not registration
                or not self._dictionary.is_body_lease_live(lease)
                or lease.word is not registration.word
                or lease.allocation_serial != declaration.allocation_generation
                or lease.body_address != declaration.body_base
                or lease.body_limit != declaration.body_base + declaration.body_size
                or registration.word.implementation is not registration.implementation
                or registration.implementation.callback is not registration.callback
                or type(control) is not _ControlLease
                or control.owner is not self._session_nonce
                or control.generation != declaration.control_generation
                or control.base != declaration.stack_base
                or control.size != declaration.stack_size):
            raise HybridExecutionError("stale_registration", "word or allocation lease was revoked")
        if self._memory.read_bytes(declaration.code_base, declaration.code_size) != declaration.code:
            raise HybridExecutionError("stale_code", "shared code bytes no longer match the sealed image")
        for site, handle in registration.exports if verify_exports else ():
            try:
                descriptor = self.semantic.verify_callback_export(handle)
                if descriptor != site.export:
                    raise ValueError("callback export descriptor changed")
            except (ExecutionError, TypeError, ValueError) as exc:
                raise HybridExecutionError("stale_export", str(exc)) from exc

    @staticmethod
    def _result_fields(raw) -> dict[str, Any]:
        return dict(
            exit_kind=raw.exit_kind, instructions=raw.instructions, cycles=raw.cycles,
            entry_pc=raw.entry_pc, pc=raw.pc, outputs=tuple(raw.outputs),
            instruction_pc=raw.instruction_pc, access_address=raw.access_address,
            access_width=raw.access_width, access_operation=raw.access_operation,
            trap_id=raw.trap_id, detail=raw.detail,
        )

    def _settle_segment(self, raw, allowance: _MachineAllowance) -> None:
        # Native fields are deltas, not invocation totals. Cancellation itself
        # has no completed work and is not another execution segment.
        allowance.instructions += raw.instructions
        self._machine_instructions += raw.instructions
        self._machine_cycles += raw.cycles
        self._machine_segments += 1
        if raw.exit_kind == "callback_request":
            allowance.callback_requests += 1
            self._callback_requests += 1

    def _settle_v2_receipt(self, runner, allowance: _MachineAllowance) -> None:
        receipt = runner.last_segment_v2()
        if receipt is None or receipt.segment_id == self._v2_segment_id:
            return
        if receipt.segment_id != self._v2_segment_id + 1:
            raise RuntimeError("native segment accounting sequence changed")
        if receipt.invocation_id != self._v2_invocation_id:
            if receipt.invocation_id <= self._v2_invocation_id:
                raise RuntimeError("native invocation accounting sequence changed")
            self._transitions += 1
            self._v2_invocation_id = receipt.invocation_id
        allowance.instructions += receipt.instructions
        self._machine_instructions += receipt.instructions
        self._machine_cycles += receipt.cycles
        self._machine_segments += 1
        if receipt.callback_request:
            allowance.callback_requests += 1
            self._callback_requests += 1
        self._v2_segment_id = receipt.segment_id

    def _native_callback_boundary(self, runner, allowance, operation, *args, **kwargs):
        # The native receipt is retained before result marshalling. Even an
        # allocation failure or a forwarding host wrapper cannot hide completed
        # work and let a later dispatch replenish its allowance.
        try:
            raw = operation(*args, **kwargs)
        except BaseException as error:
            try:
                self._settle_v2_receipt(runner, allowance)
            except BaseException as cleanup:
                self._registration_cleanup_error(error, cleanup)
            raise
        try:
            self._settle_v2_receipt(runner, allowance)
            if raw.segment_id != self._v2_segment_id:
                raise RuntimeError("native result differs from its accounting receipt")
        except BaseException as error:
            self._registration_failure = (
                f"native segment accounting failed: {_error_detail(error)}"
            )
            raise
        return raw

    def _closed_callback_boundary(self, handle, arguments, allowance, local_limit):
        """Settle only the exact engine-owned invocation, including failures.

        Accounting hooks may have changed the outer meter or raised before an
        effect. A closed invocation's one-shot receipt owns its charged count;
        this path never reads or subtracts the mutable meter's step fields.
        """
        remaining = allowance.callback_semantic_limit - allowance.callback_semantic_steps
        consume = self.semantic.consume_closed_callback_accounting
        checkpoint = self.semantic.begin_closed_callback_accounting(handle)

        def settle():
            receipt = consume(checkpoint, handle)
            if type(receipt) is not ClosedCallbackReceipt:
                raise RuntimeError("closed callback accounting returned a foreign receipt")
            steps = receipt.semantic_steps
            if (type(steps) is not int or not 0 <= steps <= min(local_limit, remaining)
                    or type(receipt.entered) is not bool or type(receipt.completed) is not bool
                    or (not receipt.entered and (steps != 0 or receipt.completed))
                    or (receipt.completed and steps == 0)):
                raise RuntimeError("closed callback accounting receipt is inconsistent")
            allowance.callback_semantic_steps += steps
            self._callback_semantic_steps += steps
            return receipt

        try:
            callback_result = self.semantic.invoke_callback_export(
                handle, arguments, semantic_step_limit=remaining,
            )
        except BaseException as error:
            try:
                settle()
            except BaseException as cleanup:
                self._registration_cleanup_error(error, cleanup)
            raise
        try:
            receipt = settle()
            if (not receipt.entered or not receipt.completed
                    or type(callback_result) is not CallbackExportResult
                    or type(callback_result.semantic_steps) is not int
                    or callback_result.semantic_steps != receipt.semantic_steps):
                raise RuntimeError("closed callback result has no matching completed invocation")
        except BaseException as error:
            self._registration_failure = f"closed callback accounting failed: {_error_detail(error)}"
            raise HybridExecutionError("callback_accounting", self._registration_failure) from error
        return callback_result

    def _semantic_callback_boundary(self, handle, request, meter, allowance):
        if request.site.export.effect == "closed_integer_colon":
            return self._closed_callback_boundary(
                handle, request.arguments, allowance, request.site.export.max_semantic_steps,
            )
        # The locked leaf path retains its existing one-tick behavior.
        starting_steps = meter.steps
        try:
            return self.semantic.invoke_callback_export(
                handle, request.arguments,
                semantic_step_limit=(allowance.callback_semantic_limit - allowance.callback_semantic_steps),
            )
        finally:
            completed_steps = meter.steps - starting_steps
            allowance.callback_semantic_steps += completed_steps
            self._callback_semantic_steps += completed_steps

    def _drive_callbacks(self, registration: _Registration, arguments: tuple,
                         spans: tuple, protected: tuple, meter: object,
                         allowance: _MachineAllowance, remaining: int):
        runner = self._runner
        pending = None
        closed_metadata = type(registration.declaration) is RoutineDeclarationV3
        request_type = CallbackRequestV3 if closed_metadata else CallbackRequestV2
        result_type = MachineSegmentResultV3 if closed_metadata else MachineSegmentResultV2
        try:
            before_segment = self._v2_segment_id
            try:
                raw = self._native_callback_boundary(
                    runner, allowance, runner.begin_v2,
                    registration.spec, arguments, spans, remaining,
                    callback_limit=max(0, allowance.callback_limit - allowance.callback_requests),
                    protected_spans=protected,
                )
            except (TypeError, ValueError) as exc:
                if self._v2_segment_id != before_segment or self._registration_failure is not None:
                    raise
                raise HybridExecutionError("rejected_access", str(exc)) from exc
            instruction_limit = min(remaining, registration.declaration.max_instructions)
            while True:
                pending = raw.token
                request = None
                handle = None
                if raw.exit_kind == "callback_request":
                    callback = raw.callback
                    matching = [(site, issued) for site, issued in registration.exports
                                if site.call_offset == callback.call_offset
                                and site.stub_offset == callback.stub_offset
                                and site.export.export_id == callback.export_id]
                    if len(matching) != 1 or pending is None:
                        raise HybridExecutionError("invalid_callback", "native callback site was not declared")
                    site, handle = matching[0]
                    request = request_type(
                        invocation_id=callback.invocation_id, sequence=callback.sequence,
                        site=site, arguments=tuple(callback.arguments),
                    )
                result = result_type(
                    **self._result_fields(raw), invocation_id=raw.invocation_id,
                    invocation_instructions=raw.invocation_instructions,
                    invocation_cycles=raw.invocation_cycles, callback=request,
                )
                if result.exit_kind is not MachineExitKindV2.CALLBACK_REQUEST:
                    return result
                # The completed CALL is visible, but no semantic leaf may run
                # unless some machine allowance remains for its return path.
                if (raw.invocation_instructions >= instruction_limit
                        or allowance.instructions >= allowance.limit):
                    raise HybridExecutionError(
                        "instruction_limit", "machine allowance exhausted before callback dispatch",
                        result=result,
                    )
                self._validate_registration(registration)
                if self._runner is not runner:
                    raise HybridExecutionError("stale_registration", "native invocation owner changed")
                if allowance.callback_semantic_steps >= allowance.callback_semantic_limit:
                    raise HybridExecutionError(
                        "callback_semantic_limit", "outer callback semantic allowance exhausted",
                        result=result,
                    )
                try:
                    callback_result = self._semantic_callback_boundary(
                        handle, request, meter, allowance,
                    )
                except CallbackExportBudgetExceeded as exc:
                    if self._registration_failure is not None:
                        raise
                    try:
                        issued = self.semantic.consume_callback_budget_failure(exc)
                    except BaseException as cleanup:
                        self._registration_cleanup_error(exc, cleanup)
                        raise exc
                    if not issued:
                        raise
                    raise HybridExecutionError(exc.reason, str(exc), result=result) from exc
                self._validate_registration(registration)
                if self._runner is not runner:
                    raise HybridExecutionError("stale_registration", "native invocation owner changed")
                before_segment = self._v2_segment_id
                try:
                    raw = self._native_callback_boundary(
                        runner, allowance, runner.resume_callback, pending, callback_result.outputs
                    )
                except (TypeError, ValueError) as exc:
                    if self._v2_segment_id != before_segment or self._registration_failure is not None:
                        raise
                    raise HybridExecutionError("invalid_callback", str(exc)) from exc
        except BaseException as error:
            try:
                # Owner cancellation is idempotent, including when native
                # execution or marshalling already revoked the pending frame.
                runner.cancel_invocation()
            except BaseException as cleanup:
                self._registration_cleanup_error(error, cleanup)
            raise

    def _service_callback_boundary(self, handle, request, accounting, machine_result):
        remaining = accounting("remaining")[2]
        consume = _SERVICE_ACCOUNTING_CONSUME
        cleanup_error = self._registration_cleanup_error
        invoke = self.semantic.invoke_callback_export
        semantic = self.semantic
        checkpoint = None

        def settle(error):
            receipt = consume(semantic, checkpoint, handle, error)
            if type(receipt) is not ServiceCallbackReceiptV5:
                raise RuntimeError("service accounting returned a foreign receipt")
            steps, entered, completed, consumed, failure = tuple(
                field.__get__(receipt, ServiceCallbackReceiptV5) for field in _SERVICE_RECEIPT_FIELDS
            )
            if (type(steps) is not int or not 0 <= steps <= min(1, remaining)
                    or type(entered) is not bool or type(completed) is not bool
                    or (not entered and (steps != 0 or completed))
                    or (completed and steps != 1)
                    or (consumed is not None and (type(consumed) is not int
                        or not 0 <= consumed <= request.site.export.input_cells))
                    or (completed and consumed != request.site.export.input_cells)):
                raise RuntimeError("service accounting receipt is inconsistent")
            # Settle the independently issued actual count before interpreting
            # failure metadata or validating a result; neither can refund work.
            accounting("semantic", (request.sequence, receipt, steps))
            if not _service_value_routes_unchanged():
                raise RuntimeError("service receipt or result metadata routes changed")
            if failure is not None:
                if type(failure) is not ServiceCallbackFailureV5:
                    raise RuntimeError("service validation returned a foreign failure")
                (export_id, name, invocation_id, sequence, call_offset, stub_offset,
                 operation, input_cells, fpcsr, ticks, cause, kind, code) = tuple(
                    field.__get__(failure, ServiceCallbackFailureV5) for field in _SERVICE_FAILURE_FIELDS
                )
                if (error is None or cause is not error or completed or not entered or steps != 1
                        or type(export_id) is not int or export_id != request.site.export.export_id
                        or type(name) is not str or name != request.site.export.name
                        or type(invocation_id) is not int or invocation_id != request.invocation_id
                        or type(sequence) is not int or sequence != request.sequence
                        or type(call_offset) is not int or call_offset != request.site.call_offset
                        or type(stub_offset) is not int or stub_offset != request.site.stub_offset
                        or type(operation) is not int or not 0 <= operation <= 0xFF
                        or type(fpcsr) is not int or fpcsr < 0 or fpcsr & ~0x1F7
                        or type(input_cells) is not int or input_cells != request.site.export.input_cells
                        or type(ticks) is not int or ticks != steps
                        or type(kind) is not str or kind != "illegal_scalar_float"
                        or type(code) is not int or code != -21):
                    raise RuntimeError("service validation failure does not match its issued request")
            return receipt, steps, entered, completed, failure

        checkpoint = _SERVICE_ACCOUNTING_BEGIN(semantic, handle, request)
        try:
            result = invoke(handle, request.arguments, semantic_step_limit=remaining)
        except BaseException as error:
            try:
                receipt, steps, entered, completed, failure = settle(error)
            except BaseException as cleanup:
                cleanup_error(error, cleanup)
                raise error
            if failure is not None:
                raise HybridExecutionError(
                    "service_fault", "admitted scalar service validation failed",
                    result=machine_result, service_failure=failure,
                ) from error
            raise
        try:
            receipt, steps, entered, completed, failure = settle(None)
            outputs, ticks = ((None, None) if type(result) is not CallbackExportResult else tuple(
                field.__get__(result, CallbackExportResult) for field in _SERVICE_RESULT_FIELDS
            ))
            if (not entered or not completed or failure is not None
                    or type(result) is not CallbackExportResult
                    or type(ticks) is not int or ticks != steps
                    or type(outputs) is not tuple or len(outputs) != request.site.export.output_cells
                    or any(type(cell) is not int or not 0 <= cell <= MASK64 for cell in outputs)):
                raise HybridExecutionError("invalid_callback", "service has no completed output receipt",
                                           result=machine_result)
            # consume() may repair corrupt host meter metadata and still return
            # truthful work. The subsequent owner proof must reject that case.
            self._require_service_profile()
        except BaseException:
            self._registration_failure = "service callback accounting or output validation failed"
            raise
        return outputs

    def _drive_services(self, registration, arguments, spans, protected, meter, allowance, remaining):
        native_routes = self._service_native_routes
        native_authority = native_routes()
        runner, methods, _classes = native_authority
        begin, resume, query, cancel = methods
        validate = self._validate_registration
        profile = self._require_service_profile
        cleanup_error = self._registration_cleanup_error
        callback_boundary = self._service_callback_boundary
        accounting = _service_accounting_authority(self, allowance, runner, meter)
        declaration = registration.declaration
        instruction_limit = min(remaining, declaration.max_instructions)
        native_result_type = self._native.RoutineSegmentResultV2

        def require_native():
            try:
                if native_routes() is not native_authority:
                    raise HybridExecutionError("invalid_callback", "service native owner authority changed")
            except BaseException:
                self._registration_failure = "service native owner authority changed"
                raise

        def boundary(operation, *args, **kwargs):
            before = accounting("segment")
            require_native()
            try:
                raw = operation(*args, **kwargs)
            except BaseException as error:
                try:
                    accounting("native", query())
                except BaseException as cleanup:
                    cleanup_error(error, cleanup)
                raise
            accounting("native", query())
            require_native()
            if (type(raw) is not native_result_type or raw.segment_id <= before
                    or raw.segment_id != accounting("segment")):
                self._registration_failure = "service native result has no fresh receipt"
                raise HybridExecutionError("invalid_callback", self._registration_failure)
            self._require_open()
            return raw

        try:
            before = accounting("segment")
            try:
                raw = boundary(begin, registration.spec, arguments, spans, remaining,
                               callback_limit=min(declaration.max_callback_requests,
                                                  accounting("remaining")[1]),
                               protected_spans=protected)
            except (TypeError, ValueError) as error:
                if accounting("segment") != before or self._registration_failure is not None:
                    raise
                raise HybridExecutionError("rejected_access", str(error)) from error
            while True:
                request = None
                if raw.exit_kind == "callback_request":
                    callback = raw.callback
                    matching = tuple((site, handle) for site, handle in registration.exports
                                     if site.call_offset == callback.call_offset
                                     and site.stub_offset == callback.stub_offset
                                     and site.export.export_id == callback.export_id)
                    if len(matching) != 1 or raw.token is None:
                        raise HybridExecutionError("invalid_callback", "service callback site was not declared")
                    site, handle = matching[0]
                    request = CallbackRequestV5(invocation_id=callback.invocation_id,
                                                sequence=callback.sequence, site=site,
                                                arguments=tuple(callback.arguments))
                result = MachineSegmentResultV5(
                    **self._result_fields(raw), invocation_id=raw.invocation_id,
                    invocation_instructions=raw.invocation_instructions,
                    invocation_cycles=raw.invocation_cycles, callback=request,
                )
                if result.exit_kind is not MachineExitKindV2.CALLBACK_REQUEST:
                    accounting("require")
                    profile()
                    validate(registration)
                    return result
                available = accounting("remaining")
                if raw.invocation_instructions >= instruction_limit or available[0] <= 0:
                    raise HybridExecutionError("instruction_limit", "machine allowance exhausted before service",
                                               result=result)
                if available[2] <= 0:
                    raise HybridExecutionError("callback_semantic_limit", "callback work allowance exhausted",
                                               result=result)
                validate(registration)
                profile()
                try:
                    callback_outputs = callback_boundary(handle, request, accounting, result)
                except CallbackExportBudgetExceeded as error:
                    if self._registration_failure is not None:
                        raise
                    try:
                        issued = self.semantic.consume_callback_budget_failure(error)
                    except BaseException as cleanup:
                        cleanup_error(error, cleanup)
                        raise error
                    if not issued:
                        raise
                    raise HybridExecutionError(error.reason, str(error), result=result) from error
                accounting("require")
                validate(registration)
                profile()
                raw = boundary(resume, raw.token, callback_outputs)
        except BaseException as error:
            # Retained original routes recover completed native work even if a
            # post-work result/receipt allocation escaped before publication.
            try:
                accounting("native", query())
            except BaseException as cleanup:
                cleanup_error(error, cleanup)
            try:
                cancel()
            except BaseException as cleanup:
                cleanup_error(error, cleanup)
            raise

    def _invoke(self, nonce: object, context: ExecutionContext, *, _private_service: bool = False) -> None:
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("enter a hybrid routine")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "machine entry is not reentrant")
            registration = self._by_nonce.get(nonce)
            if registration is None:
                raise HybridExecutionError("stale_registration", "registration identity is no longer live")
            # The task profile has its own retained budget. Until mixed-profile
            # accounting is qualified, one original meter can choose only one.
            if self._task_adapter is not None:
                meter = self._current_meter()
                if meter is None:
                    raise HybridExecutionError("invalid_entry", "routine requires an active semantic dispatch")
                self.semantic._foreign_tasks.claim_machine_profile(meter, "private")
            if type(registration.declaration) is RoutineDeclarationV5:
                if not _private_service and not self.service_callback_abi_available:
                    raise HybridExecutionError("service_unavailable", "scalar service callbacks are not enabled")
                self._require_service_profile()
            if type(registration.declaration) is RoutineDeclarationV4:
                if not self.nested_callback_abi_available:
                    raise HybridExecutionError("nested_unavailable", "nested machine execution is not enabled")
                return self._invoke_published_nested(registration.word, context)
            self._validate_registration(registration)
            declaration = registration.declaration
            protected = self._protected_spans(context)
            # Read only the declared argument cells, never the whole stack.
            arguments = tuple(context.data.peek(index)
                              for index in reversed(range(declaration.input_cells)))
            context.data.require_push_capacity(max(0, declaration.output_cells - declaration.input_cells))
            spans = []
            try:
                for rule in declaration.buffers:
                    span = rule.resolve(arguments)
                    if span.size:
                        if not any(region.base <= span.base and span.limit <= region.limit
                                   for region in self.semantic.memory.regions):
                            raise ValueError("borrowed buffer must fit one ordinary shared region")
                        if any(_overlap(span.base, span.size, base, size) for base, size in protected):
                            raise ValueError("borrowed buffer overlaps protected stack, code, or header bytes")
                    spans.append((span.base, span.size, span.access.value))
            except (TypeError, ValueError) as exc:
                raise HybridExecutionError("rejected_access", str(exc)) from exc
            meter = self._current_meter()
            if meter is None:
                raise HybridExecutionError("invalid_entry", "routine requires an active semantic dispatch")
            allowance = self._allowance(meter)
            remaining = allowance.limit - allowance.instructions
            if remaining <= 0:
                raise HybridExecutionError("instruction_limit", "outer dispatch machine allowance exhausted")
            self._active_machine = True
            try:
                if type(declaration) is RoutineDeclarationV5:
                    result = self._drive_services(
                        registration, arguments, tuple(spans), protected, meter, allowance, remaining
                    )
                elif type(declaration) in (RoutineDeclarationV2, RoutineDeclarationV3):
                    result = self._drive_callbacks(
                        registration, arguments, tuple(spans), protected, meter, allowance, remaining
                    )
                else:
                    try:
                        raw = self._runner.run(registration.spec, arguments, tuple(spans), remaining,
                                               protected_spans=protected)
                    except (TypeError, ValueError) as exc:
                        raise HybridExecutionError("rejected_access", str(exc)) from exc
                    self._settle_segment(raw, allowance)
                    self._transitions += 1
                    result = MachineRoutineResultV1(**self._result_fields(raw))
            finally:
                self._active_machine = False
            if result.exit_kind != "returned":
                raise HybridExecutionError(result.exit_kind.value, result.detail, result=result)
            if len(result.outputs) != declaration.output_cells:
                raise HybridExecutionError("invalid_return", "machine output arity changed", result=result)
            for _ in range(declaration.input_cells):
                context.data.pop()
            for cell in result.outputs:
                context.data.push(cell)

    def _call(self, operation: Callable, *args: Any,
              machine_instruction_limit: int | None = None,
              machine_instruction_budget: int | None = None,
              dispatch_callback_limit: int | None = None,
              dispatch_callback_semantic_limit: int | None = None,
              _resume: bool = False, **kwargs: Any) -> HybridRunReport:
        limit = _alias(machine_instruction_limit, machine_instruction_budget, "machine instruction budget")
        requested = (limit, dispatch_callback_limit, dispatch_callback_semantic_limit)
        maxima = (self._dispatch_instruction_limit, self._dispatch_callback_limit,
                  self._dispatch_callback_semantic_limit)
        labels = ("machine instruction limit", "dispatch callback limit", "dispatch callback semantic limit")
        for value, maximum, label in zip(requested, maxima, labels):
            if value is not None:
                _positive_limit(value, maximum, label)
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("run hybrid source")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "host entry is forbidden during a machine invocation")
            context = kwargs.get("context")
            if context is not None:
                self._remember_context(context)
            meter = self._current_meter()
            if meter is None and _resume and self.semantic._suspended_execution is not None:
                suspended = self.semantic._suspended_execution
                self._remember_context(suspended.context)
                meter = suspended.meter
            inherited = self._inherited_limits()
            allowance = self._allowances.get(meter) if meter is not None else None
            if allowance is not None:
                inherited = tuple(min(left, right) for left, right in zip(
                    inherited, (allowance.limit, allowance.callback_limit, allowance.callback_semantic_limit)
                ))
            for value, active, label in zip(requested, inherited, labels):
                if value is not None and value > active:
                    raise ValueError(f"{label} cannot raise the active dispatch allowance")
            self._wrapper_limits.append(tuple(
                active if value is None else value for value, active in zip(requested, inherited)
            ))
            before = (self._machine_instructions, self._machine_cycles, self._transitions,
                      self._callback_requests, self._callback_semantic_steps, self._machine_segments)
            completed = False
            try:
                result = operation(*args, **kwargs)
                completed = True
            except ExecutionBlocked:
                completed = True
                raise
            finally:
                # The first quantum may suspend before any registered word is
                # reached. Persist its limit now so raw or wrapped resumes
                # cannot acquire a fresh default allowance later.
                try:
                    suspended = self.semantic._suspended_execution
                    if completed and suspended is not None:
                        self._allowance(suspended.meter)
                finally:
                    self._wrapper_limits.pop()
            return HybridRunReport(
                result, self._machine_instructions - before[0],
                self._machine_cycles - before[1], self._transitions - before[2],
                self._callback_requests - before[3], self._callback_semantic_steps - before[4],
                self._machine_segments - before[5],
            )

    def evaluate(self, source: str | bytes | bytearray | memoryview, *,
                 step_budget: int | None = None, semantic_step_budget: int | None = None,
                 **kwargs: Any) -> HybridRunReport:
        if isinstance(source, str):
            source = source.encode("utf-8")
        return self._call(self.semantic.evaluate, source,
                          step_budget=_alias(step_budget, semantic_step_budget, "semantic step budget"),
                          **kwargs)

    def execute(self, name_or_xt: bytes | str | int, *, step_budget: int | None = None,
                semantic_step_budget: int | None = None, **kwargs: Any) -> HybridRunReport:
        return self._call(self.semantic.execute, name_or_xt,
                          step_budget=_alias(step_budget, semantic_step_budget, "semantic step budget"),
                          **kwargs)

    def execute_xt(self, xt: int, **kwargs: Any) -> HybridRunReport:
        if type(xt) is not int:
            raise TypeError("execution token must be an exact integer")
        return self.execute(xt, **kwargs)

    def run(self, name_or_xt: bytes | str | int, *, step_budget: int | None = None,
            semantic_step_budget: int | None = None, **kwargs: Any) -> HybridRunReport:
        return self._call(self.semantic.run_until_blocked, name_or_xt,
                          step_budget=_alias(step_budget, semantic_step_budget, "semantic step budget"),
                          **kwargs)

    def resume_yielded(self, suspension: object, **kwargs: Any) -> HybridRunReport:
        return self._call(self.semantic.resume_yielded, suspension, _resume=True, **kwargs)

    def resume(self, suspension: object, wake_receipt: object, **kwargs: Any) -> HybridRunReport:
        return self._call(self.semantic.resume, suspension, wake_receipt, _resume=True, **kwargs)

    def close(self) -> None:
        """Revoke all entries before releasing private machine ownership."""
        with self.semantic._session_owner_lock:
            if self._closed:
                return
            self.semantic._require_session_owner_access("close the hybrid owner")
            if self._active_machine or self.semantic._active_dispatches or self.semantic._active_input_states:
                raise HybridExecutionError("active_dispatch", "close only at an idle host boundary")
            failure = None
            if id(self) in _TASK_OWNER_AUTHORITIES:
                authority, changed = _task_owner_authority(self)
                adapter = authority.adapter
                if changed:
                    failure = HybridExecutionError("callback_accounting", "task installation authority changed")
                try:
                    root = authority.engine._task_root
                    if root is not None:
                        authority.engine.finish_root(root, completed=False)
                except BaseException as error:
                    if failure is None:
                        failure = error
                try:
                    # Issued before callbacks; closes the exact native owner
                    # without traversing possibly changed Python cleanup routes.
                    authority.cleanup()
                except BaseException as error:
                    if failure is None:
                        failure = error
                    else:
                        try:
                            BaseException.add_note(failure, "native task close also failed")
                        except BaseException:
                            pass
                    if not authority.closed():
                        raise failure
            else:
                self._runner.close()
            self._closed = True
            self._runner = None
            self._nested_runner = None
            self._cpu = None
            self._control_buffer = None
            self._allowances.clear()
            if failure is not None:
                raise failure


_NESTED_OWNER_ROUTES = tuple((name, vars(HybridRuntime)[name]) for name in (
    "_capture_nested_target", "_verify_nested_target", "_invoke_nested_child",
    "_admit_nested_callback", "_nested_child_edge", "_nested_borrows", "_protected_spans",
    "_require_nested_execution",
    "_drive_nested_frame", "_nested_native_boundary", "_nested_callback_boundary",
    "_settle_nested_receipt", "_settle_nested_semantics",
    "_validate_registration", "_require_open", "_require_authority",
))
_NESTED_OWNER_SPECIAL_ROUTES = tuple((name, getattr(HybridRuntime, name)) for name in (
    "__getattribute__", "__setattr__", "__delattr__",
))
_NESTED_OWNER_DICT_DESCRIPTOR = vars(HybridRuntime)["__dict__"]
_NESTED_OWNER_ABSENT = object()
_NESTED_OWNER_FIELD_ROUTES = tuple((name, vars(HybridRuntime).get(name, _NESTED_OWNER_ABSENT))
                                 for name in (
    "semantic", "_registrations", "_by_nonce", "_session_nonce", "_closed",
    "_registration_failure", "_dictionary", "_dictionary_guard", "_memory",
    "_nested_runner", "_dispatch_callback_limit", "dispatch_callback_limit",
    "_nested_execution", "_active_machine", "_native", "_v3_segment_id",
))


__all__ = ["HybridExecutionError", "HybridRunReport", "HybridRuntime"]
