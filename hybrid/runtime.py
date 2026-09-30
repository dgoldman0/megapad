"""One semantic owner with explicit, bounded architectural routine calls.

The semantic runtime remains the source, service, and continuation authority.
Only registered primitive callbacks cross into the architectural interpreter;
both engines retain the same fixed ordinary-memory buffers.
"""

from __future__ import annotations

from contextlib import nullcontext
from dataclasses import dataclass, replace
import os
from typing import Any, Callable
import weakref

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
from shared.hybrid_nested import RoutineImageV4, RoutineDeclarationV4, MAX_CHILD_EDGES
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES, Word
from simulator.errors import ExecutionBlocked, ExecutionError
from simulator.interop_exports import (
    CallbackExportBudgetExceeded, CallbackExportResult, ClosedCallbackReceipt,
)
from simulator.memory import AddressClass, MMIO_BASE, MMIO_LIMIT, SparseAddressSpace
from simulator.platform import create_one_core_address_space
from simulator.runtime import ExecutionContext, MegaForthRuntime
from simulator.stacks import DataStack, ReturnStack


class HybridExecutionError(ExecutionError):
    """An entry preflight or bounded machine interval failed coherently."""

    def __init__(self, reason: str, detail: str = "", *,
                 result: MachineRoutineResultV1 | None = None) -> None:
        self.reason = reason
        self.result = result
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
        **runtime_kwargs: Any,
    ) -> HybridRuntime:
        if type(require_nested_callbacks) is not bool:
            raise TypeError("require_nested_callbacks must be an exact boolean")
        if require_nested_callbacks:
            raise RuntimeError(
                "hybrid nested callbacks require the fully qualified semantic V4 "
                "profile and native V3 transport; this build does not enable them"
            )
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
            return cls(semantic, native, limit, machine, callback_limit, callback_semantic_limit)
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
        self._allowances: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._stack_allocations: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._wrapper_limits: list[tuple[int, int, int]] = []
        self._cpu, self._runner, self._control_base, self._control_buffer, self._nested_runner = machine
        self._callbacks_available = self._supports_callbacks(native)
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

    def _invoke_nested_child(self, use, arguments):
        # The authority/accounting foundation is installed at creation; it
        # deliberately grants no entry until native V3 composition is ready.
        raise HybridExecutionError("nested_unavailable", "nested machine execution is not enabled")

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
        return (
            self._callbacks_available
            and getattr(self.semantic, "callback_export_abi_version", None)
            == HYBRID_CLOSED_ABI_VERSION
            and callable(getattr(self.semantic, "inspect_callback_export", None))
            and callable(getattr(self.semantic, "callback_policy_core_xt", None))
            and callable(getattr(self.semantic, "consume_callback_budget_failure", None))
            and callable(getattr(self.semantic, "begin_closed_callback_accounting", None))
            and callable(getattr(self.semantic, "consume_closed_callback_accounting", None))
        )

    @property
    def nested_callback_abi_available(self) -> bool:
        """Foundation ownership alone never advertises executable V4 support."""
        return False

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

    def _require_authority(self) -> None:
        if (self.semantic.dictionary is not self._dictionary
                or self._dictionary._mutation_guard is not self._dictionary_guard
                or self.semantic.memory is not self._memory):
            raise HybridExecutionError("stale_registration", "shared dictionary or memory owner changed")

    def _registration_cleanup_error(self, error: BaseException, cleanup: BaseException) -> None:
        self._registration_failure = (
            f"registration rollback failed: {_error_detail(cleanup)}"
        )
        BaseException.add_note(error, self._registration_failure)

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
            callbacks = image_type in (RoutineImageV2, RoutineImageV3, RoutineImageV4)
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
                    }[image_type]
                    extra = dict(
                        callbacks=image.callbacks,
                        dispatch_callback_limit=self._dispatch_callback_limit,
                        dispatch_callback_semantic_limit=self._dispatch_callback_semantic_limit,
                    ) if callbacks else {}
                    if nested:
                        extra.update(routine_id=image.routine_id, max_callback_requests=image.max_callback_requests)
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
                callbacks = type(declaration) in (RoutineDeclarationV2, RoutineDeclarationV3, RoutineDeclarationV4)
                image_type = {
                    RoutineDeclarationV1: RoutineImageV1,
                    RoutineDeclarationV2: RoutineImageV2,
                    RoutineDeclarationV3: RoutineImageV3,
                    RoutineDeclarationV4: RoutineImageV4,
                }[type(declaration)]
                extra = dict(callbacks=tuple(
                    replace(site, export=replace(site.export)) for site in declaration.callbacks
                )) if callbacks else {}
                if type(declaration) is RoutineDeclarationV4:
                    extra.update(routine_id=declaration.routine_id,
                                 max_callback_requests=declaration.max_callback_requests)
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

    def _invoke(self, nonce: object, context: ExecutionContext) -> None:
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("enter a hybrid routine")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "machine entry is not reentrant")
            registration = self._by_nonce.get(nonce)
            if registration is None:
                raise HybridExecutionError("stale_registration", "registration identity is no longer live")
            if type(registration.declaration) is RoutineDeclarationV4 and not self.nested_callback_abi_available:
                raise HybridExecutionError("nested_unavailable", "nested machine execution is not enabled")
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
                if type(declaration) in (RoutineDeclarationV2, RoutineDeclarationV3):
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
            self._closed = True
            self._runner.close()
            self._runner = None
            self._nested_runner = None
            self._cpu = None
            self._control_buffer = None
            self._allowances.clear()


_NESTED_OWNER_ROUTES = tuple((name, vars(HybridRuntime)[name]) for name in (
    "_capture_nested_target", "_verify_nested_target", "_invoke_nested_child",
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
))


__all__ = ["HybridExecutionError", "HybridRunReport", "HybridRuntime"]
