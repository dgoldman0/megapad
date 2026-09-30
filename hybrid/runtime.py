"""One semantic owner with explicit, bounded architectural routine calls.

The semantic runtime remains the source, service, and continuation authority.
Only registered primitive callbacks cross into the architectural interpreter;
both engines retain the same fixed ordinary-memory buffers.
"""

from __future__ import annotations

from dataclasses import dataclass
import os
from typing import Any, Callable
import weakref

from shared.cells import MASK64
from shared.hybrid_abi import (
    CELL_BYTES, CODE_ALIGNMENT, HYBRID_ABI, HYBRID_ABI_VERSION,
    MAX_CONTROL_BYTES, MAX_DISPATCH_INSTRUCTIONS, MAX_ROUTINES,
    MAX_TOTAL_CODE_BYTES, MachineExitKindV1, MachineRoutineResultV1,
    RoutineDeclarationV1, RoutineImageV1,
)
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES, Word
from simulator.errors import ExecutionBlocked, ExecutionError
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


@dataclass(slots=True)
class _MachineAllowance:
    limit: int
    instructions: int = 0


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
    """Own one shared image and the v1 integer-routine declaration registry.

    ``semantic`` is the actual MegaForthRuntime, including its original stack,
    dictionary, native planner, timers, and services. Raw semantic entry remains
    safe: every registered callback enforces its outer meter's machine budget.
    """

    @classmethod
    def create(
        cls, *, executor: str | None = None, semantic_executor: str | None = None,
        memory: SparseAddressSpace | None = None, geometry: dict | None = None,
        dispatch_instruction_limit: int = MAX_DISPATCH_INSTRUCTIONS,
        **runtime_kwargs: Any,
    ) -> HybridRuntime:
        limit = _positive_limit(dispatch_instruction_limit,
                                MAX_DISPATCH_INSTRUCTIONS, "dispatch instruction limit")
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
        return cls(semantic, native, limit, machine)

    def __init__(self, semantic: MegaForthRuntime, native: Any, limit: int,
                 machine: tuple) -> None:
        self.semantic = semantic
        self._native = native
        self._dispatch_instruction_limit = limit
        self._executor = semantic.execution_backend
        self._session_nonce = object()
        self._closed = False
        self._registration_failure: str | None = None
        self._active_machine = False
        self._registrations: dict[int, _Registration] = {}
        self._by_nonce: dict[object, _Registration] = {}
        self._issued_code_bytes = 0
        self._control_used = 0
        self._machine_instructions = 0
        self._machine_cycles = 0
        self._transitions = 0
        self._allowances: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._stack_allocations: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._wrapper_limits: list[int] = []
        self._cpu, self._runner, self._control_base, self._control_buffer = machine
        self._remember_context(semantic.main_context)

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
        runner = native.RoutineRunnerV1(cpu, control_base, control_buffer)
        return cpu, runner, control_base, control_buffer

    @property
    def executor(self) -> str:
        """The selected semantic executor; architectural execution is native."""
        return self._executor

    @property
    def dispatch_instruction_limit(self) -> int:
        return self._dispatch_instruction_limit

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
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("register a hybrid routine")
            self.semantic._require_no_suspension("register a hybrid routine")
            if self.semantic._active_dispatches or self.semantic._active_input_states:
                raise HybridExecutionError("active_dispatch", "register only at an idle host boundary")
            if image is not None and values:
                raise TypeError("pass a RoutineImageV1 or its keyword fields, not both")
            if image is None:
                image = RoutineImageV1(**values)
            if type(image) is not RoutineImageV1:
                raise TypeError("registration requires a RoutineImageV1")
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
            try:
                word = self.semantic.define_primitive(image.name, invoke, initial_body=body)
                lease = dictionary.acquire_body_lease(word)
                if lease.body_address != body_base or lease.body_limit != body_base + len(body):
                    raise HybridExecutionError("stale_registration", "body publication geometry changed")
                generation = len(self._registrations) + 1
                control = _ControlLease(self._session_nonce, generation, stack_base, stack_size)
                declaration = RoutineDeclarationV1(
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
                )
                self._runner.publish_code(spec)
            except BaseException as error:
                try:
                    dictionary.rollback(checkpoint)
                    self.semantic.dictionary_index.rebuild()
                except BaseException as cleanup:
                    # Preserve the actual publication failure. A failed repair
                    # cannot leave this registry available for further entry;
                    # the host can still close and release its native owner.
                    self._registration_failure = (
                        f"registration rollback failed: {type(cleanup).__name__}: {cleanup}"
                    )
                    add_note = getattr(error, "add_note", None)
                    if add_note is not None:
                        add_note(self._registration_failure)
                raise
            registration = _Registration(word, declaration, spec)
            self._registrations[id(word)] = registration
            self._by_nonce[registration_nonce] = registration
            self._issued_code_bytes += len(code)
            self._control_used += stack_size
            return word

    def declaration_for(self, word: Word) -> RoutineDeclarationV1:
        """Return immutable metadata; possession does not renew a revoked lease."""
        with self.semantic._session_owner_lock:
            self._require_open()
            registration = self._registrations.get(id(word))
            if registration is None or registration.word is not word:
                raise HybridExecutionError("stale_registration", "word has no registration in this owner")
            return registration.declaration

    def _current_meter(self) -> object | None:
        # A source input is one outer budget even though it starts a fresh
        # semantic dispatch for each token. Nested evaluation shares that root.
        if self.semantic._active_input_states:
            return self.semantic._active_input_states[0].meter
        if self.semantic._active_dispatches:
            return self.semantic._active_dispatches[0].meter
        return None

    def _allowance(self, meter: object) -> _MachineAllowance:
        requested = min(self._wrapper_limits, default=self._dispatch_instruction_limit)
        allowance = self._allowances.get(meter)
        if allowance is None:
            allowance = _MachineAllowance(requested)
            self._allowances[meter] = allowance
        else:
            allowance.limit = min(allowance.limit, requested)
        return allowance

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

    def _invoke(self, nonce: object, context: ExecutionContext) -> None:
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("enter a hybrid routine")
            if self._active_machine:
                raise HybridExecutionError("active_dispatch", "machine entry is not reentrant")
            registration = self._by_nonce.get(nonce)
            if registration is None:
                raise HybridExecutionError("stale_registration", "registration identity is no longer live")
            declaration = registration.declaration
            lease = declaration.allocation_lease
            control = declaration.control_lease
            if (declaration.session_nonce is not self._session_nonce
                    or declaration.registration_nonce is not nonce
                    or not self.semantic.dictionary.is_body_lease_live(lease)
                    or lease.word is not registration.word
                    or lease.allocation_serial != declaration.allocation_generation
                    or lease.body_address != declaration.body_base
                    or lease.body_limit != declaration.body_base + declaration.body_size
                    or type(control) is not _ControlLease
                    or control.owner is not self._session_nonce
                    or control.generation != declaration.control_generation
                    or control.base != declaration.stack_base
                    or control.size != declaration.stack_size):
                raise HybridExecutionError("stale_registration", "word or allocation lease was revoked")
            if self.semantic.memory.read_bytes(declaration.code_base, declaration.code_size) != declaration.code:
                raise HybridExecutionError("stale_code", "shared code bytes no longer match the sealed image")
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
                raw = self._runner.run(registration.spec, arguments, tuple(spans), remaining,
                                       protected_spans=protected)
            except (TypeError, ValueError) as exc:
                raise HybridExecutionError("rejected_access", str(exc)) from exc
            finally:
                self._active_machine = False
            # Settle completed work before returning to Python or raising. No
            # machine cycles are added to the semantic step clock or timer.
            allowance.instructions += raw.instructions
            self._machine_instructions += raw.instructions
            self._machine_cycles += raw.cycles
            self._transitions += 1
            result = MachineRoutineResultV1(
                exit_kind=raw.exit_kind, instructions=raw.instructions, cycles=raw.cycles,
                entry_pc=raw.entry_pc, pc=raw.pc, outputs=tuple(raw.outputs),
                instruction_pc=raw.instruction_pc, access_address=raw.access_address,
                access_width=raw.access_width, access_operation=raw.access_operation,
                trap_id=raw.trap_id, detail=raw.detail,
            )
            if result.exit_kind is not MachineExitKindV1.RETURNED:
                raise HybridExecutionError(result.exit_kind.value, result.detail, result=result)
            for _ in range(declaration.input_cells):
                context.data.pop()
            for cell in result.outputs:
                context.data.push(cell)

    def _call(self, operation: Callable, *args: Any,
              machine_instruction_limit: int | None = None,
              machine_instruction_budget: int | None = None,
              _resume: bool = False, **kwargs: Any) -> HybridRunReport:
        limit = _alias(machine_instruction_limit, machine_instruction_budget, "machine instruction budget")
        if limit is not None:
            _positive_limit(limit, self._dispatch_instruction_limit, "machine instruction limit")
        with self.semantic._session_owner_lock:
            self._require_open()
            self.semantic._require_session_owner_access("run hybrid source")
            context = kwargs.get("context")
            if context is not None:
                self._remember_context(context)
            meter = self._current_meter()
            if meter is None and _resume and self.semantic._suspended_execution is not None:
                suspended = self.semantic._suspended_execution
                self._remember_context(suspended.context)
                meter = suspended.meter
            inherited = min(self._wrapper_limits, default=self._dispatch_instruction_limit)
            allowance = self._allowances.get(meter) if meter is not None else None
            if allowance is not None:
                inherited = min(inherited, allowance.limit)
            if limit is not None and limit > inherited:
                raise ValueError("machine instruction limit cannot raise the active dispatch allowance")
            self._wrapper_limits.append(inherited if limit is None else limit)
            before = self._machine_instructions, self._machine_cycles, self._transitions
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
            return HybridRunReport(result, self._machine_instructions - before[0],
                                   self._machine_cycles - before[1], self._transitions - before[2])

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
            self._cpu = None
            self._control_buffer = None
            self._allowances.clear()


__all__ = ["HybridExecutionError", "HybridRunReport", "HybridRuntime"]
