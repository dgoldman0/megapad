"""Declared MP64 machine routines inside a semantic MegaForth runtime.

A routine is an ordinary dictionary word whose body holds its machine code.
Calling it runs the code on a full MP64 core that shares the runtime's memory.
The machine keeps its return addresses on the caller's Forth return stack, as
the chip does. At a declared call site the machine stops, the named Forth
word runs on the caller's own stacks, and the machine resumes when that word
returns into the machine's return address.
"""

from __future__ import annotations

import os
import weakref
from typing import Any

from hybrid.routine_manifest import ABI, RoutineDeclaration, RoutineManifest
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES, Word
from simulator.errors import ExecutionError, IllegalInstructionFault
from simulator.memory import AddressClass, MemoryAccessError, SparseAddressSpace
from simulator.platform import create_one_core_address_space
from simulator.runtime import (
    MachineCallback,
    MachineReturned,
    MachineYield,
    MegaForthRuntime,
)
from simulator.stacks import MachineReturn, StackOverflow


CODE_ALIGNMENT = 16
NATIVE_REVISION = 1
# An allowance this large means "until the routine returns or calls back".
UNBOUNDED = 1 << 62
_RUNNING, _CALLBACK, _YIELDED = range(3)


class HybridExecutionError(ExecutionError):
    """A hybrid routine could not run or continue."""

    def __init__(self, reason: str, detail: str = "", *, event: Any = None) -> None:
        self.reason = reason
        self.detail = detail
        self.event = event
        super().__init__(f"{reason}: {detail}" if detail else reason)


class MachineBudgetExceeded(HybridExecutionError):
    """The dispatch used up its machine instruction budget."""


class MachineRoutineFault(IllegalInstructionFault):
    """A routine reached code or a control transfer the chip traps on."""

    def __init__(self, failure: str, detail: str, event: Any) -> None:
        self.failure = failure
        self.event = event
        super().__init__(f"machine routine {failure}: {detail}")


class MachineAccessFault(MemoryAccessError):
    """A routine reached memory outside its borrowed buffers."""


class _Routine:
    __slots__ = ("declaration", "image", "word", "lease", "targets")

    def __init__(self, declaration: RoutineDeclaration, image: Any) -> None:
        self.declaration = declaration
        self.image = image
        self.word: Word | None = None
        self.lease = None
        self.targets: list[Word | None] = [None] * len(declaration.callbacks)


class _Entry:
    """One machine entry begun by a call and not yet returned."""

    __slots__ = ("routine", "context", "frontier", "resume", "state", "slot",
                 "machine_return", "site", "depth")

    def __init__(self, routine, context, frontier, resume) -> None:
        self.routine = routine
        self.context = context
        self.frontier = frontier
        self.resume = resume
        self.state = _RUNNING
        self.slot = 0
        self.machine_return: MachineReturn | None = None
        self.site = None
        self.depth = 0


class HybridRuntime:
    """Own a semantic runtime, a full MP64 core and the routines they share."""

    @classmethod
    def create(
        cls,
        *,
        executor: str | None = None,
        memory: SparseAddressSpace | None = None,
        geometry: dict | None = None,
        machine_instruction_budget: int | None = None,
        **runtime_kwargs: Any,
    ) -> HybridRuntime:
        selected = executor if executor is not None else os.environ.get("MEGAFORTH_EXECUTOR", "python")
        if selected not in ("python", "native", "auto"):
            raise ValueError("semantic executor must be python, native, or auto")
        if memory is not None and geometry is not None:
            raise TypeError("specify memory or geometry, not both")
        if memory is None:
            memory = create_one_core_address_space(dense_backing=True, **(geometry or {}))
        elif type(memory) is not SparseAddressSpace or memory.dense_backing is None:
            raise ValueError("hybrid memory must be densely backed")
        if machine_instruction_budget is not None and (
                type(machine_instruction_budget) is not int or machine_instruction_budget < 1):
            raise ValueError("machine_instruction_budget must be a positive integer or None")
        try:
            import _mp64_accel as native
        except ImportError as error:
            raise RuntimeError("hybrid execution requires _mp64_accel; run make build") from error
        if (getattr(native, "HYBRID_ROUTINE_ABI", None) != ABI
                or getattr(native, "HYBRID_ROUTINE_REVISION", None) != NATIVE_REVISION):
            raise RuntimeError("hybrid execution requires a matching _mp64_accel; run make build")
        cpu = native.CPUState()
        backing = memory.dense_backing
        for region in memory.regions:
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
                raise ValueError("unsupported hybrid memory region")
        runner = native.RoutineRunner(cpu)
        try:
            semantic = MegaForthRuntime(memory=memory, execution_backend=selected, **runtime_kwargs)
        except BaseException:
            runner.close()
            raise
        return cls(semantic, native, cpu, runner, machine_instruction_budget)

    def __init__(self, semantic: MegaForthRuntime, native: Any, cpu: Any, runner: Any,
                 machine_instruction_budget: int | None) -> None:
        if semantic._machine_owner is not None:
            raise RuntimeError("the semantic runtime already has a machine owner")
        self.semantic = semantic
        self._native = native
        self._cpu = cpu
        self._runner = runner
        self._budget = machine_instruction_budget
        self._spent: weakref.WeakKeyDictionary = weakref.WeakKeyDictionary()
        self._turn_left: int | None = None
        self._routines: list[_Routine] = []
        self._by_image: dict[Any, _Routine] = {}
        self._entries: list[_Entry] = []
        self._generation = semantic.dictionary.execution_generation
        self._transitions = 0
        self._callbacks = 0
        self._closed = False
        semantic._machine_owner = self

    # -- registration -----------------------------------------------------

    def register(self, declaration: RoutineDeclaration) -> Word:
        """Define the routine's word, with its code as the word's body."""

        if type(declaration) is not RoutineDeclaration:
            raise TypeError("register requires a RoutineDeclaration")
        semantic = self.semantic
        with semantic._session_owner_lock:
            semantic._require_session_owner_access("register a machine routine")
            self._require_open()
            if (semantic._active_dispatches or semantic._active_input_states
                    or semantic._suspended_execution is not None):
                raise HybridExecutionError("busy", "register routines at an idle host boundary")
            self._sweep(force=True)
            name = declaration.name.encode("ascii")
            code = declaration.code + b"\x01" * (-len(declaration.code) % CODE_ALIGNMENT)
            dictionary = semantic.dictionary
            body_base = dictionary.here + HEADER_FIXED_BYTES + len(name) + SEMANTIC_CODE_SLOT_BYTES
            prepad = -body_base % CODE_ALIGNMENT
            image = self._native.RoutineImage(
                body_base + prepad, code, declaration.entry_offset,
                declaration.input_cells, declaration.output_cells,
                tuple((site.call_offset, site.stub_offset, site.input_cells, site.output_cells)
                      for site in declaration.callbacks))
            routine = _Routine(declaration, image)
            checkpoint = dictionary.checkpoint()
            try:
                word = semantic.define_routine(name, routine, initial_body=b"\x01" * prepad + code)
                lease = dictionary.acquire_body_lease(word)
                if lease.body_address != body_base:
                    raise HybridExecutionError("registration", "the routine body moved")
                self._runner.publish(image)
            except BaseException:
                dictionary.rollback(checkpoint)
                semantic.dictionary_index.rebuild()
                raise
            routine.word, routine.lease = word, lease
            self._routines.append(routine)
            self._by_image[image] = routine
            return word

    def register_manifest(self, manifest: RoutineManifest) -> tuple[Word, ...]:
        if type(manifest) is not RoutineManifest:
            raise TypeError("register_manifest requires a RoutineManifest")
        return tuple(self.register(declaration) for declaration in manifest.routines)

    @property
    def registered_routines(self) -> tuple[RoutineDeclaration, ...]:
        return tuple(routine.declaration for routine in self._routines if routine.image is not None)

    def _sweep(self, *, force: bool = False) -> None:
        """Withdraw the code of routines whose dictionary bodies were reclaimed."""

        dictionary = self.semantic.dictionary
        generation = dictionary.execution_generation
        if not force and generation == self._generation:
            return
        self._generation = generation
        for routine in self._routines:
            if routine.image is not None and not dictionary.is_body_lease_live(routine.lease):
                self._withdraw(routine)

    def _withdraw(self, routine: _Routine) -> None:
        if self._runner.is_published(routine.image):
            self._runner.revoke(routine.image)
        self._by_image.pop(routine.image, None)
        routine.image = None

    # -- the dispatcher's machine owner protocol ---------------------------

    @property
    def parked(self) -> bool:
        """Whether any entry may have a machine return on a Forth stack."""

        return bool(self._entries)

    def begin_turn(self, quantum: int | None) -> None:
        self._turn_left = quantum

    def finish_dispatch(self) -> None:
        """Abandon the entries of a dispatch that ended or was cancelled."""

        if self._entries:
            self._entries.clear()
            self._runner.cancel(0)

    def call(self, word, context, meter, resume, can_yield):
        routine = word.implementation.routine
        self._require_open()
        self._sweep()
        if routine.image is None or not self.semantic.dictionary.is_body_lease_live(routine.lease):
            if routine.image is not None:
                self._withdraw(routine)
            raise HybridExecutionError(
                "stale_routine", f"{routine.declaration.name}'s code was reclaimed")
        self._prune()
        declaration = routine.declaration
        data, returns = context.data, context.returns
        count = declaration.input_cells
        arguments = tuple(data.peek(index) for index in range(count - 1, -1, -1))
        spans = self._spans(declaration, arguments)
        returns.require_push_capacity(1)
        frontier = returns.pointer
        try:
            event = self._runner.begin(routine.image, arguments, spans, frontier,
                                       returns.floor, self._allowance(meter, can_yield))
        except ValueError as error:
            raise MachineAccessFault(
                f"{declaration.name}: {error}", operation="borrow",
                address=spans[0][0] if spans else 0, length=spans[0][1] if spans else 0,
            ) from None
        for _ in range(count):
            data.pop()
        entry = _Entry(routine, context, frontier, resume)
        self._entries.append(entry)
        self._transitions += 1
        return self._settle(entry, event, meter, can_yield)

    def resume(self, machine_return, context, meter, can_yield):
        entry = machine_return.entry
        entries = self._entries
        try:
            index = entries.index(entry)
        except ValueError:
            raise HybridExecutionError(
                "stale_machine_return", "Forth returned into a machine entry that was abandoned") from None
        if index + 1 < len(entries):
            # Forth returned past these entries' machine returns.
            del entries[index + 1:]
            self._runner.cancel(index + 1)
        if entry.state != _CALLBACK or entry.machine_return is not machine_return:
            raise HybridExecutionError("stale_machine_return", "the machine entry is not waiting here")
        site = entry.site
        data = context.data
        if data.depth() != entry.depth + site.output_cells:
            raise HybridExecutionError(
                "callback_stack",
                f"{site.target} left {data.depth() - entry.depth} cells where its "
                f"call site in {entry.routine.declaration.name} takes {site.output_cells}")
        outputs = tuple(data.peek(index) for index in range(site.output_cells - 1, -1, -1))
        for _ in range(site.output_cells):
            data.pop()
        entry.state = _RUNNING
        entry.machine_return = None
        event = self._runner.resume(outputs, self._allowance(meter, can_yield))
        return self._settle(entry, event, meter, can_yield)

    def advance(self, entry, context, meter, can_yield):
        if not self._entries or self._entries[-1] is not entry or entry.state != _YIELDED:
            raise HybridExecutionError("stale_machine_entry", "the yielded machine entry is gone")
        entry.state = _RUNNING
        event = self._runner.advance(self._allowance(meter, can_yield))
        return self._settle(entry, event, meter, can_yield)

    # -- settlement ---------------------------------------------------------

    def _settle(self, entry: _Entry, event, meter, can_yield):
        while True:
            self._account(meter, event.instructions)
            kind = event.kind
            if kind == "returned":
                self._entries.pop()
                entry.context.returns.set_machine_frontier(entry.frontier)
                data = entry.context.data
                for value in event.values:
                    data.push(value)
                return MachineReturned(entry.resume)
            if kind == "callback":
                return self._callback(entry, event)
            if kind == "yielded":
                if self._budget is not None and self._spent.get(meter, 0) >= self._budget:
                    self._abandon_top(entry)
                    raise MachineBudgetExceeded(
                        "instruction_budget",
                        f"{entry.routine.declaration.name} used the dispatch's "
                        f"{self._budget} machine instructions")
                if can_yield and self._turn_left is not None and self._turn_left <= 0:
                    entry.state = _YIELDED
                    entry.context.returns.set_machine_frontier(event.sp)
                    return MachineYield(entry)
                event = self._runner.advance(self._allowance(meter, can_yield))
                continue
            self._entries.pop()
            entry.context.returns.set_machine_frontier(entry.frontier)
            raise self._fault(entry, event)

    def _callback(self, entry: _Entry, event) -> MachineCallback:
        routine = self._by_image.get(event.image)
        if routine is None:
            self._abandon_top(entry)
            raise HybridExecutionError("stale_routine", "a callback site's routine was reclaimed")
        site = routine.declaration.callbacks[event.site]
        target = self._target(routine, event.site)
        context = entry.context
        returns = context.returns
        returns.set_machine_frontier(event.sp)
        machine_return = MachineReturn(entry, self.semantic.memory.read64(event.sp))
        returns.mark_machine_return(machine_return)
        entry.state = _CALLBACK
        entry.slot = event.sp
        entry.machine_return = machine_return
        entry.site = site
        data = context.data
        entry.depth = data.depth()
        for value in event.values:
            data.push(value)
        self._callbacks += 1
        return MachineCallback(target)

    def _target(self, routine: _Routine, index: int) -> Word:
        """The site's Forth word, bound by name when first needed."""

        word = routine.targets[index]
        if word is None or self.semantic.dictionary._by_xt.get(word.xt) is not word:
            name = routine.declaration.callbacks[index].target
            word = self.semantic.find(name)
            if word is None:
                raise HybridExecutionError(
                    "undefined_callback",
                    f"{routine.declaration.name} calls back {name}, which is not defined")
            routine.targets[index] = word
        return word

    def _prune(self) -> None:
        """Abandon entries whose machine returns Forth has unwound past."""

        for index, entry in enumerate(self._entries):
            if entry.state == _CALLBACK and not entry.context.returns.holds_machine_return(
                    entry.machine_return, entry.slot):
                del self._entries[index:]
                self._runner.cancel(index)
                return

    def _abandon_top(self, entry: _Entry) -> None:
        self._entries.pop()
        self._runner.cancel(len(self._entries))
        entry.context.returns.set_machine_frontier(entry.frontier)

    def _allowance(self, meter, can_yield: bool) -> int:
        allowance = UNBOUNDED
        if can_yield and self._turn_left is not None:
            allowance = max(self._turn_left, 1)
        if self._budget is not None:
            remaining = self._budget - self._spent.get(meter, 0)
            if remaining <= 0:
                raise MachineBudgetExceeded(
                    "instruction_budget", f"the dispatch used its {self._budget} machine instructions")
            allowance = min(allowance, remaining)
        return allowance

    def _account(self, meter, instructions: int) -> None:
        if self._turn_left is not None:
            self._turn_left -= instructions
        if self._budget is not None:
            self._spent[meter] = self._spent.get(meter, 0) + instructions

    @staticmethod
    def _spans(declaration: RoutineDeclaration, arguments: tuple[int, ...]) -> tuple:
        spans = []
        for rule in declaration.buffers:
            base = arguments[rule.address_argument]
            size = arguments[rule.length_argument] * rule.element_bytes
            if rule.max_bytes is not None and size > rule.max_bytes:
                raise MachineAccessFault(
                    f"{declaration.name}: a buffer is larger than its declared {rule.max_bytes} bytes",
                    operation=rule.access, address=base, length=size)
            if size:
                if base + size > 1 << 64:
                    raise MachineAccessFault(
                        f"{declaration.name}: a buffer wraps the address space",
                        operation=rule.access, address=base, length=size)
                spans.append((base, size, rule.access))
        return tuple(spans)

    def _fault(self, entry: _Entry, event) -> BaseException:
        name = entry.routine.declaration.name
        if event.failure == "rejected_access":
            if event.access_operation in ("call_stack_write", "return_stack_read"):
                return StackOverflow("return", floor=entry.context.returns.floor)
            return MachineAccessFault(
                f"{name}: {event.detail}", operation=event.access_operation,
                address=event.access_address, length=event.access_width)
        return MachineRoutineFault(event.failure, f"{name}: {event.detail}", event)

    # -- status and lifetime -------------------------------------------------

    @property
    def machine_instruction_budget(self) -> int | None:
        return self._budget

    @property
    def machine_instructions(self) -> int:
        return self._runner.instructions if not self._closed else self._final[0]

    @property
    def machine_cycles(self) -> int:
        return self._runner.cycles if not self._closed else self._final[1]

    @property
    def machine_segments(self) -> int:
        return self._runner.segments if not self._closed else self._final[2]

    @property
    def transitions(self) -> int:
        return self._transitions

    @property
    def callback_requests(self) -> int:
        return self._callbacks

    @property
    def closed(self) -> bool:
        return self._closed

    def _require_open(self) -> None:
        if self._closed:
            raise HybridExecutionError("closed", "the hybrid runtime is closed")

    def close(self) -> None:
        """Abandon machine entries and release the core."""

        if self._closed:
            return
        semantic = self.semantic
        with semantic._session_owner_lock:
            semantic._require_session_owner_access("close the hybrid runtime")
            if semantic._active_dispatches or semantic._active_input_states:
                raise HybridExecutionError("busy", "close the hybrid runtime at an idle host boundary")
            self._final = (self._runner.instructions, self._runner.cycles, self._runner.segments)
            self._entries.clear()
            self._closed = True
            semantic._machine_owner = None
            self._runner.close()


__all__ = [
    "HybridExecutionError", "HybridRuntime", "MachineAccessFault", "MachineBudgetExceeded",
    "MachineRoutineFault",
]
