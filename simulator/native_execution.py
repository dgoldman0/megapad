"""Identity-bound native plans for the existing semantic instruction stream.

The compiler and uncommon operations remain in the reference dispatcher. Each
native entry returns at an original IR boundary, with shared memory, stack
metadata, and semantic clocks settled before Python resumes that operation.
"""
from __future__ import annotations

from collections import Counter
import os
from time import perf_counter_ns

from shared import scalar_fp
from simulator import ir
from simulator import runtime as rt
from simulator.diagnostics import HostedDiagnosticsService
from simulator.memory import SparseAddressSpace
from simulator.stacks import Continuation, DataStack, ReturnStack
from simulator.timer import HostedTimerService


# Without a host quantum, native work still returns to the same dispatcher at
# this interval. It bounds one native entry, not a guest-visible boundary.
UNQUANTIZED_NATIVE_INTERVAL_STEPS = 8192
SEMANTIC_API_VERSION = 4


class NativeExecutor:
    @classmethod
    def create(cls, runtime, *, required: bool, admit_core: bool):
        try:
            import _megaforth_native as extension
        except ImportError:
            if required:
                raise RuntimeError(
                    "native semantic execution requires _megaforth_native; "
                    "run python setup_simulator_accel.py build_ext --inplace"
                ) from None
            return None
        if (getattr(extension, "SEMANTIC_API_VERSION", None) != SEMANTIC_API_VERSION
                or not hasattr(extension, "scalar_fp_execute")
                or not hasattr(extension, "keccak_f1600")
                or getattr(extension, "TILE_VALUES_API_VERSION", None) != 1
                or not callable(getattr(extension, "tile_execute_values", None))
                or not callable(getattr(extension, "tile_values_supported", None))
                or getattr(extension, "TILE_GUARD_API_VERSION", None) != 1
                or not callable(getattr(extension, "TileIdentityGuard", None))):
            if required:
                raise RuntimeError(
                    "native semantic execution requires a matching "
                    "_megaforth_native build; run make build"
                )
            return None
        return cls(runtime, extension, admit_core=admit_core)

    def __init__(self, runtime, extension, *, admit_core: bool):
        self.runtime = runtime
        self.extension = extension
        memory = runtime.memory
        if memory.dense_backing is None:
            sparse_regions = [(region.spec.base, region.spec.size, region.pages)
                              for region in memory._regions]
            dense_regions = ()
        else:
            sparse_regions = ()
            dense_regions = [(spec.base, spec.size, memory.dense_backing.buffer_at(spec.base))
                             for spec in memory.regions]
        self.program = extension.NativeProgram(
            sparse_regions,
            memory.page_size,
            Continuation,
            dense_regions=dense_regions,
        )
        # Only original installed BIOS callbacks may become native primitives.
        # Later same-named host callbacks and source definitions keep their XT.
        self.primitives = {
            word.xt: (word, extension.PRIMITIVE_OPCODES[word.name])
            for word in runtime.dictionary.words
            if admit_core and isinstance(word.implementation, rt.PrimitiveDefinition)
            and word.name in extension.PRIMITIVE_OPCODES
        }
        # BIOS closures retain this service even if the public runtime
        # attribute is replaced later. Native calls must keep the same owner.
        self.scalar_float = runtime.scalar_float
        scalar_operations = {
            name.encode("ascii"): (
                (extension.OP_FPCSR_FETCH, 0, 0) if shape == "fetch" else
                (extension.OP_FPCSR_STORE, 0, 0) if shape == "store" else
                (extension.OP_SCALAR_FP, operation,
                 {"unary": 1, "binary": 2, "fma": 3}[shape])
            )
            for name, shape, operation in scalar_fp.BIOS_WORDS
        }
        self.scalar_primitives = {
            word.xt: (word, scalar_operations[word.name])
            for word in runtime.dictionary.words
            if admit_core and isinstance(word.implementation, rt.PrimitiveDefinition)
            and word.name in scalar_operations
        }
        self.generation = runtime.dictionary.execution_generation
        self.plans = {}
        self.entry_costs = {}
        self.continuation_frames = {}
        # Machine routines planned for direct calls, by execution token, and
        # the routine words by native index.
        self.routine_owner = None
        self.routine_slots = {}
        self.routine_words = []
        self.routine_count = 0
        # Machine work a native interval left for Python, if any.
        self.machine_handoff = None
        self.entries = 0
        self.semantic_steps = 0
        self.profile_enabled = os.environ.get("MEGAFORTH_NATIVE_PROFILE") == "1"
        self.exit_counts = Counter()
        self.native_run_ns = 0
        self.settlement_ns = 0

    def invalidate(self):
        self.program.clear()
        self.plans.clear()
        self.entry_costs.clear()
        self.continuation_frames.clear()
        self.unbind_routines()
        self.generation = self.runtime.dictionary.execution_generation

    def unbind_routines(self):
        if self.routine_owner is not None:
            self.program.bind_routines(None)
        self.routine_owner = None
        self.routine_slots.clear()
        self.routine_words.clear()
        self.routine_count = 0

    def _routine_index(self, word, pending):
        """Plan a direct call to a machine routine the owner admits for one."""

        owner = self.runtime._machine_owner
        if owner is None:
            return None
        slot = self.routine_slots.get(word.xt)
        if slot is not None and slot[0] is word and self.routine_owner is owner:
            return slot[2]
        native = owner.native_routine(word)
        if native is None:
            return None
        image, inputs, outputs, rules, sites = native
        if self.routine_owner is None:
            self.program.bind_routines(owner.native_entry())
            self.routine_owner = owner
        elif self.routine_owner is not owner:
            return None
        index = self.routine_count
        self.program.set_routine(index, image.native_handle, inputs, outputs, rules, sites)
        # The slot keeps the image alive while a plan can name its index.
        self.routine_slots[word.xt] = (word, image, index)
        self.routine_words.append(word)
        self.routine_count += 1
        # A site's word is bound by Python when the site is first used; one
        # already bound runs here from now on.
        routine = word.implementation.routine
        for site in range(len(sites)):
            target = owner.bound_target(routine, site)
            if target is not None:
                self.program.set_routine_target(index, site, *self._site_operation(target, pending))
        return index

    def _site_operation(self, target, pending):
        """How native code runs a callback word: OP_CALL or one inline operation."""

        op = self.extension
        operation = self._call(target, pending)
        if operation[0] == op.OP_CALL or (
                operation[0] not in (op.OP_STOP, op.OP_CALL_ROUTINE, op.OP_EXECUTE,
                                     op.OP_I, op.OP_J, op.OP_RP_FETCH)):
            return operation
        return op.OP_STOP, 0, 0

    def routine_target_bound(self, routine, site, target):
        """Run a callback site's newly bound word natively from now on."""

        word = routine.word
        if (word is None or self.generation != self.runtime.dictionary.execution_generation
                or self.routine_owner is not self.runtime._machine_owner):
            return
        slot = self.routine_slots.get(word.xt)
        if slot is None or slot[0] is not word:
            return
        pending = []
        self.program.set_routine_target(slot[2], site, *self._site_operation(target, pending))
        self._install(pending)

    def stats(self):
        result = {"entries": self.entries, "semantic_steps": self.semantic_steps,
                  "plans": len(self.plans)}
        if self.profile_enabled:
            result["profile"] = {
                "exits": dict(self.exit_counts),
                "native_run_ns": self.native_run_ns,
                "settlement_ns": self.settlement_ns,
            }
        return result

    def snapshot_stack(self, stack):
        """Read a complete canonical stack without Python per-cell dispatch."""

        if (
            type(self.runtime.memory) is not SparseAddressSpace
            or type(stack) not in (DataStack, ReturnStack)
            or stack._memory is not self.runtime.memory
        ):
            return None
        return self.program.snapshot_stack(
            (stack._floor, stack._empty_pointer, stack._pointer),
            stack._continuations if type(stack) is ReturnStack else None,
        )

    def _profile_exit(self, xt, ip, steps, allowance, *, skipped=False):
        if steps == allowance:
            reason = "allowance"
        else:
            word = self.runtime._resolve_dispatch_word(xt)
            operations = word.implementation.operations
            if ip >= len(operations):
                reason = "end_of_plan"
            else:
                instruction = operations[ip]
                reason = type(instruction).__name__
                if isinstance(instruction, ir.Call):
                    target = self.runtime.dictionary._by_xt.get(instruction.xt)
                    if target is not None:
                        reason += ":" + target.name.decode("ascii", errors="replace")
        prefix = "skipped:" if skipped else "progress:" if steps else "empty:"
        self.exit_counts[prefix + reason] += 1

    def _admitted_context(self, context, meter):
        runtime = self.runtime
        callback = meter._on_tick
        return (
            type(runtime.memory) is SparseAddressSpace
            and type(runtime.diagnostics) is HostedDiagnosticsService
            and type(runtime.timer) is HostedTimerService
            and type(context.data) is DataStack
            and type(context.returns) is ReturnStack
            and context.data._memory is runtime.memory
            and context.returns._memory is runtime.memory
            and type(meter) is rt._StepMeter
            and "tick" not in meter.__dict__
            and getattr(callback, "__self__", None) is runtime
            and getattr(callback, "__func__", None)
            is rt.MegaForthRuntime._account_semantic_step
        )

    def _call(self, target, pending):
        op = self.extension
        implementation = target.implementation
        if isinstance(implementation, rt.ConstantDefinition):
            return op.OP_PUSH_CELL, implementation.value, 0
        if isinstance(implementation, rt.ValueDefinition):
            return op.OP_FETCH_VALUE, target.body_address, 0
        if isinstance(implementation, rt.CreatedDefinition):
            if implementation.action is None:
                return op.OP_PUSH_CELL, target.body_address, 0
        if isinstance(implementation, rt.PrimitiveDefinition):
            admitted = self.primitives.get(target.xt)
            if admitted is not None and admitted[0] is target:
                return admitted[1], 0, 0
            scalar = self.scalar_primitives.get(target.xt)
            if scalar is not None and scalar[0] is target:
                return scalar[1]
        if isinstance(implementation, rt.ColonDefinition):
            if target.xt not in self.runtime._colon_accelerators:
                pending.append(target)
                return op.OP_CALL, target.xt, 0
        if isinstance(implementation, rt.RoutineDefinition):
            index = self._routine_index(target, pending)
            if index is not None:
                return op.OP_CALL_ROUTINE, index, 0
        return op.OP_STOP, 0, 0

    def _prepare(self, initial):
        if self.generation != self.runtime.dictionary.execution_generation:
            self.invalidate()
        if initial.xt in self.plans:
            return
        self._install([initial])

    def _install(self, pending):
        """Plan these colon words and every colon word they reach."""

        runtime = self.runtime
        op = self.extension
        while pending:
            word = pending.pop()
            if word.xt in self.plans:
                continue
            if runtime.dictionary._by_xt.get(word.xt) is not word:
                continue
            if not isinstance(word.implementation, rt.ColonDefinition):
                continue
            if word.xt in runtime._colon_accelerators:
                continue
            # Mark before following recursive calls. Plans keep exact original
            # instruction indices; unsupported operations remain explicit stops.
            self.plans[word.xt] = word
            operations = []
            for instruction in word.implementation.operations:
                item = (op.OP_STOP, 0, 0)
                if isinstance(instruction, ir.Literal):
                    item = (op.OP_LITERAL, instruction.value, 0)
                elif isinstance(instruction, (ir.Call, ir.CallSelf)):
                    target = word if isinstance(instruction, ir.CallSelf) else (
                        runtime.dictionary._by_xt.get(instruction.xt)
                    )
                    if target is not None:
                        item = self._call(target, pending)
                elif isinstance(instruction, ir.Branch):
                    item = (op.OP_BRANCH, instruction.target, 0)
                elif isinstance(instruction, ir.BranchZero):
                    item = (op.OP_BRANCH_ZERO, instruction.target, 0)
                elif isinstance(instruction, ir.Return):
                    item = (op.OP_RETURN, 0, 0)
                elif isinstance(instruction, ir.RPush):
                    item = (op.OP_R_PUSH, 0, 0)
                elif isinstance(instruction, ir.RPop):
                    item = (op.OP_R_POP, 0, 0)
                elif isinstance(instruction, ir.RPeek):
                    item = (op.OP_R_PEEK, 0, 0)
                elif isinstance(instruction, ir.Do):
                    item = (op.OP_DO, 0, 0)
                elif isinstance(instruction, ir.QuestionDo):
                    item = (op.OP_QUESTION_DO, instruction.target, 0)
                elif isinstance(instruction, ir.Loop):
                    item = (op.OP_LOOP, instruction.target, 0)
                elif isinstance(instruction, ir.PlusLoop):
                    item = (op.OP_PLUS_LOOP, instruction.target, 0)
                elif isinstance(instruction, ir.Unloop):
                    item = (op.OP_UNLOOP, 0, 0)
                elif isinstance(instruction, ir.StoreValue):
                    item = (op.OP_STORE_VALUE, instruction.address, 0)
                elif isinstance(instruction, ir.PushStringLiteral):
                    item = (op.OP_STRING_LITERAL,
                            word.body_address + instruction.offset,
                            instruction.length)
                operations.append(item)
            self.program.install(word.xt, operations)
            # Zero marks a Python-owned operation. Other entries match the
            # native cost(): an IR operation costs one tick, while a compiled
            # primitive/constant call also includes its execution tick.
            self.entry_costs[word.xt] = bytes(
                0 if operation[0] == op.OP_STOP else
                1 if op.OP_LITERAL <= operation[0] <= op.OP_UNLOOP else 2
                for operation in operations
            )

    def run(self, current, ip, context, meter, quantum_limit, machine_allowance=0):
        self.machine_handoff = None
        if not self._admitted_context(context, meter):
            return None
        # This is an internal return to the same dispatcher, never a guest
        # watchdog or a new host-service/IDL boundary. A host quantum already
        # bounds the interval, so native work may run up to that boundary.
        allowance = (
            UNQUANTIZED_NATIVE_INTERVAL_STEPS if quantum_limit is None
            else quantum_limit - meter.steps
        )
        if meter.budget is not None:
            allowance = min(allowance, meter.budget - meter.steps)
        if allowance <= 0:
            return None
        if context.returns._continuation_cookie > (1 << 64) - 1 - 2 * allowance:
            return None
        self._prepare(current)
        # Decline known stops and operations that cannot fit this allowance
        # before marshalling state. Python retains the exact partial-budget
        # effects and the existing outer-IR quantum boundary.
        costs = self.entry_costs.get(current.xt)
        if costs is not None and 0 <= ip < len(costs):
            required = costs[ip]
            if required == 0 or required > allowance:
                if self.profile_enabled:
                    self._profile_exit(current.xt, ip, 0, allowance, skipped=True)
                return None
        data = context.data
        returns = context.returns
        started = perf_counter_ns() if self.profile_enabled else 0
        result = self.program.run(
            current.xt, ip,
            (data._floor, data._empty_pointer, data._pointer),
            (returns._floor, returns._empty_pointer, returns._pointer,
             returns._continuation_cookie),
            returns._continuations,
            allowance,
            self.scalar_float.fpcsr,
            machine_allowance,
        )
        (xt, resumed_ip, steps, data_pointer, return_pointer, cookie,
         updates, pointer_captures, fpcsr, machine) = result
        # Publish the completed prefix before clocks, profiling, fallback or
        # any host observer can see the native interval's boundary.
        self.scalar_float._fpcsr = fpcsr
        if self.profile_enabled:
            self.native_run_ns += perf_counter_ns() - started
            self._profile_exit(xt, resumed_ip, steps, allowance)
            started = perf_counter_ns()
        if not steps:
            return None
        # Native writes include popped slots. Their metadata must survive too:
        # a later RP! may restore a previously captured frontier.
        for slot, caller_xt, return_ip, raw in updates:
            if caller_xt == 0:
                returns._continuations.pop(slot, None)
            else:
                # Ordinary continuation values are immutable and determined
                # by their original call site. The per-slot raw cookie still
                # changes on every call and remains the authority for RP!.
                key = (caller_xt, return_ip)
                continuation = self.continuation_frames.get(key)
                if continuation is None:
                    continuation = Continuation(caller_xt, return_ip)
                    self.continuation_frames[key] = continuation
                returns._continuations[slot] = (
                    continuation, raw
                )
        data._pointer = data_pointer
        returns._pointer = return_pointer
        returns._continuation_cookie = cookie
        if pointer_captures:
            returns._pointer_capture_generation += pointer_captures
        meter.steps += steps
        self.runtime.diagnostics.account_work_many(steps)
        self.runtime.timer.advance_by(steps)
        if machine is not None:
            instructions, calls, callbacks, handoff, frames, callback = machine
            self.routine_owner.account_native(meter, instructions, calls, callbacks)
            if handoff or frames:
                # Entries this interval began and did not finish, oldest first.
                words = self.routine_words
                self.machine_handoff = (handoff, [
                    (words[routine], caller_xt, caller_ip, frontier, in_callback,
                     words[site_routine], site, slot, raw, depth)
                    for (routine, caller_xt, caller_ip, frontier, in_callback,
                         site_routine, site, slot, raw, depth) in frames
                ], callback)
        self.entries += 1
        self.semantic_steps += steps
        if self.profile_enabled:
            self.settlement_ns += perf_counter_ns() - started
        return self.runtime._resolve_dispatch_word(xt), resumed_ip
