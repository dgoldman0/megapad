"""Identity-bound native plans for the existing semantic instruction stream.

The compiler and uncommon operations remain in the reference dispatcher. Each
native entry returns at an original IR boundary, with shared memory, stack
metadata, and semantic clocks settled before Python resumes that operation.
"""
from __future__ import annotations

from simulator import ir
from simulator import runtime as rt
from simulator.diagnostics import HostedDiagnosticsService
from simulator.memory import SparseAddressSpace
from simulator.stacks import Continuation, DataStack, ReturnStack
from simulator.timer import HostedTimerService


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
        return cls(runtime, extension, admit_core=admit_core)

    def __init__(self, runtime, extension, *, admit_core: bool):
        self.runtime = runtime
        self.extension = extension
        self.program = extension.NativeProgram(
            [(region.spec.base, region.spec.size, region.pages)
             for region in runtime.memory._regions],
            runtime.memory.page_size,
        )
        # Only original installed BIOS callbacks may become native primitives.
        # Later same-named host callbacks and source definitions keep their XT.
        self.primitives = {
            word.xt: (word, extension.PRIMITIVE_OPCODES[word.name])
            for word in runtime.dictionary.words
            if admit_core and isinstance(word.implementation, rt.PrimitiveDefinition)
            and word.name in extension.PRIMITIVE_OPCODES
        }
        self.generation = runtime.dictionary.execution_generation
        self.plans = {}
        self.entries = 0
        self.semantic_steps = 0

    def invalidate(self):
        self.program.clear()
        self.plans.clear()
        self.generation = self.runtime.dictionary.execution_generation

    def stats(self):
        return {"entries": self.entries, "semantic_steps": self.semantic_steps,
                "plans": len(self.plans)}

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
        if isinstance(implementation, rt.ColonDefinition):
            if target.xt not in self.runtime._colon_accelerators:
                pending.append(target)
                return op.OP_CALL, target.xt, 0
        return op.OP_STOP, 0, 0

    def _prepare(self, initial):
        runtime = self.runtime
        if self.generation != runtime.dictionary.execution_generation:
            self.invalidate()
        pending = [initial]
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
                elif isinstance(instruction, ir.StoreValue):
                    item = (op.OP_STORE_VALUE, instruction.address, 0)
                elif isinstance(instruction, ir.PushStringLiteral):
                    item = (op.OP_STRING_LITERAL,
                            word.body_address + instruction.offset,
                            instruction.length)
                operations.append(item)
            self.program.install(word.xt, operations)

    def run(self, current, ip, context, meter, quantum_limit):
        if not self._admitted_context(context, meter):
            return None
        # This is an internal return to the same dispatcher, never a guest
        # watchdog or a new host-service/IDL boundary.
        allowance = 8192
        if meter.budget is not None:
            allowance = min(allowance, meter.budget - meter.steps)
        if quantum_limit is not None:
            allowance = min(allowance, quantum_limit - meter.steps)
        if allowance <= 0:
            return None
        if context.returns._continuation_cookie > (1 << 64) - 1 - 2 * allowance:
            return None
        self._prepare(current)
        data = context.data
        returns = context.returns
        result = self.program.run(
            current.xt, ip,
            (data._floor, data._empty_pointer, data._pointer),
            (returns._floor, returns._empty_pointer, returns._pointer,
             returns._continuation_cookie),
            allowance,
        )
        xt, resumed_ip, steps, data_pointer, return_pointer, cookie, updates = result
        if not steps:
            return None
        # Native writes include popped slots. Their metadata must survive too:
        # a later RP! may restore a previously captured frontier.
        for slot, caller_xt, return_ip, raw in updates:
            returns._continuations[slot] = (
                Continuation(caller_xt, return_ip), raw
            )
        data._pointer = data_pointer
        returns._pointer = return_pointer
        returns._continuation_cookie = cookie
        meter.steps += steps
        self.runtime.diagnostics.account_work_many(steps)
        self.runtime.timer.advance_by(steps)
        self.entries += 1
        self.semantic_steps += steps
        return self.runtime._resolve_dispatch_word(xt), resumed_ip
