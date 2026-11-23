"""The BIOS's cooperative task slots on core 0.

Task 0 is the foreground. BACKGROUND, BACKGROUND2 and BACKGROUND3 start an
execution token in slot 1, 2 or 3 on stacks of the BIOS's size, 32 cells
each, as bios.asm's task_setup_slot gives them (docs/multicore-simulator-
plan.md, stage 5). PAUSE, from the foreground, looks at the slots after the
one it last ran, in round-robin order, and runs the first active one until
the slot calls TASK-YIELD or its word returns, which ends the slot as the
BIOS's task_cleanup does; the foreground then goes on after PAUSE. The next
PAUSE that reaches a yielded slot resumes it after its TASK-YIELD. TASK-STOP
ends a slot, TASK? and #TASKS report the slots, and TASK-ID names the
running one, so a slot is a context of core 0 with its own identity for the
CRC and SHA-3 owner records and for KDOS's per-slot CATCH chains.

Slots change only at PAUSE and TASK-YIELD, so the core scheduler's policy
never enters: a slot runs inside the foreground's turns on core 0, and on a
machine with several cores the other cores take their turns around it as
they do around any dispatch of core 0's that cannot detach. The slots'
stacks lie in the 1,536 bytes below the dictionary's start, where the BIOS
keeps its own slot stacks below its dictionary.

PAUSE in a slot, TASK-YIELD in the foreground, and either word on a worker
core take the illegal-operation fault, as the BIOS's words refuse them. An
error or ABORT in a slot ends the slot, and the foreground's line that
paused ends with it, as the BIOS's QUIT ends the slot that was running.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from simulator.errors import ExecutionError, IllegalInstructionFault
from simulator.stacks import DataStack, ReturnStack

if TYPE_CHECKING:
    from simulator.runtime import MegaForthRuntime


SLOTS = 3
SLOT_CELLS = 32
SLOT_STACK_BYTES = 8 * SLOT_CELLS
# Each slot's data stack, then its return stack.
SLOT_BYTES = 2 * SLOT_STACK_BYTES
SLOT_REGION_BYTES = SLOTS * SLOT_BYTES


class HostedTasks:
    """Core 0's three background slots and the foreground's PAUSE."""

    def __init__(self, runtime: MegaForthRuntime, *, base: int) -> None:
        self.runtime = runtime
        self.base = base
        # Index 0 stands for the foreground, which is never a slot.
        self._xts = [0] * (SLOTS + 1)
        self._suspensions: list[object | None] = [None] * (SLOTS + 1)
        self._contexts: list[object | None] = [None] * (SLOTS + 1)
        # Bumped at each start, so a slot replaced while it runs is known.
        self._generations = [0] * (SLOTS + 1)
        # The round-robin cursor, the slot PAUSE last ran, as the BIOS's
        # var_task_index; and the slot running now, 0 for the foreground,
        # as its var_task_active.
        self.index = 0
        self.active = 0

    # -- what the words report ---------------------------------------------

    def is_active(self, slot: int) -> bool:
        return self._xts[slot] != 0

    def count(self) -> int:
        return sum(1 for slot in range(1, SLOTS + 1) if self._xts[slot])

    def call_chains(self):
        """For each active slot, the execution tokens of the words it runs
        inside: for a yielded slot the word it stopped in, then its
        callers'; for one that has not run yet the word it will start."""

        from simulator.stacks import Continuation

        snapshot = self.runtime._stack_snapshot
        for slot in range(1, SLOTS + 1):
            if not self._xts[slot]:
                continue
            suspended = self._suspensions[slot]
            if suspended is None:
                yield slot, [self._xts[slot]]
                continue
            chain = []
            xt = getattr(suspended.cursor, "xt", None)
            if xt is not None:
                chain.append(xt)
            for entry in snapshot(suspended.context.returns):
                if isinstance(entry, Continuation):
                    chain.append(entry.xt)
            yield slot, chain

    # -- starting and stopping ---------------------------------------------

    def _context(self, slot: int):
        from simulator.runtime import ExecutionContext

        if self.base < 0:
            raise ExecutionError(
                "the dictionary starts too low for the task slots' stacks"
            )
        memory = self.runtime.memory
        data_floor = self.base + (slot - 1) * SLOT_BYTES
        data_top = data_floor + SLOT_STACK_BYTES
        return ExecutionContext(
            data=DataStack(memory=memory, floor=data_floor, empty_pointer=data_top),
            returns=ReturnStack(
                memory=memory, floor=data_top,
                empty_pointer=data_top + SLOT_STACK_BYTES,
            ),
        )

    def _drop_suspension(self, slot: int) -> None:
        suspended = self._suspensions[slot]
        self._suspensions[slot] = None
        if suspended is not None:
            self.runtime._abandon_scheduled(suspended)

    def start(self, slot: int, xt: int) -> None:
        """BACKGROUND: the slot starts XT afresh on its own empty stacks,
        whatever it held."""

        self._drop_suspension(slot)
        self._contexts[slot] = self._context(slot)
        self._xts[slot] = xt
        self._generations[slot] += 1

    def stop(self, slot: int) -> None:
        """TASK-STOP: the slot ends. A slot that stops itself runs on until
        it yields, and is then dropped."""

        self._drop_suspension(slot)
        self._xts[slot] = 0

    # -- running ------------------------------------------------------------

    def _enter(self, slot: int) -> None:
        """Make SLOT the one running, 0 for the foreground: the task TASK-ID
        names, in the Python words and in the native engine's."""

        self.active = slot
        native = self.runtime._native_execution
        if native is not None:
            native.program.set_task(slot)

    def _require_foreground(self, word: str, *, in_slot: bool) -> None:
        """The illegal-operation fault where WORD cannot run, as the BIOS's
        word takes it: on a worker core, where the slots are not, and in
        the foreground (IN_SLOT false) or a slot (IN_SLOT true), wherever
        it does not belong."""

        runtime = self.runtime
        runtime.require_core0(f"{word} and the task slots")
        if in_slot and self.active == 0:
            raise IllegalInstructionFault(
                f"{word} runs in a task slot; core 0's foreground has no slot "
                "to leave"
            )
        if not in_slot and self.active != 0:
            raise IllegalInstructionFault(
                f"{word} runs in core 0's foreground; task slot {self.active} "
                "gives its turn back with TASK-YIELD"
            )

    def pause(self) -> None:
        """PAUSE: from core 0's foreground, run the next active slot until it
        yields or ends; with no active slot, nothing."""

        runtime = self.runtime
        self._require_foreground("PAUSE", in_slot=False)
        slot = self.index
        for _attempt in range(SLOTS):
            slot = 1 if slot >= SLOTS else slot + 1
            if self._xts[slot]:
                break
        else:
            return
        self.index = slot
        generation = self._generations[slot]
        meter = runtime._active_dispatches[-1].meter
        suspended = self._suspensions[slot]
        self._suspensions[slot] = None
        self._enter(slot)
        try:
            if suspended is None:
                word = runtime._resolve_dispatch_word(self._xts[slot])
                outcome = runtime._execute_guarded(
                    word, self._contexts[slot], meter,
                    publish=False, task_slot=True,
                )
            else:
                outcome = runtime._resume_scheduled(
                    suspended, None, task_slot=True,
                )
        except BaseException:
            # An abort or a failure ends the slot; on the machine an ABORT
            # in a slot also ends the foreground's line, as it does here.
            if self._generations[slot] == generation:
                self._xts[slot] = 0
            raise
        finally:
            self._enter(0)
            runtime._task_yield_requested = False
        if self._generations[slot] != generation or not self._xts[slot]:
            # Started again or stopped while it ran: the old run goes.
            if outcome is not None:
                runtime._abandon_scheduled(outcome)
        elif outcome is None:
            # The slot's word returned, as the BIOS's task_cleanup sees it.
            self._xts[slot] = 0
        else:
            self._suspensions[slot] = outcome

    def task_yield(self) -> None:
        """TASK-YIELD: a running slot gives the foreground its turn back at
        the next operation of the slot's own dispatch."""

        self._require_foreground("TASK-YIELD", in_slot=True)
        self.runtime._task_yield_requested = True


__all__ = [
    "SLOTS",
    "SLOT_CELLS",
    "SLOT_REGION_BYTES",
    "HostedTasks",
]
