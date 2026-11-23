"""The hosted simulator's full cores and the rounds they run in.

A machine profile with more than one full core runs its cores in rounds
(docs/multicore-simulator-plan.md, sections 4.2 to 4.4). The native
``CoreScheduler`` (``simulator/accel/core_scheduler.h``) decides whose turn
comes next and when a sleeping core wakes. This module is the glue that runs
each turn on the runtime's dispatcher and tells the scheduler what ended it.

Core 0 runs the host's dispatch: the one a session or a caller of
``MegaForthRuntime.run_until_blocked`` started. Its turns end where that
dispatch suspends. A host call returns at the end of a round, or inside
core 0's turn when the host's own quantum is the shorter. The other cores
run the BIOS worker loop: a worker sleeps until a full core sends it a job
through ``WAKE-CORE`` or ``IPI-SEND``, runs the job on fresh stacks, and
sleeps again when the job returns, faults, aborts or quits. Any other signal
is a wake-up, which it acknowledges, as the BIOS's IPI handler does. Workers
take their turns only inside a host call that runs core 0's dispatch, or in
``run_rounds`` while core 0 runs none; host evaluation of source runs no
dispatch and gives them no turn.

Time follows the round: every turn starts with the Timer at the round's
start, and the round's end moves it on by the round's longest turn.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from simulator.errors import ExecutionError, ForthAbort
from simulator.stacks import DataStack, ReturnStack

if TYPE_CHECKING:
    from simulator.runtime import MegaForthRuntime


MAX_FULL_CORES = 4
# A turn's quantum in semantic steps, fixed in the machine profile and the
# same for both executors (decision 2 of the plan).
TURN_QUANTUM_STEPS = 65_536
# The steps a chiplet may take in one visit while every core sleeps: the
# emulator's IDLE_STEP_LIMIT (emulator/accel/mp64_accel.cpp).
REST_STEP_LIMIT = 4096
# Each full core's stack zone, its return stack in the upper half and its
# data stack in the lower, as bios.asm lays them out.
STACK_ZONE_BYTES = 0x10000
WORKER_DATA_BELOW_TOP = 0x8000
# The zones lie below Bank 0's top or 1 MiB, whichever is lower.
STACK_ZONES_LIMIT = 1 << 20


class _NotRun:
    __slots__ = ()

    def __repr__(self) -> str:
        return "NOT_RUN"


# What run_host returns when core 0 slept through the round.
NOT_RUN = _NotRun()


def stack_zone_top(bank0, core: int) -> int:
    """The top of full core ``core``'s stack zone in Bank 0."""

    return min(bank0.limit, STACK_ZONES_LIMIT) - core * STACK_ZONE_BYTES


class HostedCores:
    """The full cores of one runtime, their worker loop and their rounds."""

    def __init__(
        self,
        runtime: MegaForthRuntime,
        *,
        full_cores: int,
        quantum: int,
    ) -> None:
        from simulator.native_execution import load_native_extension

        if isinstance(quantum, bool) or not isinstance(quantum, int):
            raise TypeError("the turn quantum must be an integer")
        if quantum < 1:
            raise ValueError("the turn quantum must be positive")
        if runtime._machine_owner is not None:
            raise ExecutionError(
                "machine routines run on the one-core profile alone"
            )
        extension = load_native_extension(required=True)
        self.runtime = runtime
        self.full_cores = full_cores
        self.scheduler = extension.CoreScheduler(full_cores, quantum)
        self._turn_end = extension.TurnEnd
        self._idle = extension.CoreState.IDLE
        self.active = 0
        # The worker slots, which CORE-STATUS reads: the job each core was
        # last given, zero once it ends. Core 0's slot takes a job and keeps
        # it, since core 0 has no worker loop.
        self._jobs = [0] * full_cores
        # Each worker's job between its turns; core 0's suspension is the
        # host's (MegaForthRuntime._suspended_execution).
        self._suspensions: list[object | None] = [None] * full_cores
        self._full_mask = (1 << full_cores) - 1
        bank0 = runtime._bank0
        self._zone_tops = [stack_zone_top(bank0, core) for core in range(full_cores)]
        if self._zone_tops[-1] - STACK_ZONE_BYTES <= bank0.base + bank0.size // 4:
            raise ValueError("Bank 0 is too small for every full core's stacks")
        platform = runtime.memory.mmio
        self.mailbox = platform.mailbox
        self.table = self.mailbox.table
        self._platform_table = platform.table
        # A worker's IPI handler takes a chiplet's signal as soon as one of
        # the worker's own accesses steps the chiplet into raising it.
        platform.signal_takers.takers = self._full_mask & ~1
        # The host's quantum for core 0's dispatch, kept while it runs.
        self._host_quantum: int | None = None
        # Whether another core has run since core 0's dispatch suspended:
        # its stacks may have changed, as on the machine.
        self.others_ran = False
        # The other cores' steps since the host last took them.
        self.other_steps = 0
        self._round_start_counter: int | None = None
        # The step meter's count when core 0's open turn in a host
        # evaluation last took its steps (run_word).
        self._open_mark: int | None = None

    # -- the cores' state -----------------------------------------------------

    def job(self, core: int) -> int:
        return self._jobs[core]

    def state(self, core: int) -> str:
        return self.scheduler.state(core).name.lower()

    def steps(self, core: int) -> int:
        return self.scheduler.steps(core)

    def status(self) -> list[dict[str, object]]:
        """Each core's state, semantic steps, turns and job, for host
        diagnosis."""

        scheduler = self.scheduler
        return [
            {
                "core": core,
                "state": self.state(core),
                "semantic_steps": scheduler.steps(core),
                "turns": scheduler.turns(core),
                "job": self._jobs[core],
                "deadline_ms": scheduler.deadline(core),
            }
            for core in range(self.full_cores)
        ]

    def suspended_call_chains(self):
        """For each worker whose job is suspended, the execution tokens of
        the words it runs inside: the word it stopped in, then its
        callers'."""

        from simulator.stacks import Continuation

        snapshot = self.runtime._stack_snapshot
        for core in range(1, self.full_cores):
            suspended = self._suspensions[core]
            if suspended is None:
                continue
            chain = []
            xt = getattr(suspended.cursor, "xt", None)
            if xt is not None:
                chain.append(xt)
            for entry in snapshot(suspended.context.returns):
                if isinstance(entry, Continuation):
                    chain.append(entry.xt)
            yield core, chain

    # -- the mailbox, as the BIOS words use it ---------------------------------

    def ipi_lines(self) -> int:
        """A bit for each core whose IPI line is raised."""

        any_pending = self.table.any_pending
        lines = 0
        for core in range(self.full_cores):
            if any_pending(core):
                lines |= 1 << core
        return lines

    def acknowledge_all(self, core: int) -> None:
        """``core`` takes every signal pending for it."""

        table = self.table
        mask = table.pending_mask(core)
        while mask:
            low = mask & -mask
            table.acknowledge(core, low.bit_length() - 1)
            mask ^= low

    def take_signals(self, core: int) -> None:
        """A worker takes its pending signals as the BIOS's IPI handler does:
        a full core's job enters its slot, and every signal is
        acknowledged."""

        mask = self.table.pending_mask(core)
        if not mask:
            return
        if mask & self._full_mask:
            self._jobs[core] = self.mailbox.data(core)
        self.acknowledge_all(core)

    def wake_core(self, xt: int, core: int) -> None:
        """``WAKE-CORE``: the job enters the core's slot, then goes by IPI."""

        self._jobs[core] = xt
        self.runtime.ipi_send(xt, core)

    # -- turns ---------------------------------------------------------------

    def switch_to(self, core: int) -> None:
        """Make ``core`` the one whose turn runs."""

        self.active = core
        self._platform_table.requester = core
        native = self.runtime._native_execution
        if native is not None:
            native.program.set_core(core)

    def apply_wakes(self) -> None:
        runtime = self.runtime
        lines, terminal = self._wake_sources()
        self.scheduler.apply_wakes(lines, terminal, runtime.rtc.uptime_ms)

    def _wake_sources(self) -> tuple[int, bool]:
        """The IPI lines and terminal input that may wake a core. Core 0
        wakes only while it has a host dispatch to continue."""

        lines = self.ipi_lines()
        terminal = self.runtime.uart_input_available
        if self.scheduler.state(0) == self._idle:
            lines &= ~1
            terminal = False
        return lines, terminal

    def wake_due(self) -> bool:
        """Whether a host call would run anything: an awake core, or a
        sleeping one whose wake rule holds."""

        scheduler = self.scheduler
        if scheduler.any_awake():
            return True
        lines, terminal = self._wake_sources()
        return scheduler.wake_mask(lines, terminal, self.runtime.rtc.uptime_ms) != 0

    def earliest_deadline(self) -> int | None:
        return self.scheduler.earliest_deadline

    def _start_turn(self) -> None:
        timer = self.runtime.timer
        if self._round_start_counter is None:
            self._round_start_counter = timer.counter
        else:
            timer.set_counter(self._round_start_counter)
        self.runtime._turn_end_requested = False

    def _finish_round(self) -> None:
        length = self.scheduler.finish_round()
        start = self._round_start_counter
        self._round_start_counter = None
        if start is not None:
            self.runtime.timer.set_counter(start + length)
        # The chiplets step at the end of each round (plan section 4.7), and
        # the input the host gave during the round arrives.
        ports = self.runtime.chiplet_ports
        if ports is not None:
            ports.step()
        self.runtime._admit_staged_uart_input()

    @property
    def round_open(self) -> bool:
        """Whether a round is under way: a turn runs or the round has
        served one and not yet ended."""

        scheduler = self.scheduler
        return scheduler.current >= 0 or scheduler.round_started

    def _rest_chiplets(self) -> None:
        """While every core sleeps, step the chiplets until they rest or
        ring a core, at most REST_STEP_LIMIT times, as the emulator steps
        them while its cores sleep. Stepping moves no clock, so a deadline
        is no reason to stop."""

        ports = self.runtime.chiplet_ports
        if ports is None:
            return
        raised = self.ipi_lines()
        for _ in range(REST_STEP_LIMIT):
            more = ports.step()
            if self.ipi_lines() & ~raised or not more:
                return

    def start_host(self, stint, meter, quantum_steps: int | None):
        """Begin core 0's host dispatch and run the machine with it.

        ``stint(budget)`` runs core 0's dispatch for at most ``budget`` steps
        and returns its suspension, or None once it completes. Returns what
        the last stint returned.
        """

        self._host_quantum = quantum_steps
        self.scheduler.wake(0)
        return self.run_host(stint, meter)

    def run_host(self, stint, meter):
        """Run turns until core 0's stint reaches a host boundary.

        Returns at the end of the round in which core 0's turn ended, or in
        its turn when the host's quantum ends first, with what the stint
        returned; or NOT_RUN after one round in which core 0 slept.
        """

        scheduler = self.scheduler
        turn_end = self._turn_end
        self.apply_wakes()
        outcome = NOT_RUN
        while True:
            core = scheduler.current
            if core < 0:
                core = scheduler.begin_next_turn()
                if core >= 0:
                    self._start_turn()
            if core < 0:
                if scheduler.round_started:
                    self._finish_round()
                if outcome is not NOT_RUN or not scheduler.awake(0):
                    if not scheduler.any_awake():
                        self._rest_chiplets()
                    return outcome
                # Core 0's turn has not come in this call: the next round
                # begins with it.
                continue
            if core > 0:
                self._worker_turn(core)
                self.apply_wakes()
                continue
            if outcome is not NOT_RUN:
                raise AssertionError("core 0 took two turns in one host call")
            budget = scheduler.turn_left
            quantum = self._host_quantum
            if quantum is not None and quantum < budget:
                budget = quantum
            self.switch_to(0)
            before = meter.steps
            try:
                suspended = stint(budget)
            except BaseException:
                scheduler.account(meter.steps - before)
                scheduler.end_turn(turn_end.DONE)
                self._host_quantum = None
                raise
            scheduler.account(meter.steps - before)
            self.others_ran = False
            if suspended is None:
                scheduler.end_turn(turn_end.DONE)
                self._host_quantum = None
                outcome = None
            elif suspended.cursor.host_yield:
                spin = self.runtime._turn_end_requested
                if not spin and scheduler.turn_left:
                    # The host's quantum ended inside the turn.
                    return suspended
                scheduler.end_turn(turn_end.SPIN if spin else turn_end.QUANTUM)
                outcome = suspended
            else:
                scheduler.end_turn(
                    turn_end.SLEEP, self.runtime._idle_deadline_ms
                )
                outcome = suspended
            self.apply_wakes()

    def run_word(self, word, context, meter) -> None:
        """Run to its end a word that the host's evaluator executes on core 0.

        The evaluator is core 0's outer interpreter: the words it executes
        run in core 0's turns, and a turn stays open from one word to the
        next, so the other cores take their turns as they would beside the
        BIOS's interpreter. The evaluator cannot suspend to its host, so the
        word's dispatch stays here: a turn that ends at its quantum or at a
        busy spinlock continues in the next round, and core 0 sleeps in IDL
        while another core runs and its own wake rule does not hold. Nothing
        delivers input or moves the clock while the host evaluates, so once
        no other core is awake an IDLE-UNTIL ends early, as it may, and an
        IDL without a deadline fails, since nothing could end it.
        """

        runtime = self.runtime
        scheduler = self.scheduler
        turn_end = self._turn_end
        if scheduler.current == 0:
            # The open turn takes what core 0 ran since the last word.
            self._account_open_turn(meter)
        if scheduler.state(0) == self._idle:
            scheduler.wake(0)
        suspended = None
        after_sleep = False
        try:
            while True:
                core = scheduler.current
                if core == 0 and not scheduler.turn_left:
                    scheduler.end_turn(turn_end.QUANTUM)
                    core = -1
                if core < 0:
                    core = scheduler.begin_next_turn()
                    if core >= 0:
                        self._start_turn()
                if core < 0:
                    if scheduler.round_started:
                        self._finish_round()
                    self.apply_wakes()
                    if not scheduler.any_awake():
                        self._rest_chiplets()
                        self.apply_wakes()
                    if not scheduler.any_awake():
                        if scheduler.deadline(0) is None:
                            raise ExecutionError(
                                "IDL in source evaluation has no other core "
                                "awake to end it; use run_until_blocked on a "
                                "compiled word"
                            )
                        scheduler.wake(0)
                    continue
                if core > 0:
                    self._worker_turn(core)
                    self.apply_wakes()
                    continue
                budget = scheduler.turn_left
                self.switch_to(0)
                before = meter.steps
                try:
                    if suspended is None:
                        outcome = runtime._execute_guarded(
                            word,
                            context,
                            meter,
                            allow_idle=True,
                            quantum_steps=budget,
                            publish=False,
                        )
                    else:
                        if after_sleep:
                            runtime._end_sleep()
                            after_sleep = False
                        prior, suspended = suspended, None
                        outcome = runtime._resume_scheduled(prior, budget)
                finally:
                    scheduler.account(meter.steps - before)
                if outcome is None:
                    # The turn stays open for the evaluator's next word.
                    self._open_mark = meter.steps
                    return
                suspended = outcome
                if outcome.cursor.host_yield:
                    scheduler.end_turn(
                        turn_end.SPIN if runtime._turn_end_requested
                        else turn_end.QUANTUM
                    )
                else:
                    scheduler.end_turn(turn_end.SLEEP, runtime._idle_deadline_ms)
                    after_sleep = True
                self.apply_wakes()
        except BaseException:
            if scheduler.current == 0:
                self._open_mark = meter.steps
            if suspended is not None:
                # Core 0's word will not resume: unwind it and free its
                # context, as cancelling the host's suspension does.
                runtime._abandon_scheduled(suspended)
            raise

    def _account_open_turn(self, meter) -> None:
        """Charge core 0's open turn with what it ran between words, such as
        a callback the evaluator ran itself."""

        mark = self._open_mark
        if mark is not None and meter.steps > mark:
            self.scheduler.account(meter.steps - mark)
        self._open_mark = None

    def end_evaluation(self, meter, *, completed: bool) -> None:
        """The host's evaluation has ended, and core 0's work with it.

        Core 0's turn ends and it goes idle. When the evaluation completed,
        the other cores take the rest of the round's turns; after a failure
        the round only ends, and they take their turns in the next one.
        """

        scheduler = self.scheduler
        if scheduler.current == 0:
            self._account_open_turn(meter)
            scheduler.end_turn(self._turn_end.DONE)
        elif scheduler.state(0) != self._idle:
            scheduler.rest(0)
        self._open_mark = None
        if completed and scheduler.round_started:
            while True:
                core = scheduler.begin_next_turn()
                if core < 0:
                    break
                self._start_turn()
                self._worker_turn(core)
                self.apply_wakes()
        if scheduler.round_started and scheduler.current < 0:
            self._finish_round()
        self.switch_to(0)
        if completed and not scheduler.any_awake():
            self._rest_chiplets()

    def run_rounds(self, max_rounds: int | None = None) -> int:
        """Run rounds while core 0 runs no host dispatch, until no core is
        awake or ``max_rounds`` have run. Returns the rounds run."""

        runtime = self.runtime
        if runtime._suspended_execution is not None:
            raise ExecutionError(
                "core 0's dispatch is suspended; resume it to run the cores"
            )
        scheduler = self.scheduler
        if scheduler.state(0) != self._idle or scheduler.current >= 0:
            raise ExecutionError("core 0 is running a host dispatch")
        rounds = 0
        self.apply_wakes()
        while max_rounds is None or rounds < max_rounds:
            if not scheduler.any_awake():
                self._rest_chiplets()
                self.apply_wakes()
                if not scheduler.any_awake():
                    break
            while True:
                core = scheduler.begin_next_turn()
                if core < 0:
                    break
                self._start_turn()
                self._worker_turn(core)
                self.apply_wakes()
            if scheduler.round_started:
                self._finish_round()
            rounds += 1
            self.apply_wakes()
        return rounds

    # -- the worker loop -----------------------------------------------------

    def _context(self, core: int):
        """Fresh stacks for a job, where bios.asm puts a worker's."""

        from simulator.runtime import ExecutionContext

        memory = self.runtime.memory
        top = self._zone_tops[core]
        data_top = top - WORKER_DATA_BELOW_TOP
        return ExecutionContext(
            data=DataStack(
                memory=memory,
                floor=top - STACK_ZONE_BYTES,
                empty_pointer=data_top,
            ),
            returns=ReturnStack(
                memory=memory,
                floor=data_top,
                empty_pointer=top,
            ),
        )

    def _worker_turn(self, core: int) -> None:
        runtime = self.runtime
        scheduler = self.scheduler
        turn_end = self._turn_end
        self.switch_to(core)
        self.others_ran = True
        suspended = None
        meter = None
        before = 0
        ended = False
        try:
            self.take_signals(core)
            budget = scheduler.turn_left
            prior = self._suspensions[core]
            if prior is None:
                xt = self._jobs[core]
                if xt == 0:
                    scheduler.end_turn(turn_end.DONE)
                    ended = True
                    return
                from simulator.runtime import _StepMeter

                meter = _StepMeter(None, runtime._account_semantic_step)
                try:
                    word = runtime._resolve_dispatch_word(xt)
                except KeyError:
                    raise ExecutionError(
                        f"core {core}'s job 0x{xt:016x} is not a live "
                        "execution token"
                    ) from None
                suspended = runtime._execute_guarded(
                    word,
                    self._context(core),
                    meter,
                    allow_idle=True,
                    quantum_steps=budget,
                    publish=False,
                )
            else:
                meter = prior.meter
                before = meter.steps
                self._suspensions[core] = None
                suspended = runtime._resume_scheduled(prior, budget)
        except ForthAbort:
            # ABORT, ABORT" or QUIT ended the job, or a fault did once
            # reported; the QUIT-XT! callback ran on the job's stacks.
            suspended = None
        finally:
            if not ended:
                if meter is not None:
                    scheduler.account(meter.steps - before)
                    self.other_steps += meter.steps - before
                if suspended is None:
                    self._jobs[core] = 0
                    scheduler.end_turn(turn_end.DONE)
                else:
                    self._suspensions[core] = suspended
                    if suspended.cursor.host_yield:
                        scheduler.end_turn(
                            turn_end.SPIN
                            if runtime._turn_end_requested
                            else turn_end.QUANTUM
                        )
                    else:
                        scheduler.end_turn(
                            turn_end.SLEEP, runtime._idle_deadline_ms
                        )
            runtime._turn_end_requested = False
            self.switch_to(0)


__all__ = [
    "MAX_FULL_CORES",
    "NOT_RUN",
    "STACK_ZONE_BYTES",
    "TURN_QUANTUM_STEPS",
    "WORKER_DATA_BELOW_TOP",
    "HostedCores",
    "stack_zone_top",
]
