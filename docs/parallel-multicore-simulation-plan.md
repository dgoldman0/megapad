# Parallel multicore simulation

Status: direction accepted on 2026-10-27; nothing is implemented. This note
reopens decision P3-D6 of the
[phase 3 plan](megapad-concurrency-phase3-plan.md), longer private lookahead
through speculative writes and rollback, as a direction. P3-D6's condition
still applies: it lands only as a separately reviewed optimization with
one-, two- and four-lane differential evidence.

## 1. Why

On a machine with two or more awake full cores, the emulator runs them in
lock-step passes on one host thread
([single-core-execution-kernel-plan.md](single-core-execution-kernel-plan.md),
EK-D27). On 2026-10-09 a memory-heavy loop cost 82 ns per step with two awake
cores and 68 ns with four, and a loop shaped like threaded Forth cost 110 ns,
against 25 to 32 ns for a lone core. Splitting work across cores therefore
lowers total guest throughput on the host. The hosted simulator supports
exactly one full core by contract ([simulator-contract.md](simulator-contract.md)).

The constraint is fixed: host scheduling must never choose a guest-visible
winner ([phase 2 handoff](megapad-concurrency-phase2-handoff.md)). Every race
must resolve exactly as lock-step resolves it.

The earlier parallel scheme ran each core's private prefix on helper lanes:
the register-only instructions before its next shared or uncertain access
(phase 3, the safe bounded segments that P3-D6 chose). Forth touches memory
every few instructions, so those segments stay short. A loop that touched
memory every few instructions cost 362 ns per step on one host lane and
741 ns on four, which is why lock-step replaced the coordinator for
cluster-free machines.

## 2. Direction: run each round speculatively, then check it

1. Each awake core runs one round on its own host thread, starting from a
   checkpoint, and records which memory it reads and writes.
2. After the round, the records are compared. If no core read or wrote memory
   that another core wrote during the round, the outcome equals lock-step's,
   and the round commits.
3. Otherwise the round is discarded, the checkpoint restored, and the round
   rerun in lock-step.

The host never picks a winner, because any round in which order could matter
is redone the deterministic way. MMIO, mailbox and IPI traffic, DMA and
device events stay shared boundaries that end a round.

## 3. What it needs

- Write logs or copy-on-write checkpoints for each round, and read tracking
  fine enough that false conflicts stay rare.
- Differential evidence against lock-step on one, two and four lanes, as
  P3-D6 requires. `tests/test_lockstep_rounds.py` is the selector to extend.
- Guest software that keeps cores apart. Rounds commit when each core works
  on its own stacks and buffers and hands work over through the mailbox. The
  chip favors the same discipline: it has no data cache or coherence, and all
  cores share one main bus.

## 4. Limits

- Rounds with real sharing, such as spinlock contention, rerun in lock-step,
  so they run at lock-step speed or slightly slower.
- The strict cycle model also simulates bus contention between cores, which
  couples every memory access. It stays serial unless a separate design
  handles arbitration.
- The hosted simulator needs a multicore model of its own before any of this
  applies to it.
- Microcore clusters keep the coordinator today (EK-D27). Whether clustered
  machines adopt this scheme is open.
