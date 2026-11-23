# Multicore simulator plan

Status: Accepted, 2026-11-22, with the nine decisions of section 10.
Stages 1 and 2 are built (MegaPad d8016c43; 4ea4bc91, 09a26e8d and
f317327b), stage 3 (1c4563f4 to 49d94ecd, with Akashic 95e081c9), stage 4
(d8453449 to 348a482a, with Akashic d82d6e80 to 5da2eea3), stage 5
(ee95d589, with Akashic a603041c) and stage 9 (Akashic 7cb83d31 to
7832caa6).
It is the staged plan for Part 3 of the release plan
(`dev/PLAN.md` in fantasy-engine), in which MegaPad's hosted simulator
becomes the multicore machine MegaPad is. Section 1 describes the simulator
as it stands, section 2 says what matching the emulator's lock-step can mean
for a simulator that runs Forth semantically, section 3 lists what each step
of Part 3 needs, section 4 sets out the design the stages share, and section
5 gives the stages with their tests, estimates and closing costs. Sections 6
to 9 cover the order of the work, the RTL prerequisite for microcores, the
work in flight that the plan touches, and what the investigation found that
the release plan and the simulator contract should take up. Section 10
records the decisions taken with the plan.

## 1. The simulator today

The hosted simulator runs MegaForth, KDOS and Akashic from their source. It
installs a semantic implementation of the BIOS vocabulary and compiles Forth
into operations of its own, which the native engine
(`simulator/accel/semantic_executor.cpp`) and the reference dispatcher in
`simulator/runtime.py` execute; it runs no MP64 instructions. Its contract,
`docs/simulator-contract.md`, fixes one full core:

- Runtime construction refuses a SysInfo topology other than one full core
  and no cluster.
- `COREID` and `TASK-ID` push 0, and `CORE-STATUS` accepts only core 0.
  `WAKE-CORE`, `IPI-SEND`, `BACKGROUND`, `BACKGROUND2`, `BACKGROUND3` and
  `TASK-STOP` fail without consuming their operands. `CLUSTER-EN!` accepts
  only zero, and the barrier and cluster privilege words fail.
- `PAUSE`, `TASK-YIELD`, `IPI-STATUS`, `IPI-ACK`, `MBOX!` and `MBOX@` are
  absent from the hosted vocabulary. KDOS loads all the same, because none
  of its definitions compiles a call to them.
- One compiled-word suspension exists at a time, at `IDL` or at a host
  quantum, and dictionary mutation waits while it is detached.
  `IDLE-UNTIL` ends on UART input or at its deadline; the profile has no IPI
  to end it.
- The services the machine keeps per core already take a core ID: SHA-2
  contexts, the Field ALU, the spinlock bank's owners and the tile engine's
  register file. Every caller identity is `(COREID, TASK-ID) = (0, 0)`, and
  the scalar FP unit's `FPCSR` and the tile service with its full-width
  accumulator (Part 3f, built on 2026-11-20) exist once, as core 0's.
- `OneCorePlatformMMIO` in `simulator/platform.py` hard-codes every MMIO
  window: SysInfo, AES, SHA-3, TRNG, RTC and audio, and, when the address
  space is made with `chiplet_ports`, the port window and a mailbox for core
  0. Guest memory hands each MMIO byte to the Python object that owns the
  window, and no access names its requester. The native engine serves
  ordinary memory only and returns every MMIO access to Python.
- Attached chiplets step before each read of a port register or of
  `MBOX_STATUS`, and when a dispatch returns to its host.

The emulator runs one to four full cores and up to three clusters of four
microcores. Each round gives every awake core up to 1,000 instructions, and a
round with several awake full cores runs as lock-step passes: each core runs
its register-only instructions and then executes its next memory
instruction in turn, so cores interleave at every memory access
(`tests/test_lockstep_rounds.py`). Secondary full cores boot into the BIOS
worker loop. They sleep in `IDL`; the IPI handler copies the sender's mailbox
word into the core's slot of `worker_xt_table`; the loop runs that execution
token on fresh stacks and clears the slot when it returns. `WAKE-CORE`,
`CORE-STATUS` and KDOS's `CORE-RUN`, `CORE-WAIT`, `BARRIER` and `LOCK` rest
on that loop, and so do Akashic's `worker-job.f` and Desk's sandbox job
service, which lends sandbox jobs to every other full core
(`SBOX-JOB-OTHER-CORES` in every Desktop autoexec).

A scheduler that runs each core's turn through the simulator's present
yield and resume path crosses one boundary per turn. On the native engine a
loop of `I ACC +!` runs at 2.4 ns a step when nothing interrupts it, and each
boundary adds 18 to 23 µs:

| Quantum, semantic steps | Boundaries in 14.0 million steps | ns per step |
|---|---|---|
| none | 0 | 2.4 |
| 65,536 | 213 | 2.3 |
| 16,384 | 854 | 3.4 |
| 4,096 | 3,417 | 7.8 |
| 1,024 | 13,667 | 23.0 |
| 256 | 54,545 | 87.1 |

These figures come from one shared host under a load of 2.3 to 2.9, best of
three runs with `PYTHONHASHSEED=0`, on MegaPad d1f0ea17. Today's path
therefore suits a quantum of about 65,536 steps, and a quantum near 1,000
steps needs the switch between cores to happen inside the native engine.

## 2. What matching lock-step means here

Step 3a asks that the simulator's guest-visible results match the
emulator's lock-step, so that host scheduling never picks a winner. The
emulator interleaves cores at each memory instruction. The simulator
executes semantic operations whose memory accesses differ from the BIOS's
machine code in number and order: a hosted `+!` is one operation where the
BIOS's is a load and a store, a hosted colon call keeps its continuation on
a semantic return stack, and a hosted `MOVE` copies its whole span at once.
No simulator schedule reproduces the emulator's interleaving of a program
whose cores race. The emulator's own interleaving of such a program also
follows its round reservations, and the last round of each batch shrinks to
what the batch has left, so its outcome can change with the way its host
divides the run into batches.

Here 3a means three promises, which stage 3 writes into the contract:

1. **Determinism.** A run's results depend only on the initial state, the
   machine profile (the topology and the round quantum among it), the clock
   policy, entropy and the ingress schedule. The Python and native
   executors give the same results on any host, and so does any number of
   host lanes once parallel rounds exist (3b). The host never picks a
   winner.
2. **An outcome the architecture allows.** Each semantic operation changes
   memory as one unbroken run of the BIOS's accesses would, so every
   simulator run equals an interleaving the architecture permits, with cores
   switching only between operations. The claim covers every byte except
   those the contract already lets differ: code, return addresses and
   absolute dictionary addresses.
3. **Equal results where synchronization fixes them.** For a program whose
   outcome follows from its synchronization, through mailbox jobs, the
   worker slots, spinlocks, IPIs and the per-core buffers of KDOS's
   concurrency contract, the simulator's guest-visible results equal the
   emulator's on one, two and four full cores. The differential suite checks
   exactly this. A racing program gets one fixed outcome per profile on the
   simulator, which may differ from the emulator's; the contract already
   leaves physical multicore races outside its claims.

Decision 1 adopts this reading.

## 3. What each step of Part 3 needs

**3a, full cores.** Several execution contexts with their own stacks, each
able to sit suspended while others run; a deterministic scheduler that
chooses whose turn it is; the BIOS worker loop's behaviour, with
`WAKE-CORE`, `CORE-STATUS`, `IPI-SEND` and the four absent mailbox words; a
mailbox for every core in every profile; spinlocks that a second
core can find busy, also inside the native disk words that take the
filesystem lock; `IDL` wakes by IPI; the per-core engine state that today
exists once (`FPCSR`, the tile engine, the accumulator, the caller identity
of CRC, SHA-2 and SHA-3); a topology that SysInfo reports and runtime
construction admits; and a contract whose sections 2, 5, 6 and 10 describe
it.

**3a, clusters and microcores.** Microcores cannot run today's BIOS Forth
(`docs/microcore-kernels-plan.md` §1). The direction accepted on 2026-10-27
gives them kernel words, a restricted Forth subset that the BIOS JIT
compiles for them, and that document lists the kernel vocabulary, launch,
completion and the calling convention as open questions. A semantic
simulator can only run kernels on microcores, because it runs no machine
code, so this half of 3a waits for a design round on the kernel contract,
which the release plan gains as a step (decision 6). The cluster's devices,
`CLUSTER-EN`, the barrier, the scratchpad and the engines its four
microcores share, come with the kernels.

**3b, parallel rounds.** Each core's turn in a round must run in the native
engine from its start to its end, holding the core's stack state in C++, so
that turns can run on host threads with the interpreter lock released. Guest
memory must be dense for that, since sparse pages are Python objects, and
the commit rule needs read and write records for each turn (section 4.8).

**3c, attaching devices.** One interface through which a device claims its
MMIO windows, sees which core or DMA requester made each access, keeps state
per requester, and drives each core's IPI, external-flag and wake lines.
Every hosted window moves onto it, and the multicore devices are built on it
once (section 6).

**3d, chiplet modules.** Done. Its place in rounds is section 4.7.

**3e, microcore gaps.** The RTL microcore widens to 32 registers and decodes
REX's extension bits, and boot gives every core its stacks (section 7).

**3f, the tile accumulator.** Built for core 0 on 2026-11-20. Multicore makes
the tile engine and its accumulator per full core, as the hardware has them
(stage 4).

## 4. Design

### 4.1 Topology

A machine profile names its full cores (one to four) and its clusters (zero
to three), the shapes the emulator and the BIOS support. The BIOS sets the
limits: stacks for four full cores in the top 256 KiB of a 1 MiB Bank 0
(`CORE_STACK_TOPS` in the emulator, `MP64_CORE*_STACK_TOP` in the RTL
package), and 16 slots in `worker_xt_table`. SysInfo reports the topology as
the emulator's does: `NUM_CORES`, `NUM_FULL`, `CLUSTER_EN` with every
configured cluster enabled at reset, and `NUM_BUS_PORTS` as full cores plus
clusters plus three, where the one-core profile reports 1 today. Chiplet port
IDs follow the cores, as in the emulator and the RTL: the first port's ID is
the number of cores, 1 with one core, 4 with four full cores, and 16 with four
full cores and three clusters. A one-core profile keeps every value it has
now.

A general factory, `create_address_space`, takes the topology and replaces
`create_one_core_address_space`, `OneCoreSysInfo` and `OneCorePlatformMMIO`;
one mechanical commit updates their callers in 55 MegaPad files and 42
Akashic files.

### 4.2 Cores and the active core

Each core has a core state in the runtime: its execution context, with the
stacks where `bios.asm` places them (core 0's data stack grows down from Bank
0's midpoint and its return stack from Bank 0's top; core *n* of 1 to 3 has
its return stack at the top of its 64 KiB zone and its data stack 32 KiB
below that); its dispatch and suspension state; its `WAKE_MS` deadline; and
its step meter. The runtime keeps one active core, whose state its dispatcher
uses. A turn switch saves the active core's dispatch state and installs the
next core's; that swap is thin Python glue, and everything that decides
which core runs is C++ (section 4.3). `COREID` reads the active core. SHA-2,
the Field ALU and the spinlocks take the active core as requester, CRC and
SHA-3 take `(COREID, TASK-ID)` as owner, and each full core has its own
`FPCSR` and its own tile engine with its accumulator.

Today a resumed dispatch fails if its stacks changed while it slept. That
check guards against host code; on a multicore machine another core may
write any memory, a sleeping core's stacks included, and the machine has no
protection against it. The scheduler's own turn switches therefore skip the
comparison, host-requested resumes keep it, and the return stack's
continuation checks still refuse a corrupted return.

### 4.3 The round scheduler

The scheduler is C++. It holds each core's state (running, asleep in `IDL`
with its deadline, or a worker with an empty slot), the worker slots, the
round's accounting and the wake rules, and it asks the glue to run one turn
at a time.

- A round visits the awake cores in ascending core ID.
- A turn runs until the core has used its quantum, or sleeps in `IDL`, or
  finds a spinlock busy, or ends a worker job. A fixed quantum is part of
  the machine profile and is the same for both executors (decision 2).
- A core asleep in `IDL` wakes by the rule of `docs/isa-reference.md`: an
  interrupt request from an enabled source, which is an IPI for any core,
  UART input for core 0 and the Timer's request while its interrupt is
  enabled, or its `WAKE_MS` deadline. On core 0 the sleep then acknowledges
  every pending sender in the whole mask, as the BIOS's `idle_until` does.
- A worker takes a pending IPI at its next operation boundary: straight
  after the operation that raised it when the worker itself is running,
  otherwise at the start of its next turn. The effect follows the BIOS's
  handler as stage 2 corrected it: a full core's job enters the slot, and
  any other signal is acknowledged as a wake-up. A worker with a job in its
  slot runs it on fresh stacks; a job that returns clears the slot; a fault
  prints the BIOS's report line with the core's ID, runs the `QUIT-XT!`
  callback, clears the slot and leaves the worker idle, and an abort or
  `QUIT` ends the same way.
- Ingress enters only between rounds, so a scripted ingress schedule gives
  the same run on every host. Inside a turn the host may still settle UART
  output and release its lock, as the Python executor's 8,192-step
  boundaries do today.
- With one awake core a round is that core's turn, and the scheduler adds
  no semantic step, so the one-core profile keeps today's speed.

### 4.4 Time

The emulator advances a functional round clock. The plan gives the
simulator the same shape (decision 2): a round advances the Timer by its
longest turn, and within a turn `CYCLES` and the Timer's match logic see the
round's start plus the core's own steps. With one awake core this is today's
rule, one tick per semantic operation. With four busy cores time advances by
one quantum per round instead of four. Within one round a core can read a
value that another core wrote at a later local time, so `CYCLES` values
compare only within one core. The RTC's virtual uptime still moves only by
host advancement, and the interactive clock still follows the host.

### 4.5 Devices and their attachment

One C++ interface carries every MMIO access with its requester, a core ID or
a DMA requester, and its width, through a preflight and the bytewise access
the contract specifies. A device claims its windows when it attaches,
overlaps are refused then, and an access outside every window faults as it
does now. A device may keep state per requester and drives each core's IPI
request, external flag and wake request, which the scheduler reads.
Devices written in Python today, AES, SHA-3, TRNG, RTC and audio, attach
through a thin adapter and keep their models. From stage 8a the native
engine reaches the C++ devices directly, which speeds the simulator and
leaves the device's cost as it is.

The two devices multicore adds are written once in `shared/accel`, beside
`mailbox_pending.h` and `chiplet_port.h`, so the emulator can use the same
definitions (decision 4):

- the mailbox: each participant's data slot, `SEND`, `STATUS` and `ACK` by
  requester over the existing participant table, and the IPI and
  external-flag lines;
- the spinlock bank: 16 locks owned by the requesting core, with a refused
  acquire and owner-only release, which the native disk words, the checked
  SHA-3 service and HMAC all take.

The worker slots are scheduler state in the simulator. On the machine they
are BIOS memory that the BIOS's own code reads and writes, which the
emulator runs.

### 4.6 What a worker core may do

KDOS's concurrency contract keeps the heap, arena setup and dictionary on
core 0, and the rich terminal keeps input on core 0. KDOS's own `?CORE0`
guard already turns a worker's heap call into an abort. The simulator holds
a worker to the rest of that contract (decision 9): the compiler, the
evaluator, dictionary growth and rollback, and UART input run on core 0
alone, and a worker that reaches one takes the illegal-operation fault, code
-21, through its fault path. A dictionary rollback on core 0 that would
remove a word some core's suspended continuation runs inside is refused.
Words that only read the dictionary, `FIND` and `'` among them, work on
every core.

### 4.7 Chiplets, per-core logs and mailbox signals

A chiplet steps at the end of each round, before a core's read of a port
register or of its `MBOX_STATUS` (so a polling core makes progress inside
its turn), and, while every core sleeps, until it rests or rings a core, as
in the emulator. The one-core profile's step when a dispatch returns becomes
the end of the round. In parallel rounds a chiplet steps only in the
sequential parts of a commit (section 4.8), where memory holds exactly what
the sequential order holds at that point.

Managed data's store logs and window words are ordinary memory that each
core writes in program order. Every store of a turn is in memory before the
chiplet's next step, so LM1 sees each core's log entries and window changes
in that core's order, and the two-word protocol of `docs/managed-data.md` §3
holds as on a machine whose cores have no data cache. A doorbell to core 0
wakes `IDLE-UNTIL`, which acknowledges it; managed data's waits re-read
memory after waking, as §3 requires. A doorbell to a worker is acknowledged
as a wake-up. `MBOX_STATUS` shows IDs 0 to 7, which hold a port only while
the topology has fewer than eight cores; the BIOS's acknowledgements read
CSR `MBOX`'s whole mask, and Forth reads it with `IPI-PENDING`, which
`CHIP-WAIT` polls (finding 8).

### 4.8 Parallel rounds and their commit rule

Rounds stay the unit of determinism, and the sequential order of section
4.3 stays the reference: core 0's whole turn, then core 1's, and so on. In a
parallel round each awake core's turn starts speculatively on a host thread
from the round's start, keeping its stores in a private log, serving its own
later reads from that log, and recording which cells it read. A device
access, a Python-owned operation or host service ends a core's speculative
part.

The cores then commit one at a time in ascending order. Core *i*'s
speculative part commits when it read no cell that anything earlier in the
order wrote during the round: a lower-numbered core's committed log, the
sequential remainder of a lower-numbered core's turn, or a chiplet or device
that one of them caused to step. Its log is then applied, and the rest of its
turn runs sequentially before core *i*+1 commits. Otherwise core *i*'s log is
dropped and its whole turn reruns sequentially from its state at the round's
start, which nothing has changed, since its log never reached memory.

This yields exactly the sequential result. In the sequential order core
*i*'s turn begins on the memory its predecessors left. Every cell it read
speculatively is a cell they left as it was at the round's start, so each
read returns the same value, and by induction over core *i*'s steps it takes
the same branches and stores the same values; applying its log gives the
memory its sequential turn gives. A core that reruns produces its sequential
turn by construction, and the cores above it are checked against what it
actually wrote. The rule is narrower than the one in
`docs/parallel-multicore-simulation-plan.md`, which also discards rounds in
which two cores only wrote the same cell or a lower-numbered core read a cell
that a higher one wrote; both of those reproduce the sequential order once
logs apply in core order. Under the round clock of section 4.4, `CYCLES`
reads force no reruns, because each core's time depends only on the round's
start and its own steps.

## 5. Stages

Estimates are agent hours. A closing run is the slice-end `run --recorded`
of the test selection plus the patterns the slice's changes reach; its cost
is the serial machine time of the units it reruns, from their last recorded
durations, without the `final` set (the four ext4 `@emulator` units and
`tests/test_system.py`, 134 minutes together, which run only on request).
From the record on 2026-11-22: a change to `simulator/` or the simulator's
native library reruns about 370 units in about 70 minutes; with
`akashic_tui.py` as well, about 534 units in 118 minutes; a change to
`bios.asm` and `emulator/` about 509 units in 190 minutes. The physical
Desktop journey takes about 2.5 minutes.

| Stage | Builds | Agent hours | Closing cost | Unblocks |
|---|---|---|---|---|
| 1 | Devices attach through one interface (3c) | 6 to 12 | 70 min, journey | 3, 4 |
| 2 | The BIOS worker loop takes signals and aborts correctly, boot sets every full core's stacks, and the emulator uses the shared device models | 7 to 13 | 190 min, and `TestMulticore` and `TestKDOSMulticore` of `tests/test_system.py`, one class at a time | 3; LM1 8d stage 3 and borrowed tile work on the emulator |
| 3 | Full cores, the scheduler and the worker loop (3a) | 16 to 30 | 75 min, journey | 4, 5, 9 |
| 4 | Per-core engines, contention, chiplets in rounds, worker services (3a, 3f) | 8 to 16 | 120 min, journey | 9; LM1 8d stage 3 on the simulator; release plan 9f |
| 5 | Cooperative tasks on core 0 | 3 to 6 | 70 min | the contract's named target |
| 6 | The RTL microcore (3e) | 3 to 6 | RTL benches, minutes | 7 on the chip |
| 7 | Clusters and microcore kernels (3a) | 10 to 18 after the kernel contract | 70 min, and 150 min for the BIOS | microcore work |
| 8 | Parallel rounds (3b) | 22 to 44 | 70 min per part, journeys, the lane suite | simulator speed with busy workers |
| 9 | Desk with background work on extra cores | 4 to 8 | 70 min, journeys | Part 3's done-when |

The full-core path, stages 1 to 5 and 9, comes to about 44 to 85 agent hours,
decision 4's emulator work included, and about 10 hours of closing runs.
Recent rounds finished well inside their estimates: the move of the ext4
tests onto the simulator took 2.7 agent hours against an estimate in weeks,
and LM1's core model 75 minutes against 8 to 14 hours. The ranges here keep
their margin all the same, because a multicore mistake shows up as two runs
that differ, which takes longer to trace than a failing check.

### Stage 1. Devices attach through one interface (3c)

Builds the attachment interface and table of section 4.5 and the topology
description of section 4.1, still admitting only one core. SysInfo, AES,
SHA-3, TRNG, RTC, audio and the chiplet window move onto the table; the
mailbox and the spinlock bank become the `shared/accel` devices; the
mailbox exists in every profile, since the machine always has one; the
native disk words take lock 2 from the C++ bank instead of reading the
Python owner list. The bank's window stays unattached until stage 4 makes the
crypto guard's lock 8 the bank's, as the contract requires before direct
spinlock MMIO is admitted. `HostedChipletPorts` keeps its API (`link`,
`mailbox`, `first_port_id`, `step`, `work_waits`, `save`, `restore`), which
`tests/test_lm1_chiplet.py` uses.

Tests: every platform, SysInfo, crypto, RTC, entropy, audio and chiplet unit
of the simulator passes, with the expectations that change because the
mailbox now answers without ports updated in the same commit. New focused
tests cover overlap refusal, the requester reaching the device, the lines a
device drives, and faults outside every window. The shared mailbox and
spinlock models, as the simulator's extension builds them, answered seeded
streams of requester-tagged accesses exactly as the emulator's Python
`MailboxDevice` and `SpinlockDevice` did; stage 2 built them into the
emulator's extension, and `tests/test_shared_mmio_devices.py` now holds
both backends to a reference of the two register maps.

### Stage 2. The BIOS worker loop takes signals and aborts correctly

Section 9 describes the two defects; this stage corrects them in `bios.asm`
(decision 5), and takes the full cores' part of 3e's stacks into the same
BIOS change:

- the worker's IPI handler reads the whole pending mask through CSR `MBOX`,
  takes a job only from a full core, and acknowledges every other pending
  signal, so a chiplet's or microcore's signal wakes a worker as a wake-up
  and leaves its last job alone;
- `ABORT`, `ABORT"` and `QUIT` on a worker end the job as a fault does:
  the report, the cleared slot, the worker's own stacks and the idle loop;
- boot derives each full core's stacks from `COREID` and SysInfo, at the
  addresses they have now, so the emulator's boot stops setting R2 for the
  BIOS and the RTL needs no reset value for it (section 7).

Under decision 4 the emulator's Python `MailboxDevice` data slots and
`SpinlockDevice` move to the shared models in the same stage, so both
backends' multicore devices have one definition before stage 3 builds on
them.

Tests: emulator cases on two and four full cores in which a test chiplet
rings a worker that sleeps and one that runs a job, an aborting worker job
that leaves the worker idle and core 0's stacks intact, and the existing
multicore emulator tests (`test_lockstep_rounds.py`,
`test_chiplet_emulator.py`, `test_chiplet_mailbox.py`,
`test_concurrency_handoff.py`). `TestMulticore` and `TestKDOSMulticore` live
in `tests/test_system.py`, the `final` set's file; they run one class at a
time through `make test-sequential TEST_PATH=tests/test_system.py K=...`,
with each class's peak memory noted.
This stage can run beside stage 1, since they touch different files; their
closing runs go one at a time.

Built on 2026-11-22. The BIOS change is 4ea4bc91. Its `worker_quit` also
runs the `QUIT-XT!` callback on the worker's fresh stacks, which a bus fault
on a worker used to skip, leaving KDOS's exception chain for that core
pointing into the abandoned stack. Core 0's `idle_until` now acknowledges
from CSR `MBOX`'s whole mask, since with clusters a port's doorbell has an
ID above the eight `MBOX_STATUS` shows and used to keep every later sleep
awake. The emulator's boot stops presetting R2 and still gives a bare
program a stack pointer. The emulator's move onto the shared models is
f317327b: its cores decode the mailbox and the spinlocks natively, and the
Python classes remain as facades on the bus. 09a26e8d makes an access at an
MMIO offset of 2^32 or more a guest bus fault in the emulator. A native core
used to reach the device at the offset's low 32 bits, and a core that Python
runs stopped with a host TypeError. The new tests are
`tests/test_bios_worker_signals.py`, `tests/test_mmio_aperture_faults.py`
and the rewritten `tests/test_shared_mmio_devices.py`.

### Stage 3. Full cores, the scheduler and the worker loop (3a)

Builds the topology for one to four full cores, the core states of section
4.2, the C++ scheduler of section 4.3 with the clock of section 4.4, and the
worker limits of section 4.6. `COREID`, `NCORES`, `N-FULL`, `CORE-STATUS`,
`WAKE-CORE` and `IPI-SEND` take their machine behaviour; `IPI-STATUS`,
`IPI-ACK`, `MBOX!` and `MBOX@` join the end of the hosted vocabulary;
`IDLE-UNTIL` wakes on an IPI. `megapad.py --mode simulator` and the
simulator's server take `--cores`, and session status reports each core's
state and semantic steps. The contract's sections 2, 5, 6 and 10 and its
KDOS multicore paragraphs describe the new profile, and the one-core
paragraphs become the one-core case of it.

Tests:

- a new differential file runs the same KDOS source on the emulator and the
  simulator with one, two and four full cores: `CORE-RUN`, `CORE-WAIT`,
  `ALL-CORES-WAIT` and `BARRIER`; a counter that `LOCK` and `UNLOCK`
  protect; `P.RUN-PAR`; run queues served on secondary cores; messages under
  lock 7; `CATCH` and `THROW` inside a worker job; a faulting and an
  aborting worker; a worker's IPI ending core 0's `IDLE-UNTIL`; the `CORES`
  display; the SysInfo words. It compares output text, the buffers each case
  owns and the final worker slots, and leaves per-core steps and cycles
  aside, as the contract's section 10 does;
- each multicore case runs twice and on both executors, and memory, output
  and per-core step counts must agree exactly;
- the one-core profile's units pass. With one awake core the scheduler adds
  no semantic step, which the meters show directly, since each turn's steps
  equal the operations its dispatcher ran; the Desktop journey's step counts
  are compared with today's with `PYTHONHASHSEED` pinned, knowing that code
  placement alone moves them by about 1%.

Built on 2026-11-22. The native `CoreScheduler` and the glue in
`simulator/cores.py` run the full cores in rounds (1c4563f4). Host
evaluation became core 0's outer interpreter, whose words run in core 0's
turns with the turn kept open from word to word (da1ce7a3): until then the
workers took turns only inside a host dispatch, so a line that started a
job and waited for it, as LM1's managed-data harness evaluates them, never
ended. The differential file is `tests/test_multicore_differential.py`
(3f1947e4); its cases found findings 12 to 14. `create_address_space`
replaced the one-core factory in MegaPad's 54 callers (c0b3ab83) and
Akashic's 42 (Akashic 95e081c9), after which the factory went (49d94ecd).
With one core the runtime builds no scheduler, and a KDOS boot and a
2,000-iteration dispatch take the same semantic steps and Timer counts on
d8016c43 and on stage 3, on both executors. Two parts of section 4.3 waited
for stage 4: input entered at any host boundary rather than only between
rounds, and a running worker took a chiplet's doorbell at its next turn
rather than straight after the read that raised it.

### Stage 4. Per-core engines, contention, chiplets, worker services (3a, 3f)

Builds per-core `FPCSR`, tile engine and accumulator, with `TACC-SEND` and
`TACC-RELAY` from any full core into a chiplet's window; caller identity for
CRC, SHA-2, SHA-3 and the Field ALU; contention inside native paths, where
the disk words, the checked SHA-3 service and HMAC end the turn when another
core holds their lock and retry, and the SHA-3 service's lock 8 becomes the
bank's lock 8, as the contract already requires before multicore, after
which the bank's window at `0x600` attaches to the table; and the chiplet
step points of section 4.7. Akashic's `_profile_machine_shape` admits
multicore shapes on the simulator, so the worker profiles run there:
`sandbox-capability-workers-contracts` on four cores, `agent-security` and
`audio-contracts` on two, and `sandbox-job-service-gate`.

Tests: differential cases on two and four cores for `worker-job.f` jobs
through each state, the sandbox job service lending jobs to workers,
`par.f`'s `PAR-MAP`, `PAR-REDUCE` and `PAR-FOR` over full cores, SHA-2, CRC
and tile results computed on workers, and `TACC-SEND` from a worker into the
test chiplet's window; the test chiplet and LM1's module exchanging
doorbells with core 0 and with a worker on two- and four-core simulators,
besides the one-core cases of `tests/test_lm1_chiplet.py`; two cores
writing into lent memory while the chiplet checks each core's order at its
steps; LM1's units.

Built on 2026-11-23:

- The chiplets step at the end of each round and, while every core sleeps,
  until they rest or ring a core, at most 4,096 steps a visit, the
  emulator's bound; a worker takes a chiplet's signal at once when its own
  access steps the chiplet into raising it, through a native set of signal
  takers that the mailbox and the port window share; and input the host
  gives during a round reaches the FIFO at the round's end (d8453449).
  These close the two departures stage 3 left. A first resting visit that
  read the clock after every step raced a clock bound to advance on each
  read past LM1's hold deadlines, so the visit watches the IPI lines.
- Each full core's tile engine, TACC and `FPCSR` are exchanged at each
  turn switch (0254ea99), and each core has its own CRC unit and owner
  record, as the BIOS's topology-sized table has (d88fd2d9). SHA-2 and the
  Field ALU were keyed by core already.
- The disk words poll lock 2 across turns: a call that finds another core
  holding it ends the turn and runs again at the next, up to the BIOS's
  1,048,576 polls (a600657c). The BIOS's checked SHA-3 words take lock 8
  once and report STATE at once while another core holds it, so the
  simulator's service takes the bank's lock 8 the same way rather than
  retrying, and HMAC takes lock 9 through KDOS's `LOCK`, which already ends
  the turn; the bank's window at `0x600` is attached.
- A dispatch that cannot detach to its host, as in a guest `EVALUATE` or
  a boot script, ends core 0's turn in place, and an `IDLE-UNTIL` there
  ends it early (46022dd5). Without this the four-core sandbox profile,
  whose boot script lends every job to the other cores and waits for them,
  never ended.
- `.S` joined the hosted vocabulary, which the sandbox capability
  contracts need, and the image bootstrap boots a script that binds no
  live entry (348a482a). Akashic's driver gives the simulator the
  profile's full cores (d82d6e80), and the four worker profiles pass there
  in about 9 seconds.

Tests: `tests/simulator/test_multicore_chiplets.py`,
`test_multicore_engines.py` and `test_multicore_contention.py`, new cases
in `test_multicore_runtime.py`, Akashic's
`test_profiles_on_simulator_cores.py`, `test_worker_job.py`'s cases on two
and four simulated cores (c9427e47) and `test_par_cores.py` on both
machines (5da2eea3). Two tests remain to build: two cores writing into
lent memory while a chiplet checks each core's order at its steps, which
needs a chiplet that checks order, and LM1's module on four cores, whose
port is ID 4; LM1's managed-data cases on two cores already carry each
core's store log in its order.

### Stage 5. Cooperative tasks on core 0

Under decision 7, builds the BIOS's three background slots on core 0:
`BACKGROUND`, `BACKGROUND2` and `BACKGROUND3` start a slot on stacks of the
BIOS's size, 32 cells each; `PAUSE` turns through slots 1 to 3 from the
foreground; `TASK-YIELD` returns to task 0; `TASK-STOP` ends a slot; and
`TASK-ID` names the running slot. Each slot is another context of core 0,
and slots change only at `PAUSE` and `TASK-YIELD`, so the scheduler's policy
never enters. Tests compare with the emulator on one core: KDOS's per-slot
`CATCH` chains and Akashic's `coroutine.f`.

Built on 2026-11-23 (ee95d589, with Akashic a603041c):

- `simulator/tasks.py` keeps the slots. Each is a context of core 0 whose
  32-cell data and return stacks lie in the 1,536 bytes below the
  dictionary's start, as the BIOS keeps its slot stacks below its own
  dictionary, so the checked span rule refuses a slot's Bank 0 buffers on
  both machines. A slot's run is a dispatch nested in the foreground's
  `PAUSE`. It detaches where the slot yields, once control is back in the
  slot's own frame, and the next `PAUSE` resumes it through the path the
  core scheduler takes for a worker's job. On two cores a worker's job runs
  inside a slot's run through stage 4's inline turns.
- `TASK-ID` and the CRC and SHA-3 owners take the running slot, in Python
  and in the native engine, whose `set_task` and `OP_TASK_ID` answer core
  0's slot there and 0 on the other cores. Without the native identity,
  KDOS's `HANDLER` chose the foreground's `CATCH` chain inside a natively
  run slot, and a `THROW` there unwound into the foreground's return stack.
- Where the machine would crash, the simulator faults: `PAUSE` in a slot,
  `TASK-YIELD` in the foreground, and either word on a worker core take
  the illegal-operation fault. An error or `ABORT` in a slot ends the slot
  and the line that paused, where the BIOS left the slot behind until
  714f4ce0 (finding 15). Inside a slot, as in the other dispatches that
  cannot detach, an `IDL` without a deadline fails, an `IDLE-UNTIL`
  returns early, and a disk word that finds a worker holding lock 2
  reports the timeout at once, where the machine halts the whole core or
  keeps polling.
- A dictionary rollback that would remove a word an active slot runs
  inside, or will start, is refused, as for a worker's suspended job.
- 714f4ce0 brought the BIOS level with the simulator. `PAUSE` and
  `TASK-YIELD` check `COREID` and the running slot and refuse a misplaced
  call by executing 0xFF, one of the ISA's unassigned opcodes, which the
  Python and C++ emulators and the RTL all trap as `IVEC_ILLEGAL_OP`, so
  the refusal takes the BIOS's existing fault path: the `FAULT-XT!`
  callback receives -21, which KDOS throws to the innermost `CATCH`, and
  with none the fault is reported and the line, or a worker's job, ends.
  `forth_quit` ends the slot that was running once the `QUIT-XT!` callback
  has run (finding 15). The ISA then reserved 0xFF as permanently illegal
  (decided 2026-11-23), the assembler writes it as `ILLEGAL`, and the
  report names the refusal, `*** PAUSE OR TASK-YIELD REFUSED`, on both
  machines: `illegal_op_handler` knows `task_word_refused`'s byte by the
  trap frame's PC, and the code stays -21.

Tests: `tests/test_task_slots_differential.py` runs fifteen KDOS programs
on the emulator and on three simulators, on one core and on two: the slot
words, round-robin order, loop parameters and stacks across yields,
stopping and replacing slots, `TASK-ID`, the four per-slot `CATCH` cases of
`tests/test_system.py`, the CRC owner per slot, the span rule, and a
worker's job running while a slot does, in 6 seconds, and an abort in a
slot on fresh machines, whose emulator half passes since 714f4ce0. That
commit adds misuse caught by `CATCH` on core 0 and on a worker, and an
uncaught misuse on core 0 on fresh machines;
`tests/simulator/test_task_slots.py` covers the stacks' place and size,
aborts, the faults, rollback and slots on two cores; and Akashic's
`test_coroutine_on_simulator.py` runs every check of `test_coroutine.py` on
the simulator.

### Stage 6. The RTL microcore (3e)

Section 7.

### Stage 7. Clusters and microcore kernels (3a)

After the kernel-contract round of decision 6, builds clusters of four
microcores
(IDs 4 to 15), `CLUSTER-EN`, the cluster barrier, the 1 KiB scratchpad, the
results of the engines a cluster shares, the cluster privilege registers,
and kernels running on microcore contexts with the contract's checks. Tests
compare the same kernels on the emulator, whose BIOS JIT compiles them, and
the simulator on one to three clusters.

### Stage 8. Parallel rounds (3b)

Two parts, reviewed apart from the stages before them:

- **8a, native turns.** A core's turn runs in the native engine from start
  to end, holding the core's stack pointers and continuations in C++, reaches
  the table's native devices without Python, and stops at the first
  Python-owned operation, its quantum or `IDL`. By
  estimate a turn switch then costs a fraction of a microsecond instead of
  about 20 µs, which also makes smaller quanta affordable.
- **8b, speculation.** With dense guest memory, the round's native turns run
  on 1, 2 or 4 host lanes with the interpreter lock released, under the
  commit rule of section 4.8.

Tests: every multicore case of stages 3 and 4, and generated multicore
programs, run on one, two and four lanes and must equal the sequential
scheduler bit for bit in memory, output, core state and step counts; cases
that force a conflict, a counter under a spinlock and two cores reading and
writing one cell, must take the sequential rerun; the Desktop journey runs
on one and four cores. `tests/test_lockstep_rounds.py` keeps the emulator's
lock-step evidence, and a simulator file beside it carries these lane
cases. Speed results are reported as simulator improvements, apart from
device performance.

### Stage 9. Desk with background work on extra cores

Desk's Forth already holds the background work this stage needs. Every
Desktop autoexec configures the sandbox with `SBOX-JOB-OTHER-CORES`, so on a
four-core machine Desk lends sandbox jobs to cores 1 to 3 through
`worker-job.f`, while core 0 sleeps in `IDLE-UNTIL` until a finished job's
IPI wakes it. The work is around Desk:

- `physical_desktop_acceptance.py` takes `--cores` and passes it to
  `accept`, and `akashic_tui.py`'s `serve` and `accept` give the simulator
  the topology;
- a journey reads each core's semantic steps from session status, to show
  that a sandbox job ran on a worker while core 0 kept serving frames;
- Akashic's `docs/concurrency/worker-job.md` describes the simulator beside
  the emulator.

Work that needs Desk's dispatcher to take jobs from a core other than its
own, such as borrowed tile work over every idle core, and Desk's frame flag
belong to their own parts (`docs/chiplet-interconnect-plan.md` §7).

A precondition comes from stage 5. A task slot's run is nested in the
foreground's `PAUSE`, so inside a slot an `IDL` without a deadline fails, an
`IDLE-UNTIL` returns early, and a disk word that finds a worker holding
lock 2 reports the timeout at once. Desk's background work keeps `IDL` and
disk work out of task slots, or this stage lifts the limit first. No
shipped Akashic module calls the BIOS's task words today: only
`concurrency/coroutine.f` does, which no module requires and no image
profile loads.

Tests: the canonical Desktop journey on one core, as the gate, and on four
cores; the `desktop-sandbox-modules` journey on four cores on the simulator,
with the probe's job shown on a worker, and once on the emulator for
comparison; the worker profiles of stage 4.

Built on 2026-11-23:

- `physical_desktop_acceptance.py` takes `--cores`, which `accept` checks
  against the machine shapes the backend runs, and a passing run's
  `final-guest-diagnostics.json` records each core's state, semantic steps
  and turns from the server's status (Akashic 8a066e66). `serve` and
  `accept` already gave the simulator the profile's full cores (stage 4);
  their help no longer calls `--cores` emulator only. `worker-job.md`
  describes the simulator beside the emulator.
- The physical driver has the sandbox check, `--applet sandbox`: Desk with
  the Agent and Probe on the rich terminal, composed as the emulator's
  `desktop-sandbox-modules` profile composes them (a8f21ede), and its whole
  journey (7832caa6), with nothing added to Desk, Probe or the Agent: the
  Agent's install behind its review, its invoke, its refused wrong input
  and unknown entry, and its long run; Probe refused, asking, allowed in
  Desk's access prompt and served; the inspector withdrawing Probe's grant
  and revoking the module; Desk idle at the end. No product path installs a
  module without the Agent's review, so the journey takes the review. The
  journey reads each core's semantic steps from the server's status: on
  four cores core 1 ran Probe's job and each short invoke in 86,346 steps
  and the long invoke in 3,068,562, while core 0 painted, and the long
  run's work beyond the short run's, 2,982,216 steps, showed on core 1.
  The emulator's journey compares cycle counts on Desk's core as well;
  core 0's semantic steps over a turn grow with what Desk paints, so the
  physical journey records them. It passes on one core and on four in
  about 90 seconds.
- The journey found a repaint missing in Akashic's applet host (7cb83d31):
  a shown applet touched between passes, as when the sandbox's poll in
  Desk's service step completes Probe's request, stayed dirty with no paint
  due, so on four cores Probe's answer never reached the screen. On one
  core, and on the emulator's four, the job ends inside the pass whose
  paint is already due. A shown slot whose instance changed since its last
  paint now marks the shell dirty. The canonical Desktop journey on one
  core, with `PYTHONHASHSEED=0` and `--screen-profile`, ran 2,021,077,685
  guest steps and 81 PRESENTs, against 2,016,509,878 and 81 on the stage 5
  baseline: +0.23%, within code placement's noise.
- The canonical Desktop journey passes on four cores, in 132 seconds, with
  the workers idle: no applet of the canonical Desktop uses the sandbox.
- Desk's background work runs on full cores through `worker-job.f`, never
  in a task slot, so stage 5's precondition holds.

The emulator comparison did not run: the author's standing direction
defers emulator smokes. The stage's closing run and its Desktop journey
are one combined run with the BIOS and KDOS findings' slice.

## 6. Order

Stage 1 comes before stage 3. The mailbox's data slots and lines, the IPIs,
the spinlocks with their owners and each core's wake request are exactly
the state that the attachment interface carries per requester. Built inside
today's hard-coded windows first, each would be rebuilt when 3c replaced
them, at an estimated 4 to 8 agent hours of rework, and parallel rounds would
inherit two MMIO paths to make safe instead of one. Doing 3c first costs its
6 to 12 hours before any core runs.

The full-core stages run in the order 1, 2, 3, 4, 5 and 9, each reported
once it is green; stage 2 could run beside stage 1, since they touch
different files, and stage 3 needs both. Stage 9 needs only stages 1 to 4.
Stage 6 is RTL work and can run at any time; stage 7 waits for it and for
the kernel-contract round. Stage 8 follows stage 4 and is reviewed on its
own.

## 7. The RTL prerequisite for microcores (3e)

The RTL microcore, `rtl/core/mp64_cpu_micro.v`, holds 16 registers
(`reg [63:0] R [0:15]`). Its decoder handles the REX prefix only to skip it,
so by its source an instruction naming R17 reaches R1; no bench tests this
yet. The full core forms 5-bit register numbers from REX's bits (`dst5`,
`src5` and `nib5` in `mp64_cpu.v`). The work widens the microcore's register
file to 32, threads those bits through every register field and the skip
logic, and adds a bench program that runs R16 to R31 on a microcore in
`tb_cpu_micro.v` and through a cluster in `tb_cluster.v`, with the same
program run on the emulator's microcore and the results compared. Estimate:
3 to 6 agent hours; the benches run in minutes.

The stacks reach past the microcores. The BIOS read each core's stack top
from R2, which the emulator's host-side boot set for full cores, while the
RTL clears R2 on every core at reset and the per-core stack tops in
`rtl/pkg/mp64_pkg.vh` feed no logic. A microcore released from reset also
starts at address 0 and takes the BIOS's multicore gate into
`secondary_core_entry`, which loads R16, R17 and R19; on today's RTL
microcore those loads land in R0, R1 and R3, and R3 is its program counter.
Stage 2 made boot derive every full core's stacks from `COREID` and SysInfo
(4ea4bc91), so that the chip, the emulator and the simulator follow one
rule. Microcores get theirs the same way, sized from the spill area their
kernels need, and an entry path of their own in place of the full cores'
worker loop; both come with the kernel contract (decision 6). Until then
boot leaves a microcore's R2 as reset leaves it.

## 8. Work in flight that this plan touches

- **LM1's chiplet module, stage 2.** `tests/test_lm1_chiplet.py`, on main
  since d1f0ea17, drives the one-core profile through
  `create_address_space(..., chiplet_ports=1)` since stage 3's factory
  rename (c0b3ab83), `HostedChipletPorts` and `runtime.tile`. The one-core
  profile keeps port ID 1 and the ports' API. Stage 4, which gives each core
  its own tile engine, keeps core 0's as `runtime.tile` and runs the test's
  cases on two and four cores as well.
- **LM1's chiplet module, stage 3 (managed data).** Its 27 conformance
  tests run through the real code at both ends, in the emulator and the
  simulator (decision B of LM1's `design/chiplet-module.md`). Tests 5, 6,
  14, 15 and 16 need a second MegaPad core. On the simulator they need
  stages 3 and 4. On the emulator LM1 rings a worker core whenever a
  worker's requests use a pair of their own (`docs/managed-data.md` §10
  names the core in `NOTIFY`), and since stage 2 the worker acknowledges
  that doorbell as a wake-up; before it, the worker re-ran its last job at
  each doorbell with two full cores and stayed in its IPI handler with four.
  If LM1's stage 3 lands first, its multi-core cases run on the emulator
  alone until stage 4. Section 4.7 gives the order in which LM1 sees each
  core's store log and window word.
- **Borrowed tile work** (release plan 9f) serves on a worker core that
  sleeps in `IDLE-UNTIL` until LM1 rings it
  (`docs/chiplet-interconnect-plan.md` §7). Stage 2 gave it the BIOS's
  part; on the simulator it needs stage 4's per-core tile engines.
- **Desk** changes only in stage 9, as section 5 describes.
- **The full release run** in progress from the mains: this plan's branches
  merge only after it ends.

## 9. Findings

These came out of reading the sources and, where stated, running the
emulator; findings 12 to 14 came out of stage 3's differential cases, and
16 to 19 out of the slice that fixed findings 8 and 12 to 15. Each needs a
change in the BIOS, KDOS, the release plan, the contract or the tests.

1. **A chiplet's doorbell to a BIOS worker core misfires (run on the
   emulator).** The worker's IPI handler scans pending senders 0 to 3 only
   and treats the first one it finds as a core that sent a job: it copies the
   worker's mailbox data word, which still holds the last job it was sent,
   into the worker slot and acknowledges that sender. The real BIOS booted on
   the emulator with the test chiplet on port 0, and a WAKE-CORE job on core
   1 rang the chiplet, which answers by ringing back. With two full cores the
   port's ID is 2, and the chiplet served 126 commands in 60 batches of
   20,000 steps, because the worker ran its job again at every doorbell. With
   four full cores the port's ID is 4, the handler finds no sender and
   returns with the signal still pending, and the level-triggered IPI line
   takes it straight back in: in all 60 samples the worker's program counter
   stood inside `ipi_handler`, and its pending mask still read `0x10` at the
   end. Signals from microcores, IDs 4 to 15, take the same path. The check
   ran on the emulator built from MegaPad d1f0ea17. Stage 2 corrected the
   handler and turned the check into `tests/test_bios_worker_signals.py`
   (4ea4bc91).
2. **An abort on a worker starts a second interpreter (read from the
   source).**
   `w_abort` sets the return stack to R2 and the data stack to half of R2.
   For a worker R2 is its zone top, so its data stack moves to half of that,
   0x78000 for core 1, in the region core 0's data stack grows into, and
   `forth_quit` then runs the outer interpreter on the worker, where it reads
   the UART that core 0 serves. KDOS's `THROW` without a handler and its
   `?CORE0` guard both end in `ABORT`. Worker faults already recovered to
   the idle loop; stage 2 gave aborts the same ending (4ea4bc91).
3. **On the RTL every core starts with R2 at zero (read from the
   source).** Section 7. Stage 2 has boot derive the full cores' stacks
   itself (4ea4bc91).
4. **3a's microcore half rested on unscheduled work.** Kernel words, with
   their vocabulary, launch, completion and calling convention, and their
   compilation by the BIOS JIT are open questions in
   `docs/microcore-kernels-plan.md` §6, which no part of the release plan
   scheduled, and kernels are all a semantic simulator can run on a
   microcore. Decision 6 adds a design round for the kernel contract to the
   release plan.
5. **Part 3f reads as unbuilt.** The release plan says the simulator marks
   the full-width TACC family unsupported, but it was built on 2026-11-20
   (4b07a41c, 39e8628b) and merged with the 2026-11-21 batch; what remains
   is its per-core form (stage 4).
6. **The contract's target scheduler is a task scheduler.** Section 5 of the
   contract names a deterministic cooperative scheduler over a runnable set
   and a yield sequence, which is the BIOS's background slots on one core.
   Step 3a points to that target for the multicore model, which needs a core
   scheduler as well. The plan builds the core scheduler in stage 3 and the
   cooperative slots in stage 5 (decision 7).
7. **Matching lock-step and the contract's claims.** Section 2 of the
   contract leaves physical multicore races outside the simulator's claims,
   while step 3a asks for results that match the emulator's lock-step.
   Section 2 of this plan gives a reading that satisfies both (decision 1).
8. **Forth sees only IDs 0 to 7 of a core's pending mask.** `IPI-STATUS` reads
   the 8-bit `MBOX_STATUS`, and no BIOS word reads CSR `MBOX`'s 64-bit mask
   or branches on the external flag. With three clusters the ports' IDs
   start at 16, so Forth on any backend learns of a port's doorbell only by
   checking its queues. KDOS's chiplet module (release plan 9a) needs a word
   for the whole mask. Fixed in 7521af6d: the BIOS's `IPI-PENDING
   ( -- mask )` reads CSR `MBOX`, the simulator's reads its native
   mailbox's mask after stepping the chiplets, as a read of `MBOX_STATUS`
   does, and `CHIP-WAIT` polls it. `tests/test_ipi_pending_differential.py`
   puts the test chiplet on a port with ID 16 on both machines, where
   `CHIP-WAIT` used to run into the step limit.
9. **The one-core profile's `NUM_BUS_PORTS` differs.** The simulator
   reports 1 and the emulator 4, full cores plus clusters plus three. Only
   the WOTS deadlines read it, and WOTS is absent from the simulator. Stage
   1's topology reports the emulator's value.
10. **The mailbox exists only with ports.** The simulator answers at `0x500`
    only when it has chiplet ports; the machine always has its mailbox.
    Stage 1 gives every profile one.
11. **The parallel plan's commit rule can be narrower.** Section 4.8 states
    the rule that exactly reproduces the sequential order, with its proof.
12. **KDOS's waits on the other cores never end on one core.**
    `ALL-CORES-WAIT`, `ALL-FULL-WAIT`, `BARRIER` and `SCHED-ALL` count with
    a plain `DO` from 1 to the core count. On one core the limit equals the
    start, and `DO` runs its body until the index wraps, asking `CORE-WAIT`
    about cores the machine does not have; the contract's KDOS paragraphs
    record this from the one-core qualification. `?DO` gives the one-core
    case its natural meaning, nothing to wait for. Stage 3's differential
    cases run these words on two and four cores. The fix belongs to the
    first slice that changes `kdos.f`, together with finding 13 and that
    slice's own work, since a change to `kdos.f` reruns almost every unit,
    and no later than the first stage whose release use runs these words.
    None does today: Akashic's `par.f` calls `BARRIER` only with more than
    one core, where it ends. Fixed in 0cffe2b9: the four words count with
    `?DO`, and every case of `tests/test_multicore_differential.py` runs on
    one core as well.
13. **KDOS's messages share their fields between cores (run on the
    emulator).** `MSG-SEND` stores its type, payload and target in the
    shared variables `MS-T`, `MS-P` and `MS-G` before it takes lock 7, and
    `MSG-RECV` reads a message's fields back from `MR-T`, `MR-S` and `MR-P`
    after it has released the lock; `MSG-BROADCAST` keeps its arguments in
    `MB-T` and `MB-P` with no lock at all. The contract's KDOS paragraphs
    record the race from the source, and the emulator shows its effect:
    three workers that each sent core 0 a message at once on four cores
    delivered three messages carrying core 3's payload, 900 in all where
    600 was sent. The simulator ran each short job within its turn, so its
    sends did not interleave and it delivered 600. Storing after `LOCK` and
    reading before `UNLOCK` closes the race, and per-core variables would
    serve `MSG-BROADCAST`. No Akashic module uses KDOS's message words, and
    stage 3's differential case sends from one worker at a time. The fix
    joins finding 12's in the first slice that changes `kdos.f`, and comes
    no later than the first stage whose release use sends messages from
    several cores at once. Fixed in 0cffe2b9: `MSG-SEND` stores after
    `LOCK`, `MSG-RECV` reads before `UNLOCK`, and `MSG-BROADCAST` keeps its
    arguments on its own stack, since a spinlock's owner reacquires it
    without depth and the first `MSG-SEND`'s `UNLOCK` would release a lock
    held around the loop. The differential file's messages-at-once case has
    three workers send core 0 a message, and then broadcast one, at once on
    four emulator cores, and every payload arrives whole; the old words
    delivered one of the three sends intact and one of the three
    broadcasts. Later in the same slice `MSG-RECV` lost the extra `COREID`
    cell it left under a message, which the KDOS references recorded as a
    stack defect against its stated `( -- type sender payload flag )`, so
    `MSG-DISPATCH` hands a handler exactly `( sender payload type )` and
    `MSG-FLUSH` returns a count.
14. **Three BIOS flag words answer 255 for true (run on the emulator).**
    `MICRO?`, `RESIZED?` and `RESIZE-DENIED?` load their true flag with
    `ldi r, -1`, whose 8-bit immediate the machine zero-extends, so they
    answer 255 where a Forth true flag has every bit set; the simulator's
    words answer -1. `IF` reads both alike, while `INVERT`, `AND` with
    another flag and `TRUE =` do not. Loading -1 with `ldi64`, as `=` does,
    fixes all three. Stage 3's differential case compares `MICRO?`'s truth.
    The fix belongs to the next slice that changes `bios.asm`, stage 7 at
    the latest, so that one BIOS closing run covers it; a BIOS word that
    reads CSR `MBOX`'s whole mask (finding 8) joins that slice if it is
    approved. Fixed in 88b16e84: the sysinfo case prints `MICRO?`'s flags,
    and `tests/test_accept_and_vram_words.py` checks that `RESIZED?` and
    `RESIZE-DENIED?` answer -1.
15. **An abort in a background slot leaves the slot behind (run on the
    emulator).** The BIOS's `forth_quit` resets the return stack and runs
    the `QUIT-XT!` callback, but it leaves `var_task_active` naming the slot
    that was running and the slot's entry in `var_task_pcs` in place. On
    the emulator, a slot that yielded once and then took `ABORT"` left
    `TASK-ID` answering 1 at the prompt and `1 TASK?` answering 1, so KDOS's
    `HANDLER` and the CRC and SHA-3 owner records took the foreground for
    the slot. The next `PAUSE` resumed the slot from the state saved at its
    last yield, and the line that paused reported `? (not found)`, though
    every word in it exists; a slot that aborts before it yields stays
    active the same way, to start again at the next `PAUSE`. The simulator
    ends the slot and the line, and `TASK-ID` answers 0 (the contract's
    section 5). On core 0, `forth_quit` zeroing the running slot's entry and
    then `var_task_active` gives the BIOS the same answer. The fix joins
    finding 14's in the next slice that changes `bios.asm`.
    `tests/test_task_slots_differential.py` holds the case: slot 1 aborts
    after a yield and slot 2 before one, and the foreground then reads
    `TASK-ID`, both slots' state and a `PAUSE`. The simulator's half passes;
    on the emulator `TASK-ID` answers 2, both slots stay active, and the
    `PAUSE` prints six bytes of stale memory, so that half is a strict
    expected failure, which the fixing slice turns into a pass by removing
    the mark. Fixed in 714f4ce0: `forth_quit` clears the running slot's
    entry and then `var_task_active` once the `QUIT-XT!` callback has run,
    so KDOS clears the slot's `CATCH` chain as well as the foreground's.
    The case's emulator half passes without its mark.
16. **Interpreter diagnostics in a slot leave the slot running (run on the
    emulator).** Several diagnostics that a slot reaches through
    `EVALUATE` branch straight to `quit_loop` rather than through
    `forth_quit`: a compile-only word in interpretation state, `;` outside
    a definition, a nested `:`, a quotation mismatch and `IMMEDIATE` after
    `:NONAME`. They skip the `QUIT-XT!` callback, the reset of the return
    stack and, since finding 15's fix lives in `forth_quit`, the end of a
    running slot. A slot that ran `S" DO" EVALUATE` printed `compile-only word`,
    after which `TASK-ID` and `1 TASK?` both answered 1 at the prompt and
    the interpreter ran on the slot's data stack until its overflow check
    reset it; the simulator ends the slot and the line, as for any error
    there. `QUIT` in a slot now ends the slot through `forth_quit` but
    keeps the data stack pointer in the slot's stack, below the
    dictionary, until the same check resets it. Sending these diagnostics
    through `forth_quit`, and giving the foreground its data stack back
    when `QUIT` ends a slot, would close both, in the next slice that
    changes `bios.asm`. Fixed on 2026-11-23 in the same slice: the
    diagnostics and `QUIT` reach `forth_quit` through `quit_keep_stack`,
    which first gives the foreground back the data stack pointer that
    `PAUSE` saved, and `forth_quit` now ends every nested `EVALUATE` with
    the line, so a diagnostic inside one no longer keeps its level and
    return frames. Before, sixteen of them used up `EVALUATE`'s depth for
    the rest of the session, and on a worker the same branch started a
    second outer interpreter, leaving core 0 waiting on the job; the job
    now ends. The simulator gains `QUIT`, which keeps the data stack.
    `tests/test_task_slots_differential.py` compares `S" DO" EVALUATE` and
    `QUIT` in a slot on both machines, and `tests/test_bios_line_endings.py`
    covers the endings on core 0 and on a worker.
17. **`TASK-STOP` and `TASK?` take any slot number (read from the
    source).** Both index `var_task_pcs` with the cell on the stack
    unchecked, so `TASK-STOP` with a number outside 1 to 3 clears another
    BIOS cell, for 5 to 7 a slot's saved data stack pointer and for -1 the
    running-slot mark, and `TASK?` reads one; the simulator refuses such a
    number without consuming it. A bound check that refuses it as the
    simulator does belongs to the slice of finding 16. Fixed on 2026-11-23
    in the same slice: `TASK-STOP` refuses a number outside 1 to 3 and
    `TASK?` one outside 0 to 3 before taking it, printing `no such task
    slot` and ending the line through `quit_keep_stack`, so the number stays
    on the stack as the simulator leaves it; a refusal in a slot ends the
    slot. The differential file refuses 0, 4, 5, 7 and -1 on both machines
    and then resumes a slot that yielded before them.
18. **Three system tests no longer contend for the locks they test (run on
    the emulator).** `TestBIOS::test_disk_checked_lock_contention_times_out_without_issuing`,
    `TestBIOSWotsCheckpoint3::test_guard_contention_returns_state_without_touching_wots`
    and `TestKDOSSHA3Checkpoint2::test_hmac_hkdf_lock9_contention_preserves_status_precedence`
    make a spinlock look held by core 1 through `sys.spinlock.locked[n] =
    True` and `owner[n] = 1`. Since f317327b moved the emulator's spinlocks
    onto the shared native model, those two properties build fresh lists
    from the native bank on each read, so the assignments change nothing,
    the words under test find the lock free, and all three fail on main
    (7e015eaa) as on the slice that fixed findings 8 and 12 to 15; the
    file last passed whole at d8016c43, before f317327b. The tests need to
    take the lock in the native bank as core 1, which the next change to
    `tests/test_system.py` can do. Fixed on 2026-11-23 in the same slice:
    the three tests boot two full cores, since the bank refuses a requester
    that names no core, and core 1 takes the lock through the emulator's
    spinlock window, `read8_for_requester(1, 4n)`, as its own access would;
    no test-only path was needed.
19. **After an `ABORT` or a fault in a slot the machines leave different
    data stacks (run on both).** The BIOS's `ABORT` and `fault_recover`
    reset core 0's data stack pointer, so the foreground's data stack is
    empty afterwards, while the simulator ends only the slot's context and
    the foreground keeps the data stack it had when it paused: a line
    `1 2 ' X BACKGROUND PAUSE` whose slot aborts leaves `DEPTH` 0 on the
    emulator and 2 on the simulator. After `QUIT` and the interpreter's
    diagnostics both machines keep it (finding 16). Emptying the main
    context's data stack when an `ABORT` or a fault leaves a slot would make
    the simulator agree, in its next slice. Fixed on 2026-11-23 in the same
    slice: the simulator's `PAUSE` empties the foreground's data stack when
    an `ABORT`, `ABORT"` or fault ends the slot, and keeps it for `QUIT` and
    the evaluator's errors, whose aborts say so. The same flag lets `QUIT`
    keep the data stack outside slots too, where the abort paths' reset of
    their origin used to empty it. The differential file now compares the
    data stack after each of the four endings in a slot, and after `ABORT`,
    a fault and `QUIT` outside slots, on the emulator and three simulators.

## 10. Decisions

All nine were decided on 2026-11-22, each as this plan proposed.

**1. Matching the emulator's lock-step means the three promises of section
2.** The simulator executes semantic operations whose memory accesses differ
from the BIOS's machine code, so the emulator's interleaving of a racing
program lies beyond it. Determinism, an outcome the architecture allows, and
equal results wherever a program's synchronization fixes them are what the
differential suite can check, and they agree with the contract's existing
claims.

**2. The round rule.** A turn runs for at most 65,536 semantic steps, a
quantum fixed in the machine profile and the same for both executors, and
ends early at `IDL`, at a refused spinlock acquire and at the end of a worker
job. Time follows the emulator's round clock: a round advances it by its
longest turn, and a core's reads see the round's start plus its own steps.
The quantum's measured cost is within noise on today's path, ending a turn
early stops a waiting core from spinning through its quantum, and the round
clock reduces to today's rule with one awake core and lets parallel rounds
read `CYCLES` without reruns. The quantum can shrink after stage 8a without
touching the differential suite, which checks synchronized results only.

**3. Device attachment (3c) comes before the multicore model (3a).** The
mailbox, the IPIs, the spinlocks and the wake lines are then built once, on
the new interface, and stage 8 has one MMIO path to make thread-safe. Stage 1
costs its 6 to 12 agent hours before any second core runs.

**4. One model of each multicore device serves both backends.** The mailbox
and the spinlock bank are written once in `shared/accel`, and stage 2 moves
the emulator's Python `MailboxDevice` and `SpinlockDevice` onto them, so no
second definition of either device can drift from the first and the
emulator gains native mailbox and spinlock access. The move costs 3 to 5
agent hours inside stage 2, whose closing run already covers the emulator's
units for the BIOS change.

**5. A BIOS worker acknowledges signals other than jobs as wake-ups, and an
abort on a worker ends the job as a fault does.** The worker's IPI handler
takes a job only from a full core and acknowledges every other pending
signal, as core 0's sleep already does, which fits the link's rule that a
doorbell means "look at your queues". This corrects findings 1 and 2, on
which borrowed tile work's serving core and managed data's worker cores
depend. It costs 3 to 6 agent hours and a closing run of about 190 minutes,
shared with decision 4's work in stage 2.

**6. A design round for the kernel contract comes before microcores.** The
round settles the kernel vocabulary, launch, completion, calling convention
and spill areas; kernels are then built in the BIOS JIT and in the simulator,
with the cluster devices, as stage 7. The release plan gains the round as a
step. Part 3's done-when needs full cores only. By estimate the design takes
4 to 8 agent hours, kernel compilation in the BIOS JIT 20 to 40 with a BIOS
closing run of about 150 minutes, and stage 7 10 to 18.

**7. The BIOS's cooperative task slots are built in Part 3, as stage 5.**
Step 3a points to the contract's target scheduler, which is the cooperative
one, and stage 3's contexts make the slots cheap to build: 3 to 6 agent
hours and a 70-minute closing run. Nothing in Akashic uses the slots yet,
and their 32-cell stacks suit only small work.

**8. Parallel rounds (3b) serve the simulator first.** The Desktop and its
journeys run on the simulator, Part 3 is about it, and its reference order is
whole turns in core order, where the emulator's interleaves every memory
access and needs finer records and checks. The read and write records live
in `shared/accel`, so the emulator can adopt them later. Stage 8 is estimated
at 22 to 44 agent hours.

**9. Workers are held to KDOS's core-0 contract.** The compiler, the
evaluator, dictionary growth and rollback, and UART input run on core 0
alone; a worker that reaches one faults with -21; and a rollback that would
remove a word a suspended core runs inside is refused. This keeps the
differential suite to programs that follow the contract, which is where
results are defined, and fails closed where the machine would corrupt itself.
It is part of stage 3.
