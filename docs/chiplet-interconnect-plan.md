# Chiplet interconnect

Status: design accepted on 2026-11-11. MegaPad's side of the link is in RTL
and simulated, and [chiplet-link.md](chiplet-link.md) specifies it. LM1's
end, a link endpoint and a boot fetch, is in LM1's RTL (`rtl/link/` in the
lm1 repository), where a bench joins it to MegaPad's RTL. The emulator and
the simulator model MegaPad's side, and a chiplet's model attaches to it
through a C interface. The wiring of LM1's end into its cluster, LM1's own
model and the software are still to come. The second direction of lending in
§3, in which the tile engine stores its accumulator into memory a chiplet
lends, was accepted on 2026-11-19 for `TACC.STORE`. MegaPad's RTL, the
emulator and the simulator build it; LM1's end of it and its software are
not built yet. Borrowed tile work, LM1's third use of the link (§7), in
which LM1 has MegaPad's tile engines do its number work and the results land
in its own memory, was decided on 2026-11-20 (release plan decision 38). The
BIOS words it runs on are built, in the emulator and the simulator alike;
the service, its operation and LM1's side are not. Managed data, LM1's
second use, was decided the same day as [managed-data.md](managed-data.md)
specifies it, recovery after an LM1 failure included. MegaPad's side of it
is built in the chiplet system module, `chiplet.f`, beside the general
words of §9, and LM1's side in the stand-in for LM1's runtime in LM1's
chiplet module; recovery is built as far as its stage 1, in which MegaPad's
never-free engine adopts a sound heap when LM1 fails. This note records how chiplets attach to MegaPad in
Leviathan: the contract every chiplet meets, how the chips share memory, how
they signal and exchange work, and how LM1, the first chiplet, runs sandbox
jobs, managed data and borrowed tile work over the link. It schedules no
work. The release plan's side is decision 37 and Parts 3d, 8 and 9 of
`dev/PLAN.md` in fantasy-engine.

## 1. A chiplet works on memory lent to it

To MegaPad a chiplet is a device that works directly on memory lent to it,
as the disk and network controllers already do: each is a requester on the
bus, and the SoC bounds the disk's transfers with base-and-limit ranges
(`rtl/soc/mp64_soc.v`). The chiplet reads and writes its lent ranges across
the link at MegaPad's own addresses, so both chips work on one copy of the
data, and an address means the same thing on either side.

The data both chips work on is stored beside MegaPad because the two chips
wait differently. A MegaPad core stalls until the memory answers, and since
the main bus serves one transaction at a time (`rtl/bus/mp64_bus.v`), every
other core waits behind it. LM1 switches to another hardware thread while a
read is on its way (`spec/00-overview.md` §4.6 in lm1). Most of that data is
also Desk's own, which Forth reads constantly and LM1 mainly collects. And
since MegaPad's cores cache instructions only and read data straight from
memory, every core reads the current value of memory that a second chip has
written.

## 2. The contract every chiplet meets

MegaPad has identical chiplet ports, and LM1 occupies the first. The number
built is a build parameter, as the number of storage units is, and Leviathan
builds one; a second chiplet then needs a MegaPad die built with two ports,
under the same design.

The interface sets no port limit of its own. Each port takes an ID from the
8-bit set that already numbers MegaPad's cores (`MP64_CORE_ID_BITS` in
`rtl/pkg/mp64_pkg.vh`), so its ceiling is the ID space the chip already
has. The port window reports the number of ports built, and software sizes
its tables from that count. What bounds a real chip is
physical: each port takes die edge and package space, and every chiplet
shares MegaPad's external memory, which points to one to four ports on a
modest chip.

Every chiplet:

- holds an identity record that MegaPad reads over a sideband, a slow channel
  that works before the main link has trained;
- has a reset line, and honours a request to stop and flush before a reset;
- starts by fetching an image that MegaPad names, from a range lent to it
  read-only;
- signals MegaPad, and is signalled, through doorbells (§4);
- takes and returns work through queues in lent memory (§4);
- reaches MegaPad's memory only inside the ranges lent to it (§3).

Once running, a chiplet sends its offers: each function it provides, with a
class and a version, and the queues and memory it asks for. MegaPad grants
or refuses each request, and a chiplet that later needs more asks again.
KDOS registers the functions by class, so software asks for a sandbox engine
and is given whichever chiplet offers one. Power states, a debug stream and
self-test are optional offers. Each chiplet tests its own memories and
logic, while MegaPad starts those tests and collects their results over the
sideband, so MegaPad reports one result for the whole package.

Chiplets connect only to MegaPad. Two of them share data through ranges
of the same lent memory and signal through the mailbox, which leaves
MegaPad's range checks as the one gate between them.

## 3. Sharing memory by lending

One rule governs shared memory in both directions. The chip that owns a
range lends it, the borrower reaches it across the link, and the owner
checks each access against the range and its rights and orders it among its
own. Every range therefore has one owner, which alone orders the accesses
to it.

### MegaPad lends: the direction built first

MegaPad lends ranges of its external memory. A port bridge on MegaPad's die
receives the chiplet's requests, checks each against the lent ranges, and
passes it to the external-memory controller, where the port is one more
client beside the cores and the tile engines, with a weight and a bandwidth
limit of its own. Supervisor code sets the ranges, which exclude Bank 0, the
fast internal banks, video memory, the devices and all MegaPad code. The
controller orders every access to shared memory: a write completes before it
is acknowledged, and a doorbell arrives after the data it announces. Each
chip reads shared data from that memory on every use, so the memory holds
the only copy.

### A chiplet lends: the tile engine's stores

A chiplet may lend ranges of its own memory to MegaPad. Each port has a
window in MegaPad's address space, and the chiplet's address `a` appears at
the window's base plus `a`, so a request across the link carries the
chiplet's own address. The chiplet checks each of MegaPad's accesses
against the ranges it lends and their rights, and orders it among its own;
MegaPad keeps no copy of those ranges. A chiplet grants reads only unless it
offers more, and it lends a range for writing only where it keeps plain
data, never words it reads as references. LM1 lends for writing the bytes
of a pinned bytevector, or a region outside its collected heap, so MegaPad
can store numbers in LM1's memory and can never forge a reference that LM1
would follow. A lent range stays in place until the chiplet withdraws it.

The first use of this direction is the tile engine's `TACC.STORE`, whose
result LM1 may need at once, and LM1 is its first user: when LM1 borrows
the tile engines (§7), the scores it waits on arrive in its own memory this
way, up to 64 of them in one image. A store whose destination lies in a
port's window sends the accumulator's canonical 256-byte image
([tile-engine.md](tile-engine.md)) across the link as one write of 64-byte
units, and MegaPad's external memory never holds it. The chip-wide stage
that holds an image during a store to memory holds this one until the
chiplet answers, so the path needs no buffer of its own. A format that
fills part of the image sends only its active units and has the chiplet
write zeros over the rest: four units for 8- and 16-bit integers, two for
32-bit integers, FP16, BF16 and FP32, and one for FP64. The chiplet answers
once every byte is in its memory, the store retires on that answer, and a
doorbell the core rings afterwards follows the data, as in the first
direction.

Through external memory the same image costs 64 transactions at the
controller: the tile engines' 32 word writes, which go ahead of every core
and chiplet, and the chiplet's 32 word reads. In simulation of today's RTL,
leaving out the link's crossings, with a memory that answers each word in
the cycle after accepting it, the store takes 81 cycles and the chiplet's
reread 162, and the controller is busy for 130 of them; with a memory that
answers after 21 cycles they take 721 and 802 cycles, and the controller is
busy for 1,410. The direct path does not wait on that memory. By estimate
it takes the link's two crossings and the chiplet's own writes, about 50
cycles in all for LM1, which writes its memory 64 bits at a time.

Reads of a chiplet's memory stay unbuilt. A core's read would hold
MegaPad's bus for the round trip, so data on Desk's hot paths stays in
MegaPad's memory, and heavy use would call for an arbiter that lets other
cores proceed during a slow read. A `TACC.LOAD` would hold the image stage
that all seven tile engines share for the same round trip. Until a use
needs more, a `TACC.LOAD`, a tile operand or a core's access that names a
window is a bus fault, and under the placement in §1 LM1 needs none of
them.

Start-up, signalling, jobs and Desk's shared data depend on the first
direction alone, so a failed chiplet can disturb only code that chose to
use memory the chiplet lent, and a store into that memory ends in a fault,
never a hang.

## 4. Signals and work

A doorbell is a mailbox send. Each chiplet port is a mailbox participant
beside the full cores, and a doorbell from a chiplet sets a pending bit that
raises the existing inter-core interrupt. The mailbox's register path,
reached by CSR instructions, bypasses the bus, so a core rings a doorbell
with a CSR write alone (`rtl/periph/mp64_mailbox.v`, `rtl/soc/mp64_soc.v`).

A core's pending mask is indexed by ID and is 64 bits wide through that
path, which covers the 16 cores and 48 ports. The 8-bit `MBOX_STATUS`
register shows IDs 0 to 7. The microcores are participants as the full
cores are: a signal to one sets its bit and raises its IPI line, and a
chiplet's signal raises its flag input too. A microcore reaches the mailbox
through its registers, since the register path by CSR serves the full cores
([microcore-kernels-plan.md](microcore-kernels-plan.md)).

A pending doorbell from a chiplet also drives the core's external-flag
input, so a core waiting for an answer tests it with the external-flag
branch, a test made inside the core, free of bus cycles and interrupt
frames. The branch tests the four flag inputs as one
([isa-reference.md](isa-reference.md)), and the mailbox drives the first, so
the flag has a single meaning: a chiplet has signalled this core. Microcores
keep that branch (`rtl/core/mp64_cpu_micro.v`) and have the flag as well.

Each chip hands work to the other as descriptors in queues in lent memory,
each queue index written by one side only. Each direction has its queues, so
a chiplet asks MegaPad for I/O the way MegaPad hands it a job, and a core
collects completions on its own stacks. Requests across the link take the form the
external-memory port already has: request, accept, response or error, and
cancel with confirmation, each bounded in time (`rtl/mem/mp64_extmem.v`).

The design leaves the 1802-style port instructions unused. `OUT` and `INP`
each make two bus transactions to move one byte (`rtl/core/mp64_cpu.v`), run
only on full cores in supervisor mode, and trap on microcores.

## 5. Lifecycle and protection

MegaPad holds a chiplet in reset, reads its identity over the sideband,
loads the chiplet's runtime image from disk into a range lent read-only, and
releases reset. The bridge refuses a request outside the lent ranges,
records it and reports it. MegaPad cancels a job that misses its deadline
and, when the cancel fails, resets the port; MegaPad's own engine, where one
exists, then takes over the work the chiplet was doing. The sideband also
carries wake, debug access and the self-tests of §2.

Each die makes one check. MegaPad's bridge bounds everything a chiplet can
reach in MegaPad's memory, and a chiplet's end of the link bounds what
MegaPad can reach in the chiplet's (§3). Inside LM1, capability mode bounds
each job to its own data, and only LM1's runtime turns an address received
from MegaPad into a capability, so a reference stored in shared memory
gains authority there and nowhere else.

## 6. The physical link

The contract fixes the transactions and the sideband's duties, and the wires
are chosen later. The link has to be point-to-point with a forwarded clock,
retry on errors, credit flow control, a lane count that scales, and the
sideband. UCIe, the open die-to-die standard, has each of these and can
carry the transactions as its streaming protocol, so the design follows
UCIe's layering. The physical layer and the lane count are chosen once the
release plan's D8 settles where LM1's main memory is placed.

## 7. LM1 on the link

LM1 has three uses for the link. The first two, sandbox jobs and managed
data, are structures in lent memory, built on the contract's hardware
alone. The third, borrowed tile work, adds the direct path of §3 and is
proposed below.

A sandbox job is a queue descriptor pointing at a verified plan and its
data, both created in lendable memory, and LM1 hands the result back the
same way.

Managed data (Part 9d of the release plan) is LM1 objects stored in lent
memory, which Forth reads at the cost of any external-memory read. A handle
names a slot in a table that MegaPad keeps in lent memory, which LM1 treats
as roots and rewrites when an object moves. A store of a reference through
the library writes the value and appends the slot's address to a log in
lent memory, which LM1's collector replays through its own barrier, so the
barrier's rules exist once, on LM1. A core opens a window by storing its
flag and then loading the collector's hold flag, and the collector moves or
frees shared objects only while it holds every core out.
[managed-data.md](managed-data.md) specifies all of this, decided on
2026-11-20, with the words MegaPad code uses and the requests that cross the
queues.

Beyond what a simpler chiplet needs, these uses rely on the strict ordering
of §3, on one set of addresses for both chips, and on a request path quick
enough for a collector.

Where LM1's own memory is placed is the release plan's D8. With memory of
its own, LM1 keeps its private heap there and moves an object once into lent
memory when the object becomes shared. Without it, LM1's whole heap is lent
memory.

### Borrowed tile work

Proposed on 2026-11-20. The BIOS words the service runs on are built:
`TAMAC-SCALAR` and `TAMAC-SCALARS` for `TAMAC`'s broadcast form, and
`TACC-SEND` and `TACC-RELAY`, which store into a window and return a status
for every failure (`docs/BIOS-DICTIONARY.md`, "Borrowed tile work"). The
service, `ASK` and LM1's side are not built. LM1 carries no numeric units
of its own: its spec drops the SIMD sidekick tiles it once described, and
LM1 has MegaPad's seven tile engines do its number work instead. The first
use is score-guided search, in which LM1 walks a tree of options, as a
planner, a puzzle solver or a game player does, and a tile engine rates
each candidate, with a small network, a weighted sum of features or any
other tile kernel. Each score is small, only LM1 wants it, and it decides
LM1's next step, so it goes straight into LM1's memory by the direct path
of §3. Akashic's `docs/search/scored-search.md` designs the search
library, which is written in Leviathan's language, and its words name no
chip.

**Finding the function.** A chiplet asks for a function MegaPad provides
the way it offers one of its own. A proposed operation, `ASK` (number 4 in
[chiplet-link.md](chiplet-link.md) §8 once accepted), goes on the control
queue from the chiplet with the class and version in `ARG0`, the entries it
wants in each work queue in `ARG1` and the bytes of working memory it wants
in `OUT_LEN`, as an `OFFER` carries them. MegaPad answers with the grant
record an `OFFER` gets: a queue pair whose `NOTIFY` names the core that
serves the function, and working memory in MegaPad's external memory, lent
for reading and writing, where the chiplet writes its inputs. In this pair
the chiplet's queue carries the requests and MegaPad's the replies. The
chiplet then lends a range of its own memory for the results with a `LEND`
that names the tile-work class (chiplet-link.md §8), and grows it later by
lending a larger range before withdrawing the old one.

**Kernels.** A kernel is a tile-engine word together with the parameters it
reads, such as a network's weights. The software that loads a kernel on
MegaPad registers it with the service and receives its number, and a
program passes that number to the work it starts on LM1. Parameters
therefore stay in MegaPad's memory, in HBW math RAM where they fit, and
never cross the link. A kernel turns one group of candidates into
one accumulator image, as many candidates to a group as its format has
lanes: 64 for 8-bit inputs, whose scores are 32-bit integers, 32 for FP16
and BF16, and 16 for FP32. It either leaves the group's scores in TACC or
writes them as a 256-byte image in internal memory, which the service then
moves through TACC with `TACC.LOAD`, so every result reaches LM1 by one
`TACC.STORE`.

**A request.** One entry asks for one kernel over one batch:

| Word | Request | Reply |
|------|---------|-------|
| `HEADER` | operation `RUN`, `0x100` | `REPLY`, status |
| `TAG` | chosen by the chiplet | the request's |
| `IN_ADDR`, `IN_LEN` | the candidates, in the grant's working memory | |
| `OUT_ADDR`, `OUT_LEN` | the result area, at the chiplet's own addresses inside a range it lent for the class | bytes written |
| `ARG0` | the kernel's number | images stored |
| `ARG1` | the number of candidates | |

The candidates are either rows of features, one row a candidate in a whole
number of tiles, or 8-byte indices into a table the kernel holds, such as
vectors computed once for every concept of a subject. The kernel states
which form it reads. Image `g` goes to `OUT_ADDR` + 256 × `g`, at the port's
window base plus that address, and the unused lanes of a short final group
hold zero.

**Which cores serve it.** The service runs on a full core other than
Desk's. The chiplet module provides the service's loop as a word, and
whatever owns the machine's full cores runs it on one of them while the
chiplet holds the grant: in a Desk system that is Desk's dispatcher of
worker jobs, which already lends worker cores to the sandbox's jobs.
`NOTIFY` names that core, so the chiplet's signals for tile work wake it
and never Desk's core, and Desk's core pays nothing per batch.
Dispatching each batch from Desk's core as a worker job would cost that
core about 20,000 cycles each time, the figure Akashic's scientific
computing plan measured with the strict-cycle model. The serving core
sleeps in `IDLE-UNTIL` until signalled, takes every entry waiting, runs each
request on its own private tile engine, and sleeps again. The sandbox's
job service keeps the other worker cores; on Leviathan its jobs run on LM1
(Part 9c of the release plan), which leaves those cores mostly idle. One
engine of the seven serves tile work in the first build. Spreading a
request's groups over every idle worker core needs the dispatcher to take
work from a core other than Desk's. The three cluster engines join once
microcore kernels exist ([microcore-kernels-plan.md](microcore-kernels-plan.md)),
because a cluster's engine is reached only from its microcores.

**Sharing with Desk.** The serving core's Forth and its tile engine share
the main bus and external memory with Desk's core. In Akashic's numeric
solver, four cores ran only 1.26 to 1.44 times faster than one because they
waited for the bus. Between groups the service therefore reads Desk's
frame flag and waits while Desk builds a frame, so one group bounds how
long Desk waits for the bus: by estimate about 20,000 cycles for a
two-layer network of 64 inputs and 32 hidden units. The flag is the pause
flag Akashic proposes for background work
(`docs/concurrency/microcore-kernels.md` §4 in akashic), which is not
built. LM1's writes of features pass the port's weight and per-epoch limit
at the external-memory controller (chiplet-link.md §5), which bound what
tile work takes from Desk's memory.

**Batching and cost.** These figures are estimates from the timing in §3
and §8. A request costs the hand-off of §8, about 180 to 250 clocks with
the serving core woken by its interrupt, and about 20 external-memory
transactions to read the entry and write the reply. The scores cost
little: one image carries 64 of them, and the RTL bench that joins LM1's
end to MegaPad's stores an 8-bit image in 39 cycles, with both ends on one
clock and no link crossing. The features cost most. With rows of 64 bytes,
LM1 writes eight words a candidate into external memory and the tile
engine reads them back eight words a tile, about 1,000 transactions at the
controller for 64 candidates, roughly 22,000 controller cycles with the
21-cycle memory of §3, or some 350 cycles a candidate. A request therefore
carries at least one full group, so every image it stores is full. Indexed
candidates cost 8 bytes each instead of a row, and the kernel reads their
vectors from its own table, kept in the 3 MiB of HBW math RAM where it
fits. The hand-off is then a larger share of a request, so such requests
carry several groups.

**Order.** For each group the service stores the image into LM1's memory,
and the store retires only when LM1 answers that every byte is there.
After the last group it writes the reply entry and advances `TAIL`, each
write complete before it is acknowledged, and then rings the port. LM1
therefore finds the scores in its memory before it reads the reply.

**Faults and refusals.** Before it starts, the service checks the request
and refuses it with a status of its class: `0x100` when no kernel has the
number, `0x101` when the candidates lie outside the grant's working memory
or are fewer bytes than the count needs, and `0x102` when the result area
lies outside the ranges the chiplet lent for the class, is misaligned or is
too small. The input check keeps a chiplet from pointing the service at
memory it was never lent and reading it back as scores. A store that
faults, because the chiplet refused the write, its memory failed or it
left the write unanswered for 255 cycles, ends the request with status
`0x103`, and the reply's `ARG0` counts the images stored before it. A
`CANCEL` takes effect between groups and is confirmed by the request's
reply, with status `CANCELLED` and the images stored so far. Every store
into the window faults while the bridge awaits the answer to a cancelled
or timed-out write (chiplet-link.md §4), so a run of faulting stores
leaves the port to MegaPad's existing rule: cancel, then reset.

**One engine.** On a machine without the chiplet, the program's scoring
calls the same kernel with the same parameters on MegaPad, and its images
go to MegaPad memory with a `TACC.STORE` of 6 to 9 cycles; no queue is
involved. Integer kernels give the same scores bit for bit, and float
kernels follow the tile engine's fixed reduction order, so a search that
runs on one engine or on two returns the same result.

**What LM1 lends.** A range of plain data aligned to 64 bytes, outside its
collected heap, in the shared memory of the cluster whose threads read the
scores. It holds as many images as LM1 has requests in flight. LM1 allots
each request its own images and frees them only after reading the reply,
since MegaPad may store into them until it answers. LM1's spec
(`spec/05-memory-gc.md` §11 in lm1) states these rules.

## 8. Estimated cost

Every figure here is an estimate from the timing of today's RTL; nothing is
measured. The inputs are an instruction floor of four clocks, eight to
twelve clocks for an external-memory word before the memory's own latency,
and four to five clocks for one crossing of the link.

| Operation | Estimate |
|-----------|----------|
| A MegaPad read of a shared object | One external-memory read, the same as any other |
| An LM1 access to lent memory | About 20 to 30 clocks, 8 to 10 of them the two crossings |
| A job hand-off and its answer, the core testing its flag | About 100 to 150 clocks added |
| The same, taking the interrupt | About 180 to 250 clocks added |
| Saved per hand-off by a link built for LM1 alone | About 30 to 40 clocks |

Chiplet traffic stays off the main bus. It lengthens a core's wait
whenever that core's external-memory access queues behind a chiplet's, and
the main bus is held for the length of that wait, so the port's weight and
limit at the controller bound what a chiplet takes from Desk.

## 9. What it changes

- **MegaPad RTL:** the external-memory controller (chiplet ports as clients,
  their weights and limits, the address width), the mailbox (ports as
  participants, the microcores beside them, the register path, a pending
  mask indexed by ID), the external-flag wiring, System Info, the new port
  bridge, and the tile engine's route into a chiplet's window (§3).
- **BIOS:** bringing the link up, and the doorbell interrupt. For borrowed
  tile work (§7), four words, which are built: `TAMAC-SCALAR` runs `TAMAC`'s
  broadcast form, `TAMAC-SCALARS` runs it over consecutive tiles,
  `TACC-SEND` stores what TACC holds into a window, and `TACC-RELAY` moves
  a 256-byte image from internal memory into one through TACC. The two
  stores return a status for every failure and never trap.
- **KDOS:** the system module for chiplets, and an allocator for lendable
  memory. For borrowed tile work, the service's loop, its queue handling
  and the registry of kernels. For managed data, the words, records and
  queue handling of [managed-data.md](managed-data.md).
- **The emulator and the simulator:** a model of the ports and an interface
  for a chiplet's model. The simulator, on which the release runs, also has
  the TACC family and its route into a chiplet's window, with the
  emulator's results in every format.

The software has two layers in one BIOS, accepted on 2026-11-11. A
Leviathan BIOS of its own would be a second BIOS to keep in step with the
first.

The general chiplet words are a MegaPad system module in the pattern of
`networking.f`. They serve any chiplet and name no chiplet:

| Word | Stack | Does |
|------|-------|------|
| `CHIP-PORTS` | `( -- n )` | The ports the machine has |
| `CHIP-PRESENT?` | `( port -- flag )` | Whether a chiplet is attached |
| `CHIP-ID@` | `( index port -- x )` | One word of its identity record |
| `CHIP-LEND` | `( base limit rights slot port -- ior )` | Lend a range |
| `CHIP-UNLEND` | `( slot port -- )` | Withdraw it |
| `CHIP-BOOT` | `( addr len port -- ior )` | Name the image, release the chiplet, wait for ready |
| `CHIP-STOP` | `( port -- ior )` | Ask it to stop and wait until it has |
| `CHIP-RESET` | `( port -- )` | Hold it in reset |
| `CHIP-RING` | `( port -- )` | Signal the chiplet |
| `CHIP-WAIT` | `( port -- )` | Wait for its signal with the flag branch, then acknowledge |
| `CHIP-FAULT@` | `( port -- addr info )` | Read and clear the refusal record |

`chiplet.f`, at MegaPad's root, builds these words with these names and
stack orders. Beside them it lends memory to a port from a pool of ranges
(`CHIP-LEND-ALLOT`, `CHIP-LEND-BLOCK`, `CHIP-UNLEND-BLOCK`,
`CHIP-UNLEND-OWNER`), makes queue pairs (`CHIP-PAIR-MAKE`,
`CHIP-PAIR-RECORD!`), sends requests and replies on them (`CHIP-REQUEST`,
`CHIP-REPLY`, `CHIP-PUT`), and serves a port's offers by class
(`CHIP-CLASS!`, `CHIP-SERVE`). Above them a Leviathan vocabulary names what
the machine offers and never which chip does it: finding a function by its
class, putting work on a queue and taking a result, and over those the
sandbox, managed-data and tile-work words. Its word list follows the queue
and offer formats of [chiplet-link.md](chiplet-link.md) §8; the
managed-data words are built.
- **LM1's spec:** external I/O, boot, the memory map, roots, the write
  barrier, safepoints, capability mode as a required part, the memory LM1
  lends MegaPad for writing, and borrowed tile work in place of its sidekick
  tiles.

The external-memory controller needs the most work, since every access to
shared memory passes through it. Today it moves one 64-bit word per
transaction for a core, always serves the tile engines first, and takes a
32-bit address (`rtl/mem/mp64_extmem.v`).

Of the RTL items, the port bridge, the client arbiter with its weights and
limits, the mailbox's ports, its register path, the external-flag wiring,
System Info's field, and the tile engine's route into a chiplet's window
with the bridge's writer are built. The models of the emulator and the
simulator are built too, each with the tile engine's route into a window,
and the queues and offers are specified.
[chiplet-link.md](chiplet-link.md) §10 lists what is left for later.

## 10. Open

- **LM1's memory and size** (the release plan's D8), which set the lane
  count, together with the image traffic of §3, and the load on the
  external-memory controller.
- **Other accesses to memory a chiplet lends.** The cores' loads and
  stores, `TACC.LOAD` and tile operands wait for a use that needs them, and
  a core's read would also need a bus that serves other cores while one
  waits on the link. Borrowed tile work (§7) is the first use that may need
  tile operands read from LM1's memory: its features cross external memory
  twice, written by LM1 and read back by the tile engine, about 1,000
  transactions at the controller for 64 candidates with 64-byte rows. The
  sizing study for D8 counts that traffic against Desk's.
- **Where LM1 receives images.** A pinned bytevector or a region outside
  the collected heap. LM1's spec proposes a region outside the collected
  heap, in the shared memory of the cluster whose threads read the results
  (`spec/05-memory-gc.md` §11 in lm1), and LM1's runtime settles it with
  the rest of what it lends.
- **The tile-work function.** `ASK`, the class's number and its statuses
  enter [chiplet-link.md](chiplet-link.md) §8 once the design of §7 is
  accepted. The first build serves tile work on one worker core's engine.
  Using every idle worker core needs Desk's dispatcher to take work from a
  core other than its own, and the three cluster engines wait for
  microcore kernels. Desk's frame flag, on which the service waits between
  groups, is proposed in Akashic and not built.
- **Lendable memory in Akashic.** Zero-copy depends on the data handed to a
  job being created in lendable memory, and the job service's allocation has
  yet to be read with that in mind. Managed data adds the shared heap's
  blocks, which grow as LM1 asks, and a handle table that grows by segments;
  the allocator keeps them within the port's lent ranges
  ([managed-data.md](managed-data.md) §16).
- **A data cache on a production core.** Reads of lent memory would have to
  go around it.
- **How often each chip touches shared data.** The sizing study for D8
  counts it. A workload in which LM1 walks shared objects far more than
  Forth reads them would reopen where shared data is stored.
- **UCIe's fit** was judged from summaries of the standard; the
  specification itself is still to be read.
