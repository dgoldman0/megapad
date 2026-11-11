# Chiplet interconnect

Status: design accepted on 2026-11-11. MegaPad's side of the link is in RTL
and simulated, and [chiplet-link.md](chiplet-link.md) specifies it. The
chiplet's side, the emulator's and simulator's models and the software are
still to come. This note records how chiplets attach to MegaPad in
Leviathan: the contract every chiplet meets, how the chips share memory, how
they signal and exchange work, and how LM1, the first chiplet, runs sandbox
jobs and managed data over the link. It schedules no work. The release
plan's side is decision 37 and Parts 3d, 8 and 9 of `dev/PLAN.md` in
fantasy-engine.

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
since MegaPad's cores read memory uncached, a second chip writing the same
memory leaves every core reading the current value.

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

### A chiplet lends: in the contract, unbuilt

A chiplet may offer ranges of its own memory to MegaPad. Such a range
appears in MegaPad's address space as a region that behaves as external
memory does today: the bus waits for the answer under the same timeout, and
a failed read is an ordinary bus fault for the core that made it. The
chiplet checks MegaPad's accesses and grants reads only, unless it offers
more, which keeps MegaPad from forging LM1 references. Each read of such a
region holds MegaPad's bus for the round trip, so data on Desk's hot paths
stays in MegaPad's memory, and heavy use would call for an arbiter that lets
other cores proceed during a slow read. Under the placement in §1 LM1 works
without this direction, and the first build leaves it out.

Start-up, signalling, jobs and Desk's shared data depend on the first
direction alone, so a failed chiplet can disturb only code that chose to
read memory the chiplet lent.

## 4. Signals and work

A doorbell is a mailbox send. Each chiplet port is a mailbox participant
beside the full cores, and a doorbell from a chiplet sets a pending bit that
raises the existing inter-core interrupt. The mailbox already has a register
path, reached by CSR instructions, that bypasses the bus, and the SoC ties
it off today (`rtl/periph/mp64_mailbox.v`, `rtl/soc/mp64_soc.v`). Connected,
it lets a core ring a doorbell with a CSR write alone.

The mailbox's pending status is eight bits wide today, enough for the four
full cores and four ports. It widens to cover every participant before a
fifth port is built, and that one widening also serves the microcores if
their launch comes to use the mailbox
([microcore-kernels-plan.md](microcore-kernels-plan.md)).

A pending doorbell from a chiplet also drives the core's external-flag
input, so a core waiting for an answer tests it with the external-flag
branch, a test made inside the core, free of bus cycles and interrupt
frames. The four flag inputs are tied to zero today and the branch tests
them as one ([isa-reference.md](isa-reference.md)), so they take a single
meaning: a chiplet has signalled this core. Microcores keep that branch
(`rtl/core/mp64_cpu_micro.v`).

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
reach in MegaPad's memory. Inside LM1, capability mode bounds each job to
its own data, and only LM1's runtime turns an address received from MegaPad
into a capability, so a reference stored in shared memory gains authority
there and nowhere else.

## 6. The physical link

The contract fixes the transactions and the sideband's duties, and the wires
are chosen later. The link has to be point-to-point with a forwarded clock,
retry on errors, credit flow control, a lane count that scales, and the
sideband. UCIe, the open die-to-die standard, has each of these and can
carry the transactions as its streaming protocol, so the design follows
UCIe's layering. The physical layer and the lane count are chosen once the
release plan's D8 settles where LM1's main memory is placed.

## 7. LM1 on the link

LM1's two uses are structures in lent memory, built on the contract's
hardware alone.

A sandbox job is a queue descriptor pointing at a verified plan and its
data, both created in lendable memory, and LM1 hands the result back the
same way.

Managed data (Part 9d of the release plan) is LM1 objects stored in lent
memory, which Forth reads at the cost of any external-memory read. A handle
is a slot in a table that LM1 registers as a root region and rewrites when
an object moves. A write through the library stores the value and appends
the slot's address to a log in lent memory, which LM1's collector replays
through its own barrier, so the barrier's rules exist once, on LM1. A core
opens a window by storing its flag and then loading the collector's hold
flag, and the collector moves shared objects only while it holds every core
out.

Beyond what a simpler chiplet needs, these uses rely on the strict ordering
of §3, on one set of addresses for both chips, and on a request path quick
enough for a collector.

Where LM1's own memory is placed is the release plan's D8. With memory of
its own, LM1 keeps its private heap there and moves an object once into lent
memory when the object becomes shared. Without it, LM1's whole heap is lent
memory.

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
  participants, the register path, the width of its pending status), the
  external-flag wiring, System Info, and the new port bridge.
- **BIOS:** bringing the link up, and the doorbell interrupt.
- **KDOS:** the system module for chiplets, and an allocator for lendable
  memory.
- **LM1's spec:** external I/O, boot, the memory map, roots, the write
  barrier, safepoints, and capability mode as a required part.

The external-memory controller needs the most work, since every access to
shared memory passes through it. Today it moves one 64-bit word per
transaction for a core, always serves the tile engines first, and takes a
32-bit address (`rtl/mem/mp64_extmem.v`).

Of the RTL items, the port bridge, the client arbiter with its weights and
limits, the mailbox's ports and the external-flag wiring are built.
[chiplet-link.md](chiplet-link.md) §8 lists what the RTL leaves for later.

## 10. Open

- **LM1's memory and size** (the release plan's D8), which set the lane
  count and the load on the external-memory controller.
- **Lendable memory in Akashic.** Zero-copy depends on the data handed to a
  job being created in lendable memory, and the job service's allocation has
  yet to be read with that in mind.
- **A data cache on a production core.** Reads of lent memory would have to
  go around it.
- **How often each chip touches shared data.** The sizing study for D8
  counts it. A workload in which LM1 walks shared objects far more than
  Forth reads them would reopen where shared data is stored.
- **UCIe's fit** was judged from summaries of the standard; the
  specification itself is still to be read.
