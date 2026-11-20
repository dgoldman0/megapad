# Chiplet link

Status: MegaPad's side is implemented in RTL and simulated, LM1's end
(`rtl/link/` in the lm1 repository) passes a bench joined to it, and the
emulator and the simulator model MegaPad's side (section 9). MegaPad's
writes into memory a chiplet lends (sections 3 and 4), accepted on
2026-11-19 for the tile engine's `TACC.STORE`, are built in MegaPad's RTL
and modelled in the emulator and the simulator; LM1's end and the `LEND`
entries of section 8 are not built yet. This is the specification of the link between
MegaPad and one chiplet: the port's identity, its register window, the
channels that cross the link, the sideband, and the queues in lent memory
through which the two chips exchange work (section 8). The design it
follows, with its reasons, is
[chiplet-interconnect-plan.md](chiplet-interconnect-plan.md). Section 10
lists the parts of the chiplet contract that remain to be specified or
built.

The RTL is `rtl/soc/mp64_chiplet_port.v` (the port bridge),
`rtl/mem/mp64_extmem_arb.v` (the external-memory client arbiter),
`rtl/periph/mp64_mailbox.v` (ports as mailbox participants) and their wiring
in `rtl/soc/mp64_soc.v`.

## 1. Ports and IDs

A MegaPad die is built with `NUM_CHIPLET_PORTS` ports, one by default. Port
`p` has the ID `first + p`, where `first` is the ID that follows the last
core's: 16 on the standard die of 4 full cores and 12 microcores. IDs are the
8-bit values that number the cores, so the mailbox addresses a port the way
it addresses a core. `PORT_COUNT` in the register window reports the number
of ports, the first ID and the number of lent ranges each port holds, and
software sizes its tables from it. System Info repeats the word at `+0x70`
(`CHIPLET_PORTS`), where it reads zero on a die built without ports, so
software asks there before it touches a window that may be absent.

## 2. Register window

The window is at MMIO offset `0xE00` (`0xFFFF_FF00_0000_0E00`) and is 256
bytes long. Every register is 64 bits wide and is reached by an aligned
64-bit access. An access of any other shape goes unanswered, and the bus
ends it as a bus fault.

| Offset | Register | Access | Contents |
|--------|----------|--------|----------|
| `+0x00` | `PORT_COUNT` | R | [7:0] ports built, [15:8] ID of port 0, [23:16] lent ranges per port, [31:24] `W`, each port's window being 2^`W` bytes (section 3) |
| `+0x08` | `PORT_SEL` | R/W | Port whose block appears at `+0x80` |
| `+0x10` | `EXT_CORE_QOS` | R/W | [7:0] the core path's weight at external memory (reset: 1) |
| `+0x80` | port block | | The selected port's registers, below |

`PORT_SEL` is shared by every core, and installing a range takes several
writes, so the software that owns the window serializes its use. A
`PORT_SEL` value beyond the ports built selects no port: its block reads
zero and ignores writes.

### Port block

| Offset | Register | Access | Contents |
|--------|----------|--------|----------|
| `+0x80` | `CTRL` | R/W | [0] `ENABLE`, [1] `RESET_HOLD`, [2] `STOP_REQ`, [3] `DOORBELL_EN`, [4] `ATTN_EN`, [5] `BORROW_EN`, [15:8] `ATTN_CORE` |
| `+0x88` | `STATUS` | R, W1C | [0] `PRESENT`, [1] `STOPPED`, [2] `READY`, [3] `BUSY`, [4] `FAULT`, [5] `ID_DONE`, [6] `BORROW_BUSY`, [23:16] port ID, [39:32] lent ranges |
| `+0x90` | `FAULT_ADDR` | R | Address of the first refused request since `FAULT` was cleared |
| `+0x98` | `FAULT_INFO` | R | [1:0] size, [2] write, [7:4] reason, [15:8] burst length, [31:16] refusals |
| `+0xA0` | `RANGE_SEL` | R/W | Range addressed by the three registers below |
| `+0xA8` | `RANGE_BASE` | R/W | Write: staged base. Read: installed base |
| `+0xB0` | `RANGE_LIMIT` | R/W | Write: staged limit, exclusive. Read: installed limit |
| `+0xB8` | `RANGE_RIGHTS` | R/W | [0] read, [1] write. A write installs the staged base and limit with these rights |
| `+0xC0` | `QOS` | R/W | [7:0] weight (reset: 1), [31:16] accesses per epoch, 0 for no limit |
| `+0xC8` | `ID_INDEX` | R/W | A write starts a sideband read of that identity word |
| `+0xD0` | `ID_DATA` | R | The identity word read |
| `+0xD8` | `BOOT_ADDR` | R/W | Address of the image the chiplet starts from |
| `+0xE0` | `BOOT_LEN` | R/W | Its length in bytes |
| `+0xE8` | `BOOT_ARG` | R/W | The address of the port's control block (section 8) |
| `+0xF0` | `WINDOW` | R | The base of the port's window in MegaPad's address space (section 3) |

`CTRL` resets to `RESET_HOLD` alone: the chiplet is held in reset and the
port admits nothing. `ENABLE` opens the port to memory requests.
`DOORBELL_EN` lets the chiplet's doorbells into the mailbox; while it is
clear the bridge takes them and drops them. `ATTN_EN` makes the first
refusal after each clear of `FAULT` ring `ATTN_CORE` as the port.

Writing 1 to `STATUS` bit 4 clears `FAULT` and the refusal count. The reason
field of `FAULT_INFO` is 1 when the port was closed, 2 for a misaligned
access, 3 for an address outside external memory and 4 when no lent range
grants the access.

## 3. Lent ranges

A port holds `CHIPLET_RANGES` ranges, eight by default. A range is a base,
an exclusive limit and two rights, read and write. The bridge admits a
request when all of these hold:

- `ENABLE` is set;
- the access is naturally aligned for its size;
- the access lies wholly inside external memory, a bound fixed in hardware;
- a range with the right for the access contains every byte of it.

The external-memory bound holds whatever the ranges say, so a range written
over Bank 0, the fast banks, video memory or the device window lends none of
them. A range is installed in one step, when `RANGE_RIGHTS` is written, from
the staged base and limit, so the chiplet sees the old range or the new one
and never a mixture. Writing rights of zero withdraws a range. A request
that the bridge has already admitted completes under the range it was
admitted with; asking the chiplet to stop (section 6) before withdrawing a
range is the orderly path.

### Ranges a chiplet lends

A chiplet may lend ranges of its own memory to MegaPad, which writes into
them with the write of section 4. Port `p` has a window of 2^`W` bytes in
MegaPad's address space at `0x0100_0000_0000_0000 + p × 2^W`, and the
chiplet's address `a` appears at the window's base plus `a`. `W` is a build
parameter, 48 by default, which `PORT_COUNT` reports in bits 31:24, and a
read-only `WINDOW` register at `+0xF0` holds the selected port's base. A
write across the link carries the chiplet's address.

The tile engine's `TACC.STORE` is the one writer: a store whose image lies
wholly inside one port's window goes to that port, and one that runs past
the window's end faults at the first byte beyond it. Any other access that
names a window, a core's load or store, a `TACC.LOAD` or a tile operand,
has no route and is a bus fault, as every access outside MegaPad's memory
is.

The chiplet sets the ranges it lends, keeps them and checks each of
MegaPad's writes against them; MegaPad holds no copy. The chiplet admits a
write when its address is aligned to 64 bytes and a range it lends with the
write right contains every byte the write covers, its zero units included.
It admits or refuses the whole write at its first message, so a refused
write leaves the chiplet's memory untouched. A chiplet lends a range for
writing only where it keeps plain data, never words it reads as
references, and keeps a lent range in place until it withdraws it
(section 8).

On MegaPad's side, privilege and the MPU decide which code may write into a
window, as for any memory. `CTRL` bit 5, `BORROW_EN`, opens the port to
MegaPad's writes and resets clear. While it is clear, or while the chiplet
is absent or held in reset, the bridge refuses each write itself and
nothing crosses the link. `STATUS` bit 6, `BORROW_BUSY`, is set while one
of MegaPad's writes is outstanding, a cancelled one awaiting its answer
included.

## 4. Channels

Each channel is a valid/ready handshake: a message transfers in the cycle in
which both `valid` and `ready` are high, and its payload holds steady while
`valid` waits. Any link layer that delivers each message once and in order
can therefore carry the channels. The bridge ignores every channel while the
chiplet is absent or held in reset.

### Memory request (chiplet to MegaPad)

| Field | Width | Meaning |
|-------|-------|---------|
| `addr` | 64 | MegaPad's address of the access |
| `wdata` | 64 | Write data; a sub-word write puts its data in the low bits |
| `wen` | 1 | 1 for a write |
| `size` | 2 | 0 byte, 1 half-word, 2 word, 3 double word |
| `len` | 8 | Words beyond the first: 0 for one access, up to 255 for a burst |

One request is outstanding at a time: the bridge takes the next one after
the chiplet has taken its last response. `cancel`, a level beside the
channel, abandons the outstanding request.

A burst covers `len + 1` consecutive 64-bit words from `addr`, so it uses
size 3 and an address aligned to 8. A read burst is one request message and
`len + 1` responses. A write burst is `len + 1` request messages, the first
with `addr` and `len` and each with its word in `wdata`, and one response.
The bridge admits or refuses the whole burst at its start, against one lent
range. Each word is then its own access at external memory, so another
client's access can fall between two words of a burst.

### Memory response (MegaPad to chiplet)

| Field | Width | Meaning |
|-------|-------|---------|
| `rdata` | 64 | For a read, the aligned 64-bit word that contains the address |
| `code` | 2 | 0 `OK`, 1 `REFUSED`, 2 `MEM_ERROR`, 3 `CANCELLED` |
| `last` | 1 | Set on the final response of a request |

Every request is answered, and its final response carries `last`. `REFUSED`
means the bridge admitted nothing and memory is untouched. `MEM_ERROR` means
external memory timed out or reported an error. `CANCELLED` answers a
cancelled request; a write it had started may or may not have landed,
because an access that has reached memory runs to its end. A response other
than `OK` ends a burst: the words before it were read or written, and none
after it is.

An access is one transaction at external memory. A sub-word write reads and
rewrites its word inside that one transaction, and the arbiter grants one
transaction at a time, so two chips writing different bytes of one word both
succeed.

### Write to lent memory (MegaPad to chiplet)

| Field | Width | Meaning |
|-------|-------|---------|
| `addr` | 64 | The chiplet's address, aligned to 64 bytes |
| `data` | 512 | One 64-byte unit, its first byte in the low bits |
| `len` | 8 | Units beyond the first: 0 for one unit, up to 255 |
| `zeros` | 8 | Units of zeros the chiplet writes after the data |

A write covers `len + 1 + zeros` consecutive 64-byte units from `addr`. It
is `len + 1` messages: the first carries `addr`, `len`, `zeros` and the
first unit, and each further one carries its unit. One write is outstanding
at a time, and MegaPad sends the next after the chiplet has answered the
last. `cancel`, a level beside the channel, abandons the outstanding write.

The unit is logical. The physical layer carries a unit over however many
lanes the link has, so the traffic the link must carry sets its width, and
the unit does not. The tile engine's `TACC.STORE` is the one writer
([tile-engine.md](tile-engine.md)): it sends an image as one write of the
units its format fills, four, two or one, with `zeros` covering the rest of
the 256 bytes.

### Write answer (chiplet to MegaPad)

| Field | Width | Meaning |
|-------|-------|---------|
| `code` | 2 | 0 `OK`, 1 `REFUSED`, 2 `MEM_ERROR`, 3 `CANCELLED` |

A running chiplet answers every write once. `OK` means every byte the write
covers can be read by the chiplet's own accesses. `REFUSED` means the
chiplet admitted nothing and its memory is untouched; the answer may come
before the last unit, and MegaPad then sends no more of the write.
`MEM_ERROR` means the chiplet's memory failed and `CANCELLED` answers a
cancelled write, and after either some units may have landed. MegaPad
bounds each phase of a write to 255 cycles, as it bounds each word at
external memory. When the chiplet leaves a unit untaken or the answer
missing that long, the store that made the write takes a fault, and the
bridge holds `cancel` and sends nothing more until the answer arrives or
the port is reset. A chiplet returned to reset answers nothing, so the
bridge ends a write then outstanding with a fault for its store.

A write is complete when its answer returns, so a doorbell MegaPad rings
after it follows the data, as in the other direction.

### Doorbells

| Channel | Field | Meaning |
|---------|-------|---------|
| To the chiplet | `src` (8) | ID of the participant that signalled the port |
| From the chiplet | `dst` (8) | ID of the participant the port signals |

A doorbell is a mailbox signal and carries no data. A core rings a port by
writing the port's ID to CSR `MBOX` (0x22), which takes no bus transaction,
or to the `MBOX_SEND` register. The port's pending bit stays set until the
chiplet takes the doorbell, lowest source first. A chiplet's doorbell sets
its bit in the target's pending mask, which raises the target core's IPI
line and its external-flag input 0, so the core can test for it with
`BR.EF`. The core clears it by writing the port's ID to CSR `IPIACK` (0x23)
or to `MBOX_ACK`. A port can signal another port the same way. Signals from
one source to one target coalesce, so a doorbell means "look at your queues"
and never counts events. A destination that names no participant, or names
the sender itself, is ignored.

A core's pending mask is indexed by ID: bit n is set while the participant
with ID n has signalled the core. Reading CSR `MBOX` returns all 64 bits,
with a port's bit at its ID. The 8-bit `MBOX_STATUS` register shows IDs 0
to 7, which on the standard die are full cores and microcores.

A sender writes its data and then rings, and a write is complete when its
response returns, so a doorbell follows the data it announces.

## 5. External memory

Each port is a client of the external-memory controller's 64-bit port,
beside the core path, through `mp64_extmem_arb`. The arbiter grants one
access at a time by weighted round-robin and counts completed accesses
against each port's limit per epoch of 65,536 clock cycles. A port keeps the
grant for `weight` consecutive accesses while it keeps requesting. A port
that reaches its limit waits for the next epoch. The core path carries the
weight in `EXT_CORE_QOS` and has no limit. The tile engines keep their own
port on the controller, ahead of the 64-bit port.

A lone requester passes through the arbiter with the latency it has without
it. When a core's access and a chiplet's arrive together, the later one
waits for the earlier to finish, and the main bus is held for that wait, as
it is for any external-memory access.

## 6. Sideband

The sideband works while the chiplet is held in reset.

| Signal | Direction | Meaning |
|--------|-----------|---------|
| `present` | to MegaPad | A chiplet is attached |
| `rst_n` | to chiplet | Low holds the chiplet in reset |
| `stop_req` | to chiplet | Stop: finish or abandon outstanding work and issue no new request |
| `stopped` | to MegaPad | The chiplet has stopped and holds nothing in flight |
| `ready` | to MegaPad | The chiplet's runtime is up |
| `id_req`, `id_index`, `id_ack`, `id_data` | both | Read of one identity word |
| `boot_addr`, `boot_len` | to chiplet | The image MegaPad names |
| `boot_arg` | to chiplet | The address of the control block MegaPad lends |

MegaPad starts a chiplet in this order: read the identity record, load the
image into a range lent read-only, write the control block of section 8,
write `BOOT_ADDR`, `BOOT_LEN` and `BOOT_ARG`, install the ranges, and clear
`RESET_HOLD` with `ENABLE` set. The chiplet then reads its image through the
memory channel, raises `ready`, and finds its queues through the control
block.

Identity word 0 holds the contract version in its low 32 bits, 1 for this
document, above the ASCII tag `CHIP` in its high 32 bits. Word 1 names the
device in up to eight ASCII characters, first character in the low byte.
Further words belong to the device.

A chiplet returned to reset loses any request in flight: the access runs to
its end at memory and its response is dropped. Pending doorbells for the
port stay pending and reach the chiplet after its release.

## 7. Signals at the die boundary

`mp64_soc` carries the link of every port as flat vectors named `cl_*`:
`cl_rq_*` the memory request, `cl_rs_*` its response, `cl_dbt_*` the doorbell
to the chiplet, `cl_dbr_*` the doorbell from it, and `cl_sb_*` the sideband.
A top that attaches no chiplet ties `cl_sb_present` low and the other inputs
to zero.

## 8. Queues and offers

Once a chiplet runs, the two chips exchange work as entries in queues, in
memory that MegaPad lends. These are formats in memory: of the hardware they
use the lent ranges, the doorbells and `BOOT_ARG`. Every field is a 64-bit
little-endian word unless a bit range is given, and every structure is 64
bytes long, one tile, or a multiple of that.

### Who writes what

MegaPad lends a chiplet two kinds of memory for its queues. What MegaPad
writes, it lends for reading. What the chiplet writes, it lends for reading
and writing. The bridge then holds each chip to its own words: a chiplet
cannot alter an entry MegaPad queued, a count MegaPad keeps or a record
MegaPad wrote. MegaPad checks whatever it reads from the chiplet's words.

### A queue

A queue carries entries one way, from its writer to its reader. It has two
parts, so that each lies in memory that its owner alone writes:

- the ring block, which the writer owns: `TAIL` in its first word, the rest
  of its first 64 bytes reserved, and then the ring of `CAPACITY` entries of
  64 bytes each;
- the head word, which the reader owns: `HEAD`.

`CAPACITY` is a power of two. It is asked for and granted when the queue is
made (below) and is stated in the record that names the queue. `TAIL` counts
the entries the writer has put and `HEAD` the entries the reader has taken,
both from zero and both without wrapping, and entry number n lies in slot
n mod `CAPACITY`. The queue is empty when the counts are equal and full when
they differ by `CAPACITY`.

The writer fills a slot, then advances `TAIL`, then rings the reader. The
reader takes entries until `HEAD` equals `TAIL`. A doorbell means "look at
your queues", so one ring can stand for several entries, and a reader that
finds nothing new does nothing.

A reader checks the count it reads. A `TAIL` that has moved backwards, or
that runs more than `CAPACITY` ahead of `HEAD`, marks the queue broken, and
MegaPad answers a broken queue by resetting the port.

### A queue pair

Queues come in pairs, one in each direction. MegaPad names a pair in a
record of six words:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `TO_RING` | Address of the ring block of the queue MegaPad writes |
| `+0x08` | `TO_HEAD` | Address of that queue's head word, which the chiplet writes |
| `+0x10` | `FROM_RING` | Address of the ring block of the queue the chiplet writes |
| `+0x18` | `FROM_HEAD` | Address of that queue's head word, which MegaPad writes |
| `+0x20` | `CAPACITIES` | [31:0] entries in the queue to the chiplet, [63:32] entries in the queue from it |
| `+0x28` | `NOTIFY` | [7:0] ID of the core the chiplet rings for this pair |

### The control block

Before it releases a chiplet, MegaPad writes the port's control block, lends
it for reading, and puts its address in `BOOT_ARG`:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `MAGIC` | ASCII `CHIPCTRL`, first character in the low byte |
| `+0x08` | `VERSION` | [31:0] version of these formats, 1 for this document |
| `+0x10` | queue pair | The record of the port's control queues |

### An entry

| Word | Field | Contents |
|------|-------|----------|
| 0 | `HEADER` | [15:0] operation, [16] `REPLY`, [63:32] status |
| 1 | `TAG` | Chosen by the sender of a request; a reply carries its request's tag |
| 2 | `IN_ADDR` | Address of the data the receiver reads |
| 3 | `IN_LEN` | Its length in bytes |
| 4 | `OUT_ADDR` | Address of the area the receiver may write |
| 5 | `OUT_LEN` | Its length in bytes; in a reply, the bytes written |
| 6 | `ARG0` | A word for the operation |
| 7 | `ARG1` | A word for the operation |

An entry is a request, or with `REPLY` set the answer to the request that
has its tag. A queue carries both: on the queue it writes, a chip puts its
own requests and its replies to the other chip's requests. A chiplet
therefore asks MegaPad for a service the way MegaPad hands it a job.

The sender of a request owns the memory the request names. The receiver
reads the input area and writes inside the output area, and both return to
the sender with the reply. Each area lies in memory lent with the right its
use needs.

A reply's status is 0 for `OK`, 1 for `UNSUPPORTED` when the receiver knows
no such operation, 2 for `CANCELLED`, 3 for `FAILED` and 4 for `REFUSED`.
Statuses from `0x100` and operations from `0x100` belong to the class of the
function that the pair serves. The operations below belong to every pair or
to the control queues.

| Operation | Number | Meaning |
|-----------|--------|---------|
| `CANCEL` | 1 | Abandon the request that has this entry's tag. It has no reply of its own: the request's one reply confirms it, with `CANCELLED`, or with the ordinary result when the work had finished |
| `OFFER` | 2 | On the control queue from the chiplet: a function the chiplet provides, and what it asks for |
| `LEND` | 3 | On the control queue from the chiplet: a range of its own memory it lends to MegaPad, or withdraws. Proposed, below |

### Offers and grants

A running chiplet states each function it provides in an `OFFER`. `ARG0`
holds the function's class in bits 31:0 and its version in bits 63:32,
`ARG1` holds in bits 31:0 the entries it asks for in each work queue, and
`OUT_LEN` holds the bytes of working memory it asks for.

MegaPad replies on the control queue to the chiplet. A reply with `REFUSED`
grants nothing. A reply with `OK` names in `IN_ADDR` a grant record, which
MegaPad has written and lent:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | queue pair | The record of the function's work queues |
| `+0x30` | `MEM_ADDR` | Address of the working memory, lent for reading and writing |
| `+0x38` | `MEM_LEN` | Its length in bytes |

MegaPad may grant fewer entries or bytes than were asked, and the record
states what it granted. A chiplet that later needs more sends another
`OFFER` for the class, and each grant stands beside the earlier ones. KDOS
registers a granted function under its class, and software asks for a class
and is given whichever chiplet provides it. Class 0 is reserved, and class
numbers are assigned in this section as functions are specified.

### Ranges a chiplet lends

Proposed and not built, with the ranges of section 3. A chiplet names each
range it lends to MegaPad in a `LEND`: `IN_ADDR` holds the range's base in
the chiplet's addresses, `IN_LEN` its length, and `ARG0` its rights in bits
1:0, [0] read and [1] write, and in bits 63:32 the class of the function
the range serves. MegaPad replies `OK` once it has recorded the range, and
KDOS gives the range to software of that class at its address in the
port's window. A `LEND` with rights of zero for a recorded base withdraws
the range: MegaPad stops giving it out, replies once its own writes into
the range have ended, and the chiplet takes the range out of its check
after that reply.

### Bounded waiting

- A chip keeps at most as many of its requests unanswered on a pair as the
  queue that brings their replies has entries, so the other chip never holds
  more replies than that.
- A chip takes entries from the queue it reads whenever it is signalled,
  whether or not the queue it writes has room. A reply it cannot place yet
  it keeps, and places when room appears.
- MegaPad gives each of its requests a deadline. It cancels a request that
  misses its deadline, and resets the port when the cancel goes unanswered
  too.

## 9. The emulator and the simulator

Both model MegaPad's side of the link with one C++ implementation,
`shared/accel/chiplet_port.{h,cpp}`, built into both native extensions. The
registers of section 2, the admission check of section 3 with its refusal
record, bursts, doorbells and the sideband behave as specified here. The
differences follow from two properties of the models: they have no
cycle-level arbitration at external memory, and they give a chiplet no
clock.

- A request completes inside the call that makes it. `BUSY` reads 0, `QOS`
  and `EXT_CORE_QOS` hold what was written and shape nothing, and
  `CANCELLED` answers only a request made while the chiplet is held in
  reset.
- MegaPad's write into memory a chiplet lends also completes inside one
  call, to the chiplet's model, so `BORROW_BUSY` reads 0 and the 255-cycle
  bounds never expire. A `TACC.STORE` that a chiplet takes costs its two
  base cycles, one cycle per unit sent and one for the answer, in the
  emulator's batched and strict-cycle schedulers alike. In the strict one
  the store holds the image stage from its grant until the answer, and the
  chiplet takes the write in the answer's cycle. The Python oracle routes a
  window store through the same C++ port model, and so does the hosted
  simulator's `TACC-STORE`, which completes inside its word.
- In the emulator a chiplet steps between scheduler rounds, and at each
  scheduling frontier of the cycle-exact batch. A step does a fixed budget
  of the chiplet's own units, set per port. While every core sleeps or is
  halted, the chiplets step until they rest or one has rung a core, and the
  cores are then woken by the usual rule. A chiplet reads and writes memory
  and rings cores only from its step, when no core runs.
- In the hosted simulator a chiplet steps before each read of a port
  register or of `MBOX_STATUS`, and when a dispatch returns to its host. The
  simulator's one core reaches the mailbox by MMIO alone and polls
  `MBOX_STATUS` for a port's signal. Its ports are absent unless the address
  space is created with `chiplet_ports`, and its first port has the ID 1.
- A small core in the emulator reaches the mailbox by CSR as a full core
  does. In the RTL it uses the mailbox's registers (section 10).

A chiplet's model implements `shared/accel/chiplet_module.h`, a C interface
that carries what the link carries: one memory request with the link's size
and burst length, MegaPad's write into memory the chiplet lends, doorbells
in both directions, reset, release, stop, ready and stopped, the identity
record, and save and restore of the model's state. Its version is 2, the
version that added the lent write. A model is built into an extension or
loaded at run time from a module library, by `MegapadSystem.attach_chiplet`
in the emulator and `HostedChipletPorts.attach_module` in the simulator.
`shared/accel/chiplet_test_module.cpp` is a small chiplet that the tests use
and a worked example of the interface. It lends MegaPad 4 KiB of its own
memory, from its address `0x1000`, while it runs, and copies the start of
that memory into its reply when a command asks it to.

## 10. Still to specify or build

- **The software of section 8.** Its two ends are KDOS's system module for
  chiplets, whose general words are listed in
  [chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §9, and a
  chiplet's runtime. Neither exists yet. A test chiplet that speaks the
  queues comes with the module; the present one takes its addresses from
  its boot image.
- **Classes.** Each function's class number, operations and statuses are
  assigned with the function, LM1's with LM1's layer.
- **Taking a grant back.** How MegaPad withdraws a function's queues and
  memory from a running chiplet.
- **A CSR path for small cores.** A port's signal reaches a small core as
  it reaches a full core: the mailbox raises that core's IPI line and its
  flag input. A small core rings, reads `MBOX_STATUS` and acknowledges
  through the mailbox's registers, each a bus access through its cluster's
  port, where a full core uses CSRs that take none. Its 8-bit status holds
  IDs 0 to 7, so on the standard die it learns of a port's signal from the
  flag and acknowledges the port it rang. A CSR path in the cluster would
  remove the bus accesses and show it the whole mask.
- **Reads at the edge of a range.** A read returns the aligned 64-bit word,
  so a byte read at the edge of a range whose bounds are unaligned also
  returns up to seven bytes that lie beyond the range. Software lends
  word-aligned ranges. A bridge that clears the bytes outside the range
  would remove the condition.
- **IDs above 63.** The 64-bit pending mask covers IDs 0 to 63: the 16
  cores and 48 ports. A die with more ports needs a wider mask.
- **The rest of MegaPad's writes into a chiplet.** Sections 3 and 4
  specify them for the tile engine's `TACC.STORE`, and MegaPad's RTL, the
  emulator and the simulator build them. LM1's end, whose cluster has no
  write path from outside yet, and the `LEND` entries of section 8 are
  still to build.
  Reads of memory a chiplet lends, by the cores or the tile engine, are
  still to specify.
- **The physical layer.** The channels are logical; framing them onto UCIe's
  layers waits for the choice of where LM1's memory is placed.
