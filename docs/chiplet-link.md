# Chiplet link

Status: MegaPad's side is implemented in RTL and simulated; the emulator and
the simulator have no model of it yet. This is the specification of the link
between MegaPad and one chiplet, as far as hardware defines it: the port's
identity, its register window, the channels that cross the link, and the
sideband. The design it follows, with its reasons, is
[chiplet-interconnect-plan.md](chiplet-interconnect-plan.md). Section 8
lists the parts of the chiplet contract that remain to be specified.

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
software sizes its tables from it.

## 2. Register window

The window is at MMIO offset `0xE00` (`0xFFFF_FF00_0000_0E00`) and is 256
bytes long. Every register is 64 bits wide and is reached by an aligned
64-bit access. An access of any other shape goes unanswered, and the bus
ends it as a bus fault.

| Offset | Register | Access | Contents |
|--------|----------|--------|----------|
| `+0x00` | `PORT_COUNT` | R | [7:0] ports built, [15:8] ID of port 0, [23:16] lent ranges per port |
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
| `+0x80` | `CTRL` | R/W | [0] `ENABLE`, [1] `RESET_HOLD`, [2] `STOP_REQ`, [3] `DOORBELL_EN`, [4] `ATTN_EN`, [15:8] `ATTN_CORE` |
| `+0x88` | `STATUS` | R, W1C | [0] `PRESENT`, [1] `STOPPED`, [2] `READY`, [3] `BUSY`, [4] `FAULT`, [5] `ID_DONE`, [23:16] port ID, [39:32] lent ranges |
| `+0x90` | `FAULT_ADDR` | R | Address of the first refused request since `FAULT` was cleared |
| `+0x98` | `FAULT_INFO` | R | [1:0] size, [2] write, [7:4] reason, [31:16] refusals |
| `+0xA0` | `RANGE_SEL` | R/W | Range addressed by the three registers below |
| `+0xA8` | `RANGE_BASE` | R/W | Write: staged base. Read: installed base |
| `+0xB0` | `RANGE_LIMIT` | R/W | Write: staged limit, exclusive. Read: installed limit |
| `+0xB8` | `RANGE_RIGHTS` | R/W | [0] read, [1] write. A write installs the staged base and limit with these rights |
| `+0xC0` | `QOS` | R/W | [7:0] weight (reset: 1), [31:16] accesses per epoch, 0 for no limit |
| `+0xC8` | `ID_INDEX` | R/W | A write starts a sideband read of that identity word |
| `+0xD0` | `ID_DATA` | R | The identity word read |
| `+0xD8` | `BOOT_ADDR` | R/W | Address of the image the chiplet starts from |
| `+0xE0` | `BOOT_LEN` | R/W | Its length in bytes |

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

One request is outstanding at a time: the bridge takes the next one after
the chiplet has taken the response. `cancel`, a level beside the channel,
abandons the outstanding request.

### Memory response (MegaPad to chiplet)

| Field | Width | Meaning |
|-------|-------|---------|
| `rdata` | 64 | For a read, the aligned 64-bit word that contains the address |
| `code` | 2 | 0 `OK`, 1 `REFUSED`, 2 `MEM_ERROR`, 3 `CANCELLED` |

Every request is answered. `REFUSED` means the bridge admitted nothing and
memory is untouched. `MEM_ERROR` means external memory timed out or reported
an error. `CANCELLED` answers a cancelled request; a write it had started
may or may not have landed, because an access that has reached memory runs
to its end.

An access is one transaction at external memory. A sub-word write reads and
rewrites its word inside that one transaction, and the arbiter grants one
transaction at a time, so two chips writing different bytes of one word both
succeed.

### Doorbells

| Channel | Field | Meaning |
|---------|-------|---------|
| To the chiplet | `src` (8) | ID of the participant that signalled the port |
| From the chiplet | `dst` (8) | ID of the participant the port signals |

A doorbell is a mailbox signal and carries no data. A core rings a port by
writing the port's ID to `MBOX_SEND`, and the port's pending bit stays set
until the chiplet takes the doorbell, lowest source first. A chiplet's
doorbell sets its bit in the target's pending mask, which raises the
target core's IPI line and its external-flag input 0; the core clears it by
writing the port's ID to `MBOX_ACK`. A port can signal another port the same
way. Signals from one source to one target coalesce, so a doorbell means
"look at your queues" and never counts events. A destination that names no
participant is ignored. `MBOX_STATUS` shows the first eight participants,
the full cores and then the ports.

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

MegaPad starts a chiplet in this order: read the identity record, load the
image into a range lent read-only, write `BOOT_ADDR` and `BOOT_LEN`, install
the ranges, and clear `RESET_HOLD` with `ENABLE` set. The chiplet then reads
its image through the memory channel and raises `ready`.

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

## 8. Still to specify

- **The offer list and the work queues.** A running chiplet states its
  functions and the queues and memory it asks for, and work crosses as
  descriptors in queues in lent memory. Both are formats in memory, defined
  with KDOS's system module for chiplets.
- **Bursts.** A request moves one word. Several words per request would cut
  the round trips of a queue descriptor.
- **Doorbells by CSR.** The mailbox's CSR path reaches ports, and the SoC
  leaves that path unconnected, so a core rings a port through `MBOX_SEND`.
- **The mailbox status width.** `MBOX_STATUS` holds eight participants. It
  widens before a fifth port, or before microcores join the mailbox.
- **System Info.** The port count is read from `PORT_COUNT`. System Info
  gains the field together with the emulator and the simulator, whose
  register map it matches.
- **A chiplet lending memory to MegaPad.** The contract allows it, and no
  channel carries it yet.
- **The physical layer.** The channels are logical; framing them onto UCIe's
  layers waits for the choice of where LM1's memory is placed.
