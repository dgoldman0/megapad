# Managed data

Status: proposed on 2026-11-20. Nothing is built. This is the specification
of managed data on MegaPad, and of the link's LM1 layer for it: how MegaPad
code holds, reads, writes, creates and releases objects that LM1's collector
manages, the windows in which it uses them, the handles it keeps past a
window, the requests that cross the queues, and the errors. Release plan
Part 9d sets four rules for this code, and Part 8c places this layer of the
link in MegaPad's docs (fantasy-engine `dev/PLAN.md`). LM1's side, what its
collector owes MegaPad, is §12 of LM1's `spec/05-memory-gc.md`. Uroko's
kinds for these objects, and what its checker enforces, are in
fantasy-engine `dev/ideas/language.md`. The link itself is
[chiplet-link.md](chiplet-link.md), and the design this follows is
[chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §7.

## 1. What managed data is

Desk code that wants its data collected creates it as LM1 objects: words in
LM1's tagged format (LM1's `spec/01-object-model.md`), in memory MegaPad
lends to LM1. LM1's collector frees an object once nothing uses it. Forth's
own buffers stay under manual control (decision 28 of the release plan).

The objects lie in the **shared heap**, the part of LM1's collected heap
that lies in memory MegaPad lends for the purpose. An object there is a
**shared object**. Every object MegaPad code can reach is a shared object,
and no shared object refers to an object outside the shared heap; LM1 keeps
this true. MegaPad's cores cannot read memory a chiplet lends
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §3), so the
shared heap lies in MegaPad's memory whatever the release plan's D8 decides
about LM1's own main memory. Nothing in this document depends on D8.

A **reference** is a word with one of LM1's two reference tags in bits 2:0,
`001` for an object and `011` for a cons cell, and the object's address in
bits 50:3. A reference stored in the shared heap or in a handle slot carries
zero in bits 63:51, which LM1 uses for metadata elsewhere, so two references
to one shared object are equal words. A **tagged word** is any word in LM1's
format: a fixnum, whose bit 0 is clear; a special, such as nil, t or a
character; or a reference.

Inside a window (§3), MegaPad code holds references on its stacks. Past a
window, it holds handles (§4).

## 2. The four rules

These restate Part 9d's rules as obligations on MegaPad code. The words they
name are listed in §14.

1. **Reads.** Code reads a shared object's words directly, with ordinary
   loads, at addresses inside an object it holds a valid reference to.
   Before it uses a word it read as a reference, a number or an object of
   some kind, it checks the word as §6 says.
2. **Writes.** Code changes a shared object only through the store words of
   §7, which check the target and log each reference they store. It never
   writes one with `!`, `C!`, `2!`, `+!`, `MOVE`, `FILL` or any other plain
   word, and it never writes a handle slot or a record of §9.
3. **Windows.** Code uses references only inside a window. A window belongs
   to one core and runs from an outermost open to the matching close, and
   code inside it waits for nothing but a new region (§8). While any core has
   a window open, the collector neither moves nor frees a shared object. A
   reference read inside a window therefore stays valid until that window
   closes, and no longer.
4. **Handles.** Code keeps an object past its window only as a handle, which
   names a slot in the handle table. The collector treats every slot as a
   root and rewrites it when the object moves. Every handle belongs to an
   owner, an applet, job or document that Desk tracks, and the owner's
   handles are released when it closes.

Code therefore never frees an object, never sizes one in advance, and never
sees an address change under it.

## 3. Windows and holds

### Two words

Each MegaPad core that uses managed data has a **window word** in its core
record (§9), which it alone writes:

| Value | State |
|-------|-------|
| 0 | `CLOSED`: no window is open on the core |
| 1 | `OPEN`: a window is open |
| 2 | `WAITING`: the core waits to open one |

LM1 writes one **hold word**, `HOLD`, in its block (§9), and every core reads
it:

| Bits | Field | Meaning |
|------|-------|---------|
| 0 | `HELD` | The collector holds MegaPad out: no window may open |
| 1 | `ACTIVE` | Every window has closed, and the collector is at work |
| 2 | `FAILED` | LM1 has stopped serving managed data (§11) |
| 63:3 | `EPOCH` | The number of holds that have ended since the function started |

A **hold** is the stretch from the collector's setting `HELD` to its
clearing it. While a hold is `ACTIVE`, no core has a window open, and the
collector may move and free shared objects. Outside a hold it does neither,
though it may mark, drain logs and answer requests.

The protocol relies on the order the link keeps. MegaPad's cores read data
straight from memory, and the external-memory controller completes each
access before it serves the next, so a core's store lands before its next
load is served. LM1 reaches this memory through its host-memory port, one
request at a time, each answered after it completes
([chiplet-link.md](chiplet-link.md) §4). Every access to the two words
therefore falls in one order that both chips see.

### Opening a window

A core keeps its window's nesting depth in its own memory. An open at depth
0 is an **outermost open**:

1. Store `OPEN` in the window word.
2. Load `HOLD`. If `FAILED` is set, store `CLOSED` and fail with
   `OBJ-E-FAILED`.
3. If `HELD` is clear, the window is open: set the depth to 1. If `EPOCH`
   differs from the epoch the core saw at its last open, a hold has ended
   since, and the core takes the region LM1 has left it (§8).
4. If `HELD` is set, store `WAITING` and ring the port, since the collector
   may have read `OPEN` and be waiting on this core. Then load `HOLD` again.
   If `HELD` is still set, sleep until an interrupt or a deadline. Go back
   to step 1.

An open at depth 1 or more only adds one to the depth, and never waits.

These steps keep the collector from moving an object that a window uses. A
core stores `OPEN` before it loads `HOLD`, and the collector stores `HELD`
before it loads the window words, so either the core sees `HELD` and backs
off, or the collector sees `OPEN` and waits. The second load in step 4 keeps
a wake from being lost: either it finds the hold over, or the collector,
which reads the window words after it clears `HELD`, finds `WAITING` and
rings the core.

### Closing a window

A close lowers the depth. The close that brings it to 0, the **outermost
close**:

1. stores the core's `FILL` if the window made objects (§8), and its log's
   `TAIL` if the window logged stores (§7);
2. stores `CLOSED` in the window word;
3. loads `HOLD`, and rings the port if `HELD` is set, so that the collector
   knows a window it waits for has closed.

Everything the window wrote lands before `CLOSED` does, so a collector that
reads `CLOSED` sees the window's objects and log entries.

### A hold

The collector:

1. stores `HELD` and waits for the write's answer;
2. reads every core's window word, then waits until each one it found `OPEN`
   reads otherwise, reading a core's word again when that core rings, and
   keeps answering region requests while it waits (§8);
3. sets `ACTIVE`: no window is open, and none can open;
4. does its work: it drains every log, reads every `FILL`, takes back
   MegaPad's regions and writes each core a fresh one, and moves or frees
   shared objects, rewriting the handle slots and every reference to an
   object it moves, so that when the work ends no word MegaPad can reach
   refers to an old location, and no forwarding pointer is reachable;
5. stores `HOLD` with `HELD` and `ACTIVE` clear and `EPOCH` one higher;
6. reads every core's window word again, and rings each core it finds
   `WAITING`.

### Polls

Code that works through many objects keeps its window short by polling.
`OBJ-POLL` loads `HOLD`. If `HELD` is clear, it returns false, at the cost
of that load. If `HELD` is set, it closes the window as an outermost close
does, whatever the depth, opens it again as an outermost open does, restores
the depth and returns true. True means a hold may have passed: every
reference the core read before the poll is invalid, in the polling word and
in every caller that holds one, and the code reads its objects again from
their handles. Handles stay valid through a poll.

### Nesting, exceptions and aborts

Windows nest on one core. An inner open and close count, and only the
outermost close ends the window, so a reference read at any depth stays
valid until the outermost close.

A window must close on every path, an exception's included, or the
collector waits on it for ever. `OBJ-WITHIN` runs an execution token inside
a window and closes the window when the token returns or throws, then passes
the exception on; code written by hand opens its windows that way.
`OBJ-OPEN` and `OBJ-CLOSE` serve the library and compiled code, which wrap
them in a `CATCH` of their own. KDOS turns machine faults into exceptions
([kdos-reference.md](kdos-reference.md) §1.2), so a fault inside a window
unwinds through the close. `ABORT` and `QUIT` reset the return stack, where
no close survives, so the chiplet module closes the aborting core's window
from the callback `QUIT` runs (`QUIT-XT!`), alongside the one KDOS installs.

### What a window may not contain

Inside a window, code does not:

- wait for LM1 or another core: for a task's result, a reply other than a
  region's, a lock that another core may hold across a wait, or a job;
- yield or sleep, with `PAUSE`, `TASK-YIELD`, `IDLE-UNTIL` or `MS`, or wait
  for input, as `KEY` does;
- start a task or a job with references, or hand a reference to another
  core or task, since objects cross as handles.

No window opens in an interrupt or trap handler; a handler that needs
managed data leaves the work to a task.

The first item rules out a deadlock: a core that waits inside its window for
LM1, while LM1 waits for that window to close, would wait for ever. A region
request is the one exception, because LM1 answers it without a hold. The
second keeps windows short. The third follows from rule 3, since a
reference means nothing once its window has closed, and another core or a
later task cannot tell when that is.

Microcore kernels open no windows. They work on buffers their caller
provides ([microcore-kernels-plan.md](microcore-kernels-plan.md) §2), so a
core copies what a kernel needs out of its objects inside its own window.
A microcore that later runs window code needs the order of §3's two words
from its cluster's bus port as well, which today's documents do not state.

### Bounds and starvation

The collector states three times in its block (§9), in nanoseconds:

| Field | Meaning |
|-------|---------|
| `WINDOW_LIMIT` | How long a window may stay open once `HELD` is set: code closes or polls within it |
| `HOLD_LIMIT` | How long a hold may stay `ACTIVE` |
| `HOLD_GAP` | How long the collector waits between the end of one hold and its next `HELD` |

The proposed targets for the first build are a millisecond for the first two
and a tenth of that for the third; the chiplet module measures them before
they are fixed.

Neither side starves the other while both keep these bounds. While `HELD` is
set no window opens, so the collector waits only for windows already open,
each of which closes or polls within `WINDOW_LIMIT`. A hold stays `ACTIVE`
at most `HOLD_LIMIT`, and `HOLD_GAP` lets every waiting core open its window
before the next hold begins, since a woken core needs only a store and a
load to do so.

When a side breaks its bound:

- A window still open `WINDOW_LIMIT` after `HELD` was set delays the hold.
  The collector waits on, because the window's references depend on it, and
  reports the core in a `NOTICE` (§10). A core that waits on that hold finds
  `HELD` without `ACTIVE`, knows the delay is MegaPad's own, and waits on as
  well.
- A hold that stays `ACTIVE`, at one epoch, for more than twice
  `HOLD_LIMIT` is overdue. A core that finds it so fails the function
  (§11), and the port then follows the link's rule for a chiplet that stops
  answering: cancel, then reset ([chiplet-link.md](chiplet-link.md) §8).

## 4. Handles and owners

### The handle

A **handle** is one cell:

| Bits | Field |
|------|-------|
| 31:0 | `INDEX`: the slot's number in the handle table |
| 63:32 | `GENERATION`: the slot's count of uses, from 1 |

A handle is never zero, so code can keep 0 for "no handle". It is not an
address and confers nothing: it names a slot that MegaPad's library looks
up.

### The table

The **handle table** is an array of 8-byte slots in memory MegaPad lends.
A slot holds a reference while its handle names an object, nil while its
handle is live but names none yet, and 0 while it is free.
The collector treats every slot that holds a reference as a root, and
rewrites it during a hold when the object moves. MegaPad writes slots only
inside a window, and LM1 only during a hold or in the one case below, so
the two chips never write a slot at once.

The table grows in segments and never moves. Segment k holds `FIRST` × 2^k
slots, where `FIRST` is a power of two the module chooses, and its first
slot has the index `FIRST` × (2^k − 1). The library finds an index's
segment with one count of leading zeros: k = 63 − clz(`INDEX` / `FIRST` +
1). The directory (§9) lists the segments' bases. When every slot is in
use, the module adds a segment, zeroed and listed before its first slot is
used. The 32-bit index is the only bound, and it allows about four billion
handles.

Beside the table, in its own memory, the library keeps each slot's
generation, its owner, its place in the owner's list, and its serial number
among the owner's handles. LM1 never reads these. Releasing a slot raises
its generation by one, and a slot whose generation has reached 2^32 − 1 is
retired rather than reused, so no handle ever names a slot that was reused
after it.

### Owners

An **owner** is named by an owner token: an id and a generation, both
positive, as Akashic's job service names a job's owner
(`akashic/runtime/sandbox-job-service.f`). Desk has a token for each applet
instance, its id and generation, and one for each job and document it
tracks. The library keeps a table of owners that grows on demand, and for
each owner the list of its handles, newest first.

- `OBJ-HANDLE` makes a handle owned by the token it is given.
- `OBJ-DROP` releases one handle.
- `OBJ-OWNER-MARK` returns the serial number the owner's next handle will
  take, and `OBJ-OWNER-RELEASE-TO` releases the owner's handles made since
  that mark. Compiled code uses the pair for the handles it makes while a
  word runs.
- `OBJ-OWNER-RELEASE` releases every handle the owner holds. Desk calls it
  when an owner closes, as `_DESK-HOST-RELEASE` calls
  `SBOX-CAPABILITY-OWNER-DRAIN` for a closing applet's jobs, while the
  token still names that owner and before the token can be reused.

Releasing a handle drops a root and frees nothing. The object goes once
nothing else uses it.

### Stale handles

A handle is **stale** when its index lies beyond the table, its slot is
free, or its generation is not the slot's. Every word that takes a handle
checks it, and returns `OBJ-E-STALE` for a stale one without touching the
table. Once the function has failed (§11), every handle is stale.

### Handles and tasks

Objects reach a task through handles. A request that starts a task names
its inputs by their handles, and names a handle made for nil whose slot
will receive the result. MegaPad lends these handles with the request until
its reply, as the sender of any request lends the memory it names
([chiplet-link.md](chiplet-link.md) §8), and neither re-points nor releases
them meanwhile, a cancelled request's included. LM1's runtime reads the
input slots when the task starts, and stores the result's reference in the
result slot before it replies; this is the one slot LM1 writes outside a
hold (LM1's `spec/06-runtime.md` §3.5). The rest of the protocol for tasks
belongs to the link's layer for jobs, which is still to specify.

## 5. Objects MegaPad reads and makes

Every shared object starts with a header word, tag `111`, and LM1's object
model numbers the words after it from 0 as **fields**: field i is the word
at the object's address plus 8 × (i + 1), as LM1's `LD` and `ST` count.
MegaPad reads and makes four kinds of object, with the layouts of LM1's
`spec/01-object-model.md` §4:

| Kind | Header subtype | Fields |
|------|----------------|--------|
| Cons cell | `00001`, size 2 | 0 the car and 1 the cdr, tagged words |
| Vector | `00010` | 0 the length n, a fixnum; 1 to n the elements, tagged words |
| Bytevector or string | `00011` | 0 the length in bytes, a fixnum; then the bytes, raw, from field 1's first byte |
| Record | `10000` | 0 the count t of tagged fields, a fixnum; 1 to t the tagged fields; then raw words to the end |

The header's size counts the fields. Its shape ID names the kind: LM1 states
the shape IDs of cons cells, vectors, bytevectors and strings in its block
(§9), and gives each record kind its own when MegaPad registers it (§8). A
string is a bytevector with the string shape, and its bytes are UTF-8. Cons
cells in the shared heap carry headers, unlike the headerless cells LM1 may
keep elsewhere, so their car is always field 0 and their cdr field 1. Other
kinds of object, which LM1 code may make, MegaPad reads only through their
headers.

A record's raw words hold numbers whole, such as 64-bit counts, hashes and
times. The collector never reads a raw word as a reference, nor any byte of
a bytevector, so any bit pattern is safe in either.

## 6. Reading

Inside a window, code may load any word of a shared object it holds a valid
reference to, at the reference with its low three bits cleared. Before it
uses what it reads, it checks:

- **A header.** The word at the object's address has tag `111`, is not a
  forwarding pointer, has one of §5's subtypes, and has a size that covers
  the field read. A header's subtype, size and shape never change while a
  window is open. Its GC bits, 63:56, may, and code ignores them.
- **A reference.** A tagged word used as a reference has tag `001` or
  `011`, zero in bits 63:51, and an address inside the shared heap.
  `OBJ>REF` makes this check against the blocks MegaPad has granted, which
  the library lists in its own memory. A word that fails is never followed:
  it gives `OBJ-E-KIND`, or `OBJ-E-CORRUPT` when it has a reference tag.
- **A kind.** Code that treats an object as a cons, a vector, a string or a
  record of some kind first checks the header's shape, once per object in a
  window; the result holds until the window closes, because the header does
  not change.
- **A number.** A tagged word used as a number is a fixnum, whose value
  `OBJ>INT` gives by shifting it right by one, keeping its sign.

Raw words and bytes need no check, since every bit pattern is a value.

LM1 code may write a shared object's fields while a window reads them. Each
word is read whole, as it was before or after the write, because both chips
reach it as one access at the external-memory controller. Code that needs
several fields to change together coordinates through tasks, whose inputs
are not written while they run (fantasy-engine `dev/ideas/language.md`).

These checks keep MegaPad safe from a faulty chiplet. No reference LM1
wrote leads MegaPad's reads outside the shared heap, and no store of §7
lands outside an object. A chiplet that writes wrong values into shared
objects can still mislead the code that reads them, which is the reach the
interconnect allows a failed chiplet: the code that chose to use memory the
chiplet works on.

## 7. Writing

### Stores

The store words write one field, one raw word or a run of bytes, and first
check the target: its address lies in the shared heap, its header is sound,
and the field lies inside the object. They never change a header, a
record's field 0, or the length in field 0 of a vector or bytevector.

| Word | Writes |
|------|--------|
| `OBJ!` | A tagged word into a tagged field: a cons's car or cdr, a vector's element, a record's tagged field |
| `OBJ-RAW!` | A cell into a record's raw word |
| `OBJ-BYTES!` | Bytes from a span into a bytevector |

A **reference store** is a store of a reference into a tagged field or a
handle slot. `OBJ-HANDLE` and `OBJ-SET` log every one they make. `OBJ!`
logs every one too, except a store into an object that lies in the storing
core's current region (§8): such an object was made since the last hold,
the hold that retires the region traces it whole itself whenever that
matters to what lives, and stores into it need no entry. Stores of fixnums,
specials, raw words and bytes make no reference and are never logged.

### The store log

Each core has a **store log**, a ring of 8-byte entries, each the address of
a word the core stored a reference into. The ring lies in memory MegaPad
lends for reading, and its count `TAIL` lies in the core's record; LM1
writes `LOG_HEAD`, the number of entries it has taken, in its block. Both
count from zero and never wrap. Entry n lies at the ring's base plus 8 × (n
mod `LOG_CAPACITY`), where `LOG_CAPACITY` is a power of two.

A core writes the word, then the entry, and stores `TAIL` after both: at
the outermost close, and whenever the ring is half full. An entry LM1 can
see therefore names a store that has landed. LM1 replays each entry through
its own write barrier: it reads the word the entry names and records what
its own store of that value would record. The rules for the generational
invariant, cards and remembered sets thus exist once, on LM1 (LM1's
`spec/05-memory-gc.md` §12.4). LM1 may drain a log at any time, and during
each hold it drains every log completely before it decides anything.

The log keeps the generational invariant, in which an old object may refer
to a young one only where the collector has a record of it. An old shared
object comes to refer to a young one only through a reference store into
it, MegaPad logs every such store of its own, and the collector replays
every entry before it collects. LM1's card tables and remembered sets lie
in LM1's own memory, which MegaPad cannot write, so the log is the one way
MegaPad's stores reach them.

When a core's ring is full, the core stores `TAIL`, rings the port and waits
for `LOG_HEAD` to move. LM1 drains a log without a hold, so this wait never
depends on a window, and a core may wait so even inside one; LM1 rings the
core when it has drained a ring it found full. Outside a window, once
`LOG_HEAD` has reached `TAIL`, a core may move to a larger ring. It writes
the new ring's address and capacity into its record, and only then stores a
`TAIL` that counts entries in the new ring; the count runs on from where it
stood, so entry n lies at the new ring's base plus 8 × (n mod its capacity).
LM1 reads `TAIL` before it reads the ring's address and capacity, so it
finds the new ring whenever it finds entries written there.

### Numbers

A number lives in an object in one of three ways:

- **As a fixnum** in a tagged field, when it lies between −2^62 and 2^62 −
  1: the field holds the number shifted left by one. `INT>OBJ` makes the
  fixnum, and fails with `OBJ-E-RANGE` for a number outside that range, so
  raw bits never reach a tagged field.
- **As a raw word** in a record, when the record kind declares the field as
  a cell: all 64 bits, unchanged, which the collector never reads as a
  reference. Cells wrap at 64 bits in Uroko, and any cell fits here.
- **As bytes** in a bytevector, for runs of numbers that code reads and
  writes in bulk.

A flag in a tagged field is `t` for true and nil for false, and a character
is LM1's character special.

## 8. Creating objects

### Regions

MegaPad makes objects in **allocation regions**, parts of the shared heap
that LM1 gives one core at a time. A core places each object at its
region's next free byte, writes its header and every field before the window
closes, and moves its own count of the free byte on. At the outermost close
it stores that count as `FILL` in its record. The bytes from the region's
base to `FILL` are then whole objects in a row, which the collector walks by
their headers.

A core makes objects without any hand-off while its region has room. When
the next object does not fit:

1. The core stores `FILL`, puts a `REGION` request on its own queue pair,
   asking for at least the bytes it needs, and rings the port.
2. LM1 writes a new region into the core's `NEXT_BASE` and `NEXT_LIMIT`,
   raises `NEXT_SERIAL`, and replies. It answers from free space, or by
   growing the shared heap with a `GROW` (§10), and never by collecting, so
   it answers without a hold, and while it waits for one.
3. The core takes the new region and makes the object there. LM1 writes
   `NEXT_BASE` and `NEXT_LIMIT` before it raises `NEXT_SERIAL`, and a core
   reads them after it reads `NEXT_SERIAL`, so it never takes half of one
   region and half of another.

The core may wait for this reply inside a window; it is the one wait a
window allows. LM1 keeps free space in reserve and asks for blocks before
it runs out, so the reply seldom waits on a `GROW`. When LM1 cannot meet
the request because MegaPad has refused it memory, it replies `NO_MEMORY`,
and the object is not made (`OBJ-E-NOMEM`).

A hold retires every region MegaPad holds. Before it ends, LM1 writes each
core that makes objects a fresh region, so a core finds a region waiting at
its first open after the hold (§3, step 3), and asks only when it fills one
between holds. LM1 sizes each region at least as large as asked, and larger
for a core that fills its regions quickly. No size is fixed, and an object
larger than any region is made in a region of its own, by the same request.

### Shapes

A record kind is registered once, outside a window, with `OBJ-SHAPE`, which
sends LM1 a `SHAPE` request and returns the kind's shape ID. Registering the
same description again returns the same ID. The description lies in memory
MegaPad lends for reading:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `TAGGED` | t, the number of tagged fields |
| `+0x08` | `RAW` | r, the number of raw words |
| `+0x10` | `NAME_LEN` | The length of the kind's name in bytes |
| `+0x18` | kinds | t words, one for each tagged field, then r words, one for each raw word |
| after the kinds | name | The name, UTF-8 |

A tagged field's kind word is 0 for any tagged word, 1 a fixnum, 2 a flag,
3 a character, 4 any reference, 5 a list (nil or a cons), 6 a string, 7 a
vector, 8 a bytevector, and 9 a record, with the record kind's shape ID in
bits 63:32. A raw word's kind is 16 for a cell, 17 for a signed number and
18 for an unsigned one. LM1 keeps the kinds with the shape: its runtime
refuses a store by LM1 code that breaks a field's kind, and shows records to
Lisp by them.

## 9. Records in lent memory

The chiplet module keeps its records in lendable memory, lent to LM1 with
the rights below. As with the link's queues, each record lies in memory that
only its writer can write ([chiplet-link.md](chiplet-link.md) §8), except
the handle table and the shared heap, which both chips write under the
window protocol.

| Record | Written by | Lent to LM1 for |
|--------|------------|-----------------|
| Control block, core records, store logs, handle directory, shape descriptions | MegaPad | Reading |
| LM1's block | LM1 | Reading and writing |
| Handle table segments | Both: MegaPad inside windows, LM1 during holds | Reading and writing |
| Shared heap blocks | Both | Reading and writing |
| Queue pairs | As chiplet-link.md §8 says | As it says |

Every field is a 64-bit little-endian word, and every record is a whole
number of 64-byte units.

### The control block

MegaPad names it in its `ATTACH` request (§10).

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `MAGIC` | ASCII `MANAGED` and a zero byte, first character in the low byte |
| `+0x08` | `VERSION` | 1 for this document |
| `+0x10` | `CORES` | The number of core records: the machine's full cores and microcores |
| `+0x18` | `CORE_TABLE` | Address of core record 0; record c lies 128 × c bytes on |
| `+0x20` | `LM1_BLOCK` | Address of the block LM1 writes |
| `+0x28` | `HANDLE_DIR` | Address of the handle directory |

The records are sized from the machine's core count, so the protocol serves
every core a machine has.

### A core record

Core c alone writes its record.

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `WINDOW` | The window word: 0 `CLOSED`, 1 `OPEN`, 2 `WAITING` |
| `+0x08` | `REGION` | Base of the region the core makes objects in, or 0 |
| `+0x10` | `FILL` | The first byte after the core's last whole object there |
| `+0x18` | `LOG_RING` | Address of the core's store log, or 0 |
| `+0x20` | `LOG_CAPACITY` | Entries in the ring, a power of two |
| `+0x28` | `TAIL` | Entries the core has written |
| `+0x40` | `PAIR` | The record of the core's queue pair (chiplet-link.md §8), six words, zero until the pair is made |

A `FILL` that does not lie inside the region `REGION` names means the core
has made nothing there yet.

### LM1's block

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `HOLD` | The hold word (§3) |
| `+0x08` | `WINDOW_LIMIT` | Nanoseconds (§3) |
| `+0x10` | `HOLD_LIMIT` | Nanoseconds |
| `+0x18` | `HOLD_GAP` | Nanoseconds |
| `+0x20` | `SHAPE_CONS` | The shape ID of cons cells |
| `+0x28` | `SHAPE_VECTOR` | Of vectors |
| `+0x30` | `SHAPE_BYTES` | Of bytevectors |
| `+0x38` | `SHAPE_STRING` | Of strings |
| `+0x80` | core entries | Core c's entry lies 64 × c bytes on |

Core c's entry:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `LOG_HEAD` | Entries LM1 has taken from the core's log |
| `+0x10` | `NEXT_BASE` | Base of the region LM1 gives the core next, or 0 |
| `+0x18` | `NEXT_LIMIT` | Its exclusive limit |
| `+0x20` | `NEXT_SERIAL` | Raised each time LM1 writes a region here |

### The handle directory

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `FIRST` | Slots in segment 0, a power of two |
| `+0x08` | `SEGMENTS` | Segments in use |
| `+0x10` | segment bases | 64 words, segment k's base 8 × k bytes on |

Sixty-four entries cover every 32-bit index.

### What each side checks

Each chip checks what it reads from the other's words, as the link's queues
require:

- LM1 checks MegaPad's records. A window word holds one of its three values;
  `TAIL` never moves back and never runs more than `LOG_CAPACITY` ahead of
  `LOG_HEAD`; every log entry names a word inside the shared heap or the
  handle table; every `REGION` and `FILL` lies inside what LM1 granted; and
  every object a core made has a sound header, a size inside its region, a
  registered shape, and references only into the shared heap. A failure is
  reported in a `NOTICE`, and LM1 stops serving managed data (§11).
- MegaPad checks LM1's. `EPOCH` never moves back and moves on by one for
  each hold; `FAILED` stays set once set; `LOG_HEAD` never passes `TAIL`; a
  region LM1 writes lies inside a block MegaPad granted and overlaps no
  other core's; and every reference MegaPad reads, from a handle slot or a
  field, passes §6's check.

## 10. Requests on the queues

The function's class number is assigned in
[chiplet-link.md](chiplet-link.md) §8 when this layer is accepted; this
document calls the class `MANAGED`. LM1 offers it once its runtime is up,
asking in `OUT_LEN` for the shared heap's first block, and MegaPad's grant
gives the function its first queue pair and that block as working memory.
The grant's pair carries `ATTACH` and LM1's own requests, and the core that
its `NOTIFY` names serves them, even while it waits for a reply of its own.
Each core's requests travel on a pair of the core's own, whose `NOTIFY`
names that core, so its replies wake it alone. The module makes a core's
pair before the core's first request and records it in the core record,
where LM1 reads it at the core's first ring. A core keeps one request
unanswered at a time, so its pair needs two entries each way.

From MegaPad:

| Operation | Number | Request | Reply |
|-----------|--------|---------|-------|
| `ATTACH` | `0x100` | `IN_ADDR`, `IN_LEN`: the control block | `OK` once LM1 has written its block; `BAD_RECORD` when a record fails LM1's checks |
| `REGION` | `0x101` | `ARG0`: the bytes needed, 0 for LM1's choice | `OK` once `NEXT` holds a new region; `NO_MEMORY` |
| `SHAPE` | `0x102` | `IN_ADDR`, `IN_LEN`: a description (§8) | `OK` with the shape ID in `ARG0`; `BAD_RECORD` |
| `COLLECT` | `0x103` | `ARG0`: 0 the young objects, 1 the whole shared heap, 2 the whole heap, moving every object it can | `OK` once the collection has ended, with the shared heap's live bytes in `ARG0` and the epoch in `ARG1` |

From LM1, on the grant's pair:

| Operation | Number | Request | Reply |
|-----------|--------|---------|-------|
| `GROW` | `0x180` | `ARG0`: the bytes wanted, at least | `OK` with a new block's base in `ARG0` and its length in `ARG1`, lent for reading and writing; `NO_MEMORY` when MegaPad refuses |
| `RETURN` | `0x181` | `IN_ADDR`, `IN_LEN`: a block LM1 no longer uses | `OK` once MegaPad has taken it back |
| `NOTICE` | `0x182` | `ARG0`: 1 an overdue window, 2 corrupt data, 3 a broken log; `ARG1`: in bits 15:0 the core's ID, and for an overdue window in bits 63:16 the nanoseconds it has stayed open; `IN_ADDR`: the address concerned | `OK` |

The class's statuses are `0x100` `NO_MEMORY` and `0x101` `BAD_RECORD`.
`COLLECT` serves memory pressure and the conformance tests of §15. The
collection it asks for takes holds like any other, so a core sends it
outside a window.

The shared heap grows in the blocks that `GROW` obtains. LM1 asks for blocks
that each at least double the heap so far, so a handful of blocks covers any
size, and KDOS's allocator for lendable memory
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §9) keeps
them within the port's lent ranges.

A doorbell keeps its meaning, "look at your queues", and the hold word is
one more place to look:

- a core rings the port after it puts a request on a queue, at an outermost
  close while `HELD` is set, when it stores `WAITING`, and when its store
  log is half full or full;
- LM1 rings a core after it puts a reply on the core's pair, at the end of a
  hold when it finds the core `WAITING`, and when it has drained a log it
  found full.

## 11. Errors and failure

The words return these statuses, whose numbers are assigned when the module
is written:

| Status | Meaning |
|--------|---------|
| `OBJ-E-ABSENT` | No engine serves managed data |
| `OBJ-E-FAILED` | The function has failed (below) |
| `OBJ-E-STALE` | The handle names no live slot |
| `OBJ-E-OWNER` | The owner token is not two positive cells |
| `OBJ-E-WINDOW` | A word that needs a window ran outside one, or one that needs none ran inside one |
| `OBJ-E-KIND` | A word is not of the kind expected: not a reference, not a fixnum, not of the shape |
| `OBJ-E-RANGE` | A field or byte lies outside its object, or a number does not fit a fixnum |
| `OBJ-E-CORRUPT` | A word breaks LM1's object model: a reference outside the shared heap, or a header that is not sound |
| `OBJ-E-NOMEM` | No memory for a region, a handle segment or a log |

The function **fails** when LM1 sets `FAILED`, when a hold is overdue (§3),
when one of its queues breaks or a request misses its deadline
([chiplet-link.md](chiplet-link.md) §8), or when the chiplet is reset. From
then on every word returns `OBJ-E-FAILED`, and `OBJ-WITHIN` throws it; every
handle is stale; releasing an owner succeeds and does nothing; and the
objects are lost. The module returns the function's memory to KDOS once no
core has a window open. A restarted chiplet offers the function again, and
the module starts it afresh. Managed data does not outlive a failure, so
what must outlive one is saved as Desk saves any document.

## 12. Capabilities and forging

Capability mode, which the release plan makes a required part of LM1 (Part
8b), keeps each LM1 job to its own data. Managed data meets it at three
points.

**Handles are not capabilities.** A handle is a number that MegaPad's
library looks up in its own table, and MegaPad's privilege levels and MPU
decide which MegaPad code may reach that table, as for any of its memory. A
handle confers nothing on LM1.

**LM1's runtime makes the capabilities.** MegaPad gives LM1 an object, such
as a task's input, by naming it with a handle. Only LM1's runtime reads the
slot and turns the reference into a capability for the job, read-only for a
task's input. MegaPad cannot make a capability.

**MegaPad's words gain no authority outside the shared heap.** MegaPad can
store any word into its own memory, the shared heap included, so a word
there with a reference tag may be forged. LM1 checks every reference it
loads from the shared heap, and one that names memory outside it traps and
is never followed (LM1's `spec/05-memory-gc.md` §12.8). A reference stored
in shared memory therefore gains authority there and nowhere else
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §5). Inside
the shared heap, which holds Desk's own data, the library and rule 2 keep
MegaPad's stores sound, and LM1's collector checks what it walks.

## 13. One engine

The words and rules describe what code needs, not which chip serves it, as
decision 31 of the release plan asks. On a machine without LM1, MegaPad's
own engine can serve the class with the same records and the same words:
its collector runs on a MegaPad core, holds the other cores out through the
same hold word, and replays the same logs, so a program means the same on
one engine as on two. The simplest engine that conforms never frees
anything: every rule still holds, and only memory use differs. Whether the
release builds a collector for MegaPad is open (§16), since every
configuration of the release has LM1.

A task's own objects follow decision 32 on either engine. They live in a
heap that lasts as long as the task, its result is copied out, and a store
that would let any other of them escape is refused (fantasy-engine
`dev/ideas/language.md`).

## 14. The words

These are the managed-data words of the Leviathan vocabulary, which
MegaPad's chiplet module provides
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §9). Their
names and stack orders are a first draft, to settle when the module is
written. Effects use Uroko's kinds (fantasy-engine `dev/ideas/language.md`):
`ref` a reference, `tagged` a tagged word, `handle` a handle, and `ior` a
status, 0 for success. The window column says whether a word runs inside a
window, outside one, or either.

Windows:

| Word | Effect | Window | Does |
|------|--------|--------|------|
| `OBJ-READY?` | `( -- flag )` | Either | Whether an engine serves managed data |
| `OBJ-OPEN` | `( -- ior )` | Either | Open the core's window, or nest |
| `OBJ-CLOSE` | `( -- )` | Inside | Close the innermost open |
| `OBJ-WITHIN` | `( i*x xt -- j*x )` | Either | Run xt in a window and close it on return or exception; a failed open throws its status |
| `OBJ-POLL` | `( -- reopened: flag )` | Inside | Let a waiting hold pass; true when one may have |

Handles and owners:

| Word | Effect | Window | Does |
|------|--------|--------|------|
| `OBJ-HANDLE` | `( t: tagged id: u gen: u -- h: handle ior )` | Inside | Make a handle owned by the token, for a reference or for nil |
| `OBJ-GET` | `( h: handle -- r: ref ior )` | Inside | The object the handle names; `OBJ-E-KIND` while it names none |
| `OBJ-SET` | `( r: ref h: handle -- ior )` | Inside | Make the handle name another object |
| `OBJ-DROP` | `( h: handle -- ior )` | Either | Release one handle |
| `OBJ-OWNER-MARK` | `( id: u gen: u -- mark: u )` | Either | The serial the owner's next handle takes |
| `OBJ-OWNER-RELEASE-TO` | `( mark: u id: u gen: u -- ior )` | Either | Release the owner's handles made since the mark |
| `OBJ-OWNER-RELEASE` | `( id: u gen: u -- ior )` | Outside | Release all of the owner's handles, in as many short windows as it needs |

Reading, inside a window:

| Word | Effect | Does |
|------|--------|------|
| `OBJ@` | `( r: ref i: u -- t: tagged )` | Field i, unchecked: rule 1's direct read |
| `OBJ-SHAPE@` | `( r: ref -- shape: u ior )` | The shape in the object's header, after checking the header |
| `OBJ-LENGTH` | `( r: ref -- n: u ior )` | A vector's elements, a bytevector's bytes, or a record's tagged fields |
| `OBJ>REF` | `( t: tagged -- r: ref ior )` | Check a tagged word as a reference |
| `OBJ-CAR`, `OBJ-CDR` | `( r: ref -- t: tagged ior )` | A cons cell's fields, after checking its header |
| `OBJ-RAW@` | `( r: ref j: u -- x ior )` | A record's raw word j |
| `OBJ>SPAN` | `( r: ref buf: span -- n: u ior )` | Copy a bytevector's bytes into the buffer, as many as fit |

Writing and making, inside a window:

| Word | Effect | Does |
|------|--------|------|
| `OBJ!` | `( t: tagged r: ref i: u -- ior )` | Store into tagged field i, logging a reference |
| `OBJ-RAW!` | `( x r: ref j: u -- ior )` | Store into a record's raw word j |
| `OBJ-BYTES!` | `( s: span r: ref at: u -- ior )` | Copy bytes into a bytevector from byte `at` on |
| `OBJ-CONS` | `( car: tagged cdr: tagged -- r: ref ior )` | A new cons cell |
| `OBJ-VECTOR` | `( fill: tagged n: u -- r: ref ior )` | A new vector of n elements |
| `OBJ-BYTES` | `( s: span -- r: ref ior )` | A new bytevector holding the span's bytes |
| `OBJ-STRING` | `( s: span -- r: ref ior )` | A new string holding the span's UTF-8 |
| `OBJ-RECORD` | `( shape: u -- r: ref ior )` | A new record of the shape, with nil in its tagged fields and zero in its raw words |

Values, inside or outside a window:

| Word | Effect | Does |
|------|--------|------|
| `OBJ-NIL`, `OBJ-T` | `( -- t: tagged )` | Nil and t |
| `INT>OBJ` | `( n -- t: tagged ior )` | The fixnum for n, when n fits |
| `OBJ>INT` | `( t: tagged -- n ior )` | A fixnum's value |

Kinds and collections, outside a window:

| Word | Effect | Does |
|------|--------|------|
| `OBJ-SHAPE` | `( desc: span -- shape: u ior )` | Register a record kind (§8) |
| `OBJ-COLLECT` | `( how: u -- ior )` | Ask for a collection (§10) |

Uroko's record declarations generate typed words over these, such as a
constructor for each record kind and an accessor for each field
(fantasy-engine `dev/ideas/language.md`). Akashic's library for the chiplet
(release plan Part 9b) adds Desk's owners.

A word written by hand under the rules, which copies the name held in a
record's first tagged field into a line buffer:

```forth
: TITLE  ( buf: span h: handle -- n: u )
    [:  OBJ-GET THROW         ( buf r )
        1 OBJ@ OBJ>REF THROW  ( buf name )
        -ROT OBJ>SPAN THROW   ( n )
    ;] OBJ-WITHIN ;
```

The handle comes in from outside any window, the references live only in
the quotation that `OBJ-WITHIN` runs, and the copy lands in the caller's
buffer, bounded by its length. `OBJ>SPAN` checks that the name is a
bytevector, since it reads the header for the length.

## 15. Conformance tests

The chiplet module of release plan Part 8d passes these tests from its first
day, and they join Part 8g's link conformance tests for both RTL sides
later. Each drives the link as MegaPad does, through the records, queues and
doorbells above, from MegaPad's chiplet module or from a host harness that
plays MegaPad's part. An object is **live** when it can be reached from a
handle. A **young collection** collects the shared heap's young objects, as
a nursery collection does LM1's, and `COLLECT` asks for one with 0. The
module provides two test hooks beside the link: one keeps a hold `ACTIVE`
for a set time, and one writes a chosen word into the shared heap as LM1
would.

1. **Start.** Sets up a chiplet that offers `MANAGED`. Does: MegaPad grants
   it, builds its records and sends `ATTACH`. Checks: the reply is `OK`;
   `HOLD` reads 0; the three times are nonzero; the four shape IDs are
   nonzero and differ; every core entry's `LOG_HEAD` reads zero.
2. **A handle survives a young collection.** Sets up, in one window on core
   0, a record whose tagged fields hold the fixnum 7 and a string "abc", and
   whose raw word holds 2^64 − 1, with a handle owned by (1, 1). Does:
   `COLLECT` 0. Checks: the epoch has moved on; in a new window, `OBJ-GET`
   gives a reference whose fields read 7, a string "abc" and 2^64 − 1; the
   slot holds the record's current address.
3. **A handle survives an old-generation collection.** Sets up the record of
   test 2, made old by three more young collections. Does: `COLLECT` 2,
   which moves every object it can. Checks: as in test 2; the record's
   address has changed, and the slot holds the new one.
4. **A store into an old object is seen by the next young collection.** Sets
   up an old record A held by a handle; then, in one window, core 0 makes a
   young record B holding 42, stores B into A's tagged field 1 with `OBJ!`,
   and closes, keeping no handle to B. Does: `COLLECT` 0. Checks: in a new
   window, A's field 1 refers to a record holding 42; no word refers to B's
   old address if B moved; core 0's `LOG_HEAD` equals its `TAIL`.
5. **A moving collection waits while a window is open.** Sets up record X
   held by handle hX; core 0 opens a window and reads X's reference r. Does:
   core 1 sends `COLLECT` 2 while core 0's window stays open. Checks: while
   it does, `HOLD` shows `HELD` without `ACTIVE`, the epoch stands still,
   and r still reads X's fields; after core 0 closes, the collection ends,
   the epoch moves on, and hX gives X with its fields intact.
6. **An object lives while only a window uses it.** Sets up record A, held
   by a handle, whose field 1 refers to record Y holding 9, which nothing
   else refers to; core 0 opens a window, reads Y's reference r from A, and
   stores nil into A's field 1. Does: core 1 sends `COLLECT` 1 while core 0
   keeps its window open longer than the collection would take. Checks: r
   reads 9 until core 0 closes; after the collection ends, the live bytes in
   its reply have fallen by Y's size.
7. **No window opens during a hold.** Sets up a hold that the test hook
   keeps `ACTIVE`. Does: core 0 calls `OBJ-OPEN` during it. Checks: core 0's
   window word reads `WAITING` while the hold lasts; `OBJ-OPEN` returns 0
   only after `HELD` clears; a doorbell from the port woke core 0; core 0
   sees the new epoch and makes its next object in the region LM1 wrote in
   its `NEXT` fields during the hold.
8. **Closing an owner releases its handles.** Sets up handles of owner
   (5, 1) to records P, Q and R, and one of owner (6, 1) to Q. Does:
   `OBJ-OWNER-RELEASE` 5 1, then `COLLECT` 1. Checks: the three handles of
   (5, 1) are stale; (6, 1)'s handle gives Q intact; the live bytes have
   fallen by the sizes of P and R.
9. **A mark releases a word's own handles.** Sets up one handle of owner
   (7, 1). Does: takes `OBJ-OWNER-MARK`, makes two more handles, and calls
   `OBJ-OWNER-RELEASE-TO` with the mark. Checks: the two later handles are
   stale, and the first is live.
10. **Stale handles are found.** Sets up handle h, released, then handle h2,
    which takes h's slot. Checks: h gives `OBJ-E-STALE` and h2 its object; 0
    and an index beyond the table give `OBJ-E-STALE`; a word given a stale
    handle changes no slot.
11. **The handle table grows.** Sets up `FIRST` + 1 live handles, so that the
    last lies in segment 1. Does: `COLLECT` 2. Checks: every handle gives
    its object intact, segment 1's included.
12. **Re-pointing a handle is seen.** Sets up handle h naming young record C;
    in one window, core 0 makes young record D and `OBJ-SET`s h to it. Does:
    `COLLECT` 0. Checks: h gives D; the live bytes do not count C.
13. **A full store log.** Sets up core 0 with a log of 8 entries and an old
    vector V of 32 elements held by a handle. Does: in one window, core 0
    makes 32 young records and stores each into an element of V. Checks:
    core 0's `TAIL` reached 32, and never ran more than 8 ahead of
    `LOG_HEAD`; after `COLLECT` 0, all 32 records are live through V.
14. **Several cores.** Sets up cores 0 and 1 each making records and handles
    in windows of their own at once, with core 1's window still open after
    core 0's closes. Does: `COLLECT` 2. Checks: nothing moves until core 1
    closes; both logs are drained; every handle of both cores gives its
    object.
15. **Nested windows.** Sets up core 0 with a window, a nested one, and the
    nested one closed again. Does: core 1 sends `COLLECT` 2. Checks: core
    0's window word reads `OPEN` after the inner close; nothing moves until
    the outer close turns it `CLOSED`; then the collection ends.
16. **Polls.** Sets up core 0 in a window holding record X's reference, with
    handle hX. Does: `OBJ-POLL` with no hold waiting; then core 1 sends
    `COLLECT` 2, and core 0 polls again. Checks: the first poll returns
    false and the epoch stands still; the second returns true, the epoch has
    moved on, and `OBJ-GET` of hX gives X at its new address, intact.
17. **A region request during a hold.** Sets up core 0 in a window with its
    region nearly full, and core 1's `COLLECT` 2, so that `HELD` is set and
    the collector waits for core 0. Does: core 0 makes an object larger than
    its region's remainder. Checks: LM1 answers core 0's `REGION` while
    `HELD` is set and before `ACTIVE`; the object lies in the new region;
    the collection goes ahead once core 0 closes.
18. **Regions after a hold.** Sets up objects made by core 0, then a hold.
    Does: core 0 opens a window and makes an object. Checks: the object lies
    in the region LM1 wrote during the hold, not the old one; the objects
    made before the hold give their contents through their handles.
19. **Raw words and bytes are never references.** Sets up record Z, held by
    nothing; a record R, held by a handle, whose raw word holds Z's address
    with tag `001`; and a bytevector, held by a handle, whose bytes hold the
    same word. Does: `COLLECT` 1, then `COLLECT` 2. Checks: the live bytes
    do not count Z; R's raw word and the bytevector's bytes read back
    unchanged.
20. **Numbers round trip.** Sets up a record kind with a tagged field and a
    raw word. Does: stores −2^62, 2^62 − 1 and 0 into the tagged field with
    `INT>OBJ` and `OBJ!`, tries 2^62, and stores 2^63, −1 and 1 into the raw
    word, with a `COLLECT` 2 after each store. Checks: each reads back
    exactly; `INT>OBJ` of 2^62 fails with `OBJ-E-RANGE` and nothing is
    stored.
21. **A forged reference is refused.** Sets up, through the test hook, a
    word with tag `001` that names memory outside the shared heap, in a
    tagged field of a live record. Does: through the test hook, LM1 loads
    the field with `LD`; then `COLLECT` 1. Checks: the load traps with `TRAP_SHARED_REF` and the
    named memory is never accessed; the collector reports corrupt data in a
    `NOTICE` and sets `FAILED` instead of following the word; core 0's next
    `OBJ-OPEN` returns `OBJ-E-FAILED`.
22. **MegaPad refuses a bad reference from LM1.** Sets up, through the test
    hook, a reference that names memory outside the shared heap in a live
    record's field. Does: core 0 reads the field and calls `OBJ>REF`.
    Checks: the result is `OBJ-E-CORRUPT`, and core 0 never loads the named
    address.
23. **Failure.** Sets up handles held by two owners, with no window open.
    Does: resets the chiplet. Checks: `OBJ-OPEN` and `OBJ-GET` return
    `OBJ-E-FAILED`; releasing each owner returns 0; the module returns the
    function's memory to KDOS; MegaPad memory outside the function's is
    untouched.
24. **An overdue hold.** Sets up a hold that the test hook keeps `ACTIVE`
    for more than twice `HOLD_LIMIT`. Does: core 0 calls `OBJ-OPEN`.
    Checks: core 0 fails the function and gets `OBJ-E-FAILED`; the port is
    cancelled and reset by the link's rule.
25. **An overdue window is reported.** Sets up core 0 keeping a window open
    past `WINDOW_LIMIT` after `HELD` is set. Checks: LM1 sends a `NOTICE`
    of kind 1 naming core 0; nothing moves until core 0 closes; the hold
    then completes.
26. **Writes from both chips are whole.** Sets up, through the test hook,
    LM1 writing two different references in turn into one field of a live
    record, and fixnums into another, without pause. Does: core 0 reads both
    fields in a loop, in windows that poll. Checks: every word read is one
    of the values written, and the header's subtype, size and shape never
    change.
27. **Shapes.** Does: registers one description twice, then one with a field
    kind of 15. Checks: the two registrations return the same shape ID, and
    the third fails with `BAD_RECORD`.

## 16. Still open

- **The class number**, assigned in chiplet-link.md §8.
- **Tasks.** The requests that start a task and return its result, of
  which §4 fixes only how they name objects, belong to the link's layer for
  jobs, which is still to specify.
- **A collector for MegaPad.** Whether the release builds one (§13). Every
  release configuration has LM1, and decision 32's comparison of one engine
  with two concerns tasks, whose heaps need no collector.
- **Lendable memory.** How KDOS's allocator for lendable memory places the
  shared heap's blocks, the handle table and the records within the port's
  lent ranges ([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md)
  §10).
- **Recovery.** Whether a restarted chiplet, or MegaPad's own engine, could
  adopt the shared heap after a failure instead of losing it.
- **The bounds.** The values of §3's three times, which the chiplet module
  measures.
