# Managed data

Status: decided on 2026-11-20. Nothing is built. What managed data keeps
when LM1 fails is built in stages, and each part of it names its stage, or
marks itself as one of three fixes (§11). This is the specification of
managed data on MegaPad, and of the link's LM1 layer for it: how MegaPad
code holds, reads, writes, creates and releases objects that LM1's collector
manages, the windows in which it uses them, the handles it keeps past a
window, the requests that cross the queues, the errors, and what becomes of
the objects when LM1 fails. Release plan Part 9d sets four rules for this
code, and Part 8c places this layer of the link in MegaPad's docs
(fantasy-engine `dev/PLAN.md`). LM1's side, what its
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
| 2 | `FAILED` | LM1 has found corrupt data and stopped serving managed data (§11) |
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

That order holds because MegaPad's cores have no data cache and no store
buffer. A core given either must keep its accesses to lent memory uncached
and in program order, or a core could read `HOLD` before its `OPEN` lands
and use a window the collector does not see. The interconnect plan already
lists a production core's data cache among its open items
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §10).

### Opening a window

A core keeps its window's nesting depth in its own memory. An open at depth
0 is an **outermost open**:

1. Store `OPEN` in the window word.
2. Load `HOLD`. If `FAILED` is set, or the shared heap is lost (§11), store
   `CLOSED` and fail with `OBJ-E-FAILED`.
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
  `HELD` without `ACTIVE` while a window word reads `OPEN`, knows the delay
  is MegaPad's own, and waits on as well.
- A hold that stays `ACTIVE`, at one epoch, for more than twice
  `HOLD_LIMIT` is overdue. A core that finds it so fails the function, and
  the module stops LM1 (§11).
- A hold can also stall before `ACTIVE` (fix). The collector sets `ACTIVE`
  as soon as no window is open (LM1's `spec/05-memory-gc.md` §12.3), so
  when a hold stays `HELD` without `ACTIVE` while no window word reads
  `OPEN`, the delay is LM1's. A waiting core that finds the hold so, at one
  epoch, for more than twice `HOLD_LIMIT` with no window word reading `OPEN`
  in that time, treats the hold as overdue.
- A core whose store log is full (§7) waits for `LOG_HEAD` to move for at
  most the deadline MegaPad gives a request
  ([chiplet-link.md](chiplet-link.md) §8), inside a window or outside one
  (fix). Past that deadline it fails the function, and the module stops LM1
  (§11).

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
  token still names that owner and before the token can be reused. It
  calls it after that drain, because a task's handles stay lent until the
  task's reply (below).

Releasing a handle drops a root and frees nothing. The object goes once
nothing else uses it.

Every core shares the table's free list, the owner table and the owners'
lists, so the library guards them with one of KDOS's locks. It takes the
lock only for the few accesses a handle word makes, never across a wait,
so a core inside a window that waits for the lock waits only for another
core's short update.

### Stale handles

A handle is **stale** when its index lies beyond the table, its slot is
free, or its generation is not the slot's. Every word that takes a handle
checks it, and returns `OBJ-E-STALE` for a stale one without touching the
table. Once the shared heap is lost (§11), every handle is stale.

### Handles and tasks

Objects reach a task through handles. A request that starts a task names
its inputs by their handles, and names a handle made for nil whose slot
will receive the result. MegaPad lends these handles with the request until
its reply, or until LM1's failure ends the task (§13), as the sender of any
request lends the memory it names ([chiplet-link.md](chiplet-link.md) §8),
and neither re-points nor releases them meanwhile, a cancelled request's
included. LM1's runtime reads the input slots when the task starts, and
stores the result's reference in the result slot before it replies; this is
the one slot LM1 writes outside a hold (LM1's `spec/06-runtime.md` §3.5).
The rest of the protocol for tasks belongs to the link's layer for jobs,
which is still to specify.

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

A header's size field holds up to 65,535 fields, 512 KiB. LM1's object
model gives larger objects an extended header, subtype `11111`, whose next
word holds the size and the subtype, but it does not yet fix that word's
layout. Until it does, MegaPad makes no object larger than 65,535 fields,
and a constructor asked for one fails with `OBJ-E-RANGE` (§16).

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
for `LOG_HEAD` to move, for at most the deadline of §3. LM1 drains a log
without a hold, so this wait never depends on a window, and a core may wait
so even inside one; LM1 rings the core when it has drained a ring it found
full. Outside a window, once `LOG_HEAD` has reached `TAIL`, a core may move
to a larger ring. It writes the new ring's address and capacity into its
record, and only then stores a `TAIL` that counts entries in the new ring;
the count runs on from where it stood, so entry n lies at the new ring's
base plus 8 × (n mod its capacity). LM1 reads `TAIL` before it reads the
ring's address and capacity, so it finds the new ring whenever it finds
entries written there.

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
same description again returns the same ID, and MegaPad keeps each kind's
description and ID in the shape registry (§9). The description lies in
memory MegaPad lends for reading:

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
| Control block, core records, store logs, handle directory, shape descriptions, block list, shape registry | MegaPad | Reading |
| LM1's block | LM1 | Reading and writing |
| Handle table segments | Both: MegaPad inside windows, LM1 during holds and into a task's result slot (§4) | Reading and writing |
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
| `+0x30` | `STATE` | Which engine holds the shared heap (below); stage 0 |
| `+0x38` | `CAUSE` | Why LM1's service last ended (below), or 0; stage 0 |
| `+0x40` | `BLOCKS` | Address of the block list; stage 0 |
| `+0x48` | `SHAPES` | Address of the shape registry; stage 0 |

The control block is 128 bytes. `STATE` reads 0, `LM1`, while LM1 serves
the heap; 1, `STAND-IN`, while MegaPad's stand-in serves it in LM1's place
(§13); and 2, `LOST`, once the heap is lost (§11). MegaPad writes 0 before
`ATTACH`, and each later value once the change it names is complete, so a
restarted LM1 reads a settled state. `CAUSE` names the end of LM1's last
service: 1 LM1 set `FAILED`; 2 a check of LM1's words failed (below); 3 a
queue broke; 4 a hold was overdue; 5 a full log or a request outlived its
deadline; and 6 a reset for any other reason, another function's missed
deadline among them.

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
| `+0x40` | `MOVE` | The move word (below); stage 0 |
| `+0x48` | `MOVE_SET` | Address of the move set of the hold `MOVE` names, while `MOVE` reads `FLIPPING` or `FIXED`; stage 0 |
| `+0x50` | `SHAPE_RANGE` | [31:0] the first shape ID set aside for the kinds MegaPad registers; [63:32] the first of those that the stand-in gives (§10); stage 0 |
| `+0x80` | core entries | Core c's entry lies 64 × c bytes on |

Core c's entry:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `LOG_HEAD` | Entries LM1 has taken from the core's log |
| `+0x10` | `NEXT_BASE` | Base of the region LM1 gives the core next, or 0 |
| `+0x18` | `NEXT_LIMIT` | Its exclusive limit |
| `+0x20` | `NEXT_SERIAL` | Raised each time LM1 writes a region here |

A **forwarding pointer** is the word LM1 writes over a moved object's old
header to name its new address (LM1's `spec/01-object-model.md` §3.6).
**Flipping** is the part of a hold in which forwarding pointers exist. The
**move word**, `MOVE`, records how far a hold that moves objects has gone,
in the order LM1's `spec/05-memory-gc.md` §12.3 fixes (stage 0):

| Bits | Field | Meaning |
|------|-------|---------|
| 2:0 | `PHASE` | 0 `NONE`: no hold has moved objects since the function started; 1 `FLIPPING`: the hold has begun writing forwarding pointers; 2 `FIXED`: it has rewritten every reference to the objects it moved |
| 63:3 | `EPOCH` | The epoch of that hold: `HOLD`'s `EPOCH` while it ran |

LM1 writes the word as 0 at `ATTACH`. Phases 3 to 7 are undefined.

The **move set** lists a moving hold's source and destination ranges. LM1
writes it in the shared heap's space before the hold flips:

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `SOURCES` | s, the number of source ranges |
| `+0x08` | `DESTINATIONS` | d, the number of destination ranges |
| `+0x10` | ranges | s source ranges, then d destination ranges, each a base and an exclusive limit |

`SHAPE_RANGE` divides the shape IDs from its first ID up to 2^32 − 1. LM1
gives those below the stand-in's first in reply to `SHAPE`, and the
stand-in gives the rest (LM1's `spec/05-memory-gc.md` §12.6).

### The handle directory

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `FIRST` | Slots in segment 0, a power of two |
| `+0x08` | `SEGMENTS` | Segments in use |
| `+0x10` | segment bases | 64 words, segment k's base 8 × k bytes on |

Sixty-four entries cover every 32-bit index.

### The block list

The block list names every block of the shared heap, so that a restarted
LM1 can find the heap it takes back (stage 0):

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `COUNT` | Blocks in the list |
| `+0x08` | `CAPACITY` | Entries the list has room for |
| `+0x10` | blocks | Block k's base 16 × k bytes on, and its length in bytes 8 bytes after that |

MegaPad adds a block before it lends the block to LM1: the first block
before `ATTACH`, and each later one before its `GROW` reply. It removes a
block once it has taken the block back for a `RETURN`. When the list fills,
MegaPad writes a larger one and then stores its address in `BLOCKS`.

### The shape registry

A kind's description is lent only for its `SHAPE` request, so the registry
keeps each registered kind's description and shape ID for as long as the
function lasts (stage 0):

| Offset | Field | Contents |
|--------|-------|----------|
| `+0x00` | `COUNT` | Kinds in the registry |
| `+0x08` | `CAPACITY` | Entries the registry has room for |
| `+0x10` | kinds | Kind k's shape ID 16 × k bytes on, and 8 bytes after that the address of a copy of its description (§8), lent for reading |

MegaPad enters a kind once the reply to its `SHAPE` request gives the ID,
before any core uses the ID, and grows the registry as it grows the block
list. The registry holds the kinds MegaPad registers. How LM1 records the
kinds it puts into the shared heap itself is open (§16).

### What each side checks

Each chip checks what it reads from the other's words, as the link's queues
require:

- LM1 checks MegaPad's records. A window word holds one of its three values;
  `TAIL` never moves back and never runs more than `LOG_CAPACITY` ahead of
  `LOG_HEAD`; every log entry names a word inside the shared heap or the
  handle table; every `REGION` and `FILL` lies inside what LM1 granted; and
  every object a core made has a sound header, a size inside its region, a
  registered shape, and references only into the shared heap. At a resume
  `ATTACH` it also checks the block list and the shape registry (LM1's
  `spec/05-memory-gc.md` §12.11). A failure is reported in a `NOTICE`, and
  LM1 stops serving managed data (§11).
- MegaPad checks LM1's. `EPOCH` never moves back and moves on by one for
  each hold; `FAILED` stays set once set; `LOG_HEAD` never passes `TAIL`; a
  region LM1 writes lies inside a block MegaPad granted and overlaps no
  other core's; every reference MegaPad reads, from a handle slot or a
  field, passes §6's check; and, from stage 1, LM1's writes of `MOVE`
  follow their one progression: outside a hold `MOVE` reads `NONE`, or
  `FIXED` with an epoch below `EPOCH`, and its epoch never moves back. From
  stage 1, a failure sets the module's evidence latch, and the heap is lost
  (§11).

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
| `ATTACH` | `0x100` | `IN_ADDR`, `IN_LEN`: the control block; `ARG0`: 0 to start afresh, 1 to take back the heap the control block names (stage 2) | `OK` once LM1 has written its block, or taken the heap back; `BAD_RECORD` when a record fails LM1's checks; `NO_RESUME` when LM1 cannot take that heap back (stage 2) |
| `REGION` | `0x101` | `ARG0`: the bytes needed, 0 for LM1's choice | `OK` once `NEXT` holds a new region; `NO_MEMORY` |
| `SHAPE` | `0x102` | `IN_ADDR`, `IN_LEN`: a description (§8) | `OK` with the shape ID in `ARG0`; `BAD_RECORD` |
| `COLLECT` | `0x103` | `ARG0`: 0 the young objects, 1 the whole shared heap, 2 the whole heap, moving every object it can | `OK` once the collection has ended, with the shared heap's live bytes in `ARG0` and the epoch in `ARG1` |

From LM1, on the grant's pair:

| Operation | Number | Request | Reply |
|-----------|--------|---------|-------|
| `GROW` | `0x180` | `ARG0`: the bytes wanted, at least | `OK` with a new block's base in `ARG0` and its length in `ARG1`, lent for reading and writing; `NO_MEMORY` when MegaPad refuses |
| `RETURN` | `0x181` | `IN_ADDR`, `IN_LEN`: a block LM1 no longer uses | `OK` once MegaPad has taken it back |
| `NOTICE` | `0x182` | `ARG0`: 1 an overdue window, 2 corrupt data, 3 a broken log; `ARG1`: in bits 15:0 the core's ID, and for an overdue window in bits 63:16 the nanoseconds it has stayed open; `IN_ADDR`: the address concerned | `OK` |

The class's statuses are `0x100` `NO_MEMORY`, `0x101` `BAD_RECORD` and
`0x102` `NO_RESUME` (stage 2). `COLLECT` serves memory pressure and the
conformance tests of §15. The collection it asks for takes holds like any
other, so a core sends it outside a window.

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

### While the stand-in serves

While the stand-in serves the heap in LM1's place (§13), it answers each
core's requests itself, those a core sent LM1 before the failure and still
waits on included (stage 1):

- `REGION`: a region in a new block, written into the core's `NEXT` fields
  with `NEXT_SERIAL` raised, as LM1 writes one;
- `SHAPE`: the ID the registry already holds for the same description, or
  else a new ID from the upper part of `SHAPE_RANGE`, which it enters in the
  registry before it replies;
- `COLLECT`: `OK` at once, with every byte of the heap counted live and the
  epoch unchanged;
- a full log: it moves the core's `LOG_HEAD` to `TAIL` and rings the core.

## 11. Errors and failure

The words return these statuses, whose numbers are assigned when the module
is written:

| Status | Meaning |
|--------|---------|
| `OBJ-E-ABSENT` | No engine serves managed data |
| `OBJ-E-FAILED` | The shared heap is lost (below) |
| `OBJ-E-STALE` | The handle names no live slot |
| `OBJ-E-OWNER` | The owner token is not two positive cells |
| `OBJ-E-WINDOW` | A word that needs a window ran outside one, or one that needs none ran inside one |
| `OBJ-E-KIND` | A word is not of the kind expected: not a reference, not a fixnum, not of the shape |
| `OBJ-E-RANGE` | A field or byte lies outside its object, or a number does not fit a fixnum |
| `OBJ-E-CORRUPT` | A word breaks LM1's object model: a reference outside the shared heap, or a header that is not sound |
| `OBJ-E-NOMEM` | No memory for a region, a handle segment or a log |

### When LM1's service ends

The function **fails** when LM1 sets `FAILED`; when a hold is overdue or
stalls before `ACTIVE`, or a core's full log outlives its deadline (§3);
when one of the port's queues breaks or a request misses its deadline
([chiplet-link.md](chiplet-link.md) §8); when, from stage 1, the module
sets its evidence latch (below); or when the chiplet is reset for any other
reason, another function's missed deadline among them. The module then
decides from the state recorded at the failure what the shared heap keeps.
Where the records show the heap sound, it is **adopted**: MegaPad's
never-free engine, the **stand-in**, serves it as it lies (§13), and every
handle stays valid. Where they show it unsound, or cannot tell, it is
**lost**. Losing is safe at any moment, so it is the fallback in every state
the records cannot vouch for.

At a reset, LM1 loses whatever it keeps in its own memory: its card tables
and remembered sets, its map of free space, its marks, its interned shapes,
its private heap and the work in flight on it. Everything in MegaPad's
memory survives: the shared heap, the handle table and directory, the
records of §9, the store logs, and the library's own generations, owners
and lists. The decision and the stand-in therefore read only these. When
the whole machine loses power, MegaPad loses its memory as well, so what
must outlive the machine, or a lost heap, is saved as Desk saves any
document.

### Stages

Each part of recovery names the stage that builds it, and the stages are
built in order. First come three fixes, which the rules need whether the
heap is adopted or lost. In the last column, sections are those of LM1's
`spec/05-memory-gc.md`.

| Stage | What it adds | MegaPad's chiplet module | LM1's chiplet module |
|-------|--------------|--------------------------|----------------------|
| Fixes | Bounds on the waits LM1 can leave unended, and a lost heap's memory held until LM1's reset | The bound on a hold that stalls before `ACTIVE`, and the full log's deadline (§3); memory returned only after LM1's reset (below) | Setting `ACTIVE`, and draining a full log, within those bounds (§12.3, §12.4) |
| 0 | The records the rule reads, and the write order that keeps them true | `STATE`, `CAUSE`, the block list and the shape registry (§9) | `MOVE`, the move set, the write order and marks kept outside headers (§12.3); fixed shape IDs and the registry's range (§12.6) |
| 1 | The stand-in adopts the heap | The stop request and the wait for `BUSY`, the evidence latch and the rule (below); the stand-in (§10, §13); `OBJ-ENGINE` (§14) | Nothing new |
| 2 | A restarted LM1 takes the heap back | The handback (§13) | Taking the heap back at `ATTACH`, with the registry, old blocks counted full until the first mark, and the first hold (§12.11) |
| 3 | Recovery from a failure while a hold flips | The stand-in's resolving traversal, if MegaPad resolves (§16) | The resolving traversal (§12.11) |

The release builds the fixes and stages 0 and 1, and stage 2 joins it if
release plan Part 8d's collector lands early (Part 9d). Until stage 1 is
built, every failure loses the heap.

### Stopping LM1

When the function fails, the module stops LM1, unless LM1 is held in reset
already: it cancels and resets the port by the link's rule
([chiplet-link.md](chiplet-link.md) §8), and from stage 1 it first asks LM1
to stop, by setting `STOP_REQ` and waiting for `STOPPED` for at most a
request's deadline. From stage 1 it then waits for the port's `BUSY` bit to
clear before it reads the records, since an access already admitted runs to
its end ([chiplet-link.md](chiplet-link.md) §6). Each of LM1's writes lands
whole, so the records then show LM1's writes up to one point and none after
it (LM1's `spec/05-memory-gc.md` §12.3). From then until a resume `ATTACH`
(§13), LM1 writes nothing of the function's, whether it is held in reset or
has restarted, and the stand-in, where the rule adopts the heap, is the
only writer of LM1's block.

The module handles one failure at a time: a core that finds a hold overdue
while the module is already stopping LM1 waits for the decision. `FAILED`
and the evidence latch need no stop, since they lose the heap at once
(below), and LM1's private work goes on after `FAILED`.

### The rule

The rule decides between adopting and losing the heap (stage 1). Beside
the hold word, it reads the **evidence latch**, a record in MegaPad's own
memory of evidence that LM1 wrote wrong words: a failed check of LM1's
words (§9), an `OBJ>REF` that refuses a word with a reference tag, or a
broken queue. It also reads `MOVE`, `LOG_HEAD` and the `NEXT` fields in
LM1's block; `WINDOW`, `REGION`, `FILL` and `TAIL` in the core records; the
block list; and the epoch each core last saw, which the module keeps in its
own memory. It applies top down, and the first row that matches decides:

| # | Recorded state | Decision |
|---|----------------|----------|
| 0 | LM1 could still write: it is not held in reset, or `BUSY` is set | Decide nothing yet |
| 1 | `FAILED` is set, or the evidence latch is | Lose |
| 2 | The records contradict each other: `ACTIVE` without `HELD`; `EPOCH` below some core's last-seen epoch; `MOVE` naming a later epoch than `HOLD`, or still `FLIPPING` for a hold that has ended; an undefined phase; `LOG_HEAD` above `TAIL`; a `NEXT` region outside the blocks | Lose |
| 3 | `HELD` clear | Adopt |
| 4 | `HELD` without `ACTIVE` | Adopt, and end the hold |
| 5 | `ACTIVE`, and `MOVE` reads `NONE` or names an earlier epoch | Adopt, and end the hold |
| 6 | `ACTIVE`, and `MOVE` reads `FIXED` for this epoch | Adopt, and end the hold |
| 7 | `ACTIVE`, and `MOVE` reads `FLIPPING` for this epoch | Lose until stage 3; from stage 3, adopt after the resolving traversal (below), and lose if it fails |
| 8 | A later walk of the adopted heap fails a header or reference check | Lose at that point |

Row 1 applies at once, whether or not LM1 can still write, since losing
needs nothing from LM1: a core that finds `FAILED` loses the heap as it
opens a window (§3), and the module loses it as it sets the latch. Each
adopt row rests on one rule of LM1's write order (LM1's
`spec/05-memory-gc.md` §12.3):

- row 3: every hold ends with no forwarding pointer reachable;
- row 4: nothing moves before `ACTIVE`;
- row 5: a hold writes `MOVE` before its first forwarding pointer, so this
  hold has moved nothing;
- row 6: `FIXED` follows the last rewrite, so every reference already names
  its object's new place;
- row 7: a forwarding pointer follows its copy, whole.

Adoption is idempotent: an adoption cut short runs again from the records
as they then stand, and the rule adopts them again.

The **resolving traversal** (stage 3) walks the live heap from the handles
while every core is held out. It follows each forwarding pointer it meets
once and rewrites the reference to the address the pointer names. It loses
the heap when it meets a forwarding pointer outside the move set's source
ranges, or one that names anything but a sound object inside a destination
range. LM1 makes it as it takes the heap back (LM1's `spec/05-memory-gc.md`
§12.11).

### Losing the heap

From the loss on, every word returns `OBJ-E-FAILED`, and `OBJ-WITHIN`
throws it; every handle is stale; and releasing an owner succeeds and does
nothing. The module stores `STATE` as `LOST` with the failure's `CAUSE`,
rings every core whose window word reads `WAITING`, and wakes every core
that waits on a reply or a full log, so that each wait ends in
`OBJ-E-FAILED`. No word follows a reference from a lost heap again, and
its memory stays out of use until every window has closed, so a loss,
before an adoption or after one, leaves no dangling or forged reference.

The module returns the function's memory to KDOS once no core has a window
open and LM1 has been reset, since until the reset LM1 may still hold
references into it (fix). A restarted chiplet offers the function again,
and the module starts it afresh.

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
anything: every rule still holds, and only memory use differs. The release
builds that engine and no collector for MegaPad (decided 2026-11-20), since
every configuration of the release has LM1.

A task's own objects follow decision 32 on either engine. They live in a
heap that lasts as long as the task, its result is copied out, and a store
that would let any other of them escape is refused (fantasy-engine
`dev/ideas/language.md`).

### The stand-in

The never-free engine is also the stand-in of §11: when the rule adopts the
shared heap after LM1 fails, the engine serves that heap in LM1's place
(stage 1). It uses the same words and records as LM1, so callers see one
change: `OBJ-ENGINE` (§14) reports that collection has paused. Once the
module has stopped LM1 (§11), the stand-in:

1. **Trusts none of LM1's last decisions.** It replaces every core's `NEXT`
   grant with a region in a new block, which it adds to the block list
   first, and never grants free space in the blocks LM1 managed; a core
   keeps the region it already holds until it next takes one. It takes
   each new block from KDOS's allocator for lendable memory, and keeps the
   blocks few, as LM1 does with `GROW` (§10), since a restarted LM1
   installs each block as a range (LM1's `spec/05-memory-gc.md` §12.1).
2. **Ends an interrupted hold.** Where the rule says so, it stores `HOLD`
   with `HELD` and `ACTIVE` clear and `EPOCH` one higher, and rings every
   core whose window word reads `WAITING`. It does so after step 1, so a
   core that the hold's end wakes finds the stand-in's region waiting.
   Windows open at the failure run to their close as usual.
3. **Drops the logs.** It moves every core's `LOG_HEAD` to its `TAIL`, and
   rings a core that waits on a full log. A heap that is never collected
   needs no log entries, and a restarted LM1 needs none either (LM1's
   `spec/05-memory-gc.md` §12.11).
4. **Never moves or frees anything,** so every handle, and every reference
   held in an open window, stays valid.
5. **Answers every request itself,** as §10 lists.
6. **Ends the tasks in flight.** Each fails; inside a window, the stand-in
   sets its result slot to nil; and the lend of its handles ends, so their
   owners may release them.
7. **Records its state.** It stores `STATE` as `STAND-IN`, with the
   failure's `CAUSE`.

If LM1 never takes the heap back, memory grows until KDOS refuses a block,
and constructors then fail with `OBJ-E-NOMEM`. Reads, stores and saves
still work, so the user can save and restart Desk (§16). Until stage 2 is
built, MegaPad refuses a restarted LM1's offer of the function while the
stand-in serves.

### Handing the heap back

A restarted LM1 takes the heap back from the stand-in (stage 2):

1. LM1's runtime offers the function again. MegaPad grants it with new
   queue pairs and no working memory, since the heap already exists, and
   each core makes a new pair before its first request to the restarted
   LM1 (§10).
2. The stand-in finishes the requests it has begun and takes no new ones.
   Requests sent from then on wait.
3. MegaPad sends `ATTACH` with the resume flag, naming the existing control
   block. LM1 touches the heap and its records only once that `ATTACH`
   names them.
4. On `OK`, MegaPad stores `STATE` as `LM1`, and LM1 serves from then on.
   On `NO_RESUME`, the stand-in serves on. On `BAD_RECORD`, the heap is
   lost (§11). The waiting requests go to whichever engine then serves.
5. If LM1's service ends during the handback, the module stops LM1 and
   applies the rule again, so the stand-in may adopt the heap again.

LM1's side of the handback is LM1's `spec/05-memory-gc.md` §12.11.

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
| `OBJ-ENGINE` | `( -- engine: u cause: u )` | Either | Which engine serves: 0 none, 1 LM1, 2 MegaPad's never-free engine; and the `CAUSE` (§9) of the failure that put MegaPad's engine in LM1's place or lost the heap, else 0 (stage 1) |
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
module provides test hooks beside the link: one keeps a hold `ACTIVE` for a
set time; one writes a chosen word as LM1 would, into the shared heap, LM1's
block or a queue LM1 writes; and, for the recovery tests, one stops LM1 at a
chosen point of its work.

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
    address. From stage 1 the refusal also sets the evidence latch, and the
    heap is lost (§11).
23. **A reset outside a hold.** Sets up handles held by two owners, with no
    window open. Does: resets the chiplet. Checks, until stage 1:
    `OBJ-OPEN` and `OBJ-GET` return `OBJ-E-FAILED`; releasing each owner
    returns 0; the module returns the function's memory to KDOS; MegaPad
    memory outside the function's is untouched. From stage 1 the reset
    keeps the heap, and A1 below checks that, with L1 checking a loss.
24. **An overdue hold.** Sets up a hold that the test hook keeps `ACTIVE`
    for more than twice `HOLD_LIMIT`. Does: core 0 calls `OBJ-OPEN`.
    Checks: core 0 finds the hold overdue, and the port is cancelled and
    reset by the link's rule, from stage 1 after a stop request. Until stage
    1, `OBJ-OPEN` returns `OBJ-E-FAILED`. From stage 1 the rule decides by
    `MOVE`: a hold that has moved nothing is adopted, as A3 checks, and one
    cut while it flips is lost, as L5 checks.
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

### Recovery

These tests run LM1's failures through MegaPad's chiplet module and LM1's
chiplet module together, on the simulator. Each names the stage it comes
with, and runs from that stage on. A branch's letter says which way the
rule of §11 decides: A for adopt, L for lose.

- **The killed hold** (stage 1; stage 3 for its flipping cuts). Sets up a
  hold on the simulator, and a hook on the module's `mem` callback
  (`shared/accel/chiplet_module.h`) that records every word LM1 writes,
  each word of a burst included. Does: for each cut between two recorded
  words, builds the hold's starting memory plus LM1's writes up to the cut,
  and has MegaPad's module apply the rule to it. Checks: the rule adopts in
  exactly the states its stage qualifies and loses in the rest; after an
  adoption, each handle gives its object's contents from before the hold;
  an independent walker in the host harness finds every reachable reference
  naming a sound object, no forwarding pointer reachable and no object at
  two addresses; and once a cut shows the hold's moves applied, every later
  cut does too. A hold that only drains logs and retires regions adopts at
  every cut. A moving hold loses only at its flipping cuts in stage 1, and
  adopts at every cut from stage 3.

Adopt branches:

- **A1, a reset outside a hold** (stage 1). Sets up windows open on cores 0
  and 1, logs LM1 has left undrained, and core 1 waiting inside its window
  on a full log. Does: resets LM1. Checks: both windows run to their close;
  every handle gives its object; the stand-in grants regions in a new block
  on the block list; every core's `LOG_HEAD` reaches its `TAIL`; core 1 is
  rung and goes on; `OBJ-ENGINE` reports MegaPad's engine, with a reset as
  the cause.
- **A2, waits that LM1 leaves unended** (fix and stage 1). Sets up a hook
  that stops LM1 just after it stores `HELD`, with no window open. Does:
  core 0 calls `OBJ-OPEN`. Checks: once twice `HOLD_LIMIT` has passed, core
  0 finds the hold overdue; the stand-in adopts the heap; `EPOCH` moves on
  by one; every core whose window word reads `WAITING` is rung; `OBJ-OPEN`
  returns 0. A second run stops LM1 from draining while core 0 waits inside
  a window on its full log: once the deadline of §3 passes, the stand-in
  adopts the heap, core 0's `LOG_HEAD` reaches its `TAIL`, and core 0 goes
  on.
- **A3 and A4, an `ACTIVE` hold whose `MOVE` reads `NONE` or `FIXED`**
  (stage 1). The killed hold covers them.
- **A5, a task in flight** (stage 1). Sets up a task running on LM1, with
  input handles and a result handle made for nil. Does: resets LM1. Checks:
  the task fails; its result slot holds nil; its input handles leave the
  lend, and their owner can release them.
- **A6, the handback** (stage 2). Sets up the stand-in serving after A1,
  with a shape it registered and garbage made while it served. Does: LM1
  restarts and takes the heap back. Checks: the resume `ATTACH` returns
  `OK`; every handle gives its object; the shape the stand-in registered
  keeps its ID; the garbage is freed once LM1's first full mark ends; a
  later `COLLECT` 2 moves objects, and the handles follow.
- **A7, a failure during the handback** (stage 2). Sets up A6's resume,
  with a hook that stops LM1 inside its first hold. Checks: the module stops
  LM1, the rule adopts the heap again, and every handle stays valid.

Lose branches:

- **L1, `FAILED`** (fix). Sets up test 21's forged reference, so that LM1
  sets `FAILED`. Checks: every handle is stale; the module returns the
  function's memory to KDOS only after LM1's reset.
- **L2, the evidence latch** (stage 1). Sets up a hook that moves `EPOCH`
  backwards. Does: core 0 opens a window, then the test resets LM1. Checks:
  core 0's open sets the latch and loses the heap; after the reset, the
  heap stays lost and its memory returns to KDOS.
- **L3, a broken queue** (stage 1). Sets up a hook that moves `TAIL`
  backwards on a queue LM1 writes. Checks: MegaPad finds the queue broken
  and resets the port, and the heap is lost.
- **L4, records that contradict each other** (stage 1). Sets up, one per
  run, `ACTIVE` without `HELD`, a `MOVE` epoch ahead of `HOLD`'s, and an
  undefined phase, each written by a hook before a reset. Checks: each run
  loses the heap before the module writes anything.
- **L5, a hold cut while it flips, before stage 3** (stage 1). The killed
  hold covers it.
- **L6, a bad forwarding pointer** (stage 3). Sets up a hook that writes a
  forwarding pointer outside the move set during a flipping hold, then a
  reset. Checks: the resolving traversal meets it and loses the heap.
- **L7, a refused resume** (stage 2). Sets up A6's resume with LM1 replying
  `NO_RESUME`, and in a second run `BAD_RECORD`. Checks: after `NO_RESUME`
  the stand-in serves on and every handle stays valid; after `BAD_RECORD`
  the heap is lost.
- **L8, corruption found late** (stage 2). Sets up a hook that damages a
  header which LM1's first full mark reaches after the handback. Checks:
  LM1 sets `FAILED` when its mark reaches the header, and the heap is lost
  then.

## 16. Still open

- **The class number**, assigned in chiplet-link.md §8.
- **Tasks.** The requests that start a task and return its result, of
  which §4 fixes only how they name objects, belong to the link's layer for
  jobs, which is still to specify. That layer also says how a task that
  LM1's failure ends is reported (§13).
- **Lendable memory.** How KDOS's allocator for lendable memory places the
  shared heap's blocks, the handle table and the records within the port's
  lent ranges ([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md)
  §10).
- **Kinds that LM1 puts into the shared heap** (stage 2). The shape
  registry lies in memory lent for reading, so MegaPad alone writes it, and
  it holds the kinds MegaPad registers. LM1 also puts objects of kinds of
  its own into the shared heap: records of kinds it interns itself, objects
  of a task's result, and, through Lisp's stores, closures, symbols and
  foreign pointers. A restarted LM1 needs those kinds' IDs to read such
  objects, and it has to refuse a heap that holds an object it cannot adopt
  before it resumes, since its first full mark comes after. One option has
  LM1 ask MegaPad, with a new request on the grant's pair, to enter each
  such kind in the registry before the first object of the kind reaches the
  shared heap, the entry marking a kind a restarted LM1 cannot adopt.
  Another has LM1 keep a second part of the registry, in memory lent to it
  for writing and named from its block. The cheapest has LM1 set one word
  in its block before it first puts into the shared heap an object that a
  restarted LM1 could not adopt, a record of an unregistered kind included,
  and has a restarted LM1 reply `NO_RESUME` while that word is set. Word
  values that carry LM1's compiled entry need a rule for re-binding under
  each option, and wait on how Uroko represents them.
- **The stand-in's heap before stage 2** (stage 1). Until stage 2, the
  stand-in serves an adopted heap for as long as MegaPad runs, and the heap
  only grows. Once no handle is live and no window is open, nothing can
  reach the heap; the module could then lose it, return its memory and
  start the function afresh on a restarted LM1, so that restarting Desk,
  which releases every owner's handles, brings collection back.
- **Stage 3's order of work.** LM1 makes the resolving traversal as it
  takes the heap back, so the cores stay held out from the failure through
  LM1's restart and the traversal. How the module keeps them there, and
  what `STATE` reads meanwhile, is designed with stage 3. The stand-in
  could make the traversal itself on a MegaPad core, which needs no restart
  of LM1, at perhaps 5 to 15 times LM1's time per object, by estimate.
- **Large objects.** The layout of LM1's extended header, which objects of
  more than 65,535 fields need (§5), so that a long string or vector can be
  made at production size. Release plan Part 8b's revision of LM1's object
  model defines it (decided 2026-11-20).
- **The bounds.** The values of §3's three times, and of the deadline
  MegaPad gives a request, which also bounds a full log's wait and a stop
  request (§3, §11). The chiplet module measures them.
- **A data cache or store buffer on a production core.** §3's protocol needs
  each core's accesses to lent memory to reach memory uncached and in
  program order.
