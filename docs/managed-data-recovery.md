# Managed data after an LM1 failure

Status: decided on 2026-11-20, in stages. Nothing is built. This is the
design of what managed data keeps when LM1 fails. It stays here until
[managed-data.md](managed-data.md) and LM1's `spec/05-memory-gc.md` §12
absorb it; section 10 lists where each part goes. Release plan Part 9d
records the decision (fantasy-engine `dev/PLAN.md`).

## 1. What is decided

The state recorded at the failure decides what is kept, as the rest of the
system decides its recovery. Where the records show the shared heap sound,
it is **adopted**: something keeps serving it as it lies, and every handle
stays valid. Where they show it unsound, or cannot tell, it is **lost**, as
[managed-data.md](managed-data.md) §11 describes: every handle goes stale,
the objects are gone, and saved documents remain. Losing is safe at any
moment, so it is the fallback in every state the records cannot vouch for.

The work comes in stages:

| Stage | What it adds | When |
|---|---|---|
| Fixes | Three gaps closed (section 4), needed whether the heap is adopted or lost | Release |
| 0 | The records the rule reads, and the write order that keeps them truthful | Release |
| 1 | MegaPad's never-free engine adopts the heap at once | Release |
| 2 | A restarted LM1 takes the heap back | Release if Part 8d's collector lands early; otherwise first after it |
| 3 | Recovery from a failure while a hold is moving objects | Later |

Until stage 3, the rule loses the heap in that one state.

## 2. Terms

- A **window** is a stretch in which one MegaPad core uses references
  ([managed-data.md](managed-data.md) §3).
- A **hold** is a stretch in which LM1 keeps every window shut so that it
  may move or free objects. `HELD` means a hold has begun, `ACTIVE` means
  every window has closed and LM1 is at work, and `EPOCH` counts the holds
  that have ended.
- To **adopt** the heap is to keep serving it as it lies, with every handle
  valid. To **lose** it is the fallback above.
- The **stand-in** is MegaPad's never-free engine
  ([managed-data.md](managed-data.md) §13) serving the heap in LM1's place.
- A **forwarding pointer** is the word LM1 writes over a moved object's old
  header, naming its new address. **Flipping** is the part of a hold in
  which forwarding pointers exist.
- The **MOVE word**, new in LM1's block, records whether the current hold
  has begun flipping.

## 3. What survives a failure

| Item | Survives | What recovery does with it |
|---|---|---|
| Shared heap objects, in blocks of MegaPad's memory | Yes | Sound outside a hold; inside one, see section 4 |
| Handle table segments and directory | Yes | Outside a hold, every slot names its object's current address |
| The library's slot generations, owners and lists, in MegaPad's own memory | Yes | Unchanged, so handles stay valid when the heap is adopted |
| Store logs and each core's `TAIL` | Yes | Entries past `LOG_HEAD` name stores that landed; neither adopter needs them (section 6) |
| `LOG_HEAD`, in LM1's block | Yes | The adopter sets it to `TAIL` |
| Core records: `WINDOW`, `REGION`, `FILL` | Yes | Base to `FILL` holds whole objects; windows open at the failure finish normally |
| `NEXT_BASE`, `NEXT_LIMIT`, `NEXT_SERIAL` | Yes | LM1's last grants; the adopter replaces them with regions in new memory |
| Hold word, time bounds, built-in shape IDs | Yes | The hold word is the main record the rule reads |
| Record-kind descriptions | No lasting copy, since each is lent only for its `SHAPE` request | Stage 0 adds a registry in lent memory |
| List of the heap's blocks | Only in MegaPad's own memory, which LM1 cannot read | Stage 0 adds a copy in lent memory |
| Queue pairs and requests in flight | The entries survive; the work does not | Requests in flight fail, and the restart gets new queue pairs |
| LM1's card tables and remembered sets | No | Not rebuilt: every surviving object counts as old (section 6) |
| LM1's map of free space in the shared heap | No | Rebuilt by the first full mark; until then, regions come from new blocks |
| LM1's mark state | No, or stale if kept in header bits | Stage 0 keeps marks out of headers |
| LM1's interned shapes | No | Re-interned from the registry under their recorded IDs |
| LM1's private heap and its references into the shared heap | No | Shared objects only LM1 held become garbage, which is correct |
| Tasks running on LM1 | No | They fail; their result slots return to nil, and the lend of their handles ends |
| Forwarding pointers | Yes | Reachable only if the failure hit a flipping phase |

A loss of power to the whole machine loses MegaPad's memory too. Recovery
does not make managed data persistent.

## 4. The kinds of failure

| Case | What the records show | Is the heap sound? |
|---|---|---|
| LM1 traps or hangs outside a hold | `HELD` clear | Yes. LM1 moves and frees objects only in holds, and each hold ends with no forwarding pointer reachable. Between holds it only marks, drains logs, answers requests, sweeps space a hold already found dead, and copies a task's result before storing its slot. |
| LM1 stops after storing `HELD`, before `ACTIVE` | `HELD` without `ACTIVE` | Yes, since nothing moves before `ACTIVE`; but nothing ends this hold (gap 1) |
| MegaPad resets LM1 after an overdue hold | `HELD` and `ACTIVE` | Yes if the hold had not begun moving objects; otherwise some references name old locations whose header is now a forwarding pointer. The `MOVE` word tells the two apart. |
| MegaPad resets LM1 after a broken queue | Anything | A broken queue shows LM1 wrote wrong words, so nothing vouches for the heap: lost |
| LM1 is reset, or loses its own power, during a hold | `HELD` and `ACTIVE`, with LM1's writes cut off | No word tears: each write lands whole at MegaPad's memory controller, an access in flight at a reset still finishes ([chiplet-link.md](chiplet-link.md) §6), and a burst stops between words (§4). Soundness therefore depends only on the order of LM1's writes (section 7). |
| Failure outside a hold, with log entries not replayed | `HELD` clear, `LOG_HEAD` below `TAIL` | Yes. Unreplayed entries only feed LM1's card tables, which are lost anyway and need no rebuild. |
| LM1 has set `FAILED` | `FAILED` | LM1 found corrupt data, so nothing vouches for the heap: lost |

Three gaps need closing whether the heap is adopted or lost:

1. **A stuck hold.** Only an `ACTIVE` hold times out. If LM1 dies after
   storing `HELD` and before setting `ACTIVE`, every core waits in
   `OBJ-OPEN` for ever, Desk's included. A waiting core that finds `HELD`
   without `ACTIVE` at one epoch for more than twice `HOLD_LIMIT` treats the
   hold as overdue, provided no core's window word reads `OPEN`, which shows
   that the delay is LM1's.
2. **A full store log.** A core whose log is full waits for `LOG_HEAD` with
   no deadline, possibly inside a window. It gets the deadline requests
   have.
3. **Memory returned too early.** After `FAILED`, LM1 keeps running its
   private work and may still hold references into the heap. The heap's
   memory therefore stays out of use until LM1 has been reset, and only then
   returns to KDOS ([managed-data.md](managed-data.md) §11).

Recovery also changes what other functions' failures cost. Today any reset
of the port loses Desk's managed data, including a reset that a sandbox job
or tile work caused by missing its deadline ("cancel, then reset",
[chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §5 and §7).
With adoption, such a reset keeps it.

## 5. MegaPad's engine taking over

The stand-in keeps every handle valid without collecting and hands the heap
back when LM1 restarts. It uses the same words and records as LM1
([managed-data.md](managed-data.md) §13), so callers see no change except a
report, from `OBJ-ENGINE`, that collection is paused. The takeover is sound
because:

1. **LM1 writes nothing more.** MegaPad asks LM1 to stop, cancels and resets
   it, and waits for the port's `BUSY` bit to clear before it reads the
   records. While LM1 is held in reset, the stand-in is the only writer of
   LM1's block.
2. **The rule allows it** (section 8).
3. **It trusts none of LM1's last decisions.** It replaces every core's
   `NEXT` grant with a region in a new block, which it adds to the block
   list, and never uses free space in the old blocks.
4. **It ends an interrupted hold.** It raises `EPOCH` by one, clears `HELD`
   and `ACTIVE`, and rings every core whose window word reads `WAITING`.
5. **It never moves or frees anything,** so every handle, and every
   reference held in an open window, stays valid.
6. **It answers every request itself.** `REGION` is served from new blocks,
   `SHAPE` goes into the registry under IDs set aside for the stand-in,
   `COLLECT` reports every byte live, and a full log is handled by moving
   `LOG_HEAD` to `TAIL`.
7. **It ends the tasks in flight.** It fails them, sets their result slots
   to nil, and ends the lend of their handles.
8. **Handback.** The stand-in stops serving, and MegaPad sends LM1 the
   resume `ATTACH`. A restarted LM1 touches the heap only after that
   `ATTACH` names it.
9. **If LM1 never returns,** memory grows until KDOS refuses a block, and
   constructors then fail with `OBJ-E-NOMEM`. Reads, stores and saves still
   work, so the user can save and restart Desk.

This degraded mode follows places' recovery: everything works except one
capability, and the system says which one.

## 6. A restarted LM1 taking the heap back

A restarted LM1 needs no full mark before it resumes. It counts every object
that exists at its restart as old. With no young objects there is no
old-to-young reference to record, so empty card tables and remembered sets
are already correct, and every later store is logged and barriered as
usual. Free space is the only thing it rebuilds, by an ordinary full mark in
the background; until that mark ends, it treats the old blocks as full and
makes new regions from new blocks it gets with `GROW`.

1. MegaPad resets LM1, reloads its image, grants `MANAGED` again with new
   queue pairs, and sends `ATTACH` with a resume flag naming the existing
   control block.
2. LM1 checks MegaPad's records as at any `ATTACH`, and also the block list
   and the shape registry. If any check fails, it refuses to resume.
3. LM1 installs the shared heap's ranges from the block list and re-interns
   each recorded shape under its recorded ID.
4. LM1 takes its first hold. In it, it sets each core's `LOG_HEAD` to
   `TAIL`, retires every region, grants each core a region in a new block,
   and ends the hold one epoch past the recorded `EPOCH`.
5. MegaPad has already failed the tasks that were in flight.
6. LM1 runs one full mark from the handle table and checks every object it
   walks (LM1 `spec/05-memory-gc.md` §12.5 and §12.8). If a check fails, it
   sets `FAILED` and the heap is lost at that point. When the mark ends,
   free space is whatever it did not mark.

Steps 2 to 4 take about a thousand link accesses and one `GROW`, under a
millisecond at MegaPad's 100 MHz clock. Step 6 is bounded by the link, since
LM1 keeps one request outstanding: an object with a header and five tagged
fields costs 50 to 180 MegaPad clocks to read
([chiplet-interconnect-plan.md](chiplet-interconnect-plan.md) §3 and §8).
With a handle table about the size of the object count:

| Live objects | Full mark at 100 MHz |
|---|---|
| 10,000 | 6–21 ms |
| 100,000 | 60–210 ms |
| 1,000,000 | 0.6–2.1 s |
| 10,000,000 | 6–21 s |

MegaPad never waits on this mark; garbage in the old blocks stays until it
ends. If LM1 has no memory of its own (release plan D8), its mark bitmap lies
in lent memory, which roughly doubles these times.

A restarted LM1 adopts only objects whose meaning lies wholly inside the
shared heap: cons cells, vectors, bytevectors, strings, records of registered
shapes, and boxed numbers. Closures, symbols, foreign pointers and word
values that carry LM1's compiled entry (decision 32) depend on LM1's private
state and need a rule for re-binding them; until one exists, LM1 refuses to
resume a heap that holds one. The stand-in can still serve such a heap,
since it never interprets objects.

## 7. Holds that are safe to fail in

No word tears, and LM1's requests to MegaPad's memory complete one at a
time, in order (LM1 `spec/05-memory-gc.md` §6.4), so a design only has to fix
what LM1 writes and in what order. An in-place sliding compactor cannot be
made safe this way, since a cut can leave references naming the wrong
object.

Moving holds **copy first, then flip**:

- LM1 writes each object's copy in full before it writes the forwarding
  pointer at the old location, which it sends as a request of its own. It
  rewrites a reference to the new address only after that, and it copies
  only into space that was free when the hold began. The forwarding pointer
  is then the one word that selects the new copy.
- The `MOVE` word records the hold's progress with the hold's epoch. It
  changes from `NONE` to `FLIPPING` before the first forwarding pointer and
  to `FIXED` after the last rewrite, and it names a small record of the
  hold's source and destination ranges.
- This costs two or three word writes per moving hold, in the order a
  copying collector uses anyway.
- `NONE` and `FIXED` can be adopted as they lie. `FLIPPING` can be adopted
  after a **resolving traversal** (stage 3): a walk from the handles that
  follows each forwarding pointer once and rewrites the reference. The heap
  is lost if the walk meets a forwarding pointer outside the recorded source
  ranges, or one naming anything but a sound object in a destination range.
  The walk covers the live heap with every core held out: at the rates of
  section 6 when LM1 does it after its reboot, and perhaps 5 to 15 times
  slower per object when the stand-in does it on a MegaPad core, which needs
  no reboot.

Two other designs stay open for later need:

- **A journal of moves.** Before a hold moves anything, LM1 writes every
  planned move and rewrite into lent memory and then a commit word with a
  count and a checksum. An uncommitted journal is discarded, and a committed
  one is checked in full and redone. It costs 20 to 40% more link time per
  moving hold, and bounds recovery to one hold's work. It is worth that cost
  only if Desk's heaps grow so large that a pause proportional to the heap
  is unacceptable.
- **Never moving shared objects.** Mark in place, sweep, and compact rarely.
  It needs less link traffic per collection than copying, at the cost of
  fragmentation, more `REGION` requests and a more complex allocator. That
  is a choice about the whole collector, for the D8 sizing study and Part
  8d, not one recovery alone justifies.

## 8. The records and the adoption rule

The rule reads these records:

| Record | Where it lies | Writer | New |
|---|---|---|---|
| `HOLD`: `HELD`, `ACTIVE`, `FAILED`, `EPOCH` | LM1's block, `+0x00` | LM1 | No |
| `MOVE`: phase (`NONE`, `FLIPPING`, `FIXED`) and its hold's epoch, in one word | LM1's block, `+0x40` | LM1 | Stage 0 |
| Move set: the hold's source and destination ranges | Lent memory, named from `+0x48` | LM1 | Stage 0 |
| `WINDOW`, `REGION`, `FILL`, `TAIL`; `LOG_HEAD` | Core records; LM1's block | MegaPad; LM1 | No |
| `STATE` (`LM1`, `STAND-IN`, `LOST`) and `CAUSE` | Unused words of the control block | MegaPad | Stage 0 |
| Block list | Lent for reading | MegaPad | Stage 0 |
| Shape registry: each kind's ID and description | Lent for reading | MegaPad, and LM1 for kinds it introduces | Stage 0 |
| Evidence latch: a failed check of LM1's words ([managed-data.md](managed-data.md) §9), a failed `OBJ>REF`, a broken queue | MegaPad's own memory | MegaPad | Stage 1 |
| Header and reference checks during any walk | The heap itself | — | No |

The rule applies top down, and the first row that matches decides:

| # | Recorded state | Decision |
|---|---|---|
| 0 | LM1 could still write: it is not in reset, or `BUSY` is set | Decide nothing yet |
| 1 | `FAILED` is set, or the evidence latch is set | Lose |
| 2 | The records contradict each other: `ACTIVE` without `HELD`; `EPOCH` below some core's last-seen epoch; `MOVE` naming a later epoch than `HOLD`, or still `FLIPPING` for a hold that ended; an undefined phase; `LOG_HEAD` above `TAIL`; a `NEXT` region outside the blocks | Lose |
| 3 | `HELD` clear | Adopt |
| 4 | `HELD` without `ACTIVE` | Adopt, and end the hold |
| 5 | `ACTIVE`, and `MOVE` names an earlier epoch or reads `NONE` | Adopt, and end the hold |
| 6 | `ACTIVE`, and `MOVE` reads `FIXED` for this epoch | Adopt, and end the hold |
| 7 | `ACTIVE`, and `MOVE` reads `FLIPPING` for this epoch | Lose until stage 3; from stage 3, adopt after the resolving traversal, and lose if any check fails |
| 8 | A later walk of the adopted heap fails a header or reference check | Lose at that point |

Each adopt row rests on one fact: row 3, that every hold ends with no
forwarding pointer reachable; row 4, that nothing moves before `ACTIVE`; row
5, that this hold moved nothing; row 6, that every reference was already
rewritten; row 7, that a forwarding pointer exists only after its copy is
complete. MegaPad checks after every hold that `MOVE` followed its one
allowed progression, and sets the latch if it did not.

Losing the heap is safe before an adoption or after one. Every handle goes
stale, the memory stays out of use until no window is open and LM1 has been
reset, and MegaPad never follows a reference from the lost heap again, so
the fallback cannot leave a dangling or forged reference. Adoption is
idempotent, so an adoption cut short runs again from the same records.

The rule follows the system's other recovery paths:

- **Places** (Akashic `docs/utils/fs/places.md`): the registry records the
  state and composition applies the policy, and Desk narrows what it does,
  making places read-only and saying why, rather than falling back silently.
  Here the records hold the state and MegaPad's module applies the rule; the
  stand-in narrows managed data by pausing collection, and `OBJ-ENGINE`
  says so.
- **Generation pairs** (Akashic `docs/utils/generation-pair.md`): selection
  classifies slot states from a table, reasoning from the order in which
  saves fill slots, revokes old authority before it classifies, and allows
  one progression of states. Here the rule is a table reasoning from the
  order of LM1's writes, LM1's authority is revoked (the reset, the replaced
  `NEXT` grants) before it classifies, and `MOVE` has one progression per
  epoch.
- **The ext4 driver** (Akashic `docs/utils/fs/drivers/vfs-ext4.md` and
  `drivers/ext4/vfs-ext4-orphan.md`): it validates in a pass that writes
  nothing, refuses damage rather than reading it as a torn write, resolves
  crashes forward and retries idempotently, and accepts only the states its
  recovery is qualified for. Here each stage adopts only the states its
  tests qualify.
- **The user volume's killed-run test** (Akashic
  `local_testing/test_user_volume_killed.py`): it cuts a run at every flush
  and requires the state from before or after the operation. The killed
  hold of section 9 does the same for a hold.

## 9. Tests

**The killed hold** drives every `ACTIVE` row. A hold runs on the simulator
with LM1's chiplet module, and a hook on the module's `mem` callback
(`shared/accel/chiplet_module.h`) records every write LM1 makes. Each cut's
memory is the hold's starting memory plus LM1's writes up to the cut, and at
each cut MegaPad's module applies the rule. The test checks that the rule
adopts exactly in the states its stage qualifies and loses in the rest; that
after an adoption each handle gives its object's contents from before the
hold; that an independent walker in the host harness finds every reachable
reference naming a sound object, no forwarding pointer reachable and no
object at two addresses; and that once a cut shows the hold's moves applied,
every later cut does too. A hold that only drains logs and retires regions
must adopt at every cut. On a moving hold, stage 1 loses only at the
flipping cuts, and stage 3 adopts at every cut.

Adopt branches:

- **A1, no hold.** Reset LM1 while windows are open on two cores, logs are
  undrained and one log is full. The windows finish, handles give their
  objects, the stand-in grants regions in a new block, `LOG_HEAD` reaches
  `TAIL`, and the waiting core is rung.
- **A2, `HELD` without `ACTIVE`.** A hook stops LM1 just after it stores
  `HELD`. The stuck-hold bound fires, the stand-in adopts, the epoch moves on
  by one, and `WAITING` cores are rung.
- **A3 and A4, `ACTIVE` with `MOVE` reading `NONE` or `FIXED`.** Covered by
  the killed hold.
- **A5, a task in flight.** Reset LM1 during a task. The task fails, its
  result slot holds nil, and its input handles leave the lend.
- **A6, handback.** LM1 restarts and resumes. Handles give their objects, a
  shape the stand-in registered keeps its ID, garbage made during the outage
  is freed after the first full mark, and a later `COLLECT` 2 moves objects
  with the handles following.
- **A7, a failure during the handback.** A hook stops LM1 inside its resume
  hold, and the stand-in adopts again.

Lose branches:

- **L1, `FAILED`.** Test 21's forged reference. Every handle is stale, and
  the memory returns to KDOS only after LM1's reset.
- **L2, the evidence latch.** A hook moves `EPOCH` backwards before the
  reset.
- **L3, a broken queue.** A hook moves LM1's `TAIL` backwards.
- **L4, inconsistent records.** `ACTIVE` without `HELD`, a `MOVE` epoch
  ahead of `HOLD`'s, an undefined phase. Each loses before any write.
- **L5, flipping before stage 3.** Covered by the killed hold.
- **L6, a bad forwarding pointer (stage 3).** A hook writes one outside the
  move set, and the traversal loses the heap.
- **L7, a refused resume.** On `NO_RESUME` the stand-in keeps serving; on
  `BAD_RECORD` the heap is lost.
- **L8, corruption found late.** A hook damages a header that LM1's first
  full mark reaches after the handback, and the heap is lost then.

In [managed-data.md](managed-data.md) §15, test 23, which resets LM1 outside
any hold, becomes A1, with L1 taking over the loss check, and test 24, the
overdue hold, becomes A3 or L5 by its `MOVE` state.

## 10. What changes where

When this design is written into the specifications:

- **[managed-data.md](managed-data.md):** §3, the stuck-hold bound and the
  full-log deadline; §9, `MOVE`, the move set, `STATE`, the block list and
  the shape registry; §10, the resume flag on `ATTACH`, a `NO_RESUME` status
  and the stand-in's answers; §11, the rule with loss as its fallback; §13,
  the stand-in and the handback; §14, `OBJ-ENGINE`; §15, the tests above and
  the changes to tests 23 and 24; §16, the Recovery item closed.
- **LM1 `spec/05-memory-gc.md`:** §12.3, the write order, destinations free
  at the hold's start, the `MOVE` phases, and no restart meaning in header
  GC bits; §12.6, the registry, built-in shape IDs fixed in the image and a
  range of IDs set aside for the registry; §12.9, failure; a new §12.11,
  resume.
- **LM1 `spec/06-runtime.md` §12:** resume in the managed-data service.
- **[chiplet-link.md](chiplet-link.md) §8:** for this function, a stop
  request before cancel-then-reset.

What each module adds:

- **LM1's C++ chiplet module (release plan Part 8d):** stage 0, the `MOVE`
  writes and the write order; stage 2, resume at `ATTACH`, importing the
  registry, treating old blocks as full until the first mark, and the
  resume hold; stage 3, the resolving traversal.
- **MegaPad's chiplet module (Part 9a):** stage 0, the block list, the
  registry and `STATE`; stage 1, classifying failures, the evidence latch
  and the rule, switching the library's region, shape, collect and log paths
  to the stand-in, ending an interrupted hold, failing tasks, and
  `OBJ-ENGINE`; stage 2, the handback; stage 3, the stand-in's traversal, if
  MegaPad does the resolving.

## 11. Estimates

Agent hours, by estimate:

| Work | Spec | MegaPad (9a) | LM1 (8d) | Tests | Total | Confidence |
|---|---|---|---|---|---|---|
| The three fixes | 1 | 1–2 | 0–1 | 1–2 | 3–6 | Fair |
| Stage 0: records and rules | 4–7 | 2–4 | 2–5 | 3–5 | 11–21 | Fair |
| Stage 1: the stand-in adopts | 3–5 | 10–20 | 0–1 | 7–14 | 20–40 | Moderate |
| Stage 2: LM1 takes the heap back | 3–5 | 4–8 | 11–23 | 5–9 | 23–45 | Moderate |
| Stage 3, LM1 resolving | 2–3 | 1–2 | 4–8 | 4–8 | 11–21 | Low |
| Stage 3, the stand-in resolving as well | +1 | +10–15 | — | +2 | +13–18 | Low |

Stage 1's tests include the killed-hold harness, about 5 to 8 hours, which
stage 3 reuses. The fixes and stages 0 and 1, which the release builds, come
to about 34 to 67 hours. Every figure is in addition to base work that does
not exist yet: the collector side of LM1's chiplet module and MegaPad's
managed-data words. Building stage 0's write order and records into that
collector from the start costs almost nothing; adding them after it was
built with mark bits in headers or an in-place compactor would mean a second
version of both blocks (4 to 8 hours) and reworking the collector (5 to 20
hours).

## 12. Evidence

These figures come from reading and arithmetic, not from runs. Times use the
interconnect plan's link timing (§3 and §8) and MegaPad's current 100 MHz
clock ([architecture.md](architecture.md)). No document gives LM1's reboot
time. The two largest unknowns are the jobs layer, which decides how a
failed task is reported, and the shared heap's collector design.
