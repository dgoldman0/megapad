# APT-1 RETAINED-1 ownership and lifetime ledger

Contract ID: `APT-1-RETAINED-1-2026-08-24`

This document is normative for ownership, authority, allocation, and retirement
under `APT-1-RETAINED-1.md`. The base `APT-1-OWNERSHIP.md` remains normative for
the UART stream, APT session, ANSI boundary, frame buffers, credit, CELL model,
and reset/close states.

## 1. Authority model

RETAINED-1 has two deliberately different ownership layers:

1. A single internal session-global retained backend owns discovery, the
   client parser, global model revision, transaction-ID allocator, one
   transaction slot, one resource-upload slot, credit, reset, and close. It is
   the only guest component allowed to emit retained wire frames.
2. The generic UIDL host gives that backend private per-UCTX projection
   bindings. On the wire, each materialized binding is represented exactly by
   a nonzero `(owner_id, owner_generation)`. Every owner-bound frame repeats
   that tuple. The terminal never infers authority from current focus, region,
   UIDL identity, object, resource digest, or an unscoped item ID.

The backend does not collapse bindings into one implicit owner. Conversely, a
UCTX or application never owns or services the UART. Stable UIDL element and
semantic-subkey mappings to wire owner/item tuples are private, bounded backend
state and retire with the exact projection binding. Applications receive no
broker, scope, lease, retained descriptor/provider contract, wire identity, or
retained mutation API.

The complete terminal authority key is:

```text
(session_id, presentation_epoch, owner_id, owner_generation,
 item_namespace, item_id)
```

The APT header supplies the first two components; the payload supplies owner ID,
generation, and item ID; the opcode supplies namespace. Omitting, truncating,
or accepting an older component is an authority violation.

## 2. Roles and permitted mutations

| Role | Owns | May mutate | Must not do |
|---|---|---|---|
| APT host/driver | Host lease, ingress/egress queues, terminal core, retained renderer | Decode and atomically apply validated ordered frames; publish immutable views | Infer guest owner authority; run guest code; fall back from structural LOST |
| Guest retained backend | One PT adapter, discovery, sequence/credit, revision/txid allocators, private UCTX bindings, replay plan | Project validated UIDL semantics and serialize all wire operations | Publish itself as an application service; expose wire IDs or mutation calls; allow concurrent frame writers |
| Generic UIDL host/projectors | Exact host/slot/CINST/UCTX lifecycle, UIDL tree/layout/dirty state, backend-neutral semantic snapshots | Attach, quiesce, project, relayout, and detach through the internal backend | Let application code acquire retained authority; maintain a second app-authored scene; emit protocol frames from callbacks |
| Renderer/view sink | Immutable committed CELL and retained views plus shared immutable resources | Consume every nonempty plane of one displayed revision and present/coalesce physical images within cadence rules | Drop a retained plane while claiming its revision displayed; mutate authoritative model; invent samples; release resource backing early |
| External attachment owner | Machine/session construction and coordinated reset/detach | Recreate a LOST attachment and its caller-owned capacities | Treat LOST as ANSI-safe fallback |

The foreground single-input-owner assumption of CELL-1 is unchanged. RETAINED-1
adds multiple private UCTX retained owners behind one cooperative backend;
it does not add multiple raw UART readers or writers.

## 3. State and allocation ledger

| State/allocation | Creator and allocator | Mutator | Retirement | Soft reset | Hard reset/detach |
|---|---|---|---|---|---|
| RET discovery record | Backend/client after READY; terminal validates query | Fixed CAPS/FORMATS exchange only | `presentation_epoch` end | Destroyed; rediscover | Destroyed |
| Global revision | Terminal authoritative; backend mirrors | Successful CELL/SNAPSHOT/PRESENT/OWNER_DROP commit only | `presentation_epoch` end | Reset to 0; mandatory CELL snapshot makes 1 | Destroyed |
| Global transaction ID high-water | Backend mints; terminal validates | BEGIN in either CELL or PRESENT family, or OWNER_DROP | `presentation_epoch` end | Reset for new epoch | Destroyed |
| One open transaction | Backend starts after exact byte/count/credit preflight | Backend appends canonical frames; terminal stages | Commit, abort, rejection, reset, close | Aborted | Destroyed |
| One outstanding TX_RESULT | Terminal creates after COMMIT or OWNER_DROP processing | Backend consumes and reconciles revision | Exact ordered consume | Settled before ACK | Destroyed |
| Owner live record | Backend requests for one exact private projection binding; terminal reserves quotas | Exact owner generation only | Successful OWNER_DROP status 0 -> tombstone | Destroyed, not carried across epoch | Destroyed |
| Owner tombstone | Terminal on successful exact OWNER_DROP status 0 | Newer generation open may supersede | Epoch end | Destroyed | Destroyed |
| Private UCTX projection binding | Backend from exact host/slot/CINST/UCTX authority to one wire tuple | Backend only | Terminal-confirmed OWNER_DROP status 0 or session destruction | Wire tuple invalidated before replay; live UCTX authority remains | Destroyed |
| Item ID high-water | Backend mints per owner generation/namespace; terminal mirrors | Successful DEFINE/BEGIN | Owner/epoch end; never decremented | Destroyed | Destroyed |
| Active region/object/series model | Terminal from successful commits | Exact owner transactions | Exact drop/owner drop/reset | Destroyed | Destroyed |
| Hidden retained rebuild | Terminal on valid START commit | Matching CONTINUE commits only | Atomic REVEAL, replacement START, new resize, reset, close | Destroyed | Destroyed |
| Immutable resource | Terminal after verified upload COMMIT | Never; references change transactionally | Unreferenced RESOURCE_DROP/owner drop/reset | Destroyed | Destroyed |
| One resource upload | Terminal charges owner-wide usage and physical staging on accepted BEGIN | Ordered exact-tuple chunks, COMMIT, ABORT | Exact COMMIT/ABORT or exact-upload semantic rejection, reset, close | Aborted/destroyed | Destroyed |
| Owner quota reservation | Terminal on OWNER_OPEN | Never resized in generation | Successful OWNER_DROP status 0/epoch end | Released | Released |
| Active scene quota usage | Terminal from successful DELTA/reveal within an accepted owner reservation | Exact active-target operations or atomic reveal | Exact active drop, replacement reveal, OWNER_DROP status 0, or epoch end | Released | Released |
| Hidden scene quota usage | Terminal from successful START/CONTINUE within the same owner reservation | Exact hidden-target operations only | Reveal promotion, replacement START, reset/close, or OWNER_DROP status 0 | Released | Released |
| Owner-wide resource usage | Terminal charges one count/declared bytes on accepted RESOURCE_BEGIN | Exact-upload completion/abort and exact RESOURCE_DROP | Exact-upload abort/rejection, exact RESOURCE_DROP, OWNER_DROP status 0, or epoch end | Released | Released |
| Retained immutable view | Driver publishes after logical commit/reveal | Never | Replaced after consumers release reference | Released/replaced | Released |

No row authorizes a pointer into application-owned transient memory. UIDL
semantic snapshots and source output are copied, revision-bound, or consumed
during the documented host-owned synchronous call. Host immutable views and
resources may share backing only through explicit immutable lifetime ownership.

## 4. Private projection-binding lifecycle

A projection binding is a bounded backend record containing at least:

- the exact host, host slot and slot ID, CINST pointer/ID/generation, UCTX, and
  internal binding-token generation validated by the generic host ABI;
- wire owner ID and generation when materialized;
- requested and accepted owner quotas;
- per-namespace next item ID/high-water;
- stable mappings from UCTX element index and semantic subkey to exact
  namespace/item IDs;
- lifecycle state `LOCAL`, `OPENING`, `LIVE`, `QUIESCING`, `DROPPING`,
  `TOMBSTONED`, or `QUARANTINED`; and
- copied or exact-revision-bound UIDL projection state needed for replay.

The backend allocates all storage from product policy and caller-provided
bounds. Wire maxima are upper bounds, not an instruction to allocate every
advertised slot eagerly. A profile may configure fewer binding records, but
the backend rejects projection locally before OWNER_OPEN when it cannot
represent every accepted wire record, mapping, quota, and retryable tombstone.
The complete semantic-tree admission derives the owner reservation; no applet
declares or enlarges terminal quota.

Attach validates and copies the exact live host tuple and issues an internal
generation-checked token. Before every later operation, the backend validates
that token against its own record and revalidates the stored host/slot/CINST/
UCTX graph. The token and record are private host/backend state, never an
application capability. A stale token, reused slot, changed CINST generation,
foreign host, or changed UCTX fails closed without touching wire state.

Only LIVE bindings may emit model/resource/series changes. OPENING becomes LIVE
only after `RET_RESULT(OWNER_OPEN,RET_OK)`. Host-owned pre-shutdown quiesce first
makes projection stale, synchronously detaches every semantic source callback,
and records enough exact owner state for allocation-free retryable retirement.
If callback detachment cannot be proven, application shutdown and state free
must not run. DROPPING remains authoritative until matching successful
TX_RESULT. After successful drop, the wire binding becomes TOMBSTONED and the
internal token is stale; it is never rebound to a new UCTX. A later UCTX gets a
distinct binding generation and a wire generation newer than any terminal
tombstone for a reused owner ID.

Specifically, OWNER_DROP status 2 or 3 clears the outstanding-result gate but
does not retire a DROPPING binding. The backend retains the exact wire binding,
authoritative copied desired state, quotas, and model; it neither tombstones nor
makes the reservation available to another UCTX. It may retry OWNER_DROP with
a newer transaction ID after reconciling the unchanged reported revision, or
carry the binding into the prescribed reset path. Only matching status 0
authorizes local binding retirement. Outside the reset settlement below, any
other nonzero TX_RESULT follows the base quarantine/close rule rather than this
lifecycle exception.

If an already-emitted SOFT_RESET_REQUEST crosses the pending drop before the
terminal accepted it, reset settlement instead returns status 1 without
mutation. The DROPPING binding remains authoritative until the backend consumes
that result and sends ACK; ACK then retires the entire old-epoch binding. This
status 1 permits no old-epoch retry or binding release and is not a general
third recoverable OWNER_DROP validation result.

If an owner lifecycle result is lost behind a structural frame/session fault,
the backend marks every binding QUARANTINED. It does not guess whether quota was
reserved or released. Coordinated external reset/detach destroys the epoch and
is the recovery boundary.

## 5. Exact owner and item authority checks

The terminal performs checks in this order before owner-scoped mutation:

1. header session and `presentation_epoch` under base APT-1;
2. RETAINED-1 successfully enabled in this epoch;
3. opcode legal in current global lifecycle/transaction state;
4. owner ID exists and is live;
5. generation exactly equals the live record;
6. namespace and item ID obey DEFINE/replace/drop rules; and
7. quota/reference/content validation for the whole atomic operation.

A tombstone is evidence only for rejecting stale generation and admitting a
strictly newer generation. It is not a live authority record. A digest is not a
resource authority. A globally unique application key is not a wire authority.
An object reference is valid only when its target has the same exact owner
generation unless the wire contract explicitly says otherwise; RETAINED-1
defines no cross-owner references.

IDs are never reused within an owner generation, including after failed content
commit if the terminal has acknowledged reservation of that ID. Before
acknowledged RESOURCE_BEGIN or committed DEFINE, a purely local candidate ID may
be abandoned. The backend still uses a monotonic allocator and does not depend
on reuse for boundedness.

The one session-wide upload has its own exact `(owner_id, owner_generation,
resource_id)` authority tuple. RESOURCE_CHUNK, RESOURCE_COMMIT, and
RESOURCE_ABORT validate that tuple before any destructive offset/content/state
rule. An owner/generation mismatch returns RET_STALE_OWNER; a matching owner
with an absent or wrong resource returns RET_INVALID. Neither result retires,
releases usage for, or changes the accepted offset of the actual upload. Only an
exact-tuple valid abort, successful commit, or defined semantic rejection may
retire it; abort or rejection destroys it without publication. A frame from one
owner can never destroy another owner's upload.

## 6. Quota reservation and accounting

Owner quotas are reservations, not optimistic hints. OWNER_OPEN succeeds only
when the terminal can reserve the complete request without exceeding fixed
epoch capabilities. The separate sums of live-owner region, resource, object,
and series count quotas must fit their corresponding global maxima, and the
separate resource-byte, UTF-8-byte, and sample-slot sums must fit their global
totals. Every sum is checked before mutation. This makes later admission depend
on the owner reservation rather than unrelated owners becoming idle.

The reservation ledger and usage ledgers are distinct. OWNER_OPEN increases the
aggregate live-owner reservation sums; those sums do not decrease when an item
or resource is dropped and change again only on successful OWNER_DROP status 0
or epoch retirement. Scene operations consume target-local usage, while
resource operations consume owner-wide resource-store usage, within that fixed
reservation. Upload/transaction staging also consumes bounded terminal physical
storage, but that transient storage is not a quota reservation and cannot borrow
wire authority from another owner's unused usage.

Accounting units are exact:

- one live region/resource/object/series consumes one respective count slot;
- a resource consumes its declared verified raw byte length;
- LABEL text consumes its exact UTF-8 payload byte count; a READOUT consumes
  the exact complete formatted UTF-8 byte count for its current value, signs,
  punctuation, percent marker, and unit, with no second charge for the unit;
- a series consumes its declared history capacity in sample slots, regardless
  of current sample count;
- a VECTOR object reservation permits at most `max_path_points`; checked
  `object_quota * max_path_points` bounds owner/global point storage; and
- transaction staging and upload staging are separate bounded transient pools
  derived from advertised transaction/chunk/resource policies.

For region/object/series counts, UTF-8 bytes, and sample slots, active and
committed hidden targets each have a separate logical usage ledger checked
independently against the same immutable owner reservation. They are not summed;
this permits a complete copy-on-write replacement at the negotiated logical
quota. A hidden drop changes only hidden usage. Reveal atomically promotes the
hidden ledger and retires the prior active logical ledger. The host must still
provision bounded physical staging/backing for both targets and immutable old
views to coexist; it must not accept START and discover at reveal that physical
coexistence was impossible.

Resource count and byte usage are owner-wide rather than target-local. Every
committed resource counts once whether referenced by active, hidden, both, or
neither, and an open upload charges its declared count/bytes while old resources
remain charged. Replacement does not implicitly release a resource. Only
upload abort/rejection, exact unreferenced RESOURCE_DROP, successful OWNER_DROP
status 0, or epoch retirement releases that owner-wide usage.

Successful OWNER_DROP status 0 releases every live and hidden allocation for
the exact generation and its reservations atomically. Status 2 or 3 releases
neither usage nor reservations; reset-only status 1 likewise releases neither
before ACK retires the epoch. Tombstones consume owner-record capacity but no
live-owner or item quota. Capacity exhaustion is an explicit RET_NO_CAPACITY,
never eviction of another owner.

## 7. Global transaction serialization

The backend is the sole allocator of transaction IDs and revisions. It serializes
legacy CELL transactions and PRESENT transactions through one queue:

```text
preflight -> BEGIN -> body -> COMMIT -> TX_RESULT -> next request
                            \-> ABORT -> ordered consume -> next request
```

Successful local BEGIN admission reserves all declared frames, operation/count
slots, sequence numbers, and ordinary credit through commit or abort processing.
After COMMIT, the result gate still blocks the next BEGIN even if ordinary
credit has already been returned. Projection admission returns backpressure
before BEGIN if the backend cannot finish the declared transaction. It must not
emit a partial transaction and ask UIDL or application code to recover an
unknown terminal state.

The backend accounts ordinary credit and the shared control reserve in separate
bounded ledgers. Ordered consumption of RET_RESULT, OWNER_DROP, or
RESOURCE_ABORT reclaims only its control-reserve occupancy; it never increments
ordinary released bytes or a CREDIT watermark. Transaction frames and resource
chunks remain ordinary accounting under their respective release rules.

The terminal has one staging transaction. It never applies an operation directly
to active or hidden authoritative state before validating COMMIT. A rejected
transaction releases staging, leaves active/hidden committed state unchanged,
does not advance revision, and still consumes ordered wire bytes under the base
credit/result lifecycle.

Mixed CELL/retained transactions are globally atomic. No renderer/view sink may
observe the new CELL plane with the old retained delta, or the reverse. A view
publication carries the resulting global revision and geometry generation.
Keeping that composite for diagnostics while physically rendering only its CELL
plane is also a split observation and does not satisfy publication. The
displayed revision, cadence timestamp, and input-release boundary advance only
after the sink has consumed every nonempty plane of the selected immutable
composite.

## 8. Semantic resource-source lifetime

A backend-neutral UIDL resource snapshot exposes a bounded pull source. It is
not an APT provider API and application code cannot issue retained operations
through it. The backend copies the immutable snapshot metadata and SHA3-256
digest before RESOURCE_BEGIN. For each requested offset it obtains no more than
the negotiated chunk maximum into backend-owned staging, copies or synchronously
emits exactly those bytes, and retains no returned source pointer. A source may
not write directly into the PT transmit ring or call the backend.

The backend allows one upload session-wide. It waits for successful BEGIN result,
then for CREDIT covering each chunk, then for COMMIT result. It retains enough
snapshot/digest/source state to abort exactly the active owner/generation/
resource. On source error it emits RESOURCE_ABORT if reserve and stream state
permit. It does not publish an object reference until successful resource
commit.

The backend compares every resource result's echoed tuple to that retained exact
upload tuple. It clears source/upload state only when the contract says an
exact-tuple result completed or destroyed it. A stale/wrong-tuple CHUNK or
COMMIT result leaves the real upload and source state live; reset/close cleanup
must still abort that exact upload and await its result where the wire contract
requires.

The terminal owns uploaded resource bytes after successful commit. View sinks
may share immutable backing through host reference lifetime; RESOURCE_DROP may
remove authority/model reachability but must not free backing while an already
published immutable view still references it. Such renderer retention is not a
wire resource and does not consume guest quota after the drop becomes visible.

## 9. Semantic series-source and history lifetime

A semantic UIDL series snapshot reserves a fixed history capacity for its
private owner binding. Its bounded pull source delivers explicit timestamp/value
pairs or a uniform first timestamp plus i64 values into backend-owned staging.
The backend copies each append payload before emission and retains it until the
corresponding transaction result. The source is backend-neutral and exposes no
wire descriptor, identity, or mutation authority.

The terminal's authoritative history is exactly the committed bounded ring.
Evicted oldest samples cease to be model state at commit. A renderer snapshot
may hold an immutable prior ring until its consumer releases it, under the same
host immutable-view rule as resources. Local-clock interpolation may affect
pixels between committed points only; it must not allocate, timestamp, or report
new samples.

Series replay uses DEFINE plus bounded REPLACE/APPEND chunks in a hidden
replacement/layout target. A semantic widget that cannot reproduce its declared
authoritative history must expose an honestly smaller bounded snapshot; the
backend does not synthesize missing samples.

## 10. Hidden rebuild ownership

There is at most one hidden retained target session-wide. It is terminal-owned
committed state, not an open transaction, and may span several successfully
committed PRESENT transactions so finite transaction bounds do not impose an
arbitrary maximum scene size.

Replacement START creates an empty hidden region/object/series model. Layout
START creates a copy-on-write hidden model from active retained state. Each
CONTINUE mutation has exact owner authority and quota accounting. Active
retained content is not mutated by hidden commits and is not visible after the
reset/resize boundary declares it stale. REVEAL validates the complete hidden
model and swaps it atomically; old active backing retires after immutable view
consumers release it.

A newer resize, soft reset, hard reset, detach, or valid new START retires the
old hidden target. An ordinary transaction abort retires only that transaction's
staging, not prior committed hidden work. OWNER_DROP removes matching authority
from active and hidden state together.

## 11. Reset, loss, fallback, and close

| Event | Internal backend action | Terminal retained action | ANSI authority |
|---|---|---|---|
| Unsupported discovery | Keep CELL-1; never materialize a wire owner | Skip query, send covering CREDIT only | Unchanged CELL-1 rules |
| Synchronized CLOSE/CLOSE_ACK | Stop projection, quiesce sources, drain/abort bounded lifecycle, close | Destroy retained state with session | Released only at base close boundary |
| Soft reset ACK | Invalidate wire tuples, preserve live UCTX bindings, rediscover, CELL snapshot first, allocate/replay new-epoch owners | Drop entire retained epoch, revision 0 | Binary remains owned |
| Resize | Quiesce deltas, CELL replace, hidden layout/reveal | Hide stale regions; accept newest generation layout | Binary remains owned |
| Structural/session failure | Quarantine backend and every projection binding | Freeze unusable protocol model; retain last immutable view/backing and exclusive stream ownership | Never fallback |
| Hard machine reset/detach | External owner destroys and recreates attachment/capacities | Destroy session, tombstones, views, uploads | Base external boundary decides |

Base commit settlement orders every accepted CELL or PRESENT TX_RESULT before a
locally planned reset request. A valid COMMIT crossed by an already-emitted
request is settled as aborted with unchanged revision before ACK. The same
ordering applies to OWNER_DROP: an accepted successful drop/result precedes
construction of the request, while a crossed unaccepted drop returns reset-only
status 1 and leaves its wire binding/model/quota authoritative until ACK destroys the
epoch. No result or authority disposition crosses that acknowledgement.

Unsupported RETAINED-1 is not a failure: the optional consumer stays on the real
CELL-1 output path. After successful retained discovery, a retained
semantic projection may be rejected without corrupting framing, but the backend must
reconcile its authoritative state before issuing dependent deltas. A structural
failure is never converted into “retained unavailable” or silent ANSI output.
It also is not an allocation-retirement boundary: the last immutable view and
all backing it references remain host-owned for display/diagnosis, while the
wire model and quotas remain quarantined and cannot accept or authorize work.
Only coordinated hard reset/detach or a valid close boundary retires them.

Close does not bypass authority cleanup. The backend stops new projection,
quiesces every semantic source, finishes or aborts the one upload/transaction
where the base protocol permits, services outstanding results, and then performs
synchronized close. A timeout after binary ownership was acquired remains LOST
until external reset.

## 12. Concurrency and backend ownership

The profile assumes one cooperative internal backend owner for the guest
stream. The generic UIDL host stages projection only through its private
host/slot/CINST/UCTX binding token while that exact UCTX is active or available
through validated saved context. Application callbacks cannot enqueue retained
requests. Projection admission copies or revision-binds complete semantic
snapshots into caller-bounded backend storage and reports explicit accepted,
backpressured, or failed status; it never retains arbitrary application stack
addresses or silently drops state.

Before arbitrary application shutdown, the host must quiesce the binding and
synchronously detach every semantic source callback. Quiesce records the exact
retryable owner-retirement obligation without depending on later application
state. If local callback detachment cannot be proven, shutdown and state free
must not proceed. Final host detach scrubs remaining UCTX/CINST/region references
before those objects are freed; wire acknowledgement may retire the independent
bounded tombstone afterward under the exact owner rules.

The host similarly owns one rich-terminal driver pump. Host service and guest
run alternate in bounded steps. Zero guest instructions may mean host
backpressure or admitted ingress awaiting a scheduler boundary; it is not by
itself fatal or progress. A sticky terminal/LOST failure outranks later input
acceptance. Physical renderer cadence cannot block protocol service, credit,
reset, or close.

The absence of a generic KDOS raw-ingress lease is not expanded by this profile.
The production vertical relies on the documented single-foreground UI/input
owner and cooperative PT registry. The backend must still prove the stream is
unowned before initial acquisition and preserve LOST ownership until external
attachment reset.

## 13. Required implementation invariants

Before advertising RETAINED-1, an implementation must be able to assert all of
these invariants:

1. Every retained mutation is attributable to one exact live owner generation.
2. CELL and retained transactions cannot overlap and share one ID/revision
   domain.
3. All accepted owner quotas and worst-case staging remain within caller policy.
4. No resource upload, transaction, lifecycle result, or hidden target is
   accidentally duplicated by retry.
5. Active and hidden models are distinguishable; only validating REVEAL swaps
   them.
6. Resize/reset never expose retained content before the mandatory CELL
   replacement/snapshot.
7. No application retained mutation API exists; every semantic snapshot/source
   pointer is copied, revision-bound, or consumed synchronously, and every
   host-facing shared object is immutable with explicit lifetime.
8. Unknown owner/item generations and wrong upload tuples fail closed without
   affecting another owner or the actual session-wide upload.
9. Bulk replay/upload cannot consume the base control reserve.
10. Structural failure keeps binary ownership quarantined until coordinated
    reset/detach.

These are functional contract conditions, not optional hardening. A terminal or
backend that cannot represent one of them must leave RETAINED-1 unsupported and
continue the conforming CELL-1 path.
