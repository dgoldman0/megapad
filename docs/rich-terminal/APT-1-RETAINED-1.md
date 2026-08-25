# APT-1 RETAINED-1 wire profile

Contract ID: `APT-1-RETAINED-1-2026-08-24`

Status: normative Phase 3 contract. This profile is additive to
`APT-1-CELL-1-2026-08-24`; every rule in APT-1 CELL-1 remains in force unless
this document explicitly narrows an extension point.

## 1. Scope and Akashic production vertical

RETAINED-1 adds bounded retained terminal output to an already active
APT-1 CELL-1 session. It does not replace the mandatory CELL-1 plane, ANSI
fallback, framing, negotiation, ordering, credit, close, reset, or authority
rules. A CELL-1 implementation may ignore the optional discovery query and
remain fully conforming.

Akashic consumes this profile through one generic, consumer-neutral
rich-terminal engine. Its UIDL-TUI driver may project semantic UIDL elements
as regions, vector paths, labels, readouts, meters, status indicators, bounded
series, plots, waveforms, images, and display cadence. Another trusted system
renderer may use the same engine without creating another protocol or session.
No applet is a direct protocol consumer or determines this profile's semantics
or limits.

The profile is deliberately not a canvas command stream. Owners define a
bounded terminal-side materialization. A successful update commit atomically
changes that state. The terminal may coalesce physical display refreshes, but
it may not discard committed model state or samples other than the specified
bounded-history eviction.

There is no protocol object called a presentation. `PRESENT_BEGIN` and
`PRESENT_COMMIT` are the frozen names of the atomic update envelope that can
carry CELL and retained changes together. Owners are authority and quota
namespaces within one terminal session, not presentations, documents, applets,
or application-visible scenes. Application and UIDL state remain outside the
wire and authoritative.

## 2. Base contract and scalar conventions

All messages use the APT-1 40-byte little-endian frame header, one directional
sequence space, session ID, wire `presentation_epoch`, CRC-32C, ordered lossless
transport, and cumulative byte credit. Unless a field says otherwise:

- unsigned integers are little-endian and must be in range before mutation;
- signed integers are two's-complement little-endian;
- reserved fields and unknown flag bits must be zero;
- identifiers are unsigned 64-bit integers and zero means `none` only where
  explicitly stated;
- payload sizes below are exact, except for the explicitly trailing byte or
  element arrays;
- ordinary retained frames consume byte credit exactly as CELL-1 frames do;
- all checked additions and multiplications must reject overflow rather than
  wrap; and
- a message is not partially applied. It is accepted in full, rejected in
  full, or causes the base contract's fatal structural failure.

`UNORM32` is an unsigned 32-bit coordinate. It represents the closed interval
`[0, 1]` by `value / 4294967295`. Object coordinates are relative to the
containing region or group, not terminal pixels or cells. Colors are four
consecutive bytes in red, green, blue, alpha order. Alpha is straight, not
premultiplied.

## 3. Message registry

Direction is from the APT client (`C`) or terminal (`T`). `ordinary` means the
frame uses ordinary cumulative byte credit. `reserve` means the fixed frame may
use the APT-1 control reserve under Section 17.

| Type | Name | Direction | Class | Payload |
|---:|---|---|---|---|
| `000a` | `RET_RESULT` | T -> C | reserve | 48 bytes |
| `000b` | `OWNER_DROP` | C -> T | reserve | 32 bytes |
| `000c` | `RESOURCE_ABORT` | C -> T | reserve | 32 bytes |
| `1000` | `RESOURCE_BEGIN` | C -> T | ordinary | 80 bytes |
| `1001` | `RESOURCE_CHUNK` | C -> T | ordinary | 32-byte prefix + bytes |
| `1002` | `RESOURCE_COMMIT` | C -> T | ordinary | 24 bytes |
| `1003` | `RESOURCE_DROP` | C -> T | ordinary | 24 bytes |
| `2000` | `PRESENT_BEGIN` | C -> T | ordinary | 64 bytes |
| `2001` | `PRESENT_COMMIT` | C -> T | ordinary | 16 bytes |
| `2002` | `OWNER_OPEN` | C -> T | ordinary | 64 bytes |
| `2010` | `REGION_DEFINE` | C -> T | ordinary, in transaction | 48 bytes |
| `2011` | `REGION_REPLACE` | C -> T | ordinary, in transaction | 48 bytes |
| `2012` | `REGION_DROP` | C -> T | ordinary, in transaction | 24 bytes |
| `2020` | `OBJECT_DEFINE` | C -> T | ordinary, in transaction | 64-byte prefix + body |
| `2021` | `OBJECT_REPLACE` | C -> T | ordinary, in transaction | 64-byte prefix + body |
| `2022` | `OBJECT_SET_VALUE` | C -> T | ordinary, in transaction | 32 bytes |
| `2023` | `OBJECT_SET_VISIBILITY` | C -> T | ordinary, in transaction | 32 bytes |
| `2024` | `OBJECT_DROP` | C -> T | ordinary, in transaction | 24 bytes |
| `3000` | `SERIES_DEFINE` | C -> T | ordinary, in transaction | 40 bytes |
| `3001` | `SERIES_APPEND` | C -> T | ordinary, in transaction | 40-byte prefix + samples |
| `3002` | `SERIES_REPLACE` | C -> T | ordinary, in transaction | 40-byte prefix + samples |
| `3003` | `SERIES_DROP` | C -> T | ordinary, in transaction | 24 bytes |
| `8000` | `RET_QUERY` | C -> T | ordinary optional | 8 bytes |
| `8001` | `RET_CAPS` | T -> C | ordinary optional | 64 bytes |
| `8002` | `RET_FORMATS` | T -> C | ordinary optional | 64 bytes |

The message IDs not listed here remain reserved by APT-1. In particular,
`1004..1fff`, `2003..200f`, `2013..201f`, `2025..2fff`, `3004..3fff`, and
`8003..ffff` have no meaning under this contract. A sender must not emit them;
a receiver still applies the base bit-15 optional-skip rule to the latter range.

## 4. Deterministic optional discovery

The query payload is:

```text
RET_QUERY <II>
  u32 tag                 = 0x31544552  (ASCII "RET1" in wire order)
  u32 reserved            = 0
```

The client may send exactly one `RET_QUERY` after both READY messages have
completed, the session is ACTIVE, and the client has received the successful
initial CELL snapshot TX_RESULT reporting revision one. It must send the query
outside a CELL or PRESENT transaction, outside resource upload, with no
result outstanding, and before sending any retained message. The same
CELL-snapshot-first rule applies after soft reset.
The query frame is exactly 48 complete ordinary bytes: its 40-byte header plus
8-byte payload. Let `S_before` be the client's cumulative ordinary sent-byte
counter immediately before RET_QUERY. The client checks and records
`S_query = S_before + 48`. Let `I` be the terminal receive-credit initial grant
from OFFER/READY. The unique query-release watermark is the checked sum
`G_query = I + S_query`. The client must not query if either sum exceeds u64.
It matches a received CREDIT by comparing its `cumulative_grant` payload with
`G_query`; the first payload greater than or equal to `G_query` covers the
complete query. Query sequence number, reverse-direction credit, and currently
unused client send allowance are not release watermarks.

As a separate reply-admission precondition, before sending the query the client
must have granted at least 208 currently unused terminal-to-client
ordinary bytes for the two complete 104-byte reply frames. A supporting client
receive maximum payload is therefore at least 64 bytes.

A RETAINED-1 terminal must answer in this exact terminal-stream order:

1. one `RET_CAPS` frame;
2. one `RET_FORMATS` frame; and
3. the first `CREDIT` whose cumulative value covers the complete query frame.

If the negotiated directional payload maxima cannot carry the fixed query and
two replies, or the terminal's advertised family maxima would not fit its
client-to-terminal payload maximum, the terminal gives the deterministic
CELL-only answer: no retained reply and only covering CREDIT. It must not emit
a truncated reply or advertise internally impossible capacities.

Both reply frames are exactly 64 payload bytes. They are ordinary frames and
therefore consume the terminal's outbound ordinary allowance. The terminal
must not send the covering CREDIT before both replies have been admitted to the
ordered stream. RET_CAPS and RET_FORMATS must be adjacent terminal sequence
frames: no CREDIT, input, control, or other optional frame may occur between
them. Frames already ordered before the terminal consumes the query may precede
CAPS; later frames may follow FORMATS. During this bounded reply admission the
terminal retains newly generated ordinary input under its negotiated queues.

A CELL-only terminal applies the APT-1 unknown-optional rule: it skips and
recredits `RET_QUERY` and sends no `RET_CAPS` or `RET_FORMATS`. Its first CREDIT
covering the query is the deterministic negative answer.

The client enables RETAINED-1 only if it receives one valid CAPS followed
immediately by one valid FORMATS before observing a CREDIT whose payload is at
least `G_query`. A missing, duplicate, reversed, malformed, internally
inconsistent, or late reply
is an unsupported-profile result, not permission to guess defaults. The client
continues CELL-1. A structurally valid optional reply that arrives after the
negative answer is skipped and recredited. Neither side may retry discovery in
the same `presentation_epoch`. A successful discovery marks retained
initialization required: retained content remains empty and hidden until one
RET_REPLACE_START/CONTINUE sequence completes with COMMIT_AND_REVEAL. RET_DELTA
is invalid before that first reveal.

### 4.1 RET_CAPS

`RET_CAPS` has exact layout `<IHHQIIIIIIIIQQ>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | `tag` = `0x31544552` | u32 |
| 4 | `major` = 1 | u16 |
| 6 | `minor` = 0 | u16 |
| 8 | `features` | u64 |
| 16 | `max_owner_records` | u32 |
| 20 | `max_live_owners` | u32 |
| 24 | `max_regions` | u32 |
| 28 | `max_resources` | u32 |
| 32 | `max_objects` | u32 |
| 36 | `max_series` | u32 |
| 40 | `max_operations_per_transaction` | u32 |
| 44 | `max_resource_chunk_bytes` | u32 |
| 48 | `max_retained_transaction_bytes` | u64 |
| 56 | `total_resource_bytes` | u64 |

Feature bits are:

| Bit | Name | Meaning |
|---:|---|---|
| 0 | `RET_CORE` | owners, regions, global transactions, hidden rebuild/reveal |
| 1 | `RET_VECTOR` | `GROUP` and `POLYLINE` objects |
| 2 | `RET_RGBA_IMAGE` | immutable raw RGBA8 resources and `IMAGE` objects |
| 3 | `RET_INSTRUMENT` | `LABEL`, `READOUT`, `METER`, and `STATUS` objects |
| 4 | `RET_SERIES` | bounded i64 series, `PLOT`, and `WAVEFORM` objects |
| 5 | `RET_CADENCE` | bounded display cadence and physical coalescing |
| 6 | reserved `RET_MONO_DRCS` | same-phase addendum; must be zero here |
| 7 | reserved `RET_MOSAIC` | same-phase addendum; must be zero here |

Bits 8 through 63 are zero. `RET_CORE` is mandatory for every supporting
terminal. Every other advertised feature depends on `RET_CORE`. `RET_SERIES`
also requires `RET_INSTRUMENT`, because its visible consumers are `PLOT` and
`WAVEFORM`. `RET_CADENCE` may be advertised independently of SERIES.

All maxima are terminal policy supplied by its caller. This contract does not
assign desktop-, application-, or implementation-specific numeric caps.
`max_live_owners <= max_owner_records`. The core owner, region, operation, and
transaction maxima are positive. Feature-dependent maxima are positive when
their feature is set and zero only when the corresponding family is absent.
`max_retained_transaction_bytes` includes frame headers and payloads from
`PRESENT_BEGIN` through `PRESENT_COMMIT`, and must fit both the base negotiated
transaction maximum and exact credit policy. It is never inferred from memory
available at mutation time.

The retained transaction maximum must admit BEGIN plus COMMIT plus at least one
maximum-sized operation from every advertised family. The general checked floor
is `200 + maximum_retained_operation_payload`. Exact family floors are 248 for
CORE (one REGION), `280 + 8 * max_path_points` for VECTOR,
`max(304 + max_label_bytes, 312)` for INSTRUMENT, 280 for RGBA_IMAGE, and
`max(240 + 16 * max_samples_per_append, 312)` for SERIES. These are complete
frame bytes, not payload bytes. Advertising a payload maximum that cannot be
used in one valid transaction is inconsistent discovery.

A PRESENT CELL_REPLACE with RET_NONE at geometry `(cols,rows)` requires exact
checked bytes `216 + rows * (52 + 8 * cols)`. Mixed retained operations add
their complete `40 + payload_length` frame bytes to that baseline. RETAINED-1
discovery is valid only if the
current geometry fits `max_retained_transaction_bytes`; the terminal must also
reject/defer any later RESIZE before publication unless the requested geometry
fits that maximum, the base transaction maximum, payload bounds, and available
credit. It may not publish an accepted resize that can be rebuilt only by the
now-forbidden legacy snapshot path.

### 4.2 RET_FORMATS

`RET_FORMATS` has exact layout `<IIIIIIIIIIQQQ>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | `coordinate_format` = 1 (`UNORM32`) | u32 |
| 4 | `color_format` = 1 (`RGBA8`) | u32 |
| 8 | `image_format` | u32 |
| 12 | `max_image_width` | u32 |
| 16 | `max_image_height` | u32 |
| 20 | `max_path_points` | u32 |
| 24 | `max_label_bytes` | u32 |
| 28 | `max_samples_per_append` | u32 |
| 32 | `max_history_per_series` | u32 |
| 36 | `minimum_presentation_interval_us` | u32 |
| 40 | `total_sample_slots` | u64 |
| 48 | `total_utf8_bytes` | u64 |
| 56 | `reserved` = 0 | u64 |

`image_format` is 1 for raw row-major sRGB straight-alpha RGBA8 and zero when
`RET_RGBA_IMAGE` is absent. Image width and height are positive exactly when
that feature is set. `max_path_points` is positive exactly when VECTOR is set.
Label and total UTF-8 bounds are positive exactly when INSTRUMENT is set.
Samples-per-append, history-per-series, and total sample slots are positive
exactly when SERIES is set. `minimum_presentation_interval_us` is positive
exactly when CADENCE is set and otherwise zero. It is a renderer admission
bound, not a clock source or permission to invent samples.

Advertised maxima must describe at least one usable maximum-sized item:
`total_utf8_bytes >= max_label_bytes` when INSTRUMENT is set;
`max_samples_per_append <= max_history_per_series <= total_sample_slots` when
SERIES is set; and checked
`max_image_width * max_image_height * 4 <= total_resource_bytes` when IMAGE is
set. `max_objects` is positive when any object feature is set, and `max_series`
is positive exactly when SERIES is set. Values may be smaller than another
implementation's policy, but may not be self-contradictory.

Every advertised bound is a hard acceptance bound for the `presentation_epoch`.
The terminal may reject an owner quota that cannot be reserved within these
bounds, but it may not advertise a value and then accept an owner on the
assumption that some unrelated owner will use less.

Capabilities must also be consistent with both negotiated base payload maxima.
RET_CORE requires a client-to-terminal payload maximum of at least 64 and a
terminal-to-client maximum of at least 64. IMAGE requires at least 80 inbound
bytes and `32 + max_resource_chunk_bytes` must fit. VECTOR requires
`80 + 8 * max_path_points` to fit. INSTRUMENT requires both
`80 + max_label_bytes` and `104 + max_label_bytes` to fit and at least 112 bytes
for the largest fixed body. SERIES requires at least 112 bytes and
`40 + 16 * max_samples_per_append` to fit, covering explicit samples. All
arithmetic is checked. A client must treat an inconsistent reply pair as the
deterministic unsupported-profile outcome.

## 5. Shared transaction and revision domain

CELL-1 transactions remain valid and unchanged. RETAINED-1 adds
`PRESENT_BEGIN`/`PRESENT_COMMIT` for retained-only or mixed cell/retained
updates. Both transaction families use one session-wide transaction slot, one
strictly increasing nonzero transaction-ID allocator, and one global 64-bit
model revision within the `presentation_epoch`.

A transaction ID used by either family must be greater than every previously
started transaction ID in that epoch. A sender must not begin either family
while the other is open or while the preceding `TX_RESULT` is outstanding.
Every successful CELL `TX_COMMIT`, `SNAPSHOT_COMMIT`, or `PRESENT_COMMIT`
increments the same revision by exactly one. An abort or rejected transaction
does not increment it. Existing `TX_RESULT` reports the resulting global
revision for either family.

Because base SNAPSHOT_COMMIT assigns revision one, it is permitted only as the
pre-discovery revision-zero-to-one initial CELL step or mandatory post-soft-reset
CELL recovery step. Assigning one is exactly the single global increment in
those cases. After RETAINED-1 becomes enabled in an epoch, legacy
SNAPSHOT_BEGIN is invalid. A resize or any later replace-all CELL update must use
PRESENT_BEGIN with CELL_REPLACE; its successful PRESENT_COMMIT increments the
current global revision normally. This restriction prevents a resize snapshot
from rewinding retained revision.

An implementation must expose the resulting global revision to input event
normalization. Existing KEY/TEXT/POINTER/RESIZE/FOCUS `model_revision` fields
therefore refer to this model revision once RETAINED-1 is enabled.
While a hidden replacement/layout target exists, the terminal emits no
normalized KEY/TEXT/POINTER/FOCUS event: intermediate commits advance the
global revision but do not have a matching visible retained view. The frontend
must retain bounded raw intent or apply backpressure rather than mis-stamp or
silently reroute it. RESIZE, reset, close, credit, and result control retain
their explicit lifecycle behavior.

### 5.1 PRESENT_BEGIN

`PRESENT_BEGIN` has exact layout `<QQQQIIIIIIII>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | `transaction_id` | u64 |
| 8 | `base_revision` | u64 |
| 16 | `geometry_generation` | u64 |
| 24 | `declared_transaction_bytes` | u64 |
| 32 | `cols` | u32 |
| 36 | `rows` | u32 |
| 40 | `cell_span_count` | u32 |
| 44 | `cell_count` | u32 |
| 48 | `retained_operation_count` | u32 |
| 52 | `cell_mode` | u32 |
| 56 | `retained_mode` | u32 |
| 60 | `reserved` = 0 | u32 |

`base_revision` must equal the current global revision. `cols`, `rows`, and
`geometry_generation` must equal the terminal's currently selected geometry.
The generation is the terminal-issued generation carried by its accepted
RESIZE event; before any resize it is zero. A stale geometry transaction is
rejected without staging.

`declared_transaction_bytes` is the exact sum of all complete 40-byte frame
headers and payloads from this BEGIN through its COMMIT, inclusive. It is
positive, no greater than `max_retained_transaction_bytes`, no greater than the
base negotiated transaction maximum, and completely covered by already
available ordinary byte credit before BEGIN is sent. Transaction frames remain
charged until commit or abort exactly as in CELL-1.

`cell_mode` is:

- 0 `CELL_NONE`: both cell counts are zero and no CELL_SPAN/CURSOR follows;
- 1 `CELL_DELTA`: counts describe a normal CELL-1 delta; or
- 2 `CELL_REPLACE`: `cell_span_count` equals `rows`, `cell_count` equals the
  checked product `cols * rows`, and the body contains exactly one full-width
  span `(row, 0, cols)` for each row in ascending order.

`retained_mode` is:

- 0 `RET_NONE`: retained operation count is zero;
- 1 `RET_DELTA`: operations target the active retained model;
- 2 `RET_REPLACE_START`: stage an empty hidden retained replacement that will
  supersede any old hidden target only if this transaction commits;
- 3 `RET_REPLACE_CONTINUE`: target that hidden replacement;
- 4 `RET_LAYOUT_START`: stage a copy-on-write layout target from the active
  retained model that will supersede any old hidden target only if this
  transaction commits; or
- 5 `RET_LAYOUT_CONTINUE`: target that hidden layout model.

Values outside these enums are transaction errors. A BEGIN with both modes zero
is invalid. A DELTA cannot run while a hidden replacement/layout is pending or
while reset/resize requires rebuild. START is invalid if its matching rebuild is
not required. Initial successful discovery, soft-reset replay, and resize
respectively establish the required initial-replacement, replacement, or layout
condition. CONTINUE is invalid without the matching hidden target.

START never destroys prior hidden work at BEGIN time. The prospective new
target is transaction staging. Abort, semantic rejection, stale revision, bad
counts/bytes, or failed commit leaves the previously committed hidden target
unchanged. Only a successful START commit atomically replaces it.

`retained_operation_count` must not exceed the advertised per-transaction
maximum. RET_NONE requires zero. RET_DELTA requires at least one retained
operation. START and CONTINUE may carry zero operations so an empty replacement
or a final validating reveal does not require a dummy semantic mutation.

Only REGION_DEFINE/REPLACE/DROP, OBJECT_DEFINE/REPLACE/SET_VALUE/
SET_VISIBILITY/DROP, and SERIES_DEFINE/APPEND/REPLACE/DROP count as retained
mutation frames and may occupy the retained body. Owner, resource, discovery,
input, lifecycle result, credit, reset, error, and close frames are prohibited
inside PRESENT.

Frames inside the transaction are canonical and contiguous: exactly
`cell_span_count` CELL_SPAN frames, then exactly one CURSOR when cell mode is not
NONE, then exactly `retained_operation_count` retained mutation frames, then
PRESENT_COMMIT. CELL_DELTA spans use the existing payload and scalar rules.
CELL_REPLACE uses only the canonical full-width row spans above; alternative
gapless splitting is invalid. With RET_NONE its complete transaction byte count
is exactly `216 + rows * (52 + 8 * cols)`; a mixed transaction adds every
retained operation's complete frame bytes. No other frame may intervene. Exact
count, cell coverage, operation, byte, reference, quota, and graph validation
occurs before atomic commit.

Logical scene usage is target-local. For each owner, the active target and a
committed hidden target independently check region count, object count, series
count, complete LABEL/READOUT UTF-8 bytes, and declared series history sample
slots against the same immutable OWNER_OPEN reservation. Those two logical
scene ledgers are not summed. A RET_DELTA commit validates the proposed active
ledger; START/CONTINUE validates the proposed hidden ledger. A drop in a hidden
transaction changes hidden usage only and never releases or mutates active
usage. Physical backing/staging must nevertheless be capacity-provisioned for
active and hidden targets to coexist.

Resource count and resource bytes are different: they form one owner-wide
actual resource-store usage ledger. It includes every committed resource,
including unreferenced resources and distinct old/new resources retained during
replacement, plus the declared count/bytes of the one open upload. A resource
referenced by both active and hidden targets is counted once; references do not
create a second resource allocation. This owner-wide ledger must stay within
`resource_quota` and `resource_byte_quota` throughout upload, commit, reveal,
and drop.

Every successfully committed intermediate hidden graph must be structurally
representable: owner authority, quotas, typed payloads, parent acyclicity, and
all references within the graph must already validate. Replacement transactions
therefore define regions and series before dependent objects, either in an
earlier committed hidden transaction or earlier in the same transaction.
Intermediate graphs may be incomplete only by omitting desired items not yet
defined and, for LAYOUT, by retaining regions stamped with the prior geometry.
They never contain dangling references or partially decoded items. The active
model remains untouched. REVEAL additionally requires the complete hidden graph
to be internally consistent and, for LAYOUT, every surviving region stamped
with the current generation.

### 5.2 PRESENT_COMMIT and abort

`PRESENT_COMMIT` has exact layout `<QII>`:

```text
u64 transaction_id
u32 disposition
u32 reserved = 0
```

`disposition` is 0 `COMMIT` or 1 `COMMIT_AND_REVEAL`. DELTA requires COMMIT.
REPLACE/LAYOUT intermediate commits require COMMIT. A final hidden transaction
uses COMMIT_AND_REVEAL. Reveal is valid only if the entire hidden model is
self-consistent, every owner's hidden scene ledger independently fits its fixed
reservation, owner-wide resource usage still fits its resource reservation,
and the advertised physical/model bounds remain satisfied. Reveal does not
alter the aggregate owner reservation ledger. It promotes hidden scene usage to
active and retires the old active logical ledger atomically; immutable old view
backing may remain physically retained until consumers release it. A replacement
reveal swaps the hidden retained model for the active one. A layout reveal
additionally requires every live region to have been defined or replaced
against the current geometry generation. The swap and any mixed CELL change
become visible at one logical output boundary.

The existing `TX_ABORT <QH6x>` aborts a PRESENT transaction. It discards only
that transaction's staging. It does not discard an already committed hidden
rebuild target; reset, close, a successfully committed new START, or hard reset
does that. RETAINED-1 does not add a result to base TX_ABORT: an abort is
completed by ordered frame consumption and credit release under CELL-1. A
PRESENT_COMMIT, successful or rejected, produces the existing TX_RESULT.

## 6. Owner identity and quotas

All retained items have the authority key:

```text
(session_id, presentation_epoch, owner_id, owner_generation,
 item_namespace, item_id)
```

The wire carries owner ID and generation on every owner-bound message. The
header supplies session and epoch. Owner IDs and generations are nonzero. The
same numeric item ID in different namespaces is distinct. Namespaces are
REGION, RESOURCE, OBJECT, and SERIES.

`OWNER_OPEN` has exact layout `<QQIIIIQQQQ>`:

| Offset | Field | Type |
|---:|---|---|
| 0 | `owner_id` | u64 |
| 8 | `owner_generation` | u64 |
| 16 | `region_quota` | u32 |
| 20 | `resource_quota` | u32 |
| 24 | `object_quota` | u32 |
| 28 | `series_quota` | u32 |
| 32 | `resource_byte_quota` | u64 |
| 40 | `utf8_byte_quota` | u64 |
| 48 | `sample_slot_quota` | u64 |
| 56 | `reserved` = 0 | u64 |

The terminal reserves all quotas atomically before returning success. Each
individual count quota must not exceed its corresponding advertised maximum,
and the sum of region, resource, object, and series count quotas respectively
across all live owners must not exceed `max_regions`, `max_resources`,
`max_objects`, and `max_series`. Resource-byte, UTF-8-byte, and sample-slot
reservations across all live owners likewise must not exceed their advertised
totals. Checked addition precedes mutation. An individually valid request whose
aggregate would exceed any total returns RET_NO_CAPACITY and changes no owner
record or reservation. A zero quota is valid for an unused feature family. An open
with a feature-dependent nonzero quota is invalid when that feature is absent.
Polyline point storage is bounded by `object_quota * max_path_points`, with
checked multiplication; a terminal advertising VECTOR must be able to honor
that worst case for accepted object reservations.

Region, object, series, UTF-8, and sample-slot quotas bound each logical scene
target independently: active usage and committed hidden usage must each fit the
same immutable owner reservation and are not added together. Resource count and
bytes instead bound the one owner-wide resource-store usage described in
Section 5. Physical memory for simultaneous active, hidden, transaction, upload,
and immutable-view backing is a separate advertised/policy capacity obligation;
it does not change the logical quota arithmetic.

Owner records include live owners and tombstones and are bounded by
`max_owner_records`; live owners are additionally bounded by `max_live_owners`.
Opening a never-seen owner creates a live record. Reopening a tombstoned ID
requires a generation strictly greater than the tombstone generation. An exact
duplicate open of a live `(id,generation)` with identical quotas succeeds
idempotently; different quotas are a conflict. A generation less than or equal
to a tombstone, or a different generation for a live ID, is stale authority.

`OWNER_OPEN` is serialized outside transactions and resource upload. One
`RET_RESULT` completes it before another lifecycle request.

`OWNER_DROP` is a control-reserve message with exact layout `<QQQQ>`:

```text
u64 transaction_id
u64 base_revision
u64 owner_id
u64 owner_generation
```

It is valid only with no transaction, upload, or result outstanding. The
transaction ID and base revision use the shared global rules. The terminal
atomically drops every live region, resource, object, and series for the exact
owner generation, releases its quota reservations, creates/updates the owner
tombstone, increments the global revision, and returns normal `TX_RESULT`.
Dropping the exact tombstone again is idempotent: it succeeds, advances the
revision once for that ordered request, and changes no allocations. A stale or
different live generation is rejected. Base-revision mismatch produces
TX_RESULT status 3 and no mutation; every other owner/ID/scalar rejection
produces status 2. Exact live or exact-tombstone success produces status 0 and
the incremented revision. No numeric item ID from a dropped generation may be
reused within that generation. Idempotence here is exact allocation/authority
state; each newly ordered successful drop request remains revisioned.

Only status 0 completes the drop: it tombstones the binding, releases the owner
reservation, removes the matching model, and advances the revision. Status 2 or
3 is a recoverable OWNER_DROP lifecycle rejection and must report the unchanged
current revision. It leaves the live/tombstone ledger, owner reservation and
usage, active and hidden model, immutable resources, and broker lease authority
unchanged. The broker must not optimistically remove any of those before the
result; after status 2 or 3 it retains the exact binding and authoritative
desired state and may retry with a newer transaction ID, reconcile its mirrored
revision, or enter coordinated reset. Sequence and transaction-ID high-water
consumed by the rejected request do not roll back. OWNER_DROP does not broaden
recoverability beyond the two retained exceptions explicitly defined in
Section 15; base CELL and mixed/CELL-including failures remain fatal.

There is one reset-settlement status in addition to those ordinary outcomes. If
an already-emitted SOFT_RESET_REQUEST crosses an OWNER_DROP that was emitted by
the client but not accepted by the terminal, the terminal MUST consume the drop
without applying it and return old-epoch TX_RESULT for that OWNER_DROP
transaction ID with status 1 and the unchanged revision equal to the request's
`last_revision`. The broker retains the exact live or tombstoned binding and all
owner state until the new-epoch reset ACK retires the epoch. This status 1 is
legal only for that crossed-reset cancellation; it is not an OWNER_DROP
validation status and does not authorize old-epoch retry before ACK.

## 7. Lifecycle result

`RET_RESULT` has exact layout `<HHIQQQQQ>` (48 bytes):

| Offset | Field | Type |
|---:|---|---|
| 0 | `request_type` | u16 |
| 2 | `status` | u16 |
| 4 | `detail` | u32 |
| 8 | `owner_id` | u64 |
| 16 | `owner_generation` | u64 |
| 24 | `item_id` | u64 |
| 32 | `current_revision` | u64 |
| 40 | `accepted_bytes` | u64 |

`request_type` is the type being completed. `detail` is zero unless a field
rule defines a bounded index; no rule in this version does, so senders emit
zero. `item_id` is zero for owner requests. `accepted_bytes` is the committed
resource byte length only for successful RESOURCE_COMMIT and zero otherwise.
For resource requests, `owner_id`, `owner_generation`, and `item_id` echo the
request's owner, generation, and resource ID even when the request is rejected;
they never substitute the tuple of a different session-wide upload.

Status values are:

| Value | Name | Meaning |
|---:|---|---|
| 0 | `RET_OK` | request completed |
| 1 | `RET_INVALID` | well-framed scalar/state request is invalid |
| 2 | `RET_STALE_OWNER` | owner ID/generation lacks authority |
| 3 | `RET_NO_CAPACITY` | declared quota/storage cannot be reserved |
| 4 | `RET_DUPLICATE_ID` | ID violates monotonic/duplicate definition rule |
| 5 | `RET_IN_USE` | resource/owner lifecycle conflicts with live references |
| 6 | `RET_BAD_CONTENT` | uploaded resource bytes fail digest/content validation |
| 7 | `RET_ABORTED` | upload was explicitly or semantically aborted |

Exactly one RET_RESULT is sent for OWNER_OPEN, RESOURCE_BEGIN, RESOURCE_COMMIT,
RESOURCE_DROP, and RESOURCE_ABORT. A rejected RESOURCE_CHUNK also sends exactly
one RET_RESULT with request type RESOURCE_CHUNK. It destroys an upload only for
an exact-upload semantic rejection under Section 8; a wrong tuple leaves the
actual session-wide upload unchanged. A valid chunk is acknowledged only by
covering CREDIT. OWNER_DROP uses TX_RESULT. Lifecycle errors are not transaction
downgrades and do not mutate the active model. The result frame is fixed
control-reserve traffic. The client must service it before issuing another
lifecycle request.

Lifecycle status selection is deterministic:

| Request/condition | Status | State after result |
|---|---|---|
| OWNER_OPEN exact live duplicate with identical quotas | RET_OK | existing live record unchanged |
| OWNER_OPEN stale/different generation | RET_STALE_OWNER | unchanged |
| OWNER_OPEN scalar, feature, or quota-above-advertised error | RET_INVALID | unchanged |
| OWNER_OPEN valid reservation cannot fit record/global totals | RET_NO_CAPACITY | unchanged |
| RESOURCE_BEGIN stale owner | RET_STALE_OWNER | no upload opened |
| RESOURCE_BEGIN ID at/below namespace high-water | RET_DUPLICATE_ID | no upload opened |
| RESOURCE_BEGIN invalid format/dimensions/length/flags | RET_INVALID | no upload opened |
| RESOURCE_BEGIN valid declaration cannot fit owner-wide reserved resource usage or bounded physical staging | RET_NO_CAPACITY | no upload opened; aggregate owner reservations unchanged |
| RESOURCE_CHUNK owner ID/generation differs from the open upload tuple | RET_STALE_OWNER | actual upload and usage unchanged |
| RESOURCE_CHUNK exact live owner but absent/wrong resource tuple | RET_INVALID | actual upload and usage unchanged |
| RESOURCE_CHUNK exact-upload offset/length/overrun error | RET_INVALID | exact upload destroyed; usage/staging released |
| RESOURCE_COMMIT owner ID/generation differs from the open upload tuple | RET_STALE_OWNER | actual upload and usage unchanged |
| RESOURCE_COMMIT exact live owner but absent/wrong resource tuple | RET_INVALID | actual upload and usage unchanged |
| RESOURCE_COMMIT exact-upload incomplete data | RET_INVALID | exact upload destroyed; usage/staging released |
| RESOURCE_COMMIT exact-upload digest mismatch | RET_BAD_CONTENT | exact upload destroyed; usage/staging released |
| RESOURCE_DROP absent/wrong tuple | RET_INVALID or RET_STALE_OWNER | unchanged |
| RESOURCE_DROP referenced by active or hidden model | RET_IN_USE | unchanged |
| RESOURCE_ABORT exact upload and valid reason | RET_ABORTED | upload destroyed |
| RESOURCE_ABORT absent/wrong tuple or reason | RET_INVALID or RET_STALE_OWNER | matching exact upload unchanged on bad reason; unrelated upload unchanged |

For rows offering two statuses, wrong owner generation is RET_STALE_OWNER and
an absent item under an otherwise exact live owner is RET_INVALID. No receiver
selects status from local exception text.

## 8. Immutable RGBA resources

Resources are immutable content-address-verified byte objects. RETAINED-1
defines one resource format: raw row-major sRGB straight-alpha RGBA8, with no
row padding. Resource IDs are owner-local, nonzero, and strictly increasing
within an owner generation. The content digest is SHA3-256 over the exact raw
bytes. Equal digests do not merge authority or identifiers.

Only one resource upload may exist session-wide. No transaction, owner
lifecycle request, or second upload may overlap it. OWNER_OPEN has already
reserved the owner's count and byte quota. Before acknowledging BEGIN, the
terminal must atomically charge the declared resource count and bytes to that
owner's remaining owner-wide resource usage and acquire its bounded physical
upload staging. Neither action changes any owner's quota reservation or the
aggregate live-owner reservation sums.

The open upload is identified by the exact tuple `(owner_id, owner_generation,
resource_id)`. After base framing/state validation, RESOURCE_CHUNK and
RESOURCE_COMMIT compare that tuple before offset, length, completeness, digest,
or other destructive validation. If an upload is open and owner ID or generation
differs—even when the frame names another independently live owner—the result is
RET_STALE_OWNER. If the owner tuple matches but the resource differs, the result
is RET_INVALID. With no open upload, a stale owner tuple is RET_STALE_OWNER and
an otherwise exact live owner/resource request is RET_INVALID. Every such tuple
rejection consumes the offending frame and sends its result but leaves the
actual open upload, accepted offset, owner-wide resource usage, and physical
staging unchanged. It cannot destroy or abort another owner's upload.

`RESOURCE_BEGIN` has exact layout `<QQQIIIIQ32s>`:

```text
u64 owner_id
u64 owner_generation
u64 resource_id
u32 format              = 1 (raw RGBA8)
u32 width
u32 height
u32 flags               = 0
u64 byte_length
u8  sha3_256[32]
```

Width and height are positive and within RET_FORMATS. `byte_length` must equal
checked `width * height * 4`; one resource-count slot and `byte_length` bytes
must fit the opening owner's remaining reserved usage and be representable by
the implementation's negotiated physical staging policy. A successful
RET_RESULT opens the upload; chunks sent before it are state errors. Another
owner's idle usage does not admit this resource and an upload does not alter
aggregate owner reservations.

`RESOURCE_CHUNK` begins with exact prefix `<QQQQ>` followed by bytes:

```text
u64 owner_id
u64 owner_generation
u64 resource_id
u64 offset
u8  data[payload_length - 32]
```

Data is nonempty and no longer than `max_resource_chunk_bytes`. Offset must
equal the number of bytes already accepted; chunks are contiguous, ordered,
and non-overlapping. The client must observe CREDIT covering one complete chunk
before sending the next, so bounded receiver storage never depends on an
unbounded run of chunks. A well-framed chunk with stale authority returns
RET_STALE_OWNER and a same-owner wrong resource returns RET_INVALID without
touching the actual upload, as specified above. Only after the full upload tuple
matches do bad offset, empty data, excessive chunk length, checked end-offset
overflow, or end past declared byte length return RET_INVALID and destroy that
exact upload. The terminal consumes the complete frame, releases the destroyed
upload's uncommitted owner-wide resource-usage charge and physical staging, and
sends RET_RESULT for RESOURCE_CHUNK. The owner's quota reservation and aggregate
reservation ledger remain unchanged. It never accepts a prefix or retains a
retryable partial chunk.

`RESOURCE_COMMIT <QQQ>` names owner, generation, and resource. It is valid only
for the exact open upload tuple and after exactly `byte_length` bytes. A wrong
owner/generation returns RET_STALE_OWNER and a same-owner absent/wrong resource
returns RET_INVALID; neither changes the actual upload. For an exact tuple,
incomplete data returns RET_INVALID and destroys the upload. Otherwise the
terminal validates SHA3-256 before publishing the immutable resource and
returning RET_OK. A digest mismatch returns RET_BAD_CONTENT and destroys the
exact upload without creating the ID. Each exact-upload rejection releases its
uncommitted owner-wide resource-usage charge and physical staging but no owner
quota reservation. No exact-upload COMMIT error leaves a commit-uncertain
upload open. The acknowledged resource ID high-water remains consumed.

`RESOURCE_ABORT` is a reserve message with exact layout `<QQQH6x>`: owner,
generation, resource ID, and reason. Reason is 0 caller cancel, 1 reset/rebuild
cancel, or 2 local shutdown; other values are invalid. It destroys only the
matching open upload and returns RET_ABORTED. It never publishes a resource.

`RESOURCE_DROP <QQQ>` names owner, generation, and resource. It succeeds only
outside a transaction/upload and when the resource has no reference in either
the active model or a committed hidden rebuild. It releases that resource's
owner-wide count and byte usage and returns RET_OK. It does not shrink the
OWNER_OPEN reservation or aggregate live-owner reservation sums. Dropping an
absent ID is RET_INVALID; IDs are not reused.

## 9. Regions

Regions bind retained coordinates to the selected cell geometry. DEFINE and
REPLACE have exact layout `<QQQIIIIiI>`:

```text
u64 owner_id
u64 owner_generation
u64 region_id
u32 cell_x
u32 cell_y
u32 cell_cols
u32 cell_rows
i32 z_order
u32 flags
```

`cell_cols` and `cell_rows` are positive; the checked rectangle is within the
PRESENT_BEGIN geometry. Flags bit 0 is initial visibility and bit 1 enables
clipping at the region rectangle; other bits are zero. Region IDs are nonzero
and strictly increasing on DEFINE. REPLACE requires an existing exact-owner
region and is its complete definition. `REGION_DROP <QQQ>` names owner,
generation, and ID. Commit validates that no surviving object refers to a
dropped region. Overlapping regions render by signed region z-order, then owner
ID, then region ID in ascending back-to-front order.

Regions are stamped with PRESENT_BEGIN `geometry_generation`. A resize makes
the active retained plane hidden and layout-rebuild-required. A layout reveal
is invalid until every surviving region is stamped with the new generation.

## 10. Generic objects

OBJECT_DEFINE and OBJECT_REPLACE start with exact common prefix
`<QQQHHiQQIIII>` (64 bytes), followed by the exact type body in Section 11:

| Offset | Field | Type |
|---:|---|---|
| 0 | `owner_id` | u64 |
| 8 | `owner_generation` | u64 |
| 16 | `object_id` | u64 |
| 24 | `object_type` | u16 |
| 26 | `flags` | u16 |
| 28 | `z_order` | i32 |
| 32 | `region_id` | u64 |
| 40 | `parent_object_id` | u64 |
| 48 | `left` | UNORM32 |
| 52 | `top` | UNORM32 |
| 56 | `right` | UNORM32 |
| 60 | `bottom` | UNORM32 |

Object flag bit 0 is initial visibility; all other bits are zero. `left < right`
and `top < bottom`. With parent zero, coordinates are region-relative. A
nonzero parent must be a GROUP of the same owner and region; coordinates are
relative to that group's bounds. The final parent graph must be acyclic. Group
visibility cascades; object and region z-order compare signed, with object ID
as the deterministic final tie-breaker.

DEFINE requires a nonzero ID strictly greater than the owner's prior object
high-water mark. REPLACE requires an existing exact-owner object and the same
object type; it is a complete definition. All referenced regions, parents,
resources, and series must survive the same commit. Validation is against the
transaction's final graph, so an ordered set may define dependencies earlier
in the same transaction and may repair references before commit.

`OBJECT_SET_VALUE <QQQq>` names owner, generation, object ID, and signed i64
value. It is valid only for READOUT, METER, and STATUS. Meter values must remain
within its declared range. `OBJECT_SET_VISIBILITY <QQQB7x>` carries a canonical
boolean byte 0 or 1. `OBJECT_DROP <QQQ>` drops the object at commit; surviving
children make commit invalid. These operations count as one retained operation
each.

## 11. Object types and exact bodies

Object type values are:

| Value | Name | Required feature |
|---:|---|---|
| 1 | `GROUP` | VECTOR |
| 2 | `POLYLINE` | VECTOR |
| 3 | `IMAGE` | RGBA_IMAGE |
| 4 | `LABEL` | INSTRUMENT |
| 5 | `READOUT` | INSTRUMENT |
| 6 | `METER` | INSTRUMENT |
| 7 | `STATUS` | INSTRUMENT |
| 8 | `PLOT` | SERIES |
| 9 | `WAVEFORM` | SERIES |

### 11.1 GROUP

GROUP has no type body; payload length is exactly 64. It establishes a nested
coordinate and visibility scope. Nesting depth has no separate semantic cap;
it is already bounded by the accepted object quota, and implementations must
validate/traverse iteratively if their native call stack is smaller.

### 11.2 POLYLINE

The body is `<II4BI>` followed by `point_count` points `<II>`:

```text
u32 point_count
u32 stroke_width          (UNORM32 fraction of min(object width,height))
u8  red, green, blue, alpha
u32 path_flags            (bit 0 = closed; other bits zero)
repeat point_count:
  u32 x, y                (UNORM32 relative to object bounds)
```

`point_count` is at least 2 and at most `max_path_points`; `stroke_width` is
nonzero. Paths use straight segments with round joins and round end caps. A
closed path adds the last-to-first segment; it does not imply fill.

### 11.3 IMAGE

The body is exact `<QIB3x>` (16 bytes): resource ID, fit mode, and opacity.
Fit 0 stretches, 1 contains, and 2 covers. Opacity is 0..255 and multiplies
resource alpha. The resource must be the same owner generation and format 1.

### 11.4 LABEL

The body begins with exact `<4BHHII>` (16 bytes), followed by `text_bytes` UTF-8:

```text
u8  red, green, blue, alpha
u16 horizontal_align      (0 start, 1 center, 2 end)
u16 vertical_align        (0 top, 1 middle, 2 bottom)
u32 text_bytes
u32 label_flags           (bit 0 = ellipsize; other bits zero)
u8  text[text_bytes]
```

Text is well-formed UTF-8 scalar text, contains no CR, LF, or NUL, and is at
most `max_label_bytes`. Empty text is valid. The terminal's output font
is authoritative; no font identifier or host-measured glyph metric crosses the
wire. Font choice does not affect accounting: the exact text byte count
contributes to the transaction target's post-commit UTF-8 usage and must fit
that target's copy of the owner reservation under Section 5.

### 11.5 READOUT

The body begins with exact `<8BIIqqII>` (40 bytes), followed by `unit_bytes`
UTF-8:

```text
u8  foreground_rgba[4]
u8  background_rgba[4]
u32 format                (0 integer, 1 fixed, 2 percent)
u32 decimal_places
i64 initial_value
i64 scale
u32 unit_bytes
u32 reserved = 0
u8  unit[unit_bytes]
```

Integer format requires decimal_places zero and scale one. Fixed and percent
require positive scale. Fixed formats the exact mathematical signed rational
`value / scale`. Percent formats the exact mathematical signed rational
`100 * value / scale` and then appends one ASCII `%`. The percent computation
must use a sufficiently wide intermediate or an equivalent checked
quotient/remainder decomposition; an otherwise valid signed i64 value must not
be rejected merely because the intermediate `100 * value` would overflow i64.

The canonical numeric bytes are ASCII signed decimal with no leading zero
except the single zero before a fractional part. When `decimal_places` is zero,
fixed and percent emit no decimal point and no fractional digits. When it is
positive, they emit one ASCII `.` followed by exactly `decimal_places` digits.
The exact unit UTF-8 bytes follow immediately with no implicit separator. No
positive plus sign is emitted; a negative mathematical value retains one ASCII
minus sign even if its rounded magnitude is zero. Signed rational formatting
rounds the final display digit to nearest with ties away from zero; requested
trailing fractional digits are retained. Only the checked formatted-length and
owner-quota preflight below bounds the representation size.

Before DEFINE, REPLACE, or OBJECT_SET_VALUE mutates staging, the receiver must
compute the complete formatted byte length—including minus sign, digits,
decimal point, percent sign, and unit—using checked arithmetic without first
allocating an unbounded string. That length must be at most `max_label_bytes`
and the transaction target's post-commit sum of LABEL text plus complete
READOUT formatted bytes must fit that target's copy of the owner's
`utf8_byte_quota`. The complete formatted READOUT consumes target-local usage;
its unit is not charged a second time. Failure is a transaction error and
leaves the prior object/value and active or hidden usage unchanged. Unit text
independently obeys LABEL UTF-8 scalar/control rules.

### 11.6 METER

The body is exact `<8BIIqqqQ>` (48 bytes): foreground RGBA, background RGBA,
orientation (0 horizontal, 1 vertical), meter flags (bit 0 show numeric value),
minimum i64, maximum i64, initial value i64, and reserved u64 zero. Minimum is
strictly less than maximum and value lies inclusively within the range.

### 11.7 STATUS

The body is exact `<8BqIIQ>` (32 bytes): inactive RGBA, active RGBA, initial
i64 value, shape (0 circle, 1 square, 2 diamond), status flags zero, and
reserved u64 zero. Zero is inactive and every nonzero value is active.

### 11.8 PLOT

The body is exact `<Qqq8BII>` (40 bytes): same-owner series ID, minimum i64,
maximum i64, line RGBA, fill RGBA, plot flags, and reserved zero. Minimum is
less than maximum. Plot flag bit 0 fills to the minimum and bit 1 draws sample
points; other bits are zero. The oldest committed timestamp maps to the left
edge and newest to the right; intermediate X positions are linear in timestamp.
A single sample is centered. Values map linearly between the declared vertical
minimum and maximum and are clipped to the object bounds.

### 11.9 WAVEFORM

The body is exact `<Qqq8BqII>` (48 bytes): same-owner series ID, minimum i64,
maximum i64, trace RGBA, zero-line RGBA, zero-line value i64, waveform flags,
and reserved zero. Minimum is less than maximum and includes the zero-line
value. Flag bit 0 draws the zero line; other bits are zero.
Timestamp and value mapping is identical to PLOT.

## 12. Bounded i64 series

Series IDs are nonzero and strictly increasing within an owner generation.
Series history capacity consumes the transaction target's sample-slot usage at
DEFINE time. Active and hidden series usage are checked independently against
the same owner sample-slot reservation under Section 5. Committed history is a
bounded ordered ring; it is retained terminal state, not an unbounded telemetry
archive.

`SERIES_DEFINE <QQQIIQ>` (40 bytes) carries owner ID, generation, series ID,
history capacity, timestamp mode, and uniform interval microseconds. Capacity
is positive and no greater than `max_history_per_series`. Mode 0 is EXPLICIT
and requires interval zero. Mode 1 is UNIFORM and requires a positive interval.

SERIES_APPEND and SERIES_REPLACE share a 40-byte prefix `<QQQIIQ>`:

```text
u64 owner_id
u64 owner_generation
u64 series_id
u32 sample_count
u32 timestamp_mode
u64 first_timestamp_us
```

The mode must equal the series definition. Count is positive, no greater than
`max_samples_per_append`, and no greater than the series capacity. UNIFORM then
carries exactly `sample_count` signed i64 values; timestamp `i` is checked
`first_timestamp_us + i * uniform_interval_us`. EXPLICIT requires
`first_timestamp_us` zero and then carries exactly `sample_count` pairs
`<Qq>`: timestamp microseconds and signed i64 value.

Timestamps within a payload are strictly increasing. APPEND's first timestamp
must be greater than the currently newest timestamp. If committed existing
count plus append count exceeds capacity, exactly the oldest excess samples are
evicted atomically. REPLACE discards the prior history and installs only its
payload; it is used for replay and resynchronization. `SERIES_DROP <QQQ>` drops
an unreferenced series in the transaction's final graph and releases its
target-local sample-slot usage.

Timestamps use an owner-defined monotonic microsecond origin. The terminal
compares and maps values only within one series; it neither compares origins
across owners/series nor interprets them as wall time. A history longer than one
append maximum is replayed by DEFINE, a bounded initial REPLACE or APPEND, then
ordered APPEND chunks across hidden transactions. REPLACE is not an implicit
unbounded replay message.

The terminal draws committed samples only. It must not extrapolate from its
local clock, repeat the last sample as a new sample, interpolate a new stored
sample, or fabricate timestamps. Rendering may connect committed points
visually.

## 13. Display cadence

Logical commit and physical display are separate. Every successful commit
updates the retained model and global revision immediately. When CADENCE is
advertised, the terminal may delay or coalesce physical display so the
interval between retained display refreshes is at least
`minimum_presentation_interval_us`.

Coalescing may omit superseded intermediate property images on screen. It may
not omit ordered transactions, TX_RESULT, model revisions, owner/resource
lifecycle, series samples, or bounded-history eviction. A COMMIT_AND_REVEAL is
one atomic physical boundary with any mixed CELL replacement. Close and reset
may discard an unpresented model only under their explicit lifecycle rules.

Cadence is not permission for indefinite starvation. While renderer/service
polling continues, the terminal must display the newest pending committed view
at the first eligible display opportunity after the advertised minimum
interval. An intermediate physical view superseded before that opportunity may
be skipped. Protocol parsing, logical commits, result delivery, credit, reset,
and close continue independently of renderer eligibility.

Normalized interactive input may be published only against a physically
presented view carrying the event's current global revision. If cadence has a
newer logical view pending, the terminal presents that view at the next eligible
opportunity before releasing input, retaining/backpressuring bounded raw intent
in the meantime. It never labels input for a revision the user could not yet
have observed.

CADENCE defines no local-clock sample generation and provides no real-time
delivery guarantee. The UIDL retained backend chooses publication cadence
against the advertised minimum, while semantic series retain authoritative
sample timing in SERIES payloads.

## 14. Reset, resize, snapshot, and replay

### 14.1 Soft reset

The base soft-reset handshake is unchanged. On successful ACK, the terminal
increments `presentation_epoch`, drops all owners/resources/regions/objects/
series/hidden targets, and resets the global revision and transaction-ID scope
to zero. Directional sequences and cumulative credit do not restart; they
continue monotonically across soft reset exactly as required by APT-1 CELL-1.
Discovery must run again in the new epoch.

The base accepted/crossed-COMMIT settlement applies unchanged to
PRESENT_COMMIT. An accepted PRESENT commit and its TX_RESULT precede
construction of SOFT_RESET_REQUEST; a PRESENT_COMMIT crossed by an already
emitted request is discarded and settled by status 1 with unchanged revision
before ACK. This reset-only status is recoverable even for a mixed/CELL-including
PRESENT because the mandatory new-epoch CELL snapshot invalidates and rebuilds
the optimistic front.

The terminal must defer a locally planned SOFT_RESET_REQUEST while an accepted
OWNER_OPEN or RESOURCE lifecycle request still owes RET_RESULT, or while an
accepted OWNER_DROP still owes TX_RESULT. It first emits the exact old-epoch
result, clears that bounded lifecycle/result slot, and only then constructs the
reset request. A successfully accepted OWNER_DROP therefore advances revision
and emits status 0 before `last_revision` is read. Terminal-to-client sequence
order makes every such result arrive before the request.

A reset request may nevertheless cross a lifecycle request already emitted by
the client but not yet accepted by the terminal. If the client receives
SOFT_RESET_REQUEST while awaiting RET_RESULT or OWNER_DROP TX_RESULT, it holds
the reset pending and does not advance epoch or send ACK. The terminal consumes
that one already-issued old-epoch request, emits its ordered old-epoch result,
and accepts no new lifecycle request except the matching RESOURCE_ABORT required
below. The client consumes the result, performs any resulting resource-upload
abort, and only then sends the new-epoch ACK. No RET_RESULT or TX_RESULT may
remain outstanding or cross that ACK.

A crossed OWNER_DROP is settled by the reset-only status 1 rule in Section 6,
never status 0: applying it would advance revision beyond the already-emitted
request's `last_revision`. The exact binding/model/reservation remains unchanged
until ACK retires the old epoch. The broker consumes status 1 and proceeds to
ACK without retrying that drop in the old epoch.

The same hold applies when an emitted RESOURCE_CHUNK has no definitive outcome
yet. The terminal consumes that complete old-epoch chunk and resolves it in
exactly one of two ways before reset may advance: an accepted chunk is followed
by a CREDIT whose cumulative watermark covers all client ordinary bytes through
that complete chunk, while a rejected chunk is followed by its RESOURCE_CHUNK
RET_RESULT. For a rejected chunk, that RET_RESULT must precede any CREDIT
watermark that covers the rejected frame, so the client cannot mistake reclaimed
byte credit for acceptance. The client compares the echoed result tuple with its
stored exact upload tuple.

After covering CREDIT for an accepted exact chunk, the client sends
RESOURCE_ABORT for the actual upload in the old epoch and waits for its
RET_RESULT. After an exact-tuple RESOURCE_CHUNK RET_INVALID, Section 8 has
already destroyed that upload, so the client sends no abort. A wrong-owner/
generation RET_STALE_OWNER or same-owner wrong-resource RET_INVALID preserves
the unrelated actual upload; the client therefore sends RESOURCE_ABORT naming
that stored exact tuple and waits for its RET_RESULT before ACK. If no actual
upload was open, there is nothing to abort. The client sends SOFT_RESET_ACK only
after the applicable path completes; no chunk outcome, required abort, or
RET_RESULT may cross the ACK.

Apart from the pending-chunk case above, if a resource upload is open when
SOFT_RESET_REQUEST arrives, the client first sends RESOURCE_ABORT in the old
epoch and waits for its old-epoch RET_RESULT. It then follows the base
transaction-abort rule, if applicable, and sends SOFT_RESET_ACK in the new
epoch. No upload byte or result crosses the ACK.

The first client data message after reset remains the mandatory CELL-1
`SNAPSHOT_BEGIN`; its successful commit establishes revision one. Only after
that TX_RESULT does the client send RET_QUERY. If RETAINED-1 is rediscovered,
the broker then reopens owners, reuploads needed resources, and builds a hidden
retained replacement using RET_REPLACE_START/CONTINUE. Only a validating
COMMIT_AND_REVEAL makes retained content visible. Until reveal, the committed
CELL snapshot is the complete visible terminal output. The exact order is CELL
snapshot/result, discovery, owner open, resource replay, retained hidden
replace, reveal.

### 14.2 Resize

An accepted RESIZE selects new geometry and generation under the base host-port
contract. The client first commits an exact CELL replacement at that geometry
using PRESENT_BEGIN CELL_REPLACE; legacy SNAPSHOT_BEGIN is invalid because the
global revision is nonzero. The successful PRESENT_COMMIT increments that
revision rather than assigning one. This transaction may also use
RET_LAYOUT_START, but it must commit without reveal so the CELL replacement is
the complete visible fallback. Later RET_LAYOUT_CONTINUE transactions replace
every surviving region for the accepted geometry generation.
COMMIT_AND_REVEAL atomically exposes the relaid-out retained plane, optionally
mixed with a final CELL delta.

The terminal must never scale stale cell-anchored regions as an implicit resize
policy. A second resize before reveal discards the hidden layout target and
requires the sequence above for the newest generation. Latest-wins host intent
does not authorize half admission of wire RESIZE and geometry.

### 14.3 Drop and replay

Owner drop is exact-generation, idempotent, revisioned, and terminal-acknowledged
as specified in Section 6. A broker restart, soft reset, hard reset, or detach
does not infer that prior wire authority is reusable. Replay allocates bindings
for the current session/epoch and uses generations newer than any same-epoch
tombstone.

Hard reset or detach destroys the whole retained plane. Structural session loss
instead freezes the authoritative protocol model and makes it unusable for any
further wire mutation; all authority and allocations remain quarantined. The
host retains the last published immutable CELL/retained view and its referenced
backing for display/diagnosis until coordinated hard reset, detach, or a valid
close boundary retires it. As in CELL-1, structural loss does not authorize
binary-to-ANSI fallback and does not release quarantined quota for reuse.

## 15. Validation and failure classes

Header, length, CRC, sequence, session, epoch, credit, and frame-boundary faults
remain fatal under APT-1. A retained sender must not use ERROR effects to
downgrade them.

A well-framed lifecycle request is completed by RET_RESULT. A well-framed
transaction semantic error is completed by nonzero TX_RESULT and leaves active
state unchanged. This includes stale revision/geometry, bad exact counts or
declared bytes, quota overflow, unsupported object family, invalid final graph,
wrong authority, and hidden-mode misuse.

Outside the base reset-settlement exception, RETAINED-1 defines exactly two
recoverable nonzero-result cases. First, a retained-only PRESENT transaction
(`cell_mode = CELL_NONE`) is recoverable when its broker retained the complete
authoritative desired request: it may
reconcile or retry from the unchanged revision and committed hidden/active
model. Second, OWNER_DROP status 2 or 3 is recoverable under Section 6 because
the broker retains the exact authoritative binding and never optimistically
removes the owner model or reservation. Only OWNER_DROP status 0 tombstones and
releases that authority. A mixed or CELL-including PRESENT rejection has the
base optimistic-front-buffer failure semantics and makes the session unusable
for deltas; it requires synchronized close or coordinated hard reset. Legacy
CELL/SNAPSHOT rejection and every other nonzero TX_RESULT are governed by the
base failure rule. The only OWNER_DROP status 1 exception is Section 14.1's
crossed-reset settlement, which proceeds directly to ACK rather than resuming
old-epoch work. No client may assume a partially accepted delta.

Receiving a retained family before successful discovery, a lifecycle frame
while another lifecycle result is outstanding, or a nontransaction mutation
frame is a state error. Unknown reserved opcodes with bit 15 clear are mandatory
and must not be skipped as future behavior. The full `8000..ffff` range has the
base optional-skip semantics.

## 16. Minimum Akashic UIDL conformance journey

A production RETAINED-1 qualification must use the real CELL-1 implementation,
real retained model, caller-provided capacities, and actual UIDL/UCTX lifecycle.
The minimum Akashic journey is:

1. negotiate APT-1 CELL-1 and reach ACTIVE;
2. commit the initial CELL snapshot and receive its successful TX_RESULT;
3. discover RETAINED-1 and validate both fixed replies before covering CREDIT;
4. attach one UCTX through the internal retained backend and open its bounded
   projection owner without exposing that owner to application code;
5. build the initial hidden replacement with RET_REPLACE_START followed by at
   least one separate RET_REPLACE_CONTINUE transaction, then reveal regions plus
   at least one polyline, label, readout, meter, status, bounded series, plot,
   and waveform;
6. after RETAINED-1 is enabled, commit a real legacy TX_BEGIN/CELL_SPAN/CURSOR/
   TX_COMMIT delta and prove it shares the global transaction-ID and revision
   domain with the surrounding PRESENT commits;
7. append explicit and uniform i64 samples without local-clock extrapolation;
8. deliver one normalized input tied to the global revision;
9. accept a resize and transmit a real PRESENT CELL_REPLACE using exactly the
   canonical full-width row spans; then use RET_LAYOUT_START followed by at
   least one separate RET_LAYOUT_CONTINUE transaction before reveal at the new
   geometry generation;
10. perform a soft reset, CELL snapshot, discovery/owner replay, and a hidden
    RET_REPLACE_START plus RET_REPLACE_CONTINUE replay before reveal;
11. submit two individually valid OWNER_OPEN quota requests whose aggregate
    exceeds one advertised global total and observe RET_NO_CAPACITY with the
    prior ledger unchanged; and
12. drop the owner and complete synchronized close to unchanged ANSI.

The journey must exercise ordinary-credit backpressure, the 4096-byte control
reserve, a rejected over-quota lifecycle request, transaction bytes retained
through commit/abort processing plus the separate post-COMMIT TX_RESULT gate,
and one hidden multi-transaction rebuild.
Image upload/digest qualification is required before advertising RGBA_IMAGE but
is not a prerequisite when that optional bit is clear.

## 17. Control reserve lifecycle

The base 4096-byte control reserve is shared, not multiplied by this profile.
When RETAINED-1 is enabled, the fixed frames `RET_RESULT`, `OWNER_DROP`, and
`RESOURCE_ABORT` join the base allowlist. They may use reserve only for their
exact fixed payloads above and only to terminate or advance the matching
bounded lifecycle. RESOURCE_BEGIN/CHUNK/COMMIT/DROP, discovery replies,
PRESENT transactions, object/region/series operations, and arbitrary
ERROR detail remain ordinary-credit traffic.

Control-reserve occupancy is reclaimed when the corresponding complete frame is
consumed in order under its bounded lifecycle. Those bytes are never ordinary
`released_bytes`, never increase a CREDIT watermark, and never consume ordinary
credit in the first place. A transaction's ordinary bytes remain charged until
commit or abort releases staging; after COMMIT, the separate TX_RESULT gate
still blocks another BEGIN. An upload chunk remains ordinary charged data until
covering CREDIT. Reserve cannot be borrowed to continue bulk upload, retained
replay, or ordinary mutation. Close/reset/error traffic keeps priority over
retained lifecycle traffic where the finite reserve cannot hold every candidate
simultaneously.

## 18. Conformance artifacts

Implementations must share byte-exact vectors for:

- positive and negative discovery, including reply-before-covering-CREDIT;
- every fixed payload and every variable body family;
- a legacy TX_BEGIN/TX_COMMIT after retained enablement, proving global
  transaction-ID and revision interleaving between CELL and PRESENT;
- mixed CELL/retained commit and byte/count preflight;
- a real resize PRESENT CELL_REPLACE with exactly `rows` canonical full-width
  spans, rather than a preconditioned state or legacy SNAPSHOT;
- multi-transaction RET_REPLACE_START/RET_REPLACE_CONTINUE and
  RET_LAYOUT_START/RET_LAYOUT_CONTINUE journeys using numeric modes 2/3 and 4/5;
- owner quota reservation, an aggregate RET_NO_CAPACITY from individually valid
  requests, tombstone, stale generation, and idempotent drop;
- good and bad SHA3-256 upload, ordered chunks, exact-upload abort/destruction,
  wrong-tuple CHUNK/COMMIT preservation, and in-use drop;
- object graph/reference validation and exact typed-body lengths;
- explicit/uniform series append, ring eviction, replace, and timestamp errors;
- resize layout hiding/reveal and reset CELL-first replay, including a valid
  crossed CELL/PRESENT COMMIT and crossed OWNER_DROP each settled by old-epoch
  TX_RESULT status 1 with unchanged revision before the new-epoch ACK; and
- reserve exhaustion proving bulk retained frames cannot consume control bytes.

Vectors must include complete 40-byte headers, CRC-32C, directional sequence,
session, epoch, expected credit watermark, expected global revision, visible
CELL state, active/hidden retained state, quota ledger, and result status. The
quota state must distinguish immutable owner reservation, active scene usage,
hidden scene usage, and owner-wide resource count/byte usage; a single combined
`used` total is nonconforming. A parser-only success is not a conformance
success.

Every transcript has a machine-readable expected-state sidecar containing an
ordered state record after each consumed frame. The manifest names both files.
At minimum each record carries directional sequences and ordinary sent/released/
grant counters, separate per-direction control-reserve occupancy,
`presentation_epoch`, model revision, and transaction-ID high-water,
open/result/upload lifecycle,
selected geometry/generation, visible CELL digest,
active and hidden retained digests/mode, immutable owner reservations, separate
active and hidden scene usage, owner-wide resource usage, live/tombstone ledger,
and the emitted result/status. An independently implemented deterministic state
reducer consumes the decoded transcript plus declared initial state and derives
those records for comparison. The transcript encoder/generator must not also be
the source of expected reducer state, and hand-authored terminal-only end-state
assertions do not satisfy this requirement.
