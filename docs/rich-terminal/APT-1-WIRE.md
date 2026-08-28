# APT-1 wire and CELL-1 profile

Contract ID: `APT-1-CELL-1-2026-08-24`

Status: normative for the first APT rich-terminal milestone.

The key words MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
and MAY are normative. Multi-byte integers are unsigned little-endian unless a
field is explicitly signed.

## 1. Scope

APT-1 is an optional, negotiated rich-terminal protocol over the existing
ordered MegaPad UART byte stream. CELL-1 is its first mandatory profile. It
provides transactional styled cells and normalized terminal input; retained
resources, plots, images, controls, and animation are not part of the mandatory
CELL-1 profile. The in-place additive RETAINED-1 contract defines selected
families only after its separate deterministic discovery succeeds.

APT-1 selects a mode of one terminal session. It does not create, address, or
expose an application presentation, document, scene service, broker, or
independently mutable UI object. The protocol-visible CELL state and any
additive retained records are terminal-owned output materialization derived
from a client's authoritative state. A client sends atomic terminal-state
updates; a successful commit revises that one materialization.

`PRESENT_BEGIN`, `PRESENT_COMMIT`, `presentation_epoch`, and other identifiers
containing `presentation` are frozen wire spellings. In this contract they name
only an update family, an epoch, or a physical display boundary. They do not
name a first-class presentation object or grant application authority.
Implementations MUST NOT use those spellings to expose APT as an application
broker, scope, document, or scene-mutation service.

ANSI remains the baseline terminal contract. APT-1 is initiated only by the
separately loaded `rich-terminal.f` userland module after an explicit
system composition selects it. KDOS does not contain or require APT-1.
An ANSI-only emulator frontend or physical terminal is a supported target.

## 2. Transport and ownership

The underlying byte transport is ordered and lossless within negotiated
bounds. UART publication boundaries have no framing meaning. A frame may be
split across any number of UART publications, and one publication may contain
any number of frames.

Before negotiation and after fallback, ANSI owns both directions. During a
successful APT session, APT exclusively owns both directions. ANSI bytes,
terminal replies, and framed bytes are never multiplexed in `ACTIVE`.

The implementation MUST preserve ordinary input bytes received before the
enhanced switch boundary. Binary bytes from a failed enhanced session MUST be
discarded and MUST NOT be passed to an ANSI escape or key parser.

## 3. ANSI-safe negotiation

`ESC` is byte `1B`, OSC begins `ESC ]`, and string terminator `ST` is `ESC \\`.
APT-1 uses private OSC selector `9999`; this avoids the standardized xterm
`DCS +q`/`DCS 1+r` termcap controls. Existing MegaPad ANSI handling absorbs
unknown OSC commands through `ST`, and an ANSI-only terminal may ignore the
selector without switching modes. BEL termination is not accepted for APT.

All negotiation fields are uppercase ASCII hexadecimal with the exact widths
shown. Separators are ASCII `;`. No spaces are permitted.

The client chooses a nonzero 64-bit nonce and sends:

```
ESC ] 9999;APT1;P;<nonce:16>;CELL1 ESC \
```

A willing terminal chooses a nonzero 64-bit session ID and replies:

```
ESC ] 9999;APT1;O;<nonce:16>;<session:16>;<max-payload:8>;<max-transaction:8>;<terminal-rx-credit:8>;<cols:4>;<rows:4>;CELL1 ESC \
```

The nonce MUST match. `max-payload` is at most `00100000`. It MUST admit both
the 32-byte mandatory READY payloads and one complete maximum-width row span
payload:

```
max-payload >= max(32, 12 + 8 * cols)
```

The mandatory full snapshot budget is:

```
snapshot-bytes = 176 + rows * (52 + 8 * cols)
```

`max-transaction` MUST be at least `snapshot-bytes`, and
`terminal-rx-credit` MUST be at least `max-transaction`. All arithmetic is
checked before accepting the offer. `cols` and `rows` are the terminal's
current selected geometry and are positive. Offers with invalid or unsupported
values are ignored as if no offer arrived.

The client accepts with a positive receive-credit grant:

```
ESC ] 9999;APT1;A;<nonce:16>;<session:16>;<client-max-payload:8>;<client-rx-credit:8>;CELL1 ESC \
```

`client-max-payload` is positive and at most `00100000`. The exact switch
boundary is the final byte of this `ST`:

* the terminal consumes the `OPEN` terminator as ANSI, then treats the next
  client byte as the first framed byte;
* the client emits the terminator, then treats the next terminal byte as the
  first framed byte;
* neither side sends ordinary ANSI after that boundary; and
* terminal input events are held until the ready exchange completes.

The terminal sends `SERVER_READY` as terminal sequence zero. The client
validates it and sends `CLIENT_READY` as client sequence zero. The client may
append its first transaction immediately after `CLIENT_READY`. The terminal
enters `ACTIVE` only after accepting `CLIENT_READY`; the client enters
`ACTIVE` after accepting `SERVER_READY`.

The client waits 250 milliseconds for a matching offer and may send at most
three probes with the same nonce. It waits 250 milliseconds for
`SERVER_READY` after `OPEN`. Before `OPEN`, timeout, refusal, malformed
negotiation, or an ignored probe directly restores the previous ANSI input
owner and is reported as `UNSUPPORTED`, not as an application failure.

After `OPEN`, timeout cannot locally restore ANSI because the terminal may
already own the binary stream. The client sends framed `CLOSE` using its next
sequence and completes `CLOSE_ACK`; if that cannot complete within another
250 milliseconds, the host attachment/link epoch MUST be hard-reset and its
queues drained before ANSI is allowed to consume again. Loading the module
alone emits no probe.

The exact probe, offer, and accept grammar above contains no baud-rate field
and this contract does not authorize an implicit rate change at `OPEN`. On a
physical MegaPad UART, reset, ANSI fallback, and this negotiation remain at the
baseline 115,200 baud. A future optional 1,000,000-baud profile must be an
explicitly discovered and accepted extension after the real BIOS-to-RTL TX
path works at the baseline rate. Its switch exchange remains at 115,200 until
the final acknowledged switch boundary; both endpoints then apply the selected
rate only after the transmitting FIFO and shift register are idle. Framed
close completes at the active rate and returns both endpoints to 115,200 at an
equally explicit idle boundary. Link reset always restores 115,200. A timeout
or ambiguous switch requires hard link reset rather than guessing which rate
owns the stream.

## 4. Session states

Both implementations expose these conceptual states:

| State | Meaning |
| --- | --- |
| `ANSI` | Legacy ownership; no APT frame is accepted. |
| `PROBING` | Client probe or terminal offer is pending. |
| `OPENING` | `OPEN` crossed the switch boundary; ready exchange pending. |
| `ACTIVE` | Framed traffic and CELL-1 transactions are legal. |
| `RESYNCING` | Soft cache reset accepted; only reset control and one replacement snapshot are legal. |
| `CLOSING` | A complete close/error frame ended ordinary traffic; acknowledgement may be pending. |
| `LOST` | The framed session is unusable, but binary ownership remains quarantined until an outer hard-reset-and-drain boundary. |

Hard machine reset or attachment replacement destroys the session, advances
the outer attachment epoch, drains both directions, and returns to `ANSI`.
An opening timeout after `OPEN` or a fatal framing error instead enters
`LOST`; it cannot locally prove an ANSI-safe byte boundary. A soft
terminal-output cache reset stays inside the framed session and enters
`RESYNCING`.

## 5. Frame format

Every frame has this fixed 40-byte header followed by `payload_length` bytes:

| Offset | Size | Field |
| ---: | ---: | --- |
| 0 | 4 | Magic `A5 50 54 31`. |
| 4 | 1 | Reserved, zero. |
| 5 | 1 | Header size, exactly `40`. |
| 6 | 2 | Message type. |
| 8 | 2 | Flags, zero in APT-1. |
| 10 | 2 | Reserved, zero. |
| 12 | 4 | Payload length. |
| 16 | 8 | Negotiated session ID. |
| 24 | 8 | Directional sequence number. |
| 32 | 4 | Presentation epoch. |
| 36 | 4 | CRC-32C. |

The structural payload maximum is 1,048,576 bytes. The negotiated maximum may
be smaller. Implementations MUST reject a larger length from the header before
allocating or waiting for its payload.

CRC-32C uses the Castagnoli polynomial. The reflected polynomial is
`82F63B78`, initial value is `FFFFFFFF`, input and output are reflected, and
the final XOR is `FFFFFFFF`. It covers header bytes 0 through 35 followed by
the payload; the CRC field itself is excluded.

In `OPENING` and `ACTIVE`, magic is required exactly where the next frame
begins. Bad magic, header size, flags, nonzero reserved fields, length, CRC,
session, sequence, or presentation epoch is fatal to that session. An
implementation may scan for magic to bound diagnostic discard, but it MUST
NOT resume the damaged session. Discarded binary is never reinterpreted as
ANSI.

## 6. Freshness and ordering

There are three distinct freshness scopes:

1. the MegaPad host attachment epoch, which is outside the wire protocol;
2. the 64-bit APT session ID in every frame; and
3. the 32-bit presentation epoch in every frame.

Each direction has an independent 64-bit sequence. It starts at zero with its
ready frame and increases by exactly one for every frame, including control
frames. Duplicate, missing, reordered, or wrapped sequences are fatal. A side
MUST close before its next sequence would wrap.

The wire `presentation_epoch` starts at zero. It changes only through the soft
reset exchange. Model revisions and transaction IDs are scoped to that epoch
and do not provide authorization. An enabled additive rich-terminal
profile may define additional transaction families only by sharing this one
transaction-ID and model-revision domain; it may not create a parallel commit
clock. `APT-1-RETAINED-1-2026-08-24` uses that extension rule.

## 7. Credit and bounded admission

Credit is a cumulative grant measured in complete frame bytes, including the
40-byte header. Initial grants come from `OFFER` and `OPEN`. `CREDIT` carries a
64-bit cumulative total. Grants and sent-byte counters never wrap and never
decrease.

For either direction, let `initial_grant` be the corresponding value from
OFFER/OPEN and let `released_bytes` be the receiver's cumulative count of
complete ordinary frame bytes whose bounded storage has been released. Every
CREDIT payload is exactly the checked sum
`initial_grant + released_bytes`. Release accounting is ordered and never
includes a frame prefix or control-reserve frame. A sender matches a release
watermark against this sum, not against its currently unused send allowance.

A data frame may be sent only when:

```
sent_data_bytes + complete_frame_bytes <= granted_data_bytes
```

The receiver releases transient frame capacity by increasing its grant. Bytes
belonging to an open transaction remain charged until commit or abort releases
the staging state. A sender MUST preflight the complete declared transaction
against current credit and `max-transaction` before sending its begin frame.
Transaction size is the sum of every complete frame from begin through commit,
including headers; a failed preflight emits no begin frame.

Each peer additionally reserves 4,096 bytes for `CREDIT`, `ERROR`, `CLOSE`,
`CLOSE_ACK`, `SOFT_RESET_REQUEST`, `SOFT_RESET_ACK`, `TX_ABORT`, and
`TX_RESULT`, as well as the single expected `SERVER_READY`/`CLIENT_READY`
exchange. Those types do not consume ordinary data credit. Their payloads are
limited by this document, unexpected duplicates are errors, and they cannot
be used to carry extension data. Ordinary input and output-update frames cannot
consume the reserve.

An enabled additive profile may add fixed lifecycle frames to this same reserve
only when its contract names each exact type, direction, payload length, and
termination purpose. It does not enlarge the 4,096-byte reserve. RETAINED-1
adds only `RET_RESULT` (`000a`), `OWNER_DROP` (`000b`), and `RESOURCE_ABORT`
(`000c`) under `APT-1-RETAINED-1.md` Section 17. Its discovery, resource data,
PRESENT, object, region, series, semantic-control, and `CONTROL_EVENT` frames
remain ordinary data.

Credit exhaustion is backpressure, not loss. The sender retains the exact
unsent frame/transaction and makes no sequence or model progress.

## 8. Capability and ready messages

`SERVER_READY` (`0001`, terminal to client) has the 32-byte payload:

```
u32 reserved                = 0
u32 terminal_receive_max_payload
u32 max_transaction_bytes
u32 terminal_receive_credit
u32 current_cols
u32 current_rows
u64 capabilities
```

`CLIENT_READY` (`0002`, client to terminal) also occupies 32 bytes but has
direction-specific fields:

```
u32 reserved                    = 0
u32 client_receive_max_payload
u32 reserved                   = 0
u32 client_receive_credit
u32 max_text_event_bytes
u32 reserved                   = 0
u64 capabilities
```

Capability bits are:

| Bit | Capability |
| ---: | --- |
| 0 | Styled CELL-1 spans and transactions. |
| 1 | Key input. |
| 2 | UTF-8 text input. |
| 3 | Cell-coordinate pointer input. |
| 4 | Resize input. |
| 5 | Soft terminal-output reset and replacement snapshot. |

Bits 0 through 5 are mandatory for this contract ID. Other bits are zero.
`SERVER_READY` repeats the terminal values from `OFFER`. `CLIENT_READY`
repeats `OPEN.client-max-payload` and `OPEN.client-rx-credit` and supplies the
client's bounded text-event limit. A repeated value mismatch is a fatal
opening error. `max_text_event_bytes` is positive and no greater than
`client_receive_max_payload - 12`, using checked arithmetic.

`CREDIT` (`0003`, either direction) contains one `u64 cumulative_grant`.

## 9. CELL-1 transactions

Exactly one non-nested transaction may be open per session. Mutation messages
outside one are transaction errors. A transaction ID is nonzero, monotonically
increases within the presentation epoch, and never wraps.

`TX_BEGIN` (`0100`, client to terminal) payload:

```
u64 transaction_id
u64 base_revision
u32 cols
u32 rows
u32 span_count
u32 cell_count
```

`base_revision` MUST equal the terminal's current revision. Geometry MUST
equal the current model geometry. Declared counts are exact. Spans are
row-major, non-overlapping, and inside the geometry.

`CELL_SPAN` (`0101`, client to terminal) payload begins:

```
u32 row
u32 column
u32 count
CELL cells[count]
```

`count` is positive, `column + count` uses checked arithmetic, and the result
is at most `cols`. A CELL is exactly eight bytes:

```
u32 Unicode scalar
u8  xterm-256 foreground index
u8  xterm-256 background index
u16 attributes
```

`CURSOR` (`0102`, client to terminal) payload is:

```
u32 row
u32 column
u8  visible
u8  reserved[7] = 0
```

Exactly one cursor message occurs in every transaction. `visible` is zero or
one. A visible cursor is in bounds.

`TX_COMMIT` (`0103`, client to terminal) contains its `u64 transaction_id`.
The terminal verifies the ID and exact declared counts, then atomically applies
all staged spans and cursor state and increments the 64-bit model revision by
one. It sends `TX_RESULT`. No partial mutation is visible. The client permits
only one locally accepted but unacknowledged transaction; it sends no later
begin until this result arrives.

`TX_ABORT` (`0104`, client to terminal) contains `u64 transaction_id`, `u16
reason`, and six zero bytes. It atomically discards staging and is idempotent
for the most recently aborted transaction ID.

Message IDs `0105` and `0106` reserve optional fill and scroll operations.
Their payloads are intentionally undefined and they MUST NOT be sent under
this contract ID. Styled spans are the complete mandatory update mechanism.

## 10. Replacement snapshots

`SNAPSHOT_BEGIN` (`0110`, client to terminal) has the same 32-byte payload as
`TX_BEGIN`. `base_revision` MUST be zero. Its spans MUST cover every cell
exactly once in row-major order without gaps or overlap. Exactly one cursor
message follows the spans.

`SNAPSHOT_COMMIT` (`0111`, client to terminal) contains its `u64
transaction_id`. Snapshot geometry MUST equal the terminal's current selected
geometry. Successful commit atomically replaces the CELL-1 model allocation,
cells, and cursor and sets model revision to one; it does not resize the
terminal. It produces `TX_RESULT` exactly like a normal commit. No normal
transaction may interleave, and the client waits for that result before a
later begin.

The initial visible CELL-1 state is a replacement snapshot. A soft reset also
requires a replacement snapshot; demo strings or implicit blank terminal state
do not satisfy the profile.

## 11. Cell semantics

Coordinates are zero-based. Invalid bounds reject the whole transaction;
there is no clipping. Rectangle endpoint semantics are not needed by mandatory
CELL-1 messages.

The scalar MUST be a Unicode scalar value and MUST NOT be a surrogate.
Akashic applies its `CW-CELL-CP` width-one projection before encoding. Native
codepoint zero is encoded as U+0020. A terminal renders exactly one physical
cell per atom and clips glyph drawing to that cell.

Attribute bits are independent of either implementation's native cell bits:

| Bit | Meaning |
| ---: | --- |
| 0 | Bold. |
| 1 | Dim. |
| 2 | Italic. |
| 3 | Underline. |
| 4 | Blink. |
| 5 | Reverse. |
| 6 | Strike. |

Bits 7 through 15 are zero. Wide, continuation, hidden, and implementation
private flags are not transmitted. Colors use the xterm 256-color palette;
foreground 7 and background 0 are the canonical blank defaults.

## 12. Input messages

Input messages travel terminal to client in `ACTIVE` and carry the terminal's
current model revision where specified. The client still validates focus,
modal authority, owner/control identity, operands, and text limits before
dispatch. Terminal-provided identity is routing/freshness data, not authority.

`KEY` (`0200`) payload (`<IBBHQ`):

```
u32 key_symbol
u8  action       (1 press, 2 repeat, 3 release)
u8  location     (0 standard, 1 left, 2 right, 3 keypad)
u16 modifiers
u64 model_revision
```

Modifier bits are Shift 0, Ctrl 1, Alt 2, Super 3, Caps Lock 4, and Num Lock
5. A printable key symbol is its Unicode scalar. Named symbols begin at
`00110000`: Backspace `00110001`, Tab `00110002`, Enter `00110003`, Escape
`00110004`, Insert `00110005`, Delete `00110006`, Home `00110007`, End
`00110008`, Page Up `00110009`, Page Down `0011000A`, Left `0011000B`, Right
`0011000C`, Up `0011000D`, Down `0011000E`, and F1 through F12 at `00110020`
through `0011002B`.

`TEXT` (`0201`) begins with `u16 flags`, zero `u16 reserved`, and `u64
model_revision`; the remainder is nonempty, well-formed UTF-8. Flag bit zero
means bracketed paste. Other bits are zero.

`POINTER` (`0202`) payload (`<iiHHHHhhQ`): signed cell `x`, signed cell `y`,
current buttons, changed buttons, modifiers, kind, signed horizontal wheel
steps, signed vertical wheel steps, and model revision. Button bits are left
0, middle 1, right 2, X1 3, and X2 4; other bits are zero. `changed_buttons`
is the XOR transition mask relative to the prior event. Kinds are 1 move, 2
press, 3 release, and 4 wheel. Wheel fields are zero for other kinds; for kind
4 they are logical detents with positive X right and positive Y down and may
carry magnitude. Out-of-bounds positions may report pointer exit but cannot
directly target an application object.

`RESIZE` (`0203`) payload (`<IIQ`) contains positive cols, positive rows, and
a monotonically increasing terminal geometry generation. During `ACTIVE`,
this is the authoritative geometry event and legacy MMIO notification is
reconciled rather than dispatched again. A terminal defers resize publication
while a transaction is open. Before sending it, the terminal verifies that a
full snapshot at the new dimensions satisfies the negotiated maximum payload,
transaction, and credit bounds. After the client accepts resize, normal deltas
stop until a replacement snapshot matching that geometry commits, or until an
enabled additive profile's explicitly defined replacement transaction commits.
RETAINED-1 uses PRESENT CELL_REPLACE and does not authorize legacy
SNAPSHOT_BEGIN after its discovery.

`FOCUS` (`0204`) payload is `u8 focused`, seven zero bytes, and `u64
model_revision`. `focused` is zero or one.

`CONTROL_EVENT` (`0205`) is defined only when the additive RETAINED-1 profile
has been successfully discovered with feature bit 8 `RET_CONTROLS`. Its exact
40-byte payload is `<QQQHHIQ>`:

```text
u64 owner_id
u64 owner_generation
u64 control_id
u16 event_kind             (1 = ACTIVATE)
u16 modifiers
u32 reserved               = 0
u64 model_revision
```

All other event-kind values are invalid in the first control slice. Modifier
bits are Shift 0, Ctrl 1, Alt 2, Super 3, Caps Lock 4, and Num Lock 5; all
other bits are zero. The identity is normalized routing and freshness data,
not application authority. The terminal may emit ACTIVATE only for an exact
current active CONTROL record whose kind is activatable and whose complete
ancestor chain is visible and enabled under the canonical RETAINED-1 menu
rules.

`model_revision` must be the exact current global revision of the complete
composite containing the renderer-hit-tested control, after that same revision
was physically presented and acknowledged. A hidden, superseded, disabled,
invisible, or not-yet-acknowledged control cannot produce this event. If a
newer logical revision awaits display, the terminal retains/backpressures
bounded raw intent until that exact current revision is presented or the intent
becomes stale. The terminal does not mutate control state when it emits the
event; the client revalidates the tuple and revision and routes activation to
its authoritative UI model.

## 13. Reset and close

`SOFT_RESET_REQUEST` (`0007`, terminal to client) contains `u32
requested_epoch`, four zero bytes, and `u64 last_revision`. It is legal only
in `ACTIVE`, requests exactly current epoch plus one, and means the terminal
discarded the terminal output cache but retained the framed session. After sending
it, the terminal sends no ordinary input or data frame until resynchronization
completes.

`last_revision` MUST equal the client's last successfully acknowledged model
revision. Because results and reset requests share the ordered terminal-to-
client sequence, a mismatch proves inconsistent session state and is fatal.
If the current presentation epoch is `FFFFFFFF`, soft reset is unavailable;
the terminal performs synchronized close and negotiates a new session instead.

Before constructing a locally planned SOFT_RESET_REQUEST, the terminal MUST
settle every complete COMMIT it has accepted. It applies a successful commit,
emits its old-epoch TX_RESULT, and only then reads the resulting revision for
`last_revision` and emits the request. Thus an accepted commit and its result
are ordered before the request; the request never advertises a revision that an
already accepted commit can later advance.

On receiving the request, the client stops new old-epoch work. If a transaction
is genuinely open and the client has not emitted COMMIT, it sends `TX_ABORT` in
the old epoch. If COMMIT has been emitted and its TX_RESULT is outstanding, the
client instead holds the reset request and sends neither TX_ABORT nor ACK. A
terminal which already emitted SOFT_RESET_REQUEST before accepting an otherwise
valid crossed COMMIT MUST consume the complete COMMIT, discard its staging, and
emit TX_RESULT for that transaction ID with status 1 and the unchanged revision
equal to the request's `last_revision`. The result header remains in the old
presentation epoch. The client consumes that result and only then continues the
reset. This reset-settlement status 1 is expected cancellation, not a semantic
rejection that enters SESSION_LOST. Structural or semantic invalidity is not
downgraded by reset crossing. No TX_RESULT may cross the new-epoch ACK.

Ordered delivery places all prior client frames and any required abort or
result settlement before the acknowledgement. The client then advances to the
requested epoch, resets its transaction ID and model revision scopes, and
replies with
`SOFT_RESET_ACK` (`0008`): `u32 requested_epoch`, `u16 status`, and zero `u16
reserved`. Status MUST be zero. The acknowledgement header uses the new
presentation epoch. The next data message MUST be `SNAPSHOT_BEGIN` in the new
epoch. Directional frame sequences do not restart.

The acknowledgement is the sole frame allowed to use the requested epoch
while its receiver still expects the prior epoch. The terminal continues to
parse any preceding old-epoch frames in sequence but does not make their
staged terminal output visible. Receipt of the acknowledgement discards any such
staging and atomically changes its expected epoch. A client unable to accept a
valid mandatory reset sends `CLOSE` in the old epoch instead of an
acknowledgement.

Hard machine reset, UART detach/reconnect, host attachment replacement, or
hard terminal reset destroys the session, flushes epoch-tagged queues, and
returns to ANSI negotiation. It is not a soft reset.

If RETAINED-1 is enabled, the mandatory CELL snapshot remains the first data
message after soft reset and becomes visible before any retained replay. The
retained profile then uses its hidden replace/reveal lifecycle; it does not
weaken or reorder this CELL-1 recovery boundary.

An enabled additive lifecycle profile may require outstanding fixed results to
settle before the soft-reset acknowledgement. Those results remain old-epoch
ordered control traffic and must precede the new-epoch ACK as defined by that
profile; directional sequence and cumulative credit do not restart.

`CLOSE` (`0005`) contains `u16 reason`, six zero bytes, and `u64
last_revision`. `CLOSE_ACK` (`0006`) contains the echoed reason and six zero
bytes. After the complete close/ack boundary, both sides release enhanced
ownership and ANSI is again the only accepted stream.

## 14. Results and errors

`TX_RESULT` (`0009`, terminal to client) payload is:

```
u64 transaction_id
u16 status       (0 committed, 1 aborted, 2 invalid, 3 stale revision)
u16 reserved = 0
u64 model_revision
```

A nonzero result leaves the prior visible model unchanged. Akashic may advance
its front buffer after local transport acceptance rather than waiting for this
result; except for Section 13's reset-settlement status 1 or an enabled additive
profile's exact authoritative-state exception, a nonzero result therefore makes
the session unusable for deltas and requires synchronized framed close or a
hard attachment reset before ANSI fallback. An additive exception must name the
operation and statuses, require that the sender retain authoritative desired
state until success, and leave revision, authority, and committed model
unchanged. RETAINED-1 defines narrow exceptions for retained-only PRESENT and
`OWNER_DROP` statuses 2 and 3. The module reports `SESSION_LOST` before
dispatching another application event for every result still governed by the
base rule. A pending successful result makes a new transaction begin return
`WOULD_BLOCK`; it does not require application repaint.

`ERROR` (`0004`) payload begins:

```
u16 code
u8  effect       (0 continue, 1 abort transaction, 2 fatal session)
u8  reserved = 0
u16 offending_type
u64 offending_sequence
u16 context_length
u8  UTF-8 context[context_length]
```

Context is at most 240 bytes. Codes are 1 malformed frame, 2 sequence, 3
session, 4 epoch, 5 credit, 6 state, 7 transaction, 8 bounds, 9 scalar, 10
unsupported mandatory type, and 11 internal capacity loss. Effects are fixed
by the receiving implementation's state; peers cannot use `continue` to make
a framing fault recoverable.

An unknown message with bit 15 clear is an unsupported mandatory type and is
fatal. An unknown message with bit 15 set is optional: it is validated for
framing/credit/sequence, skipped, and credited normally. No optional message
may mutate CELL-1 unless a later capability contract defines it.

## 15. Failure boundary

CRC, structural header, magic, sequence, session, presentation-epoch, and
credit violations are fatal. If the outbound control reserve is usable, the
receiver sends one fatal `ERROR`; otherwise it closes locally. In either case
it discards the remaining binary bytes for that attachment epoch and never
feeds them to ANSI. It may restore ANSI only after an acknowledged framed
close boundary or a hard attachment/link reset that advances the outer epoch
and drains both directions. A local parser fault alone is not such a boundary.

Semantic invalidity inside a well-framed transaction aborts the whole staged
transaction and sends an abort-effect error or failed `TX_RESULT`. No renderer
or application observes partial state.

## 16. Extension reservations

Message ranges are reserved as follows:

| Range | Family |
| --- | --- |
| `0001`–`00FF` | Session, credit, results, reset, close. |
| `0100`–`01FF` | CELL-1 and future cell optimizations. |
| `0200`–`02FF` | Normalized input. |
| `1000`–`1FFF` | Future resources. |
| `2000`–`2FFF` | Future retained objects and regions. |
| `3000`–`3FFF` | Future series and animation. |
| `4000`–`4FFF` | Semantic controls reserved for additive profiles. |
| `8000`–`FFFF` | Skippable optional extensions. |

Reservation does not define a payload or grant a capability. Those families
remain outside the CELL-1 implementation gate. The optional additive contract
`APT-1-RETAINED-1-2026-08-24` defines selected IDs in `000a`–`000c`,
`0205`, `1000`–`1003`, `2000`–`2024`, `3000`–`3003`, `4000`–`4002`, and
`8000`–`8002` only after its deterministic discovery succeeds. Feature bit 8
`RET_CONTROLS` gates `CONTROL_DEFINE`, `CONTROL_REPLACE`, `CONTROL_DROP`, and
`CONTROL_EVENT`; `4003`–`4FFF` remains reserved. Every other reserved ID keeps
the behavior defined here; in particular, a sender may not infer a payload
from its range. CELL-1 alone still defines no semantic controls. A complete
styled-cell or GLYPH_RUN screen is therefore a foundation and fallback, not
evidence that the retained semantic-control family crossed the rich path.

The retained control mutations use `CONTROL_DEFINE` (`4000`),
`CONTROL_REPLACE` (`4001`), and `CONTROL_DROP` (`4002`). DEFINE and REPLACE
begin with the exact 80-byte prefix `<QQQHHiQQIIIIIIII>` carrying owner ID,
owner generation, independent control ID, kind, state, z-order, region ID,
parent control ID, sibling order, optional UNORM32 bounds, label byte count,
shortcut byte count, and zero reserved; label bytes then shortcut bytes follow
without padding. REPLACE resends that complete record, but all non-state fields
must exactly match the retained control; only `state` may change. DROP is exact
`<QQQ>`. RETAINED-1 Section 9.1 defines the canonical
MENU_BAR/MENU/MENU_ITEM/MENU_SEPARATOR graph and state rules.
Controls have an independent ID high-water but share each owner's existing
object-count and aggregate UTF-8 quotas. Negotiated inbound payload and
transaction bounds are the only additional size limits; no fixed control count
or control-string maximum is introduced.

## 17. Conformance vectors

The machine-readable assets live outside documentation under
`conformance/apt1/`: `manifest.json`, textual `.hex` transcripts, and
`validate_vectors.py`. They are conformance fixtures, not documentation. The
validator independently reconstructs headers, CRC-32C values, decoded fields,
directional sequences, and expected fatal cases. Implementations MUST pin this
contract ID and produce the same bytes before paired integration.
