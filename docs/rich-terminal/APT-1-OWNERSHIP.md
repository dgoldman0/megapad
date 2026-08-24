# APT-1 ownership and lifetime ledger

Contract ID: `APT-1-CELL-1-2026-08-24`

This ledger is normative where it assigns creation, mutation, retirement, and
reset responsibility. Identifiers establish freshness and routing; they never
replace application authority checks.

| State | Creator and owner | Mutator | Retirement | Soft reset | Hard reset / detach |
| --- | --- | --- | --- | --- | --- |
| MegaPad attachment epoch | MegaPad host port | MegaPad scheduler boundary | Primary lease release/replacement | Preserved | Advanced, invalidating all terminal handles and queued ingress |
| APT session ID | Enhanced terminal during `OFFER` | Immutable | Acknowledged close or hard reset; fatal error/timeout makes it unusable but quarantines the stream | Preserved | Destroyed; ANSI owns the drained replacement stream |
| Presentation epoch | Starts at zero on open | Terminal requests exactly +1; client acknowledges | Session retirement | Advanced; model/revision/transaction scopes reset | Destroyed |
| Terminal CELL-1 model | Terminal core | Accepted snapshot/transaction commit | Session retirement | Discarded, then replace-all snapshot rebuilds it | Destroyed |
| Akashic back buffer | Akashic screen/application | Akashic paint | Screen/application teardown | Preserved and authoritative for rebuild | Preserved across terminal loss if application remains alive |
| Akashic front buffer | Akashic screen | Only after local backend commit acceptance | Screen teardown or forced redraw invalidation | Invalidated until snapshot acceptance | Invalidated; ANSI redraw required |
| Open transaction | Sender creates nonzero transaction ID | Sender appends; receiver stages | Commit, abort, error, reset, close | Uncommitted work aborted; crossed COMMIT settled by status 1 before ACK | Destroyed |
| Model revision | Terminal commit logic, scoped to presentation epoch | Successful atomic commit only | Epoch/session retirement | Reset to zero; replacement commit makes revision one | Destroyed |
| UART egress publication | MegaPad machine adapter | Immutable after publication | Primary consumer release or attachment retirement | Preserved if same attachment/session | Old epoch publication discarded |
| Terminal ingress event | Terminal frontend/session | Immutable after admission | Scheduled UART application or epoch retirement | Old presentation events rejected | Old attachment events cancelled |
| Geometry generation | Terminal frontend while active; legacy frontend while ANSI | Current authoritative frontend | Replacement by later generation | Preserved unless snapshot geometry changes it | Re-established before boot/negotiation |
| Normalized input event | Terminal session | Immutable | Akashic validation/dispatch or bounded rejection | Events for old presentation epoch rejected | Old session events rejected |
| Optional retained owner ID + generation | Akashic service-broker activation lease (not CELL-1) | Exact owning activation through the broker | Successful exact idempotent owner drop | Destroyed; broker allocates/replays current-epoch binding | Destroyed with session |
| Optional retained region/object/resource/series IDs | Exact live retained owner projection (not CELL-1) | Exact live owner generation through one global broker | Exact item drop or owner drop | Destroyed; replayed with the new epoch binding | Destroyed |

The terminal reset planner owns the commit/result settlement gate. It emits an
accepted COMMIT's result before constructing SOFT_RESET_REQUEST and derives
`last_revision` afterward. If an already-emitted request crosses a client
COMMIT awaiting result, the client holds ACK and the terminal returns status 1
with unchanged revision before either side advances epoch. Only an uncommitted
open transaction is terminated by TX_ABORT. This bounded settlement preserves
ordered ownership; it does not turn ordinary transaction rejection into a safe
fallback.

## Baseline and optional-module ownership

KDOS and BIOS own only the established UART and terminal-geometry primitives.
They do not own an APT parser or presentation model.

The root-level `presentation-terminal.f` module owns guest-side negotiation,
framing, credit, session state, and normalized enhanced input only after it is
explicitly loaded and asked to open. It borrows caller-provided bounded
storage. It returns raw-stream ownership to the prior ANSI/key path on
pre-`OPEN` refusal or timeout, acknowledged close, or a hard attachment reset
that advances the outer epoch and drains both directions. A post-`OPEN`
structural failure enters `LOST` and retains exclusive binary ownership; local
failure detection alone is never an ANSI-safe boundary.

Akashic owns application/domain state, focus, cell buffers, and whether to
request the optional service. Its ANSI backend is constructed by default. Its
APT adapter owns no terminal session independently; it borrows a live module
session and may be discarded without discarding Akashic state.

The MegaPad enhanced frontend owns terminal projection only while its primary
host-port lease exists. Without that lease, existing ANSI frontends retain
their current ownership and behavior.

## Optional retained identity rule

Retained families are not implemented by CELL-1. When the optional
`APT-1-RETAINED-1-2026-08-24` discovery succeeds, their identity boundary is
exactly `(session_id, presentation_epoch, owner_id, owner_generation,
item_namespace, item_id)`. No component pointer, region address, opaque
application key, or native Akashic instance address may appear on the wire.
Owner-wide retirement is atomic and idempotent for the exact generation.

The retained service broker is global to the one APT session because sequence,
credit, transaction IDs, presentation revision, resource upload, reset, and
close are global. Wire authority is nevertheless owner-exact: the broker may
not substitute its session ownership for the `(owner_id,owner_generation)` on
an item operation. The normative quota, tombstone, hidden-rebuild, immutable
view, and activation-lease lifetimes are in
`APT-1-RETAINED-1-OWNERSHIP.md`.
