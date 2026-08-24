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
| Open transaction | Sender creates nonzero transaction ID | Sender appends; receiver stages | Commit, abort, error, reset, close | Aborted | Destroyed |
| Model revision | Terminal commit logic, scoped to presentation epoch | Successful atomic commit only | Epoch/session retirement | Reset to zero; replacement commit makes revision one | Destroyed |
| UART egress publication | MegaPad machine adapter | Immutable after publication | Primary consumer release or attachment retirement | Preserved if same attachment/session | Old epoch publication discarded |
| Terminal ingress event | Terminal frontend/session | Immutable after admission | Scheduled UART application or epoch retirement | Old presentation events rejected | Old attachment events cancelled |
| Geometry generation | Terminal frontend while active; legacy frontend while ANSI | Current authoritative frontend | Replacement by later generation | Preserved unless snapshot geometry changes it | Re-established before boot/negotiation |
| Normalized input event | Terminal session | Immutable | Akashic validation/dispatch or bounded rejection | Events for old presentation epoch rejected | Old session events rejected |
| Future owner ID + generation | Akashic activation lease (not CELL-1) | Owning activation only | Exact idempotent owner drop | Replayed under new presentation epoch | Destroyed with session; application may allocate fresh binding |
| Future region/object/resource IDs | Akashic owner projection (not CELL-1) | Exact live owner generation | Exact item drop or owner drop | Replayed from Akashic authoritative state | Destroyed |

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

## Future retained identity rule

Retained families are not implemented by CELL-1, but their identity boundary
is fixed now: `(session_id, presentation_epoch, owner_id, owner_generation,
item_namespace, item_id)`. No component pointer, region address, or native
Akashic instance address may appear on the wire. Owner-wide retirement is
atomic and idempotent for the exact generation.
