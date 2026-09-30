# Flowing terminal appearance

The flowing appearance is a host-side rendering policy for existing retained
controls and instruments. It preserves the guest's logical cell grid, region
bounds, content, identities, state, and input semantics. The `reference`
appearance remains available for comparison. Neither appearance changes
Akashic's application behavior or supplies missing semantic information.

For an existing shared session, launch the live viewer from this checkout:

```sh
python session_viewer.py --appearance flowing
```

Add `--socket /path/to/session.sock` when the server uses a nondefault socket,
and `--font /path/to/DejaVuSansMono.ttf` to select a specific font. The same
viewer attaches to simulator and emulator sessions. It defaults to
`--appearance reference`; select the appearance when starting the viewer.

The public selection API is `rich_terminal.appearance.get_appearance(name)`,
returning an immutable `Appearance`. `REFERENCE_APPEARANCE` and
`FLOWING_APPEARANCE` name the two supplied policies. The production compositor
accepts that policy through its `appearance=` keyword.

## Render a recorded Desk frame

Run the following from the MegaPad checkout with its Python environment and
Pygame installed:

```sh
python tools/render_terminal_snapshot.py \
  tests/fixtures/compositor/desktop-six-app-final.json.gz \
  build/flowing-preview/desk-flowing.png \
  --appearance flowing

python tools/render_terminal_snapshot.py \
  tests/fixtures/compositor/desktop-six-app-final.json.gz \
  build/flowing-preview/desk-reference.png \
  --appearance reference
```

The CLI accepts one full display-offer JSON object or an array of full and
delta offers. Gzip compression is detected from the file contents. `--frame`
uses zero-based indexes; its default, `-1`, selects the final frame. Earlier
offers are decoded to reconstruct any required delta base. The script uses
`display_offer_from_wire`, `apply_terminal_snapshot`, and the actual
`compose_terminal_frame_result`; it does not alter the captured text or
invent controls. It creates a software surface without opening a window,
starting a guest, or acknowledging an offer to a live session.

The default cell/control font sizes are 18/16. The screenshot has the
recorded number of cells multiplied by the selected font's cell dimensions;
there is no scaling or layout reflow. Use `--font` and `--control-font` to
select exact faces. When `--font` is supplied without `--control-font`, both
use that face. Otherwise the system chooses monospace and sans faces.
`--fallback-font` is repeatable. `--no-font-discovery` disables automatic
fallback and styled-face discovery, which helps reproduce a capture with
explicit font files. The JSON written to stdout records selected font paths
and SHA-256 hashes, cell dimensions, appearance, and output path. Pixel-exact
comparisons also require matching Pygame/SDL/font-renderer versions.

The six-app final fixture is a full 280-by-84 display offer recorded after a
clean 52-stage simulator journey on 2026-09-30. It includes Sound Lab's real
readouts, meters, and indicators. It was produced with MegaPad based on
`a017548` and Akashic `f2f067991bb51e0c90445f5338db69c826b4e908`, using a freshly
built `desktop-apt1` simulator image. It contains guest state, not appearance
information, and can be replayed under either appearance.

The bundled typing sequence is an older real 280-by-84 Desk capture. Its first frame
includes five menubars, four item views, two tabsets, two text areas, and a
text grid. It does not contain Sound Lab's retained instruments. The older
`desktop-ready.json.gz` and `desktop-typed.json.gz` fixtures have no item
views. These are useful renderer evidence, not a claim of a new complete
six-application simulator run.

A fresh in-process Desk run can record a full offer with
`shared_session.display_offer_to_wire(offer)` at its ready boundary. Save the
complete JSON object and use it as the CLI input. An offer containing external
image-resource manifests needs its resource bytes as well; this standalone
tool refuses that case rather than producing an incomplete image.

## Scope and remaining semantic work

Existing semantic families already describe menus, tabs, text areas, logical
text grids, item views, and readout/meter/status instruments. Their surfaces
can use a shared visual vocabulary while the guest remains authoritative.
Status instruments represent simple indicators; they do not describe an
application status bar. A retained region also does not by itself identify
an application pane or its focus state.

The full pane-and-channel design needs the following semantic publication.
MegaPad now implements panes, structured status, taskbars, editable fields,
and typed spreadsheet cells; their Akashic producers remain integration work.
Border curves and material treatment remain host choices.

| Family | Required meaning | Producer work after the MegaPad side |
| --- | --- | --- |
| Pane (MegaPad implemented) | Stable identity, outer/content bounds, title metadata, visibility and focus; exact-owner content-region binding | Desk/app-host publication using existing pane state and explicit content clips |
| Structured status (MegaPad implemented) | Stable fields with separate label/value slots, severity, emphasis, order and bounds | Shared UIDL status/label observation and lowering, covering existing app status strips |
| Taskbar (MegaPad implemented) | Running-app and launcher entries, selected/minimized/enabled state, bounds, and activation intent | Desk shell publication from the same entries used for painting and hit testing |
| Editable field (MegaPad implemented) | Label/value, type, limits or choices, selected/enabled/read-only state, and revision-bound edit intents | Reusable widget with normal CELL drawing and ordinary event routing; migrate Sound Lab's parameter rows to it |
| Spreadsheet (MegaPad implemented) | Logical cells, number/formula-result/error roles, row/column headers, selection, viewport, and existing edit actions | Targeted migration of Grid's custom drawing into the reusable TEXT_GRID family |
| Waveform | Plot bounds, actual sample/series source, scale, and clip | Shared waveform widget/projector backed by Sound Lab's rendered PCM; the terminal already has a retained `WAVEFORM` object |

Grid's application currently draws its spreadsheet directly into cells;
existing support for `TEXT_GRID` refers to the reusable family used by the
Daybook calendar. Sound Lab's analysis readouts, meters, and indicators are
already semantic, while its parameter rows and waveform are still direct
CELL drawing. Daybook agenda entries and the Agent transcript already use
canonical item views. Custom titles, empty states, and Pad's gutter remain
additional residual content to consider separately.

New families must be defined in the shared contract and mirrored in the
paired repositories. They need capability negotiation, canonical validation,
wire and immutable-view support, bounded resource accounting, and existing
owner/generation/revision rules. Interactive fields and taskbar entries also
need a complete event path tied to the frame actually presented. Akashic's
shared producer must derive these records from the same widget state used by
normal drawing, with a complete TUI fallback. Until that work lands, the
current producer continues to publish its existing families unchanged.

Host rendering must not recognize filenames, glyph patterns, or application
names to infer absent roles. Rounded joins stay within the declared paint
footprints and clipping regions. Altering cell positions or making controls
larger would require an explicit layout and input decision.

## Rendering invariants

Appearance changes invalidate composition reuse. An unchanged appearance and
unchanged offer should retain the existing idle behavior: no unnecessary
composition or window flip. Partial repaint must produce the same pixels
and hit entries as full composition. Every decorative pixel must be covered
by its recorded paint extent, and hover/press rendering must preserve the
authoritative target geometry.

Corners must fit the control's existing text clearance. Text viewports and
ordinary item rows use square edges because their logical slots can reach the
boundary. Menus, tabs, grid cells, readouts, and cards limit their corner radius
to the inset already reserved by their layout, leaving a pixel for the border.
This changes the material geometry without adding padding, shifting text, or
changing input coordinates. Meters without numeric labels can retain full caps.
Menu and tab surfaces also leave up to two pixels of paint clearance inside
their existing label padding. Selection fills and open-menu accents stay off
the container dividers; label positions and the full activation bounds remain
unchanged.

The existing focused checks are `tests/test_viewer_partial_repaint.py`,
`tests/test_rich_terminal_compositor_replay.py`,
`tests/test_rich_terminal_semantic_pygame.py`, and the idle/partial-present
checks in `tests/test_session_viewer.py`. Replay these small recorded frames
while developing the renderer; reserve the full simulator journey for a
coherent implementation that is ready for interaction validation.

## Validation of the first renderer slice

On 2026-09-30, the rebuilt native backends passed the 395-test BIOS/multicore
smoke subset. The six focused renderer/viewer files below passed 228 checks:

```sh
make test-sequential TEST_PATH="tests/test_rich_terminal_appearance.py \
tests/test_viewer_partial_repaint.py tests/test_rich_terminal_semantic_pygame.py \
tests/test_rich_terminal_compositor_replay.py tests/test_session_viewer.py \
tests/test_rich_terminal_pygame_view.py"
```

The clean native-simulator Desk journey completed all 52 stages and 51 inputs
with the flowing appearance: 33.06 seconds to ready, 120.06 seconds total,
282.72 MiB peak resident memory. These are single-run wall-clock observations,
including image preparation and deliberate input pacing, not isolated renderer
benchmarks. The run used production session dispatch and composition with an
in-process client and SDL's dummy software display. It validates the headless
interaction path, not physical display presentation or audible playback.

The capture used DejaVu Sans Mono at 18/16 pixels for cells/controls, with an
11-by-21-pixel cell grid. This environment lacked complete CJK/emoji font
coverage; missing glyphs in the mixed-text acceptance data appear under both
appearances. They are preserved in the fixture rather than replaced in the
preview.

## Unified-runtime integration checkpoint

The flowing branch integrates the committed runtime extraction at `ca66ad7`
with the appearance work at `4e8bf26`. The checkpoints were combined in a
separate worktree before advancing the appearance branch. Their common edits
were the README and viewer; the viewer now imports terminal types from
`shared.session`. Both native extensions built from the combined checkout.

The renderer, replay, viewer, semantic wire/input, vertical-contract, and
package-layout checks passed 343 tests. With native semantic execution
selected, the launcher, cold-import boundary, and simulator session/server/
clock checks passed 37 tests; one Unix-socket boundary test was skipped because
this execution environment denies AF_UNIX creation. These are integration
checks of the existing object families, before pane/status implementation.

A fresh native-simulator Desk journey through
`megapad.main --mode simulator --executor native` completed all 52 stages and
51 inputs with the flowing renderer. It reached ready in 32.76 seconds,
finished in 118.87 seconds, and used 279.22 MiB peak RSS. The actual runtime
descriptor reported the native simulator and host-monotonic RTC. Production
shutdown released the owner thread, backend, runtime ownership token, terminal
driver, and display lease. Akashic remained at `f2f0679` for this run.

The acceptance harness replaced only the Unix listener bind and serving loop
with a scripted in-process client. The real unified entry point selected and
prepared the simulator, started the owner, and stopped it on return; requests
used production dispatch and the existing display/acknowledgment path. The
viewer components rendered through SDL's dummy software display. This covers
headless simulator acceptance, not socket transport, the complete standalone
viewer event loop, physical display/audio, or emulator/hybrid execution. The
timings include image preparation and deliberate input pacing and are a single
acceptance observation rather than a controlled performance comparison.

New terminal families should extend the common `shared.session` authority and
the backend-neutral codecs in `shared_session.py`. Keep the runtime adapters
responsible for engine construction, execution, timing, and resource release.
Keep rich-object schemas, guest publication, retained composition, and appearance
work together on the UI side. Coordinate shared offer/input/acknowledgment
changes and any runtime optimization of terminal projection or serialization
before either branch changes those contracts.

The runtime branch can continue from its existing history. Future integration
should merge a committed runtime checkpoint into the UI branch and repeat the
affected boundary checks. This checkpoint leaves hybrid execution as later
runtime work and establishes the common module locations for pane support.

## Pane objects and the Akashic producer handoff

`PANE` is object kind 10, gated by `RET_PANES` (feature bit 11). Its `PNE1`
body names an exact-owner content region and publishes pane-local content
bounds, clean UTF-8 title metadata, and committed focus state. The ordinary
object envelope supplies identity, generation, outer bounds, z-order, and
visibility. The normative format is in APT-1-RETAINED-1 Section 11.10; the
guest API is `PT-PANE-DEFINE` / `PT-PANE-REPLACE` in RICH-TERMINAL-MODULE.

The host validates that the content region has an explicit clip contained in
the translated content rectangle, paints after the chrome region, and belongs
to one pane. One pane consumes one object slot and its title's UTF-8 bytes.
Owner quotas, atomic transactions, visibility, reset, and stale-generation
rules remain the existing retained rules. A pane adds no semantic input
target: ordinary chrome pointer intent still reaches Desk, which publishes
the resulting focus. Hiding chrome does not implicitly mutate another region.

Both appearances paint only the declared outer rectangle minus the content
rectangle. They preserve every content pixel and cell position. A title is
drawn in the first outer row, with one cell of horizontal clearance, only
when the content rectangle leaves that row available and the outer width is
at least three cells. Otherwise it remains metadata. Desk currently starts
each application with its menu row, so adding a title row would change the
agreed geometry. Its present pane preview uses the existing divider space.

Keep the current product policies unchanged until their producers opt into
panes. Updated guests retain their existing supported objects when PANES is
absent; pane publication returns `PT-S-UNSUPPORTED` without emitting a frame.
Older guests reject unknown feature bits during discovery and retain CELL
fallback. Advertise PANES only alongside the updated guest module. The
profile needs at least two regions, positive object and aggregate UTF-8
capacity, a 104-byte inbound payload, and a 304-byte retained transaction.
Larger titles must fit the caller's actual frame, transaction, and owner quota.

For Akashic, publish the pane frame and content-region clip from the same
Desk/app-host geometry used to draw and route events. Define both regions
before their pane, keep content above chrome in region paint order, and
update focus/visibility with the existing presentation transaction. Preserve
complete CELL borders and application content for fallback. When producing
the retained view, omit the explicit divider glyph runs replaced by pane
chrome. Guest publication should provide real content-region bindings for
the contained controls; the host never discovers them from text or app names.

### Constructed six-pane preview

```sh
python tools/render_pane_preview.py \
  tests/fixtures/compositor/desktop-six-app-final.json.gz \
  build/flowing-preview/desk-panes.png --appearance flowing
```

The fixture uses the exact six-tile geometry from Akashic `f2f0679` and the
recorded 280-by-84 Desk frame. Its checked layout manifest supplies every
outer/content rectangle and title. It preserves the original CELL snapshot,
application draws, and input targets, suppressing only retained glyph runs
wholly contained in the explicitly declared divider cells. A partially
overlapping glyph run is refused instead of dropping guest text.

This is a constructed publication preview, not a run of migrated Akashic
producers. Empty content-region bindings sit below the unchanged recorded
regions and do not reparent their controls. The JSON sidecar records those
bindings, removed divider-object IDs, source hash, geometry, and focus. Use
`--focus 0..6` to preview focus, `--pane-bounds SLOT X Y COLS ROWS` to adjust
chrome within the existing dividers, and `--offer output.json.gz` to save the
constructed display offer. The production decoder and compositor render it.

### Pane validation checkpoint

The supervised sequential gate passed 626 tests in 55.27 seconds. It covers
the pane codec, quota and transaction rules, immutable projection, shared
full/delta offers, production server presentation and acknowledgment, pointer
routing, both appearances, and full/partial repaint equivalence. The complete
guest writers emitted matching canonical frames under the emulator and both
Python and native simulator executors. Unsupported profiles emitted no pane
frames. The six-pane fixture preserved content pixels and pointer targets
outside the explicitly declared dividers.

These checks qualify the MegaPad implementation and the constructed preview;
they do not represent a new live Desk run with Akashic pane producers. The
earlier unified-runtime Desk acceptance above remains a separate checkpoint.

Runtime commits through `6bea7e5` were reviewed at this checkpoint. Their native
floating-point, fault-return, Keccak, and hybrid-ABI work does not change the
pane, retained-view, or shared-session contracts, so no additional runtime
merge was needed. The next integration must rebuild both native extensions
for their changed exported APIs. The runtime team's uncommitted dense-memory
work remains in its own worktree.

## Structured status fields and producer handoff

`STATUS_FIELD` is object kind 11, gated by `RET_STATUS_FIELDS` (feature bit 12).
Each object identifies one read-only status field with an exact one-row cell
rectangle. `STF1` carries separate label and value strings, the number of cell
columns reserved for the label, severity, and emphasis. Multiple fields use
ordinary object identities, bounds, and paint order to form a status strip.
No text parsing or implicit rearrangement is needed. The normative schema is
in APT-1-RETAINED-1 Section 11.11; guest writers are
`PT-STATUS-FIELD-DEFINE` / `PT-STATUS-FIELD-REPLACE`.

The renderer starts the value at the published cell split and clips each
string to its own slot. It adds no padding or pointer target. One-row fields
use square material so edge text retains its occupied space. Severity and
emphasis change appearance; complete replacement changes published content.
The existing numeric `STATUS` indicator remains a separate object family.

The capability depends on CORE, positive object and aggregate UTF-8 capacity,
a 96-byte inbound payload, and a 296-byte retained transaction. Both strings
count toward the owner's UTF-8 reservation. Existing product profiles stay
unchanged until paired producers adopt the new capability. Updated guests
return unsupported without emitting a frame when it is absent.

Akashic should derive each field's strings, split, and bounds from the same
status-widget state and layout used for CELL drawing. Preserve CELL fallback;
omit only the corresponding retained glyph runs when publishing semantic
fields. Changes in application state must publish through the existing
presentation transaction rather than updating terminal-owned text locally.

The status-field checkpoint passed 679 supervised tests in 59.71 seconds,
including emulator and Python/native simulator publication, malformed frames,
quota and transaction rejection, full/delta offers and acknowledgment, exact
label/value clipping, and complete/partial repaint equivalence. No live
Akashic status-field publication is claimed by this checkpoint.

## Taskbar entries and producer handoff

`RET_TASKBARS` (feature bit 13) adds CONTROL kinds `TASKBAR` (10), `TASK` (11),
and `LAUNCHER` (12). It requires CONTROLS and uses the existing CONTROL wire
envelope and ACTIVATE event. `PT-CONTROL-DEFINE` / `PT-CONTROL-REPLACE` publish
these kinds with explicit geometry; there is no new event payload.

A taskbar root occupies one guest-defined cell row. Its children have exact
one-row rectangles relative to that root, stable identities and order, and
separate task or launcher meaning. Their slots must fit and remain disjoint,
including slots of hidden entries. Only TASK can be selected or minimized;
at most one task is selected and selection excludes minimization. Disabled or
hidden roots and entries cannot activate. Minimized tasks remain activatable
so the guest can restore them through its existing focus action.

The compositor clips material and labels to each entry, preserves separator
holes, and never derives hit widths from text measurement. Curves use only
unused text space. Changing label, state, font, or appearance leaves activation
bounds unchanged. The same acknowledged-frame input rules as existing menus
and tabs apply; the host does not select, restore, minimize, or launch an app
locally in response to an activation.

Desk should publish the same entry bounds and application IDs used by its
current bottom-row drawing and hit testing, and route TASK activation to its
existing focus/restore operation. LAUNCHER activation remains a guest action.
Keep title truncation and entry placement in the producer's existing layout.
When geometry, order, or ancestry changes, rebuild the subtree under the
existing identity and transaction rules; CONTROL_REPLACE changes only state
and, for entries, label/shortcut metadata. Preserve the CELL taskbar for
fallback and omit its replaced retained glyph runs during rich publication.

Taskbar qualification passed 688 checks in the broad supervised gate, including
exact Forth publication on emulator and both simulator executors. Three test
fixtures were corrected for the existing RET_CAPS header offset and CELL
fallback geometry minima; the focused model/wire rerun passed all 114 checks.
The production input test crosses projection, JSON, actual Pygame hit maps,
sink acknowledgment, shared RPC, and binary ACTIVATE. It verifies that an old
press or backpressured activation cannot cross to a replacement display.

## Editable fields and producer handoff

`RET_FIELDS` (feature bit 14) adds the root CONTROL kind `FIELD` (13). FDC1
content describes INTEGER, CHOICE, or TEXT values, a content revision,
read-only state, and explicit disjoint label/value rectangles inside the
root. Integer fields carry inclusive signed-64-bit bounds and a positive
adjustment step; the step does not constrain which in-range exact values are
valid. Choices carry ordered, unique signed values and their labels. Text
fields carry single-line UTF-8. All strings and choice records use existing
owner quotas; each choice consumes one additional object slot.

The producer retains authority over values and editing. Clicking a writable
value slot sends existing ACTIVATE for the guest's editor or choice action.
Vertical wheel input sends a 56-byte ADJUST event with signed step count and
the exact displayed content revision. Positive counts mean increase/next;
the guest applies its own overflow-safe clamp, wrap, or refusal policy and
publishes the result. Read-only, hidden, and disabled fields cannot activate
or adjust, and TEXT fields cannot adjust. The host never changes the displayed
value speculatively. Raw keyboard navigation, text input, and prompt editing
continue through their existing routes.

The host paints each text slot independently and attaches input only to the
writable value rectangle. Labels and gaps block activation of controls below
them. Every adjustment requires the current acknowledged display, owner
generation, model revision, and content revision; backpressured wheel input
is dropped rather than carried into a later field. Integer values align to
the right of their declared value slot; choice and text values use its left
edge. The geometry and clipping remain guest-defined.

FIELDS requires CONTROLS, a 176-byte inbound payload, and a 376-byte retained
transaction. The existing CORE outbound minimum already accommodates ADJUST.
Existing profiles remain unchanged. Generic `PT-CONTROL-DEFINE` / REPLACE
writers validate canonical FDC1 content before publication; public event
accessors expose ADJUST's content revision and signed adjustment count.

Sound Lab's current rows map directly: frequency 40–2000 with step 10,
amplitude 0–100 with step 5, duration 100–2000 with step 100, and waveform
choices with their existing oscillator IDs. Reuse the current label/value
positions and exact-value prompt, which accepts arbitrary in-range integers.
A shared Akashic field widget should publish from that same state and keep
its ordinary CELL drawing and event handling. Terminal-local text editing,
caret/selection state, and IME composition are outside this field contract.

The FIELD checkpoint passed all 859 supervised checks in 62.87 seconds.
Production-source publication ran on the emulator and both simulator
executors; independent incoming-frame tests exercised signed ADJUST limits,
capability negotiation, malformed tails, revision checks, and event accessors.
Host tests covered choice/object and UTF-8 quotas, atomic failure and retry,
immutable full/delta offers, driver credit, display proof, and pointer routing
through the rendered value area. Read-only behavior, unchanged raw keyboard
handling, and full/partial repaint equivalence are covered.

## Typed grid cells and producer handoff

`RET_GRID_CELLS` (feature bit 15) extends the existing TEXT_GRID control with
NUMBER (4), FORMULA (5), and ERROR (6) item roles. It requires
CONTROL_COLLECTIONS and retains the STX1 version, binary layout, quotas,
viewport, selection, and whole-cell PLACE event. There is no new object
family, evaluator, or terminal-owned spreadsheet state. Plain CONTENT grids,
including Daybook's calendar, continue to work without this capability.

The guest supplies each cell's final display string. NUMBER and FORMULA
align to the right within the existing padded rectangle, while ERROR aligns
to the left. The host applies distinct role colors, with disabled and
unavailable treatment and selected-text contrast taking precedence. Cells,
headers, viewport origins, clipping, and hit rectangles keep their published
geometry. All data roles can receive whole-cell selection; headers and
unavailable cells cannot. Read-only content remains selectable.

Grid's producer should map its existing cell types directly: NUMBER carries
the current source text, FORMULA carries the guest-computed result, ERROR
carries the current error text, and ordinary text remains CONTENT. Use
logical column spans to preserve its existing row-header and data-cell
widths. Publish the current viewport and stable cell identities from the
same model used for CELL drawing. Continue routing PLACE and raw keyboard
input to the existing selection and editor actions. The host never parses a
string to discover its role or applies edits locally.

The Forth writer inspects bounded STX1 structure and role admission before
emitting or charging a transaction; malformed framing is invalid, and a
well-formed typed grid without GRID_CELLS is unsupported with no emission.
Full canonical content validation remains on the host. Adopt the updated
paired guest module before advertising this capability. Existing product
profiles remain unchanged until the Akashic producer is ready.

The supervised grid gate passed 508 checks in 61.10 seconds. Two additional
renderer assertions incorrectly expected paragraph direction on whole-cell
input targets; their replacement verifies fixed column identities, and the
complete 21-case renderer rerun passed in 0.24 seconds. Canonical publication
passed on the emulator and both simulator executors. Tests cover feature
fallback, quotas and atomic retries, all data roles, headers/unavailable
cells, immutable projection and full/delta JSON, acknowledged input, clipping,
bounded glyph rendering, and complete/partial repaint equivalence. The host
accepted no new style-run or TEXT_AREA behavior.

## Completed MegaPad scope and runtime integration

The MegaPad side of the agreed pane-and-channel design is implemented through
`d743206`: pane chrome, structured status fields, taskbar entries, editable
fields, and typed spreadsheet cells all have negotiated contracts, bounded
guest writers, host validation, immutable transport, rendering, and applicable
input authority. The existing WAVEFORM family already supplies the remaining
Sound Lab waveform contract. Reference remains the default appearance;
flowing is opt-in. Product capability profiles remain unchanged.

The isolated integration branch combines this work with runtime code through
`6b67833` and its documentation follow-up `2393829`. Both native extensions
were rebuilt from the combined source. The shared launcher/session, wire,
display-proof/input, emulator field-receive, and viewer gate passed 375 tests
in 34.26 seconds. One existing Unix-socket boundary case was skipped because
the environment denies AF_UNIX creation. A second 80-test gate passed in
4.37 seconds, exercising all five guest publishers on both simulator
executors, the simulator session/server boundary, and typed grid transport,
input, and rendering.

The dedicated retained-hybrid test passed both executor cases in 1.50 seconds.
It loads the complete production Forth module, negotiates and publishes CELL,
TASKBAR/TASK, and FIELD through public writers, delivers and acknowledges the
display through the shared dispatcher, then calls a declared native routine.
Forth receives the exact subsequent ACTIVATE and signed ADJUST metadata;
stale generation/offer proofs are rejected and the field remains unchanged
until guest publication. Cleanup releases runtime ownership and the display
lease, stops the owner thread, and closes both backends. The test does not
replace the driver, retained state, or control-event codecs with test doubles.

A fresh six-application Desk acceptance ran at integration `ec790b3` with
Akashic `f2f067991bb51e0c90445f5338db69c826b4e908`. The unified native-simulator
launcher completed all 52 stages and 51 inputs: 33.74 seconds to desktop
ready, 122.12 seconds overall, and 283.84 MiB peak RSS. Production shutdown
released the owner thread, backend, terminal driver, runtime owner, and
display lease. This is a single acceptance observation including fresh image
preparation and paced input, with other local validation work running; it is
not an isolated performance comparison.

The Desk harness used production in-process server dispatch and display ACKs
with SDL's dummy software sink. It did not qualify Unix-socket transport,
physical display/audio, or live publication of the new families. Akashic's
existing producer continues to publish its original objects, so this journey
checks compatibility while focused guest tests qualify the new contracts.

The next implementation work belongs in Akashic: publish pane/content-region
bindings and taskbar entries from Desk, lower status fields through the
shared widgets, use typed fields for Sound Lab, map Grid to TEXT_GRID roles,
and publish Sound Lab samples through WAVEFORM. Preserve the current cell
geometry and ordinary drawing/event fallback throughout. Then advertise the
new capability bits in the paired profile and repeat the live visual/input
review. Producer-driven evidence may call for MegaPad fixes, but no further
host-side schema is currently required for this agreed scope.

The runtime team's later `7acbdcd` was reviewed during final qualification:
it adds an interoperability design document and roadmap links only, with no
new executable capability. Their native tile changes were still uncommitted
in the separate worktree and were not imported. Reconcile the next committed
runtime checkpoint before a final main merge; the qualified code boundary
here remains `6b67833`. Neither main nor Akashic was modified by this work.
