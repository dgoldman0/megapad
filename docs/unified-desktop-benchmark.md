# Bounded Desktop session benchmark

`bench_unified_desktop.py` runs an explicit prepared MP64FS image through a
production emulator, simulator, or hybrid session. It copies the image to a
private temporary directory, drives a bounded JSON keyboard journey, and emits
a JSON report. It does not import a guest source project, rebuild an image, or
change the supplied image.

The production two-key fixture has passed with simulator/Python,
simulator/native, and hybrid/native. Those gates exercise preparation, real
session ownership, input, CELL rendering, image isolation, and shutdown. The
bundled eight-step prepared Desktop journey also completed with
simulator/native on 2026-09-30: 31.111 seconds to ready, 36.340 seconds overall,
12 presented offers and 258,785,280 bytes peak RSS. Every expected step and
shutdown check passed, and the original image hash was preserved. This is one
bounded acceptance run, not a throughput comparison. Its report is
[`performance/unified-desktop-simulator-native-2026-09-30.json`](performance/unified-desktop-simulator-native-2026-09-30.json).
Prepared Desktop runs for the other modes/executors remain pending.
Hybrid uses an empty routine registry and must report zero
machine instructions and zero machine transitions.

## Scope

The harness starts the production machine owner and calls
`SessionServer.dispatch` directly. It claims the display lease, accepts
production screen updates, verifies resource chunks, composes frames with the
production viewer, stages the compositor's hit map, and acknowledges offers
after drawing to SDL's dummy software sink. Input carries the current session
generation and acknowledged display scope. Only zero-effect backpressure or
stale-display responses may be retried; partial acceptance fails the run.

Assertions inspect immutable **CELL backing snapshots**. A journey with
`require_retained: true` also requires an acknowledged, visible retained plane
with regions. Each input expectation must be observed on a newer CELL revision
or display offer. These checks do not establish retained-text/OCR equivalence,
pointer behavior, or the semantic contents of every drawn object.

No listener is opened or replaced. The run does not qualify Unix socket
transport, a physical display, audio, or the standalone viewer event loop.
Historical socket attempts in this environment failed with `PermissionError`
at `socket(AF_UNIX, SOCK_STREAM)` before binding; this direct-dispatch benchmark
is a separate supported session scope, not a transport workaround. The project
does not provide a TCP replacement for this session protocol.

Timing is host wall time plus the selected backend's reported accounting.
Simulator and hybrid semantic steps are not architectural instructions or
hardware cycle estimates. Hybrid's empty-registry run establishes session
composition compatibility, not machine-call performance. The harness selects
the normal real-time session clock policies; Daybook dates can consequently
depend on the host date.

## Prepared fixture and provenance

The inspected prepared image is:

```text
/workspace/scratch/64bce13821f6/desk-flowing-unified-integration/desktop-fresh-simulator.img
```

It is 33,554,432 bytes. Its SHA-256, observed on 2026-09-30, is
`13bbe7264c8735180fb0841ed96a863e962bba28c56b4a3f5312e180487aad88`.
It is a **used prepared image**: a previous journey persisted files, including
Daybook tasks. Its filename does not establish that it is pristine. The
harness records the actual input hash on every run and checks that hash again
after shutdown.

The reference evidence is pinned independently of this current image:

| Evidence | Identifier |
| --- | --- |
| Previous harness | `run_desk_flowing_unified.py` |
| Previous harness SHA-256 | `a9fa986c62fe9768af2f5f67d041551dfc68c7729ff3eb9e0ad55470196fd56a` |
| Runtime checkpoint | `ca66ad7bb0bde9bfc57067f7067a1bf9151c6ff0` |
| Appearance checkpoint | `4e8bf26a18346043ea6b903fc8251a14e46e15b0` |
| Previous result SHA-256 | `6ab957e2d9f1d0bfb9bbbce7d42b482b7f562f5a634b8df67722c5ffd5dfb48a` |
| Image hash recorded before the previous journey | `db0b8a2d7c5b77eb8d08fb10774b5819f698792adda1dc4468d032034dbc7e2a` |
| Reference keyboard journey source SHA-256 | `4f971973c645278c35f663672740a26544aae25acdd4c63670fc50cdf57ee1fc` |

The old result and captures are in
`/workspace/scratch/64bce13821f6/desk-flowing-unified-integration/`.
That report recorded a 52-stage journey, 32.76 seconds to ready and 118.87 seconds
overall, using the merged working tree of the two checkpoints above. Those
numbers are historical context, not measurements of this harness or the
current branch. Its old harness imported external image-building and journey
helpers; the new harness has no such dependency.

The bundled `tests/fixtures/desktop-keyboard-journey.json` copies a bounded
keyboard subset from the reference journey's stages 3 through 10: edit Pad,
focus Daybook, open its new-task prompt, type and commit a task, move to the
next date, focus File Explorer, and return to Pad. It requires 280 by 84 cells,
initial Pad focus, no visible `~` acceptance marker, and no open `New task:`
prompt. It does not reproduce the original 52-stage pointer, Unicode, and
semantic-object acceptance suite. The current image already contains a saved
`^` task for 2026-09-30; prompt transitions and subsequent date navigation are
still checked, but this input is not a unique persistence marker.

Static inspection found no prerequisite for the peer branch's new pane,
status-field, taskbar, or typed-field protocol families. The policy below uses
feature mask 1801 (`0x709`): CORE, INSTRUMENT, CONTROLS, CONTROL_COLLECTIONS,
and CONTROL_ITEMS, all supported by the current branch. The pinned captures
contain existing glyph-run, text-grid, text-area, menu, tab, item-view, meter,
readout, and status kinds. The old flowing appearance only changes host paint;
this harness uses the checkout's default compositor. This is compatibility
evidence for attempting the run, not a completed live gate or visual parity
claim. The later simulator/native run above establishes this bounded subset;
the peer's newer protocol families remain outside its coverage.

## Exact reusable policies

These complete policy objects are copied from the pinned prior report's
launcher arguments. Keep both together; retained policy requires rich-terminal
policy. They allow the fixture's large retained Desktop transactions and
collection payloads. They do not advertise the peer branch's newer families.

```bash
desktop_rich_policy='{"ansi_history_bytes":262144,"egress_high_batches":32,"egress_high_publications":2,"egress_low_batches":4,"geometry_events":8,"ingress_bytes":131072,"ingress_control_bytes":4096,"ingress_control_events":32,"ingress_events":256,"max_cols":400,"max_rows":200,"pending_outbound_bytes":131072,"pending_outbound_events":256,"service_batches":4}'
desktop_retained_policy='{"base_max_transaction_bytes":16075624,"client_to_terminal_max_payload":917608,"features":1801,"image_format":0,"max_glyph_run_bytes":1600,"max_history_per_series":0,"max_image_height":0,"max_image_width":0,"max_live_owners":1,"max_objects":109695,"max_operations_per_transaction":110720,"max_owner_records":1,"max_path_points":0,"max_regions":7169,"max_resource_chunk_bytes":0,"max_resources":0,"max_retained_transaction_bytes":16075624,"max_samples_per_append":0,"max_series":0,"minimum_presentation_interval_us":0,"terminal_to_client_max_payload":64,"total_resource_bytes":0,"total_sample_slots":0,"total_utf8_bytes":12575232}'
```

## Run the prepared journey

Run from this checkout with its locally built extensions and a Python
environment containing pygame. An explicit native executor fails if it is
unavailable. Use the normal parent CLI, which starts a fresh worker process;
`--worker` is internal and bypasses the outer watchdog.

After defining the two policy variables above, the first recommended live
attempt is simulator/native:

```bash
desktop_python=/workspace/scratch/64bce13821f6/runcheck-venv/bin/python
desktop_image=/workspace/scratch/64bce13821f6/desk-flowing-unified-integration/desktop-fresh-simulator.img
"$desktop_python" bench_unified_desktop.py \
  --image "$desktop_image" \
  --journey tests/fixtures/desktop-keyboard-journey.json \
  --mode simulator --executor native \
  --cols 280 --rows 84 --ram-kib 1024 --ext-mem-mib 320 --vram-mib 4 \
  --semantic-quantum-steps 65536 --timeout 240 \
  --rich-terminal-policy "$desktop_rich_policy" \
  --retained-terminal-policy "$desktop_retained_policy" \
  --font /usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf --font-size 12 \
  --output /tmp/megapad-desktop-simulator-native.json \
  --artifacts /tmp/megapad-desktop-simulator-native-captures
```

The artifact directory must not exist. Choose new report and capture paths
for each later run. The font path above was present during inspection; choose
an available local font elsewhere. Font identity, size and cell metrics are
recorded. This uses 12-point fonts for both CELL and controls, unlike the old
harness's 18/16-point pair, so captures and render costs are not pixel-matched
comparisons to its report.

Use `--mode hybrid --executor native` for the same prepared semantic image,
with separate outputs. The harness supplies a private, validated empty
routine manifest; both native extensions are required. `--executor python`
and `--executor auto` are available for semantic modes, and the report records
the executor actually selected. Test explicit executors when comparing costs.

For an emulator attempt, use `--mode emulator --executor native` and add
`--restore-emulator-tail`. Omit semantic budget options. This image's exact
final entry is:

```forth
' _boot-desktop-session-entry IS _SIMULATOR-SESSION-ENTRY
```

The explicit restoration replaces only that final line in the private image
copy with `_boot-desktop-session-entry`. It preserves the source prefix,
trailing blank lines, line ending, file type and flags; filesystem allocation
and modification metadata may change. It requires a matching source
definition and rejects unrecognized tails. No other guest source is patched.
This only establishes an emulator-ready entry invocation, not successful
architectural boot or sufficient execution time. No separate already-proven
emulator Desktop image was found in the inspected MegaPad run artifacts.

Do not remove failed expectations or rewrite guest state to manufacture a
pass. Inspect the report's error and last CELL snapshot first. Preparation
time, existing guest state, the real-time date, terminal negotiation, and the
emulator's different execution cost remain possible blockers. A timeout is a
failed bounded attempt; it is not a performance measurement of completion.

## Bounds and results

| Input or work | Bound / default |
| --- | --- |
| Prepared image | Regular file, 512 bytes through 32 MiB |
| Journey JSON | At most 256 KiB; duplicate keys and nonstandard JSON constants rejected |
| Journey steps | 1 through 64; only `send_text` and `send_key` |
| Input per step | 1 through 4,096 UTF-8 bytes |
| Expectation | At most 32 positive and 32 absent markers, each 1 through 256 UTF-8 bytes; positive marker required |
| Per-step time | 1 through 120 seconds; default 30 |
| Total cooperative deadline | 1 through 900 seconds; default 240, including preparation |
| Parent process watchdog | Total deadline plus 10 seconds, including native intervals and cleanup |
| Polls | At most 100,000 |
| Geometry | 1–400 columns, 1–200 rows; default 280 by 84 |
| Bank 0 / external / VRAM | 64–1,024 KiB / 0–512 MiB / 0–16 MiB; defaults 1,024 KiB / 320 MiB / 4 MiB |
| Semantic quantum | 1–1,000,000 steps; default 65,536 |
| Optional semantic budget | 1–1,000,000,000 steps; unset by default |
| Font size | 6 through 32; default 12 |

Policies are validated by the production policy parsers. The harness's bounds
do not constitute an operating-system memory limit. Large policies and SDL
surfaces still consume host memory.

Reports contain the checkout commit and dirty status, harness/journey hashes,
native extension paths and hashes, selected runtime descriptor, supplied
policies, image and autoexec hashes before/after selection, copied-image hash
after execution, timing, peak worker RSS, input retries, offers, completed
steps, final status, and cleanup checks. Loaded native extensions must come
from this checkout. Optional artifacts contain initial/final software captures
and CELL text. A successful run returns exit status zero and `complete: true`.

Ordinary failures close the owner, release the display lease and semantic
ownership, detach the terminal driver, close the hybrid composition if used,
verify the original image, and remove the temporary copy. Forced watchdog
termination reports failure and cannot certify those cleanup checks or
temporary-directory removal. The original image is never attached to the
guest for writing. Report output cannot alias any input, including hardlinks.

For the focused regression gate, use the repository's serialized Make entry:

```bash
CC=gcc CXX=g++ make test-sequential \
  VENV_PY=/workspace/scratch/64bce13821f6/runcheck-venv/bin/python \
  MP64_RUNTIME_NAMESPACE=unified-runtime \
  TEST_PATH=tests/test_unified_desktop_benchmark.py
```

This small gate and each prepared-image run should be recorded separately.
Only a successful report from the full eight-step fixture qualifies that
specific image, mode, executor, policy, and checkout combination.
