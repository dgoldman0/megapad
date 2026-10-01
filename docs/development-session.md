# Development Sessions

`emulator/session.py` provides one synchronous owner for a MegaPad machine
and its terminal. It is intended for tests, development automation, and coding agents
that need to interact with the guest without opening pygame.

## Python API

```python
from emulator.session import MachineSession

with MachineSession.from_bios(
    "bios.asm",
    cols=80,
    rows=30,
    lanes=None,
) as session:
    session.boot()
    boot = session.wait_for_idle(max_steps=2_000_000)
    if boot.reason != "idle":
        raise RuntimeError(boot)

    session.send_text("6 7 * .\n")
    result = session.wait_for_text("42 ", max_steps=2_000_000)
    if not result.matched:
        raise RuntimeError(result)

    screen = session.snapshot()
    screen.write_text("screen.txt")
    screen.write_json("screen.cells.json")
    screen.write_png("screen.png")
```

`RunReport` records the stop reason, executed steps, native batches, elapsed
time, output byte count, and whether a text wait matched.

`MachineSession.from_bios(..., nic_backend=backend)` attaches any MegaPad NIC
backend to the owned system. Closing the session stops that backend and releases
its listener, TAP descriptor, or tunnel resources.

`lanes=None` selects one, two, or four fixed host execution lanes from the
configured guest topology and the process CPU affinity. Pass `lanes=1` for
the helper-free diagnostic reference, or `lanes=2`/`lanes=4` for an explicit
width. The selection is immutable for the machine lifetime.

Direct sessions use deterministic cycle-derived RTC time by default. Pass
`realtime_clock=True` for interactive or external-network work whose deadlines
must continue to track host time while the emulator is idle or variably loaded.

## Common session boundary

`shared/session.py` owns terminal configuration, immutable captures, rich
terminal attachment, display offers, acknowledgments, and input admission.
`shared_session.py` owns the JSON protocol, socket/display leases, shared
mutation lock, and `SharedSessionOwner` lifecycle interface. Neither imports
an execution backend. Common CLI policy decoders live in
`shared/session_options.py`.

The architectural adapters are `emulator.session.MachineSession` and
`emulator.shared_session.SharedMachine`. The hosted adapters are
`simulator.session.SimulatorMachineSession` and `SimulatorSharedMachine`.
Both inherit the common authorities directly. Construction, run loops,
work accounting, diagnostics, and backend resource release stay in their
adapters. The simulator does not inherit architectural run or BIOS methods.
The viewer imports only common terminal/protocol interfaces.

Import terminal types from `shared.session`, the architectural session from
`emulator.session`, and the shared architectural owner from
`emulator.shared_session`. The root `session.py` shim and
`shared_session.SharedMachine` have been removed. The BIOS/KDOS benchmark
detects a selected checkout's layout before importing it: package checkouts
use `emulator.session` and `emulator.system`; historical flat checkouts use
their root modules. It validates source and imported-module provenance, and
an import failure in a package checkout remains an error.

Backend server interfaces live in `emulator.server`, `simulator.server`, and
`hybrid.server`. Each owns its argument parser and `main(argv=None)`;
`megapad.py --mode MODE` loads the selected package lazily; top-level help
requires no backend imports.
Simulator and hybrid image preparation also expose `prepare_server(args)`
without opening a listener.

Both detailed and lightweight status contain the same `runtime` descriptor:

| Field | Emulator | Simulator | Hybrid |
|---|---|---|---|
| `mode` | `emulator` | `simulator` | `hybrid` |
| `executor` | `native` | Selected `python` or `native` | Selected semantic `python` or `native` |
| `step_unit` | `mp64_instruction` | `semantic_step` | `semantic_step` |
| `step_request_unit` | `mp64_instruction` | `semantic_boundary` | `semantic_boundary` |
| `batch_unit` | `instruction_batch` | `semantic_boundary` | `semantic_boundary` |
| `timing.model` | `instruction_batched` | `semantic` | `semantic` |
| `timing.models_shared_clock_latency` | `false` | `false` | `false` |
| `timing.timer_unit` | `mp64_system_cycle` | `semantic_step` | `semantic_step` |
| `timing.rtc_mode` | `virtual` or `realtime` | `manual` or `host_monotonic` | `manual` or `host_monotonic` |
| `capabilities.machine_code` | `true` | `false` | `true`, declared routines only |
| `capabilities.cpu_diagnostics` | `true` | `false` | `false` |
| `capabilities.network_diagnostics` | `true` | `false` | `false` |
| `capabilities.reset` | `true` | `false` | `false` |
| `capabilities.host_profiling` | `true` | `false` | `false` |

`hybrid.session.HybridSession` retains the semantic session backend and its
terminal/continuation authority. Routine words run on the native routine runner
over the same ordinary buffers. `HybridSharedMachine` adds `machine_execution`
with the ABI identity, the routines, lifetime machine instruction, cycle,
segment, transition and callback counts, and the machine quantum and budget.
Machine cycles do not advance the semantic timer or claim whole-application
shared-clock timing. Status advertises declared routines and semantic
callbacks, and denies arbitrary machine code, machine MMIO, native BIOS boot,
multicore execution and native snapshots. The manifest and routine contract
are in [the hybrid design](hybrid-runtime.md).

The executor identifies the selected engine; native execution can include
Python fallbacks. Capabilities identify supported session operations,
independently of whether optional facilities are enabled. Existing emulator
instruction batches do not enable the system's separate strict cycle-bounded
runner. `instruction_batched` advances functional time once per equal-credit
scheduler round, using the maximum accumulated per-core cycle cost. Wake and
interrupt delivery occur at the model's round boundaries. CPU memory accesses
are ordered without strict main-bus timing. Its clock can drive devices and
report progress, but it cannot measure shared-clock worker wake latency or bus
contention. Host worker count and a native executor do not change that model.

Use `MegapadSystem.run_cycle_batch(...)` with a virtual RTC and a supported
full-core-only topology for modeled wake, interrupt, and bus timing. Its
`SystemRunStats.timing_model` is `strict_shared_clock`, and
`models_shared_clock_latency` is true. `run_batch_stats(...)` reports
`instruction_batched` and false. Zero-budget and host-backpressure results
retain the requested model. The same cycle field names exist in both results;
compare them only with their timing model recorded. The strict model does not
establish physical RTL latency or guarantee multicore speedup. The shared
application session currently selects instruction batches, including its
paused one-instruction requests.

Semantic work does not claim hardware cycles. The standalone simulator
server binds its RTC to host monotonic time; a directly constructed runtime
starts with a manually advanced RTC. Reading its RTC policy does not sample
or advance the clock.

Simulator reset still requires a newly prepared runtime. The established
status keys and detailed diagnostic selection are preserved. Shared terminal
status now consistently reports bytes, rather than frame counts, in
`rich_terminal.frame_bytes_by_type` for both adapters.

## Shared Live Session

Use the shared runtime when a person and an automation client need to watch and
control the same running machine. One server process owns execution and all
machine mutations. The pygame viewer renders revisioned immutable display views
and forwards keyboard input, so closing or reconnecting the viewer does not
reset the guest. A baseline view is a CELL snapshot; a rich view preserves the
CELL and retained planes of one composite revision through the shared-session
boundary and composites both before acknowledging that revision as displayed.

Start the machine owner from the workspace root:

```bash
python3 megapad/megapad.py --mode emulator
```

The shared server accepts the same policy as
`--lanes {auto,1,2,4}` and prints the resolved width at startup.

To attach the shared machine to an already configured Linux TAP interface:

```bash
python3 megapad/megapad.py --mode emulator --nic-tap mp64tap0
```

The server refuses startup if the TAP device is missing or inaccessible; it
does not create interfaces or alter host routing on the user's behalf.

Audible one-shot PCM playback is likewise explicit opt-in:

```bash
python3 megapad/megapad.py --mode emulator --audio
```

Without `--audio`, the guest audio device still captures every successful
submission deterministically for tests and inspection, but advertises no
audible sink. With it, the owner process opens the pygame mixer at the exact
rate and channel count requested by each submission. The viewer initializes
video and fonts only, so it cannot contend for the host audio device. Server
shutdown stops the owned voice and releases the mixer.

Shared sessions use the realtime RTC by default because they are interactive
and may participate in external protocols. Pass `--virtual-clock` for a fully
deterministic cycle-derived clock in isolated tests.

### Hosted simulator owner

`megapad.py --mode simulator` serves the same shared-session protocol from the hosted
semantic simulator instead of the emulator. It prepares an MP64FS image, runs
its ordinary autoexec, and only then exposes the socket:

```bash
python3 megapad/megapad.py --mode simulator --executor native \
  --storage path/to/image.img --ext-mem-mib 128
```

It takes `--storage` (required), `--socket`, the memory sizes (`--ram-kib`,
`--ext-mem-mib`, `--vram-mib`), and the geometry (`--cols`, `--rows`). It also
takes `--semantic-step-budget`, `--semantic-quantum-steps`, `--paused`, and
the complete caller-owned `--rich-terminal-policy` and
`--retained-terminal-policy` JSON. It has no NIC or audible audio option.
`--executor python|native|auto` takes precedence over `MEGAFORTH_EXECUTOR`.
With neither set, the production server requires native execution; only
`auto` permits fallback to Python. Embedded `MegaForthRuntime` construction
still defaults to the Python reference. `MEGAFORTH_QUANTUM_STEPS` sets the semantic steps between
host owner boundaries when the option is absent. The default is 65,536 with
the native executor and 8,192 with the Python reference. The viewer and `session_ctl.py`
attach exactly as they do to the emulator owner. Akashic's
`local_testing/akashic_tui.py serve|accept --backend simulator` builds the
image and supplies the policies for the ordinary Desktop.

Attach the live viewer in another terminal:

```bash
python3 megapad/session_viewer.py \
  --font akashic/assets/fonts/DejaVuSansMono.ttf
```

The viewer accepts composed text and Ctrl/Alt/Shift character chords. It also
preserves modifiers on arrows, Home/End, Insert/Delete, PgUp/PgDn, and
F5--F12. Held navigation and editing keys repeat after 400 ms at 35 ms
intervals. `Ctrl+F5` toggles pause, `Ctrl+F10` pauses and executes one
instruction, `Ctrl+R` resets the guest, and `Ctrl+Q` closes only the viewer.
Bare function keys reach the guest.

The viewer polls the session at `--fps` (30 by default) but redraws only
when something it draws changes: a new display offer, the screen revision,
hover or press, a visible cursor's blink, the window, or the status line.
An unchanged window is neither recomposed nor flipped, so an idle viewer
uses little CPU. A status-only change reuses the last composed frame. A
frame that does change is repainted only where it changed, exactly as a
full composition would paint it, and only those rectangles of the window
are updated (`docs/viewer-partial-repaint.md`).

Control or inspect that same machine from another process:

```bash
python3 megapad/session_ctl.py status
python3 megapad/session_ctl.py network
python3 megapad/session_ctl.py forth _ASHELL-LAST-TICK DESK-DESC
python3 megapad/session_ctl.py peek 0x1000 4
python3 megapad/session_ctl.py send '6 7 * .' --enter
python3 megapad/session_ctl.py wait-text '42 ' --scope raw
python3 megapad/session_ctl.py text
python3 megapad/session_ctl.py capture \
  --text local_testing/out/live.txt \
  --json local_testing/out/live.cells.json \
  --png local_testing/out/live.png
```

The default Unix socket is `/tmp/megapad-session-<uid>.sock`, is mode `0600`,
and can be overridden with `--socket` on all three commands. The local control
protocol is newline-delimited JSON over that local socket. Screen reads accept
a revision number and return no cell payload when nothing has changed. Display
holders use the independent `since_offer` cursor so a retained display offer
can arrive even when that CELL revision is unchanged.

Set `MP64_RUNTIME_NAMESPACE` in the server, controller, and viewer
environments to use
`/tmp/megapad-runtime-<uid>-<namespace>/session.sock` instead. The runtime
directory is owned by the current UID with mode `0700`; unsafe pre-existing
paths are rejected. This lets parallel checkouts run independent default
sessions without requiring a different `--socket` argument on every command.
An explicit `--socket` continues to override the default.

Other control commands are `pause`, `resume`, `step`, `reset`, `resize`,
`key`, `raw`, and `shutdown`. `step` requires the machine to be paused. The
viewer and CLI are peers: the server serializes input from both through the
one terminal owner. Baseline ANSI input enters the UART stream; an active
rich-terminal attachment instead receives normalized, framed input.

`status.generation` identifies the current successful boot/reset epoch. Every
`send_text`, `send_key`, and
`resize` request must echo that generation; the server returns
`stale_generation` without mutating the new machine when a request races a
reset. Input responses report `progress`, `backpressured`, `invalid`, `stale`,
`failed`, or the nonfatal `stale_display` authorization refusal, and use
all-or-zero `accepted_bytes`, `accepted_events`, or `accepted` fields. A
rich-terminal resize with `progress` may be an accepted latest-wins intent:
`requested` is the intent, while `cols`, `rows`, and `revision` describe the
currently visible snapshot until a replacement snapshot commits.

A renderer connection first calls `claim_display`. The claim is idempotent for
that connection and exclusive until disconnect. Only the holder receives a
`display_offer` from `screen`; ordinary observers retain the existing
CELL-snapshot response shape. The holder supplies both cursors:
`since=<CELL revision>` and `since_offer=<positive offer ID>`, with zero as the
initial offer sentinel. An offer contains only the immutable renderer DTOs:
its positive `offer_id`, the complete `DisplayScope`, the CELL snapshot, and
the projected `RetainedDrawPlane`. Its ordered glyph runs carry exact bounds,
foreground, background, CELL attributes, and UTF-8 text; the ordinary TUI
screen transaction must populate that bounded representation for Desk, Pad,
and Daybook. The offer never contains a `CompositeTerminalView`, a hidden
retained rebuild target, or model authority.

A holder may also pass `base_offer=<offer ID>`, naming the offer it last
presented and still holds, or zero when it holds none. When that is the
session's presented offer and the CELL geometry is unchanged, the server sends
the new offer as its changes against that base, which is usually a small part
of the screen. Such an offer also carries `base_offer_id`. Its `cell` carries
`changed_rows` in place of `runs`: one `[row, runs]` pair for each row that
differs from the base, whose runs cover exactly that row's columns. Its
retained plane lists every region with its complete header. A region with the
same owner, generation and region ID as a base region may carry `removed` and
`changed` in place of `draws`: the keys of base draws it no longer has, each
`["object", object_id]` or `["control", control_id]`, and every draw that is
new or differs from the base draw with its key. Series histories and IMAGE
manifests are always complete. Rows and draws that are not sent are the
base's. The viewer rebuilds the complete immutable offer from its base before
staging it, so composition, hit maps and `present` see exactly what a
complete offer carries. Without that base, after a reconnect, reclaim, reset,
refused presentation or geometry change, the offer is complete.

After drawing the complete offer, the holder calls `present` with the current
reset `generation`, exact `display_offer_id`, and full `display_scope` returned
by `screen`. `presented` promotes that physical view and returns its new session
`revision`; retrying the same delivered proof returns `duplicate`. Guessed,
foreign, or replaced proofs return `stale_display`, and an old reset generation
returns `stale_generation`, without failing the machine. Disconnect revokes
both unacknowledged and acknowledged physical ownership. The last CELL snapshot
remains available to observers while the newest composite is immediately
re-offered under a fresh higher offer ID to a successor.

When a retained policy is configured, `send_text`, `send_key`, and `resize`
also carry the exact acknowledged `display_offer_id` and `display_scope`. A
nonholder or mismatched proof receives `stale_display`; the holder receives
ordinary `backpressured` before its current physical ACK. This prevents queued
input from migrating to a newer view. ANSI sessions and APT sessions without a
retained policy keep their existing input behavior and do not require a display
claim or proof.

`raw` uses lifetime-monotonic absolute byte cursors. Its response reports the
requested slice's `start`, the oldest retained `available_from` offset, the
next `offset`, and whether bounded history made the request `truncated`.
Replacement-decoded `text` is accompanied by lossless `data_base64`. Reset
clears retained ANSI diagnostics without reusing old offsets.

`status` includes all CPU registers, RTC mode and values, NIC counters, the
current Forth word and BIOS primitive, and bounded data/return-stack snapshots.
Protocol clients that only need progress can send `{"detailed": false}` with
the `status` request. That compact response omits CPU, Forth, RTC, and NIC
diagnostics; the live viewer uses it for its periodic status-bar refresh so it
does not repeatedly walk the guest dictionary.
`network` reports guest and backend counters and the backend's bounded trace.
`forth` resolves named dictionary entries and CREATE data fields; `peek` reads
one through 256 consecutive 64-bit cells. These diagnostics are read-only and
remain behind the owner-only local socket.

### Warm-reset status

`reset` restarts the CPU while retaining RAM and the attached storage image.
The BIOS now clears its RAM-backed UART transmit descriptor during boot, so an
interrupted output batch cannot suppress the next boot banner. A focused BIOS
session regression covers that case.

A separate issue remains under investigation: resetting a fully loaded
Akashic Agent TUI while it is awaiting approval can leave the emulated terminal
blank even though the CPU restarts and the BIOS bytes remain intact. Fresh
boots, uninterrupted shared sessions, terminal capture, and Akashic's native
VFS persistence are unaffected. Until the remaining device/session state is
isolated, test crash recovery by reopening the runtime against the same VFS or
by starting a fresh machine with the preserved disk image; do not use an
in-place full-Akashic reset as a persistence acceptance test.

When this remaining reset defect returns to active work, include the related
KDOS language cleanup in the same maintenance pass. Preserve the standard raw
behavior of `S"`; add a distinct Forth 2012-style `S\"` escaped-string word for
both interpretation and compilation, with explicit quote, backslash, newline,
carriage-return, tab, and bounded hexadecimal escapes. Cover malformed and
unterminated input as well as compiled-literal lifetime, then replace Akashic
ASCII-34 workarounds and documentation examples only after the word exists.

## Input

`send_text()` accepts `str` or `bytes`. Strings are encoded as UTF-8.

`send_key()` accepts printable one-character strings and these names:

- `enter`, `escape`, `tab`, `backspace`, `delete`
- `up`, `down`, `left`, `right`
- `home`, `end`, `pageup`, `pagedown`, `insert`
- `f1` through `f12`
- `ctrl+a` through `ctrl+z`
- combined character modifiers such as `ctrl+shift+s` (CSI-u encoded)
- `alt+<character>`

In baseline ANSI mode, `resize(cols, rows)` updates both the terminal grid and
the guest UART geometry device immediately. In active rich-terminal mode it
records a bounded latest-wins request; the selected geometry changes atomically
with its framed RESIZE only when transaction and queue ordering permit it, and
the visible geometry changes when the required replacement snapshot commits.

## Observation

`TerminalSnapshot` is the immutable CELL compatibility and diagnostic
projection. It contains:

- Terminal dimensions.
- Every glyph, foreground color, background color, and attribute mask.
- Cursor position and visibility.
- Alternate-screen state.
- Text extraction and search helpers.
- JSON, text, and PNG writers.

An active retained session additionally offers the immutable retained plane
and global composite scope to the exclusive physical display holder. Text/search
helpers and CELL-only PNG output may continue to use `TerminalSnapshot`, but a
physical retained display must render and acknowledge the complete
`TerminalDisplayOffer`; it must not treat a CELL-only projection as the complete
displayed view.

JSON/cell assertions should be preferred for tests. PNG output is intended for
visual inspection and build artifacts. Pass `font_path` to `write_png()` when a
specific font is required; otherwise MegaPad looks for DejaVu Sans Mono and
falls back to Pillow's default font.

## Scenario CLI

`dev_session.py` executes ordered actions from JSON:

```bash
python3 dev_session.py path/to/scenario.json
```

Example:

```json
{
  "name": "bios-smoke",
  "machine": {
    "bios": "bios.asm",
    "cols": 80,
    "rows": 30,
    "lanes": "auto",
    "batch_steps": 100000
  },
  "actions": [
    {"type": "wait_idle", "max_steps": 2000000},
    {"type": "send_text", "text": "6 7 * .\n"},
    {
      "type": "wait_text",
      "text": "42 ",
      "scope": "raw",
      "max_steps": 2000000
    },
    {
      "type": "capture",
      "text": "out/screen.txt",
      "json": "out/screen.cells.json",
      "png": "out/screen.png"
    }
  ],
  "report": "out/report.json"
}
```

Paths inside a scenario are resolved relative to the scenario file. Supported
actions are:

- `run`
- `wait_idle`
- `wait_text` with `raw` or `screen` scope
- `send_text`
- `send_key`
- `resize`
- `clear_output`
- `capture`
- `print_screen`

Waits have explicit instruction and wall-time budgets. Failure output includes
the stop reason and recent UART text.

## Performance Benchmarks

Run the interactive hot-path benchmark:

```bash
python3 bench_uart_poll.py --steps 50000000
```

Run the host binary-loading benchmark:

```bash
python3 bench_load_binary.py --mib 8
```

The UART benchmark verifies that guest `KEY?` polling stays inside C++ and
reports any Python MMIO reads. The load benchmark compares the current slice
loader against the previous byte-loop behavior.
