# Development Sessions

`session.py` provides one synchronous owner for a MegaPad machine and its
terminal. It is intended for tests, development automation, and coding agents
that need to interact with the guest without opening pygame.

## Python API

```python
from session import MachineSession

with MachineSession.from_bios("bios.asm", cols=80, rows=30) as session:
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

Direct sessions use deterministic cycle-derived RTC time by default. Pass
`realtime_clock=True` for interactive or external-network work whose deadlines
must continue to track host time while the emulator is idle or variably loaded.

## Shared Live Session

Use the shared runtime when a person and an automation client need to watch and
control the same running machine. One server process owns execution and all
machine mutations. The pygame viewer only renders revisioned terminal snapshots
and forwards keyboard input, so closing or reconnecting the viewer does not
reset the guest.

Start the machine owner from the workspace root:

```bash
python3 megapad/session_server.py
```

To attach the shared machine to an already configured Linux TAP interface:

```bash
python3 megapad/session_server.py --nic-tap mp64tap0
```

The server refuses startup if the TAP device is missing or inaccessible; it
does not create interfaces or alter host routing on the user's behalf.

Shared sessions use the realtime RTC by default because they are interactive
and may participate in external protocols. Pass `--virtual-clock` for a fully
deterministic cycle-derived clock in isolated tests.

Attach the live viewer in another terminal:

```bash
python3 megapad/session_viewer.py \
  --font akashic/assets/fonts/DejaVuSansMono.ttf
```

The viewer accepts normal text and terminal keys. `Ctrl+F5` toggles pause,
`Ctrl+F10` pauses and executes one instruction, `Ctrl+R` resets the guest,
and `Ctrl+Q` closes only the viewer. Bare function keys reach the guest.

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
and can be overridden with `--socket` on all three commands. The protocol is
newline-delimited JSON over that local socket. Screen reads accept a revision
number and return no cell payload when nothing has changed.

Other control commands are `pause`, `resume`, `step`, `reset`, `resize`,
`key`, `raw`, and `shutdown`. `step` requires the machine to be paused. The
viewer and CLI are peers: input from either reaches the one UART queue owned by
the server.

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

`resize(cols, rows)` updates both the terminal grid and the guest UART geometry
device.

## Observation

`TerminalSnapshot` is immutable. It contains:

- Terminal dimensions.
- Every glyph, foreground color, background color, and attribute mask.
- Cursor position and visibility.
- Alternate-screen state.
- Text extraction and search helpers.
- JSON, text, and PNG writers.

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
