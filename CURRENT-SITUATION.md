# Megapad-64 — Current Situation

**Date:** 2026-02-19  
**Branch:** `features/testing` (merging to `main`)  
**Status:** All 1,336 tests passing.

---

## 1. What was done this session

### Bug fixes (committed to `main` at `76e2ae2`)

- **`_CI-EQ` fix** — changed `OVER` to `DUP` so case-insensitive comparison
  actually lowercases the correct character.
- **`_HTTP-PARSE-CLEN` rewrite** — replaced brittle 3-deep stack juggling
  with `_CL-PTR` / `_CL-ACC` variables.
- **`NET-IDLE`** added to all protocol handler poll loops (HTTP, HTTPS,
  FTP, Gopher).

### Test restructuring (`features/testing` branch)

- **Moved all test files to `tests/` directory:** `test_system.py`,
  `test_megapad64.py`, `test_networking.py`, `conftest.py`, `__init__.py`.
- **`test_monitor.py` stays at project root** — it's a dashboard tool,
  not a test file.
- **`test_live.py` deleted** — replaced by headless mode (see below).
- **Path fixes:** All `__file__`-relative paths updated to use
  `PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))`.

### Headless TCP terminal server (new feature)

- `--headless` boots the emulator and serves UART I/O over TCP (CPU runs
  in a background thread, default port 6464).
- `--headless-port PORT` to configure the listen port.
- `--connect [HOST:]PORT` built-in raw terminal client.
- Status written to `/tmp/megapad_headless.json` (PID + port).
- Multiple simultaneous clients supported; Ctrl+] to disconnect.
- Works with plain `nc` / `telnet` too.

## 2. What's still broken

### Stack underflow in SCROLL-GET

After fetching HTTP data, the user still sees "Stack underflow."  The
`_CI-EQ` and `_HTTP-PARSE-CLEN` fixes are correct and tested (27 unit
tests pass), but the underflow is elsewhere in the SCROLL-GET flow
after the response is received.

### Dictionary full

After loading all OS modules (autoexec.f → graphics.f → tools.f), the
system dictionary is full.  Users can't define new words at the REPL.

## 3. Files to know about

| File | What it does |
|------|-------------|
| `tools.f` | SCROLL client (HTTP/HTTPS/FTP/Gopher) + ED line editor |
| `cli.py` | Main entry point. Interactive console, debug monitor, headless server |
| `tests/conftest.py` | pytest configuration + LiveTestMonitor plugin |
| `test_monitor.py` | CLI dashboard that reads test status JSON |
| `Makefile` | Build & test entry point — all test invocation goes through here |
| `bios.asm` | BIOS Forth — prompt is `> `, success ` ok\n`, error `X ? (not found)\n` |
| `system.py` | `MegapadSystem` — the emulator core + C++ accel fast path |
| `tests/test_system.py` | 1,299 unit tests using `_run_kdos()` snapshot helper |
| `tests/test_megapad64.py` | 23 CPU instruction tests |
| `tests/test_networking.py` | 47 networking tests (simulated + real TAP) |

## 4. How to run things

```bash
make test           # unit tests (background, ~1 min with C++ accel)
make test-one K=X   # single class/test
make test-status    # check progress
make test-net       # networking tests (requires TAP device)
make disk           # rebuild sample.img

# Interactive emulator:
python cli.py --bios bios.asm --storage sample.img --nic-tap

# Headless mode (TCP terminal server):
python cli.py --bios bios.asm --storage sample.img --headless
python cli.py --connect localhost:6464
```

## 5. Test counts

- **Unit tests:** 1,336 passing (via `make test`)
- **Networking tests:** 47 (via `make test-net`, requires TAP device)
