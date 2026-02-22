# KDOS Screen System — Hardening Plan

Status: **implemented** (§1–§7; §8 deferred — enhancements, not bugs)
Covers: KDOS v1.1 screen registry, tab bar, key dispatch, WVEC widgets

---

## 1. No Screen Unregistration

**Current behaviour:** `REGISTER-SCREEN` appends to flat arrays (`SCR-XT`,
`SCR-LBL-XT`, `SCR-FLAGS`, etc.) and bumps `NSCREENS`.  There is no
`UNREGISTER-SCREEN`.  Once registered, a screen lives forever.

**Risk:** User-land experimentation or hot-reloading Forth files can leave
stale/broken screens in the tab bar with no way to remove them short of a
full system reset.

**Proposed fix:**
- Add `UNREGISTER-SCREEN ( id -- )` that clears the slot and compacts the
  arrays (shift entries above `id` down by one, decrement `NSCREENS`).
- Update `SCREEN-ID` and `SCR-SEL` if the current screen was removed or
  its index shifted.
- Alternatively, support a "hidden" flag in `SCR-FLAGS` (bit 1) so a
  screen can be disabled without compaction.  Simpler, but leaves gaps in
  the key map (digit `3` could map to nothing).

---

## 2. ABORT on Table Full Kills the Event Loop

**Current behaviour:** `REGISTER-SCREEN` uses
`ABORT" screen table full"` when `NSCREENS >= MAX-SCREENS`.  If this fires
during `SCREEN-LOOP`, KDOS dies and drops to the REPL with no cleanup.

**Risk:** Any dynamically-registered screen that pushes past 16 kills the
TUI instantly.  User sees raw Forth prompt with no indication of what
happened.

**Proposed fix:**
- Replace `ABORT"` with a checked return:
  ```forth
  NSCREENS @ MAX-SCREENS >= IF
      -1 ( error sentinel ) EXIT
  THEN
  ```
  Callers check the return value.  Or use `CATCH`/`THROW` — KDOS already
  has the EXCEPTION word set (§1.2) — and wrap `REGISTER-SCREEN` calls in
  a `CATCH` that displays an error bar instead of dying.
- Same treatment for `ADD-SUBSCREEN`'s `ABORT" sub table full"`.

---

## 3. No CATCH Around Render/Key EXECUTE

**Current behaviour:** `RENDER-SCREEN`, `HANDLE-KEY`, `DO-SELECT`, and
`CALL-SCREEN-KEY` all do bare `EXECUTE` on xts pulled from the registry
tables.  If the xt is zero (uninitialised) or the word it names crashes,
the event loop dies.

**Risk:** A user-land screen with a buggy render word aborts the entire
TUI.  Zero-xt dispatch (unlikely but possible if memory is corrupted)
causes undefined CPU behaviour.

**Proposed fix:**
- Guard every `EXECUTE` in the dispatch path with `CATCH`:
  ```forth
  ['] my-xt CATCH IF
      ." [screen error]" CR
  THEN
  ```
- For zero-xt slots, the existing `DUP 0<> IF EXECUTE ELSE DROP THEN`
  pattern (already used for `SCR-ACT-XT` and `SCR-KEY-XT`) is good.
  Extend it to `SCR-XT` and `SCR-LBL-XT` which currently assume non-zero.

---

## 4. Race Conditions with Multicore / Tasks

**Current behaviour:** All screen state (`NSCREENS`, `SCREEN-ID`,
`SCR-SEL`, the SCR-* arrays) lives in shared dictionary memory.
`SCREEN-LOOP` polls `KEY?` and `CYCLES` from one core.  Other cores can
read/write any Forth variable via the shared bus.

**Risk:** If a background task (e.g. on core 1) calls `REGISTER-SCREEN`
while core 0 is mid-render in the `NSCREENS @ 0 DO` header loop, the loop
bound was already captured but the array content changed.  Result: stale
label xt, possible zero-execute.  Same applies to `SWITCH-SCREEN` or any
write to `SCREEN-ID` from another core.

**Proposed fix:**
- **Short-term:** Document that screen registration is main-core-only.
  Background tasks use the mailbox (`mp64_mailbox.v`) to request
  registration; the event loop services the request between iterations.
- **Long-term:** Add a simple spinlock or disable interrupts around
  critical sections:
  ```forth
  VARIABLE SCR-LOCK
  : SCR-ACQUIRE  BEGIN SCR-LOCK @ 0= UNTIL  -1 SCR-LOCK ! ;
  : SCR-RELEASE  0 SCR-LOCK ! ;
  ```
  Wrap `REGISTER-SCREEN`, `RENDER-SCREEN`, and `HANDLE-KEY` in
  acquire/release pairs.

---

## 5. Tab Bar Overflow at High Screen Counts

**Current behaviour:** `SCREEN-HEADER` renders all `NSCREENS` tabs in a
single line: `" [0]Home [1]Bufs [2]Kern ..."`.  At 9 screens this already
approaches 72 columns.  With 16 screens, it wraps past 80 columns.

**Risk:** Broken TUI layout — wrapped tab bar pushes content down, footer
may scroll off-screen.

**Proposed fix:**
- Truncate labels beyond a certain screen count (e.g. show `[a]` without
  the label text when count > 10).
- Or implement a scrollable tab bar: show a window of N tabs centred on
  `SCREEN-ID`, with `<`/`>` indicators for off-screen tabs.
- Or split into two rows when count > 10.

---

## 6. Key Namespace Exhaustion

**Current behaviour:** Digit keys `0`–`9` switch to screens 0–9.  Hex
keys `a`–`f` switch to screens 10–15.  Global keys (`q`, `r`, `n`, `p`,
`A`, `[`, `]`, Enter, Space) are reserved.  Per-screen handlers
(`CALL-SCREEN-KEY`) run first and can intercept any key.

**Risk:** With 16 screens registered, `a`–`f` are consumed by screen
switching, colliding with any user-land screen that wants those keys for
its own commands.  The per-screen handler can intercept (it runs first),
but the user must know to return a consumed flag — undocumented footgun.

**Proposed fix:**
- Document the key priority chain clearly in the KDOS reference:
  1. Per-screen handler via `SCR-KEY-XT` (returns consumed flag)
  2. Digit/hex screen switching
  3. Global bindings (q/r/A/n/p/[/]/Enter/Space)
- Consider a modifier key (e.g. Ctrl or Alt prefix) for screen switching
  beyond 10, freeing `a`–`f` for screen-local use.
- Or allow screens to declare "key masks" — a bitmap of keys they claim,
  which `HANDLE-KEY` skips for global dispatch.

---

## 7. SCR-SEL / SCR-MAX Not Reset on Dynamic Registration

**Current behaviour:** `SWITCH-SCREEN` resets `SCR-SEL` and `SCR-MAX`
based on the screen's selectable flag.  But `REGISTER-SCREEN` doesn't
touch these — if you register a new screen while viewing it (edge case),
the selection state is stale.

**Proposed fix:**
- `REGISTER-SCREEN` should call `SWITCH-SCREEN` for the new screen, or
  at minimum reset `SCR-MAX` to 0 for the new slot.
- Already zeroes `SUB-COUNTS` for the new slot — do the same for any
  per-screen state.

---

## 8. W.INPUT / TUI-INPUT Hardening

Separately from the screen registry, the new input widget has its own
surface:

| Issue | Detail |
|-------|--------|
| **No timeout** | `TUI-INPUT` blocks on `KEY` forever.  A stuck UART means a frozen TUI.  Consider a `KEY-TIMEOUT` variant. |
| **No history** | No up-arrow recall of previous input.  Arrow keys are consumed and discarded. |
| **No cursor movement** | Left/right arrow within the line is not supported.  Backspace only erases from the end. |
| **Buffer size trust** | `W.INPUT` passes `maxlen` to the backend, but the backend must enforce it.  A buggy custom backend installed via `WV-INPUT WV!` could overrun. |

These are enhancements, not bugs — the current implementation is correct
for its spec.  Listed here for completeness.

---

## Priority Order

| # | Item | Severity | Effort | Status |
|---|------|----------|--------|--------|
| 1 | CATCH around EXECUTE (§3) | High | Small | **Done** |
| 2 | ABORT → checked return (§2) | High | Small | **Done** |
| 3 | Document main-core-only (§4 short-term) | Medium | Tiny | **Done** |
| 4 | Tab bar overflow (§5) | Medium | Medium | **Done** |
| 5 | Unregistration (§1) | Medium | Medium | **Done** |
| 6 | Key namespace docs (§6) | Low | Tiny | **Done** |
| 7 | SCR-SEL reset (§7) | Low | Small | **Done** |
| 8 | W.INPUT enhancements (§8) | Low | Large | Deferred |
| 9 | Spinlock for multicore (§4 long-term) | Low | Medium | Deferred |
