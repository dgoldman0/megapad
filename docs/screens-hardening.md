# KDOS Screen System — Hardening Plan

Status: **partially implemented** (§1–§7 have landed mechanisms, with the
remaining source-literal defects called out below; §8 is deferred)

Covers: KDOS v1.1 screen registry, tab bar, key dispatch, WVEC widgets

---

## 1. Unregistration Leaves Stale Tail State

**Current behaviour:** `UNREGISTER-SCREEN` exists and compacts all five
per-screen arrays, `SUB-COUNTS`, and both complete subscreen blocks. It
decrements `NSCREENS` and adjusts the current 1-based `SCREEN-ID`. It does not
clear the vacated physical tail, reset `SUBSCREEN-ID`, or choose an invalid
screen sentinel after removing the final row. The final state can therefore be
`NSCREENS = 0`, `SCREEN-ID = 1`, plus stale row-zero data.

**Risk:** Count-respecting code cannot see a vacated row, but an unchecked ID
or the zero-screen header/footer path can consume stale render, label, handler,
or subscreen cells. A retained `SUBSCREEN-ID` can also be out of range for the
fallback screen.

**Remaining hardening:** Clear the newly vacant screen row and both subscreen
blocks, reset `SUBSCREEN-ID` whenever the current row changes, and define an
explicit zero-screen state before any header/footer call is legal.

---

## 2. Capacity Failure Is Now Non-Throwing

**Current behaviour:** `REGISTER-SCREEN` returns `-1` without mutation when
`NSCREENS >= MAX-SCREENS`. `ADD-SUBSCREEN` consumes and silently ignores a
ninth entry once a parent's count reaches `MAX-SUBS`.

The original event-loop-killing `ABORT"` behavior is resolved. Callers still
need to check the screen return value, and `ADD-SUBSCREEN` provides no failure
result. More importantly, neither API validates a supplied parent/screen ID,
so the capacity check is not an address-safety check.

**Remaining hardening:** Give subscreen registration a checked result and
validate every externally supplied ID before deriving a table address.

---

## 3. CATCH Guards Have an Error-Path Stack Leak

**Current behaviour:** Header labels, subscreen labels, renderers, actions, and
per-screen key handlers now have zero-XT checks and/or `CATCH` guards. A
throwing header/tab label prints `?`; renderer/action failures print a visible
error instead of escaping the event loop.

The exact `label-xt ['] EXECUTE CATCH` path is not stack-balanced on a throw.
With the current source `CATCH` sequence, it leaves a saved data-stack-pointer
cell visible to the caller after printing `?`. Repeated failing draws can
therefore grow/corrupt the public data stack even though control survives.

**Remaining hardening:** Centralize dynamic-XT invocation in a helper whose
normal and thrown paths have an explicitly tested stack effect, then use it
for labels, renderers, actions, and key handlers.

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

**Implemented mitigation:** Screen registration and mutation are documented
as main-core-only. The source does not define a screen-request mailbox ABI or
service it from `SCREEN-LOOP`; any background producer therefore needs a
caller-supplied handoff whose mutation is performed on the main core.

**Remaining hardening:** Add a simple spinlock or disable interrupts around
critical sections:

```forth
VARIABLE SCR-LOCK
: SCR-ACQUIRE  BEGIN SCR-LOCK @ 0= UNTIL  -1 SCR-LOCK ! ;
: SCR-RELEASE  0 SCR-LOCK ! ;
```

Wrap `REGISTER-SCREEN`, `RENDER-SCREEN`, and `HANDLE-KEY` in balanced
acquire/release pairs with exception cleanup.

---

## 5. Tab Bar Overflow at High Screen Counts

**Current behaviour:** `SCREEN-HEADER` includes labels only when
`NSCREENS <= 10`; above that it emits compact numeric/hex tabs. This bounds the
worst label-driven growth and implements the original short-term fix.

**Residual risk:** The compact 16-tab form can still wrap on a sufficiently
narrow terminal, pushing content down and the footer off-screen.

**Possible enhancement:** Implement a scrollable tab bar: show a window of N
tabs centred on `SCREEN-ID`, with `<`/`>` indicators for off-screen tabs.
Alternatively, split compact tabs into two rows on narrow terminals.

---

## 6. Key Namespace Exhaustion

**Current behaviour:** Digit keys `0`–`9` switch to screens 0–9.  Hex
keys `a`–`f` switch to screens 10–15.  Global keys (`q`, `r`, `n`, `p`,
`A`, `[`, `]`, Enter, Space) are reserved.  Per-screen handlers
(`CALL-SCREEN-KEY`) run first and can intercept any key.

**Residual risk:** With 16 screens registered, `a`–`f` are consumed by screen
switching, colliding with any user-land screen that wants those keys for its
own commands. The documented priority chain lets a per-screen handler
intercept them first, but that handler must return a consumed flag.

**Current documented order:**

1. Per-screen handler via `SCR-KEY-XT` (returns consumed flag)
2. Digit/hex screen switching
3. Global bindings (`q`/`r`/`A`/`n`/`p`/`[`/`]`/Enter/Space)

**Possible enhancement:** Use a modifier key (for example, Ctrl or Alt) for
screen switching beyond 10, freeing `a`–`f` for screen-local use.
Alternatively, allow screens to declare key masks that `HANDLE-KEY` respects
before global dispatch.

---

## 7. Selection Reset on Dynamic Registration

**Current behaviour:** `SWITCH-SCREEN` resets `SCR-SEL`, `SCR-MAX`, and
`SUBSCREEN-ID`. `REGISTER-SCREEN` also resets `SCR-SEL` and `SCR-MAX` when the
new zero-based slot is the one named by the current 1-based `SCREEN-ID`.

The original stale selection issue is resolved. Registration does not clear
the new row's raw `SUB-XT`/`SUB-LBL-XT` cells, but its zero `SUB-COUNTS` value
keeps them logically inactive until added.

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
| 1 | CATCH around EXECUTE (§3) | High | Small | Guard added; thrown-path stack leak open |
| 2 | ABORT → checked return (§2) | High | Small | **Done** |
| 3 | Document main-core-only (§4 short-term) | Medium | Tiny | **Done** |
| 4 | Tab bar overflow (§5) | Medium | Medium | **Done** |
| 5 | Unregistration (§1) | Medium | Medium | Compaction done; tail/reset cleanup open |
| 6 | Key namespace docs (§6) | Low | Tiny | **Done** |
| 7 | SCR-SEL reset (§7) | Low | Small | **Done** |
| 8 | W.INPUT enhancements (§8) | Low | Large | Deferred |
| 9 | Spinlock for multicore (§4 long-term) | Low | Medium | Deferred |
