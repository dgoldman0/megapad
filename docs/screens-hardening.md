# KDOS Screen System — Hardening Plan

Status: **partially implemented** (§1–§7 have landed mechanisms; §8 input
repair and the qualified SDL defects below remain open)

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
| **No timeout** | `TUI-INPUT` blocks on `KEY` forever. A stuck UART or truncated CSI means a frozen TUI. Consider a `KEY-TIMEOUT` variant or a nonblocking parser. |
| **Parameterized CSI stack leak** | The CSI loop drops only the final byte. A simple `ESC [ A` is balanced, but `ESC [ 1 ; 5 A` leaves `49 59 53` above `( buf maxlen pos )`; subsequent editing can use those leaked cells as buffer state. This contradicts the comment that CSI is consumed harmlessly. |
| **No history** | No up-arrow recall of previous input. A simple final-byte arrow sequence is consumed and discarded; parameterized variants are affected by the stack leak above. |
| **No cursor movement** | Left/right arrow within the line is not supported.  Backspace only erases from the end. |
| **Buffer size trust** | `W.INPUT` passes `maxlen` to the backend, but the backend must enforce it.  A buggy custom backend installed via `WV-INPUT WV!` could overrun. |

History, cursor movement, timeout policy, and validation of replacement
backends are hardening or feature work. The parameterized-CSI stack leak is a
current correctness and memory-safety defect in the default backend; it is
covered as unchanged-source behavior by the simulator rather than silently
repaired there.

---

## 9. Widget SDL and Screen-definition Correctness

The unchanged §9.5–§9.6 source has additional correctness defects distinct
from optional UI features:

| Issue | Detail |
|-------|--------|
| **Unchecked widget vector** | `WV@`/`WV!` trust both index and XT, and `INSTALL-TUI` leaves raw slot 13 uninitialized. |
| **Unsafe list count** | `TUI-LIST` special-cases exact zero but accepts negative/high-cell counts; `0 DO` can traverse essentially the whole cell domain. |
| **Reversed detail bound** | `TUI-DETAIL` suppresses valid selections, then executes an out-of-range numeric selection as an XT. |
| **Row/index leaks** | `.STOR-ROW` returns its `slot`. Documentation and tutorial lists restart visible numbering independently, while `.DOCS-BODY` publishes only the final tutorial count as `SCR-MAX`. |
| **Inherited Storage fault** | A selected Storage row reaches the matched-path extra `DROP` in `FIND-NTH-ACTIVE`. |
| **Zero-count loops/state** | `.HOME-MEM-BUFS` uses non-zero-trip `0 DO`; zero-buffer `.BSTATS-BODY` returns before clearing stale counters. |
| **Fixed memory ceiling** | `SCR-HOME-MEMORY` computes free space from a hard-coded 65,536-byte dictionary ceiling. |

These are source bugs, not simulator substitutions. The hosted acceptance
keeps them observable while tests avoid the unbounded and invalid-XT paths.

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
| 8 | Fix W.INPUT parameterized CSI leak (§8) | High | Small | Open |
| 9 | Fix SDL correctness defects (§9) | High | Mixed | Open |
| 10 | Spinlock for multicore (§4 long-term) | Low | Medium | Deferred |
| 11 | W.INPUT enhancements (§8) | Low | Large | Deferred |
