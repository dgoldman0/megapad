# Handoff to Akashic: let the Desk loop sleep

MegaPad can now sleep until input or a time passes. The Akashic Desk still
polls without sleeping, so an idle Desktop keeps one host core busy. This note
describes the MegaPad words and the loop change that would let Akashic use
them. No Akashic file was changed.

## Why the Desktop never idles

`_ASHELL-LOOP` in `akashic/tui/app-shell.f` (§12) repeats: terminal service,
a non-blocking `_ASHELL-POLL-INPUT` (`KEY-POLL` or `KEY?`), deferred actions,
`_ASHELL-CHECK-TICK`, paint if dirty, and `YIELD?`. Nothing in the pass
waits. `YIELD?` does nothing unless KDOS preemption is on. The guest runs flat
out, and so does its host: the simulator for the Desktop journey, or the
emulator. `TUI-EVT-LOOP` in `akashic/tui/event.f` has the same shape.

## The MegaPad words

Both are BIOS words on the emulator and the chip, and hosted words in the
simulator.

- `IDLE-UNTIL ( deadline-ms -- )` sleeps this core until input arrives, an
  interrupt is requested, or `MS@` reaches the deadline. On core 0, input
  means UART or NIC receive. It returns at once when the deadline has passed,
  and **it may return early**, so a caller always re-checks its own events.
- `IDLE-MS ( ms -- )` is `IDLE-UNTIL` at `MS@ + ms`.

In the simulator, `IDLE-UNTIL` sleeps only inside the root dispatch the
session runs, which is where the Desk loop runs. Anywhere else, such as
during source evaluation, it returns at once. That is allowed, because the
word may return early.

## Suggested loop change

At the end of a pass that found no work, sleep until the next moment the loop
must act:

- the next tick, `_ASHELL-LAST-TICK @ _ASHELL-TICK-MS @ +`, when the app has a
  tick XT;
- a visible toast's expiry, since `_ASHELL-CHECK-TICK` also clears expired
  toasts;
- any other timed work the shell or an applet owns.

Do not sleep in a pass that consumed input, drained posted actions, painted,
or left terminal service pending. The loop then runs straight into the next
pass as it does now. A sketch:

```forth
\ In _ASHELL-LOOP, after the paint step and before YIELD?:
_ASHELL-PASS-IDLE? IF _ASHELL-NEXT-DEADLINE IDLE-UNTIL THEN
```

Keep `YIELD?`. Input, including rich-terminal traffic, arrives over the UART,
so it ends the sleep at once. If no timed work exists, `-1 IDLE-UNTIL` sleeps
until input.

`TUI-EVT-LOOP` can take the same change.

## `NET-IDLE` changed

`NET-IDLE` used to be a bare `IDL`. The chip only wakes it on an enabled
request, and the emulators used to force it awake every 2–20 ms. It is now
`20 IDLE-MS`: it sleeps until network or terminal input, or 20 ms. Loops that
count `NET-IDLE` calls as a timeout now get a real-time bound. For example,
`200 0 DO TCP-POLL NET-IDLE LOOP` in `akashic/net/http.f` waits about 4 s when
no traffic arrives. `_SRV-IDLE-DEFAULT` in `akashic/web/server.f` and the
`NET-IDLE` loops in `net/ws.f` and `net/transports/kdos-tls.f` are affected
the same way.

## How to check it

With the Desk idle, the host process should use a small fraction of one core
instead of all of it. The physical Desktop journey stays the regression gate.
Timed behaviour (ticks, toasts, the calendar clock) should look the same,
because each sleep ends at the next deadline.

Reference: `docs/bios-forth.md` (Sleeping until input or a deadline),
`docs/isa-reference.md` (Waiting with IDL), `docs/simulator-contract.md`
(`IDLE-UNTIL`).
