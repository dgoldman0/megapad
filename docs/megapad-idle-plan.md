# MegaPad idle: sleep until input or a deadline

Status: phases 1–6 complete. A MegaPad program can now sleep until input
arrives or a time passes, on the chip and in every backend. The remaining step
is Akashic's: its Desk loop must call the new words
([akashic-idle-handoff.md](akashic-idle-handoff.md)).

## 1. Why

The Akashic Desk loop polls input without blocking and never executes `IDL`,
so its host runs flat out even when nothing happens. Fixing that loop needs a
MegaPad primitive that sleeps until a key or the loop's next tick, whichever
comes first. Building that primitive exposed three MegaPad problems:

- **The BIOS key wait never wakes on the chip.** In the RTL, `IDL` ends only
  by taking an enabled interrupt. The BIOS never enables the UART receive
  interrupt, and IVT slot 9 is empty. The emulators hide this by waking core 0
  on any received byte.
- **The shared timer cannot be the alarm.** Its interrupt reaches every core
  and its IVT slot stays empty until KDOS installs a handler, so a sleeping
  worker would vector to address 0, and sleepers would contend for one
  compare register.
- **Hosts wake on fixed short timers.** The emulator runners poll every
  1–20 ms, and some waits (`NET-IDLE`, the graphics vsync wait) only end
  because a runner forces the core awake.

## 2. Architecture

The design follows RISC-V `WFI` with a per-hart timer compare, and the
deadline form of x86 `TPAUSE`.

- **`IDL` wake rule.** `IDL` waits until this core has an interrupt request
  from an enabled source (an IPI, the timer with its IRQ enabled, UART or NIC
  receive with that device's IRQ enabled), or until its wake time passes. The
  `I` flag does not affect the wait. With `I` set, the core then takes the
  interrupt as before. With `I` clear, it continues after `IDL`.
- **`WAKE_MS`, CSR `0x26`, full cores.** An absolute RTC uptime in
  milliseconds (the `MS@` clock). `IDL` also ends once uptime reaches it. Zero
  disables it. It is a wake condition, not an interrupt, so it never vectors.
- **Interrupt routing.** UART and NIC interrupts go to core 0 only, as the
  emulators already model. The timer and IPIs are unchanged.

## 3. Words

- `IDLE-UNTIL ( deadline-ms -- )`: sleep this core until input, an interrupt,
  or `MS@` reaching the deadline. It returns at once if the deadline has
  passed, and it may return early. Callers re-check their events. On core 0 it
  also wakes on UART or NIC receive.
- `IDLE-MS ( ms -- )`: `MS@ +` then `IDLE-UNTIL`.
- The BIOS key wait sleeps the same way, so it works on the chip.

## 4. Phases

1. **Emulators.** Python and native: `WAKE_MS`, the wake rule in one system
   function that every runner calls, with UART and NIC gated by their IRQ
   enables.
2. **BIOS.** `IDLE-UNTIL`, `IDLE-MS`, the key wait, and `NET-IDLE` as a
   bounded sleep.
3. **RTL.** The `IDL` wake rule on full cores and micro-cores, `WAKE_MS` fed by
   the RTC uptime, and core-0 routing for UART and NIC.
4. **Host runners.** When every core sleeps, wait for input or the earliest
   wake time instead of polling. A virtual clock jumps straight to that time.
5. **Simulator.** Hosted `IDLE-UNTIL` and `IDLE-MS`, a blocked guest that
   resumes on input or its deadline, and an owner loop that sleeps until then.
6. **Docs and handoff.** ISA, BIOS, and simulator references, plus a note for
   the Akashic team on the Desk loop change.

## 5. Results

- **Host load at an idle BIOS prompt** (emulator shared session): 2.3% of one
  core before, 0.7% after. The owner now waits for input or the next timed
  wake, rechecking every 20 ms for sources that do not notify it (NIC frames).
  The simulator owner does the same for a guest blocked in `IDLE-UNTIL`.
- **Fast paths.** The native batch scheduler applies the wake rule at every
  equal-credit round, and the cycle scheduler makes `WAKE_MS` an exact event
  frontier. A core that sleeps with `I` clear while its peers run wakes at the
  next round, not mid-round.
- **Micro-cores** follow the `IDL` wake rule but have no `WAKE_MS`.

## 6. Found on the way

- **Emulator test snapshots** saved CPU state and memory but not device
  registers. A core snapshotted asleep in the key wait has the UART and NIC
  receive requests enabled, so the snapshots now carry those two enables.
- **A fixed-address test.** The earlier worker-fault test wrote its markers to
  fixed addresses that fell inside the BIOS image once it grew. It now uses a
  variable.
- **`tests/test_shared_session.py::test_shared_server_clients_control_one_machine`**
  fails on the commit before this work too: a paused `step` at an idle prompt
  executes no instruction. It is not on this path.
- **Queued network frames.** A first version kept the sleep awake while any
  frame sat in the NIC queue. Frames nobody reads, such as stray broadcasts,
  then made every sleep return at once, and the BIOS key wait spun. The sleep
  now clears the NIC receive-pending latch and wakes only on a frame that
  arrives while it sleeps. A frame landing in the few instructions before the
  sleep waits for the deadline, so frame waiters use a short one.
- **Stalled KDOS test runs.** The two known THROW-during-load failures run
  into zeroed memory, where every byte is an `IDL`. Input no longer wakes a
  core that never enabled it, so the KDOS test harness spun to its step limit.
  It now stops when a batch makes no progress with core 0 asleep, and those
  two tests fail as before.
- **Other bare `IDL` users.** `NET-IDLE` and the `graphics.f` vsync wait relied
  on runners forcing the core awake; both now use `IDLE-MS`. KDOS `IDLE` is
  still a bare `IDL`, which now ends only on an enabled request; nothing in
  MegaPad calls it.
