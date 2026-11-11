# Host speed of the emulator and the simulator — 2026-11-10

A study of the emulator's and the simulator's host speed on 2026-11-10 found
neither near its limit, and its quick fixes and the simulator's engine work
landed the same day. None of it changes what the machine does or costs. The
emulator no longer copies its whole input journal at the end of every batch
that staged nothing, so a line-fed KDOS load takes 1.4 s where it took 3.2 s
(MegaPad 6b47e205). The ext4 harness finds an image's zero chunks with one
comparison each (Akashic f068fdd1). The simulator's native engine now defines
each operation once and compiles it for a general path and a direct path that
uses what a run has already proved (MegaPad 17db2bb7); it runs calls to
`CREATE ... DOES>` children itself (4d2cbd92), plans a word's operations
faster (73bef418), and returns to its Python dispatcher eight times less
often outside a session's quantum (7eba16c8). Its kernels run 2.0 to 8.1
times faster with the same steps and results, the headless Desktop is ready
in about 5.2 s where it took 6.2 s, and a typed character shows in 0.064 s
where it took 0.085 s. At MegaPad 7eba16c8 and Akashic f068fdd1 the 272 test
units these changes reach all passed, and so did the physical Desktop journey.
What the study left open is in fantasy-engine's
`dev/ideas/engine-speed.md`: letting the emulator's translated blocks reach each
other, which is weeks of work and not started, and the Python that still
surrounds the simulator's engine.
