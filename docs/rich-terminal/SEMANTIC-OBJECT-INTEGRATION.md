# Semantic objects and unified runtime integration

This local integration checkpoint combines the flowing rich-terminal branch
with committed nested-callback runtime work. It does not replace the separate
full Desk qualification for each newly advertised producer capability.

## Checkpoint

- MegaPad: `f9d6f07dceeffeedcbe0c73d5f09a60715752163`.
- Runtime parent: `079dae9`, merged into rich integration `f2ec566`.
- Akashic provider sources: subsequently committed as `6c2bf30` on
  `feature/rich-desk-producers`, based on main's merged FP64 work.
- Both native extensions were built in this checkout with GCC/G++ and imported
  from it during qualification. The native nested-callback ABI reported 3.

## Boundary qualification

All Python suites ran through the Make test supervisor, with unique runtime
namespaces and `MEGAPAD_ROOT` pointing at this integration checkout.

| Gate | Passed | Elapsed |
|---|---:|---:|
| Unified launcher, nested manifest/runtime/session, native publication/root/child/owner, simulator accounting and stack pointers | 470 | 6.41 s |
| FIELD/STATUS wire, models, driver, shared input and full-source simulator boundaries | 287 | 3.04 s |
| Akashic FIELD/STATUS/SERIES real providers on Python and native executors | 93 | 88.56 s |
| Total | 850 | |

There were no failures or skips. The native full 16,000-sample SERIES round trip
took 1.40 s and preserved every signed sample and its exact timestamp. This is
an individual test duration, not a full Desktop execution benchmark.

The provider suite includes immutable retry copies, hidden publication and
reveal, abort, complete history reservations, empty and mixed histories,
explicit timestamps, malformed copied backlinks, aliases and atomic refusal.

## Scope and next integration boundary

This checkpoint qualifies the runtime/provider boundaries. It does not claim a
full Desk FIELD, SERIES, PANE or TASKBAR journey on this runtime. The FIELD
investigation stays pinned to `f2ec566` to avoid changing runtime and guest
behavior together. The already completed STATUS Desk qualification is recorded
in Akashic's `docs/rich-terminal/STATUS-DESKTOP-QUALIFICATION.md`.

The parallel runtime branch subsequently reached committed `1a2267d`, adding
private scalar service execution and native task restoration/cancellation
boundaries. That later work and its uncommitted changes are not included in
the numbers above. Reconcile an appropriate committed checkpoint before final
combined Desk qualification; do not import another worktree's uncommitted work.
