# Unified runtime local acceptance — 2026-09-30

The approved local unified-runtime scope passed final selected acceptance on
`4ef08d88817af1fbe9c6a5ef17e834295bc1f17e`. All gates ran serially through Make in an isolated clean checkout.
The application gate includes private hybrid profiles 1–5 and host-prepared
task sessions, both semantic executors, all 38 native composite journeys, exact
capability reporting and production close after polling-proof failure.

| Gate | Selected files | Result |
|---|---:|---|
| Unified application and native boundaries | 51 | 2466 passed, 3 skipped in 199.27s (0:03:19) |
| Simulator, explicit Python selection | 56 | 2569 passed in 194.30s (0:03:14) |
| Simulator, explicit native selection | 56 | 2569 passed in 184.72s (0:03:04) |
| Production-source rich terminal, both backends | 1 | 8 passed in 2.53s |

Counts describe each selector, not distinct coverage to be added together.
The three skips are the existing socket-boundary and shared-client tests listed
in the JSON record. Their direct-dispatch counterparts ran. No new exclusions
were added to obtain these results.

## Build and source identity

`CC=gcc CXX=g++ make build` succeeded with Python 3.12.14. It rebuilt both
extensions at `abd77639be99284d42c55ed6c09288bae2d7c60b`. The tested activation commit changes
only Python/tests/documentation; native source and build configuration are
unchanged. Each final gate used the same rebuilt artifacts:

| Artifact | SHA-256 |
|---|---|
| `_mp64_accel.cpython-312-x86_64-linux-gnu.so` | `c3568a689aa09ef86da8b112c2644d00d75013a2cf1de44559ac0f707887665b` |
| `_megaforth_native.cpython-312-x86_64-linux-gnu.so` | `cc70cfdd2bc65942cae452ff52cee4171affa5e1e3fb442ef841166e7da35b37` |

A machine-readable record with the exact selected test files, Make commands,
executor environments, source revisions, binary identities, log hashes and
outcomes remains in the repository history. Another checkout supplies its own
Python environment through `VENV_PY`.

## Reproduction

Build first, then run the application selector with `make test-sequential` and
its recorded `TEST_PATH`. Run the simulator selector twice with
`make test-simulator`, setting `MEGAFORTH_EXECUTOR=python` and then `native`.
Finish with `make test-rich-terminal-dual`. Supply the recorded file lists via
`TEST_PATH` or `SIMULATOR_TEST_PATH`, clear `K`, and retain a dedicated
`MP64_RUNTIME_NAMESPACE`. Wait for each gate before starting the next.

## Qualified scope and limits

- Three pre-existing AF_UNIX skips: this environment prohibits Unix socket creation; direct shared-session dispatch is covered.
- No new physical display, audio output, TAP networking, Akashic build or external math-solver acceptance.
- Prepared host task APIs are qualified; generic task manifests, bootstrap ordering and task CLI options remain deferred.
- The native dictionary/compiler design is complete; implementation of its later stages is outside this gate.
- No benchmark was rerun by these functional gates. Existing performance reports retain their measured source revisions and limits.
- The Python/native simulator selectors overlap because individual fixtures may explicitly select or parameterize executors; counts are not additive distinct coverage.
