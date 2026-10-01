# Closed integer callback qualification

The bounded closed callback profile is functionally qualified. It is expensive
for small policies: the measured machine loop with 128 callbacks took about
half a second, while a native semantic loop completed the equivalent array
operation in 0.149 ms. This profile provides interoperability, not an
acceleration recommendation for a tiny signed clamp.

The complete report, which remains in the repository history, records a clean
`c6c12ab` checkout, all relevant source and native binary hashes, independent
untimed validation, one discarded warmup pair and three fresh-owner trials.
Only execution is timed; construction, registration, validation and cleanup
are excluded. Trial order alternates between the two paths. Each executor ran
in its own bounded subprocess without profiling enabled.

| Selected outer executor | Semantic loop | Machine loop with closed callbacks |
| --- | ---: | ---: |
| Python reference | 6.218 ms | 485.114 ms |
| Native semantic | 0.149 ms | 477.469 ms |

These are median host wall times, not hardware cycles. Closed policies use the
Python reference dispatcher under both outer executors. The callback path
checks captured code, method routes, private stack/control evidence and actual
work around every admitted operation. This experiment does not separately
attribute that overhead and does not justify removing any of those checks.

Both paths clamp 128 signed 64-bit values to [-17, 23] in the same bounded
ordinary-memory span. An independent integer oracle includes signed extrema
and boundary cases. Every trial produced identical array bytes, untouched
guard bytes, checksum 264 and balanced return stacks. Per-path work is exact:

| Work | Semantic loop | Hybrid loop |
| --- | ---: | ---: |
| Semantic steps | 3,845 | 897 |
| Callback semantic steps | 0 | 896 |
| MP64 instructions | 0 | 1,290 |
| MP64 cycles | 0 | 1,674 |
| Callback requests | 0 | 128 |
| Machine segments | 0 | 129 |
| Outer machine entries | 0 | 1 |

The implementation gate passed 761 checks spanning closed exports, bridge
accounting, production session dispatch, benchmark validation and existing
native/v1/v2 contracts. Another 472 checks covered ordinary dispatch,
suspension, stack restoration, dictionary rollback and KDOS behavior. The
bounded profile still excludes nested machine calls, shared-task exceptions,
suspension and service access; those have separate contracts and gates.
