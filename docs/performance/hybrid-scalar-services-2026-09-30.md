# Private scalar callback crossing measurements

The version 5 service path is functionally qualified but substantially slower
than direct semantic FP for these small operations. Direct execution remains
the performance choice for this workload; the callback path provides bounded
machine-to-semantic interoperability.

Measured clean commit `ddc986c` with its matching native artifacts, using
three fresh timed groups and one discarded warmup per executor/size. Each
group uses one exact scalar owner for direct FP, service callbacks and both
controls, then repeats the direct baseline. Setup, reset, checking and cleanup
are excluded. Each worker has the unchanged 60-second watchdog. Raw samples,
source/native hashes, assembled fixtures and validation are in the JSON.

All numbers below are median milliseconds.

| Semantic executor | Iterations | Direct FP | V5 service | Machine control | Integer callback control | Direct recheck |
|---|---:|---:|---:|---:|---:|---:|
| Python | 64 | 5.839 | 1,870.374 | 0.418 | 26.350 | 5.846 |
| Python | 256 | 24.804 | 7,535.890 | 0.665 | 108.334 | 23.826 |
| Native | 64 | 0.205 | 1,549.497 | 0.482 | 26.804 | 0.191 |
| Native | 256 | 0.362 | 6,255.921 | 0.562 | 107.063 | 0.345 |

Each iteration evaluates F64+, F64FMA, F64/ and F64SQRT, stores their four
independently known result bit patterns in a guarded buffer, and accumulates
the same checksum. Direct and V5 paths preserve exact results and sticky NX
(`FPCSR=16`). The selected scalar kernel is checked on the original owner:
Python uses `shared.scalar_fp.execute`; native uses
`_megaforth_native.scalar_fp_execute`. Private callback dispatch is Python in
both cases.

The machine control stores precomputed FP answers. The integer callback
control calls XOR(answer, 0). They perform different work from FP and leave
FPCSR zero. Their timings are reported separately and are never subtracted
to claim an isolated service cost.

| Verified service work | 64 iterations | 256 iterations |
|---|---:|---:|
| FP operations / callbacks | 256 | 1,024 |
| Callback semantic ticks | 256 | 1,024 |
| Total semantic ticks | 257 | 1,025 |
| Machine instructions | 2,886 | 11,526 |
| Machine cycles | 4,230 | 16,902 |
| Machine segments | 257 | 1,025 |
| Machine entries / maximum parked depth | 1 / 1 | 1 / 1 |

Direct execution charges 2,885 / 11,525 semantic ticks and no machine work.
Checksums are 431627363195528704 / 1726509452782114816. Every path retains the
same guarded result bytes and returns with an empty return stack. Thirty-five
Make-selected harness checks also qualify independent ordinary MP64 stepping,
kernel ownership and corruption detection.

The measured crossing cost warrants separate profiling and optimization before
fine-grained services are used for throughput. These results do not isolate
which validation or host boundary dominates, qualify bulk memory/crypto/audio
services, or establish Desktop or math-team solver performance.
