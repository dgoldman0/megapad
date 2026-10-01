# Bounded execution-model comparison

`bench_execution_timing.py` separates functional instruction-batch accounting
from strict shared-clock measurements. It contains only local assembled
MegaPad fixtures; it does not load or modify Akashic or reproduce an external
solver result.

```sh
MP64_RUNTIME_NAMESPACE=unified-runtime python bench_execution_timing.py \
  --cores 1 2 4 --workers 1 2 4 --iterations 256 \
  --trials 3 --warmup 1 --output execution-timing.json
```

Build the current emulator extension before running. Use the repository Make
targets for `tests/test_execution_timing_model.py` and
`tests/test_execution_timing_benchmark.py`. Run builds, tests, and benchmarks
sequentially so compiler or competing guest work does not contaminate host
timing.

Each case has a separate process with a bounded wall watchdog. Every trial
uses a fresh prepared machine. Setup, cache preparation, and result validation
are outside the timed action. Warmups are discarded fresh instances. Reports
include all wall/process samples, source and binary hashes, guest results,
per-core work, system-clock deltas, and explicit execution models.

| Fixture | Scope | Checked outcome |
|---|---|---|
| `wake` | Two or four full cores; one sender issues an IPI and continues a bounded private loop; the receiver starts in IDL with interrupts masked; additional cores do private work | Receiver resumes, commits one marker, and halts; sender/background loop counts and outstanding IPI agree |
| `compute` | A fixed total iteration count divided across one, two, or four full cores | Exact per-core sums and completed loop counts |
| `memory` | The same fixed-total assignment with loads/stores to disjoint cells in one ordinary RAM bank | Exact sums and final data cells; the shared bus can contend without a data race |

Guest-core count and host worker-lane count are independent axes. The report
requires identical guest results and model accounting across selected host
lane counts when a one-lane reference is present. Wall-time differences are
host performance, not a change to modeled guest time. Compute and memory
perform different instructions per iteration; the recorded instruction counts
make that difference explicit. No result is required to approach a fourfold
speedup.

`instruction_batched` runs retain a large instruction request. Their cycles
are functional round accounting, and they publish no shared-clock latency
estimate. Replacing those calls with one-instruction requests would change
their wake cadence rather than observe the same workload policy. The current
shared application session selects this model too.

`strict_shared_clock` runs use a virtual RTC, full cores only, and the existing
ready-cycle/main-bus runner. After unobserved timed trials, a separate untimed
replay advances at most one system cycle per call. Its final guest results,
instruction counts, and cycle totals must match the timed case. Wake replay
records the observation intervals containing IPI assertion, receiver wake,
and the receiver's marker store. Derived latency intervals include this
one-cycle observation resolution; they do not pretend that an after-call
observation identifies an earlier intra-call event instant. Per-core CPU cycle
counters are never subtracted across cores to infer wake latency.

The wake fixture measures masked-IPI resume and first useful memory
publication. It does not measure vector entry, a BIOS worker protocol, source
compilation, or an FP64 solver. The strict model does not establish physical
RTL latency. The original reported solver ratios require that workload and
its measurement definitions before they can be reproduced.

`--cache warm` explicitly primes only these assembled code lines outside the
measurement. `--cache cold` retains cold instruction caches. Cache policy is
part of every result; warming the host process is a separate warmup action.
The benchmark does not modify guest cache publication semantics.

## Qualified run — 2026-09-30

The 2026-09-30 run, whose raw JSON report remains in the repository history,
measured 48 fresh-process cases at 256 iterations, three measured trials and
one discarded warmup per case, using warm guest code caches. All exact-output,
accounting and separate one-cycle replay checks passed. All 16 comparisons
across one, two and four host lanes preserved guest state and clock accounting.
The 19 harness tests also passed. No scheduler implementation changed for these
measurements.

For fixed total work, strict shared-clock results were:

| Guest full cores | Compute cycles | Compute aggregate instructions | Memory cycles | Memory aggregate instructions |
|---|---:|---:|---:|---:|
| 1 | 1,792 | 1,537 | 2,560 | 1,793 |
| 2 | 896 | 1,538 | 1,410 | 1,794 |
| 4 | 448 | 1,540 | 1,029 | 1,796 |

Compute scales by 4.00× in modeled cycles from one to four guest cores; the
memory fixture scales by 2.49×. Its disjoint data cells still share the modeled
main bus. Small per-core termination overhead explains the different aggregate
instruction counts. These are workload-specific guest-cycle ratios, not host
throughput results. For example, at one host lane the corresponding median
wall times were 2.282/1.901/1.648 ms for compute and 6.119/5.586/5.860 ms for
memory. The raw report retains the other host-lane samples and process times.

The minimal masked-IPI fixture observed assertion and receiver runnable state
within cycles [0, 1], and its first useful marker within [2, 3], on both two
and four cores. Accounting for observation resolution gives 0–1 modeled cycles
from assertion to runnable and 1–3 to the marker. This fixture begins with the
receiver already idle and has no BIOS worker protocol or interrupt-vector
entry. It does not reproduce the team's reported ~1,000-cycle worker wake.

The fast path also finishes the exact work, but only its functional round
totals are recorded. In particular, equal final totals in the two-core wake
case do not establish equal wake latency. The reported ~130,000-cycle fast
wake and 1.3–1.4× solver scaling remain unverified external workload figures;
the new labels and fixtures prevent substituting another timing model or a
small integer kernel for that evidence.
