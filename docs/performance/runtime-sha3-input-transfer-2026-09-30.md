# Checked SHA3 input transfer qualification

The native permutation extraction left repeated checked byte routing as the
largest cost in the bounded SHA3 workload. This slice removes repeated address,
payload and platform-route packaging after the existing complete input-span
preflight and immutable payload snapshot.

Each byte still reaches the original SHA3 service preflight and `write8`, in
order. Status reads, commands, ownership, raw-MMIO interference, error mapping,
cleanup and final digest publication retain their existing paths. The shortcut
requires exact original memory, platform and service methods and a callback-free
permutation. Custom or late-replaced routes use the ordinary byte loop.

## Paired measurement

Both reports use fresh subprocesses, 256 SHA3-256 transactions of 256 bytes,
three timed trials and one discarded warmup per executor. Attribution runs are
separate. Setup is reported separately from the measured guest workload.

| Selected semantic executor | Baseline median wall | Qualified transfer median wall | Baseline / current |
| --- | ---: | ---: | ---: |
| Python | 392.772 ms | 292.855 ms | 1.34× |
| Native | 220.225 ms | 116.830 ms | 1.88× |

Every timed and attributed case completed with 5,381 semantic steps, stack
`[0]`, and digest-output SHA-256
`f738f9d89ade028668ccef67cfce240c3f2e58970390eafe370940c11ad2e114`.
The baseline is a detached checkout of `2393829` with its pre-tile native
artifacts. Reports include exact source and extension hashes. These are host
workload measurements, not hardware timing or Desktop throughput claims.

- [`runtime-sha3-transfer-baseline-2026-09-30.json`](runtime-sha3-transfer-baseline-2026-09-30.json)
- [`runtime-sha3-transfer-native-2026-09-30.json`](runtime-sha3-transfer-native-2026-09-30.json)

## Compatibility evidence

The new input-transfer gate passed 59 checks through Make. It covers all four
SHA3/SHAKE modes around their rate boundaries, split squeeze, actual route-call
counts, before/after customization, route changes during a callback, exact
failed-byte prefixes and MMIO exception wrapping, rejected spans and raw owner
interference. The existing 47 hosted SHA3 checks also passed with Python.
The native-selected regression gate passed 133 checks: those same 47 hosted
checks, 12 semantic/architectural differentials, 60 shared Keccak checks and
14 native SHA3 device checks.

The observed failure after a full rate block retains the ordinary byte loop's
behavior: later input bytes can change the device error before the final status
read. This slice does not replace that behavior with block batching.
