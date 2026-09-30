# AES checked-transfer qualification

Existing scalar AES routing is retained. The fully checked native-namespace
candidate passed its correctness gates but did not demonstrate acceleration
against a baseline measured again on the current host. Candidate production
changes and binaries were removed from the active tree; the complete candidate
source and binaries remain in the isolated qualification checkout at
`/workspace/scratch/bf1956fffbfc/megapad-transfer-qualification`.

The bounded workload runs 64 AES-GCM transactions with 32-byte plaintexts.
The table reports median host wall times, not modeled guest cycles or hardware
latency.

| Candidate | Python executor | Native executor | Interpretation |
| --- | ---: | ---: | --- |
| [Historical scalar baseline](runtime-residual-crypto-2026-09-30.json) | 53.277 ms | 60.373 ms | Earlier host measurement; insufficient for the final retention decision |
| [Initial transfer shortcut](runtime-aes-transfer-2026-09-30.json) | 44.385 ms | 50.014 ms | Unsafe intermediate: incomplete namespace and region admission |
| [Hardened Python qualification](runtime-aes-transfer-qualified-2026-09-30.json) | 62.512 ms | 72.261 ms | Regressing intermediate: qualification scans cost more than the routing saved |
| [Native namespace candidate](runtime-aes-transfer-native-guard-2026-09-30.json) | 52.353 ms | 55.974 ms | Python uses scalar routes; native candidate not retained |
| [Current-host scalar baseline](runtime-aes-transfer-baseline-recheck-2026-09-30.json) | 53.444 ms | 54.635 ms | Retained implementation; native is faster than the candidate in this comparison |

All four candidate/recheck reports are preserved above. The historical native
baseline varied from 60.373 ms to 54.635 ms on recheck, so its apparent advantage
for the 55.974 ms candidate was host variation rather than a demonstrated gain.
The initial timing reduction also cannot establish a safe optimization: that
candidate could invoke custom dictionary-key equality during admission and
did not reject every span/region overlap with MMIO.

The final candidate moved only bounded namespace-key inspection into an
optional shared native helper, retaining Python identity and geometry checks,
per-byte source read/device preflight/destination write ordering, and failure
prefixes. It was bound only to admitted native runtimes; Python runtimes,
automatic-backend fallback, and extensions without the helper used the original
scalar routes. It did not change AES arithmetic or tile guard behavior.

Both extensions were built and the isolated candidate passed 200 Make-selected
checks: 58 namespace checks, 111 transfer checks, 23 KDOS checks, and eight
native differential checks. Candidate and rechecked baseline guest evidence
matched exactly across Python and native execution: 2,437 semantic steps,
64 transactions, 128 AES blocks, final stack `[0]`, and done status `2`.
Ciphertext and the full authentication tag matched the independent checked-in
fixture:

```text
ciphertext: 0643975a84a4835acc00d6caf0a8392cc194c576b2391d3e7a25a7c75f2b42f0
tag:        61f3ad860a90ca7ede2074f793b887c1
```

The correctness result does not justify retaining additional admission
machinery without a measured benefit. This closes the MegaPad AES transfer
experiment for the current roadmap. Further AES optimization requires a new
measured mechanism; no Akashic source changes or AES kernel rewrite were part
of this experiment.
