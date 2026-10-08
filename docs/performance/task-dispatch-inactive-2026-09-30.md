# Inactive shared-task dispatch check

The synchronous task dispatcher retains a constant-time inactive check and
uses the ordinary meter and primitive call paths when no task root exists.
This bounded check compares the existing continuation-long workload before
and after that integration. It does not measure task callback throughput.

Both cases use 4,096 nested increments, an 8,192-step host quantum, three fresh
timed instances and one discarded warmup. Setup and validation are excluded.
Every sample validates the exact stack, return pointer, continuation slots,
retained bytes, 45,061 semantic steps and five host resumptions against a fresh
Python oracle. The native extension is unchanged between the two snapshots.

| Semantic executor | Before median | Candidate median |
|---|---:|---:|
| Python | 143.715 ms | 125.667 ms |
| Native | 1.044 ms | 1.183 ms |

These sequential short samples establish bounded cost and preserved behavior;
they do not isolate causation or establish a speed improvement. The native
sample increased by approximately 0.139 ms. No scheduling quantum, semantic
budget or observation boundary was relaxed. Full source/native hashes,
checkout state and individual observations are retained in the accompanying
JSON. The baseline is clean commit `8206806`; the candidate is the isolated
dispatcher source diff on that base, also qualified by 136 focused and 930
regression checks through Make.
