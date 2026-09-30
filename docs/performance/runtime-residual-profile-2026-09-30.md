# Residual hosted runtime profile

These bounded probes were run on the clean `c0958bd61ce3aac5378a595685c043f9f4d92fe9`
checkpoint with its matching native extensions. Every case used three fresh
unprofiled trials, one discarded warmup and a separate attribution run.
Preparation, oracle validation and cleanup are outside the measured interval.
The measurements identify follow-up work; they do not establish Desktop,
full-program or modeled-hardware performance.

| Workload | Work per trial | Python median | Native semantic median |
| --- | --- | ---: | ---: |
| AES-256-GCM | 64 transactions, 32 plaintext bytes each | 53.277 ms | 60.373 ms |
| NTT compute | 64 forward transforms, 256 coefficients | 17.735 ms | 19.031 ms |
| NTT transfer | 64 load/transform/store iterations, 2,048 transferred bytes each | 135.774 ms | 154.146 ms |
| Hot page reads | 4,096 reads | 107.299 ms | 0.965 ms |
| Scattered page reads | 4,096 reads | 105.251 ms | 1.002 ms |
| Page-crossing reads | 4,096 reads | 112.207 ms | 1.039 ms |
| Nested continuations, quantum 7 | 512 increments, 768 host resumes | 21.038 ms | 11.980 ms |
| Nested continuations, quantum 8,192 | 512 increments, no host resumes | 13.608 ms | 0.174 ms |

## Decisions from the evidence

NTT transfers cost substantially more than the transform itself in these
probes. The profile attributes that cost to checked byte-by-byte memory and
service routing. The next change qualifies complete ordinary-memory transfers
for bulk access, retaining the original scalar path for custom callbacks,
MMIO, wrapping addresses, invalid spans and other exceptional cases. The
compute kernel remains a separately measured candidate.

AES shows the same checked transfer overhead in `write8`, `_write_integer`,
`_aes_to_device` and `_mmio_write`. Its next investigation concerns those
boundaries before replacing arithmetic. Explicit native semantic selection
does not make all hosted services native, and these small transactions expose
the cost of returning to Python service words.

Page access already stays in bounded native intervals: each native trial
uses nine entries, one plan and eight allowance exits for 65,541 semantic
steps. There is no measured case here for another page-ownership or cache
rewrite merely because Python objects remain in the implementation.

The continuation probes preserve the same guest work while changing the
host-yield interval. The short case exposes settlement and dispatch costs;
the longer case completes in one native interval. Changing yield cadence is
not an optimization of the same responsiveness contract. Retain the current
continuation representation unless a production profile identifies a further
cost worth changing.

## Correctness and provenance

AES ciphertext and the full authentication tag match the checked-in known
answer. NTT results match an independent direct modular DFT outside timing.
The page variants all produce checksum 591,872 with unchanged source bytes.
Both continuation variants and executors produce 5,637 semantic steps, stack
`[512]`, return pointer 1,048,576, cookie 2,049, identical retained slots and
return-byte SHA-256
`f6519e2e14e0d3b53ef3c9cc4684ba07bb0ac24ff4802691f4c8233a49899de9`.

The benchmark additions passed 105 Make-selected checks before these runs.
Raw reports retain all samples, separate attribution, guest evidence, native
artifact hashes and checkout identity:

- [Crypto and transfer probes](runtime-residual-crypto-2026-09-30.json)
- [Page probes](runtime-residual-pages-2026-09-30.json)
- [Continuation probes](runtime-residual-continuations-2026-09-30.json)
