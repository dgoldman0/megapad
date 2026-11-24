# Crypto engines: instructions, held state and copies for each core and cluster

Status: decided on 2026-11-24, with implementation still to begin. This plan
records four decisions: MegaPad's crypto engines come under instruction
control, each engine keeps its working state inside itself while a caller
holds it, each full core and each cluster gets its own copies of the busiest
engines, and the work lands in a set order. Items carry three labels: **Decided** items are settled, **Proposed**
items are confirmed at the step named beside them, and **Open** items are
chosen in the numeric contract of step 0.
[`crypto-interface-contract.md`](crypto-interface-contract.md) remains the
numeric source of truth for what is built, and the numeric contract of each
new instruction is added to it before that instruction's encoding lands.

## 1. Summary

Software drives the crypto engines through instructions in the EXT.CRYPTO
family (`FB`). While a caller's work spans several instructions, the engine
keeps its state inside itself and the hardware records that caller as the
owner (§2). Keccak, together with the chain controller that runs Winternitz hash
chains on it, and AES each come in seven copies, one private to each full
core and one shared by the four microcores of each cluster (§3). The ML-KEM
device is removed, and software composes ML-KEM from Keccak and the NTT. The
entropy source stays one device that every core shares, and every engine in
this plan is built on MegaPad's die. (Decided)

| Engine | Today | Planned | Status |
|---|---|---|---|
| CRC | Instructions; private per full core, shared per cluster | Unchanged | Built |
| SHA-2 | Instructions; private per full core, shared per cluster | Saves and loads its chaining state (§2) | Decided |
| Field unit | Instructions run by the emulator and simulator; the circuit leaf awaits wiring into the cores | Wired into full cores and clusters, known defects fixed | Decided |
| Keccak and chain controller | One shared device at MMIO `+0x0780` and `+0x08A0` | Instructions; one copy per full core and per cluster | Decided |
| AES with GHASH | One shared device at MMIO `+0x0700` | Instructions; one copy per full core and per cluster | Decided |
| NTT | One shared device at MMIO `+0x08C0` | Modular butterfly built into the seven tile engines | Proposed, step 4 |
| ML-KEM | One shared value device at MMIO `+0x0900`; its circuit is a stub | Removed; ML-KEM composed in software | Decided |
| Entropy source | One shared device at MMIO `+0x0800` | Unchanged | Decided |

## 2. Held state and ownership (Decided)

Work that spans several instructions, such as a hash stream, an AES record
under one key schedule or a polynomial in transform, keeps its state inside
the engine while one caller holds it, and the hardware records that caller as
the owner. One-shot work, such as a raw Keccak permutation or a single hash
chain, holds state only while its instruction runs and needs no claim.

Five rules govern every engine that holds state:

1. **One attempt per claim.** A claim retires after one attempt and reports
   whether the caller now owns the engine. Software then retries, pauses,
   backs off or gives up, as it does after `TACC.TRY`.
2. **Save and release at chunk boundaries.** An owner can store the engine's
   state to its own memory and release the engine, then claim it again later
   and load the state back. An owner with a long stream lets other callers in
   between its chunks this way, and a task that moves to another core takes
   its state along the same way.
3. **One-shot work for other callers.** Where an engine offers one-shot
   operations, a caller that lacks ownership can still run them while the
   held state stays untouched. For Keccak this costs a second 1,600-bit state
   register in each copy.
4. **Forced release wipes.** A supervisor release frees an engine whose owner
   has died and zeroes its state before admitting another caller, as
   `FORCE_RELEASE` does for each tile engine's accumulator.
5. **Owner check on every instruction.** Each instruction that touches held
   state checks that its caller owns the engine. Any other caller receives a
   defined refusal, and the state stays hidden from it.

Release, error and every reset scope also wipe the state.

Held state suits a chip whose cores keep no data cache, since every load or
store of state crosses the bus. A sponge that reloaded and stored Keccak's
200-byte state for each permutation would move 50 qwords for every 24 cycles
of rounds, and keeping a 1,024-byte polynomial inside an engine saves 128
qword transfers for each step it stays there. Keys and hash states also remain out of
ordinary memory, and a fault cannot leave a half-stored state block in
memory. The chip's existing engines already work this way: CRC and SHA-2
keep their state and owner inside the engine, and software claims and
releases each tile engine's 2,048-bit accumulator.

Two costs come with the rule. The hardware records cores and microcores, so
two tasks on one core need a further rule (§7). The claim, release, save,
load, cancel and reset paths also multiply the states that the circuit tests
must cover.

## 3. Copies per core and per cluster (Decided)

Keccak with its chain controller, and AES with GHASH, each come as one copy
per full core and one per cluster, seven of each on the standard die of four
full cores and three clusters. The number of copies derives from the build's
core and cluster counts, the parameters that already set the number of tile
engines, so a build with other counts carries matching copies. A full core's
copy is private to it, while a cluster's copy serves its four microcores,
which take equal turns as they do for CRC, SHA-2 and the tile engine.

Each copy reads and writes memory through its own core's or cluster's bus
port, the path the cluster's SHA-2 engine uses today. The main bus therefore
keeps its port count, and the chain controller's separate bus port is removed
with the old device. Once the bus's share settings are wired, a copy's
traffic counts against the share of the core that asked; the sideband that
sets them is tied off today
([microcore-kernels-plan.md](microcore-kernels-plan.md) §4).

A full core's copy checks each operand span against that core's privilege
level and protection bounds, and a cluster's copy checks against the
cluster's. The cluster's protection state has open gaps
([architecture.md](architecture.md#privilege-model)), and these are closed
before untrusted code may use a cluster copy. Resetting a full core wipes its
copy, disabling or resetting a cluster wipes the cluster's copy, and
resetting one microcore cancels only that caller's work, following the reset
scopes already defined for the tile engines.

The other engines are placed differently. The NTT's modular butterfly is to be
built into the seven tile engines, which work on blocks of numbers in the
high-bandwidth bank, and the choice is confirmed at step 4 against a
standalone engine (Proposed). The ML-KEM device is removed. One noise source
serves every core, so the entropy source stays a single shared device. The
design includes no crypto chiplet. Every job sent to a chiplet costs a
request and a reply, 128 bytes of queue entries and four times the 32 bytes
one AES block moves in and out, while sealed keys, should the chip need them,
fit on the die.

### What the copies cost

Measured on 2026-11-24 with Yosys 0.33 (`synth_xilinx -family xc7 -flatten`,
one leaf at a time), `rtl/crypto/mp64_keccak_core.v` synthesizes to 6,536
LUTs and 1,608 flip-flops, and `rtl/crypto/mp64_wots.v` to 3,854 LUTs and
2,705 flip-flops; neither uses DSP or block RAM. The SHA-3 front end and the
AES leaf ran past their 240- and 300-second limits, and the SHA-3 run reached
15.6 GB of memory before it stopped, so any further synthesis of either needs
a memory cap. By estimate, six more copies each of Keccak with its chain
controller and of AES add roughly 5 percent to the chip's silicon, because
its 4 MiB of internal memory and the multipliers of its tile engines and
field units take most of its area. The chip stays modest in size.

A single caller uses one copy. For batch work such as SPHINCS+ signing, whose
Winternitz chains are independent of one another, software uses more engines
by spreading the chains across cores now, and across microcore kernels
later. By the WOTS prototype's cycle budget, one signature takes 7.3 s on one
engine and about 2.1 s on four, at 200 MHz (projected). Between uses a copy
costs leakage alone once its clock is gated, like the per-core multipliers
and FPUs. The three cluster copies gain their first users with microcore
kernels ([microcore-kernels-plan.md](microcore-kernels-plan.md)), among whose
candidate uses Akashic lists hashing.

Held state of this size fits beside a core: each tile engine already holds a
2,048-bit accumulator, more than Keccak's 1,600-bit state or AES's 1,920-bit
key schedule. Copies also keep each core's crypto off any chip-wide
serialisation point, the reason every full core has its own dictionary table
([SoC-hardening.md](SoC-hardening.md) §3).

## 4. What sets the design apart

Four properties of the device distinguish MegaPad's crypto machinery from the
nearest existing designs. Each entry names the nearest design and what
MegaPad's circuit has today.

1. **Engines hold state and record an owner.** IBM's z/Architecture crypto
   instructions keep their state in a parameter block in the caller's memory,
   which each instruction reads and updates, and Intel and ARM keep crypto
   state in each core's vector registers. A MegaPad engine holds the state for
   as long as one owner holds the engine. CRC, SHA-2 and the tile engines'
   accumulators work this way by instruction today, and the Keccak device
   holds its state behind device registers and a software lock; Keccak and
   AES gain instructions under this plan.
2. **Small cores borrow their cluster's engines.** A microcore has scalar
   datapaths only, and the four microcores of a cluster share one copy of each
   engine and its state. ARM's Cortex-A510 pairs two small cores around one
   shared vector unit. Clusters share CRC, SHA-2, the multiplier, the FPU and
   the tile engine today, and Keccak and AES join them under this plan.
3. **Engines work on memory themselves, with bounded waits, beside cores that
   keep no data cache.** The chain controller reads its 64-byte context as a
   main-bus requester, and its BIOS word derives its deadlines from the number
   of bus ports that System Info reports. A value an engine writes is the
   value a core reads next. The chain controller works this way today, and
   under this plan each copy reaches memory through its own core's port.
4. **Two operations.** One instruction multiplies two 256-bit numbers modulo a
   prime the program loads (`GF.LDPRIME`, then `GF.MUL` with the custom
   selector). Oracle's SPARC T4, whose Montgomery multiply instructions take
   operands of up to 2,048 bits, is the nearest precedent; MegaPad's emulator
   and simulator run the instruction, and its circuit leaf awaits wiring at
   step 4. The chain controller runs up to fifteen SHAKE256 steps of a
   Winternitz chain in one request, a kind of engine that appears in research
   designs, for XMSS on a RISC-V processor and for hash-based signatures in
   OpenTitan. MegaPad's is built and qualified.

Sources: Linux
[`arch/s390/include/asm/cpacf.h`](https://raw.githubusercontent.com/torvalds/linux/master/arch/s390/include/asm/cpacf.h)
(IBM's function codes, parameter blocks and partial-completion loop);
[AnandTech on the Cortex-A510](https://www.anandtech.com/show/16693/arm-announces-mobile-armv9-cpu-microarchitectures-cortexx2-cortexa710-cortexa510/4);
[GMP's notes on SPARC T4 and T5](https://gmplib.org/devel/sparc);
[XMSS and Embedded Systems](https://eprint.iacr.org/2018/1225);
[To Be, or Not to Be Stateful](https://eprint.iacr.org/2022/1198.pdf).

## 5. Order of work (Decided)

The work lands in steps, each tested on its own before the next begins. Each
old device window is removed as soon as its replacement qualifies, and one
front end serves each engine throughout. Public BIOS and KDOS words keep their
stack effects, statuses, byte order and staged publication through every
step. The capability bits in System Info keep their meanings, and a new bit is
set only once its complete path qualifies on every backend.

0. **Inventory and numeric contract.** Record what each of the 256 `FB`
   selectors does in the assembler, the full-core and microcore decoders, and
   every execution backend. Today the full-core circuit retires a selector of
   an unassigned unit as a no-op while the microcore traps it as
   `ILLEGAL_OP`, and the contract settles one behavior. Then write the numeric
   contract for the first instructions: encodings, operands, claim and
   release, faults, retirement, reset and capability bits.
1. **Raw Keccak permutation.** A one-shot instruction permutes a 200-byte
   state in memory on the caller's own copy, and the seven copies are built in
   this first step. The old shared Keccak device keeps its own engine until it
   is removed, so the old and new paths each have an engine of their own.
   `KECCAK-F1600 ( state-200 -- status )` keeps its stack effect.
2. **Streaming Keccak and the chain controller.** Claim, absorb, squeeze,
   save, load and release run on each copy, with a chain controller beside
   each. The SHA-3, SHAKE and `WOTS-CHAIN` words move onto the copies with
   their stack effects and statuses intact, and the devices at `+0x0780` and
   `+0x08A0` are then removed, along with the chain controller's bus port.
3. **AES with GHASH.** Block operations run under a key schedule held in the
   engine, and software composes GCM and CBC over them. The circuit is
   reconciled with the executable model and qualified against standard
   vectors, and an AEAD decryption authenticates before it publishes
   plaintext. The device at `+0x0700` is then removed.
4. **Field unit and lattice arithmetic.** The field unit is wired into full
   cores and clusters with its known defects fixed: the custom-prime zero
   fallback, hidden state after noncanonical `GF.ADD` and `GF.SUB`, and the
   carry in native `GF.MACR`, whose C++ emulator fix is on branch
   `secgaps-montgomery`. The NTT's butterfly is built into the tile engines
   and compared with a standalone engine before the choice is confirmed. The
   standardized ML-KEM and ML-DSA transforms receive identities of their own,
   separate from the generic cyclic NTT.
5. **ML-KEM in software and the last old windows.** Software composes ML-KEM
   over Keccak and the NTT, tested against FIPS 203 vectors that include
   malformed inputs, modified ciphertexts and implicit rejection. The devices
   at `+0x08C0` and `+0x0900` are then removed, and the capability bits change
   to match.

Each step's tests cover:

- encoding: every assigned and reserved selector, instruction lengths, traps
  and return addresses, and a trap or defined status for every unsupported
  selector;
- the mathematics, against independent standard vectors;
- operand movement: alignment, complete spans, protection, aliases and
  faults;
- ownership: full cores and microcores together, same-core tasks, and state
  kept to its own caller;
- lifetime: interrupts, cancellation, each reset scope, suppression of stale
  results, and wiping;
- the BIOS and KDOS words and their users;
- every execution backend: the Python and native C++ emulators, the hosted
  simulator and the circuit.

Synthesis, timing, side-channel resistance and entropy quality each need
checks of their own beyond these functional tests.

Each step also measures the old device path against the new instruction path
on the same correct workloads, counting latency, bus transfers and waiting
with one, four and sixteen callers, and it reports estimates apart from
measurements.

## 6. Candidates from the release's security work

The first release's certificate checks and passphrase unlock run several
operations in software that hardware could take over. The software versions
ship first, each behind an interface that a hardware path can replace. Every
candidate below gives its measured software cost on MegaPad's emulator
(2026-11-24; seconds at the documented 100 MHz clock) and the hardware that
would speed it. Item 1 follows from §2's save rule; the others remain
candidates, and any that the chip adopts follows §2 and §3.

1. **SHA-256 saves and loads its chaining state** (Decided by §2).
   PBKDF2-HMAC-SHA-256 at 100,000 iterations derives the vault key once per
   boot and takes 650 million cycles, 6.5 s, because every HMAC hash
   recompresses its 64-byte key block. Loading the saved keyed state halves
   the compressions, to about 3.3 s (projected). An HMAC iteration mode that
   chains U = HMAC(P, U) and XORs each U into T would leave about two
   compressions of engine time per iteration (candidate).
2. **A wide Montgomery unit for RSA.** RSA PKCS#1 v1.5 and RSA-PSS checks of
   2,048 to 4,096 bits, possibly 8,192, run on a Montgomery module in
   `networking.f` (branch `secgaps-montgomery`) that issues three field-unit
   calls per 256-bit digit step. A Montgomery multiply takes 55.5k cycles at
   2,048 bits, 117k at 3,072 and 201k at 4,096, and an RSA verify takes 1.68M
   cycles (17 ms), 3.50M (35 ms) and 5.93M (59 ms) at those sizes. A
   word-serial unit running the whole CIOS loop over memory operands would
   need one instruction per product.
3. **A 384-bit field width for P-384.** ECDSA P-384 verification, which 21 of
   45 surveyed TLS hosts need, takes 72.0M cycles (0.72 s) on the same
   module, against 1.82M (18 ms) for P-256 on the field unit's built-in prime.
   A field width of 384 bits, or up to 521, makes a P-384 multiply one call,
   for about 50 ms per verify (projected) and an estimated 25 agent hours of
   work. The first release keeps the software path, and hardware is chosen
   later on measured numbers.
4. **A round unit for memory-hard passphrase hashing.** Within a 6-second
   budget, software Argon2id reaches about 6 MiB and scrypt about 1 MiB
   (projected). A BLAKE2b round unit, Argon2's compression function, or a
   Salsa20/8 core, scrypt's, would make tens to hundreds of MiB practical in a
   few seconds.
5. **General multi-precision arithmetic.** Carries between 64-bit limbs cost
   about 150 to 190 cycles a limb in Forth (an 8-limb add measures 1,504
   cycles), more per bit than the multiply itself. A multiply-accumulate with
   carries across limbs, together with a Montgomery or Barrett reduction step
   for any odd modulus up to a few thousand bits, would serve RSA, P-384 and
   the language's big integers with one unit.

TLS 1.2, which about one tested feed site in ten still requires, adds no
primitive family: it uses ECDHE and AES-GCM over the curves and hashes above,
and its HMAC-based PRF would gain a little from item 1's iteration mode.

## 7. Open items

- **Tasks on one core.** Two tasks on one core share that core's copy. Either
  the checked BIOS words keep an owner record of core and task for each copy,
  as the crypto guard does today for the shared devices, or each copy holds a
  task tag beside its owner. Step 0's numeric contract chooses.
- **Encodings.** Step 0 assigns selectors, and `00`–`2F` keep their present
  assignments. The suggested ranges are `30`–`3F` for AES, `40`–`4F` for the
  binary-field arithmetic of GHASH, `50`–`5F` for Keccak and its sponge
  operations, and `70`–`7F` for hash chains. The NTT's encoding follows its
  placement at step 4.
