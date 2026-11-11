# The token form of modules — proposal, 2026-11-11

Status: a proposal with a working prototype. The prototype is on the branch
`token-modules` of MegaPad and of Akashic and is not merged. Nothing here is
enabled unless a build asks for it with `--token-modules`. The decisions that
remain with the author are under [Decisions](#decisions), and what is still
owed before the form could land is under [What remains](#what-remains).

A module's token form is a record of what the interpreter did with its
source: the words it found, the numbers it converted, and the text each
parsing word took. Loading the record does the interpreter's work again
without splitting text into tokens and with one dictionary search for each
name instead of one for each use. It is still Forth. Every word and number
passes through the same two steps of the interpreter that a source token
passes through, on the BIOS and in the simulator, so one file loads on a
device, on the emulator, in the simulator and in hybrid mode. Source files
load as they did.

## What it gives

Emulator, `desktop-apt1`, from reset to the boot line that says the guest is
entering Desk. "Source" is this branch booting source; MegaPad's main
(00074ef7) took 1,020 and 1,432 million instructions for the same two boots.

| Boot | | Instructions | System cycles | At 100 MHz |
|---|---|---:|---:|---:|
| Linked chunks | source | 1,024 M | 1,330 M | 13.3 s |
| | token forms | 428 M | 582 M | 5.8 s |
| System volume | source | 1,440 M | 1,864 M | 18.6 s |
| | token forms | 762 M | 1,009 M | 10.1 s |

A token boot builds the dictionary its source boot builds. Every entry is
at the same address with the same name and flags, `HERE` and `LATEST` are
equal, and the terminal output is identical. The bytes that differ are
cells that hold the addresses of memory the load allocated and the loader's
own bookkeeping; [Equivalence](#equivalence) gives the count and the check.

## The records

A token stream is a sequence of records, read by `TOKEN-REPLAY
( rec-addr rec-len tab-addr tab-slots -- )`. A first byte below `F0` begins a
two-byte slot number, high byte first, which names a word in the replay's
table. Any other first byte is an opcode.

| Record | Bytes | Meaning |
|---|---|---|
| word | `ss ss` | the word in slot `ssss` |
| IMPORT | `F0 n name` | find NAME now, put it in the next slot, and take it as the word found |
| NUM1, NUM2, NUM4, NUM8 | `F1 b`, `F2 h`, `F3 w`, `F4 q` | a number in one, two, four or eight bytes, signed |
| TEXT | `F5 n text` | the input for the next word: what it parses |
| LINE, LINES | `F6`, `F7 h` | the next source line begins; `h` lines on |
| SKIPPED | `F8` | the skip that the last word began ends here |
| DEFINED | `F9` | the word whose definition the last record completed takes the next slot |
| END | `FF` | the end of the stream |

### How a word is bound

The table is a row of cells, one for each word the file uses, filled as the
replay goes. Nothing in a file names a word by its place in the dictionary.
That place differs between the BIOS and the simulator, and in hybrid mode
it differs again, because the routine words are defined before the image
boots.

A word that exists before the file uses it is bound by IMPORT at the point
of its first use. The replay searches the dictionary for the name there, as
the source line would have, and later uses of that binding are two-byte
slot numbers. A name that is defined again between two uses is a different
word with a slot of its own; the recorder knows, because it sees which
definition each token found.

A word the file itself defines is bound by DEFINED, a single byte after the
record that completes the definition: the `;` of a colon definition, or the
`VARIABLE`, `CONSTANT`, `CREATE` or defining word that made it. The replay
takes the dictionary's newest entry at that point. This needs no search and
does not put the name in the file a second time. In the Desktop's chunks
35,745 of the 59,431 words bound are bound this way and 23,686 by a
search. DEFINED follows the completing record and not the `:`
because the BIOS links a colon definition into the dictionary at `:` and
the simulator at `;`; after the `;` the newest entry is the same word on
both.

Numbers are stored converted, in the smallest of four sizes.

### The text parsing words take

A word that parses takes its text from the input buffer. The recorder notes
what each word consumed when it ran: the bytes of its source line from just
after the word's own token to where the input position stood when the word
returned. The file carries those bytes in a TEXT record before the word, and
the replay makes them the whole input for that one word. `: NAME` runs with
` NAME`, `S" hello"` with ` hello"`, and `REQUIRE utils/string.f` with
` utils/string.f`. The BIOS copies the text into its input buffer, where
every parsing word already reads, so no parsing word changed. A text is at
most 255 bytes because it came out of one source line.

A backslash comment leaves nothing in the file. A parenthesis comment leaves
its word and no text: `(` is an immediate word, and running an immediate
word while compiling resets what the BIOS compiler may fuse with the code
before it, so dropping the word could change the code the next two tokens
compile to.

Two things a parsing word can do have no equivalent in a replay. A word that
looks at the rest of its line without consuming it sees an exhausted input.
A word that moves the input position backwards re-reads only its own text.
No source in MegaPad or Akashic does either: `>IN`, `SOURCE`, `REFILL` and
`TIB` occur nowhere in `kdos.f`, `networking.f`, `rich-terminal.f` or
Akashic's modules, none of those files defines an immediate word, and the
simulator has no `>IN` or `SOURCE` at all.

### Conditional compilation

`[IF]`, `[ELSE]` and `[THEN]` are replayed as the words they are. The flag
is computed again at load time, on the machine that loads, from whatever the
source computes it from. When the flag is false the interpreter begins a
skip. The source text it would skip is not in the file; the file has SKIPPED
where the skip ended when it was recorded. After each word it runs, the
replay looks at the interpreter's skip state. A skip that began where the
file has no SKIPPED, or a SKIPPED where no skip began, means the load took
another branch than the recording did, and the replay ends with status 7.
Akashic's source has 665 `[IF]`s, nearly all of them layout assertions and
guards on a definition's presence. Each is evaluated again on every load,
and none is taken on trust from the build.

## Who writes the file

The prototype records the file in the simulator when an image is built. The
builder first builds the image from source for the simulator and boots it
once with a recorder attached to the runtime (`simulator/token_recorder.py`).
The recorder taps the interpreter at the points where a source token is
disposed of: a word found, a number converted, a word returned from running.
The record is therefore made by the interpreter that loads the source, and
the builder gains no reader of Forth of its own. A file is the buffer one
walk of KDOS's loader or of its checked source evaluator covers. The
recorder learns where a walk begins from a hook on `_LD-WALK` and
`SOURCE-EVALUATE-CHECKED`, and that it ended cleanly from
`EVALUATE-FINISH`. While it records, every line goes through the simulator's
reference evaluator and the native line walker stands aside.

The one boot gives every file's token form, keyed by the SHA-256 of its
text, and the builder then writes the image a second time with each file in
that form. A linked Desktop built with `--token-modules` takes about
35 seconds and one with the system volume about
65, where the source builds take two or three.

Two other ways to produce the file were weighed.

- **A recorder in the guest.** The BIOS interpreter can write the same
  records itself. At each point the recorder taps, the BIOS has the
  dictionary entry, the value or the input positions in hand. KDOS would
  give it a buffer for the length of a load and write the file. A machine
  could then make token files itself, and the record would be its own
  interpreter's. It costs a tap in the BIOS (about three instructions for
  each source token when it is off), the same tap kept in the simulator
  with a test that both write the same file, and KDOS code that turns entry
  addresses into slot numbers and writes the file. The format needs no
  change for it. Whether to build it is the first of the decisions below.
- **A tokenizer on the host.** It would be a second reader of Forth. It
  would have to know every parsing word and every defining word, including
  those a module defines, and it cannot know which definition a name finds
  without running the load. It was not pursued.

## Where the replay lives

`TOKEN-REPLAY` is a BIOS word beside `EVALUATE`, with a hosted counterpart
in the simulator, as every BIOS word has. The interpreter's two decisions
are now stated once on each machine and shared by every front end.

- In `bios.asm`, `interp_found_word` decides whether a word runs, which it
  does while interpreting or when it is immediate, or is compiled through
  `jit_compile_word`, and `interp_found_number` compiles a literal or
  pushes. QUIT's loop and EVALUATE's loop each held a copy of both; they
  and `TOKEN-REPLAY` now call the one. A word that is to run is called by
  its front end, with the code address the decision hands back, so that it
  finds the return stack as deep as it always was.
- In `simulator/runtime.py`, `_evaluate_found_word` and
  `_evaluate_found_number` are called by the source evaluator and by the
  replay.

The call costs the source path about four instructions a token: the linked
source boot is 1,024 million instructions on this branch against 1,020
on main.

A loop in KDOS Forth was the alternative, and the measurements rule it out.
Neither backend publishes a guest `COMPILE,`, and the BIOS compiles every
found word through one entry, so a Forth loop would need new primitives on
both backends for exactly the steps above and would then run fifteen or
more Forth words for each record. In the source boot's profile a Forth word
costs about 18 instructions with its call (302 million instructions over
16.8 million calls), so such a loop would cost about 270 instructions a
record. The BIOS loop costs about 55, its two shared steps included:
58 million instructions for the 1.05 million records of
the Desktop's 52 chunks. The Forth loop's extra 225 million
would be about half of the whole token boot.

## The loader's contracts

`TOKEN-EVALUATE-CHECKED ( i*x addr len -- j*x status )` in KDOS runs a token
file held in memory, as `SOURCE-EVALUATE-CHECKED` runs a source buffer, and
the loader and Akashic's cold-source loader call one or the other according
to the buffer's first eight bytes.

| Contract | In a token load |
|---|---|
| Statuses | EVALUATE's 0 to 5, unchanged; 6, the stream or file is malformed; 7, it does not match this system |
| `EVALUATE-FINISH` | called at the end, so an unfinished definition or conditional is status 4, as for source |
| `PROVIDED` and the skip of a loaded module | the header names the module the source provides; the loader registers it before anything runs and skips the file when that module is loaded, as the prescan of a source file does |
| The registry of loaded files | holds the identity of the file that was read, so a repeated `REQUIRE` is skipped after one path resolution, as now |
| Rollback after a failed load | the same guard (`_LD-GUARDED`, `_LD-FAIL`) surrounds either walk; `TOKEN-REPLAY` saves and restores the caller's input as EVALUATE does, and KDOS unwinds an exception through `EVALUATOR-UNWIND` |
| `_LD-FAIL`'s status discipline | unchanged |
| `EVAL-LINE` | LINE records keep it at the source line being replayed, so a failure names the line of the source; `EVAL-COLUMN` is 0, and `EVAL-TOKEN` holds a name IMPORT could not find |
| The 255-byte line limit | binds the source that is recorded and each TEXT; the stream has no lines |
| The JIT and its inlining | the replay compiles through `jit_compile_word` and `jit_compile_literal` in whatever state the JIT is in at load time, which is the state a source load would find |
| Nested loads | `REQUIRE` in a token file is the word `REQUIRE` with its path as text; the nested file loads from its token form or from source |
| The cold-source container | its CRC covers the token bytes it holds |

`_LD-WALK`, `SOURCE-EVALUATE-CHECKED` and the other words that the
simulator's source overlays bind by digest are unchanged.

## The file

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `MFTOKEN1` |
| 8 | 4 | bytes before the records: 64 and the padded module name |
| 12 | 4 | cells in the table |
| 16 | 8 | bytes of records |
| 24 | 8 | bytes of the source it was recorded from |
| 32 | 4 | CRC-32 of that source |
| 36 | 4 | CRC-32 of the records |
| 40 | 4 | lines in the source |
| 44 | 2 | bytes of the module name |
| 46 | 2 | flags, zero |
| 48 | 8 | load-chain stamp (see [Staleness](#staleness)); zero in the prototype |
| 56 | 8 | reserved, zero |
| 64 | | the module name the source provides, zero-padded to eight bytes |

The table is not in the file. KDOS allocates it for the length of a replay
from the slot count in the header, so the file holds only what was recorded.

For the Desktop the token forms are 3.4 MB for the 7.1 MB of source the linked
boot loads (48%), and 3.2 MB beside the 9.5 MB of plain source files on the
system volume (33%); [Measurements](#measurements) has the parts.

### Validation

A token file is code, as its source is, and a hostile one can do whatever a
hostile source file can. What the loader guarantees is that reading the
file cannot corrupt the loader. KDOS checks that the header's sizes account
for exactly the bytes it was given before it uses any other field.
`TOKEN-REPLAY` checks every record against the stream's end before it reads
the record's operands. It takes a slot only below the count of slots
already filled and fills a slot only below the table's size. An unknown
opcode, a stream that ends without END, and a length that would wrap the
address space are malformed. The table's cells come only from a dictionary
search or from the dictionary's newest entry, and they stand only while no
entry leaves the dictionary: the BIOS counts the calls of `LATEST!` and
`DICT-ROLLBACK` that succeed, and a replay ends with status 7 when the
count moves under it.

### Staleness

A token file holds three things that were true when it was recorded and
that nothing in it checks again at load time.

1. It is the record of one particular source text.
2. Each token was a word or a number. A token that converted as a number
   stays a number, though a word of that name defined since would now be
   found first.
3. Each parsing word took the text it took. A parsing word changed to take
   more or less would be given the old text.

Names are not among them, since every name is searched at load time.
Conditional compilation is not either, since it is checked as described
above. Fact 1 depends on the file's own source. Facts 2 and 3 depend on
everything loaded before the file, the BIOS and KDOS included, though only
through those two narrow routes.

The prototype makes no staleness check: a token form that is present and
well formed is used. That suits an image built in one piece and does not
suit a volume a person edits. The rule proposed for landing is the third of
the decisions below.

## Where the files live

**The boot volume (MP64FS).** A linked chunk keeps its name and its
cold-source container and holds the token form in place of the source.
`networking.f`, `rich-terminal.f` and the cold-source loader hold their
token form under their own names; KDOS tells a token file by its first
eight bytes, whatever it is called. The directory therefore has the entries
it had, 86 of 128 for the linked Desktop and 66 with the system volume, and
the volume has more free space than before: 58,256 free sectors
against 51,032 for the linked Desktop, and 63,486 against 61,505 on the
boot volume of the system-volume build. The Desktop's
free-space reserve is not touched.

**The system volume (ext4).** A module's token form sits beside it, named
as the module with a `t` added: `utils/string.f` and `utils/string.ft`.
KDOS looks for it only when `MODULE-TOKENS` is set, which the boot script
does after it mounts a volume built with token forms. The variable is zero
when KDOS starts, so a system without token files pays nothing. With it
set, the loader asks the module source for the path with a `t` added first
and for the path itself when that names no file. The file it finds gives
the module its identity in the registry of loaded files, so a repeated
`REQUIRE` still costs one resolution. A module that has no token form costs
a second resolution each time it is required. The volume's size does not
change: the driver's geometry sets its minimum of 131 MiB, and it holds
9.5 MB of source and 3.2 MB of token forms.

The module-source contract changes in one respect. The loader may call
RESOLVE twice in one load, and a RESOLVE that returns `MS-E-NOT-FOUND` must
leave the source's current directory as it was. Akashic's VFS source
already does; KDOS's own MP64FS source now restores the directory it walked
from.

## What cannot take the form

A file stays source when

- its load takes entries out of the dictionary (`DICT-ROLLBACK`, `LATEST!`),
  which a replay cannot survive;
- it was not loaded as a file in the recorded boot;
- it uses more than 61,440 distinct words; or
- its load does not finish.

The recorder marks the first and the last, and the builder leaves any file
without a recorded form as source. No file of the Desktop was marked
unfit. Nine modules of its system volume have no form (`concurrency/event.f`,
`semaphore.f` and `guard.f`, `text/utf8.f`, `utils/uint-range.f`,
`memory-span.f` and `bitset.f`, `math/crc.f`, `utils/fs/vfs.f`). They are part
of what mounts the volume, so they load from the boot volume's chunks, in
token form there, and their files on the volume are never loaded.

The boot script, `autoexec.f`, stays source by rule. It is the file a
person edits to change what the machine loads, and it is 9 KB.

## The simulator

`TOKEN-REPLAY` is the simulator's 459th core word. It runs in Python on the
runtime's reference path and gives each record to the same two methods a
source token reaches. Hybrid mode runs Forth as the simulator does and
loads the same files; its routine words exist before the image boots, and
a token file finds them by name, as source does.

The source-acceleration overlays and their pins are untouched, because no
word they bind changed. They serve source loads as before, and a token load
does not pass through them.

Host time is not device time. In the simulator the linked Desktop
reached Desk's entry in 2.8 and 3.2 seconds from source and in 4.0 and 4.0
from token forms; with the system volume it took 11.9 and 12.2 seconds from
source and 17.5 and 18.0 from token forms. The replay is Python where the
source path has a native line walker, which is the second more on the
linked boot. On the system volume the larger part is the ext4 driver: the
token boot calls the `CRC-FEED` primitive 398,573 times where the source
boot calls it 291,011 times, and each call leaves the native executor. The
extra calls are the driver's checksums of directory blocks, which the
lookups of token forms add to. A native replay is among
[what remains](#what-remains).

### Section 9 of the simulator contract

Section 9, "Native and warm images", reserves "a future semantic warm
artifact" with its own magic, version and extension and a cache key that
includes the simulator and runtime version. That was a cache private to the
simulator, to stand beside machine-code images for the machine. The token
form replaces it with one file for every backend, and the section would
then read:

> MF64 v1/v2 images contain native dictionary bytes, relocations, imported
> execution tokens, and executable entries. The simulator may read, copy,
> hash, and persist MF64 files as opaque data, but it does not execute them
> or claim to validate their native semantics.
>
> A module may also be loaded from its token form (`MFTOKEN1`,
> `docs/token-modules.md`), a record of the interpreter's work on the
> module's source, which `TOKEN-REPLAY` runs through the interpreter's own
> steps. The same file loads on every backend. It holds no state of any
> backend, so no simulator or runtime version enters its validity; that
> document says what ties it to its source and to what was loaded before
> it. It cannot become a production path before cold, warm, and
> source/warm equivalence all pass and useful wall time is demonstrated.

The gate sentence stays as it is.

## Equivalence

The comparison boots two images of the same Desktop on the emulator, one
from source and one from token forms, until everything is loaded and the
machine waits at the prompt, and compares what the two loads built. The
images are the Desktop's with one line added to the boot script, which
hides the session entry with a word that prints a mark and returns, so the
boot stops at the prompt and does not enter Desk. It was run for the linked
boot and for the boot from the system volume.

In both pairs the dictionary's structure is equal. Every entry is at the
same address with the same name and flags, 50,472 entries in the linked
boot and 54,564 with the system volume. `HERE`, `LATEST` and the end of the
system dictionary are equal, and the terminal output of the two boots is
identical.

The bytes are not all equal. The comparison puts every byte that differs
to the word that holds it and into one of three classes, and fails on a
byte that fits none.

| | Linked | System volume |
|---|---:|---:|
| dictionary bytes compared | 8,918,819 | 9,972,567 |
| bytes that differ | 2,628 | 2,238 |
| in a cell that holds the address of memory the load allocated | 592, in 166 words | 1,054, in 350 words |
| in the data of the loader's, the allocator's or a filesystem's own words | 2,034, in 25 words | 1,182, in 49 words |
| in the data of a word set from a clock (`DNS-ID`) | 2 | 2 |
| unexplained | 0 | 0 |

**Addresses.** The loader's buffer for a token file is smaller than for
its source, so memory that a module allocates while it loads lies at
another address, and so does everything allocated after it. A cell that
holds such an address holds another value: a variable's cell, a CONSTANT's
value, or a literal compiled into a definition. Networking's tables are
most of them. The comparison accepts a difference there only when the
cell's value in both boots is an address in external memory outside the
dictionary.

**State.** The loader, the module registry, the allocator, MP64FS, the
cold-source loader, the VFS and the ext4 driver keep cursors, buffers,
sizes and checksums of the files they read, and the two boots read other
files. `MODULE-TOKENS` is among them, since only the token boot sets it.
The comparison accepts a difference there only in the data field of a
VARIABLE or CREATEd word whose name is on its list of those words.

**The clock.** `DNS-ID` takes its value from the timer when networking
loads, and the two boots reach that line at different times.

No byte of any definition's code differs except an address of the first
class. So the expectation that a token load leaves the dictionary
byte-identical holds for everything the load compiles and does not hold
for what the load allocates: the token load allocates the same things in
another place. A load that took the working space its source would have
taken would allocate in the same place too. That is possible where the
loader knows the source's size before it allocates, which the cold-source
container's header can tell it and a file found by name cannot, and it was
not built.

As a gate, the comparison is two boots and a file comparison: about 75
seconds of emulator time for the linked pair and 130 for the system
volume's. Its scripts are scratch tools of the prototype and become a test
with the landing.

## Measurements

All figures are from one round on MegaPad `token-modules` 480ade1c and
Akashic `token-modules` 66ff1fb4, run one boot at a time on a machine that
other work shared. A later commit moved the call that runs a found word
out of the shared step and into each front end; the linked boot from token
forms, measured again after it, took 428 million instructions and
582 million cycles. Device figures are the emulator's, for `desktop-apt1`
from reset to the boot line that says the guest is entering Desk, in
millions of instructions read between boot lines at a resolution of two
million.

| Linked chunks | Source | Token forms |
|---|---:|---:|
| BIOS and KDOS | 26 | 26 |
| networking, rich terminal, cold-source loader | 88 | 32 |
| the Desktop's modules, 52 chunks | 908 | 370 |
| the rest | 2 | 0 |
| total | 1,024 | 428 |
| system cycles | 1,330 | 582 |

| System volume, 4 KiB blocks | Source | Token forms |
|---|---:|---:|
| BIOS and KDOS | 26 | 26 |
| networking, rich terminal, cold-source loader | 86 | 30 |
| the volume's driver, 8 chunks | 138 | 54 |
| the mount | 12 | 12 |
| the Desktop's modules, 310 files | 1,176 | 640 |
| the rest | 2 | 0 |
| total | 1,440 | 762 |
| system cycles | 1,864 | 1,009 |

At the SoC's 100 MHz the linked boot goes from 13.3 seconds to 5.8, and the
boot from the system volume from 18.6 to 10.1.

### Where the instructions go

The guest's program counter was sampled every 4,099 instructions through
the module phase of each token boot and put to the BIOS label or Forth word
that held it. The source column is the same phase of MegaPad's main
(00074ef7), from `docs/performance/loader-line-cost-2026-11-11.md`. In
millions of instructions:

| Module phase, linked chunks | Source | Token forms |
|---|---:|---:|
| splitting text into tokens | 89 | 9 |
| dictionary searches | 135 | 26 |
| work around each line: saving and copying input, finding its end, KDOS's line words | 162 | 0 |
| the token replay's loop and the interpreter's two steps | | 58 |
| compiling: the scan of the inline table | 73 | 74 |
| compiling: the rest | 133 | 111 |
| checksums of the chunks | 77 | 38 |
| Forth words run during the load, with their calls | 238 | 55 |
| total | 907 | 370 |

For source, most of the last row is KDOS walking lines. For token forms it
is what the modules themselves run when they load. What a token load still
splits is the text of the parsing words (a definition's name, a string),
and what it still searches is each outside name once for each file, plus
the names that `'`, `[']` and `[DEFINED]` take.

The token boot from the system volume spends its module phase differently:
of 624 million instructions, 307 million are Forth words run during the
load with their calls, the ext4, VFS and disk words among them, and 43
million are checksums, which here are the driver's own of its metadata
blocks. The replay, the searches and the compiling take what they take in
the linked boot. The driver has no block cache and resolves the path of
every `REQUIRE` from the directory blocks again.

### Sizes

| | Files | Source | Token forms | |
|---|---:|---:|---:|---:|
| Linked boot: 52 chunks, `networking.f`, `rich-terminal.f`, the cold-source loader | 55 | 7,105,428 | 3,407,319 | 48% |
| of which the 52 chunks | 52 | 6,317,960 | 3,111,804 | 49% |
| System volume: modules, as plain files with their comments | 301 of 310 | 9,541,901 | 3,168,384 | 33% |

The 52 chunks hold 1,054,713 records: 685,542 words by slot (44% of the
bytes), 51,006 texts (30%), 23,686 IMPORTs with their names (12%), 73,340
numbers (6%), 185,260 line marks (6%), 35,745 DEFINEDs and 134 SKIPPEDs.
Their tables, allocated one file at a time while it loads, come to 59,431
slots, 475 KB in all and about 9 KB for a chunk.

### The build

A linked Desktop with `--token-modules` built in 34 and 36 seconds and
one with the system volume in 63 and 66, against two or three seconds for
the source builds. Nearly all of it is the recording boot, which evaluates
every line on the simulator's reference path: 31 seconds for the linked
image's 56 files and 52 for the system volume's 313.

### The simulator's host time

Host seconds from the start of `kdos.f` to the line that says the guest is
entering Desk, two runs each, native executor. These say nothing about a
device.

| | Source | Token forms |
|---|---:|---:|
| Linked chunks | 2.8, 3.2 | 4.0, 4.0 |
| System volume | 11.9, 12.2 | 17.5, 18.0 |

The simulator's semantic steps for the system-volume boot are 69.7 million
from source and 74.2 million from token forms, and its disk reads 11,986
and 10,784.

## Decisions

Three decisions are the author's. Each is stated with what it rests on, the
options and what follows from them, and a recommendation.

### 1. Where token files are built

A token file is the record of a real source load. In the prototype the
image builder makes that load happen once in the simulator and writes the
files into the image. A machine never writes one: a module that a person
writes or installs on a machine loads from source, as it does today.

- **On the host, when an image is built.** This is the prototype. It costs
  the device nothing and the builder one source boot of the simulator. The
  simulator's interpreter is the recorder, and the equivalence gate on the
  emulator is what ties its record to the BIOS.
- **Also on the machine.** The BIOS interpreter gets the recording tap and
  KDOS a word that loads a file from source while recording and writes the
  token file beside it. A volume could then be compiled where it is: by an
  installer, after an update, or on request. The record would be the
  device's own interpreter's. Applets are the case that most wants it:
  Akashic's application builder produces machine-code images on the
  machine, which cannot run in the simulator or in hybrid mode, and a token
  file recorded on the machine would run on all of them. The costs are the
  tap in two interpreters with a test that both write the same file, KDOS
  code for the slot numbers and the file, BIOS size, and a decision about
  when a machine compiles. The system volume is read-only, so its token
  files would still come with the image.

The format is the same for both; every record is something the BIOS
interpreter has in hand where the tap would sit. Recommended: build on the
host for the first release, and add the recorder on the machine when
applets or user modules are to take the form.

### 2. Whether source ships beside the token forms

A token form is 48% of the size of a linked chunk's compacted source and a
third of a plain source file with its comments. The linked boot's chunks are
already not source a person reads: they are compacted, with comments removed
and modules run together. The system volume's files are the repository's
text, which Pad and File Explorer show and which a stale token form falls
back to.

- **Token forms in place of the chunks on the boot volume, and beside the
  source on the system volume.** The boot volume gains free space and the
  system volume keeps its readable modules. This is the prototype.
- **Both on both volumes.** The boot volume could then fall back to source
  too, at the cost of its free space.
- **Token forms only.** The smallest volumes, with nothing to read and
  nothing to fall back to.

Recommended: the first. The system volume's size is set by the driver's
geometry and does not grow.

### 3. What makes a token file stale

The section [Staleness](#staleness) names the three facts a file holds from
its recording: its own source text, which tokens were numbers, and what
text each parsing word took. The last two depend on everything loaded
before the file.

- **The file's own source only.** The header holds the source's length and
  modification time (its CRC on MP64FS, where the directory has one), and
  the form is used when they match the source beside it. This is what
  Python does for its compiled files. An edit to a module makes that
  module's form stale and no other, so a change that turns a number into a
  word, or changes what a parsing word takes, would go unnoticed in the
  files loaded after it.
- **The file's own source and a stamp of the load so far.** KDOS keeps a
  running stamp: the CRC of the BIOS and of `kdos.f`, then the CRC of each
  file's source in the order they load. A token file records the stamp that
  stood when its recording began and is used only when the stamp stands the
  same at load time. A token load adds its header's source CRC to the stamp
  without reading the source; a source load adds the CRC of the text it
  read. Any change to anything loaded earlier then sends every later file
  back to source until it is recorded again. The cost at load is a few
  instructions for each file, and a CRC pass over each file that loads from
  source.
- **No check on the read-only system volume, one of the above elsewhere.**
  The image is built in one piece, so its files cannot go stale; only the
  volumes a person can write would be checked.

In each case a stale file loads from source, and the boot names the files
that did. Recommended: the second, on every volume. It is the only one that
covers all three facts, it costs little, and a boot that fell back is
slower but correct. The check against the file's own source needs the
source's directory entry, which the prototype does not fetch: it is one
more path resolution for each module that loads, about 0.4 million
instructions on the system volume with the ext4 driver as it stands. An
earlier round of the prototype resolved the source and then its token form
for each of the 301 modules it loaded, and its boot took 896 million
instructions where this one takes 762.

Two further matters follow from the measurements and are not part of the
token form. Whether the Desktop boots from the system volume by default was
waiting on the boot's cost. With token forms the linked boot takes 5.8
seconds and the boot from the system volume 10.1, and nearly all of the
difference is the ext4 driver resolving paths and reading blocks without
a cache, which token forms do not touch. And whether token builds
become what the Desktop image is built with is for the author once the
gates below have passed.

## What remains

The prototype loads the Desktop and was measured. It is not yet a change
that can land. In the order the work would go:

1. **The staleness rule and the fallback to source** (decision 3). The
   prototype has the header's fields and no check.
2. **One guarded evaluation in KDOS.** `_TOKEN-REPLAY-GUARDED` repeats
   `_EVALUATE-GUARDED` around another primitive. They become one word that
   takes the primitive, and the simulator's overlay that binds
   `_EVALUATE-GUARDED` by digest is pinned again.
3. **Tests.** The prototype has the replay's own tests on both machines
   (`tests/token_streams.py`, 15 streams, with the unlink and nested-failure
   cases). Owed: the loader's paths on both machines (the sibling chosen,
   the source file's identity kept, a module already loaded skipped from
   the header, a stale or malformed sibling, a failed replay rolled back, a
   nested `REQUIRE`, `LOAD`); the recorder (file boundaries, nesting, text,
   skips, definitions, a file marked unfit); the file format; the builder
   option and the cold-source loader in Akashic; and planted faults for
   each.
4. **Pins and documents the change moves.** `kdos.f` grew, so the
   simulator's slices are cut again with `tools/repin_kdos_slices.py` and
   the whole-load counts set by hand; the core-word count becomes 459 and
   the BIOS dictionary chain 561 entries; the dictionary-index test starts
   from a boot table that now passes the 768 names at which it doubles and
   needs a new starting point; `docs/BIOS-DICTIONARY.md`,
   `docs/bios-forth.md`, `docs/kdos-reference.md`, section 9 and the word
   list of `docs/simulator-contract.md`, and `docs/module-source-contract.md`
   (RESOLVE up to twice, and a path that names no file leaves the
   directory) take the new words; this proposal becomes
   `docs/token-modules.md`.
5. **The gates of AGENTS.md under "Forth test performance".** A real cold
   build is the recording build, which the prototype does. Source and warm
   equivalence is the comparison in [Equivalence](#equivalence), which
   becomes a test that runs with the build. Useful savings are the
   measurements above. The warm Desktop smoke is owed: the physical Desktop
   journey with `--token-modules`, linked and with `--system-volume`, which
   needs the option passed through `physical_desktop_acceptance.py`, and
   once with `--backend hybrid`.
6. **The simulator's host time.** The replay is Python; if token builds
   become what the journey boots, a native replay beside the native line
   walker keeps the simulator's boot where it is. It is a smaller piece
   than the walker, since there is no text to split.

Noticed on the way and outside this work:

- The BIOS compiler scans its table of inlinable primitives for every word
  it compiles, and a word defined above the BIOS can never be in it. That
  scan is 74 million of the linked token boot's 428 million
  instructions. One comparison of the entry's address with the
  end of the BIOS dictionary would skip it for those words, on the source
  path as well.
- The cold-source loader checksums each chunk in a loop of Forth words; with
  token chunks that is 38 million instructions. A BIOS word over a buffer
  would take most of it, as `SCAN` did for line ends.
- The boot from the system volume now spends most of its module phase in
  the ext4 driver: path resolution and block reads with no block cache.
- `S"` in interpret mode clamps its copy with `cmpi r12, 255`, and CMPI
  sign-extends its operand, so the comparison is with -1 and never true.
  The 255-byte line limit keeps the copy inside its buffer today.
