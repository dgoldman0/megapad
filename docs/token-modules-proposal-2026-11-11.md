# The token form of modules — design, 2026-11-11

Status: built and tested on the branch `token-modules` of MegaPad and of
Akashic, and not merged. Nothing here is enabled unless a build asks for it
with `--token-modules`. The three decisions that were the author's are
taken and recorded under [Decisions](#decisions); the staleness rule is
taken for now, and what is uncertain about it is stated where the rule is.
What is still owed is under [What remains](#what-remains).

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
entering Desk.

| Boot | | Instructions | System cycles | At 100 MHz |
|---|---|---:|---:|---:|
| Linked chunks | source | 1,028 M | 1,335 M | 13.4 s |
| | token forms | 430 M | 584 M | 5.8 s |
| System volume | source | 1,442 M | 1,866 M | 18.7 s |
| | token files alone, as it ships | 676 M | 899 M | 9.0 s |
| | token files with their source beside them | 1,230 M | 1,609 M | 16.1 s |

A token boot builds the dictionary its source boot builds. Every entry is
at the same address with the same name and flags, `HERE` and `LATEST` are
equal, and the terminal output is identical. The bytes that differ are
cells that hold the addresses of memory the load allocated and the loader's
own bookkeeping; [Equivalence](#equivalence) gives the count and the test.

The last row is the System Practice's volume, where KDOS reads each
module's source to check its token file against it.
[What it costs](#what-it-costs) says where those instructions go.

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

Token files are built on the host when an image is built. The builder first
builds the image from source for the simulator and boots it once with a
recorder attached to the runtime (`simulator/token_recorder.py`). The
recorder taps the interpreter at the points where a source token is
disposed of: a word found, a number converted, a word returned from running.
The record is therefore made by the interpreter that loads the source, and
the builder gains no reader of Forth of its own. A file is the buffer one
walk of KDOS's loader or of its checked source evaluator covers. The
recorder learns where a walk begins from a hook on `_LD-WALK` and
`SOURCE-EVALUATE-CHECKED`, that the loader announced the text as a file of
the load from a hook on `LOAD-STAMP-SOURCE`, and that the walk ended
cleanly from `EVALUATE-FINISH`. While it records, every line goes through
the simulator's reference evaluator and the native line walker stands aside.

The one boot gives every file's token form and the order in which the loads
began. The builder then writes the image with each file in that form,
stamped for the image ([The stamp](#the-stamp-and-when-a-token-file-is-stale)).
A linked Desktop built with `--token-modules` takes about 32 seconds and one
with the system volume about 54, where the source builds take two or three.

**A machine must also build them.** The System Practice, the developer one
with the working name Workshop (Part 7 of the release plan), has to build
token files on the machine before the release, so that a module edited
there gets its token file back without a host. That recorder is a
committed later step and none of it is built here. The format and the
loader are kept fit for it:

- The BIOS interpreter has, at each point the recorder taps, what a record
  needs: the dictionary entry, the value, or the input positions. A tap
  there costs about three instructions for each source token while it is
  off. The simulator would keep the same tap, with a test that both write
  the same file.
- A record names a word by a slot that is filled by a search or from
  `LATEST`, never by an address, so a file a machine writes is the file the
  host writes for the same load.
- The stamp a file needs is `LOAD-STAMP` as it stands when the load of its
  source begins, which KDOS keeps for any load once it has started, and the
  header fields are the source's length and CRC-32, which the loader has in
  hand when it announces a source file.
- Where the source ships beside the token file, an edited module loads from
  its source at once, and the modules after it do too, so the machine keeps
  working between an edit and the rebuild.

A tokenizer on the host was weighed and not pursued. It would be a second
reader of Forth. It would have to know every parsing word and every
defining word, including those a module defines, and it cannot know which
definition a name finds without running the load.

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

The call costs the source path about four instructions a token: stage 1
measured the linked source boot at 1,024 million instructions with it and
at 1,020 without.

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
| `PROVIDED` and the skip of a loaded module | the header names the module the source provides; the loader registers it before anything runs and skips the file when that module is loaded, as the prescan of a source file does. It takes the name only from a header whose sizes account for the file |
| The registry of loaded files | holds the identity of the file that was read, so a repeated `REQUIRE` is skipped after one path resolution, as now |
| Rollback after a failed load | the same guard (`_LD-GUARDED`, `_LD-FAIL`) surrounds either walk; `TOKEN-REPLAY` saves and restores the caller's input as EVALUATE does, and KDOS unwinds an exception through `EVALUATOR-UNWIND` |
| `_LD-FAIL`'s status discipline | unchanged |
| A file that cannot be loaded | statuses 6 and 7 end the load. The load that read the file prints ` Token file`, its name and ` cannot be loaded`, and throws the status; the loads it was nested in rethrow. Each loader frame keeps the name its file was found by, since the loads nested in a file use the path buffers again |
| `EVAL-LINE` | LINE records keep it at the source line being replayed, so a failure names the line of the source; `EVAL-COLUMN` is 0, and `EVAL-TOKEN` holds a name IMPORT could not find |
| The 255-byte line limit | binds the source that is recorded and each TEXT; the stream has no lines |
| The JIT and its inlining | the replay compiles through `jit_compile_word` and `jit_compile_literal` in whatever state the JIT is in at load time, which is the state a source load would find |
| Nested loads | `REQUIRE` in a token file is the word `REQUIRE` with its path as text; the nested file loads from its token form or from source |
| The cold-source container | its CRC covers the token bytes it holds; the boot script's wrapper names a chunk that fails |
| The stamp of the load | a token file that runs is taken into `LOAD-STAMP` by the length and CRC-32 of its source; the loader announces a source file with `LOAD-STAMP-SOURCE`, and so does the cold-source loader for a chunk of source |

`_LD-WALK`, `SOURCE-EVALUATE-CHECKED` and the other words that the
simulator's source overlays bind by digest are unchanged. The loader's own
source walk goes through a new word, `_LD-WALK-SOURCE`, which announces the
file and then calls `_LD-WALK`.

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
| 46 | 2 | flags: 1, the source ships beside the file |
| 48 | 8 | the stamp of the load where the file's recording began |
| 56 | 8 | reserved, zero |
| 64 | | the module name the source provides, zero-padded to eight bytes |

The table is not in the file. KDOS allocates it for the length of a replay
from the slot count in the header, so the file holds only what was recorded.

A file with no records only names its module. It stands on a volume for a
module that the system loads before it mounts the volume
([Where the files live](#where-the-files-live)).

For the Desktop the token forms are 3.4 MB for the 7.1 MB of source the linked
boot loads (48%), and 3.2 MB for the 9.5 MB of plain source files of the
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

## The stamp, and when a token file is stale

A token file holds three things that were true when it was recorded and
that nothing in the records checks again at load time.

1. It is the record of one particular source text.
2. Each token was a word or a number. A token that converted as a number
   stays a number, though a word of that name defined since would now be
   found first.
3. Each parsing word took the text it took. A parsing word changed to take
   more or less would be given the old text.

Names are not among them, since every name is searched at load time.
Conditional compilation is not either: the replay checks every conditional
against the record and ends with status 7 when one goes the other way.
Fact 1 depends on the file's own source. Facts 2 and 3 depend on everything
loaded before the file, the BIOS and KDOS included, though only through
those two narrow routes.

### The rule

This rule is taken for now. What is uncertain about it is stated below.

1. A token file is used only on top of what it was recorded on. KDOS keeps
   a running stamp, `LOAD-STAMP`, that starts from the identity of the BIOS
   and of KDOS and takes in each file as its load begins, in order. A token
   file's header holds the stamp that stood where its recording began, and
   the file is used only when the stamp stands the same at load time.
2. Where no source stands beside the token file, a mismatch ends the load.
   KDOS prints ` Token form recorded on another system: this volume was
   built for another system`, the loader prints ` Token file`, the file's
   name and ` cannot be loaded`, and the load throws status 7. Nothing is
   loaded in the file's place. That is every volume except the System
   Practice's system volume, and later a user volume.
3. Where the source stands beside it, the token file is also checked
   against that source, by length and CRC-32, and on either mismatch the
   module loads from its source. The loader names each file that gave way:
   ` Token file`, the name and ` is out of date: loading its source`. The
   source is taken into the stamp by its own text, so an edited module takes
   with it every module loaded after it, until their token files are built
   again.

The rule is stated once, in `TOKENS-CURRENT? ( addr -- flag )`: the header's
stamp equals `LOAD-STAMP`. Narrowing it to the file's own source means
changing that word to answer true, after which a token file with its source
beside it is checked against the source alone and one without is always
used. The replay, the format and the builder would not change; the header's
stamp and the BIOS word would simply go unread.

### What the stamp is made of

**The identity of the BIOS and of KDOS** is one cell that a new BIOS word,
`BOOT-ID ( -- u )`, gives. The BIOS hashes its own code and dictionary, the
65,490 bytes below its first variable, which never change while it runs.
It then takes in the first file `FSLOAD` loads after reset, as it reads it.
For a boot from disk that file is KDOS, as the volume holds it. So the
running system needs nobody to tell it what it is: the BIOS that answers
is the code that was hashed, and KDOS is the bytes the BIOS evaluated. The
BIOS's messages lie past its variables and are not part of the cell. Asked
before any file was loaded, the word gives the BIOS's own cell, and the
answer is fixed until the next reset.

The hash takes each eight bytes X, and then each byte left over, as
`h = (h XOR x) * K`, `h = h XOR (h >> 32)`, in eleven instructions for a
cell. It tells one build from another; it is no defence against a file
made to collide.

`BOOT-ID` is hashed again at each boot, from the bytes below the first
variable as they then stand in memory. Nothing writes there during a
session. After 756 million instructions of a Desktop boot from token files
and Desk's first frames, those 65,490 bytes were the assembled BIOS's, byte
for byte. A test resets a machine that keeps its memory, boots KDOS from
disk a second time, finds the same bytes and the same identity, and loads
the same token file again.

The simulator has no BIOS image. Its `BOOT-ID` gives the cell of the BIOS
that `bios.asm` assembles to, which it assembles once when the word is
first asked for, and of the file an image boot reads as the boot file. The
assembly adds 0.09 host seconds to a simulator boot that meets a token
file, about 2.5% of the linked Desktop's 3.5 seconds; a boot from source
never asks. That is host time only. `shared/boot_identity.py` holds the arithmetic for it and
for the tools that build images. The simulator's own session root, which
it evaluates before the boot script, is not a loaded file and is outside
the stamp.

**The files.** `LOAD-STAMP` takes in a source text by its CRC-32 and
length, and a token file by the same two numbers of the source it was
recorded from, which its header holds, with the same step as the BIOS's
hash. A file is taken in when its load begins, so a file loaded from inside
another stands on the part of that other file which came before it.

The stamp is zero until a token file first asks for it. It starts then,
from `BOOT-ID` and the source files that are loading at that moment,
outermost first: on a boot, the boot script. From there on the loader
announces every source file it reads (`LOAD-STAMP-SOURCE`), and a loader
outside KDOS announces the text it evaluates itself; Akashic's cold-source
loader does so for a chunk of source. A boot that meets no token file never
starts the stamp and computes no checksum.

### What the stamp leaves out

The stamp starts at the first token file, with the source files that are
loading at that moment, and a file is taken in when its load begins. Three
things are therefore in no stamp.

- **A source file whose load ended before the first token file was met.**
  It was loaded, and the files after it stand on it, but its text was gone
  before the stamp began.
- **Anything defined outside a file:** a line typed at the prompt, and text
  given to `EVALUATE` or `SOURCE-EVALUATE-CHECKED` that no loader announced
  with `LOAD-STAMP-SOURCE`.
- **The files a file loads itself.** A file's header is compared, and the
  file taken in, when its load begins. A module it is the first to require
  loads after that point, so that module is not in the requiring file's
  stamp. If such a module changes, the requiring file's token file is
  still current, and the part of it recorded after the `REQUIRE`, which is
  most of a module, is replayed on top of the changed module. As the rule
  stands, an edit therefore makes stale the modules that happen to load
  later, whether or not they use the edited one, and leaves in use the
  token file of the module that brought the edited one in.

In the Desktop's two token boots the first two name nothing. The boot
script is the only file that loads as source. It is still loading when the
first token file, `networking.f`, begins, so it is in the stamp. Every
other file the boot loads is a token file: 55 in the linked boot, and with
the system volume 11 on the boot volume and 301 on the system volume.
Nothing is typed, and no text is evaluated outside a file, except the
simulator's session root, which the simulator evaluates before the boot
script. A build refuses an image in which a source file would end before
the first token file. The third applies wherever one module requires
another and the volume is changed in part; a volume built in one piece is
recorded in one boot.

Two of the shared cases pin the first two: a source file that ended, and a
word typed, before the first token file leave the stamp unstarted, and a
token file stamped as if that source file were in it is refused.

### What a token file could sidestep

Nothing. No load path checks a module's source text against a recorded
hash, signature or approval where a token file is loaded in its place.
KDOS's registry of loaded modules holds names and file identities and
compares no content. A cold-source container's CRC covers the bytes it
holds, which are the token form when it holds one. Akashic's application
builder digests the source it compiles, but it reads that source itself and
never asks the loader for it, and its loader takes machine-code images by
their digests. The sandbox's modules are its own artifacts, bound by their
digests, and are not Forth source.

The builder computes each file's stamp for the image it writes. The
recording boot gives the files and the order their loads began in. The
image gives `BOOT-ID`, from its own KDOS, and the text its own boot script
has. That text differs from the recording's in two lines: the one that
turns token loading on, and the last, which a simulator image hands to the
simulator. So "the stamp that stood when its recording began" is the stamp
that will stand at that point on the image the file is built into.

### What it costs

Emulator, `desktop-apt1`; [Measurements](#measurements) has the method.

| | Instructions |
|---|---:|
| `BOOT-ID`: the BIOS's code, once at the first `FSLOAD` | 90,082 |
| `BOOT-ID`: KDOS as the image holds it, 202,713 bytes | 278,749 |
| starting the stamp: the CRC-32 of the boot script, 9,309 bytes linked and 6,237 with the system volume | 172,000 and 115,000 |
| one token file: the comparison and the take | 324 |
| all 312 token files of the system-volume boot | 101,000 |

The comparison and the take are about 320 instructions for a file in KDOS's
Forth, where a handful was estimated, and about 100,000 for every file of
the system-volume boot. With `BOOT-ID` and the start, the whole stamp is
about 0.6 million instructions of a token boot's 676 million, under a thousandth.
The `BOOT-ID` hash is paid by every boot from disk, a source boot too. A
source file taken into a started stamp costs its CRC-32, 18.5
instructions a byte in KDOS's Forth loop over the CRC unit.

The check of a token file against its own source is another matter, and it
applies only where the source ships, on the System Practice's volume:

| System volume, to Desk's entry | Instructions | System cycles |
|---|---:|---:|
| from source | 1,442 M | 1,866 M |
| token files alone | 676 M | 899 M |
| token files with their source beside them | 1,230 M | 1,609 M |

The check reads every module's source to compare it, so the volume gives
back most of what token files save: 554 million instructions more than
the volume without source. About 177 million are the CRC-32 of the
9.5 MB of source, about 135 million the 310 further path resolutions, and
the rest, about 240 million, the reads of the source through the ext4
driver. A check against the file
system's own identity of the source, its size and time, which the module
source already reports for every file it resolves, would cost one more
path resolution for each module and no read; stage 1 measured 134 million
instructions for 301 such resolutions. It would need the token file to hold
that identity, which only whoever writes the source onto the volume knows.
That is a change to weigh when the System Practice's build is designed; it
is not made here.

### What follows from the rule

- **The boot script is the first file of every load.** An edit to
  `autoexec.f` makes every token file on the machine stale. On a volume
  without source the boot then ends at the first token file, until the
  image is built again. The builder writes the script, and nothing on such
  a volume should edit it.
- **A changed BIOS or KDOS** does the same, which is the rule's purpose.
- **A file loaded from inside an edited one counts as loaded after it.** An
  edited module takes with it the modules it is the first to require, as
  well as those loaded after it ends.
- **A text loaded with `LOAD` at the prompt, or by a loader outside KDOS,**
  enters the stamp once the stamp has started. A token file loaded after
  it is then stale, since it was not recorded on top of that text.

### What is uncertain

The rule counts the modules loaded after an edited one as stale. Whether
they need to be is not settled.

What a change to something loaded earlier can still make a replay get
wrong, with the file's own source unchanged, is the following and no more.
The replay searches every name at load time and takes each word's flags as
they are then, so it finds the definition the source would find and decides
to compile or to run it as the interpreter would.

- *A number that is now a word.* A token that converted as a number is
  stored as the number. If a word of that name exists now, the source would
  find the word. The reverse is not silent: a token recorded as a word that
  no longer exists fails to import, and the load ends with status 1.
- *`BASE`.* A number is stored as it converted under the `BASE` of the
  recording. If an earlier file now leaves another `BASE` behind, the
  source would convert the same digits to another value.
- *How much text a parsing word takes.* A word that now takes more, less
  or none of its line, or that starts or stops parsing, is handed the text
  it took when it was recorded.
- *A name that gains a newer definition between two of its uses.* The
  replay searches a name where the recording first met that binding and
  uses the slot from then on. If a changed defining word, or a file loaded
  in between, now defines the name where the recording saw no definition,
  the source would bind the later uses to the new word and the replay
  keeps the old one.
- *Which entry a defining word leaves newest.* A word the file defines is
  bound to the dictionary's newest entry after the record that completes
  its definition. A defining word changed to leave another entry newest
  binds another word.
- *A conditional* that goes the other way is checked: the replay ends with
  status 7.

*For.* The recording is a reading of the text on top of the words that
existed then. Every item above but the last is silent: the system that
results differs from the one a source load builds, and nothing in the
replay notices. The stamp is the only check that covers them, and it costs
a comparison.

*Against.* All of them are rare. Nearly every edit to an earlier module
leaves the later records exactly right, and the rule throws them away all
the same. The cost is source loads until a rebuild: an edit to a module
that loads early turns the System Practice's boot back into a source boot,
and an edit to the boot script stops a volume without source altogether.
The rule also misses the place where the items above are most likely to
bite, a module's own requirements, as
[What the stamp leaves out](#what-the-stamp-leaves-out) says. A rule that
looked only at the file's own source would keep every other token file in
use and would say plainly that it covers none of the list.

The rule can be narrowed in the one word that states it.

## Where the files live

Token files ship without their source on every volume. The one exception is
the System Practice's system volume, which carries the source beside them.

**The boot volume (MP64FS).** A linked chunk keeps its name and its
cold-source container and holds the token form in place of the source.
`networking.f`, `rich-terminal.f` and the cold-source loader hold their
token form under their own names; KDOS tells a token file by its first
eight bytes, whatever it is called. The directory therefore has the entries
it had, 86 of 128 for the linked Desktop and 66 with the system volume, and
the volume has more free space than before: 58,249 free sectors
against 51,022 for the linked Desktop, and 63,480 against 61,498 on the
boot volume of the system-volume build. The Desktop's free-space reserve is
not touched. The boot script, `autoexec.f`, is the one source file: the
builder writes it, and it is 9 KB.

**The system volume (ext4).** A module's token form stands at the module's
path with a `t` added: `utils/string.ft` for `utils/string.f`. A build with
`--token-modules` writes those files and no source. KDOS looks for a token
form only when `MODULE-TOKENS` is set, which the boot script does. The
variable is zero when KDOS starts, so a system without token files pays
nothing. With it set, the loader asks the module source for the path with a
`t` added first and for the path itself when that names no file. The file
it finds gives the module its identity in the registry of loaded files, so
a repeated `REQUIRE` still costs one resolution. The volume's size does not
change: the driver's geometry sets its minimum of 131 MiB, and it holds
3.2 MB of token files where it held 9.5 MB of source.

On such a volume a module that has no token form is absent. Nine modules
of the Desktop's volume are in that state for one reason
(`concurrency/event.f`, `semaphore.f` and `guard.f`, `text/utf8.f`,
`utils/uint-range.f`, `memory-span.f` and `bitset.f`, `math/crc.f`,
`utils/fs/vfs.f`): they are part of what mounts the volume, so the boot
loads them from the boot volume's chunks before the volume exists, and
their files on the volume were never loaded. Other modules still name them
by path. With the source gone, such a `REQUIRE` printed ` Module not found:`
and the path and went on; the words were there, so the load succeeded under
a false message, once for each module that names one of the nine. The
builder now writes a file at each of those paths that only names its
module: a header with the module's `PROVIDED` name and no records. `REQUIRE`
reads it, finds the module loaded and reads no further, as it does for any
loaded module's file. If the module is not loaded, the load ends:
` Token file`, the name and ` only names its module, which is not loaded`.
A module that has no token form for any other reason has no file, and the
build's report lists it; the Desktop has none.

**With the source kept (`--keep-module-source`).** The builder writes every
module's source at its path as well and sets flag 1 in each token file. This
is the System Practice's volume. KDOS then checks each token file against
its source and loads the source of one that is out of date, as the next
section says. A module without a token form simply loads from its source.

The module-source contract changes in two respects. The loader may call
RESOLVE twice to find a file, and a RESOLVE that returns `MS-E-NOT-FOUND`
must leave the source's current directory as it was; Akashic's VFS source
already did, and KDOS's own MP64FS source now restores the directory it
walked from. And after it has read a token file that says its source ships
beside it, the loader calls RESOLVE and READ once more, for the file's own
name without its last letter, in the folder the first RESOLVE made
current.

## What cannot take the form

A file has no token form when

- its load takes entries out of the dictionary (`DICT-ROLLBACK`, `LATEST!`),
  which a replay cannot survive;
- it was not loaded as a file in the recorded boot;
- it uses more than 61,440 distinct words; or
- its load does not finish.

The recorder marks the first and the last. On the boot volume the builder
leaves such a file as source, and KDOS takes its text into the stamp like
any other file's. On a system volume without source it is absent, and with
the source kept it loads from source. No file of the Desktop was marked
unfit.

A build also refuses two shapes of boot, because the stamp could not follow
them: a source file that ends its load before the first token file begins,
which KDOS's stamp would leave out, and one source text loaded twice, which
would need two stamps in one file.

## The simulator

`TOKEN-REPLAY` is the simulator's 460th core word and `BOOT-ID` its 461st.
The replay runs in Python on the runtime's reference path and gives each
record to the same two methods a source token reaches. Hybrid mode runs
Forth as the simulator does and loads the same files; its routine words
exist before the image boots, and a token file finds them by name, as
source does.

The source-acceleration overlays and their pins are untouched, because no
word they bind changed: the loader announces a source file from a word of
its own and then calls the walker the overlay binds. They serve source
loads as before, and a token load does not pass through them.

Host time is not device time. In the simulator the linked Desktop
reached Desk's entry in 2.6 and 2.3 seconds from source and in 3.7 and 3.4
from token forms; with the system volume it took 10.6 and 9.8 seconds from
source and 12.8 and 13.2 from token files alone. The guest does less work
from token forms, 56.9 million semantic steps against 69.4 million with the
system volume, but the replay is Python where the source path has a native
line walker, and that costs more host time than the smaller load saves. A native replay is among
[what remains](#what-remains).

### Section 9 of the simulator contract

Section 9, "Native and warm images", used to reserve "a future semantic
warm artifact" with its own magic, version and extension and a cache key
that includes the simulator and runtime version. That was a cache private
to the simulator, to stand beside machine-code images for the machine. The
token form replaces it with one file for every backend, and the section now
reads:

> MF64 v1/v2 images contain native dictionary bytes, relocations, imported
> execution tokens, and executable entries. The simulator may read, copy,
> hash, and persist MF64 files as opaque data, but it does not execute them
> or claim to validate their native semantics.
>
> A module may also be loaded from its token form (`MFTOKEN1`), a record of
> the interpreter's work on the module's source, which `TOKEN-REPLAY` runs
> through the interpreter's own steps. The same file loads on every
> backend. It holds no state of any backend, so no simulator or runtime
> version enters its validity; its header's stamp ties it to the BIOS, the
> KDOS and the files it was recorded on. It cannot become a production
> path before cold, warm, and source/warm equivalence all pass and useful
> wall time is demonstrated.

The gate sentence stays as it is.

## Equivalence

Akashic's `local_testing/test_token_equivalence.py` is the proof that a
replay gives what the source gives, and its own text says what makes it
fail. For the linked Desktop and for the Desktop on its system volume it
builds one image from source and one with `--token-modules`, boots both on
the emulator until everything is loaded and the machine waits at the
prompt, and compares what the two loads built. The images are the Desktop's
with one line added to the boot script, which hides the session entry with
a word that prints a mark and returns. The system-volume pair's token image
is the one that ships, a volume of token files and no source.

In both pairs the dictionary's structure is equal. Every entry is at the
same address with the same name and flags, 50,506 entries in the linked
boot and 54,598 with the system volume. `HERE`, `LATEST` and the end of the
system dictionary are equal, and the terminal output of the two boots is
identical.

The bytes are not all equal. The test puts every byte that differs to the
word that holds it and into one of three classes, and fails on a byte that
fits none.

| | Linked | System volume |
|---|---:|---:|
| dictionary bytes compared | 8,926,044 | 9,979,792 |
| bytes that differ | 2,638 | 2,381 |
| in a cell that holds an address the load allocated | 582, in 162 cells | 977, in 324 cells |
| in the data of a listed word of the loader, the allocator or a filesystem | 2,054, in 31 words | 1,402, in 85 words |
| in the data of a word set from a clock (`DNS-ID`) | 2 | 2 |
| unexplained | 0 | 0 |

The count of state bytes moves a little from run to run: the boot volume's
directory holds each file's time, and the two images of a pair are built
seconds apart. The other two classes do not move.

**Addresses.** The loader's buffer for a token file is smaller than for
its source, so memory that a module allocates while it loads lies at
another address, and so does everything allocated after it. A cell that
holds such an address holds another value: a variable's cell, a CONSTANT's
value, or a literal compiled into a definition. The test accepts a
difference there only when the cell's value in each boot is an address that
boot's load allocated: it lies in the external memory the allocator had
handed out when the boot reached the prompt, below the userland dictionary
or between its limit and `XMEM-HERE`. A value anywhere else fails.

**State.** The loader, the module registry, the allocator, MP64FS, the
cold-source loader, the VFS and the ext4 driver keep cursors, buffers,
sizes and checksums of the files and the volume they read, and the two
boots read other files from other volumes. The test holds a list of those
words, 61 entries, each with the pairs it differs in and one line that
says why its data follows the files that were read. An entry is a word, or
the prefix of one routine's private cells. A difference is accepted only
in the data field of a `VARIABLE` or `CREATE`d word on the list, and the
data field is found from the word's own code, so a byte that differs in a
colon definition, in a name or in a link fails whatever the word is called.
The list is closed in both directions: an entry that explains no byte in a
pair it is listed for fails the test, so an entry cannot outlive its
reason.

**The clock.** `DNS-ID` takes its value from the timer when networking
loads, and the two boots reach that line at different times.

No byte of any definition's code differs except an address of the first
class.

The rules themselves are tested apart from the boots, on made-up
dictionaries of three words with one difference planted in each: an
address outside what was allocated, a state word that is not listed or is
listed for the other pair, a listed name on a colon definition's code, a
listed word that explains nothing, and a structure or an output that
differs.

The reports of the run of record are kept in Akashic's
`local_testing/evidence/token-equivalence-20261111.md`, with the result
lines of the journeys below. The test is four
builds and four emulator boots, about four minutes and 1.7 GiB at its
peak, and runs alone.

### The journeys

The physical Desktop journey, built with `--token-modules`, passed three
ways at MegaPad 3f2a162b and Akashic af437d6a: the linked Desktop in the
simulator, the Desktop on a system volume of token files alone in the
simulator, and the linked Desktop in hybrid mode.

### Would taking the source's working space make the dictionaries identical?

No. A token load that allocated what its source load allocates would put
every later allocation at the source boot's address, and the first class
would be empty: 582 bytes in the linked boot and 977 with the system
volume. The other two classes would stay. The loader's cursors, the
filesystems' caches and the checksums describe the files that were read,
which are other files, and the clock is the clock. The dictionaries would
still differ in 2,056 and 1,404 bytes, less the few cells among them that
only hold the allocator's own marks.

What it would cost, from the recorded boots:

- The loader's buffer for each file would be its source's. At the deepest
  moment of the load those buffers hold 558,410 bytes in the linked boot,
  where the token files and their tables hold 239,592, and 1,138,529
  against 393,979 with the system volume, where loads nest eleven deep. The
  memory is given back as each load ends.
- The table of found words would have to lie in the buffer's slack, since
  an allocation of its own would move whatever the load allocates next. It
  fits in every one of the Desktop's files, 55 in the linked boot and 312
  with the system volume.
- The loader would have to know the size of the source's buffer. The header
  holds the source's length; the buffer is that length as the volume rounds
  it, which a container's header can state and a file found by name on
  another volume cannot. The cold-source loader's own two buffers would
  have to follow.
- It costs no instructions.

It was not built.

## Measurements

All figures are from one round on MegaPad `token-modules` efca9db4 and the
Akashic builder that became `token-modules` 1f2c7718, run one boot at a
time on a machine that other work shared. The branches stand on MegaPad's
local main 8bbfedc2 and Akashic's 9980e390. Device figures are the
emulator's, for `desktop-apt1` from reset to the boot line that says the
guest is entering Desk, in millions of instructions read between boot lines
at a resolution of two million.

| Linked chunks | Source | Token forms |
|---|---:|---:|
| BIOS and KDOS | 28 | 26 |
| networking, rich terminal, cold-source loader | 86 | 32 |
| the Desktop's modules, 52 chunks | 912 | 370 |
| the rest | 2 | 2 |
| total | 1,028 | 430 |
| system cycles | 1,335 | 584 |

| System volume, 4 KiB blocks | Source | Token files alone | With source beside |
|---|---:|---:|---:|
| BIOS and KDOS | 26 | 26 | 26 |
| networking, rich terminal, cold-source loader | 88 | 32 | 32 |
| the volume's driver, 8 chunks | 136 | 52 | 52 |
| the mount | 14 | 12 | 12 |
| the Desktop's modules, 310 files | 1,176 | 552 | 1,106 |
| the rest | 2 | 2 | 2 |
| total | 1,442 | 676 | 1,230 |
| system cycles | 1,866 | 899 | 1,609 |
| sectors read from the system volume | 95,898 | 71,018 | 179,642 |

At the SoC's 100 MHz the linked boot goes from 13.4 seconds to 5.8, and the
boot from the system volume from 18.7 to 9.0.

The volume of token files alone loads its modules in 552 million
instructions where stage 1's volume, which kept the source, took 640
million: the nine modules loaded before the mount are now one small file
each to read, where the loader read and scanned their whole source.

### Where the instructions go

The guest's program counter was sampled every 1,009 instructions from the
BIOS's banner to Desk's entry and put to the BIOS label or Forth word that
held it. In millions of instructions, for the three token boots:

| | Linked | System volume | With source beside |
|---|---:|---:|---:|
| the token replay's loop and the interpreter's two steps | 65 | 75 | 74 |
| splitting the text of parsing words | 10 | 11 | 11 |
| dictionary searches | 28 | 29 | 29 |
| compiling: the scan of the inline table | 82 | 94 | 94 |
| compiling: the rest | 123 | 138 | 138 |
| checksums: the CRC words and the BIOS's CRC primitives | 38 | 40 | 198 |
| ext4, the VFS and the disk | 1 | 59 | 153 |
| calls between Forth words (NEXT and EXIT) | 19 | 99 | 252 |
| other BIOS primitives that Forth words run | 29 | 78 | 202 |
| other Forth words | 8 | 28 | 52 |
| sampled | 402 | 649 | 1,202 |

The replay, the searches and the compiling take the same in all three. The
last five rows are what the modules and the filesystems run. With the
source beside the token files they grow by 551 million instructions, which
is the check of each token file against its source: finding the source,
reading it, and computing its CRC-32. The rows cannot split that exactly,
because KDOS's checksum loop and the ext4 driver are both Forth words
calling BIOS primitives. Timed apart, below, the CRC-32 is 18.5
instructions a byte, 177 million for the 9.5 MB of source. Stage 1 measured
a further path resolution at 0.44 million instructions, 135 million for
310. The remaining 240 million or so are the reads of the source through
the driver, 108,624 sectors. The CRC unit takes eight bytes in one
instruction; KDOS's loop around it costs the rest.

For a source boot, `docs/performance/loader-line-cost-2026-11-11.md` has
the same table of MegaPad's main.

### The stamp

The words of the staleness rule were timed apart from a boot. A boot
volume's script ran a million passes of the per-file check and take
against a million of an empty loop, a thousand starts of the stamp, and a
hundred CRC-32s of 100,000 bytes, between boot lines stamped at a
resolution of 50,000 instructions.

| | Instructions |
|---|---:|
| the BIOS's hash of its own code, 65,490 bytes | 90,082 |
| its hash of KDOS as the Desktop's images hold it, 202,713 bytes | 278,749 |
| the check and the take for one token file | 324 |
| the CRC-32 of a source text, and so the start of the stamp for each byte of the boot script | 18.5 a byte |

The BIOS's hash is eleven instructions for a cell. The per-file figure is
a pass of 358 instructions less the empty loop's 34. In the three profiles
the stamp's own words were hit 3, 21 and 47 times in 398,848, 643,584 and
1,191,680 samples, which agrees with it.

### Sizes

| | Files | Source | Token forms | |
|---|---:|---:|---:|---:|
| Linked boot: 52 chunks, `networking.f`, `rich-terminal.f`, the cold-source loader | 55 | 7,107,018 | 3,408,153 | 48% |
| of which the 52 chunks, in their containers | 52 | 6,320,982 | 3,114,573 | 49% |
| System volume: modules, as plain files with their comments | 301 of 310 | 9,544,132 | 3,168,937 | 33% |

The system volume also holds nine files that only name a module, 744 bytes
together. With `--keep-module-source` it holds 611 files and 12,713,069
bytes. The boot volume has 58,249 free sectors where the source build has
51,022, and 63,480 against 61,498 for the boot volume of the
system-volume build.

In stage 1's count the 52 chunks held 1,054,713 records: 685,542 words by
slot (44% of the bytes), 51,006 texts (30%), 23,686 IMPORTs with their
names (12%), 73,340 numbers (6%), 185,260 line marks (6%), 35,745 DEFINEDs
and 134 SKIPPEDs. Their tables, allocated one file at a time while it
loads, came to 59,431 slots, 475 KB in all and about 9 KB for a chunk.

### The build

A linked Desktop with `--token-modules` built in 32 seconds and one with
the system volume in 54, with or without the source kept, against two
seconds for the source builds. Nearly all of it is the recording boot,
which evaluates every line on the simulator's reference path.

### The simulator's host time

Host seconds from the start of `kdos.f` to the line that says the guest is
entering Desk, two runs each, native executor. These say nothing about a
device.

| | Source | Token forms | With source beside |
|---|---:|---:|---:|
| Linked chunks | 2.6, 2.3 | 3.7, 3.4 | |
| System volume | 10.6, 9.8 | 12.8, 13.2 | 22.6, 23.4 |

The simulator's semantic steps for the system-volume boot are 69.4 million
from source, 56.9 million from token files alone and 132.5 million with the
source beside them, and its reads of the system volume 11,988, 8,878 and
22,456.

## Decisions

Three decisions were the author's. They are recorded here as taken, each
with what it rests on.

### 1. Where token files are built

On the host, when an image is built, for now. The recorder runs in the
simulator, which records a boot in under a minute and whose replay matches
the BIOS's by test. The System Practice must also build token files on the
machine before the release; that recorder is a committed later step, and
[Who writes the file](#who-writes-the-file) says what keeps the format and
the loader fit for it. None of it is built yet.

### 2. Whether source ships beside the token forms

Token files ship without their source on every volume. The System
Practice's system volume is the one exception and carries the source
beside them. The builder's default for a system volume with token modules
is therefore token files alone, and `--keep-module-source` writes the
source as well. With no source there is nothing to fall back to, so a file
that cannot be loaded ends the load, named.

Nothing in the Desktop or in its tests opens a module's source from the
system volume. Akashic's Forth names the volume only in
`utils/fs/system-volume.f`, which mounts it and fills the module source;
every read of it goes through KDOS's `LOAD` and `REQUIRE`. File Explorer
lists the boot volume, where the sample documents and `example.f` live.
The tests that read module source back from a system volume do so on a
volume built without `--token-modules`, where the source is the only form.

### 3. What makes a token file stale

The rule under [The stamp](#the-stamp-and-when-a-token-file-is-stale),
taken for now, with what is uncertain about it stated there: whether the
modules loaded after an edited one need to count as stale.

## What remains

**Before the branches merge.**

- The author's reading of this document, and of three findings in it that
  bear on the decisions: an edit to the boot script makes every token file
  stale; the stamp does not cover the modules a module requires itself;
  and the check of a token file against its source costs most of what the
  token files save on the System Practice's volume.
- The merge itself. Both branches stand on the local mains named under
  [Measurements](#measurements) and are not merged.

**Committed later steps.**

- The recorder on the machine, for the System Practice (decision 1).
- The System Practice's build, which is what uses `--keep-module-source`.
  When it is designed, the own-source check should be weighed again: by
  content it reads every source, and a check by the file system's identity
  of the source would not.

**Open, and not needed for the form to land.**

- A native replay for the simulator. The replay runs in Python where the
  source path has a native line walker, so a token boot takes more host
  time in the simulator than a source boot, though it is the smaller load.
- A BIOS word that feeds a buffer to the CRC unit. KDOS's loop over the
  unit costs 18.5 instructions a byte, where the unit takes eight bytes in
  one instruction. The cold-source containers'
  checksums, the ext4 driver's and the own-source check all pay it.
- Token forms for applets a machine builds itself. Akashic's application
  builder writes MF64 images, which the simulator and hybrid mode cannot
  run; the token form could serve there, with the recorder on the machine.
