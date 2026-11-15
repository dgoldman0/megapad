# Native dictionary and source evaluator

Status: step 1b of the [simulator speed plan](simulator-speed-plan.md),
done on 2026-10-13 in three slices: the native dictionary, the native line
walker, and plans that survive publication. A fourth slice, decided on
2026-11-11, gives the walker a second token source, the records of a
module's token form, and a tap for the token recorder. Its first stage, the
replay, is built, and [Token records](#token-records) holds its design. Its
second, the tap, is drafted in [The recorder's tap](#the-recorders-tap) and
has not yet passed its gates.

The hosted simulator compiles about 210,000 lines of KDOS and Akashic source
at every boot. After step 1a this takes about 27 s, and almost all of it is
Python. This document moves the dictionary and the common source path into
`_megaforth_native`, while the Python dictionary and evaluator stay as the
exact reference.

## Where the 27 s goes

Measured on 2026-10-13 by sampling the main thread's Python stack every
millisecond through one `desktop-apt1` boot compile (MegaPad `5e68e61f`,
Akashic `2c315526`). Each sample counts toward the innermost activity on its
stack.

| Activity | Share |
| --- | --- |
| Line walker overlays, mostly their guest cell reads and writes | 20% |
| Guest words executed from Python (dispatch, guards, primitives) | 19% |
| Tokenizing | 16% |
| Entering `EVALUATE` for each line | 9% |
| Token dispatch and compiling | 6% |
| Return-stack snapshots in the dispatch guard | 5% |
| Native executor intervals, including settlement | 5% |
| Line prescan (`_line_cannot_execute_guest`) | 5% |
| Publishing definitions and the side index | 4% |
| Name lookup | 3% |
| Line-length scan | 3% |
| Native plan building | 2% |
| Tile words in Python | 2% |

About 195,000 lines pass the checked-line prescan and take the fast path.
About 6,000 do not and run the complete KDOS `EVALUATE-CHECKED` body under
`CATCH`. Of the 866,000 tokens, about 525,000 are compiled calls, 189,000
compile-time directives and 61,000 compiled numbers. Only about 43,000 run a
word while interpreting, mostly `VARIABLE`, `CONSTANT`, `,`, `C,` and
`CREATE`.

So most of the cost is per-line and per-token bookkeeping on lines that run
no guest code at all. That work can move to C++ without changing what the
guest sees.

## Ownership

| | Python backend (reference) | Native backend |
| --- | --- | --- |
| Dictionary metadata | `simulator.dictionary.Dictionary` | `NativeDictionary` in `_megaforth_native` |
| Side index (`DICT-INDEX!`) | `HostedDictionaryIndex` | native, same slot layout and order |
| Admitted source lines | Python evaluator | native line walker |
| Other source lines | Python evaluator | Python evaluator, through the native dictionary |
| Record lines of a token stream | the replay loop in `runtime.py` | native line walker for the lines it can take, the replay loop for the rest |
| Execution | Python dispatcher | native executor, Python for stops |

`make test-simulator` keeps running the Python backend without building any
extension. The native backend never keeps a second copy of dictionary state:
Python reads and changes it only through the native interface. This is a host
metadata store for semantic IR; it emits no MP64 code and is unrelated to the
machine-code stages in
[hybrid-native-dictionary-plan.md](hybrid-native-dictionary-plan.md).

## Native dictionary

Each record holds a word's exact name, header address, XT, immediacy and kind:

- primitive: the Python `PrimitiveDefinition`, kept as a Python reference;
- colon: its IR in native form when the walker compiled it (slice 2), or
  the Python `ColonDefinition` when Python did;
- constant: its value;
- value: nothing beyond the body cell;
- created: its `DOES>` action, as a source XT and entry IP;
- directive: its `DirectiveKind`;
- routine: the Python `RoutineDefinition`, kept as a Python reference.

The native code also owns the bindings (case-insensitive, newest first), the
XT map, `HERE`, the active zone, the header limit, the numeric rollback floor,
body allocations and the execution generation. It writes headers into guest
memory exactly as `Dictionary.define` does. Every mutation keeps the Python
class's checks, order and exception types and messages, including checkpoint
seals, `rollback_to` link verification, `set_latest`, overlap checks and body
lease revocation.

A Python facade in `simulator/native_dictionary.py` implements the full
`Dictionary` interface over the native object, so the runtime, core words,
overlays, hybrid runtime and tests use either implementation unchanged:

- **Word identity.** A `Word` view is created the first time Python asks for a
  record and is cached with it, so one live definition always has one `Word`
  object. A record removed by rollback drops its view; a later record at the
  same XT gets a new one. Identity-bound overlays and leases keep working.
- **Lazy IR (slice 2).** A natively compiled colon view's `implementation`
  is a `ColonDefinition` whose `operations` tuple is built from the native IR
  on first access. Type checks do not build it.
- **Created actions.** A created word's view carries a
  `NativeCreatedDefinition`, a `CreatedDefinition` whose `action` setter also
  updates the native record, so `DOES>` keeps assigning the action as before.
- **Kinds, not classes.** The loader overlays pin exact KDOS bodies by a
  digest that names each called word's implementation. It names the kind,
  such as `CreatedDefinition`, rather than the class, so the same bodies
  match under either dictionary.
- **Fast resolution.** The facade keeps a dictionary of live views by XT.
  Every removal goes through the facade, which drops the removed XTs, so
  `resolve` stays a Python dictionary lookup for the dispatcher.
- **Leases.** `BodyAllocationLease` objects are created on demand and checked
  against the native allocation serial.
- **Private access.** `native_execution.py` and `hybrid/runtime.py` no
  longer read `Dictionary._by_xt`; both dictionaries provide `live_word(xt)`.

The side index moves into the same native object: `DICT-INDEX!`,
`DICT-INDEX@`, notification, publish and rebuild keep the BIOS slot layout,
hash, probe order and flag transitions of `HostedDictionaryIndex`.

The guest memory views are shared with `NativeProgram`. A write to a missing
sparse page creates it in the region's page dictionary, as the Python write
path does.

## Native line walker

The `SOURCE-EVALUATE-CHECKED` and `_LD-WALK` overlays call the native walker
instead of looping in Python. The walker runs the overlay's loop: line
counting, `EVAL-LINE`, the LF scan, CR stripping, and the walker's guest cells.
For each line it either evaluates the line itself or returns to Python with
the line measured and not yet evaluated. Python then evaluates that line on
its existing path and calls the walker again for the rest. When the walker
cannot even measure a line natively, because its span is not inside one
ordinary region, it returns before writing anything and Python runs that
whole iteration. `EVALUATE-FINISH` stays in Python.

Hand-back is per line, not per token. The checked-line overlay decides per
line whether the line takes the fast path or the complete KDOS `CATCH` path,
and those differ in steps and stack traffic. A line that needs Python
therefore goes to Python whole.

### Which lines the walker evaluates

A line is evaluated natively only when both hold:

1. The existing checked-line rule admits it. The native code applies
   `_line_cannot_execute_guest` and the overlay's `applicable` checks exactly,
   including the initial-primitive identities, the growth preflights, the
   1,024-byte stack reserve for defining words and the comment-only tail
   after a definition.
2. No native-only condition rejects it. These cover cases where the Python
   fast path would fail or reach Python-owned state:
   - a token that is neither a word nor a number in the current `BASE`;
   - a compile error, such as an unmatched control word, `TO` without a
     `VALUE`, or an embedded NUL in a compiled `S"`;
   - a data-stack underflow or overflow, or division by zero, in an
     interpreted primitive;
   - a `VALUE` read that overlaps a stack cell the line has written;
   - interpreted `."`, or the BIOS `PROVIDED` directive;
   - an open interpret-mode `IF` or `[IF]` skip;
   - an open `:NONAME` definition, which the walker never holds;
   - an armed index notification that this line could make due;
   - a nonzero status at nonzero evaluator depth, or depth 16;
   - a step budget that the line could exceed;
   - a definition that would shadow a token in its own comment tail.

Every natively evaluated line is therefore one Python would also evaluate on
the fast path, and every other line still meets the Python rule.

### What a native line does

The walker reproduces every guest-visible effect of the Python fast path:

- the walker cells and `EVAL-LINE`;
- `EVAL-STATUS`, `EVAL-COLUMN` and `EVAL-THROW` cleared at depth zero, and
  `EVAL-DEPTH` written up and back down;
- the data-stack writes of the line address and length, the status push, and,
  for `SOURCE-EVALUATE-CHECKED`, its `DUP`;
- one root continuation for `EVALUATE-CHECKED`, and for `_LD-WALK` one for
  `_LD-STATUS-THROW`, each with its exact cookie written to the return-stack
  slot;
- steps: one for the `EVALUATE-CHECKED` overlay, one for `EVALUATE`, one for
  the `_LD-STATUS-THROW` overlay under `_LD-WALK`, and one per interpreted
  word.

Python settles steps, diagnostics and the timer with `account_work_many` and
`advance_by` before anything else can observe them, as it does after a native
execution interval. The timer latches the same state either way.

Within the line the walker handles numbers, comments, compiled calls, `:` and
`;`, the compile-time control words, `S"`, `."`, `ABORT"`, `[']`, `[CHAR]`
and `TO` while compiling, constants, values and plain created words, the
admitted stack and arithmetic primitives, and `CONSTANT`, `VARIABLE` and
`VALUE`. It compiles exactly the IR `_apply_directive` builds, with the same
branch patching, and publishes at `;` and at defining words through the
native dictionary and index.

### Compiler state

The open definition persists across lines. When the walker starts it takes the
evaluator's current compiler, if any, and when it returns it gives back the
compiler it holds, so exactly one side owns it at any time. The hand-over
copies the name, location, mode, operations, control frames and literal pool.
It happens only when the walker starts or returns, so it costs nothing per
line.

### Exactness

Native and reference boots must agree on all guest memory, both stack
pointers, the continuation cookie counter, semantic steps, `HERE`, `LATEST`,
every definition with its IR, the side index, the evaluator cells and UART
output. Host-only counters may differ: dispatch root identifiers, and
continuation metadata for return-stack slots that are no longer live.

## Token records

Decided on 2026-11-11. A module's token form is the record of what the
interpreter did with its source
([token-modules-proposal-2026-11-11.md](token-modules-proposal-2026-11-11.md)),
and `TOKEN-REPLAY` runs the records through the interpreter's two steps.
Two things made the form slow on the host. The replay ran in Python, where
source has the walker, so a Desktop boot from token forms took more host
time than a boot from source although the guest does less work. And a token
build recorded a whole boot with every source line on the reference path,
which took most of a minute where a source build takes two seconds.

Two stages removed them. Stage 1 gives the walker a second token source, so
that it replays records. Stage 2 gives the walker a tap, so that a
recording boot keeps the walker; [The recorder's tap](#the-recorders-tap)
holds its design. A token build now takes about 10 seconds linked and 23
with the system volume, and the default Desktop build stays linked source
until a token build and a token boot cost the tests no more host time than
source does.

### One disposal, two token sources

What the interpreter does with a token it has found is stated once in the
walker, as it is stated once in `runtime.py` (`_evaluate_found_word`,
`_evaluate_found_number`) and once in the BIOS (`interp_found_word`,
`interp_found_number`):

- a directive is applied;
- a word met while compiling is appended as a call, unless it is immediate;
- any other word runs, if it is one the walker may run;
- a number is compiled as a literal or pushed.

In `line_walker.cpp` that is `dispose_word` and `dispose_number`, over
`apply_directive` and `execute`. Two token sources feed it, and it is not
told which of them a token came from.

| | Source text (`dry_run`) | Token records (`dry_run_records`) |
| --- | --- | --- |
| A word | the token, looked up by name | a slot number: the word an IMPORT or DEFINED record bound to that slot. An IMPORT looks its name up, binds the next slot and gives the word |
| A number | the token, converted in the current `BASE` | the value the record holds |
| The input of a parsing word | the rest of the line | the TEXT record before the word, and nothing where there is none |
| A line | the bytes up to the next LF | a LINE or LINES record and the records after it, up to the next LINE, LINES or END |

A parsing word reads a `Cursor` in both cases, over the line or over the
TEXT record's bytes. As in the replay loop, the input a TEXT record gives
lasts until a word runs.

### A record line

The unit of a dry run and of a hand-back stays the source line. The walker
evaluates a record line as it evaluates a source line: as a dry run on the
virtual data stack and the undo log of the open definition, with every
write held back until the dry run has succeeded. A record line holds back
two writes that a source line does not have: the new `EVAL-LINE`, and the
table cell of each slot the line binds. A line the walker rejects leaves no
trace, and Python's loop takes it from its LINE record.

The walker is entered at a record and runs line after line until it reaches
one it cannot take, whose place it returns. The loop runs from there to the
next LINE or LINES record and calls the walker again. Records that no LINE
record precedes, at the start of a stream, are one line for this purpose.

### Which record lines the walker evaluates

A source line has the checked-line rule because Python evaluates an
admitted line and any other line on two paths that differ in steps and
stack traffic. Records have one Python path, the replay loop, so a record
line needs no such rule: the walker evaluates every record line whose dry
run succeeds. The dry run fails, and the line goes to the loop, when

- a record is cut short or has an opcode the walker does not take, which
  includes SKIPPED and any opcode it does not know;
- a slot number names a slot that is not bound, or the word the line
  itself defines, which is not in the dictionary until the line commits;
- an IMPORT's name is not defined, or a binding would pass the end of the
  table or fall outside ordinary memory;
- the disposal rejects a token, by the native-only conditions listed above
  for source lines: a word that runs guest code, an interpreted `."`,
  `PROVIDED`, an unmatched control word, a stack underflow, a growth
  preflight that fails, and the rest;
- anything other than a comment word or a DEFINED record follows the
  definition the line completes. The walker publishes a line's definition
  when the line commits, so a later token that could find it, run it or
  define another is Python's. The checked-line rule makes the same demand
  of a source line, with its comment-only tail;
- a TEXT record's input is still unread when the line ends, since only the
  loop can carry it into the next line;
- `EVAL-LINE` or a table cell the line writes lies among the data-stack
  cells the line used, a table cell lies in the definition the line
  publishes, or the line reads a `VALUE` whose cell is one of them. The
  reference wrote `EVAL-LINE` before the line's words ran and each table
  cell as it was bound. The walker writes them when the line commits, which
  comes to the same wherever they do not overlap;
- the line could exceed the step budget.

The walker is not offered a line at all while a temporary or anonymous
definition is open, while a conditional skip is open, while the input of a
TEXT record is unread, or while the stacks, clocks or step meter are not
the ones its settlement assumes. The loop runs those lines.

### What a native record line does

- `EVAL-LINE` takes the line's number.
- Each slot the line binds gets its table cell, the header address of the
  word found, in slot order.
- The data stack takes the line's pushes, and the definition the line
  completes is published through the native dictionary and index, as for a
  source line.
- One step is counted for each word that ran.

It touches nothing else. `TOKEN-REPLAY` enters the BIOS evaluator once for
the whole stream and leaves it once, in Python, so a record line has no
`EVAL-DEPTH` traffic, no root continuation and no cookie.

### The replay's failures have one owner

`runtime.py`'s loop, `_run_token_records`, is the replay for both backends.
It alone ends one: at END, with status 1 for a name that is not defined,
with status 6 for a malformed stream and with status 7 for a stream that
does not match this system. The walker's record source decodes only to find
what it can take. What it cannot decode or bind it does not judge: the line
goes back to the loop, which decodes it again, does what the records before
the fault ask, and reports.

- **Slot bounds and the table's size.** The walker checks them to stay
  inside the table. A violation is a rejected line, and the status is the
  loop's.
- **A skip that differs from the record.** Every word that begins a
  conditional skip is one the disposal rejects, so skips begin and end only
  in lines the loop runs, where it compares them with the SKIPPED records.
- **The dictionary's unlink count.** Only a word that runs guest code can
  take entries out of the dictionary. Such a word runs only in a line of
  the loop's, and the loop compares the count after each.
- **END.** The walker stops before it.

The loop keeps its place, the next record and the source line reached, in a
`_TokenStream`, and runs either to the stream's end, for the reference
backend, or to the next line record, for the native one.

The words bound to a stream's slots are held once as well: in a Python list
for the reference backend, and in the native stream object for the native
one, where the loop reads and binds them through a view shaped like that
list. A word the walker bound is looked up by its XT when the loop first
uses its slot. It is still the live definition then, because the walker
binds no slot while a temporary definition is open, and every other removal
from the dictionary ends the replay.

The record decoder itself exists twice, in the loop and in the walker's
record source, as the tokenizer of source text exists in `SourceCursor` and
in the walker's `Cursor`. That is what a second token source is. What the
decoded tokens mean, and what a stream that cannot be decoded means, each
exist once.

### Exactness of a native replay

A replay through the walker and a replay by the loop alone must agree as a
native and a reference boot must agree: on all guest memory, both stack
pointers, the cookie counter, semantic steps, `HERE`, `LATEST`, every
definition with its IR, the side index, the evaluator cells and UART
output. Host-only state may differ, as before.

## The recorder's tap

Stage 2 of the slice above. A token build boots the image once from source
with a token recorder attached (`simulator/token_recorder.py`), and the
recorder writes each loaded file's token form from what the interpreter did
with the file's tokens. Before this stage only the reference evaluator could
tell it. The recorder's taps were in `_evaluate_token`, and its hooks took
the place of the two overlays that call the walker, so a recording boot
evaluated every source line in Python. The walker now has the same taps, and
a recording boot keeps the walker for the lines it admits.

### What the recorder is told

The recorder learns six things. The reference evaluator tells it all six
for the lines it runs. The walker tells it five for each source line it
evaluates:

1. **The line**: its address, its length and the number `EVAL-LINE` holds,
   by which the recorder finds the file the line belongs to and marks the
   line.
2. **A word found**, by its XT, with the token it was written as. The XT
   is how the record writer tells one binding of a name from another: the
   first record of an XT becomes an IMPORT and the later ones its slot.
3. **The input a word took** when it ran, which is the bytes its cursor
   moved over. A word compiled as a call ran nothing and took none.
4. **A number converted**, by its value.
5. **A definition completed**: the XT of the word the line publishes,
   reported after the word that completed it.

The sixth, where a conditional skip ended, stays with Python, since skips
begin and end only in lines the walker hands back.

### One recorder, two tellers

A report states what the disposal did with a token and judges nothing. What
becomes of a report is decided where it was decided before, once:

| | Stated in |
| --- | --- |
| What the interpreter does with a token | the disposal: `dispose_word` and `dispose_number` in the walker, `_evaluate_found_word` and `_evaluate_found_number` in `runtime.py` |
| What happened to a token, as a report | a tap around each disposal: in the walker's `dry_run`, and in `RecordedUnit.found_word` and `found_number` |
| A file's items: the line marks, a word with its input, a comment's input dropped | `RecordedUnit`, through the same methods for both tellers |
| Which definitions take a slot, and the module's name | `RecordedUnit.token_file` |
| The records and the file | `shared/token_form.py` |

The tap exists twice as the token source does: each evaluator reads its own
cursor before and after its own disposal. What the two read goes into one
record. The walker says of `(` that the word is the comment directive and
which bytes its cursor passed, and the recorder leaves those bytes out, as
it does when the reference evaluator reports the same word. `RecordedUnit`
stays the one recorder and `shared/token_form.py` the one writer of the
format, and a file's lines may come from either teller in any order.

### Reports are held back with the line

The tap sits in `dry_run`, which has the token, the cursor before and
after the disposal, `dispose_word`'s answer on whether the word ran, and
the pending definition. A line's reports are among the things its dry run
holds back. The snapshot marks where they begin and `restore` drops them,
so a rejected line reports nothing, and the reference evaluator records
that line as it runs it. The commit adds the line, with the number it reads
from `EVAL-LINE`, and fills in the XT of the line's definition, which the
word has only once the commit has published it.

A walk returns its reports with its result, one entry for each line it
evaluated, in order. A walk that ends in an exception leaves them in its
error state, so the lines committed before the fault are reported as they
are settled. `native_walker.py` gives a walk's reports to the recorder
before the overlay evaluates the line handed back. No guest code runs
inside a walk, so nothing reaches the recorder between a native line's
commit and that hand-over, and the recorder hears the lines of a file in
the file's order whichever teller reports them. It places a line as before,
by its address and the evaluator's depth, so a file that another file loads
gets its own lines from its own walk.

### The walk hooks stand before the overlays

The recorder learns where a file begins from hooks on `_LD-WALK` and
`SOURCE-EVALUATE-CHECKED`. They used to replace the overlays of those two
words, which sent every walk to KDOS's own loop. `bind_walks` now puts each
hook in front of the overlay's `applicable` test: the hook notes the
buffer, and the test then decides as it does with no recorder. A word
whose overlay was not installed, because KDOS's definition has changed,
gets the hook alone and runs as KDOS defines it.

### What recording changes

Nothing the guest can see. A recording boot and a plain boot run the same
overlays, and the walker evaluates the same lines in both, with two
exceptions that reach only the host's counters:

- The reference evaluator reads a line's number from `EVAL-LINE` as
  `EVALUATE` begins, after the overlay has pushed the line's address and
  length. The walker reads it when the line commits, while those pushes are
  still held back. A line whose two pushes would land on `EVAL-LINE` is
  handed back while the tap is on.
- The tap reads the `EVAL-LINE` that the overlay writes. If that cell is
  not the evaluator's line cell, the walker hands back every line of a
  recorded runtime.

A hand-back changes nothing for the guest, by the walker's first rule:
every line it hands back, the Python fast path evaluates to the same guest
state.

### The reference recording

`TokenRecorder.native_tap` is on unless a test or a comparison turns it
off. Off, the walker hands every line of a recorded runtime back, and the
reference evaluator records the whole load, under the same overlays and
with the same guest. That recording is the measure of the tap. The same
load recorded both ways must give the same items and the same token files,
byte for byte.

### Exactness of a native recording

`tests/simulator/test_native_token_record.py` loads each of its sources on
four runtimes: three on the native backend, with no recorder, with the
reference recording and with the tap, and one on the Python backend with a
recorder. The three recordings must agree on every file's items and on its
token file or the reason it has none. The three native runs must agree on
all the guest state a native and a reference boot agree on, and on UART
output. Each case also fixes how many lines the walker evaluated and how
many it handed back, with a recorder and without, so that no case passes
with the walker standing aside. The cases cover a word named by its slot
after its IMPORT, DEFINED, numbers of each of the four sizes compiled and
interpreted, the TEXT of every parsing word the walker takes, LINE and
LINES across blank, comment and continued lines, SKIPPED in lines Python
runs between native lines, lines that run guest words between native
lines, a file that requires another through `_LD-WALK`, and loads that
fail.

At the Desktop's scale the test is the same. On 2026-11-15 the
`desktop-apt1` Desktop was built with `--token-modules` in four runs,
linked and from a system volume, each time once with its recording boot on
the reference path and once with the tap (MegaPad `a73c6beb`, Akashic
`86d817ea`, one build at a time on a shared host). Every run gave the same
result:

| | Linked | System volume |
| --- | --- | --- |
| Source files recorded, each with a token file | 56 | 313 |
| Token files with the tap against the reference path | the same, byte for byte, 3,415,378 bytes | the same, byte for byte, 3,906,152 bytes |
| Lines the walker evaluated and lines it handed to Python, with a recorder and with none | 200,536 and 9,209 | 256,914 and 10,606 |
| A recording boot against a boot of its own image with no recorder | the same guest, 7.13 million steps | the same guest, 69.4 million steps |

The last row is from three of the runs and holds for the recording on the
reference path as for the one with the tap. It covers all guest memory,
every definition with its IR, the side index, both stack pointers, the
cookie counter, semantic steps, the timer and UART output. Two builds do
not make the same image, because a volume states when it was made, so
guests are compared within one build and token files across the two.

Host time over the four runs:

| | Linked | System volume |
| --- | --- | --- |
| Boot with no recorder | 2.8 to 3.0 s | 10.4 to 12.0 s |
| Recording boot on the reference path | 23.4 to 25.6 s | 50.9 to 56.5 s |
| Recording boot with the tap | 3.5 to 3.9 s | 16.7 to 19.1 s |
| Whole build, reference path | 28.1 to 30.7 s | 54.0 to 59.7 s |
| Whole build, tap | 9.1 to 10.1 s | 20.8 to 23.4 s |

With the system volume a build's recording boot still takes about six
seconds more than a boot with no recorder, where the linked one takes
under a second more. The walker's share of that is small. Timed inside
one such boot, its walks took 1.9 seconds with the tap, the hand-over to
the recorder included, against 0.7 without, and the 313 token files were
then written in 0.4 seconds. The same recording boot made outside a build
took 11.8 seconds against 10.9 with no recorder. Where the rest of the
difference inside a build comes from has not been found. A run each
ruled out the cycle collector, a boot made earlier in the same process,
and the image file as the builder leaves it.

## Plans that survive publication

Publishing a definition used to advance the dictionary's execution
generation, which discards every native plan. During boot that rebuilt the
plans of whatever ran next after each of the 49,000 definitions. A new
definition changes no existing word: it has a fresh XT, its overlap proof
rules out reusing a live one, and plans bind their targets by XT. So both
dictionaries now advance the generation only when a definition is removed,
a body is reclaimed or the head is rebound. `DOES>`, overlay installation and
closing a hybrid runtime keep invalidating plans explicitly.

## Slices and gates

Each slice keeps both backends green and is committed on its own.

1. **Native dictionary and index.** Done. The native backend uses them.
   The dictionary, lease and numeric rollback tests run against both
   implementations; nine white-box tests that rewind the Python class's
   private `HERE` or hook its memory calls stay Python-only. The
   `desktop-apt1` boot fingerprint matches 1a's exactly, including the side
   index's bytes, and three alternating boots each gave medians of 26.5 s
   against 26.1 s for 1a, which is within the host's noise.
2. **Native line walker.** Done, for both overlays, with lazy Python IR
   views and compiler hand-over. `tests/simulator/test_native_walker.py`
   runs the same source through both overlay loops on two native runtimes
   and compares all guest memory, the dictionary with its IR, stacks,
   cookies, steps, the open definition, diagnostics and UART output, and
   asserts how many lines each side evaluated. In the `desktop-apt1` boot
   the walker evaluates 194,979 lines, exactly the lines the Python rule
   admits, and hands 9,343 to Python. The boot fingerprint matches 1a's in
   every field, including all 48,922 definitions' IR and the side index's
   bytes. Three alternating boot compiles gave medians of 5.8 s against
   28.1 s for 1a.
3. **Plans that survive publication.** Done. The boot fingerprint is
   unchanged and the boot ends with 659 plans instead of 116. Four
   alternating boot compiles gave medians of 6.0 s against 6.6 s for slice
   2, on a busier host than slice 2's own measurement.
4. **Token records, the replay.** Done. The walker evaluates a record line
   through the disposal it gives a source line.
   `tests/simulator/test_native_token_replay.py` compares native record
   lines with the replay loop, and with the token and walker tests it
   passes on each executor. The `desktop-apt1` token boots were run with
   the replay in the walker and with it left to Python, and everything the
   two boots built was compared: they built the same system, in the same
   semantic steps, 5,593,450 linked and 56,926,335 with the system volume.
   The walker evaluates 200,410 record lines of the linked boot and
   225,808 with the system volume, hands 5,836 and 7,172 to Python, and
   bars none. Three boots each, one at a time on a shared host, took 1.7,
   1.7 and 2.0 seconds linked from token forms, against 3.9, 4.0 and 4.0
   with the replay in Python and 2.6, 2.8 and 3.1 from source. With the
   system volume they took 8.9, 9.0 and 9.9 seconds, against 11.7, 12.3
   and 12.0 with the replay in Python and 11.6, 11.8 and 13.2 from source.
   An earlier round, on the same change before it was brought onto the
   chiplet work in main, agreed for the linked boot and put the
   system-volume boot from token files level with source, at 10.4 to 10.7
   seconds against 10.2 to 11.2; the steps are the figures that do not
   move with the host. The physical Desktop journey passed from token
   forms linked, from the system volume and in hybrid mode. A token build
   was no faster at this slice: its recording boot still ran every line on
   the reference path.
5. **Token records, the recorder's tap.** Done. A recording boot keeps the
   walker, which reports what it did with each token to the recorder.
   `tests/simulator/test_native_token_record.py` records each of its
   sources with the tap, on the reference path and on the Python backend,
   and with the token, replay and walker tests it passes on each executor,
   210 cases. The `desktop-apt1` Desktop was then built on MegaPad
   `29b5510b` with Akashic `e554385c`, linked and from a system volume,
   three ways: with the tap, with the reference recording, and by the
   recorder MegaPad had before the tap (`5dd6fe1e`), whose hooks sent
   every walk to KDOS's own loop. All three gave the same token files,
   byte for byte: 56 files of 3,433,700 bytes linked and 316 files of
   3,922,495 bytes with the system volume. Each recording boot left the
   same guest as a boot of its own image with no recorder, and the walker
   evaluated the same lines with a recorder as without, 201,435 linked
   and 258,217 with the system volume. A linked token build took 10.1
   seconds where the reference recording took 30.9, and one with the
   system volume 23.7 against 60.2; a source build takes two. The
   physical Desktop journey passed from token forms linked, from the
   system volume and in hybrid mode, and from source.
Each slice's boot compile was measured with alternating runs as in step 1a.
`make test-simulator` and `make test-simulator-native` ran after every slice,
and the physical Desktop journey passed after slices 2 and 3.

## Not changing

- Source is still evaluated at every boot. This is not a compiled-Forth cache.
- The KDOS boot source loop in `image_bootstrap.py` stays in Python. After
  step 1 it costs about 0.15 s.
- Lines the checked-line rule does not admit keep the KDOS `CATCH` path and
  the Python evaluator. Their remaining cost is mostly the guest words they
  run, not their evaluation.
- The rules in `simulator-contract.md` §4 still hold: at most 255 bytes per
  `EVALUATE`, the sticky first failure, the temporary `IF` lifetime,
  transaction rollback, and identity-bound overlays that fall back to the
  original definition.
