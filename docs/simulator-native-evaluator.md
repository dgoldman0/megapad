# Native dictionary and source evaluator

Status: step 1b of the [simulator speed plan](simulator-speed-plan.md),
2026-10-13. Slices 1 to 3 are implemented: the native dictionary, the native
line walker and plans that survive publication. Slice 4 is designed. Each
slice updates this document when it lands.

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

## Plans that survive publication

Publishing a definition used to advance the dictionary's execution
generation, which discards every native plan. During boot that rebuilt the
plans of whatever ran next after each of the 49,000 definitions. A new
definition changes no existing word: it has a fresh XT, its overlap proof
rules out reusing a live one, and plans bind their targets by XT. So both
dictionaries now advance the generation only when a definition is removed,
a body is reclaimed or the head is rebound. `DOES>`, overlay installation and
closing a hybrid runtime keep invalidating plans explicitly.

## Wider admission

Most of the remaining Python lines are `CREATE` followed by `ALLOT`, `,` or
`C,`, several `VARIABLE`s on one line, `'` before `CONSTANT` or `IS`, and
data tables of `,`. None of these can run arbitrary guest code once growth and
stack room are proven. Widening the checked-line rule to admit them changes
the Python reference and the native walker together, and the differential
gate still compares them. Admitted lines skip the `CATCH` wrapper, so the
semantic step count falls. The contract already treats that as an
optimization property of admitted overlays.

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
4. **Wider admission** in both evaluators. The boot fingerprint now matches
   the Python backend's boot with the same rule.

After slices 2 and 4, the boot compile is measured with alternating runs as
in step 1a, and the physical Desktop journey runs as the regression gate.
`make test-simulator` and `make test-simulator-native` run after every slice.

## Not changing

- Source is still evaluated at every boot. This is not a compiled-Forth cache.
- The KDOS boot source loop in `image_bootstrap.py` stays in Python. It is
  about 0.4 s.
- The `CATCH` path for lines that run guest code stays in Python.
- The rules in `simulator-contract.md` §4 still hold: at most 255 bytes per
  `EVALUATE`, the sticky first failure, the temporary `IF` lifetime,
  transaction rollback, and identity-bound overlays that fall back to the
  original definition.
