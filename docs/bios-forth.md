# BIOS Forth Word Reference

The Megapad-64 BIOS implements a **subroutine-threaded Forth** directly in
assembly.  It boots from address zero, initializes hardware, and presents a
standard Forth REPL over the UART.  If a disk is attached it scans MP64FS for
the first Forth-type file and loads it with the `FSLOAD` machinery.  The
standard image places the KDOS core first.

> **Implementation boundary.** This reference describes the checked-in
> `bios.asm` source. A generated `bios.rom` reflects it only after explicit
> artifact regeneration. The numeric CRC, SHA3, Keccak, and WOTS interface is in
> [`crypto-interface-contract.md`](crypto-interface-contract.md). The qualified
> checkpoint-3 configuration advertises `CRYPTO_CAPS = 0xF`: reflected/raw
> CRC (bit 0), checked SHA3/SHAKE streaming (bit 1), raw Keccak-f[1600]
> (bit 2), and the production checked WOTS chain with real Bank 0 DMA (bit 3).
> The complete checkpoint-3 backend path has passed qualification and the
> checked-in `bios.rom` was regenerated from this source. A derivative backend
> missing that path must keep bit 3 clear; `WOTS-CHAIN` then returns
> `UNSUPPORTED` before argument or device access.

This document organizes the BIOS dictionary by functional category.  Each
entry shows the **stack effect**
(data-stack inputs on the left, outputs on the right of `--`), a plain-
English description, and notes on edge cases where relevant.

> **Notation.**  `( before -- after )` is the classic Forth stack comment.
> *flag* means a boolean: `0` = false, `-1` (all bits set) = true.
> *addr* means a byte address.  *u* means unsigned, *n* means signed,
> *c* means a single byte (character), *xt* means an execution token.

---

## Stack Manipulation (16 words)

These words rearrange the data stack without performing any computation.
If you are new to Forth, mastering `DUP SWAP DROP ROT OVER` is the
essential first step.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DUP` | `( x -- x x )` | Duplicate the top item. |
| `DROP` | `( x -- )` | Discard the top item. |
| `SWAP` | `( a b -- b a )` | Exchange the top two items. |
| `OVER` | `( a b -- a b a )` | Copy the second item to the top. |
| `ROT` | `( a b c -- b c a )` | Rotate the third item to the top. |
| `-ROT` | `( a b c -- c a b )` | Reverse rotate — top item goes to third position. |
| `NIP` | `( a b -- b )` | Drop the second item (`SWAP DROP`). |
| `TUCK` | `( a b -- b a b )` | Copy the top item below the second (`SWAP OVER`). |
| `?DUP` | `( x -- x x \| 0 )` | Duplicate only if non-zero.  Useful before `IF`. |
| `2DUP` | `( a b -- a b a b )` | Duplicate the top pair. |
| `2DROP` | `( a b -- )` | Discard the top pair. |
| `2SWAP` | `( a b c d -- c d a b )` | Exchange the top two pairs. |
| `2OVER` | `( a b c d -- a b c d a b )` | Copy the second pair to the top. |
| `2ROT` | `( a b c d e f -- c d e f a b )` | Rotate the third pair to the top. |
| `PICK` | `( xn ... x0 n -- xn ... x0 xn )` | Copy the *n*-th item (0 = top) to the top. |
| `DEPTH` | `( -- n )` | Number of items currently on the stack. |

**Example — swapping and duplicating:**
```forth
10 20 SWAP    \ stack: 20 10
DUP           \ stack: 20 10 10
ROT           \ stack: 10 10 20
```

---

## Return Stack (6 words)

The return stack is normally used by the compiler for loop counters and
subroutine returns, but you can temporarily stash values there.  **Always
balance your `>R` / `R>` pairs within a single definition.**

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `>R` | `( x -- ) R:( -- x )` | Move top of data stack to return stack. |
| `R>` | `( -- x ) R:( x -- )` | Move top of return stack to data stack. |
| `R@` | `( -- x ) R:( x -- x )` | Copy top of return stack (non-destructive peek). |
| `2>R` | `( a b -- ) R:( -- a b )` | Move a pair to the return stack (a pushed first). |
| `2R>` | `( -- a b ) R:( a b -- )` | Move a pair back from the return stack. |
| `2R@` | `( -- a b ) R:( a b -- a b )` | Copy a pair from the return stack (non-destructive). |

**Example — saving a value across a computation:**
```forth
: SQUARE  ( n -- n^2 )  DUP >R  R@ *  R> DROP ;
```

---

## Arithmetic (17 words)

All arithmetic operates on **64-bit** values.  Division by zero triggers a
CPU trap (vector `IVEC_DIV_ZERO`).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `+` | `( a b -- a+b )` | Addition. |
| `-` | `( a b -- a-b )` | Subtraction (a minus b). |
| `*` | `( a b -- a*b )` | Signed multiplication (low 64 bits). |
| `UM*` | `( u1 u2 -- lo hi )` | Unsigned 64×64-bit multiply, returning the low and high halves of the 128-bit product. |
| `/` | `( a b -- a/b )` | Signed integer division.  Traps on b=0. |
| `MOD` | `( a b -- a%b )` | Signed modulo. |
| `/MOD` | `( a b -- rem quot )` | Signed division with remainder. |
| `NEGATE` | `( n -- -n )` | Two's-complement negation. |
| `ABS` | `( n -- |n| )` | Absolute value. |
| `MIN` | `( a b -- min )` | Documented signed minimum; current executable behavior has the unresolved signedness discrepancy below. |
| `MAX` | `( a b -- max )` | Documented signed maximum; current executable behavior has the unresolved signedness discrepancy below. |
| `1+` | `( n -- n+1 )` | Increment by one. |
| `1-` | `( n -- n-1 )` | Decrement by one. |
| `2+` | `( n -- n+2 )` | Increment by two. |
| `2-` | `( n -- n-2 )` | Decrement by two. |
| `CELLS` | `( n -- n*8 )` | Convert a cell count to bytes (cells are 8 bytes). |
| `CELL+` | `( addr -- addr+8 )` | Advance an address by one cell (8 bytes). |

> **Open `MIN`/`MAX` signedness discrepancy.** The public descriptions and
> BIOS source comments specify signed comparison, but the current `bios.asm`
> implementations branch on MP64 `G`/`LE`, whose executable ISA and emulator
> semantics are unsigned. This note records the mismatch; it does not decide
> whether the API intent or the present implementation should change. The
> hosted simulator mirrors the current executable behavior for differential
> work until BIOS, emulator, simulator, tests, and this reference are resolved
> together.

> **Open signed-`MOD` overflow edge.** The native executable guards signed
> division for `INT64_MIN / -1`, but its signed-`MOD` path performs that same
> division in C++ without the overflow guard. That operand pair therefore has
> no qualified native result. The hosted simulator currently produces the
> mathematical remainder zero, but this note does not decide whether the
> eventual architecture should return zero or trap. Current KDOS
> `RAND-RANGE` qualification requires a positive signed divisor and cannot
> reach this edge.

**Example — computing an average:**
```forth
: AVG  ( a b -- avg )  + 2 / ;
5 15 AVG .    \ prints 10
```

---

## Logic & Bitwise (6 words)

These operate bit-by-bit on 64-bit values.  `INVERT` flips all 64 bits.
`LSHIFT` and `RSHIFT` are logical (zero-filling) shifts.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `AND` | `( a b -- a&b )` | Bitwise AND. |
| `OR` | `( a b -- a\|b )` | Bitwise OR. |
| `XOR` | `( a b -- a^b )` | Bitwise exclusive-OR. |
| `INVERT` | `( x -- ~x )` | Bitwise complement (flip every bit). |
| `LSHIFT` | `( x n -- x<<n )` | Logical left shift by *n* bits. |
| `RSHIFT` | `( x n -- x>>n )` | Logical right shift by *n* bits (zero-fill). |

**Example — masking the low byte:**
```forth
0xDEADBEEF  0xFF AND .   \ prints 239 (0xEF)
```

---

## Comparison & Testing (13 words)

All comparisons return a **flag**: `-1` for true, `0` for false.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `=` | `( a b -- flag )` | True if a equals b. |
| `<>` | `( a b -- flag )` | True if a does not equal b. |
| `<` | `( a b -- flag )` | True if a < b (signed). |
| `>` | `( a b -- flag )` | True if a > b (signed). |
| `<=` | `( a b -- flag )` | True if a ≤ b (signed). |
| `>=` | `( a b -- flag )` | True if a ≥ b (signed). |
| `U<` | `( a b -- flag )` | True if a < b (unsigned). |
| `U>` | `( a b -- flag )` | True if a > b (unsigned). |
| `0=` | `( x -- flag )` | True if x is zero. |
| `0<>` | `( x -- flag )` | True if x is non-zero. |
| `0<` | `( n -- flag )` | True if n is negative. |
| `0>` | `( n -- flag )` | True if n is positive (> 0). |
| `WITHIN` | `( x lo hi -- flag )` | True if lo ≤ x < hi (unsigned range check). |

**Example — conditional logic:**
```forth
: CLASSIFY  ( n -- )
    DUP 0= IF ." zero" DROP EXIT THEN
    DUP 0< IF ." negative" ELSE ." positive" THEN DROP ;
```

---

## Memory Access (18 words)

The Megapad-64 is a **64-bit little-endian** machine with byte-addressable
memory.  A **cell** is 8 bytes (one 64-bit word).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `@` | `( addr -- x )` | Fetch a cell (8 bytes) from memory. |
| `!` | `( x addr -- )` | Store a cell (8 bytes) to memory. |
| `C@` | `( addr -- c )` | Fetch a single byte. |
| `C!` | `( c addr -- )` | Store a single byte. |
| `W@` | `( addr -- u16 )` | Fetch a 16-bit unsigned halfword. |
| `W!` | `( u16 addr -- )` | Store a 16-bit halfword. |
| `L@` | `( addr -- u32 )` | Fetch a 32-bit unsigned longword. |
| `L!` | `( u32 addr -- )` | Store a 32-bit longword. |
| `HERE` | `( -- addr )` | Address of the next free byte in the dictionary. |
| `,` | `( x -- )` | Compile (append) a cell to the dictionary, advance HERE by 8. |
| `C,` | `( c -- )` | Compile a single byte, advance HERE by 1. |
| `ALLOT` | `( n -- )` | Advance HERE by *n* bytes (reserve space). |
| `CMOVE` | `( src dst n -- )` | Copy *n* bytes, low-to-high (safe for forward overlap). |
| `MOVE` | `( src dst n -- )` | Copy *n* bytes, direction-safe. |
| `FILL` | `( addr n c -- )` | Fill *n* bytes starting at addr with byte *c*. |
| `ERASE` | `( addr n -- )` | Zero *n* bytes starting at addr (`0 FILL`). |
| `DUMP` | `( addr n -- )` | Hex-dump *n* bytes in a readable format. |
| `TALIGN` | `( -- )` | Align HERE to the next 64-byte boundary (tile-alignment). |

**Example — reading and writing memory:**
```forth
VARIABLE COUNTER          \ allocate a cell
42 COUNTER !              \ store 42
COUNTER @ .               \ fetch and print: 42
```

---

## Input & Output (18 words)

I/O goes through the UART.  `EMIT` appends one byte to a 4096-byte TX ring
buffer in RAM (auto-flushed when full); `KEY` waits for one byte (and flushes
the TX buffer first so prompts appear before blocking).  The number-printing
words all produce human-readable ASCII output.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `EMIT` | `( c -- )` | Append one character to the TX ring buffer (flushed automatically when full, or by `TX-FLUSH`). |
| `KEY` | `( -- c )` | Flush the TX buffer, then wait for and return one character from the UART. |
| `KEY?` | `( -- flag )` | True if a character is available (non-blocking check). |
| `CR` | `( -- )` | Emit a carriage-return + line-feed (newline). |
| `SPACE` | `( -- )` | Emit a single space character (ASCII 32). |
| `SPACES` | `( n -- )` | Emit *n* space characters. |
| `.` | `( n -- )` | Print a signed number followed by a space. |
| `U.` | `( u -- )` | Print an unsigned number followed by a space. |
| `.S` | `( -- )` | Print the entire stack contents non-destructively. |
| `TYPE` | `( addr n -- )` | Print *n* characters starting at addr. |
| `ACCEPT` | `( addr n -- actual )` | Read up to *n* characters from the UART into addr.  Returns actual count.  Handles backspace. |
| `WORDS` | `( -- )` | Print all words in the dictionary. |
| `BYE` | `( -- )` | Flush the TX buffer and halt the CPU (exit Forth). |
| `TX-FLUSH` | `( -- )` | Explicitly drain the TX ring buffer to the host. |
| `CYCLES` | `( -- u )` | Read the free-running cycle counter (for timing). |
| `MS` | `( n -- )` | Delay approximately *n* milliseconds. |
| `HEX` | `( -- )` | Set numeric output base to 16. |
| `DECIMAL` | `( -- )` | Set numeric output base to 10. |

**Example — printing a greeting:**
```forth
: GREET  ." Hello, Megapad!" CR ;
GREET
```

---

## Strings (8 words)

Strings in BIOS Forth are **counted strings** (address + length on the
stack) or **compiled inline** (the `S"` and `."` pattern).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `S"` | `( -- addr len )` | In a definition, compile an inline string literal whose address and length are pushed at runtime. At the REPL, return a transient literal in the BIOS-private string buffer. The transient form is suitable for immediate CPU-only consumers such as `COMPARE`, but checked device words reject it as protected memory; compile the literal or copy it into caller-managed storage before passing it to checked crypto, entropy, or DMA interfaces. |
| `."` | *see below* | Print a string literal.  Works in **both** interpret and compile modes: in a definition it compiles inline and prints at runtime; at the REPL it prints immediately. |
| `WORD` | `( delim -- addr )` | Parse the next token delimited by *delim* from the input buffer.  Returns a counted-string address. |
| `COUNT` | `( c-addr -- addr len )` | Convert a counted string (length byte at c-addr) to an address+length pair. |
| `COMPARE` | `( a1 n1 a2 n2 -- result )` | Compare two strings lexicographically.  Returns 0 if equal, negative if first < second, positive if first > second. |
| `CHAR` | `( "c" -- n )` | Parse the next word and push its first character's ASCII value. |
| `[CHAR]` | `( "c" -- )` | Compile-time version of CHAR — compiles the character as a literal. Immediate. |
| `UCHAR` | `( c -- C )` | Convert a lowercase ASCII letter to uppercase.  Non-letters pass through unchanged. |

**Example — string comparison:**
```forth
: SAME?  ( a1 n1 a2 n2 -- )
    COMPARE 0= IF ." match" ELSE ." differ" THEN CR ;
S" hello" S" hello" SAME?    \ prints "match"
S" hello" S" world" SAME?    \ prints "differ"
```

---

## Control Flow (15 words)

These words implement branching and looping.  Most are **immediate** (they
execute at compile time to lay down branch instructions).

### Conditional: IF / ELSE / THEN

```forth
: ABS-VAL  ( n -- |n| )
    DUP 0< IF NEGATE THEN ;
```

`IF` consumes a flag.  If true (non-zero), the code between `IF` and
`ELSE` (or `THEN`) runs.  If false, execution jumps to `ELSE` (or `THEN`).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `IF` | `( flag -- )` | Begin conditional.  Immediate. |
| `ELSE` | `( -- )` | Begin false branch.  Immediate. |
| `THEN` | `( -- )` | End conditional.  Immediate. |

### Indefinite Loops: BEGIN / UNTIL / WHILE / REPEAT / AGAIN

```forth
\ Print 10 down to 1
: COUNTDOWN  ( -- )
    10 BEGIN  DUP . CR  1-  DUP 0= UNTIL  DROP ;

\ Read characters until 'q'
: READ-UNTIL-Q  ( -- )
    BEGIN  KEY  DUP [CHAR] q <>  WHILE  EMIT  REPEAT DROP ;
```

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BEGIN` | `( -- )` | Mark the start of a loop.  Immediate. |
| `UNTIL` | `( flag -- )` | Loop back to `BEGIN` if flag is false.  Immediate. |
| `WHILE` | `( flag -- )` | If flag is true, continue; if false, jump past `REPEAT`.  Immediate. |
| `REPEAT` | `( -- )` | Jump back to `BEGIN` unconditionally.  Immediate. |
| `AGAIN` | `( -- )` | Jump back to `BEGIN` unconditionally (infinite loop).  Immediate. |

### Counted Loops: DO / LOOP / +LOOP

```forth
\ Print 0 1 2 3 4
: FIVE  ( -- )  5 0 DO  I . LOOP CR ;

\ Print even numbers 0 2 4 6 8
: EVENS  ( -- )  10 0 DO  I .  2 +LOOP CR ;
```

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DO` | `( limit start -- )` | Begin a counted loop from *start* to *limit*−1.  Immediate. |
| `LOOP` | `( -- )` | Increment the loop index by 1; loop back to `DO` if not done.  Immediate. |
| `+LOOP` | `( n -- )` | Increment the loop index by *n*; loop back to `DO` if not done.  Immediate. |
| `I` | `( -- n )` | Current loop index (innermost loop). |
| `J` | `( -- n )` | Loop index of the next-outer loop. |
| `LEAVE` | `( -- )` | Exit the current loop immediately. |
| `UNLOOP` | `( -- )` | Discard loop parameters from return stack (use before `EXIT` inside a loop). |

---

## Compilation & Defining Words (24 words)

These words create new dictionary entries or control the compiler.

### Defining New Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `:` | `( "name" -- )` | Begin compiling a new word definition. |
| `;` | `( -- )` | End the current definition.  Immediate. |
| `CREATE` | `( "name" -- )` | Create a dictionary entry that pushes its data-field address at runtime. |
| `DOES>` | `( -- )` | Define the runtime behavior of a `CREATE`d word.  The code after `DOES>` executes when the child word runs, with the data-field address on the stack. |
| `VARIABLE` | `( "name" -- )` | Create a word that holds one cell of data.  Running it pushes its address. |
| `CONSTANT` | `( x "name" -- )` | Create a word that always pushes *x*. |
| `VALUE` | `( x "name" -- )` | Create a named value.  Running it pushes *x*. Modify with `TO`. |
| `TO` | `( x "name" -- )` | Change the value stored in a `VALUE` word.  Immediate (works in compile mode too). |

**Example — a custom defining word:**
```forth
: ARRAY  ( n "name" -- )
    CREATE CELLS ALLOT
    DOES>  ( index -- addr )  SWAP CELLS + ;

10 ARRAY MY-DATA       \ create a 10-cell array
42 3 MY-DATA !          \ store 42 at index 3
3 MY-DATA @ .           \ prints 42
```

### Compiler Control

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `IMMEDIATE` | `( -- )` | Mark the most recently defined word as immediate (executes during compilation). |
| `POSTPONE` | `( "name" -- )` | Compile a reference to *name* into the current definition, even if *name* is immediate.  Immediate. |
| `LITERAL` | `( x -- )` | Compile *x* as a literal into the current definition.  Immediate. |
| `[` | `( -- )` | Switch to interpret state (inside a definition).  Immediate. |
| `]` | `( -- )` | Switch to compile state. |
| `RECURSE` | `( -- )` | Compile a recursive call to the word currently being defined.  Immediate. |
| `'` | `( "name" -- xt )` | Find *name* in the dictionary and push its execution token. |
| `[']` | `( "name" -- )` | Compile-time: parse next word, compile its XT as a literal.  Equivalent to `' name LITERAL`.  Immediate. |
| `>BODY` | `( xt -- addr )` | Data-field address of a CREATEd word (xt + 30). |
| `EXECUTE` | `( xt -- )` | Call the word whose execution token is on the stack. |
| `FIND` | `( c-addr -- xt flag \| c-addr 0 )` | Search the dictionary for a counted string.  Returns the xt and a flag (+1 immediate, −1 normal) or 0 if not found. |
| `STATE` | `( -- addr )` | Address of the compiler state variable (0 = interpreting, non-zero = compiling). |
| `LATEST` | `( -- addr )` | Address of the most recent dictionary entry.  Useful for dictionary traversal. |

### Anonymous Definitions & Quotations

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `:NONAME` | `( -- xt )` | Begin an anonymous (headerless) definition.  Pushes HERE as the XT.  Terminated by `;`, which leaves the XT on the stack.  `IMMEDIATE` after `:NONAME ... ;` is an error (no dictionary entry to mark).  `RECURSE` works inside `:NONAME` (calls back into the anonymous body). |
| `[:` | `( -- )` | Open a quotation inside a definition.  Compiles a forward branch over the quotation body and pushes bookkeeping data.  Immediate.  Must be used inside `:` or `:NONAME`. |
| `;]` | `( -- )` | Close a quotation.  Compiles a return, resolves the forward branch, and compiles the quotation's XT as a literal into the enclosing definition.  Immediate.  Must pair with a preceding `[:`. |

**Example — anonymous definitions:**
```forth
:NONAME DUP + ; 21 SWAP EXECUTE .   \ prints 42

VARIABLE doubler
:NONAME DUP + ; doubler !
10 doubler @ EXECUTE .               \ prints 20
```

**Example — quotations:**
```forth
: APPLY  ( xt n -- n' )  SWAP EXECUTE ;
: TEST   [: DUP * ;] 5 APPLY . ;
TEST    \ prints 25

\ Nested quotations
: MAKE-ADDER  ( n -- xt )
    [: [: DUP ;] EXECUTE + ;] ;
5 MAKE-ADDER 3 SWAP EXECUTE .        \ prints 8
```

---

## Input Source & Evaluation (15 words)

These words control where Forth reads its input from and how it processes
text.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SOURCE` | `( -- addr len )` | Address and length of the current input buffer. |
| `>IN` | `( -- addr )` | Address of the variable holding the current parse position within `SOURCE`. |
| `EVALUATE` | `( addr len -- )` | Interpret one string as Forth source.  Retains the legacy stack effect but records any error in `EVAL-STATUS`.  Input above 255 bytes is rejected, never truncated.  Nested calls preserve and restore the caller's complete TIB bytes, length, and `>IN`. |
| `EVALUATE-CHECKED` | `( addr len -- status )` | BIOS primitive returning status 0–3.  After KDOS defines `CATCH`, KDOS deliberately shadows this name with the transaction-safe wrapper described below, which also returns status 5. |
| `EVALUATE-FINISH` | `( -- status )` | End a multi-line checked operation, returning 4 if compiler or cross-line conditional state is unfinished, otherwise 0. |
| `EVALUATOR-RESET` | `( -- )` | Clear compiler bookkeeping after the caller invokes `DICT-ROLLBACK` with its saved `HERE/LATEST` pair. Does not roll back the dictionary itself, erase diagnostics, or change the enclosing evaluator depth. |
| `EVALUATOR-UNWIND` | `( depth -- )` | Restore abandoned nested input frames down to a previously captured `EVAL-DEPTH @` checkpoint.  Negative or above-current targets are ignored. |
| `EVAL-STATUS` | `( -- addr )` | Address of the last status cell. |
| `EVAL-LINE` | `( -- addr )` | Address of the one-based source-line context/diagnostic cell. |
| `EVAL-COLUMN` | `( -- addr )` | Address of the zero-based failing-token column cell. |
| `EVAL-DEPTH` | `( -- addr )` | Address of the active evaluator-depth cell.  Transaction hosts capture its value before checked evaluation and pass that value to `EVALUATOR-UNWIND`; they do not repair the cell alone. |
| `EVAL-THROW` | `( -- addr )` | Address of the exact source-level exception code retained when the KDOS checked wrapper returns status 5. |
| `EVAL-TOKEN` | `( -- addr len )` | Stable counted view of the failing token; empty for line/depth/unfinished/throw failures. |
| `>NUMBER` | `( ud addr len -- ud' addr' len' )` | Convert characters to a number, accumulating into *ud*.  Stops at the first non-digit. |
| `QUIT` | `( -- )` | Clear the return stack and enter the outer interpreter loop (the REPL).  Does not return. |

**Example — dynamic evaluation:**
```forth
S" 2 3 + ." EVALUATE    \ prints 5
```

Checked status values are deterministic:

| Status | Constant in KDOS | Meaning |
|--------|------------------|---------|
| 0 | `EVAL-S-OK` | Success |
| 1 | `EVAL-S-UNDEFINED` | Undefined token; inspect line, column, and token diagnostics |
| 2 | `EVAL-S-LINE-TOO-LONG` | Physical input line exceeds 255 bytes |
| 3 | `EVAL-S-DEPTH` | Evaluator nesting limit exceeded |
| 4 | `EVAL-S-UNFINISHED` | End of source reached with unfinished compiler state |
| 5 | `EVAL-S-THROW` | KDOS caught a nonzero source-level `THROW`; inspect `EVAL-THROW @` |

The BIOS definition of `EVALUATE-CHECKED` exists before KDOS and therefore
cannot depend on KDOS's execution-context-local `HANDLER` table.  Once
`CATCH`/`THROW` exist, KDOS defines a newer word with the same public name.
That wrapper checkpoints `EVAL-DEPTH`, catches a source exception, asks BIOS to
restore every abandoned input frame, stores the exact exception in
`EVAL-THROW`, and returns status 5 normally.  This ordering keeps exception
ownership in KDOS while making `SOURCE-EVALUATE-CHECKED` transaction-safe.

Checked evaluation intentionally permits a colon definition or conditional to
span calls.  Use `EVALUATE-FINISH` once after the last line.  KDOS packages
that protocol as `SOURCE-EVALUATE-CHECKED` for complete multi-line buffers.
After any nonzero transactional result, pass the saved `HERE/LATEST` pair to
`DICT-ROLLBACK` before calling `EVALUATOR-RESET`; status and diagnostics remain
available afterward.

---

## Dictionary Bounds and Acceleration (9 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DICT-BOUNDS!` | `( base limit -- )` | Install a checked inclusive/exclusive external dictionary interval. Invalid or wrapping bounds fault before publication. |
| `DICT-BOUNDS-OFF` | `( -- )` | Disable the external interval and restore guarded Bank-0 allocation semantics. |
| `DICT-BASE@` | `( -- base )` | Return the active external dictionary base, or zero when disabled. |
| `DICT-LIMIT@` | `( -- limit )` | Return the active exclusive external dictionary limit, or zero when disabled. |
| `DICT-FAULT-XT!` | `( xt -- )` | Install the caller's dictionary-fault callback used by KDOS to throw `-8`. |
| `DICT-INDEX!` | `( base slots -- status )` | Install and rebuild a 16-byte-slot, power-of-two caller table, or disable with `0 0`. Returns 0 for authoritative success, 1 for invalid arguments with the old binding unchanged, or 2 for an installed saturated fallback. |
| `DICT-INDEX@` | `( -- base slots count flags )` | Return table geometry, occupied unique-name slots, and `BOUND=1`, `AUTHORITATIVE=2`, `BUILDING=4`, `SATURATED=8`. |
| `DICT-ROLLBACK` | `( saved-here saved-latest -- )` | Validate a contiguous active-zone checkpoint, clear cached bindings globally, publish both dictionary pointers, and rebuild the side index. |
| `LATEST!` | `( entry -- )` | Validate and publish any terminating replacement dictionary chain without changing `HERE`, clear cached bindings globally, and rebuild the side index. |

The linked list remains authoritative. Names through 31 bytes are eligible for
the 1,024-entry `EXT.DICT` working-set cache; the BIOS index covers every name
through the 127-byte header limit when it is bound. See
[`dictionary-acceleration.md`](dictionary-acceleration.md) for the slot,
publication-generation, replacement, rollback, and deferred RTL contracts.

---

## Disk I/O (17 words)

Sector-based disk access. Each sector is **512 bytes**. Production code uses
the checked operations, which validate the complete request, own filesystem
spinlock 2, split transfers above 255 sectors, wait for a matching completion
generation, and return precise status. The ordinary checked words snapshot
`DISK-MEDIA-GEN` under the lock; the `-GEN-CHECKED` variants instead bind the
request to a caller-supplied generation and return `MEDIA_REMOVED` (11) for a
stale identity. Both forms require the controller's generation-guard
capability (CAPS bit 6) and submit atomically through `GUARDED_CMD`. The raw
setup and command words remain available for diagnostics and controller
bring-up; they do not wait for completion and are unsafe as filesystem
primitives.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DISK@` | `( -- status )` | Read controller status: busy=bit 0, error=bit 1, rejected=bit 2, result-valid=bit 3, media-changed=bit 4, write-protected=bit 5, present=bit 7. |
| `DISK-SECTORS` | `( -- count )` | Read the attached media capacity as an unsigned count of 512-byte sectors. |
| `DISK-MEDIA-GEN` | `( -- generation )` | Read the current attachment generation as an unsigned 32-bit identity. It changes whenever media is attached, detached, or replaced. |
| `DISK-CAPS` | `( -- caps )` | Read controller capabilities: read=bit 0, write=bit 1, flush=bit 2, precise result=bit 3, completion=bit 4, media generation=bit 5, generation guard=bit 6. |
| `MP64FS-VALID?` | `( -- flag )` | Validate the attached marker, derived geometry, reserved bitmap, directory entries, parents, extents, and byte bounds. |
| `DISK-READ-CHECKED` | `( dma lba count -- completed status )` | Production read. Returns only confirmed whole sectors and the stable controller result byte. |
| `DISK-WRITE-CHECKED` | `( dma lba count -- completed status )` | Production write. Completion is not durability; follow persistent updates with checked flush. |
| `DISK-FLUSH-CHECKED` | `( -- status )` | Production ordering and durability barrier for all earlier successful writes. |
| `DISK-READ-GEN-CHECKED` | `( dma lba count generation -- completed status )` | Generation-bound production read. A stale generation returns zero completed sectors and `MEDIA_REMOVED`. |
| `DISK-WRITE-GEN-CHECKED` | `( dma lba count generation -- completed status )` | Generation-bound production write. A stale generation is rejected before DMA or media mutation. |
| `DISK-FLUSH-GEN-CHECKED` | `( generation -- status )` | Generation-bound ordering and durability barrier. A stale generation is rejected before any flush effect. |
| `DISK-SEC!` | `( sector -- )` | Diagnostic: set the raw sector register. |
| `DISK-DMA!` | `( addr -- )` | Diagnostic: set the raw 64-bit DMA address. |
| `DISK-N!` | `( n -- )` | Diagnostic: set the raw controller sector count (legal command counts are 1–255). |
| `DISK-READ` | `( -- )` | Diagnostic: issue raw READ without waiting or translating its result. |
| `DISK-WRITE` | `( -- )` | Diagnostic: issue raw WRITE without waiting or translating its result. |
| `DISK-FLUSH` | `( -- )` | Diagnostic: issue raw FLUSH without waiting. Hardware backends perform their defined media-ready durability protocol. |

**Example — checked read of sector 10 into a buffer:**
```forth
CREATE SECBUF 512 ALLOT
SECBUF 10 1 DISK-READ-CHECKED  ( completed status )
0<> ABORT" disk read failed"
1 <> ABORT" short disk read"
SECBUF 512 DUMP
```

The result enum and extended MMIO registers are frozen in
[`storage-controller-contract.md`](storage-controller-contract.md).

---

## Timer & Interrupts (6 words)

The Megapad-64 has a 32-bit free-running timer with compare-match
capability, plus an interrupt enable/disable mechanism.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TIMER!` | `( value -- )` | Write to the timer compare-match register. |
| `TIMER-CTRL!` | `( bits -- )` | Write to the timer control register (bit 0: enable, bit 1: IRQ, bit 2: auto-reload). |
| `TIMER-ACK` | `( -- )` | Acknowledge a timer interrupt (clear the compare-match flag). |
| `EI!` | `( -- )` | Enable interrupts globally. |
| `DI!` | `( -- )` | Disable interrupts globally. |
| `ISR!` | `( addr -- )` | Set the interrupt vector table base address. |

---

## Tile Engine (47 words)

The tile engine (MEX extension) performs **SIMD operations** on 64-byte
memory tiles.  Tiles are divided into lanes based on element width
(64×8-bit, 32×16-bit, 16×32-bit, 8×64-bit, or 32×FP16/BF16).

### CSR Access

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TMODE!` | `( mode -- )` | Set the element width, saturation, and rounding.  Bits 2:0 = EW (0=8b, 1=16b, 2=32b, 3=64b, 4=FP16, 5=BF16).  Bit 4 = signed.  Bit 5 = saturating.  Bit 6 = rounding shifts. |
| `TMODE@` | `( -- mode )` | Read current tile mode. |
| `TCTRL!` | `( ctrl -- )` | Set tile control (bit 0: accumulate, bit 1: zero-ACC-first). |
| `TCTRL@` | `( -- ctrl )` | Read tile control. |
| `TSRC0!` | `( addr -- )` | Set source tile 0 address. |
| `TSRC1!` | `( addr -- )` | Set source tile 1 address. |
| `TDST!` | `( addr -- )` | Set destination tile address. |
| `ACC@` | `( -- n )` | Read accumulator word 0 (low 64 bits of the 256-bit accumulator). |

### Tile ALU (lane-parallel operations)

Each of these operates on every lane independently: `dst[i] = srcA[i] OP srcB[i]`.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TADD` | `( -- )` | `dst[lane] = src0[lane] + src1[lane]` for every lane. |
| `TSUB` | `( -- )` | `dst[lane] = src0[lane] − src1[lane]` for every lane. |
| `TAND` | `( -- )` | `dst[lane] = src0[lane] AND src1[lane]`. |
| `TOR` | `( -- )` | `dst[lane] = src0[lane] OR src1[lane]`. |
| `TXOR` | `( -- )` | `dst[lane] = src0[lane] XOR src1[lane]`. |
| `TEMIN` | `( -- )` | `dst[lane] = min(src0[lane], src1[lane])`. |
| `TEMAX` | `( -- )` | `dst[lane] = max(src0[lane], src1[lane])`. |
| `TABS` | `( -- )` | `dst[lane] = abs(src0[lane])` (signed mode). |

### Tile Multiply

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TMUL` | `( -- )` | `dst[lane] = src0[lane] × src1[lane]`. |
| `TDOT` | `( -- )` | Dot product: `ACC += Σ(src0[lane] × src1[lane])`.  Result in ACC. |
| `TWMUL` | `( -- )` | Widening multiply: 8b×8b→16b, 16b×16b→32b.  Output is double-width. |
| `TMAC` | `( -- )` | Multiply-accumulate: `dst[lane] += src0[lane] × src1[lane]`. |
| `TFMA` | `( -- )` | Fused multiply-add: `dst[lane] = src0[lane] × src1[lane] + dst[lane]`. |
| `TDOTACC` | `( -- )` | 4-way dot product accumulate: `ACC[k] += dot(chunk_k)` for k=0..3. |

### Tile Reductions (result → ACC)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSUM` | `( -- )` | `ACC = Σ src0[lane]` — sum all lanes. |
| `TMIN` | `( -- )` | `ACC = min(src0[lane])` — minimum across all lanes. |
| `TMAX` | `( -- )` | `ACC = max(src0[lane])` — maximum across all lanes. |
| `TPOPCNT` | `( -- )` | `ACC = Σ popcount(src0[lane])` — total bit count. |
| `TL1` | `( -- )` | `ACC = Σ |src0[lane]|` — L1 norm. |
| `TSUMSQ` | `( -- )` | `ACC = Σ src0[lane]²` — sum of squares. |
| `TMINIDX` | `( -- )` | Minimum with index: ACC0 = lane index, ACC1 = min value. |
| `TMAXIDX` | `( -- )` | Maximum with index: ACC0 = lane index, ACC1 = max value. |

### Tile System

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TTRANS` | `( -- )` | In-place 8×8 byte transpose of the tile at TDST. |
| `TZERO` | `( -- )` | Zero the 64-byte tile at TDST. |
| `TLOADC` | `( -- )` | Load tile from cursor address into TDST. |
| `TMOVBANK` | `( -- )` | Copy tile: `mem[TDST] ← mem[TSRC0]`. |

**Example — summing a 64-byte tile of data:**
```forth
0 TMODE!                  \ 8-bit unsigned lanes (64 lanes per tile)
my-data TSRC0!            \ point source at data
2 TCTRL!                  \ zero ACC before reduction
TSUM                      \ ACC = sum of all 64 bytes
ACC@ .                    \ print the result
```

### Full-width TACC

TACC is a persistent 2,048-bit lane accumulator attached to each physical
tile engine.  Full cores 0–3 each have a private engine and TACC.  The four
microcores in each of the three microclusters share that cluster's engine and
TACC, giving seven independent ownership domains in the production topology.
Claiming TACC reserves only its persistent state; it does not reserve the
tile engine from ordinary MEX work.

The BIOS exposes the ISA lifecycle directly:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TAMAC` | `( -- )` | Accumulate `TSRC0[i] * TSRC1[i]` into every TACC lane.  TACC must be owned, valid, and in the current `TMODE` format. |
| `TACC-TRY` | `( -- )` | Try to claim the local engine's TACC.  This always retires without waiting and returns no hidden flag. |
| `TACC-CLEAR` | `( -- )` | Require ownership, latch the current `TMODE` format, zero all lanes, and mark the state valid and dirty. |
| `TACC-LOAD` | `( -- )` | Require ownership and atomically load the canonical 256-byte image at `TSRC0`, latching the current format. |
| `TACC-STORE` | `( -- )` | Require valid owned state, store the canonical image at `TDST`, and clear `DIRTY` after the complete store succeeds. |
| `TACC-RELEASE` | `( -- )` | Require ownership, zeroize and invalidate TACC, then release it. |
| `TACC-STATUS@` | `( -- status )` | Read caller-relative TACC status CSR `0x1D`. |
| `TACC-CLAIM?` | `( -- flag )` | Execute `TACC-TRY` and return true exactly when status says `MINE`.  It never spins. |

`TACC-STATUS@` reports `CLAIMED`, caller-relative `MINE`, `VALID`, `DIRTY`,
`BUSY`, the latched element width and signedness, `FORCE_PENDING`, and the
absolute owner core ID.  A successful `TACC-TRY` establishes ownership but
not a valid value.  Initialize with `TACC-CLEAR` or `TACC-LOAD` before
`TAMAC` or `TACC-STORE`.  A full core uses the same explicit lifecycle as a
microcore even though its private claim cannot lose to another core.

`TACC-LOAD` and `TACC-STORE` always transfer four consecutive 64-byte beats
at a 64-byte-aligned address.  U8 and U16 modes use all 256 image bytes.
U32, FP16, and BF16 modes use bytes 0–127 and keep bytes 128–255 zero.
Integer results are widened U32 or U64 lanes; FP16/BF16 products accumulate
as binary32 lanes.  Save the latched format alongside a context image.
Task switches and traps do not release ownership, so ordinary software must
store and release explicitly; the privileged force-release CSR is recovery
for a dead owner.

Waiting policy remains visible in software.  This bounded helper yields
between failed claims and lets its caller choose the retry budget:

```forth
: TACC-CLAIM-N  ( attempts -- flag )
    0 DO
        TACC-CLAIM? IF TRUE UNLOOP EXIT THEN
        PAUSE
    LOOP
    FALSE ;
```

`TACC-CLAIM?` is idempotently true when the same core already owns TACC.
Tasks sharing a core must therefore track task ownership themselves rather
than treating it as a recursive lock.

This U8 kernel forms two products per lane and writes only the final widened
image.  Its four source tiles and 256-byte destination must be 64-byte
aligned:

```forth
: U8-2MAC  ( a0 b0 a1 b1 dst -- flag )
    TACC-CLAIM? 0= IF
        2DROP 2DROP DROP FALSE EXIT
    THEN
    >R
    0 TMODE!
    TACC-CLEAR
    TSRC1! TSRC0! TAMAC
    TSRC1! TSRC0! TAMAC
    R> TDST!
    TACC-STORE
    TACC-RELEASE
    TRUE ;
```

For example, source pairs filled with `2,3` and `4,5` produce 64 U32 result
lanes equal to 26.  There is no intermediate product or accumulator store.

The FP16 form has the same data movement but produces 32 binary32 lanes in
the first 128 output bytes:

```forth
: FP16-2MAC  ( a0 b0 a1 b1 dst -- flag )
    TACC-CLAIM? 0= IF
        2DROP 2DROP DROP FALSE EXIT
    THEN
    >R
    4 TMODE!
    TACC-CLEAR
    TSRC1! TSRC0! TAMAC
    TSRC1! TSRC0! TAMAC
    R> TDST!
    TACC-STORE
    TACC-RELEASE
    TRUE ;
```

FP16 pairs `1.0 * 2.0` and `0.5 * 4.0` produce binary32 `4.0`
(`0x40800000`) in every active lane; the upper 128 image bytes are zero.

---

## NIC / Networking (4 words)

Low-level access to the network interface controller.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NET-STATUS` | `( -- status )` | Read NIC status. Bit 1: RX available, bit 2: link, bit 3: error (sticky until `NET-RESET`), bit 4: RX DMA busy, bit 7: present. |
| `NET-SEND` | `( addr len -- )` | Send a frame: set DMA address + length, issue SEND command. |
| `NET-RECV` | `( addr -- actual )` | Receive a frame into `addr`; wait for RTL RX DMA completion before publishing the length. Returns 0 if nothing is available. |
| `NET-MAC@` | `( -- addr )` | Return the MMIO address of the six MAC bytes. |

---

## System & Miscellaneous (9 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BL` | `( -- 32 )` | Push the ASCII space character (blank). |
| `TRUE` | `( -- -1 )` | Push the canonical true flag (all bits set). |
| `FALSE` | `( -- 0 )` | Push the canonical false flag. |
| `LATEST` | `( -- addr )` | Address of the most recent dictionary entry header. |
| `ABORT` | `( -- )` | Clear both stacks and restart the outer interpreter. |
| `ABORT"` | `( flag "msg" -- )` | If flag is true, print the message and abort.  Immediate. |
| `TALIGN` | `( -- )` | Align HERE to the next 64-byte boundary. |
| `FSLOAD` | `( "filename" -- )` | **Disk boot word.**  Reads the MP64FS directory, validates the file extent and RAM span, transfers it in guarded batches, and EVALUATEs each line.  The standard image uses it for the KDOS core; KDOS `REQUIRE` owns later userland modules. |
| `EXIT` | `( -- )` | Return from the current word immediately. |

---

## Comments (2 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `\` | *rest of line* | Line comment — everything after `\` to end-of-line is ignored. |
| `(` | *...* `)` | Inline comment — everything between `(` and `)` is ignored.  Immediate. |

---

## Multicore (11 words)

These words provide inter-core communication for the quad-core SoC.
Secondary cores boot into a worker loop; the primary core dispatches
work to them via IPI (inter-processor interrupt) through the mailbox
device.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `COREID` | `( -- n )` | Push this core’s hardware ID (`0` through `NCORES - 1`). Reads CSR 0x20. |
| `NCORES` | `( -- n )` | Push the total number of hardware cores.  Reads CSR 0x21. |
| `IPI-SEND` | `( xt core -- )` | Send an IPI to *core*: writes the 64-bit XT into the mailbox data register and triggers the interrupt.  The target core’s IPI handler will EXECUTE the XT. |
| `IPI-STATUS` | `( -- mask )` | Read pending IPI bitmask for this core.  Bit *n* set means an IPI from core *n* is pending. |
| `IPI-ACK` | `( core -- )` | Acknowledge (clear) the pending IPI from the given core. |
| `MBOX!` | `( d -- )` | Write a 64-bit value to the mailbox outgoing data register (8 bytes LE). |
| `MBOX@` | `( -- d )` | Read the 64-bit value from the mailbox incoming data register. |
| `SPIN@` | `( n -- flag )` | Try to acquire spinlock *n*. Returns 0 if free or already owned by this physical core, 1 if owned by another core. |
| `SPIN!` | `( n -- )` | Release spinlock *n* only when this physical core owns it; a free or foreign-owned release is ignored. |
| `WAKE-CORE` | `( xt core -- )` | Convenience wrapper: pre-writes the XT into the shared worker table, then sends the IPI.  This ensures `CORE-STATUS` sees the pending work immediately. |
| `CORE-STATUS` | `( core -- n )` | Read the worker XT slot for a core.  Returns 0 if the core is idle, or the pending XT if it’s busy/dispatched. |

The bank contains 16 locks and records global physical-core identity, not a
task identity. Same-core acquisition is depthless: repeated `SPIN@` calls all
return 0, but one `SPIN!` releases the lock. The BIOS words do not validate
the lock number before forming their raw MMIO address, so only IDs 0 through
15 name the documented spinlock ABI; an out-of-range cell can fault or alias
another MMIO window rather than returning a portable status.

**Example — dispatching work to core 1:**
```forth
: my-work  42 mybuf B.FILL ;
' my-work 1 WAKE-CORE      \ send XT to core 1 via IPI
BEGIN 1 CORE-STATUS 0= UNTIL  \ wait until core 1 finishes
```

---

## Performance Counters (5 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PERF-CYCLES` | `( -- n )` | Read the cycle counter (CSR 0x68). |
| `PERF-STALLS` | `( -- n )` | Read the stall counter (CSR 0x69). |
| `PERF-TILEOPS` | `( -- n )` | Read the tile operation counter (CSR 0x6A). |
| `PERF-EXTMEM` | `( -- n )` | Read the external memory beat counter (CSR 0x6B). |
| `PERF-RESET` | `( -- )` | Reset and re-enable all performance counters (CSR 0x6C ← 3). |

---

## CRC Engine and Capability Discovery (9 words)

CRC computation uses EXT.CRYPTO `FB` instructions. Full cores have private
state; each micro-core cluster shares an accelerator protected by a hardware
transaction owner. `CRC-MODE!` begins a checked transaction without changing
the accumulator. Finalization releases it. CRC state lives in CSR 0x80
(CRC_ACC) and CSR 0x81 (CRC_MODE). Micro-cores may read those CSRs, but writes
are ignored; the BIOS words mutate shared state through owner-arbitrated CRC
instructions.

At boot, BIOS reserves and scrubs one 16-byte owner record for every global
core advertised by System Info. Each record holds the full `COREID` and
`TASK-ID`. Record checks, CRC instructions, publication, and cleanup run in
critical sections which restore the caller's exact interrupt-enable state.
This prevents another cooperative task on the same core from entering an
already-active transaction. The owner must still reach a final operation:
traps and `THROW` do not automatically release CRC state.

Modes 0, 1, and 2 are MSB-first CRC-32/BZIP2, non-reflected Castagnoli, and
CRC-64/WE. Modes 4, 5, and 6 are their LSB-first reflected counterparts and
require capability bit `CRC_REFLECT_RAW`. See the
[ISA reference](isa-reference.md) for the complete tuples and check vectors.

Checked status values are 0 OK, 1 UNSUPPORTED, 2 STATE/OWNER, and 3 RANGE.
`CRC@` and `CRC-RAW-FINAL@` return the value first and status on top.
`CRC-FINAL@` is deliberately result-only and returns zero on owner misuse.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRYPTO-CAPS@` | `( -- caps )` | Read the raw System Info `CRYPTO_CAPS` qword. |
| `CRC-MODE!` | `( mode -- status )` | Validate mode 0/1/2/4/5/6, check reflected capability when needed, and begin a checked transaction without changing CRC_ACC. |
| `CRC-RESET` | `( -- status )` | Require the exact owner and reset to the selected mode's all-ones default. |
| `CRC-INIT!` | `( seed -- status )` | Require the exact owner and set a mode-width seed. |
| `CRC-FEED` | `( cell -- status )` | Require the exact owner and feed 8 bytes, least-significant byte first. |
| `CRC-FEED-BYTE` | `( byte -- status )` | Require the exact owner and feed exactly the low byte. |
| `CRC@` | `( -- raw status )` | Return the running accumulator to the exact owner; misuse returns `0 2`. |
| `CRC-RAW-FINAL@` | `( -- raw status )` | Atomically return the unmodified accumulator and release. Capability absence returns `0 1` and releases an exact-owner non-reflected transaction through ordinary finalization. |
| `CRC-FINAL@` | `( -- finalized )` | Atomically XOR-finalize and release; misuse returns zero without touching hardware. |

---

## Memory BIST (5 words)

Built-in self-test for RAM (March C−, checkerboard, address-as-data patterns).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BIST-FULL` | `( -- )` | Start the documented full memory BIST (all three test patterns); current implementations differ as noted below. |
| `BIST-QUICK` | `( -- )` | Start quick BIST (March C− only). |
| `BIST-STATUS` | `( -- n )` | Read BIST status: 0=idle, 1=running, 2=pass, 3=fail. |
| `BIST-FAIL-ADDR` | `( -- n )` | Read first failing address (valid after fail). |
| `BIST-FAIL-DATA` | `( -- n )` | Read documented packed expected/actual data after failure; current implementations differ as noted below. |

> **Open BIST implementation discrepancy.** The public design describes a
> 1 MiB full test containing March C−, checkerboard, and address-as-data
> patterns. The pure-Python emulator runs those three patterns over its own
> bounded range, the current full-core RTL command selects one smaller pattern
> run over a much smaller interval, and the accelerated emulator reports pass
> without executing the destructive sweep. `BIST-FAIL-DATA` also differs:
> documented/Python packing retains expected and actual values, while current
> RTL retains only the observed read data. This records the mismatch without
> selecting one implementation as the final contract.

---

## Tile Self-Test (3 words)

Functional check of the tile engine datapath (~200 cycles).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TILE-TEST` | `( -- )` | Start tile datapath self-test. |
| `TILE-TEST@` | `( -- n )` | Read self-test status: 0=idle, 1=running, 2=pass, 3=fail. |
| `TILE-DETAIL@` | `( -- n )` | Read failed sub-test bitmask (for diagnostics). |

> **Open tile self-test discrepancy.** The public design describes an
> ADD/MUL/DOT/SUM datapath test and admits status 1 while it is running. The
> pure-Python emulator performs those four value tests synchronously, current
> full-core RTL counts down to pass without performing them, and the accelerated
> emulator passes immediately. Unchanged KDOS currently waits only while the
> status is 0, so an observable running status is rendered as failure. This note
> does not decide whether the source, ABI description, or implementations must
> change before an asynchronous self-test is admitted.

---

## Stride / 2D Addressing (6 words)

Strided and two-dimensional tile loads/stores for accessing non-contiguous
memory regions (e.g., extracting an 8×8 patch from a 640-wide framebuffer).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSTRIDE-R!` | `( n -- )` | Set row stride in bytes (CSR 0x40). |
| `TSTRIDE-R@` | `( -- n )` | Read current row stride. |
| `TTILE-H!` | `( n -- )` | Set tile height for 2D ops (CSR 0x42). |
| `TTILE-W!` | `( n -- )` | Set tile width for 2D ops (CSR 0x43). |
| `TLOAD2D` | `( -- )` | 2D strided load: read H rows of W bytes from `[TSRC0]`. |
| `TSTORE2D` | `( -- )` | 2D strided store: write H rows of W bytes to `[TDST]`. |

---

## FP16 / BF16 Modes (2 words)

Half-precision floating-point tile operations.  Reductions (SUM, DOT,
SUMSQ) use FP32 accumulation for numerical stability.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FP16-MODE` | `( -- )` | Set TMODE to FP16 half-precision (EW=4, 32 lanes). |
| `BF16-MODE` | `( -- )` | Set TMODE to bfloat16 (EW=5, 32 lanes). |

---

## AES-256/128-GCM Engine (11 words)

Authenticated encryption via the executable/native MMIO AES ABI at
`0xFFFF_FF00_0000_0700`.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `AES-KEY!` | `( addr -- )` | Load 256-bit key (32 bytes at addr) into AES engine. |
| `AES-IV!` | `( addr -- )` | Load 96-bit IV (12 bytes at addr) into AES engine. |
| `AES-AAD-LEN!` | `( n -- )` | Set additional authenticated data length (bytes). |
| `AES-DATA-LEN!` | `( n -- )` | Set plaintext/ciphertext data length (bytes). |
| `AES-CMD!` | `( cmd -- )` | Start operation: low bit 0 = encrypt, 1 = decrypt. |
| `AES-STATUS@` | `( -- status )` | Read status: 0 = idle, 1 = active, 2 = done, 3 = authentication or transaction failure. |
| `AES-KEY-MODE!` | `( n -- )` | Select key mode: low bit 0 = AES-256, 1 = AES-128. The current executable device still requires all 32 key bytes to be written. |
| `AES-DIN!` | `( addr -- )` | Feed input data block (16 bytes at addr) to engine. |
| `AES-DOUT@` | `( addr -- )` | Read output data block (16 bytes) from engine. |
| `AES-TAG@` | `( addr -- )` | Read 128-bit authentication tag (16 bytes) from engine. |
| `AES-TAG!` | `( addr -- )` | Write expected tag (16 bytes) for decryption verification. |

> **AES ABI discrepancy record.** The table above follows unchanged KDOS,
> `bios.asm`, and the native architectural device: the byte aperture is
> `+0x700..+0x76F`, key mode is at `+0x3A`, commands are 0/1, and statuses are
> 0/1/2/3. Naturally aligned 1-, 2-, 4-, and 8-byte native accesses are
> preflighted as complete spans and decomposed little-endian; the BIOS uses
> byte loops for key, IV, data, and tag transfers and 32-bit stores for the two
> lengths. Older tables published commands 1/2, statuses 0/1/2, a 64-byte
> aperture, or key mode at `+0x70`.
>
> Current integrated RTL is not compatible with that executable byte ABI. The
> SoC decodes `+0x700..+0x77F`, does not pass access size to the AES leaf, and
> the leaf recognizes mostly 32-bit register starts plus isolated command,
> status, and key-mode byte offsets, rather than one callback at every byte;
> the BIOS byte loops therefore do not configure or transfer the same register
> image. RTL also exposes a busy/done/auth-fail bitfield rather than statuses
> 0/1/2/3 and does not implement the executable AAD/length-finalization/tag-
> comparison and fail-closed transaction protocol. Published throughput and
> interrupt behavior remain unqualified RTL design targets, not current
> executable/native ABI guarantees. This records the mismatch without choosing
> which implementation must ultimately change.

---

## Caller-Managed Span Qualification (1 word)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CALLER-SPAN-STATUS` | `( addr len -- status )` | Purely qualify a complete caller-managed span before any higher-level read or write. |

The result is `0` OK, `2` RANGE, or `3` PROTECTED. Zero length is
unconditional OK and ignores the unused address. For a nonempty span, negative
address or length cells, null, address wrap, crossing an advertised physical
window, and an unadvertised range all return RANGE.

Bank 0 admits only `[kernel-data-end, caller-DSP-8)`. The boot-computed lower
bound follows the topology-sized CRC owner table, excluding it together with
the static BIOS/private footprint, live data and return stacks, and the status
result cell. External, HBW, and VRAM spans are admitted when they fit wholly
in the corresponding nonempty SysInfo window.

One conservative boundary serves both input reads and output writes because
it qualifies ordinary caller-manageable memory, not every byte the machine
could physically read. It is deliberately stricter than a read-only
accessibility test. Success does not establish allocation ownership,
mutability, initialization, lifetime, or freedom from higher-level aliases;
each caller must enforce those properties before using the span.

---

## SHA-256 Streaming (4 words)

`SHA256-*` uses EXT.CRYPTO mode 0 behind a checked, per-core BIOS
transaction. Each core has a private 256-byte context containing the four
packed digest qwords (eight 32-bit SHA words), a checked 64-bit bit length,
partial-block offset, dedicated 64-byte data block, and 32-byte publication
stage. Short interrupt-masked
engine windows preserve the caller's exact ACC0–ACC3, TSRC0, and
interrupt-enable state.

`UPDATE` validates the complete physical span before reading any byte,
rejects an intersection with either complete SHA-2 context arena, and rejects
a nonzero high length word or overflowing 64-bit bit-length addition. Both
`UPDATE` and `FINAL` also require a byte-aligned saved length whose
modulo-64-byte position exactly matches the saved partial-block offset.
`FINAL` validates and de-aliases all 32 destination bytes, stages the digest
while the engine is owned, issues `sha.release` after scrubbing, and only
then publishes. Boot, `CLEAR`, every failure, and successful `FINAL` erase
the calling core's complete context.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA256-INIT` | `( -- status )` | Reset this core's context to the SHA-256 IV. |
| `SHA256-UPDATE` | `( addr len -- status )` | Absorb an arbitrary validated Bank 0, external, HBW, or VRAM span, including zero-length and cross-block updates. |
| `SHA256-FINAL` | `( dst -- status )` | On success, write the 32-byte big-endian digest; always erase saved and staged state. |
| `SHA256-CLEAR` | `( -- status )` | Idempotently abort and zeroize context and visible SHA state, release the engine, and return zero. |
| `SHA2-SPAN-STATUS` | `( addr len -- status )` | Pure pre-`INIT` physical-window and union-of-SHA-2-arenas check; returns only 0, 2, or 3. |

Statuses are `0` OK, `1` STATE, `2` RANGE, `3` CONTEXT-ALIAS, and
`4` LENGTH-OVERFLOW. `INIT` is required before even a zero-length `UPDATE`;
the active marker must be exactly one. Every nonzero result aborts and wipes.
A failed `FINAL` leaves every non-context destination byte unchanged.

## SHA-512 Streaming (4 words)

`SHA512-*` uses EXT.CRYPTO mode 2 but does not expose the engine's
R16–R19-backed digest state across a Forth return. Each core has a private
512-byte BIOS context containing the eight digest words, the 128-bit message
length, partial-block offset, a dedicated 128-byte data block, and a 64-byte
publication stage. Every bounded engine window preserves the
caller's exact R16–R19, ACC0–ACC3, TSRC0, and interrupt-enable state.

`UPDATE` validates the complete physical span, rejects an intersection with
either complete SHA-2 context arena, and preflights the 128-bit bit-length
addition before copying any caller byte. `FINAL` validates and de-aliases all
64 destination bytes before entering mode 2, stages the digest while the
engine is owned, releases only after cleanup, and publishes afterward. The
contexts are erased on warm boot, on every checked failure, and after
successful `FINAL` or `CLEAR`.

Before even a zero-length `UPDATE` can succeed, and before `FINAL` validates
the destination, the saved active marker must be exactly one, the partial
offset must be below 128, the low bit length must be byte-aligned, and
`((low >> 3) & 127)` must equal the saved offset. Any mismatch returns STATE
and aborts the context.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA512-INIT` | `( -- status )` | Reset this core's context to the SHA-512 IV. |
| `SHA512-UPDATE` | `( addr len -- status )` | Absorb an arbitrary validated Bank 0, external, HBW, or VRAM span, including zero-length and cross-block updates. |
| `SHA512-FINAL` | `( dst -- status )` | On success, write the 64-byte big-endian digest; always erase saved and staged state. |
| `SHA512-CLEAR` | `( -- status )` | Idempotently abort and zeroize context and visible SHA state, release the engine, and return zero. |

| Status | Name | Meaning |
|--------|------|---------|
| `0` | OK | Operation completed. |
| `1` | STATE | `UPDATE` or `FINAL` was called without an active `INIT`. |
| `2` | RANGE | The complete caller span is not in one advertised physical-memory window. |
| `3` | CONTEXT-ALIAS | The caller span intersects any core's private SHA-512 context. |
| `4` | LENGTH-OVERFLOW | Absorbing the span would wrap the 128-bit bit length. |

`INIT` is required even before a zero-length `UPDATE`. Every nonzero failure
aborts and wipes the active context. A failed `FINAL` publishes no digest and
does not modify a non-context destination. Streaming contexts are core-local
and must be updated, finalized, or cleared on their originating core.

> **Open native/RTL SHA-2 instruction discrepancy.** The checked BIOS above
> produces standard SHA-256/SHA-512 through the Python and native executable
> models, but the current RTL instruction glue is not equivalent. Full-core
> and cluster RTL make `SHA.PAD`/`SHA.FINAL` data-path no-ops even though BIOS
> relies on `SHA.FINAL` to pad; their `SHA.DOUT` selects from the encoded
> register field rather than `R[Rs] & 7`; `SHA.DIN` writes an accumulator
> qword rather than feeding one buffer byte; and their ROUND memory loaders do
> not perform the required little-endian-memory to big-endian-word conversion.
> The RTL SHA leaf also leaves SHA-384/512 as future work. Existing RTL tests
> bypass these seams with pre-padded/endian-correct words or test ownership
> only. This note records the split without choosing the eventual RTL, BIOS,
> or public-ISA correction; hosted semantic execution follows the working
> BIOS/native result and is not RTL evidence.

---

## Checked SHA-3 / SHAKE / raw Keccak (9 words)

The checked SHA aperture is exactly 96 bytes at
`0xFFFF_FF00_0000_0780..0xFFFF_FF00_0000_07DF`. `CRYPTO_CAPS` bit 1
advertises SHA3/SHAKE streaming and bit 2 advertises raw Keccak-f[1600]; both
are set at checkpoint 2. Hash modes are 0=SHA3-256, 1=SHA3-512, 2=SHAKE128,
and 3=SHAKE256.

The aperture contains byte CMD `+0x00`, byte STATUS `+0x01`, byte CTRL
`+0x02`, byte ERROR `+0x03`, byte DIN `+0x08`, the stable 64-byte DOUT window
`+0x10..+0x4F`, byte STATE_INDEX `+0x50`, and the selected little-endian lane
at STATE_DATA `+0x58..+0x5F`. DOUT and STATE_DATA permit aligned qword access;
other defined accesses are byte-wide. Commands are exactly 1 INIT, 3 FINAL,
4 NEXT, 6 KECCAK_F1600, and 7 CLEAR; 0, 2, 5, and 8..255 are reserved.
STATUS packs phase (0 IDLE, 1 BUSY, 2 DONE, 3 ERROR) in bits 1:0 and owner
(0 none, 1 sponge, 2 raw, 3 WOTS) in bits 3:2.

All checked words use `0` OK, `1` UNSUPPORTED, `2` STATE/OWNER, `3` RANGE,
`4` PROTECTED, `5` TIMEOUT, and `6` HARDWARE/PROTOCOL. Capability checks
precede argument checks. Complete caller spans are qualified before device or
destination mutation, and every destination-returning word stages its result
before publication.

Hardware spinlock 8 is reserved as `CRYPTO-LOCK`: acquire is at spinlock
offset `+0x20` (absolute MMIO `+0x620`) and release at `+0x21`. The BIOS also
publishes the full owning `(COREID,TASK-ID)` in shared fields. Acquire,
owner-field publication, cleanup, and release preserve the caller's exact
interrupt-enable state, so another task on the same physical core cannot
re-enter the transaction. SHAKE retains the guard across calls; callers do
not wrap checked words in `SPIN@`/`SPIN!` themselves.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA3-BEGIN` | `( mode -- status )` | Validate capability and mode, acquire the portable guard, program `CTRL`, and issue `INIT`. |
| `SHA3-UPDATE` | `( src len -- status )` | Require the exact owner and absorb a complete caller-readable span. A zero length ignores `src`. |
| `SHA3-FINAL` | `( dst -- status )` | Fixed modes only: stage 32/64 digest bytes, clear and scrub hardware, publish, wipe scratch, and release. |
| `SHA3-STATUS@` | `( -- status )` | Diagnostic packed hardware status: phase in bits 1:0, owner in bits 3:2. It neither acquires nor advances the guard. |
| `SHAKE-FINAL` | `( -- status )` | SHAKE modes only: finalize and set the logical output cursor to zero while retaining ownership. |
| `SHA3-MODE@` | `( -- mode )` | Diagnostic raw `CTRL` read; it neither acquires nor advances the guard. |
| `SHAKE-READ` | `( dst len -- status )` | Publish the next 0..32 XOF bytes. BIOS tracks a cursor over stable 64-byte hardware windows and issues `NEXT` only when needed. |
| `SHA3-CLEAR` | `( -- status )` | Idempotently abort/acknowledge, wipe, and release. A clear timeout returns 5 and deliberately retains the guard fail-closed. |
| `KECCAK-F1600` | `( state-200 -- status )` | In-place raw 24-round permutation of one qualified caller-owned 200-byte state. |

`SHA3-FINAL` is not a SHAKE finalizer, and `SHAKE-FINAL` is not a fixed hash
finalizer; a mode/phase mismatch returns STATE/OWNER and clears the owned
transaction. `SHAKE-READ` accepts at most 32 bytes even though the hardware
publishes a stable 64-byte `DOUT` window. Fixed SHA3-512 finalization may
stage all 64 digest bytes. SHAKE clients must finish with `SHA3-CLEAR` on
both success and handled failure paths.

`KECCAK-F1600` maps the caller image directly to the 25 little-endian lanes:

```text
lane = x + 5*y
memory[8 * lane + b] = state[lane][8*b +: 8]
```

It applies no absorb, padding, domain separator, byte reversal, or squeeze.
All 200 result bytes are staged, hardware is cleared, and only then is the
caller image overwritten; any failure leaves it unchanged.

The unreleased transaction/prototype words `SHA3-INIT`, `SHA3-MODE!`,
`SHA3-SQUEEZE`, `SHA3-SQUEEZE-NEXT`, `SHA3-DOUT@`, `WOTS-CHAIN-HW`,
`SHA3-LOCKED?`, and `WOTS-STATUS@` were removed without aliases.

---

## Checked WOTS Chain (1 word)

The WOTS accelerator is the exact byte-only 32-byte aperture
`0xFFFF_FF00_0000_08A0..0xFFFF_FF00_0000_08BF`:

| Offset | Register | Access |
|--------|----------|--------|
| `+0x00..+0x07` | 64-bit little-endian `CONTEXT_ADDR` | read/write bytes |
| `+0x08` | `STEPS` | read/write byte |
| `+0x09` | `START` | read/write byte |
| `+0x0A` | `CMD` / `STATUS` | write/read byte |
| `+0x0B` | `ERROR` | read byte |
| `+0x0C..+0x0F` | saturating diagnostic `CYCLES` | read bytes |
| `+0x10..+0x1F` | stable 16-byte `DOUT` | read bytes |

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `WOTS-CHAIN` | `( context-64 start steps dst-16 -- status )` | Run one checked WOTS chain, stage its complete result, clear hardware, and only then publish the 16 bytes with ordinary byte stores. |

`context-64` is exactly 16 bytes of `PK.seed`, 32 bytes of ADRS, and the
16-byte input node. It must be one complete nonwrapping caller-readable Bank 0
span. `dst-16` follows the common caller-writable-span policy and may overlap
the context. `start` and `steps` are each 0..15; for nonzero work their widened
sum is at most 15. A zero-step call still performs all 64 ascending DMA reads
and returns the staged input node without claiming the shared Keccak service.

The word uses the common checked statuses: `0` OK, `1` UNSUPPORTED, `2`
STATE/OWNER, `3` RANGE, `4` PROTECTED, `5` TIMEOUT, and `6`
HARDWARE/PROTOCOL. Capability and all scalar/span checks precede both guard
and device access. It then makes one nonblocking spinlock-8 attempt, records
owner kind 3, programs only byte registers, and polls with the calling core's
64-bit `PERF_CYCLES` counter. BIOS derives the request and clear bounds from
System Info `NUM_BUS_PORTS`, enables `PERF_CTRL` without resetting counters,
and restores the caller's saved enable bit on every post-enable return.

On `DONE`, BIOS stages all 16 DOUT bytes, issues `CLEAR`, and publishes only
after `STATUS` reaches `IDLE` within the independent clear deadline. Every
failure leaves all destination bytes unchanged. A failed or late clear wipes
the complete private staging area, restores `PERF_CTRL`, and retains both the
software owner and spinlock 8 fail-closed until machine reset. The complete
timed interval contains no `PAUSE`, `TASK-YIELD`, or other scheduler call.

---

## TRNG (5 words)

Hardware true random number generator at `0xFFFF_FF00_0000_0800`.
The shared device exposes an exact `USABLE` bit and fails closed: random-data
reads raise a bus fault when disabled, unhealthy, or unable to refill.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `RANDOM` | `( -- u )` | Return a 64-bit random number; propagate a TRNG bus fault when unusable. |
| `RANDOM8` | `( -- u )` | Return an 8-bit random number (0–255); propagate a TRNG bus fault when unusable. |
| `SEED-RNG` | `( u -- )` | Supplement unread/future entropy bytes when usable; never restores an unusable source. |
| `ENTROPY-FILL` | `( addr len -- status )` | Checked, bounded entropy fill; preflight the complete destination and wipe it after a detected post-start health loss. |
| `ENTROPY-READY?` | `( -- flag )` | Canonical true only when the hardware status byte is exactly one; false otherwise. |

`ENTROPY-FILL` returns `0` OK, `1` UNAVAILABLE, `2` RANGE, or `3`
PROTECTED. Length cells must be nonnegative. Zero length is an unconditional
no-op, including `(0,0)`, and its unused address is ignored. A nonempty
address must be nonnegative; null with a nonzero length is RANGE.
A nonempty destination must fit wholly and without wrap in one advertised
Bank 0, external, HBW, or VRAM physical window. Bank 0 is further limited to
`[kernel-data-end, caller-DSP-8)`, excluding the complete static BIOS/private
footprint, the dynamic CRC owner table, every live stack byte, and the future
status cell. This is a
protection boundary, not an allocation-ownership check; callers remain
responsible for supplying a buffer they manage. The word applies this policy
through the shared `CALLER-SPAN-STATUS` implementation before its first
device read.

The word reads `STATUS` before every `RAND8` and after the last byte, and
accepts only the exact value one. Initial unavailability writes nothing. If a
health loss is observed after writing begins, the entire qualified destination
is zeroed before returning UNAVAILABLE. The implementation keeps no
caller-spanning transaction state. `ENTROPY-READY?` provides the same exact
readiness check without exposing the MMIO register address to callers.

The one data-read instruction private to `ENTROPY-FILL` has a PC-scoped
bus-fault recovery point. Loss of usability between a successful status read
and that immediately following `RAND8` therefore rejoins the same
UNAVAILABLE path, including complete-span wipe after publication begins.
Unrelated faults remain diagnostic, and a health transition caused by a
successfully returned byte is still detected by the next status read (or the
mandatory final read).

---

## X25519 — RFC 7748 Scalar Multiplication (6 raw words)

Per-core Field-ALU state exposes X25519 through EXT.CRYPTO `FB 2D`:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `X25519-SCALAR!` | `( addr -- )` | Load four ascending little-endian qwords into ACC0–ACC3. |
| `X25519-POINT!` | `( addr -- )` | Set TSRC0 to the deferred 32-byte point address without reading it. |
| `X25519-GO` | `( -- )` | Clamp the scalar in ACC, mask the point's top bit, run Curve25519 synchronously, and replace ACC. |
| `X25519-WAIT` | `( -- )` | No-op because the ISA operation completes synchronously. |
| `X25519-STATUS@` | `( -- 2 )` | Return 2 unconditionally, before or after a computation. |
| `X25519-RESULT@` | `( addr -- )` | Store ACC0–ACC3 as four ascending little-endian qwords. |

This is deliberately a raw, status-free ABI. It has no capability bit,
software owner, hardware lock, complete-span check, cleanup, or all-zero
shared-secret rejection. ACC0–ACC3 and TSRC0 belong to the physical core and
are shared by its tasks. Scalar loads and result stores mutate one qword at a
time, so a later fault can leave a prefix changed; `GO` reads all point qwords
before replacing ACC. Ordinary unaligned memory is valid, and output may
alias an already consumed scalar or point.

The RFC calculation uses `A24=121665` with `E*(AA+A24*E)`. Native C++ and the
standalone Field-ALU RTL implement that value. The architectural Python
emulator currently uses `121666` with the same formula and fails the published
RFC vector. Current SoC RTL also does not connect the standalone Field engine
to either executing core path. These are open backend discrepancies; hosted
X25519 qualification is not evidence that those paths work.

---

## Field ALU — Multi-Prime Arithmetic (15 raw words)

The per-core Field unit is exposed through EXT.CRYPTO `FB 20`–`FB 2D`.
Every `a`, `b`, `e`, `r`, `rlo`, `rhi`, `p`, and `pinv` argument below is an
address. Field values are 32-byte little-endian integers; raw products use two
separate 32-byte low/high destinations.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `GF-A!` | `( a-addr -- )` | Load four ascending qwords into ACC0–ACC3. |
| `GF-R@` | `( r-addr -- )` | Store ACC0–ACC3 as four ascending qwords. |
| `GF-PRIME` | `( selector -- )` | Select by the low two bits: 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom. |
| `LOAD-PRIME` | `( p-addr pinv-addr -- )` | Latch custom `p` and Montgomery `-p^-1 mod 2^256`; it does not select custom mode. |
| `FADD` | `( a-addr b-addr r-addr -- )` | Add under the selected prime. |
| `FSUB` | `( a-addr b-addr r-addr -- )` | Subtract under the selected prime. |
| `FMUL` | `( a-addr b-addr r-addr -- )` | Multiply under the selected prime. |
| `FSQR` | `( a-addr r-addr -- )` | Square under the selected prime. |
| `FINV` | `( a-addr r-addr -- )` | Compute `a^(p-2) mod p`. |
| `FPOW` | `( a-addr e-addr r-addr -- )` | Compute `a^e mod p`. |
| `FMUL-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Publish the raw 256×256 product as low and high halves. |
| `FCMOV` | `( a-addr cond-addr -- )` | Read `cond-addr C@`; if nonzero replace ACC with `a`, otherwise retain ACC. The `a` span is read in either case. |
| `FCEQ` | `( a-addr b-addr r-addr -- )` | Store a 256-bit 1 when equal, otherwise 0. |
| `FMAC` | `( a-addr b-addr r-addr -- )` | Add the retained previous low result to the selected product. |
| `FMUL-ADD-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Add the product to the retained 512-bit previous result, wrapping at 512 bits. |

ACC, TSRC0, TDST, prime configuration, and previous low/high results belong to
the physical core and are shared by its tasks. Ordinary result operations
replace previous-low only; raw operations replace both halves. `GF-A!`,
`GF-R@`, prime selection, and `LOAD-PRIME` do not themselves publish a
previous result. Loads and stores proceed one qword at a time, so a later
fault may retain an ACC or destination prefix. `FCEQ` sets the raw instruction
Z flag, but `_gf_store_acc` executes flag-writing `ADDI` instructions before
the Forth word returns; callers must use the stored 1/0 result.

For valid primes and canonical field inputs, the modular words have their
displayed mathematical meaning. Custom parameters are not validated. Current
C++ and standalone RTL subtract at most one `p` for ADD, while the Python
emulator uses full `% p`; SUB has further noncanonical backend differences.
The native C++ `BigNum` can also retain non-architectural upper limbs from
such an add/subtract into a later `FMAC`. For raw MAC carry, the hosted model,
Python emulator, and standalone RTL implement wrapped 512-bit addition, while
native C++ currently misses a carry from bit 255 into the high half. These are
open backend defects, not alternate public ABIs. No hosted execution path
claims constant-time behavior.

---

## NTT Engine (10 raw words)

256-point Number Theoretic Transform accelerator at
`0xFFFF_FF00_0000_08C0`. The working BIOS plus architectural Python device
retain a configurable uint64 modulus (normally 3329 or 8380417), two
256-entry uint32 input buffers, one result buffer, a 16-bit index, and
idle/busy/done state.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NTT-SETQ` | `( q -- )` | Set modulus (3329 or 8380417). |
| `NTT-IDX!` | `( idx -- )` | Set the raw 16-bit coefficient index. |
| `NTT-LOAD` | `( addr buf -- )` | Load 256 uint32-LE coefficients; `buf=0` selects A and every nonzero value selects B. |
| `NTT-STORE` | `( addr -- )` | Store 256 uint32-LE result coefficients. |
| `NTT-FWD` | `( -- )` | Run the generic forward transform on A. |
| `NTT-INV` | `( -- )` | Run the generic inverse transform on A. |
| `NTT-PMUL` | `( -- )` | Pointwise multiply retained A and B modulo q. |
| `NTT-PADD` | `( -- )` | Pointwise add retained A and B modulo q. |
| `NTT-STATUS@` | `( -- status )` | Read engine status. |
| `NTT-WAIT` | `( -- )` | Poll until DONE bit 1 is set; calling it while idle does not return. |

The executable device starts at status 0. Commands use bytes 1, 3, 5, and 7
(`go` in bit 0, operation in bits 2:1), complete synchronously, and retain
status 2. Loading or storing resets IDX and transfers four bytes per
coefficient. A load publishes a coefficient and advances only after byte 3;
a result read advances before the corresponding byte-3 destination write.
There is no complete-span preflight, lock, owner, capability bit, checked
error status, automatic wipe, or unwind cleanup. Later memory faults therefore
retain completed coefficients, staging/destination prefixes, and the exact
current index.

The Python device chooses a primitive 256th root for the selected q and
implements an ordinary radix-2 transform. Consequently
`INTT(NTT(a)*NTT(b))` is cyclic convolution modulo `x^256-1`, not the
negacyclic multiplication required by ML-KEM or ML-DSA. The KEM device uses
separate ML-KEM-specific polynomial routines. Invalid or composite moduli are
outside the portable contract; a modulus without a selected root completes a
command without replacing the prior result, while q=0 faults on coefficient
commit in the current Python model.

> **Executable/RTL discrepancy.** Native accelerated execution has no C++ NTT
> algorithm and delegates this MMIO range to the Python device. That working
> byte map is STATUS `+00`, Q `+08..0F`, IDX `+10..11`, A `+18..1B`, B
> `+1C..1F`, RESULT `+20..23`, and CMD `+28`. Current RTL instead decodes
> 64-bit slots with CMD/STATUS `+00`, 32-bit Q `+08`, 8-bit IDX `+10`, A
> `+18`, and B/RESULT `+20`; it also retains q=3329 twiddles and inverse scale
> when Q changes. BIOS byte accesses therefore cannot operate that RTL path,
> and its multi-cycle BUSY behavior is not evidence for executable or hosted
> timing. This record does not choose the eventual hardware/API correction.

---

## KEM Engine — ML-KEM-512 (7 words)

The authoritative raw dictionary surface drives the executable Python KEM
device at `0xFFFF_FF00_0000_0900`. The caller supplies all key-generation and
encapsulation randomness; these words do not obtain entropy from the TRNG.
Operands and results live in five retained device buffers: 0=SEED/COIN (64
bytes), 1=PK (800 bytes), 2=SK (1,632 bytes), 3=CT (768 bytes), and 4=SS (32
bytes).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KEM-SEL!` | `( n -- )` | Select a retained buffer and reset its byte index. The executable device takes the low byte and clamps values above 4 to 4. |
| `KEM-LOAD` | `( addr count -- )` | Copy exactly *count* caller bytes to the selected buffer through DIN. |
| `KEM-STORE` | `( addr count -- )` | Copy exactly *count* DOUT bytes from the selected buffer to caller memory. |
| `KEM-KEYGEN` | `( -- )` | Replace PK and SK from the retained 64-byte `d || z` seed. |
| `KEM-ENCAPS` | `( -- )` | Replace CT and SS from retained PK and the first 32 SEED/COIN bytes. |
| `KEM-DECAPS` | `( -- )` | Replace SS from retained CT and SK. |
| `KEM-STATUS@` | `( -- n )` | Read the retained status byte: 0 initially, 2 after a completed command. |

The executable Python byte window is the half-open range
`[0xFFFF_FF00_0000_0900, 0xFFFF_FF00_0000_0928)`:

| Offset | Register | Access | Executable behavior |
|--------|----------|--------|---------------------|
| `+0x00` | STATUS | R byte | 0=idle, 2=done |
| `+0x01` | CMD | W byte | 1=KEYGEN, 2=ENCAPS, 3=DECAPS |
| `+0x08` | BUF_SEL | W byte | Select 0..4 and reset the retained index |
| `+0x10` | DIN | W byte | Write one selected-buffer byte and auto-increment while in bounds |
| `+0x18` | DOUT | R byte | Read one selected-buffer byte and auto-increment while in bounds |
| `+0x20..+0x21` | BUF_SIZE | R bytes | Selected capacity as uint16 little-endian |

Commands complete within the triggering Python write, so executable code does
not observe BUSY=1. DONE remains set across selection and transfer operations
until device reset; starting another command replaces the appropriate outputs
and leaves DONE set. Selection resets only the index. Short loads retain the
old suffix of a buffer. At capacity the executable index pins: excess DIN is
dropped and excess DOUT returns zero. The raw transfer loops do no complete-span
preflight. LOAD reads each caller byte before attempting DIN, and STORE reads
DOUT before writing the corresponding caller byte, so a failing destination
write has already consumed that device byte. There is no lock, requester
owner, capability check, transactional rollback, automatic wipe, or Forth
unwind cleanup; all callers share the buffers and status.

KDOS declares `32 CONSTANT KEM-SEED-SIZE`, but `KYBER-KEYGEN` explicitly
loads 64 bytes and the executable SEED buffer/key-generation primitive consumes
64 bytes as `d || z`. `KYBER-ENCAPS` uses 32 bytes from that same buffer as its
coin input. This is a recorded source/API discrepancy; this document does not
choose whether the constant or the key-generation interface should change.

For generated/well-formed keys, the deterministic zero-`d || z`, zero-coin
fixture produces byte-identical keys, ciphertext, and shared secret to the
locally audited OpenSSL 3.5.2 ML-KEM-512 implementation. That is interoperability
evidence for the valid-key value path, not FIPS 203 certification. The Python
implementation accepts merely length-correct noncanonical public keys and
secret keys with inconsistent embedded hashes that OpenSSL rejects, uses a
fixed 840-byte SHAKE rejection-sampling window rather than an unbounded stream,
and is ordinary non-constant-time Python. Retained buffers and host allocations
are not a protected secret boundary and are not physically erased; do not use
this service to protect host secrets.

> **Executable/RTL discrepancy.** The current RTL block has a different
> 64-bit-slot interface: CMD-write/STATUS-read share `+0x00`, BUF_SEL is
> `+0x08`, DIN-write/DOUT-read share `+0x10`, IDX_SET-write/BUF_SIZE-read share
> `+0x18`, and IDX is readable at `+0x20`. It exposes BUSY during a multi-cycle
> FSM and fills outputs with deterministic XOR test data, not ML-KEM. In
> particular, the checked-in BIOS reads executable DOUT at `+0x18`, which is
> BUF_SIZE on RTL. RTL index-overrun and out-of-range-selector behavior also
> differ. The RTL is interface-stub evidence only; its timing and values do not
> qualify the executable Python device or hosted simulator, and those paths do
> not qualify the RTL.

---

## Cooperative Multitasking (9 words)

Lightweight four-task cooperative multitasker.  R20 (REX-extended) holds
the task trampoline PC; `SEP R20` round-robin yields across active task
slots, `SEP R3` returns to Task 0.  Each task has independent data and
return stacks.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PAUSE` | `( -- )` | Round-robin yield across all 4 task slots via `SEP R20`. |
| `TASK-YIELD` | `( -- )` | Yield from the current background task back to Task 0 via `SEP R20`. |
| `BACKGROUND` | `( xt -- )` | Set xt as the Task 1 body and start it running. |
| `TASK-STOP` | `( n -- )` | Stop background task in slot n (1–3), reset to idle. |
| `TASK?` | `( n -- flag )` | Return 0 if task slot n (1–3) is idle, 1 if running. |
| `BACKGROUND2` | `( xt -- )` | Set xt as the Task 2 body and start it running. |
| `BACKGROUND3` | `( xt -- )` | Set xt as the Task 3 body and start it running. |
| `#TASKS` | `( -- n )` | Count active background tasks (0–3). |
| `TASK-ID` | `( -- n )` | Return the executing cooperative slot on core 0 (0 foreground, 1–3 background); worker cores return 0. |

---

## Boot Sequence

When the Megapad-64 powers on:

1. **CPU reset** — all registers zeroed, PC = R3 = address 0,
   SP = R15 = top of RAM
2. **BIOS starts** — initializes the interrupt vector table, configures
   the UART, sets up the Forth dictionary (HERE, LATEST, etc.)
3. **Disk check** — if a storage device is present (bit 7 of `DISK@`),
   the BIOS validates MP64FS and scans for the first Forth-type file
4. **KDOS core load** — in the standard image that file is `kdos.f`; the
   BIOS reads and evaluates it through `FSLOAD`, compiling the core into
   Bank 0
5. **KDOS startup** — the core loads MP64FS, initializes its heap, and runs
   `autoexec.f` from the filesystem
6. **Standard userland load** — autoexec enters the XMEM userland dictionary,
   loads `networking.f` with KDOS `REQUIRE`, configures networking, and loads
   `tools.f`
7. **REPL** — the Forth outer interpreter (`QUIT`) runs, accepting
   input from the UART and executing/compiling words

If no disk is attached, the BIOS drops straight into the REPL after
step 2, ready for Forth input over the UART (or via the `--forth` CLI
flag for core-only file injection).

---

## Dictionary Layout

Each dictionary entry has this structure in memory:

```
┌──────────────┬───────────────┬──────────────────┬─────────────┐
│  link (8 B)  │ flags+len (1B)│  name (N bytes)  │ code body   │
└──────────────┴───────────────┴──────────────────┘─────────────┘
```

- **link** — 8-byte pointer to the previous dictionary entry (0 = end)
- **flags+len** — 1 byte: bit 7 = immediate flag, bits 6:0 = name length
- **name** — N bytes of the word's name (not null-terminated)
- **code body** — the compiled machine code for this word

`LATEST` points to the most recent entry.  Walking the link chain from
`LATEST` traverses the entire dictionary (this is how `FIND` and `WORDS`
work).
