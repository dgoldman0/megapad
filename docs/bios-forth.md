# BIOS Forth Word Reference

The Megapad-64 BIOS implements a **subroutine-threaded Forth** directly in
assembly.  It boots from address zero, initializes hardware, and presents a
standard Forth REPL over the UART.  If a disk is attached it will
automatically attempt `FSLOAD autoexec.f` to bootstrap the operating system.

This document catalogs every word in the BIOS dictionary — **197 entries** —
organized by functional category.  Each entry shows the **stack effect**
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

## Arithmetic (16 words)

All arithmetic operates on **64-bit** values.  Division by zero triggers a
CPU trap (vector `IVEC_DIV_ZERO`).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `+` | `( a b -- a+b )` | Addition. |
| `-` | `( a b -- a-b )` | Subtraction (a minus b). |
| `*` | `( a b -- a*b )` | Signed multiplication (low 64 bits). |
| `/` | `( a b -- a/b )` | Signed integer division.  Traps on b=0. |
| `MOD` | `( a b -- a%b )` | Signed modulo. |
| `/MOD` | `( a b -- rem quot )` | Signed division with remainder. |
| `NEGATE` | `( n -- -n )` | Two's-complement negation. |
| `ABS` | `( n -- |n| )` | Absolute value. |
| `MIN` | `( a b -- min )` | Smaller of two signed values. |
| `MAX` | `( a b -- max )` | Larger of two signed values. |
| `1+` | `( n -- n+1 )` | Increment by one. |
| `1-` | `( n -- n-1 )` | Decrement by one. |
| `2+` | `( n -- n+2 )` | Increment by two. |
| `2-` | `( n -- n-2 )` | Decrement by two. |
| `CELLS` | `( n -- n*8 )` | Convert a cell count to bytes (cells are 8 bytes). |
| `CELL+` | `( addr -- addr+8 )` | Advance an address by one cell (8 bytes). |

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

## Input & Output (17 words)

I/O goes through the UART.  `EMIT` sends one byte; `KEY` waits for one byte.
The number-printing words all produce human-readable ASCII output.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `EMIT` | `( c -- )` | Send one character to the UART. |
| `KEY` | `( -- c )` | Wait for and return one character from the UART. |
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
| `BYE` | `( -- )` | Halt the CPU (exit Forth). |
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
| `S"` | *compile:* `( -- addr len )` | Compile an inline string literal.  At runtime, pushes the address and length.  Use inside a `: ... ;` definition. |
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

## Compilation & Defining Words (19 words)

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
| `EXECUTE` | `( xt -- )` | Call the word whose execution token is on the stack. |
| `FIND` | `( c-addr -- xt flag \| c-addr 0 )` | Search the dictionary for a counted string.  Returns the xt and a flag (+1 immediate, −1 normal) or 0 if not found. |
| `STATE` | `( -- addr )` | Address of the compiler state variable (0 = interpreting, non-zero = compiling). |
| `LATEST` | `( -- addr )` | Address of the most recent dictionary entry.  Useful for dictionary traversal. |

---

## Input Source & Evaluation (5 words)

These words control where Forth reads its input from and how it processes
text.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SOURCE` | `( -- addr len )` | Address and length of the current input buffer. |
| `>IN` | `( -- addr )` | Address of the variable holding the current parse position within `SOURCE`. |
| `EVALUATE` | `( addr len -- )` | Interpret the given string as Forth source code.  Temporarily redirects the input source. |
| `>NUMBER` | `( ud addr len -- ud' addr' len' )` | Convert characters to a number, accumulating into *ud*.  Stops at the first non-digit. |
| `QUIT` | `( -- )` | Clear the return stack and enter the outer interpreter loop (the REPL).  Does not return. |

**Example — dynamic evaluation:**
```forth
S" 2 3 + ." EVALUATE    \ prints 5
```

---

## Disk I/O (6 words)

Low-level sector-based disk access.  Each sector is **512 bytes**.
These words talk directly to the storage controller MMIO registers.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DISK@` | `( -- status )` | Read the storage controller status register.  Bit 7 = device present. |
| `DISK-SEC!` | `( sector -- )` | Set the sector number for the next disk operation. |
| `DISK-DMA!` | `( addr -- )` | Set the DMA address (where data will be read to or written from). |
| `DISK-N!` | `( n -- )` | Set the number of sectors to transfer. |
| `DISK-READ` | `( -- )` | Issue a read command.  Transfers sector(s) from disk to RAM at the DMA address. |
| `DISK-WRITE` | `( -- )` | Issue a write command.  Transfers sector(s) from RAM to disk. |

**Example — reading sector 10 into a buffer:**
```forth
CREATE SECBUF 512 ALLOT
10 DISK-SEC!
SECBUF DISK-DMA!
1 DISK-N!
DISK-READ
SECBUF 512 DUMP
```

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

## Tile Engine (32 words)

The tile engine (MEX extension) performs **SIMD operations** on 64-byte
memory tiles.  Tiles are divided into lanes based on element width
(64×8-bit, 32×16-bit, 16×32-bit, or 8×64-bit).

### CSR Access

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TMODE!` | `( mode -- )` | Set the element width and signedness.  Low 2 bits = width (0=8b, 1=16b, 2=32b, 3=64b).  Bit 4 = signed. |
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
| `TMIN` | `( -- )` | `dst[lane] = min(src0[lane], src1[lane])`. |
| `TMAX` | `( -- )` | `dst[lane] = max(src0[lane], src1[lane])`. |
| `TABS` | `( -- )` | `dst[lane] = abs(src0[lane])` (signed mode). |

### Tile Multiply

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TMUL` | `( -- )` | `dst[lane] = src0[lane] × src1[lane]`. |
| `TDOT` | `( -- )` | Dot product: `ACC += Σ(src0[lane] × src1[lane])`.  Result in ACC. |

### Tile Reductions (result → ACC)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TSUM` | `( -- )` | `ACC = Σ src0[lane]` — sum all lanes. |
| `TEMIN` | `( -- )` | `ACC = min(src0[lane])` — minimum across all lanes. |
| `TEMAX` | `( -- )` | `ACC = max(src0[lane])` — maximum across all lanes. |
| `TPOPCNT` | `( -- )` | `ACC = Σ popcount(src0[lane])` — total bit count. |
| `TL1` | `( -- )` | `ACC = Σ |src0[lane]|` — L1 norm. |

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

---

## NIC / Networking (4 words)

Low-level access to the network interface controller.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NET-STATUS` | `( -- status )` | Read NIC status register.  Bit 1: RX available, bit 2: link up, bit 7: present. |
| `NET-SEND` | `( addr len -- )` | Send a frame: set DMA address + length, issue SEND command. |
| `NET-RECV` | `( addr maxlen -- actual )` | Receive a frame into addr (up to maxlen bytes).  Returns actual frame length, or 0 if nothing available. |
| `NET-MAC@` | `( -- hi lo )` | Read the 6-byte MAC address as two stack values. |

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
| `FSLOAD` | `( "filename" -- )` | **Disk boot word.**  Reads the MP64FS directory from the attached disk, finds the named file, reads it sector-by-sector, and EVALUATEs each line.  This is how KDOS boots from disk. |
| `EXIT` | `( -- )` | Return from the current word immediately. |

---

## Comments (2 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `\` | *rest of line* | Line comment — everything after `\` to end-of-line is ignored. |
| `(` | *...* `)` | Inline comment — everything between `(` and `)` is ignored.  Immediate. |

---

## Boot Sequence

When the Megapad-64 powers on:

1. **CPU reset** — all registers zeroed, PC = R3 = address 0,
   SP = R15 = top of RAM
2. **BIOS starts** — initializes the interrupt vector table, configures
   the UART, sets up the Forth dictionary (HERE, LATEST, etc.)
3. **Disk check** — if a storage device is present (bit 7 of `DISK@`),
   the BIOS executes `FSLOAD autoexec.f`
4. **autoexec.f** typically contains: `FSLOAD kdos.f` — which loads the
   entire KDOS operating system from the disk
5. **REPL** — the Forth outer interpreter (`QUIT`) runs, accepting
   input from the UART and executing/compiling words

If no disk is attached, the BIOS drops straight into the REPL after
step 2, ready for Forth input over the UART (or via the `--forth` CLI
flag for file injection).

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
