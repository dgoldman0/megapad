# Megapad-64 BIOS v1.0 — Forth Dictionary Reference

Complete catalog of all **367** dictionary words defined in `bios.asm`.

---

## Dictionary Entry Format

Each entry is a linked list node:

```
[link : 8 bytes]      → pointer to previous entry (0 = end of chain)
[flags+len : 1 byte]  → bit 7 = IMMEDIATE, bits 6:0 = name length
[name : N bytes]       → ASCII name (case-insensitive lookup)
[trampoline code]      → ldi64 r11, <impl>; call.l r11; ret.l
```

- Lookup is case-insensitive (both search key and entry name are uppercased during comparison).
- The chain starts at `var_latest` (the most recently defined word) and follows links backward to `d_dup` (link = 0).
- `entry_to_code` skips `link(8) + flags(1) + name(N)` to reach the executable trampoline.
- No alignment padding is used — Megapad-64 is fully byte-addressable.

---

## Boot Sequence

1. **Hardware init**: RSP = `ram_size`, DSP = `ram_size / 2`, UART base → R8, TX ring descriptor pointer → R19, subroutine pointers → R4/R5/R6, timer enabled.  The TX ring buffer address is written to UART TX_RING_BASE (`+0x08`).
2. **IVT install**: Bus-fault handler registered via CSR 0x20.
3. **Forth variables**: `STATE` = 0 (interpreting), `BASE` = 10, `HERE` = `dict_free`, `LATEST` = `latest_entry` (FSLOAD).
4. **Banner**: Prints `"Megapad-64 Forth BIOS v1.0"`, RAM size in hex, `" ok"`.
5. **Auto-boot**: Checks disk present bit (MMIO STATUS bit 7). If set, reads directory, finds first Forth-type file (type=3), and loads it via FSLOAD.
6. **QUIT**: Falls into the outer interpreter loop.

---

## QUIT — The Outer Interpreter

`forth_quit` resets RSP to RAM top, then loops:
1. If `STATE` = 0, print `"> "` prompt.
2. `read_line` → TIB. Set `>IN` = 0.
3. Parse words via `parse_word`. For each:
   - **Found in dictionary**: If interpreting (`STATE`=0) or word is IMMEDIATE → execute. Otherwise compile a `call` to it.
   - **Not found**: Try `parse_number`. If interpreting → push. If compiling → `compile_literal`.
   - **Neither**: Print `"<word> ?"`, reset `STATE` to 0, abort line.
4. At end of line, print `" ok"`, loop.

---

## EVALUATE Implementation

`w_evaluate ( addr len -- )`:
1. Saves current `>IN` and `var_tib_len` on return stack.
2. Copies the source string into TIB (max 255 chars).
3. Sets `>IN` = 0, `var_tib_len` = len.
4. Runs a full interpreter loop (identical logic to QUIT's `interp_loop`): parse → find → execute/compile, or parse number, or print `" ?"`.
5. On completion, restores saved `>IN` and `var_tib_len` from return stack.

---

## FSLOAD Implementation

`w_fsload ( "name" -- )`:
1. Parses filename from input stream.
2. Checks disk present (MMIO STATUS bit 7).
3. Reads MP64FS directory (sectors 2–5, 2048 bytes) into buffer at `ram_size / 2`.
4. Scans 64 directory entries (32 bytes each) for exact name match (including NUL-padding check).
5. Extracts `start_sector` (u16 @ +16), `sector_count` (u16 @ +18), `used_bytes` (u32 @ +20).
6. Reads file data sectors via `disk_read_sectors` into RAM buffer.
7. Walks content line-by-line (splitting on LF, stripping trailing CR):
   - Pushes `( addr len )` for each non-empty line and calls `w_evaluate`.
8. Cleans up RSP frame on completion.

---

## How `."` Works (Interpret vs Compile)

`w_dotquote` is IMMEDIATE and state-smart:

- **Interpret mode** (`STATE` = 0): Reads characters from TIB until `"` and emits each one immediately via `emit_char`. No compilation occurs.
- **Compile mode** (`STATE` = 1): Compiles `call dotquote_runtime` followed by inline string bytes and a NUL terminator. At runtime, `dotquote_runtime` reads the return address (= string start), calls `print_str`, then scans past the NUL to adjust the return address.

---

## Important Variables

| Variable | Purpose |
|---|---|
| `var_state` | 0 = interpreting, 1 = compiling |
| `var_base` | Current number base (default 10) |
| `var_here` | Next free dictionary byte (grows up) |
| `var_latest` | Pointer to most recent dictionary entry |
| `var_to_in` | Parse offset into TIB |
| `var_tib_len` | Length of current TIB content |
| `var_word_addr` | Last parsed word address (for error messages) |
| `var_word_len` | Last parsed word length |
| `var_leave_count` | Compile-time: number of LEAVE fixups in current loop |
| `var_leave_fixups` | Array of up to 8 LEAVE branch fixup addresses |

---

## Word Catalog by Category

### Stack Manipulation (17 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 1 | `DUP` | `( a -- a a )` | | Duplicate top of stack |
| 2 | `DROP` | `( a -- )` | | Discard top of stack |
| 3 | `SWAP` | `( a b -- b a )` | | Swap top two items |
| 4 | `OVER` | `( a b -- a b a )` | | Copy second item to top |
| 5 | `ROT` | `( a b c -- b c a )` | | Rotate third item to top |
| 6 | `NIP` | `( a b -- b )` | | Drop second item |
| 7 | `TUCK` | `( a b -- b a b )` | | Copy top below second |
| 8 | `2DUP` | `( a b -- a b a b )` | | Duplicate top pair |
| 9 | `2DROP` | `( a b -- )` | | Drop top pair |
| 10 | `DEPTH` | `( -- n )` | | Number of items on data stack |
| 11 | `PICK` | `( n -- x )` | | Copy n-th stack item (0-based) |
| 12 | `ROLL` | `( xu ... x0 u -- xu-1 ... x0 xu )` | | Remove u-th item, place on top (0=nop, 1=SWAP, 2=ROT) |
| 13 | `-ROT` | `( a b c -- c a b )` | | Reverse rotate (ROT ROT) |
| 14 | `?DUP` | `( x -- x x \| 0 )` | | Duplicate if nonzero |
| 15 | `2OVER` | `( a b c d -- a b c d a b )` | | Copy second pair to top |
| 16 | `2SWAP` | `( a b c d -- c d a b )` | | Swap top two pairs |
| 17 | `2ROT` | `( a b c d e f -- c d e f a b )` | | Rotate third pair to top |

### Arithmetic (16 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 17 | `+` | `( a b -- a+b )` | | Addition |
| 18 | `-` | `( a b -- a-b )` | | Subtraction |
| 19 | `*` | `( a b -- a*b )` | | Multiplication |
| 20 | `/` | `( a b -- quot )` | | Signed division (quotient) |
| 21 | `MOD` | `( a b -- rem )` | | Signed modulus (remainder) |
| 22 | `/MOD` | `( a b -- rem quot )` | | Signed divide with remainder |
| 23 | `NEGATE` | `( n -- -n )` | | Two's complement negate |
| 24 | `ABS` | `( n -- \|n\| )` | | Absolute value |
| 25 | `1+` | `( n -- n+1 )` | | Increment by 1 |
| 26 | `1-` | `( n -- n-1 )` | | Decrement by 1 |
| 27 | `2*` | `( n -- n*2 )` | | Left shift by 1 (multiply by 2) |
| 28 | `2/` | `( n -- n/2 )` | | Right shift by 1 (divide by 2, logical) |
| 29 | `MIN` | `( a b -- min )` | | Signed minimum |
| 30 | `MAX` | `( a b -- max )` | | Signed maximum |
| 31 | `CELLS` | `( n -- n*8 )` | | Convert cell count to byte offset (cell = 8 bytes) |
| 32 | `CELL+` | `( a -- a+8 )` | | Advance address by one cell (8 bytes) |

### Logic & Bitwise (11 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 33 | `AND` | `( a b -- a&b )` | | Bitwise AND |
| 34 | `OR` | `( a b -- a\|b )` | | Bitwise OR |
| 35 | `XOR` | `( a b -- a^b )` | | Bitwise XOR |
| 36 | `INVERT` | `( a -- ~a )` | | Bitwise NOT (one's complement) |
| 37 | `LSHIFT` | `( a n -- a<<n )` | | Left shift |
| 38 | `RSHIFT` | `( a n -- a>>n )` | | Right shift (logical) |
| 39 | `POPCNT` | `( x -- n )` | | Population count (number of set bits). Uses bitfield ALU POPCNT instruction. |
| 40 | `CLZ` | `( x -- n )` | | Count leading zeros (0 → 64). Uses bitfield ALU CLZ instruction. |
| 41 | `CTZ` | `( x -- n )` | | Count trailing zeros (0 → 64). Uses bitfield ALU CTZ instruction. |
| 42 | `BITREV` | `( x -- x' )` | | Reverse all 64 bits. Uses bitfield ALU BITREV instruction. |
| 43 | `BSWAP` | `( x -- x' )` | | Byte-swap (endian reverse). Uses bitfield ALU BSWAP instruction. |

### Comparison (13 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 44 | `=` | `( a b -- flag )` | | True (-1) if equal |
| 45 | `<` | `( a b -- flag )` | | Signed less-than |
| 46 | `>` | `( a b -- flag )` | | Signed greater-than |
| 47 | `0=` | `( n -- flag )` | | True if zero |
| 48 | `0<` | `( n -- flag )` | | True if negative (sign bit set) |
| 49 | `0>` | `( n -- flag )` | | True if positive nonzero |
| 50 | `<>` | `( a b -- flag )` | | True if not equal |
| 51 | `0<>` | `( n -- flag )` | | True if nonzero |
| 52 | `>=` | `( a b -- flag )` | | Signed greater-or-equal |
| 53 | `<=` | `( a b -- flag )` | | Signed less-or-equal |
| 54 | `U<` | `( a b -- flag )` | | Unsigned less-than |
| 55 | `U>` | `( a b -- flag )` | | Unsigned greater-than |
| 56 | `WITHIN` | `( n lo hi -- flag )` | | True if `(n-lo) u< (hi-lo)` (ANS) |

### Memory (18 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 52 | `@` | `( addr -- val )` | | Fetch 64-bit cell |
| 53 | `!` | `( val addr -- )` | | Store 64-bit cell |
| 54 | `C@` | `( addr -- byte )` | | Fetch byte |
| 55 | `C!` | `( byte addr -- )` | | Store byte |
| 56 | `W@` | `( addr -- u16 )` | | Fetch 16-bit LE value |
| 57 | `W!` | `( u16 addr -- )` | | Store 16-bit LE value |
| 58 | `L@` | `( addr -- u32 )` | | Fetch 32-bit LE value |
| 59 | `L!` | `( u32 addr -- )` | | Store 32-bit LE value |
| 60 | `+!` | `( n addr -- )` | | Add n to contents of addr |
| 61 | `OFF` | `( addr -- )` | | Store 0 at addr |
| 62 | `HERE` | `( -- addr )` | | Push current dictionary pointer |
| 63 | `ALLOT` | `( n -- )` | | Advance HERE by n bytes |
| 64 | `,` | `( x -- )` | | Store cell at HERE, advance by 8 |
| 65 | `C,` | `( c -- )` | | Store byte at HERE, advance by 1 |
| 66 | `CMOVE` | `( src dst u -- )` | | Copy u bytes forward (no overlap handling) |
| 67 | `MOVE` | `( src dst u -- )` | | Copy u bytes (handles overlap correctly) |
| 68 | `FILL` | `( addr n byte -- )` | | Fill n bytes with byte value |
| 69 | `DUMP` | `( addr n -- )` | | Hex dump n bytes (16 per line with address prefix) |

### I/O & Display (18 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 70 | `EMIT` | `( c -- )` | | Append character to TX ring buffer (auto-flushed at 4096 bytes) |
| 71 | `KEY` | `( -- c )` | | Flush TX buffer, then blocking read one character from UART RX |
| 72 | `KEY?` | `( -- flag )` | | True if a character is available (non-blocking) |
| 73 | `CR` | `( -- )` | | Emit CR+LF |
| 74 | `.` | `( n -- )` | | Print signed number + trailing space using BASE |
| 75 | `U.` | `( u -- )` | | Print unsigned number + trailing space using BASE |
| 76 | `.S` | `( -- )` | | Non-destructive stack print: `<depth> val1 val2 …` |
| 77 | `HEX` | `( -- )` | | Set BASE to 16 |
| 78 | `DECIMAL` | `( -- )` | | Set BASE to 10 |
| 79 | `BASE` | `( -- addr )` | | Push address of BASE variable |
| 80 | `SPACE` | `( -- )` | | Emit one space |
| 81 | `SPACES` | `( n -- )` | | Emit n spaces |
| 82 | `TYPE` | `( addr len -- )` | | Print len characters starting at addr |
| 83 | `ACCEPT` | `( addr max -- n )` | | Read up to max chars from UART into addr, return count |
| 84 | `.ZSTR` | `( addr -- )` | | Print NUL-terminated string |
| 85 | `WORDS` | `( -- )` | | List all dictionary word names |
| 86 | `BYE` | `( -- )` | | Flush TX buffer, print "Bye!" and halt the CPU |
| 87 | `TX-FLUSH` | `( -- )` | | Explicitly drain the TX ring buffer to the host |

### String & Parsing (8 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 88 | `S"` | `( -- addr len )` | ✓ | Compile inline string; runtime pushes (addr len) |
| 89 | `."` | `( -- )` | ✓ | State-smart: interpret → print immediately; compile → compile inline string + print at runtime |
| 90 | `WORD` | `( char "ccc" -- c-addr )` | | Parse input delimited by char, store counted string at HERE (transient) |
| 91 | `COUNT` | `( c-addr -- addr len )` | | Convert counted string to (addr len) pair |
| 92 | `COMPARE` | `( addr1 u1 addr2 u2 -- n )` | | Compare two strings: returns -1 (less), 0 (equal), or 1 (greater) |
| 93 | `CHAR` | `( "name" -- c )` | | Parse next word, push its first character |
| 94 | `[CHAR]` | `( "name" -- )` | ✓ | Compile literal of next word's first character |
| 95 | `UCHAR` | `( c -- C )` | | Convert lowercase ASCII to uppercase |

### Control Flow (15 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 96 | `IF` | `( flag -- )` | ✓ | Compile conditional forward branch (taken when flag=0) |
| 97 | `ELSE` | `( -- )` | ✓ | Compile unconditional forward branch, resolve IF's branch |
| 98 | `THEN` | `( -- )` | ✓ | Resolve forward branch from IF or ELSE |
| 99 | `BEGIN` | `( -- )` | ✓ | Mark loop target (push HERE to compile-time data stack) |
| 100 | `UNTIL` | `( flag -- )` | ✓ | Compile conditional backward branch to BEGIN (loop while flag=0) |
| 101 | `WHILE` | `( flag -- )` | ✓ | Inside BEGIN…REPEAT: compile conditional forward branch (exit when flag=0) |
| 102 | `REPEAT` | `( -- )` | ✓ | Compile unconditional backward branch to BEGIN, resolve WHILE |
| 103 | `AGAIN` | `( -- )` | ✓ | Compile unconditional backward branch to BEGIN (infinite loop) |
| 104 | `DO` | `( limit index -- )` | ✓ | Compile counted loop preamble: move limit & index to RSP |
| 105 | `LOOP` | `( -- )` | ✓ | Compile loop increment (+1), compare to limit, branch back or fall through |
| 106 | `+LOOP` | `( n -- )` | ✓ | Compile loop increment (+TOS), compare to limit, branch back or fall through |
| 107 | `I` | `( -- index )` | | Push current DO…LOOP index from return stack (RSP+16) |
| 108 | `J` | `( -- outer-index )` | | Push outer loop index in nested DO…LOOP (RSP+32) |
| 109 | `LEAVE` | `( -- )` | ✓ | Compile UNLOOP + forward branch (resolved by LOOP/+LOOP) |
| 110 | `UNLOOP` | `( -- )` | ✓ | Compile `addi R15, 16` to drop loop control parameters from RSP |

### Compilation & Defining (24 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 111 | `:` | `( "name" -- )` | | Begin colon definition: create header at HERE, set STATE=1 |
| 112 | `;` | `( -- )` | ✓ | End definition: compile `sep r17` (EXIT handler), set STATE=0 |
| 113 | `EXIT` | `( -- )` | ✓ | Compile early return (`sep r17`) within a definition |
| 114 | `VARIABLE` | `( "name" -- )` | | Create word that pushes address of an 8-byte data cell (initialized to 0) |
| 115 | `CONSTANT` | `( n "name" -- )` | | Create word that pushes n |
| 116 | `VALUE` | `( x "name" -- )` | | Create word that pushes *contents* of its data cell (19-byte trampoline with `ldn` indirection) |
| 117 | `TO` | `( x "name" -- )` | ✓ | Store x into VALUE's data cell. State-smart: interpret → store directly; compile → emit inline store code |
| 118 | `CREATE` | `( "name" -- )` | | Create word with 30-byte trampoline (runtime pushes data-field addr). Includes 13-byte DOES> slot of zeroes |
| 119 | `DOES>` | `( -- )` | ✓ | Compile `call does_runtime` + `sep r17`. At runtime, patches latest CREATE'd word's trampoline offset 16–29 with jump to DOES> body |
| 120 | `IMMEDIATE` | `( -- )` | | Set IMMEDIATE flag (bit 7 of flags byte) on most recent word |
| 121 | `STATE` | `( -- addr )` | | Push address of STATE variable |
| 122 | `[` | `( -- )` | ✓ | Switch to interpret mode (STATE=0) |
| 123 | `]` | `( -- )` | | Switch to compile mode (STATE=1) |
| 124 | `LITERAL` | `( x -- )` | ✓ | Compile code to push x at runtime (16 bytes: ldi64+subi+str) |
| 125 | `POSTPONE` | `( "name" -- )` | ✓ | If IMMEDIATE: compile call. If not: compile literal(xt) + call(postpone_helper) for deferred compilation |
| 126 | `RECURSE` | `( -- )` | ✓ | Compile call to current definition (uses LATEST → entry_to_code) |
| 127 | `EXECUTE` | `( xt -- )` | | Call execution token (code field address) |
| 128 | `'` | `( "name" -- xt )` | | Parse next word, find in dictionary, push its code field address (0 if not found) |
| 129 | `[']` | `( "name" -- )` | ✓ | Compile-time: parse next word, compile its XT as a literal. Equivalent to `' name LITERAL` |
| 130 | `>BODY` | `( xt -- addr )` | | Data-field address of a CREATEd word. CREATE's trampoline is 30 bytes, so addr = xt + 30 |
| 131 | `FIND` | `( c-addr -- c-addr 0 \| xt 1 \| xt -1 )` | | ANS FIND: search dictionary for counted string. Returns xt+1 if immediate, xt+-1 if normal, c-addr+0 if not found |
| 132 | `:NONAME` | `( -- xt )` | | Begin anonymous (headerless) definition. Pushes HERE as the XT. Terminated by `;` which leaves XT on stack |
| 133 | `[:` | `( -- )` | ✓ | Open quotation: compile forward branch over body, push fixup data + sentinel. Must be inside a definition |
| 134 | `;]` | `( -- )` | ✓ | Close quotation: compile ret, resolve forward branch, compile literal of quotation XT into enclosing definition |

### Return Stack (6 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 135 | `>R` | `( x -- )` `R:( -- x )` | ✓ | Compile inline: pop data stack, push return stack (10 bytes) |
| 136 | `R>` | `( -- x )` `R:( x -- )` | ✓ | Compile inline: pop return stack, push data stack (10 bytes) |
| 137 | `R@` | `( -- x )` `R:( x -- x )` | ✓ | Compile inline: copy RSP top to data stack (7 bytes) |
| 138 | `2>R` | `( x1 x2 -- )` `R:( -- x1 x2 )` | ✓ | Compile inline: move pair to return stack (20 bytes) |
| 139 | `2R>` | `( -- x1 x2 )` `R:( x1 x2 -- )` | ✓ | Compile inline: pop pair from return stack (20 bytes) |
| 140 | `2R@` | `( -- x1 x2 )` `R:( x1 x2 -- x1 x2 )` | ✓ | Compile inline: copy pair from return stack (19 bytes) |

### Input Source & Interpreter (5 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 141 | `SOURCE` | `( -- addr len )` | | Push TIB address and current TIB length |
| 142 | `>IN` | `( -- addr )` | | Push address of `>IN` variable (parse offset into TIB) |
| 143 | `EVALUATE` | `( addr len -- )` | | Interpret string as Forth source (saves/restores TIB state on RSP) |
| 144 | `>NUMBER` | `( ud addr len -- ud' addr' len' )` | | Convert string chars to number using BASE. Stops at first non-digit. ud treated as single 64-bit value |
| 145 | `QUIT` | `( -- )` | | Reset return stack, enter outer interpreter loop (does not return) |

### Comments (2 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 146 | `\` | `( -- )` | ✓ | Line comment: set `>IN` = TIB length (skip rest of line) |
| 147 | `(` | `( -- )` | ✓ | Block comment: skip characters until matching `)` |

### JIT Compiler (4 words)

The BIOS includes an optional compile-time JIT that inlines native code
for 18 common primitives instead of emitting `call.l` instructions,
uses compact literal encodings for small constants, folds small-literal
+ ALU sequences into single immediate instructions, and fuses common
two-primitive bigrams into optimised native sequences.  JIT is **off by
default**; enable it with `JIT-ON` before compiling performance-critical
code.

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 154 | `JIT-ON` | `( -- )` | | Enable JIT inline compilation |
| 155 | `JIT-OFF` | `( -- )` | | Disable JIT inline compilation |
| 156 | `JIT-STATS` | `( -- )` | | Print JIT statistics: inlines, folds, peepholes, and bytes saved |
| 157 | `JIT-RESET` | `( -- )` | | Reset JIT counters and peephole state to zero |

**Inlined primitives (18):** `DUP` `DROP` `SWAP` `OVER` `NIP` `2DROP`
`+` `-` `AND` `OR` `XOR` `INVERT` `NEGATE` `@` `!` `CELLS` `CELL+`
`>BODY`

**Compact literal encoding:** Literals 0–255 use an 8-byte `ldi8`
sequence instead of the 16-byte `ldi64` + push.  The constant `-1`
(`TRUE`) uses a 9-byte `ldi64 r0, -1` + push.

**Literal folding:** When a small literal (0–127 for `+`/`-`, 0–255
for `AND`/`OR`/`XOR`) is followed by an ALU word, the pair is fused
into a single 7-byte immediate instruction (e.g. `3 +` → `addi`).
Saves 22 bytes per folded pair vs unoptimised compilation.

**Peephole bigrams (6 patterns):** Consecutive inlined primitives are
checked against a bigram table and replaced with fused native sequences:

| Pattern | Effect | Fused bytes |
|---------|--------|-------------|
| `DUP +` | double TOS | 6 |
| `SWAP DROP` | NIP | 7 |
| `DUP @` | copy + fetch | 9 |
| `OVER +` | add NOS to TOS | 13 |
| `DUP DROP` | nop | 0 |
| `SWAP SWAP` | nop | 0 |

**Typical speedup:** 1.4×–2.1× on primitive-heavy loops (benchmarked
with `bench_jit_prims.py`).  Compilation overhead during KDOS load is
+0.8% (~2.5M extra steps out of 310M).  KDOS load fires ~512 literal
folds, ~38 peephole bigrams, and ~5100 primitive inlines, saving ~50 KB
of compiled code.

### Miscellaneous / System (9 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 146 | `BL` | `( -- 32 )` | | Push space character constant (ASCII 32) |
| 147 | `TRUE` | `( -- -1 )` | | Push true flag (0xFFFFFFFFFFFFFFFF) |
| 148 | `FALSE` | `( -- 0 )` | | Push false flag (0) |
| 149 | `LATEST` | `( -- entry )` | | Push current LATEST pointer (most recent dictionary entry address) |
| 150 | `ABORT` | `( -- )` | | Reset DSP and RSP, jump to QUIT |
| 151 | `ABORT"` | `( flag -- )` | ✓ | Compile: if flag≠0 at runtime, print inline message string and ABORT |
| 152 | `TALIGN` | `( -- )` | | Align HERE to next 64-byte boundary (for tile data) |
| 153 | `FSLOAD` | `( "name" -- )` | | Load named file from MP64FS disk and EVALUATE its contents line-by-line |

### Tile Engine (39 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 154 | `TI` | `( -- )` | | Print all tile CSR info (mode, ctrl, src0, src1, dst, acc0–3) |
| 155 | `TVIEW` | `( addr -- )` | | Display 64 bytes as 4×16 hex grid |
| 156 | `TFILL` | `( addr byte -- )` | | Fill 64 bytes at addr with byte value |
| 157 | `TSRC0!` | `( addr -- )` | | Set tile source 0 address (CSR 0x16) |
| 158 | `TSRC1!` | `( addr -- )` | | Set tile source 1 address (CSR 0x17) |
| 159 | `TDST!` | `( addr -- )` | | Set tile destination address (CSR 0x18) |
| 160 | `TMODE!` | `( n -- )` | | Set tile mode register (CSR 0x14) |
| 161 | `TCTRL!` | `( n -- )` | | Set tile control register (CSR 0x15) |
| 162 | `TMODE@` | `( -- n )` | | Read tile mode register (CSR 0x14) |
| 163 | `TCTRL@` | `( -- n )` | | Read tile control register (CSR 0x15) |
| 164 | `TADD` | `( -- )` | | Tile element-wise addition (t.add instruction) |
| 165 | `TSUB` | `( -- )` | | Tile element-wise subtraction (t.sub) |
| 166 | `TAND` | `( -- )` | | Tile element-wise bitwise AND (t.and) |
| 167 | `TOR` | `( -- )` | | Tile element-wise bitwise OR (t.or) |
| 168 | `TXOR` | `( -- )` | | Tile element-wise bitwise XOR (t.xor) |
| 169 | `TMUL` | `( -- )` | | Tile element-wise multiplication (t.mul) |
| 170 | `TDOT` | `( -- )` | | Tile dot product, result in ACC (t.dot) |
| 171 | `TSUM` | `( -- )` | | Tile sum reduction, result in ACC (t.sum) |
| 172 | `TMIN` | `( -- )` | | Tile reduce-min, result in ACC (t.rmin) |
| 173 | `TMAX` | `( -- )` | | Tile reduce-max, result in ACC (t.rmax) |
| 174 | `TTRANS` | `( -- )` | | Tile transpose (t.trans) |
| 175 | `TZERO` | `( -- )` | | Tile zero-fill destination (t.zero) |
| 176 | `TPOPCNT` | `( -- )` | | Tile popcount reduction, result in ACC (t.popcnt) |
| 177 | `TL1` | `( -- )` | | Tile L1 norm reduction, result in ACC (t.l1) |
| 178 | `TEMIN` | `( -- )` | | Tile element-wise min, writes to DST (t.min) |
| 179 | `TEMAX` | `( -- )` | | Tile element-wise max, writes to DST (t.max) |
| 180 | `TABS` | `( -- )` | | Tile element-wise absolute value, writes to DST (t.abs) |
| 181 | `TSUMSQ` | `( -- )` | | Tile sum-of-squares reduction, result in ACC (t.sumsq) |
| 182 | `TMINIDX` | `( -- )` | | Tile min-with-index reduction, ACC0=min, ACC1=index (t.minidx) |
| 183 | `TMAXIDX` | `( -- )` | | Tile max-with-index reduction, ACC0=max, ACC1=index (t.maxidx) |
| 184 | `TWMUL` | `( -- )` | | Tile widening multiply: 8b×8b→16b, 16b×16b→32b (t.wmul) |
| 185 | `TMAC` | `( -- )` | | Tile multiply-accumulate: DST += SRC0 × SRC1 (t.mac) |
| 186 | `TFMA` | `( -- )` | | Tile fused multiply-add: DST = SRC0 × SRC1 + DST (t.fma) |
| 187 | `TDOTACC` | `( -- )` | | Tile 4-way dot product accumulate, results in ACC0–ACC3 (t.dotacc) |
| 188 | `ACC@` | `( -- n )` | | Read tile accumulator ACC0 (CSR 0x19) |
| 189 | `ACC1@` | `( -- n )` | | Read tile accumulator ACC1 (CSR 0x1A) |
| 190 | `ACC2@` | `( -- n )` | | Read tile accumulator ACC2 (CSR 0x1B) |
| 191 | `ACC3@` | `( -- n )` | | Read tile accumulator ACC3 (CSR 0x1C) |
| 192 | `CYCLES` | `( -- n )` | | Read 32-bit hardware timer counter (MMIO +0x0100) |

### NIC — Network Interface (10 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 193 | `NET-STATUS` | `( -- status )` | | Read NIC STATUS register (MMIO +0x0401) |
| 194 | `NET-SEND` | `( addr len -- )` | | DMA send frame: write DMA addr + length, issue SEND command (0x01) |
| 195 | `NET-RECV` | `( addr -- len )` | | DMA receive frame: returns frame length (0 if no frame available) |
| 196 | `NET-MAC@` | `( -- addr )` | | Push MMIO address of 6-byte MAC at NIC+0x0E |
| 197 | `NTOH` | `( x -- x' )` | | Network-to-host 64-bit byte order. Uses BSWAP instruction. |
| 198 | `HTON` | `( x -- x' )` | | Host-to-network 64-bit byte order. Alias of NTOH (self-inverse). |
| 199 | `NTOH32` | `( x -- x' )` | | Network-to-host 32-bit: BSWAP + 32 RSHIFT. |
| 200 | `HTON32` | `( x -- x' )` | | Host-to-network 32-bit. Alias of NTOH32. |
| 201 | `NTOH16` | `( x -- x' )` | | Network-to-host 16-bit: BSWAP + 48 RSHIFT. |
| 202 | `HTON16` | `( x -- x' )` | | Host-to-network 16-bit. Alias of NTOH16. |

### Pool Allocator (3 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 203 | `POOL-ALLOC` | `( bitmap -- bitmap' index )` | | Allocate lowest free slot. Uses CTZ(~bitmap). Aborts if pool full. |
| 204 | `POOL-FREE` | `( bitmap index -- bitmap' )` | | Free slot at index: clear bit. |
| 205 | `POOL-COUNT` | `( bitmap -- n )` | | Count allocated slots via POPCNT. |

### Disk / Storage (6 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 206 | `DISK@` | `( -- status )` | | Read storage STATUS register (bit7=present, bit0=busy, bit1=error) |
| 207 | `DISK-SEC!` | `( sector -- )` | | Set sector number (32-bit LE at MMIO +0x0202) |
| 208 | `DISK-DMA!` | `( addr -- )` | | Set DMA address (64-bit LE at MMIO +0x0206, upper 4 bytes zeroed) |
| 209 | `DISK-N!` | `( count -- )` | | Set sector count (byte at MMIO +0x020E) |
| 210 | `DISK-READ` | `( -- )` | | Issue READ command 0x01 (DMA: disk → RAM) |
| 211 | `DISK-WRITE` | `( -- )` | | Issue WRITE command 0x02 (DMA: RAM → disk) |
| 212 | `DISK-FLUSH` | `( -- )` | | Issue FLUSH command 0xFF (save in-memory image to host file) |

### Timer & Interrupts (6 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 203 | `TIMER!` | `( compare -- )` | | Set 32-bit compare-match register (MMIO +0x0104, written via st.w) |
| 204 | `TIMER-CTRL!` | `( ctrl -- )` | | Write timer CONTROL byte (bit0=enable, bit1=compare-match IRQ, bit2=auto-reload) |
| 205 | `TIMER-ACK` | `( -- )` | | Acknowledge timer IRQ (write 0x01 to STATUS at MMIO +0x0109) |
| 206 | `EI!` | `( -- )` | | Enable interrupts globally (EI instruction) |
| 207 | `DI!` | `( -- )` | | Disable interrupts globally (DI instruction) |
| 208 | `ISR!` | `( xt slot -- )` | | Install xt at IVT slot: writes to `ivt_table + slot*8` |

### RTC / System Clock (7 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 351 | `MS@` | `( -- ms )` | | Read 64-bit monotonic uptime in ms (reads UPTIME +0x0B00, byte 0 latches) |
| 352 | `EPOCH@` | `( -- ms )` | | Read 64-bit epoch ms since Unix epoch (reads EPOCH +0x0B08, byte 0 latches) |
| 353 | `RTC@` | `( -- sec min hour day mon year dow )` | | Read all seven calendar fields onto the stack |
| 354 | `RTC!` | `( sec min hour day mon year -- )` | | Set calendar (writes SEC–YEAR_HI at +0x10–+0x16) |
| 355 | `RTC-CTRL!` | `( ctrl -- )` | | Write RTC CTRL byte (bit0=run, bit1=alarm IRQ enable) at +0x18 |
| 356 | `RTC-ALARM!` | `( sec min hour -- )` | | Set alarm time (writes ALARM_S/M/H at +0x1A–+0x1C) |
| 357 | `RTC-ACK` | `( -- )` | | Clear alarm flag (write 0x01 to STATUS at +0x19) |

### Multicore (11 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 209 | `COREID` | `( -- n )` | | Push this core's hardware ID (0–3). Reads CSR 0x20. |
| 210 | `NCORES` | `( -- n )` | | Push total number of hardware cores. Reads CSR 0x21. |
| 211 | `IPI-SEND` | `( xt core -- )` | | Send inter-processor interrupt: writes 64-bit XT to mailbox DATA, then triggers IPI to target core. |
| 212 | `IPI-STATUS` | `( -- mask )` | | Read pending IPI bitmask for this core (bit N = IPI from core N). MMIO at MBOX_BASE+0x09. |
| 213 | `IPI-ACK` | `( core -- )` | | Acknowledge IPI from the given core. Clears the pending bit. MMIO at MBOX_BASE+0x0A. |
| 214 | `MBOX!` | `( d -- )` | | Write 64-bit value to mailbox outgoing data register (8 bytes LE at MBOX_BASE+0x00). |
| 215 | `MBOX@` | `( -- d )` | | Read 64-bit value from mailbox incoming data register (8 bytes LE at MBOX_BASE+0x00). |
| 216 | `SPIN@` | `( n -- flag )` | | Try to acquire spinlock *n*. Returns 0 if acquired, 1 if busy. MMIO at SPINLOCK_BASE + n*4. |
| 217 | `SPIN!` | `( n -- )` | | Release spinlock *n*. Writes to SPINLOCK_BASE + n*4 + 1. |
| 218 | `WAKE-CORE` | `( xt core -- )` | | Convenience: pre-writes XT into shared worker table, then sends IPI to wake the target core. |
| 219 | `CORE-STATUS` | `( core -- n )` | | Read worker XT slot for core. Returns 0 if core is idle, non-zero (= pending XT) if busy. |

### Performance Counters (5 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 220 | `PERF-CYCLES` | `( -- n )` | | Read cycle counter (CSR 0x68) |
| 221 | `PERF-STALLS` | `( -- n )` | | Read stall counter (CSR 0x69) |
| 222 | `PERF-TILEOPS` | `( -- n )` | | Read tile operation counter (CSR 0x6A) |
| 223 | `PERF-EXTMEM` | `( -- n )` | | Read external memory beat counter (CSR 0x6B) |
| 224 | `PERF-RESET` | `( -- )` | | Reset all perf counters and re-enable (CSR 0x6C ← 3) |

### CRC Engine (6 words) — ISA-native (EXT.CRYPTO `FB`)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 225 | `CRC-POLY!` | `( n -- )` | | Select polynomial: 0=CRC32, 1=CRC32C, 2=CRC64 (`crc.poly`) |
| 226 | `CRC-INIT!` | `( n -- )` | | Reset CRC accumulator (`crc.init`) |
| 227 | `CRC-FEED` | `( n -- )` | | Feed 8 bytes of data into CRC engine (`crc.din`) |
| 228 | `CRC@` | `( -- n )` | | Read current CRC result (CSR 0x80 CRC_ACC) |
| 229 | `CRC-RESET` | `( -- )` | | Reset CRC to initial value (`crc.init`) |
| 230 | `CRC-FINAL` | `( -- )` | | Finalize CRC with XOR-out (`crc.fin`, result → CSR 0x80) |

### Memory BIST (5 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 231 | `BIST-FULL` | `( -- )` | | Start full memory BIST (March C− + checkerboard + addr-as-data) |
| 232 | `BIST-QUICK` | `( -- )` | | Start quick memory BIST (March C− only) |
| 233 | `BIST-STATUS` | `( -- n )` | | Read BIST status: 0=idle, 1=running, 2=pass, 3=fail |
| 234 | `BIST-FAIL-ADDR` | `( -- n )` | | Read first failing address |
| 235 | `BIST-FAIL-DATA` | `( -- n )` | | Read expected/actual data (packed) |

### Tile Self-Test (3 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 236 | `TILE-TEST` | `( -- )` | | Start tile datapath self-test (~200 cycles) |
| 237 | `TILE-TEST@` | `( -- n )` | | Read self-test status: 0=idle, 2=pass, 3=fail |
| 238 | `TILE-DETAIL@` | `( -- n )` | | Read failed sub-test bitmask |

### Stride / 2D Addressing (6 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 239 | `TSTRIDE-R!` | `( n -- )` | | Set row stride in bytes (CSR 0x40) |
| 240 | `TSTRIDE-R@` | `( -- n )` | | Read row stride (CSR 0x40) |
| 241 | `TTILE-H!` | `( n -- )` | | Set tile height for 2D ops (CSR 0x42) |
| 242 | `TTILE-W!` | `( n -- )` | | Set tile width for 2D ops (CSR 0x43) |
| 243 | `TLOAD2D` | `( -- )` | | 2D strided load into tile register (t.load2d) |
| 244 | `TSTORE2D` | `( -- )` | | 2D strided store from tile register (t.store2d) |

### FP16 / BF16 Modes (2 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 245 | `FP16-MODE` | `( -- )` | | Set TMODE to FP16 half-precision (EW=4) |
| 246 | `BF16-MODE` | `( -- )` | | Set TMODE to bfloat16 (EW=5) |

### Instruction Cache (5 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 247 | `ICACHE-ON` | `( -- )` | | Enable the instruction cache |
| 248 | `ICACHE-OFF` | `( -- )` | | Disable the instruction cache |
| 249 | `ICACHE-INV` | `( -- )` | | Invalidate all I-cache lines, reset stats, re-enable |
| 250 | `ICACHE-HITS` | `( -- n )` | | Push I-cache hit counter |
| 251 | `ICACHE-MISSES` | `( -- n )` | | Push I-cache miss counter |

### AES-256/128-GCM Engine (11 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 252 | `AES-KEY!` | `( addr -- )` | | Load 256-bit key (32 bytes at addr) into AES engine |
| 253 | `AES-IV!` | `( addr -- )` | | Load 96-bit IV (12 bytes at addr) into AES engine |
| 254 | `AES-AAD-LEN!` | `( n -- )` | | Set additional authenticated data length (bytes) |
| 255 | `AES-DATA-LEN!` | `( n -- )` | | Set plaintext/ciphertext data length (bytes) |
| 256 | `AES-CMD!` | `( cmd -- )` | | Start operation: 1 = encrypt, 2 = decrypt |
| 257 | `AES-STATUS@` | `( -- status )` | | Read status: 0 = busy, 1 = done, 2 = auth fail |
| 258 | `AES-DIN!` | `( addr -- )` | | Feed input data block (16 bytes at addr) to engine |
| 259 | `AES-DOUT@` | `( addr -- )` | | Read output data block (16 bytes) from engine |
| 260 | `AES-TAG@` | `( addr -- )` | | Read 128-bit authentication tag (16 bytes) from engine |
| 261 | `AES-TAG!` | `( addr -- )` | | Write expected tag (16 bytes) for decryption verification |
| 262 | `AES-KEY-MODE!` | `( n -- )` | | Set key mode: 0 = AES-256 (14 rounds), 1 = AES-128 (10 rounds) |

### SHA-3 / SHAKE Engine (8 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 263 | `SHA3-INIT` | `( -- )` | | Initialize SHA3 engine for new hash computation |
| 264 | `SHA3-UPDATE` | `( addr len -- )` | | Feed data (len bytes at addr) into SHA3 engine |
| 265 | `SHA3-FINAL` | `( addr -- )` | | Finalize hash and store digest at addr (mode-aware: 32B for SHA3-256, 64B for SHA3-512) |
| 266 | `SHA3-STATUS@` | `( -- status )` | | Read engine status: 0 = busy, 1 = ready |
| 267 | `SHA3-MODE!` | `( mode -- )` | | Set mode: 0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256 |
| 268 | `SHA3-MODE@` | `( -- mode )` | | Read current hash mode |
| 269 | `SHA3-SQUEEZE` | `( addr len -- )` | | Squeeze len bytes of XOF output (SHAKE modes) |
| 270 | `SHA3-SQUEEZE-NEXT` | `( addr len -- )` | | Auto-permute and squeeze next XOF block |

### SHA-256 Engine (5 words) — ISA-native (EXT.CRYPTO `FB`)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 271 | `SHA256-INIT` | `( -- )` | | Initialize SHA-256 state (`sha.init 0`) |
| 272 | `SHA256-UPDATE` | `( addr len -- )` | | Feed data bytes into SHA-256 block buffer (`sha.din`) |
| 273 | `SHA256-FINAL` | `( addr -- )` | | Finalize hash (`sha.final` + `sha.dout`), copy 32-byte digest to addr |
| 274 | `SHA256-STATUS@` | `( -- status )` | | Always returns 0 (engine is synchronous, always ready) |
| 275 | `SHA256-DOUT@` | `( addr -- )` | | Read 32 bytes of digest via `sha.dout` to addr |

### CRC DMA (4 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 276 | `CRC-DMA` | `( addr len -- )` | | Feed len bytes via DMA to CRC engine |
| 277 | `CCRC32` | `( addr len -- crc )` | | Compute CRC32 of memory region (reset + DMA + finalize) |
| 278 | `CRC-DMA!` | `( addr -- )` | | Set CRC DMA source address |
| 279 | `CRC-DMA-LEN!` | `( n -- )` | | Set CRC DMA transfer length |

### TRNG (3 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 280 | `RANDOM` | `( -- u )` | | Return a 64-bit random number |
| 281 | `RANDOM8` | `( -- u )` | | Return an 8-bit random number (0–255) |
| 282 | `SEED-RNG` | `( u -- )` | | Seed the CSPRNG (emulator only) |

### Field ALU (12 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 283 | `GF-A!` | `( addr -- )` | | Load 256-bit operand A from addr into ACC0–ACC3 |
| 284 | `GF-R@` | `( addr -- )` | | Store ACC0–ACC3 (256-bit result) to addr |
| 285 | `GF-PRIME` | `( n -- )` | | Select prime: 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom |
| 286 | `LOAD-PRIME` | `( p-addr pinv-addr -- )` | | Latch custom prime + Montgomery p_inv |
| 287 | `FADD` | `( a b -- r )` | | (a + b) mod p |
| 288 | `FSUB` | `( a b -- r )` | | (a − b) mod p |
| 289 | `FMUL` | `( a b -- r )` | | (a · b) mod p |
| 290 | `FSQR` | `( a -- r )` | | a² mod p |
| 291 | `FINV` | `( a -- r )` | | a^(p−2) mod p |
| 292 | `FPOW` | `( a b -- r )` | | a^b mod p |
| 293 | `FMUL-RAW` | `( a b -- rlo rhi )` | | Raw 256×256→512-bit multiply |
| 294 | `FMUL-ADD-RAW` | `( a b -- rlo rhi )` | | Multiply-accumulate (raw) |

### NTT Engine (9 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 296 | `NTT-LOAD` | `( addr -- )` | | Load 256-element polynomial |
| 297 | `NTT-STORE` | `( addr -- )` | | Store 256-element result |
| 298 | `NTT-FWD` | `( -- )` | | Forward NTT (time → frequency) |
| 299 | `NTT-INV` | `( -- )` | | Inverse NTT (frequency → time) |
| 300 | `NTT-PMUL` | `( addr -- )` | | Pointwise multiply |
| 301 | `NTT-PADD` | `( addr -- )` | | Pointwise add |
| 302 | `NTT-SETQ` | `( q -- )` | | Set modulus (3329 or 8380417) |
| 303 | `NTT-STATUS@` | `( -- status )` | | Read engine status |
| 304 | `NTT-WAIT` | `( -- )` | | Busy-wait until complete |

### KEM Engine — ML-KEM-512 (7 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 305 | `KEM-KEYGEN` | `( -- )` | | Generate ML-KEM-512 keypair |
| 306 | `KEM-ENCAPS` | `( pk-addr -- )` | | Encapsulate: ciphertext + shared secret |
| 307 | `KEM-DECAPS` | `( ct-addr -- )` | | Decapsulate: recover shared secret |
| 308 | `KEM-SETQ` | `( q -- )` | | Set underlying NTT modulus |
| 309 | `KEM-STATUS@` | `( -- status )` | | Read engine status |
| 310 | `KEM-PK@` | `( addr -- )` | | Read public key to addr |
| 311 | `KEM-CT@` | `( addr -- )` | | Read ciphertext to addr |

### Cooperative Multitasking (8 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 312 | `PAUSE` | `( -- )` | | Round-robin yield across all 4 task slots via `SEP R20`; resumes when the next active task yields back |
| 313 | `TASK-YIELD` | `( -- )` | | Yield from the current background task back to Task 0 via `SEP R20` |
| 314 | `BACKGROUND` | `( xt -- )` | | Set xt as Task 1 body and start it |
| 315 | `TASK-STOP` | `( n -- )` | | Stop background task in slot n (1–3), reset to idle |
| 316 | `TASK?` | `( n -- flag )` | | Return 0 if task slot n (1–3) is idle, 1 if running |
| 317 | `BACKGROUND2` | `( xt -- )` | | Set xt as Task 2 body and start it |
| 318 | `BACKGROUND3` | `( xt -- )` | | Set xt as Task 3 body and start it |
| 319 | `#TASKS` | `( -- n )` | | Count active background tasks (0–3) |

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Stack Manipulation | 16 |
| Arithmetic | 16 |
| Logic & Bitwise | 6 |
| Comparison | 13 |
| Memory | 18 |
| I/O & Display | 17 |
| String & Parsing | 8 |
| Control Flow | 15 |
| Compilation & Defining | 21 |
| Return Stack | 6 |
| Input Source & Interpreter | 5 |
| Comments | 2 |
| Miscellaneous / System | 9 |
| Tile Engine | 39 |
| NIC | 4 |
| Disk / Storage | 6 |
| Timer & Interrupts | 6 |
| RTC / System Clock | 7 |
| Multicore | 11 |
| Performance Counters | 5 |
| CRC Engine | 6 |
| Memory BIST | 5 |
| Tile Self-Test | 3 |
| Stride / 2D Addressing | 6 |
| FP16 / BF16 Modes | 2 |
| Instruction Cache | 5 |
| AES-256/128-GCM Engine | 11 |
| SHA-3 / SHAKE | 8 |
| SHA-256 Engine | 5 |
| CRC DMA | 4 |
| TRNG | 3 |
| Field ALU | 13 |
| NTT Engine | 9 |
| KEM Engine | 7 |
| Cooperative Multitasking | 8 |
| **Total** | **363** |

### All Immediate Words (34)

`;` `IF` `ELSE` `THEN` `BEGIN` `UNTIL` `WHILE` `REPEAT` `DO` `LOOP` `+LOOP` `AGAIN` `LEAVE` `UNLOOP` `EXIT` `>R` `R>` `R@` `2>R` `2R>` `2R@` `[` `LITERAL` `S"` `."` `\` `(` `TO` `POSTPONE` `RECURSE` `[CHAR]` `ABORT"` `DOES>` `[']`

### Dictionary Chain Order (link chain: last → first)

```
#TASKS → BACKGROUND3 → BACKGROUND2 → TASK? → TASK-STOP → BACKGROUND → TASK-YIELD → PAUSE →
CRC-DMA-LEN! → CRC-DMA! → CCRC32 → CRC-DMA →
SHA256-DOUT@ → SHA256-STATUS@ → SHA256-FINAL → SHA256-UPDATE → SHA256-INIT →
SHA3-SQUEEZE-NEXT → SHA3-SQUEEZE → SHA3-MODE@ → SHA3-MODE! →
SHA3-STATUS@ → SHA3-FINAL → SHA3-UPDATE → SHA3-INIT →
AES-KEY-MODE! → AES-TAG! → AES-TAG@ → AES-DOUT@ → AES-DIN! → AES-STATUS@ → AES-CMD! →
AES-DATA-LEN! → AES-AAD-LEN! → AES-IV! → AES-KEY! →
ICACHE-MISSES → ICACHE-HITS → ICACHE-INV → ICACHE-OFF → ICACHE-ON →
BF16-MODE → FP16-MODE → TSTORE2D → TLOAD2D → TTILE-W! → TTILE-H! →
TSTRIDE-R@ → TSTRIDE-R! → TILE-DETAIL@ → TILE-TEST@ → TILE-TEST →
BIST-FAIL-DATA → BIST-FAIL-ADDR → BIST-STATUS → BIST-QUICK → BIST-FULL →
CRC-FINAL → CRC-RESET → CRC@ → CRC-FEED → CRC-INIT! → CRC-POLY! →
PERF-RESET → PERF-EXTMEM → PERF-TILEOPS → PERF-STALLS → PERF-CYCLES →
CORE-STATUS → WAKE-CORE → SPIN! → SPIN@ → MBOX@ → MBOX! → IPI-ACK →
IPI-STATUS → IPI-SEND → NCORES → COREID →
FSLOAD → QUIT → >NUMBER → DOES> → 2R@ → 2R> → 2>R → POSTPONE → TO →
VALUE → RECURSE → [CHAR] → CHAR → COMPARE → EVALUATE → >IN → SOURCE →
FIND → WITHIN → MOVE → COUNT → 2/ → LEAVE → ABORT" → ABORT → TALIGN →
UCHAR → .ZSTR → L! → L@ → W! → W@ → OFF → U> → U< → <= → >= → 2ROT →
2SWAP → 2OVER → LATEST → WORD → FALSE → TRUE → BL → -ROT → CMOVE →
2* → +! → CELL+ → CELLS → MAX → MIN → ?DUP → 0<> → <> → 0> → S" →
CREATE → IMMEDIATE → LITERAL → ] → [ → STATE → AGAIN → +LOOP → UNLOOP →
J → R@ → R> → >R → EXIT → ( → \ → ISR! → DI! → EI! → RTC-ACK →
RTC-ALARM! → RTC-CTRL! → RTC! → EPOCH@ → MS@ → RTC@ → TIMER-ACK →
TIMER-CTRL! → TIMER! → DISK-FLUSH → DISK-WRITE → DISK-READ → DISK-N! → DISK-DMA! →
DISK-SEC! → DISK@ → NET-MAC@ → NET-RECV → NET-SEND → NET-STATUS →
ACCEPT → ." → SPACES → SPACE → TYPE → CONSTANT → VARIABLE → I → LOOP →
DO → REPEAT → WHILE → UNTIL → BEGIN → THEN → ELSE → IF → ; → : → ' →
EXECUTE → TCTRL@ → TMODE@ → TABS → TDOTACC → TFMA → TMAC → TWMUL →
TMAXIDX → TMINIDX → TSUMSQ → TEMAX → TEMIN → TL1 → TPOPCNT →
ACC3@ → ACC2@ → ACC1@ → ACC@ → CYCLES → TZERO → TTRANS → TMAX → TMIN →
TSUM → TDOT → TMUL → TXOR → TOR → TAND → TSUB → TADD → TCTRL! →
TMODE! → TDST! → TSRC1! → TSRC0! → TFILL → TVIEW → TI → FILL → DUMP →
BYE → WORDS → BASE → DECIMAL → HEX → .S → U. → . → CR → KEY? → KEY →
EMIT → C, → , → ALLOT → HERE → C! → C@ → ! → @ → 0< → 0= → > → < →
= → RSHIFT → LSHIFT → INVERT → XOR → OR → AND → 1- → 1+ → ABS →
NEGATE → /MOD → MOD → / → * → - → + → ROLL → PICK → DEPTH → 2DROP → 2DUP →
TUCK → NIP → ROT → OVER → SWAP → DROP → DUP
```

### MMIO Address Map

| Base Address | Device | Registers |
|---|---|---|
| `0xFFFF_FF00_0000_0000` | UART | TX=+0, RX=+1, STATUS=+2 |
| `0xFFFF_FF00_0000_0100` | Timer | COUNT=+0..+3, COMPARE=+4..+7, CTRL=+8, STATUS=+9 |
| `0xFFFF_FF00_0000_0200` | Storage | CMD=+0, STATUS=+1, SECTOR=+2..+5, DMA=+6..+D, SEC_COUNT=+E |
| `0xFFFF_FF00_0000_0400` | NIC | CMD=+0, STATUS=+1, DMA=+2..+9, LEN=+A..+B, MAC=+E..+13 |
| `0xFFFF_FF00_0000_0500` | Mailbox | DATA=+0..+7, SEND=+8, STATUS=+9, ACK=+A |
| `0xFFFF_FF00_0000_0600` | Spinlock | Per-lock: ACQUIRE=+n*4, RELEASE=+n*4+1 |
| `0xFFFF_FF00_0000_0700` | AES-256-GCM | Key/IV/data/tag registers |
| `0xFFFF_FF00_0000_0780` | SHA-3/SHAKE | Rate/state/control (96 bytes) |
| `0xFFFF_FF00_0000_0800` | TRNG | DATA=+0..+7, STATUS=+8 |
| `0xFFFF_FF00_0000_0880` | Field ALU | OP_A=+0..+1F, OP_B=+20..+3F, CMD=+40, STATUS=+41, RESULT=+48..+67, RESULT_HI=+68..+87 |
| `0xFFFF_FF00_0000_08C0` | NTT Engine | COEFF=+0..+1FF, CMD=+200, STATUS=+201, Q=+208..+20B |
| `0xFFFF_FF00_0000_0900` | KEM Engine | CMD=+0, STATUS=+1, Q=+8, PK=+10, CT=+100, SS=+200 |
| `0xFFFF_FF00_0000_0940` | ~~SHA-256~~ | Removed — now per-core ISA (`sha.init`/`sha.din`/`sha.final`/`sha.dout`) |
| `0xFFFF_FF00_0000_0980` | ~~CRC Engine~~ | Removed — now per-core ISA (`crc.init`/`crc.poly`/`crc.din`/`crc.fin`) |
| `0xFFFF_FF00_0000_0B00` | RTC | UPTIME=+0..7 (R,latched), EPOCH=+8..F (RW,latched), SEC=+10, MIN=+11, HOUR=+12, DAY=+13, MON=+14, YEAR=+15..16, DOW=+17, CTRL=+18, STATUS=+19, ALARM=+1A..1C |

### Memory Layout

```
0x00000                BIOS code + dictionary + strings + TIB(256B) + IVT(64B)
dict_free →            User dictionary (HERE grows upward)
ram_size/2 ↓           Data stack (R14 grows downward)
ram_size/2 →           FSLOAD file buffer (grows upward, shared region)
ram_size ↓             Return stack (R15 grows downward)
```

### Register Conventions

| Register | Usage |
|----------|-------|
| R0 | Scratch / CSR operand |
| R1 | Scratch / argument / return value |
| R2 | `ram_size` (set at boot, preserved) |
| R3 | PC (PSEL=3) |
| R4 | Subroutine pointer: `emit_char` |
| R5 | Subroutine pointer: `key_char` (blocking) |
| R6 | Subroutine pointer: `print_hex_byte` |
| R7 | Scratch |
| R8 | UART TX base address |
| R19 | TX ring buffer descriptor pointer (set at boot) |
| R9 | Scratch / word pointer |
| R10 | String pointer for `print_str` |
| R11 | Scratch / temp |
| R18 | SHA shift-amount scratch (set to 32 for `shr` in digest output) |
| R12 | Scratch / counter |
| R13 | Scratch / temp |
| R14 | **DSP** — Data stack pointer (grows downward) |
| R15 | **RSP** — Return/call stack pointer (grows downward) |
| R16 | **NEXT** handler (`sep r16` = fetch inline XT, advance IP, branch) |
| R17 | **EXIT** handler (`sep r17` = pop return address from RSP, branch) |
| R20 | Task yield handler (cooperative multitasking; `SEP R20` yields) |
