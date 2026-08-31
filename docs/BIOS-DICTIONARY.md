# Megapad-64 BIOS v1.0 — Forth Dictionary Reference

The `bios.asm` dictionary link chain contains **481** entries.  The numbered
subsystem tables below are a historical catalog and do not yet enumerate every
later-added BIOS entry.

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

## Hardware Cache and BIOS Index

The dictionary-acceleration implementation keeps the linked dictionary
authoritative and adds two bounded acceleration layers. Names of at
most 31 bytes first probe the per-core
`EXT.DICT` cache. A cache miss probes the caller-backed BIOS index, which also
covers names through the dictionary header's 127-byte limit. An exact positive
result demand-fills the hardware cache when eligible. An authoritative empty
index slot proves a negative lookup; an absent, rebuilding, or saturated index
falls back to the latest-first linked chain.

The hardware cache is 256 sets by four ways, for 1,024 entries. Each set has a
deterministic round-robin replacement cursor: matching insertions update in
place, an invalid way is preferred, and a full set replaces at the cursor.
New definitions update an already resident binding but do not allocate a line
merely because they were compiled. `DCLR` and CPU reset clear both entries and
replacement state. The cache is a working set, not a partial registry whose
contents become permanent after boot.

KDOS supplies a power-of-two open-addressed index from external memory. The
canonical 128 MiB arrangement selects 65,536 16-byte slots (1 MiB), keeping the
measured 30,598-entry Desktop dictionary below 47% load. The caller-bounded
BIOS interface accepts other valid capacities, and a system without sufficient
external memory remains correct through the linked fallback.

`DICT-INDEX! ( base slots -- status )` returns 0 after a complete authoritative
install or disable, 1 for invalid arguments with the old binding unchanged, or
2 when the new table was installed but its rebuild saturated. `DICT-INDEX@`
`( -- base slots count flags )` returns occupied unique-name slots and flag bits
`BOUND=1`, `AUTHORITATIVE=2`, `BUILDING=4`, and `SATURATED=8`. Each 16-byte
slot stores its published entry pointer at `+0`, uppercase FNV-1a32 hash at
`+8`, seven-bit length at `+12`, and zero reserved bytes at `+13..+15`.
The complete table span must be 16-byte aligned, power-of-two sized,
non-wrapping, and contained in advertised external memory.

Definition publication upserts the side index. `MARKER`, `FORGET`, and
transactional compiler rollback use `DICT-ROLLBACK` to publish `HERE` and
`LATEST` together, clear `EXT.DICT`, and rebuild the index newest-first.
`LATEST!` preserves the compatible one-cell head setter for low-level loaders:
it leaves `HERE` unchanged while validating the replacement chain, globally
clearing `EXT.DICT`, and rebuilding the index under the dictionary epoch.

The sizing profile, exact cache replacement rules, side-index publication and
fallback protocol, and deferred multicore RTL requirements are authoritative
in [`dictionary-acceleration.md`](dictionary-acceleration.md). The original
cold-source timing observation remains useful context: the first nine of 27
chunks used 2.4015 billion guest instructions for 1,101,557 source bytes, while
the final nine used 5.9995 billion for 1,072,108 bytes. The dedicated profile
then attributed at least 10.69 billion guest instructions in the source-load
interval to linked-node base-loop work and showed that negative lookups remain
dominant even with an almost ideal positive cache.

---

## Boot Sequence

1. **Hardware init**: RSP = `ram_size`, DSP = `ram_size / 2`, UART base → R8, TX ring descriptor pointer → R19, subroutine pointers → R4/R5/R6, timer enabled.  The TX ring buffer address is written to UART TX_RING_BASE (`+0x08`).
2. **IVT install**: Bus-fault handler registered via CSR 0x20.
3. **Forth variables and private arena**: `STATE` = 0, `BASE` = 10, reserve
   and scrub `NUM_CORES × 16` bytes above `dict_free` for CRC owner records,
   reset the full-width SHA/Keccak/WOTS software owner fields, set `HERE` to the
   resulting kernel-data end, and set `LATEST` = `latest_entry`
   (`WOTS-CHAIN`). Hardware spinlock 8 resets independently.
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

`w_evaluate ( addr len -- )` retains its legacy stack effect, while the
checked wrapper `EVALUATE-CHECKED ( addr len -- status )` returns the same
operation's status:

1. Rejects source longer than 255 bytes before copying or executing any
   prefix.  Legacy `EVALUATE` prints the error and records status 2 rather
   than silently truncating.
2. Saves the caller's complete 256-byte TIB, `>IN`, and `var_tib_len` in
   the static frame indexed by evaluator depth.  This context survives a
   return-stack exception unwind.
3. Copies the source string into TIB, sets `>IN` = 0, and sets
   `var_tib_len` = len.
4. Runs the interpreter loop.  The first undefined token is copied to a
   stable diagnostic buffer, its zero-based column is recorded, and the
   rest of that input line is not executed.
5. Restores the caller's TIB bytes, `>IN`, and `var_tib_len`, then removes
   the depth frame.  Nested evaluator failure is sticky and propagates to
   the outer checked call.

Status values are: 0 success, 1 undefined token, 2 line too long, 3 nesting
depth exceeded, 4 unfinished compiler state, and 5 a source-level `THROW`
caught by KDOS.  The BIOS `EVALUATE-CHECKED` primitive returns 0–3 because it
exists before KDOS's exception system.  After defining `CATCH`, KDOS
deliberately shadows that public name with a wrapper which checkpoints depth,
catches `THROW`, invokes `EVALUATOR-UNWIND`, records the exception in
`EVAL-THROW`, and returns 5 normally.  `EVALUATE-FINISH` performs the
end-of-source check that reports status 4.

`EVAL-STATUS`, `EVAL-LINE`, `EVAL-COLUMN`, `EVAL-DEPTH`, and `EVAL-THROW`
return variable addresses.  `EVALUATOR-UNWIND` restores complete abandoned
input frames to a captured depth; negative and above-current checkpoints are
ignored.  `EVAL-TOKEN` returns the stable `( addr len )` token copy.  Lines
are one-based when supplied by the caller and columns are zero-based.

After a transactional compiler caller passes its saved `HERE/LATEST` pair to
`DICT-ROLLBACK`, it must call `EVALUATOR-RESET`. The reset clears `STATE`,
cross-line conditionals, quotation/noname, LEAVE, and JIT peephole bookkeeping.
It does not perform dictionary rollback, disturb an enclosing evaluator frame,
or erase the last status and diagnostics.

---

## FSLOAD Implementation

`w_fsload ( "name" -- )`:
1. Parses filename from input stream.
2. Checks disk present (MMIO STATUS bit 7).
3. Reads sector 0 and accepts only marker 1 with geometry derived from the
   exact attached `TOTAL_SECTORS` register.
4. Reads the complete bitmap and 12-sector directory, then validates reserved
   allocation bits, entry types and parents, both extents, and byte bounds.
5. Scans 128 directory entries (48 bytes each) for an exact name match.
6. Extracts the primary and optional secondary extents plus `used_bytes`.
7. Rejects the file unless its complete sector-rounded DMA span ends below the
   live return-stack frame in Bank 0.
8. Reads both validated extents contiguously into the buffer at `ram_size / 2`.
9. Walks content line-by-line (splitting on LF, stripping trailing CR):
   - Pushes `( addr len )` for each non-empty line and calls `w_evaluate`.
10. Cleans up RSP frame on completion.

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
| `var_leave_count` | Compile-time: next slot in the bounded nested-loop fixup stack |
| `var_leave_base` | Compile-time: first fixup slot owned by the current `DO`/`?DO` scope |
| `var_leave_fixups` | Stack of 128 branch fixups; each active loop owns at most 8, including `?DO`'s zero-trip branch |

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

### Arithmetic (17 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 17 | `+` | `( a b -- a+b )` | | Addition |
| 18 | `-` | `( a b -- a-b )` | | Subtraction |
| 19 | `*` | `( a b -- a*b )` | | Multiplication |
| — | `UM*` | `( u1 u2 -- lo hi )` | | Unsigned 64×64-bit multiply; returns the low and high halves of the 128-bit product |
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
| 88 | `S"` | `( -- addr len )` | ✓ | Compile inline string; at the REPL return a BIOS-private transient buffer that checked crypto, entropy, and DMA words reject as protected |
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

### Input Source & Interpreter (15 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 141 | `SOURCE` | `( -- addr len )` | | Push TIB address and current TIB length |
| 142 | `>IN` | `( -- addr )` | | Push address of `>IN` variable (parse offset into TIB) |
| 143 | `EVALUATE` | `( addr len -- )` | | Interpret string as Forth source; nested calls restore complete caller input context and errors are recorded without returning a status cell |
| — | `EVALUATE-CHECKED` | `( addr len -- status )` | | BIOS returns 0–3; the later KDOS shadow also catches source `THROW` and returns 5 |
| — | `EVALUATE-FINISH` | `( -- status )` | | Return 4 if compiler/cross-line evaluator state is unfinished, otherwise 0 |
| — | `EVALUATOR-RESET` | `( -- )` | | Clear compiler bookkeeping after caller-owned HERE/LATEST rollback; retain diagnostics and enclosing evaluator depth |
| — | `EVALUATOR-UNWIND` | `( depth -- )` | | Restore complete abandoned evaluator input frames down to a valid captured depth |
| — | `EVAL-STATUS` | `( -- addr )` | | Address of the last evaluator status cell |
| — | `EVAL-LINE` | `( -- addr )` | | Address of one-based source-line context/diagnostic cell |
| — | `EVAL-COLUMN` | `( -- addr )` | | Address of zero-based failing-token column cell |
| — | `EVAL-DEPTH` | `( -- addr )` | | Address of active evaluator nesting cell for transaction checkpoints |
| — | `EVAL-THROW` | `( -- addr )` | | Address of exact source exception code retained for status 5 |
| — | `EVAL-TOKEN` | `( -- addr len )` | | Stable copy of the failing token; empty for non-token failures |
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
| 182 | `TMINIDX` | `( -- )` | | Tile min-with-index reduction, ACC0=index, ACC1=min (t.minidx) |
| 183 | `TMAXIDX` | `( -- )` | | Tile max-with-index reduction, ACC0=index, ACC1=max (t.maxidx) |
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
| 193 | `NET-STATUS` | `( -- status )` | | Read NIC STATUS (bit 3 is sticky error; bit 4 is RX-DMA-busy) |
| 194 | `NET-SEND` | `( addr len -- )` | | DMA send frame: write DMA addr + length, issue SEND command (0x01) |
| 195 | `NET-RECV` | `( addr -- len )` | | DMA receive frame; wait for RX-DMA-busy to clear before returning committed data/length (0 if unavailable) |
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

### Disk / Storage (17 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 206 | `DISK@` | `( -- status )` | | Read storage STATUS register (bit7=present, bit0=busy, bit1=error) |
| 207 | `DISK-SECTORS` | `( -- count )` | | Read attached media capacity from MMIO +0x0211 (u32 LE) |
| 208 | `DISK-SEC!` | `( sector -- )` | | Set sector number (32-bit LE at MMIO +0x0202) |
| 209 | `DISK-DMA!` | `( addr -- )` | | Diagnostic: set the complete 64-bit LE DMA address at MMIO +0x0206 |
| 210 | `DISK-N!` | `( count -- )` | | Set sector count (byte at MMIO +0x020E) |
| 211 | `DISK-READ` | `( -- )` | | Diagnostic: issue raw READ command 0x01 without waiting |
| 212 | `DISK-WRITE` | `( -- )` | | Diagnostic: issue raw WRITE command 0x02 without waiting |
| 213 | `DISK-FLUSH` | `( -- )` | | Diagnostic: issue raw FLUSH command 0xFF without waiting |
| 214 | `MP64FS-VALID?` | `( -- flag )` | | Validate the attached marker, derived geometry, reserved bitmap, complete directory, parents, extents, and byte bounds. |
| 215 | `DISK-READ-CHECKED` | `( dma lba count -- completed status )` | | Production checked read: validates, locks, splits, waits for matching completion, and returns precise progress/result |
| 216 | `DISK-WRITE-CHECKED` | `( dma lba count -- completed status )` | | Production checked write; successful completion is not a durability claim |
| 217 | `DISK-FLUSH-CHECKED` | `( -- status )` | | Production ordering and durability barrier |
| 218 | `DISK-MEDIA-GEN` | `( -- generation )` | | Read the current attachment generation (u32 LE at MMIO +0x021A) |
| 219 | `DISK-CAPS` | `( -- caps )` | | Read controller capabilities; bit 6 advertises atomic generation-guarded submission |
| 220 | `DISK-READ-GEN-CHECKED` | `( dma lba count generation -- completed status )` | | Generation-bound checked read; rejects a stale identity before DMA |
| 221 | `DISK-WRITE-GEN-CHECKED` | `( dma lba count generation -- completed status )` | | Generation-bound checked write; rejects a stale identity before media mutation |
| 222 | `DISK-FLUSH-GEN-CHECKED` | `( generation -- status )` | | Generation-bound ordering and durability barrier |

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
| 209 | `COREID` | `( -- n )` | | Push this core's hardware ID (`0` through `NCORES - 1`). Reads CSR 0x20. |
| 210 | `NCORES` | `( -- n )` | | Push total number of hardware cores. Reads CSR 0x21. |
| 211 | `IPI-SEND` | `( xt core -- )` | | Send inter-processor interrupt: writes 64-bit XT to mailbox DATA, then triggers IPI to target core. |
| 212 | `IPI-STATUS` | `( -- mask )` | | Read pending IPI bitmask for this core (bit N = IPI from core N). MMIO at MBOX_BASE+0x09. |
| 213 | `IPI-ACK` | `( core -- )` | | Acknowledge IPI from the given core. Clears the pending bit. MMIO at MBOX_BASE+0x0A. |
| 214 | `MBOX!` | `( d -- )` | | Write 64-bit value to mailbox outgoing data register (8 bytes LE at MBOX_BASE+0x00). |
| 215 | `MBOX@` | `( -- d )` | | Read 64-bit value from mailbox incoming data register (8 bytes LE at MBOX_BASE+0x00). |
| 216 | `SPIN@` | `( n -- flag )` | | Try to acquire spinlock *n*. Returns 0 when free or already owned by this physical core, 1 when another core owns it. Same-core reacquisition has no depth count. MMIO at SPINLOCK_BASE + n*4. |
| 217 | `SPIN!` | `( n -- )` | | Release spinlock *n* only for its owning physical core; free and foreign-owned release writes are ignored. MMIO at SPINLOCK_BASE + n*4 + 1. |
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

### CRC Engine and Capability Discovery (9 words) — ISA-native (EXT.CRYPTO `FB`)

BIOS records the full `(COREID,TASK-ID)` owner in a topology-sized private
table. Checks and CRC instructions run with the exact caller interrupt state
saved and restored. Status values used here are 0 OK, 1 UNSUPPORTED,
2 STATE/OWNER, and 3 RANGE.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRYPTO-CAPS@` | `( -- caps )` | Read the raw System Info `CRYPTO_CAPS` qword |
| `CRC-MODE!` | `( mode -- status )` | Select mode 0/1/2/4/5/6 without changing CRC_ACC; reflected modes require capability bit 0 |
| `CRC-RESET` | `( -- status )` | Require the exact owner and reset to the selected all-ones initial value |
| `CRC-INIT!` | `( seed -- status )` | Require the exact owner and load a mode-width seed with `crc.seed` |
| `CRC-FEED` | `( cell -- status )` | Require the exact owner and feed 8 bytes in little-endian order with `crc.q` |
| `CRC-FEED-BYTE` | `( byte -- status )` | Require the exact owner and feed exactly the low byte with `crc.b` |
| `CRC@` | `( -- raw status )` | Return owner-visible CRC_ACC followed by status; misuse returns `0 2` |
| `CRC-RAW-FINAL@` | `( -- raw status )` | Atomically return raw CRC_ACC and release; unsupported returns `0 1` with exact-owner non-reflected cleanup |
| `CRC-FINAL@` | `( -- finalized )` | Atomically XOR-finalize and release; owner misuse returns zero without a CRC instruction |

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
| 256 | `AES-CMD!` | `( cmd -- )` | | Start operation: low bit 0 = encrypt, 1 = decrypt |
| 257 | `AES-STATUS@` | `( -- status )` | | Read status: 0 = idle, 1 = active, 2 = done, 3 = authentication or transaction failure |
| 258 | `AES-KEY-MODE!` | `( n -- )` | | Set key mode: 0 = AES-256 (14 rounds), 1 = AES-128 (10 rounds) |
| 259 | `AES-DIN!` | `( addr -- )` | | Feed input data block (16 bytes at addr) to engine |
| 260 | `AES-DOUT@` | `( addr -- )` | | Read output data block (16 bytes) from engine |
| 261 | `AES-TAG@` | `( addr -- )` | | Read 128-bit authentication tag (16 bytes) from engine |
| 262 | `AES-TAG!` | `( addr -- )` | | Write expected tag (16 bytes) for decryption verification |

The executable BIOS/native ABI places key mode at AES offset `+0x3A` inside
the `+0x700..+0x76F` byte aperture. The native configuration check requires
all 32 key bytes in either mode, although AES-128 uses the first 16. Integrated
RTL currently differs in aperture, access shape, byte protocol, status,
authentication, and qualified timing/interrupt behavior; see
`docs/bios-forth.md` for the discrepancy record.

### Checked SHA-3 / SHAKE / raw Keccak (9 words)

The 96-byte SHA aperture and checked BIOS path are implemented and advertised
by `CRYPTO_CAPS` bits 1 and 2. The common status namespace is `0` OK, `1`
UNSUPPORTED, `2` STATE/OWNER, `3` RANGE, `4` PROTECTED, `5` TIMEOUT, and `6`
HARDWARE/PROTOCOL. A failed destination-returning operation leaves the caller
destination unchanged.

The checked surface owns hardware spinlock 8 (`+0x620` acquire, `+0x621`
release) for a complete transaction and records the full `(COREID,TASK-ID)`
in BIOS. Acquisition and owner publication are one interrupt-state-preserving
critical section, preventing same-core task re-entry. Fixed hashes and raw
Keccak release only after hardware `CLEAR` proves the engine scrubbed; SHAKE
retains ownership after `SHAKE-FINAL` until `SHA3-CLEAR`.

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 263 | `SHA3-BEGIN` | `( mode -- status )` | | Require SHA-stream capability, validate mode 0..3, acquire the portable guard, select `CTRL`, and issue `INIT` |
| 264 | `SHA3-UPDATE` | `( src len -- status )` | | Require the exact owner and absorb a complete caller-readable span; zero length ignores `src` |
| 265 | `SHA3-FINAL` | `( dst -- status )` | | Fixed modes only: stage and publish exactly 32 or 64 digest bytes, clear/scrub, and release |
| 266 | `SHA3-STATUS@` | `( -- status )` | | Diagnostic raw packed MMIO status: phase in bits 1:0 and owner class in bits 3:2; does not acquire or advance the guard |
| 267 | `SHAKE-FINAL` | `( -- status )` | | SHAKE modes only: finalize the XOF, set the logical output cursor to zero, and retain ownership |
| 268 | `SHA3-MODE@` | `( -- mode )` | | Diagnostic raw `CTRL` read; does not acquire or advance the guard |
| 269 | `SHAKE-READ` | `( dst len -- status )` | | Publish the next 0..32 sequential XOF bytes from the 64-byte hardware window; stage before publishing |
| 270 | `SHA3-CLEAR` | `( -- status )` | | Idempotently abort/clear an owned SHA transaction, wipe it, and release; failed quiescence retains the guard |
| — | `KECCAK-F1600` | `( state-200 -- status )` | | In-place permutation of one checked 200-byte caller state; no absorb, padding, separator, or squeeze |

The removed unreleased words are `SHA3-INIT`, `SHA3-MODE!`, `SHA3-SQUEEZE`,
`SHA3-SQUEEZE-NEXT`, `SHA3-DOUT@`, `WOTS-CHAIN-HW`, `SHA3-LOCKED?`, and
`WOTS-STATUS@`; they are not compatibility aliases.

`KECCAK-F1600` maps lane `x + 5*y`, little endian, without reversal:
`memory[8*(x+5*y)+b] = state[x+5*y][8*b +: 8]`. It qualifies the complete
read/write span, stages all 200 output bytes, clears hardware, and only then
publishes them, so a failure leaves the input image unchanged.

### SHA-256 Streaming (4 words) — ISA-native (EXT.CRYPTO `FB`)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 271 | `SHA256-INIT` | `( -- status )` | | Reset the calling core's private SHA-256 context |
| 272 | `SHA256-UPDATE` | `( addr len -- status )` | | Preflight and absorb a complete physical-memory span using the core's dedicated 64-byte block |
| 273 | `SHA256-FINAL` | `( dst -- status )` | | Validate all 32 output bytes, stage the digest, release, publish once, and erase the context |
| 274 | `SHA256-CLEAR` | `( -- status )` | | Idempotently abort, release the SHA transaction, zeroize saved/visible state, and return 0 |

Each core owns one 256-byte context. Statuses are `0` OK, `1` STATE,
`2` RANGE, `3` CONTEXT-ALIAS, and `4` LENGTH-OVERFLOW. `UPDATE` rejects
spans intersecting either complete all-core SHA-2 context arena and rejects
any nonzero high bit-length word, non-byte-aligned or offset-inconsistent
saved length, or overflowing 64-bit bit-length addition.
Every failure aborts and wipes. `FINAL` does not modify a non-context
destination unless all checks and digest extraction succeed.

### SHA-512 Streaming (4 append-only words) — ISA-native (EXT.CRYPTO `FB`)

These words retain their append-only dictionary positions.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA512-INIT` | `( -- status )` | Initialize the calling core's private SHA-512 context |
| `SHA512-UPDATE` | `( addr len -- status )` | Preflight and absorb a physical-memory span using a dedicated 128-byte block |
| `SHA512-FINAL` | `( dst -- status )` | On success publish 64 big-endian digest bytes; erase context and stage on every path |
| `SHA512-CLEAR` | `( -- status )` | Idempotently abort, release the SHA transaction, zeroize saved/visible state, and return 0 |

Statuses are `0` OK, `1` STATE, `2` RANGE, `3` CONTEXT-ALIAS, and
`4` LENGTH-OVERFLOW. Every failure aborts and wipes; failed `FINAL` does not
publish to a non-context destination. SHA-512 input and output checks reject
both the SHA-256 and SHA-512 context arenas. Before an empty UPDATE can
succeed or FINAL can inspect its destination, the active marker must be
exactly one, the partial offset must be below 128, the low bit length must be
byte-aligned, and its modulo-128 byte position must equal that offset.

### Shared SHA-2 span qualification (1 append-only word)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA2-SPAN-STATUS` | `( addr len -- status )` | Without requiring `INIT` or mutating a context, validate one complete physical span and reject either all-core SHA-2 context arena. |

`SHA2-SPAN-STATUS` returns only `0` OK, `2` RANGE, or `3`
CONTEXT-ALIAS. Empty spans succeed without inspecting the address. Internal
physical-span results for address overflow and an unadvertised/cross-window
range are both normalized to `2`. The word is suitable for atomic
higher-level preflight before any SHA context is initialized.

### TRNG (3 historical + 2 append-only words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 279 | `RANDOM` | `( -- u )` | | Return 64 random bits; bus-fault if the shared TRNG is unusable |
| 280 | `RANDOM8` | `( -- u )` | | Return 8 random bits; bus-fault if the shared TRNG is unusable |
| 281 | `SEED-RNG` | `( u -- )` | | Supplement future entropy while usable; never restores an unusable source |

The checked append-only entries are:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ENTROPY-FILL` | `( addr len -- status )` | Preflight a complete writable destination, fill it from checked `RAND8` reads, and wipe it if a detected health loss occurs after publication starts |
| `ENTROPY-READY?` | `( -- flag )` | Return canonical true only when the hardware `STATUS` byte is exactly one; return false for unavailable or noncanonical values |

Statuses are `0` OK, `1` UNAVAILABLE, `2` RANGE, and `3` PROTECTED.
Lengths must be nonnegative. An empty span is an unconditional no-op,
including `(0,0)`, and its unused address is ignored. Every nonempty address
must be nonnegative; a nonempty null destination is RANGE. Every
nonempty span must fit wholly and without wrap in one advertised Bank 0,
external, HBW, or VRAM window. Within Bank 0, only
`[kernel-data-end, caller-DSP-8)` is admitted: the boot-computed lower bound
protects the entire static BIOS/private-state footprint and dynamic CRC owner
table, while the upper bound protects the live stacks and the status cell
that the word will return. This boundary does not
prove allocation ownership; the caller must still supply a buffer it manages.
`ENTROPY-FILL` applies this policy through the shared
`CALLER-SPAN-STATUS` implementation before reading the TRNG.

`ENTROPY-FILL` requires `STATUS` to equal exactly one before every byte read
and after the final byte. An initial unavailable result leaves the destination
unchanged. A detected loss after one or more bytes erases the entire admitted
span before returning UNAVAILABLE. The word's one private `RAND8` instruction
also has a PC-scoped bus-fault recovery point: a health loss after the status
check rejoins that same UNAVAILABLE/wipe path, while unrelated bus faults stay
diagnostic. It retains no operation state across the call. `ENTROPY-READY?`
exposes the same exact readiness decision without requiring higher-level
software to know the TRNG MMIO address.

### Caller-managed span qualification (1 append-only word)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CALLER-SPAN-STATUS` | `( addr len -- status )` | Purely qualify a complete caller-managed span before a higher-level read or write |

The word returns `0` OK, `2` RANGE, or `3` PROTECTED. Zero length is
unconditional OK and ignores the unused address. A nonempty span returns
RANGE for a negative address or length cell, null, wrap, a cross-window
interval, or an interval outside Bank 0 and the nonempty external, HBW, and
VRAM windows advertised by SysInfo.

Within Bank 0, only `[kernel-data-end, caller-DSP-8)` is admitted, excluding
the static BIOS/private footprint, dynamic CRC owner records, live stacks,
and the result cell. This same conservative caller-managed policy is used for
input and output spans; it
intentionally does not expose readable static BIOS storage. Success proves
geometry and platform protection only, not allocation ownership, mutability,
initialization, lifetime, or freedom from application-level aliases.

### X25519 (6 raw words) — ISA-native (EXT.CRYPTO `FB 2D`)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `X25519-SCALAR!` | `( addr -- )` | Load four ascending little-endian qwords into this core's ACC0–ACC3. |
| `X25519-POINT!` | `( addr -- )` | Record the deferred 32-byte point operand in this core's TSRC0. |
| `X25519-GO` | `( -- )` | Synchronously clamp ACC as an RFC 7748 scalar, multiply by the TSRC0 point using Curve25519, and replace ACC. |
| `X25519-WAIT` | `( -- )` | No-op; the ISA operation is synchronous. |
| `X25519-STATUS@` | `( -- n )` | Return 2 unconditionally, including before an operation. |
| `X25519-RESULT@` | `( addr -- )` | Store ACC0–ACC3 as four ascending little-endian qwords. |

These are raw architectural-state words, not checked transactions. They have
no capability bit, lock, task owner, complete-span preflight, wipe, or
low-order/all-zero-result rejection. `POINT!` does not touch memory until
`GO`; later scalar/result qword faults can retain an already mutated prefix.
Unaligned ordinary memory is accepted. `GO` always uses `2^255-19` regardless
of the current Field prime selection.

> **Open accelerator-catalog discrepancy.** These six dictionary entries are
> absent from the legacy ordinal/count tables below, which also disagree with
> the checked-in Field/NTT/KEM chain about several word counts and names. The
> `.dq` dictionary chain in `bios.asm` is authoritative until those later
> tables are regenerated; this X25519 table deliberately does not invent
> replacement ordinal numbers during the simulator slice.

### Field ALU (15 raw words)

All field operands and results are addresses of 32-byte little-endian values.
The raw operations take separate 32-byte low and high destination addresses.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `GF-A!` | `( a-addr -- )` | Load ACC0–ACC3 from four ascending qwords |
| `GF-R@` | `( r-addr -- )` | Store ACC0–ACC3 as four ascending qwords |
| `GF-PRIME` | `( selector -- )` | Select by low two bits: Curve25519, secp256k1, P-256, or custom |
| `LOAD-PRIME` | `( p-addr pinv-addr -- )` | Latch custom `p` and Montgomery inverse without changing the selector |
| `FADD` | `( a-addr b-addr r-addr -- )` | Selected-prime addition for canonical inputs |
| `FSUB` | `( a-addr b-addr r-addr -- )` | Selected-prime subtraction for canonical inputs |
| `FMUL` | `( a-addr b-addr r-addr -- )` | Selected ordinary/Montgomery product |
| `FSQR` | `( a-addr r-addr -- )` | Selected ordinary/Montgomery square |
| `FINV` | `( a-addr r-addr -- )` | Fermat exponent `a^(p-2) mod p` |
| `FPOW` | `( a-addr e-addr r-addr -- )` | Ordinary modular exponentiation |
| `FMUL-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Raw 256×256 product |
| `FCMOV` | `( a-addr cond-addr -- )` | Replace ACC when `cond-addr C@` is nonzero; always read `a` |
| `FCEQ` | `( a-addr b-addr r-addr -- )` | Store exact-representation equality as 256-bit 1/0 |
| `FMAC` | `( a-addr b-addr r-addr -- )` | Add retained previous-low to the selected product |
| `FMUL-ADD-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Wrapped 512-bit raw multiply-accumulate |

The old ordinal table placed only 12 Field entries before NTT. It cannot be
repaired locally without regenerating every later ordinal, so this section
deliberately omits invented numbers and follows the checked-in `.dq` chain.
Canonical field elements and valid custom-prime/Montgomery tuples are the
portable arithmetic domain; backend discrepancies outside it and the native
raw-MAC carry defect are recorded in [bios-forth.md](bios-forth.md#field-alu--multi-prime-arithmetic-15-raw-words).

### NTT Engine (10 raw words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NTT-SETQ` | `( q -- )` | Set the retained uint64 modulus |
| `NTT-IDX!` | `( idx -- )` | Set the raw 16-bit coefficient index |
| `NTT-LOAD` | `( addr buf -- )` | Load 256 uint32-LE coefficients; zero selects A, nonzero selects B |
| `NTT-STORE` | `( addr -- )` | Store 256 uint32-LE result coefficients |
| `NTT-FWD` | `( -- )` | Generic forward NTT of A |
| `NTT-INV` | `( -- )` | Generic inverse NTT of A |
| `NTT-PMUL` | `( -- )` | Pointwise multiply A and B modulo q |
| `NTT-PADD` | `( -- )` | Pointwise add A and B modulo q |
| `NTT-STATUS@` | `( -- status )` | Read 0 idle, 1 busy, or 2 done |
| `NTT-WAIT` | `( -- )` | Poll DONE; idle is not terminal |

As with the corrected Field section, these entries follow the authoritative
`.dq` chain and deliberately omit obsolete ordinal numbers instead of shifting
every later legacy row locally. Transfer/state details and the incompatible
current RTL surface are recorded in
[bios-forth.md](bios-forth.md#ntt-engine-10-raw-words).

### KEM Engine — ML-KEM-512 (7 words)

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KEM-SEL!` | `( n -- )` | Select retained buffer 0..4 and reset its byte index |
| `KEM-LOAD` | `( addr count -- )` | Copy caller bytes to the selected buffer |
| `KEM-STORE` | `( addr count -- )` | Copy selected-buffer bytes to caller memory |
| `KEM-KEYGEN` | `( -- )` | Replace PK and SK from retained 64-byte `d || z` |
| `KEM-ENCAPS` | `( -- )` | Replace CT and SS from retained PK and 32-byte coin |
| `KEM-DECAPS` | `( -- )` | Replace SS from retained CT and SK |
| `KEM-STATUS@` | `( -- n )` | Read retained raw status (0 idle, 2 done in Python execution) |

These are the seven entries in the authoritative `.dq` chain. As in the
corrected Field and NTT sections, obsolete ordinal numbers are omitted rather
than shifting every later legacy row locally. The executable device uses a
40-byte byte-register window at `+0x0900`, completes commands synchronously,
and retains shared buffers and DONE state without ownership or automatic wipe.
Current RTL instead exposes an incompatible 64-bit-slot map, BUSY timing, and
non-cryptographic deterministic stub values. The full transfer/lifecycle,
valid-key interoperability, secret-boundary qualifications, and unresolved
KDOS `KEM-SEED-SIZE` 32-versus-64-byte keygen discrepancy are recorded in
[bios-forth.md](bios-forth.md#kem-engine--ml-kem-512-7-words).

### Cooperative Multitasking (9 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 311 | `PAUSE` | `( -- )` | | Round-robin yield across all 4 task slots via `SEP R20`; resumes when the next active task yields back |
| 312 | `TASK-YIELD` | `( -- )` | | Yield from the current background task back to Task 0 via `SEP R20` |
| 313 | `BACKGROUND` | `( xt -- )` | | Set xt as Task 1 body and start it |
| 314 | `TASK-STOP` | `( n -- )` | | Stop background task in slot n (1–3), reset to idle |
| 315 | `TASK?` | `( n -- flag )` | | Return 0 if task slot n (1–3) is idle, 1 if running |
| 316 | `BACKGROUND2` | `( xt -- )` | | Set xt as Task 2 body and start it |
| 317 | `BACKGROUND3` | `( xt -- )` | | Set xt as Task 3 body and start it |
| 318 | `#TASKS` | `( -- n )` | | Count active background tasks (0–3) |
| 319 | `TASK-ID` | `( -- n )` | | Return executing cooperative slot on core 0 (0 foreground, 1–3 background); worker cores return 0 |

### Full-width TACC (8 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 464 | `TAMAC` | `( -- )` | | Accumulate the `TSRC0` × `TSRC1` tile products into owned, valid TACC lanes (`t.amac`, `E1 06`) |
| 465 | `TACC-TRY` | `( -- )` | | Atomically try to claim TACC; always retires without waiting and returns no flag (`F8 E3 02`) |
| 466 | `TACC-CLEAR` | `( -- )` | | Latch the current legal `TMODE`, zero TACC, and establish valid dirty state (`F8 E3 03`) |
| 467 | `TACC-LOAD` | `( -- )` | | Atomically load the canonical 256-byte image at `TSRC0` and latch its current format (`F8 E3 04`) |
| 468 | `TACC-STORE` | `( -- )` | | Store the canonical 256-byte image at `TDST`; clear `DIRTY` only after complete success (`F8 E3 05`) |
| 469 | `TACC-RELEASE` | `( -- )` | | Zeroize, invalidate, and release caller-owned TACC (`F8 E3 06`) |
| 470 | `TACC-STATUS@` | `( -- status )` | | Read caller-relative TACC status CSR `0x1D` (`D0 1D`) |
| 471 | `TACC-CLAIM?` | `( -- flag )` | | Execute `TACC-TRY`, then return canonical true exactly when `TACC_STATUS.MINE` is set; never spins |

### Dictionary Bounds and Fault Control (5 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 472 | `DICT-BOUNDS!` | `( base limit -- )` | | Install a validated inclusive/exclusive external dictionary interval |
| 473 | `DICT-BOUNDS-OFF` | `( -- )` | | Disable external dictionary allocation and restore guarded Bank-0 allocation |
| 474 | `DICT-BASE@` | `( -- base )` | | Return the active external dictionary base, or zero when disabled |
| 475 | `DICT-LIMIT@` | `( -- limit )` | | Return the active exclusive external dictionary limit, or zero when disabled |
| 476 | `DICT-FAULT-XT!` | `( xt -- )` | | Install the dictionary-fault callback used by the checked allocator |

### Dictionary Acceleration Control (4 words)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 477 | `DICT-INDEX!` | `( base slots -- status )` | | Install/rebuild or disable the caller-backed dictionary index; invalid arguments leave the prior binding unchanged |
| 478 | `DICT-INDEX@` | `( -- base slots count flags )` | | Return bounded index geometry, occupied-slot count, and publication flags |
| 479 | `DICT-ROLLBACK` | `( saved-here saved-latest -- )` | | Validate a contiguous-zone checkpoint, globally clear cached bindings, atomically publish HERE/LATEST, and rebuild the side index |
| 480 | `LATEST!` | `( entry -- )` | | Publish any valid terminating dictionary head without changing HERE, globally clear cached bindings, and rebuild the side index |

### Checked WOTS Chain (1 word)

| # | Word | Stack Effect | Imm | Description |
|---|------|-------------|-----|-------------|
| 481 | `WOTS-CHAIN` | `( context-64 start steps dst-16 -- status )` | | Check capability and complete arguments, run the 64-bit Bank 0 DMA/shared-Keccak chain under crypto guard 8, stage 16 result bytes, prove `CLEAR` reached `IDLE`, then publish and release |

`context-64` is exactly `PK.seed[16] || ADRS[32] || node[16]`. `start` and
`steps` are each 0..15; when `steps` is nonzero their widened sum is at most
15. The word uses the common checked status namespace and leaves all 16
destination bytes unchanged on every failure. BIOS derives its request and
clear deadlines from System Info `NUM_BUS_PORTS`, measures them with the
calling core's `PERF_CYCLES`, restores the caller's `PERF_CTRL` enable bit,
and makes no scheduler/yield call during the timed interval. Failed clear
quiescence wipes private scratch but retains owner kind 3 and spinlock 8 until
machine reset.

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Stack Manipulation | 16 |
| Arithmetic | 17 |
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
| Disk / Storage | 12 |
| Timer & Interrupts | 6 |
| RTC / System Clock | 7 |
| Multicore | 11 |
| Performance Counters | 5 |
| CRC Engine and Capability Discovery | 9 |
| Memory BIST | 5 |
| Tile Self-Test | 3 |
| Stride / 2D Addressing | 6 |
| FP16 / BF16 Modes | 2 |
| Instruction Cache | 5 |
| AES-256/128-GCM Engine | 11 |
| Checked SHA-3 / SHAKE / raw Keccak | 9 |
| SHA-256 Streaming | 4 |
| SHA-512 Streaming | 4 |
| TRNG | 3 |
| Checked Entropy Boundaries | 2 |
| Caller Span Boundary | 1 |
| Field ALU | 15 |
| NTT Engine | 10 |
| KEM Engine | 7 |
| Cooperative Multitasking | 9 |
| Full-width TACC | 8 |
| Dictionary Bounds and Fault Control | 5 |
| Dictionary Acceleration Control | 4 |
| Checked WOTS Chain | 1 |
| **Catalogued subtotal** | **391** |

### All Immediate Words (34)

`;` `IF` `ELSE` `THEN` `BEGIN` `UNTIL` `WHILE` `REPEAT` `DO` `LOOP` `+LOOP` `AGAIN` `LEAVE` `UNLOOP` `EXIT` `>R` `R>` `R@` `2>R` `2R>` `2R@` `[` `LITERAL` `S"` `."` `\` `(` `TO` `POSTPONE` `RECURSE` `[CHAR]` `ABORT"` `DOES>` `[']`

### Newest Dictionary Chain Segment (last → earlier)

The complete authoritative link chain is the `.dq` chain in `bios.asm`.
The checked WOTS word closes the newest appended segment:

```
WOTS-CHAIN → LATEST! → DICT-ROLLBACK → DICT-INDEX@ → DICT-INDEX!
→ DICT-FAULT-XT! → DICT-LIMIT@ → DICT-BASE@ → DICT-BOUNDS-OFF → DICT-BOUNDS!
→ TACC-CLAIM? → TACC-STATUS@ → TACC-RELEASE → TACC-STORE → TACC-LOAD
→ TACC-CLEAR → TACC-TRY → TAMAC → CALLER-SPAN-STATUS
→ ENTROPY-READY? → ENTROPY-FILL → SHA2-SPAN-STATUS
→ SHA512-CLEAR → SHA512-FINAL → SHA512-UPDATE
→ SHA512-INIT → TX-FLUSH
→ CRYPTO-CAPS@ → CRC-FINAL@ → CRC-RAW-FINAL@ → CRC-FEED-BYTE
→ ;] → [: → :NONAME → RESIZE-REQUEST → … → DUP
```

### MMIO Address Map

| Base Address | Device | Registers |
|---|---|---|
| `0xFFFF_FF00_0000_0000` | UART | TX=+0, RX=+1, STATUS=+2 |
| `0xFFFF_FF00_0000_0100` | Timer | COUNT=+0..+3, COMPARE=+4..+7, CTRL=+8, STATUS=+9 |
| `0xFFFF_FF00_0000_0200` | Storage | CMD=+0, STATUS=+1, SECTOR=+2..+5, DMA=+6..+D, SEC_COUNT=+E, TOTAL=+11..+14, RESULT=+15, COMPLETE=+16..+19, MEDIA_GEN=+1A..+1D, CAPS=+1E, TRANSFERRED=+1F, EXPECTED_MEDIA_GEN=+20..+23, GUARDED_CMD=+24 |
| `0xFFFF_FF00_0000_0300` | System Info | Exact 112-byte window; `NUM_CORES`=+10, `CRYPTO_CAPS`=+60, `NUM_BUS_PORTS`=+68 |
| `0xFFFF_FF00_0000_0400` | NIC | CMD=+0, STATUS=+1, DMA=+2..+9, LEN=+A..+B, MAC=+E..+13 |
| `0xFFFF_FF00_0000_0500` | Mailbox | DATA=+0..+7, SEND=+8, STATUS=+9, ACK=+A |
| `0xFFFF_FF00_0000_0600` | Spinlock | Exact 64-byte/16-lock aperture; per-lock ACQUIRE=+n*4, RELEASE=+n*4+1; lock 8 at +20/+21 is the crypto guard |
| `0xFFFF_FF00_0000_0700` | AES-256-GCM | Key/IV/data/tag registers |
| `0xFFFF_FF00_0000_0780` | SHA-3/SHAKE/raw Keccak | Exact 96-byte aperture: CMD +00, STATUS +01, CTRL +02, ERROR +03, DIN +08, 64-byte DOUT +10..+4F, STATE_INDEX +50, STATE_DATA +58..+5F |
| `0xFFFF_FF00_0000_0800` | TRNG | RAND8=+0, RAND64=+8..+F, STATUS=+10, SEED=+18..+1F |
| `0xFFFF_FF00_0000_0840` | *(free)* | Field ALU is ISA-native (`EXT.CRYPTO FB 20..2D`); no MMIO device occupies this range |
| `0xFFFF_FF00_0000_0880` | Port I/O Bridge | PORT1_TARGET..PORT7_TARGET=+00..+0D (16-bit LE, low 12 bits used), BRIDGE_CTRL=+0E |
| `0xFFFF_FF00_0000_08A0` | WOTS Chain | Exact byte-only 32-byte aperture: CONTEXT_ADDR=+00..+07, STEPS=+08, START=+09, CMD/STATUS=+0A, ERROR=+0B, CYCLES=+0C..+0F, DOUT=+10..+1F |
| `0xFFFF_FF00_0000_08C0` | NTT Engine | Executable byte map: STATUS=+00, Q=+08..+0F, IDX=+10..+11, LOAD_A=+18..+1B, LOAD_B=+1C..+1F, RESULT=+20..+23, CMD=+28; current RTL uses incompatible 64-bit slots |
| `0xFFFF_FF00_0000_0900` | KEM Engine | Executable 40-byte window: STATUS(R)=+00, CMD(W)=+01, BUF_SEL(W)=+08, DIN(W)=+10, DOUT(R)=+18, BUF_SIZE(R,uint16-LE)=+20..+21; current RTL uses an incompatible 64-bit-slot map and deterministic crypto stub |
| `0xFFFF_FF00_0000_0940` | ~~SHA-2~~ | Removed — now ISA (`sha.init`/`sha.din`/`sha.final`/`sha.dout`/`sha.release`) |
| `0xFFFF_FF00_0000_0980` | ~~CRC Engine~~ | Removed — now ISA-native (`crc.mode`/`crc.init`/`crc.seed`/`crc.b`/`crc.q`/`crc.fin`/`crc.finraw`) |
| `0xFFFF_FF00_0000_0B00` | RTC | UPTIME=+0..7 (R,latched), EPOCH=+8..F (RW,latched), SEC=+10, MIN=+11, HOUR=+12, DAY=+13, MON=+14, YEAR=+15..16, DOW=+17, CTRL=+18, STATUS=+19, ALARM=+1A..1C |

### Memory Layout

```
0x00000                BIOS code + dictionary + strings + TIB(256B) + IVT(64B)
dict_free →            NUM_CORES × 16-byte private CRC owner records
kernel-data-end →      User dictionary (HERE grows upward)
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
