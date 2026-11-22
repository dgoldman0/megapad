\ =====================================================================
\  uroko.f -- Uroko's declarations of the base words
\ =====================================================================
\
\  Uroko is the language of Leviathan, MegaPad with the LM1 chiplet.  It
\  gives every word a checked stack effect and describes cells by kinds.
\  A kind follows an item's name after a colon, inside the ordinary stack
\  comment, as in ( buf: span -- n: u ).  Akashic's
\  docs/language/checker.md describes the kinds and the checker.
\
\  The BIOS is assembly and KDOS's comments name cells informally, so the
\  base words cannot carry declarations of their own.  This module
\  declares them in Uroko's form, one word to a line:
\
\      EFFECT COMPARE ( s1: span s2: span -- n: n )
\
\  It covers the memory and text words whose cells are addresses, spans
\  and characters, and the BIOS words for terminal geometry, memory
\  regions and relocation tracking.  A declaration keeps the shape of the
\  word's effect in docs/BIOS-DICTIONARY.md or in KDOS's stack comment.
\
\  EFFECT takes the word's name and, for now, records nothing; the effect
\  after it is an ordinary comment, which KDOS's ( skips.  Uroko's checker
\  reads the same lines, today as the host prototype in Akashic's
\  toolchain/language, which reports a declaration that names no base
\  word or changes a word's shape.  The checker on the machine will give
\  EFFECT the body that records each declared effect for the word it
\  names, so this file serves both without change.  A system that lacks
\  a declared word, as the hosted simulator lacks a few BIOS words, loads
\  the file all the same.
\
\  Load after KDOS:   REQUIRE uroko.f

PROVIDED uroko.f

\ EFFECT ( "name" -- )  Declare the effect of a base word.  The effect
\   follows on the same line as a comment.
: EFFECT  ( "name" -- )  BL WORD DROP ;

\ ---------------------------------------------------------------------
\  Memory
\ ---------------------------------------------------------------------

EFFECT HERE      ( -- a: addr )
EFFECT ALLOT     ( n: n -- )
EFFECT FILL      ( a: addr u: u c: char -- )
EFFECT MOVE      ( src: addr dst: addr u: u -- )
EFFECT CMOVE     ( src: addr dst: addr u: u -- )
EFFECT CMOVE>    ( src: addr dst: addr u: u -- )
EFFECT OFF       ( a: addr -- )
EFFECT ON        ( a: addr -- )
EFFECT 2@        ( a: addr -- x1 x2 )
EFFECT 2!        ( x1 x2 a: addr -- )
EFFECT ALLOCATE  ( u: u -- a: addr ior: n )
EFFECT FREE      ( a: addr -- )
EFFECT RESIZE    ( a1: addr u: u -- a2: addr ior: n )

\ ---------------------------------------------------------------------
\  Text and characters
\ ---------------------------------------------------------------------

EFFECT COMPARE   ( s1: span s2: span -- n: n )
EFFECT COUNT     ( a: addr -- s: span )
EFFECT SOURCE    ( -- s: span )
EFFECT TYPE      ( s: span -- )
EFFECT ACCEPT    ( buf: span -- n: u )
EFFECT EMIT      ( c: char -- )
EFFECT KEY       ( -- c: char )

\ ---------------------------------------------------------------------
\  Numbers
\ ---------------------------------------------------------------------

EFFECT WITHIN    ( n: n lo: n hi: n -- f: flag )

\ ---------------------------------------------------------------------
\  Terminal geometry
\ ---------------------------------------------------------------------

EFFECT COLS            ( -- cols: u )
EFFECT ROWS            ( -- rows: u )
EFFECT TERMSIZE        ( -- cols: u rows: u )
EFFECT RESIZED?        ( -- resized: flag )
EFFECT RESIZE-DENIED?  ( -- denied: flag )
EFFECT RESIZE-REQUEST  ( cols: u rows: u -- )

\ ---------------------------------------------------------------------
\  Memory regions and cores
\ ---------------------------------------------------------------------

EFFECT HBW-BASE      ( -- a: addr )
EFFECT HBW-SIZE      ( -- u: u )
EFFECT EXT-MEM-BASE  ( -- a: addr )
EFFECT EXT-MEM-SIZE  ( -- u: u )
EFFECT VRAM-BASE     ( -- a: addr )
EFFECT VRAM-SIZE     ( -- u: u )
EFFECT N-FULL        ( -- n: u )
EFFECT MICRO?        ( id: u -- micro: flag )

\ ---------------------------------------------------------------------
\  Relocation tracking
\ ---------------------------------------------------------------------

EFFECT _RELOC-ACTIVE  ( -- holds: flag )
EFFECT _RELOC-COUNT   ( -- holds: u )
EFFECT _RELOC-BUF     ( -- holds: addr )
