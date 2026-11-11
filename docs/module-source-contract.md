# Module-source contract

This document defines the interface between the KDOS source loader and the
filesystem a module's text comes from.  `LOAD` and `REQUIRE` do not read a
disk themselves.  They ask the current *module source* to find the file a
path names and to read it, and they evaluate what it returns.  KDOS's own
MP64FS is the source selected at boot.  A filesystem layer that mounts another
volume builds a source of its own and selects it, and from then on modules
come from that volume.

A source's author writes four words and one record of five cells.  The loader
owns everything else: parsing the path from the input, the read buffer, the
line-by-line evaluation, the module registry, and the cleanup after a failed
load.  The sizes, offsets and error numbers below are ABI.

The built-in source is the only one in the tree today.  The first other source
planned is one for Akashic's VFS, which will serve modules from ext4.

## Records

A source is a record of five cells that its owner allocates and fills.

| Offset | Accessor | Field | Stack effect |
| ---: | --- | --- | --- |
| 0 | `MS.RESOLVE` | execution token of RESOLVE | `( c-addr u file source -- ior )` |
| 8 | `MS.READ` | execution token of READ | `( addr file source -- ior )` |
| 16 | `MS.DIR@` | execution token of DIR@ | `( source -- token )` |
| 24 | `MS.DIR!` | execution token of DIR! | `( token source -- )` |
| 32 | `MS.CONTEXT` | one cell for the owner's use | |

Each operation receives the record's address as its last argument, so one set
of words can serve several mounted volumes, each with its own record and
context cell.

RESOLVE describes the file it found in a second record of five cells.  The
loader owns that record and clears it before every RESOLVE.

| Offset | Accessor | Field |
| ---: | --- | --- |
| 0 | `MF.ID` | file identity, two cells; both zero when the source gives none |
| 16 | `MF.USED` | bytes of source text in the file |
| 24 | `MF.RESERVE` | bytes READ needs at its buffer, at least `MF.USED` |
| 32 | `MF.HANDLE` | one cell the source sets for its own READ |

| Constant | Value | Meaning |
| --- | ---: | --- |
| `/MODULE-SOURCE` | 40 | Bytes in a source record |
| `/MODULE-FILE` | 40 | Bytes in a file record |
| `MS-E-NOT-FOUND` | -4110 | RESOLVE: the path names no file |
| `MS-E-NO-FILESYSTEM` | -4111 | RESOLVE: the source has no mounted filesystem |

## Selecting a source

`MODULE-SOURCE` is a variable that holds the address of the current source
record.  `FS-MODULE-SOURCE` is the built-in record, and `MODULE-SOURCE` holds
it when KDOS starts.

| Word | Stack effect | Description |
| --- | --- | --- |
| `MODULE-SOURCE!` | `( source -- )` | Select the source later loads use.  Core 0 only. |
| `FS-MODULE-SOURCE` | `( -- source )` | KDOS's MP64FS as a source. |

The owner keeps a record alive, at the same address, while it is selected and
while any load that started from it is still under way.  Before it retires a
source, for example when its volume is unmounted, it selects another one;
`FS-MODULE-SOURCE MODULE-SOURCE!` returns to the built-in source.

A file being loaded may select a source.  This is how a boot script on the
boot volume hands module loading over to a volume it has just mounted.  When
the load that made the selection ends without a failure, the new source stays
selected.  When that load fails, the loader selects the source the load
started from again, because the failed load's definitions, among them
possibly the new source's own words, are removed from the dictionary.

## The calls of one load

For each `LOAD` and each `REQUIRE` the loader makes these calls, in this
order, on the source that is selected when the load begins.

1. DIR@, once.  The loader keeps the token until the load ends.
2. RESOLVE, with a cleared file record: once, with the path as it was
   written.  When `MODULE-TOKENS` is set it is called first with that path
   and a `t` added, which names the module's token form, and then, if that
   names no file, with the path itself.
3. READ, at most once, and only after a RESOLVE that returned zero.  The
   loader makes no other call to the source between a RESOLVE and its READ.
4. The calls of every load nested in the file's text, each complete before
   the next line is evaluated.
5. DIR!, once, with the token of step 1.  The loader makes this call however
   the load ended: after a file that loaded, a path that named no file, a
   file it chose not to read, and a failure thrown from any step after 1.

Module loading is core-0 work, and the loader finishes one call before it
makes the next, so a source needs no lock of its own against the loader.
`REQUIRE` and `MODULE-SOURCE!` refuse other cores.  `LOAD` has no such check
and shares the loader's state, so it belongs on core 0 as well.

Loads nest at most 16 deep; a deeper one aborts with `REQUIRE nested too
deep` before it calls the source.  An `ABORT` or `ABORT"` is not a throw.
It returns to the interpreter past the loader, as it always has, and the
loads under way at that moment end without step 5.

Because READ may never come, RESOLVE acquires nothing that only READ would
give back.  A source that needs an open file to read opens and closes it
inside READ.

## RESOLVE

`( c-addr u file source -- ior )` finds the file that the path `c-addr u`
names and fills the file record.  The path is the next blank-delimited word
of the input, at most 127 bytes; a longer word is cut to 127.  The span
belongs to the loader, and the source reads it during the call only.

The source reads a path the way its filesystem does.  The loader relies on
one rule: a relative path starts at the source's current directory, and a
successful RESOLVE makes the directory that holds the file current.  A path
inside a file being loaded therefore starts at that file's own directory.
The built-in source also takes a leading `/` as its root and `..` as the
parent directory, and module files are written in that convention, so a
source for another filesystem gives those two forms the same meaning.

RESOLVE returns zero when it has found the file and filled the record.  It
returns `MS-E-NOT-FOUND` when the path names no file and
`MS-E-NO-FILESYSTEM` when the source has no filesystem to look in, and the
loader reports those two (see [Reports](#reports)).  For a failure of its
own, such as an I/O error, it returns another nonzero value, which the
loader throws, or throws one itself; the caller of `LOAD` or `REQUIRE`
receives the value either way.  A RESOLVE that returns `MS-E-NOT-FOUND`
leaves the source's current directory as it was, because the loader may ask
for another name from the same place.  After any other failure it may leave
any directory current, since the loader gives the saved one back.

## READ

`( addr file source -- ior )` copies the file's text to `addr`.  `file` is
the record its RESOLVE filled, unchanged.  The loader has allocated
`MF.RESERVE` bytes at `addr`, and READ may write all of them; the first
`MF.USED` bytes are the text the loader evaluates.  A source whose device
reads whole sectors or blocks asks for the rounded size in `MF.RESERVE`
and reads straight into the buffer.

READ returns zero when the text is in the buffer.  It reports a failure by
returning a nonzero value, which the loader throws, or by throwing one.  The
throw passes through the load's guard: the definitions and module names the
load had published are removed, the buffer is freed, and the caller of
`LOAD` or `REQUIRE` receives the value.

The loader does not call READ for a file with `MF.USED` zero, for a file
`REQUIRE` already knows by its identity, or when it cannot allocate the
buffer.

## Directory tokens

DIR@ `( source -- token )` returns one cell that stands for the source's
current directory, and DIR! `( token source -- )` makes that directory
current again.  The loader does not interpret the token.  It gives every
token back to the source it came from exactly once, so a token may own
something that DIR! releases, such as a reference that keeps a directory in a
cache.  Loads nest, so a source gets its tokens back in the reverse of the
order in which it gave them.  Neither word throws: DIR! runs while the loader
is passing a load's own failure on, and a throw there would replace it.

When a load selects another source, the token still goes back to the source
the load started from, which gets its directory back although it is no
longer selected.

## File identity

`REQUIRE` reads a file to find its `PROVIDED` line, so a module that many
others name would be read once for each of them.  An identity lets the loader
skip that read.  The two cells of `MF.ID` say which file RESOLVE found, as
far as the source can tell without reading it: an inode number with its
generation, size and modification time, for example.  Both cells zero mean
the source gives no identity.

The module registry records an identity, together with the address of the
source record that gave it, beside the module the file provides.  `REQUIRE`
makes the record when it has read a file and found its `PROVIDED` line,
before it evaluates the rest, and also when that line names a module that is
already loaded, as a second copy of a module on another volume does.  From
then on `REQUIRE` returns without calling READ when RESOLVE gives that
identity again, which is also how a file that requires itself, directly or
through others, is read once.  The record is provisional until the load
ends, as the module's name is, so a failed load removes both.

Two rules follow for a source.

- Equal identities from one record mean the same file with the same
  content, for as long as the system runs.  A file's identity changes when
  its content does.
- A record that comes to serve another volume at the same address gives
  identities that cannot equal the earlier ones.  Mixing a number that is
  new for each mount into the identity does this.

A file without a `PROVIDED` line names no module, so its identity is never
recorded and `REQUIRE` evaluates it every time.  `LOAD` neither consults nor
records identities.  A file whose source gives no identity is read on every
`REQUIRE`, and its `PROVIDED` line still keeps it from being evaluated twice.

`MODULES` lists module names and their count; the registry's records of
loaded files are not modules and do not appear there.

## Reports

| Outcome | `LOAD` | `REQUIRE` |
| --- | --- | --- |
| RESOLVE returns `MS-E-NO-FILESYSTEM` | prints ` No filesystem` | the same |
| RESOLVE returns `MS-E-NOT-FOUND` | prints ` Not found: ` and the path | prints ` Module not found: ` and the path |
| RESOLVE returns another nonzero value | throws it | throws it |
| `MF.USED` is zero | prints ` Empty file` | prints ` Empty module` |
| the buffer cannot be allocated | prints ` File buffer allocation failed` | prints ` Module buffer allocation failed` |
| READ returns nonzero | throws it | throws it |

The path is printed as it was written.  A printed outcome is not a throw:
the word returns normally, having evaluated nothing.  Both words have
consumed the path from the input by the time any of these is reported.

## The built-in source

`FS-MODULE-SOURCE` serves the MP64FS volume KDOS mounted at boot.

- RESOLVE mounts the filesystem if `FS-ENSURE` can, walks the path with
  `_RESOLVE-PATH`, and looks the final name up in the directory it reached.
  When the name is not there it puts `CWD` back to where the walk began.
  It leaves the path in `PATHBUF` and `NAMEBUF`, as `PARSE-NAME` does.
- The current directory is `CWD`, and a token is its value: a directory's
  slot, or 255 for the root.
- The handle is the file's directory slot.  `MF.RESERVE` is the size of the
  file's two extents in whole sectors.
- The identity is the file's checksum field and used length in the first
  cell, and its modification time, first sector and slot in the second.
- READ reads both extents and reports a media failure by throwing the
  value of `DISK-IO-IOR`, as the loader's reads always have.

Its path handling keeps two limits of KDOS's path scratch.  A path component
is at most 23 bytes.  A component that is missing or is not a directory
prints ` Path component not found: ` and the component, and the lookup then
goes on with that component's name in place of the final name, so a file of
that name in the directory reached so far is resolved instead.

## What stays outside

`APP-LOAD` and the file commands (`CAT`, `DIR`, `OPEN` and the rest) use
MP64FS directly and do not go through a source.  At startup KDOS runs
`autoexec.f` from the MP64FS root through the source selected then, which is
the built-in one.  The contract covers reading modules only; a source has no
operation that writes.
