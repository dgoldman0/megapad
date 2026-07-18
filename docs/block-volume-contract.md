# Block-device, volume, and partition contract

This document defines the KDOS storage-object ABI used by filesystems and by
future VFS instances.  It sits above the byte-addressed controller contract in
`storage-controller-contract.md`: a block device binds one physical media
generation, while a volume exposes a validated half-open slice of that block
device.

All sizes and offsets below are ABI.  Callers allocate the objects; KDOS
initializes and validates them.  Multi-byte object fields are native 64-bit
cells unless a field is explicitly described as bytes.

## Constants and ownership

| Constant | Value | Meaning |
| --- | ---: | --- |
| `STORAGE-ABI` | 1 | Object ABI version |
| `/BLOCK-DEVICE` | 128 | Bytes in one block-device descriptor |
| `/VOLUME` | 144 | Bytes in one volume descriptor |
| `SECTOR` | 512 | Logical sector size supported by this backend |
| `PART-WORKSPACE-MIN` | 5120 | Minimum caller-owned partition workspace |

`BD-OPEN` creates one block-device reference to the currently attached
medium.  `VOL-RAW`, `VOL-SLICE`, and successful partition discovery create
volume references and increment the parent block device's reference count.
`VOL-CLOSE` clears one volume and releases that reference.  `BD-CLOSE` refuses
to clear a block device while any volume still refers to it.

Close operations are idempotent for already-invalid objects.  A caller must
not copy a live descriptor byte-for-byte: cookies and reference ownership
belong to the original object address.

A descriptor destination is caller-owned lifecycle state, not arbitrary
uninitialized storage.  Before its first construction it must be zero-filled;
on later construction it may instead contain the same caller's live object,
which the constructor will replace according to the operation's transactional
rules.  Likewise, every slot passed to a partition scanner must be either
zero-filled or an original live volume owned by that caller.  This lets
replacement and failure release parent references without interpreting random
bytes as an object.

Descriptor construction, rebinding, selection, and close are management-plane
operations.  Callers serialize them on core 0 (or with their own enclosing
administrative lock) and must not race them against I/O using the same object.
The public partition scanners provide that serialization internally.  Once
objects are stable, `BD-READ`/`WRITE`/`FLUSH` and volume I/O serialize complete
controller requests through the checked block lock; filesystem bindings add
their own cache/namespace locking above that boundary.

## Block-device ABI

| Offset | Bytes | Field | Public reader | Meaning |
| ---: | ---: | --- | --- | --- |
| 0 | 8 | magic | — | `BLOCK-DEVICE-MAGIC` when valid |
| 8 | 8 | ABI | — | `STORAGE-ABI` |
| 16 | 8 | cookie | `BD.COOKIE` | Nonzero object identity |
| 24 | 8 | backend kind | — | 1 for the Megapad controller |
| 32 | 8 | backend identity | — | Controller/backend instance identity |
| 40 | 8 | media generation | `BD.MEDIA-GEN` | Attachment generation captured at open |
| 48 | 8 | sector size | `BD.SECTOR-SIZE` | 512 |
| 56 | 8 | sector count | `BD.SECTORS` | Exact attached capacity |
| 64 | 8 | capabilities | `BD.CAPS` | Controller capabilities captured at open |
| 72 | 8 | flags | `BD.FLAGS` | Includes `VOL-F-READONLY` when writes are unavailable |
| 80 | 8 | synchronization kind | — | Backend synchronization contract |
| 88 | 8 | volume references | `BD.REFS` | Address of the mutable reference-count cell |
| 96 | 8 | last ior | — | Diagnostic result from the last submitted operation |
| 104 | 8 | last completed | — | Confirmed sectors from the last submitted operation |
| 112 | 8 | last LBA | — | Diagnostic accepted-request LBA |
| 120 | 8 | last count | — | Diagnostic accepted-request sector count |

`BD-VALID?` requires the magic, ABI, nonzero cookie, 512-byte sector size,
nonzero capacity, and all mandatory controller capabilities.  The mandatory
mask is `BD-REQUIRED-CAPS = 0x79`: read, precise result, completion counter,
media generation, and atomic generation guard.  Write and flush remain
discoverable rather than mandatory so read-only backends can be represented.

The public block-device words are:

| Word | Stack effect | Contract |
| --- | --- | --- |
| `BD-OPEN` | `( bd -- ior )` | Clear and bind a descriptor to the current attachment |
| `BD-CLOSE` | `( bd -- ior )` | Clear an unreferenced descriptor; return busy while volumes exist |
| `BD-VALID?` | `( bd -- flag )` | Validate permanent descriptor structure and capabilities |
| `BD-STALE?` | `( bd -- flag )` | Compare the saved attachment generation with the controller |
| `BD-READ` | `( dma lba count bd -- completed ior )` | Guarded read into DMA memory |
| `BD-WRITE` | `( dma lba count bd -- completed ior )` | Guarded write from DMA memory |
| `BD-FLUSH` | `( bd -- ior )` | Guarded durability barrier |

## Volume ABI

| Offset | Bytes | Field | Public reader | Meaning |
| ---: | ---: | --- | --- | --- |
| 0 | 8 | magic | — | `VOLUME-MAGIC` when valid |
| 8 | 8 | ABI | — | `STORAGE-ABI` |
| 16 | 8 | cookie | `VOL.COOKIE` | Nonzero object identity |
| 24 | 8 | block device | `VOL.BD` | Parent descriptor address |
| 32 | 8 | block cookie | `VOL.BD-COOKIE` | Parent identity captured at creation |
| 40 | 8 | media generation | `VOL.MEDIA-GEN` | Parent generation captured at creation |
| 48 | 8 | base LBA | `VOL.BASE` | First parent-sector LBA in the slice |
| 56 | 8 | sector count | `VOL.SECTORS` | Length of the slice |
| 64 | 8 | sector size | `VOL.SECTOR-SIZE` | Inherited from the parent |
| 72 | 8 | flags | `VOL.FLAGS` | Inherited access flags |
| 80 | 8 | scheme | `VOL.SCHEME` | `RAW`, `MBR`, or `GPT` |
| 88 | 8 | index | `VOL.INDEX` | Zero-based source-table slot; zero for raw |
| 96 | 8 | MBR type | — | Partition type byte widened to a cell; zero otherwise |
| 104 | 16 | GPT type GUID | — | Raw 16-byte type GUID; zero for non-GPT volumes |
| 120 | 16 | GPT unique GUID | — | Raw 16-byte unique GUID; zero for non-GPT volumes |
| 136 | 8 | attributes/boot | — | GPT attributes, or MBR boot marker 0/`0x80` |

Scheme values are `VOL-SCHEME-RAW = 0`, `VOL-SCHEME-MBR = 1`, and
`VOL-SCHEME-GPT = 2`.  `VOL-F-READONLY = 1` is the currently defined access
flag.

`VOL-VALID?` validates the volume and its parent, requires the saved parent
cookie to match, and revalidates the complete slice against the parent
capacity.  `VOL-STALE?` additionally requires both saved generations to match
and asks the parent whether its attachment is still current.

The public volume words are:

| Word | Stack effect | Contract |
| --- | --- | --- |
| `VOL-SLICE` | `( base length scheme index bd vol -- ior )` | Construct one validated bounded slice |
| `VOL-RAW` | `( bd vol -- ior )` | Construct the identity slice `[0, BD.SECTORS)` |
| `VOL-CLOSE` | `( vol -- ior )` | Clear the volume and release its parent reference |
| `VOL-VALID?` | `( vol -- flag )` | Validate object identity, parent identity, and bounds |
| `VOL-STALE?` | `( vol -- flag )` | Validate the complete attachment-generation chain |
| `VOL-READ` | `( dma lba count vol -- completed ior )` | Read relative sectors from the slice |
| `VOL-WRITE` | `( dma lba count vol -- completed ior )` | Write relative sectors to the slice |
| `VOL-FLUSH` | `( vol -- ior )` | Flush through the parent block device |

Volume I/O translates a relative request only after validation:

```text
parent_lba = VOL.BASE + relative_lba
```

The range predicate is deliberately subtraction-based and treats cells as
unsigned.  A request is valid exactly when `count > 0`, `count <= length`, and
`lba <= length - count`.  Thus the sector ending exactly at the volume's end
is valid, zero-length requests are invalid, and arithmetic wraparound cannot
turn an out-of-range request into an accepted one.

## Generation safety and completion

A descriptor's media generation is part of its authority.  Every operation
first rejects an already-stale descriptor in software.  Reads, writes, and
flushes then submit the saved generation through the controller's guarded
command register, so a swap between the software check and command acceptance
still fails atomically.

The generation is u32, so an unavoidable ABA horizon remains after `2^32`
attachment events: guarded acceptance eliminates ordinary swap races but
cannot distinguish mathematical identity reuse after wraparound.

On a guarded generation mismatch:

- the controller confirms zero sectors and reports `MEDIA_REMOVED`;
- KDOS returns a nonzero ior carrying the stale flag;
- no DMA read or write occurs;
- replacement-media bytes are not read or modified; and
- a stale flush performs no durability operation.

Callers must treat `completed` as authoritative even on failure.  A zero ior
is success only when the requested count was confirmed in full.  Partial
controller results retain their raw cause and carry `IOR-F-PARTIAL`.

## Structured ior values

The low 32 bits of a storage ior are stable:

| Bits | Reader | Meaning |
| --- | --- | --- |
| 7..0 | `IOR>RAW` | Unmodified controller or source cause |
| 15..8 | `IOR>CODE` | Stable KDOS error class |
| 23..16 | `IOR>DOMAIN` | Block, device, volume, or partition domain |
| 31..24 | `IOR>FLAGS` | Partial, retryable, stale, corrupt, unsupported, or read-only |

Zero alone denotes success.  Public predicates include `IOR-PARTIAL?` and
`IOR-STALE?`.  The stable domains are block 1, device 2, volume 3, and
partition 4.  Range errors use code 18; partition capacity and workspace
errors use codes 21 and 22 respectively.

## Partition discovery API

All three scanners have the same caller-owned interface:

```forth
MBR-SCAN   ( bd volumes max workspace bytes -- count ior )
GPT-SCAN   ( bd volumes max workspace bytes -- count ior )
PART-SCAN  ( bd volumes max workspace bytes -- count ior )
```

`volumes` points to `max * /VOLUME` writable bytes whose slots satisfy the
zero-or-live lifecycle rule above.  `workspace` points to at least
`PART-WORKSPACE-MIN` writable bytes.  The buffers must not overlap the live
block-device descriptor.  `count` is the number of consecutive valid volume
objects beginning at `volumes`.

The public scanners serialize their shared parser state with machine
spinlock 0; callers must not enter them while already holding that lock.
Individual block transfers retain the checked storage layer's own lock.

Discovery is transactional.  Once `volumes` and `max` identify a writable
output extent, the scanner first closes every old object in it, stages
candidate slices without magic or parent references, validates the complete
table, rechecks media generation, and only then finalizes every volume.  Any
later error—including invalid device identity, insufficient workspace, or
insufficient output capacity—leaves every output slot clear and returns
`count = 0`; callers never receive either a stale prior result or a valid
prefix from a rejected table.  A null/zero output extent is rejected without
dereferencing it.

`PART-SCAN` is the policy entry point.  It recognizes a protective MBR and
dispatches GPT, applies MBR validation to a nonempty ordinary table, and
publishes one raw identity volume when sector zero has no MBR signature or
has a signed but empty table.  Direct `MBR-SCAN` and `GPT-SCAN` are available
for explicit formats.  A protective MBR is GPT metadata and is never
published as an ordinary MBR volume.

### MBR validation

MBR discovery requires:

- the `0x55AA` signature at bytes 510..511;
- at most four primary entries;
- a boot marker of either 0 or `0x80`;
- nonempty entries with nonzero type, start LBA, and sector count;
- each half-open extent to fit completely within the block device; and
- no overlap between published extents.

An entry whose type is zero must also have zero boot marker, start, and count.
Extended-container types `0x05`, `0x0F`, and `0x85` are unsupported; KDOS does
not walk EBR chains.  A protective `0xEE` table is valid only as the sole
entry, beginning at LBA 1, covering the remaining addressable medium, and
with no boot marker.  `MBR-SCAN` reports that valid protective form as
unsupported so `PART-SCAN` can dispatch it to GPT.  `VOL.INDEX` preserves the
zero-based MBR table slot, including holes.

### GPT validation

GPT discovery requires a valid protective MBR and validates both GPT copies
before publishing any volume.  Validation includes:

- the `EFI PART` signature and supported revision;
- header-size bounds, the zero reserved field, and a zero reserved logical-
  block tail;
- each header's CRC-32 with its CRC field treated as zero;
- current, backup, usable-range, and entry-array LBAs within the device;
- supported nonzero entry count and entry size;
- overflow-safe entry-array byte and sector extents;
- matching primary/backup usable bounds, disk GUID, header size, entry count,
  entry size, and entry-array CRC declaration;
- valid nonoverlapping primary and backup metadata extents outside the usable
  range;
- the CRC-32 of each complete entry array;
- byte-for-byte agreement between the primary and backup entry arrays;
- nonempty entries with `first_lba <= last_lba`;
- every partition contained in the header's usable range and device; and
- no overlap between published partition extents.

An all-zero type GUID denotes an unused entry.  A used entry requires a
nonzero unique GUID, and unique GUIDs may not repeat.  It publishes its type
GUID, unique GUID, and attributes in the scheme-specific metadata area.
`VOL.SECTORS` is the inclusive GPT interval converted to a sector count:
`last_lba - first_lba + 1`.  `VOL.INDEX` is the zero-based entry-array slot.
Names are descriptive metadata and are not required for volume identity.

The current contract does not silently recover one GPT copy from the other.
Both headers, both arrays, and their mutual agreement are required; any
single-copy corruption rejects the table transactionally.

## Qualification requirements

The executable storage-object suite must cover:

- descriptor construction, validation, close, and reference lifetime;
- successful read, write, and flush through both object layers;
- exact-end success, zero-count failure, out-of-range failure, and unsigned
  wraparound rejection;
- raw identity translation and bounded-slice translation;
- a media swap at guarded-command acceptance proving stale fail-closed
  behavior and replacement-media non-mutation;
- valid external MBR and GPT images; and
- corrupt signatures, CRCs, extents, capacity, and workspace with no partial
  volume publication.
