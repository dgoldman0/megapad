"""Prepare one semantic runtime from the BIOS-selected MP64FS Forth file.

The executable BIOS owns disk discovery and the initial ``FSLOAD`` on the
emulator.  The semantic simulator deliberately substitutes that instruction
path, but retains its observable selection and validation rules: media is read
through the hosted storage service, marker-1 geometry and metadata are checked,
and the first occupied Forth entry is loaded across both validated extents.

Preparation stops before the ordinary autoexec body.  Its one top-level call
is blanked byte-for-byte in the loaded source, then a deferred session root is
installed.  Exact KDOS source-walker overlays are admitted at that boundary
without replacing their guest definitions.  Running the root later, under
:class:`SimulatorMachineSession`, loads the normal autoexec closure inside the
resumable semantic dispatch and continues through the Akashic-selected session
entry.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared.mp64fs import (
    MP64FS_BITMAP_START,
    MP64FS_DIRECTORY_BYTES,
    MP64FS_DIRECTORY_SECTORS,
    MP64FS_ENTRY_SIZE,
    MP64FS_MAX_FILES,
    MP64FSGeometry,
    bios_mp64fs_metadata_valid,
    decode_bios_mp64fs_geometry,
)
from shared.storage import SECTOR_SIZE, STORAGE_RESULT_OK, STORAGE_STATUS_PRESENT
from simulator.memory import SparseAddressSpace
from simulator.runtime import MegaForthRuntime
from simulator.source_acceleration import install_kdos_source_accelerators
from simulator.storage import HostedStorageService


_FORTH_FILE_TYPE = 3
_AUTOEXEC_INVOCATION = b"_AUTOEXEC-RUN"
_EVALUATE_BUFFER_BYTES = 256
_FSLOAD_RETURN_STACK_RESERVE = 128
_SESSION_ENTRY = b"_SIMULATOR-SESSION-ENTRY"
_SESSION_ROOT = b"_SIMULATOR-SESSION-ROOT"
_SESSION_ROOT_SOURCE = (
    b"DEFER _SIMULATOR-SESSION-ENTRY\n"
    b": _SIMULATOR-SESSION-ROOT\n"
    b"  _AUTOEXEC-RUN\n"
    b"  _SIMULATOR-SESSION-ENTRY ;\n"
)


class ImageBootstrapError(RuntimeError):
    """The attached image cannot produce one prepared semantic boot."""


@dataclass(frozen=True, slots=True)
class ImageBootstrapPreparation:
    """Complete state immediately before the resumable session dispatch."""

    runtime: MegaForthRuntime
    root_xt: int
    boot_filename: bytes
    geometry: MP64FSGeometry
    source_bytes: int
    source_lines: int
    preparation_semantic_steps: int
    source_accelerators: tuple[bytes, ...]


@dataclass(frozen=True, slots=True)
class _BootFile:
    name: bytes
    primary_start: int
    primary_count: int
    secondary_start: int
    secondary_count: int
    used_bytes: int

    @property
    def transfer_bytes(self) -> int:
        return (self.primary_count + self.secondary_count) * SECTOR_SIZE


def blank_terminal_autoexec_invocation(source: bytes) -> bytes:
    """Blank the sole standalone ``_AUTOEXEC-RUN`` token in ``source``.

    Only the token bytes are replaced with spaces.  Leading/trailing horizontal
    whitespace and every CR/LF byte remain at their original offset, so source
    locations and the physical-line structure do not drift.
    """

    if not isinstance(source, bytes):
        raise TypeError("boot source must be bytes")

    matches: list[tuple[int, int]] = []
    line_start = 0
    while line_start <= len(source):
        newline = source.find(b"\n", line_start)
        line_limit = len(source) if newline < 0 else newline
        content_limit = (
            line_limit - 1
            if line_limit > line_start and source[line_limit - 1] == 0x0D
            else line_limit
        )
        token_start = line_start
        while (
            token_start < content_limit
            and source[token_start] in (0x20, 0x09)
        ):
            token_start += 1
        token_limit = content_limit
        while (
            token_limit > token_start
            and source[token_limit - 1] in (0x20, 0x09)
        ):
            token_limit -= 1
        if source[token_start:token_limit] == _AUTOEXEC_INVOCATION:
            matches.append((token_start, token_limit))

        if newline < 0:
            break
        line_start = newline + 1

    if len(matches) != 1:
        raise ImageBootstrapError(
            "boot source must contain exactly one standalone "
            f"{_AUTOEXEC_INVOCATION.decode('ascii')} invocation; "
            f"found {len(matches)}"
        )

    token_start, token_limit = matches[0]
    transformed = bytearray(source)
    transformed[token_start:token_limit] = b" " * (token_limit - token_start)
    return bytes(transformed)


def prepare_image_bootstrap(
    *,
    memory: SparseAddressSpace,
    storage: HostedStorageService,
) -> ImageBootstrapPreparation:
    """Construct and prepare a runtime from one explicit memory/storage pair.

    The caller owns platform geometry and attached-media construction.  This
    seam claims ``storage`` by constructing the runtime, reads the boot file
    through that service, evaluates every physical line through execution
    tokens captured from the initial semantic BIOS, and returns the exact root
    XT that a session must dispatch.
    """

    if not isinstance(memory, SparseAddressSpace):
        raise TypeError("memory must be a SparseAddressSpace")
    if not isinstance(storage, HostedStorageService):
        raise TypeError("storage must be a HostedStorageService")

    runtime = MegaForthRuntime(memory=memory, storage=storage)
    checked = runtime.find(b"EVALUATE-CHECKED")
    finish = runtime.find(b"EVALUATE-FINISH")
    eval_line = runtime.find(b"EVAL-LINE")
    if checked is None or finish is None or eval_line is None:
        raise ImageBootstrapError("semantic BIOS evaluator vocabulary is incomplete")

    geometry, boot_file, source = _read_bios_boot_file(runtime)
    prepared_source = blank_terminal_autoexec_invocation(source)
    semantic_steps = _evaluate_checked_source(
        runtime,
        source=prepared_source,
        source_name=boot_file.name.decode("ascii", errors="backslashreplace"),
        evaluator_xt=checked.xt,
        finish_xt=finish.xt,
        line_address=eval_line.body_address,
    )
    semantic_steps += _evaluate_checked_source(
        runtime,
        source=_SESSION_ROOT_SOURCE,
        source_name="simulator/session-root.f",
        evaluator_xt=checked.xt,
        finish_xt=finish.xt,
        line_address=eval_line.body_address,
    )

    root = runtime.find(_SESSION_ROOT)
    entry = runtime.find(_SESSION_ENTRY)
    if root is None or entry is None:
        raise ImageBootstrapError("simulator session root was not published")
    acceleration = install_kdos_source_accelerators(runtime)
    if runtime.main_context.data.snapshot() or runtime.main_context.returns.snapshot():
        raise ImageBootstrapError("semantic image preparation left dirty stacks")

    return ImageBootstrapPreparation(
        runtime=runtime,
        root_xt=root.xt,
        boot_filename=boot_file.name,
        geometry=geometry,
        source_bytes=len(source),
        source_lines=_physical_line_count(source),
        preparation_semantic_steps=semantic_steps,
        source_accelerators=acceleration.installed,
    )


def _read_bios_boot_file(
    runtime: MegaForthRuntime,
) -> tuple[MP64FSGeometry, _BootFile, bytes]:
    storage = runtime.storage
    if not storage.status & STORAGE_STATUS_PRESENT:
        raise ImageBootstrapError("no MP64FS media is attached")

    generation = storage.media_generation
    scratch = runtime.main_context.data.empty_pointer
    _require_fsload_capacity(runtime, MP64FS_DIRECTORY_BYTES)
    _read_checked(
        runtime,
        dma=scratch,
        lba=0,
        count=1,
        generation=generation,
        purpose="superblock",
    )
    geometry = decode_bios_mp64fs_geometry(
        runtime.memory.read_bytes(scratch, SECTOR_SIZE),
        storage.total_sectors,
    )
    if geometry is None:
        raise ImageBootstrapError("attached media is not canonical marker-1 MP64FS")

    bitmap_address = scratch + MP64FS_DIRECTORY_BYTES
    _require_fsload_capacity(
        runtime,
        MP64FS_DIRECTORY_BYTES + geometry.bitmap_sectors * SECTOR_SIZE,
    )
    _read_checked(
        runtime,
        dma=bitmap_address,
        lba=MP64FS_BITMAP_START,
        count=geometry.bitmap_sectors,
        generation=generation,
        purpose="allocation bitmap",
    )
    _read_checked(
        runtime,
        dma=scratch,
        lba=geometry.directory_start,
        count=MP64FS_DIRECTORY_SECTORS,
        generation=generation,
        purpose="directory",
    )
    bitmap = runtime.memory.read_bytes(
        bitmap_address,
        geometry.bitmap_sectors * SECTOR_SIZE,
    )
    directory = runtime.memory.read_bytes(scratch, MP64FS_DIRECTORY_BYTES)
    if not bios_mp64fs_metadata_valid(bitmap, directory, geometry):
        raise ImageBootstrapError("MP64FS allocation or directory metadata is invalid")
    if storage.media_generation != generation:
        raise ImageBootstrapError("MP64FS media changed during metadata validation")

    boot_file = _select_first_forth_file(directory)
    _require_fsload_capacity(runtime, boot_file.transfer_bytes)
    _read_checked(
        runtime,
        dma=scratch,
        lba=boot_file.primary_start,
        count=boot_file.primary_count,
        generation=generation,
        purpose="primary boot extent",
    )
    if boot_file.secondary_count:
        _read_checked(
            runtime,
            dma=scratch + boot_file.primary_count * SECTOR_SIZE,
            lba=boot_file.secondary_start,
            count=boot_file.secondary_count,
            generation=generation,
            purpose="secondary boot extent",
        )
    if storage.media_generation != generation:
        raise ImageBootstrapError("MP64FS media changed while reading boot source")
    return (
        geometry,
        boot_file,
        runtime.memory.read_bytes(scratch, boot_file.used_bytes),
    )


def _select_first_forth_file(directory: bytes) -> _BootFile:
    for slot in range(MP64FS_MAX_FILES):
        offset = slot * MP64FS_ENTRY_SIZE
        entry = directory[offset : offset + MP64FS_ENTRY_SIZE]
        if entry[0] == 0 or entry[32] != _FORTH_FILE_TYPE:
            continue
        terminator = entry[:24].find(b"\x00")
        if terminator <= 0:
            raise ImageBootstrapError(
                f"first MP64FS Forth entry in slot {slot} has no BIOS-loadable name"
            )
        return _BootFile(
            name=entry[:terminator],
            primary_start=int.from_bytes(entry[24:26], "little"),
            primary_count=int.from_bytes(entry[26:28], "little"),
            used_bytes=int.from_bytes(entry[28:32], "little"),
            secondary_start=int.from_bytes(entry[44:46], "little"),
            secondary_count=int.from_bytes(entry[46:48], "little"),
        )
    raise ImageBootstrapError("MP64FS contains no occupied Forth boot entry")


def _read_checked(
    runtime: MegaForthRuntime,
    *,
    dma: int,
    lba: int,
    count: int,
    generation: int,
    purpose: str,
) -> None:
    completed, status = runtime.storage.read_checked(
        runtime.memory,
        dma,
        lba,
        count,
        generation=generation,
    )
    if status != STORAGE_RESULT_OK or completed != count:
        raise ImageBootstrapError(
            f"MP64FS {purpose} read failed: status={status}, "
            f"completed={completed}, expected={count}"
        )


def _require_fsload_capacity(runtime: MegaForthRuntime, byte_count: int) -> None:
    scratch = runtime.main_context.data.empty_pointer
    return_limit = (
        runtime.main_context.returns.empty_pointer - _FSLOAD_RETURN_STACK_RESERVE
    )
    if scratch > return_limit or byte_count > return_limit - scratch:
        raise ImageBootstrapError(
            "MP64FS boot source exceeds the BIOS-compatible Bank-0 load buffer"
        )


def _evaluate_checked_source(
    runtime: MegaForthRuntime,
    *,
    source: bytes,
    source_name: str,
    evaluator_xt: int,
    finish_xt: int,
    line_address: int,
) -> int:
    source_address = runtime.main_context.data.empty_pointer
    semantic_steps = 0
    for line_number, line in _physical_lines(source):
        runtime.memory.write64(line_address, line_number)
        if len(line) <= _EVALUATE_BUFFER_BYTES - 1:
            runtime.memory.write_bytes(source_address, line)
        runtime.main_context.data.push(source_address)
        runtime.main_context.data.push(len(line))
        result = runtime.execute(evaluator_xt)
        semantic_steps += result.semantic_steps
        status = runtime.main_context.data.pop()
        if status != 0:
            raise ImageBootstrapError(
                f"checked EVALUATE failed at {source_name}:{line_number}: "
                f"status={status}, source={line!r}"
            )

    result = runtime.execute(finish_xt)
    semantic_steps += result.semantic_steps
    status = runtime.main_context.data.pop()
    if status != 0:
        raise ImageBootstrapError(
            f"EVALUATE-FINISH failed for {source_name}: status={status}"
        )
    return semantic_steps


def _physical_lines(source: bytes) -> tuple[tuple[int, bytes], ...]:
    lines: list[tuple[int, bytes]] = []
    for line_number, raw_line in enumerate(source.split(b"\n"), start=1):
        if raw_line.endswith(b"\r"):
            raw_line = raw_line[:-1]
        if raw_line:
            lines.append((line_number, raw_line))
    return tuple(lines)


def _physical_line_count(source: bytes) -> int:
    if not source:
        return 0
    return source.count(b"\n") + (0 if source.endswith(b"\n") else 1)


__all__ = [
    "ImageBootstrapError",
    "ImageBootstrapPreparation",
    "blank_terminal_autoexec_invocation",
    "prepare_image_bootstrap",
]
