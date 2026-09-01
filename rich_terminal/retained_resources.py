"""Owner-bounded immutable RGBA resources and the one upload lifecycle.

Resources deliberately live outside retained scene transactions.  The store
charges committed bytes and the one open upload against OWNER_OPEN quotas,
consumes RESOURCE namespace high-water at successful BEGIN, and publishes only
digest-verified immutable bytes.  Scene references are supplied to DROP by the
coordinator rather than introducing a reverse dependency on scene internals.
"""

from __future__ import annotations

import hashlib
import mmap
import operator
import os
from dataclasses import dataclass, field
from enum import Enum
from types import MappingProxyType
from typing import Mapping

from .apt1 import UINT32_MAX, UINT64_MAX
from .retained_model import (
    ItemNamespace,
    OwnerIdentity,
    OwnerLedger,
    OwnerLedgerError,
    OwnerLedgerErrorCode,
    OwnerLedgerState,
    PreparedOwnerLedgerInstall,
    ResourceFormat,
    RetainedFeature,
)


class ResourceStoreErrorCode(str, Enum):
    INVALID = "INVALID"
    STALE_OWNER = "STALE_OWNER"
    NO_CAPACITY = "NO_CAPACITY"
    DUPLICATE_ID = "DUPLICATE_ID"
    IN_USE = "IN_USE"
    BAD_CONTENT = "BAD_CONTENT"
    STATE = "STATE"


class ResourceStoreError(ValueError):
    """Deterministic resource lifecycle failure suitable for RET_RESULT."""

    def __init__(
        self,
        code: ResourceStoreErrorCode,
        detail: str,
        *,
        prepared: PreparedResourceInstall | None = None,
    ):
        self.code = code
        self.detail = detail
        self.prepared = prepared
        super().__init__(f"{code.value}: {detail}")


_VERIFIED_RESOURCE = object()


def _integer(name: str, value, *, minimum: int, maximum: int) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not minimum <= result <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return int(result)


def _digest(value) -> bytes:
    if not isinstance(value, (bytes, bytearray, memoryview)):
        raise TypeError("digest must be bytes-like")
    result = bytes(value)
    if len(result) != 32:
        raise ValueError("digest must be exactly 32 bytes")
    return result


def _resource_key(owner: OwnerIdentity, resource_id: int) -> tuple[int, int, int]:
    return owner.owner_id, owner.owner_generation, resource_id


def _owner_key(owner: OwnerIdentity) -> tuple[int, int]:
    return owner.owner_id, owner.owner_generation


@dataclass(frozen=True, slots=True)
class ResourceDeclaration:
    resource_id: int
    format: ResourceFormat
    width: int
    height: int
    byte_length: int
    digest: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "resource_id",
            _integer(
                "resource_id", self.resource_id, minimum=1, maximum=UINT64_MAX
            ),
        )
        if isinstance(self.format, bool):
            raise TypeError("format must not be bool")
        try:
            resource_format = ResourceFormat(self.format)
        except (TypeError, ValueError) as exc:
            raise ValueError("format is not a retained resource format") from exc
        object.__setattr__(self, "format", resource_format)
        for name in ("width", "height"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=1,
                    maximum=UINT32_MAX,
                ),
            )
        object.__setattr__(
            self,
            "byte_length",
            _integer(
                "byte_length", self.byte_length, minimum=1, maximum=UINT64_MAX
            ),
        )
        expected = self.width * self.height * 4
        if expected > UINT64_MAX or self.byte_length != expected:
            raise ValueError("resource byte length is not width * height * 4")
        object.__setattr__(self, "digest", _digest(self.digest))


class _ImmutableBacking:
    """A read-only mapping whose writable upload alias is retired at commit."""

    __slots__ = ("_mapping", "_length", "_sealed")

    def __init__(self, mapping: mmap.mmap, length: int) -> None:
        self._mapping = mapping
        self._length = length
        self._sealed = False

    def seal(self) -> None:
        self._sealed = True

    def read(self, offset: int, count: int) -> bytes:
        if not self._sealed:
            raise RuntimeError("resource backing is not committed")
        return self._mapping[offset : min(self._length, offset + count)]

    def view(self) -> memoryview:
        if not self._sealed:
            raise RuntimeError("resource backing is not committed")
        view = memoryview(self._mapping)
        if not view.readonly:
            raise RuntimeError("immutable resource mapping is unexpectedly writable")
        return view

    def close(self) -> None:
        mapping = self._mapping
        if mapping is not None:
            mapping.close()
            self._mapping = None

    def __del__(self) -> None:
        try:
            self.close()
        except (BufferError, OSError):
            # An exported read-only view owns the mmap object independently;
            # it will close when that final consumer releases it.
            pass


class _UploadBacking:
    """One exact-size dual mapping allocated and physically reserved at BEGIN."""

    __slots__ = ("token", "_fd", "_write", "immutable")

    def __init__(
        self,
        token: object,
        fd: int,
        writable: mmap.mmap,
        immutable: _ImmutableBacking,
    ) -> None:
        self.token = token
        self._fd = fd
        self._write = writable
        self.immutable = immutable

    @classmethod
    def allocate(cls, byte_length: int) -> _UploadBacking:
        fd = -1
        writable = None
        readable = None
        try:
            fd = os.memfd_create(
                "megapad-retained-rgba",
                flags=getattr(os, "MFD_CLOEXEC", 0),
            )
            os.posix_fallocate(fd, 0, byte_length)
            writable = mmap.mmap(fd, byte_length, access=mmap.ACCESS_WRITE)
            readable = mmap.mmap(fd, byte_length, access=mmap.ACCESS_READ)
            return cls(
                object(),
                fd,
                writable,
                _ImmutableBacking(readable, byte_length),
            )
        except Exception:
            if readable is not None:
                readable.close()
            if writable is not None:
                writable.close()
            if fd >= 0:
                os.close(fd)
            raise

    def write(self, offset: int, data: bytes) -> None:
        self._write[offset : offset + len(data)] = data

    def digest(self) -> bytes:
        return hashlib.sha3_256(self._write).digest()

    def commit(self) -> None:
        """Retire every writable handle while preserving the read mapping."""

        writable = self._write
        fd = self._fd
        if writable is None or fd < 0 or self.immutable is None:
            raise RuntimeError("resource upload backing is not open")
        writable.close()
        os.close(fd)
        self.immutable.seal()
        self._write = None
        self._fd = -1
        self.immutable = None

    def abort(self) -> None:
        writable = self._write
        fd = self._fd
        immutable = self.immutable
        self._write = None
        self._fd = -1
        self.immutable = None
        if writable is not None:
            writable.close()
        if fd >= 0:
            os.close(fd)
        if immutable is not None:
            immutable.close()

    def __del__(self) -> None:
        try:
            self.abort()
        except (BufferError, OSError):
            pass


@dataclass(frozen=True, slots=True, init=False)
class RGBAResource:
    owner: OwnerIdentity
    declaration: ResourceDeclaration
    _backing: bytes | _ImmutableBacking = field(
        init=False, repr=False, compare=False
    )

    def __init__(
        self,
        owner: OwnerIdentity,
        declaration: ResourceDeclaration,
        data: bytes | _ImmutableBacking,
        _verification: object | None = None,
    ) -> None:
        object.__setattr__(self, "owner", owner)
        object.__setattr__(self, "declaration", declaration)
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        if not isinstance(self.declaration, ResourceDeclaration):
            raise TypeError("declaration must be ResourceDeclaration")
        if _verification is _VERIFIED_RESOURCE:
            if not isinstance(data, _ImmutableBacking):
                raise TypeError("verified upload backing must be immutable")
        elif not isinstance(data, bytes):
            raise TypeError("public committed resource data must be bytes")
        length = data._length if isinstance(data, _ImmutableBacking) else len(data)
        if length != self.declaration.byte_length:
            raise ValueError("committed resource data length is inconsistent")
        if (
            _verification is not _VERIFIED_RESOURCE
            and hashlib.sha3_256(data).digest() != self.declaration.digest
        ):
            raise ValueError("committed resource digest is inconsistent")
        object.__setattr__(self, "_backing", data)

    @property
    def resource_id(self) -> int:
        return self.declaration.resource_id

    @property
    def width(self) -> int:
        return self.declaration.width

    @property
    def height(self) -> int:
        return self.declaration.height

    @property
    def format(self) -> ResourceFormat:
        return self.declaration.format

    @property
    def byte_length(self) -> int:
        return self.declaration.byte_length

    @property
    def digest(self) -> bytes:
        return self.declaration.digest

    def read(self, offset, max_bytes) -> bytes:
        """Copy one explicitly bounded range from the immutable backing.

        Successful upload COMMIT transfers the already allocated bytearray to
        this resource instead of allocating another full image.  The mutable
        backing is never exposed; callers receive immutable bounded copies.
        """

        normalized_offset = _integer(
            "offset", offset, minimum=0, maximum=self.declaration.byte_length
        )
        normalized_count = _integer(
            "max_bytes", max_bytes, minimum=0, maximum=UINT64_MAX
        )
        end = min(
            self.declaration.byte_length,
            normalized_offset + normalized_count,
        )
        if isinstance(self._backing, bytes):
            return self._backing[normalized_offset:end]
        return self._backing.read(normalized_offset, end - normalized_offset)

    def _readonly_view(self) -> memoryview:
        """Internal zero-copy view for an in-process physical compositor."""

        if isinstance(self._backing, bytes):
            return memoryview(self._backing)
        return self._backing.view()


@dataclass(frozen=True, slots=True)
class ResourceUsage:
    resources: int = 0
    bytes: int = 0

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "resources",
            _integer("resources", self.resources, minimum=0, maximum=UINT32_MAX),
        )
        object.__setattr__(
            self,
            "bytes",
            _integer("bytes", self.bytes, minimum=0, maximum=UINT64_MAX),
        )


@dataclass(frozen=True, slots=True)
class ResourceStoreState:
    resources: Mapping[tuple[int, int, int], RGBAResource]
    usage: Mapping[tuple[int, int], ResourceUsage]


@dataclass(frozen=True, slots=True)
class ResourceUploadView:
    owner: OwnerIdentity
    declaration: ResourceDeclaration
    accepted_bytes: int

    def __post_init__(self) -> None:
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        if not isinstance(self.declaration, ResourceDeclaration):
            raise TypeError("declaration must be ResourceDeclaration")
        object.__setattr__(
            self,
            "accepted_bytes",
            _integer(
                "accepted_bytes",
                self.accepted_bytes,
                minimum=0,
                maximum=self.declaration.byte_length,
            ),
        )


@dataclass(slots=True)
class _ResourceUpload:
    owner: OwnerIdentity
    declaration: ResourceDeclaration
    backing: _UploadBacking
    accepted_bytes: int = 0

    @property
    def token(self) -> object:
        return self.backing.token

    @property
    def view(self) -> ResourceUploadView:
        return ResourceUploadView(
            self.owner,
            self.declaration,
            self.accepted_bytes,
        )


class _UploadInstallMode(Enum):
    PRESERVE = "PRESERVE"
    OPEN = "OPEN"
    APPEND = "APPEND"
    COMMIT = "COMMIT"
    ABORT = "ABORT"


@dataclass(frozen=True, slots=True)
class PreparedResourceInstall:
    """A fully checked resource mutation awaiting ordered-result preflight.

    The candidate captures both store and owner-ledger provenance.  A chunk
    candidate owns its bounded immutable input copy but does not write it into
    upload staging until install; every other candidate is a set of assignment
    targets whose allocations and digest work have already completed.
    """

    state: ResourceStoreState
    accepted_bytes: int
    resource: RGBAResource | None
    upload: ResourceUploadView | None
    _mode: _UploadInstallMode
    _new_upload: _ResourceUpload | None
    _ledger: PreparedOwnerLedgerInstall | None
    _chunk_offset: int | None
    _chunk: bytes | None
    _store_token: object
    _source_state: ResourceStoreState | None
    _source_upload_token: object | None
    _source_accepted_bytes: int | None
    _source_owner_state: OwnerLedgerState | None
    _consumed: bool = False


@dataclass(frozen=True, slots=True)
class PreparedResourceRetirement:
    owner: OwnerIdentity
    state: ResourceStoreState
    _store_token: object
    _source_state: ResourceStoreState | None
    _source_owner_state: OwnerLedgerState | None
    _consumed: bool = False


class RetainedResourceStore:
    """One epoch-scoped immutable resource store and upload staging slot."""

    def __init__(self, owners: OwnerLedger) -> None:
        if not isinstance(owners, OwnerLedger):
            raise TypeError("owners must be OwnerLedger")
        self._owners = owners
        self._state = self._make_state({}, {})
        self._upload: _ResourceUpload | None = None
        self._token = object()

    @property
    def state(self) -> ResourceStoreState:
        return self._state

    @property
    def owner_ledger(self) -> OwnerLedger:
        """The exact authority ledger shared by this resource store."""

        return self._owners

    @property
    def upload(self) -> ResourceUploadView | None:
        return None if self._upload is None else self._upload.view

    def usage(self, owner: OwnerIdentity) -> ResourceUsage:
        self._require_live(owner)
        committed = self._state.usage.get(_owner_key(owner), ResourceUsage())
        upload = self._upload
        if upload is None or upload.owner != owner:
            return committed
        return ResourceUsage(
            committed.resources + 1,
            committed.bytes + upload.declaration.byte_length,
        )

    def begin(
        self,
        owner: OwnerIdentity,
        *,
        resource_id,
        format,
        width,
        height,
        flags,
        byte_length,
        digest,
    ) -> ResourceUploadView:
        prepared = self.prepare_begin(
            owner,
            resource_id=resource_id,
            format=format,
            width=width,
            height=height,
            flags=flags,
            byte_length=byte_length,
            digest=digest,
        )
        self.install_prepared(prepared)
        assert prepared.upload is not None
        return prepared.upload

    def prepare_begin(
        self,
        owner: OwnerIdentity,
        *,
        resource_id,
        format,
        width,
        height,
        flags,
        byte_length,
        digest,
    ) -> PreparedResourceInstall:
        if self._upload is not None:
            raise ResourceStoreError(
                ResourceStoreErrorCode.STATE,
                "a resource upload is already open",
            )
        record = self._require_live(owner)

        # Authority and namespace precedence comes before resource content.
        # An already-consumed ID is DUPLICATE_ID even when the accompanying
        # declaration is malformed.
        try:
            normalized_id = _integer(
                "resource_id", resource_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        try:
            prepared_ledger = self._owners.prepare_item_id(
                owner,
                ItemNamespace.RESOURCE,
                normalized_id,
            )
        except OwnerLedgerError as exc:
            raise self._owner_error(exc) from exc
        try:
            normalized_flags = _integer("flags", flags, minimum=0, maximum=UINT32_MAX)
            declaration = ResourceDeclaration(
                normalized_id,
                format,
                width,
                height,
                byte_length,
                digest,
            )
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        if normalized_flags != 0:
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource flags are nonzero",
            )
        policy = self._owners.policy
        if (
            not policy.features & RetainedFeature.RGBA_IMAGE
            or declaration.format is not ResourceFormat.RGBA8
            or declaration.width > policy.max_image_width
            or declaration.height > policy.max_image_height
            or declaration.byte_length > policy.total_resource_bytes
        ):
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource declaration exceeds advertised image policy",
            )
        assert record.quotas is not None
        committed = self._state.usage.get(_owner_key(owner), ResourceUsage())
        if (
            committed.resources >= record.quotas.resources
            or declaration.byte_length > record.quotas.resource_bytes - committed.bytes
        ):
            raise ResourceStoreError(
                ResourceStoreErrorCode.NO_CAPACITY,
                "resource declaration exceeds owner-wide remaining quota",
            )
        try:
            backing = _UploadBacking.allocate(declaration.byte_length)
        except (MemoryError, OSError, OverflowError) as exc:
            raise ResourceStoreError(
                ResourceStoreErrorCode.NO_CAPACITY,
                "resource upload staging cannot be allocated",
            ) from exc
        self._owners.validate_prepared(prepared_ledger)
        upload = _ResourceUpload(owner, declaration, backing)
        return self._prepared(
            state=self._state,
            mode=_UploadInstallMode.OPEN,
            new_upload=upload,
            ledger=prepared_ledger,
            accepted_bytes=0,
            upload=upload.view,
        )

    def append(
        self,
        owner: OwnerIdentity,
        resource_id,
        offset,
        data,
    ) -> int:
        try:
            prepared = self.prepare_append(owner, resource_id, offset, data)
        except ResourceStoreError as exc:
            if exc.prepared is not None:
                self.install_prepared(exc.prepared)
            raise
        self.install_prepared(prepared)
        return prepared.accepted_bytes

    def prepare_append(
        self,
        owner: OwnerIdentity,
        resource_id,
        offset,
        data,
    ) -> PreparedResourceInstall:
        upload = self._match_upload(owner, resource_id)
        try:
            normalized_offset = _integer(
                "offset", offset, minimum=0, maximum=UINT64_MAX
            )
            if not isinstance(data, (bytes, bytearray, memoryview)):
                raise TypeError("resource chunk data must be bytes-like")
            view = memoryview(data)
            chunk_length = view.nbytes
            if not chunk_length:
                raise ValueError("resource chunk data is empty")
            if chunk_length > self._owners.policy.max_resource_chunk_bytes:
                raise ValueError("resource chunk exceeds advertised maximum")
            if normalized_offset != upload.accepted_bytes:
                raise ValueError("resource chunk offset is not contiguous")
            end = normalized_offset + chunk_length
            if end > UINT64_MAX or end > upload.declaration.byte_length:
                raise ValueError("resource chunk overruns declared byte length")
            chunk = view.tobytes()
        except (TypeError, ValueError) as exc:
            prepared = self._prepared(
                state=self._state,
                mode=_UploadInstallMode.ABORT,
                accepted_bytes=0,
            )
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                str(exc),
                prepared=prepared,
            ) from exc
        return self._prepared(
            state=self._state,
            mode=_UploadInstallMode.APPEND,
            accepted_bytes=end,
            upload=ResourceUploadView(owner, upload.declaration, end),
            chunk_offset=normalized_offset,
            chunk=chunk,
        )

    def commit(self, owner: OwnerIdentity, resource_id) -> RGBAResource:
        try:
            prepared = self.prepare_commit(owner, resource_id)
        except ResourceStoreError as exc:
            if exc.prepared is not None:
                self.install_prepared(exc.prepared)
            raise
        resource = prepared.resource
        assert resource is not None
        self.install_prepared(prepared)
        return resource

    def prepare_commit(
        self,
        owner: OwnerIdentity,
        resource_id,
    ) -> PreparedResourceInstall:
        upload = self._match_upload(owner, resource_id)
        if upload.accepted_bytes != upload.declaration.byte_length:
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource upload is incomplete",
                prepared=self._prepared(
                    state=self._state,
                    mode=_UploadInstallMode.ABORT,
                    accepted_bytes=0,
                ),
            )
        if upload.backing.digest() != upload.declaration.digest:
            raise ResourceStoreError(
                ResourceStoreErrorCode.BAD_CONTENT,
                "resource upload digest does not match declaration",
                prepared=self._prepared(
                    state=self._state,
                    mode=_UploadInstallMode.ABORT,
                    accepted_bytes=0,
                ),
            )
        immutable = upload.backing.immutable
        if immutable is None:
            raise RuntimeError("resource upload lost its immutable mapping")
        resource = RGBAResource(
            owner,
            upload.declaration,
            immutable,
            _VERIFIED_RESOURCE,
        )
        key = _resource_key(owner, upload.declaration.resource_id)
        if key in self._state.resources:
            raise RuntimeError("resource high-water admitted an existing resource")
        resources = dict(self._state.resources)
        resources[key] = resource
        usage = dict(self._state.usage)
        prior = usage.get(_owner_key(owner), ResourceUsage())
        usage[_owner_key(owner)] = ResourceUsage(
            prior.resources + 1,
            prior.bytes + upload.declaration.byte_length,
        )
        return self._prepared(
            state=self._make_state(resources, usage),
            mode=_UploadInstallMode.COMMIT,
            accepted_bytes=upload.declaration.byte_length,
            resource=resource,
        )

    def abort(self, owner: OwnerIdentity, resource_id, reason) -> ResourceUploadView:
        prepared = self.prepare_abort(owner, resource_id, reason)
        self.install_prepared(prepared)
        assert prepared.upload is not None
        return prepared.upload

    def prepare_abort(
        self,
        owner: OwnerIdentity,
        resource_id,
        reason,
    ) -> PreparedResourceInstall:
        upload = self._match_upload(owner, resource_id)
        try:
            normalized_reason = _integer("reason", reason, minimum=0, maximum=0xFFFF)
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        if normalized_reason not in (0, 1, 2):
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource abort reason is not defined",
            )
        view = upload.view
        return self._prepared(
            state=self._state,
            mode=_UploadInstallMode.ABORT,
            accepted_bytes=0,
            upload=view,
        )

    def drop(
        self,
        owner: OwnerIdentity,
        resource_id,
        *,
        in_use: bool,
    ) -> RGBAResource:
        prepared = self.prepare_drop(owner, resource_id, in_use=in_use)
        resource = prepared.resource
        assert resource is not None
        self.install_prepared(prepared)
        return resource

    def prepare_drop(
        self,
        owner: OwnerIdentity,
        resource_id,
        *,
        in_use: bool,
    ) -> PreparedResourceInstall:
        if self._upload is not None:
            raise ResourceStoreError(
                ResourceStoreErrorCode.STATE,
                "resource drop crossed an open upload",
            )
        self._require_live(owner)
        try:
            normalized_id = _integer(
                "resource_id", resource_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        if not isinstance(in_use, bool):
            raise TypeError("in_use must be bool")
        key = _resource_key(owner, normalized_id)
        resource = self._state.resources.get(key)
        if resource is None:
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource ID is not committed for the exact owner",
            )
        if in_use:
            raise ResourceStoreError(
                ResourceStoreErrorCode.IN_USE,
                "resource is referenced by a retained scene",
            )
        resources = dict(self._state.resources)
        del resources[key]
        usage = dict(self._state.usage)
        prior = usage[_owner_key(owner)]
        replacement = ResourceUsage(
            prior.resources - 1,
            prior.bytes - resource.declaration.byte_length,
        )
        if replacement == ResourceUsage():
            del usage[_owner_key(owner)]
        else:
            usage[_owner_key(owner)] = replacement
        return self._prepared(
            state=self._make_state(resources, usage),
            mode=_UploadInstallMode.PRESERVE,
            accepted_bytes=0,
            resource=resource,
        )

    def resource(self, owner: OwnerIdentity, resource_id) -> RGBAResource:
        self._require_live(owner)
        try:
            normalized_id = _integer(
                "resource_id", resource_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        resource = self._state.resources.get(_resource_key(owner, normalized_id))
        if resource is None:
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "resource ID is not committed for the exact owner",
            )
        return resource

    def prepare_owner_retirement(
        self,
        owner: OwnerIdentity,
    ) -> PreparedResourceRetirement:
        if self._upload is not None:
            raise ResourceStoreError(
                ResourceStoreErrorCode.STATE,
                "owner retirement crossed an open resource upload",
            )
        if not isinstance(owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        record = self._owners.state.records.get(owner.owner_id)
        if record is None or record.identity != owner:
            raise ResourceStoreError(
                ResourceStoreErrorCode.STALE_OWNER,
                "owner retirement lacks exact ledger authority",
            )
        resources = {
            key: resource
            for key, resource in self._state.resources.items()
            if key[:2] != _owner_key(owner)
        }
        usage = dict(self._state.usage)
        usage.pop(_owner_key(owner), None)
        return PreparedResourceRetirement(
            owner,
            self._make_state(resources, usage),
            self._token,
            self._state,
            self._owners.state,
        )

    def validate_owner_retirement(
        self,
        prepared: PreparedResourceRetirement,
    ) -> None:
        if not isinstance(prepared, PreparedResourceRetirement):
            raise TypeError("prepared must be PreparedResourceRetirement")
        if (
            prepared._consumed
            or prepared._source_state is None
            or prepared._source_owner_state is None
            or prepared._store_token is not self._token
            or prepared._source_state is not self._state
            or prepared._source_owner_state is not self._owners.state
            or self._upload is not None
        ):
            raise RuntimeError("prepared resource retirement is stale or foreign")

    def install_owner_retirement(
        self,
        prepared: PreparedResourceRetirement,
    ) -> None:
        self.validate_owner_retirement(prepared)
        self._install_owner_retirement_prevalidated(prepared)

    def _install_owner_retirement_prevalidated(
        self,
        prepared: PreparedResourceRetirement,
    ) -> None:
        self._state = prepared.state
        object.__setattr__(prepared, "_source_state", None)
        object.__setattr__(prepared, "_source_owner_state", None)
        object.__setattr__(prepared, "_consumed", True)

    def validate_prepared(self, prepared: PreparedResourceInstall) -> None:
        if not isinstance(prepared, PreparedResourceInstall):
            raise TypeError("prepared must be PreparedResourceInstall")
        source_upload = self._upload
        source_token = None if source_upload is None else source_upload.token
        if (
            prepared._consumed
            or prepared._source_state is None
            or prepared._source_owner_state is None
            or prepared._store_token is not self._token
            or prepared._source_state is not self._state
            or prepared._source_owner_state is not self._owners.state
            or prepared._source_upload_token is not source_token
            or (
                source_upload is not None
                and source_upload.accepted_bytes
                != prepared._source_accepted_bytes
            )
        ):
            raise RuntimeError("prepared resource install is stale or foreign")
        if prepared._ledger is not None:
            self._owners.validate_prepared(prepared._ledger)

    def install_prepared(self, prepared: PreparedResourceInstall) -> None:
        self.validate_prepared(prepared)
        self._install_prevalidated(prepared)

    def _install_prevalidated(self, prepared: PreparedResourceInstall) -> None:
        mode = prepared._mode
        source_upload = self._upload
        if mode is _UploadInstallMode.OPEN:
            target_upload = prepared._new_upload
            if source_upload is not None or target_upload is None:
                raise RuntimeError("prepared resource BEGIN lost its upload target")
            self._upload = target_upload
        elif mode is _UploadInstallMode.APPEND:
            offset = prepared._chunk_offset
            chunk = prepared._chunk
            if source_upload is None or offset is None or chunk is None:
                raise RuntimeError("prepared resource chunk lost its upload target")
            source_upload.backing.write(offset, chunk)
            source_upload.accepted_bytes = prepared.accepted_bytes
        elif mode is _UploadInstallMode.COMMIT:
            if source_upload is None:
                raise RuntimeError("prepared resource COMMIT lost its upload")
            source_upload.backing.commit()
            self._upload = None
        elif mode is _UploadInstallMode.ABORT:
            if source_upload is None:
                raise RuntimeError("prepared resource retirement lost its upload")
            source_upload.backing.abort()
            self._upload = None
        elif mode is not _UploadInstallMode.PRESERVE:
            raise RuntimeError("prepared resource install has an unknown mode")
        if prepared._ledger is not None:
            self._owners._install_prevalidated(prepared._ledger)
        self._state = prepared.state
        object.__setattr__(prepared, "resource", None)
        object.__setattr__(prepared, "_new_upload", None)
        object.__setattr__(prepared, "_ledger", None)
        object.__setattr__(prepared, "_chunk", None)
        object.__setattr__(prepared, "_source_state", None)
        object.__setattr__(prepared, "_source_owner_state", None)
        object.__setattr__(prepared, "_source_upload_token", None)
        object.__setattr__(prepared, "_consumed", True)

    def _require_live(self, owner: OwnerIdentity):
        try:
            return self._owners.require_live(owner)
        except OwnerLedgerError as exc:
            raise self._owner_error(exc) from exc

    def _match_upload(
        self,
        owner: OwnerIdentity,
        resource_id,
    ) -> _ResourceUpload:
        if not isinstance(owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        upload = self._upload
        if upload is None:
            self._require_live(owner)
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "no resource upload is open",
            )
        if owner != upload.owner:
            raise ResourceStoreError(
                ResourceStoreErrorCode.STALE_OWNER,
                "request owner does not match the open upload",
            )
        try:
            normalized_id = _integer(
                "resource_id", resource_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            raise ResourceStoreError(ResourceStoreErrorCode.INVALID, str(exc)) from exc
        if normalized_id != upload.declaration.resource_id:
            raise ResourceStoreError(
                ResourceStoreErrorCode.INVALID,
                "request resource does not match the open upload",
            )
        return upload

    def _prepared(
        self,
        *,
        state: ResourceStoreState,
        mode: _UploadInstallMode,
        new_upload: _ResourceUpload | None = None,
        ledger: PreparedOwnerLedgerInstall | None = None,
        accepted_bytes: int,
        resource: RGBAResource | None = None,
        upload: ResourceUploadView | None = None,
        chunk_offset: int | None = None,
        chunk: bytes | None = None,
    ) -> PreparedResourceInstall:
        source_upload = self._upload
        return PreparedResourceInstall(
            state=state,
            accepted_bytes=accepted_bytes,
            resource=resource,
            upload=upload,
            _mode=mode,
            _new_upload=new_upload,
            _ledger=ledger,
            _chunk_offset=chunk_offset,
            _chunk=chunk,
            _store_token=self._token,
            _source_state=self._state,
            _source_upload_token=(
                None if source_upload is None else source_upload.token
            ),
            _source_accepted_bytes=(
                None if source_upload is None else source_upload.accepted_bytes
            ),
            _source_owner_state=self._owners.state,
        )

    @staticmethod
    def _owner_error(error: OwnerLedgerError) -> ResourceStoreError:
        code = {
            OwnerLedgerErrorCode.INVALID: ResourceStoreErrorCode.INVALID,
            OwnerLedgerErrorCode.STALE_OWNER: ResourceStoreErrorCode.STALE_OWNER,
            OwnerLedgerErrorCode.NO_CAPACITY: ResourceStoreErrorCode.NO_CAPACITY,
            OwnerLedgerErrorCode.DUPLICATE_ID: ResourceStoreErrorCode.DUPLICATE_ID,
        }[error.code]
        return ResourceStoreError(code, error.detail)

    @staticmethod
    def _make_state(resources, usage) -> ResourceStoreState:
        return ResourceStoreState(
            MappingProxyType(dict(resources)),
            MappingProxyType(dict(usage)),
        )


__all__ = [
    "PreparedResourceInstall",
    "PreparedResourceRetirement",
    "RGBAResource",
    "ResourceDeclaration",
    "ResourceFormat",
    "ResourceStoreError",
    "ResourceStoreErrorCode",
    "ResourceStoreState",
    "ResourceUploadView",
    "ResourceUsage",
    "RetainedResourceStore",
]
