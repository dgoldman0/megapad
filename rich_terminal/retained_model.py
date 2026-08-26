"""Renderer-neutral RETAINED-1 capability policy and owner authority ledger.

This first retained-domain layer turns the negotiated caller policy into
checked, internally consistent capacities and reserves owner quotas against
those capacities.  Scene, resource-upload, wire, and renderer state build on
this exact-generation ledger; none of those concerns are needed to make owner
authority and reservations independently usable and testable.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass, replace
from enum import Enum, IntFlag
from types import MappingProxyType
from typing import Iterable, Mapping

from .apt1 import STRUCTURAL_MAX_PAYLOAD, UINT32_MAX, UINT64_MAX
from .update_authority import TerminalGeometry


class RetainedFeature(IntFlag):
    CORE = 1 << 0
    VECTOR = 1 << 1
    RGBA_IMAGE = 1 << 2
    INSTRUMENT = 1 << 3
    SERIES = 1 << 4
    CADENCE = 1 << 5


_ALL_FEATURES = (
    RetainedFeature.CORE
    | RetainedFeature.VECTOR
    | RetainedFeature.RGBA_IMAGE
    | RetainedFeature.INSTRUMENT
    | RetainedFeature.SERIES
    | RetainedFeature.CADENCE
)


class ItemNamespace(str, Enum):
    REGION = "REGION"
    RESOURCE = "RESOURCE"
    OBJECT = "OBJECT"
    SERIES = "SERIES"


class OwnerLedgerErrorCode(str, Enum):
    INVALID = "INVALID"
    STALE_OWNER = "STALE_OWNER"
    NO_CAPACITY = "NO_CAPACITY"
    DUPLICATE_ID = "DUPLICATE_ID"


class OwnerLedgerError(ValueError):
    """Deterministic owner lifecycle failure suitable for RET_RESULT mapping."""

    def __init__(self, code: OwnerLedgerErrorCode, detail: str):
        self.code = code
        self.detail = detail
        super().__init__(f"{code.value}: {detail}")


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


def _checked_add(name: str, *values: int, maximum: int = UINT64_MAX) -> int:
    total = 0
    for value in values:
        if value > maximum - total:
            raise ValueError(f"{name} exceeds {maximum}")
        total += value
    return total


def _checked_multiply(
    name: str, left: int, right: int, *, maximum: int = UINT64_MAX
) -> int:
    if left and right > maximum // left:
        raise ValueError(f"{name} exceeds {maximum}")
    return left * right


@dataclass(frozen=True, slots=True)
class RetainedPolicy:
    """Caller-supplied RET_CAPS/RET_FORMATS and base transport bounds.

    There are intentionally no product defaults.  A caller must choose every
    capacity, and construction rejects a policy that advertises a maximum it
    cannot carry in one valid frame/transaction.
    """

    features: RetainedFeature
    max_owner_records: int
    max_live_owners: int
    max_regions: int
    max_resources: int
    max_objects: int
    max_series: int
    max_operations_per_transaction: int
    max_resource_chunk_bytes: int
    max_retained_transaction_bytes: int
    total_resource_bytes: int
    image_format: int
    max_image_width: int
    max_image_height: int
    max_path_points: int
    max_glyph_run_bytes: int
    max_samples_per_append: int
    max_history_per_series: int
    minimum_presentation_interval_us: int
    total_sample_slots: int
    total_utf8_bytes: int
    client_to_terminal_max_payload: int
    terminal_to_client_max_payload: int
    base_max_transaction_bytes: int

    def __post_init__(self) -> None:
        if isinstance(self.features, bool):
            raise TypeError("features must not be bool")
        try:
            feature_bits = operator.index(self.features)
        except (TypeError, ValueError) as exc:
            raise TypeError("features must be RetainedFeature-compatible") from exc
        features = RetainedFeature(feature_bits)
        if int(features) & ~int(_ALL_FEATURES):
            raise ValueError("features contain reserved RETAINED-1 bits")
        object.__setattr__(self, "features", features)

        u32_fields = (
            "max_owner_records",
            "max_live_owners",
            "max_regions",
            "max_resources",
            "max_objects",
            "max_series",
            "max_operations_per_transaction",
            "max_resource_chunk_bytes",
            "image_format",
            "max_image_width",
            "max_image_height",
            "max_path_points",
            "max_glyph_run_bytes",
            "max_samples_per_append",
            "max_history_per_series",
            "minimum_presentation_interval_us",
        )
        for name in u32_fields:
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        for name in (
            "max_retained_transaction_bytes",
            "total_resource_bytes",
            "total_sample_slots",
            "total_utf8_bytes",
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT64_MAX),
            )
        for name in (
            "client_to_terminal_max_payload",
            "terminal_to_client_max_payload",
        ):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=0,
                    maximum=STRUCTURAL_MAX_PAYLOAD,
                ),
            )
        object.__setattr__(
            self,
            "base_max_transaction_bytes",
            _integer(
                "base_max_transaction_bytes",
                self.base_max_transaction_bytes,
                minimum=0,
                maximum=UINT32_MAX,
            ),
        )

        if not features & RetainedFeature.CORE:
            raise ValueError("RETAINED-1 requires CORE")
        if features & RetainedFeature.SERIES and not features & RetainedFeature.INSTRUMENT:
            raise ValueError("SERIES requires INSTRUMENT")
        if self.max_owner_records == 0 or self.max_live_owners == 0:
            raise ValueError("owner record and live-owner maxima must be positive")
        if self.max_live_owners > self.max_owner_records:
            raise ValueError("max_live_owners exceeds max_owner_records")
        if (
            self.max_regions == 0
            or self.max_operations_per_transaction == 0
            or self.max_retained_transaction_bytes == 0
        ):
            raise ValueError("CORE region, operation, and transaction maxima are positive")
        if self.client_to_terminal_max_payload < 64:
            raise ValueError("CORE requires a 64-byte client-to-terminal payload")
        if self.terminal_to_client_max_payload < 64:
            raise ValueError("CORE requires a 64-byte terminal-to-client payload")
        if self.max_retained_transaction_bytes > self.base_max_transaction_bytes:
            raise ValueError("retained transaction maximum exceeds the base maximum")

        image = bool(features & RetainedFeature.RGBA_IMAGE)
        for name in (
            "max_resources",
            "max_resource_chunk_bytes",
            "total_resource_bytes",
            "max_image_width",
            "max_image_height",
        ):
            self._require_positive_exact(name, image)
        if self.image_format != (1 if image else 0):
            raise ValueError("image_format is inconsistent with RGBA_IMAGE")

        vector = bool(features & RetainedFeature.VECTOR)
        self._require_positive_exact("max_path_points", vector)

        instrument = bool(features & RetainedFeature.INSTRUMENT)
        glyph_runs = self.max_glyph_run_bytes > 0
        if glyph_runs and self.max_objects == 0:
            raise ValueError("glyph-run capacity requires object capacity")
        if glyph_runs and self.total_utf8_bytes < self.max_glyph_run_bytes:
            raise ValueError("total UTF-8 capacity cannot admit one maximum glyph run")
        if not glyph_runs and self.total_utf8_bytes != 0:
            raise ValueError("UTF-8 capacity requires glyph-run capacity")
        if (vector or image or instrument) and self.max_objects == 0:
            raise ValueError("advertised object features require object capacity")
        if instrument and not glyph_runs:
            raise ValueError("INSTRUMENT requires glyph-run text capacity")

        series = bool(features & RetainedFeature.SERIES)
        self._require_positive_exact("max_series", series)
        self._require_positive_exact("max_samples_per_append", series)
        self._require_positive_exact("max_history_per_series", series)
        self._require_positive_exact("total_sample_slots", series)

        cadence = bool(features & RetainedFeature.CADENCE)
        self._require_positive_exact("minimum_presentation_interval_us", cadence)

        if series and not (
            self.max_samples_per_append
            <= self.max_history_per_series
            <= self.total_sample_slots
        ):
            raise ValueError("series sample/history totals are internally inconsistent")
        if image:
            pixels = _checked_multiply(
                "maximum image pixels", self.max_image_width, self.max_image_height
            )
            image_bytes = _checked_multiply("maximum image bytes", pixels, 4)
            if image_bytes > self.total_resource_bytes:
                raise ValueError("resource bytes cannot admit one maximum image")

        inbound = self.client_to_terminal_max_payload
        operation_payloads = [48]
        if vector:
            vector_payload = _checked_add(
                "maximum vector payload",
                80,
                _checked_multiply("maximum path bytes", 8, self.max_path_points),
            )
            if vector_payload > inbound:
                raise ValueError("maximum VECTOR object exceeds inbound payload")
            operation_payloads.append(vector_payload)
        if image:
            if inbound < 80:
                raise ValueError("RGBA_IMAGE requires an 80-byte inbound payload")
            if _checked_add(
                "maximum resource chunk payload", 32, self.max_resource_chunk_bytes
            ) > inbound:
                raise ValueError("maximum resource chunk exceeds inbound payload")
            operation_payloads.append(80)
        if glyph_runs:
            glyph_run_payload = _checked_add(
                "maximum GLYPH_RUN payload", 80, self.max_glyph_run_bytes
            )
            if glyph_run_payload > inbound:
                raise ValueError("maximum GLYPH_RUN object exceeds inbound payload")
            operation_payloads.append(glyph_run_payload)
        if instrument:
            readout_payload = _checked_add(
                "maximum READOUT payload", 104, self.max_glyph_run_bytes
            )
            if max(readout_payload, 112) > inbound:
                raise ValueError("maximum INSTRUMENT object exceeds inbound payload")
            operation_payloads.extend((readout_payload, 112))
        if series:
            explicit_payload = _checked_add(
                "maximum explicit series payload",
                40,
                _checked_multiply(
                    "maximum explicit sample bytes", 16, self.max_samples_per_append
                ),
            )
            if max(112, explicit_payload) > inbound:
                raise ValueError("maximum SERIES operation exceeds inbound payload")
            operation_payloads.extend((112, explicit_payload))

        transaction_floor = _checked_add(
            "retained transaction floor", 200, max(operation_payloads)
        )
        if self.max_retained_transaction_bytes < transaction_floor:
            raise ValueError(
                "retained transaction maximum cannot admit an advertised operation"
            )

    def _require_positive_exact(self, name: str, feature_present: bool) -> None:
        value = getattr(self, name)
        if feature_present and value == 0:
            raise ValueError(f"{name} must be positive when its feature is present")
        if not feature_present and value != 0:
            raise ValueError(f"{name} must be zero when its feature is absent")

    def to_dict(self) -> dict[str, int]:
        """Return the complete scalar policy for an explicit launcher handoff."""

        return {
            name: int(getattr(self, name))
            for name in self.__dataclass_fields__
        }

    def validate_geometry(self, geometry: TerminalGeometry) -> int:
        """Return exact CELL_REPLACE transaction bytes for ``geometry``."""

        if not isinstance(geometry, TerminalGeometry):
            raise TypeError("geometry must be TerminalGeometry")
        span_payload = _checked_add(
            "CELL_REPLACE span payload",
            12,
            _checked_multiply("CELL_REPLACE row cells", 8, geometry.cols),
        )
        if span_payload > self.client_to_terminal_max_payload:
            raise ValueError("CELL_REPLACE row exceeds inbound payload")
        row_bytes = _checked_add("CELL_REPLACE row frame", 40, span_payload)
        transaction_bytes = _checked_add(
            "CELL_REPLACE transaction bytes",
            216,
            _checked_multiply("CELL_REPLACE rows", geometry.rows, row_bytes),
        )
        if transaction_bytes > self.max_retained_transaction_bytes:
            raise ValueError("geometry exceeds retained transaction maximum")
        if transaction_bytes > self.base_max_transaction_bytes:
            raise ValueError("geometry exceeds base transaction maximum")
        return transaction_bytes


@dataclass(frozen=True, slots=True)
class OwnerIdentity:
    session_id: int
    presentation_epoch: int
    owner_id: int
    owner_generation: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "session_id",
            _integer("session_id", self.session_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "presentation_epoch",
            _integer(
                "presentation_epoch",
                self.presentation_epoch,
                minimum=0,
                maximum=UINT32_MAX,
            ),
        )
        object.__setattr__(
            self,
            "owner_id",
            _integer("owner_id", self.owner_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "owner_generation",
            _integer(
                "owner_generation",
                self.owner_generation,
                minimum=1,
                maximum=UINT64_MAX,
            ),
        )


@dataclass(frozen=True, slots=True)
class OwnerQuotas:
    regions: int
    resources: int
    objects: int
    series: int
    resource_bytes: int
    utf8_bytes: int
    sample_slots: int

    def __post_init__(self) -> None:
        for name in ("regions", "resources", "objects", "series"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        for name in ("resource_bytes", "utf8_bytes", "sample_slots"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT64_MAX),
            )


@dataclass(frozen=True, slots=True)
class ReservationTotals:
    live_owners: int = 0
    regions: int = 0
    resources: int = 0
    objects: int = 0
    series: int = 0
    resource_bytes: int = 0
    utf8_bytes: int = 0
    sample_slots: int = 0

    def add(self, quotas: OwnerQuotas) -> ReservationTotals:
        return ReservationTotals(
            live_owners=_checked_add("live owners", self.live_owners, 1),
            regions=_checked_add("region reservations", self.regions, quotas.regions),
            resources=_checked_add(
                "resource reservations", self.resources, quotas.resources
            ),
            objects=_checked_add("object reservations", self.objects, quotas.objects),
            series=_checked_add("series reservations", self.series, quotas.series),
            resource_bytes=_checked_add(
                "resource-byte reservations", self.resource_bytes, quotas.resource_bytes
            ),
            utf8_bytes=_checked_add(
                "UTF-8 reservations", self.utf8_bytes, quotas.utf8_bytes
            ),
            sample_slots=_checked_add(
                "sample-slot reservations", self.sample_slots, quotas.sample_slots
            ),
        )

    def subtract(self, quotas: OwnerQuotas) -> ReservationTotals:
        values = {
            "live_owners": self.live_owners - 1,
            "regions": self.regions - quotas.regions,
            "resources": self.resources - quotas.resources,
            "objects": self.objects - quotas.objects,
            "series": self.series - quotas.series,
            "resource_bytes": self.resource_bytes - quotas.resource_bytes,
            "utf8_bytes": self.utf8_bytes - quotas.utf8_bytes,
            "sample_slots": self.sample_slots - quotas.sample_slots,
        }
        if any(value < 0 for value in values.values()):
            raise RuntimeError("owner reservation ledger underflow")
        return ReservationTotals(**values)


@dataclass(frozen=True, slots=True)
class ItemHighWater:
    region: int = 0
    resource: int = 0
    object: int = 0
    series: int = 0

    def value(self, namespace: ItemNamespace) -> int:
        if not isinstance(namespace, ItemNamespace):
            raise TypeError("namespace must be ItemNamespace")
        return getattr(self, namespace.value.lower())

    def advanced(self, namespace: ItemNamespace, item_id: int) -> ItemHighWater:
        normalized_id = _integer(
            "item_id", item_id, minimum=1, maximum=UINT64_MAX
        )
        current = self.value(namespace)
        if normalized_id <= current:
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.DUPLICATE_ID,
                f"{namespace.value} ID does not exceed high-water {current}",
            )
        return replace(self, **{namespace.value.lower(): normalized_id})


@dataclass(frozen=True, slots=True)
class OwnerRecord:
    identity: OwnerIdentity
    quotas: OwnerQuotas | None
    high_water: ItemHighWater

    @property
    def live(self) -> bool:
        return self.quotas is not None


class OwnerOpenDisposition(str, Enum):
    OPENED = "OPENED"
    REOPENED = "REOPENED"
    IDEMPOTENT = "IDEMPOTENT"


class OwnerDropDisposition(str, Enum):
    DROPPED = "DROPPED"
    IDEMPOTENT = "IDEMPOTENT"


@dataclass(frozen=True, slots=True)
class OwnerLedgerState:
    records: Mapping[int, OwnerRecord]
    reservations: ReservationTotals


@dataclass(frozen=True, slots=True)
class PreparedOwnerLedgerInstall:
    state: OwnerLedgerState
    disposition: OwnerOpenDisposition | OwnerDropDisposition | None
    _ledger_token: object
    _source_state: OwnerLedgerState


class OwnerLedger:
    """Atomic live-owner/tombstone ledger for one exact session epoch."""

    def __init__(
        self,
        *,
        session_id: int,
        presentation_epoch: int,
        policy: RetainedPolicy,
    ) -> None:
        self._session_id = _integer(
            "session_id", session_id, minimum=1, maximum=UINT64_MAX
        )
        self._presentation_epoch = _integer(
            "presentation_epoch",
            presentation_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        if not isinstance(policy, RetainedPolicy):
            raise TypeError("policy must be RetainedPolicy")
        self._policy = policy
        self._state = self._make_state({}, ReservationTotals())
        self._install_token = object()

    @property
    def policy(self) -> RetainedPolicy:
        return self._policy

    @property
    def state(self) -> OwnerLedgerState:
        return self._state

    def record(self, owner_id: int) -> OwnerRecord | None:
        normalized_id = _integer(
            "owner_id", owner_id, minimum=1, maximum=UINT64_MAX
        )
        return self._state.records.get(normalized_id)

    def require_live(self, identity: OwnerIdentity) -> OwnerRecord:
        self._validate_scope(identity)
        record = self._state.records.get(identity.owner_id)
        if (
            record is None
            or not record.live
            or record.identity.owner_generation != identity.owner_generation
        ):
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.STALE_OWNER,
                "owner ID/generation is not exact live authority",
            )
        return record

    def prepare_open(
        self, identity: OwnerIdentity, quotas: OwnerQuotas
    ) -> PreparedOwnerLedgerInstall:
        self._validate_scope(identity)
        if not isinstance(quotas, OwnerQuotas):
            raise TypeError("quotas must be OwnerQuotas")
        self._validate_quotas(quotas)
        records = self._state.records
        prior = records.get(identity.owner_id)
        disposition = OwnerOpenDisposition.OPENED
        high_water = ItemHighWater()

        if prior is not None:
            prior_generation = prior.identity.owner_generation
            if prior.live:
                if prior_generation != identity.owner_generation:
                    raise OwnerLedgerError(
                        OwnerLedgerErrorCode.STALE_OWNER,
                        "a different generation is live for owner ID",
                    )
                if prior.quotas != quotas:
                    raise OwnerLedgerError(
                        OwnerLedgerErrorCode.INVALID,
                        "duplicate live OWNER_OPEN changed immutable quotas",
                    )
                return self._prepared(
                    self._state, OwnerOpenDisposition.IDEMPOTENT
                )
            if identity.owner_generation <= prior_generation:
                raise OwnerLedgerError(
                    OwnerLedgerErrorCode.STALE_OWNER,
                    "owner generation does not exceed tombstone",
                )
            disposition = OwnerOpenDisposition.REOPENED
        elif len(records) >= self._policy.max_owner_records:
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.NO_CAPACITY, "owner record capacity is exhausted"
            )

        try:
            reservations = self._state.reservations.add(quotas)
        except ValueError as exc:
            raise OwnerLedgerError(OwnerLedgerErrorCode.NO_CAPACITY, str(exc)) from exc
        self._validate_reservation_totals(reservations)
        updated = dict(records)
        updated[identity.owner_id] = OwnerRecord(identity, quotas, high_water)
        return self._prepared(self._make_state(updated, reservations), disposition)

    def open(self, identity: OwnerIdentity, quotas: OwnerQuotas) -> OwnerOpenDisposition:
        prepared = self.prepare_open(identity, quotas)
        self.install_prepared(prepared)
        assert isinstance(prepared.disposition, OwnerOpenDisposition)
        return prepared.disposition

    def prepare_drop(self, identity: OwnerIdentity) -> PreparedOwnerLedgerInstall:
        self._validate_scope(identity)
        prior = self._state.records.get(identity.owner_id)
        if prior is None or prior.identity.owner_generation != identity.owner_generation:
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.STALE_OWNER,
                "owner drop does not name the live record or exact tombstone",
            )
        if not prior.live:
            return self._prepared(
                self._state, OwnerDropDisposition.IDEMPOTENT
            )
        assert prior.quotas is not None
        reservations = self._state.reservations.subtract(prior.quotas)
        updated = dict(self._state.records)
        updated[identity.owner_id] = OwnerRecord(
            identity=prior.identity,
            quotas=None,
            high_water=prior.high_water,
        )
        return self._prepared(
            self._make_state(updated, reservations), OwnerDropDisposition.DROPPED
        )

    def drop(self, identity: OwnerIdentity) -> OwnerDropDisposition:
        prepared = self.prepare_drop(identity)
        self.install_prepared(prepared)
        assert isinstance(prepared.disposition, OwnerDropDisposition)
        return prepared.disposition

    def prepare_item_id(
        self,
        identity: OwnerIdentity,
        namespace: ItemNamespace,
        item_id: int,
    ) -> PreparedOwnerLedgerInstall:
        return self.prepare_item_ids(((identity, namespace, item_id),))

    def prepare_item_ids(
        self,
        advances: Iterable[tuple[OwnerIdentity, ItemNamespace, int]],
    ) -> PreparedOwnerLedgerInstall:
        """Prepare ordered item-ID advances as one atomic ledger candidate.

        A PRESENT transaction may define several items, including several in
        the same namespace.  The iterable is consumed synchronously and each
        advance validates against prior advances in this candidate; no
        high-water mark changes unless the resulting prepared value installs.
        """

        try:
            iterator = iter(advances)
        except TypeError as exc:
            raise TypeError("advances must be iterable") from exc
        updated = dict(self._state.records)
        for index, advance in enumerate(iterator):
            try:
                identity, namespace, item_id = advance
            except (TypeError, ValueError) as exc:
                raise TypeError(
                    f"item-ID advance {index} must contain identity, namespace, and ID"
                ) from exc
            self._validate_scope(identity)
            record = updated.get(identity.owner_id)
            if (
                record is None
                or not record.live
                or record.identity.owner_generation != identity.owner_generation
            ):
                raise OwnerLedgerError(
                    OwnerLedgerErrorCode.STALE_OWNER,
                    "item-ID advance lacks exact live owner authority",
                )
            high_water = record.high_water.advanced(namespace, item_id)
            updated[identity.owner_id] = replace(record, high_water=high_water)
        return self._prepared(
            self._make_state(updated, self._state.reservations), None
        )

    def install_prepared(self, prepared: PreparedOwnerLedgerInstall) -> None:
        """Install a fully checked state using one non-allocating assignment.

        Prepared values are domain-internal capabilities.  A later scene
        coordinator can prepare its immutable scene and this ledger state
        before making either authoritative state visible.
        """

        self.validate_prepared(prepared)
        self._install_prevalidated(prepared)

    def _install_prevalidated(self, prepared: PreparedOwnerLedgerInstall) -> None:
        """Install after a coordinator has completed every fallible check."""

        self._state = prepared.state

    def validate_prepared(self, prepared: PreparedOwnerLedgerInstall) -> None:
        """Validate install provenance without mutating the ledger."""

        if not isinstance(prepared, PreparedOwnerLedgerInstall):
            raise TypeError("prepared must be PreparedOwnerLedgerInstall")
        if (
            prepared._ledger_token is not self._install_token
            or prepared._source_state is not self._state
        ):
            raise RuntimeError("prepared owner ledger state is stale or foreign")

    def _validate_scope(self, identity: OwnerIdentity) -> None:
        if not isinstance(identity, OwnerIdentity):
            raise TypeError("identity must be OwnerIdentity")
        if (
            identity.session_id != self._session_id
            or identity.presentation_epoch != self._presentation_epoch
        ):
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.STALE_OWNER,
                "owner identity is outside this session/presentation_epoch",
            )

    def _validate_quotas(self, quotas: OwnerQuotas) -> None:
        policy = self._policy
        maxima = (
            ("regions", quotas.regions, policy.max_regions),
            ("resources", quotas.resources, policy.max_resources),
            ("objects", quotas.objects, policy.max_objects),
            ("series", quotas.series, policy.max_series),
            ("resource bytes", quotas.resource_bytes, policy.total_resource_bytes),
            ("UTF-8 bytes", quotas.utf8_bytes, policy.total_utf8_bytes),
            ("sample slots", quotas.sample_slots, policy.total_sample_slots),
        )
        for name, requested, maximum in maxima:
            if requested > maximum:
                raise OwnerLedgerError(
                    OwnerLedgerErrorCode.INVALID,
                    f"owner {name} quota exceeds advertised maximum",
                )
        if not policy.features & RetainedFeature.RGBA_IMAGE and (
            quotas.resources or quotas.resource_bytes
        ):
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.INVALID, "resource quota requires RGBA_IMAGE"
            )
        if not policy.max_glyph_run_bytes and quotas.utf8_bytes:
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.INVALID,
                "UTF-8 quota requires glyph-run capacity",
            )
        if not policy.features & RetainedFeature.SERIES and (
            quotas.series or quotas.sample_slots
        ):
            raise OwnerLedgerError(
                OwnerLedgerErrorCode.INVALID, "series quota requires SERIES"
            )

    def _validate_reservation_totals(self, totals: ReservationTotals) -> None:
        policy = self._policy
        maxima = (
            (totals.live_owners, policy.max_live_owners, "live owner"),
            (totals.regions, policy.max_regions, "region"),
            (totals.resources, policy.max_resources, "resource"),
            (totals.objects, policy.max_objects, "object"),
            (totals.series, policy.max_series, "series"),
            (totals.resource_bytes, policy.total_resource_bytes, "resource-byte"),
            (totals.utf8_bytes, policy.total_utf8_bytes, "UTF-8-byte"),
            (totals.sample_slots, policy.total_sample_slots, "sample-slot"),
        )
        for used, maximum, name in maxima:
            if used > maximum:
                raise OwnerLedgerError(
                    OwnerLedgerErrorCode.NO_CAPACITY,
                    f"aggregate {name} reservations exceed caller policy",
                )

    @staticmethod
    def _make_state(
        records: Mapping[int, OwnerRecord], reservations: ReservationTotals
    ) -> OwnerLedgerState:
        return OwnerLedgerState(MappingProxyType(dict(records)), reservations)

    def _prepared(
        self,
        state: OwnerLedgerState,
        disposition: OwnerOpenDisposition | OwnerDropDisposition | None,
    ) -> PreparedOwnerLedgerInstall:
        return PreparedOwnerLedgerInstall(
            state,
            disposition,
            self._install_token,
            self._state,
        )


__all__ = [
    "ItemHighWater",
    "ItemNamespace",
    "OwnerDropDisposition",
    "OwnerIdentity",
    "OwnerLedger",
    "OwnerLedgerError",
    "OwnerLedgerErrorCode",
    "OwnerLedgerState",
    "OwnerOpenDisposition",
    "OwnerQuotas",
    "OwnerRecord",
    "PreparedOwnerLedgerInstall",
    "ReservationTotals",
    "RetainedFeature",
    "RetainedPolicy",
]
