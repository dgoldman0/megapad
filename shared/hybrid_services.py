"""Immutable V5 private-service metadata; no execution or export authority.

The first catalog describes only scalar FP/FPCSR effects. Machine execution
still uses the integer callback transport's numerical result shape. These
values neither bind a service nor issue a fault receipt or continuation token.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI, MAX_BUFFER_RULES, MAX_CALLBACK_EXPORTS, MAX_CODE_BYTES,
    MAX_DISPATCH_CALLBACKS, MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
    MAX_SIGNATURE_CELLS, BufferRuleV1, MachineSegmentResultV2,
    RoutineDeclarationV1, RoutineImageV1, RoutineManifestV2,
    _callback_sites, _integer, _name, _routine_values, _version,
)


HYBRID_SERVICE_ABI_VERSION = 5
SERVICE_EFFECT_V5 = "scalar_fp_state"
SERVICE_CAPABILITY_V5 = "private_scalar_fp_v1"
SCALAR_FP_SIGNATURES_V5 = (
    ("FPCSR@", 0, 1), ("FPCSR!", 1, 0),
    ("F32+", 2, 1), ("F32-", 2, 1), ("F32*", 2, 1), ("F32/", 2, 1),
    ("F32SQRT", 1, 1), ("F32FMA", 3, 1),
    ("F64+", 2, 1), ("F64-", 2, 1), ("F64*", 2, 1), ("F64/", 2, 1),
    ("F64SQRT", 1, 1), ("F64FMA", 3, 1),
)


@dataclass(frozen=True, slots=True, kw_only=True)
class ServiceExportV5:
    """One fixed service signature, without a Word, owner or callable."""

    export_id: int
    name: str
    input_cells: int
    output_cells: int
    max_semantic_steps: int = 1
    effect: str = SERVICE_EFFECT_V5
    abi: str = HYBRID_ABI
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_SERVICE_ABI_VERSION)
        _integer(self.export_id, "service export ID", 0, MAX_CALLBACK_EXPORTS - 1)
        _name(self.name)
        signature = next((entry[1:] for entry in SCALAR_FP_SIGNATURES_V5
                          if entry[0] == self.name), None)
        if signature is None:
            raise ValueError("service export must name a canonical scalar FP/FPCSR operation")
        _integer(self.input_cells, "service input cells", 0, MAX_SIGNATURE_CELLS)
        _integer(self.output_cells, "service output cells", 0, MAX_SIGNATURE_CELLS)
        if (self.input_cells, self.output_cells) != signature:
            raise ValueError("service arity does not match its canonical scalar operation")
        _integer(self.max_semantic_steps, "scalar service semantic steps", 1, 1)
        if type(self.effect) is not str:
            raise TypeError("service effect must be a string")
        if self.effect != SERVICE_EFFECT_V5:
            raise ValueError("service effect must be scalar_fp_state")

    @property
    def max_input_bytes(self) -> int:
        return 0

    @property
    def max_output_bytes(self) -> int:
        return 0

    @property
    def can_suspend(self) -> bool:
        return False


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackSiteV5:
    """Numerical CALL/RET metadata whose service descriptor is revalidated."""

    call_offset: int
    stub_offset: int
    export: ServiceExportV5
    abi: str = HYBRID_ABI
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_SERVICE_ABI_VERSION)
        _integer(self.call_offset, "callback call offset", 0, MAX_CODE_BYTES - 2)
        _integer(self.stub_offset, "callback stub offset", 0, MAX_CODE_BYTES - 1)
        if self.call_offset <= self.stub_offset < self.call_offset + 2:
            raise ValueError("callback call and stub byte spans must be disjoint")
        if type(self.export) is not ServiceExportV5:
            raise TypeError("callback export must be a ServiceExportV5 value")
        ServiceExportV5.__post_init__(self.export)


def _buffer_rules(buffers: object) -> None:
    # Older routine helpers already check type/count/index geometry, but V5
    # must revalidate nested values before reading even their argument indices.
    if type(buffers) is not tuple:
        raise TypeError("buffer rules must be an immutable tuple")
    if len(buffers) > MAX_BUFFER_RULES:
        raise ValueError("a routine may declare at most 16 buffer rules")
    for rule in buffers:
        if type(rule) is not BufferRuleV1:
            raise TypeError("buffer rules must be BufferRuleV1 values")
        BufferRuleV1.__post_init__(rule)


def _callback_limit(value: int) -> None:
    _integer(value, "per-call callback requests", 0, MAX_DISPATCH_CALLBACKS)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineImageV5(RoutineImageV1):
    """Unpublished integer image with fixed scalar-service callback sites."""

    callbacks: tuple[CallbackSiteV5, ...]
    max_callback_requests: int
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        _buffer_rules(self.buffers)
        _routine_values(self, version=HYBRID_SERVICE_ABI_VERSION)
        _callback_limit(self.max_callback_requests)
        _callback_sites(self, site_type=CallbackSiteV5)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineDeclarationV5(RoutineDeclarationV1):
    """Sealed declaration; opaque leases remain adapter-owned authority."""

    callbacks: tuple[CallbackSiteV5, ...]
    max_callback_requests: int
    dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS
    dispatch_callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        _buffer_rules(self.buffers)
        RoutineDeclarationV1._validate_declaration(self, version=HYBRID_SERVICE_ABI_VERSION)
        _callback_limit(self.max_callback_requests)
        _callback_sites(self, site_type=CallbackSiteV5)
        _integer(self.dispatch_callback_limit, "dispatch callback requests", 1,
                 MAX_DISPATCH_CALLBACKS)
        _integer(self.dispatch_callback_semantic_limit, "dispatch callback semantic steps", 1,
                 MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineManifestV5(RoutineManifestV2):
    """Unpublished service descriptions; constructing them grants no effects."""

    exports: tuple[ServiceExportV5, ...]
    routines: tuple[RoutineImageV5, ...]
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        RoutineManifestV2._validate_manifest(
            self, version=HYBRID_SERVICE_ABI_VERSION,
            export_type=ServiceExportV5, routine_type=RoutineImageV5,
        )


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackRequestV5:
    """A copyable observation; it cannot resume a machine or issue a fault."""

    invocation_id: int
    sequence: int
    site: CallbackSiteV5
    arguments: tuple[int, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_SERVICE_ABI_VERSION)
        _integer(self.invocation_id, "invocation ID", 1, MASK64)
        _integer(self.sequence, "callback request sequence", 1, MAX_DISPATCH_CALLBACKS)
        if type(self.site) is not CallbackSiteV5:
            raise TypeError("callback request site must be a CallbackSiteV5 value")
        CallbackSiteV5.__post_init__(self.site)
        if type(self.arguments) is not tuple:
            raise TypeError("callback arguments must be an immutable tuple")
        if len(self.arguments) != self.site.export.input_cells:
            raise ValueError("callback argument count does not match its export")
        for argument in self.arguments:
            _integer(argument, "callback argument cell", 0, MASK64)


@dataclass(frozen=True, slots=True, kw_only=True)
class MachineSegmentResultV5(MachineSegmentResultV2):
    """Transport-2 deltas/totals adapted to V5 metadata, without a token."""

    callback: CallbackRequestV5 | None = None
    version: int = HYBRID_SERVICE_ABI_VERSION

    def __post_init__(self) -> None:
        MachineSegmentResultV2._validate_segment(
            self, version=HYBRID_SERVICE_ABI_VERSION, request_type=CallbackRequestV5,
        )


__all__ = [
    "HYBRID_SERVICE_ABI_VERSION", "SERVICE_EFFECT_V5", "SERVICE_CAPABILITY_V5",
    "SCALAR_FP_SIGNATURES_V5", "ServiceExportV5", "CallbackSiteV5",
    "RoutineImageV5", "RoutineDeclarationV5", "RoutineManifestV5",
    "CallbackRequestV5", "MachineSegmentResultV5",
]
