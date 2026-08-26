"""Renderer-facing root-LABEL projection of one immutable terminal composite.

The retained scene model deliberately remains richer than any one renderer
slice.  This module is the fail-closed boundary for the first visible slice: it
copies only the active, physically visible root LABEL values needed by a view
sink and preserves their exact composite scope and deterministic draw order.
Hidden rebuild targets never cross this boundary.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass

from .apt1 import UINT32_MAX, UINT64_MAX
from .cell_model import TerminalView
from .output_coordinator import CompositeTerminalView
from .retained_scene import (
    GroupBody,
    LabelBody,
    ObjectDefinition,
    OwnerScene,
    SceneModelState,
)
from .update_authority import TerminalGeometry


INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1


class RetainedViewError(ValueError):
    """The immutable composite cannot be consumed by the root-LABEL slice."""


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


def _boolean(name: str, value) -> bool:
    if not isinstance(value, bool):
        raise TypeError(f"{name} must be bool")
    return value


@dataclass(frozen=True, slots=True)
class DisplayScope:
    """Exact immutable model scope carried to a physical view sink."""

    attachment_epoch: int
    session_id: int
    presentation_epoch: int
    model_revision: int
    geometry_generation: int
    cell_revision: int
    retained_revision: int | None

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("attachment_epoch", 1, UINT64_MAX),
            ("session_id", 1, UINT64_MAX),
            ("presentation_epoch", 0, UINT32_MAX),
            ("model_revision", 0, UINT64_MAX),
            ("geometry_generation", 0, UINT64_MAX),
            ("cell_revision", 0, UINT64_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        retained_revision = self.retained_revision
        if retained_revision is not None:
            object.__setattr__(
                self,
                "retained_revision",
                _integer(
                    "retained_revision",
                    retained_revision,
                    minimum=0,
                    maximum=UINT64_MAX,
                ),
            )
        if self.cell_revision > self.model_revision:
            raise ValueError("CELL revision cannot exceed the composite revision")
        if (
            self.retained_revision is not None
            and self.retained_revision > self.model_revision
        ):
            raise ValueError("retained revision cannot exceed the composite revision")


@dataclass(frozen=True, slots=True)
class RetainedLabelDraw:
    """One visible, parentless LABEL in region-relative UNORM32 geometry."""

    object_id: int
    z_order: int
    left: int
    top: int
    right: int
    bottom: int
    red: int
    green: int
    blue: int
    alpha: int
    horizontal_align: int
    vertical_align: int
    ellipsize: bool
    text: str

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        for name in ("left", "top", "right", "bottom"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        if self.left >= self.right or self.top >= self.bottom:
            raise ValueError("LABEL bounds must have positive width and height")
        for name in ("red", "green", "blue", "alpha"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=0xFF),
            )
        for name in ("horizontal_align", "vertical_align"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=2),
            )
        object.__setattr__(self, "ellipsize", _boolean("ellipsize", self.ellipsize))
        if not isinstance(self.text, str):
            raise TypeError("text must be str")
        if "\0" in self.text or "\r" in self.text or "\n" in self.text:
            raise ValueError("LABEL text contains NUL, CR, or LF")
        try:
            self.text.encode("utf-8", "strict")
        except UnicodeEncodeError as exc:
            raise ValueError("LABEL text contains a non-scalar surrogate") from exc


@dataclass(frozen=True, slots=True)
class RetainedRegionDraw:
    """One visible region and its sorted root LABEL draw values."""

    owner_id: int
    owner_generation: int
    region_id: int
    cell_x: int
    cell_y: int
    cell_cols: int
    cell_rows: int
    z_order: int
    clipped: bool
    labels: tuple[RetainedLabelDraw, ...]

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "region_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        for name, minimum in (
            ("cell_x", 0),
            ("cell_y", 0),
            ("cell_cols", 1),
            ("cell_rows", 1),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT32_MAX),
            )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        object.__setattr__(self, "clipped", _boolean("clipped", self.clipped))
        labels = tuple(self.labels)
        if any(not isinstance(label, RetainedLabelDraw) for label in labels):
            raise TypeError("labels must contain only RetainedLabelDraw values")
        if tuple(sorted(labels, key=lambda label: (label.z_order, label.object_id))) != labels:
            raise ValueError("region LABEL values are not in back-to-front order")
        object.__setattr__(self, "labels", labels)


@dataclass(frozen=True, slots=True)
class RetainedRootLabelPlane:
    """The active root-LABEL draw plane for one composite revision."""

    retained_initialized: bool
    retained_visible: bool
    regions: tuple[RetainedRegionDraw, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "retained_initialized",
            _boolean("retained_initialized", self.retained_initialized),
        )
        object.__setattr__(
            self,
            "retained_visible",
            _boolean("retained_visible", self.retained_visible),
        )
        regions = tuple(self.regions)
        if any(not isinstance(region, RetainedRegionDraw) for region in regions):
            raise TypeError("regions must contain only RetainedRegionDraw values")
        if tuple(
            sorted(
                regions,
                key=lambda region: (region.z_order, region.owner_id, region.region_id),
            )
        ) != regions:
            raise ValueError("regions are not in back-to-front order")
        if not self.retained_visible and regions:
            raise ValueError("a hidden retained plane cannot contain draw regions")
        if self.retained_visible and not self.retained_initialized:
            raise ValueError("an uninitialized retained plane cannot be visible")
        object.__setattr__(self, "regions", regions)


def _effectively_visible(
    definition: ObjectDefinition,
    objects,
) -> bool:
    """Resolve object/group visibility iteratively without native recursion."""

    current = definition
    visited: set[int] = set()
    while True:
        if not current.visible:
            return False
        parent_id = current.parent_object_id
        if parent_id == 0:
            return True
        if parent_id in visited:
            raise RetainedViewError("retained object graph contains a cycle")
        visited.add(parent_id)
        parent = objects.get(parent_id)
        if parent is None:
            raise RetainedViewError("retained object refers to a missing parent")
        if parent.owner != definition.owner or parent.region_id != definition.region_id:
            raise RetainedViewError("retained object parent crosses owner or region scope")
        if not isinstance(parent.body, GroupBody):
            raise RetainedViewError("retained object parent is not a GROUP")
        current = parent


def _validate_owner_scope(owner_scene: OwnerScene, owner_key: int, view) -> None:
    owner = owner_scene.owner
    cell = view.cell
    assert cell is not None
    if owner_key != owner.owner_id:
        raise RetainedViewError("retained owner map key does not match owner identity")
    if (
        owner.session_id != cell.session_id
        or owner.presentation_epoch != view.presentation_epoch
    ):
        raise RetainedViewError("retained owner is outside the composite scope")


def project_composite_root_labels(
    view: CompositeTerminalView,
) -> tuple[DisplayScope, RetainedRootLabelPlane]:
    """Project one exact composite to the first renderer's root-LABEL DTOs.

    A physically visible non-LABEL drawing object or a visible nested LABEL is
    rejected.  Invisible regions, objects, and group cascades do not become
    draw commands.  The hidden rebuild target is intentionally never visited.
    """

    if not isinstance(view, CompositeTerminalView):
        raise TypeError("view must be CompositeTerminalView")
    if not isinstance(view.geometry, TerminalGeometry):
        raise TypeError("composite geometry must be TerminalGeometry")
    cell = view.cell
    if cell is None:
        raise RetainedViewError("a display composite requires the CELL plane")
    if not isinstance(cell, TerminalView):
        raise TypeError("composite CELL plane must be TerminalView")
    if (
        cell.presentation_epoch != view.presentation_epoch
        or cell.cols != view.geometry.cols
        or cell.rows != view.geometry.rows
    ):
        raise RetainedViewError("CELL plane does not match composite scope or geometry")
    if cell.revision > view.revision:
        raise RetainedViewError("CELL plane revision exceeds composite revision")

    retained = view.retained
    if retained is not None:
        if not isinstance(retained, SceneModelState):
            raise TypeError("composite retained plane must be SceneModelState or None")
        if retained.geometry != view.geometry:
            raise RetainedViewError("retained plane does not match composite geometry")
        if retained.revision > view.revision:
            raise RetainedViewError("retained plane revision exceeds composite revision")

    scope = DisplayScope(
        attachment_epoch=cell.attachment_epoch,
        session_id=cell.session_id,
        presentation_epoch=view.presentation_epoch,
        model_revision=view.revision,
        geometry_generation=view.geometry.generation,
        cell_revision=cell.revision,
        retained_revision=None if retained is None else retained.revision,
    )
    if retained is None:
        return scope, RetainedRootLabelPlane(False, False, ())
    if retained.retained_visible and not retained.retained_initialized:
        raise RetainedViewError("retained state is visible before initialization")
    if not retained.retained_visible:
        return scope, RetainedRootLabelPlane(
            retained.retained_initialized,
            False,
            (),
        )

    projected_regions: list[RetainedRegionDraw] = []
    for owner_key, owner_scene in retained.active.owners.items():
        if not isinstance(owner_scene, OwnerScene):
            raise RetainedViewError("retained scene contains an invalid owner value")
        _validate_owner_scope(owner_scene, owner_key, view)
        owner = owner_scene.owner

        for region_key, region in owner_scene.regions.items():
            if region_key != region.region_id or region.owner != owner:
                raise RetainedViewError("retained region map or owner identity is invalid")
            if region.geometry_generation != view.geometry.generation:
                raise RetainedViewError("retained region geometry stamp is stale")
            try:
                region.validate_geometry(view.geometry)
            except ValueError as exc:
                raise RetainedViewError(str(exc)) from exc

        for object_key, definition in owner_scene.objects.items():
            if object_key != definition.object_id or definition.owner != owner:
                raise RetainedViewError("retained object map or owner identity is invalid")
            if definition.region_id not in owner_scene.regions:
                raise RetainedViewError("retained object refers to a missing region")

        for region in owner_scene.regions.values():
            if not region.visible:
                continue
            labels: list[RetainedLabelDraw] = []
            for definition in owner_scene.objects.values():
                if definition.region_id != region.region_id:
                    continue
                if not _effectively_visible(definition, owner_scene.objects):
                    continue
                if isinstance(definition.body, GroupBody):
                    continue
                if not isinstance(definition.body, LabelBody):
                    raise RetainedViewError(
                        f"visible {definition.kind.name} object "
                        f"{definition.object_id} is unsupported by root-LABEL rendering"
                    )
                if definition.parent_object_id != 0:
                    raise RetainedViewError(
                        f"visible LABEL object {definition.object_id} is not parentless"
                    )
                bounds = definition.bounds
                body = definition.body
                labels.append(
                    RetainedLabelDraw(
                        object_id=definition.object_id,
                        z_order=definition.z_order,
                        left=bounds.left,
                        top=bounds.top,
                        right=bounds.right,
                        bottom=bounds.bottom,
                        red=body.color.red,
                        green=body.color.green,
                        blue=body.color.blue,
                        alpha=body.color.alpha,
                        horizontal_align=body.horizontal_align,
                        vertical_align=body.vertical_align,
                        ellipsize=body.ellipsize,
                        text=body.text,
                    )
                )
            labels.sort(key=lambda label: (label.z_order, label.object_id))
            projected_regions.append(
                RetainedRegionDraw(
                    owner_id=owner.owner_id,
                    owner_generation=owner.owner_generation,
                    region_id=region.region_id,
                    cell_x=region.cell_x,
                    cell_y=region.cell_y,
                    cell_cols=region.cell_cols,
                    cell_rows=region.cell_rows,
                    z_order=region.z_order,
                    clipped=region.clipped,
                    labels=tuple(labels),
                )
            )

    projected_regions.sort(
        key=lambda region: (region.z_order, region.owner_id, region.region_id)
    )
    return scope, RetainedRootLabelPlane(
        retained_initialized=retained.retained_initialized,
        retained_visible=True,
        regions=tuple(projected_regions),
    )


__all__ = [
    "DisplayScope",
    "RetainedLabelDraw",
    "RetainedRegionDraw",
    "RetainedRootLabelPlane",
    "RetainedViewError",
    "project_composite_root_labels",
]
