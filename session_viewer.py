#!/usr/bin/env python3
"""Pygame viewer/input client for a shared MegaPad session."""

from __future__ import annotations

import argparse
import base64
import binascii
import hashlib
import operator
import sys
import time
from collections import deque
from collections.abc import Mapping
from dataclasses import dataclass, field, replace
from pathlib import Path

from display import VirtualTerminal, glyph_extent
from rich_terminal.final_raster import FinalRaster
from rich_terminal.font_set import FontSet, discover_fallback_fonts
from rich_terminal.pygame_view import (
    CompositeDrawResult,
    ControlHitTarget,
    ControlIdentity,
    ControlSurface,
    HIT_MAP_ENTRY_TYPES,
    HitMapEntry,
    ItemHitTarget,
    PaintedRegion,
    PixelRect,
    PointerTarget,
    RegionOcclusion,
    ResidualPoint,
    TextHitTarget,
    TextPosition,
    composite_draw_plane,
    composite_draw_plane_result,
    hit_test_hit_map,
    opaque_cell_coverage,
    repaint_draw_plane_area,
    resolve_pointer,
    retained_plane_layout,
)
from rich_terminal.retained_model import ResourceFormat
from rich_terminal.retained_scene import ControlKind
from rich_terminal.retained_wire import ControlEventKind
from rich_terminal.retained_view import (
    DisplayScope,
    ImageResourceManifest,
    ItemViewDraw,
    MenuBarDraw,
    PlotDraw,
    PolylineDraw,
    RetainedDrawPlane,
    TextAreaDraw,
    TextGridDraw,
    WaveformDraw,
    retained_draw_control_ids,
)
from session import TerminalDisplayOffer, TerminalSnapshot
from shared_session import (
    DEFAULT_SOCKET,
    SessionClient,
    display_offer_from_wire,
    display_scope_to_wire,
    snapshot_from_wire,
)


ROOT = Path(__file__).resolve().parent
KEY_REPEAT_DELAY_MS = 400
KEY_REPEAT_INTERVAL_MS = 35
DEFAULT_PENDING_INPUT_EVENTS = 256
DISPLAY_CLAIM_RETRY_SECONDS = 0.25


def _nonnegative_wire_integer(value, name: str) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        normalized = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if normalized < 0:
        raise ValueError(f"{name} cannot be negative")
    return int(normalized)


def _host_integer(value, name: str) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        return int(operator.index(value))
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc


def _display_claimed(response) -> bool:
    if not isinstance(response, Mapping):
        raise RuntimeError("claim_display returned no response object")
    if set(response) != {"status", "claimed"}:
        raise RuntimeError("claim_display returned an invalid response shape")
    claimed = response.get("claimed")
    if not isinstance(claimed, bool):
        raise RuntimeError("claim_display returned no boolean claim state")
    expected_status = "claimed" if claimed else "display_busy"
    if response.get("status") != expected_status:
        raise RuntimeError(
            f"display claim failed: {response.get('status', 'missing status')}"
        )
    return claimed


def _status_display_required(status) -> bool:
    if not isinstance(status, Mapping):
        raise RuntimeError("status returned no response object")
    rich_terminal = status.get("rich_terminal")
    if not isinstance(rich_terminal, Mapping):
        raise RuntimeError("status has no rich-terminal state object")
    required = rich_terminal.get("display_required")
    if not isinstance(required, bool):
        raise RuntimeError("status has no boolean rich-terminal display requirement")
    return required


@dataclass(frozen=True, slots=True)
class _DisplayResourceKey:
    """Cache one immutable RGBA resource within its reset-stable scope."""

    attachment_epoch: int
    session_id: int
    presentation_epoch: int
    owner_id: int
    owner_generation: int
    resource_id: int
    format: ResourceFormat
    width: int
    height: int
    byte_length: int
    sha3_256: bytes

    @classmethod
    def from_manifest(
        cls,
        scope: DisplayScope,
        manifest: ImageResourceManifest,
    ) -> "_DisplayResourceKey":
        if not isinstance(scope, DisplayScope):
            raise TypeError("scope must be DisplayScope")
        if not isinstance(manifest, ImageResourceManifest):
            raise TypeError("manifest must be ImageResourceManifest")
        return cls(
            scope.attachment_epoch,
            scope.session_id,
            scope.presentation_epoch,
            manifest.owner_id,
            manifest.owner_generation,
            manifest.resource_id,
            manifest.format,
            manifest.width,
            manifest.height,
            manifest.byte_length,
            manifest.sha3_256,
        )

    @property
    def manifest_key(self) -> tuple:
        return (
            self.owner_id,
            self.owner_generation,
            self.resource_id,
            self.format,
            self.width,
            self.height,
            self.byte_length,
            self.sha3_256,
        )


@dataclass(slots=True)
class _DisplayResourceDownload:
    manifest: ImageResourceManifest
    data: bytearray = field(default_factory=bytearray)
    digest: object = field(default_factory=hashlib.sha3_256)

    @property
    def offset(self) -> int:
        return len(self.data)


@dataclass(slots=True)
class _CachedDisplayResource:
    """Keep the verified backing alive for a zero-copy Pygame surface."""

    surface: object
    pixels: bytearray


class _DisplayResourceCache:
    """Fetch and retain only resources needed by acknowledged/pending offers.

    Fetching is deliberately incremental: the main viewer asks for one exact
    chunk between event-pump passes and does not draw or flip the pending frame
    until every dependency has passed its length and SHA3 checks.
    """

    _REJECTION_STATUSES = {
        "stale_generation",
        "stale_display",
        "invalid_resource",
    }

    def __init__(self) -> None:
        self._surfaces: dict[_DisplayResourceKey, _CachedDisplayResource] = {}
        self._downloads: dict[_DisplayResourceKey, _DisplayResourceDownload] = {}
        self._acknowledged_keys: frozenset[_DisplayResourceKey] = frozenset()
        self._pending_token: tuple[int, DisplayScope] | None = None
        self._pending_generation: int | None = None
        self._pending_keys: tuple[_DisplayResourceKey, ...] = ()
        self._pending_manifests: dict[
            _DisplayResourceKey, ImageResourceManifest
        ] = {}

    @staticmethod
    def _offer_token(offer: TerminalDisplayOffer) -> tuple[int, DisplayScope]:
        if not isinstance(offer, TerminalDisplayOffer):
            raise TypeError("offer must be TerminalDisplayOffer")
        return offer.offer_id, offer.scope

    @staticmethod
    def _offer_resources(
        offer: TerminalDisplayOffer,
    ) -> tuple[ImageResourceManifest, ...]:
        resources = tuple(offer.retained.resources)
        if any(
            not isinstance(resource, ImageResourceManifest)
            for resource in resources
        ):
            raise TypeError("offer resources must be IMAGE resource manifests")
        return resources

    def clear(self) -> None:
        """Drop both physical-display generations and all partial bytes."""

        self._surfaces.clear()
        self._downloads.clear()
        self._acknowledged_keys = frozenset()
        self._pending_token = None
        self._pending_generation = None
        self._pending_keys = ()
        self._pending_manifests.clear()

    def stage(self, offer: TerminalDisplayOffer, generation: int) -> None:
        normalized_generation = _nonnegative_wire_integer(
            generation, "display resource generation"
        )
        manifests = self._offer_resources(offer)
        keys = tuple(
            _DisplayResourceKey.from_manifest(offer.scope, manifest)
            for manifest in manifests
        )
        if len(set(keys)) != len(keys):
            raise RuntimeError("display offer contains duplicate resource manifests")
        self._pending_token = self._offer_token(offer)
        self._pending_generation = normalized_generation
        self._pending_keys = keys
        self._pending_manifests = dict(zip(keys, manifests, strict=True))

        live_keys = self._acknowledged_keys | frozenset(keys)
        self._surfaces = {
            key: surface
            for key, surface in self._surfaces.items()
            if key in live_keys
        }
        self._downloads = {
            key: download
            for key, download in self._downloads.items()
            if key in keys and key not in self._surfaces
        }

    def _matches_pending(
        self,
        offer: TerminalDisplayOffer,
        generation: int,
    ) -> bool:
        return (
            self._pending_token == self._offer_token(offer)
            and self._pending_generation
            == _nonnegative_wire_integer(
                generation, "display resource generation"
            )
        )

    def pending_ready(
        self,
        offer: TerminalDisplayOffer,
        generation: int,
    ) -> bool:
        return self._matches_pending(offer, generation) and all(
            key in self._surfaces for key in self._pending_keys
        )

    def pending_surfaces(
        self,
        offer: TerminalDisplayOffer,
        generation: int,
    ) -> dict[tuple, object]:
        if not self.pending_ready(offer, generation):
            raise RuntimeError("pending display resources are not complete")
        return {
            key.manifest_key: self._surfaces[key].surface
            for key in self._pending_keys
        }

    @property
    def acknowledged_surfaces(self) -> dict[tuple, object]:
        return {
            key.manifest_key: self._surfaces[key].surface
            for key in self._acknowledged_keys
        }

    def promote(self, offer: TerminalDisplayOffer, generation: int) -> None:
        if not self.pending_ready(offer, generation):
            raise RuntimeError("cannot promote incomplete display resources")
        self._acknowledged_keys = frozenset(self._pending_keys)
        self._surfaces = {
            key: surface
            for key, surface in self._surfaces.items()
            if key in self._acknowledged_keys
        }
        self._downloads.clear()
        self._pending_token = None
        self._pending_generation = None
        self._pending_keys = ()
        self._pending_manifests.clear()

    @staticmethod
    def _decoded_chunk(response: Mapping, download: _DisplayResourceDownload) -> bytes:
        manifest = download.manifest
        expected_fields = {
            "status",
            "available",
            "owner_id",
            "owner_generation",
            "resource_id",
            "sha3_256",
            "offset",
            "next_offset",
            "byte_length",
            "data_base64",
            "eof",
        }
        if set(response) != expected_fields:
            raise RuntimeError("resource chunk response has invalid shape")
        if response.get("status") != "chunk" or response.get("available") is not True:
            raise RuntimeError("resource chunk response has invalid availability")
        for field_name, expected in (
            ("owner_id", manifest.owner_id),
            ("owner_generation", manifest.owner_generation),
            ("resource_id", manifest.resource_id),
            ("byte_length", manifest.byte_length),
        ):
            actual = _nonnegative_wire_integer(
                response.get(field_name), f"resource chunk {field_name}"
            )
            if actual != expected:
                raise RuntimeError(
                    f"resource chunk {field_name} does not match its manifest"
                )
        digest_text = response.get("sha3_256")
        if (
            not isinstance(digest_text, str)
            or digest_text != manifest.sha3_256.hex()
        ):
            raise RuntimeError("resource chunk digest does not match its manifest")
        offset = _nonnegative_wire_integer(
            response.get("offset"), "resource chunk offset"
        )
        next_offset = _nonnegative_wire_integer(
            response.get("next_offset"), "resource chunk next_offset"
        )
        if offset != download.offset:
            raise RuntimeError("resource chunk offset is not the requested offset")
        if not offset < next_offset <= manifest.byte_length:
            raise RuntimeError("resource chunk did not make bounded forward progress")
        eof = response.get("eof")
        if not isinstance(eof, bool):
            raise RuntimeError("resource chunk eof must be bool")
        if eof is not (next_offset == manifest.byte_length):
            raise RuntimeError("resource chunk eof does not match its next offset")
        encoded = response.get("data_base64")
        if not isinstance(encoded, str):
            raise RuntimeError("resource chunk data_base64 must be str")
        try:
            chunk = base64.b64decode(encoded, validate=True)
        except (binascii.Error, ValueError) as exc:
            raise RuntimeError("resource chunk has invalid base64 data") from exc
        if len(chunk) != next_offset - offset:
            raise RuntimeError("resource chunk byte count does not match its offsets")
        return chunk

    def fetch_pending_chunk(
        self,
        client,
        pygame_module,
        offer: TerminalDisplayOffer,
        generation: int,
    ) -> str:
        """Fetch at most one server-bounded chunk and report exact progress."""

        if not self._matches_pending(offer, generation):
            raise RuntimeError("resource fetch is outside the pending display offer")
        missing = next(
            (key for key in self._pending_keys if key not in self._surfaces),
            None,
        )
        if missing is None:
            return "ready"
        manifest = self._pending_manifests[missing]
        if manifest.format is not ResourceFormat.RGBA8:
            raise RuntimeError("display resource format is not RGBA8")
        download = self._downloads.setdefault(
            missing,
            _DisplayResourceDownload(manifest),
        )
        remaining = manifest.byte_length - download.offset
        if remaining <= 0:
            raise RuntimeError("resource download reached an uninstalled terminal state")
        response = client.request(
            "display_resource_chunk",
            generation=generation,
            display_offer_id=offer.offer_id,
            display_scope=display_scope_to_wire(offer.scope),
            owner_id=manifest.owner_id,
            owner_generation=manifest.owner_generation,
            resource_id=manifest.resource_id,
            sha3_256=manifest.sha3_256.hex(),
            offset=download.offset,
            max_bytes=remaining,
        )
        if not isinstance(response, Mapping):
            raise RuntimeError("resource chunk returned no response object")
        status = response.get("status")
        if status in self._REJECTION_STATUSES:
            if set(response) != {"status", "available"}:
                raise RuntimeError("rejected resource chunk response has invalid shape")
            if response.get("available") is not False:
                raise RuntimeError("rejected resource chunk response has invalid state")
            return str(status)

        chunk = self._decoded_chunk(response, download)
        download.data.extend(chunk)
        download.digest.update(chunk)
        if download.offset < manifest.byte_length:
            return "progress"
        if download.digest.digest() != manifest.sha3_256:
            raise RuntimeError("completed display resource failed SHA3 verification")
        try:
            surface = pygame_module.image.frombuffer(
                download.data,
                (manifest.width, manifest.height),
                "RGBA",
            )
        except AttributeError as exc:
            raise TypeError("pygame image API must expose frombuffer()") from exc
        try:
            surface_size = tuple(surface.get_size())
        except (AttributeError, TypeError, ValueError) as exc:
            raise TypeError("decoded resource surface must expose get_size()") from exc
        if surface_size != (manifest.width, manifest.height):
            raise RuntimeError("decoded resource surface has the wrong dimensions")
        self._surfaces[missing] = _CachedDisplayResource(
            surface,
            download.data,
        )
        del self._downloads[missing]
        return "ready" if self.pending_ready(offer, generation) else "progress"


def _accepted_presentation_revision(response) -> int | None:
    """Return the CELL cursor only for one accepted sink presentation."""

    if not isinstance(response, Mapping):
        raise RuntimeError("present returned no response object")
    status = response.get("status")
    if status in {"stale_display", "stale_generation"}:
        if set(response) != {"status", "presented"}:
            raise RuntimeError("rejected present response has invalid shape")
        if response.get("presented") is not False:
            raise RuntimeError("rejected present response has invalid state")
        return None
    if status not in {"presented", "duplicate"}:
        raise RuntimeError(
            f"present returned invalid status {status!r}"
        )
    if set(response) != {"status", "presented", "revision"}:
        raise RuntimeError("accepted present response has invalid shape")
    if response.get("presented") is not True:
        raise RuntimeError("accepted present response has invalid state")
    return _nonnegative_wire_integer(
        response.get("revision"), "present revision"
    )


class _RetainedDisplayState:
    """Keep offer delivery separate from acknowledged sink display state."""

    def __init__(self) -> None:
        self.since_offer = 0
        self.pending_offer: TerminalDisplayOffer | None = None
        self.pending_generation: int | None = None
        # The offer the sink presented last: the base of changes-only offers.
        self.presented_offer: TerminalDisplayOffer | None = None
        self._pending_resource_token: tuple[int, DisplayScope] | None = None
        self._pending_resources_ready = False
        self._pending_hit_token: tuple[int, DisplayScope] | None = None
        self._pending_hit_entries: tuple[HitMapEntry, ...] = ()
        self._pending_hit_map_rendered = False
        self._hit_map_token: tuple[int, DisplayScope] | None = None
        self._hit_entries: tuple[HitMapEntry, ...] = ()

    @property
    def retained_plane(self) -> RetainedDrawPlane | None:
        """The plane of the offer the sink presented last."""

        offer = self.presented_offer
        return None if offer is None else offer.retained

    @property
    def base_offer_id(self) -> int:
        """The presented offer a changes-only offer may name, or zero."""

        offer = self.presented_offer
        return 0 if offer is None else offer.offer_id

    @property
    def frame_plane(self) -> RetainedDrawPlane | None:
        if self.pending_offer is not None:
            return self.pending_offer.retained
        return self.retained_plane

    @property
    def hit_map_token(self) -> tuple[int, DisplayScope] | None:
        """Exact sink-acknowledged offer/scope owning the hit map."""

        return self._hit_map_token

    @property
    def hit_targets(self) -> tuple[ControlHitTarget, ...]:
        """Control-only view of the sink-acknowledged immutable map."""

        return tuple(
            entry
            for entry in self._hit_entries
            if isinstance(entry, ControlHitTarget)
        )

    @property
    def hit_entries(self) -> tuple[HitMapEntry, ...]:
        """Exact immutable map promoted only by accepted sink presentation."""

        return self._hit_entries

    @staticmethod
    def _offer_token(offer: TerminalDisplayOffer) -> tuple[int, DisplayScope]:
        return offer.offer_id, offer.scope

    @staticmethod
    def _validated_hit_entries(hit_entries) -> tuple[HitMapEntry, ...]:
        entries = tuple(hit_entries)
        if any(not isinstance(entry, HIT_MAP_ENTRY_TYPES) for entry in entries):
            raise TypeError(
                "hit_entries must contain only ControlHitTarget, "
                "RegionOcclusion, ControlSurface, TextHitTarget, or "
                "ItemHitTarget values"
            )
        return entries

    def _clear_hit_maps(self) -> None:
        self._pending_hit_token = None
        self._pending_hit_entries = ()
        self._pending_hit_map_rendered = False
        self._hit_map_token = None
        self._hit_entries = ()

    def _clear_pending_resources(self) -> None:
        self._pending_resource_token = None
        self._pending_resources_ready = False

    def reset(self) -> None:
        """Drop visual candidates while preserving the last sink ACK cursor."""

        self.pending_offer = None
        self.pending_generation = None
        self.presented_offer = None
        self._clear_pending_resources()
        self._clear_hit_maps()

    def stage(self, offer: TerminalDisplayOffer, generation: int) -> None:
        if not isinstance(offer, TerminalDisplayOffer):
            raise TypeError("offer must be TerminalDisplayOffer")
        pending_cursor = (
            0 if self.pending_offer is None else self.pending_offer.offer_id
        )
        if offer.offer_id <= max(self.since_offer, pending_cursor):
            raise RuntimeError("display offer did not advance the acknowledged cursor")
        normalized_generation = _nonnegative_wire_integer(
            generation, "display offer generation"
        )
        self.pending_offer = offer
        self.pending_generation = normalized_generation
        # Delivery of a newer candidate invalidates the older frame as local
        # input authority before the candidate crosses its sink boundary.
        self._hit_map_token = None
        self._hit_entries = ()
        token = self._offer_token(offer)
        self._pending_resource_token = token
        self._pending_resources_ready = not bool(offer.retained.resources)
        self._pending_hit_token = token
        self._pending_hit_entries = ()
        self._pending_hit_map_rendered = False

    @property
    def pending_resources_ready(self) -> bool:
        offer = self.pending_offer
        return bool(
            offer is not None
            and self._pending_resource_token == self._offer_token(offer)
            and self._pending_resources_ready
        )

    @property
    def poll_offer_cursor(self) -> int:
        """Suppress redelivery while a potentially large offer is fetched."""

        pending = self.pending_offer
        return max(
            self.since_offer,
            0 if pending is None else pending.offer_id,
        )

    def stage_resources_ready(self, offer: TerminalDisplayOffer) -> None:
        """Bind verified compositor resources to one exact pending offer."""

        if not isinstance(offer, TerminalDisplayOffer):
            raise TypeError("offer must be TerminalDisplayOffer")
        token = self._offer_token(offer)
        if self.pending_offer is None or token != self._offer_token(
            self.pending_offer
        ):
            raise RuntimeError("resources do not belong to the pending offer")
        if token != self._pending_resource_token:
            raise RuntimeError("pending resource token is inconsistent")
        self._pending_resources_ready = True

    def stage_frame_hit_map(
        self,
        offer: TerminalDisplayOffer,
        hit_entries,
    ) -> None:
        """Bind off-screen geometry to the exact pending offer, never authority."""

        if not isinstance(offer, TerminalDisplayOffer):
            raise TypeError("offer must be TerminalDisplayOffer")
        token = self._offer_token(offer)
        if self.pending_offer is None or token != self._offer_token(
            self.pending_offer
        ):
            raise RuntimeError("hit map does not belong to the pending offer")
        if token != self._pending_hit_token:
            raise RuntimeError("pending hit-map token is inconsistent")
        self._pending_hit_entries = self._validated_hit_entries(hit_entries)
        self._pending_hit_map_rendered = True

    def hit_test(
        self,
        x: int,
        y: int,
        *,
        display_token: tuple[int, DisplayScope] | None,
    ) -> ControlHitTarget | None:
        """Hit-test only when the input proof and sink map token agree."""

        if display_token is None or display_token != self._hit_map_token:
            return None
        return hit_test_hit_map(self._hit_entries, x, y)

    def resolve_pointer(
        self,
        x: int,
        y: int,
        *,
        display_token: tuple[int, DisplayScope] | None,
        cell_width: int,
        cell_height: int,
    ) -> PointerTarget | None:
        """Resolve one point only when the input proof and map token agree."""

        if display_token is None or display_token != self._hit_map_token:
            return None
        return resolve_pointer(
            self._hit_entries,
            x,
            y,
            cell_width=cell_width,
            cell_height=cell_height,
        )

    def text_target(
        self,
        identity: ControlIdentity,
        *,
        display_token: tuple[int, DisplayScope] | None,
    ) -> TextHitTarget | None:
        """Return the acknowledged text root with this identity, if any."""

        if display_token is None or display_token != self._hit_map_token:
            return None
        for entry in self._hit_entries:
            if isinstance(entry, TextHitTarget) and entry.identity == identity:
                return entry
        return None

    def item_target(
        self,
        identity: ControlIdentity,
        *,
        display_token: tuple[int, DisplayScope] | None,
    ) -> ItemHitTarget | None:
        """Return the acknowledged item view with this identity, if any."""

        if display_token is None or display_token != self._hit_map_token:
            return None
        for entry in self._hit_entries:
            if isinstance(entry, ItemHitTarget) and entry.identity == identity:
                return entry
        return None

    def finish_presentation(self, response) -> int | None:
        offer = self.pending_offer
        if offer is None:
            raise RuntimeError("present response has no pending display offer")
        revision = _accepted_presentation_revision(response)
        if revision is None:
            self.reset()
            return None
        token = self._offer_token(offer)
        if (
            self._pending_resource_token != token
            or not self._pending_resources_ready
            or self._pending_hit_token != token
            or not self._pending_hit_map_rendered
        ):
            self.reset()
            raise RuntimeError(
                "presented frame resources were not ready or its hit map "
                "was not rendered for the exact offer"
            )
        self.since_offer = offer.offer_id
        self.presented_offer = offer
        self._hit_map_token = token
        self._hit_entries = self._pending_hit_entries
        self.pending_offer = None
        self.pending_generation = None
        self._clear_pending_resources()
        self._pending_hit_token = None
        self._pending_hit_entries = ()
        self._pending_hit_map_rendered = False
        return revision


class _GuestKeyboardForwarder:
    """Forward pygame input once while keeping TEXTINPUT for composed text."""

    def __init__(
        self,
        pygame,
        client,
        *,
        generation: int = 0,
        max_pending_events: int = DEFAULT_PENDING_INPUT_EVENTS,
        input_enabled: bool = True,
        display_required: bool = False,
    ):
        if isinstance(max_pending_events, bool):
            raise ValueError("max_pending_events must be a positive integer")
        try:
            normalized_limit = operator.index(max_pending_events)
        except TypeError as exc:
            raise TypeError("max_pending_events must be an integer") from exc
        if normalized_limit <= 0:
            raise ValueError("max_pending_events must be a positive integer")
        self.pygame = pygame
        self.client = client
        self.generation = _nonnegative_wire_integer(
            generation, "input generation"
        )
        if not isinstance(input_enabled, bool):
            raise TypeError("input_enabled must be bool")
        if not isinstance(display_required, bool):
            raise TypeError("display_required must be bool")
        self.input_enabled = input_enabled
        self.display_required = display_required
        self.max_pending_events = int(normalized_limit)
        self.suppressed_text_keys: dict[int, set[str]] = {}
        self._pending_inputs: deque[tuple[str, dict]] = deque()
        self._display_ack: tuple[int, DisplayScope] | None = None
        self._display_transition = False
        self._keyboard_scope: DisplayScope | None = None
        self.last_error: str | None = None

    @property
    def pending_events(self) -> int:
        return len(self._pending_inputs)

    @property
    def display_ack(self) -> tuple[int, DisplayScope] | None:
        return self._display_ack

    def _enqueue_input(self, method: str, params: dict) -> bool:
        if len(self._pending_inputs) >= self.max_pending_events:
            self.last_error = (
                "input retention full while the guest is backpressured"
            )
            return False
        self._pending_inputs.append((method, params))
        return True

    def set_generation(self, generation: int) -> None:
        normalized = _nonnegative_wire_integer(
            generation, "input generation"
        )
        if normalized != self.generation:
            self._pending_inputs.clear()
            self._display_ack = None
            self._keyboard_scope = None
            self._display_transition = self.display_required
            self.generation = normalized
            self.last_error = None

    def set_input_enabled(self, enabled: bool) -> None:
        if not isinstance(enabled, bool):
            raise TypeError("enabled must be bool")
        if enabled == self.input_enabled:
            return
        self.input_enabled = enabled
        self.suppressed_text_keys.clear()
        self._pending_inputs.clear()
        self._display_ack = None
        self._keyboard_scope = None
        self._display_transition = False
        self.last_error = None

    def set_display_required(self, required: bool) -> None:
        if not isinstance(required, bool):
            raise TypeError("required must be bool")
        if required == self.display_required:
            return
        self.display_required = required
        self._pending_inputs.clear()
        self._display_ack = None
        self._keyboard_scope = None
        self._display_transition = required
        self.last_error = None

    def _retain_keyboard_events(self) -> None:
        # Keys/text are ordered user intentions, bound when they can be sent.
        # A control activation is tied to the old frame's exact hit map.
        self._pending_inputs = deque(
            (method, params) for method, params in self._pending_inputs
            if method in {"send_key", "send_text"}
        )

    def begin_display_offer(self) -> None:
        """Hold keyboard intentions until the next complete physical ACK."""

        self._retain_keyboard_events()
        self._display_ack = None
        self.display_required = True
        self._display_transition = True
        self.last_error = None

    def acknowledge_display_offer(
        self,
        offer_id: int,
        scope: DisplayScope,
    ) -> None:
        if isinstance(offer_id, bool):
            raise TypeError("offer_id must be an integer, not bool")
        normalized = operator.index(offer_id)
        if normalized < 1:
            raise ValueError("offer_id must be positive")
        if not isinstance(scope, DisplayScope):
            raise TypeError("scope must be DisplayScope")
        token = (int(normalized), scope)
        if token != self._display_ack:
            self._retain_keyboard_events()
            prior = self._keyboard_scope
            if prior is None or any(
                getattr(prior, name) != getattr(scope, name)
                for name in ("attachment_epoch", "session_id",
                             "presentation_epoch", "geometry_generation")
            ) or any(
                getattr(prior, name) is not None and (
                    getattr(scope, name) is None
                    or getattr(scope, name) < getattr(prior, name)
                )
                for name in ("model_revision", "cell_revision", "retained_revision")
            ):
                self._pending_inputs.clear()
        self._display_ack = token
        self._keyboard_scope = scope
        self._display_transition = False
        self.last_error = None

    def clear_display_context(self, *, waiting: bool) -> None:
        if not isinstance(waiting, bool):
            raise TypeError("waiting must be bool")
        self._pending_inputs.clear()
        self._display_ack = None
        self._keyboard_scope = None
        self._display_transition = waiting
        self.last_error = None

    def _bind_display_proof(self, params: dict) -> None:
        token = self._display_ack
        if token is None:
            return
        params["display_offer_id"] = token[0]
        params["display_scope"] = display_scope_to_wire(token[1])

    def _record_rejection(self, method: str, status: str | None) -> None:
        if status in {"stale", "failed", "stale_generation", "stale_display"}:
            self._pending_inputs.clear()
            self._keyboard_scope = None
        if status == "stale_display":
            self._display_ack = None
            self._display_transition = self.display_required
        elif status in {"stale", "failed", "stale_generation"}:
            self._display_ack = None
            self._display_transition = False
        self.last_error = (
            f"input rejected ({method}: {status or 'missing status'})"
        )

    def _request_now(self, method: str, **params) -> bool:
        """Send one frame-bound request now, or report that it was not sent.

        Pointer and text-position input names a place in the current
        acknowledged frame, so it is never queued behind other input or
        carried into a later frame.
        """

        if not self.input_enabled:
            self.last_error = "viewer is view-only; display lease is held elsewhere"
            return False
        if (
            self._display_transition
            or self._display_ack is None
            or self._pending_inputs
        ):
            return False
        request = dict(params)
        request["generation"] = self.generation
        self._bind_display_proof(request)
        result = self.client.request(method, **request)
        status = result.get("status")
        if status == "progress":
            return True
        if status != "backpressured":
            self._record_rejection(method, status)
        return False

    def send_pointer(
        self,
        column: int,
        row: int,
        *,
        buttons: int,
        modifiers: int,
        kind: int,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> bool:
        """Send one raw pointer event at a residual-content cell."""

        return self._request_now(
            "send_pointer",
            x=column,
            y=row,
            buttons=buttons,
            modifiers=modifiers,
            kind=kind,
            wheel_x=wheel_x,
            wheel_y=wheel_y,
        )

    def send_text_event(
        self,
        target: TextHitTarget,
        kind: ControlEventKind,
        *,
        modifiers: int,
        position: TextPosition | None = None,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> bool:
        """Send one PLACE, EXTEND, FOLLOW, or SCROLL intent for an
        acknowledged root."""

        if not isinstance(target, TextHitTarget):
            raise TypeError("target must be TextHitTarget")
        identity = target.identity
        params = {
            "owner_id": identity.owner_id,
            "owner_generation": identity.owner_generation,
            "control_id": identity.control_id,
            "event_kind": int(kind),
            "modifiers": modifiers,
        }
        if kind is ControlEventKind.SCROLL:
            params.update(wheel_x=wheel_x, wheel_y=wheel_y)
        else:
            if not isinstance(position, TextPosition):
                raise TypeError("PLACE, EXTEND, and FOLLOW require a TextPosition")
            params.update(
                content_revision=target.content_revision,
                item_key=position.item_key,
                scalar_offset=position.scalar_offset,
            )
        return self._request_now("send_text_event", **params)

    def send_item_event(
        self,
        target: ItemHitTarget,
        kind: ControlEventKind,
        *,
        modifiers: int,
        item_key: int = 0,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> bool:
        """Send one item event, or SCROLL, for an acknowledged item view."""

        if not isinstance(target, ItemHitTarget):
            raise TypeError("target must be ItemHitTarget")
        identity = target.identity
        params = {
            "owner_id": identity.owner_id,
            "owner_generation": identity.owner_generation,
            "control_id": identity.control_id,
            "event_kind": int(kind),
            "modifiers": modifiers,
        }
        if kind is ControlEventKind.SCROLL:
            params.update(wheel_x=wheel_x, wheel_y=wheel_y)
        else:
            params.update(content_revision=target.content_revision, item_key=item_key)
        return self._request_now("send_text_event", **params)

    def _request_input(self, method: str, **params) -> None:
        if not self.input_enabled:
            self._pending_inputs.clear()
            self.last_error = "viewer is view-only; display lease is held elsewhere"
            return
        params["generation"] = self.generation
        if self._display_transition or (
            self.display_required and self._display_ack is None
        ):
            if self._keyboard_scope is not None and method in {"send_key", "send_text"}:
                self._enqueue_input(method, params)
            else:
                self.last_error = "input waiting for current display acknowledgement"
            return
        if method not in {"send_key", "send_text"}:
            self._bind_display_proof(params)
        if self._pending_inputs:
            self._enqueue_input(method, params)
            return
        request = dict(params)
        self._bind_display_proof(request)
        result = self.client.request(method, **request)
        status = result.get("status")
        if status == "progress":
            return
        if status == "backpressured":
            self._enqueue_input(method, params)
            return
        self._record_rejection(method, status)

    def flush_pending(self) -> None:
        if not self.input_enabled or self._display_transition or (
            self.display_required and self._display_ack is None
        ):
            return
        while self._pending_inputs:
            method, params = self._pending_inputs[0]
            request = dict(params)
            if method in {"send_key", "send_text"}:
                self._bind_display_proof(request)
            result = self.client.request(method, **request)
            status = result.get("status")
            if status == "backpressured":
                return
            if status != "progress":
                self._pending_inputs.popleft()
                self._record_rejection(method, status)
                return
            self._pending_inputs.popleft()

    def key_down(self, event, *, repeated: bool = False) -> bool:
        key_name = _pygame_guest_key(self.pygame, event)
        if key_name is None:
            self.suppressed_text_keys.pop(event.key, None)
            return False
        if repeated and not _pygame_repeatable_guest_key(self.pygame, event):
            return True
        character = _pygame_modified_character(self.pygame, event)
        if character is not None:
            translated = getattr(event, "unicode", "")
            self.suppressed_text_keys[event.key] = {
                text for text in (character, translated) if text
            }
        self._request_input("send_key", key=key_name)
        return True

    def key_up(self, event) -> None:
        self.suppressed_text_keys.pop(event.key, None)

    def text_input(self, event) -> bool:
        if not event.text:
            return False
        if any(
            event.text in texts for texts in self.suppressed_text_keys.values()
        ):
            return True
        self._request_input("send_text", text=event.text)
        return True

    def activate_control(
        self,
        target: ControlHitTarget,
        *,
        modifiers: int = 0,
    ) -> bool:
        """Forward one renderer-qualified ACTIVATE intent with display proof."""

        if not isinstance(target, ControlHitTarget):
            raise TypeError("target must be ControlHitTarget")
        normalized_modifiers = _nonnegative_wire_integer(
            modifiers, "control modifiers"
        )
        if normalized_modifiers > 0x3F:
            raise ValueError("control modifiers contain reserved APT-1 bits")
        identity = target.identity
        self._request_input(
            "send_control_event",
            owner_id=identity.owner_id,
            owner_generation=identity.owner_generation,
            control_id=identity.control_id,
            modifiers=normalized_modifiers,
        )
        return True

    def reset(self) -> None:
        self.suppressed_text_keys.clear()

    def discard_pending(self) -> None:
        self._pending_inputs.clear()
        self.last_error = None

    def report_error(self, message: str) -> None:
        self.last_error = str(message)


# pygame buttons 1..3 are left, middle, and right: APT-1 button bits 0..2.
_POINTER_BUTTON_BITS = {1: 0x01, 2: 0x02, 3: 0x04}
_LEFT_BUTTON = 0x01
# A second press on the same item within this many seconds opens it.
_DOUBLE_PRESS_SECONDS = 0.45
_ITEM_ACTIONS = {
    "select": ControlEventKind.SELECT,
    "expand": ControlEventKind.EXPAND,
    "collapse": ControlEventKind.COLLAPSE,
    "check": ControlEventKind.CHECK,
}
_APT_SHIFT = 0x01
_APT_CTRL = 0x02
_WHEEL_LIMIT = (1 << 15) - 1


def _wheel_steps(value) -> int:
    steps = _host_integer(value, "wheel steps")
    return max(-_WHEEL_LIMIT, min(_WHEEL_LIMIT, steps))


class _PointerRouter:
    """Route pointer input through one sink-acknowledged hit map.

    A menu, menu item, or tab activates on a matching release in the same
    acknowledged frame.  A press on an enabled text root sends PLACE (EXTEND
    with Shift), and dragging from it sends EXTEND at the position under the
    pointer, clamped to the root.  A press on an item view sends EXPAND or
    COLLAPSE on a disclosure mark, CHECK on a check box, and otherwise
    SELECT, or OPEN for a second press on the same item within the
    double-press interval.  A press on CELL or residual content starts
    a raw gesture: its moves and release reach the guest at the cell under the
    pointer until no button is held.  Wheel input scrolls a text root or
    reaches residual content as raw wheel steps.

    Nothing is sent unless the acknowledged display is current.  A raw
    release that cannot be sent is owed and delivered once it can, so the
    guest always sees a gesture it saw start also end.
    """

    def __init__(
        self,
        display_state: _RetainedDisplayState,
        keyboard: _GuestKeyboardForwarder,
        *,
        cell_width: int,
        cell_height: int,
    ) -> None:
        if not isinstance(display_state, _RetainedDisplayState):
            raise TypeError("display_state must be _RetainedDisplayState")
        if not isinstance(keyboard, _GuestKeyboardForwarder):
            raise TypeError("keyboard must be _GuestKeyboardForwarder")
        self.display_state = display_state
        self.keyboard = keyboard
        self.cell_width = _nonnegative_wire_integer(cell_width, "cell width")
        self.cell_height = _nonnegative_wire_integer(cell_height, "cell height")
        if not self.cell_width or not self.cell_height:
            raise ValueError("cell geometry must be positive")
        self._observed_token: tuple[int, DisplayScope] | None = None
        self._hovered: ControlIdentity | None = None
        self._pressed_target: ControlHitTarget | None = None
        self._pressed_token: tuple[int, DisplayScope] | None = None
        self._raw_buttons = 0
        self._raw_cell: tuple[int, int] | None = None
        self._owed_release: tuple[tuple[int, int], int] | None = None
        self._text_identity: ControlIdentity | None = None
        self._text_position: TextPosition | None = None
        # The last item a press selected, for recognizing a double press.
        self._last_item_press: tuple[ControlIdentity, int, float] | None = None

    def _authority_token(self) -> tuple[int, DisplayScope] | None:
        display_ack = self.keyboard.display_ack
        if display_ack is None or display_ack != self.display_state.hit_map_token:
            return None
        return display_ack

    def _synchronize(self) -> tuple[int, DisplayScope] | None:
        token = self._authority_token()
        if token != self._observed_token:
            self._hovered = None
            self._pressed_target = None
            self._pressed_token = None
            self._observed_token = token
        return token

    @property
    def hovered(self) -> ControlIdentity | None:
        self._synchronize()
        return self._hovered

    @property
    def pressed(self) -> ControlIdentity | None:
        self._synchronize()
        if self._pressed_target is None:
            return None
        return self._pressed_target.identity

    @property
    def raw_buttons(self) -> int:
        """Buttons the guest was told are held in the current raw gesture."""

        return self._raw_buttons

    @property
    def release_owed(self) -> bool:
        return self._owed_release is not None

    def clear(self) -> None:
        """Drop renderer-local hover and press state for a changed frame.

        A raw gesture or text drag continues across frames; a menu or tab
        press does not, because activation needs one exact frame.
        """

        self._hovered = None
        self._pressed_target = None
        self._pressed_token = None
        self._observed_token = self._authority_token()

    def cancel(self) -> None:
        """End every gesture, as when the window loses focus or resizes."""

        self.clear()
        if self._raw_buttons:
            self._owed_release = (
                self._raw_cell or (0, 0),
                self.keyboard.generation,
            )
        self._raw_buttons = 0
        self._raw_cell = None
        self._text_identity = None
        self._text_position = None
        self.flush()

    @staticmethod
    def _point_and_extent(position, terminal_size) -> tuple[int, int, int, int]:
        try:
            x_value, y_value = position
        except (TypeError, ValueError) as exc:
            raise TypeError("position must be a two-item coordinate") from exc
        try:
            width_value, height_value = terminal_size
        except (TypeError, ValueError) as exc:
            raise TypeError("terminal_size must be a two-item extent") from exc
        x = _host_integer(x_value, "pointer x")
        y = _host_integer(y_value, "pointer y")
        width = _nonnegative_wire_integer(width_value, "terminal width")
        height = _nonnegative_wire_integer(height_value, "terminal height")
        return x, y, width, height

    def _cell(self, position, terminal_size) -> tuple[int, int]:
        """The cell under the pointer, clamped into the terminal grid."""

        x, y, width, height = self._point_and_extent(position, terminal_size)
        columns = max(1, width // self.cell_width)
        rows = max(1, height // self.cell_height)
        return (
            min(max(x // self.cell_width, 0), columns - 1),
            min(max(y // self.cell_height, 0), rows - 1),
        )

    def _resolve(self, position, terminal_size) -> PointerTarget | None:
        token = self._synchronize()
        x, y, width, height = self._point_and_extent(position, terminal_size)
        if token is None or x < 0 or y < 0 or x >= width or y >= height:
            return None
        return self.display_state.resolve_pointer(
            x,
            y,
            display_token=token,
            cell_width=self.cell_width,
            cell_height=self.cell_height,
        )

    def _send_raw(
        self,
        cell: tuple[int, int],
        *,
        kind: int,
        buttons: int,
        modifiers: int,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> bool:
        if self._authority_token() is None:
            return False
        return self.keyboard.send_pointer(
            cell[0],
            cell[1],
            buttons=buttons,
            modifiers=modifiers,
            kind=kind,
            wheel_x=wheel_x,
            wheel_y=wheel_y,
        )

    def flush(self) -> None:
        """Deliver an owed raw release once the displayed frame is current."""

        owed = self._owed_release
        if owed is None:
            return
        cell, generation = owed
        if generation != self.keyboard.generation:
            self._owed_release = None
            return
        if self._send_raw(cell, kind=3, buttons=0, modifiers=0):
            self._owed_release = None

    def move(self, position, terminal_size, *, modifiers: int = 0):
        target = self._resolve(position, terminal_size)
        self._hovered = (
            target.identity if isinstance(target, ControlHitTarget) else None
        )
        if self._raw_buttons:
            cell = self._cell(position, terminal_size)
            if cell != self._raw_cell and self._send_raw(
                cell,
                kind=1,
                buttons=self._raw_buttons,
                modifiers=modifiers,
            ):
                self._raw_cell = cell
        elif self._text_identity is not None:
            self._extend_text(position, terminal_size, modifiers)
        return target

    def _extend_text(self, position, terminal_size, modifiers: int) -> None:
        token = self._authority_token()
        if token is None:
            return
        target = self.display_state.text_target(
            self._text_identity,
            display_token=token,
        )
        if target is None or target.kind is not ControlKind.TEXT_AREA:
            self._text_identity = None
            self._text_position = None
            return
        x, y, _width, _height = self._point_and_extent(position, terminal_size)
        text_position = target.position_at(x, y, clamp=True)
        if text_position is None or text_position == self._text_position:
            return
        if self.keyboard.send_text_event(
            target,
            ControlEventKind.EXTEND,
            modifiers=modifiers,
            position=text_position,
        ):
            self._text_position = text_position

    def button_down(
        self,
        button: int,
        position,
        terminal_size,
        *,
        modifiers: int = 0,
    ) -> bool:
        bit = _POINTER_BUTTON_BITS.get(button)
        if bit is None:
            return False
        self.flush()
        if self._owed_release is not None:
            return False
        if self._raw_buttons:
            if self._raw_buttons & bit:
                return False
            buttons = self._raw_buttons | bit
            cell = self._cell(position, terminal_size)
            if self._send_raw(cell, kind=2, buttons=buttons, modifiers=modifiers):
                self._raw_buttons = buttons
                self._raw_cell = cell
                return True
            return False
        if self._text_identity is not None or self._pressed_target is not None:
            return False
        target = self._resolve(position, terminal_size)
        if isinstance(target, ControlHitTarget):
            if bit != _LEFT_BUTTON:
                return False
            self._hovered = target.identity
            self._pressed_target = target
            self._pressed_token = self._authority_token()
            return True
        if isinstance(target, TextHitTarget):
            if bit != _LEFT_BUTTON:
                return False
            x, y, _width, _height = self._point_and_extent(position, terminal_size)
            # SEMANTIC-CONTENT-1: a press on a link follows it with Ctrl, and
            # in read-only text without Shift or Ctrl; no drag follows it.
            if modifiers & _APT_CTRL or (
                target.read_only and not modifiers & (_APT_SHIFT | _APT_CTRL)
            ):
                link = target.link_at(x, y)
                if link is not None:
                    return self.keyboard.send_text_event(
                        target,
                        ControlEventKind.FOLLOW,
                        modifiers=modifiers,
                        position=link,
                    )
            text_position = target.position_at(x, y)
            if text_position is None:
                return False
            kind = (
                ControlEventKind.EXTEND
                if modifiers & _APT_SHIFT and target.kind is ControlKind.TEXT_AREA
                else ControlEventKind.PLACE
            )
            if not self.keyboard.send_text_event(
                target,
                kind,
                modifiers=modifiers,
                position=text_position,
            ):
                return False
            if target.kind is ControlKind.TEXT_AREA:
                self._text_identity = target.identity
                self._text_position = text_position
            return True
        if isinstance(target, ItemHitTarget):
            if bit != _LEFT_BUTTON:
                return False
            x, y, _width, _height = self._point_and_extent(position, terminal_size)
            hit = target.item_at(x, y)
            if hit is None:
                return False
            action, item_key = hit
            kind = _ITEM_ACTIONS[action]
            now = time.monotonic()
            if kind is ControlEventKind.SELECT:
                last = self._last_item_press
                if (
                    last is not None
                    and last[0] == target.identity
                    and last[1] == item_key
                    and now - last[2] <= _DOUBLE_PRESS_SECONDS
                ):
                    # SEMANTIC-CONTENT-1: a second press on the same item
                    # within the double-press interval opens it.
                    kind = ControlEventKind.OPEN
            self._last_item_press = (
                (target.identity, item_key, now)
                if kind is ControlEventKind.SELECT
                else None
            )
            return self.keyboard.send_item_event(
                target, kind, modifiers=modifiers, item_key=item_key
            )
        if isinstance(target, ResidualPoint):
            cell = (target.column, target.row)
            if self._send_raw(cell, kind=2, buttons=bit, modifiers=modifiers):
                self._raw_buttons = bit
                self._raw_cell = cell
                return True
        return False

    def button_up(
        self,
        button: int,
        position,
        terminal_size,
        *,
        modifiers: int = 0,
    ) -> bool:
        bit = _POINTER_BUTTON_BITS.get(button)
        if bit is None:
            return False
        if self._raw_buttons & bit:
            remaining = self._raw_buttons & ~bit
            cell = self._cell(position, terminal_size)
            if self._send_raw(cell, kind=3, buttons=remaining, modifiers=modifiers):
                self._raw_buttons = remaining
                self._raw_cell = cell if remaining else None
                return True
            # The guest saw the gesture start; make sure it also sees it end.
            self._owed_release = (cell, self.keyboard.generation)
            self._raw_buttons = 0
            self._raw_cell = None
            return False
        if bit != _LEFT_BUTTON:
            return False
        if self._text_identity is not None:
            self._text_identity = None
            self._text_position = None
            return False
        target = self._resolve(position, terminal_size)
        pressed = self._pressed_target
        pressed_token = self._pressed_token
        current_token = self._authority_token()
        self._pressed_target = None
        self._pressed_token = None
        self._hovered = (
            target.identity if isinstance(target, ControlHitTarget) else None
        )
        if (
            not isinstance(target, ControlHitTarget)
            or pressed is None
            or target != pressed
            or pressed_token != current_token
        ):
            return False
        self.keyboard.activate_control(target, modifiers=modifiers)
        return True

    def wheel(
        self,
        steps_x,
        steps_y,
        position,
        terminal_size,
        *,
        modifiers: int = 0,
    ) -> bool:
        """Send host wheel steps (positive Y is up) as APT detents (down)."""

        wheel_x = _wheel_steps(steps_x)
        wheel_y = -_wheel_steps(steps_y)
        if not wheel_x and not wheel_y:
            return False
        target = self._resolve(position, terminal_size)
        if isinstance(target, TextHitTarget):
            return self.keyboard.send_text_event(
                target,
                ControlEventKind.SCROLL,
                modifiers=modifiers,
                wheel_x=wheel_x,
                wheel_y=wheel_y,
            )
        if isinstance(target, ItemHitTarget):
            return self.keyboard.send_item_event(
                target,
                ControlEventKind.SCROLL,
                modifiers=modifiers,
                wheel_x=wheel_x,
                wheel_y=wheel_y,
            )
        if isinstance(target, ResidualPoint):
            return self._send_raw(
                (target.column, target.row),
                kind=4,
                buttons=self._raw_buttons,
                modifiers=modifiers,
                wheel_x=wheel_x,
                wheel_y=wheel_y,
            )
        return False


def _retry_display_claim(
    client,
    *,
    keyboard: _GuestKeyboardForwarder,
    display_state: _RetainedDisplayState,
    revision: int,
    resource_cache: _DisplayResourceCache | None = None,
) -> tuple[bool, int, bool]:
    """Retry one observer lease claim and invalidate state on exact takeover."""

    keyboard.set_input_enabled(False)
    claimed = _display_claimed(client.request("claim_display"))
    if not claimed:
        return False, revision, False
    display_state.reset()
    if resource_cache is not None:
        resource_cache.clear()
    keyboard.clear_display_context(waiting=keyboard.display_required)
    keyboard.set_input_enabled(True)
    return True, -1, True


def apply_terminal_snapshot(
    terminal: VirtualTerminal,
    snapshot: TerminalSnapshot,
) -> None:
    if not isinstance(snapshot, TerminalSnapshot):
        raise TypeError("snapshot must be TerminalSnapshot")
    if terminal.cols != snapshot.cols or terminal.rows != snapshot.rows:
        terminal.resize(snapshot.cols, snapshot.rows)
    with terminal._lock:
        terminal.grid = [
            [(cell.char, cell.fg, cell.bg, cell.attrs) for cell in row]
            for row in snapshot.cells
        ]
        terminal.cx = snapshot.cursor_col
        terminal.cy = snapshot.cursor_row
        terminal.cursor_visible = snapshot.cursor_visible
        terminal._in_alt_screen = snapshot.alternate_screen
        terminal._dirty = True


def apply_snapshot(terminal: VirtualTerminal, wire: dict) -> None:
    apply_terminal_snapshot(terminal, snapshot_from_wire(wire))


def _accept_screen_update(
    update,
    *,
    display_holder: bool,
    terminal: VirtualTerminal,
    keyboard: _GuestKeyboardForwarder,
    display_state: _RetainedDisplayState,
    revision: int,
    resource_cache: _DisplayResourceCache | None = None,
) -> tuple[int, bool]:
    """Consume one coherent screen result and return its CELL cursor/resize."""

    if not isinstance(display_holder, bool):
        raise TypeError("display_holder must be bool")
    if not isinstance(update, Mapping):
        raise RuntimeError("screen returned no response object")
    required_fields = {"changed", "revision"}
    allowed_fields = required_fields | {"snapshot"}
    if display_holder:
        required_fields.add("generation")
        allowed_fields |= {"generation", "display_offer"}
    if not required_fields <= set(update) or not set(update) <= allowed_fields:
        raise RuntimeError("screen returned an invalid response shape")
    changed = update.get("changed")
    if not isinstance(changed, bool):
        raise RuntimeError("screen returned no boolean changed state")
    has_payload = "snapshot" in update or "display_offer" in update
    if changed is not has_payload:
        raise RuntimeError("screen changed state does not match its payload")
    response_revision = _nonnegative_wire_integer(
        update.get("revision"), "screen revision"
    )
    old_size = (terminal.cols, terminal.rows)
    if display_holder:
        screen_generation = _nonnegative_wire_integer(
            update.get("generation"), "screen generation"
        )
        if screen_generation != keyboard.generation:
            keyboard.set_generation(screen_generation)
            revision = -1
            display_state.reset()
            if resource_cache is not None:
                resource_cache.clear()
    if revision < 0 and not has_payload:
        raise RuntimeError("screen refresh returned no CELL or display offer")
    if "snapshot" in update:
        apply_snapshot(terminal, update["snapshot"])
        revision = response_revision
    if "display_offer" in update:
        if not display_holder:
            raise RuntimeError("nonholder received a retained display offer")
        offer = display_offer_from_wire(
            update["display_offer"], display_state.presented_offer
        )
        display_state.stage(offer, update["generation"])
        if resource_cache is not None:
            resource_cache.stage(offer, update["generation"])
        apply_terminal_snapshot(terminal, offer.cell)
        keyboard.begin_display_offer()
    elif "snapshot" in update:
        display_state.reset()
        if resource_cache is not None:
            resource_cache.clear()
        keyboard.clear_display_context(waiting=keyboard.display_required)
    return revision, old_size != (terminal.cols, terminal.rows)


def _accept_status_update(
    latest,
    *,
    keyboard: _GuestKeyboardForwarder,
    display_state: _RetainedDisplayState,
    revision: int,
    resource_cache: _DisplayResourceCache | None = None,
) -> tuple[int, bool]:
    """Apply display-relevant status and report whether CELL must be refetched."""

    latest_required = _status_display_required(latest)
    latest_generation = _nonnegative_wire_integer(
        latest.get("generation"), "status generation"
    )
    refresh_required = False
    if latest_generation != keyboard.generation:
        keyboard.set_generation(latest_generation)
        revision = -1
        display_state.reset()
        if resource_cache is not None:
            resource_cache.clear()
        refresh_required = True
    fallback_context = not latest_required and (
        keyboard.display_required
        or display_state.pending_offer is not None
        or display_state.retained_plane is not None
        or keyboard.display_ack is not None
    )
    if fallback_context:
        revision = -1
        display_state.reset()
        if resource_cache is not None:
            resource_cache.clear()
        refresh_required = True
    keyboard.set_display_required(latest_required)
    return revision, refresh_required


def _paint_terminal_cursor(
    pygame_module,
    surface,
    terminal: VirtualTerminal,
    cell_width: int,
    cell_height: int,
    *,
    show_cursor: bool,
) -> None:
    with terminal._lock:
        cursor_visible = terminal.cursor_visible
        cursor_col = terminal.cx
        cursor_row = terminal.cy
        cols = terminal.cols
        rows = terminal.rows
    if (
        show_cursor
        and cursor_visible
        and 0 <= cursor_col < cols
        and 0 <= cursor_row < rows
    ):
        pygame_module.draw.rect(
            surface,
            (255, 255, 255),
            (
                cursor_col * cell_width,
                cursor_row * cell_height + cell_height - 2,
                cell_width,
                2,
            ),
        )


def _cell_coverage(pygame_module, terminal, retained_plane, cell_width, cell_height):
    """Cells the retained plane repaints opaquely, which CELL may skip."""

    if retained_plane is None:
        return None
    with terminal._lock:
        cols, rows = terminal.cols, terminal.rows
    return opaque_cell_coverage(
        pygame_module, retained_plane, cols, rows, cell_width, cell_height
    )


def compose_terminal_frame(
    pygame_module,
    terminal: VirtualTerminal,
    font,
    cell_width: int,
    cell_height: int,
    *,
    retained_plane: RetainedDrawPlane | None,
    show_cursor: bool,
    glyph_cache: dict | None = None,
    resource_surfaces: Mapping | None = None,
):
    """Render CELL, then retained draws, then the terminal cursor."""

    surface = terminal.render(
        pygame_module,
        font,
        cell_width,
        cell_height,
        show_cursor=False,
        _cache=glyph_cache,
        covered=_cell_coverage(
            pygame_module, terminal, retained_plane, cell_width, cell_height
        ),
    )
    if retained_plane is not None:
        if resource_surfaces is None:
            composite_draw_plane(
                pygame_module,
                surface,
                retained_plane,
                font,
                cell_width,
                cell_height,
            )
        else:
            composite_draw_plane(
                pygame_module,
                surface,
                retained_plane,
                font,
                cell_width,
                cell_height,
                resource_surfaces=resource_surfaces,
            )
    _paint_terminal_cursor(
        pygame_module,
        surface,
        terminal,
        cell_width,
        cell_height,
        show_cursor=show_cursor,
    )
    return surface


def compose_terminal_frame_result(
    pygame_module,
    terminal: VirtualTerminal,
    font,
    cell_width: int,
    cell_height: int,
    *,
    retained_plane: RetainedDrawPlane | None,
    show_cursor: bool,
    glyph_cache: dict | None = None,
    control_font=None,
    hovered: ControlIdentity | None = None,
    pressed: ControlIdentity | None = None,
    resource_surfaces: Mapping | None = None,
) -> CompositeDrawResult:
    """Render the complete frame and return hits from that exact paint pass."""

    surface = terminal.render(
        pygame_module,
        font,
        cell_width,
        cell_height,
        show_cursor=False,
        _cache=glyph_cache,
        covered=_cell_coverage(
            pygame_module, terminal, retained_plane, cell_width, cell_height
        ),
    )
    hit_entries: tuple[HitMapEntry, ...] = ()
    painted_regions = ()
    if retained_plane is not None:
        compositor_kwargs = {
            "control_font": control_font,
            "hovered": hovered,
            "pressed": pressed,
        }
        if resource_surfaces is not None:
            compositor_kwargs["resource_surfaces"] = resource_surfaces
        retained_result = composite_draw_plane_result(
            pygame_module,
            surface,
            retained_plane,
            font,
            cell_width,
            cell_height,
            **compositor_kwargs,
        )
        hit_entries = retained_result.hit_entries
        painted_regions = retained_result.regions
    _paint_terminal_cursor(
        pygame_module,
        surface,
        terminal,
        cell_width,
        cell_height,
        show_cursor=show_cursor,
    )
    return CompositeDrawResult(surface, hit_entries, painted_regions)


def capture_final_terminal_raster(pygame_module, surface) -> FinalRaster:
    """Freeze exact RGB pixels for an explicitly selected damage-aware sink.

    The ordinary SDL reference sink does not call this helper: its synchronous
    completion boundary is a successful ``pygame.display.flip()`` and it has no
    partial-refresh consumer.  A physical sink calls this only after CELL, all
    rich planes, and the cursor have been composed.
    """

    try:
        width, height = surface.get_size()
    except (AttributeError, TypeError, ValueError) as exc:
        raise TypeError("surface must expose a two-dimensional get_size()") from exc
    try:
        pixels = pygame_module.image.tobytes(surface, "RGB")
    except AttributeError as exc:
        raise TypeError("pygame image API must expose tobytes()") from exc
    return FinalRaster(
        width=width,
        height=height,
        bytes_per_pixel=3,
        pixel_format="RGB888",
        pixels=pixels,
    )


@dataclass(frozen=True, slots=True)
class ComposedFrame:
    """A composed frame, what it was composed from, and how it was reached.

    ``damage`` holds the rectangles repainted from the previous frame, or is
    None when this frame was composed in full.  The inputs let the next frame
    repaint only what changes (docs/viewer-partial-repaint.md).
    """

    surface: object
    hit_entries: tuple[HitMapEntry, ...]
    regions: tuple[PaintedRegion, ...]
    damage: tuple[PixelRect, ...] | None
    grid: tuple[tuple, ...]
    cursor: tuple[int, int] | None
    plane: RetainedDrawPlane | None
    hovered: ControlIdentity | None
    pressed: ControlIdentity | None
    geometry: tuple[int, int, int, int]
    font: object
    control_font: object
    glyph_cache: dict
    resource_surfaces: object


# Repainted only whole: these painters clip diagonal lines or fill polygons
# with rounding, and a menu bar skips its shadow when its anchor is clipped
# out, while an open menu's popups may paint anywhere in the region.
_REPAINTED_WHOLE = (PolylineDraw, PlotDraw, WaveformDraw, MenuBarDraw)


def _revision_only(previous, draw) -> bool:
    """Whether DRAW differs from PREVIOUS only in its content revision, which
    no painter reads; only its hit entries carry it."""

    return (
        type(draw) is type(previous)
        and isinstance(draw, (TextAreaDraw, TextGridDraw, ItemViewDraw))
        and replace(
            draw,
            content=replace(
                draw.content, content_revision=previous.content.content_revision
            ),
        )
        == previous
    )


def _with_revision(entries, revision: int) -> tuple[HitMapEntry, ...]:
    return tuple(
        replace(entry, content_revision=revision)
        if isinstance(entry, (TextHitTarget, ItemHitTarget))
        else entry
        for entry in entries
    )


def _region_header(region) -> tuple:
    return (
        region.logical_x,
        region.logical_y,
        region.logical_cols,
        region.logical_rows,
        region.clip_x,
        region.clip_y,
        region.clip_cols,
        region.clip_rows,
        region.z_order,
        region.clipped,
    )


def _area(rect) -> int:
    return (rect[2] - rect[0]) * (rect[3] - rect[1])


def _merged(rects) -> list[tuple[int, int, int, int]]:
    """Unite overlapping rectangles whose bounding box is no larger than the
    two together.  Overlapping rectangles may remain: each repaint recomputes
    its whole rectangle, so repainting both is still exact."""

    rects = [rect for rect in rects if rect[0] < rect[2] and rect[1] < rect[3]]
    changed = True
    while changed:
        changed = False
        united: list[tuple[int, int, int, int]] = []
        for rect in rects:
            for index, other in enumerate(united):
                box = (
                    min(rect[0], other[0]),
                    min(rect[1], other[1]),
                    max(rect[2], other[2]),
                    max(rect[3], other[3]),
                )
                if (
                    rect[0] < other[2] and other[0] < rect[2]
                    and rect[1] < other[3] and other[1] < rect[3]
                    and _area(box) <= _area(rect) + _area(other)
                ):
                    united[index] = box
                    changed = True
                    break
            else:
                united.append(rect)
        rects = united
    return rects


def _edges(rect: PixelRect | None) -> tuple[int, int, int, int] | None:
    return None if rect is None else (rect.left, rect.top, rect.right, rect.bottom)


def _frame_damage(
    previous: ComposedFrame,
    grid,
    cursor,
    plane,
    layout,
    hovered,
    pressed,
    geometry,
    cell_width: int,
    cell_height: int,
) -> list[tuple[int, int, int, int]] | None:
    """The rectangles to repaint, or None when the frame must be composed in
    full: the previous and new extents of every paint operation that
    changed, united, and grown over the draws that are repainted only whole.
    """

    cols, rows = geometry[0], geometry[1]
    width, height = cols * cell_width, rows * cell_height
    rects: list[tuple[int, int, int, int] | None] = []

    # A changed CELL row repaints its full-width band, down as far as a
    # glyph can reach.
    band = max(cell_height, glyph_extent(previous.glyph_cache)[1])
    for row, (cells, old) in enumerate(zip(grid, previous.grid)):
        if cells is not old and cells != old:
            top = row * cell_height
            rects.append((0, top, width, min(height, top + band)))
    if cursor != previous.cursor:
        for cell in (previous.cursor, cursor):
            if cell is not None:
                rects.append((
                    cell[0] * cell_width,
                    cell[1] * cell_height,
                    (cell[0] + 1) * cell_width,
                    (cell[1] + 1) * cell_height,
                ))

    whole: list[tuple[int, int, int, int]] = []
    if plane is not None:
        old_plane = previous.plane
        old_regions = {
            record.key: (region, record)
            for region, record in zip(old_plane.regions, previous.regions)
        }
        new_keys = {planned.key for planned in layout}
        if [key for key in old_regions if key in new_keys] != [
            planned.key for planned in layout if planned.key in old_regions
        ]:
            return None
        old_series = {history.key: history for history in old_plane.series}
        new_series = {history.key: history for history in plane.series}
        changed_series = {
            key
            for key in old_series.keys() | new_series.keys()
            if old_series.get(key) != new_series.get(key)
        }
        for region, planned in zip(plane.regions, layout):
            for draw, planned_draw in zip(region.draws, planned.draws):
                if isinstance(draw, _REPAINTED_WHOLE) and planned_draw.extent is not None:
                    whole.append(_edges(planned_draw.extent))
            found = old_regions.get(planned.key)
            if found is None or _region_header(found[0]) != _region_header(region):
                rects.append(_edges(planned.viewport))
                if found is not None:
                    rects.append(_edges(found[1].viewport))
                continue
            old_region, old_record = found
            old_draws = {
                draw_record.key: (draw, draw_record)
                for draw, draw_record in zip(old_region.draws, old_record.draws)
            }
            for draw, planned_draw in zip(region.draws, planned.draws):
                old = old_draws.pop(planned_draw.key, None)
                if old is None:
                    rects.append(_edges(planned_draw.extent))
                    continue
                old_draw, old_draw_record = old
                if old_draw is draw or old_draw == draw or _revision_only(old_draw, draw):
                    series_id = getattr(draw, "series_id", None)
                    if series_id is None or (
                        region.owner_id, region.owner_generation, series_id
                    ) not in changed_series:
                        continue
                rects.append(_edges(old_draw_record.extent))
                rects.append(_edges(planned_draw.extent))
            rects.extend(_edges(record.extent) for _draw, record in old_draws.values())
        rects.extend(
            _edges(record.viewport)
            for key, (_region, record) in old_regions.items()
            if key not in new_keys
        )
        # Hover and press change only the pixels of the draws that carry
        # the controls they name.
        identities = set()
        if hovered != previous.hovered:
            identities.update((hovered, previous.hovered))
        if pressed != previous.pressed:
            identities.update((pressed, previous.pressed))
        identities.discard(None)
        for draw_plane, records in ((old_plane, previous.regions), (plane, layout)):
            if not identities:
                break
            for region, record in zip(draw_plane.regions, records):
                for draw, draw_record in zip(region.draws, record.draws):
                    ids = retained_draw_control_ids(draw)
                    if any(
                        identity.owner_id == region.owner_id
                        and identity.owner_generation == region.owner_generation
                        and identity.control_id in ids
                        for identity in identities
                    ):
                        rects.append(_edges(draw_record.extent))

    damage = _merged(
        (max(0, rect[0]), max(0, rect[1]), min(width, rect[2]), min(height, rect[3]))
        for rect in rects
        if rect is not None
    )
    while True:
        grown = []
        for rect in damage:
            for extent in whole:
                if (
                    extent[0] < rect[2] and rect[0] < extent[2]
                    and extent[1] < rect[3] and rect[1] < extent[3]
                ):
                    rect = (
                        min(rect[0], extent[0]),
                        min(rect[1], extent[1]),
                        max(rect[2], extent[2]),
                        max(rect[3], extent[3]),
                    )
            grown.append(rect)
        grown = _merged(grown)
        if grown == damage:
            break
        damage = grown
    if 2 * sum(_area(rect) for rect in damage) > width * height:
        return None
    return damage


def compose_terminal_frame_changes(
    pygame_module,
    terminal: VirtualTerminal,
    font,
    cell_width: int,
    cell_height: int,
    *,
    retained_plane: RetainedDrawPlane | None,
    show_cursor: bool,
    glyph_cache: dict,
    control_font=None,
    hovered: ControlIdentity | None = None,
    pressed: ControlIdentity | None = None,
    resource_surfaces: Mapping | None = None,
    previous: ComposedFrame | None = None,
) -> ComposedFrame:
    """Compose the frame, repainting in PREVIOUS's surface only what changed.

    The frame is exactly what ``compose_terminal_frame_result`` composes
    from the same inputs, pixel for pixel and entry for entry.  It is
    composed in full for the first frame, after a change of geometry, fonts,
    glyph cache, retained visibility or IMAGE resources, when regions are
    reordered, and when the damage would cover more than half the frame.
    """

    control_font = font if control_font is None else control_font
    with terminal._lock:
        grid = tuple(tuple(row) for row in terminal.grid)
        cols, rows = terminal.cols, terminal.rows
        cursor = (
            (terminal.cx, terminal.cy)
            if show_cursor
            and terminal.cursor_visible
            and 0 <= terminal.cx < cols
            and 0 <= terminal.cy < rows
            else None
        )
    geometry = (cols, rows, cell_width, cell_height)
    layout = None
    damage = None
    if (
        previous is not None
        and previous.geometry == geometry
        and previous.font is font
        and previous.control_font is control_font
        and previous.glyph_cache is glyph_cache
        and (previous.plane is None) == (retained_plane is None)
        and (
            retained_plane is None
            or (
                retained_plane.retained_initialized,
                retained_plane.retained_visible,
                retained_plane.resources,
            )
            == (
                previous.plane.retained_initialized,
                previous.plane.retained_visible,
                previous.plane.resources,
            )
            and (not retained_plane.resources
                 or resource_surfaces is previous.resource_surfaces)
        )
    ):
        if retained_plane is not None:
            layout = retained_plane_layout(
                pygame_module,
                retained_plane,
                cols * cell_width,
                rows * cell_height,
                cell_width,
                cell_height,
                control_font=control_font,
            )
        damage = _frame_damage(
            previous,
            grid,
            cursor,
            retained_plane,
            layout,
            hovered,
            pressed,
            geometry,
            cell_width,
            cell_height,
        )
    if damage is None:
        result = compose_terminal_frame_result(
            pygame_module,
            terminal,
            font,
            cell_width,
            cell_height,
            retained_plane=retained_plane,
            show_cursor=show_cursor,
            glyph_cache=glyph_cache,
            control_font=control_font,
            hovered=hovered,
            pressed=pressed,
            resource_surfaces=resource_surfaces,
        )
        return ComposedFrame(
            result.surface, result.hit_entries, result.regions, None, grid, cursor,
            retained_plane, hovered, pressed, geometry, font, control_font,
            glyph_cache, resource_surfaces,
        )

    surface = previous.surface
    extents = {
        (planned.key, planned_draw.key): planned_draw.extent
        for planned in layout or ()
        for planned_draw in planned.draws
    }
    painted: dict = {}
    for left, top, right, bottom in damage:
        area = PixelRect(left, top, right, bottom)
        rect = pygame_module.Rect(left, top, right - left, bottom - top)
        prior_clip = surface.get_clip()
        try:
            surface.set_clip(rect)
            surface.fill(VirtualTerminal._DEFAULT_BG)
            terminal.paint_area(
                pygame_module, surface, font, cell_width, cell_height, rect,
                _cache=glyph_cache,
            )
        finally:
            surface.set_clip(prior_clip)
        if retained_plane is not None:
            for key, value in repaint_draw_plane_area(
                pygame_module,
                surface,
                retained_plane,
                font,
                cell_width,
                cell_height,
                area,
                layout=layout,
                resource_surfaces=resource_surfaces,
                control_font=control_font,
                hovered=hovered,
                pressed=pressed,
            ).items():
                extent = extents[key]
                # Only a draw painted whole under this clip made exact entries.
                if (
                    extent.left >= left and extent.top >= top
                    and extent.right <= right and extent.bottom <= bottom
                ):
                    painted[key] = value
        if cursor is not None and rect.colliderect(
            (cursor[0] * cell_width, cursor[1] * cell_height, cell_width, cell_height)
        ):
            prior_clip = surface.get_clip()
            try:
                surface.set_clip(rect)
                _paint_terminal_cursor(
                    pygame_module, surface, terminal, cell_width, cell_height,
                    show_cursor=show_cursor,
                )
            finally:
                surface.set_clip(prior_clip)

    regions: list[PaintedRegion] = []
    if retained_plane is not None:
        old_regions = {
            record.key: (region, record)
            for region, record in zip(previous.plane.regions, previous.regions)
        }
        for region, planned in zip(retained_plane.regions, layout):
            found = old_regions.get(planned.key)
            if found is not None and _region_header(found[0]) != _region_header(region):
                found = None
            old_draws = {} if found is None else {
                draw_record.key: (draw, draw_record)
                for draw, draw_record in zip(found[0].draws, found[1].draws)
            }
            draws = []
            for draw, planned_draw in zip(region.draws, planned.draws):
                old = old_draws.get(planned_draw.key)
                if old is not None:
                    old_draw, old_record = old
                    if old_draw is draw or old_draw == draw:
                        draws.append(old_record)
                        continue
                    if _revision_only(old_draw, draw):
                        draws.append(replace(
                            old_record,
                            entries=_with_revision(
                                old_record.entries, draw.content.content_revision
                            ),
                        ))
                        continue
                entries, popup_entries = painted.get(
                    (planned.key, planned_draw.key), ((), ())
                )
                draws.append(replace(
                    planned_draw, entries=entries, popup_entries=popup_entries
                ))
            regions.append(replace(planned, draws=tuple(draws)))
    hit_entries = tuple(entry for region in regions for entry in region.entries())
    return ComposedFrame(
        surface, hit_entries, tuple(regions),
        tuple(PixelRect(*rect) for rect in damage), grid, cursor,
        retained_plane, hovered, pressed, geometry, font, control_font,
        glyph_cache, resource_surfaces,
    )


def draw_flip_and_present(
    pygame_module,
    client,
    draw_frame,
    *,
    offer: TerminalDisplayOffer | None,
    generation: int,
    active: bool = True,
) -> dict | None:
    """Cross the synchronous SDL reference-sink boundary, then attest it.

    A successful ``pygame.display.flip()`` is the documented completion
    boundary for this software reference sink.  It is not evidence of e-paper
    controller completion or panel settling.
    """

    if not isinstance(active, bool):
        raise TypeError("active must be bool")
    if not active:
        return None
    if offer is not None and not isinstance(offer, TerminalDisplayOffer):
        raise TypeError("offer must be TerminalDisplayOffer or None")
    draw_frame()
    pygame_module.display.flip()
    if offer is None:
        return None
    return client.request(
        "present",
        generation=_nonnegative_wire_integer(generation, "offer generation"),
        display_offer_id=offer.offer_id,
        display_scope=display_scope_to_wire(offer.scope),
    )


class _RedrawGate:
    """Decide when the viewer must compose, and when it must flip at all.

    A pending display offer always composes, because only a physical flip may
    acknowledge it.  Otherwise the terminal frame is recomposed only when an
    input it is drawn from changes or the window system asks for it, and the
    window is flipped only when that frame or the status line changes.  An
    unchanged window is neither recomposed nor flipped.
    """

    def __init__(self) -> None:
        self._plane = None
        self._frame_key = None
        self._status_key = None
        self._forced = True

    def force(self) -> None:
        self._forced = True

    def decide(
        self, plane, frame_key, status_key, *, pending_offer: bool
    ) -> tuple[bool, bool]:
        """Return (compose, flip) for this loop iteration.

        PLANE is compared by identity; holding it keeps a replacement plane
        from ever passing for the old one.
        """

        compose = (
            pending_offer
            or self._forced
            or plane is not self._plane
            or frame_key != self._frame_key
        )
        flip = compose or status_key != self._status_key
        self._forced = False
        self._plane = plane
        self._frame_key = frame_key
        self._status_key = status_key
        return compose, flip


# Window-system events after which SDL may have discarded window contents.
_WINDOW_REPAINT_EVENTS = (
    "VIDEOEXPOSE",
    "VIDEORESIZE",
    "WINDOWEXPOSED",
    "WINDOWSHOWN",
    "WINDOWRESTORED",
    "WINDOWMAXIMIZED",
    "WINDOWRESIZED",
    "WINDOWSIZECHANGED",
)


def _status_line(status: dict, keyboard, display_holder: bool):
    """The viewer status bar's (text, colour)."""

    if (
        status["state"] in ("lost", "terminal_failed", "error")
        or keyboard.last_error is not None
    ):
        state_color = (245, 95, 95)
    elif status["state"] in ("running", "idle"):
        state_color = (100, 220, 140)
    else:
        state_color = (245, 190, 80)
    status_text = (
        f"{status['state'].upper()}  steps {status['steps']:,}  "
        f"rev {status['revision']}  "
        f"clients {status.get('clients', 0)}"
    )
    if not display_holder:
        status_text += "  VIEW ONLY"
    if keyboard.last_error is not None:
        status_text += f"  {keyboard.last_error}"
    return status_text, state_color


def main() -> int:
    parser = argparse.ArgumentParser(description="Watch a shared MegaPad session")
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--font", type=Path)
    parser.add_argument("--font-size", type=int, default=18)
    parser.add_argument(
        "--fallback-font",
        type=Path,
        action="append",
        help=(
            "a font for characters the primary font lacks, tried in order; "
            "repeat for more (default: the Noto faces fontconfig finds)"
        ),
    )
    parser.add_argument("--fps", type=int, default=30)
    parser.add_argument("--title", default="MegaPad-64 Shared Session")
    parser.add_argument(
        "--input-queue-events",
        type=int,
        default=DEFAULT_PENDING_INPUT_EVENTS,
        help="maximum viewer input events retained during guest backpressure",
    )
    parser.add_argument("--exit-after", type=float, help=argparse.SUPPRESS)
    args = parser.parse_args()
    if args.input_queue_events <= 0:
        parser.error("--input-queue-events must be positive")

    try:
        import pygame
    except ImportError:
        print("session viewer requires pygame", file=sys.stderr)
        return 2

    client = SessionClient(args.socket, timeout=2.0)
    pygame_initialized = False
    text_input_started = False
    try:
        client.connect()
        claim = client.request("claim_display")
        display_holder = _display_claimed(claim)
        status = client.request("status", detailed=False)
        generation = _nonnegative_wire_integer(
            status["generation"], "status generation"
        )
        display_required = _status_display_required(status)
        terminal = VirtualTerminal(cols=80, rows=30)
        revision = -1
        display_state = _RetainedDisplayState()
        resource_cache = _DisplayResourceCache()
        guest_keyboard = _GuestKeyboardForwarder(
            pygame,
            client,
            generation=generation,
            max_pending_events=args.input_queue_events,
            input_enabled=display_holder,
            display_required=display_required,
        )
        first = client.request("screen", since=-1, since_offer=0)
        revision, _ = _accept_screen_update(
            first,
            display_holder=display_holder,
            terminal=terminal,
            keyboard=guest_keyboard,
            display_state=display_state,
            revision=revision,
            resource_cache=resource_cache,
        )

        # The machine-owner process may hold the optional audio mixer.  This
        # viewer only needs video, font, and input, so do not claim an audio
        # device merely as a side effect of pygame.init().
        pygame.display.init()
        pygame_initialized = True
        pygame.font.init()
        _configure_keyboard(pygame)
        text_input_started = True
        fallbacks = (
            tuple(args.fallback_font)
            if args.fallback_font
            else discover_fallback_fonts()
        )
        font = FontSet(pygame, args.font, args.font_size, fallbacks)
        status_font = FontSet(
            pygame, None, max(12, args.font_size - 4), fallbacks, cells=False
        )
        cell_w = max(1, font.size("M")[0])
        cell_h = font.get_linesize()
        status_h = max(24, status_font.get_linesize() + 8)
        screen = pygame.display.set_mode(
            (terminal.cols * cell_w, terminal.rows * cell_h + status_h)
        )
        pygame.display.set_caption(args.title)
        clock = pygame.time.Clock()
    except Exception as exc:
        client.close()
        if text_input_started:
            try:
                pygame.key.stop_text_input()
            except Exception:
                pass
        if pygame_initialized:
            try:
                pygame.quit()
            except Exception:
                pass
        print(f"cannot initialize shared viewer: {exc}", file=sys.stderr)
        return 2

    glyph_cache = {}
    running = True
    pointer = _PointerRouter(
        display_state,
        guest_keyboard,
        cell_width=cell_w,
        cell_height=cell_h,
    )

    def interaction_context():
        pending = display_state.pending_offer
        pending_token = None if pending is None else (pending.offer_id, pending.scope)
        return (
            guest_keyboard.generation,
            pending_token,
            display_state.hit_map_token,
            guest_keyboard.display_ack,
        )

    def accept_screen_update(update: dict) -> bool:
        nonlocal revision
        prior_context = interaction_context()
        revision, resized = _accept_screen_update(
            update,
            display_holder=display_holder,
            terminal=terminal,
            keyboard=guest_keyboard,
            display_state=display_state,
            revision=revision,
            resource_cache=resource_cache,
        )
        if resized:
            pointer.cancel()
        elif interaction_context() != prior_context:
            pointer.clear()
        return resized

    def make_window():
        return pygame.display.set_mode(
            (terminal.cols * cell_w, terminal.rows * cell_h + status_h)
        )

    last_poll = 0.0
    last_status = 0.0
    connected = True
    screen_refresh_required = False

    def accept_status(latest: dict) -> None:
        nonlocal status
        nonlocal revision
        nonlocal screen_refresh_required

        prior_context = interaction_context()
        revision, refresh_required = _accept_status_update(
            latest,
            keyboard=guest_keyboard,
            display_state=display_state,
            revision=revision,
            resource_cache=resource_cache,
        )
        screen_refresh_required = (
            screen_refresh_required or refresh_required
        )
        if interaction_context() != prior_context:
            pointer.clear()
        status = latest

    def request_control(method: str, **params):
        if not display_holder and method in {
            "pause",
            "resume",
            "step",
            "reset",
        }:
            guest_keyboard.report_error(
                "viewer is view-only; display lease is held elsewhere"
            )
            return None
        try:
            return client.request(method, **params)
        except RuntimeError as exc:
            guest_keyboard.report_error(f"{method} rejected: {exc}")
            return None

    keys_down: set[int] = set()
    viewer_started = time.monotonic()
    last_claim_attempt = viewer_started
    redraw = _RedrawGate()
    repaint_events = {
        getattr(pygame, name) for name in _WINDOW_REPAINT_EVENTS
        if hasattr(pygame, name)
    }
    composed_frame = None

    try:
        while running:
            if args.exit_after and time.monotonic() - viewer_started >= args.exit_after:
                break
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.TEXTINPUT:
                    guest_keyboard.text_input(event)
                elif event.type == pygame.KEYDOWN:
                    mods = _pygame_event_mods(pygame, event)
                    ctrl = bool(mods & pygame.KMOD_CTRL)
                    repeated = event.key in keys_down
                    keys_down.add(event.key)
                    if ctrl and event.key == pygame.K_q and not repeated:
                        running = False
                    elif ctrl and event.key == pygame.K_F5 and not repeated:
                        latest = request_control("status", detailed=False)
                        if latest is not None:
                            status = latest
                            if status["state"] not in ("lost", "terminal_failed"):
                                method = "resume" if status["paused"] else "pause"
                                updated = request_control(method)
                                if updated is not None:
                                    accept_status(updated)
                    elif ctrl and event.key == pygame.K_F10 and not repeated:
                        latest = request_control("status", detailed=False)
                        if latest is not None:
                            status = latest
                            if status["state"] not in ("lost", "terminal_failed"):
                                paused = request_control("pause")
                                if paused is not None:
                                    accept_status(paused)
                                    stepped = request_control("step", count=1)
                                    if stepped is not None:
                                        accept_status(stepped["status"])
                    elif ctrl and event.key == pygame.K_r and not repeated:
                        reset = request_control("reset", paused=False)
                        if reset is not None:
                            accept_status(reset)
                    elif not (
                        ctrl
                        and event.key
                        in (pygame.K_q, pygame.K_F5, pygame.K_F10, pygame.K_r)
                    ):
                        guest_keyboard.key_down(event, repeated=repeated)
                elif event.type == pygame.KEYUP:
                    keys_down.discard(event.key)
                    guest_keyboard.key_up(event)
                elif event.type == getattr(pygame, "MOUSEMOTION", -1):
                    pointer.move(
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                        modifiers=_pygame_apt_modifiers(pygame, event),
                    )
                elif event.type == getattr(pygame, "MOUSEBUTTONDOWN", -1):
                    pointer.button_down(
                        event.button,
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                        modifiers=_pygame_apt_modifiers(pygame, event),
                    )
                elif event.type == getattr(pygame, "MOUSEBUTTONUP", -1):
                    pointer.button_up(
                        event.button,
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                        modifiers=_pygame_apt_modifiers(pygame, event),
                    )
                elif event.type == getattr(pygame, "MOUSEWHEEL", -1):
                    flip = -1 if getattr(event, "flipped", False) else 1
                    pointer.wheel(
                        flip * event.x,
                        flip * event.y,
                        pygame.mouse.get_pos(),
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                        modifiers=_pygame_apt_modifiers(pygame, event),
                    )
                elif event.type in {
                    getattr(pygame, "WINDOWFOCUSLOST", -1),
                    getattr(pygame, "WINDOWFOCUSGAINED", -2),
                }:
                    pointer.cancel()
                    if event.type == getattr(pygame, "WINDOWFOCUSLOST", -1):
                        keys_down.clear()
                        guest_keyboard.reset()
                elif event.type in repaint_events:
                    redraw.force()

            if not running:
                break
            guest_keyboard.flush_pending()
            pointer.flush()

            now = time.monotonic()
            if (
                not display_holder
                and now - last_claim_attempt >= DISPLAY_CLAIM_RETRY_SECONDS
            ):
                prior_holder = display_holder
                display_holder, revision, refresh_required = (
                    _retry_display_claim(
                        client,
                        keyboard=guest_keyboard,
                        display_state=display_state,
                        revision=revision,
                        resource_cache=resource_cache,
                    )
                )
                last_claim_attempt = now
                if display_holder != prior_holder:
                    pointer.cancel()
                if display_holder:
                    screen_refresh_required = (
                        screen_refresh_required or refresh_required
                    )
            if now - last_status >= 0.25:
                accept_status(client.request("status", detailed=False))
                last_status = now
            if (
                screen_refresh_required
                or now - last_poll >= 1.0 / max(1, args.fps)
            ):
                update = client.request(
                    "screen",
                    since=revision,
                    since_offer=display_state.poll_offer_cursor,
                    base_offer=display_state.base_offer_id,
                )
                if accept_screen_update(update):
                    screen = make_window()
                    redraw.force()
                screen_refresh_required = False
                last_poll = now

            cursor_blink = int(now * 2) % 2 == 0
            frame_offer = display_state.pending_offer
            frame_generation = (
                guest_keyboard.generation
                if display_state.pending_generation is None
                else display_state.pending_generation
            )
            frame_plane = display_state.frame_plane
            rendered_hit_entries: tuple[HitMapEntry, ...] | None = None
            if frame_offer is not None:
                if not resource_cache.pending_ready(
                    frame_offer,
                    frame_generation,
                ):
                    fetch_status = resource_cache.fetch_pending_chunk(
                        client,
                        pygame,
                        frame_offer,
                        frame_generation,
                    )
                    if fetch_status in {"stale_generation", "stale_display"}:
                        display_state.reset()
                        resource_cache.clear()
                        revision = -1
                        screen_refresh_required = True
                        pointer.cancel()
                        guest_keyboard.clear_display_context(
                            waiting=guest_keyboard.display_required
                        )
                        guest_keyboard.report_error(
                            f"display resource fetch rejected ({fetch_status})"
                        )
                        clock.tick()
                        continue
                    if fetch_status == "invalid_resource":
                        raise RuntimeError(
                            "display offer references an unavailable exact resource"
                        )
                    if not resource_cache.pending_ready(
                        frame_offer,
                        frame_generation,
                    ):
                        # Pump events again immediately after this server-bounded
                        # chunk.  The pending frame has not touched the sink.
                        clock.tick()
                        continue
                if not display_state.pending_resources_ready:
                    display_state.stage_resources_ready(frame_offer)
                resource_surfaces = resource_cache.pending_surfaces(
                    frame_offer,
                    frame_generation,
                )
            else:
                resource_surfaces = resource_cache.acknowledged_surfaces

            status_text, state_color = _status_line(
                status, guest_keyboard, display_holder
            )
            with terminal._lock:
                cursor_state = (
                    cursor_blink,
                    terminal.cx,
                    terminal.cy,
                ) if terminal.cursor_visible else None
            compose, flip = redraw.decide(
                frame_plane,
                (
                    revision,
                    pointer.hovered,
                    pointer.pressed,
                    cursor_state,
                    frozenset(resource_surfaces.items()),
                    screen.get_size(),
                ),
                (status_text, state_color),
                pending_offer=frame_offer is not None,
            )
            if composed_frame is None:
                compose = flip = True

            def draw_frame() -> None:
                nonlocal rendered_hit_entries, composed_frame
                screen.fill((0, 0, 0))
                if compose:
                    composed_frame = compose_terminal_frame_result(
                        pygame,
                        terminal,
                        font,
                        cell_w,
                        cell_h,
                        retained_plane=frame_plane,
                        show_cursor=cursor_blink,
                        glyph_cache=glyph_cache,
                        control_font=status_font,
                        hovered=pointer.hovered,
                        pressed=pointer.pressed,
                        resource_surfaces=resource_surfaces,
                    )
                    rendered_hit_entries = composed_frame.hit_entries
                screen.blit(composed_frame.surface, (0, 0))
                y = terminal.rows * cell_h
                pygame.draw.rect(
                    screen,
                    (28, 30, 34),
                    (0, y, screen.get_width(), status_h),
                )
                label = status_font.render(status_text, True, state_color)
                screen.blit(label, (8, y + (status_h - label.get_height()) // 2))
                if frame_offer is not None:
                    display_state.stage_frame_hit_map(
                        frame_offer,
                        rendered_hit_entries,
                    )
            presentation = draw_flip_and_present(
                pygame,
                client,
                draw_frame,
                offer=frame_offer,
                generation=frame_generation,
                active=running and flip,
            )
            if frame_offer is not None:
                accepted_revision = display_state.finish_presentation(presentation)
                if accepted_revision is not None:
                    resource_cache.promote(frame_offer, frame_generation)
                    revision = accepted_revision
                    if isinstance(status, dict):
                        status["revision"] = revision
                    guest_keyboard.acknowledge_display_offer(
                        frame_offer.offer_id,
                        frame_offer.scope,
                    )
                else:
                    resource_cache.clear()
                    revision = -1
                    screen_refresh_required = True
                    pointer.cancel()
                    guest_keyboard.clear_display_context(
                        waiting=guest_keyboard.display_required
                    )
                    guest_keyboard.report_error(
                        "display offer rejected "
                        f"({presentation.get('status')})"
                    )
            clock.tick(max(1, args.fps))
    except (OSError, ConnectionError, RuntimeError, TypeError, ValueError) as exc:
        connected = False
        print(f"shared viewer disconnected: {exc}", file=sys.stderr)
    finally:
        try:
            resource_cache.clear()
            client.close()
        finally:
            try:
                pygame.key.stop_text_input()
            finally:
                pygame.quit()
    return 0 if connected else 2


def _pygame_key_name(pygame, key: int) -> str | None:
    mapping = {
        pygame.K_RETURN: "enter",
        pygame.K_ESCAPE: "escape",
        pygame.K_TAB: "tab",
        pygame.K_BACKSPACE: "backspace",
        pygame.K_DELETE: "delete",
        pygame.K_UP: "up",
        pygame.K_DOWN: "down",
        pygame.K_LEFT: "left",
        pygame.K_RIGHT: "right",
        pygame.K_HOME: "home",
        pygame.K_END: "end",
        pygame.K_PAGEUP: "pageup",
        pygame.K_PAGEDOWN: "pagedown",
        pygame.K_INSERT: "insert",
        pygame.K_F1: "f1",
        pygame.K_F2: "f2",
        pygame.K_F3: "f3",
        pygame.K_F4: "f4",
        pygame.K_F5: "f5",
        pygame.K_F6: "f6",
        pygame.K_F7: "f7",
        pygame.K_F8: "f8",
        pygame.K_F9: "f9",
        pygame.K_F10: "f10",
        pygame.K_F11: "f11",
        pygame.K_F12: "f12",
    }
    return mapping.get(key)


def _configure_keyboard(pygame) -> None:
    pygame.key.start_text_input()
    pygame.key.set_repeat(KEY_REPEAT_DELAY_MS, KEY_REPEAT_INTERVAL_MS)


def _pygame_event_mods(pygame, event) -> int:
    mods = getattr(event, "mod", None)
    return pygame.key.get_mods() if mods is None else mods


def _pygame_apt_modifiers(pygame, event) -> int:
    """Map host masks to APT Shift/Ctrl/Alt/Super/Caps/Num bits 0..5."""

    host_modifiers = _pygame_event_mods(pygame, event)
    normalized = 0
    for host_name, apt_bit in (
        ("KMOD_SHIFT", 0),
        ("KMOD_CTRL", 1),
        ("KMOD_ALT", 2),
        ("KMOD_GUI", 3),
        ("KMOD_CAPS", 4),
        ("KMOD_NUM", 5),
    ):
        if host_modifiers & getattr(pygame, host_name, 0):
            normalized |= 1 << apt_bit
    return normalized


def _pygame_character_name(pygame, event) -> str | None:
    if pygame.K_a <= event.key <= pygame.K_z:
        return chr(ord("a") + event.key - pygame.K_a)
    if pygame.K_0 <= event.key <= pygame.K_9:
        return chr(ord("0") + event.key - pygame.K_0)
    if event.key == pygame.K_SPACE:
        return "space"
    text = getattr(event, "unicode", "")
    if len(text) == 1 and text.isascii() and text.isprintable() and text != "+":
        return text
    return None


def _pygame_modifier_names(pygame, event) -> list[str]:
    mods = _pygame_event_mods(pygame, event)
    if mods & getattr(pygame, "KMOD_MODE", 0):
        return []
    names = []
    if mods & pygame.KMOD_CTRL:
        names.append("ctrl")
    if mods & pygame.KMOD_ALT:
        names.append("alt")
    if mods & pygame.KMOD_SHIFT:
        names.append("shift")
    return names


def _pygame_modified_character(pygame, event) -> str | None:
    modifiers = _pygame_modifier_names(pygame, event)
    if "ctrl" not in modifiers and "alt" not in modifiers:
        return None
    return _pygame_character_name(pygame, event)


def _pygame_guest_key(pygame, event) -> str | None:
    modifiers = _pygame_modifier_names(pygame, event)
    named = _pygame_key_name(pygame, event.key)
    if named is not None:
        if modifiers and named in {
            "up",
            "down",
            "left",
            "right",
            "home",
            "end",
            "insert",
            "delete",
            "pageup",
            "pagedown",
            "f5",
            "f6",
            "f7",
            "f8",
            "f9",
            "f10",
            "f11",
            "f12",
        }:
            return "+".join((*modifiers, named))
        return named

    character = _pygame_modified_character(pygame, event)
    if character is None:
        return None
    return "+".join((*modifiers, character))


def _pygame_repeatable_guest_key(pygame, event) -> bool:
    """Limit host key repeat to editing and navigation operations."""

    return _pygame_key_name(pygame, event.key) in {
        "backspace",
        "delete",
        "up",
        "down",
        "left",
        "right",
        "home",
        "end",
        "pageup",
        "pagedown",
    }


if __name__ == "__main__":
    raise SystemExit(main())
