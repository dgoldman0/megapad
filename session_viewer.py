#!/usr/bin/env python3
"""Pygame viewer/input client for a shared MegaPad session."""

from __future__ import annotations

import argparse
import operator
import sys
import time
from collections import deque
from collections.abc import Mapping
from pathlib import Path

from display import VirtualTerminal
from rich_terminal.pygame_view import (
    CompositeDrawResult,
    ControlHitTarget,
    ControlIdentity,
    composite_draw_plane,
    composite_draw_plane_result,
)
from rich_terminal.retained_view import DisplayScope, RetainedDrawPlane
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


def _accepted_presentation_revision(response) -> int | None:
    """Return the CELL cursor only for one accepted physical presentation."""

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
    """Keep offer delivery separate from acknowledged physical display state."""

    def __init__(self) -> None:
        self.since_offer = 0
        self.pending_offer: TerminalDisplayOffer | None = None
        self.pending_generation: int | None = None
        self.retained_plane: RetainedDrawPlane | None = None
        self._pending_hit_token: tuple[int, DisplayScope] | None = None
        self._pending_hit_targets: tuple[ControlHitTarget, ...] = ()
        self._pending_hit_map_rendered = False
        self._hit_map_token: tuple[int, DisplayScope] | None = None
        self._hit_targets: tuple[ControlHitTarget, ...] = ()

    @property
    def frame_plane(self) -> RetainedDrawPlane | None:
        if self.pending_offer is not None:
            return self.pending_offer.retained
        return self.retained_plane

    @property
    def hit_map_token(self) -> tuple[int, DisplayScope] | None:
        """Exact physically acknowledged offer/scope owning the hit map."""

        return self._hit_map_token

    @property
    def hit_targets(self) -> tuple[ControlHitTarget, ...]:
        """Only the immutable map promoted by accepted physical presentation."""

        return self._hit_targets

    @staticmethod
    def _offer_token(offer: TerminalDisplayOffer) -> tuple[int, DisplayScope]:
        return offer.offer_id, offer.scope

    @staticmethod
    def _validated_hits(hit_targets) -> tuple[ControlHitTarget, ...]:
        targets = tuple(hit_targets)
        if any(not isinstance(target, ControlHitTarget) for target in targets):
            raise TypeError("hit_targets must contain only ControlHitTarget values")
        return targets

    def _clear_hit_maps(self) -> None:
        self._pending_hit_token = None
        self._pending_hit_targets = ()
        self._pending_hit_map_rendered = False
        self._hit_map_token = None
        self._hit_targets = ()

    def reset(self) -> None:
        """Drop visual candidates while preserving the last physical ACK cursor."""

        self.pending_offer = None
        self.pending_generation = None
        self.retained_plane = None
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
        # input authority before the candidate is drawn or physically ACKed.
        self._hit_map_token = None
        self._hit_targets = ()
        self._pending_hit_token = self._offer_token(offer)
        self._pending_hit_targets = ()
        self._pending_hit_map_rendered = False

    def stage_frame_hit_map(
        self,
        offer: TerminalDisplayOffer,
        hit_targets,
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
        self._pending_hit_targets = self._validated_hits(hit_targets)
        self._pending_hit_map_rendered = True

    def hit_test(
        self,
        x: int,
        y: int,
        *,
        display_token: tuple[int, DisplayScope] | None,
    ) -> ControlHitTarget | None:
        """Hit-test only when the input proof and physical map token agree."""

        if display_token is None or display_token != self._hit_map_token:
            return None
        for target in reversed(self._hit_targets):
            if target.rect.contains(x, y):
                return target
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
            self._pending_hit_token != token
            or not self._pending_hit_map_rendered
        ):
            self.reset()
            raise RuntimeError(
                "presented hit map was not rendered for its exact offer"
            )
        self.since_offer = offer.offer_id
        self.retained_plane = offer.retained
        self._hit_map_token = token
        self._hit_targets = self._pending_hit_targets
        self.pending_offer = None
        self.pending_generation = None
        self._pending_hit_token = None
        self._pending_hit_targets = ()
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
        self._display_transition = required
        self.last_error = None

    def begin_display_offer(self) -> None:
        """Invalidate old queued input while one newer frame awaits physical ACK."""

        self._pending_inputs.clear()
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
            self._pending_inputs.clear()
        self._display_ack = token
        self._display_transition = False
        self.last_error = None

    def clear_display_context(self, *, waiting: bool) -> None:
        if not isinstance(waiting, bool):
            raise TypeError("waiting must be bool")
        self._pending_inputs.clear()
        self._display_ack = None
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
        if status == "stale_display":
            self._display_ack = None
            self._display_transition = self.display_required
        elif status in {"stale", "failed", "stale_generation"}:
            self._display_ack = None
            self._display_transition = False
        self.last_error = (
            f"input rejected ({method}: {status or 'missing status'})"
        )

    def _request_input(self, method: str, **params) -> None:
        if not self.input_enabled:
            self._pending_inputs.clear()
            self.last_error = "viewer is view-only; display lease is held elsewhere"
            return
        if self._display_transition or (
            self.display_required and self._display_ack is None
        ):
            self._pending_inputs.clear()
            self.last_error = "input waiting for current display acknowledgement"
            return
        params["generation"] = self.generation
        self._bind_display_proof(params)
        if self._pending_inputs:
            self._enqueue_input(method, params)
            return
        result = self.client.request(method, **params)
        status = result.get("status")
        if status == "progress":
            return
        if status == "backpressured":
            self._enqueue_input(method, params)
            return
        self._record_rejection(method, status)

    def flush_pending(self) -> None:
        while self._pending_inputs:
            method, params = self._pending_inputs[0]
            result = self.client.request(method, **params)
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


class _SemanticPointerInteractor:
    """Resolve clicks exclusively through one physically ACKed semantic map."""

    def __init__(
        self,
        display_state: _RetainedDisplayState,
        keyboard: _GuestKeyboardForwarder,
    ) -> None:
        if not isinstance(display_state, _RetainedDisplayState):
            raise TypeError("display_state must be _RetainedDisplayState")
        if not isinstance(keyboard, _GuestKeyboardForwarder):
            raise TypeError("keyboard must be _GuestKeyboardForwarder")
        self.display_state = display_state
        self.keyboard = keyboard
        self._observed_token: tuple[int, DisplayScope] | None = None
        self._hovered: ControlIdentity | None = None
        self._pressed_target: ControlHitTarget | None = None
        self._pressed_token: tuple[int, DisplayScope] | None = None

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

    def clear(self) -> None:
        """Drop renderer-local focus state without altering guest semantics."""

        self._hovered = None
        self._pressed_target = None
        self._pressed_token = None
        self._observed_token = self._authority_token()

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

    def _target_at(self, position, terminal_size) -> ControlHitTarget | None:
        token = self._synchronize()
        x, y, width, height = self._point_and_extent(position, terminal_size)
        if x < 0 or y < 0 or x >= width or y >= height:
            return None
        return self.display_state.hit_test(
            x,
            y,
            display_token=token,
        )

    def move(self, position, terminal_size) -> ControlHitTarget | None:
        target = self._target_at(position, terminal_size)
        self._hovered = None if target is None else target.identity
        return target

    def left_down(self, position, terminal_size) -> bool:
        target = self._target_at(position, terminal_size)
        self._hovered = None if target is None else target.identity
        self._pressed_target = target
        self._pressed_token = self._authority_token() if target is not None else None
        return target is not None

    def left_up(
        self,
        position,
        terminal_size,
        *,
        modifiers: int,
    ) -> bool:
        target = self._target_at(position, terminal_size)
        pressed = self._pressed_target
        pressed_token = self._pressed_token
        current_token = self._authority_token()
        self._pressed_target = None
        self._pressed_token = None
        self._hovered = None if target is None else target.identity
        if (
            target is None
            or pressed is None
            or target != pressed
            or pressed_token != current_token
        ):
            return False
        self.keyboard.activate_control(target, modifiers=modifiers)
        return True


def _retry_display_claim(
    client,
    *,
    keyboard: _GuestKeyboardForwarder,
    display_state: _RetainedDisplayState,
    revision: int,
) -> tuple[bool, int, bool]:
    """Retry one observer lease claim and invalidate state on exact takeover."""

    keyboard.set_input_enabled(False)
    claimed = _display_claimed(client.request("claim_display"))
    if not claimed:
        return False, revision, False
    display_state.reset()
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
    if revision < 0 and not has_payload:
        raise RuntimeError("screen refresh returned no CELL or display offer")
    if "snapshot" in update:
        apply_snapshot(terminal, update["snapshot"])
        revision = response_revision
    if "display_offer" in update:
        if not display_holder:
            raise RuntimeError("nonholder received a retained display offer")
        offer = display_offer_from_wire(update["display_offer"])
        display_state.stage(offer, update["generation"])
        apply_terminal_snapshot(terminal, offer.cell)
        keyboard.begin_display_offer()
    elif "snapshot" in update:
        display_state.reset()
        keyboard.clear_display_context(waiting=keyboard.display_required)
    return revision, old_size != (terminal.cols, terminal.rows)


def _accept_status_update(
    latest,
    *,
    keyboard: _GuestKeyboardForwarder,
    display_state: _RetainedDisplayState,
    revision: int,
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
):
    """Render CELL, then retained draws, then the terminal cursor."""

    surface = terminal.render(
        pygame_module,
        font,
        cell_width,
        cell_height,
        show_cursor=False,
        _cache=glyph_cache,
    )
    if retained_plane is not None:
        composite_draw_plane(
            pygame_module,
            surface,
            retained_plane,
            font,
            cell_width,
            cell_height,
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
) -> CompositeDrawResult:
    """Render the complete frame and return hits from that exact paint pass."""

    surface = terminal.render(
        pygame_module,
        font,
        cell_width,
        cell_height,
        show_cursor=False,
        _cache=glyph_cache,
    )
    hit_targets: tuple[ControlHitTarget, ...] = ()
    if retained_plane is not None:
        retained_result = composite_draw_plane_result(
            pygame_module,
            surface,
            retained_plane,
            font,
            cell_width,
            cell_height,
            control_font=control_font,
            hovered=hovered,
            pressed=pressed,
        )
        hit_targets = retained_result.hit_targets
    _paint_terminal_cursor(
        pygame_module,
        surface,
        terminal,
        cell_width,
        cell_height,
        show_cursor=show_cursor,
    )
    return CompositeDrawResult(surface, hit_targets)


def draw_flip_and_present(
    pygame_module,
    client,
    draw_frame,
    *,
    offer: TerminalDisplayOffer | None,
    generation: int,
    active: bool = True,
) -> dict | None:
    """Draw and flip one frame, then ACK only its exact retained offer."""

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


def main() -> int:
    parser = argparse.ArgumentParser(description="Watch a shared MegaPad session")
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--font", type=Path)
    parser.add_argument("--font-size", type=int, default=18)
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
        )

        # The machine-owner process may hold the optional audio mixer.  This
        # viewer only needs video, font, and input, so do not claim an audio
        # device merely as a side effect of pygame.init().
        pygame.display.init()
        pygame_initialized = True
        pygame.font.init()
        _configure_keyboard(pygame)
        text_input_started = True
        font = (
            pygame.font.Font(str(args.font), args.font_size)
            if args.font else pygame.font.SysFont("monospace", args.font_size)
        )
        status_font = pygame.font.SysFont("sans", max(12, args.font_size - 4))
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
    semantic_pointer = _SemanticPointerInteractor(
        display_state,
        guest_keyboard,
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
        )
        if resized or interaction_context() != prior_context:
            semantic_pointer.clear()
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
        )
        screen_refresh_required = (
            screen_refresh_required or refresh_required
        )
        if interaction_context() != prior_context:
            semantic_pointer.clear()
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
                    semantic_pointer.move(
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                    )
                elif (
                    event.type == getattr(pygame, "MOUSEBUTTONDOWN", -1)
                    and event.button == 1
                ):
                    semantic_pointer.left_down(
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                    )
                elif (
                    event.type == getattr(pygame, "MOUSEBUTTONUP", -1)
                    and event.button == 1
                ):
                    semantic_pointer.left_up(
                        event.pos,
                        (terminal.cols * cell_w, terminal.rows * cell_h),
                        modifiers=_pygame_apt_modifiers(pygame, event),
                    )
                elif event.type in {
                    getattr(pygame, "WINDOWFOCUSLOST", -1),
                    getattr(pygame, "WINDOWFOCUSGAINED", -2),
                }:
                    semantic_pointer.clear()
                    if event.type == getattr(pygame, "WINDOWFOCUSLOST", -1):
                        keys_down.clear()
                        guest_keyboard.reset()

            if not running:
                break
            guest_keyboard.flush_pending()

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
                    )
                )
                last_claim_attempt = now
                if display_holder != prior_holder:
                    semantic_pointer.clear()
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
                    since_offer=display_state.since_offer,
                )
                if accept_screen_update(update):
                    screen = make_window()
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
            rendered_hit_targets: tuple[ControlHitTarget, ...] | None = None

            def draw_frame() -> None:
                nonlocal rendered_hit_targets
                screen.fill((0, 0, 0))
                frame_result = compose_terminal_frame_result(
                    pygame,
                    terminal,
                    font,
                    cell_w,
                    cell_h,
                    retained_plane=frame_plane,
                    show_cursor=cursor_blink,
                    glyph_cache=glyph_cache,
                    control_font=status_font,
                    hovered=semantic_pointer.hovered,
                    pressed=semantic_pointer.pressed,
                )
                rendered_hit_targets = frame_result.hit_targets
                screen.blit(frame_result.surface, (0, 0))
                y = terminal.rows * cell_h
                pygame.draw.rect(
                    screen,
                    (28, 30, 34),
                    (0, y, screen.get_width(), status_h),
                )
                if (
                    status["state"] in ("lost", "terminal_failed", "error")
                    or guest_keyboard.last_error is not None
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
                if guest_keyboard.last_error is not None:
                    status_text += f"  {guest_keyboard.last_error}"
                label = status_font.render(status_text, True, state_color)
                screen.blit(label, (8, y + (status_h - label.get_height()) // 2))
                if frame_offer is not None:
                    display_state.stage_frame_hit_map(
                        frame_offer,
                        rendered_hit_targets,
                    )

            presentation = draw_flip_and_present(
                pygame,
                client,
                draw_frame,
                offer=frame_offer,
                generation=frame_generation,
                active=running,
            )
            if frame_offer is not None:
                accepted_revision = display_state.finish_presentation(presentation)
                if accepted_revision is not None:
                    revision = accepted_revision
                    if isinstance(status, dict):
                        status["revision"] = revision
                    guest_keyboard.acknowledge_display_offer(
                        frame_offer.offer_id,
                        frame_offer.scope,
                    )
                else:
                    revision = -1
                    screen_refresh_required = True
                    semantic_pointer.clear()
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
