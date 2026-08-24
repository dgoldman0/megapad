"""Pure canonical projection for APT-1 RETAINED-1 conformance state.

This module contains no wire layouts, message decoders, transition rules, or
fixture definitions.  The independent semantic oracle and the wire reducer
share only this deterministic JSON projection and hashing convention.
"""

from __future__ import annotations

import copy
import hashlib
import json
from typing import Any


def _digest(value: Any) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha3_256(encoded).hexdigest()


def owner_key(owner_id: int, generation: int) -> str:
    return f"{owner_id:016x}:{generation}"


def resource_key(owner_id: int, generation: int, resource_id: int) -> str:
    return f"{owner_key(owner_id, generation)}:{resource_id}"


def _sorted_records(records: dict[str, dict[str, Any]]) -> list[dict[str, Any]]:
    def order(record: dict[str, Any]) -> tuple[int, int, int]:
        item_id = next(
            (int(record[field]) for field in ("region_id", "object_id", "series_id", "resource_id") if field in record),
            0,
        )
        return int(record["owner_id"]), int(record["generation"]), item_id

    return [copy.deepcopy(record) for record in sorted(records.values(), key=order)]


def _scene_projection(scene: dict[str, dict[str, Any]]) -> dict[str, Any]:
    return {
        "regions": _sorted_records(scene["regions"]),
        "objects": _sorted_records(scene["objects"]),
        "series": _sorted_records(scene["series"]),
    }


def canonical_state(state: dict[str, Any]) -> dict[str, Any]:
    """Return the compact exact state record used by checked-in sidecars."""

    cell = copy.deepcopy(state["cell"])
    cell["digest_sha3_256"] = _digest(
        {
            "cols": cell["cols"],
            "rows": cell["rows"],
            "cursor": cell["cursor"],
            "default": cell["default"],
            "overrides": cell["overrides"],
        }
    )
    active = _scene_projection(state["active"])
    hidden_state = state["hidden"]
    hidden = None
    if hidden_state is not None:
        hidden = {
            "mode": hidden_state["mode"],
            "scene": _scene_projection(hidden_state["scene"]),
        }
    reservations = _sorted_records(state["owners"])
    return {
        "session_state": state["session_state"],
        "presentation_epoch": state["presentation_epoch"],
        "global_revision": state["global_revision"],
        "transaction_id_highwater": state["transaction_id_highwater"],
        "directional_sequences": copy.deepcopy(state["directional_sequences"]),
        "credit_ledgers": copy.deepcopy(state["credit_ledgers"]),
        "control_reserve": copy.deepcopy(state["control_reserve"]),
        "selected_geometry": copy.deepcopy(state["selected_geometry"]),
        "cell": cell,
        "active_digest_sha3_256": _digest(active),
        "active": active,
        "active_usage": copy.deepcopy(state["active_usage"]),
        "hidden_digest_sha3_256": _digest(hidden),
        "hidden": hidden,
        "hidden_usage": copy.deepcopy(state["hidden_usage"]),
        "retained_visible": state["retained_visible"],
        "rebuild_required": state["rebuild_required"],
        "owner_ledger": reservations,
        "owner_reservation_totals": copy.deepcopy(state["owner_reservation_totals"]),
        "resources": _sorted_records(state["resources"]),
        "owner_wide_resource_usage": copy.deepcopy(state["owner_wide_resource_usage"]),
        "upload": copy.deepcopy(state["upload"]),
        "open_transaction": copy.deepcopy(state["open_transaction"]),
        "open_lifecycle_request": copy.deepcopy(state["open_lifecycle_request"]),
        "pending_reset": copy.deepcopy(state["pending_reset"]),
        "last_result": copy.deepcopy(state["last_result"]),
    }


def deduplicate_records(
    *,
    contract_id: str,
    transcript: str,
    initial_state: dict[str, Any],
    records: list[dict[str, Any]],
) -> dict[str, Any]:
    states: list[dict[str, Any]] = []
    indices: dict[str, int] = {}
    frame_records: list[dict[str, Any]] = []
    for record in records:
        state = record["state"]
        encoded = json.dumps(state, sort_keys=True, separators=(",", ":"))
        index = indices.get(encoded)
        if index is None:
            index = len(states)
            indices[encoded] = index
            states.append(state)
        frame_records.append({key: value for key, value in record.items() if key != "state"} | {"state_index": index})
    return {
        "contract_id": contract_id,
        "transcript": transcript,
        "initial_state": canonical_state(initial_state),
        "states": states,
        "frames": frame_records,
    }
