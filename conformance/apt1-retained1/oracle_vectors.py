#!/usr/bin/env python3
"""Independent high-level state oracle for RETAINED-1 conformance vectors.

This module intentionally does not import ``validate_vectors`` and contains no
APT frame encoder, payload layout, CRC implementation, or wire decoder.  Its
frame paths, initial models, and semantic effects are separately declared.
It is invoked explicitly to rewrite the checked-in ``.meta.json`` and
``.state.json`` oracle assets; normal fixture generation never touches them.
"""

from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path
from typing import Any

from canonical_state import canonical_state, deduplicate_records, owner_key, resource_key


CONTRACT_ID = "APT-1-RETAINED-1-2026-08-24"
OWNER_ID = 0x534F554E444C4142
OWNER_GENERATION = 7
INITIAL_C2T_GRANT = 65_536
INITIAL_T2C_GRANT = 4_096

CLIENT = "client_to_terminal"
TERMINAL = "terminal_to_client"

CONTROL = {
    "CREDIT",
    "SOFT_RESET_REQUEST",
    "SOFT_RESET_ACK",
    "TX_RESULT",
    "RET_RESULT",
    "OWNER_DROP",
    "RESOURCE_ABORT",
}


def _path(
    directions: str,
    messages: str,
    sizes: list[int],
    sequences: list[int],
    epochs: list[int],
) -> list[dict[str, Any]]:
    names = messages.split(",")
    if not (len(directions) == len(names) == len(sizes) == len(sequences) == len(epochs)):
        raise ValueError("independent oracle frame-path columns differ")
    return [
        {
            "direction": CLIENT if direction == "c" else TERMINAL,
            "message": message,
            "complete_bytes": size,
            "sequence": sequence,
            "epoch": epoch,
        }
        for direction, message, size, sequence, epoch in zip(
            directions, names, sizes, sequences, epochs, strict=True
        )
    ]


def _soft_reset_replay_path() -> list[dict[str, Any]]:
    snapshot = [
        ("c", "SNAPSHOT_BEGIN", 72, 55, 1),
        *(("c", "CELL_SPAN", 692, 56 + row, 1) for row in range(25)),
        ("c", "CURSOR", 56, 81, 1),
        ("c", "SNAPSHOT_COMMIT", 48, 82, 1),
    ]
    frames = [
        ("t", "SOFT_RESET_REQUEST", 56, 15, 0),
        ("c", "SOFT_RESET_ACK", 48, 54, 1),
        *snapshot,
        ("t", "TX_RESULT", 60, 16, 1),
        ("t", "CREDIT", 48, 17, 1),
        ("c", "RET_QUERY", 48, 83, 1),
        ("t", "RET_CAPS", 104, 18, 1),
        ("t", "RET_FORMATS", 104, 19, 1),
        ("t", "CREDIT", 48, 20, 1),
        ("c", "OWNER_OPEN", 104, 84, 1),
        ("t", "RET_RESULT", 88, 21, 1),
        ("t", "CREDIT", 48, 22, 1),
        ("c", "RESOURCE_BEGIN", 120, 85, 1),
        ("t", "RET_RESULT", 88, 23, 1),
        ("t", "CREDIT", 48, 24, 1),
        ("c", "RESOURCE_CHUNK", 76, 86, 1),
        ("t", "CREDIT", 48, 25, 1),
        ("c", "RESOURCE_CHUNK", 76, 87, 1),
        ("t", "CREDIT", 48, 26, 1),
        ("c", "RESOURCE_COMMIT", 64, 88, 1),
        ("t", "RET_RESULT", 88, 27, 1),
        ("t", "CREDIT", 48, 28, 1),
        ("c", "PRESENT_BEGIN", 104, 89, 1),
        ("c", "REGION_DEFINE", 88, 90, 1),
        ("c", "PRESENT_COMMIT", 56, 91, 1),
        ("t", "TX_RESULT", 60, 29, 1),
        ("t", "CREDIT", 48, 30, 1),
        ("c", "PRESENT_BEGIN", 104, 92, 1),
        ("c", "OBJECT_DEFINE", 120, 93, 1),
        ("c", "PRESENT_COMMIT", 56, 94, 1),
        ("t", "TX_RESULT", 60, 31, 1),
        ("t", "CREDIT", 48, 32, 1),
    ]
    return [
        {
            "direction": CLIENT if direction == "c" else TERMINAL,
            "message": message,
            "complete_bytes": size,
            "sequence": sequence,
            "epoch": epoch,
        }
        for direction, message, size, sequence, epoch in frames
    ]


FRAME_PATHS = {
    "ret_query_supported": _path(
        "cttt", "RET_QUERY,RET_CAPS,RET_FORMATS,CREDIT",
        [48, 104, 104, 48], [29, 2, 3, 4], [0] * 4,
    ),
    "ret_query_unsupported": _path(
        "ct", "RET_QUERY,CREDIT", [48, 48], [29, 2], [0] * 2,
    ),
    "soundlab_initial_replace": _path(
        "cttccccccccccttcccccccttcccctt",
        "OWNER_OPEN,RET_RESULT,CREDIT,PRESENT_BEGIN,REGION_DEFINE,SERIES_DEFINE,SERIES_REPLACE,SERIES_DEFINE,SERIES_REPLACE,OBJECT_DEFINE,OBJECT_DEFINE,OBJECT_DEFINE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,OBJECT_DEFINE,OBJECT_DEFINE,OBJECT_DEFINE,OBJECT_DEFINE,OBJECT_DEFINE,PRESENT_COMMIT,TX_RESULT,CREDIT,TX_BEGIN,CELL_SPAN,CURSOR,TX_COMMIT,TX_RESULT,CREDIT",
        [104,88,48,104,88,80,104,80,112,104,144,128,56,60,48,104,146,152,136,144,152,56,60,48,72,60,56,48,60,48],
        [30,5,6,31,32,33,34,35,36,37,38,39,40,7,8,41,42,43,44,45,46,47,9,10,48,49,50,51,11,12],
        [0] * 30,
    ),
    "soundlab_dynamic_append": _path(
        "cccccccctt",
        "PRESENT_BEGIN,OBJECT_SET_VALUE,OBJECT_SET_VALUE,OBJECT_SET_VALUE,OBJECT_SET_VISIBILITY,SERIES_APPEND,SERIES_APPEND,PRESENT_COMMIT,TX_RESULT,CREDIT",
        [104,72,72,72,72,96,112,56,60,48], [52,53,54,55,56,57,58,59,13,14], [0] * 10,
    ),
    "mutation_and_drop_lifecycle": _path(
        "cccttccccccccccccctt",
        "PRESENT_BEGIN,OBJECT_REPLACE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,OBJECT_DROP,SERIES_DROP,SERIES_DROP,REGION_DROP,PRESENT_COMMIT,TX_RESULT,CREDIT",
        [104,134,56,60,48,104,64,64,64,64,64,64,64,64,64,64,64,56,60,48],
        [48,49,50,11,12,51,52,53,54,55,56,57,58,59,60,61,62,63,13,14], [0] * 20,
    ),
    "mixed_commit_and_rejections": _path(
        "cccccttcccttcccttcccttcccttccccctt",
        "PRESENT_BEGIN,CELL_SPAN,CURSOR,OBJECT_SET_VALUE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,OBJECT_SET_VALUE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,OBJECT_SET_VALUE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,SERIES_APPEND,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,OBJECT_REPLACE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,CELL_SPAN,CURSOR,OBJECT_REPLACE,PRESENT_COMMIT,TX_RESULT,CREDIT",
        [104,60,56,72,56,60,48,104,72,56,60,48,104,72,56,60,48,104,112,56,60,48,104,144,56,60,48,104,60,56,144,56,60,48],
        [110,111,112,113,114,110,111,115,116,117,112,113,118,119,120,114,115,121,122,123,116,117,124,125,126,118,119,127,128,129,130,131,120,121], [0] * 34,
    ),
    "legacy_cell_and_replace_continue": _path(
        "cttccccttccccttccccctt",
        "OWNER_OPEN,RET_RESULT,CREDIT,TX_BEGIN,CELL_SPAN,CURSOR,TX_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,REGION_DEFINE,SERIES_DEFINE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,SERIES_REPLACE,OBJECT_DEFINE,OBJECT_DEFINE,PRESENT_COMMIT,TX_RESULT,CREDIT",
        [104,88,48,72,60,56,48,60,48,104,88,80,56,60,48,104,96,146,144,56,60,48],
        [150,150,151,151,152,153,154,152,153,155,156,157,158,154,155,159,160,161,162,163,156,157], [0] * 22,
    ),
    "resize_layout_sync": _path(
        "cccccttcccttcctt",
        "PRESENT_BEGIN,CELL_SPAN,CELL_SPAN,CURSOR,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,REGION_REPLACE,PRESENT_COMMIT,TX_RESULT,CREDIT,PRESENT_BEGIN,PRESENT_COMMIT,TX_RESULT,CREDIT",
        [104,76,76,56,56,60,48,104,88,56,60,48,104,56,60,48],
        [30,31,32,33,34,30,31,35,36,37,32,33,38,39,34,35], [0] * 16,
    ),
    "reset_crossed_present_commit": _path(
        "cctctc", "PRESENT_BEGIN,OBJECT_SET_VALUE,SOFT_RESET_REQUEST,PRESENT_COMMIT,TX_RESULT,SOFT_RESET_ACK",
        [104,72,56,56,60,48], [170,171,170,172,171,173], [0,0,0,0,0,1],
    ),
    "reset_crossed_owner_drop": _path(
        "tctc", "SOFT_RESET_REQUEST,OWNER_DROP,TX_RESULT,SOFT_RESET_ACK",
        [56,72,60,48], [180,180,181,181], [0,0,0,1],
    ),
    "reset_wrong_tuple_upload_abort": _path(
        "ctttcttctc",
        "RESOURCE_BEGIN,RET_RESULT,CREDIT,SOFT_RESET_REQUEST,RESOURCE_CHUNK,RET_RESULT,CREDIT,RESOURCE_ABORT,RET_RESULT,SOFT_RESET_ACK",
        [120,88,48,56,76,88,48,72,88,48],
        [190,190,191,192,191,193,194,192,195,193],
        [0,0,0,0,0,0,0,0,0,1],
    ),
    "soft_reset_replay": _soft_reset_replay_path(),
    "owner_drop_tombstone": _path(
        "ctctctctctt", "OWNER_DROP,TX_RESULT,OWNER_DROP,TX_RESULT,OWNER_DROP,TX_RESULT,OWNER_DROP,TX_RESULT,OWNER_OPEN,RET_RESULT,CREDIT",
        [72,60,72,60,72,60,72,60,104,88,48],
        [100,100,101,101,102,102,103,103,104,104,105], [0] * 11,
    ),
    "control_reserve_boundary": _path(
        "cttctt", "OWNER_DROP,TX_RESULT,CREDIT,OWNER_OPEN,RET_RESULT,CREDIT",
        [72,60,48,104,88,48], [140,140,141,141,142,143], [0] * 6,
    ),
    "stale_generation": _path(
        "cttctt", "OWNER_OPEN,RET_RESULT,CREDIT,OWNER_OPEN,RET_RESULT,CREDIT",
        [104,88,48,104,88,48], [60,60,61,61,62,63], [0] * 6,
    ),
    "aggregate_quota_exhaustion": _path(
        "ctt", "OWNER_OPEN,RET_RESULT,CREDIT", [104,88,48], [164,164,165], [0] * 3,
    ),
    "resource_lifecycle": _path(
        "cttcttcttctctcccttctt",
        "RESOURCE_DROP,RET_RESULT,CREDIT,RESOURCE_BEGIN,RET_RESULT,CREDIT,RESOURCE_COMMIT,RET_RESULT,CREDIT,RESOURCE_ABORT,RET_RESULT,RESOURCE_ABORT,RET_RESULT,PRESENT_BEGIN,OBJECT_DROP,PRESENT_COMMIT,TX_RESULT,CREDIT,RESOURCE_DROP,RET_RESULT,CREDIT",
        [64,88,48,120,88,48,64,88,48,72,88,72,88,104,64,56,60,48,64,88,48],
        [90,90,91,91,92,93,92,94,95,93,96,94,97,95,96,97,98,99,98,100,101], [0] * 21,
    ),
    "resource_chunk_overrun": _path(
        "cttctt", "RESOURCE_BEGIN,RET_RESULT,CREDIT,RESOURCE_CHUNK,RET_RESULT,CREDIT",
        [120,88,48,77,88,48], [70,70,71,71,72,73], [0] * 6,
    ),
    "resource_digest_failure": _path(
        "cttctctt", "RESOURCE_BEGIN,RET_RESULT,CREDIT,RESOURCE_CHUNK,CREDIT,RESOURCE_COMMIT,RET_RESULT,CREDIT",
        [120,88,48,76,48,64,88,48], [80,80,81,81,82,82,83,84], [0] * 8,
    ),
}


OBJECT_BASE = {
    1: {"object_id":1,"object_type":1,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,0,4294967295,805306367],"body_sha3_256":"a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a","visible":True},
    2: {"object_id":2,"object_type":2,"flags":1,"z_index":0,"region_id":1,"parent_id":1,"bounds":[0,0,4294967295,4294967295],"body_sha3_256":"0ba6e68a1ee0bef43c7d1c0f9b06f59e2044608d60d73303fc6bbfe8c9b32a35","visible":True},
    3: {"object_id":3,"object_type":4,"flags":1,"z_index":1,"region_id":1,"parent_id":1,"bounds":[0,0,4294967295,4294967295],"body_sha3_256":"b70c62fa76c6dc2d59747ee2c4451d0cc0cf3f7502bcaba972985e65728024cd","text_utf8_hex":"536f756e644c6162","utf8_bytes":8,"visible":True},
    4: {"object_id":4,"object_type":5,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,0,4294967295,805306367],"body_sha3_256":"a2ace35825af19dc3f83e8336f65d1acf1c2030275d1649bdca9910c6a69c4f8","format":1,"decimal_places":2,"value":-1200,"scale":100,"unit_utf8_hex":"6442","formatted_utf8_hex":"2d31322e30306442","utf8_bytes":8,"visible":True},
    5: {"object_id":5,"object_type":6,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,805306368,4294967295,1207959551],"body_sha3_256":"b64f0eb5274270a48132412211eaa091e08041e3aaf3b733d5a8a097b7264615","value":-1200,"visible":True},
    6: {"object_id":6,"object_type":7,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,1207959552,536870911,1610612735],"body_sha3_256":"f9e427a8264b162980676c80f72340d2d6f95cbc765e8de00e0dba5df340f240","value":0,"visible":True},
    7: {"object_id":7,"object_type":8,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,805306368,4294967295,4294967295],"body_sha3_256":"e1598914753be7911a334b9079f10e09b45b3e9913673f37a7f4aa73740bc18d","series_id":1,"visible":True},
    8: {"object_id":8,"object_type":9,"flags":1,"z_index":0,"region_id":1,"parent_id":0,"bounds":[0,2952790016,4294967295,4294967295],"body_sha3_256":"0e3a490a14e884002ebca36e7b4d7d0672c010be9137584bbdf8840f74496bb2","series_id":2,"visible":True},
}


def empty_scene() -> dict[str, dict[str, Any]]:
    return {"regions": {}, "objects": {}, "series": {}}


def scene_key(owner_id: int, generation: int, item_id: int) -> str:
    return f"{owner_key(owner_id, generation)}:{item_id}"


def _refresh_oracle_ledgers(state: dict[str, Any]) -> None:
    def scene_usage(scene: dict[str, dict[str, Any]]) -> list[dict[str, int]]:
        by_owner: dict[str, dict[str, int]] = {}

        def bucket(record: dict[str, Any]) -> dict[str, int]:
            key = owner_key(record["owner_id"], record["generation"])
            return by_owner.setdefault(
                key,
                {
                    "owner_id": record["owner_id"], "generation": record["generation"],
                    "regions": 0, "objects": 0, "series": 0,
                    "utf8_bytes": 0, "sample_slots": 0,
                },
            )

        for record in scene["regions"].values():
            bucket(record)["regions"] += 1
        for record in scene["objects"].values():
            usage = bucket(record)
            usage["objects"] += 1
            usage["utf8_bytes"] += record.get("utf8_bytes", 0)
        for record in scene["series"].values():
            usage = bucket(record)
            usage["series"] += 1
            usage["sample_slots"] += record["capacity"]
        return [by_owner[key] for key in sorted(by_owner)]

    fields = (
        "region_quota", "resource_quota", "object_quota", "series_quota",
        "resource_bytes", "utf8_bytes", "sample_slots",
    )
    live = [owner for owner in state["owners"].values() if owner["state"] == "live"]
    state["owner_reservation_totals"] = {
        field: sum(owner[field] for owner in live) for field in fields
    }
    state["active_usage"] = scene_usage(state["active"])
    state["hidden_usage"] = [] if state["hidden"] is None else scene_usage(state["hidden"]["scene"])

    resources: dict[str, dict[str, int]] = {}
    for record in state["resources"].values():
        key = owner_key(record["owner_id"], record["generation"])
        usage = resources.setdefault(
            key,
            {
                "owner_id": record["owner_id"], "generation": record["generation"],
                "resource_count": 0, "resource_bytes": 0,
                "upload_count": 0, "upload_bytes": 0, "upload_accepted_bytes": 0,
            },
        )
        usage["resource_count"] += 1
        usage["resource_bytes"] += record["byte_length"]
    upload = state["upload"]
    if upload is not None:
        key = owner_key(upload["owner_id"], upload["generation"])
        usage = resources.setdefault(
            key,
            {
                "owner_id": upload["owner_id"], "generation": upload["generation"],
                "resource_count": 0, "resource_bytes": 0,
                "upload_count": 0, "upload_bytes": 0, "upload_accepted_bytes": 0,
            },
        )
        usage["upload_count"] = 1
        usage["upload_bytes"] = upload["declared_bytes"]
        usage["upload_accepted_bytes"] = upload["accepted_bytes"]
    state["owner_wide_resource_usage"] = [resources[key] for key in sorted(resources)]


def owner_record(
    *,
    owner_id: int = OWNER_ID,
    generation: int = OWNER_GENERATION,
    regions: int = 2,
    resources: int = 0,
    objects: int = 12,
    series: int = 2,
    resource_bytes: int = 0,
    utf8_bytes: int = 128,
    sample_slots: int = 7,
    state: str = "live",
) -> dict[str, Any]:
    return {
        "owner_id": owner_id,
        "generation": generation,
        "region_quota": regions,
        "resource_quota": resources,
        "object_quota": objects,
        "series_quota": series,
        "resource_bytes": resource_bytes,
        "utf8_bytes": utf8_bytes,
        "sample_slots": sample_slots,
        "state": state,
    }


def region(
    cols: int,
    rows: int,
    *,
    owner_id: int = OWNER_ID,
    generation: int = OWNER_GENERATION,
    region_id: int = 1,
    geometry_generation: int = 0,
) -> dict[str, Any]:
    return {
        "owner_id": owner_id,
        "generation": generation,
        "region_id": region_id,
        "geometry_generation": geometry_generation,
        "left": 0,
        "top": 0,
        "cols": cols,
        "rows": rows,
        "z_index": 0,
        "flags": 3,
    }


def object_record(object_id: int) -> dict[str, Any]:
    return {"owner_id": OWNER_ID, "generation": OWNER_GENERATION, **copy.deepcopy(OBJECT_BASE[object_id])}


def series_record(
    series_id: int,
    capacity: int,
    timestamp_mode: int,
    interval_us: int,
    samples: list[tuple[int, int]],
    *,
    owner_id: int = OWNER_ID,
    generation: int = OWNER_GENERATION,
) -> dict[str, Any]:
    return {
        "owner_id": owner_id,
        "generation": generation,
        "series_id": series_id,
        "capacity": capacity,
        "timestamp_mode": timestamp_mode,
        "interval_us": interval_us,
        "samples": [{"timestamp_us": timestamp, "value": value} for timestamp, value in samples],
    }


def soundlab_scene(*, cols: int = 80, rows: int = 25, dynamic: bool = False) -> dict[str, dict[str, Any]]:
    scene = empty_scene()
    scene["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(cols, rows)
    uniform = [(1_000_000,-1200),(1_500_000,-900),(2_000_000,-600)]
    explicit = [(1_000_000,-2000),(1_750_000,2000)]
    if dynamic:
        uniform = [(1_500_000,-900),(2_000_000,-600),(2_500_000,-300),(3_000_000,100)]
        explicit = [(1_750_000,2000),(2_500_000,-1000),(3_250_000,1000)]
    scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = series_record(1, 4, 1, 500_000, uniform)
    scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,2)] = series_record(2, 3, 0, 0, explicit)
    for object_id in range(1, 9):
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,object_id)] = object_record(object_id)
    if dynamic:
        readout = scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,4)]
        readout.update({"value":-300,"formatted_utf8_hex":"2d332e30306442","utf8_bytes":7})
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,5)]["value"] = -300
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,6)]["value"] = 1
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,3)]["visible"] = False
    return scene


def compact_scene() -> dict[str, dict[str, Any]]:
    scene = empty_scene()
    scene["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(2, 1)
    scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = series_record(1, 4, 1, 500_000, [(1_000_000,10),(1_500_000,20)])
    readout = object_record(4)
    readout.update({
        "body_sha3_256":"7c2dd7cc4be24f77922089da133b7d169e26e3ca94e7df768c3f445f15afe02d",
        "value":10,"formatted_utf8_hex":"302e31306442","utf8_bytes":6,
    })
    scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,4)] = readout
    scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,7)] = object_record(7)
    return scene


def cell(cols: int, rows: int, *, text: str = "", cursor: tuple[int, int, bool] = (0,0,False)) -> dict[str, Any]:
    overrides = {
        f"{index // cols}:{index % cols}": [ord(char),7,0,1]
        for index, char in enumerate(text)
    }
    return {
        "cols": cols,
        "rows": rows,
        "cursor": {"row":cursor[0],"column":cursor[1],"visible":cursor[2]},
        "default": [32,7,0,0],
        "overrides": overrides,
    }


def image_scene(*, cols: int = 1, rows: int = 1) -> dict[str, dict[str, Any]]:
    scene = empty_scene()
    scene["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(cols, rows)
    image = {"owner_id":OWNER_ID,"generation":OWNER_GENERATION,**copy.deepcopy(OBJECT_BASE.get(1, {}))}
    image.update({
        "object_id":1,"object_type":3,"flags":1,"z_index":0,"region_id":1,"parent_id":0,
        "bounds":[0,0,4294967295,4294967295],
        "body_sha3_256":"1d0f6461f308a06f7ecf9bdf591bd5593373fc8d422b2b7a31aef8e4b01a2f13",
        "resource_id":1,"visible":True,
    })
    scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = image
    return scene


C2T_GRANT_BEFORE = {
    "ret_query_supported": 65_536,
    "ret_query_unsupported": 65_536,
    "soundlab_initial_replace": 83_060,
    "soundlab_dynamic_append": 85_290,
    "mutation_and_drop_lifecycle": 85_078,
    "mixed_commit_and_rejections": 130_000,
    "legacy_cell_and_replace_continue": 72_000,
    "resize_layout_sync": 75_000,
    "reset_crossed_present_commit": 85_000,
    "reset_crossed_owner_drop": 86_000,
    "reset_wrong_tuple_upload_abort": 92_000,
    "soft_reset_replay": 85_518,
    "owner_drop_tombstone": 140_000,
    "control_reserve_boundary": 150_000,
    "stale_generation": 100_000,
    "aggregate_quota_exhaustion": 101_000,
    "resource_lifecycle": 125_000,
    "resource_chunk_overrun": 110_000,
    "resource_digest_failure": 120_000,
}


def _initial_credit(name: str) -> dict[str, dict[str, int]]:
    advertised = C2T_GRANT_BEFORE[name]
    released = advertised - INITIAL_C2T_GRANT
    if name.startswith("ret_query_"):
        advertised = INITIAL_C2T_GRANT
        released = 17_476
    if name == "control_reserve_boundary":
        # One previously consumed 104-byte ordinary frame is pending release;
        # the first CREDIT in this excerpt materializes that exact allowance.
        released += 104
    sent = 150_000 if name == "control_reserve_boundary" else released
    terminal_prior = 0 if name.startswith("ret_query_") else 208
    if name == "resize_layout_sync":
        terminal_prior += 56
    return {
        CLIENT: {
            "initial_grant": INITIAL_C2T_GRANT,
            "sent_complete_bytes": sent,
            "released_complete_bytes": released,
            "last_advertised_grant": advertised,
            "advertised_released_bytes": advertised - INITIAL_C2T_GRANT,
            "pending_release_bytes": released - (advertised - INITIAL_C2T_GRANT),
        },
        TERMINAL: {
            "initial_grant": INITIAL_T2C_GRANT,
            "sent_complete_bytes": terminal_prior,
            "released_complete_bytes": terminal_prior,
            "last_advertised_grant": INITIAL_T2C_GRANT,
            "advertised_released_bytes": 0,
            "pending_release_bytes": terminal_prior,
        },
    }


def _first_sequence_before(name: str, direction: str) -> int:
    first = next(frame["sequence"] for frame in FRAME_PATHS[name] if frame["direction"] == direction)
    return first - 1


def initial_state(name: str) -> dict[str, Any]:
    revision = 1
    highwater = 1
    geometry = {"cols":1,"rows":1,"generation":0}
    cell_state = cell(1, 1, text="S")
    active = empty_scene()
    owners: dict[str, dict[str, Any]] = {}
    resources: dict[str, dict[str, Any]] = {}
    retained_visible = False
    rebuild_required: str | None = None

    if name.startswith("ret_query_"):
        geometry = {"cols":80,"rows":25,"generation":0}
        cell_state = cell(80,25,text="S")
    elif name == "soundlab_initial_replace":
        rebuild_required = "replacement"
        geometry = {"cols":80,"rows":25,"generation":0}
        cell_state = cell(80,25,text="S")
    elif name == "soundlab_dynamic_append":
        revision = highwater = 4
        geometry = {"cols":80,"rows":25,"generation":0}
        cell_state = cell(80,25,text="L",cursor=(0,1,True))
        active = soundlab_scene()
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record()
        retained_visible = True
    elif name in {"mutation_and_drop_lifecycle", "mixed_commit_and_rejections"}:
        revision = highwater = 3
        geometry = {"cols":80,"rows":25,"generation":0}
        cell_state = cell(80,25,text="L",cursor=(0,1,True))
        active = soundlab_scene(dynamic=True)
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record()
        retained_visible = True
    elif name == "legacy_cell_and_replace_continue":
        geometry = {"cols":2,"rows":1,"generation":0}
        cell_state = cell(2,1)
        rebuild_required = "replacement"
    elif name == "resize_layout_sync":
        revision = highwater = 4
        geometry = {"cols":3,"rows":2,"generation":1}
        cell_state = cell(2,1,text="OK")
        active = soundlab_scene(cols=2,rows=1)
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record()
        retained_visible = False
        rebuild_required = "layout"
    elif name in {"reset_crossed_present_commit", "reset_crossed_owner_drop"}:
        revision = highwater = 4
        geometry = {"cols":2,"rows":1,"generation":0}
        cell_state = cell(2,1,text="OK")
        active = compact_scene()
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=1,objects=2,series=1,utf8_bytes=8,sample_slots=4)
        retained_visible = True
    elif name == "reset_wrong_tuple_upload_abort":
        revision = highwater = 3
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=0,resources=1,objects=0,series=0,resource_bytes=4,utf8_bytes=0,sample_slots=0)
        retained_visible = True
    elif name == "soft_reset_replay":
        revision = highwater = 5
        geometry = {"cols":80,"rows":25,"generation":0}
        cell_state = cell(80,25,text="L",cursor=(0,1,True))
        active = soundlab_scene()
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record()
        retained_visible = True
    elif name in {"owner_drop_tombstone", "control_reserve_boundary"}:
        revision = highwater = 3
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=1,objects=0,series=0,utf8_bytes=0,sample_slots=0)
        retained_visible = True
    elif name == "stale_generation":
        revision = highwater = 3
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=2,objects=4,series=1,utf8_bytes=64,sample_slots=8)
        retained_visible = True
    elif name == "aggregate_quota_exhaustion":
        revision = highwater = 4
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=10,resources=5,objects=40,series=10,resource_bytes=700_000,utf8_bytes=10_000,sample_slots=3_000)
        retained_visible = True
    elif name == "resource_lifecycle":
        revision = highwater = 2
        active = image_scene()
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=1,resources=2,objects=1,series=0,resource_bytes=8,utf8_bytes=0,sample_slots=0)
        peer_owner = OWNER_ID + 1
        owners[owner_key(peer_owner,1)] = owner_record(
            owner_id=peer_owner,generation=1,regions=1,resources=0,
            objects=1,series=1,resource_bytes=0,utf8_bytes=0,sample_slots=2,
        )
        active["regions"][scene_key(peer_owner,1,1)] = region(
            1,1,owner_id=peer_owner,generation=1,
        )
        active["series"][scene_key(peer_owner,1,1)] = series_record(
            1,2,1,500_000,[(1_000_000,5)],owner_id=peer_owner,generation=1,
        )
        peer_plot = object_record(7)
        peer_plot.update({"owner_id":peer_owner,"generation":1,"object_id":1})
        active["objects"][scene_key(peer_owner,1,1)] = peer_plot
        resources[resource_key(OWNER_ID,OWNER_GENERATION,1)] = {
            "owner_id":OWNER_ID,"generation":OWNER_GENERATION,"resource_id":1,"format":1,
            "width":1,"height":1,"flags":0,"byte_length":4,
            "sha3_256":"af2a3c69c685db3180e1b36ed08a8569f4438e3c72ac283411376f296fca9f30",
        }
        retained_visible = True
    elif name in {"resource_chunk_overrun", "resource_digest_failure"}:
        revision = highwater = 3
        owners[owner_key(OWNER_ID,OWNER_GENERATION)] = owner_record(regions=0,resources=1,objects=0,series=0,resource_bytes=4,utf8_bytes=0,sample_slots=0)
        retained_visible = True

    return {
        "session_state":"ACTIVE",
        "presentation_epoch":0,
        "global_revision":revision,
        "transaction_id_highwater":highwater,
        "directional_sequences":{
            CLIENT:_first_sequence_before(name,CLIENT),
            TERMINAL:_first_sequence_before(name,TERMINAL),
        },
        "credit_ledgers":_initial_credit(name),
        "control_reserve":{
            CLIENT:{"capacity_bytes":4_096,"occupied_bytes":0,"transcript_consumed_frames":0,"transcript_consumed_bytes":0},
            TERMINAL:{"capacity_bytes":4_096,"occupied_bytes":0,"transcript_consumed_frames":0,"transcript_consumed_bytes":0},
        },
        "selected_geometry":geometry,
        "cell":cell_state,
        "active":active,
        "hidden":None,
        "retained_visible":retained_visible,
        "rebuild_required":rebuild_required,
        "owners":owners,
        "resources":resources,
        "upload":None,
        "open_transaction":None,
        "open_lifecycle_request":None,
        "pending_reset":None,
        "last_result":None,
    }


BEGIN_INFO = {
    ("soundlab_initial_replace",3):(2,1,"present",0,2),
    ("soundlab_initial_replace",15):(3,2,"present",0,3),
    ("soundlab_initial_replace",24):(4,3,"legacy",1,0),
    ("soundlab_dynamic_append",0):(5,4,"present",0,1),
    ("mutation_and_drop_lifecycle",0):(4,3,"present",0,1),
    ("mutation_and_drop_lifecycle",5):(5,4,"present",0,1),
    ("mixed_commit_and_rejections",0):(4,3,"present",1,1),
    ("mixed_commit_and_rejections",7):(5,4,"present",0,1),
    ("mixed_commit_and_rejections",12):(6,4,"present",0,1),
    ("mixed_commit_and_rejections",17):(7,4,"present",0,1),
    ("mixed_commit_and_rejections",22):(8,4,"present",0,1),
    ("mixed_commit_and_rejections",27):(9,4,"present",1,1),
    ("legacy_cell_and_replace_continue",3):(2,1,"legacy",1,0),
    ("legacy_cell_and_replace_continue",9):(3,2,"present",0,2),
    ("legacy_cell_and_replace_continue",15):(4,3,"present",0,3),
    ("resize_layout_sync",0):(5,4,"present",2,0),
    ("resize_layout_sync",7):(6,5,"present",0,4),
    ("resize_layout_sync",12):(7,6,"present",0,5),
    ("reset_crossed_present_commit",0):(5,4,"present",0,1),
    ("soft_reset_replay",2):(1,0,"snapshot",2,0),
    ("soft_reset_replay",49):(2,1,"present",0,2),
    ("soft_reset_replay",54):(3,2,"present",0,3),
    ("resource_lifecycle",13):(3,2,"present",0,1),
}


RESULTS = {
    "soundlab_initial_replace": {1:("RET",8194,0,OWNER_ID,7,0,1,0),13:("TX",2,0,2),22:("TX",3,0,3),28:("TX",4,0,4)},
    "soundlab_dynamic_append": {8:("TX",5,0,5)},
    "mutation_and_drop_lifecycle": {3:("TX",4,0,4),18:("TX",5,0,5)},
    "mixed_commit_and_rejections": {5:("TX",4,0,4),10:("TX",5,2,4),15:("TX",6,2,4),20:("TX",7,2,4),25:("TX",8,2,4),32:("TX",9,2,4)},
    "legacy_cell_and_replace_continue": {1:("RET",8194,0,OWNER_ID,7,0,1,0),7:("TX",2,0,2),13:("TX",3,0,3),20:("TX",4,0,4)},
    "resize_layout_sync": {5:("TX",5,0,5),10:("TX",6,0,6),14:("TX",7,0,7)},
    "reset_crossed_present_commit": {4:("TX",5,1,4)},
    "reset_crossed_owner_drop": {2:("TX",5,1,4)},
    "reset_wrong_tuple_upload_abort": {1:("RET",4096,0,OWNER_ID,7,2,3,0),5:("RET",4097,2,OWNER_ID,6,2,3,0),8:("RET",12,7,OWNER_ID,7,2,3,0)},
    "soft_reset_replay": {30:("TX",1,0,1),37:("RET",8194,0,OWNER_ID,7,0,1,0),40:("RET",4096,0,OWNER_ID,7,1,1,0),47:("RET",4098,0,OWNER_ID,7,1,1,8),52:("TX",2,0,2),57:("TX",3,0,3)},
    "owner_drop_tombstone": {1:("TX",4,0,4),3:("TX",5,0,5),5:("TX",6,2,5),7:("TX",7,3,5),9:("RET",8194,0,OWNER_ID,8,0,5,0)},
    "control_reserve_boundary": {1:("TX",4,0,4),4:("RET",8194,0,OWNER_ID,8,0,4,0)},
    "stale_generation": {1:("RET",8194,2,OWNER_ID,6,0,3,0),4:("RET",8194,1,OWNER_ID+1,1,0,3,0)},
    "aggregate_quota_exhaustion": {1:("RET",8194,3,OWNER_ID+1,1,0,4,0)},
    "resource_lifecycle": {1:("RET",4099,5,OWNER_ID,7,1,2,0),4:("RET",4096,0,OWNER_ID,7,2,2,0),7:("RET",4098,1,OWNER_ID,7,3,2,0),10:("RET",12,1,OWNER_ID,7,2,2,0),12:("RET",12,7,OWNER_ID,7,2,2,0),16:("TX",3,0,3),19:("RET",4099,0,OWNER_ID,7,1,3,0)},
    "resource_chunk_overrun": {1:("RET",4096,0,OWNER_ID,7,1,3,0),4:("RET",4097,1,OWNER_ID,7,1,3,0)},
    "resource_digest_failure": {1:("RET",4096,0,OWNER_ID,7,1,3,0),6:("RET",4098,6,OWNER_ID,7,1,3,0)},
}


def _result(value: tuple[Any, ...]) -> dict[str, Any]:
    if value[0] == "TX":
        return {"kind":"TX_RESULT","transaction_id":value[1],"status":value[2],"detail":0,"model_revision":value[3]}
    return {
        "kind":"RET_RESULT","request_type":value[1],"status":value[2],"detail":0,
        "owner_id":value[3],"generation":value[4],"item_id":value[5],
        "model_revision":value[6],"accepted_bytes":value[7],
    }


def _update_pending_release(ledger: dict[str, int]) -> None:
    ledger["pending_release_bytes"] = ledger["released_complete_bytes"] - ledger["advertised_released_bytes"]


def _reset_epoch(state: dict[str, Any], epoch: int) -> None:
    cols = state["selected_geometry"]["cols"]
    rows = state["selected_geometry"]["rows"]
    state.update({
        "presentation_epoch":epoch,"global_revision":0,"transaction_id_highwater":0,
        "cell":cell(cols,rows),"active":empty_scene(),"hidden":None,"retained_visible":False,
        "rebuild_required":"cell_snapshot","owners":{},"resources":{},"upload":None,
        "open_transaction":None,"open_lifecycle_request":None,"pending_reset":None,
    })


def _open_owner(state: dict[str, Any], record: dict[str, Any], accepted: bool) -> None:
    state["open_lifecycle_request"] = {
        "request":"OWNER_OPEN","owner_id":record["owner_id"],"generation":record["generation"],"phase":"awaiting_result"
    }
    if accepted:
        state["owners"][owner_key(record["owner_id"],record["generation"])] = copy.deepcopy(record)


def _open_upload(state: dict[str, Any], *, resource_id: int, declared_bytes: int, digest: str) -> None:
    state["upload"] = {
        "owner_id":OWNER_ID,"generation":OWNER_GENERATION,"resource_id":resource_id,
        "format":1,"width":1 if declared_bytes == 4 else 2,"height":1,"flags":0,
        "declared_bytes":declared_bytes,"sha3_256":digest,"accepted_bytes":0,
    }
    state["open_lifecycle_request"] = {
        "request":"RESOURCE_BEGIN","owner_id":OWNER_ID,"generation":OWNER_GENERATION,
        "resource_id":resource_id,"phase":"awaiting_result",
    }


def _partial_soundlab_start() -> dict[str, dict[str, Any]]:
    scene = soundlab_scene()
    scene["objects"] = {
        scene_key(OWNER_ID,OWNER_GENERATION,item): scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,item)]
        for item in (1,2,3)
    }
    return scene


def _compact_start() -> dict[str, dict[str, Any]]:
    scene = empty_scene()
    scene["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(2,1)
    scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = series_record(1,4,1,500_000,[])
    return scene


def _set_readout(scene: dict[str, dict[str, Any]], value: int, formatted: str) -> None:
    record = scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,4)]
    record["value"] = value
    encoded = formatted.encode("utf-8")
    record["formatted_utf8_hex"] = encoded.hex()
    record["utf8_bytes"] = len(encoded)


def _owner_drop(state: dict[str, Any], *, generation: int, revision: int) -> None:
    old_key = owner_key(OWNER_ID,generation)
    old = state["owners"].pop(old_key)
    old["state"] = "tombstone"
    state["owners"][old_key] = old
    state["active"] = empty_scene()
    state["hidden"] = None
    state["resources"] = {
        key:value for key,value in state["resources"].items()
        if not (value["owner_id"] == OWNER_ID and value["generation"] == generation)
    }
    state["global_revision"] = revision


def _apply_expected_effect(name: str, index: int, state: dict[str, Any]) -> None:
    # Discovery.
    if name == "ret_query_supported" and index == 2:
        state["rebuild_required"] = "replacement"

    # Full initial SoundLab replacement, reveal, then legacy CELL interleave.
    elif name == "soundlab_initial_replace":
        if index == 0:
            _open_owner(state, owner_record(), True)
        elif index == 12:
            state["hidden"] = {"mode":"replacement","scene":_partial_soundlab_start()}
            state["global_revision"] = 2
        elif index == 21:
            state["active"] = soundlab_scene()
            state["hidden"] = None
            state["retained_visible"] = True
            state["rebuild_required"] = None
            state["global_revision"] = 3
        elif index == 27:
            state["cell"]["overrides"]["0:0"] = [ord("L"),7,0,1]
            state["cell"]["cursor"] = {"row":0,"column":1,"visible":True}
            state["global_revision"] = 4

    elif name == "soundlab_dynamic_append" and index == 7:
        scene = state["active"]
        _set_readout(scene,-300,"-3.00dB")
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,5)]["value"] = -300
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,6)]["value"] = 1
        scene["objects"][scene_key(OWNER_ID,OWNER_GENERATION,3)]["visible"] = False
        scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,1)]["samples"] = [
            {"timestamp_us":t,"value":v} for t,v in ((1_500_000,-900),(2_000_000,-600),(2_500_000,-300),(3_000_000,100))
        ]
        scene["series"][scene_key(OWNER_ID,OWNER_GENERATION,2)]["samples"] = [
            {"timestamp_us":t,"value":v} for t,v in ((1_750_000,2000),(2_500_000,-1000),(3_250_000,1000))
        ]
        state["global_revision"] = 5

    elif name == "mutation_and_drop_lifecycle":
        if index == 2:
            glyph_run = state["active"]["objects"][scene_key(OWNER_ID,OWNER_GENERATION,3)]
            glyph_run.update({
                "body_sha3_256":"0928970982e7ad668e9d24fe9d8c214f410b8c967a16fc08fb8490f87ad43413",
                "text_utf8_hex":"536f756e644c61622061726d6564","utf8_bytes":14,"visible":True,
            })
            state["global_revision"] = 4
        elif index == 17:
            state["active"] = empty_scene()
            state["global_revision"] = 5

    elif name == "mixed_commit_and_rejections":
        if index == 4:
            state["cell"]["overrides"]["0:0"] = [ord("M"),2,0,1]
            state["cell"]["cursor"] = {"row":0,"column":1,"visible":True}
            _set_readout(state["active"],-250,"-2.50dB")
            state["global_revision"] = 4
        elif index == 32:
            state["session_state"] = "SESSION_LOST"

    elif name == "legacy_cell_and_replace_continue":
        if index == 0:
            _open_owner(
                state,
                owner_record(regions=1,objects=2,series=1,utf8_bytes=8,sample_slots=4),
                True,
            )
        elif index == 6:
            state["cell"]["overrides"]["0:0"] = [ord("L"),7,0,1]
            state["cell"]["cursor"] = {"row":0,"column":1,"visible":True}
            state["global_revision"] = 2
        elif index == 12:
            state["hidden"] = {"mode":"replacement","scene":_compact_start()}
            state["global_revision"] = 3
        elif index == 19:
            state["active"] = compact_scene()
            state["hidden"] = None
            state["retained_visible"] = True
            state["rebuild_required"] = None
            state["global_revision"] = 4

    elif name == "resize_layout_sync":
        if index == 4:
            state["cell"] = cell(3,2,text="RESIZE",cursor=(1,2,True))
            state["global_revision"] = 5
        elif index == 9:
            layout = copy.deepcopy(state["active"])
            layout["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(3,2,geometry_generation=1)
            state["hidden"] = {"mode":"layout","scene":layout}
            state["global_revision"] = 6
        elif index == 13:
            state["active"] = state["hidden"]["scene"]
            state["hidden"] = None
            state["retained_visible"] = True
            state["rebuild_required"] = None
            state["global_revision"] = 7

    elif name == "reset_crossed_present_commit":
        if index == 2:
            state["pending_reset"] = {"requested_epoch":1,"last_revision":4}
        elif index == 5:
            _reset_epoch(state,1)

    elif name == "reset_crossed_owner_drop":
        if index == 0:
            state["pending_reset"] = {"requested_epoch":1,"last_revision":4}
        elif index == 1:
            state["transaction_id_highwater"] = 5
            state["open_transaction"] = {"family":"owner_drop","transaction_id":5,"base_revision":4,"phase":"awaiting_result","held_complete_bytes":0}
        elif index == 3:
            _reset_epoch(state,1)

    elif name == "reset_wrong_tuple_upload_abort":
        if index == 0:
            _open_upload(state,resource_id=2,declared_bytes=4,digest="230f97596c358e8038cbff9e2c1f9d090e9a41597d56d1a3dad717d2189f30a7")
        elif index == 3:
            state["pending_reset"] = {"requested_epoch":1,"last_revision":3}
        elif index == 4:
            state["open_lifecycle_request"] = {"request":"RESOURCE_CHUNK","owner_id":OWNER_ID,"generation":6,"resource_id":2,"phase":"awaiting_result"}
        elif index == 7:
            state["open_lifecycle_request"] = {"request":"RESOURCE_ABORT","owner_id":OWNER_ID,"generation":7,"resource_id":2,"phase":"awaiting_result"}
            state["upload"] = None
        elif index == 9:
            _reset_epoch(state,1)

    elif name == "soft_reset_replay":
        if index == 0:
            state["pending_reset"] = {"requested_epoch":1,"last_revision":5}
        elif index == 1:
            _reset_epoch(state,1)
        elif index == 29:
            state["cell"] = cell(80,25,text="S")
            state["global_revision"] = 1
            state["rebuild_required"] = None
        elif index == 34:
            state["rebuild_required"] = "replacement"
        elif index == 36:
            _open_owner(
                state,
                owner_record(
                    regions=1,resources=1,objects=1,series=0,
                    resource_bytes=8,utf8_bytes=0,sample_slots=0,
                ),
                True,
            )
        elif index == 39:
            _open_upload(
                state,resource_id=1,declared_bytes=8,
                digest="2c32110f19fe66aebb4d28fee9392c8a55808d19a2daf5cc7164b4c1eee82f40",
            )
        elif index == 42:
            state["upload"]["accepted_bytes"] = 4
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_CHUNK","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_credit",
            }
        elif index == 44:
            state["upload"]["accepted_bytes"] = 8
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_CHUNK","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_credit",
            }
        elif index == 46:
            upload = state["upload"]
            state["resources"][resource_key(OWNER_ID,7,1)] = {
                "owner_id":OWNER_ID,"generation":7,"resource_id":1,"format":1,
                "width":2,"height":1,"flags":0,"byte_length":8,
                "sha3_256":upload["sha3_256"],
            }
            state["upload"] = None
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_COMMIT","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_result",
            }
        elif index == 51:
            hidden = empty_scene()
            hidden["regions"][scene_key(OWNER_ID,OWNER_GENERATION,1)] = region(80,25)
            state["hidden"] = {"mode":"replacement","scene":hidden}
            state["global_revision"] = 2
        elif index == 56:
            state["active"] = image_scene(cols=80,rows=25)
            state["hidden"] = None
            state["retained_visible"] = True
            state["rebuild_required"] = None
            state["global_revision"] = 3

    elif name == "owner_drop_tombstone":
        if index == 0:
            state["transaction_id_highwater"] = 4
            _owner_drop(state,generation=7,revision=4)
            state["open_transaction"] = {
                "family":"owner_drop","transaction_id":4,"base_revision":3,
                "phase":"awaiting_result","held_complete_bytes":0,
            }
        elif index == 2:
            state["transaction_id_highwater"] = 5
            state["global_revision"] = 5
            state["open_transaction"] = {
                "family":"owner_drop","transaction_id":5,"base_revision":4,
                "phase":"awaiting_result","held_complete_bytes":0,
            }
        elif index == 4:
            state["transaction_id_highwater"] = 6
            state["open_transaction"] = {
                "family":"owner_drop","transaction_id":6,"base_revision":5,
                "phase":"awaiting_result","held_complete_bytes":0,
            }
        elif index == 6:
            state["transaction_id_highwater"] = 7
            state["open_transaction"] = {
                "family":"owner_drop","transaction_id":7,"base_revision":4,
                "phase":"awaiting_result","held_complete_bytes":0,
            }
        elif index == 8:
            state["owners"].pop(owner_key(OWNER_ID,7))
            new = owner_record(generation=8,regions=2,objects=4,series=1,utf8_bytes=64,sample_slots=8)
            _open_owner(state,new,True)

    elif name == "control_reserve_boundary":
        if index == 0:
            state["transaction_id_highwater"] = 4
            _owner_drop(state,generation=7,revision=4)
            state["open_transaction"] = {
                "family":"owner_drop","transaction_id":4,"base_revision":3,
                "phase":"awaiting_result","held_complete_bytes":0,
            }
        elif index == 3:
            state["owners"].pop(owner_key(OWNER_ID,7))
            new = owner_record(generation=8,regions=2,objects=4,series=1,utf8_bytes=64,sample_slots=8)
            _open_owner(state,new,True)

    elif name == "stale_generation":
        if index == 0:
            _open_owner(
                state,
                owner_record(generation=6,regions=2,objects=4,series=1,utf8_bytes=64,sample_slots=8),
                False,
            )
        elif index == 3:
            _open_owner(
                state,
                owner_record(
                    owner_id=OWNER_ID+1,generation=1,regions=17,objects=0,
                    series=0,utf8_bytes=0,sample_slots=0,
                ),
                False,
            )

    elif name == "aggregate_quota_exhaustion" and index == 0:
        _open_owner(
            state,
            owner_record(
                owner_id=OWNER_ID+1,generation=1,regions=8,resources=4,
                objects=30,series=8,resource_bytes=400_000,
                utf8_bytes=7_000,sample_slots=1_500,
            ),
            False,
        )

    elif name == "resource_lifecycle":
        if index == 0:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_DROP","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_result",
            }
        elif index == 3:
            _open_upload(
                state,resource_id=2,declared_bytes=4,
                digest="03aa4563a820892273a5575ac15e45d28240f2f68c1a4c2f78ca8d9fc76c938b",
            )
        elif index == 6:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_COMMIT","owner_id":OWNER_ID,"generation":7,
                "resource_id":3,"phase":"awaiting_result",
            }
        elif index == 9:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_ABORT","owner_id":OWNER_ID,"generation":7,
                "resource_id":2,"phase":"awaiting_result",
            }
        elif index == 11:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_ABORT","owner_id":OWNER_ID,"generation":7,
                "resource_id":2,"phase":"awaiting_result",
            }
            state["upload"] = None
        elif index == 15:
            state["active"]["objects"].pop(scene_key(OWNER_ID,OWNER_GENERATION,1))
            state["global_revision"] = 3
        elif index == 18:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_DROP","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_result",
            }
            state["resources"].pop(resource_key(OWNER_ID,7,1))

    elif name == "resource_chunk_overrun":
        if index == 0:
            _open_upload(
                state,resource_id=1,declared_bytes=4,
                digest="af2a3c69c685db3180e1b36ed08a8569f4438e3c72ac283411376f296fca9f30",
            )
        elif index == 3:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_CHUNK","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_result",
            }
            state["upload"] = None

    elif name == "resource_digest_failure":
        if index == 0:
            _open_upload(state,resource_id=1,declared_bytes=4,digest="ae2a3c69c685db3180e1b36ed08a8569f4438e3c72ac283411376f296fca9f30")
        elif index == 3:
            state["upload"]["accepted_bytes"] = 4
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_CHUNK","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_credit",
            }
        elif index == 5:
            state["open_lifecycle_request"] = {
                "request":"RESOURCE_COMMIT","owner_id":OWNER_ID,"generation":7,
                "resource_id":1,"phase":"awaiting_result",
            }
            state["upload"] = None


def build_oracle(name: str) -> tuple[dict[str, Any], dict[str, Any]]:
    path = FRAME_PATHS[name]
    initial = initial_state(name)
    _refresh_oracle_ledgers(initial)
    state = copy.deepcopy(initial)
    records: list[dict[str, Any]] = []

    for index, frame in enumerate(path):
        state["last_result"] = None
        direction = frame["direction"]
        message = frame["message"]
        complete_bytes = frame["complete_bytes"]
        state["directional_sequences"][direction] = frame["sequence"]

        if message in CONTROL:
            reserve = state["control_reserve"][direction]
            reserve["transcript_consumed_frames"] += 1
            reserve["transcript_consumed_bytes"] += complete_bytes
        else:
            ledger = state["credit_ledgers"][direction]
            ledger["sent_complete_bytes"] += complete_bytes
            begin = BEGIN_INFO.get((name,index))
            if begin is not None:
                transaction_id, base_revision, family, cell_mode, retained_mode = begin
                state["transaction_id_highwater"] = max(state["transaction_id_highwater"],transaction_id)
                state["open_transaction"] = {
                    "family":family,"transaction_id":transaction_id,"base_revision":base_revision,
                    "cell_mode":cell_mode,"retained_mode":retained_mode,"phase":"body",
                    "held_complete_bytes":complete_bytes,
                }
            elif state["open_transaction"] is not None and direction == CLIENT:
                transaction = state["open_transaction"]
                transaction["held_complete_bytes"] += complete_bytes
                if message in {"TX_COMMIT","SNAPSHOT_COMMIT","PRESENT_COMMIT"}:
                    ledger["released_complete_bytes"] += transaction["held_complete_bytes"]
                    transaction["held_complete_bytes"] = 0
                    transaction["phase"] = "awaiting_result"
                    _update_pending_release(ledger)
            else:
                ledger["released_complete_bytes"] += complete_bytes
                _update_pending_release(ledger)

        _apply_expected_effect(name,index,state)

        if message == "SOFT_RESET_REQUEST" and state["pending_reset"] is None:
            # Every reset request in these journeys has an explicit semantic effect.
            raise ValueError(f"{name}: reset request lacks independent oracle effect")

        if message == "CREDIT":
            receiver_direction = CLIENT if direction == TERMINAL else TERMINAL
            ledger = state["credit_ledgers"][receiver_direction]
            ledger["last_advertised_grant"] = ledger["initial_grant"] + ledger["released_complete_bytes"]
            ledger["advertised_released_bytes"] = ledger["released_complete_bytes"]
            _update_pending_release(ledger)
            if state["open_lifecycle_request"] is not None and state["open_lifecycle_request"].get("phase") == "awaiting_credit":
                state["open_lifecycle_request"] = None

        result = RESULTS.get(name,{}).get(index)
        if result is not None:
            state["last_result"] = _result(result)
            if result[0] == "TX":
                state["open_transaction"] = None
            else:
                state["open_lifecycle_request"] = None

        _refresh_oracle_ledgers(state)
        records.append({
            "frame_index":index,"direction":direction,"message":message,
            "sequence":frame["sequence"],"epoch":frame["epoch"],
            "state":canonical_state(state),
        })

    meta = {
        "contract_id":CONTRACT_ID,
        "transcript":f"{name}.hex",
        "directions":[frame["direction"] for frame in path],
        "initial_state":initial,
    }
    sidecar = deduplicate_records(
        contract_id=CONTRACT_ID,
        transcript=f"{name}.hex",
        initial_state=initial,
        records=records,
    )
    return meta,sidecar


def write_oracles(root: Path) -> None:
    for name in FRAME_PATHS:
        meta,sidecar = build_oracle(name)
        (root / f"{name}.meta.json").write_text(json.dumps(meta,indent=2)+"\n",encoding="utf-8")
        (root / f"{name}.state.json").write_text(json.dumps(sidecar,indent=2)+"\n",encoding="utf-8")


def check_oracles(root: Path) -> None:
    for name in FRAME_PATHS:
        meta,sidecar = build_oracle(name)
        actual_meta = json.loads((root/f"{name}.meta.json").read_text(encoding="utf-8"))
        actual_sidecar = json.loads((root/f"{name}.state.json").read_text(encoding="utf-8"))
        if actual_meta != meta:
            raise AssertionError(f"{name}: checked-in meta oracle differs")
        if actual_sidecar != sidecar:
            raise AssertionError(f"{name}: checked-in state oracle differs")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write",action="store_true",help="explicitly rewrite independent meta/state oracles")
    args = parser.parse_args()
    root = Path(__file__).resolve().parent
    if args.write:
        write_oracles(root)
    else:
        check_oracles(root)
        print(f"checked {len(FRAME_PATHS)} independent RETAINED-1 state oracles")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
