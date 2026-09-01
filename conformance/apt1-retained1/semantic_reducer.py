#!/usr/bin/env python3
"""Independent byte-to-state reducer for APT-1 RETAINED-1 vectors.

The reducer consumes only checked-in complete frame bytes plus a transcript's
``.meta.json`` direction and exact initial-state oracle.  It imports neither
fixture constructors nor the independent high-level semantic oracle.
"""

from __future__ import annotations

import copy
import hashlib
import struct
from typing import Any, Sequence

from canonical_state import canonical_state, deduplicate_records, owner_key, resource_key


CONTRACT_ID = "APT-1-RETAINED-1-2026-09-01"
MAGIC = b"\xa5PT1"
HEADER_BYTES = 40
RETAINED_TAG = 0x31544552
CLIENT = "client_to_terminal"
TERMINAL = "terminal_to_client"

MESSAGE_TYPES = {
    "CREDIT":0x0003,"SOFT_RESET_REQUEST":0x0007,"SOFT_RESET_ACK":0x0008,
    "TX_RESULT":0x0009,"RET_RESULT":0x000A,"OWNER_DROP":0x000B,
    "RESOURCE_ABORT":0x000C,"TX_BEGIN":0x0100,"CELL_SPAN":0x0101,
    "CURSOR":0x0102,"TX_COMMIT":0x0103,"SNAPSHOT_BEGIN":0x0110,
    "SNAPSHOT_COMMIT":0x0111,"RESOURCE_BEGIN":0x1000,"RESOURCE_CHUNK":0x1001,
    "RESOURCE_COMMIT":0x1002,"RESOURCE_DROP":0x1003,"PRESENT_BEGIN":0x2000,
    "PRESENT_COMMIT":0x2001,"OWNER_OPEN":0x2002,"REGION_DEFINE":0x2010,
    "REGION_REPLACE":0x2011,"REGION_DROP":0x2012,"OBJECT_DEFINE":0x2020,
    "OBJECT_REPLACE":0x2021,"OBJECT_SET_VALUE":0x2022,
    "OBJECT_SET_VISIBILITY":0x2023,"OBJECT_DROP":0x2024,"SERIES_DEFINE":0x3000,
    "SERIES_APPEND":0x3001,"SERIES_REPLACE":0x3002,"SERIES_DROP":0x3003,
    "RET_QUERY":0x8000,"RET_CAPS":0x8001,"RET_FORMATS":0x8002,
}
TYPE_NAMES = {value:name for name,value in MESSAGE_TYPES.items()}

CONTROL = {
    "CREDIT","SOFT_RESET_REQUEST","SOFT_RESET_ACK","TX_RESULT","RET_RESULT",
    "OWNER_DROP","RESOURCE_ABORT",
}
BEGIN_MESSAGES = {"TX_BEGIN","SNAPSHOT_BEGIN","PRESENT_BEGIN"}
COMMIT_MESSAGES = {"TX_COMMIT","SNAPSHOT_COMMIT","PRESENT_COMMIT"}
RETAINED_OPERATIONS = {
    "REGION_DEFINE","REGION_REPLACE","REGION_DROP","OBJECT_DEFINE",
    "OBJECT_REPLACE","OBJECT_SET_VALUE","OBJECT_SET_VISIBILITY","OBJECT_DROP",
    "SERIES_DEFINE","SERIES_APPEND","SERIES_REPLACE","SERIES_DROP",
}

STATUS = {
    "RET_OK":0,"RET_INVALID":1,"RET_STALE_OWNER":2,"RET_NO_CAPACITY":3,
    "RET_DUPLICATE_ID":4,"RET_IN_USE":5,"RET_BAD_CONTENT":6,"RET_ABORTED":7,
}
OBJECT_TYPES = {
    "GROUP":1,"POLYLINE":2,"IMAGE":3,"GLYPH_RUN":4,"READOUT":5,"METER":6,
    "STATUS":7,"PLOT":8,"WAVEFORM":9,
}

CAPACITY_LIMITS = {
    "region_quota":16,"resource_quota":8,"object_quota":64,"series_quota":16,
    "resource_bytes":1_048_576,"utf8_bytes":16_384,"sample_slots":4_096,
}
MAX_OWNER_RECORDS = 8
MAX_LIVE_OWNERS = 4

HEADER_PREFIX = struct.Struct("<4sBBHHHIQQI")
HEADER = struct.Struct("<4sBBHHHIQQII")
CREDIT = struct.Struct("<Q")
TX_RESULT = struct.Struct("<QHHQ")
SOFT_RESET_REQUEST = struct.Struct("<I4xQ")
SOFT_RESET_ACK = struct.Struct("<IHH")
TX_BEGIN = struct.Struct("<QQIIII")
CELL_SPAN_PREFIX = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
TX_COMMIT = struct.Struct("<Q")
RET_CAPS = struct.Struct("<IHHQIIIIIIIIQQ")
RET_FORMATS = struct.Struct("<IIIIIIIIIIQQQ")
RET_RESULT = struct.Struct("<HHIQQQQQ")
OWNER_OPEN = struct.Struct("<QQIIIIQQQQ")
PRESENT_BEGIN = struct.Struct("<QQQQIIIIIIII")
PRESENT_COMMIT = struct.Struct("<QII")
REGION = struct.Struct("<QQQiiIIIIIIiI")
OBJECT_PREFIX = struct.Struct("<QQQHHiQQiiII")
GLYPH_RUN_BODY = struct.Struct("<4B4BHHI")
GLYPH_RUN_ATTRIBUTE_MASK = 0x006F
READOUT_BODY = struct.Struct("<8BIIqqII")
METER_BODY = struct.Struct("<8BIIqqqQ")
STATUS_BODY = struct.Struct("<8BqIIQ")
PLOT_BODY = struct.Struct("<Qqq8BII")
WAVEFORM_BODY = struct.Struct("<Qqq8BqII")
IMAGE_BODY = struct.Struct("<QIB3x")
OBJECT_SET_VALUE = struct.Struct("<QQQq")
OBJECT_SET_VISIBILITY = struct.Struct("<QQQB7x")
SERIES_DEFINE = struct.Struct("<QQQIIQ")
SERIES_SAMPLES = struct.Struct("<QQQIIQ")
RESOURCE_BEGIN = struct.Struct("<QQQIIIIQ32s")
RESOURCE_CHUNK = struct.Struct("<QQQQ")
RESOURCE_COMMIT = struct.Struct("<QQQ")
RESOURCE_ABORT = struct.Struct("<QQQH6x")
OWNER_DROP = struct.Struct("<QQQQ")
OWNER_ITEM = struct.Struct("<QQQ")


def _fail(message: str) -> None:
    raise AssertionError(message)


def _crc32c(data: bytes) -> int:
    crc = 0xFFFFFFFF
    for byte in data:
        crc ^= byte
        for _ in range(8):
            crc = (crc >> 1) ^ (0x82F63B78 if crc & 1 else 0)
    return crc ^ 0xFFFFFFFF


def _decode_frame(encoded: bytes) -> dict[str, Any]:
    if len(encoded) < HEADER_BYTES:
        _fail("semantic reducer saw a truncated frame")
    values = HEADER.unpack_from(encoded)
    magic,reserved0,header_bytes,message_type,flags,reserved,payload_bytes,session,sequence,epoch,checksum = values
    if (magic,reserved0,header_bytes,flags,reserved) != (MAGIC,0,HEADER_BYTES,0,0):
        _fail("semantic reducer saw an invalid frame header")
    if len(encoded) != HEADER_BYTES + payload_bytes:
        _fail("semantic reducer saw a payload-length mismatch")
    payload = encoded[HEADER_BYTES:]
    if _crc32c(encoded[:36] + payload) != checksum:
        _fail("semantic reducer saw a CRC-32C mismatch")
    if message_type not in TYPE_NAMES:
        _fail(f"semantic reducer saw unknown message type {message_type:#x}")
    return {
        "message":TYPE_NAMES[message_type],"payload":payload,"session":session,
        "sequence":sequence,"epoch":epoch,"complete_bytes":len(encoded),
    }


def _empty_scene() -> dict[str, dict[str, Any]]:
    return {"regions":{},"objects":{},"series":{}}


def _scene_item_key(owner: int, generation: int, item: int) -> str:
    return f"{owner_key(owner,generation)}:{item}"


def _normalize_scene_keys(scene: dict[str,Any]) -> None:
    for family,id_field in (("regions","region_id"),("objects","object_id"),("series","series_id")):
        scene[family] = {
            _scene_item_key(record["owner_id"],record["generation"],record[id_field]):record
            for record in scene[family].values()
        }


def _recompute_reducer_ledgers(state: dict[str,Any]) -> None:
    def reduce_scene(scene: dict[str,Any]) -> list[dict[str,int]]:
        accumulated: dict[tuple[int,int],dict[str,int]] = {}
        for family,count_field in (("regions","regions"),("objects","objects"),("series","series")):
            for record in scene[family].values():
                identity = (record["owner_id"],record["generation"])
                if identity not in accumulated:
                    accumulated[identity] = {
                        "owner_id":identity[0],"generation":identity[1],
                        "regions":0,"objects":0,"series":0,
                        "utf8_bytes":0,"sample_slots":0,
                    }
                item = accumulated[identity]
                item[count_field] += 1
                if family == "objects":
                    item["utf8_bytes"] += int(record.get("utf8_bytes",0))
                elif family == "series":
                    item["sample_slots"] += int(record["capacity"])
        return [accumulated[key] for key in sorted(accumulated)]

    state["active_usage"] = reduce_scene(state["active"])
    state["hidden_usage"] = [] if state["hidden"] is None else reduce_scene(state["hidden"]["scene"])
    quota_fields = (
        "region_quota","resource_quota","object_quota","series_quota",
        "resource_bytes","utf8_bytes","sample_slots",
    )
    state["owner_reservation_totals"] = {
        field: sum(
            int(owner[field]) for owner in state["owners"].values()
            if owner["state"] == "live"
        )
        for field in quota_fields
    }

    usage_by_owner: dict[tuple[int,int],dict[str,int]] = {}
    def resource_bucket(owner: int, generation: int) -> dict[str,int]:
        identity = (owner,generation)
        if identity not in usage_by_owner:
            usage_by_owner[identity] = {
                "owner_id":owner,"generation":generation,
                "resource_count":0,"resource_bytes":0,
                "upload_count":0,"upload_bytes":0,"upload_accepted_bytes":0,
            }
        return usage_by_owner[identity]

    for record in state["resources"].values():
        item = resource_bucket(record["owner_id"],record["generation"])
        item["resource_count"] += 1
        item["resource_bytes"] += int(record["byte_length"])
    if state["upload"] is not None:
        upload = state["upload"]
        item = resource_bucket(upload["owner_id"],upload["generation"])
        item["upload_count"] = 1
        item["upload_bytes"] = int(upload["declared_bytes"])
        item["upload_accepted_bytes"] = int(upload["accepted_bytes"])
    state["owner_wide_resource_usage"] = [usage_by_owner[key] for key in sorted(usage_by_owner)]


def _blank_cell(cols: int, rows: int) -> dict[str, Any]:
    return {
        "cols":cols,"rows":rows,
        "cursor":{"row":0,"column":0,"visible":False},
        "default":[32,7,0,0],"overrides":{},
    }


def _owner_record(payload: bytes) -> dict[str, Any]:
    values = OWNER_OPEN.unpack(payload)
    if values[9] != 0:
        _fail("OWNER_OPEN reserved field is nonzero")
    return {
        "owner_id":values[0],"generation":values[1],"region_quota":values[2],
        "resource_quota":values[3],"object_quota":values[4],"series_quota":values[5],
        "resource_bytes":values[6],"utf8_bytes":values[7],"sample_slots":values[8],
        "state":"live",
    }


def _format_readout(format_id: int, decimals: int, value: int, scale: int, unit: bytes) -> bytes:
    if format_id == 0:
        if decimals != 0 or scale != 1:
            _fail("invalid integer READOUT")
        text = str(value)
    else:
        if format_id not in {1,2} or scale <= 0:
            _fail("invalid rational READOUT")
        numerator = value * (100 if format_id == 2 else 1)
        sign = "-" if numerator < 0 else ""
        scaled = abs(numerator) * (10**decimals)
        quotient,remainder = divmod(scaled,scale)
        if remainder * 2 >= scale:
            quotient += 1
        integer,fraction = divmod(quotient,10**decimals)
        text = f"{sign}{integer}"
        if decimals:
            text += f".{fraction:0{decimals}d}"
        if format_id == 2:
            text += "%"
    return text.encode("ascii") + unit


def _decode_object(payload: bytes) -> dict[str, Any]:
    values = OBJECT_PREFIX.unpack_from(payload)
    body = payload[OBJECT_PREFIX.size:]
    record: dict[str,Any] = {
        "owner_id":values[0],"generation":values[1],"object_id":values[2],
        "object_type":values[3],"flags":values[4],"z_index":values[5],
        "region_id":values[6],"parent_id":values[7],"bounds":list(values[8:12]),
        "body_sha3_256":hashlib.sha3_256(body).hexdigest(),"visible":bool(values[4]&1),
    }
    object_type = values[3]
    if object_type == OBJECT_TYPES["GLYPH_RUN"]:
        fields = GLYPH_RUN_BODY.unpack_from(body)
        text = body[GLYPH_RUN_BODY.size:]
        if fields[8] & ~GLYPH_RUN_ATTRIBUTE_MASK:
            _fail("GLYPH_RUN attributes contain unsupported bits")
        if fields[9] != 0:
            _fail("GLYPH_RUN reserved field is nonzero")
        if len(text) != fields[10]:
            _fail("GLYPH_RUN length mismatch")
        record.update({"text_utf8_hex":text.hex(),"utf8_bytes":len(text)})
    elif object_type == OBJECT_TYPES["READOUT"]:
        fields = READOUT_BODY.unpack_from(body)
        unit = body[READOUT_BODY.size:]
        if len(unit) != fields[12]:
            _fail("READOUT unit length mismatch")
        formatted = _format_readout(fields[8],fields[9],fields[10],fields[11],unit)
        record.update({
            "format":fields[8],"decimal_places":fields[9],"value":fields[10],
            "scale":fields[11],"unit_utf8_hex":unit.hex(),
            "formatted_utf8_hex":formatted.hex(),"utf8_bytes":len(formatted),
        })
    elif object_type == OBJECT_TYPES["METER"]:
        record["value"] = METER_BODY.unpack_from(body)[12]
    elif object_type == OBJECT_TYPES["STATUS"]:
        record["value"] = STATUS_BODY.unpack_from(body)[8]
    elif object_type == OBJECT_TYPES["PLOT"]:
        record["series_id"] = PLOT_BODY.unpack_from(body)[0]
    elif object_type == OBJECT_TYPES["WAVEFORM"]:
        record["series_id"] = WAVEFORM_BODY.unpack_from(body)[0]
    elif object_type == OBJECT_TYPES["IMAGE"]:
        record["resource_id"] = IMAGE_BODY.unpack_from(body)[0]
    return record


def _series_samples(payload: bytes, series: dict[str,Any]) -> list[dict[str,int]]:
    owner,generation,series_id,count,timestamp_mode,first = SERIES_SAMPLES.unpack_from(payload)
    if (owner,generation,series_id,timestamp_mode) != (
        series["owner_id"],series["generation"],series["series_id"],series["timestamp_mode"]
    ):
        _fail("series authority or timestamp mode mismatch")
    body = payload[SERIES_SAMPLES.size:]
    if timestamp_mode == 1:
        if len(body) != count * 8:
            _fail("uniform series byte count mismatch")
        values = struct.unpack(f"<{count}q",body)
        return [
            {"timestamp_us":first+i*series["interval_us"],"value":value}
            for i,value in enumerate(values)
        ]
    if len(body) != count * 16:
        _fail("explicit series byte count mismatch")
    return [
        {"timestamp_us":timestamp,"value":value}
        for timestamp,value in struct.iter_unpack("<Qq",body)
    ]


def _apply_retained(
    scene: dict[str,Any],
    message: str,
    payload: bytes,
    geometry_generation: int,
) -> None:
    if message in {"REGION_DEFINE","REGION_REPLACE"}:
        (
            owner,
            generation,
            item,
            logical_x,
            logical_y,
            logical_cols,
            logical_rows,
            clip_x,
            clip_y,
            clip_cols,
            clip_rows,
            z_index,
            flags,
        ) = REGION.unpack(payload)
        key = _scene_item_key(owner,generation,item)
        if message == "REGION_DEFINE" and key in scene["regions"]:
            _fail("duplicate REGION_DEFINE")
        if message == "REGION_REPLACE" and key not in scene["regions"]:
            _fail("absent REGION_REPLACE")
        scene["regions"][key] = {
            "owner_id":owner,"generation":generation,"region_id":item,
            "logical_x":logical_x,"logical_y":logical_y,
            "logical_cols":logical_cols,"logical_rows":logical_rows,
            "clip_x":clip_x,"clip_y":clip_y,"clip_cols":clip_cols,
            "clip_rows":clip_rows,"z_index":z_index,"flags":flags,
            "geometry_generation":geometry_generation,
        }
    elif message in {"OBJECT_DEFINE","OBJECT_REPLACE"}:
        record = _decode_object(payload)
        key = _scene_item_key(record["owner_id"],record["generation"],record["object_id"])
        if message == "OBJECT_DEFINE" and key in scene["objects"]:
            _fail("duplicate OBJECT_DEFINE")
        if message == "OBJECT_REPLACE" and key not in scene["objects"]:
            _fail("absent OBJECT_REPLACE")
        scene["objects"][key] = record
    elif message == "OBJECT_SET_VALUE":
        owner,generation,item,value = OBJECT_SET_VALUE.unpack(payload)
        record = scene["objects"].get(_scene_item_key(owner,generation,item))
        if record is None or (record["owner_id"],record["generation"]) != (owner,generation):
            _fail("invalid OBJECT_SET_VALUE authority")
        record["value"] = value
        if record["object_type"] == OBJECT_TYPES["READOUT"]:
            formatted = _format_readout(
                record["format"],record["decimal_places"],value,record["scale"],
                bytes.fromhex(record["unit_utf8_hex"]),
            )
            record["formatted_utf8_hex"] = formatted.hex()
            record["utf8_bytes"] = len(formatted)
    elif message == "OBJECT_SET_VISIBILITY":
        owner,generation,item,visible = OBJECT_SET_VISIBILITY.unpack(payload)
        record = scene["objects"].get(_scene_item_key(owner,generation,item))
        if record is None or (record["owner_id"],record["generation"]) != (owner,generation):
            _fail("invalid OBJECT_SET_VISIBILITY authority")
        record["visible"] = bool(visible)
    elif message == "OBJECT_DROP":
        owner,generation,item = OWNER_ITEM.unpack(payload)
        key = _scene_item_key(owner,generation,item)
        record = scene["objects"].get(key)
        if record is None or (record["owner_id"],record["generation"]) != (owner,generation):
            _fail("invalid OBJECT_DROP authority")
        scene["objects"].pop(key)
    elif message == "SERIES_DEFINE":
        owner,generation,item,capacity,timestamp_mode,interval = SERIES_DEFINE.unpack(payload)
        key = _scene_item_key(owner,generation,item)
        if key in scene["series"] or timestamp_mode not in {0,1}:
            _fail("invalid SERIES_DEFINE")
        scene["series"][key] = {
            "owner_id":owner,"generation":generation,"series_id":item,
            "capacity":capacity,"timestamp_mode":timestamp_mode,
            "interval_us":interval,"samples":[],
        }
    elif message in {"SERIES_APPEND","SERIES_REPLACE"}:
        owner,generation,item,_,_,_ = SERIES_SAMPLES.unpack_from(payload)
        series = scene["series"].get(_scene_item_key(owner,generation,item))
        if series is None:
            _fail("samples for absent series")
        samples = _series_samples(payload,series)
        if message == "SERIES_APPEND":
            if series["samples"] and samples and samples[0]["timestamp_us"] <= series["samples"][-1]["timestamp_us"]:
                _fail("SERIES_APPEND timestamp does not advance newest history")
            series["samples"] = (series["samples"] + samples)[-series["capacity"]:]
        else:
            if any(a["timestamp_us"] >= b["timestamp_us"] for a,b in zip(samples,samples[1:])):
                _fail("SERIES_REPLACE timestamps are not strict")
            if len(samples) > series["capacity"]:
                _fail("SERIES_REPLACE exceeds capacity")
            series["samples"] = samples
    elif message in {"REGION_DROP","SERIES_DROP"}:
        owner,generation,item = OWNER_ITEM.unpack(payload)
        family = "regions" if message == "REGION_DROP" else "series"
        id_field = "region_id" if message == "REGION_DROP" else "series_id"
        key = _scene_item_key(owner,generation,item)
        record = scene[family].get(key)
        if record is None or (record["owner_id"],record["generation"],record[id_field]) != (owner,generation,item):
            _fail(f"invalid {message} authority")
        scene[family].pop(key)
    else:
        _fail(f"unimplemented retained operation {message}")


def _validate_scene(state: dict[str,Any], scene: dict[str,Any]) -> None:
    for record in scene["objects"].values():
        if _scene_item_key(record["owner_id"],record["generation"],record["region_id"]) not in scene["regions"]:
            _fail("dangling object region")
        if record["parent_id"] and _scene_item_key(record["owner_id"],record["generation"],record["parent_id"]) not in scene["objects"]:
            _fail("dangling object parent")
        if "series_id" in record and _scene_item_key(record["owner_id"],record["generation"],record["series_id"]) not in scene["series"]:
            _fail("dangling object series")
        if "resource_id" in record and resource_key(
            record["owner_id"],record["generation"],record["resource_id"]
        ) not in state["resources"]:
            _fail("dangling object resource")

    usage: dict[str,dict[str,int]] = {}
    def use(owner: int, generation: int) -> dict[str,int]:
        key = owner_key(owner,generation)
        usage.setdefault(key,{"regions":0,"objects":0,"series":0,"utf8_bytes":0,"sample_slots":0})
        return usage[key]
    for record in scene["regions"].values(): use(record["owner_id"],record["generation"])["regions"] += 1
    for record in scene["objects"].values():
        item = use(record["owner_id"],record["generation"])
        item["objects"] += 1
        item["utf8_bytes"] += int(record.get("utf8_bytes",0))
    for record in scene["series"].values():
        item = use(record["owner_id"],record["generation"])
        item["series"] += 1
        item["sample_slots"] += record["capacity"]
    mapping = {"regions":"region_quota","objects":"object_quota","series":"series_quota","utf8_bytes":"utf8_bytes","sample_slots":"sample_slots"}
    for key,counts in usage.items():
        owner = state["owners"].get(key)
        if owner is None or owner["state"] != "live":
            _fail("scene usage lacks live owner reservation")
        for count_field,quota_field in mapping.items():
            if counts[count_field] > owner[quota_field]:
                _fail(f"scene exceeds {quota_field}")


def _decode_span(payload: bytes) -> dict[str,Any]:
    row,column,count = CELL_SPAN_PREFIX.unpack_from(payload)
    if len(payload) != CELL_SPAN_PREFIX.size + count * CELL.size:
        _fail("CELL_SPAN count mismatch")
    cells = [list(values) for values in struct.iter_unpack("<IBBH",payload[CELL_SPAN_PREFIX.size:])]
    return {"row":row,"column":column,"count":count,"cells":cells}


def _apply_cell(state: dict[str,Any], transaction: dict[str,Any]) -> None:
    mode = transaction["cell_mode"]
    if mode == 0:
        return
    if transaction["cursor"] is None:
        _fail("CELL mutation lacks cursor")
    cols,rows = transaction["cols"],transaction["rows"]
    if mode == 2:
        cell = _blank_cell(cols,rows)
    elif mode == 1:
        cell = copy.deepcopy(state["cell"])
        if (cell["cols"],cell["rows"]) != (cols,rows):
            _fail("CELL delta geometry mismatch")
    else:
        _fail("unknown CELL mode")
    for span in transaction["cell_spans"]:
        if span["row"] >= rows or span["column"] + span["count"] > cols:
            _fail("CELL span out of bounds")
        for offset,value in enumerate(span["cells"]):
            key = f"{span['row']}:{span['column']+offset}"
            if value == cell["default"]:
                cell["overrides"].pop(key,None)
            else:
                cell["overrides"][key] = value
    cursor = transaction["cursor"]
    if cursor["row"] >= rows or cursor["column"] > cols:
        _fail("cursor out of bounds")
    cell["cursor"] = copy.deepcopy(cursor)
    state["cell"] = cell


def _canonical_replace(transaction: dict[str,Any]) -> None:
    if transaction["family"] != "present" or transaction["cell_mode"] != 2:
        return
    cols,rows = transaction["cols"],transaction["rows"]
    spans = transaction["cell_spans"]
    if len(spans) != rows:
        _fail("CELL_REPLACE lacks exactly one span per row")
    for row,span in enumerate(spans):
        if (span["row"],span["column"],span["count"]) != (row,0,cols):
            _fail("CELL_REPLACE is not canonical full-width ascending")
    retained_bytes = sum(item["complete_bytes"] for item in transaction["retained_operations"])
    expected = 216 + rows * (52 + 8 * cols) + retained_bytes
    if transaction["declared_transaction_bytes"] != expected or transaction["actual_transaction_bytes"] != expected:
        _fail("CELL_REPLACE byte formula mismatch")


def _apply_transaction(state: dict[str,Any], transaction: dict[str,Any]) -> None:
    if transaction["base_revision"] != state["global_revision"]:
        _fail("transaction base revision mismatch")
    if transaction["family"] == "present":
        if (
            transaction["cols"],transaction["rows"],transaction["geometry_generation"]
        ) != (
            state["selected_geometry"]["cols"],state["selected_geometry"]["rows"],
            state["selected_geometry"]["generation"],
        ):
            _fail("PRESENT geometry differs from selected geometry")
        if transaction["declared_transaction_bytes"] != transaction["actual_transaction_bytes"]:
            _fail("PRESENT declared byte count mismatch")
        if transaction["declared_span_count"] != len(transaction["cell_spans"]):
            _fail("PRESENT span count mismatch")
        if transaction["declared_cell_count"] != sum(span["count"] for span in transaction["cell_spans"]):
            _fail("PRESENT cell count mismatch")
        if transaction["declared_retained_count"] != len(transaction["retained_operations"]):
            _fail("PRESENT retained operation count mismatch")
        _canonical_replace(transaction)
    else:
        if transaction["declared_span_count"] != len(transaction["cell_spans"]):
            _fail("CELL span count mismatch")
        if transaction["declared_cell_count"] != sum(span["count"] for span in transaction["cell_spans"]):
            _fail("CELL count mismatch")
        if transaction["family"] == "snapshot" and (
            transaction["cols"],transaction["rows"]
        ) != (
            state["selected_geometry"]["cols"],state["selected_geometry"]["rows"]
        ):
            _fail("SNAPSHOT geometry differs from selected geometry")

    working = copy.deepcopy(state)
    _apply_cell(working,transaction)
    mode = transaction["retained_mode"]
    if mode:
        if mode == 1:
            candidate = copy.deepcopy(state["active"])
            hidden_kind = None
        elif mode == 2:
            candidate = _empty_scene()
            hidden_kind = "replacement"
        elif mode == 3:
            if state["hidden"] is None or state["hidden"]["mode"] != "replacement":
                _fail("REPLACE_CONTINUE lacks hidden replacement")
            candidate = copy.deepcopy(state["hidden"]["scene"])
            hidden_kind = "replacement"
        elif mode == 4:
            candidate = copy.deepcopy(state["active"])
            hidden_kind = "layout"
        elif mode == 5:
            if state["hidden"] is None or state["hidden"]["mode"] != "layout":
                _fail("LAYOUT_CONTINUE lacks hidden layout")
            candidate = copy.deepcopy(state["hidden"]["scene"])
            hidden_kind = "layout"
        else:
            _fail("unknown retained mode")
        for operation in transaction["retained_operations"]:
            _apply_retained(
                candidate,operation["message"],operation["payload"],
                transaction["geometry_generation"],
            )
        _validate_scene(state,candidate)
        if (mode == 1 or transaction["disposition"] == 1) and any(
            region["geometry_generation"] != transaction["geometry_generation"]
            for region in candidate["regions"].values()
        ):
            _fail("visible retained scene has stale region geometry generation")
        if mode == 1:
            working["active"] = candidate
        elif transaction["disposition"] == 1:
            working["active"] = candidate
            working["hidden"] = None
            working["retained_visible"] = True
            working["rebuild_required"] = None
        else:
            working["hidden"] = {"mode":hidden_kind,"scene":candidate}
    if transaction["family"] == "snapshot":
        working["rebuild_required"] = None
    state.update({key:value for key,value in working.items() if not key.startswith("_")})
    state["global_revision"] += 1


def _owner_totals(state: dict[str,Any]) -> dict[str,int]:
    return {
        field:sum(owner[field] for owner in state["owners"].values() if owner["state"]=="live")
        for field in CAPACITY_LIMITS
    }


def _predict_owner_open(state: dict[str,Any], owner: dict[str,Any]) -> int:
    same_id = [value for value in state["owners"].values() if value["owner_id"] == owner["owner_id"]]
    exact = state["owners"].get(owner_key(owner["owner_id"],owner["generation"]))
    if exact is not None and exact["state"] == "live":
        comparable = {key:value for key,value in exact.items() if key != "state"}
        requested = {key:value for key,value in owner.items() if key != "state"}
        return STATUS["RET_OK"] if comparable == requested else STATUS["RET_INVALID"]
    if any(value["state"] == "live" for value in same_id):
        return STATUS["RET_STALE_OWNER"]
    if same_id and owner["generation"] <= max(value["generation"] for value in same_id):
        return STATUS["RET_STALE_OWNER"]
    if any(owner[field] > limit for field,limit in CAPACITY_LIMITS.items()):
        return STATUS["RET_INVALID"]
    totals = _owner_totals(state)
    if (
        (len(state["owners"]) >= MAX_OWNER_RECORDS and not any(value["state"]=="tombstone" for value in same_id))
        or sum(value["state"]=="live" for value in state["owners"].values()) >= MAX_LIVE_OWNERS
        or any(totals[field] + owner[field] > CAPACITY_LIMITS[field] for field in CAPACITY_LIMITS)
    ):
        return STATUS["RET_NO_CAPACITY"]
    return STATUS["RET_OK"]


def _resource_referenced(state: dict[str,Any], owner: int, generation: int, item: int) -> bool:
    for scene in [state["active"]] + ([] if state["hidden"] is None else [state["hidden"]["scene"]]):
        for record in scene["objects"].values():
            if (
                record.get("resource_id") == item
                and (record["owner_id"],record["generation"]) == (owner,generation)
            ):
                return True
    return False


def _expected_ret(
    request: str,status: int,state: dict[str,Any],owner: int,generation: int,item: int=0,accepted: int=0
) -> dict[str,Any]:
    return {
        "kind":"RET_RESULT","request_type":MESSAGE_TYPES[request],"status":status,"detail":0,
        "owner_id":owner,"generation":generation,"item_id":item,
        "model_revision":state["global_revision"],"accepted_bytes":accepted,
    }


def _expected_tx(transaction_id: int,status: int,revision: int) -> dict[str,Any]:
    return {
        "kind":"TX_RESULT","transaction_id":transaction_id,"status":status,
        "detail":0,"model_revision":revision,
    }


def _set_lifecycle(state: dict[str,Any], request: str, owner: int, generation: int, item: int | None=None, phase: str="awaiting_result") -> None:
    value = {"request":request,"owner_id":owner,"generation":generation}
    if item is not None:
        value["resource_id"] = item
    value["phase"] = phase
    state["open_lifecycle_request"] = value


def _resource_usage(state: dict[str,Any], owner: int, generation: int) -> tuple[int,int]:
    records = [r for r in state["resources"].values() if (r["owner_id"],r["generation"])==(owner,generation)]
    count = len(records)
    size = sum(r["byte_length"] for r in records)
    upload = state["upload"]
    if upload is not None and (upload["owner_id"],upload["generation"]) == (owner,generation):
        count += 1
        size += upload["declared_bytes"]
    return count,size


def _process_lifecycle(state: dict[str,Any], message: str, payload: bytes) -> None:
    if message == "OWNER_OPEN":
        owner = _owner_record(payload)
        status = _predict_owner_open(state,owner)
        _set_lifecycle(state,message,owner["owner_id"],owner["generation"])
        if status == STATUS["RET_OK"]:
            tombstones = [
                (key,value) for key,value in state["owners"].items()
                if value["owner_id"]==owner["owner_id"] and value["state"]=="tombstone"
            ]
            if tombstones:
                for key,_ in tombstones:
                    state["owners"].pop(key)
            state["owners"][owner_key(owner["owner_id"],owner["generation"])] = owner
        state["_expected_result"] = _expected_ret(message,status,state,owner["owner_id"],owner["generation"])
        return

    if message == "RESOURCE_BEGIN":
        owner,generation,item,format_id,width,height,flags,declared,digest = RESOURCE_BEGIN.unpack(payload)
        _set_lifecycle(state,message,owner,generation,item)
        authority = state["owners"].get(owner_key(owner,generation))
        status = STATUS["RET_OK"]
        if authority is None or authority["state"] != "live":
            status = STATUS["RET_STALE_OWNER"]
        elif state["upload"] is not None or resource_key(owner,generation,item) in state["resources"]:
            status = STATUS["RET_DUPLICATE_ID"]
        else:
            count,size = _resource_usage(state,owner,generation)
            if count+1 > authority["resource_quota"] or size+declared > authority["resource_bytes"]:
                status = STATUS["RET_NO_CAPACITY"]
        if status == STATUS["RET_OK"]:
            state["upload"] = {
                "owner_id":owner,"generation":generation,"resource_id":item,"format":format_id,
                "width":width,"height":height,"flags":flags,"declared_bytes":declared,
                "sha3_256":digest.hex(),"accepted_bytes":0,
            }
            state["_upload_bytes"] = b""
        state["_expected_result"] = _expected_ret(message,status,state,owner,generation,item)
        return

    if message == "RESOURCE_CHUNK":
        owner,generation,item,offset = RESOURCE_CHUNK.unpack_from(payload)
        data = payload[RESOURCE_CHUNK.size:]
        upload = state["upload"]
        exact = upload is not None and (owner,generation,item)==(
            upload["owner_id"],upload["generation"],upload["resource_id"]
        )
        authority = state["owners"].get(owner_key(owner,generation))
        if upload is not None and (owner,generation) != (upload["owner_id"],upload["generation"]):
            status = STATUS["RET_STALE_OWNER"]
        elif upload is None and (authority is None or authority["state"] != "live"):
            status = STATUS["RET_STALE_OWNER"]
        elif not exact:
            status = STATUS["RET_INVALID"]
        elif offset != upload["accepted_bytes"] or offset + len(data) > upload["declared_bytes"]:
            status = STATUS["RET_INVALID"]
            state["upload"] = None
            state["_upload_bytes"] = b""
        else:
            status = STATUS["RET_OK"]
            upload["accepted_bytes"] += len(data)
            state["_upload_bytes"] += data
        if status == STATUS["RET_OK"]:
            _set_lifecycle(state,message,owner,generation,item,phase="awaiting_credit")
        else:
            _set_lifecycle(state,message,owner,generation,item)
            state["_expected_result"] = _expected_ret(message,status,state,owner,generation,item)
        return

    if message == "RESOURCE_COMMIT":
        owner,generation,item = RESOURCE_COMMIT.unpack(payload)
        _set_lifecycle(state,message,owner,generation,item)
        upload = state["upload"]
        exact = upload is not None and (owner,generation,item)==(
            upload["owner_id"],upload["generation"],upload["resource_id"]
        )
        authority = state["owners"].get(owner_key(owner,generation))
        if upload is not None and (owner,generation) != (upload["owner_id"],upload["generation"]):
            status = STATUS["RET_STALE_OWNER"]
            accepted = 0
        elif upload is None and (authority is None or authority["state"] != "live"):
            status = STATUS["RET_STALE_OWNER"]
            accepted = 0
        elif not exact:
            status = STATUS["RET_INVALID"]
            accepted = 0
        else:
            accepted = upload["accepted_bytes"]
            digest = hashlib.sha3_256(state.get("_upload_bytes",b"")).hexdigest()
            if accepted != upload["declared_bytes"]:
                status = STATUS["RET_INVALID"]
                accepted = 0
            elif digest != upload["sha3_256"]:
                status = STATUS["RET_BAD_CONTENT"]
                accepted = 0
            else:
                status = STATUS["RET_OK"]
                state["resources"][resource_key(owner,generation,item)] = {
                    "owner_id":owner,"generation":generation,"resource_id":item,
                    "format":upload["format"],"width":upload["width"],"height":upload["height"],
                    "flags":upload["flags"],"byte_length":upload["declared_bytes"],
                    "sha3_256":upload["sha3_256"],
                }
            state["upload"] = None
            state["_upload_bytes"] = b""
        state["_expected_result"] = _expected_ret(message,status,state,owner,generation,item,accepted)
        return

    if message == "RESOURCE_DROP":
        owner,generation,item = OWNER_ITEM.unpack(payload)
        _set_lifecycle(state,message,owner,generation,item)
        key = resource_key(owner,generation,item)
        authority = state["owners"].get(owner_key(owner,generation))
        if authority is None or authority["state"] != "live":
            status = STATUS["RET_STALE_OWNER"]
        elif key not in state["resources"]:
            status = STATUS["RET_INVALID"]
        elif _resource_referenced(state,owner,generation,item):
            status = STATUS["RET_IN_USE"]
        else:
            status = STATUS["RET_OK"]
            state["resources"].pop(key)
        state["_expected_result"] = _expected_ret(message,status,state,owner,generation,item)
        return

    if message == "RESOURCE_ABORT":
        owner,generation,item,reason = RESOURCE_ABORT.unpack(payload)
        _set_lifecycle(state,message,owner,generation,item)
        upload = state["upload"]
        exact = upload is not None and (owner,generation,item)==(
            upload["owner_id"],upload["generation"],upload["resource_id"]
        )
        authority = state["owners"].get(owner_key(owner,generation))
        if upload is not None and (owner,generation) != (upload["owner_id"],upload["generation"]):
            status = STATUS["RET_STALE_OWNER"]
        elif upload is None and (authority is None or authority["state"] != "live"):
            status = STATUS["RET_STALE_OWNER"]
        elif not exact or reason not in {0,1,2}:
            status = STATUS["RET_INVALID"]
        else:
            status = STATUS["RET_ABORTED"]
            state["upload"] = None
            state["_upload_bytes"] = b""
        state["_expected_result"] = _expected_ret(message,status,state,owner,generation,item)
        return

    _fail(f"unimplemented lifecycle request {message}")


def _begin_transaction(state: dict[str,Any], message: str, payload: bytes, complete_bytes: int) -> None:
    if state.get("_transaction") is not None:
        _fail("nested transaction")
    if message in {"TX_BEGIN","SNAPSHOT_BEGIN"}:
        txid,base,cols,rows,span_count,cell_count = TX_BEGIN.unpack(payload)
        transaction = {
            "family":"snapshot" if message=="SNAPSHOT_BEGIN" else "legacy",
            "transaction_id":txid,"base_revision":base,"cols":cols,"rows":rows,
            "declared_span_count":span_count,"declared_cell_count":cell_count,
            "cell_mode":2 if message=="SNAPSHOT_BEGIN" else 1,"retained_mode":0,
            "cell_spans":[],"cursor":None,"retained_operations":[],"disposition":0,
            "actual_transaction_bytes":complete_bytes,
        }
    else:
        values = PRESENT_BEGIN.unpack(payload)
        if values[11] != 0:
            _fail("PRESENT_BEGIN reserved field is nonzero")
        transaction = {
            "family":"present","transaction_id":values[0],"base_revision":values[1],
            "geometry_generation":values[2],"declared_transaction_bytes":values[3],
            "cols":values[4],"rows":values[5],"declared_span_count":values[6],
            "declared_cell_count":values[7],"declared_retained_count":values[8],
            "cell_mode":values[9],"retained_mode":values[10],"cell_spans":[],
            "cursor":None,"retained_operations":[],"disposition":0,
            "actual_transaction_bytes":complete_bytes,
        }
    state["_transaction"] = transaction
    state["transaction_id_highwater"] = max(state["transaction_id_highwater"],transaction["transaction_id"])
    state["open_transaction"] = {
        "family":transaction["family"],"transaction_id":transaction["transaction_id"],
        "base_revision":transaction["base_revision"],"cell_mode":transaction["cell_mode"],
        "retained_mode":transaction["retained_mode"],"phase":"body",
        "held_complete_bytes":complete_bytes,
    }


def _stage_transaction(state: dict[str,Any], message: str, payload: bytes, complete_bytes: int) -> None:
    transaction = state.get("_transaction")
    if transaction is None:
        _fail(f"{message} without open transaction")
    transaction["actual_transaction_bytes"] += complete_bytes
    state["open_transaction"]["held_complete_bytes"] += complete_bytes
    if message == "CELL_SPAN":
        transaction["cell_spans"].append(_decode_span(payload))
    elif message == "CURSOR":
        if transaction["cursor"] is not None:
            _fail("duplicate cursor")
        row,column,visible = CURSOR.unpack(payload)
        transaction["cursor"] = {"row":row,"column":column,"visible":bool(visible)}
    elif message in RETAINED_OPERATIONS:
        transaction["retained_operations"].append({
            "message":message,"payload":payload,"complete_bytes":complete_bytes,
        })
    elif message in COMMIT_MESSAGES:
        if transaction["family"] == "present":
            txid,disposition,reserved = PRESENT_COMMIT.unpack(payload)
            if reserved != 0:
                _fail("PRESENT_COMMIT reserved field is nonzero")
            transaction["disposition"] = disposition
        else:
            txid = TX_COMMIT.unpack(payload)[0]
        if txid != transaction["transaction_id"]:
            _fail("commit transaction id mismatch")
        expected_commit = {
            "present":"PRESENT_COMMIT","snapshot":"SNAPSHOT_COMMIT","legacy":"TX_COMMIT",
        }[transaction["family"]]
        if message != expected_commit:
            _fail("wrong commit family")
        before_revision = state["global_revision"]
        try:
            candidate = copy.deepcopy(state)
            _apply_transaction(candidate,transaction)
            status = 0
        except AssertionError:
            candidate = None
            status = 2
        if state["pending_reset"] is not None and status == 0:
            status = 1
        elif status == 0 and candidate is not None:
            state.update({key:value for key,value in candidate.items() if not key.startswith("_")})
        resulting_revision = state["global_revision"] if status == 0 else before_revision
        state["_expected_result"] = _expected_tx(transaction["transaction_id"],status,resulting_revision)
        state["open_transaction"]["held_complete_bytes"] = 0
        state["open_transaction"]["phase"] = "awaiting_result"
    else:
        _fail(f"unexpected transaction body frame {message}")


def _drop_owner(state: dict[str,Any], payload: bytes) -> None:
    txid,base,owner,generation = OWNER_DROP.unpack(payload)
    state["transaction_id_highwater"] = max(state["transaction_id_highwater"],txid)
    state["open_transaction"] = {
        "family":"owner_drop","transaction_id":txid,"base_revision":base,
        "phase":"awaiting_result","held_complete_bytes":0,
    }
    key = owner_key(owner,generation)
    exact = state["owners"].get(key)
    same_id = [record for record in state["owners"].values() if record["owner_id"]==owner]
    if state["pending_reset"] is not None and base == state["global_revision"] and exact is not None:
        status = 1
    elif base != state["global_revision"]:
        status = 3
    elif exact is None and same_id:
        status = 2
    elif exact is None:
        status = 2
    else:
        status = 0
        if exact["state"] == "live":
            exact["state"] = "tombstone"
            for scene_name in ("active",):
                scene = state[scene_name]
                for family in ("objects","series","regions"):
                    scene[family] = {
                        item_id:record for item_id,record in scene[family].items()
                        if (record["owner_id"],record["generation"]) != (owner,generation)
                    }
            if state["hidden"] is not None:
                scene = state["hidden"]["scene"]
                for family in ("objects","series","regions"):
                    scene[family] = {
                        item_id:record for item_id,record in scene[family].items()
                        if (record["owner_id"],record["generation"]) != (owner,generation)
                    }
            state["resources"] = {
                resource_id:record for resource_id,record in state["resources"].items()
                if (record["owner_id"],record["generation"]) != (owner,generation)
            }
            if state["upload"] is not None and (state["upload"]["owner_id"],state["upload"]["generation"]) == (owner,generation):
                state["upload"] = None
                state["_upload_bytes"] = b""
        state["global_revision"] += 1
    state["_expected_result"] = _expected_tx(txid,status,state["global_revision"])


def _reset_epoch(state: dict[str,Any], epoch: int) -> None:
    cols = state["selected_geometry"]["cols"]
    rows = state["selected_geometry"]["rows"]
    state.update({
        "presentation_epoch":epoch,"global_revision":0,"transaction_id_highwater":0,
        "cell":_blank_cell(cols,rows),"active":_empty_scene(),"hidden":None,
        "retained_visible":False,"rebuild_required":"cell_snapshot","owners":{},
        "resources":{},"upload":None,"open_transaction":None,
        "open_lifecycle_request":None,"pending_reset":None,
    })
    state["_transaction"] = None
    state["_upload_bytes"] = b""
    state["_expected_result"] = None


def _process_result(state: dict[str,Any], message: str, payload: bytes) -> None:
    expected = state.get("_expected_result")
    if message == "TX_RESULT":
        txid,status,detail,revision = TX_RESULT.unpack(payload)
        actual = {
            "kind":"TX_RESULT","transaction_id":txid,"status":status,
            "detail":detail,"model_revision":revision,
        }
    else:
        request,status,detail,owner,generation,item,revision,accepted = RET_RESULT.unpack(payload)
        actual = {
            "kind":"RET_RESULT","request_type":request,"status":status,"detail":detail,
            "owner_id":owner,"generation":generation,"item_id":item,
            "model_revision":revision,"accepted_bytes":accepted,
        }
    if expected != actual:
        _fail(f"{message} does not match independently predicted result: {actual!r} != {expected!r}")
    state["last_result"] = actual
    if message == "TX_RESULT":
        if actual["status"] != 0 and state.get("_transaction") is not None:
            transaction = state["_transaction"]
            if transaction["family"] == "present" and transaction["cell_mode"] != 0 and actual["status"] != 1:
                state["session_state"] = "SESSION_LOST"
        state["open_transaction"] = None
        state["_transaction"] = None
    else:
        state["open_lifecycle_request"] = None
    state["_expected_result"] = None


def _account_frame(state: dict[str,Any], direction: str, frame: dict[str,Any]) -> None:
    message = frame["message"]
    complete_bytes = frame["complete_bytes"]
    if message in CONTROL:
        reserve = state["control_reserve"][direction]
        reserve["transcript_consumed_frames"] += 1
        reserve["transcript_consumed_bytes"] += complete_bytes
        return
    ledger = state["credit_ledgers"][direction]
    ledger["sent_complete_bytes"] += complete_bytes
    if ledger["sent_complete_bytes"] > ledger["last_advertised_grant"]:
        _fail(f"{message} exceeds ordinary cumulative credit")
    if message in BEGIN_MESSAGES:
        return
    if direction == CLIENT and state.get("_transaction") is not None:
        if message in COMMIT_MESSAGES:
            released = state["open_transaction"]["held_complete_bytes"] + complete_bytes
            ledger["released_complete_bytes"] += released
            if ledger["released_complete_bytes"] > ledger["sent_complete_bytes"]:
                _fail("transaction release exceeds sent complete bytes")
        return
    ledger["released_complete_bytes"] += complete_bytes
    if ledger["released_complete_bytes"] > ledger["sent_complete_bytes"]:
        _fail("ordinary released bytes exceed sent complete bytes")
    ledger["pending_release_bytes"] = (
        ledger["released_complete_bytes"] - ledger["advertised_released_bytes"]
    )


def _finish_transaction_release(state: dict[str,Any], direction: str, message: str) -> None:
    if direction == CLIENT and message in COMMIT_MESSAGES:
        ledger = state["credit_ledgers"][direction]
        ledger["pending_release_bytes"] = (
            ledger["released_complete_bytes"] - ledger["advertised_released_bytes"]
        )


def _validate_initial_state(state: dict[str,Any]) -> None:
    for direction in (CLIENT,TERMINAL):
        ledger = state["credit_ledgers"][direction]
        if ledger["last_advertised_grant"] != ledger["initial_grant"] + ledger["advertised_released_bytes"]:
            _fail("initial advertised grant is inconsistent with advertised release")
        if ledger["pending_release_bytes"] != ledger["released_complete_bytes"] - ledger["advertised_released_bytes"]:
            _fail("initial pending ordinary release is inconsistent")
        if not (0 <= ledger["advertised_released_bytes"] <= ledger["released_complete_bytes"] <= ledger["sent_complete_bytes"]):
            _fail("initial cumulative ordinary counters are not monotonic")
        if ledger["sent_complete_bytes"] > ledger["last_advertised_grant"]:
            _fail("initial sent complete bytes exceed cumulative grant")
        reserve = state["control_reserve"][direction]
        if not (0 <= reserve["occupied_bytes"] <= reserve["capacity_bytes"]):
            _fail("initial control-reserve occupancy is invalid")


def reduce_transcript(meta: dict[str,Any], encoded_frames: Sequence[bytes]) -> dict[str,Any]:
    """Reduce one transcript using only its checked-in meta and frame bytes."""

    if meta.get("contract_id") != CONTRACT_ID:
        _fail("semantic meta contract id mismatch")
    directions = meta.get("directions")
    if not isinstance(directions,list) or len(directions) != len(encoded_frames):
        _fail("semantic meta direction count mismatch")
    state = copy.deepcopy(meta["initial_state"])
    _validate_initial_state(state)
    _normalize_scene_keys(state["active"])
    if state["hidden"] is not None:
        _normalize_scene_keys(state["hidden"]["scene"])
    declared_ledgers = {
        field: copy.deepcopy(state[field])
        for field in (
            "active_usage","hidden_usage","owner_reservation_totals",
            "owner_wide_resource_usage",
        )
    }
    _recompute_reducer_ledgers(state)
    for field,declared in declared_ledgers.items():
        if state[field] != declared:
            _fail(f"initial {field} differs from independently reduced model")
    state["_transaction"] = None
    state["_expected_result"] = None
    state["_upload_bytes"] = b""
    records: list[dict[str,Any]] = []
    session: int | None = None

    for index,(direction,encoded) in enumerate(zip(directions,encoded_frames,strict=True)):
        if direction not in {CLIENT,TERMINAL}:
            _fail("semantic meta has invalid direction")
        frame = _decode_frame(encoded)
        message = frame["message"]
        payload = frame["payload"]
        state["last_result"] = None
        if session is None:
            session = frame["session"]
        elif frame["session"] != session:
            _fail("semantic transcript changes session id")
        expected_sequence = state["directional_sequences"][direction] + 1
        if frame["sequence"] != expected_sequence:
            _fail(f"{message} sequence {frame['sequence']} != {expected_sequence}")
        state["directional_sequences"][direction] = frame["sequence"]
        if message == "SOFT_RESET_ACK":
            requested_epoch,_,_ = SOFT_RESET_ACK.unpack(payload)
            if frame["epoch"] != requested_epoch:
                _fail("SOFT_RESET_ACK header epoch mismatch")
        elif frame["epoch"] != state["presentation_epoch"]:
            _fail(f"{message} uses wrong presentation_epoch")

        _account_frame(state,direction,frame)

        if direction == CLIENT and message in BEGIN_MESSAGES:
            _begin_transaction(state,message,payload,frame["complete_bytes"])
        elif direction == CLIENT and state.get("_transaction") is not None and message not in {"SOFT_RESET_ACK"}:
            _stage_transaction(state,message,payload,frame["complete_bytes"])
        elif direction == CLIENT and message == "OWNER_DROP":
            _drop_owner(state,payload)
        elif direction == CLIENT and message in {
            "OWNER_OPEN","RESOURCE_BEGIN","RESOURCE_CHUNK","RESOURCE_COMMIT",
            "RESOURCE_DROP","RESOURCE_ABORT",
        }:
            _process_lifecycle(state,message,payload)
        elif direction == CLIENT and message == "SOFT_RESET_ACK":
            requested_epoch,status,reserved = SOFT_RESET_ACK.unpack(payload)
            if reserved != 0 or status != 0 or state["pending_reset"] != {
                "requested_epoch":requested_epoch,"last_revision":state["global_revision"]
            }:
                _fail("invalid SOFT_RESET_ACK")
            if state.get("_expected_result") is not None:
                _fail("SOFT_RESET_ACK crossed an unsettled result")
            if state["open_transaction"] is not None or state["open_lifecycle_request"] is not None or state["upload"] is not None:
                _fail("SOFT_RESET_ACK crossed unretired transaction or upload state")
            _reset_epoch(state,requested_epoch)
        elif direction == TERMINAL and message == "SOFT_RESET_REQUEST":
            requested_epoch,last_revision = SOFT_RESET_REQUEST.unpack(payload)
            if requested_epoch != state["presentation_epoch"] + 1 or last_revision != state["global_revision"]:
                _fail("invalid SOFT_RESET_REQUEST")
            state["pending_reset"] = {"requested_epoch":requested_epoch,"last_revision":last_revision}
        elif direction == TERMINAL and message in {"TX_RESULT","RET_RESULT"}:
            _process_result(state,message,payload)
        elif message == "CREDIT":
            receiver = CLIENT if direction == TERMINAL else TERMINAL
            ledger = state["credit_ledgers"][receiver]
            advertised = CREDIT.unpack(payload)[0]
            required = ledger["initial_grant"] + ledger["released_complete_bytes"]
            if advertised != required:
                _fail(f"CREDIT {advertised} != released watermark {required}")
            ledger["last_advertised_grant"] = advertised
            ledger["advertised_released_bytes"] = ledger["released_complete_bytes"]
            ledger["pending_release_bytes"] = 0
            lifecycle = state["open_lifecycle_request"]
            if lifecycle is not None and lifecycle.get("phase") == "awaiting_credit":
                state["open_lifecycle_request"] = None
        elif direction == CLIENT and message == "RET_QUERY":
            if len(payload) != 8:
                _fail("RET_QUERY length mismatch")
        elif direction == TERMINAL and message == "RET_CAPS":
            values = RET_CAPS.unpack(payload)
            if values[:3] != (RETAINED_TAG,0,0):
                _fail("RET_CAPS tag or reserved field mismatch")
            state["_ret_supported"] = bool(values[3] & 1)
        elif direction == TERMINAL and message == "RET_FORMATS":
            RET_FORMATS.unpack(payload)
            if not state.get("_ret_supported"):
                _fail("RET_FORMATS without successful retained discovery")
            state["rebuild_required"] = "replacement"
        else:
            _fail(f"unexpected {direction} {message}")

        _finish_transaction_release(state,direction,message)
        _recompute_reducer_ledgers(state)
        records.append({
            "frame_index":index,"direction":direction,"message":message,
            "sequence":frame["sequence"],"epoch":frame["epoch"],
            "state":canonical_state(state),
        })

    return deduplicate_records(
        contract_id=CONTRACT_ID,transcript=meta["transcript"],
        initial_state=meta["initial_state"],records=records,
    )
