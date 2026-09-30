"""GRID_CELLS extends role values while preserving STX1 bytes and minima."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_wire as helpers
from tests.test_rich_terminal_grid_cells_model import content, item, BASE_FEATURES, FEATURES
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlWireDefinition, RetainedWireError, decode_control_definition,
    decode_ret_caps, encode_control_definition, encode_ret_caps,
)
from rich_terminal.semantic_content import (
    SemanticContentError, SemanticTextRole,
    decode_semantic_text_content, encode_semantic_text_content,
)


def test_grid_cells_capability_preserves_existing_collection_capacity_floors():
    assert RetainedFeature.GRID_CELLS == 1 << 15
    caps = helpers._control_caps(features=FEATURES)
    assert decode_ret_caps(encode_ret_caps(caps)) == caps
    assert struct.unpack_from("<Q", encode_ret_caps(caps), 8)[0] == 0x8301
    policy = replace(caps, max_retained_transaction_bytes=352).policy(
        helpers._control_formats(), client_to_terminal_max_payload=152,
        terminal_to_client_max_payload=64, base_max_transaction_bytes=352)
    assert policy.features == FEATURES
    for features in (RetainedFeature.CORE | RetainedFeature.GRID_CELLS,
                     RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.GRID_CELLS):
        with pytest.raises(ValueError, match="GRID_CELLS requires CONTROL_COLLECTIONS"):
            helpers._control_caps(features=features)
        with pytest.raises(ValueError, match="GRID_CELLS requires CONTROL_COLLECTIONS"):
            replace(policy, features=features)
    with pytest.raises(ValueError, match="reserved"):
        replace(caps, features=FEATURES | (1 << 16))
    legacy = replace(caps, features=BASE_FEATURES)
    assert decode_ret_caps(encode_ret_caps(legacy)) == legacy


@pytest.mark.parametrize("role,number", [(SemanticTextRole.NUMBER, 4),
                                        (SemanticTextRole.FORMULA, 5),
                                        (SemanticTextRole.ERROR, 6)])
def test_typed_role_has_an_independent_stx1_byte_oracle_without_a_version_change(role, number):
    cell = item(key=19, row=0, column=0, role=role, column_span=6, text="茶42")
    value = content(rows=1, viewport_rows=1, primary_key=19, items=(cell,))
    text = "茶42".encode()
    expected = struct.pack("<IHHQIIIIIIIIQQII", 0x31585453, 1, 0, 7,
                           1, 6, 0, 0, 1, 6, 1, 1, 19, 0, 0, 0)
    expected += struct.pack("<QIIIIHHII", 19, 0, 0, 1, 6, number, 0, len(text), 0) + text
    assert encode_semantic_text_content(value) == expected
    assert decode_semantic_text_content(expected) == value
    assert value.requires_grid_cells
    plain = replace(value, items=(replace(cell, role=SemanticTextRole.CONTENT),))
    old = bytearray(encode_semantic_text_content(plain))
    struct.pack_into("<H", old, 72 + 24, number)
    assert bytes(old) == expected
    assert plain.wire_bytes == value.wire_bytes
    assert plain.utf8_bytes == value.utf8_bytes


@pytest.mark.parametrize("role", [SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR])
def test_control_codec_preserves_role_independently_of_negotiated_policy(role):
    body = content(items=(item(role=role),), primary_key=2)
    value = ControlWireDefinition(7, 2, 1, ControlKind.TEXT_GRID,
                                  ControlState.VISIBLE | ControlState.ENABLED,
                                  1, 1, 0, 0, ObjectBounds(0, 0, 24, 4), "", "", body)
    assert decode_control_definition(encode_control_definition(value)) == value
    # Admission is the scene's negotiated feature gate; changing the CONTROL
    # kind to TEXT_AREA still fails its semantic shape at this wire boundary.
    raw = bytearray(encode_control_definition(value))
    struct.pack_into("<H", raw, 24, int(ControlKind.TEXT_AREA))
    with pytest.raises(RetainedWireError):
        decode_control_definition(raw)


@pytest.mark.parametrize("role", [0, 7, 0xFFFF])
def test_unknown_role_values_remain_strictly_rejected(role):
    raw = bytearray(encode_semantic_text_content(content(items=(item(),), primary_key=2)))
    struct.pack_into("<H", raw, 72 + 24, role)
    with pytest.raises(SemanticContentError):
        decode_semantic_text_content(raw)


def test_role_extension_does_not_relax_exact_lengths_or_reserved_item_state():
    raw = encode_semantic_text_content(content(items=(item(),), primary_key=2))
    for malformed in (raw[:-1], raw + b"\0"):
        with pytest.raises(SemanticContentError):
            decode_semantic_text_content(malformed)
    malformed = bytearray(raw)
    struct.pack_into("<H", malformed, 72 + 26, 4)
    with pytest.raises(SemanticContentError):
        decode_semantic_text_content(malformed)
