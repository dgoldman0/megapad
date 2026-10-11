"""QOI files decoded for the hosted terminal.

Pillow's QOI plugin decodes in Python, one read for every byte, which takes
seconds for a photograph.  This decoder gives the same pixels for every file
that plugin takes, and refuses the same files, in under half the time.
That is a host-only gain: a real terminal decodes in its own native code.

Like Pillow's, it reads chunks to the end of the data, ignores the end
marker and anything after the last pixel, and ends a run that would pass the
last pixel there.  The index takes every pixel a chunk other than a run
gives, as Pillow's does.  A file whose header says three channels decodes
opaque.
"""

from __future__ import annotations

import struct
import sys
from array import array

_HEADER = struct.Struct(">4sIIBB")


def qoi_size(data: bytes) -> tuple[int, int]:
    """The width and height the file's header gives."""

    if len(data) < _HEADER.size:
        raise ValueError("the file is shorter than a QOI header")
    magic, width, height, _channels, _colorspace = _HEADER.unpack_from(data)
    if magic != b"qoif":
        raise ValueError("the file is not QOI")
    return width, height


def decode_qoi(data: bytes, width: int, height: int) -> bytes:
    """The file's RGBA8 pixels, row-major with no padding.

    The header must give WIDTH by HEIGHT before any chunk is read.  A file
    that ends inside a chunk, or before its last pixel, raises ValueError.
    """

    if qoi_size(data) != (width, height):
        raise ValueError("the file's picture is not its declared size")
    total = width * height
    pixels = array("I")
    append = pixels.append
    # Pixels are packed little-endian, red in the low byte.
    index = [0] * 64
    r = g = b = 0
    a = 255
    px = 0xFF000000
    pos = _HEADER.size
    length = len(data)
    count = 0
    try:
        while count < total:
            if pos >= length:
                raise ValueError("the file ends before its last pixel")
            byte = data[pos]
            # The chunks a photograph uses most come first.
            if 0x80 <= byte < 0xC0:  # QOI_OP_LUMA
                second = data[pos + 1]
                dg = byte - 0xA0
                r = (r + dg + (second >> 4) - 8) & 0xFF
                g = (g + dg) & 0xFF
                b = (b + dg + (second & 0x0F) - 8) & 0xFF
                pos += 2
            elif byte == 0xFE:  # QOI_OP_RGB
                r = data[pos + 1]
                g = data[pos + 2]
                b = data[pos + 3]
                pos += 4
            elif byte >= 0x40 and byte < 0x80:  # QOI_OP_DIFF
                r = (r + ((byte >> 4) & 3) - 2) & 0xFF
                g = (g + ((byte >> 2) & 3) - 2) & 0xFF
                b = (b + (byte & 3) - 2) & 0xFF
                pos += 1
            elif byte < 0x40:  # QOI_OP_INDEX
                px = index[byte]
                r = px & 0xFF
                g = (px >> 8) & 0xFF
                b = (px >> 16) & 0xFF
                a = px >> 24
                pos += 1
            elif byte == 0xFF:  # QOI_OP_RGBA
                r = data[pos + 1]
                g = data[pos + 2]
                b = data[pos + 3]
                a = data[pos + 4]
                pos += 5
            else:  # QOI_OP_RUN
                run = min((byte & 0x3F) + 1, total - count)
                pixels.extend((px,) * run)
                count += run
                pos += 1
                continue
            px = r | g << 8 | b << 16 | a << 24
            index[(r * 3 + g * 5 + b * 7 + a * 11) & 63] = px
            append(px)
            count += 1
    except IndexError as exc:
        raise ValueError("the file ends inside a chunk") from exc
    if sys.byteorder != "little":
        pixels.byteswap()
    rgba = pixels.tobytes()
    if data[12] == 3:
        opaque = bytearray(rgba)
        opaque[3::4] = b"\xff" * total
        rgba = bytes(opaque)
    return rgba
