"""The hosted terminal's QOI decoder against Pillow's.

The terminal decodes QOI files itself because Pillow's plugin takes seconds
on a photograph.  It must still give exactly the pixels Pillow gives, and
refuse exactly the files Pillow refuses.
"""

from __future__ import annotations

import io
import random
import struct

import pytest
from PIL import Image

from rich_terminal.qoi import decode_qoi, qoi_size

END = bytes(7) + b"\x01"


def _pillow(data: bytes) -> bytes | None:
    try:
        with Image.open(io.BytesIO(data), formats=["QOI"]) as image:
            return image.convert("RGBA").tobytes()
    except Exception:
        return None


def _ours(data: bytes) -> bytes | None:
    try:
        return decode_qoi(data, *qoi_size(data))
    except ValueError:
        return None


def _file(width: int, height: int, chunks: bytes, *, channels: int = 4,
          end: bytes = END) -> bytes:
    return struct.pack(">4sIIBB", b"qoif", width, height, channels, 0) + chunks + end


def _encoded(image: Image.Image) -> bytes:
    out = io.BytesIO()
    image.save(out, "QOI")
    return out.getvalue()


def _noise(size: tuple[int, int], mode: str) -> Image.Image:
    rng = random.Random(7)
    bands = len(mode)
    return Image.frombytes(
        mode, size, bytes(rng.randrange(256) for _ in range(size[0] * size[1] * bands))
    )


def _palette() -> Image.Image:
    # A few colours repeated out of order: the encoder indexes them.
    rng = random.Random(11)
    colours = [(200, 10, 30, 255), (5, 120, 240, 255), (90, 90, 90, 128)]
    image = Image.new("RGBA", (40, 9))
    image.putdata([rng.choice(colours) for _ in range(40 * 9)])
    return image


@pytest.mark.parametrize(
    "image",
    (
        _noise((37, 23), "RGBA"),
        _noise((19, 5), "RGB"),
        Image.linear_gradient("L").resize((64, 48)).convert("RGBA"),
        Image.radial_gradient("L").resize((33, 17)).convert("RGB"),
        Image.new("RGBA", (90, 3), (10, 20, 30, 255)),
        _palette(),
    ),
    ids=("noise", "noise-rgb", "gradient", "radial-rgb", "flat", "palette"),
)
def test_encoded_pictures_decode_as_pillow_decodes_them(image: Image.Image) -> None:
    data = _encoded(image)
    expected = _pillow(data)
    assert expected is not None
    assert _ours(data) == expected == image.convert("RGBA").tobytes()


# Hand-made streams for what an encoder does not make.  A pixel of (0, 0, 0,
# 64) hashes to slot 0, as the zero pixel does.
_EDGES = {
    # A three-channel file still applies an RGBA chunk's alpha to the index,
    # but decodes opaque.
    "three-channels": _file(3, 1, b"\xff\x01\x02\x03\x40\xfe\x04\x05\x06\x00",
                            channels=3),
    # A leading run repeats the start pixel without indexing it.
    "leading-run": _file(3, 1, b"\xc0\x35\xfe\x01\x02\x03"),
    # An unset slot gives the zero pixel, which then takes slot 0.
    "unset-slot": _file(4, 1, b"\xff\x00\x00\x00\x40\x05\x00\x00"),
    # A run past the last pixel ends there.
    "long-run": _file(2, 1, b"\xfe\x01\x02\x03\xc5"),
    "no-end-marker": _file(2, 1, b"\xfe\x01\x02\x03\x40", end=b""),
    "trailing-bytes": _file(2, 1, b"\xfe\x01\x02\x03\x40", end=END + b"more"),
    "luma-diff": _file(3, 1, b"\xa5\x7c\x55\xa0\x88"),
}


@pytest.mark.parametrize("case", tuple(_EDGES))
def test_unusual_streams_decode_as_pillow_decodes_them(case: str) -> None:
    data = _EDGES[case]
    expected = _pillow(data)
    assert expected is not None
    assert _ours(data) == expected


@pytest.mark.parametrize(
    "data",
    (
        b"qoif\x00\x00",
        b"qoig" + _file(1, 1, b"\xfe\x01\x02\x03")[4:],
        _file(2, 1, b"\xfe\x01\x02\x03\xfe\x04\x05", end=b""),
        _file(2, 1, b"\xfe\x01\x02\x03\xa5", end=b""),
        _file(2, 1, b"\xfe\x01\x02\x03\xff\x01\x02\x03", end=b""),
        _file(3, 1, b"\xfe\x01\x02\x03\x40", end=b""),
    ),
    ids=("short-header", "magic", "rgb-cut", "luma-cut", "rgba-cut", "pixels-missing"),
)
def test_files_pillow_refuses_are_refused(data: bytes) -> None:
    assert _pillow(data) is None
    assert _ours(data) is None


def test_the_header_must_give_the_declared_size() -> None:
    data = _encoded(_palette())
    assert qoi_size(data) == (40, 9)
    with pytest.raises(ValueError, match="declared size"):
        decode_qoi(data, 40, 10)
