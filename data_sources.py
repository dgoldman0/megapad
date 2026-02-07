"""
data_sources.py — External data injection for Megapad-64 NIC

Frame protocol (6-byte header + payload, fits NIC MTU of 1500):
    +0   u8    SRC_ID       source identifier (0-255)
    +1   u8    DTYPE        data type
    +2   u16   SEQ          sequence number (LE)
    +4   u16   PAYLOAD_LEN  payload byte count (LE)
    +6   ...   PAYLOAD      data bytes (up to 1494)

Data types:
    0 = RAW    arbitrary bytes
    1 = U8VEC  vector of unsigned 8-bit values
    2 = U16VEC vector of unsigned 16-bit LE values
    3 = U64VEC vector of unsigned 64-bit LE values
    4 = TEXT   UTF-8 text
    5 = CMD    control / metadata

Usage (emulator):
    from data_sources import SineSource, inject
    src = SineSource(src_id=1, length=64)
    inject(system, src, count=10)        # inject 10 frames

Usage (real hardware — same frames over UDP):
    src = CounterSource(src_id=2, length=64)
    send_udp(src, host='192.168.1.100', port=9000, count=5)
"""

import struct
import math

# --- Data type constants ---
DTYPE_RAW   = 0
DTYPE_U8    = 1
DTYPE_U16   = 2
DTYPE_U64   = 3
DTYPE_TEXT  = 4
DTYPE_CMD   = 5

FRAME_HDR_SIZE = 6
MAX_PAYLOAD    = 1494   # 1500 MTU - 6 header


# ---- Frame encoding / decoding ----

def encode_frame(src_id: int, dtype: int, seq: int,
                 payload: bytes) -> bytes:
    """Encode a data-port frame: 6-byte header + payload."""
    if len(payload) > MAX_PAYLOAD:
        payload = payload[:MAX_PAYLOAD]
    header = struct.pack('<BBHH', src_id & 0xFF, dtype & 0xFF,
                         seq & 0xFFFF, len(payload))
    return header + bytes(payload)


def decode_header(frame: bytes) -> dict:
    """Decode the 6-byte header of a data-port frame."""
    if len(frame) < FRAME_HDR_SIZE:
        raise ValueError("frame too short")
    src_id, dtype, seq, plen = struct.unpack('<BBHH', frame[:6])
    return {'src_id': src_id, 'dtype': dtype, 'seq': seq,
            'payload_len': plen,
            'payload': frame[6:6 + plen]}


# ---- Base class ----

class DataSource:
    """Base data source — override next_frame()."""

    def __init__(self, src_id: int, dtype: int = DTYPE_RAW):
        self.src_id = src_id
        self.dtype = dtype
        self.seq = 0

    def _frame(self, payload: bytes) -> bytes:
        f = encode_frame(self.src_id, self.dtype, self.seq, payload)
        self.seq = (self.seq + 1) & 0xFFFF
        return f

    def next_frame(self) -> bytes:
        raise NotImplementedError


# ---- Concrete sources ----

class SineSource(DataSource):
    """Sine wave → U8 vector (maps to 1-byte buffer elements)."""

    def __init__(self, src_id: int, length: int = 64,
                 frequency: float = 1.0,
                 amplitude: float = 127.0, offset: float = 128.0):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        self.freq = frequency
        self.amp = amplitude
        self.offset = offset
        self.phase = 0.0

    def next_frame(self) -> bytes:
        data = bytearray(self.length)
        for i in range(self.length):
            t = 2.0 * math.pi * self.freq * (self.phase + i) / self.length
            v = self.offset + self.amp * math.sin(t)
            data[i] = max(0, min(255, int(v)))
        self.phase += self.length
        return self._frame(bytes(data))


class RandomSource(DataSource):
    """Pseudo-random bytes — useful for noise injection."""

    def __init__(self, src_id: int, length: int = 64, seed: int = 42):
        super().__init__(src_id, DTYPE_RAW)
        self.length = length
        import random
        self.rng = random.Random(seed)

    def next_frame(self) -> bytes:
        data = bytes(self.rng.randint(0, 255) for _ in range(self.length))
        return self._frame(data)


class CounterSource(DataSource):
    """Incrementing byte counter — easy to verify in tests."""

    def __init__(self, src_id: int, length: int = 64):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        self.counter = 0

    def next_frame(self) -> bytes:
        data = bytearray(self.length)
        for i in range(self.length):
            data[i] = (self.counter + i) & 0xFF
        self.counter = (self.counter + self.length) & 0xFFFF
        return self._frame(bytes(data))


class ReplaySource(DataSource):
    """Replay a fixed byte sequence in chunks."""

    def __init__(self, src_id: int, data: bytes,
                 chunk_size: int = 64, dtype: int = DTYPE_RAW):
        super().__init__(src_id, dtype)
        self.data = bytes(data)
        self.chunk_size = chunk_size
        self._pos = 0

    def next_frame(self) -> bytes:
        end = min(self._pos + self.chunk_size, len(self.data))
        chunk = self.data[self._pos:end]
        self._pos = end
        if len(chunk) < self.chunk_size:
            chunk = chunk + b'\x00' * (self.chunk_size - len(chunk))
        return self._frame(chunk)

    @property
    def exhausted(self) -> bool:
        return self._pos >= len(self.data)


class CSVSource(DataSource):
    """Read a CSV column as U8 vector frames."""

    def __init__(self, src_id: int, filepath: str,
                 column: int = 0, chunk_size: int = 64,
                 has_header: bool = True,
                 scale: float = 1.0, offset: float = 0.0):
        super().__init__(src_id, DTYPE_U8)
        self.chunk_size = chunk_size
        self._values = self._load(filepath, column, has_header,
                                  scale, offset)
        self._pos = 0

    @staticmethod
    def _load(filepath, column, has_header, scale, offset):
        import csv
        values = []
        with open(filepath) as f:
            reader = csv.reader(f)
            if has_header:
                next(reader, None)
            for row in reader:
                try:
                    v = float(row[column]) * scale + offset
                    values.append(max(0, min(255, int(v))))
                except (IndexError, ValueError):
                    pass
        return values

    def next_frame(self) -> bytes:
        end = min(self._pos + self.chunk_size, len(self._values))
        chunk = self._values[self._pos:end]
        self._pos = end
        if len(chunk) < self.chunk_size:
            chunk = chunk + [0] * (self.chunk_size - len(chunk))
        return self._frame(bytes(chunk))

    @property
    def exhausted(self) -> bool:
        return self._pos >= len(self._values)


# ---- Injection helpers ----

def inject(system, source: DataSource, count: int = 1):
    """Inject *count* frames from *source* into system NIC rx queue."""
    for _ in range(count):
        frame = source.next_frame()
        system.nic.inject_frame(frame)


def inject_raw(system, src_id: int, payload: bytes,
               dtype: int = DTYPE_U8, seq: int = 0):
    """Inject a single hand-crafted frame."""
    frame = encode_frame(src_id, dtype, seq, payload)
    system.nic.inject_frame(frame)


def send_udp(source: DataSource, host: str, port: int,
             count: int = 1, delay: float = 0.0):
    """Send frames as UDP datagrams — bridges to real hardware."""
    import socket
    import time
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        for _ in range(count):
            frame = source.next_frame()
            sock.sendto(frame, (host, port))
            if delay > 0:
                time.sleep(delay)
    finally:
        sock.close()
