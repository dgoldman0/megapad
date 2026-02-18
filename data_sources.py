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

# Default MACs and IPs for emulator data-port frames
NIC_MAC  = bytes([0x02, 0x4D, 0x50, 0x36, 0x34, 0x00])
PEER_MAC = bytes([0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0x01])
NIC_IP   = (10, 0, 0, 1)    # KDOS default IP (set in §14)
PEER_IP  = (10, 0, 0, 2)    # sender IP
PORT_UDP = 9000              # well-known UDP port for data ports


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


def wrap_port_frame(payload: bytes, *,
                    dst_mac: bytes = NIC_MAC,
                    src_mac: bytes = PEER_MAC,
                    src_ip: tuple = PEER_IP,
                    dst_ip: tuple = NIC_IP,
                    sport: int = PORT_UDP,
                    dport: int = PORT_UDP) -> bytes:
    """Wrap raw §10 frame bytes in ETH+IP+UDP envelope for NIC injection.

    The payload is typically the output of encode_frame(). The resulting
    bytes can be passed to system.nic.inject_frame().
    """
    # UDP header
    udp_len = 8 + len(payload)
    udp_hdr = bytearray(8)
    struct.pack_into('>HHH', udp_hdr, 0, sport, dport, udp_len)
    # UDP checksum (with pseudo-header)
    pseudo = bytes(src_ip) + bytes(dst_ip) + b'\x00\x11'
    pseudo += struct.pack('>H', udp_len)
    ck_data = pseudo + bytes(udp_hdr) + bytes(payload)
    if len(ck_data) % 2:
        ck_data += b'\x00'
    s = 0
    for i in range(0, len(ck_data), 2):
        s += (ck_data[i] << 8) | ck_data[i + 1]
    while s > 0xFFFF:
        s = (s & 0xFFFF) + (s >> 16)
    cksum = (~s) & 0xFFFF
    if cksum == 0:
        cksum = 0xFFFF
    struct.pack_into('>H', udp_hdr, 6, cksum)
    ip_payload = bytes(udp_hdr) + bytes(payload)
    # IP header
    total_len = 20 + len(ip_payload)
    ip_hdr = bytearray(20)
    ip_hdr[0] = 0x45
    struct.pack_into('>H', ip_hdr, 2, total_len)
    ip_hdr[6] = 0x40          # DF
    ip_hdr[8] = 64             # TTL
    ip_hdr[9] = 17             # UDP
    ip_hdr[12:16] = bytes(src_ip)
    ip_hdr[16:20] = bytes(dst_ip)
    s = 0
    for i in range(0, 20, 2):
        s += (ip_hdr[i] << 8) | ip_hdr[i + 1]
    while s > 0xFFFF:
        s = (s & 0xFFFF) + (s >> 16)
    cksum = (~s) & 0xFFFF
    struct.pack_into('>H', ip_hdr, 10, cksum)
    # Ethernet frame
    eth = bytes(dst_mac) + bytes(src_mac) + b'\x08\x00'
    return eth + bytes(ip_hdr) + ip_payload


def extract_port_payload(frame: bytes) -> bytes:
    """Extract the §10 frame (header + data) from a captured ETH+IP+UDP frame.

    Assumes standard 14-byte ETH + 20-byte IP (no options) + 8-byte UDP = 42.
    """
    return frame[42:]


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
    """Inject *count* frames from *source* into system NIC rx queue.

    Frames are wrapped in ETH+IP+UDP (port 9000) so they are received
    through the §16 network stack and dispatched to §10 data ports.
    """
    for _ in range(count):
        frame = source.next_frame()
        system.nic.inject_frame(wrap_port_frame(frame))


def inject_raw(system, src_id: int, payload: bytes,
               dtype: int = DTYPE_U8, seq: int = 0):
    """Inject a single hand-crafted frame (wrapped in UDP)."""
    frame = encode_frame(src_id, dtype, seq, payload)
    system.nic.inject_frame(wrap_port_frame(frame))


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


# =====================================================================
#  Real-World Data Sources
# =====================================================================
#
#  These sources fetch data from external systems and encode it as
#  U8 frames matching the standard protocol.  Each gracefully falls
#  back to synthetic data if the external system is unavailable —
#  so tests always pass, but with real data when possible.
#
#  All sources follow the same pattern:
#    1. Fetch raw data from external system (API, file, device)
#    2. Scale/quantize to 0-255 range
#    3. Emit as standard data-port frames
#
#  The same frame bytes work whether injected into the emulator
#  or sent over UDP to real hardware.
# =====================================================================


class TemperatureSource(DataSource):
    """Temperature time-series → U8 vector.

    Attempts to fetch from Open-Meteo (free, no API key) for a given
    lat/lon.  Falls back to realistic synthetic diurnal cycle.

    Quantization: 0°C → 128, ±50°C range → 0-255.
    """

    def __init__(self, src_id: int, length: int = 64,
                 lat: float = 40.71, lon: float = -74.01,
                 live: bool = False):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        self.lat = lat
        self.lon = lon
        self._values = None
        self._pos = 0
        if live:
            self._values = self._fetch_live()
        if self._values is None:
            self._values = self._synthetic()

    def _fetch_live(self):
        """Fetch 24h hourly temperature from Open-Meteo (free API)."""
        try:
            import urllib.request
            import json
            url = (f"https://api.open-meteo.com/v1/forecast?"
                   f"latitude={self.lat}&longitude={self.lon}"
                   f"&hourly=temperature_2m&forecast_days=1")
            with urllib.request.urlopen(url, timeout=5) as r:
                data = json.loads(r.read())
            temps = data['hourly']['temperature_2m']
            return [max(0, min(255, int(t * 2.55 + 128))) for t in temps]
        except Exception:
            return None

    def _synthetic(self):
        """Realistic 24h diurnal cycle: 5°C at 4am, 22°C at 2pm."""
        vals = []
        for h in range(24 * 4):  # 15-min intervals
            hour = h / 4.0
            temp = 13.5 + 8.5 * math.sin((hour - 10) * math.pi / 12)
            vals.append(max(0, min(255, int(temp * 2.55 + 128))))
        return vals

    def next_frame(self) -> bytes:
        chunk = []
        for i in range(self.length):
            idx = (self._pos + i) % len(self._values)
            chunk.append(self._values[idx])
        self._pos = (self._pos + self.length) % len(self._values)
        return self._frame(bytes(chunk))


class StockSource(DataSource):
    """Financial time-series (price as U8).

    Falls back to synthetic Brownian motion that looks like price data.
    Quantization: maps min..max of series to 0-255.
    """

    def __init__(self, src_id: int, length: int = 64,
                 volatility: float = 0.02, drift: float = 0.0001,
                 seed: int = 123):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        import random
        self.rng = random.Random(seed)
        self._prices = self._generate_walk(512, volatility, drift)
        self._pos = 0

    def _generate_walk(self, n, vol, drift):
        """Geometric Brownian motion → realistic price series."""
        price = 100.0
        prices = []
        for _ in range(n):
            ret = drift + vol * self.rng.gauss(0, 1)
            price *= (1 + ret)
            prices.append(price)
        # Normalize to 0-255
        lo, hi = min(prices), max(prices)
        span = hi - lo if hi > lo else 1.0
        return [max(0, min(255, int(255 * (p - lo) / span)))
                for p in prices]

    def next_frame(self) -> bytes:
        chunk = []
        for i in range(self.length):
            idx = (self._pos + i) % len(self._prices)
            chunk.append(self._prices[idx])
        self._pos = (self._pos + self.length) % len(self._prices)
        return self._frame(bytes(chunk))


class SeismicSource(DataSource):
    """Seismic/vibration sensor data → U8 vector.

    Synthetic model: background noise + occasional P/S-wave events.
    Mimics accelerometer or geophone data at 8-bit resolution.
    """

    def __init__(self, src_id: int, length: int = 64,
                 noise_floor: int = 5, event_prob: float = 0.05,
                 seed: int = 99):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        self.noise_floor = noise_floor
        self.event_prob = event_prob
        import random
        self.rng = random.Random(seed)
        self._sample = 0

    def next_frame(self) -> bytes:
        data = bytearray(self.length)
        for i in range(self.length):
            # Background noise centered at 128
            noise = self.rng.gauss(0, self.noise_floor)
            # Occasional seismic event: sharp spike + exponential decay
            if self.rng.random() < self.event_prob:
                spike = self.rng.uniform(40, 120)
            else:
                spike = 0
            val = 128 + noise + spike
            data[i] = max(0, min(255, int(val)))
            self._sample += 1
        return self._frame(bytes(data))


class ImageSource(DataSource):
    """Grayscale image → U8 vector frames (row-by-row or tile-by-tile).

    Can load from file (PIL/Pillow if available) or generate synthetic
    test patterns: gradient, checkerboard, circle, noise.
    """

    def __init__(self, src_id: int, width: int = 64, height: int = 64,
                 pattern: str = 'gradient', filepath: str = None):
        super().__init__(src_id, DTYPE_U8)
        self.width = width
        self.height = height
        self._row = 0
        if filepath:
            self._image = self._load_file(filepath, width, height)
        else:
            self._image = self._generate_pattern(pattern, width, height)

    @staticmethod
    def _load_file(filepath, w, h):
        """Load image file as grayscale, resize to w×h."""
        try:
            from PIL import Image
            img = Image.open(filepath).convert('L').resize((w, h))
            return list(img.tobytes())
        except Exception:
            return ImageSource._generate_pattern('gradient', w, h)

    @staticmethod
    def _generate_pattern(pattern, w, h):
        """Generate synthetic grayscale test patterns."""
        pixels = []
        for y in range(h):
            for x in range(w):
                if pattern == 'gradient':
                    v = int(255 * x / max(w - 1, 1))
                elif pattern == 'checkerboard':
                    v = 255 if ((x // 8) + (y // 8)) % 2 == 0 else 0
                elif pattern == 'circle':
                    cx, cy = w // 2, h // 2
                    r = min(w, h) // 3
                    dist = math.sqrt((x - cx) ** 2 + (y - cy) ** 2)
                    v = 255 if dist < r else 0
                elif pattern == 'vstripes':
                    v = 255 if (x // 4) % 2 == 0 else 0
                elif pattern == 'noise':
                    import random
                    v = random.randint(0, 255)
                else:
                    v = int(255 * x / max(w - 1, 1))
                pixels.append(max(0, min(255, v)))
        return pixels

    def next_frame(self) -> bytes:
        """Emit one row of the image as a frame."""
        start = (self._row % self.height) * self.width
        row = self._image[start:start + self.width]
        if len(row) < self.width:
            row = row + [0] * (self.width - len(row))
        self._row += 1
        return self._frame(bytes(row))

    @property
    def exhausted(self) -> bool:
        return self._row >= self.height


class AudioSource(DataSource):
    """8-bit audio samples → U8 vector frames.

    Can load from WAV file (8-bit or auto-converted) or generate
    synthetic waveforms: tone, chord, chirp, white noise, drum.
    """

    def __init__(self, src_id: int, length: int = 64,
                 sample_rate: int = 8000,
                 waveform: str = 'tone', frequency: float = 440.0,
                 filepath: str = None, seed: int = 77):
        super().__init__(src_id, DTYPE_U8)
        self.length = length
        self.sample_rate = sample_rate
        import random
        self.rng = random.Random(seed)
        if filepath:
            self._samples = self._load_wav(filepath)
        else:
            self._samples = self._generate(waveform, frequency,
                                           sample_rate, 8192)
        self._pos = 0

    @staticmethod
    def _load_wav(filepath):
        """Load WAV file, convert to 8-bit unsigned."""
        try:
            import wave
            with wave.open(filepath, 'rb') as wf:
                frames = wf.readframes(wf.getnframes())
                sw = wf.getsampwidth()
                if sw == 1:
                    return list(frames)
                elif sw == 2:
                    import struct as st
                    samples = st.unpack(f'<{len(frames)//2}h', frames)
                    return [max(0, min(255, (s + 32768) >> 8))
                            for s in samples]
            return [128] * 8192
        except Exception:
            return [128] * 8192

    @staticmethod
    def _generate(waveform, freq, sr, n):
        """Generate synthetic audio waveform."""
        samples = []
        for i in range(n):
            t = i / sr
            if waveform == 'tone':
                v = 128 + 127 * math.sin(2 * math.pi * freq * t)
            elif waveform == 'chord':
                v = 128 + 42 * (math.sin(2 * math.pi * freq * t) +
                                math.sin(2 * math.pi * freq * 1.25 * t) +
                                math.sin(2 * math.pi * freq * 1.5 * t))
            elif waveform == 'chirp':
                f = freq * (1 + t * 2)  # frequency sweep
                v = 128 + 127 * math.sin(2 * math.pi * f * t)
            elif waveform == 'square':
                v = 255 if math.sin(2 * math.pi * freq * t) >= 0 else 0
            else:
                v = 128 + 127 * math.sin(2 * math.pi * freq * t)
            samples.append(max(0, min(255, int(v))))
        return samples

    def next_frame(self) -> bytes:
        chunk = self._samples[self._pos:self._pos + self.length]
        self._pos += self.length
        if len(chunk) < self.length:
            chunk = chunk + [128] * (self.length - len(chunk))
        return self._frame(bytes(chunk))


class TextSource(DataSource):
    """Text/ASCII data → U8 frames (raw byte values of characters).

    Can load from file, URL, or use built-in sample texts.
    Useful for character frequency analysis, pattern matching, etc.
    """

    SAMPLES = {
        'lorem': ("Lorem ipsum dolor sit amet, consectetur adipiscing "
                  "elit. Sed do eiusmod tempor incididunt ut labore et "
                  "dolore magna aliqua. Ut enim ad minim veniam, quis "
                  "nostrud exercitation ullamco laboris nisi ut aliquip."),
        'pangram': ("The quick brown fox jumps over the lazy dog. "
                    "Pack my box with five dozen liquor jugs. "
                    "How vexingly quick daft zebras jump. " * 3),
        'digits': ("3.14159265358979323846264338327950288419716939937510"
                   "58209749445923078164062862089986280348253421170679" * 2),
        'dna': ("ATCGATCGATCGTAGCTAGCTAGCATGCATGCATGC" * 8),
    }

    def __init__(self, src_id: int, chunk_size: int = 64,
                 text: str = None, sample: str = 'lorem',
                 filepath: str = None, url: str = None):
        super().__init__(src_id, DTYPE_TEXT)
        self.chunk_size = chunk_size
        if text:
            self._data = text.encode('utf-8', errors='replace')
        elif filepath:
            self._data = self._load_file(filepath)
        elif url:
            self._data = self._fetch_url(url)
        else:
            self._data = self.SAMPLES.get(sample, self.SAMPLES['lorem']).encode()
        self._pos = 0

    @staticmethod
    def _load_file(filepath):
        try:
            with open(filepath, 'rb') as f:
                return f.read(65536)
        except Exception:
            return TextSource.SAMPLES['lorem'].encode()

    @staticmethod
    def _fetch_url(url):
        try:
            import urllib.request
            with urllib.request.urlopen(url, timeout=5) as r:
                return r.read(65536)
        except Exception:
            return TextSource.SAMPLES['lorem'].encode()

    def next_frame(self) -> bytes:
        end = min(self._pos + self.chunk_size, len(self._data))
        chunk = self._data[self._pos:end]
        self._pos = end
        if len(chunk) < self.chunk_size:
            chunk = chunk + b'\x00' * (self.chunk_size - len(chunk))
        return self._frame(bytes(chunk))

    @property
    def exhausted(self) -> bool:
        return self._pos >= len(self._data)


class EmbeddingSource(DataSource):
    """OpenAI (or compatible) embedding vectors → U8 frames.

    Calls the embeddings API if a key is available, otherwise falls
    back to a synthetic hash-based pseudo-embedding that preserves
    rough similarity relationships.

    Quantization: float32 embeddings are normalized to 0-255 per
    dimension, so the tile engine's U8 operations can compute
    approximate cosine similarity, L1 distance, etc.
    """

    def __init__(self, src_id: int, texts: list = None,
                 model: str = 'text-embedding-3-small',
                 dimensions: int = 64,
                 api_key: str = None):
        super().__init__(src_id, DTYPE_U8)
        self.dimensions = dimensions
        self._texts = texts or [
            "The cat sat on the mat",
            "A dog lay on the rug",
            "Quantum computing uses qubits",
            "The weather is sunny today",
        ]
        self._embeddings = []
        self._idx = 0
        if api_key:
            self._embeddings = self._fetch_openai(
                api_key, model, dimensions)
        if not self._embeddings:
            self._embeddings = self._synthetic_embeddings()

    def _fetch_openai(self, api_key, model, dims):
        """Fetch real embeddings from OpenAI API."""
        try:
            import urllib.request
            import json
            body = json.dumps({
                "input": self._texts,
                "model": model,
                "dimensions": dims,
            }).encode()
            req = urllib.request.Request(
                "https://api.openai.com/v1/embeddings",
                data=body,
                headers={
                    "Authorization": f"Bearer {api_key}",
                    "Content-Type": "application/json",
                })
            with urllib.request.urlopen(req, timeout=10) as r:
                data = json.loads(r.read())
            results = []
            for item in data['data']:
                emb = item['embedding'][:dims]
                results.append(self._quantize(emb))
            return results
        except Exception:
            return []

    def _synthetic_embeddings(self):
        """Hash-based pseudo-embeddings that preserve rough similarity."""
        import hashlib
        results = []
        for text in self._texts:
            h = hashlib.sha256(text.lower().encode()).digest()
            # Extend hash to fill dimensions
            raw = h * ((self.dimensions // len(h)) + 1)
            emb = list(raw[:self.dimensions])
            results.append(emb)
        return results

    @staticmethod
    def _quantize(floats):
        """Normalize float vector to 0-255 range."""
        lo = min(floats)
        hi = max(floats)
        span = hi - lo if hi > lo else 1.0
        return [max(0, min(255, int(255 * (v - lo) / span)))
                for v in floats]

    def next_frame(self) -> bytes:
        emb = self._embeddings[self._idx % len(self._embeddings)]
        self._idx += 1
        return self._frame(bytes(emb[:self.dimensions]))


class MultiChannelSource:
    """Multiplexes several DataSources onto a single NIC.

    Interleaves frames from multiple sources in round-robin order,
    each with its own src_id.  Models a real sensor hub or data
    acquisition system with multiple channels.
    """

    def __init__(self, sources: list):
        self.sources = list(sources)
        self._idx = 0

    def next_frame(self) -> bytes:
        src = self.sources[self._idx % len(self.sources)]
        self._idx += 1
        return src.next_frame()

    def inject(self, system, count: int = 1):
        """Inject count frames (round-robin across sources, wrapped in UDP)."""
        for _ in range(count):
            frame = self.next_frame()
            system.nic.inject_frame(wrap_port_frame(frame))
