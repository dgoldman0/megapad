# One-shot PCM audio output

**Implementation status:** the Python emulator contract and deterministic
capture oracle are implemented. The physical DMA/I2S bridge and its RTL are
not; software must use the capability register and must not assume that an
audible host sink is attached.

MegaPad exposes a buffer-oriented audio device at MMIO offset `0x0C00`.
The first contract is intentionally one-shot: software renders a complete
signed-PCM buffer, configures its format, and submits it once. This avoids
pretending that the serialized emulator can sustain a real-time producer
until that has been measured.

Every successful submission is copied into device-owned host memory before a
playback callback runs. Headless tests can therefore inspect exact bytes
without an audio server, and guest mutation after submission cannot alter the
capture. A host sink is optional and is advertised separately from capture.

## Register map

All multi-byte registers are little-endian.

| Offset | Width | Access | Meaning |
|---:|---:|:---:|---|
| `0x00` | 1 | W | Command: `1` submit, `2` stop, `3` clear |
| `0x01` | 1 | R | Status: busy bit 0, done bit 1, error bit 2, host-playing bit 3, present bit 7 |
| `0x02` | 1 | RW | Format; currently `1` = signed PCM16 little-endian |
| `0x03` | 1 | RW | Channels; currently mono or stereo |
| `0x04` | 4 | RW | Sample rate, 8,000--192,000 Hz |
| `0x08` | 8 | RW | DMA source byte address |
| `0x10` | 4 | RW | Frame count |
| `0x14` | 4 | R | Successful-submission generation |
| `0x18` | 1 | R | Latched error code |
| `0x19` | 1 | R | Capabilities: bit 0 deterministic capture, bit 1 fully managed host sink attached |

A frame contains one signed 16-bit sample per channel. The submitted byte
count is therefore `frames * channels * 2`. The emulator currently limits one
capture to 1 MiB and rejects zero frames, unsupported formats, unsupported
channel counts, oversized transfers, address overflow, and every DMA span not
fully contained in exactly one physical RAM window before reading memory.

## Status and ownership

`SUBMIT` snapshots the complete buffer and increments the generation. `DONE`
remains latched until a successful `CLEAR`; a rejected later descriptor never
destroys the earlier coherent capture or its ownership state.
Failure in an optional playback sink sets `ERROR` but preserves `DONE`, because
the deterministic capture still succeeded. `STOP` asks an attached sink to
release its current voice but retains both the capture and any diagnostic
error. A failed stop conservatively retains the playing state. `CLEAR` first
stops the sink and only clears diagnostics and captured bytes if release
succeeds; it retains the generation counter.

A submit callback that throws is also treated conservatively: it may have
started a voice before failing, so the device retains host-voice ownership
until `STOP` succeeds. An ordinary `False` return is the sink's explicit
guarantee that playback was rejected without leaving a voice active.

Capture and playback are deliberately different capabilities. A headless
machine can accept a submission for deterministic verification even when
nothing can make it audible. Guest interfaces must not label capture-only
operation as playback.

An audible sink must provide submit, stop, and cheap non-blocking playback
state callbacks as one lifecycle contract. Natural completion is observed on
status reads; callback failure is never interpreted as proof of silence.

The emulator currently invokes buffer copying and host submit/stop callbacks
synchronously inside the MMIO command. Guest polling deadlines begin only
after that command returns and therefore are not hard host-wall-clock bounds.
Host adapters must keep these callbacks short; making host dispatch
asynchronous with cancellable ownership is a separate streaming-era hardening
step, not a property this one-shot revision pretends to provide.

Interactive shared sessions may opt into audible playback with
`session_server.py --audio`. The adapter initializes only `pygame.mixer`,
requires the exact requested signed-PCM16 format, owns a single replaceable
voice, and advertises the host-sink capability only after initialization
succeeds. The separate pygame viewer intentionally does not initialize a
mixer, so it cannot accidentally reserve the audio device.

## Hardware direction

The emulator model establishes the software-visible contract and test oracle;
it is not an excuse for an emulator-only BIOS shortcut. A hardware
implementation should connect the same descriptor contract to an I2S or audio
codec bridge and read PCM through a bounded DMA path. Streaming rings,
interrupts, underrun accounting, and input devices belong to later revisions
after one-shot throughput and ownership have been measured.
