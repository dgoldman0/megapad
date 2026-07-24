#!/usr/bin/env python3
"""Bounded Phase 0 baseline for the legacy MegaPad system runner.

This benchmark is deliberately diagnostic rather than aspirational.  It
records what the current ``MegapadSystem.run_batch()`` path does before the
deterministic concurrency scheduler replaces it.

The report keeps four quantities separate:

* returned aggregate instructions (the legacy ``run_batch`` result);
* exact per-core instructions from a benchmark-only accounting replay;
* per-core architectural cycle-counter deltas; and
* arguments passed to ``DeviceBus.tick()``.

The native owner now contains a system clock, but the legacy runner does not
advance it yet.  ``virtual_system_cycles`` therefore remains JSON ``null``.
In particular, neither the sum nor the maximum of the per-core cycle counters
is silently relabelled as system time.

Default coverage:

* 1, 2, and 4 full cores;
* private register/ALU work;
* same-address shared-memory pressure;
* mixed native and Python-dispatched MMIO polling;
* periodic timer interrupts; and
* legacy sequential storage/display orchestration around guest VRAM writes.

Examples::

    python3 bench_phase0_concurrency.py --quick --json
    python3 bench_phase0_concurrency.py --instructions 2m \
        --output /tmp/megapad-phase0.json
    python3 bench_phase0_concurrency.py --cores 4 \
        --scenarios private_compute,shared_memory --repeats 5

The storage/display case uses a temporary deterministic disk image and a
headless native RGB conversion.  Its direct ``Storage.write8()`` commands
bypass ``DeviceBus`` and complete before each CPU batch; rendering happens
after the batch.  It is therefore a synchronous legacy orchestration baseline,
not a claim of DMA/display/core overlap.
"""

from __future__ import annotations

import argparse
import gc
import hashlib
import importlib.metadata
import json
import os
import platform
import statistics
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable, Iterable

import _mp64_accel
from accel_wrapper import ACCEL_AVAILABLE
from asm import assemble
from devices import (
    MMIO_BASE,
    STORAGE_CMD_READ,
    STORAGE_RESULT_OK,
    SYSINFO_BASE,
    TIMER_BASE,
)
from megapad64 import IVEC_TIMER
from system import MegapadSystem, VRAM_BASE


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.phase0-concurrency-baseline"
SCHEMA_VERSION = 2
STATE_SCHEMA = "megapad.phase0-canonical-state"
STATE_SCHEMA_VERSION = 3

RAM_SIZE = 1 << 20
CODE_BASE = 0x1000
IVT_BASE = 0x2000
SHARED_DATA_BASE = 0x40000
DMA_DATA_BASE = 0x60000
FRAME_WIDTH = 128
FRAME_HEIGHT = 96
FRAME_STRIDE = FRAME_WIDTH
FRAME_BYTES = FRAME_STRIDE * FRAME_HEIGHT

DETERMINISTIC_RTC_EPOCH_MS = 946_684_800_000  # 2000-01-01T00:00:00Z

STATE_COMPARISON_SCOPE = {
    "oracle": (
        "SHA-256 of canonical JSON containing exactly the state enumerated "
        "under included plus deterministic workload counters"
    ),
    "interpretation_limit": (
        "Oracle equality proves equality only for the captured scope. It is "
        "not proof that unbound native state listed under explicit_exclusions "
        "is equal."
    ),
    "native_audit_basis": (
        "CPUState, SystemState, and their owned or borrowed device structs in "
        "accel/mp64_accel.cpp and accel/mp64_*.h; native fields without "
        "non-destructive pybind readback are exclusions even when they affect "
        "future execution"
    ),
    "included": [
        "all 32 GPRs and every scalar CPUState field exposed by the binding",
        "per-core interrupt-line state and Python-reserved tstride_c",
        "native port output/map state and accelerator hook count",
        "complete shared, HBW, external, and VRAM byte regions via size and "
        "SHA-256",
        "all scalar and buffer state of shared Python devices, with large "
        "buffers and ordered frame queues represented by stable hashes",
        "native timer, framebuffer (including palette), RTC, UART geometry, "
        "UART, crypto-MMIO-visible, NIC-MMIO-visible, and TRNG enable state "
        "for every full core, including secondary cores",
        "native system-cycle and event-horizon state, registered-device "
        "layout, platform topology, and benchmark orchestration counters",
    ],
    "explicit_exclusions": [
        {
            "state": "native input-port latch values",
            "reason": "the binding permits writes but exposes no readback",
        },
        {
            "state": "native CPUState::tstride_c",
            "reason": (
                "the field exists in native CPUState but is neither bound nor "
                "read by the current native executor; the captured "
                "Megapad64.tstride_c value is a separate Python-side "
                "compatibility field and cannot observe it"
            ),
        },
        {
            "state": (
                "native Field-ALU gf_custom_p, gf_mont_pinv, gf_prev_lo, "
                "and gf_prev_hi"
            ),
            "reason": (
                "these custom-prime, Montgomery, and previous-result BigNum "
                "fields affect future EXT.CRYPTO Field-ALU operations but the "
                "binding exposes only gf_prime_sel"
            ),
        },
        {
            "state": "native EXT.DICT CPUState::dict_table entries",
            "reason": (
                "the architecturally active 64-set, four-way dictionary "
                "cache can only be cleared through the binding, not read"
            ),
        },
        {
            "state": (
                "native UART queued payload bytes and native NIC queued frame "
                "payload bytes"
            ),
            "reason": (
                "only queue depth/status is exposed non-destructively; drain "
                "APIs would mutate the measured state"
            ),
        },
        {
            "state": "native UART tx_ring_addr_bytes partial-write assembly",
            "reason": (
                "the resolved TX ring base and safe registers are captured, "
                "but an in-progress byte-serial base write is not bound"
            ),
        },
        {
            "state": "native UART geometry request_generation",
            "reason": (
                "the visible geometry registers and pending request are "
                "captured, but the unbound host-side stale-ack token affects "
                "future conditional resize completion"
            ),
        },
        {
            "state": (
                "native NIC dma_push_ctr and data_window_valid bitset"
            ),
            "reason": (
                "the complete readable NIC register window, queue depths, and "
                "counters are captured, but these future-affecting write "
                "assembly/validity fields are not exposed"
            ),
        },
        {
            "state": (
                "native framebuffer frame_cycles, pal_idx, byte-assembly "
                "buffers, base_push_ctr, and base_push_buf"
            ),
            "reason": (
                "final framebuffer registers, palette, vsync, and vblank are "
                "captured, but these sub-frame and partial-write fields are "
                "not bound"
            ),
        },
        {
            "state": (
                "native AES write-only key/IV/input and running GHASH state; "
                "SHA-3 sponge, absorb, and squeeze state; and WOTS write-only "
                "configuration beyond bytes returned by crypto_read8"
            ),
            "reason": (
                "the oracle hashes all non-destructively MMIO-readable crypto "
                "bytes, but those additional future-affecting internals are "
                "not bound"
            ),
        },
        {
            "state": "native TRNG pool bytes, pool position, and entropy source",
            "reason": (
                "only the native TRNG enable flag is exposed; Phase 0 "
                "workloads do not read the TRNG"
            ),
        },
        {
            "state": (
                "native RTC host_mono_anchor, host_uptime_anchor, and "
                "host_epoch_anchor"
            ),
            "reason": (
                "these realtime-only anchors are not bound; the harness pins "
                "RTC realtime=false, so they are inactive in every Phase 0 "
                "case"
            ),
        },
        {
            "state": "registered accelerator hook addresses",
            "reason": "the binding exposes only the hook count",
        },
        {
            "state": (
                "host callback identities, sockets, backend/thread objects, "
                "temporary storage path, and memory callback identities"
            ),
            "reason": (
                "non-architectural host plumbing is process-specific; the "
                "guest-visible state included by this oracle and all storage "
                "media bytes are captured separately"
            ),
        },
        {
            "state": "micro-core cluster scratchpads and scheduler state",
            "reason": (
                "Phase 0 cases explicitly construct num_clusters=0; topology "
                "is captured and the oracle rejects no hidden cluster presence"
            ),
        },
        {
            "state": "lazy pure-Python fallback CPU mirror contents",
            "reason": (
                "Phase 0 programs stay on the native instruction path; the "
                "oracle captures whether a fallback mirror was instantiated"
            ),
        },
    ],
}

COVERAGE_METADATA = {
    "implemented_cases": [
        {
            "scenario": "private_compute",
            "classification": "diagnostic_baseline",
            "covers": "private register/ALU execution",
            "does_not_claim": "real instruction-cache-pressure coverage",
        },
        {
            "scenario": "shared_memory",
            "classification": "diagnostic_baseline",
            "covers": "same-address shared-memory access pressure",
        },
        {
            "scenario": "mmio_poll",
            "classification": "diagnostic_baseline",
            "covers": "mixed native and Python-dispatched MMIO polling",
        },
        {
            "scenario": "timer_interrupt",
            "classification": "behavior_oracle_and_diagnostic_baseline",
            "covers": "legacy batch-boundary timer interrupt delivery",
        },
        {
            "scenario": "legacy_storage_display_orchestration",
            "classification": "sequential_legacy_diagnostic_only",
            "covers": (
                "direct synchronous storage transfer before CPU execution and "
                "headless framebuffer conversion after CPU execution"
            ),
            "does_not_claim": (
                "DeviceBus-routed storage, NIC DMA, or DMA/display/core overlap"
            ),
        },
    ],
    "deferred_gates": [
        {
            "gate": "real_instruction_cache_pressure",
            "status": "deferred",
            "reason": (
                "the legacy emulator exposes counters/configuration but no "
                "real guest instruction-cache timing model whose pressure can "
                "be validated without production changes"
            ),
        },
        {
            "gate": "nic_dma_with_active_display_overlap",
            "status": "deferred",
            "reason": (
                "the bounded Phase 0 harness has no honest concurrent NIC-DMA "
                "and display execution path; the existing storage/display "
                "case is deliberately sequential"
            ),
        },
    ],
}


PRIVATE_COMPUTE = """
loop:
    add r4, r5
    xor r6, r4
    roli r6, 7
    addi r7, 1
    br loop
"""

SHARED_MEMORY = """
loop:
    st.w r5, r4
    ld.w r6, r5
    add r4, r6
    xori r4, 0x5a
    br loop
"""

MMIO_POLL = """
loop:
    ld.b r6, r5
    ld.b r7, r8
    add r4, r6
    xor r4, r7
    br loop
"""

TIMER_INTERRUPT = """
loop:
    add r4, r5
    xor r6, r4
    br loop

timer_handler:
    inc r12
    st.b r10, r11
    rti
"""

VRAM_WRITER = """
loop:
    st.b r5, r4
    addi r4, 1
    xor r6, r4
    br loop
"""


def parse_count(text: str) -> int:
    """Parse a positive count with optional k/m/g suffix."""
    raw = text.strip().lower().replace("_", "")
    factors = {"k": 1_000, "m": 1_000_000, "g": 1_000_000_000}
    factor = factors.get(raw[-1:], 1)
    if factor != 1:
        raw = raw[:-1]
    try:
        value = float(raw)
        result = int(value * factor)
    except (ValueError, OverflowError) as exc:
        raise argparse.ArgumentTypeError(f"invalid count: {text!r}") from exc
    if result <= 0:
        raise argparse.ArgumentTypeError("count must be positive")
    return result


def parse_positive_int(text: str) -> int:
    try:
        value = int(text)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(f"invalid integer: {text!r}") from exc
    if value <= 0:
        raise argparse.ArgumentTypeError("value must be positive")
    return value


def parse_nonnegative_int(text: str) -> int:
    try:
        value = int(text)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(f"invalid integer: {text!r}") from exc
    if value < 0:
        raise argparse.ArgumentTypeError("value must be non-negative")
    return value


def _git_output(*args: str) -> str | None:
    try:
        completed = subprocess.run(
            ["git", *args],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    return completed.stdout.strip()


def repository_metadata() -> dict:
    status = _git_output("status", "--porcelain")
    return {
        "root": str(ROOT),
        "commit": _git_output("rev-parse", "HEAD"),
        "branch": _git_output("branch", "--show-current"),
        "dirty": None if status is None else bool(status),
    }


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _elf_build_id(path: Path) -> str | None:
    try:
        completed = subprocess.run(
            ["readelf", "-n", str(path)],
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    marker = "Build ID:"
    for line in completed.stdout.splitlines():
        if marker in line:
            value = line.split(marker, 1)[1].strip()
            return value or None
    return None


def accelerator_metadata() -> dict:
    artifact = Path(_mp64_accel.__file__).resolve()
    distribution_versions = {}
    for distribution in ("mp64_accel", "mp64-accel"):
        try:
            distribution_versions[distribution] = (
                importlib.metadata.version(distribution)
            )
        except importlib.metadata.PackageNotFoundError:
            continue
    module_version = getattr(_mp64_accel, "__version__", None)
    build_id = _elf_build_id(artifact)
    return {
        "module": "_mp64_accel",
        "loaded_artifact_path": str(artifact),
        "loaded_artifact_size_bytes": artifact.stat().st_size,
        "loaded_artifact_sha256": _sha256_file(artifact),
        "elf_build_id": build_id,
        "module_version": module_version,
        "distribution_versions": distribution_versions,
        "available_build_identity": {
            "kind": (
                "elf_build_id" if build_id is not None
                else "module_or_distribution_version"
                if module_version is not None or distribution_versions
                else None
            ),
            "value": (
                build_id
                if build_id is not None
                else module_version
                if module_version is not None
                else distribution_versions or None
            ),
        },
    }


def host_metadata() -> dict:
    return {
        "platform": platform.platform(),
        "machine": platform.machine(),
        "processor": platform.processor() or None,
        "logical_cpus": os.cpu_count(),
        "python": platform.python_version(),
        "python_implementation": platform.python_implementation(),
        "executable": sys.executable,
        "accelerator_available": bool(ACCEL_AVAILABLE),
        "accelerator": accelerator_metadata(),
    }


@dataclass
class Execution:
    requested_aggregate_instructions: int
    requested_run_batch_units: int
    returned_aggregate_instructions: int
    run_batch_calls: int

    def as_dict(self) -> dict:
        return {
            "requested_aggregate_instructions":
                self.requested_aggregate_instructions,
            "requested_run_batch_units": self.requested_run_batch_units,
            "returned_aggregate_instructions":
                self.returned_aggregate_instructions,
            "aggregate_instruction_overshoot":
                self.returned_aggregate_instructions
                - self.requested_aggregate_instructions,
            "run_batch_calls": self.run_batch_calls,
        }


@dataclass
class Workload:
    system: MegapadSystem
    batch_request: int | None = None
    before_batch: Callable[[int], None] | None = None
    after_batch: Callable[[int], None] | None = None
    metrics: dict = field(default_factory=dict)
    cleanup: Callable[[], None] = lambda: None

    def execute(self, target_instructions: int) -> Execution:
        returned = 0
        requested_units = 0
        calls = 0
        while returned < target_instructions:
            remaining = target_instructions - returned
            request = remaining
            if self.batch_request is not None:
                request = min(request, self.batch_request)
            if self.before_batch is not None:
                self.before_batch(calls)
            executed = self.system.run_batch(request)
            if executed <= 0:
                raise RuntimeError(
                    "workload stopped before its aggregate instruction budget "
                    f"({returned:,}/{target_instructions:,})"
                )
            returned += executed
            requested_units += request
            calls += 1
            if self.after_batch is not None:
                self.after_batch(calls)
        return Execution(
            requested_aggregate_instructions=target_instructions,
            requested_run_batch_units=requested_units,
            returned_aggregate_instructions=returned,
            run_batch_calls=calls,
        )

    def close(self) -> None:
        self.cleanup()


@dataclass(frozen=True)
class Scenario:
    name: str
    description: str
    limitation: str
    coverage_classification: str
    coverage_claim: str
    build: Callable[[int], Workload]


def _base_system(
    num_cores: int,
    source: str,
    *,
    storage_image: str | None = None,
) -> tuple[MegapadSystem, dict[str, int]]:
    labels: dict[str, int] = {}
    code = assemble(source, base_addr=CODE_BASE, labels_out=labels)
    system = MegapadSystem(
        ram_size=RAM_SIZE,
        storage_image=storage_image,
        num_cores=num_cores,
    )

    # Independent workload instances must not inherit construction-time host
    # clock or terminal state, otherwise replay equality tests the host rather
    # than the emulator.
    rtc = system.rtc
    rtc.realtime = False
    rtc.uptime_ms = 0
    rtc.epoch_ms = DETERMINISTIC_RTC_EPOCH_MS
    rtc.sec = 0
    rtc.min = 0
    rtc.hour = 0
    rtc.day = 1
    rtc.mon = 1
    rtc.year = 2000
    rtc.dow = 6
    rtc.ctrl = 1
    rtc.status = 0
    rtc.alarm_sec = 0
    rtc.alarm_min = 0
    rtc.alarm_hour = 0
    rtc.irq_pending = False
    rtc._ms_prescaler = 0
    rtc._sec_prescaler = 0
    rtc._uptime_latch = 0
    rtc._epoch_latch = 0
    geometry = system.uart_geom
    geometry.cols = 80
    geometry.rows = 24
    geometry.status = 0
    geometry.ctrl = 0
    geometry.req_cols = 0
    geometry.req_rows = 0

    system.load_binary(CODE_BASE, code)
    system.boot(CODE_BASE)
    for core_id, cpu in enumerate(system.cores):
        cpu.pc = CODE_BASE
        cpu.regs[4] = 0x9E37_79B9 ^ (core_id * 0x0101_0101)
        cpu.regs[5] = 0x1000_0001 + core_id * 2
        cpu.regs[6] = 0xD1B5_4A32_D192_ED03 ^ core_id
        cpu.regs[7] = core_id
        cpu.flag_i = 0
        cpu.halted = False
        cpu.idle = False
    return system, labels


def build_private_compute(num_cores: int) -> Workload:
    system, _ = _base_system(num_cores, PRIVATE_COMPUTE)
    return Workload(system)


def build_shared_memory(num_cores: int) -> Workload:
    system, _ = _base_system(num_cores, SHARED_MEMORY)
    for core_id, cpu in enumerate(system.cores):
        cpu.regs[4] = 0x1020_3040 ^ core_id
        cpu.regs[5] = SHARED_DATA_BASE
    return Workload(system)


def build_mmio_poll(num_cores: int) -> Workload:
    system, _ = _base_system(num_cores, MMIO_POLL)
    timer_count_low = MMIO_BASE + TIMER_BASE
    system_info_num_cores = MMIO_BASE + SYSINFO_BASE + 0x10
    system.timer.counter = 0
    system.timer.control = 1
    for cpu in system.cores:
        cpu.regs[5] = timer_count_low
        cpu.regs[8] = system_info_num_cores
    return Workload(system)


def build_timer_interrupt(num_cores: int) -> Workload:
    system, labels = _base_system(num_cores, TIMER_INTERRUPT)
    handler = labels["timer_handler"]
    vector_addr = IVT_BASE + IVEC_TIMER * 8
    system.load_binary(vector_addr, handler.to_bytes(8, "little"))
    timer_status = MMIO_BASE + TIMER_BASE + 0x09
    for cpu in system.cores:
        cpu.ivt_base = IVT_BASE
        cpu.flag_i = 1
        cpu.regs[10] = timer_status
        cpu.regs[11] = 1
        cpu.regs[12] = 0
    system.timer.counter = 0
    system.timer.compare = 1_000
    system.timer.status = 0
    system.timer.irq_pending = False
    system.timer.control = 0b111
    return Workload(system, batch_request=1_000)


def build_legacy_storage_display_orchestration(
    num_cores: int,
) -> Workload:
    temp_dir = tempfile.TemporaryDirectory(prefix="megapad_phase0_")
    image_path = Path(temp_dir.name) / "phase0-storage.img"
    sector_zero = bytes(index & 0xFF for index in range(512))
    sector_one = bytes((index * 73 + 41) & 0xFF for index in range(512))
    image_path.write_bytes(sector_zero + sector_one)

    system, _ = _base_system(
        num_cores,
        VRAM_WRITER,
        storage_image=str(image_path),
    )
    for core_id, cpu in enumerate(system.cores):
        cpu.regs[4] = 17 + core_id * 29
        cpu.regs[5] = VRAM_BASE + core_id * 64

    system.fb.fb_base = VRAM_BASE
    system.fb.width = FRAME_WIDTH
    system.fb.height = FRAME_HEIGHT
    system.fb.stride = FRAME_STRIDE
    system.fb.mode = 0
    system.fb.enable = 1
    system.fb.cycles_per_frame = 10_000

    render = getattr(system.fb._cs, "render_fb_rgb", None)
    if render is not None:
        # The first call lazily initializes NumPy through pybind11.  Keep that
        # one-time process setup outside the timed region so scenario ordering
        # cannot dominate the 1-core display sample.
        preflight = render()
        expected_shape = (FRAME_WIDTH, FRAME_HEIGHT, 3)
        if preflight is None or tuple(preflight.shape) != expected_shape:
            temp_dir.cleanup()
            raise RuntimeError("native framebuffer render preflight failed")

    metrics = {
        "orchestration": "synchronous_sequential_legacy",
        "direct_storage_commands": 0,
        "direct_storage_bytes": 0,
        "sequential_display_renders": 0,
        "display_render_path":
            "native_rgb_conversion" if render is not None
            else "raw_vram_snapshot",
        "display_render_failures": 0,
        "storage_command_path":
            "direct_Storage.write8_bypasses_DeviceBus",
        "device_bus_routed_storage_commands": False,
        "storage_transfer_overlaps_cpu_execution": False,
        "display_render_overlaps_cpu_execution": False,
        "storage_and_display_overlap": False,
        "disk_sector_sha256": [
            hashlib.sha256(sector_zero).hexdigest(),
            hashlib.sha256(sector_one).hexdigest(),
        ],
        "disk_sectors_are_distinct": sector_zero != sector_one,
        "storage_sector_numbers_issued": [],
        "storage_sector_request_counts": [0, 0],
        "both_distinct_disk_sectors_exercised": False,
    }

    def issue_direct_storage_transfer(call_index: int) -> None:
        sector = call_index & 1
        system.storage.sector_num = sector
        system.storage.dma_addr = DMA_DATA_BASE + sector * 512
        system.storage.sec_count = 1
        system.storage.write8(0x00, STORAGE_CMD_READ)
        if system.storage.result != STORAGE_RESULT_OK:
            raise RuntimeError(
                "direct synchronous storage transfer failed with result "
                f"{system.storage.result:#x}"
            )
        metrics["direct_storage_commands"] += 1
        metrics["direct_storage_bytes"] += 512
        metrics["storage_sector_numbers_issued"].append(sector)
        metrics["storage_sector_request_counts"][sector] += 1
        metrics["both_distinct_disk_sectors_exercised"] = all(
            metrics["storage_sector_request_counts"]
        )

    def render_frame(_calls: int) -> None:
        if render is None:
            bytes(system._vram_mem[:FRAME_BYTES])
        else:
            result = render()
            if result is None:
                metrics["display_render_failures"] += 1
                raise RuntimeError("native framebuffer render returned None")
            expected_shape = (FRAME_WIDTH, FRAME_HEIGHT, 3)
            if tuple(result.shape) != expected_shape:
                metrics["display_render_failures"] += 1
                raise RuntimeError(
                    "unexpected framebuffer shape "
                    f"{tuple(result.shape)!r}, expected {expected_shape!r}"
                )
        metrics["sequential_display_renders"] += 1

    return Workload(
        system,
        # The bounded --quick profile targets 100,000 aggregate
        # instructions. Two 50,000-unit orchestration batches guarantee that
        # it reads both deterministic sectors (0 then 1) while remaining
        # small enough for a smoke benchmark.
        batch_request=50_000,
        before_batch=issue_direct_storage_transfer,
        after_batch=render_frame,
        metrics=metrics,
        cleanup=temp_dir.cleanup,
    )


SCENARIOS = {
    scenario.name: scenario
    for scenario in (
        Scenario(
            "private_compute",
            "Register/ALU loop with no data-memory or MMIO operations.",
            "Instruction fetch still reads the shared code image; the legacy "
            "emulator has no guest instruction-cache timing model.",
            "diagnostic_baseline",
            "private register/ALU execution only",
            build_private_compute,
        ),
        Scenario(
            "shared_memory",
            "All cores repeatedly write and read the same shared RAM word.",
            "The legacy runner serializes cores and does not model main-bus "
            "contention, so this records overhead rather than bus bandwidth.",
            "diagnostic_baseline",
            "same-address shared-memory access pressure",
            build_shared_memory,
        ),
        Scenario(
            "mmio_poll",
            "Each loop polls native Timer MMIO and Python SystemInfo MMIO.",
            "Timer state is one native SystemState singleton, while SystemInfo "
            "falls through to the shared Python DeviceBus.",
            "diagnostic_baseline",
            "mixed native and Python-dispatched MMIO polling",
            build_mmio_poll,
        ),
        Scenario(
            "timer_interrupt",
            "Auto-reload timer IRQ with one 1,000-unit run_batch request per "
            "delivery horizon.",
            "Interrupts are delivered only at legacy batch/round boundaries; "
            "this is intentionally a baseline for that artifact.",
            "behavior_oracle_and_diagnostic_baseline",
            "legacy batch-boundary timer interrupt delivery",
            build_timer_interrupt,
        ),
        Scenario(
            "legacy_storage_display_orchestration",
            "Sequential legacy orchestration: a direct one-sector Storage "
            "read completes before each CPU batch, then headless framebuffer "
            "conversion runs after that batch.",
            "Direct Storage.write8 calls bypass DeviceBus. Storage transfer, "
            "CPU execution, and rendering do not overlap; this is not a DMA "
            "or concurrent-display performance claim.",
            "sequential_legacy_diagnostic_only",
            "synchronous storage/display orchestration with no overlap",
            build_legacy_storage_display_orchestration,
        ),
    )
}


def _canonical_json_bytes(value: object) -> bytes:
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("ascii")


def _json_sha256(value: object) -> str:
    return hashlib.sha256(_canonical_json_bytes(value)).hexdigest()


def _blob_summary(data: bytes | bytearray | memoryview) -> dict:
    return {
        "size_bytes": len(data),
        "sha256": hashlib.sha256(data).hexdigest(),
    }


def _integer_sequence_summary(values: Iterable[int]) -> dict:
    materialized = [int(value) for value in values]
    return {
        "element_count": len(materialized),
        "canonical_json_sha256": _json_sha256(materialized),
    }


def _ordered_blob_queue(queue: Iterable[bytes]) -> dict:
    entries = [_blob_summary(frame) for frame in queue]
    return {
        "entry_count": len(entries),
        "entries": entries,
        "canonical_json_sha256": _json_sha256(entries),
    }


def _core_state(cpu) -> dict:
    cs = cpu._cs
    return {
        "identity": {
            "core_id": int(cpu.core_id),
            "num_cores": int(cpu.num_cores),
            "mem_size": int(cpu.mem_size),
        },
        "registers": [int(cpu.regs[index]) for index in range(32)],
        "program_counter": int(cpu.pc),
        "selectors": {
            "psel": int(cpu.psel),
            "xsel": int(cpu.xsel),
            "spsel": int(cpu.spsel),
        },
        "flags": {
            name: int(getattr(cpu, f"flag_{name}"))
            for name in ("z", "c", "n", "v", "p", "g", "i", "s")
        },
        "legacy_registers": {
            "d_reg": int(cpu.d_reg),
            "q_out": int(cpu.q_out),
            "t_reg": int(cpu.t_reg),
        },
        "tile": {
            "sb": int(cpu.sb),
            "sr": int(cpu.sr),
            "sc": int(cpu.sc),
            "sw": int(cpu.sw),
            "tmode": int(cpu.tmode),
            "tctrl": int(cpu.tctrl),
            "tsrc0": int(cpu.tsrc0),
            "tsrc1": int(cpu.tsrc1),
            "tdst": int(cpu.tdst),
            "accumulators": [int(cpu.acc[index]) for index in range(4)],
            "tstride_r": int(cpu.tstride_r),
            "tstride_c_python_reserved": int(cpu.tstride_c),
            "ttile_h": int(cpu.ttile_h),
            "ttile_w": int(cpu.ttile_w),
        },
        "trap_interrupt": {
            "ivt_base": int(cpu.ivt_base),
            "ivec_id": int(cpu.ivec_id),
            "trap_addr": int(cpu.trap_addr),
            "ef_flags": int(cpu.ef_flags),
            "irq_ipi": bool(cpu.irq_ipi),
        },
        "execution": {
            "halted": bool(cpu.halted),
            "idle": bool(cpu.idle),
            "cycle_count": int(cpu.cycle_count),
        },
        "performance_counters": {
            "perf_enable": int(cpu.perf_enable),
            "perf_cycles": int(cpu.perf_cycles),
            "perf_stalls": int(cpu.perf_stalls),
            "perf_tileops": int(cpu.perf_tileops),
            "perf_extmem": int(cpu.perf_extmem),
        },
        "bist": {
            "bist_status": int(cpu.bist_status),
            "bist_fail_addr": int(cpu.bist_fail_addr),
            "bist_fail_data": int(cpu.bist_fail_data),
            "tile_selftest": int(cpu.tile_selftest),
            "tile_st_detail": int(cpu.tile_st_detail),
        },
        "instruction_cache": {
            "enabled": int(cpu.icache_enabled),
            "hits": int(cpu.icache_hits),
            "misses": int(cpu.icache_misses),
        },
        "privilege_and_mpu": {
            "priv_level": int(cpu.priv_level),
            "mpu_base": int(cpu.mpu_base),
            "mpu_limit": int(cpu.mpu_limit),
        },
        "extension_state": {
            "ext_modifier": int(cpu.ext_modifier),
            "crc_acc": int(cs.crc_acc),
            "crc_mode": int(cs.crc_mode),
            "sha_mode": int(cs.sha_mode),
            "sha_msglen_lo": int(cs.sha_msglen_lo),
            "sha_msglen_hi": int(cs.sha_msglen_hi),
            "gf_prime_sel": int(cs.gf_prime_sel),
        },
        "attached_memory": {
            "hbw_base": int(cs.hbw_base),
            "hbw_size": int(cs.hbw_size),
            "ext_mem_base": int(cs.ext_mem_base),
            "ext_mem_size": int(cs.ext_mem_size),
            "vram_base": int(cs.vram_base),
            "vram_size": int(cs.vram_size),
        },
        "port_io": {
            "python_port_map": [
                int(cpu.port_map.get(index, 0)) for index in range(8)
            ],
            "native_port_map": [
                int(cs.get_port_map(index)) for index in range(8)
            ],
            "native_port_out": [
                int(cs.get_port_out(index)) for index in range(8)
            ],
        },
        "accelerator_hooks": {
            "registered_count": int(cs.accel_hook_count),
        },
        "python_fallback_instantiated": cpu._py_fallback is not None,
    }


def _native_device_state(cpu) -> dict:
    cs = cpu._cs
    crypto_windows = {
        "aes_0x0700_0x076f": _blob_summary(bytes(
            cs.crypto_read8(address) for address in range(0x0700, 0x0770)
        )),
        "sha3_0x0780_0x07cf": _blob_summary(bytes(
            cs.crypto_read8(address) for address in range(0x0780, 0x07D0)
        )),
        "wots_0x08a0_0x08bf": _blob_summary(bytes(
            cs.crypto_read8(address) for address in range(0x08A0, 0x08C0)
        )),
    }
    nic_window = bytes(
        cs.nic_read8(address) for address in range(0x0400, 0x0480)
    )
    rtc = cs.rtc_snapshot()
    return {
        "core_id": int(cpu.core_id),
        "timer": {
            "enabled": bool(cs.timer_enabled()),
            "counter": int(cs.timer_counter),
            "compare": int(cs.timer_compare),
            "control": int(cs.timer_control),
            "status": int(cs.timer_status),
            "irq_pending": bool(cs.timer_irq_pending),
        },
        "framebuffer": {
            "enabled": bool(cs.fb_enabled()),
            "base": int(cs.fb_base_addr),
            "width": int(cs.fb_width),
            "height": int(cs.fb_height),
            "stride": int(cs.fb_stride),
            "mode": int(cs.fb_mode),
            "enable_register": int(cs.fb_enable),
            "vsync_count": int(cs.fb_vsync_count),
            "vblank": bool(cs.fb_vblank),
            "cycles_per_frame": int(cs.fb_cycles_per_frame),
            "irq_pending": bool(cs.fb_irq_pending()),
            "palette": _integer_sequence_summary(cs.fb_get_palette()),
        },
        "rtc": {
            "enabled": bool(rtc[0]),
            "realtime": bool(rtc[1]),
            "uptime_ms": int(rtc[2]),
            "epoch_ms": int(rtc[3]),
            "calendar": {
                "second": int(rtc[4]),
                "minute": int(rtc[5]),
                "hour": int(rtc[6]),
                "day": int(rtc[7]),
                "month": int(rtc[8]),
                "year": int(rtc[9]),
                "day_of_week": int(rtc[10]),
            },
            "control": int(rtc[11]),
            "status": int(rtc[12]),
            "alarm": {
                "second": int(rtc[13]),
                "minute": int(rtc[14]),
                "hour": int(rtc[15]),
            },
            "irq_pending": bool(rtc[16]),
            "millisecond_prescaler": int(rtc[17]),
            "second_prescaler": int(rtc[18]),
            "uptime_latch": int(rtc[19]),
            "epoch_latch": int(rtc[20]),
        },
        "uart_geometry": {
            "enabled": bool(cs.uart_geom_enabled()),
            "columns": int(cs.uart_geom_cols),
            "rows": int(cs.uart_geom_rows),
            "status": int(cs.uart_geom_status),
            "control": int(cs.uart_geom_ctrl),
            "requested_columns": int(cs.uart_geom_req_cols),
            "requested_rows": int(cs.uart_geom_req_rows),
        },
        "uart": {
            "enabled": bool(cs.uart_enabled()),
            "has_rx": bool(cs.uart_has_rx()),
            "rx_queue_size": int(cs.uart_rx_size()),
            "tx_ring_base": int(cs.uart_tx_ring_base),
            "safe_registers": {
                "status": int(cs.uart_read8(0x02)),
                "control": int(cs.uart_read8(0x03)),
                "baud_low": int(cs.uart_read8(0x04)),
                "baud_high": int(cs.uart_read8(0x05)),
            },
        },
        "crypto": {
            "enabled": bool(cs.crypto_enabled()),
            "wots_status": int(cs.crypto_wots_status()),
            "mmio_visible_windows": crypto_windows,
        },
        "nic": {
            "enabled": bool(cs.nic_enabled()),
            "rx_queue_size": int(cs.nic_rx_queue_size()),
            "tx_queue_size": int(cs.nic_tx_queue_size()),
            "rx_count": int(cs.nic_get_rx_count()),
            "tx_count": int(cs.nic_get_tx_count()),
            "irq_pending": bool(cs.nic_irq_pending()),
            "mmio_visible_window": _blob_summary(nic_window),
        },
        "trng": {
            "enabled": bool(cs.trng_enabled()),
        },
    }


def _storage_state(storage) -> dict:
    faults = [
        {
            "stage": fault.stage,
            "result": int(fault.result),
            "command": fault.command,
            "sector_index": fault.sector_index,
            "byte_index": fault.byte_index,
            "action": fault.action,
        }
        for fault in storage._faults
    ]
    return {
        "capacity_sectors": int(storage._capacity_sectors),
        "media": _blob_summary(storage._image_data),
        "sector_num": int(storage.sector_num),
        "dma_addr": int(storage.dma_addr),
        "sector_count": int(storage.sec_count),
        "dma_push_counter": int(storage._dma_push_ctr),
        "status_present_latch": int(storage.status),
        "busy": bool(storage.busy),
        "error": bool(storage.error),
        "rejected": bool(storage.rejected),
        "result_valid": bool(storage.result_valid),
        "media_changed": bool(storage.media_changed),
        "write_protected": bool(storage.write_protected),
        "result": int(storage.result),
        "completion": int(storage.completion),
        "media_generation": int(storage.media_generation),
        "expected_media_generation":
            int(storage.expected_media_generation),
        "transferred_sectors": int(storage.transferred),
        "data_port_buffer": _blob_summary(storage.data_port_buf),
        "data_port_position": int(storage.data_port_pos),
        "active_request": (
            None if storage._active_request is None
            else [int(value) for value in storage._active_request]
        ),
        "active_effect": bool(storage._active_effect),
        "active_transferred": int(storage._active_transferred),
        "stalled": bool(storage._stalled),
        "queued_faults": faults,
    }


def _shared_device_state(system: MegapadSystem) -> dict:
    uart = system.uart
    audio = system.audio
    nic = system.nic
    sysinfo = system.sysinfo
    mailbox = system.mailbox
    spinlock = system.spinlock
    ntt = system.ntt
    kem = system.kem
    port_bridge = system.port_bridge
    wots = system.wots
    system_cycles, event_deadlines, event_deadline, event_sources = (
        system._native_system.system_clock_snapshot()
    )
    return {
        "system_clock": {
            "cycles": int(system_cycles),
            "event_deadline": (
                None if event_deadline is None else int(event_deadline)
            ),
            "event_source_mask": int(event_sources),
            "source_deadlines": [
                None if deadline is None else int(deadline)
                for deadline in event_deadlines
            ],
        },
        "device_bus": {
            "registered_devices": [
                {
                    "name": device.name,
                    "base": int(device.base),
                    "size": int(device.size),
                }
                for device in system.bus.devices
            ],
        },
        "uart_facade": {
            "tx_buffer": _blob_summary(bytes(uart.tx_buffer)),
            "rx_buffer": _blob_summary(bytes(uart.rx_buffer)),
            "control": int(uart.control),
            "baud_low": int(uart.baud_lo),
            "baud_high": int(uart.baud_hi),
            "tx_ring_address_bytes": _blob_summary(
                uart._tx_ring_addr_bytes
            ),
            "python_tx_ring_base": int(uart._UART__tx_ring_base),
            "resolved_tx_ring_base": int(uart._tx_ring_base),
            "native_attached": uart._native is not None,
            "system_tx_log": _blob_summary(bytes(system._tx_log)),
        },
        "storage": _storage_state(system.storage),
        "audio": {
            "max_capture_bytes": int(audio.max_capture_bytes),
            "format": int(audio.format),
            "channels": int(audio.channels),
            "rate": int(audio.rate),
            "dma_addr": int(audio.dma_addr),
            "frames": int(audio.frames),
            "generation": int(audio.generation),
            "error": int(audio.error),
            "busy": bool(audio.busy),
            "done": bool(audio.done),
            "playing": bool(audio.playing),
            "last_pcm": _blob_summary(audio.last_pcm),
            "last_rate": int(audio.last_rate),
            "last_channels": int(audio.last_channels),
            "last_frames": int(audio.last_frames),
            "host_sink_attached": bool(audio._sink_attached()),
        },
        "nic_facade": {
            "mac": bytes(nic.mac).hex(),
            "dma_addr": int(nic.dma_addr),
            "frame_len": int(nic.frame_len),
            "irq_control": int(nic.irq_ctrl),
            "irq_status": int(nic.irq_status),
            "error": bool(nic.error),
            "link_up": bool(nic.link_up),
            "tx_count": int(nic.tx_count),
            "rx_count": int(nic.rx_count),
            "dma_push_counter": int(nic._dma_push_ctr),
            "tx_queue": _ordered_blob_queue(nic.tx_queue),
            "rx_queue": _ordered_blob_queue(nic.rx_queue),
            "data_window": _blob_summary(nic._data_window),
            "data_window_valid_mask": int(nic._data_window_valid),
            "backend_mode": nic.backend_name,
            "background_receiver_running": bool(nic._running),
        },
        "system_info": {
            "bank0_size": int(sysinfo.bank0_size),
            "num_cores": int(sysinfo.num_cores),
            "num_full_cores": int(sysinfo.num_full_cores),
            "cluster_enable": int(sysinfo.cluster_en),
            "hbw_base": int(sysinfo.hbw_base),
            "hbw_size": int(sysinfo.hbw_size),
            "internal_memory_total": int(sysinfo.int_mem_total),
            "external_memory_base": int(sysinfo.ext_mem_base),
            "external_memory_size": int(sysinfo.ext_mem_size),
            "vram_base": int(sysinfo.vram_base),
            "vram_size": int(sysinfo.vram_size),
            "has_storage": bool(sysinfo.has_storage),
            "has_nic": bool(sysinfo.has_nic),
            "register_table": [
                [int(offset), int(value)]
                for offset, value in sorted(sysinfo._regs.items())
            ],
        },
        "mailbox": {
            "num_cores": int(mailbox.num_cores),
            "data": [int(value) for value in mailbox.data],
            "pending": [int(value) for value in mailbox.pending],
        },
        "spinlocks": {
            "num_locks": int(spinlock.num_locks),
            "locked": [bool(value) for value in spinlock.locked],
            "owner": [int(value) for value in spinlock.owner],
        },
        "ntt": {
            "q": int(ntt._q),
            "index": int(ntt._idx),
            "busy": bool(ntt._busy),
            "done": bool(ntt._done),
            "omega": ntt._omega,
            "omega_inverse": ntt._omega_inv,
            "n_inverse": ntt._n_inv,
            "poly_a": _integer_sequence_summary(ntt._poly_a),
            "poly_b": _integer_sequence_summary(ntt._poly_b),
            "result": _integer_sequence_summary(ntt._result),
            "load_a_buffer": _blob_summary(ntt._load_a_buf),
            "load_b_buffer": _blob_summary(ntt._load_b_buf),
        },
        "kem": {
            "status": int(kem.status),
            "buffer_select": int(kem._buf_sel),
            "buffer_index": int(kem._buf_idx),
            "buffers": [_blob_summary(buffer) for buffer in kem._bufs],
        },
        "port_bridge": {
            "table": [int(value) for value in port_bridge._table],
            "control": int(port_bridge._ctrl),
        },
        "python_wots_chain": {
            "seed_addr": int(wots._seed_addr),
            "adrs_addr": int(wots._adrs_addr),
            "input_addr": int(wots._input_addr),
            "steps": int(wots._steps),
            "start": int(wots._start),
            "status": int(wots._status),
            "last_cycles": int(wots._last_cycles),
            "output": _blob_summary(wots._dout),
            "memory_attached": wots._mem is not None,
        },
    }


def _state_observation(workload: Workload) -> dict:
    system = workload.system
    cores = [_core_state(cpu) for cpu in system.cores]
    memory = {
        "shared_ram": _blob_summary(system._shared_mem),
        "hbw_ram": _blob_summary(system._hbw_mem),
        "external_memory": _blob_summary(system._ext_mem),
        "vram": _blob_summary(system._vram_mem),
    }
    native_devices = [
        _native_device_state(cpu) for cpu in system.cores
    ]
    topology = {
        "ram_size": int(system.ram_size),
        "num_full_cores": int(system.num_full_cores),
        "num_micro_cores": int(system.num_micro_cores),
        "num_all_cores": int(system.num_cores),
        "num_clusters": int(system.num_clusters),
        "cluster_count_constructed": len(system.clusters),
        "hbw_size": int(system.hbw_size),
        "hbw_end": int(system.hbw_end),
        "external_memory_base": int(system.ext_mem_base),
        "external_memory_size": int(system.ext_mem_size),
        "external_memory_end": int(system.ext_mem_end),
        "vram_base": int(system.vram_base),
        "vram_size": int(system.vram_size),
        "vram_end": int(system.vram_end),
        "booted": bool(system._booted),
    }
    shared_devices = _shared_device_state(system)
    canonical_state = {
        "schema": STATE_SCHEMA,
        "schema_version": STATE_SCHEMA_VERSION,
        "topology": topology,
        "cores": cores,
        "memory": memory,
        "shared_devices": shared_devices,
        "native_devices_per_full_core": native_devices,
    }
    workload_metrics = json.loads(
        _canonical_json_bytes(workload.metrics).decode("ascii")
    )
    canonical_state_sha256 = _json_sha256(canonical_state)
    behavior_oracle_sha256 = _json_sha256({
        "canonical_state_sha256": canonical_state_sha256,
        "workload_metrics": workload_metrics,
    })
    return {
        "state_schema": STATE_SCHEMA,
        "state_schema_version": STATE_SCHEMA_VERSION,
        "canonical_state_sha256": canonical_state_sha256,
        "behavior_oracle_sha256": behavior_oracle_sha256,
        "component_sha256": {
            "topology": _json_sha256(topology),
            "cores": _json_sha256(cores),
            "memory": _json_sha256(memory),
            "shared_devices": _json_sha256(shared_devices),
            "native_devices_per_full_core": _json_sha256(native_devices),
        },
        "canonical_state": canonical_state,
        "per_core": [
            {
                "core_id": int(cpu.core_id),
                "pc": int(cpu.pc),
                "cycle_count": int(cpu.cycle_count),
                "halted": bool(cpu.halted),
                "idle": bool(cpu.idle),
                "interrupt_handler_entries": int(cpu.regs[12]),
            }
            for cpu in system.cores
        ],
        "workload_metrics": workload_metrics,
    }


def _timed_sample(scenario: Scenario, num_cores: int, target: int) -> dict:
    workload = scenario.build(num_cores)
    gc_enabled_before = gc.isenabled()
    collected_before_timing = gc.collect()
    try:
        if gc_enabled_before:
            gc.disable()
        gc_enabled_during_timing = gc.isenabled()
        try:
            wall_start = time.perf_counter()
            cpu_start = time.process_time()
            execution = workload.execute(target)
            process_cpu_s = time.process_time() - cpu_start
            wall_s = time.perf_counter() - wall_start
        finally:
            if gc_enabled_before:
                gc.enable()
        gc_restored_to_prior_state = gc.isenabled() == gc_enabled_before
        observation = _state_observation(workload)
    finally:
        if gc.isenabled() != gc_enabled_before:
            if gc_enabled_before:
                gc.enable()
            else:
                gc.disable()
        workload.close()

    instructions = execution.returned_aggregate_instructions
    return {
        **execution.as_dict(),
        "wall_time_s": wall_s,
        "process_cpu_time_s": process_cpu_s,
        "host_cpu_utilization_percent":
            (process_cpu_s / wall_s * 100.0) if wall_s else None,
        "aggregate_instructions_per_s":
            (instructions / wall_s) if wall_s else None,
        "aggregate_mips":
            (instructions / wall_s / 1_000_000.0) if wall_s else None,
        "cyclic_gc": {
            "enabled_before_timing": gc_enabled_before,
            "collected_objects_before_timing": collected_before_timing,
            "enabled_during_timing": gc_enabled_during_timing,
            "disabled_by_harness": gc_enabled_before,
            "restored_to_prior_state": gc_restored_to_prior_state,
        },
        "observation": observation,
    }


def _install_accounting(workload: Workload) -> tuple[list[dict], dict]:
    """Install accounting-only wrappers; never used for timed throughput."""
    core_stats = []
    for cpu in workload.system.cores:
        stats = {
            "core_id": cpu.core_id,
            "instructions": 0,
            "run_steps_calls": 0,
            "stop_reasons": {},
            "python_mmio_reads": 0,
            "python_mmio_writes": 0,
        }
        core_stats.append(stats)

        original_run_steps = cpu.run_steps
        original_read = cpu._mmio_read8
        original_write = cpu._mmio_write8

        def counted_run_steps(
            max_steps: int,
            *,
            _original=original_run_steps,
            _stats=stats,
        ):
            steps, reason = _original(max_steps)
            _stats["instructions"] += int(steps)
            _stats["run_steps_calls"] += 1
            reason_key = {
                0: "max_steps",
                1: "halt",
                2: "idle",
            }.get(int(reason), f"unknown_{int(reason)}")
            _stats["stop_reasons"][reason_key] = (
                _stats["stop_reasons"].get(reason_key, 0) + 1
            )
            return steps, reason

        def counted_read(addr: int, *, _original=original_read, _stats=stats):
            _stats["python_mmio_reads"] += 1
            return _original(addr)

        def counted_write(
            addr: int,
            value: int,
            *,
            _original=original_write,
            _stats=stats,
        ):
            _stats["python_mmio_writes"] += 1
            return _original(addr, value)

        cpu.run_steps = counted_run_steps
        cpu._mmio_read8 = counted_read
        cpu._mmio_write8 = counted_write

    bus_stats = {"tick_calls": 0, "tick_argument_units": 0}
    original_tick = workload.system.bus.tick

    def counted_tick(units: int):
        bus_stats["tick_calls"] += 1
        bus_stats["tick_argument_units"] += int(units)
        return original_tick(units)

    workload.system.bus.tick = counted_tick
    return core_stats, bus_stats


def _accounting_probe(
    scenario: Scenario,
    num_cores: int,
    target: int,
) -> dict:
    workload = scenario.build(num_cores)
    core_stats, bus_stats = _install_accounting(workload)
    start_cycles = [int(cpu.cycle_count) for cpu in workload.system.cores]
    try:
        execution = workload.execute(target)
        observation = _state_observation(workload)
        end_cycles = [
            int(cpu.cycle_count) for cpu in workload.system.cores
        ]
    finally:
        workload.close()

    per_core_cycles = [
        end - start for start, end in zip(start_cycles, end_cycles)
    ]
    per_core_instructions = [entry["instructions"] for entry in core_stats]
    aggregate_core_cycles = sum(per_core_cycles)
    aggregate_instructions = sum(per_core_instructions)
    returned = execution.returned_aggregate_instructions
    return {
        "instrumented": True,
        "used_for_throughput": False,
        "execution": execution.as_dict(),
        "per_core": [
            {
                **stats,
                "architectural_cycles": cycles,
            }
            for stats, cycles in zip(core_stats, per_core_cycles)
        ],
        "aggregate_instructions_from_per_core": aggregate_instructions,
        "instruction_accounting_matches_runner":
            aggregate_instructions == returned,
        "aggregate_core_architectural_cycles": aggregate_core_cycles,
        "max_core_architectural_cycles": max(per_core_cycles, default=0),
        "device_bus_tick_calls": bus_stats["tick_calls"],
        "device_bus_tick_argument_units": bus_stats["tick_argument_units"],
        "device_tick_to_returned_instruction_ratio":
            (bus_stats["tick_argument_units"] / returned) if returned else None,
        "virtual_system_cycles": None,
        "virtual_system_cycles_availability":
            "unavailable: the legacy runner does not drive the native "
            "system clock",
        "observation": observation,
    }


def _summary(samples: list[dict], accounting: dict) -> dict:
    rates = [sample["aggregate_instructions_per_s"] for sample in samples]
    walls = [sample["wall_time_s"] for sample in samples]
    cpu_util = [sample["host_cpu_utilization_percent"] for sample in samples]
    state_signatures = [
        sample["observation"]["canonical_state_sha256"]
        for sample in samples
    ]
    oracle_signatures = [
        sample["observation"]["behavior_oracle_sha256"]
        for sample in samples
    ]
    median_rate = statistics.median(rates)
    accounting_instructions = accounting[
        "aggregate_instructions_from_per_core"
    ]
    accounting_oracle = accounting["observation"][
        "behavior_oracle_sha256"
    ]
    accounting_execution = accounting["execution"]
    replay_comparisons = []
    for repeat_index, (sample, timed_oracle) in enumerate(
        zip(samples, oracle_signatures)
    ):
        execution_matches = all(
            sample[key] == accounting_execution[key]
            for key in (
                "requested_aggregate_instructions",
                "requested_run_batch_units",
                "returned_aggregate_instructions",
                "aggregate_instruction_overshoot",
                "run_batch_calls",
            )
        )
        oracle_matches = timed_oracle == accounting_oracle
        replay_comparisons.append({
            "repeat_index": repeat_index,
            "behavior_oracle_matches": oracle_matches,
            "execution_accounting_matches": execution_matches,
            "matches": oracle_matches and execution_matches,
        })

    all_replays_match = (
        bool(replay_comparisons)
        and all(entry["matches"] for entry in replay_comparisons)
    )
    derivation_available = (
        all_replays_match
        and accounting["instruction_accounting_matches_runner"]
        and accounting_instructions > 0
    )
    per_core_rates = None
    derivation_unavailable_reason = None
    if derivation_available:
        per_core_rates = []
        for core in accounting["per_core"]:
            share = core["instructions"] / accounting_instructions
            per_core_rates.append(
                {
                    "core_id": core["core_id"],
                    "instruction_share": share,
                    "derived_instructions_per_s": median_rate * share,
                    "derived_mips": median_rate * share / 1_000_000.0,
                }
            )
    else:
        reasons = []
        if not all_replays_match:
            reasons.append(
                "accounting replay did not equal every timed repeat"
            )
        if not accounting["instruction_accounting_matches_runner"]:
            reasons.append(
                "per-core instruction sum did not equal the runner result"
            )
        if accounting_instructions <= 0:
            reasons.append("accounting replay recorded no instructions")
        derivation_unavailable_reason = "; ".join(reasons)

    return {
        "repeat_count": len(samples),
        "median_wall_time_s": statistics.median(walls),
        "min_wall_time_s": min(walls),
        "max_wall_time_s": max(walls),
        "median_aggregate_instructions_per_s": median_rate,
        "median_aggregate_mips": median_rate / 1_000_000.0,
        "min_aggregate_instructions_per_s": min(rates),
        "max_aggregate_instructions_per_s": max(rates),
        "population_stdev_aggregate_instructions_per_s":
            statistics.pstdev(rates),
        "median_host_cpu_utilization_percent": statistics.median(cpu_util),
        "deterministic_timed_repeats":
            len(set(oracle_signatures)) == 1,
        "unique_timed_state_signatures": len(set(state_signatures)),
        "unique_timed_behavior_oracle_signatures":
            len(set(oracle_signatures)),
        "accounting_replay_comparison_by_timed_repeat":
            replay_comparisons,
        "accounting_matches_each_timed_repeat": [
            entry["matches"] for entry in replay_comparisons
        ],
        "accounting_matches_all_timed_repeats": all_replays_match,
        "per_core_rate_derivation":
            "exact accounting-replay instruction share multiplied by "
            "uninstrumented median aggregate throughput; published only when "
            "the replay equals every timed repeat and instruction accounting "
            "matches the runner",
        "per_core_rate_derivation_available": derivation_available,
        "per_core_rate_derivation_unavailable_reason":
            derivation_unavailable_reason,
        "derived_per_core_instruction_rates": per_core_rates,
    }


def _run_warmup(scenario: Scenario, num_cores: int, instructions: int) -> None:
    workload = scenario.build(num_cores)
    try:
        workload.execute(instructions)
    finally:
        workload.close()


def run_report(
    *,
    core_counts: Iterable[int],
    scenario_names: Iterable[str],
    instructions: int,
    repeats: int,
    warmups: int,
    warmup_instructions: int,
) -> dict:
    core_counts = list(core_counts)
    scenario_names = list(scenario_names)
    selected_scenarios = [SCENARIOS[name] for name in scenario_names]
    results = []
    for scenario in selected_scenarios:
        for num_cores in core_counts:
            for _ in range(warmups):
                _run_warmup(scenario, num_cores, warmup_instructions)
            samples = [
                _timed_sample(scenario, num_cores, instructions)
                for _ in range(repeats)
            ]
            accounting = _accounting_probe(
                scenario,
                num_cores,
                instructions,
            )
            results.append(
                {
                    "scenario": scenario.name,
                    "description": scenario.description,
                    "known_limitation": scenario.limitation,
                    "coverage_classification":
                        scenario.coverage_classification,
                    "coverage_claim": scenario.coverage_claim,
                    "full_cores": num_cores,
                    "timed_samples": samples,
                    "accounting_probe": accounting,
                    "summary": _summary(samples, accounting),
                }
            )

    validation = {
        "all_instruction_accounting_matches": all(
            result["accounting_probe"][
                "instruction_accounting_matches_runner"
            ]
            for result in results
        ),
        "all_timed_repeats_deterministic": all(
            result["summary"]["deterministic_timed_repeats"]
            for result in results
        ),
        "all_accounting_replays_match_every_timed_repeat": all(
            result["summary"]["accounting_matches_all_timed_repeats"]
            for result in results
        ),
        "all_derived_per_core_rates_guarded_by_matching_replay": all(
            (
                result["summary"]["derived_per_core_instruction_rates"]
                is not None
            ) == result["summary"]["per_core_rate_derivation_available"]
            for result in results
        ),
        "all_timed_samples_ran_with_cyclic_gc_disabled": all(
            not sample["cyclic_gc"]["enabled_during_timing"]
            for result in results
            for sample in result["timed_samples"]
        ),
        "all_timed_samples_restored_prior_gc_state": all(
            sample["cyclic_gc"]["restored_to_prior_state"]
            for result in results
            for sample in result["timed_samples"]
        ),
    }
    return {
        "schema": SCHEMA,
        "schema_version": SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "repository": repository_metadata(),
        "host": host_metadata(),
        "coverage": COVERAGE_METADATA,
        "state_comparison_scope": STATE_COMPARISON_SCOPE,
        "configuration": {
            "full_core_counts": list(core_counts),
            "scenarios": [scenario.name for scenario in selected_scenarios],
            "aggregate_instruction_target_per_sample": instructions,
            "timed_repeats": repeats,
            "warmup_runs_per_case": warmups,
            "warmup_instructions_per_run": warmup_instructions,
        },
        "measurement_semantics": {
            "timed_throughput":
                "uninstrumented wall/process time around workload execution "
                "with cyclic garbage collection disabled and restored safely",
            "aggregate_instructions":
                "sum of values returned by legacy MegapadSystem.run_batch()",
            "per_core_instructions":
                "exact benchmark-only run_steps wrapper counts from a "
                "separate, untimed accounting replay",
            "per_core_architectural_cycles":
                "delta of each CPU architectural cycle_count",
            "aggregate_core_architectural_cycles":
                "sum of per-core cycle_count deltas; not elapsed system time",
            "max_core_architectural_cycles":
                "diagnostic critical-path proxy only; not system time",
            "device_bus_tick_argument_units":
                "sum of integer arguments passed to DeviceBus.tick(); the "
                "legacy runner derives these from aggregate instructions",
            "virtual_system_cycles": None,
            "virtual_system_cycles_availability":
                "unavailable until the native scheduler drives the owned "
                "system clock",
            "host_cpu_utilization_percent":
                "process CPU time divided by wall time; may exceed 100% when "
                "host worker threads overlap",
            "derived_per_core_throughput":
                "accounting-replay instruction share multiplied by the "
                "uninstrumented median aggregate instruction rate; omitted "
                "unless the accounting replay equals every timed repeat",
            "canonical_state_oracle":
                "canonical JSON over the captured scope documented in "
                "state_comparison_scope; workload counters are added to form "
                "the behavior oracle",
            "deterministic_platform_initialization":
                "the harness pins the virtual RTC to "
                "2000-01-01T00:00:00Z and UART geometry to 80x24",
        },
        "validation": validation,
        "results": results,
    }


def parse_core_counts(text: str) -> list[int]:
    try:
        values = [int(item.strip()) for item in text.split(",") if item.strip()]
    except ValueError as exc:
        raise argparse.ArgumentTypeError(
            "cores must be a comma-separated subset of 1,2,4"
        ) from exc
    if not values or any(value not in {1, 2, 4} for value in values):
        raise argparse.ArgumentTypeError(
            "cores must be a comma-separated subset of 1,2,4"
        )
    return list(dict.fromkeys(values))


def parse_scenarios(text: str) -> list[str]:
    if text.strip().lower() == "all":
        return list(SCENARIOS)
    names = [item.strip() for item in text.split(",") if item.strip()]
    unknown = [name for name in names if name not in SCENARIOS]
    if not names or unknown:
        available = ", ".join(SCENARIOS)
        detail = f"unknown scenarios: {', '.join(unknown)}; " if unknown else ""
        raise argparse.ArgumentTypeError(detail + f"available: {available}")
    return list(dict.fromkeys(names))


def print_human(report: dict) -> None:
    config = report["configuration"]
    print("MegaPad Phase 0 concurrency/performance baseline")
    print(
        f"  target={config['aggregate_instruction_target_per_sample']:,} "
        f"aggregate instructions, repeats={config['timed_repeats']}, "
        f"warmups={config['warmup_runs_per_case']}"
    )
    print()
    print(
        f"{'scenario':<41} {'cores':>5} {'agg MIPS':>10} "
        f"{'host CPU':>9} {'callbacks':>10} {'deterministic':>14}"
    )
    print("-" * 96)
    for result in report["results"]:
        summary = result["summary"]
        accounting = result["accounting_probe"]
        callbacks = sum(
            core["python_mmio_reads"] + core["python_mmio_writes"]
            for core in accounting["per_core"]
        )
        deterministic = (
            summary["deterministic_timed_repeats"]
            and summary["accounting_matches_all_timed_repeats"]
        )
        print(
            f"{result['scenario']:<41} {result['full_cores']:>5} "
            f"{summary['median_aggregate_mips']:>10.2f} "
            f"{summary['median_host_cpu_utilization_percent']:>8.1f}% "
            f"{callbacks:>10,} {str(deterministic):>14}"
        )
    print()
    print(
        "Virtual system cycles: native owner exists but the legacy runner "
        "does not drive it (JSON value is null)."
    )
    print(
        "Per-core instruction rates are derived from an exact untimed "
        "accounting replay only when it matches every timed repeat; timed "
        "aggregate rates are uninstrumented."
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Run a reproducible, bounded Phase 0 baseline for MegaPad's "
            "legacy multicore system runner."
        )
    )
    parser.add_argument(
        "--cores",
        type=parse_core_counts,
        default=parse_core_counts("1,2,4"),
        help="comma-separated subset of 1,2,4 (default: 1,2,4)",
    )
    parser.add_argument(
        "--scenarios",
        type=parse_scenarios,
        default=parse_scenarios("all"),
        help="comma-separated scenario names or 'all' (default: all)",
    )
    parser.add_argument(
        "--instructions",
        type=parse_count,
        default=parse_count("2m"),
        help="aggregate instruction target per sample; k/m/g accepted "
             "(default: 2m)",
    )
    parser.add_argument(
        "--repeats",
        type=parse_positive_int,
        default=3,
        help="timed repetitions per scenario/core count (default: 3)",
    )
    parser.add_argument(
        "--warmups",
        type=parse_nonnegative_int,
        default=1,
        help="discarded warmup runs per scenario/core count (default: 1)",
    )
    parser.add_argument(
        "--warmup-instructions",
        type=parse_count,
        default=parse_count("100k"),
        help="aggregate instructions per warmup run (default: 100k)",
    )
    parser.add_argument(
        "--quick",
        action="store_true",
        help="bounded smoke profile: 100k instructions, one repeat, no warmup",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="write the complete report as JSON to stdout",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="also write the complete JSON report to this path",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not ACCEL_AVAILABLE:
        parser.error(
            "the C++ accelerator is required; build it with `make accel`"
        )
    if args.quick:
        args.instructions = 100_000
        args.repeats = 1
        args.warmups = 0
        args.warmup_instructions = 10_000

    report = run_report(
        core_counts=args.cores,
        scenario_names=args.scenarios,
        instructions=args.instructions,
        repeats=args.repeats,
        warmups=args.warmups,
        warmup_instructions=args.warmup_instructions,
    )
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        print_human(report)
        if args.output is not None:
            print(f"JSON report: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
