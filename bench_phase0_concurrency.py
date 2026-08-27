#!/usr/bin/env python3
"""Bounded Phase 0 workload baseline across MegaPad host-execution lanes.

This benchmark is deliberately diagnostic rather than aspirational. It
compares the current deterministic ``MegapadSystem.run_batch()`` behavior
across fixed one-, two-, and four-lane native worker configurations.

The report keeps four quantities separate:

* returned aggregate instructions (the legacy ``run_batch`` result);
* exact per-core instructions from native system-batch results;
* per-core architectural cycle-counter deltas; and
* authoritative virtual system cycles passed through ``DeviceBus.tick()``.

The native owner contains the authoritative system clock. The coordinator
advances it from exact core results while keeping aggregate and per-core
architectural counters distinct.

Default coverage:

* 1, 2, and 4 full cores;
* private register/ALU work;
* same-address shared-memory pressure;
* mixed native and Python-dispatched MMIO polling;
* periodic timer interrupts; and
* legacy sequential storage/display orchestration around guest VRAM writes.

The report also carries a separate strict-cycle DMA probe. Two guest cores
start one native NIC transmit and one storage read, then the physical NIC and
disk ports contend under the integrated equal round-robin arbiter. DMA bytes
and virtual cycles are reported separately from the legacy instruction/MIPS
scenarios.

Examples::

    python3 bench_phase0_concurrency.py --quick --json
    python3 bench_phase0_concurrency.py --instructions 2m \
        --output /tmp/megapad-phase0.json
    python3 bench_phase0_concurrency.py --cores 4 \
        --worker-counts 1,2,4 \
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
    NIC_BASE,
    SECTOR_SIZE,
    STORAGE_BASE,
    STORAGE_CMD_READ,
    STORAGE_RESULT_OK,
    STORAGE_STATUS_BUSY,
    STORAGE_STATUS_ERROR,
    STORAGE_STATUS_MEDIA_CHANGED,
    STORAGE_STATUS_PRESENT,
    STORAGE_STATUS_REJECTED,
    STORAGE_STATUS_RESULT_VALID,
    SYSINFO_BASE,
    TIMER_BASE,
)
from megapad64 import IVEC_TIMER
from system import MegapadSystem, VRAM_BASE


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.phase0-concurrency-baseline"
SCHEMA_VERSION = 19
STATE_SCHEMA = "megapad.phase0-canonical-state"
STATE_SCHEMA_VERSION = 12

RAM_SIZE = 1 << 20
CODE_BASE = 0x1000
IVT_BASE = 0x2000
SHARED_DATA_BASE = 0x40000
DMA_DATA_BASE = 0x60000
FRAME_WIDTH = 128
FRAME_HEIGHT = 96
FRAME_STRIDE = FRAME_WIDTH
FRAME_BYTES = FRAME_STRIDE * FRAME_HEIGHT
STRICT_DMA_DEFAULT_BYTES = 1024
STRICT_DMA_QUICK_BYTES = SECTOR_SIZE
STRICT_DMA_MAX_INSTRUCTIONS = 16

DETERMINISTIC_RTC_EPOCH_MS = 946_684_800_000  # 2000-01-01T00:00:00Z

ARBITRATION_CONTRACT = {
    "policy": "weighted_round_robin",
    "weight_range": [1, 255],
    "bandwidth_epoch_cycles": 65536,
    "zero_bandwidth_limit": "unlimited",
    "unused_capacity": "work_conserving",
    "post_completion_bubble_cycles": 1,
    "wots_qos": {
        "weight": 1,
        "bandwidth_limit": 0,
        "programmable": False,
    },
}

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
        "the compiled accelerator sources and headers under accel/; native "
        "fields without "
        "non-destructive pybind readback are exclusions even when they affect "
        "future execution"
    ),
    "included": [
        "all 32 GPRs and every scalar CPUState field exposed by the binding",
        "per-core interrupt-line state and native tstride_c",
        "native port output/map state and accelerator hook count",
        "complete shared, HBW, external, and VRAM byte regions via size and "
        "SHA-256",
        "all scalar and buffer state of shared Python devices, with large "
        "buffers and ordered frame queues represented by stable hashes",
        "SystemState-owned native timer, framebuffer (including palette), "
        "RTC, UART geometry, UART, crypto-MMIO-visible, NIC-MMIO-visible, "
        "and TRNG state as observed through every full core, plus the "
        "authoritative WOTS architectural/private snapshot and its "
        "topology-derived deadlines",
        "native system-cycle and event-horizon state, logical scheduler "
        "cursor, complete main-bus arbiter snapshot, observable quiescent "
        "cycle-execution state, complete NIC/disk DMA coordinator and "
        "resumable-FSM diagnostics, the immutable timestamped external-event "
        "pending/history journal including payload hashes and sequence order, "
        "registered-device layout, platform topology, and benchmark "
        "orchestration counters",
    ],
    "explicit_exclusions": [
        {
            "state": "native input-port latch values",
            "reason": "the binding permits writes but exposes no readback",
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
                "APIs would mutate the measured state. Payloads retained in "
                "the external-event journal are captured separately."
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
                "and SHA-3 sponge, absorb, and squeeze state"
            ),
            "reason": (
                "the oracle hashes all non-destructively MMIO-readable crypto "
                "bytes and the complete bound WOTS snapshot, but those AES "
                "and public SHA-3 future-affecting internals are not bound"
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
                "cumulative native_batch_runs and native_dispatches "
                "provenance counters and transient native_batch_active flag"
            ),
            "reason": (
                "the counters are reported as per-execution deltas outside "
                "the behavior hash because they are host provenance, while "
                "the replay-visible external-ingress boundary timeline remains "
                "canonical and may distinguish different public invocation "
                "sequences; the active flag is an in-call exclusion guard and "
                "is false at every completed observation boundary"
            ),
        },
        {
            "state": (
                "suspended cycle-execution checkpoints, completed-access "
                "journals, deferred retirement cycles, and target-completion "
                "frontiers"
            ),
            "reason": (
                "those internals are not a public snapshot format; the Phase "
                "0 canonical observer refuses non-quiescent cycle execution "
                "instead of hashing an incomplete continuation"
            ),
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
            "state": (
                "micro-core cluster scratchpads and cluster scheduler state"
            ),
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
            "covers": (
                "private register/ALU execution with the production "
                "instruction-cache model"
            ),
            "does_not_claim": (
                "a dedicated instruction-cache capacity/conflict stress test"
            ),
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
        {
            "scenario": "strict_nic_disk_dma",
            "classification": "strict_cycle_real_dma_baseline",
            "covers": (
                "guest-issued native NIC transmit and storage read bytes "
                "contending on their physical main-bus ports"
            ),
            "does_not_claim": (
                "hard-QoS eligibility transitions, unused-reservation "
                "borrowing, active-display overlap, or the deferred Phase 4 "
                "bulk-DMA optimization"
            ),
        },
    ],
    "deferred_gates": [
        {
            "gate": "dedicated_instruction_cache_pressure",
            "status": "covered_by_separate_phase2_oracle",
            "reason": (
                "this hot-loop workload records complete production cache "
                "state, while bench_phase2_icache.py owns alternating hot and "
                "disabled-cache pressure measurements"
            ),
        },
        {
            "gate": "nic_dma_with_active_display_overlap",
            "status": "deferred",
            "reason": (
                "the strict DMA probe now exercises real NIC and disk ports, "
                "but it deliberately leaves display traffic inactive; the "
                "existing storage/display case remains sequential"
            ),
        },
    ],
    "main_bus_arbitration_contract": ARBITRATION_CONTRACT,
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

STRICT_DMA_COMMAND = """
    st.b r1, r2
    halt
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


def parse_strict_dma_bytes(text: str) -> int:
    value = parse_count(text)
    if value % SECTOR_SIZE != 0 or value > 1024:
        raise argparse.ArgumentTypeError(
            "strict DMA bytes must be 512 or 1024"
        )
    return value


def parse_worker_counts(text: str) -> list[int]:
    try:
        values = [
            int(item.strip())
            for item in text.split(",")
            if item.strip()
        ]
    except ValueError as exc:
        raise argparse.ArgumentTypeError(
            "worker counts must be a comma-separated subset of 1,2,4"
        ) from exc
    if not values or any(value not in {1, 2, 4} for value in values):
        raise argparse.ArgumentTypeError(
            "worker counts must be a comma-separated subset of 1,2,4"
        )
    return list(dict.fromkeys(values))


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
    build: Callable[[int, int], Workload]


def _base_system(
    num_cores: int,
    source: str,
    *,
    storage_image: str | None = None,
    worker_count: int = 1,
) -> tuple[MegapadSystem, dict[str, int]]:
    labels: dict[str, int] = {}
    code = assemble(source, base_addr=CODE_BASE, labels_out=labels)
    system = MegapadSystem(
        ram_size=RAM_SIZE,
        storage_image=storage_image,
        num_cores=num_cores,
        worker_count=worker_count,
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


def build_private_compute(
    num_cores: int,
    worker_count: int = 1,
) -> Workload:
    system, _ = _base_system(
        num_cores,
        PRIVATE_COMPUTE,
        worker_count=worker_count,
    )
    return Workload(system)


def build_shared_memory(
    num_cores: int,
    worker_count: int = 1,
) -> Workload:
    system, _ = _base_system(
        num_cores,
        SHARED_MEMORY,
        worker_count=worker_count,
    )
    for core_id, cpu in enumerate(system.cores):
        cpu.regs[4] = 0x1020_3040 ^ core_id
        cpu.regs[5] = SHARED_DATA_BASE
    return Workload(system)


def build_mmio_poll(
    num_cores: int,
    worker_count: int = 1,
) -> Workload:
    system, _ = _base_system(
        num_cores,
        MMIO_POLL,
        worker_count=worker_count,
    )
    timer_count_low = MMIO_BASE + TIMER_BASE
    system_info_num_cores = MMIO_BASE + SYSINFO_BASE + 0x10
    system.timer.counter = 0
    system.timer.control = 1
    for cpu in system.cores:
        cpu.regs[5] = timer_count_low
        cpu.regs[8] = system_info_num_cores
    return Workload(system)


def build_timer_interrupt(
    num_cores: int,
    worker_count: int = 1,
) -> Workload:
    system, labels = _base_system(
        num_cores,
        TIMER_INTERRUPT,
        worker_count=worker_count,
    )
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
    worker_count: int = 1,
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
        worker_count=worker_count,
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
            "The compact hot loop exercises real cache refill/hit behavior "
            "but is not a cache capacity or conflict-pressure workload.",
            "diagnostic_baseline",
            "private register/ALU execution only",
            build_private_compute,
        ),
        Scenario(
            "shared_memory",
            "All cores repeatedly write and read the same shared RAM word.",
            "The unbounded coordinator orders shared accesses but deliberately "
            "does not apply strict-cycle main-bus timing, so this records "
            "coordinator overhead rather than physical bus bandwidth.",
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


def _assembled_fixture(source: str) -> dict:
    labels: dict[str, int] = {}
    image = assemble(source, base_addr=CODE_BASE, labels_out=labels)
    return {
        "load_address": CODE_BASE,
        "assembled_image": _blob_summary(image),
        "labels": {
            name: int(address)
            for name, address in sorted(labels.items())
        },
    }


def _fixture_manifest(strict_dma_bytes: int) -> dict:
    sector_zero = bytes(index & 0xFF for index in range(SECTOR_SIZE))
    sector_one = bytes(
        (index * 73 + 41) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    nic_payload = bytes(
        (index * 73 + 41) & 0xFF
        for index in range(strict_dma_bytes)
    )
    disk_payload = bytes(
        (index * 13 + 7) & 0xFF
        for index in range(strict_dma_bytes)
    )
    manifest = {
        "schema": "megapad.phase3-benchmark-fixtures",
        "schema_version": 1,
        "assembled_programs": {
            "private_compute": _assembled_fixture(PRIVATE_COMPUTE),
            "shared_memory": _assembled_fixture(SHARED_MEMORY),
            "mmio_poll": _assembled_fixture(MMIO_POLL),
            "timer_interrupt": _assembled_fixture(TIMER_INTERRUPT),
            "legacy_storage_display_orchestration":
                _assembled_fixture(VRAM_WRITER),
            "strict_nic_disk_dma":
                _assembled_fixture(STRICT_DMA_COMMAND),
        },
        "payloads": {
            "legacy_storage_sector_zero": _blob_summary(sector_zero),
            "legacy_storage_sector_one": _blob_summary(sector_one),
            "strict_dma_nic_source": _blob_summary(nic_payload),
            "strict_dma_storage_media": _blob_summary(disk_payload),
        },
    }
    return {
        **manifest,
        "canonical_json_sha256": _json_sha256(manifest),
    }


def _host_worker_snapshot(system: MegapadSystem) -> dict:
    pool = dict(system._native_system._worker_pool_diagnostics())
    private = dict(system._native_system._private_worker_diagnostics())
    lanes = [
        {
            "lane_index": int(lane["lane_index"]),
            "auxiliary": bool(lane["auxiliary"]),
            "thread_token": int(lane["thread_token"]),
            "completed_commands": int(lane["completed_commands"]),
            "completed_steps": int(lane["completed_steps"]),
        }
        for lane in private["lanes"]
    ]
    return {
        "pool": {
            "schema_version": int(pool["schema_version"]),
            "worker_count": int(pool["worker_count"]),
            "auxiliary_worker_count": int(
                pool["auxiliary_worker_count"]
            ),
            "live_auxiliary_workers": int(
                pool["live_auxiliary_workers"]
            ),
            "launch_count": int(pool["launch_count"]),
            "inline_reference": bool(pool["inline_reference"]),
        },
        "private": {
            "schema_version": int(private["schema_version"]),
            "wave_epoch": int(private["wave_epoch"]),
            "next_command_sequence": int(
                private["next_command_sequence"]
            ),
            "wave_active": bool(private["wave_active"]),
            "lanes": lanes,
        },
    }


def _host_worker_diagnostics(
    before: dict,
    after: dict,
) -> dict:
    before_lanes = before["private"]["lanes"]
    after_lanes = after["private"]["lanes"]
    lane_deltas = [
        {
            "lane_index": int(after_lane["lane_index"]),
            "auxiliary": bool(after_lane["auxiliary"]),
            "completed_commands": (
                int(after_lane["completed_commands"])
                - int(before_lane["completed_commands"])
            ),
            "completed_steps": (
                int(after_lane["completed_steps"])
                - int(before_lane["completed_steps"])
            ),
        }
        for before_lane, after_lane in zip(
            before_lanes,
            after_lanes,
            strict=True,
        )
    ]
    return {
        "architectural_hash_scope": "excluded_host_only",
        "before": before,
        "after": after,
        "deltas": {
            "wave_epochs": (
                after["private"]["wave_epoch"]
                - before["private"]["wave_epoch"]
            ),
            "command_sequences": (
                after["private"]["next_command_sequence"]
                - before["private"]["next_command_sequence"]
            ),
            "lanes": lane_deltas,
        },
        "every_configured_lane_participated": all(
            lane["completed_commands"] > 0
            and lane["completed_steps"] > 0
            for lane in lane_deltas
        ),
    }


def _system_run_stats_state(result) -> dict:
    return {
        "instructions_executed": int(result.instructions_executed),
        "system_cycles_advanced": int(result.system_cycles_advanced),
        "per_core_instructions": [
            int(value) for value in result.per_core_instructions
        ],
        "per_core_cycles": [
            int(value) for value in result.per_core_cycles
        ],
        "per_core_dispatches": [
            int(value) for value in result.per_core_dispatches
        ],
        "per_core_stop_reasons": [
            [int(value) for value in reasons]
            for reasons in result.per_core_stop_reasons
        ],
        "native_scheduler": bool(result.native_scheduler),
        "native_rounds": int(result.native_rounds),
        "native_continuations": int(result.native_continuations),
        "system_stop_reason": str(result.system_stop_reason),
        "stop_cycle": int(result.stop_cycle),
        "event_source_mask": int(result.event_source_mask),
        "per_core_interrupts": [
            int(value) for value in result.per_core_interrupts
        ],
        "interrupts_delivered": int(result.interrupts_delivered),
        "external_events_applied": int(result.external_events_applied),
        "pending_interrupt_core": int(result.pending_interrupt_core),
        "pending_interrupt_vector": int(result.pending_interrupt_vector),
    }


def _integer_sequence_summary(values: Iterable[int]) -> dict:
    materialized = [int(value) for value in values]
    return {
        "element_count": len(materialized),
        "canonical_json_sha256": _json_sha256(materialized),
    }


def _enum_name(value) -> str:
    """Return one stable lowercase name for a bound native enum."""
    return str(value).rsplit(".", 1)[-1].lower()


def _bus_request_state(request) -> dict:
    return {
        "requester_id": int(request.requester_id),
        "ready_cycle": int(request.ready_cycle),
        "operation": _enum_name(request.operation),
        "address": int(request.address),
        "width": _enum_name(request.width),
        "write_data": int(request.write_data),
        "ordering": {
            "main_port_id": int(request.ordering.main_port_id),
            "issue_sequence": int(request.ordering.issue_sequence),
            "port_io": bool(request.ordering.port_io),
        },
    }


def _bus_grant_state(grant) -> dict:
    return {
        "request": _bus_request_state(grant.request),
        "grant_sequence": int(grant.grant_sequence),
        "grant_cycle": int(grant.grant_cycle),
        "target": _enum_name(grant.target),
        "timeout_cycle": int(grant.timeout_cycle),
    }


def _dma_coordinator_state(system: MegapadSystem) -> dict:
    snapshot = system._native_system._cycle_dma_snapshot()
    endpoints = []
    for endpoint in snapshot["endpoints"]:
        pending = endpoint["pending_request"]
        endpoints.append({
            "requester_id": int(endpoint["requester_id"]),
            "main_bus_port_id": int(endpoint["main_bus_port_id"]),
            "next_issue_sequence": int(
                endpoint["next_issue_sequence"]
            ),
            "highest_observed_token": int(
                endpoint["highest_observed_token"]
            ),
            "timeline_active": bool(endpoint["timeline_active"]),
            "pending_accepted": bool(endpoint["pending_accepted"]),
            "pending_token": (
                None
                if endpoint["pending_token"] is None
                else int(endpoint["pending_token"])
            ),
            "pending_request": (
                None if pending is None else _bus_request_state(pending)
            ),
        })
    return {
        "schema_version": int(snapshot["schema_version"]),
        "endpoints": endpoints,
        "active_target_forced_fault": (
            None
            if snapshot["active_target_forced_fault"] is None
            else _enum_name(snapshot["active_target_forced_fault"])
        ),
        "next_wots_forced_fault": (
            None
            if snapshot["next_wots_forced_fault"] is None
            else _enum_name(snapshot["next_wots_forced_fault"])
        ),
        "wots_dma_acceptance_suppressed": bool(
            snapshot["wots_dma_acceptance_suppressed"]
        ),
    }


def _native_wots_state(system: MegapadSystem) -> dict:
    snapshot = system.cpu._cs.crypto_wots_snapshot()
    return {
        "context_addr": int(snapshot["context_addr"]),
        "steps": int(snapshot["steps"]),
        "start": int(snapshot["start"]),
        "status": int(snapshot["status"]),
        "error": int(snapshot["error"]),
        "cycles": int(snapshot["cycles"]),
        "dout": _blob_summary(bytes(snapshot["dout"])),
        "phase": str(snapshot["phase"]),
        "active_context_addr": int(snapshot["active_context_addr"]),
        "active_steps": int(snapshot["active_steps"]),
        "active_start": int(snapshot["active_start"]),
        "dma_index": int(snapshot["dma_index"]),
        "chain_index": int(snapshot["chain_index"]),
        "next_dma_token": int(snapshot["next_dma_token"]),
        "dma_token": (
            None
            if snapshot["dma_token"] is None
            else int(snapshot["dma_token"])
        ),
        "dma_address": (
            None
            if snapshot["dma_address"] is None
            else int(snapshot["dma_address"])
        ),
        "dma_accepted": bool(snapshot["dma_accepted"]),
        "dma_accept_elapsed": int(snapshot["dma_accept_elapsed"]),
        "keccak_claimed": bool(snapshot["keccak_claimed"]),
        "clear_pending": bool(snapshot["clear_pending"]),
        "private_zeroized": bool(snapshot["private_zeroized"]),
        "topology": {
            "bank0_size": int(snapshot["bank0_size"]),
            "num_bus_ports": int(snapshot["num_bus_ports"]),
            "dma_accept_cycles": int(snapshot["dma_accept_cycles"]),
            "dma_beat_cycles": int(snapshot["dma_beat_cycles"]),
            "max_request_cycles": int(snapshot["max_request_cycles"]),
            "clear_cycles": int(snapshot["clear_cycles"]),
        },
    }


def _main_bus_state(system: MegapadSystem) -> dict:
    owner = system._native_system
    snapshot = owner._main_bus_snapshot()
    pending = owner._cycle_pending_bus_requests()
    return {
        "arbitration_contract": ARBITRATION_CONTRACT,
        "schema_version": int(snapshot.schema_version),
        "port_count": int(snapshot.port_count),
        "last_grant": int(snapshot.last_grant),
        "reset_port_zero_credit": bool(snapshot.reset_port_zero_credit),
        "next_grant_sequence": int(snapshot.next_grant_sequence),
        "earliest_arbitration_cycle": int(
            snapshot.earliest_arbitration_cycle
        ),
        "served_last": bool(snapshot.served_last),
        "last_arbitration_cycle": (
            None
            if snapshot.last_arbitration_cycle is None
            else int(snapshot.last_arbitration_cycle)
        ),
        "active_grant": (
            None
            if snapshot.active_grant is None
            else _bus_grant_state(snapshot.active_grant)
        ),
        "last_issue_sequences": [
            int(value) for value in snapshot.last_issue_sequences
        ],
        "sticky_bus_errors": [
            int(value) for value in snapshot.sticky_bus_errors
        ],
        "weights": [int(value) for value in snapshot.weights],
        "bandwidth_limits": [
            int(value) for value in snapshot.bandwidth_limits
        ],
        "bandwidth_counts": [
            int(value) for value in snapshot.bandwidth_counts
        ],
        "fixed_weight_one_unlimited": [
            bool(value)
            for value in snapshot.fixed_weight_one_unlimited
        ],
        "weight_remaining": int(snapshot.weight_remaining),
        "qos_epoch_start_cycle": int(snapshot.qos_epoch_start_cycle),
        "cycle_execution_pending": bool(
            owner.cycle_execution_pending
        ),
        "cycle_pending_requests": [
            _bus_request_state(request) for request in pending
        ],
        "dma_coordinator": _dma_coordinator_state(system),
    }


def _external_event_record_state(event) -> dict:
    return {
        "cycle": int(event.cycle),
        "sequence": int(event.sequence),
        "kind": _enum_name(event.kind),
        "payload": _blob_summary(bytes(event.payload)),
        "argument0": int(event.argument0),
        "argument1": int(event.argument1),
        "release_boundary": int(event.release_boundary),
        "release_phase": _enum_name(event.release_phase),
    }


def _external_event_journal_state(system: MegapadSystem) -> dict:
    owner = system._native_system
    pending = [
        _external_event_record_state(event)
        for event in owner.external_event_pending
    ]
    history = [
        _external_event_record_state(event)
        for event in owner.external_event_history
    ]
    return {
        "next_cycle": (
            None
            if owner.external_event_next_cycle is None
            else int(owner.external_event_next_cycle)
        ),
        "next_sequence": int(owner.external_event_next_sequence),
        "completed_batch_boundaries": int(
            owner.external_event_batch_boundaries
        ),
        "next_before_cycle": (
            None
            if owner.external_event_next_before_cycle is None
            else int(owner.external_event_next_before_cycle)
        ),
        "replay_sealed": bool(owner.external_event_replay_sealed),
        "pending": pending,
        "history": history,
        "pending_canonical_json_sha256": _json_sha256(pending),
        "history_canonical_json_sha256": _json_sha256(history),
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
    icache_valid, icache_tags, icache_data = cs.icache_snapshot()
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
            "tstride_c": int(cpu.tstride_c),
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
            "valid_lines": _blob_summary(icache_valid),
            "tags": _integer_sequence_summary(icache_tags),
            "data": _blob_summary(icache_data),
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
    sha3_readable_addresses = (
        tuple(range(0x0780, 0x0784))
        + (0x0788,)
        + tuple(range(0x0790, 0x07D1))
        + tuple(range(0x07D8, 0x07E0))
    )
    crypto_windows = {
        "aes_0x0700_0x076f": _blob_summary(bytes(
            cs.crypto_read8(address) for address in range(0x0700, 0x0770)
        )),
        "sha3_readable_0x0780_0x07df": _blob_summary(bytes(
            cs.crypto_read8(address) for address in sha3_readable_addresses
        )),
        "wots_0x08a0_0x08bf": _blob_summary(bytes(
            cs.crypto_read8(address) for address in range(0x08A0, 0x08C0)
        )),
    }
    nic_window = bytes(
        cs.nic_read8(address) for address in range(0x0400, 0x0480)
    )
    nic_dma = cs.nic_cycle_dma_snapshot()
    nic_pending = nic_dma["pending"]
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
            "cycle_dma": {
                "schema_version": int(nic_dma["schema_version"]),
                "rx_active": bool(nic_dma["rx_active"]),
                "tx_active": bool(nic_dma["tx_active"]),
                "rx_base": int(nic_dma["rx_base"]),
                "tx_base": int(nic_dma["tx_base"]),
                "tx_length": int(nic_dma["tx_length"]),
                "rx_index": int(nic_dma["rx_index"]),
                "tx_index": int(nic_dma["tx_index"]),
                "rx_frame": _blob_summary(nic_dma["rx_frame"]),
                "tx_frame": _blob_summary(nic_dma["tx_frame"]),
                "next_token": int(nic_dma["next_token"]),
                "pending": (
                    None
                    if nic_pending is None
                    else {
                        "token": int(nic_pending["token"]),
                        "owner": {
                            0: "none",
                            1: "rx",
                            2: "tx",
                        }[int(nic_pending["owner"])],
                        "address": int(nic_pending["address"]),
                        "write": bool(nic_pending["write"]),
                        "write_data": int(nic_pending["write_data"]),
                    }
                ),
            },
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
    dma_pending = storage._dma_pending
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
        "dma_fsm": {
            "strict_cycle_submission": bool(
                storage._strict_cycle_submission
            ),
            "async": bool(storage._dma_async),
            "phase": storage._dma_phase,
            "sector_index": int(storage._dma_sector_index),
            "byte_index": int(storage._dma_byte_index),
            "sector_data": _blob_summary(storage._dma_sector_data),
            "write_sector": _blob_summary(storage._dma_write_sector),
            "read_port_prefix": _blob_summary(
                storage._dma_read_port_prefix
            ),
            "pending": (
                None
                if dma_pending is None
                else {
                    "token": int(dma_pending.token),
                    "write": bool(dma_pending.write),
                    "address": int(dma_pending.address),
                    "write_data": int(dma_pending.write_data),
                }
            ),
            "next_token": int(storage._dma_next_token),
        },
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
    native_wots = _native_wots_state(system)
    return {
        "scheduler": {
            "next_core_index": int(system._scheduler_cursor),
        },
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
        "external_events": _external_event_journal_state(system),
        "main_bus": _main_bus_state(system),
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
        "native_wots": {
            "register_window": _blob_summary(bytes(
                wots.read8(offset) for offset in range(0x20)
            )),
            "snapshot": native_wots,
        },
    }


def _state_observation(workload: Workload) -> dict:
    with workload.system._scheduler_lock:
        return _state_observation_locked(workload)


def _state_observation_locked(workload: Workload) -> dict:
    system = workload.system
    coordinator = _dma_coordinator_state(system)
    storage_active, storage_pending = system.storage.cycle_dma_view()
    nic_dma = system.cpu._cs.nic_cycle_dma_snapshot()
    wots = _native_wots_state(system)
    endpoint_pending = any(
        endpoint["timeline_active"]
        or endpoint["pending_token"] is not None
        or endpoint["pending_request"] is not None
        for endpoint in coordinator["endpoints"]
    )
    device_pending = (
        storage_active
        or storage_pending is not None
        or bool(nic_dma["rx_active"])
        or bool(nic_dma["tx_active"])
        or nic_dma["pending"] is not None
        or wots["status"] == 1
        or wots["phase"] != "idle"
        or wots["dma_token"] is not None
        or wots["dma_accepted"]
        or wots["keccak_claimed"]
        or wots["clear_pending"]
        or not wots["private_zeroized"]
    )
    if (
        system._native_system.cycle_execution_pending
        or endpoint_pending
        or device_pending
    ):
        raise RuntimeError(
            "Phase 0 canonical state requires quiescent cycle, DMA, and "
            "WOTS execution"
        )
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


def _timed_sample(
    scenario: Scenario,
    num_cores: int,
    target: int,
    worker_count: int = 1,
) -> dict:
    workload = scenario.build(num_cores, worker_count)
    gc_enabled_before = gc.isenabled()
    collected_before_timing = gc.collect()
    workers_before = _host_worker_snapshot(workload.system)
    native_batches_before = int(
        workload.system._native_system.native_batch_runs
    )
    native_dispatches_before = int(
        workload.system._native_system.native_dispatches
    )
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
        native_batch_delta = (
            int(workload.system._native_system.native_batch_runs)
            - native_batches_before
        )
        native_dispatch_delta = (
            int(workload.system._native_system.native_dispatches)
            - native_dispatches_before
        )
        workers_after = _host_worker_snapshot(workload.system)
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
        "host_worker_diagnostics": _host_worker_diagnostics(
            workers_before,
            workers_after,
        ),
        "scheduler_provenance": {
            "native_batch_runs_counter_delta": native_batch_delta,
            "native_dispatches_counter_delta": native_dispatch_delta,
            "all_run_batch_calls_native":
                native_batch_delta == execution.run_batch_calls
                and native_dispatch_delta > 0,
        },
        "cyclic_gc": {
            "enabled_before_timing": gc_enabled_before,
            "collected_objects_before_timing": collected_before_timing,
            "enabled_during_timing": gc_enabled_during_timing,
            "disabled_by_harness": gc_enabled_before,
            "restored_to_prior_state": gc_restored_to_prior_state,
        },
        "observation": observation,
    }


def _write_le_register(write8, offset: int, value: int, width: int) -> None:
    for index in range(width):
        write8(
            offset + index,
            (value >> (8 * index)) & 0xFF,
        )


def _build_strict_nic_disk_dma(
    payload_bytes: int,
    worker_count: int = 1,
) -> tuple[Workload, dict]:
    if (
        payload_bytes % SECTOR_SIZE != 0
        or not SECTOR_SIZE <= payload_bytes <= 1024
    ):
        raise ValueError(
            "strict DMA payload must be 512 or 1024 bytes"
        )

    temp_dir = tempfile.TemporaryDirectory(
        prefix="megapad_phase0_strict_dma_"
    )
    image_path = Path(temp_dir.name) / "strict-dma-storage.img"
    nic_payload = bytes(
        (index * 73 + 41) & 0xFF
        for index in range(payload_bytes)
    )
    disk_payload = bytes(
        (index * 13 + 7) & 0xFF
        for index in range(payload_bytes)
    )

    try:
        image_path.write_bytes(disk_payload)
        system, _ = _base_system(
            2,
            STRICT_DMA_COMMAND,
            storage_image=str(image_path),
            worker_count=worker_count,
        )
        nic_source = DMA_DATA_BASE
        disk_target = DMA_DATA_BASE + 0x4000
        system.cpu.mem[
            nic_source:nic_source + payload_bytes
        ] = nic_payload

        native = system.cpu._cs
        _write_le_register(
            native.nic_write8,
            NIC_BASE + 0x02,
            nic_source,
            8,
        )
        _write_le_register(
            native.nic_write8,
            NIC_BASE + 0x0A,
            payload_bytes,
            2,
        )

        storage = system.storage
        storage.write8(0x01, STORAGE_STATUS_MEDIA_CHANGED)
        _write_le_register(storage.write8, 0x02, 0, 4)
        _write_le_register(
            storage.write8,
            0x06,
            disk_target,
            8,
        )
        storage.write8(0x0E, payload_bytes // SECTOR_SIZE)

        nic_core, disk_core = system.cores
        nic_core.regs[1] = MMIO_BASE + NIC_BASE
        nic_core.regs[2] = 0x01
        disk_core.regs[1] = MMIO_BASE + STORAGE_BASE
        disk_core.regs[2] = STORAGE_CMD_READ

        metrics = {
            "mode": "strict_cycle",
            "payload_bytes_per_endpoint": payload_bytes,
            "total_dma_payload_bytes": payload_bytes * 2,
            "nic_operation": "transmit_memory_read",
            "storage_operation": "read_media_to_memory",
            "nic_source": _blob_summary(nic_payload),
            "storage_media": _blob_summary(disk_payload),
            "arbitration_contract": ARBITRATION_CONTRACT,
            "ordering_evidence_scope": (
                "two continuously eligible default-policy NIC and disk peers"
            ),
        }
        workload = Workload(
            system,
            metrics={"strict_nic_disk_dma": metrics},
            cleanup=temp_dir.cleanup,
        )
        return workload, {
            "nic_payload": nic_payload,
            "disk_payload": disk_payload,
            "nic_source": nic_source,
            "disk_target": disk_target,
        }
    except Exception:
        temp_dir.cleanup()
        raise


def _execute_strict_nic_disk_dma(
    system: MegapadSystem,
    payload_bytes: int,
    *,
    cycle_slice: int | None,
) -> dict:
    owner = system._native_system
    before = owner._main_bus_snapshot()
    trace = []
    instructions = 0
    system_cycles = 0
    calls = 0
    final = None
    public_batch_results = []
    cycle_budget = payload_bytes * 8 + 256
    max_calls = (
        cycle_budget + 8
        if cycle_slice is not None
        else 2
    )

    for _ in range(max_calls):
        previous_grant_sequence = int(
            owner._main_bus_snapshot().next_grant_sequence
        )
        final = system.run_cycle_batch(
            cycle_budget if cycle_slice is None else cycle_slice,
            max_instructions=STRICT_DMA_MAX_INSTRUCTIONS,
        )
        public_batch_results.append(_system_run_stats_state(final))
        calls += 1
        instructions += int(final.instructions_executed)
        system_cycles += int(final.system_cycles_advanced)
        current_bus = owner._main_bus_snapshot()
        grant_delta = (
            int(current_bus.next_grant_sequence)
            - previous_grant_sequence
        )
        if cycle_slice is not None:
            if grant_delta > 1:
                raise RuntimeError(
                    "one-cycle DMA replay observed multiple grants"
                )
            if grant_delta == 1:
                trace.append(int(current_bus.last_grant))

        quiescent = (
            all(cpu.halted for cpu in system.cores)
            and not owner.cycle_execution_pending
            and not owner._cycle_pending_bus_requests()
            and owner._main_bus_snapshot().active_grant is None
        )
        if quiescent and final.system_stop_reason == "all_halted":
            break
        if cycle_slice is None:
            if quiescent:
                cycle_slice = 1
                continue
            raise RuntimeError(
                "strict DMA probe exceeded its one-shot cycle budget"
            )
    else:
        raise RuntimeError("strict DMA probe did not quiesce")

    return {
        "cycle_batch_calls": calls,
        "instructions_retired": instructions,
        "system_cycles_advanced": system_cycles,
        "stop_reason": final.system_stop_reason,
        "service_trace": trace,
        "ordered_public_batch_results": public_batch_results,
        "before_bus": before,
        "after_bus": owner._main_bus_snapshot(),
    }


def _strict_nic_disk_dma_sample(
    payload_bytes: int,
    *,
    worker_count: int = 1,
    cycle_slice: int | None = None,
    used_for_throughput: bool = True,
) -> dict:
    workload, context = _build_strict_nic_disk_dma(
        payload_bytes,
        worker_count,
    )
    system = workload.system
    owner = system._native_system
    gc_enabled_before = gc.isenabled()
    collected_before_timing = gc.collect()
    workers_before = _host_worker_snapshot(system)
    try:
        if gc_enabled_before:
            gc.disable()
        gc_enabled_during_timing = gc.isenabled()
        try:
            wall_start = time.perf_counter()
            cpu_start = time.process_time()
            execution = _execute_strict_nic_disk_dma(
                system,
                payload_bytes,
                cycle_slice=cycle_slice,
            )
            process_cpu_s = time.process_time() - cpu_start
            wall_s = time.perf_counter() - wall_start
        finally:
            if gc_enabled_before:
                gc.enable()
        gc_restored_to_prior_state = (
            gc.isenabled() == gc_enabled_before
        )

        before = execution.pop("before_bus")
        after = execution.pop("after_bus")
        nic_requester = owner.NIC_DMA_REQUESTER_ID
        disk_requester = owner.DISK_DMA_REQUESTER_ID
        nic_port = owner.main_bus_port_for_requester(nic_requester)
        disk_port = owner.main_bus_port_for_requester(disk_requester)
        issue_deltas = [
            int(end) - int(start)
            for start, end in zip(
                before.last_issue_sequences,
                after.last_issue_sequences,
            )
        ]
        grant_delta = (
            int(after.next_grant_sequence)
            - int(before.next_grant_sequence)
        )
        native = system.cpu._cs
        storage = system.storage
        destination = bytes(
            system.cpu.mem[
                context["disk_target"]:
                context["disk_target"] + payload_bytes
            ]
        )
        published_frames = list(system.nic.tx_queue)
        storage_status = storage.read8(0x01)
        coordinator = _dma_coordinator_state(system)
        checks = {
            "all_halted":
                execution["stop_reason"] == "all_halted",
            "nic_payload_exact":
                published_frames == [context["nic_payload"]],
            "storage_payload_exact":
                destination == context["disk_payload"],
            "nic_issue_count_exact":
                issue_deltas[nic_port] == payload_bytes,
            "disk_issue_count_exact":
                issue_deltas[disk_port] == payload_bytes,
            "command_issue_counts_exact":
                issue_deltas[:2] == [3, 3],
            "grant_count_exact":
                grant_delta == payload_bytes * 2 + 6,
            "storage_terminal_result":
                storage.result == STORAGE_RESULT_OK
                and storage.completion == 1
                and storage.transferred
                    == payload_bytes // SECTOR_SIZE,
            "storage_terminal_status_exact":
                storage_status
                == (
                    STORAGE_STATUS_PRESENT
                    | STORAGE_STATUS_RESULT_VALID
                )
                and not bool(
                    storage_status
                    & (
                        STORAGE_STATUS_BUSY
                        | STORAGE_STATUS_ERROR
                        | STORAGE_STATUS_REJECTED
                        | STORAGE_STATUS_MEDIA_CHANGED
                    )
                ),
            "nic_terminal_result":
                native.nic_get_tx_count() == 1
                and native.nic_tx_queue_size() == 1
                and not bool(
                    native.nic_read8(NIC_BASE + 0x01) & 0x01
                ),
            "coordinator_quiescent": (
                not owner.cycle_execution_pending
                and not owner._cycle_pending_bus_requests()
                and after.active_grant is None
                and all(
                    not endpoint["timeline_active"]
                    and endpoint["pending_token"] is None
                    and endpoint["pending_request"] is None
                    for endpoint in coordinator["endpoints"]
                )
            ),
            "no_sticky_bus_error":
                int(after.sticky_bus_errors[nic_port]) == 0
                and int(after.sticky_bus_errors[disk_port]) == 0,
        }
        failed = [
            name for name, passed in checks.items() if not passed
        ]
        if failed:
            raise RuntimeError(
                "strict NIC/disk DMA probe failed: "
                + ", ".join(failed)
            )

        deterministic_metrics = workload.metrics[
            "strict_nic_disk_dma"
        ]
        deterministic_metrics.update({
            "nic_requester_id": int(nic_requester),
            "disk_requester_id": int(disk_requester),
            "nic_main_bus_port_id": int(nic_port),
            "disk_main_bus_port_id": int(disk_port),
            "issue_sequence_deltas": issue_deltas,
            "main_bus_grant_sequence_delta": grant_delta,
            "published_nic_frames": _ordered_blob_queue(
                published_frames
            ),
            "storage_destination": _blob_summary(destination),
            "storage_result": int(storage.result),
            "storage_status": int(storage_status),
            "storage_completion": int(storage.completion),
            "storage_transferred_sectors": int(storage.transferred),
            "native_nic_tx_count": int(native.nic_get_tx_count()),
            "validation": checks,
        })
        workers_after = _host_worker_snapshot(system)
        observation = _state_observation(workload)
    finally:
        if gc.isenabled() != gc_enabled_before:
            if gc_enabled_before:
                gc.enable()
            else:
                gc.disable()
        workload.close()

    total_bytes = payload_bytes * 2
    return {
        "used_for_throughput": used_for_throughput,
        "worker_count": worker_count,
        "cycle_slice": cycle_slice,
        "payload_bytes_per_endpoint": payload_bytes,
        "total_dma_payload_bytes": total_bytes,
        "wall_time_s": wall_s,
        "process_cpu_time_s": process_cpu_s,
        "host_cpu_utilization_percent":
            (process_cpu_s / wall_s * 100.0) if wall_s else None,
        "dma_payload_bytes_per_s":
            (total_bytes / wall_s) if wall_s else None,
        "virtual_cycles_per_dma_payload_byte":
            execution["system_cycles_advanced"] / total_bytes,
        **execution,
        "host_worker_diagnostics": _host_worker_diagnostics(
            workers_before,
            workers_after,
        ),
        "main_bus": {
            "port_count": int(after.port_count),
            "nic_port_id": int(nic_port),
            "disk_port_id": int(disk_port),
            "issue_sequence_deltas": issue_deltas,
            "grant_sequence_delta": grant_delta,
            "sticky_bus_errors": [
                int(value) for value in after.sticky_bus_errors
            ],
        },
        "validation": checks,
        "cyclic_gc": {
            "enabled_before_timing": gc_enabled_before,
            "collected_objects_before_timing": collected_before_timing,
            "enabled_during_timing": gc_enabled_during_timing,
            "disabled_by_harness": gc_enabled_before,
            "restored_to_prior_state": gc_restored_to_prior_state,
        },
        "observation": observation,
    }


def _strict_nic_disk_dma_report(
    payload_bytes: int,
    *,
    repeats: int,
    warmups: int,
    worker_count: int = 1,
) -> dict:
    for _ in range(warmups):
        _strict_nic_disk_dma_sample(
            payload_bytes,
            worker_count=worker_count,
            used_for_throughput=False,
        )
    samples = [
        _strict_nic_disk_dma_sample(
            payload_bytes,
            worker_count=worker_count,
        )
        for _ in range(repeats)
    ]
    sliced = _strict_nic_disk_dma_sample(
        payload_bytes,
        worker_count=worker_count,
        cycle_slice=1,
        used_for_throughput=False,
    )
    nic_port = sliced["main_bus"]["nic_port_id"]
    disk_port = sliced["main_bus"]["disk_port_id"]
    expected_trace = [
        0,
        1,
        0,
        1,
        0,
        1,
        *[
            port
            for _ in range(payload_bytes)
            for port in (nic_port, disk_port)
        ],
    ]
    trace = sliced["service_trace"]
    timed_oracles = [
        sample["observation"]["behavior_oracle_sha256"]
        for sample in samples
    ]
    sliced_oracle = sliced["observation"][
        "behavior_oracle_sha256"
    ]
    def state_without_batch_boundary_count_sha256(
        sample: dict,
    ) -> str:
        observation = sample["observation"]
        state = json.loads(json.dumps(observation["canonical_state"]))
        state["shared_devices"]["external_events"].pop(
            "completed_batch_boundaries",
            None,
        )
        return _json_sha256({
            "canonical_state": state,
            "workload_metrics": observation["workload_metrics"],
        })

    timed_state_without_boundary_count = [
        state_without_batch_boundary_count_sha256(sample)
        for sample in samples
    ]
    sliced_state_without_boundary_count = (
        state_without_batch_boundary_count_sha256(sliced)
    )
    rates = [
        sample["dma_payload_bytes_per_s"] for sample in samples
    ]
    walls = [sample["wall_time_s"] for sample in samples]
    validations = {
        "all_timed_samples_valid": all(
            all(sample["validation"].values()) for sample in samples
        ),
        "sliced_replay_valid": all(sliced["validation"].values()),
        "timed_repeats_deterministic":
            len(set(timed_oracles)) == 1,
        "one_shot_and_sliced_state_equal_except_batch_boundary_count":
            bool(timed_state_without_boundary_count)
            and all(
                value == sliced_state_without_boundary_count
                for value in timed_state_without_boundary_count
            ),
        "default_weight_one_peer_trace_is_round_robin":
            trace == expected_trace,
    }
    return {
        "description": (
            "Two cold guest I-cache refills precede guest-issued commands "
            "that start native NIC TX and storage READ DMA on their physical "
            "strict-cycle main-bus ports."
        ),
        "arbitration_contract": ARBITRATION_CONTRACT,
        "configuration": {
            "full_cores": 2,
            "worker_count": worker_count,
            "host_execution_lanes": worker_count,
            "auxiliary_worker_count": worker_count - 1,
            "payload_bytes_per_endpoint": payload_bytes,
            "total_dma_payload_bytes_per_sample": payload_bytes * 2,
            "timed_repeats": repeats,
            "discarded_warmups": warmups,
            "sliced_oracle_cycle_budget": 1,
            "one_shot_cycle_budget": payload_bytes * 8 + 256,
            "max_instructions_per_cycle_batch":
                STRICT_DMA_MAX_INSTRUCTIONS,
            "ordering_evidence_scope": (
                "two continuously eligible default-policy NIC and disk peers"
            ),
        },
        "timed_samples": samples,
        "sliced_oracle_replay": sliced,
        "service_trace": {
            "ports": trace,
            "summary": _integer_sequence_summary(trace),
            "expected_summary":
                _integer_sequence_summary(expected_trace),
        },
        "summary": {
            "median_wall_time_s": statistics.median(walls),
            "median_dma_payload_bytes_per_s":
                statistics.median(rates),
            "median_dma_payload_mib_per_s":
                statistics.median(rates) / (1024 * 1024),
            "median_host_cpu_utilization_percent":
                statistics.median(
                    sample["host_cpu_utilization_percent"]
                    for sample in samples
                ),
            "virtual_cycles_per_dma_payload_byte":
                samples[0]["virtual_cycles_per_dma_payload_byte"],
            "timed_behavior_oracle_sha256": timed_oracles,
            "sliced_behavior_oracle_sha256": sliced_oracle,
            "timed_state_without_batch_boundary_count_sha256":
                timed_state_without_boundary_count,
            "sliced_state_without_batch_boundary_count_sha256":
                sliced_state_without_boundary_count,
        },
        "validation": validations,
    }


_CONCURRENCY_PROFILE_COUNT_FIELDS = (
    "batches",
    "prepare_batch_calls",
    "scheduler_rounds",
    "uncontended_rounds",
    "uncontended_dispatches",
    "uncontended_steps",
    "uncontended_continuations",
    "uncontended_callback_errors",
    "uncontended_interrupt_boundaries",
    "uncontended_block_lookups",
    "uncontended_block_hits",
    "uncontended_block_misses",
    "uncontended_block_build_attempts",
    "uncontended_block_nonresident_rejections",
    "uncontended_block_zero_instruction_rejections",
    "uncontended_block_one_instruction_rejections",
    "uncontended_block_structure_rejections",
    "uncontended_block_rejection_cache_hits",
    "uncontended_block_rejection_cache_stores",
    "uncontended_block_rejection_cache_replacements",
    "uncontended_block_builds",
    "uncontended_block_evictions",
    "uncontended_block_executions",
    "uncontended_block_steps",
    "uncontended_jit_compile_attempts",
    "uncontended_jit_compilations",
    "uncontended_jit_compile_failures",
    "uncontended_jit_plan_evictions",
    "uncontended_jit_arena_allocations",
    "uncontended_jit_arena_allocation_failures",
    "uncontended_jit_slot_publications",
    "uncontended_jit_slot_rewrites",
    "uncontended_jit_code_bytes",
    "uncontended_jit_max_code_bytes",
    "uncontended_jit_executions",
    "uncontended_jit_steps",
    "logical_subfrontiers",
    "round_absorptions",
    "worker_waves",
    "worker_commands",
    "frontier_routing_waves",
    "frontier_routing_commands",
    "frontier_preclassification_commands",
    "frontier_preclassification_calls",
    "worker_bypassed_commands",
    "private_steps",
    "private_classification_calls",
    "private_decode_cache_lookups",
    "private_decode_cache_hits",
    "private_decode_cache_misses",
    "micro_oracle_proof_reuses",
    "frontier_decode_cache_lookups",
    "frontier_decode_cache_hits",
    "frontier_decode_cache_misses",
    "zero_step_commands",
    "checkpoint_captures",
    "checkpoint_restores",
    "coordinator_boundaries",
    "settle_round_calls",
    "settle_round_native_calls",
    "settle_round_python_calls",
)

_CONCURRENCY_PROFILE_COUNT_MAP_FIELDS = (
    "private_stop_reasons",
    "worker_bypass_stop_reasons",
    "coordinator_boundary_origins",
)

_CONCURRENCY_PROFILE_COUNT_LIST_FIELDS = (
    "lane_commands",
    "lane_steps",
)

_CONCURRENCY_PROFILE_WALL_FIELDS = (
    "batch_total",
    "prepare_batch",
    "scheduler_round",
    "uncontended_round",
    "uncontended_dispatch",
    "uncontended_jit_compile",
    "uncontended_jit_arena_allocation",
    "uncontended_jit_publication",
    "logical_subfrontier",
    "round_absorption",
    "worker_wave",
    "worker_wave_prepare",
    "worker_wave_wait",
    "worker_wave_gather",
    "frontier_fast_path",
    "private_command_sum",
    "private_command_max",
    "private_scope_setup",
    "checkpoint_capture",
    "checkpoint_restore",
    "coordinator_boundary",
    "settle_round",
)


def _normalized_concurrency_profile_snapshot(owner) -> dict:
    """Copy the fixed native host-profile schema into JSON-native values."""
    raw = dict(owner._concurrency_profile_snapshot())
    raw_counts = dict(raw["counts"])
    raw_wall = dict(raw["wall_ns"])
    raw_jit_storage = dict(raw["single_core_jit_storage"])
    raw_block_rejection_cache = dict(
        raw["single_core_block_rejection_cache"]
    )
    counts = {
        name: int(raw_counts[name])
        for name in _CONCURRENCY_PROFILE_COUNT_FIELDS
    }
    counts.update({
        name: {
            str(key): int(value)
            for key, value in sorted(dict(raw_counts[name]).items())
        }
        for name in _CONCURRENCY_PROFILE_COUNT_MAP_FIELDS
    })
    counts.update({
        name: [int(value) for value in raw_counts[name]]
        for name in _CONCURRENCY_PROFILE_COUNT_LIST_FIELDS
    })
    wall_ns = {
        name: int(raw_wall[name])
        for name in _CONCURRENCY_PROFILE_WALL_FIELDS
    }
    wall_ns["coordinator_boundary_origins"] = {
        str(key): int(value)
        for key, value in sorted(
            dict(raw_wall["coordinator_boundary_origins"]).items()
        )
    }
    return {
        "schema_version": int(raw["schema_version"]),
        "enabled": bool(raw["enabled"]),
        "generation": int(raw["generation"]),
        "architectural_hash_scope": str(raw["architectural_hash_scope"]),
        "measurement_scope": str(raw["measurement_scope"]),
        "timing_semantics": str(raw["timing_semantics"]),
        "single_core_jit_backend": str(
            raw["single_core_jit_backend"]
        ),
        "single_core_jit_storage": {
            "kind": str(raw_jit_storage["kind"]),
            "w_x_model": str(raw_jit_storage["w_x_model"]),
            "ready": bool(raw_jit_storage["ready"]),
            "failed": bool(raw_jit_storage["failed"]),
            "slot_count": int(raw_jit_storage["slot_count"]),
            "slot_bytes": int(raw_jit_storage["slot_bytes"]),
            "mapped_bytes_per_alias": int(
                raw_jit_storage["mapped_bytes_per_alias"]
            ),
        },
        "single_core_block_rejection_cache": {
            "kind": str(raw_block_rejection_cache["kind"]),
            "entries": int(raw_block_rejection_cache["entries"]),
            "identity_bytes": int(
                raw_block_rejection_cache["identity_bytes"]
            ),
        },
        "counts": counts,
        "wall_ns": wall_ns,
        "lane_active_ns": [
            int(value) for value in raw["lane_active_ns"]
        ],
    }


def _freeze_python_callback_profile(profile: dict) -> dict:
    """Freeze accounting wrappers before canonical state observation."""
    per_core = [
        {
            "core_id": int(entry["core_id"]),
            "mmio_read_calls": int(entry["mmio_read_calls"]),
            "mmio_read_ns": int(entry["mmio_read_ns"]),
            "mmio_write_calls": int(entry["mmio_write_calls"]),
            "mmio_write_ns": int(entry["mmio_write_ns"]),
        }
        for entry in profile["per_core"]
    ]
    tick = profile["device_tick"]
    return {
        "per_core": per_core,
        "mmio_read_calls": sum(
            entry["mmio_read_calls"] for entry in per_core
        ),
        "mmio_read_ns": sum(entry["mmio_read_ns"] for entry in per_core),
        "mmio_write_calls": sum(
            entry["mmio_write_calls"] for entry in per_core
        ),
        "mmio_write_ns": sum(entry["mmio_write_ns"] for entry in per_core),
        "device_tick_calls": int(tick["calls"]),
        "device_tick_ns": int(tick["wall_ns"]),
        "device_tick_argument_units": int(tick["argument_units"]),
    }


def _optional_ratio(numerator: int, denominator: int) -> float | None:
    return None if denominator == 0 else numerator / denominator


def _host_profile_probe(
    *,
    native_snapshot: dict,
    python_callbacks: dict,
    accounting: dict,
) -> dict:
    """Build compact structural attribution and cross-layer reconciliations."""
    native_counts = native_snapshot["counts"]
    native_wall = native_snapshot["wall_ns"]
    jit_storage = native_snapshot["single_core_jit_storage"]
    block_rejection_cache = native_snapshot[
        "single_core_block_rejection_cache"
    ]
    jit_backend_available = (
        native_snapshot["single_core_jit_backend"] == "x86_64"
    )
    scheduler = accounting["scheduler_provenance"]
    worker = accounting["host_worker_diagnostics"]["deltas"]
    worker_lanes = worker["lanes"]
    worker_lane_commands = [
        int(lane["completed_commands"]) for lane in worker_lanes
    ]
    worker_lane_steps = [
        int(lane["completed_steps"]) for lane in worker_lanes
    ]
    accounting_mmio_reads = sum(
        int(core["python_mmio_reads"])
        for core in accounting["per_core"]
    )
    accounting_mmio_writes = sum(
        int(core["python_mmio_writes"])
        for core in accounting["per_core"]
    )
    returned_instructions = int(
        accounting["aggregate_instructions_from_per_core"]
    )
    stop_reason_total = sum(
        native_counts["private_stop_reasons"].values()
    )
    bypass_stop_reason_total = sum(
        native_counts["worker_bypass_stop_reasons"].values()
    )
    boundary_origin_total = sum(
        native_counts["coordinator_boundary_origins"].values()
    )
    worker_count = int(accounting["worker_count"])
    logical_private_commands = (
        native_counts["worker_commands"]
        + native_counts["worker_bypassed_commands"]
    )
    logical_zero_step_commands = (
        native_counts["zero_step_commands"]
        + native_counts["worker_bypassed_commands"]
    )
    total_classification_calls = (
        native_counts["private_classification_calls"]
        + native_counts["frontier_preclassification_calls"]
    )

    validation = {
        "native_profile_schema_supported":
            native_snapshot["schema_version"] == 11,
        "native_profile_frozen": not native_snapshot["enabled"],
        "native_profile_generation_positive":
            native_snapshot["generation"] > 0,
        "native_profile_excluded_from_architectural_hash":
            native_snapshot["architectural_hash_scope"]
            == "excluded_host_only",
        "native_profile_scope_is_unbounded_batch_only":
            native_snapshot["measurement_scope"]
            == "unbounded_native_system_batch_only",
        "native_profile_timers_are_inclusive_nested_wall_time":
            native_snapshot["timing_semantics"]
            == "inclusive_nested_host_wall_nanoseconds",
        "single_core_jit_storage_backend_matches": (
            (
                jit_storage["kind"]
                == "memfd-dual-mapped-fixed-slots"
                and jit_storage["w_x_model"]
                == "distinct-rw-and-rx-aliases"
            )
            if jit_backend_available
            else (
                jit_storage["kind"] == "unavailable"
                and jit_storage["w_x_model"] == "unavailable"
            )
        ),
        "single_core_jit_storage_geometry_is_bounded": (
            (
                jit_backend_available
                and not jit_storage["failed"]
                and jit_storage["slot_count"] > 0
                and jit_storage["slot_bytes"] > 0
                and jit_storage["mapped_bytes_per_alias"]
                == jit_storage["slot_count"]
                * jit_storage["slot_bytes"]
                and native_counts["uncontended_jit_max_code_bytes"]
                <= jit_storage["slot_bytes"]
            )
            if jit_storage["ready"]
            else (
                jit_storage["slot_count"] == 0
                and jit_storage["slot_bytes"] == 0
                and jit_storage["mapped_bytes_per_alias"] == 0
                and native_counts["uncontended_jit_max_code_bytes"] == 0
            )
        ),
        "single_core_jit_use_has_ready_storage": (
            native_counts["uncontended_jit_slot_publications"] == 0
            or (
                jit_backend_available
                and jit_storage["ready"]
                and not jit_storage["failed"]
            )
        ),
        "single_core_block_rejection_cache_is_bounded_exact_span": (
            block_rejection_cache["kind"]
            == "direct-mapped-exact-icache-span"
            and block_rejection_cache["entries"] == 512
            and block_rejection_cache["identity_bytes"] == 16
        ),
        "native_batches_match_accounting": (
            native_counts["batches"]
            == scheduler["native_system_batch_calls"]
            == scheduler["native_batch_runs_counter_delta"]
        ),
        "prepare_batch_calls_match_batches":
            native_counts["prepare_batch_calls"]
            == native_counts["batches"],
        "scheduler_rounds_match_accounting":
            native_counts["scheduler_rounds"]
            == scheduler["native_rounds"],
        "uncontended_rounds_within_scheduler_rounds": (
            0 <= native_counts["uncontended_rounds"]
            <= native_counts["scheduler_rounds"]
        ),
        "uncontended_path_covers_singleton_accounting": (
            (
                native_counts["uncontended_rounds"]
                == native_counts["scheduler_rounds"]
                and native_counts["uncontended_steps"]
                == returned_instructions
                and native_counts["uncontended_dispatches"]
                == scheduler["reported_native_dispatches"]
                and native_counts["uncontended_continuations"]
                == scheduler["native_continuations"]
            )
            if native_counts["uncontended_rounds"] > 0
            else all(
                native_counts[name] == 0
                for name in (
                    "uncontended_dispatches",
                    "uncontended_steps",
                    "uncontended_continuations",
                    "uncontended_callback_errors",
                    "uncontended_interrupt_boundaries",
                    "uncontended_block_lookups",
                    "uncontended_block_hits",
                    "uncontended_block_misses",
                    "uncontended_block_build_attempts",
                    "uncontended_block_nonresident_rejections",
                    "uncontended_block_zero_instruction_rejections",
                    "uncontended_block_one_instruction_rejections",
                    "uncontended_block_structure_rejections",
                    "uncontended_block_rejection_cache_hits",
                    "uncontended_block_rejection_cache_stores",
                    "uncontended_block_rejection_cache_replacements",
                    "uncontended_block_builds",
                    "uncontended_block_evictions",
                    "uncontended_block_executions",
                    "uncontended_block_steps",
                    "uncontended_jit_compile_attempts",
                    "uncontended_jit_compilations",
                    "uncontended_jit_compile_failures",
                    "uncontended_jit_plan_evictions",
                    "uncontended_jit_arena_allocations",
                    "uncontended_jit_arena_allocation_failures",
                    "uncontended_jit_slot_publications",
                    "uncontended_jit_slot_rewrites",
                    "uncontended_jit_code_bytes",
                    "uncontended_jit_max_code_bytes",
                    "uncontended_jit_executions",
                    "uncontended_jit_steps",
                )
            )
        ),
        "uncontended_callback_errors_within_dispatches": (
            0 <= native_counts["uncontended_callback_errors"]
            <= native_counts["uncontended_dispatches"]
        ),
        "uncontended_interrupts_within_rounds": (
            0 <= native_counts["uncontended_interrupt_boundaries"]
            <= native_counts["uncontended_rounds"]
        ),
        "uncontended_block_cache_counts_reconcile": (
            native_counts["uncontended_block_lookups"]
            == native_counts["uncontended_block_hits"]
            + native_counts["uncontended_block_misses"]
        ),
        "uncontended_block_builds_within_misses": (
            native_counts["uncontended_block_builds"]
            <= native_counts["uncontended_block_misses"]
        ),
        "uncontended_block_build_attempts_reconcile": (
            native_counts["uncontended_block_build_attempts"]
            == native_counts["uncontended_block_builds"]
            + native_counts[
                "uncontended_block_nonresident_rejections"
            ]
            + native_counts[
                "uncontended_block_zero_instruction_rejections"
            ]
            + native_counts[
                "uncontended_block_one_instruction_rejections"
            ]
            + native_counts["uncontended_block_structure_rejections"]
        ),
        "uncontended_block_rejection_cache_stores_reconcile": (
            native_counts["uncontended_block_rejection_cache_stores"]
            == native_counts[
                "uncontended_block_zero_instruction_rejections"
            ]
            + native_counts[
                "uncontended_block_one_instruction_rejections"
            ]
            + native_counts["uncontended_block_structure_rejections"]
        ),
        "uncontended_block_rejection_cache_replacements_within_stores": (
            native_counts[
                "uncontended_block_rejection_cache_replacements"
            ]
            <= native_counts["uncontended_block_rejection_cache_stores"]
        ),
        "uncontended_block_miss_paths_reconcile": (
            native_counts["uncontended_block_rejection_cache_hits"]
            + native_counts["uncontended_block_build_attempts"]
            == native_counts["uncontended_block_misses"]
        ),
        "uncontended_block_evictions_within_builds": (
            native_counts["uncontended_block_evictions"]
            <= native_counts["uncontended_block_builds"]
        ),
        "uncontended_jit_plan_evictions_within_builds": (
            native_counts["uncontended_jit_plan_evictions"]
            <= native_counts["uncontended_block_builds"]
        ),
        "uncontended_block_executions_have_plans": (
            native_counts["uncontended_block_executions"]
            <= native_counts["uncontended_block_hits"]
            + native_counts["uncontended_block_builds"]
        ),
        "uncontended_block_steps_within_uncontended_steps": (
            native_counts["uncontended_block_executions"]
            <= native_counts["uncontended_block_steps"]
            <= native_counts["uncontended_steps"]
        ),
        "uncontended_jit_compilation_counts_reconcile": (
            native_counts["uncontended_jit_compile_attempts"]
            == native_counts["uncontended_jit_compilations"]
            + native_counts["uncontended_jit_compile_failures"]
        ),
        "uncontended_jit_publications_match_compilations": (
            native_counts["uncontended_jit_slot_publications"]
            == native_counts["uncontended_jit_compilations"]
        ),
        "uncontended_jit_rewrites_within_publications": (
            native_counts["uncontended_jit_slot_rewrites"]
            <= native_counts["uncontended_jit_slot_publications"]
        ),
        "uncontended_jit_arena_allocations_within_attempts": (
            native_counts["uncontended_jit_arena_allocations"]
            <= native_counts["uncontended_jit_compile_attempts"]
        ),
        "uncontended_jit_arena_failures_within_attempts": (
            native_counts[
                "uncontended_jit_arena_allocation_failures"
            ]
            <= native_counts["uncontended_jit_compile_attempts"]
        ),
        "uncontended_jit_arena_allocation_time_within_compile_time": (
            native_wall["uncontended_jit_arena_allocation"]
            <= native_wall["uncontended_jit_compile"]
        ),
        "uncontended_jit_publication_time_within_compile_time": (
            native_wall["uncontended_jit_publication"]
            <= native_wall["uncontended_jit_compile"]
        ),
        "uncontended_jit_attempts_within_block_hits": (
            native_counts["uncontended_jit_compile_attempts"]
            <= native_counts["uncontended_block_hits"]
        ),
        "uncontended_jit_executions_within_block_executions": (
            native_counts["uncontended_jit_executions"]
            <= native_counts["uncontended_block_executions"]
        ),
        "uncontended_jit_steps_within_block_steps": (
            native_counts["uncontended_jit_executions"]
            <= native_counts["uncontended_jit_steps"]
            <= native_counts["uncontended_block_steps"]
        ),
        "round_absorptions_match_logical_subfrontiers":
            native_counts["round_absorptions"]
            == native_counts["logical_subfrontiers"],
        "worker_waves_match_worker_diagnostics":
            native_counts["worker_waves"] == worker["wave_epochs"],
        "worker_commands_match_worker_diagnostics":
            native_counts["worker_commands"] == worker["command_sequences"],
        "frontier_routing_matches_logical_commands": (
            native_counts["frontier_routing_commands"]
            == logical_private_commands
        ),
        "worker_waves_within_frontier_routing": (
            0 <= native_counts["worker_waves"]
            <= native_counts["frontier_routing_waves"]
        ),
        "frontier_preclassification_within_routing": (
            0
            <= native_counts["frontier_preclassification_commands"]
            <= native_counts["frontier_routing_commands"]
        ),
        "worker_bypass_reasons_match_bypassed_commands": (
            bypass_stop_reason_total
            == native_counts["worker_bypassed_commands"]
        ),
        "private_steps_match_worker_diagnostics":
            native_counts["private_steps"] == sum(worker_lane_steps),
        "native_lane_arrays_match_worker_count": (
            len(native_counts["lane_commands"]) == worker_count
            and len(native_counts["lane_steps"]) == worker_count
            and len(native_snapshot["lane_active_ns"]) == worker_count
        ),
        "lane_commands_match_worker_diagnostics":
            native_counts["lane_commands"] == worker_lane_commands,
        "lane_steps_match_worker_diagnostics":
            native_counts["lane_steps"] == worker_lane_steps,
        "native_lane_totals_match_native_totals": (
            sum(native_counts["lane_commands"])
            == native_counts["worker_commands"]
            and sum(native_counts["lane_steps"])
            == native_counts["private_steps"]
        ),
        "private_stop_reasons_match_worker_commands":
            stop_reason_total == native_counts["worker_commands"],
        "coordinator_boundary_origins_match_boundary_count":
            boundary_origin_total == native_counts["coordinator_boundaries"],
        "settle_round_calls_match_rounds_plus_batches": (
            native_counts["settle_round_calls"]
            == native_counts["scheduler_rounds"]
            + native_counts["batches"]
        ),
        "settle_round_routes_reconcile": (
            native_counts["settle_round_calls"]
            == native_counts["settle_round_native_calls"]
            + native_counts["settle_round_python_calls"]
        ),
        "classification_covers_private_steps":
            total_classification_calls
            >= native_counts["private_steps"],
        "private_decode_cache_counts_reconcile": (
            native_counts["private_decode_cache_lookups"]
            == native_counts["private_decode_cache_hits"]
            + native_counts["private_decode_cache_misses"]
        ),
        "private_decode_cache_lookups_within_classification": (
            native_counts["private_decode_cache_lookups"]
            <= native_counts["private_classification_calls"]
        ),
        "frontier_decode_cache_counts_reconcile": (
            native_counts["frontier_decode_cache_lookups"]
            == native_counts["frontier_decode_cache_hits"]
            + native_counts["frontier_decode_cache_misses"]
        ),
        "frontier_decode_cache_lookups_within_classification": (
            native_counts["frontier_decode_cache_lookups"]
            <= native_counts["frontier_preclassification_calls"]
        ),
        "micro_oracle_proof_reuses_within_private_steps": (
            native_counts["micro_oracle_proof_reuses"]
            <= native_counts["private_steps"]
        ),
        "frontier_classifications_within_preclassification": (
            0
            <= native_counts["frontier_preclassification_calls"]
            <= native_counts["frontier_preclassification_commands"]
        ),
        "zero_step_commands_within_worker_commands": (
            0 <= native_counts["zero_step_commands"]
            <= native_counts["worker_commands"]
        ),
        "checkpoints_cover_progressing_worker_commands": (
            native_counts["worker_commands"]
            - native_counts["zero_step_commands"]
            <= native_counts["checkpoint_captures"]
            <= native_counts["worker_commands"]
        ),
        "checkpoint_restores_within_captures": (
            0 <= native_counts["checkpoint_restores"]
            <= native_counts["checkpoint_captures"]
        ),
        "bypassed_commands_within_preclassification": (
            0 <= native_counts["worker_bypassed_commands"]
            <= native_counts["frontier_preclassification_commands"]
        ),
        "python_mmio_reads_match_accounting":
            python_callbacks["mmio_read_calls"] == accounting_mmio_reads,
        "python_mmio_writes_match_accounting":
            python_callbacks["mmio_write_calls"] == accounting_mmio_writes,
        "device_tick_calls_match_accounting":
            python_callbacks["device_tick_calls"]
            == accounting["device_bus_tick_calls"],
        "device_tick_units_match_accounting":
            python_callbacks["device_tick_argument_units"]
            == accounting["device_bus_tick_argument_units"],
        "device_tick_calls_match_scheduler_rounds":
            python_callbacks["device_tick_calls"]
            == native_counts["scheduler_rounds"],
        "native_wall_times_nonnegative": (
            all(value >= 0 for value in native_wall.values()
                if not isinstance(value, dict))
            and all(
                value >= 0
                for value in native_wall[
                    "coordinator_boundary_origins"
                ].values()
            )
            and all(value >= 0 for value in native_snapshot["lane_active_ns"])
        ),
        "python_callback_wall_times_nonnegative": all(
            python_callbacks[name] >= 0
            for name in (
                "mmio_read_ns",
                "mmio_write_ns",
                "device_tick_ns",
            )
        ),
    }
    return {
        "schema": "megapad.phase4-concurrency-host-profile",
        "schema_version": 11,
        "architectural_hash_scope": "excluded_host_only",
        "used_for_throughput": False,
        "native_snapshot": native_snapshot,
        "python_callbacks": python_callbacks,
        "structural_ratios": {
            "uncontended_steps_per_dispatch": _optional_ratio(
                native_counts["uncontended_steps"],
                native_counts["uncontended_dispatches"],
            ),
            "uncontended_step_fraction_of_returned_instructions":
                _optional_ratio(
                    native_counts["uncontended_steps"],
                    returned_instructions,
                ),
            "worker_commands_per_wave": _optional_ratio(
                native_counts["worker_commands"],
                native_counts["worker_waves"],
            ),
            "worker_wave_bypass_fraction": _optional_ratio(
                native_counts["frontier_routing_waves"]
                - native_counts["worker_waves"],
                native_counts["frontier_routing_waves"],
            ),
            "private_steps_per_worker_command": _optional_ratio(
                native_counts["private_steps"],
                native_counts["worker_commands"],
            ),
            "private_steps_per_logical_command": _optional_ratio(
                native_counts["private_steps"],
                logical_private_commands,
            ),
            "classification_calls_per_private_step": _optional_ratio(
                total_classification_calls,
                native_counts["private_steps"],
            ),
            "private_decode_cache_hit_fraction": _optional_ratio(
                native_counts["private_decode_cache_hits"],
                native_counts["private_decode_cache_lookups"],
            ),
            "frontier_decode_cache_hit_fraction": _optional_ratio(
                native_counts["frontier_decode_cache_hits"],
                native_counts["frontier_decode_cache_lookups"],
            ),
            "micro_oracle_proof_reuse_fraction": _optional_ratio(
                native_counts["micro_oracle_proof_reuses"],
                native_counts["private_steps"],
            ),
            "zero_step_command_fraction": _optional_ratio(
                logical_zero_step_commands,
                logical_private_commands,
            ),
            "worker_bypass_fraction": _optional_ratio(
                native_counts["worker_bypassed_commands"],
                logical_private_commands,
            ),
            "returned_instructions_per_logical_subfrontier": _optional_ratio(
                returned_instructions,
                native_counts["logical_subfrontiers"],
            ),
            "coordinator_boundaries_per_returned_instruction":
                _optional_ratio(
                    native_counts["coordinator_boundaries"],
                    returned_instructions,
                ),
            "private_step_fraction_of_returned_instructions":
                _optional_ratio(
                    native_counts["private_steps"],
                    returned_instructions,
                ),
        },
        "validation": validation,
    }


def _install_accounting(
    workload: Workload,
    *,
    host_profile: bool = False,
) -> tuple[list[dict], dict, dict, dict]:
    """Install accounting-only wrappers; never used for timed throughput."""
    stop_reason_names = (
        "run_limit",
        "halt",
        "idle",
        "mex_fallback",
        "ext_fallback",
        "trap",
        "reset",
    )
    core_stats = []
    callback_profile = {
        "per_core": [],
        "device_tick": {
            "calls": 0,
            "wall_ns": 0,
            "argument_units": 0,
        },
    }
    for cpu in workload.system.cores:
        stats = {
            "core_id": cpu.core_id,
            "instructions": 0,
            "scheduler_cycles": 0,
            "native_dispatches": 0,
            "native_stop_reasons": {
                reason: 0 for reason in stop_reason_names
            },
            "python_mmio_reads": 0,
            "python_mmio_writes": 0,
        }
        core_stats.append(stats)
        profile_stats = {
            "core_id": cpu.core_id,
            "mmio_read_calls": 0,
            "mmio_read_ns": 0,
            "mmio_write_calls": 0,
            "mmio_write_ns": 0,
        }
        callback_profile["per_core"].append(profile_stats)

        original_read = cpu._mmio_read8
        original_write = cpu._mmio_write8

        def counted_read(
            addr: int,
            *,
            _original=original_read,
            _stats=stats,
            _profile=profile_stats,
        ):
            _stats["python_mmio_reads"] += 1
            if not host_profile:
                return _original(addr)
            _profile["mmio_read_calls"] += 1
            started_ns = time.perf_counter_ns()
            try:
                return _original(addr)
            finally:
                _profile["mmio_read_ns"] += (
                    time.perf_counter_ns() - started_ns
                )

        def counted_write(
            addr: int,
            value: int,
            *,
            _original=original_write,
            _stats=stats,
            _profile=profile_stats,
        ):
            _stats["python_mmio_writes"] += 1
            if not host_profile:
                return _original(addr, value)
            _profile["mmio_write_calls"] += 1
            started_ns = time.perf_counter_ns()
            try:
                return _original(addr, value)
            finally:
                _profile["mmio_write_ns"] += (
                    time.perf_counter_ns() - started_ns
                )

        cpu._mmio_read8 = counted_read
        cpu._mmio_write8 = counted_write

    scheduler_stats = {
        "system_batch_calls": 0,
        "native_system_batch_calls": 0,
        "compatibility_system_batch_calls": 0,
        "native_rounds": 0,
        "native_continuations": 0,
        "ordered_public_batch_results": [],
    }
    original_run_batch_stats = workload.system.run_batch_stats

    def counted_run_batch_stats(max_steps: int):
        result = original_run_batch_stats(max_steps)
        core_count = len(core_stats)
        if (
            len(result.per_core_instructions) != core_count
            or len(result.per_core_cycles) != core_count
        ):
            raise RuntimeError(
                "system batch returned incomplete per-core accounting"
            )

        scheduler_stats["system_batch_calls"] += 1
        scheduler_stats[
            "native_system_batch_calls"
            if result.native_scheduler
            else "compatibility_system_batch_calls"
        ] += 1
        scheduler_stats["native_rounds"] += int(result.native_rounds)
        scheduler_stats["native_continuations"] += int(
            result.native_continuations
        )
        scheduler_stats["ordered_public_batch_results"].append(
            _system_run_stats_state(result)
        )

        dispatches = result.per_core_dispatches or (0,) * core_count
        stop_reasons = result.per_core_stop_reasons or ((),) * core_count
        if len(dispatches) != core_count or len(stop_reasons) != core_count:
            raise RuntimeError(
                "system batch returned incomplete scheduler diagnostics"
            )

        for index, stats in enumerate(core_stats):
            stats["instructions"] += int(
                result.per_core_instructions[index]
            )
            stats["scheduler_cycles"] += int(
                result.per_core_cycles[index]
            )
            stats["native_dispatches"] += int(dispatches[index])
            for reason, count in enumerate(stop_reasons[index]):
                count = int(count)
                if count == 0:
                    continue
                reason_key = (
                    stop_reason_names[reason]
                    if reason < len(stop_reason_names)
                    else f"unknown_{reason}"
                )
                stats["native_stop_reasons"][reason_key] = (
                    stats["native_stop_reasons"].get(reason_key, 0) + count
                )
        return result

    workload.system.run_batch_stats = counted_run_batch_stats

    bus_stats = {"tick_calls": 0, "tick_argument_units": 0}
    original_tick = workload.system.bus.tick

    def counted_tick(units: int):
        bus_stats["tick_calls"] += 1
        bus_stats["tick_argument_units"] += int(units)
        if not host_profile:
            return original_tick(units)
        tick_profile = callback_profile["device_tick"]
        tick_profile["calls"] += 1
        tick_profile["argument_units"] += int(units)
        started_ns = time.perf_counter_ns()
        try:
            return original_tick(units)
        finally:
            tick_profile["wall_ns"] += (
                time.perf_counter_ns() - started_ns
            )

    workload.system.bus.tick = counted_tick
    return core_stats, bus_stats, scheduler_stats, callback_profile


def _accounting_probe(
    scenario: Scenario,
    num_cores: int,
    target: int,
    worker_count: int = 1,
    *,
    host_profile: bool = False,
) -> tuple[dict, dict | None]:
    workload = scenario.build(num_cores, worker_count)
    owner = workload.system._native_system
    (
        core_stats,
        bus_stats,
        scheduler_stats,
        callback_profile,
    ) = _install_accounting(
        workload,
        host_profile=host_profile,
    )
    workers_before = _host_worker_snapshot(workload.system)
    start_cycles = [int(cpu.cycle_count) for cpu in workload.system.cores]
    start_system_cycles = int(owner.system_cycles)
    start_native_batches = int(owner.native_batch_runs)
    start_native_dispatches = int(owner.native_dispatches)
    profile_started = False
    native_profile_snapshot = None
    python_callback_snapshot = None
    try:
        if host_profile:
            owner._start_concurrency_profile()
            profile_started = True
        execution = workload.execute(target)

        # Freeze every host-only timing/count source before canonical state
        # observation. Profiling therefore covers only the existing untimed
        # accounting replay and cannot accidentally include serialization.
        if host_profile:
            owner._stop_concurrency_profile()
            profile_started = False
            native_profile_snapshot = (
                _normalized_concurrency_profile_snapshot(owner)
            )
            python_callback_snapshot = _freeze_python_callback_profile(
                callback_profile
            )

        end_cycles = [
            int(cpu.cycle_count) for cpu in workload.system.cores
        ]
        end_system_cycles = int(owner.system_cycles)
        end_native_batches = int(owner.native_batch_runs)
        end_native_dispatches = int(owner.native_dispatches)
        workers_after = _host_worker_snapshot(workload.system)
        observation = _state_observation(workload)
    finally:
        if profile_started:
            owner._stop_concurrency_profile()
        workload.close()

    per_core_cycles = [
        end - start for start, end in zip(start_cycles, end_cycles)
    ]
    per_core_instructions = [entry["instructions"] for entry in core_stats]
    aggregate_core_cycles = sum(per_core_cycles)
    aggregate_instructions = sum(per_core_instructions)
    aggregate_native_dispatches = sum(
        entry["native_dispatches"] for entry in core_stats
    )
    returned = execution.returned_aggregate_instructions
    native_batch_delta = end_native_batches - start_native_batches
    native_dispatch_delta = (
        end_native_dispatches - start_native_dispatches
    )
    native_stop_reason_count = sum(
        sum(entry["native_stop_reasons"].values())
        for entry in core_stats
    )
    native_continuation_stop_count = sum(
        sum(
            entry["native_stop_reasons"][reason]
            for reason in (
                "mex_fallback",
                "ext_fallback",
                "trap",
                "reset",
            )
        )
        for entry in core_stats
    )
    virtual_system_cycles = end_system_cycles - start_system_cycles
    public_accounting = {
        "execution": execution.as_dict(),
        "ordered_system_run_stats":
            scheduler_stats["ordered_public_batch_results"],
        "per_core": [
            {
                "core_id": int(stats["core_id"]),
                "instructions": int(stats["instructions"]),
                "scheduler_cycles": int(stats["scheduler_cycles"]),
                "native_dispatches": int(stats["native_dispatches"]),
                "native_stop_reasons": stats["native_stop_reasons"],
                "architectural_cycles": int(cycles),
            }
            for stats, cycles in zip(core_stats, per_core_cycles)
        ],
        "virtual_system_cycles": int(virtual_system_cycles),
    }
    worker_diagnostics = _host_worker_diagnostics(
        workers_before,
        workers_after,
    )
    accounting = {
        "instrumented": True,
        "used_for_throughput": False,
        "worker_count": worker_count,
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
        "scheduler_provenance": {
            **scheduler_stats,
            "reported_native_dispatches": aggregate_native_dispatches,
            "native_batch_runs_counter_delta": native_batch_delta,
            "native_dispatches_counter_delta": native_dispatch_delta,
            "all_batches_native": (
                scheduler_stats["native_system_batch_calls"]
                == scheduler_stats["system_batch_calls"]
                and scheduler_stats["compatibility_system_batch_calls"] == 0
            ),
            "native_batch_count_matches_runner_calls": (
                native_batch_delta == execution.run_batch_calls
                == scheduler_stats["system_batch_calls"]
            ),
            "native_dispatch_count_matches_results":
                aggregate_native_dispatches == native_dispatch_delta,
            "benchmark_stop_reason_count_matches_dispatches":
                native_stop_reason_count == aggregate_native_dispatches,
            "native_continuation_count_matches_stop_reasons":
                native_continuation_stop_count
                == scheduler_stats["native_continuations"],
            "device_tick_calls_match_native_rounds":
                bus_stats["tick_calls"] == scheduler_stats["native_rounds"],
            "device_tick_units_match_virtual_system_cycles":
                bus_stats["tick_argument_units"] == virtual_system_cycles,
        },
        "aggregate_core_architectural_cycles": aggregate_core_cycles,
        "max_core_architectural_cycles": max(per_core_cycles, default=0),
        "device_bus_tick_calls": bus_stats["tick_calls"],
        "device_bus_tick_argument_units": bus_stats["tick_argument_units"],
        "device_cycle_to_returned_instruction_ratio":
            (bus_stats["tick_argument_units"] / returned) if returned else None,
        "virtual_system_cycles": virtual_system_cycles,
        "virtual_system_cycles_availability":
            "available from the authoritative native system clock",
        "public_accounting_oracle": public_accounting,
        "public_accounting_oracle_sha256":
            _json_sha256(public_accounting),
        "host_worker_diagnostics": worker_diagnostics,
        "observation": observation,
    }
    profile_probe = None
    if host_profile:
        if native_profile_snapshot is None or python_callback_snapshot is None:
            raise RuntimeError(
                "host profiling completed without frozen profile snapshots"
            )
        profile_probe = _host_profile_probe(
            native_snapshot=native_profile_snapshot,
            python_callbacks=python_callback_snapshot,
            accounting=accounting,
        )
    return accounting, profile_probe


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
    accounting_scheduler = accounting["scheduler_provenance"]
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
        timed_scheduler = sample["scheduler_provenance"]
        scheduler_matches = (
            timed_scheduler["all_run_batch_calls_native"]
            and accounting_scheduler["all_batches_native"]
            and timed_scheduler["native_batch_runs_counter_delta"]
            == accounting_scheduler["native_batch_runs_counter_delta"]
            and timed_scheduler["native_dispatches_counter_delta"]
            == accounting_scheduler["native_dispatches_counter_delta"]
        )
        replay_comparisons.append({
            "repeat_index": repeat_index,
            "behavior_oracle_matches": oracle_matches,
            "execution_accounting_matches": execution_matches,
            "scheduler_provenance_matches": scheduler_matches,
            "matches": (
                oracle_matches
                and execution_matches
                and scheduler_matches
            ),
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


def _run_warmup(
    scenario: Scenario,
    num_cores: int,
    instructions: int,
    worker_count: int = 1,
) -> None:
    workload = scenario.build(num_cores, worker_count)
    try:
        workload.execute(instructions)
    finally:
        workload.close()


def _cross_worker_equivalence(results: list[dict]) -> list[dict]:
    grouped: dict[tuple[str, int], list[dict]] = {}
    for result in results:
        grouped.setdefault(
            (result["scenario"], result["full_cores"]),
            [],
        ).append(result)

    groups = []
    for (scenario, full_cores), entries in grouped.items():
        entries.sort(key=lambda entry: entry["worker_count"])
        baseline = next(
            (
                entry
                for entry in entries
                if entry["worker_count"] == 1
            ),
            None,
        )
        baseline_rate = (
            None
            if baseline is None
            else baseline["summary"][
                "median_aggregate_instructions_per_s"
            ]
        )
        observations = [
            entry["accounting_probe"]["observation"]
            for entry in entries
        ]
        accounting_hashes = [
            entry["accounting_probe"][
                "public_accounting_oracle_sha256"
            ]
            for entry in entries
        ]
        members = []
        for entry, observation, accounting_hash in zip(
            entries,
            observations,
            accounting_hashes,
            strict=True,
        ):
            rate = entry["summary"][
                "median_aggregate_instructions_per_s"
            ]
            ratio = (
                None
                if baseline_rate is None or baseline_rate == 0
                else rate / baseline_rate
            )
            entry["summary"]["one_lane_relative_throughput"] = ratio
            members.append({
                "worker_count": entry["worker_count"],
                "canonical_state_sha256":
                    observation["canonical_state_sha256"],
                "behavior_oracle_sha256":
                    observation["behavior_oracle_sha256"],
                "public_accounting_oracle_sha256": accounting_hash,
                "median_aggregate_instructions_per_s": rate,
                "one_lane_relative_throughput": ratio,
            })
        validation = {
            "one_lane_reference_present": baseline is not None,
            "canonical_state_equal":
                len({
                    observation["canonical_state_sha256"]
                    for observation in observations
                }) == 1,
            "behavior_oracle_equal":
                len({
                    observation["behavior_oracle_sha256"]
                    for observation in observations
                }) == 1,
            "ordered_public_accounting_cycles_dispatches_stops_equal":
                len(set(accounting_hashes)) == 1,
        }
        validation["equivalent"] = all(validation.values())
        groups.append({
            "scenario": scenario,
            "full_cores": full_cores,
            "reference_worker_count": (
                None if baseline is None else 1
            ),
            "members": members,
            "validation": validation,
        })
    return groups


def _strict_dma_cross_worker_equivalence(
    worker_reports: list[dict],
) -> dict:
    baseline = next(
        (
            report
            for report in worker_reports
            if report["configuration"]["worker_count"] == 1
        ),
        None,
    )
    baseline_rate = (
        None
        if baseline is None
        else baseline["summary"]["median_dma_payload_bytes_per_s"]
    )
    members = []
    for report in worker_reports:
        timed = report["timed_samples"]
        first = timed[0]
        rate = report["summary"]["median_dma_payload_bytes_per_s"]
        public_hashes = [
            _json_sha256(sample["ordered_public_batch_results"])
            for sample in timed
        ]
        members.append({
            "worker_count": report["configuration"]["worker_count"],
            "timed_behavior_oracle_sha256":
                first["observation"]["behavior_oracle_sha256"],
            "timed_public_batch_results_sha256": public_hashes[0],
            "sliced_behavior_oracle_sha256": report[
                "sliced_oracle_replay"
            ]["observation"]["behavior_oracle_sha256"],
            "sliced_public_batch_results_sha256": _json_sha256(
                report["sliced_oracle_replay"][
                    "ordered_public_batch_results"
                ]
            ),
            "median_dma_payload_bytes_per_s": rate,
            "one_lane_relative_throughput": (
                None
                if baseline_rate is None or baseline_rate == 0
                else rate / baseline_rate
            ),
            "timed_repeats_public_results_deterministic":
                len(set(public_hashes)) == 1,
        })
    validation = {
        "one_lane_reference_present": baseline is not None,
        "timed_behavior_oracle_equal": len({
            member["timed_behavior_oracle_sha256"]
            for member in members
        }) == 1,
        "timed_ordered_public_results_equal": len({
            member["timed_public_batch_results_sha256"]
            for member in members
        }) == 1,
        "sliced_behavior_oracle_equal": len({
            member["sliced_behavior_oracle_sha256"]
            for member in members
        }) == 1,
        "sliced_ordered_public_results_equal": len({
            member["sliced_public_batch_results_sha256"]
            for member in members
        }) == 1,
        "all_timed_repeats_public_results_deterministic": all(
            member["timed_repeats_public_results_deterministic"]
            for member in members
        ),
    }
    validation["equivalent"] = all(validation.values())
    return {
        "reference_worker_count": None if baseline is None else 1,
        "members": members,
        "validation": validation,
    }


def run_report(
    *,
    core_counts: Iterable[int],
    worker_counts: Iterable[int] = (1, 2, 4),
    scenario_names: Iterable[str],
    instructions: int,
    repeats: int,
    warmups: int,
    warmup_instructions: int,
    strict_dma_bytes: int = STRICT_DMA_DEFAULT_BYTES,
    host_profile: bool = False,
) -> dict:
    core_counts = list(core_counts)
    worker_counts = list(worker_counts)
    scenario_names = list(scenario_names)
    selected_scenarios = [SCENARIOS[name] for name in scenario_names]
    results = []
    for scenario in selected_scenarios:
        for num_cores in core_counts:
            for worker_count in worker_counts:
                for _ in range(warmups):
                    _run_warmup(
                        scenario,
                        num_cores,
                        warmup_instructions,
                        worker_count,
                    )
                samples = [
                    _timed_sample(
                        scenario,
                        num_cores,
                        instructions,
                        worker_count,
                    )
                    for _ in range(repeats)
                ]
                accounting, host_profile_probe = _accounting_probe(
                    scenario,
                    num_cores,
                    instructions,
                    worker_count,
                    host_profile=host_profile,
                )
                lane_participation_required = (
                    scenario.name == "private_compute"
                    # An exactly single-core system uses the native
                    # coordinator's uncontended loop and intentionally does
                    # not submit worker commands. Multi-core runs remain the
                    # worker-lane participation evidence.
                    and num_cores > 1
                    and num_cores >= worker_count
                )
                lane_participation_observed = all(
                    sample["host_worker_diagnostics"][
                        "every_configured_lane_participated"
                    ]
                    for sample in samples
                ) and accounting["host_worker_diagnostics"][
                    "every_configured_lane_participated"
                ]
                results.append(
                    {
                        "scenario": scenario.name,
                        "description": scenario.description,
                        "known_limitation": scenario.limitation,
                        "coverage_classification":
                            scenario.coverage_classification,
                        "coverage_claim": scenario.coverage_claim,
                        "full_cores": num_cores,
                        "worker_count": worker_count,
                        "host_execution_lanes": worker_count,
                        "auxiliary_worker_count": worker_count - 1,
                        "lane_participation": {
                            "required": lane_participation_required,
                            "observed": lane_participation_observed,
                            "requirement_satisfied": (
                                lane_participation_observed
                                if lane_participation_required
                                else True
                            ),
                        },
                        "timed_samples": samples,
                        "accounting_probe": accounting,
                        "host_profile_probe": host_profile_probe,
                        "summary": _summary(samples, accounting),
                    }
                )

    cross_worker_groups = _cross_worker_equivalence(results)
    strict_worker_reports = [
        _strict_nic_disk_dma_report(
            strict_dma_bytes,
            repeats=repeats,
            warmups=warmups,
            worker_count=worker_count,
        )
        for worker_count in worker_counts
    ]
    strict_dma_equivalence = _strict_dma_cross_worker_equivalence(
        strict_worker_reports
    )
    validation = {
        "host_profile_presence_matches_request": all(
            (result["host_profile_probe"] is not None) == host_profile
            for result in results
        ),
        "all_host_profile_probes_valid": all(
            result["host_profile_probe"] is None
            or all(result["host_profile_probe"]["validation"].values())
            for result in results
        ),
        "all_instruction_accounting_matches": all(
            result["accounting_probe"][
                "instruction_accounting_matches_runner"
            ]
            for result in results
        ),
        "all_timed_samples_used_native_scheduler": all(
            sample["scheduler_provenance"]["all_run_batch_calls_native"]
            for result in results
            for sample in result["timed_samples"]
        ),
        "all_accounting_batches_used_native_scheduler": all(
            result["accounting_probe"]["scheduler_provenance"][
                "all_batches_native"
            ]
            for result in results
        ),
        "all_native_batch_counts_match_runner_calls": all(
            result["accounting_probe"]["scheduler_provenance"][
                "native_batch_count_matches_runner_calls"
            ]
            for result in results
        ),
        "all_native_dispatch_counts_match_reported_results": all(
            result["accounting_probe"]["scheduler_provenance"][
                "native_dispatch_count_matches_results"
            ]
            for result in results
        ),
        "all_benchmark_stop_reason_counts_match_dispatches": all(
            result["accounting_probe"]["scheduler_provenance"][
                "benchmark_stop_reason_count_matches_dispatches"
            ]
            for result in results
        ),
        "all_native_continuation_counts_match_stop_reasons": all(
            result["accounting_probe"]["scheduler_provenance"][
                "native_continuation_count_matches_stop_reasons"
            ]
            for result in results
        ),
        "all_device_ticks_match_native_rounds_and_system_cycles": all(
            (
                result["accounting_probe"]["scheduler_provenance"][
                    "device_tick_calls_match_native_rounds"
                ]
                and result["accounting_probe"]["scheduler_provenance"][
                    "device_tick_units_match_virtual_system_cycles"
                ]
            )
            for result in results
        ),
        "all_external_event_journals_quiescent": all(
            not sample["observation"]["canonical_state"][
                "shared_devices"
            ]["external_events"]["pending"]
            and not sample["observation"]["canonical_state"][
                "shared_devices"
            ]["external_events"]["history"]
            and sample["observation"]["canonical_state"][
                "shared_devices"
            ]["external_events"]["next_sequence"] == 1
            for result in results
            for sample in (
                *result["timed_samples"],
                result["accounting_probe"],
            )
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
        "all_required_private_lanes_participated": all(
            result["lane_participation"]["requirement_satisfied"]
            for result in results
        ),
        "all_cross_worker_groups_equivalent": all(
            group["validation"]["equivalent"]
            for group in cross_worker_groups
        ),
        "strict_dma_all_timed_samples_valid": all(
            report["validation"]["all_timed_samples_valid"]
            for report in strict_worker_reports
        ),
        "strict_dma_sliced_replay_valid": all(
            report["validation"]["sliced_replay_valid"]
            for report in strict_worker_reports
        ),
        "strict_dma_timed_repeats_deterministic": all(
            report["validation"]["timed_repeats_deterministic"]
            for report in strict_worker_reports
        ),
        "strict_dma_one_shot_and_sliced_state_equal_except_batch_boundary_count":
            all(
                report["validation"][
                    "one_shot_and_sliced_state_equal_except_batch_boundary_count"
                ]
                for report in strict_worker_reports
            ),
        "strict_dma_default_weight_one_peer_trace_is_round_robin":
            all(
                report["validation"][
                    "default_weight_one_peer_trace_is_round_robin"
                ]
                for report in strict_worker_reports
            ),
        "strict_dma_cross_worker_equivalent":
            strict_dma_equivalence["validation"]["equivalent"],
    }
    return {
        "schema": SCHEMA,
        "schema_version": SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "repository": repository_metadata(),
        "host": host_metadata(),
        "coverage": COVERAGE_METADATA,
        "main_bus_arbitration_contract": ARBITRATION_CONTRACT,
        "state_comparison_scope": STATE_COMPARISON_SCOPE,
        "fixture_manifest": _fixture_manifest(strict_dma_bytes),
        "configuration": {
            "full_core_counts": list(core_counts),
            "worker_counts": list(worker_counts),
            "scenarios": [scenario.name for scenario in selected_scenarios],
            "aggregate_instruction_target_per_sample": instructions,
            "timed_repeats": repeats,
            "warmup_runs_per_case": warmups,
            "warmup_instructions_per_run": warmup_instructions,
            "host_profile": host_profile,
            "strict_dma_payload_bytes_per_endpoint":
                strict_dma_bytes,
            "execution_order": {
                "strategy": (
                    "deterministic scenario, full-core count, then configured "
                    "worker-count order; warmups precede timed repeats"
                ),
                "worker_count_order": list(worker_counts),
            },
        },
        "measurement_semantics": {
            "timed_throughput":
                "uninstrumented wall/process time around workload execution "
                "with cyclic garbage collection disabled and restored safely",
            "aggregate_instructions":
                "sum of values returned by MegapadSystem.run_batch()",
            "per_core_instructions":
                "exact per-core instructions from native SystemRunStats in a "
                "separate, untimed accounting replay",
            "per_core_scheduler_cycles":
                "exact scheduler-visible per-core cycles from native "
                "SystemRunStats in the untimed accounting replay",
            "native_dispatches":
                "raw native C++ run_steps segments reported per core by "
                "SystemRunStats",
            "native_stop_reasons":
                "seven native scheduler stop observations: run_limit, halt, "
                "idle, MEX fallback, EXT fallback, trap, and reset; a Python "
                "callback exception settled outside run_steps has no raw "
                "stop-reason bucket",
            "scheduler_provenance":
                "native batch/dispatch counter deltas outside the canonical "
                "behavior hash, checked across timed and accounting runs",
            "per_core_architectural_cycles":
                "delta of each CPU architectural cycle_count",
            "aggregate_core_architectural_cycles":
                "sum of per-core cycle_count deltas; not elapsed system time",
            "max_core_architectural_cycles":
                "diagnostic critical-path proxy only; not system time",
            "device_bus_tick_argument_units":
                "sum of virtual-cycle arguments passed to DeviceBus.tick()",
            "virtual_system_cycles":
                "delta of the authoritative native SystemState clock",
            "virtual_system_cycles_availability":
                "available for every accounting replay",
            "host_cpu_utilization_percent":
                "process CPU time divided by wall time; may exceed 100% when "
                "host worker threads overlap",
            "host_worker_diagnostics":
                "persistent pool identity and per-lane command/step deltas; "
                "host-only and excluded from every architectural hash",
            "host_profile_probe":
                "optional fixed-size native and Python callback attribution "
                "from the separate untimed accounting replay; explicitly "
                "stopped before canonical observation, excluded from every "
                "architectural hash, and never used for throughput",
            "host_profile_timing_scope":
                "timers are nested host wall observations; lane-active sums "
                "may exceed worker-wave or batch wall time and derived ratios "
                "are structural diagnostics rather than an additive causal "
                "partition",
            "python_callback_profile":
                "per-core MMIO and DeviceBus.tick wall time measured after "
                "entry into their Python accounting wrappers; native "
                "coordinator timers retain the surrounding GIL transition",
            "derived_per_core_throughput":
                "accounting-replay instruction share multiplied by the "
                "uninstrumented median aggregate instruction rate; omitted "
                "unless the accounting replay equals every timed repeat",
            "canonical_state_oracle":
                "canonical JSON over the captured scope documented in "
                "state_comparison_scope; workload counters are added to form "
                "the behavior oracle",
            "timestamped_external_event_oracle":
                "pending and historical external inputs are captured in "
                "cycle/sequence/release order with payload size and SHA-256, "
                "including replay seal and batch-boundary diagnostics; Phase "
                "0 validates that runs remain quiescent",
            "deterministic_platform_initialization":
                "the harness pins the virtual RTC to "
                "2000-01-01T00:00:00Z and UART geometry to 80x24",
            "strict_nic_disk_dma":
                "separate cycle-bounded byte throughput and virtual-cycle "
                "measurement; it is not folded into instruction MIPS",
            "dma_qos_and_ordering":
                "hard QoS controls must/may eligibility and entitlement; "
                "simultaneously eligible physical peers use equal "
                "round-robin, with unused reservation work-conserving and "
                "no weights or secondary bias",
        },
        "validation": validation,
        "results": results,
        "cross_worker_equivalence_groups": cross_worker_groups,
        "strict_nic_disk_dma": {
            "worker_reports": strict_worker_reports,
            "cross_worker_equivalence": strict_dma_equivalence,
        },
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
        f"{'scenario':<41} {'cores':>5} {'lanes':>5} {'agg MIPS':>10} "
        f"{'vs 1':>7} {'host CPU':>9} {'callbacks':>10} "
        f"{'deterministic':>14}"
    )
    print("-" * 112)
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
        ratio = summary["one_lane_relative_throughput"]
        ratio_text = "-" if ratio is None else f"{ratio:.2f}x"
        print(
            f"{result['scenario']:<41} {result['full_cores']:>5} "
            f"{result['worker_count']:>5} "
            f"{summary['median_aggregate_mips']:>10.2f} "
            f"{ratio_text:>7} "
            f"{summary['median_host_cpu_utilization_percent']:>8.1f}% "
            f"{callbacks:>10,} {str(deterministic):>14}"
        )
    print()
    for dma in report["strict_nic_disk_dma"]["worker_reports"]:
        dma_summary = dma["summary"]
        dma_valid = all(dma["validation"].values())
        print(
            "Strict NIC+disk DMA "
            f"({dma['configuration']['worker_count']} lanes): "
            f"{dma_summary['median_dma_payload_mib_per_s']:.2f} MiB/s, "
            f"{dma_summary['virtual_cycles_per_dma_payload_byte']:.3f} "
            "virtual cycles/payload byte, "
            f"deterministic={dma_valid}"
        )
    print()
    print(
        "Virtual system cycles: reported from the authoritative native clock "
        "in each accounting replay."
    )
    print(
        "Per-core instruction rates are derived from an exact untimed "
        "accounting replay only when it matches every timed repeat; timed "
        "aggregate rates are uninstrumented."
    )
    print(
        "Overall architectural validation: "
        + (
            "PASS"
            if all(report["validation"].values())
            else "FAIL"
        )
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Run a reproducible, bounded Phase 0 baseline for MegaPad's "
            "deterministic multicore system runner."
        )
    )
    parser.add_argument(
        "--cores",
        type=parse_core_counts,
        default=parse_core_counts("1,2,4"),
        help="comma-separated subset of 1,2,4 (default: 1,2,4)",
    )
    parser.add_argument(
        "--worker-counts",
        type=parse_worker_counts,
        default=parse_worker_counts("1,2,4"),
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
        "--strict-dma-bytes",
        type=parse_strict_dma_bytes,
        default=STRICT_DMA_DEFAULT_BYTES,
        help=(
            "bytes transferred by each strict NIC/disk endpoint per sample; "
            "512 or 1024 (default: 1024)"
        ),
    )
    parser.add_argument(
        "--host-profile",
        action="store_true",
        default=False,
        help=(
            "enable host-only attribution during the separate untimed "
            "accounting replay (default: disabled)"
        ),
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
        args.strict_dma_bytes = STRICT_DMA_QUICK_BYTES

    report = run_report(
        core_counts=args.cores,
        worker_counts=args.worker_counts,
        scenario_names=args.scenarios,
        instructions=args.instructions,
        repeats=args.repeats,
        warmups=args.warmups,
        warmup_instructions=args.warmup_instructions,
        strict_dma_bytes=args.strict_dma_bytes,
        host_profile=args.host_profile,
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
    return 0 if all(report["validation"].values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
