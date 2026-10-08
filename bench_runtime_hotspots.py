#!/usr/bin/env python3
"""Bounded, sequential hotspot measurements with independently checked results.

Each executor/workload pair runs in a fresh subprocess with a wall watchdog.
Each trial has a fresh prepared instance; discarded warmups warm process code,
not a retained guest/JIT cache. Setup and validation are outside the measured
action. Optional attribution repeats the action on another fresh instance and
is never included in the unprofiled timing samples.

The default cases cover scalar FP, hosted FP64 tiles, SHA3, PCM capture, and
synthetic source compilation. Opt-in cases cover AES-GCM, page access,
continuation resumption, and NTT computation/transfers. They measure the current
implementation, including any remaining Python service work. This does not
measure Desktop, full KDOS boot, audio playback,
or architectural/semantic timing equivalence. Use bench_simulator_kdos_load.py
and bench_bios_kdos_load.py for their separately qualified full-source cases.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import cProfile
from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import importlib
import json
import os
from pathlib import Path
import platform
import pstats
import resource
import statistics
import struct
import subprocess
import sys
import time
from typing import Callable


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.runtime-hotspots"
DEFAULT_WORKLOADS = ("fp64", "tile-fp64", "sha3", "audio", "source-load")
EXTENDED_WORKLOAD_LIMITS = {
    "aes-gcm32": 1024,
    "page-hot": 65_536,
    "page-scattered": 65_536,
    "page-crossing": 65_536,
    "continuation-short": 4096,
    "continuation-long": 4096,
    "ntt-compute": 256,
    "ntt-transfer": 256,
}
WORKLOADS = DEFAULT_WORKLOADS + tuple(EXTENDED_WORKLOAD_LIMITS)
EXECUTORS = ("emulator-native", "simulator-python", "simulator-native")
MASK64 = (1 << 64) - 1
# Independent binary64 known answers, round-to-nearest-even.
FP_RESULTS = (0x4010000000000000, 0x4024000000000000,
              0x3FD5555555555555, 0x3FF6A09E667F3BCD)
FP_XOR = FP_RESULTS[0] ^ FP_RESULTS[1] ^ FP_RESULTS[2] ^ FP_RESULTS[3]
# Below the default Bank-0 data-stack boundary (0x80000), above the
# protected dictionary prefix, and outside these small compiled kernels.
SOURCE_ADDRESS, SOURCE1_ADDRESS, DESTINATION_ADDRESS = 0x20000, 0x20100, 0x20200
AUDIO_BYTES = 4096
# Extended cases own disjoint buffers, outside the original tile/SHA3/audio
# locations and below the default Bank-0 stack boundary.
CRYPTO_SOURCE, CRYPTO_DESTINATION = 0x30000, 0x31000
AES_KEY_ADDRESS, AES_IV_ADDRESS, AES_TAG_ADDRESS = 0x32000, 0x32100, 0x32200
PAGE_BUFFER_BASE = 0x40000
PAGE_SIZE = 4096
PAGE_VALUES = tuple(17 * (index + 1) for index in range(16))
AES_PLAINTEXT = b"A" * 16 + b"B" * 16
# Checked-in independent known answer from tests/simulator/test_kdos_aes.py.
# AES-256, key bytes(range(32)), IV bytes(range(12)), no AAD.
AES_CIPHERTEXT = bytes.fromhex(
    "0643975a84a4835acc00d6caf0a8392c"
    "c194c576b2391d3e7a25a7c75f2b42f0"
)
AES_TAG = bytes.fromhex("61f3ad860a90ca7ede2074f793b887c1")
NTT_MODULUS, NTT_ROOT, NTT_LENGTH = 3329, 3061, 256


class BenchmarkError(RuntimeError):
    pass


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise BenchmarkError(message)


def _bounded_int(lower: int, upper: int):
    def parse(value: str) -> int:
        try:
            result = int(value)
        except ValueError as exc:
            raise argparse.ArgumentTypeError("must be an integer") from exc
        if not lower <= result <= upper:
            raise argparse.ArgumentTypeError(f"must be between {lower} and {upper}")
        return result
    return parse


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workload", nargs="+", choices=WORKLOADS,
                        default=list(DEFAULT_WORKLOADS))
    parser.add_argument("--executor", nargs="+", choices=EXECUTORS, default=list(EXECUTORS))
    parser.add_argument("--iterations", type=_bounded_int(1, 100_000), default=64)
    parser.add_argument("--trials", type=_bounded_int(1, 10), default=3)
    parser.add_argument("--warmup", type=_bounded_int(0, 3), default=1)
    parser.add_argument("--timeout", type=_bounded_int(1, 120), default=30,
                        help="wall seconds per executor/workload subprocess (default: 30)")
    parser.add_argument("--attribution", action="store_true",
                        help="separate cProfile and native attribution run")
    parser.add_argument("--output", type=Path, help="also write the complete JSON report here")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def supported(workload: str, executor: str) -> bool:
    return executor != "emulator-native" or workload == "fp64"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _repository() -> dict:
    def git(*arguments):
        result = subprocess.run(
            ["git", "--no-optional-locks", *arguments], cwd=ROOT,
            capture_output=True, text=True, check=False,
        )
        return result.stdout.strip() if result.returncode == 0 else None
    status = git("status", "--porcelain=v1", "--untracked-files=all")
    return {"root": str(ROOT), "commit": git("rev-parse", "HEAD"),
            "dirty": None if status is None else bool(status), "status": status}


def _source_manifest() -> dict[str, str]:
    paths = {Path(__file__).resolve()}
    for directory in ("emulator", "simulator", "shared", "rich_terminal"):
        paths.update(path for path in (ROOT / directory).rglob("*")
                     if path.is_file() and path.suffix in (".py", ".cpp", ".h"))
    paths.update(path for path in ROOT.iterdir() if path.is_file()
                 and path.suffix in (".py", ".asm", ".f")
                 and not path.name.startswith("bench_"))
    return {str(path.relative_to(ROOT)): _sha256(path) for path in sorted(paths)}


def _native_artifact(executor: str) -> dict | None:
    if executor == "simulator-python":
        return None
    name = "_mp64_accel" if executor == "emulator-native" else "_megaforth_native"
    module = importlib.import_module(name)
    path = Path(module.__file__).resolve()
    _require(path.parent == ROOT, f"{name} was loaded outside this checkout: {path}")
    return {"module": name, "path": str(path), "bytes": path.stat().st_size,
            "sha256": _sha256(path)}


def _peak_rss_bytes() -> int:
    value = int(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
    return value if sys.platform == "darwin" else value * 1024


def _delta(before: dict, after: dict) -> dict:
    result = {}
    for name, value in after.items():
        if isinstance(value, dict):
            result[name] = _delta(before.get(name, {}), value)
        elif isinstance(value, (int, float)) and not isinstance(value, bool):
            result[name] = value - before.get(name, 0)
    return result


@contextmanager
def _native_profiling(enabled: bool):
    old = os.environ.get("MEGAFORTH_NATIVE_PROFILE")
    os.environ["MEGAFORTH_NATIVE_PROFILE"] = "1" if enabled else "0"
    try:
        yield
    finally:
        if old is None:
            os.environ.pop("MEGAFORTH_NATIVE_PROFILE", None)
        else:
            os.environ["MEGAFORTH_NATIVE_PROFILE"] = old


@dataclass
class PreparedWorkload:
    action: Callable
    observe: Callable
    close: Callable
    counters: Callable
    description: dict
    start_attribution: Callable | None = None
    stop_attribution: Callable | None = None


def _fp_source(iterations: int) -> bytes:
    return (
        f": HOTSPOT 0 {iterations} 0 DO "
        "0x3FF8000000000000 0x4004000000000000 F64+ "
        "0x4000000000000000 0x4008000000000000 0x4010000000000000 F64FMA XOR "
        "0x3FF0000000000000 0x4008000000000000 F64/ XOR "
        "0x4000000000000000 F64SQRT XOR + LOOP ;"
    ).encode("ascii")


def prepare_emulator_fp(iterations: int) -> PreparedWorkload:
    from asm import assemble
    from emulator.megapad64 import CSR_FPCSR
    from emulator.session import MachineSession
    from emulator.system import MegapadSystem

    source = """
loop:
    mov r1, r12
    fadd.d r1, r13
    mov r4, r14
    fma.d r4, r8, r9
    mov r5, r7
    fdiv.d r5, r9
    fsqrt.d r6, r8
    xor r1, r4
    xor r1, r5
    xor r1, r6
    add r11, r1
    dec r10
    cmpi r10, 0
    lbrne loop
    halt
"""
    code = bytes(assemble(source))
    system = MegapadSystem(ram_size=64 << 10, ext_mem_size=0, vram_size=0,
                           num_cores=1, num_clusters=0, worker_count=1)
    session = MachineSession(system)
    try:
        system.load_binary(0, code)
        session.boot()
        cpu = system.cpu
        for register, value in {
            7: 0x3FF0000000000000, 8: 0x4000000000000000,
            9: 0x4008000000000000, 10: iterations, 11: 0,
            12: 0x3FF8000000000000, 13: 0x4004000000000000,
            14: 0x4010000000000000,
        }.items():
            cpu.regs[register] = value
        cpu.csr_write(CSR_FPCSR, 0)
    except BaseException:
        session.close()
        raise

    def observe(result):
        expected = (FP_XOR * iterations) & MASK64
        _require(system.all_halted, "FP kernel did not halt inside its instruction budget")
        _require(cpu.regs[10] == 0 and cpu.regs[11] == expected,
                 "architectural FP iteration/checksum mismatch")
        _require(tuple(cpu.regs[index] for index in (1, 4, 5, 6)) ==
                 (FP_XOR, *FP_RESULTS[1:]), "architectural FP result bits differ")
        _require(cpu.csr_read(CSR_FPCSR) == 16, "architectural FPCSR differs")
        return {"checksum": expected, "fpcsr": 16, "fp_operations": 4 * iterations,
                "instructions": result.instructions_executed,
                "system_cycles": result.system_cycles_advanced,
                "stop_reason": result.system_stop_reason}

    return PreparedWorkload(
        action=lambda: system.run_batch_stats(iterations * 20 + 32),
        observe=observe, close=session.close, counters=lambda: {},
        description={"scope": "one-core native scheduler; assembled counted FP loop",
                     "program_sha256": hashlib.sha256(code).hexdigest(),
                     "operations_per_iteration": 4, "work_unit": "scalar_fp64_operation"},
        start_attribution=system.start_host_profile,
        stop_attribution=system.stop_host_profile,
    )


def _extended_fixture(runtime, source: bytes, iterations: int, expected_stack: tuple,
                      description: dict, check: Callable) -> PreparedWorkload:
    """Compile an opt-in fixture without changing the original case paths."""
    from simulator.runtime import ExecutionResult

    budget = iterations * 200 + 10_000
    runtime.evaluate(source, source_name="hotspot-extended.f", step_budget=budget)
    _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty stack")
    description["source_sha256"] = hashlib.sha256(source).hexdigest()

    def observe(result):
        _require(isinstance(result, ExecutionResult), "semantic kernel did not complete")
        _require(runtime.main_context.data.snapshot() == expected_stack,
                 "semantic result stack differs")
        returns = runtime.main_context.returns
        _require(returns.snapshot() == () and returns.pointer == returns.empty_pointer,
                 "semantic return stack differs")
        observation = {"semantic_steps": result.semantic_steps, "stack": list(expected_stack)}
        observation.update(check())
        return observation

    return PreparedWorkload(
        action=lambda: runtime.execute("HOTSPOT", step_budget=budget),
        observe=observe, close=runtime.memory.mmio.audio.release_host_sink,
        counters=lambda: runtime.native_execution_stats, description=description,
    )


def _prepare_aes(runtime, iterations: int) -> PreparedWorkload:
    key, iv = bytes(range(32)), bytes(range(12))
    inputs = ((AES_KEY_ADDRESS, key), (AES_IV_ADDRESS, iv),
              (CRYPTO_SOURCE, AES_PLAINTEXT))
    for address, payload in inputs:
        runtime.memory.write_bytes(address, payload)
    # Explicit AES-256 mode selection is setup, as are all input buffers.
    runtime.evaluate(b"0 AES-KEY-MODE!")
    source = (
        f": HOTSPOT 0 {iterations} 0 DO "
        f"{AES_KEY_ADDRESS} AES-KEY! {AES_IV_ADDRESS} AES-IV! "
        "0 AES-AAD-LEN! 32 AES-DATA-LEN! 0 AES-CMD! "
        f"{CRYPTO_SOURCE} AES-DIN! {CRYPTO_DESTINATION} AES-DOUT@ "
        f"{CRYPTO_SOURCE + 16} AES-DIN! {CRYPTO_DESTINATION + 16} AES-DOUT@ "
        f"{AES_TAG_ADDRESS} AES-TAG@ AES-STATUS@ 2 - OR LOOP ;"
    ).encode("ascii")

    def check():
        ciphertext = runtime.memory.read_bytes(CRYPTO_DESTINATION, 32)
        tag = runtime.memory.read_bytes(AES_TAG_ADDRESS, 16)
        _require(ciphertext == AES_CIPHERTEXT, "AES ciphertext differs")
        _require(tag == AES_TAG, "AES authentication tag differs")
        _require(runtime.aes.status == 2 and runtime.aes.key_mode == 0,
                 "AES status or key mode differs")
        _require(all(runtime.memory.read_bytes(address, len(payload)) == payload
                     for address, payload in inputs), "AES source bytes changed")
        return {"transactions": iterations, "aes_blocks": iterations * 2,
                "status": 2, "ciphertext_hex": ciphertext.hex(), "tag_hex": tag.hex()}

    return _extended_fixture(
        runtime, source, iterations, (0,),
        {"scope": "hosted AES-256-GCM BIOS transfers and service computation",
         "work_unit": "aes_gcm32_transaction", "plaintext_bytes_per_iteration": 32,
         "aad_bytes_per_iteration": 0, "aes_blocks_per_iteration": 2,
         "oracle": "fixed ciphertext and full tag from checked-in AES fixture"}, check,
    )


def _page_geometry(workload: str) -> tuple[int, int, tuple[int, ...]]:
    stride = 8 if workload == "page-hot" else PAGE_SIZE
    base = PAGE_BUFFER_BASE + (PAGE_SIZE - 4 if workload == "page-crossing" else 0)
    return base, stride, tuple(base + index * stride for index in range(16))


def _prepare_pages(runtime, workload: str, iterations: int) -> PreparedWorkload:
    _require(runtime.memory.page_size == PAGE_SIZE, "page fixture requires 4096-byte pages")
    base, stride, addresses = _page_geometry(workload)
    # Materialize every configured page before timing, including cell crossings.
    # Guard bytes and inter-cell padding must remain unchanged after reads.
    span_start, span_end = addresses[0] - 16, addresses[-1] + 8 + 16
    image = bytearray(b"\xa5" * (span_end - span_start))
    for address, value in zip(addresses, PAGE_VALUES):
        offset = address - span_start
        image[offset:offset + 8] = value.to_bytes(8, "little")
    expected_image = bytes(image)
    runtime.memory.write_bytes(span_start, expected_image)
    source = (
        f": HOTSPOT 0 {iterations} 0 DO I 15 AND {stride} * {base} + @ + LOOP ;"
    ).encode("ascii")
    full, tail = divmod(iterations, len(PAGE_VALUES))
    checksum = (full * sum(PAGE_VALUES) + sum(PAGE_VALUES[:tail])) & MASK64
    pages = lambda cells: sorted({page for address in cells
                                 for page in (address // PAGE_SIZE, (address + 7) // PAGE_SIZE)})
    configured_pages = pages(addresses)
    touched_pages = pages(addresses[:min(iterations, 16)])

    def check():
        actual = runtime.memory.read_bytes(span_start, len(expected_image))
        _require(actual == expected_image, "page source or guard bytes changed")
        return {"checksum": checksum, "cell_reads": iterations,
                "source_sha256": hashlib.sha256(actual).hexdigest()}

    return _extended_fixture(
        runtime, source, iterations, (checksum,),
        {"scope": "identical guest cell-read loop over prepared ordinary pages",
         "work_unit": "cell_read", "page_size": PAGE_SIZE, "stride_bytes": stride,
         "cell_width": 8, "configured_cells": 16, "base_address": base,
         "configured_data_pages": len(configured_pages),
         "touched_data_pages": len(touched_pages),
         "crosses_page_per_read": workload == "page-crossing",
         "buffer_start": span_start, "buffer_bytes": len(expected_image)}, check,
    )


def _ntt_direct_expected(coefficients: tuple[int, ...]) -> tuple[int, ...]:
    """Independent quadratic DFT; no shared transform or device helper calls."""
    _require(len(coefficients) == NTT_LENGTH, "NTT oracle needs 256 coefficients")
    result = []
    for frequency in range(NTT_LENGTH):
        ratio = pow(NTT_ROOT, frequency, NTT_MODULUS)
        factor, total = 1, 0
        for coefficient in coefficients:
            total = (total + coefficient * factor) % NTT_MODULUS
            factor = factor * ratio % NTT_MODULUS
        result.append(total)
    return tuple(result)


def _prepare_ntt(runtime, workload: str, iterations: int) -> PreparedWorkload:
    coefficients = tuple((17 * index + 3) % NTT_MODULUS for index in range(NTT_LENGTH))
    payload = struct.pack("<256I", *coefficients)
    expected = _ntt_direct_expected(coefficients)
    expected_bytes = struct.pack("<256I", *expected)
    runtime.memory.write_bytes(CRYPTO_SOURCE, payload)
    destination_initial = b"\xa5" * (len(payload) + 32)
    runtime.memory.write_bytes(CRYPTO_DESTINATION - 16, destination_initial)
    runtime.ntt.set_modulus(NTT_MODULUS)
    runtime.ntt.load(CRYPTO_SOURCE, 0, runtime.memory)
    transfer = workload == "ntt-transfer"
    body = (f"{CRYPTO_SOURCE} 0 NTT-LOAD NTT-FWD {CRYPTO_DESTINATION} NTT-STORE"
            if transfer else "NTT-FWD")
    source = f": HOTSPOT {iterations} 0 DO {body} LOOP ;".encode("ascii")
    destination_expected = (b"\xa5" * 16 + expected_bytes + b"\xa5" * 16
                            if transfer else destination_initial)

    def check():
        _require(runtime.ntt.result() == expected, "NTT result differs from direct DFT")
        _require(runtime.ntt.polynomial_a() == coefficients and
                 runtime.ntt.polynomial_b() == (0,) * NTT_LENGTH,
                 "NTT input polynomial changed")
        _require(runtime.ntt.status == 2 and runtime.ntt.index == 0 and
                 runtime.ntt.modulus == NTT_MODULUS, "NTT retained registers differ")
        roots = runtime.ntt.roots
        _require(roots is not None and
                 (roots.forward, roots.inverse, roots.size_inverse) == (3061, 2298, 3316),
                 "NTT selected roots differ")
        _require(runtime.memory.read_bytes(CRYPTO_SOURCE, len(payload)) == payload,
                 "NTT source bytes changed")
        _require(runtime.memory.read_bytes(CRYPTO_DESTINATION - 16, len(destination_expected)) ==
                 destination_expected, "NTT output or guard bytes differ")
        return {"transforms": iterations, "coefficients": NTT_LENGTH, "status": 2,
                "index": 0, "output_sha256": hashlib.sha256(expected_bytes).hexdigest(),
                "guest_transfer_bytes": iterations * len(payload) * 2 if transfer else 0}

    return _extended_fixture(
        runtime, source, iterations, (),
        {"scope": "hosted NTT command with BIOS transfers" if transfer else
                  "hosted NTT command; polynomial loaded during untimed setup",
         "work_unit": "ntt_forward_transform", "modulus": NTT_MODULUS,
         "forward_root": NTT_ROOT, "coefficients_per_transform": NTT_LENGTH,
         "guest_transfer_bytes_per_iteration": len(payload) * 2 if transfer else 0,
         "oracle": "independent direct 256-point modular DFT computed outside timing"}, check,
    )


def _run_continuations(runtime, quantum: int, budget: int):
    from simulator.runtime import ExecutionResult, YieldedExecution

    result = runtime.run_until_blocked("HOTSPOT", quantum_steps=quantum, step_budget=budget)
    resumes, prior_steps = 0, -1
    # The cumulative semantic budget and this host bound are independent stops.
    while isinstance(result, YieldedExecution):
        _require(prior_steps < result.semantic_steps <= budget,
                 "continuation made no progress or exceeded its step budget")
        _require(resumes < budget // quantum + 1, "continuation resume bound exceeded")
        prior_steps = result.semantic_steps
        result = runtime.resume_yielded(result.suspension)
        resumes += 1
    _require(isinstance(result, ExecutionResult), "continuation did not complete; unexpected block")
    _require(prior_steps < result.semantic_steps <= budget, "continuation completion steps differ")
    return result, resumes


def _continuation_state(runtime) -> dict:
    context = runtime.main_context
    returns = context.returns
    slots = []
    for address, (frame, raw) in sorted(returns._continuations.items()):
        _require(frame.fault_abort is None, "unexpected fault continuation in benchmark")
        slots.append((address, frame.xt, frame.ip, frame.root, frame.dispatch_id, raw))
    return {"stack": context.data.snapshot(), "return_stack": returns.snapshot(),
            "return_pointer": returns.pointer, "empty_return_pointer": returns.empty_pointer,
            "continuation_cookie": returns._continuation_cookie,
            "continuation_slots": tuple(slots),
            "retained_return_bytes": runtime.memory.read_bytes(returns.empty_pointer - 256, 256)}


def _prepare_continuations(runtime, workload: str, iterations: int) -> PreparedWorkload:
    from simulator.runtime import MegaForthRuntime

    quantum = 7 if workload == "continuation-short" else 8192
    budget = iterations * 32 + 64
    source = (
        ": HSLEAF 1+ ; : HSL1 HSLEAF ; : HSL2 HSL1 ; : HSL3 HSL2 ; "
        f": HOTSPOT 0 {iterations} 0 DO HSL3 LOOP ;"
    ).encode("ascii")
    runtime.evaluate(source, source_name="hotspot-continuation.f", step_budget=10_000)
    reference = MegaForthRuntime(execution_backend="python")
    try:
        reference.evaluate(source, source_name="hotspot-continuation.f", step_budget=10_000)
        reference_result, reference_resumes = _run_continuations(reference, quantum, budget)
        expected_state = _continuation_state(reference)
    finally:
        reference.memory.mmio.audio.release_host_sink()
    _require(expected_state["stack"] == (iterations,), "Python continuation work differs")
    _require(expected_state["return_stack"] == () and
             expected_state["return_pointer"] == expected_state["empty_return_pointer"],
             "Python continuation return stack differs")
    _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty stack")

    def observe(outcome):
        result, resumes = outcome
        actual = _continuation_state(runtime)
        _require(actual == expected_state, "continuation retained state differs from Python reference")
        _require(result.semantic_steps == reference_result.semantic_steps and
                 resumes == reference_resumes, "continuation work or yield count differs")
        return {"semantic_steps": result.semantic_steps, "stack": list(actual["stack"]),
                "increments": iterations, "host_resumes": resumes,
                "return_pointer": actual["return_pointer"],
                "continuation_cookie": actual["continuation_cookie"],
                "continuation_slots": [list(slot) for slot in actual["continuation_slots"]],
                "retained_return_sha256": hashlib.sha256(actual["retained_return_bytes"]).hexdigest()}

    return PreparedWorkload(
        action=lambda: _run_continuations(runtime, quantum, budget), observe=observe,
        close=runtime.memory.mmio.audio.release_host_sink,
        counters=lambda: runtime.native_execution_stats,
        description={"scope": "nested colon calls and bounded host-quantum resumption",
                     "work_unit": "nested_increment", "calls_per_iteration": 4,
                     "quantum_steps": quantum, "semantic_step_budget": budget,
                     "max_host_resumes": budget // quantum + 1,
                     "source_sha256": hashlib.sha256(source).hexdigest(),
                     "oracle": "fresh Python execution; exact steps, RP, cookie, slots and retained bytes"},
    )


def _prepare_extended_semantic(workload: str, executor: str, iterations: int) -> PreparedWorkload:
    from simulator.runtime import MegaForthRuntime

    limit = EXTENDED_WORKLOAD_LIMITS[workload]
    if type(iterations) is not int or not 1 <= iterations <= limit:
        raise ValueError(f"{workload} iterations must be between 1 and {limit}")
    backend = executor.removeprefix("simulator-")
    runtime = MegaForthRuntime(execution_backend=backend)
    try:
        _require(runtime.execution_backend == backend, "requested semantic executor was not selected")
        if workload == "aes-gcm32":
            return _prepare_aes(runtime, iterations)
        if workload.startswith("page-"):
            return _prepare_pages(runtime, workload, iterations)
        if workload.startswith("ntt-"):
            return _prepare_ntt(runtime, workload, iterations)
        return _prepare_continuations(runtime, workload, iterations)
    except BaseException:
        runtime.memory.mmio.audio.release_host_sink()
        raise


def prepare_semantic(workload: str, executor: str, iterations: int) -> PreparedWorkload:
    from simulator.memory import MMIO_BASE
    from simulator.runtime import MegaForthRuntime

    if workload in EXTENDED_WORKLOAD_LIMITS:
        return _prepare_extended_semantic(workload, executor, iterations)

    backend = executor.removeprefix("simulator-")
    runtime = MegaForthRuntime(execution_backend=backend)
    _require(runtime.execution_backend == backend, "requested semantic executor was not selected")
    before_words = len(runtime.dictionary.words)
    expected_stack: tuple[int, ...] = ()
    expected_memory: bytes | None = None
    description = {"scope": "hosted guest word dispatch", "work_unit": workload}
    if workload == "fp64":
        source = _fp_source(iterations)
        expected_stack = ((FP_XOR * iterations) & MASK64,)
        description["operations_per_iteration"] = 4
    elif workload == "tile-fp64":
        runtime.memory.write_bytes(SOURCE_ADDRESS, struct.pack("<8d", *range(1, 9)))
        runtime.memory.write_bytes(SOURCE1_ADDRESS, struct.pack("<8d", *range(2, 10)))
        runtime.tile.set_mode(7)
        runtime.tile.set_source0(SOURCE_ADDRESS)
        runtime.tile.set_source1(SOURCE1_ADDRESS)
        runtime.tile.set_destination(DESTINATION_ADDRESS)
        source = f": HOTSPOT {iterations} 0 DO TADD LOOP ;".encode("ascii")
        expected_memory = struct.pack("<8d", *range(3, 18, 2))
        description["lanes_per_iteration"] = 8
    elif workload == "sha3":
        message = bytes(range(256))
        runtime.memory.write_bytes(SOURCE_ADDRESS, message)
        source = (
            f": HOTSPOT 0 {iterations} 0 DO 0 SHA3-BEGIN + "
            f"{SOURCE_ADDRESS} 256 SHA3-UPDATE + "
            f"{DESTINATION_ADDRESS} SHA3-FINAL + SHA3-CLEAR + LOOP ;"
        ).encode("ascii")
        expected_stack = (0,)
        expected_memory = hashlib.sha3_256(message).digest()
        description["message_bytes_per_iteration"] = len(message)
    elif workload == "audio":
        pcm = bytes(range(256)) * (AUDIO_BYTES // 256)
        runtime.memory.write_bytes(SOURCE_ADDRESS, pcm)
        base = MMIO_BASE + 0xC00
        runtime.memory.write64(base + 8, SOURCE_ADDRESS)
        runtime.memory.write32(base + 16, len(pcm) // 2)
        source = f": HOTSPOT {iterations} 0 DO 1 {base} C! LOOP ;".encode("ascii")
        expected_memory = pcm
        description.update(scope="hosted guest MMIO; synchronous headless PCM capture",
                           pcm_bytes_per_iteration=len(pcm))
    elif workload == "source-load":
        source = b"\n".join(
            f": HS{index} {index} ;".encode("ascii") for index in range(iterations)
        ) + f"\n: HOTSPOT HS0 HS{iterations - 1} + ;\n".encode("ascii")
        description.update(scope="synthetic colon-source compilation; verification execution excluded",
                           definitions=iterations + 1, source_bytes=len(source))
    else:
        raise ValueError(f"unsupported semantic workload: {workload}")

    description["source_sha256"] = hashlib.sha256(source).hexdigest()
    budget = iterations * 200 + 10_000
    if workload == "source-load":
        action = lambda: runtime.evaluate(source, source_name="hotspot-source.f", step_budget=budget)
    else:
        runtime.evaluate(source, source_name="hotspot-kernel.f", step_budget=budget)
        _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty stack")
        action = lambda: runtime.execute("HOTSPOT", step_budget=budget)

    def observe(result):
        observation = {"semantic_steps": result.semantic_steps}
        if workload == "source-load":
            _require(len(runtime.dictionary.words) - before_words == iterations + 1,
                     "source-load definition count differs")
            runtime.execute("HOTSPOT", step_budget=100)
            stack = (iterations - 1,)
            observation["definitions"] = iterations + 1
            observation["source_bytes"] = len(source)
        else:
            stack = expected_stack
        actual_stack = runtime.main_context.data.snapshot()
        _require(actual_stack == stack,
                 f"semantic result stack differs: actual={actual_stack!r}, expected={stack!r}")
        actual_returns = runtime.main_context.returns.snapshot()
        _require(actual_returns == (),
                 f"semantic return stack differs: actual={actual_returns!r}, expected=()")
        observation["stack"] = list(stack)
        if workload == "fp64":
            _require(runtime.scalar_float.fpcsr == 16, "semantic FPCSR differs")
            observation.update(checksum=stack[0], fpcsr=16, fp_operations=iterations * 4)
        if workload == "tile-fp64":
            _require(runtime.diagnostics.perf_tileops == iterations, "tile operation count differs")
            observation["tile_operations"] = iterations
        if expected_memory is not None:
            if workload == "audio":
                audio = runtime.memory.mmio.audio
                actual = audio.last_pcm
                _require(audio.generation == iterations and audio.error == 0,
                         "PCM submission count or error differs")
                _require((audio.last_frames, audio.last_channels, audio.last_rate) ==
                         (AUDIO_BYTES // 2, 1, 8000), "PCM metadata differs")
                observation["submissions"] = audio.generation
            else:
                actual = runtime.memory.read_bytes(DESTINATION_ADDRESS, len(expected_memory))
            _require(actual == expected_memory, f"{workload} output bytes differ")
            observation["output_sha256"] = hashlib.sha256(actual).hexdigest()
        return observation

    return PreparedWorkload(
        action=action, observe=observe,
        close=runtime.memory.mmio.audio.release_host_sink,
        counters=lambda: runtime.native_execution_stats,
        description=description,
    )


def prepare(workload: str, executor: str, iterations: int) -> PreparedWorkload:
    if not supported(workload, executor):
        raise ValueError(f"{workload} is not provided for {executor}")
    if executor == "emulator-native":
        return prepare_emulator_fp(iterations)
    return prepare_semantic(workload, executor, iterations)


def _profile_summary(profile: cProfile.Profile) -> dict:
    stats = pstats.Stats(profile).stats
    records = []
    fallbacks = {name: 0 for name in (
        "_step_python_fallback_in_memory_scope", "_sync_cs_to_py", "_sync_py_to_cs",
    )}
    for (filename, line, name), (primitive, total, own, cumulative, _callers) in stats.items():
        if name in fallbacks and filename.endswith("emulator/accel_wrapper.py"):
            fallbacks[name] += total
        try:
            label = str(Path(filename).relative_to(ROOT))
        except ValueError:
            label = filename
        records.append({"file": label, "line": line, "function": name,
                        "primitive_calls": primitive, "total_calls": total,
                        "own_seconds": own, "cumulative_seconds": cumulative})
    records.sort(key=lambda row: row["cumulative_seconds"], reverse=True)
    return {"python_fallback_and_sync_calls": fallbacks,
            "top_functions_by_cumulative_time": records[:25]}


def measure_once(workload: str, executor: str, iterations: int, *, attribution: bool) -> dict:
    with _native_profiling(attribution):
        setup_wall, setup_cpu = time.perf_counter(), time.process_time()
        fixture = prepare(workload, executor, iterations)
        setup = {"wall_seconds": time.perf_counter() - setup_wall,
                 "process_cpu_seconds": time.process_time() - setup_cpu}
        profile = cProfile.Profile() if attribution else None
        try:
            before = fixture.counters()
            if attribution and fixture.start_attribution:
                fixture.start_attribution()
            wall, cpu = time.perf_counter(), time.process_time()
            if profile is not None:
                profile.enable()
            try:
                result = fixture.action()
            finally:
                if profile is not None:
                    profile.disable()
            cpu_seconds, wall_seconds = time.process_time() - cpu, time.perf_counter() - wall
            after = fixture.counters()
            host_profile = (
                fixture.stop_attribution()
                if attribution and fixture.stop_attribution else None
            )
            # Capture counters before result checking; source-load validation
            # executes its generated word and must not inflate the load sample.
            observation = fixture.observe(result)
            record = {
                "wall_seconds": wall_seconds, "process_cpu_seconds": cpu_seconds,
                "process_lifetime_peak_rss_bytes": _peak_rss_bytes(),
                "setup": setup, "description": fixture.description,
                "guest": observation, "native_counter_delta": _delta(before, after),
            }
            if profile is not None:
                record["attribution"] = _profile_summary(profile)
                record["attribution"]["native_host_profile"] = host_profile
            return record
        finally:
            fixture.close()


def run_case(workload: str, executor: str, *, iterations: int, trials: int,
             warmup: int, attribution: bool) -> dict:
    artifact = _native_artifact(executor)
    for _ in range(warmup):
        measure_once(workload, executor, iterations, attribution=False)
    samples = [measure_once(workload, executor, iterations, attribution=False)
               for _ in range(trials)]
    expected = samples[0]["guest"]
    _require(all(sample["guest"] == expected for sample in samples),
             "guest observations differ across unprofiled trials")
    attributed = None
    if attribution:
        attributed = measure_once(workload, executor, iterations, attribution=True)
        _require(attributed["guest"] == expected, "profiling changed guest observations")
    if artifact is not None:
        _require(_sha256(Path(artifact["path"])) == artifact["sha256"],
                 "native binary changed during the case")
    return {"workload": workload, "executor": executor, "iterations": iterations,
            "warmup_instances": warmup, "native_artifact": artifact,
            "samples": samples, "attribution_run": attributed,
            "median_wall_seconds": statistics.median(row["wall_seconds"] for row in samples),
            "median_process_cpu_seconds": statistics.median(
                row["process_cpu_seconds"] for row in samples)}


def run_suite(args: argparse.Namespace) -> dict:
    repository, sources = _repository(), _source_manifest()
    cases, unsupported = [], []
    for workload in dict.fromkeys(args.workload):
        for executor in dict.fromkeys(args.executor):
            if not supported(workload, executor):
                unsupported.append({"workload": workload, "executor": executor})
                continue
            command = [sys.executable, str(Path(__file__).resolve()), "--worker",
                       "--workload", workload, "--executor", executor,
                       "--iterations", str(args.iterations), "--trials", str(args.trials),
                       "--warmup", str(args.warmup)]
            if args.attribution:
                command.append("--attribution")
            completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True,
                                       timeout=args.timeout, check=False)
            _require(completed.returncode == 0,
                     f"{workload}/{executor} failed: {completed.stderr.strip()}")
            cases.append(json.loads(completed.stdout))
    _require(bool(cases), "no supported workload/executor pair selected")
    _require(_source_manifest() == sources, "bound runtime sources changed during measurement")
    _require(_repository()["commit"] == repository["commit"], "repository commit changed during measurement")
    return {
        "schema": SCHEMA, "schema_version": 1,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "repository": repository, "source_sha256": sources,
        "host": {"platform": platform.platform(), "machine": platform.machine(),
                 "python": sys.version, "python_executable": sys.executable,
                 "cpu_count": os.cpu_count()},
        "measurement": {
            "order": "sequential fresh subprocess per workload/executor",
            "trials": "fresh prepared instance; setup, validation and cleanup excluded",
            "warmup": "discarded fresh instances; no retained guest/JIT cache",
            "attribution": "separate fresh instance, excluded from timing summaries",
            "rss": "process lifetime high-water mark, not a per-trial allocation delta",
            "case_timeout_seconds": args.timeout,
            "scope": "bounded kernels and synthetic source; no Desktop or full boot claim",
        },
        "unsupported_pairs": unsupported, "cases": cases,
    }


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.worker and (len(args.workload) != 1 or len(args.executor) != 1):
        parser.error("worker requires one workload and one executor")
    try:
        if args.worker:
            report = run_case(args.workload[0], args.executor[0], iterations=args.iterations,
                              trials=args.trials, warmup=args.warmup, attribution=args.attribution)
        else:
            report = run_suite(args)
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired) as exc:
        print(f"benchmark failed: {exc}", file=sys.stderr)
        return 1
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
