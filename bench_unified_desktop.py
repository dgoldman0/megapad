#!/usr/bin/env python3
"""Bounded Desktop journeys over production in-process session APIs.

Prepared images and journey expectations are explicit inputs. Every run owns
an isolated writable image copy; no source project is imported or rebuilt.
SDL's dummy software sink exercises composition and presentation acknowledgments,
not physical display, audio, Unix transport, or the standalone viewer loop.
CELL marker assertions describe the backing snapshot, not retained-text OCR.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager, redirect_stdout
from dataclasses import dataclass
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import shutil
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.unified-desktop.v1"
JOURNEY_SCHEMA = "megapad.desktop-journey.v1"
MAX_IMAGE_BYTES = 32 << 20
MAX_JOURNEY_BYTES = 256 << 10
REFERENCE_PROVENANCE = {
    "harness": "run_desk_flowing_unified.py",
    "harness_sha256": "a9fa986c62fe9768af2f5f67d041551dfc68c7729ff3eb9e0ad55470196fd56a",
    "runtime_checkpoint": "ca66ad7bb0bde9bfc57067f7067a1bf9151c6ff0",
    "appearance_checkpoint": "4e8bf26a18346043ea6b903fc8251a14e46e15b0",
    "recorded_image_before_journey_sha256": "db0b8a2d7c5b77eb8d08fb10774b5819f698792adda1dc4468d032034dbc7e2a",
    "recorded_result_sha256": "6ab957e2d9f1d0bfb9bbbce7d42b482b7f562f5a634b8df67722c5ffd5dfb48a",
}
_SEMANTIC_TAIL = re.compile(rb"' ([!-~]+) IS _SIMULATOR-SESSION-ENTRY")


class BenchmarkError(RuntimeError):
    pass


def _require(condition, message):
    if not condition:
        raise BenchmarkError(message)


def _bounded(value, label, minimum, maximum):
    if type(value) is not int or not minimum <= value <= maximum:
        raise ValueError(f"{label} must be an integer in {minimum}..{maximum}")
    return value


def _number(minimum, maximum):
    def parse(value):
        try:
            return _bounded(int(value), "value", minimum, maximum)
        except ValueError as exc:
            raise argparse.ArgumentTypeError(str(exc)) from exc
    return parse


def _sha256(path):
    with Path(path).open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def _repository():
    result = {}
    for key, command in (("commit", ["rev-parse", "HEAD"]),
                         ("status", ["status", "--porcelain=v1"])):
        item = subprocess.run(["git", "--no-optional-locks", *command], cwd=ROOT,
                              capture_output=True, text=True, timeout=5, check=False)
        result[key] = item.stdout.strip() if item.returncode == 0 else None
    result["harness_sha256"] = _sha256(Path(__file__).resolve())
    return result


def _strict_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate JSON field: {key}")
        result[key] = value
    return result


def _fields(value, required, optional=()):
    if type(value) is not dict or not set(required) <= value.keys() or value.keys() - set(required) - set(optional):
        raise ValueError(f"expected fields {sorted(required)} with optional {sorted(optional)}")


def _markers(value, label):
    if type(value) is not list or len(value) > 32:
        raise ValueError(f"{label} must be an array of at most 32 markers")
    if any(type(item) is not str or not 1 <= len(item.encode("utf-8")) <= 256 for item in value):
        raise ValueError(f"{label} markers must contain 1..256 UTF-8 bytes")
    return tuple(value)


@dataclass(frozen=True)
class Expectation:
    contains: tuple[str, ...]
    absent: tuple[str, ...]

    def matches(self, text):
        return all(item in text for item in self.contains) and all(item not in text for item in self.absent)


@dataclass(frozen=True)
class JourneyStep:
    name: str
    method: str
    value: str
    expect: Expectation
    timeout_seconds: int


@dataclass(frozen=True)
class Journey:
    name: str
    ready: Expectation
    steps: tuple[JourneyStep, ...]
    require_retained: bool
    sha256: str
    provenance: dict


def _expectation(value):
    _fields(value, ("contains",), ("absent",))
    contains = _markers(value["contains"], "contains")
    absent = _markers(value.get("absent", []), "absent")
    if not contains or set(contains) & set(absent):
        raise ValueError("an expectation needs positive, noncontradictory CELL markers")
    return Expectation(contains, absent)


def load_journey(path):
    path = Path(path)
    if not path.is_file() or path.stat().st_size > MAX_JOURNEY_BYTES:
        raise ValueError("journey must be a regular JSON file of at most 256 KiB")
    payload = path.read_bytes()
    value = json.loads(payload, object_pairs_hook=_strict_object,
                       parse_constant=lambda value: (_ for _ in ()).throw(ValueError(f"invalid JSON constant: {value}")))
    _fields(value, ("schema", "name", "ready", "steps", "require_retained"), ("provenance",))
    if value["schema"] != JOURNEY_SCHEMA:
        raise ValueError("unsupported Desktop journey schema")
    if type(value["name"]) is not str or not 1 <= len(value["name"]) <= 128:
        raise ValueError("journey name must contain 1..128 characters")
    if type(value["require_retained"]) is not bool:
        raise ValueError("require_retained must be a boolean")
    rows = value["steps"]
    if type(rows) is not list or not 1 <= len(rows) <= 64:
        raise ValueError("a journey must contain 1..64 bounded steps")
    steps = []
    for row in rows:
        _fields(row, ("name", "method", "value", "expect"), ("timeout_seconds",))
        if type(row["name"]) is not str or not 1 <= len(row["name"]) <= 128:
            raise ValueError("step name must contain 1..128 characters")
        if row["method"] not in ("send_text", "send_key"):
            raise ValueError("journey input must be send_text or send_key")
        if type(row["value"]) is not str or not 1 <= len(row["value"].encode("utf-8")) <= 4096:
            raise ValueError("input value must contain 1..4096 UTF-8 bytes")
        steps.append(JourneyStep(row["name"], row["method"], row["value"],
                                 _expectation(row["expect"]),
                                 _bounded(row.get("timeout_seconds", 30), "step timeout", 1, 120)))
    provenance = value.get("provenance", {})
    if type(provenance) is not dict:
        raise ValueError("provenance must be a JSON object")
    return Journey(value["name"], _expectation(value["ready"]), tuple(steps),
                   value["require_retained"], hashlib.sha256(payload).hexdigest(), provenance)


def inspect_autoexec(payload):
    """Identify an exact final entry line without executing or rewriting it."""
    lines = payload.splitlines(keepends=True)
    nonblank = [index for index, line in enumerate(lines) if line.strip()]
    if not nonblank:
        raise ValueError("autoexec.f has no final entry line")
    index = nonblank[-1]
    tail = lines[index].strip()
    match = _SEMANTIC_TAIL.fullmatch(tail)
    entry = match[1] if match else tail
    if not re.fullmatch(rb"[!-~]+", entry):
        raise ValueError("autoexec.f has no recognized standalone final entry")
    if not re.search(rb"(?m)^\s*:\s+" + re.escape(entry) + rb"(?:\s|$)", payload):
        raise ValueError("autoexec final entry has no source definition")
    return {"kind": "semantic_deferred" if match else "ordinary_invocation",
            "entry": entry.decode("ascii"), "line_index": index,
            "tail": tail.decode("ascii"),
            "sha256": hashlib.sha256(payload).hexdigest()}


def restore_emulator_tail(payload):
    """Reverse only the explicit semantic deferral on a private fixture copy."""
    info = inspect_autoexec(payload)
    if info["kind"] != "semantic_deferred":
        raise ValueError("tail restoration requires the exact semantic entry binding")
    lines = payload.splitlines(keepends=True)
    index = info["line_index"]
    ending = b"\r\n" if lines[index].endswith(b"\r\n") else b"\n" if lines[index].endswith(b"\n") else b""
    lines[index] = info["entry"].encode("ascii") + ending
    return b"".join(lines)


@contextmanager
def isolated_image(source, mode, *, restore_tail=False):
    """Yield a private image and provenance; preserve the original on all exits."""
    from diskutil import MP64FS
    source = Path(source).resolve()
    if not source.is_file() or not 512 <= source.stat().st_size <= MAX_IMAGE_BYTES:
        raise ValueError("prepared image must be a regular file of at most 32 MiB")
    original_hash = _sha256(source)
    with tempfile.TemporaryDirectory(prefix="megapad-desktop-image-") as directory:
        image = Path(directory) / "desktop.img"
        shutil.copyfile(source, image)
        copied_hash = _sha256(image)
        _require(copied_hash == original_hash, "prepared image changed while copying")
        fs = MP64FS.load(image)
        autoexec = fs.read_file("autoexec.f")
        info = inspect_autoexec(autoexec)
        before = dict(info)
        if restore_tail:
            if mode != "emulator":
                raise ValueError("emulator tail restoration is available only in emulator mode")
            replacement = restore_emulator_tail(autoexec)
            _, entry = fs.find_file("autoexec.f")
            fs.delete_file("autoexec.f")
            fs.inject_file("autoexec.f", replacement, ftype=entry.ftype, flags=entry.flags)
            fs.save(image)
            info = inspect_autoexec(replacement)
        required = "ordinary_invocation" if mode == "emulator" else "semantic_deferred"
        if info["kind"] != required:
            raise ValueError(f"{mode} requires autoexec kind {required}; observed {info['kind']}")
        metadata = {"source": str(source), "source_sha256": original_hash,
                    "copy_before_execution_sha256": _sha256(image),
                    "autoexec_before": before, "autoexec_selected": info,
                    "restored_emulator_tail": restore_tail,
                    "original_preserved": False}
        try:
            yield image, metadata
        finally:
            metadata["copy_after_execution_sha256"] = _sha256(image)
            metadata["original_preserved"] = _sha256(source) == original_hash
            _require(metadata["original_preserved"], "original prepared image changed during the run")


def build_parser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--image", type=Path, required=True)
    parser.add_argument("--journey", type=Path, required=True)
    parser.add_argument("--mode", choices=("emulator", "simulator", "hybrid"), required=True)
    parser.add_argument("--executor", choices=("python", "native", "auto"), default="native")
    parser.add_argument("--rich-terminal-policy", help="complete caller-owned JSON policy")
    parser.add_argument("--retained-terminal-policy", help="complete caller-owned JSON policy")
    parser.add_argument("--cols", type=_number(1, 400), default=280)
    parser.add_argument("--rows", type=_number(1, 200), default=84)
    parser.add_argument("--ram-kib", type=_number(64, 1024), default=1024)
    parser.add_argument("--ext-mem-mib", type=_number(0, 512), default=320)
    parser.add_argument("--vram-mib", type=_number(0, 16), default=4)
    parser.add_argument("--semantic-quantum-steps", type=_number(1, 1_000_000),
                        help="override the production session's selected-executor quantum")
    parser.add_argument("--semantic-step-budget", type=_number(1, 1_000_000_000))
    parser.add_argument("--timeout", type=_number(1, 900), default=240)
    parser.add_argument("--font", type=Path)
    parser.add_argument("--font-size", type=_number(6, 32), default=12)
    parser.add_argument("--restore-emulator-tail", action="store_true")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--artifacts", type=Path, help="optional directory for initial/final SDL captures")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def validate_paths(args):
    inputs = [args.image, args.journey] + ([] if args.font is None else [args.font])
    if args.output is not None:
        target = args.output.resolve()
        for source in inputs:
            if target == source.resolve() or target.exists() and source.exists() and target.samefile(source):
                raise ValueError("output must not replace an input file")
    if args.artifacts is not None and args.artifacts.exists():
        raise ValueError("artifact directory must be new for this run")


def _server_arguments(args, image, directory):
    result = ["--storage", str(image), "--socket", str(directory / "unused.sock"),
              "--ram-kib", str(args.ram_kib), "--ext-mem-mib", str(args.ext_mem_mib),
              "--vram-mib", str(args.vram_mib), "--cols", str(args.cols), "--rows", str(args.rows)]
    for option in ("rich_terminal_policy", "retained_terminal_policy"):
        value = getattr(args, option)
        if value is not None:
            result += ["--" + option.replace("_", "-"), value]
    if args.mode != "emulator":
        result += ["--executor", args.executor]
        if args.semantic_quantum_steps is not None:
            result += ["--semantic-quantum-steps", str(args.semantic_quantum_steps)]
        if args.semantic_step_budget is not None:
            result += ["--semantic-step-budget", str(args.semantic_step_budget)]
    elif args.executor != "native" or args.semantic_step_budget is not None:
        raise ValueError("emulator requires native execution and has no semantic budget")
    if args.mode == "hybrid":
        from shared.hybrid_abi import HYBRID_ABI
        manifest = directory / "empty-routines.json"
        manifest.write_text(json.dumps({"abi": HYBRID_ABI, "version": 1,
                                       "dispatch_instruction_limit": 1_000_000, "routines": []}))
        result += ["--hybrid-routines", str(manifest)]
    return result


def prepare_server(args, image, directory):
    """Use production constructors without opening or replacing a listener."""
    arguments = _server_arguments(args, image, directory)
    if args.mode != "emulator":
        if args.mode == "hybrid":
            from hybrid import server as backend
        else:
            import simulator_server as backend
        return backend.prepare_server(backend.build_argument_parser().parse_args(arguments)).server
    import session_server as backend
    from emulator.session import MachineSession
    from emulator.shared_session import SharedMachine
    from shared_session import SessionServer
    options = backend.build_argument_parser().parse_args(arguments)
    if options.retained_terminal_policy is not None and options.rich_terminal_policy is None:
        raise ValueError("retained terminal policy requires rich terminal policy")
    rich = None if options.rich_terminal_policy is None else options.rich_terminal_policy.configuration(
        options.cols, options.rows, retained_policy=options.retained_terminal_policy)
    session = MachineSession.from_bios(
        options.bios, storage_image=options.storage, ram_size=options.ram_kib << 10,
        ext_mem_size=options.ext_mem_mib << 20, vram_size=options.vram_mib << 20,
        num_cores=1, num_clusters=0, lanes=1, cols=options.cols, rows=options.rows,
        batch_steps=options.batch_steps, realtime_clock=True, rich_terminal=rich,
    )
    return SessionServer(SharedMachine(session), options.socket)


class DirectClient:
    def __init__(self, server):
        self.server = server

    def request(self, method, **params):
        return self.server.dispatch(method, params, connection_id=1)


def send_input(client, step, generation, display_ack):
    from shared_session import display_scope_to_wire
    field = "text" if step.method == "send_text" else "key"
    params = {field: step.value, "generation": generation}
    if display_ack is not None:
        params.update(display_offer_id=display_ack[0], display_scope=display_scope_to_wire(display_ack[1]))
    result = client.request(step.method, **params)
    count_field = "accepted_bytes" if step.method == "send_text" else "accepted_events"
    expected = len(step.value.encode("utf-8")) if step.method == "send_text" else 1
    status = result.get("status")
    if status == "progress":
        _require(result.get(count_field) == expected, "input publication was partial")
        return True
    _require(result.get(count_field) == 0, "rejected input reported nonzero effects")
    _require(status in ("backpressured", "stale_display"), f"input rejected: {status}")
    return False


def run_journey(server, args, journey, report, started):
    import pygame
    from display import VirtualTerminal
    from rich_terminal.font_set import FontSet
    from shared_session import snapshot_from_wire
    from session_viewer import (_DisplayResourceCache, _GuestKeyboardForwarder,
        _RetainedDisplayState, _accept_screen_update, _accept_status_update,
        compose_terminal_frame_changes, draw_flip_and_present)
    client = DirectClient(server)
    _require(client.request("claim_display").get("claimed"), "direct client could not claim display")
    status = client.request("status", detailed=False)
    report["runtime"] = status["runtime"]
    report["last_status"] = status
    report["actual_semantic_quantum_steps"] = status.get("semantic_execution", {}).get("quantum_steps")
    _require(status["runtime"]["mode"] == args.mode, "selected session mode differs")
    pygame.display.init()
    pygame.font.init()
    terminal = VirtualTerminal(cols=args.cols, rows=args.rows)
    font = FontSet(pygame, args.font, args.font_size)
    control_font = FontSet(pygame, args.font, args.font_size, cells=False)
    width, height = font.cell_width, font.cell_height
    window = pygame.display.set_mode((args.cols * width, args.rows * height))
    report["font"] = {"path": str(font.faces[0].path), "sha256": _sha256(font.faces[0].path),
                      "size": args.font_size, "cell_width": width, "cell_height": height}
    display, resources = _RetainedDisplayState(), _DisplayResourceCache()
    keyboard = _GuestKeyboardForwarder(pygame, client, generation=status["generation"],
        display_required=status["rich_terminal"]["display_required"])
    revision, previous, glyph_cache = -1, None, {}
    generation = status["generation"]
    stage, sent, sent_token = -1, False, None
    stage_started = time.monotonic()
    deadline = started + args.timeout
    last_text, last_snapshot = "", None
    report.update(steps=[], presented_offers=0, input_retries=0)

    def capture(name):
        if args.artifacts is not None and previous is not None:
            args.artifacts.mkdir(parents=True, exist_ok=True)
            pygame.image.save(previous.surface, str(args.artifacts / f"{name}.png"))
            (args.artifacts / f"{name}.cell.txt").write_text(last_text, encoding="utf-8")

    for _poll in range(100_000):
        now = time.monotonic()
        _require(now < deadline, f"Desktop deadline reached at stage {stage}")
        if stage >= 0:
            _require(now - stage_started <= journey.steps[stage].timeout_seconds,
                     f"Desktop step timed out: {journey.steps[stage].name}")
        pygame.event.pump()
        status = client.request("status", detailed=False)
        report["last_status"] = status
        _require(not status.get("error"), f"guest session failed: {status.get('error')}")
        _require(not status["rich_terminal"].get("failure"), "rich terminal failed")
        _require(status["generation"] == generation, "session generation changed during the journey")
        revision, _ = _accept_status_update(status, keyboard=keyboard, display_state=display,
                                             resource_cache=resources, revision=revision)
        update = client.request("screen", since=revision, since_offer=display.poll_offer_cursor,
                                base_offer=display.base_offer_id)
        revision, resized = _accept_screen_update(update, display_holder=True, terminal=terminal,
            keyboard=keyboard, display_state=display, resource_cache=resources, revision=revision)
        _require(not resized, "journey changed its declared terminal geometry")
        if "snapshot" in update:
            last_snapshot = snapshot_from_wire(update["snapshot"])
        offer = display.pending_offer
        if offer is not None:
            if not resources.pending_ready(offer, generation):
                outcome = resources.fetch_pending_chunk(client, pygame, offer, generation)
                _require(outcome not in ("invalid_resource", "stale_generation"), f"resource fetch failed: {outcome}")
                if outcome == "stale_display":
                    display.reset(); resources.clear(); revision = -1
                    keyboard.clear_display_context(waiting=keyboard.display_required)
                    continue
                if not resources.pending_ready(offer, generation):
                    continue
            display.stage_resources_ready(offer)
            surfaces = resources.pending_surfaces(offer, generation)
        else:
            surfaces = resources.acknowledged_surfaces
        if offer is not None or update["changed"]:
            def draw():
                nonlocal previous
                previous = compose_terminal_frame_changes(pygame, terminal, font, width, height,
                    retained_plane=display.frame_plane, show_cursor=True, glyph_cache=glyph_cache,
                    control_font=control_font, resource_surfaces=surfaces, previous=previous)
                window.blit(previous.surface, (0, 0))
                if offer is not None:
                    display.stage_frame_hit_map(offer, previous.hit_entries)
            response = draw_flip_and_present(pygame, client, draw, offer=offer, generation=generation)
            if offer is not None:
                accepted = display.finish_presentation(response)
                if accepted is None:
                    resources.clear(); revision = -1
                    keyboard.clear_display_context(waiting=keyboard.display_required)
                    continue
                resources.promote(offer, generation)
                revision = accepted
                keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
                report["presented_offers"] += 1
                last_snapshot = offer.cell
        presented = display.presented_offer
        last_text = "" if last_snapshot is None else last_snapshot.text(trim_right=True)
        report["last_cell_text"] = last_text
        token = (revision, 0 if presented is None else presented.offer_id)
        retained_ready = (not journey.require_retained or presented is not None and
                          presented.retained.retained_visible and bool(presented.retained.regions))
        if stage < 0:
            if retained_ready and journey.ready.matches(last_text):
                report["ready_seconds"] = time.monotonic() - started
                capture("initial")
                stage, stage_started = 0, time.monotonic()
        elif not sent:
            if keyboard.display_required and keyboard.display_ack is None:
                continue
            sent = send_input(client, journey.steps[stage], generation, keyboard.display_ack)
            if sent:
                sent_token = token
            else:
                report["input_retries"] += 1
                revision = -1
        elif token != sent_token and retained_ready and journey.steps[stage].expect.matches(last_text):
            report["steps"].append({"name": journey.steps[stage].name,
                "elapsed_seconds": time.monotonic() - started,
                "step_seconds": time.monotonic() - stage_started,
                "cell_revision": revision, "offer_id": token[1]})
            stage += 1
            if stage == len(journey.steps):
                report["final_status"] = client.request("status", detailed=False)
                report["last_status"] = report["final_status"]
                capture("final")
                return
            sent, stage_started = False, time.monotonic()
        time.sleep(0.01)
    raise BenchmarkError("Desktop poll bound exhausted")


def _cleanup_status(server, mode):
    machine, session = server.machine, server.machine.session
    result = {"owner_thread_stopped": machine._thread is None or not machine._thread.is_alive(),
              "display_lease_released": server._display_holder is None,
              "terminal_driver_closed": session.rich_terminal_driver is None,
              "no_socket_created": server._socket is None and server._socket_owner is None}
    if mode != "emulator":
        result.update(backend_closed=session.backend.closed,
                      runtime_owner_released=session.runtime._session_owner_token is None)
    if mode == "hybrid":
        result["hybrid_closed"] = session.hybrid.closed
    _require(all(result.values()), f"session cleanup did not settle: {result}")
    return result


def run_case(args):
    validate_paths(args)
    journey = load_journey(args.journey)
    if args.retained_terminal_policy is not None and args.rich_terminal_policy is None:
        raise ValueError("retained terminal policy requires rich terminal policy")
    if journey.require_retained and args.retained_terminal_policy is None:
        raise ValueError("journey requires an explicit retained terminal policy")
    repository = _repository()
    saved_environment = {key: os.environ.get(key) for key in
                         ("SDL_VIDEODRIVER", "SDL_AUDIODRIVER", "PYGAME_HIDE_SUPPORT_PROMPT")}
    os.environ["SDL_VIDEODRIVER"] = "dummy"
    os.environ["SDL_AUDIODRIVER"] = "dummy"
    os.environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "1"
    report = {"schema": SCHEMA, "mode": args.mode, "requested_executor": args.executor,
              "complete": False, "reference_provenance": REFERENCE_PROVENANCE,
              "repository": repository,
              "journey": {"name": journey.name, "sha256": journey.sha256, "steps": len(journey.steps),
                          "provenance": journey.provenance},
              "scope": {"transport": "direct SessionServer.dispatch; no listener",
                        "display": "SDL dummy software sink",
                        "assertions": "CELL backing snapshot markers plus production composition and acknowledgments",
                        "socket_qualified": False, "physical_display_qualified": False,
                        "audio_qualified": False, "standalone_viewer_qualified": False,
                        "hybrid_machine_calls_exercised": False},
              "configuration": {name: getattr(args, name) for name in (
                  "cols", "rows", "ram_kib", "ext_mem_mib", "vram_mib", "timeout",
                  "semantic_quantum_steps", "semantic_step_budget", "rich_terminal_policy",
                  "retained_terminal_policy")}}
    started = time.monotonic()
    server = None
    try:
        with isolated_image(args.image, args.mode, restore_tail=args.restore_emulator_tail) as (image, metadata):
            report["image"] = metadata
            try:
                server = prepare_server(args, image, image.parent)
                report["preparation_seconds"] = time.monotonic() - started
                report["native_artifacts"] = []
                for name in ("_mp64_accel", "_megaforth_native"):
                    module = sys.modules.get(name)
                    if module is None:
                        continue
                    path = Path(module.__file__).resolve()
                    _require(path.parent == ROOT, f"{name} was loaded outside this checkout")
                    report["native_artifacts"].append({"module": name, "path": str(path),
                                                       "sha256": _sha256(path)})
                server.machine.start()
                run_journey(server, args, journey, report, started)
                if args.mode == "hybrid":
                    machine = report["final_status"]["machine_execution"]
                    _require(machine["instructions"] == machine["transitions"] == 0,
                             "empty-registry Desktop unexpectedly executed a machine routine")
                    report["hybrid_registry"] = "empty; zero machine calls; composition compatibility only"
            finally:
                if server is not None:
                    server.stop()
                    report["cleanup"] = _cleanup_status(server, args.mode)
        report["complete"] = True
    except Exception as exc:
        report["error"] = f"{type(exc).__name__}: {exc}"
    finally:
        if "pygame" in sys.modules:
            sys.modules["pygame"].quit()
        for key, value in saved_environment.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value
        report["elapsed_seconds"] = time.monotonic() - started
        rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        report["peak_rss_bytes"] = rss if sys.platform == "darwin" else rss * 1024
    return report


def main(argv=None):
    arguments = list(sys.argv[1:] if argv is None else argv)
    args = build_parser().parse_args(arguments)
    try:
        validate_paths(args)
    except ValueError as exc:
        build_parser().error(str(exc))
    if args.worker:
        with redirect_stdout(sys.stderr):
            try:
                report = run_case(args)
            except Exception as exc:
                report = {"schema": SCHEMA, "mode": args.mode, "complete": False,
                          "error": f"{type(exc).__name__}: {exc}"}
    else:
        # The outer watchdog bounds preparation, native intervals, owner-lock
        # acquisition and shutdown as well as the cooperative journey loop.
        try:
            worker = subprocess.run([sys.executable, str(Path(__file__).resolve()),
                *arguments, "--worker"], cwd=ROOT, capture_output=True, text=True,
                timeout=args.timeout + 10, check=False,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"})
            report = json.loads(worker.stdout)
            if worker.returncode != 0 and report.get("complete"):
                raise BenchmarkError("worker exit status disagrees with its report")
        except subprocess.TimeoutExpired:
            report = {"schema": SCHEMA, "mode": args.mode, "complete": False,
                      "error": "Desktop subprocess watchdog expired", "timeout_seconds": args.timeout + 10}
        except (ValueError, BenchmarkError) as exc:
            report = {"schema": SCHEMA, "mode": args.mode, "complete": False,
                      "error": f"invalid worker report: {exc}",
                      "worker_stderr_tail": worker.stderr[-4096:]}
    if args.output is not None and not args.worker:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    return 0 if report.get("complete") else 1


if __name__ == "__main__":
    raise SystemExit(main())
