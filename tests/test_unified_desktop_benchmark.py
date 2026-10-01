"""Desktop qualification owns fixture copies and publishes honest evidence."""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys

import pytest

import bench_unified_desktop as bench
from diskutil import FTYPE_FORTH, MP64FS


AUTOEXEC = b": LIVE KEY EMIT ;\n' LIVE IS _SIMULATOR-SESSION-ENTRY\n"


def _image(tmp_path, autoexec=AUTOEXEC, *, runnable=False):
    fs = MP64FS(total_sectors=128)
    fs.format()
    if runnable:
        fs.inject_file("kdos.f", (
            b": DEFER CREATE ['] ABORT , DOES> @ EXECUTE ;\n"
            b": IS ' >BODY ! ;\n"
            b': LIVE ." ready>" KEY EMIT KEY EMIT ;\n'
            b': _AUTOEXEC-RUN S" \' LIVE IS _SIMULATOR-SESSION-ENTRY" EVALUATE ;\n'
            b"_AUTOEXEC-RUN\n"
        ), ftype=FTYPE_FORTH)
    fs.inject_file("autoexec.f", autoexec, ftype=FTYPE_FORTH)
    fs.inject_file("untouched.txt", b"original unrelated content")
    path = tmp_path / "source.img"
    fs.save(path)
    return path


def _document():
    return {"schema": bench.JOURNEY_SCHEMA, "name": "two keys", "require_retained": False,
            "ready": {"contains": ["ready>"]}, "steps": [
                {"name": "first", "method": "send_text", "value": "K", "expect": {"contains": ["ready>K"]}},
                {"name": "second", "method": "send_key", "value": "x", "expect": {"contains": ["ready>Kx"]}},
            ]}


def _journey(tmp_path, document=None):
    path = tmp_path / "journey.json"
    path.write_text(json.dumps(_document() if document is None else document))
    return path


def _args(tmp_path, *, mode="simulator", executor="python", quantum_steps=None):
    image = _image(tmp_path, runnable=True)
    journey = _journey(tmp_path)
    arguments = [
        "--image", str(image), "--journey", str(journey), "--mode", mode,
        "--executor", executor, "--cols", "20", "--rows", "3",
        "--ext-mem-mib", "0", "--vram-mib", "0", "--timeout", "20",
    ]
    if quantum_steps is not None:
        arguments += ["--semantic-quantum-steps", str(quantum_steps)]
    return bench.build_parser().parse_args(arguments)


def test_image_copy_is_private_and_original_survives_guest_failure(tmp_path):
    source = _image(tmp_path)
    original = source.read_bytes()
    with pytest.raises(RuntimeError, match="guest failure"):
        with bench.isolated_image(source, "simulator") as (copy, metadata):
            assert copy != source
            assert copy.read_bytes() == original
            copy.write_bytes(copy.read_bytes()[:-1] + b"!")
            raise RuntimeError("guest failure")
    assert source.read_bytes() == original
    assert metadata["original_preserved"]
    assert metadata["source_sha256"] != metadata["copy_after_execution_sha256"]
    assert not copy.exists()


def test_emulator_tail_restoration_is_explicit_and_only_changes_the_entry_source(tmp_path):
    source = _image(tmp_path)
    original = source.read_bytes()
    with pytest.raises(ValueError, match="ordinary_invocation"):
        with bench.isolated_image(source, "emulator"):
            pytest.fail("semantic image was accepted for emulator boot")
    with bench.isolated_image(source, "emulator", restore_tail=True) as (copy, metadata):
        fs = MP64FS.load(copy)
        assert fs.read_file("autoexec.f") == b": LIVE KEY EMIT ;\nLIVE\n"
        assert fs.read_file("untouched.txt") == b"original unrelated content"
        assert metadata["autoexec_selected"]["kind"] == "ordinary_invocation"
        assert metadata["restored_emulator_tail"]
    assert source.read_bytes() == original
    with pytest.raises(ValueError, match="only in emulator"):
        with bench.isolated_image(source, "hybrid", restore_tail=True):
            pytest.fail("hybrid image was rewritten")


@pytest.mark.parametrize("payload", (
    b"", b"' LIVE IS _SIMULATOR-SESSION-ENTRY\n",
    b": LIVE ;\nLIVE EXTRA\n", b": LIVE ;\n' LIVE IS _SIMULATOR-SESSION-ENTRY EXTRA\n",
))
def test_unrecognized_image_tail_is_rejected_without_guessing(payload):
    with pytest.raises(ValueError):
        bench.inspect_autoexec(payload)


def test_tail_restoration_preserves_prefix_comments_blank_lines_and_crlf():
    prefix = b"\\ ' OTHER IS _SIMULATOR-SESSION-ENTRY\r\n: LIVE ;\r\n"
    assert bench.restore_emulator_tail(prefix + b"' LIVE IS _SIMULATOR-SESSION-ENTRY\r\n\r\n") == (
        prefix + b"LIVE\r\n\r\n"
    )


@pytest.mark.parametrize("change", (
    lambda value: value.update(steps=[]),
    lambda value: value.update(steps=value["steps"] * 33),
    lambda value: value["steps"][0].update(method="execute_source"),
    lambda value: value["steps"][0].update(value="x" * 4097),
    lambda value: value["steps"][0].update(timeout_seconds=241),
    lambda value: value["ready"].update(contains=[]),
    lambda value: value["ready"].update(absent=["ready>"]),
    lambda value: value.update(extra=True),
))
def test_journey_requires_finite_explicit_input_and_positive_expectations(tmp_path, change):
    value = _document()
    change(value)
    with pytest.raises(ValueError):
        bench.load_journey(_journey(tmp_path, value))


def test_bundled_keyboard_subset_has_pinned_provenance_and_eight_steps():
    journey = bench.load_journey(bench.ROOT / "tests/fixtures/desktop-keyboard-journey.json")
    assert len(journey.steps) == 8
    assert journey.require_retained
    assert "subset" in journey.provenance["scope"]
    assert len(journey.provenance["reference_source_sha256"]) == 64


@pytest.mark.parametrize("seconds", (1, 30, 120, 121, 240))
def test_explicit_step_timeouts_accept_bounded_diagnostic_allowances(tmp_path, seconds):
    document = _document()
    document["steps"][0]["timeout_seconds"] = seconds
    journey = bench.load_journey(_journey(tmp_path, document))
    assert journey.steps[0].timeout_seconds == seconds
    assert journey.steps[1].timeout_seconds == 30


@pytest.mark.parametrize("seconds", (-1, 0, 241, True, False, 30.0, None, "240"))
def test_step_timeouts_reject_out_of_bounds_and_non_integer_values(tmp_path, seconds):
    document = _document()
    document["steps"][0]["timeout_seconds"] = seconds
    with pytest.raises(ValueError, match="step timeout must be an integer in 1..240"):
        bench.load_journey(_journey(tmp_path, document))


@pytest.mark.parametrize("seconds", (None, "1", "900", "1200"))
def test_overall_timeout_keeps_default_and_accepts_bounded_diagnostics(seconds):
    argv = ["--image", "prepared.img", "--journey", "journey.json", "--mode", "simulator"]
    if seconds is not None:
        argv += ["--timeout", seconds]
    args = bench.build_parser().parse_args(argv)
    assert args.timeout == (240 if seconds is None else int(seconds))
    assert args.semantic_quantum_steps is None


@pytest.mark.parametrize("seconds", ("-1", "0", "1201", "1.5", "nan"))
def test_overall_timeout_rejects_unbounded_or_non_integer_values(seconds):
    with pytest.raises(SystemExit) as error:
        bench.build_parser().parse_args([
            "--image", "prepared.img", "--journey", "journey.json", "--mode", "simulator",
            "--timeout", seconds,
        ])
    assert error.value.code == 2


def test_python_diagnostic_preserves_exact_standard_journey_except_declared_timeouts():
    standard_path = bench.ROOT / "tests/fixtures/desktop-keyboard-journey.json"
    diagnostic_path = bench.ROOT / "tests/fixtures/desktop-keyboard-python-diagnostic.json"
    standard_bytes = standard_path.read_bytes()
    standard = json.loads(standard_bytes)
    diagnostic = json.loads(diagnostic_path.read_bytes())
    standard_sha = hashlib.sha256(standard_bytes).hexdigest()
    assert standard_sha == "6446634c46a53013b16b27ceaf8b1b90fbe0715bfd881d698dc65ab9bcac3049"
    assert "diagnostic" in diagnostic["name"]
    provenance = diagnostic["provenance"]
    assert provenance["derived_from"] == "tests/fixtures/desktop-keyboard-journey.json"
    assert provenance["derived_from_sha256"] == standard_sha
    assert "not production latency qualification" in provenance["diagnostic"]
    assert [step.pop("timeout_seconds") for step in diagnostic["steps"]] == [
        60, 60, 240, 60, 240, 240, 60, 60,
    ]
    diagnostic["name"] = standard["name"]
    diagnostic["provenance"] = {key: provenance[key] for key in standard["provenance"]}
    assert diagnostic == standard

    original = bench.load_journey(standard_path)
    expanded = bench.load_journey(diagnostic_path)
    assert [step.timeout_seconds for step in original.steps] == [30] * 8
    assert [step.timeout_seconds for step in expanded.steps] == [60, 60, 240, 60, 240, 240, 60, 60]
    assert expanded.ready == original.ready
    assert expanded.require_retained is original.require_retained is True
    assert expanded.sha256 != original.sha256


def test_outputs_cannot_replace_inputs_or_reuse_an_artifact_directory(tmp_path):
    args = _args(tmp_path)
    args.output = args.image
    with pytest.raises(ValueError, match="replace an input"):
        bench.validate_paths(args)
    args.output = tmp_path / "hardlinked-output.json"
    args.output.hardlink_to(args.image)
    with pytest.raises(ValueError, match="replace an input"):
        bench.validate_paths(args)
    args.output = None
    args.artifacts = tmp_path
    with pytest.raises(ValueError, match="must be new"):
        bench.validate_paths(args)


def test_emulator_and_hybrid_configuration_keep_separate_execution_contracts(tmp_path):
    args = _args(tmp_path, mode="emulator", executor="python")
    with pytest.raises(ValueError, match="requires native"):
        bench._server_arguments(args, args.image, tmp_path)
    args.executor = "native"
    argv = bench._server_arguments(args, args.image, tmp_path)
    assert "--executor" not in argv and "--semantic-quantum-steps" not in argv
    args.mode = "hybrid"
    argv = bench._server_arguments(args, args.image, tmp_path)
    # The Desktop benchmark defines no routines; hybrid composition only.
    assert "--hybrid-routines" not in argv
    assert argv[argv.index("--executor") + 1] == "native"


@pytest.mark.parametrize("mode", ("simulator", "hybrid"))
@pytest.mark.parametrize("quantum_steps", (None, 4096))
def test_semantic_quantum_uses_production_policy_unless_explicit(tmp_path, mode, quantum_steps):
    args = _args(tmp_path, mode=mode, quantum_steps=quantum_steps)
    assert args.semantic_quantum_steps == quantum_steps
    argv = bench._server_arguments(args, args.image, tmp_path)
    if quantum_steps is None:
        assert "--semantic-quantum-steps" not in argv
    else:
        assert argv[argv.index("--semantic-quantum-steps") + 1] == "4096"


def test_input_retry_preserves_exact_generation_and_display_proof():
    from rich_terminal.retained_view import DisplayScope
    scope = DisplayScope(1, 2, 0, 3, 0, 3, 3)
    responses = iter(({"status": "backpressured", "accepted_bytes": 0},
                      {"status": "progress", "accepted_bytes": 2}))
    calls = []

    class Client:
        def request(self, method, **params):
            calls.append((method, params))
            return next(responses)

    step = bench.JourneyStep("accent", "send_text", "é", bench.Expectation(("é",), ()), 1)
    assert not bench.send_input(Client(), step, 7, (2, scope))
    assert bench.send_input(Client(), step, 7, (2, scope))
    assert calls[0] == calls[1]
    assert calls[0][1]["generation"] == 7
    assert calls[0][1]["display_offer_id"] == 2


@pytest.mark.parametrize("response", (
    {"status": "progress", "accepted_bytes": 1},
    {"status": "backpressured", "accepted_bytes": 1},
    {"status": "stale_generation", "accepted_bytes": 0},
))
def test_partial_or_unauthorized_input_is_not_retried(response):
    class Client:
        def request(self, *_args, **_params):
            return response
    step = bench.JourneyStep("accent", "send_text", "é", bench.Expectation(("é",), ()), 1)
    with pytest.raises(bench.BenchmarkError):
        bench.send_input(Client(), step, 7, None)


@pytest.mark.parametrize("mode,executor", (("simulator", "python"), ("simulator", "native"), ("hybrid", "native")))
def test_real_direct_session_runs_copied_image_and_releases_ownership(tmp_path, monkeypatch, mode, executor):
    pytest.importorskip("pygame")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    if mode == "hybrid":
        pytest.importorskip("_mp64_accel")
    monkeypatch.delenv("MEGAFORTH_QUANTUM_STEPS", raising=False)
    args = _args(tmp_path, mode=mode, executor=executor)
    original = args.image.read_bytes()
    report = bench.run_case(args)
    assert report["complete"], report
    assert [item["name"] for item in report["steps"]] == ["first", "second"]
    assert report["runtime"]["mode"] == mode
    assert report["configuration"]["semantic_quantum_steps"] is None
    assert report["actual_semantic_quantum_steps"] == (8192 if executor == "python" else 65536)
    assert report["actual_semantic_quantum_steps"] == report["final_status"]["semantic_execution"]["quantum_steps"]
    assert report["last_status"] == report["final_status"]
    assert all(report["cleanup"].values())
    assert report["image"]["original_preserved"]
    assert args.image.read_bytes() == original
    assert not report["scope"]["socket_qualified"]
    assert not report["scope"]["physical_display_qualified"]
    if mode == "hybrid":
        assert report["final_status"]["machine_execution"]["instructions"] == 0
        assert "zero machine calls" in report["hybrid_registry"]


def test_failed_expectation_is_reported_and_owner_still_closes(tmp_path, monkeypatch):
    pytest.importorskip("pygame")
    monkeypatch.setenv("MEGAFORTH_QUANTUM_STEPS", "4096")
    observed_status = []
    request = bench.DirectClient.request

    def record_status(self, method, **params):
        response = request(self, method, **params)
        if method == "status":
            observed_status.append(response)
        return response

    monkeypatch.setattr(bench.DirectClient, "request", record_status)
    args = _args(tmp_path)
    document = _document()
    document["steps"][0].update(expect={"contains": ["never emitted"]}, timeout_seconds=1)
    _journey(tmp_path, document)
    report = bench.run_case(args)
    assert not report["complete"]
    assert "step timed out" in report["error"]
    assert "final_status" not in report
    assert len(observed_status) > 1
    assert report["last_status"] is observed_status[-1]
    assert report["last_status"] is not observed_status[0]
    assert report["configuration"]["semantic_quantum_steps"] is None
    assert report["actual_semantic_quantum_steps"] == 4096
    assert report["last_status"]["semantic_execution"]["quantum_steps"] == 4096
    assert all(report["cleanup"].values())
    assert report["image"]["original_preserved"]


def test_help_requires_neither_native_engines_nor_external_source_project():
    code = """
import importlib.abc
import sys
class Guard(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in ('_mp64_accel', '_megaforth_native', 'pygame', 'akashic_tui', 'rich_terminal_desktop_acceptance'):
            raise AssertionError(fullname)
sys.meta_path.insert(0, Guard())
import bench_unified_desktop
bench_unified_desktop.main(['--help'])
"""
    result = subprocess.run([sys.executable, "-c", code], cwd=bench.ROOT,
                            capture_output=True, text=True, timeout=10)
    assert result.returncode == 0, result.stderr
    assert "--mode {emulator,simulator,hybrid}" in result.stdout
    assert "reference-executor diagnostics" in " ".join(result.stdout.split())


@pytest.mark.parametrize("seconds", (1, 1200))
def test_outer_watchdog_reports_failure_without_a_success_claim(tmp_path, monkeypatch, seconds):
    args = _args(tmp_path)
    output = tmp_path / "report.json"

    def timeout(command, **options):
        assert options["timeout"] == seconds + 10
        assert options["env"]["PYTHONDONTWRITEBYTECODE"] == "1"
        raise subprocess.TimeoutExpired(command, options["timeout"])

    monkeypatch.setattr(bench.subprocess, "run", timeout)
    assert bench.main(["--image", str(args.image), "--journey", str(args.journey),
                       "--mode", "simulator", "--timeout", str(seconds), "--output", str(output)]) == 1
    report = json.loads(output.read_text())
    assert not report["complete"]
    assert "watchdog" in report["error"]
    assert report["timeout_seconds"] == seconds + 10
