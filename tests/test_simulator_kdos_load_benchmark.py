from pathlib import Path

import pytest

import bench_simulator_kdos_load as benchmark


def test_parser_selects_the_canonical_semantic_kdos_shape() -> None:
    args = benchmark.build_parser().parse_args([])

    assert benchmark.SCHEMA_VERSION == 1
    assert args.runtime_root == benchmark.ROOT
    assert args.ram_kib == 1_024
    assert args.ext_mem_mib == 128
    assert args.hbw_mib == 3
    assert args.vram_mib == 4
    assert (args.cols, args.rows) == (280, 84)


@pytest.mark.parametrize(
    "option",
    ("--ram-kib", "--ext-mem-mib", "--hbw-mib", "--vram-mib", "--cols", "--rows"),
)
def test_parser_rejects_nonpositive_geometry(option: str) -> None:
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, "0"])


def test_exact_kdos_payload_matches_the_shared_mp64fs_packer() -> None:
    source = benchmark._verified_source(benchmark.ROOT)
    packed, submitted = benchmark._packed_lines(source)

    assert len(source) == benchmark.KDOS_BYTES
    assert len(submitted) == benchmark.SUBMITTED_LINES
    assert sum(len(line) for _number, line in submitted) == (
        benchmark.SUBMITTED_PAYLOAD_BYTES
    )
    assert len(packed) == benchmark.PACKED_KDOS_BYTES
    assert packed.endswith(b"JIT-OFF\nCR\n")


def test_runtime_root_cannot_silently_select_another_checkout(
    tmp_path: Path,
) -> None:
    with pytest.raises(RuntimeError, match="measured checkout"):
        benchmark._activate_runtime(tmp_path)


def test_marker_fixture_transcript_binds_the_autoexec_boundary() -> None:
    assert benchmark.EXPECTED_STARTUP_TRANSCRIPT.startswith(
        benchmark.STARTUP_BANNER + b" MP64FS loaded\r\n"
    )
    assert b" Running autoexec.f...\r\n" in benchmark.EXPECTED_STARTUP_TRANSCRIPT
    assert benchmark.bios_bench.COMPLETION_MARKER.encode("ascii") in (
        benchmark.EXPECTED_STARTUP_TRANSCRIPT
    )
