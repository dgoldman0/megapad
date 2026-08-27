from __future__ import annotations

import hashlib

import bench_guest_jit_source_load as benchmark


def test_guest_jit_source_fixture_is_deterministic_and_external_memory_shaped():
    source = benchmark.synthetic_source(
        benchmark.DEFAULT_DEFINITIONS,
        benchmark.DEFAULT_ITERATIONS,
    )
    lines = source.splitlines()

    assert len(source) == 221_303
    assert hashlib.sha256(source).hexdigest() == (
        "f9cabe6b9d9538fbf23320a60269c1161608e5540d615f72318cd85e36bdaab1"
    )
    assert lines[0] == b"CREATE GJIT-CELL 0 ,"
    assert lines[1].startswith(
        b": GJIT-000000 OVER XOR NIP INVERT NEGATE SWAP OVER"
    )
    assert lines[-4:] == [
        b": GJIT-TOUCH GJIT-CELL @ 1+ GJIT-CELL ! ;",
        b": GJIT-SCALAR 123 INVERT NEGATE 7 XOR DROP ;",
        b": GJIT-RUN",
        b"    2000000 0 DO GJIT-TOUCH GJIT-SCALAR LOOP ;",
    ]
    assert max(map(len, lines)) == 149
    assert sum(line.startswith(b": GJIT-") for line in lines) == 2_051
    assert sum(line.startswith(b"VARIABLE GJIT-V-") for line in lines) == 1_024


def test_guest_jit_source_timing_boundary_fixes_internal_jit_policy():
    autoexec = benchmark.synthetic_autoexec().decode("ascii").splitlines()

    jit = autoexec.index("JIT-ON JIT-RESET")
    start = autoexec.index(f'." {benchmark.START_MARKER}" CR')
    pause = autoexec.index("KEY DROP")
    load = autoexec.index("REQUIRE guest-jit-source.f")
    compiled = autoexec.index(f'." {benchmark.COMPILED_MARKER}" CR')
    run = autoexec.index("GJIT-RUN")
    complete = autoexec.index(
        f'." {benchmark.COMPLETION_PREFIX}" GJIT-CELL @ . CR'
    )

    assert jit < start < pause < load < compiled < run < complete
    assert hashlib.sha256(benchmark.synthetic_autoexec()).hexdigest() == (
        "e9eb5e5d8ccef81a0aef3fa7f52077b8d94d088cb9f2bb752f9d8b7dbc061998"
    )


def test_guest_jit_source_parser_keeps_host_profile_opt_in():
    defaults = benchmark.build_parser().parse_args([])
    profiled = benchmark.build_parser().parse_args(["--host-profile"])

    assert defaults.definitions == 2_048
    assert defaults.iterations == 2_000_000
    assert not defaults.host_profile
    assert profiled.host_profile
