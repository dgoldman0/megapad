"""Focused construction coverage for the semantic simulator server CLI."""

from __future__ import annotations

import pytest

from simulator.memory import AddressClass
from simulator_server import build_argument_parser, prepare_server
from tests.simulator.test_image_bootstrap import _boot_image


def _region_sizes(prepared) -> dict[AddressClass, int]:
    return {
        region.kind: region.size
        for region in prepared.preparation.runtime.memory.regions
    }


def test_server_cli_builds_the_shared_semantic_facade(tmp_path) -> None:
    image = tmp_path / "desktop-simulator.img"
    image.write_bytes(_boot_image())
    args = build_argument_parser().parse_args(
        [
            "--storage",
            str(image),
            "--socket",
            str(tmp_path / "simulator.sock"),
            "--ram-kib",
            "768",
            "--ext-mem-mib",
            "5",
            "--vram-mib",
            "2",
            "--cols",
            "96",
            "--rows",
            "32",
            "--paused",
        ]
    )

    prepared = prepare_server(args)
    try:
        assert prepared.preparation.boot_filename == b"kdos.f"
        assert prepared.machine.semantic_session.entry == (
            prepared.preparation.root_xt
        )
        assert prepared.machine.paused
        assert prepared.server.machine is prepared.machine
        assert prepared.server.socket_path == str(tmp_path / "simulator.sock")
        sizes = _region_sizes(prepared)
        assert sizes[AddressClass.BANK0] == 768 << 10
        assert sizes[AddressClass.EXTERNAL] == 5 << 20
        assert sizes[AddressClass.VRAM] == 2 << 20
        assert sizes[AddressClass.HBW] == 3 << 20
    finally:
        prepared.machine.stop()


def test_server_cli_rejects_emulator_only_arguments_and_missing_images(
    tmp_path,
) -> None:
    parser = build_argument_parser()
    for option in ("--bios", "--nic-tap", "--audio", "--host-profile"):
        with pytest.raises(SystemExit):
            parser.parse_args(
                ["--storage", str(tmp_path / "missing.img"), option]
            )

    args = parser.parse_args(
        ["--storage", str(tmp_path / "missing.img")]
    )
    with pytest.raises(ValueError, match="storage image does not exist"):
        prepare_server(args)
