#!/usr/bin/env python3
"""Launch a shared MegaPad session using the selected execution mode."""

from __future__ import annotations

import argparse
import importlib
import sys


_SERVER_MODULES = {
    "emulator": "emulator.server",
    "simulator": "simulator.server",
    "hybrid": "hybrid.server",
}


def _argument_parser(*, selector_only: bool = False) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run a shared MegaPad session (default mode: emulator)",
        add_help=not selector_only,
        allow_abbrev=False,
        epilog=(
            "Use --mode MODE --help for mode-specific options. All modes "
            "use session_ctl.py and session_viewer.py. Hybrid mode combines "
            "source execution with declared bounded integer machine routines."
        ),
    )
    parser.add_argument(
        "--mode",
        choices=tuple(_SERVER_MODULES),
        default=None if selector_only else "emulator",
        help="execution mode (default: emulator)",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    # Only mode selection belongs to this layer. Leave every other argument
    # in its original order for the selected server's parser and lifecycle.
    # Help without an explicit mode and invalid modes need no backend imports.
    arguments = list(sys.argv[1:] if argv is None else argv)
    selection, backend_arguments = _argument_parser(
        selector_only=True
    ).parse_known_args(arguments)
    if selection.mode is None and any(
        argument in ("-h", "--help") for argument in backend_arguments
    ):
        _argument_parser().print_help()
        return 0

    mode = selection.mode or "emulator"
    server = importlib.import_module(_SERVER_MODULES[mode])
    return server.main(backend_arguments)


if __name__ == "__main__":
    raise SystemExit(main())
