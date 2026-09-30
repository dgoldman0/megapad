#!/usr/bin/env python3
"""Deprecated script entry point; use ``megapad.py --mode emulator``.

Kept temporarily for external launchers that have not migrated.
The implementation and programmatic interfaces live in ``emulator.server``.
"""

from emulator.server import main


if __name__ == "__main__":
    raise SystemExit(main())
