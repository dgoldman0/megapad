#!/usr/bin/env python3
"""Deprecated script entry point; use ``megapad.py --mode simulator``.

Kept temporarily for external launchers that have not migrated.
The implementation and programmatic interfaces live in ``simulator.server``.
"""

from simulator.server import main


if __name__ == "__main__":
    raise SystemExit(main())
