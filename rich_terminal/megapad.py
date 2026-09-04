"""Temporary import alias for the MegaPad-specific terminal attachment."""

from importlib import import_module as _import_module
import sys as _sys


_sys.modules[__name__] = _import_module("emulator.rich_terminal_host")
