"""Common command-line policy decoders for session launchers."""

import argparse
import json
import os

from rich_terminal.retained_model import RetainedPolicy
from shared.session import RichTerminalSessionPolicy


SEMANTIC_EXECUTOR_ENVIRONMENT = "MEGAFORTH_EXECUTOR"
DEFAULT_PRODUCTION_EXECUTOR = "native"


def configured_production_executor(value: str | None) -> str:
    """Select the explicit executor, environment, then required native.

    This is the production session policy. Embedded semantic runtimes retain
    their Python reference default; only an explicit ``auto`` may fall back.
    Resolving the choice never imports an execution engine or changes the
    caller's environment.
    """
    selected = (os.environ.get(SEMANTIC_EXECUTOR_ENVIRONMENT, DEFAULT_PRODUCTION_EXECUTOR)
                if value is None else value)
    if selected not in ("python", "native", "auto"):
        raise ValueError(
            f"semantic executor ({SEMANTIC_EXECUTOR_ENVIRONMENT} or --executor) "
            "must be python, native, or auto"
        )
    return selected


def rich_terminal_policy(value: str) -> RichTerminalSessionPolicy:
    try:
        payload = json.loads(value)
    except json.JSONDecodeError as exc:
        raise argparse.ArgumentTypeError(
            f"invalid rich-terminal policy JSON: {exc.msg}"
        ) from exc
    if not isinstance(payload, dict):
        raise argparse.ArgumentTypeError(
            "rich-terminal policy JSON must be an object"
        )
    try:
        return RichTerminalSessionPolicy(**payload)
    except (TypeError, ValueError, OverflowError) as exc:
        raise argparse.ArgumentTypeError(
            f"invalid rich-terminal policy: {exc}"
        ) from exc


def retained_policy(value: str) -> RetainedPolicy:
    try:
        payload = json.loads(value)
    except json.JSONDecodeError as exc:
        raise argparse.ArgumentTypeError(
            f"invalid retained policy JSON: {exc.msg}"
        ) from exc
    if not isinstance(payload, dict):
        raise argparse.ArgumentTypeError("retained policy JSON must be an object")
    try:
        return RetainedPolicy(**payload)
    except (TypeError, ValueError, OverflowError) as exc:
        raise argparse.ArgumentTypeError(f"invalid retained policy: {exc}") from exc
