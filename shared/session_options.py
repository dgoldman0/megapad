"""Common command-line policy decoders for session launchers."""

import argparse
import json

from rich_terminal.retained_model import RetainedPolicy
from shared.session import RichTerminalSessionPolicy


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
