"""Common command-line policy decoders for session launchers."""

import argparse
import json
import os

from rich_terminal import LinkPace
from rich_terminal.retained_model import RetainedPolicy
from shared.session import RichTerminalSessionPolicy
from shared.storage import BOOT_UNIT, STORAGE_UNIT_TABLE


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


def add_storage_options(parser, *, path_type, boot_required: bool = False) -> None:
    """Add the option that attaches an image to each storage unit.

    PATH_TYPE converts each option's value.  A launcher that cannot run
    without a boot disk passes BOOT_REQUIRED.
    """
    for unit in STORAGE_UNIT_TABLE:
        protection = "write-protected " if unit.write_protected else ""
        parser.add_argument(
            unit.option,
            type=path_type,
            default=None,
            required=boot_required and unit is BOOT_UNIT,
            help=f"{unit.disk} disk image, attached {protection}"
                 f"to storage unit {unit.number}",
        )


def storage_image_arguments(args) -> dict:
    """The emulator's image arguments from the options of
    :func:`add_storage_options`."""
    return {
        unit.image_name: getattr(args, unit.host_name)
        for unit in STORAGE_UNIT_TABLE
    }


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


def link_pace(value: str) -> LinkPace:
    """BYTES_PER_SECOND:FRAME_US, a paced client-to-terminal link."""

    rate, separator, frame = value.partition(":")
    try:
        if not separator:
            raise ValueError("expected BYTES_PER_SECOND:FRAME_US")
        return LinkPace(int(rate, 10), int(frame, 10))
    except (TypeError, ValueError, OverflowError) as exc:
        raise argparse.ArgumentTypeError(f"invalid terminal link pace: {exc}") from exc


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
