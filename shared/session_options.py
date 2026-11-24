"""Common command-line policy decoders for session launchers."""

import argparse
import json
import os
import sys

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


def add_network_options(parser) -> None:
    """Add --nic, which attaches a network card, and --local-access."""

    parser.add_argument(
        "--nic",
        metavar="SPEC",
        help=(
            "attach a network card: slirp, the user-mode network, which needs "
            "no administrator rights, or tap:IFNAME, a TAP device set up "
            "beforehand"
        ),
    )
    parser.add_argument(
        "--local-access",
        action="store_true",
        help=(
            "let the slirp network reach this computer and its local "
            "network, which it refuses otherwise"
        ),
    )


def report_network(backend) -> None:
    """Say which network a server's machine has (a nic_backends backend, or
    None), and warn at every start when it may reach this computer and its
    local network."""

    print("[shared] network: " + ("none" if backend is None else backend.name), flush=True)
    if getattr(backend, "local_access", False):
        from nic_backends import LOCAL_ACCESS_WARNING

        print(f"[shared] warning: {LOCAL_ACCESS_WARNING}", file=sys.stderr, flush=True)


def protect_process_memory() -> bool:
    """Turn off core dumps of this process and other processes' access to
    its memory, as a server without debugging access does, since the
    machine's memory can hold a user's keys and unsealed tokens (prctl
    PR_SET_DUMPABLE 0).  Whether it took effect: only Linux has the call."""

    if not sys.platform.startswith("linux"):
        return False
    import ctypes

    libc = ctypes.CDLL(None, use_errno=True)
    pr_set_dumpable = 4
    if libc.prctl(pr_set_dumpable, 0, 0, 0, 0) != 0:
        number = ctypes.get_errno()
        raise OSError(number, os.strerror(number))
    return True


def report_debugging(debug_access: bool, protected: bool) -> None:
    """Say at start whether clients may read and steer the machine, and
    whether its memory is closed to other processes."""

    if debug_access:
        state = "on"
    elif protected:
        state = "off; memory closed to other processes and core dumps"
    else:
        state = "off"
    print(f"[shared] debugging access: {state}", flush=True)


def add_debug_option(parser) -> None:
    """Add --debug-access, which opens shared_session.DEBUG_METHODS to the
    server's clients."""

    parser.add_argument(
        "--debug-access",
        action="store_true",
        help=(
            "let clients read the machine's memory and dictionary, control "
            "its execution, run profiles and write captures over the socket; "
            "off by default, since memory can hold keys and tokens"
        ),
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
