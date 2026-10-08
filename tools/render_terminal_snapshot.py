#!/usr/bin/env python3
"""Render an existing display offer with the production terminal compositor.

The input is a full display-offer JSON object or an array of full/delta offers,
optionally gzip-compressed. No simulator, display window, or guest is started.
"""

from __future__ import annotations

import argparse
import gzip
import json
import os
from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))


def positive_int(value: str) -> int:
    number = int(value)
    if number <= 0:
        raise argparse.ArgumentTypeError("must be a positive integer")
    return number


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("snapshot", type=Path, help="display-offer JSON or JSON.gz")
    parser.add_argument("output", type=Path, help="output PNG path")
    parser.add_argument("--appearance", choices=("reference", "flowing"), default="flowing")
    parser.add_argument(
        "--frame", type=int, default=-1,
        help="zero-based frame in a sequence; negative indexes count from the end (default: -1)",
    )
    parser.add_argument("--font", type=Path, help="cell font file (default: system monospace)")
    parser.add_argument("--font-size", type=positive_int, default=18)
    parser.add_argument(
        "--control-font", type=Path,
        help="control font file (default: --font when supplied, otherwise system sans)",
    )
    parser.add_argument("--control-font-size", type=positive_int, default=16)
    parser.add_argument(
        "--fallback-font", type=Path, action="append", default=[],
        help="explicit fallback font; repeat to fix the fallback order instead of discovery",
    )
    parser.add_argument(
        "--no-font-discovery", action="store_true",
        help="disable discovery of fallback and styled faces for reproducible font selection",
    )
    parser.add_argument("--hide-cursor", action="store_true")
    return parser


def load_offer(path: Path, frame_index: int):
    from shared_session import display_offer_from_wire

    encoded = path.read_bytes()
    if encoded.startswith(b"\x1f\x8b"):
        encoded = gzip.decompress(encoded)
    document = json.loads(encoded)
    frames = document if isinstance(document, list) else [document]
    if not frames:
        raise ValueError("snapshot sequence is empty")
    selected = frame_index if frame_index >= 0 else len(frames) + frame_index
    if not 0 <= selected < len(frames):
        raise ValueError(f"frame {frame_index} is outside the {len(frames)}-frame sequence")

    # A delta may name an earlier full offer instead of the immediately prior
    # frame. Keep decoded bases until the requested frame has been reconstructed.
    decoded = {}
    offer = None
    for index, wire in enumerate(frames[:selected + 1]):
        if not isinstance(wire, dict):
            raise ValueError(f"frame {index} must be a display-offer JSON object")
        base = decoded.get(wire.get("base_offer_id"))
        try:
            offer = display_offer_from_wire(wire, base)
        except (TypeError, ValueError) as exc:
            raise ValueError(f"invalid display offer at frame {index}: {exc}") from exc
        if offer.offer_id in decoded:
            raise ValueError(f"duplicate offer id {offer.offer_id} at frame {index}")
        decoded[offer.offer_id] = offer
    return offer, selected, len(frames)


def render(args) -> dict:
    # Font initialization and software surfaces suffice; never open an SDL
    # window or claim/present a session's display offer.
    os.environ.setdefault("SDL_VIDEODRIVER", "dummy")
    os.environ.setdefault("SDL_AUDIODRIVER", "dummy")
    os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
    import pygame

    from display import VirtualTerminal
    from rich_terminal.appearance import get_appearance
    from rich_terminal.font_set import FontSet, discover_fallback_fonts, font_sha256
    from session_viewer import apply_terminal_snapshot, compose_terminal_frame_result

    if args.output.suffix.lower() != ".png":
        raise ValueError("output must have a .png extension")
    if args.output.resolve() == args.snapshot.resolve():
        raise ValueError("output must not replace the input snapshot")
    for path in (args.font, args.control_font, *args.fallback_font):
        if path is not None and not path.is_file():
            raise ValueError(f"font file does not exist: {path}")

    offer, selected, frame_count = load_offer(args.snapshot, args.frame)
    if offer.retained.resources:
        raise ValueError(
            "snapshot references external image resources; standalone offers contain "
            "their manifests, not their image bytes, so this tool cannot reproduce them"
        )
    fallbacks = tuple(args.fallback_font)
    if not fallbacks and not args.no_font_discovery:
        fallbacks = discover_fallback_fonts()
    styles = {} if args.no_font_discovery else None
    pygame.font.init()
    try:
        font = FontSet(pygame, args.font, args.font_size, fallbacks, styles=styles)
        control_font = FontSet(
            pygame, args.control_font or args.font, args.control_font_size,
            fallbacks, cells=False, styles=styles,
        )
        terminal = VirtualTerminal(cols=offer.cell.cols, rows=offer.cell.rows)
        apply_terminal_snapshot(terminal, offer.cell)
        frame = compose_terminal_frame_result(
            pygame, terminal, font, font.cell_width, font.cell_height,
            retained_plane=offer.retained, show_cursor=not args.hide_cursor,
            glyph_cache={}, control_font=control_font,
            appearance=get_appearance(args.appearance),
        )
        args.output.parent.mkdir(parents=True, exist_ok=True)
        pygame.image.save(frame.surface, str(args.output))

        def font_record(font_set):
            paths = list(dict.fromkeys(
                [face.path for face in font_set.faces] + list(font_set.style_paths.values())
            ))
            return [{"path": str(path.resolve()), "sha256": font_sha256(path)} for path in paths]

        return {
            "snapshot": str(args.snapshot.resolve()),
            "frame": selected,
            "frame_count": frame_count,
            "offer_id": offer.offer_id,
            "appearance": args.appearance,
            "cells": [terminal.cols, terminal.rows],
            "cell_pixels": [font.cell_width, font.cell_height],
            "image_pixels": list(frame.surface.get_size()),
            "hit_entries": len(frame.hit_entries),
            "cell_fonts": font_record(font),
            "control_fonts": font_record(control_font),
            "output": str(args.output.resolve()),
        }
    finally:
        pygame.quit()


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    try:
        result = render(args)
    except (OSError, ValueError, TypeError, ImportError) as exc:
        parser.exit(2, f"{parser.prog}: {exc}\n")
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
