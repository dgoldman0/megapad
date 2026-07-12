#!/usr/bin/env python3
"""Control a running shared MegaPad session."""

from __future__ import annotations

import argparse
import json
import sys
import time

from shared_session import DEFAULT_SOCKET, SessionClient


def print_json(value):
    print(json.dumps(value, indent=2, ensure_ascii=False))


def main() -> int:
    parser = argparse.ArgumentParser(description="Control a shared MegaPad session")
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    commands = parser.add_subparsers(dest="command", required=True)

    commands.add_parser("status")
    commands.add_parser("network")
    forth = commands.add_parser("forth")
    forth.add_argument("names", nargs="+")
    peek = commands.add_parser("peek")
    peek.add_argument("address", type=lambda value: int(value, 0))
    peek.add_argument("count", nargs="?", type=int, default=1)
    commands.add_parser("text")
    raw = commands.add_parser("raw")
    raw.add_argument("--since", type=int, default=0)

    send = commands.add_parser("send")
    send.add_argument("text")
    send.add_argument("--enter", action="store_true")
    key = commands.add_parser("key")
    key.add_argument("key")

    commands.add_parser("pause")
    commands.add_parser("resume")
    step = commands.add_parser("step")
    step.add_argument("count", nargs="?", type=int, default=1)
    reset = commands.add_parser("reset")
    reset.add_argument("--paused", action="store_true")
    resize = commands.add_parser("resize")
    resize.add_argument("cols", type=int)
    resize.add_argument("rows", type=int)

    wait = commands.add_parser("wait-text")
    wait.add_argument("text")
    wait.add_argument("--scope", choices=("raw", "screen"), default="raw")
    wait.add_argument("--timeout", type=float, default=10.0)
    wait.add_argument("--from-now", action="store_true")

    capture = commands.add_parser("capture")
    capture.add_argument("--text")
    capture.add_argument("--json")
    capture.add_argument("--png")
    capture.add_argument("--font")
    capture.add_argument("--font-size", type=int, default=16)

    commands.add_parser("shutdown")
    args = parser.parse_args()

    try:
        with SessionClient(args.socket) as client:
            if args.command == "status":
                print_json(client.request("status"))
            elif args.command == "network":
                print_json(client.request("network"))
            elif args.command == "forth":
                print_json(client.request("forth", names=args.names))
            elif args.command == "peek":
                print_json(
                    client.request("peek", address=args.address, count=args.count)
                )
            elif args.command == "text":
                print(client.request("text")["text"])
            elif args.command == "raw":
                result = client.request("raw", since=args.since)
                print(result["text"], end="")
            elif args.command == "send":
                text = args.text + ("\n" if args.enter else "")
                print_json(client.request("send_text", text=text))
            elif args.command == "key":
                print_json(client.request("send_key", key=args.key))
            elif args.command == "pause":
                print_json(client.request("pause"))
            elif args.command == "resume":
                print_json(client.request("resume"))
            elif args.command == "step":
                print_json(client.request("step", count=args.count))
            elif args.command == "reset":
                print_json(client.request("reset", paused=args.paused))
            elif args.command == "resize":
                print_json(client.request("resize", cols=args.cols, rows=args.rows))
            elif args.command == "wait-text":
                return wait_for_text(client, args)
            elif args.command == "capture":
                print_json(client.request(
                    "capture",
                    text=args.text,
                    json=args.json,
                    png=args.png,
                    font=args.font,
                    font_size=args.font_size,
                ))
            elif args.command == "shutdown":
                print_json(client.request("shutdown"))
    except (OSError, ConnectionError, RuntimeError, ValueError) as exc:
        print(f"session control failed: {exc}", file=sys.stderr)
        return 2
    return 0


def wait_for_text(client: SessionClient, args) -> int:
    deadline = time.monotonic() + args.timeout
    accumulated = ""
    offset = 0
    if args.scope == "raw" and args.from_now:
        offset = client.request("status")["raw_bytes"]

    while time.monotonic() < deadline:
        if args.scope == "screen":
            accumulated = client.request("text")["text"]
        else:
            result = client.request("raw", since=offset)
            offset = result["offset"]
            accumulated += result["text"]
        if args.text in accumulated:
            print_json({"matched": True, "text": args.text, "scope": args.scope})
            return 0
        time.sleep(0.02)

    print_json({
        "matched": False,
        "text": args.text,
        "scope": args.scope,
        "recent": accumulated[-500:],
    })
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
