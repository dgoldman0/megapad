#!/usr/bin/env python3
"""Live HTTP/HTTPS diagnostic test over TAP.

Boots BIOS + KDOS + tools.f via UART, configures real TAP networking,
then runs SCROLL-GET for both http:// and https:// against example.com.
Prints full UART output for diagnosis.

Usage:
    python test_live.py            # runs both HTTP and HTTPS tests
    python test_live.py http       # only HTTP
    python test_live.py https      # only HTTPS

Requires: mp64tap0 TAP device configured (10.64.0.1/24 on host side).
"""
from __future__ import annotations

import os
import sys
import time

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "tests"))

from asm import assemble
from system import MegapadSystem
from nic_backends import TAPBackend, tap_available

TAP_NAME = os.environ.get("MP64_TAP", "mp64tap0")
HOST_IP  = os.environ.get("MP64_HOST_IP", "10.64.0.1")
EMU_IP   = os.environ.get("MP64_EMU_IP", "10.64.0.2")

BIOS_PATH  = os.path.join(PROJECT_ROOT, "bios.asm")
KDOS_PATH  = os.path.join(PROJECT_ROOT, "kdos.f")
TOOLS_PATH = os.path.join(PROJECT_ROOT, "tools.f")


def _load_bios():
    with open(BIOS_PATH) as f:
        return assemble(f.read())


def _load_forth_lines(path):
    with open(path) as f:
        lines = []
        for line in f.read().splitlines():
            s = line.strip()
            if not s or s.startswith('\\'):
                continue
            lines.append(line)
        return lines


def _next_line_chunk(data: bytes, pos: int) -> bytes:
    nl = data.find(b'\n', pos)
    if nl == -1:
        return data[pos:]
    return data[pos:nl + 1]


def capture_uart(sys_obj):
    buf = []
    sys_obj.uart.on_tx = lambda b: buf.append(b)
    return buf


def uart_text(buf):
    return "".join(
        chr(b) if (0x20 <= b < 0x7F or b in (10, 13, 9)) else ""
        for b in buf
    )


def save_cpu_state(cpu):
    return {
        'pc': cpu.pc,
        'regs': list(cpu.regs),
        'psel': cpu.psel, 'xsel': cpu.xsel, 'spsel': cpu.spsel,
        'flag_z': cpu.flag_z, 'flag_c': cpu.flag_c,
        'flag_n': cpu.flag_n, 'flag_v': cpu.flag_v,
        'flag_p': cpu.flag_p, 'flag_g': cpu.flag_g,
        'flag_i': cpu.flag_i, 'flag_s': cpu.flag_s,
        'd_reg': cpu.d_reg, 'q_out': cpu.q_out, 't_reg': cpu.t_reg,
        'ivt_base': cpu.ivt_base, 'ivec_id': cpu.ivec_id,
        'trap_addr': cpu.trap_addr,
        'halted': cpu.halted, 'idle': cpu.idle,
        'cycle_count': cpu.cycle_count,
        '_ext_modifier': cpu._ext_modifier,
    }


def restore_cpu_state(cpu, state):
    cpu.regs[:] = state['regs']
    for k in ('psel', 'xsel', 'spsel',
               'flag_z', 'flag_c', 'flag_n', 'flag_v',
               'flag_p', 'flag_g', 'flag_i', 'flag_s',
               'd_reg', 'q_out', 't_reg',
               'ivt_base', 'ivec_id', 'trap_addr',
               'halted', 'idle', 'cycle_count', '_ext_modifier'):
        setattr(cpu, k, state[k])


def build_snapshot():
    """Boot BIOS + load KDOS + tools.f via UART.  Returns (mem_bytes, cpu_state)."""
    print("[*] Building snapshot: BIOS + KDOS + tools.f ...")
    bios_code = _load_bios()
    kdos_lines = _load_forth_lines(KDOS_PATH)
    tools_lines = _load_forth_lines(TOOLS_PATH)

    sys_obj = MegapadSystem(ram_size=1024 * 1024)
    buf = capture_uart(sys_obj)
    sys_obj.load_binary(0, bios_code)
    sys_obj.boot()

    all_lines = kdos_lines + tools_lines
    payload = "\n".join(all_lines) + "\n"
    data = payload.encode()
    pos = 0
    steps = 0
    max_steps = 600_000_000  # generous — KDOS + tools.f is ~11k lines

    while steps < max_steps:
        if sys_obj.cpu.halted:
            break
        if sys_obj.cpu.idle and not sys_obj.uart.has_rx_data:
            if pos < len(data):
                chunk = _next_line_chunk(data, pos)
                sys_obj.uart.inject_input(chunk)
                pos += len(chunk)
            else:
                break
            continue
        batch = sys_obj.run_batch(min(100_000, max_steps - steps))
        steps += max(batch, 1)

    text = uart_text(buf)
    # Check for compilation errors
    if "?" in text and ("Undefined" in text or "?" in text.split('\n')[-5:]):
        last_lines = text.strip().split('\n')[-20:]
        print("[!] Possible compilation errors in snapshot:")
        for ln in last_lines:
            print(f"    {ln}")

    snapshot = (bytes(sys_obj.cpu.mem), save_cpu_state(sys_obj.cpu))
    print(f"[*] Snapshot ready.  {steps:,} steps, {len(data):,} bytes injected.")
    return snapshot


def run_with_tap(snapshot, commands, label="test", max_steps=800_000_000):
    """Restore snapshot, attach TAP, configure IP, run commands."""
    print(f"\n{'='*60}")
    print(f"[*] Running: {label}")
    print(f"{'='*60}")

    mem_bytes, cpu_state = snapshot
    backend = TAPBackend(tap_name=TAP_NAME)
    sys_obj = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend)
    buf = capture_uart(sys_obj)

    # Restore memory + CPU state
    sys_obj.cpu.mem[:len(mem_bytes)] = mem_bytes
    restore_cpu_state(sys_obj.cpu, cpu_state)

    # IP config + user commands
    parts = EMU_IP.split(".")
    gw = HOST_IP.split(".")
    ip_lines = [
        f"{parts[0]} {parts[1]} {parts[2]} {parts[3]} IP-SET",
        f"{gw[0]} {gw[1]} {gw[2]} {gw[3]} GW-IP IP!",
        "255 255 255 0 NET-MASK IP!",
        "8 8 8 8 DNS-SERVER-IP IP!",
    ]

    all_lines = ip_lines + commands
    payload = "\n".join(all_lines) + "\nBYE\n"
    data = payload.encode()
    pos = 0
    steps = 0
    idle_polls = 0
    max_idle_polls = 100   # ~2 seconds of idle polling

    t0 = time.time()
    try:
        while steps < max_steps:
            if sys_obj.cpu.halted:
                break
            if sys_obj.cpu.idle:
                # ALWAYS sleep when CPU is idle to let TAP thread
                # deliver frames.  Without this, the tight loop between
                # run_batch calls starves the GIL and the TAP thread
                # can never append to rx_queue.
                if not sys_obj.uart.has_rx_data and pos < len(data):
                    chunk = _next_line_chunk(data, pos)
                    sys_obj.uart.inject_input(chunk)
                    pos += len(chunk)
                    idle_polls = 0
                elif sys_obj.nic.rx_queue:
                    idle_polls = 0
                elif idle_polls < max_idle_polls:
                    idle_polls += 1
                else:
                    break
                time.sleep(0.02)
                sys_obj.cpu.idle = False
                continue
            idle_polls = 0
            batch = sys_obj.run_batch(min(100_000, max_steps - steps))
            steps += max(batch, 1)
    finally:
        sys_obj.nic.stop()

    elapsed = time.time() - t0
    text = uart_text(buf)

    print(f"[*] Completed in {elapsed:.1f}s, {steps:,} steps")
    print(f"[*] UART output ({len(text)} chars):")
    print("-" * 60)
    # Print last 2000 chars (most relevant)
    if len(text) > 2000:
        print(f"  ... ({len(text) - 2000} chars truncated) ...")
    print(text[-2000:])
    print("-" * 60)

    return text


def main():
    if not tap_available(TAP_NAME):
        print(f"[!] TAP device '{TAP_NAME}' not available.")
        print("    Set up with: sudo ip tuntap add dev mp64tap0 mode tap user $USER")
        print("    Then: sudo ip addr add 10.64.0.1/24 dev mp64tap0 && sudo ip link set mp64tap0 up")
        sys.exit(1)

    mode = sys.argv[1] if len(sys.argv) > 1 else "both"

    snapshot = build_snapshot()

    if mode in ("https", "both"):
        text = run_with_tap(snapshot, [
            'SCROLL-GET http://example.com',
        ], label="HTTP: SCROLL-GET http://example.com",
           max_steps=800_000_000)

        if "Example Domain" in text or "bytes in SCROLL-BUF" in text:
            print("[OK] HTTP test PASSED")
        else:
            print("[FAIL] HTTP test — no expected content found")

    if mode in ("https", "both"):
        # Minimal diagnostic: just try TLS-CONNECT
        text = run_with_tap(snapshot, [
            '." [1]START " CR',
            '_SC-HOST-LEN @ 0 _SC-HOST-LEN !',
            'S" example.com" DUP _SC-HOST-LEN ! _SC-HOST SWAP CMOVE',
            '_SC-HOST _SC-HOST-LEN @ DNS-RESOLVE DUP 0= IF ." [DNS-FAIL]" CR ELSE ." [2]DNS-OK ip=" DUP . CR _SC-IP ! THEN',
            '_SC-HOST-LEN @ 63 MIN DUP TLS-SNI-LEN ! _SC-HOST TLS-SNI-HOST ROT CMOVE',
            '." [3]TLS-CONNECT " CR _SC-IP @ 443 12345 TLS-CONNECT DUP ." [4]TLS-RESULT=" . CR',
            'DUP 0<> IF ." [5]TLS-OK " TLS-CLOSE ELSE DROP ." [5]TLS-FAIL" THEN CR',
        ], label="HTTPS diagnostic: TLS-CONNECT only",
           max_steps=100_000_000)

        if "[4]TLS-RESULT=" in text:
            print("[INFO] TLS-CONNECT returned a result")
        elif "[3]TLS-CONNECT" in text:
            print("[INFO] Hung inside TLS-CONNECT")
        elif "[2]DNS-OK" in text:
            print("[INFO] DNS OK but hung before TLS-CONNECT")
        elif "[DNS-FAIL]" in text:
            print("[INFO] DNS failed")
        elif "[1]START" in text:
            print("[INFO] Started but hung early")

    print("\n[*] Done.")


if __name__ == "__main__":
    main()
