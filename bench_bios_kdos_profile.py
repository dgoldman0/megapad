#!/usr/bin/env python3
"""Profile BIOS and KDOS: ROM size, RAM usage, cycles, dictionary, HERE."""

import os, sys, time
sys.path.insert(0, os.path.dirname(__file__))

from asm import assemble
from system import MegapadSystem
from accel_wrapper import Megapad64, HaltError

BIOS_PATH = os.path.join(os.path.dirname(__file__), "bios.asm")
KDOS_PATH = os.path.join(os.path.dirname(__file__), "kdos.f")

def make_system(ram_kib=1024, ext_mem_mib=16):
    return MegapadSystem(ram_size=ram_kib*1024, ext_mem_size=ext_mem_mib*(1<<20))

def capture_uart(sys):
    buf = []
    sys.uart.on_tx = lambda b: buf.append(b)
    return buf

def uart_text(buf):
    return "".join(chr(b) if (0x20<=b<0x7F or b in (10,13,9)) else "" for b in buf)

def _next_line_chunk(data, pos):
    nl = data.find(b'\n', pos)
    if nl == -1:
        return data[pos:]
    return data[pos:nl+1]

def feed_and_run(sys_obj, buf, lines, max_steps=400_000_000):
    payload = "\n".join(lines) + "\n"
    data = payload.encode()
    pos = 0; total = 0
    while total < max_steps:
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
        batch = sys_obj.run_batch(min(100_000, max_steps - total))
        total += max(batch, 1)
    return total

def read_var(cpu, labels, name):
    """Read an 8-byte LE value at label address."""
    addr = labels[name]
    val = 0
    for i in range(8):
        val |= cpu.mem[addr + i] << (8 * i)
    return val

def ram_high_water(mem, ram_size):
    """Find last non-zero byte in RAM."""
    for i in range(ram_size - 1, -1, -1):
        if mem[i] != 0:
            return i + 1
    return 0

def fmt_bytes(n):
    if n < 1024:
        return f"{n} B"
    elif n < 1024*1024:
        return f"{n/1024:.1f} KiB"
    else:
        return f"{n/(1024*1024):.2f} MiB"

def fmt_cycles(n):
    if n < 1_000_000:
        return f"{n:,}"
    else:
        return f"{n/1_000_000:.2f}M"

# ═══════════════════════════════════════════════════════════════
#  1. Assemble BIOS
# ═══════════════════════════════════════════════════════════════
print("=" * 70)
print("  BIOS/KDOS Memory & Performance Profile")
print("=" * 70)

labels = {}
with open(BIOS_PATH) as f:
    bios_src = f.read()

t0 = time.time()
bios_code = assemble(bios_src, labels_out=labels)
t_asm = time.time() - t0

bios_rom_size = len(bios_code)
# Find key sections
dict_free = labels.get('dict_free', 0)
tib_buffer = labels.get('tib_buffer', 0)
ivt_table = labels.get('ivt_table', 0)
str_banner = labels.get('str_banner', 0)

print(f"\n{'─'*50}")
print(f"  BIOS ROM Image")
print(f"{'─'*50}")
print(f"  Total ROM size:      {fmt_bytes(bios_rom_size)} ({bios_rom_size} bytes)")
print(f"  Assembly time:       {t_asm:.3f}s")
print(f"  Code+data end:       0x{bios_rom_size:X}")
if dict_free:
    print(f"  dict_free label:     0x{dict_free:X}")
if str_banner:
    print(f"  String constants at: 0x{str_banner:X}")
if ivt_table:
    print(f"  IVT table at:        0x{ivt_table:X}")

# Count labels for reference
code_labels = [k for k in labels if not k.startswith(('var_', 'str_', 'd_', 'tib_', 'squote_'))]
var_labels = [k for k in labels if k.startswith('var_')]
dict_labels = [k for k in labels if k.startswith('d_')]
print(f"  Code/helper labels:  {len(code_labels)}")
print(f"  Variable labels:     {len(var_labels)}")
print(f"  Dictionary entries:  {len(dict_labels)}")

# ═══════════════════════════════════════════════════════════════
#  2. Boot BIOS only — measure cycles to idle
# ═══════════════════════════════════════════════════════════════
print(f"\n{'─'*50}")
print(f"  BIOS Boot (to idle prompt)")
print(f"{'─'*50}")

sys_obj = make_system()
buf = capture_uart(sys_obj)
sys_obj.load_binary(0, bios_code)
sys_obj.boot()

t0 = time.time()
boot_steps = 0
for _ in range(10_000_000):
    if sys_obj.cpu.halted or sys_obj.cpu.idle:
        break
    try:
        sys_obj.step()
        boot_steps += 1
    except HaltError:
        break
t_boot = time.time() - t0

bios_cycles = sys_obj.cpu.cycle_count
here_after_boot = read_var(sys_obj.cpu, labels, 'var_here')
latest_after_boot = read_var(sys_obj.cpu, labels, 'var_latest')
ram_hw = ram_high_water(sys_obj.cpu.mem, 1024*1024)

print(f"  Cycles to idle:      {fmt_cycles(bios_cycles)}")
print(f"  Wall time:           {t_boot:.3f}s")
print(f"  HERE after boot:     0x{here_after_boot:X} ({fmt_bytes(here_after_boot)})")
print(f"  LATEST after boot:   0x{latest_after_boot:X}")
print(f"  RAM high-water:      0x{ram_hw:X} ({fmt_bytes(ram_hw)})")
print(f"  UART banner:")
banner = uart_text(buf).strip()
for line in banner.split('\n'):
    print(f"    {line}")

# ═══════════════════════════════════════════════════════════════
#  3. Load KDOS (JIT OFF) — measure cycles, HERE, dictionary
# ═══════════════════════════════════════════════════════════════
def load_kdos(jit_on=False):
    """Boot BIOS, optionally enable JIT, load KDOS, return stats."""
    sys_obj = make_system()
    buf = capture_uart(sys_obj)
    sys_obj.load_binary(0, bios_code)
    sys_obj.boot()
    # Boot to idle
    for _ in range(10_000_000):
        if sys_obj.cpu.halted or sys_obj.cpu.idle:
            break
        try:
            sys_obj.step()
        except HaltError:
            break

    cycles_at_bios_idle = sys_obj.cpu.cycle_count

    # Prepare KDOS lines
    kdos_lines = []
    with open(KDOS_PATH) as f:
        for line in f.read().splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith('\\'):
                continue
            kdos_lines.append(line)

    # Optionally enable JIT before loading
    preamble = []
    if jit_on:
        preamble = ["JIT-ON"]

    # Append queries for stats after KDOS loads
    postamble = [
        'CR ." [HERE=" HERE . ." ]"',
        'CR ." [JIT=" _JIT-ENABLED @ . ." ]"' if '_JIT-ENABLED' in uart_text(buf) else '',
    ]
    # Filter empties
    postamble = [l for l in postamble if l]

    all_lines = preamble + kdos_lines + postamble

    t0 = time.time()
    steps = feed_and_run(sys_obj, buf, all_lines)
    t_load = time.time() - t0

    cycles_total = sys_obj.cpu.cycle_count
    cycles_kdos = cycles_total - cycles_at_bios_idle
    here_after = read_var(sys_obj.cpu, labels, 'var_here')
    latest_after = read_var(sys_obj.cpu, labels, 'var_latest')
    ram_hw = ram_high_water(sys_obj.cpu.mem, 1024*1024)

    # JIT stats if available
    jit_inlines = read_var(sys_obj.cpu, labels, 'var_jit_inlines')
    jit_folds = read_var(sys_obj.cpu, labels, 'var_jit_folds')
    jit_peepholes = read_var(sys_obj.cpu, labels, 'var_jit_peepholes')
    jit_bytes_saved = read_var(sys_obj.cpu, labels, 'var_jit_bytes_saved')
    jit_enabled = read_var(sys_obj.cpu, labels, 'var_jit_enabled')

    # Dictionary size = HERE - dict_free (user definitions)
    dict_used = here_after - here_after_boot if here_after > here_after_boot else 0

    text = uart_text(buf)

    return {
        'jit_on': jit_on,
        'cycles_kdos': cycles_kdos,
        'cycles_total': cycles_total,
        'wall_time': t_load,
        'here': here_after,
        'latest': latest_after,
        'ram_hw': ram_hw,
        'dict_used': dict_used,
        'jit_inlines': jit_inlines,
        'jit_folds': jit_folds,
        'jit_peepholes': jit_peepholes,
        'jit_bytes_saved': jit_bytes_saved,
        'jit_enabled': jit_enabled,
        'kdos_lines': len(kdos_lines),
        'text': text,
    }

def print_kdos_stats(s, label):
    print(f"\n{'─'*50}")
    print(f"  KDOS Load — {label}")
    print(f"{'─'*50}")
    print(f"  KDOS source lines:   {s['kdos_lines']}")
    print(f"  Cycles (KDOS only):  {fmt_cycles(s['cycles_kdos'])}")
    print(f"  Cycles (total):      {fmt_cycles(s['cycles_total'])}")
    print(f"  Wall time:           {s['wall_time']:.3f}s")
    print(f"  HERE after load:     0x{s['here']:X} ({fmt_bytes(s['here'])})")
    print(f"  Dictionary used:     {fmt_bytes(s['dict_used'])}")
    print(f"  RAM high-water:      0x{s['ram_hw']:X} ({fmt_bytes(s['ram_hw'])})")
    print(f"  JIT enabled var:     {s['jit_enabled']}")
    if s['jit_on']:
        print(f"  JIT inlines:         {s['jit_inlines']}")
        print(f"  JIT folds:           {s['jit_folds']}")
        print(f"  JIT peepholes:       {s['jit_peepholes']}")
        print(f"  JIT bytes saved:     {fmt_bytes(s['jit_bytes_saved'])}")

print("\nLoading KDOS (JIT OFF)...")
stats_off = load_kdos(jit_on=False)
print_kdos_stats(stats_off, "JIT OFF")

print("\nLoading KDOS (JIT ON)...")
stats_on = load_kdos(jit_on=True)
print_kdos_stats(stats_on, "JIT ON")

# ═══════════════════════════════════════════════════════════════
#  4. Comparison
# ═══════════════════════════════════════════════════════════════
print(f"\n{'═'*70}")
print(f"  Comparison: JIT OFF vs JIT ON")
print(f"{'═'*70}")
print(f"  {'':30s} {'JIT OFF':>14s} {'JIT ON':>14s} {'Delta':>14s}")
print(f"  {'─'*72}")

def cmp_row(label, off_val, on_val, fmt_fn=str):
    delta = on_val - off_val
    sign = "+" if delta >= 0 else ""
    print(f"  {label:30s} {fmt_fn(off_val):>14s} {fmt_fn(on_val):>14s} {sign}{fmt_fn(delta):>13s}")

cmp_row("Cycles (KDOS load)", stats_off['cycles_kdos'], stats_on['cycles_kdos'], fmt_cycles)
cmp_row("Wall time (s)", stats_off['wall_time'], stats_on['wall_time'], lambda x: f"{x:.3f}")
cmp_row("HERE", stats_off['here'], stats_on['here'], lambda x: f"0x{x:X}")
cmp_row("Dictionary used", stats_off['dict_used'], stats_on['dict_used'], fmt_bytes)
cmp_row("RAM high-water", stats_off['ram_hw'], stats_on['ram_hw'], fmt_bytes)

if stats_on['jit_bytes_saved']:
    print(f"\n  JIT compilation net effect on dictionary:")
    print(f"    Bytes saved by inlining:  {fmt_bytes(stats_on['jit_bytes_saved'])}")
    print(f"    Dictionary size diff:     {fmt_bytes(stats_on['dict_used'] - stats_off['dict_used'])}")
    if stats_on['dict_used'] < stats_off['dict_used']:
        print(f"    → JIT makes dictionary SMALLER (net savings)")
    else:
        print(f"    → JIT makes dictionary LARGER (inlines expand more than saves)")

print(f"\n  Cycle speedup from JIT-at-load: ", end="")
if stats_on['cycles_kdos'] < stats_off['cycles_kdos']:
    pct = (1 - stats_on['cycles_kdos'] / stats_off['cycles_kdos']) * 100
    print(f"{pct:.1f}% fewer cycles")
else:
    pct = (stats_on['cycles_kdos'] / stats_off['cycles_kdos'] - 1) * 100
    print(f"{pct:.1f}% MORE cycles (JIT compilation overhead)")

print()
