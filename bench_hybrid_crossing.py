"""Measure the cost of crossings between Forth and hybrid machine routines.

Each case runs a counted Forth loop whose body crosses into machine code once
(or into machine code and back to Forth once) and subtracts the same loop
around the equivalent Forth word, so the figure is the cost the crossing adds
per iteration. Results are host wall time, not MP64 cycles.
"""

from __future__ import annotations

import argparse
import json
import time

from asm import assemble
from hybrid.manifest import CallbackSite, RoutineDeclaration
from hybrid.runtime import HybridRuntime


CALLBACK_SOURCE = """
    mov r12, r3
after_pc:
    addi r12, 0
call:
    call.l r12
    ret.l
stub:
    ret.l
"""


def _callback_code():
    labels = {}
    assemble(CALLBACK_SOURCE, labels_out=labels)
    delta = labels["stub"] - labels["after_pc"]
    labels = {}
    code = bytes(assemble(CALLBACK_SOURCE.replace("addi r12, 0", f"addi r12, {delta}"),
                          labels_out=labels))
    return code, labels


def _runtime(executor):
    hybrid = HybridRuntime.create(executor=executor,
                                  geometry={"bank0_size": 1 << 20, "external_size": 1 << 20})
    hybrid.register(RoutineDeclaration("M+", bytes(assemble("add r4, r5\nret.l")),
                                       input_cells=2, output_cells=1))
    code, labels = _callback_code()
    for name, target in (("M-PRIM", "+"), ("M-COLON", "COLON+")):
        hybrid.register(RoutineDeclaration(name, code, input_cells=2, output_cells=1, callbacks=(
            CallbackSite(labels["call"], labels["stub"], target, 2, 1),)))
    hybrid.semantic.evaluate(b": COLON+ + ;")
    for name, body in (("BASE", "+"), ("CALL", "M+"), ("PRIM", "M-PRIM"),
                       ("COLON", "M-COLON"), ("COLON-BASE", "COLON+")):
        hybrid.semantic.evaluate(f": LOOP-{name} 0 SWAP 0 DO 1 {body} LOOP DROP ;".encode())
    return hybrid


def _seconds(hybrid, name, iterations):
    runtime = hybrid.semantic
    runtime.main_context.data.push(iterations)
    started = time.perf_counter()
    runtime.execute(f"LOOP-{name}")
    return time.perf_counter() - started


def measure(executor, iterations, trials):
    hybrid = _runtime(executor)
    try:
        cases = {}
        for name in ("BASE", "CALL", "PRIM", "COLON", "COLON-BASE"):
            _seconds(hybrid, name, max(1, iterations // 10))  # warm plans and caches
            cases[name] = min(_seconds(hybrid, name, iterations) for _ in range(trials))
        per_call = lambda name, base: (cases[name] - cases[base]) / iterations * 1e6
        return {
            "executor": hybrid.semantic.execution_backend,
            "iterations": iterations,
            "forth_to_machine_us": per_call("CALL", "BASE"),
            "machine_to_primitive_and_back_us": per_call("PRIM", "BASE"),
            "machine_to_colon_and_back_us": per_call("COLON", "COLON-BASE"),
            "machine_instructions": hybrid.machine_instructions,
            "callbacks": hybrid.callback_requests,
        }
    finally:
        hybrid.close()


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executor", nargs="+", default=["python", "native"])
    parser.add_argument("--iterations", type=int, default=20_000)
    parser.add_argument("--trials", type=int, default=3)
    args = parser.parse_args(argv)
    results = [measure(executor, args.iterations, args.trials) for executor in args.executor]
    print(json.dumps(results, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
