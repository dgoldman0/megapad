"""FF, which the ISA reserves as permanently illegal (docs/isa-reference.md).

The assembler writes it for ILLEGAL, and the CLI's disassembler reads it
back as ILLEGAL, one byte that starts nothing after it. Every backend traps
it as IVEC_ILLEGAL_OP; tests/test_fp_scalar.py runs it on the Python core
beside the unassigned F7, FD and FE. The BIOS executes it on purpose at
task_word_refused, when PAUSE or TASK-YIELD cannot run, and its fault report
names that refusal; an FF anywhere else is reported as an illegal
instruction. Both give the FAULT-XT! callback -21.
"""

from __future__ import annotations

from asm import assemble
from cli import disasm_one
from tests.test_bios_worker_signals import _Machine


def test_illegal_assembles_to_ff_and_disassembles_back():
    assert assemble("illegal") == b"\xff"
    assert assemble("ILLEGAL\nnop") == b"\xff\x01"
    assert disasm_one(b"\xff\x01", 0, 2) == ("ILLEGAL", 1)


def test_the_bios_names_a_refused_slot_word_and_any_other_ff_is_illegal():
    machine = _Machine(full_cores=1, chiplet=False)
    machine.run([
        # The callback reports the code it is given and returns, so the
        # BIOS's report follows.
        ": CODE ( code -- ) REPORT ;",
        "' CODE FAULT-XT!",
    ])
    reports, text = machine.run(["HERE 255 C, EXECUTE"])
    assert reports == [-21]
    assert "*** ILLEGAL INSTRUCTION PC=" in text
    assert "REFUSED" not in text

    reports, text = machine.run(["TASK-YIELD"])
    assert reports == [-21]
    assert "*** PAUSE OR TASK-YIELD REFUSED PC=" in text
    assert "ILLEGAL INSTRUCTION" not in text
