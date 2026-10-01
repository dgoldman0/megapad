# Hybrid mode

Hybrid mode runs Forth semantically, as the simulator does, and runs declared
MP64 machine routines on a native full core. Both share one memory image. The
machine side follows the chip: a routine's CALL.L and RET.L use the Forth
return stack, and a routine can call Forth words, which run on the caller's
own stacks.

Start a hybrid session with the unified launcher:

```bash
python megapad.py --mode hybrid --storage desktop.img --executor native \
    --hybrid-routines routines.json
```

## Routines

A routine is an ordinary dictionary word whose body holds its machine code.
Registering a routine defines the word and publishes its code. Forth calls it
like any other word: interpreted, compiled, or through `EXECUTE`.

A routine takes up to eight cells and gives back up to eight, passed in
r4-r11. The deepest input cell is r4; the first output cell becomes the
deepest pushed cell. A call pops the inputs when the machine starts and pushes
the outputs when it returns.

The machine runs integer instructions only. It cannot change the program
counter selector or write the stack pointer except through CALL.L and RET.L,
and it has no access to MMIO or devices.

Every published routine is executable at its own instruction boundaries, so
machine code may CALL.L another routine directly. Execution anywhere else
fails.

## The return stack

As on the chip, machine return addresses live on the Forth return stack.
When a routine starts, the runner writes a sentinel one cell below the
caller's frontier. The routine's frames grow down from there toward the
stack floor. Its final RET.L pops the sentinel and returns to Forth.

Each machine entry may touch only the stack below its own sentinel. A routine
started from a callback therefore cannot disturb the frames of the routine
waiting above it. A routine may also be entered again while it waits, which
gives ordinary recursion. Running out of return stack is a stack overflow,
exactly as for Forth.

## Memory

A routine reaches memory only through buffers named by its arguments. Each
buffer rule pairs an address argument with a length argument, times an
element size, with read, write or read-write access and an optional maximum.
Buffers may not cover published routine code or the return stack below the
caller's frontier. Any other load or store is refused before it happens.

Forth may write over a routine's code. As on the chip, the instruction cache
decides whether the machine sees the old bytes or the new ones. Machine
stores can never reach published code.

## Callbacks

A callback site is a CALL.L whose target is a RET.L stub in the same image.
The manifest names the Forth word to run there and how many cells it takes and
gives back. When the machine reaches the site:

1. The Forth return stack is pointed at the machine's stack pointer, so the
   routine's sentinel and frames are ordinary cells on it.
2. The slot holding the CALL.L return address becomes a machine return entry.
3. The site's argument cells are pushed and the named word runs on the
   caller's stacks, in whichever Forth executor is active.
4. When the word returns into the machine return, the data stack must hold
   exactly the site's output cells. They go to r4 onward, and the machine's
   own RET.L at the stub returns past the CALL.L.

The named word can be any word: a colon definition, a primitive, another
routine or the routine itself. It is looked up by name the first time the site
is used and kept while that word lives. Machine code cannot choose a callback
target at run time.

Nothing special happens when Forth unwinds. A THROW, ABORT or failed dispatch
moves the return stack past a machine return, and that machine entry is
abandoned before the next call or when the dispatch ends. A callback may use
EVALUATE, CATCH and THROW, IDL and the semantic quantum like any other Forth
code. The waiting routine survives a suspension in its callback.

## Faults, quanta and budgets

| Event | Result |
|---|---|
| Illegal instruction, jump outside published code, a stub reached without its callback, a bad return | The fault callback (`FAULT-XT!`) runs with throw code -21, as for an illegal instruction on the chip |
| Load or store outside the routine's buffers | A memory access error, as for a Forth memory fault in the simulator |
| CALL.L past the return stack floor | A return stack overflow |
| A callback leaving the wrong number of cells | A hybrid execution error naming the word and the site |

A session gives each host turn a machine quantum. A routine that uses it up
yields where an IDL could suspend, and resumes on the next turn; elsewhere,
such as inside EVALUATE, it simply keeps running. An optional per-dispatch
machine instruction budget stops runaway code with
`MachineBudgetExceeded`.

## Crossing costs

The native semantic executor calls routines without callback sites itself,
through `shared/accel/routine_call.h`. Routines with callback sites, and the
callbacks, go through the Python dispatcher. A routine that yields or faults
during a native call is taken over by Python without being run again.

`bench_hybrid_crossing.py` measures the added cost per crossing. On the
development machine, with 20,000 iterations:

| Crossing | Native executor | Python executor |
|---|---:|---:|
| Forth to a routine without callback sites | 0.45 us | 9.9 us |
| Routine calling back a primitive and returning | 39 us | 30 us |
| Routine calling back a colon word and returning | 47 us | 26 us |

These are host wall times, not MP64 cycles. Machine instructions and cycles
are counted separately from semantic steps.

## Manifest

```json
{
  "abi": "megapad.hybrid.routines",
  "version": 1,
  "routines": [
    {
      "name": "BLEND",
      "image": "blend.bin",
      "entry_offset": 0,
      "input_cells": 3,
      "output_cells": 1,
      "buffers": [
        {"address_argument": 0, "length_argument": 1, "element_bytes": 8,
         "access": "read_write", "max_bytes": 65536}
      ],
      "callbacks": [
        {"call_offset": 24, "stub_offset": 48, "target": "MIX",
         "input_cells": 2, "output_cells": 1}
      ]
    }
  ]
}
```

Images are raw MP64 code, read relative to the manifest. Their load address
is not known in advance, so code must be position independent; a callback
site can form its stub address from the PC register. `name` and `image` are
required; the other routine fields default to zero or empty. Unknown fields
are refused, and routine names must be distinct.

The server defines every routine before the image boots, so boot source can
call them. A manifest is optional.

## Sessions

`hybrid.session.HybridSession` is the semantic session with a machine owner
attached. The server takes:

- `--hybrid-routines MANIFEST`
- `--machine-quantum-instructions N`, default 1,000,000 per host turn
- `--machine-instruction-budget N`, default none

Session status reports `machine_execution`: the ABI, the routines, lifetime
machine instructions, cycles, segments, transitions and callbacks, and the
quantum and budget. Capabilities advertise declared machine routines and
semantic callbacks, and deny arbitrary machine code, machine MMIO, native BIOS
boot, multicore execution and native snapshots.

## Where the code lives

| Part | Source |
|---|---|
| Routine runner, images and events | `emulator/accel/mp64_accel.cpp`, `RoutineRunner` |
| Executor-to-runner call interface | `shared/accel/routine_call.h` |
| Routine words, machine returns and dispatch | `simulator/runtime.py`, `simulator/stacks.py` |
| Direct calls from native Forth | `simulator/native_execution.py`, `simulator/accel/semantic_executor.cpp` |
| Manifest | `hybrid/manifest.py` |
| Owner, faults, quanta and budgets | `hybrid/runtime.py` |
| Session and server | `hybrid/session.py`, `hybrid/server.py` |

## Not covered

Machine access to MMIO and devices, multicore hybrid execution, native
snapshots, and running compiled Forth or BIOS code as machine code are
outside this mode. The last is the staged design in
[`hybrid-native-dictionary-plan.md`](hybrid-native-dictionary-plan.md).
