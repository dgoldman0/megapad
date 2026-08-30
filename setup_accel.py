"""
setup_accel.py — Build the _mp64_accel C++ extension module.

Usage:
    python setup_accel.py build_ext --inplace
    # or via Makefile:
    make accel
"""

import hashlib
import os
from pathlib import Path

from setuptools import Extension, setup
import pybind11


_ROOT = Path(__file__).resolve().parent
_ACCEL_ROOT = _ROOT / "emulator" / "accel"
_CRYPTO_SOURCE = _ACCEL_ROOT / "mp64_crypto.h"
_AES_MODEL_SOURCE_SHA256 = hashlib.sha256(_CRYPTO_SOURCE.read_bytes()).hexdigest()

_SANITIZER = os.environ.get("MP64_ACCEL_SANITIZER", "none")
_SANITIZER_FLAGS = {
    "none": [],
    "address-undefined": ["-fsanitize=address,undefined"],
    "thread": ["-fsanitize=thread"],
}
if _SANITIZER not in _SANITIZER_FLAGS:
    choices = ", ".join(_SANITIZER_FLAGS)
    raise SystemExit(
        "MP64_ACCEL_SANITIZER must be one of "
        f"{choices}; got {_SANITIZER!r}"
    )

_compile_args = [
    "-std=c++17",
    "-march=native",
    "-Wall",
    "-Wextra",
    "-Wno-unused-parameter",
    "-fvisibility=hidden",
    "-pthread",
]
_link_args = ["-pthread"]
if _SANITIZER == "none":
    _compile_args.append("-O3")
else:
    _instrumentation_args = [
        "-O1",
        "-g",
        "-fno-omit-frame-pointer",
        "-fno-sanitize-recover=all",
        *_SANITIZER_FLAGS[_SANITIZER],
    ]
    _compile_args.extend(_instrumentation_args)
    _link_args.extend(
        [
            "-fno-sanitize-recover=all",
            *_SANITIZER_FLAGS[_SANITIZER],
        ]
    )


ext = Extension(
    "_mp64_accel",
    sources=[
        "emulator/accel/mp64_accel.cpp",
        "emulator/accel/dbt/executable_arena.cpp",
        "emulator/accel/dbt/x86_64/emitter.cpp",
        "emulator/accel/dbt/x86_64/lowering.cpp",
        "emulator/accel/machine/settlement.cpp",
    ],
    depends=[
        "emulator/accel/cpu/mp64/block_ir.h",
        "emulator/accel/cpu/mp64/decode.h",
        "emulator/accel/cpu/mp64/decode_impl.h",
        "emulator/accel/cpu/mp64/interpreter.h",
        "emulator/accel/cpu/mp64/semantics.h",
        "emulator/accel/dbt/executable_arena.h",
        "emulator/accel/dbt/host_jit_config.h",
        "emulator/accel/dbt/x86_64/emitter.h",
        "emulator/accel/dbt/x86_64/lowering.h",
        "emulator/accel/machine/memory.h",
        "emulator/accel/machine/settlement.h",
        "emulator/accel/mp64_crypto.h",
        "emulator/accel/mp64_fb.h",
        "emulator/accel/mp64_nic.h",
        "emulator/accel/mp64_rtc.h",
        "emulator/accel/mp64_timer.h",
        "emulator/accel/mp64_uart.h",
        "emulator/accel/mp64_uart_geom.h",
    ],
    include_dirs=[pybind11.get_include()],
    define_macros=[
        ("MP64_AES_MODEL_SOURCE_SHA256", f'"{_AES_MODEL_SOURCE_SHA256}"'),
    ],
    language="c++",
    extra_compile_args=_compile_args,
    extra_link_args=_link_args,
)

setup(
    name="mp64_accel",
    version="0.1.0",
    description="C++ accelerated core for Megapad-64 emulator",
    ext_modules=[ext],
)
