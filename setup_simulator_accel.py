"""Build the independent native MegaForth semantic executor.

    python setup_simulator_accel.py build_ext --inplace
"""
import os

import pybind11
from setuptools import Extension, setup

sanitizer = os.environ.get("MEGAFORTH_NATIVE_SANITIZER", "none")
if sanitizer not in ("none", "address-undefined"):
    raise SystemExit("MEGAFORTH_NATIVE_SANITIZER must be none or address-undefined")
flags = ["-std=c++17", "-Wall", "-Wextra", "-fvisibility=hidden"]
links = []
if sanitizer == "none":
    flags += ["-O3"]
else:
    instrumentation = ["-fsanitize=address,undefined", "-fno-sanitize-recover=all"]
    flags += ["-O1", "-g", "-fno-omit-frame-pointer", *instrumentation]
    links += instrumentation
setup(
    name="megaforth_native",
    version="0.1.0",
    ext_modules=[Extension(
        "_megaforth_native", ["simulator/accel/semantic_executor.cpp"],
        include_dirs=[pybind11.get_include()], language="c++",
        extra_compile_args=flags, extra_link_args=links,
    )],
)
