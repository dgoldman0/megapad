"""Build the independent native MegaForth semantic executor.

    python setup_simulator_accel.py build_ext --inplace
"""
import os

import pybind11
from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext

sanitizer = os.environ.get("MEGAFORTH_NATIVE_SANITIZER", "none")
if sanitizer not in ("none", "address-undefined"):
    raise SystemExit("MEGAFORTH_NATIVE_SANITIZER must be none or address-undefined")
flags = ["-std=c++17", "-ffp-contract=off", "-frounding-math", "-Wall", "-Wextra", "-fvisibility=hidden"]
links = []
if sanitizer == "none":
    flags += ["-O3"]
else:
    instrumentation = ["-fsanitize=address,undefined", "-fno-sanitize-recover=all"]
    flags += ["-O1", "-g", "-fno-omit-frame-pointer", *instrumentation]
    links += instrumentation

class IsolatedBuildExt(build_ext):
    """Keep shared source objects specific to this extension's compile flags."""

    def finalize_options(self):
        super().finalize_options()
        self.build_temp = os.path.join(self.build_temp, "megaforth")

setup(
    cmdclass={"build_ext": IsolatedBuildExt},
    name="megaforth_native",
    version="0.1.0",
    ext_modules=[Extension(
        "_megaforth_native", [
            "simulator/accel/semantic_executor.cpp", "shared/accel/scalar_fp.cpp", "shared/accel/keccak.cpp",
            "shared/accel/tile_values.cpp",
        ],
        depends=["shared/accel/scalar_fp.h", "shared/accel/scalar_fp_bindings.h",
                 "shared/accel/keccak.h", "shared/accel/keccak_bindings.h",
                 "shared/accel/tile_values.h", "shared/accel/tile_values_bindings.h",
                 "shared/accel/tile_guard_bindings.h", "shared/accel/routine_call.h"],
        include_dirs=[pybind11.get_include()], language="c++",
        extra_compile_args=flags, extra_link_args=links,
    )],
)
