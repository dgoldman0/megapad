"""
setup_accel.py — Build the _mp64_accel C++ extension module.

Usage:
    python setup_accel.py build_ext --inplace
    # or via Makefile:
    make accel
"""

from setuptools import setup, Extension
import pybind11


ext = Extension(
    "_mp64_accel",
    sources=["accel/mp64_accel.cpp"],
    depends=["accel/mp64_crypto.h", "accel/mp64_nic.h"],
    include_dirs=[pybind11.get_include()],
    language="c++",
    extra_compile_args=[
        "-std=c++17",
        "-O3",
        "-march=native",
        "-Wall",
        "-Wextra",
        "-Wno-unused-parameter",
        "-fvisibility=hidden",
    ],
)

setup(
    name="mp64_accel",
    version="0.1.0",
    description="C++ accelerated core for Megapad-64 emulator",
    ext_modules=[ext],
)
