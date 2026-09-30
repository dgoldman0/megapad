#pragma once

// Optional Python value boundary shared by both extension modules. The kernel
// itself has no Python dependency; execution adapters also call it directly.
#include <pybind11/pybind11.h>
#include "scalar_fp.h"

namespace megapad::scalar_fp {
inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;
    module.def("scalar_fp_validate", [](unsigned op, unsigned tail, uint64_t csr) {
        if (const char* error = validate(op, tail, csr))
            throw py::value_error(error);
    }, py::arg("op"), py::arg("tail") = 0, py::arg("fpcsr") = 0);
    module.def("scalar_fp_execute", [](unsigned op, uint64_t rd, uint64_t rs,
                                       uint64_t rt, uint64_t csr) {
        if (const char* error = validate(op, 0, csr))
            throw py::value_error(error);
        const Outcome result = execute(op, rd, rs, rt, csr);
        py::tuple value(3);
        if (result.has_relation) {
            value[0] = py::none();
            value[2] = py::int_(result.relation);
        } else {
            value[0] = py::int_(result.value);
            value[2] = py::none();
        }
        value[1] = py::int_(result.flags);
        return value;
    }, py::arg("op"), py::arg("rd"), py::arg("rs"),
       py::arg("rt") = 0, py::arg("fpcsr") = 0);
}
}  // namespace megapad::scalar_fp
