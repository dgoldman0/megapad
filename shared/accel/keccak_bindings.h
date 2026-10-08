#pragma once

// Python value boundary shared by both extensions. Device and runtime state
// remain with their callers; the native kernel has no Python dependency.
#include <pybind11/pybind11.h>
#include "keccak.h"

namespace megapad::keccak {

inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;
    const py::object sequence_type =
        py::module_::import("collections.abc").attr("Sequence");
    module.def("keccak_f1600", [sequence_type](py::object lanes) {
        if (!py::isinstance(lanes, sequence_type))
            throw py::type_error("Keccak state must be a sequence of uint64 lanes");
        if (py::len(lanes) != lane_count)
            throw py::value_error("Keccak state must contain exactly 25 lanes");

        struct LocalState {
            std::uint64_t lanes[lane_count]{};
            ~LocalState() {
                // Also erase our adapter-owned copy on Python conversion or
                // result-allocation failure. Caller inputs remain unchanged.
                volatile std::uint64_t* cursor = lanes;
                for (std::size_t index = 0; index < lane_count; ++index)
                    cursor[index] = 0;
            }
        } state;

        py::object iterator = py::reinterpret_steal<py::object>(
            PyObject_GetIter(lanes.ptr()));
        if (!iterator)
            throw py::error_already_set();
        for (std::size_t index = 0; index < lane_count; ++index) {
            py::object lane = py::reinterpret_steal<py::object>(
                PyIter_Next(iterator.ptr()));
            if (!lane) {
                if (PyErr_Occurred())
                    throw py::error_already_set();
                throw py::value_error("Keccak state must contain exactly 25 lanes");
            }
            if (PyBool_Check(lane.ptr()) || !PyLong_Check(lane.ptr()))
                throw py::type_error("Keccak lanes must be uint64 integers");
            const unsigned long long value = PyLong_AsUnsignedLongLong(lane.ptr());
            if (PyErr_Occurred()) {
                if (!PyErr_ExceptionMatches(PyExc_OverflowError))
                    throw py::error_already_set();
                PyErr_Clear();
                throw py::value_error("Keccak lanes must be uint64 integers");
            }
            state.lanes[index] = static_cast<std::uint64_t>(value);
        }
        py::object excess = py::reinterpret_steal<py::object>(
            PyIter_Next(iterator.ptr()));
        if (excess)
            throw py::value_error("Keccak state must contain exactly 25 lanes");
        if (PyErr_Occurred())
            throw py::error_already_set();

        permute(state.lanes);
        py::tuple result(lane_count);
        for (std::size_t index = 0; index < lane_count; ++index)
            result[index] = py::int_(state.lanes[index]);
        return result;
    }, py::arg("lanes"));
}

}  // namespace megapad::keccak
