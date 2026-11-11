// Cycle collection for the extension's classes.
//
// Python frees a group of objects that refer to one another only when its
// collector can see every reference inside the group. A pybind11 class
// shows it nothing by default, so whatever its C++ object refers to counts
// as referenced from outside, and a cycle through that object is never
// freed. A machine is such a cycle: the runtime owns the native dictionary,
// and the dictionary owns every definition and view, which lead back to the
// runtime. The line walker owns the dictionary as well, and the program
// holds the machine's tile service and memory through its tile guard.
//
// A class registered with `collected::setup<T>()` shows the collector what
// it owns through two members:
//
//     int  T::gc_traverse(visitproc visit, void* arg) const noexcept;
//     void T::gc_clear() noexcept;
//
// gc_traverse visits every Python reference the object owns, once for each
// reference it owns and none that it only borrows: the collector subtracts
// the visits from the reference counts, and a visit too many frees a live
// object. gc_clear drops references so that the cycle breaks, and leaves
// the object in a state its methods accept.
//
// The collector may traverse an object whenever Python code runs, which
// includes the release of a Python reference. An owner therefore never
// keeps a released reference in place: it moves the reference out of its
// member or container first and lets the moved-out copy release it.
#pragma once

#include <pybind11/pybind11.h>

namespace megapad::collected {
namespace py = pybind11;

template <typename T>
py::custom_type_setup setup() {
    return py::custom_type_setup([](PyHeapTypeObject* heap_type) {
        PyTypeObject* type = &heap_type->ht_type;
        type->tp_flags |= Py_TPFLAGS_HAVE_GC;
        type->tp_traverse = [](PyObject* self, visitproc visit, void* arg) -> int {
            // An instance of a heap type owns a reference to its type.
            Py_VISIT(Py_TYPE(self));
            // An instance is tracked from its allocation, and its
            // constructor can run Python code before the C++ object exists.
            if (!py::detail::is_holder_constructed(self)) return 0;
            return py::cast<T&>(py::handle(self)).gc_traverse(visit, arg);
        };
        type->tp_clear = [](PyObject* self) -> int {
            if (py::detail::is_holder_constructed(self))
                py::cast<T&>(py::handle(self)).gc_clear();
            return 0;
        };
    });
}

}  // namespace megapad::collected
