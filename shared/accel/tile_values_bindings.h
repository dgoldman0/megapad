#pragma once

#include <pybind11/pybind11.h>
#include "tile_guard_bindings.h"
#include "tile_values.h"

namespace megapad::tile_values {

namespace binding_detail {
namespace py = pybind11;

inline unsigned integer(py::handle value, unsigned maximum, const char* label) {
    if (!PyLong_CheckExact(value.ptr()))
        throw py::type_error(std::string(label) + " must be an exact integer");
    const auto number = PyLong_AsUnsignedLongLong(value.ptr());
    if (PyErr_Occurred()) {
        if (!PyErr_ExceptionMatches(PyExc_OverflowError))
            throw py::error_already_set();
        PyErr_Clear();
        throw py::value_error(std::string(label) + " is outside its unsigned range");
    }
    if (number > maximum)
        throw py::value_error(std::string(label) + " is outside its unsigned range");
    return static_cast<unsigned>(number);
}

inline bool operation(py::handle name, Operation& result) {
    if (!PyUnicode_CheckExact(name.ptr()))
        throw py::type_error("tile operation must be an exact string");
    Py_ssize_t size;
    const char* data = PyUnicode_AsUTF8AndSize(name.ptr(), &size);
    if (!data)
        throw py::error_already_set();
    return parse_operation(std::string_view(data, static_cast<std::size_t>(size)), result);
}

inline ByteView bytes(py::handle value, const char* label) {
    if (!PyBytes_CheckExact(value.ptr()))
        throw py::type_error(std::string(label) + " must be exact bytes");
    return {reinterpret_cast<const std::uint8_t*>(PyBytes_AS_STRING(value.ptr())),
            static_cast<std::size_t>(PyBytes_GET_SIZE(value.ptr()))};
}
}  // namespace binding_detail

inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;
    megapad::tile_guard::register_bindings(module);
    module.attr("TILE_VALUES_API_VERSION") = 1;
    py::list operations;
    for (unsigned index = 0; ; ++index) {
        const char* name = operation_name(static_cast<Operation>(index));
        if (!name)
            break;
        operations.append(py::str(name));
    }
    module.attr("TILE_VALUE_OPERATIONS") = py::tuple(operations);
    module.def("tile_values_supported", [](py::object operation, py::object mode,
                                            py::object argument) {
        Operation selected{};
        const bool known = binding_detail::operation(operation, selected);
        const auto selected_mode = binding_detail::integer(mode, 0x7F, "tile mode");
        const auto selected_argument = binding_detail::integer(argument, 0xFF, "tile argument");
        return known && supported(selected, selected_mode, selected_argument);
    }, py::arg("operation"), py::arg("mode"), py::arg("argument") = 0);
    module.def("tile_execute_values", [](py::object operation, py::object mode,
                                          py::object source0, py::object source1,
                                          py::object destination, py::object argument) -> py::object {
        Operation selected{};
        if (!binding_detail::operation(operation, selected))
            throw py::value_error("unknown tile value operation");
        const auto selected_mode = binding_detail::integer(mode, 0x7F, "tile mode");
        const auto selected_argument = binding_detail::integer(argument, 0xFF, "tile argument");
        const auto first = binding_detail::bytes(source0, "source0");
        const auto second = binding_detail::bytes(source1, "source1");
        const auto third = binding_detail::bytes(destination, "destination");
        // execute restores the host environment before allocating Python
        // results. Inputs remain immutable and alive throughout the call.
        const auto result = execute(selected, selected_mode, first, second, third,
                                    selected_argument);
        if (result.kind == ResultKind::Bytes)
            return py::bytes(reinterpret_cast<const char*>(result.bytes.data()), result.size);
        if (result.kind == ResultKind::Scalar)
            return py::int_(result.values[0]);
        const unsigned count = result.kind == ResultKind::Indexed ? 2 : 4;
        py::tuple values(count);
        for (unsigned index = 0; index < count; ++index)
            values[index] = py::int_(result.values[index]);
        return values;
    }, py::arg("operation"), py::arg("mode"), py::arg("source0"),
       py::arg("source1") = py::bytes(), py::arg("destination") = py::bytes(),
       py::arg("argument") = 0);
}

}  // namespace megapad::tile_values
