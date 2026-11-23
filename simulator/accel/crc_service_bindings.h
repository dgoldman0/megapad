#pragma once

// Python boundary of the hosted CRC service (crc_service.h). The Python CRC
// words call these methods with their caller's (COREID, TASK-ID); the native
// engine calls the service itself.
#include <pybind11/pybind11.h>

#include <stdexcept>

#include "crc_service.h"

namespace megapad::crc_service {

namespace py = pybind11;

// The capability bit that admits the reflected modes and the raw fetch.
inline constexpr Cell capability_reflect_raw = 1;

inline Identity identity_from(const py::handle& value) {
    auto pair = py::reinterpret_borrow<py::tuple>(value);
    if (pair.size() != 2)
        throw py::value_error("a CRC caller identity is (COREID, TASK-ID)");
    return Identity{pair[0].cast<Cell>(), pair[1].cast<Cell>()};
}

// A Python integer reduced to its 64-bit cell, as the words pop it.
inline Cell cell_from(const py::handle& value) {
    py::object index = py::reinterpret_steal<py::object>(PyNumber_Index(value.ptr()));
    if (!index) throw py::error_already_set();
    const unsigned long long cell = PyLong_AsUnsignedLongLongMask(index.ptr());
    if (cell == static_cast<unsigned long long>(-1) && PyErr_Occurred())
        throw py::error_already_set();
    return static_cast<Cell>(cell);
}

inline void register_bindings(py::module_& module) {
    py::class_<Service>(module, "CRCService")
        .def(py::init([](py::object capabilities, int core_count) {
                 if (PyBool_Check(capabilities.ptr()) || !PyLong_Check(capabilities.ptr()))
                     throw py::type_error("CRC capabilities must be a uint64 integer");
                 int overflow = 0;
                 const long long value =
                     PyLong_AsLongLongAndOverflow(capabilities.ptr(), &overflow);
                 if (overflow != 0 || value < 0)
                     throw py::value_error("CRC capabilities must be a uint64 integer");
                 if (static_cast<Cell>(value) & ~capability_reflect_raw)
                     throw py::value_error(
                         "hosted CRC service cannot admit unimplemented capabilities");
                 if (core_count < 1 || core_count > 256)
                     throw py::value_error("a CRC service serves 1 to 256 cores");
                 return Service((static_cast<Cell>(value) & capability_reflect_raw) != 0,
                                core_count);
             }),
             py::arg("capabilities"), py::kw_only(), py::arg("core_count") = 1)
        .def_property_readonly("core_count", &Service::core_count)
        .def_property_readonly("capabilities", [](const Service& service) {
            return service.reflect_raw() ? capability_reflect_raw : Cell{0};
        })
        // Core 0's unit, and each core's by its ID.
        .def_property_readonly("mode", [](const Service& service) {
            return static_cast<Cell>(service.mode());
        })
        .def_property_readonly("accumulator", [](const Service& service) {
            return service.accumulator();
        })
        .def_property_readonly("owner", [](const Service& service) -> py::object {
            if (!service.owner()) return py::none();
            return py::make_tuple(service.owner()->core, service.owner()->task);
        })
        .def("unit_owner", [](const Service& service, int core) -> py::object {
            const auto& owner = service.owner(core);
            if (!owner) return py::none();
            return py::make_tuple(owner->core, owner->task);
        }, py::arg("core"))
        .def("select_mode", [](Service& service, py::handle who, py::handle mode) {
            return service.select_mode(identity_from(who), cell_from(mode));
        }, py::arg("identity"), py::arg("mode"))
        .def("reset", [](Service& service, py::handle who) {
            return service.reset(identity_from(who));
        }, py::arg("identity"))
        .def("seed", [](Service& service, py::handle who, py::handle seed) {
            return service.seed(identity_from(who), cell_from(seed));
        }, py::arg("identity"), py::arg("seed"))
        .def("feed_cell", [](Service& service, py::handle who, py::handle cell) {
            return service.feed_cell(identity_from(who), cell_from(cell));
        }, py::arg("identity"), py::arg("cell"))
        .def("feed_byte", [](Service& service, py::handle who, py::handle byte) {
            return service.feed_byte(identity_from(who), cell_from(byte));
        }, py::arg("identity"), py::arg("byte"))
        .def("feed_bytes", [](Service& service, py::handle who, py::buffer payload) {
            const Identity caller = identity_from(who);
            py::buffer_info info = payload.request();
            if (info.itemsize != 1 || info.ndim != 1 ||
                (info.ndim == 1 && info.strides[0] != 1))
                throw py::type_error("CRC payload must be bytes-like");
            return service.feed_bytes(caller, static_cast<const std::uint8_t*>(info.ptr),
                                      static_cast<std::size_t>(info.size));
        }, py::arg("identity"), py::arg("payload"))
        .def("fetch", [](const Service& service, py::handle who) {
            const auto [value, status] = service.fetch(identity_from(who));
            return py::make_tuple(value, status);
        }, py::arg("identity"))
        .def("raw_final", [](Service& service, py::handle who) {
            const auto [value, status] = service.raw_final(identity_from(who));
            return py::make_tuple(value, status);
        }, py::arg("identity"))
        .def("final", [](Service& service, py::handle who) {
            return service.final(identity_from(who));
        }, py::arg("identity"));
}

}  // namespace megapad::crc_service
