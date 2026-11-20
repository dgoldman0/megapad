#pragma once

// Python boundary of the hosted TACC service (tacc_service.h). The hosted
// tile service's words call it with the tile registers and raise the fault
// an operation reports (simulator/tacc.py). Each operation returns its
// outcome as (fault, cause, address, length, retired, answer), with the values of
// tacc_service.h's Fault and Cause, followed by what the operation gives
// back: TSRC0 for tamac_scalars and the status for send.
#include <pybind11/pybind11.h>

#include <string>

#include "../../shared/accel/chiplet_bindings.h"
#include "collected.h"
#include "guest_memory.h"
#include "tacc_service.h"

namespace megapad::tacc_service {

inline py::tuple outcome_tuple(const Outcome& outcome) {
    return py::make_tuple(static_cast<unsigned>(outcome.fault),
                          static_cast<unsigned>(outcome.cause), outcome.address,
                          outcome.length, outcome.retired, outcome.answer);
}

inline void register_bindings(py::module_& module) {
    namespace chiplet = megapad::chiplet;
    py::class_<Service>(module, "TACCService", collected::setup<Service>())
        // MEMORY is the profile's SparseAddressSpace and LINK its chiplet
        // ports' ChipletLink, or None when it has no ports.
        .def(py::init([](py::handle memory, py::object link) {
                 megapad::guest::GuestMemory bound =
                     megapad::guest::address_space_memory(memory, "TACC memory");
                 chiplet::ChipletWindow* windows = nullptr;
                 if (!link.is_none())
                     windows = &py::cast<chiplet::ChipletLink&>(link).window_;
                 return Service(std::move(bound), py::reinterpret_borrow<py::object>(memory),
                                windows, std::move(link));
             }),
             py::arg("memory"), py::arg("link") = py::none())
        .def_property_readonly("memory", &Service::memory_owner)
        .def_property_readonly("status", &Service::status)
        .def_property_readonly("owner", &Service::owner)
        .def_property_readonly("valid", &Service::valid)
        .def_property_readonly("dirty", &Service::dirty)
        .def_property_readonly("format_ew", &Service::format_ew)
        .def_property_readonly("format_signed", &Service::format_signed)
        .def_property_readonly("bank", [](const Service& service) {
            const auto& bank = service.bank();
            return py::bytes(reinterpret_cast<const char*>(bank.data()), bank.size());
        })
        .def("try_claim", [](Service& service) { return outcome_tuple(service.try_claim()); })
        .def("clear", [](Service& service, Cell tmode) {
            return outcome_tuple(service.clear(tmode));
        }, py::arg("tmode"))
        .def("load", [](Service& service, Cell tmode, Cell source) {
            return outcome_tuple(service.load(tmode, source));
        }, py::arg("tmode"), py::arg("source"))
        .def("store", [](Service& service, Cell destination) {
            return outcome_tuple(service.store(destination));
        }, py::arg("destination"))
        .def("release", [](Service& service) { return outcome_tuple(service.release()); })
        .def("tamac", [](Service& service, Cell tmode, Cell a, Cell b) {
            return outcome_tuple(service.tamac(tmode, a, b));
        }, py::arg("tmode"), py::arg("a"), py::arg("b"))
        .def("tamac_scalar", [](Service& service, Cell tmode, Cell a, Cell scalar) {
            return outcome_tuple(service.tamac_scalar(tmode, a, scalar));
        }, py::arg("tmode"), py::arg("a"), py::arg("scalar"))
        .def("tamac_scalars",
             [](Service& service, Cell tmode, Cell tiles, Cell scalars, Cell count,
                Cell source0) {
                 const Outcome outcome =
                     service.tamac_scalars(tmode, tiles, scalars, count, source0);
                 return py::make_tuple(outcome_tuple(outcome), source0);
             },
             py::arg("tmode"), py::arg("tiles"), py::arg("scalars"), py::arg("count"),
             py::arg("source0"))
        .def("send",
             [](Service& service, Cell tmode, Cell source, Cell destination) {
                 Cell status = send_ok;
                 const Outcome outcome = service.send(tmode, source, destination, status);
                 return py::make_tuple(outcome_tuple(outcome), status);
             },
             py::arg("tmode"), py::arg("source"), py::arg("destination"));
}

}  // namespace megapad::tacc_service
