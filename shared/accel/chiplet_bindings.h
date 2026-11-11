#pragma once

// Python boundary of the chiplet-link model, shared by both extensions. The
// model itself has no Python dependency. Each extension registers these
// classes for itself (module_local), so a process may load both.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include "mailbox_pending.h"

namespace megapad::chiplet {

inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;

    py::class_<MailboxPending>(module, "MailboxPending", py::module_local())
        .def(py::init([](int core_count, int port_count,
                         py::object first_port_id) {
                 if (core_count < 1 || core_count > 256)
                     throw py::value_error("core_count must be 1 to 256");
                 if (port_count < 0 || port_count > 256)
                     throw py::value_error("port_count must be 0 to 256");
                 int first = core_count;
                 if (!first_port_id.is_none()) {
                     first = first_port_id.cast<int>();
                     if (first < core_count)
                         throw py::value_error(
                             "first_port_id must be at or above core_count");
                 }
                 if (port_count > 0 && first + port_count > 256)
                     throw py::value_error("port IDs must fit in 8 bits");
                 return MailboxPending(core_count, port_count, first);
             }),
             py::arg("core_count"), py::arg("port_count") = 0,
             py::arg("first_port_id") = py::none())
        .def_property_readonly("core_count", &MailboxPending::core_count)
        .def_property_readonly("port_count", &MailboxPending::port_count)
        .def_property_readonly("first_port_id",
                               &MailboxPending::first_port_id)
        .def("port_id", &MailboxPending::port_id, py::arg("port"))
        .def("is_participant", &MailboxPending::is_participant,
             py::arg("id"))
        .def("send", &MailboxPending::send,
             py::arg("source_id"), py::arg("target_id"))
        .def("acknowledge", &MailboxPending::acknowledge,
             py::arg("target_id"), py::arg("source_id"))
        .def("is_pending", &MailboxPending::is_pending,
             py::arg("target_id"), py::arg("source_id"))
        .def("any_pending", &MailboxPending::any_pending,
             py::arg("target_id"))
        .def("port_pending", &MailboxPending::port_pending,
             py::arg("target_id"))
        .def("lowest_source",
             [](const MailboxPending& table, int target_id) -> py::object {
                 const int source = table.lowest_source(target_id);
                 if (source == MailboxPending::no_source)
                     return py::none();
                 return py::int_(source);
             },
             py::arg("target_id"))
        .def("pending_mask", &MailboxPending::pending_mask,
             py::arg("target_id"))
        .def("clear_target", &MailboxPending::clear_target,
             py::arg("target_id"))
        .def("clear", &MailboxPending::clear)
        .def("words", [](const MailboxPending& table) {
                 return table.words();
             })
        .def("restore_words", &MailboxPending::restore_words,
             py::arg("words"));
}

}  // namespace megapad::chiplet
