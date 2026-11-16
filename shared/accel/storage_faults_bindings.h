#pragma once

// Python boundary of the storage faults and request observer, shared by
// both extensions. Each extension registers the classes for itself
// (module_local), so a process may load both. The controllers that use them
// keep their media, registers and guest memory.
#include <pybind11/pybind11.h>

#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <utility>

#include "storage_faults.h"

namespace megapad::storage {

namespace bindings {

namespace py = pybind11;

// A Python integer as int64, saturated, so that a value past a range check
// still fails that check instead of wrapping into it.
inline std::int64_t saturated(py::handle value) {
    py::object index = py::reinterpret_steal<py::object>(
        PyNumber_Index(value.ptr()));
    if (!index)
        throw py::error_already_set();
    int overflow = 0;
    const long long converted =
        PyLong_AsLongLongAndOverflow(index.ptr(), &overflow);
    if (converted == -1 && PyErr_Occurred())
        throw py::error_already_set();
    if (overflow > 0)
        return std::numeric_limits<std::int64_t>::max();
    if (overflow < 0)
        return std::numeric_limits<std::int64_t>::min();
    return converted;
}

inline std::optional<std::int64_t> optional_integer(py::handle value) {
    if (value.is_none())
        return std::nullopt;
    return saturated(value);
}

inline py::object optional_object(const std::optional<std::uint64_t>& value) {
    if (!value)
        return py::none();
    return py::int_(*value);
}

inline Stage stage_from(const std::string& name) {
    Fault probe = make_fault(name, result_media_failure, std::nullopt,
                             std::nullopt, std::nullopt, "fail");
    return probe.stage;
}

inline std::uint8_t command_from(py::handle value) {
    const std::int64_t command = saturated(value);
    if (command < 0 || command > 0xFF)
        throw py::value_error("a storage command fits in one byte");
    return static_cast<std::uint8_t>(command);
}

inline std::uint64_t count_from(py::handle value, const char* what) {
    const std::int64_t count = saturated(value);
    if (count < 0)
        throw py::value_error(std::string(what) + " cannot be negative");
    return static_cast<std::uint64_t>(count);
}

inline std::optional<std::uint64_t> optional_index(py::handle value,
                                                   const char* what) {
    if (value.is_none())
        return std::nullopt;
    return count_from(value, what);
}

inline std::string fault_repr(const Fault& fault) {
    auto field = [](const char* name, const std::optional<std::uint64_t>& value) {
        return std::string(name) + "="
            + (value ? std::to_string(*value) : std::string("None"));
    };
    std::optional<std::uint64_t> command;
    if (fault.command)
        command = *fault.command;
    return "StorageFault(stage='" + std::string(stage_name(fault.stage))
        + "', result=" + std::to_string(fault.result) + ", "
        + field("command", command) + ", "
        + field("sector_index", fault.sector_index) + ", "
        + field("byte_index", fault.byte_index) + ", action='"
        + std::string(action_name(fault.action)) + "')";
}

}  // namespace bindings

inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;
    using namespace bindings;

    module.attr("STORAGE_FAULTS_API_VERSION") = 1;

    py::class_<Fault>(module, "StorageFault", py::module_local())
        .def_property_readonly("stage", [](const Fault& fault) {
            return std::string(stage_name(fault.stage));
        })
        .def_property_readonly("result", [](const Fault& fault) {
            return static_cast<int>(fault.result);
        })
        .def_property_readonly("command", [](const Fault& fault) -> py::object {
            if (!fault.command)
                return py::none();
            return py::int_(*fault.command);
        })
        .def_property_readonly("sector_index", [](const Fault& fault) {
            return optional_object(fault.sector_index);
        })
        .def_property_readonly("byte_index", [](const Fault& fault) {
            return optional_object(fault.byte_index);
        })
        .def_property_readonly("action", [](const Fault& fault) {
            return std::string(action_name(fault.action));
        })
        .def("__eq__", [](const Fault& fault, py::object other) {
            if (!py::isinstance<Fault>(other))
                return false;
            return fault == other.cast<const Fault&>();
        })
        .def("__hash__", [](const Fault& fault) {
            py::object command = py::none();
            if (fault.command)
                command = py::int_(*fault.command);
            return py::hash(py::make_tuple(
                static_cast<int>(fault.stage), fault.result, command,
                optional_object(fault.sector_index),
                optional_object(fault.byte_index),
                static_cast<int>(fault.action)));
        })
        .def("__repr__", &fault_repr);

    module.def(
        "storage_fault",
        [](const std::string& stage, py::object result, py::object command,
           py::object sector_index, py::object byte_index,
           const std::string& action) {
            try {
                return make_fault(stage, saturated(result),
                                  optional_integer(command),
                                  optional_integer(sector_index),
                                  optional_integer(byte_index), action);
            } catch (const std::invalid_argument& error) {
                throw py::value_error(error.what());
            }
        },
        py::arg("stage"), py::arg("result") = result_media_failure,
        py::kw_only(), py::arg("command") = py::none(),
        py::arg("sector_index") = py::none(),
        py::arg("byte_index") = py::none(), py::arg("action") = "fail");

    module.def("storage_fault_result", &fault_result, py::arg("fault"),
               py::arg("effect_applied"));

    py::class_<FaultQueue>(module, "StorageFaultQueue", py::module_local())
        .def(py::init<>())
        .def("push", &FaultQueue::push, py::arg("fault"))
        .def("clear", &FaultQueue::clear)
        .def("__len__", &FaultQueue::size)
        .def("__bool__", [](const FaultQueue& queue) { return !queue.empty(); })
        .def("__iter__", [](const FaultQueue& queue) {
            py::list faults;
            for (const Fault& fault : queue.faults())
                faults.append(py::cast(fault));
            return py::iter(faults);
        })
        .def("take",
             [](FaultQueue& queue, const std::string& stage, py::object command,
                py::object sector_index, py::object byte_index) -> py::object {
                 std::optional<Fault> fault;
                 try {
                     fault = queue.take(
                         stage_from(stage), command_from(command),
                         optional_index(sector_index, "a sector index"),
                         optional_index(byte_index, "a byte index"));
                 } catch (const std::invalid_argument& error) {
                     throw py::value_error(error.what());
                 }
                 if (!fault)
                     return py::none();
                 return py::cast(*fault);
             },
             py::arg("stage"), py::arg("command"),
             py::arg("sector_index") = py::none(),
             py::arg("byte_index") = py::none())
        .def("take_in_transfer",
             [](FaultQueue& queue, py::object command,
                py::object count) -> py::object {
                 std::optional<TransferFault> found;
                 try {
                     found = queue.take_in_transfer(
                         command_from(command), count_from(count, "a count"));
                 } catch (const std::invalid_argument& error) {
                     throw py::value_error(error.what());
                 }
                 if (!found)
                     return py::none();
                 return py::make_tuple(
                     found->fault, std::string(stage_name(found->stage)),
                     found->sector_index, optional_object(found->byte_index),
                     found->moved, found->confirmed);
             },
             py::arg("command"), py::arg("count"));

    py::class_<RequestObserver>(module, "StorageRequestObserver",
                                py::module_local())
        .def(py::init<>())
        .def("plan",
             [](RequestObserver& observer, const Fault& fault,
                py::object command, py::object ordinal,
                py::object after_writes) {
                 std::optional<std::uint64_t> after;
                 if (!after_writes.is_none())
                     after = count_from(after_writes, "a write count");
                 try {
                     observer.plan(fault, command_from(command),
                                   count_from(ordinal, "an ordinal"), after);
                 } catch (const std::invalid_argument& error) {
                     throw py::value_error(error.what());
                 }
             },
             py::arg("fault"), py::arg("command"), py::arg("ordinal"),
             py::arg("after_writes") = py::none())
        .def("admit",
             [](RequestObserver& observer, py::object command, py::object lba,
                py::object count, FaultQueue& queue) {
                 try {
                     return observer.admit(command_from(command),
                                           count_from(lba, "an LBA"),
                                           count_from(count, "a count"), queue);
                 } catch (const std::invalid_argument& error) {
                     throw py::value_error(error.what());
                 }
             },
             py::arg("command"), py::arg("lba"), py::arg("count"),
             py::arg("queue"))
        .def_property_readonly("requests", [](const RequestObserver& observer) {
            const auto& requests = observer.requests();
            py::tuple result(requests.size());
            for (std::size_t index = 0; index < requests.size(); ++index) {
                const Request& request = requests[index];
                result[index] = py::make_tuple(
                    static_cast<int>(request.command), request.lba, request.count);
            }
            return result;
        })
        .def_property_readonly("reads", &RequestObserver::reads)
        .def_property_readonly("writes", &RequestObserver::writes)
        .def_property_readonly("flushes", &RequestObserver::flushes)
        .def_property_readonly("pending", &RequestObserver::pending);
}

}  // namespace megapad::storage
