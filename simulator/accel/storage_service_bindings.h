#pragma once

// Python boundary of the hosted storage service (storage_service.h).  The
// class keeps the Python interface the hosted service has always had, so the
// runtime, the BIOS words' Python side and tests call it unchanged; the
// checks that interface makes of its arguments are made here.
#include <pybind11/pybind11.h>

#include <optional>
#include <string>

#include "../../shared/accel/storage_faults_bindings.h"
#include "collected.h"
#include "storage_service.h"

namespace megapad::storage_service {

namespace bindings {

// A uint64 cell argument, refusing bools and other types as the hosted
// service always did.
inline Cell cell_argument(py::handle value, const char* label) {
    if (PyBool_Check(value.ptr()) || !PyLong_Check(value.ptr()))
        throw py::type_error(std::string(label) + " must be an integer");
    int overflow = 0;
    const unsigned long long cell = PyLong_AsUnsignedLongLong(value.ptr());
    if (cell == static_cast<unsigned long long>(-1) && PyErr_Occurred()) {
        PyErr_Clear();
        (void)overflow;
        throw py::value_error(std::string(label) + " must be a uint64 cell");
    }
    return static_cast<Cell>(cell);
}

inline std::optional<Cell> generation_argument(py::handle value) {
    if (value.is_none()) return std::nullopt;
    return cell_argument(value, "storage media generation");
}

inline bool flag_argument(py::handle value) {
    if (!PyBool_Check(value.ptr()))
        throw py::type_error("storage write_protected flag must be bool");
    return value.ptr() == Py_True;
}

// IMAGE as a new bytearray the service owns, checked as a medium.
inline py::object copied_image(py::handle image) {
    if (!PyBytes_Check(image.ptr()) && !PyByteArray_Check(image.ptr()) &&
            !PyMemoryView_Check(image.ptr()))
        throw py::type_error("storage image must be bytes-like");
    PyObject* copy = PyByteArray_FromObject(image.ptr());
    if (copy == nullptr) throw py::error_already_set();
    return py::reinterpret_steal<py::object>(copy);
}

// IMAGE itself, a bytearray or writable mapping, as the medium.
inline py::object adopted_image(py::handle image) {
    if (!PyByteArray_Check(image.ptr()) &&
            !py::isinstance(image, py::module_::import("mmap").attr("mmap")))
        throw py::type_error("an adopted storage image must be a bytearray or a mapping");
    PyObject* raw_view = PyMemoryView_FromObject(image.ptr());
    if (raw_view == nullptr) throw py::error_already_set();
    auto view = py::reinterpret_steal<py::object>(raw_view);
    const bool readonly = PyMemoryView_GET_BUFFER(view.ptr())->readonly != 0;
    view.attr("release")();
    if (readonly) throw py::type_error("an adopted storage image must be writable");
    return py::reinterpret_borrow<py::object>(image);
}

inline void check_size(const py::object& image) {
    const Py_ssize_t size = PyObject_Length(image.ptr());
    if (size < 0) throw py::error_already_set();
    const auto bytes = static_cast<Cell>(size);
    if (bytes % sector_size)
        throw py::value_error("storage image size " + std::to_string(bytes)
            + " is not a multiple of the 512-byte sector size");
    if (bytes / sector_size > max_media_sectors)
        throw py::value_error("storage image exceeds the u32 sector capacity");
}

inline py::object path_object(py::handle path) {
    if (path.is_none()) return py::none();
    return py::module_::import("pathlib").attr("Path")(path);
}

// MEMORY, a SparseAddressSpace, as native code reaches its regions.
inline megapad::guest::GuestMemory guest_memory_of(py::handle memory) {
    py::module_ module = py::module_::import("simulator.memory");
    if (!py::isinstance(memory, module.attr("SparseAddressSpace")))
        throw py::type_error("storage DMA memory must be a SparseAddressSpace");
    py::tuple descriptors = module.attr("native_region_descriptors")(memory);
    const Cell page_size = memory.attr("page_size").cast<Cell>();
    return megapad::guest::GuestMemory(
        megapad::guest::build_regions(descriptors[0], page_size, descriptors[1]),
        page_size);
}

// A transfer for the Python words: through the memory the service is bound
// to when MEMORY is it, otherwise through MEMORY for this call alone, so
// that a caller's own memory never replaces the runtime's.
inline Outcome python_transfer(Service& service, bool write, py::handle memory,
                               py::handle dma, py::handle lba, py::handle count,
                               py::handle generation) {
    std::optional<megapad::guest::GuestMemory> other;
    if (!service.memory_owner().is(memory)) other.emplace(guest_memory_of(memory));
    const Cell address = cell_argument(dma, "storage DMA address");
    const Cell first = cell_argument(lba, "storage LBA");
    const Cell sectors = cell_argument(count, "storage sector count");
    const std::optional<Cell> bound = generation_argument(generation);
    return service.transfer(other ? *other : service.memory(), write, address, first,
                            sectors, bound);
}

inline py::tuple outcome_tuple(const Outcome& outcome) {
    return py::make_tuple(outcome.completed, outcome.status);
}

// A fault as the emulator's controller makes one, refusing a stall: this
// service completes each request when it is made.
inline Fault service_fault(py::handle stage, py::handle result, py::handle command,
                           py::handle sector_index, py::handle byte_index,
                           py::handle action) {
    namespace faults = megapad::storage::bindings;
    Fault fault;
    try {
        fault = megapad::storage::make_fault(
            stage.cast<std::string>(), faults::saturated(result),
            faults::optional_integer(command), faults::optional_integer(sector_index),
            faults::optional_integer(byte_index), action.cast<std::string>());
    } catch (const std::invalid_argument& error) {
        throw py::value_error(error.what());
    }
    if (fault.action == megapad::storage::Action::stall)
        throw py::value_error("the hosted storage service completes each request when "
                              "it is made and cannot hold one stalled");
    return fault;
}

}  // namespace bindings

inline void attach_python(Service& service, py::handle image, py::handle image_path,
                          py::handle write_protected, bool file_holds_media, bool adopt) {
    using namespace bindings;
    const bool protect = flag_argument(write_protected);
    if (file_holds_media && image_path.is_none())
        throw py::value_error("a medium held in its file needs the file's path");
    py::object medium = adopt ? adopted_image(image) : copied_image(image);
    check_size(medium);
    service.attach(std::move(medium), path_object(image_path), protect, file_holds_media);
}

inline void register_bindings(py::module_& module) {
    using namespace bindings;
    namespace storage = megapad::storage;

    py::class_<Service, PythonService>(module, "StorageService", collected::setup<Service>())
        .def(py::init([](py::object image, py::object image_path,
                         py::object write_protected, py::object capabilities) {
                 if (!image.is_none() && !image_path.is_none())
                     throw py::value_error(
                         "storage image bytes and image path are mutually exclusive");
                 const bool protect = flag_argument(write_protected);
                 if (PyBool_Check(capabilities.ptr()) || !PyLong_Check(capabilities.ptr()))
                     throw py::type_error("storage capabilities must be an integer");
                 int overflow = 0;
                 const long long caps =
                     PyLong_AsLongLongAndOverflow(capabilities.ptr(), &overflow);
                 if (overflow != 0 || caps < 0 || caps > 0xFF ||
                         (static_cast<Cell>(caps) & ~all_caps))
                     throw py::value_error("storage capabilities contain unsupported bits");
                 // Returned by value, so that pybind11 makes a Python
                 // subclass's instance the alias, moved from this one.
                 Service service(static_cast<Cell>(caps));
                 if (!image_path.is_none()) {
                     py::object path = path_object(image_path);
                     py::object payload;
                     bool found = true;
                     try {
                         payload = path.attr("read_bytes")();
                     } catch (py::error_already_set& error) {
                         // A write-protected medium is never written, so a
                         // file that holds no image can never come to hold one.
                         if (!error.matches(PyExc_FileNotFoundError) || protect) throw;
                         payload = py::bytes(std::string(
                             static_cast<size_t>(sector_size * default_image_sectors), '\0'));
                         found = false;
                     }
                     attach_python(service, payload, path, write_protected,
                                   false, false);
                     // Media read from its own file needs only its changes written.
                     service.host_file().attr("media_attached")(
                         py::arg("file_holds_media") = found);
                 } else if (!image.is_none()) {
                     attach_python(service, image, py::none(), write_protected,
                                   false, false);
                 } else if (protect) {
                     throw py::value_error("absent storage cannot be write protected");
                 }
                 return service;
             }),
             py::arg("image") = py::none(), py::kw_only(),
             py::arg("image_path") = py::none(), py::arg("write_protected") = false,
             py::arg("capabilities") = all_caps)
        .def("claim", &Service::claim)
        .def_property_readonly("present", &Service::present)
        .def_property_readonly("write_protected", &Service::write_protected)
        .def_property_readonly("status", &Service::status)
        .def_property_readonly("capabilities", &Service::capabilities)
        .def_property_readonly("total_sectors", &Service::total_sectors)
        .def_property_readonly("media_generation", &Service::media_generation)
        .def_property_readonly("completion", &Service::completion)
        .def_property_readonly("result", &Service::result)
        .def_property_readonly("transferred", &Service::transferred)
        .def_property_readonly("image_bytes", [](const Service& service) {
            return py::bytes(reinterpret_cast<const char*>(service.data()),
                             static_cast<size_t>(service.size()));
        })
        .def_property_readonly("image_path", &Service::image_path)
        .def("attach", [](Service& service, py::handle image, py::handle image_path,
                          py::handle write_protected, bool file_holds_media, bool adopt) {
                 attach_python(service, image, image_path, write_protected,
                               file_holds_media, adopt);
             },
             py::arg("image"), py::kw_only(), py::arg("image_path") = py::none(),
             py::arg("write_protected") = false, py::arg("file_holds_media") = false,
             py::arg("adopt") = false)
        .def("detach", &Service::detach)
        .def("set_write_protected", [](Service& service, py::handle enabled) {
            service.set_write_protected(flag_argument(enabled));
        }, py::arg("enabled"))
        .def("acknowledge_media_change", &Service::acknowledge_media_change)
        .def("read_checked",
             [](Service& service, py::handle memory, py::handle dma, py::handle lba,
                py::handle count, py::handle generation) {
                 return outcome_tuple(python_transfer(service, false, memory, dma, lba,
                                                      count, generation));
             },
             py::arg("memory"), py::arg("dma"), py::arg("lba"), py::arg("count"),
             py::kw_only(), py::arg("generation") = py::none())
        .def("write_checked",
             [](Service& service, py::handle memory, py::handle dma, py::handle lba,
                py::handle count, py::handle generation) {
                 return outcome_tuple(python_transfer(service, true, memory, dma, lba,
                                                      count, generation));
             },
             py::arg("memory"), py::arg("dma"), py::arg("lba"), py::arg("count"),
             py::kw_only(), py::arg("generation") = py::none())
        .def("flush_checked", [](Service& service, py::handle generation) {
            return service.flush(generation_argument(generation));
        }, py::kw_only(), py::arg("generation") = py::none())
        .def("_before_guarded_accept", &Service::before_guarded_accept,
             py::arg("command"), py::arg("expected_generation"))
        .def("bind_memory", [](Service& service, py::handle memory) {
            megapad::guest::GuestMemory bound = guest_memory_of(memory);
            service.bind_memory(py::reinterpret_borrow<py::object>(memory),
                                std::move(bound));
        }, py::arg("memory"))
        .def_property_readonly("hooked", &Service::hooked)
        .def_property_readonly("_faults", [](Service& service) -> storage::FaultQueue& {
            return service.faults();
        }, py::return_value_policy::reference_internal)
        .def("inject_fault",
             [](Service& service, py::object stage, py::object result, py::object command,
                py::object sector_index, py::object byte_index, py::object action) {
                 Fault fault = service_fault(stage, result, command, sector_index,
                                             byte_index, action);
                 service.faults().push(fault);
                 return fault;
             },
             py::arg("stage"), py::arg("result") = storage::result_media_failure,
             py::kw_only(), py::arg("command") = py::none(),
             py::arg("sector_index") = py::none(), py::arg("byte_index") = py::none(),
             py::arg("action") = "fail")
        .def("clear_faults", [](Service& service) { service.faults().clear(); })
        .def("observe", [](Service& service, py::object hook) {
            py::object observer = py::cast(RequestObserver());
            service.observe(observer, std::move(hook));
            return observer;
        }, py::arg("hook") = py::none())
        .def("stop_observing", &Service::stop_observing)
        .def("plan_fault",
             [](Service& service, py::object request_command, py::object ordinal,
                py::object after_writes, py::kwargs fault) {
                 RequestObserver* observer = service.observer();
                 if (observer == nullptr)
                     throw std::runtime_error("plan a storage fault after observe()");
                 Fault made = service_fault(
                     fault.contains("stage") ? fault["stage"] : py::object(py::none()),
                     fault.contains("result") ? fault["result"]
                         : py::object(py::int_(storage::result_media_failure)),
                     fault.contains("command") ? fault["command"] : py::object(py::none()),
                     fault.contains("sector_index") ? fault["sector_index"]
                         : py::object(py::none()),
                     fault.contains("byte_index") ? fault["byte_index"]
                         : py::object(py::none()),
                     fault.contains("action") ? fault["action"] : py::object(py::str("fail")));
                 std::optional<Cell> after;
                 if (!after_writes.is_none())
                     after = cell_argument(after_writes, "a write count");
                 try {
                     observer->plan(made,
                                    static_cast<std::uint8_t>(
                                        cell_argument(request_command, "a request command")),
                                    cell_argument(ordinal, "an ordinal"), after);
                 } catch (const std::invalid_argument& error) {
                     throw py::value_error(error.what());
                 }
                 return made;
             },
             py::arg("request_command"), py::arg("ordinal"), py::kw_only(),
             py::arg("after_writes") = py::none());
}

}  // namespace megapad::storage_service
