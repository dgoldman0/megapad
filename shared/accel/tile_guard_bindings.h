#pragma once

// Hosted admission only: the tile arithmetic library has no Python dependency.
// Every match observes current identities while the GIL is held. There are no
// generation caches, calls to Python equality, or custom attribute lookups.
#include <pybind11/pybind11.h>

#include <array>
#include <utility>
#include <vector>

namespace megapad::tile_guard {
namespace py = pybind11;

class IdentityGuard {
    static constexpr Py_ssize_t max_identities = 128;
    using Identities = std::vector<std::pair<py::object, py::object>>;

    static Identities identities(py::handle entries) {
        if (!PyTuple_CheckExact(entries.ptr()))
            throw py::type_error("tile guard identities must be an exact tuple");
        const auto count = PyTuple_GET_SIZE(entries.ptr());
        if (count > max_identities)
            throw py::value_error("tile guard admits at most 128 identities per owner");
        Identities result;
        result.reserve(static_cast<std::size_t>(count));
        for (Py_ssize_t index = 0; index < count; ++index) {
            auto* entry = PyTuple_GET_ITEM(entries.ptr(), index);
            if (!PyTuple_CheckExact(entry) || PyTuple_GET_SIZE(entry) != 2)
                throw py::type_error("tile guard entries must be exact (name, value) tuples");
            auto* name = PyTuple_GET_ITEM(entry, 0);
            if (!PyUnicode_CheckExact(name))
                throw py::type_error("tile guard names must be exact strings");
            if (PyUnicode_GET_LENGTH(name) == 0)
                throw py::value_error("tile guard names must not be empty");
            for (const auto& previous : result) {
                if (PyUnicode_Compare(previous.first.ptr(), name) == 0)
                    throw py::value_error("tile guard identity names must be unique");
            }
            result.emplace_back(py::reinterpret_borrow<py::object>(name),
                py::reinterpret_borrow<py::object>(PyTuple_GET_ITEM(entry, 1)));
        }
        return result;
    }

    // Even an ordinary instance/module dictionary can contain a hostile key
    // inserted directly through __dict__. Prove exact string keys before a
    // dict lookup, so hash collisions can never invoke user-defined equality.
    static bool string_dictionary(PyObject* dictionary) {
        if (!dictionary || !PyDict_Check(dictionary) || PyDict_Size(dictionary) > 4096)
            return false;
        Py_ssize_t position = 0;
        PyObject *key, *value;
        while (PyDict_Next(dictionary, &position, &key, &value)) {
            if (!PyUnicode_CheckExact(key))
                return false;
        }
        return true;
    }

    static PyObject* lookup(PyObject* dictionary, PyObject* name) {
        auto* value = PyDict_GetItemWithError(dictionary, name);
        if (!value && PyErr_Occurred())
            throw py::error_already_set();
        return value;
    }

    struct ClassIdentity {
        py::object owner;
        Identities methods;

        ClassIdentity(py::handle type, py::handle entries)
            : owner(py::reinterpret_borrow<py::object>(type)),
              methods(identities(entries)) {
            if (!PyType_Check(type.ptr()) || Py_TYPE(type.ptr()) != &PyType_Type)
                throw py::type_error("tile guard classes must have the exact built-in metaclass");
        }

        bool matches(py::handle instance) const {
            auto* type = reinterpret_cast<PyTypeObject*>(owner.ptr());
            if (Py_TYPE(instance.ptr()) != type || Py_TYPE(owner.ptr()) != &PyType_Type ||
                type->tp_base != &PyBaseObject_Type ||
                !PyTuple_CheckExact(type->tp_bases) || PyTuple_GET_SIZE(type->tp_bases) != 1 ||
                PyTuple_GET_ITEM(type->tp_bases, 0) != reinterpret_cast<PyObject*>(&PyBaseObject_Type) ||
                type->tp_getattro != PyObject_GenericGetAttr || type->tp_getattr != nullptr ||
                type->tp_setattro != PyObject_GenericSetAttr || type->tp_setattr != nullptr ||
                !string_dictionary(type->tp_dict))
                return false;

            py::object local;
            bool has_dictionary = type->tp_dictoffset != 0;
#ifdef Py_TPFLAGS_MANAGED_DICT
            has_dictionary = has_dictionary || (type->tp_flags & Py_TPFLAGS_MANAGED_DICT);
#endif
            if (has_dictionary) {
                // Public raw-dictionary API: do not evaluate an overridden
                // __dict__ property or an instance's __getattribute__ method.
                local = py::reinterpret_steal<py::object>(
                    PyObject_GenericGetDict(instance.ptr(), nullptr));
                if (!local)
                    throw py::error_already_set();
                if (!string_dictionary(local.ptr()))
                    return false;
            }
            for (const auto& method : methods) {
                if (lookup(type->tp_dict, method.first.ptr()) != method.second.ptr() ||
                    (local && lookup(local.ptr(), method.first.ptr()) != nullptr))
                    return false;
            }
            return true;
        }
    };

    std::array<ClassIdentity, 3> classes_;
    py::object module_;
    Identities helpers_;

public:
    IdentityGuard(py::handle service_type, py::handle service_methods,
                  py::handle memory_type, py::handle memory_methods,
                  py::handle registers_type, py::handle registers_methods,
                  py::handle module, py::handle helpers)
        : classes_{{ClassIdentity(service_type, service_methods),
                    ClassIdentity(memory_type, memory_methods),
                    ClassIdentity(registers_type, registers_methods)}},
          module_(py::reinterpret_borrow<py::object>(module)), helpers_(identities(helpers)) {
        if (!PyModule_CheckExact(module.ptr()))
            throw py::type_error("tile guard helper owner must be an exact module");
    }

    bool matches(py::handle service, py::handle memory, py::handle registers) const {
        if (!classes_[0].matches(service) || !classes_[1].matches(memory) ||
            !classes_[2].matches(registers) || !PyModule_CheckExact(module_.ptr()))
            return false;
        auto* dictionary = PyModule_GetDict(module_.ptr());
        if (!string_dictionary(dictionary))
            return false;
        for (const auto& helper : helpers_) {
            if (lookup(dictionary, helper.first.ptr()) != helper.second.ptr())
                return false;
        }
        return true;
    }
};

inline void register_bindings(py::module_& module) {
    module.attr("TILE_GUARD_API_VERSION") = 1;
    // Both native extensions expose the same implementation independently.
    py::class_<IdentityGuard>(module, "TileIdentityGuard", py::module_local())
        .def(py::init<py::handle, py::handle, py::handle, py::handle,
                      py::handle, py::handle, py::handle, py::handle>(),
             py::arg("service_type"), py::arg("service_methods"),
             py::arg("memory_type"), py::arg("memory_methods"),
             py::arg("registers_type"), py::arg("registers_methods"),
             py::arg("module"), py::arg("helpers"))
        .def("matches", &IdentityGuard::matches, py::arg("service"),
             py::arg("memory"), py::arg("registers"));
}

}  // namespace megapad::tile_guard
