// Guest memory regions shared with Python's SparseAddressSpace.
//
// Sparse regions keep Python's own page dictionary of fixed-size bytearrays;
// dense regions keep a writable buffer lease. Native code reads and writes
// those same bytes, so there is no copied guest address space.
#pragma once

#include <pybind11/pybind11.h>

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>

namespace megapad::guest {
namespace py = pybind11;
using Cell = uint64_t;
constexpr Cell CELL_MASK = ~Cell{0};

struct Region {
    Cell base;
    Cell size;
    py::dict pages;
    // A dense descriptor owns a buffer export for the owner's full
    // lifetime. The pointer cannot be invalidated by exporter resizing.
    std::unique_ptr<py::buffer_info> dense_lease;
    uint8_t* dense = nullptr;
};

// Validate sparse (base, size, pages) and dense (base, size, buffer)
// descriptors. All spans must be nonwrapping and mutually disjoint, and dense
// host buffers must not alias.
inline std::vector<Region> build_regions(const py::iterable& regions, Cell page_size,
                                         const py::iterable& dense_regions) {
    if (page_size == 0 || (page_size & (page_size - 1)) != 0)
        throw py::value_error("page size must be a positive power of two");
    std::vector<Region> built;
    auto validate_region = [&](Cell base, Cell size) {
        if (size == 0 || base > CELL_MASK - (size - 1))
            throw py::value_error("region has an invalid ordinary span");
        for (const Region& previous : built) {
            if (base <= previous.base + previous.size - 1 &&
                previous.base <= base + size - 1)
                throw py::value_error("ordinary regions must not overlap");
        }
    };
    for (py::handle item : regions) {
        auto entry = py::cast<py::tuple>(item);
        if (entry.size() != 3)
            throw py::value_error("region must be (base, size, pages)");
        const Cell base = entry[0].cast<Cell>();
        const Cell size = entry[1].cast<Cell>();
        validate_region(base, size);
        built.push_back(Region{base, size, entry[2].cast<py::dict>(), nullptr});
    }
    for (py::handle item : dense_regions) {
        auto entry = py::cast<py::tuple>(item);
        if (entry.size() != 3)
            throw py::value_error("dense region must be (base, size, buffer)");
        const Cell base = entry[0].cast<Cell>();
        const Cell size = entry[1].cast<Cell>();
        validate_region(base, size);
        auto buffer = entry[2].cast<py::buffer>();
        // Export from an independent view, not a caller-owned memoryview:
        // releasing the caller's view must not detach or invalidate us.
        PyObject* raw_view = PyMemoryView_FromObject(buffer.ptr());
        if (raw_view == nullptr) throw py::error_already_set();
        auto owned_view = py::reinterpret_steal<py::buffer>(raw_view);
        auto lease = std::make_unique<py::buffer_info>(owned_view.request(true));
        if (lease->readonly)
            throw py::buffer_error("dense region requires a writable buffer");
        if (lease->ndim != 1 || lease->itemsize != 1 ||
            lease->shape.size() != 1 || lease->shape[0] < 0)
            throw py::value_error("dense region requires a one-dimensional byte buffer");
        if (lease->strides.size() != 1 || lease->strides[0] != 1 ||
            !lease->view() || !PyBuffer_IsContiguous(lease->view(), 'C'))
            throw py::value_error("dense region requires a C-contiguous buffer");
        if (static_cast<Cell>(lease->shape[0]) != size)
            throw py::value_error("dense region size must equal buffer capacity");
        if (lease->ptr == nullptr)
            throw py::value_error("dense region exposes a null data pointer");
        auto* pointer = static_cast<uint8_t*>(lease->ptr);
        const auto address = reinterpret_cast<std::uintptr_t>(pointer);
        for (const Region& previous : built) {
            if (previous.dense == nullptr) continue;
            const auto prior = reinterpret_cast<std::uintptr_t>(previous.dense);
            if (address <= prior ? prior - address < size
                                 : address - prior < previous.size)
                throw py::value_error("dense region buffers must not alias");
        }
        built.push_back(Region{base, size, py::dict(), std::move(lease), pointer});
    }
    return built;
}

// Byte access to ordinary regions for host-side services such as the native
// dictionary. Every access must lie wholly inside one region; callers have
// already proven that, as the Python services do before touching memory.
// A sparse write creates a missing zeroed page exactly as the Python write
// path does, while a zero fill leaves missing pages absent.
class GuestMemory {
public:
    GuestMemory() = default;
    GuestMemory(std::vector<Region> regions, Cell page_size)
        : regions_(std::move(regions)), page_size_(page_size) {
        for (Cell size = page_size; size > 1; size >>= 1) ++page_shift_;
    }

    bool contains(Cell address, Cell length) const {
        return find(address, length) != nullptr;
    }

    void read(Cell address, uint8_t* out, Cell length) {
        if (length == 0) return;
        Region& region = require(address, length);
        Cell offset = address - region.base;
        if (region.dense != nullptr) {
            std::memcpy(out, region.dense + offset, static_cast<size_t>(length));
            return;
        }
        while (length != 0) {
            const Cell page_offset = offset & (page_size_ - 1);
            const Cell chunk = std::min(page_size_ - page_offset, length);
            uint8_t* page = sparse_page(region, offset >> page_shift_, false);
            if (page == nullptr) std::memset(out, 0, static_cast<size_t>(chunk));
            else std::memcpy(out, page + page_offset, static_cast<size_t>(chunk));
            out += chunk;
            offset += chunk;
            length -= chunk;
        }
    }

    void write(Cell address, const uint8_t* data, Cell length) {
        if (length == 0) return;
        Region& region = require(address, length);
        Cell offset = address - region.base;
        if (region.dense != nullptr) {
            std::memcpy(region.dense + offset, data, static_cast<size_t>(length));
            return;
        }
        while (length != 0) {
            const Cell page_offset = offset & (page_size_ - 1);
            const Cell chunk = std::min(page_size_ - page_offset, length);
            uint8_t* page = sparse_page(region, offset >> page_shift_, true);
            std::memcpy(page + page_offset, data, static_cast<size_t>(chunk));
            data += chunk;
            offset += chunk;
            length -= chunk;
        }
    }

    void fill(Cell address, Cell length, uint8_t value) {
        if (length == 0) return;
        Region& region = require(address, length);
        Cell offset = address - region.base;
        if (region.dense != nullptr) {
            std::memset(region.dense + offset, value, static_cast<size_t>(length));
            return;
        }
        while (length != 0) {
            const Cell page_offset = offset & (page_size_ - 1);
            const Cell chunk = std::min(page_size_ - page_offset, length);
            uint8_t* page = sparse_page(region, offset >> page_shift_, value != 0);
            if (page != nullptr)
                std::memset(page + page_offset, value, static_cast<size_t>(chunk));
            offset += chunk;
            length -= chunk;
        }
    }

    Cell read64(Cell address) {
        uint8_t bytes[8];
        read(address, bytes, 8);
        Cell value = 0;
        for (int i = 7; i >= 0; --i) value = (value << 8) | bytes[i];
        return value;
    }

    uint8_t read8(Cell address) {
        uint8_t value = 0;
        read(address, &value, 1);
        return value;
    }

    void write64(Cell address, Cell value) {
        uint8_t bytes[8];
        for (int i = 0; i < 8; ++i) bytes[i] = static_cast<uint8_t>(value >> (8 * i));
        write(address, bytes, 8);
    }

    void write8(Cell address, uint8_t value) { write(address, &value, 1); }

private:
    const Region* find(Cell address, Cell length) const {
        if (length != 0 && address > CELL_MASK - (length - 1)) return nullptr;
        for (const Region& region : regions_) {
            if (address < region.base || address - region.base >= region.size) continue;
            return length <= region.size - (address - region.base) ? &region : nullptr;
        }
        return nullptr;
    }

    Region& require(Cell address, Cell length) {
        const Region* region = find(address, length);
        if (region == nullptr)
            throw std::runtime_error("native guest access left its ordinary regions");
        return const_cast<Region&>(*region);
    }

    uint8_t* sparse_page(Region& region, Cell index, bool create) {
        py::int_ key(index);
        PyObject* value = PyDict_GetItemWithError(region.pages.ptr(), key.ptr());
        if (value == nullptr && PyErr_Occurred()) throw py::error_already_set();
        if (value == nullptr) {
            if (!create) return nullptr;
            PyObject* page = PyByteArray_FromStringAndSize(nullptr,
                                                           static_cast<Py_ssize_t>(page_size_));
            if (page == nullptr) throw py::error_already_set();
            auto owned = py::reinterpret_steal<py::object>(page);
            std::memset(PyByteArray_AsString(page), 0, static_cast<size_t>(page_size_));
            if (PyDict_SetItem(region.pages.ptr(), key.ptr(), page) != 0)
                throw py::error_already_set();
            return reinterpret_cast<uint8_t*>(PyByteArray_AsString(page));
        }
        if (!PyByteArray_Check(value) ||
            static_cast<Cell>(PyByteArray_Size(value)) != page_size_)
            throw std::runtime_error("sparse guest page is not a page-sized bytearray");
        return reinterpret_cast<uint8_t*>(PyByteArray_AsString(value));
    }

    std::vector<Region> regions_;
    Cell page_size_ = 1;
    unsigned page_shift_ = 0;
};

}  // namespace megapad::guest
