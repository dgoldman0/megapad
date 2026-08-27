#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

// One non-owning reference into the system-owned exact-single JIT arena.
// Only the arena owns and ultimately unmaps executable storage.
class HostExecutableCode {
public:
    HostExecutableCode() = default;
    explicit HostExecutableCode(void* address) noexcept
        : address_(address) {}
    HostExecutableCode(const HostExecutableCode&) = delete;
    HostExecutableCode& operator=(const HostExecutableCode&) = delete;
    HostExecutableCode(HostExecutableCode&& other) noexcept
        : address_(other.address_) {
        other.address_ = nullptr;
    }
    HostExecutableCode& operator=(HostExecutableCode&& other) noexcept {
        if (this == &other)
            return *this;
        reset();
        address_ = other.address_;
        other.address_ = nullptr;
        return *this;
    }
    ~HostExecutableCode() = default;

    explicit operator bool() const noexcept {
        return address_ != nullptr;
    }
    void* address() const noexcept {
        return address_;
    }
    void reset() noexcept {
        address_ = nullptr;
    }

private:
    void* address_ = nullptr;
};

// Exact-single execution owns one direct-mapped decoded-block table. Each
// table index gets one stable, cache-line-aligned slot in this bounded dense
// arena, so replacement code never allocates or changes VMA permissions on
// the hot path. Linux maps the same memfd through separate RW and RX aliases;
// no virtual mapping is writable and executable at the same time. Publication
// requires exclusive native-scheduler ownership. The arena is process-local:
// construct fresh system state after fork rather than using inherited aliases.
class HostExecutableArena {
public:
    static constexpr std::size_t SLOT_CACHE_LINE_BYTES = 64;
    static constexpr std::size_t SLOT_CACHE_LINES = 21;
    static constexpr std::size_t FIXED_SLOT_BYTES =
        SLOT_CACHE_LINE_BYTES * SLOT_CACHE_LINES;
    static_assert(
        FIXED_SLOT_BYTES % 16 == 0 &&
        (SLOT_CACHE_LINES & 1U) != 0,
        "dense x86-64 JIT slots must remain aligned and set-rotating");

    HostExecutableArena() = default;
    HostExecutableArena(const HostExecutableArena&) = delete;
    HostExecutableArena& operator=(const HostExecutableArena&) = delete;
    ~HostExecutableArena();

    bool ensure(
        std::size_t slot_count,
        bool& allocation_attempted,
        bool& allocated);
    HostExecutableCode publish(
        std::size_t slot,
        const std::vector<uint8_t>& code,
        bool& rewrote_slot) noexcept;

    bool ready() const noexcept;
    bool failed() const noexcept;
    std::size_t slot_count() const noexcept;
    std::size_t slot_bytes() const noexcept;
    std::size_t mapped_bytes() const noexcept;

private:
    enum class State : uint8_t {
        UNINITIALIZED = 0,
        READY = 1,
        FAILED = 2,
    };

    void reset() noexcept;

    State state_ = State::UNINITIALIZED;
    uint8_t* writable_ = nullptr;
    uint8_t* executable_ = nullptr;
    std::size_t slot_count_ = 0;
    std::size_t slot_bytes_ = 0;
    std::size_t mapped_bytes_ = 0;
    std::vector<std::size_t> published_slot_sizes_;
};
