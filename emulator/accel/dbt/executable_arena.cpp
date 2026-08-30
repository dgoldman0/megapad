#include "executable_arena.h"

#include "host_jit_config.h"

#include <algorithm>
#include <atomic>
#include <cstring>
#include <limits>
#include <new>

#if MP64_HAS_X86_64_JIT
#include <cerrno>
#if __has_include(<linux/memfd.h>)
#include <linux/memfd.h>
#endif
#include <sys/mman.h>
#include <unistd.h>
#ifndef MFD_CLOEXEC
#define MFD_CLOEXEC 0x0001U
#endif
#ifndef MFD_EXEC
#define MFD_EXEC 0x0010U
#endif
#endif

HostExecutableArena::~HostExecutableArena() {
    reset();
}

bool HostExecutableArena::ensure(
        std::size_t slot_count,
        bool& allocation_attempted,
        bool& allocated) {
    allocation_attempted = false;
    allocated = false;
#if MP64_HAS_X86_64_JIT
    if (state_ == State::READY)
        return slot_count == slot_count_;
    if (state_ == State::FAILED)
        return false;

    allocation_attempted = true;
    state_ = State::FAILED;
    if (slot_count == 0)
        return false;
    if (
        slot_count >
        std::numeric_limits<std::size_t>::max() /
            FIXED_SLOT_BYTES
    ) {
        return false;
    }
    const std::size_t mapped_bytes =
        slot_count * FIXED_SLOT_BYTES;
    if (
        mapped_bytes >
        static_cast<std::size_t>(
            std::numeric_limits<off_t>::max())
    ) {
        return false;
    }

    try {
        published_slot_sizes_.assign(slot_count, 0);
    } catch (const std::bad_alloc&) {
        published_slot_sizes_.clear();
        return false;
    }

    int descriptor = static_cast<int>(::syscall(
        SYS_memfd_create,
        "mp64-single-core-jit",
        MFD_CLOEXEC | MFD_EXEC));
    if (descriptor < 0 && errno == EINVAL) {
        descriptor = static_cast<int>(::syscall(
            SYS_memfd_create,
            "mp64-single-core-jit",
            MFD_CLOEXEC));
    }
    if (descriptor < 0) {
        published_slot_sizes_.clear();
        return false;
    }
    if (
        ::ftruncate(
            descriptor,
            static_cast<off_t>(mapped_bytes)) != 0
    ) {
        ::close(descriptor);
        published_slot_sizes_.clear();
        return false;
    }

    void* writable = ::mmap(
        nullptr,
        mapped_bytes,
        PROT_READ | PROT_WRITE,
        MAP_SHARED,
        descriptor,
        0);
    if (writable == MAP_FAILED) {
        ::close(descriptor);
        published_slot_sizes_.clear();
        return false;
    }
    void* executable = ::mmap(
        nullptr,
        mapped_bytes,
        PROT_READ | PROT_EXEC,
        MAP_SHARED,
        descriptor,
        0);
    ::close(descriptor);
    if (executable == MAP_FAILED) {
        ::munmap(writable, mapped_bytes);
        published_slot_sizes_.clear();
        return false;
    }

    writable_ = static_cast<uint8_t*>(writable);
    executable_ = static_cast<uint8_t*>(executable);
    slot_count_ = slot_count;
    slot_bytes_ = FIXED_SLOT_BYTES;
    mapped_bytes_ = mapped_bytes;
    state_ = State::READY;
    allocated = true;
    return true;
#else
    (void)slot_count;
    return false;
#endif
}

HostExecutableCode HostExecutableArena::publish(
        std::size_t slot,
        const std::vector<uint8_t>& code,
        bool& rewrote_slot) noexcept {
    rewrote_slot = false;
#if MP64_HAS_X86_64_JIT
    if (
        state_ != State::READY ||
        slot >= slot_count_ ||
        code.empty() ||
        code.size() > slot_bytes_
    ) {
        return {};
    }
    const std::size_t offset = slot * slot_bytes_;
    uint8_t* const writable = writable_ + offset;
    uint8_t* const executable = executable_ + offset;
    const std::size_t prior_size = published_slot_sizes_[slot];
    std::memcpy(writable, code.data(), code.size());
    if (prior_size > code.size()) {
        std::memset(
            writable + code.size(),
            0xCC,
            prior_size - code.size());
    }
    std::atomic_thread_fence(std::memory_order_release);
    const std::size_t synchronized_size =
        std::max(prior_size, code.size());
    __builtin___clear_cache(
        reinterpret_cast<char*>(executable),
        reinterpret_cast<char*>(
            executable + synchronized_size));
    rewrote_slot = prior_size != 0;
    published_slot_sizes_[slot] = code.size();
    return HostExecutableCode(executable);
#else
    (void)slot;
    (void)code;
    return {};
#endif
}

bool HostExecutableArena::ready() const noexcept {
    return state_ == State::READY;
}

bool HostExecutableArena::failed() const noexcept {
    return state_ == State::FAILED;
}

std::size_t HostExecutableArena::slot_count() const noexcept {
    return slot_count_;
}

std::size_t HostExecutableArena::slot_bytes() const noexcept {
    return slot_bytes_;
}

std::size_t HostExecutableArena::mapped_bytes() const noexcept {
    return mapped_bytes_;
}

void HostExecutableArena::reset() noexcept {
#if MP64_HAS_X86_64_JIT
    if (writable_ != nullptr)
        ::munmap(writable_, mapped_bytes_);
    if (executable_ != nullptr)
        ::munmap(executable_, mapped_bytes_);
#endif
    writable_ = nullptr;
    executable_ = nullptr;
    slot_count_ = 0;
    slot_bytes_ = 0;
    mapped_bytes_ = 0;
    published_slot_sizes_.clear();
    state_ = State::UNINITIALIZED;
}
