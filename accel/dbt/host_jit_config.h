#pragma once

// Keep host-code availability identical in every translation unit that owns
// or consumes JIT state. Sanitized builds deliberately use the portable
// execution path: generated code is not instrumented by the active sanitizer.
#if defined(__SANITIZE_ADDRESS__) || \
    defined(__SANITIZE_THREAD__) || \
    defined(__SANITIZE_UNDEFINED__)
#define MP64_SANITIZER_BUILD 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer) || \
    __has_feature(thread_sanitizer) || \
    __has_feature(undefined_behavior_sanitizer)
#define MP64_SANITIZER_BUILD 1
#else
#define MP64_SANITIZER_BUILD 0
#endif
#else
#define MP64_SANITIZER_BUILD 0
#endif

#if defined(__x86_64__) && !defined(__ILP32__) && \
    defined(__linux__) && \
    !MP64_SANITIZER_BUILD
#include <sys/syscall.h>
#if defined(SYS_memfd_create)
#define MP64_HAS_X86_64_JIT 1
#else
#define MP64_HAS_X86_64_JIT 0
#endif
#else
#define MP64_HAS_X86_64_JIT 0
#endif
