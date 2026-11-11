/*
 * The interface between MegaPad's model of a chiplet port and the model of
 * the chiplet behind it.
 *
 * It is plain C so that a chiplet's model can be built from its own
 * repository, with its own compiler, and loaded at run time. What crosses
 * is what crosses the link (docs/chiplet-link.md): memory requests,
 * doorbells and the sideband. The port bridge on MegaPad's side checks
 * every memory request against the ranges MegaPad has lent, exactly as the
 * hardware bridge does, so a module is held to the same bounds as a chip.
 *
 * Calling rules:
 *   - The bridge calls the module on one thread, never re-entrantly.
 *   - A module calls the host's services only from inside its `step`, and
 *     starts no thread and reads no clock of its own: its results then
 *     depend on the machine's state, its own state and the order of steps.
 *   - `identity` works at any time, in reset included. Between `reset` and
 *     the next `release` a module holds no request and makes no call.
 */
#ifndef MP64_CHIPLET_MODULE_H
#define MP64_CHIPLET_MODULE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MP64_CHIPLET_ABI 1

/* Response codes of a memory request, as on the link. */
#define MP64_CHIPLET_OK        0
#define MP64_CHIPLET_REFUSED   1
#define MP64_CHIPLET_MEM_ERROR 2
#define MP64_CHIPLET_CANCELLED 3

/* What the port bridge offers the module. */
typedef struct mp64_chiplet_host {
    void *ctx;

    /*
     * One memory request. `size` is 0 for a byte, 1 a half-word, 2 a word,
     * 3 a double word. `len` is the count of 64-bit words beyond the first,
     * 0 to 255; a burst (len > 0) uses size 3 and an address aligned to 8.
     * `data` holds len + 1 words. A read fills each with the aligned 64-bit
     * word that contains the address; a write takes each, a sub-word write
     * from the low bits. `done`, when given, receives the count of words
     * completed. The bridge admits or refuses the whole request at its
     * start. Returns a response code.
     */
    int (*mem)(void *ctx, uint64_t addr, unsigned size, unsigned len,
               int wen, uint64_t *data, unsigned *done);

    /* Ring the participant with ID `dst` as this port. */
    void (*doorbell)(void *ctx, uint8_t dst);

    /* The sideband's ready and stopped signals. */
    void (*status)(void *ctx, int ready, int stopped);
} mp64_chiplet_host;

/* What a chiplet's model offers the port bridge. */
typedef struct mp64_chiplet_module {
    uint32_t abi;               /* MP64_CHIPLET_ABI */
    void *self;

    /* The host's services, given once when the module is attached to a
       port. The pointer stays valid until `destroy`. */
    void (*bind)(void *self, const mp64_chiplet_host *host);

    /* One word of the identity record. */
    uint64_t (*identity)(void *self, uint8_t index);

    /* MegaPad holds the chiplet in reset: drop all work in hand. */
    void (*reset)(void *self);

    /* MegaPad releases the chiplet, naming its boot image. */
    void (*release)(void *self, uint64_t boot_addr, uint64_t boot_len);

    /* The sideband's stop request changed. */
    void (*stop)(void *self, int stop_req);

    /* A participant has rung this port. Returns nonzero when the module
       takes the doorbell; one left untaken stays pending and is offered
       again before the next step. */
    int (*doorbell)(void *self, uint8_t src);

    /* Do up to `budget` units of work, a unit being the module's own
       measure. Returns nonzero while more work waits. */
    int (*step)(void *self, uint32_t budget);

    /* Write the module's whole state to `buffer` when `capacity` holds it.
       Returns the size the state needs. */
    size_t (*save)(void *self, uint8_t *buffer, size_t capacity);

    /* Take back a state that `save` wrote. Returns 0 on success, and
       leaves the module in reset on failure. */
    int (*restore)(void *self, const uint8_t *buffer, size_t length);

    void (*destroy)(void *self);
} mp64_chiplet_module;

/*
 * The one function a module library exports. `config` is a string the
 * module defines, and may be NULL. Returns 0 and fills `out` on success.
 */
typedef int (*mp64_chiplet_create_fn)(const char *config,
                                      mp64_chiplet_module *out);
#define MP64_CHIPLET_CREATE_SYMBOL "mp64_chiplet_create"

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* MP64_CHIPLET_MODULE_H */
