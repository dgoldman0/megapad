// The test chiplet: a small chiplet model that exercises every part of the
// link, for the tests of MegaPad's port bridge and as a worked example of
// chiplet_module.h.
//
// Identity: word 0 is "CHIP" over contract version 1, word 1 is "TESTCHIP".
//
// Boot image, five 64-bit words at the address MegaPad names:
//   0  magic "TESTBOOT"
//   1  command address
//   2  reply address
//   3  probe address, one MegaPad has lent no write right for
//   4  reserved
//
// After release it fetches the image in one burst and raises ready. For
// each doorbell it then serves one command for the participant that rang:
//   1. read the command word at the command address; its low byte is n;
//   2. write the complement of the command word at the reply address;
//   3. write the count of commands served, one byte, at reply + 8;
//   4. try to write at the probe address, and
//   5. store that request's response code as a word at reply + 16;
//   6. when n > 0, read n words from command + 8 in one burst and
//   7. write them at reply + 24 in one burst;
//   8. ring the participant that rang.
// A unit of work is one of these link operations, so a command can span
// several steps. Asked to stop, it abandons the command in hand, reports
// stopped and takes no doorbell until the request is withdrawn.
//
// The file builds two ways: into MegaPad's extensions, where the creating
// function has an internal name, and alone as a loadable module, where it
// is exported as mp64_chiplet_create.

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <new>

#include "chiplet_module.h"

namespace {

constexpr std::uint64_t IDENTITY_0 = 0x4348495000000001ull;  // "CHIP", v1
constexpr std::uint64_t IDENTITY_1 = 0x5049484354534554ull;  // "TESTCHIP"
constexpr std::uint64_t BOOT_MAGIC = 0x544F4F4254534554ull;  // "TESTBOOT"
constexpr unsigned IMAGE_WORDS = 5;

enum class Phase : std::uint8_t {
    Reset,      // held in reset
    Boot,       // released, image still to fetch
    Failed,     // the image was refused or malformed
    Running,
};

struct TestChiplet {
    const mp64_chiplet_host* host = nullptr;

    // Everything below is the saved state.
    Phase phase = Phase::Reset;
    std::uint8_t stop_req = 0;
    std::uint8_t stopped = 0;
    std::uint64_t boot_addr = 0;
    std::uint64_t boot_len = 0;
    std::uint64_t command_addr = 0;
    std::uint64_t reply_addr = 0;
    std::uint64_t probe_addr = 0;
    std::uint64_t waiting[4] = {0, 0, 0, 0};  // senders by ID, one bit each
    std::uint8_t serving = 0;       // a command is in hand
    std::uint8_t sender = 0;        // who rang for it
    std::uint8_t stage = 0;         // next operation of the command, 1 to 8
    std::uint64_t command = 0;
    std::uint64_t probe_code = 0;
    std::uint64_t served = 0;
    std::uint64_t words[255] = {};  // the burst a command carries
};

constexpr std::size_t SAVED_FROM = offsetof(TestChiplet, phase);
constexpr std::size_t SAVED_SIZE = sizeof(TestChiplet) - SAVED_FROM;

void clear_work(TestChiplet& chip) {
    for (std::uint64_t& word : chip.waiting)
        word = 0;
    chip.serving = 0;
    chip.stage = 0;
}

void bind(void* self, const mp64_chiplet_host* host) {
    static_cast<TestChiplet*>(self)->host = host;
}

std::uint64_t identity(void*, std::uint8_t index) {
    return index == 0 ? IDENTITY_0 : index == 1 ? IDENTITY_1 : 0;
}

void reset(void* self) {
    auto& chip = *static_cast<TestChiplet*>(self);
    chip.phase = Phase::Reset;
    chip.stop_req = 0;
    chip.stopped = 0;
    clear_work(chip);
}

// The test chiplet takes its addresses from its image and has no use for
// the boot argument, which names the control block of the queue layer.
void release(void* self, std::uint64_t boot_addr, std::uint64_t boot_len,
             std::uint64_t) {
    auto& chip = *static_cast<TestChiplet*>(self);
    chip.phase = Phase::Boot;
    chip.stop_req = 0;
    chip.stopped = 0;
    chip.boot_addr = boot_addr;
    chip.boot_len = boot_len;
    chip.served = 0;
    clear_work(chip);
}

void stop(void* self, int stop_req) {
    static_cast<TestChiplet*>(self)->stop_req = stop_req != 0;
}

int doorbell(void* self, std::uint8_t src) {
    auto& chip = *static_cast<TestChiplet*>(self);
    // A doorbell that arrives before the runtime is up, or while it is
    // stopped, stays pending at MegaPad.
    if (chip.phase != Phase::Running || chip.stop_req || chip.stopped)
        return 0;
    chip.waiting[src / 64] |= std::uint64_t{1} << (src % 64);
    return 1;
}

bool next_sender(TestChiplet& chip) {
    for (unsigned word = 0; word < 4; ++word) {
        if (chip.waiting[word] == 0)
            continue;
        const unsigned bit =
            static_cast<unsigned>(__builtin_ctzll(chip.waiting[word]));
        chip.waiting[word] &= ~(std::uint64_t{1} << bit);
        chip.sender = static_cast<std::uint8_t>(word * 64 + bit);
        chip.serving = 1;
        chip.stage = 1;
        return true;
    }
    return false;
}

bool work_waits(const TestChiplet& chip) {
    if (chip.phase == Phase::Boot)
        return true;
    if (chip.phase != Phase::Running)
        return false;
    if (chip.stop_req != chip.stopped)
        return true;
    if (chip.stopped)
        return false;
    return chip.serving || chip.waiting[0] || chip.waiting[1] ||
           chip.waiting[2] || chip.waiting[3];
}

// One link operation of the command in hand.
void serve_stage(TestChiplet& chip) {
    const mp64_chiplet_host& host = *chip.host;
    const unsigned count = static_cast<unsigned>(chip.command & 0xFF);
    std::uint64_t word = 0;
    switch (chip.stage) {
        case 1:
            host.mem(host.ctx, chip.command_addr, 3, 0, 0, &chip.command,
                     nullptr);
            break;
        case 2:
            word = ~chip.command;
            host.mem(host.ctx, chip.reply_addr, 3, 0, 1, &word, nullptr);
            break;
        case 3:
            word = chip.served + 1;
            host.mem(host.ctx, chip.reply_addr + 8, 0, 0, 1, &word, nullptr);
            break;
        case 4:
            word = 0x5052;
            chip.probe_code = static_cast<std::uint64_t>(
                host.mem(host.ctx, chip.probe_addr, 3, 0, 1, &word, nullptr));
            break;
        case 5:
            host.mem(host.ctx, chip.reply_addr + 16, 3, 0, 1,
                     &chip.probe_code, nullptr);
            break;
        case 6:
            if (count > 0)
                host.mem(host.ctx, chip.command_addr + 8, 3, count - 1, 0,
                         chip.words, nullptr);
            break;
        case 7:
            if (count > 0)
                host.mem(host.ctx, chip.reply_addr + 24, 3, count - 1, 1,
                         chip.words, nullptr);
            break;
        default:
            host.doorbell(host.ctx, chip.sender);
            chip.served += 1;
            chip.serving = 0;
            chip.stage = 0;
            return;
    }
    chip.stage += 1;
}

int step(void* self, std::uint32_t budget) {
    auto& chip = *static_cast<TestChiplet*>(self);
    const mp64_chiplet_host& host = *chip.host;

    if (chip.phase == Phase::Boot && budget > 0) {
        budget -= 1;
        std::uint64_t image[IMAGE_WORDS] = {};
        const bool fetched =
            chip.boot_len >= IMAGE_WORDS * 8 &&
            host.mem(host.ctx, chip.boot_addr, 3, IMAGE_WORDS - 1, 0, image,
                     nullptr) == MP64_CHIPLET_OK &&
            image[0] == BOOT_MAGIC;
        if (fetched) {
            chip.command_addr = image[1];
            chip.reply_addr = image[2];
            chip.probe_addr = image[3];
            chip.phase = Phase::Running;
            host.status(host.ctx, 1, 0);
        } else {
            chip.phase = Phase::Failed;
        }
    }
    if (chip.phase != Phase::Running)
        return work_waits(chip);

    if (chip.stop_req != chip.stopped) {
        // Stopping abandons the command in hand; the doorbells already
        // taken are dropped with it.
        if (chip.stop_req)
            clear_work(chip);
        chip.stopped = chip.stop_req;
        host.status(host.ctx, 1, chip.stopped);
    }
    while (!chip.stopped && budget > 0) {
        if (!chip.serving && !next_sender(chip))
            break;
        serve_stage(chip);
        budget -= 1;
    }
    return work_waits(chip);
}

std::size_t save(void* self, std::uint8_t* buffer, std::size_t capacity) {
    if (buffer != nullptr && capacity >= SAVED_SIZE)
        std::memcpy(buffer,
                    static_cast<const std::uint8_t*>(self) + SAVED_FROM,
                    SAVED_SIZE);
    return SAVED_SIZE;
}

int restore(void* self, const std::uint8_t* buffer, std::size_t length) {
    if (buffer == nullptr || length != SAVED_SIZE ||
        buffer[0] > static_cast<std::uint8_t>(Phase::Running)) {
        reset(self);
        return 1;
    }
    std::memcpy(static_cast<std::uint8_t*>(self) + SAVED_FROM, buffer,
                SAVED_SIZE);
    return 0;
}

void destroy(void* self) {
    delete static_cast<TestChiplet*>(self);
}

}  // namespace

#ifdef MP64_TEST_CHIPLET_BUILTIN
// Built into one of MegaPad's extensions.
namespace megapad::chiplet {
int create_test_chiplet(const char*, mp64_chiplet_module* out);
}
int megapad::chiplet::create_test_chiplet(const char*,
                                          mp64_chiplet_module* out)
#else
// Built alone, as a module MegaPad loads at run time.
extern "C" __attribute__((visibility("default")))
int mp64_chiplet_create(const char*, mp64_chiplet_module* out)
#endif
{
    auto* chip = new (std::nothrow) TestChiplet();
    if (chip == nullptr || out == nullptr) {
        delete chip;
        return 1;
    }
    out->abi = MP64_CHIPLET_ABI;
    out->self = chip;
    out->bind = bind;
    out->identity = identity;
    out->reset = reset;
    out->release = release;
    out->stop = stop;
    out->doorbell = doorbell;
    out->step = step;
    out->save = save;
    out->restore = restore;
    out->destroy = destroy;
    return 0;
}
