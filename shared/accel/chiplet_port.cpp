#include "chiplet_port.h"

#include <cstring>
#include <utility>

#include <dlfcn.h>

#if !defined(__BYTE_ORDER__) || __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error "The chiplet model copies guest words directly and needs a little-endian host."
#endif

namespace megapad::chiplet {

namespace {

constexpr std::uint32_t SAVE_MAGIC = 0x5043504D;  // "MPCP"
constexpr std::uint32_t SAVE_VERSION = 1;
constexpr std::uint32_t WINDOW_MAGIC = 0x5743504D;  // "MPCW"

unsigned range_sel_mask_for(int range_count) noexcept {
    // RANGE_SEL holds ceil(log2(ranges)) bits, at least one, as the RTL's
    // RANGE_BITS does.
    unsigned bits = 1;
    while ((1 << bits) < range_count)
        ++bits;
    return (1u << bits) - 1;
}

class Writer {
public:
    void u8(std::uint8_t value) { bytes_.push_back(value); }
    void u32(std::uint32_t value) { raw(&value, sizeof value); }
    void u64(std::uint64_t value) { raw(&value, sizeof value); }
    void raw(const void* data, std::size_t size) {
        const auto* bytes = static_cast<const std::uint8_t*>(data);
        bytes_.insert(bytes_.end(), bytes, bytes + size);
    }
    std::vector<std::uint8_t> take() { return std::move(bytes_); }

private:
    std::vector<std::uint8_t> bytes_;
};

class Reader {
public:
    explicit Reader(const std::vector<std::uint8_t>& bytes) noexcept
        : bytes_(bytes) {}
    bool u8(std::uint8_t& value) noexcept { return raw(&value, 1); }
    bool u32(std::uint32_t& value) noexcept {
        return raw(&value, sizeof value);
    }
    bool u64(std::uint64_t& value) noexcept {
        return raw(&value, sizeof value);
    }
    bool raw(void* out, std::size_t size) noexcept {
        if (size > bytes_.size() - at_)
            return false;
        std::memcpy(out, bytes_.data() + at_, size);
        at_ += size;
        return true;
    }
    const std::uint8_t* here() const noexcept { return bytes_.data() + at_; }
    bool skip(std::size_t size) noexcept {
        if (size > bytes_.size() - at_)
            return false;
        at_ += size;
        return true;
    }
    bool done() const noexcept { return at_ == bytes_.size(); }

private:
    const std::vector<std::uint8_t>& bytes_;
    std::size_t at_ = 0;
};

int host_mem(void* ctx, std::uint64_t addr, unsigned size, unsigned len,
             int wen, std::uint64_t* data, unsigned* done) {
    return static_cast<ChipletPort*>(ctx)->request(
        addr, size, len, wen != 0, data, done);
}

void host_doorbell(void* ctx, std::uint8_t dst) {
    static_cast<ChipletPort*>(ctx)->ring(dst);
}

void host_status(void* ctx, int ready, int stopped) {
    static_cast<ChipletPort*>(ctx)->set_status(ready != 0, stopped != 0);
}

}  // namespace

// ---------------------------------------------------------------------------
//  Port bridge
// ---------------------------------------------------------------------------

ChipletPort::ChipletPort(int index, int id, int range_count,
                         ExternalMemory* memory, PortMailbox* mailbox)
    : index_(index),
      id_(id),
      memory_(memory),
      mailbox_(mailbox),
      range_sel_mask_(range_sel_mask_for(range_count)),
      ranges_(static_cast<std::size_t>(range_count < 1 ? 1 : range_count)) {
    host_.ctx = this;
    host_.mem = host_mem;
    host_.doorbell = host_doorbell;
    host_.status = host_status;
}

ChipletPort::~ChipletPort() {
    destroy_module();
}

void ChipletPort::destroy_module() noexcept {
    if (!attached_)
        return;
    attached_ = false;
    if (module_.destroy != nullptr)
        module_.destroy(module_.self);
    module_ = mp64_chiplet_module{};
}

bool ChipletPort::attach(const mp64_chiplet_module& module) noexcept {
    if (module.abi != MP64_CHIPLET_ABI || module.bind == nullptr ||
        module.identity == nullptr || module.reset == nullptr ||
        module.release == nullptr || module.stop == nullptr ||
        module.doorbell == nullptr || module.step == nullptr ||
        module.save == nullptr || module.restore == nullptr)
        return false;
    detach();
    module_ = module;
    attached_ = true;
    module_.bind(module_.self, &host_);
    // A chiplet attached to a released port starts as one released there.
    if (!reset_hold_) {
        module_.release(module_.self, boot_addr_, boot_len_);
        if (stop_req_)
            module_.stop(module_.self, 1);
        more_work_ = true;
    }
    return true;
}

void ChipletPort::detach() noexcept {
    destroy_module();
    ready_ = false;
    stopped_ = false;
    id_done_ = false;
    more_work_ = false;
    doorbell_declined_ = false;
}

void ChipletPort::enter_reset() noexcept {
    // A chiplet in reset drives neither ready nor stopped and holds no
    // work. Doorbells that wait for the port stay pending in the mailbox.
    ready_ = false;
    stopped_ = false;
    more_work_ = false;
    doorbell_declined_ = false;
    if (attached_)
        module_.reset(module_.self);
}

std::uint64_t ChipletPort::read_register(unsigned index) const noexcept {
    const bool range_valid = range_sel_ < ranges_.size();
    switch (index) {
        case CTRL:
            return (enable_ ? CTRL_ENABLE : 0) |
                   (reset_hold_ ? CTRL_RESET_HOLD : 0) |
                   (stop_req_ ? CTRL_STOP_REQ : 0) |
                   (doorbell_en_ ? CTRL_DOORBELL_EN : 0) |
                   (attn_en_ ? CTRL_ATTN_EN : 0) |
                   (static_cast<std::uint64_t>(attn_core_) << 8);
        case STATUS:
            return (attached_ ? STATUS_PRESENT : 0) |
                   (stopped_ ? STATUS_STOPPED : 0) |
                   (ready_ ? STATUS_READY : 0) |
                   (fault_ ? STATUS_FAULT : 0) |
                   (id_done_ ? STATUS_ID_DONE : 0) |
                   (static_cast<std::uint64_t>(id_ & 0xFF) << 16) |
                   (static_cast<std::uint64_t>(ranges_.size() & 0xFF) << 32);
        case FAULT_ADDR:
            return fault_addr_;
        case FAULT_INFO:
            return static_cast<std::uint64_t>(fault_size_ & 3) |
                   (fault_wen_ ? 1u << 2 : 0) |
                   (static_cast<std::uint64_t>(fault_why_ & 0xF) << 4) |
                   (static_cast<std::uint64_t>(fault_len_) << 8) |
                   (static_cast<std::uint64_t>(refusals_) << 16);
        case RANGE_SEL:
            return range_sel_;
        case RANGE_BASE:
            return range_valid ? ranges_[range_sel_].base : 0;
        case RANGE_LIMIT:
            return range_valid ? ranges_[range_sel_].limit : 0;
        case RANGE_RIGHTS:
            return range_valid ? ranges_[range_sel_].rights : 0;
        case QOS:
            return static_cast<std::uint64_t>(qos_weight_) |
                   (static_cast<std::uint64_t>(qos_limit_) << 16);
        case ID_INDEX:
            return id_index_;
        case ID_DATA:
            return id_data_;
        case BOOT_ADDR:
            return boot_addr_;
        case BOOT_LEN:
            return boot_len_;
        default:
            return 0;
    }
}

void ChipletPort::write_register(unsigned index,
                                 std::uint64_t value) noexcept {
    switch (index) {
        case CTRL: {
            const bool was_up = link_up();
            const bool was_stop = stop_req_;
            enable_ = (value & CTRL_ENABLE) != 0;
            reset_hold_ = (value & CTRL_RESET_HOLD) != 0;
            stop_req_ = (value & CTRL_STOP_REQ) != 0;
            doorbell_en_ = (value & CTRL_DOORBELL_EN) != 0;
            attn_en_ = (value & CTRL_ATTN_EN) != 0;
            attn_core_ = static_cast<std::uint8_t>(value >> 8);
            const bool now_up = link_up();
            if (was_up && !now_up) {
                enter_reset();
            } else if (!was_up && now_up) {
                module_.release(module_.self, boot_addr_, boot_len_);
                if (stop_req_)
                    module_.stop(module_.self, 1);
                // A released chiplet has its start to do, so the port
                // counts as holding work until the chiplet has stepped.
                more_work_ = true;
            } else if (now_up && was_stop != stop_req_) {
                module_.stop(module_.self, stop_req_ ? 1 : 0);
                // The chiplet answers a changed stop request in a step.
                more_work_ = true;
            }
            break;
        }
        case STATUS:
            if ((value & STATUS_FAULT) != 0) {
                fault_ = false;
                refusals_ = 0;
            }
            break;
        case RANGE_SEL:
            range_sel_ = static_cast<unsigned>(value) & range_sel_mask_;
            break;
        case RANGE_BASE:
            stage_base_ = value;
            break;
        case RANGE_LIMIT:
            stage_limit_ = value;
            break;
        case RANGE_RIGHTS:
            // The staged base and limit take effect together with the
            // rights, in this one step.
            if (range_sel_ < ranges_.size()) {
                Range& range = ranges_[range_sel_];
                range.base = stage_base_;
                range.limit = stage_limit_;
                range.rights = static_cast<std::uint8_t>(value & 3);
            }
            break;
        case QOS:
            qos_weight_ = static_cast<std::uint8_t>(value);
            qos_limit_ = static_cast<std::uint16_t>(value >> 16);
            break;
        case ID_INDEX:
            // The sideband read completes at once when a chiplet answers,
            // and stays incomplete when none is attached.
            id_index_ = static_cast<std::uint8_t>(value);
            id_done_ = false;
            if (attached_) {
                id_data_ = module_.identity(module_.self, id_index_);
                id_done_ = true;
            }
            break;
        case BOOT_ADDR:
            boot_addr_ = value;
            break;
        case BOOT_LEN:
            boot_len_ = value;
            break;
        default:
            break;
    }
}

unsigned ChipletPort::refusal_reason(std::uint64_t addr, unsigned size,
                                     unsigned len,
                                     bool wen) const noexcept {
    if (!enable_)
        return WHY_CLOSED;
    const bool burst = len != 0;
    // A burst is made of whole 64-bit words.
    const bool aligned = size > 3 || len > 255
        ? false
        : burst ? (size == 3 && (addr & 7) == 0)
                : (addr & ((std::uint64_t{1} << size) - 1)) == 0;
    if (!aligned)
        return WHY_MISALIGNED;
    const std::uint64_t span = burst
        ? (static_cast<std::uint64_t>(len) + 1) * 8
        : std::uint64_t{1} << size;
    // end is exclusive; an access that wraps the address space lies
    // outside every range.
    const std::uint64_t end = addr + span;
    const bool wraps = end < addr;
    if (wraps || addr < memory_->base() || end > memory_->limit())
        return WHY_NOT_EXTERNAL;
    const std::uint8_t right = wen ? 2 : 1;
    for (const Range& range : ranges_) {
        if ((range.rights & right) != 0 && addr >= range.base &&
            end <= range.limit)
            return 0;
    }
    return WHY_NOT_LENT;
}

void ChipletPort::record_refusal(std::uint64_t addr, unsigned size,
                                 unsigned len, bool wen,
                                 unsigned reason) noexcept {
    if (refusals_ != 0xFFFF)
        ++refusals_;
    if (fault_)
        return;
    // The first refusal after each clear is the one recorded.
    fault_ = true;
    fault_addr_ = addr;
    fault_size_ = static_cast<std::uint8_t>(size & 3);
    fault_wen_ = wen;
    fault_why_ = static_cast<std::uint8_t>(reason);
    fault_len_ = static_cast<std::uint8_t>(len);
    if (attn_en_)
        mailbox_->send(index_, attn_core_);
}

int ChipletPort::request(std::uint64_t addr, unsigned size, unsigned len,
                         bool wen, std::uint64_t* data,
                         unsigned* done) noexcept {
    if (done != nullptr)
        *done = 0;
    active_ = true;
    // The bridge ignores the channel while the chiplet is held in reset.
    if (!link_up())
        return MP64_CHIPLET_CANCELLED;
    const unsigned reason = refusal_reason(addr, size, len, wen);
    if (reason != 0) {
        record_refusal(addr, size, len, wen, reason);
        return MP64_CHIPLET_REFUSED;
    }
    if (data == nullptr)
        return MP64_CHIPLET_MEM_ERROR;

    const unsigned words = len + 1;
    auto* bytes = reinterpret_cast<std::uint8_t*>(data);

    if (len == 0) {
        if (wen) {
            if (!memory_->write(addr, bytes, std::size_t{1} << size))
                return MP64_CHIPLET_MEM_ERROR;
        } else {
            // A read returns the aligned 64-bit word that holds the address.
            if (!memory_->read(addr & ~std::uint64_t{7}, bytes, 8)) {
                data[0] = 0;
                return MP64_CHIPLET_MEM_ERROR;
            }
        }
        if (done != nullptr)
            *done = 1;
        return MP64_CHIPLET_OK;
    }

    // A burst moves in one copy. When memory reports an error the burst is
    // replayed word by word, so the words before the failing one stand and
    // none after it is touched, as on the link.
    const std::size_t span = static_cast<std::size_t>(words) * 8;
    const bool moved = wen ? memory_->write(addr, bytes, span)
                           : memory_->read(addr, bytes, span);
    if (moved) {
        if (done != nullptr)
            *done = words;
        return MP64_CHIPLET_OK;
    }
    for (unsigned word = 0; word < words; ++word) {
        const std::uint64_t at = addr + static_cast<std::uint64_t>(word) * 8;
        const bool ok = wen ? memory_->write(at, bytes + word * 8, 8)
                            : memory_->read(at, bytes + word * 8, 8);
        if (!ok) {
            if (!wen) {
                for (unsigned rest = word; rest < words; ++rest)
                    data[rest] = 0;
            }
            if (done != nullptr)
                *done = word;
            return MP64_CHIPLET_MEM_ERROR;
        }
    }
    // Every word went through singly: the error was transient.
    if (done != nullptr)
        *done = words;
    return MP64_CHIPLET_OK;
}

void ChipletPort::ring(std::uint8_t dst) noexcept {
    active_ = true;
    // While DOORBELL_EN is clear the bridge takes the chiplet's doorbells
    // and drops them.
    if (!link_up() || !doorbell_en_)
        return;
    mailbox_->send(index_, dst);
}

void ChipletPort::set_status(bool ready, bool stopped) noexcept {
    active_ = true;
    if (!link_up())
        return;
    ready_ = ready;
    stopped_ = stopped;
}

bool ChipletPort::step(std::uint32_t budget) noexcept {
    if (!link_up()) {
        more_work_ = false;
        return false;
    }
    bool declined = false;
    for (;;) {
        const int source = mailbox_->doorbell(index_);
        if (source == MailboxPending::no_source)
            break;
        if (module_.doorbell(module_.self,
                             static_cast<std::uint8_t>(source)) == 0) {
            declined = true;
            break;
        }
        mailbox_->take(index_, source);
    }
    active_ = false;
    more_work_ = module_.step(module_.self, budget) != 0;
    // A chiplet that declined a doorbell and then used the link may take
    // the doorbell next time, so it is offered again. One that declined and
    // did nothing waits for MegaPad, and the doorbell waits with it.
    doorbell_declined_ = declined && !active_;
    return work_waits();
}

bool ChipletPort::work_waits() const noexcept {
    if (!link_up())
        return false;
    if (more_work_)
        return true;
    // A waiting doorbell is work, unless the chiplet declined it in its
    // last step and did nothing else there.
    return !doorbell_declined_ &&
        mailbox_->doorbell(index_) != MailboxPending::no_source;
}

std::vector<std::uint8_t> ChipletPort::save() const {
    Writer out;
    out.u32(SAVE_MAGIC);
    out.u32(SAVE_VERSION);
    out.u32(static_cast<std::uint32_t>(ranges_.size()));
    out.u64(read_register(CTRL));
    out.u32(range_sel_);
    out.u64(stage_base_);
    out.u64(stage_limit_);
    for (const Range& range : ranges_) {
        out.u64(range.base);
        out.u64(range.limit);
        out.u8(range.rights);
    }
    out.u64(read_register(QOS));
    out.u64(boot_addr_);
    out.u64(boot_len_);
    out.u8(fault_ ? 1 : 0);
    out.u64(fault_addr_);
    out.u64(read_register(FAULT_INFO));
    out.u8(id_done_ ? 1 : 0);
    out.u8(id_index_);
    out.u64(id_data_);
    out.u8(ready_ ? 1 : 0);
    out.u8(stopped_ ? 1 : 0);
    out.u8(more_work_ ? 1 : 0);
    out.u8(doorbell_declined_ ? 1 : 0);
    out.u8(attached_ ? 1 : 0);
    if (attached_) {
        const std::size_t size = module_.save(module_.self, nullptr, 0);
        std::vector<std::uint8_t> blob(size);
        module_.save(module_.self, blob.data(), blob.size());
        out.u64(blob.size());
        out.raw(blob.data(), blob.size());
    }
    return out.take();
}

bool ChipletPort::restore(const std::vector<std::uint8_t>& state) noexcept {
    Reader in(state);
    std::uint32_t magic = 0, version = 0, range_count = 0, range_sel = 0;
    std::uint64_t ctrl = 0, stage_base = 0, stage_limit = 0;
    if (!in.u32(magic) || !in.u32(version) || !in.u32(range_count) ||
        magic != SAVE_MAGIC || version != SAVE_VERSION ||
        range_count != ranges_.size())
        return false;
    if (!in.u64(ctrl) || !in.u32(range_sel) || !in.u64(stage_base) ||
        !in.u64(stage_limit))
        return false;
    std::vector<Range> ranges(ranges_.size());
    for (Range& range : ranges) {
        if (!in.u64(range.base) || !in.u64(range.limit) ||
            !in.u8(range.rights))
            return false;
    }
    std::uint64_t qos = 0, boot_addr = 0, boot_len = 0, fault_addr = 0,
                  fault_info = 0, id_data = 0, blob_size = 0;
    std::uint8_t fault = 0, id_done = 0, id_index = 0, ready = 0,
                 stopped = 0, more = 0, declined = 0, attached = 0;
    if (!in.u64(qos) || !in.u64(boot_addr) || !in.u64(boot_len) ||
        !in.u8(fault) || !in.u64(fault_addr) || !in.u64(fault_info) ||
        !in.u8(id_done) || !in.u8(id_index) || !in.u64(id_data) ||
        !in.u8(ready) || !in.u8(stopped) || !in.u8(more) ||
        !in.u8(declined) || !in.u8(attached))
        return false;
    // The state names a chiplet exactly when one is attached here.
    if ((attached != 0) != attached_)
        return false;
    const std::uint8_t* blob = nullptr;
    if (attached != 0) {
        if (!in.u64(blob_size))
            return false;
        blob = in.here();
        if (!in.skip(static_cast<std::size_t>(blob_size)))
            return false;
    }
    if (!in.done())
        return false;
    if (attached != 0 &&
        module_.restore(module_.self, blob,
                        static_cast<std::size_t>(blob_size)) != 0) {
        // The module is in reset now, so the port holds it there.
        enable_ = false;
        reset_hold_ = true;
        ready_ = false;
        stopped_ = false;
        more_work_ = false;
        doorbell_declined_ = false;
        return false;
    }

    enable_ = (ctrl & CTRL_ENABLE) != 0;
    reset_hold_ = (ctrl & CTRL_RESET_HOLD) != 0;
    stop_req_ = (ctrl & CTRL_STOP_REQ) != 0;
    doorbell_en_ = (ctrl & CTRL_DOORBELL_EN) != 0;
    attn_en_ = (ctrl & CTRL_ATTN_EN) != 0;
    attn_core_ = static_cast<std::uint8_t>(ctrl >> 8);
    range_sel_ = range_sel & range_sel_mask_;
    stage_base_ = stage_base;
    stage_limit_ = stage_limit;
    ranges_ = std::move(ranges);
    qos_weight_ = static_cast<std::uint8_t>(qos);
    qos_limit_ = static_cast<std::uint16_t>(qos >> 16);
    boot_addr_ = boot_addr;
    boot_len_ = boot_len;
    fault_ = fault != 0;
    fault_addr_ = fault_addr;
    fault_size_ = static_cast<std::uint8_t>(fault_info & 3);
    fault_wen_ = (fault_info & 4) != 0;
    fault_why_ = static_cast<std::uint8_t>((fault_info >> 4) & 0xF);
    fault_len_ = static_cast<std::uint8_t>(fault_info >> 8);
    refusals_ = static_cast<std::uint16_t>(fault_info >> 16);
    id_done_ = id_done != 0;
    id_index_ = id_index;
    id_data_ = id_data;
    ready_ = ready != 0;
    stopped_ = stopped != 0;
    more_work_ = more != 0;
    doorbell_declined_ = declined != 0;
    return true;
}

// ---------------------------------------------------------------------------
//  Register window
// ---------------------------------------------------------------------------

ChipletWindow::ChipletWindow(int port_count, int first_port_id,
                             int range_count, ExternalMemory* memory,
                             PortMailbox* mailbox)
    : first_port_id_(first_port_id),
      range_count_(range_count < 1 ? 1 : range_count) {
    for (int port = 0; port < port_count; ++port) {
        ports_.push_back(std::make_unique<ChipletPort>(
            port, first_port_id + port, range_count_, memory, mailbox));
        budgets_.push_back(DEFAULT_BUDGET);
    }
}

std::uint64_t ChipletWindow::port_count_word() const noexcept {
    return static_cast<std::uint64_t>(ports_.size() & 0xFF) |
           (static_cast<std::uint64_t>(first_port_id_ & 0xFF) << 8) |
           (static_cast<std::uint64_t>(range_count_ & 0xFF) << 16);
}

std::uint64_t ChipletWindow::read64(unsigned offset) const noexcept {
    if (offset >= PORT_BLOCK) {
        // A PORT_SEL beyond the ports built selects no port.
        if (port_sel_ >= ports_.size())
            return 0;
        return ports_[port_sel_]->read_register((offset - PORT_BLOCK) / 8);
    }
    switch (offset) {
        case PORT_COUNT:
            return port_count_word();
        case PORT_SEL:
            return port_sel_;
        case EXT_CORE_QOS:
            return ext_core_qos_;
        default:
            return 0;
    }
}

void ChipletWindow::write64(unsigned offset, std::uint64_t value) noexcept {
    if (offset >= PORT_BLOCK) {
        if (port_sel_ < ports_.size())
            ports_[port_sel_]->write_register(
                (offset - PORT_BLOCK) / 8, value);
        return;
    }
    switch (offset) {
        case PORT_SEL:
            port_sel_ = static_cast<std::uint8_t>(value);
            break;
        case EXT_CORE_QOS:
            ext_core_qos_ = static_cast<std::uint8_t>(value);
            break;
        default:
            break;
    }
}

void ChipletWindow::set_budget(int port, std::uint32_t budget) noexcept {
    if (port >= 0 && port < port_count())
        budgets_[static_cast<std::size_t>(port)] = budget;
}

bool ChipletWindow::step() noexcept {
    bool more = false;
    for (std::size_t port = 0; port < ports_.size(); ++port)
        more = ports_[port]->step(budgets_[port]) || more;
    return more;
}

bool ChipletWindow::work_waits() const noexcept {
    for (const auto& port : ports_)
        if (port->work_waits())
            return true;
    return false;
}

std::vector<std::uint8_t> ChipletWindow::save() const {
    Writer out;
    out.u32(WINDOW_MAGIC);
    out.u32(SAVE_VERSION);
    out.u32(static_cast<std::uint32_t>(ports_.size()));
    out.u8(port_sel_);
    out.u8(ext_core_qos_);
    for (std::size_t port = 0; port < ports_.size(); ++port) {
        out.u32(budgets_[port]);
        const std::vector<std::uint8_t> state = ports_[port]->save();
        out.u64(state.size());
        out.raw(state.data(), state.size());
    }
    return out.take();
}

bool ChipletWindow::restore(const std::vector<std::uint8_t>& state) noexcept {
    Reader in(state);
    std::uint32_t magic = 0, version = 0, port_count = 0;
    std::uint8_t port_sel = 0, ext_core_qos = 0;
    if (!in.u32(magic) || !in.u32(version) || !in.u32(port_count) ||
        magic != WINDOW_MAGIC || version != SAVE_VERSION ||
        port_count != ports_.size() || !in.u8(port_sel) ||
        !in.u8(ext_core_qos))
        return false;
    // Parse the whole state before changing any port. A port that then
    // refuses its state leaves the ports before it restored.
    std::vector<std::uint32_t> budgets(ports_.size());
    std::vector<std::vector<std::uint8_t>> states(ports_.size());
    for (std::size_t port = 0; port < ports_.size(); ++port) {
        std::uint64_t size = 0;
        if (!in.u32(budgets[port]) || !in.u64(size))
            return false;
        const std::uint8_t* bytes = in.here();
        if (!in.skip(static_cast<std::size_t>(size)))
            return false;
        states[port].assign(bytes, bytes + size);
    }
    if (!in.done())
        return false;
    for (std::size_t port = 0; port < ports_.size(); ++port)
        if (!ports_[port]->restore(states[port]))
            return false;
    port_sel_ = port_sel;
    ext_core_qos_ = ext_core_qos;
    budgets_ = std::move(budgets);
    return true;
}

// ---------------------------------------------------------------------------
//  Loadable modules
// ---------------------------------------------------------------------------

bool load_module(const char* path, const char* config,
                 mp64_chiplet_module* out, std::string* error) {
    const auto fail = [error](const std::string& why) {
        if (error != nullptr)
            *error = why;
        return false;
    };
    if (path == nullptr || out == nullptr)
        return fail("a module path and a destination are required");
    void* library = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (library == nullptr) {
        const char* why = dlerror();
        return fail(why != nullptr ? why : "the library did not load");
    }
    const auto create = reinterpret_cast<mp64_chiplet_create_fn>(
        dlsym(library, MP64_CHIPLET_CREATE_SYMBOL));
    if (create == nullptr) {
        dlclose(library);
        return fail("the library exports no " MP64_CHIPLET_CREATE_SYMBOL);
    }
    mp64_chiplet_module module{};
    if (create(config, &module) != 0) {
        dlclose(library);
        return fail("the module refused its configuration");
    }
    if (module.abi != MP64_CHIPLET_ABI) {
        if (module.destroy != nullptr)
            module.destroy(module.self);
        dlclose(library);
        return fail("the module was built for another interface version");
    }
    // The module's code must outlive the module, so the library stays.
    *out = module;
    return true;
}

}  // namespace megapad::chiplet
