// _mp64_slirp: the machine's user-mode network, through libslirp.
//
// libslirp, the user-mode network QEMU uses, turns the guest's Ethernet
// frames into ordinary connections from the user's own account, with DHCP
// and a DNS forwarder of its own, so the machine reaches the internet
// without administrator rights.  This module runs one libslirp instance on
// a thread of its own: every libslirp call happens there, and the guest's
// side meets it through two queues.  send() queues a frame from the guest,
// and receive() takes the next frame for the guest, waiting without the
// GIL.
//
// Its packet handling shares the process that holds guest memory, so the
// module turns on only what the guest uses.  DHCP, DNS and NAT stay on;
// libslirp's TFTP server, its protocol emulation and IPv6 stay off.  Unless
// the network is opened for local access, the module also refuses the
// guest's packets to the user's own computer and to local addresses:
// loopback, the private, shared, link-local and multicast ranges, the
// reserved ones, and every address of the host's own interfaces.  libslirp's
// own limit, disable_host_loopback, refuses connections to the host's
// 127.0.0.1 through the gateway and is set as well.  A refused frame counts
// as sent, as a firewall's drop does, so the guest's network card reports
// no error for it.
#include <pybind11/pybind11.h>

extern "C" {
#include <slirp/libslirp.h>
}

#include <arpa/inet.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <poll.h>
#include <sys/eventfd.h>
#include <unistd.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <cstring>
#include <deque>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace py = pybind11;

namespace megapad::network {

// The largest frame the guest's network card carries: an Ethernet header
// and 1,500 bytes of IPv4.
constexpr std::size_t kMaxFrame = 1514;
constexpr std::size_t kEthernetHeader = 14;
constexpr std::size_t kIPv4Header = 20;
// Frames for the guest wait here until the guest's card takes them.  TCP
// recovers a frame dropped past this, as it does on a busy real link.
constexpr std::size_t kReceiveLimit = 256;
// How long the host's own addresses are trusted before they are read again.
constexpr std::int64_t kHostAddressesLifetimeMs = 2000;

std::int64_t monotonic_ns() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

std::uint32_t read_be32(const std::uint8_t* bytes) {
    return (std::uint32_t(bytes[0]) << 24) | (std::uint32_t(bytes[1]) << 16) |
           (std::uint32_t(bytes[2]) << 8) | std::uint32_t(bytes[3]);
}

std::uint32_t parse_ipv4(const std::string& text, const char* what) {
    in_addr address{};
    if (inet_pton(AF_INET, text.c_str(), &address) != 1)
        throw py::value_error(std::string(what) + " must be an IPv4 address");
    return ntohl(address.s_addr);
}

in_addr network_order(std::uint32_t address) {
    in_addr result{};
    result.s_addr = htonl(address);
    return result;
}

// Destinations that belong to the user's computer or local network rather
// than the internet.
bool local_destination(std::uint32_t address) {
    struct Range {
        std::uint32_t base;
        int prefix;
    };
    static constexpr Range ranges[] = {
        {0x00000000, 8},   // this network
        {0x0A000000, 8},   // private
        {0x64400000, 10},  // shared address space
        {0x7F000000, 8},   // loopback
        {0xA9FE0000, 16},  // link-local
        {0xAC100000, 12},  // private
        {0xC0000000, 24},  // IETF protocol assignments
        {0xC0A80000, 16},  // private
        {0xC6120000, 15},  // benchmarking networks
        {0xE0000000, 4},   // multicast
        {0xF0000000, 4},   // reserved, with the limited broadcast
    };
    for (const Range& range : ranges) {
        const std::uint32_t mask = ~std::uint32_t(0) << (32 - range.prefix);
        if ((address & mask) == range.base)
            return true;
    }
    return false;
}

class SlirpNetwork {
public:
    SlirpNetwork(const std::string& network, int prefix, const std::string& gateway,
                 const std::string& dhcp_start, const std::string& nameserver,
                 bool local_access)
        : local_access_(local_access) {
        if (prefix < 8 || prefix > 30)
            throw py::value_error("the network's prefix must be from 8 to 30 bits");
        netmask_ = ~std::uint32_t(0) << (32 - prefix);
        network_ = parse_ipv4(network, "network");
        if (network_ & ~netmask_)
            throw py::value_error("network has bits outside its prefix");
        const std::uint32_t host = parse_ipv4(gateway, "gateway");
        const std::uint32_t first = parse_ipv4(dhcp_start, "dhcp_start");
        const std::uint32_t dns = parse_ipv4(nameserver, "nameserver");
        for (std::uint32_t address : {host, first, dns})
            if ((address & netmask_) != network_)
                throw py::value_error("gateway, dhcp_start and nameserver must lie in the network");

        wake_ = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
        if (wake_ < 0)
            throw std::runtime_error("the network could not make its wake-up descriptor");

        SlirpConfig config{};
        config.version = 4;
        config.restricted = 0;
        config.in_enabled = true;
        config.vnetwork = network_order(network_);
        config.vnetmask = network_order(netmask_);
        config.vhost = network_order(host);
        config.in6_enabled = false;
        config.vdhcp_start = network_order(first);
        config.vnameserver = network_order(dns);
        config.if_mtu = 1500;
        config.if_mru = 1500;
        config.disable_host_loopback = !local_access;
        config.enable_emu = false;
        config.disable_dns = false;
        config.disable_dhcp = false;
        // tftp_path, bootfile and the IPv6 fields stay empty: no TFTP server.

        callbacks_.send_packet = &SlirpNetwork::send_packet;
        callbacks_.guest_error = &SlirpNetwork::guest_error;
        callbacks_.clock_get_ns = &SlirpNetwork::clock_get_ns;
        callbacks_.timer_free = &SlirpNetwork::timer_free;
        callbacks_.timer_mod = &SlirpNetwork::timer_mod;
        callbacks_.register_poll_fd = &SlirpNetwork::poll_fd_unused;
        callbacks_.unregister_poll_fd = &SlirpNetwork::poll_fd_unused;
        callbacks_.notify = &SlirpNetwork::notify;
        callbacks_.timer_new_opaque = &SlirpNetwork::timer_new_opaque;

        slirp_ = slirp_new(&config, &callbacks_, this);
        if (slirp_ == nullptr) {
            close(wake_);
            throw std::runtime_error("libslirp refused the network's settings");
        }
        thread_ = std::thread([this] { run(); });
    }

    ~SlirpNetwork() { shut(); }

    SlirpNetwork(const SlirpNetwork&) = delete;
    SlirpNetwork& operator=(const SlirpNetwork&) = delete;

    // A frame from the guest.  False when the network is closed or the
    // frame is no Ethernet frame the card could carry; a frame the policy
    // refuses counts as sent.
    bool send(const std::string& frame) {
        if (stopping_.load())
            return false;
        if (frame.size() < kEthernetHeader || frame.size() > kMaxFrame)
            return false;
        const auto* bytes = reinterpret_cast<const std::uint8_t*>(frame.data());
        if (!admitted(bytes, frame.size())) {
            refused_.fetch_add(1);
            return true;
        }
        {
            std::lock_guard<std::mutex> lock(transmit_mutex_);
            transmit_.emplace_back(bytes, bytes + frame.size());
        }
        sent_.fetch_add(1);
        wake();
        return true;
    }

    // The next frame for the guest, waiting up to TIMEOUT seconds; None when
    // none came or the network is closed.
    py::object receive(double timeout) {
        std::vector<std::uint8_t> frame;
        {
            py::gil_scoped_release unlocked;
            std::unique_lock<std::mutex> lock(receive_mutex_);
            const auto limit = std::chrono::duration<double>(std::max(0.0, timeout));
            receive_ready_.wait_for(lock, limit, [this] {
                return !receive_.empty() || stopping_.load();
            });
            if (receive_.empty())
                return py::none();
            frame = std::move(receive_.front());
            receive_.pop_front();
        }
        return py::bytes(reinterpret_cast<const char*>(frame.data()), frame.size());
    }

    void shut() {
        if (stopping_.exchange(true))
            return;
        wake();
        receive_ready_.notify_all();
        if (thread_.joinable())
            thread_.join();
        slirp_cleanup(slirp_);
        slirp_ = nullptr;
        timers_.clear();
        close(wake_);
    }

    py::dict stats() const {
        py::dict result;
        result["sent"] = sent_.load();
        result["refused"] = refused_.load();
        result["received"] = received_.load();
        result["dropped"] = dropped_.load();
        result["guest_errors"] = guest_errors_.load();
        result["local_access"] = local_access_;
        return result;
    }

    bool local_access() const { return local_access_; }

private:
    struct Timer {
        SlirpTimerId id;
        void* cb_opaque;
        std::int64_t expire_ms = 0;
        bool armed = false;
    };

    // Whether the policy lets a frame from the guest reach libslirp.
    bool admitted(const std::uint8_t* frame, std::size_t length) {
        const std::uint16_t type = std::uint16_t(frame[12] << 8 | frame[13]);
        if (type == 0x0806)
            return true;  // ARP: libslirp answers only for its own addresses
        if (type != 0x0800 || length < kEthernetHeader + kIPv4Header)
            return false;  // the guest speaks IPv4 alone
        const std::uint32_t destination = read_be32(frame + kEthernetHeader + 16);
        if ((destination & netmask_) == network_ || destination == 0xFFFFFFFF)
            return true;  // libslirp's own network: its gateway, DNS and DHCP
        if (local_access_)
            return true;
        return !local_destination(destination) && !host_address(destination);
    }

    bool host_address(std::uint32_t address) {
        std::lock_guard<std::mutex> lock(host_mutex_);
        const std::int64_t now = monotonic_ns() / 1000000;
        if (host_addresses_read_ == 0 || now - host_addresses_read_ > kHostAddressesLifetimeMs) {
            host_addresses_.clear();
            ifaddrs* list = nullptr;
            if (getifaddrs(&list) == 0) {
                for (ifaddrs* entry = list; entry != nullptr; entry = entry->ifa_next) {
                    if (entry->ifa_addr != nullptr && entry->ifa_addr->sa_family == AF_INET) {
                        const auto* inet = reinterpret_cast<const sockaddr_in*>(entry->ifa_addr);
                        host_addresses_.push_back(ntohl(inet->sin_addr.s_addr));
                    }
                }
                freeifaddrs(list);
            }
            host_addresses_read_ = now;
        }
        return std::find(host_addresses_.begin(), host_addresses_.end(), address) !=
               host_addresses_.end();
    }

    void wake() {
        const std::uint64_t one = 1;
        [[maybe_unused]] ssize_t written = ::write(wake_, &one, sizeof one);
    }

    // The thread that owns libslirp.
    void run() {
        std::vector<std::vector<std::uint8_t>> frames;
        while (!stopping_.load()) {
            {
                std::lock_guard<std::mutex> lock(transmit_mutex_);
                frames.assign(std::make_move_iterator(transmit_.begin()),
                              std::make_move_iterator(transmit_.end()));
                transmit_.clear();
            }
            for (const auto& frame : frames)
                slirp_input(slirp_, frame.data(), int(frame.size()));
            frames.clear();

            polls_.clear();
            polls_.push_back(pollfd{wake_, POLLIN, 0});
            std::uint32_t timeout = 1000;
            slirp_pollfds_fill(slirp_, &timeout, &SlirpNetwork::add_poll, this);
            const std::int64_t now = monotonic_ns() / 1000000;
            for (const auto& timer : timers_)
                if (timer->armed)
                    timeout = std::uint32_t(std::clamp<std::int64_t>(
                        timer->expire_ms - now, 0, std::int64_t(timeout)));
            const int ready = ::poll(polls_.data(), polls_.size(), int(timeout));
            if (ready > 0 && (polls_[0].revents & POLLIN)) {
                std::uint64_t count = 0;
                [[maybe_unused]] ssize_t taken = ::read(wake_, &count, sizeof count);
            }
            slirp_pollfds_poll(slirp_, ready < 0, &SlirpNetwork::get_revents, this);
            fire_timers();
        }
    }

    void fire_timers() {
        const std::int64_t now = monotonic_ns() / 1000000;
        // A timer's callback may change the list, so each due one is found
        // afresh.
        for (;;) {
            Timer* due = nullptr;
            for (const auto& timer : timers_)
                if (timer->armed && timer->expire_ms <= now) {
                    due = timer.get();
                    break;
                }
            if (due == nullptr)
                return;
            due->armed = false;
            slirp_handle_timer(slirp_, due->id, due->cb_opaque);
        }
    }

    static int add_poll(int fd, int events, void* opaque) {
        auto* self = static_cast<SlirpNetwork*>(opaque);
        short wanted = 0;
        if (events & SLIRP_POLL_IN)
            wanted |= POLLIN;
        if (events & SLIRP_POLL_OUT)
            wanted |= POLLOUT;
        if (events & SLIRP_POLL_PRI)
            wanted |= POLLPRI;
        self->polls_.push_back(pollfd{fd, wanted, 0});
        return int(self->polls_.size() - 1);
    }

    static int get_revents(int index, void* opaque) {
        auto* self = static_cast<SlirpNetwork*>(opaque);
        if (index < 0 || std::size_t(index) >= self->polls_.size())
            return 0;
        const short got = self->polls_[std::size_t(index)].revents;
        int events = 0;
        if (got & POLLIN)
            events |= SLIRP_POLL_IN;
        if (got & POLLOUT)
            events |= SLIRP_POLL_OUT;
        if (got & POLLPRI)
            events |= SLIRP_POLL_PRI;
        if (got & POLLERR)
            events |= SLIRP_POLL_ERR;
        if (got & POLLHUP)
            events |= SLIRP_POLL_HUP;
        return events;
    }

    static ssize_t send_packet(const void* buffer, std::size_t length, void* opaque) {
        auto* self = static_cast<SlirpNetwork*>(opaque);
        if (length > kMaxFrame) {
            self->dropped_.fetch_add(1);
            return ssize_t(length);
        }
        const auto* bytes = static_cast<const std::uint8_t*>(buffer);
        {
            std::lock_guard<std::mutex> lock(self->receive_mutex_);
            if (self->receive_.size() >= kReceiveLimit) {
                self->dropped_.fetch_add(1);
                return ssize_t(length);
            }
            self->receive_.emplace_back(bytes, bytes + length);
        }
        self->received_.fetch_add(1);
        self->receive_ready_.notify_one();
        return ssize_t(length);
    }

    static void guest_error(const char*, void* opaque) {
        static_cast<SlirpNetwork*>(opaque)->guest_errors_.fetch_add(1);
    }

    static int64_t clock_get_ns(void*) { return monotonic_ns(); }

    static void* timer_new_opaque(SlirpTimerId id, void* cb_opaque, void* opaque) {
        auto* self = static_cast<SlirpNetwork*>(opaque);
        self->timers_.push_back(std::make_unique<Timer>(Timer{id, cb_opaque}));
        return self->timers_.back().get();
    }

    static void timer_free(void* timer, void* opaque) {
        auto* self = static_cast<SlirpNetwork*>(opaque);
        auto& timers = self->timers_;
        timers.erase(std::remove_if(timers.begin(), timers.end(),
                                    [timer](const std::unique_ptr<Timer>& entry) {
                                        return entry.get() == timer;
                                    }),
                     timers.end());
    }

    static void timer_mod(void* timer, int64_t expire_ms, void*) {
        auto* entry = static_cast<Timer*>(timer);
        entry->expire_ms = expire_ms;
        entry->armed = true;
    }

    static void poll_fd_unused(int, void*) {}

    static void notify(void* opaque) { static_cast<SlirpNetwork*>(opaque)->wake(); }

    const bool local_access_;
    std::uint32_t network_ = 0;
    std::uint32_t netmask_ = 0;
    SlirpCb callbacks_{};
    Slirp* slirp_ = nullptr;
    int wake_ = -1;
    std::thread thread_;
    std::atomic<bool> stopping_{false};

    std::mutex transmit_mutex_;
    std::deque<std::vector<std::uint8_t>> transmit_;
    std::mutex receive_mutex_;
    std::condition_variable receive_ready_;
    std::deque<std::vector<std::uint8_t>> receive_;

    // Touched only by the thread that owns libslirp.
    std::vector<pollfd> polls_;
    std::vector<std::unique_ptr<Timer>> timers_;

    std::mutex host_mutex_;
    std::vector<std::uint32_t> host_addresses_;
    std::int64_t host_addresses_read_ = 0;

    std::atomic<std::uint64_t> sent_{0};
    std::atomic<std::uint64_t> refused_{0};
    std::atomic<std::uint64_t> received_{0};
    std::atomic<std::uint64_t> dropped_{0};
    std::atomic<std::uint64_t> guest_errors_{0};
};

}  // namespace megapad::network

PYBIND11_MODULE(_mp64_slirp, module) {
    using megapad::network::SlirpNetwork;
    module.doc() = "The machine's user-mode network, through libslirp.";
    module.attr("MAX_FRAME") = megapad::network::kMaxFrame;
    module.def("slirp_version", [] { return std::string(slirp_version_string()); });
    module.def("local_destination", [](const std::string& address) {
        return megapad::network::local_destination(
            megapad::network::parse_ipv4(address, "address"));
    });
    py::class_<SlirpNetwork>(module, "SlirpNetwork")
        .def(py::init<const std::string&, int, const std::string&, const std::string&,
                      const std::string&, bool>(),
             py::arg("network"), py::arg("prefix"), py::arg("gateway"),
             py::arg("dhcp_start"), py::arg("nameserver"), py::arg("local_access"))
        .def("send", [](SlirpNetwork& self, py::bytes frame) {
            return self.send(std::string(frame));
        })
        .def("receive", &SlirpNetwork::receive, py::arg("timeout"))
        .def("close", &SlirpNetwork::shut, py::call_guard<py::gil_scoped_release>())
        .def("stats", &SlirpNetwork::stats)
        .def_property_readonly("local_access", &SlirpNetwork::local_access);
}
