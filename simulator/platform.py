"""The hosted machine's MMIO devices, attached to one native table.

Every device claims its windows on the simulator's native ``PlatformTable``
(``simulator/accel/platform_bindings.h``), which refuses overlapping windows
and gives each access, with its requester, to the device whose window holds
its first byte: a whole-access preflight, then each byte lowest address
first. System Info, the chiplet port window and the mailbox are native
devices. AES, SHA-3, the TRNG, the RTC and the audio output are Python
services that attach through the table's adapter and keep their models.

A machine profile names its topology, and System Info reports it with the
memory geometry of the exact :class:`~simulator.memory.SparseAddressSpace`
the factory below returns. Runtime construction still admits only one full
core with no cluster (``docs/simulator-contract.md``, section 6).

This module is glue. The table and the native devices load with the
simulator's extension when a platform is made, so importing the simulator
needs no build.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

from shared.cells import MASK64
from shared.memory_backing import DenseMemoryBacking
from shared.audio_output import AUDIO_LIMIT, AUDIO_OFFSET
from shared.crypto_caps import (
    CRYPTO_CAP_CRC_REFLECT_RAW,
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
)
from simulator.aes import AES_LIMIT, AES_OFFSET, HostedAESService
from simulator.audio import HostedAudioService
from simulator.chiplet import (
    CHIPLET_LIMIT,
    CHIPLET_OFFSET,
    MAILBOX_LIMIT,
    MAILBOX_OFFSET,
    ChipletAccessError,
    HostedChipletPorts,
)
from simulator.entropy import (
    DEFAULT_TRNG_SEED,
    TRNG_LIMIT,
    TRNG_OFFSET,
    HostedTRNGService,
)
from simulator.memory import (
    BANK0_DEFAULT_SIZE,
    DEFAULT_PAGE_SIZE,
    AddressClass,
    SparseAddressSpace,
)
from simulator.rtc import (
    RTC_EPOCH_LIMIT,
    RTC_EPOCH_STAT,
    RTC_UPTIME,
    HostedRTCService,
)
from simulator.sha3 import (
    SHA3_LIMIT, SHA3_OFFSET, HostedSHA3Service, _register_input_platform_route,
)


SYSINFO_OFFSET = 0x300
SYSINFO_SIZE = 0x78
SYSINFO_LIMIT = SYSINFO_OFFSET + SYSINFO_SIZE
SYSINFO_BANK0_SIZE = SYSINFO_OFFSET + 0x08
SYSINFO_NUM_CORES = SYSINFO_OFFSET + 0x10
SYSINFO_HBW_BASE = SYSINFO_OFFSET + 0x20
SYSINFO_HBW_SIZE = SYSINFO_OFFSET + 0x28
SYSINFO_EXTERNAL_BASE = SYSINFO_OFFSET + 0x38
SYSINFO_EXTERNAL_SIZE = SYSINFO_OFFSET + 0x40
SYSINFO_NUM_FULL = SYSINFO_OFFSET + 0x48
SYSINFO_VRAM_BASE = SYSINFO_OFFSET + 0x50
SYSINFO_VRAM_SIZE = SYSINFO_OFFSET + 0x58
SYSINFO_CRYPTO_CAPS = SYSINFO_OFFSET + 0x60
SYSINFO_NUM_BUS_PORTS = SYSINFO_OFFSET + 0x68
SYSINFO_CHIPLET_PORTS = SYSINFO_OFFSET + 0x70

BOARD_ID_VERSION = 0x4D50_3634_0002_0001

HOSTED_CRYPTO_CAPABILITIES = (
    CRYPTO_CAP_CRC_REFLECT_RAW
    | CRYPTO_CAP_SHA3_STREAM
    | CRYPTO_CAP_KECCAK_F1600
)

# The Python services that attach through the table's adapter, by the name
# each answers to, and their windows. The RTC answers in its clock registers
# and in its one epoch status byte.
_PYTHON_DEVICE_WINDOWS = {
    "aes": ((AES_OFFSET, AES_LIMIT),),
    "sha3": ((SHA3_OFFSET, SHA3_LIMIT),),
    "entropy": ((TRNG_OFFSET, TRNG_LIMIT),),
    "rtc": ((RTC_UPTIME, RTC_EPOCH_LIMIT), (RTC_EPOCH_STAT, RTC_EPOCH_STAT + 1)),
    "audio": ((AUDIO_OFFSET, AUDIO_LIMIT),),
}
_PYTHON_DEVICE_TYPES = {
    "aes": HostedAESService,
    "sha3": HostedSHA3Service,
    "entropy": HostedTRNGService,
    "rtc": HostedRTCService,
    "audio": HostedAudioService,
}


class SysInfoAccessError(ValueError):
    """One direct access does not belong to the hosted SysInfo contract."""

    def __init__(
        self,
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        self.offset = offset
        self.width = width
        self.write = write
        super().__init__(message)


class PlatformMMIOAccessError(ValueError):
    """One access falls outside every platform window, or the mailbox
    refuses it."""

    def __init__(
        self,
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        self.offset = offset
        self.width = width
        self.write = write
        super().__init__(message)


# The error each native device's refusal raises, by its name on the table.
_REFUSAL_ERRORS = {
    "sysinfo": SysInfoAccessError,
    "chiplet": ChipletAccessError,
}


def _refuse(name: str, message: str, offset: int, width: int, write: bool) -> None:
    raise _REFUSAL_ERRORS.get(name, PlatformMMIOAccessError)(
        message, offset=offset, width=width, write=write,
    )


@dataclass(frozen=True, slots=True)
class Topology:
    """The machine's cores: one to four full cores, and zero to three
    clusters of four microcores, the shapes the BIOS and the emulator
    support."""

    full_cores: int = 1
    clusters: int = 0

    def __post_init__(self) -> None:
        for value, label in ((self.full_cores, "full cores"), (self.clusters, "clusters")):
            if isinstance(value, bool) or not isinstance(value, int):
                raise TypeError(f"a topology's {label} must be an integer")
        if not 1 <= self.full_cores <= 4:
            raise ValueError("a machine has one to four full cores")
        if not 0 <= self.clusters <= 3:
            raise ValueError("a machine has zero to three clusters")

    @property
    def core_count(self) -> int:
        """Every core: the full cores, then four microcores per cluster."""

        return self.full_cores + 4 * self.clusters


ONE_CORE = Topology()


def _native_extension():
    try:
        import _megaforth_native as extension
    except ImportError:
        extension = None
    if extension is None or not hasattr(extension, "PlatformTable"):
        raise RuntimeError(
            "the hosted platform requires _megaforth_native, built to match "
            "this checkout: run make simulator-accel"
        )
    return extension


def _python_service_property(name: str) -> property:
    def get(self: "HostedPlatform"):
        return self._table.device(name)

    def put(self: "HostedPlatform", service: object) -> None:
        self._check_service(name, service)
        self._table.replace(name, service)

    return property(get, put, doc=f"The platform's {name} service.")


class HostedPlatform:
    """The machine's MMIO devices and the native table that routes to them.

    The platform is the address space's MMIO port. Its services are named
    attributes; assigning a Python service puts it in the place of the one
    attached under that name, in the same windows.
    """

    __slots__ = (
        "_table", "_topology", "_crypto_capabilities", "_chiplet",
        "_sysinfo", "_mailbox", "_signal_takers", "_bound", "__weakref__",
    )

    def __init__(
        self,
        *,
        aes: HostedAESService,
        sha3: HostedSHA3Service,
        entropy: HostedTRNGService,
        rtc: HostedRTCService,
        audio: HostedAudioService | None = None,
        chiplet: HostedChipletPorts | None = None,
        topology: Topology = ONE_CORE,
        crypto_capabilities: int = HOSTED_CRYPTO_CAPABILITIES,
    ) -> None:
        if not isinstance(topology, Topology):
            raise TypeError("platform topology must be a Topology")
        if (
            isinstance(crypto_capabilities, bool)
            or not isinstance(crypto_capabilities, int)
        ):
            raise TypeError("crypto capabilities must be a uint64 integer")
        if not 0 <= crypto_capabilities <= MASK64:
            raise ValueError("crypto capabilities must be a uint64 integer")
        if crypto_capabilities & ~HOSTED_CRYPTO_CAPABILITIES:
            raise ValueError(
                "hosted profile cannot advertise unimplemented crypto bits"
            )
        if chiplet is not None and not isinstance(chiplet, HostedChipletPorts):
            raise TypeError("platform chiplet ports must be HostedChipletPorts")
        if chiplet is not None and chiplet.core_count != topology.core_count:
            raise ValueError(
                "the chiplet ports' IDs must follow the topology's cores"
            )
        services = {
            "aes": aes, "sha3": sha3, "entropy": entropy, "rtc": rtc,
            "audio": HostedAudioService() if audio is None else audio,
        }
        for name, service in services.items():
            self._check_service(name, service)
        table = _native_extension().PlatformTable()
        table.set_refusal(_refuse)
        for name, service in services.items():
            table.attach(name, service, _PYTHON_DEVICE_WINDOWS[name])
        self._table = table
        self._topology = topology
        self._crypto_capabilities = crypto_capabilities
        self._chiplet = chiplet
        self._sysinfo = None
        self._mailbox = None
        self._signal_takers = None
        self._bound = False

    @staticmethod
    def _check_service(name: str, service: object) -> None:
        expected = _PYTHON_DEVICE_TYPES[name]
        if not isinstance(service, expected):
            raise TypeError(f"platform {name} must be a {expected.__name__}")

    def bind(self, memory: SparseAddressSpace) -> None:
        """Bind once to the address space whose geometry System Info
        reports, and attach the devices that need it."""

        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("platform memory must be a SparseAddressSpace")
        if self._bound:
            raise RuntimeError("the platform is already bound")
        extension = _native_extension()
        regions = {spec.kind: spec for spec in memory.regions}

        def geometry(kind: AddressClass) -> tuple[int, int]:
            spec = regions.get(kind)
            return (0, 0) if spec is None else (spec.base, spec.size)

        _bank0_base, bank0_size = geometry(AddressClass.BANK0)
        hbw_base, hbw_size = geometry(AddressClass.HBW)
        external_base, external_size = geometry(AddressClass.EXTERNAL)
        vram_base, vram_size = geometry(AddressClass.VRAM)
        chiplet = self._chiplet
        sysinfo = extension.SysInfo(
            board_id=BOARD_ID_VERSION,
            full_cores=self._topology.full_cores,
            clusters=self._topology.clusters,
            bank0_size=bank0_size,
            hbw_base=hbw_base,
            hbw_size=hbw_size,
            external_base=external_base,
            external_size=external_size,
            vram_base=vram_base,
            vram_size=vram_size,
            crypto_capabilities=self._crypto_capabilities,
            chiplet_ports=0 if chiplet is None else chiplet.port_count_word,
        )
        self.audio.bind(memory)
        # The cores that take a chiplet's signal at once when it arrives in
        # one of their own accesses; the core scheduler names the workers.
        self._signal_takers = extension.SignalTakers()
        if chiplet is None:
            mailbox = extension.Mailbox(self._topology.core_count)
        else:
            chiplet.bind(memory)
            mailbox = extension.Mailbox(chiplet.link, self._signal_takers)
            self._table.attach(
                "chiplet",
                extension.ChipletWindowDevice(chiplet.link, self._signal_takers),
                ((CHIPLET_OFFSET, CHIPLET_LIMIT),),
            )
        self._table.attach("sysinfo", sysinfo, ((SYSINFO_OFFSET, SYSINFO_LIMIT),))
        self._table.attach("mailbox", mailbox, ((MAILBOX_OFFSET, MAILBOX_LIMIT),))
        self._sysinfo = sysinfo
        self._mailbox = mailbox
        self._bound = True

    # -- the port -----------------------------------------------------------

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        self._table.preflight(offset, width, write)

    def read8(self, offset: int) -> int:
        return self._table.read8(offset)

    def write8(self, offset: int, value: int) -> None:
        self._table.write8(offset, value)

    # -- the devices --------------------------------------------------------

    @property
    def table(self):
        """The native table every access goes through."""

        return self._table

    @property
    def topology(self) -> Topology:
        return self._topology

    @property
    def sysinfo(self):
        """System Info's native device, once the platform is bound."""

        return self._sysinfo

    @property
    def mailbox(self):
        """The native mailbox, once the platform is bound."""

        return self._mailbox

    @property
    def signal_takers(self):
        """The native set of cores that take a chiplet's signal at once,
        once the platform is bound: a bit for each core whose IPI handler
        acknowledges a signal that arrives during its own access."""

        return self._signal_takers

    @property
    def chiplet(self) -> HostedChipletPorts | None:
        """The profile's chiplet ports, or None when it has none."""

        return self._chiplet

    aes = _python_service_property("aes")
    sha3 = _python_service_property("sha3")
    entropy = _python_service_property("entropy")
    rtc = _python_service_property("rtc")
    audio = _python_service_property("audio")


# Capture the original route identities before any platform instance or
# caller customization exists. SHA3 requalifies them for each submission.
_register_input_platform_route(HostedPlatform)


def create_address_space(
    *,
    topology: Topology = ONE_CORE,
    bank0_size: int = BANK0_DEFAULT_SIZE,
    external_size: int = 0,
    vram_size: int = 0,
    hbw_size: int = 0,
    page_size: int = DEFAULT_PAGE_SIZE,
    dense_backing: DenseMemoryBacking | Literal[True] | None = None,
    crypto_capabilities: int = HOSTED_CRYPTO_CAPABILITIES,
    entropy_seed: bytes = DEFAULT_TRNG_SEED,
    entropy_usable: bool = True,
    initial_epoch_ms: int | None = None,
    initial_uptime_ms: int = 0,
    chiplet_ports: int = 0,
) -> SparseAddressSpace:
    """Return guest memory with the platform of ``topology``; sparse unless
    explicit.

    ``chiplet_ports`` gives the profile that many chiplet ports, with their
    register window; their IDs follow the topology's cores. The ports are
    the native model, as the platform's table and devices are.
    """

    if not isinstance(topology, Topology):
        raise TypeError("topology must be a Topology")
    chiplet = (
        HostedChipletPorts(chiplet_ports, core_count=topology.core_count)
        if chiplet_ports else None
    )
    platform = HostedPlatform(
        aes=HostedAESService(),
        sha3=HostedSHA3Service(
            crypto_capabilities
            & (CRYPTO_CAP_SHA3_STREAM | CRYPTO_CAP_KECCAK_F1600)
        ),
        entropy=HostedTRNGService(
            entropy_seed,
            usable=entropy_usable,
        ),
        rtc=HostedRTCService(
            initial_epoch_ms,
            initial_uptime_ms=initial_uptime_ms,
        ),
        chiplet=chiplet,
        topology=topology,
        crypto_capabilities=crypto_capabilities,
    )
    memory = SparseAddressSpace(
        bank0_size=bank0_size,
        external_size=external_size,
        vram_size=vram_size,
        hbw_size=hbw_size,
        mmio=platform,
        page_size=page_size,
        dense_backing=dense_backing,
    )
    platform.bind(memory)
    return memory


__all__ = [
    "BOARD_ID_VERSION",
    "HOSTED_CRYPTO_CAPABILITIES",
    "ONE_CORE",
    "SYSINFO_BANK0_SIZE",
    "SYSINFO_CHIPLET_PORTS",
    "SYSINFO_CRYPTO_CAPS",
    "SYSINFO_EXTERNAL_BASE",
    "SYSINFO_EXTERNAL_SIZE",
    "SYSINFO_HBW_BASE",
    "SYSINFO_HBW_SIZE",
    "SYSINFO_LIMIT",
    "SYSINFO_NUM_BUS_PORTS",
    "SYSINFO_NUM_CORES",
    "SYSINFO_NUM_FULL",
    "SYSINFO_OFFSET",
    "SYSINFO_SIZE",
    "HostedPlatform",
    "PlatformMMIOAccessError",
    "SysInfoAccessError",
    "Topology",
    "create_address_space",
]
