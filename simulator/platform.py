"""Focused platform services for the hosted one-core simulator profile.

The initial hosted profile exposes only the read-only SysInfo MMIO window.
Its memory-topology registers are built from the exact
:class:`~simulator.memory.SparseAddressSpace` returned by the factory below;
the service does not carry an independent copy of the address-space defaults.
"""

from __future__ import annotations

from shared.cells import MASK64
from shared.crc import CRYPTO_CAP_CRC_REFLECT_RAW
from simulator.memory import (
    BANK0_DEFAULT_SIZE,
    DEFAULT_PAGE_SIZE,
    AddressClass,
    SparseAddressSpace,
)


SYSINFO_OFFSET = 0x300
SYSINFO_SIZE = 0x70
SYSINFO_LIMIT = SYSINFO_OFFSET + SYSINFO_SIZE
SYSINFO_BANK0_SIZE = SYSINFO_OFFSET + 0x08
SYSINFO_NUM_CORES = SYSINFO_OFFSET + 0x10
SYSINFO_NUM_FULL = SYSINFO_OFFSET + 0x48
SYSINFO_CRYPTO_CAPS = SYSINFO_OFFSET + 0x60

BOARD_ID_VERSION = 0x4D50_3634_0002_0001

_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))


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


class OneCoreSysInfo:
    """Read-only SysInfo service for one hosted full core.

    Constructing the service and its address space is a two-phase operation
    because :class:`SparseAddressSpace` receives its MMIO port at construction
    time, while SysInfo must report that constructed space's actual regions.
    Callers should normally use :func:`create_one_core_address_space`.
    """

    __slots__ = ("_crypto_capabilities", "_image")

    def __init__(
        self,
        *,
        crypto_capabilities: int = CRYPTO_CAP_CRC_REFLECT_RAW,
    ) -> None:
        if (
            isinstance(crypto_capabilities, bool)
            or not isinstance(crypto_capabilities, int)
        ):
            raise TypeError("crypto capabilities must be a uint64 integer")
        if not 0 <= crypto_capabilities <= MASK64:
            raise ValueError("crypto capabilities must be a uint64 integer")
        if crypto_capabilities & ~CRYPTO_CAP_CRC_REFLECT_RAW:
            raise ValueError(
                "hosted profile cannot advertise unimplemented crypto bits"
            )
        self._crypto_capabilities = crypto_capabilities
        self._image: bytes | None = None

    def bind(self, memory: SparseAddressSpace) -> None:
        """Bind once to the address space whose geometry SysInfo reports."""

        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("SysInfo memory must be a SparseAddressSpace")
        if self._image is not None:
            raise RuntimeError("SysInfo is already bound")

        regions = {spec.kind: spec for spec in memory.regions}

        def geometry(kind: AddressClass) -> tuple[int, int]:
            spec = regions.get(kind)
            if spec is None:
                return 0, 0
            return spec.base, spec.size

        _bank0_base, bank0_size = geometry(AddressClass.BANK0)
        hbw_base, hbw_size = geometry(AddressClass.HBW)
        external_base, external_size = geometry(AddressClass.EXTERNAL)
        vram_base, vram_size = geometry(AddressClass.VRAM)

        register_values = (
            BOARD_ID_VERSION,
            bank0_size,
            1,  # NUM_CORES: the initial profile has one full core.
            0,  # CLUSTER_EN: there are no hosted micro-core clusters.
            hbw_base,
            hbw_size,
            bank0_size + hbw_size,
            external_base,
            external_size,
            1,  # NUM_FULL
            vram_base,
            vram_size,
            self._crypto_capabilities,
            1,  # NUM_BUS_PORTS: the sole full-core requester.
        )
        image = bytearray(SYSINFO_SIZE)
        for index, value in enumerate(register_values):
            start = index * 8
            image[start : start + 8] = value.to_bytes(8, "little")
        self._image = bytes(image)

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Validate one complete naturally aligned scalar MMIO access."""

        self._require_bound()
        if not isinstance(offset, int):
            raise TypeError("SysInfo offset must be an integer")
        if not isinstance(width, int):
            raise TypeError("SysInfo width must be an integer")
        if width not in _INTEGER_WIDTHS:
            self._reject(
                "SysInfo width must be 1, 2, 4, or 8 bytes",
                offset=offset,
                width=width,
                write=write,
            )
        if offset < SYSINFO_OFFSET or offset + width > SYSINFO_LIMIT:
            self._reject(
                "access is outside the exact SysInfo MMIO window",
                offset=offset,
                width=width,
                write=write,
            )
        if offset % width:
            self._reject(
                "SysInfo access is not naturally aligned",
                offset=offset,
                width=width,
                write=write,
            )
        if write:
            self._reject(
                "the hosted SysInfo window is read-only",
                offset=offset,
                width=width,
                write=True,
            )

    def read8(self, offset: int) -> int:
        """Return one little-endian byte after whole-access preflight."""

        image = self._require_bound()
        self._require_byte_offset(offset, write=False)
        return image[offset - SYSINFO_OFFSET]

    def write8(self, offset: int, value: int) -> None:
        """Reject a byte write even if a caller bypasses preflight."""

        del value
        self._require_bound()
        self._require_byte_offset(offset, write=True)
        self._reject(
            "the hosted SysInfo window is read-only",
            offset=offset,
            width=1,
            write=True,
        )

    def _require_bound(self) -> bytes:
        image = self._image
        if image is None:
            raise RuntimeError("SysInfo is not bound to an address space")
        return image

    def _require_byte_offset(self, offset: int, *, write: bool) -> None:
        if not isinstance(offset, int):
            raise TypeError("SysInfo offset must be an integer")
        if not SYSINFO_OFFSET <= offset < SYSINFO_LIMIT:
            self._reject(
                "byte access is outside the exact SysInfo MMIO window",
                offset=offset,
                width=1,
                write=write,
            )

    @staticmethod
    def _reject(
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        raise SysInfoAccessError(
            message,
            offset=offset,
            width=width,
            write=write,
        )


def create_one_core_address_space(
    *,
    bank0_size: int = BANK0_DEFAULT_SIZE,
    external_size: int = 0,
    vram_size: int = 0,
    hbw_size: int = 0,
    page_size: int = DEFAULT_PAGE_SIZE,
    crypto_capabilities: int = CRYPTO_CAP_CRC_REFLECT_RAW,
) -> SparseAddressSpace:
    """Return sparse guest memory with the one-core SysInfo service attached."""

    sysinfo = OneCoreSysInfo(crypto_capabilities=crypto_capabilities)
    memory = SparseAddressSpace(
        bank0_size=bank0_size,
        external_size=external_size,
        vram_size=vram_size,
        hbw_size=hbw_size,
        mmio=sysinfo,
        page_size=page_size,
    )
    sysinfo.bind(memory)
    return memory


__all__ = [
    "BOARD_ID_VERSION",
    "SYSINFO_BANK0_SIZE",
    "SYSINFO_CRYPTO_CAPS",
    "SYSINFO_LIMIT",
    "SYSINFO_NUM_CORES",
    "SYSINFO_NUM_FULL",
    "SYSINFO_OFFSET",
    "SYSINFO_SIZE",
    "OneCoreSysInfo",
    "SysInfoAccessError",
    "create_one_core_address_space",
]
