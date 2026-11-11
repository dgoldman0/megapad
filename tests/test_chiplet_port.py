"""The chiplet port bridge and its register window, with no machine around.

The cases follow rtl/sim/tb_chiplet_port.v, with a chiplet driven from the
test in place of the bench's link signals. The model completes a request
inside the call that makes it, so the bench's cases about a request
cancelled in flight have no counterpart here: a request made while the
chiplet is held in reset is the one that ends CANCELLED. Both extensions
must be built before this gate.
"""

from __future__ import annotations

import importlib

import pytest


EXT_BASE = 0x0020_0000
EXT_SIZE = 0x1000
EXT_LIMIT = EXT_BASE + EXT_SIZE

PORT_COUNT = 0x00
PORT_SEL = 0x08
EXT_CORE_QOS = 0x10
CTRL = 0x80
STATUS = 0x88
FAULT_ADDR = 0x90
FAULT_INFO = 0x98
RANGE_SEL = 0xA0
RANGE_BASE = 0xA8
RANGE_LIMIT = 0xB0
RANGE_RIGHTS = 0xB8
QOS = 0xC0
ID_INDEX = 0xC8
ID_DATA = 0xD0
BOOT_ADDR = 0xD8
BOOT_LEN = 0xE0
BOOT_ARG = 0xE8

ENABLE = 1 << 0
RESET_HOLD = 1 << 1
STOP_REQ = 1 << 2
DOORBELL_EN = 1 << 3
ATTN_EN = 1 << 4

PRESENT = 1 << 0
STOPPED = 1 << 1
READY = 1 << 2
BUSY = 1 << 3
FAULT = 1 << 4
ID_DONE = 1 << 5

OK, REFUSED, MEM_ERROR, CANCELLED = 0, 1, 2, 3
CLOSED, MISALIGNED, NOT_EXTERNAL, NOT_LENT = 1, 2, 3, 4
READ, WRITE = 1, 2

PORT_ID = 16
IDENTITY = [0x4348_4950_0000_0001, 0x0000_0000_0031_4D4C, 0,
            0x4C4D_3100_0001_0000]


@pytest.fixture(params=("_mp64_accel", "_megaforth_native"))
def native(request):
    return importlib.import_module(request.param)


class Bench:
    """Sixteen cores, one port with six ranges, and a chiplet in hand."""

    def __init__(self, native, *, attach=True):
        self.memory = bytearray(EXT_SIZE)
        self.link = native.ChipletLink(self.memory, EXT_BASE, 16, 1, 6)
        self.chiplet = native.ManualChiplet(IDENTITY)
        if attach:
            self.link.attach_manual_chiplet(0, self.chiplet)

    def lend(self, index, base, limit, rights):
        self.link.write64(RANGE_SEL, index)
        self.link.write64(RANGE_BASE, base)
        self.link.write64(RANGE_LIMIT, limit)
        self.link.write64(RANGE_RIGHTS, rights)

    def open(self, extra=0):
        self.link.write64(CTRL, ENABLE | extra)

    def read(self, addr, size=3, length=0):
        return self.chiplet.request(addr, size, length, False)

    def write(self, addr, *words, size=3):
        return self.chiplet.request(
            addr, size, len(words) - 1, True, list(words))

    def word(self, addr):
        offset = addr - EXT_BASE
        return int.from_bytes(self.memory[offset:offset + 8], "little")

    def reason(self):
        return (self.link.read64(FAULT_INFO) >> 4) & 0xF

    def refusals(self):
        return (self.link.read64(FAULT_INFO) >> 16) & 0xFFFF


@pytest.fixture
def bench(native):
    return Bench(native)


# ---------------------------------------------------------------------------
#  Reset, presence and the window
# ---------------------------------------------------------------------------

def test_the_chiplet_is_held_in_reset_after_reset(bench):
    assert bench.link.read64(CTRL) == RESET_HOLD
    assert not bench.chiplet.released
    assert bench.read(EXT_BASE)[0] == CANCELLED
    assert bench.link.read64(STATUS) & FAULT == 0


def test_status_reports_the_chiplet_the_port_id_and_the_ranges(bench):
    status = bench.link.read64(STATUS)
    assert status & PRESENT
    assert (status >> 16) & 0xFF == PORT_ID
    assert (status >> 32) & 0xFF == 6
    assert status & BUSY == 0


def test_a_port_without_a_chiplet_reports_none(native):
    bench = Bench(native, attach=False)
    assert bench.link.read64(STATUS) & PRESENT == 0
    bench.link.write64(ID_INDEX, 0)
    assert bench.link.read64(STATUS) & ID_DONE == 0
    bench.open()
    assert not bench.link.step()


def test_port_count_names_the_ports_their_first_id_and_their_ranges(bench):
    assert bench.link.read64(PORT_COUNT) == 1 | (PORT_ID << 8) | (6 << 16)
    assert bench.link.port_count_word == bench.link.read64(PORT_COUNT)
    bench.link.write64(PORT_COUNT, 0xFFFF)
    assert bench.link.read64(PORT_COUNT) == 1 | (PORT_ID << 8) | (6 << 16)


def test_a_port_sel_beyond_the_ports_selects_none(bench):
    bench.link.write64(PORT_SEL, 1)
    assert bench.link.read64(PORT_SEL) == 1
    assert bench.link.read64(CTRL) == 0
    assert bench.link.read64(STATUS) == 0
    bench.link.write64(CTRL, ENABLE)
    bench.link.write64(PORT_SEL, 0)
    assert bench.link.read64(CTRL) == RESET_HOLD
    assert not bench.chiplet.released


def test_the_shared_registers_hold_what_was_written(bench):
    assert bench.link.read64(EXT_CORE_QOS) == 1
    bench.link.write64(EXT_CORE_QOS, 0x1234)
    assert bench.link.read64(EXT_CORE_QOS) == 0x34
    assert bench.link.read64(QOS) == 1
    bench.link.write64(QOS, (500 << 16) | 0xAB07)
    assert bench.link.read64(QOS) == (500 << 16) | 0x07
    assert bench.link.read64(0x18) == 0
    assert bench.link.read64(0xF8) == 0


@pytest.mark.parametrize("offset", [0x04, 0x81, 0x100, 0x1000])
def test_the_window_takes_aligned_accesses_inside_itself(bench, offset):
    with pytest.raises(ValueError):
        bench.link.read64(offset)
    with pytest.raises(ValueError):
        bench.link.write64(offset, 0)


def test_release_hands_the_chiplet_its_boot_words(bench):
    bench.link.write64(BOOT_ADDR, EXT_BASE + 0x100)
    bench.link.write64(BOOT_LEN, 40)
    bench.link.write64(BOOT_ARG, EXT_BASE + 0x800)
    bench.link.write64(CTRL, 0)
    assert bench.chiplet.released
    assert bench.chiplet.releases == [(EXT_BASE + 0x100, 40, EXT_BASE + 0x800)]
    assert bench.link.read64(BOOT_ADDR) == EXT_BASE + 0x100
    assert bench.link.read64(BOOT_LEN) == 40
    assert bench.link.read64(BOOT_ARG) == EXT_BASE + 0x800
    assert bench.link.read64(0xF0) == 0


# ---------------------------------------------------------------------------
#  Admission and the refusal record
# ---------------------------------------------------------------------------

def test_a_request_on_a_closed_port_is_refused_and_recorded(bench):
    bench.link.write64(CTRL, 0)
    code, done, _ = bench.read(EXT_BASE)
    assert (code, done) == (REFUSED, 0)
    assert bench.link.read64(STATUS) & FAULT
    assert bench.link.read64(FAULT_ADDR) == EXT_BASE
    info = bench.link.read64(FAULT_INFO)
    assert info & 3 == 3
    assert info & 4 == 0
    assert bench.reason() == CLOSED
    assert bench.refusals() == 1

    bench.link.write64(STATUS, FAULT)
    assert bench.link.read64(STATUS) & FAULT == 0
    assert bench.refusals() == 0


def test_nothing_is_lent_until_a_range_is_installed(bench):
    bench.open()
    assert bench.read(EXT_BASE)[0] == REFUSED
    assert bench.reason() == NOT_LENT


def test_a_read_only_range_serves_reads_alone(bench):
    bench.memory[0x100:0x108] = (0x1122_3344_5566_7788).to_bytes(8, "little")
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.open()

    code, done, data = bench.read(EXT_BASE + 0x100)
    assert (code, done, data) == (OK, 1, [0x1122_3344_5566_7788])

    assert bench.write(EXT_BASE + 0x100, 0xDEAD)[0] == REFUSED
    assert bench.word(EXT_BASE + 0x100) == 0x1122_3344_5566_7788
    info = bench.link.read64(FAULT_INFO)
    assert info & 4
    assert bench.reason() == NOT_LENT


def test_a_write_range_takes_writes_and_they_read_back(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ | WRITE)
    bench.open()
    assert bench.write(EXT_BASE + 0x108, 0xCAFE_F00D_0000_0001)[:2] == (OK, 1)
    assert bench.word(EXT_BASE + 0x108) == 0xCAFE_F00D_0000_0001
    assert bench.read(EXT_BASE + 0x108)[2] == [0xCAFE_F00D_0000_0001]
    assert bench.link.read64(STATUS) & FAULT == 0


def test_a_write_only_range_refuses_reads(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, WRITE)
    bench.open()
    assert bench.write(EXT_BASE + 0x100, 5)[0] == OK
    assert bench.read(EXT_BASE + 0x100)[0] == REFUSED


def test_the_range_bounds_are_exact(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ | WRITE)
    bench.open()
    # The last four bytes of the range are reachable.
    assert bench.write(EXT_BASE + 0x1FC, 0xAABBCCDD, size=2)[0] == OK
    # An access that crosses the limit, and one below the base.
    assert bench.read(EXT_BASE + 0x200)[0] == REFUSED
    assert bench.read(EXT_BASE + 0x1FC, 3)[0] == REFUSED
    assert bench.read(EXT_BASE + 0xF8)[0] == REFUSED
    assert bench.read(EXT_BASE + 0xFF, 0)[0] == REFUSED


def test_a_sub_word_access_moves_its_own_bytes(bench):
    bench.memory[0x100:0x108] = bytes(range(0x10, 0x18))
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ | WRITE)
    bench.open()

    # A read returns the aligned word that holds the address.
    whole = int.from_bytes(bytes(range(0x10, 0x18)), "little")
    assert bench.read(EXT_BASE + 0x103, 0)[2] == [whole]
    assert bench.read(EXT_BASE + 0x104, 2)[2] == [whole]

    # A write takes its data from the low bits.
    assert bench.write(EXT_BASE + 0x103, 0xFFEE_DDCC_BBAA_99A5, size=0)[0] == OK
    assert bench.memory[0x100:0x108] == bytes(
        [0x10, 0x11, 0x12, 0xA5, 0x14, 0x15, 0x16, 0x17])
    assert bench.write(EXT_BASE + 0x106, 0x1234_BEEF, size=1)[0] == OK
    assert bench.memory[0x106:0x108] == bytes([0xEF, 0xBE])


@pytest.mark.parametrize(
    ("addr", "size"),
    [(EXT_BASE + 0x104, 3), (EXT_BASE + 0x102, 2), (EXT_BASE + 0x101, 1)],
)
def test_a_misaligned_access_is_refused(bench, addr, size):
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ | WRITE)
    bench.open()
    assert bench.read(addr, size)[0] == REFUSED
    assert bench.reason() == MISALIGNED


@pytest.mark.parametrize(
    "addr", [0, 0x1000, EXT_BASE - 8, EXT_LIMIT, 0xFFFF_FF00_0000_0E00])
def test_a_range_over_other_memory_lends_none_of_it(bench, addr):
    bench.lend(0, 0, 0xFFFF_FFFF_FFFF_FFFF, READ | WRITE)
    bench.open()
    assert bench.read(addr)[0] == REFUSED
    assert bench.reason() == NOT_EXTERNAL
    assert bench.read(EXT_BASE)[0] == OK


def test_an_access_that_wraps_the_address_space_is_refused(bench):
    bench.lend(0, 0, 0xFFFF_FFFF_FFFF_FFFF, READ | WRITE)
    bench.open()
    assert bench.read(0xFFFF_FFFF_FFFF_FFF8, 3, 1)[0] == REFUSED
    assert bench.reason() == NOT_EXTERNAL


def test_a_range_is_installed_in_one_step(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.open()

    # A staged base and limit leave the installed range in place.
    bench.link.write64(RANGE_BASE, EXT_BASE + 0x400)
    bench.link.write64(RANGE_LIMIT, EXT_BASE + 0x500)
    assert bench.link.read64(RANGE_BASE) == EXT_BASE + 0x100
    assert bench.link.read64(RANGE_LIMIT) == EXT_BASE + 0x200
    assert bench.read(EXT_BASE + 0x100)[0] == OK
    assert bench.read(EXT_BASE + 0x400)[0] == REFUSED

    # Writing the rights installs both.
    bench.link.write64(RANGE_RIGHTS, READ)
    assert bench.link.read64(RANGE_BASE) == EXT_BASE + 0x400
    assert bench.link.read64(RANGE_RIGHTS) == READ
    assert bench.read(EXT_BASE + 0x400)[0] == OK
    assert bench.read(EXT_BASE + 0x100)[0] == REFUSED

    # Rights of zero withdraw the range.
    bench.link.write64(RANGE_RIGHTS, 0)
    assert bench.read(EXT_BASE + 0x400)[0] == REFUSED


def test_each_range_is_its_own(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.lend(5, EXT_BASE + 0x800, EXT_BASE + 0x900, READ | WRITE)
    bench.open()
    assert bench.read(EXT_BASE + 0x100)[0] == OK
    assert bench.write(EXT_BASE + 0x800, 1)[0] == OK
    assert bench.write(EXT_BASE + 0x100, 1)[0] == REFUSED
    bench.link.write64(RANGE_SEL, 0)
    assert bench.link.read64(RANGE_LIMIT) == EXT_BASE + 0x200
    bench.link.write64(RANGE_SEL, 5)
    assert bench.link.read64(RANGE_LIMIT) == EXT_BASE + 0x900


def test_a_range_index_beyond_the_ports_ranges_installs_nothing(bench):
    # Six ranges give RANGE_SEL three bits, so 6 and 7 name no range.
    bench.lend(7, EXT_BASE, EXT_LIMIT, READ | WRITE)
    assert bench.link.read64(RANGE_SEL) == 7
    assert bench.link.read64(RANGE_BASE) == 0
    assert bench.link.read64(RANGE_LIMIT) == 0
    assert bench.link.read64(RANGE_RIGHTS) == 0
    bench.open()
    assert bench.read(EXT_BASE)[0] == REFUSED
    bench.link.write64(RANGE_SEL, 8 + 2)
    assert bench.link.read64(RANGE_SEL) == 2


def test_a_memory_error_is_reported_and_is_no_refusal(bench):
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ | WRITE)
    bench.open()
    bench.link.fail_memory_at(EXT_BASE + 0x40)
    assert bench.read(EXT_BASE + 0x40)[0] == MEM_ERROR
    assert bench.write(EXT_BASE + 0x40, 1)[0] == MEM_ERROR
    assert bench.link.read64(STATUS) & FAULT == 0
    assert bench.refusals() == 0
    bench.link.fail_memory_at(None)
    assert bench.read(EXT_BASE + 0x40)[0] == OK


def test_the_first_refusal_rings_the_attention_core(bench):
    bench.link.write64(CTRL, ENABLE | ATTN_EN | (2 << 8))
    mailbox = bench.link.mailbox
    assert bench.read(EXT_BASE)[0] == REFUSED
    assert mailbox.pending_mask(2) == 1 << PORT_ID
    assert mailbox.port_pending(2)

    # Further refusals stay quiet until FAULT is cleared.
    mailbox.acknowledge(2, PORT_ID)
    assert bench.read(EXT_BASE + 8)[0] == REFUSED
    assert mailbox.pending_mask(2) == 0
    assert bench.refusals() == 2
    assert bench.link.read64(FAULT_ADDR) == EXT_BASE

    bench.link.write64(STATUS, FAULT)
    assert bench.read(EXT_BASE + 16)[0] == REFUSED
    assert mailbox.pending_mask(2) == 1 << PORT_ID
    assert bench.link.read64(FAULT_ADDR) == EXT_BASE + 16


def test_the_refusal_count_saturates(bench):
    bench.open()
    for _ in range(0x1_0002):
        bench.read(EXT_BASE)
    assert bench.refusals() == 0xFFFF


# ---------------------------------------------------------------------------
#  Doorbells and the sideband
# ---------------------------------------------------------------------------

def test_a_chiplets_doorbell_needs_doorbell_en(bench):
    mailbox = bench.link.mailbox
    bench.open()
    bench.chiplet.ring(1)
    assert mailbox.pending_mask(1) == 0

    bench.open(DOORBELL_EN)
    bench.chiplet.ring(1)
    assert mailbox.pending_mask(1) == 1 << PORT_ID
    assert mailbox.any_pending(1)
    assert mailbox.port_pending(1)


def test_a_chiplet_in_reset_rings_nothing(bench):
    bench.link.write64(CTRL, RESET_HOLD | DOORBELL_EN)
    bench.chiplet.ring(1)
    bench.chiplet.set_status(True, True)
    assert bench.link.mailbox.pending_mask(1) == 0
    assert bench.link.read64(STATUS) & (READY | STOPPED) == 0


def test_pending_doorbells_reach_the_chiplet_lowest_source_first(bench):
    mailbox = bench.link.mailbox
    bench.open()
    mailbox.send(3, PORT_ID)
    mailbox.send(1, PORT_ID)
    assert bench.link.work_waits()

    assert not bench.link.step()
    assert bench.chiplet.doorbells == [1, 3]
    assert not mailbox.any_pending(PORT_ID)
    assert bench.chiplet.steps == 1


def test_a_doorbell_the_chiplet_declines_stays_pending(bench):
    mailbox = bench.link.mailbox
    bench.open()
    bench.chiplet.accept_doorbells = False
    mailbox.send(2, PORT_ID)

    assert not bench.link.step()
    assert bench.chiplet.doorbells == []
    assert mailbox.lowest_source(PORT_ID) == 2
    assert not bench.link.work_waits()

    bench.chiplet.accept_doorbells = True
    bench.link.step()
    assert bench.chiplet.doorbells == [2]
    assert not mailbox.any_pending(PORT_ID)


def test_an_identity_word_is_read_over_the_sideband(bench):
    bench.link.write64(ID_INDEX, 3)
    assert bench.link.read64(ID_INDEX) == 3
    assert bench.link.read64(STATUS) & ID_DONE
    assert bench.link.read64(ID_DATA) == 0x4C4D_3100_0001_0000
    bench.link.write64(ID_INDEX, 0)
    assert bench.link.read64(ID_DATA) == 0x4348_4950_0000_0001
    bench.link.write64(ID_INDEX, 200)
    assert bench.link.read64(ID_DATA) == 0


def test_stop_and_the_chiplets_status_cross_the_sideband(bench):
    bench.open()
    assert bench.chiplet.stop_req == 0
    bench.open(STOP_REQ)
    assert bench.chiplet.stop_req == 1
    bench.chiplet.set_status(True, True)
    assert bench.link.read64(STATUS) & (READY | STOPPED) == READY | STOPPED
    bench.open()
    assert bench.chiplet.stop_req == 0


def test_a_stop_request_held_through_reset_is_given_at_release(bench):
    bench.link.write64(CTRL, RESET_HOLD | STOP_REQ)
    assert bench.chiplet.stop_req == 0
    bench.open(STOP_REQ)
    assert bench.chiplet.released
    assert bench.chiplet.stop_req == 1


def test_a_chiplet_returned_to_reset_keeps_its_doorbells(bench):
    mailbox = bench.link.mailbox
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ)
    bench.open()
    bench.chiplet.set_status(True, False)
    assert bench.read(EXT_BASE)[0] == OK

    bench.link.write64(CTRL, ENABLE | RESET_HOLD)
    assert bench.chiplet.resets == 1
    assert not bench.chiplet.released
    assert bench.link.read64(STATUS) & READY == 0
    assert bench.read(EXT_BASE)[0] == CANCELLED
    assert bench.link.read64(STATUS) & FAULT == 0

    # A doorbell rung meanwhile waits for the release.
    mailbox.send(0, PORT_ID)
    assert not bench.link.step()
    assert bench.chiplet.doorbells == []
    assert mailbox.lowest_source(PORT_ID) == 0

    bench.open()
    assert len(bench.chiplet.releases) == 2
    assert bench.read(EXT_BASE)[0] == OK
    bench.link.step()
    assert bench.chiplet.doorbells == [0]


def test_a_detached_chiplet_leaves_the_port_empty(bench):
    bench.open()
    bench.chiplet.set_status(True, False)
    bench.link.detach(0)
    assert not bench.chiplet.attached
    assert bench.link.read64(STATUS) & (PRESENT | READY) == 0
    with pytest.raises(RuntimeError):
        bench.chiplet.ring(0)


# ---------------------------------------------------------------------------
#  Bursts
# ---------------------------------------------------------------------------

def _fill(bench, addr, count):
    words = [0x1000_0000_0000_0000 + index for index in range(count)]
    offset = addr - EXT_BASE
    for index, word in enumerate(words):
        bench.memory[offset + 8 * index:offset + 8 * index + 8] = (
            word.to_bytes(8, "little"))
    return words


def test_a_read_burst_returns_its_words_in_order(bench):
    words = _fill(bench, EXT_BASE + 0x100, 4)
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.open()
    assert bench.read(EXT_BASE + 0x100, 3, 3) == (OK, 4, words)


def test_a_burst_that_leaves_the_range_is_refused_whole(bench):
    _fill(bench, EXT_BASE + 0x1E8, 3)
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.open()
    code, done, data = bench.read(EXT_BASE + 0x1E8, 3, 3)
    assert (code, done, data) == (REFUSED, 0, [0, 0, 0, 0])
    assert bench.refusals() == 1
    assert (bench.link.read64(FAULT_INFO) >> 8) & 0xFF == 3
    assert bench.reason() == NOT_LENT
    assert bench.read(EXT_BASE + 0x1E8, 3, 2)[0] == OK


@pytest.mark.parametrize(("addr", "size"), [(0x100, 2), (0x104, 3)])
def test_a_burst_is_made_of_whole_aligned_words(bench, addr, size):
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ)
    bench.open()
    assert bench.read(EXT_BASE + addr, size, 1)[0] == REFUSED
    assert bench.reason() == MISALIGNED


def test_a_write_burst_lands_whole(bench):
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ | WRITE)
    bench.open()
    assert bench.write(EXT_BASE + 0x110, 11, 22, 33)[:2] == (OK, 3)
    assert [bench.word(EXT_BASE + 0x110 + 8 * index)
            for index in range(3)] == [11, 22, 33]


def test_the_longest_burst_is_256_words(bench):
    words = _fill(bench, EXT_BASE, 256)
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ | WRITE)
    bench.open()
    assert bench.read(EXT_BASE, 3, 255) == (OK, 256, words)
    assert bench.read(EXT_BASE, 3, 256)[0] == REFUSED
    assert bench.reason() == MISALIGNED


def test_a_memory_error_ends_a_burst_at_its_failing_word(bench):
    bench.lend(0, EXT_BASE, EXT_LIMIT, READ | WRITE)
    bench.open()
    bench.link.fail_memory_at(EXT_BASE + 0x110 + 3)

    code, done, _ = bench.write(EXT_BASE + 0x100, 1, 2, 3, 4)
    assert (code, done) == (MEM_ERROR, 2)
    assert [bench.word(EXT_BASE + 0x100 + 8 * index)
            for index in range(4)] == [1, 2, 0, 0]

    code, done, data = bench.read(EXT_BASE + 0x100, 3, 3)
    assert (code, done, data) == (MEM_ERROR, 2, [1, 2, 0, 0])
    assert bench.link.read64(STATUS) & FAULT == 0

    bench.link.fail_memory_at(None)
    assert bench.read(EXT_BASE + 0x100)[0] == OK


# ---------------------------------------------------------------------------
#  Save and restore
# ---------------------------------------------------------------------------

def test_the_links_state_survives_a_save_and_restore(native, bench):
    bench.link.write64(BOOT_ADDR, EXT_BASE + 0x100)
    bench.link.write64(BOOT_LEN, 40)
    bench.link.write64(BOOT_ARG, EXT_BASE + 0x440)
    bench.lend(0, EXT_BASE + 0x100, EXT_BASE + 0x200, READ)
    bench.lend(3, EXT_BASE + 0x400, EXT_BASE + 0x500, READ | WRITE)
    bench.link.write64(RANGE_BASE, 0x77)
    bench.link.write64(QOS, (9 << 16) | 4)
    bench.link.write64(EXT_CORE_QOS, 5)
    bench.link.write64(ID_INDEX, 1)
    bench.open(DOORBELL_EN | ATTN_EN | (3 << 8))
    bench.chiplet.set_status(True, False)
    bench.read(EXT_BASE)            # one refusal, recorded
    bench.chiplet.blob = [1, 2, 3]
    bench.link.set_budget(0, 7)
    saved = bench.link.save()

    other = Bench(native)
    assert other.link.restore(saved)
    assert other.chiplet.blob == [1, 2, 3]
    for offset in range(0, 0x100, 8):
        assert other.link.read64(offset) == bench.link.read64(offset), offset
    other.link.write64(RANGE_SEL, 0)
    assert other.link.read64(RANGE_LIMIT) == EXT_BASE + 0x200
    # The restored port is open under the restored ranges.
    assert other.write(EXT_BASE + 0x400, 0xAB)[0] == OK
    assert other.read(EXT_BASE + 0x100)[0] == OK
    assert other.write(EXT_BASE + 0x100, 1)[0] == REFUSED
    # The staged base came back too: installing it moves range 0.
    other.link.write64(RANGE_RIGHTS, READ)
    assert other.link.read64(RANGE_BASE) == 0x77


def test_a_state_is_refused_by_a_link_of_another_shape(native, bench):
    saved = bench.link.save()
    assert not Bench(native, attach=False).link.restore(saved)
    two_ports = native.ChipletLink(bytearray(EXT_SIZE), EXT_BASE, 16, 2, 6)
    assert not two_ports.restore(saved)
    four_ranges = native.ChipletLink(bytearray(EXT_SIZE), EXT_BASE, 16, 1, 4)
    four_ranges.attach_manual_chiplet(0, native.ManualChiplet())
    assert not four_ranges.restore(saved)
    assert not bench.link.restore(saved[:-1])
    assert not bench.link.restore(saved + b"\0")
    assert bench.link.restore(saved)
