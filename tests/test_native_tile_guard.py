"""The native tile admission guard observes late changes without callbacks."""

from __future__ import annotations

import importlib
import struct
from types import ModuleType

import pytest

from shared import tile_float
from simulator import tile
from simulator.field import HostedFieldALUService
from simulator.memory import SparseAddressSpace


@pytest.fixture(params=("_megaforth_native", "_mp64_accel"))
def native(request):
    extension = importlib.import_module(request.param)
    assert extension.TILE_GUARD_API_VERSION == 1
    return extension


def _arguments():
    return (
        tile.HostedTileService, tile._REFERENCE_TILE_METHODS,
        SparseAddressSpace, tile._REFERENCE_MEMORY_METHODS,
        HostedFieldALUService, tile._REFERENCE_REGISTER_METHODS,
        tile_float, tile._REFERENCE_VALUE_HELPERS,
    )


def _context():
    memory = SparseAddressSpace(bank0_size=0x1000)
    registers = HostedFieldALUService(core_count=1)
    service = tile.HostedTileService(memory, registers)
    return service, memory, registers


@pytest.mark.parametrize("owner,name", (
    ("service_class", "_binary"),
    ("memory_class", "write_bytes"),
    ("registers_class", "accumulator_words"),
    ("memory", "_read_integer"),
    ("registers", "_cell"),
    ("helpers", "elementwise"),
))
def test_every_owner_is_rechecked_after_binding(native, monkeypatch, owner, name):
    context = _context()
    guard = native.TileIdentityGuard(*_arguments())
    assert guard.matches(*context)
    owners = dict(zip(("service", "memory", "registers"), context))
    owners.update(service_class=tile.HostedTileService,
                  memory_class=SparseAddressSpace,
                  registers_class=HostedFieldALUService, helpers=tile_float)

    class Replacement:
        def __eq__(self, other):
            raise AssertionError("identity admission must not call equality")

    with monkeypatch.context() as patch:
        if owner in ("memory", "registers"):
            # Restore absence in the instance dictionary, rather than leaving
            # an injected bound-method copy after monkeypatch.setattr undo.
            patch.setitem(vars(owners[owner]), name, Replacement())
        else:
            patch.setattr(owners[owner], name, Replacement())
        assert not guard.matches(*context)
    assert guard.matches(*context)


def test_hot_guard_avoids_python_loop_and_late_helper_uses_reference(native, monkeypatch):
    service, memory, registers = _context()
    service.set_mode(7)
    service.set_source0(0x100)
    service.set_source1(0x200)
    service.set_destination(0x300)
    memory.write_bytes(0x100, struct.pack("<8d", *([2.0] * 8)))
    memory.write_bytes(0x200, struct.pack("<8d", *([3.0] * 8)))
    calls = []

    def execute(*args):
        calls.append(args[0])
        return native.tile_execute_values(*args)

    assert service.bind_native_values(execute, guard_factory=native.TileIdentityGuard)

    def forbidden(*args):
        raise AssertionError("hot operation used the Python identity loop")

    monkeypatch.setattr(tile, "_canonical_methods", forbidden)
    service.add()
    assert memory.read_bytes(0x300, 64) == struct.pack("<8d", *([5.0] * 8))
    with monkeypatch.context() as patch:
        patch.setattr(tile_float, "elementwise",
                      lambda fmt, op, left, right: [0x4045000000000000] * len(left))
        service.add()
        assert memory.read_bytes(0x300, 64) == struct.pack("<8d", *([42.0] * 8))
    service.add()
    assert calls == ["add", "add"]
    assert memory.read_bytes(0x300, 64) == struct.pack("<8d", *([5.0] * 8))


@pytest.mark.parametrize("magic", ("__getattribute__", "__getattr__", "__setattr__", "__delattr__"))
def test_custom_attribute_routing_declines_without_executing_it(native, monkeypatch, magic):
    context = _context()
    guard = native.TileIdentityGuard(*_arguments())

    def forbidden(*args):
        raise AssertionError("guard executed custom attribute routing")

    with monkeypatch.context() as patch:
        patch.setattr(SparseAddressSpace, magic, forbidden, raising=False)
        assert not guard.matches(*context)
    assert guard.matches(*context)


def test_hostile_instance_dictionary_key_never_receives_equality(native):
    context = _context()
    guard = native.TileIdentityGuard(*_arguments())

    class Key:
        def __hash__(self):
            return hash("read_bytes")

        def __eq__(self, other):
            raise AssertionError("guard compared an injected dictionary key")

    # No read_bytes instance entry exists, so insertion itself has no equality
    # collision. A naive subsequent dict lookup would collide with this key.
    key = Key()
    context[1].__dict__[key] = object()
    assert not guard.matches(*context)
    del context[1].__dict__[key]
    assert guard.matches(*context)


def test_missing_helper_never_calls_module_getattr(native, monkeypatch):
    context = _context()
    guard = native.TileIdentityGuard(*_arguments())

    def forbidden(name):
        raise AssertionError("guard invoked module __getattr__")

    with monkeypatch.context() as patch:
        patch.delattr(tile_float, "elementwise")
        patch.setattr(tile_float, "__getattr__", forbidden, raising=False)
        assert not guard.matches(*context)


def test_exact_types_and_module_identity_are_required(native, monkeypatch):
    service, memory, registers = _context()
    guard = native.TileIdentityGuard(*_arguments())

    class Memory(SparseAddressSpace):
        pass

    assert not guard.matches(service, Memory(bank0_size=0x1000), registers)

    class Helpers(ModuleType):
        pass

    with monkeypatch.context() as patch:
        patch.setattr(tile_float, "__class__", Helpers)
        assert not guard.matches(service, memory, registers)
    assert guard.matches(service, memory, registers)


def test_replacing_helper_module_declines_even_while_original_stays_canonical(native, monkeypatch):
    service, memory, registers = _context()
    assert service.bind_native_values(native.tile_execute_values,
                                      guard_factory=native.TileIdentityGuard)
    replacement = ModuleType("replacement_tile_float")
    replacement.__dict__.update(vars(tile_float))
    replacement.elementwise = object()
    monkeypatch.setattr(tile, "tile_float", replacement)
    assert service._native_value_executor() is None


def test_unbounded_custom_namespace_declines(native):
    context = _context()
    guard = native.TileIdentityGuard(*_arguments())
    context[1].__dict__.update((f"custom_{index}", None) for index in range(4097))
    assert not guard.matches(*context)


@pytest.mark.parametrize("entries,error", (
    ([], TypeError),
    ((["x", object()],), TypeError),
    (((1, object()),), TypeError),
    ((("", object()),), ValueError),
    ((("same", object()), ("same", object())), ValueError),
    (tuple((str(index), object()) for index in range(129)), ValueError),
))
def test_identity_description_is_exact_and_bounded(native, entries, error):
    arguments = list(_arguments())
    arguments[1] = entries
    with pytest.raises(error):
        native.TileIdentityGuard(*arguments)


def test_rebinding_without_factory_retains_python_fallback_and_disable_clears_guard(native):
    service, memory, registers = _context()
    execute = native.tile_execute_values
    assert service.bind_native_values(execute, guard_factory=native.TileIdentityGuard)
    assert service._native_guard_matches is not None
    assert service.bind_native_values(execute)
    assert service._native_guard_matches is None
    assert service._native_value_executor() is execute
    memory.read_bytes = lambda *_: bytes(64)
    assert service._native_value_executor() is None
    assert service.bind_native_values(None)
    assert service._native_values is None and service._native_guard_matches is None
