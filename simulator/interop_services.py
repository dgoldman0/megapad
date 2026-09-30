"""Isolated identity/provenance foundation for the private V5 scalar profile.

Nothing here registers an export, charges a semantic tick, invokes a Word,
enters native execution, or issues a guest-fault receipt. The existing export
engine must eventually own those actions and arm a validation boundary only
after its private-stack checks and charged tick. Captures retain the original
core Words; numerical descriptors and validation observations are not handles.
"""

from __future__ import annotations

from dataclasses import dataclass
from types import BuiltinFunctionType, FunctionType, ModuleType
import sys

from shared import cells, ieee_fp, scalar_fp
from shared.hybrid_services import ServiceExportV5
from simulator import core_words, scalar_float
from simulator.dictionary import Dictionary, Word
from simulator.errors import IllegalInstructionFault, InstructionFault, ExecutionError, SimulatorError
from simulator.interop_exports import CallbackExportError
from simulator.native_execution import NativeExecutor
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition
from simulator.scalar_float import HostedScalarFloatService


_MAX_NAMESPACE_KEYS = 4096
_MAX_DICTIONARY_WORDS = 65536
_MODULE_DICT = ModuleType.__dict__["__dict__"]
_MISSING = object()
_EXCEPTION_NEW = BaseException.__new__
_EXCEPTION_INIT = BaseException.__init__


def _capture_error(message):
    # Admission must be able to reject a changed inherited exception
    # constructor without executing that constructor to report the rejection.
    error = _EXCEPTION_NEW(CallbackExportError)
    _EXCEPTION_INIT(error, message)
    return error

# This finite mapping is independent of mutable BIOS_WORDS metadata.
_SCALAR_WORDS = (
    ("FPCSR@", "fetch", None), ("FPCSR!", "store", None),
    ("F32+", "binary", 0x00), ("F32-", "binary", 0x01),
    ("F32*", "binary", 0x02), ("F32/", "binary", 0x03),
    ("F32SQRT", "unary", 0x04), ("F32FMA", "fma", 0x07),
    ("F64+", "binary", 0x40), ("F64-", "binary", 0x41),
    ("F64*", "binary", 0x42), ("F64/", "binary", 0x43),
    ("F64SQRT", "unary", 0x44), ("F64FMA", "fma", 0x47),
)


def _keys(namespace):
    # Scan before ANY string lookup: a non-exact key may run __eq__ during
    # dict.get and change a route that was already checked.
    if len(namespace) > _MAX_NAMESPACE_KEYS or any(type(key) is not str for key in namespace):
        raise _capture_error("scalar service namespace is not canonical")
    return namespace


def _module_namespace(module):
    if type(module) is not ModuleType:
        raise _capture_error("scalar service module owner changed")
    return _keys(_MODULE_DICT.__get__(module))


def _class_namespace(cls):
    if type(cls) is not type:
        raise _capture_error("scalar service metaclass changed")
    return _keys(type.__getattribute__(cls, "__dict__"))


@dataclass(frozen=True, slots=True)
class _FunctionSeal:
    function: object
    code: object
    globals: object
    defaults: object
    kwdefaults: object
    closure: object
    contents: tuple

    @classmethod
    def capture(cls, function):
        if type(function) is not FunctionType:
            raise _capture_error("scalar service requires its original Python closure")
        closure = function.__closure__
        return cls(function, function.__code__, function.__globals__,
                   function.__defaults__, function.__kwdefaults__, closure,
                   () if closure is None else tuple(cell.cell_contents for cell in closure))

    def verify(self):
        function = self.function
        if (type(function) is not FunctionType or function.__code__ is not self.code
                or function.__globals__ is not self.globals
                or function.__defaults__ is not self.defaults
                or function.__kwdefaults__ is not self.kwdefaults
                or function.__closure__ is not self.closure):
            raise _capture_error("scalar service function implementation changed")
        if self.closure is not None:
            try:
                intact = all(cell.cell_contents is value
                             for cell, value in zip(self.closure, self.contents))
            except ValueError:
                intact = False
            if not intact:
                raise _capture_error("scalar service closure contents changed")


class _ClassSeal:
    def __init__(self, cls, *, functions=False):
        self.cls = cls
        self.mro = type.__getattribute__(cls, "__mro__")
        # copyreg may lazily add this derived cache during an otherwise
        # harmless metadata copy. It is not an executable route. Accept only
        # the exact standard cache, without relaxing any other namespace pin.
        self.slot_names = []
        for base in self.mro:
            slots = _class_namespace(base).get("__slots__", ())
            slots = (slots,) if type(slots) is str else slots
            if type(slots) is not tuple or any(type(name) is not str for name in slots):
                raise _capture_error("scalar service class slots are not canonical")
            for name in slots:
                if name in ("__dict__", "__weakref__"):
                    continue
                if name.startswith("__") and not name.endswith("__"):
                    owner = type.__getattribute__(base, "__name__").lstrip("_")
                    name = f"_{owner}{name}" if owner else name
                self.slot_names.append(name)
        self.slot_names = tuple(self.slot_names)
        self.entries = tuple((name, value) for name, value in _class_namespace(cls).items()
                             if name != "__slotnames__")
        self.dictionary_descriptor = dict(self.entries).get("__dict__")
        self.functions = tuple(
            _FunctionSeal.capture(function)
            for _name, value in self.entries
            for function in ((value.fget, value.fset, value.fdel) if type(value) is property else (value,))
            if functions and type(function) is FunctionType
        )

    def verify(self):
        namespace = _class_namespace(self.cls)
        cache = namespace.get("__slotnames__", _MISSING)
        if cache is not _MISSING and (
                type(cache) is not list or len(cache) != len(self.slot_names)
                or any(type(name) is not str for name in cache)
                or tuple(cache) != self.slot_names):
            raise _capture_error("scalar service derived slot cache changed")
        if (type.__getattribute__(self.cls, "__mro__") is not self.mro
                or len(namespace) != len(self.entries) + (cache is not _MISSING)
                or any(namespace.get(name) is not value for name, value in self.entries)):
            raise _capture_error("scalar service class route changed")
        for function in self.functions:
            function.verify()

    def instance(self, instance):
        self.verify()
        if type(instance) is not self.cls:
            raise _capture_error("scalar service requires exact canonical owners")
        if self.dictionary_descriptor is None:
            return None
        namespace = self.dictionary_descriptor.__get__(instance, self.cls)
        if type(namespace) is not dict:
            raise _capture_error("scalar service object namespace changed")
        return _keys(namespace)


class _ModuleSeal:
    def __init__(self, module, names):
        self.module = module
        namespace = _module_namespace(module)
        self.entries = tuple((name, namespace[name]) for name in names)
        self.functions = tuple(_FunctionSeal.capture(value) for _name, value in self.entries
                               if type(value) is FunctionType)

    def verify(self):
        namespace = _module_namespace(self.module)
        if any(namespace.get(name, _MISSING) is not value for name, value in self.entries):
            raise _capture_error("scalar service canonical helper changed")
        for function in self.functions:
            function.verify()


_SERVICE_CLASS = _ClassSeal(HostedScalarFloatService, functions=True)
_BOUNDARY_CLASS = _ClassSeal(scalar_float._ValidationBoundary, functions=True)
_WORD_CLASS = _ClassSeal(Word)
_PRIMITIVE_CLASS = _ClassSeal(PrimitiveDefinition)
_RUNTIME_CLASS = _ClassSeal(MegaForthRuntime)
_DICTIONARY_CLASS = _ClassSeal(Dictionary)
_EXECUTOR_CLASS = _ClassSeal(NativeExecutor)
_EXPORT_CLASS = _ClassSeal(ServiceExportV5, functions=True)
_OUTCOME_CLASS = _ClassSeal(scalar_fp.Outcome, functions=True)
_FORMAT_CLASS = _ClassSeal(ieee_fp.Format, functions=True)
_ILLEGAL_OPERATION_CLASS = _ClassSeal(scalar_fp.IllegalOperation, functions=True)
_ILLEGAL_SCALAR_CLASS = _ClassSeal(scalar_float.IllegalScalarFloatError, functions=True)
_EXCEPTION_BASES = tuple(_ClassSeal(cls, functions=True) for cls in (
    IllegalInstructionFault, InstructionFault, ExecutionError, SimulatorError,
))
_COMMON_MODULES = (
    _ModuleSeal(core_words, ("scalar_fp", "_scalar_float_word")),
    _ModuleSeal(scalar_float, tuple(name for name in vars(scalar_float) if not name.startswith("__"))),
    _ModuleSeal(scalar_fp, tuple(name for name in vars(scalar_fp) if not name.startswith("__"))),
    _ModuleSeal(cells, ("u64", "MASK64")),
    _ModuleSeal(ieee_fp, ("RMM",)),
)
_ORACLE_MODULE = _ModuleSeal(ieee_fp, tuple(name for name in vars(ieee_fp) if not name.startswith("__")))
_SQRT_MODULE = _ModuleSeal(ieee_fp.math, ("isqrt",))
_FORMATS = tuple((fmt, tuple(vars(fmt).items())) for fmt in (ieee_fp.FP32, ieee_fp.FP64))
_TEMPLATES = tuple((name, core_words._scalar_float_word(None, shape, op).__code__)
                   for name, shape, op in _SCALAR_WORDS)
_BEGIN = HostedScalarFloatService._begin_validation_boundary
_FINISH = HostedScalarFloatService._finish_validation_boundary
_FPCSR_SLOT = HostedScalarFloatService.__dict__["_fpcsr"]
_KERNEL_SLOT = HostedScalarFloatService.__dict__["_native_execute"]
_SCOPE_SLOT = HostedScalarFloatService.__dict__["_validation_scope"]
_ARMED_SLOT = HostedScalarFloatService.__dict__["_validation_armed"]
_VALIDATE_DESCRIPTOR = ServiceExportV5.__post_init__


@dataclass(frozen=True, slots=True)
class _OriginalScalarWord:
    name: str
    operation: int | None
    word: Word
    xt: int
    implementation: PrimitiveDefinition
    function: _FunctionSeal


def _require_word(dictionary_namespace, original):
    _WORD_CLASS.instance(original.word)
    _PRIMITIVE_CLASS.instance(original.implementation)
    by_xt = dictionary_namespace.get("_by_xt")
    if (type(by_xt) is not dict or len(by_xt) > _MAX_DICTIONARY_WORDS
            or any(type(key) is not int for key in by_xt)):
        raise _capture_error("scalar service dictionary token map changed")
    word = original.word
    if (by_xt.get(original.xt) is not word or type(word.xt) is not int
            or word.xt != original.xt or word.implementation is not original.implementation
            or original.implementation.callback is not original.function.function):
        raise _capture_error("scalar service original Word is stale or changed")
    original.function.verify()


class ScalarServiceCatalog:
    """Original core identities, with one later backend-selection finalization.

    Construct at the core-install boundary. ``finalize_executor`` is a trusted
    constructor seam after backend selection, not permission to choose a new
    kernel at export publication. This catalog is not an export registry.
    """

    @classmethod
    def capture(cls, runtime, *, core_installed: bool):
        if type(core_installed) is not bool:
            raise TypeError("core_installed must be an exact bool")
        for base in _EXCEPTION_BASES:
            base.verify()
        namespace = _RUNTIME_CLASS.instance(runtime)
        dictionary = namespace.get("dictionary")
        dictionary_namespace = _DICTIONARY_CLASS.instance(dictionary)
        service = namespace.get("scalar_float")
        _SERVICE_CLASS.instance(service)
        for module in _COMMON_MODULES:
            module.verify()
        result = cls()
        result._runtime, result._dictionary, result._service = runtime, dictionary, service
        result._finalized = False
        result._failed = False
        result._executor = result._extension = result._kernel = None
        originals = []
        if core_installed:
            definitions = dictionary_namespace.get("_definitions")
            if type(definitions) is not list or len(definitions) > _MAX_DICTIONARY_WORDS:
                raise _capture_error("scalar service requires canonical core definitions")
            for name, _shape, operation in _SCALAR_WORDS:
                # Use the fresh definition list, without name-lookup callbacks.
                matches = []
                for word in definitions:
                    _WORD_CLASS.instance(word)
                    if type(word.name) is not bytes:
                        raise _capture_error("scalar service Word name is not canonical")
                    if word.name == name.encode("ascii"):
                        matches.append(word)
                if len(matches) != 1:
                    raise _capture_error("scalar service capture requires the original core-install boundary")
                word = matches[0]
                if type(word.xt) is not int or not 0 < word.xt <= cells.MASK64:
                    raise _capture_error("scalar service Word XT is not canonical")
                implementation = word.implementation
                _PRIMITIVE_CLASS.instance(implementation)
                function = _FunctionSeal.capture(implementation.callback)
                if (function.code is not dict(_TEMPLATES)[name]
                        or function.globals is not _module_namespace(core_words)
                        or function.defaults is not None or function.kwdefaults is not None):
                    raise _capture_error("scalar service Word is not the original canonical closure")
                expected = {"service": service} if operation is None else {"service": service, "op": operation}
                if len(function.contents) != len(expected):
                    raise _capture_error("scalar service closure shape changed")
                for field, value in zip(function.code.co_freevars, function.contents):
                    wanted = expected.get(field, _MISSING)
                    if field == "op":
                        if type(value) is not int or value != wanted:
                            raise _capture_error("scalar service closure opcode changed")
                    elif value is not wanted:
                        raise _capture_error("scalar service closure owner changed")
                original = _OriginalScalarWord(name, operation, word, word.xt, implementation, function)
                _require_word(dictionary_namespace, original)
                originals.append(original)
        result._originals = tuple(originals)
        return result

    def finalize_executor(self, executor):
        if self._finalized:
            raise _capture_error("scalar service executor was already finalized")
        namespace = self._verify_owner()
        if namespace.get("_native_execution", _MISSING) is not executor:
            raise _capture_error("scalar service executor is not the runtime's selected executor")
        kernel = _KERNEL_SLOT.__get__(self._service)
        extension = None
        if executor is None:
            if kernel is not None:
                raise _capture_error("Python scalar service cannot select a native or custom kernel")
        else:
            values = _EXECUTOR_CLASS.instance(executor)
            extension = values.get("extension")
            module = _module_namespace(extension)
            if (values.get("runtime") is not self._runtime
                    or values.get("scalar_float") is not self._service
                    or sys.modules.get("_megaforth_native") is not extension
                    or type(kernel) is not BuiltinFunctionType
                    or kernel.__name__ != "scalar_fp_execute"
                    or kernel.__module__ != "_megaforth_native"
                    or module.get("scalar_fp_execute") is not kernel):
                raise _capture_error("scalar service native value executor was not admitted here")
        self._executor, self._extension, self._kernel = executor, extension, kernel
        self._finalized = True
        self.verify()

    def _verify_owner(self):
        if self._failed:
            raise _capture_error("scalar service capture failed closed")
        namespace = _RUNTIME_CLASS.instance(self._runtime)
        if namespace.get("dictionary") is not self._dictionary or namespace.get("scalar_float") is not self._service:
            raise _capture_error("scalar service runtime owner changed")
        _SERVICE_CLASS.instance(self._service)
        _BOUNDARY_CLASS.verify()
        _ILLEGAL_OPERATION_CLASS.verify()
        _ILLEGAL_SCALAR_CLASS.verify()
        for base in _EXCEPTION_BASES:
            base.verify()
        for module in _COMMON_MODULES:
            module.verify()
        value = _FPCSR_SLOT.__get__(self._service)
        if type(value) is not int or value < 0 or value & ~0x1F7:
            raise _capture_error("scalar service FPCSR is not a canonical cell")
        return namespace

    def verify(self):
        namespace = self._verify_owner()
        if not self._finalized:
            raise _capture_error("scalar service executor has not been finalized")
        if (namespace.get("_native_execution", _MISSING) is not self._executor
                or _KERNEL_SLOT.__get__(self._service) is not self._kernel):
            raise _capture_error("scalar service selected executor changed")
        if self._executor is not None:
            values = _EXECUTOR_CLASS.instance(self._executor)
            module = _module_namespace(self._extension)
            if (values.get("extension") is not self._extension
                    or values.get("runtime") is not self._runtime
                    or values.get("scalar_float") is not self._service
                    or sys.modules.get("_megaforth_native") is not self._extension
                    or module.get("scalar_fp_execute") is not self._kernel):
                raise _capture_error("scalar service native executor owner changed")
        else:
            _ORACLE_MODULE.verify()
            _SQRT_MODULE.verify()
            _OUTCOME_CLASS.verify()
            _FORMAT_CLASS.verify()
            for fmt, entries in _FORMATS:
                attributes = _FORMAT_CLASS.instance(fmt)
                if len(attributes) != len(entries) or any(attributes.get(name) is not value for name, value in entries):
                    raise _capture_error("scalar service format values changed")
        _DICTIONARY_CLASS.instance(self._dictionary)

    def bind(self, descriptor):
        self.verify()
        _EXPORT_CLASS.instance(descriptor)
        _VALIDATE_DESCRIPTOR(descriptor)
        original = next((item for item in self._originals if item.name == descriptor.name), None)
        if original is None:
            raise _capture_error("scalar service Word was not captured at core installation")
        # Own an independent numerical copy, without granting export authority.
        descriptor = ServiceExportV5(export_id=descriptor.export_id, name=descriptor.name,
                                     input_cells=descriptor.input_cells, output_cells=descriptor.output_cells)
        result = ScalarServiceCapture(self, descriptor, original)
        result.verify()
        return result


@dataclass(frozen=True, slots=True)
class ScalarServiceCapture:
    catalog: ScalarServiceCatalog
    descriptor: ServiceExportV5
    original: _OriginalScalarWord

    @property
    def word(self):
        return self.original.word

    @property
    def callback(self):
        return self.original.function.function

    def verify(self):
        self.catalog.verify()
        if not any(self.original is item for item in self.catalog._originals):
            raise _capture_error("scalar service Word was not captured by this catalog")
        _EXPORT_CLASS.instance(self.descriptor)
        _VALIDATE_DESCRIPTOR(self.descriptor)
        if self.descriptor.name != self.original.name:
            raise _capture_error("scalar service descriptor no longer matches its original Word")
        namespace = _DICTIONARY_CLASS.instance(self.catalog._dictionary)
        _require_word(namespace, self.original)

    def validation_boundary(self):
        return _ServiceValidationScope(self)


@dataclass(frozen=True, slots=True)
class ScalarValidationFailure:
    """Copyable local observation, not an engine-issued V5 failure receipt."""

    cause: BaseException
    operation: int
    fpcsr: int


class _ServiceValidationScope:
    def __init__(self, capture):
        self._capture = capture
        self._boundary = None
        self._entered = False
        self.failure = None

    def __enter__(self):
        if self._entered:
            raise _capture_error("scalar validation scope is one-shot")
        self._capture.verify()
        service = self._capture.catalog._service
        if _SCOPE_SLOT.__get__(service) is not None or _ARMED_SLOT.__get__(service) is not None:
            raise _capture_error("scalar validation scope is already active")
        self._boundary = _BEGIN(service, self._capture.original.operation)
        self._entered = True
        return self

    def __exit__(self, kind, error, traceback):
        if not self._entered or self._boundary is None:
            raise _capture_error("scalar validation scope is not active")
        boundary, self._boundary = self._boundary, None
        catalog = self._capture.catalog
        try:
            evidence = _FINISH(catalog._service, boundary, error)
            if evidence is not None:
                self.failure = ScalarValidationFailure(*evidence)
        except BaseException:
            catalog._failed = True
            if error is None:
                raise
            try:
                BaseException.add_note(error, "scalar validation cleanup failed; capture disabled")
            except BaseException:
                pass
            # Preserve the original host exception object and type.
        return False


__all__ = ["ScalarServiceCatalog", "ScalarServiceCapture", "ScalarValidationFailure"]
