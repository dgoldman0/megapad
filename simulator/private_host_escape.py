"""Scoped provenance for a trusted host error whose type is also guest ABORT.

The exception is never marked or rewritten.  One runtime-local record follows
only the original traceback through already-live ordinary runtime guards.  A
new entry, an explicit reuse, or a changed traceback retires that authority.
This is not a general exception whitelist or a guest-visible capability.
"""

from __future__ import annotations

import sys

from simulator.interop_exports import (
    CallbackExportEngine, _ActiveExport, _CanonicalLeaf, _ExportBinding,
)


_TRACEBACK = BaseException.__dict__["__traceback__"]
_GETFRAME = sys._getframe
_EXCEPTION = sys.exception
_LEAF_ROUTES = tuple((name, vars(CallbackExportEngine)[name]) for name in (
    "_binding", "_require_binding", "_require_leaf",
))


def _clear(record):
    record._error = None
    record._cursor = None
    record._cursor_frame = None
    record._matched_scope = None


def _is_scope(record, frame):
    return (any(frame.f_code is code for code in record._scope_codes)
            and frame.f_locals.get("self") is record._runtime)


def _outer_scope(record, frame):
    frame = frame.f_back
    while frame is not None:
        if _is_scope(record, frame):
            return frame
        frame = frame.f_back
    return None


class PrivateHostAbortProvenance:
    """One issued primitive escape, bounded by its live Python unwind path."""

    __slots__ = ("_runtime", "_issuer_code", "_scope_codes", "_error",
                 "_cursor", "_cursor_frame", "_matched_scope", "_leaf_scope")

    def __init__(self, runtime, runtime_class):
        self._runtime = runtime
        self._issuer_code = runtime_class._invoke_primitive.__code__
        self._scope_codes = tuple(getattr(runtime_class, name).__code__ for name in (
            "_execute_guarded", "_resume_guarded", "_execute_guest_fault_guarded",
            "_evaluate_source",
        ))
        self._leaf_scope = None
        _clear(self)

    def enter(self):
        # An entry after issuance means someone caught the exception and started
        # another operation.  Existing enclosing scopes entered before issuance.
        _clear(self)
        self._leaf_scope = None

    def capture_leaf(self, word, context):
        """Pin an already-issued V2 leaf before its ordinary private tick.

        The older leaf dispatcher deliberately uses the ordinary guard. Its
        host tick must not normalize a raw host ForthAbort before the enclosing
        machine primitive sees it. No source Word or merely similar context
        can acquire this exact active-engine admission.
        """
        frame = _GETFRAME(1)
        if frame.f_code is not self._scope_codes[0] or not _is_scope(self, frame):
            return
        namespace = object.__getattribute__(self._runtime, "__dict__")
        engine = dict.get(namespace, "_callback_exports")
        if type(engine) is not CallbackExportEngine:
            return
        values = object.__getattribute__(engine, "__dict__")
        active = dict.get(values, "_active")
        if type(active) is not _ActiveExport or active.context is not context:
            return
        if (dict.get(values, "_runtime") is not self._runtime
                or any(name in values or vars(CallbackExportEngine).get(name) is not route
                       for name, route in _LEAF_ROUTES)):
            return
        binding = active.binding
        if (type(binding) is not _ExportBinding or type(binding.leaf) is not _CanonicalLeaf
                or binding.closed is not None or binding.service is not None
                or binding.leaf.word is not word or context.data is not active.data
                or context.returns is not active.returns):
            return
        issued = _LEAF_ROUTES[0][1](engine, binding.handle)
        if issued is binding:
            self._leaf_scope = frame

    def issue_leaf(self, error):
        frame = _GETFRAME(1)
        if self._leaf_scope is not frame or not _is_scope(self, frame):
            return
        traceback = _TRACEBACK.__get__(error, BaseException)
        if traceback is None or traceback.tb_frame is not frame:
            return
        _clear(self)
        self._error = error
        self._cursor = traceback
        self._cursor_frame = frame
        self._matched_scope = frame

    def issue(self, admission, implementation, callback, error):
        """Called only by the pinned canonical primitive invocation route."""

        frame = _GETFRAME(1)
        _clear(self)
        namespace = object.__getattribute__(self._runtime, "__dict__")
        admissions = dict.get(namespace, "_primitive_host_escapes")
        if (frame.f_code is not self._issuer_code
                or frame.f_locals.get("self") is not self._runtime
                or type(admissions) is not dict
                or dict.get(admissions, id(implementation)) is not admission
                or type(admission) is not tuple or len(admission) != 2
                or admission[0] is not implementation or admission[1] is not callback
                or _outer_scope(self, frame) is None):
            return
        traceback = _TRACEBACK.__get__(error, BaseException)
        if traceback is None or traceback.tb_frame is not frame:
            return
        self._error = error
        self._cursor = traceback
        self._cursor_frame = frame

    def matches(self, error):
        """Accept only automatic propagation into this exact live guard."""

        if self._error is not error:
            _clear(self)
            return False
        frame = _GETFRAME(1)
        traceback = _TRACEBACK.__get__(error, BaseException)
        if not _is_scope(self, frame) or traceback is None:
            _clear(self)
            return False
        cursor = self._cursor
        if traceback is cursor:
            valid = self._matched_scope is frame
        else:
            # New traceback nodes run outer-to-inner, while f_back runs in the
            # opposite direction.  Every node must be a still-live caller of
            # the previous accepted cursor, exactly once, ending at that same
            # traceback object. Explicit ``raise error`` introduces a repeated
            # or non-caller frame and therefore cannot reuse this authority.
            node = traceback
            previous = None
            valid = node.tb_frame is frame
            while valid and node is not cursor:
                if node is None:
                    valid = False
                    break
                current = node.tb_frame
                if previous is not None and current.f_back is not previous:
                    valid = False
                    break
                previous = current
                node = node.tb_next
            valid = (valid and node is cursor and previous is not None
                     and self._cursor_frame.f_back is previous)
        if not valid:
            _clear(self)
            return False
        self._cursor = traceback
        self._cursor_frame = frame
        self._matched_scope = frame
        return True

    def leave(self):
        frame = _GETFRAME(1)
        if self._leaf_scope is frame:
            self._leaf_scope = None
        if self._error is None:
            return
        if (self._matched_scope is not frame or _EXCEPTION() is not self._error
                or _outer_scope(self, frame) is None):
            _clear(self)
