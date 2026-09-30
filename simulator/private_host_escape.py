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
                 "_cursor", "_cursor_frame", "_matched_scope", "_leaf_scope",
                 "_task_installation", "_task_deadline_installation")

    def __init__(self, runtime, runtime_class):
        self._runtime = runtime
        self._issuer_code = runtime_class._invoke_primitive.__code__
        self._scope_codes = tuple(getattr(runtime_class, name).__code__ for name in (
            "_execute_guarded", "_resume_guarded", "_execute_guest_fault_guarded",
            "_evaluate_source",
        ))
        self._leaf_scope = None
        self._task_installation = None
        self._task_deadline_installation = None
        _clear(self)

    def install_task_issuers(self, engine, dispatch_class):
        """Capture only the original task engine's admitted host-call boundaries.

        Construction happens before user customization, before any task root
        exists. This installs no guest exception or arbitrary-callable route.
        """
        from simulator.foreign_runtime import (
            CapturedTaskExport, ForeignTaskEngine, _RTC_FIELDS, _TaskRTCSeal,
        )
        from simulator.foreign_dispatch import TaskDispatchRoot
        from simulator.foreign_control import ForeignReturnControl
        from simulator.runtime import ExecutionContext
        from simulator.rtc import HostedRTCService
        from simulator.stacks import ReturnStack

        frame = _GETFRAME(1)
        if (self._task_installation is not None or type(engine) is not ForeignTaskEngine
                or dispatch_class is not TaskDispatchRoot
                or frame.f_code is not ForeignTaskEngine.__init__.__code__
                or frame.f_locals.get("self") is not engine):
            raise RuntimeError("task host-escape issuers require original engine construction")
        engine_namespace = vars(ForeignTaskEngine)["__dict__"]
        values = engine_namespace.__get__(engine, ForeignTaskEngine)
        runtime_descriptor = next(vars(base)["__dict__"]
                                  for base in type(self._runtime).__mro__
                                  if "__dict__" in vars(base))
        runtime_values = runtime_descriptor.__get__(self._runtime, type(self._runtime))
        context = dict.get(values, "_context")
        if (dict.get(values, "_runtime") is not self._runtime
                or type(context) is not ExecutionContext
                or context is not dict.get(runtime_values, "main_context")
                or dict.get(values, "_task_root") is not None):
            raise RuntimeError("task host-escape engine has no original context")
        self._task_installation = (
            engine, engine_namespace, values, dispatch_class,
            vars(dispatch_class)["__dict__"],
            (dispatch_class.tick.__code__, dispatch_class._owned_call.__code__),
            context, vars(ExecutionContext)["returns"], ForeignReturnControl,
            vars(ForeignReturnControl)["_issuer"], vars(ForeignReturnControl)["_stack"],
            runtime_descriptor, vars(ReturnStack)["__dict__"],
        )
        self._task_deadline_installation = (
            ForeignTaskEngine.read_task_uptime.__code__,
            next(value.fget for name, value in _RTC_FIELDS if name == "uptime_ms"),
            HostedRTCService, CapturedTaskExport, _TaskRTCSeal,
        )

    def issue_task(self, root, error):
        """Issue only an admitted task accounting/adapter host-call failure."""
        installation = self._task_installation
        if installation is None:
            return
        (engine, engine_descriptor, engine_values, dispatch_class, root_descriptor,
         codes, context, returns_descriptor, control_class,
         issuer_descriptor, stack_descriptor, runtime_descriptor, returns_namespace) = installation
        frame = _GETFRAME(1)
        if (type(root) is not dispatch_class or not any(frame.f_code is code for code in codes)
                or frame.f_locals.get("self") is not root):
            return
        values = root_descriptor.__get__(root, dispatch_class)
        unwinding = dict.get(values, "_unwinding_error")
        # Cleanup can encounter another host error while preserving a primary
        # escape. It must neither replace its record nor mark an ordinary ABORT.
        if unwinding is not None and unwinding is not error:
            return
        runtime_values = runtime_descriptor.__get__(self._runtime, type(self._runtime))
        control = dict.get(values, "control")
        returns = returns_descriptor.__get__(context, type(context))
        if (dict.get(runtime_values, "_foreign_tasks") is not engine
                or engine_descriptor.__get__(engine, type(engine)) is not engine_values
                or dict.get(engine_values, "_task_root") is not root
                or dict.get(engine_values, "_runtime") is not self._runtime
                or dict.get(engine_values, "_context") is not context
                or dict.get(values, "engine") is not engine
                or dict.get(values, "context") is not context
                or type(control) is not control_class
                or issuer_descriptor.__get__(control, control_class) is not dict.get(values, "issuer")
                or stack_descriptor.__get__(control, control_class) is not returns
                or dict.get(returns_namespace.__get__(returns, type(returns)), "_foreign_control") is not control
                or _outer_scope(self, frame) is None):
            return
        traceback = _TRACEBACK.__get__(error, BaseException)
        if traceback is None or traceback.tb_frame is not frame:
            return
        _clear(self)
        self._error = error
        self._cursor = traceback
        self._cursor_frame = frame

    def issue_task_deadline(self, root, error):
        """Preserve only a captured IdleUntil clock failure in its original guard.

        The canonical engine frame has already validated its RTC seal and
        retained the exact getter/owner before the host clock runs. Reading
        those locals avoids consulting host-mutated RTC routes during unwind.
        """
        installation = self._task_installation
        deadline = self._task_deadline_installation
        if installation is None or deadline is None:
            return
        (engine, engine_descriptor, engine_values, dispatch_class, root_descriptor,
         _codes, context, returns_descriptor, control_class,
         issuer_descriptor, stack_descriptor, runtime_descriptor, returns_namespace) = installation
        code, getter, rtc_class, capture_class, seal_class = deadline
        frame = _GETFRAME(1)
        local = frame.f_locals
        if (frame.f_code is not code or local.get("self") is not engine
                or local.get("root") is not root or type(root) is not dispatch_class
                or local.get("getter") is not getter
                or type(local.get("rtc_owner")) is not rtc_class
                or type(local.get("capture")) is not capture_class
                or type(local.get("rtc")) is not seal_class):
            return
        values = root_descriptor.__get__(root, dispatch_class)
        unwinding = dict.get(values, "_unwinding_error")
        if unwinding is not None and unwinding is not error:
            return
        runtime_values = runtime_descriptor.__get__(self._runtime, type(self._runtime))
        control = dict.get(values, "control")
        returns = returns_descriptor.__get__(context, type(context))
        if (dict.get(runtime_values, "_foreign_tasks") is not engine
                or engine_descriptor.__get__(engine, type(engine)) is not engine_values
                or dict.get(engine_values, "_task_root") is not root
                or dict.get(engine_values, "_runtime") is not self._runtime
                or dict.get(engine_values, "_context") is not context
                or dict.get(values, "engine") is not engine
                or dict.get(values, "context") is not context
                or type(control) is not control_class
                or issuer_descriptor.__get__(control, control_class) is not dict.get(values, "issuer")
                or stack_descriptor.__get__(control, control_class) is not returns
                or dict.get(returns_namespace.__get__(returns, type(returns)), "_foreign_control") is not control
                or _outer_scope(self, frame) is None):
            return
        traceback = _TRACEBACK.__get__(error, BaseException)
        if traceback is None or traceback.tb_frame is not frame:
            return
        _clear(self)
        self._error = error
        self._cursor = traceback
        self._cursor_frame = frame

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
