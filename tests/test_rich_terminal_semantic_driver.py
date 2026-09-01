"""Focused core/driver tests for revision-bound semantic control events."""

from __future__ import annotations

from types import MappingProxyType, SimpleNamespace

import pytest

from rich_terminal.apt1 import (
    FrameEncoder,
    IncrementalFrameDecoder,
    MessageType,
)
from rich_terminal.driver import (
    DriverLimits,
    DriverStatus,
    RichTerminalDriver,
)
from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_scene import (
    ControlDefinition,
    ControlKind,
    ControlState,
    ObjectBounds,
    OwnerScene,
    RegionDefinition,
    RetainedScene,
    RetainedSceneModel,
    SceneModelState,
    SceneUsage,
)
from rich_terminal.retained_resources import RetainedResourceStore
from rich_terminal.retained_wire import (
    ControlEvent,
    ControlEventKind,
    decode_control_event,
)
from rich_terminal.server import (
    RichTerminalCore,
    TerminalConfig,
    TerminalSessionError,
    TerminalState,
)
from rich_terminal.testing import FakeTerminalHost
from rich_terminal.transport import EgressWatermarks, HostPortLimits
from rich_terminal.update_authority import TerminalGeometry, TerminalUpdateAuthority


SESSION_ID = 0x0123456789ABCDEF
OWNER_ID = 7
OWNER_GENERATION = 3
MODEL_REVISION = 11
CONTROL_FRAME_BYTES = 80
BASE_FIXED_INPUT_FRAME_BYTES = 68


def _config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=512,
        max_transaction_bytes=4_096,
        terminal_receive_credit=8_192,
        max_cells=4,
        max_feed_bytes=8_192,
        max_cols=2,
        max_rows=2,
        cols=2,
        rows=2,
    )


def _policy(*, controls: bool) -> RetainedPolicy:
    return RetainedPolicy(
        features=(
            RetainedFeature.CORE | RetainedFeature.CONTROLS
            if controls
            else RetainedFeature.CORE
        ),
        max_owner_records=2,
        max_live_owners=1,
        max_regions=2,
        max_resources=0,
        max_objects=4 if controls else 0,
        max_series=0,
        max_operations_per_transaction=8,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=4_096,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=128 if controls else 0,
        client_to_terminal_max_payload=512,
        terminal_to_client_max_payload=512,
        base_max_transaction_bytes=4_096,
    )


def _host_limits(ordinary_ingress_bytes: int) -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(8_192, 1_024, 8, 1),
        retained_publication_bytes=8_192,
        ingress_bytes=4_096 + ordinary_ingress_bytes,
        ingress_events=9,
        ingress_control_bytes=4_096,
        ingress_control_events=8,
        geometry_events=1,
    )


def _control_scene(
    clock: TerminalUpdateAuthority,
    policy: RetainedPolicy,
    *,
    menu_open: bool,
) -> tuple[OwnerLedger, RetainedSceneModel]:
    geometry = TerminalGeometry(2, 2, 0)
    owner = OwnerIdentity(SESSION_ID, 0, OWNER_ID, OWNER_GENERATION)
    owners = OwnerLedger(
        session_id=SESSION_ID,
        presentation_epoch=0,
        policy=policy,
    )
    controls_enabled = bool(policy.features & RetainedFeature.CONTROLS)
    owners.open(
        owner,
        OwnerQuotas(
            regions=1,
            resources=0,
            objects=4 if controls_enabled else 0,
            series=0,
            resource_bytes=0,
            utf8_bytes=64 if controls_enabled else 0,
            sample_slots=0,
        ),
    )
    model = RetainedSceneModel(
        clock=clock,
        owners=owners,
        resources=RetainedResourceStore(owners),
        geometry=geometry,
    )
    controls: dict[int, ControlDefinition] = {}
    if controls_enabled:
        visible_enabled = ControlState.VISIBLE | ControlState.ENABLED
        controls = {
            1: ControlDefinition(
                owner,
                1,
                ControlKind.MENU_BAR,
                visible_enabled,
                20,
                1,
                0,
                0,
                ObjectBounds(0, 0, 2, 1),
                "",
                "",
            ),
            2: ControlDefinition(
                owner,
                2,
                ControlKind.MENU,
                visible_enabled
                | (ControlState.OPEN if menu_open else ControlState(0)),
                0,
                1,
                1,
                0,
                None,
                "File",
                "",
            ),
            3: ControlDefinition(
                owner,
                3,
                ControlKind.MENU_ITEM,
                visible_enabled,
                0,
                1,
                2,
                0,
                None,
                "Open",
                "Ctrl+O",
            ),
        }
    region = RegionDefinition(
        owner, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, True, True, 0
    )
    owner_scene = OwnerScene(
        owner=owner,
        regions=MappingProxyType({1: region}),
        objects=MappingProxyType({}),
        series=MappingProxyType({}),
        usage=SceneUsage(
            regions=1,
            objects=len(controls),
            utf8_bytes=sum(
                len(control.label.encode("utf-8"))
                + len(control.shortcut.encode("utf-8"))
                for control in controls.values()
            ),
        ),
        controls=MappingProxyType(controls),
    )
    model._state = SceneModelState(
        revision=clock.revision,
        geometry=geometry,
        active=RetainedScene(MappingProxyType({OWNER_ID: owner_scene})),
        hidden=None,
        hidden_kind=None,
        requirement=None,
        retained_visible=True,
        retained_initialized=True,
    )
    return owners, model


def _active_core(
    *,
    controls: bool = True,
    menu_open: bool = True,
) -> RichTerminalCore:
    policy = _policy(controls=controls)
    clock = TerminalUpdateAuthority(
        presentation_epoch=0,
        revision=MODEL_REVISION,
        transaction_high_water=MODEL_REVISION,
    )
    owners, retained = _control_scene(clock, policy, menu_open=menu_open)
    core = RichTerminalCore(
        _config(),
        attachment_epoch=1,
        retained_policy=policy,
        session_id_factory=lambda: SESSION_ID,
    )

    # Establish the exact settled ACTIVE boundary under test without replaying
    # negotiation/PRESENT ingress, which has separate byte-oracle coverage.
    core._state = TerminalState.ACTIVE
    core._session_id = SESSION_ID
    core._encoder = FrameEncoder(SESSION_ID, max_payload=512)
    core._model = SimpleNamespace(awaiting_snapshot=False, transaction_open=False)
    core._clock = clock
    core._session_retained_policy = policy
    core._owner_ledger = owners
    core._retained_model = retained
    core._retained_enabled = True
    core._server_data_grant = 8_192
    return core


def _driver(core: RichTerminalCore) -> RichTerminalDriver:
    host = FakeTerminalHost()
    limits = _host_limits(104)
    lease = host.attach(limits)
    return RichTerminalDriver(
        lease,
        core,
        limits,
        DriverLimits(4_096, 3),
    )


def test_driver_retains_one_exact_control_event_without_mutating_scene() -> None:
    core = _active_core()
    driver = _driver(core)
    before = core.retained_state

    assert (
        driver.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
            modifiers=0x07,
        )
        is DriverStatus.PROGRESS
    )
    assert driver.pending_outbound_events == 1
    assert driver.pending_outbound_bytes == CONTROL_FRAME_BYTES
    record = driver._pending[0].record
    assert len(record.payload) == CONTROL_FRAME_BYTES
    assert not record.control
    frames = IncrementalFrameDecoder(SESSION_ID, max_payload=512).feed(record.payload)
    assert len(frames) == 1
    assert frames[0].message_type == MessageType.CONTROL_EVENT
    assert decode_control_event(frames[0].payload) == ControlEvent(
        OWNER_ID,
        OWNER_GENERATION,
        3,
        ControlEventKind.ACTIVATE,
        0x07,
        MODEL_REVISION,
    )
    assert core.retained_state is before


def test_driver_preflights_the_full_control_event_before_encoding(monkeypatch) -> None:
    core = _active_core()
    driver = _driver(core)
    checked = []
    before_sent = core._server_data_sent
    before = core.retained_state

    def reject_retention(additional_bytes: int, additional_events: int) -> bool:
        checked.append((additional_bytes, additional_events))
        return False

    monkeypatch.setattr(driver, "_can_retain", reject_retention)
    assert (
        driver.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
        )
        is DriverStatus.BACKPRESSURED
    )
    assert checked == [(CONTROL_FRAME_BYTES, 1)]
    assert core._server_data_sent == before_sent
    assert core.retained_state is before


def test_attach_preserves_cell_only_floor_and_admits_control_configuration() -> None:
    class AttachSpy:
        called = False

        def attach_rich_terminal(self, limits):
            self.called = True
            raise AssertionError("capacity validation reached attachment")

    system = AttachSpy()
    with pytest.raises(ValueError, match="fixed-size input frame"):
        RichTerminalDriver.attach(
            system,
            _host_limits(BASE_FIXED_INPUT_FRAME_BYTES - 1),
            _config(),
            DriverLimits(4_096, 3),
        )
    assert not system.called

    with pytest.raises(AssertionError, match="reached attachment"):
        RichTerminalDriver.attach(
            system,
            _host_limits(BASE_FIXED_INPUT_FRAME_BYTES),
            _config(),
            DriverLimits(4_096, 3),
        )
    assert system.called

    system.called = False
    with pytest.raises(AssertionError, match="reached attachment"):
        RichTerminalDriver.attach(
            system,
            _host_limits(104),
            _config(),
            DriverLimits(4_096, 3),
            retained_policy=_policy(controls=True),
        )
    assert system.called


def test_driver_preserves_state_when_core_credit_backpressures_event() -> None:
    core = _active_core()
    core._server_data_grant = CONTROL_FRAME_BYTES - 1
    core._server_data_sent = 0
    driver = _driver(core)
    before = core.retained_state

    assert (
        driver.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
        )
        is DriverStatus.BACKPRESSURED
    )
    assert core._server_data_sent == 0
    assert driver.pending_outbound_events == 0
    assert driver.pending_outbound_bytes == 0
    assert core.retained_state is before


def test_core_rejects_feature_revision_owner_and_closed_ancestry() -> None:
    no_controls = _active_core(controls=False)
    with pytest.raises(TerminalSessionError, match="RET_CONTROLS"):
        no_controls.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
        )

    core = _active_core()
    before = core.retained_state
    with pytest.raises(TypeError, match="model_revision"):
        core.send_control_event(OWNER_ID, OWNER_GENERATION, 3)
    with pytest.raises(TerminalSessionError, match="current model revision"):
        core.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION - 1,
        )
    with pytest.raises(TerminalSessionError, match="not interactable"):
        core.send_control_event(
            OWNER_ID,
            OWNER_GENERATION + 1,
            3,
            model_revision=MODEL_REVISION,
        )

    closed = _active_core(menu_open=False)
    closed_before = closed.retained_state
    with pytest.raises(TerminalSessionError, match="not interactable"):
        closed.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
        )
    assert core.retained_state is before
    assert closed.retained_state is closed_before


def test_driver_maps_malformed_and_ineligible_controls_to_invalid() -> None:
    driver = _driver(_active_core(menu_open=False))
    assert (
        driver.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
        )
        is DriverStatus.INVALID
    )
    assert (
        driver.send_control_event(
            OWNER_ID,
            OWNER_GENERATION,
            3,
            model_revision=MODEL_REVISION,
            modifiers=0x40,
        )
        is DriverStatus.INVALID
    )
    assert driver.pending_outbound_events == 0
    assert driver.pending_outbound_bytes == 0


def test_driver_maps_closed_and_failed_lifetimes_before_control_validation() -> None:
    stale = _driver(_active_core())
    stale._closed = True
    assert (
        stale.send_control_event(0, 0, 0, model_revision=MODEL_REVISION)
        is DriverStatus.STALE
    )

    failed = _driver(_active_core())
    failed._failure_reason = "latched"
    assert (
        failed.send_control_event(0, 0, 0, model_revision=MODEL_REVISION)
        is DriverStatus.FAILED
    )
