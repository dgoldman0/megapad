"""Bounded V4 nested-policy metadata and proofs, without execution authority.

IDs name nodes in one supplied graph snapshot. They are not Words, XTs, issued
child edges or persistent owner identities. Captured graphs may assign fresh
IDs to shadowed Words provided every reference is rebased consistently.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TypeAlias

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI, MAX_BUFFER_RULES, MAX_CALLBACK_EXPORTS, MAX_CALLBACK_SITES,
    MAX_CALL_INSTRUCTIONS, MAX_CODE_BYTES, MAX_DISPATCH_CALLBACKS,
    MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS, MAX_DISPATCH_INSTRUCTIONS, MAX_ROUTINES,
    MAX_SIGNATURE_CELLS, BufferRuleV1, CallbackExportV2, MachineSegmentResultV2,
    RoutineDeclarationV1, RoutineImageV1, RoutineManifestV2,
    _callback_sites, _integer, _name, _routine_values, _version,
)
from shared.hybrid_closed import (
    CLOSED_STACK_CELLS, CORE_STACK_EFFECTS, MAX_CLOSED_OPERATIONS,
    MAX_CLOSED_POLICIES, MAX_CLOSED_WORDS, ClosedPolicyProofV3,
    PolicyBranchV3, PolicyBranchZeroV3, PolicyCallV3, PolicyCoreCallV3,
    PolicyLiteralV3, PolicyReturnV3,
)


HYBRID_NESTED_ABI_VERSION = 4
MAX_MACHINE_DEPTH = 8
CALLBACK_WORK_SATURATION = 4097
MAX_CHILD_EDGES_PER_ROUTINE = 4096
MAX_CHILD_EDGES = 65536


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyMachineCallV4:
    routine_id: int

    def __post_init__(self) -> None:
        _integer(self.routine_id, "routine ID", 0, MAX_ROUTINES - 1)


PolicyOperationV4: TypeAlias = (
    PolicyLiteralV3 | PolicyCoreCallV3 | PolicyCallV3 | PolicyBranchV3
    | PolicyBranchZeroV3 | PolicyReturnV3 | PolicyMachineCallV4
)
_OPERATION_TYPES = (
    PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3, PolicyBranchV3,
    PolicyBranchZeroV3, PolicyReturnV3, PolicyMachineCallV4,
)


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyBodyV4:
    policy_id: int
    name: str
    operations: tuple[PolicyOperationV4, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_NESTED_ABI_VERSION)
        _integer(self.policy_id, "policy ID", 0, MAX_CLOSED_POLICIES - 1)
        _name(self.name)
        if type(self.operations) is not tuple:
            raise TypeError("policy operations must be an exact immutable tuple")
        _integer(len(self.operations), "policy operation count", 1, MAX_CLOSED_OPERATIONS)
        for index, operation in enumerate(self.operations):
            if type(operation) not in _OPERATION_TYPES:
                raise TypeError("policy operation must have an exact admitted V4 value type")
            if type(operation) is not PolicyReturnV3:
                type(operation).__post_init__(operation)
            if type(operation) in (PolicyBranchV3, PolicyBranchZeroV3):
                if not index < operation.target < len(self.operations):
                    raise ValueError("policy branches must target a later in-body operation")


@dataclass(frozen=True, slots=True, kw_only=True)
class ClosedPolicyV4(PolicyBodyV4):
    input_cells: int
    output_cells: int

    def __post_init__(self) -> None:
        PolicyBodyV4.__post_init__(self)
        _integer(self.input_cells, "policy input cells", 0, CLOSED_STACK_CELLS)
        _integer(self.output_cells, "policy output cells", 0, CLOSED_STACK_CELLS)


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackExportV4:
    export_id: int
    name: str
    input_cells: int
    output_cells: int
    max_semantic_steps: int = 1
    effect: str = "integer_leaf"
    policy_id: int | None = None
    abi: str = HYBRID_ABI
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_NESTED_ABI_VERSION)
        if type(self.effect) is not str:
            raise TypeError("callback effect must be an exact string")
        if self.effect == "integer_leaf":
            if self.policy_id is not None:
                raise ValueError("an integer leaf must not declare a policy ID")
            CallbackExportV2(export_id=self.export_id, name=self.name,
                             input_cells=self.input_cells, output_cells=self.output_cells,
                             max_semantic_steps=self.max_semantic_steps)
        elif self.effect in ("closed_integer_colon", "closed_integer_nested"):
            _integer(self.export_id, "export ID", 0, MAX_CALLBACK_EXPORTS - 1)
            _integer(self.policy_id, "policy ID", 0, MAX_CLOSED_POLICIES - 1)
            _name(self.name)
            _integer(self.input_cells, "callback input cells", 0, MAX_SIGNATURE_CELLS)
            _integer(self.output_cells, "callback output cells", 0, MAX_SIGNATURE_CELLS)
            _integer(self.max_semantic_steps, "callback semantic steps", 1, 4096)
        else:
            raise ValueError("unsupported V4 callback effect")

    @property
    def can_suspend(self) -> bool:
        return False


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackSiteV4:
    call_offset: int
    stub_offset: int
    export: CallbackExportV4
    abi: str = HYBRID_ABI
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_NESTED_ABI_VERSION)
        _integer(self.call_offset, "callback call offset", 0, MAX_CODE_BYTES - 2)
        _integer(self.stub_offset, "callback stub offset", 0, MAX_CODE_BYTES - 1)
        if self.call_offset <= self.stub_offset < self.call_offset + 2:
            raise ValueError("callback call and stub byte spans must be disjoint")
        _exact(self.export, CallbackExportV4, "callback export")


def _exact(value: object, kind: type, label: str) -> None:
    if type(value) is not kind:
        raise TypeError(f"{label} must have exact type {kind.__name__}")
    kind.__post_init__(value)


def _tuple(value: object, maximum: int, label: str) -> None:
    if type(value) is not tuple:
        raise TypeError(f"{label} must be an exact immutable tuple")
    if len(value) > maximum:
        raise ValueError(f"{label} exceeds its {maximum}-entry limit")


def _metadata_sites(sites: tuple[CallbackSiteV4, ...]) -> None:
    _tuple(sites, MAX_CALLBACK_SITES, "callback sites")
    occupied: set[int] = set()
    exports = {}
    for site in sites:
        _exact(site, CallbackSiteV4, "callback site")
        offsets = (site.call_offset, site.call_offset + 1, site.stub_offset)
        if any(offset in occupied for offset in offsets):
            raise ValueError("callback site byte spans must not overlap or repeat")
        occupied.update(offsets)
        if exports.setdefault(site.export.export_id, site.export) != site.export:
            raise ValueError("callback export ID has conflicting descriptors")


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineGraphNodeV4:
    """Only the machine metadata needed before any image is opened."""

    routine_id: int
    name: str
    input_cells: int
    output_cells: int
    max_instructions: int
    max_callback_requests: int
    callbacks: tuple[CallbackSiteV4, ...]

    def __post_init__(self) -> None:
        _integer(self.routine_id, "routine ID", 0, MAX_ROUTINES - 1)
        _name(self.name)
        _integer(self.input_cells, "routine input cells", 0, MAX_SIGNATURE_CELLS)
        _integer(self.output_cells, "routine output cells", 0, MAX_SIGNATURE_CELLS)
        _integer(self.max_instructions, "per-call instructions", 1, MAX_CALL_INSTRUCTIONS)
        _integer(self.max_callback_requests, "per-call callback requests", 0,
                 MAX_DISPATCH_CALLBACKS)
        _metadata_sites(self.callbacks)


@dataclass(frozen=True, slots=True, kw_only=True)
class ChildCallDeclarationV4:
    """Static IR location, not the engine's captured Call or an issued edge."""

    policy_id: int
    operation_index: int
    routine_id: int

    def __post_init__(self) -> None:
        _integer(self.policy_id, "policy ID", 0, MAX_CLOSED_POLICIES - 1)
        _integer(self.operation_index, "operation index", 0, MAX_CLOSED_OPERATIONS - 1)
        _integer(self.routine_id, "routine ID", 0, MAX_ROUTINES - 1)


@dataclass(frozen=True, slots=True, kw_only=True)
class NestedNodeV4:
    kind: str
    node_id: int

    def __post_init__(self) -> None:
        if type(self.kind) is not str or self.kind not in ("policy", "routine"):
            raise ValueError("graph node kind must be policy or routine")
        _integer(self.node_id, "graph node ID", 0, 63)


def _base_policy_proof(value: object) -> ClosedPolicyProofV3:
    return ClosedPolicyProofV3(**{
        name: getattr(value, name) for name in ClosedPolicyProofV3.__dataclass_fields__
    })


@dataclass(frozen=True, slots=True, kw_only=True)
class ClosedPolicyProofV4(ClosedPolicyProofV3):
    routine_ids: tuple[int, ...]
    max_machine_depth: int
    child_calls: tuple[ChildCallDeclarationV4, ...]

    def __post_init__(self) -> None:
        _base_policy_proof(self)
        _tuple(self.routine_ids, MAX_ROUTINES, "captured routine IDs")
        for routine_id in self.routine_ids:
            _integer(routine_id, "captured routine ID", 0, MAX_ROUTINES - 1)
        if self.routine_ids != tuple(sorted(set(self.routine_ids))):
            raise ValueError("captured routine IDs must be sorted and unique")
        if len(self.policy_ids) + len(self.core_names) + len(self.routine_ids) > MAX_CLOSED_WORDS:
            raise ValueError("a policy closure may capture at most 64 Words")
        _integer(self.max_machine_depth, "policy machine depth", 0, MAX_MACHINE_DEPTH)
        _tuple(self.child_calls, MAX_CLOSED_OPERATIONS, "captured child calls")
        keys = []
        for call in self.child_calls:
            _exact(call, ChildCallDeclarationV4, "child call")
            if call.policy_id not in self.policy_ids or call.routine_id not in self.routine_ids:
                raise ValueError("child call is outside its captured policy closure")
            keys.append((call.policy_id, call.operation_index))
        if keys != sorted(set(keys)):
            raise ValueError("child calls must be sorted and unique by static location")
        if bool(self.routine_ids) != bool(self.max_machine_depth):
            raise ValueError("captured routine IDs and machine depth are inconsistent")

    def validate_signature(self, input_cells: int, output_cells: int,
                           max_semantic_steps: int = 4096) -> None:
        _exact(self, ClosedPolicyProofV4, "policy proof")
        _base_policy_proof(self).validate_signature(input_cells, output_cells, max_semantic_steps)

    def max_data_depth(self, input_cells: int) -> int:
        _integer(input_cells, "policy input cells", 0, CLOSED_STACK_CELLS)
        self.validate_signature(input_cells, input_cells + self.net_data_cells)
        return input_cells + self.peak_data_growth


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineGraphProofV4:
    routine_id: int
    max_machine_depth: int
    callback_work_bound: int
    child_edge_count: int

    def __post_init__(self) -> None:
        _integer(self.routine_id, "routine ID", 0, MAX_ROUTINES - 1)
        _integer(self.max_machine_depth, "routine machine depth", 1, MAX_MACHINE_DEPTH)
        # 4097 means exceeds the per-callback proof allowance, not an exact
        # upper bound. A root may have this summary; a consuming policy may not.
        _integer(self.callback_work_bound, "saturated callback work", 0, CALLBACK_WORK_SATURATION)
        _integer(self.child_edge_count, "routine child edges", 0, MAX_CHILD_EDGES_PER_ROUTINE)


@dataclass(frozen=True, slots=True, kw_only=True)
class NestedGraphProofV4:
    policy_proofs: tuple[ClosedPolicyProofV4, ...]
    routine_proofs: tuple[RoutineGraphProofV4, ...]
    publication_order: tuple[NestedNodeV4, ...]
    child_calls: tuple[ChildCallDeclarationV4, ...]

    def __post_init__(self) -> None:
        nodes = set()
        for values, kind, label, maximum in (
            (self.policy_proofs, ClosedPolicyProofV4, "policy", MAX_CLOSED_POLICIES),
            (self.routine_proofs, RoutineGraphProofV4, "routine", MAX_ROUTINES),
        ):
            _tuple(values, maximum, f"{label} proofs")
            for value in values:
                _exact(value, kind, f"{label} proof")
                node = (label, getattr(value, f"{label}_id"))
                if node in nodes:
                    raise ValueError("duplicate graph proof node")
                nodes.add(node)
        _tuple(self.publication_order, MAX_CLOSED_POLICIES + MAX_ROUTINES, "publication order")
        order = []
        for node in self.publication_order:
            _exact(node, NestedNodeV4, "publication node")
            order.append((node.kind, node.node_id))
        if len(order) != len(nodes) or set(order) != nodes:
            raise ValueError("publication order must contain every proof node exactly once")
        _tuple(self.child_calls, MAX_CLOSED_OPERATIONS, "graph child calls")
        keys = []
        for call in self.child_calls:
            _exact(call, ChildCallDeclarationV4, "child call")
            if ("policy", call.policy_id) not in nodes or ("routine", call.routine_id) not in nodes:
                raise ValueError("child call names an undeclared graph node")
            keys.append((call.policy_id, call.operation_index))
        if keys != sorted(set(keys)):
            raise ValueError("graph child calls must be sorted and unique")
        if self.child_edge_count > MAX_CHILD_EDGES:
            raise ValueError("combined graph exceeds 65536 child edges")

    @property
    def child_edge_count(self) -> int:
        return sum(proof.child_edge_count for proof in self.routine_proofs)


def _prove_body(body: PolicyBodyV4, policies: dict, routines: dict,
                machine_nodes: dict) -> ClosedPolicyProofV4:
    policy_ids, core_names, routine_ids = {body.policy_id}, set(), set()
    child_calls: dict[tuple[int, int], ChildCallDeclarationV4] = {}
    machine_depth = 0
    for index, operation in enumerate(body.operations):
        if type(operation) is PolicyCallV3:
            dependency = policies[operation.policy_id]
            policy_ids.update(dependency.policy_ids)
            core_names.update(dependency.core_names)
            routine_ids.update(dependency.routine_ids)
            machine_depth = max(machine_depth, dependency.max_machine_depth)
            child_calls.update(((call.policy_id, call.operation_index), call)
                               for call in dependency.child_calls)
        elif type(operation) is PolicyCoreCallV3:
            core_names.add(operation.name)
        elif type(operation) is PolicyMachineCallV4:
            routine_ids.add(operation.routine_id)
            machine_depth = max(machine_depth, routines[operation.routine_id].max_machine_depth)
            child_calls[body.policy_id, index] = ChildCallDeclarationV4(
                policy_id=body.policy_id, operation_index=index, routine_id=operation.routine_id)
    if len(policy_ids) + len(core_names) + len(routine_ids) > MAX_CLOSED_WORDS:
        raise ValueError("a policy closure may capture at most 64 Words")

    incoming = {0: (0, 0, 0)}
    required = growth = 0
    return_cells = 1
    exits = []
    for index, operation in enumerate(body.operations):
        if index not in incoming:
            continue
        depth, minimum, maximum = incoming[index]
        demand = delta = peak = 0
        low_cost = high_cost = 1
        kind = type(operation)
        if kind is PolicyLiteralV3:
            delta = peak = 1
        elif kind is PolicyCoreCallV3:
            demand, delta = CORE_STACK_EFFECTS[operation.name]
            peak = max(0, delta)
            low_cost = high_cost = 2
        elif kind is PolicyCallV3:
            dependency = policies[operation.policy_id]
            demand, delta, peak = (dependency.required_input_cells,
                                  dependency.net_data_cells, dependency.peak_data_growth)
            low_cost = 1 + dependency.min_semantic_steps
            high_cost = 1 + dependency.max_semantic_steps
            return_cells = max(return_cells, 1 + dependency.max_return_cells)
        elif kind is PolicyMachineCallV4:
            machine = machine_nodes[operation.routine_id]
            demand = machine.input_cells
            delta = machine.output_cells - demand
            peak = max(0, delta)
            low_cost = 2
            high_cost = 2 + routines[operation.routine_id].callback_work_bound
        elif kind is PolicyBranchZeroV3:
            demand, delta = 1, -1
        required = max(required, demand - depth)
        growth = max(growth, depth + peak)
        depth += delta
        minimum += low_cost
        maximum = min(CALLBACK_WORK_SATURATION, maximum + high_cost)
        if maximum >= CALLBACK_WORK_SATURATION:
            raise ValueError("policy work bound exceeds 4096 semantic steps")
        if required + growth > CLOSED_STACK_CELLS or return_cells > CLOSED_STACK_CELLS:
            raise ValueError("policy exceeds its eight-cell private stack bounds")
        if kind is PolicyReturnV3:
            exits.append((depth, minimum, maximum))
            continue
        targets = ((operation.target,) if kind is PolicyBranchV3 else
                   (index + 1, operation.target) if kind is PolicyBranchZeroV3 else
                   (index + 1,))
        for target in targets:
            if target >= len(body.operations):
                raise ValueError("every reachable policy path must end in Return")
            previous = incoming.get(target)
            if previous is None:
                incoming[target] = (depth, minimum, maximum)
            else:
                if previous[0] != depth:
                    raise ValueError("policy control-flow joins must have identical data depths")
                incoming[target] = (depth, min(minimum, previous[1]), max(maximum, previous[2]))
    if not exits:
        raise ValueError("policy has no reachable Return")
    net = exits[0][0]
    if any(depth != net for depth, _, _ in exits):
        raise ValueError("all policy return paths must have identical data depths")
    proof = ClosedPolicyProofV4(
        policy_id=body.policy_id, required_input_cells=required, net_data_cells=net,
        peak_data_growth=growth, max_return_cells=return_cells,
        min_semantic_steps=min(item[1] for item in exits),
        max_semantic_steps=max(item[2] for item in exits),
        policy_ids=tuple(sorted(policy_ids)), core_names=tuple(sorted(core_names)),
        routine_ids=tuple(sorted(routine_ids)), max_machine_depth=machine_depth,
        child_calls=tuple(child_calls[key] for key in sorted(child_calls)),
    )
    if type(body) is ClosedPolicyV4:
        proof.validate_signature(body.input_cells, body.output_cells)
    return proof


def prove_nested_graph(*, policies: tuple[PolicyBodyV4 | ClosedPolicyV4, ...],
                       routines: tuple[RoutineGraphNodeV4, ...],
                       exports: tuple[CallbackExportV4, ...],
                       dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS,
                       ) -> NestedGraphProofV4:
    """Prove every declared node, including unused sites and unreachable calls.

    Graph depth and captured dependencies include all declared operations;
    executable forward-path work and stack joins retain V3 semantics. Machine
    callbacks contribute work but use distinct semantic return/data contexts.
    No graph identifier, order or copied proof grants child-call authority.
    """

    _integer(dispatch_callback_limit, "dispatch callback requests", 1, MAX_DISPATCH_CALLBACKS)
    _tuple(policies, MAX_CLOSED_POLICIES, "policies")
    _tuple(routines, MAX_ROUTINES, "routines")
    _tuple(exports, MAX_CALLBACK_EXPORTS, "exports")
    policy_nodes, machine_nodes, export_nodes = {}, {}, {}
    names, total = set(), 0
    for policy in policies:
        if type(policy) not in (PolicyBodyV4, ClosedPolicyV4):
            raise TypeError("policies must have exact V4 body or declaration types")
        type(policy).__post_init__(policy)
        if policy.policy_id in policy_nodes:
            raise ValueError("duplicate policy ID")
        if type(policy) is ClosedPolicyV4:
            if policy.name.upper() in names:
                raise ValueError("duplicate policy name")
            names.add(policy.name.upper())
        policy_nodes[policy.policy_id] = policy
        total += len(policy.operations)
        if total > MAX_CLOSED_OPERATIONS:
            raise ValueError("policy table may contain at most 4096 operations")
    for routine in routines:
        _exact(routine, RoutineGraphNodeV4, "routine graph node")
        if routine.routine_id in machine_nodes:
            raise ValueError("duplicate routine ID")
        machine_nodes[routine.routine_id] = routine
    for export in exports:
        _exact(export, CallbackExportV4, "callback export")
        if export.export_id in export_nodes:
            raise ValueError("duplicate callback export ID")
        export_nodes[export.export_id] = export
        if export.policy_id is not None:
            policy = policy_nodes.get(export.policy_id)
            if policy is None:
                raise ValueError("callback export names an undeclared policy ID")
            if export.name != policy.name:
                raise ValueError("callback export name does not match its policy")
            if type(policy) is ClosedPolicyV4 and (
                    export.input_cells, export.output_cells) != (policy.input_cells, policy.output_cells):
                raise ValueError("callback export arity does not match its policy")
    dependencies = {}
    child_calls = []
    for policy in policies:
        deps = []
        for index, operation in enumerate(policy.operations):
            if type(operation) is PolicyCallV3:
                if operation.policy_id not in policy_nodes:
                    raise ValueError("policy call names an undeclared policy ID")
                deps.append(("policy", operation.policy_id))
            elif type(operation) is PolicyMachineCallV4:
                if operation.routine_id not in machine_nodes:
                    raise ValueError("machine call names an undeclared routine ID")
                deps.append(("routine", operation.routine_id))
                child_calls.append(ChildCallDeclarationV4(
                    policy_id=policy.policy_id, operation_index=index,
                    routine_id=operation.routine_id))
        dependencies["policy", policy.policy_id] = tuple(dict.fromkeys(deps))
    for routine in routines:
        deps = []
        for site in routine.callbacks:
            export = export_nodes.get(site.export.export_id)
            if export is None or export != site.export:
                raise ValueError("callback site has an undeclared or conflicting export")
            if export.policy_id is not None:
                deps.append(("policy", export.policy_id))
        dependencies["routine", routine.routine_id] = tuple(dict.fromkeys(deps))

    visiting, done, order = set(), set(), []
    policy_proofs, routine_proofs = {}, {}

    def visit(node: tuple[str, int]) -> None:
        if node in visiting:
            raise ValueError("combined policy/machine dependency graph must be acyclic")
        if node in done:
            return
        visiting.add(node)
        for dependency in dependencies[node]:
            visit(dependency)
        kind, identifier = node
        if kind == "policy":
            policy_proofs[identifier] = _prove_body(
                policy_nodes[identifier], policy_proofs, routine_proofs, machine_nodes)
        else:
            routine = machine_nodes[identifier]
            depth, work, child_edges = 1, 0, 0
            for site in routine.callbacks:
                export = site.export
                if export.policy_id is None:
                    work = max(work, 1)
                else:
                    proof = policy_proofs[export.policy_id]
                    depth = max(depth, 1 + proof.max_machine_depth)
                    work = max(work, proof.max_semantic_steps)
                    # One occurrence per captured policy Word/IR location,
                    # counted separately for each declared callback site.
                    child_edges += len(proof.child_calls)
            if child_edges > MAX_CHILD_EDGES_PER_ROUTINE:
                raise ValueError("routine exceeds 4096 child edges")
            if depth > MAX_MACHINE_DEPTH:
                raise ValueError("combined graph exceeds eight active machine frames")
            work = min(CALLBACK_WORK_SATURATION, work * min(
                routine.max_callback_requests, routine.max_instructions, dispatch_callback_limit))
            routine_proofs[identifier] = RoutineGraphProofV4(
                routine_id=identifier, max_machine_depth=depth, callback_work_bound=work,
                child_edge_count=child_edges)
        visiting.remove(node)
        done.add(node)
        order.append(NestedNodeV4(kind=kind, node_id=identifier))

    for node in dependencies:
        visit(node)
    for export in exports:
        if export.policy_id is None:
            continue
        proof = policy_proofs[export.policy_id]
        proof.validate_signature(export.input_cells, export.output_cells, export.max_semantic_steps)
        if export.effect == "closed_integer_colon" and proof.routine_ids:
            raise ValueError("closed_integer_colon cannot capture a machine call")
    return NestedGraphProofV4(
        policy_proofs=tuple(policy_proofs.values()), routine_proofs=tuple(routine_proofs.values()),
        publication_order=tuple(order), child_calls=tuple(sorted(
            child_calls, key=lambda call: (call.policy_id, call.operation_index))),
    )


def _buffers(value: object) -> None:
    _tuple(value.buffers, MAX_BUFFER_RULES, "buffer rules")
    for rule in value.buffers:
        _exact(rule, BufferRuleV1, "buffer rule")


def _routine_graph(value: object) -> RoutineGraphNodeV4:
    return RoutineGraphNodeV4(**{name: getattr(value, name)
                                for name in RoutineGraphNodeV4.__dataclass_fields__})


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineImageV4(RoutineImageV1):
    routine_id: int
    max_callback_requests: int
    callbacks: tuple[CallbackSiteV4, ...]
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _buffers(self)
        _routine_values(self, version=HYBRID_NESTED_ABI_VERSION)
        _routine_graph(self)
        _callback_sites(self, site_type=CallbackSiteV4)

    def graph_node(self) -> RoutineGraphNodeV4:
        _exact(self, RoutineImageV4, "routine image")
        return _routine_graph(self)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineDeclarationV4(RoutineDeclarationV1):
    routine_id: int
    max_callback_requests: int
    callbacks: tuple[CallbackSiteV4, ...]
    dispatch_callback_limit: int = MAX_DISPATCH_CALLBACKS
    dispatch_callback_semantic_limit: int = MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _buffers(self)
        self._validate_declaration(version=HYBRID_NESTED_ABI_VERSION)
        _routine_graph(self)
        _callback_sites(self, site_type=CallbackSiteV4)
        _integer(self.dispatch_callback_limit, "dispatch callback requests", 1, MAX_DISPATCH_CALLBACKS)
        _integer(self.dispatch_callback_semantic_limit, "dispatch callback semantic steps", 1,
                 MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS)

    def graph_node(self) -> RoutineGraphNodeV4:
        _exact(self, RoutineDeclarationV4, "routine declaration")
        return _routine_graph(self)


@dataclass(frozen=True, slots=True, kw_only=True)
class RoutineManifestV4(RoutineManifestV2):
    policies: tuple[ClosedPolicyV4, ...]
    exports: tuple[CallbackExportV4, ...]
    routines: tuple[RoutineImageV4, ...]
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_manifest(version=HYBRID_NESTED_ABI_VERSION,
                                export_type=CallbackExportV4, routine_type=RoutineImageV4)
        _tuple(self.policies, MAX_CLOSED_POLICIES, "manifest policies")
        for policy in self.policies:
            _exact(policy, ClosedPolicyV4, "manifest policy")
        routine_names = {routine.name.upper() for routine in self.routines}
        if any(policy.name.upper() in routine_names for policy in self.policies):
            raise ValueError("policy and routine names must not collide")
        self.graph_proof()

    def graph_proof(self) -> NestedGraphProofV4:
        _tuple(self.routines, MAX_ROUTINES, "manifest routines")
        return prove_nested_graph(policies=self.policies,
                                  routines=tuple(RoutineImageV4.graph_node(routine)
                                                 for routine in self.routines),
                                  exports=self.exports,
                                  dispatch_callback_limit=self.dispatch_callback_limit)


@dataclass(frozen=True, slots=True, kw_only=True)
class CallbackRequestV4:
    invocation_id: int
    sequence: int
    site: CallbackSiteV4
    arguments: tuple[int, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        _version(self.abi, self.version, expected=HYBRID_NESTED_ABI_VERSION)
        _integer(self.invocation_id, "invocation ID", 1, MASK64)
        _integer(self.sequence, "callback request sequence", 1, MAX_DISPATCH_CALLBACKS)
        _exact(self.site, CallbackSiteV4, "callback site")
        _tuple(self.arguments, MAX_SIGNATURE_CELLS, "callback arguments")
        if len(self.arguments) != self.site.export.input_cells:
            raise ValueError("callback argument count does not match its export")
        for argument in self.arguments:
            _integer(argument, "callback argument", 0, MASK64)


@dataclass(frozen=True, slots=True, kw_only=True)
class MachineSegmentResultV4(MachineSegmentResultV2):
    segment_id: int
    root_invocation_id: int
    parent_invocation_id: int
    depth: int
    invocation_started: bool
    chain_instructions: int
    chain_cycles: int
    callback: CallbackRequestV4 | None = None
    version: int = HYBRID_NESTED_ABI_VERSION

    def __post_init__(self) -> None:
        self._validate_segment(version=HYBRID_NESTED_ABI_VERSION, request_type=CallbackRequestV4)
        _integer(self.segment_id, "segment ID", 1, MASK64)
        _integer(self.root_invocation_id, "root invocation ID", 1, MASK64)
        _integer(self.parent_invocation_id, "parent invocation ID", 0, MASK64)
        _integer(self.depth, "machine depth", 1, MAX_MACHINE_DEPTH)
        if self.depth == 1:
            if self.parent_invocation_id != 0 or self.root_invocation_id != self.invocation_id:
                raise ValueError("root invocation identity and depth are inconsistent")
        elif not self.root_invocation_id <= self.parent_invocation_id < self.invocation_id:
            raise ValueError("child invocation identities are inconsistent")
        elif (self.depth == 2 and self.parent_invocation_id != self.root_invocation_id
              or self.depth > 2 and self.parent_invocation_id < self.root_invocation_id + self.depth - 2):
            raise ValueError("parent invocation identity cannot have the declared depth")
        if type(self.invocation_started) is not bool:
            raise TypeError("invocation_started must be an exact boolean")
        _integer(self.chain_instructions, "chain instructions", 0, MAX_DISPATCH_INSTRUCTIONS)
        _integer(self.chain_cycles, "chain cycles", 0, MASK64)
        if self.invocation_instructions > self.chain_instructions or self.invocation_cycles > self.chain_cycles:
            raise ValueError("invocation counters cannot exceed chain totals")
        instructions = self.chain_instructions - self.invocation_instructions
        cycles = self.chain_cycles - self.invocation_cycles
        if cycles < instructions or (instructions == 0 and cycles != 0):
            raise ValueError("chain instruction and cycle counters are inconsistent")
        if self.invocation_started and (self.instructions != self.invocation_instructions
                                       or self.cycles != self.invocation_cycles):
            raise ValueError("a started invocation cannot have prior own work")


__all__ = [
    "HYBRID_NESTED_ABI_VERSION", "MAX_MACHINE_DEPTH", "CALLBACK_WORK_SATURATION",
    "MAX_CHILD_EDGES_PER_ROUTINE", "MAX_CHILD_EDGES",
    "PolicyMachineCallV4", "PolicyOperationV4", "PolicyBodyV4", "ClosedPolicyV4",
    "CallbackExportV4", "CallbackSiteV4", "RoutineGraphNodeV4", "ChildCallDeclarationV4",
    "NestedNodeV4", "ClosedPolicyProofV4", "RoutineGraphProofV4", "NestedGraphProofV4",
    "prove_nested_graph", "RoutineImageV4", "RoutineDeclarationV4", "RoutineManifestV4",
    "CallbackRequestV4", "MachineSegmentResultV4",
]
