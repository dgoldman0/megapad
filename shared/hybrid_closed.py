"""Bounded value-only admission for closed integer callback policies.

This module knows neither Words nor execution engines. A proof summarizes
immutable input values; an engine must independently capture and revalidate
the exact live definitions, primitive identities and dispatch authority.
"""

from __future__ import annotations

from dataclasses import dataclass
from types import MappingProxyType
from typing import TypeAlias

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI, HYBRID_CLOSED_ABI_VERSION, MAX_CALLBACK_SEMANTIC_STEPS,
    _integer, _name, _version,
)


MAX_CLOSED_POLICIES = 64
MAX_CLOSED_OPERATIONS = 4096
MAX_CLOSED_WORDS = 64
CLOSED_STACK_CELLS = 8
CORE_STACK_EFFECTS = MappingProxyType({
    "MIN": (2, -1), "MAX": (2, -1), "AND": (2, -1),
    "OR": (2, -1), "XOR": (2, -1), "ABS": (1, 0),
    "DUP": (1, 1), "DROP": (1, -1), "SWAP": (2, 0),
    "OVER": (2, 1), "ROT": (3, 0),
})


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyLiteralV3:
    value: int

    def __post_init__(self) -> None:
        _integer(self.value, "policy literal", 0, MASK64)


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyCoreCallV3:
    name: str

    def __post_init__(self) -> None:
        _name(self.name)
        if self.name not in CORE_STACK_EFFECTS:
            raise ValueError("policy core call must name an admitted canonical primitive")


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyCallV3:
    policy_id: int

    def __post_init__(self) -> None:
        _integer(self.policy_id, "called policy ID", 0, MAX_CLOSED_POLICIES - 1)


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyBranchV3:
    target: int

    def __post_init__(self) -> None:
        _integer(self.target, "policy branch target", 0, MAX_CLOSED_OPERATIONS - 1)


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyBranchZeroV3:
    target: int

    def __post_init__(self) -> None:
        _integer(self.target, "policy branch target", 0, MAX_CLOSED_OPERATIONS - 1)


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyReturnV3:
    pass


PolicyOperationV3: TypeAlias = (
    PolicyLiteralV3 | PolicyCoreCallV3 | PolicyCallV3 |
    PolicyBranchV3 | PolicyBranchZeroV3 | PolicyReturnV3
)
_OPERATION_TYPES = (
    PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3,
    PolicyBranchV3, PolicyBranchZeroV3, PolicyReturnV3,
)


def _body_values(value: PolicyBodyV3) -> None:
    _version(value.abi, value.version, expected=HYBRID_CLOSED_ABI_VERSION)
    _integer(value.policy_id, "policy ID", 0, MAX_CLOSED_POLICIES - 1)
    _name(value.name)
    if type(value.operations) is not tuple:
        raise TypeError("policy operations must be an exact immutable tuple")
    _integer(len(value.operations), "policy operation count", 1, MAX_CLOSED_OPERATIONS)
    for index, operation in enumerate(value.operations):
        kind = type(operation)
        if kind not in _OPERATION_TYPES:
            raise TypeError("policy operations must have exact admitted value types")
        if kind is not PolicyReturnV3:
            # Revalidate forged frozen values without dispatching an override.
            kind.__post_init__(operation)
        if kind in (PolicyBranchV3, PolicyBranchZeroV3):
            if not index < operation.target < len(value.operations):
                raise ValueError("policy branches must target a later operation in the same body")


@dataclass(frozen=True, slots=True, kw_only=True)
class PolicyBodyV3:
    """Captured IR identified by policy ID, with a diagnostic source name."""

    policy_id: int
    name: str
    operations: tuple[PolicyOperationV3, ...]
    abi: str = HYBRID_ABI
    version: int = HYBRID_CLOSED_ABI_VERSION

    def __post_init__(self) -> None:
        _body_values(self)


@dataclass(frozen=True, slots=True, kw_only=True)
class ClosedPolicyV3(PolicyBodyV3):
    """A declarative manifest policy with an explicit public signature."""

    input_cells: int
    output_cells: int

    def __post_init__(self) -> None:
        _body_values(self)
        _integer(self.input_cells, "policy input cells", 0, CLOSED_STACK_CELLS)
        _integer(self.output_cells, "policy output cells", 0, CLOSED_STACK_CELLS)


@dataclass(frozen=True, slots=True, kw_only=True)
class ClosedPolicyProofV3:
    """Conservative control-flow bounds, not executable admission authority.

    Data growth is relative to entry depth; a captured helper needs no guessed
    public signature. Return depth includes the entry continuation itself.
    Min/max work treat both conditional edges as feasible and need not be tight.
    """

    policy_id: int
    required_input_cells: int
    net_data_cells: int
    peak_data_growth: int
    max_return_cells: int
    min_semantic_steps: int
    max_semantic_steps: int
    policy_ids: tuple[int, ...]
    core_names: tuple[str, ...]

    def __post_init__(self) -> None:
        _integer(self.policy_id, "proved policy ID", 0, MAX_CLOSED_POLICIES - 1)
        _integer(self.required_input_cells, "required input cells", 0, CLOSED_STACK_CELLS)
        _integer(self.net_data_cells, "net data cells", -CLOSED_STACK_CELLS, CLOSED_STACK_CELLS)
        _integer(self.peak_data_growth, "peak data growth", 0, CLOSED_STACK_CELLS)
        _integer(self.max_return_cells, "maximum return cells", 1, CLOSED_STACK_CELLS)
        _integer(self.min_semantic_steps, "minimum semantic steps", 1, MAX_CALLBACK_SEMANTIC_STEPS)
        _integer(self.max_semantic_steps, "maximum semantic steps", self.min_semantic_steps,
                 MAX_CALLBACK_SEMANTIC_STEPS)
        if self.required_input_cells + self.peak_data_growth > CLOSED_STACK_CELLS:
            raise ValueError("policy cannot fit the bounded private data stack")
        if self.required_input_cells + self.net_data_cells < 0:
            raise ValueError("policy output depth cannot be negative")
        if type(self.policy_ids) is not tuple or type(self.core_names) is not tuple:
            raise TypeError("proof closure identities must be exact immutable tuples")
        for policy_id in self.policy_ids:
            _integer(policy_id, "closure policy ID", 0, MAX_CLOSED_POLICIES - 1)
        for name in self.core_names:
            PolicyCoreCallV3(name=name)
        if self.policy_ids != tuple(sorted(set(self.policy_ids))) or self.policy_id not in self.policy_ids:
            raise ValueError("proof policy IDs must be sorted, unique and contain the entry")
        if self.core_names != tuple(sorted(set(self.core_names))):
            raise ValueError("proof core names must be sorted and unique")
        if len(self.policy_ids) + len(self.core_names) > MAX_CLOSED_WORDS:
            raise ValueError("a policy closure may capture at most 64 Words")

    def validate_signature(
        self, input_cells: int, output_cells: int,
        max_semantic_steps: int = MAX_CALLBACK_SEMANTIC_STEPS,
    ) -> None:
        if type(self) is not ClosedPolicyProofV3:
            raise TypeError("policy proof must have its exact value type")
        ClosedPolicyProofV3.__post_init__(self)
        _integer(input_cells, "policy input cells", 0, CLOSED_STACK_CELLS)
        _integer(output_cells, "policy output cells", 0, CLOSED_STACK_CELLS)
        _integer(max_semantic_steps, "policy semantic allowance", 1, MAX_CALLBACK_SEMANTIC_STEPS)
        if input_cells < self.required_input_cells:
            raise ValueError("policy input signature can underflow its private data stack")
        if output_cells != input_cells + self.net_data_cells:
            raise ValueError("policy output signature does not match every return path")
        if input_cells + self.peak_data_growth > CLOSED_STACK_CELLS:
            raise ValueError("policy signature exceeds its private data stack capacity")
        if self.max_semantic_steps > max_semantic_steps:
            raise ValueError("policy work bound exceeds its semantic allowance")

    def max_data_depth(self, input_cells: int) -> int:
        _integer(input_cells, "policy input cells", 0, CLOSED_STACK_CELLS)
        self.validate_signature(input_cells, input_cells + self.net_data_cells)
        return input_cells + self.peak_data_growth


def _prove_body(body: PolicyBodyV3, known: dict[int, ClosedPolicyProofV3]) -> ClosedPolicyProofV3:
    # All dependencies, including those in unreachable suffixes, belong to the
    # immutable captured closure and its revocation/metadata bounds.
    policy_ids = {body.policy_id}
    core_names: set[str] = set()
    for operation in body.operations:
        if type(operation) is PolicyCallV3:
            dependency = known[operation.policy_id]
            policy_ids.update(dependency.policy_ids)
            core_names.update(dependency.core_names)
        elif type(operation) is PolicyCoreCallV3:
            core_names.add(operation.name)
    if len(policy_ids) + len(core_names) > MAX_CLOSED_WORDS:
        raise ValueError("a policy closure may capture at most 64 Words")

    # A forward DAG lets each operation receive its final join state before
    # analysis. Depth is relative to entry; work retains both path extrema.
    incoming: dict[int, tuple[int, int, int]] = {0: (0, 0, 0)}
    required = growth = 0
    return_cells = 1
    exits: list[tuple[int, int, int]] = []
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
            low_cost = high_cost = 2  # IR Call tick plus primitive tick.
        elif kind is PolicyCallV3:
            dependency = known[operation.policy_id]
            demand, delta, peak = (dependency.required_input_cells,
                                  dependency.net_data_cells, dependency.peak_data_growth)
            low_cost = 1 + dependency.min_semantic_steps
            high_cost = 1 + dependency.max_semantic_steps
            return_cells = max(return_cells, 1 + dependency.max_return_cells)
        elif kind is PolicyBranchZeroV3:
            demand, delta = 1, -1

        required = max(required, demand - depth)
        growth = max(growth, depth + peak)
        depth += delta
        minimum += low_cost
        maximum += high_cost
        if maximum > MAX_CALLBACK_SEMANTIC_STEPS:
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
    if any(exit_depth != net for exit_depth, _, _ in exits):
        raise ValueError("all policy return paths must have identical data depths")
    proof = ClosedPolicyProofV3(
        policy_id=body.policy_id, required_input_cells=required,
        net_data_cells=net, peak_data_growth=growth, max_return_cells=return_cells,
        min_semantic_steps=min(item[1] for item in exits),
        max_semantic_steps=max(item[2] for item in exits),
        policy_ids=tuple(sorted(policy_ids)), core_names=tuple(sorted(core_names)),
    )
    if type(body) is ClosedPolicyV3:
        proof.validate_signature(body.input_cells, body.output_cells)
    return proof


def prove_policies(
    policies: tuple[PolicyBodyV3 | ClosedPolicyV3, ...],
) -> tuple[ClosedPolicyProofV3, ...]:
    """Validate the complete bounded table and return dependency-first proofs.

    Manifest callers use ClosedPolicyV3 entries with declared arities. Engine
    callers may use PolicyBodyV3 while translating already captured IR, then
    validate the entry signature against its export descriptor. Captured bodies
    may share diagnostic names after shadowing; manifest declarations may not.
    No proof here resolves an XT, observes a dictionary or grants authority.
    """

    if type(policies) is not tuple:
        raise TypeError("policies must be an exact immutable tuple")
    if len(policies) > MAX_CLOSED_POLICIES:
        raise ValueError("a policy table may contain at most 64 definitions")
    by_id: dict[int, PolicyBodyV3 | ClosedPolicyV3] = {}
    names: set[str] = set()
    total = 0
    for policy in policies:
        if type(policy) not in (PolicyBodyV3, ClosedPolicyV3):
            raise TypeError("policies must have exact admitted body or declaration types")
        type(policy).__post_init__(policy)
        if policy.policy_id in by_id:
            raise ValueError("duplicate policy ID")
        if type(policy) is ClosedPolicyV3:
            if policy.name.upper() in names:
                raise ValueError("duplicate policy name")
            names.add(policy.name.upper())
        by_id[policy.policy_id] = policy
        total += len(policy.operations)
        if total > MAX_CLOSED_OPERATIONS:
            raise ValueError("a policy table may contain at most 4096 operations")
    for policy in policies:
        for operation in policy.operations:
            if type(operation) is PolicyCallV3 and operation.policy_id not in by_id:
                raise ValueError("policy call names an undeclared policy ID")

    visiting: set[int] = set()
    known: dict[int, ClosedPolicyProofV3] = {}

    def visit(policy_id: int) -> None:
        if policy_id in visiting:
            raise ValueError("closed policy call graph must be acyclic")
        if policy_id in known:
            return
        visiting.add(policy_id)
        policy = by_id[policy_id]
        for operation in policy.operations:
            if type(operation) is PolicyCallV3:
                visit(operation.policy_id)
        visiting.remove(policy_id)
        known[policy_id] = _prove_body(policy, known)

    for policy in policies:
        visit(policy.policy_id)
    return tuple(known.values())


__all__ = [
    "MAX_CLOSED_POLICIES", "MAX_CLOSED_OPERATIONS", "MAX_CLOSED_WORDS",
    "CLOSED_STACK_CELLS", "CORE_STACK_EFFECTS", "PolicyLiteralV3",
    "PolicyCoreCallV3", "PolicyCallV3", "PolicyBranchV3", "PolicyBranchZeroV3",
    "PolicyReturnV3", "PolicyOperationV3", "PolicyBodyV3", "ClosedPolicyV3",
    "ClosedPolicyProofV3", "prove_policies",
]
