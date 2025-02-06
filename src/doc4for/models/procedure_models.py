from typing import TypedDict, List, Dict, Optional, Union, TypeGuard

Argument = TypedDict(
    "Argument",
    {
        "type": str,
        "description": str,
        "dimension": Optional[str],  # TODO replace with Dimension
    },
)

FunctionDescription = TypedDict(
    "FunctionDescription",
    {
        "attributes": List[str],
        "description": str,
        "arguments": List[str],
        "in": Dict[str, Argument],
        "out": Dict[str, Argument],
        "return": Dict[str, Argument],
        "binding_type": Optional[str],  # 'deferred', 'non-deferred', 'type-bound', or 'final'
        "interface": Optional[str],  # Name of the interface if applicable
    },
)

SubroutineDescription = TypedDict(
    "SubroutineDescription",
    {
        "attributes": List[str],
        "description": str,
        "arguments": List[str],
        "in": Dict[str, Argument],
        "out": Dict[str, Argument],
        "binding_type": Optional[str],  # 'deferred', 'non-deferred', 'type-bound', or 'final'
        "interface": Optional[str],  # Name of the interface if applicable
    },
)

ProcedureDescription = TypedDict(
    "ProcedureDescription",
    {
        "name": str,
        "description": Optional[str],
        "attributes": List[str],
        "is_final": bool,
        "bound_to": Optional[Union[str, List[str]]],
    },
)


def is_function_description(
    description: Union[FunctionDescription, SubroutineDescription]
) -> TypeGuard[FunctionDescription]:
    return isinstance(description, dict) and "return" in description


def is_subroutine_description(
    description: Union[FunctionDescription, SubroutineDescription]
) -> TypeGuard[SubroutineDescription]:
    return isinstance(description, dict) and "return" not in description
