from typing import TypedDict, List, Dict, Optional, Union, TypeGuard, Any
from doc4for.models.common import BindingType

Argument = TypedDict(
    "Argument",
    {
        "type": str,
        "description": str,
        "dimension": Optional[str],  # TODO replace with Dimension
        "interface_name": Optional[str],
        "enum_type": Optional[str],  # Reference to enum if this is an enum type
    },
)

ModuleProcedureDescription = TypedDict(
    "ModuleProcedureDescription",
    {
        "name": str,
        "description": str,
    }
)

InterfaceDescription = TypedDict(
    "InterfaceDescription",
    {
        "name": Optional[str], # abstract interfaces don't have a name
        "description": str,
        "attributes": List[str],
        "operator_symbol": Optional[str],
        "procedures": Dict[str, Any], # deferred 
        "module_procedures": Dict[str, ModuleProcedureDescription],
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
        "argument_interfaces": Dict[str, InterfaceDescription],
        "binding_type": BindingType
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
        "argument_interfaces": Dict[str, InterfaceDescription],
        "binding_type": BindingType
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
        "binding_type": Optional[BindingType]
    },
)

InterfaceDescription.__annotations__['procedures'] = Dict[str, Union[FunctionDescription, SubroutineDescription]]

def is_function_description(
    description: Union[FunctionDescription, SubroutineDescription]
) -> TypeGuard[FunctionDescription]:
    return isinstance(description, dict) and "return" in description


def is_subroutine_description(
    description: Union[FunctionDescription, SubroutineDescription]
) -> TypeGuard[SubroutineDescription]:
    return isinstance(description, dict) and "return" not in description
