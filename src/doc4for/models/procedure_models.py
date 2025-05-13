from typing import TypedDict, List, Dict, Optional, Union, TypeGuard, Any, Tuple
from doc4for.models.common import BindingType
from doc4for.models.dimension_models import Dimension
from enum import Enum, auto

#TODO add coarray details
Argument = TypedDict(
    "Argument",
    {
        "type": str,
        "description": str,
        "dimension": Dimension,
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
        "return": Argument, 
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

class PassType(Enum):
    NONE = auto()
    DEFAULT = auto()
    NAMED = auto()

ProcedureDescription = TypedDict(
    "ProcedureDescription",
    {
        "name": str,
        "description": Optional[str],
        "attributes": List[str],
        "is_final": bool, 
        "bound_to": Optional[str], # e.g. procedure(iname) :: proc would give bound_to=iname
        "pass_type": Optional[PassType], # pass, nopass
        "pass_name": Optional[str],
        "implementation": Optional[str] # e.g. add => add_implementation
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
