from typing import TypedDict, List, Dict, Optional, Union, TypeGuard, Any
from doc4for.models.common import BindingType, Uses
from doc4for.models.dimension_models import Dimension
from doc4for.models.variable_models import PolymorphismType
from enum import Enum, auto

#TODO add source file information to all of these
#TODO add coarray details
Argument = TypedDict(
    "Argument",
    {
        "type": str,
        "type_param": Optional[str], # for parameterized types
        "kind": Optional[str],
        "length": Optional[str], # character length
        "description": str,
        "dimension": Dimension,
        "attributes": List[str],
        "default_value": Optional[str],
        "interface_name": Optional[str],
        "enum_type": Optional[str],  # reference to enum if this is an enum type
        "polymorphism_type": Optional[PolymorphismType], # for CLASS=polymorphic, TYPE=non-polymorphic, INTRINSIC=None
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
        "uses": Dict[str, Uses],
    },
)

#TODO should the declaration string be added
FunctionDescription = TypedDict(
    "FunctionDescription",
    {
        "attributes": List[str],
        "description": str,
        "arguments": List[str],
        "in": Dict[str, Argument],
        "out": Dict[str, Argument],
        "return": Argument, #TODO maybe add return variable name to argument
        "argument_interfaces": Dict[str, InterfaceDescription],
        "binding_type": BindingType,
        "uses": Dict[str, Uses],
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
        "binding_type": BindingType,
        "uses": Dict[str, Uses],
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
        "implementation": Optional[str], # e.g. add => add_implementation
        "binding_type": BindingType
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
