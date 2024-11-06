from typing import List, Dict, Any, Optional, TypedDict, Union, TypeGuard

ANNOTATION_PREFIX = "@"
"""Prefix used to identify annotations in comments."""

"""Prefix used to mark the start of a section to be ignored."""
IGNORE_PREFIX = "!*"

IGNORE_SUFFIX = "*!"
"""Suffix used to mark the end of a section to be ignored."""

# TODO don't need a dict
Dimension_TEMP = TypedDict("Dimension_TEMP", {"dimensions": List[Union[int, str]]})

ArrayBound = TypedDict("ArrayBound", {
    "lower": Optional[str],  # None for allocatable, should be 1 by default
    "upper": Optional[str]   # None for allocatable
})

Dimension = TypedDict("Dimension", {
    "dimensions": List[ArrayBound]
})

Argument = TypedDict(
    "Argument",
    {
        "type": str,
        "description": str,
        "dimension": Optional[str],  # TODO replace with Dimension
    },
)
"""
Represents an argument of a Fortran 90 function.

Fields:
    type (str): The data type of the argument e.g. real(kind=8)
    description (str): A description of the argument's purpose, read from the comments 
    dimension (Optional[str]): The dimension of a vector or matrix argument, if applicable.
"""

Uses = TypedDict("Uses", {"module_name": str, "selections": List[str]})

FunctionDescription = TypedDict(
    "FunctionDescription",
    {
        "attributes": List[str],
        "description": str,
        "arguments": List[str],
        "in": Dict[str, Argument],
        "out": Dict[str, Argument],
        "return": Dict[str, Argument],
        "binding_type": Optional[
            str
        ],  # 'deferred', 'non-deferred', 'type-bound', or 'final'
        "interface": Optional[str],  # Name of the interface if applicable
    },
)
"""
Describes a Fortran 90 function's attributes, arguments and return values, including any
description in the comments.

Fields:
    attributes (List[str]): A list of function attributes e.g. pure, elemental, etc.
    description (str): A description of the function's purpose.
    in (Dict[str, Argument]): The input (intent(in) or intent(inout) arguments to a function.
    out (Dict[str, Argument]): The output (intent(out) or intent(inout)) arguments to a function.
    return (Dict[str, Argument]): The return value ('result') of a function
"""

SubroutineDescription = TypedDict(
    "SubroutineDescription",
    {
        "attributes": List[str],
        "description": str,
        "arguments": List[str],
        "in": Dict[str, Argument],
        "out": Dict[str, Argument],
        "binding_type": Optional[
            str
        ],  # 'deferred', 'non-deferred', 'type-bound', or 'final'
        "interface": Optional[str],  # Name of the interface if applicable
    },
)
"""
Describes a Fortran 90 subroutines's attributes and arguments, including any
description in the comments.

Fields:
    attributes (List[str]): A list of function attributes e.g. pure, elemental, etc.
    description (str): A description of the function's purpose.
    in (Dict[str, Argument]): The input (intent(in) or intent(inout) arguments to a function.
    out (Dict[str, Argument]): The output (intent(out) or intent(inout)) arguments to a function.
"""

ParameterDescription = TypedDict(
    "ParameterDescription",
    {
        "description": str,
        "type": "str",
        "name": "str",
        "value": "str",
        "dimension": Optional[str],
        "attributes": List[str],  # For public/private, etc.
    },
)

DataComponent = TypedDict(
    "DataComponent",
    {
        "name": str,
        "type": str,
        "kind": Optional[str],
        "description": str,
        "dimension": Optional[Dimension_TEMP],
        "len": Optional[str],
        "initial_value": Optional[str],
        "attributes": List[str],
    },
)

GenericInterface = TypedDict(
    "GenericInterface",
    {
        "generic_spec": str,
        "description": str,
        "attributes": List[str],
        "specific_procedures": List[str],
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

VariableDescription = TypedDict(
    "VariableDescription",
    {
        "description": str,
        "type": str,
        "name": str,
        "dimension": Optional[Dimension],
        "attributes": List[str],  # For public/private, etc.
        "kind": Optional[str],
        "initial_value": Optional[str],
    },
)

TypeDescription = TypedDict(
    "TypeDescription",
    {
        "type_name": str,
        "attributes": List[str],
        "description": str,
        "data_components": Dict[str, DataComponent],
        "procedures": Dict[str, ProcedureDescription],
        "generic_interfaces": Dict[str, GenericInterface],
        "extends": Optional[str],
    },
)

InterfaceDescription = TypedDict(
    "InterfaceDescription",
    {
        "name": str,
        "description": str,
        "attributes": List[str],
        "is_abstract": bool,
        "is_operator": bool,
        "operator_symbol": Optional[str],
    },
)

BlockDataDescription = TypedDict(
    "BlockDataDescription",
    {
        "name": str,
        "description": str,
        "common_blocks": Dict[str, Dict[str, VariableDescription]],
    },
)

# TODO add @version annotation
# TODO add @author annotation
ModuleDescription = TypedDict(
    "ModuleDescription",
    {
        "module_name": str,
        "parameters": Dict[str, ParameterDescription],
        "variables": Dict[str, VariableDescription],
        "functions": Dict[str, FunctionDescription],
        "subroutines": Dict[str, SubroutineDescription],
        "types": Dict[str, TypeDescription],
        "file_name": str,
        "module_description": str,
    },
)
"""
Represents data for a module.

Fields:
    module_name (str): The name of the module.
    constants (Dict[str, Any]): All public constants defined in the module.
    functions (Dict[str, FunctionDetails]): All public functions in the module.
    subroutines (Dict[str, SubroutineDetails]): All public subroutines in the module.
    file_name (str): The name of the file containing the module.
    module_description (str): A description of the module's purpose.
"""

ProgramDescription = TypedDict(
    "ProgramDescription",
    {
        "program_name": str,
        "file_name": str,
        "program_description": str,
        "uses": Dict[str, Uses],
    },
)

FileDescription = TypedDict(
    "FileDescription",
    {
        "file_name": str,
        "file_description": str,
        "functions": Dict[str, FunctionDescription],
        "subroutines": Dict[str, SubroutineDescription],
        "modules": Dict[str, ModuleDescription],
        "programs": Dict[str, ProgramDescription],
        "block_data": Dict[str, BlockDataDescription],
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
