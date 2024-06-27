from typing import List, Dict, Any, Optional, TypedDict, Union, TypeGuard

ANNOTATION_PREFIX = '@'
"""Prefix used to identify annotations in comments."""

"""Prefix used to mark the start of a section to be ignored."""
IGNORE_PREFIX = '!*'

IGNORE_SUFFIX = '*!'
"""Suffix used to mark the end of a section to be ignored."""

Argument = TypedDict('Argument', {
    'type': str,
    'description': str,
    'dimension': Optional[str]
})
"""
Represents an argument of a Fortran 90 function.

Fields:
    type (str): The data type of the argument e.g. real(kind=8)
    description (str): A description of the argument's purpose, read from the comments 
    dimension (Optional[str]): The dimension of a vector or matrix argument, if applicable.
"""

FunctionDescription = TypedDict('FunctionDescription', {
    'attributes': List[str],
    'description': str,
    'in': Dict[str, Argument],
    'out': Dict[str, Argument],
    'return': Dict[str, Argument]
})
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

FunctionDetails = TypedDict('FunctionDetails', {
    'details': FunctionDescription
})
"""
Contains detailed information about a Fortran 90 function.

Fields:
    details (FunctionDescription): Detailed description of the function.
"""

SubroutineDescription = TypedDict('SubroutineDescription', {
    'attributes': List[str],
    'description': str,
    'in': Dict[str, Argument],
    'out': Dict[str, Argument]
})
"""
Describes a Fortran 90 subroutines's attributes and arguments, including any
description in the comments.

Fields:
    attributes (List[str]): A list of function attributes e.g. pure, elemental, etc.
    description (str): A description of the function's purpose.
    in (Dict[str, Argument]): The input (intent(in) or intent(inout) arguments to a function.
    out (Dict[str, Argument]): The output (intent(out) or intent(inout)) arguments to a function.
"""

SubroutineDetails = TypedDict('SubroutineDetails', {
    'details': SubroutineDescription
})
"""
Contains detailed information about a Fortran 90 subroutine.

Fields:
    details (SubroutineDescription): Detailed description of the subroutine.
"""

#TODO add @version annotation
#TODO add @author annotation
ModuleData = TypedDict('ModuleData', {
    'module_name': str,
    'constants': Dict[str, Any],
    'functions': Dict[str, FunctionDetails],
    'subroutines': Dict[str, SubroutineDetails],
    'file_name': str,
    'module_description': str
})
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

def is_function_description(description: Union[FunctionDescription, SubroutineDescription]) -> TypeGuard[FunctionDescription]:
    return isinstance(description, dict) and 'return' in description

def is_subroutine_description(description: Union[FunctionDescription, SubroutineDescription]) -> TypeGuard[SubroutineDescription]:
    return isinstance(description, dict) and 'return' not in description

