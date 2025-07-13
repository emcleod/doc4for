from typing import Optional, List, TypedDict, Dict, Union, Literal, Tuple
from enum import Enum, auto
from dataclasses import dataclass

ANNOTATION_PREFIX = "@"
"""Prefix used to identify annotations in comments."""

"""Prefix used to mark the start of a section to be ignored."""
IGNORE_PREFIX = "!*"

IGNORE_SUFFIX = "*!"
"""Suffix used to mark the end of a section to be ignored."""

UNKNOWN = 'UNKNOWN'

UnknownType = Literal['UNKNOWN']

class ExpressionType(Enum):
    LITERAL = "literal"
    VARIABLE = "variable"
    FUNCTION_CALL = "function_call"
    
@dataclass
class Expression:
    expr_type: ExpressionType
    value: str
    function_name: Optional[str] = None
    arguments: Optional[List["Expression"]] = None

    def __str__(self):
        return f"Expression({self.expr_type}, {self.value}, {self.function_name if self.function_name else ''}, {self.arguments if self.arguments else ''})"
    

ARGUMENT_PATTERN = r'''
    (?P<var_name>\w+)      # Variable name
    \s+                    # Required whitespace
    (?P<description>.+)    # Description (rest of the line)
'''


class BindingTypeEnum(Enum):
    BIND_C = auto()

BindingType = TypedDict(
    "BindingType",
    {
        "type": BindingTypeEnum,
        "name": Optional[str] # External name for C binding if specified
    }
)

EnumeratorDescription = TypedDict(
    "EnumeratorDescription",
    {
        "name": str,
        "value": str,  
        "description": str,
    }
)

EnumDescription = TypedDict(
    "EnumDescription",
    {
        "name": Optional[str],  # Enums might not always have names
        "description": str,
        "attributes": List[str],  # For public/private, bind(c), etc.
        "enumerators": Dict[str, EnumeratorDescription],
        "binding_type": BindingType
    },
)

Uses = TypedDict(
    "Uses", 
    {
        "module_name": str, 
        "selections": List[Union[str, Tuple[str, str]]],  # Can be either a simple string or a tuple with ("local_name": "original_name")
        "description": Optional[str]
    }
)

EquivalenceRelationship = TypedDict(
    "EquivalenceRelationship",
    {
        "variables": List[str],  # Names of all variables in this equivalence group
        "description": Optional[str], 
        "offsets": Dict[str, int],  # Variable name -> byte offset from start of group
                                    # e.g., {"buffer": 0, "int_view": 0, "header": 0, "body": 40}
    }
)
