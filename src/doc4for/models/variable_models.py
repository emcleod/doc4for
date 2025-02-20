from typing import TypedDict, Optional, List
from doc4for.models.dimension_models import Dimension, Dimension_TEMP
from doc4for.models.common import BindingType, BindingTypeEnum
from enum import Enum, auto

#TODO add this information
class ArrayType(Enum):
    EXPLICIT = auto()           # Regular arrays with explicit dimensions
    IMPLIED_SHAPE = auto()      # Implied shape arrays (F2008+)
    DEFERRED_SHAPE = auto()     # Allocatable or pointer arrays
    ASSUMED_SHAPE = auto()      # Arrays as dummy arguments (:)
    ASSUMED_SIZE = auto()       # Arrays as dummy arguments (*)
    ASSUMED_RANK = auto()       # F2008+ (..)

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
        "length": Optional[str],  # for character lengths,
        "binding_type": BindingType
    },
)

ParameterDescription = TypedDict(
    "ParameterDescription",
    {
        "description": str,
        "type": "str",
        "name": "str",
        "value": "str",
        "dimension": Optional[str],
        "attributes": List[str],  # For public/private, etc.
        "length": Optional[str],   # for character lengths
        "binding_type": BindingType
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