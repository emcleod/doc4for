from typing import TypedDict, Optional, List
from doc4for.models.dimension_models import Dimension
from doc4for.models.common import BindingType
from enum import Enum, auto

#TODO add this information
class ArrayType(Enum):
    EXPLICIT = auto()           # Regular arrays with explicit dimensions
    IMPLIED_SHAPE = auto()      # Implied shape arrays (F2008+)
    DEFERRED_SHAPE = auto()     # Allocatable or pointer arrays
    ASSUMED_SHAPE = auto()      # Arrays as dummy arguments (:)
    ASSUMED_SIZE = auto()       # Arrays as dummy arguments (*)
    ASSUMED_RANK = auto()       # F2008+ (..)

class PolymorphismType(Enum):
    NONE = "none"        # Not polymorphic (covers both intrinsic and TYPE)
    LIMITED = "limited"  # CLASS(type_name)
    UNLIMITED = "unlimited"  # CLASS(*)

#TODO add source file information to all of these

#TODO add coarray information
#TODO polymorphic information should contain information about the allowed types
# e.g. class(shape) where shape is a type, need to indicate that the variable can
# be of any type that extends shape. Maybe it could go in PolymorphismType
VariableDescription = TypedDict(
    "VariableDescription",
    {
        "description": str,
        "type": str,
        "name": str,
        "dimension": Optional[Dimension],
        "polymorphism_type": Optional[PolymorphismType], # for CLASS=polymorphic, TYPE=non-polymorphic, INTRINSIC=None
        "attributes": List[str],  # For public/private, etc.
        "kind": Optional[str], #TODO might want to link to the type for declarations like (kind=MY_DEFINED_TYPE)
        "initial_value": Optional[str],
        "length": Optional[str],  # for character lengths
        "binding_type": Optional[BindingType],
        "is_saved": bool
    },
)

ParameterDescription = TypedDict(
    "ParameterDescription",
    {
        "description": str,
        "type": str,
        "name": str,
        "value": str,
        "dimension": Optional[Dimension],
        "polymorphism_type": Optional[PolymorphismType], # for CLASS=polymorphic, TYPE=non-polymorphic, INTRINSIC=None
        "attributes": List[str],  # For public/private, etc.
        "kind": Optional[str], #TODO might want to link to the type for declarations like (kind=MY_DEFINED_TYPE)
        "length": Optional[str],   # for character lengths
        "binding_type": Optional[BindingType]
    },
)

DataComponent = TypedDict(
    "DataComponent",
    {
        "description": str,
        "type": str,
        "type_params": Optional[str], # for parameterized types
        "name": str,
        "initial_value": Optional[str],
        "dimension": Optional[Dimension],
        "polymorphism_type": PolymorphismType,
        "kind": Optional[str], #TODO might want to link to the type for declarations like (kind=MY_DEFINED_TYPE)
        "len": Optional[str],
        "attributes": List[str],
    },
)